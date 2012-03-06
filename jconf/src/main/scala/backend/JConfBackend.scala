package cap.jeeves.jconf.backend

import cap.scalasmt._
import cap.jeeves._

import scala.collection.mutable.Map
import scala.collection.mutable.Set

import Expr._

object JConfBackend extends JeevesLib with Serializable {
  class AccessError extends Exception
  class PermissionsError extends Exception
  class NoSuchUserError(uid: Int) extends Exception
  class NoSuchPaperError(uid: Int) extends Exception

  println("JConfBackend initialization");

  /* Database initialization. */
  private val TEST = true;
  private val useDB = true;

  private val jconfUsers: Map[Int, ConfUser] = Map[Int, ConfUser]()
  private val jconfPapers: Map[Int, PaperRecord] = Map[Int, PaperRecord]()
  private val jconfReviews: Map[Int, PaperReview] = Map[Int, PaperReview]()

  /* Debugging variables. */
  private var numConfUserPs = 0;
  def logConfUserPolicy() = numConfUserPs = numConfUserPs + 1
  private var numPaperRecordPs = 0;
  def logPaperRecordPolicy() = numPaperRecordPs = numPaperRecordPs + 1
  private var numPaperReviewPs = 0;
  def logPaperReviewPolicy() = numPaperReviewPs = numPaperReviewPs + 1
  def printPolicyCount() = {
    println("***")
    println("# ConfUser policies: " + numConfUserPs)
    println("# PaperRecord policies: " + numPaperRecordPs)
    println("# PaperReview policies: " + numPaperReviewPs)
    println("***")
  }

  private var confStage = Submission

  val defaultUser = {
    getUserById(-1) match {
      // If the default user exists already...
      case Some(u) => u
      // Otherwise create a new one.
      case None =>
      val defaultEmail = "defaultUser";
      JConfTables.getDBConfUserByEmail(defaultEmail) match {
        case Some(user) => user
        case None => {
          val defaultName = "Default User";
          val defaultPwd = "";
          val defaultGrad = false;
          val defaultAcm = -1
          val defaultStatus = PublicStatus;
          val (id, secretId) =
            getUserUid(
              defaultEmail, defaultName, defaultPwd, defaultGrad, defaultAcm
            , Conversions.role2Field(defaultStatus))
          new ConfUser ( id, secretId
            , defaultEmail, defaultName, defaultPwd, defaultGrad, defaultAcm
            , defaultStatus )
        }
      }
    }
  }
  val defaultPaper = {
    getPaperById(-1) match {
      case Some(p) => p
      case None =>
        val defaultTitle = "---";
        val (id, secretId) = getPaperUid(defaultTitle, "")
        new PaperRecord (id, secretId, defaultTitle)
    }
  }
  val defaultReview = {
    JConfTables.getReviewByPaperReviewer(-1, -1) match {
      case Some(r) => r
      case None =>
        val (id, key) = getReviewUid()
        new PaperReview (uid=id, key=key)
    }
  }

  /* Making papers. */
  private var _usercount = 1;
  private def getUserUid (
    email: String, name: String, password: String, isGrad: Boolean
    , acmNum: Int, role: Int): (Int, String) = {
    // Generate a secretId.
    val secretId = RandomGenerator.generateSecretId();
    // Use the secretId.
    val userRecord: ConfUserRecord =
      new ConfUserRecord(secretId, email, name, password, isGrad, acmNum, role);
    JConfTables.writeDBUser(userRecord);
    (userRecord.id, secretId)
  }
  private var _papercount = 1;
  private def getPaperUid (title: String, file: String) : (Int, String) = {
    val secretId = RandomGenerator.generateSecretId();
    val paperRecord: PaperItemRecord =
      new PaperItemRecord(secretId, title, file)
    JConfTables.writeDBPaper(paperRecord)
    (paperRecord.id, secretId)
  }
  private var _reviewcount = 1;
  def getReviewUid (
    paperId: Int = -1, reviewerId: Int = -1, body: String = ""
    , problemScore: Int = 3, backgroundScore: Int = 3
    , approachScore: Int = 3, resultScore: Int = 3): (Int, String) = {
    val key = RandomGenerator.generateSecretId();

    val reviewRecord: PaperReviewRecord =
      new PaperReviewRecord(secretId=key, paperId=paperId, reviewerId=reviewerId
      , body=body
      , problemScore=problemScore, backgroundScore=backgroundScore
      , approachScore=approachScore, resultScore=resultScore);

    JConfTables.writeDBReview(reviewRecord)
    (reviewRecord.id, key)
  }

  def getContext(user: ConfUser): ConfContext =
    new ConfContext(user, confStage)
  def show[T](ctxt: ConfContext, v: Symbolic):T = {
    concretize(ctxt, v).asInstanceOf[T]
  }

  def getConcrete[T](ctxt: ConfContext, v: Symbolic):T = {
    concretize(ctxt, v).asInstanceOf[T]
  }

  def addUser(email: String
    , name: String, password: String, isGrad: Boolean, acmNum: Int
    , role: UserStatus): ConfUser = {
    val (id, secretId) =
      getUserUid (email, name, password, isGrad, acmNum
      , Conversions.role2Field(role));
    val user =
      new ConfUser(id, secretId, email, name, password, isGrad, acmNum, role);

    // Add paper to in-memory cache.
    jconfUsers += (id -> user)

    user
  }

  /* This function should only be called for brand-new papers. */
  def addPaper(name : String, authors : List[ConfUser]
    , file: String = "", tags : List[PaperTag] = Nil)
      : PaperRecord = {
    // TODO: Make sure UID is unique...
    val (uid, secretId) = getPaperUid (name, file);

    val paper = new PaperRecord(uid, secretId, name, authors, file, tags)
    authors.foreach(a => a.addSubmittedPaper(uid))
    authors.foreach(a => JConfTables.writeDBAuthor(uid.toInt, a.uid.toInt))

    // Add paper to in-memory cache.
    jconfPapers += (uid -> paper)
    paper
  }
 
  /* Reviews. */
  def assignReview (p: PaperRecord, reviewer: ConfUser): Unit = {
    // TODO: Where do we want to have integrity policies about this manifest?
    if (!((reviewer.role == ReviewerStatus) || (reviewer.role == PCStatus)))
      return;

    // Write persistently.
    JConfTables.writeAssignment(reviewer.uid.toInt, p.uid.toInt)
    p.addReview(reviewer)
    p.addTag(NeedsReview(reviewer.uid))
  }
  def isAssigned (p: PaperRecord, reviewer: ConfUser): Boolean = {
    JConfTables.isAssigned(reviewer.uid.toInt, p.uid.toInt)
  }
  
  /* Searching. */
  def lookupCachedPaper(uid: Int): Option[PaperRecord] = { jconfPapers.get(uid) }
  def cachePaper(paper: PaperRecord) = jconfPapers += (paper.uid.toInt -> paper)
  def getPaperById(uid: Int): Option[PaperRecord] = {
    lookupCachedPaper(uid) match {
      case Some(paper) => Some(paper)
      case None =>
        val p = JConfTables.getDBPaperRecord(uid)
        p match {
          case Some(paper) => cachePaper(paper)
          case None => ()
        }
        p
    }
  }
  def getPapersByIds(ids: List[Int]): List[Option[PaperRecord]] =
    ids.map(id => getPaperById(id))
  
  def searchByTitle(title: String) = 
    JConfTables.getAllDBPapers().filter(_.title === StringVal(title))
  
  def searchByAuthor(author: ConfUser) = 
    JConfTables.getAllDBPapers().filter(_.getAuthors().has(author))
  
  def searchByTag(tag: PaperTag) = {
    JConfTables.getAllDBPapers().filter(_.getTags().has(tag))
  }
 
  def lookupCachedUser(uid: Int): Option[ConfUser] = { jconfUsers.get(uid) }
  def cacheUser(user: ConfUser) = jconfUsers += (user.uid.toInt -> user)
  def getUserById(uid: Int): Option[ConfUser] = {
    lookupCachedUser(uid) match {
      case Some(user) => Some(user)
      case None =>
        val u = JConfTables.getDBConfUser(uid)
        u match {
          case Some(user) => cacheUser(user)
          case None => ()
        }
        u
    }
  }
  def loginUser(uname: String, password: String): Option[ConfUser] = {
    JConfTables.getDBConfUserByEmail(uname) match {
      case Some(user) =>
        cacheUser(user);
        val userCtxt = new ConfContext(user, confStage);
        val pwd : String = user.showPassword(userCtxt);
        if (pwd.equals(password)) Some(user) else None
      case None => None
    }
  }
}
