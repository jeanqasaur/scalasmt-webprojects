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
    val defaultEmail = "defaultUser";
    JConfTables.getDBConfUserByEmail(defaultEmail) match {
      case Some(user) => user
      case None => {
        val defaultName = "Default User";
        val defaultPwd = "";
        val defaultStatus = PublicStatus;
        new ConfUser (
          getUserUid(
            defaultEmail, defaultName, defaultPwd
            , Conversions.role2Field(defaultStatus))
          , defaultEmail, defaultName, defaultPwd, defaultStatus)
      }
    }
  }
  val defaultPaper = {
    val defaultTitle = "---";
    new PaperRecord (getPaperUid(defaultTitle, ""), defaultTitle)
  }
  val defaultReview = new PaperReview (uid = getReviewUid())

  /* Making papers. */
  private var _usercount = 1;
  private def getUserUid (
    email: String, name: String, password: String, role: Int): Int = {
    val userRecord: ConfUserRecord =
      new ConfUserRecord(email, name, password, role);
    JConfTables.writeDBUser(userRecord);
    userRecord.id
  }
  private var _papercount = 1;
  private def getPaperUid (title: String, file: String) : Int = {
    val paperRecord: PaperItemRecord = new PaperItemRecord(title, file)
    JConfTables.writeDBPaper(paperRecord)
    paperRecord.id
  }
  private var _reviewcount = 1;
  def getReviewUid (
    paperId: Int = -1, reviewerId: Int = -1, body: String = ""
    , problemScore: Int = 3, backgroundScore: Int = 3
    , approachScore: Int = 3, resultScore: Int = 3): Int = {
    val reviewRecord: PaperReviewRecord =
      new PaperReviewRecord(paperId, reviewerId, body
      , problemScore, backgroundScore, approachScore, resultScore);
    JConfTables.writeDBReview(reviewRecord)
    reviewRecord.id
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
    , name: String, password: String, role: UserStatus): ConfUser = {
    val id =
      getUserUid (email, name, password, Conversions.role2Field(role));
    val user = new ConfUser(id, email, name, password, role);

    // Add paper to in-memory cache.
    jconfUsers += (id -> user)

    user
  }

  /* This function should only be called for brand-new papers. */
  def addPaper(name : String, authors : List[ConfUser]
    , file: String = "", tags : List[PaperTag] = Nil)
      : PaperRecord = {
    // TODO: Make sure UID is unique...
    val uid = getPaperUid (name, file);

    val paper = new PaperRecord(uid, name, authors, file, tags)
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

    // TODO: Store in memory?  Load this from memory when we start up...
  }
  def isAssigned (p: PaperRecord, reviewer: ConfUser): Boolean = {
    JConfTables.isAssigned(reviewer.uid.toInt, p.uid.toInt)
  }
  def addReview
    (p: PaperRecord, reviewer: ConfUser, rtext: String
      , problemScore: Int, backgroundScore: Int
      , approachScore: Int, resultScore: Int)
    : PaperReview = {
    if (isAssigned (p, reviewer)) {
      val r =
        p.addReview(reviewer, rtext
        , problemScore, backgroundScore, approachScore, resultScore);
      r
    } else {
      throw new PermissionsError        
    }
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
