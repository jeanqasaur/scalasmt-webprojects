package cap.jeeves.jconf

import cap.scalasmt._
import cap.jeeves._

import org.squeryl.adapters.MySQLAdapter
import org.squeryl.PrimitiveTypeMode._
// import org.squeryl.customtypes.CustomTypesMode._
// import org.squeryl.customtypes._
import org.squeryl.Session
import org.squeryl.SessionFactory

import scala.collection.mutable.Map
import scala.collection.mutable.Set

import org.squeryl.Schema

import Expr._

object JConfBackend extends JeevesLib with Serializable {
  class AccessError extends Exception
  class PermissionsError extends Exception
  class NoSuchUserError(uid: Int) extends Exception

  println("JConfBackend initialization");

  /* Database initialization. */
  private val TEST = true;

  private val jconfUsers: Map[Int, ConfUser] = Map[Int, ConfUser]()
  private val jconfPapers: Map[Int, PaperRecord] = Map[Int, PaperRecord]()
  private val jconfReviews: Map[Int, PaperReview] = Map[Int, PaperReview]()

  Class.forName("com.mysql.jdbc.Driver");
  val dbUsername = "jeanyang";
  val dbPassword = "scalasmt";
  val dbConnection = {
    if (TEST)
      "jdbc:mysql://mysql.csail.mit.edu/JYTestDB"
    else
      "jdbc:mysql://mysql.csail.mit.edu/JeevesConfDB"
  }
  SessionFactory.concreteFactory = Some(()=>
     Session.create(
       java.sql.DriverManager.getConnection(dbConnection, dbUsername, dbPassword)
       , new MySQLAdapter))
  // If we are testing, initialize tables.
  if (TEST) {
    try {
      transaction {
        JConfTables.create;
      }
    } catch {
      case e: Exception =>
        try {
          transaction {
            JConfTables.assignments.deleteWhere(a => a.reviewerId.~ > -1)
            JConfTables.authors.deleteWhere(u => u.paperId.~ > -1)
            JConfTables.papers.deleteWhere(p => p.id.~ > -1)
            JConfTables.reviews.deleteWhere(r => r.id.~ >= -1)
            JConfTables.tags.deleteWhere(t => t.paperId.~ > -1)
            JConfTables.users.deleteWhere(u => u.id.~ > -1)
          }
        }
    }
  }

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

  val defaultUser = new ConfUser ()
  val defaultPaper = new PaperRecord ()

  /* Making papers. */
  private var _usercount = 1;
  private def getUserUid (): Int = {
    val count = _usercount;
    _usercount = _usercount + 1;
    count
  }
  private var _papercount = 1;
  private def getPaperUid () : Int = {
    val count = _papercount;
    _papercount = _papercount + 1;
    count
  }
  private var _reviewcount = 1;
  def getReviewUid (): Int = {
    val count = _reviewcount;
    _reviewcount = _reviewcount + 1;
    count
  }

  def getContext(user: ConfUser): ConfContext =
    new ConfContext(user, confStage)
  def show[T](ctxt: ConfContext, v: Symbolic):T = {
    concretize(ctxt, v).asInstanceOf[T]
  }

  def getConcrete[T](ctxt: ConfContext, v: Symbolic):T = {
    concretize(ctxt, v).asInstanceOf[T]
  }

  def showList[T](user: ConfContext, vs: List[Symbolic]):List[T] = {
    vs.map(x => show[T](user, x))
  }

  def addUser(username: String
    , name: String, password: String, role: UserStatus): ConfUser = {
    val id = getUserUid ();
    val user =
      new ConfUser(id, Username(username), Name(name), password, role);

    // Add user to persistent database.
    val u = transaction {
      JConfTables.users.insert(user.getConfUserRecord())
    }

    // Add paper to in-memory cache.
    jconfUsers += (id -> user)

    user
  }

  def addPaper(name : String, authors : List[ConfUser], tags : List[PaperTag])
      : PaperRecord = {
    // TODO: Make sure UID is unique...
    val uid = getPaperUid ();

    val paper = new PaperRecord(uid, Title(name), authors)
    tags.foreach(t => paper.addTag(t))

    authors.foreach(a => a.addSubmittedPaper(uid))

    // Add paper to persistent database.
    val paperRecord =
      transaction {
        JConfTables.papers.insert(paper.getPaperItemRecord())
      }
    
    // Add paper to in-memory cache.
    jconfPapers += (uid -> paper)
    paper
  }
 
  /* Reviews. */
  def assignReview (p: PaperRecord, reviewer: ConfUser): Unit = {
    // TODO: Where do we want to have integrity policies about this manifest?
    if (!((reviewer.role == ReviewerStatus) || (reviewer.role == PCStatus)))
      return;
    // TODO: Store this in memory too...
    transaction {
      JConfTables.assignments.insert(
        new Assignment(reviewer.uid.toInt, p.uid.toInt))
    }
  }
  def isAssigned (p: PaperRecord, reviewer: ConfUser): Boolean = {
    val c: Long = from(JConfTables.assignments)(a =>
      where((a.paperId === p.uid.toInt.~)
        and (a.reviewerId === reviewer.uid.toInt.~))
      compute(count)
    )
    c > 0
  }
  def addReview
    (p: PaperRecord, reviewer: ConfUser, rtext: String, score: Int)
    : PaperReview = {
    if (isAssigned (p, reviewer)) {
      val r = p.addReview(reviewer, rtext, score);
      r
    } else {
      throw new PermissionsError        
    }
  }

  /* Searching. */
  def allPapers(): List[PaperRecord] = {
    transaction {
      val paperRecords = from(JConfTables.papers)(p => select(p)).toList;
      paperRecords.map(r => r.getPaperRecord())
    }
  }
  def getPaperById(id: Int): Option[PaperRecord] = {
    jconfPapers.get(id) match {
      case Some(paper) => Some(paper)
      case None =>
        transaction { JConfTables.papers.lookup(id) } match {
            case Some(paperRecord) =>
              Some(paperRecord.getPaperRecord())
            case None => None
        }
    }
  }
  def getPapersByIds(ids: List[Int]): List[Option[PaperRecord]] =
    ids.map(id => getPaperById(id))
 /*
  def searchByTitle(title: String) = 
    allPapers().filter(_.title === Title(title))
 */
  def searchByAuthor(author: ConfUser) = 
    allPapers().filter(_.getAuthors().has(author))
  
  def searchByTag(tag: PaperTag) = {
    allPapers().filter(_.getTags().has(tag))
  }
 
  def getUserById(id: Int): Option[ConfUser] = {
    jconfUsers.get(id) match {
      case Some(user) => Some(user)
      case None =>
        try {
          val userRecord: Option[ConfUserRecord] =
            transaction { JConfTables.users.lookup(id) }
          userRecord match {
            case Some(u) => Some(u.getConfUser())
            case None => None
          }
        } catch {
          case e: Exception =>
            println(e);
            None
        }
    }
  }
  def loginUser(id: String, password: String): Option[ConfUser] = {
    transaction {
      val userRecord = from(JConfTables.users)(u =>
        where(u.username like id)
        select(u)).single;
      val user = userRecord.getConfUser()
      val userCtxt = new ConfContext(user, confStage);
      val pwd : Password = user.showPassword(userCtxt);
      if (pwd.pwd.equals(password)) Some(user) else None
    }
  }
}
