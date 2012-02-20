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

object JConfBackend extends JeevesLib {
  class AccessError extends Exception
  class PermissionsError extends Exception
  class NoSuchUserError extends Exception

  println("JConfBackend initialization");

  /* Database initialization. */
  private val TEST = true;

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
            JConfTables.users.deleteWhere(u => u.id.~ > -1)
            JConfTables.assignments.deleteWhere(a => a.reviewerId.~ > -1)
            JConfTables.reviews.deleteWhere(r => r.id.~ >= -1)
            JConfTables.tags.deleteWhere(t => t.paperId.~ > -1)
          }
        }
    }
  }

  private var confStage = Submission

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

  def getContext(user: ConfUser): ConfContext = new ConfContext(user, confStage)
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
    transaction {
      val u =
        JConfTables.users.insert(
          new ConfUserRecord(
            id, username, name, password, Conversions.role2Field(role)))
      user
    }
  }

  def addPaper(name : Title, authors : List[ConfUser], tags : List[PaperTag])
      : PaperRecord = {
    val uid = getPaperUid ();
    val paper = new PaperRecord(uid, name, authors);
    // TODO: Add tags
    JConfTables.papers = paper::JConfTables.papers;
    tags.foreach(t => paper.addTag(t))

    // For each author, add this paper to their submitted papers.
    authors.foreach {
      author => author.addSubmittedPaper(paper)
    }

    paper
  }
 
  /* Reviews. */
  def assignReview (p: PaperRecord, reviewer: ConfUser): Unit = {
    // TODO: Where do we want to have integrity policies about this manifest?
    if (!((reviewer.role == ReviewerStatus) || (reviewer.role == PCStatus)))
      return;
    transaction {
      JConfTables.assignments.insert(new Assignment(reviewer.id, p.id))
    }
  }
  def isAssigned (p: PaperRecord, reviewer: ConfUser): Boolean = {
    val c: Long = from(JConfTables.assignments)(a =>
      where((a.paperId === p.id.~) and (a.reviewerId === reviewer.id.~))
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
  def getPaperById(id: Int): Option[PaperRecord] =
    JConfTables.papers.find ((p: PaperRecord) => p.id == id)
  def getPapersByIds(ids: List[Int]): List[Option[PaperRecord]] =
    ids.map(id => getPaperById(id))
 
  def searchByTitle(title: String) = 
    JConfTables.papers.filter(_.title === Title(title))
  
  def searchByAuthor(author: ConfUser) = 
    JConfTables.papers.filter(_.getAuthors().has(author))
  
  def searchByTag(tag: PaperTag) = JConfTables.papers.filter(_.getTags().has(tag))

 
  def record2User(u: ConfUserRecord): ConfUser = {
    ConfUser(
        u.id, Username(u.username), Name(u.name), u.pwd
      , Conversions.field2Role(u.role))
  }
  /* More mundane logistical things. */
  def getUserById(id: Int): Option[ConfUser] = {
    transaction {
      try {
        val userRecord: Option[ConfUserRecord] = JConfTables.users.lookup(id)
        userRecord match {
          case Some(u) => Some(record2User(u))
          case None => None
        }
      } catch {
        case e: Exception => None
      }
    }
  }
  def loginUser(id: String, password: String): Option[ConfUser] = {
    transaction {
      val userRecord = from(JConfTables.users)(u =>
        where(u.username like id)
        select(u)).single;
      val user = record2User(userRecord)
      val userCtxt = new ConfContext(user, Submission);
      val pwd : Password = user.showPassword(userCtxt);
      if (pwd.pwd.equals(password)) Some(user) else None
    }
  }
}
