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

object JConfBackend extends JeevesLib with Schema {
  class AccessError extends Exception
  class PermissionsError extends Exception
  class NoSuchUserError extends Exception

  private val users = table[ConfUserRecord]
  private var assignments : Map[Int, Set[ConfUser]] = Map[Int, Set[ConfUser]]()
  private var papers : List[PaperRecord] = Nil

  private var actionQueue = 0
  private var confStage = Submission

  Class.forName("com.mysql.jdbc.Driver");
  val dbUsername = "jeanyang";
  val dbPassword = "scalasmt";
  val dbConnection = "jdbc:mysql://mysql.csail.mit.edu/JConfDB";
  SessionFactory.concreteFactory = Some(()=>
     Session.create(
       java.sql.DriverManager.getConnection(dbConnection, dbUsername, dbPassword)
       , new MySQLAdapter))

  /* Making papers. */
  private var _usercount = 0;
  private def getUserUid (): Int = {
    val count = _usercount;
    _usercount = _usercount + 1;
    count
  }
  private var _papercount = 0;
  private def getPaperUid () : Int = {
    val count = _papercount;
    _papercount = _papercount + 1;
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
      val u = users.insert(new ConfUserRecord(id, username, name, password))
      user
    }
  }

  def addPaper(name : Title, authors : List[ConfUser], tags : List[PaperTag])
      : PaperRecord = {
    val uid = getPaperUid ();
    val paper = new PaperRecord(uid, name, authors, tags);
    papers = paper::papers;

    // For each author, add this paper to their submitted papers.
    authors.foreach {
      author => author.addSubmittedPaper(paper)
    }

    paper
  }
 
  /* Reviews. */
  def assignReview (p: PaperRecord, reviewer: ConfUser): Unit = {
    if (!((reviewer.role == ReviewerStatus) || (reviewer.role == PCStatus)))
      return;
    assignments.get(p.id) match {
      case Some(reviewers) => reviewers += reviewer
      case None =>
        val reviewers = Set[ConfUser]();
        reviewers += reviewer;
        assignments += (p.id -> reviewers)
    };
    reviewer.addReviewPaper(p);
  }
  def isAssigned (p: PaperRecord, reviewer: ConfUser): Boolean = {
    assignments.get(p.id) match {
      case Some(reviewers) => reviewers.contains(reviewer)
      case None => false
    }
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
    papers.find ((p: PaperRecord) => p.id == id)
  def getPapersByIds(ids: List[Int]): List[Option[PaperRecord]] =
    ids.map(id => getPaperById(id))
 
  def searchByTitle(title: String) = 
    papers.filter(_.title === Title(title))
  
  def searchByAuthor(author: ConfUser) = 
    papers.filter(_.getAuthors().has(author))
  
  def searchByTag(tag: PaperTag) = papers.filter(_.getTags().has(tag))

 
  def record2User(u: ConfUserRecord): ConfUser = {
    ConfUser(u.id, Username(u.username), Name(u.name), u.pwd, PublicStatus)
  }

  /* More mundane logistical things. */
  def getUserById(id: Int): Option[ConfUser] = {
    // users.get(id)
    transaction {
      try {
        val userRecord: Option[ConfUserRecord] = users.lookup(id)
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
      val userRecord = from(users)(u =>
        where(u.username like id)
        select(u)).single;
      val user = record2User(userRecord)
      val userCtxt = new ConfContext(user, Submission);
      val pwd : Password = user.showPassword(userCtxt);
      if (pwd.pwd.equals(password)) Some(user) else None
    }
  }
}
