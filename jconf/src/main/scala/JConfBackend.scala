package cap.jeeves.jconf

import cap.scalasmt._
import cap.jeeves._

import scala.collection.mutable.Map
import scala.collection.mutable.Set

import Expr._

object Util {
  def addShutdownHook(body: => Unit) = 
    Runtime.getRuntime.addShutdownHook(new Thread {
      override def run { body }
    })
}

object JConfBackend extends JeevesLib {
  class AccessError extends Exception
  class PermissionsError extends Exception
  class NoSuchUserError extends Exception

  private val usercache = ".jconfusers.cache"
  private val assignmentcache = ".jconfassignments.cache"
  private val papercache = ".jconfpapers.cache"

  private var users : Map[Username, ConfUser] = Map[Username, ConfUser]()
  private var assignments : Map[Int, Set[ConfUser]] = Map[Int, Set[ConfUser]]()
  private var papers : List[PaperRecord] = Nil

  private var actionQueue = 0
  private var confStage = Submission

  def JConfBackend() {
    /* TODO: Figure out if there is some way to log errors in deployed web
       applications. */
    try {
      Persistence.readFromFile[Map[Username, ConfUser]](usercache);
    } catch {
      case e: Exception => ()
    }
    try {
      Persistence.readFromFile[Map[Int, Set[ConfUser]]](assignmentcache);
    } catch {
      case e: Exception => ()
    }
    try {
      Persistence.readFromFile[List[PaperRecord]](papercache)
    } catch {
      case e: Exception => ()
    }
  }

  /* Making papers. */
  private var _papercount = 0;
  private def getPaperUid () : Int = {
    val count = _papercount;
    _papercount = _papercount + 1;
    count
  }

  private def logToFile() {
    // TODO: Get persistence working.
    /* if (actionQueue > 5) {
      Persistence.writeToFile(users, usercache);
      Persistence.writeToFile(assignments, assignmentcache);
      Persistence.writeToFile(papers, papercache)
      actionQueue = 0
    } else */ actionQueue = actionQueue + 1
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

  def addUser(newUser: ConfUser) = {
    users += (newUser.username -> newUser)
    logToFile ()
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

    logToFile();
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
    logToFile()
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
      logToFile();
      r
    } else {
      throw new PermissionsError        
    }
  }

  /* Searching. */
  def getById(id: Int) = papers.find ((p: PaperRecord) => p.id == id)
  def getByIds(ids: List[Int]) = ids.map(id => getById(id))
 
  def searchByTitle(title: String) = 
    papers.filter(_.title === Title(title))
  
  def searchByAuthor(author: ConfUser) = 
    papers.filter(_.getAuthors().has(author))
  
  def searchByTag(tag: PaperTag) = papers.filter(_.getTags().has(tag))

  /* More mundane logistical things. */
  def getUserById(id: String): Option[ConfUser] = users.get(Username(id))
  def loginUser(id: String, password: String): Option[ConfUser] = {
    users.get(Username(id)) match {
      case Some(user) =>
        // Stage should not matter...
        val userCtxt = new ConfContext(user, Submission);
        val pwd : Password = user.showPassword(userCtxt);
        if (pwd.pwd.equals(password)) Some(user) else None
      case None =>
        println("user not found");
        None
    }
  }
}
