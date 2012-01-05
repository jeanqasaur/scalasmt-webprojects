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
  private val usercache = ".jconfusers.cache"
  private val assignmentcache = ".jconfassignments.cache"
  private val papercache = ".jconfpapers.cache"

  private var users : Map[Username, ConfUser] = Map[Username, ConfUser]()
  private var assignments : Map[Int, Set[ConfUser]] = Map[Int, Set[ConfUser]]()
  private var papers : List[PaperRecord] = Nil

  private var actionQueue = 0

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
    if (actionQueue > 5) {
      Persistence.writeToFile(users, usercache);
      Persistence.writeToFile(assignments, assignmentcache);
      Persistence.writeToFile(papers, papercache)
      actionQueue = 0
    } else actionQueue = actionQueue + 1
  }

  def addUser(newUser: ConfUser) = {
    users += (newUser.username -> newUser)
    logToFile ()
  }

  def addPaper(name : Title, authors : List[ConfUser], tags : List[PaperTag])
      : PaperRecord = {
    val paper = new PaperRecord(getPaperUid(), name, authors, tags);
    papers = paper::papers;
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
    : Unit = {
      if (isAssigned (p, reviewer))
          p.addReview(reviewer, rtext, score);
      logToFile()
  }

  /* Searching. */
  def getById(id: Int) = 
    papers.find ((p: PaperRecord) => p.id == id)
  
  def searchByName(name: String) = 
    papers.filter(_.name === Title(name))
  
  def searchByAuthor(author: ConfUser) = 
    papers.filter(_.authors.has(author))
  
  def searchByTag(tag: PaperTag) = papers.filter(_.getTags().has(tag))
  
  /* More mundane logistical things. */
  def loginUser(id: String, password: String): Option[ConfUser] = {
    println("logging in...");
    users foreach ( (elt) => println (elt._1 + ", " + elt._2));
    users.get(Username(id)) match {
      case Some(user) =>
        // Stage should not matter...
        val userCtxt = new ConfContext(user, Submission);
        val pwd : Password = user.getPassword(userCtxt);
        if (pwd.pwd.equals(password)) Some(user) else None
      case None =>
        println("user not found");
        None
    }
  }
}
