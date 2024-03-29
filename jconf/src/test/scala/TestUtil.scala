package test.cap.jeeves.jconf.backend

import cap.scalasmt._
import cap.jeeves._
import cap.jeeves.jconf.backend._
import JConfBackend._

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import org.squeryl.adapters.MySQLAdapter
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Session
import org.squeryl.SessionFactory

import scala.collection.immutable.Map
import scala.collection.mutable.Set
import scala.util.Random

object TestUtil {
  Class.forName("com.mysql.jdbc.Driver");
  val dbUsername = "jeanyang";
  val dbPassword = "scalasmt";
  val dbConnection = "jdbc:mysql://mysql.csail.mit.edu/JYTestDB"

  println("initializing session...")

  SessionFactory.concreteFactory = Some(()=>
    Session.create(
      java.sql.DriverManager.getConnection(
        dbConnection, dbUsername, dbPassword)
      , new MySQLAdapter))
    // If we are testing, initialize tables.
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

  def withDataInDatabase(test: => Any) {
    try { test }
    finally { Session.cleanupResources }
  }

  def mkUser( userName : String, name: String
            , pwd: String, isGrad: Boolean = false, acmNum: Int = -1
            , userStatus : UserStatus): ConfUser = {
    addUser(userName, name, pwd, isGrad, acmNum, userStatus)
  }

  // jconf users.
  val author0 = mkUser("author0", "Author0", "a0p", userStatus=AuthorStatus)
  def getAuthorCtxt0 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(author0, stage);
  val author1 = mkUser("author1", "Author1", "", userStatus=AuthorStatus);
  def getAuthorCtxt1 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(author1, stage);
  val author2 = mkUser("author2", "Author2", "", userStatus=AuthorStatus)
  def getAuthorCtxt2 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(author2, stage);

  val reviewer0 = mkUser("reviewer0", "Reviewer0", "", userStatus=ReviewerStatus);
  def getReviewerCtxt0 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(reviewer0, stage);
  val reviewer1 = mkUser("reviewer1", "Reviewer1", "", userStatus=ReviewerStatus);

  val pc0 = mkUser("pc0", "PC0", "", userStatus=PCStatus);
  def getPcCtxt0 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(public0, stage);

  val public0 = mkUser("public0", "Public0", "", userStatus=PublicStatus);
  def getPublicCtxt0 (stage: PaperStage = Submission)
  : ConfContext = new ConfContext(public0, stage);

  // papers.
  val emptyName = "No permission"

  val paper0Name = "my paper"
  val paper0 = addPaper(paper0Name, List(author0, author1));
  assignReview(paper0, reviewer1);
  assignReview(paper0, reviewer0);
  val paper0Review = paper0.addReview(reviewer0, "very nice", 3);

  val paper1Name = "hello world"
  val paper1 = addPaper(paper1Name, List(author2), tags=List(Accepted));
  val paper1Review = paper1.addReview(reviewer1, "eh", 2);
}
