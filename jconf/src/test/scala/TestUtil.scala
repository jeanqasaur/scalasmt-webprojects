package test.cap.jeeves.jconf

import cap.scalasmt._
import cap.jeeves._
import cap.jeeves.jconf._
import JConfBackend._

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import scala.collection.immutable.Map
import scala.collection.mutable.Set
import scala.util.Random

object TestUtil {
  def mkUser( userName : String, name: String
            , pwd: String, userStatus : UserStatus): ConfUser = {
    addUser(userName, name, pwd, userStatus)
  }

  // jconf users.
  val author0 = mkUser("author0", "Author0", "a0p", AuthorStatus)
  def getAuthorCtxt0 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(author0, stage);
  val author1 = mkUser("author1", "Author1", "", AuthorStatus);
  def getAuthorCtxt1 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(author1, stage);
  val author2 = mkUser("author2", "Author2", "", AuthorStatus)
  def getAuthorCtxt2 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(author2, stage);

  val reviewer0 = mkUser("reviewer0", "Reviewer0", "", ReviewerStatus);
  def getReviewerCtxt0 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(reviewer0, stage);
  val reviewer1 = mkUser("reviewer1", "Reviewer1", "", ReviewerStatus);

  val pc0 = mkUser("pc0", "PC0", "", PCStatus);
  def getPcCtxt0 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(public0, stage);

  val public0 = mkUser("public0", "Public0", "", PublicStatus);
  def getPublicCtxt0 (stage: PaperStage = Submission)
  : ConfContext = new ConfContext(public0, stage);

  // papers.
  val emptyName = "No permission"

  val paper0Name = "my paper"
  val paper0 = addPaper(paper0Name, List(author0, author1), Nil);
  assignReview(paper0, reviewer1);
  assignReview(paper0, reviewer0);
  val paper0Review = paper0.addReview(reviewer0, "very nice", 3);

  val paper1Name = "hello world"
  val paper1 = addPaper(paper1Name, List(author2), List(Accepted));
  val paper1Review = paper1.addReview(reviewer1, "eh", 2);
}
