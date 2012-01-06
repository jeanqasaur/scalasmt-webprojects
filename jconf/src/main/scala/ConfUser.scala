package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._

import JConfBackend._

sealed trait UserStatus extends JeevesRecord
object PublicStatus extends UserStatus
object AuthorStatus extends UserStatus
object ReviewerStatus extends UserStatus
object PCStatus extends UserStatus

/*
trait UserID {
  var count = 0
  def genID() : String = {
    val curID = count;
    count = count + 1;
    "id" + curID.toString
  }
}
*/

/* Conference User */
case class Username (name: String) extends JeevesRecord
case class Name (name: String) extends JeevesRecord
case class Password (val pwd: String) extends JeevesRecord
case class ConfUser( val username: Username, val name: Name, pwd: String
  , val role: UserStatus )
  extends JeevesRecord {
    // Level variables and policies.
    private val self: Formula = CONTEXT.viewer === this;
    private val selfL = mkLevel ();
    policy (selfL, !self, LOW);

    // Submitted papers.
    private var _submittedPapers: List[PaperRecord] = Nil
    def addSubmittedPaper (p: PaperRecord): Unit =
      _submittedPapers = p::_submittedPapers
    def getSubmittedPapers (): List[Symbolic] =
      _submittedPapers.map(p => mkSensitive(selfL, p, NULL))

    // Papers to review.
    private var _reviewPapers: List[PaperRecord] = Nil
    def addReviewPaper (r: PaperRecord): Unit = _reviewPapers = r::_reviewPapers
    def getReviewPapers (): List[Symbolic] =
      _reviewPapers.map(r => mkSensitive(selfL, r, NULL))

    // Reviews submitted.
    private var _reviews: List[PaperReview] = Nil
    def addReview (r: PaperReview): Unit =_reviews = r::_reviews
    def getReviews (): List[Symbolic] =
      _reviews.map(r => mkSensitive(selfL, r, NULL))

    // Password.
    private var _password = pwd
    def setPassword (p: String) = _password = p
    def getPassword (): Symbolic =
      mkSensitive(selfL, Password(_password), Password("default"))
  }
