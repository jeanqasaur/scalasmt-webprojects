package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._
import org.squeryl.PrimitiveTypeMode._

import JConfBackend._

class PaperReview(val uid: BigInt
  , private val _paperId: BigInt = -1
  , private val _reviewerId: BigInt = -1
  , private var _body: String = ""
  , private var _score: Int = -1)
  extends JeevesRecord with Serializable {
  // TODO: Is this where we want to associate reviews with users?

  /*************/
  /* Policies. */
  /*************/
  private val _reviewerL = mkLevel ();
  private val _scoreL = mkLevel ();
  private val _isInternalF: Formula = {
    val vrole = CONTEXT.viewer.role;
    (vrole === ReviewerStatus) || (vrole === PCStatus);
  }
  policy(_reviewerL, !_isInternalF, LOW);
  logPaperReviewPolicy();

  def getReviewer(): IntExpr =
    mkSensitiveInt(_reviewerL, _reviewerId, -1)
  def showReviewer(ctxt: ConfContext): ConfUser = {
    concretize(ctxt, getReviewer()).asInstanceOf[ConfUser]
  }
  def getReviewerTag(): Symbolic =
    mkSensitive(_reviewerL, ReviewedBy(_reviewerId), EmptyTag)
  def showReviewerTag(ctxt: ConfContext): PaperTag = {
    concretize(ctxt, getReviewerTag()).asInstanceOf[PaperTag]
  }

  def setBody (newbody: String) = _body = newbody
  def getBody (): String = _body

  def setScore (newscore: Int) = _score = newscore
  def getScore (): Int = _score

  def getPaperReviewRecord(): PaperReviewRecord = {
    transaction { JConfTables.reviews.get(uid.toInt) }
    /*
    new PaperReviewRecord(
      uid.toInt, _paperId.toInt, _reviewerId.toInt, _body, _score)
    */
  }
}
