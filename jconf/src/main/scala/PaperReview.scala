package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._

import JConfBackend._

class PaperReview(val id: Int = -1
  , reviewer: ConfUser = new ConfUser()
  , body: String = ""
  , score: Int = -1
  , isDeclassified: Boolean = false)
  extends JeevesRecord with Serializable {

  /**************/
  /* Variables. */
  /**************/
  private val _reviewer: ConfUser = reviewer
  private var _body: String = body
  private var _score: Int = score

  /*************/
  /* Policies. */
  /*************/
  private val _reviewerL = mkLevel ();
  private val _scoreL = mkLevel ();
  private val _isInternalF: Formula = {
    val vrole = CONTEXT.viewer.role;
    (vrole === ReviewerStatus) || (vrole === PCStatus);
  }
  policy(_reviewerL, _isInternalF, HIGH);
  policy(_reviewerL, !_isInternalF, LOW);
  def getReviewer(): Symbolic =
    mkSensitive(_reviewerL, _reviewer, new ConfUser())
  def showReviewer(ctxt: ConfContext): ConfUser = {
    concretize(ctxt, getReviewer()).asInstanceOf[ConfUser]
  }
  def getReviewerTag(): Symbolic =
    mkSensitive(_reviewerL, ReviewedBy(_reviewer), new ConfUser())
  def showReviewerTag(ctxt: ConfContext): PaperTag = {
    concretize(ctxt, getReviewerTag()).asInstanceOf[PaperTag]
  }

  def setBody (newbody: String) = _body = newbody
  def getBody (): String = _body

  def setScore (newscore: Int) = _score = newscore
  def getScore (): Int = _score
}
