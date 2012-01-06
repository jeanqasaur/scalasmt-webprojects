package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._

import JConfBackend._

class PaperReview(val id: Int, reviewer: ConfUser, body: String, score: Int)
  extends JeevesRecord {
  private val _reviewer: ConfUser = reviewer

  private val _reviewerL = mkLevel ();
  private val _isInternalF: Formula = {
    val vrole = CONTEXT.viewer.role;
    (vrole === ReviewerStatus) || (vrole === PCStatus);
  }
  policy(_reviewerL, _isInternalF, HIGH);
  policy(_reviewerL, !_isInternalF, LOW);
  def getReviewer(): Symbolic = mkSensitive(_reviewerL, _reviewer, NULL)
  def getReviewerTag(): Symbolic =
    mkSensitive(_reviewerL, ReviewedBy(_reviewer), NULL)

  private var _body: String = body
  def setBody (newbody: String) = _body = newbody
  def getBody (): String = _body

  private var _score: Int = score
  private val _scoreL = mkLevel ();
  def setScore (newscore: Int) = _score = newscore
  def getScore (): Int = _score
}