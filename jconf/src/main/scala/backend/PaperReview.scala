package cap.jeeves.jconf.backend

/*
 * User records for jconf case study.
 * @author jeanyang
 */
import cap.scalasmt._
import org.squeryl.PrimitiveTypeMode._

import cap.scalasmt._
import Expr._

class PaperReview(
            val  b: JConfBackend
  ,         val  uid: BigInt
  ,         val  key: String
  ,         var  paperId: BigInt = -1
  , private val _reviewerId: BigInt = -1
  , private var _body: String = ""
  , private var _problemScore: Int = 3
  , private var _backgroundScore: Int = 3
  , private var _approachScore: Int = 3
  , private var _resultScore: Int = 3)
  extends Atom with Serializable {
  b.register(this)

  /*************/
  /* Policies. */
  /*************/
  private val _reviewerL = b.mkLevel ();
  private val _scoreL = b.mkLevel ();
  private val _isInternalF: Formula = {
    val vrole = b.CONTEXT.viewer.role;
    (vrole === b.reviewerStatus) || (vrole === b.pcStatus);
  }
  b.policy(_reviewerL,
    !((b.CONTEXT.viewer~'uid === _reviewerId)
      || (b.CONTEXT.viewer.role === b.pcStatus))
    , b.LOW);
  b.logPaperReviewPolicy();

  val reviewer: IntExpr = b.mkSensitiveInt(_reviewerL, _reviewerId, -1)
  def showReviewer(ctxt: ConfContext): ConfUser = {
    val reviewerId: BigInt =
      b.concretize(ctxt, reviewer).asInstanceOf[BigInt]
    b.getUserById(reviewerId.toInt) match {
      case Some(u) => u
      case None => b.defaultUser
    }
  }
  def getReviewerTag(): b.Symbolic =
    b.mkSensitive(_reviewerL, ReviewedBy(b, _reviewerId), EmptyTag(b))
  def showReviewerTag(ctxt: ConfContext): PaperTag = {
    println("showing reviewer tag")
    b.concretize(ctxt, getReviewerTag()).asInstanceOf[PaperTag]
  }

  def setBody (newbody: String) = _body = newbody
  def getBody (): String = _body
  def showBody(ctxt: ConfContext): String = _body

  /* Score. */
  def setProblemScore (newscore: Int) = _problemScore = newscore
  def getProblemScore (): Int = _problemScore
  def showProblemScore (ctxt: ConfContext): Int = _problemScore

  def setBackgroundScore (newscore: Int) = _backgroundScore = newscore
  def getBackgroundScore (): Int = _backgroundScore
  def showBackgroundScore (ctxt: ConfContext): Int = _backgroundScore

  def setApproachScore (newscore: Int) = _approachScore = newscore
  def getApproachScore (): Int = _approachScore
  def showApproachScore (ctxt: ConfContext): Int = _approachScore

  def setResultScore (newscore: Int) = _resultScore = newscore
  def getResultScore (): Int = _resultScore
  def showResultScore (ctxt: ConfContext): Int = _resultScore

  def getPaperReviewRecord(): PaperReviewRecord = {
    transaction { JConfTables.reviews.get(uid.toInt) }
  }

  /* URL links. */
  private val _reviewL = b.mkLevel()
  b.policy ( _reviewL
    , !((b.CONTEXT.viewer.role === b.reviewerStatus) ||
        (b.CONTEXT.viewer.role === b.pcStatus) ||
        ((b.CONTEXT.viewer.role === b.authorStatus) &&
        b.CONTEXT.stage === Public(b)))
    , b.LOW )
  private val _editL = b.mkLevel()
  b.policy( _editL
    , !((b.CONTEXT.viewer~'uid === reviewer)
      && (b.CONTEXT.stage === Review(b)))
    , b.LOW )

  private val _reviewLink: String = "review?id=" + uid + "&key=" + key
  val reviewLink: b.Symbolic = 
    b.mkSensitive(_reviewL, StringVal(b, _reviewLink), StringVal(b, ""))
  def showReviewLink(ctxt: ConfContext): String = {
    (b.concretize(ctxt, reviewLink).asInstanceOf[StringVal]).v
  }
  private val _editReviewLink = "edit_review?id=" + uid + "&key=" + key
  val editReviewLink: b.Symbolic = 
    b.mkSensitive(_editL, StringVal(b, _editReviewLink), StringVal(b, ""))
  def showEditReviewLink(ctxt: ConfContext): String = {
    (b.concretize(ctxt, editReviewLink).asInstanceOf[StringVal]).v
  }
  private val _postReviewLink = "review?id=" + uid + "&key=" + key
  val postReviewLink: b.Symbolic =
    b.mkSensitive(_editL, StringVal(b, _postReviewLink), StringVal(b, ""))
  def showPostReviewLink(ctxt: ConfContext): String = {
    println("showing post review link")
    (b.concretize(ctxt, postReviewLink).asInstanceOf[StringVal]).v
  }
}
