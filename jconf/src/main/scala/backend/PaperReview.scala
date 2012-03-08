package cap.jeeves.jconf.backend

/*
 * User records for jconf case study.
 * @author jeanyang
 */
import cap.scalasmt._
import org.squeryl.PrimitiveTypeMode._

import JConfBackend._

class PaperReview(
            val  uid: BigInt
  ,         val  key: String
  ,         var  paperId: BigInt = -1
  , private val _reviewerId: BigInt = -1
  , private var _body: String = ""
  , private var _problemScore: Int = 3
  , private var _backgroundScore: Int = 3
  , private var _approachScore: Int = 3
  , private var _resultScore: Int = 3)
  extends JeevesRecord with Serializable {
  /*************/
  /* Policies. */
  /*************/
  private val _reviewerL = mkLevel ();
  private val _scoreL = mkLevel ();
  private val _isInternalF: Formula = {
    val vrole = CONTEXT.viewer.role;
    (vrole === ReviewerStatus) || (vrole === PCStatus);
  }
  policy(_reviewerL,
    !((CONTEXT.viewer~'uid === _reviewerId)
      || (CONTEXT.viewer.role === PCStatus))
    , LOW);
  logPaperReviewPolicy();

  val reviewer: IntExpr = mkSensitiveInt(_reviewerL, _reviewerId, -1)
  def showReviewer(ctxt: ConfContext): ConfUser = {
    val reviewerId: BigInt = concretize(ctxt, reviewer).asInstanceOf[BigInt]
    JConfBackend.getUserById(reviewerId.toInt) match {
      case Some(u) => u
      case None => 
        println("Cannot find reviewer " + reviewerId.toInt)
        JConfBackend.defaultUser
    }
  }
  def getReviewerTag(): Symbolic =
    mkSensitive(_reviewerL, ReviewedBy(_reviewerId), EmptyTag)
  def showReviewerTag(ctxt: ConfContext): PaperTag = {
    concretize(ctxt, getReviewerTag()).asInstanceOf[PaperTag]
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
  private val _editL = mkLevel()
  policy( _editL
    , !((CONTEXT.viewer~'uid === reviewer) && (CONTEXT.stage === Review))
    , LOW )

  def showReviewLink(ctxt: ConfContext): String = {
    "review?id=" + uid + "&key=" + key
  }
  private val _editReviewLink = "edit_review?id=" + uid + "&key=" + key
  val editReviewLink: Symbolic = 
    mkSensitive(_editL, StringVal(_editReviewLink), StringVal(""))
  def showEditReviewLink(ctxt: ConfContext): String = {
    show[StringVal](ctxt, editReviewLink).v
  }
  private val _postReviewLink = "review?id=" + uid + "&key=" + key
  val postReviewLink: Symbolic =
    mkSensitive(_editL, StringVal(_postReviewLink), StringVal(""))
  def showPostReviewLink(ctxt: ConfContext): String = {
    show[StringVal](ctxt, postReviewLink).v
  }
}
