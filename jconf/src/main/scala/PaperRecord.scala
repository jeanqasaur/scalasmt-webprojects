package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._

import org.squeryl.PrimitiveTypeMode._

import scala.collection.immutable.List;
import scala.collection.mutable.Map;

import Expr._
import JConfBackend._

sealed trait PaperStage extends JeevesRecord
object Submission extends PaperStage
object Review extends PaperStage
object Rebuttal extends PaperStage
object Decision extends PaperStage
object Public extends PaperStage

sealed trait PaperTag extends JeevesRecord
case class NeedsReview (reviewer: BigInt) extends PaperTag
case class ReviewedBy (reviewer: BigInt) extends PaperTag
object Accepted extends PaperTag
object EmptyTag extends PaperTag

case class Title (title : String) extends JeevesRecord

class PaperRecord( val uid: BigInt = -1
                 , private var _title: Title = Title("Untitled")
                 , private var _authors: List[ConfUser] = Nil
                 , private var _tags: List[PaperTag] = Nil)
               extends JeevesRecord with Serializable {
  _authors.foreach(a => JConfTables.writeDBAuthor(uid.toInt, a.uid.toInt))
  
  /**************/
  /* Variables. */
  /**************/
  private var _authorL = mkLevel()
  private val titleL = mkLevel();

  /*************/
  /* Policies. */
  /*************/
  private val isAuthor: Formula =
    (getAuthors ()).hasFormula((a: Symbolic) => a~'uid === CONTEXT.viewer~'uid);
  private val isInternal: Formula =
    (CONTEXT.viewer.role === ReviewerStatus) ||
    (CONTEXT.viewer.role === PCStatus)
  private val authorCanSeeReview: Formula =
    (CONTEXT.stage === Rebuttal) || (CONTEXT.stage === Decision)
  private def isPublic : Formula =
    (CONTEXT.stage === Public) && (getTags ()).has(Accepted)

  policy ( _authorL
         , !(isAuthor
           || (isInternal && (CONTEXT.stage === Decision))
           || isPublic)
         , LOW);
  logPaperRecordPolicy();
  policy (titleL
    , !(isAuthor
      || isInternal || isPublic), LOW);
  logPaperRecordPolicy();

  /************************/
  /* Getters and setters. */
  /************************/
  def setTitle(name: Title) = _title = name
  var title: Symbolic = mkSensitive(titleL, _title, Title("No permission"))
  def getTitle(): Symbolic =  mkSensitive(titleL, _title, Title("No permission"))
  def showTitle(ctxt: ConfContext): String =
    (concretize(ctxt, getTitle ()).asInstanceOf[Title]).title

  def getAuthors() : List[Symbolic] = {
    _authors.map(author => mkSensitive(_authorL, author, defaultUser))
  }
  def showAuthors(ctxt: ConfContext): List[ConfUser] = {
    (getAuthors ()).map(a => concretize(ctxt, a).asInstanceOf[ConfUser])
  }

  /* Managing tags. */
  private def addTagPermission (tag : PaperTag) : Symbolic = {
    val level = mkLevel ();
    tag match {
      case NeedsReview (reviewer) =>
        val canSee : Formula = isInternal && CONTEXT.stage === Review;
        policy (level, !canSee, LOW);
        logPaperRecordPolicy();
      case ReviewedBy (reviewer) =>
        policy (level, !isInternal, LOW);
        logPaperRecordPolicy();
      // Can see the "Accepted" tag if is an internal user at the decision
      // stage or if all information is visible.
      case Accepted =>
        val stage = CONTEXT.stage;
        val canSee : Formula =
          (isInternal && (stage == Decision)) || (stage === Public);
        policy (level, !canSee, LOW);
        logPaperRecordPolicy();
      case EmptyTag => ()
    }
    mkSensitive(level, tag, EmptyTag)
  }

  def addTag (newtag: PaperTag) = {
    _tags = newtag::_tags
    JConfTables.addDBTag(uid.toInt, newtag)
  }
  def getTags (): List[Symbolic] = {
    _tags.map(t => addTagPermission(t))
  }
  def removeTag (tag : PaperTag) : Unit = {
    _tags.filterNot(_ == tag)
    JConfTables.removeDBTag(uid.toInt, tag)
  }
  def hasTag (tag : Symbolic) : Formula = (getTags ()).has(tag)
  def showTags (ctxt: ConfContext): List[PaperTag] = {
    (getTags ()).map(t => concretize(ctxt, t).asInstanceOf[PaperTag])
  }

  def addReview (reviewer: ConfUser, body: String, score: Int): PaperReview = {
   val reviewUid = getReviewUid();
   val r = new PaperReview(reviewUid, uid, reviewer.uid, body, score);
   JConfTables.writeDB(r);
   addTag(ReviewedBy(reviewer.uid))
   r
  }
  def isReviewedBy(reviewer: ConfUser): Formula = {
    hasTag(ReviewedBy(reviewer.uid))
  }
  def needsReviewBy(ctxt: ConfContext): Formula = {
    hasTag(NeedsReview(ctxt.viewer.uid))
  }
  def showIsReviewedBy(ctxt: ConfContext, reviewer: ConfUser): Boolean = {
    concretize(ctxt, isReviewedBy(reviewer))
  }
  def addReviewPolicy (r: PaperReview): Symbolic = {
    val reviewerTag = r.getReviewerTag ();
    val level = mkLevel();
    policy( level
            , !((CONTEXT.stage === Review && (hasTag (reviewerTag))) ||
                ((CONTEXT.stage === Decision) && isInternal) ||
                (isAuthor && authorCanSeeReview))
            , LOW);
    logPaperRecordPolicy();
    mkSensitive(level, r, new PaperReview())
  }
  def getReviews (): List[Symbolic] = {
    JConfTables.getReviewsByPaper(uid.toInt).map(r => addReviewPolicy(r))
  }
  def showReviews (ctxt: ConfContext): List[PaperReview] = {
    (getReviews ()).map(r => concretize(ctxt, r).asInstanceOf[PaperReview])
  }

  def getPaperItemRecord(): PaperItemRecord = {
    new PaperItemRecord(uid.toInt, _title.title)
  }
  def debugPrint(): Unit = {
    println("PaperRecord(id=" + uid + ",title=" + _title + ")")
    _authors.foreach(a => a.debugPrint())
  }
}
