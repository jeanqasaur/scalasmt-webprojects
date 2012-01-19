package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._
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
object NeedsReview extends PaperTag
case class ReviewedBy (reviewer: Symbolic) extends PaperTag
object Accepted extends PaperTag
object EmptyTag extends PaperTag

case class Title (name : String) extends JeevesRecord

class PaperRecord( val id: Int = -1
                 , name: Title = Title("Untitled")
                 , authors: List[ConfUser] = Nil
                 , tags: List[PaperTag] = Nil
                 , reviews: List[PaperReview] = Nil )
               extends JeevesRecord with Serializable {
  /**************/
  /* Variables. */
  /**************/
  private var _authorL = mkLevel()
  private val titleL = mkLevel();

  private var _title = name;
  val title: Symbolic = mkSensitive(titleL, _title, Title("No permission"))

  private var _authors: List[ConfUser] = authors
  private var _tags : List[PaperTag] = tags
  private var _reviews: List[PaperReview] = reviews

  /*************/
  /* Policies. */
  /*************/
  private val isAuthor: Formula = (getAuthors()).has(CONTEXT.viewer);
  private val isInternal: Formula =
    (CONTEXT.viewer.role === ReviewerStatus) ||
    (CONTEXT.viewer.role === PCStatus)
  private val authorCanSeeReview: Formula =
    (CONTEXT.stage === Rebuttal) || (CONTEXT.stage === Decision)
  private def isPublic (curtags : List[Symbolic]) : Formula =
    (CONTEXT.stage === Public) && curtags.has(Accepted)

  policy ( _authorL
         , !(isAuthor || (isInternal && (CONTEXT.stage === Decision)) ||
            isPublic(getTags ()))
         , LOW);
  policy (titleL, !(isAuthor || isInternal || isPublic(getTags ())), LOW);

  /************************/
  /* Getters and setters. */
  /************************/
  def setTitle(name: Title) = _title = name
  def getTitle(): Symbolic =  mkSensitive(titleL, _title, Title("No permission"))
  def showTitle(ctxt: ConfContext): String =
    (concretize(ctxt, getTitle ()).asInstanceOf[Title]).name

  def getAuthors() : List[Symbolic] = {
    _authors.map(a => mkSensitive(_authorL, a, new ConfUser()))
  }
  def showAuthors(ctxt: ConfContext): List[ConfUser] = {
    _authors.map(a => concretize(ctxt, a).asInstanceOf[ConfUser])
  }

  /* Managing tags. */
  private def addTagPermission (tag : PaperTag) : Symbolic = {
    val level = mkLevel ();
    tag match {
      case NeedsReview =>
        val canSee : Formula = isInternal && CONTEXT.stage === Review;
        policy (level, canSee, HIGH);
        policy (level, !canSee, LOW);
      case ReviewedBy (reviewer) =>
        policy (level, isInternal, HIGH);
        policy (level, !isInternal, LOW)
      // Can see the "Accepted" tag if is an internal user at the decision
      // stage or if all information is visible.
      case Accepted =>
        val stage = CONTEXT.stage;
        val canSee : Formula =
          (isInternal && (stage == Decision)) || (stage === Public);
        policy (level, canSee, HIGH);
        policy (level, !canSee, LOW);
      case EmptyTag => ()
    }
    mkSensitive(level, tag, EmptyTag)
  }

  def addTag (newtag: PaperTag) = _tags = newtag::_tags
  def getTags (): List[Symbolic] = _tags.map(t => addTagPermission(t))
  def removeTag (tag : PaperTag) : Unit =
    _tags = _tags.filterNot(t => t.equals(tag))
  def hasTag (tag : Symbolic) : Formula = (getTags ()).has(tag)

  /* Managing reviews. */
  private var reviewIds = 0;
  private def getReviewId () : Int = {
    val id = reviewIds;
    reviewIds = reviewIds + 1;
    id
  }
  
  def addReview (reviewer: ConfUser, body: String, score: Int): PaperReview = {
    val reviewId = getReviewId();
    val r = new PaperReview(reviewId, reviewer, body, score);
    _reviews = r::_reviews;
    addTag(ReviewedBy(reviewer))
    r
  }
  def addReviewPolicy (r: PaperReview): Symbolic = {
    val reviewerTag = r.getReviewerTag ();
    val level = mkLevel();
    policy( level
            , !((CONTEXT.stage === Review && (hasTag (reviewerTag))) ||
                ((CONTEXT.stage === Decision) && isInternal) ||
                (isAuthor && authorCanSeeReview))
            , LOW);
    mkSensitive(level, r, new PaperReview())
  }
  def getReviews (): List[Symbolic] = _reviews.map(r => addReviewPolicy(r))
  def showReviews (ctxt: ConfContext): List[PaperReview] = {
    (getReviews ()).map(r => concretize(ctxt, r).asInstanceOf[PaperReview])
  }
}
