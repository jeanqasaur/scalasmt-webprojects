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

case class Title (name : String) extends JeevesRecord

class PaperRecord( val id : Int
                 , _name : Title, authors : List[ConfUser]
                 , tags : List[PaperTag] ) extends JeevesRecord {
  private def isPublic (curtags : List[Symbolic]) : Formula =
    (CONTEXT.stage === Public) && curtags.has(Accepted)

  // Some formulas.
  private val isAuthor: Formula = (getAuthors()).has(CONTEXT.viewer);
  private val isInternal: Formula =
    (CONTEXT.viewer.role === ReviewerStatus) ||
    (CONTEXT.viewer.role === PCStatus)

  // The name of the paper is always visible to the authors.
  private var name = _name;
  private val titleL = mkLevel();
  policy (titleL, !(isAuthor || isInternal || isPublic(getTags ())), LOW);
  def setTitle(_name: Title) = name = _name
  def getTitle(): Symbolic =  mkSensitive(titleL, name, Title(""))

  private var _authors: List[ConfUser] = authors
  private var _authorL = mkLevel()
  policy ( _authorL
         , !(isAuthor || (isInternal && (CONTEXT.stage === Decision)) ||
            isPublic(getTags ()))
         , LOW);
  def getAuthors() : List[Symbolic] =
    _authors.map(a => mkSensitive(_authorL, a, NULL))

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
    }
    mkSensitive(level, tag, NULL)
  }

  private var _tags : List[PaperTag] = tags
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
  
  private var _reviews: List[PaperReview] = Nil
  private var authorCanSeeReview: Formula =
    (CONTEXT.stage === Rebuttal) || (CONTEXT.stage === Decision)
  def addReview (reviewer: ConfUser, body: String, score: Int) = {
    val reviewId = getReviewId();
    val r = new PaperReview(reviewId, reviewer, body, score);
    _reviews = r::_reviews;
    addTag(ReviewedBy(reviewer))
  }
  def addReviewPolicy (r: PaperReview): Symbolic = {
    val reviewerTag = r.getReviewerTag ();
    val level = mkLevel();
    policy( level
            , !((CONTEXT.stage === Review && (hasTag (reviewerTag))) ||
                ((CONTEXT.stage === Decision) && isInternal) ||
                (isAuthor && authorCanSeeReview))
            , LOW);
    mkSensitive(level, r, NULL)
  }
  def getReviews (): List[Symbolic] = _reviews.map(r => addReviewPolicy(r))
}
