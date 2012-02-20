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
case class NeedsReview (reviewer: ConfUser) extends PaperTag
case class ReviewedBy (reviewer: ConfUser) extends PaperTag
object Accepted extends PaperTag
object EmptyTag extends PaperTag

case class Title (title : String) extends JeevesRecord

class PaperRecord( val id: Int = -1
                 , private var _title: Title = Title("Untitled")
                 , private var _authors: List[ConfUser] = Nil )
               extends JeevesRecord with Serializable {
  /**************/
  /* Variables. */
  /**************/
  private var _authorL = mkLevel()
  private val titleL = mkLevel();

  val title: Symbolic = mkSensitive(titleL, _title, Title("No permission"))

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
    (concretize(ctxt, getTitle ()).asInstanceOf[Title]).title

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
      case NeedsReview (reviewer) =>
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

  def addTag (newtag: PaperTag) = {
    val (tag, tagVal) = Conversions.tag2Field(newtag);
    transaction {
      JConfTables.tags.insert(new PaperTagRecord(id, tag, tagVal))
    }
  }
  def getTags (): List[Symbolic] = {
    transaction {
      val tags: Iterable[PaperTagRecord] = from(JConfTables.tags)(t =>
        where(t.paperId === id.~)
        select(t)
      );
      tags.toList.map(
        t => addTagPermission(Conversions.field2Tag(t.tagId, t.tagData)))
    }
  }
  def removeTag (tag : PaperTag) : Unit = {
    val (tagId, tagVal) = Conversions.tag2Field(tag);
    transaction {
      JConfTables.tags.deleteWhere(t =>
        (t.paperId === id.~) and (t.tagId === tagId.~)
        and (t.tagData === tagVal.~))
    }
  }
  def hasTag (tag : Symbolic) : Formula = (getTags ()).has(tag)
  def showTags (ctxt: ConfContext): List[PaperTag] = {
    (getTags ()).map(t => concretize(ctxt, t).asInstanceOf[PaperTag])
  }

  def addReview (reviewer: ConfUser, body: String, score: Int): PaperReview = {
    transaction {
      val reviewRecord: PaperReviewRecord =
        JConfTables.reviews.insert(
          new PaperReviewRecord(getReviewUid (), reviewer.id, body, score))
      val r = new PaperReview(reviewRecord.id, reviewer, body, score);
      addTag(ReviewedBy(reviewer))
      r
    }
  }
  def isReviewedBy(reviewer: ConfUser): Formula = {
    hasTag(ReviewedBy(reviewer))
  }
  def needsReviewBy(ctxt: ConfContext): Formula = {
    hasTag(NeedsReview(ctxt.viewer))
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
    mkSensitive(level, r, new PaperReview())
  }
  def getReviews (): List[Symbolic] = {
    transaction {
      val reviews: Iterable[PaperReviewRecord] = from(JConfTables.reviews)(r =>
        where(r.id.~ === id)
        select(r)
      )
      reviews.toList.map(r =>
        getUserById(r.reviewer) match {
          case Some(reviewer) =>
            addReviewPolicy(new PaperReview(r.id, reviewer, r.body, r.score))
          case None => throw new NoSuchUserError
        })
    }
  }
  def showReviews (ctxt: ConfContext): List[PaperReview] = {
    (getReviews ()).map(r => concretize(ctxt, r).asInstanceOf[PaperReview])
  }
}
