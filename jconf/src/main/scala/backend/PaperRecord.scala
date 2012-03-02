package cap.jeeves.jconf.backend

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._

import org.squeryl.PrimitiveTypeMode._

import scala.collection.immutable.List;

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

class PaperRecord(         val uid: BigInt
                 , private var _title: String
                 , private var _authors: List[ConfUser] = Nil
                 , private var _file: String = ""
                 , private var _tags: List[PaperTag] = Nil )
               extends JeevesRecord with Serializable {
  // We record the authors as well.
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
  def setTitle(name: String) = {
    _title = name
    title = mkSensitive(titleL, StringVal(_title), StringVal("No permission"))
  }
  var title: Symbolic =
    mkSensitive(titleL, StringVal(_title), StringVal("No permission"))
  def showTitle(ctxt: ConfContext): String =
    (concretize(ctxt, title).asInstanceOf[StringVal]).v

  def addAuthor(author: ConfUser): Unit = {
    _authors = author::_authors
  }
  def getAuthors() : List[Symbolic] = {
    _authors.map(author => mkSensitive(_authorL, author, defaultUser))
  }
  def showAuthors(ctxt: ConfContext): List[ConfUser] = {
    (getAuthors ()).map(a => concretize(ctxt, a).asInstanceOf[ConfUser])
  }

  def getFile(): String = _file
  def setFile(file: String): Unit = _file = file

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

  def addReview (reviewer: ConfUser, body: String = ""
    , problemScore: Int = 3, backgroundScore: Int = 3
    , approachScore: Int = 3, resultScore: Int =3)
  : PaperReview = {
   val reviewUid =
     getReviewUid(uid.toInt, reviewer.uid.toInt, body
     , problemScore, backgroundScore, approachScore, resultScore);
   val r =
     new PaperReview(reviewUid, uid, reviewer.uid, body
     , problemScore, backgroundScore, approachScore, resultScore);
   addTag(ReviewedBy(reviewer.uid))
   r
  }
  def isReviewedBy(reviewer: ConfUser): Formula = {
    hasTag(ReviewedBy(reviewer.uid))
  }
  def showNeedsReviewBy(ctxt: ConfContext): Boolean = {
    concretize(ctxt, hasTag(NeedsReview(ctxt.viewer.uid)))
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
    mkSensitive(level, r, defaultReview)
  }
  def getReviews (): List[Symbolic] = {
    JConfTables.getReviewsByPaper(uid.toInt).map(r => addReviewPolicy(r))
  }
  def showReviews (ctxt: ConfContext): List[PaperReview] = {
    (getReviews ()).map(r => concretize(ctxt, r).asInstanceOf[PaperReview])
  }

  def showIsAuthor (ctxt: ConfContext): Boolean = {
    concretize(ctxt, isAuthor)
  }

  def getPaperItemRecord(): PaperItemRecord =
    transaction { JConfTables.papers.get(uid.toInt) }
  
  def debugPrint(): Unit = {
    println("PaperRecord(id=" + uid + ",title=" + _title + ")")
    _authors.foreach(a => a.debugPrint())
  }
}
