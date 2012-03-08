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
object Public extends PaperStage

sealed trait PaperTag extends JeevesRecord {
  def showTag(ctxt: ConfContext): String
}
case class NeedsReview (reviewer: BigInt) extends PaperTag {
  def showTag(ctxt: ConfContext): String = {
    "Needs review " + reviewer
  }
}
case class ReviewedBy (reviewer: BigInt) extends PaperTag {
  def showTag(ctxt: ConfContext): String = {
    "Reviewed by " + reviewer
  }
}
object Accepted extends PaperTag {
  def showTag(ctxt: ConfContext): String = {
    "Accepted"
  }
}
object EmptyTag extends PaperTag {
  def showTag(ctxt: ConfContext): String = {
    "--"
  }
}

class PaperRecord(         val uid: BigInt
                 ,         val key: String
                 , private var _title: String
                 , private var _authors: List[ConfUser] = Nil
                 , private var _file: String = ""
                 , private var _tags: List[PaperTag] = Nil
                 , private val _conflicts: List[BigInt] )
               extends JeevesRecord with Serializable {
  /**************/
  /* Variables. */
  /**************/
  private var _authorL = mkLevel()
  private var _reviewerL = mkLevel()
  private val titleL = mkLevel();

  /*************/
  /* Policies. */
  /*************/
  private val isAuthor: Formula =
    (getAuthors ()).hasFormula((a: Symbolic) => a~'uid === CONTEXT.viewer~'uid);
  private val isInternal: Formula =
    (CONTEXT.viewer.role === ReviewerStatus) ||
    (CONTEXT.viewer.role === PCStatus)
  private val isPC: Formula =
    (CONTEXT.viewer.role === PCStatus)
  private val authorCanSeeReview: Formula =
    (CONTEXT.stage === Public)
  private def isPublic : Formula =
    (CONTEXT.stage === Public) && (getTags ()).has(Accepted)

  policy ( _authorL
         , !(isAuthor || isPC || isPublic)
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
  def showFile(ctxt: ConfContext): String = _file

  /* Managing tags. */
  private def addTagPermission (tag : PaperTag) : Symbolic = {
    val level = mkLevel ();
    tag match {
      case NeedsReview (reviewerId) =>
        policy (level
          , !(isPC || (CONTEXT.viewer~'uid === reviewerId))
          , LOW);
        logPaperRecordPolicy();
      case ReviewedBy (reviewer) =>
        policy (level, !isPC, LOW);
        logPaperRecordPolicy();
      // Can see the "Accepted" tag if is an internal user at the decision
      // stage or if all information is visible.
      case Accepted =>
        policy (level, !(isInternal || CONTEXT.stage === Public), LOW);
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
   val (reviewUid, reviewKey) =
     getReviewUid(uid.toInt, reviewer.uid.toInt, body
     , problemScore, backgroundScore, approachScore, resultScore);
   val r =
     new PaperReview(reviewUid, reviewKey
     , uid, reviewer.uid, body
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
    val level = mkLevel();
    policy( level
            , !( isInternal || (isAuthor && authorCanSeeReview) )
            , LOW );
    logPaperRecordPolicy();
    mkSensitive(level, r, defaultReview)
  }
  def getReviews (): List[Symbolic] = {
    JConfTables.getReviewsByPaper(uid.toInt).map(r => addReviewPolicy(r))
  }
  def showReviews (ctxt: ConfContext): List[PaperReview] = {
    (getReviews ()).map(r => concretize(ctxt, r).asInstanceOf[PaperReview])
  }
  def getReviewByReviewer (reviewerId: BigInt): Symbolic = {
    val r = {
      JConfTables.getReviewByPaperReviewer(uid.toInt, reviewerId.toInt) match {
        case Some(r) => r
        case None => defaultReview
      }
    }
    addReviewPolicy(r)
  }
  def showReviewByReviewer(ctxt: ConfContext, reviewerId: BigInt)
    : PaperReview = {
    show[PaperReview](ctxt, getReviewByReviewer(reviewerId))
  }

  def showIsAuthor (ctxt: ConfContext): Boolean = concretize(ctxt, isAuthor)

  def getPaperItemRecord(): PaperItemRecord =
    transaction { JConfTables.papers.get(uid.toInt) }
  
  def debugPrint(): Unit = {
    _authors.foreach(a => a.debugPrint())
  }

  /* Writing paper to file. */

  /* URLs. */
  private val _editL = mkLevel()
  policy(_editL, !(isAuthor && CONTEXT.stage === Submission), LOW)

  def showLink(ctxt: ConfContext): String = {
    "paper?id=" + uid + "&key=" + key
  }

  private val path: String = new java.io.File("").getAbsolutePath()
  def getBackupLoc(): Symbolic = {
    val backupLoc = path + "/papers/" + "jcp" + key + "_" + _file
    mkSensitive(_editL, StringVal(backupLoc), StringVal(""))
  }
  def getTomcatLoc(): Symbolic = {
    val tomcatLoc = path + "/webapps/jconf/papers/" + "jcp" + key + "_" + _file
    mkSensitive(_editL, StringVal(tomcatLoc), StringVal(""))
  }
  // Permanent storage location for file.
  def showFileLocations(ctxt: ConfContext): (String, String) = {
    val backupLoc = show[StringVal](ctxt, getBackupLoc ())
    val tomcatLoc = show[StringVal](ctxt, getTomcatLoc ())
    (backupLoc.v, tomcatLoc.v)
  }
  // Where the file is stored for display online.
  def getFileStorageLocation(paperSecretId: String, filename: String): String = {
    path + "/webapps/jconf/papers/" + "jcp" + paperSecretId + "_" + filename
  }

  // The public link for this directory.
  def showFileDisplayLocation(ctxt: ConfContext): String = {
    "papers/" + "jcp" + key + "_" + showFile(ctxt)
  }

  private val _editLink = "edit_paper?id=" + uid + "&key=" + key
  def getEditLink(): Symbolic = {
    mkSensitive(_editL, StringVal(_editLink), StringVal(""))
  }
  def showEditLink(ctxt: ConfContext): String = {
    show[StringVal](ctxt, getEditLink()).v
  }
  private val _postLink = "paper?id=" + uid + "&key=" + key
  def postLink: Symbolic = {
    mkSensitive(_editL, StringVal(_postLink), StringVal(""))
  }
  def showPostLink(ctxt: ConfContext): String = {
    show[StringVal](ctxt, postLink).v
  }

  def getUploadLink(file: String): String = {
    if (!file.isEmpty()) {
      "uploadedPaper?id=" + uid + "&key=" + key + "&filename=" + file
    } else { "" }
  }
  def showUploadLink(ctxt: ConfContext): String = {
    getUploadLink(showFile(ctxt))
  }
}
