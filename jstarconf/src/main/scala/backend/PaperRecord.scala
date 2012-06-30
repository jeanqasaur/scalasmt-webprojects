package cap.jeeves.jconf.backend

/*
* User records for jconf case study.
* @author jeanyang
*/

import JConfBackend._
import cap.scalasmt._

import org.squeryl.PrimitiveTypeMode._

import scala.collection.immutable.List;

sealed trait PaperStage extends JeevesRecord
case object Submission extends PaperStage
case object Review extends PaperStage
case object Public extends PaperStage

sealed trait PaperTag extends JeevesRecord {
  def showTag(ctxt: ConfContext): String
}
case class NeedsReview (reviewer: BigInt) extends PaperTag {
  def showTag(ctxt: ConfContext): String = { "Needs review " + reviewer }
}
case class ReviewedBy (reviewer: BigInt) extends PaperTag {
  def showTag(ctxt: ConfContext): String = { "Reviewed by " + reviewer }
}
case object Accepted extends PaperTag {
  def showTag(ctxt: ConfContext): String = { "Accepted" }
}
case object EmptyTag extends PaperTag {
  def showTag(ctxt: ConfContext): String = { "--" }
}

class PaperRecord(         val uid: BigInt
                 ,         val key: String
                 , private var _title: String
                 , private val _authors: List[BigInt] = Nil
                 , private var _file: String = ""
                 , private var _tags: List[PaperTag] = Nil
                 , private val _conflicts: List[BigInt] )
               extends JeevesRecord {
  /**************/
  /* Variables. */
  /**************/
  private var _authorL = mkLevel()
  private var _reviewerL = mkLevel()
  private val titleL = mkLevel();

  /*************/
  /* Policies. */
  /*************/
  private def isAuthor (ctxt: Symbolic): Formula =
    authors.has(ctxt.viewer~'uid);
  private def isInternal (ctxt: Symbolic): Formula =
    (ctxt.viewer.role === ReviewerStatus) ||
    (ctxt.viewer.role === PCStatus)
  private def isPC (ctxt: Symbolic): Formula = (ctxt.viewer.role === PCStatus)
  private def authorCanSeeReview (ctxt: Symbolic): Formula =
    (ctxt.stage === Public)
  private def isPublic (ctxt: Symbolic): Formula =
    (ctxt.stage === Public) && (getTags ()).has(Accepted)

  restrict ( _authorL
         , (ctxt: Symbolic) =>
            (isAuthor (ctxt) || isPC (ctxt) || isPublic (ctxt)) );
  logPaperRecordPolicy();
  restrict (titleL
    , (ctxt: Symbolic) => (isAuthor (ctxt)
      || isInternal (ctxt) || isPublic (ctxt)));
  logPaperRecordPolicy();

  /************************/
  /* Getters and setters. */
  /************************/
  def setTitle(name: String) = {
    _title = name
    title =
      mkSensitive(titleL, StringVal(_title), emptyStringVal)
  }
  var title: Symbolic =
    mkSensitive(titleL, StringVal(_title), emptyStringVal)
  def showTitle(ctxt: ConfContext): String = {
    println("showing paper title")
    (concretize(ctxt, title).asInstanceOf[StringVal]).v
  }

  def authors : List[IntExpr] = {
    _authors.map(author => mkSensitiveInt(_authorL, author, -1))
  }
  def showAuthors(ctxt: ConfContext): List[BigInt] = {
    authors.map(a => concretize(ctxt, a).asInstanceOf[BigInt])
  }

  def getFile(): String = _file
  def setFile(file: String): Unit = _file = file
  def showFile(ctxt: ConfContext): String = _file

  /* Managing tags. */
  private def addTagPermission (tag : PaperTag) : Symbolic = {
    val level = mkLevel ();
    tag match {
      case NeedsReview(reviewerId) =>
        restrict (level
          , (ctxt: Symbolic) =>
            (isPC (ctxt) || (ctxt.viewer~'uid === reviewerId)) );
        logPaperRecordPolicy();
      case ReviewedBy (reviewer) =>
      restrict (level, (ctxt: Symbolic) => isPC (ctxt));
        logPaperRecordPolicy();
      // Can see the "Accepted(b)" tag if is an internal user at the decision
      // stage or if all information is visible.
      case Accepted =>
        restrict (level
          , (ctxt: Symbolic) => (isInternal (ctxt) || ctxt.stage === Public) );
        logPaperRecordPolicy();
      case EmptyTag => ()
    }
    mkSensitive(level, tag, EmptyTag)
  }

  def addTag (newtag: PaperTag) = {
    _tags = newtag::_tags
    JConfTables.addDBTag(conversions, uid.toInt, newtag)
  }
  def getTags (): List[Symbolic] = {
    _tags.map(t => addTagPermission(t))
  }
  def removeTag (tag : PaperTag) : Unit = {
    _tags.filterNot(_ == tag)
    JConfTables.removeDBTag(conversions, uid.toInt, tag)
  }
  def hasTag (tag : Symbolic) : Formula = (getTags ()).has(tag)
  def showTags (ctxt: ConfContext): List[PaperTag] = {
    (getTags ()).map(t => concretize(ctxt, t).asInstanceOf[PaperTag])
  }

  def hasConflict(reviewer: BigInt): Boolean = {
    _conflicts.exists(_ == reviewer)
  }

  // We only add to reviews and don't take away.
  var reviews: List[Symbolic] = {
    JConfTables.getReviewsByPaper(uid.toInt).map(r => addReviewPolicy(r))
  }
  /*
    Adds a review to the backend.
  */
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
   reviews = addReviewPolicy(r) :: reviews
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
    restrict( level
      , (ctxt: Symbolic) => ( (isInternal (ctxt) && (!isAuthor (ctxt))) ||
                (isAuthor (ctxt) && authorCanSeeReview (ctxt)) ) );
    logPaperRecordPolicy();
    mkSensitive(level, r, defaultReview)
  }
  def showReviews (ctxt: ConfContext): List[PaperReview] = {
    reviews.map(r => concretize(ctxt, r).asInstanceOf[PaperReview])
  }
  def getReviewByReviewer (reviewerId: BigInt): Symbolic = {
    val review = {
      JConfTables.getReviewByPaperReviewer(uid.toInt, reviewerId.toInt) match {
        case Some(r) => r
        case None => defaultReview
      }
    }
    addReviewPolicy(review)
  }
  def showReviewByReviewer(ctxt: ConfContext, reviewerId: BigInt)
    : PaperReview = {
    concretize(ctxt, getReviewByReviewer(reviewerId)).asInstanceOf[PaperReview]
  }

  def showIsAuthor (ctxt: ConfContext): Boolean =
    concretize(ctxt, isAuthor (ctxt))

  def getPaperItemRecord(): PaperItemRecord =
    transaction { JConfTables.papers.get(uid.toInt) }
  
  def debugPrint(): Unit = {
    _authors.foreach(a => println(a))
  }

  /* Writing paper to file. */

  /* URLs. */
  private val _editL = mkLevel()
  restrict(_editL
    , (ctxt: Symbolic) => (isAuthor (ctxt) && (ctxt.stage === Submission)) )

  def showLink(ctxt: ConfContext): String = {
    "paper?id=" + uid + "&key=" + key
  }

  private val path: String = new java.io.File("").getAbsolutePath()
  def getBackupLoc(): Symbolic = {
    val backupLoc = path + "/papers/" + "jcp" + key + "_" + _file
    mkSensitive(_editL, StringVal(backupLoc), emptyStringVal)
  }
  def getTomcatLoc(): Symbolic = {
    val tomcatLoc = path + "/webapps/src2012/papers/" + "jcp" + key + "_" + _file
    mkSensitive(_editL, StringVal(tomcatLoc), emptyStringVal)
  }
  // Permanent storage location for file.
  def showFileLocations(ctxt: ConfContext): (String, String) = {
    val backupLoc = {
      concretize(ctxt, getBackupLoc ()).asInstanceOf[StringVal]
    }
    val tomcatLoc = concretize(ctxt, getTomcatLoc ()).asInstanceOf[StringVal]
    (backupLoc.v, tomcatLoc.v)
  }
  // Where the file is stored for display online.
  def getFileStorageLocation(paperSecretId: String, filename: String): String = {
    path + "/webapps/src2012/papers/" + "jcp" + paperSecretId + "_" + filename
  }

  // The public link for this directory.
  def showFileDisplayLocation(ctxt: ConfContext): String = {
    "papers/" + "jcp" + key + "_" + showFile(ctxt)
  }

  private val _assignL = mkLevel ()
  restrict (_assignL, (ctxt: Symbolic) => isPC (ctxt))
  private val _assignLink = "assign_paper?id=" + uid + "&key=" + key
  def getAssignLink(userId: BigInt): Symbolic = {
    mkSensitive(_assignL
      , StringVal(_assignLink + "&userId=" + userId)
      , emptyStringVal)
  }
  def showAssignLink(ctxt: ConfContext, userId: BigInt): String = {
    concretize(
      ctxt, getAssignLink(userId)).asInstanceOf[StringVal].v
  }

  private val _editLink = "edit_paper?id=" + uid + "&key=" + key
  val editLink: Symbolic = {
    mkSensitive(_editL, StringVal(_editLink), emptyStringVal)
  }
  def showEditLink(ctxt: ConfContext): String = {
    concretize(ctxt, editLink).asInstanceOf[StringVal].v
  }

  private val _withdrawLink = "withdraw_paper?id=" + uid + "&key=" + key
  val withdrawLink: Symbolic = {
    mkSensitive(_editL, StringVal(_withdrawLink), emptyStringVal)
  }
  def showWithdrawLink(ctxt: ConfContext): String = {
    concretize(ctxt, withdrawLink).asInstanceOf[StringVal].v
  }

  private val _postLink = "paper?id=" + uid + "&key=" + key
  def postLink: Symbolic = {
    mkSensitive(_editL, StringVal(_postLink), emptyStringVal)
  }
  def showPostLink(ctxt: ConfContext): String = {
    (concretize(ctxt, postLink).asInstanceOf[StringVal]).v
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
