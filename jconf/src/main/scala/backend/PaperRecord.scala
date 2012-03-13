package cap.jeeves.jconf.backend

/*
* User records for jconf case study.
* @author jeanyang
*/

import cap.scalasmt._
import cap.jeeves._
import Expr._

import org.squeryl.PrimitiveTypeMode._

import scala.collection.immutable.List;

sealed trait PaperStage extends Atom with Serializable
case class Submission(b: JConfBackend) extends PaperStage {
  b.register(this)
}
case class Review(b: JConfBackend) extends PaperStage {
  b.register(this)
}
case class Public(b: JConfBackend) extends PaperStage {
  b.register(this)
}

sealed trait PaperTag extends Atom with Serializable {
  def showTag(ctxt: ConfContext): String
}
case class NeedsReview (b: JConfBackend, reviewer: BigInt) extends PaperTag {
  b.register(this)

  def showTag(ctxt: ConfContext): String = { "Needs review " + reviewer }
}
case class ReviewedBy (b: JConfBackend, reviewer: BigInt) extends PaperTag {
  b.register(this)

  def showTag(ctxt: ConfContext): String = { "Reviewed by " + reviewer }
}
case class Accepted (b: JConfBackend) extends PaperTag {
  b.register(this)

  def showTag(ctxt: ConfContext): String = { "Accepted" }
}
case class EmptyTag (b: JConfBackend) extends PaperTag {
  b.register(this)

  def showTag(ctxt: ConfContext): String = { "--" }
}

class PaperRecord(         val b: JConfBackend
                 ,         val uid: BigInt
                 ,         val key: String
                 , private var _title: String
                 , private val _authors: List[BigInt] = Nil
                 , private var _file: String = ""
                 , private var _tags: List[PaperTag] = Nil
                 , private val _conflicts: List[BigInt] )
               extends Atom {
  b.register(this)
 
  /**************/
  /* Variables. */
  /**************/
  private var _authorL = b.mkLevel()
  private var _reviewerL = b.mkLevel()
  private val titleL = b.mkLevel();

  /*************/
  /* Policies. */
  /*************/
  private val isAuthor: Formula = authors.has(b.CONTEXT.viewer~'uid);
  private val isInternal: Formula =
    (b.CONTEXT.viewer.role === b.reviewerStatus) ||
    (b.CONTEXT.viewer.role === b.pcStatus)
  private val isPC: Formula =
    (b.CONTEXT.viewer.role === b.pcStatus)
  private val authorCanSeeReview: Formula =
    (b.CONTEXT.stage === Public(b))
  private def isPublic : Formula =
    (b.CONTEXT.stage === Public(b)) && (getTags ()).has(Accepted(b))

  b.policy ( _authorL
         , !(isAuthor || isPC || isPublic)
         , b.LOW);
  b.logPaperRecordPolicy();
  b.policy (titleL
    , !(isAuthor
      || isInternal || isPublic), b.LOW);
  b.logPaperRecordPolicy();

  /************************/
  /* Getters and setters. */
  /************************/
  def setTitle(name: String) = {
    _title = name
    title =
      b.mkSensitive(titleL, StringVal(b, _title), b.emptyStringVal)
  }
  var title: b.Symbolic =
    b.mkSensitive(titleL, StringVal(b, _title), b.emptyStringVal)
  def showTitle(ctxt: ConfContext): String = {
    println("showing paper title")
    (b.concretize(ctxt, title).asInstanceOf[StringVal]).v
  }

  def authors : List[IntExpr] = {
    _authors.map(author => b.mkSensitiveInt(_authorL, author, -1))
  }
  def showAuthors(ctxt: ConfContext): List[BigInt] = {
    authors.map(a => b.concretize(ctxt, a).asInstanceOf[BigInt])
  }

  def getFile(): String = _file
  def setFile(file: String): Unit = _file = file
  def showFile(ctxt: ConfContext): String = _file

  /* Managing tags. */
  private def addTagPermission (tag : PaperTag) : b.Symbolic = {
    val level = b.mkLevel ();
    tag match {
      case NeedsReview(b, reviewerId) =>
        b.policy (level
          , !(isPC || (b.CONTEXT.viewer~'uid === reviewerId))
          , b.LOW);
        b.logPaperRecordPolicy();
      case ReviewedBy (b, reviewer) =>
        b.policy (level, !isPC, b.LOW);
        b.logPaperRecordPolicy();
      // Can see the "Accepted(b)" tag if is an internal user at the decision
      // stage or if all information is visible.
      case Accepted(b) =>
        b.policy (level
          , !(isInternal || b.CONTEXT.stage === Public(b))
          , b.LOW);
        b.logPaperRecordPolicy();
      case EmptyTag(b) => ()
    }
    b.mkSensitive(level, tag, EmptyTag(b))
  }

  def addTag (newtag: PaperTag) = {
    _tags = newtag::_tags
    JConfTables.addDBTag(b.conversions, uid.toInt, newtag)
  }
  def getTags (): List[b.Symbolic] = {
    _tags.map(t => addTagPermission(t))
  }
  def removeTag (tag : PaperTag) : Unit = {
    _tags.filterNot(_ == tag)
    JConfTables.removeDBTag(b.conversions, uid.toInt, tag)
  }
  def hasTag (tag : b.Symbolic) : Formula = (getTags ()).has(tag)
  def showTags (ctxt: ConfContext): List[PaperTag] = {
    (getTags ()).map(t => b.concretize(ctxt, t).asInstanceOf[PaperTag])
  }

  def hasConflict(reviewer: BigInt): Boolean = {
    _conflicts.exists(_ == reviewer)
  }

  // We only add to reviews and don't take away.
  var reviews: List[b.Symbolic] = {
    JConfTables.getReviewsByPaper(b, uid.toInt).map(r => addReviewPolicy(r))
  }
  /*
    Adds a review to the b.
  */
  def addReview (reviewer: ConfUser, body: String = ""
    , problemScore: Int = 3, backgroundScore: Int = 3
    , approachScore: Int = 3, resultScore: Int =3)
  : PaperReview = {
   val (reviewUid, reviewKey) =
     b.getReviewUid(uid.toInt, reviewer.uid.toInt, body
     , problemScore, backgroundScore, approachScore, resultScore);
   val r =
     new PaperReview(b, reviewUid, reviewKey
     , uid, reviewer.uid, body
     , problemScore, backgroundScore, approachScore, resultScore);
   addTag(ReviewedBy(b, reviewer.uid))
   reviews = addReviewPolicy(r) :: reviews
   r
  }
  def isReviewedBy(reviewer: ConfUser): Formula = {
    hasTag(ReviewedBy(b, reviewer.uid))
  }
  def showNeedsReviewBy(ctxt: ConfContext): Boolean = {
    b.concretize(ctxt, hasTag(NeedsReview(b, ctxt.viewer.uid)))
  }
  def showIsReviewedBy(ctxt: ConfContext, reviewer: ConfUser): Boolean = {
    b.concretize(ctxt, isReviewedBy(reviewer))
  }
  def addReviewPolicy (r: PaperReview): b.Symbolic = {
    val level = b.mkLevel();
    b.policy( level
            , !( (isInternal && (!isAuthor)) ||
                (isAuthor && authorCanSeeReview) )
            , b.LOW );
    b.logPaperRecordPolicy();
    b.mkSensitive(level, r, b.defaultReview)
  }
  def showReviews (ctxt: ConfContext): List[PaperReview] = {
    reviews.map(r => b.concretize(ctxt, r).asInstanceOf[PaperReview])
  }
  def getReviewByReviewer (reviewerId: BigInt): b.Symbolic = {
    val r = {
      JConfTables.getReviewByPaperReviewer(b, uid.toInt, reviewerId.toInt) match {
        case Some(r) => r
        case None => b.defaultReview
      }
    }
    addReviewPolicy(r)
  }
  def showReviewByReviewer(ctxt: ConfContext, reviewerId: BigInt)
    : PaperReview = {
    b.concretize(ctxt, getReviewByReviewer(reviewerId)).asInstanceOf[PaperReview]
  }

  def showIsAuthor (ctxt: ConfContext): Boolean = b.concretize(ctxt, isAuthor)

  def getPaperItemRecord(): PaperItemRecord =
    transaction { JConfTables.papers.get(uid.toInt) }
  
  def debugPrint(): Unit = {
    _authors.foreach(a => println(a))
  }

  /* Writing paper to file. */

  /* URLs. */
  private val _editL = b.mkLevel()
  b.policy(_editL
    , !(isAuthor
      && (b.CONTEXT.stage === Submission(b)))
    , b.LOW)

  def showLink(ctxt: ConfContext): String = {
    "paper?id=" + uid + "&key=" + key
  }

  private val path: String = new java.io.File("").getAbsolutePath()
  def getBackupLoc(): b.Symbolic = {
    val backupLoc = path + "/papers/" + "jcp" + key + "_" + _file
    b.mkSensitive(_editL, StringVal(b, backupLoc), b.emptyStringVal)
  }
  def getTomcatLoc(): b.Symbolic = {
    val tomcatLoc = path + "/webapps/src2012/papers/" + "jcp" + key + "_" + _file
    b.mkSensitive(_editL, StringVal(b, tomcatLoc), b.emptyStringVal)
  }
  // Permanent storage location for file.
  def showFileLocations(ctxt: ConfContext): (String, String) = {
    val backupLoc = {
      b.concretize(ctxt, getBackupLoc ()).asInstanceOf[StringVal]
    }
    val tomcatLoc = b.concretize(ctxt, getTomcatLoc ()).asInstanceOf[StringVal]
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

  private val _assignL = b.mkLevel ()
  b.policy (_assignL, !isPC, b.LOW)
  private val _assignLink = "assign_paper?id=" + uid + "&key=" + key
  def getAssignLink(userId: BigInt): b.Symbolic = {
    b.mkSensitive(_assignL
      , StringVal(b, _assignLink + "&userId=" + userId)
      , b.emptyStringVal)
  }
  def showAssignLink(ctxt: ConfContext, userId: BigInt): String = {
    b.concretize(
      ctxt, getAssignLink(userId)).asInstanceOf[StringVal].v
  }

  private val _editLink = "edit_paper?id=" + uid + "&key=" + key
  val editLink: b.Symbolic = {
    b.mkSensitive(_editL, StringVal(b, _editLink), b.emptyStringVal)
  }
  def showEditLink(ctxt: ConfContext): String = {
    b.concretize(ctxt, editLink).asInstanceOf[StringVal].v
  }
  private val _postLink = "paper?id=" + uid + "&key=" + key
  def postLink: b.Symbolic = {
    b.mkSensitive(_editL, StringVal(b, _postLink), b.emptyStringVal)
  }
  def showPostLink(ctxt: ConfContext): String = {
    (b.concretize(ctxt, postLink).asInstanceOf[StringVal]).v
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
