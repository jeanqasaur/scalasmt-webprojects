package cap.jeeves.jconf.backend

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._

import org.squeryl.PrimitiveTypeMode._

import JConfBackend._

sealed trait UserStatus extends JeevesRecord
case object PublicStatus extends UserStatus
case object AuthorStatus extends UserStatus
case object ReviewerStatus extends UserStatus
case object PCStatus extends UserStatus

/* Conference User */
case class Username (name: String) extends JeevesRecord
case class Name (name: String) extends JeevesRecord
case class Password (val pwd: String) extends JeevesRecord
case class ConfUser(
    val uid: BigInt
  , val username: Username
  , private var _name: Name
  , private var _password: String
  , val role: UserStatus
  , private var _submittedPapers: List[BigInt] = Nil )
  extends JeevesRecord {
    /*************/
    /* Policies. */
    /*************/
    private val isReviewer: Formula =
      CONTEXT.viewer.status === ReviewerStatus
    private val isPC: Formula = CONTEXT.viewer.status === PCStatus

    private val selfL = mkLevel ();
    policy (selfL, !(CONTEXT.viewer.username === username), LOW);
    logConfUserPolicy();
    def isSelf(): Formula = selfL

    // For now, everyone can see the name.
    // TODO: Um, do something about making this less stupid.
    private val nameL = mkLevel ();
//    policy (nameL, false, LOW);

    def setName (n: Name): Unit = {
      _name = n
    }
    def getName (): Symbolic = mkSensitive(nameL, _name, Name("--"))
    def showName (ctxt: ConfContext): String = {
      concretize(ctxt, getName ()).asInstanceOf[Name].name
    }

    // Submitted papers.
    def addSubmittedPaper(p: BigInt) = {
      _submittedPapers = p::_submittedPapers
    }
    def getSubmittedPapers (): List[IntExpr] = {
      _submittedPapers.map(p => mkSensitiveInt(selfL, p, -1))
    }
    def showSubmittedPapers (ctxt: ConfContext): List[PaperRecord] = {
      val paperIds: List[BigInt] =
        (getSubmittedPapers ()).map(p => concretize(ctxt, p));
        paperIds.map(pid =>
          getPaperById(pid.toInt) match {
            case Some(paper) => paper
            case None => defaultPaper
          })
    }

    // Papers to review.
    def getReviewPapers (): List[Symbolic] = {
      val papers: List[PaperRecord] =
        JConfTables.getPapersByReviewer(uid.toInt);
      papers.map(p => mkSensitive(selfL, p, defaultPaper))
    }
    def showReviewPapers (ctxt: ConfContext): List[PaperRecord] = {
      (getReviewPapers ()).map(p =>
        concretize(ctxt, p).asInstanceOf[PaperRecord])
    }

    // Reviews submitted.
    def getReviews (): List[Symbolic] = {
      val reviews: List[PaperReview] =
        JConfTables.getReviewsByReviewer(uid.toInt);
      reviews.map(r => mkSensitive(selfL, r, defaultReview))
    }
    def showReviews (ctxt: ConfContext): List[PaperReview] = {
      (getReviews ()).map(r => concretize(ctxt, r).asInstanceOf[PaperReview])
    }

    // Password.
    def setPassword (p: String) = _password = p
    def getPassword (): Symbolic =
      mkSensitive(selfL, Password(_password), Password("default"))
    def showPassword (ctxt: ConfContext): Password = {
      concretize(ctxt, getPassword ()).asInstanceOf[Password]
    }

    def getConfUserRecord(): ConfUserRecord = {
      transaction { JConfTables.users.get(uid.toInt) }
    }
    def debugPrint(): Unit = {
      println("ConfUser(id=" + uid + ",username=" + username + ")")
    }
  }
