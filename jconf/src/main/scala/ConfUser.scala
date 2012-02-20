package cap.jeeves.jconf

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._

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
    val id: Int = -1
  , val username: Username = Username("-")
  , private var _name: Name = new Name("")
  , private var _password: String = ""
  , val role: UserStatus = PublicStatus
  , private var _submittedPapers: List[PaperRecord] = Nil
  , private var _reviewPapers: List[PaperRecord] = Nil
  , private var _reviews: List[PaperReview] = Nil )
  extends JeevesRecord {
    /*************/
    /* Policies. */
    /*************/
    private val isReviewer: Formula =
      CONTEXT.viewer.status === ReviewerStatus
    private val isPC: Formula = CONTEXT.viewer.status === PCStatus

    private val selfL = mkLevel ();
    policy (selfL, !(CONTEXT.viewer.username === username), LOW);

    // For now, everyone can see the name.
    private val nameL = mkLevel ();
    policy (nameL, false, LOW);

    def setName (n: Name): Unit = {
      _name = n
    }
    def getName (): Symbolic = mkSensitive(nameL, _name, Name("--"))
    def showName (ctxt: ConfContext): String = {
      concretize(ctxt, getName ()).asInstanceOf[Name].name
    }

    // Submitted papers.
    def addSubmittedPaper (p: PaperRecord): Unit =
      _submittedPapers = p::_submittedPapers
    def getSubmittedPapers (): List[Symbolic] =
      _submittedPapers.map(p => mkSensitive(selfL, p, new PaperRecord()))
    def showSubmittedPapers (ctxt: ConfContext): List[PaperRecord] = {
      (getSubmittedPapers ()).map(
        p => concretize(ctxt, p).asInstanceOf[PaperRecord])
    }

    // Papers to review.
    def addReviewPaper (r: PaperRecord): Unit = _reviewPapers = r::_reviewPapers
    def getReviewPapers (): List[Symbolic] =
      _reviewPapers.map(p => mkSensitive(selfL, p, new PaperRecord()))
    def showReviewPapers (ctxt: ConfContext): List[PaperRecord] = {
      (getReviewPapers ()).map(p => concretize(ctxt, p).asInstanceOf[PaperRecord])
    }

    // Reviews submitted.
    def addReview (r: PaperReview): Unit =_reviews = r::_reviews
    def getReviews (): List[Symbolic] =
      _reviews.map(r => mkSensitive(selfL, r, new PaperReview()))
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
  }
