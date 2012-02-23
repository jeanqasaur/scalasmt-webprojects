package cap.jeeves.jconf

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
    val uid: BigInt = -1
  , val username: Username = Username("-")
  , private var _name: Name = new Name("")
  , private var _password: String = ""
  , val role: UserStatus = PublicStatus )
  extends JeevesRecord {
    /*************/
    /* Policies. */
    /*************/
    private val isReviewer: Formula =
      CONTEXT.viewer.status === ReviewerStatus
    private val isPC: Formula = CONTEXT.viewer.status === PCStatus

    private val selfL = mkLevel ();
    policy (selfL, !(CONTEXT.viewer.username === username), LOW);
    def isSelf(): Formula = selfL

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
    def getSubmittedPapers (): List[Symbolic] = {
      transaction {
        val submittedPapers: Iterable[PaperItemRecord] =
          from(JConfTables.authors, JConfTables.papers)((a, p) =>
            where(a.authorId.~ === uid.toInt and a.paperId.~ === p.id)
            select(p))
        println("papers --")
        submittedPapers.toList.map(p => println(p.id))
        println("--")
        submittedPapers.toList.map(p =>
          mkSensitive(
              selfL, p.getPaperRecord(), new PaperRecord()))
      }
    }
    def showSubmittedPapers (ctxt: ConfContext): List[PaperRecord] = {
      (getSubmittedPapers ()).map(
        p => concretize(ctxt, p).asInstanceOf[PaperRecord])
    }

    /*
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
    */

    // Password.
    def setPassword (p: String) = _password = p
    def getPassword (): Symbolic =
      mkSensitive(selfL, Password(_password), Password("default"))
    def showPassword (ctxt: ConfContext): Password = {
      concretize(ctxt, getPassword ()).asInstanceOf[Password]
    }

    def getConfUserRecord(): ConfUserRecord = {
      new ConfUserRecord(
          uid.toInt, username.name, _name.name, _password
        , Conversions.role2Field(role))
      // Persistence.serialize(this)
    }
    def debugPrint(): Unit = {
      println("ConfUser(id=" + uid + ",username=" + username + ")")
    }
  }
