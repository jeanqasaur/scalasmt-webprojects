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
case class ConfUser(
            val uid: BigInt
  ,         val email: String
  , private var _name: String
  , private var _password: String
  ,         val role: UserStatus
  , private var _submittedPapers: List[BigInt] = Nil )
  extends JeevesRecord {
    /*************/
    /* Policies. */
    /*************/
    private val isReviewer: Formula =
      CONTEXT.viewer.status === ReviewerStatus
    private val isPC: Formula = CONTEXT.viewer.status === PCStatus

    private val selfL = mkLevel ();
    policy (selfL, !(CONTEXT.viewer~'uid === uid), LOW);
    logConfUserPolicy();
    def isSelf(): Formula = selfL

    /* If you can see this user, you can see their name. */
    private val nameL = mkLevel ();
    // No policies on name--should always be high...
    def setName (n: String): Unit = {
      _name = n;
      name = mkSensitive(nameL, StringVal(_name), StringVal("--"))
    }
    var name: Symbolic =
      mkSensitive(nameL, StringVal(_name), StringVal("--"))
    def showName (ctxt: ConfContext): String = {
      concretize(ctxt, name).asInstanceOf[StringVal].v
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
    def setPassword (p: String) = {
      _password = p
      password = mkSensitive(selfL, StringVal(_password), StringVal("default"))
    }
    var password =
      mkSensitive(selfL, StringVal(_password), StringVal("default"))
    def showPassword (ctxt: ConfContext): String = {
      concretize(ctxt, password).asInstanceOf[StringVal].v
    }
    def emailPassword(): Unit = {
      JConfMail.sendMail(
          "jeanyang@csail.mit.edu"
        , "Jean Yang"
        , email
        , "Welcome to JConf"
        , "Your password is " + _password + "." )
    }


    def getConfUserRecord(): ConfUserRecord = {
      transaction { JConfTables.users.get(uid.toInt) }
    }
    def debugPrint(): Unit = {
      println("ConfUser(id=" + uid + ",email=" + email + ")")
    }
  }
