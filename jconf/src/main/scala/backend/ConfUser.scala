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
  ,         val secretId: String  // Used in the link
  ,         val email: String
  , private var _name: String
  , private var _affiliation: String
  , private var _password: String
  , private var _isGrad: Boolean
  , private var _acmNum: String = ""
  ,         val role: UserStatus
  , private var _conflicts: List[BigInt]
  , private var _submittedPapers: List[BigInt] = Nil )
  extends JeevesRecord {
    /*************/
    /* Policies. */
    /*************/
    private val isSelf: Formula = CONTEXT.viewer~'uid === uid

    private val isReviewer: Formula =
      CONTEXT.viewer.status === ReviewerStatus
    private val isPC: Formula = CONTEXT.viewer.status === PCStatus

    private val selfL = mkLevel ();
    policy (selfL, !isSelf, LOW);
    logConfUserPolicy();
    def showIsSelf(ctxt: ConfContext): Boolean = {
      concretize(ctxt, selfL)
    }

    // No policies on name--should always be high...
    def setName (n: String): Unit = { _name = n; }
    def showName (ctxt: ConfContext): String = {
      _name
    }
    def setAffiliation (a: String): Unit = {
      _affiliation = a;
    }
    def showAffiliation (ctxt: ConfContext): String = {
      _affiliation
    }

    def setIsGrad (isGrad: Boolean): Unit = {
      _isGrad = isGrad
    }
    def showIsGrad(ctxt: ConfContext): Boolean = {
      _isGrad
    }

    private val numL = mkLevel()
    policy (numL, !(isSelf || isPC), LOW)
    def setAcmNum (acmNum: String): Unit = {
      _acmNum = acmNum
    }
    def getAcmNum (): Symbolic = {
      mkSensitive(numL, StringVal(_acmNum), StringVal(""))
    }
    def showAcmNum (ctxt: ConfContext): String = {
      show[StringVal](ctxt, getAcmNum ()).v
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
        , "Your PLDI SRC 2012 Password"
        , "Your password is " + _password + "." )
    }

    def addConflict(c: BigInt): Unit = {
      _conflicts = c::_conflicts
    }
    def setConflicts(cs: List[BigInt]): Unit = {
      // If something is added...
      cs.foreach{ c =>
        if (!_conflicts.exists(_ == c)) {
          JConfTables.writeDBConflict(uid.toInt, c.toInt)
        }
      }
      // If something has been removed...
      _conflicts.foreach{ c =>
        if (!cs.exists(_ == c)) {
          JConfTables.removeDBConflict(uid.toInt, c.toInt)
        }
      }

      _conflicts = cs
    }
    // Conflicts are public right now...
    def getConflicts(): List[BigInt] = {
      _conflicts
      /*
      _conflicts.map( (c: BigInt) => {
        val conflictL = mkLevel ()
        policy ( conflictL
        , !(isPC || isSelf || (CONTEXT.viewer~'uid === c))
        , LOW )
        mkSensitiveInt(conflictL, c, -1) } ) */
    }
    def hasConflict(userId: BigInt): Boolean = {
      (getConflicts ()).exists(_ == userId)
    }
    def showHasConflict(ctxt: ConfContext, userId: BigInt): Boolean = {
//      concretize(ctxt, 
      hasConflict(userId)
    }

    def update(params: Map[String, String]
      , conflicts: List[BigInt])
    : Unit = {
      val name: String = params("name")
      val affiliation: String = params("affiliation")
      val isGrad: Boolean = {
        try { params("isGrad") == "yes" }
        catch { case e: Exception => false }
      }
      val acmNum: String = {
        try { params("acmNum") } catch { case e: Exception => "" }
      }

      setName(name)
      setAffiliation(affiliation)
      setIsGrad(isGrad)
      setAcmNum(acmNum)
      setConflicts(conflicts)

      JConfTables.updateDBUser(this, name, affiliation, isGrad, acmNum)
    }

    def getConfUserRecord(): ConfUserRecord = {
      transaction { JConfTables.users.get(uid.toInt) }
    }
    def debugPrint(): Unit = {
      println("ConfUser(id=" + uid + ",email=" + email + ")")
    }
  }
