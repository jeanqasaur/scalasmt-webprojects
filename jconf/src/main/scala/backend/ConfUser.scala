package cap.jeeves.jconf.backend

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._
import org.squeryl.PrimitiveTypeMode._

sealed trait UserStatus extends Atom with Serializable
case class PublicStatus (b: JConfBackend) extends UserStatus {
  b.register(this)
}
case class AuthorStatus (b: JConfBackend) extends UserStatus {
  b.register(this)
}
case class ReviewerStatus (b: JConfBackend) extends UserStatus {
  b.register(this)
}
case class PCStatus (b: JConfBackend) extends UserStatus {
  b.register(this)
}

/* Conference User */
case class ConfUser(
            val b: JConfBackend
  ,         val uid: BigInt
  ,         val secretId: String  // Used in the link
  ,         val email: String
  , private var _name: String
  , private var _affiliation: String
  , private var _password: String
  , private var _isGrad: Boolean
  , private var _acmNum: String = ""
  ,         val role: UserStatus
  , private var _conflicts: List[BigInt] )
  extends Atom {
  b.register(this)

    /*************/
    /* Policies. */
    /*************/
    private val isSelf: Formula = b.CONTEXT.viewer~'uid === uid

    private val isReviewer: Formula =
      b.CONTEXT.viewer.status === b.reviewerStatus
    private val isPC: Formula = b.CONTEXT.viewer.status === b.pcStatus

    private val selfL = b.mkLevel ();
    b.policy (selfL, !isSelf, b.LOW);
    b.logConfUserPolicy();
    def showIsSelf(ctxt: ConfContext): Boolean = {
      b.concretize(ctxt, selfL)
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

    private val numL = b.mkLevel()
    b.policy (numL, !(isSelf || isPC), b.LOW)
    var acmNum = b.mkSensitive(numL, StringVal(b, _acmNum), StringVal(b, ""))
    def setAcmNum (newNum: String): Unit = {
      _acmNum = newNum
      acmNum = b.mkSensitive(numL, StringVal(b, _acmNum), StringVal(b, ""))
    }
    def showAcmNum (ctxt: ConfContext): String = {
      (b.concretize(ctxt, acmNum).asInstanceOf[StringVal]).v
    }

    // Submitted papers.
    var _submittedPapers = JConfTables.getDBSubmittedPapers(uid.toInt)
    var submittedPapers =
      _submittedPapers.map(p => b.mkSensitiveInt(selfL, p, -1))
    def showSubmittedPapers (ctxt: ConfContext): List[PaperRecord] = {
      val paperIds: List[BigInt] =
        submittedPapers.map(p => b.concretize(ctxt, p));
        paperIds.map(pid => b.getPaperById(pid.toInt) match {
            case Some(paper) => paper
            case None => b.defaultPaper
          })
    }
    def addSubmittedPaper (p: PaperRecord): Unit = {
      _submittedPapers = p.uid :: _submittedPapers
      submittedPapers = (b.mkSensitiveInt(selfL, p.uid, -1)) :: submittedPapers
    }
    // TODO: Withdraw submitted paper
    def withdrawSubmittedPaper (p: PaperRecord): Unit = {
      // Remove from database.
      JConfTables.removeDBPaper(p.getPaperItemRecord());
      
      // Remove from list and create a new sensitive list. 
      _submittedPapers = _submittedPapers.filterNot(_ == p.uid)
      submittedPapers =
        _submittedPapers.map(p => b.mkSensitiveInt(selfL, p, -1))
    }

    // Papers to review.
    def getReviewPapers (): List[b.Symbolic] = {
      val papers: List[PaperRecord] =
        JConfTables.getPapersByReviewer(b, uid.toInt);
      papers.map(p => b.mkSensitive(selfL, p, b.defaultPaper))
    }
    def showReviewPapers (ctxt: ConfContext): List[PaperRecord] = {
      (getReviewPapers ()).map(p =>
        b.concretize(ctxt, p).asInstanceOf[PaperRecord])
    }

    // Reviews submitted.
    def getReviews (): List[b.Symbolic] = {
      val reviews: List[PaperReview] =
        JConfTables.getReviewsByReviewer(b, uid.toInt);
      reviews.map(r => b.mkSensitive(selfL, r, b.defaultReview))
    }
    def showReviews (ctxt: ConfContext): List[PaperReview] = {
      (getReviews ()).map(r =>
        b.concretize(ctxt, r).asInstanceOf[PaperReview])
    }

    // Password.
    def setPassword (p: String) = {
      _password = p
      password =
        b.mkSensitive(selfL, StringVal(b, _password), StringVal(b, "default"))
    }
    var password =
      b.mkSensitive(selfL, StringVal(b, _password), StringVal(b, "default"))
    def showPassword (ctxt: ConfContext): String = {
      b.concretize(ctxt, password).asInstanceOf[StringVal].v
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
        val conflictL = b.mkLevel ()
        b.policy ( conflictL
        , !(isPC || isSelf || (b.CONTEXT.viewer~'uid === c))
        , b.LOW )
        b.mkSensitiveInt(conflictL, c, -1) } ) */
    }
    def hasConflict(userId: BigInt): Boolean = {
      (getConflicts ()).exists(_ == userId)
    }
    def showHasConflict(ctxt: ConfContext, userId: BigInt): Boolean = {
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
      var acmNum: String = {
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
