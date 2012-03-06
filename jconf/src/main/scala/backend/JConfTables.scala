package cap.jeeves.jconf.backend

import org.squeryl.KeyedEntity
import org.squeryl.adapters.MySQLAdapter
import org.squeryl.PrimitiveTypeMode._
// import org.squeryl.customtypes.CustomTypesMode._
// import org.squeryl.customtypes._
import org.squeryl.Schema
import org.squeryl.Session
import org.squeryl.SessionFactory

import JConfBackend._

object UserStatusField extends Enumeration {
  type UserStatusField = Value
  val PublicStatus = Value(1, "PublicStatus")
  val AuthorStatus = Value(2, "AuthorStatus")
  val ReviewerStatus = Value(3, "ReviewerStatus")
  val PCStatus = Value(4, "PCStatus")
}

class ConfUserRecord(
    val secretId: String
  , val email: String
  , var name: String
  , var pwd: String
  , var isGrad: Boolean
  , var acmNum: Int
  , var role: Int
  ) extends KeyedEntity[Int] {
  def this() = this("", "", "", "", false, -1, -1)
  val id = 0;

  def getSubmittedPapers(): List[BigInt] = {
    transaction {
      val submittedPapers: Iterable[Int] =
        from(JConfTables.authors)(a =>
          where(a.authorId.~ === id)
          select(a.paperId))
      submittedPapers.toList.map(pid => BigInt(pid))
    }
  }

  def getConfUser() = {
    lookupCachedUser(id) match {
      case Some(u) => u
      case None =>
        val u =
          new ConfUser(id, secretId, email, name, pwd, isGrad, acmNum
            , Conversions.field2Role(role), getSubmittedPapers() );
        cacheUser(u);
        u
    }
  }
}

class Assignment(val reviewerId: Int, val paperId: Int);

class PaperItemRecord(
  val secretId: String, var title: String, var file: String )
extends KeyedEntity[Int] {
  def this() = this("", "", "")
  val id: Int = 0

  private def getAuthors(): List[ConfUser] = {
   transaction {
      val authors: Iterable[PaperAuthorRecord] = from(JConfTables.authors)(a =>
        where(a.paperId === id.~)
        select(a)
      )
      authors.toList.map(a =>
        getUserById(a.authorId) match {
          case Some(author) => author
          case None =>
            println(a.authorId)
            throw new NoSuchUserError(a.authorId)
        })
    }
  }
  def getTags(): List[PaperTag] = {
    transaction {
      val tags: Iterable[PaperTagRecord] = from(JConfTables.tags)(t =>
        where(t.paperId === id.~)
        select(t)
      );
      tags.toList.map(t => Conversions.field2Tag(t.tagId, t.tagData))
    }
  }
  def getPaperRecord() = {
    lookupCachedPaper(id) match {
      case Some(p) => p
      case None =>
        val p =
          new PaperRecord(id, secretId, title, getAuthors (), file, getTags ())
        cachePaper(p);
        p
    }
  }
}

class PaperAuthorRecord(
    val paperId: Int
  , val authorId: Int ) {
  def debugPrint():Unit = {
    println(
      "PaperAuthorRecord(paperId=" + paperId + ",authorId=" + authorId + ")")
  }
}

class PaperReviewRecord(
    val secretId: String
  , val paperId: Int
  , val reviewerId: Int
  , var body: String
  , var problemScore: Int
  , var backgroundScore: Int
  , var approachScore: Int
  , var resultScore: Int )
extends KeyedEntity[Int] {
  def this() = this("", -1, -1, "", 3, 3, 3, 3)
  val id: Int = 0;

  def getPaperReview(): PaperReview = {
    new PaperReview(id, secretId, paperId, reviewerId, body
      , problemScore, backgroundScore, approachScore, resultScore )
  }
}

class PaperTagRecord(val paperId: Int, val tagId: Int, val tagData: Int)

object JConfTables extends Schema {
  /* Users. */
  val users = table[ConfUserRecord]("ConfUsers")
  on(users)(u => declare(
      u.id        is(autoIncremented)
    , u.email     is(unique)
  ))
  def writeDBUser(userRecord: ConfUserRecord): Unit = {
    transaction { users.insert(userRecord) }
  }
  def getDBConfUser(uid: Int): Option[ConfUser] = {
    try {
      val userRecord: Option[ConfUserRecord] =
        transaction { JConfTables.users.lookup(uid) }
      userRecord match {
        case Some(u) => Some(u.getConfUser())
        case None => None
      }
    } catch {
      case e: Exception =>
      println(e);
      None
    }
  }
  def getDBConfUserByEmail(email: String): Option[ConfUser] = {
    try {
      transaction {
        val userRecord = from(JConfTables.users)(u =>
        where(u.email like email)
        select(u)).single;
        Some(userRecord.getConfUser())
      }
    } catch {
      case e: Exception => None
    }
  }
  def updateDBUser(user: ConfUser, name: String, isGrad: Boolean, acmNum: Int)
    : Unit = {
    val userRecord: ConfUserRecord = user.getConfUserRecord();
    userRecord.name = name
    userRecord.isGrad = isGrad
    userRecord.acmNum = acmNum
    transaction { users.update(userRecord) }
  }

  /* Review assignments. */
  val assignments = table[Assignment]
  on(assignments)(a => declare(
      a.reviewerId  is(indexed)
    , a.paperId     is(indexed)
  ))
  def writeAssignment(reviewerId: Int, paperId: Int): Unit = {
    transaction {
      JConfTables.assignments.insert(new Assignment(reviewerId, paperId))
    }
  }
  def isAssigned(reviewerId: Int, paperId: Int): Boolean = {
    val c: Long =
      transaction {
        from(JConfTables.assignments)(a =>
        where((a.paperId === paperId.~)
          and (a.reviewerId === reviewerId.~))
        compute(count) )
      }
    c > 0
  }

  /* Papers. */
  var papers = table[PaperItemRecord]
  on(papers)(a => declare(
      a.id      is(autoIncremented)
  ))
  def writeDBPaper(paperRecord: PaperItemRecord) = {
    transaction { papers.insert(paperRecord) }
  }
  def updateDBPaper(paper: PaperRecord, title: String, file: String) = {
    val paperRecord: PaperItemRecord = paper.getPaperItemRecord()
    paperRecord.title = title
    paperRecord.file = file
    transaction { papers.update(paperRecord) }
  }
  def getDBPaperRecord(uid: Int): Option[PaperRecord] = {
    transaction { papers.lookup(uid) } match {
      case Some(paperRecord) => Some(paperRecord.getPaperRecord())
      case None => None
    }
  }
  def getAllDBPapers(): List[PaperRecord] = {
    transaction {
      val paperRecords =
        from(papers)(p => select(p)).toList
      paperRecords.map(r => r.getPaperRecord())
    }
  }
  def getPapersByReviewer(userId: Int): List[PaperRecord] = {
    transaction {
      val paperRecords: Iterable[PaperItemRecord] =
        from(papers, assignments)( (p, a) =>
          where ((userId.~ === a.reviewerId) and (p.id === a.paperId))
          select(p) )
      paperRecords.toList.map(p => p.getPaperRecord())
    }
  }

  /* Authors. */
  val authors = table[PaperAuthorRecord]
  on(authors)(a => declare(
      a.paperId   is(indexed)
    , a.authorId  is(indexed)
  ))
  def writeDBAuthor(paperId: Int, authorId: Int): Unit = {
    println("writeDBAuthor: " + paperId + ", " + authorId);
    transaction{ authors.insert(new PaperAuthorRecord(paperId, authorId)) } 
  }

  /* Reviews. */
  val reviews = table[PaperReviewRecord]
  on(reviews)(r => declare(
      r.id          is(autoIncremented)
    , r.paperId     is(indexed)
    , r.reviewerId  is(indexed)
    , r.body        is(dbType("varchar(3000)"))
  ))
  def writeDBReview(reviewRecord: PaperReviewRecord) = {
    transaction { reviews.insert(reviewRecord) }
  }
  def updateDBReview(review: PaperReview
    , problemScore: Int, backgroundScore: Int
    , approachScore: Int, resultScore: Int
    , body: String) = {
    val reviewRecord = review.getPaperReviewRecord();
    reviewRecord.problemScore = problemScore
    reviewRecord.backgroundScore = backgroundScore
    reviewRecord.approachScore = approachScore
    reviewRecord.resultScore = resultScore
    reviewRecord.body = body
    transaction { reviews.update(reviewRecord) }
  }

  def getDBPaperReview(uid: Int): Option[PaperReview] = {
    try {
      val reviewRecord: Option[PaperReviewRecord] =
        transaction { JConfTables.reviews.lookup(uid) }
      reviewRecord match {
        case Some(u) => Some(u.getPaperReview())
        case None => None
      }
    } catch {
      case e: Exception => println(e); None
    }
  }
  def getReviewByPaperReviewer(paperId: Int, reviewerId: Int)
    : Option[PaperReview] = {
    try {
      transaction {
        Some(from(reviews)(r =>
          where((r.paperId === paperId.~) and (r.reviewerId === reviewerId.~))
          select(r)).single.getPaperReview())
      }
    } catch {
      case e: Exception => None
    }
  }
  def getReviewsByPaper(paperId: Int): List[PaperReview] = {
    transaction {
      val rs: Iterable[PaperReviewRecord] =
        from(reviews)(r =>
          where(r.paperId === paperId.~)
          select(r));
      rs.toList.map(r => r.getPaperReview())
    }
  }
  def getReviewsByReviewer(reviewerId: Int): List[PaperReview] = {
    transaction {
      val rs: Iterable[PaperReviewRecord] =
        from(reviews)(r =>
          where(r.reviewerId === reviewerId.~)
          select(r));
      rs.toList.map(r => r.getPaperReview())
    }
  }

  val tags = table[PaperTagRecord]
  on(tags)(t => declare(
      t.paperId is(indexed)
    , t.tagId   is(indexed)
  ))
  def addDBTag(paperId: Int, newtag: PaperTag): Unit = {
    val (tag, tagVal) = Conversions.tag2Field(newtag)
    transaction {
      tags.insert(new PaperTagRecord(paperId, tag, tagVal))
    }
  }
  def removeDBTag(paperId: Int, tag: PaperTag): Unit = {
    val (tagId, tagVal) = Conversions.tag2Field(tag);
    transaction {
      JConfTables.tags.deleteWhere(t =>
        (t.paperId === paperId.~) and (t.tagId === tagId.~)
        and (t.tagData === tagVal.~))
    }
  }
}
