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
    val username: String
  , val name: String
  , val pwd: String
  , val role: Int
  ) extends KeyedEntity[Int] {
  def this() = this("", "", "", -1)
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
          new ConfUser(
              id, Username(username), Name(name), pwd
            , Conversions.field2Role(role), getSubmittedPapers() );
        cacheUser(u);
        u
    }
  }
}

class Assignment(val reviewerId: Int, val paperId: Int);

class PaperItemRecord( val title: String) extends KeyedEntity[Int] {
  def this() = this("")
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
        val p = new PaperRecord(id, Title(title), getAuthors (), getTags ())
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
    val paperId: Int
  , val reviewerId: Int
  , val body: String
  , val score: Int ) extends KeyedEntity[Int] {
  def this() = this(-1, -1, "", -1)
  val id: Int = 0;

  def getPaperReview(): PaperReview = {
    new PaperReview(id, paperId, reviewerId, body, score)
  }
}

class PaperTagRecord(val paperId: Int, val tagId: Int, val tagData: Int)

object JConfTables extends Schema {
  /* Users. */
  val users = table[ConfUserRecord]("ConfUsers")
  on(users)(u => declare(
      u.id        is(autoIncremented)
    , u.username  is(unique)
  ))
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
    transaction{ authors.insert(new PaperAuthorRecord(paperId, authorId)) } 
  }

  /* Reviews. */
  val reviews = table[PaperReviewRecord]
  on(reviews)(r => declare(
      r.id      is(autoIncremented)
  ))

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
