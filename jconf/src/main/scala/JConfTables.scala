package cap.jeeves.jconf

import cap.scalasmt._

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
    val id: Int
  , val username: String
  , val name: String
  , val pwd: String
  , val role: Int
  ) extends KeyedEntity[Int] {
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
    new ConfUser(
      id, Username(username), Name(name), pwd, Conversions.field2Role(role)
    , getSubmittedPapers() )
  }
}

class Assignment(val reviewerId: Int, val paperId: Int);

class PaperItemRecord( val id: Int, val title: String)
  extends KeyedEntity[Int] {
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
  def getPaperRecord() = {
    new PaperRecord(id, Title(title), getAuthors())
    // Persistence.deserialize[PaperRecord](item)
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
    val id: Int = -1
  , val reviewer: Int
  , val body: String
  , val score: Int ) extends KeyedEntity[Int] {
}

class PaperTagRecord(val paperId: Int, val tagId: Int, val tagData: Int)

object JConfTables extends Schema {
  /* Users. */
  val users = table[ConfUserRecord]("ConfUsers")
  on(users)(u => declare(
      u.id        is(indexed, unique)
    , u.username  is(unique)
  ))
  def writeDB(user: ConfUser): Unit = {
    transaction {
      users.insert(user.getConfUserRecord())
    }
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
      a.id      is(indexed, unique)
  ))
  def writeDB(paper: PaperRecord): Unit = {
    transaction {
      papers.insert(paper.getPaperItemRecord())
    }
  }
  def getDBPaperRecord(uid: Int): Option[PaperRecord] = {
    transaction { papers.lookup(uid) } match {
      case Some(paperRecord) => Some(paperRecord.getPaperRecord())
      case None => None
    }
  }
  def getAllDBPapers(): List[PaperRecord] = {
    val paperRecords =
      transaction { from(JConfTables.papers)(p => select(p)).toList }
    paperRecords.map(r => r.getPaperRecord())
  }

  val authors = table[PaperAuthorRecord]
  on(authors)(a => declare(
      a.paperId   is(indexed)
    , a.authorId  is(indexed)
  ))
  def writeDBAuthor(paperId: Int, authorId: Int): Unit = {
    transaction{ authors.insert(new PaperAuthorRecord(paperId, authorId)) } 
  }

  // Tables having to do with reviews.
  val reviews = table[PaperReviewRecord]
  on(reviews)(r => declare(
      r.id      is(indexed, unique)
  ))

  val tags = table[PaperTagRecord]
  on(tags)(t => declare(
      t.paperId is(indexed)
    , t.tagId   is(indexed)
  ))
}
