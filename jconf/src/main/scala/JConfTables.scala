package cap.jeeves.jconf

import org.squeryl.KeyedEntity
import org.squeryl.adapters.MySQLAdapter
import org.squeryl.PrimitiveTypeMode._
// import org.squeryl.customtypes.CustomTypesMode._
// import org.squeryl.customtypes._
import org.squeryl.Schema
import org.squeryl.Session
import org.squeryl.SessionFactory

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

}

class Assignment(val reviewerId: Int, val paperId: Int);

class PaperItemRecord(
    val id: Int
  , val title: String
  ) { // extends KeyedEntity[Int] {
  def this() = this(-1, "")
}

class PaperReviewRecord(
    val id: Int = -1
  , val reviewer: Int
  , val body: String
  , val score: Int ) extends KeyedEntity[Int] {
}

class PaperTagRecord(
    val paperId: Int
  , val tagId: Int
  , val tagData: Int) {
  def this() = this(-1, -1, -1)
  
} 

object JConfTables extends Schema {
  val users = table[ConfUserRecord]("ConfUsers")
  on(users)(u => declare(
      u.id        is(indexed, unique)
    , u.username  is(unique)
  ))

  val assignments = table[Assignment]
  on(assignments)(a => declare(
      a.reviewerId  is(indexed)
    , a.paperId     is(indexed)
  ))

  var papers: List[PaperRecord] = Nil

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
