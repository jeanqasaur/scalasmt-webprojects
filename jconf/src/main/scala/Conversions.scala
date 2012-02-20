package cap.jeeves.jconf

import cap.scalasmt._

object Conversions {
  class MalformedTagError extends Exception

  // Roles.
  def role2Field(status: UserStatus): Int = {
    status match {
      case PublicStatus   => 1
      case AuthorStatus   => 2
      case ReviewerStatus => 3
      case PCStatus       => 4
    }
  }
  def field2Role(status: Int): UserStatus = {
    status match {
      case 1 => PublicStatus
      case 2 => AuthorStatus
      case 3 => ReviewerStatus
      case 4 => PCStatus
    }
  }

  def tag2Field(tag: PaperTag): (Int, Int) = {
    tag match {
      case NeedsReview(reviewer)  => (1, reviewer.id)
      case ReviewedBy(reviewer)   => (2, reviewer.id)
      case Accepted               => (3, -1)
      case EmptyTag               => (4, -1)
    }
  }
  def field2Tag(tag: (Int, Int)): PaperTag = {
    tag match {
      case (1, id) =>
        JConfBackend.getUserById(id) match {
          case Some(u) => NeedsReview(u)
          case None =>
            println(id)
            throw new JConfBackend.NoSuchUserError
        }
      case (2, id) =>
        JConfBackend.getUserById(id) match {
          case Some(u) => ReviewedBy(u)
          case None =>
            println(id)
            throw new JConfBackend.NoSuchUserError
        }
      case (3, _) => Accepted
      case (4, _) => EmptyTag
      case (_, _) => throw new MalformedTagError
    }
  }
}
