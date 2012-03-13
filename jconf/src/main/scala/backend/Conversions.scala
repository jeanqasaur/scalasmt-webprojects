package cap.jeeves.jconf.backend

//  TODO: Paramaterize by jcbackend.
class Conversions(b: JConfBackend) {
  class MalformedTagError extends Exception

  // Roles.
  def role2Field(status: UserStatus): Int = {
    status match {
      case PublicStatus(_b)   => 1
      case AuthorStatus(_b)   => 2
      case ReviewerStatus(_b) => 3
      case PCStatus(_b)       => 4
    }
  }
  def field2Role(status: Int): UserStatus = {
    status match {
      case 1 => b.publicStatus
      case 2 => b.authorStatus
      case 3 => b.reviewerStatus
      case 4 => b.pcStatus
    }
  }

  def tag2Field(tag: PaperTag): (Int, Int) = {
    tag match {
      case NeedsReview(_b, reviewer)  => (1, reviewer.toInt)
      case ReviewedBy(_b, reviewer)   => (2, reviewer.toInt)
      case Accepted(_b)               => (3, -1)
      case EmptyTag(_b)               => (4, -1)
    }
  }
  def field2Tag(tag: (Int, Int)): PaperTag = {
    tag match {
      case (1, id) => NeedsReview(b, id)
      case (2, id) => ReviewedBy(b, id)
      case (3, _) => Accepted(b)
      case (4, _) => EmptyTag(b)
      case (_, _) => throw new MalformedTagError
    }
  }
}
