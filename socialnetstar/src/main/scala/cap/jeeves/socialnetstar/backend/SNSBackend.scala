package cap.jeeves.socialnetstar.backend

import scala.collection.mutable.HashMap;
import scala.collection.mutable.Set;

import cap.scalasmt._
import cap.jeeves._

import Expr._

/**
 * External interface to social network.
 * @author kuat
 */
object SNSBackend extends JeevesLib {
  private var users: List[UserRecord] = Nil;

  /* Database functions. */
  def addUser(u: UserRecord) {
    users = u :: users
  }
  
  def addFriendship(record1: UserRecord, record2: UserRecord) {
    record1.addFriend(record2);
    record2.addFriend(record1);
  }

  def removeFriendship(record1: UserRecord, record2: UserRecord) {
    record1.removeFriend(record2);
    record2.removeFriend(record1);
  }

  def getFriendNetworks(user: UserRecord) =
    user.getFriends().map(_.network)

  def getUsersByCircle(network : SocialCircle) = 
    users.filter(_.network === network)

  def announceName(u: UserRecord) = 
    for (f <- u.getFriends())
      yield email(f, u.name)

  def email(f: Sensitive, b: Sensitive) = 
    Receipt(concretize(f, f.email), concretize(f, b))

  case class Receipt(email: Atom, body: Atom)

  // send email to multiple people at the same time
} 
