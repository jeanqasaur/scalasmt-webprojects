package cap.jeeves.socialnetstar.backend

/*
 * User records for jeeves social net case study.
 * @author jeanyang, kuat
 */

import cap.scalasmt._
import cap.jeeves.JeevesLib._

import cap.jeeves.socialnetstar.backend.SocialNetBackend._
import cap.scalasmt.Expr._
                                                                                                                                                                                            
case class Name(s: String) extends JeevesRecord
case class Email(s: String) extends JeevesRecord
case class SocialCircle(s: String) extends JeevesRecord

sealed trait PrivacyLevel 
object Anyone extends PrivacyLevel
object Self extends PrivacyLevel
object Friends extends PrivacyLevel

class UserRecord(
    /*
     * <attribute>V: actual value
     * <attribute>L: privacy level */
    
  nameV: Name, 
  nameL: PrivacyLevel, 
  emailV: Email, 
  emailL: PrivacyLevel, 
  circleV: SocialCircle, 
  circleL: PrivacyLevel, 
  friendsL: PrivacyLevel) 
extends JeevesRecord {
  private var friends: Set[UserRecord] = Set()
  var X: IntExpr = 1000
  var Y: IntExpr = 1000

  /** Mutators */
  def add(u: UserRecord) {friends = friends + u}
  def remove(u: UserRecord) {friends = friends - u}
  def setLocation(x: BigInt, y: BigInt)(implicit ctxt:Sensitive){
    val l = mkLevel();
    restrict(l, distance(ctxt, this) >= 10);
    UserRecord.this.X = mkSensitiveInt(l, x, 1000);
    UserRecord.this.Y = mkSensitiveInt(l, y, 1000);
  }

 /** Observers */
  val name = mkSensitive(level(nameL), nameV)
  val email = mkSensitive(level(emailL), emailV)
  val network = mkSensitive(level(networkL), networkV);
  def getFriends() = {
    val l = level(friendsL);
    friends.map(mkSensitive(l, _))
  }
  def isFriends(u: UserRecord) = getFriends.has(u)
  def location() = (X, Y);
  
  
/** Helpers */
  private def level(ul: PrivacyLevel) (implicit ctxt:Sensitive)= {
    val l = mkLevel();
    val me = ctxt.viewer === this;
    ul match {
      case Anyone => 
      case Self => restrict(l, ! me)
      case Friends => restrict(l, ! (me || friends.has(ctxt.viewer)));
    }
    l
  }

  private def distance(a: Sensitive, b: Sensitive) = 
    abs(a.X - b.X) + abs(a.Y - b.Y)
    
}


 

  
