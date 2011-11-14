package cap.scalasmt.webapp

class PrivacyTag ()
{
  private var restrictedUsers: List[Int] = Nil
  private var friendsPartialVisibility = Array(false, false, false, false)
  private var publicPartialVisibility = Array(false, false, false, false)
  private var levelOfVisibility = 0

  def addRestrictedPerson(user: User)
  {
      restrictedUsers = user.getTag() :: restrictedUsers;
  }

  def changeLevelOfVisibility(newVisibility: Int): Boolean =
  {
     if (!(newVisibility == 0 || newVisibility == 1 || newVisibility == 2))
       false
     levelOfVisibility = newVisibility
    true
  }

  def changePublicPartialVisibility(newVisibility: Array[Boolean]) =
  {
    publicPartialVisibility = newVisibility
  }

  def changeFriendsPartialVisibility(newVisibility: Array[Boolean])
  {
    friendsPartialVisibility = newVisibility
  }
}