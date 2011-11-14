package cap.scalasmt.webapp

/**
 * Created by IntelliJ IDEA.
 * User: Arjun
 * Date: 11/1/11
 * Time: 12:34 AM
 * To change this template use File | Settings | File Templates.
 */

class Event (title: String, date: Date,  startTime: Time, endTime: Time, location: String, val privacyTag: PrivacyTag)
{
  def this(title2:String,  date2:Date, startTime2: Time, endTime2: Time, loc2: Address, privacyTag: PrivacyTag)
  {
    this(title2, date2, startTime2, endTime2, loc2.toString(), privacyTag)
  }

  private var leisureOrWork: Int = -1

  override def toString(): String =
  {
      "Title: " + title + ", Date: " + date.toString + "\n" +
      "Start Time: " + startTime.toString() + "End Time: " + endTime.toString() + "\n" +
      "Location: " + location
  }

  def changeLeisureOrWork(newStatus: Int): Boolean =
  {
     if (!(newStatus == -1 || newStatus == 0 || newStatus == 1))
        false
    leisureOrWork = newStatus
    true
  }
}