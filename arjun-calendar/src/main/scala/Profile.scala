package cap.scalasmt.webapp

/**
 * Created by IntelliJ IDEA.
 * User: Arjun
 * Date: 11/1/11
 * Time: 8:25 PM
 * To change this template use File | Settings | File Templates.
 */

class Profile (var name: String, var gender: Int, var email: String)
{
  val workInfo = new WorkInfo()
  val sportsInfo = new SportsInfo()
  val entertainmentInfo = new EntertainmentInfo()
  val calendar = new Calendar()
}