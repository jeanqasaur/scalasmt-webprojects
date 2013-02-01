package cap.scalasmt.webapp

/**
 * Created by IntelliJ IDEA.
 * User: Arjun
 * Date: 11/1/11
 * Time: 10:09 PM
 * To change this template use File | Settings | File Templates.
 */

class SportsInfo
{
  private val teamsPanel: InfoPanel = new InfoPanel()
  private val athletesPanel: InfoPanel = new InfoPanel()

  def getTeamsPanel(): InfoPanel  =
  {
     teamsPanel
  }

  def getAthletesPanel(): InfoPanel  =
  {
     athletesPanel
  }

}