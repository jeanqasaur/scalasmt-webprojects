package cap.scalasmt.webapp

/**
 * Created by IntelliJ IDEA.
 * User: Arjun
 * Date: 11/1/11
 * Time: 10:12 PM
 * To change this template use File | Settings | File Templates.
 */

class EntertainmentInfo()
{
  private val musicPanel: InfoPanel = new InfoPanel()
  private val booksPanel: InfoPanel = new InfoPanel()
  private val moviesPanel: InfoPanel = new InfoPanel()
  private val televisionPanel: InfoPanel = new InfoPanel()
  private val gamesPanel: InfoPanel = new InfoPanel()

  def getMusicPanel(): InfoPanel  =
  {
     musicPanel
  }

  def getBooksPanel(): InfoPanel  =
  {
     booksPanel
  }

  def getMoviesPanel(): InfoPanel  =
  {
     moviesPanel
  }

  def getTelevisionPanel(): InfoPanel =
  {
    televisionPanel
  }

  def getGamesPanel(): InfoPanel =
  {
    gamesPanel
  }




}