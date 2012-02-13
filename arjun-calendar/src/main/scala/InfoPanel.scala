package cap.scalasmt.webapp

/**
 * Created by IntelliJ IDEA.
 * User: Arjun
 * Date: 11/1/11
 * Time: 9:47 PM
 * To change this template use File | Settings | File Templates.
 */

class InfoPanel ()
{
  private var tiles: List[Tile] = Nil

  def addTile(t: Tile)
  {
    tiles = t :: tiles
  }

  def removeTile(t: Tile)
  {
    if (tiles.indexOf(t) < 0)
      false

    //Question about removing elements from lists
    true
  }

  def getTiles(): List[Tile] =
  {
    tiles
  }
}