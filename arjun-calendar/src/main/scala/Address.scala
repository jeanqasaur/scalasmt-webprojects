package cap.scalasmt.webapp

/**
 * Created by IntelliJ IDEA.
 * User: Arjun
 * Date: 11/1/11
 * Time: 6:53 PM
 * To change this template use File | Settings | File Templates.
 */

class Address (val streetNumber: Int, val streetName: String, val city: String, val state: String, val zip: Int)
{
  override def toString(): String =
  {
    streetNumber + " " + streetName + "\n" +
    city + " , " + state + " " + zip
  }
}