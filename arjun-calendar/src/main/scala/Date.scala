 package cap.scalasmt.webapp

/**
 * Created by IntelliJ IDEA.
 * User: Arjun
 * Date: 10/31/11
 * Time: 11:41 PM
 * To change this template use File | Settings | File Templates.
 */

class Date (val month: String, val day: Int, val year: Int)
{
  var events: List[Event]  = Nil

  if (!Date.checkDate(month, day))
  {
     throw new Exception("Your date was invalid. Please make sure the month and day are correct");
  }

  def getIntMonth(): Int =
  {
    Date.months.indexOf(month) + 1
  }

  def  getShortDate(): String =
  {
     this.getIntMonth() + "/" + day + "/" + year
  }

  override def toString(): String =
  {
     month + " " + day + ", " + year;
  }

  def addEvent(e: Event)
  {
     events = e :: events
  }

  def removeEvent(e: Event)
  {
    if (events.indexOf(e) < 0)
      false

    //Question about removing elements from lists
    true
  }

  def getEvents(): List[Event] =
  {
       events
  }
}

object Date
{
  val months = Array("january", "february", "march", "april", "may", "june", "july", "august",
	  "september", "october", "november", "december")

  val months31 = Array("january", "march", "may", "july", "august", "october", "december")

  val months30 = Array("april", "june", "september", "november")

  private def checkMonth(month: String): Boolean =
  {
    if (months.contains(month.toLowerCase))
    {
      true
    }
    false
  }

  private def checkDay(month: String, day: Int): Boolean =
  {
    val lowerBound = 1
    var upperBound = 0

    if (months31.contains(month.toLowerCase))
    {
       upperBound = 31
    }

    else if (months30.contains(month.toLowerCase))
    {
      upperBound = 30
    }

    else
    {
       upperBound = 28;
    }

    if (day >= lowerBound && day <= upperBound)
      true
    else
      false
  }

  def checkDate(month: String, day: Int): Boolean =
  {
    if (checkMonth(month) && checkDay(month, day))
    {
       true
    }

    false
  }
}



