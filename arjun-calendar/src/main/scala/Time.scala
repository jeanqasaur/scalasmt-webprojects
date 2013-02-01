package cap.scalasmt.webapp

/**
 * Created by IntelliJ IDEA.
 * User: Arjun
 * Date: 11/1/11
 * Time: 12:34 AM
 * To change this template use File | Settings | File Templates.
 */

class Time (val hour: Int,  val minutes: Double)
{
   if (!Time.checkTime(hour, minutes))
   {
     throw new Exception ("Your time is invalid.")
   }

  override def toString():String =
  {
     hour + ":" + minutes
  }
}

object Time
{
   private def checkHour(hour: Int): Boolean  =
     {
        hour >= 1 && hour <= 12
     }

   private def checkMinutes(minutes: Double):Boolean =
   {
      minutes >= 0 && minutes < 60
   }

   def checkTime(hour: Int, minutes: Double): Boolean =
   {
      checkHour(hour) && checkMinutes(minutes)
   }
}