package models

import java.util.Date
import java.util.Calendar


case class Activity(project:String,phase:String, time:Float, comment:String)

case class Day(activities:List[Activity],date:Date)

object DataMocker {

   def getTime(n:Int):Date =  {
      val cal = Calendar.getInstance()
      cal.add(Calendar.DAY_OF_MONTH,-n)
      cal.getTime
   }
   
   def days(nr:Int):List[Day] = nr match {
      case -1 => Nil
      case n =>  Day(
                       List( Activity("JS Absence","Public Holiday",8f,"foobar") ),
                       DataMocker.getTime(n)
                    ) :: days(n-1)
   }

}
