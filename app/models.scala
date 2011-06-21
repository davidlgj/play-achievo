package models

import java.util.Date
import java.util.Calendar


case class Activity(project:String,phase:String, time:Float, comment:String)

case class Day(activities:List[Activity],date:Date)

object DataMocker {

   // Mock some activites objects
   def days() = {
      val cal = Calendar.getInstance()
      var days = List[Day]()
      
      for (day <- 1 to 20) {
         days = Day(
                     List( Activity("JS Absence","Public Holiday",8f,"foobar") ),
                     cal.getTime
                   ) :: days
         cal.add(Calendar.DAY_OF_MONTH,-1)
      }
      
      days     
   }

}
