package models

import org.joda.time.{Period, DateTime, LocalDate}

case class Activity(project: String, phase: String, time: Period, comment: String)

case class Day(activities: List[Activity], date: LocalDate)

object DataMocker {

   def getTime(n: Int): LocalDate =  {
       val dt = new DateTime
       dt.plusDays(n).toLocalDate
   }
   
   def days(nr:Int):List[Day] = nr match {
      case -1 => Nil
      case n =>  Day(
                       List( Activity("JS Absence", "Public Holiday", Period.hours(7).plusMinutes(30), "foobar") ),
                       DataMocker.getTime(n)
                    ) :: days(n-1)
   }

}
