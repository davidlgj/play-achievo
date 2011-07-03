package models

import org.joda.time.{Period, DateTime, LocalDate}

case class Project(id: String, title: String, phases: List[Phase])

case class Phase(id: String, title: String)

case class Activity(project: Project, phase: Phase, time: Period, comment: String)

case class Day(activities: List[Activity], date: LocalDate) {
    def this(activity: Activity, date: LocalDate) = this (List(activity), date)
}

object DataMocker {
    val phase = Phase("apa", "Public Holiday")
    val project = Project("bepa", "JS Absence", List(phase))

    def getTime(n: Int): LocalDate = {
        val dt = new DateTime
        dt.plusDays(n).toLocalDate
    }

    def days(nr: Int): List[Day] = nr match {
        case -1 => Nil
        case n => Day(
            List(Activity(project, phase, Period.hours(7).plusMinutes(30), "foobar")),
            DataMocker.getTime(n)
        ) :: days(n - 1)
    }

}
