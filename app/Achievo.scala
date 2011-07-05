package databinder.achievo

import dispatch._
import java.io.StringReader
import org.apache.http.client.HttpClient
import thread.ThreadSafeHttpClient
import scala.collection.JavaConversions._
import org.apache.http.cookie.Cookie
import xml.{Node, NodeSeq}

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: 22/6-11
 * Time: 07:08 
 */

object Achievo {
    val host = :/("ssl.linpro.no").secure
    val parserFactory = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl

    def apply(user: String, pw: String) = new Achievo(user, pw)
}

class Achievo(val user: String, pw: String) {
    val parser = Achievo.parserFactory.newSAXParser()
    val adapter = new scala.xml.parsing.NoBindingFactoryAdapter
    val h = Http
    login

    private def login {
        val result = h(Login << Map("auth_user" -> user, "auth_pw" -> pw) as_str)
        val source = new org.xml.sax.InputSource(new StringReader(result))

        val nodeSeq = adapter.loadXML(source, parser)
        if ((nodeSeq \\ "div").find(n => (n \ "@id").text == "loginform").isDefined) {
            h.client.asInstanceOf[ThreadSafeHttpClient].getCookieStore.clear()
        }
    }

    def achievoCookie: Option[Cookie] = {
        h.client.asInstanceOf[ThreadSafeHttpClient].getCookieStore.getCookies.find(_.getName == "achievo")
    }

    def timeRegistrationForm = {
        val result = h(TimeRegistrationForm as_str)
        val source = new org.xml.sax.InputSource(new StringReader(result))

        val nodeSeq = adapter.loadXML(source, parser)

        extractForms(nodeSeq)
    }

    def timeSurveyForm = {
        val result = h(TimeSurvey as_str)
        val source = new org.xml.sax.InputSource(new StringReader(result))

        val nodeSeq = adapter.loadXML(source, parser)

        extractForms(nodeSeq)(0)
    }

    def timeSurveyData = {
        val result = h(TimeSurvey as_str)
        val source = new org.xml.sax.InputSource(new StringReader(result))

        val nodeSeq = adapter.loadXML(source, parser)
        val table = (nodeSeq \\ "table").filter(n => (n \ "@class" text) == "recordListContainer")
        val headerTable = (table \\ "table").filter(n => (n \ "@id" text) == "rl_1")
        val headerList = (headerTable \\ "th").map(n => n.text.trim)
        val rowsTr = (table \\ "tr").filter(n => (n \ "@id" text).startsWith("rl_1") )
        val rowsData = rowsTr.map(n => (n \ "td").map(_.text.trim))
        rowsData.map(headerList.zip(_))
    }

    private def extractForms(ns: NodeSeq) = {
        val formNs = ns \\ "form"

        formNs.map(n => Form(n \ "@name" text, n \ "@action" text, n \ "@method" text, n \ "@enctype" text,
            extractInputs(n), extractTextareas(n), extractSelects(n)))
    }

    private def extractInputs(formNs: NodeSeq): Seq[(String, String, String)] = {
        val inputNs = formNs \\ "input"

        inputNs.map(n => (n \ "@name" text, n \ "@value" text, n \ "@type" text))
    }

    private def extractTextareas(formNs: NodeSeq): Seq[(String, String, String)] = {
        val inputNs = formNs \\ "textarea"

        inputNs.map(n => (n \ "@name" text, n.text, "textarea"))
    }

    private def extractSelects(formNs: NodeSeq) = {
        val selectNs = formNs \\ "select"

        selectNs.map(n => (n \ "@name" text, selectedOptionValue(n), "select", extractOptions(n)))
    }

    private def selectedOptionValue(n: Node): String = {
        extractOptions(n).filter(opt => (opt._3 == "selected")) match {
            case o :: _ => o._1
            case _ => ""
        }
    }

    private def extractOptions(n: Node) = {
        val optionNs = n \\ "option"

        optionNs.map(n => (n \ "@value" text, n.text, n \ "@selected" text))
    }

    def logout = {
        h(Logout as_str)
        h.client.asInstanceOf[ThreadSafeHttpClient].getCookieStore.clear()
    }
}

object Login extends Request(Achievo.host / "achievo" / "index.php" >\ "iso-8859-1")

object Logout extends Request(Achievo.host / "achievo" / "index.php" <<? Map("atklogout" -> "-1"))

object TimeRegistrationForm extends Request(Achievo.host / "achievo" / "dispatch.php" <<?
    Map("atknodetype" -> "timereg.hours",
        "atkaction" -> "add",
        "atklevel" -> "1",
        "atkprevlevel" -> "0",
        "atkstackid" -> "4e05be4ac26c5")
)

object TimeSurvey extends Request(Achievo.host / "achievo" / "dispatch.php" <<?
    Map("atknodetype" -> "reports.hoursurvey",
        "atkaction" -> "report",
        "atklevel" -> "-1",
        "atkprevlevel" -> "0")
)

object Projects extends Request(Achievo.host / "achievo" / "dispatch.php" <<?
    Map("atknodetype" -> "project.projectselector",
        "atkaction" -> "select",
        "atkstartat" -> "10",
        "atklevel" -> "1",
        "atkprevlevel" -> "1")
)


case class Form(name: String, action: String, method: String, enctype: String, inputs: Seq[(String, String, String)],
    textareas: Seq[(String, String, String)], selects: Seq[(String, String, String, Seq[(String, String, String)])]) {

    def toMap = {
        val map = inputs.filterNot(p => p._3 == "reset").filterNot(p => p._3 == "submit").map(p => (p._1 -> p._2))
        map ++ textareas.map(p => (p._1 -> p._2))
        map ++ selects.map(p => (p._1 -> p._2))
    }
}
