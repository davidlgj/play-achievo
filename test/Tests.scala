import _root_.databinder.achievo.Achievo
import secret.Secret
import play._
import play.test._

import org.scalatest._
import org.scalatest.junit._
import org.scalatest.matchers._

class AchievoBasicTests extends UnitFlatSpec with ShouldMatchers {
    val achievo = Achievo(Secret.name, Secret.pw)

    it should "run this dumb test" in {
        (1 + 1) should be (2)
    }

    it should "return a cookie for a known user" in {
        val cookie = achievo.achievoCookie
        cookie.isDefined should be (true)
    }
}

class AchievoRegistrationFormTests extends UnitFlatSpec with ShouldMatchers {
    val achievo = Achievo(Secret.name, Secret.pw)

    it should "find an time registration form" in {
        val forms = achievo.timeRegistrationForm
        forms.size should be (1)
        println("inputs:\n"+forms(0).inputs.mkString("\n"))
        println("")
        println("textares:\n"+forms(0).textareas.mkString("\n"))
        println("")
        println("selects:\n"+forms(0).selects.mkString("\n"))

        val postParams = forms(0).toMap
        println("")
        println("post params:\n"+postParams.mkString("\n"))

        postParams.filter(p => p._1 == "achievo")(0)._2 should be (achievo.achievoCookie.get.getValue)
    }
}

class AchievoRecordFormTests extends UnitFlatSpec with ShouldMatchers {
    val achievo = Achievo(Secret.name, Secret.pw)

    it should "find a time survey form" in {
        val form = achievo.timeSurveyForm
        println("Survey form:")
        println("action: "+form.action)
        println(form.toMap.mkString("\n"))

        form.action should be ("dispatch.php")
        form.toMap.size should be (50)
    }
}


class AchievoRecordTests extends UnitFlatSpec with ShouldMatchers {
    val achievo = Achievo(Secret.name, Secret.pw)

    it should "extract records from rl_1 table" in {
        val reportData = achievo.timeSurveyData
        println(reportData)
        reportData.size should be (5)
        reportData.foreach(_.size should be (16))
    }
}
