import _root_.databinder.achievo.Achievo
import secret.Secret
import play._
import play.test._

import org.scalatest._
import org.scalatest.junit._
import org.scalatest.matchers._

class BasicTests extends UnitFlatSpec with ShouldMatchers {
    
    it should "run this dumb test" in {
        (1 + 1) should be (2)
    }

    it should "return a cookie for a known user" in {
        val a = Achievo(Secret.name, Secret.pw)

        val cookie = a.achievoCookie
        cookie.isDefined should be (true)
    }

    it should "find an empty time registration form" in {
        val a = Achievo(Secret.name, Secret.pw)

        val forms = a.timeRegistrationForm
        forms.size should be (1)
        println("inputs:\n"+forms(0).inputs.mkString("\n"))
        println("")
        println("textares:\n"+forms(0).textareas.mkString("\n"))
        println("")
        println("selects:\n"+forms(0).selects.mkString("\n"))

        val postParams = forms(0).toMap
        println("")
        println("post params:\n"+postParams.mkString("\n"))

        postParams.filter(p => p._1 == "achievo")(0)._2 should be (a.achievoCookie.get.getValue)
    }

    it should "find a time survey form" in {
        val a = Achievo(Secret.name, Secret.pw)

        val forms = a.timeSurveyForm
        println("Survey form:\n"+forms(0).toMap.mkString("\n"))

        forms.size should be (3)


    }

}