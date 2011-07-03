import _root_.databinder.achievo.Achievo
import secret.Secret
import play._
import play.test._

import org.scalatest._
import org.scalatest.junit._
import org.scalatest.matchers._

trait AchievoSetup extends FlatSpec with BeforeAndAfterEach {
    var achievo: Achievo = _

    override def beforeEach {
        achievo = Achievo(Secret.name, Secret.pw)
        println(achievo.achievoCookie.get.getValue)
        super.beforeEach
    }

    override def afterEach {
        achievo.logout
        super.afterEach
    }
}

class AchievoBasic extends UnitFlatSpec with ShouldMatchers with AchievoSetup {

    it should "run this dumb test" in {
        (1 + 1) should be(2)
    }

    it should "be able to access logged in user" in {
        achievo.user should be(Secret.name)

    }

    it should "return a cookie for a known user" in {
        achievo.achievoCookie.isDefined should be(true)
    }

    it should "return a cookie with length 32" in {
        achievo.achievoCookie.get.getValue.size should be(32)
    }
}

class AchievoLoginFailed extends UnitFlatSpec with ShouldMatchers {
    it should "not set cookie if login fail" in {
        val achievo = Achievo("falseUser", "falsePassword")

        achievo.achievoCookie.isDefined should be(false)
    }
}

class AchievoLogout extends UnitFlatSpec with ShouldMatchers {
    it should "return a new cookie after logout" in {
        var achievo = Achievo(Secret.name, Secret.pw)
        val cookie1 = achievo.achievoCookie.get.getValue
        println(cookie1)

        achievo.logout

        achievo = Achievo(Secret.name, Secret.pw)
        val cookie2 = achievo.achievoCookie.get.getValue
        println(cookie2)

        (cookie1 == cookie2) should be (false)

        achievo.logout
    }
}

class AchievoRegistrationFormTests extends UnitFlatSpec with ShouldMatchers with AchievoSetup {
    it should "find an time registration form" in {
        val forms = achievo.timeRegistrationForm
        forms.size should be(1)
        println("inputs:\n" + forms(0).inputs.mkString("\n"))
        println("")
        println("textares:\n" + forms(0).textareas.mkString("\n"))
        println("")
        println("selects:\n" + forms(0).selects.mkString("\n"))

        val postParams = forms(0).toMap
        println("")
        println("post params:\n" + postParams.mkString("\n"))

        postParams.filter(p => p._1 == "achievo")(0)._2 should be(achievo.achievoCookie.get.getValue)
    }
}

class AchievoRecordFormTests extends UnitFlatSpec with ShouldMatchers with AchievoSetup {
    it should "find a time survey form" in {
        val form = achievo.timeSurveyForm
        println("Survey form:")
        println("action: " + form.action)
        println(form.toMap.mkString("\n"))

        form.action should be("dispatch.php")
        form.toMap.size should be(50)
    }
}


class AchievoRecordTests extends UnitFlatSpec with ShouldMatchers with AchievoSetup {
    it should "extract records from rl_1 table" in {
        val reportData = achievo.timeSurveyData
        println(reportData)
        reportData.size should be(5)
        reportData.foreach(_.size should be(16))
    }
}
