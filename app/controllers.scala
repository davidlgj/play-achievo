package controllers

import _root_.databinder.achievo._
import _root_.secret.Secret
import play._
import play.mvc._

import models._

object Application extends Controller {
    
    import views.Application._
    
    def index = {
        html.index(DataMocker.days(20))
    }

    def achievo = {
        html.achievo(Achievo(Secret.name, Secret.pw))
    }
    
}
