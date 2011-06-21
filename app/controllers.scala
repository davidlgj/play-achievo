package controllers

import play._
import play.mvc._

import models._

object Application extends Controller {
    
    import views.Application._
    
    def index = {
        html.index(DataMocker.days(20))
    }
    
}
