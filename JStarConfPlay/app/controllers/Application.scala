package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

object Application extends Controller {

  val loginForm = Form(
      tuple (
      "username" -> nonEmptyText,
      "password" -> nonEmptyText
      ))
  
  def index = Action {
    Ok(views.html.login_user(loginForm))
  }

  def about = TODO

  def sendPassword = TODO

  def loginUser = TODO

  def login = TODO

  def validateProfile = TODO

  def updateProfile = TODO

  def profile = TODO

  def postPaper = TODO

  def getPaper = TODO

  def uploadedPaper = TODO

  def newPaper = TODO

  def editPaper = TODO

  def withdrawPaper = TODO

  def assignPapers = TODO

  def paperAssignments = TODO

  def makeReview = TODO

  def getReview = TODO

  def editReview = TODO

  def editProfile = TODO

  def logout = TODO  

}