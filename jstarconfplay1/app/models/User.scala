package models

import anorm._
import anorm.SqlParser._
import play.api.db._
import play.api.Play.current

case class User(id:Long, email:String, password:String, fName:String, lName:String)

object User {

       val user = {
       	 get[Long] ("id") ~
	 get[String] ("email") ~
	 get[String] ("password") ~
	 get[String] ("firstName") ~
	 get[String] ("lastName") map 
	 { case id~email~password~firstName~lastName => User(id,email,password,firstName,lastName)}
	 }
 
       def authenticate(e:String, p:String) {

       	   
       }

       def create(email:String, password:String, confirm_password:String, fName:String, lName:String) {
       	   DB.withConnection { implicit c =>
   	      SQL("insert into User (email, password, firstName, lastName) values ({email}, {password}, {firstName}, {lastName})").on(
      	      "email" -> email,
	      "password" -> password,
	      "firstName" -> fName,
	      "lastName" -> lName
    	      ).executeUpdate()
	}
       }

       def all():List[User] = DB.withConnection { implicit c =>
       	   SQL("select * from User").as(user *)
       }

       def checkUser(email:String, password:String): Boolean = DB.withConnection { implicit c =>
       	   val findUser = SQL("SELECT * FROM User WHERE email = {emailaddress} and password = {pass}").on(
	   "emailaddress" -> email,
	   "pass" -> password)

	   val user = findUser().map (row =>
	       row[String]("email")).toList

	   return user.size == 1
       }
       	   

}