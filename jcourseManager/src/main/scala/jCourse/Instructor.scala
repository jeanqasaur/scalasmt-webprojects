package jCourse

import Main._
class Instructor(id: Long,
		username:String,
		firstName: String,
		lastName: String,
		email: Option[String]) 
	extends User(id,username,firstName,lastName,email) {
  
}