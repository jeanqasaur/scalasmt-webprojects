package jCourse

import Main._
class Student(id: Long,
		username:String,
		firstName: String,
		lastName: String,
		email: Option[String]) 
	extends User(id,username,firstName,lastName,email) {
  
  
}