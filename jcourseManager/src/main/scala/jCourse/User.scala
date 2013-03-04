package jCourse

import Main._
import java.security.MessageDigest
import scala.collection.mutable.HashMap
import scala.None
import java.util.Date

class User(val id: Long,
			val username:String,
			var firstName: String,
			var lastName: String,
			var email: String) {
	
	def this() {
	  this(-1,"","","","")
	}
	
	def this(id: Long, username:String, firstName: String, lastName: String) {
		this(id, username, firstName, lastName,username+"@mit.edu")
	}
  
	private var password : String = ""
	var permissionLevel = "None"
	
	/* Level Variables */
	  
	/* Policies */
	  
	/* Password Functions */
	def md5(s:String):String = {
		new String(MessageDigest.getInstance("MD5").digest(s.getBytes))
	}

	def setPassword(pass:String) {
		password = md5(pass)
	}

	def validate(pass:String) = {
		password == md5(pass)
	}
	
	/*Action Methods */
	def submitAssignment(assignment: Assignment, name:String) {
		//assignments += name -> null
	}
	
	def createAssignment(assignmentName: String, 
	    dueDate: Date, 
	    maxPoints: Int,
		prompt: String): Assignment = {
	  new Assignment(0, assignmentName, dueDate, maxPoints, prompt, username)	  
	}
	
	def setPermission(pL: String): Boolean = {	        
		if (pL.equalsIgnoreCase("Student")) {
			permissionLevel = "Student"
			true
		}
		else if (pL.equalsIgnoreCase("Instructor")) {
		  permissionLevel = "Instructor"
		  true
		}
		else {
		  false
		}
	}
	
/*	def getSubmissions(): List[Option[Submission]] = {
	  new List[Some(new Submission())]
	}*/
	
	/* Default Methods */
    def print(): Unit =	{
        println("My name is "+username+".")
    }
    
    override def toString(): String = {
      "User("+username+")"
    }
    /*override def equals(that: Any) = that match {
      case other: Base
    }*/
}