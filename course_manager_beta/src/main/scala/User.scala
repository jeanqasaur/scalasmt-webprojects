import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Schema
import org.squeryl.annotations.Column
import java.security.MessageDigest
import scala.collection.mutable._

class User(val id: Long,
			val username:String,
			var firstName: String,
			var lastName: String,
			var email: Option [String]) {
	private var password : String = ""
	var privileges = "None"

	def md5(s:String):String = {
		new String(MessageDigest.getInstance("MD5").digest(s.getBytes))
	}

	def setPassword(pass:String) {
		password = md5(pass)
	}

	def validate(pass:String) = {
		password == md5(pass)
	}

	def submitAssignment(assignment: Assignment, name:String) {
		//assignments += name -> null
	}
	
    def print(): Unit =	{
        println("My name is "+username+".")
    }
}