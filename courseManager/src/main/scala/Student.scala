import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Schema
import org.squeryl.annotations.Column
class Student(id: Long,
				username:String,
				firstName: String,
				lastName: String,
				email: Option[String]) extends User(id,username,firstName,lastName,email) {
	override var privileges = "Student"
}