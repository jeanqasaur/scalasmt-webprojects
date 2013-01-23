import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Schema
import org.squeryl.annotations.Column
 
object CourseManager extends Schema {
	//When the table name doesn't match the class name, it is specified here :
	val users = table[User]("USERS")
	
	val assignments = table[Assignment]("ASSIGNMENTS")
	
	val submissions = table[Submission]	("SUBMISSIONS")
 }