import org.squeryl._
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Schema
import org.squeryl.adapters.MySQLAdapter
import java.util.Date
import java.util.Calendar
import java.text.SimpleDateFormat
import java.util.Locale
import java.sql.Timestamp
import CourseManager._

object Main {
	var assignmentCount = 1
	var userCount = 1
	var submissionCount = 1
	val assignmentPrivilegeLevel = "Instructor"
	
	def addAssignment(userName: String, name: String, dueDate: Date, prompt: String): Unit = {
		//SELECT * FROM users {WHERE id=1; || WHERE username="bshaibu"};
		var user = CourseManager.users.where(a => a.username === userName).single
		if (user.privileges == assignmentPrivilegeLevel) {
			var assignment = new Assignment(assignmentCount, name, dueDate, prompt,user.id)
			assignmentCount +=1
			var input = "User: " + userName + ",Enter the password:"
			val password = readLine(input)	
			if (user.validate(password)) { 
				try {
					transaction {
						import CourseManager._
						assignments.insert(assignment)
						printDdl
					}
				}
			}
		}
		else {
			println("Only " + assignmentPrivilegeLevel + "s can add assignments!")
		}
	}
	
	def addSubmission(userName: String, assignmentName: String, fileRef: String): Unit = {
		//SELECT * FROM users {WHERE id=1; || WHERE username="bshaibu"};
		var user = CourseManager.users.where(a => a.username === userName).single
		println(CourseManager.assignments.where(a => a.name === assignmentName).single)
 		var assignment = CourseManager.assignments.where(a => a.name === assignmentName).single
		var input = "User: " + userName + ",Enter the password:"
		val password = readLine(input)
		if (user.validate(password)) {
			var date = new java.util.Date();
			var now = new java.sql.Timestamp(date.getTime())
			var submission = new Submission(submissionCount, assignment.id, user.id, fileRef, now)
			submissionCount +=1	
			try {
				inTransaction {
					CourseManager.submissions.insert(submission)
					printDdl
				}
			}
		}
	}

	def main(args: Array[String]): Unit = {
		Class.forName("com.mysql.jdbc.Driver")
		
		val dbUsername = "bshaibu";
		val dbPassword = "giraffes";
		var dbName = "CMTest"
		val dbConnection = "jdbc:mysql://localhost/" + dbName
	
		SessionFactory.concreteFactory = Some( () =>
			Session.create(
				java.sql.DriverManager.getConnection(
					dbConnection, dbUsername, dbPassword),
				new MySQLAdapter)
		)
		
		try {
			transaction {	CourseManager.create	}
		}
		catch {
			case e: Exception => println("Tables already exist!")
			try {
				transaction {
					import CourseManager._
					drop
					create
					printDdl
				}
			}
		}
		try {
			transaction {
				import CourseManager._
				var uml = new User(1,"mclovin","Mohamed","McLovin",Some("superbad@mit.edu"))
				uml.setPassword("secrets")
				var ben = new Student(2,"bshaibu","Benjamin","Shaibu",Some("bshaibu@mit.edu"))
				ben.setPassword("giraffes")
				var jean = new Instructor(3,"jeanyang","Jean","Yang",Some("jeanyang@csail.mit.edu"))
				jean.setPassword("write")
				var amadu = new Student(4,"amadu","Amadu","Durham",Some("amadu@mit.edu"))
				amadu.setPassword("k")
				users.insert(uml)
				users.insert(ben);
				users.insert(jean);
				users.insert(amadu);
				ben.print()
				//println("Old is Student:"+ben.isInstanceOf[Student]) true
				//println("Old is User:"+ben.isInstanceOf[User]) true
				
				//var user = CourseManager.users.where(a => a.username === "bshaibu").single
				//need to check if it's the right class
				//println("New is Student:"+user.isInstanceOf[Student]) false
				//println("New is User:"+user.isInstanceOf[User]) true	
				var date = new SimpleDateFormat("yyyy-MM-dd", Locale.ENGLISH).parse("2013-01-25")
				addAssignment("jeanyang", "Sprint1", date, "Sprints mean running!")
				addAssignment("bshaibu", "Pizza Party", date, "pizza. everywhere.")					
				//addSubmission("bshaibu","Sprint1","println('Hello World!')")
				//addSubmission("amadu", "Sprint1", "println('Hellow World!')")
				println("Hello World! Reached the end!")
			}
		}
	}
}