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
	
	def addAssignment(userName: String, name: String, dueDate: Date, prompt: String, password: String): Unit = {
		//SELECT * FROM users {WHERE id=1; || WHERE username="bshaibu"};
		var user = CourseManager.users.where(a => a.username === userName).single
		if (user.privileges == assignmentPrivilegeLevel) {
			var assignment = new Assignment(assignmentCount, name, dueDate, prompt,user.id)
			if (user.validate(password)) { 
				//inTransaction {
					assignments.insert(assignment)
					assignmentCount +=1
					printDdl
					println(assignment.name + " added successfully!")
				//}
			}
		}
		else {
			println("Submission failed; Only " + assignmentPrivilegeLevel + "s can add assignments!")
		}
	}
	
	def addSubmission(userName: String, assignmentName: String, fileRef: String, password: String): Unit = {
		//SELECT * FROM users {WHERE id=1; || WHERE username="bshaibu"};
		var user = CourseManager.users.where(u => u.username === userName).single
		var assignment = assignments.where(u => u.name === assignmentName).single
		var input = "User: " + userName + ",Enter the password:"
		if (user.validate(password)) {
			var date = new java.util.Date();
			var now = new java.sql.Timestamp(date.getTime())
			var submission = new Submission(submissionCount, assignment.id, user.id, fileRef, now)
			try {
				CourseManager.submissions.insert(submission)
				submissionCount +=1
				printDdl
			}
		}
	}

	/* def viewSubmission(viewer: User, target: User): String = {
		var user = CourseManager.users.where(u => u.username === viewer.username).single
		var submission = "You do not have access"
		if (user.privileges == "Instructor" || viewer == target) {
				submission = CourseManager.submissions.where(s => s.submitterId === target.id).single.fileRef
		}
		submission
	} */
	
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
			case e: Exception => 
			println("Tables already exist!")
			try {
				transaction {
					drop
					create
					printDdl
				}
			}
		}
		try {
			val uml = new User(1,"mclovin","Mohamed","McLovin",Some("superbad@mit.edu"))
			uml.setPassword("secrets")
			val ben = new Student(2,"bshaibu","Benjamin","Shaibu",Some("bshaibu@mit.edu"))
			ben.setPassword("giraffes")
			val jean = new Instructor(3,"jeanyang","Jean","Yang",Some("jeanyang@csail.mit.edu"))
			jean.setPassword("write")
			val amadu = new Student(4,"amadu","Amadu","Durham",Some("amadu@mit.edu"))
			amadu.setPassword("1401")
				
			transaction {
				users.insert(uml)
				users.insert(ben);
				users.insert(jean);
				users.insert(amadu);

				var date = new SimpleDateFormat("yyyy-MM-dd", Locale.ENGLISH).parse("2013-01-25")
				addAssignment("jeanyang", "Sprint1", date, "Sprints mean running!","write")
				addAssignment("bshaibu", "Pizza Party", date, "pizza. everywhere.","giraffes")
			
				addSubmission("bshaibu","Sprint1","print('Hello World!')","giraffes")
				addSubmission("amadu", "Sprint1", "println('Hellow World!')","1401")								

				// var submission = submissions.where(s => s.submitterId === 2).single.fileRef
				// println(submission)
				println("Jean views User:" + viewSubmission(jean,uml))
				println("User views User:" + viewSubmission(uml,uml))
				println("Amadu views User:" + viewSubmission(uml,uml))
			}
			
			println("Hello World! Reached the end!")			
		}
		catch {
			case e: Exception => e.printStackTrace
		}
	}

}