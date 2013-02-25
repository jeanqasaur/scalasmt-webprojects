package jCourse

import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._
import cap.jeeveslib.jeeves._
import cap.jeeveslib.env._
import cap.jeeveslib.eval._
import cap.jeeveslib.smt._
import cap.jeeveslib.util._
import java.util.Date

import scala.collection.mutable.HashMap

case class cmContext(viewer: User) extends Atom
object Main extends JeevesLib[cmContext] {
	
	/* Initialize dictionaries */
	val userList = new HashMap[String, User]
	//val instructorList = new Hashmap[String, Instructor]
	//val studentList = new HashMap[String, Student]
	val assignmentList = new HashMap[String, Assignment]
	val submissionList = new HashMap[String, Submission]	
	
	
	/*System Variables */
	val emptyStringVal = S("")
	
	/*Count Variables */
	private var submissionCount = -1
	private var assignmentCount = -1
	private var userCount = -1
	
	/* Methods */
	def addUser(username:String,
			firstName: String,
			lastName: String,
			email: Option [String]): User = {
	  userCount +=1
	  new User(userCount, username, firstName: String, lastName, email)
	}
	
	def addAssignment(name: String,
	    dueDate: Date,
		maxPoints: Int,
		prompt: String,
		authorName: String): Assignment = {
			assignmentCount +=1
			val user = userList.get("default")
			val uname = user map {_.username} getOrElse "?"
			new Assignment(assignmentCount, name, dueDate, maxPoints, prompt, uname)
	}
	
	//edit for concurrency/threading/locking
	def addSubmission(submissionTitle: String, 
	    assignmentName: String,
	    submitterName: String,
	    fileRef: String,
	    submittedOn: Long): Submission = {
	  submissionCount+=1
	  new Submission(submissionCount, submissionTitle, assignmentName, submitterName, fileRef, submittedOn)
	}
	
	def gradeAssignment() : Int = {
	  0
	}
	
	//def getSubmissions(): List[Option[Submission]] = submissionList
	
	//def getAssignments(): List[Option[Assignment]] = assignmentList
	
	//def getStudents(): List[Option[Student]]
	
	
	def loginUser(uname: String, password: String): Option[User] = {
	  userList.get(uname)
	}
	
/*	def showUsersSubmissions(ctxt: cmContext, uname: String): List[Submissions] = {
	  
	}*/
	
	def main(args: Array[String]): Unit = {
		val l = mkLevel();
		val x = mkSensitiveInt(l, 42, -1);

		println("Expect 42:");
		println(concretize(l === HIGH, x) );
		println("Expect -1:");
		println(concretize(l === LOW, x));		
	}
}