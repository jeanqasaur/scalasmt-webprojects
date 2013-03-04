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
//import scala.collection.mutable.Iterable

case class cmContext(viewer: User) extends Atom
object Main extends JeevesLib[cmContext] {
	/*Set Active User*/
	var activeUser = new User();
	
	/* Initialize dictionaries */
	val userList = new HashMap[String, User]
	val assignmentList = new HashMap[Int, Assignment]
	val submissionList = new HashMap[Int, Submission]	
	
	
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
		email: String,
		password: String): String = {
		  userCount +=1
		  var newUser = new User(userCount, username, firstName: String, lastName, email)
		  newUser.setPassword(password)
		  //handle user already exists 
		  userList(username) = newUser
		  username
	}
	
	def setUserPermissionLevel(username: String, permission: String): Boolean = {
		userList.get(username).get.setPermission(permission)
	}
	
	def addAssignment(name: String,
	    dueDate: Date,
		maxPoints: Int,
		prompt: String,
		authorName: String): Int = {
			assignmentCount +=1
			val user = userList.get(authorName)
			val uname = user map {_.username} getOrElse "?"
			assignmentList(assignmentCount) = new Assignment(assignmentCount, name, dueDate, maxPoints, prompt, uname)
			assignmentCount
	}
	
	//edit for concurrency/threading/locking
	def addSubmission(submissionTitle: String, 
	    assignmentName: String,
	    submitterName: String,
	    fileRef: String,
	    submittedOn: Long): Int = {
			submissionCount+=1
			submissionList(submissionCount) = new Submission(submissionCount, submissionTitle, assignmentName, submitterName, fileRef, submittedOn)
			submissionCount
	}

	def gradeSubmission(submission: Submission,
	    grade: Double) : Submission = {
			submission.setGrade(grade)
			submission
	}
	
	def getSubmissions(): Iterable[Submission] = {
		submissionList.values.toList
	}
	
	def getAssignments(): Iterable[Assignment] = {
		assignmentList.values.toList
	}	
	
	def getStudents(): List[Submission] =  {
		submissionList.values.toList
	}
		
	def loginUser(uname: String, password: String): Option[User] = {
	  var possibleUser = userList.get(uname).get
	  if (possibleUser.validate(password)) {
	    activeUser = possibleUser
	    Some(activeUser)	    
	  }
	  else {
	    None
	  }
	}
	
	def showUsersSubmissions(ctxt: cmContext, uname: String): List[Submission] = {
		submissionList.values.filter(submission => submission.submitterName == uname).toList
	}
	
	def main(args: Array[String]): Unit = {
		val l = mkLevel();
		val x = mkSensitiveInt(l, 42, -1);

		println("Expect 42:");
		println(concretize(l === HIGH, x) );
		println("Expect -1:");
		println(concretize(l === LOW, x));		
	}
}