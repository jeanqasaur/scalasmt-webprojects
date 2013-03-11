package jCourse

import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._
import cap.jeeveslib.jeeves._
import cap.jeeveslib.env._
import cap.jeeveslib.eval._
import cap.jeeveslib.smt._
import cap.jeeveslib.util._
import java.util.{Date, Calendar}
import scala.collection.mutable.HashMap
//import scala.collection.mutable.Iterable

case class cmContext(viewer: User) extends Atom

object Main extends JeevesLib[cmContext] {
	/*Set Active User*/
	var activeUser = new User();
	var userCtxt = new cmContext(activeUser)
	
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
	
	//Level Variables
	private var _viewerL = mkLevel()
	private var _editorL = mkLevel()
	private var _adminL = mkLevel()
	
	/*Policies*/
	private def isUser(ctxt: ObjectExpr[cmContext]): Formula = {
	  userList.contains(activeUser.username);
	}
	
	private def isInstructor(ctxt: ObjectExpr[cmContext]): Formula = {
	  //ctxt.viewer.permissionLevel === InstructorLevel
	  activeUser.permissionLevel == InstructorLevel
	}
	
	restrict(_viewerL, (ctxt: ObjectExpr[cmContext]) => isUser(ctxt)  )
	restrict(_editorL, (ctxt: ObjectExpr[cmContext]) => isUser(ctxt) )
	restrict(_adminL, (ctxt: ObjectExpr[cmContext]) => isInstructor(ctxt) )
	
	/* Methods */
	def addUser(username:String,
		firstName: String,
		lastName: String,
		email: String,
		password: String,
		permissionLevel: PermissionLevel): String = {
		  userCount +=1
		  var newUser = new User(userCount, username, firstName: String, lastName, email, permissionLevel)
		  newUser.setPassword(password)
		  //TODO: handle if user already exists 
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
		authorName: String): String = {
	  	  	val returnVal = mkSensitiveInt(_adminL, 1, 0);	  
	  	  	if (concretize(userCtxt,returnVal) == 1) {
	  	  		assignmentCount +=1
				val user = userList.get(authorName)
				val uname = user map {_.username} getOrElse "?"
				assignmentList(assignmentCount) = new Assignment(assignmentCount, name, dueDate, maxPoints, prompt, uname)
				"Assignment Count is now " + assignmentCount
	  	  	}
	  	  	else {	"Access Denied"	}
	}
	
	//TODO edit for concurrency/threading/locking
	def addSubmission(submissionTitle: String, 
	    assignmentName: String,
	    fileRef: String,
	    submittedOn: Long): String = {
	  	  	val returnVal = mkSensitiveInt(_editorL, 1, 0);	  	  	
	  	  	if (concretize(userCtxt,returnVal) == 1) {
	  	  		submissionCount+=1
	  	  		submissionList(submissionCount) = new Submission(submissionCount, submissionTitle, assignmentName, activeUser.username, fileRef, submittedOn)
	  	  		"Succesful Submission!"
	  	  	}
	  	  	else {	"Access Denied"	}
	}

	def gradeSubmission(submission: Submission,
	    grade: Double) : String = {
			if(submission.setGrade(userCtxt,grade)) {
			  "Successfully graded!"
			}			
			else {
			  "Access Denied"
			}
	}
	
	def getSubmissions(): String = {
	  val submissions = submissionList.values.toList.toString
	  val returnVal = mkSensitive(_adminL, submissions, "Access Denied");
	  return (concretize(userCtxt, returnVal).asInstanceOf[S]).s
	}
	
	def getAssignments(): Iterable[Assignment] = {
		assignmentList.values.toList
	}	
	
	def getStudents(): String =  {
		val students = userList.values.filter(user => user.permissionLevel == StudentLevel).toList.toString		
		val returnVal = mkSensitive(_adminL, students, "Access Denied");
		return (concretize(userCtxt, returnVal).asInstanceOf[S]).s
	}
		
	def loginUser(uname: String, password: String): Boolean = {
	  var possibleUser = userList(uname)	
	  if (possibleUser.validate(password)) {
	    activeUser = possibleUser
	    userCtxt = new cmContext(activeUser)
	    true	    
	  }
	  else {
	  	false
	  }
	}
	
	def logOut(): Unit = {
	  var activeUser = new User();
	  var userCtxt = new cmContext(activeUser)
	}
	
	def showUsersSubmissions(ctxt: cmContext, uname: String): String = {
		val subs = submissionList.values.filter(submission => submission.submitterName == uname).toList.toString
		val returnVal = mkSensitive(_adminL, subs, "Access Denied");
		return (concretize(userCtxt, returnVal).asInstanceOf[S]).s
	}
	
	def showMySubmissions(ctxt: cmContext): String = {
		val subs = submissionList.values.filter(submission => submission.submitterName == activeUser.username).toList.toString
		val returnVal = mkSensitive(_viewerL, subs, "Access Denied");
		return (concretize(userCtxt, returnVal).asInstanceOf[S]).s
	}
	
	def main(args: Array[String]): Unit = {
	

		
		
		logOut()
		println("Welcome to jCourseManager!")
		println("Available Actions:")
		println("login, logout, viewSubmissions, viewAssignments, viewUserSubmission, gradeAssignment, exit, back")
		var displayState = "start"
		//Scala How to do Cases			
		for( ln <- io.Source.stdin.getLines ) {
			val input = ln.toString()
			if (input.equalsIgnoreCase("exit")) {
			  return
			}
			else if(input.equalsIgnoreCase("back")) {
			  displayState="start"
			  println("Available Actions:")
			  println("login, logout, viewSubmissions, viewAssignments, viewUserSubmission, gradeAssignment, exit, back")
			}
			else {
				if (displayState.equalsIgnoreCase("start")) {
					println("Available Actions:")
					println("login, logout, viewSubmissions, viewAssignments, viewUserSubmission, gradeAssignment, exit, back")
				}
				else if (displayState.equalsIgnoreCase("login")) {
					println("Available Actions:")
					println("login, logout, viewSubmissions, viewAssignments, viewUserSubmission, gradeAssignment, exit, back")
					//var un = readLine("Username:")
					//var pwd = readLine("Password:")
				}
				else {
					println( ln )
				}
			}			
		}
		
	}
}