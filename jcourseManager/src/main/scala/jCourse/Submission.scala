package jCourse

import Main._
import cap.jeeveslib.ast.{Atom, Formula, IntExpr, Object, ObjectExpr, S}
import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._
import cap.jeeveslib.jeeves._
import cap.jeeveslib.env._

class Submission(val id: Long,
		val submissionTitle: String, 
	    val assignmentName: String,
	    val submitterName: String,
	    var fileRef: String,
	    val submittedOn: Long) 
    extends Atom {
	
	/* Variables */
	private var grade = -1.0
	//Level Variables
	private var _viewerL = mkLevel()
	private var _editorL = mkLevel()
	private var _adminL = mkLevel()
	
	/* Policies */
	private def isUser(ctxt: ObjectExpr[cmContext]): Formula = {
	  userList.contains(activeUser.username);
	}
	
	private def isSubmitter(ctxt: ObjectExpr[cmContext]): Formula = {
	  //ctxt.viewer.username === submitterName
	  activeUser.username == submitterName
	}
	private def isInstructor(ctxt: ObjectExpr[cmContext]): Formula = {
	  //ctxt.viewer.permissionLevel === InstructorLevel
	  activeUser.permissionLevel == InstructorLevel
	}	
	
	//restrict (levelVariable, (ctxt: typeOf(condition)) => ctxt === condition)	
	restrict(_viewerL, (ctxt: ObjectExpr[cmContext]) => ( isSubmitter(ctxt) || isInstructor(ctxt) ) )
	restrict(_editorL, (ctxt: ObjectExpr[cmContext]) => isSubmitter(ctxt) )
	restrict(_adminL, (ctxt: ObjectExpr[cmContext]) => isInstructor(ctxt) )
	
	/* Getters and Setters */
	def setGrade(context: ObjectExpr[cmContext], score: Double): Boolean = {
	  val returnVal = mkSensitiveInt(_adminL, 1, 0);	  
	  if (concretize(context,returnVal) == 1) {
	    grade = score
	    true
	  }
	  else {	false }
	}
	
	def getGrade(context: ObjectExpr[cmContext]): String = {
	  //Make Sensitve Object		//val sensObj = mkSensitive(levelVariable, highComponentObject, lowComponentObject);	
	  val returnVal = mkSensitive(_viewerL, grade.toString, "Access Denied");
	  //val returnVal = mkSensitive(_editorL, grade.toString, "Access Denied");
	  //Concretize SensitiveVal		//val result = concretize (context, sensObj)
	  return (concretize(context, returnVal).asInstanceOf[S]).s
	}
	
	def printContext(): String = {
	  return activeUser.toString()
	}
	
	def getRef(): String = {
	  fileRef
	}
	
	def editRef(newRef: String): Unit = {
	  fileRef = newRef
	}
	
	def printActiveUser(): Unit = {
	  println(activeUser)
	}
	
	override def toString(): String = {
	  "Submission(#"+id+",A:" + assignmentName + ",S:" + submitterName + ",Date:" + submittedOn.toString + ")"  
	}
}