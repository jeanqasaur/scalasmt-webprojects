package jCourse

import Main._
import jCourse._
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
	//Labels
	private var _viewerL = mkLevel()
	private var _editorL = mkLevel()
	private var _adminL = mkLevel()
	
	/* Policies */
	private def isUser(ctxt: ObjectExpr[CmContext]): Formula = {
	  userList.contains(ctxt.viewer.username)
	  //userList.contains(activeUser.username)
	}
	
	private def isSubmitter(ctxt: ObjectExpr[CmContext]): Formula = {	  
	  ctxt.viewer.username === submitterName
	  //activeUser.username == submitterName
	}
	
	private def isInstructor(ctxt: ObjectExpr[CmContext]): Formula = {
/*	  println(ctxt)
	  println("Context Level:"+ctxt.viewer.permissionLevel.toString())
	  println("Required Level:"+InstructorLevel) */
	  ctxt.viewer.permissionLevel === InstructorLevel  
	  //activeUser.permissionLevel == InstructorLevel
	}	
	
	restrict(_viewerL, (ctxt: ObjectExpr[CmContext]) => ( isSubmitter(ctxt) || isInstructor(ctxt) ) )
	restrict(_editorL, (ctxt: ObjectExpr[CmContext]) => isSubmitter(ctxt) )
	restrict(_adminL, (ctxt: ObjectExpr[CmContext]) => isInstructor(ctxt) )
	
	/* Getters and Setters */
	def setGrade(context: ObjectExpr[CmContext], score: Double): Boolean = {
	  val returnVal = mkSensitiveInt(_adminL, 1, 0);	  
	  if (concretize(context,returnVal) == 1) {
	    grade = score
	    true
	  }
	  else { false }
	}
	
	def getGrade(context: ObjectExpr[CmContext]): String = {
	  val returnVal = mkSensitive(_viewerL, S(grade.toString), S("Access Denied"));
	  return (concretize(context, returnVal).asInstanceOf[S]).s
	}
	
	def printContext(): String = {
	  return activeUser.toString()
	}
	
	def getRef(context: ObjectExpr[CmContext]): String = {
	  fileRef
	}
	
	def editRef(context: ObjectExpr[CmContext], newRef: String): Unit = {
	  fileRef = newRef
	}
	
	def printActiveUser(): Unit = {
	  println(activeUser)
	}
	
	override def toString(): String = {
	  "Submission(#"+id+",A:" + assignmentName + ",S:" + submitterName + ",Date:" + submittedOn.toString + ")"  
	}
}