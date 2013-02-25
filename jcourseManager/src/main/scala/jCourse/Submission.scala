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
	private var grade = -1
	//Level Variables
	private var _submitterL = mkLevel()
	private var _instructorL = mkLevel()
	
	/* Policies */
	
	
	/* Getters and Setters */
	def setGrade(score: Int) {
	  val grade = score
	}
	
	def getGrade(): Int = {
		grade
	}
	
	def getRef(): String = {
	  fileRef
	}
	
	def editRef(newRef: String): Unit = {
	  fileRef = newRef
	}
	
	override def toString(): String = {
		"Submission for " + assignmentName + " by " + submitterName + " on " + "timestamp."  
	}
}