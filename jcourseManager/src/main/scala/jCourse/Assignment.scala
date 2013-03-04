package jCourse

import Main._
import cap.jeeveslib.ast._
import java.util.Date
import scala.math._
import scala.collection.mutable.HashMap

class Assignment(val id: Long,
		var name: String,
		var dueDate: Date,
		var maxPoints: Int,
		var prompt: String,
		val authorName: String) 
	extends Atom {
  
	/* Variables */
	private val assignmentSubmissions = new HashMap[String, Submission]
	
	/* Policies */
	
	/*Mathematical Functions */
	def average(list:List[Double]): Double = {
		list.reduce(_+_)/list.length
		//list.sum/list.length
	}
	
	def std(list:List[Double]): Double = {
		var ave = average(list)
		var variances = (list.map((k:Double)=> k-ave).map((k:Double)=>Math.pow(k,2)))
		return sqrt(variances.sum/(variances.length-1))
	}
	
	def median(list: List[Double]): Double = {
		val (lower,upper) = list.sortWith(_<_).splitAt(list.size/2)
		if (list.size %2 == 0) (lower.last+upper.head)/2.0 else upper.head 
	}
	
	/* Getters and Setter */
	def getSubmissionCount(): Int = {
	  assignmentSubmissions.size
	}
	
	/**/
	override def toString(): String = {
	  "Assignment(" + name + ", id=" + id.toString() + ", prompt=" + prompt + ")"	  
	}

}