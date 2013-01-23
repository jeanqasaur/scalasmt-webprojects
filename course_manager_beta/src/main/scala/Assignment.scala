import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Schema
import org.squeryl.annotations.Column
import scala.math._
import java.util.Date

class Assignment(val id: Long,
				var name: String,
				var dueDate: Date,
				var prompt: String,
				@Column("USER_ID") val authorId: Long
				) {
	
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

}