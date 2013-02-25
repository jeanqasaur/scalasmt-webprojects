import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Schema
import org.squeryl.annotations.Column
import java.sql.Timestamp

class Submission(val id: Long,
				 @Column("ASSIGNMENT_ID") val assignmentId: Long, 
				 @Column("USER_ID") val submitterId: Long, 
				 val fileRef: String,
				 val submittedOn: Timestamp)