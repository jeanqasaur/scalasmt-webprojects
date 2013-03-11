import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import jCourse._
import java.util.Date
import cap.jeeveslib.ast.Atom
import cap.jeeveslib.jeeves.JeevesLib
import Main._

case class Dummy(id: BigInt) extends Atom
class BasicTest extends FunSuite with BeforeAndAfter with JeevesLib[Dummy] {	  
	
	test("Basic Jeeves Functionality") {
		val l = mkLevel();
		val x = mkSensitiveInt(l, 42, -1);

		assert(concretize(l === HIGH, x) === 42)
		assert(concretize(l === LOW, x) === -1)	
	}
	
	test ("Create Environment") {
		Main.addUser("ProfessorA", "Instructor", "One", "prof1@mit.edu", "beFair", InstructorLevel)
		Main.addUser("ProfessorB", "Professor", "Two", "prof2@mit.edu", "firehose", InstructorLevel)
		Main.addUser("StudentA", "Student", "One", "stud1@mit.edu", "dilligent", StudentLevel)
		Main.addUser("StudentB", "Student", "Two", "stud2@mit.edu", "nope", StudentLevel)	
		
		loginUser("ProfessorA","beFair")
		Main.addAssignment("Assignment One", new Date(2013, 3, 4), 100, "Eat your vegetables.", "ProfessorA")
		Main.addAssignment("Assignment Two", new Date(2013, 3, 27), 93, "Design a chatwindow.", "ProfesosrA")
		loginUser("ProfessorB","firehose")
		Main.addAssignment("Assignment Three", new Date(2013, 4, 1), 1000, "Solve P?=NP.", "ProfessorB")
		
		loginUser("StudentA","dilligent")
		Main.addSubmission("sub1", "Assignment One", "I ate Broccoli.", System.currentTimeMillis())
		Main.addSubmission("sub2", "Assignment Two", "I redid AIM", System.currentTimeMillis())
		loginUser("StudentB","nope")
		Main.addSubmission("sub3", "Assignment Three", "Obviously P=NP because everyone else is wrong.", System.currentTimeMillis())
		
		assert(userList.size === 4)
		assert(assignmentList.size === 3)
		assert(submissionList.size === 3)
	}
	
	test("Instructor Creates Assignment") {
		loginUser("ProfessorA","beFair")
		Main.addAssignment("Assignment Four", new Date(2013, 5, 4), 100, "Create a factorial program.", "ProfessorA")
		assert(assignmentList.size === 4)
		assert(assignmentList(3).name === "Assignment Four")	  
	}
	
	test("Student Views Assignment and Enters Submission") {
		loginUser("StudentA","dilligent")
		assert(getAssignments().size === 4)
		assert(Main.addSubmission("sub3", "Assignment Four", "recursion", System.currentTimeMillis()) === "Succesful Submission!")		
	}
	
	test("Instructor views all Submissions") {
		loginUser("ProfessorA","beFair")
		assert(Main.getSubmissions() != "Access Denied")
	}
	
	test("Instructor views a Student Submission") {
		loginUser("ProfessorA","beFair")
		assert(submissionList(0).getGrade(userCtxt) === "-1.0")
	}
	
	test("Instructor Grades Assignments") {
	  	loginUser("ProfessorA","beFair")
		Main.gradeSubmission(submissionList(0), 100)
		assert(submissionList(0).getGrade(userCtxt) === "100.0")
		Main.gradeSubmission(submissionList(1), 85)
		assert(submissionList(1).getGrade(userCtxt) === "85.0")
		loginUser("ProfessorB","firehose")
		Main.gradeSubmission(submissionList(2), 10)
		assert(submissionList(2).getGrade(userCtxt) === "10.0")
		Main.gradeSubmission(submissionList(3), 40)
		assert(submissionList(3).getGrade(userCtxt) === "40.0")
	}
	
	test("Student Views their Submission") {
		loginUser("StudentA","dilligent")			
		assert(submissionList(0).getGrade(userCtxt) === "100.0")
		assert(submissionList(1).getGrade(userCtxt) === "85.0")
		assert(submissionList(3).getGrade(userCtxt) === "40.0")
	}

	test("Student Fail to View Other Student's Submission") {
		loginUser("StudentB","nope")
		assert(submissionList(0).getGrade(userCtxt) === "Access Denied")
	}
	
	test("Student Attempt to Grade A Student's Submission") {
		loginUser("StudentB","nope")
		assert(Main.gradeSubmission(submissionList(2), 100) === "Access Denied")		
	}
	
	test("Student Attempts to Create Assignment") {
		loginUser("StudentB","nope")	
		assert(Main.addAssignment("Assignment Five", new Date(2013, 4, 17), 1000, "Give StudentB money.", "StudentB") === "Access Denied")
	}
	
	test("Student Attempts to View All Students") {
		loginUser("StudentB","nope")
		assert(Main.getStudents() === "Access Denied")
	}
	
	test("Instructor views all students") {
		loginUser("ProfessorA","beFair")
		assert(Main.getStudents() != "Access Denied")
	}
}