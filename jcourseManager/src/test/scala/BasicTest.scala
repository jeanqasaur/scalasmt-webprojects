import org.scalatest.FunSuite
import jCourse._
import java.util.Date

class BasicTest extends FunSuite {
	test ("create environment") {
	  Main.addUser("ProfessorA", "Instructor", "One", "prof1@mit.edu", "beFair")
	  Main.addUser("ProfessorB", "Professor", "Two", "prof2@mit.edu", "firehose")
	  Main.addUser("StudentA", "Student", "One", "stud1@mit.edu", "dilligent")
	  Main.addUser("StudentB", "Student", "Two", "stud2@mit.edu", "nope")
	  
	  Main.addAssignment("Assignment One", new Date(2013, 3, 4), 100, "Eat your vegetables.", "ProfessorA")
	  Main.addAssignment("Assignment Two", new Date(2013, 3, 27), 93, "Design a chatwindow.", "ProfesosrA")
	  Main.addAssignment("Assignment Three", new Date(2013, 4, 1), 1000, "Solve P?=NP.", "ProfessorB")
	  
	  Main.addSubmission("sub1", "Assignment One", "StudentA", "I ate Broccoli.", 123456789)
	  Main.addSubmission("sub2", "Assignment Two", "StudentA", "I redid AIM", 123458889)
	  Main.addSubmission("sub3", "Assignment Three", "StudentB", "Obviously P=NP because everyone else is wrong.", 123488889)
	}
}