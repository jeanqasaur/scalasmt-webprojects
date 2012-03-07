package cap.jeeves.jconf.frontend

//import cap.jeeves._
import cap.jeeves.jconf.backend._
//import JConfBackend._

import java.io.File
import org.apache.commons.io.FileUtils
import org.squeryl.adapters.MySQLAdapter
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Session
import org.squeryl.SessionFactory

object Init {
  private var clearEverything = false;

  def initDB(): Unit = {
    Class.forName("com.mysql.jdbc.Driver");

    val dbUsername = "jeanyang";
    val dbPassword = "scalasmt";
    val dbConnection = "jdbc:mysql://mysql.csail.mit.edu/JeevesConfDB"
    SessionFactory.concreteFactory = Some(()=>
      Session.create(
        java.sql.DriverManager.getConnection(
          dbConnection, dbUsername, dbPassword)
        , new MySQLAdapter))

      try {
        transaction { JConfTables.create; }
      } catch {
        case e: Exception =>
        try {
          if (clearEverything) {
            transaction {
              JConfTables.assignments.deleteWhere(a => a.reviewerId.~ > -1)
              JConfTables.authors.deleteWhere(u => u.paperId.~ > -1)
              JConfTables.papers.deleteWhere(p => p.id.~ > -1)
              JConfTables.reviews.deleteWhere(r => r.id.~ >= -1)
              JConfTables.tags.deleteWhere(t => t.paperId.~ > -1)
              JConfTables.users.deleteWhere(u => u.id.~ > -1)
            }
          }
        }
      }

      clearEverything = {
        transaction {
          val c: Long =
          transaction {
            from(JConfTables.authors)(a =>
            compute(count) )
          }
          c == 0
       }
     }
   }
  
  def initDirectory (): Unit = {
    val path: String = new java.io.File("").getAbsolutePath();
    println(path);
    val paperPath = path + "/webapps/jconf/papers"
    println("trying to make dir " + paperPath)
    val madeDir: Boolean = (new java.io.File(paperPath)).mkdir()
    if (!madeDir) {
      println("Failed to create directory.")
    }

    // Copy files.
    FileUtils.copyDirectory(
      new File(path + "/papers")
    , new File(paperPath) )
  }

  def initUsers (): Unit = {
    if (clearEverything) {
      // Add some dummy users.
      val pcArmando =
        JConfBackend.addUser(
        "asolar@mit.edu", "Armando Solar-Lezama", "MIT"
        , "armando", true, "", PCStatus);
      
      /* Add actual reviewers. */
      JConfBackend.addUser("dummyemail0@mit.edu", "Andrew Rybalchenko", "TUM"
        , RandomGenerator.generatePassword(), true, "", ReviewerStatus )
      JConfBackend.addUser("dummyemail1@mit.edu", "Kathleen Fisher", "Tufts"
        , RandomGenerator.generatePassword(), true, "", ReviewerStatus )
      JConfBackend.addUser("dummyemail2@mit.edu", "Martin Vechev", "ETH"
        , RandomGenerator.generatePassword(), true, "", ReviewerStatus )
      JConfBackend.addUser("dummyemail3@mit.edu", "Isil Dillig", "William & Mary"
        , RandomGenerator.generatePassword(), true, "", ReviewerStatus )
      JConfBackend.addUser("dummyemail4@mit.edu", "Sriram Rajamani", "Microsoft"
        , RandomGenerator.generatePassword(), true, "", ReviewerStatus )

      // These 
      val authorJean =
        JConfBackend.addUser(
        "jeanyang@mit.edu", "Jean Yang", "MIT"
        , "jean", true, "", ReviewerStatus);
      val reviewerKuat =
        JConfBackend.addUser(
        "kuat@mit.edu", "Kuat Yessenov", "MIT"
        , "kuat", true, "", ReviewerStatus);

      // Add some dummy papers.
      val paper0Name = "A Language for Automatically Enforcing Privacy";
      val paper0 = JConfBackend.addPaper(paper0Name, List(authorJean));
      JConfBackend.assignReview(paper0, reviewerKuat);

      val paper1Name = "Matchmaker";
      val paper1 = JConfBackend.addPaper(paper1Name, List(reviewerKuat));
      JConfBackend.assignReview(paper1, authorJean);
    }
   }
 }
