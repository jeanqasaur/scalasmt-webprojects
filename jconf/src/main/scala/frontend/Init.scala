package cap.jeeves.jconf.frontend

import cap.jeeves.jconf.backend._
import JConfBackend._

import java.io.File
import org.apache.commons.io.FileUtils
import org.squeryl.adapters.MySQLAdapter
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Session
import org.squeryl.SessionFactory

object Init {
  private var clearEverything = false;
  private var test = true;

  def initDB(): Unit = {
    Class.forName("com.mysql.jdbc.Driver");

    val dbName = { if (test) { "JYTestDB" } else { "JeevesConfDB" } }
    val dbUsername = "jeanyang";
    val dbPassword = "scalasmt";
    val dbConnection = "jdbc:mysql://mysql.csail.mit.edu/" + dbName
    SessionFactory.concreteFactory = Some(()=>
      Session.create(
        java.sql.DriverManager.getConnection(
          dbConnection, dbUsername, dbPassword)
        , new MySQLAdapter))

      try {
        transaction { JConfTables.create; }
      } catch {
        case e: Exception =>
        println("Tables exist already.")
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
            from(JConfTables.users)(a =>
            compute(count) )
          }
          c == 0
       }
     }
   }
  
  def initDirectory (): Unit = {
    val path: String = new java.io.File("").getAbsolutePath();
    println(path);
    val paperPath = path + "/webapps/src2012/papers"
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
      val armandoPassword =
        if (test) { "armando" } else { RandomGenerator.generatePassword() }
      val pcArmando =
        addUser(
        "asolar@mit.edu", "Armando Solar-Lezama", "MIT"
        , armandoPassword, true, "", PCStatus);
      
      val studentJean =
        addUser(
        "jeanyang@csail.mit.edu", "Jean Yang", "MIT CSAIL"
        , "jean", true, "", AuthorStatus, List(pcArmando.uid))

      // Add some dummy papers.
      if (test) {
        val authorJean =
          addUser(
          "jeanyang@mit.edu", "Jean Yang", "MIT"
          , "jean", true, "", ReviewerStatus);
        val reviewerKuat =
          addUser(
          "kuat@mit.edu", "Kuat Yessenov", "MIT"
          , "kuat", true, "", ReviewerStatus);

        val paper0Name = "A Language for Automatically Enforcing Privacy";
        val paper0 = addPaper(paper0Name, List(authorJean));
        assignReview(paper0, reviewerKuat);

        val paper1Name = "Matchmaker";
        val paper1 = addPaper(paper1Name, List(reviewerKuat));
        assignReview(paper1, authorJean);
      }
    }
  }
}
