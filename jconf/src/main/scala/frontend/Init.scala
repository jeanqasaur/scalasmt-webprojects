package cap.jeeves.jconf.frontend

//import cap.jeeves._
import cap.jeeves.jconf.backend._
//import JConfBackend._

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
  
  def initDummyUsers (): Unit = {
    if (clearEverything) {
      // Add some dummy users.
      val pcArmando =
        JConfBackend.addUser(
        "asolar@mit.edu", "Armando Solar-Lezama", "armando", PCStatus);
      val authorJean =
        JConfBackend.addUser(
        "jeanyang@mit.edu", "Jean Yang", "jean", ReviewerStatus);
      val reviewerKuat =
        JConfBackend.addUser(
        "kuat@mit.edu", "Kuat Yessenov", "kuat", ReviewerStatus);

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
