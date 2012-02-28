package cap.jeeves.jconf.frontend

//import cap.jeeves._
import cap.jeeves.jconf.backend._
//import JConfBackend._

import org.squeryl.adapters.MySQLAdapter
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Session
import org.squeryl.SessionFactory

object Init {
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
          // TODO: Stop taking all users out of the DB at some point...
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
  
  def initDummyUsers (): Unit = {
     // Add some dummy users.
     val pcArmando =
       JConfBackend.addUser(
        "armando", "Armando Solar-Lezama", "armando", PCStatus);
     val authorJean =
       JConfBackend.addUser(
        "jeanyang", "Jean Yang", "jean", ReviewerStatus);
     val reviewerKuat =
       JConfBackend.addUser(
       "kuat", "Kuat Yessenov", "kuat", ReviewerStatus);

     // Add some dummy papers.
     val paper0Name = "A Language for Automatically Enforcing Privacy";
     val paper0 = JConfBackend.addPaper(paper0Name, List(authorJean), Nil);
     JConfBackend.assignReview(paper0, reviewerKuat);

     val paper1Name = "Matchmaker";
     val paper1 = JConfBackend.addPaper(paper1Name, List(reviewerKuat), Nil);
     JConfBackend.assignReview(paper1, authorJean);
   }
 }
