//import cap.jeeves.jconf.frontend._
import models._
import org.squeryl.{Session, SessionFactory}

import play.api.GlobalSettings
import play.api.Application
import play.api.db.DB
import org.squeryl.adapters.MySQLAdapter

object Global extends GlobalSettings{
       
      val dbUsername = "jeanyang";
      val dbPassword = "scalasmt";
      val dbConnection = "jdbc:mysql://mysql.csail.mit.edu/JYTestDB"
 
      override def onStart(app: Application) {
       		SessionFactory.concreteFactory = app.configuration.getString("db.default.driver") match {
		case Some("com.mysql.jdbc.Driver") => Some(() => getSession(new MySQLAdapter, app))
		case _ => Some (() => sys.error("Database driver must be com.mysql.jdbc.Driver"))
		}
       }

       def getSession(adapter:MySQLAdapter, app: Application) = {
       	   Session.create(java.sql.DriverManager.getConnection(dbConnection, dbUsername, dbPassword), adapter);   
	   }
}