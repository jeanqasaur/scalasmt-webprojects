import cap.jeeves.jconf.frontend

import play.api.Application

object Global extends GlobalSettings {
       
       override def onStart(app: Application) {
       		SessionFactory.concreteFactory = app.configuration.getString("db.default.driver") match {
		case Some("com.mysql.jdbc.Driver") => Some(() => getSession())
		case _ => sys.error("Database driver must be com.mysql.jdbc.Driver")
		}
       }

       def getSession() = {
       	   Init.initDB();
	   Init.initDirectory();
	   Init.initUsers();
	   }
}