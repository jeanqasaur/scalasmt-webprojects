package cap.jeeves.jconf.backend

import JConfBackend._

case class ConfContext( viewer  : ConfUser
                      , var stage   : PaperStage ) extends JeevesRecord
