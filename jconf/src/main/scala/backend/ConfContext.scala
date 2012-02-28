package cap.jeeves.jconf.backend

import JConfBackend._

/*
 * User records for jconf case study.
 * @author jeanyang
 */

case class ConfContext( viewer  : ConfUser
                      , stage   : PaperStage ) extends JeevesRecord
