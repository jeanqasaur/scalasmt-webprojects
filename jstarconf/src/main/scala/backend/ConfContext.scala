package cap.jeeves.jconf.backend

import JConfBackend._
import cap.jeeveslib.ast.Atom

case class ConfContext( viewer  : ConfUser
                      , var stage   : PaperStage ) extends Atom
