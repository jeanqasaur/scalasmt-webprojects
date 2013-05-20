//package cap.jeeves.jconf.backend
package models

import JConfBackend._
import cap.jeeveslib.ast.Atom
import play.api.libs.json._
import play.api.libs.functional.syntax._

case class ConfContext( viewer  : ConfUser
                      , var stage   : PaperStage ) extends Atom {
}

//object ConfContext {
//	implicit val ctxtReads = Json.reads[ConfContext]
////	implicit val ctxtReads = (
////		(__ \ 'viewer).read[ConfUser] and
////		(__ \ 'stage).read[PaperStage]
////		)(ConfContext) 
//	implicit val writer = Json.writes[ConfContext];
//	
//}
