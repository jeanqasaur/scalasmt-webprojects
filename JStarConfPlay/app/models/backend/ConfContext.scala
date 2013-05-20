package cap.jeeves.jconf.backend

import JConfBackend._
import cap.jeeveslib.ast.Atom
import play.api.libs.json._

case class ConfContext( viewer  : ConfUser
                      , var stage   : PaperStage ) extends Atom {
}

//object ConfContext {
//	implicit object ContextFormat extends Format[ConfContext] {
//	  def reads(json: JsValue) = ConfContext (
//	      (json \ "viewer").asInstanceOf[ConfUser],
//	      (json \ "stage").asInstanceOf[PaperStage])
//	  implicit val writer = (
//	      (__ \ "viewer").write[ConfUser] and
//	      (__ \ "stage").write[PaperStage]
//	      )(ConfContext.apply _)
//	}
//	
//}
