package cap.jeeves.jconf.backend

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import JConfBackend._

case class StringVal(v: String) extends JeevesRecord {
  implicit def fromString(str: String) = StringVal(str)
}
