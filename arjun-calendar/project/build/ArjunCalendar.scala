import sbt._

class ScalaSMT(info: ProjectInfo) extends DefaultProject(info) {
  val scalatest = "org.scalatest" % "scalatest_2.9.0" % "1.6.1.RC1" % "test"
}
