import sbt._

class JConf(info: ProjectInfo) extends DefaultProject(info) {
  val scalatest = "org.scalatest" % "scalatest_2.9.0" % "1.6.1.RC1" % "test"

  override def compileOptions =
    CompileOption("-Xexperimental") ::
    Unchecked ::
    Deprecation ::
    super.compileOptions.toList

  override def libraryDependencies = Set(
  "org.scalatra" %% "scalatra" % "2.0.0.RC1",
  "org.scalatra" %% "scalatra-scalate" % "2.0.0.RC1",
  "org.eclipse.jetty" % "jetty-webapp" % "7.4.5.v20110725" % "test",
  "javax.servlet" % "servlet-api" % "2.5" % "provided",
  "org.scalatra" %% "scalatra-auth" % "2.0.0.RC1"
  ) ++ super.libraryDependencies
}
