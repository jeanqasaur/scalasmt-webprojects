import sbt._

class JConf(info: ProjectInfo) extends DefaultWebProject(info) {
  System.setProperty("smt.home", "/opt/z3/bin/z3")

  val scalatest = "org.scalatest" % "scalatest_2.9.0" % "1.6.1.RC1" % "test"
  val h2 = "com.h2database" % "h2" % "1.2.127"
  val mysqlDriver = "mysql" % "mysql-connector-java" % "5.1.10"
  val posgresDriver = "postgresql" % "postgresql" % "8.4-701.jdbc4"
  val msSqlDriver = "net.sourceforge.jtds" % "jtds" % "1.2.4"
  val derbyDriver = "org.apache.derby" % "derby" % "10.7.1.1"
  val scalatraVersion = "2.0.0.RC1"

  override def compileOptions =
    CompileOption("-Xexperimental") ::
    Unchecked ::
    Deprecation ::
    super.compileOptions.toList

  override def libraryDependencies = Set(
    "org.scalatra" %% "scalatra" % "2.0.0.RC1"
  , "org.scalatra" %% "scalatra-scalate" % "2.0.0.RC1"
  , "org.eclipse.jetty" % "jetty-webapp" % "7.4.5.v20110725" % "test"
  , "javax.servlet" % "servlet-api" % "2.5" % "provided"
  , "org.scalatra" %% "scalatra-auth" % "2.0.0.RC1"
  , "org.squeryl" %% "squeryl" % "0.9.5-RC1"
  , "postgresql" % "postgresql" % "8.4-701.jdbc4"
  , "org.scalatra" %% "scalatra-fileupload" % scalatraVersion
  ) ++ super.libraryDependencies
}
