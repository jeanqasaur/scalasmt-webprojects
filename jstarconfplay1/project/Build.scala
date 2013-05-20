import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "jstarconf_play"
  val appVersion      = "1.0-SNAPSHOT"

  libraryDependencies ++= Seq(
    "org.squeryl" %% "squeryl" % "0.9.5-6"
  )

  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    anorm,
    "org.squeryl" %% "squeryl" % "0.9.5-6",
    "commons-io" % "commons-io" % "2.3",
    "commons-lang" % "commons-lang" % "2.6",
    "com.h2database" % "h2" % "1.3.168",
    "mysql" % "mysql-connector-java" % "5.1.19",
    "postgresql" % "postgresql" % "9.1-901-1.jdbc4",
    "org.fusesource.scalate" % "scalate-core_2.10" % "1.6.1"
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental"),
    resolvers += "webjars" at "http://webjars.github.com/m2"
  )

}
