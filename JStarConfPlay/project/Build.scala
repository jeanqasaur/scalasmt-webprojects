import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "JStarConfPlay"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      // Add your project dependencies here,
      "org.squeryl" %% "squeryl" % "0.9.5-2",
      "commons-io" % "commons-io" % "2.3",
      "commons-lang" % "commons-lang" % "2.6",
      "com.h2database" % "h2" % "1.3.168",
      "mysql" % "mysql-connector-java" % "5.1.19",
      "postgresql" % "postgresql" % "9.1-901-1.jdbc4",
      "org.fusesource.scalate" % "scalate-core" % "1.5.3"
      )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
            
      scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental")
     )

}
