import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "JStarConf"
    val appVersion      = "1.0-SNAPSHOT"

   // From the Play tutorial
   val appDependencies = Seq(
     // Add your project dependencies here,
     "org.squeryl" %% "squeryl" % "0.9.5-2", // Copied from above so that it compiles (?)
     "postgresql" % "postgresql" % "9.1-901-1.jdbc4"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      // Add your own project settings here      
      
    )

}
