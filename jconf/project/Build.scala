import sbt._

object ApplicationBuild extends Build {

  val appName = "myappname"
  val appVersion = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
//    jdbc,filters,
    "org.squeryl" % "squeryl_2.10.0-RC2" % "0.9.5-4",
    "postgresql" % "postgresql" % "9.1-901-1.jdbc4"
  )

   lazy val root = Project(id = "hello",
                    base = file("."),
                    settings = Project.defaultSettings)
}
