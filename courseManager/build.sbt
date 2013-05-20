organization := "com.example"

name := "course-manager-beta"

version := "0.1.0"

scalaVersion := "2.10.0"

classpathTypes ~= (_ + "orbit")

libraryDependencies ++= Seq(
  "org.squeryl" %% "squeryl" % "0.9.5-6",
  "com.h2database" % "h2" % "1.3.168",
  "mysql" % "mysql-connector-java" % "5.1.19",
  "postgresql" % "postgresql" % "9.1-901-1.jdbc4",
  "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
 )

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental")