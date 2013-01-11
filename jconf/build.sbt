organization := "com.example"

name := "scalatra-sbt-prototype"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.9.0-1"

seq(webSettings :_*)

classpathTypes ~= (_ + "orbit")

libraryDependencies ++= Seq(
  "org.scalatra" %% "scalatra" % "2.1.0-SNAPSHOT",
  "org.scalatra" %% "scalatra-scalate" % "2.1.0-SNAPSHOT",
  "org.scalatra" %% "scalatra-fileupload" % "2.1.0-SNAPSHOT",
  "org.scalatra" %% "scalatra-specs2" % "2.1.0-SNAPSHOT" % "test",
  "org.scalatest" %% "scalatest" % "1.8" % "test",
  "ch.qos.logback" % "logback-classic" % "1.0.6" % "runtime",
  "commons-lang" % "commons-lang" % "2.6",
  "commons-io" % "commons-io" % "2.3",
  "org.squeryl" %% "squeryl" % "0.9.5-2",
  "com.h2database" % "h2" % "1.3.168",
  "mysql" % "mysql-connector-java" % "5.1.19",
  "postgresql" % "postgresql" % "9.1-901-1.jdbc4",
  "org.eclipse.jetty" % "jetty-webapp" % "8.1.7.v20120910" % "container;test",
  "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "container;provided;test" artifacts (Artifact("javax.servlet", "jar", "jar"))
)

libraryDependencies ++= Seq("org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "container,compile"         artifacts (Artifact("javax.servlet", "jar", "jar")))

resolvers += "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental")
