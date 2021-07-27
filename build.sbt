scalaVersion := "3.0.0"
name := "gradus-io"
organization := "com.marekscholle.gradus"
version := "0.1"
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "3.1.1",
)
Compile / run / fork := true
