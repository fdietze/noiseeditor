name := "NoiseEditor"

version := "0.1"

scalaVersion := "2.10.1"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-swing" % "2.10.1",
  "org.scala-lang" % "scala-compiler" % "2.10.1",
  "org.scala-lang" % "scala-actors" % "2.10.1",
  "org.simplex3d" %% "simplex3d-data-double" % "2.4.7"
)

//fork in run := true

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

//scalacOptions += "-optimize"
