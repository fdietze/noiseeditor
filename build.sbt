name := "NoiseEditor"

version := "0.1"

scalaVersion := "2.10.6"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-swing" % "2.10.6",
  "org.scala-lang" % "scala-compiler" % "2.10.6",
  "org.scala-lang" % "scala-actors" % "2.10.6",
  "org.simplex3d" %% "simplex3d-data-double" % "2.4.7"
)

//fork in run := true

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Yinline-warnings",
  "-language:_"
)

//scalacOptions += "-optimize"
