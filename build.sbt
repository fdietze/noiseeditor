name := "NoiseEditor"

version := "0.1"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
	"org.scala-lang" % "scala-swing" % "2.9.1",
	"org.scala-lang" % "scala-compiler" % "2.9.1"
)

defaultExcludes ~= (filter => filter || "*~")

//fork in run := true

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"
