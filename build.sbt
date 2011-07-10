name := "NoiseEditor"

version := "0.1"

scalaVersion := "2.9.0-1"

libraryDependencies ++= Seq(
	"org.scala-lang" % "scala-swing" % "2.9.0-1",
	"org.scala-lang" % "scala-compiler" % "2.9.0-1"
)

defaultExcludes ~= (filter => filter || "*~")

