name := "NoiseEditor"

version := "0.1"

scalaVersion := "2.9.0-1"

libraryDependencies ++= Seq(
	"org.scala-lang" % "scala-swing" % "2.9.0-1",
	"org.scala-lang" % "scala-compiler" % "2.9.0-1",
	"com.nativelibs4java" % "scalacl" % "0.2"
)

//fork in run := true

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-optimize"


// ScalaCL:

resolvers += "NativeLibs4Java Repository" at "http://nativelibs4java.sourceforge.net/maven/"

autoCompilerPlugins := true

addCompilerPlugin("com.nativelibs4java" % "scalacl-compiler-plugin" % "0.2")

