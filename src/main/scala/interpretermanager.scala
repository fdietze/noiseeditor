package noiseeditor
import utilities._

object InterpreterManager extends InterpreterQueue {
	println("Starting InterpreterManager...")
	
	// no hardcoded classpath
	// http://stackoverflow.com/questions/4713031/how-to-use-scalatest-to-develop-a-compiler-plugin-in-scala/4937135#4937135
	/*import tools.nsc.util.ClassPath
	val loader = getClass.getClassLoader.asInstanceOf[java.net.URLClassLoader]
	val entries = loader.getURLs map(_.getPath)
	// annoyingly, the Scala library is not in our classpath, so we have to add it manually
	val sclpath = entries find(_.endsWith("scala-compiler.jar")) map(
	  _.replaceAll("scala-compiler.jar", "scala-library.jar"))
	settings.classpath.value = ClassPath.join((entries ++ sclpath) : _*)*/
	
	settings.embeddedDefaults[noiseeditor.Material]
	
	
	Seq("lib/simplex3d-math-core.jar","lib/simplex3d-math-double.jar",
		"lib/simplex3d-algorithm-noise.jar").
	    foreach (settings.classpath.append _)
	
	init
	
	def init {
		val imports =
			"import simplex3d.math._\n" + 
			"import simplex3d.math.double._\n" +
			"import simplex3d.math.double.functions._\n" +
			"import simplex3d.noise._\n" +
			"import noiseeditor.Material\n" +
			"import noiseeditor.utilities.Box\n"

		apply(imports)
	}
	
	override def reset {
		super.reset
		init
	}
}
