package noiseeditor
import utilities._

object InterpreterManager extends InterpreterQueue {
	println("Starting InterpreterManager...")
	
	// Set Interpreter Execution Path to Project's Classpath
	settings.embeddedDefaults[noiseeditor.NoiseEditor.type]
	
	
	Seq("lib/simplex3d-math-core.jar","lib/simplex3d-math-double.jar").
			foreach (settings.classpath.append _)
	
	def init {
		val imports =
			"import simplex3d.math._\n" + 
			"import simplex3d.math.double._\n" +
			"import simplex3d.math.double.functions._\n" +
			"import noiseeditor.Material\n" +
			"import noiseeditor.utilities.Box\n"

		apply(imports)
	}
	
	override def reset {
		super.reset
		init
	}
}
