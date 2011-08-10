package noiseeditor.manager

import noiseeditor.util._

object InterpreterManager extends InterpreterQueue {
	println("Starting InterpreterManager...")
	
	// Set Interpreter Execution Path to Project's Classpath
	settings.embeddedDefaults[noiseeditor.NoiseEditor.type]
	
	
	Seq("lib/simplex3d-math-core.jar","lib/simplex3d-math-double.jar").
			foreach (settings.classpath.append _)
	
	def init {
		val imports =
			"""import simplex3d.math._
			import simplex3d.math.double._
			import simplex3d.math.double.functions._
			import noiseeditor.Material
			import noiseeditor.util.Box"""

		apply(imports)
	}
	
	override def reset {
		println("InterpreterManager: reset")
		super.reset
		init
	}
}


