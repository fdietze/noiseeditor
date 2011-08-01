package noiseeditor
import utilities._
//TODO: Compile check of all Nodes
//TODO: More High Level nodes, like Surface, Layers, Fractal Noise, Turbulence
//TODO: More Noise types, like cellular noise
//TODO: Tooltip with node description
//TODO: Different Noise Dimensions

trait Module {
	val title:String = getClass.getName.split('.').last.dropRight(1)
	val scalainitcode:String
	val nodeCategories:Seq[NodeCategory]
	val languages:Seq[String]
}

object DummyModule extends Module {
	val scalainitcode = "null"
	val nodeCategories = Nil
	val languages = Nil
}

object ModuleManager{
	println("Starting ModuleManager...")
	
	val available = Seq(modules.GameEngine)
	
	var currentmodule:Module = DummyModule
	
	def load(module:Module) {
		import actors.Futures.future
		if( FileManager.unsavedQuestion ) {
			println("ModuleManager: Loading Module " + module.title)
			currentmodule = module
			NoiseEditor.reset
			InterpreterManager(scalainitcode)
		}
	}
	
	def scalainitcode = currentmodule.scalainitcode
	def nodeCategories = currentmodule.nodeCategories
}
