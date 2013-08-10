package noiseeditor.manager

import noiseeditor.Module
import noiseeditor.modules
import noiseeditor.NoiseEditor

import noiseeditor.datastructure._

// Loads modules and provides access to the currently loaded module
object ModuleManager{
	println("Starting ModuleManager...")
	
	val available = Seq(modules.GameEngine)
	var currentmodule:Module = null
	
	def load(moduletitle:String) {
		println("ModuleManager: Loading Module " + moduletitle)
		available.find(_.title == moduletitle) match {
			case Some(module) =>
				//if( check(module) ) {
					currentmodule = module
					InterpreterManager.reset()
					InterpreterManager(ModuleManager.scalainitcode)
					NoiseEditor.rebuildmenu()
					// Load some preconnected nodes
					try {
						//TODO: Different Resourcepath on Mac OSX?
						FileManager.newSession()
						FileManager.readSession(
							getClass.getClassLoader.getResource("default_"+moduletitle+".xml").getPath
						)
					}
					catch {
						case e:Exception =>
							FileManager.newSession()
					}
/*					}
				else {
					throw new Exception("Error in Module definitions.")
				}*/
			case None =>
				throw new Exception("Module " + moduletitle + " does not exist.")
		}
	}
	
	def title = currentmodule.title
	def scalainitcode = currentmodule.scalainitcode
	def typedefaults = currentmodule.typedefaults
	def sliderdatatypes = currentmodule.sliderdatatypes
	def nodecategories = if(currentmodule != null) currentmodule.nodecategories else Seq()
	def exporttypes = if(currentmodule != null) currentmodule.exporttypes else Seq()
	def export = currentmodule.export _
  def findNodeType(funcName:String) = nodecategories.flatMap(_.nodetypes).find(_.functions("scala").values.head.name == funcName)
	
	def check(module:Module):Boolean = {
		var isvalid = true
		//TODO: Every module needs to contain the language scala
		for( language <- module.languages ) {
			for( NodeCategory(cattitle, nodetypes) <- module.nodecategories ) {
				for( NodeType(title, arguments, sliders, functions) <- nodetypes ) {
					val prefix = "ERROR:   %s / %s / %s: ".format(language, cattitle, title)
					def error(msg:String)   { println(prefix + msg); isvalid = false }
					def warning(msg:String) { println(prefix + msg) }

					if( !arguments.isDefinedAt(language) ) error("Arguments not defined for every language")
						// TODO: no default values for types
						// Warning Different function names
				}
			}
		}
		isvalid
	}
}
