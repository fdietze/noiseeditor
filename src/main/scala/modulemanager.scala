package noiseeditor

import utilities._
import datastructures._

//TODO: png export

trait Module {
	val title:String = getClass.getName.split('.').last.dropRight(1)
	val languages:Seq[String]
	val exporttypes:Seq[String]
	val scalainitcode:String
	val nodecategories:Seq[NodeCategory]
	val typedefaults:LanguageMap[Map[String,String]]
	def export(preview:Preview, exporttype:String)
	val sliderdatatypes:LanguageMap[String]
}


object ModuleManager{
	println("Starting ModuleManager...")
	
	val available = Seq(modules.GameEngine)
	
	
	assert(available.size >= 1)
	var currentmodule:Module = available(0)
	
	def reset {
		NoiseEditor.reset
		InterpreterManager.reset
		InterpreterManager(scalainitcode)
	}
	
	def load(moduletitle:String) {
		if( FileManager.unsavedQuestion ) {
			println("ModuleManager: Loading Module " + moduletitle)
			available.find(_.title == moduletitle) match {
				case Some(module) =>
					//if( check(module) ) {
						currentmodule = module
						NoiseEditor.reset
						InterpreterManager.reset
						InterpreterManager(scalainitcode)
/*					}
					else {
						throw new Exception("Error in Module definitions.")
					}*/
				case None =>
					throw new Exception("Module " + moduletitle + " does not exist.")
			}
		}
	}
	
	def title = currentmodule.title
	def scalainitcode = currentmodule.scalainitcode
	def nodecategories = currentmodule.nodecategories
	def typedefaults = currentmodule.typedefaults
	//def languages = currentmodule.languages
	def exporttypes = currentmodule.exporttypes
	def export = currentmodule.export _
	//def resultfunctions = currentmodule.resultfunctions
	def sliderdatatypes = currentmodule.sliderdatatypes
	
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
		return isvalid
	}
}
