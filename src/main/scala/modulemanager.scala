package noiseeditor

import utilities._
import datastructures._

//TODO: Compile check of all Nodes
//TODO: png export

trait Module {
	val title:String = getClass.getName.split('.').last.dropRight(1)
	val languages:Seq[String]
	val scalainitcode:String
	val nodeCategories:Seq[NodeCategory]
	val typedefaults:LanguageMap[Map[String,String]]
	def export(composition:Composition, language:String):String
	val resultfunctions:LanguageMap[NodeFunctionFull]
	val sliderdatatypes:LanguageMap[String]
}


object ModuleManager{
	println("Starting ModuleManager...")
	
	val available = Seq(modules.GameEngine)
	
	
	assert(available.size >= 1)
	var currentmodule:Module = available(0)
	
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
	def nodecategories = currentmodule.nodeCategories
	def typedefaults = currentmodule.typedefaults
	def languages = currentmodule.languages
	def export = currentmodule.export _
	def resultfunctions = currentmodule.resultfunctions
	def sliderdatatypes = currentmodule.sliderdatatypes
}
