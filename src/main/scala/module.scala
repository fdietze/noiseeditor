package noiseeditor

import util._
import datastructure._

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
