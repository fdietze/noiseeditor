package noiseeditor

import util._
import datastructure._

trait Module {
	val title:String = getClass.getName.split('.').last.dropRight(1)
	val languages:Seq[String] = Seq("scala")
	val exporttypes:Seq[String] = Seq()
	val scalainitcode:String = ""
	lazy val nodecategories:Seq[NodeCategory] = Seq()
	val typedefaults:LanguageMap[Map[String,String]] = Map()
	def export(preview:Preview, exporttype:String) {}
	val sliderdatatypes:LanguageMap[String]
}
