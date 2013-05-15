package noiseeditor

import datastructure._

trait Module {
	val title:String = getClass.getName.split('.').last.dropRight(1)
	val languages:Seq[String] = Seq("scala")
	val scalainitcode:String = ""
	val typedefaults:LanguageMap[Map[String,String]] = Map()
	val sliderdatatypes:LanguageMap[String]
	lazy val nodecategories:Seq[NodeCategory] = Seq()
	val exporttypes:Seq[String] = Seq()
	def export(preview:Preview, exporttype:String) {}
}
