package noiseeditor

import swing._
import config._
import utilities._

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._



object CodeGenerator {

	// Walks through the connected Nodes starting at given OutConnector
	// and creates a simple data structure for code generation
	def composition(out:OutConnector):Composition = {
		import ConnectionManager.connections

		val resultfunction = out.node.functions("result")
		val functions = collection.mutable.Set(resultfunction)
		
		def tree( out: OutConnector ):CompositionTree = {
			// for each connection create a new tree
			// for no connection give the argument's default
			val arguments = out.node.inconnectors.map( connections(_) ).zip(out.node.arguments).map{
				case (connection, argument) =>
					connection match {
						case Some(outconnector) => Right(tree(outconnector)) // recursion
						case None => Left(argument.default)
					}
			}
			functions += out.function
			return CompositionTree(out.function, out.node.id, out.node.sliders, arguments)
		}
		
		val compositiontree = tree(out)
		Composition(functions.toSet, compositiontree)
	}

	
	def involvedsliders(composition:Composition):Set[String] = {
		import composition._
		val involved = collection.mutable.Set[String]()
		val nexttrees = new collection.mutable.Queue[CompositionTree]
		nexttrees += calltree
		while( nexttrees.nonEmpty ) {
			val currenttree = nexttrees.dequeue
			import currenttree._
			involved ++= sliders.map(_.globalname)
			// Move on with all arguments referring to other functions
			nexttrees ++= currenttree.arguments.collect{case Right(x) => x}.distinct
		}
		involved.toSet
	}


	// Generates Scala Code
	def generatecode(composition:Composition):String = {
		import composition._
		
		// Function Definitions
		val functioncode = (for( NodeFunctionFull(name,returntype, code, arguments, sliders) <- functions ) yield {
			"def %s(%s):%s = {%s}".format(
				name,
				(arguments ++ sliders).map(a => "%s:%s".format(a.name, a.datatype)).mkString(", "),
				returntype,
				code
			)
		}).mkString("\n")
		
		
		// Function Calls via BFS
		var functioncalls:Seq[String] = Nil
		val nexttrees = new collection.mutable.Queue[CompositionTree]
		nexttrees += calltree
		while( nexttrees.nonEmpty ) {
			val currenttree = nexttrees.dequeue
			import currenttree._
			functioncalls +:= "val %s = %s(%s)".format(
				varname,
				function.name,
				(
					arguments.map{
						case Right(arg) => arg.varname
						case Left(default) => default }
					++
					sliders.map(s => "%s.value".format(s.globalname))
				).mkString(", ")
			)
			// Move on with all arguments referring to other functions
			nexttrees ++= currenttree.arguments.collect{case Right(x) => x}.distinct
		}
		val functioncallcode = functioncalls.distinct.mkString("\n")
		
		
		// Final Value
		val returnvalue = calltree.varname
		
		"(world:Vec3) => {\n%s\n\n%s\n\n%s\n}\n".format(
			functioncode, functioncallcode, returnvalue
		)
	}
}
