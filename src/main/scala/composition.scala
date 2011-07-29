package noiseeditor

import swing._
import config._
import utilities._

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

class FunctionTree {
	class Node {
		val children = new collection.mutable.HashMap[String, FunctionNode]()
		def apply(parameter:String) = children(parameter)
		def update(parameter:String, function:Function) { children(parameter) = new FunctionNode(function) }
	}
	
	class FunctionNode(val function:Function) extends Node {
		def name = function.name
		def code = function.code
		def outtype = function.outtype
	}
	
	val root = new Node
	def apply(parameter:String) = root(parameter)
	def update(parameter:String, function:Function) { root(parameter) = function }
}

class Composition extends Publisher{
	var densityfunction:( Vec3 ) => (Double, Material) = (v) => (0,Material())
	var code = "0"
	var involvedsliders = Set[String]()

	def apply( v:Vec3 ):(Double, Material) = densityfunction(v)
	
	/*def functiontree(
		densityconnector:InConnector,
		materialconnector:InConnector):FunctionTree = {
		
		import ConnectionManager.connections
		val tree = new FunctionTree
		val densityconnection = connections(densityconnector)
		val materialconnection = connections(materialconnector)

		val nextnodes = new collection.mutable.Queue[Node]
		


		// If there is a connection at this densityconnector
		if( densityconnection.nonEmpty ){
			val connector = densityconnection.head
			tree("d") = connector.function
			nextnodes += connector.node
		}			
		
		tree
	}*/
	
	def generate (
		densityconnector:InConnector,
		materialconnector:InConnector,
		constantsliders:Boolean = false ):String = {

		import ConnectionManager.connections
		
		var nodevals:Seq[String] = Nil
		var funcdefs = Set[String]()
		involvedsliders = Set[String]()
		
		val resultingval = new {
			var density ="0.0"
			var material="Material()"
		}

		val densityconnection = connections(densityconnector)
		val materialconnection = connections(materialconnector)

		val nextnodes = new collection.mutable.Queue[Node]
		


		// If there is a connection at this densityconnector
		if( densityconnection.nonEmpty ){
			val connector = densityconnection.head
			resultingval.density = "vn" + connector.node.id + "_" + connector.funcname
			nextnodes += connector.node
		}			
		
		// If there is a connection at this materialconnector
		if( materialconnection.nonEmpty ){
			val connector = materialconnection.head
			resultingval.material = "vn" + connector.node.id + "_" + connector.funcname
			nextnodes += connector.node
		}			
				
		while( nextnodes.nonEmpty )
		{
			val currentnode = nextnodes.dequeue
			import currentnode._
			
			val inconnections = inconnectors.map(c => connections(c))
			
			val currentargs = 
			((for( (inconnection, intype) <- inconnections zip intypes ) yield {
				if( inconnection.nonEmpty ) {
					var connector = inconnection.head
			
						nextnodes += connector.node
						"vn" + connector.node.id + "_" + connector.funcname
				}
				else {
					val RegexArg(_, argtype, _, argdefault) = intype
					if( argdefault == null )
						TypeDefaults(argtype)
					else
						argdefault
				}
			}) ++
	
			(if( constantsliders )
				sliders.map(s => s.globalvalue + "f")
			else {
				for( slider <- sliders ) yield {
					involvedsliders += slider.globalname
					slider.globalname + ".value"
				}
			}
	
			)).mkString(", ")

			for( Function(name, code, outtype, _) <- currentnode.functions ) {
				funcdefs += "def %s(%s) = {%s}".format(
					name,
					(intypes ++ sliders.map( s => s.name + ":Double")).mkString(", "),
					code
				)
				var currentnodeval = "val vn%d_%s = %s(%s)".format(
					id,
					name,
					name,
					currentargs
				)

				nodevals +:= currentnodeval
			}
		}
		
		val composition = """(source:Vec3) => {
val noise1 = new Noise(ClassicalGradientNoise){
	def apply(u:Vec3):Double = super.apply(u)
}
%s

%s

(%s, %s)
}""".format(
			funcdefs.mkString("\n"),
			nodevals.distinct.mkString("\n"),
			resultingval.density,
			resultingval.material
			)
		
		if( !constantsliders ) {
			code = composition
			//println("new composition:\n" + code + "\n\n")
		}

		composition
	}
	
	def compile {
		type t = (Vec3) => (Double, Material)
		//type t = Function1[Vec3, Tuple2[Double, Material]]
		val compilation = InterpreterManager[t](code)
		compilation() match {
			case Some(function) => 
				densityfunction = function
			case None =>
		}
	}
}
