package noiseeditor

import swing._
import config._
import utilities._

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._



class Composition extends Publisher{
	var densityfunction:( Vec3 ) => (Double, Material) = (v) => (0,Material())
	var code = "0"
	var involvedsliders = Set[String]()

	def apply( v:Vec3 ):(Double, Material) = densityfunction(v)
	
	def functiontree(in:InConnector):FunctionTree = {
		import ConnectionManager.connections

		connections(in) match {
			case Some(out) =>
				val children = out.node.inconnectors.map(
						nextin => nextin.title -> functiontree(nextin)
					).filter( x => x._2 != null ).toMap

				val sliders = out.node.sliders.map(
					s => s.name -> s.globalname
				).toMap

				FunctionTree(out.function, children, sliders, out.node.id)
				
			case None =>
				null
		}
	}

	def generatecode(tree:FunctionTree):String = {
		var functions = Set[(Function,Map[String,String],Int)]()
		
		// get all Functions via BFS
		var nexttrees = new collection.mutable.Queue[FunctionTree]
		nexttrees += tree
		while( nexttrees.nonEmpty ) {
			val currenttree = nexttrees.dequeue
			functions += ((currenttree.function, currenttree.sliders, currenttree.nodeid))
			nexttrees ++= currenttree.parameters.values
		}
		
		println(functions)
		
		""
	}

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

		val nextnodes = new collection.mutable.Queue[Node]

		// If there is a connection at this densityconnector
		connections(densityconnector) match {
			case Some(connector) =>
				resultingval.density = "vn" + connector.node.id + "_" + connector.funcname
				nextnodes += connector.node
			case None =>
		}
		
		// If there is a connection at this materialconnector
		connections(materialconnector) match {
			case Some(connector) =>
				resultingval.material = "vn" + connector.node.id + "_" + connector.funcname
				nextnodes += connector.node
			case None =>
		}
				
		while( nextnodes.nonEmpty ) {
			val currentnode = nextnodes.dequeue
			import currentnode._
			
			val inconnections = inconnectors.map( c => connections(c) )
			
			val currentargs =
			((for( (inconnection, intype) <- inconnections zip intypes ) yield {
				
				inconnection match {
					case Some(connector) =>
						nextnodes += connector.node
						"vn" + connector.node.id + "_" + connector.funcname
					case None =>
						val RegexArg(_, argtype, _, argdefault) = intype
						if( argdefault == null )
							TypeDefaults(argtype)
						else
							argdefault
				}
			}) ++
	
			(if( constantsliders )
				sliders.map( s => s.globalvalue )
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
