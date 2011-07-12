package noiseeditor

import swing._
import config._
import utilities._

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

class Composition extends Publisher{
	var densityfunction:( Vec3 ) => (Float, Material) = (v) => (0f,Material())
	var code = "0f"
	var involvedsliders = Set[String]()

	def apply( v:Vec3 ):(Float, Material) = densityfunction(v)
	
	def generate (
		densityconnector:InConnector,
		materialconnector:InConnector,
		constantsliders:Boolean = false ):String = {

		import ConnectionManager.connections
		
		var nodevals:Seq[String] = Nil
		var funcdefs = Set[String]()
		involvedsliders = Set[String]()
		
		val resultingval = new {
			var density ="0f"
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
					var connectors:Set[_ <: Connector] = Set(inconnection.head)
					var prefix = ""
					var suffix = ""
			
					val RegexArg(_, RegexType(argsupertype, _, _), _, _) = intype
					// If this connector accepts more than one input
					if( argsupertype == "Seq" ) {
						connectors = inconnection.toSet
						prefix = "Seq("
						suffix = ")"
					}
			
					prefix + (for( connector <- connectors ) yield {
						nextnodes += connector.node
						"vn" + connector.node.id + "_" + connector.funcname
					}).mkString(", ") + suffix
				}
				else {
					val RegexArg(_, RegexType(argsupertype, _, _), _, argdefault) = intype
					if( argdefault == null )
						TypeDefaults(argsupertype)
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
					(intypes ++ sliders.map( s => s.name + ":Float")).mkString(", "),
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
	def apply(u:Vec3):Float = super.apply(u).toFloat
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
		type t = (Vec3) => (Float, Material)
		//type t = Function1[Vec3, Tuple2[Float, Material]]
		val compilation = InterpreterManager[t](code)
		compilation() match {
			case Some(function) => 
				densityfunction = function
			case None =>
		}
	}
}
