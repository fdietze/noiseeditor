package noiseeditor

import util._
import connector._
import manager._


package object datastructure {

// Map with individual values for each language
type LanguageMap[T] = Map[String,T]
object LanguageMap {
	def apply[T]( args:Tuple2[String,T]* ):LanguageMap[T] = Map(args:_*)
}

// Transposes a Map of Sequences to a Sequence of Maps
implicit def transposeonmapseq[K,V](m:Map[K,Traversable[V]]) = new { def mapseqtranspose = m.values.transpose.map( values => (m.keys zip values).toMap )}
// Transposes a Map of Maps
implicit def transposeonmapmap[K1,K2,V](m:Map[K1,Map[K2,V]]) = new { def mapmaptranspose = (m.values.head.keys zip m.values.map(_.values).transpose.map( x => (m.keys zip x).toMap)).toMap}


// Types for the definition of Nodes used in modules
case class NodeCategory(title:String, nodetypes:Seq[NodeType])

case class NodeType(title:String, arguments:LanguageMap[Seq[NodeArgument]], sliders:Seq[NodeSlider], functions:LanguageMap[Map[String, NodeFunctionFull]]) {
	assert( functions.size > 0, "Nodes need at least one function." )
}


trait NodeFunctionArgument {
	def name:String
	def datatype:String
}

case class NodeArgument(name:String, datatype:String, default:String = "") extends NodeFunctionArgument
case class NodeSlider(name:String, formula:String = "s", initvalue:Int = 50, datatype:String = "") extends NodeFunctionArgument

case class NodeFunctionFull(name:String, returntype:String, code:String, arguments:Seq[NodeArgument], sliders:Seq[NodeSlider])
case class NodeFunction(name:String, returntype:String, code:String)

object NodeType {
	def apply(title:String, arguments:LanguageMap[Seq[NodeArgument]], sliders:Seq[NodeSlider], functions:LanguageMap[ Map[String, NodeFunction]])(implicit e: DummyImplicit) = {
		// store arguments and sliders in every function
		val newfunctions = for( (language, functionmap) <- functions ) yield {
			language -> (for( (title, NodeFunction(name, returntype, code)) <- functionmap ) yield {
				title -> NodeFunctionFull(name, returntype, code, arguments(language), sliders.map {
					// for every slider: add datatype for this language
					case NodeSlider(sname, formula, initvalue, "") => 
					NodeSlider(sname, formula, initvalue, ModuleManager.sliderdatatypes(language))
				})
			})
		}
		
		// add default values to arguments depending on language
		val newarguments = arguments.map {
			case (language,args) =>
				language -> args.map{
					case NodeArgument(name,datatype,"") =>
						NodeArgument(name,datatype,ModuleManager.typedefaults(language)(datatype))
					case argument => argument
				}
		}
		
		new NodeType(title,
		newarguments,
		sliders,
		newfunctions)
	}
}



// Abstract syntax tree
case class CompositionTree(
		function: NodeFunctionFull,
		nodeid: Int,
		sliders: Seq[FormulaSlider],
		arguments: Seq[Either[String, CompositionTree]] ) {
		
	def varname = "vn%d_%s".format(nodeid, function.name)
}

// Used for generating code
// the set of functions makes it easier to build function lists instead of traversing the tree
case class Composition( functions:Set[NodeFunctionFull], calltree:CompositionTree )



// Organizes the Connections between Nodes
class ConnectionTree {
	// Bipartite directed Graph G(FROM,TO)
	// from FROM to TO
	// without cycles over nodes
	// out-degree = 1
	type FROM = InConnector
	type TO   = OutConnector
	type NODE = Node
	
	import collection.mutable.HashMap
	var edges = new HashMap[FROM,TO]
	
	override def toString = "NodeTree(" + edges + ")"
	
	def clear = edges.clear
	
	// add edge and tell if something changed
	def += (edge:(FROM,TO)):Boolean = {
		val (from,to) = edge
		val oldvalue = edges.get(from)
		edges += edge
		if( cycleAt(to,from) ) {
			oldvalue match {
				case Some(value) => edges += (from -> value)
				case None => edges -= from
			}
			false
		}
		else
			true
	}
	
	// Remove Connection from -> to
	def -= (edge:(FROM,TO)) {
		val (from,to) = edge
		edges.get(from) match {
			case Some(to) =>
				edges -= from
			case None =>
		}
	}
	
	// Remove vertices
	def -=[T1]   (vertex:FROM) { edges -= vertex }
	def -=[T1,T2](vertex:TO  ) { edges = edges.filter{ case (_,x) => x ne vertex } }
	
	// Remove all connections to a node
	def -=[T1,T2,T3](node:NODE) {
		for( in <- node.inconnectors )
			this -= in
		for( out <- node.outconnectors )
			this -= out
	}
	
	// Tell if there is a connection: from -> to
	def apply(from:FROM, to:TO):Boolean = {
		if( edges isDefinedAt from )
			edges(from) eq to
		else
			false
	}
	
	// Find parents of vertex
	def apply(vertex:TO):Set[FROM] = {
		val parents = collection.mutable.Set[FROM]()
		for( (from,to) <- edges if vertex eq to )
			parents += from
		parents.toSet
	}

	// Child of vertex
	def apply(vertex:FROM):Option[TO] = edges.get(vertex)



	// Tell if there is a path: to -> from over corresponding nodes
	def cycleAt(to:TO,from:FROM):Boolean = {
		var nextnodes = new collection.mutable.Queue[Node]
		nextnodes += to.node
		while( nextnodes.nonEmpty ) {
			val currentnode = nextnodes.dequeue
			val froms = currentnode.inconnectors
			if( froms contains from )
				return true
			nextnodes ++= froms.map(apply _).flatten.map(_.node).distinct
		}
		return false
	}
}

}
