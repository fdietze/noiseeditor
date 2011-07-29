package noiseeditor

import swing._
import event._
import Orientation._
import javax.swing.border._
import javax.swing.border.BevelBorder._

import java.io.File

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

package object utilities {

def time[A](msg:String)(function: => A) = {
	val start = System.nanoTime
	val returnvalue = function
	val duration = (System.nanoTime-start)/1000000000.0
	printf("%s: %fs\n", msg, duration)
	returnvalue
}

class Timer {
	var starttime = 0L
	var passedtime = 0L

	def getTime = System.nanoTime

	def start  { starttime = getTime }
	def stop   { passedtime += getTime - starttime }
	def measure[A](function: => A) = {
		start
		val returnvalue = function
		stop
		returnvalue
	}
	def reset  { passedtime = 0 }
	def read =   passedtime/1000000000.0
}


def rgbcolor(r:Int, g:Int, b:Int) = r << 16 | g << 8 | b
def graycolor(w:Int) = rgbcolor(w,w,w)
def red(c:Int) = c >> 16
def green(c:Int) = (c & 0x00FF00) >> 8
def blue(c:Int) = c & 0xFF
def mixcolors(a:Int, b:Int, t:Double=0.5) = {
	rgbcolor(
		(t*red(a)   + (1-t)*red(b)  ).toInt,
		(t*green(a) + (1-t)*green(b)).toInt,
		(t*blue(a)  + (1-t)*blue(b) ).toInt
	)
}


class ConnectionTree {
	// Graph without cycles and out-degree = 1
	type FROM = InConnector
	type TO   = OutConnector
	type NODE = Node
	
	import collection.mutable._
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

	def -= (edge:(FROM,TO)) {
		val (from,to) = edge
		edges.get(from) match {
			case Some(to) =>
				edges -= from
			case None =>
		}
	}
	
/*	
	def -=[T1](vertex:FROM) {
		edges -= vertex
	}
	
	def -=[T1,T2](vertex:TO) {
		for( (from,to) <- edges; if to == vertex )
			edges -= from
	}*/

	def -=[T1,T2,T3](node:NODE) {
		edges = edges.filterNot { p => {
			val (k,v) = p
			(k.node ne node) || (v.node ne node)
		}}
	}
	
	def apply(from:FROM, to:TO):Boolean = {
		if( edges isDefinedAt from )
			edges(from) eq to
		else
			false
	}
	
	// Find parents of vertex
	def apply(vertex:TO):Set[FROM] = {
		var parents = Set[FROM]()
		for( (from,to) <- edges if vertex eq to )
			parents += from
		parents
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

class Graph[T] {
	import collection.mutable._
	private var data = new HashMap[T, Set[T]] with MultiMap[T,T]
	var edges = Set[(T,T)]()
	def vertices = data.keys
	
	def reset {
		edges = Set()
		data = new HashMap[T, Set[T]] with MultiMap[T,T]
	}
	
	def += (edge:(T,T)) {
		val (a,b) = edge
		data.addBinding(a,b)
		data.addBinding(b,a)
		if( !edges.contains((b,a)) )
			edges += ((a,b))
	}

	def -= (edge:(T,T)) {
		val (a,b) = edge
		data.removeBinding(a,b)
		data.removeBinding(b,a)
		edges -= ((a,b))
		edges -= ((b,a))
	}

	def -= (vertex:T) {
		val adjacents = apply(vertex)
		for( adjacent <- adjacents ) {
			this -= (vertex, adjacent)
		}
	}
	
	def edgeExists(edge:(T,T)) = {
		data.entryExists(edge._1, x => x == edge._2)
	}
	
	def apply(vertex:T) = {
		data.get(vertex) match{
			case Some(adjacents) => adjacents
			case None => Set()
		}
	}
	
	def flipedge(edge:(T,T)){
		if( edges.contains(edge) || edges.contains((edge._2,edge._1)) )
			this -= edge
		else
			this += edge
	}
}

implicit def awtPointToVec2i( v:java.awt.Point ) = Vec2i(v.x, v.y)
implicit def Vec2iToawtPoint( v:Vec2i ) = new java.awt.Point(v.x, v.y)
implicit def awtDimensionToVec2i( v:java.awt.Dimension ) = Vec2i(v.getWidth.toInt, v.getHeight.toInt)
implicit def Vec2ToawtDimension( v:Vec2i ) = new java.awt.Dimension(v.x, v.y)

class NullPanel(name:String, components:Component*) extends Panel with LayoutContainer {
// Idea from http://dgronau.wordpress.com/2010/08/28/eine-frage-des-layouts/

	components.reverse.foreach( addComponent _ )
	
	override lazy val peer = new javax.swing.JPanel(null) with SuperMixin
	type Constraints = Rectangle
	protected def areValid(c: Constraints): (Boolean, String) = (true, "")
	protected def constraintsFor(comp: Component) = comp.bounds
	def add(c: Component, b: Constraints) {
		if(b != null) {
			c.bounds.x = b.x
			c.bounds.y = b.y
			c.bounds.width = b.width
			c.bounds.height = b.height
			c.peer.setBounds(b)
		}
		peer.add(c.peer)
	}
	
	def addComponent( c:Component ) {
		if( c.bounds.width == 0 && c.bounds.height == 0 ){
			c.peer.setSize(c.preferredSize)
		}
		println(name + ": adding " + c)
		add( c, null )
		revalidate
		repaint
	}
	
	def removeComponent( c:Component ) {
		peer.remove(c.peer)
		revalidate
		repaint
	}
	
	/*def drawFirst(component:Component){
		peer.setComponentZOrder(component.peer,peer.getComponentCount-1)
	}*/

	override def toString = name
}

trait Movable extends Component{
	var dragstart:Vec2i = null
	listenTo(mouse.clicks, mouse.moves)
	reactions += {
		case e:MouseDragged =>
			if( dragstart != null )
				peer.setLocation(location - dragstart + e.point)
		case e:MousePressed =>
			this match {
				case component:Resizable =>
					if( component.resizestart == null )
						dragstart = e.point
				case _ =>
					dragstart = e.point
					 
			}
		case e:MouseReleased =>
			dragstart = null	
	}
}

trait Resizable extends Component{
	import java.awt.Cursor
	import java.awt.Cursor._
	var resizestart:Vec2i = null
	var oldsize = Vec2i(0)
	//val resizecursor = new Cursor(SE_RESIZE_CURSOR)
	//val defaultcursor = new Cursor(DEFAULT_CURSOR)
	def resized {}
	listenTo(mouse.clicks, mouse.moves)
	reactions += {
		case e:MouseDragged =>
			if( resizestart != null ){
				val minsize = Vec2i(50,50)
				peer.setSize(max(oldsize - resizestart + e.point, minsize))
				resized
				revalidate
			}
		case e:MousePressed =>
			if( size.width - e.point.x < 15
			 && size.height - e.point.y < 15 ){
				resizestart = e.point
				oldsize = size
				
				this match {
					case component:Movable =>
						component.dragstart = null
					case _ =>
				}
			}
		case e:MouseReleased =>
			resizestart = null
		
		//TODO: Change Cursor over resizable area
		/*case e:MouseMoved =>
			if( size.width - e.point.x < 15
			 && size.height - e.point.y < 15 ){
				cursor = resizecursor
				println("resizecursor")
			}
			else
				cursor = defaultcursor*/
	}
}

trait ScrollableZoomOffset extends Component{
	var offset = Vec2(0.0)
	var zoom = 1.0
	val zoomFactor = 1.1
	var lastpoint = Vec2(0)
	def transformZoomOffset(v:Vec2) = v * zoom + offset
	def transformZoomOffset(v:Vec3) = v * zoom + Vec3(offset,0.0)
	def scrolledorzoomed {}
	listenTo(mouse.clicks, mouse.moves, mouse.wheel)
	reactions += {
		case e:MouseDragged =>
			val mousepos = Vec2(e.point)
			offset += (lastpoint - mousepos) * zoom
			lastpoint = mousepos
			scrolledorzoomed
		case e:MousePressed =>
			val mousepos = Vec2(e.point)
			lastpoint = mousepos
		case e:MouseWheelMoved =>
			val mousepos = Vec2(e.point)
			if( e.rotation == 1 ){
				zoom *= zoomFactor
				offset += mousepos*zoom/zoomFactor - mousepos*zoom
			}
			else{
				zoom /= zoomFactor
				offset += mousepos*zoom*zoomFactor - mousepos*zoom
			}
			scrolledorzoomed
	}
}

import scala.actors.DaemonActor
case class Job (function:() => Any)
class JobQueue extends DaemonActor {
	start
	def act = {
		loop {
			react {
				case j:Job =>
					try
						reply(j.function())
					catch {
						case e:Exception => println("JobQueue: Exception caught: " + e)
					}
				case _ => throw new RuntimeException("Message was not a Job\n")
			}
		}
	}
}

object Box{	def apply[T](value:T) = new Box[T](value) }
class Box[T](var value:T){ override def toString = value.toString }

class InterpreterQueue extends tools.nsc.interpreter.IMain {
	import javax.script.ScriptException
	import tools.nsc.interpreter.Results._
	import actors.Future
	import actors.Futures.future

	val jq = new JobQueue
	
	
	private def compile[T:Manifest](code:String):Option[T] = {
		//TODO: Important: Better handling of wrong type
		if( interpret(code) == Success ) {
/*			val term = runtimeTypeOfTerm(mostRecentVar)
//			val termtype = manifest[term]
			val manifesttype = manifest[T]
			
			println(term + "\n" + manifesttype + "\n" + manifesttype.erasure.getName)
			
			if( manifesttype != termtype ) {
				println("Type of compiled code does not match:\nIs: " + termtype + "\nShould be: " + manifesttype)
				None
			}
			else*/
				valueOfTerm(mostRecentVar).asInstanceOf[Option[T]]
		}
		else {
			println("error in interpreted code: "+code+"\n")
			None
		}
	}
	
	def apply[T:Manifest](code:String):Future[Option[T]] = {
		(jq !! Job(() => {
			compile[T](code)
		})).asInstanceOf[Future[Option[T]]]
	}
	
	def fbind(name: String, boundType: String, value: Any): Future[Result] = {
		(jq !! Job(() => {
			super.bind(name, boundType, value)
		})).asInstanceOf[Future[Result]]
	}
	
	override def bind(name: String, boundType: String, value: Any): Result = {
		println("Warning: Use fbind() instead of bind() which will return a Future[Result].")
		super.bind(name, boundType, value)
	}
	
	override def reset {
		jq ! Job(() => super.reset)
	}
}

}
