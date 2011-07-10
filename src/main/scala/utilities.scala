package noiseeditor

import swing._
import event._
import Orientation._
import javax.swing.border._
import javax.swing.border.BevelBorder._

import java.io.File

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

package object utilities {

def time[A](msg:String)(function: => A) = {
	val start = System.nanoTime
	val returnvalue = function
	val duration = (System.nanoTime-start)/1000000000.
	printf("%s: %fs\n", msg, duration)
	returnvalue
}

object timer {
	var starttime = 0L
	var endtime = 0L
	def getTime = System.nanoTime

	def start { starttime = getTime }
	def stop = { endtime = getTime }
	def read = (endtime - starttime)/1000000000.
}


def rgbcolor(r:Int, g:Int, b:Int) = r << 16 | g << 8 | b
def graycolor(w:Int) = rgbcolor(w,w,w)
def red(c:Int) = c >> 16
def green(c:Int) = (c & 0x00FF00) >> 8
def blue(c:Int) = c & 0xFF
def mixcolors(a:Int, b:Int, t:Float=0.5f) = {
	rgbcolor(
		(t*red(a)   + (1-t)*red(b)  ).toInt,
		(t*green(a) + (1-t)*green(b)).toInt,
		(t*blue(a)  + (1-t)*blue(b) ).toInt
	)
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
	var offset = Vec2(0)
	var zoom = 1f
	val zoomFactor = 1.1f
	var lastpoint = Vec2(0)
	def transformZoomOffset(v:Vec2) = v * zoom + offset
	def transformZoomOffset(v:Vec3) = v * zoom + Vec3(offset,0f)
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
				case _ => throw new RuntimeException("Message was not a Job")
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
	
	
	private def compile[T](code:String):T = {
		if( interpret(code) == Success )
			valueOfTerm(mostRecentVar) match {
				case Some(result) => result.asInstanceOf[T]
				case None => Unit.asInstanceOf[T]
			}
		else
			throw new ScriptException("error in interpreted code\n")
	}
	
	def apply[T](code:String):Future[T] = {
		(jq !! Job(() => {
			compile[T](code)
		})).asInstanceOf[Future[T]]
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
