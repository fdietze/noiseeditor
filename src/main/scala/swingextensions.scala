package noiseeditor

import swing._
import event._
import Orientation._
import javax.swing.border._
import javax.swing.border.BevelBorder._
import javax.swing.JPopupMenu

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

package object swingextensions {

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
	def resized(delta:Vec2i) {}
	listenTo(mouse.clicks, mouse.moves)
	reactions += {
		case e:MouseDragged =>
			if( resizestart != null ){
				val delta = e.point - resizestart
				peer.setSize(max(oldsize + delta, minimumSize))

				oldsize += delta
				resizestart += delta

				resized(delta)
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

trait ScrollableZoomOffset extends Component {
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
			if( e.rotation == 1 ) {
				zoom *= zoomFactor
				offset += mousepos*zoom/zoomFactor - mousepos*zoom
			}
			else {
				zoom /= zoomFactor
				offset += mousepos*zoom*zoomFactor - mousepos*zoom
			}
			scrolledorzoomed
	}
}

trait ScrollableSlider extends Slider {
	listenTo(mouse.wheel)
	reactions += {
		case e:MouseWheelMoved =>
			value -= e.rotation
	}
}


import javax.swing.JPopupMenu
import scala.swing.{ Component, MenuItem }
import scala.swing.SequentialContainer.Wrapper

object PopupMenu {
	private[PopupMenu] trait JPopupMenuMixin { def popupMenuWrapper: PopupMenu }
}

class PopupMenu extends Component with Wrapper {
	override lazy val peer: JPopupMenu = new JPopupMenu with PopupMenu.JPopupMenuMixin with SuperMixin {
		def popupMenuWrapper = PopupMenu.this
	}

	def show(invoker: Component, x: Int, y: Int): Unit = peer.show(invoker.peer, x, y)
}



}
