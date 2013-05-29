package noiseeditor.manager

import noiseeditor.Node
import noiseeditor.event._
import noiseeditor.NoiseEditor

import noiseeditor.util._
import noiseeditor.swingextension._

import swing.event._
import java.awt.event.InputEvent.BUTTON1_DOWN_MASK


import simplex3d.math._


// A Component containing all Nodes
object NodeManager extends NullPanel("NodeManager") {
	println("Starting NodeManager...")
	
	var nodes = Set[Node]()
	val slidervalues = new collection.mutable.HashMap[String, Box[Double]]
	var spawnpos:Option[Vec2i] = None

	def add( node:Node ) {
		nodes += node
		addComponent(node)
		
		spawnpos match {
			case Some(pos) =>
				node.peer.setLocation(pos - node.size/2)
				spawnpos = None
			case None =>
				//TODO: Bug: after moving working area this value is not true anymore
				node.peer.setLocation(Vec2i(10,10))
		}
		
		listenTo(node)
		
		for(slider <- node.sliders) {
			slidervalues(slider.globalname) = Box(slider.globalvalue)
// TODO: bind slidervalues in background (?)			future{
				InterpreterManager.fbind(
					slider.globalname,
					"noiseeditor.util.Box[Double]",
					slidervalues(slider.globalname)
				)
//			}
		}
	}

	def remove( node:Node, publishChange:Boolean = true ) {
		deafTo(node)
		node.deafTo(ConnectionManager, NodeManager)
		
		ConnectionManager.removeNode(node)
		
		removeComponent(node)
		nodes -= node
		if( publishChange )
			publish(NodeConnected(this))
	}
	
	def reset() {
		nodes.foreach(remove(_, publishChange = false))
		publish(NodeConnected(this))
	}

	reactions += {
		case NodeValueChanged(source, node, slider, value) if(source ne this) =>
			slidervalues(slider).value = value
			publish(NodeValueChanged(source = this, node, slider, value))

		case NodeChanged(source, node) if(source ne this) =>
			publish(NodeChanged(source = this, node))

		case HitConnector(source, connector, clicks) if(source ne this) =>
			publish(HitConnector(source = this, connector, clicks))

		case UIElementMoved(source:Node) =>
			publish(NodeMoved(this, node = source))

		case UIElementResized(source:Node) =>
			publish(NodeResized(this, node = source))

		case UIElementResized(source) if( source eq NoiseEditor.window) =>
			peer.setSize(source.size)
	}
	
	// Context Menu with all Window-Menu-Items
	reactions += {
		case e:MousePressed =>
			if(e.triggersPopup) {
				val popupMenu = new PopupMenu {
					contents ++= NoiseEditor.createmenu.menus
				}
				spawnpos = Some(e.point)
				popupMenu.show(this, e.point.x, e.point.y)
			}
	}

	
	// Open files with double click
	reactions += {
		// double click with left mouse button
		// TODO: don't use hard coded modifier: 0
		case MouseClicked(_, point, 0, 2, _) =>
			if( FileManager.unsavedQuestion() ) FileManager.open()
	}
	

	// Movable Working Area
	var lastpoint = Vec2i(0)
	listenTo(mouse.clicks, mouse.moves, mouse.wheel)
	reactions += {
		// mouse drag with left mouse button
		case e @ MouseDragged(_, point, BUTTON1_DOWN_MASK) =>
			val mousepos:Vec2i = point
			val offset = lastpoint - mousepos
			lastpoint = mousepos
			for( node <- nodes )
				node.peer.setLocation(node.location - offset)

		// single click with left mouse button
		case MousePressed(_, point, BUTTON1_DOWN_MASK, 1, _) =>
			val mousepos = Vec2i(point)
			lastpoint = mousepos

		// scrolled down
		case MouseWheelMoved(_,_,_,1) =>
				for( node <- nodes )
					node.peer.setLocation(node.location + Vec2i(0,-20))

		// scrolled up
		case MouseWheelMoved(_,_,_,-1) =>
				for( node <- nodes )
					node.peer.setLocation(node.location + Vec2i(0,20))
			
	}

  def materialNodes = nodes.filter( n =>
       n.functions("scala").head._2.returntype == "Material"
    && n.functions("scala").head._2.arguments.isEmpty
    ).toSeq.sortBy(_.id)
}
