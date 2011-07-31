package noiseeditor

import swing._
import event._
import utilities._
import config._

import javax.swing.border._
import javax.swing.border.BevelBorder._
import Orientation._
import java.awt.Color._

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import actors.Futures.future



object NodeManager extends NullPanel("NodeManager") {
	println("Starting NodeManager...")
	
	var nodes = Set[Node]()
	val slidervalues = new collection.mutable.HashMap[String, Box[Double]]
	var spawnpos = Vec2i(10,10)
	
	def add( node:Node ) {
		nodes += node
		addComponent(node)
		//TODO: Better Node placing
		node.peer.setLocation(spawnpos)
		//spawnpos += Vec2i(0,10)
		
		listenTo(node)
		
		for(slider <- node.sliders) {
			slidervalues(slider.globalname) = Box(slider.globalvalue)
			InterpreterManager.fbind(slider.globalname, "noiseeditor.utilities.Box[Double]", slidervalues(slider.globalname))
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
	
	def reset {
		nodes.foreach(remove(_, false))
		publish(NodeConnected(this))
	}

	reactions += {
		case NodeValueChanged(source, node, slider, value) if(source ne this) =>
			slidervalues(slider).value = value
			publish(NodeValueChanged(source = this, node, slider, value))

		case NodeChanged(source, node) if(source ne this) =>
			publish(NodeChanged(source = this, node))

		case HitConnector(source, connector) if(source ne this) =>
			publish(HitConnector(source = this, connector))

		case UIElementMoved(source:Node) =>
			publish(NodeMoved(this, node = source))

		case UIElementResized(source:Node) =>
			publish(NodeResized(this, node = source))

		case UIElementResized(source) if( source eq NoiseEditor.top) =>
			peer.setSize(source.size)
	}
	
	// Movable Working Area
	var lastpoint = Vec2i(0)
	listenTo(mouse.clicks, mouse.moves, mouse.wheel)
	reactions += {
		case e:MouseDragged =>
			val mousepos:Vec2i = Vec2i(e.point)
			val offset = lastpoint - mousepos
			lastpoint = mousepos
			for( node <- nodes )
				node.peer.setLocation(node.location - offset)

		case e:MousePressed =>
			val mousepos = Vec2i(e.point)
			lastpoint = mousepos
		case e:MouseClicked =>
			if( e.clicks == 2 ) // doubleclick
				FileManager.open
		case e:MouseWheelMoved =>
			if( e.rotation == 1 ) {
				val offset = Vec2i(0,20)
				for( node <- nodes )
					node.peer.setLocation(node.location - offset)
			}
			else {
				val offset = Vec2i(0,-20)
				for( node <- nodes )
					node.peer.setLocation(node.location - offset)
			}
			
	}

}
