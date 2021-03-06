package noiseeditor.event

import noiseeditor.Node
import noiseeditor.connector._

import swing._
import swing.event._

// These Swing-Events are used to communicate between different Components and Managers

abstract class NoiseEditorEvent(source:Publisher) extends Event {
	//println("Event: " + getClass.getName + ", source: " + source)
	//println("Event: " + this)
}

case class NodeValueChanged(source:Publisher, node:Node, slider:String, value:Double) extends NoiseEditorEvent(source)
case class NodeChanged(source:Publisher, node:Node) extends NoiseEditorEvent(source)
case class HitConnector(source:Publisher, connector:Connector, clicks:Int = 1) extends NoiseEditorEvent(source)
case class NodeMoved(source:Publisher, node:Node) extends NoiseEditorEvent(source)
case class NodeResized(source:Publisher, node:Node) extends NoiseEditorEvent(source)
case class NodeConnected(source:Publisher) extends NoiseEditorEvent(source)
