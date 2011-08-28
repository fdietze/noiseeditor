package noiseeditor.connector

import noiseeditor.Node
import noiseeditor.event._

import noiseeditor.util._
import noiseeditor.config._
import noiseeditor.datastructure._

import swing._
import swing.event._
import javax.swing.SwingUtilities._
import Orientation._
import java.awt.Color._

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._


abstract class Connector(val title:String, val datatype:String, val node:Node) extends Button(title) {
	margin = new Insets(0,0,0,0)
	val originalbackground = background
	val highlightbackground = ConnectorHighlightColor
	tooltip = "Type: " + datatype
	
	
	listenTo(mouse.clicks)	
	
	reactions += {
		case e:ButtonClicked =>
			publish(HitConnector(source = this, connector = this))
		case MouseClicked(_, point, 0, 2, _) =>
			publish(HitConnector(source = this, connector = this, clicks=2))
	}
	override def toString = getClass.getName.split('.').last + "(" + title + ", " + node + ")"
}

case class InConnector(
		override val title:String,
		override val datatype:String,
		val argdefault:String,
		override val node:Node ) extends Connector(title, datatype, node)

case class OutConnector(
		override val title:String,
		var function:LanguageMap[NodeFunctionFull],
		override val node:Node) extends Connector(title, function("scala").returntype, node) {
}

