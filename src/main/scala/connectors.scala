package noiseeditor

import swing._
import event._
import utilities._
import config._
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

	reactions += {
		case e:ButtonClicked =>
			publish(HitConnector(source = this,connector = this))
	}
	override def toString = getClass.getName.split('.').last + "(" + title + ", " + node + ")"
	def funcname = ""
}

case class InConnector(
		override val title:String,
		override val datatype:String,
		override val node:Node ) extends Connector(title, datatype, node)

case class OutConnector(
		override val title:String,
		val function:NodeFunction,
		override val node:Node) extends Connector(title, function.returntype, node) {
}


