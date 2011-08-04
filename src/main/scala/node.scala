package noiseeditor

import utilities._
import config._
import swingextensions._
import datastructures._

import swing._
import event._
import javax.swing.border._
import javax.swing.border.BevelBorder._
import Orientation._
import java.awt.Color._

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import actors.Futures.future

//TODO: comments and help in Nodes

object Node {
	var id = 0
	def nextid = {
		id += 1
		id
	}
	
	def reset {
		id = 0
	}
	
	def getIdMapping(ids:Seq[Int]) = {
		ids map (_ -> nextid) toMap
	}

	def apply(nodetype:NodeType, id:Int = nextid) = {
		new PredefinedNode(nodetype.title, id, nodetype)
	}
	
	def custom(id:Int = nextid) = {
		//TODO: Export to file and load it everytime
		new CustomNode("Custom", id,
			arguments = LanguageMap("scala" -> Seq(
				NodeArgument("v","Vec3","Vec3(0)"),
				NodeArgument("a","Double","0.0"),
				NodeArgument("b","Double","0.0"),
				NodeArgument("c","Double","0.0")
			)),
			customsliders = Seq(
					NodeSlider("lin1", "s", 50, "Double"),
					NodeSlider("lin2", "s", 50, "Double"),
					NodeSlider("exp1", "pow(256,((s-0.5)*2))", 50, "Double"),
					NodeSlider("exp2", "pow(256,((s-0.5)*2))", 50, "Double")
				)
			)
	}

	def loadcustom( sliders:Seq[NodeSlider], arguments:LanguageMap[Seq[NodeArgument]], code:String, id:Int = nextid ) = {
		val node = new CustomNode("Custom", id, arguments, sliders)
		node.funcfield.text = code
		node
	}
	
	def preview(id:Int = nextid) = {
		new Preview(id)
	}
}

class FormulaSlider(slidername:String, nodeid:Int, initvalue:Int = 50) extends Slider with ScrollableSlider {
	name = slidername
	value = initvalue
	val globalname = "n" + nodeid + "_" + name

	var formula = "s"
	var transform:Double => Double = s => s

	def globalvalue = transform(value/100.0)
	tooltip = globalvalue.toString

	//TODO: variable slider size
	preferredSize = Vec2i(100,preferredSize.height)
}

abstract class Node(val title:String, val id:Int = Node.nextid) extends BoxPanel(Vertical) with Movable {
	def arguments:LanguageMap[Seq[NodeArgument]] = LanguageMap()
	def sliderdefinitions:Seq[NodeSlider] = Nil
	def functions:LanguageMap[Map[String, NodeFunctionFull]] = LanguageMap()

	val sliders:Seq[FormulaSlider] = Nil
	val inconnectors:Seq[InConnector] = Nil
	val outconnectors:Seq[OutConnector] = Nil

	def thisnode = this // For accessing the node instance in inner classes
	override def toString = getClass.getName + "(" + title + ")"
}

trait NodeInit extends Node with DelayedInit {
	val titledborder = new TitledBorder(new SoftBevelBorder(RAISED), title)
	border = titledborder

	override val inconnectors = for( NodeArgument(argname,argtype,_) <- arguments("scala") ) yield{
		new InConnector(argname, argtype, thisnode)
	}
	
	override val outconnectors = (for( (title, function) <- functions.mapmaptranspose ) yield {
		new OutConnector(title, function, thisnode)
	}).toSeq
	
	val inconnectorpanel = new BoxPanel(Vertical) {
		contents ++= inconnectors
	}

	val outconnectorpanel = new BoxPanel(Vertical) {
		contents ++= outconnectors
	}

	override val sliders = 
		for( NodeSlider(name, sformula, initvalue,  _) <- sliderdefinitions ) yield {
			val compilation = InterpreterManager[Double => Double]("(s:Double) => " + sformula)
			new FormulaSlider(name, nodeid = id, initvalue) {
				compilation() match {
					case Some(f) =>
						transform = f
						formula = sformula
					case None =>
				}
				tooltip = globalvalue.toString
			}
		}

	val sliderpanel = new GridBagPanel {
		val constraints = new Constraints
		var row = 0
		for( slider <- sliders ) {
			constraints.grid = (0, row)
			layout(new Label(slider.name)) = constraints
			constraints.grid = (1, row)
			layout(slider) = constraints
			row += 1
		}
	}
	
	val removebutton = new RemoveButton("x") {
		reactions += {
			case e:ButtonClicked =>
				NodeManager.remove(thisnode)
		}
	}

	// listen to all connectors and publish events for this node
	for( connector <- inconnectors ++ outconnectors )
		listenTo(connector)

	reactions += {
		case HitConnector(source:Connector, connector) =>
			publish(HitConnector(source=this, connector))
	}	

	// listen to sliders and publish for this node
	for( slider <- sliders )
		listenTo(slider)

	reactions += {
		case ValueChanged(slider:FormulaSlider) =>
			publish(NodeValueChanged(
				source = thisnode,
				node = thisnode,
				slider.globalname,
				slider.globalvalue
			))
			slider.tooltip = slider.globalvalue.toString
	}

	def delayedInit(constructor: => Unit) {
		constructor
		
		//Draw the complete border with title. Even if the content is smaller
		preferredSize = max(titledborder.getMinimumSize(thisnode.peer), preferredSize)
	}
	
	override def toString = "Node("+title+")"
}

class PredefinedNode(title:String, id:Int, nodetype:NodeType) extends Node(title, id) with NodeInit {
	override def functions = nodetype.functions
	override def arguments = nodetype.arguments
	override def sliderdefinitions = nodetype.sliders

	contents +=	new BoxPanel(Horizontal){
		contents += inconnectorpanel
		contents += sliderpanel
		contents += outconnectorpanel
	}
	contents += removebutton
}

class CustomNode(title:String, id:Int, override val arguments:LanguageMap[Seq[NodeArgument]], customsliders:Seq[NodeSlider]) extends Node(title, id) with NodeInit with Resizable {
	//TODO show compile errors in Custom Node?
	override def sliderdefinitions = customsliders
	override def functions = LanguageMap("scala" -> Map("o" -> customfunction))
	
	def customfunction = NodeFunctionFull(
		name = "custom_f" + id,
		returntype = "Double",
		code = if(funcfield != null) funcfield.text else "0.0",
		arguments = arguments("scala"),
		customsliders
	)

	val funcfield = new TextArea("0.0") {
		font = new Font("Monospaced", java.awt.Font.PLAIN, 11)
		tabSize = 4
		//lineWrap = true
		editable = true
	}
	
	val compilebutton = new Button("compile")

	def compile {
		outconnectors(0).function = LanguageMap("scala" -> customfunction)
		publish(NodeChanged(source = thisnode, node = thisnode))
	}
	
	listenTo(funcfield, compilebutton)
	reactions += {
		case ButtonClicked(`compilebutton`) => compile
	}
	
	val controlpanel = new BoxPanel(Horizontal) {
			contents += inconnectorpanel
			contents += sliderpanel
			contents += outconnectorpanel
			maximumSize = preferredSize
	}

	contents +=	controlpanel
	contents += new ScrollPane(funcfield)
	contents += new BoxPanel(Horizontal){
		contents += compilebutton
		contents += removebutton
	}
}

