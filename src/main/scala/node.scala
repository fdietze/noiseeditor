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
import simplex3d.math.float._
import simplex3d.math.float.functions._

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

	def apply(nodetype:FunctionNodeType, id:Int = nextid) = {
		new PredefinedNode(nodetype.title, id, nodetype)
	}
	
	def custom(id:Int = nextid) = {
		//TODO: Export to file and load it everytime
		new CustomNode("Custom", id,
			slidernames = Seq("s1","s2","s3","s4"),
			intypes = Seq("a:Float","b:Float","c:Float","d:Float"))
	}

	def loadcustom( slidernames:Seq[String], intypes:Seq[String], function:String, id:Int = nextid ) = {
		val node = new CustomNode("Custom", id, slidernames, intypes)
		node.funcfield.text = function
		node
	}
	
	def preview(id:Int = nextid) = {
		new Preview(id)
	}
}

class FunctionSlider(slidername:String, nodeid:Int, initvalue:Int = 50) extends Slider {
	name = slidername
	value = initvalue
	val globalname = "n" + nodeid + "_" + name

	var tformula = "s"
	var transform:Float => Float = s => s

	def globalvalue = transform(value/100f)
	tooltip = globalvalue.toString

	//TODO: variable slider size
	preferredSize = Vec2i(100,preferredSize.height)
}

abstract class Node(val title:String, val id:Int = Node.nextid) extends BoxPanel(Vertical) with Movable {
	def intypes:Seq[String] = Nil
	def sliderdefinitions:Seq[AnyRef] = Nil
	def functions:Seq[Function] = Nil

	val sliders:Seq[FunctionSlider] = Nil
	val inconnectors:Seq[InConnector] = Nil
	val outconnectors:Seq[OutConnector] = Nil

	def thisnode = this // For accessing the node instance in inner classes
	override def toString = getClass.getName + "(" + title + ")"
}

trait NodeInit extends Node with DelayedInit {
	val titledborder = new TitledBorder(new SoftBevelBorder(RAISED), title)
	border = titledborder

	override val inconnectors = for( intype <- intypes ) yield{
		val RegexArg(argname, argtype, _, _) = intype
		new InConnector(argname, argtype, thisnode)
	}
	override val outconnectors = for( Function(funcname, _, outtype, outname) <- functions ) yield{
		new OutConnector(outname, funcname, outtype, thisnode)
	}
	
	val inconnectorpanel = new BoxPanel(Vertical) {
		contents ++= inconnectors
	}

	val outconnectorpanel = new BoxPanel(Vertical) {
		contents ++= outconnectors
	}

	override val sliders = 
	for( slider <- sliderdefinitions ) yield {
		slider match {
			case name:String => new FunctionSlider(name, nodeid = id)
			case (name:String, formula:String) =>
				val compilation = InterpreterManager[Float => Float]("(s:Float) => " + formula)
				new FunctionSlider(name, nodeid = id) {
					compilation() match {
						case Some(f) =>
							transform = f
							tformula = formula
						case None =>
					}
					tooltip = globalvalue.toString
				}
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
		case ValueChanged(slider:FunctionSlider) =>
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

class PredefinedNode(title:String, id:Int, nodetype:FunctionNodeType) extends Node(title, id) with NodeInit {
	override def functions = nodetype.functions
	override def intypes = nodetype.intypes
	override def sliderdefinitions = nodetype.sliders

	contents +=	new BoxPanel(Horizontal){
		contents += inconnectorpanel
		contents += sliderpanel
		contents += outconnectorpanel
	}
	contents += removebutton
}

class CustomNode(title:String, id:Int, slidernames:Seq[String], override val intypes:Seq[String]) extends Node(title, id) with NodeInit with Resizable {
	//TODO compilefehler im Node anzeigen
	println("CustomNode Constructor")
	override def sliderdefinitions = slidernames
	override def functions = Seq(Function(
		name = "custom_f" + id,
		code = if(funcfield != null) funcfield.text else "",
		outtype = "Float"
	))

	val funcfield = new TextArea("0f") {
		font = new Font("Monospaced", java.awt.Font.PLAIN, 11)
		tabSize = 4
		//lineWrap = true
		editable = true
	}
	
	val compilebutton = new Button("compile")

	listenTo(funcfield, compilebutton)
	reactions += {
		case ButtonClicked(`compilebutton`) =>
			publish(NodeChanged(source = thisnode, node = thisnode))
		case UIElementResized(`funcfield`) =>
			//TODO: resize node
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

