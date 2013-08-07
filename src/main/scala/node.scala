package noiseeditor

import noiseeditor.event._
import noiseeditor.connector._
import noiseeditor.manager._

import noiseeditor.config._
import noiseeditor.swingextension._
import noiseeditor.datastructure._

import swing._
import swing.event._
import javax.swing.border._
import javax.swing.border.BevelBorder._
import Orientation._

import simplex3d.math._
import simplex3d.math.double.functions._


//TODO: comments and help in Nodes
//TODO: feature to replace nodes

object Node {
	var id = 0
	def nextid = {
		id += 1
		id
	}
	
	def reset() {
		id = 0
	}
	
	def getIdMapping(ids:Seq[Int]) = {
		(ids map (_ -> nextid)).toMap
	}

	def apply(nodetype:NodeType, id:Int = nextid) = {
		new PredefinedNode(nodetype.title, id, nodetype)
	}
	
	def custom(id:Int = nextid) = {
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

	def loadcustom( title:String, sliders:Seq[NodeSlider], arguments:LanguageMap[Seq[NodeArgument]], code:String, id:Int = nextid ) = {
		val node = new CustomNode(title, id, arguments, sliders)
		node.funcfield.text = code
    node.refreshFunctions()
		node
	}
	
	def preview(title:String = "Preview", id:Int = nextid) = {
		new Preview(title, id)
	}
}


abstract class Node(var title:String, val id:Int = Node.nextid) extends BoxPanel(Vertical) with Movable {
  var arguments:LanguageMap[Seq[NodeArgument]] = LanguageMap()
  var sliderdefinitions:Seq[NodeSlider] = Nil
	var functions:LanguageMap[Map[String, NodeFunctionFull]] = LanguageMap()

	var sliders:Seq[FormulaSlider] = Nil
	var inconnectors:Seq[InConnector] = Nil
	var outconnectors:Seq[OutConnector] = Nil

	def thisnode = this // For accessing the node instance in inner classes
  override def toString() = "Node("+title+")"

  var inConnectorPanel:Panel = null
  var outConnectorPanel:Panel = null
  var sliderPanel:Panel = null
  var removeButton:Button = null
  var renameButton:Button = null

  val titledborder = new TitledBorder(new SoftBevelBorder(RAISED), title)
  def postinit() {
    border = titledborder
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

    //Draw the complete border with title. Even if the content is smaller
    preferredSize = max(titledborder.getMinimumSize(thisnode.peer), preferredSize)
    peer.setSize(preferredSize)
  }

  def createConnectors() {
    inconnectors = for( NodeArgument(argname,argtype,argdefault) <- arguments("scala") ) yield{
      new InConnector(argname, argtype, argdefault, thisnode)
    }

    outconnectors = (for( (title, function) <- functions.mapmaptranspose ) yield {
      new OutConnector(title, function, thisnode)
    }).toSeq

    // listen to all connectors and publish events for this node
    for( connector <- inconnectors ++ outconnectors )
      listenTo(connector)

    reactions += {
      case HitConnector(source:Connector, connector, clicks) =>
        publish(HitConnector(source=this, connector, clicks))
    }
  }

  def refreshConnectors() {
    for( ((title, function),connector) <- functions.mapmaptranspose zip outconnectors ) yield {
      assert(connector.title == title)
      connector.function = function
    }
  }

  def layout() {
    createConnectors()

    inConnectorPanel = new BoxPanel(Vertical) {
      contents ++= inconnectors
    }

    outConnectorPanel = new BoxPanel(Vertical) {
      contents ++= outconnectors
    }

    sliders =
    for( NodeSlider(name, sformula, initvalue,  _) <- sliderdefinitions ) yield {
      val compilation = InterpreterManager[Double => Double]("(s:Double) => {" + sformula + "}")
      new FormulaSlider(name, nodeid = id, initvalue) {
        // TODO: compile sliders in background for faster Composition loading
        //				future {
        compilation() match {
          case Some(f) =>
            transform = f
            formula = sformula
          case None =>
        }
        tooltip = globalvalue.toString
        //					publish(new ValueChanged(this))
        //				}
      }
    }

    sliderPanel = new GridBagPanel {
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

    removeButton = new RemoveButton {
      reactions += {
        case e:ButtonClicked =>
          NodeManager.remove(thisnode)
      }
    }

    renameButton = new Button("rename") {
      //TODO: not working correctly in CustomNodes
      margin = new Insets(0,0,0,0)
      reactions += {
        case e:ButtonClicked =>
          Dialog.showInput(
            parent = NoiseEditor.window.contents.head,
            message = "Enter new name: ",
            title = "Rename Node",
            messageType = Dialog.Message.Plain,
            icon = null,
            entries = Seq(),
            initial = titledborder.getTitle
          ) match {
            case Some(newname) =>
              thisnode.title = newname
              titledborder.setTitle(thisnode.title)
              thisnode.peer.setSize(max(titledborder.getMinimumSize(thisnode.peer), thisnode.preferredSize))
              thisnode.revalidate()
              thisnode.repaint()
            case None =>
          }
      }
    }
  }
}

class PredefinedNode(title:String, id:Int, nodetype:NodeType) extends Node(title, id) {
  functions = nodetype.functions
	arguments = nodetype.arguments
	sliderdefinitions = nodetype.sliders

  layout()

  contents +=	new BoxPanel(Horizontal){
		contents += inConnectorPanel
		contents += sliderPanel
		contents += outConnectorPanel
	}
	contents += new BoxPanel(Horizontal) {
		contents += renameButton
		contents += removeButton
	}

  postinit()
}
object CustomNode {
  val codePrefix = "try{{"
  val codeSuffix = "}.toDouble}catch{ case _:Throwable ⇒ 0.0}"
}
class CustomNode(title:String, id:Int, arguments:LanguageMap[Seq[NodeArgument]], customsliders:Seq[NodeSlider]) extends Node(title, id) with Resizable {
	//TODO show compile errors in GUI

  thisnode.arguments = arguments
	sliderdefinitions = customsliders
  def refreshFunctions() {
    functions = LanguageMap("scala" -> Map("o" -> customfunction))
  }

	def customfunction = NodeFunctionFull(
		name = "custom_f" + id,
		returntype = "Double",
		code = if(funcfield != null) (CustomNode.codePrefix + funcfield.text + CustomNode.codeSuffix) else "0.0",
		arguments = arguments("scala"),
		customsliders
	)

	val funcfield = new TextArea("0.0") {
		font = new Font("Monospaced", java.awt.Font.PLAIN, 11)
		tabSize = 4
		//lineWrap = true
		editable = true
	}
	
	val compilebutton = new Button("compile") {
		margin = new Insets(0,0,0,0)
		reactions += {
			case e:ButtonClicked => compile()
		}
	}

	def compile() {
		outconnectors(0).function = LanguageMap("scala" -> customfunction)
		publish(NodeChanged(source = thisnode, node = thisnode))
	}

  refreshFunctions()
  layout()

  val controlpanel = new BoxPanel(Horizontal) {
			contents += inConnectorPanel
			contents += sliderPanel
			contents += outConnectorPanel
			maximumSize = preferredSize
	}

	contents +=	controlpanel
	contents += new ScrollPane(funcfield)
	contents += new BoxPanel(Horizontal){
		contents += compilebutton
		contents += renameButton
		contents += removeButton
	}

  postinit()
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
