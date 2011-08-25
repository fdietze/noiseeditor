package noiseeditor

import noiseeditor.manager._
import noiseeditor.event._
import util._
import config._
import datastructure._
import swingextension._

import swing._
import swing.event._
import Orientation._
import javax.swing.border._
import javax.swing.border.BevelBorder._
import swing.ListView._

import java.awt.Graphics2D
import java.awt.Color.BLACK
import java.awt.image.BufferedImage

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import actors.Futures.future

case class Material(color:Int = MaterialDefaultColor)

class Preview(title:String, id:Int) extends Node(title, id) with NodeInit with Resizable {
	def thispreview = this
	override def functions = LanguageMap(
		"scala" -> Map(
			"result" -> NodeFunctionFull("result", "(Double, Material)", "(d,m)",
				Seq(
					NodeArgument("d","Double", "0.0"),
					NodeArgument("m","Material", "Material(0x000000)")
				),
				Nil
			)
		)
	)
	
	override def arguments = LanguageMap("scala" -> functions("scala")("result").arguments)
	
	type Compositiontype = (Vec3) => (Double, Material)
	var interpretedcomposition:Compositiontype = (world:Vec3) => (0.0, Material())
	var involvedsliders = Set[String]()
	
	def connectedoutconnector(name:String) = {
		import ConnectionManager.connections
		
		inconnectors.find( _.title == name ) match {
			case Some(inconnector) => connections(inconnector)
			case None => None
		}
	}
	
	minimumSize = Vec2i(100,100)
	
	override def resized(delta:Vec2i) = {
		image.recalc
		image.offset -= delta / 2.0 * image.zoom
	}
	
	val viewtypes = Seq(
		"iso" -> "Iso surface",
		"isowithdepth" -> "Iso surface depth",
		"values" -> "Values",
		"valueswithiso" -> "Values and Iso",
		"valuesnormalized" -> "Values normalized"
		)
	
	val perspectives = Seq(
		("sideview_zup", ((v:Vec3) => Vec3(v.x,v.z,-v.y)), "Side view (z up)"),
		("topview_zup", ((v:Vec3) => Vec3(v.x,v.y,-v.z)), "Top view (z up)"),
		("sideview_yup", ((v:Vec3) => Vec3(v.x,-v.y,v.z)), "Side view (y up)"),
		("topview_yup", ((v:Vec3) => Vec3(v.x,v.z,-v.y)), "Top view (y up)")
		)
		
	val timer = new Timer
	
	override def paint(g:Graphics2D) {
		image.repaint
		super.paint(g)
	}
	
	val image = new PreviewImage
	class PreviewImage extends Component with ScrollableZoomOffset {
		preferredSize = Vec2i(250, 250)
		peer.setSize(preferredSize)
		background = BLACK
		
		var bufferedimage:BufferedImage = null
		var needsrecalc = true
		def recalc {needsrecalc = true; repaint}

		var z = 0.0
		
		reset
		
		
		
		override def scrolledorzoomed = {
			depthslider.value = (100*GridIndicatorScale*z/(32*zoom))+50
			recalc
		}
		
		def reset {
			zoom = GridIndicatorScale
			offset = -size / 2 * zoom
			if( depthslider != null ) depthslider.value = 50
			recalc
		}
		
		def transformcoords(x:Double, y:Double, z:Double) =
			perspective(Vec3(transformZoomOffset(Vec2(x,y)),z) )
		def valueat(x:Double, y:Double, z:Double) =
			interpretedcomposition(transformcoords(x,y,z))
		
		override def paint(g:Graphics2D) {
			import g._
			val width = size.width
			val height = size.height

			if( bufferedimage == null
				|| width != bufferedimage.getWidth
				|| height != bufferedimage.getHeight ) {
				bufferedimage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
			}

			// TODO: limit framerate
/*			val maxframerate = 10.0
			val minframepause = 1000.0 / maxframerate
			val lastcalc = (timer.getTime - timer.starttime)/1000000.0
			Thread.sleep(max(0,minframepause - lastcalc).toInt)*/
			
			if( needsrecalc ) {
				timer.reset
				timer.start

				val data:Array[Int] =
				viewcombobox.selected match {
					case "iso" =>
						(0 until width*height).par.map{i =>
							val x = i%width
							val y = i/width
							val result = valueat(x,y,z)
							if( result._1 >= 0 )
								result._2.color
							else
								0xFFFFFF
						}.toArray

					case "isowithdepth" =>
						(0 until width*height).par.map{i =>
							val x = i%width
							val y =i/width
							var tmpz = z

							var counter = 0
					
							var result = valueat(x,y,z)
							while( result._1 < 0 && counter < DepthMaxsteps ){
								tmpz += DepthStepSize*zoom
								counter += 1
								result = valueat(x,y,tmpz)
							}
							
							val color = result._2.color
							counter = if(counter == 0) 0 else counter + 2
							val factor = math.pow(DepthFadeOutFactor,counter)
					
							val nr = 255 - ((255-red(color)) * factor).toInt
							val ng = 255 - ((255-green(color)) * factor).toInt
							val nb = 255 - ((255-blue(color)) * factor).toInt
					
							rgbcolor(nr,ng,nb)
						}.toArray

					case "values" =>
						(0 until width*height).par.map{i =>
							val x = i%width
							val y =i/width
							val result = valueat(x,y,z)
							val value = (clamp( (result._1+1)*0.5, 0, 1 )*255).toInt
							graycolor(value)
						}.toArray

					case "valueswithiso" =>
						(0 until width*height).par.map{i =>
							val x = i%width
							val y =i/width
							val translated = transformcoords(x,y,z)

/*							if( abs(translated.x - round(translated.x)) < zoom*0.5
							 && abs(translated.y - round(translated.y)) < zoom*0.5
							 && abs(translated.z - round(translated.z)) < zoom*0.5 )
								GridColor
							else {*/
								val result = valueat(x,y,z)
								val value = (clamp( (result._1+1)*0.5, 0, 1 )*255).toInt
								if( result._1 > 0 )
									mixcolors(IsolineColor, graycolor(value), 0.3)
								else
									graycolor(value)
//							}
						}.toArray

					case "valuesnormalized" =>
						var minvalue = scala.Double.MaxValue
						var maxvalue = scala.Double.MinValue
						(0 until width*height).par.map{i =>
							val x = i%width
							val y = i/width
							val result = valueat(x,y,z)._1
							if( result < minvalue ) minvalue = result
							if( result > maxvalue ) maxvalue = result
							result
						}.map{ v =>
							val value = (((v - minvalue) / (maxvalue-minvalue))*255).toInt
							val isovalue = (((0 - minvalue) / (maxvalue-minvalue))*255).toInt
							if( abs(value-isovalue) < 3 )
								IsolineColor
							else
								graycolor(value)
						}.toArray
				}
				timer.stop
				def thousandspoints(s:String) = {
					var i = 0
					("" /: s.reverse)( (x,y) => {i+=1;x + (if(i%3==1) "." else "") +y}).drop(1).reverse
				}

				speedlabel.value = math.max((width*height)/timer.read, speedlabel.value)
				speedlabel.text = "%s px/s max".format(thousandspoints((speedlabel.value).toLong.toString))	
				bufferedimage.setRGB(0, 0, width, height, data, 0, width)
				
				if( gridcheckbox.selected )
				{
					val ig = bufferedimage.createGraphics
					ig.setColor(GridIndicatorColor);
					// TODO: Draw Grid
				}
				else
				{
					// Grid indicator
					val ig = bufferedimage.createGraphics
					ig.setColor(GridIndicatorColor);
					//TODO: Calculate Color to be always visible?
					ig.drawRect(10,10,(1/zoom).toInt,(1/zoom).toInt)
				}
				
				needsrecalc = false
			}
			
			super.paintComponent(g) // Paint background
			drawImage(bufferedimage, null, 0, 0)

			if( continouscheckbox.selected ) recalc
		}
		
		listenTo(mouse.moves, mouse.clicks)
		reactions += {
			case e:MouseMoved =>
				tooltip = transformcoords(e.point.x,e.point.y,z) + " => " +
					valueat(e.point.x,e.point.y,z)
			case e:MouseDragged =>
				tooltip = transformcoords(e.point.x,e.point.y,z) + " => " +
					valueat(e.point.x,e.point.y,z)
		}
	}
	
	val depthslider = new BoxPanel(Horizontal) {
		val slider = new Slider with ScrollableSlider {
			reactions += {
				case e:ValueChanged =>
					if( floatvalue.toInt != value )
						floatvalue = value.toDouble
					image.z = (floatvalue-50)/100/GridIndicatorScale*32*image.zoom
					image.recalc
			}
		}
		contents += new Label("depth:")
		contents += slider
		var floatvalue = 50.0
		def value = floatvalue
		def value_=(x:Double) = {
			floatvalue = max(0,min(x,100))
			slider.value = floatvalue.toInt
		}
	}
	
	val viewcombobox = new ComboBox(viewtypes) {
		renderer = Renderer(_._2)
		maximumSize = preferredSize
		def selected = selection.item._1
		def select(item:String) {
			val index = viewtypes.map(_._1).indexOf(item)
			selection.index = if( index == -1 ) selection.index else index

		}
	}

	val perspective = new ComboBox(perspectives) {
		renderer = Renderer(_._3)
		maximumSize = preferredSize
		def apply(v:Vec3) = selection.item._2(v)
		def selectedname = selection.item._1
		def select(item:String) {
			val index = perspectives.map(_._1).indexOf(item)
			selection.index = if( index == -1 ) selection.index else index

		}
	}

	val resetbutton = new Button("reset") {
		margin = new Insets(0,0,0,0)
		reactions += {
			case e:ButtonClicked =>
				image.reset
		}
	}

	val exportcontrols = new BoxPanel(Horizontal) {
		val exportcombobox = new ComboBox(ModuleManager.exporttypes) {
			maximumSize = preferredSize
			def selected = selection.item
		}
		contents += exportcombobox
		contents += new Button("export") {
			margin = new Insets(0,0,0,0)
			reactions += {
				case e:ButtonClicked =>
					ModuleManager.export(thispreview, exportcombobox.selected)
			}
		}
	}



	
	val speedlabel = new Label(""){
		var value:Double = 0.0
	}
	
	//TODO: save grid and continous in file
	val gridcheckbox = new CheckBox("grid") {
		margin = new Insets(0,0,0,0)
		reactions += {
			case e:ButtonClicked =>
				image.recalc
		}
	}
	val continouscheckbox = new CheckBox("continous") {
		margin = new Insets(0,0,0,0)
		reactions += {
			case e:ButtonClicked =>
				image.recalc
		}
	}
	
	contents += new BoxPanel(Horizontal) {
		contents += inconnectorpanel
		contents += new BoxPanel(Vertical) {
			contents += image
			contents += speedlabel
			contents += depthslider
			contents += new BoxPanel(Horizontal) {
				contents += viewcombobox
				contents += perspective
				contents += resetbutton
			}
			contents += new BoxPanel(Horizontal) {
				contents += gridcheckbox
				contents += continouscheckbox
				contents += exportcontrols
			}
			contents += new BoxPanel(Horizontal) {
				contents += renamebutton
				contents += removebutton
			}
		}
	}

	def recompile {
		println("Preview("+id+"): starting compiler in background...")
		future {
			val code = CompositionManager.generatepreviewcode(outconnectors.head)
			val compilation = InterpreterManager[Compositiontype](code) // Future
			compilation() match {
				case Some(interpretedcode) =>
					interpretedcomposition = interpretedcode
					involvedsliders = CompositionManager.involvedsliders(outconnectors.head)
					speedlabel.value = 0.0
					image.recalc
				case None =>
			}
		}
	}
	
	listenTo(ConnectionManager, NodeManager, viewcombobox.selection, perspective.selection)
	reactions += {
		case NodeValueChanged(source, node, slider, value) =>
			if( involvedsliders contains slider )
				image.recalc
		
		case e:NodeConnected =>
		//TODO: only recompile, if own composition is affected
			recompile
		
		case e:NodeChanged =>
			recompile

		case SelectionChanged(`viewcombobox`) =>
			speedlabel.value = 0.0
			image.recalc

		case SelectionChanged(`perspective`) =>
			speedlabel.value = 0.0
			image.recalc
	}	
}
