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
import swing.ListView._

import java.awt.Graphics2D
import java.awt.Color.BLACK
import java.awt.image.BufferedImage

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import actors.Futures.future

case class Material(id:Int = -1, color:Int = 0xFF00FF, texture:Option[String] = None) {
  def r = color >> 16
  def g = (color & 0x00FF00) >> 8
  def b = color & 0xFF
  def rgb  = Vec3(r/255.0,g/255.0,b/255.0)
}
object Material {
  def apply(id:Int, r:Int, g:Int, b:Int):Material = new Material(id, color=clamp(r,0,0xFF) << 16 | clamp(g,0,0xFF) << 8 | clamp(b,0,0xFF) )
  def apply(r:Int, g:Int, b:Int):Material = new Material(color=clamp(r,0,0xFF) << 16 | clamp(g,0,0xFF) << 8 | clamp(b,0,0xFF) )
  def apply(id:Int, r:Double, g:Double, b:Double):Material = Material(id, (r*255).toInt, (g*255).toInt, (b*255).toInt)
  def apply(r:Double, g:Double, b:Double):Material = Material((r*255).toInt, (g*255).toInt, (b*255).toInt)
}

class Preview(title:String, id:Int) extends Node(title, id) with Resizable {
	def thispreview = this
	functions = LanguageMap(
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
	
	arguments = LanguageMap("scala" -> functions("scala")("result").arguments)

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
	
	override def resized(delta:Vec2i) {
		image.recalc()
		image.offset -= delta / 2.0 * image.zoom
	}
	
	val viewtypes = Seq(
		'iso -> "Iso surface",
		'isowithdepth -> "Iso surface depth",
		'values -> "Values",
		'valueswithiso -> "Values and Iso",
		'valuesnormalized -> "Values normalized"
		)
	
	val perspectives = Seq(
		('sideview_zup, ((v:Vec3) => Vec3(v.x,v.z,-v.y)), "Side view (z up)"),
		('topview_zup, ((v:Vec3) => Vec3(v.x,v.y,-v.z)), "Top view (z up)"),
		('sideview_yup, ((v:Vec3) => Vec3(v.x,-v.y,v.z)), "Side view (y up)"),
		('topview_yup, ((v:Vec3) => Vec3(v.x,v.z,-v.y)), "Top view (y up)")
		)
		
	val timer = new Timer
	
	override def paint(g:Graphics2D) {
		image.repaint()
		super.paint(g)
	}
	
	val image = new PreviewImage
	class PreviewImage extends Component with Zoomable {
		preferredSize = Vec2i(250, 250)
		peer.setSize(preferredSize)
		background = BLACK
		
		var bufferedimage:BufferedImage = null
		var needsrecalc = true
		def recalc() {needsrecalc = true; repaint()}

		var z = 0.0
		
		reset()
		
		
		
		override def scrolledorzoomed() {
			depthslider.value = (100*defaultZoom*z/(32*zoom))+50
			recalc()
		}
		
		def reset() {
			zoom = defaultZoom
			offset = -size / 2 * zoom
			if( depthslider != null ) depthslider.value = 50
			recalc()
		}
		
		def transformcoords(x:Double, y:Double, z:Double) =
			perspective(Vec3(transformZoomOffset(Vec2(x,y)),z) )
		
		def valueat(x:Double, y:Double, z:Double) = interpretedcomposition(transformcoords(x,y,z))
		
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
				timer.reset()
				timer.start()

				val data:Array[Int] =
				viewcombobox.selected match {
					case 'iso =>
						(0 until width*height).par.map{i =>
							val x = i%width
							val y = i/width
							val result = valueat(x,y,z)
							if( result._1 >= 0 )
								result._2.color
							else
								0xFFFFFF
						}.toArray

					case 'isowithdepth =>
						(0 until width*height).par.map{i =>
							val x = i%width
							val y =i/width
							var tmpz = z

							var counter = 0
					
							var result = valueat(x,y,z)
							while( result._1 < 0 && counter < depthMaxsteps ){
								tmpz += depthStepSize*zoom
								counter += 1
								result = valueat(x,y,tmpz)
							}
							
							val color = result._2.color
							counter = if(counter == 0) 0 else counter + 2
							val factor = math.pow(depthFadeOutFactor,counter)
					
							val nr = 255 - ((255-red(color)) * factor).toInt
							val ng = 255 - ((255-green(color)) * factor).toInt
							val nb = 255 - ((255-blue(color)) * factor).toInt
					
							rgbcolor(nr,ng,nb)
						}.toArray

					case 'values =>
						(0 until width*height).par.map{i =>
							val x = i%width
							val y =i/width
							val result = valueat(x,y,z)
							val value = (clamp( (result._1+1)*0.5, 0, 1 )*255).toInt
							graycolor(value)
						}.toArray

					case 'valueswithiso =>
						(0 until width*height).par.map{i =>
							val x = i%width
							val y =i/width
/*							val translated = transformcoords(x,y,z)

							if( abs(translated.x - round(translated.x)) < zoom*0.5
							 && abs(translated.y - round(translated.y)) < zoom*0.5
							 && abs(translated.z - round(translated.z)) < zoom*0.5 )
								GridColor
							else {*/
								val result = valueat(x,y,z)
								val value = (clamp( (result._1+1)*0.5, 0, 1 )*255).toInt
								if( result._1 > 0 )
									mixcolors(isolineColor, graycolor(value), 0.3)
								else
									graycolor(value)
//							}
						}.toArray

					case 'valuesnormalized =>
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
								isolineColor
							else
								graycolor(value)
						}.toArray
				}
				timer.stop()

				speedlabel.value = math.max((width*height)/timer.read, speedlabel.value)
				speedlabel.text = "%s px/s max".format(thousandsseparator((speedlabel.value).toLong.toString))	
				bufferedimage.setRGB(0, 0, width, height, data, 0, width)
				
				
				if( gridcheckbox.selected ) // Draw grid
				{
					val ig = bufferedimage.createGraphics
					import ig._
					
					def drawGrid(delta:Double, colorindex:Int) {
						val alpha = remap( delta, minGridSize, gridDistance*minGridSize, 0, 255).toInt
						setColor(gridColors( mod(colorindex,gridColors.size).toInt ).setAlpha(alpha))
            var x = mod(-offset.x / zoom, delta)
						while( x < width ) {
							drawLine( x.toInt, 0, x.toInt, height)
              x += delta
						}
						var y = mod(-offset.y / zoom, delta)
						while( y < height ) {
							drawLine( 0, y.toInt, width, y.toInt)
              y += delta
						}
					}
					
					// Choose Grid distances and colors depending on the zoom level
					// http://www3.wolframalpha.com/input/?i=plot+3%5Eceil%28log%285%2Fx%29%2Flog%283%29%29%2C+x*3%5Eceil%28log%285%2Fx%29%2Flog%283%29%29%2C+ceil%28log%285%2Fx%29%2Flog%283%29%29++from+x+%3D+0..5
					val delta = 1/zoom * pow(gridDistance,ceil(log(minGridSize * zoom)/log(gridDistance)))
					val colorindex = ceil(log(minGridSize*zoom)/log(gridDistance)).toInt
					
					drawGrid(delta, colorindex)
					drawGrid(delta*gridDistance, colorindex + 1)
				}
				else // Draw Unit Square
				{
					val ig = bufferedimage.createGraphics
					ig.setColor(gridColors(0))
          ig.drawRect(10,10,(1/zoom).toInt,(1/zoom).toInt)
				}
				
				needsrecalc = false
			}
			
			super.paintComponent(g) // Paint background
			drawImage(bufferedimage, null, 0, 0)

			if( continouscheckbox.selected ) recalc()
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
    var floatvalue = 50.0
		val slider = new Slider with ScrollableSlider {
			reactions += {
				case e:ValueChanged =>
					if( floatvalue.toInt != value )
						floatvalue = value.toDouble
					image.z = (floatvalue-50)/100/defaultZoom*32*image.zoom
					image.recalc()
			}
		}
		contents += new Label("depth:")
		contents += slider
		def value = floatvalue
		def value_=(x:Double) {
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
				image.reset()
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
	
	val gridcheckbox = new CheckBox("grid") {
		margin = new Insets(0,0,0,0)
		reactions += {
			case e:ButtonClicked =>
				image.recalc()
		}
	}
	val continouscheckbox = new CheckBox("continous") {
		margin = new Insets(0,0,0,0)
		reactions += {
			case e:ButtonClicked =>
				image.recalc()
		}
	}

  layout()

  contents += new BoxPanel(Horizontal) {
		contents += inConnectorPanel
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
				contents += renameButton
				contents += removeButton
			}
		}
	}

  postinit()

	def recompile() {
		println("Preview("+id+"): starting compiler in background...")
		future {
			val code = CompositionManager.generatepreviewcode(outconnectors.head)
			val compilation = InterpreterManager[Compositiontype](code) // Future
			compilation() match {
				// Compilation successful
				case Some(interpretedcode) =>
					interpretedcomposition = interpretedcode
					involvedsliders = CompositionManager.involvedsliders(outconnectors.head)
					speedlabel.value = 0.0
					image.recalc()
				// Compilation not successful
				case None =>
					interpretedcomposition = (world:Vec3) => (0.0, Material(color=0xFF0000))
					image.recalc()
			}
		}
	}
	
	listenTo(ConnectionManager, NodeManager, viewcombobox.selection, perspective.selection)
	reactions += {
		case e:NodeValueChanged =>
			if( involvedsliders contains e.slider )
				image.recalc()
		
		case e:NodeConnected =>
		//TODO: only recompile, if own composition is affected
			recompile()
		
		case e:NodeChanged =>
			recompile()

		case SelectionChanged(`viewcombobox`) =>
			speedlabel.value = 0.0
			image.recalc()

		case SelectionChanged(`perspective`) =>
			speedlabel.value = 0.0
			image.recalc()
	}	
}
