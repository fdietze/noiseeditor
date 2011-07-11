package noiseeditor

import swing._
import event._
import Orientation._
import javax.swing.border._
import javax.swing.border.BevelBorder._
import utilities._
import config._
import swing.ListView._

import java.awt.Graphics2D
import java.awt.Color._
import java.awt.image.BufferedImage

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import actors.Futures.future

//TODO: png export
//TODO: shader export

class Preview(id:Int) extends Node("Preview", id) with NodeInit with Resizable {

	override def resized = image.recalc
	
	
	override def intypes = Seq("d:Float", "m:Material")
	
	val densityconnector = inconnectors(0)
	val materialconnector = inconnectors(1)
	
	val composition = new Composition
	
	val viewtypes = Seq(
		"iso" -> "Iso surface",
		"isodepth" -> "Iso surface depth",
		"valuesclamped" -> "Values clamped",
		"valuesclampedgrid" -> "Values clamped +Grid",
		"valuesstretched" -> "Values stretched",
		"valuesnormalized" -> "Values normalized")
	
	val timer = new Timer
	
	val image = new PreviewImage
	class PreviewImage extends Component with ScrollableZoomOffset {
		preferredSize = Vec2i(250, 250)
		background = BLACK
		
		var bufferedimage:BufferedImage = null
		var needsrecalc = true
		def recalc {needsrecalc = true; repaint}

		var z = 0f

		override def scrolledorzoomed = {
			zslider.value = (100f*GridIndicatorScale*z/(32f*zoom))+50f
			recalc
		}
		zoom = GridIndicatorScale

		override def paint(g:Graphics2D) {
			import g._
			val width = size.width
			val height = size.height

			if( bufferedimage == null
				|| width != bufferedimage.getWidth
				|| height != bufferedimage.getHeight ) {
				bufferedimage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
			}
			
			if( needsrecalc ) {
				//TODO: Ursprung links unten, koordinaten swizzeling
				//TODO: Tooltip-raytrace
				
				timer.reset
				timer.start

				val data:Array[Int] =
				viewcombobox.selection.item._1 match {
					case "iso" =>
						(0 until width*height).par.map{i =>
							val x = i%width
							val y = i/width
							val result = composition( Vec3( transformZoomOffset(Vec2(x,y)) ,z) )
							if( result._1 >= 0 )
								result._2.color
							else
								0xFFFFFF
						}.toArray

					case "isodepth" =>
						(0 until width*height).par.map{i =>
							val x = i%width
							val y = i/width
							var tmpz = z

							var counter = 0
					
							var result = composition( Vec3( transformZoomOffset(Vec2(x,y)) ,tmpz) )
							while( result._1 < 0 && counter < DepthMaxsteps ){
								tmpz += DepthStepSize*zoom
								counter += 1
								result = composition( Vec3( transformZoomOffset(Vec2(x,y)) ,tmpz) )
							} 
					
							counter = if(counter == 0) 0 else counter + 2
							val factor = math.pow(DepthFadeOutFactor,counter)
					
							val color = result._2.color
							val r = (color & 0xff0000) >> 16
							val g = (color & 0x00ff00) >> 8
							val b =  color & 0x0000ff
						
							val nr = 255 - ((255-r) * factor).toInt
							val ng = 255 - ((255-g) * factor).toInt
							val nb = 255 - ((255-b) * factor).toInt
					
							(nr << 16 | ng << 8 | nb)
						}.toArray

					case "valuesclamped" =>
						(0 until width*height).par.map{i =>
							val x = i%width
							val y = i/width
							val result = composition( Vec3( transformZoomOffset(Vec2(x,y)) ,z) )
							val value = (clamp( (result._1+1f)*0.5f, 0f, 1f )*255f).toInt
							graycolor(value)
						}.toArray

					case "valuesclampedgrid" =>
						(0 until width*height).par.map{i =>
							val x = i%width
							val y = i/width
							val translated = transformZoomOffset(Vec2(x,y))

							if( abs(translated.x - round(translated.x)) < zoom*0.5f
							 || abs(translated.y - round(translated.y)) < zoom*0.5f )
								GridColor
							else {
								val result = composition( Vec3( translated ,z) )
								val value = (clamp( (result._1+1f)*0.5f, 0f, 1f )*255f).toInt
								if( result._1 > 0 )
									mixcolors(IsolineColor, graycolor(value), 0.3f)
								else
									graycolor(value)
							}
						}.toArray

					case "valuesstretched" =>
						def stretch(x:Float) = atan(x*0.33f)/math.Pi+0.5
						
						(0 until width*height).par.map{i =>
							val x = i%width
							val y = i/width
							val result = composition( Vec3( transformZoomOffset(Vec2(x,y)) ,z) )
							val value = (stretch( result._1)*255f).toInt
							graycolor(value)
						}.toArray

					case "valuesnormalized" =>
						var minvalue = scala.Float.MaxValue
						var maxvalue = scala.Float.MinValue
						(0 until width*height).par.map{i =>
							val x = i%width
							val y = i/width
							val result = composition( Vec3( transformZoomOffset(Vec2(x,y)) ,z) )._1
							if( result < minvalue ) minvalue = result
							if( result > maxvalue ) maxvalue = result
							result
						}.map{ v =>
							val value = (((v - minvalue) / (maxvalue-minvalue))*255f).toInt
							val isovalue = (((0f - minvalue) / (maxvalue-minvalue))*255f).toInt
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
				speedlabel.text = "%s px/s".format(thousandspoints(((width*height)/timer.read).toLong.toString))	
				bufferedimage.setRGB(0, 0, width, height, data, 0, width)
				
				// Grid indicator
				val ig = bufferedimage.createGraphics
				ig.setColor(GridIndicatorColor);
				ig.drawRect(10,10,(1f/zoom).toInt,(1f/zoom).toInt)
				
				needsrecalc = false
			}
			
			super.paintComponent(g) // Paint background
			drawImage(bufferedimage, null, 0, 0)
		}
		
		listenTo(mouse.moves, mouse.clicks)
		reactions += {
			case e:MouseMoved =>
				tooltip = transformZoomOffset(Vec3(e.point,z)) + " => " +
					composition( transformZoomOffset(Vec3(e.point,z)) )
			case e:MouseDragged =>
				tooltip = transformZoomOffset(Vec3(e.point,z)) + " => " +
					composition( transformZoomOffset(Vec3(e.point,z)) )
		}
	}
	
	val zslider = new BoxPanel(Horizontal) {
		val slider = new Slider {
			reactions += {
				case e:ValueChanged =>
					if( floatvalue.toInt != value )
						floatvalue = value.toFloat
					image.z = (floatvalue-50)/100f/GridIndicatorScale*32f*image.zoom
					image.recalc
			}
		}
		contents += new Label("z:")
		contents += slider
		var floatvalue = 50f
		def value = floatvalue
		def value_=(x:Float) = {
			floatvalue = max(0f,min(x,100f))
			slider.value = floatvalue.toInt
		}
	}
	
	val viewcombobox = new ComboBox(viewtypes) {
		renderer = Renderer(_._2)
		maximumSize = preferredSize
	}

	val resetbutton = new Button("reset") {
		margin = new Insets(1,1,1,1)
		reactions += {
			case e:ButtonClicked =>
				image.zoom = GridIndicatorScale
				image.offset = Vec2(0)
				zslider.value = 50
				image.recalc
		}
	}

	val exportbutton = new Button("export") {
		margin = new Insets(1,1,1,1)
		reactions += {
			case e:ButtonClicked =>
				import FileChooser.Result._
				import FileManager.chooser
				val oldselectedfile = chooser.selectedFile
				
				chooser.title = "Export generated code"
				chooser.setExtensionFilter("Scala function", "scala")
				chooser.selectedFile = null
				chooser.showSaveDialog match {
					case Approve =>
						val out = new java.io.FileWriter(chooser.selectedFile)
						out.write(composition.generate(densityconnector, materialconnector, constantsliders = true))
						out.close
					case Cancel =>
				}
				chooser.selectedFile = oldselectedfile
		}
	}
	
	val speedlabel = new Label("")

	contents += new BoxPanel(Horizontal) {
		contents += inconnectorpanel
		contents += new BoxPanel(Vertical) {
			contents += image
			contents += speedlabel
			contents += zslider
			contents += new BoxPanel(Horizontal) {
				contents += viewcombobox
				contents += resetbutton
				contents += exportbutton
				contents += removebutton
			}
		}
	}
	
	def recompile {
		println("Preview("+id+"): starting compiler in background...")
		future{
			composition.generate(densityconnector, materialconnector)
			composition.compile
			image.recalc
		}
	}
	
	listenTo(ConnectionManager, NodeManager, viewcombobox.selection)
	reactions += {
		case NodeValueChanged(source, node, slider, value) =>
			if( composition.involvedsliders contains slider )
				image.recalc
		
		case e:NodeConnected =>
			recompile
		
		case e:NodeChanged =>
			recompile

		case SelectionChanged(source) if( source eq viewcombobox ) =>
			image.recalc
	}	
}
