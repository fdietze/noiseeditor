package noiseeditor.modules

import noiseeditor.util._
import noiseeditor.datastructure._

object GameEngineUtil {

	def readMaterials = {
		import xml._
		import java.awt.image.BufferedImage
		import javax.imageio.ImageIO

		// Read PNG textures from materials.xml
		def readResource(s:String) = getClass.getClassLoader.getResourceAsStream(s)
	
		val document = try { 
			XML.load( readResource("materials.xml") )
		}
		catch {
			case _ => <document/>
		}
	

		for( material <- (document \ "materials" \ "material") ) yield {
			val matname =  (material \ "@name").text
		
			// Texturdatei auslesen und samplen um an Farbe zu kommen
		
			val colors = 
			try {
				val img = ImageIO.read(readResource("materials/%s.png" format matname))
				val colors = Array.ofDim[Int](img.getWidth * img.getHeight)
				img.getRGB(0, 0, img.getWidth, img.getHeight, colors, 0, img.getWidth)
			} catch {
				case x => println("Unable to load texture: " + "materials/%s.png" format matname)
				Array(0x0000FF)
			}
		
			val r = colors.map( red _ ).sum / colors.size
			val g = colors.map( green _ ).sum / colors.size
			val b = colors.map( blue _ ).sum / colors.size
		
		
			NodeType(matname,
				LanguageMap("scala" -> Nil, "glsl" -> Nil),	Nil,
				LanguageMap(
					"scala" -> Map( "m" -> NodeFunction("mat" + matname, "Material", "Material(%d,'%s)" format(rgbcolor(r,g,b), matname) ) ),
					"glsl" -> Map("m" -> NodeFunction("mat" + matname, "vec4", "return vec4(%f, %f, %f, 0);".format(r/256.0,g/256.0,b/256.0) ) )
				)
			)
		}
	}
}
