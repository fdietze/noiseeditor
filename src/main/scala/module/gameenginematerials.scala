package noiseeditor.modules

import noiseeditor.util._
import noiseeditor.datastructure._

import xml._
import javax.imageio.ImageIO
import collection.immutable.VectorBuilder

object GameEngineMaterials {

	// Read PNG textures from materials.xml
	val (materialNames, nodeTypes) =
	{
		def readResource(s:String) = getClass.getClassLoader.getResourceAsStream(s)

		val document = try { 
			XML.load( readResource("materials.xml") )
		}
		catch {
			case _:Throwable => <document/>
		}
	
		val namebuilder     = new VectorBuilder[String]
		val nodetypebuilder = new VectorBuilder[NodeType]
	
	
		for( (material,matid) <- (document \ "materials" \ "material").zipWithIndex ) {
			val matname =  (material \ "@name").text
			namebuilder += matname
			// Texturdatei auslesen und samplen um an Farbe zu kommen
	
			val colors = 
			try {
				val img = ImageIO.read(readResource("materials/%s.png" format matname))
				val colors = Array.ofDim[Int](img.getWidth * img.getHeight)
				img.getRGB(0, 0, img.getWidth, img.getHeight, colors, 0, img.getWidth)
			} catch {
				case x:Throwable => println("Unable to load texture: " + "materials/%s.png" format matname)
				Array(0x0000FF)
			}
	
			val r = colors.map( red _ ).sum / colors.size
			val g = colors.map( green _ ).sum / colors.size
			val b = colors.map( blue _ ).sum / colors.size
	
			nodetypebuilder +=
				NodeType(matname,
					LanguageMap("scala" -> Nil, "glsl" -> Nil),	Nil,
					LanguageMap(
						"scala" -> Map( "m" -> NodeFunction("mat" + matname, "Material", "Material(%d,%d)" format(rgbcolor(r,g,b), matid) ) ),
						"glsl" -> Map("m" -> NodeFunction("mat" + matname, "vec4", "return vec4(%f, %f, %f, 0);".format(r/256.0,g/256.0,b/256.0) ) )
					)
				)
		}
		( namebuilder.result(), nodetypebuilder.result() )
	}
}
