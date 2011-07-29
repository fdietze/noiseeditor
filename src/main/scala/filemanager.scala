package noiseeditor
import xml._
import java.io.File

import swing._
import event._
import Orientation._

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import utilities._

object FileManager extends Publisher {
	println("Starting SaveManager")
	
	// TODO: reset selected filename in FileChooser
	
	val chooser = new FileChooser(new File("./saves")) {
		import FileChooser.Result.Value

		fileSelectionMode = FileChooser.SelectionMode.FilesOnly
	
		def setExtensionFilter(description:String, extension:String) {
			peer.resetChoosableFileFilters
			peer.setFileFilter(new javax.swing.filechooser.FileNameExtensionFilter(description, extension) )
		}
	
		def showOpenDialog:Value = showOpenDialog(NoiseEditor.top.contents.head)
		def showSaveDialog:Value = showSaveDialog(NoiseEditor.top.contents.head)
	}
	
	var currentFile:Option[File] = None
	var filechanged:Boolean = false
	
	def newSession {
		if( unsavedQuestion ) {
			NoiseEditor.reset
			currentFile = None
			setFileunchanged
		}
	}
	
	def open {
		import FileChooser.Result._

		if( unsavedQuestion ) {
			chooser.title = "Open Composition"
			chooser.setExtensionFilter("XML Files", "xml")
			chooser.showOpenDialog match {
				case Approve =>
					NoiseEditor.reset
					FileManager.readSession(chooser.selectedFile)
					currentFile = Some(chooser.selectedFile)
					setFileunchanged
				case Cancel =>
			}
		}
	}
	

	def saveAs:Boolean = {
		import FileChooser.Result._
		chooser.title = "Save Composition"
		chooser.setExtensionFilter("XML Files", "xml")
		chooser.showSaveDialog match {
			case Approve =>
				var file = chooser.selectedFile
				if( !file.getName.toLowerCase.endsWith(".xml") )
					file = new File(file.getPath + ".xml")
				//TODO: Ask if overwriting file	
				FileManager.writeSession(file)
				currentFile = Some(file)
				setFileunchanged
				true
			case Cancel =>
				false
		}
	}
	
	def save {
		currentFile match {
			case Some(file) =>
				FileManager.writeSession(currentFile.get)
				setFileunchanged
				true
			case None =>
				saveAs
		}
	}

	def unsavedQuestion:Boolean = {
		import Dialog.Result._
		if( filechanged && NodeManager.nodes.size > 0 ) {
			Dialog.showConfirmation(
				parent = NoiseEditor.top.contents.head,
				message = "Current session not saved yet. Save it now?",
				optionType = Dialog.Options.YesNoCancel
				
			) match {
				case Ok =>
					save
					true
				case No =>
					true
				case Cancel =>
					false
			}
		}
		else
			true
	}


	// TODO: Fix Filechanged
	listenTo(NodeManager, ConnectionManager)
	reactions += {
		case e:NodeChanged => setFilechanged(e)
		case e:NodeValueChanged => setFilechanged(e)
		case e:NodeConnected => setFilechanged(e)
		case e:NodeMoved => setFilechanged(e)
		case e:NodeResized => setFilechanged(e)
		//TODO: Add Changes in Previews and CustomNodes
	}
	
	def setFilechanged(reason:Event) {
		//println("setFileChanged, reason: " + reason)
		filechanged = true
		currentFile match {
			case Some(file) =>
				NoiseEditor.setTitle(NoiseEditor.top, "*" + file.getName)
			case None =>
				NoiseEditor.setTitle(NoiseEditor.top, "*")
		}
	}

	def setFileunchanged {
		filechanged = false
		currentFile match {
			case Some(file) =>
				NoiseEditor.setTitle(NoiseEditor.top, file.getName)
			case None =>
				NoiseEditor.setTitle(NoiseEditor.top, "")
		}
	}
	
	
	
	val prettyprinter = new PrettyPrinter(80,2)
	
	def readSession(path:String) { readSession(new File(path)) }
	def writeSession(path:String) { writeSession(new File(path)) }
	
	def readSession(file:File) {
		println("SaveManager: reading " + file)
		val document = XML.loadFile(file)
		
		val ids = (document \ "nodes" \ "node") map ( n => (n \ "@id").text.toInt)
		val newid = Node.getIdMapping(ids)
		val nodeforid = new collection.mutable.HashMap[Int,Node]
		
		for( node <- document \ "nodes" \ "node" ) {
			val title = (node \ "@title").text
			val nodetype = (node \ "@type").text
			val id = (node \ "@id").text.toInt
			val x = (node \ "pos" \ "@x").text.toInt
			val y = (node \ "pos" \ "@y").text.toInt
			val location = Vec2i(x,y)

			val slidernames = ( node \ "sliders" \ "slider" ) map ( _ \ "@name" text)
			val slidervalues = ( node \ "sliders" \ "slider" ) map ( s => (s \ "@value").text.toInt)
			val sliderformulas = ( node \ "sliders" \ "slider" ) map ( s => (s \ "@formula").text) map ( f => if( f.isEmpty ) "s" else f)
			val intypes = ( node \ "intypes" \ "intype" ) map ( _  \ "@type" text )
			
			val functions =
				for( function <- node \ "functions" \ "function" ) yield {
					val name =    (function \ "@name"   ).text
					val code =    function.text
					val outtype = (function \ "@outtype").text
					var outname = (function \ "@outname").text

					new Function(name,code,outtype,outname)
				}
			

			
			nodetype match {
			case "predefined" =>
				val nodetype = FunctionNodeType("", title, intypes, slidernames zip sliderformulas, functions:_*)
				val predefined = Node(nodetype, newid(id))
				nodeforid(predefined.id) = predefined
				NodeManager.add(predefined)
				for( (slider, value) <- predefined.sliders zip slidervalues )
					slider.value = value
				predefined.peer.setLocation(location)
			case "custom" =>
				val width = (node \ "size" \ "@width").text.toInt
				val height = (node \ "size" \ "@height").text.toInt
				val size = Vec2i(width,height)
				val custom = Node.loadcustom( slidernames, intypes, functions.head.code, newid(id) )
				nodeforid(custom.id) = custom
				NodeManager.add(custom)
				for( (slider, value) <- custom.sliders zip slidervalues )
					slider.value = value
				custom.peer.setLocation(location)
				custom.peer.setSize(size)
			case "preview" =>
				val width = (node \ "size" \ "@width").text.toInt
				val height = (node \ "size" \ "@height").text.toInt
				val size = Vec2i(width,height)
				val offsetx = (node \ "image" \ "@offsetx").text.toDouble
				val offsety = (node \ "image" \ "@offsety").text.toDouble
				val offset = Vec2(offsetx, offsety)
				val zoom = (node \ "image" \ "@zoom").text.toDouble
				val selectedview = (node \ "view" \ "@selected").text
				val selectedperspective = (node \ "perspective" \ "@selected").text
				val depthslider = (node \ "depthslider" \ "@value").text.toDouble
				val preview = Node.preview(newid(id))

				nodeforid(preview.id) = preview
				NodeManager.add(preview)
				preview.peer.setLocation(location)
				preview.peer.setSize(size)
				preview.image.offset = offset
				preview.image.zoom = zoom
				
				preview.viewcombobox.select(selectedview)
				preview.perspectivecombobox.select(selectedperspective)				
				preview.depthslider.value = depthslider
			}
		}

		for( connection <- document \ "connections" \ "connection" ) {
			val innodeid  = (connection \ "in"  \ "@nodeid").text.toInt
			val outnodeid = (connection \ "out" \ "@nodeid").text.toInt
			val inindex   = (connection \ "in"  \ "@connector").text.toInt
			val outindex  = (connection \ "out" \ "@connector").text.toInt
			val inconnector  = nodeforid(newid(innodeid)).inconnectors(inindex)
			val outconnector = nodeforid(newid(outnodeid)).outconnectors(outindex)
			ConnectionManager.changeConnection(inconnector, outconnector)
		}
		ConnectionManager.commitconnection
	}

	def writeSession(file:File) {
		println("writing " + file)
		implicit def intToString(x:Int) = x.toString
		implicit def doubleToString(x:Double) = x.toString
		implicit def booleanToString(x:Boolean) = x.toString
		
		//TODO: replace last suffix
		val functionsuffix = "_t" + (System.currentTimeMillis/1000).toString
		
		val document = 
		<document>
			<nodes>{
				for(node <- NodeManager.nodes) yield {
					import node._
					val nodetype = node match{
						case n:PredefinedNode => "predefined"
						case n:CustomNode => "custom"
						case n:Preview => "preview"
					}
				
	
					<node title={title} type={nodetype} id={id}>
						<pos x={location.x} y={location.y}/>
						{if(nodetype == "preview") {
							val preview = node.asInstanceOf[Preview]
							import preview._
							<size width={size.width} height={size.height} />
							<image offsetx={image.offset.x} offsety={image.offset.y} zoom={image.zoom} />
							<view selected={viewcombobox.selected} />
							<perspective selected={perspectivecombobox.selectedname} />
							<depthslider value={depthslider.value} />
						}
						else if( nodetype == "custom") {
							<size width={size.width} height={size.height} />
						}
						}
						<sliders>
							{sliders.map( slider => <slider name={slider.name} value={slider.value} formula={slider.tformula} /> )}
						</sliders>
		
						<intypes>
							{intypes.map( intype => <intype type={intype} /> )}
						</intypes>
		
						<functions>
							{functions.map( f => <function name={f.name + functionsuffix} outtype={f.outtype} outname={f.outname}>{f.code}</function> )}
						</functions>
					</node>
				}
			}</nodes>
			<connections>{/*
				for( connection <- ConnectionManager.connections.edges ) yield {
					val (in,out) =
					connection match {
						case (in:InConnector, out:OutConnector) =>
							(in,out)
						case (out:OutConnector, in:InConnector) =>
							(in, out)
					}
					// extract indizes of connectors in their nodes
					val  inindex = in .node.inconnectors.indexWhere( _ eq in)
					val outindex = out.node.outconnectors.indexWhere( _ eq out)
					
					<connection>
						<in  nodeid={ in.node.id} connector={inindex } />
						<out nodeid={out.node.id} connector={outindex} />
					</connection>
				}
			*/}</connections>
		</document>
		
		XML.save(file.getPath, node = document, enc="UTF-8", xmlDecl=true)
		/* PrettyPrinter scheint die Funktionen zu verfälschen
		val out = new java.io.FileWriter(file)
		out.write("<?xml version='1.0' encoding='UTF-8'?>\n")
		out.write(prettyprinter.format(document))
		out.close*/
	}
	
	
}
