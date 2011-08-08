package noiseeditor

import utilities._
import datastructures._
import swingextensions._

import swing._
import event._
import Orientation._

import xml._
import java.io.File

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._


object FileManager extends Publisher {
	println("Starting FileManager")
	
	// TODO: reset selected filename in FileChooser
	
	val chooser = new FileChooser(new File("./saves")) {
		import FileChooser.Result.Value

		fileSelectionMode = FileChooser.SelectionMode.FilesOnly
	
		def setExtensionFilter(description:String, extension:String) {
			peer.resetChoosableFileFilters
			peer.setFileFilter(new javax.swing.filechooser.FileNameExtensionFilter(description, extension) )
		}
	
		def showOpenDialog:Value = showOpenDialog(NoiseEditor.window.contents.head)
		def showSaveDialog:Value = showSaveDialog(NoiseEditor.window.contents.head)
	}
	
	var currentFile:Option[File] = None
	var filechanged:Boolean = false
	
	def newSession {
		ConnectionManager.reset
		NodeManager.reset
		Node.reset

		currentFile = None
		setFileunchanged
	}
	
	def open {
		import FileChooser.Result._
		chooser.title = "Open Composition"
		chooser.setExtensionFilter("XML Files", "xml")
		chooser.showOpenDialog match {
			case Approve =>
				newSession
				FileManager.readSession(chooser.selectedFile)
				currentFile = Some(chooser.selectedFile)
				setFileunchanged
			case Cancel =>
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
				parent = NoiseEditor.window.contents.head,
				message = "Leaving unsaved Session. Save it now?",
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
		//TODO: Add FileChanges in Previews and CustomNodes
	}
	
	def setFilechanged(reason:Event) {
		//println("setFileChanged, reason: " + reason)
		filechanged = true
		currentFile match {
			case Some(file) =>
				NoiseEditor.setTitle(NoiseEditor.window, "*" + file.getName)
			case None =>
				NoiseEditor.setTitle(NoiseEditor.window, "*")
		}
	}

	def setFileunchanged {
		filechanged = false
		currentFile match {
			case Some(file) =>
				NoiseEditor.setTitle(NoiseEditor.window, file.getName)
			case None =>
				NoiseEditor.setTitle(NoiseEditor.window, "")
		}
	}
	
	
	
	val prettyprinter = new PrettyPrinter(80,2)
	
	def readSession(path:String) { readSession(new File(path)) }
	def writeSession(path:String) { writeSession(new File(path)) }
	
	def readSession(file:File) {
		println("SaveManager: reading " + file)
		val document = XML.loadFile(file)
		
		val modulename = (document \ "module" \ "@name").text
		if( ModuleManager.title != modulename )
			ModuleManager.load(modulename)

		val ids = (document \ "nodes" \ "node") map ( n => (n \ "@id").text.toInt)
		val newid = Node.getIdMapping(ids)
		val nodeforid = new collection.mutable.HashMap[Int,Node]
		
		for( node <- document \ "nodes" \ "node" ) {
			val title = (node \ "@title").text
			val nodetype = (node \ "@type").text
			val id = (node \ "@id").text.toInt
			val x = (node \ "location" \ "@x").text.toInt
			val y = (node \ "location" \ "@y").text.toInt
			val location = Vec2i(x,y)
			
			val arguments =
				(for( language <- node \ "arguments" \ "language" ) yield {
					(language \ "@name" text) -> 
					(for( argument <- language \ "argument" ) yield {
						NodeArgument(
							argument \ "@name" text,
							argument \ "@datatype" text
						)
					})
				}).toMap
			
			val sliders =
				for( slider <- node \ "sliders" \ "slider" ) yield {
					NodeSlider(
						slider \ "@name" text,
						slider \ "@formula" text,
						(slider \ "@value" text).toInt
					)
				}
			
			val functions =
				(for( language <- node \ "functions" \ "language" ) yield {
					(language \ "@name" text) -> 
					(for( function <- language \ "function" ) yield {
						(function \ "@outname" text) ->
						NodeFunction(
							function \ "@name" text,
							function \ "@returntype" text,
							function text
						)
					}).toMap
				}).toMap
			

			
			nodetype match {
			case "predefined" =>
				val predefined = Node(NodeType(title, arguments, sliders, functions), newid(id))
				nodeforid(predefined.id) = predefined
				NodeManager.add(predefined)

				predefined.peer.setLocation(location)
			case "preview" =>
				val width = (node \ "size" \ "@width").text.toInt
				val height = (node \ "size" \ "@height").text.toInt
				val size = Vec2i(width,height)
				
				val offsetx = (node \ "image" \ "@offsetx").text.toDouble
				val offsety = (node \ "image" \ "@offsety").text.toDouble
				val offset = Vec2(offsetx, offsety)
				
				val zoom = (node \ "image" \ "@zoom").text.toDouble
				val view = (node \ "view" \ "@mode").text
				val perspective = (node \ "view" \ "@perspective").text
				val depthslider = (node \ "depthslider" \ "@value").text.toDouble

				val preview = Node.preview(newid(id))
				nodeforid(preview.id) = preview
				NodeManager.add(preview)

				preview.peer.setLocation(location)
				preview.peer.setSize(size)
				preview.image.offset = offset
				preview.image.zoom = zoom
				
				preview.viewcombobox.select(view)
				preview.perspective.select(perspective)				
				preview.depthslider.value = depthslider
			case "custom" =>
				val width = (node \ "size" \ "@width").text.toInt
				val height = (node \ "size" \ "@height").text.toInt
				val size = Vec2i(width,height)
				
				// add datatypes to sliders
				val newsliders = sliders.map{
					case NodeSlider(name, formula, value, "") => NodeSlider(name, formula, value, ModuleManager.sliderdatatypes("scala"))}
				
				// add default values to arguments
				val newarguments = arguments.map{ case (language,args) => (language -> args.map{
					case NodeArgument(name,datatype,"") => NodeArgument(name,datatype,ModuleManager.typedefaults("scala")(datatype)) }) }
				
				val custom = Node.loadcustom( newsliders, newarguments, functions("scala").values.head.code, newid(id) )
				nodeforid(custom.id) = custom
				NodeManager.add(custom)
				
				custom.peer.setLocation(location)
				custom.peer.setSize(size)
				custom.compile
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
		NoiseEditor.window.repaint
	}

	def writeSession(file:File) {
		println("writing " + file)
		implicit def intToString(x:Int) = x.toString
		implicit def doubleToString(x:Double) = x.toString
		implicit def booleanToString(x:Boolean) = x.toString
		
		//TODO: replace last suffix, higher base
		def uidsuffix(name:String) = {
			val suffix = "_uid" + (System.currentTimeMillis/1000).toHexString
			name.split("_uid").head + suffix
		}
		
		val document = 
		<document>
			<module name={ModuleManager.title} />
			<nodes>{
				for(node <- NodeManager.nodes) yield {
					import node._
					val nodetype = node match {
						case n:PredefinedNode => "predefined"
						case n:CustomNode => "custom"
						case n:Preview => "preview"
					}
				
	
					<node title={title} type={nodetype} id={id}>
						<location x={location.x} y={location.y}/>
						{
							if(nodetype == "preview") {
								val preview = node.asInstanceOf[Preview]
								import preview._
								<size width={size.width} height={size.height} />
								<image offsetx={image.offset.x} offsety={image.offset.y} zoom={image.zoom} />
								<view mode={viewcombobox.selected} perspective={perspective.selectedname} />
								<depthslider value={depthslider.value} />
							}
							else if( nodetype == "custom") {
								<size width={size.width} height={size.height} />
							}
						}
						<arguments>{
							for( (language, nodearguments) <- arguments ) yield {
								<language name={language}>{
									for( NodeArgument(name, datatype, _) <- nodearguments ) yield {
										<argument name={name} datatype={datatype} />
									}
								}</language>
							}
						}</arguments>

						<sliders>
							{sliders.map( slider => <slider name={slider.name} formula={slider.formula} value={slider.value} /> )}
						</sliders>
		
						<functions>{
							for( (language, nodefunctions) <- functions ) yield {
								<language name={language}>{
									for( (outname, NodeFunctionFull(name, returntype, code, _, _)) <- nodefunctions ) yield {
										<function outname={outname} name={name} returntype={returntype}>{code}</function>
									}
								}</language>
							}
						}</functions>
					</node>
				}
			}</nodes>
			<connections>{
				for( (in,out) <- ConnectionManager.connections.edges ) yield {
					// extract indizes of connectors in their nodes
					val  inindex = in .node.inconnectors .indexWhere( _ eq in )
					val outindex = out.node.outconnectors.indexWhere( _ eq out)
					
					<connection>
						<in  nodeid={in .node.id} connector={inindex } />
						<out nodeid={out.node.id} connector={outindex} />
					</connection>
				}
			}</connections>
		</document>
		
		XML.save(file.getPath, node = document, enc="UTF-8", xmlDecl=true)
		/* PrettyPrinter scheint die Funktionen zu verfälschen
		val out = new java.io.FileWriter(file)
		out.write("<?xml version='1.0' encoding='UTF-8'?>\n")
		out.write(prettyprinter.format(document))
		out.close*/
	}
	
	
}
