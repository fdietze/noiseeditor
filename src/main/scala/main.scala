package noiseeditor

import config._
import utilities._
import swingextensions._
import datastructures._

import swing._
import event._
import javax.swing.UIManager
import javax.swing.SwingUtilities._
import javax.swing.border._
import javax.swing.border.BevelBorder._
import javax.swing.KeyStroke.getKeyStroke
import Orientation._
import java.awt.Color._

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

//TODO: folding nodes?
//TODO: Metanodes?
//TODO: Contextmenu for adding nodes

object NoiseEditor extends SimpleSwingApplication {
	UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName())
	
	//TODO: reorganize resets
	def reset {
		//TODO: BUG: repaint does not work
		top.menuBar = rebuildmenu
		NodeManager.reset
		ConnectionManager.reset
		InterpreterManager.reset
		Node.reset
		FileManager.setFileunchanged
	}
	
	def setTitle(window:MainFrame, subtitle:String = ""){
		if( subtitle.isEmpty )
			window.title = "Noise Editor"
		else
			window.title = subtitle + " - Noise Editor"
	}

	def init {
		reset
		NodeManager.listenTo(top)
		ConnectionManager.listenTo(top, NodeManager)

		NodeManager.peer.setSize(top.preferredSize)
		ConnectionManager.peer.setSize(top.preferredSize)
		
		// Load some preconnected nodes
		//TODO: Different Resourcepath on Mac OSX?
		//FileManager.readSession(getClass.getClassLoader.getResource("default.xml").getPath)
		FileManager.setFileunchanged
	}

	val top = new MainFrame {
		peer.setLocationByPlatform( true )
		minimumSize = Vec2i(320, 240)
		preferredSize = Vec2i(800,600)
		setTitle(this)
		
		// If window is closed check if current document is saved
		import javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE
		peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)
		
		override def closeOperation = {
		//	if( FileManager.unsavedQuestion )
				sys.exit(0)
		}
		
		contents = new NullPanel("MainWindow", NodeManager, ConnectionManager)
	}

	init

	def rebuildmenu = new MenuBar {
		contents += new Menu("File"){

			contents += new MenuItem("New") {
				mnemonic = Key.O
				action = new Action("New") {
					def apply = FileManager.newSession
					accelerator = Some(getKeyStroke("ctrl N"))
				}
			}				

			contents += new Menu("Load Module") {
				mnemonic = Key.M
				for( module <- ModuleManager.available.map(_.title) )
					contents += new MenuItem(module) {
						action = new Action(module) {
							def apply = { ModuleManager.load(module) }
						}
					}
			}				


			contents += new MenuItem("Open") {
				mnemonic = Key.O
				action = new Action("Open") {
					def apply = FileManager.open
					accelerator = Some(getKeyStroke("ctrl O"))
				}
			}				
			
			contents += new MenuItem("Save") {
				mnemonic = Key.S
				action = new Action("save") {
					def apply = FileManager.save
					accelerator = Some(getKeyStroke("ctrl S"))
				}
			}				

			contents += new MenuItem("Save As") {
				mnemonic = Key.S
				action = new Action("save As") {
					def apply = FileManager.saveAs
					accelerator = Some(getKeyStroke("ctrl shift S"))
				}
			}				

			contents += new MenuItem("Quit") {
				mnemonic = Key.Q
				action = new Action("Quit") {
					def apply() = top.closeOperation
					//TODO: Escape to quit program
					accelerator = Some(getKeyStroke("alt Q"))
				}
			}
		}
		
		for( NodeCategory(title, nodetypes) <- ModuleManager.nodecategories ) {
			contents += new Menu(title){
				for( nodetype <- nodetypes ) {
					contents += new MenuItem(nodetype.title) {
						action = new Action(nodetype.title) {
							def apply = NodeManager.add(Node(nodetype))
						}
					}
				}
			}
		}
		
		contents += new Menu("Other"){
			contents += new MenuItem("Preview") {
				action = new Action("Preview") {
					def apply = NodeManager.add(Node.preview())
				}
			}
			contents += new MenuItem("Custom Node") {
				action = new Action("Custom Node") {
					def apply = NodeManager.add(Node.custom())
				}
			}
		}
	}
}


