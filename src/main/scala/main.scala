package noiseeditor

import manager._

import config._
import util._
import swingextension._
import datastructure._

import swing._
import swing.event._
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

object NoiseEditor extends SimpleSwingApplication {
	UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName())
	
	def init {
		InterpreterManager.reset
		
		NodeManager.listenTo(window)
		ConnectionManager.listenTo(window, NodeManager)

		NodeManager.peer.setSize(window.preferredSize)
		ConnectionManager.peer.setSize(window.preferredSize)
		
		rebuildmenu
		
		
		ModuleManager.load("GameEngine")
	}

	def setTitle(window:MainFrame, subtitle:String = "") {
		if( subtitle.isEmpty )
			window.title = "Noise Editor"
		else
			window.title = subtitle + " - Noise Editor"
	}

	def window = top
	val top = new MainFrame {
		peer.setLocationByPlatform( true )
		minimumSize = Vec2i(320, 240)
		preferredSize = Vec2i(800,600)
		setTitle(this)
		
		// If window is closed check if current document is saved
		import javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE
		peer.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE)
		
		override def closeOperation = {
			if( FileManager.unsavedQuestion )
				sys.exit(0)
		}
		
		contents = new NullPanel("MainWindow", NodeManager, ConnectionManager)
	}

	init

	def rebuildmenu {
		window.menuBar = createmenu
		//TODO: Remove Workaround: Menu repaint does not work when there is no node in the window
		window.peer.setSize(window.size + Vec2i(1))
		window.peer.setSize(window.size - Vec2i(1))
	}
	
	def createmenu = new MenuBar {
		contents += new Menu("File"){

			contents += new MenuItem("New") {
				mnemonic = Key.O
				action = new Action("New") {
					def apply = if( FileManager.unsavedQuestion ) FileManager.newSession
					accelerator = Some(getKeyStroke("ctrl N"))
				}
			}				

			contents += new Menu("Load Module") {
				mnemonic = Key.M
				for( module <- ModuleManager.available.map(_.title) )
					contents += new MenuItem(module) {
						action = new Action(module) {
							def apply = { if( FileManager.unsavedQuestion ) ModuleManager.load(module) }
						}
					}
			}				


			contents += new MenuItem("Open") {
				mnemonic = Key.O
				action = new Action("Open") {
					def apply = if( FileManager.unsavedQuestion ) FileManager.open
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
					def apply() = window.closeOperation
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
	
		contents += new Menu("Custom"){
			contents += new MenuItem("Custom Node") {
				action = new Action("Custom Node") {
					def apply = NodeManager.add(Node.custom())
				}
			}
		}

		contents += new Menu("Preview"){
			contents += new MenuItem("Preview") {
				action = new Action("Preview") {
					def apply = NodeManager.add(Node.preview())
				}
			}
		}
	}
}

