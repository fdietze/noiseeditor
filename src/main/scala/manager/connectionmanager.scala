package noiseeditor.manager


import noiseeditor.NoiseEditor
import noiseeditor.Node
import noiseeditor.connector._
import noiseeditor.event._

import noiseeditor.util._
import noiseeditor.config._
import noiseeditor.datastructure._
import noiseeditor.swingextension._

import swing._
import swing.event._
import javax.swing.SwingUtilities._
import Orientation._
import java.awt.Color
import java.awt.Color._

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._


object ConnectionManager extends Component {
	println("Starting ConnectionManager...")
	
	val connections = new ConnectionTree

	def reset {
		connections.clear
		commitconnection
	}
	
	// removes all connections to a node
	def removeNode(node:Node) {
		connections -= node
		resetconnstart
	}
	
	
	override def paint(g:Graphics2D) {
		import java.awt.Color._
		
		super.paintComponent(g)
		import g._
		
		for( ( a, b ) <- connections.edges ){
			val apos = convertPoint(a.peer.getParent, a.location, this.peer) + a.size / 2
			val bpos = convertPoint(b.peer.getParent, b.location, this.peer) + b.size / 2
			
			// Color the lines depending on the distance
			val d = distance(apos, bpos) / 500.0
			val t = clamp(d*d,0,1)
			setColor(new Color(mixcolors(0x0000FF,0x000000,t)))
			
			//TODO: Draw Curves instead of Lines
			drawLine( apos.x, apos.y, bpos.x, bpos.y)
		}
	}
	

	var connstart:Option[Connector] = None

	def resetconnstart {
		connstart match {
			case Some(connector) =>
				connector.background = connector.originalbackground
			case None =>
		}
		connstart = None
	}
	
	def setconnstart(connector:Connector) {
		resetconnstart
		connstart = Some(connector)
		connstart.get.background = connstart.get.highlightbackground
	}
	
	def commitconnection {
		resetconnstart
		publish(NodeConnected(this))
	}
	
	// Try to connect in and out and tell if something changed
	def changeConnection(in:InConnector, out:OutConnector):Boolean = {
		if( out.datatype == in.datatype ) {
			// If already connected
			if( connections(in, out) )
			{
				// disconnect
				connections -= (in,out)
				true
			}
			else // not connected or connected with a different connector
			{
				// add or replace connection
				if( connections += (in -> out) ) {
					this.repaint
					true
				}
				else // Connection would produce cycle
					false
			}
		}
		else // Datatypes don't match
			false
	}
	
	reactions += {
		// Double Click on Connector:
		// Remove all connections to this connector
		case HitConnector(source, connector, 2) =>
			connector match {
				case in:InConnector => connections -= in
				case out:OutConnector => connections -= out
			}
			commitconnection
		
		// Single Click on Connector
		case HitConnector(source, connector, 1) =>
			(connstart, connector) match {
				case (Some(in:InConnector), out:OutConnector) =>
					if( changeConnection(in, out) )
						commitconnection
					else
						setconnstart(connector)
				
				case (Some(out:OutConnector), in:InConnector) =>
					if( changeConnection(in, out) )
						commitconnection
					else
						setconnstart(connector)
				
				// Hit the same connector again
				case (Some(connectora), connectorb) if( connectora eq connectorb)=>
					resetconnstart
				
				// Hit a connector on the same Node
				case (Some(connectora), connectorb) if( connectora.node eq connectorb.node)=>
					setconnstart(connectorb)
				
				case _ => 
					setconnstart(connector)
			}

		case e:NodeMoved =>
			this.repaint

		case e:NodeResized =>
			this.repaint
			
		case e:NodeConnected =>
			this.repaint
		
		case UIElementResized(source) if( source eq NoiseEditor.window) =>
			peer.setSize(source.size)
	}

	override def toString = "ConnectionManager"
}
