package noiseeditor

import swing._
import event._
import utilities._
import config._
import javax.swing.SwingUtilities._
import Orientation._
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
	
	// remove all connections to a node
	def removeNode(node:Node) {
		connections -= node
		resetconnstart
	}
	
	
	override def paint(g:Graphics2D) {
		import java.awt.Color._
		
		super.paintComponent(g)
		import g._
		
		setColor(BLACK)
		for( ( a, b ) <- connections.edges ){
			val apos = convertPoint(a.peer.getParent, a.location, this.peer) + a.size / 2
			val bpos = convertPoint(b.peer.getParent, b.location, this.peer) + b.size / 2
			
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
		case HitConnector(source, connector) =>
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
		
		case UIElementResized(source) if( source eq NoiseEditor.top) =>
			peer.setSize(source.size)
	}

	override def toString = "ConnectionManager"
}
