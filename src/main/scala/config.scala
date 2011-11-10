package noiseeditor

import swing._
import java.awt.Color
import java.awt.Color._

package object config {
	
	val lookAndFeel = "com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel" 


	// Preview and Grid:
	val gridColors:IndexedSeq[Color] = IndexedSeq(
		new Color(0x39B3E9),
		new Color(0xB339E9),
		new Color(0xE9B339),
		new Color(0x39E9B3)
	)
	val isolineColor:Int = 0x39E9B3 // Type: Integer, because it is used on the underlying integer array of the preview image
	// the minimum distance of gridlines in pixels 
	val minGridSize = 5
	// the number of gridlines until a major grid line
	val gridDistance = 8
	val defaultZoom = 0.1
	
	
	// For the depth view:
	val depthStepSize = 3.0
	val depthMaxsteps = 16
	val depthFadeOutFactor = 0.81
	
	
	val connectorHighlightColor = new Color(0x73FF7B)
	val materialDefaultColor = 0x000000
	
	
	class RemoveButton(title:String = "x") extends Button(title) {
		margin = new Insets(0,0,0,0)
		background = new Color(0xFF7380)
		tooltip = "Remove"
	}
}
