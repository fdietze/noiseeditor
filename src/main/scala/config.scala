package noiseeditor

import swing._
import java.awt.Color
import java.awt.Color._

package object config {
	val gridIndicatorColor:Color = new Color(0x39B3E9)
	val gridDistance = 8
	val gridIndicatorScale = 0.1
	val isolineColor:Int = 0x39E9B3
	val gridColor:Int = 0xFFB578
	
	
	val connectorHighlightColor = GREEN
	
	val materialDefaultColor = 0x000000
	
	val depthStepSize = 3.0
	val depthMaxsteps = 16
	val depthFadeOutFactor = 0.81
	
	class RemoveButton(title:String = "x") extends Button(title) {
		margin = new Insets(0,0,0,0)
		background = new Color(0xFF7380)
		tooltip = "Remove"
	}
}
