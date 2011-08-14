package noiseeditor

import swing._
import java.awt.Color
import java.awt.Color._

package object config {
	val GridIndicatorColor:Color = new Color(0x39B3E9)
	val GridIndicatorScale = 0.1
	val IsolineColor:Int = 0x39E9B3
	val GridColor:Int = 0xFFB578
	
	
	val ConnectorHighlightColor = GREEN
	
	val MaterialDefaultColor = 0x000000
	
	val DepthStepSize = 3.0
	val DepthMaxsteps = 16
	val DepthFadeOutFactor = 0.81
	
	class RemoveButton(title:String = "x") extends Button(title) {
		margin = new Insets(0,0,0,0)
		background = RED
		tooltip = "Remove"
	}
}
