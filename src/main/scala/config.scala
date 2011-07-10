
import swing._
import java.awt.Color
import java.awt.Color._
import scala.util.matching.Regex

package object config{
	val RegexArg = new Regex("""([a-z][A-Za-z0-9]*)\W*\:\W*([A-Z][A-Za-z0-9\[\]\ ]*)(\W*\=\W*(.*))?""")
	// val RegexArg(arname, argtype, _, argdefault) = "xs : Seq[Float] = Seq(0f)"

	val RegexType = new Regex("""([A-Z][A-Za-z0-9]*)(\W*\[\W*([A-Z][A-Za-z0-9]*)\W*\]\W*)?""")
	// val RegexType(supertype, _, subtype) = argtype
	
	val GridIndicatorColor:Color = new Color(0x39B3E9)
	val GridIndicatorScale = 0.1f
	val IsolineColor:Int = 0x39E9B3
	val GridColor:Int = 0xFFB578
	
	
	val ConnectorHighlightColor = GREEN
	
	val MaterialDefaultColor = 0x000000
	
	val DepthStepSize = 3f
	val DepthMaxsteps = 16
	val DepthFadeOutFactor = 0.81
	
	class RemoveButton(title:String) extends Button(title) {
		margin = new Insets(0,0,0,0)
		background = RED
		tooltip = "Remove"
	}
	
	val TypeDefaults = Map(
		"Float" -> "0f",
		"Seq" -> "Nil",
		"Vec3" -> "Vec3(0)"
	)
	
	
}
