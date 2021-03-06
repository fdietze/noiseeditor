package noiseeditor

import simplex3d.math.double.functions._


import java.awt.Color

// Contains misc. functions and datastructures for general use.
package object util {

// Can be used as stopwatch to measure times
class Timer {
	var starttime = 0L
	var passedtime = 0L

	def getTime = System.nanoTime

	def start()  { starttime = getTime }
	def stop()   { passedtime += getTime - starttime }
	def measure[A](function: => A) = {
		start()
		val returnvalue = function
		stop()
		returnvalue
	}
	def reset()  { passedtime = 0 }
	def read =   passedtime/1000000000.0
}

def nextPowerOfTwo(n:Int) = {
  var m = n - 1
  m |= m >> 1
  m |= m >> 2
  m |= m >> 4
  m |= m >> 8
  m |= m >> 16
  m+1
}

def hexStringtoInt(hex:String) = {
  val HexReg = """0x([0-9a-fA-F]+)""".r
  val HexReg(base16) = hex
  Integer.parseInt(base16,16)
}

//color manipulation
def rgbcolor(r:Int, g:Int, b:Int) = r << 16 | g << 8 | b
def graycolor(w:Int) = rgbcolor(w,w,w)
def red(c:Int) = c >> 16
def green(c:Int) = (c & 0x00FF00) >> 8
def blue(c:Int) = c & 0xFF
def mixcolors(a:Int, b:Int, t:Double=0.5) = {
	rgbcolor(
		(t*red(a)   + (1-t)*red(b)  ).toInt,
		(t*green(a) + (1-t)*green(b)).toInt,
		(t*blue(a)  + (1-t)*blue(b) ).toInt
	)
}
implicit def colorSetAlpha(c:Color) = new { def setAlpha(alpha:Int) = new Color(c.getRed,c.getGreen,c.getBlue, alpha) }

// Re-maps a number from one range to another
def remap(value:Double, low1:Double, high1:Double, low2:Double, high2:Double) =
	clamp( (value - low1)*(high2 - low2) / (high1 - low1) + low2, low2, high2 )


def thousandsseparator(s:String) = {
	var i = 0
	("" /: s.reverse)( (x,y) ⇒ {i+=1;x + (if(i%3==1) "." else "") +y}).drop(1).reverse
}


object Box{	def apply[T](value:T) = new Box[T](value) }
class Box[T](var value:T) { override def toString = value.toString }

class InterpreterQueue extends tools.nsc.interpreter.IMain {
	import tools.nsc.interpreter.Results._
	import actors.Future

	val jq = new JobQueue
	
	private def compile[T:Manifest](code:String):Option[T] = {
		//TODO: Important: Better handling of wrong type
		if( beQuietDuring(interpret(code)) == Success ) {
			valueOfTerm(mostRecentVar).asInstanceOf[Option[T]]
		}
		else {
			println("error in interpreted code: "+code+"\n")
			None
		}
	}
	
	def apply[T:Manifest](code:String):Future[Option[T]] = {
		(jq !! Job(() => {
			compile[T](code)
		})).asInstanceOf[Future[Option[T]]]
	}
	
	def fbind(name: String, boundType: String, value: Any): Future[Result] = {
		(jq !! Job(() => {
      beQuietDuring(super.bind(name, boundType, value))
		})).asInstanceOf[Future[Result]]
	}
	
	override def bind(name: String, boundType: String, value: Any, modifiers: List[String] = Nil): Result = {
		println("Warning: Use fbind() instead of bind() which will return a Future[Result].")
		super.bind(name, boundType, value)
	}
	
	override def reset() {
	//TODO: reset actor with poison-pill
		jq ! Job(() => {
			super.reset()
		})
	}
}

case class Job (function:() => Any)
class JobQueue extends actors.DaemonActor {
	start()
	def act() {
		loop {
			react {
				case j:Job =>
					try
						reply(j.function())
					catch {
						case e:Exception => println("JobQueue: Exception caught: " + e)
					}
				case _ => throw new RuntimeException("Message was not a Job\n")
			}
		}
	}
}

}
