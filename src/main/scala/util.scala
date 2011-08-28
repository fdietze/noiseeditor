package noiseeditor

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import config._


package object util {

def time[A](msg:String)(function: => A) = {
	val start = System.nanoTime
	val returnvalue = function
	val duration = (System.nanoTime-start)/1000000000.0
	printf("%s: %fs\n", msg, duration)
	returnvalue
}

class Timer {
	var starttime = 0L
	var passedtime = 0L

	def getTime = System.nanoTime

	def start  { starttime = getTime }
	def stop   { passedtime += getTime - starttime }
	def measure[A](function: => A) = {
		start
		val returnvalue = function
		stop
		returnvalue
	}
	def reset  { passedtime = 0 }
	def read =   passedtime/1000000000.0
}


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

object Box{	def apply[T](value:T) = new Box[T](value) }
class Box[T](var value:T) { override def toString = value.toString }

class InterpreterQueue extends tools.nsc.interpreter.IMain {
	import javax.script.ScriptException
	import tools.nsc.interpreter.Results._
	import actors.Future
	import actors.Futures.future

	val jq = new JobQueue
	
	
	private def compile[T:Manifest](code:String):Option[T] = {
		//TODO: Important: Better handling of wrong type
		if( interpret(code) == Success ) {
/*			val term = runtimeTypeOfTerm(mostRecentVar)
//			val termtype = manifest[term]
			val manifesttype = manifest[T]
			
			println(term + "\n" + manifesttype + "\n" + manifesttype.erasure.getName)
			
			if( manifesttype != termtype ) {
				println("Type of compiled code does not match:\nIs: " + termtype + "\nShould be: " + manifesttype)
				None
			}
			else*/
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
			super.bind(name, boundType, value)
		})).asInstanceOf[Future[Result]]
	}
	
	override def bind(name: String, boundType: String, value: Any): Result = {
		println("Warning: Use fbind() instead of bind() which will return a Future[Result].")
		super.bind(name, boundType, value)
	}
	
	override def reset {
	//TODO: reset actor with poison-pill
		jq ! Job(() => {
			super.reset
		})
	}
}

case class Job (function:() => Any)
class JobQueue extends actors.DaemonActor {
	start
	def act = {
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