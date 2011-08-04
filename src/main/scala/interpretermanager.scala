package noiseeditor

import utilities._

object InterpreterManager extends InterpreterQueue {
	println("Starting InterpreterManager...")
	
	// Set Interpreter Execution Path to Project's Classpath
	settings.embeddedDefaults[noiseeditor.NoiseEditor.type]
	
	
	Seq("lib/simplex3d-math-core.jar","lib/simplex3d-math-double.jar").
			foreach (settings.classpath.append _)
	
	def init {
		val imports =
			"""import simplex3d.math._
			import simplex3d.math.double._
			import simplex3d.math.double.functions._
			import noiseeditor.Material
			import noiseeditor.Box"""

		apply(imports)
	}
	
	override def reset {
		super.reset
		init
		println("InterpreterManager: reset")
	}
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
		jq ! Job(() => super.reset)
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

