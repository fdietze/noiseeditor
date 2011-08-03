package noiseeditor

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import config._


package object utilities {

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

}
