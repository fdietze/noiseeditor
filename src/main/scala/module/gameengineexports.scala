package noiseeditor.modules

import noiseeditor.util._
import noiseeditor.datastructure._

import noiseeditor.manager.CompositionManager
import noiseeditor.connector.OutConnector

object GameEngineExports {
	
	def generatexmlmaterials:String = {
		val document =
		<document>{
		for( (matname, index) <- GameEngineMaterials.materialNames zipWithIndex ) yield {
			<material id={index.toString} name={matname} />
		}
		}</document>
		document.toString
	}
	
	def generatescalacode(functionname:String, defaultreturn:String, outconnector:Option[OutConnector]):String = {
		var functioncode     = ""
		var functioncallcode = ""
		var returnvalue      = defaultreturn
		
		outconnector match {
			case Some(out) =>
				val composition = CompositionManager.create(out, "scala")
				import composition._
		
				// Function Definitions
				functioncode = (for( NodeFunctionFull(name, returntype, code, arguments, sliders) <- functions ) yield {
					"def %s(%s):%s = {%s}".format(
						name,
						(arguments ++ sliders).map(a => "%s:%s".format(a.name, a.datatype)).mkString(", "),
						returntype,
						code
					)
				}).mkString("\n")


				// Function Calls via BFS
				var functioncalls:Seq[String] = Nil
				val nexttrees = new collection.mutable.Queue[CompositionTree]
				nexttrees += calltree
				while( nexttrees.nonEmpty ) {
					val currenttree = nexttrees.dequeue
					import currenttree._
					functioncalls +:= "val %s = %s(%s)".format(
						varname,
						function.name,
						(
							arguments.map{
								case Right(arg) => arg.varname
								case Left(default) => default }
							++
							sliders.map(_.globalvalue)
						).mkString(", ")
					)
					// Move on with all arguments referring to other functions
					nexttrees ++= currenttree.arguments.collect{case Right(x) => x}.distinct
				}
				functioncallcode = functioncalls.distinct.mkString("\n")


				// Final Value
				returnvalue = calltree.varname
			
			case None =>
		}

""" 
package openworld.gen

import noise.Noise.noise3
import noise.Worley.cellnoise

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

// case class Material(color:Int = 0x000000)
import openworld.Util.Material

object %s {

def apply(world:Vec3) = {

%s

%s

%s
}

}
""".format( functionname, functioncode, functioncallcode, returnvalue )
	}



	def generateglslcode(outconnector:Option[OutConnector]):String = {
		var functioncode     = ""
		var functioncallcode = ""
		var returnvalue      = "vec4(0.5, 0.5, 0.5, 0.0)"

		outconnector match {
			case Some(out) =>
				val composition = CompositionManager.create(out, "glsl")
				import composition._

				// Function Definitions
				functioncode = (for( NodeFunctionFull(name, returntype, code, arguments, sliders) <- functions ) yield {
					"%s %s(%s) {%s}".format(
						returntype,
						name,
						(arguments ++ sliders).map(a => "%s %s".format(a.datatype, a.name)).mkString(", "),
						code
					)
				}).mkString("\n")


				// Function Calls via BFS
				var functioncalls:Seq[String] = Nil
				val nexttrees = new collection.mutable.Queue[CompositionTree]
				nexttrees += calltree
				while( nexttrees.nonEmpty ) {
					val currenttree = nexttrees.dequeue
					import currenttree._
					functioncalls +:= "%s %s = %s(%s);".format(
						function.returntype,
						varname,
						function.name,
						(
							arguments.map{
								case Right(arg) => arg.varname
								case Left(default) => default }
							++
							sliders.map(_.globalvalue)
						).mkString(", ")
					)
					// Move on with all arguments referring to other functions
					nexttrees ++= currenttree.arguments.collect{case Right(x) => x}.distinct
				}
				functioncallcode = functioncalls.distinct.mkString("\n")


				// Final Value
				returnvalue = calltree.varname
			
			case None =>
		}

""" 
#version 120
//#extension GL_EXT_gpu_shader4 : enable
//#extension GL_ARB_gpu_shader_fp64 : enable

varying vec3 vertex;
varying vec3 normal;
varying vec4 world;
uniform float time;

/*
possible vertex shader:
#version 120

varying vec3 vertex;
varying vec3 normal;
varying vec4 world;

void main () {
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;

	vertex = vec3(gl_ModelViewMatrix * gl_Vertex);       
	normal = normalize(gl_NormalMatrix * gl_Normal);
    world  = gl_Vertex;
}
*/

/*int seed = 0;
int a = (seed ^ int(0xB5C18E6A)) | ((1 << 16) + 1);
int c = seed ^ int(0xF292D0B2);
int hash(int x){ return (a*(x ^ c)) >> 16; }
int hash(int k) { return ((k*int(0x12345678)) >> (k*int(0x87754351))) & 0x7FFFFFFF; }*/
int hash(int k) { return int(mod(((k*34)+1)*k, 289)); }


/*float grad(int hash, float x, float y, float z) {
      int h = hash & 0xF;
      float u = h<8 ? x : y,
             v = h<4 ? y : h==12||h==14 ? x : z;
      return ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
}*/
float grad(int hash, float x, float y, float z) {
      int h = int(mod(hash,16));
      float u = h<8 ? x : y,
             v = h<4 ? y : h==12||h==14 ? x : z;
      return ((mod(h,2)) == 0 ? u : -u) + ((mod(h,4)-mod(h,2)) == 0 ? v : -v);
}

int fastfloor(float x) { return int( x > 0 ? x : x-1); }
float fade(float t) { return t * t * t * (t * (t * 6 - 15) + 10); }
float lerp(float t, float a, float b) { return a + t * (b - a); }

float noise3(float x, float y, float z) {
	int X = fastfloor(x);
	int Y = fastfloor(y);
	int Z = fastfloor(z);

	float relx = x - float(X);
	float rely = y - float(Y);
	float relz = z - float(Z);

	float u = fade(relx);
	float v = fade(rely);
	float w = fade(relz);
	
	int A = hash(X  )+Y; int AA = hash(A)+Z; int AB = hash(A+1)+Z;
	int	B = hash(X+1)+Y; int BA = hash(B)+Z; int BB = hash(B+1)+Z;

	return lerp(w,	lerp(v,	lerp(u, grad(hash(AA  ), relx  , rely  , relz	),
									grad(hash(BA  ), relx-1, rely  , relz	)),
							lerp(u, grad(hash(AB  ), relx  , rely-1, relz	),
									grad(hash(BB  ), relx-1, rely-1, relz	))),
					lerp(v, lerp(u, grad(hash(AA+1), relx  , rely  , relz-1 ),
									grad(hash(BA+1), relx-1, rely  , relz-1 )),
							lerp(u, grad(hash(AB+1), relx  , rely-1, relz-1 ),
									grad(hash(BB+1), relx-1, rely-1, relz-1 ))));
}

float noise3(vec3 v) {return noise3(v.x, v.y, v.z);}

/////////////////////////////////////////////////////

%s



void main(){

%s


	vec4 materialcolor = %s;
	
	vec3 L = normalize(gl_LightSource[0].position.xyz - vertex);   
	vec4 Idiff = clamp(gl_FrontLightProduct[0].diffuse * max(dot(normal,L), 0.0), 0.0, 1.0);  

	gl_FragColor = materialcolor * Idiff;
}
""".format(	functioncode, functioncallcode, returnvalue	)

	}

	def generatepredictioncode(outconnector:Option[OutConnector]):String = {
		var functioncode     = ""
		var functioncallcode = ""
		var returnvalue      = "Interval(0,0)"

		outconnector match {
			case Some(out) =>
				val composition = CompositionManager.create(out, "prediction")
				import composition._

				// Function Definitions
				functioncode = (for( NodeFunctionFull(name, returntype, code, arguments, sliders) <- functions ) yield {
					"def %s(%s):%s = {%s}".format(
						name,
						(arguments ++ sliders).map(a => "%s:%s".format(a.name, a.datatype)).mkString(", "),
						returntype,
						code
					)
				}).mkString("\n")


				// Function Calls via BFS
				var functioncalls:Seq[String] = Nil
				val nexttrees = new collection.mutable.Queue[CompositionTree]
				nexttrees += calltree
				while( nexttrees.nonEmpty ) {
					val currenttree = nexttrees.dequeue
					import currenttree._
					functioncalls +:= "val %s = %s(%s)".format(
						varname,
						function.name,
						(
							arguments.map{
								case Right(arg) => arg.varname
								case Left(default) => default }
							++
							sliders.map(_.globalvalue)
						).mkString(", ")
					)
					// Move on with all arguments referring to other functions
					nexttrees ++= currenttree.arguments.collect{case Right(x) => x}.distinct
				}
				functioncallcode = functioncalls.distinct.mkString("\n")


				// Final Value
				returnvalue = calltree.varname
			
			case None =>
		}
		
""" 
package openworld.gen

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import noise.Noise.noise3_prediction
import noise.interval
import noise.interval.{Interval, Volume, Interval4D}

object prediction {

def apply(world:Volume) = {

%s

%s

%s
}

}
""".format(	functioncode, functioncallcode, returnvalue	)
	}
}