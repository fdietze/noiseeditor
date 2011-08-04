package noiseeditor.modules

import noiseeditor.utilities._
import noiseeditor.datastructures._

import noiseeditor.{ModuleManager, Module}

//TODO: More High Level nodes, like Surface, Layers, Fractal Noise, Turbulence
//TODO: More Noise types, like cellular noise
//TODO: Tooltip with node description
//TODO: Different Noise Dimensions

object GameEngine extends Module {
	
	val languages = Seq("scala", "glsl")

	val scalainitcode = """
		import simplex3d.math._
		import simplex3d.math.double._
		import simplex3d.math.double.functions._
		import noise.Noise.noise3
	"""
	val typedefaults = LanguageMap(
		"scala" -> 	Map(
			"Int" -> "0",
			"Double" -> "0.0",
			"Seq" -> "Nil",
			"Vec2" -> "Vec2(0)",
			"Vec3" -> "Vec3(0)",
			"Material" -> "Material(0x000000)"
		),
		"glsl" -> 	Map(
			"int" -> "0",
			"float" -> "0.0",
			"vec2" -> "vec2(0)",
			"vec3" -> "vec3(0)",
			"vec4" -> "vec4(0)",
			"Material" -> "Material(0x000000)"
		)
	)
	
	val sliderdatatypes = LanguageMap(
		"scala" -> "Double",
		"glsl" -> "float"
	)

	val resultfunctions = LanguageMap(
		"scala" -> NodeFunctionFull("result", "(Double, Material)", "(d,m)",
			Seq(
				NodeArgument("d","Double", "0.0"),
				NodeArgument("m","Material", "Material(0x000000)")
			),
			Nil
		),
		"glsl" -> NodeFunctionFull("result", "vec4", "return m;",
			Seq(
				NodeArgument("d","float", "0.0"),
				NodeArgument("m","vec4", "vec4(0)")
			),
			Nil
		)
	)
	

	lazy val nodeCategories:Seq[NodeCategory] = Seq(

		/*FunctionNodeType("Noise", "Noise xyz", Seq("x:Double","y:Double","z:Double"),
			Seq(
				("size", "pow(256,((0.5-s)*2))"),
				("scale", "pow(256,((s-0.5)*2))"),
				("offset", "(s-0.5)*2")),
			Function("noise3xyz" ,"""(noise1(Vec3(x,y,z)*size)+offset)*scale/size""", "Double")),


		FunctionNodeType("Noise", "Rich Noise", Seq("v:Seq[Vec3]","x:Seq[Double]","y:Seq[Double]","z:Seq[Double]","add:Seq[Double]","sub:Seq[Double]"),
			Seq(
				("size", "pow(256,((0.5-s)*2))"),
				("scale", "pow(256,((s-0.5)*2))"),
				("offset", "(s-0.5)*2")),
			Function("richnoise3","""
			val sumv = v.fold[Vec3](Vec3(0))( (a,b) => a+b ) + Vec3(x.sum,y.sum,z.sum)
			(noise1(sumv*size)+offset)*scale/size + add.sum - sub.sum
			""", "Double")),*/





		NodeCategory("Noise",
			Seq(
				NodeType("3D Perlin Noise",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("v","Vec3"),
							NodeArgument("x","Double"),
							NodeArgument("y","Double"),
							NodeArgument("z","Double"),
							NodeArgument("add","Double"),
							NodeArgument("sub","Double")
						),
						"glsl" -> Seq(
							NodeArgument("v","vec3"),
							NodeArgument("x","float"),
							NodeArgument("y","float"),
							NodeArgument("z","float"),
							NodeArgument("add","float"),
							NodeArgument("sub","float")
						)
					),
					Seq(
						NodeSlider("size", "pow(256,((0.5-s)*2))"),
						NodeSlider("scale", "pow(256,((s-0.5)*2))"),
						NodeSlider("offset", "(s-0.5)*2")
					),
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("summedinputnoise3", "Double",
							"""(noise3((v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("summedinputnoise3", "float",
							"""return (noise3((v + vec3(x,y,z))*size)+offset)*scale/size + add - sub;""")
						)
					)
				)
			)
		),

		NodeCategory("Sources",
			Seq(
				NodeType("World coordinates",
					LanguageMap(
						"scala" -> Nil,
						"glsl" -> Nil
					),
					Seq(
						NodeSlider("scale","pow(256,((0.5-s)*2))")
					),
					LanguageMap(
						"scala" -> Map(	
							"v" -> NodeFunction("scalesrcv", "Vec3",   "world   * scale"),
							"x" -> NodeFunction("scalesrcx", "Double", "world.x * scale"),
							"y" -> NodeFunction("scalesrcy", "Double", "world.y * scale"),
							"z" -> NodeFunction("scalesrcz", "Double", "world.z * scale")
						),
						"glsl" -> Map(	
							"v" -> NodeFunction("scalesrcv", "vec3",   "return world.xyz * scale;"),
							"x" -> NodeFunction("scalesrcx", "float", "return world.x * scale;"),
							"y" -> NodeFunction("scalesrcy", "float", "return world.y * scale;"),
							"z" -> NodeFunction("scalesrcz", "float", "return world.z * scale;")
						)
					)
				)
			)
		),
		// King Arthurs Gold
	/*	FunctionNodeType("Material", "Earth", Nil, Nil, Function("matearth", "Material(0x5a3910)", "Material")),
		FunctionNodeType("Material", "Cave",  Nil, Nil, Function("matcave",  "Material(0x10000)", "Material")),
		FunctionNodeType("Material", "Gravel",Nil, Nil, Function("matgravel","Material(0x282828)", "Material")),
		FunctionNodeType("Material", "Stone", Nil, Nil, Function("matstone", "Material(0x373737)", "Material")),
		FunctionNodeType("Material", "Gold",  Nil, Nil, Function("matgold",  "Material(0xfab614)", "Material")),
		FunctionNodeType("Material", "Solid", Nil, Nil, Function("matsolid", "Material(0x1e321e)", "Material")),
		FunctionNodeType("Material", "Wood", Nil, Nil, Function("matwood",  "Material(0x097b11)", "Material"))*/
		NodeCategory("Materials",
			Seq(
				NodeType("Gold",
					LanguageMap("scala" -> Nil, "glsl" -> Nil),	Nil,
					LanguageMap("scala" -> Map( "m" -> NodeFunction("matgold", "Material", "Material(0xfab614);") ),
						"glsl" -> Map("m" -> NodeFunction("matgold", "vec4", "return vec4(0.98, 0.71, 0.08, 0.0);")	)
					)
				),
				NodeType("Stone",
					LanguageMap("scala" -> Nil, "glsl" -> Nil),	Nil,
					LanguageMap("scala" -> Map( "m" -> NodeFunction("matstone", "Material", "Material(0x8e8e8e);") ),
						"glsl" -> Map("m" -> NodeFunction("matstone", "vec4", "return vec4(0.56, 0.56, 0.56, 0.0);")	)
					)
				),
				NodeType("Gravel",
					LanguageMap("scala" -> Nil, "glsl" -> Nil),	Nil,
					LanguageMap("scala" -> Map( "m" -> NodeFunction("matgravel", "Material", "Material(0x4f4f4f);") ),
						"glsl" -> Map("m" -> NodeFunction("matgravel", "vec4", "return vec4(0.31, 0.31, 0.31, 0.0);")	)
					)
				),
				NodeType("Earth",
					LanguageMap("scala" -> Nil, "glsl" -> Nil),	Nil,
					LanguageMap("scala" -> Map( "m" -> NodeFunction("matearth", "Material", "Material(0x5a3910);") ),
						"glsl" -> Map("m" -> NodeFunction("matearth", "vec4", "return vec4(0.35, 0.22, 0.06, 0.0);")	)
					)
				),
				NodeType("Mix Materials",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("m1","Material"),
							NodeArgument("t","Double"),
							NodeArgument("m2","Material")
						),
						"glsl" -> Seq(
							NodeArgument("m1","vec4"),
							NodeArgument("t","float"),
							NodeArgument("m2","vec4")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"m" -> NodeFunction("matthreshold", "Material", "if(t >= 0) m1 else m2;")
						),
						"glsl" -> Map(
							"m" -> NodeFunction("matthreshold", "vec4", "return t >= 0 ? m1 : m2;")
						)
					)
				)
			)
		)


	)
		
/*		NodeCategory("Sources",
			Seq(
				NodeType("Scalable World",
					Nil,
					Seq(
						NodeSlider("scale","pow(256,((0.5-s)*2))")
						),
					Map(	
						"v" -> NodeFunction("scalesrcv", "Vec3",   "world   * scale"),
						"x" -> NodeFunction("scalesrcx", "Double", "world.x * scale"),
						"y" -> NodeFunction("scalesrcy", "Double", "world.y * scale"),
						"z" -> NodeFunction("scalesrcz", "Double", "world.z * scale")
					)
				)
			)
		)*/

/*		

		
		FunctionNodeType("Noise", "Noise v", Seq("v:Vec3=Vec3(0)"),
			Seq(
				("size", "pow(256,((0.5-s)*2))"),
				("scale", "pow(256,((s-0.5)*2))"),
				("offset", "(s-0.5)*2")),
			Function("noise3v" ,"(noise1(v*size)+offset)*scale/size", "Double")),


			
		FunctionNodeType("Combinations","Mix", Seq("x:Double","y:Double"), Seq("mixvalue"),
			Function("mix", "x*(1-mixvalue)+y*mixvalue", "Double")),
			
		FunctionNodeType("Combinations", "Threshold", Seq("x:Double=1","t:Double","y:Double=0"),	Seq(("threshold","(s-0.5)*2")),
			Function("threshold", "if(t > threshold) x else y", "Double")),
			
		FunctionNodeType("Combinations", "Threshold abs", Seq("x:Double=1","t:Double","y:Double=0"), Seq(("threshold","(s-0.5)*2")),
			Function( "absthreshold", "if(abs(t) > threshold) x else y", "Double")),

		FunctionNodeType("Vectors", "Vec3", Seq("x:Seq[Double]","y:Seq[Double]","z:Seq[Double]"),Nil,
			Function("vec3", "Vec3(x.sum,y.sum,z.sum)", "Vec3")),

		FunctionNodeType("Vectors", "Vec3.xyz", Seq("v:Vec3"), Nil,
			Function("vec3x", "v.x", "Double", "x"),
			Function("vec3y", "v.y", "Double", "y"),
			Function("vec3z", "v.z", "Double", "z")),

		FunctionNodeType("Vectors", "Vec3*s",Seq("v:Vec3","s:Double"),Nil, Function("vec3p", "v*s", "Vec3")),

		FunctionNodeType("Vectors", "Vec3*expscale", Seq("v:Vec3"), Seq(("factor","pow(256,((s-0.5)*2))")),
			Function("vec3p", "v*factor", "Vec3")),

		FunctionNodeType("Math", "Min", Seq("xs:Seq[Double]"), Nil, Function("min", "xs.min", "Double")),
		FunctionNodeType("Math", "Max", Seq("xs:Seq[Double]"), Nil, Function("max", "xs.max", "Double")),
		FunctionNodeType("Math", "Product", Seq("xs:Seq[Double]"), Nil, Function("product", "xs.product", "Double")),
		FunctionNodeType("Math", "Sum", Seq("xs:Seq[Double]"), Nil, Function("sum", "xs.sum", "Double")),

		FunctionNodeType("Math", "Div", Seq("x:Double","y:Double=1"), Nil, Function("division", "x/y", "Double")),
		FunctionNodeType("Math", "Diff", Seq("x:Double","y:Double"), Nil, Function("difference", "x-y", "Double")),

		FunctionNodeType("Math", "Exp Clamp 256", Seq("x:Double"), Seq(("maxabs","pow(256,((s-0.5)*2))")),
			Function("expclamp256", "clamp(x,-maxabs,maxabs)", "Double")),
		
		FunctionNodeType("Math", "Squeeze", Seq("x:Double"), Seq(("max","pow(256,((s-0.5)*2))")),
			Function("squeeze", "(if(x<=0) (1/(1-x)-1) else 1/(-1-x)+1)*max", "Double")),

		FunctionNodeType("Math", "Exp Scale 256", Seq("x:Double"), Seq(("factor","pow(256,((s-0.5)*2))")),
			Function("expscale256", "x*factor", "Double")),

		FunctionNodeType("Math", "Constant 1/256..256", Nil, Seq(("constant","pow(256,((s-0.5)*2))")),
			Function("constant256", "constant", "Double")),
		FunctionNodeType("Math", "Constant -1..1", Nil, Seq(("constant","(s-0.5)*2")),
			Function("constantm1" ,"constant", "Double")),

		FunctionNodeType("High Level", "Heart Surface", Seq("v:Vec3"),
			Seq(
				("scale","pow(10,2*s-2.5)"),
				("xshift","(s-0.5)*20"),
				("yshift","(s-0.5)*20"),
				("zshift","(s-0.5)*20")),
			Function("heart", "val input = (Vec3(v.x,-v.y,v.z)*scale + Vec3(xshift, yshift, zshift)).xzy; import input._; -(pow(pow(x,2)+9/4*pow(y,2)+pow(z,2)-1,3)-pow(x,2)*pow(z,3)-9/80*pow(y,2)*pow(z,3))", "Double")),

		FunctionNodeType("Math", "Root", Seq("a:Double"), Seq(("degree","1/pow(256, s)")),
			Function("root", "sign(a)*pow(abs(a), degree)", "Double")),
		
		FunctionNodeType("Material", "Material Red", Nil, Nil, Function("matred", "Material(0xFF0000)", "Material")),
		FunctionNodeType("Material", "Material Green", Nil, Nil, Function("matgreen", "Material(0x00FF00)", "Material")),
		FunctionNodeType("Material", "Material Blue", Nil, Nil, Function("matblue", "Material(0x0000FF)", "Material")),
		FunctionNodeType("Material", "Material Yellow", Nil, Nil, Function("matyellow", "Material(0xFFFF00)", "Material")),
		FunctionNodeType("Material", "Material RGB", Nil,
			Seq(("r","s*255"),("g","s*255"),("b","s*255")),
			Function("matrgb", "Material(r.toInt << 16 | g.toInt << 8 | b.toInt)", "Material")),


		FunctionNodeType("Material", "Material Threshold", Seq("x:Material=Material(0xFFFFFF)","t:Double","y:Material=Material(0)"),
			Seq(("threshold","(s-0.5)*2")),
			Function("matthreshold", "if(t > threshold) x else y", "Material")),
		
		FunctionNodeType("Sources", "Scalable World", Nil, Seq(("scale","pow(256,((0.5-s)*2))")),
			Function("scalesrcv", "world * scale", "Vec3", "v"),
			Function("scalesrcx", "world.x * scale", "Double", "x"),
			Function("scalesrcy", "world.y * scale", "Double", "y"),
			Function("scalesrcz", "world.z * scale", "Double", "z")),
			
		FunctionNodeType("Sources", "World", Nil, Nil,
			Function("srcv", "world", "Vec3", "v"),
			Function("srcxyzx", "world.x", "Double", "x"),
			Function("srcxyzy", "world.y", "Double", "y"),
			Function("srcxyzz", "world.z", "Double", "z")),

		
		// King Arthurs Gold
		FunctionNodeType("Material", "Earth", Nil, Nil, Function("matearth", "Material(0x5a3910)", "Material")),
		FunctionNodeType("Material", "Cave",  Nil, Nil, Function("matcave",  "Material(0x10000)", "Material")),
		FunctionNodeType("Material", "Gravel",Nil, Nil, Function("matgravel","Material(0x282828)", "Material")),
		FunctionNodeType("Material", "Stone", Nil, Nil, Function("matstone", "Material(0x373737)", "Material")),
		FunctionNodeType("Material", "Gold",  Nil, Nil, Function("matgold",  "Material(0xfab614)", "Material")),
		FunctionNodeType("Material", "Solid", Nil, Nil, Function("matsolid", "Material(0x1e321e)", "Material")),
		FunctionNodeType("Material", "Wood", Nil, Nil, Function("matwood",  "Material(0x097b11)", "Material"))
		
		
	)
*/
	def export(composition:Composition, language:String):String = {
		language match {
			case "scala" =>
				import composition._
		
				// Function Definitions
				val functioncode = (for( NodeFunctionFull(name,returntype, code, arguments, sliders) <- functions("scala") ) yield {
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
						function("scala").name,
						(
							arguments.map{
								case Right(arg) => arg.varname
								case Left(default) => default("scala") }
							++
							sliders.map(_.globalvalue)
						).mkString(", ")
					)
					// Move on with all arguments referring to other functions
					nexttrees ++= currenttree.arguments.collect{case Right(x) => x}.distinct
				}
				val functioncallcode = functioncalls.distinct.mkString("\n")
		
		
				// Final Value
				val returnvalue = calltree.varname
		
				"(world:Vec3) => {\n%s\n\n%s\n\n%s\n}\n".format(
					functioncode, functioncallcode, returnvalue
				)


			case "glsl" =>
				import composition._
		
				// Function Definitions
				val functioncode = (for( NodeFunctionFull(name,returntype, code, arguments, sliders) <- functions("glsl") ) yield {
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
						function("glsl").returntype,
						varname,
						function("glsl").name,
						(
							arguments.map{
								case Right(arg) => arg.varname
								case Left(default) => default("glsl") }
							++
							sliders.map(_.globalvalue)
						).mkString(", ")
					)
					// Move on with all arguments referring to other functions
					nexttrees ++= currenttree.arguments.collect{case Right(x) => x}.distinct
				}
				val functioncallcode = functioncalls.distinct.mkString("\n")
		
		
				// Final Value
				val returnvalue = calltree.varname

"""#version 120
#extension GL_EXT_gpu_shader4 : enable
//#extension GL_ARB_gpu_shader_fp64 : enable

varying vec3 vertex;
varying vec3 normal;
varying vec4 world;

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

int seed = 0;
int a = (seed ^ int(0xB5C18E6A)) | ((1 << 16) + 1);
int c = seed ^ int(0xF292D0B2);
int hash(int x){ return (a*(x ^ c)) >> 16; }

int fastfloor(float x) { return int( x > 0 ? x : x-1); }
float fade(float t) { return t * t * t * (t * (t * 6 - 15) + 10); }
float lerp(float t, float a, float b) { return a + t * (b - a); }

float grad(int hash, float x, float y, float z) {
      int h = hash & 0xF;
      float u = h<8 ? x : y,
             v = h<4 ? y : h==12||h==14 ? x : z;
      return ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
}

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


""".format(
					functioncode, functioncallcode, returnvalue
				)
		}
	}

}
