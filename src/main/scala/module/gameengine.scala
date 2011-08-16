package noiseeditor.modules

import noiseeditor.util._
import noiseeditor.datastructure._

import noiseeditor.{Preview, Module}
import noiseeditor.manager.CompositionManager
import noiseeditor.connector.OutConnector

//TODO: More High Level nodes, like Surface, Layers, Fractal Noise, Turbulence
//TODO: More Noise types, like cellular noise
//TODO: Different Noise Dimensions

object GameEngine extends Module {
	
	override val exporttypes = Seq("engine", "scala_density", "scala_material", "glsl_material", "prediction")
	override val languages = Seq("scala", "glsl", "prediction")

	override val scalainitcode = """
		import noise.Noise.noise3
	"""
	
	override val typedefaults = LanguageMap(
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
		),
		"prediction" -> 	Map(
			"Interval" -> "Interval(0,0)",
			"Volume" -> "Volume(Vec3(0),Vec3(0))"
		)
	)
	
	val sliderdatatypes = LanguageMap(
		"scala"      -> "Double",
		"glsl"       -> "float",
		"prediction" -> "Double"
	)


	override lazy val nodecategories:Seq[NodeCategory] = Seq(

		NodeCategory("Sources",
			Seq(
				NodeType("World coordinates",
					LanguageMap(
						"scala" -> Nil,
						"glsl" -> Nil,
						"prediction" -> Nil
					),
					Seq(
						NodeSlider("scale","pow(256, 1-s*2)")
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
						),
						"prediction" -> Map(	
							"v" -> NodeFunction("scalesrcv", "Volume",   "world   * scale"),
							"x" -> NodeFunction("scalesrcx", "Interval", "world.x * scale"),
							"y" -> NodeFunction("scalesrcy", "Interval", "world.y * scale"),
							"z" -> NodeFunction("scalesrcz", "Interval", "world.z * scale")
						)
					)
				),
				NodeType("Time",
					LanguageMap(
						"scala" -> Nil,
						"glsl" -> Nil,
						"prediction" -> Nil
					),
					Nil,
					LanguageMap(
						"scala" -> Map(	
							"s" -> NodeFunction("timeseconds", "Double",   "System.currentTimeMillis.toDouble/1000.0")
						),
						"glsl" -> Map(	
							"s" -> NodeFunction("timeseconds", "float",   "return time;")
						)
					)
				)
			)
		),

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
						),
						"prediction" -> Seq(
							NodeArgument("v","Volume"),
							NodeArgument("x","Interval"),
							NodeArgument("y","Interval"),
							NodeArgument("z","Interval"),
							NodeArgument("add","Interval"),
							NodeArgument("sub","Interval")
						)
					),
					Seq(
						NodeSlider("size", "pow(256, 1-s*2)"),
						NodeSlider("scale", "pow(256, s*2-1)"),
						NodeSlider("offset", "s*2-1")
					),
					//TODO: transpose language and function map to reduce errors when writing definitions?
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("perlinnoise3", "Double",
							"""(noise3((v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("perlinnoise3", "float",
							"""return (noise3((v + vec3(x,y,z))*size)+offset)*scale/size + add - sub;""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("perlinnoise3", "Interval",
							"""(noise3_prediction((v + Volume(x,y,z))*size)+offset)*scale/size + add - sub""")
						)
					)
				)
			)
		),

		NodeCategory("Math",
			Seq(
				NodeType("Min 2",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("a","Double"),
							NodeArgument("b","Double")
						),
						"glsl" -> Seq(
							NodeArgument("a","float"),
							NodeArgument("b","float")
						),
						"prediction" -> Seq(
							NodeArgument("a","Interval"),
							NodeArgument("b","Interval")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("min2", "Double",
							"""min(a,b)""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("min2", "float",
							"""return min(a,b);""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("min2", "Interval",
							"""interval.min(a,b)""")
						)
					)
				),
				NodeType("Min 3",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("a","Double"),
							NodeArgument("b","Double"),
							NodeArgument("c","Double")
						),
						"glsl" -> Seq(
							NodeArgument("a","float"),
							NodeArgument("b","float"),
							NodeArgument("c","float")
						),
						"prediction" -> Seq(
							NodeArgument("a","Interval"),
							NodeArgument("b","Interval"),
							NodeArgument("c","Interval")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("min3", "Double",
							"""min(min(a,b),c)""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("min3", "float",
							"""return min(min(a,b),c);""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("min3", "Interval",
							"""interval.min(interval.min(a,b),c)""")
						)
					)
				),
				NodeType("Max 2",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("a","Double"),
							NodeArgument("b","Double")
						),
						"glsl" -> Seq(
							NodeArgument("a","float"),
							NodeArgument("b","float")
						),
						"prediction" -> Seq(
							NodeArgument("a","Interval"),
							NodeArgument("b","Interval")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("max2", "Double",
							"""max(a,b)""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("max2", "float",
							"""return max(a,b);""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("max2", "Interval",
							"""interval.max(a,b)""")
						)
					)
				),
				NodeType("Max 3",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("a","Double"),
							NodeArgument("b","Double"),
							NodeArgument("c","Double")
						),
						"glsl" -> Seq(
							NodeArgument("a","float"),
							NodeArgument("b","float"),
							NodeArgument("c","float")
						),
						"prediction" -> Seq(
							NodeArgument("a","Interval"),
							NodeArgument("b","Interval"),
							NodeArgument("c","Interval")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("max3", "Double",
							"""max(max(a,b),c)""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("max3", "float",
							"""return max(max(a,b),c);""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("max3", "Interval",
							"""interval.max(interval.max(a,b),c)""")
						)
					)
				),
				NodeType("Sum 2",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("a","Double"),
							NodeArgument("b","Double")
						),
						"glsl" -> Seq(
							NodeArgument("a","float"),
							NodeArgument("b","float")
						),
						"prediction" -> Seq(
							NodeArgument("a","Interval"),
							NodeArgument("b","Interval")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("sum2", "Double",
							"""a+b""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("sum2", "float",
							"""return a+b;""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("sum2", "Interval",
							"""a+b""")
						)
					)
				),
				NodeType("Sum 3",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("a","Double"),
							NodeArgument("b","Double"),
							NodeArgument("c","Double")
						),
						"glsl" -> Seq(
							NodeArgument("a","float"),
							NodeArgument("b","float"),
							NodeArgument("c","float")
						),
						"prediction" -> Seq(
							NodeArgument("a","Interval"),
							NodeArgument("b","Interval"),
							NodeArgument("c","Interval")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("sum3", "Double",
							"""a+b+c""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("sum3", "float",
							"""return a+b+c;""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("sum3", "Interval",
							"""a+b+c""")
						)
					)
				),
				NodeType("Product 2",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("a","Double", "1.0"),
							NodeArgument("b","Double", "1.0")
						),
						"glsl" -> Seq(
							NodeArgument("a","float", "1.0"),
							NodeArgument("b","float", "1.0")
						),
						"prediction" -> Seq(
							NodeArgument("a","Interval","Interval(1,1)"),
							NodeArgument("b","Interval","Interval(1,1)")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("product2", "Double",
							"""a*b""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("product2", "float",
							"""return a*b;""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("product2", "Interval",
							"""a*b""")
						)
					)
				),
				NodeType("Product 3",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("a","Double", "1.0"),
							NodeArgument("b","Double", "1.0"),
							NodeArgument("c","Double", "1.0")
						),
						"glsl" -> Seq(
							NodeArgument("a","float", "1.0"),
							NodeArgument("b","float", "1.0"),
							NodeArgument("c","float", "1.0")
						),
						"prediction" -> Seq(
							NodeArgument("a","Interval","Interval(1,1)"),
							NodeArgument("b","Interval","Interval(1,1)"),
							NodeArgument("c","Interval","Interval(1,1)")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("product3", "Double",
							"""a*b*c""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("product3", "float",
							"""return a*b*c;""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("product3", "Interval",
							"""a*b*c""")
						)
					)
				),
				NodeType("Diff 2",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("a","Double"),
							NodeArgument("b","Double")
						),
						"glsl" -> Seq(
							NodeArgument("a","float"),
							NodeArgument("b","float")
						),
						"prediction" -> Seq(
							NodeArgument("a","Interval"),
							NodeArgument("b","Interval")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("diff2", "Double",
							"""a-b""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("diff2", "float",
							"""return a-b;""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("diff2", "Interval",
							"""a-b""")
						)
					)
				),
				NodeType("Negate",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("a","Double")
						),
						"glsl" -> Seq(
							NodeArgument("a","float")
						),
						"prediction" -> Seq(
							NodeArgument("a","Interval")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("negate", "Double",
							"""-a""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("negate", "float",
							"""return -a;""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("negate", "Interval",
							"""-a""")
						)
					)
				),
				NodeType("Constant Exp",
					LanguageMap(
						"scala" -> Nil,
						"glsl" -> Nil,
						"prediction" -> Nil
					),
					Seq(
						NodeSlider("value", "pow(256, s*2-1)")
					),
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("constantexp", "Double",
							"""value""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("constantexp", "float",
							"""return value;""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("constantexp", "Interval",
							"""Interval(value)""")
						)
					)
				),
				NodeType("Constant -1..1",
					LanguageMap(
						"scala" -> Nil,
						"glsl" -> Nil,
						"prediction" -> Nil
					),
					Seq(
						NodeSlider("value", "s*2-1")
					),
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("constant1", "Double",
							"""value""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("constant1", "float",
							"""return value;""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("constant1", "Interval",
							"""Interval(value)""")
						)
					)
				),
				NodeType("Add Exp Constant",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("a","Double")
						),
						"glsl" -> Seq(
							NodeArgument("a","float")
						),
						"prediction" -> Seq(
							NodeArgument("a","Interval")
						)
					),
					Seq(
						NodeSlider("value", "val s1 = (s*2-1); if(s1 >= 0) pow(257, s1)-1 else 1-pow(257, -s1)")
					),
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("addconstantexp", "Double",
							"""a+value""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("addconstantexp", "float",
							"""return a+value;""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("addconstantexp", "Interval",
							"""a+value""")
						)
					)
				),
				NodeType("Multiply Exp Constant",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("a","Double")
						),
						"glsl" -> Seq(
							NodeArgument("a","float")
						),
						"prediction" -> Seq(
							NodeArgument("a","Interval")
						)
					),
					Seq(
						NodeSlider("value", "pow(256, s*2-1)")
					),
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("multiplyconstantexp", "Double",
							"""a*value""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("multiplyconstantexp", "float",
							"""return a*value;""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("multiplyconstantexp", "Interval",
							"""a*value""")
						)
					)
				),
				NodeType("Scale Vec3",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("v","Vec3")
						),
						"glsl" -> Seq(
							NodeArgument("v","vec3")
						),
						"prediction" -> Seq(
							NodeArgument("v","Volume")
						)
					),
					Seq(
						NodeSlider("x", "pow(256, s*2-1)"),
						NodeSlider("y", "pow(256, s*2-1)"),
						NodeSlider("z", "pow(256, s*2-1)")
					),
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("scalevec3", "Vec3",
							"""v*Vec3(x,y,z)""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("scalevec3", "vec3",
							"""return v*vec3(x,y,z);""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("scalevec3", "Volume",
							"""v*Volume(Vec3(x,y,z))""")
						)
					)
				),
				NodeType("Sphere",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("v","Vec3")
						),
						"glsl" -> Seq(
							NodeArgument("v","vec3")
						),
						"prediction" -> Seq(
							NodeArgument("v","Volume")
						)
					),
					Seq(
						NodeSlider("radius", "pow(256, s*2-1)")
					),
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("sphere", "Double",
							"""radius - sqrt(dot(v,v))""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("sphere", "float",
							"""return radius - sqrt(dot(v,v));""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("sphere", "Interval",
							"""-interval.length(v) + radius""")
						)
					)
				),
				NodeType("Z-Rotation",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("v","Vec3")
						),
						"glsl" -> Seq(
							NodeArgument("v","vec3")
						),
						"prediction" -> Seq(
							NodeArgument("v","Volume")
						)
					),
					Seq(
						NodeSlider("angle", "(s*2-1)*Pi")
					),
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("rotate", "Vec3",
							"""Mat3(Mat3x4 rotateZ angle) * v""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("rotate", "vec3",
							"""Mat3(Mat3x4 rotateZ angle) * v""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("rotate", "Volume",
							"""Mat3(Mat3x4 rotateZ angle) * v""")
						)
					)
				),
				NodeType("Vec3",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("x","Double"),
							NodeArgument("y","Double"),
							NodeArgument("z","Double")
						),
						"glsl" -> Seq(
							NodeArgument("x","float"),
							NodeArgument("y","float"),
							NodeArgument("z","float")
						),
						"prediction" -> Seq(
							NodeArgument("x","Interval"),
							NodeArgument("y","Interval"),
							NodeArgument("z","Interval")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"v" -> NodeFunction("createvec3", "Vec3",
							"""Vec3(x,y,z)""")
						),
						"glsl" -> Map(
							"v" -> NodeFunction("createvec3", "vec3",
							"""return vec3(x,y,z);""")
						),
						"prediction" -> Map(
							"v" -> NodeFunction("createvec3", "Volume",
							"""Volume(x,y,z)""")
						)
					)
				),
				NodeType("Extract Vec3",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("v","Vec3")
						),
						"glsl" -> Seq(
							NodeArgument("v","vec3")
						),
						"prediction" -> Seq(
							NodeArgument("v","Volume")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"x" -> NodeFunction("vec3x", "Double","v.x"),
							"y" -> NodeFunction("vec3y", "Double","v.y"),
							"z" -> NodeFunction("vec3z", "Double","v.z")
						),
						"glsl" -> Map(
							"x" -> NodeFunction("vec3x", "float","v.x"),
							"y" -> NodeFunction("vec3y", "float","v.y"),
							"z" -> NodeFunction("vec3z", "float","v.z")
						),
						"prediction" -> Map(
							"x" -> NodeFunction("vec3x", "Interval","v.x"),
							"y" -> NodeFunction("vec3y", "Interval","v.y"),
							"z" -> NodeFunction("vec3z", "Interval","v.z")
						)
					)
				)			
			)
		),

		NodeCategory("Materials",
			Seq(
				NodeType("RGB",
					LanguageMap("scala" -> Nil, "glsl" -> Nil),
					Seq(NodeSlider("r"), NodeSlider("g"), NodeSlider("b")),
					LanguageMap("scala" -> Map( "m" -> NodeFunction("matrgb", "Material", "Material((r*255).toInt << 16 | (g*255).toInt << 8 | (b*255).toInt)") ),
						"glsl" -> Map("m" -> NodeFunction("matrgb", "vec4", "return vec4(r, g, b, 0.0);")	)
					)
				),
				NodeType("Gold",
					LanguageMap("scala" -> Nil, "glsl" -> Nil),	Nil,
					LanguageMap("scala" -> Map( "m" -> NodeFunction("matgold", "Material", "Material(0xfab614)") ),
						"glsl" -> Map("m" -> NodeFunction("matgold", "vec4", "return vec4(0.98, 0.71, 0.08, 0.0);")	)
					)
				),
				NodeType("Stone",
					LanguageMap("scala" -> Nil, "glsl" -> Nil),	Nil,
					LanguageMap("scala" -> Map( "m" -> NodeFunction("matstone", "Material", "Material(0x8e8e8e)") ),
						"glsl" -> Map("m" -> NodeFunction("matstone", "vec4", "return vec4(0.56, 0.56, 0.56, 0.0);")	)
					)
				),
				NodeType("Gravel",
					LanguageMap("scala" -> Nil, "glsl" -> Nil),	Nil,
					LanguageMap("scala" -> Map( "m" -> NodeFunction("matgravel", "Material", "Material(0x4f4f4f)") ),
						"glsl" -> Map("m" -> NodeFunction("matgravel", "vec4", "return vec4(0.31, 0.31, 0.31, 0.0);")	)
					)
				),
				NodeType("Earth",
					LanguageMap("scala" -> Nil, "glsl" -> Nil),	Nil,
					LanguageMap("scala" -> Map( "m" -> NodeFunction("matearth", "Material", "Material(0x5a3910)") ),
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
					Seq(
						NodeSlider("shift", "val s1 = (s*2-1); if(s1 >= 0) pow(257, s1)-1 else 1-pow(257, -s1)")
					),
					LanguageMap(
						"scala" -> Map(
							"m" -> NodeFunction("matmix", "Material", "if(t >= shift) m1 else m2")
						),
						"glsl" -> Map(
							"m" -> NodeFunction("matmix", "vec4", "return t >= shift ? m1 : m2;")
						)
					)
				),
				NodeType("Blend Materials (not working yet)",
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
							"m" -> NodeFunction("matthreshold", "Material",
								"""Material(
									((m1.color >> 16)*clamp(t,0,1) + (m2.color >> 16)*clamp(1-t,0,1)).toInt << 16)""")
						),
						"glsl" -> Map(
							"m" -> NodeFunction("matthreshold", "vec4", "return ;")
						)
					)
				),
				NodeType("Switch 2 Materials",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("m1","Material"),
							NodeArgument("t1","Double"),
							NodeArgument("t2","Double"),
							NodeArgument("m2","Material")
						),
						"glsl" -> Seq(
							NodeArgument("m1","vec4"),
							NodeArgument("t1","float"),
							NodeArgument("t2","float"),
							NodeArgument("m2","vec4")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"m" -> NodeFunction("matswitch2", "Material", "if(t1 >= t2) m1 else m2;")
						),
						"glsl" -> Map(
							"m" -> NodeFunction("matswitch2", "vec4", "return t1 >= t2 ? m1 : m2;")
						)
					)
				)

			)
		)


	)
		
/*		FunctionNodeType("Combinations","Mix", Seq("x:Double","y:Double"), Seq("mixvalue"),
			Function("mix", "x*(1-mixvalue)+y*mixvalue", "Double")),
			
		FunctionNodeType("Combinations", "Threshold", Seq("x:Double=1","t:Double","y:Double=0"),	Seq(("threshold","(s-0.5)*2")),
			Function("threshold", "if(t > threshold) x else y", "Double")),
			
		FunctionNodeType("Combinations", "Threshold abs", Seq("x:Double=1","t:Double","y:Double=0"), Seq(("threshold","(s-0.5)*2")),
			Function( "absthreshold", "if(abs(t) > threshold) x else y", "Double")),

		FunctionNodeType("Vectors", "Vec3*s",Seq("v:Vec3","s:Double"),Nil, Function("vec3p", "v*s", "Vec3")),

		FunctionNodeType("Vectors", "Vec3*expscale", Seq("v:Vec3"), Seq(("factor","pow(256,((s-0.5)*2))")),
			Function("vec3p", "v*factor", "Vec3")),

		FunctionNodeType("Math", "Exp Clamp 256", Seq("x:Double"), Seq(("maxabs","pow(256,((s-0.5)*2))")),
			Function("expclamp256", "clamp(x,-maxabs,maxabs)", "Double")),
		
		FunctionNodeType("Math", "Squeeze", Seq("x:Double"), Seq(("max","pow(256,((s-0.5)*2))")),
			Function("squeeze", "(if(x<=0) (1/(1-x)-1) else 1/(-1-x)+1)*max", "Double")),

		FunctionNodeType("High Level", "Heart Surface", Seq("v:Vec3"),
			Seq(
				("scale","pow(10,2*s-2.5)"),
				("xshift","(s-0.5)*20"),
				("yshift","(s-0.5)*20"),
				("zshift","(s-0.5)*20")),
			Function("heart", "val input = (Vec3(v.x,-v.y,v.z)*scale + Vec3(xshift, yshift, zshift)).xzy; import input._; -(pow(pow(x,2)+9/4*pow(y,2)+pow(z,2)-1,3)-pow(x,2)*pow(z,3)-9/80*pow(y,2)*pow(z,3))", "Double")),

		FunctionNodeType("Math", "Root", Seq("a:Double"), Seq(("degree","1/pow(256, s)")),
			Function("root", "sign(a)*pow(abs(a), degree)", "Double")),
		
		
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

					/*import FileChooser.Result._
					import FileManager.chooser
					val oldselectedfile = chooser.selectedFile
				
					chooser.title = "Export: " + exportcombobox.selected
					//chooser.setExtensionFilter("Scala function", "scala")
					chooser.selectedFile = null
					chooser.showSaveDialog match {
						case Approve =>
							val out = new java.io.FileWriter(chooser.selectedFile)
							out.write(ModuleManager.export(CodeGenerator.composition(outconnectors(0)), exportcombobox.selected))
							out.close
						case Cancel =>
					}
					chooser.selectedFile = oldselectedfile*/

	override def export(preview:Preview, exporttype:String) {
		def generatescaladensitycode  = generatescalacode("density", "0.0", preview.connectedoutconnector("d"))
		def generatescalamaterialcode = generatescalacode("material", "Material()", preview.connectedoutconnector("m"))
		def generateglslmaterialcode  = generateglslcode (preview.connectedoutconnector("m"))
		def generatepredictiondensitycode = generatepredictioncode(preview.connectedoutconnector("d"))

		try {
			exporttype match {
				case "scala_density" =>
					println(generatescaladensitycode)
				
				case "scala_material" =>
					println(generatescalamaterialcode)

				case "glsl_material" =>
					println(generateglslmaterialcode)

				case "prediction" =>
					println(generatepredictiondensitycode)

				case "engine" =>
					println("Exporting to Engine:")
					
					var path = "../gameengine"
					
					println("clearing world cache...")
					Runtime.getRuntime.exec("rm " + path + "/worldoctree")

					println("generating scala density code...")
					var out = new java.io.FileWriter(path + "/src/main/scala/worldfunction.scala")
					out.write(generatescaladensitycode)
					out.close

					println("generating glsl code...")
					out = new java.io.FileWriter(path + "/src/main/resources/shaders/screen.frag")
					out.write(generateglslmaterialcode)
					out.close
				
					println("generating prediction code...")
					out = new java.io.FileWriter(path + "/src/main/scala/worldprediction.scala")
					out.write(generatepredictiondensitycode)
					out.close
					println("done")
			}
		}
		catch {
			case e:Exception =>
			e.printStackTrace()
		}
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
package xöpäx.gen

import noise.Noise.noise3

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

case class Material(color:Int = 0x000000)

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
package xöpäx.gen

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import noise.Noise.noise3_prediction
import noise.interval
import noise.interval.{Interval, Volume}

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
