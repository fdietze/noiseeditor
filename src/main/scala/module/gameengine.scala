package noiseeditor.modules

import noiseeditor.datastructure._

import noiseeditor.datastructure.NodeArgument
import noiseeditor.datastructure.NodeCategory
import noiseeditor.datastructure.NodeFunction
import noiseeditor.datastructure.NodeSlider
import noiseeditor.manager.{InterpreterManager, CompositionManager, NodeManager}
import noiseeditor.{Material, Preview, Module}

import GameEngineExports._
import simplex3d.math.doublex.functions
import scala.util.matching.Regex
import scala.util.matching.Regex.{Groups, Match}
import simplex3d.math.double._
import scala.Some
import java.awt.image.BufferedImage
import java.awt.Color
import javax.imageio.ImageIO
import java.io.File

import noiseeditor.util.{nextPowerOfTwo,hexStringtoInt,red,green,blue}
import scala.Some

//TODO: More High Level nodes, like Surface, Layers, Fractal Noise, Turbulence
//TODO: More Noise types, like cellular noise
//TODO: Different Noise Dimensions

object GameEngine extends Module {
	
	override val exporttypes = Seq("engine", "scala_density", "scala_material", "xml_materials", "glsl_material", "prediction")
	override val languages = Seq("scala", "glsl", "prediction")

	override val scalainitcode = """
                                 |import noise.Noise.{noise3 => perlinNoise3}
                                 |import noise.Worley.{cellnoise => worleyNoise3}
                                 |import noise.split.splitNoise3
                                 |//for compatibility:
                                 |import noise.Noise.noise3
                                 |import noise.Worley.cellnoise
                                 |
                                 |                               """.stripMargin
	
	override val typedefaults = LanguageMap(
		"scala" -> 	Map(
			"Int" -> "0",
			"Double" -> "0.0",
			"Seq" -> "Nil",
			"Vec2" -> "Vec2(0)",
			"Vec3" -> "Vec3(0)",
			"Vec4" -> "Vec4(0)",
			"Material" -> "Material()"
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
      "Interval3" -> "Interval3(Vec3(0),Vec3(0))",
      "Interval4" -> "Interval4(Vec4(0),Vec4(0))",
      //for compatibility:
      "Volume" -> "Interval3(Vec3(0),Vec3(0))",
      "Interval4D" -> "Interval4(Vec4(0),Vec4(0))",
			"Material" -> "Material()"
		),
		"bounds" -> 	Map(
			"Interval" -> "Interval(0,0)",
			"Interval3" -> "Interval3(Vec3(0),Vec3(0))",
			"Interval4" -> "Interval4(Vec4(0),Vec4(0))",
			"Material" -> "Material()"
		)
	)

	val sliderdatatypes = LanguageMap(
		"scala"      -> "Double",
		"glsl"       -> "float",
		"prediction" -> "Double",
		"bounds" -> "Double"
	)


	override lazy val nodecategories:Seq[NodeCategory] = Seq(

//******************************************************************************
//******************* Sources **************************************************
//******************************************************************************
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
							"v" -> NodeFunction("scalesrcv", "Interval3",   "world   * scale"),
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
							"s" -> NodeFunction("timeseconds", "Double",   "InterpreterUptime")
						),
						"glsl" -> Map(
							"s" -> NodeFunction("timeseconds", "float",   "return time;")
						)
					)
				),
				NodeType("Exponential slider: Scalar",
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
							"o" -> NodeFunction("expconstant", "Double",
							"""value""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("expconstant", "float",
							"""return value;""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("expconstant", "Interval",
							"""Interval(value)""")
						)
					)
				),
				NodeType("Linear slider: Scalar",
					LanguageMap(
						"scala" -> Nil,
						"glsl" -> Nil,
						"prediction" -> Nil
					),
					Seq(
						NodeSlider("value", "s*2-1"),
						NodeSlider("scale", "pow(256, s*2-1)")
					),
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("linearconstant", "Double",
							"""value*scale""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("linearconstant", "float",
							"""return value*scale;""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("linearconstant", "Interval",
							"""Interval(value*scale)""")
						)
					)
				),
				NodeType("Exponential slider: Vec3",
					LanguageMap(
						"scala" -> Nil,
						"glsl" -> Nil,
						"prediction" -> Nil
					),
					Seq(
						NodeSlider("x", "pow(256, s*2-1)"),
						NodeSlider("y", "pow(256, s*2-1)"),
						NodeSlider("z", "pow(256, s*2-1)")
					),
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("expconstantvec3", "Vec3",
							"""Vec3(x,y,z)""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("expconstantvec3", "vec3",
							"""return vec3(x,y,z);""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("expconstantvec3", "Interval3",
							"""Interval3(Vec3(x,y,z))""")
						)
					)
				),
				NodeType("Linear slider: Vec3",
					LanguageMap(
						"scala" -> Nil,
						"glsl" -> Nil,
						"prediction" -> Nil
					),
					Seq(
						NodeSlider("x", "s*2-1"),
						NodeSlider("y", "s*2-1"),
						NodeSlider("z", "s*2-1"),
						NodeSlider("scale", "pow(256, s*2-1)")
					),
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("linearconstantvec3", "Vec3",
							"""Vec3(x,y,z)*scale""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("linearconstantvec3", "vec3",
							"""return vec3(x,y,z)*scale;""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("linearconstantvec3", "Interval3",
							"""Interval3(Vec3(x,y,z)*scale)""")
						)
					)
				)
			)
		),
//******************************************************************************
//******************* Noise ****************************************************
//******************************************************************************

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
              NodeArgument("v","Interval3"),
              NodeArgument("x","Interval"),
              NodeArgument("y","Interval"),
              NodeArgument("z","Interval"),
              NodeArgument("add","Interval"),
              NodeArgument("sub","Interval")
            ),
            "bounds" -> Seq(
              NodeArgument("v","Interval3"),
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

					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("perlinnoise3", "Double",
							"""((perlinNoise3(v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("perlinnoise3", "float",
							"""return ((perlinNoise3(v + vec3(x,y,z))*size)+offset)*scale/size + add - sub;""")
						),
            "prediction" -> Map(
              "o" -> NodeFunction("perlinnoise3", "Interval",
                """((perlinNoise3Prediction(v + Interval3(x,y,z))*size)+offset)*scale/size + add - sub""")
            ),
            "bounds" -> Map(
              "o" -> NodeFunction("perlinnoise3", "Interval",
                """((perlinNoise3Bounds(v + Interval3(x,y,z))*size)+offset)*scale/size + add - sub""")
            )
					)
				),

				NodeType("3D Perlin Noise Sum",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("v","Vec3")
						),
						"glsl" -> Seq(
							NodeArgument("v","vec3")
						),
            "prediction" -> Seq(
              NodeArgument("v","Interval3")
            ),
            "bounds" -> Seq(
              NodeArgument("v","Interval3")
            )
					),
					Seq(
						NodeSlider("size", "pow(256, 1-s*2)"),
						NodeSlider("scale", "pow(256, s*2-1)"),
						NodeSlider("offset", "s*2-1"),
						NodeSlider("steps", "1+(s*10).floor"),
						NodeSlider("factor", "1+s*2", 50)
					),
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("perlinnoise3sum", "Double",
							"""
val pos = v*size
var sum = 0.0
for(i <- 0 until steps.toInt) {
	val f = pow(factor,i)
	sum += perlinNoise3(pos*f)/f
}
(sum+offset)*scale/size""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("perlinnoise3sum", "float",
							"""
vec3 pos = v*size;
float res = 0.0;
int intsteps = int(steps);
for(int i = 0; i < intsteps; ++i) {
	float f = pow(factor,i);
	res += perlinNoise3(pos*f)/f;
}
return (res+offset)*scale/size;""")
						),
            "prediction" -> Map(
              "o" -> NodeFunction("perlinnoise3sum", "Interval",
                """
val pos = v*size
var sum = Interval(0.0)
for(i <- 0 until steps.toInt) {
	val f = pow(factor,i)
	sum += perlinNoise3Prediction(pos*f)/f
}
(sum+offset)*scale/size""")
            ),
  "bounds" -> Map(
    "o" -> NodeFunction("perlinnoise3sum", "Interval",
      """
val pos = v*size
var sum = Interval(0.0)
for(i <- 0 until steps.toInt) {
	val f = pow(factor,i)
	sum += perlinNoise3Bounds(pos*f)/f
}
(sum+offset)*scale/size""")
  )
					)
				),


        NodeType("3D Worley Noise",
          LanguageMap(
            "scala" -> Seq(
              NodeArgument("v","Vec3")
            ),
            "prediction" -> Seq(
              NodeArgument("v","Interval3")
            ),
            "bounds" -> Seq(
              NodeArgument("v","Interval3")
            )
          ),
          Nil,
          LanguageMap(
            "scala" -> Map(
              "v4" -> NodeFunction("worleynoise3", "Vec4",
                """worleyNoise3(v)""")
            ),
            "prediction" -> Map(
              "v4" -> NodeFunction("worleynoise3", "Interval4",
                """worleyNoise3Prediction(v)""")
            ),
            "bounds" -> Map(
              "v4" -> NodeFunction("worleynoise3", "Interval4",
                """worleyNoise3Bounds(v)""")
            )
          )
        ),

          NodeType("3D Split Noise",
          LanguageMap(
            "scala" -> Seq(
              NodeArgument("v","Vec3"),
              NodeArgument("a","Double", "-world.z"),
              NodeArgument("ma", "Material"),
              NodeArgument("b","Double", "-world.z"),
              NodeArgument("mb", "Material"),
              NodeArgument("c","Double", "-world.z"),
              NodeArgument("mc", "Material"),
              NodeArgument("d","Double", "-world.z"),
              NodeArgument("md", "Material")
            ),
            "prediction" -> Seq(
              NodeArgument("v","Interval3"),
              NodeArgument("a","Interval", "-world.z"),
              NodeArgument("ma", "Material"),
              NodeArgument("b","Interval", "-world.z"),
              NodeArgument("mb", "Material"),
              NodeArgument("c","Interval", "-world.z"),
              NodeArgument("mc", "Material"),
              NodeArgument("d","Interval", "-world.z"),
              NodeArgument("md", "Material")
            ),
            "bounds" -> Seq(
              NodeArgument("v","Interval3"),
              NodeArgument("a","Interval", "-world.z"),
              NodeArgument("ma", "Material"),
              NodeArgument("b","Interval", "-world.z"),
              NodeArgument("mb", "Material"),
              NodeArgument("c","Interval", "-world.z"),
              NodeArgument("mc", "Material"),
              NodeArgument("d","Interval", "-world.z"),
              NodeArgument("md", "Material")
            )
          ),
          Nil,
          LanguageMap(
            "scala" -> Map(
              "o" -> NodeFunction("splitnoise3", "Double",
                """splitNoise3(v,4,0)*a +
                   splitNoise3(v,4,1)*b +
                   splitNoise3(v,4,2)*c +
                   splitNoise3(v,4,3)*d
                """),
              "m" -> NodeFunction("splitnoise3m", "Material",
                """
                  val resa = splitNoise3(v,4,0)
                  val resb = splitNoise3(v,4,1)
                  val resc = splitNoise3(v,4,2)
                  val resd = splitNoise3(v,4,3)


                  var max = resa
                  var mat = ma

                  if( resb > max  ) {max = resb; mat = mb}
                  if( resc > max  ) {max = resc; mat = mc}
                  if( resd > max  ) {max = resd; mat = md}

                  mat
                """)
            ),
            "prediction" -> Map(
              "o" -> NodeFunction("splitnoise3", "Interval",
                """Interval(min(min(a.low,b.low),min(c.low,d.low)),max(max(a.high, b.high), max(c.high, d.high)))"""),
              "m" -> NodeFunction("splitnoise3m", "Material", """sys.error()""")
            ),
            "bounds" -> Map(
              "o" -> NodeFunction("splitnoise3", "Interval",
                """Interval(min(min(a.low,b.low),min(c.low,d.low)),max(max(a.high, b.high), max (c.high, d.high)))"""),
              "m" -> NodeFunction("splitnoise3m", "Material", """sys.error()""")
            )
          )
          )


      ) // Seq
		), // NodeCategory
//******************************************************************************
//******************* Basic Math ***********************************************
//******************************************************************************
		NodeCategory("Basic Math",
			Seq(
				NodeType("a + b",
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
				NodeType("a + b + c",
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
				NodeType("a + Exp Slider",
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
				NodeType("a - b",
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
				NodeType("-a",
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
				NodeType("a * b",
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
				NodeType("a * b * c",
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
				NodeType("a * Exp Slider",
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
				NodeType("a / b",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("a","Double","1"),
							NodeArgument("b","Double","1")
						),
						"glsl" -> Seq(
							NodeArgument("a","float","1"),
							NodeArgument("b","float","1")
						),
						"prediction" -> Seq(
							NodeArgument("a","Interval","1"),
							NodeArgument("b","Interval","1")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("divide2", "Double",
							"""a/b""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("divide2", "float",
							"""return a/b;""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("divide2", "Interval",
							"""a/b""")
						)
					)
				),
				NodeType("exp(x)",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("x","Double","1")
						),
						"glsl" -> Seq(
							NodeArgument("x","float","1")
						),
						"prediction" -> Seq(
							NodeArgument("x","Interval","Interval(1)")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("exponential", "Double",
							"""exp(x)""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("exponential", "float",
							"""return exp(x);""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("exponential", "Interval",
							"""interval.functions.exp(x)""")
						)
					)
				),
				NodeType("Min(a,b)",
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
							"""interval.functions.min(a,b)""")
						)
					)
				),
				NodeType("Min(a,b,c)",
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
							"""interval.functions.min(interval.functions.min(a,b),c)""")
						)
					)
				),
				NodeType("Max(a,b)",
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
							"""interval.functions.max(a,b)""")
						)
					)
				),
				NodeType("Max(a,b,c)",
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
							"""interval.functions.max(interval.functions.max(a,b),c)""")
						)
					)
				)
			)
		),

		NodeCategory("Extended Math",
			Seq(
				NodeType("Lerp with Slider",
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
					Seq(
						NodeSlider("t")
					),
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("lerpslider", "Double",
							"""a + t * (b - a)""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("lerpslider", "float",
							"""return a + t * (b - a);""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("lerpslider", "Interval",
							"""a + t * (b - a)""")
						)
					)
				),
				NodeType("Lerp",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("a","Double"),
							NodeArgument("t","Double"),
							NodeArgument("b","Double")
						),
						"glsl" -> Seq(
							NodeArgument("a","float"),
							NodeArgument("t","float"),
							NodeArgument("b","float")
						),
						"prediction" -> Seq(
							NodeArgument("a","Interval"),
							NodeArgument("t","Interval"),
							NodeArgument("b","Interval")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("lerp", "Double",
							"""a + clamp(t,0,1) * (b - a)""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("lerp", "float",
							"""return a + clamp(t,0,1) * (b - a);""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("lerp", "Interval",
							"""a + clamp(t,0,1) * (b - a)""")
						)
					)
				)
			)
		),

		NodeCategory("Linear Algebra",
			Seq(
				NodeType("Vec3(x,y,z)",
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
							"v" -> NodeFunction("createvec3", "Interval3",
							"""Interval3(x,y,z)""")
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
							NodeArgument("v","Interval3")
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
				),
				NodeType("Vec3 + Scalar",
					LanguageMap(

						"scala" -> Seq(
							NodeArgument("v","Vec3"),
							NodeArgument("s","Double")
						),
						"glsl" -> Seq(
							NodeArgument("v","vec3"),
							NodeArgument("s","float")
						),
						"prediction" -> Seq(
							NodeArgument("v","Interval3"),
							NodeArgument("s","Interval")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("scalarplusvec3", "Vec3",
							"""v+s""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("scalarplusvec3", "vec3",
							"""return v+s;""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("scalarplusvec3", "Interval3",
							"""v+s""")
						)
					)
				),
				NodeType("Vec3 - Scalar",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("v","Vec3"),
							NodeArgument("s","Double")
						),
						"glsl" -> Seq(
							NodeArgument("v","vec3"),
							NodeArgument("s","float")
						),
						"prediction" -> Seq(
							NodeArgument("v","Interval3"),
							NodeArgument("s","Interval")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("scalarminusvec3", "Vec3",
							"""v-s""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("scalarminusvec3", "vec3",
							"""return v-s;""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("scalarminusvec3", "Interval3",
							"""v-s""")
						)
					)
				),
				NodeType("Vec3 * Scalar",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("v","Vec3"),
							NodeArgument("s","Double","1")
						),
						"glsl" -> Seq(
							NodeArgument("v","vec3"),
							NodeArgument("s","float","1")
						),
						"prediction" -> Seq(
							NodeArgument("v","Interval3"),
							NodeArgument("s","Interval","Interval(1)")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("scalartimesvec3", "Vec3",
							"""v*s""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("scalartimesvec3", "vec3",
							"""return v*s;""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("scalartimesvec3", "Interval3",
							"""v*s""")
						)
					)
				),
				NodeType("Vec3 / Scalar",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("v","Vec3"),
							NodeArgument("s","Double","1")
						),
						"glsl" -> Seq(
							NodeArgument("v","vec3"),
							NodeArgument("s","float","1")
						),
						"prediction" -> Seq(
							NodeArgument("v","Interval3"),
							NodeArgument("s","Interval","Interval(1)")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("dividevec3byscalar", "Vec3",
							"""v/s""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("dividevec3byscalar", "vec3",
							"""return v/s;""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("dividevec3byscalar", "Interval3",
							"""v/s""")
						)
					)
				),
				NodeType("Vec3 + Vec3",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("v1","Vec3"),
							NodeArgument("v2","Vec3")
						),
						"glsl" -> Seq(
							NodeArgument("v1","vec3"),
							NodeArgument("v2","vec3")
						),
						"prediction" -> Seq(
							NodeArgument("v1","Interval3"),
							NodeArgument("v2","Interval3")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("vec3plusvec3", "Vec3",
							"""v1+v2""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("vec3plusvec3", "vec3",
							"""return v1+v2;""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("vec3plusvec3", "Interval3",
							"""v1+v2""")
						)
					)
				),
				NodeType("Vec3 - Vec3",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("v1","Vec3"),
							NodeArgument("v2","Vec3")
						),
						"glsl" -> Seq(
							NodeArgument("v1","vec3"),
							NodeArgument("v2","vec3")
						),
						"prediction" -> Seq(
							NodeArgument("v1","Interval3"),
							NodeArgument("v2","Interval3")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("vec3minusvec3", "Vec3",
							"""v1-v2""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("vec3minusvec3", "vec3",
							"""return v1-v2;""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("vec3minusvec3", "Interval3",
							"""v1-v2""")
						)
					)
				),
				NodeType("Vec3 * Vec3",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("v1","Vec3","Vec3(1)"),
							NodeArgument("v2","Vec3","Vec3(1)")
						),
						"glsl" -> Seq(
							NodeArgument("v1","vec3","vec3(1)"),
							NodeArgument("v2","vec3","vec3(1)")
						),
						"prediction" -> Seq(
							NodeArgument("v1","Interval3","Interval3(1)"),
							NodeArgument("v2","Interval3","Interval3(1)")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("vec3timesvec3", "Vec3",
							"""v1*v2""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("vec3timesvec3", "vec3",
							"""return v1*v2;""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("vec3timesvec3", "Interval3",
							"""v1*v2""")
						)
					)
				),
				NodeType("Vec3 / Vec3",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("v1","Vec3","Vec3(1)"),
							NodeArgument("v2","Vec3","Vec3(1)")
						),
						"glsl" -> Seq(
							NodeArgument("v1","vec3","vec3(1)"),
							NodeArgument("v2","vec3","vec3(1)")
						),
						"prediction" -> Seq(
							NodeArgument("v1","Interval3","Interval3(1)"),
							NodeArgument("v2","Interval3","Interval3(1)")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("dividevec3byvec3", "Vec3",
							"""v1/v2""")
						),
						"glsl" -> Map(
							"o" -> NodeFunction("dividevec3vec3", "vec3",
							"""return v1/v2;""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("dividevec3vec3", "Interval3",
							"""v1/v2""")
						)
					)
				),
				NodeType("Extract Vec4",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("v","Vec4")
						),
						"prediction" -> Seq(
							NodeArgument("v","Interval4")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"x" -> NodeFunction("vec4x", "Double","v.x"),
							"y" -> NodeFunction("vec4y", "Double","v.y"),
							"z" -> NodeFunction("vec4z", "Double","v.z"),
							"w" -> NodeFunction("vec4w", "Double","v.w")
						),
						"prediction" -> Map(
							"x" -> NodeFunction("vec4x", "Interval","v.x"),
							"y" -> NodeFunction("vec4y", "Interval","v.y"),
							"z" -> NodeFunction("vec4z", "Interval","v.z"),
							"w" -> NodeFunction("vec4w", "Interval","v.w")
						)
					)
				),


				NodeType("Z-Rotation",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("v","Vec3"),
							NodeArgument("angle","Double")
						),
						"glsl" -> Seq(
							NodeArgument("v","vec3"),
							NodeArgument("angle","float")
						),
						"prediction" -> Seq(
							NodeArgument("v","Interval3"),
							NodeArgument("angle","Interval")
						)
					),
					Nil,
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("rotate", "Vec3",
							"""Mat3(Mat3x4 rotateZ angle) * v""")
						)
/*						"glsl" -> Map(
							"o" -> NodeFunction("rotate", "vec3",
							"""Mat3(Mat3x4 rotateZ angle) * v""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("rotate", "Interval3",
							"""Mat3(Mat3x4 rotateZ angle) * v""")
						)*/
					)
				),
				NodeType("Z-Rotation Slider",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("v","Vec3")
						),
						"glsl" -> Seq(
							NodeArgument("v","vec3")
						),
						"prediction" -> Seq(
							NodeArgument("v","Interval3")
						)
					),
					Seq(
						NodeSlider("angle", "(s*2-1)*Pi")
					),
					LanguageMap(
						"scala" -> Map(
							"o" -> NodeFunction("rotate", "Vec3",
							"""Mat3(Mat3x4 rotateZ angle) * v""")
						)
/*						"glsl" -> Map(
							"o" -> NodeFunction("rotate", "vec3",
							"""Mat3(Mat3x4 rotateZ angle) * v""")
						),
						"prediction" -> Map(
							"o" -> NodeFunction("rotate", "Interval3",
							"""Mat3(Mat3x4 rotateZ angle) * v""")
						)*/
					)
				)

			)
		),

		NodeCategory("Single Objects",
			Seq(
				NodeType("Sphere",
					LanguageMap(
						"scala" -> Seq(
							NodeArgument("v","Vec3")
						),
						"glsl" -> Seq(
							NodeArgument("v","vec3")
						),
						"prediction" -> Seq(
							NodeArgument("v","Interval3")
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
							"""-interval.functions.length(v) + radius""")
						)
					)
				)
			)
		),
		NodeCategory("Materials",
			Seq(
				NodeType("RGB Color",
					LanguageMap("scala" -> Nil, "glsl" -> Nil),
					Seq(NodeSlider("r"), NodeSlider("g"), NodeSlider("b")),
					LanguageMap(
            "scala" -> Map("m" -> NodeFunction("matrgb", "Material", "Material(-1,r,g,b)") ),
						"glsl" ->  Map("m" -> NodeFunction("matrgb", "vec4",     "return vec4(r, g, b, 1);")	)
					)
				)
			) ++
			GameEngineMaterials.nodeTypes
		),
		NodeCategory("Material Functions",
			Seq(
				NodeType("Vec3 to color",
					LanguageMap(
						"scala" -> Seq(	NodeArgument("v","Vec3")),
						"glsl" -> Seq(NodeArgument("v","vec3"))
					),

					Nil,
					LanguageMap(
						"scala" -> Map(
							"m" -> NodeFunction("vectocolor", "Material", "Material(-1, r, g, b)") ),
						"glsl" -> Map(
							"m" -> NodeFunction("vectocolor", "vec4", "return vec4(v,1);")	)
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
/*				NodeType("Blend Materials (not working yet)",
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
				),*/
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
    // generate Materials

    val Matreg = new Regex(""".*Material\((.+),(.+),(.+),(.+)\).*""","id","r","g","b")
    val MatregOld = new Regex(""".*Material\((.+),(.+),(.+)\).*""","r","g","b")
    val MatregOld2 = new Regex(""".*Material\((.+),(.+)\).*""","color","id")
    val MatregOld3 = new Regex(""".*Material\((.+)\).*""","color")
    val MatregDummy = new Regex(""".*Material\((.*)\).*""","args")

    // replace material id in scala code of node
    def setMatId(matCode:String, id:Int) = {
      val c = matCode match {
        case Matreg(_,_,_,_) =>
          Matreg.replaceAllIn(matCode,_ match { case Groups(oldId,r,g,b) => s"Material($id,$r,$g,$b)" })

        case MatregOld(_,_,_) =>
          MatregOld.replaceAllIn(matCode,_ match { case Groups(r,g,b) => s"Material($id,$r,$g,$b)" })

        case MatregOld2(_,_) =>
          MatregOld2.replaceAllIn(matCode,_ match { case Groups(color, oldId) =>
            s"Material($id,$color)" })

        case MatregOld3(_) =>
          MatregOld3.replaceAllIn(matCode,_ match { case Groups(color) =>
            s"Material($id,$color)" })

        case MatregDummy(_) =>
          MatregDummy.replaceAllIn(matCode,_ match { case Groups(args) => s"Material($id,255,0,255)" })
      }
      println(matCode,c)
      c
    }

    // compile and execute code of node to get material color
    def getColor(matNode:noiseeditor.Node):Vec3 = {
      import matNode.outconnectors
      val code = CompositionManager.generatepreviewcode(outconnectors.head)
      val compilation = InterpreterManager[(Vec3 => Material)](code) // Future
      compilation() match {
        // Compilation successful
        case Some(interpretedcode) =>
          interpretedcode(Vec3(0)).rgb
        // Compilation not successful
        case None => ???
      }
    }


    // write ids into material code, generate texture atlas
    val resolution = 64
    val width = nextPowerOfTwo(resolution * NodeManager.materialNodes.size)
    val height = resolution

    // TYPE_INT_ARGB specifies the image format: 8-bit RGBA packed into integer pixels
    val atlas = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
    val atlasGr = atlas.createGraphics();

    println("writing ids into materials (" + NodeManager.materialNodes.size + "):")
    for( (materialNode,id) <- NodeManager.materialNodes.zipWithIndex ) {
      // replace material id
      val nodeFunction = materialNode.functions("scala").head._2
      val newNodeFunction = nodeFunction.copy(name="material__"+id, code = setMatId(nodeFunction.code,id))
      val key = materialNode.functions("scala").head._1
      materialNode.functions = materialNode.functions.updated("scala",Map(key -> newNodeFunction))
      materialNode.refreshConnectors()
      assert(newNodeFunction eq materialNode.outconnectors.head.function("scala"))

      // create color rectangle in atlas
      val color = getColor(materialNode)
      atlasGr.setColor(new Color(color.r.toFloat, color.g.toFloat, color.b.toFloat))
      atlasGr.fillRect(id*resolution,0,resolution, resolution)
    }

		def densityCode           = generateScalaCode("density", "0.0", preview.connectedoutconnector("d"))
		def materialCode          = generateScalaCode("material", "Material()", preview.connectedoutconnector("m"))
		def glslMaterialCode      = generateglslcode (preview.connectedoutconnector("m"))
    def intervalExtensionCode = generatePredictionCode(preview.connectedoutconnector("d"))
    def boundsCode            = generatePredictionCode(preview.connectedoutconnector("d"), onlyBounds = true)

		try {
			exporttype match {
				case "scala_density" =>
					println(densityCode)
				
				case "scala_material" =>
					println(materialCode)
				
//				case "xml_materials" =>
//					println(generatexmlmaterials)

				case "glsl_material" =>
					println(glslMaterialCode)

				case "prediction" =>
					println(intervalExtensionCode)

				case "engine" =>
					println("Exporting to Engine:")
					
					var path = "../gameengine"
					
					//println("clearing world cache...")
					//TODO: remove or do it with the java-api
					//Runtime.getRuntime.exec("rm " + path + "/worldoctree")


          val skeleton =
            """
              |package downearth.generation
              |
              |import interval.{Interval, Interval3, Interval4}
              |import simplex3d.math.double._
              |import simplex3d.math.double.functions._
              |import downearth.resources.Material
              |
              |object WorldDefinition extends WorldFunction {
              |
              |  %s
              |
              |  %s
              |
              |  %s
              |
              |  %s
              |
              |}
            """.stripMargin

          val worldDefinition = skeleton format(
              densityCode,
              materialCode,
              boundsCode,
              intervalExtensionCode
            )

					var out = new java.io.FileWriter(path + "/src/main/scala/downearth/generation/WorldDefinition.scala")
					out.write(worldDefinition)
					out.close()

          ImageIO.write(atlas, "PNG", new File(path + "/src/main/resources/materials.png"));

          println("done.")
			}
		}
		catch {
			case e:Exception =>
			e.printStackTrace()
		}
	}
}
