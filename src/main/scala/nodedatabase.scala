package noiseeditor

case class Function(name:String, code:String, outtype:String, outname:String = " ")
case class FunctionNodeType(category:String, title:String, intypes:Seq[String], sliders:Seq[AnyRef], functions:Function*)
//TODO: import from XML file
//TODO: All Scales exponential
//TODO: Compile check of all Nodes
//TODO: More High Level nodes, like Surface, Layers, Fractal Noise, Turbulence
//TODO: More Noise types, like cellular noise
object FunctionNodeDatabase{
	val functionnodetypes = Seq(
		FunctionNodeType("Noise", "Noise xyz", Seq("x:Float","y:Float","z:Float"), Seq("size",("outscale","s*5"),"outoffset"),
			Function("noise3" ,"""
			val v = Vec3(x,y,z)
			val invexpsize = pow(256,((0.5f-size)*2f))
			val expoutscale = pow(256,((outscale-0.5f)*2f))/invexpsize
			val linearoutoffset = (outoffset-0.5f)*2f
			(noise1(v*invexpsize)+linearoutoffset)*expoutscale
			""", "Float")),

		FunctionNodeType("Noise", "Rich Noise", Seq("v:Seq[Vec3]=Seq(Vec3(0))","x:Seq[Float]=Nil","y:Seq[Float]=Nil","z:Seq[Float]=Nil","add:Seq[Float]=Nil","sub:Seq[Float]=Nil"),	Seq("size","scale","offset"),
			Function("fnoise","""
			val invexpsize = pow(256,((0.5f-size)*2f))
			val expoutscale = pow(256,((scale-0.5f)*2f))/invexpsize
			val linearoutoffset = (offset-0.5f)*2f
			val sumv = v.reduce( (x,y) => x+y ) + Vec3(x.sum,y.sum,z.sum);
			(noise1(sumv*invexpsize)+linearoutoffset)*expoutscale + add.sum - sub.sum
			""","Float")),
		
		FunctionNodeType("Noise", "Noise v", Seq("v:Vec3=Vec3(0)"),	Seq("size","outscale","outoffset"),
			Function("noise3v", """
			val invexpsize = pow(256,((0.5f-size)*2f))
			val expoutscale = pow(256,((outscale-0.5f)*2f))/invexpsize
			val linearoutoffset = (outoffset-0.5f)*2f
			(noise1(v*invexpsize)+linearoutoffset)*expoutscale
			""", "Float")),
			
		FunctionNodeType("Combinations","Mix", Seq("x:Float","y:Float"), Seq("mixvalue"),
			Function("mix", "x*(1-mixvalue)+y*mixvalue", "Float")),
			
		FunctionNodeType("Combinations", "Threshold", Seq("x:Float=1f","t:Float","y:Float=0f"),	Seq("threshold"),
			Function("threshold", "if(t > (threshold-0.5f)*2f) x else y", "Float")),
			
		FunctionNodeType("Combinations", "Threshold abs", Seq("x:Float=1f","t:Float","y:Float=0f"), Seq("threshold"),
			Function( "absthreshold", "if(abs(t) > (threshold-0.5f)*2f) x else y", "Float")),

		FunctionNodeType("Vectors", "Vec3", Seq("x:Seq[Float]","y:Seq[Float]","z:Seq[Float]"),Nil,
			Function("vec3", "Vec3(x.sum,y.sum,z.sum)", "Vec3")),

		FunctionNodeType("Vectors", "Vec3.xyz", Seq("v:Vec3=Vec3(0)"), Nil,
			Function("vec3x", "v.x", "Float", "x"),
			Function("vec3y", "v.y", "Float", "y"),
			Function("vec3z", "v.z", "Float", "z")),

		FunctionNodeType("Vectors", "Vec3*s",Seq("v:Vec3=Vec3(0)","s:Float"),Nil,
			Function("vec3p", "v*s", "Vec3")),

		FunctionNodeType("Vectors", "Vec3*expscale", Seq("v:Vec3=Vec3(0)"), Seq("factor"),
			Function("vec3p", "v*(pow(256,((factor-0.5f)*2f)))", "Vec3")),

		FunctionNodeType("Math", "Min", Seq("xs:Seq[Float]=Nil"), Nil, Function("min", "xs.min", "Float")),
		FunctionNodeType("Math", "Max", Seq("xs:Seq[Float]=Nil"), Nil, Function("max", "xs.max", "Float")),
		FunctionNodeType("Math", "Product", Seq("xs:Seq[Float]=Nil"), Nil, Function("product", "xs.product", "Float")),
		FunctionNodeType("Math", "Sum", Seq("xs:Seq[Float]=Nil"), Nil, Function("sum", "xs.sum", "Float")),

		FunctionNodeType("Math", "Div", Seq("x:Float","y:Float=1f"), Nil, Function("division", "x/y", "Float")),
		FunctionNodeType("Math", "Diff", Seq("x:Float","y:Float"), Nil, Function("difference", "x-y", "Float")),

		FunctionNodeType("Math", "Exp Clamp 256", Seq("x:Float"), Seq("maxabs"),
			Function("expclamp256", "val expmaxabs = (pow(256,((maxabs-0.5f)*2f))); clamp(x,-maxabs,maxabs)", "Float")),


		FunctionNodeType("Math", "Exp Scale 256", Seq("x:Float"), Seq("factor"),
			Function("expscale256", "x*(pow(256,((factor-0.5f)*2f)))", "Float")),

		FunctionNodeType("Math", "Constant 1/256..256", Nil, Seq("factor"),
			Function("constant256", "pow(256,((factor-0.5f)*2f))", "Float")),
		FunctionNodeType("Math", "Constant -1..1", Nil, Seq("factor"),
			Function("constantm1" ,"(factor-0.5f)*2f", "Float")),

		FunctionNodeType("High Level", "Heart Surface", Seq("v:Vec3=Vec3(0)"), Seq("scale","xshift","yshift","zshift"),
			Function("heart", "val v = (Vec3(v.x,-v.y,v.z)*pow(10f,2f*scale-2.5f) + (Vec3(xshift, yshift, zshift)-0.5f)*20f).xzy; import v._; -(pow(pow(x,2)+9/4*pow(y,2)+pow(z,2)-1,3)-pow(x,2)*pow(z,3)-9/80*pow(y,2)*pow(z,3))", "Float")),

		FunctionNodeType("Math", "Root", Seq("a:Float"), Seq("factor"),
			Function("root", "sign(a)*pow(abs(a), (pow(256,((factor-0.5f)*2f))))", "Float")),
		
		FunctionNodeType("Material", "Material Red", Nil, Nil, Function("matred", "Material(0xFF0000)", "Material")),
		FunctionNodeType("Material", "Material Green", Nil, Nil, Function("matgreen", "Material(0x00FF00)", "Material")),
		FunctionNodeType("Material", "Material Blue", Nil, Nil, Function("matblue", "Material(0x0000FF)", "Material")),
		FunctionNodeType("Material", "Material Yellow", Nil, Nil, Function("matyellow", "Material(0xFFFF00)", "Material")),
		FunctionNodeType("Material", "Material RGB", Nil, Seq("r","g","b"), Function("matrgb", "Material((r*255).toInt << 16 | (g*255).toInt << 8 | (b*255).toInt)", "Material")),


		FunctionNodeType("Material", "Material Threshold", Seq("x:Material=Material(0xFFFFFF)","t:Float","y:Material=Material(0)"),	Seq("threshold"),
			Function("matthreshold", "if(t > (threshold-0.5f)*2f) x else y", "Material")),
		
		FunctionNodeType("Source", "Scalable Source", Nil, Seq("scale"),
			Function("scalesrcv", "source * (pow(256,((0.5f-scale)*2f)))", "Vec3", "v"),
			Function("scalesrcx", "(source * (pow(256,((0.5f-scale)*2f)))).x", "Float", "x"),
			Function("scalesrcy", "(source * (pow(256,((0.5f-scale)*2f)))).y", "Float", "y"),
			Function("scalesrcz", "(source * (pow(256,((0.5f-scale)*2f)))).z", "Float", "z")),
			
		FunctionNodeType("Source", "Source", Nil, Nil,
			Function("srcv", "source", "Vec3", "v"),
			Function("srcxyzx", "source.x", "Float", "x"),
			Function("srcxyzy", "source.y", "Float", "y"),
			Function("srcxyzz", "source.z", "Float", "z")),

		
		// King Arthurs Gold
		FunctionNodeType("Material", "Earth", Nil, Nil, Function("matearth", "Material(0x5a3910)", "Material")),
		FunctionNodeType("Material", "Cave",  Nil, Nil, Function("matcave",  "Material(0x1f0000)", "Material")),
		FunctionNodeType("Material", "Gravel",Nil, Nil, Function("matgravel","Material(0x282828)", "Material")),
		FunctionNodeType("Material", "Stone", Nil, Nil, Function("matstone", "Material(0x373737)", "Material")),
		FunctionNodeType("Material", "Gold",  Nil, Nil, Function("matgold",  "Material(0xfab614)", "Material")),
		FunctionNodeType("Material", "Solid", Nil, Nil, Function("matsolid", "Material(0x1e321e)", "Material")),
		FunctionNodeType("Material", "Wood", Nil, Nil, Function("matwood",  "Material(0x097b11)", "Material"))
		
		
	)

//TODO: Tooltip mit nodebeschreibung


//TODO: Threshold mit abschneiden und linearinterpolation
	val categories = functionnodetypes.map(_.category).distinct
}
