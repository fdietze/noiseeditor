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

		FunctionNodeType("Noise", "Noise xyz", Seq("x:Float","y:Float","z:Float"),
			Seq(
				("size", "pow(256,((0.5f-s)*2f))"),
				("scale", "pow(256,((s-0.5f)*2f))"),
				("offset", "(s-0.5f)*2f")),
			Function("noise3xyz" ,"""(noise1(Vec3(x,y,z)*size)+offset)*scale/size""", "Float")),


		FunctionNodeType("Noise", "Rich Noise", Seq("v:Seq[Vec3]","x:Seq[Float]","y:Seq[Float]","z:Seq[Float]","add:Seq[Float]","sub:Seq[Float]"),
			Seq(
				("size", "pow(256,((0.5f-s)*2f))"),
				("scale", "pow(256,((s-0.5f)*2f))"),
				("offset", "(s-0.5f)*2f")),
			Function("richnoise3","""
			val sumv = v.fold[Vec3](Vec3(0))( (a,b) => a+b ) + Vec3(x.sum,y.sum,z.sum)
			(noise1(sumv*size)+offset)*scale/size + add.sum - sub.sum
			""", "Float")),
		
		FunctionNodeType("Noise", "Noise v", Seq("v:Vec3=Vec3(0)"),
			Seq(
				("size", "pow(256,((0.5f-s)*2f))"),
				("scale", "pow(256,((s-0.5f)*2f))"),
				("offset", "(s-0.5f)*2f")),
			Function("noise3v" ,"(noise1(v*size)+offset)*scale/size", "Float")),


			
		FunctionNodeType("Combinations","Mix", Seq("x:Float","y:Float"), Seq("mixvalue"),
			Function("mix", "x*(1-mixvalue)+y*mixvalue", "Float")),
			
		FunctionNodeType("Combinations", "Threshold", Seq("x:Float=1f","t:Float","y:Float=0f"),	Seq(("threshold","(s-0.5f)*2f")),
			Function("threshold", "if(t > threshold) x else y", "Float")),
			
		FunctionNodeType("Combinations", "Threshold abs", Seq("x:Float=1f","t:Float","y:Float=0f"), Seq(("threshold","(s-0.5f)*2f")),
			Function( "absthreshold", "if(abs(t) > threshold) x else y", "Float")),

		FunctionNodeType("Vectors", "Vec3", Seq("x:Seq[Float]","y:Seq[Float]","z:Seq[Float]"),Nil,
			Function("vec3", "Vec3(x.sum,y.sum,z.sum)", "Vec3")),

		FunctionNodeType("Vectors", "Vec3.xyz", Seq("v:Vec3"), Nil,
			Function("vec3x", "v.x", "Float", "x"),
			Function("vec3y", "v.y", "Float", "y"),
			Function("vec3z", "v.z", "Float", "z")),

		FunctionNodeType("Vectors", "Vec3*s",Seq("v:Vec3","s:Float"),Nil, Function("vec3p", "v*s", "Vec3")),

		FunctionNodeType("Vectors", "Vec3*expscale", Seq("v:Vec3"), Seq(("factor","pow(256,((s-0.5f)*2f))")),
			Function("vec3p", "v*factor", "Vec3")),

		FunctionNodeType("Math", "Min", Seq("xs:Seq[Float]"), Nil, Function("min", "xs.min", "Float")),
		FunctionNodeType("Math", "Max", Seq("xs:Seq[Float]"), Nil, Function("max", "xs.max", "Float")),
		FunctionNodeType("Math", "Product", Seq("xs:Seq[Float]"), Nil, Function("product", "xs.product", "Float")),
		FunctionNodeType("Math", "Sum", Seq("xs:Seq[Float]"), Nil, Function("sum", "xs.sum", "Float")),

		FunctionNodeType("Math", "Div", Seq("x:Float","y:Float=1f"), Nil, Function("division", "x/y", "Float")),
		FunctionNodeType("Math", "Diff", Seq("x:Float","y:Float"), Nil, Function("difference", "x-y", "Float")),

		FunctionNodeType("Math", "Exp Clamp 256", Seq("x:Float"), Seq(("maxabs","pow(256,((s-0.5f)*2f))")),
			Function("expclamp256", "clamp(x,-maxabs,maxabs)", "Float")),
		
		FunctionNodeType("Math", "Squeeze", Seq("x:Float"), Seq(("max","pow(256,((s-0.5f)*2f))")),
			Function("squeeze", "(if(x<=0) (1/(1-x)-1) else 1/(-1-x)+1)*max", "Float")),

		FunctionNodeType("Math", "Exp Scale 256", Seq("x:Float"), Seq(("factor","pow(256,((s-0.5f)*2f))")),
			Function("expscale256", "x*factor", "Float")),

		FunctionNodeType("Math", "Constant 1/256..256", Nil, Seq(("constant","pow(256,((s-0.5f)*2f))")),
			Function("constant256", "constant", "Float")),
		FunctionNodeType("Math", "Constant -1..1", Nil, Seq(("constant","(s-0.5f)*2f")),
			Function("constantm1" ,"constant", "Float")),

		FunctionNodeType("High Level", "Heart Surface", Seq("v:Vec3"),
			Seq(
				("scale","pow(10f,2f*s-2.5f)"),
				("xshift","(s-0.5f)*20f"),
				("yshift","(s-0.5f)*20f"),
				("zshift","(s-0.5f)*20f")),
			Function("heart", "val input = (Vec3(v.x,-v.y,v.z)*scale + Vec3(xshift, yshift, zshift)).xzy; import input._; -(pow(pow(x,2)+9/4*pow(y,2)+pow(z,2)-1,3)-pow(x,2)*pow(z,3)-9/80*pow(y,2)*pow(z,3))", "Float")),

		FunctionNodeType("Math", "Root", Seq("a:Float"), Seq(("degree","1/pow(256, s)")),
			Function("root", "sign(a)*pow(abs(a), degree)", "Float")),
		
		FunctionNodeType("Material", "Material Red", Nil, Nil, Function("matred", "Material(0xFF0000)", "Material")),
		FunctionNodeType("Material", "Material Green", Nil, Nil, Function("matgreen", "Material(0x00FF00)", "Material")),
		FunctionNodeType("Material", "Material Blue", Nil, Nil, Function("matblue", "Material(0x0000FF)", "Material")),
		FunctionNodeType("Material", "Material Yellow", Nil, Nil, Function("matyellow", "Material(0xFFFF00)", "Material")),
		FunctionNodeType("Material", "Material RGB", Nil,
			Seq(("r","s*255"),("g","s*255"),("b","s*255")),
			Function("matrgb", "Material(r.toInt << 16 | g.toInt << 8 | b.toInt)", "Material")),


		FunctionNodeType("Material", "Material Threshold", Seq("x:Material=Material(0xFFFFFF)","t:Float","y:Material=Material(0)"),
			Seq(("threshold","(s-0.5f)*2f")),
			Function("matthreshold", "if(t > threshold) x else y", "Material")),
		
		FunctionNodeType("Source", "Scalable Source", Nil, Seq(("scale","pow(256,((0.5f-s)*2f))")),
			Function("scalesrcv", "source * scale", "Vec3", "v"),
			Function("scalesrcx", "source.x * scale", "Float", "x"),
			Function("scalesrcy", "source.y * scale", "Float", "y"),
			Function("scalesrcz", "source.z * scale", "Float", "z")),
			
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
