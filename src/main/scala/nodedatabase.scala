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

		FunctionNodeType("Noise", "Noise xyz", Seq("x:Double","y:Double","z:Double"),
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
			""", "Double")),

		FunctionNodeType("Noise", "Fast Rich Noise", Seq("v:Vec3","x:Double","y:Double","z:Double","add:Double","sub:Double"),
			Seq(
				("size", "pow(256,((0.5-s)*2))"),
				("scale", "pow(256,((s-0.5)*2))"),
				("offset", "(s-0.5)*2")),
			Function("frichnoise3","""
			val sumv = v + Vec3(x,y,z)
			(noise1(sumv*size)+offset)*scale/size + add - sub
			""", "Double")),

		
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
		
		FunctionNodeType("Source", "Scalable Source", Nil, Seq(("scale","pow(256,((0.5-s)*2))")),
			Function("scalesrcv", "source * scale", "Vec3", "v"),
			Function("scalesrcx", "source.x * scale", "Double", "x"),
			Function("scalesrcy", "source.y * scale", "Double", "y"),
			Function("scalesrcz", "source.z * scale", "Double", "z")),
			
		FunctionNodeType("Source", "Source", Nil, Nil,
			Function("srcv", "source", "Vec3", "v"),
			Function("srcxyzx", "source.x", "Double", "x"),
			Function("srcxyzy", "source.y", "Double", "y"),
			Function("srcxyzz", "source.z", "Double", "z")),

		
		// King Arthurs Gold
		FunctionNodeType("Material", "Earth", Nil, Nil, Function("matearth", "Material(0x5a3910)", "Material")),
		FunctionNodeType("Material", "Cave",  Nil, Nil, Function("matcave",  "Material(0x10000)", "Material")),
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
