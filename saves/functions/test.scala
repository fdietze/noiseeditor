(source:Vec3) => {

val noise1 = new Noise(ClassicalGradientNoise) {
	def apply(u:Vec3):Double = super.apply(u)
}

def srcxyzz() = {source.z}
def srcxyzy() = {source.y}

def frichnoise3(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double) = {
			val sumv = v + Vec3(x,y,z)
			(noise1(sumv*size)+offset)*scale/size + add - sub
}

def srcv() = {source}
def srcxyzx() = {source.x}

val vn1_srcxyzz = srcxyzz()
val vn1_srcxyzy = srcxyzy()
val vn1_srcxyzx = srcxyzx()
val vn1_srcv = srcv()
val vn2_frichnoise3 = frichnoise3(vn1_srcv, 0.0, 0.0, 0.0, vn1_srcxyzy, 0.0, 1.0, 1.0, 0.0)

(vn2_frichnoise3, Material())

}

#####################################

Shader-Syntax:

[returntype name([ parametertype parametername ],[ slidertype slidername ]) {
	code
}]

double result() {
	double vn nodeid funcname = funcname( vn nodeid funcname, funcparamdefault, slidername slidervalue )
	
	return vn nodeid funcname
}
