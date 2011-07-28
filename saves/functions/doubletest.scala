(source:Vec3) => {
val noise1 = new Noise(ClassicalGradientNoise){
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
val vn2_frichnoise3 = frichnoise3(vn1_srcv, 0.0, 0.0, 0.0, 0.0, 0.0, 256.0f, 0.00390625f, -1.0f)

(vn2_frichnoise3, Material())
}