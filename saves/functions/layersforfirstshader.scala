(source:Vec3) => {
val noise1 = new Noise(ClassicalGradientNoise){
	def apply(u:Vec3):Float = super.apply(u).toFloat
}
def sum(xs:Seq[Float]=Nil) = {xs.sum}
def srcxyzz() = {source.z}
def matrgb(r:Float, g:Float, b:Float) = {Material((r*255).toInt << 16 | (g*255).toInt << 8 | (b*255).toInt)}
def noise3v(v:Vec3=Vec3(0), size:Float, outscale:Float, outoffset:Float) = {
			val invexpsize = pow(256,((0.5f-size)*2f))
			val expoutscale = pow(256,((outscale-0.5f)*2f))/invexpsize
			val linearoutoffset = (outoffset-0.5f)*2f
			(noise1(v*invexpsize)+linearoutoffset)*expoutscale
			}
def matthreshold(x:Material=Material(0xFFFFFF), t:Float, y:Material=Material(0), threshold:Float) = {if(t > (threshold-0.5f)*2f) x else y}
def srcxyzy() = {source.y}
def noise3(x:Float, y:Float, z:Float, size:Float, outscale:Float, outoffset:Float) = {
			val v = Vec3(x,y,z)
			val invexpsize = pow(256,((0.5f-size)*2f))
			val expoutscale = pow(256,((outscale-0.5f)*2f))/invexpsize
			val linearoutoffset = (outoffset-0.5f)*2f
			(noise1(v*invexpsize)+linearoutoffset)*expoutscale
			}
def srcv() = {source}
def srcxyzx() = {source.x}

val vn4_srcv = srcv()
val vn9_srcxyzz = srcxyzz()
val vn9_srcxyzy = srcxyzy()
val vn9_srcxyzx = srcxyzx()
val vn13_noise3v = noise3v(vn4_srcv, 0.65f, 0.5f, 0.5f)
val vn14_sum = sum(Seq(vn13_noise3v, vn9_srcxyzy))
val vn6_matrgb = matrgb(0.47f, 0.29f, 0.12f)
val vn15_noise3 = noise3(0f, vn14_sum, 0f, 0.5f, 0.5f, 0.4f)
val vn7_matrgb = matrgb(1.0f, 0.81f, 0.5f)
val vn8_matthreshold = matthreshold(vn7_matrgb, vn15_noise3, vn6_matrgb, 0.5f)

(0f, vn8_matthreshold)
}