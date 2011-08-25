(source:Vec3) => {
val noise1 = new Noise(ClassicalGradientNoise){
	def apply(u:Vec3):Float = super.apply(u).toFloat
}
def sum(xs:Seq[Float]=Nil) = {xs.sum}
def sum_t1310348205(xs:Seq[Float]=Nil) = {xs.sum}
def noise3_t1310348205(x:Float, y:Float, z:Float, size:Float, outscale:Float, outoffset:Float) = {
			val v = Vec3(x,y,z)
			val invexpsize = pow(256,((0.5f-size)*2f))
			val expoutscale = pow(256,((outscale-0.5f)*2f))/invexpsize
			val linearoutoffset = (outoffset-0.5f)*2f
			(noise1(v*invexpsize)+linearoutoffset)*expoutscale
			}
def noise3v(v:Vec3=Vec3(0), size:Float, outscale:Float, outoffset:Float) = {
			val invexpsize = pow(256,((0.5f-size)*2f))
			val expoutscale = pow(256,((outscale-0.5f)*2f))/invexpsize
			val linearoutoffset = (outoffset-0.5f)*2f
			(noise1(v*invexpsize)+linearoutoffset)*expoutscale
			}
def srcxyzy_t1310348205() = {source.y}
def srcv_t1310348205() = {source}
def srcxyzz_t1310348205() = {source.z}
def matthreshold_t1310348205(x:Material=Material(0xFFFFFF), t:Float, y:Material=Material(0), threshold:Float) = {if(t > (threshold-0.5f)*2f) x else y}
def matrgb_t1310348205(r:Float, g:Float, b:Float) = {Material((r*255).toInt << 16 | (g*255).toInt << 8 | (b*255).toInt)}
def srcxyzx_t1310348205() = {source.x}
def noise3v_t1310348205(v:Vec3=Vec3(0), size:Float, outscale:Float, outoffset:Float) = {
			val invexpsize = pow(256,((0.5f-size)*2f))
			val expoutscale = pow(256,((outscale-0.5f)*2f))/invexpsize
			val linearoutoffset = (outoffset-0.5f)*2f
			(noise1(v*invexpsize)+linearoutoffset)*expoutscale
			}

val vn7_srcv_t1310348205 = srcv_t1310348205()
val vn9_noise3v_t1310348205 = noise3v_t1310348205(vn7_srcv_t1310348205, 0.65f, 0.5f, 0.5f)
val vn8_srcxyzz_t1310348205 = srcxyzz_t1310348205()
val vn8_srcxyzy_t1310348205 = srcxyzy_t1310348205()
val vn8_srcxyzx_t1310348205 = srcxyzx_t1310348205()
val vn3_sum_t1310348205 = sum_t1310348205(Seq(vn8_srcxyzy_t1310348205, vn9_noise3v_t1310348205))
val vn11_noise3v = noise3v(vn7_srcv_t1310348205, 0.16f, 0.81f, 0.5f)
val vn2_noise3_t1310348205 = noise3_t1310348205(0f, vn3_sum_t1310348205, 0f, 0.5f, 0.5f, 0.4f)
val vn1_matrgb_t1310348205 = matrgb_t1310348205(0.47f, 0.29f, 0.12f)
val vn12_sum = sum(Seq(vn2_noise3_t1310348205, vn11_noise3v))
val vn5_matrgb_t1310348205 = matrgb_t1310348205(1.0f, 0.81f, 0.5f)
val vn6_matthreshold_t1310348205 = matthreshold_t1310348205(vn5_matrgb_t1310348205, vn12_sum, vn1_matrgb_t1310348205, 0.5f)

(0f, vn6_matthreshold_t1310348205)
}