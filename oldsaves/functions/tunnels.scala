(source:Vec3) => {
val noise1 = new Noise(ClassicalGradientNoise){
	def apply(u:Vec3):Float = super.apply(u).toFloat
}
def fsrcy_1308958664_1308963222(scale:Float, x:Float, y:Float, z:Float) = {(source.y + (y-0.5f)*256f) * (pow(256,((0.5f-scale)*2f)))}
def constant256_1308963222(factor:Float) = {pow(256,((factor-0.5f)*2f))}
def fnoise_1308963222(v:Seq[Vec3]=Seq(Vec3(0)), x:Seq[Float]=Nil, y:Seq[Float]=Nil, z:Seq[Float]=Nil, add:Seq[Float]=Nil, sub:Seq[Float]=Nil, size:Float, scale:Float, offset:Float) = {
			val invexpsize = pow(256,((0.5f-size)*2f))
			val expoutscale = pow(256,((scale-0.5f)*2f))/invexpsize
			val linearoutoffset = (offset-0.5f)*2f
			val sumv = v.reduce( (x,y) => x+y ) + Vec3(x.sum,y.sum,z.sum);
			(noise1(sumv*invexpsize)+linearoutoffset)*expoutscale + add.sum - sub.sum
			}
def fsrcz_1308958664_1308963222(scale:Float, x:Float, y:Float, z:Float) = {(source.z + (z-0.5f)*256f) * (pow(256,((0.5f-scale)*2f)))}
def fsrcx_1308958664_1308963222(scale:Float, x:Float, y:Float, z:Float) = {(source.x + (x-0.5f)*256f) * (pow(256,((0.5f-scale)*2f)))}
def constant256_1308958664_1308963222(factor:Float) = {pow(256,((factor-0.5f)*2f))}
def fnoise_1308958664_1308963222(v:Seq[Vec3]=Seq(Vec3(0)), x:Seq[Float]=Nil, y:Seq[Float]=Nil, z:Seq[Float]=Nil, add:Seq[Float]=Nil, sub:Seq[Float]=Nil, size:Float, scale:Float, offset:Float) = {
			val invexpsize = pow(256,((0.5f-size)*2f))
			val expoutscale = pow(256,((scale-0.5f)*2f))/invexpsize
			val linearoutoffset = (offset-0.5f)*2f
			val sumv = v.reduce( (x,y) => x+y ) + Vec3(x.sum,y.sum,z.sum);
			(noise1(sumv*invexpsize)+linearoutoffset)*expoutscale + add.sum - sub.sum
			}
def product_1308958664_1308963222(xs:Seq[Float]=Nil) = {xs.product}
def constantm1_1308958664_1308963222(factor:Float) = {(factor-0.5f)*2f}
def fsrcv_1308958664_1308963222(scale:Float, x:Float, y:Float, z:Float) = {(source + (Vec3(x,y,z)-0.5f)*256f) * (pow(256,((0.5f-scale)*2f)))}
def product_1308963222(xs:Seq[Float]=Nil) = {xs.product}

val vn2_constant256_1308963222 = constant256_1308963222(0.9f)
val vn7_fsrcz_1308958664_1308963222 = fsrcz_1308958664_1308963222(0.81f, 0.5f, 0.5f, 0.5f)
val vn7_fsrcy_1308958664_1308963222 = fsrcy_1308958664_1308963222(0.81f, 0.5f, 0.5f, 0.5f)
val vn7_fsrcx_1308958664_1308963222 = fsrcx_1308958664_1308963222(0.81f, 0.5f, 0.5f, 0.5f)
val vn7_fsrcv_1308958664_1308963222 = fsrcv_1308958664_1308963222(0.81f, 0.5f, 0.5f, 0.5f)
val vn18_constant256_1308958664_1308963222 = constant256_1308958664_1308963222(0.0f)
val vn12_constantm1_1308958664_1308963222 = constantm1_1308958664_1308963222(0.88f)
val vn21_product_1308963222 = product_1308963222(Seq(vn2_constant256_1308963222))
val vn24_fnoise_1308963222 = fnoise_1308963222(Seq(vn7_fsrcv_1308958664_1308963222), Nil, Nil, Nil, Nil, Nil, 0.66f, 0.52f, 0.5f)
val vn13_product_1308963222 = product_1308963222(Seq(vn2_constant256_1308963222))
val vn1_product_1308958664_1308963222 = product_1308958664_1308963222(Seq(vn18_constant256_1308958664_1308963222, vn7_fsrcx_1308958664_1308963222))
val vn19_fnoise_1308958664_1308963222 = fnoise_1308958664_1308963222(Seq(Vec3(0)), Seq(vn1_product_1308958664_1308963222), Seq(vn13_product_1308963222, vn7_fsrcy_1308958664_1308963222, vn24_fnoise_1308963222), Seq(vn7_fsrcz_1308958664_1308963222, vn21_product_1308963222), Seq(vn12_constantm1_1308958664_1308963222), Nil, 0.5f, 0.5f, 0.5f)

(vn19_fnoise_1308958664_1308963222, Material())
}