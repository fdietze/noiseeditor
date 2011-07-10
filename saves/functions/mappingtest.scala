(source:Vec3) => {
val noise1 = new Noise(ClassicalGradientNoise){
	def apply(u:Vec3):Float = super.apply(u).toFloat
}
def srcv1308700001_1308700798_t1309182284_t1309291439_t1309294560_t1309295107(scale:Float, x:Float, y:Float, z:Float) = {(source + (Vec3(x,y,z)-0.5f)*256f) * (pow(256,((0.5f-scale)*2f)))}
def srcy1308700001_1308700798_t1309182284_t1309291439_t1309294560_t1309295107(scale:Float, x:Float, y:Float, z:Float) = {(source.y + (y-0.5f)*256f) * (pow(256,((0.5f-scale)*2f)))}
def srcz1308700001_1308700798_t1309182284_t1309291439_t1309294560_t1309295107(scale:Float, x:Float, y:Float, z:Float) = {(source.z + (z-0.5f)*256f) * (pow(256,((0.5f-scale)*2f)))}
def srcx1308700001_1308700798_t1309182284_t1309291439_t1309294560_t1309295107(scale:Float, x:Float, y:Float, z:Float) = {(source.x + (x-0.5f)*256f) * (pow(256,((0.5f-scale)*2f)))}
def custom_f3(a:Float, b:Float, c:Float, d:Float, s1:Float, s2:Float, s3:Float) = {if( a < 0f ) 0.5f else {if(floor(a).toInt % 2 == 0) 1f else -1f}
a}

val vn1_srcz1308700001_1308700798_t1309182284_t1309291439_t1309294560_t1309295107 = srcz1308700001_1308700798_t1309182284_t1309291439_t1309294560_t1309295107(0.6f, 0.47f, 0.45f, 0.5f)
val vn1_srcy1308700001_1308700798_t1309182284_t1309291439_t1309294560_t1309295107 = srcy1308700001_1308700798_t1309182284_t1309291439_t1309294560_t1309295107(0.6f, 0.47f, 0.45f, 0.5f)
val vn1_srcx1308700001_1308700798_t1309182284_t1309291439_t1309294560_t1309295107 = srcx1308700001_1308700798_t1309182284_t1309291439_t1309294560_t1309295107(0.6f, 0.47f, 0.45f, 0.5f)
val vn1_srcv1308700001_1308700798_t1309182284_t1309291439_t1309294560_t1309295107 = srcv1308700001_1308700798_t1309182284_t1309291439_t1309294560_t1309295107(0.6f, 0.47f, 0.45f, 0.5f)
val vn3_custom_f3 = custom_f3(vn1_srcx1308700001_1308700798_t1309182284_t1309291439_t1309294560_t1309295107, 0f, 0f, 0f, 0.5f, 0.5f, 0.5f)

(vn3_custom_f3, Material())
}