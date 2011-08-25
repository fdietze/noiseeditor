(source:Vec3) => {
val noise1 = new Noise(ClassicalGradientNoise){
	def apply(u:Vec3):Float = super.apply(u).toFloat
}
def fsrcy_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872(scale:Float, x:Float, y:Float, z:Float) = {(source.y + (y-0.5f)*256f) * (pow(256,((0.5f-scale)*2f)))}
def fsrcx_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872(scale:Float, x:Float, y:Float, z:Float) = {(source.x + (x-0.5f)*256f) * (pow(256,((0.5f-scale)*2f)))}
def fsrcv_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872(scale:Float, x:Float, y:Float, z:Float) = {(source + (Vec3(x,y,z)-0.5f)*256f) * (pow(256,((0.5f-scale)*2f)))}
def fsrcz_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872(scale:Float, x:Float, y:Float, z:Float) = {(source.z + (z-0.5f)*256f) * (pow(256,((0.5f-scale)*2f)))}
def custom_f2(a:Float, b:Float, c:Float, d:Float, s1:Float, s2:Float, s3:Float) = {object gradients {
	//util.Random.shuffle(for(i <- 0 until 16) yield { val a = i*2*Pi/16.toFloat; Vec2(cos(a),sin(a)) }).toArray
	val g2d16 = Array(Vec2(-0.9238795f, -0.38268343f), Vec2(0.38268343f, 0.9238795f), Vec2(0.707107f, -0.70710653f), Vec2(-0.9238796f, 0.38268328f), Vec2(-0.70710677f, 0.70710677f), Vec2(-4.371139E-8f, 1.0f), Vec2(-0.38268313f, -0.9238797f), Vec2(-0.70710665f, -0.7071069f), Vec2(-0.38268352f, 0.9238795f), Vec2(1.0f, 0.0f), Vec2(-1.0f, -8.742278E-8f), Vec2(0.92387956f, -0.38268343f), Vec2(1.1924881E-8f, -1.0f), Vec2(0.9238795f, 0.38268346f), Vec2(0.70710677f, 0.70710677f), Vec2(0.3826836f, -0.92387944f))
	val g3d16 = Array(Vec3( +1.000e+00f,+0.000e+00f,+0.000e+00f ),   Vec3( +4.607e-01f,-8.512e-01f,-2.514e-01f ),   Vec3( -8.579e-01f,-2.110e-01f,+4.686e-01f ),   Vec3( -1.817e-01f,-8.059e-01f,+5.635e-01f ),   Vec3( -7.634e-01f,+6.329e-01f,-1.294e-01f ),   Vec3( -1.817e-01f,-3.705e-01f,-9.109e-01f ),   Vec3( -4.443e-01f,+5.611e-01f,+6.984e-01f ),   Vec3( -1.928e-01f,+5.310e-01f,-8.252e-01f ),   Vec3( -4.443e-01f,-8.681e-01f,-2.212e-01f ),   Vec3( -8.923e-01f,-1.287e-01f,-4.326e-01f ),   Vec3( +6.039e-01f,-1.342e-01f,-7.857e-01f ),   Vec3( +6.557e-01f,-5.042e-01f,+5.620e-01f ),   Vec3( +5.900e-01f,+5.313e-01f,+6.080e-01f ),   Vec3( +2.207e-02f,-5.640e-02f,+9.982e-01f ),   Vec3( +6.039e-01f,+6.773e-01f,-4.202e-01f ),   Vec3( +2.207e-02f,+9.967e-01f,+7.794e-02f )	)
} 

def noise3(pos:Vec3,seed:Int=0) = {
	def kernel(v:Vec3, gradient:Vec3) = {
		import v.{x,y,z}
		val t = s1-(x*x + y*y + z*z)
		if( t > 0 )
			t*t*t*t*(gradient.x * x + gradient.y * y + gradient.z * z)
		else
			0f
	}
	def hash(k:Int,seed:Int=0) = (((k*0x12345678) >>> (k*0x87754351))^seed) & 0x7FFFFFFF

	def gradient(vertex:Vec3i) = {
		gradients.g3d16( hash(hash(hash(vertex.x,seed) + vertex.y,seed) + vertex.z,seed) & 15)
	}

	def fastfloor(x:Float):Int = if(x > 0) x.toInt else (x-1).toInt
	
	val floorx = fastfloor(pos.x)
	val floory = fastfloor(pos.y)
	val floorz = fastfloor(pos.z)
	
	def isbitset(a:Int, bit:Int) = (a & (1 << bit)) >> bit
	
	val vertices = new Array[Vec3i](8)
	for( i <- 0 until 8 ) {
		vertices(i) = Vec3i(
			floorx + isbitset(i,0),
			floory + isbitset(i,1),
			floorz + isbitset(i,2)
		)
	}
	
	vertices.map( vertex => kernel(pos-vertex, gradient(vertex)) ).sum*3.1604938271604937f
/*	val vertex1 = Vec3i(floorx  , floory  , floorz)
	val vertex2 = Vec3i(floorx+1, floory  , floorz)
	val vertex3 = Vec3i(floorx  , floory+1, floorz)
	val vertex4 = Vec3i(floorx+1, floory+1, floorz)
	val vertex5 = Vec3i(floorx  , floory  , floorz+1)
	val vertex6 = Vec3i(floorx+1, floory  , floorz+1)
	val vertex7 = Vec3i(floorx  , floory+1, floorz+1)
	val vertex8 = Vec3i(floorx+1, floory+1, floorz+1)

	(
	kernel(pos-vertex1, gradient(vertex1)) +
	kernel(pos-vertex2, gradient(vertex2)) +
	kernel(pos-vertex3, gradient(vertex3)) +
	kernel(pos-vertex4, gradient(vertex4)) +
	kernel(pos-vertex5, gradient(vertex5)) +
	kernel(pos-vertex6, gradient(vertex6)) +
	kernel(pos-vertex7, gradient(vertex7)) +
	kernel(pos-vertex8, gradient(vertex8)) )*3.1604938271604937f
	*/
}

noise3(Vec3(a,b,c),(s2*100).toInt)

}

val vn1_fsrcz_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872 = fsrcz_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872(0.69f, 0.5f, 0.5f, 0.5f)
val vn1_fsrcy_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872 = fsrcy_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872(0.69f, 0.5f, 0.5f, 0.5f)
val vn1_fsrcx_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872 = fsrcx_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872(0.69f, 0.5f, 0.5f, 0.5f)
val vn1_fsrcv_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872 = fsrcv_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872(0.69f, 0.5f, 0.5f, 0.5f)
val vn2_custom_f2 = custom_f2(vn1_fsrcx_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872, vn1_fsrcy_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872, vn1_fsrcz_t1309428937_t1309431402_t1309432915_t1309435462_t1309436660_t1309437545_t1309442790_t1309525872, 0f, 1.0f, 0.0f, 0.5f)

(vn2_custom_f2, Material())
}
