(source:Vec3) => {
val noise1 = new Noise(ClassicalGradientNoise){
	def apply(u:Vec3):Double = super.apply(u)
}
def srcxyzy_t1311862866() = {source.y}
def frichnoise3_t1311862866(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double) = {
			val sumv = v + Vec3(x,y,z)
			(noise1(sumv*size)+offset)*scale/size + add - sub
			}
def srcv_t1311862866() = {source}
def srcxyzx_t1311862866() = {source.x}
def frichnoise3(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double) = {
			val sumv = v + Vec3(x,y,z)
			(noise1(sumv*size)+offset)*scale/size + add - sub
			}
def srcxyzz_t1311862866() = {source.z}

val vn1_srcxyzz_t1311862866 = srcxyzz_t1311862866()
val vn1_srcxyzy_t1311862866 = srcxyzy_t1311862866()
val vn1_srcxyzx_t1311862866 = srcxyzx_t1311862866()
val vn1_srcv_t1311862866 = srcv_t1311862866()
val vn5_frichnoise3 = frichnoise3(vn1_srcv_t1311862866, 0.0, 0.0, 0.0, 0.0, 0.0, 2.428389768790094, 0.32987697769322366, 0.020000000000000018)
val vn4_frichnoise3 = frichnoise3(vn1_srcv_t1311862866, vn5_frichnoise3, vn5_frichnoise3, vn5_frichnoise3, 0.0, 0.0, 1.11728713807222, 0.7169776240079135, 0.040000000000000036)
val vn2_frichnoise3_t1311862866 = frichnoise3_t1311862866(vn1_srcv_t1311862866, vn4_frichnoise3, vn4_frichnoise3, vn4_frichnoise3, 0.0, 0.0, 0.8950250709279723, 1.248330548901612, 0.10000000000000009)

(vn2_frichnoise3_t1311862866, Material())
}