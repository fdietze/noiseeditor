(world:Vec3) => {
def result(d:Double, m:Material):(Double, Material) = {(d,m)}
def scalesrcv(scale:Double):Vec3 = {world   * scale}
def scalesrcy(scale:Double):Double = {world.y * scale}
def summedinputnoise3(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double):Double = {(noise3((v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub}

val vn2_scalesrcv = scalesrcv(1.0)
val vn2_scalesrcy = scalesrcy(1.0)
val vn4_summedinputnoise3 = summedinputnoise3(vn2_scalesrcv, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5140569133280329, 4.228072162245522, 0.0)
val vn3_summedinputnoise3 = summedinputnoise3(vn2_scalesrcv, vn4_summedinputnoise3, vn4_summedinputnoise3, vn4_summedinputnoise3, 0.0, vn2_scalesrcy, 0.32987697769322366, 1.0, 0.0)
val vn1_result = result(vn3_summedinputnoise3, Material(0x000000))

vn1_result
}
