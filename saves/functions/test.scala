(world:Vec3) => {
def result(d:Double, m:Material):(Double, Material) = {(d,m)}
def scalesrcv(scale:Double):Vec3 = {world   * scale}
def summedinputnoise3(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double):Double = {(noise3((v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub}

val vn3_scalesrcv = scalesrcv(1.0)
val vn2_summedinputnoise3 = summedinputnoise3(vn3_scalesrcv, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0)
val vn1_result = result(vn2_summedinputnoise3, Material(0x000000))

vn1_result
}
