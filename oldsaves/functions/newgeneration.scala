(world:Vec3) => {
def result(d:Double, m:Material):(Double, Material) = {(d,m)}
def scalesrcv(scale:Double):Vec3 = {world   * scale}
def scalesrcy(scale:Double):Double = {world.y * scale}
def frichnoise3(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double):Double = {(noise3((v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub}

val vn3_scalesrcy = scalesrcy(n3_scale.value)
val vn3_scalesrcv = scalesrcv(n3_scale.value)
val vn4_frichnoise3 = frichnoise3(vn3_scalesrcv, 0.0, 0.0, 0.0, vn3_scalesrcy, 0.0, n4_size.value, n4_scale.value, n4_offset.value)
val vn1_result = result(vn4_frichnoise3, Material(0x000000))

vn1_result
}
