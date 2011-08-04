def proceduralworld(world:Vec3) = {
def result(d:Double, m:Material):(Double, Material) = {(d,m)}
def summedinputnoise3(v:Vec3, x:Double, y:Double, z:Double, add:Double, sub:Double, size:Double, scale:Double, offset:Double):Double = {(noise3((v + Vec3(x,y,z))*size)+offset)*scale/size + add - sub}
def matthreshold(m1:Material, t:Double, m2:Material):Material = {if(t >= 0) m1 else m2;}
def matrgb(r:Double, g:Double, b:Double):Material = {Material((r*255).toInt << 16 | (g*255).toInt << 8 | (b*255).toInt);}
def scalesrcv(scale:Double):Vec3 = {world   * scale}

val vn3_scalesrcv = scalesrcv(1.0)
val vn2_summedinputnoise3 = summedinputnoise3(vn3_scalesrcv, 0.0, 0.0, 0.0, 0.0, 0.0, 0.10881882041201557, 1.9453098948245722, 0.3799999999999999)
val vn5_matrgb = matrgb(0.05, 0.41, 0.36)
val vn9_summedinputnoise3 = summedinputnoise3(vn3_scalesrcv, vn2_summedinputnoise3, vn2_summedinputnoise3, vn2_summedinputnoise3, 0.0, 0.0, 0.21168632809063176, 11.471641984126617, 0.17999999999999994)
val vn6_matrgb = matrgb(0.35, 0.78, 0.0)
val vn7_scalesrcv = scalesrcv(1.0)
val vn4_matthreshold = matthreshold(vn6_matrgb, vn9_summedinputnoise3, vn5_matrgb)
val vn1_summedinputnoise3 = summedinputnoise3(vn7_scalesrcv, 0.0, 0.0, 0.0, 0.0, 0.0, 0.09739557245756253, 1.0, 0.19999999999999996)
val vn8_result = result(vn1_summedinputnoise3, vn4_matthreshold)

vn8_result
}
