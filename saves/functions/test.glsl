varying vec4 world;

double summedinputnoise3(vec3 v, double x, double y, double z, double add, double sub, double size, double scale, double offset) {return (noise3((v + vec3(x,y,z))*size)+offset)*scale/size + add - sub;}
Material result(double d, Material m) {return m;}
vec3 scalesrcv(double scale) {return world   * scale;}

void main() {
vec3 vn3_scalesrcv = scalesrcv(1.0);
double vn2_summedinputnoise3 = summedinputnoise3(vn3_scalesrcv, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0);
Material vn1_result = result(vn2_summedinputnoise3, Material(0x000000));

gl_FragColor = vn1_result;
}
