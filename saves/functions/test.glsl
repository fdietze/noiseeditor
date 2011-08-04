#version 120
#extension GL_EXT_gpu_shader4 : enable
//#extension GL_ARB_gpu_shader_fp64 : enable

varying vec3 vertex;
varying vec3 normal;
varying vec4 world;

/*
possible vertex shader:
#version 120

varying vec3 vertex;
varying vec3 normal;
varying vec4 world;

void main () {
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;

	vertex = vec3(gl_ModelViewMatrix * gl_Vertex);       
	normal = normalize(gl_NormalMatrix * gl_Normal);
    world  = gl_Vertex;
}
*/

int seed = 0;
int a = (seed ^ int(0xB5C18E6A)) | ((1 << 16) + 1);
int c = seed ^ int(0xF292D0B2);
int hash(int x){ return (a*(x ^ c)) >> 16; }

int fastfloor(float x) { return int( x > 0 ? x : x-1); }
float fade(float t) { return t * t * t * (t * (t * 6 - 15) + 10); }
float lerp(float t, float a, float b) { return a + t * (b - a); }

float grad(int hash, float x, float y, float z) {
      int h = hash & 0xF;
      float u = h<8 ? x : y,
             v = h<4 ? y : h==12||h==14 ? x : z;
      return ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
}

float noise3(float x, float y, float z) {
	int X = fastfloor(x);
	int Y = fastfloor(y);
	int Z = fastfloor(z);

	float relx = x - float(X);
	float rely = y - float(Y);
	float relz = z - float(Z);

	float u = fade(relx);
	float v = fade(rely);
	float w = fade(relz);
	
	int A = hash(X  )+Y; int AA = hash(A)+Z; int AB = hash(A+1)+Z;
	int	B = hash(X+1)+Y; int BA = hash(B)+Z; int BB = hash(B+1)+Z;

	return lerp(w,	lerp(v,	lerp(u, grad(hash(AA  ), relx  , rely  , relz	),
									grad(hash(BA  ), relx-1, rely  , relz	)),
							lerp(u, grad(hash(AB  ), relx  , rely-1, relz	),
									grad(hash(BB  ), relx-1, rely-1, relz	))),
					lerp(v, lerp(u, grad(hash(AA+1), relx  , rely  , relz-1 ),
									grad(hash(BA+1), relx-1, rely  , relz-1 )),
							lerp(u, grad(hash(AB+1), relx  , rely-1, relz-1 ),
									grad(hash(BB+1), relx-1, rely-1, relz-1 ))));
}

float noise3(vec3 v) {return noise3(v.x, v.y, v.z);}

/////////////////////////////////////////////////////

vec4 result(float d, vec4 m) {return m;}
vec3 scalesrcv(float scale) {return world.xyz * scale;}
float summedinputnoise3(vec3 v, float x, float y, float z, float add, float sub, float size, float scale, float offset) {return (noise3((v + vec3(x,y,z))*size)+offset)*scale/size + add - sub;}
vec4 matrgb(float r, float g, float b) {return vec4(r, g, b, 0.0);}
vec4 matthreshold(vec4 m1, float t, vec4 m2) {return t >= 0 ? m1 : m2;}



void main(){

vec3 vn3_scalesrcv = scalesrcv(1.0);
float vn2_summedinputnoise3 = summedinputnoise3(vn3_scalesrcv, 0.0, 0.0, 0.0, 0.0, 0.0, 0.10881882041201557, 1.9453098948245722, 0.3799999999999999);
vec4 vn5_matrgb = matrgb(0.05, 0.41, 0.36);
float vn9_summedinputnoise3 = summedinputnoise3(vn3_scalesrcv, vn2_summedinputnoise3, vn2_summedinputnoise3, vn2_summedinputnoise3, 0.0, 0.0, 0.21168632809063176, 11.471641984126617, 0.17999999999999994);
vec4 vn6_matrgb = matrgb(0.35, 0.78, 0.0);
vec3 vn7_scalesrcv = scalesrcv(1.0);
vec4 vn4_matthreshold = matthreshold(vn6_matrgb, vn9_summedinputnoise3, vn5_matrgb);
float vn1_summedinputnoise3 = summedinputnoise3(vn7_scalesrcv, 0.0, 0.0, 0.0, 0.0, 0.0, 0.09739557245756253, 1.0, 0.19999999999999996);
vec4 vn8_result = result(vn1_summedinputnoise3, vn4_matthreshold);


	vec4 materialcolor = vn8_result;
	
	vec3 L = normalize(gl_LightSource[0].position.xyz - vertex);   
	vec4 Idiff = clamp(gl_FrontLightProduct[0].diffuse * max(dot(normal,L), 0.0), 0.0, 1.0);  

	gl_FragColor = materialcolor * Idiff;
}


