unit BZRayMarchMath;

// from http://www.iquilezles.org/www/articles
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}

{$CODEALIGN LOCALMIN=16}
interface

uses
  Classes, SysUtils,
  BZMath, BZVectorMath;

//-----[ Ray Marching functions ]-----------------------------------------------

{
 A list of useful distance function to simple primitives, and an example on how to
 do some interesting boolean operations, repetition and displacement.

  More info here: http://www.iquilezles.org/www/articles/distfunctions/distfunctions.htm

  Licence : MIT

  NOTICE :
  All primitives are centered at the origin.
  You will have to transform the point to get arbitrarily rotated, translated and scaled objects
}

// Signed distance
function DistanceBox(P:TBZVector; S : Single):Single; overload;
Function DistanceBox(p,b:TBZVector4f):Single;   overload;

function DistanceSphere(P:TBZVector; S : Single):Single;
function DistanceTorus(P:TBZVector; S : Single):Single;
function DistanceTorus82(P:TBZVector; S : Single):Single;
function DistanceTorus88(P:TBZVector; S : Single):Single;
function DistanceCylinder(P:TBZVector; S : Single):Single;
function DistanceCone(P:TBZVector; S : Single):Single;
function DistancePlane(P:TBZVector; S : Single):TBZVector2f;
function DistanceHexaGonalPrism(P:TBZVector; S : Single):Single;
function DistanceTriangularPrism(P:TBZVector; S : Single):Single;
function DistanceCapsuleLine(P:TBZVector; S : Single):Single;
function DistanceCappedCylinder(P:TBZVector; S : Single):Single;
function DistanceCappedCone(P:TBZVector; S : Single):Single;
function DistanceEllipsoid(P:TBZVector; S : Single):Single;
// Unsigned Distance
function uDistanceTriangle(P:TBZVector; S : Single):Single;
function uDistanceQuad(P:TBZVector; S : Single):Single;
function uDistanceBox(P:TBZVector; S : Single):Single;
function uDistanceRoundBox(P:TBZVector; S : Single):Single;

// Operation
function opUnion(d1, d2 : TBZVector2f):TBZVector2f;
function opSubstract(d1, d2 : TBZVector2f):TBZVector2f;
function opIntersect(d1, d2 : Single):Single;

function opRepeat(p, c : TBZVector4f):TBZVector4f;
function opRotateTranslate(d1, d2 : Single):Single;
function opScale(d1, d2 : Single):Single;

function opDisplace(v:TBZVector):Single;
function opBlend(v:TBZVector):Single;
function opTwist(p:TBZVector):TBZVector4f;
function opCheapBend(v:TBZVector):Single;



implementation

//------------------------------------------------------------------
// The distance object function takes in a point along the ray
// we are marching and parameters of object (eg radius for sphere).  
// The Distance function will then return the distance from the input 
// point p to the closest point on the sphere.  
// The objets are assumed to be centered on the origin which is (0,0,0).

Function DistancePlane(p:TBZVector4f; s:single):TBZVector2f;
begin
  Result.x := p.y+s;
  Result.y:= 0;
end;

Function DistanceSphere(p:TBZVector4f; s: Single ):Single;
begin
  result := p.Length - s;
end;

Function DistanceBox(p,b:TBZVector4f):Single;
var
  d: TBZVector4f;
begin
  d := p.Abs - b;
  Result := min(d.MaxXYZComponent,0.0) + (d.max(0.0)).length;
end;

Function DistanceEllipsoid(p,r : TBZVector4f):Single;
begin
    //return (length( p/r ) - 1.0) * min(min(r.x,r.y),r.z);
end;

Function uDistanceRoundBox(p,b:TBZVector4f; r:Single ):Single;
begin
    //return length(max(abs(p)-b,0.0))-r;
end;

Function DistanceTorus(p:TBZVector4f; t : TBZVector2f ):Single;
begin
    //return length( vec2(length(p.xz)-t.x,p.y) )-t.y;
end;

Function DistanceHexPrism(p:TBZVector4f; h:TBZVector2f ):Single;
begin
//    vec3 q = abs(p);
//#if 0
//    return max(q.z-h.y,max((q.x*0.866025+q.y*0.5),q.y)-h.x);
//#else
//    float d1 = q.z-h.y;
//    float d2 = max((q.x*0.866025+q.y*0.5),q.y)-h.x;
//    return length(max(vec2(d1,d2),0.0)) + min(max(d1,d2), 0.);
//#endif
end;

Function DistanceCapsule(p,a,b:TBZVector4f; r:Single ):Single;
begin
	//vec3 pa = p-a, ba = b-a;
	//float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
	//return length( pa - ba*h ) - r;
end;

Function DistanceEquilateralTriangle(p:TBZVector2f):Single;
begin
    //const float k = sqrt(3.0);
    //p.x = abs(p.x) - 1.0;
    //p.y = p.y + 1.0/k;
    //if( p.x + k*p.y > 0.0 ) p = vec2( p.x - k*p.y, -k*p.x - p.y )/2.0;
    //p.x += 2.0 - 2.0*clamp( (p.x+2.0)/2.0, 0.0, 1.0 );
    //return -length(p)*sign(p.y);
end;

Function DistanceTriPrism(p:TBZVector4f; h:TBZVector2f ):Single;
begin
//    vec3 q = abs(p);
//    float d1 = q.z-h.y;
//#if 1
//    // distance bound
//    float d2 = max(q.x*0.866025+p.y*0.5,-p.y)-h.x*0.5;
//#else
//    // correct distance
//    h.x *= 0.866025;
//    float d2 = sdEquilateralTriangle(p.xy/h.x)*h.x;
//#endif
//    return length(max(vec2(d1,d2),0.0)) + min(max(d1,d2), 0.);
end;

Function DistanceCylinder(p:TBZVector4f; h:TBZVector2f ):Single;
begin
  //vec2 d = abs(vec2(length(p.xz),p.y)) - h;
  //return min(max(d.x,d.y),0.0) + length(max(d,0.0));
end;

Function DistanceCone(p,c:TBZVector4f):Single;
begin
    //vec2 q = vec2( length(p.xz), p.y );
    //float d1 = -q.y-c.z;
    //float d2 = max( dot(q,c.xy), q.y);
    //return length(max(vec2(d1,d2),0.0)) + min(max(d1,d2), 0.);
end;

Function DistanceConeSection(p:TBZVector4f;h,r1,r2 : Single ):Single;
begin
    //float d1 = -p.y - h;
    //float q = p.y - h;
    //float si = 0.5*(r1-r2)/h;
    //float d2 = max( sqrt( dot(p.xz,p.xz)*(1.0-si*si)) + q*si - r2, q );
    //return length(max(vec2(d1,d2),0.0)) + min(max(d1,d2), 0.);
end;

Function DistancePryamid4(p, h : TBZVector4f ):Single; // h = { cos a, sin a, height }
begin
    // Tetrahedron = Octahedron - Cube
    //float box = sdBox( p - vec3(0,-2.0*h.z,0), vec3(2.0*h.z) );
    //
    //float d = 0.0;
    //d = max( d, abs( dot(p, vec3( -h.x, h.y, 0 )) ));
    //d = max( d, abs( dot(p, vec3(  h.x, h.y, 0 )) ));
    //d = max( d, abs( dot(p, vec3(  0, h.y, h.x )) ));
    //d = max( d, abs( dot(p, vec3(  0, h.y,-h.x )) ));
    //float octa = d - h.z;
    //return max(-box,octa); // Subtraction
end;

Function length2(p:TBZVector2f ):Single;
begin
	result := p.Length;
end;


Function length6(p:TBZVector2f ):Single;
begin
	p := p*p*p;
  p := p*p;
	result := pow( p.x + p.y, 1.0/6.0 );
end;

Function length8(p:TBZVector2f ):Single;
begin
	p := p*p;
  p := p*p;
  p := p*p;
	Result := pow( p.x + p.y, 1.0/8.0 );
end;

Function DistanceTorus82(p:TBZVector4f; t:TBZVector2f ):Single;
var q:TBZVector2f;
begin
    //q.Create(length2(p.xz)-t.x,p.y);
    Result := Length8(q)-t.y;
end;

Function DistanceTorus88(p:TBZVector4f; t:TBZVector2f ):Single;
begin
    //vec2 q = vec2(length8(p.xz)-t.x,p.y);
    //return length8(q)-t.y;
end;

Function DistanceCylinder6( p:TBZVector4f; h:TBZVector2f ):Single;
begin
   // return max( length6(p.xz)-h.x, abs(p.y)-h.y );
end;

Function uDistanceSegment(p,a,b:TBZVector4f):Single;
begin
	//vec3 pa = p-a, ba = b-a;
	//float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
	//return vec2( length( pa - ba*h ), h );
end;

// http://research.microsoft.com/en-us/um/people/hoppe/ravg.pdf
(* float det( vec2 a, vec2 b ) { return a.x*b.y-b.x*a.y; }
vec3 getClosest( vec2 b0, vec2 b1, vec2 b2 )
{

  float a =     det(b0,b2);
  float b = 2.0*det(b1,b0);
  float d = 2.0*det(b2,b1);
  float f = b*d - a*a;
  vec2  d21 = b2-b1;
  vec2  d10 = b1-b0;
  vec2  d20 = b2-b0;
  vec2  gf = 2.0*(b*d21+d*d10+a*d20); gf = vec2(gf.y,-gf.x);
  vec2  pp = -f*gf/dot(gf,gf);
  vec2  d0p = b0-pp;
  float ap = det(d0p,d20);
  float bp = 2.0*det(d10,d0p);
  float t = clamp( (ap+bp)/(2.0*a+b+d), 0.0 ,1.0 );
  return vec3( mix(mix(b0,b1,t), mix(b1,b2,t),t), t );
}

vec2 sdBezier( vec3 a, vec3 b, vec3 c, vec3 p, out vec2 pos )
{
	vec3 w = normalize( cross( c-b, a-b ) );
	vec3 u = normalize( c-b );
	vec3 v = normalize( cross( w, u ) );

	vec2 a2 = vec2( dot(a-b,u), dot(a-b,v) );
	vec2 b2 = vec2( 0.0 );
	vec2 c2 = vec2( dot(c-b,u), dot(c-b,v) );
	vec3 p3 = vec3( dot(p-b,u), dot(p-b,v), dot(p-b,w) );

	vec3 cp = getClosest( a2-p3.xy, b2-p3.xy, c2-p3.xy );

    pos = cp.xy;

	return vec2( sqrt(dot(cp.xy,cp.xy)+p3.z*p3.z), cp.z );
}

float calcSoftShadow( in vec3 ro, in vec3 rd, float k )
{
    vec3 kk;
    float res = 1.0;
    float t = 0.01;
    for( int i=0; i<32; i++ )
    {
        float h = map(ro + rd*t, kk ).x;
        res = min( res, smoothstep(0.0,1.0,k*h/t) );
        t += clamp( h, 0.05, 0.5 );
		if( res<0.01 ) break;
    }
    return clamp(res,0.0,1.0);
}


float calcAO( in vec3 pos, in vec3 nor )
{
    vec3 kk;
	float ao = 0.0;
    for( int i=0; i<32; i++ )
    {
        vec3 ap = forwardSF( float(i), 32.0 );
        float h = hash1(float(i));
		ap *= sign( dot(ap,nor) ) * h*0.3;
        ao += clamp( mapWithTerrain( pos + nor*0.01 + ap, kk ).x*1.0/h, 0.0, 1.0 );
    }
	ao /= 32.0;

    return clamp( ao*4.0*(1.0+0.25*nor.y), 0.0, 1.0 );
}


float calcSSS( in vec3 pos, in vec3 nor )
{
    float ao = 1.0;
    float totao = 0.0;
    float sca = 1.0;
    for( int aoi=0; aoi<5; aoi++ )
    {
        float hr = 0.01 + 0.4*float(aoi)/4.0;
        vec3 aopos =  nor * hr + pos;
        float dd = map( aopos ).x;
        totao += (hr-min(dd,0.0))*sca;
        sca *= 0.9;
    }
    return pow( clamp( 1.2 - 0.25*totao, 0.0, 1.0 ), 16.0 );
}

float smin( float a, float b, float k )
{
	float h = clamp( 0.5 + 0.5*(b-a)/k, 0.0, 1.0 );
	return mix( b, a, h ) - k*h*(1.0-h);
}

vec2 smin( vec2 a, vec2 b, float k )
{
	float h = clamp( 0.5 + 0.5*(b.x-a.x)/k, 0.0, 1.0 );
	return vec2( mix( b.x, a.x, h ) - k*h*(1.0-h), mix( b.y, a.y, h ) );
}

float smax( float a, float b, float k )
{
	float h = clamp( 0.5 + 0.5*(b-a)/k, 0.0, 1.0 );
	return mix( a, b, h ) + k*h*(1.0-h);
}

*)

Function opSubstract(d1,d2:TBZVector2f):TBZVector2f;
begin
    Result := d1.Max(-d2);// max(-d2,d1);
end;

Function opUnion(d1,d2:TBZVector2f):TBZVector2f;
begin
  if (d1.x<d2.x) then result:=d1 else result:=d2;
end;

Function opRepeat(p,c : TBZVector4f ):TBZVector4f;
begin
    //result := mod(p,c)-0.5*c;
end;

Function opTwist(p: TBZVector4f): TBZVector4f;
begin
    //float  c = cos(10.0*p.y+10.0);
    //float  s = sin(10.0*p.y+10.0);
    //mat2   m = mat2(c,-s,s,c);
    //return vec3(m*p.xz,p.y);
end;

function DistanceBox(P: TBZVector; S: Single): Single;
begin
  p := p.Abs - S;
  Result := max(max(p.x,p.y),p.z);
end;

function DistanceTorus(P: TBZVector; S: Single): Single;
begin

end;

function DistanceTorus82(P: TBZVector; S: Single): Single;
begin

end;

function DistanceTorus88(P: TBZVector; S: Single): Single;
begin

end;

function DistanceCylinder(P: TBZVector; S: Single): Single;
begin

end;

function DistanceCone(P: TBZVector; S: Single): Single;
begin

end;



function DistanceHexaGonalPrism(P: TBZVector; S: Single): Single;
begin

end;

function DistanceTriangularPrism(P: TBZVector; S: Single): Single;
begin

end;

function DistanceCapsuleLine(P: TBZVector; S: Single): Single;
begin

end;

function DistanceCappedCylinder(P: TBZVector; S: Single): Single;
begin

end;

function DistanceCappedCone(P: TBZVector; S: Single): Single;
begin

end;

function DistanceEllipsoid(P: TBZVector; S: Single): Single;
begin

end;

function uDistanceTriangle(P: TBZVector; S: Single): Single;
begin

end;

function uDistanceQuad(P: TBZVector; S: Single): Single;
begin

end;

function uDistanceBox(P: TBZVector; S: Single): Single;
begin

end;

function uDistanceRoundBox(P: TBZVector; S: Single): Single;
begin

end;


function opIntersect(d1, d2: Single): Single;
begin

end;

function opRotateTranslate(d1, d2: Single): Single;
begin

end;

function opScale(d1, d2: Single): Single;
begin

end;

function opDisplace(v: TBZVector): Single;
begin

end;

function opBlend(v: TBZVector): Single;
begin

end;

function opCheapBend(v: TBZVector): Single;
begin

end;

(*

vec3 render( in vec3 ro, in vec3 rd )
{
    vec3 col = vec3(0.7, 0.9, 1.0) +rd.y*0.8;
    vec2 res = castRay(ro,rd);
    float t = res.x;
	float m = res.y;
    if( m>-0.5 )
    {
        vec3 pos = ro + t*rd;
        vec3 nor = calcNormal( pos );
        vec3 ref = reflect( rd, nor );

        // material
		col = 0.45 + 0.35*sin( vec3(0.05,0.08,0.10)*(m-1.0) );
        if( m<1.5 )
        {

            float f = checkersGradBox( 5.0*pos.xz );
            col = 0.3 + f*vec3(0.1);
        }

        // lighitng
        float occ = calcAO( pos, nor );
		vec3  lig = normalize( vec3(-0.4, 0.7, -0.6) );
        vec3  hal = normalize( lig-rd );
		float amb = clamp( 0.5+0.5*nor.y, 0.0, 1.0 );
        float dif = clamp( dot( nor, lig ), 0.0, 1.0 );
        float bac = clamp( dot( nor, normalize(vec3(-lig.x,0.0,-lig.z))), 0.0, 1.0 )*clamp( 1.0-pos.y,0.0,1.0);
        float dom = smoothstep( -0.1, 0.1, ref.y );
        float fre = pow( clamp(1.0+dot(nor,rd),0.0,1.0), 2.0 );

        dif *= calcSoftshadow( pos, lig, 0.02, 2.5 );
        dom *= calcSoftshadow( pos, ref, 0.02, 2.5 );

		float spe = pow( clamp( dot( nor, hal ), 0.0, 1.0 ),16.0)*
                    dif *
                    (0.04 + 0.96*pow( clamp(1.0+dot(hal,rd),0.0,1.0), 5.0 ));

		vec3 lin = vec3(0.0);
        lin += 1.30*dif*vec3(1.00,0.80,0.55);
        lin += 0.40*amb*vec3(0.40,0.60,1.00)*occ;
        lin += 0.50*dom*vec3(0.40,0.60,1.00)*occ;
        lin += 0.50*bac*vec3(0.25,0.25,0.25)*occ;
        lin += 0.25*fre*vec3(1.00,1.00,1.00)*occ;
		col = col*lin;
		col += 10.00*spe*vec3(1.00,0.90,0.70);

    	col = mix( col, vec3(0.8,0.9,1.0), 1.0-exp( -0.0002*t*t*t ) );
    }

	return vec3( clamp(col,0.0,1.0) );
}

mat3 setCamera( in vec3 ro, in vec3 ta, float cr )
{
	vec3 cw = normalize(ta-ro);
	vec3 cp = vec3(sin(cr), cos(cr),0.0);
	vec3 cu = normalize( cross(cw,cp) );
	vec3 cv = normalize( cross(cu,cw) );
    return mat3( cu, cv, cw );
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 mo = iMouse.xy/iResolution.xy;
	float time = 15.0 + iTime;


    vec3 tot = vec3(0.0);
#if AA>1
    for( int m=0; m<AA; m++ )
    for( int n=0; n<AA; n++ )
    {
        // pixel coordinates
        vec2 o = vec2(float(m),float(n)) / float(AA) - 0.5;
        vec2 p = (-iResolution.xy + 2.0*(fragCoord+o))/iResolution.y;
#else
        vec2 p = (-iResolution.xy + 2.0*fragCoord)/iResolution.y;
#endif

		// camera
        vec3 ro = vec3( -0.5+3.5*cos(0.1*time + 6.0*mo.x), 1.0 + 2.0*mo.y, 0.5 + 4.0*sin(0.1*time + 6.0*mo.x) );
        vec3 ta = vec3( -0.5, -0.4, 0.5 );
        // camera-to-world transformation
        mat3 ca = setCamera( ro, ta, 0.0 );
        // ray direction
        vec3 rd = ca * normalize( vec3(p.xy,2.0) );

        // render
        vec3 col = render( ro, rd );

		// gamma
        col = pow( col, vec3(0.4545) );

        tot += col;
#if AA>1
    }
    tot /= float(AA*AA);
#endif


    fragColor = vec4( tot, 1.0 );
}
*)

end.

