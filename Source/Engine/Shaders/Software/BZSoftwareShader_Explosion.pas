(*
@created(2019-12-27)
@author(J.Delauney (BeanzMaster))
Historique : @br
@unorderedList(
  @item(27/12/2019 : Creation  )
)
--------------------------------------------------------------------------------@br
  ------------------------------------------------------------------------------
  Description :  Simulation d'un shader GLSL

  Code source de référence de Wouter Van Nifterick

  http://glslsandbox.com/e#44943.0

  ------------------------------------------------------------------------------
  @bold(Notes :) Le rendu n'est pas bon, je ne sais as trop pourquoi ?????? Manque de précision ????

  Quelques liens :
   @unorderedList(
       @item(http://www.shadertoy.com)
       @item(http://glslsandbox.com)
     )
  ------------------------------------------------------------------------------@br

  @bold(Credits :)
    @unorderedList(
      @item(J.Delauney (BeanzMaster))
      @item(https://github.com/WouterVanNifterick/delphi-shader)
    )

  ------------------------------------------------------------------------------@br
  LICENCE : GPL/MPL
  @br
  ------------------------------------------------------------------------------
 *==============================================================================*)
unit BZSoftwareShader_Explosion;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\..\bzscene_options.inc}

//------------------------
{$ALIGN 16}
{$CODEALIGN CONSTMIN=16}
{$CODEALIGN LOCALMIN=16}
{$CODEALIGN VARMIN=16}
//------------------------
//==============================================================================
  

interface

uses
  Classes, SysUtils, Math,
  BZClasses, BZMath, BZVectorMath, BZVectorMathEx, BZRayMarchMath,
  BZColors, BZGraphic, BZBitmap,
  BZCadencer, BZCustomShader;

Type

  { TBZSoftShader_GroundAndDistortPhongSphere }

  TBZSoftShader_Explosion = Class(TBZCustomSoftwareShader)
  protected
  public
    Constructor Create; override;
    Destructor Destroy; override;

    function Clone : TBZCustomSoftwareShader; override;
    
    function ShadePixelFloat:TBZColorVector; override;
  end;

implementation

{ TBZSoftShader_GroundAndDistortPhongSphere }

Constructor TBZSoftShader_Explosion.Create;
begin
  inherited Create;
end;

Destructor TBZSoftShader_Explosion.Destroy;
begin
  inherited Destroy;
end;

function TBZSoftShader_Explosion.Clone : TBZCustomSoftwareShader;
begin
  Result := TBZSoftShader_Explosion.Create;
  Result.Assign(Self);
end; 

function TBZSoftShader_Explosion.ShadePixelFloat : TBZColorVector;

  function snoise(uv : TBZVector4f; res : Single) : Single;
  Const
    {$CODEALIGN CONSTMIN=16}
    s : TBZVector4f =(x:1e0; y : 1e2; z:1e3; w:0.0);
    vf: TBZVector4f =(x:3.0; y : 3.0; z:3.0; w:0.0);
    {$CODEALIGN CONSTMIN=4}
  var
    {$CODEALIGN VARMIN=16}
    uv0, uv1, f, v, r : TBZVector4f;
    uvi : TBZVector4i;
    {$CODEALIGN VARMIN=4}
    r0, r1, d : Double;
  begin
    uv := uv * res;
    //vec3 uv0 = floor(mod(uv, res))*s;
    uv0.x := Floor(fmod(uv.x, res));
    uv0.y := Floor(fmod(uv.y, res));
    uv0.z := Floor(fmod(uv.z, res));
    uv0 := uv0 * s;
    //vec3 uv1 = floor(mod(uv+vec3(1.), res))*s;
    v := uv + 1.0;
    uv1.x := Floor(fmod(v.x, res));
    uv1.y := Floor(fmod(v.y, res));
    uv1.z := Floor(fmod(v.z, res));
    uv1 := uv1 * s;

    f := uv.Fract;
    f :=  f * f * (vf - (f + f));

    v.Create(uv0.x+uv0.y+uv0.z, uv1.x+uv0.y+uv0.z,
		      	 uv0.x+uv1.y+uv0.z, uv1.x+uv1.y+uv0.z);

    r.Create(Fract(System.Sin(v.x*1e-1)*1e3), Fract(System.Sin(v.y*1e-1)*1e3),
             Fract(System.Sin(v.z*1e-1)*1e3), Fract(System.Sin(v.w*1e-1)*1e3));
   // r := r.Fract;
    r0 := mix(mix(r.x, r.y, f.x), mix(r.z, r.w, f.x), f.y);
    d := uv1.z - uv0.z;
    // v := v + d;
    r.Create(Fract(System.sin((v.x + d)*1e-1)*1e3), Fract(System.sin((v.y + d)*1e-1)*1e3),
             Fract(System.sin((v.z + d)*1e-1)*1e3), Fract(System.sin((v.w + d)*1e-1)*1e3));
    //r := r.fract;
    r1 := mix(mix(r.x, r.y, f.x), mix(r.z, r.w, f.x), f.y);

    Result := mix(r0, r1, f.z)*2.0-1.0;
  end;
  
  function ComputePixel(Coord:TBZVector2f; aTime:Single) : TBZColorVector;
  var
    {$CODEALIGN VARMIN=16}
    p,pt : TBZVector2f;
    nc, nt : TBZVector4f;
    {$CODEALIGN VARMIN=4}
    C, pw : Double;
    i : Integer;
  begin
	  //vec2 p = -.5 + gl_FragCoord.xy / resolution.xy;
  	//p.x *= resolution.x/resolution.y;
    p := (Coord * FInvResolution) - 0.5; //* 2.0 - 1.0;
    p.x := p.x * 1.33333;

	  pt := p + p;   //p*2.0
	  c := 3.0 - (5.0 * pt.Length );
	
	  nc.Create(Math.arctan2(p.x,p.y)*cInv2Pi + 0.5, p.Length * 0.4, 0.5);
	  nt.Create(0.0,-aTime*0.05, aTime*0.01);
    nc := nc + nt;
 	  for i := 1 to 7 do
    begin
      pw := power(2.0, i);
		  c := c + (1.5 / pw) * snoise(nc, pw*16.0);
    end;

    Result.Create(c, power(max(c,0.0),2.0)*0.4, power(max(c,0.0),3.0) * 0.15 , 1.0);
  end;

begin
  Result := ComputePixel(FragCoords, iTime);
end;

end.

