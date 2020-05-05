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

  ------------------------------------------------------------------------------
  @bold(Notes :)

  Quelques liens :
   @unorderedList(
       @item(http://www.shadertoy.com)
       @item(http://glslsandbox.com)
     )
  ------------------------------------------------------------------------------@br

  @bold(Credits :)
    @unorderedList(
      @item(J.Delauney (BeanzMaster))
      @item(http://glslsandbox.com/e#57162.1 )
    )

  ------------------------------------------------------------------------------@br
  LICENCE : GPL/MPL
  @br
  ------------------------------------------------------------------------------
 *==============================================================================*)
unit BZSoftwareShader_MorphingSphereAndCube;

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
  BZClasses, BZMath, BZVectorMath, BZRayMarchMath,
  BZColors, BZGraphic, BZBitmap,
  BZCadencer, BZCustomShader;

Type

  { TBZSoftShader_GroundAndDistortPhongSphere }
  TBZSoftShader_MorphingSphereAndCube = Class(TBZCustomSoftwareShader)
  protected
  public
    Constructor Create; override;
    Destructor Destroy; override;

    function Clone : TBZCustomSoftwareShader; override;

    function ShadePixelFloat:TBZColorVector; override;
  end;

implementation

uses BZLogger;

{ TBZSoftShader_GroundAndDistortPhongSphere }

Constructor TBZSoftShader_MorphingSphereAndCube.Create;
begin
  inherited Create;
end;

Destructor TBZSoftShader_MorphingSphereAndCube.Destroy;
begin
  inherited Destroy;
end;

function TBZSoftShader_MorphingSphereAndCube.Clone : TBZCustomSoftwareShader;
begin
  Result := TBZSoftShader_MorphingSphereAndCube.Create;
  Result.Assign(Self);
end;

function TBZSoftShader_MorphingSphereAndCube.ShadePixelFloat : TBZColorVector;
var
  tTime : single;

  //function sphere(p : TBZVector4f; size : Single) : Single;
  //begin
	 // Result := p.length - size;
  //end;
  //
  //function cube(p : TBZVector4f; Size : Single) : Single;
  //begin
	 // p := p.abs - size;
  //  Result := max(max(p.x,p.y),p.z);
  //end;

  function morph(p : TBZVector4f; size, t : Single) : Single;
  begin
	  //Result := sphere(p,size) * t + cube(p,size) * (1.0 - t);
    Result := DistanceSphere(p,size) * t + DistanceBox(p,size) * (1.0 - t);
  end;

  function RayMarch_Map(p : TBZVector4f): Single;
  var
    {$CODEALIGN VARMIN=16}
    p1, p2, pv :TBZVector4f;
    {$CODEALIGN VARMIN=4}
    d1, d2, d3, s, tt : Single;
  begin
    p1 := p;
    p2 := p;
    p.x := fmod(p.x,3.0) - 1.5;//modulus(p.x,3.0) - 1.5;
    p.y := fmod(p.y,3.0) - 1.5;//modulus(p.y,3.0) - 1.5;
    p1.z := modulus(p1.Z,6.0);// - 1.0;//modulus(p1.Z,8.0) - 1.0;
    //p.xz*=rot(time);
    d1 := DistanceBox(p,1.0);
    d2 := DistanceBox(p1,3.0);
    s := Sin(ttime*3.0);
    tt := ttime *5.0;
    pv.Create(s,s,tt);
    d3 := morph(p2-pv,1.0,((1.0+ Sin(tt)) * 0.5));
    Result := min(max(d1,-d2),d3);
  end;

  function ComputePixel(Coord:TBZVector2f; aTime:Single) : TBZColorVector;
  var
    {$CODEALIGN VARMIN=16}
    uv, sc :TBZVector2f;
    ro, rd,  rPos : TBZVector4f;
    finalColor : TBZColorVector;
    {$CODEALIGN VARMIN=4}
    ScreenZ, Depth, d, vc : Single;
    i : Integer;
  begin
    Result.Create(0.0, 1.0, 1.0, 1.0);
    tTime := aTime;
    //uv := (Coord * 2.0 - resolution) / min(resolution.Width,resolution.Height);
    uv :=(Coord * FInvResolution) * 2.0 - 1.0;
    uv.x := uv.x * 1.33333; // (resolution.x / resolution.y);
    //sc.Create(0.5,0.5); //(Resolution.Width * 0.5, Resolution.Height * 0.5);
    ro.Create(0,0,  -4.0 + tTime * 5.0,1.0);
   	screenZ := 1.0;
    rd.Create(uv.x, uv.Y, ScreenZ);
   	rd := rd.Normalize;
    //rd.XY.Rotate(atime * 0.3, sc);
    depth := 0.0;
   	rPos := ro;

   	for i :=0 to 63 do
    begin
   		d := RayMarch_Map(rPos);
   		if (d < 0.00001) then
      begin
        vc := i * 0.1;
   			Result.Create(vc, 0.0, vc, 1.0);
   			Break;
      end;
   		depth := Depth + d;
   		rPos := ro + (rd * depth);
    end;
  end;

begin
  //ttime := max(0.0,iTime-2.0);
  Result := ComputePixel(FragCoords, iTime);
end;

end.


