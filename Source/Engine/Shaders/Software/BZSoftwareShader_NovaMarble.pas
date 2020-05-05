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
      @item(http://glslsandbox.com/e#63123.1 )
    )

  ------------------------------------------------------------------------------@br
  LICENCE : GPL/MPL
  @br
  ------------------------------------------------------------------------------
 *==============================================================================*)
unit BZSoftwareShader_NovaMarble;

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
  TBZSoftShader_NovaMarble = Class(TBZCustomSoftwareShader)
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

Constructor TBZSoftShader_NovaMarble.Create;
begin
  inherited Create;
end;

Destructor TBZSoftShader_NovaMarble.Destroy;
begin
  inherited Destroy;
end;

function TBZSoftShader_NovaMarble.Clone : TBZCustomSoftwareShader;
begin
  Result := TBZSoftShader_NovaMarble.Create;
  Result.Assign(Self);
end;

function TBZSoftShader_NovaMarble.ShadePixelFloat : TBZColorVector;
var
  tTime : single;

  function RotY(p : TBZVector4f; a : Single) : TBZVector4f;
  Var
    mat3 : TBZMatrix;
  begin
    mat3.CreateRotationMatrixY(a);
    Result := p * mat3;
  end;

  function map(p : TBZVector4f) : Single;
  var
    res, f : Single;
    i : Integer;
    {$CODEALIGN VARMIN=16}
    c, p2 : TBZVector4f;
    p1 : TBZVector2f;
    {$CODEALIGN VARMIN=4}
  begin
    res := 0;
    c := p;
    for i := 0 to 9 do
    begin
      p := ((p.Abs * 0.9) / p.DotProduct(p)) - 0.7;
      p1.Create(p.y * p.y - p.z * p.z, 2.0 * p.y * p.z);
      p.Y := p1.X;
      p.X := p1.Y;
      f := abs(p.DotProduct(c));
    	res := res + System.exp(-30.0 * f);
    end;
    result := res * 0.5;
  end;

  function RayMarch(ro, rd : TBZVector4f) : TBZVector4f;
  var
    i : Integer;
    t, c, cv : Single;
    {$CODEALIGN VARMIN=16}
    p, col: TBZVector4f;
    {$CODEALIGN VARMIN=4}
  begin
    t := 1.0;
    Col.Create(0,0,0,0);
  	for i :=0 to 9 do
    begin
    	t := t + (0.02 * System.exp(-2.0*c));
    	c := map(ro + (rd * t));
      p.Create(c * 0.5, c*c, c);
      p := p * 0.10; // /10
      col := col + p;
    end;
    Result := Col;
  end;

  function ComputePixel(Coord:TBZVector2f; aTime:Single) : TBZColorVector;
  var
    {$CODEALIGN VARMIN=16}
    uv :TBZVector2f;
    ro, rd, uu, vv, c, pt : TBZVector4f;
    {$CODEALIGN VARMIN=4}
    ScreenZ, Depth, d, vc : Single;
    i : Integer;
  begin
    Result.Create(0.0, 1.0, 1.0, 1.0);
    tTime := aTime;
    uv :=(Coord * FInvResolution) * 2.0 - 1.0;
    uv.x := uv.x * 1.33333; // (resolution.x / resolution.y);
    uv := uv * 18.0;
    pt.Create(3.0,3.0,3.0,1.0);
    ro := RotY(pt, tTime * 0.1);
    pt.Create(1.0,0.0,0.0,1.0);
    uu := ro.CrossProduct(pt).Normalize;
    vv := uu.CrossProduct(ro).Normalize;
    pt := (uu * uv.X) ;
    pt := pt + (vv * uv.Y) ;
    pt := pt - (ro * 0.3);
    rd := pt.Normalize;
    c := RayMarch(ro, rd);
    c := c + RayMarch(ro, (rd * 0.1));
    c := c + RayMarch(ro, (rd * 0.2));

    Result.Create(c);
    Result.Alpha := 1.0;
  end;

begin
  //ttime := max(0.0,iTime-2.0);
  Result := ComputePixel(FragCoords, iTime);
end;

end.


