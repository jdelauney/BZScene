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
      @item(http://glslsandbox.com/e#55798.0 )
    )

  ------------------------------------------------------------------------------@br
  LICENCE : GPL/MPL
  @br
  ------------------------------------------------------------------------------
 *==============================================================================*)
unit BZSoftwareShader_WaterPaint;

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
  TBZSoftShader_WaterPaint = Class(TBZCustomSoftwareShader)
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

Constructor TBZSoftShader_WaterPaint.Create;
begin
  inherited Create;
end;

Destructor TBZSoftShader_WaterPaint.Destroy;
begin
  inherited Destroy;
end;

function TBZSoftShader_WaterPaint.Clone : TBZCustomSoftwareShader;
begin
  Result := TBZSoftShader_WaterPaint.Create;
  Result.Assign(Self);
end;

function TBZSoftShader_WaterPaint.ShadePixelFloat : TBZColorVector;


  function palette( t : Single;  a,  b, c, d : TBZVector4f ) : TBZVector4f;
  Var
    {$CODEALIGN VARMIN=16}
    ct : TBZVector4f;
    {$CODEALIGN VARMIN=4}
  begin
    ct := (d + (c * t)) * 6.28318;
    ct.x := Cos(ct.X);
    ct.y := Cos(ct.Y);
    ct.z := Cos(ct.Z);
    ct.w := 1.0;
    Result := a + (b * ct);
  end;

  function ComputePixel(Coord:TBZVector2f; aTime:Single) : TBZColorVector;
  Var
    i : Integer;
    tt, r : Single;
    {$CODEALIGN VARMIN=16}
    uv : TBZVector2f;
    a, b, c, ct : TBZVector4f;

    {$CODEALIGN VARMIN=4}
  begin
    //uv :=(Coord * FInvResolution) * 2.0 - 1.0;
    //uv.x := uv.x * 1.33333; // (resolution.x / resolution.y);
    uv := Coord / resolution.Width * 0.9;
    tt := aTime * 0.1;

    for i := 1 to 19 do
    begin
      uv.x := uv.x + 0.1 / i * sin(i * 7.0 * uv.y + tt + cos((tt / (20.0 * i)) * i));
     	uv.y := uv.y + 0.1 / i * cos(i * 10.0 * uv.x + tt + sin((tt / (15.0 * i)) * i));
    end;
    r := abs(uv.x + uv.y);
    	//gl_FragColor = vec4(palette(col.r, vec3(0.8, 0.5, 0.4), vec3(0.2, 0.4, 0.2), vec3(2.0, 1.0, 1.0), vec3(0.00, 0.25, 0.25)), 1.0);
    a.Create(0.5, 0.5, 0.5);
    b.Create(1.0, 1.0, 1.0);
    c.Create(0.00, 0.10, 0.20);

   	ct := palette(r, a, a, b, c);
    Result.Create(ct.x, ct.y, ct.z,1.0);
  end;

begin
  //ttime := max(0.0,iTime-2.0);
  Result := ComputePixel(FragCoords, iTime);
end;

end.



