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

  http://glslsandbox.com/e#45003.0

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
      @item(https://github.com/WouterVanNifterick/delphi-shader)
    )

  ------------------------------------------------------------------------------@br
  LICENCE : GPL/MPL
  @br
  ------------------------------------------------------------------------------
 *==============================================================================*)
unit BZSoftwareShader_TwistedColoredLines;

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
  Classes, SysUtils,
  BZClasses, BZMath, BZVectorMath, BZRayMarchMath,
  BZColors, BZGraphic, BZBitmap,
  BZCadencer, BZCustomShader;

Type

  { TBZSoftShader_GroundAndDistortPhongSphere }

  TBZSoftShader_TwistedColoredLines = Class(TBZCustomSoftwareShader)
  protected
  public
    Constructor Create; override;
    Destructor Destroy; override;

    function Clone : TBZCustomSoftwareShader; override;
    
    function ShadePixelFloat:TBZColorVector; override;
  end;

implementation

{ TBZSoftShader_GroundAndDistortPhongSphere }

Constructor TBZSoftShader_TwistedColoredLines.Create;
begin
  inherited Create;
end;

Destructor TBZSoftShader_TwistedColoredLines.Destroy;
begin
  inherited Destroy;
end;

function TBZSoftShader_TwistedColoredLines.Clone : TBZCustomSoftwareShader;
begin
  Result := TBZSoftShader_TwistedColoredLines.Create;
  Result.Assign(Self);
end; 

function TBZSoftShader_TwistedColoredLines.ShadePixelFloat : TBZColorVector;
  function ComputePixel(Coord:TBZVector2f; aTime:Single) : TBZColorVector;
  // original shader :
  const
   {$CODEALIGN CONSTMIN=16}
    cORANGE : TBZColorVector = (x:10.4;y:0.8;z:0.4;w:1.0);
    cBLUE : TBZColorVector = (x:0.5; y:0.9; z:1.3;w:1.0);
    cGREEN : TBZColorVector = (x:0.9; y:1.4; z:0.4;w:1.0);
    cRED : TBZColorVector = (x:1.8; y:0.4; z:0.3;w:1.0);
    {$CODEALIGN CONSTMIN=4}
    cLines : Integer = 5;
    cBrightness : Single = 0.8;
  var
   {$CODEALIGN VARMIN=16}
   //ct1, ct2 :TBZVector4f;
   ct1, ct2, c, vt : TBZColorVector;
   uv : TBZVector2f;
   {$CODEALIGN VARMIN=4}
   t,y, x, ta,tb,tc: Single;
   i : Integer;
  begin
    c.Create(0.0);
    t := atime * 10.0;
    Coord := Coord * InvResolution; //(Coord / resolution) * 2.0 - 1.0; //
    uv.x := Coord.y;
    uv.y := Coord.x;
    x := uv.X;
    i:=-cLines;
    t:=t * 0.250;
    ta := x + t * 0.050;
    tb := x * 6.350 + t * 0.250;
    tc := x * 12.35 + t * 0.134;
    While (i<cLines) do
    begin
      y := uv.y + (
        0.150 * sin(i * 0.4 + ta)
      + 0.100 * cos(i * 0.7 + tb)
      + 0.024 * sin(i * 0.8 + tc)
      + 0.600);
      //vt.Create((1.0 - power(clamp(abs(1.0 - y)* 60.0,0.0, 1.0),0.25)));
      vt.Create((1.0 - clamp(abs(1.0 - y)* 60.0,0.0, 1.0)));
      c := c + vt;
      //i:=i+0.5;

      inc(i);
    end;

    ct1 := cORANGE.Lerp(cBLUE, uv.x);
    ct2 :=  cGREEN.Lerp(cRED, uv.x);
   // ct2 := cRed;
    c := c * ct1.Lerp(ct2,(sin(t * 0.02) + 1.0) *0.1) * cBRIGHTNESS ;

    result:= c;
    result.Alpha :=1.0;
  end;
begin
  Result := ComputePixel(FragCoords, iTime);
end;

end.

