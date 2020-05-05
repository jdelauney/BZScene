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
unit BZSoftwareShader_SymetryDisco;

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
  BZClasses, BZMath, BZVectorMath, BZVectorMathUtils, BZRayMarchMath,
  BZColors, BZGraphic, BZBitmap,
  BZCadencer, BZCustomShader;

Type

  { TBZSoftShader_GroundAndDistortPhongSphere }

  TBZSoftShader_SymetryDisco = Class(TBZCustomSoftwareShader)
  protected
    Fax: array [0 .. 26] of double;
    Fay: array [0 .. 26] of double;
    FaTime : Double;

    procedure DoApply(var rci: Pointer; Sender: TObject); override; 
  public
    Constructor Create; override;
    Destructor Destroy; override;

    function Clone : TBZCustomSoftwareShader; override;
    
    function ShadePixelFloat:TBZColorVector; override;
  end;

implementation

{ TBZSoftShader_GroundAndDistortPhongSphere }

Constructor TBZSoftShader_SymetryDisco.Create;
begin
  inherited Create;
end;

Destructor TBZSoftShader_SymetryDisco.Destroy;
begin
  inherited Destroy;
end;

function TBZSoftShader_SymetryDisco.Clone : TBZCustomSoftwareShader;
begin
  Result := TBZSoftShader_SymetryDisco.Create;
  Result.Assign(Self);
end; 

procedure TBZSoftShader_SymetryDisco.DoApply(var rci: Pointer; Sender: TObject);
Var
   t : Double;
begin
  inherited DoApply(rci,sender);
  t := FiTime * 0.5;
  FaTime := Sin(FiTime * 0.3) * 2.0 + 3.0 + Tan(FiTime / 12.2);

  Fax[0]  := Tan(t * 3.3) * 0.3;
  Fay[0]  := Tan(t * 2.9) * 0.3;
  Fax[1]  := Tan(t * 1.9) * 0.4;
  Fay[1]  := Tan(t * 2.0) * 0.4;
  Fax[2]  := Tan(t * 0.8) * 0.4;
  Fay[2]  := Tan(t * 0.7) * 0.5;
  Fax[3]  := Tan(t * 2.3) * 0.6;
  Fay[3]  := Tan(t * 0.1) * 0.3;
  Fax[4]  := Tan(t * 0.8) * 0.5;
  Fay[4]  := Tan(t * 1.7) * 0.4;
  Fax[5]  := Tan(t * 0.3) * 0.4;
  Fay[5]  := Tan(t * 1.0) * 0.4;
  Fax[6]  := Tan(t * 1.4) * 0.4;
  Fay[6]  := Tan(t * 1.7) * 0.5;
  Fax[7]  := Tan(t * 1.3) * 0.6;
  Fay[7]  := Tan(t * 2.1) * 0.3;
  Fax[8]  := Tan(t * 1.8) * 0.5;
  Fay[8]  := Tan(t * 1.7) * 0.4;
  Fax[9]  := Tan(t * 1.2) * 0.3;
  Fay[9]  := Tan(t * 1.9) * 0.3;
  Fax[10] := Tan(t * 0.7) * 0.4;
  Fay[10] := Tan(t * 2.7) * 0.4;
  Fax[11] := Tan(t * 1.4) * 0.4;
  Fay[11] := Tan(t * 0.6) * 0.5;
  Fax[12] := Tan(t * 2.6) * 0.6;
  Fay[12] := Tan(t * 0.4) * 0.3;
  Fax[13] := Tan(t * 0.7) * 0.5;
  Fay[13] := Tan(t * 1.4) * 0.4;
  Fax[14] := Tan(t * 0.7) * 0.4;
  Fay[14] := Tan(t * 1.7) * 0.4;
  Fax[15] := Tan(t * 0.8) * 0.4;
  Fay[15] := Tan(t * 0.5) * 0.5;
  Fax[16] := Tan(t * 1.4) * 0.6;
  Fay[16] := Tan(t * 0.9) * 0.3;
  Fax[17] := Tan(t * 0.7) * 0.5;
  Fay[17] := Tan(t * 1.3) * 0.4;
  Fax[18] := Tan(t * 3.7) * 0.3;
  Fay[18] := Tan(t * 0.3) * 0.3;
  Fax[19] := Tan(t * 1.9) * 0.4;
  Fay[19] := Tan(t * 1.3) * 0.4;
  Fax[20] := Tan(t * 0.8) * 0.4;
  Fay[20] := Tan(t * 0.9) * 0.5;
  Fax[21] := Tan(t * 1.2) * 0.6;
  Fay[21] := Tan(t * 1.7) * 0.3;
  Fax[22] := Tan(t * 0.3) * 0.5;
  Fay[22] := Tan(t * 0.6) * 0.4;
  Fax[23] := Tan(t * 0.3) * 0.4;
  Fay[23] := Tan(t * 0.3) * 0.4;
  Fax[24] := Tan(t * 1.4) * 0.4;
  Fay[24] := Tan(t * 0.8) * 0.5;
  Fax[25] := Tan(t * 0.2) * 0.6;
  Fay[25] := Tan(t * 0.6) * 0.3;
  Fax[26] := Tan(t * 1.3) * 0.5;
  Fay[26] := Tan(t * 0.5) * 0.4;
  
end;  

function TBZSoftShader_SymetryDisco.ShadePixelFloat : TBZColorVector;
const 
  cZ=1/64;
  // By @paulofalcao
  //
  // Some blobs modifications with symmetries

  // nice stuff :)
  procedure makePoint(x, y: Single; i: Integer; var res:Single);
  var
    xx, yy: Single;
  begin
    xx     := x + Fax[i]; // tan(t*fx)*sx
    yy     := y - Fay[i]; // tan(t*fy)*sy
    Res := Res + (cZ*0.8 / Sqrt(System.abs(x * xx + yy * yy)));
  end;

  function sim(const p: TBZVector4f; s: Single): TBZVector4f;
  var
    sd : Single;
  begin
    sd := (s * 0.5);
    Result := p;
    Result := p + sd;
    Result := Result / s;
    Result := Result.Fract * s;
    Result := Result - sd;
  end;

  function rot(const p: TBZVector2f; r: Single): TBZVector2f;
  var sr,cr:Double;
  begin
    //Result := p.Rotate(r,cNullVector2f);
    sr := Sin(r);
    cr := Cos(r);
    Result.x := p.x * cr - p.y * sr;
    Result.y := p.x * sr + p.y * cr;
  end;

  function rotsim(const p: TBZVector2f; s: Single): TBZVector2f;
  begin
    Result := p;
    Result := rot(p, -cPI / (s + s));
    Result := rot(p, floor(Math.arctan2(Result.x, Result.y) / cPI * s) * (cPI / s));
  end;

  function makeSymmetry(const p: TBZVector2f): TBZVector2f;
  begin
    Result   := p;
    Result   := rotsim(Result, FaTime);
    Result.x := System.abs(Result.x);
  end;

  function ComputePixel(Coord:TBZVector2f; aTime:Single) : TBZColorVector;
  var
    {$CODEALIGN VARMIN=16}
    finalColor : TBZColorVector;    
    d: TBZVector4f;
    p : TBZVector2f;
    {$CODEALIGN VARMIN=4}  
    x: Single;
    y: Single;
    a,
    b,
    c: Single;
    
  begin
    //p := (gl_FragCoord.xy / resolution.x) * 2.0 - vec2.Create(1.0, resolution.y / resolution.x); //(2.0 * gl_FragCoord.xy - resolution) / resolution.y;
    //p := ((Coord * FInvResolution.x) * 2.0) - vec2(1.0,FResolution.y / FResolution.x);
    p := ((Coord * FInvResolution) * 2.0) - 1.0;
    p.x := p.x * 1.33333;
    // p := p*2.0;
    p := makeSymmetry(p);
    x := p.x;
    y := p.y;
    a := 0;
    makePoint(x, y, 0, a);
    makePoint(x, y, 1, a);
    makePoint(x, y, 2, a);
    makePoint(x, y, 3, a);
    makePoint(x, y, 4, a);
    makePoint(x, y, 5, a);
    makePoint(x, y, 6, a);
    makePoint(x, y, 7, a);
    makePoint(x, y, 8, a);

    b := -a;
    makePoint(x, y,  9, b);
    makePoint(x, y, 10, b);
    makePoint(x, y, 11, b);
    makePoint(x, y, 12, b);
    makePoint(x, y, 13, b);
    makePoint(x, y, 14, b);
    makePoint(x, y, 15, b);
    makePoint(x, y, 16, b);
    makePoint(x, y, 17, b);

    c := -b;
    makePoint(x, y, 18, c);
    makePoint(x, y, 19, c);
    makePoint(x, y, 20, c);
    makePoint(x, y, 21, c);
    makePoint(x, y, 22, c);
    makePoint(x, y, 23, c);
    makePoint(x, y, 24, c);
    makePoint(x, y, 25, c);
    makePoint(x, y, 26, c);

    Result.Create(a,b,c);
  end;
begin
  Result := ComputePixel(FragCoords, iTime);
end;

end.

