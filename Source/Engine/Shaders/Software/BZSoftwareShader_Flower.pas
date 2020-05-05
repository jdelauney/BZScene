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
unit BZSoftwareShader_Flower;

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

  TBZSoftShader_Flower = Class(TBZCustomSoftwareShader)
  protected
  public
    Constructor Create; override;
    Destructor Destroy; override;

    function Clone : TBZCustomSoftwareShader; override;
    
    function ShadePixelFloat:TBZColorVector; override;
  end;

implementation

{ TBZSoftShader_GroundAndDistortPhongSphere }

Constructor TBZSoftShader_Flower.Create;
begin
  inherited Create;
end;

Destructor TBZSoftShader_Flower.Destroy;
begin
  inherited Destroy;
end;

function TBZSoftShader_Flower.Clone : TBZCustomSoftwareShader;
begin
  Result := TBZSoftShader_Flower.Create;
  Result.Assign(Self);
end; 

function TBZSoftShader_Flower.ShadePixelFloat : TBZColorVector;

  function uStep(Const x : Single) : Single;
  begin
    //if x>0 then Result := 1 else Result := 0;
    Result := 0.5+0.5*Sign(x);
  end;
  
  function ComputePixel(Coord:TBZVector2f; aTime:Single) : TBZColorVector;
  var
    {$CODEALIGN VARMIN=16}
    p  : TBZVector2f;
    {$CODEALIGN VARMIN=4} 
    a  : Single;
    r  : Single;
    w  : Single;
    h  : Single;
    d  : Single;
    col: Single;
    n  : Single;
  begin
    p := (Coord * FInvResolution) * 2.0 - 1.0; //(2.0 * gl_FragCoord.xy - resolution) / resolution.y;
    p.x := p.x * 1.33333;
    
    a := arctan2(p.x, p.y);
    r := p.length * 0.75;
    w := Cos(cPi * iTime - (r+r)); //cosLarge(3.1415927 * time - r * 2.0);

    h := 0.5 + 0.5 * cos(12.0 * a - w * 7 + r * 8);
    d := 0.25 + 0.75 * power(h, 1.0 * r) * (0.7 + 0.3 * w);

    n := 1.0 - r / d;

    if n <= 0 then
    begin
      col := 0
    end
    else
    begin
      col  := uStep(d - r) * Sqrt(n) * r * 2.5;
    end;
    col    := col * (1.25 + 0.25 * Cos((12 * a - w * 7 + r * 8) / 2));
    col    := col * (1 - 0.35 * (0.5 + 0.5 * Sin(r * 30)) * (0.5 + 0.5 * Cos(12 * a - w * 7 + r * 8)));
    Result.Create(col, col - h * 0.5 + r * 0.2 + 0.35 * h * (1 - r), col - h * r + 0.1 * h * (1 - r), 1.0);
  end;
begin
  Result := ComputePixel(FragCoords, iTime);
end;

end.

