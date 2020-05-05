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
unit BZSoftwareShader_FuzzyMandelbrot;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BZClasses, BZMath, BZVectorMath, BZVectorMathUtils, //BZRayMarchMath,
  BZColors, BZGraphic, BZBitmap,
  BZCadencer, BZCustomShader;

Type

  { TBZSoftShader_GroundAndDistortPhongSphere }

  TBZSoftShader_FuzzyMandelbrot = Class(TBZCustomSoftwareShader)
  protected
  public
    Constructor Create; override;
    Destructor Destroy; override;

    function Clone : TBZCustomSoftwareShader; override;
    
    function ShadePixelFloat:TBZColorVector; override;
  end;

implementation

{ TBZSoftShader_GroundAndDistortPhongSphere }

Constructor TBZSoftShader_FuzzyMandelbrot.Create;
begin
  inherited Create;
end;

Destructor TBZSoftShader_FuzzyMandelbrot.Destroy;
begin
  inherited Destroy;
end;

function TBZSoftShader_FuzzyMandelbrot.Clone : TBZCustomSoftwareShader;
begin
  Result := TBZSoftShader_FuzzyMandelbrot.Create;
  Result.Assign(Self);
end; 

function TBZSoftShader_FuzzyMandelbrot.ShadePixelFloat : TBZColorVector;

  function ComputePixel(Coord:TBZVector2f; aTime:Single) : TBZColorVector;
  const
    iterations : integer = 32;
  var
    {$CODEALIGN RECORDMIN=16}
    vZ, vC,  uv, r : TBZVector2f;
    {$CODEALIGN RECORDMIN=4}
    color, scale : Single;
    iteration, i : Integer;
  begin
    r.Create(Resolution.X, Resolution.Y);
    uv := (Coord / r) * 2.0 - 1.0;   //resolution

    Scale := sin(aTime*0.15)*4+2.0;

    vZ := vec2(0.5,0.5);
    vZ :=vZ + vec2((sin(iTime*0.25)*1.25)-0.75, cos(iTime*0.15)*0.5);
    vC := uv * Scale;

    iteration := 1;
    for i := 0 to iterations do
    begin
      vZ := vec2(vZ.x * vZ.x - vZ.y * vZ.y, 2.0 * vZ.x * vZ.y) + vC;
      if (vZ.DotProduct(vZ) > 4.0) then
      begin
        iteration := i;
        break;
      end;
    end;
    color := iteration / Iterations;
    Result.Create(color+0.2, color+0.4, color+0.8, 1.0);
  end;

begin
  Result := ComputePixel(FragCoords, iTime);
end;

end.

