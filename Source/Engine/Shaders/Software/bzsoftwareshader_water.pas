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
      @item(http://glslsandbox.com/e#45202.0 )
    )

  ------------------------------------------------------------------------------@br
  LICENCE : GPL/MPL
  @br
  ------------------------------------------------------------------------------
 *==============================================================================*)
unit BZSoftwareShader_Water;

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
  TBZSoftShader_Water = Class(TBZCustomSoftwareShader)
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

Constructor TBZSoftShader_Water.Create;
begin
  inherited Create;
end;

Destructor TBZSoftShader_Water.Destroy;
begin
  inherited Destroy;
end;

function TBZSoftShader_Water.Clone : TBZCustomSoftwareShader;
begin
  Result := TBZSoftShader_Water.Create;
  Result.Assign(Self);
end;

function TBZSoftShader_Water.ShadePixelFloat : TBZColorVector;
Const
  inten : Single = 0.025; // brightness; larger -> brighter
  speed : Single  = 1.5; // larger -> slower
  speed2 : Single  = 3.0; // larger -> slower
  freq : Single  = 0.8; // ripples
  xflow : Single  = 1.5; // flow speed in x direction
  yflow : Single  = 0.0; // flow speed in y direction

  function ComputePixel(Coord:TBZVector2f; aTime:Single) : TBZColorVector;
  Var
    n : Integer;
    t : Single;
    c, qc : Single;
    {$CODEALIGN VARMIN=16}
    uv, p, sp, pi : TBZVector2f;
    t1 : TBZVector2f;
    ct : TBZColorVector;
    {$CODEALIGN VARMIN=4}
  begin
    uv :=(Coord * FInvResolution) * 2.0 - 1.0;
    uv.x := uv.x * 1.33333; // (resolution.x / resolution.y);
    c := 1.0;
    sp := uv;
    p.Create(20.0,20.0);
    p := sp * 15.0 - p;
    pi := p;
    for n := 0 to 9 do
    begin
      t := aTime * (1.0 - (3.0 / (n + speed)));
      t1.Create(cos(t - pi.x * freq) + sin(t + pi.y * freq) + (atime * xflow), sin(t - pi.y * freq) + cos(t + pi.x * freq) + (atime * yflow));
    	pi := p + t1;
      t1.Create(p.x / (sin(pi.x + t * speed2) / inten), p.y / (cos(pi.y + t * speed2) / inten));
    	c := c + (1.0 / t1.length);
    end;

    c := c * 0.1;
    c := 1.5 - Sqrt(c);
    qc := c * c * c * c;
    ct.Create(0.0, 0.4, 0.55, 1.0);
    Result.Create(qc, qc, qc, 0.0);
    Result := Result + ct;
  end;

begin
  //ttime := max(0.0,iTime-2.0);
  Result := ComputePixel(FragCoords, iTime);
end;

end.


