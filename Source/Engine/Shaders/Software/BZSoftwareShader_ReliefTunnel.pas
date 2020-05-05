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

  https://www.shadertoy.com/view/4sXGRn


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
unit BZSoftwareShader_ReliefTunnel;

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

  TBZSoftShader_ReliefTunnel = Class(TBZCustomSoftwareShader)
  protected
  public
    Constructor Create; override;
    Destructor Destroy; override;

    function Clone : TBZCustomSoftwareShader; override;
    
    function ShadePixelFloat:TBZColorVector; override;
  end;

implementation

{ TBZSoftShader_GroundAndDistortPhongSphere }

Constructor TBZSoftShader_ReliefTunnel.Create;
begin
  inherited Create;
end;

Destructor TBZSoftShader_ReliefTunnel.Destroy;
begin
  inherited Destroy;
end;

function TBZSoftShader_ReliefTunnel.Clone : TBZCustomSoftwareShader;
begin
  Result := TBZSoftShader_ReliefTunnel.Create;
  Result.Assign(Self);
end; 

function TBZSoftShader_ReliefTunnel.ShadePixelFloat : TBZColorVector;

  //vec2 p = (-iResolution.xy + 2.0*fragCoord)/iResolution.y;
  //
  //p *= 0.75;
  //
  //float a = atan( p.y, p.x );
  //float r = sqrt( dot(p,p) );
  //
  //a += sin(0.5*r-0.5*iTime );
  //
  //float h = 0.5 + 0.5*cos(9.0*a);
  //
  //float s = smoothstep(0.4,0.5,h);
  //
  //vec2 uv;
  //uv.x = iTime + 1.0/(r + .1*s);
  //uv.y = 3.0*a/3.1416;
  //
  //vec3 col = texture( iChannel0, uv ).xyz;
  //
  //float ao = smoothstep(0.0,0.3,h)-smoothstep(0.5,1.0,h);
  //col *= 1.0 - 0.6*ao*r;
  //col *= r;
  //
  //fragColor = vec4( col, 1.0 );

  function ComputePixel(Coord:TBZVector2f; aTime:Single) : TBZColorVector;
  var
    {$CODEALIGN RECORDMIN=16}
    p, uv: TBZVector2f;
    Col : TBZColorVector;
    {$CODEALIGN RECORDMIN=4}   
    a, r, s, w, ao,t: Double;
  begin
    //p.x := -1 + 2 * gl_FragCoord.x / Resolution.x;
    //p.y := -1 + 2 * gl_FragCoord.y / Resolution.y;
    p := (Coord * InvResolution) * 2.0 - 1.0; 
    p.x := p.x * 1.33333;
    
    r := Sqrt(p.DotProduct(p));
    a := arctan2(p.y, p.x) + 0.5 *
         Sin(0.5 * r - 0.5 * aTime);
    s := 0.5 + 0.5 * Cos(7 * a);
    ao := s;
    s := smoothstep(0, 1, s);

    if System.abs(s)<0.0001 then s := 0;
    //  s := clamp(s,0.0001,1);
    t := (r + 0.2 * s);

    // Woute van Nifterick, 2013:
    // sometimes at the end of the tunnel, t gets rounded to 0,
    // because the tunnel is basically infinite. It's usually a single pixel.
    // We can safely paint that black.
    if t<0.001 then Col.Create(0,0,0,0);
    uv.x := iTime + 1 / t;
    uv.y := 3 * a / cPi;

    w := (0.5 + 0.5 * s) * r * r;

    col := texture2D(iChannel0, uv);

    ao := smoothstep(0.0, 0.4, ao) -
          smoothstep(0.4, 0.7, ao);
    ao := 1 - 0.5 * ao * r;

    Col := Col * w;
    Result := Col * ao;
  end;
begin
  Result := ComputePixel(FragCoords, iTime);
end;

end.

