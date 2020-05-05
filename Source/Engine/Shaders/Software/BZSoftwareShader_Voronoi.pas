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
      @item(https://www.shadertoy.com/view/MslGD8 )
    )

  ------------------------------------------------------------------------------@br
  LICENCE : GPL/MPL
  @br
  ------------------------------------------------------------------------------
 *==============================================================================*)
unit BZSoftwareShader_Voronoi;

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
  TBZSoftShader_Voronoi = Class(TBZCustomSoftwareShader)
  protected
  public
    Constructor Create; override;
    Destructor Destroy; override;

    function Clone : TBZCustomSoftwareShader; override;

    function ShadePixelFloat:TBZColorVector; override;
  end;

implementation


{ TBZSoftShader_GroundAndDistortPhongSphere }

Constructor TBZSoftShader_Voronoi.Create;
begin
  inherited Create;
end;

Destructor TBZSoftShader_Voronoi.Destroy;
begin
  inherited Destroy;
end;

function TBZSoftShader_Voronoi.Clone : TBZCustomSoftwareShader;
begin
  Result := TBZSoftShader_Voronoi.Create;
  Result.Assign(Self);
end;

function TBZSoftShader_Voronoi.ShadePixelFloat : TBZColorVector;
const
  {$CODEALIGN CONSTMIN=16}
  vec2_1: TBZVector2f = (x: 269.5; y: 183.3);
  vec2_2: TBZVector2f = (x: 127.1; y: 311.7);
  vec2_3: TBZVector2f = (x: 8; y: 8);
  vec2_4: TBZVector2f = (x: 7; y: 113);
  vec4_5: TBZColorVector = (x: 0; y: 1.0; z: 2.0; w:1);
  {$CODEALIGN CONSTMIN=4}
var
  tTime : single;

  function hash(n: Single): Single;
  begin
    Result := (fract(sin(n) * 43758.5453));
  end;

  function hash2(p: TBZVector2f): TBZVector2f;
  begin
    p.Create(p.DotProduct(vec2_2), p.DotProduct(vec2_1));
    Result.x := (System.sin(p.x) * 18.5453);
    Result.y := (System.sin(p.y) * 18.5453);
    Result := Result.Fract;
  end;

  function Voronoi(const p: TBZVector2f): TBZVector2f;
  var
    {$CODEALIGN VARMIN=16}
    n, f, m, g, o, r, t : TBZVector2f;
    {$CODEALIGN VARMIN=4}
    j, i: integer;
    d, itx, ity: Single;

  begin
    n.Create(floor(p.x), floor(p.y));
    f.Create(fract(p.x), fract(p.y));
    m := vec2_3;

    for j   := -1 to 1 do
      for i := -1 to 1 do
      begin
        g.x := i;
        g.y := j;
        t := n + g;
        o := hash2(t);
        o.x := sin(iTime + 6.2831 * o.x);
        o.y := sin(iTime + 6.2831 * o.y);
        o := (o * 0.5) + 0.5;
        r := g - f + o;

        d := r.DotProduct(r);
        if d < m.x then
        begin
          m.x := d;
          m.y := hash(t.DotProduct(vec2_4)); //o.x + o.y;
        end;
      end;

    Result.x := system.sqrt(m.x);
    Result.y := m.y;
  end;

  function ComputePixel(Coord:TBZVector2f; aTime:Single) : TBZColorVector;
  var
    {$CODEALIGN VARMIN=16}
    uv, c, r  : TBZVector2f;
    Col, ct : TBZColorVector;
    {$CODEALIGN VARMIN=4}
    s : single;
  begin
    r.Create(FInvResolution.x, FInvResolution.x);
    uv := Coord * r;


    // computer voronoi patterm
    //c := Voronoi(uv * 8);
    c := voronoi( uv * (14.0+6.0*sin(0.2*iTime)) );

    // colorize
    s := c.y * 6.2831; // * 100;
    ct := vec4_5 + s;
    //
    ct.X := Cos(ct.X);
    ct.Y := Cos(ct.Y);
    ct.Z := Cos(ct.Z);
    //
    col := (ct * 0.5) + 0.5;

    //col := col * clamp(1.0 - 0.4*c.x*c.x,0.0,1.0);
    //col := col - (1.0-smoothstep( 0.08, 0.09, c.x));



    col := col * (0.8 - 0.4 * c.x);
    col := col + (0.4 * (2 - SmoothStep(0, 0.12, c.x) - SmoothStep(0, 0.04, c.x)));

    Result := col;
    Result.Alpha := 1.0;
  end;

begin
  //ttime := max(0.0,iTime-2.0);
  Result := ComputePixel(FragCoords, iTime);
end;

end.

