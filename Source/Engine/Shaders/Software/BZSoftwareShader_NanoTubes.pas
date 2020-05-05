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

  https://www.shadertoy.com/view/lslGRH

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
unit BZSoftwareShader_NanoTubes;

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

  TBZSoftShader_NanoTubes = Class(TBZCustomSoftwareShader)
  protected
    {$CODEALIGN RECORDMIN=16}
    FCameraTarget, FCameraUp,
    FCameraSide, FR : TBZVector4f;

    {$CODEALIGN RECORDMIN=4}
    FFocus : Single;
    
    procedure DoApply(var rci: Pointer; Sender: TObject); override;
    
  public
    Constructor Create; override;
    Destructor Destroy; override;
    
    
    function Clone : TBZCustomSoftwareShader; override;
    
    function ShadePixelFloat:TBZColorVector; override;
  end;

implementation

{ TBZSoftShader_GroundAndDistortPhongSphere }

Constructor TBZSoftShader_NanoTubes.Create;
begin
  inherited Create;
  FFocus := 1.8;
end;

Destructor TBZSoftShader_NanoTubes.Destroy;
begin
  inherited Destroy;
end;

function TBZSoftShader_NanoTubes.Clone : TBZCustomSoftwareShader;
begin
  Result := TBZSoftShader_NanoTubes.Create;
  Result.Assign(Self);
end; 

procedure TBZSoftShader_NanoTubes.DoApply(var rci: Pointer; Sender: TObject);
Var
  t : Double;
begin
  inherited DoApply(rci,sender);  
  t := FiTime * 0.3;
  FCameraPosition.CreatePoint(Cos(t), Sin(t), 3.5); //(cosLarge(t),sinLarge(t), 3.5);
  FCameraTarget.CreatePoint(0, 0, 0);
  CameraDirection := (FCameraTarget - CameraPosition);
  CameraDirection := CameraDirection.Normalize;
  FCameraUp.CreatePoint(0,1,0);
  FCameraUp:= FCameraUp.Normalize;
  FCameraSide := CameraDirection.CrossProduct(FCameraUp);
  FR.Create(0, 0, FiTime * 0.5);
end;

function TBZSoftShader_NanoTubes.ShadePixelFloat : TBZColorVector;
  const
    MAX_MARCH    = 49;
    MAX_DISTANCE = 49;
    RADIUS       = 0.25;
    {$CODEALIGN CONSTMIN=16}
    vec3_3: TBZColorVector = (x: 0.025; y: 0.025; z: 0.02; w:1);
    {$CODEALIGN CONSTMIN=4}
    
  function rand(n: TBZVector4f) : Single;
  var
   {$CODEALIGN VARMIN=16}
    v : TBZVector4i;
    {$CODEALIGN VARMIN=4}
  begin
    v := n.floor;
    //Result := (fract(sinLarge((n.x + n.y * 1E2 + n.z * 1E4) * 1E-4) * 1E5));
    Result := (fract(System.Sin((v.x + v.y * 1E2 + v.z * 1E4) * 1E-4) * 1E5));
  end;
  
  // .x is distance, .y = colour
  function map(p: TBZVector4f): TBZVector2f;
  var
    {$CODEALIGN VARMIN=16}
    f : TBZVector4f;
    {$CODEALIGN VARMIN=4}
    d : Single;
    cr: Single;
    cd: Single;
    rr: Single;
    rn: Single;
    rm: Single;
    rd: Single;
  begin
    // cylinder
    f  := p.Fract;
    f := f - 0.5;
    d  := f.xy.length;
    cr := rand(p);
    cd := d - cr * RADIUS;

    // end - calc (rand) radius at more stable pos
    p.z := p.z - 0.5;
    rr  := rand(p);
    rn  := d - rr * RADIUS;
    rm  := System.abs(fract(p.z) - 0.5); // offset so at end of cylinder

    rd := System.Sqrt(rn * rn + rm * rm); // end with ring

    if cd < rd then
    begin
      Result.x := cd;
      Result.y := cr;
    end
    else
    begin
      Result.x := rd;
      Result.y := rr;
    end;

  end;  
  
  function ComputePixel(Coord:TBZVector2f; aTime:Single) : TBZColorVector;
  var
    {$CODEALIGN VARMIN=16}    
    rayDir, ray, f : TBZVector4f;
    pos         : TBZVector2f;
    d           : TBZVector2f;
    FinaleColor : TBZColorVector;
    {$CODEALIGN VARMIN=4}          
    m           : Single;
    total_d     : Single;
    i           : integer;
    c           : Single;    
  begin
    //pos := (Coord / resolution) * 2.0 - 1.0; //((Coord + Coord) - FResolution) * FInvResolution.y;
    //pos.x := pos.x * 1.33333;
    pos :=  Coord * InvResolution;
    rayDir := FCameraSide * pos.x;
    rayDir := rayDir + (FCameraUp * pos.y);
    rayDir := rayDir + (FCameraDirection * FFocus);
    rayDir := RayDir.normalize;
    ray    := FCameraPosition;
    m      := 0.32;
    total_d      := 0;

    for i := 0 to MAX_MARCH do
    begin
      d       := map(Ray - FR);
      total_d := total_d + d.x;
      ray     := ray + (rayDir * d.x);
      m       := m + 1;
      if System.abs(d.x) < 0.01 then break;

      if total_d > MAX_DISTANCE then
      begin
        total_d := MAX_DISTANCE;
        break;
      end;
    end;

    c := 1 - (total_d * 0.001);
    FinaleColor.Create(c);
    //m := m * 0.8;
    Result := (FinaleColor - ((vec3_3 * m)*0.8));
    Result := Result * d.y;
    Result.Alpha := 1.0;
  end;
  
begin
  Result := ComputePixel(FragCoords, iTime);
end;

end.

