(*
  @abstract(Contient les classes pour l'application de filtres de déformations )

  -------------------------------------------------------------------------------------------------------------

  @created(2017-08-22)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(22/08/2017 : Creation)
    @item(18/06/2019 : Mise à jour)
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes :)@br

  -------------------------------------------------------------------------------------------------------------

  @bold(Dépendances) : BZClasses, BZMath, BZVectorMath, BZVectorMathEx, BZColors, BZGraphic, BZBitmapFilterClasses

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)
     @unorderedList(
       @item(FPC/Lazarus)
     )

  -------------------------------------------------------------------------------------------------------------

  @bold(Licence) : MPL / LGPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZBitmapDeformationFilters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  BZClasses, BZMath, BZVectorMath, BZVectorMathEx, BZColors, BZGraphic, BZBitmapFilterClasses;

type
  { TBZBitmapTwirlFilter }
  TBZBitmapTwirlFilter = Class(TBZCustomBitmapFilterTransformation)
  protected
    FCenterX  : Integer;
    FCenterY  : Integer;
    FRadius   : Integer;
    FRadSqr   : Integer;
    FAngle    : Integer;

    procedure PrepareTransformation; override;
    function ComputeTransformation(x,y : Integer) : TBZFloatPoint; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); override;

    property CenterX : Integer read FCenterX write FCenterX;
    property CenterY : Integer read FCenterY write FCenterY;
    property Radius  : Integer read FRadius write FRadius;
    property Angle   : Integer read FAngle write FAngle;
  end;

  { TBZBitmapPinchFilter }
  TBZBitmapPinchFilter = Class(TBZCustomBitmapFilterTransformation)
  protected
    FCenterX  : Integer;
    FCenterY  : Integer;
    FRadius   : Integer;
    FRadSqr   : Integer;
    FAngle    : Integer;
    FAmount   : Single;

    procedure PrepareTransformation; override;
    function ComputeTransformation(x,y : Integer) : TBZFloatPoint; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); override;

    property CenterX : Integer read FCenterX write FCenterX;
    property CenterY : Integer read FCenterY write FCenterY;
    property Radius  : Integer read FRadius write FRadius;
    property Angle   : Integer read FAngle write FAngle;
    property Amount  : Single read FAmount write FAmount;
  end;


  { TBZBitmapFishEyeFilter }
  TBZBitmapFishEyeFilter = Class(TBZCustomBitmapFilterTransformation)
  protected
    FAmount    : Single;
    FRadiusMax : Single;
    FCenterX  : Integer;
    FCenterY  : Integer;
    FRadius   : Integer;

    procedure PrepareTransformation; override;
    function ComputeTransformation(x,y : Integer) : TBZFloatPoint; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); override;

    property CenterX : Integer read FCenterX write FCenterX;
    property CenterY : Integer read FCenterY write FCenterY;
    property Radius  : Integer read FRadius write FRadius;
    property Amount  : Single read FAmount write FAmount;
  end;

  { TBZBitmapWaveFilter }
  TBZWaveDistorsionMode = (wdmSine, wdmSquare, wdmSawTooth, wdmTriangle, wdmRandom);
  TBZBitmapWaveDistorsionFilter = Class(TBZCustomBitmapFilterTransformation)
  private
  protected
    FAmplitudeX : Single;
    FAmplitudeY : Single;
    FWaveLengthX : Single;
    FWaveLengthY : Single;
    FMode : TBZWaveDistorsionMode;

    procedure PrepareTransformation; override;
    function ComputeTransformation(x,y : Integer) : TBZFloatPoint; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); override;

    property Mode : TBZWaveDistorsionMode read FMode write FMode;
    property AmplitudeX : Single read FAmplitudeX write FAmplitudeX;
    property AmplitudeY : Single read FAmplitudeY write FAmplitudeY;
    property WaveLengthX : Single read FWaveLengthX write FWaveLengthX;
    property WaveLengthY : Single read FWaveLengthY write FWaveLengthY;

  end;

  { TBZBitmapWaveRippleFilter }
  TBZBitmapWaterRippleFilter = Class(TBZCustomBitmapFilterTransformation)
  private
  protected
    FCenterX  : Integer;
    FCenterY  : Integer;
    FRadius   : Integer;
    FAmplitude : Single;
    FWaveLength : Single;
    FPhase : Single;
    FRadSqr : Single;

    procedure PrepareTransformation; override;
    function ComputeTransformation(x,y : Integer) : TBZFloatPoint; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); override;

    property CenterX : Integer read FCenterX write FCenterX;
    property CenterY : Integer read FCenterY write FCenterY;
    property Radius  : Integer read FRadius write FRadius;
    property Amplitude  : Single read FAmplitude write FAmplitude;
    property WaveLength : Single read FWaveLength write FWaveLength;
    property Phase      : Single read FPhase write FPhase;

  end;

  TBZBitmapDiffusionFilter = Class(TBZCustomBitmapFilterTransformation)
  protected
    FCosLUT, FSinLUT    : Array[0..255] of Single;
    FForceHorizontal : Single;
    FForceVertical : Single;
    FMinDistance : Single;
    FMaxDistance : Single;

    procedure PrepareTransformation; override;
    function ComputeTransformation(x,y : Integer) : TBZFloatPoint; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); override;

    property Horizontal  : Single read FForceHorizontal write FForceHorizontal;
    property Vertical  : Single read FForceVertical write FForceVertical;
    property MinDistance  : Single read FMinDistance write FMinDistance;
    property MaxDistance  : Single read FMaxDistance write FMaxDistance;
  end;

  TBZBitmapAffineTransformationFilter = Class(TBZCustomBitmapFilterTransformation)
  protected
    FMatrix : TBZAffineMatrix;

    procedure PrepareTransformation; override;
    function ComputeTransformation(x,y : Integer) : TBZFloatPoint; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); override;

    property Matrix  : TBZAffineMatrix read FMatrix write FMatrix;
  end;

  { TBZBitmapPolarTransformationFilter }
  TBZPolarTransformationMode = (ptmToPolar, ptmToCartesian);
  TBZBitmapPolarTransformationFilter = Class(TBZCustomBitmapFilterTransformation)
  private
  protected
    FMode : TBZPolarTransformationMode;
    FCenterX  : Integer;
    FCenterY  : Integer;

    //FRadius : Integer;
    FMin, FMax : Integer;
    FX, FY,
    FSqrY,
    FAngleFactor,
    FCurrentRadius : Single;
    FCenterMin : Single;

    procedure PrepareTransformation; override;
    procedure DoOnNextLine; override;
    function ComputeTransformation(x,y : Integer) : TBZFloatPoint; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); override;

    property CenterX : Integer read FCenterX write FCenterX;
    property CenterY : Integer read FCenterY write FCenterY;
    property Mode  : TBZPolarTransformationMode read FMode write FMode;
    //property Radius  : Integer read FRadius write FRadius;

  end;

implementation

uses BZUtils;

{ TBZBitmapPolarTransformationFilter }

Constructor TBZBitmapPolarTransformationFilter.Create(Const AOwner : TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner, False);
  //FRadius := 100;
  FCenterX := OwnerBitmap.CenterX;
  FCenterY :=  OwnerBitmap.CenterY;
  FMode := ptmToPolar;
  FEdgeAction := peaClamp;
  FScannerDescription := 'Transformation polaire';
end;

procedure TBZBitmapPolarTransformationFilter.PrepareTransformation;
begin
  inherited PrepareTransformation;
  if OwnerBitmap.Width >= OwnerBitmap.Height then
  begin
    FMin := OwnerBitmap.Height;
    FMax := OwnerBitmap.Width;
  end
  else
  begin
    FMin := OwnerBitmap.Width;
    FMax := OwnerBitmap.MaxHeight;
  end;

  if FMode = ptmToCartesian then
  begin
    FAngleFactor := 360 / FMax;
    FCurrentRadius := 0;
  end
  else
  begin
    FCurrentRadius := 0;
    FAngleFactor := FMax / 360;
  end;

  FCenterMin := Min(FCenterX, FCenterY); // * 0.5;
end;

procedure TBZBitmapPolarTransformationFilter.DoOnNextLine;
begin
  if FMode = ptmToPolar then
  begin
    FY := FCenterY - FCurrentY;
    FSqrY := FY * FY;
  end
  else
  begin
    FCurrentRadius := FCenterMin * (FCurrentY / FMin);
  end;
end;

function TBZBitmapPolarTransformationFilter.ComputeTransformation(x, y : Integer) : TBZFloatPoint;
Var
  CosAngle, SinAngle, Theta, r, a, Angle : Single;
  cx : Single;
begin
  if FMode = ptmToCartesian then
  begin
    FX := X - FCenterX ;
    Theta := DegToRadian(FX * FAngleFactor);
    CosAngle := System.Cos(Theta);
    SinAngle := System.Sin(Theta);
    Result.X := FCenterX + (-FCurrentRadius * SinAngle);
    Result.Y := FCenterY + (FCurrentRadius * CosAngle);
  end
  else
  begin
    cx := FCenterX - x;
    r := System.Sqrt(cx*cx + FSqrY);
    a :=  Math.ArcTan2(cx, -FY) - cPI;
    // On ajuste pour que l'angle soit dans l'interval [0,360] et non pas dans l'interval [-180,180]
    if a < 0 then a := a + c2PI;
    Angle := RadianToDeg(a);
    //if Angle<0 then Angle := Angle + 360;

    Result.X := (Angle * FAngleFactor);
    Result.Y := OwnerBitmap.Height * (r / FCenterMin);
  end;
end;



{%region=====[ TBZBitmapTwirlFilter ]============================================}

Constructor TBZBitmapTwirlFilter.Create(Const AOwner : TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner, DirectWrite);
  FRadius := 100;
  FCenterX := OwnerBitmap.CenterX;
  FCenterY :=  OwnerBitmap.CenterY;
  FAngle := 0;
  FEdgeAction := peaClamp;
  FScannerDescription := 'Tournoyer';
end;

procedure TBZBitmapTwirlFilter.PrepareTransformation;
begin
  inherited PrepareTransformation;
  FRadSqr := FRadius * FRadius;
end;

function TBZBitmapTwirlFilter.ComputeTransformation(x, y : Integer) : TBZFloatPoint;
Var
  a, Dist : Single;
  {$CODEALIGN VARMIN=16}
  p1, p2 : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin
  p1.Create(x,y);
  p2.Create(FCenterX, FCenterY);
  p1 := p1 - p2;
  //p2 := p1;
  Dist := p1.LengthSquare;
  if (Dist > FRadSqr) then
  begin
    Result.X := x;
    Result.Y := y;
  end
  else
  begin
    Dist := Sqrt(Dist);
    a := ArcTan2(p1.Y, p1.x) +  DegToRadian(FAngle) * (FRadius - Dist) / FRadius;
    Result.X := FCenterX + Dist * Cos(a);
    Result.Y := FCenterY + Dist * Sin(a);
  end;
end;

{%endregion%}

{%region=====[ TBZBitmapPinchFilter ]============================================}

Constructor TBZBitmapPinchFilter.Create(Const AOwner : TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner, DirectWrite);
  FRadius := 100;
  FCenterX := OwnerBitmap.CenterX;
  FCenterY :=  OwnerBitmap.CenterY;
  FAngle := 0;
  FAmount := 0.8;
  FEdgeAction := peaClamp;
  FFilterGetPixel := psmMean; //psmBicubic; // psmBilinear;
  FScannerDescription := 'Pincer et tournoyer';
end;

procedure TBZBitmapPinchFilter.PrepareTransformation;
begin
  inherited PrepareTransformation;
  FRadSqr := FRadius * FRadius;
end;

function TBZBitmapPinchFilter.ComputeTransformation(x, y : Integer) : TBZFloatPoint;
Var
  t, r , a, s, c, Dist : Single;
  {$CODEALIGN VARMIN=16}
  p1, p2 : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin
  p1.Create(x,y);
  p2.Create(FCenterX, FCenterY);
  p1 := p1 - p2;
  //p2 := p1;
  Dist := p1.LengthSquare;
  if (Dist > FRadSqr) then // or (Dist = 0) then
  begin
    Result.X := x;
    Result.Y := y;
  end
  else
  begin
    Dist := System.Sqrt(Dist / FRadSqr);
    t := Math.power( System.sin( cPiDiv2 * dist ), -amount);
    p2.Create(t,t);
    p1 := p1 * p2;
    r := 1.0 - dist;
    a := DegToRadian(FAngle) * r * r;
    s := System.sin(a);
    c := System.cos(a);
    Result.X := FCenterX + c * p1.x - s * p1.y;
    Result.Y := FCenterY + s * p1.x + c * p1.y;
  end;
end;

{%endregion%}

{%region=====[ TBZBitmapFishEyeFilter ]==========================================}

Constructor TBZBitmapFishEyeFilter.Create(Const AOwner : TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner, DirectWrite);
  FRadius:= 0;
  FAmount := 1;
  FEdgeAction := peaClamp;
  FScannerDescription := 'Distortion de lentille';
end;

procedure TBZBitmapFishEyeFilter.PrepareTransformation;
begin
  inherited PrepareTransformation;
  FRadiusMax := FRadius * FAmount; //OwnerBitmap.Width * FAmount;
end;

function TBZBitmapFishEyeFilter.ComputeTransformation(x, y : Integer) : TBZFloatPoint;
Var
  Dist, Dist2 : Single;
  {$CODEALIGN VARMIN=16}
  p1, p2, pDist1, pDist2 : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin
  p1.Create(x,y);
  p2.Create(FCenterX, FCenterY);
  p1 := p1 - p2;
  //p2 := p1;
  Dist := p1.Length;
  if (Dist = 0) then
  begin
    Result.X := FCenterX;
    Result.Y := FCenterY; //OwnerBitmap.CenterY;
  end
  else
  begin
    //= 1.0 + ((0.5 - dist) / 0.5) * scale
    Dist2 := FRadiusMax / 2 * (1 / (1 - Dist / FRadiusMax) - 1);
    //r' = r + (1 - sqrt(1 -r^2)) / 2
    pDist2.Create(Dist2, Dist2);
    pDist1.Create(Dist, Dist);
    Result := (((p1 * pDist2) / pDist1) + p2);
    //Result.X := p1.x * Dist2 / Dist + xmid;
    //Result.Y := p1.y * Dist2 / Dist + ymid;
  end;
end;

{%endregion%}

{%region=====[ TBZBitmapWaveFilter ]=============================================}

Constructor TBZBitmapWaveDistorsionFilter.Create(Const AOwner : TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner, DirectWrite);
  FEdgeAction := peaClamp;

  FAmplitudeX := 5.0;
  FWaveLengthX := 16;
  FAmplitudeY := 0.0;
  FWaveLengthY := 16;

  FMode := wdmSine;   //wdmRandom; //wdmSquare; //wdmTriangle; //wdmSawTooth;
  FScannerDescription := 'Ondulation';
end;

procedure TBZBitmapWaveDistorsionFilter.PrepareTransformation;
begin
  inherited PrepareTransformation;
  Randomize;
end;

function TBZBitmapWaveDistorsionFilter.ComputeTransformation(x, y : Integer) : TBZFloatPoint;
var
  nx, ny, fx, fy, rx, ry : Single;
begin
  nx := y / FWaveLengthX;
  ny := x / FWaveLengthY;

  Case FMode of
    wdmSine :
    begin
      fx := Sin(nx);
      fy := Sin(ny);
    end;
    wdmSquare :
    begin
      fx := Sign(Sin(nx));
      fy := Sign(Sin(ny));
    end;
    wdmSawTooth :
    begin
      fx := modulus(nx,1.0);
      fy := modulus(ny,1.0);
    end;
    wdmTriangle :
    begin
      rx := modulus(nx,1.0);
      ry := modulus(ny,1.0);
      if rx >= 0.5 then rx := 1.0 - rx;
      if ry >= 0.5 then ry := 1.0 - ry;
      fx := rx + rx;
      fy := ry + ry;
    end;
    wdmRandom :
    begin
      fx :=  (Random * (nx + nx)) - nx;    //fx.RandomRange(-1.0, 1.0);
      fy :=  (Random * (ny + ny)) - ny;
    end;
    //wdmNoise:
    //begin
    //  fx := NoiseGenerator.Noise1D(fx);
    //  fy := NoiseGenerator.Noise1D(fy);
    //end;
  end;

  Result.x := x + FAmplitudeX * fx;
  Result.y := y + FAmplitudeY * fy;

end;

{%endregion%}

{%region=====[ TBZBitmapWaveRippleFilter ]=======================================}

Constructor TBZBitmapWaterRippleFilter.Create(Const AOwner : TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner, DirectWrite);
  FRadius := 100;
  FCenterX := OwnerBitmap.CenterX;
  FCenterY :=  OwnerBitmap.CenterY;
  FAmplitude := 5.0;
  FWaveLength := 16;
  FPhase := 1.0;
  FScannerDescription := 'Ondulation d''eau';
end;

procedure TBZBitmapWaterRippleFilter.PrepareTransformation;
begin
  inherited PrepareTransformation;
  FRadSqr := FRadius * FRadius;
end;

function TBZBitmapWaterRippleFilter.ComputeTransformation(x, y : Integer) : TBZFloatPoint;
Var
  a, Dist : Single;
  {$CODEALIGN VARMIN=16}
  p1, p2 : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin
  p1.Create(x,y);
  p2.Create(FCenterX, FCenterY);
  p2 := p1 - p2;
  //p2 := p1;
  Dist := p2.LengthSquare;
  if (Dist > FRadSqr) then
  begin
    Result.X := x;
    Result.Y := y;
  end
  else
  begin
    Dist := Sqrt(Dist);
		a := FAmplitude * Sin(Dist / FWaveLength * c2Pi - FPhase);
		a := a * (FRadius - Dist) / FRadius;
		if ( Dist <> 0 ) then	a := a * FWavelength / Dist;

    Result := p1 + (p2 * a);
  end;
end;

{%endregion%}

{%region=====[ TBZBitmapDiffusionFilter ]========================================}

Constructor TBZBitmapDiffusionFilter.Create(Const AOwner : TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner, DirectWrite);
  FForceVertical := 1.0;
  FForceHorizontal := 1.0;
  FScannerDescription := 'Eparpiller';
end;

procedure TBZBitmapDiffusionFilter.PrepareTransformation;
Var
  i : byte;
  a : Single;
begin
  inherited PrepareTransformation;
  For  i:=0 to 255 do
  begin
    a := c2Pi * i / 256;
    FCosLUT[i] := FForceVertical * Cos(a);
    FSinLUT[i] := FForceHorizontal * Sin(a);
  end;
  Randomize;
end;

function TBZBitmapDiffusionFilter.ComputeTransformation(x, y : Integer) : TBZFloatPoint;
Var
  d, dx, dy : Single;
  i : Byte;
begin
  i := Random(255);
  d := Random;
  dx := max(FMinDistance, min(FMaxDistance, d * FSinLUT[i]));
  dy := max(FMinDistance, min(FMaxDistance, d * FCosLUT[i]));
  Result.x := x + dx;
  Result.y := y + dy;
end;

{%endregion%}

{%region=====[ TBZBitmapAffineTransformationFilter ]=============================}

Constructor TBZBitmapAffineTransformationFilter.Create(Const AOwner : TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner, DirectWrite);
  FEdgeAction := peaZero;
  FMatrix.CreateIdentityMatrix;
end;

procedure TBZBitmapAffineTransformationFilter.PrepareTransformation;
begin
  inherited PrepareTransformation;
  FMatrix := FMatrix.Invert;
end;

function TBZBitmapAffineTransformationFilter.ComputeTransformation(x, y : Integer) : TBZFloatPoint;
Var
  {$CODEALIGN VARMIN=16}
  pt : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin
  pt.Create(x,y);
  //Result := FMatrix.TransformPoint(pt);
  Result := pt * FMatrix;
end;

{%endregion%}

end.

