(*
  @abstract(Contient les classes pour l'application de filtres sur les couleurs.)

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

  @bold(Dépendances) : BZClasses, BZControlClasses, BZGraphic, BZBitmap, BZInterpolationFilters

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)
     @unorderedList(
       @item(FPC/Lazarus)
     )

  -------------------------------------------------------------------------------------------------------------

  @bold(Licence) : MPL / LGPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZBitmapColorFilters;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================


interface

uses
  Classes, SysUtils, Math,
  BZClasses, BZMath, BZColors, BZGraphic, BZBitmapFilterClasses;

Type

  { TBZBitmapColorFilterSwapChannel }
  TBZBitmapColorFilterSwapChannel = class(TBZCustomBitmapFilterPointTransform)
  private
    FSwapMode : TBZColorFilterSwapChannelMode;
  protected
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False);  Override;

    property SwapMode : TBZColorFilterSwapChannelMode read FSwapMode write FSwapMode default scmRedBlue;
  End;

  { TBZBitmapColorFilterNegate }
  TBZBitmapColorFilterNegate = class(TBZCustomBitmapFilterPointTransformEx)
  protected
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False);  Override;
  End;

  { TBZBitmapColorFilterHyperSat }
  TBZBitmapColorFilterHyperSat = class(TBZCustomBitmapFilterPointTransformEx)
  protected
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False);  Override;
  End;

  { TBZBitmapColorFilterMix }
  TBZBitmapColorFilterMix = class(TBZCustomBitmapFilterPointTransformEx)
  private
    FHoverColor : TBZColor;
    FFactor  : Single;
  protected
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False);  Override;

    property HoverColor : TBZColor read FHoverColor write FHoverColor;
    property Factor : Single read FFactor write FFactor default 0.5;
  End;

  { TBZBitmapColorFilterMixInv }
  TBZBitmapColorFilterMixInv = class(TBZCustomBitmapFilterPointTransformEx)
  private
    FHoverColor : TBZColor;
    FFactor  : Single;
  protected
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;

    property HoverColor : TBZColor read FHoverColor write FHoverColor;
    property Factor : Single read FFactor write FFactor default 0.5;
  End;

  { TBZBitmapColorFilterAverage }
  TBZBitmapColorFilterAverage = class(TBZCustomBitmapFilterPointTransform)
  private
    FHoverColor : TBZColor;
  protected
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;

    property HoverColor : TBZColor read FHoverColor write FHoverColor;
  End;

  { TBZBitmapColorFilterModulate }
  TBZBitmapColorFilterModulate = class(TBZCustomBitmapFilterPointTransformEx)
  private
    FHoverColor : TBZColor;
  protected
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;

    property HoverColor : TBZColor read FHoverColor write FHoverColor;
  End;

  { TBZBitmapColorFilterColorize }
  TBZBitmapColorFilterColorize = class(TBZCustomBitmapFilterPointTransformEx)
  private
    FHoverColor : TBZColor;
  protected
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;

    property HoverColor : TBZColor read FHoverColor write FHoverColor;
  End;

  { TBZBitmapColorFilterGrayScale }
  TBZBitmapColorFilterGrayScale = class(TBZCustomBitmapFilterPointTransformEx)
  private
    FMode: TBZGrayConvertMode; // = gcmLuminosity;
    FMatrix: TBZGrayMatrixType; //=gmtJPEG;
    FOptionalValue : Single; // = 0
    FGammaFactor : Single;
  protected
    procedure DoInitializeScanner; Override;
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;

  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); override;

    property Mode : TBZGrayConvertMode read FMode write FMode default gcmLuminosity;
    property Matrix : TBZGrayMatrixType read FMatrix write FMatrix default gmtJPEG;
    property OptionalValue : Single read FOptionalValue write FOptionalValue default 0;
    property GammaFactor : Single read FGammaFactor write FGammaFactor;
  End;

  { TBZBitmapColorFilterAdjustBrightness }
  TBZBitmapColorFilterAdjustBrightness = class(TBZCustomBitmapFilterApplyLUT)
  private
    FFactor  : Single;
  protected
    procedure DoInitializeScanner; Override;
    procedure InitializeLUT; Override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;

    property Factor : Single read FFactor write FFactor default 0;
  End;

  { TBZBitmapColorFilterAdjustContrast }
  TBZBitmapColorFilterAdjustContrast = class(TBZCustomBitmapFilterApplyLUT)
  private
    FFactor  : Single;
  protected
    procedure InitializeLUT; Override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;

    property Factor : Single read FFactor write FFactor default 0;
  End;

  { TBZBitmapColorFilterAutoAdjustContrast }
  TBZBitmapColorFilterAutoAdjustContrast = class(TBZCustomBitmapFilterPointTransform)
  private
    //FFactor  : Single;
    FIntensityMin : Byte;
    FIntensityMax : Byte;
    FCoef : Byte;
  protected
    procedure DoInitializeScanner; Override;
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;
  End;

  { TBZBitmapColorFilterAdjustSaturation }
  TBZBitmapColorFilterAdjustSaturation = class(TBZCustomBitmapFilterPointTransformEx)
  private
    FFactor  : Single;
    FLimitLuminosity : Boolean; // Renforcer : "EnforceLuminosity" serait-ce mieux ??
  protected
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;

    property Factor : Single read FFactor write FFactor default 0;
    property LimitLuminosity : Boolean Read FLimitLuminosity Write FLimitLuminosity  default False;
  End;

  { TBZBitmapColorFilterGammaCorrection }
  TBZBitmapColorFilterGammaCorrection = class(TBZCustomBitmapFilterApplyLUT)
  private
    FFactor  : Single;
  protected
    procedure InitializeLUT; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;

    property Factor : Single read FFactor write FFactor default 2.2;
  End;

  { TBZBitmapColorFilterPosterize }
  TBZBitmapColorFilterPosterize = class(TBZCustomBitmapFilterPointTransformEx)
  private
    FFactor  : Single;
    FAmount : Integer;
  protected
    procedure DoInitializeScanner; Override;
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;

    property Factor : Single read FFactor write FFactor default 0;
  End;

  { TBZBitmapColorFilterSolarize }
  TBZBitmapColorFilterSolarize = class(TBZCustomBitmapFilterPointTransformEx)
  private
    FFactor  : Single;
    FAmount : Integer;
  protected
    procedure DoInitializeScanner; Override;
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;

    property Factor : Single read FFactor write FFactor default 0;
  End;

  { TBZBitmapColorFilterSplitLight }
  TBZBitmapColorFilterSplitLight = class(TBZCustomBitmapFilterPointTransform)
  private
    FAmount : Integer;
    FFactor : Single;
  protected
    procedure DoInitializeScanner; Override;
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;

    property Amount : Integer read FAmount write FAmount default 1;
  End;

  { TBZBitmapColorFilterFilterColor }
  TBZBitmapColorFilterFilterColor = class(TBZCustomBitmapFilterPointTransform)
  private
    FFactor  : Single;
    FInRangeMode : Boolean;
    FMinRange, FMaxRange : Byte;
    FColorMask : TBZColorMaskComponent;
    FKeepMode : Boolean;
  protected
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;

    property ColorMask : TBZColorMaskComponent read FColorMask write FColorMask default cmcRed;
    property Factor : Single read FFactor write FFactor default 0;
    property InRangeMode : Boolean Read FInRangeMode Write FInRangeMode default False;
    property KeepMode : Boolean Read FKeepMode Write FKeepMode default True;
    property MinRange : Byte Read FMinRange Write FMinRange Default 0;
    property MaxRange : Byte Read FMaxRange Write FMaxRange Default 255;
  End;

  { TBZBitmapColorFilterExcludeColor }
  TBZBitmapColorFilterExcludeColor = class(TBZCustomBitmapFilterPointTransform)
  private
    FInRangeMode : Boolean;
    FMinRange, FMaxRange : TBZColor;
    //FThresOld : Byte;
  protected

    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;

    property InRangeMode : Boolean Read FInRangeMode Write FInRangeMode default False;
    property MinRange : TBZColor Read FMinRange Write FMinRange;
    property MaxRange : TBZColor Read FMaxRange Write FMaxRange;
  End;

  { TBZBitmapColorFilterExtractColor }
  TBZBitmapColorFilterExtractColor = class(TBZCustomBitmapFilterPointTransform)
  private
    FInRangeMode : Boolean;
    FMinRange, FMaxRange : TBZColor;
    //FThresOld : Byte;
  protected
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;

    property InRangeMode : Boolean Read FInRangeMode Write FInRangeMode default False;
    property MinRange : TBZColor Read FMinRange Write FMinRange;
    property MaxRange : TBZColor Read FMaxRange Write FMaxRange;
  End;

  { TBZBitmapColorFilterModifyAlpha }
  TBZBitmapColorFilterModifyAlpha = class(TBZCustomBitmapFilterPointTransform)
  private
    FAlphaMode : TBZBitmapAlphaSetMode;
    FColor : TBZColor;
  protected
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;

    property Mode : TBZBitmapAlphaSetMode read FAlphaMode Write FAlphaMode default asmDefault;
    property TransparentColor : TBZColor Read FColor Write FColor;
  End;

  { TBZBitmapColorFilterMinimum }
  TBZBitmapColorFilterMinimum = class(TBZCustomBitmapFilterPointTransformEx)
  private
  protected
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;
    procedure Render; override;
  End;

  { TBZBitmapColorFilterMaximum }
  TBZBitmapColorFilterMaximum = class(TBZCustomBitmapFilterPointTransformEx)
  private
  protected
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;
    procedure Render; override;
  End;

  { TBZBitmapColorFilterGrayOut : filtre qui 'grise' une image en faisant la moyenne de chaque pixel avec du blanc }
  TBZBitmapColorFilterGrayOut = class(TBZCustomBitmapFilterPointTransformEx)
  protected
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;
  End;

  { TBZBitmapColorFilterExposure : Filtre à améliorer en prenant en compte le "seuil du point noir" }
  TBZBitmapColorFilterExposure = class(TBZCustomBitmapFilterApplyLUT)
  private
    FFactor  : Single;
  protected
    procedure InitializeLUT; Override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;

    property Factor : Single read FFactor write FFactor default 0;
  End;

  { TBZBitmapColorFilterBoost : Filtre multipliant les couleurs par un facteur }
  TBZBitmapColorFilterBoost = class(TBZCustomBitmapFilterApplyLUT)
  private
    FFactor  : Single;
  protected
    procedure InitializeLUT; Override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;

    property Factor : Single read FFactor write FFactor default 0;
  End;

  { TBZBitmapColorFilterGain }
  TBZBitmapColorFilterGain = class(TBZCustomBitmapFilterApplyLUT)
  private
    FFactorGain, FFactorBias  : Single;
  protected
    procedure InitializeLUT; Override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;

    property FactorGain : Single read FFactorGain write FFactorGain default 0;
    property FactorBias : Single read FFactorBias write FFactorBias default 0;
  End;

  { TBZBitmapColorFilterAdjustRGB }
  TBZBitmapColorFilterAdjustRGB = class(TBZCustomBitmapFilterPointTransformEx)
  private
    FFactorRed, FFactorBlue, FFactorGreen  : Single;
    procedure SetFactorRed(const AValue : Single);
    procedure SetFactorGreen(const AValue : Single);
    procedure SetFactorBlue(const AValue : Single);
    function GetFactorRed : Single;
    function GetFactorGreen : Single;
    function GetFactorBlue : Single;
  protected
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;

    property FactorRed : Single read GetFactorRed write SetFactorRed default 0;
    property FactorGreen : Single read GetFactorGreen write SetFactorGreen default 0;
    property FactorBlue : Single read GetFactorBlue write SetFactorBlue default 0;
  End;

  { TBZBitmapColorFilterBalance }
  TBZBitmapColorFilterBalance = class(TBZCustomBitmapFilterApplyLUT)
  private
    FCyanToRedFactor : Single;
    FMagentaToGreenFactor : Single;
    FYellowToBlueFactor : Single;
    FScaleFactor : Single;
  protected
    procedure InitializeLUT; Override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;

    property CyanToRedFactor : Single read FCyanToRedFactor write FCyanToRedFactor;
    property MagentaToGreenFactor : Single read FMagentaToGreenFactor write FMagentaToGreenFactor;
    property YellowToBlueFactor : Single read FYellowToBlueFactor write FYellowToBlueFactor;
    property ScaleFactor : Single read FScaleFactor write FScaleFactor;
  end;

  { TBZBitmapColorFilterAdjustHSL }
  TBZBitmapColorFilterAdjustHSV = class(TBZCustomBitmapFilterPointTransformEx)
  private
    FHueFactor : Single;
    FSaturationFactor : Single;
    FValueFactor  : Single;
  protected
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;

    property HueFactor : Single read FHueFactor write FHueFactor;
    property SaturationFactor : Single read FSaturationFactor write FSaturationFactor;
    property ValueFactor  : Single read FValueFactor write FValueFactor;
  End;



  { TBZBitmapColorFilterRemoveRedEye }
  TBZBitmapColorFilterRemoveRedEye = class(TBZCustomBitmapFilterPointTransform)
    private
    protected
      Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
    public
      Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;
    End;


  { TBZBitmapColorFilterSepia }
  TBZBitmapColorFilterSepia = class(TBZCustomBitmapFilterPointTransform)
  private
    FScale : Single;
    FMatrix : TBZColorMatrix;
  protected
    procedure DoInitializeScanner; override;
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;

    property Scale : Single read FScale write FScale;
  End;

  { TBZBitmapColorHueRotate }
  TBZBitmapColorFilterHueRotate = class(TBZCustomBitmapFilterPointTransform)
  private
    FShift : Integer;
    FDelta : Single;
    FMatrix : TBZColorMatrix;
  protected
    procedure DoInitializeScanner; Override;
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;

    property Shift : Integer read FShift write FShift;
  End;

  { TBZBitmapColorFilterMatrix }
  TBZBitmapColorFilterMatrix = class(TBZCustomBitmapFilterPointTransform)
  private
    FColorMatrix : TBZColorMatrix;
  protected
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;

    property ColorMatrix : TBZColorMatrix read FColorMatrix write FColorMatrix;
  End;


  { Filtres "ala" Instagram prédéfinis }
  TBZInstagramFilterType = (ift1977, iftAden, iftAmaro, iftAntique, iftBlackAndWhite, iftBrannan, iftBrooklyn, iftClarendon, iftDream,
                            iftEasyBird, iftEverGlow, iftFreshAqua, iftForest, iftGingham, iftHudson, iftInkWell, iftJuno, iftKelvin, iftLark, iftLight, iftLime, iftLofi, iftLudwig,
                            iftMajesty, iftMaven, iftMayfair, iftMoon, iftNashville, iftOldPhoto, iftPeachy, iftPerpetua, iftPolaroid, iftPolaroidII,
                            iftRetro, iftReyes, iftRise, iftSlumber, iftStinson, iftToaster, iftValencia, iftVintage,  iftWalden, iftWillow, iftXPro2);


implementation

{ TBZBitmapColorFilterMatrix }

Constructor TBZBitmapColorFilterMatrix.Create(Const AOwner : TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner, DirectWrite);
  FScannerDescription := 'Conversion matrice de couleur';
end;

Function TBZBitmapColorFilterMatrix.ProcessPixel(Const inColor : TBZColor) : TBZColor;
Var
  {$CODEALIGN VARMIN=16}
  ColorF : TBZColorVector;
  {$CODEALIGN VARMIN=4}
begin
  ColorF.Create(InColor);
  ColorF := ColorF.ApplyColorMatrix(FColorMatrix);
  Result.Create(ColorF);
end;



{ TBZBitmapColorHueRotate }
Constructor TBZBitmapColorFilterHueRotate.Create(Const AOwner : TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner, DirectWrite);
  FShift := 0;
  FScannerDescription := 'Rotation de la teinte';
end;

procedure TBZBitmapColorFilterHueRotate.DoInitializeScanner;
//Const _Coef : Single = 1.0 / 3.0;
//Var
//  ColorMatrix : TBZColorMatrixConversion;
//  CosA, SinA, d, s : Single;
begin
  inherited DoInitializeScanner;
  FDelta := FShift / 360;
  //cosA := cos(DegToRadian(FShift));
  //sinA := sin(DegToRadian(FShift));
  //d := 1.0 - CosA;
  //s := Sqrt(_Coef);
  //
  ////Red
  //FMatrix[0] := cosA + d / 3.0;
  //FMatrix[5] := _Coef * d - s * sinA;
  //FMatrix[10] := _Coef * d + s * sinA;
  //FMatrix[3] := 1.0;
  //FMatrix[4] := 0.0;
  //
  ////Green
  //FMatrix[1] := _Coef * d + s * sinA;
  //FMatrix[6] := cosA + _Coef * d;
  //FMatrix[11] := _Coef * d - s * sinA;
  //FMatrix[8] := 1.0;
  //FMatrix[9] := 0.0;
  //
  ////Blue
  //FMatrix[2] := _Coef * d - s * sinA;
  //FMatrix[7] := _Coef * d + s * sinA;
  //FMatrix[12] := cosA + _Coef * d;
  //FMatrix[13] := 1.0;
  //FMatrix[14] := 0.0;
  //
  //// Alpha
  //FMatrix[15] := 0.0;
  //FMatrix[16] := 0.0;
  //FMatrix[17] := 0.0;
  //FMatrix[18] := 1.0;
  //FMatrix[19] := 0.0;
end;

Function TBZBitmapColorFilterHueRotate.ProcessPixel(Const inColor : TBZColor) : TBZColor;
Var
  {$CODEALIGN VARMIN=16}
  ColorF : TBZColorVector;
  {$CODEALIGN VARMIN=4}
  HSVColor : TBZColorFloatHSV;
begin

  ColorF.Create(inColor);
  HSVColor := ColorF.ToColorFloatHSV(hsvNormalize);
  HSVColor.Hue := HSVColor.Hue + FDelta;
  Result := HSVColor.ToColorRBGA;
  //ColorF.Create(InColor);
  //ColorF := ColorF.ApplyColorMatrix(FMatrix);
  //Result.Create(ColorF);
end;

{ TBZBitmapColorFilterAdjustHSL }

Constructor TBZBitmapColorFilterAdjustHSV.Create(Const AOwner : TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner, DirectWrite);
  FHueFactor := 0.0;
  FSaturationFactor := 0.0;
  FValueFactor := 0.0;
  FScannerDescription := 'Ajustement HSV';
end;

Function TBZBitmapColorFilterAdjustHSV.ProcessPixel(Const inColor : TBZColor) : TBZColor;
Var
  HSVColor : TBZColorFloatHSV;
  OutColor : TBZColor;
begin
  HSVColor := InColor.ToColorFloatHSV(hsvNormalize);
  HSVColor.Hue := (HSVColor.Hue + FHueFactor);
  if HSVColor.Hue < 0.0 then HSVColor.Hue := 1.0 + HSVColor.Hue;
  if HSVColor.Hue > 1.0 then HSVColor.Hue := HSVColor.Hue - 1.0;
  HSVColor.Saturation := Clamp((HSVColor.Saturation + FSaturationFactor),0.0, 1.0);
  HSVColor.Value := Clamp((HSVColor.Value + FValueFactor),0.0, 1.0);
  Result := HSVColor.ToColorRBGA;
end;

{ TBZBitmapColorFilterBalance }

Constructor TBZBitmapColorFilterBalance.Create(Const AOwner : TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner, DirectWrite);
  FCyanToRedFactor := 0.0;
  FMagentaToGreenFactor := 0.0;
  FYellowToBlueFactor := 0.0;
  FScaleFactor := 0.3;
  FScannerDescription := 'Balance de couleur';
end;

procedure TBZBitmapColorFilterBalance.InitializeLUT;
Var
  i : Byte;
begin
  FApplyOnlyGlobalLUT := False;
  for i := 0 to 255 do
  begin
    FRedLUT[i]   := Clamp(Round(i * (CyanToRedFactor * FScaleFactor + 1.0)), 0, 255);
    FGreenLUT[i] := Clamp(Round(i * (MagentaToGreenFactor * FScaleFactor + 1.0)), 0, 255);
    FBlueLUT[i]  := Clamp(Round(i * (YellowToBlueFactor * FScaleFactor + 1.0)), 0, 255);
  end;
end;

{ TBZBitmapColorFilterSepia }

constructor TBZBitmapColorFilterSepia.Create(const AOwner : TBZBaseBitmap; const DirectWrite : Boolean);
begin
  inherited Create(AOwner, DirectWrite);
  FScannerDescription := 'Filtre sepia';
end;

procedure TBZBitmapColorFilterSepia.DoInitializeScanner;
begin
  inherited DoInitializeScanner;
  FMatrix.CreateSepia(FScale);
end;

function TBZBitmapColorFilterSepia.ProcessPixel(const inColor : TBZColor) : TBZColor;
Var
  {$CODEALIGN VARMIN=16}
  ColorF : TBZColorVector;
  {$CODEALIGN VARMIN=4}
begin
  ColorF.Create(InColor);
  ColorF := ColorF.ApplyColorMatrix(FMatrix);
  Result.Create(ColorF);
end;

{ TBZBitmapColorFilterAutoAdjustContrast }

procedure TBZBitmapColorFilterAutoAdjustContrast.DoInitializeScanner;
begin
  inherited DoInitializeScanner;
  FIntensityMin := OwnerBitmap.getIntensityMin;
  FIntensityMax := OwnerBitmap.getIntensityMax;
  FCoef := (FIntensityMax - FIntensityMin);
end;

Function TBZBitmapColorFilterAutoAdjustContrast.ProcessPixel(Const inColor : TBZColor) : TBZColor;
Var
  //Intensity : Byte;
  OutColor : TBZColor;
begin
  //Intensity := InColor.Luminance;
  //b = float(a - min) / (max - min) * 255
  OutColor.Red := Clamp(Round(((InColor.Red - FIntensityMin) / FCoef) * 255), 0, 255);
  OutColor.Green := Clamp(Round(((InColor.Green - FIntensityMin) / FCoef) * 255), 0, 255);
  OutColor.Blue := Clamp(Round(((InColor.Blue - FIntensityMin) / FCoef) * 255), 0, 255);
  OutColor.Alpha := InColor.Alpha;
  Result := OutColor;
end;

Constructor TBZBitmapColorFilterAutoAdjustContrast.Create(Const AOwner : TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner, DirectWrite);
  FScannerDescription := 'Ajustement automatique du contraste';
end;


{%region ====[ TBZBitmapColorFilterSwapChannel ]===============================}

Constructor TBZBitmapColorFilterSwapChannel.Create(Const AOwner : TBZBaseBitmap; Const DirectWrite : Boolean);
Begin
  Inherited Create(AOwner,True);
  FScannerDescription := 'Swap Channel';
End;

Function TBZBitmapColorFilterSwapChannel.ProcessPixel(Const inColor : TBZColor) : TBZColor;

  Function DoColorSwapRB: TBZColor; inline;
  Begin
    Result.alpha := InColor.Alpha;
    Result.red := InColor.Blue;
    Result.green := InColor.green;
    Result.blue := InColor.Red;
  End;

  Function DoColorSwapRG: TBZColor; inline;
  Begin
    Result.alpha := InColor.Alpha;
    Result.red := InColor.Green;
    Result.green := InColor.Red;
    Result.blue := InColor.Blue;
  End;

  Function DoColorSwapGB: TBZColor; inline;
  Begin
    Result.alpha := InColor.Alpha;
    Result.red := InColor.Red;
    Result.green := InColor.Blue;
    Result.blue := InColor.Green;
  End;
Begin
  Case FSwapMode of
    scmRedBlue   : Result := DoColorSwapRB;
    scmRedGreen  : Result := DoColorSwapRG;
    scmGreenBlue : Result := DoColorSwapGB;
  End;
end;

{%endregion}

{%region ====[ TBZBitmapColorFilterNegate ]====================================}

Constructor TBZBitmapColorFilterNegate.Create(Const AOwner : TBZBaseBitmap; Const DirectWrite : Boolean);
Begin
  Inherited Create(AOwner,True);
  FScannerDescription := 'Inverser';
End;

Function TBZBitmapColorFilterNegate.ProcessPixel(Const inColor : TBZColor) : TBZColor;
Begin
 Result := inColor.Negate;
end;

{%endregion}

{%region ====[ TBZBitmapColorFilterHyperSat ]==================================}

Constructor TBZBitmapColorFilterHyperSat.Create(Const AOwner : TBZBaseBitmap; Const DirectWrite : Boolean);
Begin
  Inherited Create(AOwner,True);
  FScannerDescription := 'Hyper Saturation';
End;

Function TBZBitmapColorFilterHyperSat.ProcessPixel(Const inColor : TBZColor) : TBZColor;
Begin
  Result := inColor.HyperSat;
end;

{%endregion}

{%region ====[ TBZBitmapColorFilterMix ]=======================================}

Constructor TBZBitmapColorFilterMix.Create(Const AOwner : TBZBaseBitmap; Const DirectWrite : Boolean);
Begin
  Inherited Create(AOwner,True);
  FScannerDescription := 'Mixage de couleur';
End;

Function TBZBitmapColorFilterMix.ProcessPixel(Const inColor : TBZColor) : TBZColor;
Begin
  Result := inColor.Mix(FHoverColor, FFActor);
end;

{%endregion}

{%region ====[ TBZBitmapColorFilterMixInv ]====================================}

Constructor TBZBitmapColorFilterMixInv.Create(Const AOwner: TBZBaseBitmap; Const DirectWrite : Boolean);
Begin
  Inherited Create(AOwner,True);
  FScannerDescription := 'Mixage de couleur inverser';
End;

Function TBZBitmapColorFilterMixInv.ProcessPixel(Const inColor : TBZColor) : TBZColor;
Begin
  Result := inColor.MixInv(FHoverColor, FFActor);
end;

{%endregion}

{%region ====[ TBZBitmapColorFilterAverage ]===================================}

Constructor TBZBitmapColorFilterAverage.Create(Const AOwner: TBZBaseBitmap; Const DirectWrite : Boolean);
Begin
  Inherited Create(AOwner,True);
  FScannerDescription := 'Moyenne de couleur';
End;

Function TBZBitmapColorFilterAverage.ProcessPixel(Const inColor : TBZColor) : TBZColor;
Begin
  Result := inColor.Average(FHoverColor);
end;

{%endregion}

{%region ====[ TBZBitmapColorFilterModulate ]==================================}

Constructor TBZBitmapColorFilterModulate.Create(Const AOwner: TBZBaseBitmap; Const DirectWrite : Boolean);
Begin
  Inherited Create(AOwner,True);
  FScannerDescription := 'Modulation de couleur';
End;

Function TBZBitmapColorFilterModulate.ProcessPixel(Const inColor : TBZColor) : TBZColor;
Begin
  Result := inColor.Modulate(FHoverColor);
end;

{%endregion}

{%region ====[ TBZBitmapColorFilterColorize ]==================================}

Constructor TBZBitmapColorFilterColorize.Create(Const AOwner: TBZBaseBitmap; Const DirectWrite : Boolean);
Begin
  Inherited Create(AOwner,True);
  FScannerDescription := 'Colorier';
End;

Function TBZBitmapColorFilterColorize.ProcessPixel(Const inColor : TBZColor) : TBZColor;
Begin
  Result := inColor;
end;

{%endregion}

{%region ====[ TBZBitmapColorFilterGrayScale ]=================================}

Constructor TBZBitmapColorFilterGrayScale.Create(Const AOwner: TBZBaseBitmap; Const DirectWrite : Boolean);
Begin
  Inherited Create(AOwner,True);
End;

procedure TBZBitmapColorFilterGrayScale.DoInitializeScanner;
begin
  inherited DoInitializeScanner;
  FScannerDescription := 'Conversion en Niveaux de gris';
end;

Function TBZBitmapColorFilterGrayScale.ProcessPixel(Const inColor : TBZColor) : TBZColor;
Begin
  Result := inColor.Desaturate(FMode,FMatrix,FOptionalValue, FGammaFactor);
end;

{%endregion}

{%region ====[ TBZBitmapColorFilterAdjustBrightness ]==========================}

Constructor TBZBitmapColorFilterAdjustBrightness.Create(Const AOwner: TBZBaseBitmap; Const DirectWrite : Boolean);
Begin
  Inherited Create(AOwner,True);
  FScannerDescription := 'Ajustement de la luminosité';
End;

procedure TBZBitmapColorFilterAdjustBrightness.DoInitializeScanner;
begin
  inherited DoInitializeScanner;
  FPreserveLuminosity := False;
end;

procedure TBZBitmapColorFilterAdjustBrightness.InitializeLUT;
Var
  i : Byte;
  k:integer;
  f : Single;
Begin
  FApplyOnlyGlobalLUT := True;
  f := 1.0 + FFactor;
  // Precalculs
  For i := 0 To 255 Do
  Begin
    k := Round(f * i);
    If k > 255 Then
      k := 255;
    FGlobalLUT[i] := k;
  End;
 // FLUTPtr := @FGLobalLUT[0];
End;

{%endregion}

{%region ====[ TBZBitmapColorFilterAdjustContrast ]============================}

Constructor TBZBitmapColorFilterAdjustContrast.Create(Const AOwner: TBZBaseBitmap; Const DirectWrite : Boolean);
Begin
  Inherited Create(AOwner,True);
  FScannerDescription := 'Ajustement du contraste';
End;

procedure TBZBitmapColorFilterAdjustContrast.InitializeLUT;
Var
  i : Byte;
  Coef : Single;
  k : Integer;
Begin
  FApplyOnlyGlobalLUT := True;

  // Precalculs
  //Coef := (1.015 * (FFactor + 1.0)) / (1.0 * (1.015 - FFactor));
  if FFactor = 0 then Coef := 1
  else Coef := Math.power((1.0 + FFactor) / 1.0,2); //Trunc(Factor* 255);
  For i := 0 To 255 Do
  Begin
    k := trunc(((((i / 255.0) - 0.5) * Coef) + 0.5) * 255.0);
    FGlobalLUT[i] := ClampByte(K);
  End;
End;


{%endregion}

{%region ====[ TBZBitmapColorFilterAdjustSaturation ]==========================}

Constructor TBZBitmapColorFilterAdjustSaturation.Create(Const AOwner: TBZBaseBitmap; Const DirectWrite : Boolean);
Begin
  Inherited Create(AOwner,True);
  FScannerDescription := 'Ajustement de la saturation';
End;

Function TBZBitmapColorFilterAdjustSaturation.ProcessPixel(Const inColor : TBZColor) : TBZColor;
Var
  Gray : Byte;
Begin
  Result.Alpha := InColor.Alpha;
  Gray := inColor.Luminosity;
  If FLimitLuminosity Then
  Begin
    Result.Red := ClampByte(Gray + Round(((inColor.Red - Gray) * FFactor)));
    Result.Green := ClampByte(Gray + Round(((inColor.Green - Gray) * FFactor)));
    Result.Blue := ClampByte(Gray + Round(((inColor.Blue - Gray) * FFactor)));
  End
  Else
  Begin
    Result.Red := ClampByte(inColor.Red + Round(((inColor.Red - Gray) * FFactor)));
    Result.Green := ClampByte(inColor.Green + Round(((inColor.Green - Gray) * FFactor)));
    Result.Blue := ClampByte(inColor.Blue + Round(((inColor.Blue - Gray) * FFactor)));
  End;
end;

{%endregion}

{%region ====[ TBZBitmapColorFilterGammaCorrection ]===========================}

Constructor TBZBitmapColorFilterGammaCorrection.Create(Const AOwner: TBZBaseBitmap; Const DirectWrite : Boolean);
Begin
  Inherited Create(AOwner,True);
  FScannerDescription := 'Correction gamma';
End;

procedure TBZBitmapColorFilterGammaCorrection.InitializeLUT;
Var
  i : Byte;
  invGamma:  Single;
Begin
  FApplyOnlyGlobalLUT := True;

  // Precalculs
  If FFactor < 0.1 Then
    invGamma := 10
  Else
    invGamma := 1 / Factor;
  For i := 0 To 255 Do
    FGlobalLUT[i] := Round(255 * Math.Power(i * (1 / 255), InvGamma));
End;

{%endregion}

{%region ====[ TBZBitmapColorFilterPosterize ]=================================}

Constructor TBZBitmapColorFilterPosterize.Create(Const AOwner: TBZBaseBitmap; Const DirectWrite : Boolean);
Begin
  Inherited Create(AOwner,True);
  FScannerDescription := 'Posterization';
End;

procedure TBZBitmapColorFilterPosterize.DoInitializeScanner;
Begin
  Inherited DoInitializeScanner;
  FAmount := Round(FFactor * 255);
End;

Function TBZBitmapColorFilterPosterize.ProcessPixel(Const inColor : TBZColor) : TBZColor;
Var
  Gray: Integer;
Begin
  Result := inColor;
  Gray := inColor.Luminosity;
  If Gray > FAmount Then
  Begin
    Result.Red := round(inColor.Red / FAmount) * FAmount;
    Result.Green := round(inColor.Green / FAmount) * FAmount;
    Result.Blue := round(inColor.Blue / FAmount) * FAmount;
  End;
end;

{%endregion}

{%region ====[ TBZBitmapColorFilterSolarize ]==================================}

Constructor TBZBitmapColorFilterSolarize.Create(Const AOwner: TBZBaseBitmap; Const DirectWrite : Boolean);
Begin
  Inherited Create(AOwner,True);
  FScannerDescription := 'Solarization';
End;

procedure TBZBitmapColorFilterSolarize.DoInitializeScanner;
Begin
  Inherited DoInitializeScanner;
  FAmount := Round(FFactor * 255);
End;

Function TBZBitmapColorFilterSolarize.ProcessPixel(Const inColor : TBZColor) : TBZColor;
Var
  Gray: Integer;
Begin
  Result := inColor;
  Gray := inColor.Luminosity;
  If Gray < FAmount Then
  Begin
    Result.Red := 255 - inColor.Red;
    Result.Green := 255 - inColor.Green;
    Result.Blue := 255 - inColor.Blue;
  End;
end;

{%endregion}

{%region ====[ TBZBitmapColorFilterSplitLight ]================================}

Constructor TBZBitmapColorFilterSplitLight.Create(Const AOwner: TBZBaseBitmap; Const DirectWrite : Boolean);
Begin
  Inherited Create(AOwner,True);
  FScannerDescription := 'Découpage lumière';
End;

procedure TBZBitmapColorFilterSplitLight.DoInitializeScanner;
Begin
  Inherited DoInitializeScanner;
  FFactor := (_FloatColorRatio*cPi)*0.5;
  Passes := FAmount;
End;

Function TBZBitmapColorFilterSplitLight.ProcessPixel(Const inColor : TBZColor) : TBZColor;
Begin
  Result.Red :=ClampByte(Round(System.sin(inColor.Red*FFactor)*255));
  Result.Green :=ClampByte(Round(System.sin(inColor.Green*FFactor)*255));
  Result.Blue :=ClampByte(Round(System.sin(inColor.Blue*FFactor)*255));
  Result.Alpha := inColor.Alpha;
end;

{%endregion}

{%region ====[ TBZBitmapColorFilterKeepColor ]=================================}

Constructor TBZBitmapColorFilterFilterColor.Create(Const AOwner: TBZBaseBitmap; Const DirectWrite : Boolean);
Begin
  Inherited Create(AOwner,True);
  FScannerDescription := 'Filtre les couleurs';
End;

Function TBZBitmapColorFilterFilterColor.ProcessPixel(Const inColor : TBZColor) : TBZColor;
Begin
  if FInRangeMode then
  begin
    Case FColorMask of
      cmcRed   :
      begin
        if (inColor.Red<FMinRange) or (inColor.Red>FMaxRange) then Result.Red := 0;
        if FKeepMode then
        begin
          Result.Green := 0;
          Result.Blue := 0;
        end;
      End;
      cmcGreen :
      begin
        if (inColor.Green<FMinRange) or (inColor.Green>FMaxRange) then Result.Green := 0;
        if FKeepMode then
        begin
          Result.Red := 0;
          Result.Blue := 0;
        end;
      End;
      cmcBlue  :
      begin
        if (inColor.Blue<FMinRange) or (inColor.Blue>FMaxRange) then Result.Blue := 0;
        if FKeepMode then
        begin
          Result.Green := 0;
          Result.Red := 0;
        end;
      End;
      cmcAlpha :
      begin
        if (inColor.Alpha<FMinRange) or (inColor.Alpha>FMaxRange) then Result.Alpha := 0;
        if FKeepMode then
        begin
          Result.Red := 0;
          Result.Green := 0;
          Result.Blue := 0;
        end;
      End;
    End;
  End
  else
  begin
    Case FColorMask of
      cmcRed   :
      begin
        Result.Red :=ClampByte(Round(inColor.Red * FFactor));
        if FKeepMode then
        begin
          Result.Green := 0;
          Result.Blue := 0;
        end;
      End;
      cmcGreen :
      begin
        Result.Green :=ClampByte(Round(inColor.Green * FFactor));
        if FKeepMode then
        begin
          Result.Red := 0;
          Result.Blue := 0;
        end;
      End;
      cmcBlue  :
      begin
        Result.Blue :=ClampByte(Round(inColor.Blue * FFactor));
        if FKeepMode then
        begin
          Result.Red := 0;
          Result.Green := 0;
        end;
      End;
      cmcAlpha :
      begin
        Result.Alpha :=ClampByte(Round(inColor.Alpha * FFactor));
        if FKeepMode then
        begin
          Result.Red := 0;
          Result.Green := 0;
          Result.Blue := 0;
        end;
      End;
    End;
  End;
end;

{%endregion}

{%region ====[ TBZBitmapColorFilterExcludeColor ]==============================}

Constructor TBZBitmapColorFilterExcludeColor.Create(Const AOwner: TBZBaseBitmap; Const DirectWrite : Boolean);
Begin
  Inherited Create(AOwner,True);
  FScannerDescription := 'Exlusion de couleurs';
End;


Function TBZBitmapColorFilterExcludeColor.ProcessPixel(Const inColor : TBZColor) : TBZColor;
Begin
  if FInRangeMode then
  begin
    if inColor.IsInRange(FMinRange, FMaxRange) then Result := clrTransparent else Result := inColor;
  End
  else
  begin
    if (inColor = FMinRange) then Result := clrTransparent else Result := inColor;
  End;
end;

{%endregion}

{%region ====[ TBZBitmapColorFilterExtractColor ]==============================}

Constructor TBZBitmapColorFilterExtractColor.Create(Const AOwner: TBZBaseBitmap; Const DirectWrite : Boolean);
Begin
  Inherited Create(AOwner,True);
  FScannerDescription := 'Extraction de couleurs';
End;


Function TBZBitmapColorFilterExtractColor.ProcessPixel(Const inColor : TBZColor) : TBZColor;
Begin
  if FInRangeMode then
  begin
    if inColor.IsInRange(FMinRange, FMaxRange) then Result := inColor else Result := clrTransparent;
  End
  else
  begin
    if (inColor = FMinRange) then Result := inColor else Result := clrTransparent;
  End;
end;

{%endregion}

{%region ====[ TBZBitmapColorFilterModifyAlpha ]===============================}

Constructor TBZBitmapColorFilterModifyAlpha.Create(Const AOwner: TBZBaseBitmap; Const DirectWrite : Boolean);
Begin
  Inherited Create(AOwner,True);
  FScannerDescription := 'Modifie le canal alpha';
End;

Function TBZBitmapColorFilterModifyAlpha.ProcessPixel(Const inColor : TBZColor) : TBZColor;
Begin
  Case FAlphaMode Of
    asmAlphaFromIntensity    : Result.SetAlphaFromIntensity(inColor);
    asmDefault, asmSuperBlackTransparent : Result .SetAlphaFromSuperBlackTransparent(inColor);
    asmLuminance             : Result .SetAlphaFromLuminance(inColor);
    asmLuminanceSqrt         : Result .SetAlphaFromLuminanceSqrt(inColor);
    asmOpaque                : Result .SetAlphaOpaque(inColor);
    asmInverseLuminance      : Result .SetAlphaFromInverseLuminance(inColor);
    asmInverseLuminanceSqrt  : Result .SetAlphaFromInverseLuminanceSqrt(inColor);
    asmAlphaFromColor        : if (FColor = inColor) then Result  := clrTransparent;
  End;
end;

{%endregion}

{%region ====[ TBZBitmapColorFilterGrayOut ]===================================}

Constructor TBZBitmapColorFilterGrayOut.Create(Const AOwner: TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner, True);
  FScannerDescription :='Gray out';
end;

Function TBZBitmapColorFilterGrayOut.ProcessPixel(Const inColor : TBZColor) : TBZColor;
begin
	Result.Red   := (inColor.Red + 255) shr 1;
	Result.Green := (inColor.Green + 255) shr 1;
	Result.Blue  := (inColor.Blue + 255) shr 1;
end;

{%endregion}

{%region ====[ TBZBitmapColorFilterExposure ]==================================}

Constructor TBZBitmapColorFilterExposure.Create(Const AOwner: TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner, true);
  FScannerDescription := 'Exposure';
end;

procedure TBZBitmapColorFilterExposure.InitializeLUT;
Var
  i : Byte;
Begin
  FApplyOnlyGlobalLUT  := True;
  For i := 0 To 255 Do
  Begin
    FGlobalLUT[i] := ClampByte(Round(255 * (1 - System.exp(-(i/255) * FFactor))));
  End;
end;

{%endregion}

{%region ====[ TBZBitmapColorFilterExposure ]==================================}

Constructor TBZBitmapColorFilterBoost.Create(Const AOwner: TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner, true);
  FScannerDescription := 'Boost';
end;

procedure TBZBitmapColorFilterBoost.InitializeLUT;
Var
  i : Byte;
Begin
  FApplyOnlyGlobalLUT  := True;
  For i := 0 To 255 Do
  Begin
    FGlobalLUT[i] := ClampByte(Round(i * FFactor));
  End;
end;

{%endregion}

{%region ====[ TBZBitmapColorFilterGain ]======================================}

Constructor TBZBitmapColorFilterGain.Create(Const AOwner: TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner, True);
  FScannerDescription := 'Gain';
end;

procedure TBZBitmapColorFilterGain.InitializeLUT;
Var
  i : Byte;

  Function ComputeCoef(f : Single) : Single;
  begin
    Result :=	vGain(f, FFactorGain);
		Result := bias(Result, FFactorBias);
  end;

Begin
  FApplyOnlyGlobalLUT  := True;
  For i := 0 To 255 Do
  Begin
    FGlobalLUT[i] := ClampByte(Round(255 * ComputeCoef(i/255)));
  End;
end;

{%endregion}

{%region ====[ TBZBitmapColorFilterAdjustRGB ]=================================}

Constructor TBZBitmapColorFilterAdjustRGB.Create(Const AOwner: TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner, true);
  FScannerDescription := 'Ajustement RGB';
end;

procedure TBZBitmapColorFilterAdjustRGB.SetFactorRed(const AValue : Single);
begin
  if FFactorRed = (1.0 + AValue) then Exit;
  FFactorRed := 1.0 +  AValue;
end;

procedure TBZBitmapColorFilterAdjustRGB.SetFactorGreen(const AValue : Single);
begin
  if FFactorGreen = (1.0 + AValue) then Exit;
  FFactorGreen := 1.0 + AValue;
end;

procedure TBZBitmapColorFilterAdjustRGB.SetFactorBlue(const AValue : Single);
begin
  if FFactorBlue = (1.0 + AValue) then Exit;
  FFactorBlue := 1.0 + AValue;
end;

function TBZBitmapColorFilterAdjustRGB.GetFactorRed : Single;
begin
  Result := FFactorRed - 1.0;
end;

function TBZBitmapColorFilterAdjustRGB.GetFactorGreen : Single;
begin
  Result := FFactorGreen - 1.0;
end;

function TBZBitmapColorFilterAdjustRGB.GetFactorBlue : Single;
begin
  Result := FFactorBlue - 1.0;
end;

Function TBZBitmapColorFilterAdjustRGB.ProcessPixel(Const inColor : TBZColor) : TBZColor;
Var
  FColor, Factors : TBZColorVector;
Begin
  Factors.Create(FFactorRed, FFactorGreen, FFactorBlue, 1.0);
  FColor :=  inColor.AsColorVector;
  FColor := FColor * Factors;
  Result.Create(FColor);
end;

{%endregion}

{%region ====[ TBZBitmapColorFilterRemoveRedEye ]==============================}

Constructor TBZBitmapColorFilterRemoveRedEye.Create(Const AOwner: TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner, True);
  FScannerDescription :='Suppression des yeux rouge';
end;

Function TBZBitmapColorFilterRemoveRedEye.ProcessPixel(Const inColor : TBZColor) : TBZColor;
var
  nrv, bluf, redq : Single;
  powr, powb, powg: Single;
Begin
  Result := inColor;
  nrv    := inColor.Green + inColor.Blue;
  if nrv < 1 then
  begin
    nrv := 1;
  end;

  if inColor.Green > 1 then
  begin
    bluf := inColor.Blue / inColor.Green;
  end
  else
  begin
    bluf := inColor.Blue;
  end;

  //redIntensity = ((float)pixel.R / ((pixel.G + pixel.B) / 2));
  //if (redIntensity > 1.5f) bm.SetPixel(i, j, Color.FromArgb((pixel.G + pixel.B) / 2, pixel.G, pixel.B))

  bluf := Max ( 0.5, Min ( 1.5, Sqrt(bluf) ) );
  redq := (inColor.Red / nrv) * bluf;

  if redq > 0.7 then //0.75
  begin
    powr := 1.775 - (redq * 0.75 + 0.25);

    if powr < 0 then
    begin
      powr := 0;
    end;

    powr := powr * powr;
    powb := 1 - (1 - powr) / 2;
    powg := 1 - (1 - powr) / 4;


    Result.Red := Round (powr * inColor.Red);
    Result.Green := Round (powg * inColor.Green);
    Result.Blue := Round (powb * inColor.Blue);
    Result.Alpha := inColor.Alpha;
  end;
end;

{%endregion}

{%region ====[ TBZBitmapColorFilterMinimum ]===================================}

Constructor TBZBitmapColorFilterMinimum.Create(Const AOwner: TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner);
  FScannerDescription := 'Filtre minimum';
end;

Function TBZBitmapColorFilterMinimum.ProcessPixel(Const inColor : TBZColor) : TBZColor;
begin
  // ne fait rien ici
  Result := clrTransparent;
end;

procedure TBZBitmapColorFilterMinimum.Render;
Var
  x,y,i,j,dx,dy : Integer;
  inColor, outColor  : TBZColor;
  Delta : Single;
begin
  DoInitializeScanner;
  InitProgress(FInternalClipRect.Width,FInternalClipRect.Height);
  StartProgressSection(0,'');
  StartProgressSection(100,FScannerDescription);
  Delta := 100 / (FInternalClipRect.Height);
  For y := FInternalClipRect.Top to FInternalClipRect.Bottom do
  begin
    For x := FInternalClipRect.Left to FInternalClipRect.Right do
    Begin
       outColor := clrWhite;
       For j := -1 to 1 do
       begin
         dy := y + j;
         For i := -1 to 1 do
         Begin
            dx := x + i;
            if OwnerBitmap.CheckPixelBound(dx,dy) then
            begin
              inColor := OwnerBitmap.GetPixel(dx, dy);
              outColor := outColor.Min(inColor);
            end else outColor := OwnerBitmap.GetPixel(x, y);
         end;
       end;
       FDestBmp.setPixel(x,y,outColor);
    End;
    AdvanceProgress(Delta,0,1,True);
  End;
  FinishProgressSection(False);
  FinishProgressSection(True);
  DoFinalizeScanner;
end;

{%endregion}

{%region ====[ TBZBitmapColorFilterMaximum ]===================================}

Constructor TBZBitmapColorFilterMaximum.Create(Const AOwner: TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner);
  FScannerDescription := 'Filtre maximum';
end;

Function TBZBitmapColorFilterMaximum.ProcessPixel(Const inColor : TBZColor) : TBZColor;
begin
  // ne fait rien ici
  Result := clrTransparent;
end;

procedure TBZBitmapColorFilterMaximum.Render;
Var
  x,y,i,j,dx,dy : Integer;
  TestColor, inColor, outColor  : TBZColor;
  Delta : Single;
begin
  DoInitializeScanner;
  InitProgress(FInternalClipRect.Width,FInternalClipRect.Height);
  StartProgressSection(0,'');
  StartProgressSection(100,FScannerDescription);
  Delta := 100 / (FInternalClipRect.Height);
  For y := FInternalClipRect.Top to FInternalClipRect.Bottom do
  begin
    For x := FInternalClipRect.Left to FInternalClipRect.Right do
    Begin
       outColor := clrBlack;
       For j := -1 to 1 do
       begin
         dy := y + j;
         For i := -1 to 1 do
         Begin
            dx := x + i;
            if OwnerBitmap.CheckPixelBound(dx,dy) then
            begin
              inColor := OwnerBitmap.GetPixel(dx, dy);
              outColor := outColor.Max(inColor);
            end else outColor := OwnerBitmap.GetPixel(x, y);
         end;
       end;
       FDestBmp.setPixel(x,y,outColor);
    End;
    AdvanceProgress(Delta,0,1,True);
  End;
  FinishProgressSection(False);
  FinishProgressSection(True);
  DoFinalizeScanner;
end;

{%endregion}



end.

