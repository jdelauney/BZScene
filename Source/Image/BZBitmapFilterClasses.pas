(*
  @abstract(Contient les classes de base devant être hérité pour l'application de filtres graphique)

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

  @bold(Dépendances) : BZClasses, BZColors, BZGraphic, BZMath, BZVectorMath

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)
     @unorderedList(
       @item(FPC/Lazarus)
     )

  -------------------------------------------------------------------------------------------------------------

  @bold(Licence) : MPL / LGPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZBitmapFilterClasses;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================


interface

uses
  Classes, SysUtils, Math,
  BZClasses, BZColors, BZGraphic, BZMath, BZVectorMath;

const
  { Limite max des tons foncés }
  cShadowToneLimit =  85;
  { Limite max des tons moyens }
  cMidToneLimit    = 170;

Type
  { LUT pour stocker des valeurs précalculées }
  TBZColorChannelLUT = array[0..255] of Byte;
  { Pointeurr vers une table LUT : TBZColorChannelLUT }
  PBZColorChannelLUT = ^TBZColorChannelLUT;


  { Fonctions pour le calcul dune valeur suréchantillionnée }
  TBZGetPixelSampleFunction = Function(var Pixels: TBZDynColorArray): TBZColor;
  //TBZFilterGetPixelMode = (gpmDefault, gpmClamp, gpmWrap);

  { Classe de base d'aide servant à appliquer divers traitement sur un bitmap. @br
    Les masques sont pris en charge en cas d'utilisation de ceux-ci }
  TBZCustomBitmapScanner = class(TBZCustomOwnerBitmap)
  private

  protected
    FPasses : Integer;
    FDestBmp : TBZCustomBitmap;

    FInColor, FOutColor : TBZColor;
    FCurrentSrcPixelPtr: PBZColor;
    FCurrentDstPixelPtr: PBZColor;
    FMaskPixelPtr : PBZColor;

    FInternalClipRect : TBZRect;
    FMaxWidth, FMaxHeight : Integer;

    FMaxBufferSize : Longint;
    FNextLineIncr : Integer;
    FCurrentX, FCurrentY : Integer;
    //FDestX, FDestY : Integer;
    FDestPoint : TBZPoint;
    FDestFloatPoint : TBZFloatPoint;

    FDirectWrite : Boolean;
    FScannerDescription : String;

    procedure ComputeClipping;

   //procedure DoComputePixel;  virtual; abstract;
    Function DoComputePixel(Const inColor : TBZColor) : TBZColor;  virtual;

    Function GetPixelColor : TBZColor; virtual;
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor;  virtual; abstract;

    procedure DoFinalizeScanner; Virtual;
    procedure DoInitializeScanner;Virtual;
    procedure DoOnNextLine;Virtual;

    property Passes : Integer read FPasses write FPasses default 1;
  public                                            //Const  DirectApply :Boolean = True
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); virtual; overload;
    Destructor Destroy; Override;

    procedure Render;virtual;

    //function  GetFilteredBitmap : TBZBitmap;

   //procedure Render(CustomProc : TBZBitmapScannerRenderPixelProc); ??????
  End;

  { Classe à hériter pour appliquer un filtre à une couleur }
  TBZCustomBitmapFilterPointTransform = Class(TBZCustomBitmapScanner)
  protected
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor;  override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); override;
  end;

  {  Classe à hériter pour appliquer un filtre à une couleur avec prise en charge des tons clair, moyen et sombre, ainsi que la préservation de la luminosité }
  TBZCustomBitmapFilterPointTransformEx = Class(TBZCustomBitmapFilterPointTransform)
  private
  protected
    FApplyOnShadowColor : Boolean;
    FApplyOnMidColor : Boolean;
    FApplyOnHighlightColor : Boolean;
    FPreserveLuminosity : Boolean;
   //FApplyOnRedHue : Boolean
   //FApplyOnBlueHue : Boolean
   //FApplyOnGreenHue : Boolean
   //FApplyOnCyanHue : Boolean
   //FApplyOnMagentaHue : Boolean
   //FApplyOnYellowHue : Boolean
   //FApplyOnMono : Boolean;

    function ApplyFilter(Const inColor : TBZColor) : Boolean;
    function ApplyPreserveLuminosity(inColor, outColor : TBZColor) : TBZColor;
    Function DoComputePixel(Const inColor : TBZColor) : TBZColor;  override;
    Function ProcessPixel(Const inColor : TBZColor) : TBZColor;  override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); override;
    // Appliquer sur changements dans les tons sombre
    property ApplyOnShadowColor : Boolean read FApplyOnShadowColor write FApplyOnShadowColor;
    // Appliquer sur changements dans les tons moyen
    property ApplyOnMidColor : Boolean read FApplyOnMidColor write FApplyOnMidColor;
    // Appliquer sur changements dans les tons clair
    property ApplyOnHighlightColor : Boolean read FApplyOnHighlightColor write FApplyOnHighlightColor;
    // Applique les changement en conservant la luminosité des couleurs
    property PreserveLuminosity : Boolean read FPreserveLuminosity write FPreserveLuminosity;
  end;

  { Classe à hériter pour l'application d'un filtre à partir de tables précalculées LUT }
  TBZCustomBitmapFilterApplyLUT  = Class(TBZCustomBitmapFilterPointTransformEx)
  private

  protected
   FRedLUT : TBZColorChannelLUT;
   FGreenLUT : TBZColorChannelLUT;
   FBlueLUT : TBZColorChannelLUT;
   FGlobalLUT : TBZColorChannelLUT;
   FApplyOnlyGlobalLUT : Boolean;

   procedure DoInitializeScanner;override;
   Function ProcessPixel(Const inColor : TBZColor) : TBZColor;  override;

   procedure InitializeLUT; virtual;
  public
   Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); override;
   Destructor Destroy; Override;
  end;

  //TBZColorVectorSelectFunction = Function(var Pixels: TBZDynColorVectorArray): TBZColorVector;

  { Classe à hériter pour l'application de filtre non-linéaire }
  TBZCustomBitmapFilterNonLinear  = Class(TBZCustomBitmapFilterPointTransformEx)
  private
  protected
    FFilterSize: Byte;
    FFilterRadius : Integer;
    FFilterGetPixel : TBZGetPixelSampleMethod;
    //FCustomGetPixelSampleFunc : TBZGetPixelSampleFunction;
    FEdgeAction : TBZPixelEdgeAction;

    procedure SetEdgeAction(const AValue : TBZPixelEdgeAction);
    procedure SetFilterSize(const AValue : Byte);
    procedure SetFilterGetPixel(const AValue : TBZGetPixelSampleMethod);

    Function ProcessPixel(Const inColor : TBZColor) : TBZColor;  override;
    Function GetPixelColor : TBZColor; override;

  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); override;

    property GetPixelSampleMethod : TBZGetPixelSampleMethod read FFilterGetPixel write SetFilterGetPixel;
    // property CustomGetPixelSampleFunc : TBZGetPixelSampleFunction read FCustomGetPixelSampleFunc write SetCustomGetPixelSampleFunc;
    property FilterSize : Byte read FFilterSize write SetFilterSize;
    property PixelEdgeAction : TBZPixelEdgeAction read FEdgeAction  write SetEdgeAction ;
  end;

  { Classe à hériter pour l'application d'une transformation d'un point }
  TBZCustomBitmapFilterTransformation  = Class(TBZCustomBitmapFilterNonLinear)
  protected
    procedure DoInitializeScanner; override;
    procedure PrepareTransformation; virtual;
    function ComputeTransformation(x,y : Integer) : TBZFloatPoint; virtual;
    Function GetPixelColor : TBZColor;  override;
  end;

  { TODO 1 -oBZBitmap -cFiltre : Classe à hériter pour l'application d'une transformation d'un point avec une table de "mapping" }

  //TBZCustomBitmapFilterMap  = Class(TBZCustomBitmapFilterTransform)
  //protected
  //  function MapX : Single; virtual;
  //  function MapY : Single; virtual;
  //
  //  function TransformInverse(x,y : Integer) : TBZFloatPoint; override;
  //end;


implementation

uses BZLogger;

{%region ====[ TBZCustomBitmapScanner ]=========================================}

Constructor TBZCustomBitmapScanner.Create(Const AOwner : TBZBaseBitmap; Const DirectWrite : Boolean); //Const  DirectWrite :Boolean = False);
Begin
  Inherited Create(AOwner);
  FCurrentX := 0;
  FCurrentY := 0;
  FDirectWrite := DirectWrite;
  FPasses := 1;
  FScannerDescription :='';
End;

Destructor  TBZCustomBitmapScanner.Destroy;
begin
  // if Assign(FDestBmp) then FreeAndNil(FDestBmp);
  Inherited Destroy;
End;

procedure TBZCustomBitmapScanner.ComputeClipping;
Begin
  If Clipping Then
    Begin
      FInternalClipRect := ClipRect;
    End
    Else
    Begin
      With FInternalClipRect do
      begin
        Left := 0;
        Top := 0;
        Right := OwnerBitmap.MaxWidth;
        Bottom := OwnerBitmap.MaxHeight;
      End;
    End;
    FMaxWidth := FInternalClipRect.Width-1;
    FMaxHeight := FInternalClipRect.Height-1;
    FMaxBufferSize := (FInternalClipRect.Width * FInternalClipRect.Height) - 1;
    FNextLineIncr := OwnerBitmap.Width - FInternalClipRect.Width
End;

Function TBZCustomBitmapScanner.DoComputePixel(Const inColor : TBZColor) : TBZColor;
begin
  Result := ProcessPixel(InColor);
end;

Function TBZCustomBitmapScanner.GetPixelColor : TBZColor;
begin
  Result := FCurrentSrcPixelPtr^;
end;

procedure TBZCustomBitmapScanner.DoFinalizeScanner;
Begin
  if not(FDirectWrite) then   // And DirectApply
  begin
    OwnerBitmap.Assign(FDestBmp);
    FreeAndNil(FDestBmp);
  End;
  OwnerBitmap.EndUpdate;
End;

procedure TBZCustomBitmapScanner.DoInitializeScanner;
Begin
  OwnerBitmap.BeginUpdate;
  ComputeClipping;
  FCurrentX := FInternalClipRect.Left;
  FCurrentY := FInternalClipRect.Top;
  FCurrentSrcPixelPtr := OwnerBitmap.GetPixelPtr(FCurrentX, FCurrentY);
  if not(FDirectWrite) then
  begin
    FDestBmp := TBZCustomBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
    FDestBmp.Clear(clrTransparent);
    FCurrentDstPixelPtr := FDestBmp.GetPixelPtr(FCurrentX, FCurrentY);
  End;
End;

procedure TBZCustomBitmapScanner.DoOnNextLine;
Begin
  // Do Nothing here
End;

procedure TBZCustomBitmapScanner.Render;
Var
  Cnt : Integer;
  Delta, DeltaPass : Single;
Begin
  ComputeClipping;
  Cnt := 0;

  DoInitializeScanner;
  InitProgress(OwnerBitmap.Width,OwnerBitmap.Height);
  DeltaPass := 100 / FPasses;
  StartProgressSection(0,'');
  StartProgressSection(100,FScannerDescription);
  AdvanceProgress(0,0,1,True);
  While FPasses > 0 do
  begin
    StartProgressSection(100,FScannerDescription);
    Delta := 100 / OwnerBitmap.Height;
    While (cnt<=FMaxBufferSize) do
    begin
      FDestFloatPoint.Create(FCurrentX, FCurrentY);
      FInColor := GetPixelColor; //(FCurrentX, FCurrentY);

      if not(TBZBaseBitmap(OwnerBitmap).UseSelectionMask) then //and (TBZBaseBitmap(OwnerBitmap).ApplyMask)) then
      begin
        FOutColor := DoComputePixel(FInColor);
      end
      else
      begin
          FMaskPixelPtr := TBZBaseBitmap(OwnerBitmap).SelectionMask.GetPixelPtr(FCurrentX, FCurrentY);
          if FMaskPixelPtr^.Red > 0 then
          begin
            if FMaskPixelPtr^.Red = 255 then FOutColor := DoComputePixel(FInColor)
            else
            begin
              FInColor.Alpha := FMaskPixelPtr^.Red;
              FOutColor := DoComputePixel(FInColor);
              FOutColor := FOutColor.Blend(FInColor, FInColor.Alpha);
            end;
          end;
          Inc(FMaskPixelPtr);
      end;

      if FDirectWrite then FCurrentSrcPixelPtr^ := FOutColor
      else FCurrentDstPixelPtr^ := FOutColor;

      Inc(FCurrentX);
      Inc(FCurrentSrcPixelPtr);
      if not(FDirectWrite) then Inc(FCurrentDstPixelPtr);

      if (FCurrentX>FMaxWidth) then
      begin
        Inc(FCurrentY);
        FCurrentX := FInternalClipRect.Left;
        If FNextLineIncr>0 then
        begin
          Inc(FCurrentSrcPixelPtr,FNextLineIncr);
          if not(FDirectWrite) then Inc(FCurrentDstPixelPtr,FNextLineIncr);
          if (TBZBaseBitmap(OwnerBitmap).UseSelectionMask) then Inc(FMaskPixelPtr, FNextLineIncr); //and (TBZBaseBitmap(OwnerBitmap).ApplyMask)
        End;
        DoOnNextLine;
        AdvanceProgress(Delta,0,1,True);
      End;
      inc(Cnt);

    End;
    dec(FPasses);
    FinishProgressSection(True);
    AdvanceProgress(DeltaPass,0,1,False);
  End;
  DoFinalizeScanner;
  FinishProgressSection(False);
  FinishProgressSection(True);

End;

{%endregion}

{%region ====[ TBZCustomBitmapFilterPointTransform ]============================}

//Function TBZCustomBitmapFilterPointTransform.ProcessPixel(Const inColor : TBZColor) : TBZColor;
//begin
//  Result := inherited ProcessPixel(inColor);
//end;

Function TBZCustomBitmapFilterPointTransform.ProcessPixel(Const inColor : TBZColor) : TBZColor;
begin
  Result := inColor;
end;

Constructor TBZCustomBitmapFilterPointTransform.Create(Const AOwner : TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner, DirectWrite);
end;

{%endregion%}

{%region ====[ TBZCustomBitmapFilterPointTransformEx ]==========================}

function TBZCustomBitmapFilterPointTransformEx.ApplyFilter(Const inColor : TBZColor) : Boolean;
Var
  Lum : Byte;
  //HSLColor : TBZColorFloatHSL;
begin
  //HSLColor.Create(inColor);
  //HueIndex := HSLColor.GetHueIndex;
  Lum := inColor.Luminance;
  Result := (((Lum <  cShadowToneLimit) and FApplyOnShadowColor) or
             ((Lum <  cMidToneLimit)    and FApplyOnMidColor and (Lum >= cShadowToneLimit)) or
             ((Lum >= cMidToneLimit)    and FApplyOnHighlightColor));
           //((FApplyOnRedHue      and (HueIndex = 1)) or
           // (FApplyOnYellowHue   and (HueIndex = 2)) or
           // (FApplyOnGreenHue    and (HueIndex = 3)) or
           // (FApplyOnCyanHue     and (HueIndex = 4)) or
           // (FApplyOnBlueHue     and (HueIndex = 5)) or
           // (FApplyOnMagentaHue  and (HueIndex = 6)) or
           // (FApplyOnMonoHue     and (HueIndex = 7)));
end;

function TBZCustomBitmapFilterPointTransformEx.ApplyPreserveLuminosity(inColor, outColor : TBZColor) : TBZColor;
Var
  LumIn, LumOut  : Byte;
  LumMean, LumDiff : Integer;
  TmpColor : TBZColor;
begin
  LumIn := inColor.Luminance;
  LumOut := outColor.Luminance;
  LumDiff := LumOut - LumIn;
  LumMean := inColor.Luminosity;
  TmpColor := inColor - LumMean;
  LumMean := LumMean + LumDiff;
  Result.Red := ClampByte(TmpColor.Red + LumMean);
  Result.Green := ClampByte(TmpColor.Green + LumMean);
  Result.Blue := ClampByte(TmpColor.Blue + LumMean);
  Result.Alpha := inColor.Alpha;
end;

Function TBZCustomBitmapFilterPointTransformEx.DoComputePixel(Const inColor : TBZColor) : TBZColor;
Var
  outColor : TBZColor;
begin
  Result := inColor;
  if ApplyFilter(inColor) then
  begin
    if not(FPreserveLuminosity) then
    begin
      Result := ProcessPixel(InColor);
    end
    else
    begin
      outColor := ProcessPixel(InColor);
      Result := ApplyPreserveLuminosity(InColor, outColor);
    end;
  end;
end;

Function TBZCustomBitmapFilterPointTransformEx.ProcessPixel(Const inColor : TBZColor) : TBZColor;
begin
  Result := inColor;
end;

Constructor TBZCustomBitmapFilterPointTransformEx.Create(Const AOwner : TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner, DirectWrite);
  FApplyOnShadowColor := True;
  FApplyOnMidColor := True;
  FApplyOnHighlightColor := True;
  FPreserveLuminosity := False;
end;

{%endregion%}

{%region ====[ TBZCustomBitmapFilterApplyLUT ]==================================}

Constructor TBZCustomBitmapFilterApplyLUT.Create(Const AOwner : TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner);
  FApplyOnlyGlobalLUT := False;
end;

Destructor TBZCustomBitmapFilterApplyLUT.Destroy;
begin
  inherited Destroy;
end;

procedure TBZCustomBitmapFilterApplyLUT.InitializeLUT;
Var
  i : Byte;
begin
  For i:=0 to 255 do
  begin
    FRedLUT[i] := 0;
    FGreenLUT[i] := 0;
    FBlueLUT[i] := 0;
    FGlobalLUT[i] := 0;
  end;
end;

procedure TBZCustomBitmapFilterApplyLUT.DoInitializeScanner;
begin
  inherited DoInitializeScanner;
  InitializeLUT;
end;

Function  TBZCustomBitmapFilterApplyLUT.ProcessPixel(Const inColor : TBZColor) : TBZColor;
begin
  Result := inColor;
  if FApplyOnlyGlobalLUT then
  begin
    Result.Create(FGlobalLUT[inColor.Red], FGlobalLUT[inColor.Green], FGlobalLUT[inColor.Blue], inColor.Alpha);
  end
  else
  begin
    Result.Create(FRedLUT[inColor.Red], FGreenLUT[inColor.Green], FBlueLUT[inColor.Blue], inColor.Alpha);
  end;
end;

{%endregion%}

{%region ====[ TBZCustomBitmapFilterNonLinear ]=================================}

Constructor TBZCustomBitmapFilterNonLinear.Create(Const AOwner : TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner, DirectWrite);
  FFilterGetPixel := psmDefault;
  FFilterSize := 3;
  FFilterRadius := 1;
  FEdgeAction := peaClamp;
end;

procedure TBZCustomBitmapFilterNonLinear.SetFilterGetPixel(const AValue : TBZGetPixelSampleMethod);
begin
  if FFilterGetPixel = AValue then Exit;
  FFilterGetPixel := AValue;
end;

procedure TBZCustomBitmapFilterNonLinear.SetEdgeAction(const AValue : TBZPixelEdgeAction);
begin
  if FEdgeAction = AValue then Exit;
  FEdgeAction := AValue;
end;

procedure TBZCustomBitmapFilterNonLinear.SetFilterSize(const AValue : Byte);
begin
  if FFilterSize = AValue then Exit;
  FFilterSize := AValue;
  FFilterRadius := (AValue - 1) div 2;
end;

Function TBZCustomBitmapFilterNonLinear.ProcessPixel(Const inColor : TBZColor) : TBZColor;
begin
  Result := InColor; //inherited DoComputePixel(inColor);
end;

Function TBZCustomBitmapFilterNonLinear.GetPixelColor : TBZColor;
begin
  Result := OwnerBitmap.GetSamplePixel(FDestFloatPoint.X, FDestFloatPoint.Y, FFilterGetPixel, FFilterRadius, FEdgeAction);
end;

{%endregion%}

{%region ====[ TBZCustomBitmapFilterTransformation ]============================}

procedure TBZCustomBitmapFilterTransformation.DoInitializeScanner;
begin
  inherited DoInitializeScanner;
  PrepareTransformation;
end;

procedure TBZCustomBitmapFilterTransformation.PrepareTransformation;
begin
  // Rien a faire pour le moment
end;

function TBZCustomBitmapFilterTransformation.ComputeTransformation(x, y : Integer) : TBZFloatPoint;
begin
  Result.Create(0,0);
end;

Function TBZCustomBitmapFilterTransformation.GetPixelColor : TBZColor;
Var
  {$CODEALIGN VARMIN=16}
  OutPoint : TBZFloatPoint;
  NewPoint : TBZPoint;
  {$CODEALIGN VARMIN=4}
  OldX, OldY : Integer;
begin
  OldX := FCUrrentX;
  OldY := FCurrentY;
  OutPoint := Self.ComputeTransformation(FCurrentX, FCurrentY);
  FDestFloatPoint := OutPoint;
  NewPoint := OutPoint.Floor;
  FCurrentX := NewPoint.X;
  FCurrentY := NewPoint.Y;
  Result := inherited GetPixelColor;
  FCurrentX := OldX;
  FCurrentY := OldY;
end;

{%endregion%}

end.

