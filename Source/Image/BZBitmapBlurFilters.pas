(*
  @abstract(Contient les classes pour l'application de filtres d'adoucissement )

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
unit BZBitmapBlurFilters;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================


interface

uses
  Classes, SysUtils, Math,
  BZClasses, BZMath, BZVectorMath, BZColors, BZGraphic, BZBitmapFilterClasses;

Type
  { @abstract(Applique un filtre d'adoucissement de type radial.)

   Le rendu se fait en 2 temps. Le filtre est appliqué d'abord Verticlament puis horizontalement }
  TBZBitmapFilterBoxBlur =  class(TBZCustomBitmapScanner)
  private
    FRadius : Integer;
    FKSize : Integer;
    FHBlur : TBZBaseBitmap;
    FAvg : Single;
  protected
  //       hSum, AvgSum : TSumRec;

    procedure DoInitializeScanner; Override;
    procedure DoFinalizeScanner; Override;
    function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  Public
    Constructor Create(Const AOwner: TBZBaseBitmap ; Const  DirectWrite :Boolean = False); Override;

    procedure Render; Override;

    { Taille du filtre }
    property Radius : Integer Read FRadius Write FRadius default 3;
  End;

  { Applique un filtre d'adoucissement de type gaussien }
  TBZBitmapFilterGaussianBlur = class(TBZCustomBitmapScanner)
  private
    FRadius : Single;
    FSigma : Single;
    FGaussSum : Single;
    FSize : Integer;
    FGaussMatrice : Array of Single;

  protected
    procedure DoInitializeScanner; Override;
    procedure DoFinalizeScanner; Override;
    Function DoComputePixel(Const inColor : TBZColor) : TBZColor; override;
    function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); override;

    { Taille du filtre }
    property Radius : Single read FRadius write FRadius default 3;
    { Sigma }
    property Sigma : Single read FSigma write FSigma default 2;
  End;

  { Applique un filtre d'adoucissement de type mouvement }
  TBZBitmapFilterMotionBlur = class(TBZCustomBitmapScanner)
  private
    {$CODEALIGN RECORDMIN=16}
    FCenter, FTranslate  : TBZVector2f;
    FBmpSize : TBZVector2i;
    {$CODEALIGN RECORDMIN=4}
    FDirection : Single;
    FDistance : Integer;
    FRotation : Single;
    FZoom : Single;

    FSinAngle, FCosAngle: Single;
    FImageRadius, FMaxDistance : Single;
    FAmount : Integer;

  protected
    procedure DoInitializeScanner; Override;
    function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); override;

    procedure Render; Override;

    { Direction }
    property Direction : Single read FDirection write FDirection default 0;
    { Distance }
    property Distance : Integer read FDistance write FDistance default 5;
    { Angle de rotation }
    property Rotation : Single read FRotation write FRotation default 0;
    { Facteur de zoom }
    property Zoom : Single read FZoom write FZoom default 1;
  End;

  { Applique un filtre d'adoucissement de type zoom }
  TBZBitmapFilterZoomBlur =  class(TBZCustomBitmapScanner)
  private
    FRadius : Integer;
    FFactorPrecisionX, FFactorPrecisionY : Integer;
    FCx, FCy : Integer;
    FDivisor : Single;
  protected
    procedure DoInitializeScanner; Override;
    function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  Public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); override;

    procedure Render; Override;

    { Taille du filtre. Doit-être un nombre impair }
    property Radius : Integer Read FRadius Write FRadius default 3;
    { Position horizontale du centre }
    property CenterX : Integer read FCx write FCx;
    { Position verticale du centre }
    property CenterY : Integer read FCy write FCy;
    { Facteur de précision horizontale }
    property FactorPrecisionX : Integer read FFactorPrecisionX write FFactorPrecisionX;
    { Facteur de précision verticale }
    property FactorPrecisionY : Integer read FFactorPrecisionY write FFactorPrecisionY;
  End;

  { Applique un filtre d'adoucissement de type radial }
  TBZBitmapFilterRadialBlur =  class(TBZCustomBitmapScanner)
  private
    FRadius : Integer;
    FCx, FCy : Integer;
    FQuality : Byte;
    FFactorK : Single;
  protected
    procedure DoInitializeScanner; Override;
    function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  Public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); override;

    procedure Render; Override;

    { Taille du filtre. Doit-être un nombre impair }
    property Radius : Integer Read FRadius Write FRadius default 15;
    { Position horizontale du centre }
    property CenterX : Integer read FCx write FCx;
    { Position verticale du centre }
    property CenterY : Integer read FCy write FCy;
    { Qualité du filtre : 0 : Meilleur, 1 : Bonne, 2 : Moyenne, 3 : Basse}
    property Quality : Byte read FQuality write FQuality;
  End;

  { Applique un filtre d'adoucissement de type adaptatif }
  TBZBitmapFilterThresholdBlur = class(TBZCustomBitmapScanner)
  private
    FRadius : Single;
    FThresholdValue : Byte;
    FGaussianMatrix : Array of single; //PSingle;
    FMatrixWidth : Integer;
  protected
    procedure DoInitializeScanner; Override;
    procedure DoFinalizeScanner; Override;
    function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
  public
    Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); override;

    procedure Render; Override;

    { Taille du filtre. Doit-être un nombre impair }
    property Radius : Single read FRadius write FRadius default 3;
    { Valeur de seuillage }
    property Threshold : Byte read FThresholdValue write FThresholdValue;
  End;

  {Applique des filtres non lineaire. Majoritairement pour la réduction de bruit. cf : TBZGetPixelSampleMethod) }
  TBZBitmapFilterNonLinearBlur = class(TBZCustomBitmapFilterNonLinear);

  { TODO 2 -oBZBitmap -cFiltre : Ajouter d'autre méthodes de lissage
    StackBlur : http://www.quasimondo.com/StackBlurForCanvas/StackBlurDemo.html
    FastBlur : http://www.quasimondo.com/BoxBlurForCanvas/FastBlurDemo.html }

implementation

uses
  BZSystem,
  BZUtils,
  BZVectorMathEx,
  BZVectorMathUtils,
  BZTypesHelpers;

Type
  TSumRec = packed record
    Red, Green, Blue, Alpha : Single;
  End;

{%region ====[ TBZBitmapFilterBoxBlur ]=========================================}

constructor TBZBitmapFilterBoxBlur.Create(const AOwner : TBZBaseBitmap; const DirectWrite : Boolean);
Begin
  Inherited Create(AOwner, True);
  FScannerDescription := 'Adoucissement encadré';
End;

procedure TBZBitmapFilterBoxBlur.DoInitializeScanner;
Begin
  Inherited DoInitializeScanner;
  FKSize :=FRadius+FRadius+1;
  FAvg := FKSize.Reciprocal;
End;

procedure TBZBitmapFilterBoxBlur.DoFinalizeScanner;
begin
  if FHBlur<> nil then FreeAndNil(FHBlur);
  inherited DoFinalizeScanner;
end;

function TBZBitmapFilterBoxBlur.ProcessPixel(const inColor : TBZColor) : TBZColor;
begin
  // Ne fait rien
  Result := InColor;
end;

// Référence : http://blog.ivank.net/fastest-gaussian-blur.html
procedure TBZBitmapFilterBoxBlur.Render;
var
  SrcPtr1, SrcPtr2, SrcPtr3, SrcPtr4, DstPtr : PBZColor;
  Color1, Color2, DstColor, TempColor : TBZColor;
  x,y,j,r,r2, WidthInc,aRadius : Integer;
  Delta : Single;
  hSum, AvgSum : TBZColorVector;
begin
  DoInitializeScanner;
  aRadius := Round(FRadius);
  r := aRadius - 1;
  r2 := aRadius + 1;
  //rs2 := r2;
  FHBlur :=  OwnerBitmap.CreateClone;
  //  Vertical Box Blur
  InitProgress(FInternalClipRect.Width,FInternalClipRect.Height);
  StartProgressSection(0,'');
  StartProgressSection(100,FScannerDescription);

  Delta := 50 / (FInternalClipRect.Height);
  For y:= FInternalClipRect.Top to FInternalClipRect.Bottom do
  begin
    SrcPtr1 := OwnerBitmap.GetScanLine(y); //ti
    SrcPtr4 := OwnerBitmap.GetPixelPtr(FInternalClipRect.Right,y);
    SrcPtr2 := SrcPtr1;
    SrcPtr3 :=  OwnerBitmap.GetPixelPtr(FInternalClipRect.Left+aRadius,y); //ri

    DstPtr := FHBlur.GetScanLine(y); //li

    Color1 := SrcPtr1^;
    Color2 := SrcPtr4^;
    With HSum do
    begin
      Red := Color1.Red * r2;
      Green := Color1.Green * r2;
      Blue := Color1.Blue * r2;
      Alpha := Color1.Alpha * r2;
    End;

    For J:=FInternalClipRect.Left to (FInternalClipRect.Left+r) do
    begin
      TempColor := SrcPtr2^;

      With HSum do
      begin
        Red := HSum.Red + TempColor.Red;
        Green := HSum.Green + TempColor.Green;
        Blue := HSum.Blue + TempColor.Blue;
        Alpha := HSum.Alpha + TempColor.Alpha;
      End;

      inc(SrcPtr2);
    End;

    For J:=FInternalClipRect.Left to (FInternalClipRect.Left+aRadius) do
    begin
      TempColor := SrcPtr3^;

      With HSum do
      begin
        Red := HSum.Red + TempColor.Red - Color1.Red;
        Green := HSum.Green + TempColor.Green - Color1.Green;
        Blue := HSum.Blue + TempColor.Blue - Color1.Blue;
        Alpha := HSum.Alpha + TempColor.Alpha - Color1.Alpha;
      End;
      With DstColor do
      begin
        Red := ClampByte(Round(HSum.Red * FAvg));
        Green := ClampByte(Round(HSum.Green * FAvg));
        Blue := ClampByte(Round(HSum.Blue * FAvg));
        Alpha := ClampByte(Round(HSum.Alpha * FAvg));
      End;
      DstPtr^:= DstColor;
      inc(SrcPtr3);
      inc(DstPtr);
    End;

    For J:=(FInternalClipRect.Left+r2) to (FInternalClipRect.Right-r2) do
    begin
      TempColor := SrcPtr3^;
      Color1 := SrcPtr1^;
      With HSum do
      begin
        Red := HSum.Red + TempColor.Red - Color1.Red;
        Green := HSum.Green + TempColor.Green - Color1.Green;
        Blue := HSum.Blue + TempColor.Blue - Color1.Blue;
        Alpha := HSum.Alpha + TempColor.Alpha - Color1.Alpha;
      End;
      With DstColor do
      begin
        Red := ClampByte(Round(HSum.Red * FAvg));
        Green := ClampByte(Round(HSum.Green * FAvg));
        Blue := ClampByte(Round(HSum.Blue * FAvg));
        Alpha := ClampByte(Round(HSum.Alpha * FAvg));
      End;
      DstPtr ^:= DstColor;
      inc(SrcPtr3);
      inc(DstPtr);
      inc(SrcPtr1);
    End;

    For J:=(FInternalClipRect.Right-aRadius) to (FInternalClipRect.Right) do
    begin
      Color1 := SrcPtr1^;
      With HSum do
      begin
        Red := HSum.Red + Color2.Red - Color1.Red;
        Green := HSum.Green + Color2.Green  - Color1.Green;
        Blue := HSum.Blue + Color2.Blue - Color1.Blue;
        Alpha := HSum.Alpha + Color2.Alpha - Color1.Alpha;
      End;
      With DstColor do
      begin
        Red := ClampByte(Round(HSum.Red * FAvg));
        Green := ClampByte(Round(HSum.Green * FAvg));
        Blue := ClampByte(Round(HSum.Blue * FAvg));
        Alpha := ClampByte(Round(HSum.Alpha * FAvg));
      End;
      DstPtr ^:= DstColor;
      inc(DstPtr);
      inc(SrcPtr1);
    End;
    AdvanceProgress(Delta,0,1,True);
  End;

  // Horizontal Box Blur
  Delta := 50 / (FInternalClipRect.Width);
  WidthInc :=  FHBlur.Width;
  For x:= FInternalClipRect.Left to FInternalClipRect.Right do
  begin
    SrcPtr1 := FHBlur.GetPixelPtr(X,FInternalClipRect.Top);
    SrcPtr2 := SrcPtr1;
    SrcPtr4 := FHBlur.GetPixelPtr(X,FInternalClipRect.Bottom);
    SrcPtr3 := FHBlur.GetPixelPtr(X,aRadius);
    DstPtr := OwnerBitmap.GetPixelPtr(X,FInternalClipRect.Top);
    Color1 := SrcPtr1^;
    Color2 := SrcPtr4^;
    With HSum do
    begin
      Red := Color1.Red * r2;
      Green := Color1.Green * r2;
      Blue := Color1.Blue * r2;
      Alpha := Color1.Alpha * r2;
    End;

    For J:=FInternalClipRect.Top to (FInternalClipRect.Top+r) do
    begin
      TempColor := SrcPtr2^;
      With HSum do
      begin
        Red := HSum.Red + TempColor.Red;
        Green := HSum.Green + TempColor.Green;
        Blue := HSum.Blue + TempColor.Blue;
        Alpha := HSum.Alpha + TempColor.Alpha;
      End;
      inc(SrcPtr2,WidthInc);
    End;

    For J:=FInternalClipRect.Top to (FInternalClipRect.Top+aRadius) do
    begin
      TempColor := SrcPtr3^;
      With HSum do
      begin
        Red := HSum.Red + TempColor.Red - Color1.Red;
        Green := HSum.Green + TempColor.Green - Color1.Green;
        Blue := HSum.Blue + TempColor.Blue - Color1.Blue;
        Alpha := HSum.Alpha + TempColor.Alpha - Color1.Alpha;
      End;
      With DstColor do
      begin
      Red := ClampByte(Round(HSum.Red * FAvg));
      Green := ClampByte(Round(HSum.Green * FAvg));
      Blue := ClampByte(Round(HSum.Blue * FAvg));
      Alpha := ClampByte(Round(HSum.Alpha * FAvg));
    End;

      if (OwnerBitmap.UseSelectionMask)  then //and (OwnerBitmap.ApplyMask)
      begin
        FMaskPixelPtr := OwnerBitmap.SelectionMask.GetPixelPtr(X,J);
        if FMaskPixelPtr^.Red > 0 then
        begin
          if FMaskPixelPtr^.Red = 255 then DstPtr^:= DstColor
          else
          begin
            //TempColor.Alpha := FMaskPixelPtr^.Red;
            DstPtr^:= TempColor.Blend(DstColor, FMaskPixelPtr^.Red);
          end;
        end;
      end
      else DstPtr^:= DstColor;

      Inc(SrcPtr3,WidthInc);
      inc(DstPtr,WidthInc);
    End;

    For J:=(FInternalClipRect.Top+r2) to (FInternalClipRect.Bottom-r2) do
    begin
      TempColor := SrcPtr3^;
      Color1 := SrcPtr1^;
      With HSum do
      begin
        Red := HSum.Red + TempColor.Red - Color1.Red;
        Green := HSum.Green + TempColor.Green - Color1.Green;
        Blue := HSum.Blue + TempColor.Blue - Color1.Blue;
        Alpha := HSum.Alpha + TempColor.Alpha - Color1.Alpha;
      End;
      With DstColor do
      begin
        Red := ClampByte(Round(HSum.Red * FAvg));
        Green := ClampByte(Round(HSum.Green * FAvg));
        Blue := ClampByte(Round(HSum.Blue * FAvg));
        Alpha := ClampByte(Round(HSum.Alpha * FAvg));
      End;

      if (OwnerBitmap.UseSelectionMask) then // and (OwnerBitmap.ApplyMask)
      begin
        FMaskPixelPtr := OwnerBitmap.SelectionMask.GetPixelPtr(X,J);
        if FMaskPixelPtr^.Red > 0 then
        begin
          if FMaskPixelPtr^.Red = 255 then DstPtr^:= DstColor
          else
          begin
            //TempColor.Alpha := FMaskPixelPtr^.Red;
            DstPtr^:= TempColor.Blend(DstColor, FMaskPixelPtr^.Red);
          end;
        end;
      end
      else DstPtr^:= DstColor;

      inc(SrcPtr3,WidthInc);
      inc(DstPtr,WidthInc);
      inc(SrcPtr1,WidthInc);
    End;

    For J:=(FInternalClipRect.Bottom-aRadius) to (FInternalClipRect.Bottom) do
    begin
      Color1 := SrcPtr1^;
      With HSum do
      begin
        Red := HSum.Red + Color2.Red - Color1.Red;
        Green := HSum.Green + Color2.Green  - Color1.Green;
        Blue := HSum.Blue + Color2.Blue - Color1.Blue;
        Alpha := HSum.Alpha + Color2.Alpha - Color1.Alpha;
      End;
      With DstColor do
      begin
        Red := ClampByte(Round(HSum.Red * FAvg));
        Green := ClampByte(Round(HSum.Green * FAvg));
        Blue := ClampByte(Round(HSum.Blue * FAvg));
        Alpha := ClampByte(Round(HSum.Alpha * FAvg));
      End;

      if (OwnerBitmap.UseSelectionMask)  then //and (OwnerBitmap.ApplyMask)
      begin
        FMaskPixelPtr := OwnerBitmap.SelectionMask.GetPixelPtr(X,J);
        if FMaskPixelPtr^.Red > 0 then
        begin
          if FMaskPixelPtr^.Red = 255 then DstPtr^:= DstColor
          else
          begin
            //TempColor.Alpha := FMaskPixelPtr^.Red;
            DstPtr^:= Color1.Blend(DstColor, FMaskPixelPtr^.Red);
          end;
        end;
      end
      else DstPtr^:= DstColor;

      inc(DstPtr,WidthInc);
      inc(SrcPtr1,WidthInc);
    End;
    AdvanceProgress(Delta,0,1,True);
  End;

  FinishProgressSection(False);
  FinishProgressSection(True);
  DoFinalizeScanner;
End;

{%endregion}

{%region ====[ TBZBitmapFilterGaussianBlur ]====================================}

constructor TBZBitmapFilterGaussianBlur.Create(const AOwner : TBZBaseBitmap; const DirectWrite : Boolean);
Begin
  Inherited Create(AOwner, True);
  FScannerDescription := 'Adoucissement gaussien';
End;

procedure TBZBitmapFilterGaussianBlur.DoInitializeScanner;
VAr
  Theta2, Theta3 : Single;
  j,i : Integer;
  MSize,  MatrixSize, jj,ii:Integer;
  GaussPtr : PSingle;
  Coef : Single;
Begin
  Inherited DoInitializeScanner;
  // si la taille est inférieur ou égal à 0.8, il n'y a pas de flou...
  If Radius <= 0.8 Then exit; // result := false; exit;
  MatrixSize := Round(Radius * Radius);
  // calcul la matrice pour le filtre
  Theta3 := FSigma * FSigma;
  theta2:= 2 * Theta3;
  //Theta3 := cPI * Theta2;
  //Theta3 := Theta3.Reciprocal;
  Theta3 := 1 / (c2Pi * Theta3);
  FSize:= (MatrixSize div 2);
  setlength(FGaussMatrice, MatrixSize);
  //FGaussMatrice := nil;
  //memReAlloc(FGaussMatrice, (MSize*2) * Sizeof(Single));
  //ReallocMem(FGaussMatrice, MatrixSize * Sizeof(Single));
  //GaussPtr := FGaussMatrice;
  FGaussSum:=0;
  for j:=-FSize to FSize do
  begin
    jj := FSize + j;
    //setlength(FGaussMatrice[pj],MSize);
    for i:=-FSize to FSize do
     begin
      ii := FSize + i;
      //FGaussMatrice[i,j]
      Coef:= System.exp(-(j * j + i * i) / theta2) * theta3;
      FGaussMatrice[jj * (FSize+1) + ii] := Coef;
      //GaussPtr^:= Coef;
      //PSingle(GaussPtr+(pj*FSize+pi))^ := Coef;
      FGaussSum := FGaussSum + Coef;
      //Inc(GaussPtr);
     end;
  end;
  FGaussSum := FGaussSum.Reciprocal;
  OwnerBitmap.PreMultiplyAlpha;
End;

procedure TBZBitmapFilterGaussianBlur.DoFinalizeScanner;
var
  i, MSize : Integer;
Begin

  //MSize:= (FSize+FSize)+1;
  //for i := 0 to MSize-1 do
  //begin
  //  Setlength(FGaussMatrice[i],0);
  //  FGaussMatrice[i] := nil;
  //end;
  Setlength(FGaussMatrice,0);
  //FreeMem(FGaussMatrice);
 // owner.UnPreMultiplyAlpha;
  FGaussMatrice := nil;
  Inherited DoFinalizeScanner;
End;

function TBZBitmapFilterGaussianBlur.DoComputePixel(const inColor : TBZColor) : TBZColor;
//Type
//  TSumRec = packed record
//    Red,Green, Blue, Alpha : Single;
//  end;
var
  SumRec, ColorF, WeightColor: TBZColorVector; //TSumRec;
  x,y,xx,yy,j,i:integer;
  f : Single;
  GaussPtr : PSingle;
Begin
  //With SumRec Do
  //Begin
  //  Red := 0;
  //  Green := 0;
  //  Blue := 0;
  //  Alpha := 0;
  //End;
  SumRec.Create(0,0);
//  DoInitializeScanner;
  //GaussPtr := FGaussMatrice;
  for y:=-FSize to FSize do
  begin
    j := FSize + y;
    yy := Clamp(FCurrentY + y, 0, OwnerBitmap.MaxHeight);
    for x:=-FSize to FSize do
    begin
      i := FSize + x;
      xx := Clamp(FCurrentX + x, 0, OwnerBitmap.MaxWidth);
      ColorF := OwnerBitmap.getPixel(xx, yy).AsColorVector;
      //f := GaussPtr^; //PSingle(GaussPtr+(j * FSize + i))^ ; //FGaussMatrice[pj,pi];
      f := FGaussMatrice[j * (FSize+1) + i];
      WeightColor.Create(f,f);
      ColorF := ColorF * WeightColor;
      SumRec := SumRec + ColorF;
      //Inc(GaussPtr);
      //With SumRec  Do
      //Begin
      //  Red := Red + f * C.Red;
      //  Green := Green + f * C.Green;
      //  Blue := Blue + f * C.Blue;
      //  Alpha := Alpha + f * C.Alpha;
      //End;
    end;
  end;
  WeightColor.Create(FGaussSum,FGaussSum);
  ColorF := SumRec * WeightColor;
  Result.Create(ColorF);
  //With C Do
  //Begin
  //  Red := ClampByte(Round(SumRec.Red*FGaussSum));// total));
  //  Green := ClampByte(Round(SumRec.Green*FGaussSum)); // total));
  //  Blue := ClampByte(Round(SumRec.Blue*FGaussSum)); // total));
  //  Alpha := ClampByte(Round(SumRec.Alpha*FGaussSum)); // total));
  //End;

End;

function TBZBitmapFilterGaussianBlur.ProcessPixel(const inColor : TBZColor) : TBZColor;
begin
  // Ne fait rien
  Result := inColor;
end;


{%endregion}

{%region ====[ TBZBitmapFilterMotionBlur ]======================================}

constructor TBZBitmapFilterMotionBlur.Create(const AOwner : TBZBaseBitmap; const DirectWrite : Boolean);
begin
  inherited Create(AOwner);
  FScannerDescription := 'Flou de mouvement';

end;

procedure TBZBitmapFilterMotionBlur.DoInitializeScanner;
Var
  Dir : Single;
Begin
  Inherited DoInitializeScanner;
  Dir := DegToRadian(FDirection);
  FSinAngle := System.Sin(Dir);
  FCosAngle := System.Cos(Dir);
  FBmpSize.Create(OwnerBitmap.Width, OwnerBitmap.Height);
  FCenter.Create(OwnerBitmap.CenterX+1,OwnerBitmap.CenterY+1);
  FImageRadius := FCenter.Length;
  if FRotation > 0 then FRotation := DegToRadian(FRotation) / FDistance;
  FMaxDistance := FDistance + Abs(FImageRadius * FRotation) + FZoom * FImageRadius;
  if FMaxDistance  > 100 then FMaxDistance := 100 + Sqrt(FMaxDistance-100);
  FAmount := Round(FMaxDistance)-1;
  FTranslate.Create(FDistance * FCosAngle, FDistance * -FSinAngle);
End;

function TBZBitmapFilterMotionBlur.ProcessPixel(const inColor : TBZColor) : TBZColor;
begin
  // Ne fait rien
  Result := inColor;
end;

procedure TBZBitmapFilterMotionBlur.Render;
Var
  Delta,f : Single;
  X, Y : Integer;
  aColor : TBZColor;

  procedure RenderPixel(px,py : Integer);
  Var
    i, j, CountPixel : Integer;
    {$CODEALIGN VARMIN=16}
    Pt : TBZVector2f;
    nPt : TBZVector2i;
    inColor, SumColor : TBZColorVector;
    AffineTransform : TBZAffineMatrix;
    TransVec : TBZVector2f;
    {$CODEALIGN VARMIN=4}

    s : Single;
    outColor : TBZColor;


  begin
    SumColor.Create(0,0);
    CountPixel := 0;
    For i := 0 to FAmount do
    begin
      f := i / FAmount;
      pt.Create(px,py);
      s := 1-FZoom*f;
      AffineTransform.CreateIdentityMatrix;
      TransVec := FCenter + (FTranslate * f);
      AffineTransform.Translate(TransVec);
      if ( FRotation <> 0 ) then AffineTransform.Rotate( -FRotation*f );
      AffineTransform.Scale(s,s);
      AffineTransform.Translate(-FCenter);
       pt := AffineTransform * pt;

      //pt := pt + FCenter;
      //pt := pt + (FTranslate * f);
      //pt := pt * s;
      //if ( FRotation <> 0 ) then pt := pt.Rotate(-FRotation * f, FCenter);
      //pt := pt - FCenter;

      npt := pt.Round;
      npt := npt mod FBmpSize;
      npt := npt.Abs;

      inColor := OwnerBitmap.getPixel(npt.x, npt.y).AsColorVector;

      Inc(CountPixel);
      SumColor := sumColor + inColor;
    end;
    if CountPixel > 0 then
    begin
      SumColor := SumColor / CountPixel;
      outColor.Create(SumColor);
      FDestBmp.setPixel(pX,pY,outColor);
    end
    else
    begin
      outColor :=  OwnerBitmap.GetPixel(pX, pY);
      FDestBmp.setPixel(pX,pY,outColor);
    end;

  end;

begin
  DoInitializeScanner;
  InitProgress(FInternalClipRect.Width,FInternalClipRect.Height);
  StartProgressSection(0,'');
  StartProgressSection(100,FScannerDescription);
  Delta := 100 / (FInternalClipRect.Height);
  For Y := FInternalClipRect.Top To FInternalClipRect.Bottom Do
  Begin
    For X := FInternalClipRect.Left To FInternalClipRect.Right Do
    begin
      if not(OwnerBitmap.UseSelectionMask)  then //and (OwnerBitmap.ApplyMask)
      begin
        RenderPixel(X,Y);
      end
      else
      begin
        FMaskPixelPtr := OwnerBitmap.SelectionMask.GetPixelPtr(X, Y);
        if FMaskPixelPtr^.Red>0 then
        begin
          RenderPixel(X,Y);
        end
        else
        begin
          aColor :=  OwnerBitmap.GetPixel(X, Y);
          FDestBmp.setPixel(X,Y,aColor);
        end;
      end;
    end;
    AdvanceProgress(Delta,0,1,True);
  end;
  FinishProgressSection(False);
  FinishProgressSection(True);
  DoFinalizeScanner;
end;

{%endregion}

{%region=====[ TBZBitmapFilterZoomBlur ]========================================}

constructor TBZBitmapFilterZoomBlur.Create(const AOwner : TBZBaseBitmap; const DirectWrite : Boolean);
begin
  inherited Create(AOwner,False);
  FScannerDescription := 'Adoucissement Zoom';
end;

procedure TBZBitmapFilterZoomBlur.DoInitializeScanner;
begin
  inherited DoInitializeScanner;
  FRadius := Abs(FRadius);
  if FRadius=0 then FRadius := 1;
  FDivisor := FRadius + FRadius + 1;
end;

function TBZBitmapFilterZoomBlur.ProcessPixel(const inColor : TBZColor) : TBZColor;
begin
  // Ne fait rien
  Result := inColor;
end;

procedure TBZBitmapFilterZoomBlur.Render;
Var
  x,y,i, DistX, DistY, PixX, PixY : Integer;
  Delta, Divisor : Single;
  ColorSum, InColor : TBZColorVector;
  OutColor : TBZColor;
  TmpBmp : TBZBaseBitmap;
  Alpha : Byte;
begin
  DoInitializeScanner;
  //TmpBmp := TBZBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);
  InitProgress(FInternalClipRect.Width,FInternalClipRect.Height);
  StartProgressSection(0,'');
  StartProgressSection(100,FScannerDescription);
  Delta := 100 / (FInternalClipRect.Height);
   //From -10 to 10
  For Y := FInternalClipRect.Top To FInternalClipRect.Bottom Do
  Begin
    DistY := Y-FCy;
    For X := FInternalClipRect.Left To FInternalClipRect.Right Do
    Begin

      DistX := X-FCx;
      ColorSum.Create(0,0);
      if not(OwnerBitmap.UseSelectionMask)  then //and (OwnerBitmap.ApplyMask)
      begin
        //Alpha := OwnerBitmap.getPixel(x, y).Alpha;
        For I := -FRadius To FRadius Do
        Begin
          PixX := Round(X+I*DistX/FFactorPrecisionX);
          PixY := Round(Y+I*DistY/FFactorPrecisionY);
          InColor := OwnerBitmap.getPixel(PixX, PixY, peaClamp).AsColorVector;
          ColorSum := ColorSum + InColor;
        End;
        ColorSum := ColorSum / FDivisor;
        OutColor.Create(ColorSum);
        //OutColor.Alpha := Alpha;
        FDestBmp.setPixel(X,Y, OutColor);
      end
      else
      begin
         FMaskPixelPtr := OwnerBitmap.SelectionMask.GetPixelPtr(X, Y);
         if FMaskPixelPtr^.Red>0 then
         begin
           //Alpha := OwnerBitmap.getPixel(x, y).Alpha;
           For I := -FRadius To FRadius Do
           Begin
             PixX := Round(X+I*DistX/FFactorPrecisionX);
             PixY := Round(Y+I*DistY/FFactorPrecisionY);
             InColor := OwnerBitmap.getPixel(PixX, PixY, peaClamp).AsColorVector;
             ColorSum := ColorSum + InColor;
           End;
           ColorSum := ColorSum / FDivisor;
           OutColor.Create(ColorSum);
           //OutColor.Alpha := Alpha;
           FDestBmp.setPixel(X,Y, OutColor);
         end;
      end;
    End;
    AdvanceProgress(Delta,0,1,True);
  End;
  FinishProgressSection(False);
  FinishProgressSection(True);
  DoFinalizeScanner;
//
//  OwnerBitmap.FastCopy(TmpBmp);
//  FreeAndNil(TmpBmp);
end;

{%endregion%}

{%region=====[ TBZBitmapFilterRadialBlur ]======================================}

constructor TBZBitmapFilterRadialBlur.Create(const AOwner : TBZBaseBitmap; const DirectWrite : Boolean);
begin
  inherited Create(AOwner);
  FScannerDescription := 'Adoucissement RadialZoom';
end;

procedure TBZBitmapFilterRadialBlur.DoInitializeScanner;
begin
  inherited DoInitializeScanner;
  FRadius := Clamp(FRadius,1,100);
  FFactorK := FRadius / 400;
end;

function TBZBitmapFilterRadialBlur.ProcessPixel(const inColor : TBZColor) : TBZColor;
begin
  // Ne fait rien
  Result := inColor;
end;

procedure TBZBitmapFilterRadialBlur.Render;
Var
  j,i,l,dX,dY, px, py : Integer;
  Delta, Hyp,Alpha, nX, nY : Single;
  vMax, Divisor : Integer;
  ColorSum, InColor : TBZColorVector;
  OutColor : TBZColor;
begin
  DoInitializeScanner;
  InitProgress(FInternalClipRect.Width,FInternalClipRect.Height);
  StartProgressSection(0,'');
  StartProgressSection(100,FScannerDescription);
  Delta := 100 / (FInternalClipRect.Height);
  For j := FInternalClipRect.Top to FInternalClipRect.Bottom do
  begin
    dY := Abs(j - FCy);
    If dy = 0 Then dy := 1;
    For i := FInternalClipRect.Left to FInternalClipRect.Right do
    begin
      dX := Abs(i - FCx);
      If dX = 0 Then dX := 1;

      Alpha := Arctan2(dy,dx);
      Hyp := Sqrt(dX * dX + dY * dY);

      ColorSum.Create(0,0);
      Divisor := 0;
      vMax := Abs(Round(Hyp * FFactorK));

      //if not((TBZBitmap(OwnerBitmap).UseMask) and not(TBZBitmap(OwnerBitmap).Mask.Apply)) then
      //begin
        l := -vMax;
        While l<=vMax do
        begin
          If (i < FCx) Then nX := i + (System.Cos(Alpha) * l)
          Else If (i >= FCx) Then nX := i - (System.Cos(Alpha) * l)
          Else nX := 0;
          If (j < FCy) Then nY := j + (System.Sin(Alpha) * l)
          Else If (j >= FCy) Then nY := j - (System.Sin(Alpha) * l)
          Else nY := 0;
          px := Round(nX);
          py := Round(nY);
          If OwnerBitmap.CheckPixelBound(pX, py) Then
          begin
            InColor := OwnerBitmap.GetPixel(pX, pY).AsColorVector;
            ColorSum := ColorSum + InColor;
            Divisor := Divisor + 1
          End;
          l := l + Quality;
        end;
        If Divisor > 0 Then
        begin
          InColor := ColorSum / Divisor;
          OutColor.Create(InColor);
          FDestBmp.SetPixel(i,j,OutColor);
        end
        else
        begin
          OutColor := OwnerBitmap.GetPixel(i,j);
          FDestBmp.SetPixel(i,j,OutColor);
        end;
      //end
      //else
      //begin
      //  FMaskPixelPtr := TBZBitmap(OwnerBitmap).Mask.GetPixelPtr(i, j);
      //  if FMaskPixelPtr^.Red>0 then
      //  begin
      //    l := -vMax;
      //    While l<=vMax do
      //    begin
      //      If (i < FCx) Then nX := i + (System.Cos(Alpha) * l)
      //      Else If (i >= FCx) Then nX := i - (System.Cos(Alpha) * l)
      //      Else nX := 0;
      //      If (j < FCy) Then nY := j + (System.Sin(Alpha) * l)
      //      Else If (j >= FCy) Then nY := j - (System.Sin(Alpha) * l)
      //      Else nY := 0;
      //      px := Round(nX);
      //      py := Round(nY);
      //      If OwnerBitmap.CheckPixelBound(pX, py) Then
      //      begin
      //        InColor := OwnerBitmap.GetPixel(pX, pY).AsColorVector;
      //        ColorSum := ColorSum + InColor;
      //        Divisor := Divisor + 1
      //      End;
      //      l := l + Quality;
      //    end;
      //
      //    If Divisor > 0 Then
      //    begin
      //      InColor := ColorSum / Divisor;
      //      OutColor.Create(InColor);
      //      FDestBmp.SetPixel(i,j,OutColor);
      //    end
      //    else
      //    begin
      //      OutColor := OwnerBitmap.GetPixel(i,j);
      //      FDestBmp.SetPixel(i,j,OutColor);
      //    end;
      //  end;
      //end;
    end;
    AdvanceProgress(Delta,0,1,True);
  end;
  FinishProgressSection(False);
  FinishProgressSection(True);
  DoFinalizeScanner;
end;

{%endregion%}

{%region=====[ TBZBitmapFilterThresholdBlur ]===================================}

constructor TBZBitmapFilterThresholdBlur.Create(const AOwner : TBZBaseBitmap; const DirectWrite : Boolean);
begin
  inherited Create(AOwner, True);
  FScannerDescription := 'Adoucissement par seuillage';
end;

procedure TBZBitmapFilterThresholdBlur.DoInitializeScanner;
var
  i, r, Index  : Integer;
  Col, Cols   : Integer;
  Sigma        : Single;
  Sigma22      : Single;
  SigmaPi2     : Single;
  SqrtSigmaPi2 : Single;
  Radius2      : Single;
  Total        : Single;
  Distance     : Single;
 // Matrix       : PSiingle; //array of Single;
begin
  inherited DoInitializeScanner;
  // Creation de la matrice gaussienne
  r            := Ceil(FRadius);
  FMatrixWidth := r * 2 + 1;
  Sigma        := FRadius / 3;
  Sigma22      := 2 * Sigma * Sigma;
  SigmaPi2     := c2Pi * Sigma;
  SqrtSigmaPi2 := System.Sqrt(SigmaPi2);
  Radius2      := FRadius * FRadius;
  Total        := 0.0;
  Index        := 0;

  SetLength(FGaussianMatrix, FMatrixWidth);
  for Col := -r to r do
  begin
    Distance := Col * Col;
    if Distance > Radius2 then
    begin
      FGaussianMatrix[Index] := 0.0;
    end
    else
    begin
      FGaussianMatrix[Index] := System.Exp(-Distance / Sigma22) / SqrtSigmaPi2;
    end;
    Total := Total + FGaussianMatrix[Index];
    Inc(Index);
  end;

  for i := 0 to (FMatrixWidth - 1) do
  begin
    FGaussianMatrix[i] := FGaussianMatrix[i] / Total;
  end;
end;

procedure TBZBitmapFilterThresholdBlur.DoFinalizeScanner;
begin
  SetLength(FGaussianMatrix,0);
  FGaussianMatrix := nil;
  inherited DoFinalizeScanner;
end;

function TBZBitmapFilterThresholdBlur.ProcessPixel(const inColor : TBZColor) : TBZColor;
begin
  // Ne fait rien
  Result := inColor;
end;

procedure TBZBitmapFilterThresholdBlur.Render;
Var
  DstBmp : TBZCustomBitmap;
  Delta : Single;

  procedure ThresholdBlur(inBmp : TBZCustomBitmap; Var outBmp : TBZCustomBitmap; aWidth, aHeight : Integer); //Const AAlpha: Boolean = true);
  var
    x, y, ix, xstart, yStart    : Integer;
    col           : Integer;
    cols, cols2  : Integer;
    ioffset       : Integer;
    moffset       : Integer;
    outIndex      : Integer;
    ColorF : TBZColorVector;
    Color1F, Color2F, FColor : TBZColorVector;
    outColor : TBZColor;
    f, t, d              : Single;
  begin

    t := FThresholdValue * _FloatColorRatio;
    cols  := FMatrixWidth;
    cols2 := cols div 2;

    xStart := FInternalClipRect.Left;
    yStart := FInternalClipRect.Top;

    if aWidth < aHeight then
    begin
      xStart := FInternalClipRect.Top;
      yStart := FInternalClipRect.Left;
    end;

    for y := yStart to aHeight-1 do
    begin
      ioffset  := y * aWidth;
      outIndex := y;

      for x := xStart to aWidth-1 do
      begin
        //r := 0;
        //g := 0;
        //b := 0;
        //a := 0;
        FColor.Create(0,0);

        moffset := cols2;


        Color1F := inBmp.getPixelOffset(ioffset + x).AsColorVector;
        //FMaskPixelPtr^ := TBZBitmap(OwnerBitmap).Mask.getPixelOffset(ioffset + x);
        //if ((TBZBitmap(OwnerBitmap).UseMask) and (TBZBitmap(OwnerBitmap).Mask.Apply)) and (FMaskPixelPtr^.Red=0) then
        //begin
        //  outColor.Create(Color1F);
        //  outBmp.setPixelOffset(outIndex,outColor);
        //  OutIndex := OutIndex + inBmp.Height;
        //  continue;
        //end;
        //else
        //begin
          ColorF.Create(0,0);

          for col := -cols2 to cols2 do
          begin
            f := FGaussianMatrix[moffset + col];

            if (f <> 0) then
            begin
              ix := x + col;

              if not ( (ix >= 0) and (ix < aWidth) ) then
              begin
                ix := x;
              end;
              Color2F := inBmp.getPixelOffset(ioffset + ix).AsColorVector;
              d := Color1F.Alpha - Color2F.Alpha;
              if (d >= (-t)) and (d <= t) then
              begin
                FColor.Alpha  := FColor.Alpha + f * Color2F.Alpha;
                ColorF.Alpha := ColorF.Alpha + f;
              end;

              d := Color1F.Red - Color2F.Red;
              if (d >= (-t)) and (d <= t) then
              begin
                FColor.Red  := FColor.Red + f * Color2F.Red;
                ColorF.Red := ColorF.Red + f;
              end;

              d := Color1F.Green - Color2F.Green;
              if (d >= (-t)) and (d <= t) then
              begin
                FColor.Green  := FColor.Green + f * Color2F.Green;
                ColorF.Green := ColorF.Green + f;
              end;

              d := Color1F.Blue - Color2F.Blue;
              if (d >= (-t)) and (d <= t) then
              begin
                FColor.Blue  := FColor.Blue + f * Color2F.Blue;
                ColorF.Blue := ColorF.Blue + f;
              end;
            end;
          end;

          if ColorF.Alpha = 0 then
          begin
            FColor.Alpha := Color1F.Alpha;
          end
          else
          begin
            FColor.Alpha := FColor.Alpha / ColorF.Alpha;
          end;

          if ColorF.Red = 0 then
          begin
            FColor.Red := Color1F.Red;
          end
          else
          begin
            FColor.Red := FColor.Red / ColorF.Red;
          end;

          if ColorF.Green = 0 then
          begin
            FColor.Green := Color1F.Green;
          end
          else
          begin
            FColor.Green := FColor.Green / ColorF.Green;
          end;

          if ColorF.Blue = 0 then
          begin
            FColor.Blue := Color1F.Blue;
          end
          else
          begin
            FColor.Blue := FColor.Blue / ColorF.Blue;
          end;

          outColor.Create(FColor);
          outBmp.setPixelOffset(outIndex,outColor);


          OutIndex := OutIndex + aHeight; //FInternalClipRect.Height;
        end;
      //end;
      AdvanceProgress(Delta,0,1,True);
    end;
  end;

begin
  DoInitializeScanner;

  DstBmp := TBZCustomBitmap.Create(OwnerBitmap.Width, OwnerBitmap.Height);

  InitProgress(FInternalClipRect.Width,FInternalClipRect.Height);
  StartProgressSection(0,'');
  StartProgressSection(100,FScannerDescription);
  //Delta := 100 / (FInternalClipRect.Height);

  StartProgressSection(50,FScannerDescription + ' Passe 1');
  Delta := 100 / (FInternalClipRect.Height);
  ThresholdBlur(OwnerBitmap, DstBmp, FInternalClipRect.Width, FInternalClipRect.Height);
  FinishProgressSection(True);
  //AdvanceProgress(50,0,1,True);


  StartProgressSection(50,FScannerDescription + ' Passe 2');
  Delta := 100 / (FInternalClipRect.Width);
  ThresholdBlur(DstBmp, TBZCustomBitmap(OwnerBitmap), FInternalClipRect.Height, FInternalClipRect.Width);
  FinishProgressSection(True);
  //AdvanceProgress(50,0,1,True);


  FreeAndNil(DstBmp);

  FinishProgressSection(False);
  FinishProgressSection(True);
  DoFinalizeScanner;
end;

{%endregion%}

end.

