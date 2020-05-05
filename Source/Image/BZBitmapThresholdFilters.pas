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
unit BZBitmapThresholdFilters;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================


interface

uses
  Classes, SysUtils,
  BZClasses, BZMath, BZColors, BZGraphic, BZBitmapFilterClasses;

Type
  { Enumération des methodes pour le filtre de seuillage adaptatif }
   TBZFilterAdaptativeThresOldMode = (atmAverage, atmMinMax, atmGaussian);

   { @abstract(Classe de base pour l'application d'un filtre de seuillage)
     @unorderedlist(
       @item(https://en.wikipedia.org/wiki/Thresholding_(image_processing))
       @item(https://homepages.inf.ed.ac.uk/rbf/HIPR2/adpthrsh.htm)
       @item(http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.420.7883&rep=rep1&type=pdf)
       @item(https://en.wikipedia.org/wiki/Balanced_histogram_thresholding)
       @item(https://en.wikipedia.org/wiki/Otsu%27s_method)
       @item(https://theailearner.com/2019/07/19/improving-global-thresholding/)
       @item(http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.475.6638&rep=rep1&type=pdf)
       @item(https://dsp.stackexchange.com/questions/2411/what-are-the-most-common-algorithms-for-adaptive-thresholding)
       @item(https://dsp.stackexchange.com/questions/1932/what-are-the-best-algorithms-for-document-image-thresholding-in-this-example)
       @item(https://perso.liris.cnrs.fr/christian.wolf/papers/icpr2002v.pdf)
       @item(https://github.com/chriswolfvision/local_adaptive_binarization)
       @item(http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=2CB60307341127953FEE6E84A4E5CCFD?doi=10.1.1.130.8869&rep=rep1&type=pdf)
       @item(https://www.researchgate.net/publication/220827321_Adaptive_Thresholding_Methods_for_Documents_Image_Binarization)
       @item(http://cs.haifa.ac.il/hagit/courses/ip/Lectures/Ip12_Segmentation.pdf)
       @item(https://www.researchgate.net/publication/309967669_Image_thresholding_techniques_A_survey_over_categories)
       @item(https://aircconline.com/cseij/V6N1/6116cseij01.pdf)
       @item(https://www.cse.unr.edu/~bebis/CS791E/Notes/Thresholding.pdf)
       @item(https://eprints.ucm.es/16932/1/Tesis_Master_Daniel_Martin_Carabias.pdf)
       @item(
     ) }
   TBZBitmapThresholdBaseFilter = class(TBZCustomBitmapFilterPointTransformEx)
   private

     FMulti : Boolean;
     { TODO 2 -oBZBitmap -cFiltre : Remplacer FMulti par FDual et ajouter le mode multi (tableau de seuil min et max) }

     FRedMinThreshold : Byte;
     FGreenMinThreshold : Byte;
     FBlueMinThreshold : Byte;
     FRedMaxThreshold : Byte;
     FGreenMaxThreshold : Byte;
     FBlueMaxThreshold : Byte;

     FMinThreshold : Byte;
     FMaxThreshold : Byte;
     FMinColor : TBZColor;
     FMaxColor : TBZColor;
     FInterpolate : Boolean;
     FAsGray : Boolean;


     procedure SetRedMinThreshold(const AValue : Byte);
     procedure SetGreenMinThreshold(const AValue : Byte);
     procedure SetBlueMinThreshold(const AValue : Byte);
     procedure SetMulti(const AValue : Boolean);
     procedure SetMinThreshold(const AValue : byte);
     procedure SetMaxThresHold(const AValue : Byte);
     procedure SetInterpolate(const AValue : Boolean);
     procedure SetMinColor(const AValue : TBZColor);
     procedure SetMaxColor(const AValue : TBZColor);
     procedure SetRedMaxThreshold(const AValue : Byte);
     procedure SetGreenMaxThreshold(const AValue : Byte);
     procedure SetBlueMaxThreshold(const AValue : Byte);
     procedure SetAsGray(const AValue : Boolean);
   protected
     FHistogram : Array[0..255] of byte;
     procedure ComputeHistogram;

     procedure DoInitializeScanner; Override;
     Function ProcessPixel(Const inColor : TBZColor) : TBZColor; override;
   public
     Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;

     { Drapeau pour le mode multi/dual }
     property Multi : Boolean read FMulti write SetMulti;

     { Seuil minimum pour le canal rouge }
     property RedMinThreshold : Byte read FRedMinThreshold write SetRedMinThreshold;
     { Seuil minimum pour le canal vert }
     property GreenMinThreshold : Byte read FGreenMinThreshold write SetGreenMinThreshold;
     { Seuil minimum pour le canal bleu }
     property BlueMinThreshold : Byte read FBlueMinThreshold write SetBlueMinThreshold;
     { Seuil maximum pour le canal rouge }
     property RedMaxThreshold : Byte read FRedMaxThreshold write SetRedMaxThreshold;
     { Seuil maximum pour le canal vert }
     property GreenMaxThreshold : Byte read FGreenMaxThreshold write SetGreenMaxThreshold;
     { Seuil maximum pour le canal bleu }
     property BlueMaxThreshold : Byte read FBlueMaxThreshold write SetBlueMaxThreshold;
     { Seuil minimum pour tous les canaux }
     property MinThreshold : byte read FMinThreshold write SetMinThreshold;
     { Seuil maximum pour tous les canaux }
     property MaxThresHold : Byte read FMaxThresHold write SetMaxThresHold;
     { Drapeau d'interpolation pour le mode multi/dual }
     property Interpolate : Boolean read FInterpolate write SetInterpolate;
     { Couleur minimum de l'interval à seuiller }
     property MinColor : TBZColor read FMinColor write SetMinColor;
     { Couleur maximum de l'interval à seuiller }
     property MaxColor : TBZColor read FMaxColor write SetMaxColor;
     { Drapeau pour retourner le resultat en niveau de gris }
     property AsGray : Boolean read FAsGray write SetAsGray;
   End;

   { Méthode de calcul du seuillage automatique }
   TBZAutomaticThresholdMode = (atmTriangle, atmMinimum, atmMean, atmPercentile,
                                atmOtsu, atmYen, atmHang, atmShanbhag,
                                atmMoments,  atmMinError, atmMinimumCrossEntropy, atmMaxEntropy, atmRenyiEntropy,
                                atmIsoData, atmIntermodes);

   { Classe pour l'application d'un filtre de seuillage automatique }
   TBZBitmapThresholdAutomaticFilter = Class(TBZBitmapThresholdBaseFilter)
   private
     FAutomaticThresholdMode : TBZAutomaticThresholdMode;
   protected

     procedure GetOtsuThreshold(var MinThres, MaxThres : Byte);

     { TODO 2 -oBZBitmap -cFiltre : Ajouter d'autres methodes de seuillage automatique
       atmTriangle, atmMinimum, atmMean, atmPercentile,
       atmYen, atmHang, atmShanbhag,
       atmMoments,  atmMinError,
       atmMinimumCrossEntropy, atmMaxEntropy,  atmRenyiEntropy,
       atmIsoData, atmIntermodes }

     procedure DoInitializeScanner; Override;
   public
     Constructor Create(Const AOwner: TBZBaseBitmap; Const  DirectWrite :Boolean = False); Override;
     { Méthode de calcul du seuillage automatique }
     property AutomaticThresholdMode : TBZAutomaticThresholdMode read FAutomaticThresholdMode write FAutomaticThresholdMode;
   end;

  { TODO 2 -oBZBitmap -cFiltre :    Ajouter d'autres methodes de seuillage

     TBZBitmapThresholdLocalFilter = Class(TBZBitmapNonLinearFilter)
     TBZBitmapThresholdHysteris
     TBZBitmapThresholdNiblack
     TBZBitmapThresholdSauvola
     TBZBitmapThresholdBernsen
     TBZBitmapThresholdSavakis
     TBZBitmapThresholdYanowitzBruckstein
     TBZBitmapThresholdAdaptativeFilter
  }


implementation


{%region ====[ TBZBitmapThresoldBaseFilter ]==========================================}

Constructor TBZBitmapThresholdBaseFilter.Create(Const AOwner: TBZBaseBitmap; Const DirectWrite : Boolean);
begin
  inherited Create(AOwner, True);
  FMinColor := clrBlack;
  FMaxColor := clrWhite;
  FMinThreshold := 64;
  FMaxThreshold := 192;
  FRedMinThreshold := 64;
  FGreenMinThreshold := 64;
  FBlueMinThreshold := 64;
  FRedMaxThreshold := 192;
  FGreenMaxThreshold := 192;
  FBlueMaxThreshold := 192;
  FMulti := False;

  FInterpolate := False;
  FAsGray := True;
  //FAdaptative := False;
  //FAdaptativeMode := atmAverage;
  FScannerDescription := 'Seuillage';
end;

procedure TBZBitmapThresholdBaseFilter.DoInitializeScanner;
begin
  inherited DoInitializeScanner;
  //if FAdaptative then
  //begin
  //  FScannerDescription := FScannerDescription + ' adaptatif';
  //  Case FAdaptativeMode of
  //    atmAverage : FScannerDescription := FScannerDescription + ' : Moyen';
  //    atmMinMax : FScannerDescription := FScannerDescription + ' : MinMax';
  //    atmGaussian : FScannerDescription := FScannerDescription + ' : Gaussien';
  //  end;
  //end
  //else
  //begin
  //
    FPreserveLuminosity := False;

    if FMulti then FScannerDescription := FScannerDescription + ' Multi';
    if FInterpolate then FScannerDescription := FScannerDescription + ' Interpolé';

end;

//procedure TBZBitmapThresholdBaseFilter.SetAdaptative(const AValue : Boolean);
//begin
//  if FAdaptative = AValue then Exit;
//  FAdaptative := AValue;
//end;
//
//procedure TBZBitmapThresholdBaseFilter.SetAdaptativeMode(const AValue : TBZFilterAdaptativeThresOldMode);
//begin
//  if FAdaptativeMode = AValue then Exit;
//  FAdaptativeMode := AValue;
//end;

procedure TBZBitmapThresholdBaseFilter.SetInterpolate(const AValue : Boolean);
begin
  if FInterpolate = AValue then Exit;
  FInterpolate := AValue;
end;

procedure TBZBitmapThresholdBaseFilter.SetMulti(const AValue : Boolean);
begin
  if FMulti = AValue then Exit;
  FMulti := AValue;
end;

procedure TBZBitmapThresholdBaseFilter.SetMaxThresHold(const AValue : Byte);
begin
  if FMaxThresHold = AValue then Exit;
  FMaxThresHold := AValue;
end;

procedure TBZBitmapThresholdBaseFilter.SetMinThreshold(const AValue : byte);
begin
  if FMinThreshold = AValue then Exit;
  FMinThreshold := AValue;
end;

procedure TBZBitmapThresholdBaseFilter.SetMinColor(const AValue : TBZColor);
begin
  if FMinColor = AValue then Exit;
  FMinColor := AValue;
end;

procedure TBZBitmapThresholdBaseFilter.SetMaxColor(const AValue : TBZColor);
begin
  if FMaxColor = AValue then Exit;
  FMaxColor := AValue;
end;

procedure TBZBitmapThresholdBaseFilter.SetRedMinThreshold(const AValue : Byte);
begin
  if FRedMinThreshold = AValue then Exit;
  FRedMinThreshold := AValue;
end;

procedure TBZBitmapThresholdBaseFilter.SetGreenMinThreshold(const AValue : Byte);
begin
  if FGreenMinThreshold = AValue then Exit;
  FGreenMinThreshold := AValue;
end;

procedure TBZBitmapThresholdBaseFilter.SetBlueMinThreshold(const AValue : Byte);
begin
  if FBlueMinThreshold = AValue then Exit;
  FBlueMinThreshold := AValue;
end;

procedure TBZBitmapThresholdBaseFilter.SetRedMaxThreshold(const AValue : Byte);
begin
  if FRedMaxThreshold = AValue then Exit;
  FRedMaxThreshold := AValue;
end;

procedure TBZBitmapThresholdBaseFilter.SetGreenMaxThreshold(const AValue : Byte);
begin
  if FGreenMaxThreshold = AValue then Exit;
  FGreenMaxThreshold := AValue;
end;

procedure TBZBitmapThresholdBaseFilter.SetBlueMaxThreshold(const AValue : Byte);
begin
  if FBlueMaxThreshold = AValue then Exit;
  FBlueMaxThreshold := AValue;
end;

procedure TBZBitmapThresholdBaseFilter.SetAsGray(const AValue : Boolean);
begin
  if FAsGray = AValue then Exit;
  FAsGray := AValue;
end;


procedure TBZBitmapThresholdBaseFilter.ComputeHistogram;
Var
  i : Integer;
  Lum : Byte;
  PixPtr : PBZColor;

begin
  for i := 0 to 255 do
  begin
    FHistogram[i] := 0;
  end;
  PixPtr := OwnerBitmap.GetScanLine(0);
  i := 0;
  While i < OwnerBitmap.MaxSize do
  begin
    Lum := PixPtr^.Luminosity;
    inc(FHistogram[Lum]);
    inc(i);
    inc(PixPtr);
  end;
end;

Function TBZBitmapThresholdBaseFilter.ProcessPixel(Const inColor : TBZColor) : TBZColor;
Var
  Lum,f,a,b : Single;
  outColor : TBZColor;
  inColorF, outColorF : TBZColorVector;
  l : Byte;
begin
  if FInterpolate then
  begin
    if FMulti then
    begin
      inColorF := inColor.AsColorVector;
      outColorF := inColorF;
      // Red
      if FRedMinThreshold = FRedMaxThreshold then
        if FRedMaxThreshold = 255 then FRedMinThreshold := FRedMinThreshold - 1
        else FRedMaxThreshold := FRedMaxThreshold + 1;

      a := FRedMinThreshold * _FloatColorRatio;
      b := FRedMaxthreshOld * _FloatColorRatio;

      f := SmoothStep(a, b, inColorF.Red);
      OutColorF.Red := Lerp(FMinColor.AsColorVector.Red, FMaxColor.AsColorVector.Red, f);

      // Green
      if FGreenMinThreshold = FGreenMaxThreshold then
        if FGreenMaxThreshold = 255 then FGreenMinThreshold := FGreenMinThreshold - 1
        else FGreenMaxThreshold := FGreenMaxThreshold + 1;

      a := FGreenMinThreshold * _FloatColorRatio;
      b := FGreenMaxthreshOld * _FloatColorRatio;

      f := SmoothStep(a, b, inColorF.Green);
      OutColorF.Green := Lerp(FMinColor.AsColorVector.Green, FMaxColor.AsColorVector.Green, f);

      // Blue
      if FBlueMinThreshold = FBlueMaxThreshold then
        if FRedMaxThreshold = 255 then FBlueMinThreshold := FBlueMinThreshold - 1
        else FBlueMaxThreshold := FBlueMaxThreshold + 1;

      a := FBlueMinThreshold * _FloatColorRatio;
      b := FBlueMaxthreshOld * _FloatColorRatio;

      f := SmoothStep(a, b, inColorF.Blue);
      OutColorF.Blue := Lerp(FMinColor.AsColorVector.Blue, FMaxColor.AsColorVector.Blue, f);

      OutColor.Create(outColorF);
      Result:= outColor;
    end
    else
    begin
      if FMinThreshold = FMaxThreshold then
        if FMaxThreshold = 255 then FMinThreshold := FMinThreshold - 1
        else FMaxThreshold := FMaxThreshold + 1;

      a := FMinThreshold * _FloatColorRatio;
      b := FMaxthreshOld * _FloatColorRatio;
      if FAsGray then
      begin
        Lum := FCurrentSrcPixelPtr^.Luminosity * _FloatColorRatio;
        f := SmoothStep(a, b, Lum);
        Result := FMinColor.Mix(FMaxColor, f);
      end
      else
      begin
        inColorF := FCurrentSrcPixelPtr^.AsColorVector;
        outColorF := inColorF;
        f := SmoothStep(a, b, inColorF.Red);
        OutColorF.Red := Lerp(FMinColor.AsColorVector.Red, FMaxColor.AsColorVector.Red, f);
        f := SmoothStep(a, b, inColorF.Green);
        OutColorF.Green := Lerp(FMinColor.AsColorVector.Green, FMaxColor.AsColorVector.Green, f);
        f := SmoothStep(a, b, inColorF.Blue);
        OutColorF.Blue := Lerp(FMinColor.AsColorVector.Blue, FMaxColor.AsColorVector.Blue, f);
        OutColor.Create(outColorF);
        Result := outColor;
      end;
    end;
  end
  else
  begin
    outColor := inColor;
    if FMulti then
    begin
      if FRedMinThresHold = FRedMaxThreshold then
      begin
        if inColor.Red >= FRedMinThreshold then outColor.Red := FMaxColor.Red
        else outColor.Red := FMinColor.Red;
      end
      else
      begin
        if (inColor.Red >= FRedMinThreshold) and (inColor.Red <= FRedMaxThreshold) then outColor.Red := FMaxColor.Red
        else outColor.Red := FMinColor.Red;
      end;

      if FGreenMinThreshold = FGreenMaxThreshold then
      begin
        if inColor.Green <= FGreenMinThreshold then outColor.Green := FMinColor.Green
        else if inColor.Green >= FGreenMaxThreshold then outColor.Green := FMaxColor.Green;
      end
      else
      begin
        if (inColor.Green >= FGreenMinThreshold) and (inColor.Green <= FGreenMaxThreshold) then outColor.Green := FMaxColor.Green
        else outColor.Green := FMinColor.Green;
      end;

      if FBlueMinThreshold =  FBlueMaxThreshold then
      begin
        if inColor.Blue <= FBlueMinThreshold then outColor.Blue := FMinColor.Blue
        else if inColor.Blue >= FBlueMaxThreshold then outColor.Blue := FMaxColor.Blue;
      end
      else
      begin
        if (inColor.Blue >= FBlueMinThreshold) and (inColor.Blue <= FBlueMaxThreshold) then outColor.Blue := FMaxColor.Blue
        else outColor.Blue := FMinColor.Blue;
      end;
    end
    else
    begin
      if FMinThreshold = FMaxThreshold then
      begin
        if FAsGray then
        begin
          l := inColor.Luminosity;
          if l >= FMinThreshold then outColor := FMaxColor
          else outColor := FMinColor;
        end
        else
        begin
          if inColor.Red >= FMinThreshold then outColor.Red := FMaxColor.Red
          else outColor.Red := FMinColor.Red;
          if inColor.Green >= FMinThreshold then outColor.Green := FMaxColor.Green
          else outColor.Green := FMinColor.Green;
          if inColor.Blue >= FMinThreshold then outColor.Blue := FMaxColor.Blue
          else outColor.Blue := FMinColor.Blue;
        end;
      end
      else
      begin
        if FAsGray then
        begin
          l := inColor.Luminosity;
          if (l >= FMinThreshold) and (l <= FMaxThreshold) then outColor := FMaxColor
          else outColor := FMinColor;
        end
        else
        begin
          if (inColor.Red >= FMinThreshold) and (inColor.Red <= FMaxThreshold) then outColor.Red := FMaxColor.Red
          else outColor.Red := FMinColor.Red;
          if (inColor.Green >= FMinThreshold) and (inColor.Green <= FMaxThreshold) then outColor.Green := FMaxColor.Green
          else outColor.Green := FMinColor.Green;
          if (inColor.Blue >= FMinThreshold) and (inColor.Blue <= FMaxThreshold) then outColor.Blue := FMaxColor.Blue
          else outColor.Blue := FMinColor.Blue;
        end;
      end;
    end;
    Result := outColor;
  end;
end;

{%endregion%}

{%region=====[ TBZBitmapThresholdAutomaticFilter ]=====================================}

procedure TBZBitmapThresholdAutomaticFilter.GetOtsuThreshold(var MinThres, MaxThres : Byte);
Var
  p1, p2, p3, Diff : Single;
  k : Integer;
  v : Array[0..255] of Single;

  function FuncOtsuA(ii,jj : Byte) : Single;
  Var
    Sum, i : Integer;
  begin
    Sum := 0;
    for i := ii to jj do
    begin
      Sum := Sum + FHistogram[i];
    end;
    Result := Sum;
  end;

  function FuncOtsuB(ii,jj : Byte) : Single;
  Var
    Sum, i : Integer;
  begin
    Sum := 0;
    for i := ii to jj do
    begin
      Sum := Sum + (i*FHistogram[i]);
    end;
    Result := Sum;
  end;

  procedure FindMinMax(var MinIdx, MaxIdx : Byte);
  Var
    f, MaxV, MinV : Single;
    i, idxMin, idxMax : Byte;
  begin
    MaxV := 0;
    MinV := 255; //1.0 ???
    for i := 0 to 255 do
    begin
      f := v[i];
      if f < MinV then
      begin
        MinV := f;
        idxMin := i;
      end;
      if f > MaxV then
      begin
        MaxV := f;
        idxMax := i;
      end;
    end;
    MinIdx := idxMin;
    MaxIdx := idxMax;
    if idxMin > idxMax then
    begin
      MinIdx := idxMax;
      MaxIdx := idxMin;
    end;
  end;

begin
  MinThres := 0;
  MaxThres := 255;
  For k := 0 to 255 do
  begin
    p1 := FuncOtsuA(0, k);
    p2 := FuncOtsuA(k + 1, 255);
    p3 := p1 * p2;
    if (p3 = 0) then p3 := 1;
    Diff := (FuncOtsuB(0, k) * p2) - (FuncOtsuB(k + 1, 255) * p1);
    v[k] := Diff * Diff / p3;
    //v[k] := Math.Pow((FuncOtsuB(0, k) * p2) - (FuncOtsuB(k + 1, 255) * p1), 2) / p3;
  end;
  FindMinMax(MinThres, MaxThres);
end;

procedure TBZBitmapThresholdAutomaticFilter.DoInitializeScanner;
Var
  thresMin, ThresMax : Byte;
begin
  inherited DoInitializeScanner;
  FScannerDescription := FScannerDescription + ' Otsu';
  ComputeHistogram;
  FMulti := False;
  GetOtsuThreshold(thresMin, thresMax);
  MinThreshold := thresMin;
  MaxThreshold := thresMax;
end;

constructor TBZBitmapThresholdAutomaticFilter.Create(const AOwner : TBZBaseBitmap; const DirectWrite : Boolean);
begin
  inherited Create(AOwner, DirectWrite);
  FAutomaticThresholdMode := atmOtsu;
  FScannerDescription := 'Seuiilage Otsu';
end;

{%endregion%}

end.

