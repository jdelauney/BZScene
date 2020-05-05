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
unit BZBitmapMorphologicalFilters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  BZClasses, BZMath, BZVectorMath, BZVectorMathEx, BZColors, BZGraphic, BZBitmapFilterClasses;

type
  { TBZBitmapMorphologicalFilter }
  TBZBitmapMorphologicalFilter = Class(TBZCustomBitmapFilterNonLinear)
  private
    FOperator : TBZMorphologicalOperator;
    FResetValue : Byte;
  protected
    procedure DoInitializeScanner; override;
    Function GetPixelColor : TBZColor; override;
  public
    property OperatorType : TBZMorphologicalOperator read FOperator write FOperator;
  end;

implementation

uses BZLogger;
{ TBZBitmapMorphologicalFilter }

procedure TBZBitmapMorphologicalFilter.DoInitializeScanner;
begin
  inherited DoInitializeScanner;
  if FOperator = moErode then
  begin
    FResetValue := 255;
    FScannerDescription := 'Erosion';
  end
  else
  begin
    FResetValue := 0;
    FScannerDescription := 'Dilatation';
  end;
  GetPixelSampleMethod := psmCustom;
end;

function TBZBitmapMorphologicalFilter.GetPixelColor : TBZColor;
Var
  i, j : Integer;
  inColor : TBZColor;
  RedValue, BlueValue, GreenValue : Byte;
begin
  RedValue := FResetValue;
  GreenValue := FResetValue;
  BlueValue := FResetValue;
  //GlobalLogger.LogNotice('FilterRadius = ' + FFilterRadius.ToString);
  for j := -FFilterRadius to FFilterRadius do
  begin
    for i := -FFilterRadius to FFilterRadius do
    begin
      inColor := OwnerBitmap.GetPixel(FCurrentX + i, FCurrentY + j,FEdgeAction);
      if FOperator = moDilate then
      begin
        if (inColor.Red > RedValue) then RedValue := inColor.Red;
        if (inColor.Green > GreenValue) then GreenValue := inColor.Green;
        if (inColor.Blue > BlueValue) then BlueValue := inColor.Blue;
      end
      else
      begin
        if (inColor.Red < RedValue) then RedValue := inColor.Red;
        if (inColor.Green < GreenValue) then GreenValue := inColor.Green;
        if (inColor.Blue < BlueValue) then BlueValue := inColor.Blue;
      end
    end;
  end;
  Result := BZColor(RedValue, GreenValue, BlueValue, inColor.Alpha);
end;

end.

