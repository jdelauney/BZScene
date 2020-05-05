(*
  @abstract(Contient une classe pour le rendu de police de caractères bitmap)

  --------------------------------------------------------------------------------

  @created(2018-07-11)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(10/07/2018 : Creation)
  )

  --------------------------------------------------------------------------------

  @bold(Notes) :

  --------------------------------------------------------------------------------

  @bold(Dependances) : BZClasses, BZGraphic, BZBitmap, BZBitmapIO;

  --------------------------------------------------------------------------------

  @bold(Credits :)
   @unorderedList(
     @item ()
     @item(J.Delauney (BeanzMaster))
   )

  --------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  -------------------------------------------------------------------------------- *)
Unit BZBitmapFont;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  BZClasses, BZGraphic, BZBitmap, BZBitmapIO;

type

  { Alignement vertical }
  TBZBitmapFontVerticalAlign = ( vaTop, vaCenter, vaBottom);
  { Alignement horizontal }
  TBZBitmapFontHorizontalAlign = ( haLeft, haCenter, haRight);
  { Gestion d'une police de caractères bitmap }
  TBZBitmapFont = Class(TBZUpdateAbleObject)
  private
    FBitmap : TBZBitmap;
    FAlphabet : String;
    FSpaceOffset : Integer;
    FNbLetterByLine : Integer;
    FNbLine : Integer;
    FCarWidth : Byte;
    FCarHeight : Byte;
    FHorizontalLowerCaseGapSize, FVerticalLowerCaseGapSize : Integer;
    FHorizontalGapSize, FVerticalGapSize : Integer;
    FVerticalAlign : TBZBitmapFontVerticalAlign;
    FHorizontalAlign : TBZBitmapFontHorizontalAlign;

    FDrawMode : TBZBitmapDrawMode;
    FAlphaMode : TBZBitmapAlphaMode;
    FMasterAlpha : Byte;

    function GetBitmap : TBZBitmap;
    procedure SetBitmap(const AValue : TBZBitmap);
    procedure SetCarWidth(const AValue : Byte);
    procedure SetCarHeight(const AValue : Byte);
    procedure SetAlphabet(const AValue : String);

    //FImageList : TBZImageList;
    //FFontIndex : Integer;
  protected
  public
    Constructor Create; override;
    Constructor Create(Const BitmapFont : TBZBitmap;  const aCarWidth, aCarHeight : Byte); overload;
    Constructor Create(Const FileFontName : String;  const aCarWidth, aCarHeight : Byte); overload;
    Destructor Destroy; override;


    procedure LoadFromFile(Const FileFontName : String;  const aCarWidth, aCarHeight : Byte);

    procedure TextOut(Bmp : TBZBitmap; px, py : Integer; Const Text : String);
    procedure TextOutRect(Bmp : TBZBitmap; aRect : TBZRect ; Const Text : String; Const HorizontalAlign : TBZBitmapFontHorizontalAlign = haLeft; Const VerticalAlign : TBZBitmapFontVerticalAlign = vaTop);

    //property ImageList : TBZImageList read FImageList write SetImageList;
    //property FontIndex : Integer read SetIndex write SetIndex;

    property Alphabet : String read FAlphabet write SetAlphabet;
    property SpaceOffset : Integer read FSpaceOffset write FSpaceOffset;
    property CarWidth : Byte read FCarWidth write SetCarWidth;
    property CarHeight : Byte read FCarHeight write SetCarHeight;
    property Bitmap : TBZBitmap read GetBitmap write SetBitmap;
    property DrawMode : TBZBitmapDrawMode read FDrawMode write FDrawMode;
    property AlphaMode : TBZBitmapAlphaMode read FAlphaMode write FAlphaMode;
    property MasterAlpha : Byte read FMasterAlpha write FMasterAlpha;
    property HorizontalLowerCaseGapSize: Integer read FHorizontalLowerCaseGapSize write FHorizontalLowerCaseGapSize;
    property VerticalLowerCaseGapSize : Integer read FVerticalLowerCaseGapSize write FVerticalLowerCaseGapSize;
    property HorizontalGapSize : Integer read FHorizontalGapSize write FHorizontalGapSize;
    property VerticalGapSize : Integer read FVerticalGapSize write FVerticalGapSize;
  end;

implementation

Uses
  BZLogger, BZMath;

{%region====[ TBZBitmapFont ]=======================================================}

constructor TBZBitmapFont.Create;
begin
  inherited Create;
  FBitmap := TBZBitmap.Create;
  FAlphabet := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789<>=()-''!_+\/{}^&%.=$#?*';
  FDrawMode := dmSet;
  FAlphaMode := amAlphaBlend;
  FMasterAlpha := 255;
end;

constructor TBZBitmapFont.Create(const BitmapFont : TBZBitmap; const aCarWidth, aCarHeight : Byte);
begin
  Create;
  FBitmap.Assign(BitmapFont);
  FCarWidth := aCarWidth;
  FCarHeight := aCarHeight;
  FNbLetterByLine := (FBitmap.Width div FCarWidth);
  FNBLine := Round((Length(FAlphabet) / FNbLetterByLine) + 0.5);
end;

constructor TBZBitmapFont.Create(const FileFontName : String; const aCarWidth, aCarHeight : Byte);
begin
  Create;
  FBitmap.LoadFromFile(FileFontName);
  FCarWidth := aCarWidth;
  FCarHeight := aCarHeight;
  FNbLetterByLine := (FBitmap.Width div FCarWidth);
  FNBLine := Round((Length(FAlphabet) / FNbLetterByLine) + 0.5);
end;

destructor TBZBitmapFont.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TBZBitmapFont.LoadFromFile(const FileFontName : String; const aCarWidth, aCarHeight : Byte);
begin
  FBitmap.LoadFromFile(FileFontName);
  FCarWidth := aCarWidth;
  FCarHeight := aCarHeight;
  FNbLetterByLine := (FBitmap.Width div FCarWidth);
  FNBLine := Round((Length(FAlphabet) / FNbLetterByLine) + 0.5);
end;

function TBZBitmapFont.GetBitmap : TBZBitmap;
begin
  Result := FBitmap;
end;

procedure TBZBitmapFont.SetBitmap(const AValue : TBZBitmap);
begin
  FBitmap := aValue;
  FNbLetterByLine := FBitmap.Width div (FCarWidth+1);
  FNBLine := FBitmap.Height div (FCarHeight+1);
end;

procedure TBZBitmapFont.SetCarHeight(const AValue : Byte);
begin
  if FCarHeight = AValue then Exit;
  FCarHeight := AValue;
  FNBLine := Round((Length(FAlphabet) / FNbLetterByLine) + 0.5);
end;

procedure TBZBitmapFont.SetAlphabet(const AValue : String);
begin
  if FAlphabet = AValue then Exit;
  FAlphabet := AValue;
  FNBLine := Round((Length(FAlphabet) / FNbLetterByLine) + 0.5);
end;

procedure TBZBitmapFont.SetCarWidth(const AValue : Byte);
begin
  if FCarWidth = AValue then Exit;
  FCarWidth := AValue;
  FNbLetterByLine := (FBitmap.Width div FCarWidth);
  FNBLine := Round((Length(FAlphabet) / FNbLetterByLine) + 0.5);
end;

procedure TBZBitmapFont.TextOut(Bmp : TBZBitmap; px, py : Integer; const Text : String);
Var
  sx, sy, dx, dy, i, Letter, lp : Integer;
  c : char;
begin
  //GlobalLogger.LogNotice('Bitmap Font TextOut');
  //GlobalLogger.LogNotice('Length = '+length(Text).ToString);
  //GlobalLogger.LogNotice('Alphabet = '+length(FAlphabet).ToString);
  //GlobalLogger.LogNotice('Lines = '+FNBLine.ToString);
  //GlobalLogger.LogNotice('Char by Line = '+FNBLetterByLine.ToString);

  dx := px;

  for i := 1 to length(Text) do
  begin
    dy := py;
    //GlobalLogger.LogNotice('---------------------------------------------------------------');
    //GlobalLogger.LogNotice('Current Char ['+i.ToString+ '] = '+Text[i]);
    c := Text[i];
    if (c <> ' ') then
    begin
      Letter := AnsiPos(c, FAlphabet) ;
      lp := ((Letter-1) div (FNBLetterByLine));
      sy := FCarHeight * lp;
      if ((Letter-1) > FNBLetterByLine) then
        sx := (((Letter-1) mod FNBLetterByLine) * FCarWidth) // - ((lp * FNBLetterByLine)+1))
      else
        sx := ((Letter-1) * FCarWidth);

      //GlobalLogger.LogNotice('Alphabet CharPos = '+Letter.ToString);
      //GlobalLogger.LogNotice('Alphabet LinePos = '+lp.ToString);
      //GlobalLogger.LogNotice('Font CharPos Y = '+sy.ToString);
      //GlobalLogger.LogNotice('Font CharPos X = '+sx.ToString);

      if (c in ['a'..'z']) then
      begin
        if (i>1) then
        begin
          dx := dx + (FCarWidth + FSpaceOffset) + FHorizontalLowerCaseGapSize;
          dy := dy + FVerticalLowerCaseGapSize;
        end;
      end
      else
      begin
        if (i>1) then
        begin
          dx := dx + (FCarWidth + FSpaceOffset) + FHorizontalGapSize;
          dy := dy + FVerticalGapSize;
        end;
      end;
      //GlobalLogger.LogNotice('Draw CharPos Y = '+dy.ToString);
      //GlobalLogger.LogNotice('Draw CharPos X = '+dx.ToString);
      bmp.PutImage(FBitmap,sx,sy,FCarWidth, FCarHeight,dx,dy, FDrawMode, FAlphaMode, FMasterAlpha);
    end
    else  dx := dx + (FCarWidth + FSpaceOffset);
  end;
end;

procedure TBZBitmapFont.TextOutRect(Bmp : TBZBitmap; aRect : TBZRect; const Text : String; const HorizontalAlign : TBZBitmapFontHorizontalAlign; const VerticalAlign : TBZBitmapFontVerticalAlign);
begin

end;

{%endregion%}

end.

