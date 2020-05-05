(*

  @abstract(Contient des classes de bases conçues à des fins génériques pour gérer et afficher des textes
  en UTF-8 et WideString suivant la police de de craractères choisie.)

  -------------------------------------------------------------------------------------------------------------

  @created(2016-11-16)
  @author(J.Delauney (BeanzMaster))
  Historique :
  @unorderedList(
    @item(06/06/2019 : Creation  )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) :

  -------------------------------------------------------------------------------------------------------------

  @bold(Dependances) : BZSystem,  BZClasses, BZGraphic, BZLibFreeType, BZLibFreeTypehDyn

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)
   @unorderedList(
     @item (Une partie des objets sont basés sur les sources de la FCL (FCL-Image)
     @item(J.Delauney (BeanzMaster))
   )

   -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZFontClasses;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================

interface

uses
  {$IFDEF UNIX}CWString,{$ENDIF} Classes, SysUtils,Dialogs,
  BZClasses, BZColors, BZGraphic, BZLibFreeType, BZLibFreeTypehDyn; //, BZLibParseTTF;

Type
  {* Measurements of a font --> Importer de BGRABitmap }
  TFontPixelMetric = record
    {** The values have been computed }
    Defined: boolean;
    {** Position of the baseline, where most letters lie }
    Baseline,
    {** Position of the top of the small letters (x being one of them) }
    xLine,
    {** Position of the top of the UPPERCASE letters }
    CapLine,
    {** Position of the bottom of letters like g and p }
    DescentLine,
    {** Total line height including line spacing defined by the font }
    Lineheight: integer;
  end;

Type
  TBZFontQuality = (fqDraft, fqAntiAliased, fqClearType);

  { Classe de convenance décrivant les paramètres pour une police de caractères. Basé sur les sources de la FCL-Image }
  TBZCustomFont = class(TBZUpdateAbleObject)
  private
    FName : string;
    FOrientation,
    FSize : integer;
    FFlags : word;
    FColor : TBZColor;
    FAlignment : TAlignment;
    FWordBreak: Boolean;
    FQuality : TBZFontQuality;
    FResolution : Longword;
  protected

    procedure SetFlags (index:integer; AValue:boolean); virtual;
    function GetFlags (index:integer) : boolean; virtual;
    //procedure DoCopyProps (From:TFPCanvasHelper); override;
    procedure SetName (AValue:string); virtual;
    procedure SetSize (AValue:integer); virtual;
    procedure SetOrientation (AValue:integer); virtual;
    function GetOrientation : Integer;
    procedure SetQuality(AValue : TBZFontQuality);

    procedure DoCopyProps(From:TBZCustomFont); virtual;

  public
    Constructor Create; override;
    Destructor Destroy; override;

    // Creates a copy of the font with all properties the same, but not allocated
    function CopyFont : TBZCustomFont;
    Procedure Assign(Source: TPersistent); override;

    procedure GetTextSize (text:string; var w,h:integer);
    function GetTextHeight (text:string) : integer;
    function GetTextWidth (text:string) : integer;

    property Name : string read FName write SetName;
    property Size : integer read FSize write SetSize;
    property Bold : boolean index 5 read GetFlags write SetFlags;
    property Italic : boolean index 6 read GetFlags write SetFlags;
    property Underline : boolean index 7 read GetFlags write SetFlags;
    property StrikeThrough : boolean index 8 read GetFlags write SetFlags;
    property Orientation: Integer read GetOrientation write SetOrientation default 0;
    property Color : TBZColor read FColor Write FColor;
    Property WordBreak: Boolean read FWordBreak write FWordBreak;
    property Alignment : TAlignment read Falignment Write FAlignment;
    property Quality : TBZFontQuality read FQuality write SetQuality;
    property Resolution : LongWord read FResolution write Fresolution;
  end;
  { Classe de type TBZCustomFont }
  TBZFontClass = class of TBZCustomFont;

  { Classe à hériter pour afficher un texte  }
  TBZCustomFontDrawer = class(TBZCustomFont)
  private

    FIndex, FFontID : integer;
    FFace : PFT_Face;
    FAngle : Single;


  protected
    FLastText : TBZBaseStringBitmaps;

    procedure ClearLastText;

    procedure Internal_DrawText (x,y:integer; text:string); virtual; abstract;
    procedure Internal_DrawText (x,y:integer; text:unicodestring); virtual;

    procedure Internal_GetTextSize(text:string; var w,h:integer); virtual;
    function Internal_GetTextHeight(text:string) : integer; virtual;
    function Internal_GetTextWidth(text:string) : integer; virtual;

    procedure Internal_GetTextSize(text: unicodestring; var w,h:integer); overload; virtual;
    function Internal_GetTextHeight(text: unicodestring) : integer; overload; virtual;
    function Internal_GetTextWidth(text: unicodestring) : integer; overload; virtual;

    procedure GetText(aText:string);
    procedure GetText(aText:unicodestring);
    procedure GetFace;

    procedure SetName (AValue:string); override;
    procedure SetIndex (AValue : integer);
    procedure SetSize (AValue : integer); override;
    function GetFlags (index:integer) : boolean; override;
    procedure SetFlags (index:integer; AValue:boolean); override;
    procedure DoCopyProps(From:TBZCustomFont); override;
  public

    Constructor Create; override;
    Destructor Destroy; override;

    procedure DrawText (x,y:integer; text:string);
    //procedure DrawTextRect (DestRect:TBZRect; text:string);

    procedure GetTextSize (text:string; var w,h:integer);
    function GetTextHeight (text:string) : integer;
    function GetTextWidth (text:string) : integer;

    procedure DrawText (x,y:integer; text:unicodestring);
    procedure GetTextSize (text: unicodestring; var w,h:integer);
    function GetTextHeight (text: unicodestring) : integer;
    function GetTextWidth (text: unicodestring) : integer;


    property Angle : Single read FAngle write FAngle;

  end;

  { Police de caratère vide }
  TBZEmptyFont = class(TBZCustomFont);
  { Classe d"crivant une police de caractères }
  TBZFont = Class(TBZCustomFontDrawer);

Const
  { Nom des fichiers TTF pouvant être utilisé comme police par défaut @A remplacer par nom de famille }
  cDefaultFontList : array[0..8] of string = ('Arial', 'Segoeui', 'DejaVuSans', 'LiberationSans-Regular', 'NotoSans-Regular',
                                              'Roboto-Regular', 'Helvetica', 'Verdana','Tahoma');

  { Index de la police de caractères préférées }
  {$IFDEF WINDOWS}
  cDefaultPreferredFontIndex = 1;
  {$ENDIF}
  {$IFDEF UNIX}
  cDefaultPreferredFontIndex = 3;
  {$ENDIF}

  //'Helvetica', 'Helvetica Neue',
  //'Nimbus Sans L', 'Microsoft Sans Serif', 'FreeSans',
  //'Liberation Sans', 'DejaVu Sans Condensed', 'Tahoma');
var
  { Variable globale pour référencer les polices de caractères disponibles dans le systeme }
  BZFontMgr : TBZFontManager = nil;
  { Nom de la police par défault }
  DefautFontName : String ='';

{ Initialisation du moteur de rendu des polices de caractères }
procedure InitFontEngine;

{ Libèration du moteur de rendu des polices de caractères }
procedure DoneFontEngine;

implementation

uses
  LazFileUtils, Lazutf8,
  {$IFDEF WINDOWS}Win32Extra,{$ENDIF}
  {$IFDEF LINUX}fileutil,{$ENDIF}
  BZSystem;

{%region=====[ TBZCustomFont ]===============================================================================}

constructor TBZCustomFont.Create;
begin
  inherited Create;
end;

Destructor TBZCustomFont.Destroy;
begin
  Inherited Destroy;
end;

procedure TBZCustomFont.SetFlags (index:integer; AValue:boolean);
begin
  if AValue then
    FFlags := FFlags or (1 shl index)
  else
    FFlags := FFlags and not (1 shl index);
end;

function TBZCustomFont.GetFlags (index:integer) : boolean;
begin
  result := (FFlags and (1 shl index)) <> 0;
end;

procedure TBZCustomFont.SetName (AValue:string);
begin
  if AValue = FName then exit;
  FName := AValue;
end;

procedure TBZCustomFont.SetSize (AValue:integer);
begin
  if AValue = FSize then exit;
  FSize := AValue;
end;

procedure TBZCustomFont.SetOrientation (AValue:integer);
begin
  if AValue = FOrientation then exit;
  FOrientation := AValue;
end;

function TBZCustomFont.GetOrientation : Integer;
begin
  Result := FOrientation;
end;

procedure TBZCustomFont.SetQuality(AValue : TBZFontQuality);
begin
  if AValue = FQuality then exit;
  FQuality := AValue;
end;

procedure TBZCustomFont.DoCopyProps(From:TBZCustomFont);
begin
  with from as TBZCustomFont do
  begin
    self.FName := FName;
    self.FSize := FSize;
    self.FColor := FColor;
    self.FFlags := FFlags;
    self.FOrientation := FOrientation;
    self.FWordBreak := FWordBreak;
    self.FAlignment := FAlignment;
  end;
end;

function TBZCustomFont.CopyFont : TBZCustomFont;
begin
  result := TBZCustomFont(self.ClassType.Create);
  result.DoCopyProps(Self);
end;

Procedure TBZCustomFont.Assign(Source: TPersistent);
begin
  if Source is TBZCustomFont then
  begin
    Self.DoCopyProps(TBZCustomFont(Source));
  end;
  inherited Assign(Source);
end;

procedure TBZCustomFont.GetTextSize (text:string; var w,h:integer);
begin
  w:= -1;
  h := -1;
  if inheritsFrom (TBZCustomFontDrawer) then
    TBZCustomFontDrawer(self).Internal_GetTextSize(text,w,h);
end;

function TBZCustomFont.GetTextHeight (text:string) : integer;
begin
  Result :=-1;
  if inheritsFrom (TBZCustomFontDrawer) then
    result := TBZCustomFontDrawer(self).Internal_GetTextHeight(text);
end;

function TBZCustomFont.GetTextWidth (text:string) : integer;
begin
  Result :=-1;
  if inheritsFrom (TBZCustomFontDrawer) then
    result := TBZCustomFontDrawer(self).Internal_GetTextWidth(text);
end;

{%endregion%}

{%region=====[ TBZCustomFontDrawer ]=========================================================================}

constructor TBZCustomFontDrawer.Create;
begin
  inherited Create;
  FFontID := -1;
  FQuality := fqAntialiased;
  FResolution := DefaultResolution;
end;

destructor TBZCustomFontDrawer.Destroy;
begin
  ClearLastText;
  inherited Destroy;
end;

procedure TBZCustomFontDrawer.Internal_GetTextSize(text:string; var w,h:integer);
var r : TRect;
begin
  GetText(text);
  FLastText.GetBoundRect(r);
  with r do
  begin
    w := right - left;
    h := top - bottom;
  end;
end;

function TBZCustomFontDrawer.Internal_GetTextHeight(text:string) : integer;
var r : TRect;
begin
  GetText(text);
  FLastText.GetBoundRect(r);
  with r do
    result := top - bottom;
end;

function TBZCustomFontDrawer.Internal_GetTextWidth(text:string) : integer;
var r : TRect;
begin
  GetText(text);
  FLastText.GetBoundRect(r);
  with r do
    result := right - left;
end;

procedure TBZCustomFontDrawer.Internal_GetTextSize(text:unicodestring; var w,h:integer);
var r : TRect;
begin
  GetText (text);
  FLastText.GetBoundRect(r);
  with r do
  begin
    w := right - left;
    h := top - bottom;
  end;
end;

function TBZCustomFontDrawer.Internal_GetTextHeight(text:unicodestring) : integer;
var r : TRect;
begin
  GetText(text);
  FLastText.GetBoundRect(r);
  with r do
    result := top - bottom;
end;

function TBZCustomFontDrawer.Internal_GetTextWidth(text:unicodestring) : integer;
var r : TRect;
begin
  GetText(text);
  FLastText.GetBoundRect(r);
  with r do
    result := right - left;
end;

procedure TBZCustomFontDrawer.ClearLastText;
begin
  if assigned(FLastText) then
  begin
    FLastText.Free;
    FlastText := nil;
  end;
end;

procedure TBZCustomFontDrawer.GetFace;
begin
  if not assigned(FFace) then
    FFace := BZFontMgr.GetFreeTypeFont(FFontID);
end;

procedure TBZCustomFontDrawer.GetText(aText:string);
var b : boolean;
begin
  if assigned(FLastText) then
  begin
    if FLastText.InheritsFrom(TBZUnicodeStringBitmaps) or  (CompareStr(TBZStringBitMaps(FLastText).Text,aText) <> 0) then
    begin
      FLastText.Free;
      b := true;
    end
    else
    begin
      if Quality = fqAntialiased then
        b := (FLastText.mode <> bt256Gray)
      else
        b := (FLastText.mode <> btBlackWhite);
      if b then FLastText.Free;
    end;
  end
  else
    b := true;

  if b then
  begin
    BZFontMgr.Resolution := FResolution;
    if Quality = fqAntialiased  then
      FLastText := BZFontMgr.GetStringGray(FFontId, aText, Size, Angle)
    else
      FLastText := BZFontMgr.GetString(FFontId, aText, Size, Angle);
  end;
end;

procedure TBZCustomFontDrawer.GetText(aText : unicodestring);
var b : boolean;
begin
  if assigned(FLastText) then
  begin
    if FLastText.InheritsFrom(TBZStringBitmaps) or  (TBZUnicodeStringBitMaps(FLastText).Text<>aText) then
    begin
      FLastText.Free;
      b := true;
    end
    else
    begin
      if Quality = fqAntialiased then b := (FLastText.mode <> bt256Gray)
      else b := (FLastText.mode <> btBlackWhite);
      if b then FLastText.Free;
    end;
  end
  else
    b := true;
  if b then
  begin
    BZFontMgr.Resolution := FResolution;
    if Quality = fqAntialiased  then
      FLastText := BZFontMgr.GetStringGray(FFontId, aText, Size, Angle)
    else
      FLastText := BZFontMgr.GetString(FFontId, aText, Size, Angle);
  end;
end;

procedure TBZCustomFontDrawer.SetName(AValue:string);
begin
  inherited;
  ClearLastText;
  FFontID := BZFontMgr.RequestFont(Name, FIndex);
end;

procedure TBZCustomFontDrawer.SetIndex(AValue : integer);
begin
  FIndex := AValue;
  ClearLastText;
  FFontID := BZFontMgr.RequestFont(Name, FIndex);
end;

procedure TBZCustomFontDrawer.SetSize(AValue : integer);
begin
  ClearLastText;
  inherited;
end;

procedure TBZCustomFontDrawer.SetFlags(index:integer; AValue:boolean);
begin
  if not (index in [5,6]) then   // bold,italic
    inherited SetFlags (index, AValue);
end;

function TBZCustomFontDrawer.GetFlags(index:integer) : boolean;
begin
  if index = 5 then        //bold
    begin
    GetFace;
    result := (FFace^.style_flags and FT_STYLE_FLAG_BOLD) <> 0;
    end
  else if index = 6 then    //italic
    begin
    GetFace;
    result := (FFace^.style_flags and FT_STYLE_FLAG_ITALIC) <> 0;
    end
  else
    result := inherited GetFlags (index);
end;

procedure TBZCustomFontDrawer.Internal_DrawText (x,y:integer; text:unicodestring);
begin
  internal_DrawText(x,y,String(Text));
end;

procedure TBZCustomFontDrawer.DoCopyProps(From:TBZCustomFont);
Var
  TmpFont : TBZCustomFontDrawer;
begin
  inherited DoCopyProps(From);
  if From is TBZCustomFontDrawer then
  begin
    TmpFont := TBZCustomFontDrawer(From);
    Self.FQuality := TmpFont.FQuality;
    Self.FResolution := TmpFont.FResolution;
    Self.FIndex := TmpFont.FIndex;
    Self.Angle := TmpFont.Angle;
  end;
end;

procedure TBZCustomFontDrawer.DrawText (x,y:integer; text:string);
begin
  internal_DrawText(x,y,UTF8Decode(Text));
end;

procedure TBZCustomFontDrawer.GetTextSize (text:string; var w,h:integer);
begin
  Internal_GetTextSize(Text,w,h);
end;

function TBZCustomFontDrawer.GetTextHeight (text:string) : integer;
begin
  Result := Internal_GetTextHeight(Text);
end;

function TBZCustomFontDrawer.GetTextWidth (text:string) : integer;
begin
  Result := Internal_GetTextWidth(Text);
end;

procedure TBZCustomFontDrawer.DrawText (x,y:integer; text:unicodestring);
begin
  internal_DrawText(x,y,UTF8Decode(String(Text)));
end;

procedure TBZCustomFontDrawer.GetTextSize (text: unicodestring; var w,h:integer);
begin
  Internal_GetTextSize(Text,w,h);
end;

function TBZCustomFontDrawer.GetTextHeight (text: unicodestring) : integer;
begin
  Result := Internal_GetTextHeight(Text);
end;

function TBZCustomFontDrawer.GetTextWidth (text: unicodestring) : integer;
begin
  Result := Internal_GetTextWidth(Text);
end;

{%endregion%}

procedure InitFontEngine;
begin
  if not assigned (BZFontMgr) then BZFontMgr := TBZFontManager.create;
end;

procedure DoneFontEngine;
begin
  FreeAndNil(BZFontMgr);
end;

Procedure LoadFreeTypeFontsFromSystem;
Var
  {$IFDEF linux}FolderList : TStringList;{$ENDIF}
  FontDirList: TStringList;
  i,j: Integer;
  s: String;

Begin
  //MyFFC := TBZCustomBitmapFontDrawerCollection.Create;
  FontDirList := TStringList.Create;
  {$IFDEF WINDOWS}
  s := SHGetFolderPathUTF8(20); // CSIDL_FONTS = 20
  If s <> '' Then FontDirList.Add(s);
  {$ENDIF}
  {$IFDEF linux}
  FontDirList.Add('/usr/share/cups/fonts/');
  FontDirList.Add('/usr/local/lib/X11/fonts/');
  Try
    FolderList := TStringList.Create;
    FolderList := FindAllDirectories('/usr/share/fonts/',true);
    for i:=0 to FolderList.Count-1 do
    begin
      FontDirList.Add(FolderList.Strings[i]);
    end;
  finally
    FolderList.Free;
  End;
  FontDirList.Add(GetUserDir + '.fonts/');
  {$ENDIF}

  For i := 0 To FontDirList.Count - 1 Do
  begin
    //if i>0 then
      //BZFontMgr.SearchPath := BZFontMgr.SearchPath + ';' +FontDirList.Strings[I]
    //else
    BZFontMgr.SearchPath := FontDirList.Strings[I];
   // BZFontMgr.Collection.SearchPath.Add(FontDirList.Strings[I]);
  end;
 // BZFontMgr.Collection.BuildFontCache;
  FontDirList.Free;

  DefautFontName := '';
  j:=-1;
  if BZFontMgr.SearchFont(cDefaultFontList[cDefaultPreferredFontIndex] ,True)='' then
  begin
    for i:=0 to high(cDefaultFontList) do
    begin
      if BZFontMgr.SearchFont(cDefaultFontList[i] ,false)<>'' then
      begin
        j := i;
        break;
      end;
    end;
  end
  else j := cDefaultPreferredFontIndex;

  if j>=0 then DefautFontName := cDefaultFontList[j] else Raise Exception.Create('Impossible de trouver une police de caractères par défaut');
End;


Initialization
  // On enregistre nos classes pour la persitence des objets
  RegisterClasses([TBZCustomFont, TBZCustomFontDrawer]);

  // Chargement de la liste des polices de caractères installées sur le systeme
  InitFontEngine;
  LoadFreeTypeFontsFromSystem;

  // Ajout du dossier de l'application +/fonts
  BZFontMgr.SearchPath := FixPathDelimiter(ExtractFilePath(ParamStrUTF8(0))+'/fonts/');
  //FontCacheList := BZFontMgr.Collection;



Finalization

  DoneFontEngine;
  UnRegisterClasses([TBZCustomFont, TBZCustomFontDrawer]);

end.

