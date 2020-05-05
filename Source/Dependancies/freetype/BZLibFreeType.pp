{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Basic canvas definitions.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$mode objfpc}{$h+}
unit BZLibFreeType;

{$DEFINE DYNAMIC}
{$WARN 2005 off : Comment level $1 found}

interface

uses
  Classes, SysUtils, contnrs,
  {$IFDEF DYNAMIC}BZLibFreeTypehDyn{$ELSE}BZLibFreeTypeh{$ENDIF},
  BZLibParseTTF;

{ TODO : take resolution in account to find the size }
{ TODO : speed optimization: search glyphs with a hash-function/tree/binary search/... }
{ TODO : memory optimization: TBZStringBitMaps keeps for each differnet character
         only 1 bitmap }
{ TODO : load other files depending on the extention }
{ possible TODO : different sizes/resolutions for x and y }
{ possible TODO : TBZFontManager can fill a list of all the fonts he can find
              fontfiles and faces available in a fontfile }

// determine if file comparison need to be case sensitive or not
{$ifdef WIN32}
  {$undef CaseSense}
{$else}
  {$define CaseSense}
{$endif}


const
  sErrErrorsInCleanup : string = '%d errors detected while freeing a Font Manager object';
  sErrFontFileNotFound : string = 'Font file "%s" not found';
  sErrFreeType : string = 'Error %d while %s';
  sInitializing : string = 'initializing font engine';
  sDestroying : string = 'destroying font engine';
  sErrErrorInCleanup : string = 'freeing Font Manager object';
  sErrSetPixelSize : string = 'setting pixel size %d (resolution %d)';
  sErrSetCharSize : string = 'setting char size %d (resolution %d)';
  sErrLoadingGlyph : string = 'loading glyph';
  sErrKerning : string = 'determining kerning distance';
  sErrMakingString1 : string = 'making string bitmaps step 1';
  sErrMakingString2 : string = 'making string bitmaps step 2';
  sErrMakingString3 : string = 'making string bitmaps step 3';
  sErrMakingString4 : string = 'making string bitmaps step 4';
  sErrLoadFont : string = 'loading font %d from file %s';
  sErrInitializing : string = 'initializing FreeType';
  sErrDestroying : string = 'finalizing FreeType';

  DefaultFontExtention : string = '.ttf';

  // Standard location for fonts in the Operating System
  {$ifdef Darwin}
  DefaultSearchPath : string = '/Library/Fonts/';
  {$else}
  DefaultSearchPath : string = '';
  {$endif}

  {$IFDEF MAC}
  DefaultResolution : integer = 72;
  {$ELSE}
  DefaultResolution : integer = 97;
  {$ENDIF}

Type
  EFreeTypeException = class (exception);

  TBZTrueTypeFontStyle = (fsRegular, fsItalic, fsBold, fsCondensed, fsExtraLight, fsLight, fsSemibold, fsMedium, fsBlack, fsFixedWidth);
  TBZTrueTypeFontStyles = set of TBZTrueTypeFontStyle;

  TBZFontBitmapType = (btBlackWhite, bt256Gray);
  TBZFontBitmap = record
    height, width, pitch,
    x,y, advanceX, advanceY : integer;
    data : PByteArray;
  end;
  PBZFontBitmap = ^TBZFontBitmap;


  TBZBaseStringBitMaps = class
  private
    FList : TList;
    FBounds : TRect;
    FMode : TBZFontBitmapType;
    function GetCount : integer;
    function GetBitmap (index:integer) : PBZFontBitmap;
    procedure CalculateGlobals;
  public
    constructor Create (ACount : integer);
    destructor destroy; override;
    procedure GetBoundRect (out aRect : TRect);
    property Mode : TBZFontBitmapType read FMode;
    property Count : integer read GetCount;
    property Bitmaps[index:integer] : PBZFontBitmap read GetBitmap;
  end;

  TBZStringBitMaps = class(TBZBaseStringBitMaps)
  private
    FText : STring;
  public
    property Text : string read FText;
  end;

  TBZUnicodeStringBitMaps = class(TBZBaseStringBitMaps)
  private
    FText : UnicodeString;
  public
    property Text : Unicodestring read FText;
  end;

  TBZFontManager = class;

  PBZMgrGlyph = ^TBZMgrGlyph;
  TBZMgrGlyph = record
    Character : unicodechar;
    GlyphIndex : FT_UInt;
    Glyph : PFT_Glyph;
  end;

  PBZMgrSize = ^TBZMgrSize;
  TBZMgrSize = record
    Resolution, Size : integer;
    Glyphs : TList;
  end;

  TBZMgrFont = class
  private
    Mgr : TBZFontManager;
    Font : PFT_Face;
    FSizes : TList;
    Filename : string;
    LastSize : PBZMgrSize;
    procedure FreeGlyphs;
  public
    constructor Create (aMgr:TBZFontManager; afilename:string; anindex:integer);
    destructor destroy; override;
  end;

  { Forward declaration }
   TBZFontCacheList = class;
   TBZFontCacheItem = class(TObject)
   private
     FFamilyName: String;
     FFileName: String;
     FStyleFlags: TBZTrueTypeFontStyles;
     FFileInfo: TTFFileInfo;
     FOwner: TBZFontCacheList; // reference to FontCacheList that owns this instance
     FPostScriptName: string;
     FHumanFriendlyName: string; // aka FullName
     procedure   DoLoadFileInfo;
     procedure   LoadFileInfo;
     procedure   BuildFontCacheItem;
     procedure   SetStyleIfExists(var AText: string; var AStyleFlags: TBZTrueTypeFontStyles; const AStyleName: String; const AStyle: TBZTrueTypeFontStyle);
     function    GetIsBold: boolean;
     function    GetIsFixedWidth: boolean;
     function    GetIsItalic: boolean;
     function    GetIsRegular: boolean;
     function    GetFamilyName: String;
     function    GetPostScriptName: string;
     function    GetHumanFriendlyName: string;
     function    GetFileInfo: TTFFileInfo;
   public
     constructor Create(const AFilename: String);
     destructor  Destroy; override;
     { Result is in pixels }
     function    TextWidth(const AStr: utf8string; const APointSize: single): single;
     { Result is in pixels }
     function    TextHeight(const AText: utf8string; const APointSize: single; out ADescender: single): single;
     property    FileName: String read FFileName;
     property    FamilyName: String read GetFamilyName;
     property    PostScriptName: string read GetPostScriptName;
     property    HumanFriendlyName: string read GetHumanFriendlyName;
     property    FontData: TTFFileInfo read GetFileInfo;
     { A bitmasked value describing the full font style }
     property    StyleFlags: TBZTrueTypeFontStyles read FStyleFlags;
     { IsXXX properties are convenience properties, internally querying StyleFlags. }
     property    IsFixedWidth: boolean read GetIsFixedWidth;
     property    IsRegular: boolean read GetIsRegular;
     property    IsItalic: boolean read GetIsItalic;
     property    IsBold: boolean read GetIsBold;
   end;


   TBZFontCacheList = class(TObject)
   private
     FBuildFontCacheIgnoresErrors: Boolean;
     FList: TObjectList;
     FSearchPath: TStringList;
     FDPI: integer;
     procedure   SearchForFonts(const AFontPath: String);
     procedure   SetDPI(AValue: integer);
     { Set any / or \ path delimiters to the OS specific delimiter }
     procedure   FixPathDelimiters;
   protected
     function    GetCount: integer; virtual;
     function    GetItem(AIndex: Integer): TBZFontCacheItem; virtual;
     procedure   SetItem(AIndex: Integer; AValue: TBZFontCacheItem); virtual;
   public
     constructor Create;
     destructor  Destroy; override;
     procedure   BuildFontCache;
     function    Add(const AObject: TBZFontCacheItem): integer;
     procedure   AssignFontList(const AStrings: TStrings);
     procedure   Clear;
     procedure   LoadFromFile(const AFilename: string);
     procedure   ReadStandardFonts;
     property    Count: integer read GetCount;
     function    IndexOf(const AObject: TBZFontCacheItem): integer;
     function    Find(const AFontCacheItem: TBZFontCacheItem): integer; overload;
     function    Find(const AFamilyName: string; ABold: boolean; AItalic: boolean): TBZFontCacheItem; overload;
     function    Find(const APostScriptName: string): TBZFontCacheItem; overload;
     { not used: utility function doing a conversion for us. }
     function    PointSizeInPixels(const APointSize: single): single;
     property    Items[AIndex: Integer]: TBZFontCacheItem read GetItem write SetItem; default;
     property    SearchPath: TStringList read FSearchPath;
     property    DPI: integer read FDPI write SetDPI;
     Property    BuildFontCacheIgnoresErrors : Boolean Read FBuildFontCacheIgnoresErrors Write FBuildFontCacheIgnoresErrors;
   end;

  { TBZFontManager }

  TBZFontManager = class
  private
    FTLib : PFT_Library;
    FList : TList;
    FPaths : TStringList;
    FFontCollection : TBZFontCacheList;
    FExtention : string;
    FResolution : integer;
    CurFont : TBZMgrFont;
    CurSize : PBZMgrSize;
    CurRenderMode : FT_Render_Mode;
    UseKerning : boolean;
    function GetSearchPath : string;
    procedure SetSearchPath(AValue : string);
    procedure SetExtention(AValue : string);
    Procedure DoMakeString(Text : Array of cardinal; ABitmaps  : TBZBaseStringBitMaps);
    Procedure DoMakeString(Text : Array of cardinal; angle: real; ABitmaps  : TBZBaseStringBitMaps);
  protected
    function GetFontId (afilename:string; anindex:integer) : integer;
    function CreateFont (afilename:string; anindex:integer) : integer;
    function GetFont(FontID:integer) : TBZMgrFont;
    procedure GetSize(aSize, aResolution : integer);
    function CreateSize(aSize, aResolution : integer) : PBZMgrSize;
    procedure SetPixelSize(aSize, aResolution : integer);
    function GetGlyph(c : cardinal) : PBZMgrGlyph;
    function CreateGlyph(c : cardinal) : PBZMgrGlyph;
    procedure MakeTransformation (angle:real; out Transformation:FT_Matrix);
    procedure InitMakeString (FontID, Size:integer);
    function MakeString(FontId:integer; Text:string; size:integer; angle:real) : TBZStringBitMaps;
    function MakeString(FontId:integer; Text:string; Size:integer) : TBZStringBitMaps;
    function MakeString(FontId:integer; Text:Unicodestring; size:integer; angle:real) : TBZUnicodeStringBitMaps;
    function MakeString(FontId:integer; Text:Unicodestring; Size:integer) : TBZUnicodeStringBitMaps;
    function GetFontCollection : TBZFontCacheList;
  public
    constructor Create;
    destructor destroy; override;
    function SearchFont(afilename: string; doraise: boolean=true): string;
    function RequestFont(afilename:string) : integer;
    function RequestFont(afilename:string; anindex:integer) : integer;
    function GetFreeTypeFont(aFontID:integer) : PFT_Face;
    function GetString(FontId:integer; Text:string; size:integer; angle:real) : TBZStringBitMaps;
    function GetString(FontId:integer; Text:Unicodestring; size:integer; angle:real) : TBZUnicodeStringBitMaps;
    // Black and white
    function GetStringGray(FontId:integer; Text:string; size:integer; angle:real) : TBZStringBitMaps;
    function GetStringGray(FontId:integer; Text:unicodestring; size:integer; angle:real) : TBZUnicodeStringBitMaps;
    // Anti Aliased gray scale
    function GetString(FontId:integer; Text:string; Size:integer) : TBZStringBitMaps;
    function GetString(FontId:integer; Text:Unicodestring; Size:integer) : TBZUnicodeStringBitMaps;
    // Black and white, following the direction of the font (left to right, top to bottom, ...)
    function GetStringGray(FontId:integer; Text: String; Size:integer) : TBZStringBitMaps;
    function GetStringGray(FontId:integer; Text:Unicodestring; Size:integer) : TBZUnicodeStringBitMaps;
    // Anti Aliased gray scale, following the direction of the font (left to right, top to bottom, ...)
    property SearchPath : string read GetSearchPath write SetSearchPath;
    property DefaultExtention : string read FExtention write SetExtention;
    property Resolution : integer read Fresolution write FResolution;
    property Collection : TBZFontCacheList read GetFontCollection write FFontCollection;
  end;



function GetTTFontCacheList: TBZFontCacheList;

implementation
uses
  LazUTF8
  {$IFDEF UNIX}
  ,DOM
  ,XMLRead
  {$ENDIF}
  {$IFDEF Windows}
  ,Windows  // for SHGetFolderPath API call used by gTTFontCache.ReadStandardFonts() method
    {$IFDEF win32}, dos{$ENDIF}
  {$ENDIF};

resourcestring
  rsNoSearchPathDefined = 'No search path was defined';
  rsNoFontFileName = 'The FileName property is empty, so we can''t load font data.';
  rsMissingFontFile = 'The font file <%s> can''t be found.';

var
  uFontCacheList: TBZFontCacheList;

{%region%======[ Internal Functions ]===========================================}

function GetTTFontCacheList: TBZFontCacheList;
begin
 if not Assigned(uFontCacheList) then
 begin
   uFontCacheList := TBZFontCacheList.Create;
   uFontCacheList.ReadStandardFonts;
   //uFontCacheList.BuildFontCache;
 end;
 Result := uFontCacheList;
end;

procedure FTError(Event:string; Err:integer);
begin
  raise EFreeTypeException.CreateFmt (sErrFreeType, [Err,Event]);
end;

Function FTCheck(Res: Integer; Msg:string) : Integer;
begin
  Result:=Res;
  If (Result<>0) then FTError(Msg,Result);
end;

{$ifdef windows}
procedure SetWindowsFontPath;
begin
  DefaultSearchPath := includetrailingbackslash(GetEnvironmentVariableUTF8('windir')) + 'fonts';
end;
{$endif}

{%endregion%}

{procedure WriteFT_Face(CurFont: PFT_Face);
var
  i: Integer;
begin
  writeln(' num_faces=',CurFont^.num_faces);
  writeln(' face_index=',CurFont^.face_index);
  writeln(' face_flags=',CurFont^.face_flags);
  writeln(' style_flags=',CurFont^.style_flags);
  writeln(' num_glyphs=',CurFont^.num_glyphs);
  writeln(' family_name=',CurFont^.family_name<>nil);
  writeln(' style_name=',CurFont^.style_name<>nil);
  {if CurFont^.style_name<>nil then begin
    writeln('   ',CurFont^.style_name^);
  end;}
  writeln(' num_fixed_sizes=',CurFont^.num_fixed_sizes);
  writeln(' available_sizes=',CurFont^.available_sizes<>nil);
  for i:=1 to CurFont^.num_fixed_sizes do begin
    writeln('   ',i,' ',CurFont^.available_sizes^[i-1].width,'x',CurFont^.available_sizes^[i-1].height);
  end;
  writeln(' num_charmaps=',CurFont^.num_charmaps);
  writeln(' charmaps=',CurFont^.charmaps<>nil);
  writeln(' generic.data=',CurFont^.generic.data<>nil);
  writeln(' generic.finalizer=',CurFont^.generic.finalizer<>nil);
  writeln(' bbox.xMin=',CurFont^.bbox.xMin,
    ' bbox.xMax=',CurFont^.bbox.xMax,
    ' bbox.yMin=',CurFont^.bbox.yMin,
    ' bbox.yMax=',CurFont^.bbox.yMax,
    ' units_per_EM=',CurFont^.units_per_EM,
    ' ascender=',CurFont^.ascender,
    ' descender=',CurFont^.descender,
    ' height=',CurFont^.height,
    ' max_advance_width=',CurFont^.max_advance_width,
    ' max_advance_height=',CurFont^.max_advance_height,
    ' underline_position=',CurFont^.underline_position,
    ' underline_thickness=',CurFont^.underline_thickness,
    ' glyph=',CurFont^.glyph<>nil,
    ' size=',CurFont^.size<>nil,
    ' charmap=',CurFont^.charmap<>nil,
    '');
end;}

{%region%=====[ TBZMgrFont ]====================================================}

constructor TBZMgrFont.Create(aMgr:TBZFontManager; afilename:string; anindex:integer);
begin
  inherited create;
  Filename := afilename;
  Mgr := aMgr;
  FSizes := TList.create;
  LastSize := nil;
  Try
    FTCheck(FT_New_Face(aMgr.FTLib, pchar(afilename), anindex, font),format (sErrLoadFont,[anindex,afilename]));
    //WriteFT_Face(font);
  except
    Font:=Nil;
    Raise;
  end;
end;

destructor TBZMgrFont.destroy;
begin
  try
    FreeGlyphs;
  finally
    FSizes.Free;
    inherited Destroy;
  end;
end;

procedure TBZMgrFont.FreeGlyphs;
var r,t : integer;
    S : PBZMgrSize;
    G : PBZMgrGlyph;
begin
  for r := FSizes.count-1 downto 0 do
  begin
    with PBZMgrSize(FSizes[r])^ do
    begin
      for t := Glyphs.count-1 downto 0 do
      begin
        with PBZMgrGlyph(Glyphs[t])^ do
          FT_Done_Glyph (Glyph);
        G := PBZMgrGlyph(Glyphs[t]);
        dispose (G);
      end;
      Glyphs.Free;
    end;
    S := PBZMgrSize(FSizes[r]);
    dispose(S);
    end;
end;

{%endregion%}

{%region%=====[ TBZFontCacheItem ]==============================================}

procedure TBZFontCacheItem.DoLoadFileInfo;
begin
  if not Assigned(FFileInfo) then
    LoadFileInfo;
end;

procedure TBZFontCacheItem.LoadFileInfo;
begin
  if FileExists(FFilename) then
  begin
    FFileInfo := TTFFileInfo.Create;
    FFileInfo.LoadFromFile(FFilename);
    BuildFontCacheItem;
  end
  else
    raise ETTF.CreateFmt(rsMissingFontFile, [FFilename]);
end;

function TBZFontCacheItem.GetIsBold: boolean;
begin
  DoLoadFileInfo;
  Result := fsBold in FStyleFlags;
end;

function TBZFontCacheItem.GetIsFixedWidth: boolean;
begin
  DoLoadFileInfo;
  Result := fsFixedWidth in FStyleFlags;
end;

function TBZFontCacheItem.GetIsItalic: boolean;
begin
  DoLoadFileInfo;
  Result := fsItalic in FStyleFlags;
end;

function TBZFontCacheItem.GetIsRegular: boolean;
begin
  DoLoadFileInfo;
  Result := fsRegular in FStyleFlags;
end;

function TBZFontCacheItem.GetFamilyName: String;
begin
  DoLoadFileInfo;
  Result := FFamilyName;
end;

function TBZFontCacheItem.GetPostScriptName: string;
begin
  DoLoadFileInfo;
  Result := FPostScriptName;
end;

function TBZFontCacheItem.GetHumanFriendlyName: string;
begin
  DoLoadFileInfo;
  Result := FHumanFriendlyName;
end;

function TBZFontCacheItem.GetFileInfo: TTFFileInfo;
begin
  DoLoadFileInfo;
  Result := FFileInfo;
end;

procedure TBZFontCacheItem.BuildFontCacheItem;
var
  s: string;
begin
  s := FFileInfo.PostScriptName;
  FPostScriptName := s;
  FFamilyName := FFileInfo.FamilyName;
  if Pos(s, FFamilyName) = 1 then
    Delete(s, 1, Length(FFamilyName));
  FHumanFriendlyName := FFileInfo.HumanFriendlyName;

  FStyleFlags := [fsRegular];

  // extract simple styles first
  if FFileInfo.PostScript.isFixedPitch > 0 then
    FStyleFlags := [fsFixedWidth]; // this should overwrite Regular style

  if FFileInfo.PostScript.ItalicAngle <> 0 then
    FStyleFlags := FStyleFlags + [fsItalic];

  // Now to more complex styles stored in StyleName field. eg: 'Condensed Medium'
  SetStyleIfExists(s, FStyleFlags, 'Bold', fsBold);
  SetStyleIfExists(s, FStyleFlags, 'Condensed', fsCondensed);
  SetStyleIfExists(s, FStyleFlags, 'ExtraLight', fsExtraLight);
  SetStyleIfExists(s, FStyleFlags, 'Light', fsLight);
  SetStyleIfExists(s, FStyleFlags, 'Semibold', fsSemibold);
  SetStyleIfExists(s, FStyleFlags, 'Medium', fsMedium);
  SetStyleIfExists(s, FStyleFlags, 'Black', fsBlack);
  SetStyleIfExists(s, FStyleFlags, 'Oblique', fsItalic);
end;

procedure TBZFontCacheItem.SetStyleIfExists(var AText: string; var AStyleFlags: TBZTrueTypeFontStyles;
  const AStyleName: String; const AStyle: TBZTrueTypeFontStyle);
var
  i: integer;
begin
  i := Pos(AStyleName, AText);
  if i > 0 then
  begin
    AStyleFlags := AStyleFlags + [AStyle];
    Delete(AText, i, Length(AStyleName));
  end;
end;

constructor TBZFontCacheItem.Create(const AFilename: String);
begin
  inherited Create;
  FFileName := AFilename;
  FStyleFlags := [fsRegular];

  if AFileName = '' then
    raise ETTF.Create(rsNoFontFileName);
end;

destructor TBZFontCacheItem.Destroy;
begin
  FFileInfo.Free;
  inherited Destroy;
end;

{ TextWidth returns with width of the text. If APointSize = 0.0, then it returns
  the text width in Font Units. If APointSize > 0 then it returns the text width
  in Pixels. }
function TBZFontCacheItem.TextWidth(const AStr: utf8string; const APointSize: single): single;
{
    From Microsoft's Typography website:
    Converting FUnits (font units) to pixels

    Values in the em square are converted to values in the pixel coordinate system
    by multiplying them by a scale. This scale is:

    pointSize * resolution / ( 72 points per inch * units_per_em )

    where pointSize is the size at which the glyph is to be displayed, and resolution
    is the resolution of the output device. The 72 in the denominator reflects the
    number of points per inch.

    For example, assume that a glyph feature is 550 FUnits in length on a 72 dpi
    screen at 18 point. There are 2048 units per em. The following calculation
    reveals that the feature is 4.83 pixels long.

    550 * 18 * 72 / ( 72 * 2048 ) = 4.83
}
var
  i: integer;
  lWidth: integer;
  lGIndex: integer;
  us: UnicodeString;
  {$IFDEF ttfdebug}
  sl: TStringList;
  s: string;
  {$ENDIF}
begin
  DoLoadFileInfo;
  Result := 0;
  if Length(AStr) = 0 then
    Exit;

  if not Assigned(FFileInfo) then
    Exit;

  {$IFDEF ttfdebug}
    sl := TStringList.Create;
    s := '';
    for i := 0 to 255 do
    begin
      lGIndex := FFileInfo.GetGlyphIndex(i);
      lWidth := FFileInfo.GetAdvanceWidth(lGIndex);
      s := s + ',' + IntToStr(lWidth);
    end;
    sl.Add(s);
    sl.Add('UnitsPerEm = ' + IntToStr(FFileInfo.Head.UnitsPerEm));
    sl.SaveToFile(GetTempDir(True) + FFileInfo.PostScriptName + '.txt');
    sl.Free;
  {$ENDIF}

  lWidth := 0;
  us := UTF8Decode(AStr);
  for i := 1 to Length(us) do
  begin
    lGIndex := FFileInfo.GetGlyphIndex(Word(us[i]));
    lWidth := lWidth + FFileInfo.GetAdvanceWidth(lGIndex);
  end;
  if APointSize = 0.0 then
    Result := lWidth
  else
  begin
    { Converting Font Units to Pixels. The formula is:
      pixels = glyph_units * pointSize * resolution / ( 72 points per inch * THead.UnitsPerEm )  }
    Result := lWidth * APointSize * FOwner.DPI / (72 * FFileInfo.Head.UnitsPerEm);
  end;
end;

function TBZFontCacheItem.TextHeight(const AText: utf8string; const APointSize: single; out ADescender: single): single;
begin
  DoLoadFileInfo;
  { Both lHeight and lDescenderHeight are in pixels }
  Result := FFileInfo.CapHeight * APointSize * GetTTFontCacheList.DPI / (72 * FFileInfo.Head.UnitsPerEm);
  ADescender := Abs(FFileInfo.Descender) * APointSize * GetTTFontCacheList.DPI / (72 * FFileInfo.Head.UnitsPerEm);
end;

{%endregion%}

{%region%=====[ TBZFontCacheList ]==============================================}

procedure TBZFontCacheList.SearchForFonts(const AFontPath: String);
var
  sr: TSearchRec;
  lFont: TBZFontCacheItem;
  s: String;
begin
  if SysUtils.FindFirst(AFontPath + AllFilesMask, faAnyFile, sr) = 0 then
  begin
    repeat
      // check if special files to skip
      if (sr.Name = '.') or (sr.Name = '..') or (sr.Name = '') then
        Continue;
      // We got something, so lets continue
      s := sr.Name;
      if (sr.Attr and faDirectory) <> 0 then // found a directory
        SearchForFonts(IncludeTrailingPathDelimiter(AFontPath + s))
      else
      begin // we have a file
        if (lowercase(ExtractFileExt(s)) = '.ttf') or
           (lowercase(ExtractFileExt(s)) = '.otf') then
        begin
          try
            lFont := TBZFontCacheItem.Create(AFontPath + s);
            Add(lFont);
          except
            if not FBuildFontCacheIgnoresErrors then
              Raise;
          end;
        end;
      end;
    until SysUtils.FindNext(sr) <> 0;
  end;
  SysUtils.FindClose(sr);
end;

procedure TBZFontCacheList.SetDPI(AValue: integer);
begin
  if FDPI = AValue then Exit;
  FDPI := AValue;
end;

procedure TBZFontCacheList.FixPathDelimiters;
var
  i: integer;
begin
  for i := 0 to FSearchPath.Count-1 do
    FSearchPath[i] := SetDirSeparators(FSearchPath[i]);
end;

function TBZFontCacheList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TBZFontCacheList.GetItem(AIndex: Integer): TBZFontCacheItem;
begin
  Result := TBZFontCacheItem(FList.Items[AIndex]);
end;

procedure TBZFontCacheList.SetItem(AIndex: Integer; AValue: TBZFontCacheItem);
begin
  FList.Items[AIndex] := AValue;
end;

constructor TBZFontCacheList.Create;
begin
  inherited Create;
  FList := TObjectList.Create;
  FSearchPath := TStringList.Create;
  FDPI := 96; // The default is the most common dpi used
end;

destructor TBZFontCacheList.Destroy;
begin
  FList.Free;
  FSearchPath.Free;
  inherited Destroy;
end;

procedure TBZFontCacheList.BuildFontCache;
var
  lPath: String;
  i: integer;
begin
  if FSearchPath.Count < 1 then
    raise ETTF.Create(rsNoSearchPathDefined);

  FixPathDelimiters;
  for i := 0 to FSearchPath.Count-1 do
  begin
    lPath := FSearchPath[i];
    if DirectoryExists(lPath) then
      SearchForFonts(IncludeTrailingPathDelimiter(lPath));
  end;
end;

function TBZFontCacheList.Add(const AObject: TBZFontCacheItem): integer;
begin
  Result := FList.Add(AObject);
  AObject.FOwner := self;
end;

procedure TBZFontCacheList.AssignFontList(const AStrings: TStrings);
var
  i: integer;
begin
  if not Assigned(AStrings) then
    Exit;
  AStrings.Clear;
  for i := 0 to FList.Count-1 do
    AStrings.Add(TBZFontCacheItem(FList.Items[i]).PostScriptName);
end;

procedure TBZFontCacheList.Clear;
begin
  FList.Clear;
end;

procedure TBZFontCacheList.LoadFromFile(const AFilename: string);
var
  sl: TStringList;
  i: integer;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFilename);
    for i := 0 to sl.Count-1 do
      Add(TBZFontCacheItem.Create(sl[i]));
  finally
    sl.Free;
  end;
end;

{ This is operating system dependent. Our default implementation only supports
  Linux, FreeBSD, Windows and OSX. On other platforms, no fonts will be loaded,
  until a implementation is created.

  NOTE:
    This is definitely not a perfect solution, especially due to the inconsistent
    implementations and locations of files under various Linux distros. But it's
    the best we can do for now. }
procedure TBZFontCacheList.ReadStandardFonts;

  {$ifdef linux}
    {$define HasFontsConf}
    const
      cFontsConf = '/etc/fonts/fonts.conf';
  {$endif}

  {$ifdef freebsd}
    {$define HasFontsConf}
    const
      cFontsConf = '/usr/local/etc/fonts/fonts.conf';
  {$endif}

  {$ifdef mswindows}
  function GetWinDir: string;
  var
    dir: array [0..MAX_PATH] of Char;
  begin
    GetWindowsDirectory(dir, MAX_PATH);
    Result := StrPas(dir);
  end;
  {$endif}

{$ifdef HasFontsConf}
var
  doc: TXMLDocument;
  lChild: TDOMNode;
  lDir: string;
{$endif}
begin
  {$ifdef HasFontsConf} // Linux & FreeBSD
  ReadXMLFile(doc, cFontsConf);
  try
    lChild := doc.DocumentElement.FirstChild;
    while Assigned(lChild) do
    begin
      if lChild.NodeName = 'dir' then
      begin
        if lChild.FirstChild.NodeValue = '~/.fonts' then
          lDir := ExpandFilename(lChild.FirstChild.NodeValue)
        else
          lDir := lChild.FirstChild.NodeValue;
        SearchPath.Add(lDir);
//        writeln(lDir);
      end;
      lChild := lChild.NextSibling;
    end;
  finally
    doc.Free;
  end;
  {$endif}

  {$ifdef mswindows}
  SearchPath.Add(GetWinDir);
  {$endif}

  {$ifdef darwin} // OSX
  { As per Apple Support page: https://support.apple.com/en-us/HT201722 }
  SearchPath.Add('/System/Library/Fonts/');
  SearchPath.Add('/Library/Fonts/');
  SearchPath.Add(ExpandFilename('~/Library/Fonts/'));
  {$endif}

  BuildFontCache;
end;

function TBZFontCacheList.IndexOf(const AObject: TBZFontCacheItem): integer;
begin
  Result := FList.IndexOf(AObject);
end;

function TBZFontCacheList.Find(const AFontCacheItem: TBZFontCacheItem): integer;
var
  i: integer;
begin
  Result := -1; // nothing found
  for i := 0 to Count-1 do
  begin
    if (Items[i].FamilyName = AFontCacheItem.FamilyName) and
       (Items[i].StyleFlags = AFontCacheItem.StyleFlags) then
    begin
      Result := i;
      exit;
    end;
  end;
end;

function TBZFontCacheList.Find(const AFamilyName: string; ABold: boolean; AItalic: boolean): TBZFontCacheItem;
var
  i: integer;
begin
  for i := 0 to Count-1 do
  begin
    Result := Items[i];
    if (Result.FamilyName = AFamilyName) and (Result.IsItalic = AItalic)
        and (Result.IsBold = ABold)
    then
      exit;
  end;
  Result := nil;
end;

function TBZFontCacheList.Find(const APostScriptName: string): TBZFontCacheItem;
var
  i: integer;
begin
  for i := 0 to Count-1 do
  begin
    Result := Items[i];
    if (Result.PostScriptName = APostScriptName) then
      Exit;
  end;
  Result := nil;
end;

function TBZFontCacheList.PointSizeInPixels(const APointSize: single): single;
begin
  Result := APointSize * DPI / 72;
end;

{%endregion%}

{%region%=====[ TBZFontManager ]================================================}

constructor TBZFontManager.Create;
var r : integer;
begin
  inherited create;
  FList := Tlist.Create;
  FPaths := TStringList.Create;
{$IFDEF DYNAMIC}
  if Pointer(FT_Init_FreeType)=Nil then InitializeFreetype();
{$ENDIF}
  r := FT_Init_FreeType(FTLib);
  if r <> 0  then
  begin
    FTLib := nil;
    FTError(sErrInitializing, r);
  end;
  SearchPath := DefaultSearchPath;
  DefaultExtention := DefaultFontExtention;
  Resolution := DefaultResolution;
  //FFontCollection := TBZFontCacheList.Create;
end;

destructor TBZFontManager.destroy;
  procedure FreeFontObjects;
  var r : integer;
  begin
    for r := FList.Count-1 downto 0 do
      begin
      GetFont(r).Free;
      end;
  end;
  procedure FreeLibrary;
  var r : integer;
  begin
    r := FT_Done_FreeType (FTlib);
    if r <> 0 then
      FTError (sErrDestroying, r);
  end;
begin
  FreeFontObjects;
  FFontCollection.Free;
  FList.Free;
  FPaths.Free;
  try
    if assigned(FTLib) then
      FreeLibrary;
  finally
    inherited Destroy;
  end;
end;

function TBZFontManager.GetSearchPath : string;
var r : integer;
begin
  if FPaths.count > 0 then
  begin
    result := FPaths[0];
    for r := 1 to FPaths.count-1 do
      result := result + ';' + FPaths[r];
  end
  else
    result := '';
end;

procedure TBZFontManager.SetSearchPath(AValue : string);
  procedure AddPath(apath : string);
  begin
    FPaths.Add(IncludeTrailingBackslash(Apath));
  end;
var p : integer;
begin
  while (AValue <> '') do
  begin
    p := pos (';', AValue);
    if p = 0 then
    begin
      AddPath(AValue);
      //FFontCollection.SearchPath.Add(AValue);
      AValue := '';
    end
    else
    begin
      AddPath (copy(AValue,1,p-1));
      delete(AVAlue,1,p);
      //FFontCollection.SearchPath.Add(AValue);
    end;
  end;
end;

procedure TBZFontManager.SetExtention(AValue : string);
begin
  if AValue <> '' then
    if AValue[1] <> '.' then
      FExtention := '.' + AValue
    else
      FExtention := AValue
  else
    AValue := '';
end;

function TBZFontManager.SearchFont(afilename:string; doraise : boolean = true) : string;
// returns full filename of font, taking SearchPath in account
var p,fn : string;
    r : integer;
begin
  Result:='';
  if (pos('.', afilename)=0) and (DefaultFontExtention<>'') then
    fn := afilename + DefaultFontExtention
  else
    fn := aFilename;
  if FileExists(fn) then result := ExpandFilename(fn)
  else
  begin
    p := ExtractFilepath(fn);
    if p = '' then
    begin  // no path given, look in SearchPaths
      r := FPaths.Count;
      repeat
        dec (r);
      until (r < 0) or FileExists(FPaths[r]+fn);
      if r >= 0 then
        Result := FPaths[r]+fn;
    end
  end;
  if (Result='') and doRaise then
    raise EFreeTypeException.CreateFmt(sErrFontFileNotFound, [fn])
end;

function TBZFontManager.GetFontId(afilename:string; anindex:integer) : integer;
begin
  result := FList.count-1;
  while (result >= 0) and
        ( ({$ifdef CaseSense}CompareText{$else}CompareStr{$endif}
              (TBZMgrFont(FList[Result]).Filename, afilename) <> 0) or
          (anIndex <> TBZMgrFont(FList[Result]).font^.face_index)
        ) do
    dec (result);
end;

function TBZFontManager.CreateFont(afilename:string; anindex:integer) : integer;
var f : TBZMgrFont;
begin
//  writeln ('creating font ',afilename,' (',anindex,')');
  f := TBZMgrFont.Create(self, afilename, anindex);
  result := FList.Count;
  Flist.Add(f);
end;

function TBZFontManager.GetFont(FontID:integer) : TBZMgrFont;
begin
  if (FontID >= 0) and (FontID < FList.Count) then
  begin
    result := TBZMgrFont(FList[FontID]);

    if result <> CurFont then  // set last used size of the font as current size
    begin
      CurSize := result.LastSize;
    end;
  end
  else
    Result := nil;
end;

procedure TBZFontManager.GetSize(aSize, aResolution : integer);
var r : integer;
begin
  if not ( assigned(CurSize) and
          (CurSize^.Size = aSize) and (CurSize^.resolution = aResolution)) then
  begin
    r := CurFont.FSizes.count;
    repeat
      dec (r)
    until (r < 0) or ( (PBZMgrSize(CurFont.FSizes[r])^.size = aSize) and
                       (PBZMgrSize(CurFont.FSizes[r])^.resolution = FResolution) );
    if r < 0 then
      CurSize := CreateSize (aSize,aResolution)
    else
      CurSize := PBZMgrSize(CurFont.FSizes[r]);
    SetPixelSize(CurSize^.Size, CurSize^.resolution);
    CurFont.LastSize := CurSize;
  end;
end;

function TBZFontManager.CreateSize(aSize, aResolution : integer) : PBZMgrSize;
begin
  new (result);
  result^.Size := aSize;
  result^.Resolution := aResolution;
  result^.Glyphs := Tlist.Create;
  SetPixelSize (aSize,aResolution);
  CurFont.FSizes.Add (result);
end;

procedure TBZFontManager.SetPixelSize(aSize, aResolution : integer);

  procedure CheckSize;
  var r : integer;
  begin
    with Curfont.Font^ do
    begin
      r := Num_fixed_sizes;
      repeat
        dec (r);
      until (r < 0) or
         ( (available_sizes^[r].height=asize) and
           (available_sizes^[r].width=asize) );
      if r >= 0 then
        raise EFreeTypeException.CreateFmt ('Size %d not available for %s %s',
                  [aSize, style_name, family_name]);
    end;
  end;

var s : longint;
    Err : integer;
begin
  with Curfont, Font^ do
    if (face_flags and FT_Face_Flag_Fixed_Sizes) <> 0 then
    begin
      CheckSize;
      Err := FT_Set_pixel_sizes (Font, aSize, aSize);
      if Err <> 0 then
        FTError (format(sErrSetPixelSize,[aSize,aResolution]), Err);
    end
    else
    begin
      s := aSize shl 6;
      Err := FT_Set_char_size (Font, s, s, aResolution, aResolution);
      if Err <> 0 then FTError (format(sErrSetCharSize,[aSize,aResolution]), Err);
    end;
end;

procedure TBZFontManager.MakeTransformation (angle:real; out Transformation:FT_Matrix);
begin
  with Transformation do
  begin
    xx := round( cos(angle)*$10000);
    xy := round(-sin(angle)*$10000);
    yx := round( sin(angle)*$10000);
    yy := round( cos(angle)*$10000);
  end;
end;

function TBZFontManager.CreateGlyph (c : cardinal) : PBZMgrGlyph;
var e : integer;
begin
  new (result);
  FillByte(Result^,SizeOf(Result),0);
  result^.character := unicodechar(c);
  result^.GlyphIndex := FT_Get_Char_Index (CurFont.font, c);
  //WriteFT_Face(CurFont.Font);
  e := FT_Load_Glyph (CurFont.font, result^.GlyphIndex, FT_Load_Default);
  if e <> 0 then
  begin
    FTError (sErrLoadingGlyph, e);
  end;
  e := FT_Get_Glyph (Curfont.font^.glyph, result^.glyph);
  if e <> 0 then
  begin
    FTError (sErrLoadingGlyph, e);
  end;
  CurSize^.Glyphs.Add (result);
end;

function TBZFontManager.GetGlyph (c : cardinal) : PBZMgrGlyph;
var r : integer;
begin
  With CurSize^ do
  begin
    r := Glyphs.Count;
    repeat
      dec (r)
    until (r < 0) or (PBZMgrGlyph(Glyphs[r])^.character = unicodechar(c));
    if r < 0 then
      result := CreateGlyph (c)
    else
      result := PBZMgrGlyph(Glyphs[r]);
  end;
end;

procedure TBZFontManager.InitMakeString (FontID, Size:integer);
begin
  GetSize (size,Resolution);
  UseKerning := ((Curfont.font^.face_flags and FT_FACE_FLAG_KERNING) <> 0);
end;

function TBZFontManager.MakeString(FontId:integer; Text:string; size:integer; angle:real) : TBZStringBitMaps;
Var
  T : Array of cardinal;
  C,I : Integer;
begin
  CurFont := GetFont(FontID);
  InitMakeString (FontID, Size);
  c := length(text);
  result := TBZStringBitMaps.Create(c);
  result.FText := Text;
  SetLength(T,Length(Text));
  For I:=1 to Length(Text) do T[I-1]:=Ord(Text[i]);
  DoMakeString(T,Angle,Result);
  SetLength(T,0);
  T := nil;
end;

function TBZFontManager.MakeString(FontId:integer; Text:Unicodestring; size:integer; angle:real) : TBZUnicodeStringBitMaps;
Var
  T : Array of cardinal;
  c,I : Integer;
begin
  CurFont := GetFont(FontID);
  InitMakeString (FontID, Size);
  c := length(text);
  result := TBZUnicodeStringBitMaps.Create(c);
  result.FText := Text;
  SetLength(T,C);
  For I:=1 to c do T[I-1]:=Ord(Text[i]);
  DoMakeString(T,Angle,Result);
  SetLength(T,0);
  T := nil;
end;


procedure TBZFontManager.DoMakeString(Text : array of cardinal; angle : real; ABitmaps : TBZBaseStringBitMaps);
var g : PBZMgrGlyph;
    bm : PFT_BitmapGlyph;
    gl : PFT_Glyph;
    prevIndex, prevx, r, rx : integer;
    pre, adv, pos, kern : FT_Vector;
    buf : PByteArray;
    reverse : boolean;
    trans : FT_Matrix;
    FBM : PBZFontBitmap;
begin
  if  (Angle = 0) or   // no angle asked, or can't work with angles (not scalable)
      ((CurFont.Font^.face_flags and FT_FACE_FLAG_SCALABLE)=0) then DoMakeString (Text, ABitmaps)
  else
  begin
    if (CurRenderMode = FT_RENDER_MODE_MONO) then
      ABitmaps.FMode := btBlackWhite
    else
      ABitmaps.FMode := bt256Gray;
    MakeTransformation (angle, trans);
    prevIndex := 0;
    prevx := 0;
    pos.x := 0;
    pos.y := 0;
    pre.x := 0;
    pre.y := 0;
    for r := 0 to Length(Text)-1 do
    begin
      // retrieve loaded glyph
      g := GetGlyph (Text[r]);
      // check kerning
      if UseKerning and (g^.glyphindex <>0) and (PrevIndex <> 0) then
      begin
        prevx := pre.x;
        FTCheck(FT_Get_Kerning (Curfont.Font, prevIndex, g^.GlyphIndex, ft_kerning_default, kern),sErrKerning);
        pre.x := pre.x + kern.x;
      end;
      // render the glyph
      Gl:=Nil;
      FTCheck(FT_Glyph_Copy (g^.glyph, gl),sErrMakingString1);
      //    placing the glyph
      FTCheck(FT_Glyph_Transform (gl, nil, @pre),sErrMakingString2);
      adv := gl^.advance;
      //    rotating the glyph
      FTCheck(FT_Glyph_Transform (gl, @trans, nil),sErrMakingString3);
      //    rendering the glyph
      FTCheck(FT_Glyph_To_Bitmap (gl, CurRenderMode, nil, true),sErrMakingString4);
      // Copy what is needed to record
      bm := PFT_BitmapGlyph(gl);
      FBM:=ABitmaps.Bitmaps[r];
      with FBM^ do
      begin
        with gl^.advance do
        begin
          advanceX := x div 64;
          advanceY := y div 64;
        end;
        with bm^ do
        begin
          height := bitmap.rows;
          width := bitmap.width;
          x := {(pos.x div 64)} + left;  // transformed bitmap has correct x,y
          y := {(pos.y div 64)} - top;   // not transformed has only a relative correction
          buf := PByteArray(bitmap.buffer);
          reverse := (bitmap.pitch < 0);
          if reverse then
          begin
            pitch := -bitmap.pitch;
            getmem (data, pitch*height);
            for rx := height-1 downto 0 do
              move (buf^[rx*pitch], data^[(height-rx-1)*pitch], pitch);
          end
          else
          begin
            pitch := bitmap.pitch;
            rx := pitch*height;
            if RX=0 then Data:=Nil
            else
            begin
              getmem (data, rx);
              move (buf^[0], data^[0], rx);
            end;
          end;
        end;
      end;
      // place position for next glyph
      with gl^.advance do
      begin
        pos.x := pos.x + (x div 1024);
        pos.y := pos.y + (y div 1024);
      end;
      with adv do
        pre.x := pre.x + (x div 1024);
      if prevx > pre.x then pre.x := prevx;
      // finish rendered glyph
      FT_Done_Glyph (gl);
    end;
    ABitmaps.CalculateGlobals;
  end;
end;

function TBZFontManager.MakeString(FontId:integer; Text:string; Size:integer) : TBZStringBitMaps;
Var
  T : Array of Cardinal;
  C,I : Integer;
begin
  CurFont := GetFont(FontID);
  InitMakeString (FontID, Size);
  c := length(text);
  result := TBZStringBitMaps.Create(c);
  result.FText := Text;
  SetLength(T,Length(Text));
  For I:=1 to Length(Text) do T[I-1]:=Ord(Text[i]);
  DoMakeString(T,Result);
end;

function TBZFontManager.MakeString(FontId:integer; Text:Unicodestring; Size:integer) : TBZUnicodeStringBitMaps;
Var
  T : Array of Cardinal;
  C,I : Integer;
begin
  CurFont := GetFont(FontID);
  InitMakeString (FontID, Size);
  c := length(text);
  result := TBZUnicodeStringBitMaps.Create(c);
  result.FText := Text;
  SetLength(T,C);
  For I:=1 to C do T[I-1]:=Ord(Text[i]);
  DoMakeString(T,Result);
end;

function TBZFontManager.GetFontCollection : TBZFontCacheList;
begin
  if Assigned(FFontCollection) then
  begin
    Result := FFontCollection;
  end
  else
  Begin
    Result := GetTTFontCacheList;
  end;
end;

procedure TBZFontManager.DoMakeString(Text : array of cardinal; ABitmaps : TBZBaseStringBitMaps);
var g : PBZMgrGlyph;
    bm : PFT_BitmapGlyph;
    gl : PFT_Glyph;
    e, prevIndex, prevx, r, rx : integer;
    pos, kern : FT_Vector;
    buf : PByteArray;
    reverse : boolean;
begin
  if (CurRenderMode = FT_RENDER_MODE_MONO) then
    ABitmaps.FMode := btBlackWhite
  else
    ABitmaps.FMode := bt256Gray;
  prevIndex := 0;
  prevx := 0;
  pos.x := 0;
  pos.y := 0;
  for r := 0 to length(text)-1 do
  begin
    // retrieve loaded glyph
    g := GetGlyph (Text[r]);
    // check kerning
    if UseKerning and (g^.glyphindex <>0) and (PrevIndex <> 0) then
    begin
      prevx := pos.x;
      e := FT_Get_Kerning (Curfont.Font, prevIndex, g^.GlyphIndex, ft_kerning_default, kern);
      if e <> 0 then
        FTError (sErrKerning, e);
      pos.x := pos.x + kern.x;
    end;
    // render the glyph
    FTCheck(FT_Glyph_Copy (g^.glyph, gl),sErrMakingString1);
    FTCheck(FT_Glyph_To_Bitmap (gl, CurRenderMode, @pos, true),sErrMakingString4);
    // Copy what is needed to record
    bm := PFT_BitmapGlyph(gl);
    with ABitmaps.Bitmaps[r]^ do
    begin
      with gl^.advance do
      begin
        advanceX := x shr 6;
        advanceY := y shr 6;
      end;
      with bm^ do
      begin
        height := bitmap.rows;
        width := bitmap.width;
        x := (pos.x shr 6) + left;   // transformed bitmap has correct x,y
        y := (pos.y shr 6) - top;    // not transformed has only a relative correction
        buf := PByteArray(bitmap.buffer);
        reverse := (bitmap.pitch < 0);
        if reverse then
        begin
          pitch := -bitmap.pitch;
          getmem (data, pitch*height);
          for rx := height-1 downto 0 do
            move(buf^[rx*pitch], data^[(height-rx-1)*pitch], pitch);
        end
        else
        begin
          pitch := bitmap.pitch;
          rx := pitch*height;
          getmem(data, rx);
          move(buf^[0], data^[0], rx);
        end;
      end;
    end;
    // place position for next glyph
    // The previous code in this place used shr 10, which
    // produces wrongly spaced text and looks very ugly
    // for more information see: http://bugs.freepascal.org/view.php?id=17156
    pos.x := pos.x + (gl^.advance.x shr 11);
    // pos.y := pos.y + (gl^.advance.y shr 6); // for angled texts also
    if prevx > pos.x then
      pos.x := prevx;
    // finish rendered glyph
    FT_Done_Glyph(gl);
    end;
  ABitmaps.CalculateGlobals;
end;

function TBZFontManager.GetString(FontId:integer; Text:string; size:integer; angle:real) : TBZStringBitMaps;
// Black and white
begin
  CurRenderMode := FT_RENDER_MODE_MONO;
  result := MakeString(FontID, text, Size, angle);
end;

function TBZFontManager.GetStringGray(FontId:integer; Text:string; size:integer; angle:real) : TBZStringBitMaps;
// Anti Aliased gray scale
begin
  CurRenderMode := FT_RENDER_MODE_NORMAL;
  result := MakeString(FontID, text, Size, angle);
end;

{ Procedures without angle have own implementation to have better speed }

function TBZFontManager.GetString(FontId:integer; Text:string; Size:integer) : TBZStringBitMaps;
// Black and white, following the direction of the font (left to right, top to bottom, ...)
begin
  CurRenderMode := FT_RENDER_MODE_MONO;
  result := MakeString(FontID, text, Size);
end;

function TBZFontManager.GetStringGray(FontId : integer; Text : String; Size : integer) : TBZStringBitMaps;
// Anti Aliased gray scale, following the direction of the font (left to right, top to bottom, ...)
begin
  CurRenderMode := FT_RENDER_MODE_NORMAL;
  result := MakeString(FontID, text, Size);
end;

function TBZFontManager.GetString(FontId:integer; Text:Unicodestring; size:integer; angle:real) : TBZUnicodeStringBitMaps;
// Black and white
begin
  CurRenderMode := FT_RENDER_MODE_MONO;
  result := MakeString(FontID, text, Size, angle);
end;

function TBZFontManager.GetStringGray(FontId : integer; Text : unicodestring; size : integer; angle : real) : TBZUnicodeStringBitMaps;
// Anti Aliased gray scale
begin
  CurRenderMode := FT_RENDER_MODE_NORMAL;
  result := MakeString(FontID, text, Size, angle);
end;

{ Procedures without angle have own implementation to have better speed }

function TBZFontManager.GetString(FontId:integer; Text:Unicodestring; Size:integer) : TBZUnicodeStringBitMaps;
// Black and white, following the direction of the font (left to right, top to bottom, ...)
begin
  CurRenderMode := FT_RENDER_MODE_MONO;
  result := MakeString (FontID, text, Size);
end;

function TBZFontManager.GetStringGray(FontId:integer; Text:Unicodestring; Size:integer) : TBZUnicodeStringBitMaps;
// Anti Aliased gray scale, following the direction of the font (left to right, top to bottom, ...)
begin
  CurRenderMode := FT_RENDER_MODE_NORMAL;
  result := MakeString (FontID, text, Size);
end;

function TBZFontManager.RequestFont(afilename:string) : integer;
begin
  result := RequestFont (afilename,0);
end;

function TBZFontManager.RequestFont(afilename:string; anindex:integer) : integer;
var s : string;
begin
  if afilename = '' then
    result := -1
  else
    begin
    s := SearchFont(afilename);
    result := GetFontID(s,anindex);
    if result < 0 then
      result := CreateFont(s,anindex);
    end;
end;

function TBZFontManager.GetFreeTypeFont(aFontID:integer) : PFT_Face;
begin
  result := GetFont(aFontID).font;
end;

{%endregion%}

{%region%=====[ TBZStringBitMaps ]==============================================}

function TBZBaseStringBitMaps.GetCount : integer;
begin
  result := FList.Count;
end;

function TBZBaseStringBitMaps.GetBitmap(index:integer) : PBZFontBitmap;
begin
  result := PBZFontBitmap(FList[index]);
end;

constructor TBZBaseStringBitMaps.Create(ACount : integer);
var r : integer;
    bm : PBZFontBitmap;
begin
  inherited create;
  FList := Tlist.Create;
  FList.Capacity := ACount;
  for r := 0 to ACount-1 do
  begin
    new (bm);
    FillChar(BM^,SizeOf(TBZFontBitmap),#0);
    FList.Add (bm);
  end;
end;

destructor TBZBaseStringBitMaps.destroy;
var r : integer;
    bm : PBZFontBitmap;
begin
  for r := 0 to Flist.count-1 do
    begin
    bm := PBZFontBitmap(FList[r]);
    freemem (bm^.data);
    dispose (bm);
    end;
  FList.Free;
  inherited;
end;

(*
Procedure DumpBitmap(BM : PBZFontBitmap);

begin
  Writeln('Bitmap h: ',BM^.height,', w: ',BM^.width,', x:',BM^.x,', y: ',bm^.y);
end;
*)

procedure TBZBaseStringBitMaps.CalculateGlobals;
var
  l,r : integer;
begin
  if count = 0 then Exit;
  l:=0;
  // Find first non-empty bitmap. Bitmaps can be empty for spaces.
  While (l<Count) and (BitMaps[l]^.Width=0) and (BitMaps[l]^.Height=0) do Inc(l);
  if L<Count then
    with BitMaps[0]^ do
    begin
      FBounds.left := x;
      FBounds.top := y + height;
      FBounds.bottom := y;
      FBounds.right := x + width;
    end;
  // Find last non-empty bitmap
  r:=Count-1;
  While (R>l) and (BitMaps[r]^.Width=0) and (BitMaps[r]^.Height=0) do Dec(r);
  if R>L then
    With Bitmaps[R]^ do
      FBounds.right := x + width;
  // check top/bottom of other bitmaps
  for r := 1 to count-1 do
  begin
    with Bitmaps[r]^ do
    begin
      if FBounds.top < y + height then
        FBounds.top := y + height;
      if FBounds.bottom > y then
        FBounds.bottom := y;
    end;
  end;
end;

procedure TBZBaseStringBitMaps.GetBoundRect (out aRect : TRect);
begin
  aRect := FBounds;
end;

{%endregion%}

initialization
   uFontCacheList := nil;

  {$ifdef windows}
    SetWindowsFontPath;
  {$endif}
  {$ifdef unix}
    DefaultSearchPath := '/usr/share/fonts/'
  {$endif}

finalization
  if Assigned(uFontCacheList ) then FreeAndNil(uFontCacheList);
end.
