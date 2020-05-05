(*
  @abstract(Contient une classe et un composant d'aide à la recherche de répertoires et/ou de fichiers dans un support de stockage.)

  --------------------------------------------------------------------------------

  @created(14/12/2019)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(Creation : 14/12/2019)
    @item(Mise à jour : )
  )

  --------------------------------------------------------------------------------

  @bold(Notes) : @br
  @bold(Améliorations à faire :)@br
    @unorderedList(
      @item({ TODO 1 -oBZFileFinder -cGenerale  : Ajouter la recherche de contenu. })
      @item({ TODO 1 -oBZFileFinder -cGenerale  : Recherche des fichiers avec des Theads. })
      @item({ TODO 1 -oBZFileFinder -cGenerale  : Ajouter méthodes de tri du résultat (par nom, dates, taille) })
      @item({ TODO 1 -oBZFileFinder -cGenerale  : S'assurer de la compatibilité avec Linux et MacOs. })
    )


  --------------------------------------------------------------------------------

  @bold(Dépendances) : BZClasses, BZSystem, BZUtils

  --------------------------------------------------------------------------------

  @bold(Credits :)@br
    @unorderedList(
      @item(J.Delauney (BeanzMaster))
    )

  --------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  -------------------------------------------------------------------------------- *)
unit BZFileFinder;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils, LazUTF8, LazFileUtils, Masks, DateUtils,
  {$IFDEF WINDOWS}
    Windows,
  {$ENDIF}
  {$IFDEF UNIX}
    BaseUnix,
  {$ENDIF}
  BZClasses, BZSystem, BZUtils, BZControlClasses;

CONST
  { Masques de recherche tous les fichiers }
  cMskSEARCH_ALLFILES = '*.*';
  { Masques de recherche standard pour les fichiers graphique }
  cMskSEARCH_ALLSUPPORTEDGRAPHICS ='*.bmp; *.jpg; *.png; *.tga; *.gif; *.tif';
  { Masques de recherche standard pour les fichiers audio }
  cMskSEARCH_ALLSUPPORTEDSOUND ='*.wav; *.mp3; *.ogg; *.wma';
  { Masques de recherche standard pour les fichiers video }
  cMskSEARCH_ALLSUPPORTEDVIDEO ='*.avi; *.mp4; *.mpg; *.wmv; *.mkv';
  { Masques de recherche standard pour les fichiers web }
  cMskSEARCH_WEBFILE ='*.url; *.htm*';
  { Masques de recherche standard pour les fichiers ressources web }
  cMskSEARCH_WEBRESSOURCESFILE ='*.css;*.js*;*.php;*.asp,*.swf;*.fla';
  { Masques de recherche standard pour les fichiers des langages de développement }
  cMskSEARCH_DEVFILE ='*.cs,*.c;*.cpp;*.h;*.inc;*.pas;*.asm;*.py';
  { Masques de recherche standard pour les fichiers texte }
  cMskSEARCH_ANSIFILE ='*.txt;*.diz; *.me';
  { Masques de recherche standard pour les fichiers de type document}
  cMskSEARCH_DOCUMENTFILE ='*.doc;*.xls;*.pp;*.odt;*.ods';

type
  { TBZSearchFileOptions : Attributs des fichiers à rechercher. Utilisé par TBZFileSearchEngine}
  TBZFileSearchOptions = record
    IncludeSubfolder    : Boolean;
    LookForReadOnlyFile : Boolean;
    LookForHiddenFile   : Boolean;
    LookForSystemFile   : Boolean;
    LookForDirectory    : Boolean;
    LookForArchiveFile  : Boolean;
    LookForAnyFile      : Boolean;
  end;

  { TBZSearchOptionsParams : Paramètres des attibuts de fichier à rechercher. Utilisé par TBZFileFinder }
  TBZSearchOptionsParams = Class(TBZCustomComponentProperty)
  private
    FIncludeSubfolder : Boolean;
    FLookForReadOnlyFile : Boolean;
    FLookForHiddenFile : Boolean;
    FLookForSystemFile : Boolean;
    FLookForDirectory : Boolean;
    FLookForArchiveFile : Boolean;
    FLookForAnyFile : Boolean;
    procedure SetIncludeSubfolder(const AValue : Boolean);
    procedure SetLookForReadOnlyFile(const AValue : Boolean);
    procedure SetLookForHiddenFile(const AValue : Boolean);
    procedure SetLookForSystemFile(const AValue : Boolean);
    procedure SetLookForDirectory(const AValue : Boolean);
    procedure SetLookForArchiveFile(const AValue : Boolean);
    procedure SetLookForAnyFile(const AValue : Boolean);

  public
    constructor Create(AComponent: TComponent); override;
  published
    { Invlure les sous-dossiers dans la recherche }
    property IncludeSubfolder    : Boolean read FIncludeSubfolder write SetIncludeSubfolder;
    { Rechercher les fichiers en lecture seule }
    property LookForReadOnlyFile : Boolean read FLookForReadOnlyFile write SetLookForReadOnlyFile;
    { Rechercher les fichiers cachés }
    property LookForHiddenFile   : Boolean read FLookForHiddenFile write SetLookForHiddenFile;
    { Rechercher les fichiers systeme }
    property LookForSystemFile   : Boolean read FLookForSystemFile write SetLookForSystemFile;
    { Rechercher les répertoires }
    property LookForDirectory    : Boolean read FLookForDirectory write SetLookForDirectory;
    { Rechercher les fichiers archive }
    property LookForArchiveFile  : Boolean read FLookForArchiveFile write SetLookForArchiveFile;
    { Rechercher n'importe quel fichier }
    property LookForAnyFile      : Boolean read FLookForAnyFile write SetLookForAnyFile;
  end;

  { TBZDateFilterAccessKind : Type de date sur lequel appliquer les filtres de date }
  TBZDateFilterAccessKind = (dfakCreatedFiles, dfakModifiedFiles, dfakOpenedFiles, dfakAnyFiles);

  { TBZDateFilterKind : Mode de comparaison pour les filtres sur les dates }
  TBZDateFilterKind = (dfkBetween, dfkBefore, dfkAfter, dfkSame);

  { TBZFileDateFilter : Enregistrement qui décrit le filtre sur les dates. Utilisé par TBZFileSearchEngine }
  TBZFileDateFilter = record
    FilterOnDate     : Boolean;
    FilterAccessKind : TBZDateFilterAccessKind;
    DateFilterKind   : TBZDateFilterKind;
    FirstDate        : TDateTime;
    SecondDate       : TDateTime;
  end;

  { TBZDateFilterParams : Classe de paramètres pour l'édition du filtre sur les date. Utilisé par TBZFileFinder }
  TBZDateFilterParams = Class(TBZCustomComponentProperty)
  private
    FFilter : Boolean;
    FAccessKind : TBZDateFilterAccessKind;
    FFilterKind : TBZDateFilterKind;
    FFirstDate : TDateTime;
    FSecondDate : TDateTime;
    procedure SetFilter(const AValue : Boolean);
    procedure SetAccessKind(const AValue : TBZDateFilterAccessKind);
    procedure SetFilterKind(const AValue : TBZDateFilterKind);
    procedure SetFirstDate(const AValue : TDateTime);
    procedure SetSecondDate(const AValue : TDateTime);

  public
    constructor Create(AComponent: TComponent); override;
  published
    { Filtre actif }
    property Filter : Boolean read FFilter write SetFilter;
    { Type de date sur lequel appliquer le filtre }
    property AccessKind : TBZDateFilterAccessKind read FAccessKind write SetAccessKind;
    { Mode de comparaison }
    property FilterKind : TBZDateFilterKind read FFilterKind write SetFilterKind;
    { Première date }
    property FirstDate : TDateTime read FFirstDate write SetFirstDate;
    {Seconde date }
    property SecondDate : TDateTime read FSecondDate write SetSecondDate;
  end;

  { TBZSizeFilterKind : Mode de comparaison pour le filtre sur la taille des fichiers }
  TBZSizeFilterKind = (sfkSmallerOrEqualTo, sfkBiggerOrEqualTo, sfkInRangeOrEqual);

  { TBZFileSizeFilter : Enregistrement qui définit le filtre sur la taille des fichiers. Utilisé par TBZFileSearchEngine }
  TBZFileSizeFilter = record
    FilterOnSize   : Boolean;
    SizeFilterKind : TBZSizeFilterKind;
    Size           : Int64;
    SizeMax        : Int64
  end;

  { TBZSizeFilterParams : Classe de paramètres pour l'édition du filtre sur la taille des fichiers. Utilisé par TBZFileFinder }
  TBZSizeFilterParams = Class(TBZCustomComponentProperty)
  private
    FFilter : Boolean;
    FFilterKind : TBZSizeFilterKind;
    FSize : Int64;
    FSizeMax : Int64;
    procedure SetFilter(const AValue : Boolean);
    procedure SetFilterKind(const AValue : TBZSizeFilterKind);
    procedure SetSize(const AValue : Int64);
    procedure SetSizeMax(const AValue : Int64);

  public
    constructor Create(AComponent: TComponent); override;
  published
    { Filtre actif }
    property Filter : Boolean read FFilter write SetFilter;
    { Mode de comparaison du filtre }
    property FilterKind : TBZSizeFilterKind read FFilterKind write SetFilterKind;
    { Taille minimum en octet }
    property Size : Int64 read FSize write SetSize;
    { Taille maximum en octet }
    property SizeMax : Int64 read FSizeMax write SetSizeMax;
  end;

  //TBZContentFilterParams = Class(TBZCustomComponentProperty)
  //private
  //public
  //  constructor Create(AComponent: TComponent); override;
  //published
  //  property Filter : Boolean;
  //  property Content : String;
  //  property CaseSensitive : Boolean;
  //end;

  { TBZFileTime : Enregistrement des dates d'un fichier. Utilisé par TBZFileSearchEngine }
  TBZFileTime = packed record
    CreationTime : TDateTime;
    AccessTime   : TDateTime;
    WriteTime    : TDateTime;
  end;

  {TBZFileInformations : Informations sur le fichier }
  TBZFileInformations = packed record
    Name       : String   ;   //< nom du fichier
    Path       : String   ;   //< Chemin du fichier
    Attributes : Integer  ;   //< Attributs du fichier
    Size       : Integer  ;   //< Taille du fichier
    Time       : TBZFileTime; //< Date du dernier acces au fichier
  end;

  { TBZSearchStatistics : Statistiques de la recherche }
  TBZSearchStatistics = packed record
    NbFilesFound  : LongWord;     //< Nombre de fichiers trouvés correspondant aux critères de recherche
    FoundInNbPath : LongWord;     //< Nombre de chemin différents trouvés
    NbPathFound   : LongWord;     //< Nombre de chemin trouvés correspondant aux critères de recherche
  end;

  { Evènement déclencher au démarrage de la recherche }
  TBZOnSearchStart   = procedure (Sender : TObject) of object;
  { Evènement déclencher lorqu'un fichier ou dossier est trouvé }
  TBZOnSearchFileFound   = procedure (Sender : TObject; FileFound : TBZFileInformations) of object;
  // TBZOnSearchFolderFound   = procedure (Sender : TObject; FolderName : String) of object;

  { Evènement déclencher lors du changement du chemein de recherche }
  TBZOnSearchChangeFolder = procedure (Sender : TObject; NewPath : String) of object;
  { Evènement déclencher lorsque la recherche est terminée }
  TBZOnSearchCompleted = procedure (Sender : TObject; Stats : TBZSearchStatistics) of object;
  { Evènement déclencher lorsque lorsqu'un fichier est sur le point d'être accepté comme résultat valide. L'utilisateur peut dire oui ou non }
  TBZOnSearchAcceptFile = function (Sender : TObject; FileFound : TBZFileInformations) : Boolean of object;

  { TBZFileSearchEngine : Moteur de recherche de dossiers et fichiers }
  TBZFileSearchEngine = class
  private

  protected
    function GetFileInformations(Path : String; SearchInfos : TSearchRec) : TBZFileInformations;
    function GetAttributeFilter : Integer;
    function IsAttributesOk(AttrToCheck : Integer) : Boolean;
    function IsDateOk(FileInfos : TBZFileInformations) : Boolean;
    function IsSizeOk(FileInfos : TBZFileInformations; SizeFilter : TBZFileSizeFilter) : Boolean;
    function IsFileMatching(FileName : String) : Boolean;
    function IsAllFileMarkerPresents : Boolean;
    function ExcludeFile(FileInfos : TBZFileInformations; Exclude : TStringList) : Boolean;

  public
    SearchOptions : TBZFileSearchOptions;
    DateOptions   : TBZFileDateFilter;
    SizeOptions   : TBZFileSizeFilter;
    ExcludeMasks : TStringList;
    FileMasks     : TStringList;
    RootPath      : String;

    OnSearchFileFound    : TBZOnSearchFileFound;
    OnSearchChangeFolder : TBZOnSearchChangeFolder;
    OnSearchCompleted    : TBZOnSearchCompleted;
    OnSearchAcceptFile   : TBZOnSearchAcceptFile;
    OnSearchStart        : TBZOnSearchStart;

    constructor Create;
    destructor Destroy; override;

    { Recherche les dossiers et fichiers en fonction des critères de recherche. @br
      Retourne @TRUE en cas de succès.}
    function Search : Boolean;

   end;

   { TBZFileFinder : Composant englobant la classe TBZFileSearchEngine pour rechercher des dossiers et/ou fichiers }
   TBZFileFinder = class(TComponent)
   private
     FSearchEngine   : TBZFileSearchEngine;
     FSearchOptions : TBZSearchOptionsParams;
     FDateFilter : TBZDateFilterParams;
     FSizeFilter : TBZSizeFilterParams;

     FOnSearchFileFound    : TBZOnSearchFileFound;
     FOnSearchAcceptFile   : TBZOnSearchAcceptFile;
     FOnSearchChangeFolder : TBZOnSearchChangeFolder;
     FOnSearchCompleted    : TBZOnSearchCompleted;
     FOnSearchStart        : TBZOnSearchStart;

     FSearchResult : TStringList;

     procedure ConvertSearchOptions;
     procedure ConvertFilterOptions;

     procedure SetRootPath(NewRootPath : String);
     function  GetRootPath : String;

   protected
     FExcludedMasks : TStrings;
     FFileMasks : TStrings;

     procedure SetExcludedMasks(NewExcluded : TStrings);
     function GetExcludedMasks : TStrings;

     procedure SetFileMasks(NewFileNames : TStrings);
     function GetFileMasks : TStrings;

     procedure DoOnSearchFileFound(Sender : TObject; FileFound : TBZFileInformations);
     function  DoOnSearchAcceptFile(Sender : TObject; FileFound : TBZFileInformations) : Boolean;
     procedure DoOnSearchChangeFolder(Sender : TObject; NewPath : String);
     procedure DoOnSearchStart(Sender : TObject);
     procedure DoOnSearchCompleted(Sender : TObject; Stats : TBZSearchStatistics);

   public
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;

     { Recherche les dossiers et fichiers en fonction des critères de recherche. @br
      Retourne @TRUE en cas de succès.}
     function Search : Boolean;

     property SearchResult : TStringList read FSearchResult;
   published
     property SearchOptions : TBZSearchOptionsParams read FSearchOptions write FSearchOptions;
     property ExcludedMasks : TStrings read GetExcludedMasks write SetExcludedMasks;

     property FileMasks : TStrings read GetFileMasks write SetFileMasks;
     property RootPath   : String  read GetRootPath  write SetRootPath ;

     property OnFileFound    : TBZOnSearchFileFound    read FOnSearchFileFound     write FOnSearchFileFound;
     property OnAcceptFile   : TBZOnSearchAcceptFile   read FOnSearchAcceptFile    write FOnSearchAcceptFile;
     property OnChangeFolder : TBZOnSearchChangeFolder read FOnSearchChangeFolder  write FOnSearchChangeFolder;
     property OnStart        : TBZOnSearchStart        read FOnSearchStart         write FOnSearchStart;
     property OnCompleted    : TBZOnSearchCompleted    read FOnSearchCompleted     write FOnSearchCompleted;
   end;

   { Recherche de dossiers }
   procedure SearchFolder(var aFolderList : TStringList; aRootPath : String ; Recurse : Boolean);
   { Recherche de fichiers }
   procedure SearchFile(var aFileList : TStringList; aRootPath, FileMask : String ; Recurse : Boolean);


implementation

Uses BZTypesHelpers;

procedure SearchFolder(var aFolderList : TStringList; aRootPath : String; Recurse : Boolean);
Var
  Finder : TBZFileFinder;
begin
  Finder := TBZFileFinder.Create(nil);
  With Finder do
  begin
    SearchOptions.IncludeSubfolder :=  Recurse;
    SearchOptions.LookForDirectory := True;
    SearchOptions.LookForAnyFile := False;
    SearchOptions.LookForArchiveFile := False;
    SearchOptions.LookForReadOnlyFile := False;
    SearchOptions.LookForHiddenFile := False;
    SearchOptions.LookForSystemFile := False;
    RootPath := aRootPath;
    FileMasks.Text := '*.*';
    Search;
    aFolderList.Clear;
    aFolderList.Assign(SearchResult);
  end;
  FreeAndNil(Finder);
end;

procedure SearchFile(var aFileList : TStringList; aRootPath, FileMask : String; Recurse : Boolean);
Var
  Finder : TBZFileFinder;
begin
  Finder := TBZFileFinder.Create(nil);
  With Finder do
  begin
    SearchOptions.FIncludeSubfolder :=  Recurse;
    SearchOptions.LookForDirectory := False;
    SearchOptions.LookForAnyFile := True;
    SearchOptions.LookForArchiveFile := False;
    SearchOptions.LookForReadOnlyFile := False;
    SearchOptions.LookForHiddenFile := False;
    SearchOptions.LookForSystemFile := False;
    RootPath := aRootPath;
    FileMasks.Text := FileMask;
    Search;
    aFileList.Clear;
    aFileList.Assign(SearchResult);
  end;
  FreeAndNil(Finder);
end;

{%region=====[ TBZSearchOptionsParams ]=============================================================}

constructor TBZSearchOptionsParams.Create(AComponent : TComponent);
begin
  inherited Create(AComponent);
  FIncludeSubfolder := False;
  FLookForReadOnlyFile := True;
  FLookForHiddenFile := True;
  FLookForSystemFile := True;
  FLookForDirectory := True;
  FLookForArchiveFile := True;
  FLookForAnyFile := True;
end;

procedure TBZSearchOptionsParams.SetIncludeSubfolder(const AValue : Boolean);
begin
  if FIncludeSubfolder = AValue then Exit;
  FIncludeSubfolder := AValue;
end;

procedure TBZSearchOptionsParams.SetLookForAnyFile(const AValue : Boolean);
begin
  if FLookForAnyFile = AValue then Exit;
  FLookForAnyFile := AValue;
end;

procedure TBZSearchOptionsParams.SetLookForArchiveFile(const AValue : Boolean);
begin
  if FLookForArchiveFile = AValue then Exit;
  FLookForArchiveFile := AValue;
end;

procedure TBZSearchOptionsParams.SetLookForDirectory(const AValue : Boolean);
begin
  if FLookForDirectory = AValue then Exit;
  FLookForDirectory := AValue;
end;

procedure TBZSearchOptionsParams.SetLookForHiddenFile(const AValue : Boolean);
begin
  if FLookForHiddenFile = AValue then Exit;
  FLookForHiddenFile := AValue;
end;

procedure TBZSearchOptionsParams.SetLookForReadOnlyFile(const AValue : Boolean);
begin
  if FLookForReadOnlyFile = AValue then Exit;
  FLookForReadOnlyFile := AValue;
end;

procedure TBZSearchOptionsParams.SetLookForSystemFile(const AValue : Boolean);
begin
  if FLookForSystemFile = AValue then Exit;
  FLookForSystemFile := AValue;
end;

{%endregion%}

{%region=====[ TBZDateFilterParams ]=============================================================== }

constructor TBZDateFilterParams.Create(AComponent : TComponent);
begin
  inherited Create(AComponent);
  FFilter := False;
  FAccessKind := dfakAnyFiles;
  FFilterKind := dfkBefore;
  FFirstDate := Now;
  FSecondDate := Now;
end;

procedure TBZDateFilterParams.SetAccessKind(const AValue : TBZDateFilterAccessKind);
begin
  if FAccessKind = AValue then Exit;
  FAccessKind := AValue;
end;

procedure TBZDateFilterParams.SetFilter(const AValue : Boolean);
begin
  if FFilter = AValue then Exit;
  FFilter := AValue;
end;

procedure TBZDateFilterParams.SetFilterKind(const AValue : TBZDateFilterKind);
begin
  if FFilterKind = AValue then Exit;
  FFilterKind := AValue;
end;

procedure TBZDateFilterParams.SetFirstDate(const AValue : TDateTime);
begin
  if FFirstDate = AValue then Exit;
  FFirstDate := AValue;
end;

procedure TBZDateFilterParams.SetSecondDate(const AValue : TDateTime);
begin
  if FSecondDate = AValue then Exit;
  FSecondDate := AValue;
end;

{%endregion%}

{%region=====[ TBZSizeFilterParams ]================================================================}

constructor TBZSizeFilterParams.Create(AComponent : TComponent);
begin
  inherited Create(AComponent);
  FFilter := False;
  FFilterKind := sfkSmallerOrEqualTo;
  FSize := 0;
end;

procedure TBZSizeFilterParams.SetFilter(const AValue : Boolean);
begin
  if FFilter = AValue then Exit;
  FFilter := AValue;
end;

procedure TBZSizeFilterParams.SetFilterKind(const AValue : TBZSizeFilterKind);
begin
  if FFilterKind = AValue then Exit;
  FFilterKind := AValue;
end;

procedure TBZSizeFilterParams.SetSize(const AValue : Int64);
begin
  if FSize = AValue then Exit;
  FSize := AValue;
end;

procedure TBZSizeFilterParams.SetSizeMax(const AValue : Int64);
begin
  if FSizeMax = AValue then Exit;
  FSizeMax := AValue;
end;

{%endregion%}

{%region=====[ TBZFileSearchEngine ]================================================================}

constructor TBZFileSearchEngine.Create;
begin
  inherited Create;

  SearchOptions.IncludeSubfolder    := False;
  SearchOptions.LookForReadOnlyFile := True ;
  SearchOptions.LookForHiddenFile   := True ;
  SearchOptions.LookForSystemFile   := True ;
  SearchOptions.LookForDirectory    := True ;
  SearchOptions.LookForArchiveFile  := True ;
  SearchOptions.LookForAnyFile      := True ;

  DateOptions.FilterOnDate     := False       ;
  DateOptions.FilterAccessKind := dfakAnyFiles;
  DateOptions.DateFilterKind   := dfkBefore   ;
  DateOptions.FirstDate        := Date        ;
  DateOptions.SecondDate       := Date        ;

  SizeOptions.FilterOnSize   := False              ;
  SizeOptions.SizeFilterKind := sfkSmallerOrEqualTo;
  SizeOptions.Size           := 0                  ;

  ExcludeMasks := TStringList.Create;

  FileMasks := TStringList.Create;
  FileMasks.Add('*.*');
  RootPath := FixPathDelimiter('.\') ;

  OnSearchFileFound  := nil;
  OnSearchChangeFolder := nil;
  OnSearchCompleted   := nil;
end;

destructor TBZFileSearchEngine.Destroy;
begin
  FreeAndNil(FileMasks);
  FreeAndNil(ExcludeMasks);

  inherited Destroy;
end;

function TBZFileSearchEngine.GetAttributeFilter : Integer;
begin
  Result := 0;
  if SearchOptions.LookForReadOnlyFile = True then Result := Result or faReadOnly;
  if SearchOptions.LookForHiddenFile   = True then Result := Result or faHidden{%H-};
  if SearchOptions.LookForSystemFile   = True then Result := Result or faSysFile{%H-};
  // Linux + faSymLink
  if SearchOptions.LookForDirectory    = True then Result := Result or faDirectory;
  if SearchOptions.LookForArchiveFile  = True then Result := Result or faArchive  ;
end;

function TBZFileSearchEngine.GetFileInformations(Path : String; SearchInfos : TSearchRec) : TBZFileInformations;
{$IFDEF UNIX} //LINUX
Var
 FileInfos : Stat;
{$ENDIF}
begin
  Result.Name       := SearchInfos.Name;
  Result.Path       := Path            ;
  Result.Attributes := SearchInfos.Attr;
  Result.Size       := SearchInfos.Size;
 {$IFDEF WINDOWS}
   Result.Time.CreationTime.FromFileTime(SearchInfos.FindData.ftCreationTime); // := SystemTimeToDateTime(); //FileDateToDateTime(SearchInfos.FindData.ftCreationTime);
   Result.Time.AccessTime.FromFileTime(SearchInfos.FindData.ftLastAccessTime); //FileDateToDateTime(SearchInfos.FindData.ftLastAccessTime);
   Result.Time.WriteTime.FromFileTime(SearchInfos.FindData.ftLastWriteTime); //FileDateToDateTime(SearchInfos.FindData.ftLastWriteTime);
 {$ENDIF}
 {$IFDEF UNIX} //LINUX
   if fpstat(Result.Name,FileInfos)=0 then
   begin
     Result.Time.CreationTime := FileDateToDateTime(FileInfos.ctime);
     Result.Time.AccessTime := FileDateToDateTime(FileInfos.st_atime);
     Result.Time.WriteTime := FileDateToDateTime(FileInfos.st_mtime);
   end
   else
   begin
     Result.Time.CreationTime := now;
     Result.Time.AccessTime := now;
     Result.Time.WriteTime := now;
   end;
 {$ENDIF}
 //{$IFDEF DARWIN}
 //{$ENDIF}
end;

function TBZFileSearchEngine.IsAttributesOk(AttrToCheck : Integer) : Boolean;
var
  AttrFilter : Integer;
begin
  Result := False;

  AttrFilter := GetAttributeFilter;

  if ((AttrToCheck and faReadOnly ) and (AttrFilter and faReadOnly ))<>0 then Result := True;
  if ((AttrToCheck and faHidden{%H-}) and (AttrFilter and faHidden{%H-}))<>0 then Result := True;
  if ((AttrToCheck and faSysFile{%H-}) and (AttrFilter and faSysFile{%H-}))<>0 then Result := True;
  // Linux + faSymLink
  if ((AttrToCheck and faDirectory) and (AttrFilter and faDirectory))<>0 then Result := True;
  if ((AttrToCheck and faArchive  ) and (AttrFilter and faArchive  ))<>0 then Result := True;
end;

function TBZFileSearchEngine.IsDateOk(FileInfos : TBZFileInformations) : Boolean;
var
  CreationTime : TDateTime;
  AccessTime   : TDateTime;
  WriteTime    : TDateTime;
  CreationOk   : Boolean  ;
  AccessOk     : Boolean  ;
  WriteOk      : Boolean  ;
  DateFilterOk : Boolean  ;
  FileInfosRef : TBZFileInformations;
  SysTime      : TSystemTime;

begin
  if DateOptions.FilterOnDate=False then DateFilterOk := True else
  begin
    DateFilterOk := False;
    CreationOk   := False;
    AccessOk     := False;
    WriteOk      := False;
    FileInfosRef := FileInfos;

    DateTimeToSystemTime(DateOptions.FirstDate, SysTime);
    {$IFDEF WINDOWS}
    SysTime.wHour         := 0;
    SysTime.wMinute       := 0;
    SysTime.wSecond       := 0;
    SysTime.wMilliseconds := 0;
    {$ELSE}
    SysTime.Hour         := 0;
    SysTime.Minute       := 0;
    SysTime.Second       := 0;
    SysTime.Millisecond  := 0;
    {$ENDIF}

    DateOptions.FirstDate := SystemTimeToDateTime(SysTime);

    DateTimeToSystemTime(DateOptions.SecondDate, SysTime);
    {$IFDEF WINDOWS}
    SysTime.wHour         := 0;
    SysTime.wMinute       := 0;
    SysTime.wSecond       := 0;
    SysTime.wMilliseconds := 0;
    {$ELSE}
    SysTime.Hour         := 0;
    SysTime.Minute       := 0;
    SysTime.Second       := 0;
    SysTime.Millisecond  := 0;
    {$ENDIF}
    DateOptions.SecondDate := SystemTimeToDateTime(SysTime);

    FileInfosRef.Time.CreationTime := now;
    FileInfosRef.Time.CreationTime := RecodeTime(FileInfosRef.Time.CreationTime,0,0,0,0);
    CreationTime := FileInfosRef.Time.CreationTime;

    FileInfosRef.Time.AccessTime := now;
    FileInfosRef.Time.AccessTime := RecodeTime(FileInfosRef.Time.AccessTime,0,0,0,0);
    AccessTime := FileInfosRef.Time.AccessTime;

    FileInfosRef.Time.WriteTime := now;
    FileInfosRef.Time.WriteTime := RecodeTime(FileInfosRef.Time.WriteTime,0,0,0,0);
    WriteTime := FileInfosRef.Time.WriteTime;

    case DateOptions.DateFilterKind of
      dfkBetween:
      begin
        if (CreationTime>=DateOptions.FirstDate) and (CreationTime<=DateOptions.SecondDate) then CreationOk := True;
        if (AccessTime  >=DateOptions.FirstDate) and (AccessTime  <=DateOptions.SecondDate) then AccessOk   := True;
        if (WriteTime   >=DateOptions.FirstDate) and (WriteTime   <=DateOptions.SecondDate) then WriteOk    := True;
      end;
      dfkBefore:
      begin
        if CreationTime<DateOptions.FirstDate then CreationOk := True;
        if AccessTime  <DateOptions.FirstDate then AccessOk   := True;
        if WriteTime   <DateOptions.FirstDate then WriteOk    := True;
      end;
      dfkAfter  :
      begin
        if CreationTime>DateOptions.FirstDate then CreationOk := True;
        if AccessTime  >DateOptions.FirstDate then AccessOk   := True;
        if WriteTime   >DateOptions.FirstDate then WriteOk    := True;
      end;
      dfkSame:
      begin
        if CreationTime=DateOptions.FirstDate then CreationOk := True;
        if AccessTime  =DateOptions.FirstDate then AccessOk   := True;
        if WriteTime   =DateOptions.FirstDate then WriteOk    := True;
      end;
    end;

    case DateOptions.FilterAccessKind of
      dfakCreatedFiles : if CreationOk=True then DateFilterOk := True;
      dfakModifiedFiles: if WriteOk=True then DateFilterOk := True;
      dfakOpenedFiles  : if AccessOk=True then DateFilterOk := True;
      dfakAnyFiles     : if (CreationOk=True) or (AccessOk=True) or (WriteOk=True) then DateFilterOk := True;
    end;
  end;

  Result := DateFilterOk;
end;

function TBZFileSearchEngine.IsSizeOk(FileInfos : TBZFileInformations; SizeFilter : TBZFileSizeFilter) : Boolean;
var
  SizeFilterOk : Boolean;
begin
  if SizeFilter.FilterOnSize=False then SizeFilterOk := True else
  begin
    SizeFilterOk := False;
    case SizeFilter.SizeFilterKind of
      sfkSmallerOrEqualTo : if FileInfos.Size<=SizeFilter.Size then SizeFilterOk := True;
      sfkBiggerOrEqualTo  : if FileInfos.Size>=SizeFilter.Size then SizeFilterOk := True;
      sfkInRangeOrEqual   : if ((FileInfos.Size>=SizeFilter.Size) and (FileInfos.Size<=SizeFilter.SizeMax)) then SizeFilterOk := True;
    end;
  end;

  Result := SizeFilterOk;
end;


function TBZFileSearchEngine.IsFileMatching(FileName : String) : Boolean;
var
  VerifMask    : TMask;
  FileMatching : Boolean;
  Pos          : Integer;
begin
  FileMatching := False;
  Pos          := 0;

  while (FileMatching=False) and (Pos<FileMasks.Count) do
  begin
    VerifMask := TMask.Create(FileMasks.Strings[Pos]);
    FileMatching := VerifMask.Matches(FileName);
    VerifMask.Free;
    Inc(Pos);
  end;

  Result := FileMatching;
end;

function TBZFileSearchEngine.IsAllFileMarkerPresents : Boolean;
begin
  Result := (FileMasks.IndexOf('*.*') > -1);
end;

function TBZFileSearchEngine.ExcludeFile(FileInfos : TBZFileInformations; Exclude : TStringList) : Boolean;
var
  VerifMask   : TMask;
  PosFilter   : Integer;
  MustExclude : Boolean;
begin
  MustExclude := False;
  PosFilter   := 0;

  while (MustExclude=False) and (PosFilter<Exclude.Count) do
  begin
    VerifMask := TMask.Create(Exclude.Strings[PosFilter]);
    MustExclude := VerifMask.Matches(FileInfos.Name);
    VerifMask.Free;
    Inc(PosFilter);
  end;

  Result := MustExclude;
end;

function TBZFileSearchEngine.Search : Boolean;
var
  bAcceptFile : Boolean;
  bCont       : Boolean;
  bNewPath    : Boolean;
  bStop       : Boolean;
  bFirstPass  : Boolean;
  ListePath   : TStringList;
  sPathName   : String;
  sFiltreOk   : String;
  stFindData  : TSearchRec;
  FileInfos   : TBZFileInformations;
  Stats       : TBZSearchStatistics;
  IncludeFile : Boolean;

Begin
  Result := true;
  if assigned(OnSearchStart)=True then OnSearchStart(Self);

  ListePath    := TStringList.Create;
  bCont        := True;
  bFirstPass   := True;
  sPathName    := RootPath;

  Stats.NbFilesFound   := 0;
  Stats.FoundInNbPath  := 0;
  Stats.NbPathFound    := 0;

  FileMasks.Sort;

  while bCont=True do
  begin
    bNewPath := True;
    if bFirstPass=False then
    begin
      if ListePath.Count=0 then bCont := False
      else
      begin
        sPathName := ListePath.Strings[0];
        ListePath.Delete(0);
      end;
    end;

    if bCont=True then
    begin
      if assigned(OnSearchChangeFolder)=True then OnSearchChangeFolder(Self, sPathName);

      if sPathName[Length(sPathName)]<>FixPathDelimiter('\') then sPathName := FixPathDelimiter(sPathName + '\');
      sFiltreOk := sPathName + '*.*';

      if FindFirstUTF8(sFiltreOk, faAnyFile, stFindData)=0 then
      begin
        bStop := False;
        while bStop=False and bCont=True do
        begin
          if (stFindData.Name<>'.') and (stFindData.Name<>'..') then
          begin
            if ((stFindData.Attr and faDirectory)<>0) and (SearchOptions.IncludeSubfolder=True) then ListePath.Add(sPathName+stFindData.Name);
            FileInfos := GetFileInformations(sPathName, stFindData);
            IncludeFile := False;
            if IsAttributesOk(stFindData.Attr)=True then IncludeFile := True;
            if (SearchOptions.LookForAnyFile=True) and ((stFindData.Attr and faDirectory)=0) then IncludeFile := True;
            if IsDateOk(FileInfos)=False then IncludeFile := False;
            if IsSizeOk(FileInfos, SizeOptions)=False then IncludeFile := False;
            if ExcludeFile(FileInfos, ExcludeMasks)=True then IncludeFile := False;
            if (stFindData.Attr and faDirectory)<>0 then
            begin
              if (IsFileMatching(stFindData.Name)=False) and (IsAllFileMarkerPresents=False) then IncludeFile := False;
            end
            else if IsFileMatching(stFindData.Name)=False then IncludeFile := False; { FileCheck}

            if IncludeFile=True then
            begin
              bAcceptFile := True;
              if Assigned(OnSearchAcceptFile)=True then bAcceptFile := OnSearchAcceptFile(Self, FileInfos);

              if bAcceptFile=True then
              begin
                if (stFindData.Attr and faDirectory)<>0 then
                begin
                  Inc(Stats.NbPathFound);
                  if assigned(OnSearchFileFound)=True then OnSearchFileFound(Self, FileInfos);
                end
                else
                begin
                  Inc(Stats.NbFilesFound);
                  if bNewPath=True then
                  begin
                    bNewPath := False;
                    Inc(Stats.FoundInNbPath);
                  end;
                  if assigned(OnSearchFileFound)=True then OnSearchFileFound(Self, FileInfos);
                end;
              end;
            end;
          end;
          if FindNextUTF8(stFindData)<>0 then bStop := True;
        end;
        FindCloseUTF8(stFindData);
      end
      else
      begin
       bCont  := False;
       Result := False;
      end;

      bFirstPass := False;
    end;
  end;

  if assigned(OnSearchCompleted) then OnSearchCompleted(Self, Stats);

  FreeAndNil(ListePath);
end;

{%endregion%}

{%region=====[ TBZFileFinder ]======================================================================}

constructor TBZFileFinder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOnSearchFileFound    := nil;
  FOnSearchAcceptFile   := nil;
  FOnSearchChangeFolder := nil;
  FOnSearchCompleted     := nil;

  FSearchOptions := TBZSearchOptionsParams.Create(Self);

  FExcludedMasks := TStringList.Create;

  FFileMasks := TStringList.Create;
  FFileMasks.Add('*.*');

  FSearchEngine := TBZFileSearchEngine.Create;
  FSearchEngine.OnSearchFileFound    := @DoOnSearchFileFound;
  FSearchEngine.OnSearchAcceptFile   := @DoOnSearchAcceptFile;
  FSearchEngine.OnSearchChangeFolder := @DoOnSearchChangeFolder;
  FSearchEngine.OnSearchCompleted    := @DoOnSearchCompleted;
  FSearchEngine.OnSearchStart        := @DoOnSearchStart;

  FDateFilter := TBZDateFilterParams.Create(Self);
  FSizeFilter := TBZSizeFilterParams.Create(Self);

  FSearchResult := TStringList.Create;
end;

destructor TBZFileFinder.Destroy;
begin
  FreeAndNil(FSearchResult);
  FreeAndNil(FSizeFilter);
  FreeAndNil(FDateFilter);
  FreeAndNil(FSearchEngine);
  FreeAndNil(FFileMasks);
  FreeAndNil(FExcludedMasks);

  inherited Destroy;
end;

procedure TBZFileFinder.SetExcludedMasks(NewExcluded : TStrings);
begin
  FExcludedMasks.Clear;
  FExcludedMasks.Assign(NewExcluded);
end;

function TBZFileFinder.GetExcludedMasks : TStrings;
begin
  Result := FExcludedMasks;
end;

procedure TBZFileFinder.SetFileMasks(NewFileNames : TStrings);
begin
  FFileMasks.Clear;
  FFileMasks.Assign(NewFileNames);
end;

function TBZFileFinder.GetFileMasks : TStrings;
begin
  Result := FFileMasks;
end;

function TBZFileFinder.Search : Boolean;
begin
  ConvertSearchOptions;
  ConvertFilterOptions;
  Result := FSearchEngine.Search;
end;

procedure TBZFileFinder.ConvertSearchOptions;
begin
  with FSearchEngine do
  begin
    SearchOptions.IncludeSubfolder    := FSearchOptions.IncludeSubfolder;
    SearchOptions.LookForReadOnlyFile := FSearchOptions.LookForReadOnlyFile;
    SearchOptions.LookForHiddenFile   := FSearchOptions.LookForHiddenFile;
    SearchOptions.LookForSystemFile   := FSearchOptions.LookForSystemFile;
    SearchOptions.LookForDirectory    := FSearchOptions.LookForDirectory;
    SearchOptions.LookForArchiveFile  := FSearchOptions.LookForArchiveFile;
    SearchOptions.LookForAnyFile      := FSearchOptions.LookForAnyFile;
  end;
end;

procedure TBZFileFinder.SetRootPath(NewRootPath : String);
begin
  FSearchEngine.RootPath := NewRootPath;
end;

function TBZFileFinder.GetRootPath : String;
begin
  Result := FSearchEngine.RootPath;
end;

procedure TBZFileFinder.ConvertFilterOptions;
begin
  with FSearchEngine do
  begin
    DateOptions.FilterOnDate     := FDateFilter.Filter;
    DateOptions.FilterAccessKind := FDateFilter.AccessKind;
    DateOptions.DateFilterKind   := FDateFilter.FilterKind;
    DateOptions.FirstDate        := FDateFilter.FirstDate;
    DateOptions.SecondDate       := FDateFilter.SecondDate;

    SizeOptions.FilterOnSize   := FSizeFilter.Filter;
    SizeOptions.SizeFilterKind := FSizeFilter.FilterKind;
    SizeOptions.Size           := FSizeFilter.Size;
    SizeOptions.SizeMax        := FSizeFilter.SizeMax;

    ExcludeMasks.Assign(FExcludedMasks);
    FileMasks.Assign(FFileMasks);
  end;

end;

procedure TBZFileFinder.DoOnSearchFileFound(Sender : TObject; FileFound : TBZFileInformations);
begin
  FSearchResult.Add(FixpathDelimiter(IncludeTrailingPathDelimiter(FileFound.Path))+ FileFound.Name);
  if assigned(FOnSearchFileFound)=True then FOnSearchFileFound(Self, FileFound);
end;

function TBZFileFinder.DoOnSearchAcceptFile(Sender : TObject;FileFound : TBZFileInformations) : Boolean;
begin
  Result := True;
  if Assigned(FOnSearchAcceptFile)=True then Result := FOnSearchAcceptFile(Self, FileFound);
end;

procedure TBZFileFinder.DoOnSearchChangeFolder(Sender : TObject; NewPath : String);
begin
  if assigned(FOnSearchChangeFolder)=True then FOnSearchChangeFolder(Self, NewPath);
end;

procedure TBZFileFinder.DoOnSearchStart(Sender : TObject);
begin
  FSearchResult.Clear;
  if assigned(FOnSearchStart)=True then FOnSearchStart(Self);
end;

procedure TBZFileFinder.DoOnSearchCompleted(Sender : TObject; Stats : TBZSearchStatistics);
begin
  if assigned(FOnSearchCompleted)=True then FOnSearchCompleted(Self,Stats);
end;

{%endregion%}


end.

