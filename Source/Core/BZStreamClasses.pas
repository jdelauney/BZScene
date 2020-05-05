(*
  Objets stream étendues et optimisés pour l'accès aux données séquentiellement.@br
  Grace à un systeme de cache et de l'utilisation du "File Mapping" pour les fichiers.

  Contient TBZZLibStream un cllasse spécialisée pour la compression et decompression de flux avec ZLib

  -------------------------------------------------------------------------------------------------------------

  @created(2017-06-11)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(11/06/2017 : Creation)
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) : Cette unité n'est pas 100% finalisé. Il manque quelques classes.

  -------------------------------------------------------------------------------------------------------------

  @bold(Dependances) : BZSystem, BZUtils.  + BZLogger si DEBUG est activé

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)
   @unorderedList(
     @item(J.Delauney (BeanzMaster))
   )

  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
Unit BZStreamClasses;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================

Interface

Uses
  LCLType, LCLIntf, Classes, SysUtils, Math,
  ZBase
  //GZio, Dialogs
  // {$IFDEF WINDOWS}
  //  Windows,
  // {$ENDIF}
  {$IFDEF LINUX}
  //BaseUnix, 
  ,Unix
  //UnixType, UnixUtil
  {$ENDIF};

Const
  { cDefaultBufferedStreamBlockSize : par défaut tampon de 32Mo }
  cDefaultBufferedStreamBlockSize = 1024 * 1024 * 32;
  { cDefaultCharsDelims : Caractères de délimitation : Tabulation, retour à la ligne, espace }
  cDefaultCharsDelims = #8#9#10#13#32;

Type
  { @abstract(TBZCustomBufferedStream :  Classe d'aide à la lecture et ou ecriture de données dans un flux (TStream).@br
    L'acces se fait par le biais d'un tampon de taille définie.)

    L'acces aux données est un acces directe en mémoire et se fait de façon séquentielle. @br
    TBZCustomBufferedStream contient de nombreuse procedures et fonctions optimisées
    pour la lecture et l'écriture de valeurs de différents types. @br
    Disponibles en 2 versions "Little-Endian" et "Big-Endian".

    Cette classe ameliore surtout les performances d'accès aux données de fichiers physique. @br
    Ne pas employer directement TBZCustomBufferedStream. Utilsez la classe TBZBufferedStream
    et les autres classes descendantes. }
  TBZCustomBufferedStream = Class(TStream)
  Private
    Procedure LoadBuffer; Virtual;//(var Buf; BufSize: integer) : integer; virtual;
    Procedure WriteBuffer; Virtual;

  Protected
    Stream:     TStream; // Les données que l'on veux exploiter
    StreamSize: Int64; // La taille des données du stream
    StreamPosition: Int64; // Position dans le stream
    AutoFreeStream: Boolean; //Indicateur si l'on doit libérer le stream ou pas
    StreamViewStart, StreamViewEnd, StreamViewLength: Int64; // position et longueur du tampon dans le stream
    //StreamViewStartPtr, FZStreamViewEndPtr : PByte; // pointe directement sur le debut ou la fin peut-être utile dans certain cas (lecture depuis la fin par ex)
    StreamBytesLeft: Int64; // Nombre d'octet qui reste à lire
    StreamBytesRead: Int64; // Nombre d'octet deja lu (égual à FZStreamPosition+1)


    Buffer:     Pointer; //PByte; // Tampon mémoire pour l'acces au donnée par bloque
    BufferDefaultSize: Int64; // Taille du tampon par defaut
    BufferSize: Int64; // Taille réelle du tampon
    BufferPosition: Int64;  // Position dans le tampon
    BufferBytesRead, BufferBytesWrite: Int64;  // Nombre d'octet deja lu ou écrit dans le tampon
    BufferBytesLeft: Int64; // Nombre d'octet qui reste à lire dans le tampon

    FUseAlignedCache: Boolean; // On aligne la taille du tampon sur 32bit (accélère les échanges mémoire dans certain cas)
    StrideSize: Byte; //Taille en octet à ajouté à la fin du tampon pour l'alignement des données

    NeedStreamWrite : Boolean;
    BytesInBuf : Int64;
    BytesWritten: Int64;

    Procedure SetSize(Const NewSize: Int64); Override;
    Function GetStreamPosition: Int64;

  Public
    { Créer un nouveau flux en mémoire tampon avec une taille de bloc définie par le parmètre "aBlockSize" (64 Mo par défaut) }
    Constructor Create(Const aBlockSize: Integer = cDefaultBufferedStreamBlockSize);
    { Créer un nouveau flux mis en mémoire tampon à partir d'un objet TStream hérité avec une taille de bloc définie (64 Mo par défaut) }
    Constructor Create(AStream: TStream; Const aBlockSize: Integer = cDefaultBufferedStreamBlockSize); Overload;
    { Destruction de TBZCustomBufferedStream }
    Destructor Destroy; Override;
    { Assigne un tampon de données "aBuffer" de taille "aSize" en octet }
    Procedure AssignBuffer(aBuffer: Pointer; aSize:Int64);
    { Assign les données d'un objet TStream "aStream" }
    Procedure AssignStream(aStream: TStream);
    { Vide le tampon }
    Procedure Flush;
    { Lit des données du flux, de taille "Count" a partir de la position courante et retourne les données dans un tampon "aBuffer" }
    Function Read(Var aBuffer; Count: Longint):Longint; Override;
    { Ecrit les données du tampon de "aBuffer" de taille "Count" à la position courrante dans le flux }
    Function Write(Const aBuffer; Count: Longint): Longint; Override;
    { Se déplacer dans le tampon suivant "Offset" et en fonction du paramètre "Origin" }
    Function Seek(Const Offset: Int64; Origin: TSeekOrigin): Int64; Override;
    { Se déplacer dans le tampon vers l'avant de "Offset" }
    Function SeekForward(Offset : Int64):Int64;
    { Se déplacer dans le tampon en arrière de "Offset" }
    Function SeekBackward(Offset : Int64):Int64;
    { Lit une valeur "Byte" dans le tampon à la position actuelle et se déplace à la position suivante }
    Function ReadByte: Byte; virtual;
    { Ecrit une valeur "Byte" dans le tampon à la position actuelle et se déplace à la position suivante }
    procedure WriteByte(Const Value : Byte); virtual;
    { Se deplace de "aCount" Byte vers l'avant dans le flux depuis la position courrante }
    Procedure SkipNextByte(Const aCount: Integer = 1); virtual;
    { Se deplace de "aCount" Byte vers l'arrière dans le flux depuis la position courrante }
    Procedure GotoPreviousByte(Const aCount: Integer = 1); virtual;
    { Si les données sont du texte alors se déplace sur la ligne suivante }
    Procedure GotoNextStringLine; virtual;
    { Lit une valeur de type Word à la position courrante }
    Function ReadWord: Word; virtual;
    { Ecrit une valeur de type Word à la position courrante }
    procedure WriteWord(Const Value : Word);
    { Lit une valeur de type Integer à la position courrante }
    Function ReadInteger: Integer; virtual;
    { Ecrit une valeur de type Integer à la position courrante }
    procedure WriteInteger(Const Value : Integer); virtual;
    { Lit une valeur de type LongWord à la position courrante }
    Function ReadLongWord: Longword; virtual;
    { Ecrit une valeur de type LongWord à la position courrante }
    procedure WriteLongWord(Const Value : LongWord); virtual;
    { Lit une valeur de type Longint à la position courrante }
    Function ReadLongint: Longint; virtual;
    { Ecrit une valeur de type Longint à la position courrante }
    procedure WriteLongint(Const Value : Longint); virtual;
    { Lit une valeur de type Cardinal à la position courrante }
    Function ReadCardinal: Cardinal; virtual;
    { Ecrit une valeur de type Cardinal à la position courrante }
    procedure WriteCardinal(Const Value : Cardinal);
    { Lit une valeur de type Single à la position courrante }
    Function ReadSingle: Single; virtual;
    { Ecrit une valeur de type Single à la position courrante }
    procedure WriteSingle(Const Value : Single); virtual;
    { Lit une valeur de type Double à la position courrante }
    Function ReadDouble: Double; virtual;
    { Ecrit une valeur de type Double à la position courrante }
    procedure WriteDouble(Const Value : Double); virtual;
    { Lit une valeur de type Char à la position courrante }
    Function ReadChar: Char; virtual;
    { Ecrit une valeur de type Char à la position courrante }
    procedure WriteChar(Const Value : Char); virtual;
    { Lit une valeur de type Word à la position suivante, sans incrémenter la postion }
    Function ReadNextChar: Char; virtual;
    { Saute les caractères inclus dans  CharsDelim. (Par defaut : Espace, tab, EOL) }
    Procedure SkipChar(Const CharsDelim: String = cDefaultCharsDelims); virtual;
    { Lit une ligne de texte délimiter par #0 ou #13 à la position courrante }
    Function ReadLnString: String; virtual;
    //procedure WriteLnString(Const Value : String);
    { Lit une ligne de texte de longueur indéfinie à la position courrante }
    Function ReadString: String; virtual;
    //procedure WriteString(Const Value : String);
    { Lit une ligne de texte de longueur "Len" à la position courrante }
    Function ReadString(len: Integer): String; Overload;
    { Lit une chaine de caractères délimiter par "CharsDelim" }
    Function ReadStrToken(Const CharsDelim: String = cDefaultCharsDelims): String;
    { Lit une valeur integer dans une chaine de caractères }
    Function ReadStrIntToken: Integer;
    { Efface les données }
    Procedure Clear;
    { Retourne le tampon de données }
    Function GetBuffer: Pointer;
    { Sauvegarde le tampon de données dans le flux}
    procedure Save;
    { Retourne le tampon de données sous forme de chaine de caractères. @br
      @bold(Attention) :  La taille maximal est celle définie à la création du TBZBufferdStream }
    Function GetBufferAsString: String;
    { Retourne @True si la fin du flux est atteinte}
    Function EOS: Boolean;
    { Retourne le flux }
    function GetStream : TStream;

    { Retourne la taille totale du flux }
    Property Size: Int64 read StreamSize;
    { Retourne la position dans le flux }
    Property position: Int64 read GetStreamPosition;
    { Retourne le nombre d'octets restant à parcourir }
    property BytesLeft : Int64 read BufferBytesLeft;
  End;

  { Descendant class type of TBZCustomBufferedStream }
  TBZCustomBufferedStreamClass = Class Of TBZCustomBufferedStream;

  //TBZFileMapStream = class(TStream)
  //
  //End;

  //TBZBufferFileMapStream = Class(TBZCustomBufferedStream)
  //private
  //  hMapping : THandle;   // Handle de l'objet file-mapping
  //  //FMemory  : ;// Adresse de base du mapping
  //  FHandle  : THandle;   // Handle du fichier ouvert pour le mapping
  //  FMapPosition,
  //  FMapSize    ,
  //  FMapMaxSize : Integer;
  //public
  //  Constructor Create(AStream: TStream; Const aBlockSize: Integer = cDefaultBufferedStreamBlockSize); Overload;
  //  // Entregistre les pages modifiées dans le fichier sur le disque
  //  procedure Flush;
  //  // Charge Count octet du flux dans Buffer
  //  function  Read(var Buffer; Count: Longint): Longint; override;
  //  // Ecrit Count octet de Buffer dans le flux
  //  function  Write(const Buffer; Count: Longint): Longint; override;
  //  // Déplace le cuseur de lectre/ecriure dans le flux
  //  function  Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  //  // Tableau d'octet pour un accés direct en mémoire
  //  //property  MapMemory: pByteArray read FMemory;
  //  // Retoune la position ducurseu delecture/écriture dans le Flux
  //  property  MapPosition: Integer read FPosition;
  //  // Taille du Flux
  //  property  MapSize : Integer read FMapSize;
  //  // Taille Maximum du Flux
  //  property  MapMaxSize : Integer read FMapMaxSize;
  //  // Handle du fichier
  //  property  MapHandle: TMapHandle read FHandle;
  //  constructor Create(FileName: string;
  //                     MappingName: String ;
  //                     Mode: Word;
  //                     Rights: Cardinal;
  //                     Offset: Cardinal;
  //                     Count : Cardinal;
  //                     MaxSize: Cardinal;
  //                     WriteCopy: Boolean = true);
  //  destructor Destroy; override;
  //published
  //  { Published declarations }
  //End;

  { TBZBufferedStream : See @link(TBZCustomBufferedStream) for more informations }
  TBZBufferedStream = Class(TBZCustomBufferedStream);

{ : TBZBufferedFileStream Stream spécialisé dans l'acces aux données d'un fichier sur le disque, par tampon }
(*  TBZBufferedFileStream = class(TBZCustomBufferedStream)
  private
    procedure FlushCache;
  protected
    FHandle: THandle;
    FOwnsHandle: Boolean;
    FFileName: string;
    FFileSize: Int64;

    function CreateHandle(FlagsAndAttributes: DWORD): THandle;
    function GetFileSize: Int64; virtual;

  public
    constructor Create(const FileName: string); overload;
    constructor Create(const FileName: string; CacheSize: Integer); overload;
    constructor Create(const FileName: string; CacheSize: Integer; Handle: THandle); overload; virtual;

    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

    property FileName : String Read FFileName;
  end; *)

{ : TBZBufferedFileMapStream est spécialisé dans l'acces aux données d'un fichier mappé (File Mapping), par tampon }
(*  TBZBufferedFileMapStream  = class(TBZCustomBufferedFileStream)
  private

  protected

    FMapHandle : THandle;

    FOwnsMapHandle: Boolean;


    function CreateHandle(FlagsAndAttributes: DWORD): THandle;override;
    function GetFileSize: Int64; virtual;

  public
    constructor Create(const AStream: TStream); overload;
    constructor Create(const FileName: string; CacheSize: Integer); overload;
    constructor Create(const FileName: string; CacheSize: Integer; Handle: THandle); overload; virtual;
    constructor Create(const FileName: string); overload;

    destructor Destroy; override;
  end; *)

  { : TBZStream "Super Classe" pour la gestion d'acces aux données via et en fonction d'autres stream.
    L'acces via les classes TStream de base (TStream, TCustomMemoryStream, ThandleStream,...) sont soit
    utilisé tel quel, soit pour le THandleStream utilise TBZBufferedFileStream ou TBZBufferedFileMapStream
    pour améliorer les performances. Les Stream de type inconnue sont convertit en TMemoryStream
  }
(*  TBZStream = class(TStream)
  private
     FAutoCacheFile : Boolean;
     FAutoFileMap : Boolean;
     FUseFileMapping : Boolean;
     FWrappedStream : TStream;
     BufferedStreamClass : TBZCustomBufferedStreamClass;
  protected
  public
    Constructor Create(AStream : TStream); overload; override;
    Constructor Create(AFileName : String; Mode:byte); overload; override;
    Destructor Destroy;

    procedure LoadFromStream( AStream : TStream);
    procedure LoadFromFile( AFileName : String);

    procedure SaveToStream( AStream : TStream);
    procedure SaveToFile( AFileName : String);

    // procedure clone(var aBZStreamClass; aBZStreamClassMode);

    property AutoCacheFile : Boolean Read FAutoCacheFile Write AutoCacheFile;
    property AutoFileMap : Boolean read FAutoFileMap Write FAutoFileMap;
    property UseFileMapping : Boolean Read FUseFileMapping Write FUseFileMapping;
  end; *)

Type
  { TGZCompressionLevel : Enumeration des niveaux de compression }
  TGZCompressionLevel = (
    clnone,                     //< Do not use compression, just copy data.
    clfastest,                  //< Use fast (but less) compression.
    cldefault,                  //< Use default compression
    clmax                       //< Use maximum compression
    );

  { TGZOpenMode : Enumeration des modes d'acces aux fichiers compressés }
  TGZOpenMode = (
    gzopenread,                 //< Open file for reading.
    gzopenwrite                 //< Open file for writing.
    );

  { TBZZLibStream : Classe utile pour compresser et décompresser un flux avec ZLib. @br
    Version améliorée de la classe ZStream de FPC }
  TBZZLibStream = Class(TOwnerStream)
  Private
    Fonprogress: TNotifyEvent;
    FBuffer:     pointer;
    FWriteMode:  Boolean;
  Protected
    FZStream: z_stream;


    raw_written, compressed_written: Int64;
    raw_read, compressed_read: Int64;

    skipheader: Boolean;

    Procedure reset;

    Function GetPosition(): Int64; Override;

    Function GetAvailableInput: Integer;
    Function GetAvailableOutput: Integer;

    Procedure Progress(Sender: TObject);

  Public
    Constructor Create(stream: TStream);

    Constructor Create(Level: TGZCompressionLevel; Dest: TStream; Askipheader: Boolean = False); Overload; // Pour l'ecriture
    Constructor Create(ASource: TStream; Askipheader: Boolean = False);  // Pour la lecture

    Destructor Destroy; Override;

    Function Write(Const buffer; Count: Longint): Longint; Override;
    Function Read(Var buffer; Count: Longint): Longint; Override;
    Function Seek(Const Offset: Int64; Origin: TSeekOrigin): Int64; Override;
    Procedure Flush;

    Function Get_CompressionReadRate: Single;
    Function Get_CompressionWriteRate: Single;

    Property AvailableInput: Integer read GetAvailableInput;
    Property AvailableOutput: Integer read GetAvailableOutput;

    Property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  End;

  { Exception levée par la bibliothèque ZLib }
  EZLibError = Class(EStreamError);
  { Exception levée par la bibliothèque ZLib en cas d'erreur pendant la compression }
  EGZCompressionError = Class(EZLibError);
  { Exception levée par la bibliothèque ZLib en cas d'erreur pendant la décompression }
  EGZDecompressionError = Class(EZLibError);
  { Exception levée si le fichier n'existe pas }
  EBZFileNotExist = Class(Exception);
  { Exception levée si le fichier ne peux pas être créé }
  EBZCantCreateFile = Class(Exception);

{ Creation d'un flux pour l'acces à un fichier en écriture et ou en lecture) }
Function CreateFileStream(Const fileName: String; mode: Word = fmOpenRead + fmShareDenyNone): TStream;

Implementation

Uses Dialogs, zdeflate, zinflate,
     BZSystem, BZUtils
     //, BZCrossPlateFormTools
     {$IFDEF DEBUG}
     ,BZLogger
     {$ENDIF};


Const
  {Taille du tampon utilisé pour stocker temporairement les données du flux enfant.}
  cGZBufsize = 1024 * 64;


Function CreateFileStream(Const fileName: String; mode: Word = fmOpenRead + fmShareDenyNone): TStream;
var fn : String;
Begin
  fn:=filename;
  FixPathDelimiter(fn);
  If ((mode And fmCreate) = fmCreate) Or FileExists(fn) Then
    Result := TFileStream.Create(fn, mode) //TBZFileMapping.Create(fileName,fmmOpenOrCreate) //
  Else
    Raise EBZFileNotExist.Create('Fichier non trouvé : "' + fn + '"');

End;

{%region%=====[ TBZCustomBufferedStream ]=====================================}

constructor TBZCustomBufferedStream.Create(const aBlockSize : Integer);
Begin
  Inherited Create;
  Buffer := nil;
  BufferDefaultSize := aBlockSize;
  BufferSize := BufferDefaultSize;
  BufferBytesRead := 0;
  BufferBytesLeft := -1;
  BufferPosition := 0;

  Stream := nil;
  StreamSize := BufferSize;
  StreamPosition := 0;
  StreamViewStart := 0;
  StreamViewEnd := 0;
  StreamViewLength := 0;
  AutoFreeStream := False;
  StreamBytesLeft := 0;
  StreamBytesRead := 0;

  NeedStreamWrite := False;
  BytesInBuf := 0;
End;

constructor TBZCustomBufferedStream.Create(AStream : TStream; const aBlockSize : Integer);
Begin
  Inherited Create;
  Buffer := nil;
  BufferDefaultSize := aBlockSize;
  BufferSize := BufferDefaultSize;
  BufferBytesRead := 0;
  BufferBytesLeft := -1;
  BufferPosition := 0;

  Stream := nil;
  StreamSize := BufferSize;

  NeedStreamWrite := False;
  BytesInBuf := 0;

  Stream := AStream;
  StreamSize := AStream.Size;

  StreamPosition := 0;
  StreamViewStart := 0;
  StreamViewEnd := 0;
  StreamViewLength := 0;
  AutoFreeStream := False;
  StreamBytesLeft := StreamSize;
  StreamBytesRead := 0;
End;

destructor TBZCustomBufferedStream.Destroy;
Begin

  If AutoFreeStream Then FreeAndNil(Stream);
  //FreeAndNil(Buffer);
  memReAlloc(Buffer, 0);
  FreeMem(Buffer);
  Buffer := nil;
  Inherited Destroy;
End;

procedure TBZCustomBufferedStream.Flush;
Begin

  //Clear;
  StreamViewStart := StreamViewStart + BufferPosition;
  //StreamViewEnd :=
  // StreamViewLength :=StreamViewEnd - StreamViewStart + 1;
  StreamBytesLeft := StreamSize - BufferPosition;
  BufferSize := BufferDefaultSize;

  BufferBytesRead := 0;
  BufferBytesLeft := -1;//BufferSize;
  BufferPosition := 0;
  BytesInBuf := 0;
  NeedStreamWrite := False;
End;

procedure TBZCustomBufferedStream.LoadBuffer;
Var
  SeekResult: Integer;
  RSize:      Int64;
Begin
  //GlobalLogger.LogNotice('Load Buffer from file');
  // C'est la 1er fois ? on initialise le tampon
  If Not (Assigned(Buffer)) Then SetSize(BufferDefaultSize);

  // Fin des données, plus rien à lire, on remet tout a zero et on s'en va
  If (StreamViewStart > StreamSize - 1) Or (StreamBytesLeft <= 0) Then
  Begin

    // BufferPosition:=0;
    BufferBytesRead := 0;
    BufferBytesLeft := 0;
    StreamViewLength := 0;
    StreamBytesLeft := 0;
    StreamViewStart := 0;
    StreamViewEnd := 0;
    //EndOFZStream := true;
    exit;
  End;

  // On se place sous la bonne fenètre
  SeekResult := Stream.Seek(StreamViewStart, soBeginning);
  If SeekResult = -1 Then
    Raise EStreamError.Create('TBZCustomBufferedStream.LoadBuffer: Erreur lors du positionnement dans le flux');
  If (StreamBytesLeft < BufferDefaultSize) Then
    Rsize := StreamBytesLeft
  Else
    RSize := BufferDefaultSize;
  // On lit les données, et on transfert dans le tampon
  BufferSize := Stream.Read(Buffer^, RSize);
  If BufferSize <= 0 Then
    Raise EStreamError.Create('TBZCustomBufferedStream.LoadBuffer: Erreur lors de la Lecture du flux');

  // On met à jour les marqueurs de la "fenêtre" de lecture du Stream pour le chargement

  StreamPosition := StreamViewStart;
  StreamViewLength := BufferSize;
  StreamViewEnd := StreamViewStart + StreamViewLength - 1;
  If StreamViewEnd >= StreamSize Then
    Raise EStreamError.Create('TBZCustomBufferedStream.LoadBuffer: Index StreamViewEnd hors limite');
  Dec(StreamBytesLeft, BufferSize);
  Inc(StreamBytesRead, BufferSize);

  // On reinitialise les marqueurs du Buffer
  BufferPosition := 0;
  BufferBytesRead := 0;
  BufferBytesLeft := BufferSize;

End;

procedure TBZCustomBufferedStream.WriteBuffer;
var
  SeekResult: Integer;
begin
  {$IFDEF DEBUG}GlobalLogger.LogNotice('Ecriture du fichier sur le disque');{$ENDIF}
  If Not (Assigned(Buffer)) Then Exit; // rien à écrire
 // if Not(NeedStreamWrite) or (BytesInBuf<=0) then exit; // On n'a pas demandé l'ecriture ou il n'y a rien à écrire

  SeekResult := Stream.Seek(StreamViewStart, soBeginning);
  if SeekResult = -1 then
    raise EStreamError.Create('TBZCustomBufferedStream.WriteBuffer: Erreur lors du positionnement dans le flux');

  BytesWritten := Stream.Write(Buffer^, BytesInBuf);
  if BytesWritten <> BytesInBuf then
    raise EStreamError.Create('TBZCustomBufferedStream.LoadBuffer: Erreur lors de l''ecriture du flux');

  Dec(BytesInBuf,BytesWritten);
  if BytesinBuf<>0 then ShowMessage('TBZCustomBufferedStream.LoadBuffer: Erreur probable lors de l''ecriture du flux');
  NeedStreamWrite := False;
End;

procedure TBZCustomBufferedStream.SetSize(const NewSize : Int64);
Begin
  memReAlloc(Buffer, NewSize);
  BufferSize := NewSize;
  BufferPosition := 0;
  BufferBytesRead := 0;
  BufferBytesWrite := 0;
  BufferBytesLeft := NewSize;
End;

function TBZCustomBufferedStream.GetStreamPosition : Int64;
Begin
  Result := StreamViewStart + BufferPosition;
  //GlobalLogger.LogStatus(' - Stream View Start : '+Inttostr(StreamViewStart));
  //GlobalLogger.LogStatus(' - Stream Position : '+Inttostr(result));
  //GlobalLogger.LogStatus(' - Buffer Position : '+Inttostr(BufferPosition));
End;

function TBZCustomBufferedStream.Read(var aBuffer; Count : Longint) : Longint;
Var
  NumOfBytesToCopy, NumOfBytesLeft: Int64;  //, NumOfBytesRead
  CachePtr, BufferPtr: PByte;
Begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('Read Data in Buffer : '+Inttostr(Count)+' Octets');
      GlobalLogger.LogStatus(' - Stream Position : '+Inttostr(Position));
      GlobalLogger.LogStatus(' - Buffer Position : '+Inttostr(BufferPosition));
      GlobalLogger.LogStatus(' - Buffer Bytes Left : '+Inttostr(BufferBytesLeft));
      GlobalLogger.LogStatus(' - Stream Bytes Left : '+Inttostr(StreamBytesLeft));
      GlobalLogger.LogStatus(' - Stream Size : '+Inttostr(StreamSize));
      GlobalLogger.LogStatus(' - Buffer Size : '+Inttostr(BufferSize));
    {$ENDIF}
  {$ENDIF}
  Result := 0;
  If (StreamBytesLeft > 0) then NumOfBytesLeft := Count
    else if (Count>BufferBytesLeft) then NumOfBytesLeft := BufferBytesLeft
    else NumOfBytesLeft := Count;


  BufferPtr := @aBuffer;
  While NumOfBytesLeft > 0 Do
  Begin

    If (BufferBytesLeft <= 0) Then
    Begin
      //StreamViewStart := StreamViewStart + BufferPosition;
      Flush;
      LoadBuffer; // On charge un nouveau tampon depuis le stream
    End;
    // On copie les données
    NumOfBytesToCopy := Min(BufferSize - BufferPosition, NumOfBytesLeft);
    CachePtr :=Self.Buffer;
    Inc(CachePtr, BufferPosition);

    Move(CachePtr^, BufferPtr^, NumOfBytesToCopy);
    Inc(Result, NumOfBytesToCopy);
    Inc(BufferPosition, NumOfBytesToCopy);
    Inc(BufferPtr, NumOfBytesToCopy);
    // On met à jour les marqueur de notre tampon
    Inc(BufferBytesRead, NumOfBytesToCopy);
    Dec(BufferBytesLeft, NumOfBytesToCopy);
    Dec(NumOfBytesLeft, NumOfBytesToCopy);

  End;
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogStatus(' - New Buffer Position : '+Inttostr(BufferPosition));
      GlobalLogger.LogStatus(' - New Stream Position : '+Inttostr(Position));
    {$ENDIF}
  {$ENDIF}
End;

function TBZCustomBufferedStream.Write(const aBuffer; Count : Longint) : Longint;
Var
  NumOfBytesToCopy, NumOfBytesLeft, NumOfBytesWriteLeft: Longint;
  CachePtr, BufferPtr: PByte;
 // BytesWritten : Longint;
Begin
  {$IFDEF DEBUG}
    //{$IFDEF DEBUGLOG}
    //  GlobalLogger.LogNotice('Write Data in Buffer : '+Inttostr(Count)+' Octets');
    //  GlobalLogger.LogStatus(' - Stream Position : '+Inttostr(Position));
    //  GlobalLogger.LogStatus(' - Buffer Position : '+Inttostr(BufferPosition));
    //  GlobalLogger.LogStatus(' - Bytes in buf  : '+Inttostr(BytesInBuf));
    //  GlobalLogger.LogStatus(' - Stream Bytes Left : '+Inttostr(StreamBytesLeft));
    //  GlobalLogger.LogStatus(' - Stream Size : '+Inttostr(StreamSize));
    //  GlobalLogger.LogStatus(' - Buffer Size : '+Inttostr(BufferSize));
    //{$ENDIF}
  {$ENDIF}
  Result := 0;
  NumOfBytesLeft := Count;
 // If (BufferSize>0) and (BytesInBuf+1 > pred(BufferSize)) Then Save;
  If Not (Assigned(Buffer)) Then SetSize(BufferDefaultSize);
  BufferPtr := @aBuffer;
 // NumOfBytesToCopy := 0;
  While NumOfBytesLeft > 0 Do
  Begin
    NumOfBytesToCopy := 0;
    If (BufferPosition + NumOfBytesLeft) >= pred(BufferSize) Then NumOfBytesToCopy := Pred(BufferSize) - BufferPosition;
    NumOfBytesWriteLeft := NumOfBytesLeft-NumOfBytesToCopy;
    //if NumOfBytesWriteLeft < 0 then NumOfBytesWriteLeft:=0;

    if NumOfBytesToCopy>0 then
    begin
      CachePtr := Buffer;
      Inc(CachePtr, BufferPosition);
      Move(BufferPtr^,CachePtr^, NumOfBytesToCopy);

      //Inc(BufferPosition, NumOfBytesToCopy-1);
      Inc(BufferPtr, NumOfBytesToCopy);
      Inc(BytesInBuf, NumOfBytesToCopy); //BytesInBuf:=BufferSize;
      BufferPosition := BytesInBuf-1; //-1 car on commence à la position 0
      NeedStreamWrite := True;
      WriteBuffer;
      Inc(Result, NumOfBytesToCopy);
      Dec(NumOfBytesLeft,NumOfBytesToCopy);
      Flush;
    end
    else
    begin
      CachePtr := Buffer;
      Inc(CachePtr, BufferPosition);
      Move(BufferPtr^, CachePtr^, NumOfBytesWriteLeft);
      //Inc(BufferPosition,NumOfBytesWriteLeft);
      Inc(BytesInBuf,NumOfBytesWriteLeft);
      BufferPosition := BytesInBuf;

      Dec(NumOfBytesLeft,NumOfBytesWriteLeft);
      Inc(Result,NumOfBytesWriteLeft);
    End;
  end;
end;

function TBZCustomBufferedStream.Seek(const Offset : Int64; Origin : TSeekOrigin) : Int64;
Var
  //NewBufStart, 
  NewPos: Integer;
Begin
  // Calcul de la nouvelle position
  Case Origin Of
    soBeginning: NewPos := Offset;
    soCurrent: NewPos := StreamViewStart + BufferPosition + Offset;
    soEnd: NewPos := pred(StreamSize) - Offset;
    Else
      Raise Exception.Create('TBZCustomBufferedStream.Seek: Origine Invalide');
  End;
  Result :=NewPos;
 // if Offset = 0 then exit;

  // Calcul de la fenêtre du stream, alignement des données
//  NewBufStart := NewPos and not Pred(BufferSize);
  //NewBufStart := NewPos - (StreamViewStart mod BufferSize);
 (*  if NewBufStart <> StreamViewStart then
  begin

    StreamViewStart := NewBufStart;
    StreamPosition := NewBufStart;
    BufferBytesLeft:=0;
  end
  else StreamPosition := NewPos; //- NewBufStart;  *)
  if NeedStreamWrite then
  begin
    WriteBuffer;
    NeedStreamWrite := False;
    Flush;
  end;

  BufferPosition := NewPos;
  BufferBytesLeft := BufferSize - BufferPosition;
  Result := NewPos;
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
    GlobalLogger.LogStatus(' - Seek New Buffer Position : '+Inttostr(BufferPosition));
    GlobalLogger.LogStatus(' - Seek New Stream Position : '+Inttostr(Position));
    {$ENDIF}
  {$ENDIF}
End;

function TBZCustomBufferedStream.SeekForward(Offset : Int64) : Int64;
Begin
  result := Seek(Abs(Offset),soCurrent);
  //SkipNextByte(Abs(Offset));
  //result := Position;
End;

function TBZCustomBufferedStream.SeekBackward(Offset : Int64) : Int64;
Begin
  if Offset>0 then Offset := -Offset;
  result := Seek(Offset,soCurrent);
  ////Offset := Position - Offset;
  ////result := Seek(Offset,soBeginning);
  //GotoPreviousByte(Abs(Offset));
  //result := Position;
End;

procedure TBZCustomBufferedStream.AssignBuffer(aBuffer : Pointer; aSize : Int64);
Begin
  SetSize(aSize);
  Move(aBuffer^, Buffer, aSize);
End;

procedure TBZCustomBufferedStream.Save;
begin
  if BytesInBuf>0 then
  begin
    NeedStreamWrite := True;
    WriteBuffer;
    Flush;
  End;
End;

procedure TBZCustomBufferedStream.AssignStream(aStream : TStream);
Var
  ms: TMemoryStream;
Begin
  If (aStream Is TCustomMemoryStream) Then
  Begin
    //    AssignBuffer(TCustomMemoryStream(aStream).Memory,aStream.Size);
    Stream := TCustomMemoryStream(aStream);
    StreamSize := TCustomMemoryStream(aStream).Size;
    StreamPosition := 0;
    StreamViewStart := 0;
    StreamViewEnd := StreamSize - 1;
    StreamViewLength := StreamSize;
    StreamBytesLeft := StreamSize;
    StreamBytesRead := 0;
    AutoFreeStream := False;
  End
  Else
  Begin
    ms := TMemoryStream.Create;
    With ms Do
    Begin
      CopyFrom(aStream, 0);
      Position := 0;
    End;
    Stream := ms;
    StreamSize := ms.Size;
    StreamPosition := 0;
    StreamViewStart := 0;
    StreamViewEnd := StreamSize - 1;
    StreamViewLength := StreamSize;
    StreamBytesLeft := StreamSize;
    StreamBytesRead := 0;
    AutoFreeStream := True;
  End;
End;

function TBZCustomBufferedStream.ReadByte : Byte;
Begin
  If (BufferBytesLeft < 1) Then
  Begin
    Flush;
    LoadBuffer;
  End;
  Result := PByte(Buffer + BufferPosition)^;
  Inc(BufferPosition);
  Inc(BufferBytesRead);
  Dec(BufferBytesLeft);
End;

procedure TBZCustomBufferedStream.WriteByte(const Value : Byte);
Begin
  If (BufferSize>0) and (BytesInBuf+1 > pred(BufferSize)) Then Save;
  If Not (Assigned(Buffer)) Then SetSize(BufferDefaultSize);
  PByte(Buffer + BufferPosition)^ := value;
  Inc(BufferPosition);
  Inc(BytesInBuf);
End;

procedure TBZCustomBufferedStream.SkipNextByte(const aCount : Integer);
Begin
  If (BufferBytesLeft <  aCount) Then
  Begin
    Flush;
    LoadBuffer;
  End;

  Inc(BufferPosition, aCount);
  Inc(BufferBytesRead, aCount);
  Dec(BufferBytesLeft, aCount);
End;

procedure TBZCustomBufferedStream.GotoPreviousByte(const aCount : Integer);
Begin
  { TODO 0 -oBZStreamClasses -cTBZCustomBufferedStream : GotoPreviousByte. Gérer le rechargement en arriere du buffer }
  Dec(BufferPosition,aCount);
  Dec(BufferBytesRead,aCount);
  Inc(BufferBytesLeft,aCount);
End;

function TBZCustomBufferedStream.ReadWord : Word;
Begin
  If (BufferBytesLeft < 2) Then
  Begin
    flush;
    loadbuffer;
  End;
  Result := PWord(Buffer + BufferPosition)^;
  Inc(BufferPosition, 2);
  Inc(BufferBytesRead, 2);
  Dec(BufferBytesLeft, 2);
End;

procedure TBZCustomBufferedStream.WriteWord(const Value : Word);
Begin
  If (BufferSize>0) and ((BytesInBuf+2) > pred(BufferSize)) Then Save;
  If Not (Assigned(Buffer)) Then SetSize(BufferDefaultSize);
  PWord(Buffer + BufferPosition)^ := value;
  Inc(BufferPosition,2);
  Inc(BytesInBuf,2);
End;

function TBZCustomBufferedStream.ReadInteger : Integer;
Begin
  If (BufferBytesLeft < 4) Then
  Begin
    flush;
    loadbuffer;
  End;
  Result := PInteger(Buffer + BufferPosition)^;
  Inc(BufferPosition, 4);
  Inc(BufferBytesRead, 4);
  Dec(BufferBytesLeft, 4);
End;

procedure TBZCustomBufferedStream.WriteInteger(const Value : Integer);
Begin
  If (BufferSize>0) and ((BytesInBuf+4) > pred(BufferSize)) Then Save;
  If Not (Assigned(Buffer)) Then SetSize(BufferDefaultSize);
  PInteger(Buffer + BufferPosition)^ := value;
  Inc(BufferPosition,4);
  Inc(BytesInBuf,4);
End;

function TBZCustomBufferedStream.ReadLongint : Longint;
Begin
  If (BufferBytesLeft < 4) Then
  Begin
    flush;
    loadbuffer;
  End;
  Result := PLongint(Buffer + BufferPosition)^;
  Inc(BufferPosition, 4);
  Inc(BufferBytesRead, 4);
  Dec(BufferBytesLeft, 4);
End;

procedure TBZCustomBufferedStream.WriteLongint(const Value : Longint);
Begin
  If (BufferSize>0) and ((BytesInBuf+4) > pred(BufferSize)) Then Save;
  If Not (Assigned(Buffer)) Then SetSize(BufferDefaultSize);
  PLongint(Buffer + BufferPosition)^ := value;
  Inc(BufferPosition,4);
  Inc(BytesInBuf,4);
End;

function TBZCustomBufferedStream.ReadLongWord : Longword;
Begin
  If (BufferBytesLeft < 4) Then
  Begin
    flush;
    loadbuffer;
  End;
  Result := PLongWord(Buffer + BufferPosition)^;
  Inc(BufferPosition, 4);
  Inc(BufferBytesRead, 4);
  Dec(BufferBytesLeft, 4);
End;

procedure TBZCustomBufferedStream.WriteLongWord(const Value : LongWord);
Begin
  If (BufferSize>0) and ((BytesInBuf+4) > pred(BufferSize)) Then Save;
  If Not (Assigned(Buffer)) Then SetSize(BufferDefaultSize);
  PLongWord(Buffer + BufferPosition)^ := value;
  Inc(BytesInBuf,4);
  BufferPosition := BytesInBuf;
End;

function TBZCustomBufferedStream.ReadCardinal : Cardinal;
Begin
  If (BufferBytesLeft < 4) Then
  Begin
    flush;
    loadbuffer;
  End;
  Result := PCardinal(Buffer + BufferPosition)^;
  Inc(BufferPosition, 4);
  Inc(BufferBytesRead, 4);
  Dec(BufferBytesLeft, 4);
End;

procedure TBZCustomBufferedStream.WriteCardinal(const Value : Cardinal);
Begin
  If (BufferSize>0) and ((BytesInBuf+4) > pred(BufferSize)) Then Save;
  If Not (Assigned(Buffer)) Then SetSize(BufferDefaultSize);
  PCardinal(Buffer + BufferPosition)^ := value;
  Inc(BufferPosition,4);
  Inc(BytesInBuf,4);
End;

function TBZCustomBufferedStream.ReadSingle : Single;
Var
  ts: Byte;
Begin
  ts := SizeOf(Single);
  If (BufferBytesLeft < ts) Then
  Begin
    flush;
    loadbuffer;
  End;
  Result := PSingle(Buffer + BufferPosition)^;

  Inc(BufferPosition, ts);
  Inc(BufferBytesRead, ts);
  Dec(BufferBytesLeft, ts);
End;

procedure TBZCustomBufferedStream.WriteSingle(const Value : Single);
Var
  ts: Byte;
Begin
  ts := SizeOf(Single);
  If (BufferSize>0) and ((BytesInBuf+ts) > pred(BufferSize)) Then Save;
  If Not (Assigned(Buffer)) Then SetSize(BufferDefaultSize);
  PSingle(Buffer + BufferPosition)^ := value;
  Inc(BufferPosition,ts);
  Inc(BytesInBuf,ts);
End;

function TBZCustomBufferedStream.ReadDouble : Double;
Var
  ts: Byte;
Begin
  If (BufferBytesLeft < 8) Then
  Begin
    flush;
    loadbuffer;
  End;
  Result := PDouble(Buffer + BufferPosition)^;
  ts := SizeOf(Double);
  Inc(BufferPosition, ts);
  Inc(BufferBytesRead, ts);
  Dec(BufferBytesLeft, ts);
End;

procedure TBZCustomBufferedStream.WriteDouble(const Value : Double);
Var
  ts: Byte;
Begin
  ts := SizeOf(Double);
  If (BufferSize>0) and ((BytesInBuf+ts) > pred(BufferSize)) Then Save;
  If Not (Assigned(Buffer)) Then SetSize(BufferDefaultSize);
  PDouble(Buffer + BufferPosition)^ := value;
  Inc(BufferPosition,ts);
  Inc(BytesInBuf,ts);
End;

function TBZCustomBufferedStream.ReadChar : Char;
Begin
  If (BufferBytesLeft < 1) Then
  Begin
    flush;
    loadbuffer;
  End;
  Result := PChar(Buffer + BufferPosition)^;
  Inc(BufferPosition);
  Inc(BufferBytesRead);
  Dec(BufferBytesLeft);
End;

procedure TBZCustomBufferedStream.WriteChar(const Value : Char);
Begin
  If (BufferSize>0) and ((BytesInBuf+1) > pred(BufferSize)) Then Save;
  If Not (Assigned(Buffer)) Then SetSize(BufferDefaultSize);
  PChar(Buffer + BufferPosition)^ := value;
  Inc(BufferPosition);
  Inc(BytesInBuf);
End;

function TBZCustomBufferedStream.ReadNextChar : Char;
Begin
  If (BufferBytesLeft < 1) Then
  Begin
    flush;
    loadbuffer;
  End;
  Result := PChar(Buffer + BufferPosition)^;
End;

procedure TBZCustomBufferedStream.GotoNextStringLine;
Var
  c: Char;
  sLineEnding: String;
Begin
  sLineEnding := #13#10;
  C := ReadChar;
  While ((BufferPosition < BufferSize) And Not (pos(C, sLineEnding) > 0)) Do
  Begin
    C := ReadChar;
  End;
  If (C = #13) Then ReadChar;      //or (C=#10)
End;

function TBZCustomBufferedStream.ReadLnString : String;
Var
  C1, C2: Char;
  Stop:   Boolean;
  S:      String;
Begin
  If (BufferBytesLeft <= 0) Then
  Begin
    flush;
    loadbuffer;
  End;
  S := '';
  Stop := False;
  While Not (Stop) And (BufferPosition <= BufferSize - 1) Do
  Begin
    C1 := ReadChar;
    If (C1 = #13) Or (C1 = #10) or (C1 = #0) Then
    Begin
      Stop := True;
      C2 := ReadChar;
      If (C2 In [#32..#255]) Then
      Begin
        Dec(BufferPosition);
        Dec(BufferBytesRead);
        Inc(BufferBytesLeft);
      End;
      // S:=S+C1;
    End
    Else
    Begin
      If (C1 = #9) Or (C1 = #32) Or (C1 In [#32..#255]) Then
        S := S + C1;
    End;
  End;
  Result := S;
End;

function TBZCustomBufferedStream.ReadString(len : Integer) : String;
Var
  S: String;
  TmpBuffer: PByte;
Begin
  //GlobalLogger.LogNotice('ReadString At : '+IntToStr(GetPosition));
  If (BufferBytesLeft < Len) Then
  Begin
    flush;
    loadbuffer;
  End;
  Result := '';
  S := '';
  SetLength(S, Len);
  Move(PChar(Buffer + BufferPosition)^, S[1], Len);
  TmpBuffer := Pointer(S) + Len;
  TmpBuffer^ := 0;
  Result := S;
  Len:=Len-1;
  Inc(BufferPosition, Len);
  Inc(BufferBytesRead, Len);
  Dec(BufferBytesLeft, Len);

End;

function TBZCustomBufferedStream.ReadString : String;
Var
  Len: Integer;
  TmpBuffer: PByte;
Begin
  Len := ReadInteger;
  SetLength(Result, Len);
  Read(Pointer(Result), Len);
  TmpBuffer := Pointer(Result) + Len;
  TmpBuffer^ := 0;
End;

procedure TBZCustomBufferedStream.SkipChar(const CharsDelim : String);
Var
  c: Char;
Begin
  C := ReadChar;
  While ((BufferPosition < BufferSize) And (pos(C, CharsDelim) > 0)) Do
  Begin
    C := ReadChar;
  End;
  Dec(BufferPosition);
  Dec(BufferBytesRead);
  Inc(BufferBytesLeft);
End;

function TBZCustomBufferedStream.ReadStrToken(const CharsDelim : String) : String;
Var
  LastC: Char;
Begin
  Result := '';
  SkipChar(CharsDelim);
  LastC := ReadChar;
  While ((BufferPosition < BufferSize) And Not (pos(LastC, CharsDelim) > 0)) Do
  Begin
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result)] := LastC;
    LastC := ReadChar;
  End;
End;

function TBZCustomBufferedStream.ReadStrIntToken : Integer;
Var
  LastC: Char;
  S:     String;
Begin
  Result := -1;
  S := '';
  SkipChar(cDefaultCharsDelims);
  LastC := ReadChar;
  While ((BufferPosition < BufferSize) And (pos(LastC, '0123456789') > 0)) Do
  Begin
    SetLength(S, Length(S) + 1);
    S[Length(S)] := LastC;
    LastC := ReadChar;
  End;
  If s <> '' Then
  Begin
    Result := StrToInt(S);
    GotoPreviousByte;
  End;
End;

(*function TBZCustomBufferedStream.ReadBigEndianCardinal: Cardinal;
// Reads the next four bytes from the memory pointed to by Run, converts this into a cardinal number (inclusive byte
// order swapping) and advances Run.
begin
  if BufferBytesLeft = 0 then begin flush; loadbuffer; end;
  Result := SwapLong(PCardinal(Buffer+BufferPosition)^);
  Inc(BufferPosition,Sizeof(Cardinal));
 // Inc(PCardinal(Buffer)); //Inc(Buffer,Sizeof(Cardinal));
  Dec(BufferBytesLeft,Sizeof(Cardinal));
end;

function TBZCustomBufferedStream.ReadBigEndianDouble: Double;
// Reads the next two bytes from the memory pointed to by Run, converts this into a word number (inclusive byte
// order swapping) and advances Run.
begin
  result:=-1;
  if BufferBytesLeft = 0 then begin flush; loadbuffer; end;
  SwapDouble(PDouble(Buffer+BufferPosition)^, Result);
  Inc(BufferPosition,Sizeof(Double));
 // Inc(Buffer,Sizeof(Double));
  Dec(BufferBytesLeft,Sizeof(Double));
end;

function TBZCustomBufferedStream.ReadBigEndianInteger: Integer;
// Reads the next four bytes from the memory pointed to by Run, converts this into a cardinal number (inclusive byte
// order swapping) and advances Run.
begin
  if BufferBytesLeft = 0 then begin flush; loadbuffer; end;
  Result := SwapLong(PInteger(Buffer+BufferPosition)^);
  Inc(BufferPosition,Sizeof(Integer));
 // Inc(PInteger(Buffer));
  Dec(BufferBytesLeft,Sizeof(Integer));
end;

function TBZCustomBufferedStream.ReadBigEndianString(Len: Cardinal): WideString;
// Reads the next Len bytes from the memory pointed to by Run, converts this into a Unicode string (inclusive byte
// order swapping) and advances Run.
// Run is not really a PChar type, but an untyped pointer using PChar for easier pointer maths.
begin
  if BufferBytesLeft = 0 then begin flush; loadbuffer; end;
  SetString(Result, PWideChar(PAnsiChar(Buffer+BufferPosition)), Len);
//  Inc(PWideChar(PAnsiChar(Buffer+BufferPosition)), Len);
  Inc(BufferPosition,Len);
  Dec(BufferBytesLeft,Len);
  SwapShort(Pointer(Result), Len);
end;

function TBZCustomBufferedStream.ReadBigEndianString: WideString;
// Same as ReadBigEndianString with length parameter. However the length must first be retrieved.
var
  Len: Cardinal;
begin
  Len := ReadBigEndianCardinal;
  Result := ReadBigEndianString(Len);
end;

function TBZCustomBufferedStream.ReadBigEndianWord: Word;
// Reads the next two bytes from the memory pointed to by Run, converts this into a word number (inclusive byte
// order swapping) and advances Run.
begin
  Result := Swap(PWord(Buffer+BufferPosition)^);
  Inc(BufferPosition,Sizeof(Word));
 // Inc(PWord(Buffer));
  Dec(BufferBytesLeft,Sizeof(Word));
end;
 *)
procedure TBZCustomBufferedStream.Clear;
Begin
  FillByte(PByte(Buffer)^, BufferSize, 0);
End;


function TBZCustomBufferedStream.GetBuffer : Pointer;
Var
  Buf : PByte;
//  S : Int64;
Begin
 // S := StreamSize;
  Buf := nil;
  GetMem(Buf,StreamSize);
  Seek(0,soBeginning);
  Read(Buf^,StreamSize);
  Result := Pointer(Buf);
End;

function TBZCustomBufferedStream.GetBufferAsString : String;
Var
  S: String;
Begin
  If (BufferBytesLeft <= 0) Then
  Begin
    flush;
    loadbuffer;
  End;
  SetLength(S, BufferSize);
  Move(PChar(Buffer)^, S[1], BufferSize);
  Result := S;
End;

function TBZCustomBufferedStream.EOS : Boolean;
Begin
  Result := False;
  If ((StreamBytesLeft <= 0) And (BufferBytesLeft <= 0)) Then Result := True;
  //if result then GlobalLogger.LogWarning('EOS');
  //result:= GetStreamPosition>=StreamSize;
End;

function TBZCustomBufferedStream.GetStream : TStream;
begin
 // Stream.Seek(0,soBeginning);
  Result := Stream;
end;

(* Search
type  TPatternArray = Array of byte;

function TBZCustomBufferedStream.Search(Pattern: TPatternArray): Int64;
var
  idx: Integer;
begin
  Result := -1;
  for idx := FStream.Position to FStream.Size - Length(Pattern) do
  begin
    if CompareMem(FStream.Memory + Idx, @Pattern[0], Length(Pattern)) then exit(idx);
    FStream.LoadBuffer();
  end;
end;   *)



{%endregion%}

{%region%=====[ TBZBufferMapStream ]==========================================}
(*procedure TBZBufferMapStream.Flush;
begin
  FlushViewOfFile(FMemory, 0);
end;

function TBZBufferMapStream.Read(var Buffer; Count: Integer): Integer;
begin
  if FPosition + Count > FMaxSize then Count := FMaxSize - FPosition;
  move(FMemory[FPosition], Buffer, Count);
  Result := Count;
end;

function TBZBufferMapStream.Write(const Buffer; Count: Longint): Longint;
begin
  if FPosition + Count > FMaxSize then Count := FMaxSize - FPosition;
  move(Buffer, FMemory[FPosition], Count);
  Result := Count;
end;

function TBZBufferMapStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Case Origin of

  //   Seek from the beginning of the resource. The seek operation moves to a
  // specified position (offset), which must be greater than or equal to zero.
  soBeginning:
    if Offset < FMaxSize then
      FPosition := Offset
    else
      raise ERangeError.Create('Offset en dehors de la projection');

  //   Seek from the current position in the resource. The seek operation moves
  // to an offset from the current position (position + offset). The offset is
  // positive to move forward, negative to move backward.
  soCurrent  :
    if FPosition + Offset < FMaxSize then
      FPosition := FPosition + Offset
    else
      raise ERangeError.Create('Offset en dehors de la projection');

  //   Seek from the end of the resource. The seek operation moves to an offset
  // from the end of the resource, where the offset is expressed as a negative
  // value because it is moving toward the beginning of the resource  soEnd :
  soEnd :
    if FSize - Offset >= 0 then
      FPosition := FMaxSize - Offset
    else
      raise ERangeError.Create('Offset en dehors de la projection');
  end;

  result := FPosition;
end;

constructor TBZBufferMapStream.Create(FileName: string;
                                     MappingName: String;
                                     Mode: Word;
                                     Rights: Cardinal;
                                     Offset: Cardinal;
                                     Count: Cardinal;
                                     MaxSize: Cardinal;
                                     WriteCopy: Boolean = true);
var
    dwDA, dwSM, dwCD, flP, dwVA: DWORD;
    FileInfo: _BY_HANDLE_FILE_INFORMATION;
    pchName : pChar;
begin
// Initialise correctement les attributs de construction du mapping

  If Mode = fmOpenRead then begin
    dwDA := GENERIC_READ;
    dwCD := OPEN_EXISTING;
    dwVA := FILE_MAP_READ;
    flP  := PAGE_READONLY; end
  Else begin
    dwDA := GENERIC_READ or GENERIC_WRITE;
    If WriteCopy then begin
      dwVA := FILE_MAP_COPY;
      flP  := PAGE_WRITECOPY; end
    else begin
      dwVA := FILE_MAP_WRITE;
      flP  := PAGE_READWRITE;
    end;

    Case Mode of
      fmCreate:        dwCD := CREATE_ALWAYS;
      fmOpenWrite:     dwCD := TRUNCATE_EXISTING;
      fmOpenReadWrite: dwCD := OPEN_EXISTING;
    end;
  end;

  case Rights of
    fmShareCompat or fmShareExclusive: dwSM := 0;
    fmShareDenyWrite: dwSM := FILE_SHARE_READ;
    fmShareDenyRead: dwSM := FILE_SHARE_WRITE;
    fmShareDenyNone: dwSM := FILE_SHARE_READ and FILE_SHARE_WRITE;
  end;

// Verife si le mapping a créer est nomé.
  if MappingName <> '' then begin
//   Le cas échéant, essayons dans un premier temps d'ouvrir un mapping existant
    pchName := pChar(MappingName);
    hMapping := OpenFileMapping(dwDA, false, pchName); end
  else begin
//   Si non, RAZ de pchName et hMapping
    pchName := nil;
    hMapping := 0;
  end;

  inherited create;

// Si le mapping n'est pas déjà ouvert
  If hMapping = 0 then begin
//   Ouvre le fichier
    FHandle := CreateFile(pChar(FileName), dwDA, dwSM, nil, dwCD, 0, 0);
    if FHandle = INVALID_HANDLE_VALUE then raise Exception.Create(SysErrorMessage(GetLastError));

    if MaxSize = 0 then begin
      if not GetFileInformationByHandle(FHandle, FileInfo) then begin
        CloseHandle(FHandle);
        raise Exception.Create(SysErrorMessage(GetLastError));
      end;
      FMapSize := FileInfo.nFileSizeLow; end
    else
      FMapSize := MaxSize;

    hMapping := CreateFileMapping(FHandle, nil, flP, 0, FSize, pchName);
    if (hMapping = INVALID_HANDLE_VALUE) or (hMapping = 0) then begin
      FileClose(FHandle);
      raise Exception.Create(SysErrorMessage(GetLastError));
    end;

  end;

  FMemory := MapViewOfFile(hMapping, dwVA, 0, Offset, Count);
  if FMemory = nil then
  begin
    FileClose(FHandle);
    CloseHandle(hMapping);
    raise Exception.Create(SysErrorMessage(GetLastError));
  end;

end;

destructor TBZBufferMapStream.Destroy;
begin
  Flush;
  UnMapViewOfFile(FMemory);
  CloseHandle(hMapping);
  CloseHandle(FHandle);

  inherited destroy;
end;*)

{%endregion%}

{%region%=====[ TBZBufferedFileMapStream ]====================================}
{%endregion%}

{%region%=====[ TBZStream ]===================================================}
{%endregion%}

{%region%=====[ TBZZLibStream ]===============================================}
Constructor TBZZLibStream.Create(stream: Tstream);

Begin
  assert(stream <> nil);
  Inherited Create(stream);
  FBuffer := nil;
  GetMem(FBuffer, cGZBufsize);
End;

Procedure TBZZLibStream.Progress(Sender: TObject);

Begin
  If FOnProgress <> nil Then
    FOnProgress(Sender);
End;

Destructor TBZZLibStream.Destroy;

Begin
  If FWriteMode Then
    inflateEnd(FZStream)
  Else
  Begin
    Try
      Flush;
    Finally
      deflateEnd(FZStream);

    End;
  End;
  FreeMem(FBuffer);
  FBuffer := nil;
  Inherited Destroy;
End;

{***************************************************************************}

Constructor TBZZLibStream.Create(level: TGZCompressionLevel; Dest: TStream; Askipheader: Boolean = False);
Var
  err, l: Smallint;
Begin
  Inherited Create(dest);
  FZStream.next_out := Fbuffer;
  FZStream.avail_out := cGZBufsize;

  Case level Of
    clnone:
      l := Z_NO_COMPRESSION;
    clfastest:
      l := Z_BEST_SPEED;
    cldefault:
      l := Z_DEFAULT_COMPRESSION;
    clmax:
      l := Z_BEST_COMPRESSION;
  End;

  If Askipheader Then
    err := deflateInit2(FZStream, l, Z_DEFLATED, -MAX_WBITS, DEF_MEM_LEVEL, 0)
  Else
    err := deflateInit(FZStream, l);
  If err <> Z_OK Then
    Raise EGZCompressionError.Create(zerror(err));
  FWriteMode := True;
End;


Function TBZZLibStream.GetAvailableInput: Integer;
Begin
  Result := FZStream.avail_in;
End;

//----------------------------------------------------------------------------------------------------------------------

Function TBZZLibStream.GetAvailableOutput: Integer;
Begin
  Result := FZStream.avail_out;
End;

Function TBZZLibStream.Write(Const buffer; Count: Longint): Longint;
Var
  err: Smallint;
  lastavail, written: Longint;
Begin
  FZStream.next_in := @buffer;
  FZStream.avail_in := Count;
  lastavail := Count;
  While FZStream.avail_in <> 0 Do
  Begin
    If FZStream.avail_out = 0 Then
    Begin
      { Flush the buffer to the stream and update progress }
      written := Source.Write(Fbuffer^, cGZBufsize);
      Inc(compressed_written, written);
      Inc(raw_written, lastavail - FZStream.avail_in);
      lastavail := FZStream.avail_in;
      progress(self);
      { reset output buffer }
      FZStream.next_out := Fbuffer;
      FZStream.avail_out := cGZBufsize;
    End;
    err := deflate(FZStream, Z_NO_FLUSH);
    If err <> Z_OK Then
      Raise EGZCompressionError.Create(zerror(err));
  End;
  Inc(raw_written, lastavail - FZStream.avail_in);
  Write := Count;
End;

Function TBZZLibStream.Get_CompressionWriteRate: Single;
Begin
  Result := 100 * compressed_written / raw_written;
End;


Procedure TBZZLibStream.Flush;
Var
  err:     Smallint;
  written: Longint;
Begin
  {Compress remaining data still in internal zlib data buffers.}
  Repeat
    If FZStream.avail_out = 0 Then
    Begin
      { Flush the buffer to the stream and update progress }
      written := Source.Write(Fbuffer^, cGZBufsize);
      Inc(compressed_written, written);
      progress(self);
      { reset output buffer }
      FZStream.next_out := Fbuffer;
      FZStream.avail_out := cGZBufsize;
    End;
    err := deflate(FZStream, Z_FINISH);
    If err = Z_STREAM_END Then
      break;
    If (err <> Z_OK) Then
      Raise EGZCompressionError.Create(zerror(err));
  Until False;
  If FZStream.avail_out < cGZBufsize Then
  Begin
    Source.writebuffer(FBuffer^, cGZBufsize - FZStream.avail_out);
    Inc(compressed_written, cGZBufsize - FZStream.avail_out);
    progress(self);
  End;
End;


{***************************************************************************}

Constructor TBZZLibStream.Create(Asource: TStream; Askipheader: Boolean = False);
Var
  err: Smallint;
Begin
  Inherited Create(Asource);

  skipheader := Askipheader;
  If Askipheader Then
    err := inflateInit2(FZStream, -MAX_WBITS)
  Else
    err := inflateInit(FZStream);
  If err <> Z_OK Then
    Raise EGZCompressionError.Create(zerror(err));
  FWriteMode := False;
End;

Function TBZZLibStream.Read(Var buffer; Count: Longint): Longint;
Var
  err: Smallint;
  lastavail: Longint;
Begin
  FZStream.next_out := @buffer;
  FZStream.avail_out := Count;
  lastavail := Count;
  While FZStream.avail_out <> 0 Do
  Begin
    If FZStream.avail_in = 0 Then
    Begin
      {Refill the buffer.}
      FZStream.next_in := Fbuffer;
      FZStream.avail_in := Source.Read(Fbuffer^, cGZBufsize);
      Inc(compressed_read, FZStream.avail_in);
      Inc(raw_read, lastavail - FZStream.avail_out);
      lastavail := FZStream.avail_out;
      progress(self);
    End;
    err := inflate(FZStream, Z_NO_FLUSH);
    If err = Z_STREAM_END Then
      break;
    If err <> Z_OK Then
      Raise EGZDecompressionError.Create(zerror(err));
  End;
  If err = Z_STREAM_END Then
    Dec(compressed_read, FZStream.avail_in);
  Inc(raw_read, lastavail - FZStream.avail_out);
  Read := Count - FZStream.avail_out;
End;

Procedure TBZZLibStream.reset;
Var
  err: Smallint;
Begin
  Source.seek(-compressed_read, sofromcurrent);
  raw_read := 0;
  compressed_read := 0;
  inflateEnd(FZStream);
  If skipheader Then
    err := inflateInit2(FZStream, -MAX_WBITS)
  Else
    err := inflateInit(FZStream);
  If err <> Z_OK Then
    Raise EGZDecompressionError.Create(zerror(err));
End;

Function TBZZLibStream.GetPosition(): Int64;
Begin
  Result := raw_read;
End;

Function TBZZLibStream.Seek(Const Offset: Int64; Origin: TSeekOrigin): Int64;
Var
  c, off: Int64;
Begin
  off := Offset;

  If origin = soCurrent Then
    Inc(off, raw_read);
  If (origin = soEnd) Or (off < 0) Then
    Raise EGZDecompressionError.Create('Seek in deflate compressed stream failed.');

  seek := off;

  If off < raw_read Then
    reset
  Else
    Dec(off, raw_read);

  While off > 0 Do
  Begin
    c := off;
    If c > cGZBufsize Then
      c := cGZBufsize;
    If Read(Fbuffer^, c) <> c Then
      Raise EGZDecompressionError.Create('Seek in deflate compressed stream failed.');
    Dec(off, c);
  End;
End;

Function TBZZLibStream.Get_CompressionReadRate: Single;

Begin
  Result := 100 * compressed_read / raw_read;
End;


{%endregion%}



End.
