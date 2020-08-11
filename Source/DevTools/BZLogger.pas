(*
  @abstract(Contient une classe TBZLogger servant à générer des fichiers logs
  simplement. Elle utilise un Thread pour ecrire les messages en arrière-plan.)

  Vous pouvez ajoutez des sorties supplémentaires en héritant la classe TBZCustomLoggerWriter. @br
  Ensuite il vous suffira de déclarer les nouvelles classes en les ajoutant au TBZLogger
  grace à la commande AddWriter(aLogWriter : TBZCustomLoggerWriter); @br
  Vous pouvez accéder aux "Writer" via la propriété LogWriters.Items[]

  Par défaut 3 "writers" sont initialisés : @br
  @unorderedList(
    @item(LogWriters.Items[0] = TBZFileLoggerWriter : Ecriture dans un fichier)
    @item(LogWriters.Items[1] = TBZCallBackLoggerWriter : Ecriture dans la fenêtre de log)
    @item(LogWriters.Items[2] = TBZCallBackLoggerWriter : Ecriture personnalisé)
  )

  L'écriture du log est intercepté par l'évènement OnCallBack du Logger.

  Pour afficher ou cacher la fenêtre de log incorporée ; utilisez les commandes : @br
  "ShowLogView" et "HideLogView" @br

  Voir l'exemple dans : Demos\Basics\Logger


  -------------------------------------------------------------------------------------------------------------

  @created(24/02/2019)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(Creation : 24/02/2019)
    @item(Mise à jour : )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes :)@br
  En plus vous disposer d'un "Writer", qui écrit le log dans une console (Terminal). @br
  Celui-ci supporte différentes couleurs prédéfinies en fonction du type de message. @br
  @br
  Pour l'ajouter : @br
  @longCode(#
  Var
    ConsoleLoggerWriter : TBZConsoleLoggerWriter;
  begin
    ConsoleLoggerWriter := TBZConsoleLoggerWriter.Create(GlobalLogger);
    GlobalLogger.LogWriters.AddWriter(ConsoleLoggerWriter);
  end; #)

  @unorderedlist(
    @item(Le logger protège les écritures des log par des sections critique. Il est donc "Thread Safe". )
    @item(Les "Writers" ajoutés, sont automatiquement libérés de la mémoire par le Logger. )
    @item(Un objet nommé "GlobalLogger" de TBZLogger est directement initialisé et accessible
    @item(Pour utiliser la sortie console allez dans les options de votre projet @br
          --> Options du compilateur --> Configuration et cible et décochez l'option : @br )
              "Application graphique W32 (-WG)" et bien sure ajoutez le "Writer". )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Dependances) : BZSystem, BZClasses, BZTypesHelpers, uViewLoggerForm

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)@br
  @unorderedList(
    @item(J.Delauney (BeanzMaster))
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZLogger;

//------------------------------------------------------------------------------
//----------------------------[ TODO LIST ]-------------------------------------

{ TODO -oBZLogger : Corriger le bug avec la fenêtre de log, celle-ci bloque et gèle l'application }

//------------------------------------------------------------------------------



//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils,
  BZSystem, BZClasses,  uViewLoggerForm;

//==============================================================================
type
  { Niveau du message de log }
  TBZLogLevel = (llNone, llNotice, llStatus, llHint, llWarn, llException, llError);
  { Procedure de callback pour ecrire un message de log }
  TBZLoggerCallback = procedure ( aLevel: TBZLogLevel;  aTimeStamp : TDateTime; const aMessage, ParseMsg : String) of object;

  { Gestion du nom du fichier journal.
    Et servira à stocker temporairement les noms de fichiers existants }
  FlName = record
    fullName: TFileName;
    ext3: string;
    isGdg: boolean;
  end;
  PFlName = ^FlName;

const
  { Nom par defaut du fichier journal }
  cDefaultLogFileName = 'DebugLog';
  { Niveau en chaine de caractère }
  cLogLevelName    : array[TBZLogLevel] of String = ( 'NONE', 'NOTICE', 'STATUS', 'HINT', 'WARNING', 'EXCEPTION', 'ERROR');
  { Numéro des couleurs pour la sortie console des messages en fonction du niveau de ceux-ci }
  cLogLevelColor   : array[TBZLogLevel] of Byte = (15,10,15,11,14,12,12);

type
  { Représentation d'un élément de log dans la liste }
  TBZLoggerItem = Class
    Level : TBZLogLevel;
    TimeStamp : TDateTime;
    Msg : String;
  end;

  { Compteurs pour les différents niveaux des messages }
  TBZLoggerCounter = packed record
    Hints : LongWord;
    Warnings : LongWord;
    Errors : LongWord;
    Exceptions : LongWord;
  end;

  { Classe à hériter pour ajouter un nouveau moteur d'écriture des messages }
  TBZCustomLoggerWriter = Class(TBZThreadAbleObject)
  protected
    FOwnerThread : TThread;
    FEnabled : Boolean;


    function ParseLog(LogLevel : TBZLogLevel; aTimeStamp : TDateTime; aMsg : String) : String;
    procedure DoWriteLog(LogLevel : TBZLogLevel; aTimeStamp : TDateTime; aMsg : String); virtual;
  public
    { Création }
    Constructor Create(OwnerThread : TThread); overload; virtual;
    { Destruction }
    Destructor Destroy; override;
    { Ecrit un log }
    procedure WriteLog(LogLevel : TBZLogLevel; aTimeStamp : TDateTime; aMsg : String);
    { Activation / Désactivation }
    property Enabled : Boolean read FEnabled write FEnabled;
  end;

  { Classe de gestion du nom du fichier journal, définition du mode d'accès
    et paramètres pour la générations des noms de fichiers. }
  TBZLogFileDef = class
  private
    //fields
    FPath, FDdn,
    FExt,
    FLFN: string;
    FGDGMax: word;
    FSafeGDGMax: word;
    FDoAppend: boolean;
    FBuild: boolean;
    FUseSizeLimit: boolean;
    FLogFileMaxSize: integer;
    FUseSafeFileNames: boolean;
    FCreated: boolean;

    procedure SetPath(Value: string);
    procedure SetExt(Value: string);
    procedure SetFileName(lfn: TFileName);
    procedure SetDoAppend(Value: boolean);
    procedure SetGdgMax(Value: word);
    function BuildFileName: TFileName;
    function GetGdg(sL: TList): string;
    function GetFileName: TFileName;
    function GetOpenMode: word;
    function FileInUse(f: string): boolean;
  public
    { Coréation de la classe }
    constructor Create;
    { Destruction de la classe }
    destructor Destroy; override;

    { Génération d'un nom de fichier sécurisé }
    procedure BuildSafeFileName;

    { Nom du fichiers sans extension }
    property ddname: string read FDdn write FDdn;
    { Chemin d'accès aux fichiers }
    property path: string read FPath write SetPath;
    { Extension du nom de fichier. '.log' par défaut si non GDG. }
    property ext: string read FExt write SetExt;
    { Nom du fichier journal }
    property fileName: TFileName read GetFileName write SetFileName;
    { Le journal doit-il être vidé ou non ? , si il existe. Ignoré si FGDGMax > 0 }
    property append: boolean read FDoAppend write SetDoAppend;
    { Drapeau si le fichier existe déja }
    property Created: boolean read FCreated write FCreated default False;
    { Nombre max de fichier à générer et conserver. (0 < GDG < 999 / 0 = pas de compteur) }
    property gdgMax: word read FGDGMax write SetGdgMax;
    { Nombre max de fichier à générer et conserver, sécurisé. (0 < GDG < 999 / 0 = pas de compteur) }
    property SafegdgMax: word read FSafeGDGMax write fSafeGDGMax;
    { Type du mode d'acces du fichier journal en cours d'utilisation }
    property OpMode: word read GetOpenMode;
    { Fermer le fichier journal et en démarrer un nouveau lorsque la taille limite est dépassée ? }
    property UseFileSizeLimit: boolean read FUseSizeLimit write FUseSizeLimit;
    { taille maximale du fichier journal utilisé pour un fonctionnement en continu }
    property LogFileMaxSize: integer read FLogFileMaxSize write FLogFileMaxSize;
    { Utiliser des noms de fichier unique pour chaque journal }
    property UseSafeFilenames: boolean read FUseSafeFileNames write FUseSafeFileNames;
  end;

  { Moteur d'écriture des messages dans un fichier }
  TBZFileLoggerWriter = Class(TBZCustomLoggerWriter)
  private
    FLogFileDef: TBZLogFileDef;
    FLogStream: THandleStream;
  protected
    function GetLogFileName: string;

    procedure DoWriteLog(LogLevel : TBZLogLevel; aTimeStamp : TDateTime; aMsg : String); override;
  public
    { Création }
    Constructor Create(OwnerThread : TThread); override;
    { Destruction }
    Destructor Destroy; override;

    { Propriété pour la génération du nom de fichier }
    property LogFileDef: TBZLogFileDef read FLogFileDef;
    { Nom du fichier log }
    property LogFileName: string read GetLogFileName;
  end;

  { Moteur d'écriture des messages personnalisé. @br
    cf dans TBZLogger l'évènement OnCallBack}
  TBZCallBackLoggerWriter = Class(TBZCustomLoggerWriter)
  private
    FOnCallBack : TBZLoggerCallback;
  protected

    procedure DoWriteLog(LogLevel : TBZLogLevel; aTimeStamp : TDateTime; aMsg : String); override;
  public
    { Création }
    Constructor Create(OwnerThread : TThread); override;
    { Destruction }
    Destructor Destroy; override;
    { Evènement levé pour ecrire un log personnalisé }
    property OnCallBack : TBZLoggerCallback read FOnCallBack write FOnCallBack;
  end;


  TBZLogViewMsgData = record
    Msg : string;
  end;
  PBZLogViewMsgData = ^TBZLogViewMsgData;
  { Moteur d'écriture des message dans la fenêtre dédiée. @br
    cf dans TBZLogger les méthodes "ShowLogView" et "HideLogView"}
  TBZLogViewLoggerWriter = Class(TBZCustomLoggerWriter)
  private
    FViewLoggerForm : TViewLoggerForm;
    FParseMsg : String;
  protected
    procedure SynchronizeWriteLog(Data: PtrInt);
    procedure DoWriteLog(LogLevel : TBZLogLevel; aTimeStamp : TDateTime; aMsg : String); override;
  public
    { Création }
    Constructor Create(OwnerThread : TThread); override;
    { Destruction }
    Destructor Destroy; override;

    { Fenêtre de visionnage du log }
    property ViewLoggerForm : TViewLoggerForm read FViewLoggerForm;
  end;

  { TBZConsoleLoggerWriter : Moteur d'écriture des messages dans la console. @br
    Adapté de  : @br
      @author(Kornel Kisielewicz <epyon@chaosforge.org>) @br
      @link(https://github.com/chaosforgeorg/fpcvalkyrie) }
  TBZConsoleLoggerWriter = Class(TBZCustomLoggerWriter)
  private
     FColorEnabled : Boolean;
  protected
    procedure SetColor(aColor: byte);
    procedure DoWriteLog(LogLevel : TBZLogLevel; aTimeStamp : TDateTime; aMsg : String); override;
  public
    { Création }
    Constructor Create(OwnerThread : TThread); override;
    { Destruction }
    Destructor Destroy; override;
  end;

  { Liste pour enregistrer les différents moteur d'écriture des messages disponibles. }
  TBZLoggerWriterList = Class(TBZPersistentObjectList)
  private
    Function GetLoggerWriterItem(index: Integer): TBZCustomLoggerWriter;
    Procedure SetLoggerWriterItem(index: Integer; val: TBZCustomLoggerWriter);
  protected
  public
    { Création }
    Constructor Create; Override;
    { Destruction }
    Destructor Destroy; Override;

    //procedure Assign(source: TPersistent); override;

    { Efface la liste de Bitmaps Brute}
    Procedure Clear; Override;
    { Ajoute une image à la liste}
    Function AddWriter(Const aLayerItem: TBZCustomLoggerWriter): Integer; Overload;
    { Ajoute une nouvelle image vide }
    Function AddWriter : TBZCustomLoggerWriter;

    { Acces aux  éléments de la liste }
    Property Items[Index: Integer]: TBZCustomLoggerWriter read GetLoggerWriterItem write setLoggerWriterItem;
  end;

  {  Classe spécialisée simple pour écrire des logs }

  { TBZLogger }

  TBZLogger = class(TThread)
  private
    FEnabled : Boolean;
   // FCriticalSection : SyncObjs.TCriticalSection;
    FHandleApplicationException : Boolean;
    //FOldExceptProc : TExceptProc;
    FLogEnd, FLogSystemInfos : Boolean;
    FLogWriters : TBZLoggerWriterList;
    FQueue : TThreadList;
    FCurrentItem  : TBZLoggerItem;

    FOnCallBack : TBZLoggerCallback;
    FLoggerCounter : TBZLoggerCounter;
    FLogViewEnabled : Boolean;

    procedure SetOnCallBack(const AValue : TBZLoggerCallback);
    procedure SetHandleApplicationException(const AValue : Boolean);

    procedure WriteToLog;
    procedure SetLogViewEnabled(const AValue : Boolean);
  protected

    procedure Execute; override;

    procedure AddLog(LogLevel : TBZLogLevel; aTimeStamp : TDateTime; aMsg : String);
    procedure WriteLogItem(aItem : TBZLoggerItem);

    procedure DumpExceptionCallStack(E: Exception);

  public
    constructor Create; overload;
    constructor Create( CreateSuspended : Boolean ); overload;
    destructor Destroy; override;

    { Ecrit un message de type "erreur" dans le journal }
    procedure LogError(ErrorMessage: string);
    { Ecrit un message de type "avertissement" dans le journal }
    procedure LogWarning(WarningMessage: string);
    { Ecrit un message de type "Statut" dans le journal }
    procedure LogStatus(StatusMessage: string);
    { Ecrit un message de type "Notification" dans le journal }
    procedure LogNotice(NoticeMessage: string);
    { Ecrit un message de type "conseil" dans le journal }
    procedure LogHint(HintMessage: string);
    { Ecrit un message de type "Exception" dans le journal }
    procedure LogException(ExceptionMessage: string);
    { Ecrit un message simple dans le journal }
    procedure Log(aMsg: string); //None

    { Ecrit des informations, concernant le projet et le système ou celui-ci est lancé }
    procedure LogSystemInfos;
    { Ecrit les données de fin du log }
    procedure LogEnd;

    procedure CatchException(Sender: TObject; E: Exception);
    //procedure ExceptionThreadHandler(aThread : TThread; E : Exception);

    { Affiche la fenêtre de log interne }
    procedure ShowLogView;
    { Cache la fenêtre de log interne }
    procedure HideLogView;

    { Evènement pour intercepter les messages }
    property OnCallBack : TBZLoggerCallback read FOnCallBack write SetOnCallBack;
    { Compteurs des messages en fonction des niveaux }
    property LoggerCounter : TBZLoggerCounter read FLoggerCounter;
    { Liste des moteurs d'écriture attachés au TBZLogger }
    property LogWriters : TBZLoggerWriterList read FLogWriters;
    { Active/désactive l'interception des exceptions dans l'application en cours }
    property HandleApplicationException : Boolean read FHandleApplicationException write SetHandleApplicationException;
    { Active / désactive la sortie du log dans la fenêtre dédiée }
    property LogViewEnabled : Boolean read FLogViewEnabled write SetLogViewEnabled;
  end;

//==============================================================================

var
  GlobalLogger: TBZLogger;  //< Variable globale d'acces a un logger automatiquement créé et détruit.

//==============================================================================

implementation

//==============================================================================

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  Forms,{ Crt,} LCLVersion,
  BZTypesHelpers;


function CompareGdgNames(p1, p2: Pointer): integer;
begin
  Result := CompareText(PFlName(p1)^.ext3, PFlName(p2)^.ext3);
end;


{%region%=====[ TBZCustomLoggerWriter ]==========================================}

Constructor TBZCustomLoggerWriter.Create(OwnerThread : TThread);
begin
  inherited Create;
  FOwnerThread := OwnerThread;
 // FCriticalSection := SyncObjs.TCriticalSection.Create;
  FEnabled := False;
end;

Destructor TBZCustomLoggerWriter.Destroy;
begin
  //FreeAndNil(FCriticalSection);
  inherited Destroy;
end;

function TBZCustomLoggerWriter.ParseLog(LogLevel : TBZLogLevel; aTimeStamp : TDateTime; aMsg : String) : String;
begin
  if aMsg = '' then
    Result := '' //#13#10
  else
  begin
    //'['+ {$I %FILE"NAME"%} + ']' + '[Routine = '+ {$I %CURRENTROUTINE%} + ' - Ligne = '+ {$I %LINE%} +']
    Case LogLevel of
      llNone : Result := aMsg;
      llStatus, llHint : Result := '[' + cLogLevelName[LogLevel] + '] : ' + aMsg;
      llNotice, llWarn,
      llException, llError : Result := '[' + FormatDateTime('hh:nn:ss',aTimeStamp) + '][' + cLogLevelName[LogLevel] + '] : ' + aMsg; //'hh:nn:ss.zzz'
    end;
    //Result := Result + #13#10;
  end;
end;

procedure TBZCustomLoggerWriter.DoWriteLog(LogLevel : TBZLogLevel; aTimeStamp : TDateTime; aMsg : String);
begin
  // Ne fait rien
end;

procedure TBZCustomLoggerWriter.WriteLog(LogLevel : TBZLogLevel; aTimeStamp : TDateTime; aMsg : String);
begin
  if not(FEnabled) then Exit;
  Lock;
  DoWriteLog(LogLevel, aTimeStamp, aMsg);
  UnLock;
end;

{%endregion%}

{%region%=====[ TBZLogFileDef ]==================================================}

constructor TBZLogFileDef.Create;
begin
  inherited;
  Path := GetApplicationPath; //ExtractFilePath(Application.ExeName);
  FDdn := ChangeFileExt(GetApplicationFileName, '');
  //ChangeFileExt(ExtractFileName(Application.ExeName),'');
  FExt := '.log';
  FGDGMax := 0;
  // as long as an ext is provided, no gdg is managed
  FDoAppend := False; // if exists, will be overrided
  FBuild := False;

  SafegdgMax := 0;
  UseFileSizeLimit := False;
  LogFileMaxSize := 0;
  UseSafeFilenames := True;

end;

destructor TBZLogFileDef.Destroy;
begin
  inherited;
end;

function TBZLogFileDef.GetFileName: TFileName;
begin
  if (FGDGMax = 0) or (Self.FBuild) then
    Result := FixPathdelimiter(FPath) + RemoveFileNamePathDelimiter(FDdn) + FExt
  else
    Result := BuildFileName;
end;

procedure TBZLogFileDef.SetFileName(lfn: TFileName);
//  Sets the full log name
begin
  if lfn <> '' then
  begin
    FPath := ExtractFilePath(lfn);                    // if not provided...
    if FPath <> '' then SetPath(FPath);               // ... then current path is kept
    FDdn := ChangeFileExt(ExtractFileName(lfn), ''); // Vittorio Loschi
    if FDdn = '' then FDdn := cDefaultLogFileName;            // '' is forbidden;
    FExt := ExtractFileExt(lfn);
    if FExt <> '' then FGDGMax := 0;
    // If provided, THLFileDef is no longer supposed to manage GDG
  end;
end;

procedure TBZLogFileDef.SetPath(Value: string);
begin
  if Value <> '' then                                // Otherwise unchanged.
  begin
    if Value[length(Value)] <> '\' then
      Value := Value + '\';
    self.FPath := Value;
  end;
end;

procedure TBZLogFileDef.SetExt(Value: string);
begin
  if Value[1] <> '.' then Value := '.' + Value;
  Self.FExt := Value;                                // empty allowed
  Self.GdgMax := 0;                                  // Gdg no longer managed
end;

procedure TBZLogFileDef.SetDoAppend(Value: boolean);
begin
  Self.FDoAppend := Value;
  if Value then Self.FGDGMax := 0;                   // Gdg no longer managed
end;

procedure TBZLogFileDef.SetGdgMax(Value: word);
begin
  if Value > 999 then Self.FGDGMax := 999
  else Self.FGDGMax := Value;
  if Value > 0 then Self.FDoAppend := False;
end;

function TBZLogFileDef.GetOpenMode: word;
  // v 1.1
begin
  if FCreated then
  begin
    Result := fmOpenReadWrite or fmShareDenyWrite;
  end
  else
  begin
    if FDoAppend then
    begin
      if FileExists(Self.fileName) then
        Result := fmOpenReadWrite or fmShareDenyWrite
      else
        Result := fmCreate or fmShareDenyWrite;
    end
    else
      Result := fmCreate or fmShareDenyWrite;
  end;
end;

function TBZLogFileDef.BuildFileName: TFileName;
  // Called only if gdg extention has to be managed.
  // Returns the long file name of the new log.
var
  sRec: SysUtils.TSearchRec;
  lstFlNames: TList;
  sTmp: string;
  aGdgRec: PFlName;
begin
  // First populate a list of potential "on disk"
  // log files (extention temporary ingnored)
  lstFlNames := TList.Create;
  // allways created, to avoid compiler warning
  try {Finally}
    try {Except}
      sTmp := FixPathDelimiter(self.path) + RemoveFileNamePathDelimiter(self.ddname) + '.*';
      if SysUtils.FindFirst(sTmp, faAnyfile, sRec) = 0 then
      begin
        new(aGdgRec);
        aGdgRec^.fullName := self.path + sRec.Name;
        aGdgRec^.isGdg := False;
        // will be tested later, if needed
        lstFlNames.Add(aGdgRec);
        while (SysUtils.FindNext(sRec) = 0) do
        begin
          new(aGdgRec);
          aGdgRec^.fullName := FixPathDelimiter(self.path) + RemoveFileNamePathDelimiter(sRec.Name);
          aGdgRec^.isGdg := False;
          lstFlNames.Add(aGdgRec);
        end;
        SysUtils.FindClose(sRec);

        Result := FixPathDelimiter(self.path) + RemoveFileNamePathDelimiter(self.ddname) + GetGdg(lstFlNames);
        // GetGdg wil return the new generation number
      end
      else
        Result := FixPathDelimiter(path) + RemoveFileNamePathDelimiter(ddname) + '.001';
      // No fileName actually looks like the one wanted
    except
      Result := RemoveFileNamePathDelimiter(self.ddname) + '.~og';
    end;

  finally                                                     // Cleanup list items then the list
    try
      FBuild := True;
      FPath := FixPathDelimiter(ExtractFilePath(Result));
      FDdn := ChangeFileExt(RemoveFileNamePathDelimiter(ExtractFileName(Result)), '');
      FExt := ExtractFileExt(Result);
      FLFN := Result;
      while lstFlNames.Count > 0 do
      begin
        if assigned(lstFlNames[0]) then
        begin
          aGdgRec := lstFlNames.Items[0];
          Dispose(aGdgRec);
        end;
        lstFlNames.Delete(0);
      end;
      lstFlNames.Free;
    except;
    end;
  end;
end;

function TBZLogFileDef.GetGdg(sL: TList): string;

  // Called only if at least one file still exists, with a "gdg looking like"
  // filename. Sets the generation number of a gdg file.
  // (".001" < gdg < "."gdgmax ). Lists all gdg present with the ddn HLFileDef.ddname,
  // and keeps only the gdgMax -1 most recent (highest gdg) ones, & renames them
  // (001..gdgMax-1). Then returns the new gdg extention. NOT(FileExists)  and
  // NOT(InUse) newFileName are controlled too.

var
  gTmp, i: integer;
  sTmp: string;
  Succes: boolean;
  aGdgRec: PFlName;
begin
  try
    for i := 0 to sL.Count - 1 do
    begin
      sTmp := RightStr(ExtractFileExt(PFlName(sL.items[i])^.fullName), 3);
      try
        StrToInt(sTmp);
        PFlName(sL.items[i])^.ext3 := sTmp;
        // PROGRAMMERS, if an ex raises : Keep cool !...
        PFlName(sL.items[i])^.isGdg := True;
        // ...EConvertError wonn't appear at run time
      except
        aGdgRec := PFlName(sL.items[i]);
        Dispose(aGdgRec);
        // not a gdg file name...
        sL.items[i] := nil;
        // ...then, remove it
      end;
    end;

    {pack will apply the deletions, if any happened}
    sL.Pack;
    // sL now contains gdg fileNames only.
    if sL.Count = 0 then
    begin
      Result := '.001';
      Exit;
    end;

    if sL.Count > 1 then sL.Sort(@CompareGdgNames);                      // let's sort them ascending
    for i := 0 to sl.Count - self.gdgMax do
    begin
      if SysUtils.DeleteFile(PFlName(sL.items[i])^.fullName) then sL.Items[i] := nil;
      // If unable to delete... keep that generation
    end;

    sl.Pack;
    Succes := True;
    for i := 0 to sl.Count - 1 do
      // Now tries to rename each kept file
    begin
      sTmp := IntToStr(i + 1);
      // generation numbers, starting from "001"
      while Length(sTmp) < 3 do sTmp := '0' + sTmp;
      sTmp := ChangeFileExt(PFlName(sL.items[i])^.fullName, '.' + sTmp);
      if not (RenameFile(PFlName(sL.items[i])^.fullName, sTmp)) then succes := False;
    end;

    if succes then
      gTmp := sl.Count + 1
    else
    begin
      sTmp := RightStr(ExtractFileExt(PFlName(sL.items[sl.Count])^.fullName), 3);
      try
        gTmp := StrToInt(sTmp) + 1;
        // will try to assign the new generation a number
      except
        gTmp := 999;                                        // higher than the one soon present.
      end;
    end;

    Result := IntToStr(gTmp);
    while Length(Result) < 3 do Result := '0' + Result;
    Result := '.' + Result;
    sTmp := ChangeFileExt(PFlName(sL.items[0])^.fullName, Result);
    if not (Succes) or FileExists(sTmp) or FileInUse(sTmp) then
      Result := '.~og';
  except
    Result := '.~og';
  end;
end;

function TBZLogFileDEf.FileInUse(f: string): boolean;
var
  hF: THandle;
begin
  Result := False;
  if not FileExists(f) then exit;
  hF := FileCreate(f, fmOpenReadWrite);
  Result := (hF > 0);
  // If NOT Result Then CloseHandle(hF);
end;

procedure TBZLogFileDef.BuildSafeFileName;
(* extension is assumed to remain the default of "log" *)
begin
  ddname := FormatDateTime('yyyy-mm-dd hh.mm.ss', Now); //keep it at least semi-readable
end;

{%endregion%}

{%region%=====[ TBZFileLoggerWriter ]============================================}

Constructor TBZFileLoggerWriter.Create(OwnerThread : TThread);
begin
  inherited Create(OwnerThread);
  FLogFileDef := TBZLogFileDef.Create;
  FLogStream := TFileStream.Create(LogFileDef.fileName, LogFileDef.OpMode);
  FLogFileDef.Created := True;
end;

Destructor TBZFileLoggerWriter.Destroy;
begin
  FreeAndNil(FLogStream);
  FreeAndNil(FLogFileDef); //.Free;
  inherited Destroy;
end;

function TBZFileLoggerWriter.GetLogFileName : string;
begin
  Result := FLogFileDef.fileName;
end;

procedure TBZFileLoggerWriter.DoWriteLog(LogLevel : TBZLogLevel; aTimeStamp : TDateTime; aMsg : String);
Var
  Tmp : String;
begin
  try
    FLogStream.Seek(0, soEnd);
    tmp := ParseLog(LogLevel, aTimeStamp, aMsg);
    tmp := tmp + #13#10;
    FLogStream.WriteBuffer(PChar(tmp)^, Length(tmp));
  except
    On EInOutError do
    begin
      //FStarted := False;
      // Terminate;  // File can't be used;
    end;
    //else; // hides error and continues
  end;
  // (if it was not a FatalException...)
end;

{%endregion%}

{%region%=====[ TBZCallBackLoggerWriter ]========================================}

Constructor TBZCallBackLoggerWriter.Create(OwnerThread : TThread);
begin
  inherited Create(OwnerThread);
end;

Destructor TBZCallBackLoggerWriter.Destroy;
begin
  inherited Destroy;
end;

procedure TBZCallBackLoggerWriter.DoWriteLog(LogLevel : TBZLogLevel; aTimeStamp : TDateTime; aMsg : String);
Var
  Tmp : String;
begin
  if Assigned(FOnCallBack) then
  begin
    Tmp := ParseLog(LogLevel, aTimeStamp, aMsg);
    FOnCallBack(LogLevel, aTimeStamp, aMsg, Tmp);
  end;
end;

{%endregion%}

{%region%=====[ TBZLogViewLoggerWriter ]==========================================}

Constructor TBZLogViewLoggerWriter.Create(OwnerThread : TThread);
begin
  inherited Create(OwnerThread);
  FViewLoggerForm := TViewLoggerForm.Create(nil); //(Application);
end;

Destructor TBZLogViewLoggerWriter.Destroy;
begin
 // if Assigned(FViewLoggerForm) then FViewLoggerForm.Close;
  FEnabled := False;
  FreeAndNil(FViewLoggerForm);
  inherited Destroy;
end;

procedure TBZLogViewLoggerWriter.SynchronizeWriteLog(Data: PtrInt);
begin
  //
  //FViewLoggerForm.MemoLog.Lines.Add(FParseMsg);
  Try
    if FEnabled and (FViewLoggerForm <> nil) and (not Application.Terminated) then
    begin
      //FViewLoggerForm.MemoLog.Lines.BeginUpdate;
      FViewLoggerForm.MemoLog.Append(PBZLogViewMsgData(Data)^.Msg);
      //FViewLoggerForm.MemoLog.Lines.EndUpdate;
    end;
  finally
    Dispose(PBZLogViewMsgData(Data));
  end;
  // Hack pour se placer automatiquement à la dernière ligne d'un TMemo
  //FViewLoggerForm.MemoLog.SelStart:=Length(FViewLoggerForm.MemoLog.lines.Text)-1;
  //FViewLoggerForm.MemoLog.VertScrollBar.Position:=10000000;
end;

procedure TBZLogViewLoggerWriter.DoWriteLog(LogLevel : TBZLogLevel; aTimeStamp : TDateTime; aMsg : String);
var
  LogViewMsgData : PBZLogViewMsgData;
begin
  FParseMsg := ParseLog(LogLevel, aTimeStamp, aMsg);
  New(LogViewMsgData);
  LogViewMsgData^.Msg := FParseMsg;
  Application.QueueAsyncCall(@SynchronizeWriteLog, PtrInt(LogViewMsgData));


  //FViewLoggerForm.MemoLog.Append(FParseMsg);
  //SynchronizeWriteLog;
  //FOwnerThread.Synchronize(FOwnerThread, @SynchronizeWriteLog);
end;

{%endregion%}

{%region%=====[ TBZConsoleLoggerWriter ]=========================================}

Constructor TBZConsoleLoggerWriter.Create(OwnerThread : TThread);
begin
  inherited Create(OwnerThread);
  FColorEnabled := True;
  {$IFDEF WINDOWS}
  SetConsoleOutputCP(CP_UTF8);   // On doit passé la console en UTF-8 pour la prise en charge des caractères accentués
  {$ENDIF}
end;

Destructor TBZConsoleLoggerWriter.Destroy;
begin
  //FreeAndNil(FProcess);
  inherited Destroy;
end;

{$IFDEF WINDOWS}
procedure TBZConsoleLoggerWriter.SetColor(aColor: byte);
begin
  // La console de Lazarus est limité on doit faire appel a l'API de Windows directement
  // les méthodes de l'unité CRT ne fonctionne pas dans ce cas.
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), aColor);
end;
{$ELSE}
procedure TBZConsoleLoggerWriter.SetColor(aColor: byte);
begin
  // Pour les systèmles unix on encode directement les couleurs
  // Ne fonctionne pas chez moi sous Manjaro dans un Xterm et Konsole
  // https://stackoverflow.com/questions/287871/how-to-print-colored-text-in-terminal-in-python
  // https://askubuntu.com/questions/558280/changing-colour-of-text-and-background-of-terminal

  Case aColor of
    0: write('\033[30m ');   // Black
    1: write('\033[34m ');   // Blue
    2: write('\033[32m ');   // Green
    3: write('\033[36m ');   // Cyan
    4: write('\033[31m ');   // Red
    5: write('\033[35m ');   // Magenta
    6: write('\033[33m ');   // Brown / Yellow
    7: write('\033[37m ');   // LightGray
    8: write('\033[90m ');   // DarkGray
    9: write('\033[94m ');   // LightBlue
    10: write('\033[92m ');  // LightGreen
    11: write('\033[96m ');  // LightCyan
    12: write('\033[91m ');  // LightRed
    13: write('\033[95m ');  // LightMagenta
    14: write('\033[93m ');  // LightYellow
    15: write('\033[97m ');  // White
  end;
end;
{$ENDIF}

procedure TBZConsoleLoggerWriter.DoWriteLog(LogLevel : TBZLogLevel; aTimeStamp : TDateTime; aMsg : String);
var
  cs : String;
begin
  //if LogLevel = llException then exit;

  if FColorEnabled then SetColor(cLogLevelColor[LogLevel] )
  else SetColor(7);

  if LogLevel<>llNone then
  begin
    cs := '['+ TimeToStr(aTimeStamp) + ']';
    {$IFDEF UNIX}cs := cs + ' \033[0m';{$ENDIF}
    Write(cs);

    cs := '[' + cLogLevelName[LogLevel] +'] : ';
    {$IFDEF UNIX}cs := cs + ' \033[0m';{$ENDIF}
    Write(cs);
  end;
  cs := aMsg;
  {$IFDEF UNIX}cs := cs + ' \033[0m';{$ENDIF}
  Writeln(cs);
end;

{%endregion%}

{%region%=====[ TBZLoggerWriterList ]============================================}

Constructor TBZLoggerWriterList.Create;
begin
  inherited Create;
end;

Destructor TBZLoggerWriterList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

Function TBZLoggerWriterList.GetLoggerWriterItem(index : Integer) : TBZCustomLoggerWriter;
begin
  Result := TBZCustomLoggerWriter(Get(Index));
end;

Procedure TBZLoggerWriterList.SetLoggerWriterItem(index : Integer; val : TBZCustomLoggerWriter);
begin
  Put(Index, Val);
end;

Procedure TBZLoggerWriterList.Clear;
Var
  i:  Integer;
  pm: TBZCustomLoggerWriter;
Begin
  if Count < 1 then exit;
  For i := Count - 1 downTo 0 Do
  Begin
    pm := GetLoggerWriterItem(i);
    //If Assigned(pm) Then
    pm.Free;
  End;
  Inherited Clear;
end;

Function TBZLoggerWriterList.AddWriter(Const aLayerItem : TBZCustomLoggerWriter) : Integer;
begin
  Result := Add(aLayerItem);
end;

Function TBZLoggerWriterList.AddWriter : TBZCustomLoggerWriter;
Var
  anItem: TBZCustomLoggerWriter;
Begin
  anitem := TBZCustomLoggerWriter.Create;
  Add(anItem);
  Result:= Items[Self.Count-1];
End;

{%endregion%}

{%region%=====[ TBZLogger ]======================================================}

constructor TBZLogger.Create;
begin
  Create(False);
 // FCriticalSection := SyncObjs.TCriticalSection.Create;
end;

constructor TBZLogger.Create(CreateSuspended : Boolean);
Var
  aFileLoggerWriter : TBZFileLoggerWriter;
  aViewLoggerCallBackWriter : TBZLogViewLoggerWriter;
  aCallBackLoggerWriter : TBZCallBackLoggerWriter;
begin
  FreeOnTerminate := False;
  FLogEnd := False;
  FLogSystemInfos := False;
  //FEventLog := TEvent.Create(nil,false,true,'EVENTLOG');
  FQueue := TThreadList.Create;
  FLogWriters := TBZLoggerWriterList.Create;
  aFileLoggerWriter := TBZFileLoggerWriter.Create(Self);
  aFileLoggerWriter.LogFileDef.append := True;
  aCallBackLoggerWriter := TBZCallBackLoggerWriter.Create(Self);
  aViewLoggerCallBackWriter := TBZLogViewLoggerWriter.Create(Self);
  FLogWriters.AddWriter(aFileLoggerWriter);
  FLogWriters.AddWriter(aViewLoggerCallBackWriter);
  FLogWriters.AddWriter(aCallBackLoggerWriter);

  TBZFileLoggerWriter(FLogWriters.Items[0]).Enabled := True;
  TBZLogViewLoggerWriter(FLogWriters.Items[1]).Enabled := false;
  TBZCallBackLoggerWriter(FLogWriters.Items[2]).Enabled := False;

  FLoggerCounter.Hints := 0;
  FLoggerCounter.Warnings := 0;
  FLoggerCounter.Errors := 0;
  FLoggerCounter.Exceptions := 0;

  FEnabled := True;
  inherited Create(CreateSuspended);
end;

destructor TBZLogger.Destroy;
begin

  //if FHandleApplicationException then ExceptProc := FOldExceptProc;

  // On désactive le callback et le  "LogView"
  TBZLogViewLoggerWriter(FLogWriters.Items[1]).Enabled := False;
  TBZCallBackLoggerWriter(FLogWriters.Items[2]).OnCallBack := nil;
  TBZCallBackLoggerWriter(FLogWriters.Items[2]).Enabled := False;

  if Not(FLogEnd) then LogEnd;
  Sleep(3000); // petite pause de 3 secondes pour laisser le temps au derniers logs d'être écris
  //While FQueue.LockList.Count > 0 do
  //begin
  //  Sleep(0);
  //end;
  FQueue.Clear;
  FreeAndNil(FLogWriters);
  FreeAndNil(FQueue);
  //FreeAndNil(FCriticalSection);
  //FreeAndNil(FViewLoggerForm);  // La form a comme propriétaire Application, elle se libère toutes seule ;)
  inherited Destroy;
end;

procedure TBZLogger.SetOnCallBack(const AValue : TBZLoggerCallback);
begin
  if FOnCallBack = AValue then Exit;
  FOnCallBack := AValue;
  TBZCallBackLoggerWriter(FLogWriters.Items[2]).OnCallBack := FOnCallBack;
  if AValue <> nil then TBZCallBackLoggerWriter(FLogWriters.Items[2]).Enabled := True
  else TBZCallBackLoggerWriter(FLogWriters.Items[2]).Enabled := False;
end;

procedure TBZLogger.SetHandleApplicationException(const AValue : Boolean);
begin
  if FHandleApplicationException = AValue then Exit;
  FHandleApplicationException := AValue;
  if AValue then
  begin
    Application.OnException := @Self.CatchException;
    //FOldExceptProc := ExceptProc;
    //ExceptProc := @CatchUnhandledException;
  end
  else
  begin
    Application.OnException := nil;
    //ExceptProc := FOldExceptProc;
  end;
end;

procedure TBZLogger.WriteToLog;
var
  I : Integer;
begin
  for I := 0 to FLogWriters.Count - 1 do
  begin
    if FLogWriters.Items[I].Enabled then
      FLogWriters.Items[I].WriteLog(FCurrentItem.Level, FCurrentItem.TimeStamp, FCurrentItem.Msg);
  end;
end;

procedure TBZLogger.SetLogViewEnabled(const AValue : Boolean);
begin
  if FLogViewEnabled = AValue then Exit;
  FLogViewEnabled := AValue;
  TBZLogViewLoggerWriter(FLogWriters.Items[1]).Enabled := AValue;
  if AValue = False then HideLogView;
end;

procedure TBZLogger.Execute;
var
  LList: TList;
  LItem: TBZLoggerItem;
  LastCount: integer;
begin
  if Not(FEnabled) then Exit;
  LastCount:=0;

  while (not(Terminated))  do //  (not(Terminated) and (LastCount=0))
  begin
    try
      LItem := nil;
      if Assigned(FQueue) then
      begin
        LList:= FQueue.LockList;
        if (Assigned(LList)) then
        begin
          try
            LastCount := LList.Count;
            if LastCount > 0 then
            begin
              LItem := TBZLoggerItem(LList.Items[0]);
            end;
          finally
            if LItem <> nil then
            begin
              WriteLogItem(LItem);
              FreeAndNil(LItem);
              LList.Delete(0);
              LastCount := LList.Count;
            end;
            FQueue.UnlockList;
          end;
        end;
      end;
    except
      //Handle exception...
    end;
    Sleep(1);
  end;

end;

procedure TBZLogger.WriteLogItem(aItem : TBZLoggerItem);
//var
//  I : Integer;
//begin
//  for I := 0 to FLogWriters.Count - 1 do
//  begin
//    if FLogWriters.Items[I].Enabled then
//      FLogWriters.Items[I].WriteLog(aItem.Level, aItem.TimeStamp, aItem.Msg);
//  end;
begin
  FCurrentItem := aItem;
//  Synchronize(@WriteToLog);
  WriteToLog;
end;

procedure TBZLogger.AddLog(LogLevel : TBZLogLevel; aTimeStamp : TDateTime; aMsg : String);
var
  LList : TList;
  LItem : TBZLoggerItem;
begin
  //if Terminated then Exit;

  LItem := TBZLoggerItem.Create;
  LItem.Level := LogLevel;
  LItem.TimeStamp := now;
  LItem.Msg      := aMsg;
  if Assigned(FQueue) then
  begin
    LList := FQueue.LockList;
    try
      LList.Add(LItem);
    finally
      FQueue.UnlockList;
    end;
  end;
end;

procedure TBZLogger.LogError(ErrorMessage : string);
begin
  if Not(FLogSystemInfos) then LogSystemInfos;
  Inc(FLoggerCounter.Errors);
  AddLog(llError, now, ErrorMessage);
end;

procedure TBZLogger.LogWarning(WarningMessage : string);
begin
  if Not(FLogSystemInfos) then LogSystemInfos;
  Inc(FLoggerCounter.Warnings);
  AddLog(llWarn, now, WarningMessage);
end;

procedure TBZLogger.LogStatus(StatusMessage : string);
begin
  if Not(FLogSystemInfos) then LogSystemInfos;
  AddLog(llStatus, now, StatusMessage);
end;

procedure TBZLogger.LogNotice(NoticeMessage : string);
begin
  if Not(FLogSystemInfos) then LogSystemInfos;
  AddLog(llNotice, now, NoticeMessage);
end;

procedure TBZLogger.LogHint(HintMessage : string);
begin
  if Not(FLogSystemInfos) then LogSystemInfos;
  Inc(FLoggerCounter.Hints);
  AddLog(llHint, now, HintMessage);
end;

procedure TBZLogger.LogException(ExceptionMessage : string);
begin
  if Not(FLogSystemInfos) then LogSystemInfos;
  Inc(FLoggerCounter.Exceptions);
  AddLog(llException, now, ExceptionMessage);
end;

procedure TBZLogger.Log(aMsg : string);
begin
  if Not(FLogSystemInfos) then LogSystemInfos;
  AddLog(llNone, now, aMsg);
end;

procedure TBZLogger.LogSystemInfos;
Var
  TmpStr : String;
begin
  FLogSystemInfos := True;
  TmpStr := '';
  TmpStr.RepeatChar('-',80);
  log(TmpStr);
  LogStatus('[ Start Logging ] - [ ' + DateTimeToStr(Now) + ' ] ' + GetApplicationFileName);
  TmpStr := '[ Informations ]';
  TmpStr := TmpStr.PadCenter(80,'-');
  log(TmpStr);
  log(Format('Compilé             : à %s le %s', [{$I %TIME%}, {$I %DATE%}])); // par %s', {$I %USER%}]));
  log(Format('FPC                 : %s', [{$I %FPCVERSION%}]));
  log(Format('LCL                 : %s', [lcl_version]));
  log(Format('OS Cible            : %s', [{$I %FPCTARGETOS%}]));
  log(Format('CPU Cible           : %s', [{$I %FPCTARGETCPU%}]));
  log(Format('Widget              : %s', [GetWidgetSet()]));
  TmpStr := '[ Informations complémentaires ]';
  TmpStr := TmpStr.PadCenter(80,'-');
  log(TmpStr);
  log(Format('OS Informations     : %s', [GetPlatformVersionAsString()]));
  log(Format('Type CPU            : %s', [BZCPUInfos.BrandName]));
  log(Format('Dossier courrant    : %s', [GetCurrentDir()]));
  TmpStr.RepeatChar('-',80);
  log(TmpStr);
  log('');

end;

procedure TBZLogger.LogEnd;
Var
  TmpStr : String;
begin
  FLogEnd := True;
  log('');
  TmpStr := '';
  TmpStr.RepeatChar('-',80);
  log(TmpStr);
  LogStatus(Format('[ Conseils : %d | Warnings : %d | Erreurs : %d | Exceptions : %d ]',[LoggerCounter.Hints, LoggerCounter.Warnings, LoggerCounter.Errors, LoggerCounter.Exceptions]));
  log(TmpStr);
  LogStatus('[ Stop Logging] - [ ' + DateTimeToStr(Now) + ' ] ' + GetApplicationFileName);
  log(TmpStr);

end;

//https://wiki.freepascal.org/Logging_exceptions
procedure TBZLogger.DumpExceptionCallStack(E : Exception);
var
  I: Integer;
  Frames: PPointer;
  Report: string;
begin

  if E <> nil then
  begin
    Report := 'Classe : ' + E.ClassName + ' - Message: ' + E.Message + LineEnding;
  end;
  Report := Report +'         Pile d''appel :'+ BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + '                       ' + BackTraceStrFunc(Frames[I]);

  LogException(Report);
end;

procedure TBZLogger.CatchException(Sender : TObject; E : Exception);
begin
  DumpExceptionCallStack(E);
  //Halt; // End of program execution
end;

procedure TBZLogger.ShowLogView;
Var
  LoggerForm : TViewLoggerForm;
begin
  if FLogViewEnabled then
  begin
    LoggerForm := TBZLogViewLoggerWriter(FLogWriters.Items[1]).ViewLoggerForm;
    if (LoggerForm <> nil) then LoggerForm.Show;
  end;
end;

procedure TBZLogger.HideLogView;
Var
  LoggerForm : TViewLoggerForm;
begin
  //if FLogViewEnabled then
  //begin
    LoggerForm := TBZLogViewLoggerWriter(FLogWriters.Items[1]).ViewLoggerForm;
    if (LoggerForm <> nil) then LoggerForm.Hide;
  //end;
end;

//procedure CatchUnhandledException(Obj: TObject; Addr: Pointer; FrameCount: Longint; Frames: PPointer);
//var
//  Message: string;
//  i: LongInt;
//begin
//  Message := 'Une exception non gérée s''est produite à l''adresse $', sysBackTraceStr(addr), ' :' + LineEnding;
//  if Obj is exception then
//   begin
//     Message := Message + Exception(Obj).ClassName + ' : ' + Exception(Obj).Message + LineEnding;
//   end
//  else
//   Message := Message + 'Exception object : '+ Obj.ClassName + ' n''est pas une classe d''exception.' + LineEnding;
//
//  Message := Message + BackTraceStrFunc(Addr) + LineEnding;
//  if (FrameCount > 0) then
//    begin
//      for i := 0 to FrameCount - 1 do
//        Message := Message + BackTraceStrFunc(Frames[i]) + LineEnding;
//    end;
//
//  GlobalLogger.LogException(Message);
//end;



{%endregion%}

//================================================================================

initialization

  GlobalLogger := TBZLogger.Create;

  //with GlobalLogger do
  //begin
  //  LogSystemInfos;
  //end;

finalization

  //with GlobalLogger do
  //begin
  //  OnCallBack := nil;
  //  LogEnd;
  //end;
 // Sleep(20000);
  FreeAndNil(GlobalLogger);
end.
