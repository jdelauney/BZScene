(*
  @abstract(Contient des classes conçues à des fins génériques. @br
  Elles implémentent un mécanisme de persistance légèrement différent de celui de la LCL / VCL. @br
  Elles permettent le contrôle de version au niveau de l'objet avec une persistance 100% ascendante et une compatibilité polymorphe.)
   
  Cette unité, contient des définitions d'objets à surcharger.

  Ces objets peuvent : @br
  @unorderedList(
    @item(Etre notifiés lors de changements interne)
    @item(Etre cadencés (simulations basées sur le temps). )
    @item(Inclure un méchanisme de lecture/écriture de données dans un flux)
  )

  Contient également des classes spécialisées et utiles comme le remplacement d'un TStringList basé sur le hachage.

  -------------------------------------------------------------------------------------------------------------

  @created(2016-11-16)
  @author(J.Delauney (BeanzMaster))
  Historique :
  @unorderedList(
    @item(16/11/2016 : Creation  )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes :)

  -------------------------------------------------------------------------------------------------------------

  @bold(Dependances) : BZSystem, BZUtils,  BZTypesHelpers, BZMath, BZStreamClasses

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)
   @unorderedList(
     @item (Une partie des objets sont basés sur GLScene http://www.sourceforge.net/glscene)
     @item (Une partie des objets sont basés sur GraphicEx https://github.com/mike-lischke/GraphicEx)
     @item(J.Delauney (BeanzMaster))
   )

  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  -------------------------------------------------------------------------------------------------------------*)
Unit BZClasses;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}

//==============================================================================

//------------------------------------------------------------------------------
//----------------------------[ TODO LIST ]-------------------------------------

{ TODO -oBZClasses : Changer "FHashArray" de TBZCustomHashStringList par un "PInteger" }

//------------------------------------------------------------------------------

Interface

Uses
  Classes, SysUtils, Contnrs, Graphics, LCLType,
  BZSystem,  BZUtils, BZTypesHelpers, BZMath, BZStreamClasses;

//==============================================================================

Type
  { Exception de base déclenchée lors d'une erreur }
  EBZBaseException = Class(Exception);
  { Exception déclenchée sur un format de fichier incorrect ou des données endommagées }
  EInvalidFileSignature = class(EBZBaseException);
  { Exception déclenchée lorsqu'une erreur est détectée lors de l'écriture des données }
  EFilerException = class(EBZBaseException);

Type
  { PObject : Definit un  pointer sur un TObject }
  PObject = ^TObject;


Type
  { Capacité pour les classes de lecture et ou d'ecriture de données dans un fichier. @br
    cf : TBZCustomDataFile }
  TBZDataFileCapability   = (dfcRead, dfcWrite);
  TBZDataFileCapabilities = Set Of TBZDataFileCapability;

  { Type d'enregistrement pour stocker une progression dans le temps }
  TBZProgressTimes = Record
    deltaTime, newTime: Double
  End;

  { Événement de progression pour des animations / simulations basées sur le temps. @br
    DeltaTime est le delta du temps écoulé depuis le dernier état et newTime est la nouvelle heure après la fin de l'événement.}
  TBZCadencerProgressEvent = Procedure(Sender: TObject; Const deltaTime, newTime: Double) Of Object;

Type
  { Décrit une entrée dans la pile de progression pour les sections imbriquées. }
  TBZProgressSection = record
    { Position actuelle (en %)  }
    Position,
    { Taille de la section dans le contexte de la section parente (en %). }
    ParentSize,
    { Facteur accumulé pour transformer une étape dans la section en une valeur globale. }
    TransformFactor: Single;
    { Message à afficher pour la section. }
    Msg: string;
  end;
  PBZProgressSection = ^TBZProgressSection;

Type
   { Action de la progression (pour eviter d'utiliser l'uniter FPImage) }
   TBZProgressStage = (opsStarting, opsRunning, opsEnding);
   { Definition du type fonction pour l'evenement OnProgress }
   TBZProgressEvent = procedure (Sender: TObject; Stage: TBZProgressStage;
                                  PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
                                  const Msg: string; Var Continue:Boolean) of object;

{%region%===[ Virtual Reader/Writer ]===========================================}

  { Identique à TReader, mais réutilisable. @br
    Utilisé pour implémenter le mécanisme de persistance dans des classes }
  TVirtualReader = class
  private
    FStream: TStream;

  public
    { Creation }
    constructor Create(Stream: TStream); virtual;

    { Lève une exception en cas d'erreur }
    procedure ReadTypeError;

    { Lecture d'un tampon de donnée de taille "Count" }
    procedure Read(var Buf; Count: Longint); virtual; abstract;
    { Retourne le type de la valeur suivante }
    function NextValue: TValueType; virtual; abstract;
    { Lit une valeur de type Integer }
    function ReadInteger: Integer; virtual; abstract;
    { Lit une valeur de type Boolean }
    function ReadBoolean: Boolean; virtual; abstract;
    { Lit une valeur de type String }
    function ReadString: string; virtual; abstract;
    { Lit une valeur de type virgule flottante au format Extended }
    function ReadFloat: Extended; virtual; abstract;

    { Flag de début de lecture d'une liste de données }
    procedure ReadListBegin; virtual; abstract;
    { Flag de fin de lecture d'une liste de données }
    procedure ReadListEnd; virtual; abstract;
    { Retourne @True si la fin de la liste des données est atteinte }
    function EndOfList: Boolean; virtual; abstract;
    { Lit plusieurs chaine de caractères et place le résultat dans un TStrings }
    procedure ReadTStrings(aStrings: TStrings);

    { Retourne le flux de lecture }
	  property Stream: TStream read FStream;
  end;

  { Identique à TWriter, mais réutilisable. @br
    Utilisé pour implémenter le mécanisme de persistance }
  TVirtualWriter = class
  private
    FStream: TStream;

  public
    { Création de la classe }
    constructor Create(Stream: TStream); virtual;

    { Ecrit un tampon de donnée de taille "Count" }
    procedure Write(const Buf; Count: Longint); virtual; abstract;
    { Ecrit une valeur de type Integer }
    procedure WriteInteger(anInteger: Integer); virtual; abstract;
    { Ecrit une valeur de type Boolean }
    procedure WriteBoolean(aBoolean: Boolean); virtual; abstract;
    { Ecrit une valeur de type String }
    procedure WriteString(const aString: string); virtual; abstract;
    { Ecrit une valeur de type Virgule flottante }
    procedure WriteFloat(const aFloat: Extended); virtual; abstract;

    { Flag de début de l'écriture d'une liste }
    procedure WriteListBegin; virtual; abstract;
    { Flag de fin de l'écriture d'une liste }
    procedure WriteListEnd; virtual; abstract;
    { Ecrit une liste de caractère de type TStrings. La paramètre "storeObjects" indique si les objets attachés doivent également être sauvegardé }
    procedure WriteTStrings(const aStrings: TStrings; storeObjects: Boolean = True);

    { Retourne le flux d'écriture }
    property Stream: TStream read FStream;

  end;

  TVirtualReaderClass = class of TVirtualReader;
  TVirtualWriterClass = class of TVirtualWriter;

   { Lecteur virtuelle de données binaire }
   TBinaryReader = class(TVirtualReader)
   private
   protected
     function ReadValue: TValueType;
     function ReadWideString(vType: TValueType): WideString;

   public
     { Lit dans le flux "Count" valeur et les place dans "Buffer" }
     procedure Read(var Buf; Count: Longint); override;
     { Retourne le type la valeur suivante }
     function NextValue: TValueType; override;
     { Lit un Integer }
     function ReadInteger: Integer; override;
     { Lit un boolean }
     function ReadBoolean: Boolean; override;
     { Lit une chaine de caractères }
     function ReadString: string; override;
     { Lit une valeur de type Extended }
     function ReadFloat: Extended; override;
     { Debut la lecture d'une liste de valeur }
     procedure ReadListBegin; override;
     { Fin la lecture d'une liste de valeur }
     procedure ReadListEnd; override;
     { Retourne @TRUE si la fin de la liste des valeurs à lire est attainte }
     function EndOfList: Boolean; override;
   end;

   { Ecriture virtuelle de données binaire }
   TBinaryWriter = class(TVirtualWriter)
   private
   protected
     procedure WriteAnsiString(const aString: AnsiString); virtual;
     procedure WriteWideString(const aString: WideString); virtual;

   public
     { Ecrit un tampon de données "Buffer", de "count" valeur dans le flux }
     procedure Write(const Buf; Count: Longint); override;
     { Ecrit un Integer }
     procedure WriteInteger(anInteger: Integer); override;
     { Ecrit un boolean }
     procedure WriteBoolean(aBoolean: Boolean); override;
     { Ecrit une chaine de caractères }
     procedure WriteString(const aString: string); override;
     { Ecrit un nombre en virgule flottante de type Extended }
     procedure WriteFloat(const aFloat: Extended); override;
     { Debute l'écriture d'une liste de valeurs }
     procedure WriteListBegin; override;
     { Termine l'écriture d'une liste de valeurs }
     procedure WriteListEnd; override;
   end;

   { Lecture virtuelle de chaine de caractères }
   TTextReader = class(TVirtualReader)
   private
     FValueType: string;
     FData:      string;

   protected
     procedure ReadLine(const requestedType: string = '');

   public
     { Lit dans le flux "Count" valeur et les place dans "Buffer" }
     procedure Read(var Buf; Count: Longint); override;
     { Retourne le type la valeur suivante }
     function NextValue: TValueType; override;
     { Lit un Integer }
     function ReadInteger: Integer; override;
     { Lit un boolean }
     function ReadBoolean: Boolean; override;
     { Lit une chaine de caractères }
     function ReadString: string; override;
     { Lit une valeur de type Extended }
     function ReadFloat: Extended; override;
     { Debut la lecture d'une liste de valeur }
     procedure ReadListBegin; override;
     { Fin la lecture d'une liste de valeur }
     procedure ReadListEnd; override;
     { Retourne @TRUE si la fin de la liste des valeurs à lire est attainte }
     function EndOfList: Boolean; override;
   end;

   { Ecriture virtuelle de chaine de caractères }
   TTextWriter = class(TVirtualWriter)
   private
     FIndentLevel: Integer;

   protected
     procedure WriteLine(const valueType, data: string);

   public
     { Création }
     constructor Create(aStream: TStream); override;
     { Destruction }
     destructor Destroy; override;

     { Ecrit un tampon de données "Buffer", de "count" valeur dans le flux }
     procedure Write(const Buf; Count: Longint); override;
     { Ecrit un Integer }
     procedure WriteInteger(anInteger: Integer); override;
     { Ecrit un boolean }
     procedure WriteBoolean(aBoolean: Boolean); override;
     { Ecrit une chaine de caractères }
     procedure WriteString(const aString: string); override;
     { Ecrit un nombre en virgule flottante de type Extended }
     procedure WriteFloat(const aFloat: Extended); override;
     { Debute l'écriture d'une liste de valeurs }
     procedure WriteListBegin; override;
     { Termine l'écriture d'une liste de valeurs }
     procedure WriteListEnd; override;
   end;

{%endregion%}

{%region%===[ Interfaces ]======================================================}

  { Interface pour les objets persistants. @br
    Cette interface ne permet pas vraiment la persistance polymorphe, mais plutôt
    un moyen d'unifier les appels de persistance pour les itérateurs. }
  IBZPersistentObject = interface(IInterface)
    ['{28E473E4-68D3-4327-A75F-FB2BCF9B1B68}']
    procedure WriteToFiler(writer: TVirtualWriter);
    procedure ReadFromFiler(reader: TVirtualReader);
  end;

  { Interface pour les objets avec un systeme et d'evenement de notifications.
    Offre la cappacité de notifer les changements de valeurs des propriétés à d'autres classes. }
  IBZNotifyAble = Interface(IInterface)
    ['{194A11B4-1B42-4673-913D-958246993510}']
    Procedure NotifyChange(Sender: TObject);
  End;

  { Interface offrant la possiblités d'animer, de faire des simulations basées sur le temps. }
  IBZCadenceAble = Interface(IInterface)
    ['{7728299B-436E-4F1D-AAED-E4F63AD5E5A3}']
    Procedure DoProgress(Const progressTime: TBZProgressTimes);
  End;

{%endregion%}

{%region%===[ Persistent Classes ]==============================================}

  { @abstract(Classe de base pour les objets persistants.)

    L'exigence de base est l'implémentation de ReadFromFiler et WriteToFiler dans les sous-classes. @br
    L'autre exigence est l'utilisation d'un constructeur virtuel, ce qui permet
    la construction polymorphe (ne pas oublier d'enregistrer vos sous-classes).

    Les avantages immédiats sont le support du streaming (flux, fichier ou chaîne), l'affectation et le clonage. }
  TBZPersistentObject = class(TPersistent, IBZPersistentObject)
  private
  protected
    procedure RaiseFilerException(const archiveVersion: Integer);

    function QueryInterface(constref IID: TGUID; out Obj): HResult; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};

  public
    { Creation }
    constructor Create; virtual;
    { Creation de l'objet depuis la lecture de valeurs de propriétés d'un TVirtualReader}
    constructor CreateFromFiler(reader: TVirtualReader);
    { Destruction }
    destructor Destroy; override;

    { Assigne un autre TBZPersistentObject }
    procedure Assign(source: TPersistent); override;
    { Creation d'un clone du TBZPersistentObject }
    function CreateClone: TBZPersistentObject; dynamic;

    { Retourne la signature de l'objet dans la persistence des données }
    class function FileSignature: string; virtual;
    { Retourne la classe du lecteur des propriétiés de l'objet dans la persistence des données }
    class function FileVirtualWriter: TVirtualWriterClass; virtual;
    { Retourne la classe de l'enregistreur des propriétiés de l'objet dans la persistence des données }
    class function FileVirtualReader: TVirtualReaderClass; virtual;

    { Ecriture des propriétés dans la persistence des données }
    procedure WriteToFiler(writer: TVirtualWriter); dynamic;
    { Lecture des propriétés dans la persistence des données }
    procedure ReadFromFiler(reader: TVirtualReader); dynamic;

    { Sauvegarde les propriétés dans un flux }
    procedure SaveToStream(Stream: TStream; writerClass: TVirtualWriterClass = nil); dynamic;
    { Charge les propriétés depuis un flux }
    procedure LoadFromStream(Stream: TStream; readerClass: TVirtualReaderClass = nil); dynamic;
    { Sauvegarde les propriétés dans un fichier }
    procedure SaveToFile(const fileName: string; writerClass: TVirtualWriterClass = nil); dynamic;
    { Charge les propriétés depuis un fichier }
    procedure LoadFromFile(const fileName: string; readerClass: TVirtualReaderClass = nil); dynamic;
    { Sauvegarde les propriétés dans une chaine de caractère sérialisée }
    function SaveToString(writerClass: TVirtualWriterClass = nil): string; dynamic;
    { Charge les propriétés depuis une chaine de caractère sérialisée }
    procedure LoadFromString(const data: string; readerClass: TVirtualReaderClass = nil); dynamic;
  end;

  { Type de classes héritée de TBZPersistentObject }
  TBZPersistentObjectClass = class of TBZPersistentObject;

  { Tableau d'objets}
  TPointerObjectList     = array [0 .. MaxInt div (2 * SizeOf(Pointer))] of TObject;
  { Pointeru vers un tableau d'objets }
  PPointerObjectList     = ^TPointerObjectList;

  { Fonction pour comparer deux TObject }
  TObjectListSortCompare = function(item1, item2: TObject): Integer of object;

  { @abstract(Liste d'objets persistants.
    Similaire à TList, mais fonctionne sur des objets TObject directement et dispose d'un
    mécanisme de persistance des données.)

    Contrairement à la TObjectList de la VCL / LCL, TBZPersistentObjectList ne libère pas ses objets
    lors de la destruction ou de Clear. Vous devez utiliser les fonctions Clean et CleanFree.

    @unorderedlist(
      @item(Seuls les éléments TBZPersistentObject et descendants seront gérés correctement.)
      @item(La liste peut être utilisée sous forme de LIFO  avec Push and Pop.)
      @item(Vous pouvez effectuer des opérations booléennes de base.)
    )

    @bold(Note) : L'implémentation de la fonction IndexOf est jusqu'à 3 fois plus rapide que celle de TList }
  TBZPersistentObjectList = class(TBZPersistentObject)
  private
    FList:        PPointerObjectList;
    FCount:       Integer;
    FCapacity:    Integer;
    FGrowthDelta: Integer;

  protected
    function Get(Index: Integer): TObject;
    procedure Put(Index: Integer; Item: TObject);
    procedure SetCapacity(newCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    function GetFirst: TObject;
    procedure SetFirst(Item: TObject);
    function GetLast: TObject;
    procedure SetLast(Item: TObject);

    // : Default event for ReadFromFiler
    procedure AfterObjectCreatedByReader(Sender: TObject); virtual;
    procedure DoClean;

  public
    { Creation }
    constructor Create; override;
    { Destruction }
    destructor Destroy; override;

    { Ecriture des propriétés avec le mechanisme de Persistance des objets }
    procedure WriteToFiler(writer: TVirtualWriter); override;
    { Lecture des propriétés avec le mechanisme de Persistance des objets }
    procedure ReadFromFiler(reader: TVirtualReader); override;
    { Lecture des propriétés avec le mechanisme de Persistance des objets, avec un évènement déclencher à la fin de la lecture }
    procedure ReadFromFilerWithEvent(reader: TVirtualReader; afterSenderObjectCreated: TNotifyEvent);

    { Ajoute un objet à la liste }
    function Add(const Item: TObject): Integer;
    { Ajoute plusieur élément VIDE à la liste }
    procedure AddNils(nbVals: Cardinal);
    { Efface une élément de la liste }
    procedure Delete(Index: Integer);
    { Efface plusieurs éléments de la liste }
    procedure DeleteItems(Index: Integer; nbVals: Cardinal);
    { Echange deux élément dans la liste }
    procedure Exchange(Index1, Index2: Integer);
    { Insert un élément à la position "Index" dans la liste }
    procedure Insert(Index: Integer; Item: TObject);
    { Insert plusieurs éléments VIDE à la position "Index" dans la liste }
    procedure InsertNils(Index: Integer; nbVals: Cardinal);
    { Deplace un élément dans la liste }
    procedure Move(CurIndex, NewIndex: Integer);
    { Remove : Efface un élément et renvoi le nouvel "index" }
    function Remove(Item: TObject): Integer;
    { Efface un élément de la liste et le libère }
    procedure DeleteAndFree(Index: Integer);
    { Efface plusieurs éléments de la liste et les libères }
    procedure DeleteAndFreeItems(Index: Integer; nbVals: Cardinal);
    { Efface un élément et le libère. Renvoi le nouvel "index" }
    function RemoveAndFree(Item: TObject): Integer;
    { Indice d'augmentation de la capacité du nombre d'éléments de la liste }
    property GrowthDelta: Integer read FGrowthDelta write FGrowthDelta;
    { Augmente la taille de la liste de @link(GrowDelta) }
    function Expand: TBZPersistentObjectList;

    { Retourne la liste des objets }
    property Items[Index: Integer]: TObject read Get write Put; default;
    { Nombre d'objets dans la liste }
    property Count: Integer read FCount write SetCount;
    { Retourne un pointer de la liste d'objet }
    property List: PPointerObjectList read FList;
    { Capacité de la liste }
    property Capacity: Integer read FCapacity write SetCapacity;

    { Assure que la capacité est au moins aussi grande que Capacity. }
    procedure RequiredCapacity(aCapacity: Integer);

    { Supprime tous les 'nil' de la liste.  @br
      @bold(Notes) : @br
       - La capacité est inchangée, pas de mémoire libérée, la liste est juste plus courte. @br
       - Cette fonctions est plus rapides que celle de TList. }
    procedure Pack;
    { Efface la liste sans libérer les objets. }
    procedure Clear; dynamic;
    { Vide la liste et libère les objets. }
    procedure Clean; dynamic;
    { Vide la liste, libere les objets et libere la liste elle même }
    procedure CleanFree;
    { Renvoie l'index de "Item" dans la liste }
    function IndexOf(Item: TObject): Integer;
    { Retourne le premier élément de la liste }
    property First: TObject read GetFirst write SetFirst;
    { Retourne le dernier élément de la liste }
    property Last: TObject read GetLast write SetLast;
    { Ajoute un élément en haut de la liste }
    procedure Push(Item: TObject);
    { Retourne le dernier élément de la liste }
    function Pop: TObject;
    { Retourne le dernier élément de la liste et libère l'objet }
    procedure PopAndFree;
    { Ajoute une liste à la liste }
    function AddObjects(const objectList: TBZPersistentObjectList): Integer;
    { Efface les objets de la liste "ObjectList" }
    procedure RemoveObjects(const objectList: TBZPersistentObjectList);
    { Fonction de tri }
    procedure Sort(compareFunc: TObjectListSortCompare);
  end;

  { TPersistent qui connait son propriétaire }
  TBZOwnedPersistent = class(TPersistent)
  private
    FOwner: TPersistent;
  protected
    function GetOwner: TPersistent; override;
  public
    { Creation }
    constructor Create(AOwner: TPersistent); virtual;
  end;

  { TPersistent qui implémente une IInterface. }
  TBZInterfacedPersistent = class(TPersistent, IInterface)
  protected
    function QueryInterface(constref IID: TGUID; out Obj): HResult; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  end;

  { TCollectionItem  qui implémente une IInterface. }
  TBZInterfacedCollectionItem = class(TCollectionItem, IInterface)
  protected
    // Implementing IInterface.
    function QueryInterface(constref IID: TGUID; out Obj): HResult; virtual;  {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef: Integer; virtual; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: Integer; virtual;  {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};

  end;

{%endregion%}

{%region%===[ Base Classes ]====================================================}

  { Classe abstraite décrivant l'interface "IBZNotifyAble"
    Offre un systeme d'evenement de notifications, qui donne  la capacité de notifer
    les changements de valeurs des propriétés à d'autres classes. }
  TBZUpdateAbleObject = Class(TBZInterfacedPersistent, IBZNotifyAble)
  Private
    FOwner:    TPersistent;
    FUpdating: Integer;
    FOnChange: TNotifyEvent;

  protected
    Function GetOwner: TPersistent; Override;

  Public
    { Creation }
    Constructor Create; Overload; Virtual;
    { Creation avec un propriétaire }
    Constructor Create(AOwner: TPersistent); Overload; Virtual;

    { Notifie un changement dans l'objet. L'énènement se propagera dans les objets propriétaires (Owner) de celui-ci }
    Procedure NotifyChange(Sender: TObject); Virtual;
    { A Surcharger dans les classe enfant }
    Procedure Notification(Sender: TObject; {%H-}Operation: TOperation); Virtual;

    { Débute une mise à jour de l'objet }
    Procedure BeginUpdate;
    { Fin d'une mise à jour de l'objet }
    Procedure EndUpdate;

    { Propriétaire de l'objet }
    Property Owner: TPersistent read GetOwner;
    { Compteur des mise à jour de l'objet }
    Property Updating: Integer read FUpdating;
    { Evènement levé lors d'une changement d'état de l'objet }
    Property OnChange: TNotifyEvent read FOnChange write FOnChange;
  End;
  
  { Classe abstraite ajoutant la gestion d'une progression }
  TBZProgressAbleObject = Class(TBZUpdateAbleObject)
  private
    // Support pour l'affichage de la progression d'un traitement (Section)
    FProgressStack: TStack;          // Utilisé pour gérer les section imbriquées
    FProgressRect: TRect;
    FPercentDone: Single;            // Progression du travail en cours

    FOnProgress : TBZProgressEvent; // Evenement pour l'avancement d'un traitement

  protected
     ContinueProgress: Boolean;  //< Drapeau pour savoir si la progression continue

    { Initialisation du processus de progression }
    procedure InitProgress(AWidth, AHeight: Integer);
    { Initialisation d'une sous section de progression }
    procedure StartProgressSection(Size: Single; const S: string);
    { Avance de l'opération d'une section }
    procedure AdvanceProgress(Amount: Single; OffsetX, OffsetY: Integer; DoRedraw: Boolean);
    { Finalisation d'une sous section de progression }
    procedure FinishProgressSection(DoRedraw: Boolean);

    { Execution de l'evènement OnProgress }
    procedure Progress(Sender: TObject; Stage: TBZProgressStage;
                       PercentDone: Byte;  RedrawNow: Boolean; const R: TRect;
                       const Msg: String; var aContinue: Boolean); Virtual;

    { Efface la liste des progressions }
    procedure ClearProgressStack;

  public
    { Evènement levé lors de la progression d'un travail }
    property OnProgress: TBZProgressEvent read FOnProgress write FOnProgress;
  End;

  { En plus des méthodes de TBZUpdateAbleObject, et TBZProgressAbleObject
    il rajoute un verrouillage pour les threads }
  TBZThreadAbleObject = class(TBZProgressAbleObject)
  private
    FLockCount : Integer;

  protected
    CriticalSection : TRTLCriticalSection;
    property LockCount: Integer read FLockCount;

  public
    constructor Create; override;
    destructor Destroy; override;
    { Verrouille l'objet pour les accès en écriture et lacture }
    procedure Lock;
    { Dévérrouille l'accès à l'objet }
    procedure Unlock;
  End;

  { Classe abstraite décrivant l'interface "IBZProgessAble". @br
    Elle est basée sur le Temps, permettant de créer des animations / simulations (2D,3D,...)
    ou autres fonctions d'arrière-plan }
  TBZCadenceAbleComponent = Class(TComponent, IBZCadenceAble)
  Public
    { Execution de la progression }
    Procedure DoProgress(Const {%H-}progressTime: TBZProgressTimes); Virtual;
  End;

  { Classe abstraite décrivant et supportant IBZProgessAble et IBZNotifyAble. @br
    Elle offre la cappacité de notifer les changements de valeurs des propriétés à d'autres classes. @br
    Elle permet "d'animer" les propriétés en se basant sur le temps }
  TBZUpdateAbleComponent = Class(TBZCadenceAbleComponent, IBZNotifyAble)
  Public
    { Notification des changements d'états de l'objet }
    Procedure NotifyChange(Sender: TObject); Virtual;
  End;

  { Decrit l'algorithme de compression utilisé dans un fichier }
  TBZEncodingType = (etNone, etBitFields, etRLE, etHuffman, etLZW, etLZ77, etThunder, etJPEG, etVP8, etNotSupported, etUnknown);
  { Description minimale pour la prise en charge d'un format de fichier spécifique }
  TBZDataFileFormatDesc = Class(TObject)
  Private
    FFileMask: String;
    FDesc:     String;
    FName:     String;
    FVersion:  String;
    FVersionAsInteger : Integer;
    FEncoding: TBZEncodingType;

  Public
    { Création }
    Constructor Create;
    { Destruction }
    Destructor Destroy; Override;

    { Assigne un autre TBZDataFileFormatDesc }
    procedure Assign(Source : TBZDataFileFormatDesc);

    { Masque des extensions de fichier }
    Property FileMask: String read FFileMask write FFileMask;
    { Description du format }
    Property Desc: String read FDesc write FDesc;
    { Nom du format }
    Property Name: String read FName write FName;
    { Version du format }
    Property Version: String read FVersion write FVersion;
    { Version du format en integer }
    property VersionAsInteger : Integer Read  FVersionAsInteger Write FVersionAsInteger;
    { Type d'encodage/compression du format }
    Property Encoding: TBZEncodingType read FEncoding write FEncoding;
  End;

  {  Evènement levé lors d'une erreur au chargement d'un flux par TBZCustomDataFile }
  TBZDataFileLoadErrorEvent = procedure (Sender: TObject;Const ErrorCount : Integer;Const ErrorList:TStringList) of object;
  { @abstract(Classe de base abstraite pour les interfaces de formats de fichiers de données.
    Cette classe déclare les comportements de base liés aux fichiers.
    C'est à dire, pouvoir charger / enregistrer à partir d'un fichier ou d'un flux.)

    Il est fortement recommandé de surcharger SEULEMENT les méthodes d'acces à la mémoire.
    (fonction CanLoad et procedure LoadFromMemory)

    Les comportements en flux permettent d'ajouter d'autres capacités en E/S,
    comme la compression, la mise en cache, etc... sans qu'il soit nécessaire de réécrire de sous-classes.
    La base provient de GLScene/GraphicEx.

    @bold(Note) : CheckFormat / LoadFromMemory / SaveToMemory :  @br
    Les enfants doivent réécrirent ces méthodes pour supporter la lecture et ou l'écriture d'un format de fichier.}
  TBZCustomDataFile = Class(TBZThreadAbleObject)
  Private
    // Decoder : TBZDataFileDecoder; // prise ne chage de differents type de compression/decompression ou autre
    // Nom du fichier ou de la ressource chargée
    FResourceName: String;
    FFullFileName: String;
    FDataFileFormatDesc: TBZDataFileFormatDesc;
    // Buffer pour accéder aux données du fichier ou d'un stream
    FData: TBZBufferedStream;

    FErrorList : TStringList;
    FErrorCount : Integer;
    FOnLoadError : TBZDataFileLoadErrorEvent;

    // FMapFileMode :Boolean;
    //  FFileMapStream : TFileMapStream;
    // Données Brutes apres lecture des donnée du fichier ou du stream
    //FRawData : TRawDataRec;

    Procedure InternalLoadFromStream(aStream: TStream);
    Procedure InternalSaveToStream(aStream: TStream);

  Protected
    { Verification du format dans le flux }
    Function CheckFormat(): Boolean; Virtual;
    { Lecture du format dans le flux }
    Procedure LoadFromMemory; Virtual;
    { Enregistre le format dans le flux }
    Procedure SaveToMemory; Virtual;

    { Ajout d'une erreur }
    procedure AddError(Msg:String);
    { Notification des erreurs }
    procedure NotifyError;

  Public
    { Creation }
    Constructor Create; Override;
    { Creation de l'objet TBZCustomDataFile avec un propriétaire }
    Constructor Create(AOwner: TPersistent); Override;
    { Destruction }
    Destructor Destroy; Override;

    { Décrit les capacités de lecture et ou écriture des données
      Valeur par defaut [dfcRead]. }
    Class Function Capabilities: TBZDataFileCapabilities; Virtual;

    { Assigne un autre TBZCustomDataFile }
    procedure Assign(Source: TPersistent); override;

    { Duplique et renvoie un TBZCustomDataFile
      Les enfants doivent réécrirent cette méthode pour supporter leurs données. }
    Function CreateCopy(AOwner: TPersistent): TBZCustomDataFile; Dynamic;

    { Vérification si on peux charger les données depuis un fichier. }
    Function CanLoad(Const FileName: String): Boolean; Overload;
    { Vérification si on peux charger les données depuis un flux. }
    Function CanLoad(aStream: TStream): Boolean; Overload;

    //procedure WriteToMemory(); virtual;

    { Charge les données depuis un flux TStream}
    Procedure LoadFromStream(aStream: TStream); Virtual;
    { Charge les données depuis un fichier physique }
    Procedure LoadFromFile(Const FileName: String); Virtual;

    { A Surcharger uniquement si l'enfant veux et peux exporter les données }
    Procedure SaveToFile(Const fileName: String); Virtual;
    { A Surcharger uniquement si l'enfant veux et peux exporter les données }
    Procedure SaveToStream(aStream: TStream); Virtual;
    { A Surcharger pour initialiser des paramètres personnels. (Avant la lecture et l'écriture) }
    Procedure Initialize; Dynamic;

    { Lorsque  LoadFromFile/SaveToFile est utilisé , le nom du fichier est placé ici.
      Lorsque un Stream ou une variante est utilsé l'appelant peux placer son nom
      ou autre pour une utilisation ultérieure. }
    Property ResourceName: String read FResourceName write FResourceName;
    { Retourne le nom fichier avec chemin complet }
    Property FullFileName: String read FFullFileName write FFullFileName;
    { Retourne les informations basiques sur le format du fichier }
    Property DataFormatDesc: TBZDataFileFormatDesc read FDataFileFormatDesc;

    // property RawData : TRawDataRec Read FRawData;

    { Retourne le flux direct de lecture ou d'écriture des données }
    Property Memory: TBZBufferedStream read FData;

    { Nombre d'erreur produite loars d'un chargement ou d'un enregistrement }
    property ErrorCount : Integer Read FErrorCount;
    { Retourne la liste des erreurs }
    property Errors : TStringList Read FErrorList;
    { Evenement pour intercepter les erreurs notifiées lors du chargement des données }
    property OnLoadError : TBZDataFileLoadErrorEvent Read FOnLoadError Write FOnLoadError;

  End;

  { Classe de type TBZCustomDataFile }
  TBZDataFileClass = Class Of TBZCustomDataFile;

{%endregion%}

{%region%===[ TStringList replacement Class ]===================================}

Type
  { Classe abstraite permettant de gérer une liste de chaine de caractère, en associant chaque item à un "Hash" permettant ainsi de
    retrouver un item spécifique très rapidement juste par correspondance. }
  TBZCustomHashStringList = Class;

  { Classe abstraite décrivant un élément dans TBZCustomHashStringList. @br
    On peut associer un objet dans "ObjectRef" de type TObjet et un "Tag" de type Integer }
  TBZCustomHashListItem   = Class(TBZPersistentObject)
  Private
    FOwner: TBZCustomHashStringList;
    FText: String;
    FObjectRef: TObject;
    FTag: Integer; //Pointer;
    FTagFloat: Single; //Pointer;
    FTagText : String;
  Protected
    Procedure SetText(aValue: String);

  Public
    { Creation }
    Constructor Create; Override;
    { Creation de l'objet TBZCustomHashListItem avec un propriétaire de type TBZCustomHashStringList}
    Constructor Create(aOwner: TBZCustomHashStringList); Overload;
    //destructor Destroy; override;

    { Prise en charge de l'écriture du mécanisme de persistance des données }
    Procedure WriteToFiler(writer: TVirtualWriter); Override;
    { Prise en charge de lecture du mécanisme de persistance des données }
    Procedure ReadFromFiler(reader: TVirtualReader); Override;

    { Retourne de propriétaire de l'item }
    Property Owner: TBZCustomHashStringList read FOwner write FOwner;
    { Chaine de caractères associé à l'item }
    Property Text: String read FText write SetText;
    { Objet attaché à l'item }
    Property ObjectRef: TObject read FObjectRef write FObjectRef;
    { Tag de l'item }
    Property Tag: Integer read FTag write FTag;
    { Tag de type single }
    Property TagFloat: Single read FTagFloat write FTagFloat;
    { Tag de type string }
    property TagText : String read FTagText write FTagText;
  End;

  { Type Tableau pour stocker des éléments de type TBZCustomHashListItem }
  TBZCustomHashListItemArray = Array[0..cHash2 - 1] Of TBZCustomHashListItem;
  { Pointeur vers un Tableau s de type TBZCustomHashListItemArray }
  PBZCustomHashListItemArray = ^TBZCustomHashListItemArray;

  { Classe optimisée pour utiliser une liste de chaine de caractères
    avec la possibiliter d'associer  un nom, un d'objets et ou  un Tag. @br
    Elle intègre une recherche basé sur le "Haschage". @br
    Ce qui fait que la recherche est un simple test de correspondance. @br
    La liste n'a donc pas besoin d'être triée pou effectuer un recherche rapide..}
  TBZCustomHashStringList = Class(TBZCustomDataFile)
  Private
    FItemList:      TBZPersistentObjectList;
    FCaseSensitive: Boolean;
    FIsSorted:      Boolean;
    FDirectList:    PBZCustomHashListItemArray;
    FHashArray:     Array[0..cHash2 - 1] Of Integer; // Tableau des hashs des éléments de la liste

    Function GetStringText(Index: Integer): String;
    Procedure SetStringText(Index: Integer; Const Value: String);

    Function GetObjectRef(Index: Integer): TObject;
    Procedure SetObjectRef(Index: Integer; Const Value: TObject);

  Protected
    Function GetItem(Index: Integer): TBZCustomHashListItem;
    Procedure SetItem(Index: Integer; Const Value: TBZCustomHashListItem);

    Procedure AfterItemCreated(Sender: TObject);
    { Renvoie la liste sous forme de pointeur. Cette liste peut-être triée préalablement avec
      la procedure @link(DirecSort) qui est 1000x plus rapide que QuickSort }
    Function getDirectList: PBZCustomHashListItemArray;
    // procedure UpdateHashArray;
    Function CompareStringListItemHashText(item1, item2: TObject): Integer;
    Function CompareStringListItemText(item1, item2: TObject): Integer;

  Public
    { Creation }
    Constructor Create(AOwner: TPersistent); Override;
    { Destruction }
    Destructor Destroy; Override;

    { UpdateDirectList : Met à jour la liste de données DirectList }
    Procedure UpdateDirectList;

    { Ajoute un élément à la liste }
    Function Add(Const Value: TBZCustomHashListItem): Integer;
    { Ajoute une chaine de caractères à la liste }
    Function Add(Const s: String): Integer; Overload;
    { Ajoute une chaine de caractères à la liste }
    Function Add(Const s: String; Const aTag : String): Integer; Overload;
    { Ajoute une chaine de caractères à la liste et assigne un Tag }
    Function AddInt(Const s: String; Const aTag: Integer): Integer;
    { Ajoute une chaine de caractères à la liste et assigne un Tag }
    Function Add(Const s: String; Const aTag: Single): Integer; Overload;
    { Ajoute une chaine de caractères à la liste et attache un Objet }
    Function Add(Const s: String; AObject: TObject): Integer; Overload;
    { Efface la liste }
    procedure Clear;
    { Integration avec L'IDE  : Ecriture des données de la classe }
    Procedure WriteToFiler(writer: TVirtualWriter);
    { Integration avec L'IDE  : Lecture des données de la classe }
    Procedure ReadFromFiler(reader: TVirtualReader);

    { Renvoie le nombre d'éléments dans la liste }
    Function Count: Integer;

    { Find : Recherche une chaine de caractère. @br
      Renvoie @True si trouvé et retourne dans "Index" la position de l'élément trouvé dans la liste.
      Sinon renvoi -1. }
    Function Find(Const S: String; Var index: Integer): Boolean;

    { Renvoie la position d'un élément dans la liste en utilsant le hash }
    Function IndexOf(Const aText: String): Integer;
    { Renvoie la position du nom d'un élément de la liste }
    Function IndexOfText(Const s: String): Integer;
    { Renvoie la position d'un élément dans la liste }
    Function IndexOfItem(aItem: TBZCustomHashListItem): Integer;

    { Supprime et libère un élément de la liste }
    Procedure RemoveAndFreeItem(aItem: TBZCustomHashListItem);

    { Nettoie, compresse la liste afin d'optimiser l'utilisation de la mémoire }
    Procedure Pack;

    { La procedure de trie ici n'utilise pas la fonction de base par défaut qui utilise
      l'algorithme QuickSort. A la place le trie est effectué par l'algorithme
      "A-Sort" qui est 1000x plus rapide. Cet algorithme est basé sur un article de
      Lionel Defosse parue dans le magazine pascalissisme n°59.
      ( https://ldelafosse.pagesperso-orange.fr/Sort/Papers/Asort.htm ) @br
      Le trie est effectué uniquement dans DirectList dans un soucis de performance. @br
      La Liste de l'objet n'est pas impacté. }
    Procedure DirectSort;

    { Trie la liste. Avec la fonction par défaut QuickSort. }
    Procedure Sort;

    { Renvoie un élément de la liste }
    Property Items[Index: Integer]: TBZCustomHashListItem read GetItem write SetItem; Default;
    { Renvoie le nom associer à un élément de la liste }
    Property Strings[Index: Integer]: String read GetStringText write SetStringText;
    { Renvoie l'objet associer à un élément de la liste }
    Property Objects[Index: Integer]: TObject read GetObjectRef write SetObjectRef;
    { Définit si sensible à la casse }
    Property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
    { Renvoie @True si la liste a été triée }
    Property IsSorted: Boolean read FIsSorted;
    { Renvoie la liste sous forme d'un pointeur }
    Property List: PBZCustomHashListItemArray read getDirectList;

  End;

  { Gestion d'une liste de chaine de caractères. Les fonction recherches sont optimisées : @br
    @unorderedList(
     @item(IndexOf     : Recherche par correspondance basée sur le haschage (très rapide))
     @item(IndexOfText : Recherche par comparaison. Nb : trie de la liste avant de lancer la recherche ) (rapide))
     @item(IndexOfItem : Recherche native de TBZPersistentObjectList)
    )

    Offre également la capacité de charger des données TEXT depuis un flux ou un fichier.
  }
  TBZStringList = Class(TBZCustomHashStringList)
  Private
    FContent: String;

    Function CheckDataIsText(Const aBuf: Pointer): Boolean;
  Protected
    Procedure LoadFromMemory(); Override;
    Function CheckFormat(): Boolean; Override;
  Public
    { Ligne en cours }
    Line: Integer;

    Constructor Create(AOwner: TPersistent); Override;
    { Retourne les capacités de la classe lecture/écriture ou les deux}
    Class Function Capabilities: TBZDataFileCapabilities; Override;
    { Contenu de la liste sous forme de string }
    Property Content: String read FContent write FContent;
  End;

{%endregion%}

{%region%===[ Misc Classes ]====================================================}

  { Collection avec un systeme et d'evenement de notifications. @br
    Offre la capacité de notifer les changements de valeurs des propriétés à d'autres classes.  }
  TBZNotifyCollection = Class(TOwnedCollection)
  Private
    FOnNotifyChange: TNotifyEvent;

  Protected
    Procedure Update(item: TCollectionItem); Override;

  Public
    { Creation }
    Constructor Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
    { Evènement de notification de changement }
    Property OnNotifyChange: TNotifyEvent read FOnNotifyChange write FOnNotifyChange;
  End;

{%endregion%}

{ Méthode de convenance qui déclenche une exception lors de la lecture à partir de données persistantes }
procedure RaiseFilerException(aClass: TClass; archiveVersion: Integer);
{ Creation d'un flux TStream depuis un fichier }
function CreateFileStream(const fileName: string; mode: Word = fmOpenRead + fmShareDenyNone): TStream;

Implementation

Uses
  LazUTF8Classes, LazUTF8
  //, BZCrossPlateFormTools
  {$IFDEF DEBUG}
  //,Dialogs,
  ,BZLogger
  {$ENDIF};

const
  { cDefaultListGrowthDelta : Indice d'augmentation de la capacité de la liste }
  cDefaultListGrowthDelta = 16;

const
  { Constante pour le mechanisme de persitance des données }
  cVTInteger   = 'Int';
  cVTFloat     = 'Float';
  cVTString    = 'Str';
  cVTBoolean   = 'Bool';
  cVTRaw       = 'Raw';
  cVTListBegin = '{';
  cVTListEnd   = '}';
  cTrue  = 'True';
  cFalse = 'False';

{ Constantes et type utiles à l'algorithme A-Sort. cf procedure DirectSort }
Const
  MaxAlphaSize = MaxInt Shr 4;
  MaxStrLength = 127;

{ Définition pour l'algorithme de trie "Alpha-Sort" }
Type
  PStr      = ^String;
  RangeArray = Array[0..255] Of Word;
  LetterArray = Array[1..MaxStrLength] Of RangeArray;
  WordArray = Array[1..MaxAlphaSize] Of Int64;
  StrArray  = Array[0..MaxAlphaSize] Of Pointer;
{%endregion%}

{%region%===[ Helpers Functions ]===============================================}

procedure RaiseFilerException(aClass: TClass; archiveVersion: Integer);
begin
  // rsStr cUnknownArchiveVersion
  raise EFilerException.Create(aClass.ClassName + ' -> Archive Version Inconnue :'+ IntToStr(archiveVersion));
end;

{%endregion%}

{%region%===[ Virtual Reader/Writer ]===========================================}

{%region%-----[ Virtual Reader ]------------------------------------------------}
constructor TVirtualReader.Create(Stream: TStream);
begin
  FStream := Stream;
end;

procedure TVirtualReader.ReadTypeError;
begin
  raise EReadError.CreateFmt('%s, Erreur de lecture', [ClassName]);
end;

procedure TVirtualReader.ReadTStrings(aStrings: TStrings);
var
  i:             Integer;
  objectsStored: Boolean;
begin
  aStrings.BeginUpdate;
  aStrings.Clear;
  objectsStored := ReadBoolean;
  i             := ReadInteger;
  if objectsStored then
    while i > 0 do
    begin
      aStrings.AddObject(ReadString, TObject(PtrUInt(ReadInteger)));
      Dec(i);
    end
  else
    while i > 0 do
    begin
      aStrings.Add(ReadString);
      Dec(i);
    end;
  aStrings.EndUpdate;
end;
{%endregion%}
{%region%-----[ Virtual Writer ]------------------------------------------------}

constructor TVirtualWriter.Create(Stream: TStream);
begin
  FStream := Stream;
end;

procedure TVirtualWriter.WriteTStrings(const aStrings: TStrings; storeObjects: Boolean = True);
var
  i: Integer;
begin
  WriteBoolean(storeObjects);
  if Assigned(aStrings) then
  begin
    WriteInteger(aStrings.Count);
    if storeObjects then
      for i := 0 to aStrings.Count - 1 do
      begin
        WriteString(aStrings[i]);
        WriteInteger(integer(aStrings.Objects[i].GetHashCode));
      end
    else
      for i := 0 to aStrings.Count - 1 do
        WriteString(aStrings[i]);
  end
  else
    WriteInteger(0);
end;

{%endregion%}
{%region%-----[ Virtual Binary Reader ]-----------------------------------------}

procedure TBinaryReader.Read(var Buf; Count: Longint);
begin
  FStream.Read(Buf, Count);
end;

function TBinaryReader.ReadValue: TValueType;
var
  b: Byte;
begin
  b:=0;
  Read(b, 1);
  Result := TValueType(b);
end;

function TBinaryReader.NextValue: TValueType;
var
  pos: Int64;
begin
  pos              := FStream.Position;
  Result           := ReadValue;
  FStream.Position := pos;
end;

function TBinaryReader.ReadInteger: Integer;
var
  tempShort:    ShortInt;
  tempSmallInt: SmallInt;
begin
  tempShort:=0;
  tempSmallInt:=0;
  case ReadValue of
    vaInt8:
      begin
        Read(tempShort, 1);
        Result := tempShort;
      end;
    vaInt16:
      begin
        Read(tempSmallInt, 2);
        Result := tempSmallInt;
      end;
    vaInt32:
      Read(Result, 4);
  else
    begin
      Result := 0;
      ReadTypeError;
    End;
  end;
end;

function TBinaryReader.ReadBoolean: Boolean;
begin
  case ReadValue of
    vaTrue:
      Result := True;
    vaFalse:
      Result := False;
  else
    begin
      ReadTypeError;
      Result := False;
    End;
  end;
end;

function TBinaryReader.ReadString: string;
var
  n:          Cardinal;
  vType:      TValueType;
  tempString: String;
begin
  n     := 0;
  vType := ReadValue;
  case Cardinal(vType) of
    Cardinal(vaWString), Cardinal(vaInt64) + 1:
      begin // vaUTF8String
        Result := String(ReadWideString(vType));
        Exit;
      end;
    Cardinal(vaString):
      Read(n, 1);
    Cardinal(vaLString):
      Read(n, 4);
  else
    ReadTypeError;
  end;
  SetLength(tempString{%H-}, n);
  if n > 0 then
    Read(tempString[1], n);
  Result := string(tempString);
end;

function TBinaryReader.ReadWideString(vType: TValueType): WideString;
var
  n:       Cardinal;
  utf8buf: AnsiString;
begin
  n:=0;
  Read(n, 4);
  case Cardinal(vType) of
    Cardinal(vaWString):
      begin
        SetLength(Result{%H-}, n);
        if n > 0 then
          Read(Result[1], n * 2);
      end;
    Cardinal(vaInt64) + 1:
      begin // vaUTF8String
        SetLength(utf8buf{%H-}, n);
        if n > 0 then
        begin
          Read(utf8buf[1], n);
          Result := utf8buf.ToWideString; //UTF8ToWideString(utf8buf);
        end;
      end;
  else
    ReadTypeError;
  end;
end;

function TBinaryReader.ReadFloat: Extended;
{$IFDEF CPU64}
var
   C  :TExtended80Rec; // Temporary variable to store 10 bytes floating point number in a Win64 application
{$ENDIF}
begin
  {$IFDEF CPU64}
  if ReadValue = vaExtended then
  begin
    Read({%H-}C, SizeOf(C));     // Load value into the temp variable
    Result := C.Float;
  end
  else
    ReadTypeError;
  {$ELSE}
  result:=0.0;
  if ReadValue = vaExtended then
    Read(Result, SizeOf(Result))
  else
    ReadTypeError;
  {$ENDIF}
end;

procedure TBinaryReader.ReadListBegin;
begin
  if ReadValue <> vaList then ReadTypeError;
end;

procedure TBinaryReader.ReadListEnd;
begin
  if ReadValue <> vaNull then ReadTypeError;
end;

function TBinaryReader.EndOfList: Boolean;
begin
  Result := (NextValue = vaNull);
end;

{%endregion%}
{%region%-----[ Virtual Binary Writer ]-----------------------------------------}

procedure TBinaryWriter.Write(const Buf; Count: Longint);
begin
  FStream.Write(Buf, Count);
end;

procedure TBinaryWriter.WriteInteger(anInteger: Integer);
type
  TIntStruct = packed record
    typ: Byte;
    val: Integer;
  end;
var
  ins: TIntStruct;
begin
  ins.val := anInteger;
  if (anInteger >= Low(ShortInt)) and (anInteger <= High(ShortInt)) then
  begin
    ins.typ := Byte(vaInt8);
    Write(ins, 2);
  end
  else if (anInteger >= Low(SmallInt)) and (anInteger <= High(SmallInt)) then
  begin
    ins.typ := Byte(vaInt16);
    Write(ins, 3);
  end
  else
  begin
    ins.typ := Byte(vaInt32);
    Write(ins, 5);
  end;
end;

procedure TBinaryWriter.WriteBoolean(aBoolean: Boolean);
const
  cBoolToType: array [False .. True] of Byte = (Byte(vaFalse), Byte(vaTrue));
begin
  Write(cBoolToType[aBoolean], 1);
end;

procedure TBinaryWriter.WriteAnsiString(const aString: AnsiString);
type
  TStringHeader = packed record
    typ: Byte;
    Length: Integer;
  end;
var
  sh: TStringHeader;
begin
  sh.Length := Length(aString);
  if sh.Length <= 255 then
  begin
    sh.typ := Byte(vaString);
    Write(sh, 2);
    if sh.Length > 0 then
      Write(aString[1], sh.Length);
  end
  else
  begin
    sh.typ := Byte(vaLString);
    Write(sh, 5);
    Write(aString[1], sh.Length);
  end;
end;

procedure TBinaryWriter.WriteWideString(const aString: WideString);
type
  TStringHeader = packed record
    typ: Byte;
    Length: Integer;
  end;
var
  sh: TStringHeader;
begin
  sh.Length := Length(aString);
  sh.typ    := Byte(vaWString);
  Write(sh, 5);
  if sh.Length > 0 then
    Write(aString[1], sh.Length * SizeOf(WideChar));
end;

procedure TBinaryWriter.WriteString(const aString: string);
begin
  {$IFDEF UNICODE}
  // TODO: should really check if the string can be simplified to: vaString / vaLString / vaUTF8String
  WriteWideString(aString);
  {$ELSE}
  WriteAnsiString(aString);
  {$ENDIF}
end;

procedure TBinaryWriter.WriteFloat(const aFloat: Extended);
type
  TExtendedStruct = packed record
    typ: Byte;
    {$IFDEF CPU64}
    val  :TExtended80Rec;  // Structure to handle a 10 bytes floating point value
    {$ELSE}
    val  :Extended;
    {$ENDIF}
  end;
var
  str: TExtendedStruct;
begin
  {$IFDEF CPU64}
  str.typ := byte(vaExtended);
  str.val.Float := aFloat;
  Write(str, SizeOf(str));
  {$ELSE}
  str.typ := byte(vaExtended);
  str.val := aFloat;
  Write(str, SizeOf(str));
  {$ENDIF}
end;

procedure TBinaryWriter.WriteListBegin;
const
  Buf: Byte = Byte(vaList);
begin
  Write(Buf, 1);
end;

procedure TBinaryWriter.WriteListEnd;
const
  Buf: Byte = Byte(vaNull);
begin
  Write(Buf, 1);
end;

{%endregion%}
{%region%-----[ Virtual Text Reader ]-------------------------------------------}

procedure TTextReader.ReadLine(const requestedType: string = '');
var
  line: string;
  c:    Byte;
  p:    Integer;
begin
  // will need speed upgrade, someday...
  c:=0;
  line := '';
  repeat
    Stream.Read(c, 1);
    if c >= 32 then
      line := line + chr(c);
  until c = 10;
  line := Trim(line);
  p    := pos(' ', line);
  if p > 0 then
  begin
    FValueType := Copy(line, 1, p - 1);
    FData      := Trim(Copy(line, p + 1, MaxInt));
  end
  else
  begin
    FValueType := line;
    FData      := '';
  end;
  if requestedType <> '' then
    if requestedType <> FValueType then
      raise EFilerException.Create('Invalid type, expected "' + requestedType + '", found "FValueType".');
end;

procedure TTextReader.Read(var Buf; Count: Longint);

  function HexCharToInt(const c: Char): Integer;
  begin
    if c <= '9' then
      Result := Integer(c) - Integer('0')
    else if c < 'a' then
      Result := Integer(c) - Integer('A') + 10
    else
      Result := Integer(c) - Integer('a') + 10;
  end;

var
  i, J: Integer;
begin
  ReadLine(cVTRaw);
  J     := 1;
  for i := 0 to Count - 1 do
  begin
    PAnsiChar(@Buf)[i] := AnsiChar((HexCharToInt(FData[J]) shl 4) + HexCharToInt(FData[J + 1]));
    Inc(J, 2);
  end;
end;

function TTextReader.NextValue: TValueType;
var
  p: Int64;
begin
  p := Stream.Position;
  ReadLine;
  if FValueType = cVTInteger then
    Result := vaInt32
  else if FValueType = cVTFloat then
    Result := vaExtended
  else if FValueType = cVTString then
    Result := vaString
  else if FValueType = cVTBoolean then
    if FData = cTrue then
      Result := vaTrue
    else
      Result := vaFalse
  else if FValueType = cVTRaw then
    Result := vaBinary
  else if FValueType = cVTListBegin then
    Result := vaList
  else
    Result        := vaNull;
  Stream.Position := p;
end;

function TTextReader.ReadInteger: Integer;
begin
  ReadLine(cVTInteger);
  Result := StrToInt(FData);
end;

function TTextReader.ReadBoolean: Boolean;
begin
  ReadLine(cVTBoolean);
  Result := (FData = cTrue);
end;

function TTextReader.ReadString: string;
var
  i: Integer;
begin
  ReadLine(cVTString);
  Result := '';
  i      := 1;
  while i < Length(FData) do
  begin
    if FData[i] = '#' then
    begin
      Result := Result + Char(StrToInt(Copy(FData, i + 1, 3)));
      Inc(i, 3);
    end
    else
      Result := Result + FData[i];
    Inc(i);
  end;
  Assert(FData[i] = '.', 'Invalid stored string.');
end;

function TTextReader.ReadFloat: Extended;
var
  oldDc: Char;
begin
  ReadLine(cVTInteger);
  oldDc := GetDecimalSeparator;
  SetDecimalSeparator('.');
  Result := StrToFloat(FData);
  SetDecimalSeparator(oldDc);
end;

procedure TTextReader.ReadListBegin;
begin
  ReadLine(cVTListBegin);
end;

procedure TTextReader.ReadListEnd;
begin
  ReadLine(cVTListEnd);
end;

function TTextReader.EndOfList: Boolean;
var
  p: Int64;
begin
  p := Stream.Position;
  ReadLine;
  Result          := (FValueType = cVTListEnd);
  Stream.Position := p;
end;

{%endregion%}
{%region%-----[ Virtual Text Writer ]-------------------------------------------}

constructor TTextWriter.Create(aStream: TStream);
begin
  inherited;
end;

destructor TTextWriter.Destroy;
begin
  inherited;
end;

procedure TTextWriter.WriteLine(const valueType, data: string);
var
  Buf: AnsiString;
begin
  Buf := StringOfChar(AnsiChar(#32), FIndentLevel);
  Buf := Buf + AnsiString(valueType + ' ' + data) + #13#10;
  Stream.Write(Buf[1], Length(Buf));
end;

procedure TTextWriter.Write(const Buf; Count: Longint);
const
  cNibbleToHex: PChar = '0123456789ABCDEF';
var
  i, J, b: Integer;
  data:    string;
begin
  SetLength(data{%H-}, Count * 2);
  J     := 1;
  for i := 0 to Count - 1 do
  begin
    b           := Integer(PAnsiChar(@Buf)[i]);
    data[J]     := cNibbleToHex[b shr 4];
    data[J + 1] := cNibbleToHex[b and 15];
    Inc(J, 2);
  end;
  WriteLine(cVTRaw, data);
end;

procedure TTextWriter.WriteInteger(anInteger: Integer);
begin
  WriteLine(cVTInteger, IntToStr(anInteger));
end;

procedure TTextWriter.WriteBoolean(aBoolean: Boolean);
begin
  if aBoolean then
    WriteLine(cVTBoolean, cTrue)
  else
    WriteLine(cVTBoolean, cFalse);
end;

procedure TTextWriter.WriteString(const aString: string);
var
  i: Integer;
  s: string;
begin
  s     := '';
  for i := 1 to Length(aString) do
    if aString[i] >= #32 then
      s := s + aString[i]
    else
      s := s + Format('#%.3d', [Integer(aString[i])]);
  WriteLine(cVTString, s + '.');
end;

procedure TTextWriter.WriteFloat(const aFloat: Extended);
begin
  WriteLine(cVTInteger, FloatToStr(aFloat));
end;

procedure TTextWriter.WriteListBegin;
begin
  WriteLine(cVTListBegin, '');
  Inc(FIndentLevel, 3);
end;

procedure TTextWriter.WriteListEnd;
begin
  Dec(FIndentLevel, 3);
  WriteLine(cVTListEnd, '');
end;

{%endregion%}

{%endregion%}

{%region%===[ TBZInterfacedPersistent ]=========================================}

function TBZInterfacedPersistent._AddRef: Integer; {$IFDEF WINDOWS} stdcall; {$ElSE} CDecl; {$Endif}
begin
  Result := -1; // ignore
end;

function TBZInterfacedPersistent._Release: Integer; {$IFDEF WINDOWS} stdcall; {$ElSE} CDecl; {$Endif}
begin
  Result := -1; // ignore
end;

function TBZInterfacedPersistent.QueryInterface(constref IID: TGUID; out Obj): HResult; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

{%endregion%}

{%region%===[ TBZPersistentObject ]=============================================}

constructor TBZPersistentObject.Create;
begin
  inherited Create;
end;

constructor TBZPersistentObject.CreateFromFiler(reader: TVirtualReader);
begin
  Create;
  ReadFromFiler(reader);
end;

destructor TBZPersistentObject.Destroy;
begin
  inherited Destroy;
end;

procedure TBZPersistentObject.Assign(source: TPersistent);
var
  ms: TStringStream; // plus rapide que TMemoryStream...
begin
  if source.ClassType = Self.ClassType then
  begin
    ms := TStringStream.Create('');
    try
      TBZPersistentObject(source).SaveToStream(ms);
      ms.Position := 0;
      LoadFromStream(ms);
    finally
      ms.Free;
    end;
  end
  else
    inherited;
end;

function TBZPersistentObject.CreateClone: TBZPersistentObject;
begin
  Result := TBZPersistentObjectClass(Self.ClassType).Create;
  Result.Assign(Self);
end;

class function TBZPersistentObject.FileSignature: string;
begin
  Result := '';
end;

class function TBZPersistentObject.FileVirtualWriter: TVirtualWriterClass;
begin
  Result := TBinaryWriter;
end;

class function TBZPersistentObject.FileVirtualReader: TVirtualReaderClass;
begin
  Result := TBinaryReader;
end;

procedure TBZPersistentObject.WriteToFiler(writer: TVirtualWriter);
begin
  // nothing
  Assert(Assigned(writer));
end;

procedure TBZPersistentObject.ReadFromFiler(reader: TVirtualReader);
begin
  // nothing
  Assert(Assigned(reader));
end;

procedure TBZPersistentObject.RaiseFilerException(const archiveVersion: Integer);
begin
  raise EFilerException.Create(ClassName +'Format de fichier invalide ! Version = ' + IntToStr(archiveVersion)); // :IGNORE
end;

function TBZPersistentObject.QueryInterface(constref IID: TGUID; out Obj): HResult; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TBZPersistentObject._AddRef: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  // ignore
  Result := 1;
end;

function TBZPersistentObject._Release: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  // ignore
  Result := 0;
end;

procedure TBZPersistentObject.SaveToStream(Stream: TStream; writerClass: TVirtualWriterClass = nil);
var
  wr:      TVirtualWriter;
  fileSig: AnsiString;
begin
  if writerClass = nil then
    writerClass := TBinaryWriter;
  wr            := writerClass.Create(Stream);
  try
    if FileSignature <> '' then
    begin
      fileSig := AnsiString(FileSignature);
      wr.Write(fileSig[1], Length(fileSig));
    end;
    WriteToFiler(wr);
  finally
    wr.Free;
  end;
end;

procedure TBZPersistentObject.LoadFromStream(Stream: TStream; readerClass: TVirtualReaderClass = nil);
var
  rd:  TVirtualReader;
  sig: AnsiString;
begin
  if readerClass = nil then
    readerClass := TBinaryReader;
  rd            := readerClass.Create(Stream);
  try
    if FileSignature <> '' then
    begin
      SetLength(sig{%H-}, Length(FileSignature));
      rd.Read(sig[1], Length(FileSignature));
      if sig <> AnsiString(FileSignature) then
        raise EInvalidFileSignature.Create('Structure de Fichier Invalide');
    end;
    ReadFromFiler(rd);
  finally
    rd.Free;
  end;
end;

function CreateFileStream(const fileName: string; mode: Word = fmOpenRead + fmShareDenyNone): TStream;
var
  fn:String;
begin
  fn:=FixPathDelimiter(fileName);
  Result:=nil;
  if ((mode and fmCreate)=fmCreate) or FileExists(fn) then
    Result:=TFileStreamUTF8.Create(fn, mode)    // TFileStreamUTF8
  else raise Exception.Create('File not found: "'+fn+'"');
end;

procedure TBZPersistentObject.SaveToFile(const fileName: string; writerClass: TVirtualWriterClass = nil);
var
  fs: TStream;
begin
  if writerClass = nil then
    writerClass := FileVirtualWriter;
  fs            := CreateFileStream(fileName, fmCreate);
  try
    SaveToStream(fs, writerClass);
  finally
    fs.Free;
  end;
end;

procedure TBZPersistentObject.LoadFromFile(const fileName: string; readerClass: TVirtualReaderClass = nil);
var
  fs: TStream;
begin
  if readerClass = nil then readerClass := FileVirtualReader;
  fs := CreateFileStream(fileName, fmOpenRead + fmShareDenyWrite);
  try
    LoadFromStream(fs, readerClass);
  finally
    fs.Free;
  end;
end;

function TBZPersistentObject.SaveToString(writerClass: TVirtualWriterClass = nil): string;
var
  ss: TStringStream;
begin
  ss := TStringStream.Create('');
  try
    SaveToStream(ss, writerClass);
    Result := ss.DataString;
  finally
    ss.Free;
  end;
end;

procedure TBZPersistentObject.LoadFromString(const data: string; readerClass: TVirtualReaderClass = nil);
var
  ss: TStringStream;
begin
  ss := TStringStream.Create(data);
  try
    LoadFromStream(ss, readerClass);
  finally
    ss.Free;
  end;
end;

{%endregion%}

{%region%===[ TBZPersistentObjectList ]=========================================}

constructor TBZPersistentObjectList.Create;
begin
  inherited Create;
  FGrowthDelta := cDefaultListGrowthDelta;
  FCount:=0;
end;

destructor TBZPersistentObjectList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TBZPersistentObjectList.Add(const Item: TObject): Integer;
begin
  Result := FCount;
  if Result <= FCapacity then SetCapacity(FCapacity + FGrowthDelta);
  FList^[Result] := Item;
  Inc(FCount);
end;

procedure TBZPersistentObjectList.AddNils(nbVals: Cardinal);
begin
  if Integer(nbVals) + Count > Capacity then SetCapacity(Integer(nbVals) + Count);
  FillChar(FList^[FCount], Integer(nbVals) * SizeOf(TObject), 0);
  FCount := FCount + Integer(nbVals);
end;

function TBZPersistentObjectList.AddObjects(const objectList: TBZPersistentObjectList): Integer;
begin
  if Assigned(objectList) then
  begin
    Result := FCount;
    SetCount(Result + objectList.Count);
    System.Move(objectList.FList^[0], FList^[Result], objectList.FCount * SizeOf(TObject));
  end
  else
    Result := 0;
end;

procedure TBZPersistentObjectList.RemoveObjects(const objectList: TBZPersistentObjectList);
var
  i: Integer;
begin
  for i := 0 to objectList.Count - 1 do Remove(objectList[i]);
end;

procedure TBZPersistentObjectList.Clear;
begin
  if Assigned(Self) and Assigned(FList) then
  begin
    SetCount(0);
    SetCapacity(0);
  end;
end;

procedure TBZPersistentObjectList.Delete(Index: Integer);
begin
  {$IFOPT R+}
  if Cardinal(Index) >= Cardinal(FCount) then raise EListError.Create('Liste Erreur Index' );  ;
  {$ENDIF}
  Dec(FCount);
  if Index < FCount then
  begin
    System.Move(FList^[index + 1], FList^[index], (FCount - index) * SizeOf(TObject));
  end;
end;

procedure TBZPersistentObjectList.DeleteItems(Index: Integer; nbVals: Cardinal);
begin
  {$IFOPT R+}
  Assert(Cardinal(index) < Cardinal(FCount));
  {$ENDIF}
  if nbVals > 0 then
  begin
    if index + Integer(nbVals) < FCount then
    begin
      System.Move(FList^[index + Integer(nbVals)], FList^[index], (FCount - index - Integer(nbVals)) * SizeOf(TObject));
    end;
    Dec(FCount, nbVals);
  end;
end;

procedure TBZPersistentObjectList.Exchange(Index1, Index2: Integer);
var
  Item:    TObject;
  locList: PPointerObjectList;
begin
  {$IFOPT R+}
  if (Cardinal(Index1) >= Cardinal(FCount)) or (Cardinal(Index2) >= Cardinal(FCount)) then raise EListError.Create('Liste Erreur Index' );
  {$ENDIF}
  locList          := FList;
  Item             := locList^[Index1];
  locList^[Index1] := locList^[Index2];
  locList^[Index2] := Item;
end;

function TBZPersistentObjectList.Expand: TBZPersistentObjectList;
begin
  if FCount = FCapacity then SetCapacity(FCapacity + FGrowthDelta);
  Result := Self;
end;

function TBZPersistentObjectList.GetFirst: TObject;
begin
  {.$IFOPT R+}
  if Cardinal(FCount) = 0 then raise EListError.Create('Liste Erreur Index' );
  {.$ENDIF}
  Result := FList^[0];
end;

procedure TBZPersistentObjectList.SetFirst(Item: TObject);
begin
  {$IFOPT R+}
  if Cardinal(FCount) = 0 then raise EListError.Create('Liste Erreur Index' );
  {$ENDIF}
  FList^[0] := Item;
end;


function TBZPersistentObjectList.Get(Index: Integer): TObject;
begin
  {.$IFOPT R+}
  if Cardinal(Index) >= Cardinal(FCount) then raise EListError.Create('Liste Erreur Index : '+Index.ToString() );
  {.$ENDIF}
  //GlobalLogger.LogStatus('TBZPersistentObjectList : '+Self.ClassName+' -->'+' GET Index'+Index.ToString());
  Result := FList^[Index];
end;

function TBZPersistentObjectList.IndexOf(Item: TObject): Integer;
{$IFDEF USE_ASM_OPTIMIZATIONS}
var
  c: Int64;
  p: ^TObject;
begin
  if FCount <= 0 then Result := -1
  else
  begin
    c := FCount;
    p := @FList^[0];
    asm

      {$IFDEF CPU64}
      mov rax, [Item];
      mov rcx, c;
      mov rdx, rcx;
      push rdi;
      mov rdi, p;
      {$ELSE}
      mov eax, Item;
      mov ecx, c;
      mov edx, ecx;
      push edi;
      mov edi, p;
      {$ENDIF}
      repne scasd;
      je @@FoundIt
      mov edx, -1;
      jmp @@SetResult;
    @@FoundIt:
      sub edx, ecx;
      dec edx;
    @@SetResult:
      mov Result, edx;
    {$IFDEF CPU64}
     pop rdi;
    {$ELSE}
    pop edi;
    {$ENDIF}

    end;
  end;
end;
{$ELSE}

var
  i: Integer;
begin
  if FCount <= 0 then
    Result := -1
  else
  begin
    Result := -1;
    for i  := 0 to FCount - 1 do
      if FList^[i] = Item then
      begin
        Result := i;
        Exit;
      end;
  end;
end;
{$ENDIF}

procedure TBZPersistentObjectList.Insert(Index: Integer; Item: TObject);
begin
  {$IFOPT R+}
  if Cardinal(index) >= Cardinal(FCount) then raise EListError.Create('Liste Erreur Index' );  ;
  {$ENDIF}
  if FCount = FCapacity then SetCapacity(FCapacity + FGrowthDelta);
  if Index < FCount then
    System.Move(FList^[index], FList^[index + 1], (FCount - index) * SizeOf(TObject));
  FList^[index] := Item;
  Inc(FCount);
end;

procedure TBZPersistentObjectList.InsertNils(Index: Integer; nbVals: Cardinal);
var
  nc: Integer;
begin
  {$IFOPT R+}
  if Cardinal(index) >= Cardinal(FCount) then raise EListError.Create('Liste Erreur Index' );
  {$ENDIF}
  if nbVals > 0 then
  begin
    nc := FCount + Integer(nbVals);
    if nc > FCapacity then SetCapacity(nc);
    if Index < FCount then
      System.Move(FList^[Index], FList^[Index + Integer(nbVals)], (FCount - Index) * SizeOf(TObject));
    FillChar(FList^[Index], Integer(nbVals) * SizeOf(TObject), 0);
    FCount := nc;
  end;
end;

function TBZPersistentObjectList.GetLast: TObject;
begin
  {$IFOPT R+}
  if Cardinal(FCount) = 0 then raise EListError.Create('Liste Erreur Index' );
  {$ENDIF}
  Result := FList^[FCount - 1];
end;

procedure TBZPersistentObjectList.SetLast(Item: TObject);
begin
  {$IFOPT R+}
  if Cardinal(FCount) = 0 then raise EListError.Create('Liste Erreur Index' );
  {$ENDIF}
  FList^[FCount - 1] := Item;
end;

procedure TBZPersistentObjectList.Move(CurIndex, NewIndex: Integer);
var
  Item: Pointer;
begin
  if CurIndex <> NewIndex then
  begin
    {$IFOPT R+}
    if Cardinal(NewIndex) >= Cardinal(Count) then raise EListError.Create('Liste Erreur Index' );
    if Cardinal(CurIndex) >= Cardinal(Count) then raise EListError.Create('Liste Erreur Index' );
    {$ENDIF}
    Item := FList^[CurIndex];
    if CurIndex < NewIndex then
    begin
      // curIndex+1 necessarily exists since curIndex<newIndex and newIndex<Count
      System.Move(List^[CurIndex + 1], List^[CurIndex], (NewIndex - CurIndex) * SizeOf(TObject));
    end
    else
    begin
      // newIndex+1 necessarily exists since newIndex<curIndex and curIndex<Count
      System.Move(List^[NewIndex], List^[NewIndex + 1], (CurIndex - NewIndex) * SizeOf(TObject));
    end;
    FList^[NewIndex] := TObject(Item);
  end;
end;

procedure TBZPersistentObjectList.Put(Index: Integer; Item: TObject);
begin
  {$IFOPT R+}
  if Cardinal(Index) >= Cardinal(FCount) then raise EListError.Create('Liste Erreur Index' );
  {$ENDIF}
  FList^[Index] := Item;
end;

function TBZPersistentObjectList.Remove(Item: TObject): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then Delete(Result);
end;

procedure TBZPersistentObjectList.Pack;
var
  i, J, n: Integer;
  p:       PPointerObjectList;
  pk:      PObject;
begin
  p := List;
  n := Count - 1;
  while (n >= 0) and (p^[n] = nil) do Dec(n);
  for i := 0 to n do
  begin
    if p^[i] = nil then
    begin
      pk    := @(p^[i]);
      for J := i + 1 to n do
      begin
        if p^[J] <> nil then
        begin
          pk^ := p^[J];
          Inc(pk);
        end;
      end;
      SetCount(({%H-}PtrUInt(pk) - {%H-}PtrUInt(p)) div SizeOf(TObject));
      Exit;
    end;
  end;
  SetCount(n + 1);
end;

procedure TBZPersistentObjectList.SetCapacity(newCapacity: Integer);
begin
  if newCapacity <> FCapacity then
  begin
    if newCapacity <= FCount then FCount := newCapacity;
    memReAlloc(FList, newCapacity * SizeOf(TObject));
    FCapacity := newCapacity;
  end;
end;

procedure TBZPersistentObjectList.RequiredCapacity(aCapacity: Integer);
begin
  if FCapacity < aCapacity then SetCapacity(aCapacity);
end;

procedure TBZPersistentObjectList.SetCount(NewCount: Integer);
begin
  if NewCount > FCapacity then SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(TObject), 0);
  FCount := NewCount;
end;

procedure TBZPersistentObjectList.DeleteAndFree(Index: Integer);
var
  Obj: TObject;
begin
  if Index<=FCount then exit;
  Obj := Get(index);
  Delete(index);
  Obj.Free;
end;

procedure TBZPersistentObjectList.DeleteAndFreeItems(Index: Integer; nbVals: Cardinal);
var
  i, n: Integer;
begin
  {$IFOPT R+}
  Assert(Cardinal(index) < Cardinal(FCount));
  {$ENDIF}
  n := index + Integer(nbVals);
  if n >= FCount then
    n   := FCount - 1;
  for i := index to n do FList^[i].Free;
  DeleteItems(index, nbVals);
end;

function TBZPersistentObjectList.RemoveAndFree(Item: TObject): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
  begin
    Delete(Result);
    Item.Free;
  end;
end;

procedure TBZPersistentObjectList.DoClean;
var
  i: Integer;
begin
  // a 'for' loop could crash if freeing an item removes other items form the list
  if FCount < 1 then exit;
  i := FCount - 1;
  while i >= 0 do
  begin
    if i < FCount then FList^[i].Free;
    Dec(i);
  end;
end;

procedure TBZPersistentObjectList.Clean;
begin
  DoClean;
  Clear;
end;

procedure TBZPersistentObjectList.CleanFree;
begin
  if Self <> nil then
  begin
    Clean;
    Destroy;
  end;
end;

procedure TBZPersistentObjectList.WriteToFiler(writer: TVirtualWriter);
(*
  Object List Filer Format :

  Integer (Version)
  ListBegin
  ...[Object]...[Object]...
  ListEnd

  with [Object] being either (read vertically)

  Boolean (unused)        String (ClassName)        Integer (reference)
  Integer                 Object Data               Object Data
*)
var
  i, objId: Integer;
  objTypes: TList;
  aType:    TClass;
begin
  objTypes := TList.Create;
  try
    with writer do
    begin
      WriteInteger(0); // Archive Version 0 (uh... not exactly... but...)
      WriteListBegin;
      for i := 0 to FCount - 1 do
      begin
        if FList^[i] = nil then
        begin
          // store nil as... nil
          WriteBoolean(False);
          WriteInteger(0);
        end
        else if (FList^[i] is TBZPersistentObject) then
        begin
          // yeah, a TBZPersistentObject
          aType := FList^[i].ClassType;
          objId := objTypes.IndexOf(aType);
          if objId < 0 then
          begin
            // class is unknown
            objTypes.Add(aType);
            WriteString(aType.ClassName);
          end
          else
          begin
            // class already registered
            WriteInteger(objId);
          end;
          TBZPersistentObject(FList^[i]).WriteToFiler(writer);
        end
        else
        begin
          // Dunno that stuff here, store as is
          WriteBoolean(False);
          WriteInteger((FList^[i].GetHashCode));
        end;
      end;
      WriteListEnd;
    end;
  finally
    objTypes.Free;
  end;
end;

procedure TBZPersistentObjectList.ReadFromFilerWithEvent(reader: TVirtualReader; afterSenderObjectCreated: TNotifyEvent);
var
  Obj:      TBZPersistentObject;
  m:        TBZPersistentObjectClass;
  version:  Integer;
  objTypes: TList;
begin
  objTypes := TList.Create;
  try
    Clean;
    with reader do
    begin
      version := ReadInteger;
      if version = 0 then
      begin
        ReadListBegin;
        while not EndOfList do
          case Cardinal(NextValue) of
            Cardinal(vaFalse), Cardinal(vaTrue):
              begin
                // stored 'as was' value
                ReadBoolean; // ignored
                Add(TObject(PtrUInt(ReadInteger)));
              end;
            Cardinal(vaString), Cardinal(vaLString), Cardinal(vaWString), Cardinal(vaInt64) + 1 { vaUTF8String } :
              begin
                // Unknown class, to be registered
                m := TBZPersistentObjectClass(FindClass(ReadString));
                objTypes.Add(m);
                Obj := m.Create;
                if Assigned(afterSenderObjectCreated) then
                  afterSenderObjectCreated(Obj);
                Obj.ReadFromFiler(reader);
                Add(Obj);
              end;
            Cardinal(vaInt8), Cardinal(vaInt16), Cardinal(vaInt32):
              begin
                // known class, direct retrieve
                m   := TBZPersistentObjectClass(objTypes[ReadInteger]);
                Obj := m.Create;
                if Assigned(afterSenderObjectCreated) then
                  afterSenderObjectCreated(Obj);
                Obj.ReadFromFiler(reader);
                Add(Obj);
              end;
          else
            raise Exception.Create('Archive Corrompue');
          end;
        ReadListEnd;
      end
      else
        RaiseFilerException(version);
    end;
  finally
    objTypes.Free;
  end;
end;

procedure TBZPersistentObjectList.ReadFromFiler(reader: TVirtualReader);
begin
  ReadFromFilerWithEvent(reader, @AfterObjectCreatedByReader);
end;

procedure TBZPersistentObjectList.AfterObjectCreatedByReader(Sender: TObject);
begin
  // nothing
end;

procedure TBZPersistentObjectList.Push(Item: TObject);
begin
  Add(Item);
end;

function TBZPersistentObjectList.Pop: TObject;
begin
  if FCount > 0 then
  begin
    Result := FList^[FCount - 1];
    Dec(FCount);
  end
  else
    Result := nil;
end;

procedure TBZPersistentObjectList.PopAndFree;
begin
  Pop.Free;
end;

procedure POListQuickSort(SortList: PPointerObjectList; L, R: Integer; compareFunc: TObjectListSortCompare);
var
  i, J: Integer;
  p, T: TObject;
begin
  repeat
    i := L;
    J := R;
    p := SortList^[(L + R) shr 1];
    repeat
      while compareFunc(SortList^[i], p) < 0 do Inc(i);
      while compareFunc(SortList^[J], p) > 0 do Dec(J);
      if i <= J then
      begin
        T            := SortList^[i];
        SortList^[i] := SortList^[J];
        SortList^[J] := T;
        Inc(i);
        Dec(J);
      end;
    until i > J;
    if L < J then
      POListQuickSort(SortList, L, J, compareFunc);
    L := i;
  until i >= R;
end;

procedure TBZPersistentObjectList.Sort(compareFunc: TObjectListSortCompare);
begin
  if Count > 1 then POListQuickSort(FList, 0, Count - 1, compareFunc);
end;

{%endregion%}

{%region%===[ TBZOwnedPersistent ]==============================================}

constructor TBZOwnedPersistent.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
end;

function TBZOwnedPersistent.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{%endregion%}

{%region%===[ TBZInterfacedCollectionItem ]=====================================}

function TBZInterfacedCollectionItem._AddRef: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := -1; // ignore
end;

function TBZInterfacedCollectionItem._Release: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := -1; // ignore
end;

function TBZInterfacedCollectionItem.QueryInterface(constref IID: TGUID; out Obj): HResult; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

{%endregion%}

{%region%===[ TBZUpdateAbleObject ]=============================================}

Constructor TBZUpdateAbleObject.Create;
Begin
  Inherited Create;
  FOwner := nil;
End;

Constructor TBZUpdateAbleObject.Create(AOwner: TPersistent);
Begin
  Inherited Create;
  if (AOwner<>nil) then FOwner := AOwner;
End;

Procedure TBZUpdateAbleObject.NotifyChange(Sender: TObject);
Begin
  If FUpdating = 0 Then
  Begin
    If Assigned(Owner) Then
    Begin
      If Owner Is TBZUpdateAbleObject Then TBZUpdateAbleObject(Owner).NotifyChange(Self)
      Else If Owner Is TBZUpdateAbleComponent Then TBZUpdateAbleComponent(Owner).NotifyChange(Self);
    End;
    If Assigned(FOnChange) Then FOnChange(Self);
  End;
End;

Procedure TBZUpdateAbleObject.Notification(Sender: TObject; Operation: TOperation);
Begin
  // Ne fait rien
End;

Function TBZUpdateAbleObject.GetOwner: TPersistent;
Begin
  Result := FOwner;
End;

Procedure TBZUpdateAbleObject.BeginUpdate;
Begin
  Inc(FUpdating);
End;

Procedure TBZUpdateAbleObject.EndUpdate;
Begin
  Dec(FUpdating);
  If FUpdating <= 0 Then
  Begin
    Assert(FUpdating = 0);
    NotifyChange(Self);
  End;
End;

{%endregion%}

{%region%===[ TBZProgressAbleObject ]===========================================}

{ Adapté de GraphicEX

  Puisque le chargement d'une image ou autre, implique souvent beaucoup de traitement, il est difficile
  de fournir à l'utilisateur des informations de progression utiles.
  Ceci est principalement dû à l'impossibilité de préciser la valeur du pourcentage global qu'un traitement particulier a besoin et / ou quand il fini.
  TBZProgressAbleObject implémente une gestion avancée qui prend les sections appelées comme intervalle de base.
  Une section est la quantité de pourcentages qu'un traitement prendra dans l'interval 0..100% par rapport à sa «section parente».
  L'augmentation de la progression est toujours compter 'localement' (c'est-à-dire dans la section actuelle du traitement).
  De cette façon, un processus particulier peut toujours passer de 0 à 100% et les étapes sont
  automatiquement transformées en une valeur globale en fonction de la taille des sections.
}
procedure TBZProgressAbleObject.AdvanceProgress(Amount: Single; OffsetX, OffsetY: Integer; DoRedraw: Boolean);
{
  Évalue la section de progression actuelle en pourcentage (0..100%).
  La signification des paramètres dans la méthode est:
   - Amount     Valeur utilisée pour augmenter la position de progression actuelle de la section (0..100%)
   - OffsetX,
     OffsetY  : Valeurs pour compenser le rectangle de progression
   - DoRedraw : Indique à l'application de mettre à jour son affichage.
}
var
  CurrentSection: PBZProgressSection;
 // GoAway : Boolean;
  NewAmount:Single;

  { Cette Fonction est présente dans LCLProcs et dans BZGraphics
    Juste pour ne pas avoir de référence circulaires avec BZGraphics }
  function InternalOffSetRect(var ARect: TRect; dx,dy: Integer): Boolean;
  Begin
    with ARect do
    begin
      Left := Left + dx;
      Right := Right + dx;
      Top := Top + dy;
      Bottom := Bottom + dy;
    end;
    Result := (ARect.Left >= 0) and (ARect.Top >= 0);
  end;

begin
  Assert(Assigned(FProgressStack), 'Start progress display first using InitProgress.');
  Assert(FProgressStack.Count > 0, 'Initialize a progress section first using StartProgressSection.');
  // Advance the top section.
  CurrentSection := FProgressStack.Peek;
  NewAmount := Amount / 100;
  // Ensure that we never exceed the 100% limit.
  if CurrentSection^.Position + NewAmount > 1 then
  begin
    NewAmount := 1 - CurrentSection^.Position;
    CurrentSection^.Position := 1;
  end
  else
    CurrentSection^.Position := CurrentSection^.Position + NewAmount;

  // Sum up the section's percents under consideration of the section size.
  FPercentDone := FPercentDone + CurrentSection^.TransformFactor * NewAmount;
  InternalOffsetRect(FProgressRect, OffsetX, OffsetY);
  Progress(Self, opsRunning, Trunc(100 * FPercentDone), DoRedraw, FProgressRect, CurrentSection^.Msg, ContinueProgress);
end;

// Vide et libère les sections la pile de progression actuelle et se libère ensuite.
procedure TBZProgressAbleObject.ClearProgressStack;
var
  CurrentSection: PBZProgressSection;
begin
  if Assigned(FProgressStack) then
  begin
    while FProgressStack.Count > 0 do
    begin
      CurrentSection := FProgressStack.Pop;
      Dispose(CurrentSection);
    end;
    FreeAndNil(FProgressStack);
  end;
end;

{ Termine la section actuelle et la supprime de la pile de progression.
  La section parente est mise à jour en supposant que la progression de
  cette section soit exactement de 100% (quel que soit la valeur réelle). }
procedure TBZProgressAbleObject.FinishProgressSection(DoRedraw: Boolean);
var
  Percent: Single;
  CurrentSection,
  ParentSection: PBZProgressSection;

begin
  Assert(Assigned(FProgressStack), 'Start progress display first using InitProgress.');
  Assert(FProgressStack.Count > 0, 'Initialize a progress section first using StartProgressSection.');

  CurrentSection := FProgressStack.Pop;
  if FProgressStack.Count = 0 then
    FreeAndNil(FProgressStack)
  else
  begin
    // Update position of the parent section.
    ParentSection := FProgressStack.Peek;
    if ParentSection^.Position + CurrentSection^.ParentSize > 1 then
      ParentSection^.Position := 1
    else
      ParentSection^.Position :=  ParentSection^.Position + CurrentSection^.ParentSize;
  end;

  // Update the overall percent value.
  Percent := 1 - CurrentSection^.Position;
  if Percent > 0 then FPercentDone := FPercentDone + CurrentSection^.TransformFactor * Percent;
  Dispose(CurrentSection);

  if FProgressStack = nil then
    Progress(Self, opsEnding, Trunc(100 * FPercentDone), DoRedraw, FProgressRect, '', ContinueProgress)
  else
    Progress(Self, opsRunning, Trunc(100 * FPercentDone), DoRedraw, FProgressRect, '',ContinueProgress);
end;

// Initialise toutes les variables relative à la progrssion
procedure TBZProgressAbleObject.InitProgress(AWidth, AHeight: Integer);
begin
  ClearProgressStack;
  FProgressStack := TStack.Create;
  FProgressRect := Rect(0, 0, AWidth, AHeight);
  FPercentDone := 0;
end;

{  Démarre une nouvelle section de progression dans la section actuelle.
   La taille détermine la taille que la nouvelle section prend dans la section actuelle
   et doit être donnée en Pourcentage (0..100%).
   Si la taille est 0, le reste complet de la section actuelle est pris.
    - S est le message à utiliser pour la progressioin de la section
}
procedure TBZProgressAbleObject.StartProgressSection(Size: Single; const S: string);
var
  CurrentSection,
  NewSection: PBZProgressSection;
begin
  Assert(Assigned(FProgressStack), 'Start progress display first using InitProgress.');

  New(NewSection);
  if FProgressStack.Count = 0 then
  begin
    // This is the first (root) section.
    NewSection^.ParentSize := 1;
    NewSection^.TransformFactor := 1;
  end
  else
  begin
    CurrentSection := FProgressStack.Peek;
    if Size = 0 then
      NewSection^.ParentSize := 1 - CurrentSection^.Position
    else
      NewSection^.ParentSize := Size / 100;

    NewSection^.TransformFactor := CurrentSection^.TransformFactor * NewSection^.ParentSize;
  end;
 // GlobalLogger.LogStatus('NewSection.TransformFactor = '+FloattoStr(NewSection^.TransformFactor));
 // GlobalLogger.LogStatus('NewSection.ParentSize = '+FloattoStr(NewSection^.ParentSize));

  NewSection^.Position := 0;
  NewSection^.Msg := S;
  //GlobalLogger.LogNotice('Start Progress Section : FPercentDone = '+FloattoStr(FPercentDone)+' ( '+InttoStr(Round(FPercentDone*100))+'% )');
  FProgressStack.Push(NewSection);
  if FProgressStack.Count = 1 then
    Progress(Self, opsStarting, Trunc(100 * FPercentDone), False, FProgressRect, S,ContinueProgress)
  else
    Progress(Self, opsRunning, Trunc(100 * FPercentDone), False, FProgressRect, S,ContinueProgress);
end;

Procedure TBZProgressAbleObject.Progress(Sender: TObject; Stage: TBZProgressStage;
                                      PercentDone: Byte;  RedrawNow: Boolean; const R: TRect;
                                      const Msg: String; var aContinue: Boolean);
begin

  If Assigned(FOnProgress) then
  begin
   // GlobalLogger.LogStatus(Msg+' --> Percent Done : '+InttoStr(PercentDone));
    FOnProgress(Sender,Stage,PercentDone,RedrawNow,R,Msg,aContinue);
  End;
end;

{%endregion%}

{%region%===[ TBZThreadAbleObject ]=============================================}

constructor TBZThreadAbleObject.Create;
begin
  inherited Create;
  System.InitCriticalSection(CriticalSection);
end;

destructor TBZThreadAbleObject.Destroy;
begin
  System.DoneCriticalsection(CriticalSection);
  inherited Destroy;
end;

procedure TBZThreadAbleObject.Lock;
begin
  InterlockedIncrement(FLockCount);
  System.EnterCriticalsection(CriticalSection);
end;

procedure TBZThreadAbleObject.Unlock;
begin
  System.LeaveCriticalsection(CriticalSection);
  InterlockedDecrement(FLockCount);
end;

{%endregion%}

{%region%===[ TBZCadenceAbleComponent ]=========================================}

Procedure TBZCadenceAbleComponent.DoProgress(Const progressTime: TBZProgressTimes);
Begin
  // nothing
End;

{%endregion%}

{%region%===[ TBZUpdateAbleComponent ]==========================================}

Procedure TBZUpdateAbleComponent.NotifyChange(Sender: TObject);
Begin
  If Assigned(Owner) Then
    If (Owner Is TBZUpdateAbleComponent) Then (Owner As TBZUpdateAbleComponent).NotifyChange(Self);
End;

{%endregion%}

{%region%===[ TBZNotifyCollection ]=============================================}

Constructor TBZNotifyCollection.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
Begin
  Inherited Create(AOwner, AItemClass);
  If Assigned(AOwner) And (AOwner Is TBZUpdateAbleComponent) Then
    OnNotifyChange := @TBZUpdateAbleComponent(AOwner).NotifyChange;
End;

Procedure TBZNotifyCollection.Update(item: TCollectionItem);
Begin
  Inherited;
  If Assigned(FOnNotifyChange) Then FOnNotifyChange(Self);
End;

{%endregion%}

{%region%===[ TBZCustomDataFile ]===============================================}

Constructor TBZDataFileFormatDesc.Create;
Begin
  Inherited Create;
  FName := 'Tous';
  FDesc := 'Tous les fichiers';
  FVersion := '0.0';
  FEncoding := etNone;
  FFileMask := '*.*';
End;

Destructor TBZDataFileFormatDesc.Destroy;
Begin
  Inherited Destroy;
End;

procedure TBZDataFileFormatDesc.Assign(Source : TBZDataFileFormatDesc);
begin
  FName := Source.Name;
  FDesc := Source.Desc;
  FVersion := Source.Version;
  FEncoding := Source.Encoding;
  FFileMask := Source.FileMask;
end;

Constructor TBZCustomDataFile.Create;
Begin
  inherited  Create;
  FDataFileFormatDesc := nil;
  FDataFileFormatDesc := TBZDataFileFormatDesc.Create;
  FData := nil;
  FResourceName := '';
  FFullFileName := '';
  FErrorCount := 0;
  FErrorList := TStringList.Create;
  //FOnLoadError := nil;
End;

Constructor TBZCustomDataFile.Create(AOwner: TPersistent);
Begin
  Inherited Create(AOwner);
  FDataFileFormatDesc := nil;
  FDataFileFormatDesc := TBZDataFileFormatDesc.Create;
  FData := nil;
  FResourceName := '';
  FFullFileName := '';
  FErrorCount := 0;
  FErrorList := TStringList.Create;
  //FOnLoadError := nil;
End;

Destructor TBZCustomDataFile.Destroy;
Begin
  FreeAndNil(FErrorList);
  FDataFileFormatDesc.Free;
  FData := nil;
  Inherited Destroy;
End;

Class Function TBZCustomDataFile.Capabilities: TBZDataFileCapabilities;
Begin
  Result := [dfcRead];
End;

Function TBZCustomDataFile.CreateCopy(AOwner: TPersistent): TBZCustomDataFile;
Begin
  If Self <> nil Then
    Result := TBZDataFileClass(Self.ClassType).Create(AOwner)
  Else
    Result := nil;
End;

Procedure TBZCustomDataFile.Assign(Source : TPersistent);
begin
  if (Source Is TBZCustomDataFile) then
  begin
   // GlobalLogger.LogNotice('TBZCustomDataFile.Assign : FileName = '+TBZCustomDataFile(Source).FullFileName);
    FFullFileName := TBZCustomDataFile(Source).FullFileName;
    FResourceName := TBZCustomDataFile(Source).ResourceName;
    FDataFileFormatDesc.Assign(TBZCustomDataFile(Source).DataFormatDesc);
  End;
End;

Procedure TBZCustomDataFile.LoadFromFile(Const FileName: String);
Var
  fs: TStream;
  fn:String;
Begin
  fn := FixPathDelimiter(filename);
  ResourceName := ExtractFileName(fn);
  FFullFileName := Fn;
  {$IFDEF DEBUG}GlobalLogger.LogNotice('TBZCustomDataFile.LoadFromFile : FileName = '+FullFileName);{$ENDIF}
  fs := CreateFileStream(fn, fmOpenRead + fmShareDenyNone);
  Try
     LoadFromStream(fs);
  Finally
    fs.Free;
  End;
End;

Procedure TBZCustomDataFile.SaveToFile(Const fileName: String);
Var
  fs: TStream;
  fn : String;
Begin
  fn := FixPathDelimiter(filename);
  ResourceName := ExtractFileName(fn);
  fs := CreateFileStream(fn, fmCreate);
  Try
    SaveToStream(fs);
    // WriteFromMemory
  Finally
    fs.Free;
  End;
End;

Function TBZCustomDataFile.CanLoad(Const FileName: String): Boolean;
Var
  Stream: TFileStream;
Begin
  Stream := TFileStream.Create(FixPathDelimiter(filename), fmOpenRead Or fmShareDenyWrite);
  Try
    Result := CanLoad(Stream);
  Finally
    Stream.Free;
  End;
End;

Function TBZCustomDataFile.CheckFormat(): Boolean;
Begin
  Result := True;
End;

Function TBZCustomDataFile.CanLoad(aStream: TStream): Boolean;
Begin
  If Not (assigned(FData)) Then
  Begin
    FData := TBZBufferedStream.Create(aStream);
  End
  else FData.AssignStream(aStream);
  Result := CheckFormat;
End;

Procedure TBZCustomDataFile.InternalLoadFromStream(aStream: TStream);
Begin
  FData := TBZBufferedStream.Create(aStream);
  Try
    If CheckFormat Then LoadFromMemory;
  Finally
    FreeAndNil(FData);
  End;
End;

Procedure TBZCustomDataFile.InternalSaveToStream(aStream: TStream);
Begin
  FData := TBZBufferedStream.Create(aStream);
  Try
    SaveToMemory;
    Memory.Save;
  Finally
    FreeAndNil(FData);
  End;
end;

Procedure TBZCustomDataFile.LoadFromStream(aStream: TStream);
Begin
  InternalLoadFromStream(aStream);
End;

Procedure TBZCustomDataFile.LoadFromMemory;
Begin
  Assert(False, 'Import de ' + ClassName + ' vers ' + Self.ClassName + ' non implenté.');
End;

Procedure TBZCustomDataFile.SaveToStream(aStream: TStream);
Begin
  InternalSaveToStream(aStream);
End;

Procedure TBZCustomDataFile.SaveToMemory;
Begin
  Assert(False, 'Export de ' + ClassName + ' vers ' + Self.ClassName + ' non implenté.');
End;

Procedure TBZCustomDataFile.AddError(Msg: String);
Begin
  FErrorList.Add(Msg);
  inc(FErrorCount);
End;

Procedure TBZCustomDataFile.NotifyError;
Begin
  If (FErrorCount>0) then
  begin
    If Assigned(FOnLoadError) then
    begin
       FOnLoadError(Self, FErrorCount,FErrorList);
     //ShowMessage(FErrorList.Text); //, [mbOK],0);
    End;
  End;
End;

Procedure TBZCustomDataFile.Initialize;
Begin
  // Ne fait rien
End;


{%endregion%}

{%region%===[ TBZCustomHashListItem ]===========================================}

Constructor TBZCustomHashListItem.Create;
Begin
  Inherited Create;
  FOwner := nil;
  FText := '';
  //FHashText:=-1;
  FTag := 0;
End;

Constructor TBZCustomHashListItem.Create(aOwner: TBZCustomHashStringList);
Begin
  Inherited Create;
  FOwner := aOwner;
End;

Procedure TBZCustomHashListItem.SetText(aValue: String);
Begin
  If aValue = FText Then exit;
  FText := aValue;
 (* if assigned(FOwner) then
  begin
    if FOwner.CaseSensitive then FHashText:= ComputeSensitiveStringHash(aValue)
    else FHashText:= ComputeInSensitiveStringHash(aValue);
  end;*)

End;

(*function TBZCustomHashListItem.GetHashText:Integer;
begin
  result:=FHashText;
end; *)

Procedure TBZCustomHashListItem.WriteToFiler(writer: TVirtualWriter);
Begin
  Inherited WriteToFiler(writer);
  With writer Do
  Begin
    WriteInteger(0); // Archive Version 0
    WriteString(FText);
    // WriteInteger(FHashText);
    WriteInteger(FTag);
  End;
End;


Procedure TBZCustomHashListItem.ReadFromFiler(reader: TVirtualReader);
Var
  archiveVersion: Integer;
Begin
  Inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  If archiveVersion = 0 Then
    With reader Do
    Begin
      FText := ReadString;
      // FHashText := ReadInteger;
      FTag := ReadInteger;
    End
  Else
    RaiseFilerException(archiveVersion);
End;


{%endregion%}

{%region%===[ TBZCustomHashStringList ]=========================================}

Function TBZCustomHashStringList.getDirectList : PBZCustomHashListItemArray;
Begin
  //FDirectList :=  PBZStringListItemArray(FItemList.List);
  Result := FDirectList;
End;

Function TBZCustomHashStringList.GetItem(Index : Integer) : TBZCustomHashListItem;
Begin
  Result := TBZCustomHashListItem(FItemList.Items[Index]);
End;

Procedure TBZCustomHashStringList.SetItem(Index : Integer; Const Value : TBZCustomHashListItem);
Begin
  FItemList.Items[Index] := Value;
End;

Function TBZCustomHashStringList.GetStringText(Index : Integer) : String;
Begin
  Result := GetItem(Index).Text;
End;

Procedure TBZCustomHashStringList.SetStringText(Index : Integer; Const Value : String);
Begin
  TBZCustomHashListItem(FItemList.Items[Index]).Text := Value;
End;

Function TBZCustomHashStringList.GetObjectRef(Index : Integer) : TObject;
Begin
  Result := GetItem(Index).ObjectRef;
End;

Procedure TBZCustomHashStringList.SetObjectRef(Index : Integer; Const Value : TObject);
Begin
  TBZCustomHashListItem(FItemList.Items[Index]).ObjectRef := Value;
End;

Procedure TBZCustomHashStringList.AfterItemCreated(Sender : TObject);
Begin
  (Sender As TBZCustomHashListItem).Owner := Self;
End;

Constructor TBZCustomHashStringList.Create(AOwner : TPersistent);
var
  i : Integer;
Begin
  Inherited Create(AOwner);

  FItemList := nil;
  FItemList := TBZPersistentObjectList.Create;
  FItemList.GrowthDelta := 4096;
  // FItemList.Capacity:=256;
  FDirectList := nil;
  FCaseSensitive := False;
  FIsSorted := False;
  for i := 0 to cHash2 - 1 do
  begin
    FHashArray[i]:=-255;
  end;
  //for i:=Low(FHashArray) to High(FHashArray) do FHashArray[I]:=-1;
End;

Destructor TBZCustomHashStringList.Destroy;
Begin
  FItemList.CleanFree;
  FreeAndNil(FDirectList);
  Inherited Destroy;
End;

Function TBZCustomHashStringList.Add(Const Value : TBZCustomHashListItem) : Integer;
Var
  Hash: Integer;
  //S:String;
Begin
 // S:= Value.text;
  result :=-1;

  Hash := Value.Text.ComputeHash(not(FCaseSensitive));

  Result := FItemList.Add(Value);
  //Collision := (FHashArray[Hash]=result)
  //SetLength(FHashArray[Hash], Collision +1);
  FHashArray[Hash] := Result;
  FIsSorted := False;
End;

Function TBZCustomHashStringList.Add(Const s : String) : Integer;
Var
  aItem: TBZCustomHashListItem;
Begin
  Result := -1;
  aItem := TBZCustomHashListItem.Create(Self);
  aItem.Text := S;
  aItem.ObjectRef := nil;
  aItem.Tag := 0;
  Result := Self.Add(aItem);
End;

Function TBZCustomHashStringList.Add(Const s : String; Const aTag : String) : Integer;
Var
  aItem: TBZCustomHashListItem;
Begin
  Result := -1;
  aItem := TBZCustomHashListItem.Create(Self);
  aItem.Text := S;
  aItem.ObjectRef := nil;
  aItem.Tag := 0;
  aItem.TagText := aTag;
  Result := Add(aItem);
end;

Function TBZCustomHashStringList.AddInt(Const s : String; Const aTag : Integer) : Integer;
Var
  aItem: TBZCustomHashListItem;
Begin
  Result := -1;
  aItem := TBZCustomHashListItem.Create(Self);
  aItem.Text := S;
  aItem.ObjectRef := nil;
  aItem.Tag := aTag;
  Result := Add(aItem);
End;

Function TBZCustomHashStringList.Add(Const s : String; Const aTag : Single) : Integer;
Var
  aItem: TBZCustomHashListItem;
Begin
  Result := -1;
  aItem := TBZCustomHashListItem.Create(self);
  aItem.Text := S;
  aItem.ObjectRef := nil;
  aItem.Tag := 0;
  aItem.TagFloat := aTag;
  Result := Add(aItem);
end;

Function TBZCustomHashStringList.Add(Const s : String; AObject : TObject) : Integer;
Var
  aItem: TBZCustomHashListItem;
Begin
  Result := -1;
  aItem := TBZCustomHashListItem.Create(self);
  aItem.Text := S;
  aItem.ObjectRef := AObject;
  aItem.Tag := 0;
  Result := Add(aItem);
End;

procedure TBZCustomHashStringList.Clear;
Var
  i : Integer;
begin
  //if Assigned(FItemList) and (FItemList.Count> 0) then
  //begin
  //  FItemList.Clean;
  //end;
  for i := 0 to cHash2 - 1 do
   begin
     FHashArray[i]:=-255;
   end;
end;

Procedure TBZCustomHashStringList.UpdateDirectList;
Begin
  FDirectList := PBZCustomHashListItemArray(FItemList.List);
End;

Procedure TBZCustomHashStringList.WriteToFiler(writer : TVirtualWriter);
Begin
  // inherited WriteToFiler(writer);
  With writer Do
  Begin
    WriteInteger(0); // Archive Version 0
    FItemList.WriteToFiler(writer);
  End;
End;

Procedure TBZCustomHashStringList.ReadFromFiler(reader : TVirtualReader);
Var
  archiveVersion: Integer;
Begin
  // inherited ReadFromFiler(reader);
  FIsSorted := False;
  archiveVersion := reader.ReadInteger;
  If archiveVersion = 0 Then
    With reader Do
    Begin
      FItemList.ReadFromFilerWithEvent(reader, @AfterItemCreated);
      Sort;
    End
  Else
    Raise Exception.Create('ID Corrompu');
End;

Function TBZCustomHashStringList.Count : Integer;
Begin
  Result := FItemList.Count;
End;

Function TBZCustomHashStringList.IndexOf(Const aText : String) : Integer;
Var
    i         :integer;
  Hash: Integer;
Begin

  Hash := aText.ComputeHash(not(FCaseSensitive));

  //Si pas de collision (rapide)
  // if Length(FHashArray[Hash]) = 1 then
  Result := FHashArray[Hash];
 // GlobalLogger.LogNotice('===> TBZCustomHashStringList.IndexOf = '+Result.toString);
(*  else
  begin
    // Collision impossible de déterminer le bon élément
    // Baisse des performance on doit parcourir la liste et comparer les texte ala mano
     for i := 0 to High(FHashArray[Hash]) do
    begin
      Result := FHashArray[Hash,i];
      if (aText=TBZCustomHashListItem(FItemList[Result]).Text) then Exit;
    end;
    GlobalLogger.LogWarning('Collision');
    result:=-1;
  end; *)

  { NOTE : Le test de collision est mis en veille :
     Le nombre maximal de hash différents avant colision est de :
       en 64bits : MaxInt shr 4 = 134 217 727
       en 32bits : MaxInt shr 4 =

    Largement suffisant ici je pense. Sinon il faut rajouter une 2eme dimension au tableau de haschage.
    Et ainsi de suite pour augmenter la capacité de stockage des "Hash". Il faudra donc
    changer le tablaeu actuel par un tableau dynamique ou un "Pointer".
    Merci AndNotOr :
    https://www.developpez.net/forums/d1687750/autres-langages/pascal/lazarus/generer-nombre-unique-chaine-caracteres/#post9225345
  }
End;



{ Version amélioré d'après
  http://www.thedelphigeek.com/2006/10/faster-than-speed-of-binary-search.html
  et //http://www.swissdelphicenter.ch/en/showcode.php?id=1916
}
Function TBZCustomHashStringList.Find(Const S : String; Var index : Integer) : Boolean;
Var
  HashKey, Pivot, First, Last: Integer;
  found: Boolean;
Begin
  Result := False;
(* if Count = 0 then exit;
 if not(FIsSorted) then Sort;
 Index:=Count;
 Found  := False;
 result:=false;
 First:=0;
 Last:=Count-1;

 // On génère le hash de S pour la comparaison
 if FCaseSensitive then HashKey:=ComputeSensitiveStringHash(S)
 else HashKey:=ComputeInSensitiveStringHash(S);

 // Quelques vérifications basiques
 // On vérifie avec le 1er item
 if HashKey = FDirectList^[0].getHashText then   //if S=GetItem(0).Text then
 begin
   index := 0;
   Result := True;
   Exit;
 end;
 // On vérifie avec le dernier item
 if HashKey = FDirectList^[Last].getHashText then     //if S=GetItem(Count-1).Text then
 begin
   index := Count-1;
   Result := true;
   Exit;
 end;
 // On borne par rapport à l'item du milieu de la liste
 // On choisi donc quelle partie à chercher dans la liste
 // On réduit ainsi deja par 2 le nombre items à parcourir lors de la recherche
 Pivot :=(Count div 2);
 if HashKey = FDirectList^[Pivot].getHashText then  //if S=GetItem(Pivot).Text then
 begin
   Index:=Pivot;
   Result:=true;
   Exit;
 end
 else if HashKey > FDirectList^[Pivot].getHashText then
 begin
   First:=Pivot;
 end
 else
 begin
   Last:=Pivot;
 end;
 Index:=Last;
 repeat
    // On borne par rapport à l'item du milieu de la tranche actuel
    Pivot := ((First + Last) div 2);
    // On a trouvé on s'en va
    if HashKey =FDirectList^[Pivot].getHashText then
    begin
      Index:=Pivot;
      Result:=true;
      Exit;
    end
    else  // On marque les nouvelles bornes de recherche
    if HashKey > FDirectList^[Pivot].getHashText then
    begin
      First:=Pivot;
      Dec(Index);
    end
    else
    begin
      Last:=Pivot-1;
      Index:=Last; // On laisse comme ca pour pouvoir sortir de la boucle Index < 0. On a n'a pas trouvé l'élément
      if Last<0 then Last:=0; // si négatif provoque une erreur

    end;
    // On vérifie si notre position est valide
    if (Index>=First) and (Index<=Last) then Found:=(FDirectList^[Index].getHashText=HashKey);
 until (Index<First) or (Index<0) or found;
 if found then result:=true; *)
End;

Function TBZCustomHashStringList.IndexOfText(Const s : String) : Integer;
Begin
  //Result := -1;
  Result := Self.IndexOf(s); //find(s, Result);
End;

Function TBZCustomHashStringList.IndexOfItem(aItem : TBZCustomHashListItem) : Integer;
Begin
  Result := FItemList.IndexOf(aItem);
End;

Procedure TBZCustomHashStringList.RemoveAndFreeItem(aItem : TBZCustomHashListItem);
Var
  i: Integer;
Begin
  i := FItemList.IndexOf(aItem);
  If i >= 0 Then
  Begin
    If aItem.Owner = Self Then
      aItem.Owner := nil;
    aItem.Free;
    FItemList.List^[i] := nil;
  End;
  FIsSorted := False;
End;

Procedure TBZCustomHashStringList.Pack;
Begin
  FItemList.Pack;
End;

Function TBZCustomHashStringList.CompareStringListItemHashText(item1, item2: TObject): Integer;
Begin
  //  result:=TBZCustomHashListItem(item1).getHashText-TBZCustomHashListItem(item2).getHashText;
  Result := 0;
End;

Function TBZCustomHashStringList.CompareStringListItemText(item1, item2: TObject): Integer;
Begin
  Result := CompareStr(TBZCustomHashListItem(item1).Text, TBZCustomHashListItem(item2).Text);
End;

Procedure TBZCustomHashStringList.DirectSort;

  Procedure AlphaSort(Var A: TBZCustomHashListItemArray; Lo, Hi: Int64);
  {-}
  Var
    I:      Integer;
    LLimit, HLimit: Int64;
    CharProcessed: Integer;
    TmpPtr: Int64;
    Last:   Int64;
    LetterTracker: ^LetterArray;
    WordTracker: ^WordArray;

    Procedure SecondPass(Var NextPtr: Int64; CurrentLetter, Last: Int64);
    {-}
    Var
      EmptyPtr: Int64;
      TmpPtr:   Int64;
      CharProcessed: Int64;
      LLimit, HLimit: Int64;
      RangePtr: ^RangeArray;
    Begin
      RangePtr := @LetterTracker^[CurrentLetter];
      EmptyPtr := 0;
      LLimit := MaxInt;
      HLimit := 0;
      Repeat
        TmpPtr := NextPtr;
        NextPtr := WordTracker^[TmpPtr];
        If CurrentLetter <= Length(A[TmpPtr].Text) Then
        Begin
          If Not (FCaseSensitive) Then
            CharProcessed := Ord(Upcase(A[TmpPtr].Text[CurrentLetter]))
          Else
            CharProcessed := Ord(A[TmpPtr].Text[CurrentLetter]);
          If CharProcessed < LLimit Then
            LLimit := CharProcessed;
          If CharProcessed > HLimit Then
            HLimit := CharProcessed;
          WordTracker^[TmpPtr] := RangePtr^[CharProcessed];
          RangePtr^[CharProcessed] := TmpPtr;
        End
        Else
        Begin
          WordTracker^[TmpPtr] := EmptyPtr;
          EmptyPtr := TmpPtr;
        End;
      Until NextPtr = 0;

      Inc(HLimit);
      Repeat
        Dec(HLimit);
        NextPtr := RangePtr^[HLimit];
        If (NextPtr <> 0) Then
        Begin
          If WordTracker^[NextPtr] <> 0 Then
          Begin
            If CurrentLetter <> MaxStrLength Then
              SecondPass(NextPtr, CurrentLetter + 1, Last) {recursive call}
            Else
            Begin  {maximum string length reached, so we stop the process}
              {link other strings}
              TmpPtr := NextPtr;
              While WordTracker^[TmpPtr] <> 0 Do
                TmpPtr := WordTracker^[TmpPtr];

              WordTracker^[TmpPtr] := Last;
            End;
          End
          Else
            WordTracker^[NextPtr] := Last; {link}
          Last := NextPtr;
          RangePtr^[HLimit] := 0;
        End;
      Until HLimit <= LLimit;

      If EmptyPtr <> 0 Then
      Begin                  {link empty strings for this level}
        NextPtr := EmptyPtr;
        While WordTracker^[EmptyPtr] <> 0 Do
          EmptyPtr := WordTracker^[EmptyPtr];

        WordTracker^[EmptyPtr] := Last;
      End;
    End;

  Begin
    {-Initialize data structures}
    GetMem(WordTracker, SizeOf(WordArray));
    fillChar(WordTracker^, ((Hi - Lo) + 1) * SizeOf(Word), 0);
    GetMem(LetterTracker, SizeOf(LetterArray));
    If LetterTracker = nil Then
      Exit;
    fillChar(LetterTracker^, SizeOf(LetterArray), 0);
    {-Read the list of strings}
    LLimit := MaxInt;
    HLimit := 0;
    For I := Lo To Hi Do
    Begin
      If Not (FCaseSensitive) Then
        CharProcessed := Ord(Upcase(A[I].Text[1]))
      Else
        CharProcessed := Ord(A[I].Text[1]);
      If CharProcessed < LLimit Then LLimit := CharProcessed;
      If CharProcessed > HLimit Then HLimit := CharProcessed;
      WordTracker^[I] := LetterTracker^[1, CharProcessed];
      LetterTracker^[1, CharProcessed] := I;
    End;
    Last := 0;
    For I := HLimit Downto LLimit Do
    Begin
      TmpPtr := LetterTracker^[1, I];
      If TmpPtr <> 0 Then
      Begin
        { MaxStrLength must be greater than or equal to 2 }
        If WordTracker^[TmpPtr] <> 0 Then
          SecondPass(TmpPtr, 2, Last) {recursive call}
        Else
          WordTracker^[TmpPtr] := Last; {link}
        Last := TmpPtr;
      End;
    End;
    FreeMem(WordTracker, SizeOf(WordArray));
    FreeMem(LetterTracker, SizeOf(LetterArray));
  End;

Begin
  // FItemList.Sort(@CompareStringListItemHashText);
  // FDirectList :=  PBZCustomHashListItemArray(FItemList.List);
  AlphaSort(FDirectList^, 0, Count - 1);
  FIsSorted := True;
End;

Procedure TBZCustomHashStringList.Sort;
Begin
  // Trie de base QuickSort très long
  FItemList.Sort(@CompareStringListItemHashText);
  // FDirectList :=  PBZStringListItemArray(FItemList.List);
  FIsSorted := True;
End;

{%endregion%}

{%region%=====[ TBZStringList ]=================================================}

Constructor TBZStringList.Create(AOwner: TPersistent);
Begin
  Inherited Create(AOwner);
  With DataFormatDesc Do
  Begin
    Name := 'Text';
    Desc := 'Texte';
    FileMask := '*.*;*.txt';
  End;
  FContent := '';
  Line := 0;
End;

Procedure TBZStringList.LoadFromMemory();
Begin
  FContent := '';
  FContent := Memory.getBufferAsString;
End;

Class Function TBZStringList.Capabilities: TBZDataFileCapabilities;
Begin
  Result := [dfcRead];
End;

Function TBZStringList.CheckDataIsText(Const aBuf: Pointer): Boolean;
Var
  Buf: String;
  i, Len: Integer;
  NewLine: Boolean;
  p: PChar;
  ZeroAllowed: Boolean;

Begin
  //ShowMessage('Verification DataIsText');
  Result := False;

  Len := 1024;
  SetLength(Buf{%H-}, Len + 1);
  Len := SizeOf(aBuf);

  If Len > 0 Then
  Begin
    Buf[Len + 1] := #0;

    p := PChar(aBuf);
    ZeroAllowed := False;
    If (p[0] = #$EF) And (p[1] = #$BB) And (p[2] = #$BF) Then
    Begin
      //ShowMessage('UTF-8 BOM (Byte Order Mark)');
      Inc(p, 3);
    End
    Else If (p[0] = #$FF) And (p[1] = #$FE) Then
    Begin
      //ShowMessage('ucs-2le BOM FF FE');
      Inc(p, 2);
      ZeroAllowed := True;
    End
    Else If (p[0] = #$FE) And (p[1] = #$FF) Then
    Begin
      //ShowMessage('ucs-2be BOM FE FF');
      Inc(p, 2);
      ZeroAllowed := True;
    End
    Else
    Begin
      ZeroAllowed := True;
      //ShowMessage('Ansi String');
    End;

    NewLine := False;
    i := 0;
    While (i < len) Do
    Begin
      Case p^ Of
        #0:
          If p - PChar(Buf) >= Len Then
            break
          Else If Not ZeroAllowed Then
          Begin
            Raise Exception.Create('Erreur DataIsText ZeroAllowed');
            exit;
          End;
        // #12: form feed
        // #26: end of file
        #1..#7, #11, #14..#25, #27..#31:
        Begin
          Raise Exception.Create('Erreur DataIsText Invalide CHAR : ' + IntToStr(Ord(p^)) + ' pos : ' + IntToStr(i));
          exit;
        End;
        #10, #13: NewLine := True;
      End;
      Inc(p);
      Inc(i);
    End;
    If NewLine Or (Len < 1024) Then
      Result := True
    Else
      Raise Exception.Create('Erreur DataIsText Longueur invalide');
  End;
End;

Function TBZStringList.CheckFormat(): Boolean;
Begin
  Result := true; // CheckDataIsText(Memory.getBuffer);
End;

{%endregion%}

initialization

  // Enregistrement des classes
  RegisterClass(TBZPersistentObjectList); // + les autres

End.
