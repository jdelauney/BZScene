(*
  @abstract(Contient les classes et collections pour la gestion des sources audio et des effets)

  -------------------------------------------------------------------------------------------------------------

  @created(2016-11-16)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(05/11/2017 : Creation  )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) :


  -------------------------------------------------------------------------------------------------------------

  @bold(Dépendances) : BZClasses, BZVectorMath, BZSoundSample, BZXCollection, BZCadencer, BZUtils, BZMath, BZLIbOpenAL

  -------------------------------------------------------------------------------------------------------------

  Credits :
   @unorderedList(
     @item (Codé sur une base de GLScene http://www.sourceforge.net/glscene)
   )

  -------------------------------------------------------------------------------------------------------------

  @bold(Licence) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZSound;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils,
  BZClasses, BZVectorMath, BZSoundSample, BZXCollection, BZCadencer;

type
  // TBZAbstractSoundFXLibrary = class;
   TBZSoundFXLibrary = class;
   TBZSoundFXName = string;

  (* IBZSoundFXLibrarySupported = interface(IInterface)
     ['{D9457925-AD4E-4854-BE8B-790D54AC2AF9}']
     function GetSoundFXLibrary: TBZAbstractSoundFXLibrary;
   end; *)

  { Stores a single PCM coded sound sample. }
  TBZSoundSampleItem = class(TCollectionItem) //TBZXCollectionItem //@TODO à renommer en TBZSoundSampleItemItem
  private
     
    FName: string;
    FData: TBZSoundSample;
    FTag: Integer;

  protected
     
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream); virtual;
    procedure WriteData(Stream: TStream); virtual;
    function GetDisplayName: string; override;
    procedure SetData(const val: TBZSoundSample);

  public
     
    constructor Create(aCollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure LoadFromFile(const fileName: string);

    // This Tag is reserved for sound manager use only
    property ManagerTag: Integer read FTag write FTag;

  published
     
    property Name: string read FName write FName;
    property Data: TBZSoundSample read FData write SetData stored False;

  end;

  { Gestion d'une collection d'échantillons audio }
  TBZSoundSampleItems = class(TCollection) //TBZXCollection
  protected
     
    FOwner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TBZSoundSampleItem);
    function GetItems(index: Integer): TBZSoundSampleItem;

  public
     
    constructor Create(AOwner: TComponent);
    function Add: TBZSoundSampleItem;
    function FindItemID(ID: Integer): TBZSoundSampleItem;
    function GetByName(const aName: string): TBZSoundSampleItem;
    function AddFile(const fileName: string; const sampleName: string = ''): TBZSoundSampleItem;

    property Items[index: Integer]: TBZSoundSampleItem read GetItems write SetItems; default;
  end;

  { Composant non-visuel pour gérer une liste d'échantillon audio }
  TBZSoundLibrary = class(TComponent)
  private
    FSamples: TBZSoundSampleItems;
  protected
    procedure SetSamples(const val: TBZSoundSampleItems);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Samples: TBZSoundSampleItems read FSamples write SetSamples;
  end;

  TBZSoundSourceChange = (sscTransformation, sscSample, sscStatus, sscFXStatus, sscNewSample);  //sscEffects, sscFilters
  TBZSoundSourceChanges = set of TBZSoundSourceChange;
  TBZSoundSourceFXChange = (sfxcDirectFilter, sfxEffectSlot0Status, sfxEffectFiltering, sfxcFilter);  //sscEffects, sscFilters
  TBZSoundSourceFXChanges = set of TBZSoundSourceFXChange;

  //TBZBSoundEmitter = class;

  { Description d' un effet audio }
  TBZCustomSoundFXItem = class(TBZXCollectionItem)//, IBZSoundFXLibrarySupported)
  private
    FTag: PtrUInt;
  protected
    procedure SetName(const val: string); override;
    {Override this function to write subclass data. }
    procedure WriteToFiler(Awriter: TWriter); override;
    {Override this function to read subclass data. }
    procedure ReadFromFiler(Areader: TReader); override;
  public
    constructor Create(aOwner: TBZXCollection); override;
    destructor Destroy; override;

    class function FriendlyName: String; override;
    //procedure DoProgress(const progressTime: TProgressTimes); virtual;
    // This Tag is reserved for sound manager use only

    property ManagerTag: PtrUInt read FTag write FTag;

  end;

  { Classe de type TBZCustomSoundFXItem }
  BZSoundFXItemClass = class of TBZCustomSoundFXItem;

  { Classe abstraite à hériter pour la gestion de filtres audio }
  TBZCustomSoundFilterItem = class(TBZCustomSoundFXItem)
  public
    class function FriendlyName: String; override;
  End;

  { Déclaration d'un filtre de type "Low-pass" }
  TBZLowPassSoundFilter =  Class(TBZCustomSoundFilterItem)
  private
    FGAIN : Single;
    FGAINHF : Single;
  protected
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  public
    constructor Create(AOwner: TBZXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    class function FriendlyName: String; override;

  published
    property Gain : Single read FGain Write FGain;
    property GainHF : Single read FGainHF Write FGainHF;
  end;

  { Déclaration d'un filtre de type "High-pass" }
  TBZHighPassSoundFilter =  Class(TBZCustomSoundFilterItem)
  private
    FGAIN : Single;
    FGAINLF : Single;
  protected
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  public
    constructor Create(AOwner: TBZXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    class function FriendlyName: String; override;

  published
    property Gain : Single read FGain Write FGain;
    property GainLF : Single read FGainLF Write FGainLF;
  end;

  { Déclaration d'un filtre de type "Band-pass" }
  TBZBandPassSoundFilter =  Class(TBZCustomSoundFilterItem)
  private
    FGAIN : Single;
    FGAINHF : Single;
    FGAINLF : Single;
  protected
    procedure WriteToFiler(AWriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;
  public
    constructor Create(AOwner: TBZXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    class function FriendlyName: String; override;

  published
    property Gain : Single read FGain Write FGain;
    property GainLF : Single read FGainLF Write FGainLF;
    property GainHF : Single read FGainLF Write FGainHF;
  end;

  { Classe abstraite à hériter pour la gestion d'effets audio }
  TBZCustomSoundEffectItem = class(TBZCustomSoundFXItem)
  private
    FFilterName : String;
    FFilteringActivated : Boolean;

  protected

  public
    function GetFilter : TBZCustomSoundFilterItem;

    class function FriendlyName: String; override;
  published

    property FilterName : String read FFilterName Write FFilterName;
    property FilteringActivated : Boolean read FFilteringActivated write FFilteringActivated;
  end;

  { EAX standard sound environments. (112 presets) }
  TBZSoundReverbEnvironment = (
    seGeneric, sePaddedCell, seRoom, seBathroom,
    seLivingRoom, seStoneroom, seAuditorium,
    seConcertHall, seCave, seArena, seHangar,
    seCarpetedHallway, seHallway, seStoneCorridor,
    seAlley, seForest, seCity, seMountains, seQuarry,
    sePlain, seParkingLot, seSewerPipe, seUnderWater,
    seDrugged, seDizzy, sePsychotic,
    seCastle_SmallRoom, seCastle_ShortPassage, seCastle_MediumRoom,
    seCastle_LargeRoom, seCastle_LongPassage, seCastle_Hall, seCastle_CupBoarb,
    seCastle_CourtYard, seCastle_Alcove,
    seFactory_SmallRoom, seFactory_ShortPassage, seFactory_MediumRoom,
    seFactory_LargeRoom, seFactory_LongPassage, seFactory_Hall, seFactory_CupBoard,
    seFactory_CourtYard, seFactory_Alcove,
    seIcePalace_SmallRoom, seIcePalace_ShortPassage, seIcePalace_MediumRoom,
    seIcePalace_LargeRoom, seIcePalace_LongPassage, seIcePalace_Hall, seIcePalace_CupBoard,
    seIcePalace_CourtYard, seIcePalace_Alcove,
    seSpaceStation_SmallRoom, seSpaceStation_ShortPassage, seSpaceStation_MediumRoom,
    seSpaceStation_LargeRoom, seSpaceStation_LongPassage, seSpaceStation_Hall, seSpaceStation_CupBoard,
     seSpaceStation_Alcove, //seSpaceStation_CourtYard,
    seWooden_SmallRoom, seWooden_ShortPassage, seWooden_MediumRoom,
    seWooden_LargeRoom, seWooden_LongPassage, seWooden_Hall, seWooden_CupBoard,
    seWooden_CourtYard, seWooden_Alcove,
    seSport_EmptyStadium, seSport_SquashCourt, seSport_SmallSwimmingPool,
    seSport_LargeSwimmingPool, seSport_Gymnasium, seSport_FullStadium,
    seSport_StadiumTannoy,
    sePrefab_WorkShop, sePrefab_SchoolRoom, sePrefab_PractiseRoom, sePrefab_OutHouse,
    sePrefab_Caravan, seDome_Tomb,
    sePipe_Small, seDome_SaintPauls, sePipe_Longthin,
    sePipe_Large, sePipe_Resonant,
    seOutDoors_BackYard, seOutDoors_RollingPlains, seOutDoors_DeepCanyon,
    seOutDoors_Creek, seOutDoors_Valley,
    seMood_Heaven, seMood_Hell, seMood_Memory,
    seDriving_Commentator, seDriving_PitGarage, seDriving_Incar_Racer,
    seDriving_Incar_Sports, seDriving_Incar_Luxury, seDriving_FullGrandStand,
    seDriving_EmptyGrandStand, seDriving_Tunnel,
    seCity_Streets, seCity_SubWay, seCity_Museum, seCity_Library, seCity_UnderPass,
    seCity_Abandoned, seDustyRoom, seChapel, seSmallWaterRoom);

  { Description de l'effet audio "Reverb" }
  TBZReverbSoundEffect =  Class(TBZCustomSoundEffectItem)
  private
    FPreset : TBZSoundReverbEnvironment;
    FDENSITY : Single;
    FDIFFUSION : Single;
    FGAIN : Single;
    FGAINHF : Single;
    FDECAY_TIME : Single;
    FDECAY_HFRATIO : Single;
    FREFLECTIONS_GAIN : Single;
    FREFLECTIONS_DELAY : Single;
    FLATE_REVERB_GAIN : Single;
    FLATE_REVERB_DELAY : Single;
    FAIR_ABSORPTION_GAINHF : Single;
    FROOM_ROLLOFF_FACTOR : Single;
    FDECAY_HFLIMIT : Boolean;

    procedure SetPreset(val : TBZSoundReverbEnvironment);
  protected
    procedure WriteToFiler(Awriter: TWriter); override;
    procedure ReadFromFiler(AReader: TReader); override;

  public
    constructor Create(AOwner: TBZXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    class function FriendlyName: String; override;
  published
    property Preset : TBZSoundReverbEnvironment read FPreset write SetPreset;

    property DENSITY : Single read FDensity write FDensity;
    property DIFFUSION : Single read FDiffusion write FDiffusion;
    property GAIN : Single read FGain write FGain;
    property GAINHF : Single read FGainHF write FGainHF;
    property DECAY_TIME : Single read FDecay_Time write FDecay_Time;
    property DECAY_HFRATIO : Single read FDecay_HFRatio write FDecay_HFRatio;
    property REFLECTIONS_GAIN : Single read FReflections_Gain write FReflections_Gain;
    property REFLECTIONS_DELAY : Single read FReflections_Delay write FReflections_Delay;
    property LATE_REVERB_GAIN : Single read FLate_Reverb_Gain write FLate_Reverb_Gain;
    property LATE_REVERB_DELAY : Single read FLate_Reverb_Delay write FLate_Reverb_Delay;
    property AIR_ABSORPTION_GAINHF : Single read FAir_Absorption_GainHF write FAir_Absorption_GainHF;
    property ROOM_ROLLOFF_FACTOR : Single read FRoom_RollOff_Factor write FRoom_RollOff_Factor;
    property DECAY_HFLIMIT : Boolean read FDecay_HFLimit write FDecay_HFLimit;
  end;

  { Description de l'effet audio "EAX Reverb" }
  TBZEAXReverbSoundEffect =  Class(TBZCustomSoundEffectItem)
  private
    FPreset : TBZSoundReverbEnvironment;

    FDENSITY : Single;
    FDIFFUSION : Single;
    FGAIN : Single;
    FGAINHF : Single;
    FGAINLF : Single;
    FDECAY_TIME : Single;
    FDECAY_HFRATIO : Single;
    FDECAY_LFRATIO : Single;
    FREFLECTIONS_GAIN : Single;
    FREFLECTIONS_DELAY : Single;
  //  FREFLECTIONS_PAN : TBZVector;
    FLATE_REVERB_GAIN : Single;
    FLATE_REVERB_DELAY : Single;
  //  FLATE_REVERB_PAN : TBZVector;
    FECHO_TIME : Single;
    FECHO_DEPTH : Single;
    FMODULATION_TIME : Single;
    FMODULATION_DEPTH : Single;
    FAIR_ABSORPTION_GAINHF : Single;
    FHF_REFERENCE : Single;
    FLF_REFERENCE : Single;
    FROOM_ROLLOFF_FACTOR : Single;
    FDECAY_HFLIMIT : Boolean;

    procedure SetPreset(val : TBZSoundReverbEnvironment);

  protected
    procedure WriteToFiler(Awriter: TWriter);  override;
    procedure ReadFromFiler(Areader: TReader); override;
  public
    constructor Create(AOwner: TBZXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    class function FriendlyName: String; override;
  published
    property Preset : TBZSoundReverbEnvironment read FPreset write SetPreset;

    property DENSITY : Single read FDensity write FDensity;
    property DIFFUSION : Single read FDiffusion write FDiffusion;
    property GAIN : Single read FGain write FGain;
    property GAINHF : Single read FGainHF write FGainHF;
    property GAINLF : Single read FGainHF write FGainLF;
    property DECAY_TIME : Single read FDecay_Time write FDecay_Time;
    property DECAY_HFRATIO : Single read FDecay_HFRatio write FDecay_LFRatio;
    property DECAY_LFRATIO : Single read FDecay_LFRatio write FDecay_LFRatio;
    property REFLECTIONS_GAIN : Single read FReflections_Gain write FReflections_Gain;
    property REFLECTIONS_DELAY : Single read FReflections_Delay write FReflections_Delay;
  //  property REFLECTIONS_PAN : TBZVector read FReflections_Pan write FReflections_Pan;
    property LATE_REVERB_GAIN : Single read FLate_Reverb_Gain write FLate_Reverb_Gain;
    property LATE_REVERB_DELAY : Single read FLate_Reverb_Delay write FLate_Reverb_Delay;
   // property LATE_REVERB_PAN : TBZVector read FLate_Reverb_Delay write FLate_Reverb_Pan;
    property ECHO_TIME : Single read FEcho_Time write FEcho_Time;
    property ECHO_DEPTH : Single read FEcho_Depth write FEcho_Depth;
    property MODULATION_TIME : Single read FModulation_Time write FModulation_Time;
    property MODULATION_DEPTH : Single read FModulation_Depth write FModulation_Depth;
    property AIR_ABSORPTION_GAINHF : Single read FAir_Absorption_GainHF write FAir_Absorption_GainHF;
    property HF_REFERENCE : Single read FHF_Reference write FHF_Reference;
    property LF_REFERENCE : Single read FLF_Reference write FLF_Reference;
    property ROOM_ROLLOFF_FACTOR : Single read FRoom_RollOff_Factor write FRoom_RollOff_Factor;
    property DECAY_HFLIMIT : Boolean read FDecay_HFLimit write FDecay_HFLimit;

  end;

  { Description de l'effet audio "Chorus" }
  TBZChorusSoundEffect =  Class(TBZCustomSoundEffectItem)
  private
  protected
    procedure WriteToFiler(Awriter: TWriter); override;
    procedure ReadFromFiler(Areader: TReader); override;
  public
    constructor Create(AOwner: TBZXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    class function FriendlyName: String; override;
  end;

  { Description de l'effet audio "Echo" }
  TBZEchoSoundEffect =  Class(TBZCustomSoundEffectItem)
  private
  protected
    procedure WriteToFiler(Awriter: TWriter); override;
    procedure ReadFromFiler(Areader: TReader); override;
  public
    constructor Create(AOwner: TBZXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    class function FriendlyName: String; override;
  end;

  { Description de l'effet audio "Distortion" }
  TBZDistortionSoundEffect =  Class(TBZCustomSoundEffectItem)
  private
  protected
    procedure WriteToFiler(Awriter: TWriter); override;
    procedure ReadFromFiler(Areader: TReader); override;
  public
    constructor Create(AOwner: TBZXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    class function FriendlyName: String; override;
  end;

  { Description de l'effet audio "Flanger" }
  TBZFlangerSoundEffect =  Class(TBZCustomSoundEffectItem)
  private
  protected
    procedure WriteToFiler(Awriter: TWriter); override;
    procedure ReadFromFiler(Areader: TReader); override;
  public
    constructor Create(AOwner: TBZXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    class function FriendlyName: String; override;
  end;

  { Description de l'effet audio "Compressor" }
  TBZCompressorSoundEffect =  Class(TBZCustomSoundEffectItem)
  private
  protected
    procedure WriteToFiler(Awriter: TWriter); override;
    procedure ReadFromFiler(Areader: TReader); override;
  public
    constructor Create(AOwner: TBZXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    class function FriendlyName: String; override;
  end;

  { Description de l'effet audio "Equalizer" }
  TBZEqualizerSoundEffect =  Class(TBZCustomSoundEffectItem)
  private
  protected
    procedure WriteToFiler(Awriter: TWriter); override;
    procedure ReadFromFiler(Areader: TReader); override;
  public
    constructor Create(AOwner: TBZXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    class function FriendlyName: String; override;
  end;

  { Gestion d'une collection d'effets audio }
  TBZSoundFXLib = class(TBZXCollection)
  protected

    function GetItem(index: Integer): TBZCustomSoundFXItem;
  public

    function GetNamePath: string; override;
    class function ItemsClass: TBZXCollectionItemClass; override;

    property FXItems[index: Integer]: TBZCustomSoundFXItem read GetItem; default;

    function CanAdd(aClass: TBZXCollectionItemClass): Boolean; override;
    //procedure DoProgress(const progressTimes: TBZProgressTimes);

    function GetItemByName(const AName: TBZSoundFXName): TBZCustomSoundFXItem;

    function AddFilterLowPass(AName: TBZSoundFXName) : TBZLowPassSoundFilter;
    function AddFilterHighPass(AName: TBZSoundFXName) : TBZHighPassSoundFilter;
    function AddFilterBandPass(AName: TBZSoundFXName) : TBZBandPassSoundFilter;

    function GetFilterLowPassByName(const AName: TBZSoundFXName): TBZLowPassSoundFilter;
    function GetFilterHighPassByName(const AName: TBZSoundFXName): TBZHighPassSoundFilter;
    function GetFilterBandPassByName(const AName: TBZSoundFXName): TBZBandPassSoundFilter;

    function AddEffectReverb(AName: TBZSoundFXName) : TBZReverbSoundEffect;
    function AddEffectEAXReverb(AName: TBZSoundFXName) : TBZEAXReverbSoundEffect;
    function AddEffectChorus(AName: TBZSoundFXName) : TBZChorusSoundEffect;
    function AddEffectEcho(AName: TBZSoundFXName) : TBZEchoSoundEffect;
    function AddEffectDistortion(AName: TBZSoundFXName) : TBZDistortionSoundEffect;
    function AddEffectFlanger(AName: TBZSoundFXName) : TBZFlangerSoundEffect;
    function AddEffectCompressor(AName: TBZSoundFXName) : TBZCompressorSoundEffect;
    function AddEffectEqualizer(AName: TBZSoundFXName) : TBZEqualizerSoundEffect;

    function GetEffectReverbByName(const AName: TBZSoundFXName): TBZReverbSoundEffect;
    function GetEffectEAXReverbByName(const AName: TBZSoundFXName): TBZEAXReverbSoundEffect;
    function GetEffectChorusByName(const AName: TBZSoundFXName): TBZChorusSoundEffect;
    function GetEffectEchoByName(const AName: TBZSoundFXName): TBZEchoSoundEffect;
    function GetEffectDistortionByName(const AName: TBZSoundFXName): TBZDistortionSoundEffect;
    function GetEffectFlangerByName(const AName: TBZSoundFXName): TBZFlangerSoundEffect;
    function GetEffectCompressorByName(const AName: TBZSoundFXName): TBZCompressorSoundEffect;
    function GetEffectEqualizerByName(const AName: TBZSoundFXName): TBZEqualizerSoundEffect;


  end;

  { Composant non-visuel pour gérer une liste d'effets audio }
  TBZSoundFXLibrary = class(TComponent)
  private
    FSoundFXLib: TBZSoundFXLib;
  protected
    procedure SetSoundFXLib(const val: TBZSoundFXLib);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property FX: TBZSoundFXLib read FSoundFXLib write SetSoundFXLib;
  end;

  { Descritpion d'un "Slot" permettant d'assigner un effet à un échantillon audio }
  TBZSoundFXAuxSlot = class(TCollectionItem)
  private
    FTag : PtrUInt;
    FSoundFXLibrary : TBZSoundFXLibrary;
    FSoundFXLibraryName : String;
    FSoundFXName : String;
    FSoundFXCache : TBZCustomSoundFXItem;
    FActivated : Boolean;
    FEffectGain : Single;
    FAutoAdjust : Boolean;

    function GetSoundFXLibrary : TBZSoundFXLibrary;
    procedure SetSoundFXLibrary(Const AValue : TBZSoundFXLibrary);

  //  procedure SetSoundFXLibraryName(AValue : String);
    procedure SetSoundFXName(Val : String);
    procedure SetActivated(val : Boolean);
  protected
    procedure WriteToFiler(Awriter: TWriter);
    procedure ReadFromFiler(Areader: TReader);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    //procedure SetCacheFX(Val : TBZCustomSoundFXItem);

    property ManagerTag : PtrUInt read FTag  write FTag;
    property CacheFX : TBZCustomSoundFXItem read FSoundFXCache;
    property SoundFXLibrary :  TBZSoundFXLibrary read GetSoundFXLibrary write SetSoundFXLibrary;
  published
    property SoundFXlibraryName : String Read FSoundFXLibraryName Write FSoundFXLibraryName;
    property Name : String read FSoundFXName Write SetSoundFXName;
    property Activated : Boolean read FActivated Write SetActivated;
    property Gain : Single read FEffectGain write FEffectGain;
    property AutoAdjust : Boolean read FAutoAdjust write FAutoAdjust;
  End;

  { Gestion d'une collection de "slots" }
  TBZSoundFXAuxSlots = class(TCollection)
  protected

    FOwner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TBZSoundFXAuxSlot);
    function GetItems(index: Integer): TBZSoundFXAuxSlot;

    function FindItemID(ID: Integer): TBZSoundFXAuxSlot;

  public

    constructor Create(AOwner: TComponent);
    function Add: TBZSoundFXAuxSlot;

    property Items[index: Integer]: TBZSoundFXAuxSlot read GetItems write SetItems; default;
  End;

  { Description de base d'un échantillon audio }
  TBZBaseSoundSource = class(TCollectionItem)
  private
     
    //FBehaviourToNotify: TBZBSoundEmitter;
    FAutoFreeOnStop : Boolean;
      // private only, NOT persistent, not assigned
    FPriority: Integer;
    //FOrigin: TBZBaseSceneObject; // NOT persistent
    FVolume: Single;
    FMinDistance, FMaxDistance: Single;
    FInsideConeAngle, FOutsideConeAngle: Single;
    FConeOutsideVolume: Single;

    FSoundLibraryName: string; // used for persistence
    FSoundLibrary: TBZSoundLibrary; // persistence via name
    FSoundName: string;

    FSoundFXLibraryName : String;
    FSoundFXLibrary: TBZSoundFXLibrary; // persistence via name

    FAuxSlots : TBZSoundFXAuxSlots;
   (* FAuxSlots: array of PtrUInt;
    FAuxSlotCount : Byte;
    FAuxSlotFXName  : array of string;
    FAuxSlotFXActivated : array of Boolean;
    FAuxSlotFXCache : Array of TBZCustomSoundFXItem; *)

//    FDirectFilteringType : TBZSoundFilteringType;
    FDirectFilterName : String;
    FDirectFilterActivated : Boolean;
    FDirectFilterTag : PtrUInt;
    FDirectFilterCache : TBZCustomSoundFilterItem;

    FMute: Boolean;
    FPause: Boolean;
    FPlaying: Boolean;
    FPitch : Single;
    FChanges: TBZSoundSourceChanges; // NOT persistent, not assigned
    FFXChanges: TBZSoundSourceFXChanges; // NOT persistent, not assigned

    FNbLoops: Integer;
    FTag: PtrUInt; // NOT persistent, not assigned
    FLoaded : Boolean;
    FTimePosition : Single;

    FFrequency: Integer;// A voir si on garde

    FUseEnvironnment : Boolean; //Slot 2
    FUseEqualizer : Boolean;    //Slot 3

  protected

    procedure WriteToFiler(Awriter: TWriter);
    procedure ReadFromFiler(Areader: TReader);

    function GetDisplayName: string; override;
    procedure SetPriority(const val: Integer);
    //procedure SetOrigin(const val: TBZBaseSceneObject);
    procedure SetVolume(const val: Single);
    procedure SetMinDistance(const val: Single);
    procedure SetMaxDistance(const val: Single);
    procedure SetInsideConeAngle(const val: Single);
    procedure SetOutsideConeAngle(const val: Single);
    procedure SetConeOutsideVolume(const val: Single);

    function GetSoundLibrary: TBZSoundLibrary;
    procedure SetSoundLibrary(const val: TBZSoundLibrary);
    procedure SetSoundName(const val: string);

    function GetSoundFXLibrary: TBZSoundFXLibrary;
    procedure SetSoundFXLibrary(const val: TBZSoundFXLibrary);
    procedure setDirectFilterName(val : String);

    procedure SetDirectFilterActivated(val : Boolean);
    function GetDirectFilterActivated: Boolean;
    function GetDirectFilterFXCache:TBZCustomSoundFilterItem;

    procedure SetMute(const val: Boolean);
    procedure SetPause(const val: Boolean);
    procedure SetPlaying(const val: Boolean);
    procedure SetNbLoops(const val: Integer);
    procedure SetFrequency(const val: Integer);
    procedure SetPitch(const val: Single);

    Function GetTimePositionInByte:Int64;
    //function GetPeaksMeter(PosInByte : Int64; var Channels : Array of single);

  public
    //OpenALBuffers : array[0..1] of PtrUInt; // Pour OpenAL

    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property Changes: TBZSoundSourceChanges read FChanges Write FChanges;
    property FXChanges: TBZSoundSourceFXChanges read FFXChanges Write FFXChanges;

    function Sample: TBZSoundSampleItem;
    procedure UpdateTimePosition(Const val : Single);

    // This Tag is reserved for sound manager use only
    property ManagerTag: PtrUInt read FTag write FTag;

    property DirectFilterTag : PtrUInt read FDirectFilterTag write FDirectFilterTag;
    property DirectFilterFX : TBZCustomSoundFilterItem read GetDirectFilterFXCache;
    { Origin object for the sound sources.
       Absolute object position/orientation are taken into account, the
       object's TBZBInertia is considered if any.
       If origin is nil, the source is assumed to be static at the origin.
        Note :  since TCollectionItem do not support the "Notification"
       scheme, it is up to the Origin object to take care of updating this
       property prior to release/destruction. }
    //property Origin: TBZBaseSceneObject read FOrigin write SetOrigin;

   // property DirectFilter : TBZCustomSoundFilter read FDirectFilter;
    property TimePosition : Single Read FTimePosition;
    property TimePositionInByte : Int64 read GetTimePositionInByte;

  published
     
    property SoundLibrary: TBZSoundLibrary read GetSoundLibrary write SetSoundLibrary;
    property SoundName: string read FSoundName write SetSoundName;

    property SoundFXLibrary  : TBZSoundFXLibrary read GetSoundFXLibrary write SetSoundFXLibrary;
    property DirectFilter : String read FDirectFilterName Write setDirectFilterName;
    property DirectFilterActivated : Boolean read GetDirectFilterActivated Write setDirectFilterActivated;

    property AuxSlots : TBZSoundFXAuxSlots read FAuxSlots;

    { Volume of the source, [0.0; 1.0] range }
    property Volume: Single read FVolume write SetVolume;
    { Nb of playing loops. }
    property NbLoops: Integer read FNbLoops write SetNbLoops default 1;

    property Mute: Boolean read FMute write SetMute default False;
    property Pause: Boolean read FPause write SetPause default False;
    property Playing: Boolean read FPlaying write SetPlaying default False;

    { Sound source priority, the higher the better.
       When maximum number of sound sources is reached, only the sources
       with the highest priority will continue to play, however, even
       non-playing sources should be tracked by the manager, thus allowing
       an "unlimited" amount of sources from the application point of view. }
    property Priority: Integer read FPriority write SetPriority default 0;

    { Min distance before spatial attenuation occurs.
       1.0 by default }
    property MinDistance: Single read FMinDistance write SetMinDistance;
    { Max distance, if source is further away, it will not be heard.
       100.0 by default }
    property MaxDistance: Single read FMaxDistance write SetMaxDistance;

    { Inside cone angle, [0° 360°].
       Sound volume is maximal within this cone.
       See DirectX SDK for details. }
    property InsideConeAngle: Single read FInsideConeAngle write SetInsideConeAngle;
    { Outside cone angle, [0°; 360°].
       Between inside and outside cone, sound volume decreases between max
       and cone outside volume.
       See DirectX SDK for details. }
    property OutsideConeAngle: Single read FOutsideConeAngle write SetOutsideConeAngle;
    { Cone outside volume, [0.0; 1.0] range.
       See DirectX SDK for details. }
    property ConeOutsideVolume: Single read FConeOutsideVolume write SetConeOutsideVolume;
    { Sample custom playback frequency.
       Values null or negative are interpreted as 'default frequency'. }
    property Frequency: Integer read FFrequency write SetFrequency default -1;
    property Pitch: Single read FPitch write SetPitch default 1.0;

    property UseEnvironnment : Boolean read FUseEnvironnment write FUseEnvironnment;
    property UseEqualizer : Boolean read FUseEnvironnment write FUseEqualizer;

    property AutoFreeOnStop : Boolean read FAutoFreeOnStop write FAutoFreeOnStop;
  end;

  { Description d'un échantillon audio avec prise en charge de l'orientation. @br
    @bold(Note) : l'orientation"  est la Direction de propagation l'onde sonore}
  TBZSoundSource = class(TBZBaseSoundSource)
  public
    destructor Destroy; override;
  published
    //property Origin;
  end;

  { Gestion d'une collection d'échantillons audio }
  TBZSoundSources = class(TCollection)
  protected
     
    FOwner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TBZSoundSource);
    function GetItems(index: Integer): TBZSoundSource;

    function Add: TBZSoundSource;
    function FindItemID(ID: Integer): TBZSoundSource;

  public
     
    constructor Create(AOwner: TComponent);

    property Items[index: Integer]: TBZSoundSource read GetItems write SetItems; default;
  end;

  TBZSoundManagerChange = (smcTransformation, smcEnvironnment, smcEqualizer);  //sscEffects, sscFiltersmc
  TBZSoundManagerChanges = set of TBZSoundManagerChange;

  { @abstract(Classe de base pour les composants du gestionnaire de son. @br
    Le composant sound manager est l'interface d'une API audio de bas niveau (comme DirectSound, Bass, FMod, OpenAL...))
    
    Il ne peut y avoir qu'un seul gestionnaire actif à tout moment (Cette classe prend soin de cela). @br
    La sous-classe doit remplacer les méthodes protégées DoActivate et DoDeActivate pour 'initialiser / unitialiser' leur couche sonore. }
  TBZSoundManager = class(TBZCadenceAbleComponent)
  private
     
    FActive: Boolean;
    FMute: Boolean;
    FPause: Boolean;
    FPlaying : Boolean;

    FMasterVolume: Single;

    //FListener: TBZBaseSceneObject;
    //FLastListenerPosition: TVector;
    FSources: TBZSoundSources;
    FMaxChannels: Integer;
    FOutputFrequency: Integer;
    FUpdateFrequency: Single;
    FDistanceFactor: Single;
    FRollOffFactor: Single;
    FDopplerFactor: Single;
    FSoundEnvironment: TBZSoundReverbEnvironment;
    FLastUpdateTime, FLastDeltaTime: Single;
      // last time UpdateSources was fired, not persistent
    FCadencer: TBZCadencer;
    FUseEnvironment : Boolean;

    procedure SetActive(const val: Boolean);
    procedure SetMute(const val: Boolean);
    procedure SetPause(const val: Boolean);
    procedure SetPlaying(const val: Boolean);
    procedure SetUseEnvironment(const val: Boolean);

    procedure WriteDoppler(writer: TWriter);
    procedure ReadDoppler(reader: TReader);

  protected
     
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    procedure SetSources(const val: TBZSoundSources);
    procedure SetMasterVolume(const val: Single);
    //procedure SetListener(const val: TBZBaseSceneObject);
    procedure SetMaxChannels(const val: Integer);
    procedure SetOutputFrequency(const val: Integer);
    procedure SetUpdateFrequency(const val: Single);
    function StoreUpdateFrequency: Boolean;
    procedure SetCadencer(const val: TBZCadencer);
    procedure SetDistanceFactor(const val: Single);
    function StoreDistanceFactor: Boolean;
    procedure SetRollOffFactor(const val: Single);
    function StoreRollOffFactor: Boolean;
    procedure SetDopplerFactor(const val: Single);
    procedure SetSoundEnvironment(const val: TBZSoundReverbEnvironment);

    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;

    procedure ListenerCoordinates(var position, velocity, direction, up: TBZVector);

    function DoActivate: Boolean; dynamic;
    // Invoked AFTER all sources have been stopped
    procedure DoDeActivate; dynamic;
    { Effect mute of all sounds.
       Default implementation call MuteSource for all non-muted sources
       with "True" as parameter. }
    function DoMute: Boolean; dynamic;
    { Effect un-mute of all sounds.
       Default implementation call MuteSource for all non-muted sources
       with "False" as parameter. }
    procedure DoUnMute; dynamic;
    { Effect pause of all sounds.
       Default implementation call PauseSource for all non-paused sources
       with "True" as parameter. }
    function DoPause: Boolean; dynamic;
    { Effect un-pause of all sounds.
       Default implementation call PauseSource for all non-paused sources
       with "True" as parameter. }
    procedure DoUnPause; dynamic;
    procedure DoPlay;Dynamic;
    procedure DoStop;Dynamic;

    procedure NotifyMasterVolumeChange; dynamic;
    procedure Notify3DFactorsChanged; dynamic;
    procedure NotifyEnvironmentChanged; dynamic;

    // Called when a source will be freed
    procedure KillSource(aSource: TBZBaseSoundSource); virtual;
    { Request to update source's data in low-level sound API.
       Default implementation just clears the "Changes" flags. }
    procedure UpdateSource(aSource: TBZBaseSoundSource); virtual;
    procedure MuteSource(aSource: TBZBaseSoundSource; muted: Boolean); virtual;
    procedure PauseSource(aSource: TBZBaseSoundSource; paused: Boolean); virtual;
    procedure PlaySource(aSource : TBZBaseSoundSource; playing : Boolean); virtual;
    function GetDefaultFrequency(aSource : TBZBaseSoundSource) : Integer; virtual;
    function GetTimePosition(aSource : TBZBaseSoundSource): Single; virtual;
  public
     
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Manual request to update all sources to reflect changes.
       Default implementation invokes UpdateSource for all known sources. }
    procedure UpdateSources; virtual;
    { Stop and free all sources. }
    procedure StopAllSources;

    { Progress notification for time synchronization.
       This method will call UpdateSources depending on the last time
       it was performed and the value of the UpdateFrequency property. }
    procedure DoProgress(const progressTime: TBZProgressTimes); override;

    { Sound manager API reported CPU Usage.
       Returns -1 when unsupported. }
    function CPUUsagePercent: Single; virtual;
    { True if EAX is supported. }
    function EAXSupported: Boolean; dynamic;

    function GetInformations : String; virtual;
  published
     
     { Activation/deactivation of the low-level sound API }
    property Active: Boolean read FActive write SetActive default False;

    { Maximum number of sound output channels.
       While some drivers will just ignore this value, others cannot
       dynamically adjust the maximum number of channels (you need to
       de-activate and re-activate the manager for this property to be
       taken into account). }
    property MaxChannels: Integer read FMaxChannels write SetMaxChannels default 8;
    { Sound output mixing frequency.
       Commonly used values ar 11025, 22050 and 44100.
       Note that most driver cannot dynamically adjust the output frequency
       (you need to de-ativate and re-activate the manager for this property
       to be taken into account). }
    property OutputFrequency: Integer read FOutputFrequency write SetOutputFrequency default 44100;

    { Request to mute all sounds.
       All sound requests should be handled as if sound is unmuted though,
       however drivers should try to take a CPU advantage of mute over
       MasterVolume=0 }
    property Mute: Boolean read FMute write SetMute default False;
    { Request to pause all sound, sound output should be muted too.
       When unpausing, all sound should resume at the point they were paused. }
    property Pause: Boolean read FPause write SetPause default False;
    { Master Volume adjustement in the [0.0; 1.0] range.
       Driver should take care of properly clamping the master volume. }
    property MasterVolume: Single read FMasterVolume write SetMasterVolume;

    { Scene object that materializes the listener.
       The sceneobject's AbsolutePosition and orientation are used to define
       the listener coordinates, velocity is automatically calculated
       (if you're using DoProgress or connected the manager to a cadencer).
       If this property is nil, the listener is assumed to be static at
       the NullPoint coordinate, facing Z axis, with up being Y (ie. the
       default GLScene orientation). }
   // property Listener: TBZBaseSceneObject read FListener write SetListener;
    { Currently active and playing sound sources. }
    property Sources: TBZSoundSources read FSources write SetSources;

    { Update frequency for time-based control (DoProgress).
       Default value is 10 Hz (frequency is clamped in the 1Hz-60Hz range). }
    property UpdateFrequency: Single read FUpdateFrequency write SetUpdateFrequency stored StoreUpdateFrequency;
    { Cadencer for time-based control. }
    property Cadencer: TBZCadencer read FCadencer write SetCadencer;
    { Engine relative distance factor, compared to 1.0 meters.
       Equates to 'how many units per meter' your engine has. }
    property DistanceFactor: Single read FDistanceFactor write SetDistanceFactor stored StoreDistanceFactor;
    { Sets the global attenuation rolloff factor.
       Normally volume for a sample will scale at 1 / distance.
       This gives a logarithmic attenuation of volume as the source gets
       further away (or closer). 
       Setting this value makes the sound drop off faster or slower.
       The higher the value, the faster volume will fall off. }
    property RollOffFactor: Single read FRollOffFactor write SetRollOffFactor stored StoreRollOffFactor;
    { Engine relative Doppler factor, compared to 1.0 meters.
       Equates to 'how many units per meter' your engine has. }
    property DopplerFactor: Single read FDopplerFactor write SetDopplerFactor stored False;
    { Sound environment (requires EAX compatible soundboard). }
    property Environment: TBZSoundReverbEnvironment read FSoundEnvironment write SetSoundEnvironment default seGeneric;
    property UseEnvironment : Boolean read FUseEnvironment write SetUseEnvironment;
//    property Equalizer : TBZEqualizerSoundEffect read FEqualizer;
  end;

  // TBZBSoundEmitter
  //
  { A sound emitter behaviour, plug it on any object to make it noisy.
       This behaviour is just an interface to a TBZSoundSource, for editing
       convenience. }
(*  TBZBSoundEmitter = class(TBZBehaviour)
  private
     
    FPlaying: Boolean; // used at design-time ONLY
    FSource: TBZBaseSoundSource;
    FPlayingSource: TBZSoundSource;

  protected
     
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;

    procedure SetSource(const val: TBZBaseSoundSource);
    procedure SetPlaying(const val: Boolean);
    function GetPlaying: Boolean;

    procedure NotifySourceDestruction(aSource: TBZSoundSource);

  public
     
    constructor Create(aOwner: TBZXCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    class function UniqueItem: Boolean; override;

    procedure DoProgress(const progressTime: TProgressTimes); override;

    property PlayingSource: TBZSoundSource read FPlayingSource;

  published
     
    property Source: TBZBaseSoundSource read FSource write SetSource;
    property Playing: Boolean read GetPlaying write SetPlaying default False;

  end;   *)

{ Retourne le gestionnaire d'audio actuellement actif }
function ActiveSoundManager: TBZSoundManager;
{ Retourne la librairie d'un échantillon audio d'après son nom }
function GetSoundLibraryByName(const aName: string): TBZSoundLibrary;

//function GetOrCreateSoundEmitter(behaviours: TBZBehaviours): TBZBSoundEmitter; overload;
//function GetOrCreateSoundEmitter(obj: TBZBaseSceneObject): TBZBSoundEmitter; overload;

var
  { Variable global pour l'affichage des erreurs }
  vVerboseGLSMErrors: Boolean = True;
  //vActiveSoundManager: TBZSoundManager;

implementation

uses BZUtils, BZMath, BZLIbOpenAL, Dialogs;

var
  vSoundLibraries: TList;
  vSoundFXLibraries: TList;
  vActiveSoundManager: TBZSoundManager;
  vBZSoundFXItemNameChangeEvent: TNotifyEvent;

function ActiveSoundManager: TBZSoundManager;
begin
  Result := vActiveSoundManager;
end;

function GetSoundLibraryByName(const aName: string): TBZSoundLibrary;
var
  i: Integer;
begin
  Result := nil;
  if Assigned(vSoundLibraries) then
    for i := 0 to vSoundLibraries.Count - 1 do
      if TBZSoundLibrary(vSoundLibraries[i]).Name = aName then
      begin
        Result := TBZSoundLibrary(vSoundLibraries[i]);
        Break;
      end;
end;

function GetSoundFXLibraryByName(const aName: string): TBZSoundFXLibrary;
var
  i: Integer;
begin
  Result := nil;
  if Assigned(vSoundFXLibraries) then
    for i := 0 to vSoundLibraries.Count - 1 do
      if TBZSoundFXLibrary(vSoundFXLibraries[i]).Name = aName then
      begin
        Result := TBZSoundFXLibrary(vSoundFXLibraries[i]);
        Break;
      end;
end;

(* function GetOrCreateSoundEmitter(behaviours: TBZBehaviours): TBZBSoundEmitter;
var
  i: Integer;
begin
  i := behaviours.IndexOfClass(TBZBSoundEmitter);
  if i >= 0 then Result := TBZBSoundEmitter(behaviours[i])
  else Result := TBZBSoundEmitter.Create(behaviours);
end;

function GetOrCreateSoundEmitter(obj: TBZBaseSceneObject): TBZBSoundEmitter;
begin
  Result := GetOrCreateSoundEmitter(obj.Behaviours);
end; *)

{%region=====[ TBZSoundSampleItem ]===============================================}

constructor TBZSoundSampleItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FData := TBZSoundSample.Create(Self);
end;

destructor TBZSoundSampleItem.Destroy;
begin
  FreeAndNil(FData);
  inherited Destroy;
end;

procedure TBZSoundSampleItem.Assign(Source: TPersistent);
begin
  if Source is TBZSoundSampleItem then
  begin
    FName := TBZSoundSampleItem(Source).Name;
    FData.Free;
    FData := TBZSoundSample(TBZSoundSampleItem(Source).Data.CreateCopy(Self));
  end
  else
    inherited Assign(Source); // error
end;

procedure TBZSoundSampleItem.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('BinData', @ReadData, @WriteData, Assigned(FData));
end;

procedure TBZSoundSampleItem.ReadData(Stream: TStream);
var
  n: Integer;
  clName: AnsiString;
begin
  n:=0;
  with Stream do
  begin
    Read(n, SizeOf(Integer));
    SetLength(clName, n);
    if n > 0 then Read(clName[1], n);
    FData := TBZSoundSample.Create(Self); //TBZSoundSampleClass(FindClass(string(clName))).Create(Self);
    FData.LoadFromStream(Stream);
  end;
end;

procedure TBZSoundSampleItem.WriteData(Stream: TStream);
var
  n: Integer;
  buf: AnsiString;
begin
  with Stream do
  begin
    n := Length(FData.ClassName);
    Write(n, SizeOf(Integer));
    buf := AnsiString(FData.ClassName);
    if n > 0 then Write(buf[1], n);
    FData.SaveToStream(Stream);
  end;
end;

function TBZSoundSampleItem.GetDisplayName: string;
var
  s: string;
begin
  if Assigned(FData) then
  begin
    if Data.NbChannels > 1 then s := 's'
    else s := '';
    Result := Format('%s (%d Hz, %d bits, %d channel%s, %.2f sec)',
      [Name, Data.Frequency,
      Data.BitsPerSample,
      Data.NbChannels, s, Data.LengthInSec])
  end
  else
    Result := Format('%s (empty)', [Name]);
end;

procedure TBZSoundSampleItem.LoadFromFile(const fileName: string);
begin
  FData.LoadFromFile(FileName);
  FName := ExtractFileName(fileName);
end;

(* procedure TBZSoundSampleItem.PlayOnWaveOut;
begin
  if Assigned(FData) then FData.PlayOnWaveOut;
end;  *)


procedure TBZSoundSampleItem.SetData(const val: TBZSoundSample);
begin
  FData.Free;
  if Assigned(val) then FData := TBZSoundSample(val.CreateCopy(Self))
  else FData := nil;
end;

constructor TBZSoundSampleItems.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  inherited Create(TBZSoundSampleItem);
end;

function TBZSoundSampleItems.GetOwner: TPersistent;
begin
  Result := Owner;
end;

{%endregion%}

{%region=====[ TBZSoundSampleItems ]==============================================}

procedure TBZSoundSampleItems.SetItems(index: Integer; const val: TBZSoundSampleItem);
begin
  inherited Items[index] := val;
end;

function TBZSoundSampleItems.GetItems(index: Integer): TBZSoundSampleItem;
begin
  Result := TBZSoundSampleItem(inherited Items[index]);
end;

function TBZSoundSampleItems.Add: TBZSoundSampleItem;
begin
  Result := TBZSoundSampleItem.Create(Self);//(inherited Add) as TBZSoundSampleItem;
end;

function TBZSoundSampleItems.FindItemID(ID: Integer): TBZSoundSampleItem;
begin
  Result := (inherited FindItemID(ID)) as TBZSoundSampleItem;
end;

function TBZSoundSampleItems.GetByName(const aName: string): TBZSoundSampleItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if CompareText(Items[i].Name, aName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TBZSoundSampleItems.AddFile(const fileName: string; const sampleName: string = ''): TBZSoundSampleItem;
begin
  Result := Add;
  Result.LoadFromFile(fileName);
  if sampleName <> '' then Result.Name := sampleName;
end;

{%endregion%}

{%region=====[ TBZSoundLibrary ]==============================================}

constructor TBZSoundLibrary.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSamples := TBZSoundSampleItems.Create(Self);
  vSoundLibraries.Add(Self);
end;

destructor TBZSoundLibrary.Destroy;
begin
  vSoundLibraries.Remove(Self);
  FSamples.Free;
  inherited Destroy;
end;

procedure TBZSoundLibrary.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
end;

procedure TBZSoundLibrary.SetSamples(const val: TBZSoundSampleItems);
begin
  FSamples.Assign(val);
end;

{%endregion%}

{%region=====[ TBZCustomSoundFXItem ]=========================================}

constructor TBZCustomSoundFXItem.Create(aOwner: TBZXCollection);
begin
  inherited Create(aOwner);
  FTag := 0;
end;

destructor TBZCustomSoundFXItem.Destroy;
begin
  inherited Destroy;
end;

procedure TBZCustomSoundFXItem.SetName(const val: string);
begin
  inherited SetName(val);
  if Assigned(vBZSoundFXItemNameChangeEvent) then
    vBZSoundFXItemNameChangeEvent(Self);
end;

procedure TBZCustomSoundFXItem.WriteToFiler(Awriter: TWriter);
begin
  inherited;
  with Awriter do
  begin
    WriteInteger(0); // Archive Version 0
  end;
end;

procedure TBZCustomSoundFXItem.ReadFromFiler(Areader: TReader);
begin
  if Owner.ArchiveVersion > 0 then inherited;
  with AReader do
  begin
    if ReadInteger <> 0 then  Assert(False);
  end;
end;

class function TBZCustomSoundFXItem.FriendlyName: String;
begin
  Result := 'Custom_Sound_FX_Item';
End;

(*function TBZCustomSoundFXItem.OwnerBaseSceneObject: TGLBaseSceneObject;
begin
  Result := TGLBaseSceneObject(Owner.Owner);
end;

procedure TBZCustomSoundFXItem.DoProgress(const progressTime: TProgressTimes);
begin
end;  *)

{%endregion%}

{%region=====[ TBZCustomSoundEffectItem ]=====================================}

function TBZCustomSoundEffectItem.GetFilter:TBZCustomSoundFilterItem;
var
  idx : integer;
begin
  Result := nil;
  idx := Owner.IndexOfName(FFilterName);
  if idx>0 then Result := TBZCustomSoundFilterItem(Owner.Items[Idx]);

end;

class function TBZCustomSoundEffectItem.FriendlyName: String;
begin
  Result := 'Custom_Sound_Effect_Item';
End;
{%endregion%}

{%region=====[ TBZReverbSoundEffect ]=========================================}

constructor TBZReverbSoundEffect.Create(AOwner: TBZXCollection);
begin
  inherited Create(AOwner);
  FPreset                := seGeneric;
  FDENSITY               := 1.0;
  FDIFFUSION             := 1.0;
  FGAIN                  := 0.32;
  FGAINHF                := 0.89;
  FDECAY_TIME            := 1.49;
  FDECAY_HFRATIO         := 0.83;
  FREFLECTIONS_GAIN      := 0.05;
  FREFLECTIONS_DELAY     := 0.007;
  FLATE_REVERB_GAIN      := 1.26;
  FLATE_REVERB_DELAY     := 0.011;
  FAIR_ABSORPTION_GAINHF := 0.994;
  FROOM_ROLLOFF_FACTOR   := 0.0;
  FDECAY_HFLIMIT         := True;
end;

destructor TBZReverbSoundEffect.Destroy;
begin
  Inherited Destroy;
end;

class function TBZReverbSoundEffect.FriendlyName: String;
begin
  Result := 'EFX_Reverb_Sound_Effect';
End;

procedure TBZReverbSoundEffect.SetPreset(val : TBZSoundReverbEnvironment);
var
  ThePreset : EFXEAXREVERBPROPERTIES;

  procedure SetParams(aPreset : EFXEAXREVERBPROPERTIES);
  begin
    FDENSITY               := aPreset.flDensity;
    FDIFFUSION             := aPreset.flDiffusion;
    FGAIN                  := aPreset.flGain;
    FGAINHF                := aPreset.flGainHF;
    FDECAY_TIME            := aPreset.flDecayTime;
    FDECAY_HFRATIO         := aPreset.flDecayHFRatio;
    FREFLECTIONS_GAIN      := aPreset.flReflectionsGain;
    FREFLECTIONS_DELAY     := aPreset.flReflectionsDelay;
    FLATE_REVERB_GAIN      := aPreset.flLateReverbGain;
    FLATE_REVERB_DELAY     := aPreset.flLateReverbDelay;
    FAIR_ABSORPTION_GAINHF := aPreset.flAirAbsorptionGainHF;
    FROOM_ROLLOFF_FACTOR   := aPreset.flRoomRolloffFactor;
    FDECAY_HFLIMIT         := (aPreset.iDecayHFLimit>0);
  End;

begin
  Case val of
    seGeneric : ThePreset := EFX_REVERB_PRESET_GENERIC;
    sePaddedCell : ThePreset := EFX_REVERB_PRESET_PADDEDCELL;
    seRoom : ThePreset := EFX_REVERB_PRESET_ROOM;
    seBathroom : ThePreset := EFX_REVERB_PRESET_BATHROOM;
    seLivingRoom : ThePreset := EFX_REVERB_PRESET_LIVINGROOM;
    seStoneroom : ThePreset := EFX_REVERB_PRESET_STONEROOM;
    seAuditorium : ThePreset := EFX_REVERB_PRESET_AUDITORIUM;
    seConcertHall : ThePreset := EFX_REVERB_PRESET_CONCERTHALL;
    seCave : ThePreset := EFX_REVERB_PRESET_CAVE;
    seArena : ThePreset := EFX_REVERB_PRESET_ARENA;
    seHangar : ThePreset := EFX_REVERB_PRESET_HANGAR;
    seCarpetedHallway : ThePreset := EFX_REVERB_PRESET_CARPETEDHALLWAY;
    seHallway : ThePreset := EFX_REVERB_PRESET_HALLWAY;
    seStoneCorridor : ThePreset := EFX_REVERB_PRESET_STONECORRIDOR;
    seAlley : ThePreset := EFX_REVERB_PRESET_ALLEY;
    seForest : ThePreset := EFX_REVERB_PRESET_FOREST;
    seCity : ThePreset := EFX_REVERB_PRESET_CITY;
    seMountains : ThePreset := EFX_REVERB_PRESET_MOUNTAINS;
    seQuarry : ThePreset := EFX_REVERB_PRESET_QUARRY;
    sePlain : ThePreset := EFX_REVERB_PRESET_PLAIN;
    seParkingLot : ThePreset := EFX_REVERB_PRESET_PARKINGLOT;
    seSewerPipe : ThePreset := EFX_REVERB_PRESET_SEWERPIPE;
    seUnderWater : ThePreset := EFX_REVERB_PRESET_UNDERWATER;
    seDrugged : ThePreset := EFX_REVERB_PRESET_DRUGGED;
    seDizzy : ThePreset := EFX_REVERB_PRESET_DIZZY;
    sePsychotic : ThePreset := EFX_REVERB_PRESET_PSYCHOTIC;
    seCastle_SmallRoom : ThePreset := EFX_REVERB_PRESET_CASTLE_SMALLROOM;
    seCastle_ShortPassage : ThePreset := EFX_REVERB_PRESET_CASTLE_SHORTPASSAGE;
    seCastle_MediumRoom : ThePreset := EFX_REVERB_PRESET_CASTLE_MEDIUMROOM;
    seCastle_LargeRoom : ThePreset := EFX_REVERB_PRESET_CASTLE_LARGEROOM;
    seCastle_LongPassage : ThePreset := EFX_REVERB_PRESET_CASTLE_LONGPASSAGE;
    seCastle_Hall : ThePreset := EFX_REVERB_PRESET_CASTLE_HALL;
    seCastle_CupBoarb : ThePreset := EFX_REVERB_PRESET_CASTLE_CUPBOARD;
    seCastle_CourtYard : ThePreset := EFX_REVERB_PRESET_CASTLE_COURTYARD;
    seCastle_Alcove : ThePreset := EFX_REVERB_PRESET_CASTLE_ALCOVE;
    seFactory_SmallRoom : ThePreset := EFX_REVERB_PRESET_FACTORY_SMALLROOM;
    seFactory_ShortPassage : ThePreset := EFX_REVERB_PRESET_FACTORY_SHORTPASSAGE;
    seFactory_MediumRoom : ThePreset := EFX_REVERB_PRESET_FACTORY_MEDIUMROOM;
    seFactory_LargeRoom : ThePreset := EFX_REVERB_PRESET_FACTORY_LARGEROOM;
    seFactory_LongPassage : ThePreset := EFX_REVERB_PRESET_FACTORY_LONGPASSAGE;
    seFactory_Hall : ThePreset := EFX_REVERB_PRESET_FACTORY_HALL;
    seFactory_CupBoard : ThePreset := EFX_REVERB_PRESET_FACTORY_CUPBOARD;
    seFactory_CourtYard : ThePreset := EFX_REVERB_PRESET_FACTORY_COURTYARD;
    seFactory_Alcove : ThePreset := EFX_REVERB_PRESET_FACTORY_ALCOVE;
    seIcePalace_SmallRoom : ThePreset := EFX_REVERB_PRESET_ICEPALACE_SMALLROOM;
    seIcePalace_ShortPassage : ThePreset := EFX_REVERB_PRESET_ICEPALACE_SHORTPASSAGE;
    seIcePalace_MediumRoom : ThePreset := EFX_REVERB_PRESET_ICEPALACE_MEDIUMROOM;
    seIcePalace_LargeRoom : ThePreset := EFX_REVERB_PRESET_ICEPALACE_LARGEROOM;
    seIcePalace_LongPassage : ThePreset := EFX_REVERB_PRESET_ICEPALACE_LONGPASSAGE;
    seIcePalace_Hall : ThePreset := EFX_REVERB_PRESET_ICEPALACE_HALL;
    seIcePalace_CupBoard : ThePreset := EFX_REVERB_PRESET_ICEPALACE_CUPBOARD;
    seIcePalace_CourtYard : ThePreset := EFX_REVERB_PRESET_ICEPALACE_COURTYARD;
    seIcePalace_Alcove : ThePreset := EFX_REVERB_PRESET_ICEPALACE_ALCOVE;
    seSpaceStation_SmallRoom : ThePreset := EFX_REVERB_PRESET_SPACESTATION_SMALLROOM;
    seSpaceStation_ShortPassage : ThePreset := EFX_REVERB_PRESET_SPACESTATION_SHORTPASSAGE;
    seSpaceStation_MediumRoom : ThePreset := EFX_REVERB_PRESET_SPACESTATION_MEDIUMROOM;
    seSpaceStation_LargeRoom : ThePreset := EFX_REVERB_PRESET_SPACESTATION_LARGEROOM;
    seSpaceStation_LongPassage : ThePreset := EFX_REVERB_PRESET_SPACESTATION_LONGPASSAGE;
    seSpaceStation_Hall : ThePreset := EFX_REVERB_PRESET_SPACESTATION_HALL;
    seSpaceStation_CupBoard : ThePreset := EFX_REVERB_PRESET_SPACESTATION_CUPBOARD;
   // seSpaceStation_CourtYard : ThePreset := EFX_REVERB_PRESET_SPACESTATION_COURTYARD;
    seSpaceStation_Alcove : ThePreset := EFX_REVERB_PRESET_SPACESTATION_ALCOVE;
    seWooden_SmallRoom : ThePreset := EFX_REVERB_PRESET_WOODEN_SMALLROOM;
    seWooden_ShortPassage : ThePreset := EFX_REVERB_PRESET_WOODEN_SHORTPASSAGE;
    seWooden_MediumRoom : ThePreset := EFX_REVERB_PRESET_WOODEN_MEDIUMROOM;
    seWooden_LargeRoom : ThePreset := EFX_REVERB_PRESET_WOODEN_LARGEROOM;
    seWooden_LongPassage : ThePreset := EFX_REVERB_PRESET_WOODEN_LONGPASSAGE;
    seWooden_Hall : ThePreset := EFX_REVERB_PRESET_WOODEN_HALL;
    seWooden_CupBoard : ThePreset := EFX_REVERB_PRESET_WOODEN_CUPBOARD;
    seWooden_CourtYard : ThePreset := EFX_REVERB_PRESET_WOODEN_COURTYARD;
    seWooden_Alcove : ThePreset := EFX_REVERB_PRESET_WOODEN_ALCOVE;
    seSport_EmptyStadium : ThePreset := EFX_REVERB_PRESET_SPORT_EMPTYSTADIUM;
    seSport_SquashCourt : ThePreset := EFX_REVERB_PRESET_SPORT_SQUASHCOURT;
    seSport_SmallSwimmingPool : ThePreset := EFX_REVERB_PRESET_SPORT_SMALLSWIMMINGPOOL;
    seSport_LargeSwimmingPool : ThePreset := EFX_REVERB_PRESET_SPORT_LARGESWIMMINGPOOL;
    seSport_Gymnasium : ThePreset := EFX_REVERB_PRESET_SPORT_GYMNASIUM;
    seSport_FullStadium : ThePreset := EFX_REVERB_PRESET_SPORT_FULLSTADIUM;
    seSport_StadiumTannoy : ThePreset := EFX_REVERB_PRESET_SPORT_STADIUMTANNOY;
    sePrefab_WorkShop : ThePreset := EFX_REVERB_PRESET_PREFAB_WORKSHOP;
    sePrefab_SchoolRoom : ThePreset := EFX_REVERB_PRESET_PREFAB_SCHOOLROOM;
    sePrefab_PractiseRoom : ThePreset := EFX_REVERB_PRESET_PREFAB_PRACTISEROOM;
    sePrefab_OutHouse : ThePreset := EFX_REVERB_PRESET_PREFAB_OUTHOUSE;
    sePrefab_Caravan : ThePreset := EFX_REVERB_PRESET_PREFAB_CARAVAN;
    seDome_Tomb : ThePreset := EFX_REVERB_PRESET_DOME_TOMB;
    seDome_SaintPauls : ThePreset := EFX_REVERB_PRESET_DOME_SAINTPAULS;
    sePipe_Small : ThePreset := EFX_REVERB_PRESET_PIPE_SMALL;
    sePipe_Longthin : ThePreset := EFX_REVERB_PRESET_PIPE_LONGTHIN;
    sePipe_Large : ThePreset := EFX_REVERB_PRESET_PIPE_LARGE;
    sePipe_Resonant : ThePreset := EFX_REVERB_PRESET_PIPE_RESONANT;
    seOutDoors_BackYard : ThePreset := EFX_REVERB_PRESET_OUTDOORS_BACKYARD;
    seOutDoors_RollingPlains : ThePreset := EFX_REVERB_PRESET_OUTDOORS_ROLLINGPLAINS;
    seOutDoors_DeepCanyon : ThePreset := EFX_REVERB_PRESET_OUTDOORS_DEEPCANYON;
    seOutDoors_Creek : ThePreset := EFX_REVERB_PRESET_OUTDOORS_CREEK;
    seOutDoors_Valley : ThePreset := EFX_REVERB_PRESET_OUTDOORS_VALLEY;
    seMood_Heaven : ThePreset := EFX_REVERB_PRESET_MOOD_HEAVEN;
    seMood_Hell : ThePreset := EFX_REVERB_PRESET_MOOD_HELL;
    seMood_Memory : ThePreset := EFX_REVERB_PRESET_MOOD_MEMORY;
    seDriving_Commentator : ThePreset := EFX_REVERB_PRESET_DRIVING_COMMENTATOR;
    seDriving_PitGarage : ThePreset := EFX_REVERB_PRESET_DRIVING_PITGARAGE;
    seDriving_Incar_Racer : ThePreset := EFX_REVERB_PRESET_DRIVING_INCAR_RACER;
    seDriving_Incar_Sports : ThePreset := EFX_REVERB_PRESET_DRIVING_INCAR_SPORTS;
    seDriving_Incar_Luxury : ThePreset := EFX_REVERB_PRESET_DRIVING_INCAR_LUXURY;
    seDriving_FullGrandStand : ThePreset := EFX_REVERB_PRESET_DRIVING_FULLGRANDSTAND;
    seDriving_EmptyGrandStand : ThePreset := EFX_REVERB_PRESET_DRIVING_EMPTYGRANDSTAND;
    seDriving_Tunnel : ThePreset := EFX_REVERB_PRESET_DRIVING_TUNNEL;
    seCity_Streets : ThePreset := EFX_REVERB_PRESET_CITY_STREETS;
    seCity_SubWay : ThePreset := EFX_REVERB_PRESET_CITY_SUBWAY;
    seCity_Museum : ThePreset := EFX_REVERB_PRESET_CITY_MUSEUM;
    seCity_Library : ThePreset := EFX_REVERB_PRESET_CITY_LIBRARY;
    seCity_UnderPass : ThePreset := EFX_REVERB_PRESET_CITY_UNDERPASS;
    seCity_Abandoned : ThePreset := EFX_REVERB_PRESET_CITY_ABANDONED;
    seDustyRoom : ThePreset := EFX_REVERB_PRESET_DUSTYROOM;
    seChapel : ThePreset := EFX_REVERB_PRESET_CHAPEL;
    seSmallWaterRoom : ThePreset := EFX_REVERB_PRESET_SMALLWATERROOM;
  end;
  Setparams(ThePreset);
end;

procedure TBZReverbSoundEffect.WriteToFiler(Awriter: TWriter);
begin
  inherited;
  With AWriter do
  begin
    WriteInteger(0);
  end;
end;

procedure TBZReverbSoundEffect.ReadFromFiler(Areader: TReader);
begin
  inherited;
  with AReader do
  begin
    ReadInteger; // ignore archiveVersion
  end;
end;

procedure TBZReverbSoundEffect.Assign(Source: TPersistent);
begin
  if Source is TBZReverbSoundEffect then
  begin

  end
  else inherited Assign(Source);
end;

{%endregion%}

{%region=====[ TBZEAXReverbSoundEffect ]======================================}

constructor TBZEAXReverbSoundEffect.Create(AOwner: TBZXCollection);
begin
 inherited Create(AOwner);
end;

destructor TBZEAXReverbSoundEffect.Destroy;
begin
  Inherited Destroy;
end;

class function TBZEAXReverbSoundEffect.FriendlyName: String;
begin
  Result := 'EFX_EAX_Reverb_Sound_Effect';
End;

procedure TBZEAXReverbSoundEffect.SetPreset(val : TBZSoundReverbEnvironment);
var
  ThePreset : EFXEAXREVERBPROPERTIES;

  procedure SetParams(aPreset : EFXEAXREVERBPROPERTIES);
  begin
    FDENSITY               := aPreset.flDensity;
    FDIFFUSION             := aPreset.flDiffusion;
    FGAIN                  := aPreset.flGain;
    FGAINHF                := aPreset.flGainHF;
    FDECAY_TIME            := aPreset.flDecayTime;
    FDECAY_HFRATIO         := aPreset.flDecayHFRatio;
    FREFLECTIONS_GAIN      := aPreset.flReflectionsGain;
    FREFLECTIONS_DELAY     := aPreset.flReflectionsDelay;
    FLATE_REVERB_GAIN      := aPreset.flLateReverbGain;
    FLATE_REVERB_DELAY     := aPreset.flLateReverbDelay;
    FAIR_ABSORPTION_GAINHF := aPreset.flAirAbsorptionGainHF;
    FROOM_ROLLOFF_FACTOR   := aPreset.flRoomRolloffFactor;
    FDECAY_HFLIMIT         := (aPreset.iDecayHFLimit>0);
  End;

begin
  Case val of
    seGeneric : ThePreset := EFX_REVERB_PRESET_GENERIC;
    sePaddedCell : ThePreset := EFX_REVERB_PRESET_PADDEDCELL;
    seRoom : ThePreset := EFX_REVERB_PRESET_ROOM;
    seBathroom : ThePreset := EFX_REVERB_PRESET_BATHROOM;
    seLivingRoom : ThePreset := EFX_REVERB_PRESET_LIVINGROOM;
    seStoneroom : ThePreset := EFX_REVERB_PRESET_STONEROOM;
    seAuditorium : ThePreset := EFX_REVERB_PRESET_AUDITORIUM;
    seConcertHall : ThePreset := EFX_REVERB_PRESET_CONCERTHALL;
    seCave : ThePreset := EFX_REVERB_PRESET_CAVE;
    seArena : ThePreset := EFX_REVERB_PRESET_ARENA;
    seHangar : ThePreset := EFX_REVERB_PRESET_HANGAR;
    seCarpetedHallway : ThePreset := EFX_REVERB_PRESET_CARPETEDHALLWAY;
    seHallway : ThePreset := EFX_REVERB_PRESET_HALLWAY;
    seStoneCorridor : ThePreset := EFX_REVERB_PRESET_STONECORRIDOR;
    seAlley : ThePreset := EFX_REVERB_PRESET_ALLEY;
    seForest : ThePreset := EFX_REVERB_PRESET_FOREST;
    seCity : ThePreset := EFX_REVERB_PRESET_CITY;
    seMountains : ThePreset := EFX_REVERB_PRESET_MOUNTAINS;
    seQuarry : ThePreset := EFX_REVERB_PRESET_QUARRY;
    sePlain : ThePreset := EFX_REVERB_PRESET_PLAIN;
    seParkingLot : ThePreset := EFX_REVERB_PRESET_PARKINGLOT;
    seSewerPipe : ThePreset := EFX_REVERB_PRESET_SEWERPIPE;
    seUnderWater : ThePreset := EFX_REVERB_PRESET_UNDERWATER;
    seDrugged : ThePreset := EFX_REVERB_PRESET_DRUGGED;
    seDizzy : ThePreset := EFX_REVERB_PRESET_DIZZY;
    sePsychotic : ThePreset := EFX_REVERB_PRESET_PSYCHOTIC;
    seCastle_SmallRoom : ThePreset := EFX_REVERB_PRESET_CASTLE_SMALLROOM;
    seCastle_ShortPassage : ThePreset := EFX_REVERB_PRESET_CASTLE_SHORTPASSAGE;
    seCastle_MediumRoom : ThePreset := EFX_REVERB_PRESET_CASTLE_MEDIUMROOM;
    seCastle_LargeRoom : ThePreset := EFX_REVERB_PRESET_CASTLE_LARGEROOM;
    seCastle_LongPassage : ThePreset := EFX_REVERB_PRESET_CASTLE_LONGPASSAGE;
    seCastle_Hall : ThePreset := EFX_REVERB_PRESET_CASTLE_HALL;
    seCastle_CupBoarb : ThePreset := EFX_REVERB_PRESET_CASTLE_CUPBOARD;
    seCastle_CourtYard : ThePreset := EFX_REVERB_PRESET_CASTLE_COURTYARD;
    seCastle_Alcove : ThePreset := EFX_REVERB_PRESET_CASTLE_ALCOVE;
    seFactory_SmallRoom : ThePreset := EFX_REVERB_PRESET_FACTORY_SMALLROOM;
    seFactory_ShortPassage : ThePreset := EFX_REVERB_PRESET_FACTORY_SHORTPASSAGE;
    seFactory_MediumRoom : ThePreset := EFX_REVERB_PRESET_FACTORY_MEDIUMROOM;
    seFactory_LargeRoom : ThePreset := EFX_REVERB_PRESET_FACTORY_LARGEROOM;
    seFactory_LongPassage : ThePreset := EFX_REVERB_PRESET_FACTORY_LONGPASSAGE;
    seFactory_Hall : ThePreset := EFX_REVERB_PRESET_FACTORY_HALL;
    seFactory_CupBoard : ThePreset := EFX_REVERB_PRESET_FACTORY_CUPBOARD;
    seFactory_CourtYard : ThePreset := EFX_REVERB_PRESET_FACTORY_COURTYARD;
    seFactory_Alcove : ThePreset := EFX_REVERB_PRESET_FACTORY_ALCOVE;
    seIcePalace_SmallRoom : ThePreset := EFX_REVERB_PRESET_ICEPALACE_SMALLROOM;
    seIcePalace_ShortPassage : ThePreset := EFX_REVERB_PRESET_ICEPALACE_SHORTPASSAGE;
    seIcePalace_MediumRoom : ThePreset := EFX_REVERB_PRESET_ICEPALACE_MEDIUMROOM;
    seIcePalace_LargeRoom : ThePreset := EFX_REVERB_PRESET_ICEPALACE_LARGEROOM;
    seIcePalace_LongPassage : ThePreset := EFX_REVERB_PRESET_ICEPALACE_LONGPASSAGE;
    seIcePalace_Hall : ThePreset := EFX_REVERB_PRESET_ICEPALACE_HALL;
    seIcePalace_CupBoard : ThePreset := EFX_REVERB_PRESET_ICEPALACE_CUPBOARD;
    seIcePalace_CourtYard : ThePreset := EFX_REVERB_PRESET_ICEPALACE_COURTYARD;
    seIcePalace_Alcove : ThePreset := EFX_REVERB_PRESET_ICEPALACE_ALCOVE;
    seSpaceStation_SmallRoom : ThePreset := EFX_REVERB_PRESET_SPACESTATION_SMALLROOM;
    seSpaceStation_ShortPassage : ThePreset := EFX_REVERB_PRESET_SPACESTATION_SHORTPASSAGE;
    seSpaceStation_MediumRoom : ThePreset := EFX_REVERB_PRESET_SPACESTATION_MEDIUMROOM;
    seSpaceStation_LargeRoom : ThePreset := EFX_REVERB_PRESET_SPACESTATION_LARGEROOM;
    seSpaceStation_LongPassage : ThePreset := EFX_REVERB_PRESET_SPACESTATION_LONGPASSAGE;
    seSpaceStation_Hall : ThePreset := EFX_REVERB_PRESET_SPACESTATION_HALL;
    seSpaceStation_CupBoard : ThePreset := EFX_REVERB_PRESET_SPACESTATION_CUPBOARD;
    //seSpaceStation_CourtYard : ThePreset := EFX_REVERB_PRESET_SPACESTATION_COURTYARD;
    seSpaceStation_Alcove : ThePreset := EFX_REVERB_PRESET_SPACESTATION_ALCOVE;
    seWooden_SmallRoom : ThePreset := EFX_REVERB_PRESET_WOODEN_SMALLROOM;
    seWooden_ShortPassage : ThePreset := EFX_REVERB_PRESET_WOODEN_SHORTPASSAGE;
    seWooden_MediumRoom : ThePreset := EFX_REVERB_PRESET_WOODEN_MEDIUMROOM;
    seWooden_LargeRoom : ThePreset := EFX_REVERB_PRESET_WOODEN_LARGEROOM;
    seWooden_LongPassage : ThePreset := EFX_REVERB_PRESET_WOODEN_LONGPASSAGE;
    seWooden_Hall : ThePreset := EFX_REVERB_PRESET_WOODEN_HALL;
    seWooden_CupBoard : ThePreset := EFX_REVERB_PRESET_WOODEN_CUPBOARD;
    seWooden_CourtYard : ThePreset := EFX_REVERB_PRESET_WOODEN_COURTYARD;
    seWooden_Alcove : ThePreset := EFX_REVERB_PRESET_WOODEN_ALCOVE;
    seSport_EmptyStadium : ThePreset := EFX_REVERB_PRESET_SPORT_EMPTYSTADIUM;
    seSport_SquashCourt : ThePreset := EFX_REVERB_PRESET_SPORT_SQUASHCOURT;
    seSport_SmallSwimmingPool : ThePreset := EFX_REVERB_PRESET_SPORT_SMALLSWIMMINGPOOL;
    seSport_LargeSwimmingPool : ThePreset := EFX_REVERB_PRESET_SPORT_LARGESWIMMINGPOOL;
    seSport_Gymnasium : ThePreset := EFX_REVERB_PRESET_SPORT_GYMNASIUM;
    seSport_FullStadium : ThePreset := EFX_REVERB_PRESET_SPORT_FULLSTADIUM;
    seSport_StadiumTannoy : ThePreset := EFX_REVERB_PRESET_SPORT_STADIUMTANNOY;
    sePrefab_WorkShop : ThePreset := EFX_REVERB_PRESET_PREFAB_WORKSHOP;
    sePrefab_SchoolRoom : ThePreset := EFX_REVERB_PRESET_PREFAB_SCHOOLROOM;
    sePrefab_PractiseRoom : ThePreset := EFX_REVERB_PRESET_PREFAB_PRACTISEROOM;
    sePrefab_OutHouse : ThePreset := EFX_REVERB_PRESET_PREFAB_OUTHOUSE;
    sePrefab_Caravan : ThePreset := EFX_REVERB_PRESET_PREFAB_CARAVAN;
    seDome_Tomb : ThePreset := EFX_REVERB_PRESET_DOME_TOMB;
    seDome_SaintPauls : ThePreset := EFX_REVERB_PRESET_DOME_SAINTPAULS;
    sePipe_Small : ThePreset := EFX_REVERB_PRESET_PIPE_SMALL;
    sePipe_Longthin : ThePreset := EFX_REVERB_PRESET_PIPE_LONGTHIN;
    sePipe_Large : ThePreset := EFX_REVERB_PRESET_PIPE_LARGE;
    sePipe_Resonant : ThePreset := EFX_REVERB_PRESET_PIPE_RESONANT;
    seOutDoors_BackYard : ThePreset := EFX_REVERB_PRESET_OUTDOORS_BACKYARD;
    seOutDoors_RollingPlains : ThePreset := EFX_REVERB_PRESET_OUTDOORS_ROLLINGPLAINS;
    seOutDoors_DeepCanyon : ThePreset := EFX_REVERB_PRESET_OUTDOORS_DEEPCANYON;
    seOutDoors_Creek : ThePreset := EFX_REVERB_PRESET_OUTDOORS_CREEK;
    seOutDoors_Valley : ThePreset := EFX_REVERB_PRESET_OUTDOORS_VALLEY;
    seMood_Heaven : ThePreset := EFX_REVERB_PRESET_MOOD_HEAVEN;
    seMood_Hell : ThePreset := EFX_REVERB_PRESET_MOOD_HELL;
    seMood_Memory : ThePreset := EFX_REVERB_PRESET_MOOD_MEMORY;
    seDriving_Commentator : ThePreset := EFX_REVERB_PRESET_DRIVING_COMMENTATOR;
    seDriving_PitGarage : ThePreset := EFX_REVERB_PRESET_DRIVING_PITGARAGE;
    seDriving_Incar_Racer : ThePreset := EFX_REVERB_PRESET_DRIVING_INCAR_RACER;
    seDriving_Incar_Sports : ThePreset := EFX_REVERB_PRESET_DRIVING_INCAR_SPORTS;
    seDriving_Incar_Luxury : ThePreset := EFX_REVERB_PRESET_DRIVING_INCAR_LUXURY;
    seDriving_FullGrandStand : ThePreset := EFX_REVERB_PRESET_DRIVING_FULLGRANDSTAND;
    seDriving_EmptyGrandStand : ThePreset := EFX_REVERB_PRESET_DRIVING_EMPTYGRANDSTAND;
    seDriving_Tunnel : ThePreset := EFX_REVERB_PRESET_DRIVING_TUNNEL;
    seCity_Streets : ThePreset := EFX_REVERB_PRESET_CITY_STREETS;
    seCity_SubWay : ThePreset := EFX_REVERB_PRESET_CITY_SUBWAY;
    seCity_Museum : ThePreset := EFX_REVERB_PRESET_CITY_MUSEUM;
    seCity_Library : ThePreset := EFX_REVERB_PRESET_CITY_LIBRARY;
    seCity_UnderPass : ThePreset := EFX_REVERB_PRESET_CITY_UNDERPASS;
    seCity_Abandoned : ThePreset := EFX_REVERB_PRESET_CITY_ABANDONED;
    seDustyRoom : ThePreset := EFX_REVERB_PRESET_DUSTYROOM;
    seChapel : ThePreset := EFX_REVERB_PRESET_CHAPEL;
    seSmallWaterRoom : ThePreset := EFX_REVERB_PRESET_SMALLWATERROOM;
  end;
  Setparams(ThePreset);
end;

procedure TBZEAXReverbSoundEffect.WriteToFiler(Awriter: TWriter);
begin
  inherited;
  With AWriter do
  begin
    WriteInteger(0);
  end;
end;

procedure TBZEAXReverbSoundEffect.ReadFromFiler(Areader: TReader);
begin
  inherited;
  with AReader do
  begin
    ReadInteger; // ignore archiveVersion
  end;
end;

procedure TBZEAXReverbSoundEffect.Assign(Source: TPersistent);
begin
  if Source is TBZReverbSoundEffect then
  begin

  end
  else inherited Assign(Source);
end;

{%endregion%}

{%region=====[ TBZChorusSoundEffect ]=========================================}

constructor TBZChorusSoundEffect.Create(AOwner: TBZXCollection);
begin
 inherited Create(AOwner);
end;

destructor TBZChorusSoundEffect.Destroy;
begin
  Inherited Destroy;
end;

class function TBZChorusSoundEffect.FriendlyName: String;
begin
  Result := 'EFX_Chorus_Sound_Effect';
End;

procedure TBZChorusSoundEffect.WriteToFiler(Awriter: TWriter);
begin
  inherited;
  With AWriter do
  begin
    WriteInteger(0);
  end;
end;

procedure TBZChorusSoundEffect.ReadFromFiler(Areader: TReader);
begin
  inherited;
  with AReader do
  begin
    ReadInteger; // ignore archiveVersion
  end;
end;

procedure TBZChorusSoundEffect.Assign(Source: TPersistent);
begin
  if Source is TBZReverbSoundEffect then
  begin

  end
  else inherited Assign(Source);
end;

{%endregion%}

{%region=====[ TBZEchoSoundEffect ]===========================================}

constructor TBZEchoSoundEffect.Create(AOwner: TBZXCollection);
begin
 inherited Create(AOwner);
end;

destructor TBZEchoSoundEffect.Destroy;
begin
  Inherited Destroy;
end;

class function TBZEchoSoundEffect.FriendlyName: String;
begin
  Result := 'EFX_Echo_Sound_Effect';
End;

procedure TBZEchoSoundEffect.WriteToFiler(Awriter: TWriter);
begin
  inherited;
  With AWriter do
  begin
    WriteInteger(0);
  end;
end;

procedure TBZEchoSoundEffect.ReadFromFiler(Areader: TReader);
begin
  inherited;
  with AReader do
  begin
    ReadInteger; // ignore archiveVersion
  end;
end;

procedure TBZEchoSoundEffect.Assign(Source: TPersistent);
begin
  if Source is TBZReverbSoundEffect then
  begin

  end
  else inherited Assign(Source);
end;
{%endregion%}

{%region=====[ TBZDistorsionSoundEffect ]=====================================}

constructor TBZDistortionSoundEffect.Create(AOwner: TBZXCollection);
begin
 inherited Create(AOwner);
end;

destructor TBZDistortionSoundEffect.Destroy;
begin
  Inherited Destroy;
end;

class function TBZDistortionSoundEffect.FriendlyName: String;
begin
  Result := 'EFX_Distortion_Sound_Effect';
End;

procedure TBZDistortionSoundEffect.WriteToFiler(Awriter: TWriter);
begin
  inherited;
  With AWriter do
  begin
    WriteInteger(0);
  end;
end;

procedure TBZDistortionSoundEffect.ReadFromFiler(Areader: TReader);
begin
  inherited;
  with AReader do
  begin
    ReadInteger; // ignore archiveVersion
  end;
end;

procedure TBZDistortionSoundEffect.Assign(Source: TPersistent);
begin
  if Source is TBZReverbSoundEffect then
  begin

  end
  else inherited Assign(Source);
end;
{%endregion%}

{%region=====[ TBZFlangerSoundEffect ]========================================}

constructor TBZFlangerSoundEffect.Create(AOwner: TBZXCollection);
begin
 inherited Create(AOwner);
end;

destructor TBZFlangerSoundEffect.Destroy;
begin
  Inherited Destroy;
end;

class function TBZFlangerSoundEffect.FriendlyName: String;
begin
  Result := 'EFX_Flanger_Sound_Effect';
End;

procedure TBZFlangerSoundEffect.WriteToFiler(Awriter: TWriter);
begin
  inherited;
  With AWriter do
  begin
    WriteInteger(0);
  end;
end;

procedure TBZFlangerSoundEffect.ReadFromFiler(Areader: TReader);
begin
  inherited;
  with AReader do
  begin
    ReadInteger; // ignore archiveVersion
  end;
end;

procedure TBZFlangerSoundEffect.Assign(Source: TPersistent);
begin
  if Source is TBZReverbSoundEffect then
  begin

  end
  else inherited Assign(Source);
end;

{%endregion%}

{%region=====[ TBZCompressorSoundEffect ]=====================================}

constructor TBZCompressorSoundEffect.Create(AOwner: TBZXCollection);
begin
 inherited Create(AOwner);
end;

destructor TBZCompressorSoundEffect.Destroy;
begin
  Inherited Destroy;
end;

class function TBZCompressorSoundEffect.FriendlyName: String;
begin
  Result := 'EFX_Compressor_Sound_Effect';
End;

procedure TBZCompressorSoundEffect.WriteToFiler(Awriter: TWriter);
begin
  inherited;
  With AWriter do
  begin
    WriteInteger(0);
  end;
end;

procedure TBZCompressorSoundEffect.ReadFromFiler(Areader: TReader);
begin
  inherited;
  with AReader do
  begin
    ReadInteger; // ignore archiveVersion
  end;
end;

procedure TBZCompressorSoundEffect.Assign(Source: TPersistent);
begin
  if Source is TBZReverbSoundEffect then
  begin

  end
  else inherited Assign(Source);
end;
{%endregion%}

{%region=====[ TBZEqualizerSoundEffect ]======================================}

constructor TBZEqualizerSoundEffect.Create(AOwner: TBZXCollection);
begin
 inherited Create(AOwner);
end;

destructor TBZEqualizerSoundEffect.Destroy;
begin
  Inherited Destroy;
end;

class function TBZEqualizerSoundEffect.FriendlyName: String;
begin
  Result := 'EFX_Equalizer_Sound_Effect';
End;

procedure TBZEqualizerSoundEffect.WriteToFiler(Awriter: TWriter);
begin
  inherited;
  With AWriter do
  begin
    WriteInteger(0);
  end;
end;

procedure TBZEqualizerSoundEffect.ReadFromFiler(Areader: TReader);
begin
  inherited;
  with AReader do
  begin
    ReadInteger; // ignore archiveVersion
  end;
end;

procedure TBZEqualizerSoundEffect.Assign(Source: TPersistent);
begin
  if Source is TBZReverbSoundEffect then
  begin

  end
  else inherited Assign(Source);
end;

{%endregion%}

{%region=====[ TBZCustomSoundFilterItem ]=====================================}

class function TBZCustomSoundFilterItem.FriendlyName: String;
begin
  Result := 'Custom_Sound_Filter';
End;

{%endregion%}

{%region=====[ TBZLowPassSoundFilter ]========================================}

constructor TBZLowPassSoundFilter.Create(AOwner: TBZXCollection);
begin
 inherited Create(AOwner);
 FGAIN := 1.0;
 FGAINHF := 1.0;
end;

destructor TBZLowPassSoundFilter.Destroy;
begin
  Inherited Destroy;
end;

class function TBZLowPassSoundFilter.FriendlyName: String;
begin
  Result := 'LowPass_Sound_Filter';
End;

procedure TBZLowPassSoundFilter.WriteToFiler(Awriter: TWriter);
begin
  inherited;
  With AWriter do
  begin
    WriteInteger(0);
  end;
end;

procedure TBZLowPassSoundFilter.ReadFromFiler(Areader: TReader);
begin
  inherited;
  with AReader do
  begin
    ReadInteger; // ignore archiveVersion
  end;
end;

procedure TBZLowPassSoundFilter.Assign(Source: TPersistent);
begin
  if Source is TBZReverbSoundEffect then
  begin

  end
  else inherited Assign(Source);
end;

{%endregion%}

{%region=====[ TBZHighPassSoundFilter ]=======================================}

constructor TBZHighPassSoundFilter.Create(AOwner: TBZXCollection);
begin
 inherited Create(AOwner);
end;

destructor TBZHighPassSoundFilter.Destroy;
begin
  Inherited Destroy;
end;

class function TBZHighPassSoundFilter.FriendlyName: String;
begin
  Result := 'HighPass_Sound_Filter';
End;

procedure TBZHighPassSoundFilter.WriteToFiler(Awriter: TWriter);
begin
  inherited;
  With AWriter do
  begin
    WriteInteger(0);
  end;
end;

procedure TBZHighPassSoundFilter.ReadFromFiler(Areader: TReader);
begin
  inherited;
  with AReader do
  begin
    ReadInteger; // ignore archiveVersion
  end;
end;

procedure TBZHighPassSoundFilter.Assign(Source: TPersistent);
begin
  if Source is TBZReverbSoundEffect then
  begin

  end
  else inherited Assign(Source);
end;

{%endregion%}

{%region=====[ TBZBandPassSoundFilter ]=======================================}

constructor TBZBandPassSoundFilter.Create(AOwner: TBZXCollection);
begin
 inherited Create(AOwner);
end;

destructor TBZBandPassSoundFilter.Destroy;
begin
  Inherited Destroy;
end;

class function TBZBandPassSoundFilter.FriendlyName: String;
begin
  Result := 'BandPass_Sound_Filter';
End;

procedure TBZBandPassSoundFilter.WriteToFiler(Awriter: TWriter);
begin
  inherited;
  With AWriter do
  begin
    WriteInteger(0);
  end;
end;

procedure TBZBandPassSoundFilter.ReadFromFiler(Areader: TReader);
begin
  inherited;
  with AReader do
  begin
    ReadInteger; // ignore archiveVersion
  end;
end;

procedure TBZBandPassSoundFilter.Assign(Source: TPersistent);
begin
  if Source is TBZReverbSoundEffect then
  begin

  end
  else inherited Assign(Source);
end;

{%endregion%}

{%region=====[ TBZSoundFXLib ]================================================}

(*constructor  TBZSoundFXLib.Create(aOwner: TPersistent);
begin
 // Assert(aOwner is TGLBaseSceneObject);
  inherited Create(aOwner);
end; *)

function  TBZSoundFXLib.GetNamePath: string;
var
  s: string;
begin
  Result := ClassName;
  if GetOwner = nil then
    Exit;
  s := GetOwner.GetNamePath;
  if s = '' then
    Exit;
  Result := s + '.SoundFXLib';
end;

class function  TBZSoundFXLib.ItemsClass: TBZXCollectionItemClass;
begin
  Result := TBZCustomSoundFXItem;
end;

function TBZSoundFXLib.GetItem(index: Integer): TBZCustomSoundFXItem;
begin
 // ShowMessage('SoundFXLib GetItem At : '+IntToStr(Index));
  Result := nil;
  if (Index>=0) and (Index<Count) then
    Result := TBZCustomSoundFXItem(Items[index]);
end;

function TBZSoundFXLib.CanAdd(aClass: TBZXCollectionItemClass): Boolean;
begin
  Result := (aClass.InheritsFrom(TBZCustomSoundFXItem)) and (inherited CanAdd(aClass));
  ShowMessage('CanAdd :'+BoolToStr(Result,True));
end;

function TBZSoundFXLib.GetItemByName(const AName: TBZSoundFXName): TBZCustomSoundFXItem;
var
  i:Integer;
begin
  Result := nil;
  //ShowMessage('Count : '+InttoStr(Count));
  for i := 0 to Count - 1 do
  begin
    if Items[i]=nil then  ShowMessage('ERROR');
   // ShowMessage('Compare : '+InttoStr(I)+' - '+AName+' --> '+Self.Items[i].Name);
    if (Items[i].Name = AName) then
    begin
      Result := TBZCustomSoundFXItem(Items[i]);
      exit;
    end;
  end;
End;

function TBZSoundFXLib.AddFilterLowPass(AName: TBZSoundFXName) : TBZLowPassSoundFilter;
begin
  Result := TBZLowPassSoundFilter.Create(Self);
  Result.Name := AName;
  Add(Result);
End;

function TBZSoundFXLib.AddFilterHighPass(AName: TBZSoundFXName) : TBZHighPassSoundFilter;
begin
  Result := TBZHighPassSoundFilter.Create(Self);
  Result.Name := AName;
  Add(Result);
End;

function TBZSoundFXLib.AddFilterBandPass(AName: TBZSoundFXName) : TBZBandPassSoundFilter;
begin
  Result := TBZBandPassSoundFilter.Create(Self);
  Result.Name := AName;
  Add(Result);
End;

function TBZSoundFXLib.GetFilterLowPassByName(const AName: TBZSoundFXName): TBZLowPassSoundFilter;
var
 I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TBZLowPassSoundFilter) and (Items[I].Name = AName) then
    begin
        Result := TBZLowPassSoundFilter(Items[I]);
        exit;
    end;
  end;
end;

function TBZSoundFXLib.GetFilterHighPassByName(const AName: TBZSoundFXName): TBZHighPassSoundFilter;
var
 I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TBZHighPassSoundFilter) and (Items[I].Name = AName) then
    begin
        Result := TBZHighPassSoundFilter(Items[I]);
        exit;
    end;
  end;
end;

function TBZSoundFXLib.GetFilterBandPassByName(const AName: TBZSoundFXName): TBZBandPassSoundFilter;
var
 I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TBZBandPassSoundFilter) and (Items[I].Name = AName) then
    begin
        Result := TBZBandPassSoundFilter(Items[I]);
        exit;
    end;
  end;
end;

function TBZSoundFXLib.AddEffectReverb(AName: TBZSoundFXName) : TBZReverbSoundEffect;
begin
  Result := TBZReverbSoundEffect.Create(Self);
  Result.Name := AName;
  Add(Result);
End;

function TBZSoundFXLib.AddEffectEAXReverb(AName: TBZSoundFXName) : TBZEAXReverbSoundEffect;
begin
  Result := TBZEAXReverbSoundEffect.Create(Self);
  Result.Name := AName;
  Add(Result);
End;

function TBZSoundFXLib.AddEffectChorus(AName: TBZSoundFXName) : TBZChorusSoundEffect;
begin
  Result := TBZChorusSoundEffect.Create(Self);
  Result.Name := AName;
  Add(Result);
End;

function TBZSoundFXLib.AddEffectEcho(AName: TBZSoundFXName) : TBZEchoSoundEffect;
begin
  Result := TBZEchoSoundEffect.Create(Self);
  Result.Name := AName;
  Add(Result);
End;

function TBZSoundFXLib.AddEffectDistortion(AName: TBZSoundFXName) : TBZDistortionSoundEffect;
begin
  Result := TBZDistortionSoundEffect.Create(Self);
  Result.Name := AName;
  Add(Result);
End;

function TBZSoundFXLib.AddEffectFlanger(AName: TBZSoundFXName) : TBZFlangerSoundEffect;
begin
  Result := TBZFlangerSoundEffect.Create(Self);
  Result.Name := AName;
  Add(Result);
End;

function TBZSoundFXLib.AddEffectCompressor(AName: TBZSoundFXName) : TBZCompressorSoundEffect;
begin
  Result := TBZCompressorSoundEffect.Create(Self);
  Result.Name := AName;
  Add(Result);
End;

function TBZSoundFXLib.AddEffectEqualizer(AName: TBZSoundFXName) : TBZEqualizerSoundEffect;
begin
  Result := TBZEqualizerSoundEffect.Create(Self);
  Result.Name := AName;
  Add(Result);
End;

function TBZSoundFXLib.GetEffectReverbByName(const AName: TBZSoundFXName): TBZReverbSoundEffect;
var
 I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TBZReverbSoundEffect) and (Items[I].Name = AName) then
    begin
        Result := TBZReverbSoundEffect(Items[I]);
        exit;
    end;
  end;
end;

function TBZSoundFXLib.GetEffectEAXReverbByName(const AName: TBZSoundFXName): TBZEAXReverbSoundEffect;
var
 I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TBZEAXReverbSoundEffect) and (Items[I].Name = AName) then
    begin
        Result := TBZEAXReverbSoundEffect(Items[I]);
        exit;
    end;
  end;
end;

function TBZSoundFXLib.GetEffectChorusByName(const AName: TBZSoundFXName): TBZChorusSoundEffect;
var
 I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TBZChorusSoundEffect) and (Items[I].Name = AName) then
    begin
        Result := TBZChorusSoundEffect(Items[I]);
        exit;
    end;
  end;
end;

function TBZSoundFXLib.GetEffectEchoByName(const AName: TBZSoundFXName): TBZEchoSoundEffect;
var
 I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TBZEchoSoundEffect) and (Items[I].Name = AName) then
    begin
        Result := TBZEchoSoundEffect(Items[I]);
        exit;
    end;
  end;
end;

function TBZSoundFXLib.GetEffectDistortionByName(const AName: TBZSoundFXName): TBZDistortionSoundEffect;
var
 I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TBZDistortionSoundEffect) and (Items[I].Name = AName) then
    begin
        Result := TBZDistortionSoundEffect(Items[I]);
        exit;
    end;
  end;
end;

function TBZSoundFXLib.GetEffectFlangerByName(const AName: TBZSoundFXName): TBZFlangerSoundEffect;
var
 I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TBZFlangerSoundEffect) and (Items[I].Name = AName) then
    begin
        Result := TBZFlangerSoundEffect(Items[I]);
        exit;
    end;
  end;
end;

function TBZSoundFXLib.GetEffectCompressorByName(const AName: TBZSoundFXName): TBZCompressorSoundEffect;
var
 I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TBZCompressorSoundEffect) and (Items[I].Name = AName) then
    begin
        Result := TBZCompressorSoundEffect(Items[I]);
        exit;
    end;
  end;
end;

function TBZSoundFXLib.GetEffectEqualizerByName(const AName: TBZSoundFXName): TBZEqualizerSoundEffect;
var
 I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if (Items[I] is TBZEqualizerSoundEffect) and (Items[I].Name = AName) then
    begin
        Result := TBZEqualizerSoundEffect(Items[I]);
        exit;
    end;
  end;
end;
(*procedure TBZSoundFXLib.DoProgress(const progressTimes: TProgressTimes);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TBZCustomSoundFXItem(Items[i]).DoProgress(progressTimes);
end; *)

{%endregion%}

{%region=====[ TBZSoundFXLibrary ]============================================}

constructor TBZSoundFXLibrary.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSoundFXLib := TBZSoundFXLib.Create(Self);
  vSoundFXLibraries.Add(Self);
end;

destructor TBZSoundFXLibrary.Destroy;
begin
  vSoundFXLibraries.Remove(Self);
  FSoundFXLib.Free;
  inherited Destroy;
end;

procedure TBZSoundFXLibrary.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
end;

procedure TBZSoundFXLibrary.SetSoundFXLib(const val: TBZSoundFXLib);
begin
  FSoundFXLib.Assign(val);
end;

{%endregion%}

{%region=====[ TBZSoundFXAuxSlot ]============================================}

constructor TBZSoundFXAuxSlot.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FSoundFXName :='';
  FSoundFXCache := nil;
  FSoundFXLibrary := nil;
end;

destructor TBZSoundFXAuxSlot.Destroy;
begin
  inherited Destroy;
end;

procedure TBZSoundFXAuxSlot.Assign(Source: TPersistent);
begin
  if Source is TBZSoundFXAuxSlot then
  begin

    FSoundFXLibraryName := TBZSoundFXAuxSlot(Source).FSoundFXLibraryName;
    FSoundFXLibrary := TBZSoundFXAuxSlot(Source).FSoundFXLibrary;
    FSoundFXName := TBZSoundFXAuxSlot(Source).FSoundFXName;
    FActivated := TBZSoundFXAuxSlot(Source).FActivated;
  end
  else
    inherited Assign(Source);
end;

procedure TBZSoundFXAuxSlot.WriteToFiler(Awriter: TWriter);
begin
  inherited;
  with Awriter do
  begin
    WriteInteger(0); // Archive Version 0
    if Assigned(FSoundFXLibrary) then WriteString(FSoundFXLibrary.Name)
    else WriteString(FSoundFXLibraryName);
    WriteString(FSoundFXName);
    WriteBoolean(FActivated);
    // WriteInteger(FFrequency);
  end;
end;

procedure TBZSoundFXAuxSlot.ReadFromFiler(Areader: TReader);
begin
  inherited;
  with AReader do
  begin
    ReadInteger; // ignore archiveVersion
    FSoundFXLibraryName := ReadString;
    FSoundFXLibrary := nil;
    FSoundFXName := ReadString;
    FActivated := ReadBoolean;
  end;
end;

function TBZSoundFXAuxSlot.GetSoundFXLibrary: TBZSoundFXLibrary;
begin
  if (FSoundFXLibrary = nil) and (FSoundFXLibraryName <> '') then
    FSoundFXLibrary := GetSoundFXLibraryByName(FSoundFXLibraryName);
  Result := FSoundFXLibrary;
end;

procedure TBZSoundFXAuxSlot.SetSoundFXLibrary(Const AValue : TBZSoundFXLibrary);
begin
  if  AValue<>nil then
  begin
    if AValue <> FSoundFXLibrary then
    begin
      FSoundFXLibrary := AValue;
      if Assigned(FSoundFXLibrary) then FSoundFXLibraryName := FSoundFXLibrary.Name
      else FSoundFXLibraryName := '';
      //Include(FChanges, sscSample);
    end;
  End;
end;

procedure TBZSoundFXAuxSlot.setSoundFXName(val : String);
begin
  if FSoundFXName = val then exit;
  FSoundFXName := val;
  if FSoundFXLibrary=nil then
  begin
    showmessage('Error');
    exit;
  End;
  FSoundFXCache := FSoundFXLibrary.FX.GetItemByName(val);
//  Include(FChanges, sscSample);
end;

procedure TBZSoundFXAuxSlot.SetActivated(val : Boolean);
begin
  if FActivated = val then exit;
  FActivated := val;
//  Include(FChanges, sscSample);
end;

{%endregion%}

{%region=====[ TBZSoundFXAuxSlots ]===========================================}

constructor TBZSoundFXAuxSlots.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  inherited Create(TBZSoundFXAuxSlot);
end;

function TBZSoundFXAuxSlots.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TBZSoundFXAuxSlots.SetItems(index: Integer; const val: TBZSoundFXAuxSlot);
begin
  inherited Items[index] := val;
end;

function TBZSoundFXAuxSlots.GetItems(index: Integer): TBZSoundFXAuxSlot;
begin
  Result := TBZSoundFXAuxSlot(inherited Items[index]);
end;

function TBZSoundFXAuxSlots.Add: TBZSoundFXAuxSlot;
begin
  Result := (inherited Add) as TBZSoundFXAuxSlot;
end;

function TBZSoundFXAuxSlots.FindItemID(ID: Integer): TBZSoundFXAuxSlot;
begin
  Result := (inherited FindItemID(ID)) as TBZSoundFXAuxSlot;
end;

{%endregion%}

{%region=====[ TBZBaseSoundSource ]===========================================}

constructor TBZBaseSoundSource.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FAuxSlots := TBZSoundFXAuxSlots.Create(nil);
  FChanges := [sscTransformation, sscSample, sscStatus];
  FVolume := 1.0;
  FMinDistance := 1.0;
  FMaxDistance := 100.0;
  FInsideConeAngle := 360;
  FOutsideConeAngle := 360;
  FConeOutsideVolume := 0.0;
  FPitch := 1.0;
  FNbLoops := 1;
  FFrequency := -1;
  FPause := False;
  FLoaded := False;
  FTimePosition := 0.0;
  FPlaying := False; // On met la lecture en pause;
  FDirectFilterName := '';
  FFXChanges := [];
  FAutoFreeOnStop := False;
end;

destructor TBZBaseSoundSource.Destroy;
begin
  FreeAndNil(FAuxSlots);
  inherited Destroy;
end;

function TBZBaseSoundSource.GetSoundLibrary: TBZSoundLibrary;
begin
  if (FSoundLibrary = nil) and (FSoundLibraryName <> '') then
    FSoundLibrary := GetSoundLibraryByName(FSoundLibraryName);
  Result := FSoundLibrary;
end;

procedure TBZBaseSoundSource.SetSoundLibrary(const val: TBZSoundLibrary);
begin
  if val <> FSoundLibrary then
  begin
    FSoundLibrary := val;
    if Assigned(FSoundLibrary) then FSoundLibraryName := FSoundLibrary.Name
    else FSoundLibraryName := '';
   // Include(FChanges, sscSample);
  end;
end;

procedure TBZBaseSoundSource.SetSoundName(const val: string);
begin
  if val <> FSoundName then
  begin
    FSoundName := val;
    Include(FChanges,sscNewSample);
    Include(FChanges,sscSample);
  end;
end;

function TBZBaseSoundSource.GetSoundFXLibrary: TBZSoundFXLibrary;
begin
  if (FSoundFXLibrary = nil) and (FSoundFXLibraryName <> '') then
    FSoundFXLibrary := GetSoundFXLibraryByName(FSoundFXLibraryName);
  Result := FSoundFXLibrary;
end;

procedure TBZBaseSoundSource.SetSoundFXLibrary(const val: TBZSoundFXLibrary);
begin
  if val <> FSoundFXLibrary then
  begin
    FSoundFXLibrary := val;
    if Assigned(FSoundFXLibrary) then FSoundFXLibraryName := FSoundFXLibrary.Name
    else FSoundFXLibraryName := '';
    Include(FChanges, sscSample);
  end;
end;

procedure TBZBaseSoundSource.setDirectFilterName(val : String);
begin
  if FDirectFilterName = val then exit;
  FDirectFilterName := val;
  FDirectFilterCache := TBZCustomSoundFilterItem(FSoundFXLibrary.FX.GetItemByName(val));
  Include(FChanges, sscSample);
end;

procedure TBZBaseSoundSource.SetDirectFilterActivated(val : Boolean);
begin
  if FDirectFilterActivated = val then exit;
  FDirectFilterActivated := val;
  Include(FChanges, sscFXStatus);
  Include(FFXChanges, sfxcDirectFilter);
End;

function TBZBaseSoundSource.GetDirectFilterActivated: Boolean;
begin
  Result := FDirectFilterActivated;
End;

function TBZBaseSoundSource.GetDirectFilterFXCache:TBZCustomSoundFilterItem;
begin
  result := FDirectFilterCache;
End;

function TBZBaseSoundSource.GetDisplayName: string;
begin
  Result := Format('%s', [FSoundName]);
end;

procedure TBZBaseSoundSource.Assign(Source: TPersistent);
begin
  if Source is TBZBaseSoundSource then
  begin
    FPriority := TBZBaseSoundSource(Source).FPriority;
   // FOrigin := TBZBaseSoundSource(Source).FOrigin;
    FVolume := TBZBaseSoundSource(Source).FVolume;
    FMinDistance := TBZBaseSoundSource(Source).FMinDistance;
    FMaxDistance := TBZBaseSoundSource(Source).FMaxDistance;
    FInsideConeAngle := TBZBaseSoundSource(Source).FInsideConeAngle;
    FOutsideConeAngle := TBZBaseSoundSource(Source).FOutsideConeAngle;
    FConeOutsideVolume := TBZBaseSoundSource(Source).FConeOutsideVolume;
    FSoundLibraryName := TBZBaseSoundSource(Source).FSoundLibraryName;
    FSoundLibrary := TBZBaseSoundSource(Source).FSoundLibrary;
    FSoundName := TBZBaseSoundSource(Source).FSoundName;
    FMute := TBZBaseSoundSource(Source).FMute;
    FPause := TBZBaseSoundSource(Source).FPause;
    FPlaying := TBZBaseSoundSource(Source).FPlaying;
    FChanges := [sscTransformation, sscSample, sscStatus];
    FNbLoops := TBZBaseSoundSource(Source).FNbLoops;
    FFrequency := TBZBaseSoundSource(Source).FFrequency;
    FPitch := TBZBaseSoundSource(Source).FPitch;
  end
  else
    inherited Assign(Source);
end;

procedure TBZBaseSoundSource.WriteToFiler(Awriter: TWriter);
begin
  inherited;
  with Awriter do
  begin
    WriteInteger(0); // Archive Version 0
    WriteInteger(FPriority);
    WriteFloat(FVolume);
    WriteFloat(FMinDistance);
    WriteFloat(FMaxDistance);
    WriteFloat(FInsideConeAngle);
    WriteFloat(FOutsideConeAngle);
    WriteFloat(FConeOutsideVolume);
    if Assigned(FSoundLibrary) then WriteString(FSoundLibrary.Name)
    else WriteString(FSoundLibraryName);
    WriteString(FSoundName);
    WriteBoolean(FMute);
    WriteBoolean(FPause);
    WriteBoolean(FPlaying);
    WriteInteger(FNbLoops);
    WriteFloat(FPitch);
    // WriteInteger(FFrequency);
  end;
end;

procedure TBZBaseSoundSource.ReadFromFiler(Areader: TReader);
begin
  inherited;
  with AReader do
  begin
    ReadInteger; // ignore archiveVersion
    FPriority := ReadInteger;
    FVolume := ReadFloat;
    FMinDistance := ReadFloat;
    FMaxDistance := ReadFloat;
    FInsideConeAngle := ReadFloat;
    FOutsideConeAngle := ReadFloat;
    FConeOutsideVolume := ReadFloat;
    FSoundLibraryName := ReadString;
    FSoundLibrary := nil;
    FSoundName := ReadString;
    FMute := ReadBoolean;
    FPause := ReadBoolean;
    FPlaying := ReadBoolean;
    FChanges := [sscTransformation, sscSample, sscStatus];
    FNbLoops := ReadInteger;
    FPitch := ReadFloat;
    FUseEnvironnment := False;
    FUseEqualizer := False;
    // FFrequency:=ReadInteger;
  end;
end;

function TBZBaseSoundSource.Sample: TBZSoundSampleItem;
begin
  if SoundLibrary <> nil then Result := FSoundLibrary.Samples.GetByName(FSoundName)
  else Result := nil;
end;

procedure TBZBaseSoundSource.SetPriority(const val: Integer);
begin
  if val <> FPriority then
  begin
    FPriority := val;
    Include(FChanges, sscStatus);
  end;
end;

(*procedure TBZBaseSoundSource.SetOrigin(const val: TBZBaseSceneObject);
begin
  if val <> FOrigin then
  begin
    FOrigin := val;
    Include(FChanges, sscTransformation);
  end;
end;*)

procedure TBZBaseSoundSource.SetVolume(const val: Single);
begin
  if val <> FVolume then
  begin
    FVolume := Clamp(val, 0, 1);
    Include(FChanges, sscStatus);
  end;
end;

procedure TBZBaseSoundSource.SetMinDistance(const val: Single);
begin
  if val <> FMinDistance then
  begin
    FMinDistance := ClampMin(val, 0);
    Include(FChanges, sscStatus);
  end;
end;

procedure TBZBaseSoundSource.SetMaxDistance(const val: Single);
begin
  if val <> FMaxDistance then
  begin
    FMaxDistance := ClampMin(val, 0);
    Include(FChanges, sscStatus);
  end;
end;

procedure TBZBaseSoundSource.SetInsideConeAngle(const val: Single);
begin
  if val <> FInsideConeAngle then
  begin
    FInsideConeAngle := Clamp(val, 0, 360);
    Include(FChanges, sscStatus);
  end;
end;

procedure TBZBaseSoundSource.SetOutsideConeAngle(const val: Single);
begin
  if val <> FOutsideConeAngle then
  begin
    FOutsideConeAngle := Clamp(val, 0, 360);
    Include(FChanges, sscStatus);
  end;
end;

procedure TBZBaseSoundSource.SetConeOutsideVolume(const val: Single);
begin
  if val <> FConeOutsideVolume then
  begin
    FConeOutsideVolume := Clamp(val, 0, 1);
    Include(FChanges, sscStatus);
  end;
end;

procedure TBZBaseSoundSource.SetPause(const val: Boolean);
begin
  if val <> FPause then
  begin
    FPause := val;
    if Collection <> nil then
      TBZSoundManager(TBZSoundSources(Collection).owner).PauseSource(Self, FPause);
  end;
end;

procedure TBZBaseSoundSource.SetPlaying(const val: Boolean);
begin
  if val <> FPlaying then
  begin
    FPlaying := val;
    if Collection <> nil then
      TBZSoundManager(TBZSoundSources(Collection).owner).PlaySource(Self, FPlaying);
  end;
end;

procedure TBZBaseSoundSource.SetNbLoops(const val: Integer);
begin
  if val <> FNbLoops then
  begin
    FNbLoops := val;
    Include(FChanges, sscSample);
  end;
end;

procedure TBZBaseSoundSource.SetPitch(const val: Single);
begin
  if val <> FPitch then
  begin
    FPitch:= val;
    Include(FChanges, sscStatus);
  end;
end;

procedure TBZBaseSoundSource.SetFrequency(const val: integer);
begin
  if val <> FFrequency then
  begin
    FFrequency := val;
    Include(FChanges, sscStatus);
  end;
end;

procedure TBZBaseSoundSource.SetMute(const val: Boolean);
begin
  if val <> FMute then
  begin
    FMute := val;
    if Collection <> nil then
      TBZSoundManager(TBZSoundSources(Collection).owner).MuteSource(Self, FMute);
  end;
end;

procedure  TBZBaseSoundSource.UpdateTimePosition(Const val : Single);
begin
  FTimePosition := val;
end;

Function TBZBaseSoundSource.GetTimePositionInByte:Int64;
var tp : Single;
begin
  if Collection <> nil then
  begin
    tp := TBZSoundManager(TBZSoundSources(Collection).owner).GetTimePosition(Self);
    result:=Round(tp*Self.Sample.Data.Frequency);
  end
  else result := 0;

end;

{%endregion%}

{%region=====[ TBZSoundSource ]===============================================}

destructor TBZSoundSource.Destroy;
begin
  //if Assigned(FBehaviourToNotify) then FBehaviourToNotify.NotifySourceDestruction(Self);
  if Collection <> nil then
    ((Collection as TBZSoundSources).Owner as TBZSoundManager).KillSource(Self);
  inherited;
end;

{%endregion%}

{%region=====[ TBZSoundSources ]==============================================}

constructor TBZSoundSources.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  inherited Create(TBZSoundSource);
end;

function TBZSoundSources.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TBZSoundSources.SetItems(index: Integer; const val: TBZSoundSource);
begin
  inherited Items[index] := val;
end;

function TBZSoundSources.GetItems(index: Integer): TBZSoundSource;
begin
  Result := TBZSoundSource(inherited Items[index]);
end;

function TBZSoundSources.Add: TBZSoundSource;
begin
  Result := TBZSoundSource.Create(Self);//(inherited Add);// as TBZSoundSource;
end;

function TBZSoundSources.FindItemID(ID: Integer): TBZSoundSource;
begin
  Result := (inherited FindItemID(ID)) as TBZSoundSource;
end;

{%endregion%}

{%region=====[ TBZSoundManager ]==============================================}

constructor TBZSoundManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSources := TBZSoundSources.Create(Self);
  FMasterVolume := 1.0;
  FOutputFrequency := 44100;
  FMaxChannels := 8;
  FUpdateFrequency := 10;
  FLastUpdateTime := -1e30;
  FDistanceFactor := 1.0;
  FRollOffFactor := 1.0;
  FDopplerFactor := 1.0;
  FSoundEnvironment := seGeneric;

end;

destructor TBZSoundManager.Destroy;
begin
  Active := False;
 // Listener := nil;
  FSources.Free;
  inherited Destroy;
end;

procedure TBZSoundManager.Notification(AComponent: TComponent; Operation:TOperation);
begin
  if Operation = opRemove then
  begin
   // if AComponent = FListener then Listener := nil;
    if AComponent = FCadencer then Cadencer := nil;
  end;
  inherited Notification(AComponent, Operation);
end;

procedure TBZSoundManager.SetActive(const val: Boolean);
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then FActive := val
  else if val <> FActive then
  begin
    if val then
    begin
      if Assigned(vActiveSoundManager) then vActiveSoundManager.Active := False;
      if DoActivate then
      begin
        FActive := True;
        vActiveSoundManager := Self;
      end;
    end
    else
    begin
      try
        StopAllSources;
        DoDeActivate;
      finally
        FActive := val;
        vActiveSoundManager := nil;
      end;
    end;
  end;
end;

function TBZSoundManager.DoActivate: Boolean;
begin
  Result := True;
end;

procedure TBZSoundManager.DoDeActivate;
begin
  StopAllSources;
end;

procedure TBZSoundManager.SetMute(const val: Boolean);
begin
  if val <> FMute then
  begin
    if val then
    begin
      if DoMute then FMute := True
    end
    else
    begin
      DoUnMute;
      FMute := False;
    end;
  end;
end;

function TBZSoundManager.DoMute: Boolean;
var
  i: Integer;
begin
  for i := 0 to Sources.Count - 1 do
    if not Sources[i].Mute then MuteSource(Sources[i], True);
  Result := True;
end;

procedure TBZSoundManager.DoUnMute;
var
  i: Integer;
begin
  for i := 0 to Sources.Count - 1 do
    if not Sources[i].Mute then MuteSource(Sources[i], False);
end;

procedure TBZSoundManager.DoPlay;
var
  i: Integer;
begin
  for i := 0 to Sources.Count - 1 do
    if not Sources[i].Playing then PlaySource(Sources[i], True);
end;

procedure TBZSoundManager.DoStop;
var
  i: Integer;
begin
  for i := 0 to Sources.Count - 1 do
    if not Sources[i].Playing then PlaySource(Sources[i], False);
end;

procedure TBZSoundManager.SetPlaying(const val: Boolean);
begin
  if val <> FPlaying then
  begin
    if val then
    begin
      DoPlay;
      FPlaying := True;
      FPause := False;
      //SetPause(False);
    end
    else
    begin
      DoStop;
      FPlaying := False;
      FPause := True;
    end;
  end;
end;

procedure TBZSoundManager.SetPause(const val: Boolean);
begin
  if val <> FPause then
  begin
    if val then
    begin
      if DoPause then FPause := True
    end
    else
    begin
      DoUnPause;
      FPause := False;
    end;
  end;
end;

procedure TBZSoundManager.SetUseEnvironment(const val: Boolean);
begin
  if FUseEnvironment = val then exit;
  FUseEnvironment := Val;
  //Include(FChange, smcEnv);
End;

procedure TBZSoundManager.Loaded;
begin
  inherited;
  if Active and (not (csDesigning in ComponentState)) then
  begin
    FActive := False;
    Active := True;
  end;
end;

procedure TBZSoundManager.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Doppler', @ReadDoppler, @WriteDoppler, (DopplerFactor <>1));
end;

procedure TBZSoundManager.WriteDoppler(writer: TWriter);
begin
  writer.WriteFloat(DopplerFactor);
end;

procedure TBZSoundManager.ReadDoppler(reader: TReader);
begin
  FDopplerFactor := reader.ReadFloat;
end;

function TBZSoundManager.DoPause: Boolean;
var
  i: Integer;
begin
  for i := 0 to Sources.Count - 1 do
    if not Sources[i].Pause then PauseSource(Sources[i], True);
  Result := True;
end;

procedure TBZSoundManager.DoUnPause;
var
  i: Integer;
begin
  for i := 0 to Sources.Count - 1 do
    if not Sources[i].Pause then PauseSource(Sources[i], False);
end;

procedure TBZSoundManager.SetMasterVolume(const val: Single);
begin
  if val < 0 then FMasterVolume := 0
  else if val > 1 then FMasterVolume := 1
  else FMasterVolume := val;
  NotifyMasterVolumeChange;
end;

procedure TBZSoundManager.SetMaxChannels(const val: Integer);
begin
  if val <> FMaxChannels then
  begin
    if val < 1 then
      FMaxChannels := 1
    else
      FMaxChannels := val;
  end;
end;

procedure TBZSoundManager.SetOutputFrequency(const val: Integer);
begin
  if val <> FOutputFrequency then
  begin
    if val < 11025 then
      FOutputFrequency := 11025
    else
      FOutputFrequency := val;
  end;
end;

procedure TBZSoundManager.SetUpdateFrequency(const val: Single);
begin
  FUpdateFrequency := Clamp(val, 1, 60);
end;

function TBZSoundManager.StoreUpdateFrequency: Boolean;
begin
  Result := (FUpdateFrequency <> 10);
end;

procedure TBZSoundManager.SetCadencer(const val: TBZCadencer);
begin
  if val <> FCadencer then
  begin
    if Assigned(FCadencer) then FCadencer.UnSubscribe(Self);
    FCadencer := val;
    if Assigned(FCadencer) then FCadencer.Subscribe(Self);
  end;
end;

procedure TBZSoundManager.SetDistanceFactor(const val: Single);
begin
  if val <= 0 then FDistanceFactor := 1
  else FDistanceFactor := val;
  Notify3DFactorsChanged;
end;

function TBZSoundManager.StoreDistanceFactor: Boolean;
begin
  Result := (FDistanceFactor <> 1);
end;

procedure TBZSoundManager.SetRollOffFactor(const val: Single);
begin
  if val <= 0 then FRollOffFactor := 1
  else FRollOffFactor := val;
  Notify3DFactorsChanged;
end;

function TBZSoundManager.StoreRollOffFactor: Boolean;
begin
  Result := (FRollOffFactor <> 1);
end;

procedure TBZSoundManager.SetDopplerFactor(const val: Single);
begin
  if val < 0 then FDopplerFactor := 0
  else if val > 10 then FDopplerFactor := 10
  else FDopplerFactor := val;
  Notify3DFactorsChanged;
end;

procedure TBZSoundManager.SetSoundEnvironment(const val: TBZSoundReverbEnvironment);
begin
  if val <> FSoundEnvironment then
  begin
    FSoundEnvironment := val;
    NotifyEnvironmentChanged;
  end;
end;

procedure TBZSoundManager.ListenerCoordinates(var position, velocity, direction, up: TBZVector);
var
  right: TBZVector;
begin
 (* if Listener <> nil then
  begin
    position := Listener.AbsolutePosition;
    if FLastDeltaTime <> 0 then
    begin
      velocity := VectorSubtract(position, FLastListenerPosition);
      ScaleVector(velocity, 1 / FLastDeltaTime);
    end;
    FLastListenerPosition := position;
    if (Listener is TBZCamera) and (TBZCamera(Listener).TargetObject <> nil)
      then
    begin
      // special case of the camera targeting something
      direction := TBZCamera(Listener).AbsoluteVectorToTarget;
      NormalizeVector(direction);
      up := Listener.AbsoluteYVector;
      right := VectorCrossProduct(direction, up);
      up := VectorCrossProduct(right, direction);
    end
    else
    begin
      direction := Listener.AbsoluteZVector;
      up := Listener.AbsoluteYVector;
    end;
  end
  else *)
  begin
    position := NullHmgVector; //NullHmgPoint;
    velocity := NullHmgVector;
    direction := ZHmgVector;
    up := YHmgVector;
  end;
end;

procedure TBZSoundManager.NotifyMasterVolumeChange;
begin
  // nothing
end;

procedure TBZSoundManager.Notify3DFactorsChanged;
begin
  // nothing
end;

procedure TBZSoundManager.NotifyEnvironmentChanged;
begin
  // nothing
end;

(* procedure TBZSoundManager.SetListener(const val: TBZBaseSceneObject);
begin
  if Assigned(FListener) then FListener.RemoveFreeNotification(Self);
  FListener := val;
  if Assigned(FListener) then FListener.FreeNotification(Self);
end; *)

procedure TBZSoundManager.SetSources(const val: TBZSoundSources);
begin
  FSources.Assign(val);
end;

procedure TBZSoundManager.KillSource(aSource: TBZBaseSoundSource);
begin
  // nothing
end;

procedure TBZSoundManager.UpdateSource(aSource: TBZBaseSoundSource);
begin
  aSource.FChanges := [];
end;

procedure TBZSoundManager.MuteSource(aSource: TBZBaseSoundSource; muted: Boolean);
begin
  // nothing
end;

procedure TBZSoundManager.PauseSource(aSource: TBZBaseSoundSource; paused: Boolean);
begin
  // nothing
end;

procedure TBZSoundManager.PlaySource(aSource : TBZBaseSoundSource; playing : Boolean);
begin
  // nothing
end;

function TBZSoundManager.GetDefaultFrequency(aSource : TBZBaseSoundSource) : Integer;
begin
  Result := 44100;
end;

function TBZSoundManager.GetTimePosition(aSource : TBZBaseSoundSource): Single;
begin
  result:=0.0;
end;

procedure TBZSoundManager.UpdateSources;
var
  i: Integer;
begin
  for i := Sources.Count - 1 downto 0 do
    UpdateSource(Sources[i]);
end;

procedure TBZSoundManager.StopAllSources;
var
  i: Integer;
begin
  for i := Sources.Count - 1 downto 0 do Sources.Delete(i);
end;

procedure TBZSoundManager.DoProgress(const progressTime: TBZProgressTimes);
begin
  if not Active then Exit;
  with progressTime do
    if newTime - FLastUpdateTime > 1 / FUpdateFrequency then
    begin
      FLastDeltaTime := newTime - FLastUpdateTime;
      FLastUpdateTime := newTime;
      UpdateSources;
    end;
end;

function TBZSoundManager.CPUUsagePercent: Single;
begin
  Result := -1;
end;

function TBZSoundManager.EAXSupported: Boolean;
begin
  Result := False;
end;

function TBZSoundManager.GetInformations : String;
begin
  Result := 'No Informations';
end;

{%endregion%}

(* constructor TBZBSoundEmitter.Create(aOwner: TBZXCollection);
begin
  inherited Create(aOwner);
  FSource := TBZSoundSource.Create(nil);
end;

destructor TBZBSoundEmitter.Destroy;
begin
  if Assigned(FPlayingSource) then FPlayingSource.Free;
  FSource.Free;
  inherited Destroy;
end;

procedure TBZBSoundEmitter.Assign(Source: TPersistent);
begin
  if Source is TBZBSoundEmitter then
  begin
    FSource.Assign(TBZBSoundEmitter(Source).FSource);
  end;
  inherited Assign(Source);
end;

procedure TBZBSoundEmitter.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FSource.WriteToFiler(writer);
    WriteBoolean(FPlaying);
  end;
end;

procedure TBZBSoundEmitter.ReadFromFiler(reader: TReader);
begin
  inherited;
  with AReader do
  begin
    ReadInteger; // ignore archiveVersion
    FSource.ReadFromFiler(reader);
    FPlaying := ReadBoolean;
  end;
end;

procedure TBZBSoundEmitter.Loaded;
begin
  inherited;
  if not (csDesigning in OwnerBaseSceneObject.ComponentState) then SetPlaying(FPlaying);
end;

class function TBZBSoundEmitter.FriendlyName: string;
begin
  Result := 'Sound Emitter';
end;

class function TBZBSoundEmitter.FriendlyDescription: string;
begin
  Result := 'A simple sound emitter behaviour';
end;

class function TBZBSoundEmitter.UniqueItem: Boolean;
begin
  Result := False;
end;

procedure TBZBSoundEmitter.DoProgress(const progressTime: TProgressTimes);
begin
  // nothing, yet
end;

procedure TBZBSoundEmitter.SetSource(const val: TBZBaseSoundSource);
begin
  FSource.Assign(val);
end;

procedure TBZBSoundEmitter.SetPlaying(const val: Boolean);
begin
  if FPlaying = val then exit;
  if csDesigning in OwnerBaseSceneObject.ComponentState then FPlaying := val
  else
  if ActiveSoundManager <> nil then
  begin
    if val <> Playing then
    begin
      if val then
      begin
        FPlayingSource := ActiveSoundManager.Sources.Add;
        FPlayingSource.FBehaviourToNotify := Self;
        FPlayingSource.Assign(FSource);
        FPlayingSource.Origin := OwnerBaseSceneObject;
      end
      else
        FPlayingSource.Free;
    end;
  end
 // else if vVerboseGLSMErrors then
 //   InformationDlg('No Active Sound Manager.'#13#10'Make sure manager is created before emitter');
end;

function TBZBSoundEmitter.GetPlaying: Boolean;
begin
  if csDesigning in OwnerBaseSceneObject.ComponentState then
    Result := FPlaying
  else
    Result := Assigned(FPlayingSource);
end;

procedure TBZBSoundEmitter.NotifySourceDestruction(aSource: TBZSoundSource);
begin
  Assert(FPlayingSource = aSource);
  FPlayingSource := nil;
end;  *)

initialization
  // class registrations
  RegisterClasses([TBZSoundLibrary, TBZSoundFXLibrary]);

  RegisterXCollectionItemClass(TBZLowPassSoundFilter);
  RegisterXCollectionItemClass(TBZHighPassSoundFilter);
  RegisterXCollectionItemClass(TBZBandPassSoundFilter);
  RegisterXCollectionItemClass(TBZReverbSoundEffect);
  RegisterXCollectionItemClass(TBZEAXReverbSoundEffect);
  RegisterXCollectionItemClass(TBZChorusSoundEffect);
  RegisterXCollectionItemClass(TBZDistortionSoundEffect);
  RegisterXCollectionItemClass(TBZEchoSoundEffect);
  RegisterXCollectionItemClass(TBZFlangerSoundEffect);
  RegisterXCollectionItemClass(TBZCompressorSoundEffect);
  RegisterXCollectionItemClass(TBZEqualizerSoundEffect);

  //RegisterXCollectionItemClass(TBZBSoundEmitter);
  vSoundLibraries := TList.Create;
  vSoundFXLibraries := TList.Create;


finalization

  if Assigned(vActiveSoundManager) then vActiveSoundManager.Active := False;

  vSoundLibraries.Free;
  vSoundLibraries := nil;

  vSoundFXLibraries.Free;
  vSoundFXLibraries := nil;

  UnRegisterXCollectionItemClass(TBZLowPassSoundFilter);
  UnRegisterXCollectionItemClass(TBZHighPassSoundFilter);
  UnRegisterXCollectionItemClass(TBZBandPassSoundFilter);
  UnRegisterXCollectionItemClass(TBZReverbSoundEffect);
  UnRegisterXCollectionItemClass(TBZEAXReverbSoundEffect);
  UnRegisterXCollectionItemClass(TBZChorusSoundEffect);
  UnRegisterXCollectionItemClass(TBZDistortionSoundEffect);
  UnRegisterXCollectionItemClass(TBZEchoSoundEffect);
  UnRegisterXCollectionItemClass(TBZFlangerSoundEffect);
  UnRegisterXCollectionItemClass(TBZCompressorSoundEffect);
  UnRegisterXCollectionItemClass(TBZEqualizerSoundEffect);

  //UnregisterXCollectionItemClass(TBZBSoundEmitter);

end.

