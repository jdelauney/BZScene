(*
  @abstract(Contient le "Manager de son" pour la librairie OpenAL)

  Les pilotes officiels d' OpenAL peuvent être téléchargés à partir du site OpenAL @br
  (http://www.openal.org). ou de votre carte son site Web du fabricant. @br
  Ou pouvez télécharger la version d'OpenAL Soft (http://www.openal-soft.org/) @br
  Cette version à l'avantage d'être open source et est disponible pour toutes les plateformes @br
  et supporte les environnements 32bits et 64bits. C'est cette version qui est utilisée @br
  afin de conserver la compatibilité partout.@br

  Pourquoi avoir choisi d'utiliser la version OpenAL Soft plutot l'officiel ?
  Les différences entre ces 2 librairies sont : @br
  Pour la version officielle 32bits : @br
  @unorderedList(
    @item (Support EAX 2.0,3.0,4.0,5.0)
    @item (Effets supportés : Reverb, EAX Reverb, Chorus, Echo, Distorion, Frequency Shifter, @br
                              vocal morpher, Pitch Shifter, Ring Modulator, compressor, equalizer)
    @item (Filtres supportés : Low Pass)
  )

  Pour la version officielle 64bits : @br
  @unorderedList(
    @item (Support EAX 2.0,3.0,4.0,5.0)
    @item (Effets supportés : Reverb, EAX Reverb)
    @item (Filtres supportés : Low Pass)
  )

  Pour la version non-officielle (Soft OpenAL) 32bits et 64bits : @br
  @unorderedList(
    @item (Support EAX 2.0)
    @item (Effets supportés : Reverb, EAX Reverb, Chorus, Echo, Distorion,@br
                              Ring Modulator, compressor, equalizer)
    @item (Filtres supportés : Low Pass, High Pass, Band Pass)
  )

  Voilà c'est juste à cause des effets non supportés par la version 64bits qui à fait basculer la balance

  Actuellement OpenAL sound manager supporte : @br
  @unorderedList(
  @item (Le positionnement des sons en 3D)
  @item (Les filtres :
    @unorderedList(
     @item (Low pass)
     @item (Hi pass)
     @item (Band pass)
    ))
  @item (Les effets :
    @unorderedList(
     @item (Reverb)
     @item (EAX Reverb)
     @item (Chorus)
     @item (Echo)
     @item (Distorsion)
     @item (Flanger)
     @item (Ring Tone)
     @item (Equalizer)
    ))
  @item (Nombre d'effets maximum par source : 4 + 1 filtre direct)
  @item (Effet Doppler)
  @item (Le changement du volume principal)
  @item (Le changement du volume d'une source)
  @item (Le changement de pitch d'une source)
  @item (Le support de sons multi-cannaux (1 = Mono, 2 = Strereo, 3 et 4 = Quadriphonie, 6 = 5.1, 7 = 6.1 et 8 = 7.1 ))
  @item (Le support des environnement 3D virtuels via les effets REVERB et EAX_REVERB )
  )

  -------------------------------------------------------------------------------------------------------------

  @created(2016-11-16)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(05/11/2017 : Creation  )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) : @br
  Pour l'implementation des filtres et effets voir unité BZSound.pas@br

  Malheureusement avec les 2 librairies (officielles et soft) ont ne peux pas utiliser@br
  les effets d'environnements virtuel 3D, car les fonctions "eaxSet" et "eaxGet"@br
  ne sont pas disponiblent. Il nous faut donc les émuler via les effets "Reverb" EFX ou EAX. De plus ces fonctions ne fonctionnent que si le mode@br
  "Hardware" est activé, ce qui est impossible sous Windows (du à sont fonctionnement :( )@br
  Je  n'ai pas testé sous d'autres OS comme linux, donc à voir.@br
  Cependant sous Windows (non testé avec d'autres OS) les fonctions "EAXSet" et "EAXget" @br
  sont disponiblent uniquement avec la version officielle d'OpenAL, mais il faut utiliser @br
  le fichier "wrap_oal.dll" à la place de "OpenAL32.dll" mais idem cela ne change rien du fait @br
  que le mode Hardware n'est pas disponible.@br

  La solution est donc d'utiliser les effets REVERB et EAX_REVERB pour simuler les
  environnements.

  Pour infos l'installateur de la version officiel d'OpenAL pour Windows sur des sytemes @br
  64 bits place les Librairie DLL 32 bits dans le dossier c:\Windows\sysWOW64
  et les librairies 64 bits dans le dossier c:\Windows\system32. @br
  @bold(Attention) : leurs noms ne changent pas et reste "OpenAL32.dll" et "wrap_oal.dll"

  -------------------------------------------------------------------------------------------------------------

  @bold(Dépendances) :

  -------------------------------------------------------------------------------------------------------------

  Credits :
   @unorderedList(
     @item (Codé sur une base de GLScene http://www.sourceforge.net/glscene)
   )

  -------------------------------------------------------------------------------------------------------------

  @bold(Licence) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)

unit BZOpenALManager;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils,
  BZLibOpenAL, BZSound, BZSoundSample;

Const
  OAL_MIN_PITCH : Single = 0.1;
  OAL_MAX_PITCH : Single = 4.0;

type
  { Exception levée par TBZSoundOpenALManager }
  EBZSoundOpenALManager = class(EBZSoundException);

  { Gestionnaire audio pour OpenAL }
  TBZSoundOpenALManager = class (TBZSoundManager)
  private
    FActivated : Boolean;
    FMajorVersion, FMinorVersion: Integer;
    FChanges : TBZSoundManagerChanges;
  protected
    function DoActivate : Boolean; override;
    procedure DoDeActivate; override;
    procedure NotifyMasterVolumeChange; override;
    procedure Notify3DFactorsChanged; override;
    procedure NotifyEnvironmentChanged; override;

    procedure InitSoundSource(aSource : TBZBaseSoundSource);
    procedure InitSourceAuxSlots(aSource : TBZBaseSoundSource);
    procedure InitSoundEffect(anEffect : TBZCustomSoundFXItem);
    procedure InitSoundFilter(aFilter : TBZCustomSoundFXItem);
    //procedure InitSoundDirectFilter(aFilter : TBZCustomSoundFXItem);

    procedure UpdateSoundSourceParams;
    procedure UpdateSoundFilterParams(aFilter : TBZCustomSoundFXItem);
    procedure UpdateSoundEffectParams(anEffect : TBZCustomSoundFXItem);

    //Procedure UpdateListener;
    //Procedure UpdateEmitter;

    procedure LinkEffectToSource(aSource : TBZBaseSoundSource;aSlot : Integer);
    procedure UnLinkEffectFromSource(aSource : TBZBaseSoundSource;aSlot : Integer);
    procedure LinkFilterToEffect(aSource : TBZBaseSoundSource;aSlot : Integer);
    procedure UnLinkFilterToEffect(aSource : TBZBaseSoundSource;aSlot : Integer);
    procedure LinkDirectFilterToSource(aSource : TBZBaseSoundSource);
    procedure UnLinkDirectFilterFromSource(aSource : TBZBaseSoundSource);

    procedure KillSource(aSource : TBZBaseSoundSource); override;
    procedure UpdateSource(aSource : TBZBaseSoundSource); override;
    procedure MuteSource(aSource : TBZBaseSoundSource; muted : Boolean); override;
    procedure PauseSource(aSource : TBZBaseSoundSource; paused : Boolean); override;
    procedure PlaySource(aSource : TBZBaseSoundSource; playing : Boolean); override;

    function GetDefaultFrequency(aSource : TBZBaseSoundSource) : Integer;
    function GetALFormat(sampling : TBZSoundSample) : Integer;

    function GetTimePosition(aSource : TBZBaseSoundSource): Single; override;

    function EffectIsSupported(FXId : TALInt):Boolean;
    function FilterIsSupported(FilterID : TALInt):Boolean;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    { Mise à jour des sources audio}
    procedure UpdateSources; override;

    { Retourne @True si EAX est supporté }
    function EAXSupported : Boolean; override;
    { Retourne @True si la totalité des effets EAX sont supporté }
    function FullEAXSupported : Boolean;
    { Retourne @True si EFX est supporté }
    function EFXSupported : Boolean;
    { Retourne @True si XRam est supporté }
    function XRamSupported : Boolean;
    { Retourne la taille en octet de XRam }
    function getXRamSize : Int64;
    { Retourne la taille libre en octet de XRam }
    function getXRamFree : Int64;

    { Retourne @TRUE si le format audio 4 canaux (surround) est supporté }
    function OutputFormat4ChannelsIsSupported : Boolean;
    { Retourne @TRUE si le format audio 5.1 canaux est supporté }
    function OutputFormat51ChannelsIsSupported : Boolean;
    { Retourne @TRUE si le format audio 6.1 canaux est supporté }
    function OutputFormat61ChannelsIsSupported : Boolean;
    { Retourne @TRUE si le format audio 7.1 canaux est supporté }
    function OutputFormat71ChannelsIsSupported : Boolean;

    { Retourne le nombre maximal d'effets applicables par source }
    function getMaxEffectsPerSource : Integer;
    { Retourne les informations sur OpenAL sous forme de chaine de caractères }
    function getInformations : String;
    { Retourne les changements d'état lors de la lecture d'une source audio }
    property Changes : TBZSoundManagerChanges read FChanges;
  end;

  { Exception levé par OpenAL }
  EOpenALError = Exception;

//procedure Register;


implementation


uses
  //Forms, Dialogs,
  //GLVectorGeometry,
  Math,StrUtils,
  BZUtils, BZMath, BZVectorMath,
  BZLogger, Dialogs{al, alut, alTypes};

var
  error : TALenum;

//procedure Register;
//begin
//   RegisterComponents('GLScene', [TBZSoundOpenALManager]);
//end;

//checks for an error and raises an exception if necessary
procedure CheckOpenALError;
var
   error : TALenum;
begin
   error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
end;

//clears the error-states
procedure ClearOpenALError;
begin
   alGetError;
end;

constructor TBZSoundOpenALManager.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FMinorVersion := 0;
  FMajorVersion := 0;
  FChanges := [];
end;

destructor TBZSoundOpenALManager.Destroy;
begin
  inherited Destroy;
end;

function TBZSoundOpenALManager.DoActivate : Boolean;
var
  dummy: array of PALbyte;
procedure ParseVersion(const Version: string; out Major, Minor: Integer);
var
  DotP, SpaceP: Integer;
begin
  { version unknown }
  Major := 0;
  Minor := 0;

  DotP := Pos('.', Version);
  if DotP <> 0 then
  try
    Major := StrToInt(Trim(Copy(Version, 1, DotP - 1)));
    SpaceP := PosEx(' ', Version, DotP + 1);
    if SpaceP <> 0 then
      Minor := StrToInt(Trim(Copy(Version, DotP + 1, SpaceP - DotP))) else
      Minor := StrToInt(Trim( Copy(Version, DotP + 1, MaxInt)));
  except
    on EConvertError do
    begin
      Major := 0;
      Minor := 0;
    end;
  end;
end;

begin
  Result:=false;
  GlobalLogger.LogNotice('Activate OpenAL Sound Manager');
  // Setup OpenAL
  if not InitOpenAL() then
  begin
    Raise EOpenALError.Create('OpenAL non initialiser');
    Exit;
  End;
  dummy:= nil;
  alutInit(nil, dummy);
  error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
  alDistanceModel(AL_INVERSE_DISTANCE);
  error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
  ReadOpenALExtensions();

  // Set any global states
  FActivated:=true;
  NotifyMasterVolumeChange;
  Notify3DFactorsChanged;
//  if (Environment<>seDefault) then NotifyEnvironmentChanged;
  ParseVersion(alGetString(AL_VERSION), FMajorVersion, FMinorVersion);
  Result:=True;
end;

procedure TBZSoundOpenALManager.DoDeActivate;
var
  i:integer;
begin
  FActivated:=false;
  for i := 0 to Sources.Count - 1 do
  begin
    Sources[i].Sample.ManagerTag := 0;
  end;
  alutExit;
end;

procedure TBZSoundOpenALManager.NotifyMasterVolumeChange;
begin
  if FActivated then
  begin
    alListenerf(AL_GAIN,MasterVolume);
  end;
end;

procedure TBZSoundOpenALManager.Notify3DFactorsChanged;
begin
  if FActivated then
  begin
    alDopplerFactor(DopplerFactor);
  end;
end;

procedure TBZSoundOpenALManager.NotifyEnvironmentChanged;
var
  Env: TAluInt;
begin
  if FActivated then
  begin
    // check extension is available + update
    if FullEAXSupported then //And UseEAX
    begin
      Case Environment of
        seGeneric : Env := EAX_ENVIRONMENT_GENERIC;
        sePaddedCell : Env := EAX_ENVIRONMENT_PADDEDCELL;
        seRoom : Env := EAX_ENVIRONMENT_ROOM;
        seBathroom : Env := EAX_ENVIRONMENT_BATHROOM;
        seLivingRoom : Env := EAX_ENVIRONMENT_LIVINGROOM;
        seStoneroom : Env := EAX_ENVIRONMENT_STONEROOM;
        seAuditorium : Env := EAX_ENVIRONMENT_AUDITORIUM;
        seConcertHall : Env := EAX_ENVIRONMENT_CONCERTHALL;
        seCave : Env := EAX_ENVIRONMENT_CAVE;
        seArena : Env := EAX_ENVIRONMENT_ARENA;
        seHangar : Env := EAX_ENVIRONMENT_HANGAR;
        seCarpetedHallway : Env := EAX_ENVIRONMENT_CARPETEDHALLWAY;
        seHallway : Env := EAX_ENVIRONMENT_HALLWAY;
        seStoneCorridor : Env := EAX_ENVIRONMENT_STONECORRIDOR;
        seAlley : Env := EAX_ENVIRONMENT_ALLEY;
        seForest : Env := EAX_ENVIRONMENT_FOREST;
        seCity : Env := EAX_ENVIRONMENT_CITY;
        seMountains : Env := EAX_ENVIRONMENT_MOUNTAINS;
        seQuarry : Env := EAX_ENVIRONMENT_QUARRY;
        sePlain : Env := EAX_ENVIRONMENT_PLAIN;
        seParkingLot : Env := EAX_ENVIRONMENT_PARKINGLOT;
        seSewerPipe : Env := EAX_ENVIRONMENT_SEWERPIPE;
        seUnderWater : Env := EAX_ENVIRONMENT_UNDERWATER;
        seDrugged : Env := EAX_ENVIRONMENT_DRUGGED ;
        seDizzy : Env := EAX_ENVIRONMENT_DIZZY;
        sePsychotic : Env := EAX_ENVIRONMENT_PSYCHOTIC;
      End;
      if pointer(EAXSet)<>nil then
      begin
        eaxSet(DSPROPSETID_EAX20_ListenerProperties,
               DSPROPERTY_EAXLISTENER_ENVIRONMENT or
               DSPROPERTY_EAXLISTENER_DEFERRED,
               0, @Env, sizeof(TALuint));
        error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
        Include(FChanges, smcEnvironnment);
      end;
    end
    else  Include(FChanges, smcEnvironnment);
    // if USeEAX then elese
  end;
end;

function TBZSoundOpenALManager.EffectIsSupported(FXId : TALInt):Boolean;
var
  error : TALenum;
  FxTest : PtrUInt;
begin
  Result := false;
  FXTest := 0;
  alGenEffects(1, @FxTest);
  error := alGetError();
  if Error =  AL_NO_ERROR then
  begin
    alEffecti(FxTest, AL_EFFECT_TYPE, FXId);
    Error := alGetError();
    if Error =  AL_NO_ERROR then result := true;
  End;
  alDeleteEffects(1, @FxTest);
end;

function TBZSoundOpenALManager.FilterIsSupported(FilterID : TALInt):Boolean;
var
  error : TALenum;
  FilterTest : PtrUInt;
begin
  Result := false;
  FilterTest := 0;
  alGenFilters(1, @FilterTest);
  error := alGetError();
  if Error =  AL_NO_ERROR then
  begin
    alFilteri(FilterTest, AL_FILTER_TYPE, FilterID);
    Error := alGetError();
    if Error =  AL_NO_ERROR then result := true;
  End;
  alDeleteFilters(1, @FilterTest);
end;

function TBZSoundOpenALManager.GetInformations : String;
var
  s:string;
 // FxTest : TALUInt;
 // FilterTest : TALUInt;
  error : TALenum;
  //devtmp,
  devicelist, defaultdevice: String;
  (*devices: TStringList;
  loop : integer;
  pDevice : TALCdevice;
  pContext : TALCcontext;
  i : Integer; *)
  efxmajor, efxMinor, maxaux : Integer;



begin

{  you can query the maximum source by :

ALCint nummono, numstereo;
alcGetIntegerv(device, ALC_MONO_SOURCES, 1, &nummono);
alcGetIntegerv(device, ALC_STEREO_SOURCES, 1, &numstereo); }
  Result :='';
  S := 'Devices : '+#13+#10;
  //enumerate devices
  defaultDevice := '';
  deviceList := '';

 if alcIsExtensionPresent(nil,'ALC_ENUMERATE_ALL_EXT') then//'ALC_ENUMERATION_EXT') = TRUE then
  begin
    defaultDevice := alcGetString(nil, ALC_DEFAULT_DEVICE_SPECIFIER);
    deviceList := alcGetString(nil, ALC_DEVICE_SPECIFIER);
    S:=S+'Device Liste  : '+DeviceList+#13+#10;
    S:=S+'Default Device: '+defaultDevice+#13+#10;
  end;

  S:=S+'--------------------------------------------------------------------'+#13+#10+
  'Vendor     : '+alGetString(AL_VENDOR) +#13+#10+
  'Renderer   : '+alGetString(AL_RENDERER)+#13+#10+
  'Version    : '+alGetString(AL_VERSION)+' - major: '+InttoStr(FMajorVersion)+', minor: ' +InttoStr(FMinorVersion)+#13+#10+
  'Extensions : '+alGetString(AL_EXTENSIONS) +#13+#10+
  '--------------------------------------------------------------------'+#13+#10+
  'OpenAL Sources alloués : '+IntToStr(Sources.Count) +#13+#10+
  '--------------------------------------------------------------------'+#13+#10;
  if EAXSupported then
  begin
    S := S +'EAX   : True' +#13+#10;
    if XRamSupported then
    begin
      S := S +
      'X-RAM : True'+#13+#10+
      'RAM Size = '+InttoStr(getXRamSize)+' Mo'+#13+#10+
      'RAM Free = '+InttoStr(getXRamFree)+' Mo'+#13+#10;
    end
    else
    begin
      S := S +'X-RAM : False' +#13+#10;
    end;
  end
  else
  begin
    S := S +'EAX   : False' +#13+#10;
  end;
  if EFXSupported then
  begin
      alcGetIntegerv(alcGetContextsDevice(alcGetCurrentContext()),ALC_EFX_MAJOR_VERSION,Sizeof(integer),@efxMajor);
      alcGetIntegerv(alcGetContextsDevice(alcGetCurrentContext()),ALC_EFX_MINOR_VERSION,Sizeof(integer),@efxMinor);
      alcGetIntegerv(alcGetContextsDevice(alcGetCurrentContext()),ALC_MAX_AUXILIARY_SENDS,Sizeof(integer),@MaxAux);
      S := S +'EFX   : True - Max Effets par source : '+Inttostr(maxaux)+#13+#10; //GetMaxEffectsPerSource) +#13+#10;
      S := S+'Version : '+InttoStr(efxMajor)+'.'+InttoStr(efxMinor)+#13+#10;
   // if Error =  AL_NO_ERROR then
   // begin
      S:=S+#13+#10+'  Effets supportés : '+#13+#10;
      S:=S+'  - Reverb            : '+BoolToStr(EffectIsSupported(AL_EFFECT_REVERB),true)+#13+#10;
      S:=S+'  - EAX Reverb        : '+BoolToStr(EffectIsSupported(AL_EFFECT_EAXREVERB),true)+#13+#10;
      S:=S+'  - Chorus            : '+BoolToStr(EffectIsSupported(AL_EFFECT_CHORUS),true)+#13+#10;
      S:=S+'  - Distorsion        : '+BoolToStr(EffectIsSupported(AL_EFFECT_DISTORTION),true)+#13+#10;
      S:=S+'  - Echo              : '+BoolToStr(EffectIsSupported(AL_EFFECT_ECHO),true)+#13+#10;
      S:=S+'  - Flanger           : '+BoolToStr(EffectIsSupported(AL_EFFECT_FLANGER),true)+#13+#10;
      S:=S+'  - Frequency Shifter : '+BoolToStr(EffectIsSupported(AL_EFFECT_FREQUENCY_SHIFTER),true)+#13+#10;
      S:=S+'  - Vocal Morpher     : '+BoolToStr(EffectIsSupported(AL_EFFECT_VOCAL_MORPHER),true)+#13+#10;
      S:=S+'  - Pitch Shifter     : '+BoolToStr(EffectIsSupported(AL_EFFECT_PITCH_SHIFTER),true)+#13+#10;
      S:=S+'  - Ring Modulator    : '+BoolToStr(EffectIsSupported(AL_EFFECT_RING_MODULATOR),true)+#13+#10;
      S:=S+'  - Autowah           : '+BoolToStr(EffectIsSupported(AL_EFFECT_AUTOWAH),true)+#13+#10;
      S:=S+'  - Compressor        : '+BoolToStr(EffectIsSupported(AL_EFFECT_COMPRESSOR),true)+#13+#10;
      S:=S+'  - Equalizer         : '+BoolToStr(EffectIsSupported(AL_EFFECT_EQUALIZER),true)+#13+#10;

      S:=S+#13+#10+'  Filtres supportés : '+#13+#10;

    //  if Error =  AL_NO_ERROR then
    //  begin
        S:=S+'  - Low Pass         : '+BoolToStr(FilterIsSupported(AL_FILTER_LOWPASS),true)+#13+#10;
        S:=S+'  - High Pass        : '+BoolToStr(FilterIsSupported(AL_FILTER_HIGHPASS),true)+#13+#10;
        S:=S+'  - Band Pass        : '+BoolToStr(FilterIsSupported(AL_FILTER_BANDPASS),true)+#13+#10;
   //   end
   //   else S:=S+'  - Aucun : '+#13+#10;


    //end;
   // else Raise EBZSoundOpenALManager.Create('Erreur lors du listage des effets disponibles');
  end
  else
  begin
    S := S +'EFX   : False' +#13+#10;
  end;

  S:= S+#13+#10 +'Format de sortie supportés : '+#13+#10+
  '  - 4 Canaux, 16 bit          : '+BoolToStr(OutputFormat4ChannelsIsSupported,true)+#13+#10+
  '  - 6 Canaux (5.1), 16 bit  : '+BoolToStr(OutputFormat51ChannelsIsSupported,true)+#13+#10+
  '  - 7 Canaux (6.1), 16 bit  : '+BoolToStr(OutputFormat61ChannelsIsSupported,true)+#13+#10+
  '  - 8 Canaux (7.1), 16 bit  : '+BoolToStr(OutputFormat71ChannelsIsSupported,true)+#13+#10;

  result := S;
End;

procedure TBZSoundOpenALManager.InitSoundSource(aSource : TBZBaseSoundSource);
begin
  if aSource.ManagerTag<>0 then exit;

  { Si l'échantillon n'a pas de référence à un tampon, nous devons créer un tampon,
  et charger les données de l'échantillon dans celui-ci }
  if not(Assigned(aSource)) then exit;
  if (aSource.Sample = nil) then exit;
  if Assigned(aSource.Sample.Data) then
  begin
    if aSource.sample.data.SoundDataSize<=0 then exit;

    alGenSources(1, PALuint(@aSource.managerTag));
    error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');

    alGenBuffers(1, PALuint(@aSource.sample.ManagerTag));
    error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
    // fill buffer (once buffer filled, can't fill buffer again, unless no other sources playing)
    alBufferData(aSource.sample.ManagerTag,
                   GetALFormat(aSource.sample.Data),
                   aSource.sample.Data.SoundData,
                   aSource.sample.data.SoundDataSize,
                   aSource.Sample.Data.Frequency);
    error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
  end;
End;

procedure TBZSoundOpenALManager.InitSourceAuxSlots(aSource : TBZBaseSoundSource);
var
  MaxSlot,i,j : Integer;
begin
  MaxSlot := getMaxEffectsPerSource;
  if MaxSlot <= 0 then exit;
  if aSource.AuxSlots.Count>0 then
  begin
    j:=Min(MaxSlot,aSource.AuxSlots.Count);


      For i:=0 to J-1 do
      begin
        alGenAuxiliaryEffectSlots(1,PALUInt(@aSource.AuxSlots.Items[i].ManagerTag));
        error:= alGetError();
        if error<>AL_NO_ERROR then
          raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
       //set master volume of effect slot
       alAuxiliaryEffectSlotf(aSource.AuxSlots.Items[i].ManagerTag,AL_EFFECTSLOT_GAIN, aSource.AuxSlots.Items[i].Gain);
       error:= alGetError();
       if error<>AL_NO_ERROR then
          raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
       //toggle automatic send adjustments based on locations of sources and listeners
        if aSource.AuxSlots.Items[i].AutoAdjust then
          alAuxiliaryEffectSloti(aSource.AuxSlots.Items[i].ManagerTag, AL_EFFECTSLOT_AUXILIARY_SEND_AUTO,AL_TRUE)
        else
          alAuxiliaryEffectSloti(aSource.AuxSlots.Items[i].ManagerTag, AL_EFFECTSLOT_AUXILIARY_SEND_AUTO,AL_FALSE);
        error:= alGetError();
        if error<>AL_NO_ERROR then
          raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
      end;

  End
  else if UseEnvironment then
  begin
    with aSource.AuxSlots.Add Do
    begin
      Name := 'Global Sound Manager Env';
      Activated := true;
      //SetCacheFX(FGlobalReverbFX);
    End;
    alGenAuxiliaryEffectSlots(1,PALUInt(@aSource.AuxSlots.Items[0].ManagerTag));
    error:= alGetError();
    if error<>AL_NO_ERROR then
       raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
  End;
End;

procedure TBZSoundOpenALManager.InitSoundEffect(anEffect : TBZCustomSoundFXItem);
var
  TheEffect : TBZCustomSoundEffectItem;
  TheFilter : TBZCustomSoundFilterItem;
begin
  if not(anEffect.inheritsFrom(TBZCustomSoundEffectItem)) then
  begin
    Raise EBZSoundOpenALManager.Create('L''effet sélectionné n''est pas un Effet');
    exit;
  End
  else TheEffect := TBZCustomSoundEffectItem(anEffect);
  if TheEffect.ManagerTag=0 then
  begin
    alGenEffects(1, PALUInt(@TheEffect.ManagerTag));
    error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');

    if TheEffect is TBZReverbSoundEffect  and (EffectIsSupported(AL_EFFECT_REVERB)) then
      alEffecti(TheEffect.ManagerTag,AL_EFFECT_TYPE,AL_EFFECT_REVERB)
    else if TheEffect is TBZReverbSoundEffect  and (EffectIsSupported(AL_EFFECT_EAXREVERB)) then
      alEffecti(TheEffect.ManagerTag,AL_EFFECT_TYPE,AL_EFFECT_EAXREVERB)
    else if TheEffect is TBZReverbSoundEffect  and (EffectIsSupported(AL_EFFECT_CHORUS)) then
      alEffecti(TheEffect.ManagerTag,AL_EFFECT_TYPE,AL_EFFECT_CHORUS)
    else if TheEffect is TBZReverbSoundEffect  and (EffectIsSupported(AL_EFFECT_ECHO)) then
      alEffecti(TheEffect.ManagerTag,AL_EFFECT_TYPE,AL_EFFECT_ECHO)
    else if TheEffect is TBZReverbSoundEffect  and (EffectIsSupported(AL_EFFECT_DISTORTION)) then
      alEffecti(TheEffect.ManagerTag,AL_EFFECT_TYPE,AL_EFFECT_DISTORTION)
    else if TheEffect is TBZReverbSoundEffect  and (EffectIsSupported(AL_EFFECT_FLANGER)) then
      alEffecti(TheEffect.ManagerTag,AL_EFFECT_TYPE,AL_EFFECT_FLANGER)
    else if TheEffect is TBZReverbSoundEffect  and (EffectIsSupported(AL_EFFECT_COMPRESSOR)) then
      alEffecti(TheEffect.ManagerTag,AL_EFFECT_TYPE,AL_EFFECT_COMPRESSOR)
    else if TheEffect is TBZReverbSoundEffect  and (EffectIsSupported(AL_EFFECT_EQUALIZER)) then
      alEffecti(TheEffect.ManagerTag,AL_EFFECT_TYPE,AL_EFFECT_EQUALIZER);

    error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');

    if (TBZCustomSoundEffectItem(TheEffect).FilterName<>'') then
    begin
      TheFilter := TheEffect.GetFilter;
      InitSoundFilter(TheFilter);
    end;
  end;
End;

procedure TBZSoundOpenALManager.InitSoundFilter(aFilter : TBZCustomSoundFXItem);
var TheFilter : TBZCustomSoundFilterItem;
begin
  if not(aFilter.inheritsFrom(TBZCustomSoundFilterItem)) then
  begin
    Raise EBZSoundOpenALManager.Create('L''effet sélectionné n''est pas un filtre');
    exit;
  End
  else TheFilter := TBZCustomSoundFilterItem(aFilter);
  if TheFilter.ManagerTag = 0 then
  begin
    if (TheFilter Is TBZLowPassSoundFilter) and (FilterIsSupported(AL_FILTER_LOWPASS)) then
    begin
       alGenFilters(1,PALUInt(@TheFilter.ManagerTag));
       error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
       alFilteri(TheFilter.ManagerTag, AL_FILTER_TYPE,AL_FILTER_LOWPASS);
       error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
    end
    else if (TheFilter Is TBZHighPassSoundFilter) and (FilterIsSupported(AL_FILTER_HIGHPASS)) then
    begin
      alGenFilters(1,PALUInt(@TheFilter.ManagerTag));
      error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
      alFilteri(TheFilter.ManagerTag,AL_FILTER_TYPE,AL_FILTER_HIGHPASS);
      error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
    end
    else if (aFilter Is TBZBandPassSoundFilter) and (FilterIsSupported(AL_FILTER_BANDPASS)) then
    begin
       alGenFilters(1,PALUInt(@TheFilter.ManagerTag));
       error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
       alFilteri(TheFilter.ManagerTag,AL_FILTER_TYPE,AL_FILTER_BANDPASS);
       error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
    end;
  End;
End;

procedure TBZSoundOpenALManager.UpdateSoundSourceParams;
begin

End;

procedure TBZSoundOpenALManager.UpdateSoundFilterParams(aFilter : TBZCustomSoundFXItem);
var
  TheFilter : TBZCustomSoundFilterItem;
begin
  // On ne fait pas de verification, cela a deja ete fait par InitSoundFilter
  TheFilter := TBZCustomSoundFilterItem(aFilter);

  if (TheFilter Is TBZLowPassSoundFilter) then
  begin
    With TheFilter As TBZLowPassSoundFilter do
    begin
      alFilterf(ManagerTag, AL_LOWPASS_GAIN, Gain);
      alFilterf(ManagerTag, AL_LOWPASS_GAINHF, GainHF);
    End;
    error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
  End
  else if (TheFilter Is TBZHighPassSoundFilter) then
  begin
     With TheFilter As TBZHighPassSoundFilter do
    begin
      alFilterf(ManagerTag, AL_LOWPASS_GAIN, Gain);
      alFilterf(ManagerTag, AL_LOWPASS_GAINHF, GainLF);
    End;
    error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
  End
  else if (TheFilter Is TBZBandPassSoundFilter) then
  begin
    With aFilter As TBZBandPassSoundFilter do
    begin
      alFilterf(ManagerTag, AL_LOWPASS_GAIN, Gain);
      alFilterf(ManagerTag, AL_LOWPASS_GAINHF, GainLF);
      alFilterf(ManagerTag, AL_LOWPASS_GAINHF, GainHF);
    End;
    error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
  End;
End;

procedure TBZSoundOpenALManager.UpdateSoundEffectParams(anEffect : TBZCustomSoundFXItem);
begin
  if anEffect is TBZReverbSoundEffect then
  begin
    With AnEffect as TBZReverbSoundEffect do
    begin
      alEffectf(ManagerTag, AL_REVERB_DENSITY, DENSITY);
      alEffectf(ManagerTag, AL_REVERB_DIFFUSION, DIFFUSION);
      alEffectf(ManagerTag, AL_REVERB_GAIN, GAIN);
      alEffectf(ManagerTag, AL_REVERB_GAINHF, GAINHF);
      alEffectf(ManagerTag, AL_REVERB_DECAY_TIME, DECAY_TIME);
      alEffectf(ManagerTag, AL_REVERB_DECAY_HFRATIO, DECAY_HFRATIO);
      alEffectf(ManagerTag, AL_REVERB_REFLECTIONS_GAIN, REFLECTIONS_GAIN);
      alEffectf(ManagerTag, AL_REVERB_REFLECTIONS_DELAY, REFLECTIONS_DELAY);
      alEffectf(ManagerTag, AL_REVERB_LATE_REVERB_GAIN, LATE_REVERB_GAIN);
      alEffectf(ManagerTag, AL_REVERB_LATE_REVERB_DELAY, LATE_REVERB_DELAY);
      alEffectf(ManagerTag, AL_REVERB_AIR_ABSORPTION_GAINHF, AIR_ABSORPTION_GAINHF);
      alEffectf(ManagerTag, AL_REVERB_ROOM_ROLLOFF_FACTOR, ROOM_ROLLOFF_FACTOR);
      if DECAY_HFLIMIT then alEffecti(ManagerTag, AL_REVERB_DECAY_HFLIMIT, AL_TRUE)
      else alEffecti(ManagerTag, AL_REVERB_DECAY_HFLIMIT, AL_FALSE);
      error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
    end;
  end
  else if anEffect is TBZEAXReverbSoundEffect then
  begin

  End
  else if anEffect is TBZChorusSoundEffect then
  begin

  End
  else if anEffect is TBZEchoSoundEffect then
  begin

  End
  else if anEffect is TBZDistortionSoundEffect then
  begin

  End
  else if anEffect is TBZFlangerSoundEffect then
  begin

  End
  else if anEffect is TBZCompressorSoundEffect then
  begin

  End
  else if anEffect is TBZEqualizerSoundEffect then
  begin

  End;
End;

//Procedure UpdateListener;
//Procedure UpdateEmitter;
procedure TBZSoundOpenALManager.LinkFilterToEffect(aSource : TBZBaseSoundSource;aSlot : Integer);
var
  TheFilter : TBZCustomSoundFilterItem;
begin
  TheFilter := TBZCustomSoundEffectItem(aSource.AuxSlots.Items[aSlot].CacheFX).GetFilter;
  alSource3i(aSource.ManagerTag,AL_AUXILIARY_SEND_FILTER, aSource.AuxSlots.Items[aSlot].ManagerTag,1, TheFilter.ManagerTag);
  error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
End;

procedure TBZSoundOpenALManager.UnLinkFilterToEffect(aSource : TBZBaseSoundSource;aSlot : Integer);
begin
  alSource3i(aSource.ManagerTag,AL_AUXILIARY_SEND_FILTER,aSource.AuxSlots.Items[aSlot].ManagerTag,0, AL_FILTER_NULL);
  error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
End;

procedure TBZSoundOpenALManager.LinkEffectToSource(aSource : TBZBaseSoundSource;aSlot : Integer);
begin
  alAuxiliaryEffectSloti(aSource.AuxSlots.Items[aSlot].ManagerTag,AL_EFFECTSLOT_EFFECT, aSource.AuxSlots.Items[aSlot].CacheFX.ManagerTag);
  error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
End;

procedure TBZSoundOpenALManager.UnLinkEffectFromSource(aSource : TBZBaseSoundSource;aSlot : Integer);
begin
  alSource3i(aSource.ManagerTag, AL_AUXILIARY_SEND_FILTER, AL_EFFECTSLOT_NULL, aSlot, AL_FILTER_NULL);
  error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
End;

procedure TBZSoundOpenALManager.LinkDirectFilterToSource(aSource : TBZBaseSoundSource);
begin
  // On assigne le filtre direct
  alSourcei(aSource.ManagerTag, AL_DIRECT_FILTER, aSource.DirectFilterFX.ManagerTag);
  error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
End;

procedure TBZSoundOpenALManager.UnLinkDirectFilterFromSource(aSource : TBZBaseSoundSource);
begin
  alSourcei(aSource.ManagerTag, AL_DIRECT_FILTER, AL_FILTER_NULL);
  error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
End;

procedure TBZSoundOpenALManager.KillSource(aSource : TBZBaseSoundSource);
var
  i, currentBufferTag, bufferCount:integer;
  ptr : PtrUInt;
  FxItem : TBZCustomSoundFXItem;
begin
  GlobalLogger.LogNotice('OpenAL Sound Manager : KillSource '+aSource.SoundName);

  if aSource.ManagerTag<>0 then
  begin
    // Utilise un filtre direct ?
    alSourceStop(ASource.ManagerTag);
    if  aSource.DirectFilterActivated then
    begin
      // On active le filtrage direct
      alSourcei(aSource.ManagerTag, AL_DIRECT_FILTER, AL_FILTER_NULL);
    end;
    if aSource.AuxSlots.Count > 0 then
    begin
      For I:=0 to aSource.AuxSlots.Count-1 do
      begin
        // On supprime les effets précédents
        alSource3i(aSource.ManagerTag, AL_AUXILIARY_SEND_FILTER, AL_EFFECTSLOT_NULL, I, AL_FILTER_NULL);
        // Remise à zero des filtres et ou effets
        alAuxiliaryEffectSloti(aSource.AuxSlots.Items[I].ManagerTag, AL_EFFECTSLOT_EFFECT, AL_EFFECT_NULL);
        // On supprime les effets
        FXItem := aSource.AuxSlots.Items[I].CacheFX;
        if (FXItem.inheritsFrom(TBZCustomSoundEffectItem)) then
            alDeleteEffects(1, PALUInt(@FXItem.ManagerTag)) // Effet
         else if (FXItem.inheritsFrom(TBZCustomSoundFilterItem)) then
            alDeleteFilters(1, PALUInt(@FXItem.ManagerTag)); // Filtre
        // Suppression des slots
        alDeleteAuxiliaryEffectSlots(1,PALUInt(@aSource.AuxSlots.Items[I].ManagerTag));
      end;
    end;

    // Supression de la source
    alDeleteSources(1, PALuint(@ASource.ManagerTag));
    ASource.ManagerTag:=0;

    { Nous ne pouvons pas simplement supprimer le tampon, car d'autres sources peuvent l'utiliser
      nous comptons donc le nombre de sources utilisées, puis supprimons si c'est le seul à l'utiliser
      Idem pour ASource.Sample.ManagerTag, nous mettons à zéro une fois que le sample n'est plus
      utilisé par d'autres sources }

    currentBufferTag:=ASource.Sample.ManagerTag;
    bufferCount:=0;
    if Asource.Sample <> nil then
    begin
      if currentBufferTag<>0 then
      begin
        for i := 0 to Sources.Count - 1 do
        begin
          if Sources[i].Sample.ManagerTag = currentBufferTag then
          begin
            bufferCount:=bufferCount+1;
          end;
        end;
        if bufferCount=1 then
        begin
          alDeleteBuffers(1, PALuint(@ASource.Sample.ManagerTag));
          ASource.Sample.ManagerTag := 0;
        end;
      end;
    end;
  end;
end;

procedure TBZSoundOpenALManager.UpdateSource(aSource : TBZBaseSoundSource);
var
  a: TALint;
  //MaxFX : Integer;
  i : Integer;
  ptr, tmpptr : PtrUInt;
  FxItem : TBZCustomSoundFXItem;

begin
  // Remise à zero des erreurs
  ClearOpenALError;
  if (sscNewSample in aSource.Changes) then
  begin
     if (Cadencer.Enabled=True) then Cadencer.Enabled := False;
     KillSource(aSource);
     aSource.Changes := aSource.Changes - [sscNewSample];
     Cadencer.Enabled := True;
  end;
  // Creation d'un source OpenAL, si besoin, et place son ID dans aSource.ManagerTag
  if aSource.SoundName <> '' then
  begin
    if aSource.ManagerTag = 0 then
    begin
      InitSoundSource(aSource);
      // Application des filtres et effets si assigné

      // 1. Filtre Direct
      if (aSource.DirectFilter<>'') and (aSource.DirectFilterActivated) then
      begin
        // Recuperation du filtre dans le cache dédié
        FXItem := aSource.DirectFilterFX;
        // Initialisation du filtre
        InitSoundFilter(FXItem);
        // Mise à jour des parametres
        UpdateSoundFilterParams(FXItem);
        // Liaison du filtre à la source
        LinkDirectFilterToSource(aSource);
      end;

      // 2. Initialisation des Slots si besoin
      InitSourceAuxSlots(aSource);
      // 3. Assignation des effets et filtres dans chaque Slot
      If aSource.AuxSlots.Count > 0 then
      begin
        if UseEnvironment then
        begin

        end;

      end;
   end
   else
   begin
      // Check to see if source has stopped, if so free it as limited number of sources allowed
      alGetSourcei(aSource.ManagerTag,AL_SOURCE_STATE,@a);
      error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
      if a=AL_STOPPED then
      begin
        aSource.Pause := true; // On ne libere pas la ressource on met juste la lecture en pause
        aSource.Playing := False;
        If aSource.AutoFreeOnStop then
        begin
          aSource.Free;
          Exit;
        end;
      end;
   end;

   if (aSource.Sample.Data<>nil) then
   begin



      if (sscSample in aSource.Changes) then
      begin

        // Associate buffer with source, buffer may have either been recently
        // created, or already existing if being used by another source
        alSourcei(aSource.ManagerTag, AL_BUFFER, aSource.sample.ManagerTag);
        error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');

        // If NbLoops>1 the source will constantly loop the sample, otherwise only play once
        alSourcei(aSource.managerTag, AL_LOOPING, Integer(aSource.NbLoops>1));
        error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
        // Start the source playing!
        if not(aSource.Pause) and (aSource.Playing) then
        begin
          alSourcePlay(aSource.ManagerTag);
          error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
        End;

        aSource.Changes := aSource.Changes - [sscSample];
      end;


      if (sscStatus in aSource.changes) then
      begin

        alSourcef(aSource.ManagerTag,AL_GAIN,aSource.Volume);
        error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
        alSourcef(aSource.managerTag, AL_MAX_DISTANCE, aSource.MaxDistance);
        error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');

        alSourcef(aSource.managerTag, AL_ROLLOFF_FACTOR, 1.0);
        error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');

        alSourcef(aSource.ManagerTag, AL_REFERENCE_DISTANCE, aSource.MinDistance);
        error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
        alSourcef(aSource.ManagerTag, AL_CONE_INNER_ANGLE, aSource.InsideConeAngle);
        error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
        alSourcef(aSource.ManagerTag, AL_CONE_OUTER_ANGLE, aSource.OutsideConeAngle);
        error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
        alSourcef(aSource.ManagerTag, AL_CONE_OUTER_GAIN, aSource.ConeOutsideVolume);
        error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
        alSourcef(aSource.managerTag, AL_PITCH,Clamp(aSource.Pitch,OAL_MIN_PITCH,OAL_MAX_PITCH));
        error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
        aSource.Changes := aSource.Changes - [sscStatus];
      end;

      if (sscFXStatus in aSource.Changes) then
      begin
         if (sfxcDirectFilter in aSource.FXChanges) then
         begin
           if ASource.DirectFilterActivated then LinkDirectFilterToSource(aSource)
           else UnLinkDirectFilterFromSource(aSource);
           aSource.FXChanges := aSource.FXChanges - [sfxcDirectFilter];
         End;

       (*  if (sfxEffectSlot0Status in aSource.FXChanges) then
         begin
           //ptr := aSource.AuxSlot[aSlot];
           if aSource.AuxSlot0FXActivated then
           begin
            if TBZCustomSoundEffectItem(aSource.AuxSlot0FX).FilteringActivated and (TBZCustomSoundEffectItem(aSource.AuxSlot0FX).FilterName<>'') then
            begin
              alSource3i(aSource.ManagerTag,AL_AUXILIARY_SEND_FILTER, aSource.AuxSlot[0],1, TBZCustomSoundEffectItem(aSource.AuxSlot0FX).FilterTag);
              error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
            end
            else
            begin
              alSource3i(aSource.ManagerTag,AL_AUXILIARY_SEND_FILTER,aSource.AuxSlot[0],0, AL_FILTER_NULL);
              error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
            end;
           End
           else

           aSource.FXChanges := aSource.FXChanges - [sfxEffectSlot0Status];
         End;  *)

         aSource.Changes := aSource.Changes - [sscFXStatus];
      End;

   (*   if (aSource.UseEnvironnment) and (smcEnvironnment in FChanges) then
      begin
        // Commit settings on source 0
       //Showmessage('Change EAX');
        eaxSet(DSPROPSETID_EAX20_BufferProperties,
               DSPROPERTY_EAXBUFFER_COMMITDEFERREDSETTINGS,
               aSource.ManagerTag, nil, 0);
        // Commit Listener settings
        eaxSet(DSPROPSETID_EAX20_ListenerProperties,
             DSPROPERTY_EAXLISTENER_COMMITDEFERREDSETTINGS, 0, nil, 0);
        FChanges:=[];
       end; *)

       //if sscTransformation in aSource.Changes then
       //begin
       //  alSourcefv(aSource.ManagerTag, AL_POSITION, PALFloat(aSource.Origin.Position.asAddress));
       //   error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
       //  alSourcefv(aSource.ManagerTag, AL_DIRECTION, PALFloat(aSource.Origin.Direction.asAddress));
       //  error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
       //  if (aSource.UseEnvironnemnt) and assigned(aSource.Sample.Data) then
       //   begin
            // Commit Listener settings
            //   eaxSet(DSPROPSETID_EAX20_ListenerProperties,
            //     DSPROPERTY_EAXLISTENER_COMMITDEFERREDSETTINGS, 0, nil, 0);
       //   end;
       //end;
    end;
  end;

  inherited UpdateSource(aSource);
end;

procedure TBZSoundOpenALManager.MuteSource(aSource : TBZBaseSoundSource; muted : Boolean);
begin
     if muted then alSourcef(aSource.ManagerTag, AL_MAX_GAIN, 0.0)
     else alSourcef(aSource.ManagerTag, AL_MAX_GAIN, 1.0);
end;

procedure TBZSoundOpenALManager.PauseSource(aSource : TBZBaseSoundSource; paused : Boolean);
begin
  if not paused then
  begin
    alSourcePlay(aSource.ManagerTag);
  end
  else
  begin
    alSourcePause(aSource.ManagerTag);
  end;
end;

procedure TBZSoundOpenALManager.PlaySource(aSource : TBZBaseSoundSource; playing : Boolean);
begin
  if not playing then
  begin
    alSourceStop(aSource.ManagerTag);
  end
  else
  begin
    alSourceRewind(aSource.managerTag);
    //alSourcePlay(aSource.ManagerTag);
    PauseSource(aSource,False);
  end;
end;

procedure TBZSoundOpenALManager.UpdateSources;
 var
  pos, dir, up, vel: TBZVector;
  DirUp: array[0..5] of TALfloat; //orientation
begin
  pos:=NullHmgVector;
  dir :=NullHmgVector;
  up:=NullHmgVector;
  vel:=NullHmgVector;

  ListenerCoordinates(pos, vel, dir, up);
  alListenerfv(AL_POSITION, PALfloat(@pos));
  alListenerfv(AL_VELOCITY, PALfloat(@vel));

  dirUp[0]:= dir.V[0];
  dirUp[1]:= dir.V[1];
  dirUp[2]:= dir.V[2];
  dirUp[3]:= up.V[0];
  dirUp[4]:= up.V[1];
  dirUp[5]:= up.V[2];
  alListenerfv(AL_ORIENTATION, PALfloat(@dirUp));

  inherited UpdateSources;
end;

function TBZSoundOpenALManager.EAXSupported : Boolean;
begin
  result:= alIsExtensionPresent('EAX2.0');
end;

function TBZSoundOpenALManager.EFXSupported : Boolean;
begin
  result:= alcIsExtensionPresent(alcGetContextsDevice(alcGetCurrentContext()), ALC_EXT_EFX_NAME);
  error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
end;

function TBZSoundOpenALManager.FullEAXSupported : Boolean;
begin
  result:= alIsExtensionPresent('EAX2.0') and (eaxSet<>nil) and (eaxGet<>nil);
end;

function TBZSoundOpenALManager.XRamSupported : Boolean;
begin
  result:= alIsExtensionPresent(PAnsiChar('EAX-RAM'));
  error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
end;

function TBZSoundOpenALManager.getXRamSize : Int64;
begin
  result := 0;
  if XRamSupported then result := Round(alGetInteger(AL_EAX_RAM_SIZE) / (1024*1024));
end;

function TBZSoundOpenALManager.getXRamFree : Int64;
begin
  result := 0;
  if XRamSupported then result := Round(alGetInteger(AL_EAX_RAM_FREE) / (1024*1024));
end;

function TBZSoundOpenALManager.getMaxEffectsPerSource : Integer;
Var
  MaxSlot : Integer;
begin
  MaxSlot:=0;
  alcGetIntegerv(alcGetContextsDevice(alcGetCurrentContext()),ALC_MAX_AUXILIARY_SENDS,Sizeof(integer),@MaxSlot);
  error:= alGetError(); if error<>AL_NO_ERROR then raise EOpenALError.Create('OpenAL Error #' + IntToStr(error) + ' (HEX: $'+ IntToHex(error,4)+')');
  result := MaxSlot;
end;

function TBZSoundOpenALManager.OutputFormat4ChannelsIsSupported : Boolean;
var Format : TALEnum;
begin
  result := false;
  Format := 0;
  Format := alGetEnumValue('AL_FORMAT_QUAD16');
  result := (Format > 0);
end;

function TBZSoundOpenALManager.OutputFormat51ChannelsIsSupported : Boolean;
var Format : TALEnum;
begin
  result := false;
  Format := 0;
  Format := alGetEnumValue('AL_FORMAT_51CHN16');
  result := (Format > 0);
end;

function TBZSoundOpenALManager.OutputFormat61ChannelsIsSupported : Boolean;
var Format : TALEnum;
begin
  result := false;
  Format := 0;
  Format := alGetEnumValue('AL_FORMAT_61CHN16');
  result := (Format > 0);
end;

function TBZSoundOpenALManager.OutputFormat71ChannelsIsSupported : Boolean;
var Format : TALEnum;
begin
  result := false;
  Format := 0;
  Format := alGetEnumValue('AL_FORMAT_71CHN16');
  result := (Format > 0);
end;

function TBZSoundOpenALManager.GetDefaultFrequency(aSource : TBZBaseSoundSource): integer;
begin
  Result:=aSource.Frequency;
end;

function TBZSoundOpenALManager.GetTimePosition(aSource : TBZBaseSoundSource): Single;
var
  tp :Single;
begin
  tp:=0.0;
  alGetSourcef(aSource.managerTag, AL_SEC_OFFSET, @tp);
  result := tp;
end;

function TBZSoundOpenALManager.GetALFormat(sampling: TBZSoundSample): integer;
begin
  result:= 0;
  //mono
  Case sampling.NbChannels of
    1 :
    begin
      case sampling.BitsPerSample of
        4:
        begin
          result := alGetEnumValue('AL_FORMAT_MONO_IMA4')
        end;
        8: result:= AL_FORMAT_MONO8;
        16: result:= AL_FORMAT_MONO16;
      end;
    end;
    2 :
    begin
      case sampling.BitsPerSample of //stereo
        4:
        begin
          result := alGetEnumValue('AL_FORMAT_STEREO_IMA4');
        end;
        8: result:= AL_FORMAT_STEREO8;
        16: result:= AL_FORMAT_STEREO16;
      end;
    end;
    4 :
    begin
      if sampling.BitsPerSample<>16 then Raise EBZSoundOpenALManager.Create('Nombre de bits par sample invalide')
      else
      begin
        if not(OutputFormat4ChannelsIsSupported) then Raise EBZSoundOpenALManager.Create('Format de sortie 4 canaux non supporté')
        else
        begin
          result := alGetEnumValue('AL_FORMAT_QUAD16');
        end;
      end;
    end;
    6 :
    begin
      if sampling.BitsPerSample<>16 then Raise EBZSoundOpenALManager.Create('Nombre de bits par sample invalide')
      else
      begin
        if not(OutputFormat51ChannelsIsSupported) then Raise EBZSoundOpenALManager.Create('Format de sortie 5.1 non supporté')
        else
        begin
          result := alGetEnumValue('AL_FORMAT_51CHN16');
        end;
      end;
    end;
    7 :
    begin
      if sampling.BitsPerSample<>16 then Raise EBZSoundOpenALManager.Create('Nombre de bits par sample invalide')
      else
      begin
        if not(OutputFormat61ChannelsIsSupported) then Raise EBZSoundOpenALManager.Create('Format de sortie 6.1 non supporté')
        else
        begin
          result := alGetEnumValue('AL_FORMAT_61CHN16');
        end;
      end;
    end;
    8 :
    begin
      if sampling.BitsPerSample<>16 then Raise EBZSoundOpenALManager.Create('Nombre de bits par sample invalide')
      else
      begin
        if not(OutputFormat71ChannelsIsSupported) then Raise EBZSoundOpenALManager.Create('Format de sortie 7.1 non supporté')
        else
        begin
          result := alGetEnumValue('AL_FORMAT_71CHN16');
        end;
      end;
    end;
  End;
end;

end.

