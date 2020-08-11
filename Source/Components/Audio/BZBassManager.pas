(*
  @abstract(Contient le "Manager de son" pour la librairie Bass)

  Les pilotes officiels de Bass peuvent être téléchargés à partir du site un4seen (http://www.un4seen.com).@br
  Cette version est disponible pour les plateformes (Windows, Linux et Android) @br
  et supporte les environnements 32bits et 64bits.

  Actuellement Bass sound manager supporte : @br
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
  @item (Le support des environnement 3D virtuels via les effets REVERB et EAX_REVERB (Uniquement sous Windows) )
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
unit BZBassManager;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils, Forms, Controls,
  BZLibBass, BZMath, BZSound, BZSoundSample;

Type
  { Exception levée par TBZSoundBassManager }
  EBZSoundBassManager = class(EBZSoundException);

  TBZBass3DAlgorithm = (baDefault, baOff, baFull, baLight);

  { Gestionnaire audio pour Bass }
  TBZSoundBassManager = class (TBZSoundManager)
  private
    FActivated : Boolean;
    FAlgorithm3D : TBZBASS3DAlgorithm;
    FChanges : TBZSoundManagerChanges;
  protected
    function GetErrorString(ErrCode : Integer) : String;
    procedure ShowError(msg: string);

    function DoActivate : Boolean; override;
    procedure DoDeActivate; override;
    procedure NotifyMasterVolumeChange; override;
    procedure Notify3DFactorsChanged; override;
    procedure NotifyEnvironmentChanged; override;

    procedure KillSource(aSource : TBZBaseSoundSource); override;
    procedure UpdateSource(aSource : TBZBaseSoundSource); override;
    procedure MuteSource(aSource : TBZBaseSoundSource; muted : Boolean); override;
    procedure PauseSource(aSource : TBZBaseSoundSource; paused : Boolean); override;
    procedure PlaySource(aSource : TBZBaseSoundSource; playing : Boolean); override;

    function GetDefaultFrequency(aSource : TBZBaseSoundSource) : Integer; override;
    function GetTimePosition(aSource : TBZBaseSoundSource): Single; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure UpdateSources; override;

    function CPUUsagePercent : Single; override;
    function EAXSupported : Boolean; override;

    function GetInformations : String; override;

    { Retourne les changements d'état lors de la lecture d'une source audio }
    property Changes : TBZSoundManagerChanges read FChanges;
  published

    property Algorithm3D : TBZBass3DAlgorithm read FAlgorithm3D write FAlgorithm3D default baDefault;
  end;

type
   TBZBASSInfo =  record
      channel : HCHANNEL;
      sample : HSAMPLE;
   end;
   PBZBASSInfo = ^TBZBASSInfo;

implementation

uses
  BZLogger, BZTypesHelpers, BZVectorMath, Dialogs;


procedure VectorToBASSVector(const aVector : TBZVector; var aBASSVector : BASS_3DVECTOR);
begin
  aBASSVector.x:=aVector.X;
  aBASSVector.y:=aVector.Y;
  aBASSVector.z:=-aVector.Z;
end;

constructor TBZSoundBassManager.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
  GlobalLogger.LogNotice('Initialize Sound BASS Manager. Ver ' + BASSVERSIONTEXT);
  FAlgorithm3D := baFull;
	// check the correct BASS was loaded
	if (HI(BASS_GetVersion) <> BASSVERSION) then
	begin
    MessageDlg('An incorrect version of BASS.DLL was loaded', mtError, [mbOK],0);
		Halt;
	end;



	// Initialize audio - default device, 44100hz, stereo, 16 bits
	//if not(BASS_Init(-1, 44100, 0, Handle, nil) then ShowError('Error initialization ! ');
  //if not(BASS_Init(1, OutputFrequency, BASS_DEVICE_3D, (TWinControl(Owner).Handle), nil)) then
  //begin
  //  ShowError('Error initializing audio!');
  //  Exit;
  //end;

 //
  //BASS_Load(bassdll);
  MaxChannels:=32;
end;

destructor TBZSoundBassManager.Destroy;
begin
  GlobalLogger.LogNotice('Finalize Sound BASS Manager');
	inherited Destroy;
  //BASS_UnLoad;
end;


function TBZSoundBassManager.GetErrorString(ErrCode : Integer) : String;
begin
  Case ErrCode of
    BASS_OK:
       Result :=  'all is OK';
    BASS_ERROR_MEM:
       Result :=  'memory error';
    BASS_ERROR_FILEOPEN:
       Result :=  'can''t open the file';
    BASS_ERROR_DRIVER:
       Result :=  'can''t find a free/valid driver';
    BASS_ERROR_BUFLOST:
       Result :=  'the sample buffer was lost';
    BASS_ERROR_HANDLE:
       Result :=  'invalid handle';
    BASS_ERROR_FORMAT:
       Result :=  'unsupported sample format';
    BASS_ERROR_POSITION:
       Result :=  'invalid position';
    BASS_ERROR_INIT:
       Result :=  'BASS_Init has not been successfully called';
    BASS_ERROR_START:
       Result :=  'BASS_Start has not been successfully called';
    BASS_ERROR_ALREADY:
       Result :=  'already initialized/paused/whatever';
    BASS_ERROR_NOCHAN:
       Result :=  'can''t get a free channel';
    BASS_ERROR_ILLTYPE:
       Result :=  'an illegal type was specified';
    BASS_ERROR_ILLPARAM:
       Result :=  'an illegal parameter was specified';
    BASS_ERROR_NO3D:
       Result :=  'no 3D support';
    BASS_ERROR_NOEAX:
       Result :=  'no EAX support';
    BASS_ERROR_DEVICE:
       Result :=  'illegal device number';
    BASS_ERROR_NOPLAY:
       Result :=  'not playing';
    BASS_ERROR_FREQ:
       Result :=  'illegal sample rate';
    BASS_ERROR_NOTFILE:
       Result :=  'the stream is not a file stream';
    BASS_ERROR_NOHW:
       Result :=  'no hardware voices available';
    BASS_ERROR_EMPTY:
       Result :=  'the MOD music has no sequence data';
    BASS_ERROR_NONET:
       Result :=  'no internet connection could be opened';
    BASS_ERROR_CREATE:
       Result :=  'couldn''t create the file';
    BASS_ERROR_NOFX:
       Result :=  'effects are not available';
    BASS_ERROR_NOTAVAIL:
       Result :=  'requested data is not available';
    BASS_ERROR_DECODE:
       Result :=  'the channel is/isn''t a decoding channel';
    BASS_ERROR_DX:
       Result :=  'a sufficient DirectX version is not installed';
    BASS_ERROR_TIMEOUT:
       Result :=  'connection timedout';
    BASS_ERROR_FILEFORM:
       Result :=  'unsupported file format';
    BASS_ERROR_SPEAKER:
       Result :=  'unavailable speaker';
    BASS_ERROR_VERSION:
       Result :=  'invalid BASS version (used by add-ons)';
    BASS_ERROR_CODEC:
       Result :=  'codec is not available/supported';
    BASS_ERROR_ENDED:
       Result :=  'the channel/file has ended';
    BASS_ERROR_BUSY:
       Result :=  'the device is busy';
    BASS_ERROR_UNKNOWN:
        Result := 'Unknown Error';
    else
        Result :=  'Unknown Error';
   end;

end;

procedure TBZSoundBassManager.ShowError(msg: string);
var
	s: string;
begin
	s := msg + LineEnding + 'Error code : ' + IntToStr(BASS_ErrorGetCode) +
             LineEnding + '   Message : ' + GetErrorString(BASS_ErrorGetCode);
  GlobalLogger.LogError(s);
  MessageDlg(s, mtError, [mbOK],0);
end;

function TBZSoundBassManager.DoActivate : Boolean;
const
  c3DAlgo : array [baDefault..baLight] of Integer =
      (BASS_3DALG_DEFAULT, BASS_3DALG_OFF, BASS_3DALG_FULL, BASS_3DALG_LIGHT);
begin
   //Assert(bass_isloaded,'BASS DLL is not present');
   GlobalLogger.LogNotice('Activate Sound BASS Manager. OutputFrequency = ' + OutputFrequency.ToString);
   //
   if not(BASS_Init(-1, OutputFrequency, BASS_DEVICE_3D + BASS_DEVICE_SPEAKERS + BASS_DEVICE_FREQ, (TWinControl(Owner).Handle), nil)) then
   begin
   	 Raise EBZSoundBassManager.Create('Error on initializing engine !' + LineEnding + GetErrorString(BASS_ErrorGetCode));
     Result:=False;
     Exit;
   end;
   if not(BASS_Start) then
   begin
     Raise EBZSoundBassManager.Create('Error on start engine !' + LineEnding + GetErrorString(BASS_ErrorGetCode));
     Result:=False;
     Exit;
   end;
   FActivated := True;
   BASS_SetConfig(BASS_CONFIG_3DALGORITHM, c3DAlgo[FAlgorithm3D]);
   NotifyMasterVolumeChange;
   Notify3DFactorsChanged;
   if (Environment<>seGeneric) then NotifyEnvironmentChanged;
   Result:=True;
end;

procedure TBZSoundBassManager.DoDeActivate;
begin
  //GlobalLogger.LogNotice('Desactivate Sound BASS Manager');
  FActivated:=False;
  BASS_Stop;
  BASS_Free;
end;

procedure TBZSoundBassManager.NotifyMasterVolumeChange;
begin
  if FActivated then BASS_SetVolume(MasterVolume);
end;

procedure TBZSoundBassManager.Notify3DFactorsChanged;
begin
  if FActivated then BASS_Set3DFactors(DistanceFactor, RollOffFactor, DopplerFactor);
end;

procedure TBZSoundBassManager.NotifyEnvironmentChanged;
const
  cEnvironmentToBASSConstant : array [seGeneric..sePsychotic] of Integer = (
      EAX_ENVIRONMENT_GENERIC, EAX_ENVIRONMENT_PADDEDCELL, EAX_ENVIRONMENT_ROOM,
      EAX_ENVIRONMENT_BATHROOM, EAX_ENVIRONMENT_LIVINGROOM, EAX_ENVIRONMENT_STONEROOM,
      EAX_ENVIRONMENT_AUDITORIUM, EAX_ENVIRONMENT_CONCERTHALL, EAX_ENVIRONMENT_CAVE,
      EAX_ENVIRONMENT_ARENA, EAX_ENVIRONMENT_HANGAR, EAX_ENVIRONMENT_CARPETEDHALLWAY,
      EAX_ENVIRONMENT_HALLWAY, EAX_ENVIRONMENT_STONECORRIDOR, EAX_ENVIRONMENT_ALLEY,
      EAX_ENVIRONMENT_FOREST, EAX_ENVIRONMENT_CITY, EAX_ENVIRONMENT_MOUNTAINS,
      EAX_ENVIRONMENT_QUARRY, EAX_ENVIRONMENT_PLAIN, EAX_ENVIRONMENT_PARKINGLOT,
      EAX_ENVIRONMENT_SEWERPIPE, EAX_ENVIRONMENT_UNDERWATER, EAX_ENVIRONMENT_DRUGGED,
      EAX_ENVIRONMENT_DIZZY, EAX_ENVIRONMENT_PSYCHOTIC);
begin
  if FActivated and EAXSupported then BASS_SetEAXParameters(cEnvironmentToBASSConstant[Environment],-1,-1,-1);
end;

procedure TBZSoundBassManager.KillSource(aSource : TBZBaseSoundSource);
var
  p : PBZBASSInfo;
begin
  if aSource.ManagerTag<>0 then
  begin
    GlobalLogger.LogNotice('KillSource');
    p := PBZBASSInfo(aSource.ManagerTag);
    if p^.channel<>0 then
       if not(BASS_ChannelStop(p^.channel)) then 	ShowError('Error kill source !'); //Assert(False);
    BASS_SampleFree(p^.sample);
    FreeMem(p);
    aSource.ManagerTag := 0;
  end;
end;

procedure TBZSoundBassManager.UpdateSource(aSource : TBZBaseSoundSource);
var
   i : Integer;
   p : PBZBASSInfo;
   objPos, objOri, objVel : TBZVector;
   position, orientation, velocity : BASS_3DVECTOR;
   res: Boolean;
   FxItem : TBZCustomSoundFXItem;
begin
  //if (sscSample in aSource.Changes) then
  //begin
  //  KillSource(aSource);
  //end;

  if (sscNewSample in aSource.Changes) or (sscSample in aSource.Changes) then
  begin
     if (Cadencer.Enabled=True) then Cadencer.Enabled := False;
     KillSource(aSource);
     aSource.Changes := aSource.Changes - [sscNewSample];
     Cadencer.Enabled := True;
  end;

  // Creation d'un source OpenAL, si besoin, et place son ID dans aSource.ManagerTag
  if aSource.SoundName <> '' then
  begin
    if (aSource.Sample=nil) or (aSource.Sample.Data=nil) or
       (aSource.Sample.Data.SoundDataSize = 0) then Exit;

    if (aSource.ManagerTag = 0) then
    begin
      //GlobalLogger.LogNotice('UpdateSource - Loading Data');
      p := AllocMem(SizeOf(TBZBASSInfo));
      p^.channel := 0;
      i := BASS_SAMPLE_VAM + BASS_SAMPLE_OVER_DIST;// + BASS_SAMPLE_3D;
      if aSource.Sample.Data.BitsPerSample = 8 then i := i + BASS_SAMPLE_8BITS;
      if aSource.NbLoops>1 then i := i + BASS_SAMPLE_LOOP;

      //GlobalLogger.LogStatus('- Sample Data Size in bytes = ' + aSource.Sample.Data.SoundDataSize.ToString);
      //GlobalLogger.LogStatus('- Sample Data Frequency     = ' + aSource.Sample.Data.Frequency.ToString);
      //GlobalLogger.LogStatus('- Sample Data Channels      = ' + aSource.Sample.Data.NbChannels.ToString);
      //GlobalLogger.LogStatus('- Sample Data Bits per sample = ' + aSource.Sample.Data.BitsPerSample.ToString);
      p^.Sample := BASS_SampleCreate(aSource.Sample.Data.SoundDataSize,aSource.Sample.Data.Frequency,aSource.Sample.Data.NbChannels,MaxChannels,i);
      if (p^.sample=0) then
      begin
        Raise EBZSoundBassManager.Create('Error initializing sample !' + LineEnding + GetErrorString(BASS_ErrorGetCode));
        Exit;
      end;
      BASS_SampleSetData(p^.Sample, aSource.Sample.Data.SoundData);
      p^.channel := BASS_SampleGetChannel(p^.sample, true);
      BASS_SetConfig(BASS_CONFIG_SRC_SAMPLE,0);

      //if (p^.sample=0) then 	ShowError('Error load sample source !');
      //p^.sample := BASS_SampleLoad(True, aSource.Sample.Data.SoundData, 0, 0, MaxChannels, i);
      //Assert(p^.sample<>0, 'BASS Error '+IntToStr(Integer(BASS_ErrorGetCode)));

      aSource.ManagerTag := Integer(p);

      // 1. Filtre Direct
      if (aSource.DirectFilter<>'') and (aSource.DirectFilterActivated) then
      begin
        // Recuperation du filtre dans le cache dédié
        FXItem := aSource.DirectFilterFX;
        // Initialisation du filtre
        //InitSoundFilter(FXItem);
        // Mise à jour des parametres
        //UpdateSoundFilterParams(FXItem);
        // Liaison du filtre à la source
        //LinkDirectFilterToSource(aSource);
      end;

      // 2. Initialisation des Slots si besoin
      //InitSourceAuxSlots(aSource);
      // 3. Assignation des effets et filtres dans chaque Slot
      If aSource.AuxSlots.Count > 0 then
      begin
        if UseEnvironment then
        begin

        end;
      end;

      if aSource.Frequency<=0 then aSource.Frequency:=-1;
    end;

  end
  else
  begin
    if aSource.ManagerTag<>0 then
    begin
      //GlobalLogger.LogNotice('UpdateSource - desactivate source');
      p := PBZBASSInfo(aSource.ManagerTag);
      if BASS_ChannelIsActive(p^.channel) <> BASS_ACTIVE_PLAYING then
      begin
        aSource.Pause := true; // On ne libere pas la ressource on met juste la lecture en pause
        aSource.Playing := False;
        If aSource.AutoFreeOnStop then
        begin
          p^.channel:=0;
          aSource.Free;
          Exit;
        end;
      end;
    end;
  end;

  if (aSource.ManagerTag <> 0) then
  begin
    p := PBZBASSInfo(aSource.ManagerTag);
    if (p^.channel=0) and (aSource.Sample.Data<>nil)  then
    begin
      p^.channel := BASS_SampleGetChannel(p^.sample,false);
    end;

    if (sscSample in aSource.Changes) then
    begin
      if not(aSource.Pause) and (aSource.Playing) then
      begin
        BASS_ChannelPlay(p^.channel,true);
      End;
      aSource.Changes := aSource.Changes - [sscSample];
    end;

    if (sscStatus in aSource.changes) then
    begin
      //res := BASS_ChannelSetAttribute(p^.channel, BASS_ATTRIB_FREQ, 0);
      //if Res<>BASS_OK then ShowError('Error  ChannelSetAttribute - BASS_ATTRIB_FREQ');

      if aSource.Mute then
        res := BASS_ChannelSetAttribute(p^.channel, BASS_ATTRIB_VOL, 0)
      else
        res := BASS_ChannelSetAttribute(p^.channel, BASS_ATTRIB_VOL, aSource.Volume);
      if not(Res) then
      begin
        Raise EBZSoundBassManager.Create('Error initializing channel attribute : VOLUME !' + LineEnding + GetErrorString(BASS_ErrorGetCode));
        Exit;
      end;


      //res := BASS_ChannelSet3DAttributes(p^.channel, BASS_3DMODE_NORMAL,
      //                                   aSource.MinDistance, aSource.MaxDistance,
      //                                   Round(aSource.InsideConeAngle),
      //                                   Round(aSource.OutsideConeAngle),
      //                                   Round(aSource.ConeOutsideVolume*100));
      //if not(Res) then ShowError('Error BASS_ChannelSet3DAttributes - BASS_3DMODE_NORMAL');
      end;

      // Pitch are not supported by Bass or it is BITRATE ???
      aSource.Changes := aSource.Changes - [sscStatus];
    end;

    if (sscFXStatus in aSource.Changes) then
    begin
      if (sfxcDirectFilter in aSource.FXChanges) then
      begin
        //if ASource.DirectFilterActivated then LinkDirectFilterToSource(aSource)
        //else UnLinkDirectFilterFromSource(aSource);
        aSource.FXChanges := aSource.FXChanges - [sfxcDirectFilter];
      end;
      aSource.Changes := aSource.Changes - [sscFXStatus];
    end;

    // if (aSource.UseEnvironnment) and (smcEnvironnment in FChanges) then

    //if sscTransformation in aSource.Changes then
    //begin
      //if aSource.Origin<>nil then
      //begin
      //  objPos := aSource.Origin.AbsolutePosition;
      //  objOri := aSource.Origin.AbsoluteZVector;
      //  objVel := NullHmgVector;
      //end
      //else
      //begin
      //  objPos := NullHmgPoint;
      //  objOri := ZHmgVector;
      //  objVel := NullHmgVector;
      //end;

      //VectorToBASSVector(objPos, position);
      //VectorToBASSVector(objVel, velocity);
      //VectorToBASSVector(objOri, orientation);
      //res := BASS_ChannelSet3DPosition(p^.channel,position, orientation, velocity);
      //if Res<>BASS_OK then ShowError('Error BASS_ChannelSet3DPosition');
      //Source.Changes := aSource.Changes - [sscTransformation];
    //end;

  //end;
  //else
  //  aSource.Free;

  inherited UpdateSource(aSource);
end;

procedure TBZSoundBassManager.MuteSource(aSource : TBZBaseSoundSource; muted : Boolean);
var
   p : PBZBASSInfo;
   res : Boolean;
begin
  if aSource.ManagerTag<>0 then
  begin
    p := PBZBASSInfo(aSource.ManagerTag);
    if muted then res := BASS_ChannelSetAttribute(p^.channel,  BASS_ATTRIB_VOL, 0)
    else res := BASS_ChannelSetAttribute(p^.channel, BASS_ATTRIB_VOL, aSource.Volume);
    Assert(res);
  end;
end;

procedure TBZSoundBassManager.PauseSource(aSource : TBZBaseSoundSource; paused : Boolean);
var
  p : PBZBASSInfo;
begin
  if aSource.ManagerTag<>0 then
  begin
    GlobalLogger.LogNotice('UpdateSource - Pause sound');
    p := PBZBASSInfo(aSource.ManagerTag);
    if paused then BASS_ChannelPause(p^.channel)
    else BASS_ChannelPlay(p^.channel,false);
  end;
end;

procedure TBZSoundBassManager.PlaySource(aSource : TBZBaseSoundSource; playing : Boolean);
var
  p : PBZBASSInfo;
begin
  p := PBZBASSInfo(aSource.ManagerTag);
  if not(playing) then
  begin
    if not(BASS_ChannelPlay(p^.channel,true)) then
    begin
      Raise EBZSoundBassManager.Create('Error while playing audio !' + LineEnding + GetErrorString(BASS_ErrorGetCode));
      Exit;
    end;
    //aSource.Playing := True;
  end
  else
  begin
    //PauseSource(aSource,False);
    BASS_ChannelStop(p^.channel);
    //aSource.Playing := False;
    aSource.Pause := False;
  end;
end;

procedure TBZSoundBassManager.UpdateSources;
var
  objPos, objVel, objDir, objUp : TBZVector;
  position, velocity, fwd, top : BASS_3DVECTOR;
begin
   //GlobalLogger.LogNotice('UpdateSources');
   // update listener
   ListenerCoordinates(objPos, objVel, objDir, objUp);
   VectorToBASSVector(objPos, position);
   VectorToBASSVector(objVel, velocity);
   VectorToBASSVector(objDir, fwd);
   VectorToBASSVector(objUp, top);
   if not BASS_Set3DPosition(position, velocity, fwd, top) then
   begin
     Raise EBZSoundBassManager.Create('Error initializing 3D position !' + LineEnding + GetErrorString(BASS_ErrorGetCode));
     Exit;
   end;
   // update sources
   inherited UpdateSources;
   BASS_Apply3D;
end;

function TBZSoundBassManager.CPUUsagePercent : Single;
begin
  Result := BASS_GetCPU*100;
end;

function TBZSoundBassManager.EAXSupported : Boolean;
var
  c : Cardinal;
  s : Single;
begin
  {$IFDEF WINDOWS}
  Result := BASS_GetEAXParameters(c, s, s, s);
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TBZSoundBassManager.GetInformations : String;
var
  s  : string;
  n : Integer;
  DeviceInfo : BASS_DEVICEINFO;
  BassInfos  : BASS_INFO;

function getDeviceTypeInfo(di : BASS_DEVICEINFO) : String;
var
  St : String;
begin
  case (di.flags and BASS_DEVICE_TYPE_MASK) of
    BASS_DEVICE_TYPE_NETWORK :
      St := 'Remote Network';

    BASS_DEVICE_TYPE_SPEAKERS :
      St := 'Speakers';

    BASS_DEVICE_TYPE_LINE :
      St := 'Line';

    BASS_DEVICE_TYPE_HEADPHONES :
      St := 'Headphones';

    BASS_DEVICE_TYPE_MICROPHONE :
      St := 'Microphone';

    BASS_DEVICE_TYPE_HEADSET :
      St := 'Headset';

    BASS_DEVICE_TYPE_HANDSET :
      St := 'Handset';

    BASS_DEVICE_TYPE_DIGITAL :
      St := 'Digital';

    BASS_DEVICE_TYPE_SPDIF :
      St := 'SPDIF';

    BASS_DEVICE_TYPE_HDMI :
      St := 'HDMI';

    BASS_DEVICE_TYPE_DISPLAYPORT :
      St := 'DisplayPort';
  else
    St := 'Unknown';
  end;
  Result := St;
  //St := St + 'flags:';
  //if (di.flags and BASS_DEVICE_LOOPBACK) = BASS_DEVICE_LOOPBACK then
  //  St := St + ' loopback';
  //if (di.flags and BASS_DEVICE_ENABLED) = BASS_DEVICE_ENABLED then
  //  St := St + ' enabled';
  //if (di.flags and BASS_DEVICE_DEFAULT) = BASS_DEVICE_DEFAULT then
  //  St := St + ' default';
  //WriteLn(St, ' (', di.flags, ')');
end;
begin
  S := 'BASS Version :  ' + BASSVERSIONTEXT + LineEnding + LineEnding;
  S := S + '--------------------------------------------------------------------' + LineEnding;
  S := S + 'Output devices : ' + LineEnding;
  n := 1;
  While BASS_GetDeviceInfo(n, DeviceInfo) do
  begin
    S := S + '   - ' + n.ToString + ' : ' + AnsiToUTF8(DeviceInfo.name) + LineEnding;
    S := S + '    Driver : ' + DeviceInfo.driver + LineEnding;
    S := S + '      Type : ' + getDeviceTypeInfo(DeviceInfo) + LineEnding;
    S := S + LineEnding;
    inc(n);
  end;
  S :=  S + 'Input devices : ' + LineEnding;
  n := 0;
  While  BASS_RecordGetDeviceInfo(n, DeviceInfo) do
  begin
    S := S + '   - ' + n.ToString + ' : ' + AnsiToUTF8(DeviceInfo.name) + LineEnding;
    S := S + '         Driver : ' + DeviceInfo.driver + LineEnding;
    S := S + '           Type : ' + getDeviceTypeInfo(DeviceInfo) + LineEnding;
    S := S + LineEnding;
    inc(n);
  end;
  S := S + '--------------------------------------------------------------------' + LineEnding + LineEnding;
  BASS_GetInfo(BassInfos);
  S := S + ' Number of speaker : ' + BassInfos.speakers.ToString + LineEnding;
  S := S + '     EAX Supported : ' + BassInfos.eax.ToString() + LineEnding;
  S := S + '   Total HW Memory : ' + (BassInfos.hwsize / 1024).ToString + 'Ko'  + LineEnding;
  S := S + '    Free HW Memory : ' + (BassInfos.hwfree / 1024).ToString + 'Ko'  + LineEnding;
  S := S + '          Min Rate : ' + BassInfos.minrate.ToString + LineEnding;
  S := S + '          Max Rate : ' + BassInfos.maxrate.ToString + LineEnding;
  S := S + '    Free HW Memory : ' + (BassInfos.hwfree / 1024).ToString + 'Ko'  + LineEnding;
  S := S + '  Free sample slot : ' + BassInfos.freesam.ToString + LineEnding;
  S := S + ' Current frequency : ' + BassInfos.freq.ToString + ' Mhz' + LineEnding;
  S := S + '           Latency : ' + BassInfos.latency.ToString + ' ms' + LineEnding;

  Result := S;
end;

function TBZSoundBassManager.GetDefaultFrequency(aSource : TBZBaseSoundSource) : Integer;
var
  p : PBZBASSInfo;
  sampleInfo : BASS_Sample;
begin
  try
    p := PBZBASSInfo(aSource.ManagerTag);
    BASS_SampleGetInfo(p^.sample, sampleInfo);
    Result := sampleInfo.freq;
  except
    Result:=-1;
  end;
end;

function TBZSoundBassManager.GetTimePosition(aSource : TBZBaseSoundSource) : Single;
var
  p : PBZBASSInfo;
begin
  Result := 0.0;
  if (aSource.ManagerTag<>0) then
  begin
    p := PBZBASSInfo(aSource.ManagerTag);
    Result := BASS_ChannelGetPosition(p^.channel,BASS_POS_BYTE) / aSource.Sample.Data.BytesPerSec;
  end;
end;

end.

