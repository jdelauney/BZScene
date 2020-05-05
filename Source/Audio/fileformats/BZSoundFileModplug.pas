(*====< GLSoundFileModPlug.pas >===================================================@br
  @created(2016-11-16)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(06/11/2017 : Creation  )
  )
--------------------------------------------------------------------------------@br

  @bold(Description :)@br
    L'unité @bold(GLSoundFileModPlug permet le chargement de fichier musicaux  de style "Tracker" au format
    MOD, S3M, XM.... par le biais de librairie externe LibModPlug.
    Une fois chargé le fichier pourra être joué via le "SoundManager" de
    disponible de votre choix

  ------------------------------------------------------------------------------@br

  Notes : @br
   Vous trouverez les librairies nécessaire DLL pour Windows 32bits et 64bits dans le
   dossier "/Externals"
  ------------------------------------------------------------------------------@br

  Credits :
   @unorderedList(
     @item (Codé sur une base de GLScene http://www.sourceforge.net/glscene)
   )
   @br
  ------------------------------------------------------------------------------@br
  LICENCE : MPL / GPL @br
  @br
 *==============================================================================*)
unit BZSoundFileModplug;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\bzscene_options.inc}
//==============================================================================

interface


uses
  Classes, SysUtils,
  BZClasses, BZSoundSample,
  // OGG-Vorbis units
  BZLibModplug,
  ctypes;

Type
   EBZModFileError = Class(EBZInvalidSoundFile);
   EBZModLoadError  = Class(EBZSoundException);

   TBZSoundModplugFormat = class (TBZSoundFileIO)
   private
     FDataByteLength : Int64;
   protected
     Function CheckFormat(): Boolean; override;
     Procedure LoadFromMemory; override;

   public
     Constructor Create(AOwner: TPersistent); Override;
     Destructor Destroy; Override;

     class function Capabilities : TBZDataFileCapabilities; override;

   end;

implementation

uses BZLogger, BZStreamClasses, BZLibOpenAL, Dialogs;

Constructor TBZSoundModplugFormat.Create(AOwner: TPersistent);
begin
  inherited create(AOwner);
  With DataFormatDesc do
  begin
    Name:='Modplug';
    Desc:='Module Tracker File';
    FileMask:='*.mod;*.s3m;*.xm';
  end;
  FDataByteLength := 0;
end;

Destructor TBZSoundModplugFormat.Destroy;
begin
  inherited Destroy;
end;

class function TBZSoundModplugFormat.Capabilities : TBZDataFileCapabilities;
begin
   Result:=[dfcRead];
end;

function TBZSoundModplugFormat.CheckFormat(): Boolean;
begin
  Result := True;
End;

procedure TBZSoundModplugFormat.LoadFromMemory;
const
   BufSize = 20000;
var
  TmpStream     : TMemoryStream;
  Dt : PtrUint;
  Freq : LongWord;
  mod_file: PModPlugFile;
  Buffer : Pointer;
  ReadCount : LongWord;
  ModFile_Settings : PModPlug_Settings;

  function mod_read(const Buffer: Pointer; const Count: Longword): Longword;
  var
    Res: cint;
  begin
    Res := ModPlug_Read(mod_file,Buffer, Count);
    if Res < 0 then Result := 0 else Result := Res;
  end;

begin
  mod_file := Nil;
  Buffer := Nil;

  GetMem(Buffer, Self.Memory.Size);
  Self.Memory.Read(Buffer^, Self.Memory.Size);
  mod_file := ModPlug_Load(Buffer, Self.Memory.Size);
  if mod_file = nil then
  begin
    FreeMem(Buffer);
    EBZModFileError.Create('Loading Module Error');
  end;
  //FreeAnNil(TmpStream);
  FreeMem(Buffer);
  Buffer := Nil;
  TmpStream := TMemoryStream.Create;
  GetMem(Buffer,BufSize);
  ReadCount := BufSize;
  repeat
   ReadCount := mod_read(Buffer, BufSize);
   TmpStream.WriteBuffer(Buffer^, BufSize)
  until ReadCount = 0;

  FreeMem(Buffer);
  Buffer:=nil;
  FDataByteLength := TmpStream.Size;
  SetSize(FDataByteLength);
  //Reallocmem(FSoundData, FDataByteLength);
  Move(PByte(TmpStream.Memory)^,SoundData^,FDataByteLength);
  FreeAndNil(TmpStream);

  //With Sampling do
  //begin
  New(ModFile_Settings);
  ModPlug_GetSettings(ModFile_Settings);


    Frequency := ModFile_Settings^.mFrequency; // 44100;
    BitsPerSample := ModFile_Settings^.mBits; // 16;
    NbChannels := ModFile_Settings^.mChannels;
  //end;
  Dispose(ModFile_Settings);
  ModPlug_Unload(Mod_File);
End;

initialization

  RegisterSoundFileFormat('mod', 'Amiga ProTracker files', TBZSoundModplugFormat);
  RegisterSoundFileFormat('s3m', 'ScreamTracker 3 files', TBZSoundModplugFormat);
  RegisterSoundFileFormat('stm', 'ScreamTracker 2 files', TBZSoundModplugFormat);
  RegisterSoundFileFormat('xm', 'FastTracker 2 files', TBZSoundModplugFormat);
  RegisterSoundFileFormat('it', 'ImpulseTracker files', TBZSoundModplugFormat);

{
All module format file extension

MOD (ProTracker (Amiga))
STM (ScreamTracker 2)
S3M (ScreamTracker 3)
XM (FastTracker 2)
IT (Impulse Tracker)

----- Addon format file extention can be added :

  669 (Composer 669)
  AMF (ASYLUM Music Format / DSMI Advanced Music Format)
  AMS (Extreme's Tracker / Velvet Studio)
  DBM (Digi Booster Pro)
  DMF (X-Tracker / Delusion Digital Music File )
  DSF	Delusion Digital Sound File
  DSM (DSIK Format / Digital Sound Module)
  DSP	Dynamic Studio Professional Module
  FAR (Farandole Composer)
  FSM	Farandole Composer WaveSample File
  MDL (DigiTrakker)
  MDR	ModPlug Compressed Module
  MED (OctaMED (Amiga))
  MTM (MultiTracker)
  OKT (Oktalyzer)
  PTM (PolyTracker)
  ULT (UltraTracker)
  UMX (Unreal Music Package)
  MT2 (MadTracker 2)
  PSM (Epic Megagames MASI / Protracker Studio Module  )
  S3Z	Compressed Scream Tracker 3 Module
  UMX	Unreal Music Package
  XMZ	FastTracker 2 Extended Module
  }

end.

