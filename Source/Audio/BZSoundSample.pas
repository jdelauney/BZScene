(*
  @abstract(permet le support pour le chargement de fichier audio. @br
  Une fois chargé le fichier pourra être joué via le "SoundManager" de disponible de votre choix)

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

  @bold(Dépendances) : BZClasses, BZSystem

  -------------------------------------------------------------------------------------------------------------

  Credits :
   @unorderedList(
     @item (Codé sur une base de GLScene http://www.sourceforge.net/glscene)
   )

  -------------------------------------------------------------------------------------------------------------

  @bold(Licence) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZSoundSample;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils, Dialogs,
  BZClasses;

type
  { Exception levée en cas d'erreur }
  EBZSoundException = Class(EBZBaseException);
  { Exception levée en cas de fichier audio invalide }
  EBZInvalidSoundFile = Class(EBZSoundException);

  { @abstract(Classe de base représentant un échantillon sonore et les informations relative à sa qualité.)

  Cette classe disposes de quelques fonctions utiles pour la convertion des ondes sonores et pour
  récupérer les "peaks" à un temps donnés. }
  TBZCustomSoundSample = class(TBZCustomDataFile)
  private
    FOwner : TPersistent;
    FFrequency : Integer;
    FNbChannels : Integer;
    FBitsPerSample : Integer;

    FSoundData : PByte;
    FSoundDataSize : LongWord;
    FWaveFormData : PSingle;

    function GetWaveForm : PSingle;
  protected
    function GetOwner : TPersistent; override;
  public
    constructor Create(AOwner: TPersistent);  override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure SetSize(Const value : LongWord);

    { Retourne le nombre d'octet par seconde }
    function BytesPerSec : Integer;
    {Retourne le nombre d'octet par échantillon }
    function BytesPerSample : Integer;
    { Retourne "le nombre d'intensité" dans l'échantillon. }
    function LengthInSamples : Integer;
    { Retourne la durée de l'échantillon à la vitesse nominale en secondes. }
    function LengthInSec : Single;
    { Retourne la taille de l'échantillon en octet }
    function LengthInBytes: Integer;

    { Convertis de l'onde sonore }
    procedure ComputeWaveForm;
    { Convertis le bps de 8 bits vers 16 bits }
    procedure ConvertBps8to16;
    { Convertis le bps de 16 bits vers 8 bits }
    procedure ConvertBps16to8;
    { Convertis le bps de 32 bits en virgule flottante vers 16 bits }
    procedure ConvertBpsFloat32To16;
    { Convertis de Mono vers stéréo }
    procedure ConvertMonoToStereo;
    { Convertis de Stéréo vers mono }
    procedure ConvertStereoToMono;

    { Retourne @TRUE si l'échantillon est en stéréo }
    function IsStereo : Boolean;
    { Retourne @TRUE si l'échantillon est en mono }
    function IsMono : Boolean;
    { Retourne @TRUE si l'échantillon à plusieurs canaux (exemple un échantillon en 5.1) }
    function IsMultiChannel : Boolean;

    { Retourne les "Peaks" gauche et droite, à la position "TimeInSec" }
    procedure GetPeaks(TimeInSec : Single; var LeftChannel,RightChannel : Single);

    { Retourne le tampon des données }
    property SoundData : PByte read FSoundData write FSoundData;     //@Renommer par Buffer
    { Retourne la taille du tampon }
    property SoundDataSize : LongWord read FSoundDataSize;
    { Retourne le tampon de l'onde sonore en virgule flottante }
    property WaveForm : PSingle Read GetWaveForm;

    { Retourne la fréquence d'échantillonnage en Hz (= échantillons par seconde) }
    property Frequency : Integer read FFrequency write FFrequency default 22050;
    { Retourne le nombre  de canaux d'échantillonnage. 1 = mono, 2 = stéréo, etc... }
    property NbChannels : Integer read FNbChannels write FNbChannels default 1;
    { Retourne le nombre de bits par échantillon. Les valeurs communes sont 8 et 16 bits. }
    property BitsPerSample : Integer read FBitsPerSample write FBitsPerSample default 8;
  end;

  { Fonctions disponibles pour la génération d'onde sonore }
  TBZSoundWaveGenerator = (swgSine, swgSquare, swgSawtooth, swgTriangle);

  { Classe représentant un échantillon sonore avec prise en charge des différents format de fichier. @br
  Cette classe ajoute également la possibilité de créer ces propres échantillons sonore }
  TBZSoundSample = Class(TBZCustomSoundSample)
  public
    Procedure LoadFromFile(Const FileName: String);override;
    // procedure GenerateWaveForm(WaveGen : TBZSoundWaveGenerator; TimeInSec, Freq : Integer);
  end;

  { Classe de type TBZCustomSoundSample }
  TBZSoundSampleClass = class of TBZCustomSoundSample;

  { @abstract(Classe de base abstraite pour différents formats de fichiers audio.)

    L'implémentation réelle de ces fichiers (WAV, RAW ...) doit être faite
    séparément. Le concept de TBZSoundFile est très similaire à TGraphic   
    
    L'implémentation par défaut pour LoadFromFile / SaveToFile consiste à appeler directemente
    les méthodes basées sur les flux, c.-à-d. vous devrez simplement remplacer les
    méthodes "CheckData" et "LoadFromMemory" provenant de la classe parente TBZCustomDataFile. }
  TBZSoundFileIO = class(TBZCustomSoundSample)
  private

  protected
    procedure RaiseInvalidSoundFile(Msg:String);
  public

    Constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

  end;

 // TBZSoundSampleClass = class of TBZSoundSample;

  { Description basique pour les formats de fichier audio supporté }
  TBZSoundFileFormat = record
    SoundFileClass : TBZSoundSampleClass;
    Extension      : String;
    Description    : String;
    DescResID      : Integer;
  end;
  PSoundFileFormat = ^TBZSoundFileFormat;

  { Liste des format audio supportés }
  TBZSoundFileFormatsList = class(TList)
  public
    destructor Destroy; override;
    procedure Add(const Ext, Desc: String; DescID: Integer; AClass: TBZSoundSampleClass);
    function FindExt(Ext: string): TBZSoundSampleClass;
    Function FindFromFileName(Const fileName: String): TBZSoundSampleClass;
    procedure Remove(AClass: TBZSoundSampleClass);
    procedure BuildFilterStrings(SoundFileClass: TBZSoundSampleClass; out Descriptions, Filters: string);
  end;

{ Retourne la liste des formats audio supportés}
Function GetBZSoundFileFormats: TBZSoundFileFormatsList;

{ Enregistrement d'un format audio }
procedure RegisterSoundFileFormat(const AExtension, ADescription: String; AClass: TBZSoundSampleClass);
{ Libération d'un format audio supporté }
procedure UnregisterSoundFileClass(AClass: TBZSoundSampleClass);

implementation

uses BZSystem;

var
  vSoundFileFormats : TBZSoundFileFormatsList;

procedure RegisterSoundFileFormat(const AExtension, ADescription: String; AClass: TBZSoundSampleClass);
begin
   RegisterClass(AClass);
   vSoundFileFormats.Add(AExtension, ADescription, 0, AClass);
end;

procedure UnregisterSoundFileClass(AClass: TBZSoundSampleClass);
begin
  if Assigned(vSoundFileFormats) then
	  vSoundFileFormats.Remove(AClass);
end;

{%region=====[ TBZCustomSoundSample ]=============================================}

constructor TBZCustomSoundSample.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner:=AOwner;
  FFrequency:=22050;
  FNbChannels:=1;
  FBitsPerSample:=8;
  FSoundData := nil;
  FSoundDataSize := 0;
  FWaveFormData := nil;
end;

destructor TBZCustomSoundSample.Destroy;
begin
  if FSoundData<>nil then FreeMem(FSoundData);
  FSoundData := nil;
  if FWaveFormData<> nil then FreeMem(FWaveFormData);
  FWaveFormData := nil;
	inherited Destroy;
end;

procedure TBZCustomSoundSample.Assign(Source: TPersistent);
begin
  if Source is TBZCustomSoundSample then
  begin
    FFrequency:=TBZCustomSoundSample(Source).Frequency;
    FNbChannels:=TBZCustomSoundSample(Source).NbChannels;
    FBitsPerSample:=TBZCustomSoundSample(Source).BitsPerSample;
    SetSize(TBZCustomSoundSample(Source).SoundDataSize);
    Move(TBZCustomSoundSample(Source).SoundData^, FSoundData^, FSoundDataSize);
  end else inherited Assign(Source);
end;

procedure TBZCustomSoundSample.SetSize(const value : LongWord);
begin
  if FSoundDataSize = Value then exit;
  if Value = 0 then
  begin
    if FSoundData<>nil then
    begin
      FreeMem(FSoundData);
      FSoundData := nil;
      FSoundDataSize := 0;
      Exit;
    end;
  end
  else if FSoundData<>nil then
  begin
    FreeMem(FSoundData);
    ReAllocMem(FSoundData, Value);
    FSoundDataSize := Value;
  end
  else
  begin
    ReAllocMem(FSoundData, Value);
    FSoundDataSize := Value;
  end;

  if FWaveFormData<>nil then
  begin
    FreeMem(FWaveFormData);
    FWaveFormData := nil;
  end;
end;

function TBZCustomSoundSample.GetWaveForm : PSingle;
begin
  if FWaveFormData = nil then ComputeWaveForm;
  Result := FWaveFormData;
end;

function TBZCustomSoundSample.GetOwner : TPersistent;
begin
   Result:=FOwner;
end;

function TBZCustomSoundSample.BytesPerSec : Integer;
begin
   Result:=(FFrequency*FBitsPerSample*FNbChannels) shr 3;
end;

function TBZCustomSoundSample.BytesPerSample : Integer;
begin
   Result:=FBitsPerSample shr 3;
end;

function TBZCustomSoundSample.LengthInSamples : Integer;
var
   d : Integer;
begin
   d:=BytesPerSample*FNbChannels;
   if d>0 then Result:=FSoundDataSize div d
   else Result:=0;
end;

function TBZCustomSoundSample.LengthInSec : Single;
Var
   bps : Integer;
begin
   Result := 0;
   bps := BytesPerSec;
  // ShowMessage('BPS : ' +Bps.ToString + #10+#13+'DataSize : ' +FSoundDataSize.ToString);

   if bps<>0 then Result:=FSoundDataSize/bps;
end;

function TBZCustomSoundSample.LengthInBytes: Integer;
begin
   Result :=Round(LengthInSec*FFrequency);
end;

procedure TBZCustomSoundSample.ComputeWaveForm;
begin

end;

procedure TBZCustomSoundSample.ConvertBps8to16;
begin

end;

procedure TBZCustomSoundSample.ConvertBps16to8;
begin

end;

procedure TBZCustomSoundSample.ConvertBpsFloat32To16;
begin

end;

procedure TBZCustomSoundSample.ConvertMonoToStereo;
begin

end;

procedure TBZCustomSoundSample.ConvertStereoToMono;
begin

end;

function TBZCustomSoundSample.IsStereo : Boolean;
begin
  Result := (FNbChannels = 2);
end;

function TBZCustomSoundSample.IsMono : Boolean;
begin
  Result := (FNbChannels = 1);
end;

function TBZCustomSoundSample.IsMultiChannel : Boolean;
begin
  Result := (FNbChannels > 2);
end;

procedure TBZCustomSoundSample.GetPeaks(TimeInSec : Single; var LeftChannel, RightChannel : Single);
var
 Buffer : PSingle;
 Offset : LongWord;
begin
  if FWaveFormData = nil then ComputeWaveForm;
  if IsStereo or IsMultiChannel then
  begin
    Buffer := PSingle(FWaveFormData + Offset);
    LeftChannel := Buffer^;
    Inc(Buffer);
    RightChannel := Buffer^;
  end
  else
  begin
     Buffer := PSingle(FWaveFormData + Offset);
     LeftChannel := Buffer^;
  end;
end;

{%endregion%}

{%region=====[ TBZSoundSample ]===================================================}

procedure TBZSoundSample.LoadFromFile(Const FileName: String);
Var
  BaseSampleClass: TBZSoundSampleClass;
  tempSample:      TBZCustomSoundSample;

Begin
  //GlobalLogger.LogNotice('TBZCustomSoundSample.LoadFromFile');
  If filename = '' Then exit;
  BaseSampleClass := GetBZSoundFileFormats.FindFromFileName(FixPathDelimiter(filename));
  TempSample := nil;
  tempSample := BaseSampleClass.Create(nil);
  //TempImage.OnProgress := Self.OnProgress;
  //TempImage.OnLoadError := Self.OnLoadError;
  Try
    tempSample.LoadFromFile(fileName);
    Self.Assign(TempSample);
  Finally
    FreeAndNil(tempSample);
  End;
End;

{%endregion}

{%region=====[ TBZSoundFileIO ]===================================================}

constructor TBZSoundFileIO.Create(AOwner: TPersistent);
begin
   inherited Create(AOwner);
end;

destructor TBZSoundFileIO.Destroy;
begin
   inherited Destroy;
end;

procedure TBZSoundFileIO.RaiseInvalidSoundFile(Msg:String);
begin
  raise EBZInvalidSoundFile.Create('Erreur lors du chargement du fichier : '+ExtractFileName(FullFileName)+#13+#10+
                                   'au format '+DataFormatDesc.Name+' - '+DataFormatDesc.Desc+'. Son Invalide .'+#13+#10+
                                   'Message : '+Msg);
End;

{%endregion%}

{%region=====[ TBZSoundFileFormatsList ]==========================================}

destructor TBZSoundFileFormatsList.Destroy;
var
   i : Integer;
begin
   for i:=0 to Count-1 do Dispose(PSoundFileFormat(Items[i]));
   inherited Destroy;
end;

procedure TBZSoundFileFormatsList.Add(const Ext, Desc: String; DescID: Integer; AClass: TBZSoundSampleClass);
var
   newRec: PSoundFileFormat;
begin
   New(newRec);
   with newRec^ do
   begin
      Extension := LowerCase(Ext);
      SoundFileClass := AClass;
      Description := Desc;
      DescResID := DescID;
   end;
   inherited Add(NewRec);
end;

function TBZSoundFileFormatsList.FindExt(Ext: string): TBZSoundSampleClass;
var
   i : Integer;
begin
   Ext := LowerCase(Ext);
   for I := Count-1 downto 0 do with PSoundFileFormat(Items[I])^ do
   begin
      if (Extension = Ext) or ('.'+Extension = Ext) then
      begin
         Result := SoundFileClass;
         Exit;
      end;
   End;
   Result := nil;
end;

Function TBZSoundFileFormatsList.FindFromFileName(Const fileName: String): TBZSoundSampleClass;
Var
  ext: String;
Begin
  ext := ExtractFileExt(Filename);
  System.Delete(ext, 1, 1);
  Result := FindExt(ext);
  If Not Assigned(Result) Then
    Raise EBZInvalidSoundFile.CreateFmt('Format de fichier %s non supporté ', [UpperCase(ext)]);

End;

procedure TBZSoundFileFormatsList.Remove(AClass: TBZSoundSampleClass);
var
   i : Integer;
   p : PSoundFileFormat;
begin
   for I := Count-1 downto 0 do
   begin
      P := PSoundFileFormat(Items[I]);
      if P^.SoundFileClass.InheritsFrom(AClass) then
      begin
         Dispose(P);
         Delete(I);
      end;
   end;
end;

procedure TBZSoundFileFormatsList.BuildFilterStrings(SoundFileClass: TBZSoundSampleClass; out Descriptions, Filters: string);
var
   c, i : Integer;
   p    : PSoundFileFormat;
begin
   Descriptions := '';
   Filters := '';
   C := 0;
   for I := Count-1 downto 0 do
   begin
      P := PSoundFileFormat(Items[I]);
      if P^.SoundFileClass.InheritsFrom(SoundFileClass) and (P^.Extension <> '') then
      begin
         with P^ do
         begin
            if C <> 0 then
            begin
               Descriptions := Descriptions+'|';
               Filters := Filters+';';
            end;
            if (Description = '') and (DescResID <> 0) then Description := LoadStr(DescResID);
            FmtStr(Descriptions, '%s%s (*.%s)|*.%2:s',[Descriptions, Description, Extension]);
            FmtStr(Filters, '%s*.%s', [Filters, Extension]);
            Inc(C);
         end;
      end;
   end;
   if C > 1 then FmtStr(Descriptions, '%s (%s)|%1:s|%s', ['Tous les fichiers', Filters, Descriptions]);
end;

{%endregion%}

Function GetBZSoundFileFormats: TBZSoundFileFormatsList;
Begin
  If Not Assigned(vSoundFileFormats) Then
    vSoundFileFormats := TBZSoundFileFormatsList.Create;
  Result := vSoundFileFormats;
End;

initialization
   vSoundFileFormats := TBZSoundFileFormatsList.Create;

finalization

  FreeAndNil(vSoundFileFormats);

end.

