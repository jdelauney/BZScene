(*====< BZSoundFileWAV.pas >===================================================@br
@created(2016-11-16)
@author(J.Delauney (BeanzMaster))
Historique : @br
@unorderedList(
  @item(09/11/2017 : Creation  )
)
--------------------------------------------------------------------------------@br

@bold(Description :)@br
  L'unité @bold(BZSoundFileWAV) permet le chargement de fichier musicaux au format
  Windows WAV.
  Une fois chargé le fichier pourra être joué via le "SoundManager" de
  disponible de votre choix

------------------------------------------------------------------------------@br

Notes : @br

------------------------------------------------------------------------------@br

Credits :
 @unorderedList(
   @item (Codé sur une base de GLScene http://www.sourceforge.net/glscene)
   @item ()
 )
 @br
------------------------------------------------------------------------------@br
LICENCE : MPL / GPL @br
@br
*==============================================================================*)
unit BZSoundFileWAV;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils,
  BZClasses, BZSoundSample;

type
  TWavHeader = packed record
    RIFFHeader       : array[0..3] of char;
    FileSize         : integer;
    WAVEHeader       : array[0..3] of char;
    FormatHeader     : array[0..3] of char;
    FormatHeaderSize : integer;
    FormatCode       : word;
    ChannelNumber    : word;
    SampleRate       : longword;
    BytesPerSecond   : longword;
    BytesPerSample   : word;
    BitsPerSample    : word;
  end;

  TWavChunk = packed record
    ChunkName        : array[0..3] of char;
    ChunkSize        : integer;
  end;

   { Support for Windows WAV format. }
   TBZSoundWAVFormat = class (TBZSoundFileIO)
   private
     FHeader: TWavHeader;
     FDataStreamPos : int64;
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

uses BZLogger;

Constructor TBZSoundWAVFormat.Create(AOwner: TPersistent);
begin
  inherited create(AOwner);
  With DataFormatDesc do
  begin
    Name:='WAV';
    Desc:='Windows WAV File';
    FileMask:='*.wav';
  end;
  FDataByteLength := 0;
  FDataStreamPos := 0;
end;

Destructor TBZSoundWAVFormat.Destroy;
begin
  inherited Destroy;
end;

class function TBZSoundWAVFormat.Capabilities : TBZDataFileCapabilities;
begin
   Result:=[dfcRead];
end;

function TBZSoundWAVFormat.CheckFormat(): Boolean;
begin
  Memory.ReadBuffer( FHeader, sizeof(TWavHeader));
  Result := False;
  GlobalLogger.LogNotice('Read WAV HEADER Size : '+InttoStr(sizeof(TWavHeader))+' AT '+InttoStr(Memory.Position));

  with FHeader do
  begin
    if ( RIFFHeader[0]<>'R' ) or ( RIFFHeader[1]<>'I' ) or
       ( RIFFHeader[2]<>'F' ) or ( RIFFHeader[3]<>'F' ) then
    begin
       RaiseInvalidSoundFile('En-tête Format WAV Invalide');
       exit;
    end;
    FormatHeaderSize := LEtoN(FHeader.FormatHeaderSize);
    FormatCode := LEtoN(FHeader.FormatCode);
    ChannelNumber := LEtoN(FHeader.ChannelNumber);
    SampleRate := LEtoN(FHeader.SampleRate);
    BytesPerSecond := LEtoN(FHeader.BytesPerSecond);
    BytesPerSample := LEtoN(FHeader.BytesPerSample);
    BitsPerSample := LEtoN(FHeader.BitsPerSample);
    if ( BitsPerSample <> 8 ) and ( BitsPerSample <> 16 )then
    begin
      RaiseInvalidSoundFile('Nombre de Bits par Sample non supporté : '+Inttostr(BitsPerSample));
      exit;
    end;
  End;
  //With Sampling do
  //begin
    Frequency := FHeader.SampleRate;
    BitsPerSample :=  FHeader.BitsPerSample;
    NbChannels := FHeader.ChannelNumber;
  //End;
  // GlobalLogger.LogNotice('Format HEADER Size : '+InttoStr(FHeader.FormatHeaderSize)+' New pos '+InttoStr(Memory.Position-FHeader.FormatHeaderSize));
  // GlobalLogger.LogNotice('New Read  AT '+InttoStr(Memory.Position));
  Result := True;
End;

procedure TBZSoundWAVFormat.LoadFromMemory;
var
  Chunk: TWavChunk;
  DataLoaded : Boolean;
begin
  Memory.seek(-16+FHeader.FormatHeaderSize,soFromCurrent); //hmm crappy...
  Chunk.ChunkName := 'NOPE';
  Chunk.ChunkSize := 0;
  DataLoaded:=False;
  //GlobalLogger.LogNotice('Begin Read Chunk  :  At '+InttoStr(Memory.Position));
  repeat
    Memory.ReadBuffer( Chunk, sizeof (Chunk));  // read chunk header
    Chunk.ChunkSize := LEtoN( Chunk.ChunkSize );
    //GlobalLogger.LogNotice('Load Chunk : '+(Chunk.ChunkName)+' Size : '+InttoStr(Chunk.ChunkSize));
    if Chunk.ChunkName = 'data' then
    begin
      //GlobalLogger.LogNotice('Load Chunk WAV DATA : '+InttoStr(Chunk.ChunkSize)+' At '+InttoStr(Memory.Position));
      FDataByteLength := Chunk.ChunkSize;
      FDataStreamPos := Memory.Position;
      //GlobalLogger.LogNotice('Load WAV DATA : '+InttoStr(FDataByteLength)+' At '+InttoStr(FDataStreamPos));
      SetSize(FDataByteLength);
      //ReallocMem(FSoundData, FDataByteLength);
      Memory.Read(SoundData^,FDataByteLength);
      DataLoaded := True;
      break;
    end else Memory.Seek(Chunk.ChunkSize, soCurrent);
  until Memory.Position >= pred(Memory.Size);
  if not(DataLoaded) then RaiseInvalidSoundFile('Aucune données audio trouvées');
End;


initialization

  RegisterSoundFileFormat('wav', 'Windows WAV files', TBZSoundWAVFormat);

end.
