(*====< GLSoundFileOGG.pas >===================================================@br
  @created(2016-11-16)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(06/11/2017 : Creation  )
  )
--------------------------------------------------------------------------------@br

  @bold(Description :)@br
    L'unité @bold(GLSoundFileOGG) permet le chargement de fichier musicaux au format
    OGG Vorbis (https://www.xiph.org) par le biais de livrairie externe LibOgg et
    LibVorbis. Une fois chargé le fichier pourra être joué via le "SoundManager" de
    disponible de votre choix

  ------------------------------------------------------------------------------@br

  Notes : @br
   Vous trouverez les librairies nécessaire DLL pour Windows 32bits et 64bits dans le
   dossier "GLScene/Externals"
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
unit BZSoundFileOGG;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\bzscene_options.inc}
//==============================================================================

interface


uses
  Classes, SysUtils,
  BZClasses, BZSoundSample,
  // OGG-Vorbis units
  BZLibOgg,
  BZLibVorbis, ctypes;

Type
   EBZVorbisFileError = Class(EBZInvalidSoundFile);
   EBZVorbisLoadError  = Class(EBZSoundException);

   TBZSoundOGGFormat = class (TBZSoundFileIO)
   private
     FDataByteLength : Int64;
     //FOpenALOGGExtension : Boolean;
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

Constructor TBZSoundOGGFormat.Create(AOwner: TPersistent);
begin
  inherited create(AOwner);
  With DataFormatDesc do
  begin
    Name:='OGG';
    Desc:='OGG Vorbis File';
    FileMask:='*.ogg';
  end;
  FDataByteLength := 0;

  { FOpenALOGGExtension := alIsExtensionPresent('AL_EXT_vorbis');
  oggfile:=TMemoryStream.Create;
  oggfile.LoadFromFile(’boom.ogg’);
  AlBufferData(buffer, AL_FORMAT_VORBIS_EXT, oggfile.Memory, oggfile.Size, 44800);
  oggfile.Free; }

end;

Destructor TBZSoundOGGFormat.Destroy;
begin
  inherited Destroy;
end;

class function TBZSoundOGGFormat.Capabilities : TBZDataFileCapabilities;
begin
   Result:=[dfcRead];
end;

function TBZSoundOGGFormat.CheckFormat(): Boolean;
begin
  Result := True;
End;

{ VorbisDecoder_ callbacks basé sur le code de  Noeska
  (http://www.noeska.com/doal/tutorials.aspx).
}

function VorbisDecoder_read_func(ptr: pointer; size, nmemb: csize_t; datasource: pointer): csize_t; cdecl;
var
  ReadCount: Int64;
begin
  if (size = 0) or (nmemb = 0) or (ptr=nil) then
  begin
    Result := 0;
    Exit;
  end;
  ReadCount := TBZBufferedStream(DataSource).GetStream.Read(ptr^, Size * nmemb);
  Assert(ReadCount mod Size = 0);
  Result := ReadCount div Size;
end;

function VorbisDecoder_seek_func(datasource: pointer; offset: ogg_int64_t; whence: cint): cint; cdecl;
const
  SEEK_SET = 0;
  SEEK_CUR = 1;
  SEEK_END = 2;
begin
  try
    case whence of
      SEEK_CUR:  TBZBufferedStream(DataSource).Seek(offset, soFromCurrent);
      SEEK_END:  TBZBufferedStream(DataSource).Seek(offset, soFromEnd);
      SEEK_SET:  TBZBufferedStream(DataSource).Seek(offset, soFromBeginning);
      else raise EBZSoundException.CreateFmt('Invalid VorbisDecoder_seek_func ' +
        'whence param: %d', [whence]);
    end;
    Result := 0;
  except
    Result := -1;
  end;
end;

function VorbisDecoder_close_func(DataSource: Pointer): CInt; cdecl;
begin
  Result := 0;
end;

function VorbisDecoder_tell_func(DataSource: Pointer): CLong;cdecl;
begin
  Result :=  TBZBufferedStream(DataSource).Position;
end;

function VorbisErrorString(Code : Integer) : String;
begin
  case Code of
   OV_EREAD      : Result := 'Read from Media.';
   OV_ENOTVORBIS : Result := 'Not Vorbis data.';
   OV_EVERSION   : Result := 'Vorbis version mismatch.';
   OV_EBADHEADER : Result := 'Invalid Vorbis header.';
   OV_EFAULT     : Result := 'Internal logic fault (bug or heap/stack corruption.';
  else
   Result := 'Unknown Ogg error.';
  end;
end;

function VorbisDecode(aStream: TBZBufferedStream; out DataFormat: PtrUInt; out Frequency: LongWord): TMemoryStream;

   procedure CheckVorbisFileError(Err: CInt; const Event: string);
   var
     ErrDescription: string;
   begin
     { Liste d'erreurs (http://xiph.org/vorbis/doc/vorbisfile/return.html) }
     case Err of
       OV_FALSE: ErrDescription := 'No data available';
       OV_HOLE: ErrDescription := 'Vorbisfile encountered missing or corrupt data in the bitstream'; {. Recovery is normally automatic and this return code is for informational purposes only. }
       OV_EREAD: ErrDescription := 'Read error while fetching compressed data for decode';
       OV_EFAULT: ErrDescription := 'Internal inconsistency in decode state'; {. Continuing is likely not possible. }
       OV_EIMPL: ErrDescription := 'Feature not implemented';
       OV_EINVAL: ErrDescription := 'Either an invalid argument, or incompletely initialized argument passed to libvorbisfile call';
       OV_ENOTVORBIS: ErrDescription := 'The given file/data was not recognized as Ogg Vorbis data';
       OV_EBADHEADER: ErrDescription := 'The file/data is apparently an Ogg Vorbis stream, but contains a corrupted or undecipherable header';
       OV_EVERSION: ErrDescription := 'The bitstream format revision of the given stream is not supported';
       OV_EBADLINK: ErrDescription := 'The given link exists in the Vorbis data stream, but is not decipherable due to garbacge or corruption';
       OV_ENOSEEK: ErrDescription := 'The given stream is not seekable';
       else ErrDescription := '(unknown vorbisfile error code)';
     end;

     if Err <> 0 then
       raise EBZVorbisFileError.CreateFmt('VorbisFile error %d at "%s": %s',
         [Err, Event, ErrDescription]);
   end;

const
   BufSize = 1024 * 1024;

 var
   OggFile: OggVorbis_File;
   OggInfo: Pvorbis_info;
   Callbacks: ov_callbacks;
   ReadCount: cLong;
   Res:Integer;
   Buffer: Pointer;
   BitStream: cInt;
 begin
   //if not VorbisFileInited then
   //  raise EBZVorbisFileError.Create('vorbisfile library is not available, ' +
   //    'cannot decode OggVorbis file');

   Result := TMemoryStream.Create;
   try
     Callbacks.read := @VorbisDecoder_read_func;
     Callbacks.seek := @VorbisDecoder_seek_func;
     Callbacks.close := @VorbisDecoder_close_func;
     Callbacks.tell := @VorbisDecoder_tell_func;

     Res:=0;
     Buffer := Nil;
     ReallocMem(Buffer,BufSize);
     Res:= ov_open_callbacks(aStream, OggFile,nil,0, Callbacks);
     CheckVorbisFileError(Res,'ov_open_callbacks');
     //Result.WriteBuffer(Buffer^, BufSize);

     OggInfo := ov_info(OggFile, -1);

     if OggInfo^.channels = 1 then DataFormat := AL_FORMAT_MONO16
     else DataFormat := AL_FORMAT_STEREO16;

     Frequency := OggInfo^.rate;

     try
       repeat
//         ov_read(var vf: OggVorbis_File; buffer: pointer; length: cint; bigendianp: cbool; word: cint; sgned: cbool; bitstream: pcint): clong;
         ReadCount := ov_read(OggFile, Buffer, BufSize, False,2, True, @BitStream);
         if ReadCount < 0 then
           CheckVorbisFileError(ReadCount, 'ov_read');
         Result.WriteBuffer(Buffer^, ReadCount);
       until ReadCount <= 0;
     finally
       FreeMem(Buffer);
       Buffer:=nil;
     end;
     ov_clear(OggFile);
   except
     FreeAndNil(Result);
     raise;
   end;
 end;

procedure TBZSoundOGGFormat.LoadFromMemory;
var
  TmpStream : TMemoryStream;
  OggStream : TFileStream;
  Dt : PtrUint;
  Freq : LongWord;
begin
  Try
   // OggStream := TFileStream.Create(Self.FullFileName, fmOpenRead);
   // Self.Memory.
    TmpStream := VorbisDecode(Self.Memory, dt,freq);

    //With Sampling do
    //begin
      Frequency := Freq;
      BitsPerSample :=  16;
      Case dt of
        AL_FORMAT_MONO16 :  NbChannels := 1;
        AL_FORMAT_STEREO16 :  NbChannels := 2;
        else NbChannels := -1;
      End;
   // End;
    FDataByteLength := TmpStream.Size;
    SetSize(FDataByteLength);
    //Reallocmem(FSoundData, FDataByteLength);
    Move(PByte(TmpStream.Memory)^,SoundData^,FDataByteLength);
  Finally
    TmpStream.Free;

  End;
End;


initialization

  RegisterSoundFileFormat('ogg', 'OGG Vorbis files', TBZSoundOGGFormat);

end.
