(*====< BZSoundFileMP3.pas >===================================================@br
  @created(2016-11-16)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(09/11/2017 : Creation  )
  )
--------------------------------------------------------------------------------@br

  @bold(Description :)@br
    L'unité @bold(BZSoundFileMP3) permet le chargement de fichier musicaux au format
    MPEG Layer 3 (MP3) par le biais de librairie externe "mpg123" disponible ici:
    https://www.mpg123.org/
    Une fois chargé le fichier pourra être joué via le "SoundManager" de
    disponible de votre choix

  ------------------------------------------------------------------------------@br

  Notes : @br
    Vous trouverez les librairies nécessaire DLL pour Windows 32bits et 64bits dans le
    dossier "BZScene/Externals"
  ------------------------------------------------------------------------------@br

  Credits :
   @unorderedList(
     @item (Codé sur une base de GLScene http://www.sourceforge.net/glscene)
     @item (En-tête pour la libraire MP3 par DJMaster http://www.djmaster.com/freepascal/bindings/mpg123.php)
   )
   @br
  ------------------------------------------------------------------------------@br
  LICENCE : MPL / GPL @br
  @br
 *==============================================================================*)
unit BZSoundFileMP3;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils,
   BZClasses, BZSoundSample,
  // MP3-123 Lib units
  mpg123, CTypes;

Type
   EBZMP3FileError = Class(EBZInvalidSoundFile);


   TBZSoundMP3Format = class (TBZSoundFileIO)
   private
     FDataByteLength : Int64;
     Fhandle   : Pmpg123_handle;
     Fencoding : Integer;

   protected

     function initmpg123: Boolean;
     function mp3Decode(aStream: TStream): TMemoryStream;

     Function CheckFormat(): Boolean; override;
     Procedure LoadFromMemory; override;

   public
     Constructor Create(AOwner: TPersistent); Override;
     Destructor Destroy; Override;

     class function Capabilities : TBZDataFileCapabilities; override;
   end;

implementation

uses BZLogger, BZStreamClasses, Dialogs;

Constructor TBZSoundMP3Format.Create(AOwner: TPersistent);
begin
  inherited create(AOwner);
  With DataFormatDesc do
  begin
    Name:='MP3';
    Desc:='MP3 File';
    FileMask:='*.mp3';
  end;
  FDataByteLength := 0;
end;

Destructor TBZSoundMP3Format.Destroy;
begin
  inherited Destroy;
end;

class function TBZSoundMP3Format.Capabilities : TBZDataFileCapabilities;
begin
   Result:=[dfcRead];
end;


(* Non utilisé ici permet le chargment d'un mp3 via un callback sur un handle deja disponible ou depuis
un flux internet*

function mp3Decoder_read_func(fd: pointer; buf: pointer; count: csize_t): csize_t; cdecl;
var
  ReadCount: Int64;
begin
  if Count = 0 then
  begin
    Result := 0;
    Exit;
  end;
  ReadCount := TBZBufferedStream(fd).Read(buf^, Count);
  Assert(ReadCount mod Count = 0);
  Result := ReadCount div Count;
end;

function mp3Decoder_seek_func(fd: pointer; count: off_t; offset: cint): off_t; cdecl;
const
  SEEK_SET = 0;
  SEEK_CUR = 1;
  SEEK_END = 2;
begin
  try
    case offset of
      SEEK_CUR:  TBZBufferedStream(fd).Seek(Count, soFromCurrent);
      SEEK_END:  TBZBufferedStream(fd).Seek(Count, soFromEnd);
      SEEK_SET:  TBZBufferedStream(fd).Seek(Count, soFromBeginning);
      else raise EBZSoundException.CreateFmt('Invalid Mp3Decoder_seek_func ' +
        'whence param: %d', [offset]);
    end;
    Result := 0;
  except
    Result := -1;
  end;
end;
*)

{  MP3 basé sur le code de  Noeska
  (http://www.noeska.com/doal/tutorials.aspx).
}
function TBZSoundMP3Format.initmpg123: Boolean;
Var
  Err : CInt;
  Rate, Channels: Integer;
  FrameInfo :  mpg123_frameinfo;
begin
  Result := False;
  Rate := 22005;
  Channels := 2;
  if mpg123_init <> MPG123_OK then Exit;

  Fhandle := mpg123_new('', @Err);
  if Err<>MPG123_OK  then
  begin
    raise EBZSoundException.Create('Invalid MPG123 Initialisation');
  end;
  mpg123_open(Fhandle, PChar(Self.FullFileName));
  mpg123_getformat(Fhandle, @rate, @channels, @Fencoding);
  mpg123_format_none(Fhandle);
  mpg123_format(Fhandle, rate, channels, FEncoding);
  mpg123_info(FHandle,@FrameInfo);
  //With Sampling do
  //begin
    Frequency := rate;
    NbChannels := channels;
    Case FrameInfo.Mode of
      MPG123_M_STEREO, MPG123_M_JOINT, MPG123_M_DUAL : BitsPerSample := 16; //Par default 16bit en MP3
      MPG123_M_MONO  : BitsPerSample := 8;
//    end;

  end;
  Result := True;
end;

function TBZSoundMP3Format.mp3Decode(aStream: TStream): TMemoryStream;
const
   BufSize = 1024 * 1024;
var
  ReadCount: cLong;
  Err:Longint;
  Buffer: Pointer;

  procedure CheckMp3FileError(Err : Longint;event : String);
  Var
    ErrDescription : String;
  begin
     // MPG123_NEW_FORMAT ErrDescription := 'Output format will be different on next call.
     // Note that some libmpg123 versions between 1.4.3 and 1.8.0 insist on you calling mpg123_getformat()
     // after getting this message code. Newer verisons behave like advertised:
     // You have the chance to call mpg123_getformat(),
     // but you can also just continue decoding and get your data.
     if (Err<>MPG123_OK) and (Err<>MPG123_DONE) and (Err<>MPG123_NEW_FORMAT) then
     begin
       case Err of
         MPG123_NEED_MORE : ErrDescription := 'For feed reader: "Feed me more!" (call mpg123_feed() or mpg123_decode() with some new input data).';
         MPG123_ERR : ErrDescription := 'Generic Error';
         MPG123_BAD_OUTFORMAT : ErrDescription := 'Unable to set up output format!';
         MPG123_BAD_CHANNEL : ErrDescription := 'nvalid channel number specified.';
         MPG123_BAD_RATE : ErrDescription := 'Invalid sample rate specified.';
         //MPG123_ERR_16TO8TABL : ErrDescription := 'Unable to allocate memory for 16 to 8 converter table!';
         MPG123_BAD_PARAM : ErrDescription := 'Bad parameter id!';
         MPG123_BAD_BUFFER : ErrDescription := 'Bad buffer given -- invalid pointer or too small size.';
         MPG123_OUT_OF_MEM : ErrDescription := 'Out of memory -- some malloc() failed.';
         MPG123_NOT_INITIALIZED : ErrDescription := 'You didn''t initialize the library!';
         MPG123_BAD_DECODER : ErrDescription := 'Invalid decoder choice.';
         MPG123_BAD_HANDLE  : ErrDescription := 'Invalid mpg123 handle.';
         MPG123_NO_BUFFERS : ErrDescription := 'Unable to initialize frame buffers (out of memory?).';
         MPG123_BAD_RVA : ErrDescription := 'Invalid RVA mode.';
         MPG123_NO_GAPLESS : ErrDescription := 'This build doesn''t support gapless decoding.';
         MPG123_NO_SPACE : ErrDescription := 'Not enough buffer space.';
         MPG123_BAD_TYPES : ErrDescription := 'Incompatible numeric data types.';
         MPG123_BAD_BAND : ErrDescription := 'Bad equalizer band.';
         MPG123_ERR_NULL : ErrDescription := 'Null pointer given where valid storage address needed.';
         MPG123_ERR_READER  : ErrDescription := 'Error reading the stream.';
         MPG123_NO_SEEK_FROM_END : ErrDescription := 'Cannot seek from end (end is not known).';
         MPG123_BAD_WHENCE : ErrDescription := 'Invalid whence for seek function.';
         MPG123_NO_TIMEOUT : ErrDescription := 'Build does not support stream timeouts.';
         MPG123_BAD_FILE : ErrDescription := 'File access error.';
         MPG123_NO_SEEK : ErrDescription := 'Seek not supported by stream.';
         MPG123_NO_READER : ErrDescription := 'No stream opened.';
         MPG123_BAD_PARS : ErrDescription := 'Bad parameter handle.';
         MPG123_BAD_INDEX_PAR : ErrDescription := 'Bad parameters to mpg123_index() and mpg123_set_index().';
         MPG123_OUT_OF_SYNC : ErrDescription := 'Lost track in bytestream and did not try to resync.';
         MPG123_RESYNC_FAIL : ErrDescription := 'Resync failed to find valid MPEG data.';
         MPG123_NO_8BIT : ErrDescription := 'No 8bit encoding possible.';
         MPG123_BAD_ALIGN : ErrDescription := 'Stack aligmnent error.';
         MPG123_NULL_BUFFER : ErrDescription := 'NULL input buffer with non-zero size...';
         MPG123_NO_RELSEEK : ErrDescription := 'Relative seek not possible (screwed up file offset)';
         MPG123_NULL_POINTER : ErrDescription := 'You gave a null pointer somewhere where you shouldn''t have.';
         MPG123_BAD_KEY : ErrDescription := 'Bad key value given.';
         MPG123_NO_INDEX : ErrDescription := 'No frame index in this build.';
         MPG123_INDEX_FAIL : ErrDescription := 'Something with frame index went wrong.';
         MPG123_BAD_DECODER_SETUP : ErrDescription := 'Something prevents a proper decoder setup';
         MPG123_MISSING_FEATURE : ErrDescription := 'This feature has not been built into libmpg123.';
         MPG123_BAD_VALUE : ErrDescription := 'A bad value has been given, somewhere.';
         MPG123_LSEEK_FAILED : ErrDescription := 'Low-level seek failed.';
         MPG123_BAD_CUSTOM_IO : ErrDescription := 'Custom I/O not prepared.';
         MPG123_LFS_OVERFLOW : ErrDescription := 'Offset value overflow during translation of large file API calls -- your client program cannot handle that large file.';
         MPG123_INT_OVERFLOW : ErrDescription := 'Some integer overflow.';
         else ErrDescription := '(unknown LibMPG123 error code)';
       end;

       raise  EBZMP3FileError.CreateFmt('MP3 error %d at "%s": %s', [Err, Event, ErrDescription]);
       Exit;
     end;
  End;

begin
  Result := TMemoryStream.Create;
  try
    Err:=0;
    Buffer := Nil;
    ReallocMem(Buffer,BufSize);
    try
      repeat
        Err  := mpg123_read(FHandle, Buffer,BufSize, @ReadCount);
        CheckMp3FileError(Err,'MP3 Decode');
        Result.WriteBuffer(Buffer^, ReadCount);
      until ReadCount <= 0;
    finally
      FreeMem(Buffer);
      Buffer:=nil;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TBZSoundMP3Format.CheckFormat(): Boolean;
begin
  Result := InitMPG123;
End;

procedure TBZSoundMP3Format.LoadFromMemory;
Var
  TmpStream : TMemoryStream;
begin
  Try
    TmpStream:=mp3Decode(Memory);
    FDataByteLength := TmpStream.Size;
    //Reallocmem(FSoundData, FDataByteLength);
    SetSize(FDataByteLength);
    Move(PByte(TmpStream.Memory)^,SoundData^,FDataByteLength);
  finally
    TmpStream.Free;
  end;
End;

initialization

  RegisterSoundFileFormat('MP3', 'MP3 files', TBZSoundMP3Format);

end.
