(*
  @abstract(Unité regroupant plusieurs classes de gestion d'algorithmes de compressions.)

  Les algorithmes supportés ici s'utilisent principalement avec des données provenent d'images (BMP, TGA, PCX etc...)

     Les méthodes supportées :
     @unorderedlist(
	     @item(RLE_BMP, RLE_TGA, RLA_PCX, RLE_PackBits)
	     @item(LZW, Huffman, thunder)
     )


  --------------------------------------------------------------------------------

  @created(2018-07-11)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(03/08/2017 : Creation)
  )

  --------------------------------------------------------------------------------

  @bold(Notes) : TODO : Prise en charge d'une liste de Décodeur/Encodeur (RLE_RAW, RLE_BMP, RLE_TGA,
              RLE_PCX, RLE_RLA, LZ77, LZW, HUFFMan ect...)

  --------------------------------------------------------------------------------

  @bold(Dependances) : ZBase, BZClasses, BZStreamClasses

  --------------------------------------------------------------------------------

  @bold(Credits :)
   @unorderedList(
     @item(unité à été adpaté de l'unité GraphicCompression.Pas de la librairie GraphicEX)
     @item(J.Delauney (BeanzMaster))
   )

  --------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  -------------------------------------------------------------------------------- *)
unit BZBitmapCompression;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils, Dialogs,
  //ZLib 1.1.2
  ZBase,
  BZClasses, BZStreamClasses;
type
  { Classe décorative pour définir les fonctionnalités
    de base d'un encodeur / décodeur ; compresseur / décompresseur.

    Modifié et adapté de GraphicEx
  }
  TBZCustomBitmapEncoder = class(TBZUpdateAbleObject)
  private
    FDataOwner : TBZCustomDataFile;
  protected
    property Owner : TBZCustomDataFile Read FDataOwner;
  public
    constructor Create(AOwner : TBZCustomDataFile); overload;
    Destructor Destroy; Override;

    { Initialisation du décodage des données }
    procedure DecodeInit; virtual;
    { Décodage des données, renvoie dans "Dest"  les données décodées.
      La lecture des données compressées se fait via la propriété "Memory" du "Owner".
      Ainsi lors de la lecture  a partir d'un flux, la position dans celui-ci est conservé et la
      lecture séquentielle peux se poursuivre. }
    procedure Decode(var Dest: Pointer; UnpackedSize: Integer); virtual;abstract;
    { Finalisation du décodagedes données }
    procedure DecodeEnd; virtual;

    { Initialisation de l'encodage des données }
    procedure EncodeInit; virtual;
    { Encodage des données, renvoie dans "Dest"  les données encodées.
      L'écriture des données se fait via la propriété "Memory" du "Owner".
      Ainsi lors de l'écriture dans le flux, la position dans celui-ci est conservé et la
      l'écriture séquentielle peux se poursuivre. }
    procedure Encode(Source : Pointer; var Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); virtual; abstract;
    { Finalisation de l'encodage des données }
    procedure EncodeEnd; virtual;
  end;
  TBZDataEncoderClass = class of TBZCustomBitmapEncoder;

//  TBZDataEncoderRLE_RAW = class(TBZCustomDataEncoder);

  //TBZDataEncoderRLE_BMP = class(TBZCustomDataEncoder);
  //TBZDataEncoderRLE_TGA = class(TBZCustomDataEncoder);

  { Edncodeur / decodeur PCX }
  TBZDataEncoderRLE_PCX = class(TBZCustomBitmapEncoder)
  public
    procedure Decode(var Dest: Pointer; UnpackedSize: Integer); override;
    procedure Encode(Source : Pointer; var Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  //TBZDataEncoderRLE_BMP = class(TBZCustomDataEncoder);
  //TBZDataEncoderRLE_TGA = class(TBZCustomDataEncoder);

  { Encordeur / Decodeur RLA }
  TBZDataEncoderRLA = class(TBZCustomBitmapEncoder)
  public
    procedure Decode(var Dest: Pointer; UnpackedSize: Integer); override;
    procedure Encode(Source : Pointer; var Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;
  end;

  { Decompression / Compression LZ via la librairie fournie avec FPC "pasZLib" }
  TBZDataEncoderLZ77 = class(TBZCustomBitmapEncoder)
  private
    FBuffer:pointer;
    FFlushMode: Integer; // one of flush constants declard in ZLib.pas
                         // this is usually Z_FINISH for PSP and Z_PARTIAL_FLUSH for PNG
    FAutoReset: Boolean; // TIF, PSP and PNG share this decoder, TIF needs a reset for each
                         // decoder run
    //FDecompressor : TDeCompressionStream;
    //FCompressor : TCompressionStream;
    //FZStream : TBZZLibStream;
    FZStream:z_stream;


    function GetAvailableInput: Integer;
    function GetAvailableOutput: Integer;
  protected
    raw_written,compressed_written: int64;
    raw_read,compressed_read:int64;
  public

    constructor Create(AOwner : TBZCustomDataFile; FlushMode: Integer; AutoReset: Boolean); overload;

    procedure DecodeInit; override;
    procedure Decode(var Dest: Pointer; UnpackedSize: Integer); override;
    procedure DecodeEnd; override;

    procedure Encode(Source : Pointer; var Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal); override;

    property AvailableInput: Integer read GetAvailableInput;
    property AvailableOutput: Integer read GetAvailableOutput;
  end;

  implementation

//uses dialogs;
uses
    // ZLib
    ZDeflate, ZInflate
    {$IFDEF DEBUG}
    ,BZLogger
    {$ENDIF};

const   GZBufsize=1024*64;    {Size of the buffer used for temporarily storing
                              data from the child stream.}

{%region% =====[ TBZCustomBitmapEncoder ]=====================================}

constructor TBZCustomBitmapEncoder.Create(AOwner : TBZCustomDataFile);
begin
  Inherited Create(nil);
  FDataOwner := AOwner;
end;

Destructor TBZCustomBitmapEncoder.Destroy;
begin
  Inherited Destroy;
end;

procedure TBZCustomBitmapEncoder.DecodeInit;
begin
  // Rien à faire ici
end;

procedure TBZCustomBitmapEncoder.DecodeEnd;
begin
  // Rien à faire ici
end;

procedure TBZCustomBitmapEncoder.EncodeInit;
begin
  // Rien à faire ici
end;

procedure TBZCustomBitmapEncoder.EncodeEnd;
begin
 // Rien à faire ici
end;

{%endregion%}

{%region% =====[ TBZDataEncoderRLE_BMP ]======================================}
{%endregion%}

{%region% =====[ TBZDataEncoderRLE_TGA ]======================================}
{%endregion%}

{%region% =====[ TBZDataEncoderRLE_PCX ]======================================}

procedure TBZDataEncoderRLE_PCX.Decode(var Dest: Pointer; UnpackedSize: Integer);
Var
  OwnBitmap : TBZCustomDataFile;  // On a juste besoin de la propriété "Memory" sinon on aurait pu choisir : "TBZBitmap"
  DstPtr : PByte;
  OpCode, Value : Byte;
  Count : Integer;
begin
  // Ici on a 2 solutions pour decoder :
  // 1 : On lit et on transfert direct dans le owner, on verifie avec "packedSize, UnpackedSize" (OwnBitmap := TBZBitmap(Owner);)
  // 2 : On lit et on renvoie le dest suivant "packedSize, UnpackedSize"
  // J'ai choisi la 2 pour garder la main sur le controle des données de sortie

  //GlobalLogger.LogNotice('TBZBitmapPCXImage ----> Decompression des données RLE ');
  OwnBitmap := TBZCustomDataFile(FDataOwner);
  DstPtr := PByte(Dest);
 // UnpackedSize:=UnpackedSize+1;
  while UnpackedSize > 0 do
  begin
    OpCode := OwnBitmap.Memory.ReadByte;
    if (OpCode and $C0) = $C0 then
    begin
      // RLE-Code
      Count := OpCode and $3F;
      if UnpackedSize < Count then Count := UnpackedSize;
      Value := OwnBitmap.Memory.ReadByte;
      FillChar(DstPtr^, Count, Value);
      Inc(DstPtr, Count);
      Dec(UnpackedSize, Count);
    end
    else
    begin
      // not compressed
      DstPtr^ := OpCode;
      Inc(DstPtr);
      Dec(UnpackedSize);
    end;
  end;
end;

procedure TBZDataEncoderRLE_PCX.Encode(Source : Pointer; var Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);
begin

end;

{%endregion%}

{%region% =====[ TBZDataEncoderRLA ]==========================================}

procedure TBZDataEncoderRLA.Decode(var Dest: Pointer; UnpackedSize: Integer);
// decodes a simple run-length encoded strip of size PackedSize
// this is very similar to TPackbitsRLEDecoder

var
  OwnBitmap : TBZCustomDataFile;
  TmpPtr,
  SourcePtr,
  TargetPtr: PByte;
  N: SmallInt;
  B:byte;
  //Counter,
  PackedSize : Integer;
  RLELength: Word;
  //RawBuf:pointer;
begin
  Assert(Dest=nil,'La destination ne peut pas être nulle');
  //GlobalLogger.LogNotice('RLA DECOMPRESSOR');
  OwnBitmap := TBZCustomDataFile(FDataOwner);
  TargetPtr := PByte(Dest);
  //Counter:=0;
  TmpPtr := OwnBitmap.Memory.GetBuffer;
  SourcePtr:=PByte(TmpPtr+OwnBitmap.Memory.position);
  //RawBuf:=nil;
 // RLELength:=OwnBitmap.Memory.ReadWord;
//  RLELength := Swap(RLELength);
//  PackedSize := RLELength;
 // //GlobalLogger.LogStatus('--> RLE LENGTH : '+InttoStr(packedSize));
//  GetMem(RawBuf, RLELength);
//  OwnBitmap.Memory.Read(RawBuf^, RLELength);
//  SourcePtr := PByte(RawBuf);
 (* while PackedSize > 0 do
    begin
      N := ShortInt(SourcePtr^);
      Inc(SourcePtr);
      Dec(PackedSize);

      if N >= 0 then // replicate next Byte N + 1 times
      begin
       // FillChar(TargetPtr^, N + 1, SourcePtr^);

       // B := ;
        FillChar(TargetPtr^, N + 1, SourcePtr^);
        Inc(TargetPtr, N + 1);
        Inc(SourcePtr);
        Dec(PackedSize);
        Inc(Counter,N+1);
      end
      else
      begin // copy next -N bytes literally
        Move(SourcePtr^, TargetPtr^, -N);
        Inc(TargetPtr, -N);
        Inc(SourcePtr, -N);
        Inc(PackedSize, N);
        Inc(Counter,-N);
      end;
      //GlobalLogger.LogStatus('--> Rest of UnPacked : '+InttoStr(packedSize));
      //GlobalLogger.LogStatus('--> UnPackedSize : '+InttoStr(UnpackedSize)+' = '+Inttostr(Counter+1));
    end; *)

  while UnpackedSize > 0 do
  begin
     // //GlobalLogger.LogStatus('--> Rest of UnPacked : '+InttoStr(UnpackedSize));
    B:=SourcePtr^; //OwnBitmap.Memory.ReadByte;
    N := ShortInt(B);
    Inc(SourcePtr);
    Dec(UnPackedSize);
    if N >= 0 then // replicate next Byte N + 1 times
    begin
      B:=SourcePtr^;// OwnBitmap.Memory.ReadByte
      FillByte(TargetPtr^, N + 1, B);
      Inc(TargetPtr, N + 1);
      Inc(SourcePtr);
      Dec(UnPackedSize, N+1);
    end
    else
    begin // copy next -N bytes literally
      N:=Abs(N);
      Dec(SourcePtr,N);
      Move(SourcePtr^, TargetPtr^, N);
      Inc(TargetPtr, N);
      Inc(SourcePtr, N);
      Dec(UnPackedSize, N);
    end;
  end;
//  FreeMem(RawBuf);
  //RawBuf:=nil;
end;

procedure TBZDataEncoderRLA.Encode(Source : Pointer; var Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);
begin

end;

{%endregion%}

{%region% =====[ TBZDataEncoderLZ77 ]=========================================}

constructor TBZDataEncoderLZ77.Create(AOwner : TBZCustomDataFile; FlushMode: Integer; AutoReset: Boolean);
begin
  Inherited Create(AOwner);
  FillChar(FZStream, SizeOf(FZStream), 0);
  FFlushMode := FlushMode;
  FAutoReset := AutoReset;
end;

function TBZDataEncoderLZ77.GetAvailableInput: Integer;
begin
  Result:=FZStream.avail_in;
end;

function TBZDataEncoderLZ77.GetAvailableOutput: Integer;
begin
  result:=FZStream.avail_out;
end;

procedure TBZDataEncoderLZ77.DecodeInit;
begin
   if InflateInit(FZStream) < 0 then ShowMessage('Erreur Init LZ77');
//     CompressionError(gesLZ77Error);
end;

procedure TBZDataEncoderLZ77.Decode(var Dest: Pointer; UnpackedSize: Integer);
var err:smallint;
    lastavail:longint;
begin
(*  FStream.NextIn := Source;
  FStream.AvailIn := PackedSize;

   if FAutoReset then
     FZLibResult := InflateReset(FStream);

   if FZLibResult = Z_OK then
   begin *)

  FZStream.next_out:=@Dest;
  FZStream.avail_out:=UnpackedSize;
  lastavail:=UnpackedSize;
  while FZStream.avail_out<>0 do
  begin
    if FZStream.avail_in=0 then
      begin
        {Refill the buffer.}
        FZStream.next_in:=FBuffer;
        FZStream.avail_in:=Owner.Memory.read(Fbuffer^,GZBufsize);
        inc(compressed_read,FZStream.avail_in);
        inc(raw_read,lastavail-FZStream.avail_out);
        lastavail:=FZStream.avail_out;
       // progress(self);
      end;
    err:=inflate(FZStream,Z_NO_FLUSH);
    if err=Z_STREAM_END then
      break;
    if err<>Z_OK then
      raise EGZDecompressionError.create(zerror(err));
  end;
  if err=Z_STREAM_END then
    dec(compressed_read,FZStream.avail_in);
  inc(raw_read,lastavail-FZStream.avail_out);
//  Result:=UnpackedSize-FZStream.avail_out;

    (* FStream.NextOut := Dest;
     FStream.AvailOut := UnpackedSize;
     FZLibResult := Inflate(FStream, FFlushMode);
     // advance pointers so used input can be calculated
     Source := FStream.NextIn;
     Dest := FStream.NextOut; *)

end;

procedure TBZDataEncoderLZ77.Encode(Source : Pointer; var Dest: Pointer; Count: Cardinal; var BytesStored: Cardinal);
var err:smallint;
    lastavail,
    written:longint;
begin
  FZStream.next_in:=@Dest;
  FZStream.avail_in:=Count;
  lastavail:=count;
  Written:=0;
  while FZStream.avail_in<>0 do
    begin
      if FZStream.avail_out=0 then
        begin
          { Flush the buffer to the stream and update progress }
         // written:=source.write(Source^,Count);
          inc(compressed_written,written);
          inc(raw_written,lastavail-FZStream.avail_in);
          lastavail:=FZStream.avail_in;
         // progress(self);
          { reset output buffer }
         // FZStream.next_out:=Fbuffer;
         // FZStream.avail_out:=GZbufsize;
        end;
      err:=deflate(FZStream,Z_NO_FLUSH);
      if err<>Z_OK then
        raise EGZCompressionError.create(zerror(err));
    end;
  inc(raw_written,lastavail-FZStream.avail_in);
//  Result:=count;
end;

procedure TBZDataEncoderLZ77.DecodeEnd;
begin
   if InflateEnd(FZStream) < 0 then ShowMessage('Erreur Fin LZ77');
     //CompressionError(gesLZ77Error);
end;

{%endregion%}

//---------------------------
// *1 = NDMM : Note De Moi Même
//---------------------------
End.
            
