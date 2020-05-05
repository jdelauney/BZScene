(*
  @abstract(La classe décrite ici, "TBZCustomImageFileIO" est l'ancêtre des classes
  servant à lire ou écrire les différents formats d'image supportés (BMP, TGA, PCX,...).@br
  Elle est un pont direct avec notre gestionnaire de bitmap "TBZCustomBitmap".)

  Cette classe est avant une classe d'aide à la lecture et l'écriture des données. @br
  Elle inclue des fonctions utiles pour la lecture des pixels dans un format donné
  RGB 16bits, RGB 24bits, BGRA,... ; pour créer la palette de couleur issue du fichier.@br
  Ou bien encore pour décompresser, ou décoder les données suivant un schéma fournit.

  Elle inclut également le support d'informations complémentaires lors de la lecture
  d'un fichier. Par exemple les données EXIF des fichiers JPEG.

  -------------------------------------------------------------------------------------------------------------

  @created(2017-05-11)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(11/05/2017 : Creation  )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) : @br

  -------------------------------------------------------------------------------------------------------------

  @bold(Dependances) : BZClasses, BZColors, BZGraphic, BZBitmap

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :) Tous les liens au dessus, dessous, dedans, et au-delà......

  ------------------------------------------------------------------------------@br

  @bold(TODO :)
   - Prise en charge d'une liste de Décodeur/Encodeur (RLE_RAW, RLE_BMP, RLE_TGA, RLE_PCX, LZW, HUF ect...)

  -------------------------------------------------------------------------------------------------------------

  LICENCE : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZImageFileIO;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils,
  BZClasses, BZColors, BZGraphic, BZBitmap{, BZBitmapCompression};

type
  { Exception qui indique un type d'image invalide générale }
  EInvalidImageFile   = Class(EBZBitmapException);
  { Exception qui indique un En-tête Invalide d'image invalide }
  EInvalidImageHeader = Class(EInvalidImageFile);
  { Exception qui indique des Données Invalides dans l'image }
  EInvalidImageData   = Class(EInvalidImageFile);


Type
  { Prise en charge du mode de compression RLE 4, 8, 16, 24, 32 bits }
  TBZRLEOpCode = packed record
    Count, Command : Byte;
  end;
  PBZRLEOpCode = ^TBZRLEOpCode;


Type
  TBZNotifySeverity = (nsInfo, nsWarning, nsError);

  { Classe abstraite pour le support des fichiers image }

  { TBZCustomImageFileIO }

  TBZCustomImageFileIO = Class(TBZCustomBitmap)
  private
    FImageProperties : Pointer;
    FSupportedColorFormat : TBZSupportedColorFormat;
  protected
    //  Encoder : TBZDataEncoderClass;

   (* Procedure LineDecodeRGBPalette4(Buffer, Palette:Pointer; Line:Cardinal);
    Procedure LineDecodeRGBPalette8(Buffer, Palette:Pointer; Line:Cardinal);
    Procedure LineDecodeRGB8(Buffer:Pointer; Line:Cardinal);
    Procedure LineDecodeRGB565(Buffer:Pointer; Line:Cardinal);
    Procedure LineDecodeRGB555(Buffer:Pointer; Line:Cardinal);
    Procedure LineDecodeRGBA555(Buffer:Pointer; Line:Cardinal);
    Procedure LineDecodeRGBA444(Buffer:Pointer; Line:Cardinal);

    Procedure LineDecodeRGB24(Buffer:Pointer; Line:Cardinal);
    Procedure LineDecodeRGB32(Buffer:Pointer; Line:Cardinal);
    Procedure LineDecodeRLE4(Buffer:Pointer; Line:Cardinal);
    Procedure DecodeRLE8(Buffer:Pointer; Line:Cardinal);
    Procedure LineDecodeRLE24(Buffer:Pointer; Line:Cardinal);
    *)
    Procedure FlipX;
    Procedure FlipY;

    procedure LineDecodeXRGB16_1555(Const aBuffer: Pointer;Const aLine:Cardinal);
    procedure LineDecodeXBGR16_1555(Const aBuffer: Pointer;Const aLine:Cardinal);
    procedure LineDecodeRGB24(Const aBuffer: Pointer;Const aLine:Cardinal;Const UseBitFields:Boolean=False);
    procedure LineDecodeBGR24(Const aBuffer: Pointer; Const aLine:Cardinal;Const UseBitFields:Boolean=False);
    procedure LineDecodeRGBA32(Const aBuffer: Pointer; Const aLine:Cardinal;Const UseBitFields:Boolean=False);
    procedure LineDecodeBGRA32(Const aBuffer: Pointer; Const aLine:Cardinal;Const UseBitFields:Boolean=False);
    procedure LineDecodeARGB32(Const aBuffer: Pointer; Const aLine:Cardinal;Const UseBitFields:Boolean=False);
    procedure LineDecodeABGR32(Const aBuffer: Pointer; Const aLine:Cardinal;Const UseBitFields:Boolean=False);


    { Creation d'une palette en noir et blanc
      "WhiteFirst" permet de commencer la palette par la couleur blanche }
    Procedure CreateBWRawPalette(Const WhiteFirst : Boolean=false);

    { CreateGrayRawPalette : Creation d'une palette en 256 niveau de gris
      "WhiteFirst" permet de commencer la palette par la couleur blanche }
    Procedure CreateGrayRawPalette(Const WhiteFirst : Boolean=false);// Const aCount=256);

    { CreateRawPalette : Création d'une palette de couleurs à partir des données fournies
      qui peuvent être sous différents bruts suivant la valeur de "aFormat"
       - entrelacé ou par plan
       - 8 bits ou 16 bits par composant
       - en ordre RGB ou BGR
       - avec 3 ou 4 composants par entrée (RGB/RGBA)

     "aColorCount" détermine le nombre d'entrées de couleurs à créer.
     Si ce nombre n'est pas égal au nombre d'entrées de palette qui
     résulteraient des bits de cible donnés par la résolution de l'échantillon,
     Alors, la palette est ajustée en conséquence pour permettre la conversion
     entre les résolutions.

     Remarques: Pour les formats entrelacés, un seul pointeur doit être transmis
                dans "aBuffer" (seul le premier est utilisé), tandis que pour les
                données par plan, 3 pointeurs minimum sont nécessaires (un pointeur pour chaque plan).
     L'ordre des données est RGB ou BGR ("aSwapRB = True) en mode entrelacé.
     En mode avion, les trois pointeurs nécessaires doivent également être donnés
     de telle sorte que le pointeur vers les composants rouges se trouve dans "aBuffer[0]",
     le pointeur vert dans "aBuffer[1]" et le bleu dans "aBuffer[2]" et Alpha dans "aBuffer[3]".
     Dans ce cas, la conversion BGR->RGB ou vice-versa n'est pas nécessaire.

     NB : Inspiré par GraphicEx }
    procedure CreateRawPalette(Const aBuffer : array of Pointer; aFormat:TBZImageRawPaletteFormat; aColorCount : Cardinal; aSwapRB : Boolean);

    //procedure  LineDecodeRLE(const Buffer:Pointer;nbits:Byte; Line:Cardinal);

    //Procedure DecoreRLE1
    //Procedure DecoreRLE4
    //Procedure DecoreRLE8
    //Procedure DecoreRLE16
    //Procedure DecoreRLE24
    //Procedure DecoreRLE32

    { Extrait les bits 1 à 1 des données Format pf1bit. 8 pixels par Byte }
    function ExtractPixel1Bit(B,Idx:Integer):Byte;
    { Extrait les bits 2 par 2 des données. Format pf2bits. 4 pixels par Byte }
    function ExtractPixel2Bits(B,Idx:Integer):Byte;
    { Extrait les bits 4 par 4 des données. Format pf4bits. 2 pixels par Byte }
    function ExtractPixel4Bits(B,Idx:Integer):Byte;

    procedure SwapRBBuffer(Buf: PBZColor; pixelCount: Integer);

    function ContvertToRGB24(var aBufferSize : Int64) : PBZColor24;

    function ContvertToBGR24(var aBufferSize : Int64) : PBZColorBGR_24;

    { Lecture des proriétés de l'image. Fonction à surcharger obligatoirement }
    function ReadImageProperties:Boolean;Virtual;

    procedure RaiseInvalidImageHeader(Const Msg:String='');
    procedure RaiseInvalidImageData;
    procedure RaiseInvalidImageFile(Msg:String);

    procedure NotifyUser(Msg : String ; Const Severity : TBZNotifySeverity=nsWarning);

    function CheckIfTransparent(Var IgnoreAlpha : Boolean) : Boolean;
    procedure MakeOpaque;

  public
    Constructor Create(AOwner: TPersistent; AWidth, AHeight: integer); override;
    Destructor Destroy; override;

    { Renvoie les propriétés de l'image sous forme de chaine de caratères. Fonction à surcharger }
    function getImagePropertiesAsString : String; Virtual;

    { Charge une image depuis le fichier "FileName" }
    procedure LoadFromFile(Const FileName:String); override;

    { Pointeur vers les propriétés de l'image. Peux avoir un structure différente en fonction du format }
    property ImageProperty : Pointer read  FImageProperties write FImageProperties;
    { Formats de couleur supportés. Variable à défénir par les enfants }
    property SupportedColorFormat : TBZSupportedColorFormat read FSupportedColorFormat write FSupportedColorFormat;

    property FullFileName;
  end;

  TBZImageFileIO = Class(TBZCustomImageFileIO);

implementation

Uses
  LazFileUtils, Dialogs, BZSystem, BZUtils
  {$IFDEF DEBUG}
  , BZLogger
  {$ENDIF};

Const
  { Constantes d'aide pour la conversion entre 8bits et 1/2/4 bits. }
  cBitsMask1: array[0..7] of Byte = ($80, $40, $20, $10, $08, $04, $02, $01);
  cBitsShift1: array[0..7] of Byte = (7, 6, 5, 4, 3, 2, 1, 0);
  cBitsMask2: array[0..3] of Byte = ($C0, $30, $0C, $03);
  cBitsShift2: array[0..3] of Byte = (6, 4, 2, 0);
  cBitsMask4: array[0..1] of Byte = ($F0, $0F);
  cBitsShift4: array[0..1] of Byte = (4, 0);


{%region% =====[ TBZCustomImageFileIO ]========================================}

Constructor TBZCustomImageFileIO.Create(AOwner : TPersistent; AWidth, AHeight : integer);
begin
  Inherited Create(AOwner,AWidth, AHeight);
//  FSupportedColorFormat = [cfRGB, cfRGBA];
  include(FSupportedColorFormat,cfRGB);
  include(FSupportedColorFormat,cfRGBA);
  include(FSupportedColorFormat,cfBGR);
  include(FSupportedColorFormat,cfBGRA);
end;

Destructor TBZCustomImageFileIO.Destroy;
begin
  FSupportedColorFormat := [];
  Inherited Destroy;
end;

Procedure TBZCustomImageFileIO.FlipX;
Var
  pLine1, pLine2: PBZColor;
  c2, c1: TBZColor;
  Y, X:   Longint;
Begin
  For Y := 0 To MaxHeight Do
  Begin
    pLine1 := GetScanLine(Y);
    pLine2 := GetPixelPtr(MaxWidth, Y);
    For X := 0 To CenterX - 1 Do
    Begin
      c1 := pLine1^;
      c2 := pLine2^;
      pLine1^ := c2;
      pLine2^ := c1;
      Inc(pLine1);
      Dec(pLine2);
    End;
  End;
End;

Procedure TBZCustomImageFileIO.FlipY;
Var
  P1, P2, Buff:  Pointer;
  WidthBytes, I: Longint;
Begin
  WidthBytes := Width * 4;
  GetMem(Buff, WidthBytes);
  Try
    // Swap all scanlines of image
    For I := 0 To CenterY Do
    Begin
      P1 := GetScanLine(I);
      P2 := GetScanLine(MaxHeight - I);
      Move(P1^, Buff^, WidthBytes);
      Move(P2^, P1^, WidthBytes);
      Move(Buff^, P2^, WidthBytes);
    End;
  Finally
    FreeMem(Buff);
  End;
End;


procedure TBZCustomImageFileIO.LineDecodeXRGB16_1555(Const aBuffer : Pointer; Const aLine : Cardinal);
var
 X:Integer;
 DstColor: TBZColor;
 SrcPtr : PWord;
 //SrcPtr : PBZColorXRGB16_1555;
 DstLine : PBZColor;
begin

  DstLine:=GetScanLine(aLine);
  DstColor.Alpha:=255;
  SrcPtr := PWord(aBuffer);
  //SrcPtr := PBZColorXRGB16_1555(aBuffer);
  For X:=0 to MaxWidth do
  begin
    DstColor.Red :=  (SrcPtr^  and $7C00) shr 7;
    DstColor.Green := (SrcPtr^ and $3E0) shr 2;
    DstColor.Blue := (SrcPtr^  and $1F) shl 3;
 (*   DstColor.Red :=  SrcPtr^.Red;
    DstColor.Green := SrcPtr^.Green;
    DstColor.Blue := SrcPtr^.Blue; *)
    DstLine^:=DstColor;
    Inc(SrcPtr);
    Inc(DstLine);
  end;
end;

procedure TBZCustomImageFileIO.LineDecodeXBGR16_1555(Const aBuffer : Pointer; Const aLine : Cardinal);
var
 X:Integer;
 DstColor: TBZColor;
 SrcPtr : PBZColorXBGR16_1555;
 DstLine : PBZColor;
begin
  DstLine:=GetScanLine(aLine);
  DstColor.Alpha:=255;
  SrcPtr := PBZColorXBGR16_1555(aBuffer);
  For X:=0 to MaxWidth do
  begin
    DstColor.Red := SrcPtr^.Red;
    DstColor.Green := SrcPtr^.Green;
    DstColor.Blue := SrcPtr^.Blue;
    DstLine^:=DstColor;
    Inc(SrcPtr);
    Inc(DstLine);
  end;
end;

procedure TBZCustomImageFileIO.LineDecodeRGB24(Const aBuffer : Pointer; Const aLine : Cardinal; Const UseBitFields : Boolean);
var
 X:Integer;
 DstColor: TBZColor;
 SrcPtr : PBZColor24;
 SrcColor : TBZColor24;
 DstLine : PBZColor;
begin
  DstLine:=GetScanLine(aLine);
  SrcPtr := PBZColor24(aBuffer);
  X:=MaxWidth;
  While (X>0) do  // Plus performant que For X:=0 to MaxWidth do
  begin
    SrcColor:=SrcPtr^;
    DstColor.Red := SrcColor.Red;
    DstColor.Green := SrcColor.Green;
    DstColor.Blue := SrcColor.Blue;
    DstColor.Alpha:=255;
    if UseBitFields then DstLine^:=ConvertBitFieldsToBZColor(ImageDescription.BitFields,DstColor.AsInteger) else DstLine^:=DstColor;

    Inc(SrcPtr);
    Inc(DstLine);
    Dec(X);
  end;
end;

procedure TBZCustomImageFileIO.LineDecodeBGR24(Const aBuffer : Pointer; Const aLine : Cardinal; Const UseBitFields : Boolean);
var
 X:Integer;
 DstColor: TBZColor;
 SrcPtr : PBZColor24;
 SrcColor : TBZColor24;
 DstLine : PBZColor;
begin
  DstLine:=GetScanLine(aLine);
  SrcPtr := PBZColor24(aBuffer);
  X:=MaxWidth;
  While (X>0) do  // Plus performant que For X:=0 to MaxWidth do
  begin
    SrcColor:=SrcPtr^;
    DstColor.Red := SrcColor.Blue;
    DstColor.Green := SrcColor.Green;
    DstColor.Blue := SrcColor.Red;
    DstColor.Alpha:=255;

    if UseBitFields then DstLine^:=ConvertBitFieldsToBZColor(ImageDescription.BitFields,DstColor.AsInteger) else DstLine^:=DstColor;

    Inc(SrcPtr);
    Inc(DstLine);
    Dec(X);
  end;
end;

procedure TBZCustomImageFileIO.LineDecodeRGBA32(Const aBuffer : Pointer; Const aLine : Cardinal; Const UseBitFields : Boolean);
var
 X:Integer;
 DstColor: TBZColor;
 SrcPtr : PBZColor32;
 SrcColor : TBZColor32;
 DstLine : PBZColor;
begin
  DstLine:=GetScanLine(aLine);

  SrcPtr := PBZColor32(aBuffer);
  For X:=0 to MaxWidth do
  begin
    SrcColor:=SrcPtr^;
    DstColor.Red := SrcColor.Red;
    DstColor.Green := SrcColor.Green;
    DstColor.Blue := SrcColor.Blue;
    DstColor.Alpha:=SrcColor.Alpha;

    if UseBitFields then DstLine^:=ConvertBitFieldsToBZColor(ImageDescription.BitFields,DstColor.AsInteger) else DstLine^:=DstColor;

    Inc(SrcPtr);
    Inc(DstLine);
  end;
end;

procedure TBZCustomImageFileIO.LineDecodeBGRA32(Const aBuffer : Pointer; Const aLine : Cardinal; Const UseBitFields : Boolean);
var
 X:Integer;
 DstColor: TBZColor;
 SrcPtr : PBZColor32;
 SrcColor : TBZColor32;
 DstLine : PBZColor;
begin
  DstLine:=GetScanLine(aLine);

  SrcPtr := PBZColor32(aBuffer);
  For X:=0 to MaxWidth do
  begin
    SrcColor:=SrcPtr^;
    DstColor.Red := SrcColor.Blue;
    DstColor.Green := SrcColor.Green;
    DstColor.Blue := SrcColor.Red;
    DstColor.Alpha:= SrcColor.Alpha;

    if UseBitFields then DstLine^:=ConvertBitFieldsToBZColor(ImageDescription.BitFields,DstColor.AsInteger) else DstLine^:=DstColor;

    Inc(SrcPtr);
    Inc(DstLine);
  end;
end;

procedure TBZCustomImageFileIO.LineDecodeARGB32(Const aBuffer : Pointer; Const aLine : Cardinal; Const UseBitFields : Boolean);
var
 X:Integer;
 DstColor: TBZColor;
 SrcPtr : PBZColor32;
 SrcColor : TBZColor32;
 DstLine : PBZColor;
begin
  DstLine:=GetScanLine(aLine);

  SrcPtr := PBZColor32(aBuffer);
  For X:=0 to MaxWidth do
  begin
    SrcColor:=(SrcPtr+X)^;
    DstColor.Red := SrcColor.Alpha;
    DstColor.Green := SrcColor.Red;
    DstColor.Blue := SrcColor.Green;
    DstColor.Alpha:= SrcColor.Blue;

    if UseBitFields then DstLine^:=ConvertBitFieldsToBZColor(ImageDescription.BitFields,DstColor.AsInteger) else
    DstLine^:=DstColor;

    Inc(DstLine);
  end;
end;

procedure TBZCustomImageFileIO.LineDecodeABGR32(Const aBuffer : Pointer; Const aLine : Cardinal; Const UseBitFields : Boolean);
var
 X:Integer;
 DstColor: TBZColor;
 SrcPtr : PBZColor32;
 SrcColor : TBZColor32;
 DstLine : PBZColor;
begin
  DstLine:=GetScanLine(aLine);

  SrcPtr := PBZColor32(aBuffer);
  For X:=0 to MaxWidth do
  begin
    SrcColor:=(SrcPtr+X)^;
    DstColor.Red := SrcColor.Alpha;
    DstColor.Green := SrcColor.Blue;
    DstColor.Blue := SrcColor.Green;
    DstColor.Alpha:= SrcColor.Red;

    if UseBitFields then DstLine^:=ConvertBitFieldsToBZColor(ImageDescription.BitFields,DstColor.AsInteger) else
    DstLine^:=DstColor;

    Inc(DstLine);
  end;
end;

(* procedure  TBZCustomImageFileIO.LineDecodeRLE(nbits:Byte;Line:Cardinal);
Var
 DstLine : PBZColor;

  procedure DecodeRLE8;
  Var
   row, col: Integer;

   I: Integer;
   OpCode : PBZRLEOpCode;
   Done:Boolean;
   dx, dy, C, data: Byte;
  begin
    Data:=0;
    Row:=Line;
    Col:=0;
    OpCode := PBZRLEOpCode(Buffer);
    Done := false;
    while not Done do
    begin
      if (OpCode^.Count = 0) then
      begin
        // A byte count of zero means that this is a special
        // instruction.
        case (OpCode^.Command) of
          0, 1 : done := true;  // 0 => Fin de ligne, 1 => Fin de l'image
          2: // Lit la position relative
            begin
              dx:=Memory.ReadByte;
              dy:=Memory.ReadByte;
              inc(col, dx);
              dec(row, dy);
            end
         else
         begin
            // On verifie que les données non pas été corrompues
            if (row > MaxHeight) or (col + OpCode^.Command > MaxWidth) then
            begin
              Raise exception.Create('Données compressée RLE corrompue');
              //exit;
            end;

            for i := 0 to OpCode^.Command - 1 do
            begin
               Data:=Memory.ReadByte;
              // DstLine^:=FPalette(Data);
               Inc(DstLine);
              //TBitmapData8(image.Data).RawArray^[row * width + col] := data;
              Inc(col);
            end;
            // C'est un chiffre impaire, alors y a 1 octet de "padding"
            if ((OpCode^.Command mod 2) = 1) then
            begin
              Memory.SkipNextByte;
            end;
          end;
        end;
      end
      else
      begin

        //ImageDescription.
        // On verifie que les données non pas été corrompues
        C:= OpCode^.count;
        if (row > MaxHeight) or (col + c > MaxWidth) then Raise exception.Create('Données compressée RLE corrompue');
        // On rempli notre bitmap et dans ce cas vue qu'il s'agit d'une image indexé nous
        // sauvagrdons également cette valeur
        for i := 0 to OpCode^.count - 1 do
        begin
          //TBitmapData8(image.Data).RawArray^[row * width + col] := opcode.command;
          Inc(col);
        end;
      end;
    end;
  end;


begin
  DstLine:=getScanLine(Line);
  Case nbits of
   4: DecodeRLE4;
   8: DecodeRLE8;
   16: DecodeRLE16;
   24: DecodeRLE24;
   32: DecodeRLE32;
  end;
end; *)

(* var
  I: Integer;
  SourcePtr,
  TargetPtr:  PByte;
  RunLength:  Longword;
  Counter:    Longword;

begin
  Result := 0;
  Counter := 0;
  TargetPtr := @Target;
  SourcePtr := @Source;

  While Counter<MaxWidth do
  Begin
    RunLength := 1 + (SourcePtr^ and $7F);
    if SourcePtr^ > $7F then
    begin
     // Decodage RLE en fonctions du nombre de Bits de la valeur à lire
      Inc(SourcePtr);
      for I := 0 to RunLength - 1 do
      begin
        Case Fetch of
        4:
        begin
        end;
        8:  TargetPtr^:=SourcePtr^;
        16:  PWord(TargetPtr)^:=PWord(SourcePtr)^;
        24:  PJLTriple(TargetPtr)^:=PJLTriple(SourcePtr)^;
        32:  PLongword(TargetPtr)^:=PLongword(SourcePTR)^;
        end;
        Inc(TargetPtr,Fetch);
      end;
      Inc(SourcePtr, Fetch);
      Inc(Result, Fetch + 1);
    end
    else
    begin
      //decode NON-RLE packet
      Inc(SourcePtr);
      Move(SourcePtr^,targetptr^,RunLength);
      inc(targetptr,RunLength);
      inc(sourceptr,RunLength);
      Inc(Result, RunLength + 1)
    end;
    Inc(Counter, RunLength);
  end;
end; *)

function TBZCustomImageFileIO.ExtractPixel1Bit(B, Idx : Integer) : Byte;
begin
  Result := (B and cBitsMask1[Idx and 7]) shr cBitsShift1[Idx and 7];
end;

function TBZCustomImageFileIO.ExtractPixel2Bits(B, Idx : Integer) : Byte;
begin
  Result := (B and cBitsMask2[Idx and 3]) shr cBitsShift2[Idx and 3];
end;

function TBZCustomImageFileIO.ExtractPixel4Bits(B, Idx : Integer) : Byte;
begin
  Result := (B and cBitsMask4[Idx and 1]) shr cBitsShift4[Idx and 1];
end;

procedure TBZCustomImageFileIO.SwapRBBuffer(Buf : PBZColor; pixelCount : Integer);
var
  Pixptr: PBZColor;
  AIntColor : Cardinal;
begin
  PixPtr := Buf;
  while pixelCount > 0 do
  begin
    AIntColor := PixPtr^.AsInteger;
    PixPtr^.AsInteger := AIntColor and $FF00FF00 or (AintColor and $000000FF SHL 16) or (AIntColor and $00FF0000 SHR 16);
    Inc(PixPtr);
    Dec(pixelCount);
  end;
end;

function TBZCustomImageFileIO.ContvertToRGB24(var aBufferSize : Int64) : PBZColor24;
Var
  Buffer, PixBuf : PBZColor24;
  Count : Int64;
  PixPtr : PBZColor;
begin
  aBufferSize := Int64((Int64(Self.Width) * Int64(Self.Height)) * 3);
  Buffer := nil;
  memAlloc(Buffer, aBufferSize);
  PixBuf := Buffer;
  PixPtr:= Self.GetScanLine(0);
  Count := 0;
  While (Count <= Self.MaxSize) do
  begin
    PixBuf^.Red := PixPtr^.Red;
    PixBuf^.Green := PixPtr^.Green;
    PixBuf^.Blue := PixPtr^.Blue;
    inc(PixPtr);
    Inc(PixBuf);
    Inc(Count);
  end;
  Result := Buffer;
end;

function TBZCustomImageFileIO.ContvertToBGR24(var aBufferSize : Int64) : PBZColorBGR_24;
Var
  Buffer, PixBuf : PBZColorBGR_24;
  Count : Int64;
  PixPtr : PBZColor;
begin
  aBufferSize := Int64((Int64(Self.Width) * Int64(Self.Height)) * 3);
  Buffer := nil;
  memAlloc(Buffer, aBufferSize);
  PixBuf := Buffer;
  PixPtr:= Self.GetScanLine(0);
  Count := 0;
  While (Count <= Self.MaxSize) do
  begin
    PixBuf^.Blue := PixPtr^.Blue;
    PixBuf^.Red := PixPtr^.Red;
    PixBuf^.Green := PixPtr^.Green;
    inc(PixPtr);
    Inc(PixBuf);
    Inc(Count);
  end;
  Result := Buffer;
end;

procedure TBZCustomImageFileIO.RaiseInvalidImageHeader(Const Msg : String);
begin
  raise EInvalidImageHeader.Create('En-tête du fichier : '+ExtractFileName(FullFileName)+#13+#10+
                                   'au format : '+DataFormatDesc.Name+' - '+DataFormatDesc.Desc+'. Invalide.'+
                                   #13+#10+Msg);
End;

procedure TBZCustomImageFileIO.RaiseInvalidImageData;
begin
  raise EInvalidImageData.Create('Données du fichier : '+ExtractFileName(FullFileName)+#13+#10+
                                   'au format : '+DataFormatDesc.Name+' - '+DataFormatDesc.Desc+'. Corrompues !');
End;

procedure TBZCustomImageFileIO.RaiseInvalidImageFile(Msg : String);
begin
  raise EInvalidImageData.Create('Erreur lors du chargement du fichier : '+ExtractFileName(FullFileName)+#13+#10+
                                   'au format '+DataFormatDesc.Name+' - '+DataFormatDesc.Desc+#13+#10+
                                   'Message : '+Msg);
End;

procedure TBZCustomImageFileIO.NotifyUser(Msg : String; Const Severity : TBZNotifySeverity);
Begin
  Case Severity of
    nsInfo :
      begin
        MessageDlg(Msg, mtInformation, [mbOK],0);
      End;
    nsWarning :
      begin
        MessageDlg(Msg, mtWarning, [mbOK],0);
      End;
    nsError :
      begin
        RaiseInvalidImageFile(Msg);
      End;
  End;
End;

function TBZCustomImageFileIO.CheckIfTransparent(Var IgnoreAlpha : Boolean) : Boolean;
Var
 PixPtr : PBZColor;
 i : Int64;
 IsTransparent : Boolean;
 SrcColor : TBZColor;
begin
  i := 0;
  PixPtr := Self.GetScanLine(0);
  IgnoreAlpha := True;
  IsTransparent := True;
  While (I<Self.MaxSize) do
  begin
    SrcColor := PixPtr^;
    IgnoreAlpha := IgnoreAlpha and (SrcColor.alpha = 0);
    IsTransparent := IsTransparent and (SrcColor.alpha < 255);
    Inc(PixPtr);
    Inc(I);
  end;
  Result := IsTransparent;
end;

procedure TBZCustomImageFileIO.MakeOpaque;
var
  i : Integer;
  PixPtr : PBZColor;
begin
  i:=0;
  PixPtr := Self.GetScanLine(0);
  While i<Self.MaxSize do
  begin
    PixPtr^.Alpha:= 255;
    inc(PixPtr);
    inc(i);
  end;
end;


function TBZCustomImageFileIO.ReadImageProperties : Boolean;
begin
  result:=true;
end;

function TBZCustomImageFileIO.getImagePropertiesAsString : String;
begin
  result:='';
end;

procedure TBZCustomImageFileIO.LoadFromFile(Const FileName : String);
Var
  fs: TStream;
  fn:String;
Begin
  fn := FixPathDelimiter(filename);
  ResourceName := ExtractFileName(fn);
  FullFileName := Fn;
  {$IFDEF DEBUG}GlobalLogger.LogNotice('TBZCustomDataFile.LoadFromFile : FileName = '+FullFileName);{$ENDIF}
  fs := CreateFileStream(fn, fmOpenRead + fmShareDenyNone);
  Try
     LoadFromStream(fs);
  Finally
    fs.Free;
  End;
end;

(* procedure TBZCustomImageFileIO.LoadFromFile(Const FileName:String);
begin
  inherited LoadFromFile(FileName);
end;    *)

Procedure TBZCustomImageFileIO.CreateBWRawPalette(Const WhiteFirst : Boolean);
begin
  With ImageDescription do
  begin
    PaletteCount := 2;
    if WhiteFirst then
    begin
      PaletteEntries^[0].AsInteger:=clrWhite.AsInteger;
      PaletteEntries^[1].AsInteger:=clrBlack.AsInteger;
    end
    else
    begin
      PaletteEntries^[0].AsInteger:=clrBlack.AsInteger;
      PaletteEntries^[1].AsInteger:=clrWhite.AsInteger;
    end;
  end;
end;

Procedure TBZCustomImageFileIO.CreateGrayRawPalette(Const WhiteFirst : Boolean);
Var
  I,J : Integer;
Begin
  With ImageDescription do
  begin
    PaletteCount := 256;
    For I:=0 To 255 Do
    Begin
      With PaletteEntries^[I] do
      begin
        if WhiteFirst then J:=255-I else J:=I;
        Red:=J;
        Green:=J;
        Blue:=J;
        Alpha:=255;
      end;
    end;
  end;
End;

procedure TBZCustomImageFileIO.CreateRawPalette(Const aBuffer : array of Pointer; aFormat : TBZImageRawPaletteFormat; aColorCount : Cardinal; aSwapRB : Boolean);
Var
  aCount : Cardinal;
  MinIn : Cardinal; //, MaxIn
  I,J:Integer;
  SrcPtrR : PByte;
  bc: byte;
    //SrcPtrG, SrcPtrB, SrcPtrA : PByte;
  //AColor : TBZColor;

begin
  //SHowMessage(ImageDescription.getDescriptionAsString);
(*   SrcPtrR:=nil;
  Case aFormat of
    pfInterlaced8Triple, // 24bits RGB, 8 bits par canal
    pfInterlaced8Quad :  // 32bits RGBA, 8 bits par canal
    Begin
     // SrcPtrR:=aBuffer[0];
      memReAlloc(SrcPtrR,768);
      FillByte(SrcPtrR,768,255);
      // On verifie le nombre de couleur dans les données et par rapport à "aColorCount"
     if aFormat=pfInterlaced8Quad then aCount := aColorCount * 4
      else aCount := := aColorCount * 3;

      //GlobalLogger.LogStatus('CreateRawPalette - aCount / ColorCount = '+InttoStr(aCount)+' / '+InttoStr(aColorCount));
      // La taille est différente
      if aCount<>aColorCount then
      begin
        // Le nombre de couleurs données est différente.
        // On check avec le ImageDescription.BitCount

        if aFormat=pfInterlaced8Quad then MinIn :=  (1 shl 32) - 1
        else MinIn :=  (1 shl 24) - 1;

        //GlobalLogger.LogStatus('CreateRawPalette - MinIn = '+InttoStr(MinIn));
        // Erreur, Trop de couleur dans les données
        if (aCount>MinIn) then
        begin
          ShowMessage('Nombre de couleur dans les donées incorrecte');
          Exit;
        end
        else
          MinIn:=aCount;

        //GlobalLogger.LogStatus('CreateRawPalette - MinIn / ColorCount = '+InttoStr(MinIn)+' / '+InttoStr(aColorCount));
        // Nombre de couleur trop grand, on tronque
        With ImageDescription do
        begin
          if aColorCount > MinIn then PaletteCount:=MinIn   // Sinon avec "ImageDescription.PaletteCount" PAF SIGSEGV :(
          else ImageDescription.PaletteCount:=aColorCount;
        end;
      end
      else ImageDescription.PaletteCount:=aColorCount; // Ok tout va bene



      With ImageDescription do
      begin
        J:=PaletteCount-1; //aColorCount -1;//Pred(ImageDescription.PaletteCount);
        //GlobalLogger.LogStatus('CreateRawPalette -> '+InttoStr(J));
        For i:=0 to J do
        begin
          //GlobalLogger.LogStatus('CreateRawPalette - Color '+InttoStr(I));
          With PaletteEntries^[0] do
          begin
            // On intervertit les canaux rouge et bleu si demandé.
            if aSwapRB then Blue := SrcPtrR^ else Red := SrcPtrR^;
            Inc(SrcPtrR);

            Green := SrcPtrR^;
            Inc(SrcPtrR);

            if aSwapRB then Red := SrcPtrR^ else Blue := SrcPtrR^;
            Inc(SrcPtrR);

            if aFormat=pfInterlaced8Quad then // Format RGBA, on lit le canal Alpha
            begin
              Alpha := SrcPtrR^;
              Inc(SrcPtrR);
            end
            else Alpha := 255;
          end;
        end;
      end;
      FreeMem(SrcPtrR);
    end;
    pfPlane8Triple,      // 3 plans séparés, 8 bits par canal
    pfPlane8Quad :       // 4 plans séparés, 8 bits par canal
    begin

    end;
  end;  *)
end;

{%endregion%}
end.

