(*
  @abstract(Prise en charge des Images au format TIFF en lecture)

  Spécifications :
  @unorderedList(
    @item(Méthode de compression     :
    @item(Nombre de couleurs	       :
    @item(Supporte plusieurs images  :
    @item(Format des nombres	       :
    @item(Auteur	                   :
    @item(Extensons                  : *.tif
  )

  -------------------------------------------------------------------------------------------------------------

  @created(2017-04-30)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(30/04/2017 : Creation  )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) :@br
    Informations sur le format TIFF :
    @unorderedList(
       @item(https://wiki.fileformat.com/image/tiff/)
       @item(https://www.loc.gov/preservation/digital/formats/fdd/fdd000022.shtml)
       @item(https://www.adobe.io/content/dam/udp/en/open/standards/tiff/TIFF6.pdf)
       @item(https://www.adobe.io/content/dam/udp/en/open/standards/tiff/TIFFPM6.pdf)
       @item(http://paulbourke.net/dataformats/tiff/tiff_summary.pdf)
       @item(http://www.fileformat.info/format/tiff/corion.htm)
       @item(http://www.fileformat.info/format/tiff/egff.htm)
       @item(https://www.loc.gov/preservation/digital/formats/content/tiff_tags.shtml)
    )

    Encodage/Decodage : LZW / Thunder / CITT / Zip / Jpeg

    Autres informations utiles :
         Fichiers test : http

  -------------------------------------------------------------------------------------------------------------

  @bold(Dépendances) : BZClasses, BZColors, BZGraphic, BZBitmap, BZImageFileIO, BZImageStrConsts, BZUtils

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits) :
    Quelques parties, proviennent de GraphicEx par Mike Lischke et d'autres provenant d'obscures sources trouvées sur le web. @br


  @bold(Contributeurs) :
  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZImageFileTIFF;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils,
  BZClasses, BZGraphic, BZBitmap, BZImageFileIO;


Const
  cTIFFByteOrderLittle : word = $4949;   // II
  cTIFFByteOrderBig    : word = $4D4D;   // MM
  cTIFFMagic = $002A; //42;

  cTIFF_FIELDTYPE_BYTE     = 1; // 8-bit unsigned integer
  cTIFF_FIELDTYPE_ASCII    = 2; // 8-bit byte that contains a 7-bit ASCII code; the last byte must be NUL (binary zero).
  cTIFF_FIELDTYPE_SHORT    = 3; // 16-bit (2-byte) unsigned
  cTIFF_FIELDTYPE_LONG     = 4; // 32-bit (4-byte) unsigned
  cTIFF_FIELDTYPE_RATIONAL = 5; // Two LONGs: the first represents the numerator of a fraction; the second, the denominator.

  cTIFF_FIELDTYPE_SBYTE      = 6;  // 8-bit signed
  cTIFF_FIELDTYPE_UNDEF      = 7;  // 8-bit byte that may contain anything, depending on the definition of the field
  cTIFF_FIELDTYPE_SSHORT     = 8;  // 16-bit (2-byte) signed
  cTIFF_FIELDTYPE_SLONG      = 9;  // 32-bit (4-byte) signed
  cTIFF_FIELDTYPE_SRATIONAL  = 10; // Two SLONG’s: the first represents the numerator of a fraction, the second the denominator.
  cTIFF_FIELDTYPE_FLOAT      = 11; // Single precision (4-byte)
  cTIFF_FIELDTYPE_DOUBLE     = 12; // Double precision (8-byte)

  cTIFF_TAG_Photometric = 252;
    cTIFF_TAG_PHOTOMETRIC_WhiteIsZero = 0;
    cTIFF_TAG_PHOTOMETRIC_BlackIsZero = 1;
    cTIFF_TAG_PHOTOMETRIC_RGB         = 2;
    cTIFF_TAG_PHOTOMETRIC_Indexed     = 3;
    cTIFF_TAG_PHOTOMETRIC_AlphaMask   = 4;
    cTIFF_TAG_PHOTOMETRIC_CMYK        = 5;
    cTIFF_TAG_PHOTOMETRIC_YCbCr       = 6;
    cTIFF_TAG_PHOTOMETRIC_CIELab      = 8;
    cTIFF_TAG_PHOTOMETRIC_ITULab      = 10;
    cTIFF_TAG_PHOTOMETRIC_LOGL        = 32844;
    cTIFF_TAG_PHOTOMETRIC_LOGLUV      = 32845;

  cTIFF_TAG_WhitePoint            = 318;
  cTIFF_TAG_PrimaryChromaticities = 319;
  cTIFF_TAG_TransferFunction      = 301;
  cTIFF_TAG_TransferRange         = 342;
  cTIFF_TAG_ReferenceBlackWhite   = 532;
  cTIFF_TAG_YCbCrCoeffs           = 529;
  cTIFF_TAG_YCbCrSubSampling      = 530;
  cTIFF_TAG_YCbCrPositioning      = 531;

  cTIFF_TAG_JPEGProc                    = 512;
    cTIFF_JPEGPROC_BaseLine           = 1;             // baseline sequential
    cTIFF_JPEGPROC_LossLess           = 14;            // Huffman coded lossless
  cTIFF_TAG_JPEGInterchangeFormat       = 513;
  cTIFF_TAG_JPEGInterchangeFormatLength = 514;
  cTIFF_TAG_JPEGRestartInterval         = 515;
  cTIFF_TAG_JPEGLosslessPredictors      = 517;
  cTIFF_TAG_JPEGPointTransform          = 518;
  cTIFF_TAG_JPEGQTables                 = 519;
  cTIFF_TAG_JPEGDCTables                = 520;
  cTIFF_TAG_JPEGACTables                = 521;

  cTIFF_TAG_Compression            = 259;
    cTIFF_COMPRESSION_None         = 1;
    cTIFF_COMPRESSION_Uncompressed = 32771;
    cTIFF_COMPRESSION_CCITTRLE     = 2;
    cTIFF_COMPRESSION_CCITTFAX3    = 3;
    cTIFF_COMPRESSION_CCITTFAX4    = 4;
    cTIFF_COMPRESSION_LZW          = 5;
    cTIFF_COMPRESSION_OldJPEG      = 6;
    cTIFF_COMPRESSION_NewJPEG      = 7;
    cTIFF_COMPRESSION_Deflate      = 8;
    cTIFF_COMPRESSION_JBIGBW       = 9;
    cTIFF_COMPRESSION_JBIGCol      = 10;
    cTIFF_COMPRESSION_NeXT         = 32766;
    cTIFF_COMPRESSION_CCITTRLEW    = 32771;
    cTIFF_COMPRESSION_PackBits     = 32773;
    cTIFF_COMPRESSION_ThunderScan  = 32809;
    cTIFF_COMPRESSION_IT8CTPAD     = 32895;
    cTIFF_COMPRESSION_IT8LW        = 32896;
    cTIFF_COMPRESSION_IT8MP        = 32897;
    cTIFF_COMPRESSION_IT8BL        = 32898;
    cTIFF_COMPRESSION_PixarFilm    = 32908;
    cTIFF_COMPRESSION_PixarLog     = 32909;
    cTIFF_COMPRESSION_DeflateZLib  = 32946;
    cTIFF_COMPRESSION_DCS          = 32947;
    cTIFF_COMPRESSION_JBIG         = 34661;
    cTIFF_COMPRESSION_SGILog       = 34676;
    cTIFF_COMPRESSION_SGILog24     = 34677;
    cTIFF_COMPRESSION_JPEG2000     = 34712;

  cTIFF_TAG_T4Options = 292;
    cT4Options_2DENCODING        = $1;            // 2-dimensional coding
    cT4Options_UNCOMPRESSED      = $2;            // data not compressed
    cT4Options_FILLBITS          = $4;            // fill to byte boundary

  cTIFF_TAG_T6Options = 293;
    cT6Options_UNCOMPRESSED      = $2;            // data not compressed

  cTIFF_TAG_Predictor = 317;
    cTIFF_PREDICTION_None = 1;            // no prediction scheme used before coding
    cTIFF_PREDICTION_Diff = 2;            // horizontal differencing prediction

  cTIFF_TAG_ImageWidth                = 256;
  cTIFF_TAG_ImageHeight               = 257;

  cTIFF_TAG_ResolutionUnit            = 296;
    cTIFF_RESOLUTIONUNIT_NoAbsolute = 0;
    cTIFF_RESOLUTIONUNIT_Inch       = 1;
    cTIFF_RESOLUTIONUNIT_Centimeter = 2;

  cTIFF_TAG_XResolution = 282;
  cTIFF_TAG_YResolution = 283;

  cTIFF_TAG_XPosition = 286;
  cTIFF_TAG_YPosition = 287;

  cTIFF_TAG_TileWidth      = 322;
  cTIFF_TAG_TileHeight     = 323;
  cTIFF_TAG_TileOffsets    = 324;
  cTIFF_TAG_TileByteCounts = 325;

  cTIFF_TAG_InkSet        = 332;
  cTIFF_TAG_InkName       = 333;
  cTIFF_TAG_NumberOfInks  = 334;
  cTIFF_TAG_DotRange      = 336;
  cTIFF_TAG_TargetPrinter = 337;

  cTIFF_TAG_HalfToneHints = 321;

  cTIFF_TAG_Orientation = 274;
    cTIFF_ORIENTATION_TopLeft     = 1;
    cTIFF_ORIENTATION_TopRight    = 2;
    cTIFF_ORIENTATION_BottomRight = 3;
    cTIFF_ORIENTATION_BottomLeft  = 4;
    cTIFF_ORIENTATION_LeftTop     = 5;
    cTIFF_ORIENTATION_RightTop    = 6;
    cTIFF_ORIENTATION_RightBottom = 7;
    cTIFF_ORIENTATION_LeftBottom  = 8;

  cTIFF_TAG_PlanarConfiguration = 284;
    cTIFF_PLANARCFG_Chunky = 1;
    cTIFF_PLANARCFG_Planar = 2;

  cTIFF_TAG_RowPerStrip     = 278;
  cTIFF_TAG_StripOffsets    = 273;
  cTIFF_TAG_StripByteCounts = 279;

  cTIFF_TAG_BitsPerSample  = 258;
  cTIFF_TAG_SamplePerPixel = 277;
  cTIFF_TAG_MinSampleValue = 280;
  cTIFF_TAG_MaxSampleValue = 281;

  cTIFF_TAG_ExtraSamples =  338;
    cTIFF_EXTRASAMPLES_Undef             = 0;
    cTIFF_EXTRASAMPLES_AssociatedAlpha   = 1;  // Associated alpha data (with pre-multiplied color)
    cTIFF_EXTRASAMPLES_UnAssociatedAlpha = 2;

  cTIFF_TAG_SampleFormat = 339;
    cTIFF_SAMPLEFORMAT_UnSigned  = 1;
    cTIFF_SAMPLEFORMAT_TwoSigned = 2;
    cTIFF_SAMPLEFORMAT_IEEE      = 3;
    cTIFF_SAMPLEFORMAT_Undef     = 4;

  cTIFF_TAG_SMinSampleValue = 340;
  cTIFF_TAG_SMaxSampleValue = 341;

  cTIFF_TAG_ColorMap       = 320;

  cTIFF_TAG_FillOrder = 266;
    cTIFF_FILLORDER_HIGHTER = 1;
    cTIFF_FILLORDER_LOWER   = 2;

  cTIFF_TAG_FreeOffsets   = 288;
  cTIFF_TAG_FreeByteCount = 289;

  cTIFF_TAG_GrayResponseUnit  = 290;
    cTIFF_GRAYRESPONSEUNIT_Tenghs             = 1; //10x
    cTIFF_GRAYRESPONSEUNIT_Hundredths         = 2; //100x
    cTIFF_GRAYRESPONSEUNIT_Thousandths        = 3; //1000x
    cTIFF_GRAYRESPONSEUNIT_TenThousandths     = 4; //10000x
    cTIFF_GRAYRESPONSEUNIT_HundredThousandths = 5; //100000x

  cTIFF_TAG_GrayResponseCurve = 291;

  cTIFF_TAG_NewSubFileType = 254;
  cTIFF_TAG_SubFileType    = 255;
    cTIFF_SUBFILETYPE_Full       = 1;
    cTIFF_SUBFILETYPE_Reduce     = 2;
    cTIFF_SUBFILETYPE_SinglePage = 3;

  cTIFF_TAG_ThreshHolding = 263;
    cTIFF_THRESHHOLDING_NONE    = 1;
    cTIFF_THRESHHOLDING_ORDERED = 2;
    cTIFF_THRESHHOLDING_ERROR   = 3;

  cTIFF_TAG_CellWidth  = 264;
  cTIFF_TAG_CellHeight = 265;

  cTIFF_EXIF_TAG_Artist           = 315;
  cTIFF_EXIF_TAG_Copyright        = 33432;
  cTIFF_EXIF_TAG_DateTime         = 306;
  cTIFF_EXIF_TAG_HostComputer     = 316;
  cTIFF_EXIF_TAG_ImageDescription = 270;
  cTIFF_EXIF_TAG_ScanVendor       = 271;
  cTIFF_EXIF_TAG_ScanModel        = 272;
  cTIFF_EXIF_TAG_Software         = 305;
  cTIFF_EXIF_TAG_DocumentName     = 269;
  cTIFF_EXIF_TAG_PageName         = 285;
  cTIFF_EXIF_TAG_PageNumber       = 297;

Type
  TBZTIFF_FileHeader = record
    ByteOrder : Word;
    Magic     : Word;
    FIDOffset : LongWord; //DWord
  end;

  TBZTIFF_IFDEntry = record
    TagID    : Word;
    DataType : Word;
    DataSize : LongWord;
    Offset   : LongWord;
  end;


  PBZTIFF_IFDEntry = ^TBZTIFF_IFDEntry;
  //TBZTiffListTag = array of TBZTIFF_IFDEntry;
  //PBZTiffListTag = ^TBZTiffListTag;

	// StripOffsets and StripByteCounts Information List
	TBZTiffStripInfo=record
    StripOffsets    : LongInt;
    StripByteCounts : LongInt;
  end;
  PBZTiffStripInfo = ^TBZTiffStripInfo;
  TBZTiffListStripInfo = array of TBZTiffStripInfo;
  PBZTiffListStripInfo = ^TBZTiffListStripInfo;


  TBZTIFFDataType = (
    TIFF_NOTYPE,    // 0, placeholder
    TIFF_BYTE,      // 1, 8-bit unsigned integer
    TIFF_ASCII,     // 2, 8-bit bytes w/ last byte null
    TIFF_SHORT,     // 3, 16-bit unsigned integer
    TIFF_LONG,      // 4, 32-bit unsigned integer
    TIFF_RATIONAL,  // 5, 64-bit unsigned fraction
    TIFF_SBYTE,     // 6, !8-bit signed integer
    TIFF_UNDEFINED, // 7, !8-bit untyped data
    TIFF_SSHORT,    // 8, !16-bit signed integer
    TIFF_SLONG,     // 9. !32-bit signed integer
    TIFF_SRATIONAL, // 10, !64-bit signed fraction
    TIFF_FLOAT,     // 11, !32-bit IEEE floating point
    TIFF_DOUBLE     // 12, !64-bit IEEE floating point
  );

  TBZBitmapTIFFImage = Class(TBZCustomImageFileIO)
  private
    FHeader : TBZTIFF_FileHeader;
    FBmpWidth, FBmpHeight : Integer;
    FLittleEndian : Boolean;
    FBasePosition : Int64;
    FIFD : array of TBZTIFF_IFDEntry;  //TBZTiffListTag;
    FImgWidth, FImgHeight : Longword;

    FTileWidth, FTileHeight : Longword;
    FOffet, FTileOffset, FStripOffset : LongWord;
    FIsTiled : Boolean;
    FColorSpace : Word;
    FHasAlpha : Boolean;

    FSamplePerPixel : Byte;
    FBitsPerSample : Byte;

    FPalette: TTIFFPalette;

    FYCbCrPositioning: Cardinal;
    FYCbCrCoefficients: TFloatArray;
    FYCbCrSubSampling: TByteArray;

    FPredictor: Integer;
    FRowsPerStrip: TCardinalArray;

    FJPEGTables: TByteArray;
    FJPEGColorMode: Cardinal;
    FJPEGTablesMode: Cardinal;
    FJPEGOffset: Cardinal;
    FJPEGSize: Cardinal;
    FJPEGProc: Word; //1 for baseline, 14 for lossless

    FPlanarConfig: Cardinal;

    //QTables: TCardinalArray;
    //HuffDCTables: TCardinalArray;
    //HuffACTables: TCardinalArray;

    function FixEndian(AValue: Word): Word; overload;
    function FixEndian(AValue: LongWord): LongWord; overload;
    function FixEndian(AValue: QWord): QWord; overload;

    function  FindTag(TagID: word; var Index: Cardinal): Boolean;
    procedure SwapIFD;
    procedure SortIFD;

    procedure GetTagValueList(TagID : word; var Values: TByteArray); overload;
    procedure GetTagValueList(TagID : word; var Values: TCardinalArray); overload;
    procedure GetTagValueList(TagID : word; var Values: TSingleArray); overload;

    function GetTagValue(TagID: Word; Default: Single = 0): Single; overload;
    function GetTagValue(TagID : word; Default: Cardinal = 0): Cardinal; overload;
    function GetTagValue(TagID : word; var DataSize: Cardinal; Default: Cardinal): Cardinal; overload;

  protected


    Procedure LoadFromMemory(); override;
    Procedure SaveToMemory; override;

    Function CheckFormat(): Boolean; override;
    Function ReadImageProperties: Boolean; override;


  public

    Constructor Create(AOwner: TPersistent; AWidth, AHeight: Integer); override;
    Destructor Destroy; override;

    Class Function Capabilities: TBZDataFileCapabilities; override;

    //Function getImagePropertiesAsString: String; override;


  end;

implementation

uses
  BZImageStrConsts,
  BZUtils;

type
  TCardinalArray = array of cardinal;
  TByteArray = array of byte;
  TFloatArray = array of single;
  TTIFFPalette = array[0..787] of Word;


procedure Depredict1(P: Pointer; Count: Cardinal);
var Run: PByte absolute P;
    Val: Byte;
begin
  while Count > 0 do begin
    Val := Run^;
    inc(Run);
    inc(Run^, Val);
    dec(Count);
  end;
end;

procedure Depredict3(P: Pointer; Count: Cardinal);
var Src: PByte absolute P;
    Dest: PByte;
begin
  Dest := Src;
  inc(Dest, 3);
  Count := Count * 3;
  while count > 0 do begin
    inc(Dest^, Src^);
    inc(Dest);
    inc(Src);
    dec(Count);
  end;
end;

procedure Depredict4(P: Pointer; Count: Cardinal);
var Src: PByte absolute P;
    Dest: PByte;
begin
  Dest := Src;
  inc(Dest, 4);
  Count := Count * 4;
  while count > 0 do begin
    inc(Dest^, Src^);
    inc(Dest);
    inc(Src);
    dec(Count);
  end;
end;

{ TBZBitmapTIFFImage }

Constructor TBZBitmapTIFFImage.Create(AOwner: TPersistent; AWidth, AHeight: Integer);
Begin
  Inherited Create(AOwner, AWidth, AHeight);
  With DataFormatDesc Do
  Begin
    Name := 'TIFF';
    Desc := 'Tagged Image File Format';
    FileMask := '*.tif; *.tiff';
    Version := '';
    Encoding := etNone;
  End;

  SupportedColorFormat := SupportedColorFormat + [cfBGR, cfRGB, cfRGBA, cfBGRA];
End;

Destructor TBZBitmapTIFFImage.Destroy;
Begin
  SupportedColorFormat := [];
  Inherited Destroy;
End;

Procedure TBZBitmapTIFFImage.LoadFromMemory();
begin
  //if Ok then FreeMem(TmpBuf);
end;

Procedure TBZBitmapTIFFImage.SaveToMemory;
begin
  inherited SaveToMemory;
end;

function TBZBitmapTIFFImage.FixEndian(AValue: Word): Word;
begin
  If FLittleEndian then
    result := LEtoN(AValue)
  else
    result := BEtoN(AValue);
end;

function TBZBitmapTIFFImage.FixEndian(AValue: LongWord): LongWord;
begin
  If FLittleEndian then
    result := LEtoN(AValue)
  else
    result := BEtoN(AValue);
end;

function TBZBitmapTIFFImage.FixEndian(AValue: QWord): QWord;
begin
  If FLittleEndian then
    result := LEtoN(AValue)
  else
    result := BEtoN(AValue);
end;

function TBZBitmapTIFFImage.FindTag(TagID: word; var Index: Cardinal): Boolean;
// recherche dans l'IFD actuellement chargé indiquée par TagID;
// renvoie True et l'index de l'entrée dans Index si l'entrée est présent
// sinon le résultat est False et Index -1
// Remarque: l'IFD est trié avant, afin que nous puissions utiliser une recherche binaire.
var
  L, H, I, C: Integer;

begin
  Result := False;
  L := 0;
  H := High(FIFD);
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Integer(FIFD[I].TagID) - Integer(TagID);
    if C < 0 then  L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

procedure TBZBitmapTIFFImage.SwapIFD;
// swap the member fields of all entries of the currently loaded IFD from big endian to little endian
var
  I: Integer;
  Size: Cardinal;
begin
  for I := 0 to High(FIFD) do
  begin
    with FIFD[I] do
    begin
      TagID := FixEndian(Tag);
      DataType := FixEndian(DataType);
      DataSize := FixEndian(DataLength);
      Offset := FixEndian(Offset);
    end;
  end;
end;

procedure TBZBitmapTIFFImage.SortIFD;
// Bien que toutes les entrées de l'IFD doivent être triées,
// il existe encore des fichiers où ce n'est pas le cas.
// Parce que la recherche de certaines balises dans l'IFD utilise la recherche binaire,
// il faut s'assurer que l'IFD soit trié.

  procedure QuickSort(L, R: Integer);
  var
    I, J, M: Integer;
    T: TBZTIFF_IFDEntry;

  begin
    repeat
      I := L;
      J := R;
      M := (L + R) shr 1;
      repeat
        while FIFD[I].TagID < FIFD[M].TagID do Inc(I);
        while FIFD[J].TagID > FIFD[M].TagID do Dec(J);
        if I <= J then
        begin
          T := FIFD[I];
          FIFD[I] := FIFD[J];
          FIFD[J] := T;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then QuickSort(L, J);
      L := I;
    until I >= R;
  end;

begin
  QuickSort(0, High(FIFD));
end;

const
  DataTypeToSize: array[TIFF_NOTYPE..TIFF_SLONG] of Byte = (0, 1, 1, 2, 4, 8, 1, 1, 2, 4);

procedure TBZBitmapTIFFImage.GetValueList(TagID : Cardinal; var Values: TByteArray);
var
  Index,
  Value,
  Shift: Cardinal;
  I: Integer;

begin
  Values := nil;
  if FindTag(Tag, Index) and (FIFD[Index].DataSize > 0) then
  begin
    // prepare value list
    SetLength(Values, FIFD[Index].DataLength);

    // determine whether the data fits into 4 bytes
    Value := DataTypeToSize[TTiffDataType(FIFD[Index].DataType)] * FIFD[Index].DataSize;

    // data fits into one cardinal -> extract it
    if Value <= 4 then
    begin
      Shift := DataTypeToSize[TTiffDataType(FIFD[Index].DataType)] * 8;
      Value := FIFD[Index].Offset;
      for I := 0 to FIFD[Index].DataLength - 1 do
      begin
        case TTiffDataType(FIFD[Index].DataType) of
          TIFF_BYTE:
            Values[I] := Byte(Value);
          TIFF_SHORT,
          TIFF_SSHORT:
            // no byte swap needed here because values in the IFD are already swapped
            // (if necessary at all)
            Values[I] := Word(Value);
          TIFF_LONG,
          TIFF_SLONG:
            Values[I] := Value;
        end;
        Value := Value shr Shift;
      end;
    end
    else
    begin
      // data of this tag does not fit into one 32 bits value
      Stream.Position := FBasePosition + FIFD[Index].Offset;
      // bytes sized data can be read directly instead of looping through the array
      if TTiffDataType(FIFD[Index].DataType) in [TIFF_BYTE, TIFF_ASCII, TIFF_SBYTE, TIFF_UNDEFINED] then
        Memory.ReadBuffer(Values[0], Value)
      else
      begin
        for I := 0 to High(Values) do
        begin
          //Value := Memory.Re
          //Memory.ReadBuffer(Value, DataTypeToSize[TTiffDataType(FIFD[Index].DataType)]);
          case TTiffDataType(FIFD[Index].DataType) of
            TIFF_BYTE : Value := Memory.ReadByte; //Byte(Value);
            TIFF_SHORT,
            TIFF_SSHORT : Value := FixEndian(Memory.ReadWord); //FixEndian(Word(Value));
            TIFF_LONG,
            TIFF_SLONG : Value := FixEndian(Memory.ReadCardinal); //FixEndian(Value);
          end;
          Values[I] := Value;
        end;
      end;
    end;
  end;
end;

procedure TBZBitmapTIFFImage.GetTagValueList(TagID : word; var Values: TCardinalArray);
var
  Index,
  Value,
  Shift: Cardinal;
  I: Integer;

begin
  Values := nil;
  if FindTag(Tag, Index) and (FIFD[Index].DataSize > 0) then
  begin
    // prepare value list
    SetLength(Values, FIFD[Index].DataLength);

    // determine whether the data fits into 4 bytes
    Value := DataTypeToSize[TTiffDataType(FIFD[Index].DataType)] * FIFD[Index].DataSize;

    // data fits into one cardinal -> extract it
    if Value <= 4 then
    begin
      Shift := DataTypeToSize[TTiffDataType(FIFD[Index].DataType)] * 8;
      Value := FIFD[Index].Offset;
      for I := 0 to FIFD[Index].DataSize - 1 do
      begin
        case TTiffDataType(FIFD[Index].DataType) of
          TIFF_BYTE,
          TIFF_ASCII,
          TIFF_SBYTE,
          TIFF_UNDEFINED:
            Values[I] := Byte(Value);
          TIFF_SHORT,
          TIFF_SSHORT:
            // no byte swap needed here because values in the IFD are already swapped
            // (if necessary at all)
            Values[I] := Word(Value);
          TIFF_LONG,
          TIFF_SLONG:
            Values[I] := Value;
        end;
        Value := Value shr Shift;
      end;
    end
    else
    begin
      // data of this tag does not fit into one 32 bits value
      Stream.Position := FBasePosition + FIFD[Index].Offset;
      // even bytes sized data must be read by the loop as it is expanded to cardinals
      for I := 0 to High(Values) do
      begin
        //Stream.Read(Value, DataTypeToSize[TTiffDataType(FIFD[Index].DataType)]);
        case TTiffDataType(FIFD[Index].DataType) of
          TIFF_BYTE: Value := Memory.ReadByte; //Byte(Value);
          TIFF_SHORT,
          TIFF_SSHORT: Value := FixEndian(Memory.ReadWord); //FixEndian(Word(Value));
          TIFF_LONG,
          TIFF_SLONG: Value := FixEndian(Memory.ReadCardinal); //FixEndian(Value);
        end;
        Values[I] := Value;
      end;
    end;
  end;
end;

procedure TBZBitmapTIFFImage.GetTagValueList(TagID : word; var Values: TFloatArray);
// returns the values of the IFD entry indicated by Tag
var
  Index,
  Shift,
  IntValue: Cardinal;
  Value: Single;
  I: Integer;
  IntNominator,
  IntDenominator: Cardinal;
  FloatNominator,
  FloatDenominator: Cardinal;

begin
  Values := nil;
  if FindTag(Tag, Index) and (FIFD[Index].DataLength > 0) then
  begin
    // prepare value list
    SetLength(Values, FIFD[Index].DataSize);

    // determine whether the data fits into 4 bytes
    Value := DataTypeToSize[TTiffDataType(FIFD[Index].DataType)] * FIFD[Index].DataSize;

    // data fits into one cardinal -> extract it
    if Value <= 4 then
    begin
      Shift := DataTypeToSize[TTiffDataType(FIFD[Index].DataType)] * 8;
      IntValue := FIFD[Index].Offset;
      for I := 0 to FIFD[Index].DataLength - 1 do
      begin
        case TTiffDataType(FIFD[Index].DataType) of
          TIFF_BYTE,
          TIFF_ASCII,
          TIFF_SBYTE,
          TIFF_UNDEFINED: Values[I] := Byte(IntValue);
          TIFF_SHORT,
          TIFF_SSHORT:
            // no byte swap needed here because values in the IFD are already swapped
            // (if necessary at all)
            Values[I] := Word(IntValue);
          TIFF_LONG,
          TIFF_SLONG:
            Values[I] := IntValue;
        end;
        IntValue := IntValue shr Shift;
      end;
    end
    else
    begin
      // data of this tag does not fit into one 32 bits value
      Memory.Position := FBasePosition + FIFD[Index].Offset;
      // even bytes sized data must be read by the loop as it is expanded to Single
      for I := 0 to High(Values) do
      begin
        case TTiffDataType(FIFD[Index].DataType) of
          TIFF_BYTE:
            begin
              //Memory.ReadBuffer(IntValue, DataTypeToSize[TTiffDataType(FIFD[Index].DataType)]);

              Value := Memory.ReadByte; //Byte(IntValue);
            end;
          TIFF_SHORT,
          TIFF_SSHORT:
            begin
              //Memory.ReadBuffer(IntValue, DataTypeToSize[TTiffDataType(FIFD[Index].DataType)]);
              Value := FixEndian(Memory.ReadWord) //FixEndian(Word(IntValue));
            end;
          TIFF_LONG,
          TIFF_SLONG:
            begin
              //Memory.ReadBuffer(IntValue, DataTypeToSize[TTiffDataType(FIFD[Index].DataType)]);
              Value := FixEndian(Memory.ReadLongint); //FixEndian(IntValue);
            end;
          TIFF_RATIONAL,
          TIFF_SRATIONAL:
            begin
              //Memory.ReadBuffer(FloatNominator, SizeOf(FloatNominator));
              //Memory.ReadBuffer(FloatDenominator, SizeOf(FloatDenominator));
              FloatNominator := Memory.ReadCardinal;
              FloatDenominator := Memory.ReadCardinal;
              if not(FLittleEndian)then
              begin
                FloatNominator := FixEndian(Cardinal(FloatNominator));
                FloatDenominator := FixEndian(Cardinal(FloatDenominator));
              end;
              Value := FloatNominator / FloatDenominator;
            end;
          TIFF_FLOAT:
            begin
              //Memory.ReadBuffer(IntNominator, SizeOf(IntNominator));
              //Memory.ReadBuffer(IntDenominator, SizeOf(IntDenominator));
              IntNominator := Memory.ReadCardinal;
              IntDenominator := Memory.ReadCardinal;
              if not(FLittleEndian)then
              begin
                IntNominator := FixEndian(IntNominator);
                IntDenominator := FixEndian(IntDenominator);
              end;
              Value := IntNominator / IntDenominator;
            end;
          end;
        Values[I] := Value;
      end;
    end;
  end;
end;

function TBZBitmapTIFFImage.GetTagValue(TagID: Word; Default: Single = 0): Single;
// returns the value of the IFD entry indicated by Tag or the default value if the entry is not there
var
  Index: Cardinal;
  IntNominator,
  IntDenominator: Cardinal;
  FloatNominator,
  FloatDenominator: Cardinal;

begin
  Result := Default;
  if FindTag(Tag, Index) then
  begin
    // if the data length is > 1 then Offset is a real offset into the stream,
    // otherwise it is the value itself and must be shortend depending on the data type
    if FIFD[Index].DataSize = 1 then
    begin
      case TBZTIFFDataType(FIFD[Index].DataType) of
        TIFF_BYTE: Result := Byte(FIFD[Index].Offset);
        TIFF_SHORT,
        TIFF_SSHORT: Result := Word(FIFD[Index].Offset);
        TIFF_LONG,
        TIFF_SLONG: Result := FIFD[Index].Offset;
        TIFF_RATIONAL,
        TIFF_SRATIONAL:
        begin
          Memory.Position := FBasePosition + FIFD[Index].Offset;
          //Memory.ReadBuffer(FloatNominator, SizeOf(FloatNominator));
          //Memory.ReadBuffer(FloatDenominator, SizeOf(FloatDenominator));
          FloatNominator := Memory.ReadCardinal;
          FloatDenominator := Memory.ReadCardinal;
          if not(FLittleEndian)then
          begin
            FloatNominator := FixEndian(Cardinal(FloatNominator));
            FloatDenominator := FixEndian(Cardinal(FloatDenominator));
          end;
          Result := FloatNominator / FloatDenominator;
        end;
        TIFF_FLOAT:
        begin
          Memory.Position := FBasePosition + FIFD[Index].Offset;
          //Memory.ReadBuffer(IntNominator, SizeOf(IntNominator));
          //Memory.ReadBuffer(IntDenominator, SizeOf(IntDenominator));
          IntNominator := Memory.ReadCardinal;
          IntDenominator := Memory.ReadCardinal;
          if not(FLittleEndian) then
          begin
            IntNominator := FixEndian(IntNominator);
            IntDenominator := FixEndian(IntDenominator);
          end;
          Result := IntNominator / IntDenominator;
        end;
      end;
    end;
  end;
end;

function TBZBitmapTIFFImage.GetTagValue(TagID : word; Default: Cardinal = 0): Cardinal;
// returns the value of the IFD entry indicated by Tag or the default value if the entry is not there
var
  Index: Cardinal;

begin
  if not FindTag(Tag, Index) then
    Result := Default
  else
  begin
    Result := FIFD[Index].Offset;
    // if the data length is > 1 then Offset is a real offset into the stream,
    // otherwise it is the value itself and must be shortend depending on the data type
    if FIFD[Index].DataSize = 1 then
    begin
      case TBZTIFFDataType(FIFD[Index].DataType) of
        TIFF_BYTE: Result := Byte(Result);
        TIFF_SHORT,
        TIFF_SSHORT: Result := Word(Result);
        TIFF_LONG,
        TIFF_SLONG: ; // nothing to do
      else
        Result := Default;
      end;
    end;
  end;
end;

function TBZBitmapTIFFImage.GetTagValue(TagID : word; var DataSize: Cardinal; Default: Cardinal): Cardinal;
// Renvoie la valeur de l'entrée IFD indiquée par TagID ou la valeur par défaut
// si l'entrée n'est pas présente. Si la balise existe, la taille des données est également renvoyée.
var
  Index: Cardinal;

begin
  if not FindTag(Tag, Index) then
  begin
    Result := Default;
    DataSize := 0;
  end
  else
  begin
    Result := FIFD[Index].Offset;
    DataSize := FIFD[Index].DataSize;
    // if the data length is > 1 then Offset is a real offset into the stream,
    // otherwise it is the value itself and must be shortend depending on the data type
    if FIFD[Index].DataSize = 1 then
    begin
      case TBZTIFFDataType(FIFD[Index].DataType) of
        TIFF_BYTE: Result := Byte(Result);
        TIFF_SHORT,
        TIFF_SSHORT: Result := Word(Result);
        TIFF_LONG,
        TIFF_SLONG: ;// nothing to do
      else
        Result := Default;
      end;
    end;
  end;
end;

Function TBZBitmapTIFFImage.CheckFormat() : Boolean;
Var
  Magic : Word;
begin
  Result := False;

  Memory.Read(FHeader, SizeOf(TBZTIFF_FileHeader));
  if (FHeader.ByteOrder = cTIFFByteOrderLittle) then
  begin
    FLittleEndian := True;
    Result := True;
  end
  else if (FHeader.ByteOrder = cTIFFByteOrderBig) then
  begin
    FLittleEndian := False;
    Result := True;
  end;
  Magic := FixEndian(FHeader.Magic);
  Result := Result and (Magic = cTIFFMagic);
  // Lecture de l'entête ici
  if Result then
  begin
    Result := ReadImageProperties;
  end
  else
  begin
    RaiseInvalidImageFile(Format(rsBadSignature,[Magic.ToString]));
  end;
end;

Function TBZBitmapTIFFImage.ReadImageProperties : Boolean;
var
  IFDOffset : LongWord;
  IFDCount : Word;
  LocalBitsPerSample: TCardinalArray;

begin
  Result := True;
  IFDOffset := FHeader.FIDOffset;
  FBasePosition := Memory.position;

  Repeat
     Memory.Seek(IFDOffset,soCurrent);
     IFDCount := FixEndian(Memory.ReadWord);
     Memory.Seek(IFDCount * SizeOf(TBZTIFF_IFDEntry), soCurrent);
     IFDOffset := Memory.ReadWord;
     if IFDOffset = 0 then exit;
  until false;

  SetLength(FIFD, IFDCount);
  Memory.ReadBuffer(FIFD[0], IFDCount * SizeOf(TBZTIFF_IFDEntry));
  if not(FLittleEndian) then SwapIFD;

  // la spécification TIFF insiste sur les balises d'entrée ordonnées dans chaque IFD
  // Cela permet de repérer les fichiers endommagés.
  // Mais certains programmes comme 'GraphicConverter' ne commandent pas les balises d'extension
  // correctement. Nous trions donc les balises
  SortIFD;

  FImgWidth  := GetTagValue(cTIFF_TAG_ImageWidth);
  FImgHeight := GetTagValue(cTIFF_TAG_ImageHeight);

  if (FImgWidth = 0) or (FImgHeight = 0) then
  begin
    Result := False;
    RaiseInvalidImageFile(Format(rsBadImageSize,[FImgWidth,FImgHeight]));
  end;

  // data organization
  GetTagValueList(Stream, cTIFF_TAG_RowPerStrip, FRowsPerStrip);

  // certaines images dépendent de la taille par défaut ($ FFFFFFFF)
  // si une seule bande est présente dans l'image,
  // Dans le cas contraire, il faut s'assurer qu'il y a une valeur valide.
  if (Length(FRowsPerStrip) = 0) or (FRowsPerStrip[0] = $FFFFFFFF) then
  begin
    SetLength(FRowsPerStrip, 1);
    FRowsPerStrip[0] := FImgHeight;
  end;

  // nombre de composantes de couleur par pixel (1 pour n & b, 16 et 256 couleurs, 3 pour RVB, 4 pour CMJN etc.)
  FSamplesPerPixel := GetTagValue(cTIFF_TAG_SamplePerPixel, 1);

  // number of bits per color component
  GetTagValueList(Stream, cTIFF_TAG_BitsPerSample, LocalBitsPerSample);
  if Length(LocalBitsPerSample) = 0 then FBitsPerSample := 1
  else FBitsPerSample := LocalBitsPerSample[0];

  // déterminer si l'image est en mosaïque et récupérer les données de mosaïque si nécessaire
  FTileWidth := GetTagValue(TIFFTAG_TILEWIDTH, 0);
  FTileHeight := GetTagValue(TIFFTAG_TILELENGTH, 0);
  if  (TileWidth > 0) and (TileLength > 0) then FIsTiled := True else FIsTiled := False;

  // corrige l'IFD s'il est censé utiliser des tuiles mais fournir des morceaux sous forme de bandes
  // On lit les données du répertoire nécessaires au chargement réel de l'image:
  if FIsTiled then
  begin
    GetValueList(Stream, TIFFTAG_TILEOFFSETS, FOffsets);
    GetValueList(Stream, TIFFTAG_TILEBYTECOUNTS, FByteCounts);
  end;
  // Curieusement, dans TOUTES les images de la suite de tests en mosaïque,
  // les balises (TileOffsets, Tilecounts) sont absentes, mais StripOffsets et StripByteCounts sont présents.

  // c'est pourquoi une solutions de contournement est nécessaire:
  // si TileOffsets ou TileByteCounts n'est pas présent.
  if not (FIsTiled) or (Offsets = 0) or (ByteCounts = 0) then
  begin
    // data organization
    GetValueList(Stream, TIFFTAG_STRIPOFFSETS, Offsets);
    GetValueList(Stream, TIFFTAG_STRIPBYTECOUNTS, ByteCounts);
  end;

  if FTileWidth > 0 then
  begin
    if (Offsets=0) and (IFD.StripOffsets <> 0) then
  //  begin
  //    IFD.TileOffsets := IFD.StripOffsets;
  //    IFD.StripOffsets := 0;
  //  end;
  //  if (IFD.TileByteCounts=0) and (IFD.StripByteCounts <> 0) then
  //  begin
  //    IFD.TileByteCounts := IFD.StripByteCounts;
  //    IFD.StripByteCounts:= 0;
  //  end;
  //end else
  //begin
  //  //if not specified, the strip is the whole image
  //  if IFD.RowsPerStrip = 0 then IFD.RowsPerStrip:= IFD.ImageHeight;
  //end;

  With DataFormatDesc do
  begin
    Version := '-.-';

  end;
  ImageDescription.InitDefault(FBmpWidth, FBmpHeight, 32);

end;

Class Function TBZBitmapTIFFImage.Capabilities : TBZDataFileCapabilities;
begin
  Result := [dfcRead]; //, dfcWrite];
end;

Initialization

  RegisterRasterFormat('tif', 'Tagged Image File Format', TBZBitmapTIFFImage);
  RegisterRasterFormat('tiff', 'Tagged Image File Format', TBZBitmapTIFFImage);

Finalization

  UnregisterRasterFormat(TBZBitmapTIFFImage);

end.

