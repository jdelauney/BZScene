(*
  @abstract(Prise en charge des Images au format "PNG" Potable Network Graphic.)

  Spécifications : @br
  @unorderedList(
    @item(Méthode de compression     : LZ77)
    @item(Nombre de couleurs	       : 1 à 64 bits)
    @item(Supporte plusieurs images  : Oui formats : APNG, MNG)
    @item(Format des nombres	       : big-endian)
    @item(Auteurs                    : "the developers of PNG")
    @item(Extensions                 : *.png, *.apng, *.mng, *.jng)
    @item(Dimensions Maximum         : 2Gx2G pixels)
  )

  -------------------------------------------------------------------------------------------------------------

  @created(2017-05-11)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(11/05/2017 : Creation  )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) :

  Informations sur le format PNG : @br
  @unorderedList(
    @item(https://fr.wikipedia.org/wiki/Portable_Network_Graphics)
    @item(https://en.wikipedia.org/wiki/Portable_Network_Graphics)
    @item(https://www.w3.org/TR/PNG/ et https://www.w3.org/TR/REC-png.pdf)
    @item(http://www.fileformat.info/format/png/egff.htm)
    @item(http://www.libpng.org/pub/png/spec/)
    @item(http://www.libpng.org/pub/png/spec/iso/index-object.html#4Concepts.FormatChunks)
    @item(http://www.libpng.org/pub/png/spec/register/pngext-1.4.0-pdg.html)
    @item(ftp://ftp.simplesystems.org/pub/png/documents/pngextensions.html)
    @item(http://www.dcode.fr/chunks-png)
    @item(http://fileformats.wikia.com/wiki/Portable_Network_Graphics)
    @item(http://fileformats.archiveteam.org/wiki/PNG)
    @item(https://www.buvetteetudiants.com/cours/administrator/html-css/formats-png.php)
  )

    Autres informations utiles : @br
    @unorderedList(
      @item(https://fr.99designs.ch/blog/tips/image-file-types/)
      @item(http://www.martinreddy.net/gfx/2d-hi.html)
      @item(https://books.google.ch/books?id=_nJLvY757dQC&pg=PA191&lpg=PA191&dq=png+chunk+type&source=bl&ots=0gQL41jh6k&sig=fV9AD4-94SK9qI2VxyuCzAS3-vo&hl=fr&sa=X&ved=0ahUKEwj1srXR1dfVAhVJXRQKHYs5BKAQ6AEIeTAN#v=onepage&q=png%20chunk%20type&f=false)
      @item(https://blog.johnnovak.net/2016/09/21/what-every-coder-should-know-about-gamma/)
      @item(http://www.profil-couleur.com/tp/205-profil-icc.php)
    )

    Fichiers test : @br
    @unorderedList(
      @item(http://www.schaik.com/pngsuite/)
      @item(https://code.google.com/archive/p/imagetestsuite/downloads)
      @item(https://yardstick.pictures)
    )
  -------------------------------------------------------------------------------------------------------------

  @bold(Dépendances) : BZClasses, BZColors, BZGraphic,  BZBitmap, BZImageFileIO, BZImageStrConsts, BZUtils

  -------------------------------------------------------------------------------------------------------------

  @bold(Crédits) :
     Tous les liens au dessus, dessous, dedans, dehors, et au delà.... @br
     + FPC, GraphicEx, Vampire, Graphic32

  -------------------------------------------------------------------------------------------------------------

  @bold(Licence) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
Unit BZImageFilePNG;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\bzscene_options.inc}
//==============================================================================

//------------------------------------------------------------------------------
//----------------------------[ TODO LIST ]-------------------------------------
    { TODO 0 -oBZBitmap -cSupport_Images_PNG : - Ecriture Format PNG }
    { TODO 1 -oBZBitmap -cSupport_Images_PNG : - Support Chunk iTXT  }
    { TODO 1 -oBZBitmap -cSupport_Images_PNG : - Support Chunk eXIf  }
    { TODO 2 -oBZBitmap -cSupport_Images_PNG : - Prise en charge format MNG }
    { TODO 2 -oBZBitmap -cSupport_Images_PNG : - Prise en charge format JNG }
    { TODO 2 -oBZBitmap -cSupport_Images_PNG : - Prise en charge format APNG }
    { TODO 3 -oBZBitmap -cSupport_Images_PNG : - Support Chunk ICC   }
    { TODO 5 -oBZBitmap -cSupport_Images_PNG : - Support Chunk dSIG  }
    { TODO 5 -oBZBitmap -cSupport_Images_PNG : - Support Chunk hIST  }
    { TODO 5 -oBZBitmap -cSupport_Images_PNG : - Support Chunk sTER  }
//------------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, bzZStream,
  BZClasses, BZColors, BZGraphic,  BZBitmap, BZImageFileIO;

Type
  TBZPNGMagicID =  Array[0..7] of Byte;
  TBZPNGChunkCode = array[0..3] of char;
  TBZPNGICCPName = Array[0..78] of char;

const
  cNULL_MAGICID   : TBZPNGMagicID = ($00, $00, $00, $00, $00, $00, $00, $00);
  cPNG_MAGICID    : TBZPNGMagicID = ($89, $50, $4E, $47, $0D, $0A, $1A, $0A);
  cMNG_MAGICID    : TBZPNGMagicID = ($89, $4D, $4E, $47, $0D, $0A, $1A, $0A);
  cJNG_MAGICID    : TBZPNGMagicID = ($89, $4A, $4E, $47, $0D, $0A, $1A, $0A);

  {Modes de couleur valide pour le PNG}
  COLOR_GRAYSCALE      = 0;
  COLOR_RGB            = 2;
  COLOR_PALETTE        = 3;
  COLOR_GRAYSCALEALPHA = 4;
  COLOR_RGBA           = 6;

Type
  TBZPortableNetworkGraphicFormatType = (ftPNG, tfAPNG, ftMNG, ftJNG);

{ 0	Perceptual	for images preferring good adaptation to the output device gamut at the expense of colorimetric accuracy, such as photographs.
  1	Relative colorimetric	for images requiring colour appearance matching (relative to the output device white point), such as logos.
  2	Saturation	for images preferring preservation of saturation at the expense of hue and lightness, such as charts and graphs.
  3	Absolute colorimetric	for images requiring preservation of absolute colorimetry, such as previews of images destined for a different output device (proofs). }
  TBZPNGsRGBType =(rtPerceptual, rtRelative, rtSaturation, rtAbsolute);

  { Les différents types de "Chunk" supportés }
  TBZPNGChunkTypes = (
     ctIHDR,  ctcHRM,  ctgAMA,  ctsBIT,
     ctPLTE,  ctbKGD,  cthIST,  cttRNS,
     ctoFFs,  ctpHYs,  ctIDAT,  cttIME,
     ctsCAL,  cttEXt,  ctzTXt,  ctIEND,  ctIEND2, ctIdND,
     ctsRGB,  ctiCCP,  ctiTXt,  ctsPLT,
     ctMHDR,  ctMEND,  ctJHDR,  ctJDAT,
     ctJDAA,  ctJSEP,  ctBACK,  ctDEFI,
     ctTERM,  ctacTL,  ctfcTL,  ctfdAT,
     ctUnknown
     );

  { En-tête des "Chunk" suivie des données de taille "DataSize" et du checksum "CRC" }
  TBZPNGChunkHeader = packed record
    DataSize: LongWord;
    Name : TBZPNGChunkCode;
  end;

  TBZPNGChunkInfos = packed record
    ChunkHeader: TBZPNGChunkHeader;
    ChunkData: Pointer; // Buffer temporaire pour la lecture des données. (Utilisation non obligatoire pour tous les "Chunks")
    ChunkCrc: LongWord;
    ChunkType : TBZPNGChunkTypes;
  end;

 { IHDR chunk format - En-tête PNG.}
  TBZPNGChunk_IHDR = packed record
    Width: LongWord;              //< Image width
    Height: LongWord;             //< Image height
    BitDepth: Byte;               //< Bits par pixel ou bits par canal (pour "TrueColor")
    ColorType: Byte;              //< 0 = grayscale, 2 = truecolor, 3 = palette,
                                  //< 4 = gray + alpha, 6 = truecolor + alpha
    Compression: Byte;            //< Compression type:  0 = ZLib
    Filter: Byte;                 //< Filtre de prediction utilisé avant la compression
    Interlacing: Byte;            //< Entrelacé: 0 = non int, 1 = Adam7
  end;
  PBZPNGChunk_IHDR = ^TBZPNGChunk_IHDR;

  { MHDR chunk format - En-tête MNG.}
  TBZPNGChunk_MHDR = packed record
    FrameWidth: LongWord;         //< Frame width
    FrameHeight: LongWord;        //< Frame height
    TicksPerSecond: LongWord;     //< FPS of animation
    NominalLayerCount: LongWord;  //< Number of layers in file
    NominalFrameCount: LongWord;  //< Number of frames in file
    NominalPlayTime: LongWord;    //< Play time of animation in ticks
    SimplicityProfile: LongWord;  //< Defines which MNG features are used in this file
  end;
  PBZPNGChunk_MHDR = ^TBZPNGChunk_MHDR;

  { JHDR chunk format - En-tête JNG.}
  TBZPNGChunk_JHDR = packed record
    Width: LongWord;              //< Image width
    Height: LongWord;             //< Image height
    ColorType: Byte;              //< 8 = grayscale (Y), 10 = color (YCbCr),
                                  //< 12 = gray + alpha (Y-alpha), 14 = color + alpha (YCbCr-alpha)
    SampleDepth: Byte;            //< 8, 12 or 20 (8 and 12 samples together) bit
    Compression: Byte;            //< Compression type:  8 = Huffman coding
    Interlacing: Byte;            //< 0 = single scan, 8 = progressive
    AlphaSampleDepth: Byte;       //< 0, 1, 2, 4, 8, 16 if alpha compression is 0 (PNG). 8 if alpha compression is 8 (JNG)
    AlphaCompression: Byte;       //< 0 = PNG graysscale IDAT, 8 = grayscale 8-bit JPEG
    AlphaFilter: Byte;            //< 0 = PNG filter or no filter (JPEG)
    AlphaInterlacing: Byte;       //< 0 = non interlaced
  end;
  PBZPNGChunk_JHDR = ^TBZPNGChunk_JHDR;

  { acTL chunk format - APNG animation control.}
  TBZPNGChunk_acTL = packed record
    NumFrames: LongWord;          //< Number of frames
    NumPlay: LongWord;            //< Number of times to loop the animation (0 = inf)
  end;
  PBZPNGChunk_acTL =^TBZPNGChunk_acTL;

  { fcTL chunk format - APNG frame control.}
  TBZPNGChunk_fcTL = packed record
    SeqNumber: LongWord;          //< Sequence number of the animation chunk, starting from 0
    Width: LongWord;              //< Width of the following frame
    Height: LongWord;             //< Height of the following frame
    XOffset: LongWord;            //< X position at which to render the following frame
    YOffset: LongWord;            //< Y position at which to render the following frame
    DelayNumer: Word;             //< Frame delay fraction numerator
    DelayDenom: Word;             //< Frame delay fraction denominator
    DisposeOp: Byte;              //< Type of frame area disposal to be done after rendering this frame
    BlendOp: Byte;                //< Type of frame area rendering for this frame
  end;
  PBZPNGChunk_fcTL = ^TBZPNGChunk_fcTL;

  TBZPNGChunk_cHRM = packed record
    WhitePointX : LongWord;
    WhitePointY : LongWord;
    RedX        : LongWord;
    RedY        : LongWord;
    GreenX      : LongWord;
    GreenY      : LongWord;
    BlueX       : LongWord;
    BlueY       : LongWord;
  end;
  PBZPNGChunk_cHRM = ^TBZPNGChunk_cHRM;

  TBZPNGChunk_iCCP = packed record
    name :  TBZPNGICCPName;
    nullValue : Byte;
    CompressMethod : Byte;
    Profil : Pointer;
  end;

  TBZPNGChunk_pHYs= packed record
    PixelsPerX : LongWord;
    PixelsPerY : LongWord;
    Unity      : Byte;  //< 0 = Inconnu 1 = Metre
  end;
  PBZPNGChunk_pHYs = ^TBZPNGChunk_pHYs;

  TBZPNGChunk_tIME= packed record
    Year   : Word;
    Month  : Byte;
    Day    : Byte;
    Hour   : Byte;
    Minute : Byte;
    Second : Byte;
  end;
  PBZPNGChunk_tIME = ^TBZPNGChunk_tIME;

Type
  { TBZBitmapPortableNetworkGraphicImage : Classe de base pour la prise en charge
    des formats PNG, MNG et JNG}
  TBZBitmapNetworkGraphicImage = Class(TBZCustomImageFileIO)
  private
     FGlobalPalette : TBZColorList;
  protected
   // Decoder: TBZDataEncoderLZ77;7
    ImageType :  TBZPortableNetworkGraphicFormatType; // Type d'image, pour les reconnaitres car les en-têtes sont differents

    ZData : TMemoryStream;

    imgWidth, imgHeight : LongWord; // Infos de base
    bitCount : Byte;

    // En-tête des différent formats
    PNGHeader : TBZPNGChunk_IHDR;
    MNGHeader : TBZPNGChunk_MHDR;
    JNGHeader : TBZPNGChunk_JHDR;

    BackgroundColor : TBZColor;
    HasTransparency : Boolean;
    ATransparentColor : TBZColor;
    ATransparentColorIndex : Byte;

    AlphaPalette : Array of byte; //Canal Alpha pour les images indexées  (Utilisation d'une palette de couleurs)

    // Pour l Lecture du "Chunk" en cours
    ChunkInfos : TBZPNGChunkInfos;

    sRGBType : TBZPNGsRGBType;

    CIExyz : TBZPNGChunk_cHRM;
    iCCP_Profil : TBZPNGChunk_iCCP;

    HasCIExyz : Boolean;
    HassRGB : Boolean;
    HasICCP : Boolean;

    GammaCorrection : Boolean;
    GammaFactor  : Single;
    GammaTable   : array[0..255] of Byte;
    GammaTable16 : array[0..65534] of Word;
    //InverseGammaTable : array[0..25] of Byte;

    sBits : Boolean;
    sGrayBits, sRedBits, sGreenBits, sBlueBits, sAlphaBits : Byte;

    procedure ReadChunkHeader;
    function ReadChunkData:Boolean;
    procedure SkipChunkData;


    function SetupColorDepth(ColorType, BitDepth: Integer): Integer;
    procedure ApplyFilter(Filter: Byte; aLine, aPrevLine, Target: PByte; BPP, BytesPerRow: Integer);
    procedure DecodeData;

    Procedure LoadFromMemory(); override;
    Function CheckFormat(): Boolean; override;
    Function ReadImageProperties: Boolean; override;
  public

    Constructor Create(AOwner: TPersistent; AWidth, AHeight: Integer); override;
    Destructor Destroy; override;

    Class Function Capabilities: TBZDataFileCapabilities; override;

    Function getImagePropertiesAsString: String; override;
  end;

implementation

Uses
  BZImageStrConsts, Math, BZUtils

  //ZBase, ZInflate
  {$IFDEF DEBUG}
  , Dialogs, BZLogger
  {$ENDIF};

Const
  CRCTable: array[0..255] of LongWord = (
      $00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F, $E963A535, $9E6495A3,
      $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
      $1DB71064, $6AB020F2, $F3B97148, $84BE41DE, $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
      $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC, $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
      $3B6E20C8, $4C69105E, $D56041E4, $A2677172, $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
      $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
      $26D930AC, $51DE003A, $C8D75180, $BFD06116, $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
      $2802B89E, $5F058808, $C60CD9B2, $B10BE924, $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,
      $76DC4190, $01DB7106, $98D220BC, $EFD5102A, $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
      $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
      $6B6B51F4, $1C6C6162, $856530D8, $F262004E, $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
      $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
      $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2, $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
      $4369E96A, $346ED9FC, $AD678846, $DA60B8D0, $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
      $5005713C, $270241AA, $BE0B1010, $C90C2086, $5768B525, $206F85B3, $B966D409, $CE61E49F,
      $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,
      $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A, $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
      $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8, $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
      $F00F9344, $8708A3D2, $1E01F268, $6906C2FE, $F762575D, $806567CB, $196C3671, $6E6B06E7,
      $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC, $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
      $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
      $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
      $CB61B38C, $BC66831A, $256FD2A0, $5268E236, $CC0C7795, $BB0B4703, $220216B9, $5505262F,
      $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,
      $9B64C2B0, $EC63F226, $756AA39C, $026D930A, $9C0906A9, $EB0E363F, $72076785, $05005713,
      $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38, $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
      $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
      $88085AE6, $FF0F6A70, $66063BCA, $11010B5C, $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
      $A00AE278, $D70DD2EE, $4E048354, $3903B3C2, $A7672661, $D06016F7, $4969474D, $3E6E77DB,
      $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
      $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605, $CDD70693, $54DE5729, $23D967BF,
      $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94, $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D
    );

  cChunkTypesName : array[TBZPNGChunkTypes] of TBZPNGChunkCode = (
  'IHDR',  'cHRM',  'gAMA',  'sBIT',
  'PLTE',  'bKGD',  'hIST',  'tRNS',
  'oFFs',  'pHYs',  'IDAT',  'tIME',
  'sCAL',  'tEXt',  'zTXt',  'IEND', 'wEND', 'IdND',       // Note : les tags :  'wEND', 'IdND',  ne sont pas des tags officiels. Il sont introduit notamment par Adobe FireWorks
  'sRGB',  'iCCP',  'iTXt',  'sPLT',

  'MHDR',  'MEND',  'JHDR',  'JDAT',
  'JDAA',  'JSEP',  'BACK',  'DEFI',
  'TERM',  'acTL',  'fcTL',  'fdAT',
  'Unkn');

  cValidKeyWords : array[0..10] of String = (
  'Title', 'Author', 'Description', 'Copyright',
  'Creation Time', 'Software', 'Disclaimer',
  'Legal disclaimer', 'Warning', 'Source', 'Comment');

  MaxChunkLength = $7FFFFFFF;

  cInterlaced_Adam7_RowStart: array[0..6] of LongInt = (0, 0, 4, 0, 2, 0, 1);
  cInterlaced_Adam7_ColumnStart: array[0..6] of LongInt = (0, 4, 0, 2, 0, 1, 0);
  cInterlaced_Adam7_RowIncrement: array[0..6] of LongInt = (8, 8, 8, 4, 4, 2, 2);
  cInterlaced_Adam7_ColumnIncrement: array[0..6] of LongInt = (8, 8, 4, 4, 2, 2, 1);
  cInterlaced_Adam7_PassMask: array[0..6] of Byte = ($80, $08, $88, $22, $AA, $55, $FF);

  { APNG frame dispose operations.}
  cAPNG_DisposeOpNone       = 0;
  cAPNG_DisposeOpBackground = 1;
  cAPNG_DisposeOpPrevious   = 2;

  { APNG frame blending modes}
  cAPNG_BlendOpSource = 0;
  cAPNG_BlendOpOver   = 1;

function GetCRC(const Buffer: Pointer; const Size: Integer): LongWord;
  var i: Integer;
  var pb: PByte;
begin
  Result := $ffffffff;
  pb := Buffer;
  for i := 0 to Size - 1 do
  begin
    Result:= CRCTable[(Result xor pb^) and $ff] xor (Result shr 8);
    Inc(pb);
  end;
  Result := Result xor $ffffffff;
end;

(* procedure Decompress(const Buffer: Pointer; const Size: Integer; const Output: TStream);
const
  BufferSize = $8000;
var
  ZStreamRec: z_stream;
  ZResult: Integer;
  TempBuffer: Pointer;
begin
  FillChar(ZStreamRec, SizeOf(z_stream), 0);
  ZStreamRec.next_in := Buffer;
  ZStreamRec.avail_in := Size;
  if inflateInit(ZStreamRec) < 0 then Exit;
  GetMem(TempBuffer, BufferSize);
  try
    while ZStreamRec.avail_in > 0 do
    begin
      ZStreamRec.next_out := TempBuffer;
      ZStreamRec.avail_out := BufferSize;
      inflate(ZStreamRec, Z_NO_FLUSH);
      Output.Write(TempBuffer^, BufferSize - ZStreamRec.avail_out);
    end;
    repeat
      ZStreamRec.next_out := TempBuffer;
      ZStreamRec.avail_out := BufferSize;
      ZResult := inflate(ZStreamRec, Z_FINISH);
      Output.Write(TempBuffer^, BufferSize - ZStreamRec.avail_out);
    until (ZResult = Z_STREAM_END) and (ZStreamRec.avail_out > 0);
  finally
    FreeMem(TempBuffer, BufferSize);
    inflateEnd(ZStreamRec);
  end;
end; *)

function CheckCRC(const Chunk: TBZPNGChunkInfos): Boolean;
  var i: Integer;
  var CRC: LongWord;
  var Data: PByte;
begin
  CRC := $ffffffff;
  for i := 0 to 3 do
    CRC := CRCTable[(CRC xor i) and $ff] xor (CRC shr 8); //Byte(Chunk.ChunkType(i)
  Data := Chunk.ChunkData;
  for i := 0 to Chunk.ChunkHeader.DataSize - 1 do
  begin
    CRC := CRCTable[(CRC xor Data^) and $ff] xor (CRC shr 8);
    Inc(Data);
  end;
  CRC := CRC xor $ffffffff;
  Result := SwapEndian(CRC) = Chunk.ChunkCRC;
end;

{%region%=====[  TBZBitmapNetworkGraphicImage ]===============================}

constructor TBZBitmapNetworkGraphicImage.Create(AOwner : TPersistent; AWidth, AHeight : Integer);
Begin
  Inherited Create(aOwner, AWidth, AHeight);
  //////GlobalLogger.LogNotice('Creation de TBZBitmapNetworkGraphicImage');
  With DataFormatDesc Do
  Begin
    Name := 'PNG';
    Desc := 'Portable NetWork Graphic image';
    FileMask := '*.png; *.apng; *.mng; *.jng';
    Version := '1.x';
    Encoding := etLZ77;
  End;
  //ChunkInfos := nil;
  HasTransparency:=false;
  BackgroundColor:=clrBlack;
end;

destructor TBZBitmapNetworkGraphicImage.Destroy;
Begin
  //if ChunkInfos<>nil then
  //begin
  //  FreeMem(ChunkInfos);
  //  ChunkInfos := nil;
  //end;
  //SupportedColorFormat := [];
  Inherited Destroy;
End;

class function TBZBitmapNetworkGraphicImage.Capabilities : TBZDataFileCapabilities;
Begin
  Result := [dfcRead]; //[dfcRead, dfcWrite]
End;

function TBZBitmapNetworkGraphicImage.getImagePropertiesAsString : String;
Var
  S: String;
Begin
  S := 'na';
  Result:=S;
end;

procedure TBZBitmapNetworkGraphicImage.ReadChunkHeader;

{function TPNGGraphic.IsValidChunk(ChunkType: TChunkType): Boolean;

// determines, independant of the cruxial 5ths bits in each "letter", whether the
// current chunk type in the header is the same as the given chunk type

const
  Mask = not $20202020;

begin
  Result := (FHeader.ChunkMask and Mask) = (PDWORD(@ChunkType)^ and Mask);
end; }

Var
  aType:Integer;
begin
  ////GlobalLogger.LogStatus('======================================================================');
  ////GlobalLogger.LogNotice('Read Chunk Header');
  ////GlobalLogger.LogStatus('Memory Position  : '+InttoStr(Memory.Position));
  //Memory.Read(ChunkInfos.ChunkHeader.DataSize, 4);
  ChunkInfos.ChunkHeader.DataSize := Memory.ReadLongWord;
  Memory.Read(ChunkInfos.ChunkHeader.Name, 4);
  {$IFDEF ENDIAN_LITTLE}
    ChunkInfos.ChunkHeader.DataSize := BEToN(ChunkInfos.ChunkHeader.DataSize);
  {$ENDIF}
  ////GlobalLogger.LogStatus('Memory Position  : '+InttoStr(Memory.Position));
  ////GlobalLogger.LogStatus('---> Name     : '+String(ChunkInfos.ChunkHeader.Name));
  ////GlobalLogger.LogStatus('---> DataSize : '+InttoStr(ChunkInfos.ChunkHeader.DataSize));

  // On trouve le type de "chunk"
  aType :=0;
  while (aType < 33) and (cChunkTypesName[TBZPNGChunkTypes(aType)] <> ChunkInfos.ChunkHeader.Name) do inc (aType);
  ChunkInfos.ChunkType:=TBZPNGChunkTypes(aType);
end;

function  TBZBitmapNetworkGraphicImage.ReadChunkData:Boolean;
Var
  BytesRead : Longint;
begin
  Result:=False;
  //GlobalLogger.LogNotice('Read Chunk Data');
  //GlobalLogger.LogStatus('Memory Position  : '+InttoStr(Memory.Position));
  if ChunkInfos.ChunkHeader.DataSize > 0 then
  begin
    ReAllocMem(ChunkInfos.ChunkData, ChunkInfos.ChunkHeader.DataSize);
    BytesRead := Memory.Read(ChunkInfos.ChunkData^,ChunkInfos.ChunkHeader.DataSize);
  end
  else ChunkInfos.ChunkData := nil;

  ChunkInfos.ChunkCrc := Memory.ReadLongWord;
  If (BytesRead <> ChunkInfos.ChunkHeader.DataSize) or (ChunkInfos.ChunkHeader.DataSize>MaxChunkLength) then
  begin
    RaiseInvalidImageFile('Erreur de lecture : Taille des donnée du chunk = '+String(ChunkInfos.ChunkHeader.Name)+' incorrecte');
  end;

  Result := true;
  // Vérification du CRC
  //result := checkCrc(ChunkInfos.ChunkData);
  //if not(Result) then
  //begin
  //  RaiseInvalidImageFile('Erreur de lecture : Chunk CRC incorrecte');
  //end;
end;

procedure TBZBitmapNetworkGraphicImage.SkipChunkData;
begin
  //GlobalLogger.LogNotice('Skip Chunk');
  Memory.SkipNextByte(ChunkInfos.ChunkHeader.DataSize + 4);
end;

function TBZBitmapNetworkGraphicImage.SetupColorDepth(ColorType, BitDepth : Integer) : Integer;
begin
  Case ColorType of
    COLOR_GRAYSCALE : // Gray scale
    begin
      if BitDepth in [1, 2, 4, 8, 16] then
      begin
        Result := (BitDepth + 7) div 8;
      end;
    end;
    COLOR_RGB : // RGB
    begin
      if BitDepth in [8, 16] then
      begin
        Result := BitDepth * 3 div 8;
      end;
    end;
    COLOR_PALETTE : // Indexed
    begin
      if BitDepth in [1, 2, 4, 8] then
      begin
        Result := 1;
      end;
    end;
     COLOR_GRAYSCALEALPHA :  // Gray RGBA
    begin
      if BitDepth in [8, 16] then
      begin
        Result := 2 * BitDepth div 8;
      end;
    end;
    COLOR_RGBA :  // RGBA
    begin
      if BitDepth in [8, 16] then
      begin
        Result := BitDepth * 4 div 8;
      end;
    end;
  end;
end;

procedure TBZBitmapNetworkGraphicImage.ApplyFilter(Filter : Byte; aLine, aPrevLine, Target : PByte; BPP, BytesPerRow : Integer);

  function PaethPredictor(a, b, c: Byte): Byte;
  var
    p, pa, pb, pc: Integer;
  begin
    // a = left, b = above, c = upper left
    p := a + b - c;        // initial estimate
    pa := Abs(p - a);      // distances to a, b, c
    pb := Abs(p - b);
    pc := Abs(p - c);
    // return nearest of a, b, c, breaking ties in order a, b, c
    if (pa <= pb) and (pa <= pc) then  Result := a
    else
      if pb <= pc then Result := b
      else Result := c;
  end;

// Applies the filter given in Filter to all bytes in Line (eventually using PrevLine).
// Note: The filter type is assumed to be of filter mode 0, as this is the only one currently
//       defined in PNG.
//       In opposition to the PNG documentation different identifiers are used here.
//       Raw refers to the current, not yet decoded value. Decoded refers to the current, already
//       decoded value (this one is called "raw" in the docs) and Prior is the current value in the
//       previous line. For the Paeth prediction scheme a fourth pointer is used (PriorDecoded) to describe
//       the value in the previous line but less the BPP value (Prior[x - BPP]).

var
  I: Integer;
  Raw,
  Decoded,
  Prior,
  PriorDecoded,
  TargetRun: PByte;

begin
  //////GlobalLogger.LogNotice('Apply Filter');
  case Filter of
    0: // no filter, just copy data
      begin
        ////GlobalLogger.LogNotice('Apply Filter : None');
        Move(aLine^, Target^, BytesPerRow);

      end;
    1: // subtraction filter
      begin
        ////GlobalLogger.LogNotice('Apply Filter : Sub');
        Raw := aLine;
        TargetRun := Target;
        // Transfer BPP bytes without filtering. This mimics the effect of bytes left to the
        // scanline being zero.
        Move(Raw^, TargetRun^, BPP);

        // now do rest of the line
        Decoded := TargetRun;
        Inc(Raw, BPP);
        Inc(TargetRun, BPP);
        Dec(BytesPerRow, BPP);
        while BytesPerRow > 0 do
        begin
          TargetRun^ := Byte(Raw^ + Decoded^);
          Inc(Raw);
          Inc(Decoded);
          Inc(TargetRun);
          Dec(BytesPerRow);
        end;
      end;
    2: // Up filter
      begin
        ////GlobalLogger.LogNotice('Apply Filter : Up');
        Raw := aLine;
        Prior := aPrevLine;
        TargetRun := Target;
        while BytesPerRow > 0 do
        begin
          TargetRun^ := Byte(Raw^ + Prior^);
          Inc(Raw);
          Inc(Prior);
          Inc(TargetRun);
          Dec(BytesPerRow);
        end;
      end;
    3: // average filter
      begin
        ////GlobalLogger.LogNotice('Apply Filter : Average');
        // first handle BPP virtual pixels to the left
        Raw := aLine;
        Decoded := aLine;
        Prior := aPrevLine;
        TargetRun := Target;
        for I := 0 to BPP - 1 do
        begin
          TargetRun^ := Byte(Raw^ + Floor(Prior^ / 2));
          Inc(Raw);
          Inc(Prior);
          Inc(TargetRun);
        end;
        Dec(BytesPerRow, BPP);

        // now do rest of line
        while BytesPerRow > 0 do
        begin
          TargetRun^ := Byte(Raw^ + Floor((Decoded^ + Prior^) / 2));
          Inc(Raw);
          Inc(Decoded);
          Inc(Prior);
          Inc(TargetRun);
          Dec(BytesPerRow);
        end;
      end;
   4: // paeth prediction
     begin
       ////GlobalLogger.LogNotice('Apply Filter : Paeth');
       // again, start with first BPP pixel which would refer to non-existing pixels to the left
       Raw := aLine;
       Decoded := Target;
       Prior := aPrevLine;
       PriorDecoded := aPrevLine;
       TargetRun := Target;
       for I := 0 to BPP - 1 do
       begin
         TargetRun^ := Byte(Raw^ + PaethPredictor(0, Prior^, 0));
         Inc(Raw);
         Inc(Prior);
         Inc(TargetRun);
       end;
       Dec(BytesPerRow, BPP);

       // finally do rest of line
       while BytesPerRow > 0 do
       begin
         TargetRun^ := Byte(Raw^ + PaethPredictor(Decoded^, Prior^, PriorDecoded^));
         if BytesPerRow > 0 then
         begin
           Inc(Raw);
           Inc(Decoded);
           Inc(Prior);
           Inc(PriorDecoded);
           Inc(TargetRun);
         end;
         Dec(BytesPerRow);
       end;
     end;
   end;
end;

procedure TBZBitmapNetworkGraphicImage.DecodeData;
var
  aBytesPerRow, Row : Integer;
  SourceBPP, TargetBPP : Byte;
  RowBuffer: array[Boolean] of PByte;
  EvenRow: Boolean; // distincts between the two rows we need to hold for filtering
  Pass: Integer;
  InterlaceRowBytes,
  InterlaceWidth: Integer;
  PixPtr : PBZColor;
  temp : byte;
  IgnoreAlpha : Boolean;
  Idx : Word;
  Color1, Color2 : TBZColor;
  DecompressStream : TDecompressionStream;


  procedure ConvertRowColorData(aSource : PByte; aTarget: PBZColor);
  var
    sx : Integer;
    TargetColor : TBZColor;
    Source16 : PWord;
  begin
    Case PNGHeader.ColorType of
      COLOR_GRAYSCALE :
      begin
        //GlobalLogger.logNotice('Convert Row ColorData : GRAYSCALE, '+PNGHeader.BitDepth.ToString+' bits');
        Case PNGHeader.BitDepth of
          1  :
          begin
            Color1:=clrBlack;
            Color2:=clrWhite;
            sx := 0;
            While (sx<=MaxWidth) do
            begin
              Idx := Byte(ASource^ shl (sx and 7)) shr 7; //ExtractPixel1Bit(PByte(ASource+(sx div 8))^,sx);
              ////GlobalLogger.LogNotice(' Idx = '+Idx.ToString);
              if Idx = 0 then  aTarget^:=Color1 else aTarget^:=Color2;
              IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
              if sx and 7=7 then Inc(ASource);
              inc(sx);
              inc(aTarget);
            end;
          end;
          2  :
          begin
            sx := 0;
            While (sx<=MaxWidth) do
            begin
              Idx := Round(((ASource^ shr ((not sx and 3) shl 1)) and $3)); //ExtractPixel2Bit
              ////GlobalLogger.LogNotice(' Idx = '+Idx.ToString);
              Case Idx of
                0 : TargetColor := clrBlack;
                1 : TargetColor := clrGray;
                2 : TargetColor := clrLtGray;
                3 : TargetColor := clrWhite;
              end;
              aTarget^:= TargetColor;
              IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
              if sx and 3=3 then Inc(ASource);
              inc(sx);
              inc(aTarget);
            end;
          end;
          4  :
          begin
            sx := 0;
            While (sx<=MaxWidth) do
            begin
              Idx := Round(((ASource^ shr ((not sx and 1) shl 2)) and $f)); //ExtractPixel4Bits
              temp := (Idx * 255) div 16;
              ////GlobalLogger.LogNotice(' Idx = '+Idx.ToString+ ' = ' + Temp.ToString);
              TargetColor.Create(Temp,Temp,Temp);

              aTarget^:= TargetColor;
              IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
              if Boolean(sx and 1) then Inc(ASource);
              inc(sx);
              inc(aTarget);
            end;
          end;
          8  :
          begin
            sx := 0;
            While (sx<=MaxWidth) do
            begin
              Idx :=ASource^;
              TargetColor.Create(Idx,Idx,Idx);
              aTarget^:= TargetColor;
              IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
              Inc(ASource);
              inc(sx);
              inc(aTarget);
            end;
          end;
          16 :
          begin
            sx := 0;
            Source16 := PWord(ASource);
            While (sx<=MaxWidth) do
            begin
              Idx :=Hi(Source16^);
              TargetColor.Create(Idx,Idx,Idx);
              aTarget^:= TargetColor;
              IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
              Inc(Source16);
              inc(sx);
              inc(aTarget);
            end;
          end;
        end;
      end;
      COLOR_RGB :  // RGB
      begin
        //GlobalLogger.logNotice('Convert Row ColorData : RGB, '+PNGHeader.BitDepth.ToString+' bits');
        if PNGHeader.BitDepth = 8 then    // 24bits
        begin
          For sx := 0 to MaxWidth do
          begin
            TargetColor.Red := aSource^;
            inc(aSource);
            TargetColor.Green := aSource^;
            inc(aSource);
            TargetColor.Blue := aSource^;
            inc(aSource);
            TargetColor.Alpha := 255;
            // Le support du Gamma avec les PNG pose des problème de rendu
            // On désactive simplement cette possibilité
            { if GammaCorrection then
            begin
              TargetColor.Red := GammaTable[TargetColor.Red];
              TargetColor.Green := GammaTable[TargetColor.Green];
              TargetColor.Blue := GammaTable[TargetColor.Blue];
            end; }

            aTarget^:= TargetColor;
            inc(aTarget);
          end;
        end
        else // 16    // 48 bits
        begin
          Source16 := PWord(aSource);
          For sx := 0 to MaxWidth do
          begin
            {if GammaCorrection then
            begin
              TargetColor.Red := MulDiv(GammaTable16[SwapEndian(Source16^)], 255, 65535); Inc(Source16);
              TargetColor.Green := MulDiv(GammaTable16[SwapEndian(Source16^)], 255, 65535); Inc(Source16);
              TargetColor.Blue := MulDiv(GammaTable16[SwapEndian(Source16^)], 255, 65535);Inc(Source16);
              TargetColor.Alpha := 255;
            end
            else }
            begin
              TargetColor.Red := MulDiv(SwapEndian(Source16^), 255, 65535); Inc(Source16);
              TargetColor.Green := MulDiv(SwapEndian(Source16^), 255, 65535); Inc(Source16);
              TargetColor.Blue := MulDiv(SwapEndian(Source16^), 255, 65535);Inc(Source16);
              TargetColor.Alpha := 255;
            end;
            //////GlobalLogger.LogNotice('TargetColor = '+TargetColor.ToString);
            aTarget^:= TargetColor;
            inc(aTarget);
          end;
        end;
      end;
      COLOR_PALETTE :  // Indexed
      begin
        //GlobalLogger.logNotice('Convert Row ColorData : INDEXED, '+PNGHeader.BitDepth.ToString+' bits');
        Case PNGHeader.BitDepth of
          1  :
          begin
            Color1:=clrBlack;
            Color2:=clrWhite;
            if (ImageDescription.PaletteCount>0) then
            begin
              Color1.AsInteger:= ImageDescription.PaletteEntries^[0].AsInteger;
              Color2.AsInteger:= ImageDescription.PaletteEntries^[1].AsInteger;
            end else AddError(Format(rsBitmapBadPaletteIndex,[Idx]));

            sx := 0;
            While (sx<=MaxWidth) do
            begin
              Idx := Byte(ASource^ shl (sx and 7)) shr 7;
              ////GlobalLogger.LogNotice('X : '+sx.ToString+' --> Idx = '+Idx.ToString + ' Color = '+ImageDescription.PaletteEntries^[Idx].ToString);
              if Idx = 0 then  aTarget^:=Color1 else aTarget^:=Color2;
              IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
              if sx and 7=7 then Inc(ASource);
              inc(sx);
              inc(aTarget);
            end;
          end;
          2  :
          begin
            sx := 0;
            While (sx<=MaxWidth) do
            begin
              Idx := Round(((ASource^ shr ((not sx and 3) shl 1)) and $3)); //ExtractPixel2Bit
              ////GlobalLogger.LogNotice(' Idx = '+Idx.ToString);
              if (ImageDescription.PaletteCount>0) and  (Idx<ImageDescription.PaletteCount) then
              begin
                aTarget^.AsInteger:= ImageDescription.PaletteEntries^[Idx].AsInteger;
                IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
              end else AddError(Format(rsBitmapBadPaletteIndex,[Idx]));
              IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
              if sx and 3=3 then Inc(ASource);
              inc(sx);
              inc(aTarget);
            end;
          end;
          4  :
          begin
            sx := 0;
            While (sx<=MaxWidth) do
            begin
              Idx := Round(((ASource^ shr ((not sx and 1) shl 2)) and $f)); //ExtractPixel4Bits
              temp := (Idx * 255) div 16;
              ////GlobalLogger.LogNotice(' Idx = '+Idx.ToString+ ' = ' + Temp.ToString);

              if (ImageDescription.PaletteCount>0) and (Idx<ImageDescription.PaletteCount) then
              begin
                 TargetColor.AsInteger:= ImageDescription.PaletteEntries^[Idx].AsInteger;
              end
              else
              begin
                if Idx<16 then
                begin
                  Case Idx of
                    0 : TargetColor := clrBlack;
                    1 : TargetColor := clrMaroon;
                    2 : TargetColor := clrGreen;
                    3 : TargetColor := clrOlive;
                    4 : TargetColor := clrNavy;
                    5 : TargetColor := clrPurple;
                    6 : TargetColor := clrTeal;
                    7 : TargetColor := clrGray;
                    8 : TargetColor := clrSilver;
                    9 : TargetColor := clrRed;
                    10 : TargetColor := clrLime;
                    11 : TargetColor := clrYellow;
                    12 : TargetColor := clrBlue;
                    13 : TargetColor := clrFuchsia;
                    14 : TargetColor := clrAqua;
                    15 : TargetColor := clrWhite;
                  end;
                end else AddError(Format(rsBitmapBadPaletteIndex,[Idx]));
              end;// else AddError(Format(rsBitmapBadPaletteIndex,[Idx]));
              aTarget^:= TargetColor;
              IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
              if Boolean(sx and 1) then Inc(ASource);
              inc(sx);
              inc(aTarget);
            end;
          end;
          8  :
          begin
            sx := 0;
            While (sx <= MaxWidth) do
            begin
              Idx:=aSource^;
              if (ImageDescription.PaletteCount>0) and (Idx<ImageDescription.PaletteCount) then
              begin
                 TargetColor.AsInteger:= ImageDescription.PaletteEntries^[Idx].AsInteger;
              end else AddError(Format(rsBitmapBadPaletteIndex,[Idx]));
              ////GlobalLogger.LogNotice('X : '+sx.ToString+' --> Idx = '+Idx.ToString + ' Color = '+TargetColor.ToString);
              aTarget^ := TargetColor;
              IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
              inc(aSource);
              inc(aTarget);
              inc(sx);
            end;
          end;
          16 :
          begin
            Source16 := PWord(ASource);
            For sx := 0 to MaxWidth do
            begin
              Idx := MulDiv(SwapEndian(Source16^), 255, 65535);
              if (ImageDescription.PaletteCount>0) and (Idx<ImageDescription.PaletteCount) then
              begin
                 TargetColor.AsInteger:= ImageDescription.PaletteEntries^[Idx].AsInteger;
              end else AddError(Format(rsBitmapBadPaletteIndex,[Idx]));
              aTarget^ := TargetColor;
              IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
              inc(Source16);
              inc(aTarget);
            end;
          end;
        end;
      end;
      COLOR_GRAYSCALEALPHA : // RGBA Gray Scale
      begin
        //GlobalLogger.logNotice('Convert Row ColorData : GRAYSCALE ALPHA, '+PNGHeader.BitDepth.ToString+' bits');
        if PNGHeader.BitDepth = 8 then
        begin
          For sx := 0 to MaxWidth do
          begin
            TargetColor.Red := aSource^;
            TargetColor.Green := aSource^;
            TargetColor.Blue := aSource^;
            inc(aSource);
            TargetColor.Alpha := aSource^;
            inc(aSource);
            {if GammaCorrection then
            begin
              TargetColor.Red := GammaTable[TargetColor.Red];
              TargetColor.Green := GammaTable[TargetColor.Green];
              TargetColor.Blue := GammaTable[TargetColor.Blue];
            end;}
            aTarget^:= TargetColor;
            IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
            inc(aTarget);
          end;
        end
        else // 16
        begin
          Source16 := PWord(aSource);
          For sx := 0 to MaxWidth do
          begin
            //TargetColor.Red := Round(aSource^ * 63 / 255 );
            Temp := MulDiv(SwapEndian(Source16^), 255, 65535);
            TargetColor.Red := Temp;
            TargetColor.Green := Temp;
            TargetColor.Blue := Temp;
            Inc(Source16);
            TargetColor.Alpha := MulDiv(SwapEndian(Source16^), 255, 65535);
            Inc(Source16);
            {if GammaCorrection then
            begin
              TargetColor.Red := GammaTable[TargetColor.Red];
              TargetColor.Green := GammaTable[TargetColor.Green];
              TargetColor.Blue := GammaTable[TargetColor.Blue];
            end; }
            aTarget^:= TargetColor;
            IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
            inc(aTarget);
          end;
        end;
      end;
      COLOR_RGBA:  // RGBA
      begin
        //GlobalLogger.logNotice('Convert Row ColorData : RGBA, '+PNGHeader.BitDepth.ToString+' bits');
        if PNGHeader.BitDepth = 8 then
        begin
          For sx := 0 to MaxWidth do
          begin
            TargetColor.Red := aSource^;
            inc(aSource);
            TargetColor.Green := aSource^;
            inc(aSource);
            TargetColor.Blue := aSource^;
            inc(aSource);
            TargetColor.Alpha := aSource^;
            inc(aSource);
            { if GammaCorrection then
            begin
              TargetColor.Red := GammaTable[TargetColor.Red];
              TargetColor.Green := GammaTable[TargetColor.Green];
              TargetColor.Blue := GammaTable[TargetColor.Blue];
            end; }
           // //GlobalLogger.LogNotice('TargetColor = '+TargetColor.ToString);
            aTarget^:= TargetColor;
            IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
            inc(aTarget);
          end;
        end
        else // 16
        begin
          Source16 := PWord(aSource);
          For sx := 0 to MaxWidth do
          begin
            //TargetColor.Red := Round(aSource^ * 63 / 255 );
            TargetColor.Red := MulDiv(SwapEndian(Source16^), 255, 65535); Inc(Source16);
            TargetColor.Green := MulDiv(SwapEndian(Source16^), 255, 65535); Inc(Source16);
            TargetColor.Blue := MulDiv(SwapEndian(Source16^), 255, 65535);Inc(Source16);
            TargetColor.Alpha := MulDiv(SwapEndian(Source16^), 255, 65535);Inc(Source16);
            { if GammaCorrection then
            begin
              TargetColor.Red := GammaTable[TargetColor.Red];
              TargetColor.Green := GammaTable[TargetColor.Green];
              TargetColor.Blue := GammaTable[TargetColor.Blue];
            end; }
            aTarget^:= TargetColor;
            IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
            inc(aTarget);
          end;
        end;
      end;
    end;
  end;

  procedure ConvertRowInterlacedColorData(aSource : PByte; aTarget: PBZColor; Mask : Word);
  var
    sx : Integer;
    TargetColor : TBZColor;
    Source16 : PWord;
	  BitRun : Byte;
  begin
    BitRun := $80;
    Case PNGHeader.ColorType of
      COLOR_GRAYSCALE :
      begin
        //GlobalLogger.logNotice('Convert Interlaced Row ColorData : GRAYSCALE, '+PNGHeader.BitDepth.ToString+' bits');
        Case PNGHeader.BitDepth of
          1  :
          begin
            Color1:=clrBlack;
            Color2:=clrWhite;
            sx := 0;
            While (sx<=MaxWidth) do
            begin
              if Boolean(Mask and BitRun) then
           	  begin
                Idx := Byte(ASource^ shl (sx and 7)) shr 7; //ExtractPixel1Bit(PByte(ASource+(sx div 8))^,sx);
                if Idx = 0 then  aTarget^:=Color1 else aTarget^:=Color2;
                IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
                if sx and 7=7 then Inc(ASource);
                inc(sx);
              end;
              BitRun := RorByte(BitRun);
              inc(aTarget);
            end;
          end;
          2 :
          begin
            sx := 0;
            While (sx<=MaxWidth) do
            begin
              if Boolean(Mask and BitRun) then
           	  begin
                Idx := Round(((ASource^ shr ((not sx and 3) shl 1)) and $3)); //ExtractPixel2Bit
                Case Idx of
                  0 : TargetColor := clrBlack;
                  1 : TargetColor := clrGray;
                  2 : TargetColor := clrLtGray;
                  3 : TargetColor := clrWhite;
                end;
                aTarget^:= TargetColor;
                IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
                if sx and 3=3 then Inc(ASource);
                inc(sx);
              end;
              BitRun := RorByte(BitRun);
              inc(aTarget);
            end;
          end;
          4  :
          begin
            sx := 0;
            While (sx<=MaxWidth) do
            begin
              if Boolean(Mask and BitRun) then
           	  begin
                Idx := Round(((ASource^ shr ((not sx and 1) shl 2)) and $f)); //ExtractPixel4Bits
                temp := (Idx * 255) div 16;
                ////GlobalLogger.LogNotice(' Idx = '+Idx.ToString+ ' = ' + Temp.ToString);
                TargetColor.Create(Temp,Temp,Temp);

                aTarget^:= TargetColor;
                IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
                if Boolean(sx and 1) then Inc(ASource);
                inc(sx);
              end;
              BitRun := RorByte(BitRun);
              inc(aTarget);
            end;
          end;
          8  :
          begin
            For sx := 0 to MaxWidth do
            begin
            	if Boolean(Mask and BitRun) then
            	begin
                TargetColor.Create(aSource^, aSource^, aSource^);
            		aTarget^:= TargetColor;
                inc(aSource);
            	end;
            	BitRun := RorByte(BitRun);
            	inc(aTarget);
            end;
          end;
          16 :
          begin
            Source16 := PWord(ASource);
            For sx := 0 to MaxWidth do
            begin
            	if Boolean(Mask and BitRun) then
            	begin
                Idx := MulDiv(SwapEndian(Source16^), 255, 65535);
                TargetColor.Create(Idx,Idx,Idx);
                aTarget^:= TargetColor;
                IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
                Inc(Source16);
            	end;
            	BitRun := RorByte(BitRun);
            	inc(aTarget);
            end;
          end;
        end;
      end;
      COLOR_RGB :  // RGB  24 bits
      begin
        //GlobalLogger.logNotice('Convert Interlaced Row ColorData : RGB, '+PNGHeader.BitDepth.ToString+' bits');
        if PNGHeader.BitDepth = 8 then
        begin
          For sx := 0 to MaxWidth do
          begin
		        if Boolean(Mask and BitRun) then
		        begin
			        TargetColor.Red := aSource^;
			        inc(aSource);
			        TargetColor.Green := aSource^;
			        inc(aSource);
			        TargetColor.Blue := aSource^;
			        inc(aSource);
			        TargetColor.Alpha := 255;
			        aTarget^:= TargetColor;
		        end;
		        BitRun := RorByte(BitRun);
		        inc(aTarget);
		      end;
        end
        else // 16 // 48 bits
        begin
          Source16 := PWord(aSource);
          For sx := 0 to MaxWidth do
          begin
            if Boolean(Mask and BitRun) then
            begin
              TargetColor.Red := MulDiv(SwapEndian(Source16^), 255, 65535); Inc(Source16);
              TargetColor.Green := MulDiv(SwapEndian(Source16^), 255, 65535); Inc(Source16);
              TargetColor.Blue := MulDiv(SwapEndian(Source16^), 255, 65535);Inc(Source16);
              TargetColor.Alpha := 255;
              aTarget^:= TargetColor;
            end;
            BitRun := RorByte(BitRun);
            inc(aTarget);
          end;
        end;
      end;
      COLOR_PALETTE :  // Indexed
      begin
        //GlobalLogger.logNotice('Convert Interlaced Row ColorData : INDEXED, '+PNGHeader.BitDepth.ToString+' bits');
        Case PNGHeader.BitDepth of
          1  :
          begin
            Color1:=clrBlack;
            Color2:=clrWhite;
            if (ImageDescription.PaletteCount>0) then
            begin
              Color1.AsInteger:= ImageDescription.PaletteEntries^[0].AsInteger;
              Color2.AsInteger:= ImageDescription.PaletteEntries^[1].AsInteger;
            end else AddError(Format(rsBitmapBadPaletteIndex,[Idx]));

            sx := 0;
            While (sx<=MaxWidth) do
            begin
              if Boolean(Mask and BitRun) then
            	begin
                Idx := Byte(ASource^ shl (sx and 7)) shr 7;
                if Idx = 0 then  aTarget^:=Color1 else aTarget^:=Color2;
                IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
                if sx and 7=7 then Inc(ASource);
                inc(sx);
              end;
              BitRun := RorByte(BitRun);
              inc(aTarget);
            end;
          end;
          2  :
          begin
            sx := 0;
            While (sx<=MaxWidth) do
            begin
              if Boolean(Mask and BitRun) then
            	begin
                Idx := Round(((ASource^ shr ((not sx and 3) shl 1)) and $3)); //ExtractPixel2Bit
                if (ImageDescription.PaletteCount>0) and  (Idx<ImageDescription.PaletteCount) then
                begin
                  aTarget^.AsInteger:= ImageDescription.PaletteEntries^[Idx].AsInteger;
                  IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
                end else AddError(Format(rsBitmapBadPaletteIndex,[Idx]));
                IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
                if sx and 3=3 then Inc(ASource);
                inc(sx);
              end;
              BitRun := RorByte(BitRun);
              inc(aTarget);
            end;
          end;
          4  :
          begin
            sx := 0;
            While (sx<=MaxWidth) do
            begin
              if Boolean(Mask and BitRun) then
            	begin
                Idx := Round(((ASource^ shr ((not sx and 1) shl 2)) and $f)); //ExtractPixel4Bits
                temp := (Idx * 255) div 16;
                ////GlobalLogger.LogNotice(' Idx = '+Idx.ToString+ ' = ' + Temp.ToString);

                if (ImageDescription.PaletteCount>0) and (Idx<ImageDescription.PaletteCount) then
                begin
                   TargetColor.AsInteger:= ImageDescription.PaletteEntries^[Idx].AsInteger;
                end
                else
                begin
                  if Idx<16 then
                  begin
                    Case Idx of
                      0 : TargetColor := clrBlack;
                      1 : TargetColor := clrMaroon;
                      2 : TargetColor := clrGreen;
                      3 : TargetColor := clrOlive;
                      4 : TargetColor := clrNavy;
                      5 : TargetColor := clrPurple;
                      6 : TargetColor := clrTeal;
                      7 : TargetColor := clrGray;
                      8 : TargetColor := clrSilver;
                      9 : TargetColor := clrRed;
                      10 : TargetColor := clrLime;
                      11 : TargetColor := clrYellow;
                      12 : TargetColor := clrBlue;
                      13 : TargetColor := clrFuchsia;
                      14 : TargetColor := clrAqua;
                      15 : TargetColor := clrWhite;
                    end;
                  end else AddError(Format(rsBitmapBadPaletteIndex,[Idx]));
                end;// else AddError(Format(rsBitmapBadPaletteIndex,[Idx]));
                aTarget^:= TargetColor;
                IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
                if Boolean(sx and 1) then Inc(ASource);
                inc(sx);
              end;
              BitRun := RorByte(BitRun);
              inc(aTarget);
            end;
          end;
          8  :
          begin
            sx := 0;
            While (sx <= MaxWidth) do
            begin
              if Boolean(Mask and BitRun) then
            	begin
                Idx:=aSource^;
                if (ImageDescription.PaletteCount>0) and (Idx<ImageDescription.PaletteCount) then
                begin
                   TargetColor.AsInteger:= ImageDescription.PaletteEntries^[Idx].AsInteger;
                end else AddError(Format(rsBitmapBadPaletteIndex,[Idx]));

                aTarget^ := TargetColor;
                IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
                inc(aSource);
                inc(sx);
              end;
              BitRun := RorByte(BitRun);
              inc(aTarget);
            end;
          end;
          { 16 :  // Ce format existe-t-il dans des fichiers mal formés ????
          begin
            Source16 := PWord(ASource);
            For sx := 0 to MaxWidth do
            begin
              if Boolean(Mask and BitRun) then
            	begin
                Idx := MulDiv(SwapEndian(Source16^), 255, 65535); // ??? ou juste Source16^ ???
                if (ImageDescription.PaletteCount>0) and (Idx<ImageDescription.PaletteCount) then
                begin
                   TargetColor.AsInteger:= ImageDescription.PaletteEntries^[Idx].AsInteger;
                end else AddError(Format(rsBitmapBadPaletteIndex,[Idx]));
                aTarget^ := TargetColor;
                IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
                Inc(Source16);
            	end;
            	BitRun := RorByte(BitRun);
            	inc(aTarget);
            end;
          end; }
        end;
      end;
      COLOR_GRAYSCALEALPHA : // RGBA Gray Scale
      begin
        //GlobalLogger.logNotice('Convert Interlaced Row ColorData : GRAYSCALEALPHA, '+PNGHeader.BitDepth.ToString+' bits');
        if PNGHeader.BitDepth = 8 then
        begin
          For sx := 0 to MaxWidth do
          begin
            if Boolean(Mask and BitRun) then
            begin
              TargetColor.Create(aSource^, aSource^, aSource^);
              inc(aSource);
              TargetColor.Alpha := aSource^;
              inc(aSource);
              aTarget^:= TargetColor;
              IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
            end;
            BitRun := RorByte(BitRun);
            inc(aTarget);
          end;
        end
        else // 16
        begin
          Source16 := PWord(aSource);
          For sx := 0 to MaxWidth do
          begin
            if Boolean(Mask and BitRun) then
            begin
              Temp := MulDiv(SwapEndian(Source16^), 255, 65535);
              TargetColor.Create(Temp, Temp,Temp);
              Inc(Source16);
              TargetColor.Alpha := MulDiv(SwapEndian(Source16^), 255, 65535);
              Inc(Source16);
              aTarget^:= TargetColor;
              IgnoreAlpha := IgnoreAlpha and (aTarget^.alpha = 0);
            end;
            BitRun := RorByte(BitRun);
            inc(aTarget);
          end;
        end;
      end;
      COLOR_RGBA:  // RGBA
      begin
        //GlobalLogger.logNotice('Convert Interlaced Row ColorData : RGBA, '+PNGHeader.BitDepth.ToString+' bits');
        if PNGHeader.BitDepth = 8 then  // 32 bits
        begin
          For sx := 0 to MaxWidth do
          begin
		        if Boolean(Mask and BitRun) then
		        begin
			        TargetColor.Red := aSource^;
			        inc(aSource);
			        TargetColor.Green := aSource^;
			        inc(aSource);
			        TargetColor.Blue := aSource^;
			        inc(aSource);
			        TargetColor.Alpha := aSource^;
			        inc(aSource);
              aTarget^:= TargetColor;
		        end;
		        BitRun := RorByte(BitRun);
		        inc(aTarget);
		      end;
        end
        else // 16   // 64 bits
        begin
          Source16 := PWord(aSource);
          For sx := 0 to MaxWidth do
          begin
            if Boolean(Mask and BitRun) then
		        begin
              TargetColor.Red := MulDiv(SwapEndian(Source16^), 255, 65535);
              Inc(Source16);
              TargetColor.Green := MulDiv(SwapEndian(Source16^), 255, 65535);
              Inc(Source16);
              TargetColor.Blue := MulDiv(SwapEndian(Source16^), 255, 65535);
              Inc(Source16);
              TargetColor.Alpha := MulDiv(SwapEndian(Source16^), 255, 65535);
              Inc(Source16);
              aTarget^:= TargetColor;
            end;
		        BitRun := RorByte(BitRun);
            inc(aTarget);
          end;
        end;
      end;
    end;
  end;

begin
  //GlobalLogger.LogNotice('Decode Data');
  ZData.position := 0;
  //IsOpaque := False;
  IgnoreAlpha := False;
  Color1 := clrBlack;
  Color2 := clrWhite;
  //GlobalLogger.LogNotice('ZData Buffer Size : '+ZData.Size.ToString);
  RowBuffer[False] := nil;
  RowBuffer[True] := nil;
  EvenRow := False;
  SourceBPP := SetupColorDepth(PNGHeader.ColorType, PNGHeader.BitDepth);

  if PNGHeader.BitDepth = 16 then TargetBPP := SourceBPP div 2
  else TargetBPP := SourceBPP;

  //TargetBPP=4;

  //GlobalLogger.LogNotice('SourceBPP : '+ SourceBPP.toString + ' TargetBPP : '+TargetBPP.ToString);
  aBytesPerRow := TargetBPP * ((Width * PNGHeader.BitDepth + 7) div 8) + 1;
  ////GlobalLogger.LogNotice('BytesPerRow : '+aBytesPerRow.ToString);

  ReAllocMem(RowBuffer[True] , aBytesPerRow);
  ReAllocMem(RowBuffer[False], aBytesPerRow);
 // ReAllocMem(TargetLine, BytesPerRow);

  ZData.position := 0;
  DecompressStream := TDecompressionStream.Create(ZData);
  //DecompressStream.SourceOwner := False;
  DecompressStream.Position := 0;
  try
    if PNGHeader.Interlacing = 1 then  // Image entrlacée
    begin
      //GlobalLogger.LogNotice('Process Interlaced ADAM 7');
      for Pass := 0 to 6 do
      begin
        // prepare next interlace run
        if Width <= cInterlaced_Adam7_ColumnStart[Pass] then  Continue;
        InterlaceWidth := (Width + cInterlaced_Adam7_ColumnIncrement[Pass] - 1 - cInterlaced_Adam7_ColumnStart[Pass]) div cInterlaced_Adam7_ColumnIncrement[Pass];
        InterlaceRowBytes := TargetBPP * ((InterlaceWidth * PNGHeader.BitDepth + 7) div 8) + 1;

        Row := cInterlaced_Adam7_RowStart[Pass];
        while Row < Self.Height do
        begin
          PixPtr := GetScanLine(Row);
          //ReadRow(Source, RowBuffer[EvenRow], InterlaceRowBytes);
          DecompressStream.Read(RowBuffer[EvenRow]^, InterlaceRowBytes);

          //ApplyFilter(Filter: Byte; Line, PrevLine, Target: PByte; BPP, BytesPerRow: Integer);
          ApplyFilter(Byte(RowBuffer[EvenRow]^),
                      PByte(RowBuffer[EvenRow] + 1),
                      PByte(RowBuffer[not EvenRow] + 1),
                      PByte(RowBuffer[EvenRow] + 1),
                      SourceBPP,
                      InterlaceRowBytes - 1);

          ConvertRowInterlacedColorData(PByte(RowBuffer[EvenRow] + 1), PixPtr, cInterlaced_Adam7_PassMask[Pass]);

          EvenRow := not EvenRow;
          // continue with next row in interlaced order
          Inc(Row, cInterlaced_Adam7_RowIncrement[Pass]);

          //if Pass = 6 then
          //begin
          //  // progress event only for last (and most expensive) pass
          //  Progress(Self, psRunning, MulDiv(Row, 100, Height), True, FProgressRect, '');
          //  OffsetRect(FProgressRect, 0, 1);
          //end;
        end;
      end;
    end
    else
    begin
      //GlobalLogger.LogNotice('Process Regular');
      for Row := 0 to Height - 1 do
      begin
        PixPtr := GetScanLine(Row);
        ////GlobalLogger.LogNotice('Read Line : '+Row.ToString);
        DecompressStream.Read(RowBuffer[EvenRow]^, aBytesPerRow);

        ApplyFilter(Byte(RowBuffer[EvenRow]^),
                    PByte(RowBuffer[EvenRow] + 1),
                    PByte(RowBuffer[not EvenRow] + 1),
                    PByte(RowBuffer[EvenRow] + 1),
                    SourceBPP,
                    aBytesPerRow - 1);

        ConvertRowColorData(PByte(RowBuffer[EvenRow]+1), PixPtr);
        //ColorManager.ConvertRow([Pointer(RowBuffer[EvenRow] + 1)], ScanLine[Row], Width, $FF);

        EvenRow := not EvenRow;

        //Progress(Self, psRunning, MulDiv(Row, 100, Height), True, FProgressRect, '');
        //OffsetRect(FProgressRect, 0, 1);
      end;
    end;
  finally
     FreeMem(RowBuffer[True]);
     FreeMem(RowBuffer[False]);
     FreeAndNil(DecompressStream);
   end;
end;

function TBZBitmapNetworkGraphicImage.ReadImageProperties : Boolean;
Var
  OldPos :Int64;

  procedure ProcessChunk_sRGB; //http://www.libpng.org/pub/png/spec/iso/index-object.html#11sRGB
  var
   b:Byte;
  begin
    if ChunkInfos.ChunkHeader.DataSize>0 then
    begin
      b := Memory.ReadByte;
      sRGBType := TBZPNGsRGBType(b);
      HassRGB := true;
      HasCIExyz :=  HasCIExyz and not(HasICCP) and not(HassRGB);
    end;
    ChunkInfos.ChunkCrc:=Memory.ReadLongWord;
    //CheckChunkCRC;
  end;

  { NB :Si un chunk de type sRGB ou iCCP est présent et reconnu, alors celui-ci remplace le bloc gAMA.
    lors de la couleur finale

    http://www.libpng.org/pub/png/spec/iso/index-object.html#11gAMA
    http://www.libpng.org/pub/png/spec/iso/index-object.html#13Decoder-gamma-handling
    http://www.libpng.org/pub/png/spec/iso/index-object.html#C-GammaAppendix
    http://www.libpng.org/pub/png/spec/1.2/PNG-GammaAppendix.html
  }
  procedure ProcessChunk_gAMA;
  Var
    vGamma : LongWord;
    i : Integer;
  begin
    //GlobalLogger.LogNotice('Process Chunk gAMA');
    vGamma := Memory.ReadLongWord;
    //GlobalLogger.LogNotice('vGamma Factor = '+vGamma.ToString);
    GammaFactor :=  (vGamma / 1000000000);
    //GlobalLogger.LogNotice('Gamma Factor = '+GammaFactor.ToString);
    GammaFactor :=  1 / GammaFactor;
    //GlobalLogger.LogNotice('Inv Gamma Factor = '+GammaFactor.ToString);
    //if GammaFactor < 1.0 then GammaFactor := GammaFactor * 10;
    GammaFactor :=  GammaFactor * _DefaultGammaFactor;
    //GlobalLogger.LogNotice('Corrected Gamma Factor = '+GammaFactor.ToString);

    GammaCorrection := True;

   // If GammaFactor < 0.1 Then
   //   GammaFactor := 10
   // Else
   //   GammaFactor := 1 / GammaFactor; // * _DefaultGammaFactor;

    if PNGHeader.BitDepth = 16 then
    begin
      for I := 0 to 65534 do
      begin
        GammaTable16[I] := Round(65534 * Math.Power(i * (1 / 65534), GammaFactor));
        //Round(Power((I / 65535), 1 / (GammaFactor * 2.2)) * 65535);
       // gamma :=
        //InverseGammaTable[Round(Power((I / 255), 1 / (GammaFactor * 2.2)) * 255)] := I;
      end;
    end
    else
    begin
      for I := 0 to 255 do
      begin
        GammaTable[I] := Round(255 * Math.Power(i * _FloatColorRatio, GammaFactor));
        //Round(Power((I / 255), 1 / (GammaFactor * 2.2)) * 255);
       // gamma :=
        //InverseGammaTable[Round(Power((I / 255), 1 / (GammaFactor * 2.2)) * 255)] := I;
      end;
    end;

    {Create gamma table and inverse gamma table (for saving)}
    ChunkInfos.ChunkCrc:=Memory.ReadLongWord;
    //CheckCRC
  end;

  { NB :Si un chunk de type sRGB ou iCCP est présent et reconnu, alors celui-ci remplace le bloc cHRM.
    lors de la couleur finale

    http://www.libpng.org/pub/png/spec/iso/index-object.html#11cHRM
    http://www.libpng.org/pub/png/spec/1.2/PNG-ColorAppendix.html
  }
  procedure ProcessChunk_cHRM;
  begin
    //GlobalLogger.LogNotice('Process Chunk cHRM');
    ReadChunkData;
    CIExyz := TBZPNGChunk_cHRM(ChunkInfos.ChunkData^);
    HasCIExyz :=  True;
    HasCIExyz :=  HasCIExyz and not(HasICCP) and not(HassRGB);

  end;

  procedure ProcessChunk_sBIT;  // http://www.libpng.org/pub/png/spec/iso/index-object.html#11sBIT
  var
    rb,gb,bb,ab:byte;
  begin
    //GlobalLogger.LogNotice('Process Chunk sBIT');
    SkipChunkData;
   { Case PNGHeader.ColorType of
      0:
      begin
        sGrayBits := Memory.ReadByte;
      end;
      2, 3 :
      begin
        sRedBits := Memory.ReadByte;
        sGreenBits := Memory.ReadByte;
        sBlueBits := Memory.ReadByte;
      end;
      4:
      begin
        sGrayBits := Memory.ReadByte;
        sAlphaBits := Memory.ReadByte;
      end;
      6:
      begin
        sRedBits := Memory.ReadByte;
        sGreenBits := Memory.ReadByte;
        sBlueBits := Memory.ReadByte;
        sAlphaBits := Memory.ReadByte;
      end;
    end;
    ChunkInfos.ChunkCrc:=Memory.ReadLongWord;
    // CheckCRC;
    sBits := True;}
  end;

  procedure ProcessChunk_iCCP; // http://www.libpng.org/pub/png/spec/iso/index-object.html#11iCCP
  Var
    BytesRead : LongWord;
    s : String;
    c : Char;
    l : Integer;
  begin
    //GlobalLogger.LogNotice('Process Chunk iCCP');
    fillChar(iCCP_Profil.name,79,' ');
    s := '';

    c := Memory.ReadChar;
    l:=1;
    While c<>#0 do
    begin
      s :=s + c;
      c := Memory.ReadChar;
      inc(l);
    end;
    iCCP_Profil.name := S;
    //Memory.Read(iCCP_Profil.name,79);
    ////GlobalLogger.LogNotice('iCCP Profil Name : ' + String(iCCP_Profil.name));
    iCCP_Profil.CompressMethod := Memory.ReadByte;
    inc(l);
    ////GlobalLogger.LogNotice('iCCP Header : '+l.ToString);
    ////GlobalLogger.LogNotice('iCCP Compress method : ' + iCCP_Profil.CompressMethod.ToString);
    iCCP_Profil.Profil := nil;
    ////GlobalLogger.LogNotice('iCCP Profil Data Size : ' + (ChunkInfos.ChunkHeader.DataSize - l).ToString);
    if ((ChunkInfos.ChunkHeader.DataSize-l) > 0) then
    begin
      ReAllocMem(iCCP_Profil.Profil, ChunkInfos.ChunkHeader.DataSize-l);
      Memory.Read(iCCP_Profil.Profil^,ChunkInfos.ChunkHeader.DataSize-l);
    end;
    ChunkInfos.ChunkCrc:=Memory.ReadLongWord;
    //CheckCRC
    HasICCP := True;
    HasCIExyz :=  HasCIExyz and not(HasICCP) and not(HassRGB);
  end;

  //Procedure ProcessChunk_pHYs;

  procedure ProcessChunk_tEXT; // chunk "zTXt" idem mais compressé
  Var
    KeyWord : Array[0..78] of Char;
    { Mot clefs valides :
    Title            Short (one line) title or caption for image
    Author           Name of image's creator
    Description      Description of image (possibly long)
    Copyright        Copyright notice
    Creation Time    Time of original image creation
    Software         Software used to create the image
    Disclaimer       Legal disclaimer
    Warning          Warning of nature of content
    Source           Device used to create the image
    Comment          Miscellaneous comment; conversion from
                     GIF comment
    }
    c : Char;
    NullSep : Byte;
    S, AText : String; // Longueur definie par l'en-tete du chunk -> DataSize
    l : Integer;
  begin
    //GlobalLogger.LogNotice('Process Chunk tEXT');
    fillChar(Keyword,79,' ');
    s := '';

    c := Memory.ReadChar;
    l:=1;
    While (c<>#0) and (c<>':') do  // Adobe FireWorks ne suit pas les recommandation on doit donc tester la présence des ':' si on veut éviter des erreurs de lecture
    begin
      s :=s + c;
      c := Memory.ReadChar;
      inc(l);
    end;

    KeyWord := String(S);
    Atext := '';
    ////GlobalLogger.LogNotice('TextSize = '+(ChunkInfos.ChunkHeader.DataSize - l).ToString);
    if (ChunkInfos.ChunkHeader.DataSize-l>0) then
    begin
      AText := Memory.ReadString((ChunkInfos.ChunkHeader.DataSize - l));
      Memory.SkipNextByte();
    end;
    ////GlobalLogger.LogNotice('KeyWord = '+KeyWord+ ' : ' +AText);

    //ImageDescription.ExtraInfos.Add(String(Keyword) + ' : ' +AText);
    ChunkInfos.ChunkCrc := Memory.ReadLongWord;
    // CheckCRC;
  end;

  {  A zTXt chunk begins with an uncompressed Latin-1 keyword
   followed by a null (0) character, just as in the tEXt chunk. The
   next byte after the null contains a compression type byte, for
   which the only presently legitimate value is zero (deflate/inflate
   compression). The compression-type byte is followed by a
   compressed data stream which makes up the remainder of the
   chunk. Decompression of this data stream yields Latin-1 text
   which is equivalent to the text stored in a tEXt chunk.
  }

  { 4.2.3.3. iTXt International textual data

    This chunk is semantically equivalent to the tEXt and zTXt chunks, but the textual data is in the UTF-8 encoding of the Unicode character set instead of Latin-1. This chunk contains:

       Keyword:             1-79 bytes (character string)
       Null separator:      1 byte
       Compression flag:    1 byte
       Compression method:  1 byte
       Language tag:        0 or more bytes (character string)
       Null separator:      1 byte
       Translated keyword:  0 or more bytes
       Null separator:      1 byte
       Text:                0 or more bytes
    The keyword is described above.

    The compression flag is 0 for uncompressed text, 1 for compressed text. Only the text field may be compressed. The only value presently defined for the compression method byte is 0, meaning zlib datastream with deflate compression. For uncompressed text, encoders should set the compression method to 0 and decoders should ignore it.
    The language tag [RFC-1766] indicates the human language used by the translated keyword and the text. Unlike the keyword, the language tag is case-insensitive. It is an ASCII [ISO-646] string consisting of hyphen-separated words of 1-8 letters each (for example: cn, en-uk, no-bok, x-klingon). If the first word is two letters long, it is an ISO language code [ISO-639]. If the language tag is empty, the language is unspecified.
    The translated keyword and text both use the UTF-8 encoding of the Unicode character set [ISO/IEC-10646-1], and neither may contain a zero byte (null character). The text, unlike the other strings, is not null-terminated; its length is implied by the chunk length.
    Line breaks should not appear in the translated keyword. In the text, a newline should be represented by a single line feed character (decimal 10). The remaining control characters (1-9, 11-31, and 127-159) are discouraged in both the translated keyword and the text. Note that in UTF-8 there is a difference between the characters 128-159 (which are discouraged) and the bytes 128-159 (which are often necessary).
    The translated keyword, if not empty, should contain a translation of the keyword into the language indicated by the language tag, and applications displaying the keyword should display the translated keyword in addition.
  }

 //procedure ProcessChunk_tIME;

Begin
  //GlobalLogger.LogNotice('Read Image Properties');
  Result := True;
  GammaCorrection := False;
  HasCIExyz := False;
  HassRGB := False;
  HasICCP := False;
  ReadChunkHeader;
  Case ImageType of
    ftPNG:
    begin
      //GlobalLogger.LogNotice('PNG Header detected');
      if (ChunkInfos.ChunkType<>ctIHDR) then // or (ChunkType<>ctfcTL) then
      begin
        Result:=false;
        Errors.Add('Fichier PNG en-tête non valide');
        Exit;
      end;
      ReadChunkData;
      PNGHeader := PBZPNGChunk_IHDR(ChunkInfos.ChunkData)^;
      //if ChunkInfos.ChunkData<>nil then FreeMem(ChunkInfos.ChunkData);
      {$IFDEF ENDIAN_LITTLE}
      PNGHeader.Width := BEToN(PNGHeader.Width);
      PNGHeader.Height := BEToN(PNGHeader.height);
      {$ENDIF}
      if not(PNGHeader.ColorType in [0,2,3,4,6]) then
      begin
        Result:=false;
        Errors.Add('Fichier PNG Format de couleur non supporté.');
        Exit;
      end;
      if PNGHEader.Compression<>0 then
      begin
        Result:=false;
        Errors.Add('Fichier PNG Format de compression non supporté.');
        Exit;
      end;

      Case PNGHeader.ColorType of
        COLOR_GRAYSCALE : BitCount := PNGHeader.BitDepth;   // 1,2,4,8 ou 16 bits
        COLOR_RGB : BitCount := PNGHeader.BitDepth * 3; // 24 ou 48 bits
        COLOR_PALETTE : BitCount := PNGHeader.BitDepth;   // Image indexée. 1, 2, 4 ou 8 Bits
        COLOR_GRAYSCALEALPHA : BitCount := PNGHeader.BitDepth * 2; // Image en Niveaux de gris + Alpha. 16 ou 32 bits
        COLOR_RGBA : BitCount := PNGHeader.BitDepth * 4; // Image ARGB 32 ou 64 bits
      end;
      // On met à jour la description du ImageDescription
      // On initialise la descritption du "ImageDescription"
      //GlobalLogger.LogNotice('Description.InitDefault: '+InttoStr(PNGHeader.Width)+'x'+InttoStr(PNGHeader.Height)+'x'+InttoStr(BitCount)+' Type : '+Inttostr(PNGHeader.ColorType));
      ImageDescription.InitDefault(PNGHeader.Width, PNGHeader.Height, BitCount);
      HasTransparency := False;
      With ImageDescription do
      Begin
         Interlaced := (PNGHeader.Interlacing = 1);
         Case PNGHeader.ColorType of
           COLOR_GRAYSCALE :
           begin
             if BitCount = 1 then ColorFormat := cfMono
             else ColorFormat := cfRGB; //cfGray
           end;
           COLOR_RGB : ColorFormat := cfRGB;
           COLOR_PALETTE : ColorFormat := cfIndexed;
           COLOR_GRAYSCALEALPHA :
           begin
             ColorFormat := cfGrayAlpha;
             HasTransparency := True;
           end;
           COLOR_RGBA :
           begin
             ColorFormat := cfRGBA;
             HasTransparency := True;
           end;
         end;
      end;

      { On doit parcourir tout le fichier à la recherche des chunks d'informations }
      OldPos := Memory.Position;
      While (ChunkInfos.ChunkType<>ctiEND) and (ChunkInfos.ChunkType<>ctiEND2) and (ChunkInfos.ChunkType<>ctIdND) do
      begin
        ReadChunkHeader;
        if (ChunkInfos.ChunkType<>ctiEND) and (ChunkInfos.ChunkType<>ctiEND2) and (ChunkInfos.ChunkType<>ctIdND) then
        begin
          Case ChunkInfos.ChunkType of
            ctICCP : processChunk_ICCP;
            cttExt : ProcessChunk_tEXT;
            //ctzTXT : ProcessChunk_zTXT;
            //ctiTXT : ProcessChunk_iTXT;
            ctcHRM : ProcessChunk_cHRM;
            ctsBit : ProcessChunk_sBit;
            //cttIME  : ProcessChunk_tIME;
            //ctpHYs  : ProcessChunk_pHYs;
            ctsRGB : processChunk_sRGB;
            ctgAMA : ProcessChunk_gAMA;
            else SkipChunkData;
          end;
        end;
      end;
      // On se replace à notre position initiale
      Memory.Seek(OldPos, soBeginning);
    end;
    ftMNG:
    begin
      //GlobalLogger.LogWarning('MNG Header detected');
      MNGHeader := PBZPNGChunk_MHDR(ChunkInfos.ChunkData)^;

    end;
    ftJNG:
    begin
      //GlobalLogger.LogWarning('JNG Header detected');
      JNGHeader := PBZPNGChunk_JHDR(ChunkInfos.ChunkData)^;

    end;
    //ftAPNG
  end;

  // On met à jours les infos sur le format de fichier
  With DataFormatDesc Do
  Begin
    Version := '-';
    Encoding := etNone;
    if (ImageType = ftPNG) then if PNGHeader.Compression=1 then Encoding := etLZ77
    else if (ImageType = ftMNG) then Encoding := etNone
    else if (ImageType = ftJNG) then
    begin
      Case JNGHeader.Compression of
        1 : Encoding := etJPEG;
        8 : Encoding := etHuffman;
      end;
    end;
  End;

  if (ImageType = ftMNG) or (ImageType = ftJNG) then
  begin
    Result := false;
    Exit;
  end;

End;

function TBZBitmapNetworkGraphicImage.CheckFormat() : Boolean;
Var
  MagicID : TBZPNGMagicID;

Begin
  //GlobalLogger.LogNotice('Check format');
  MagicID := cNULL_MagicID;
  Result :=  False;
  Memory.Read(MagicID,8);
  if CompareMem(@MagicID, @cPNG_MagicID, 8) then
  begin
    ImageType := ftPNG;
    Result:=true;
  end
  else if CompareMem(@MagicID, @cMNG_MagicID, 8) then
  begin
    ImageType := ftMNG;
    Result:=true;
  end
  else if CompareMem(@MagicID, @cJNG_MagicID, 8) then
  begin
    ImageType := ftJNG;
    Result:=true;
  end;

  if Result then Result := ReadImageProperties;

End;

procedure TBZBitmapNetworkGraphicImage.LoadFromMemory();
Var
 Chunk_PLTE_Ok : Boolean;

  procedure ProcessChunk_PLTE;
  Var
   MaxCols, i : Integer;
   r,g,b : Byte;
   pcolor : TBZColor;
  begin
    //GlobalLogger.LogNotice('Process Chunk PLTE');
    MaxCols := ChunkInfos.ChunkHeader.DataSize div 3;
    ImageDescription.UsePalette := True;
    ImageDescription.PaletteCount := MaxCols;
    if (MaxCols > 256) then Errors.Add('Too Many Colors');
    //GlobalLogger.LogNotice('Load :' + MaxCols.ToString + ' Colors');
    if MaxCols > 0 then
    begin
      //if FGlobalPalette =  nil then FGlobalPalette := TBZColorList.Create else FGlobalPalette.Clear;

      for i := 0 to MaxCols - 1 do
      begin
        R := Memory.ReadByte;
        G := Memory.ReadByte;
        B := Memory.ReadByte;
        pColor.Create(r,g,b);
        //FGlobalPalette.AddColor(pColor);
        ImageDescription.PaletteEntries^[i] := pColor;
      end;
      ImageDescription.PaletteCount := MaxCols;
    end;
    ChunkInfos.ChunkCrc:=Memory.ReadLongWord;
    //CheckCrc;
    Chunk_PLTE_Ok:=True;
  end;

  procedure ProcessChunk_tRNS;
  var
    AlphaValue : Byte;
    maxCols, i : Integer;
    RGBColor : TBZColorRGB_24;
    RGBAColor : TBZColor;
    rw,gw,bw : Word;
  begin
    //GlobalLogger.LogNotice('Process Chunk tRNS');

    Case PNGHeader.ColorType of
       0:  // MonoChrome / Niveaux de gris
       begin
         rw := Memory.ReadWord;
         ATransparentColorIndex := (rw and $FFFF) shr 8;
         // TODO : Utiliser une palette locale pour la gestion de la transparence
         ImageDescription.PaletteCount := ATransparentColorIndex + 1;
         ImageDescription.PaletteEntries^[ATransparentColorIndex].Alpha := 0;
         ImageDescription.HasAlpha := True;
         ChunkInfos.ChunkCrc:=Memory.ReadLongWord;
       end;
       2:
       begin
         rw := Memory.ReadWord;
         gw := Memory.ReadWord;
         bw := Memory.ReadWord;
         ATransparentColor.Alpha := 255;
         ATransparentColor.Red := MulDiv(SwapEndian(rw),255,65535);
         ATransparentColor.Green := MulDiv(SwapEndian(gw),255,65535);
         ATransparentColor.Blue := MulDiv(SwapEndian(bw),255,65535);
         ImageDescription.HasAlpha := True;
         ChunkInfos.ChunkCrc:=Memory.ReadLongWord;
         //SkipChunkData;
       end;
       3 :  // Indexé
       begin
         if Chunk_PLTE_Ok then
         begin
           ImageDescription.HasAlpha := True;
           //GlobalLogger.LogNotice('Indexed');
           if ChunkInfos.ChunkHeader.DataSize > 0 then
           begin
             MaxCols := ChunkInfos.ChunkHeader.DataSize;
             ////GlobalLogger.LogNotice('Nbre Transparent Colors : '+MaxCols.ToString);
             // if (MaxCols > 256) or (MaxCols>FGlobalPalette.Count) then Errors.Add('Too Many Colors');
              For i := 0 to MaxCols -1 do
              begin
                AlphaValue := Memory.ReadByte - 1;
                ////GlobalLogger.LogNotice('Idx : '+i.ToString + ' AlphaValue : '+AlphaValue.ToString);
                if (AlphaValue < ImageDescription.PaletteCount) then
                begin
                  ImageDescription.PaletteEntries^[AlphaValue].Alpha := 0;
                  ImageDescription.HasAlpha := True;
                end;
              end;
           end;
           ChunkInfos.ChunkCrc:=Memory.ReadLongWord;
         end
         else
         begin
           ////GlobalLogger.LogNotice('Skip Transparence');
           //Errors.Add
           SkipChunkData;
         end;
       end;
       4, 6 :; //RGBA 32/64 bits / Niveaux de gris 16bits Gray-Alpha --> Pas besoins de ce chunk
     end;

    //CheckCrc;
  end;

  { Le chunk bKGD :
    - Apparait qu'un seule fois et obligatoirement après le chunk PLTE dans le cas d'un format indexé (2,4,8,16 bits) }
  procedure ProcessChunk_bKGD;
  Var
    Index : Byte; // ColorType 2, 5
    Value : Word;  // ColorType 0, 4
    vR, vG, vB, vA : Word; // ColorType 2,6
  begin
    //GlobalLogger.LogNotice('Process Chunk bKGD');
    if Chunk_PLTE_Ok then
    begin
      // resultat Couleur RGBA. Canal Alpha en fonction du chunk tRNS (Transparence)
      //ReadChunkData;
      //GlobalLogger.LogNotice('PNG Color Type = '+PNGHeader.ColorType.ToString);
      Case PNGHeader.ColorType of
        0,4:
        begin
          Value := Memory.ReadWord;
          if ImageDescription.BitCount<16 then
          begin
            Index:=Lo(Value);
            With BackgroundColor do
            begin
              Red:=Index;
              Green:=Index;
              Blue:=Index;
  //            if HasTransparency then Alpha:=ColTrns[Idx]
  //            else
              Alpha:=255;
            end;
          end
          else
          begin
            // Conversion 16 bits vers 8 bits
            Index := MulDiv(SwapEndian(Value), 255, 65535); // (Value * 65535 div 255);
            With BackgroundColor do
            begin
              Red:= Index;
              Green:= Index;
              Blue:= Index;
  //            if HasTransparency then Alpha:=ColTrns[Idx]
  //            else
              Alpha:=255;
            end;
          end;
        end;
        3: // 8 Bits, indexé
        begin
          Index := Memory.ReadByte;
          With BackgroundColor do
          begin
            Red := ImageDescription.PaletteEntries^[Index].Red;
            Green := ImageDescription.PaletteEntries^[Index].Green;
            Blue := ImageDescription.PaletteEntries^[Index].Blue;
  //            if HasTransparency then Alpha:=ColTrns[Idx]
  //            else
            Alpha := ImageDescription.PaletteEntries^[Index].Alpha;     //255;

          end;
        end;
        2,6:  // Conversion RGB 48-->24 bits et RGBA 64-->32 bits
        begin
          vR := Memory.ReadWord;
          vG := Memory.ReadWord;
          vB := Memory.ReadWord;
          With BackgroundColor do
          begin
             // Conversion 16 bits vers 8 bits
             ////GlobalLogger.LogStatus('Read KGD values at Position  : '+InttoStr(Memory.Position));
             Red   := BZUtils.MulDiv(SwapEndian(vR), 255, 65535);//vR * 65535 div 255;
             Green := BZUtils.MulDiv(SwapEndian(vG), 255, 65535); // vG * 65535 div 255;
             Blue  := BZUtils.MulDiv(SwapEndian(vB), 255, 65535); //vB * 65535 div 255;
             Alpha := 255;
             if (ChunkInfos.ChunkHeader.DataSize>6) and(PNGHeader.ColorType = 6) then
             begin
               vA :=  Memory.ReadWord;
               Alpha  := BZUtils.MulDiv(SwapEndian(vA), 255, 65535); //vA * 65535 div 255;
             end;
          end;
        end;
      end;
      ChunkInfos.ChunkCrc:=Memory.ReadLongWord;
    end else SkipChunkData;
    //CheckCrc;
  end;

  procedure ProcessChunk_IDAT;
  begin
    //GlobalLogger.LogNotice('Process Chunk IDAT');
    ReadChunkData;
    ZData.Write(ChunkInfos.ChunkData^, ChunkInfos.ChunkHeader.DataSize);
  end;


Begin
  Chunk_PLTE_Ok := False;
  //GlobalLogger.LogNotice('Load from memory');
  Case ImageType of
    ftPNG:
    begin
      ZData := TMemoryStream.Create;
      SetSize(PNGHeader.Width, PNGHeader.Height);
      //Decoder:=TBZDataEncoderLZ77.Create(self);
      ChunkInfos.ChunkType:=ctUnknown;
      While (ChunkInfos.ChunkType<>ctiEND) and (ChunkInfos.ChunkType<>ctiEND2) and (ChunkInfos.ChunkType<>ctIdND) do
      begin
        ReadChunkHeader;
        if (ChunkInfos.ChunkType<>ctiEND) and (ChunkInfos.ChunkType<>ctiEND2 ) and (ChunkInfos.ChunkType<>ctIdND) then
        begin
          Case ChunkInfos.ChunkType of
            ctPLTE : ProcessChunk_PLTE; // Chargement palette
            ctbKGD : ProcessChunk_bKGD; // Couleur de fond
            cttRNS : ProcessChunk_tRNS; // Couleur(s) transparente
            ctIDAT :
            begin
              ProcessChunk_IDAT; // Chargement de l'image
            end;
            else SkipChunkData;  // Les autres chunks ont été traité dans "ReadImageProperties'
          end;
        end;
      end;
      if ZData.Size > 0 then DecodeData;
      FreeAndNil(ZData);
    end;
    ftMNG:
    begin

    end;
    ftJNG:
    begin

    end;
  end;
  if Assigned(FGlobalPalette) then FreeAndNil(FGlobalPalette);
  if HasICCP then if (iCCP_Profil.Profil<>nil) then FreeMem(iCCP_Profil.Profil);
  if ChunkInfos.ChunkData<>nil then FreeMem(ChunkInfos.ChunkData);
End;

{%endregion%}

Initialization
  RegisterRasterFormat('PNG', 'Portable Network Graphic image', TBZBitmapNetworkGraphicImage);
  //RegisterRasterFormat('MNG', 'Multi Network Graphic image', TBZBitmapNetworkGraphicImage);
  //RegisterRasterFormat('JNG', 'Jpeg Network Graphic image', TBZBitmapNetworkGraphicImage);
  //RegisterRasterFormat('APNG','Animated Portable Network Graphic image', TBZBitmapNetworkGraphicImage);


Finalization
  UnregisterRasterFormat(TBZBitmapNetworkGraphicImage);


end.

