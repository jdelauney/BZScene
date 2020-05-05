(*

  @abstract(Prise en charge des Images au format BMP (Windows et Osx) en lecture et écriture.)

  @bold(Spécifications) : @br
  @unorderedList(
    @item(Méthode de compression    : Aucune, encodage BitField, RLE, Huffman 1D)
    @item(Nombre de couleurs	      : 1,2,4,8,15,16,24,32,64 bits
    @item(Supporte plusieurs images : Non, une seule image dans un même fichier)
    @item(Format des nombres	      : Mixte
    @item(Auteur                    : OSx, Windows)
    @item(Extensions                : *.bmp, *.rle, *.dib)
    @item(Version                   : de 1 à 5)
  )

  -------------------------------------------------------------------------------------------------------------

  @created(2017-04-17)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(17/04/2017 : Creation  )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) : @br
    Informations sur le format BMP : @br
    @unorderedList(
       @item(https://fr.wikipedia.org/wiki/Windows_bitmap)
       @item(http://fileformats.archiveteam.org/wiki/BMP)
       @item(https://en.wikipedia.org/wiki/BMP_file_format  (plus complet que la version française))
       @item(https://delphi.developpez.com/cours/graphiques/menugra/bitmap2.php)
       @item(http://www.alrj.org/docs/formats/bmp/BMP.htm)
       @item(http://netghost.narod.ru/gff/graphics/summary/os2bmp.htm)
       @item(http://www.drdobbs.com/the-bmp-file-format-part-2/184409533?pgno=5)
       @item(http://www.fileformat.info/format/bmp/egff.htm)
       @item(http://www.fileformat.info/format/os2bmp/egff.htm)
    )

    Encodage/Decodage : @br
    @unorderedList(
      @item(https://msdn.microsoft.com/en-us/library/windows/desktop/dd183376%28v=vs.85%29.aspx)
      @item(https://en.wikipedia.org/wiki/File:BitfieldsSLN.png)
      @item(http://netghost.narod.ru/gff/graphics/book/ch09_01.htm)
      @item(https://fr.wikipedia.org/wiki/Codage_de_Huffman)
      @item(http://tcharles.developpez.com/Huffman/)
    )

    Autres informations utiles : @br
    @unorderedList(
       @item(https://msdn.microsoft.com/en-us/library/windows/desktop/dd183393(v=vs.85).aspx)
       @item(https://msdn.microsoft.com/en-us/library/windows/desktop/dd183353(v=vs.85).aspx)
       @item(https://msdn.microsoft.com/en-us/library/windows/desktop/dd183376(v=vs.85).aspx)
    )

    Fichiers test : @br
    @unorderedList(
      @item(http://entropymine.com/jason/bmpsuite/)
      @item(https://samples.libav.org/image-samples/)
   )

  -------------------------------------------------------------------------------------------------------------

  @bold(BUGS :)@br
    - Problème de chargement avec certains fichiers BMP OS2v1 1/4/8 bits (problèmes chargement palette)

  -------------------------------------------------------------------------------------------------------------

  @bold(TODO) :@br
  @unorderedList(
     @item(Fichier Icones et cusrseurs)
     @item(Decompression Huffman 1D)
     @item(Prise en charge pixelformat = 2 et 64 bits)
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)
   @unorderedList(
     @item(FPC/Lazarus)
   )

  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
Unit BZImageFileBMP;

//------------------------------------------------------------------------------
//----------------------------[ TODO LIST ]-------------------------------------

    { TODO 0  -oBZBitmap -cSupport_Images_BMP : Prise en charge pixelformat = 64 bits    }
    { TODO 0  -oBZBitmap -cSupport_Images_BMP : Ecriture Format BMP }
    { TODO 5  -oBZBitmap -cSupport_Images_BMP : Decompression Huffman 1D  }
    { TODO 10 -oBZBitmap -cSupport_Images_BMP : Decompression RLE 24 pour les version OS/2x  }
    { TODO 10 -oBZBitmap -cSupport_Images_BMP : Decompression JPG et PNG  }

//------------------------------------------------------------------------------

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\bzscene_options.inc}
//==============================================================================

Interface

Uses
  Classes, SysUtils,
  BZClasses, BZColors, BZGraphic,  BZBitmap, BZImageFileIO;

Const
  { ID BMP }
  BMP_MAGIC_WINBMP = 'BM'; // 19778;
  BMP_MAGIC_OS2BMP = 'BA';
  BMP_MAGIC_OS21ICO = 'CI';
  BMP_MAGIC_OS21CUR = 'CP';
  BMP_MAGIC_OS22ICO = 'IC';
  BMP_MAGIC_OS22CUR = 'PT';

  { Types de compression pris en charge }
  BMP_COMPRESSION_NONE = Longword(0);  //< Non compressé
  BMP_COMPRESSION_RLE8 = Longword(1);  //< Compressé avec RLE 8bits
  BMP_COMPRESSION_RLE4 = Longword(2);  //< Compressé avec RLE 4bits
  BMP_COMPRESSION_BITF = Longword(3);  //< Nom compressé BitFields winV2 Mask RGB / V3 Mask RGBA,   et seulement 16 et 32 bits
  BMP_COMPRESSION_HUF1D = Longword(3); //< OSX22 = Hufman1D seulement 1bit
  BMP_COMPRESSION_JPEG = Longword(4);  //< Compressé avec JPEG si OSX22 = RLE 24bits
  BMP_COMPRESSION_PNG = Longword(5);   //< Compressé avec PNG
  BMP_COMPRESSION_ALPHABITF = Longword(6); //< Compressé avec AlphaBitFields Mask RGBA (seulement WinCE5.0 et avec .NET > 4.0)

  //11,12,13 seulement Windows Metafile CMYK
  //       11 : "BI_CMYK - pas de compression
  //       12 : "BI_CMYKRLE8 - RLE-8
  //       13 : "BI_CMYKRLE4 - RLE-4
  { @groupend }
Type
  { L'entête principale d'un fichier BMP est composée de 4 champs :@br
    - La signature (sur 2 octets), indiquant qu'il s'agit d'un fichier BMP à l'aide de 2 caractères :
    @unorderedList(
      @item( BM $424D en hexadécimal, indique qu'il s'agit d'un Bitmap Windows.)
      @item( BA $4241 indique qu'il s'agit d'un Bitmap OS/2.)
      @item( CI $4349 indique qu'il s'agit d'une icone couleur OS/2.)
      @item( CP $4350 indique qu'il s'agit d'un pointeur de couleur OS/2.)
      @item( IC $4943 indique qu'il s'agit d'une icone OS/2.)
      @item( PT $5054 indique qu'il s'agit d'un pointeur OS/2.)
    - La taille totale du fichier en octets (codée sur 4 octets)@br
    - 2 champs réservés (de 2 octets)@br
    - L'offset de l'image (sur 4 octets), en français décalage }
  TBZBMPFileHeader = Packed Record   // < Taille de l'en-tête commune 14 octets
    bfType: Array[0..1] Of Char; // < bfType, Normalement Word;@brEn déclarant un array of char on evite de se prendre la tête avec  le "Little et Big Endian"
    bfSize: Longword;   // Taille totale du fichier
    bfReserved1: Word;  // Hospot X -> OS2x
    bfReserved2: Word;  // Hospot Y -> OS2x
    bfOffBits: Longword; // Position des données de l'image dans le fichier
  End;

  { Type de l'en-tête }
  TBZBMPHeaderType = (bmpht_unknown,
    bmpht_Os21x, bmpht_Os22x,
    bmpht_WindowsV1, bmpht_WindowsV2, bmpht_WindowsV3,
    bmpht_WindowsV4, bmpht_WindowsV5);

  { En-têtes BMP et OS/2 minimal@br
    biSize: LongWord; Taille de l'en-tête  = 12 octets @br
    ( biSize: LongWord pour la taille de l'en-tête ) }
  TBZOs21xBMPInfoHeader = Packed Record  //< Ici Taille 8, biSize est lu avant
    biWidth: Word;                  //< Largeur de l'image en pixels
    biHeight: Word;                 //< Hauteur de l'image en pixels
    biPlanes: Word;                 //< Nombre de Plans de couleur
    biBitCount: Word;               //< Nombre de bits par pixel
  End;

  { En-têtes BMP OS/2 V2@br
    biSize: LongWord; Taille de l'en-tête  = 64 octets }
  TBZOs22xBMPInfoHeader = Packed Record  //< Ici Taille 60, biSize est lu avant
    biWidth: Longword;               //< Largeur de l'image en pixels
    biHeight: Longword;              //< Hauteur de l'image en pixels
    biPlanes: Word;                  //< Nombre de Plans de couleur  = 1
    biBitCount: Word;                //< Nombre de bits par pixel
    biCompression: Word;             //< Type de compression
    biSizeImage: Longword;           //< Taille de l'image  AVEC LE PADDING
    biXPixelsPerMeter: Longint;      //< Nombre de pixel horizontal par mètre
    biYPixelsPerMeter: Longint;      //< Nombre de pixel vertical par mètre
    biClrUsed: Longword;             //< Nombre de couleurs utilisées (0 = toutes)
    biClrImportant: Longword;        //< Nombre de couleurs importantes (0 = toutes)
    biUnits: Word;                   //< Type d'unité utilisée pour mesurer les "distances"
    biReserved: Word;                //< Réservé (Pad structure to 4-byte boundary)
    biRecording: Word;               //< Algorithme d'enregistrement utilisé
    biRendering: Word;               //< Algorithme d' "Halftoning" utilisé
    biSize1: Longword;               //< Réservé pour l'algorithme d' "Halftoning"
    biSize2: Longword;               //< Reserved for halftoning algorithm use
    biColorEncoding: Longword;       //< Model de couleur utilisé
    biIdentifier: Longword;          //< Réservé pour l'appication qui enregistre le fichier
  End;

  { En-têtes BMP OS/2 V2@br
    biSize: LongWord; Taille de l'en-tête  =  40 octets  }
  TBZWindowsBMPInfoHeaderV1 = Packed Record  //< Ici Taille 36, biSize est lu avant
    biWidth: Longint;               //< Largeur de l'image en pixels
    biHeight: Longint;              //< Hauteur de l'image en pixels
    biPlanes: Word;                 //< Nombre de Plans de couleur  = 1
    biBitCount: Word;               //< Nombre de bits par pixel
    biCompression: Longword;        //< Type de compression
    biSizeImage: Longword;          //< Taille de l'image  AVEC LE PADDING
    biXPixelsPerMeter: Longint;     //< Nombre de pixel horizontal par mètre
    biYPixelsPerMeter: Longint;     //< Nombre de pixel vertical par mètre
    biClrUsed: Longword;            //< Nombre de couleurs utilisées (0 = toutes)
    biClrImportant: Longword;       //< Nombre de couleurs importantes (0 = toutes)
  End;

  { Definitions pour la prise en charge type de couleur spéciale CIExyz@br
   dans les en-têtes Windows V4 et V5 }
  TBZBMP_CIEXYZCoord = Packed Record
    X: Longint;
    Y: Longint;
    Z: Longint;
  End;

  TBZBMP_CIEXYZCoordTriple = Packed Record
    CRed: TBZBMP_CIExyzCoord;
    CGreen: TBZBMP_CIExyzCoord;
    CBlue: TBZBMP_CIExyzCoord;
  End;

  { En-tête BMP Windows V2 (non documenté - Adobe Photoshop)@br
    biSize: LongWord; Taille de l'en-tête  = 52 octets }
  TBZWindowsBMPInfoHeaderV2 = Packed Record   //< Ici Taille 48, biSize est lu avant
    biWidth: Longint;                //< Largeur de l'image en pixels
    biHeight: Longint;               //< Hauteur de l'image en pixels
    biPlanes: Word;                  //< Nombre de Plans de couleur  = 1
    biBitCount: Word;                //< Nombre de bits par pixel
    biCompression: Longword;         //< Type de compression
    biSizeImage: Longword;           //< Taille de l'image  AVEC LE PADDING
    biXPixelsPerMeter: Longint;      //< hoizontal pixels per meter
    biYPixelsPerMeter: Longint;      //< vertical pixels per meter
    biClrUsed: Longword;             //< Nombre de couleurs utilisées (0 = toutes)
    biClrImportant: Longword;        //< Nombre de couleurs importantes (0 = toutes)
    biRedMask: Longword;             //< Masque Couleur Rouge
    biGreenMask: Longword;           //< Masque Couleur Vert
    biBlueMask: Longword;            //< Masque Couleur Bleu
  End;

  { En-tête BMP Windows V3 (Adobe Photoshop)@br
    biSize: LongWord; Taille de l'en-tête  = 56 octets  }
  TBZWindowsBMPInfoHeaderV3 = Packed Record  // < Ici Taille 52, biSize est lu avant
    biWidth: Longint;                //< Largeur de l'image en pixels
    biHeight: Longint;               //< Hauteur de l'image en pixels
    biPlanes: Word;                  //< Nombre de Plans de couleur  = 1
    biBitCount: Word;                //< Nombre de bits par pixel
    biCompression: Longword;         //< Type de compression
    biSizeImage: Longword;           //< Taille de l'image  AVEC LE PADDING
    biXPixelsPerMeter: Longint;      //< hoizontal pixels per meter
    biYPixelsPerMeter: Longint;      //< vertical pixels per meter
    biClrUsed: Longword;             //< Nombre de couleurs utilisées (0 = toutes)
    biClrImportant: Longword;        //< Nombre de couleurs importantes (0 = toutes)
    biRedMask: Longword;             //< Masque Couleur Rouge
    biGreenMask: Longword;           //< Masque Couleur Vert
    biBlueMask: Longword;            //< Masque Couleur Bleu
    biAlphaMask: Longword;           //< Masque Couleur Alpha
  End;

  { En-tête BMP Windows V4@br
    biSize: LongWord; Taille de l'en-tête  = 108 octets }
  TBZWindowsBMPInfoHeaderV4 = Packed Record   //< Ici Taille 104, biSize est lu avant
    biWidth: Longint;                //< Largeur de l'image en pixels
    biHeight: Longint;               //< Hauteur de l'image en pixels
    biPlanes: Word;                  //< Nombre de Plans de couleur  = 1
    biBitCount: Word;                //< Nombre de bits par pixel
    biCompression: Longword;         //< Type de compression
    biSizeImage: Longword;           //< Taille de l'image  AVEC LE PADDING peut être 0 si pas de compression
    biXPixelsPerMeter: Longint;      //< hoizontal pixels per meter
    biYPixelsPerMeter: Longint;      //< vertical pixels per meter
    biClrUsed: Longword;             //< Nombre de couleurs utilisées (0 = toutes)
    biClrImportant: Longword;        //< Nombre de couleurs importantes (0 = toutes)
    biRedMask: Longword;             //< Masque Couleur Rouge
    biGreenMask: Longword;           //< Masque Couleur Vert
    biBlueMask: Longword;            //< Masque Couleur Bleu
    biAlphaMask: Longword;           //< Masque Couleur Alpha
    biCSType: Longword;              //< Type de l'espace de couleur CIE
    biEndpoints: TBZBMP_CIEXYZCoordTriple; //< "Color space endpoints"
    biGammaRed: Longword;            //< Correction Gamma Rouge
    biGammaGreen: Longword;          //< Correction Gamma Vert
    biGammaBlue: Longword;           //< Correction Gamma Bleu
  End;

  { En-tête BMP Windows V5@br
    biSize: LongWord; Taille de l'en-tête  = 124 octets }
  TBZWindowsBMPInfoHeaderV5 = Packed Record  //< Ici Taille 120, biSize est lu avant
    biWidth: Longint;                //< Largeur de l'image en pixels
    biHeight: Longint;               //< Hauteur de l'image en pixels
    biPlanes: Word;                  //< Nombre de Plans de couleur  = 1
    biBitCount: Word;                //< Nombre de bits par pixel
    biCompression: Longword;         //< Type de compression
    biSizeImage: Longword;           //< Taille de l'image  AVEC LE PADDING
    biXPixelsPerMeter: Longint;      //< hoizontal pixels per meter
    biYPixelsPerMeter: Longint;      //< vertical pixels per meter
    biClrUsed: Longword;             //< Nombre de couleurs utilisées (0 = toutes)
    biClrImportant: Longword;        //< Nombre de couleurs importantes (0 = toutes)
    biRedMask: Longword;             //< Masque Couleur Rouge
    biGreenMask: Longword;           //< Masque Couleur Vert
    biBlueMask: Longword;            //< Masque Couleur Bleu
    biAlphaMask: Longword;           //< Masque Couleur Alpha
    biCSType: Longword;              //< Type de l'espace de couleur CIE
    biEndpoints: TBZBMP_CIEXYZCoordTriple; //< "Color space endpoints"
    biGammaRed: Longword;            //< Correction Gamma Rouge
    biGammaGreen: Longword;          //< Correction Gamma Vert
    biGammaBlue: Longword;           //< Correction Gamma Bleu
    biIntent: Longword;              //< "rendering intent"
    biProfileData: Longword;         //< Position des données du profile de couleur
    biProfileSize: Longword;         //< Taille des données du profile de couleur
    biReserved: Longword;            //< Réservé
  End;

  { En-tête BMP }
  TBZBitmapBMPInfoHeader = Packed Record
    Case Size: Longword Of
      0: (Offset: Array[0..0] Of Byte);
      8: (Os21x: TBZOs21xBMPInfoHeader);
      36: (WindowsV1: TBZWindowsBMPInfoHeaderV1);
      48: (WindowsV2: TBZWindowsBMPInfoHeaderV2);
      52: (WindowsV3: TBZWindowsBMPInfoHeaderV3);
      60: (Os22x: TBZOs22xBMPInfoHeader);
      104: (WindowsV4: TBZWindowsBMPInfoHeaderV4);
      120: (WindowsV5: TBZWindowsBMPInfoHeaderV5);
  End;
  PBZBitmapBMPInfoHeader = ^TBZBitmapBMPInfoHeader;

Type
  EBZSaveImageException = Class(EBZBitmapException);

  { Classe de lecture et d"ecriture des images au format BMP }
  TBZBitmapBMPImage = Class(TBZCustomImageFileIO)
  private
    FBmpFileHeader: TBZBMPFileHeader;
    FInfoHeader: TBZBitmapBMPInfoHeader;
    FHeaderType: TBZBMPHeaderType;
    FInternalReadSize: Longword;
    FHeaderSize: Longword;
    FRowSize, FLineEndGapSize: Integer;
    FIgnoreAlpha: Boolean;

    FNeedAdjustBFH : Boolean;
    // Fonctions d'aide à la lecture des infos dans les en-têtes
    Function GetHeaderWidth: Integer;
    Function GetHeaderHeight: Integer;
    Function GetHeaderBitCount: Word;
    Function GetHeaderCompression: Longword;
    Function GetHeaderUsedColors: Longword;
    Function GetHeaderRedMask: Longword;
    Function GetHeaderGreenMask: Longword;
    Function GetHeaderBlueMask: Longword;
    Function GetHeaderAlphaMask: Longword;
    Function GetRowSize: Integer;
    Function GetPadding: Integer;

    function getInfoHeaderSize(aVersion : TBZBMPHeaderType): Integer;
    function ComputePaletteOrMaskSize:Integer;
    function ComputeCompressionType : LongWord;
    function CreateInfoHeader(aVersion : TBZBMPHeaderType):TBZBitmapBMPInfoHeader;
    procedure InitSaving(aVersion : TBZBMPHeaderType);
    procedure InitBitFieldMasks;
  protected
    // Variables utiles lors de l'encodage et du décodage des données
    RedMask, GreenMask, BlueMask, AlphaMask: Longword;
    RedBitShift, GreenBitShift, BlueBitShift, AlphaBitShift: Shortint;
    AbitCount, Padding: Byte;
    RowSize: Integer;
    Compression: Longword;
    bmpWidth, bmpHeight: Integer;
    TopDown: Boolean;
    GapSize1, GapSize2: Integer;

    procedure SaveHeader;
    procedure SavePalette;
    procedure SaveImageData;

    Procedure LoadFromMemory(); override;
    Function CheckFormat(): Boolean; override;
    Function ReadImageProperties: Boolean; override;
    Procedure SaveToMemory();Override;
  public

    Constructor Create(AOwner: TPersistent; AWidth, AHeight: Integer); override;
    Destructor Destroy; override;

    Class Function Capabilities: TBZDataFileCapabilities; override;

    Function getImagePropertiesAsString: String; override;
  End;

Implementation

Uses
  BZImageStrConsts,
  BZUtils
 {$IFDEF DEBUG}
 , Dialogs, BZLogger
 {$ENDIF};

{%region%=====[ TBZBitmapBMPImage ]============================================}

Constructor TBZBitmapBMPImage.Create(AOwner: TPersistent; AWidth, AHeight: Integer);
Begin
  Inherited Create(AOwner, AWidth, AHeight);
  With DataFormatDesc Do
  Begin
    Name := 'BMP';
    Desc := 'Windows / OS2 Bitmap';  // , icone, curseur';
    FileMask := '*.bmp;*.dib;*.rle'; //*.ico;*.cur;
    Version := '';
    Encoding := etNone;
  End;

  SupportedColorFormat := SupportedColorFormat + [cfXRGB_1555, cfRGB_565, cfBGR_565, cfBGR, cfRGB, cfARGB, cfABGR, cfRGBA, cfBGRA];
  FInternalReadSize := 0;
End;

Destructor TBZBitmapBMPImage.Destroy;
Begin
  SupportedColorFormat := [];
  Inherited Destroy;
End;

Class Function TBZBitmapBMPImage.Capabilities: TBZDataFileCapabilities;
Begin
  Result := [dfcRead, dfcWrite]
End;

Function TBZBitmapBMPImage.getImagePropertiesAsString: String;
Var
  S: String;
Begin
  S := '';
  With DataFormatDesc Do
  Begin
    S := 'Format de fichier : ' + Name + #13 + #10; //+'('+Desc+')'+#13+#10;
  End;
  S := S + 'Version : ';
  Case FHeaderType Of
    bmpht_Os21x: S := S + 'OS2x Version 1';
    bmpht_WindowsV1: S := S + 'Windows Version 1';
    bmpht_WindowsV2: S := S + 'Windows Version 2';
    bmpht_WindowsV3: S := S + 'Windows Version 3';
    bmpht_Os22x: S := S + 'OS2x Version 2';
    bmpht_WindowsV4: S := S + 'Windows Version 4';
    bmpht_WindowsV5: S := S + 'Windows Version 5';
    Else
      S := S + 'Version Inconnue';
  End;
  S := S + #13 + #10 + 'Encodage : ';
  Case DataFormatDesc.Encoding Of
    etNone: S := S + 'Aucun';
    etRLE: S := S + 'RLE';
    etBitFields: S := S + 'BitFields';
    else S:='Non supporté';
  end;
{    BMP_COMPRESSION_JPEG: If FHeaderType = bmpht_Os22x Then
        S := S + 'RLE 24Bits'
      Else
        S := S + 'JPEG';
    BMP_COMPRESSION_PNG: S := S + 'PNG'; }

  Result := S;
End;

Function TBZBitmapBMPImage.GetHeaderWidth: Integer;
Begin
  Case FHeaderType Of
    bmpht_Os21x: Result := FInfoHeader.Os21x.biWidth;
    bmpht_WindowsV1: Result := FInfoHeader.WindowsV1.biWidth;
    bmpht_WindowsV2: Result := FInfoHeader.WindowsV2.biWidth;
    bmpht_WindowsV3: Result := FInfoHeader.WindowsV3.biWidth;
    bmpht_Os22x: Result := FInfoHeader.Os22x.biWidth;
    bmpht_WindowsV4: Result := FInfoHeader.WindowsV4.biWidth;
    bmpht_WindowsV5: Result := FInfoHeader.WindowsV5.biWidth;
    Else
      Result := 0;
  End;
  result:=abs(result); // La largeur ne peut pas être inversée comme la hauteur
End;

Function TBZBitmapBMPImage.GetHeaderHeight: Integer;
Begin
  Case FHeaderType Of
    bmpht_Os21x: Result := FInfoHeader.Os21x.biHeight;
    bmpht_WindowsV1: Result := FInfoHeader.WindowsV1.biHeight;
    bmpht_WindowsV2: Result := FInfoHeader.WindowsV2.biHeight;
    bmpht_WindowsV3: Result := FInfoHeader.WindowsV3.biHeight;
    bmpht_Os22x: Result := FInfoHeader.Os22x.biHeight;
    bmpht_WindowsV4: Result := FInfoHeader.WindowsV4.biHeight;
    bmpht_WindowsV5: Result := FInfoHeader.WindowsV5.biHeight;
    Else
      Result := 0;
  End;
End;

Function TBZBitmapBMPImage.GetHeaderBitCount: Word;
Begin
  Case FHeaderType Of
    bmpht_Os21x: Result := FInfoHeader.Os21x.biBitCount;
    bmpht_WindowsV1: Result := FInfoHeader.WindowsV1.biBitCount;
    bmpht_WindowsV2: Result := FInfoHeader.WindowsV2.biBitCount;
    bmpht_WindowsV3: Result := FInfoHeader.WindowsV3.biBitCount;
    bmpht_Os22x: Result := FInfoHeader.Os22x.biBitCount;
    bmpht_WindowsV4: Result := FInfoHeader.WindowsV4.biBitCount;
    bmpht_WindowsV5: Result := FInfoHeader.WindowsV5.biBitCount;
    Else
      Result := 0;
  End;
End;

Function TBZBitmapBMPImage.GetHeaderCompression: Longword;
Begin
  Case FHeaderType Of
    bmpht_WindowsV1: Result := FInfoHeader.WindowsV1.biCompression;
    bmpht_WindowsV2: Result := FInfoHeader.WindowsV2.biCompression;
    bmpht_WindowsV3: Result := FInfoHeader.WindowsV3.biCompression;
    bmpht_Os22x: Result := FInfoHeader.Os22x.biCompression;
    bmpht_WindowsV4: Result := FInfoHeader.WindowsV4.biCompression;
    bmpht_WindowsV5: Result := FInfoHeader.WindowsV5.biCompression;
    Else
      Result := 0;
  End;
End;

Function TBZBitmapBMPImage.GetHeaderUsedColors: Longword;
Begin
  Result:=0;
  Case FHeaderType Of
    {Pour detecter la présence d'une palette dans un fichier BMP OS2x.
     Au lieu de juste supposer la présence d'une palette, ou de se fier au
     parametre "biclrUsed" dans l'en-tête OS2x.
     Il faut mieux calculer le nombre de d'octets entre l'en-tête et les données
     puis diviser ce nombre par la taille d'un élément de la palette }
    bmpht_OS21x : Result := Round((FBMPFileHeader.bfOffBits - FInternalReadSize) / 3);
    bmpht_WindowsV1: Result := FInfoHeader.WindowsV1.biclrUsed;
    bmpht_WindowsV2: Result := FInfoHeader.WindowsV2.biclrUsed;
    bmpht_WindowsV3: Result := FInfoHeader.WindowsV3.biclrUsed;
    bmpht_Os22x: Result := Round((FBMPFileHeader.bfOffBits - FInternalReadSize) / 4);      //  Result := FInfoHeader.Os22x.biclrUsed;
    bmpht_WindowsV4: Result := FInfoHeader.WindowsV4.biclrUsed;
    bmpht_WindowsV5: Result := FInfoHeader.WindowsV5.biclrUsed;
    Else
      Result:=0;
  End;
  Result := abs(result);
End;

Function TBZBitmapBMPImage.GetHeaderRedMask: Longword;
Begin
  Result := 0;
  If FHeaderType >= bmpht_WindowsV2 Then
  Begin
    Case FHeaderType Of
      bmpht_WindowsV2: Result := (FInfoHeader.WindowsV2.biRedMask);
      bmpht_WindowsV3: Result := (FInfoHeader.WindowsV3.biRedMask);
      bmpht_WindowsV4: Result := (FInfoHeader.WindowsV4.biRedMask);
      bmpht_WindowsV5: Result := (FInfoHeader.WindowsV5.biRedMask);
    End;
  End;
End;

Function TBZBitmapBMPImage.GetHeaderGreenMask: Longword;
Begin
  Result := 0;
  If FHeaderType >= bmpht_WindowsV2 Then
  Begin
    Case FHeaderType Of
      bmpht_WindowsV2: Result := (FInfoHeader.WindowsV2.biGreenMask);
      bmpht_WindowsV3: Result := (FInfoHeader.WindowsV3.biGreenMask);
      bmpht_WindowsV4: Result := (FInfoHeader.WindowsV4.biGreenMask);
      bmpht_WindowsV5: Result := (FInfoHeader.WindowsV5.biGreenMask);
    End;
  End;
End;

Function TBZBitmapBMPImage.GetHeaderBlueMask: Longword;
Begin
  Result := 0;
  If FHeaderType >= bmpht_WindowsV2 Then
  Begin
    Case FHeaderType Of
      bmpht_WindowsV2: Result := (FInfoHeader.WindowsV2.biBlueMask);
      bmpht_WindowsV3: Result := (FInfoHeader.WindowsV3.biBlueMask);
      bmpht_WindowsV4: Result := (FInfoHeader.WindowsV4.biBlueMask);
      bmpht_WindowsV5: Result := (FInfoHeader.WindowsV5.biBlueMask);
    End;
  End;
End;

Function TBZBitmapBMPImage.GetHeaderAlphaMask: Longword;
Begin
  Result := 0;
  If FHeaderType >= bmpht_WindowsV3 Then
  Begin
    Case FHeaderType Of
      bmpht_WindowsV3: Result := (FInfoHeader.WindowsV3.biAlphaMask);
      bmpht_WindowsV4: Result := (FInfoHeader.WindowsV4.biAlphaMask);
      bmpht_WindowsV5: Result := (FInfoHeader.WindowsV5.biAlphaMask);
    End;
  End;
End;

Function TBZBitmapBMPImage.GetRowSize: Integer;
Begin
  // Alignement d'une LIGNE sur 4 octets
  Result := Round((GetHeaderBitCount * GetHeaderWidth + 31) shr 5) shl 2;
  //Result := ((getHeaderWidth * getHeaderBitCount + 31) and - 32) shr 3;
End;

Function TBZBitmapBMPImage.GetPadding: Integer;
Begin
  Result := 0;
  Case GetHeaderBitCount Of
    4: Result := GetRowSize - (getHeaderWidth shr 1);
    8: Result := GetRowSize - getHeaderWidth;
    16: Result := GetRowSize - (getHeaderWidth * 2);
    24:
    begin
      Result := GetRowSize - (getHeaderWidth * 3);
      //GlobalLogger.LogStatus('RowSize  :'+inttostr(GetRowSize));
      //GlobalLogger.LogStatus('Line Len :'+inttostr((getHeaderWidth * 3)));
      //GlobalLogger.LogStatus('Padding  :'+inttostr(GetRowSize- (getHeaderWidth * 3)));
    End;
  End;
  If Result > 3 Then
    Result := 0;
End;

Function TBZBitmapBMPImage.CheckFormat(): Boolean;
Var
  s : String;
  TmpBmpFileHeader :TBZBMPFileHeader;
Begin
  s := '';
  Result := False;
  Memory.Read(TmpBmpFileHeader{%H-}, SizeOf(TBZBMPFileHeader));
  //GlobalLogger.LogStatus('------------------- LOADING : '+FullFileName);

  // Fichier OS2.2 BGA (multi-image), on saute les 14 octets supplémentaires
  // Ici, on lit que la première ( cf : http://fileformats.archiveteam.org/wiki/OS/2_Bitmap_Array )
  if (TmpBmpFileHeader.bfType = BMP_MAGIC_OS2BMP) then
  begin
    Memory.Seek(0,soBeginning);
    Memory.SeekForward(14);
    Memory.Read(TmpBmpFileHeader, SizeOf(TBZBMPFileHeader));
  End;

  With TmpBmpFileHeader Do
  Begin
    If (bfType = BMP_MAGIC_WINBMP)  or (bfType = BMP_MAGIC_OS21ICO) or
      (bfType = BMP_MAGIC_OS21CUR) or (bfType = BMP_MAGIC_OS22ICO) or (bfType = BMP_MAGIC_OS22CUR) Then
    Begin
      FHeaderSize := Memory.ReadLongWord;
      {$IFDEF FPC_BIG_ENDIAN}
        FHeaderSize := LEtoN(FHeaderSize);
        With  FBmpFileHeader do
        begin
          bfSize := LEToN(TmpBmpFileHeader.bfSize);
          bfReserved1 := LEToN(TmpBmpFileHeader.bfReserved1);
          bfReserved2 := LEToN(TmpBmpFileHeader.bfReserved2);
          bfOffBits := LEToN(TmpBmpFileHeader.bfOffBits);
        end;
      {$ELSE}
        FBmpFileHeader := TmpBmpFileHeader;
      {$ENDIF}
      Result := ReadImageProperties; // On Lit les propriétes de l'image
    End
    Else
    begin
      Result := False;
      s:= bfType[0] + bfType[1];
      RaiseInvalidImageHeader(Format(rsBadSignature,[s]));
    End;
  End;
End;

Function TBZBitmapBMPImage.ReadImageProperties: Boolean;
Var
  UsedColors : Integer;
  checkpalcount : Integer;
  checksize : Int64;
Begin
  Result := True;
  // On lit le bon en-tête suivant sa taille.
  { NOTE : La structure With..Do est obligatoire ici pour assigner les paramètres.
    Sinon si sous la forme DataFormatDesc.Desc:='blablabla'; --> Error : Argument Cannot be assigned to }
  With DataFormatDesc do begin Desc:='Microsoft / OSx Bitmap' end;
  Case FHeaderSize Of
    12:
    Begin
      FHeaderType := bmpht_Os21x;
      Memory.Read(FInfoHeader.Os21x, 12);
      With DataFormatDesc do begin Version := 'OSx Bitmap 2.1'; end;
    End;
    40:
    Begin
      FHeaderType := bmpht_WindowsV1;
      Memory.Read(FInfoHeader.WindowsV1, 36);
      With DataFormatDesc do begin Version := 'Windows Bitmap 1.0'; end;
    End;
    52:
    Begin
      FHeaderType := bmpht_WindowsV2;
      Memory.Read(FInfoHeader.WindowsV2, 48);
      With DataFormatDesc do begin Version := 'Windows Bitmap 2.0'; end;
    End;
    56:
    Begin
      FHeaderType := bmpht_WindowsV3;
      Memory.Read(FInfoHeader.WindowsV3, 52);
      With DataFormatDesc do begin Version := 'Windows Bitmap 3.0'; end;
    End;
    64:
    Begin
      FHeaderType := bmpht_Os22x;
      Memory.Read(FInfoHeader.Os22x, 60);
      With DataFormatDesc do begin Version := 'OSx Bitmap 2.2'; end;
    End;
    108:
    Begin
      FHeaderType := bmpht_WindowsV4;
      Memory.Read(FInfoHeader.WindowsV4, 104);
      With DataFormatDesc do begin Version := 'Windows Bitmap 4.0'; end;
    End;
    124:
    Begin
      FHeaderType := bmpht_WindowsV5;
      Memory.Read(FInfoHeader.WindowsV5, 120);
      With DataFormatDesc do begin Version := 'Windows Bitmap 5.0'; end;
    End;
    Else
    begin
      Result:=false;
      RaiseInvalidImageHeader(Format(rsInvalidHeaderSize,[FHeaderSize]));
    end;
  End;
  // On verifie l'ID en cas de fichier OS22x Bitmap Array (BA)
  if FBmpFileHeader.bfType = BMP_MAGIC_OS2BMP then  With DataFormatDesc do begin Version := 'OSx Bitmap 2.2 BA'; end;

  //GlobalLogger.LogNotice('Version detected : '+DataFormatDesc.Version);
  // On calcul combien d'octets ont deja été lu dans le fichier (servira pour la calcul des "GapSize")

  //FInternalReadSize := 14 + FHeaderSize + 4;
  FInternalReadSize := Memory.Position;
  //GlobalLogger.LogNotice('InternalReadSize : '+inttostr(Memory.position));

  // On récupère les infos basiques
  bmpWidth := GetHeaderWidth;
  bmpHeight := GetHeaderHeight;
  ABitCount := GetHeaderBitCount;
  TopDown := (bmpHeight < 0);
  bmpHeight := abs(bmpHeight);

  (*GlobalLogger.LogStatus('Width  : '+Inttostr(bmpWidth));
  GlobalLogger.LogStatus('Height : '+Inttostr(bmpHeight));
  GlobalLogger.LogStatus('Bpp    : '+Inttostr(ABitCount));
  GlobalLogger.LogStatus('Bmp Size   : '+Inttostr(FBmpFileHeader.bfSize));
  GlobalLogger.LogStatus('Offset     : '+Inttostr(FBmpFileHeader.bfOffBits));
  GlobalLogger.LogStatus('File Size  : '+Inttostr(Memory.Size));*)

  // Vérification des dimensions
  checksize := (bmpWidth * BmpHeight) * 4;
  {$ifdef cpu64}
    If ((bmpWidth = 0) or (bmpHeight = 0)) or (checkSize>4096000000) Then // or ((bmpWidth > 32000) or (bmpHeight > 32000))
  {$else}
    If ((bmpWidth = 0) or (bmpHeight = 0)) or (checkSize>2048000000) Then //or ((bmpWidth > 32000) or (bmpHeight > 32000))
  {$endif}
  Begin
    Result := False;
    RaiseInvalidImageFile(Format(rsBadImageSize,[BmpWidth,BmpHeight]));
  End;

  // Taille du fichier incorrect
 (* if (FBmpFileHeader.bfSize>Memory.Size) or (((bmpWidth*bmpHeight)*(ABitCount div 8))>(Memory.Size-FBmpFileHeader.bfOffBits)) then
  Begin
     Result := False;
     RaiseInvalidImageFile('Taille du fichier invalide');
  End;  *)

  { On connait ou se trouve les données, on sait combien de données on a lu
    On peut donc déterminer le 1er "GapSize" c'est à dire l'espace perdu entre
    la palette de couleurs et les données.
    Le GapSize peut-être égal à 0.
    Cette espace perdu est présent surtout avec les en-tête Windows 1,4 et 5
    Si la compression est égale à BITFIELD si ce "GapSize" est égal à 12
    alors cet espace contient les masques RGBA donc BitCount = 24.
    Si égal à 16 alors cet espace contient les masques RGBA donc BitCount = 32.
    Si ce gapSize est egal à zero alors format par defaut BGRA. Et pour les version 4 et 5
    on doit utiliser les masques présents dans l'en-tête.
    La valeur de bfOffBits peut également être  "erronée", souvent de 4 octets (je n'arrive pas à comprendre pourquoi )
  }

  If ((FInternalReadSize) > FBMPFileHeader.bfOffBits) Then
  Begin
   // Result := False;
    //RaiseInvalidImageFile('Erreur de lecture dans le fichier'+#13+#10+'InternalRead :'+Inttostr(FInternalReadSize)+#13+#10+
    //'OffBits : '+Inttostr(FBMPFileHeader.bfOffBits));
    //On se replace simplement
    //Memory.Seek(FBMPFileHeader.bfOffBits, soBeginning);
  End
  Else
  Begin
    GapSize1 := (FBMPFileHeader.bfOffBits) - (FInternalReadSize);
  End;
  //GlobalLogger.LogStatus('gapSize1 = ' + IntToStr(GapSize1)+' Cols 24 = ' + IntToStr(GapSize1 div 3));
  //GlobalLogger.LogStatus('gapSize1 = ' + IntToStr(GapSize1)+' Cols 32 = ' + IntToStr(GapSize1 div 4));

  // On fait un 1er test sur la valeur "Compression"
  Compression := GetHeaderCompression;

  If not (Compression in [BMP_COMPRESSION_NONE..BMP_COMPRESSION_ALPHABITF]) Then
  Begin
    Result := False;
    RaiseInvalidImageFile(rsCompressionMethodInvalid);
  End;

  // Compression Huffman 1D non supporté
  If (ABitCount = 1) and (Compression = BMP_COMPRESSION_HUF1D) Then
  Begin
    Result := False;
    RaiseInvalidImageFile(rsHUFFMAN1DCompressionNotSupported);
  End;
  { La compression PNG est uniquement supporté par Windows et  par les fonction StrechBits et
    pour l'affichage. cf : https://msdn.microsoft.com/en-us/library/dd145023(VS.85).aspx
  }
  If (Compression = BMP_COMPRESSION_PNG) Then
  Begin
    Result := False;
    RaiseInvalidImageFile(rsPNGCompressionNotSupported);
  End;

  { Idem que ci-dessus mais au format JPEG. Sauf si le fichier est de Type OS2 version en-tete 2.
    Dans ce cas la compression peut-être activer uniquement pour le format 24bits.
    L'algorithme utilisé sera alors le RLE 24bits
  }
  If not (FHeaderType = bmpht_Os22x) and (Compression = BMP_COMPRESSION_JPEG) Then
  Begin
    Result := False;
    RaiseInvalidImageFile(rsJPGCompressionNotSupported);
  End;

  Case Compression of
     BMP_COMPRESSION_NONE      : DataFormatDesc.Encoding := etNone;
     BMP_COMPRESSION_BITF,
     BMP_COMPRESSION_ALPHABITF : DataFormatDesc.Encoding := etBitFields;
     BMP_COMPRESSION_RLE4,
     BMP_COMPRESSION_RLE8      : DataFormatDesc.Encoding := etRLE;

     else DataFormatDesc.Encoding := etNotSupported;
  end;

  { On a récupéré nos informations, on met à jour le ImageDescription du TBZBitmap
     On charge la palette de couleur si besoins
     On initialise quelques variables utiles pour la lecture des données suivant
     le "PixelFormat"
   }
  With ImageDescription Do
  Begin
    //GlobalLogger.LogStatus('Read ABitCount = ' + IntToStr(ABitCount));

    InitDefault(bmpWidth, bmpHeight, ABitCount);
    //GlobalLogger.LogStatus('BPP == ' + IntToStr(BitsPerPixel));

    // Une palette existe ?
    UsedColors := GetHeaderUsedColors;
    //GlobalLogger.LogNotice('------> Used Color : '+inttostr(UsedColors));
    //GlobalLogger.LogStatus('GAPSIZE 1 = '+Inttostr(GapSize1));
    // Taile d'une ligne en octets avec Padding et suivant le format de pixel
    FRowSize := Compute_BytesPerLine(bmpWidth, AbitCount, bleDWordBoundary);

    Case ABitCount Of
      1:
      Begin
        PixelFormat := pf1Bit;
        ColorFormat := cfMono;
        UsedColors := GetHeaderUsedColors;
        //If (FHeaderType = bmpht_Os22x) or (FHeaderType = bmpht_Os21x) then

        // La description du nombre de couleur est fausse. Une palette est présente
        if ((GapSize1 > 0) and ((GapSize1 div 4)<=2) and (UsedColors = 0)) then UsedColors := 2;

        If (UsedColors > 0) Then
        begin
          PaletteCount := UsedColors;
          UsePalette := True;
        End;
      End;
      2:
      Begin
        PixelFormat := pf2Bits;
        UsePalette := True;
        PaletteCount := 4;
        //If (FHeaderType = bmpht_Os22x) or (FHeaderType = bmpht_Os21x) then

        // La description du nombre de couleur est fausse. Une palette est présente
        if ((GapSize1 > 0) and ((GapSize1 div 4)<=4) and (UsedColors = 0)) then UsedColors := (GapSize1 div 4);
        If (UsedColors >= 2) and (UsedColors <= PaletteCount) Then
        Begin
          PaletteCount := UsedColors;
        End
        Else If (UsedColors > PaletteCount) Then
        Begin
          //Raise exception.Create('Erreur : Trop de couleur dans la palette > 4');
          AddError(Format(rsTooManyColorInPalette,[UsedColors, PaletteCount]));
          Result := False;
        End;
      End;
      4:
      Begin
       // PixelFormat := pf4Bits;
        ColorFormat := cfBGRA;
        UsePalette := True;
        PaletteCount := 16;
        //If (FHeaderType = bmpht_Os22x) or (FHeaderType = bmpht_Os21x) then

        // La description du nombre de couleur est fausse. Une palette est présente
        if ((GapSize1 > 0) and ((GapSize1 div 4)<=16) and (UsedColors = 0)) then UsedColors := (GapSize1 div 4);
        if UsedColors>16 then PaletteCount:=16 else PaletteCount := UsedColors;

       (* CheckPalCount := GapSize1 div 3;
        If (UsedColors >= 2) and (UsedColors <= CheckPalCount) Then
        Begin
          PaletteCount := UsedColors;
        End else PaletteCount := CheckPalCount;

        If (UsedColors > CheckPalCount) Then
        Begin
          ShowMessage('Attention fichier invalide : '+#13+#10+
                      'Erreur dans la description du nombre de couleur dans la palette : '+InttoStr(UsedColors)+#13+#10+
                      'Au lieu de : '+InttoStr(CheckPalCount));
          // On tente de charger avec le bon nombre.
          PaletteCount := CheckPalCount;
        End;  *)

        If not (Compression = BMP_COMPRESSION_NONE) and not (Compression = BMP_COMPRESSION_RLE4) Then
        Begin
          Result := False;
          RaiseInvalidImageFile(rsCompressionMethodInvalid);
        End;
      End;
      8:
      Begin
       // PixelFormat := pf8Bits;
        ColorFormat := cfBGRA;
        UsePalette := True;

        { /!\ Normalement un fichier WINDOWS BMP VALIDE 8bits ne peut pas avoir une palette de plus de 256 couleurs.
        Cependant certain logiciels peuvent avoir avec des bugs dans leurs procedures d'enregistrement du format BMP.
        Et certain peuvent enregistré une palette des couleurs utilisées dans l'image (cf format 24 et 32 bits plus bas)
        De même certain fichier peuvent être invalide en indiquant un mauvais nombre de couleur dans la palette }



        If (FHeaderType = bmpht_Os22x) or (FHeaderType = bmpht_Os21x) then
        begin
          CheckPalCount := (GapSize1) div 3; // Div 3 --> Format OS2.x
          If (UsedColors = 0) then  PaletteCount := CheckPalCount
          else If (UsedColors > 0) then //and (UsedColors <= CheckPalCount) Then
          Begin
            PaletteCount := UsedColors;
          End;
          If (UsedColors > CheckPalCount) Then
          Begin
            // Erreur dans la description du nombre de couleur dans la palette
            AddError(Format(rsBMPBadHeaderPalcount,[UsedColors, CheckPalCount]));
            // On tente de charger avec le bon nombre.
            PaletteCount := CheckPalCount;
          End;
        End
        else
        Begin
          // La description du nombre de couleur est fausse. Une palette est présente
          if ((GapSize1 > 0) and ((GapSize1 div 4)<=256) and (UsedColors = 0)) then
          begin
            AddError(Format(rsBMPBadHeaderPalcount,[UsedColors,(GapSize1 div 4)]));
            UsedColors := (GapSize1 div 4);
          end;
          if UsedColors>256 then
          begin
            AddError(Format(rsBMPBadHeaderPalcount,[UsedColors,256]));
            PaletteCount:=256;
          End
          else PaletteCount := UsedColors;
        End;

        If not (Compression = BMP_COMPRESSION_NONE) and not (Compression = BMP_COMPRESSION_RLE8) Then
        Begin
          Result := False;
          RaiseInvalidImageFile(rsCompressionMethodInvalid);
        End;
      End;
      16:
      Begin
        If (UsedColors > 0) Then
        Begin
          //UsePalette := True;
          PaletteCount := UsedColors;
          // Le fichier contient une palette. On doit calculer la taille du GapSize1 en fonction du nombre de couleurs dans la palette
          // Au cas ou un champs de description pour les bitfields est présent
          if ((GapSize1 div 4) - UsedColors)>0 then GapSize1 := GapSize1 - (UsedColors*4);
        End;

        //PixelFormat := pf16Bits;
        If not (Compression = BMP_COMPRESSION_NONE) and not (Compression = BMP_COMPRESSION_BITF) Then
        Begin
          Result := False;
          RaiseInvalidImageFile(rsCompressionMethodInvalid);
        End;
        If (Compression = BMP_COMPRESSION_NONE) Then
        Begin
          With BitFields Do
          Begin
            //format XRGB 1555
            AlphaMask := $00008000;
            RedMask := $00007C00;
            GreenMask := $000003E0;
            BlueMask := $0000001F;
           {$ifdef ENDIAN_BIG}
            AlphaMask := LEtoN(AlphaMask);
            RedMask := LEtoN(RedMask);
            GreenMask := LEtoN(GreenMask);
            BlueMask := LEtoN(BlueMask);
           {$endif}
          End;
        End;
        If (Compression = BMP_COMPRESSION_BITF) Then
        Begin
          With BitFields Do
          Begin
            If (FHeaderType = bmpht_WindowsV1) or (FHeaderType = bmpht_WindowsV4) or (FHeaderType = bmpht_WindowsV5) Then
            Begin
              If GapSize1 = 12 Then
              Begin
                RedMask := (Memory.ReadLongWord);
                GreenMask := (Memory.ReadLongWord);
                BlueMask := (Memory.ReadLongWord);
              End
              Else If GapSize1 = 16 Then
              Begin
                RedMask := (Memory.ReadLongWord);
                GreenMask := (Memory.ReadLongWord);
                BlueMask := (Memory.ReadLongWord);
                AlphaMask := (Memory.ReadLongWord);
              End
              Else If GapSize1 = 0 Then
              Begin
                If (FHeaderType = bmpht_WindowsV4) or (FHeaderType = bmpht_WindowsV5) Then
                Begin
                  RedMask := GetHeaderRedMask;
                  GreenMask := GetHeaderGreenMask;
                  BlueMask := GetHeaderBlueMask;
                  AlphaMask := GetHeaderAlphaMask;
                End
                Else
                Begin
                  //AlphaMask := $0000;
                  RedMask := $00007C00;
                  GreenMask := $000003E0;
                  BlueMask := $0000001F;
                   {$ifdef ENDIAN_BIG}
                  AlphaMask := LEtoN(AlphaMask);
                  RedMask := LEtoN(RedMask);
                  GreenMask := LEtoN(GreenMask);
                  BlueMask := LEtoN(BlueMask);
                   {$endif}
                End;
              End;
            End
            Else
            Begin
              RedMask := GetHeaderRedMask;
              GreenMask := GetHeaderGreenMask;
              BlueMask := GetHeaderBlueMask;
              If (FHeaderType > bmpht_WindowsV2) Then
              Begin
                AlphaMask := GetHeaderAlphaMask;
              End;
            End;
          End;
        End;
        With BitFields Do
        Begin
          RedShift := GetMaskShift(RedMask);
          GreenShift := GetMaskShift(GreenMask);
          BlueShift := GetMaskShift(BlueMask);
          AlphaShift := GetMaskShift(AlphaMask);

          RedSize := GetMaskSize(RedMask, RedShift);
          GreenSize := GetMaskSize(GreenMask, GreenShift);
          BlueSize := GetMaskSize(BlueMask, BlueShift);
          AlphaSize := GetMaskSize(AlphaMask, AlphaShift);

          RedDeltaShift := GetMaskDeltaShift(RedSize);
          GreenDeltaShift := GetMaskDeltaShift(GreenSize);
          BlueDeltaShift := GetMaskDeltaShift(BlueSize);
          AlphaDeltaShift := GetMaskDeltaShift(AlphaSize);
          // Le Mask Alpha cache-t-il une autre composante de la couleur
          If (RedMask or GreenMask or BlueMask) and AlphaMask <> 0 Then
          Begin
            AlphaMask := 0;
            AlphaShift := 0;
            AlphaSize := 0;
          End;
        End;
      End;
      24:
      Begin
        PixelFormat := pf24Bits;
        ColorFormat := cfBGR;

        If (UsedColors > 0) Then
        Begin
        { On vérifie si une palette est présente. Normalement il n'y a pas de palette avec le format BMP 24 ou 32 bits.
          Cependant certain logiciels sauvegarde une palette pouvant servir de référence pour une réduction
          du nombre de couleur }

          //UsePalette := True;
          PaletteCount := UsedColors;
        End;

        If not (Compression = BMP_COMPRESSION_NONE) and not (Compression = BMP_COMPRESSION_BITF) Then
        Begin
          Result := False;
          // Avec la version de l'en-tête Os22x la compression JPEG correspond à une compression RLE (Uniquement pour les images 24Bits)
          If (FHeaderType = bmpht_Os22x) and (Compression = BMP_COMPRESSION_JPEG) Then Result := True;
          if Result=False then RaiseInvalidImageFile(rsCompressionMethodInvalid);
        End;
      End;
      32:
      Begin
        PixelFormat := pf32Bits;
        ColorFormat := cfBGRA;

        If (UsedColors > 0) Then
        Begin
          //UsePalette := True;
          PaletteCount := UsedColors;
          // Le fichier contient une palette. On doit calculer la taille du GapSize1 en fonction du nombre de couleurs dans la palette
          // Au cas ou un champs de description pour les bitfields est présent
          if ((GapSize1 div 4) - UsedColors)>0 then GapSize1 := GapSize1 - (UsedColors*4);
        End;
        If not (Compression = BMP_COMPRESSION_NONE) and not (Compression = BMP_COMPRESSION_BITF) Then
        Begin
          Result := False;
          RaiseInvalidImageFile(rsCompressionMethodInvalid);
        End;

        If (Compression = BMP_COMPRESSION_BITF) Then
        Begin
          With BitFields Do
          Begin
            If (FHeaderType = bmpht_WindowsV1) or (FHeaderType = bmpht_WindowsV4) or (FHeaderType = bmpht_WindowsV5) Then
            Begin
              If GapSize1 = 12 Then
              Begin
                Size := 12;
                RedMask := (Memory.ReadLongWord);
                GreenMask := (Memory.ReadLongWord);
                BlueMask := (Memory.ReadLongWord);
                AlphaMask := 0;
                AlphaShift := 0;
                AlphaSize := 0;
              End
              Else If GapSize1 = 16 Then
              Begin
                Size := 16;
                RedMask := (Memory.ReadLongWord);
                GreenMask := (Memory.ReadLongWord);
                BlueMask := (Memory.ReadLongWord);
                AlphaMask := (Memory.ReadLongWord);
              End
              Else If GapSize1 = 0 Then
              Begin
                If (FHeaderType = bmpht_WindowsV4) or (FHeaderType = bmpht_WindowsV5) Then
                Begin
                  Size := 16;
                  RedMask := GetHeaderRedMask;
                  GreenMask := GetHeaderGreenMask;
                  BlueMask := GetHeaderBlueMask;
                  AlphaMask := GetHeaderAlphaMask;
                End;

              End;
            End
            Else    // Version Windows 2 ou 3
            Begin
              Size := 12;
              RedMask := GetHeaderRedMask;
              GreenMask := GetHeaderGreenMask;
              BlueMask := GetHeaderBlueMask;
              AlphaSize := 0;
              AlphaMask := 0;
              If (FHeaderType > bmpht_WindowsV2) Then
              Begin
                Size := 16;
                AlphaMask := GetHeaderAlphaMask;

              End;
            End;

            RedShift := GetMaskShift(RedMask);
            GreenShift := GetMaskShift(GreenMask);
            BlueShift := GetMaskShift(BlueMask);
            AlphaShift := GetMaskShift(AlphaMask);

            RedSize := GetMaskSize(RedMask, RedShift);
            GreenSize := GetMaskSize(GreenMask, GreenShift);
            BlueSize := GetMaskSize(BlueMask, BlueShift);
            AlphaSize := GetMaskSize(AlphaMask, AlphaShift);

            If AlphaSize > 0 Then
            Begin
              // Le Mask Alpha cache-t-il une autre composante de la couleur
              If (RedMask or GreenMask or BlueMask) and AlphaMask <> 0 Then
              Begin
                AlphaMask := 0;
                AlphaShift := 0;
                AlphaSize := 0;
              End;
            End;
          End;
        End;
      End;
      else
      begin
        // Mauvais bitcount
         RaiseInvalidImageFile(Format(rsBitmapBitPerPixelNotSupported,[ABitCount]));
      End;
      // 64 : Notes les fichier DIB avec GDI+ peuvent avoir un bitcount de 64bits
    End;

    FLineEndGapSize := getPadding;
    If TopDown Then LineOrder := bloTopToBottom
    Else LineOrder := bloBottomToTop;
 (*   GlobalLogger.LogStatus('*Line Padding = ' + IntToStr(FLineEndGapSize));
    GlobalLogger.LogStatus('*BitCount = ' + IntToStr(ImageDescription.BitCount));
    With BitFields Do
    Begin
      GlobalLogger.LogStatus('Red Shift = ' + IntToStr(RedShift));
      GlobalLogger.LogStatus('Green Shift = ' + IntToStr(GreenShift));
      GlobalLogger.LogStatus('Blue Shift = ' + IntToStr(BlueShift));
      GlobalLogger.LogStatus('Alpha Shift = ' + IntToStr(AlphaShift));

      GlobalLogger.LogStatus('Red Size = ' + IntToStr(RedSize));
      GlobalLogger.LogStatus('Green Size = ' + IntToStr(GreenSize));
      GlobalLogger.LogStatus('Blue Size = ' + IntToStr(BlueSize));
      GlobalLogger.LogStatus('Alpha Size = ' + IntToStr(AlphaSize));
      GlobalLogger.LogStatus('BitFields Size = ' + IntToStr(Size));
    End; *)

  End;
End;


Procedure TBZBitmapBMPImage.LoadFromMemory();
Var
  YY, Y: Integer;
  X: Integer;
  DstColor, Color1, Color2: TBZColor;
  SrcPtrIdx : PByte;
  Idx:Byte;
  SrcPtr: PLongWord;
  SrcPtr16: PWord;
  SrcPtrRGB24: PBZColor24; //PBZColorBGR_24;
  DstLine: PBZColor;
  SrcColor: Longword;
  SrcColor16: Word;
  SrcColorRGB24: TBZColor24; // TBZColorBGR_24;
  LineBuffer: PByte;
  Done:Boolean;
  OpCode : TBZRLEOpCode;
  aCount, I, OpCodeSize : Integer;
  Data, Dx, Dy, low, hi : Byte;
  PaletteOS2x24, tmppal :PBZColorBGR_24;
  //PalCol : TBZColorBGR_24;
  //PalSize : Longint;
  Delta:Single;
  isOpaque : Boolean;

  procedure MakeOpaque;
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

Begin
  IsOpaque := True;
  FIgnoreAlpha := True;
  OpCode.Command:=0;
  OpCode.Count:=0;
  // On initialise les dimensions de notre bitmap
  SetSize(bmpWidth, bmpHeight);
  Clear(clrTransparent);
  InitProgress(bmpWidth,bmpHeight);
  StartProgressSection(0, ''); // On debute une nouvelle section globale

  Delta := 100 / Height;
  StartProgressSection(100 ,rsLoadingBitmapData);

  // Chargement de la palette si elle existe
  //GlobalLogger.LogNotice('Load palette : '+inttostr(ImageDescription.PaletteCount));
  if (ImageDescription.UsePalette) or (ImageDescription.PaletteCount>0) then  // and (ImageDescription.BitsPerPixel>8)
  begin
    If (FHeaderType = bmpht_OS21x)  then
    begin
      // On doit faire un saut en arriere de 4 octets. Pourquoi ????? je ne sais plus ;(
      // On pourrait également verifier avec FBMPInfoHeader.bfSize qui avec un format OSx indique
      // le debut de la palette et non pas la taille du fichier.

      Memory.SkipNextByte(-4);
      PaletteOs2x24:=nil;
      aCount := ImageDescription.PaletteCount-1;
      memReAlloc(PaletteOs2x24, ImageDescription.PaletteCount*3);
      Memory.Read(PaletteOs2x24^, ImageDescription.PaletteCount*3);
      tmppal:= PaletteOs2x24;

      Try
        For I:=0 to aCount do
        Begin

          //if ImageDescription.PixelFormat = pf4Bits then
          //begin
          //  With ImageDescription do
          //  begin
          //
          //    PaletteEntries^[I].Red:=tmppal^.Red;
          //    PaletteEntries^[I].Green:=tmppal^.Green;
          //    PaletteEntries^[I].Blue:=tmppal^.Blue;
          //    PaletteEntries^[I].Alpha:= 255;
          //  end;
          //End
          //else
          With ImageDescription do
          begin
            PaletteEntries^[I].Red:=tmppal^.Red;
            PaletteEntries^[I].Green:=tmppal^.Green;
            PaletteEntries^[I].Blue:=tmppal^.Blue;
            PaletteEntries^[I].Alpha:= 255;
          end;
          inc(tmppal);
        end;
      finally
        FreeMem(PaletteOs2x24);
        PaletteOs2x24:=nil
      end;
    End
    else
    begin
      //GlobalLogger.LogNotice('Palette Size : '+inttostr(ImageDescription.PaletteCount*4));
      Memory.Read(ImageDescription.PaletteEntries^, ImageDescription.PaletteCount*4);
      { les couleurs de la palette sont stockées au format BGR donc on Swap le canal Rouge et bleue
        Si c'est un fichier OS22 alors par défaut la valeur Alpha est à 0 (transparent).
        Il faut donc changer celle-ci par 255 (opaque) }
      if (FHeaderType = bmpht_OS22x) then
      begin
        For I:=0 to ImageDescription.PaletteCount-1 do
        Begin
          With ImageDescription do
          begin
            PaletteEntries^[I].Alpha:= 255;
            {$IFDEF LINUX}
            PaletteEntries^[I].AsInteger := PaletteEntries^[I].FastSwapRedBlue;
            {$ENDIF}
          end;
        end;
      end
      else
      begin
        {$IFDEF LINUX} // Sous Linux format couleur RGB on doit donc intervertir le Rouge et Blue
        For I:=0 to ImageDescription.PaletteCount-1 do
        Begin
          With ImageDescription do
          begin
           // PaletteEntries^[I].Alpha:= 255;
            PaletteEntries^[I].AsInteger := PaletteEntries^[I].FastSwapRedBlue;
            //GlobalLogger.LogStatus('Palette IDX : '+I.ToString+' : '+PaletteEntries^[I].ToString);
          end;
        end;
        {$ENDIF}
      End;
    End;
  End;

  Case ImageDescription.PixelFormat Of
    pf1bit:  // Compression Huffman1D possible. Utilisation palette possible, 2 couleurs
    Begin
      if (Memory.position<>FBMPFileHeader.bfOffBits) then Memory.Seek(FBMPFileHeader.bfOffBits,soBeginning);

      If (Compression = BMP_COMPRESSION_NONE) Then
      Begin
        Color1:=clrBlack;
        Color2:=clrWhite;
        if ImageDescription.PaletteCount>0 then
        begin
          Color1.AsInteger:= ImageDescription.PaletteEntries^[0].AsInteger;
          Color2.AsInteger:= ImageDescription.PaletteEntries^[1].AsInteger;
        end;
        LineBuffer := nil;
        memReAlloc(LineBuffer, FRowSize);
        Try
          Y:=0;
          Memory.Read(LineBuffer^, FRowSize);
          If TopDown Then  YY := Y Else YY := MaxHeight - Y;
          DstLine := GetPixelPtr(MaxWidth,YY);
          X:= Width;
          While (Y<Height) do
          begin
            // Blanc ou Noir ?
            //if ((PByte(SrcPtr+(X div 8))^ shr (7-(X and 7)) ) and 1)<>0 then Idx:=1 else Idx:=0;
            Idx := ExtractPixel1Bit(PByte(LineBuffer+(X div 8))^,X);
            if Idx <> 0 then  DstLine^:=Color2 else DstLine^:=Color1;
            FIgnoreAlpha := FIgnoreAlpha and (DstLine^.alpha = 0);
            Dec(X);
            Dec(DstLine);

            If X<=0 then
            begin
              Inc(Y);
              AdvanceProgress(Delta,0,1,False);
              Memory.Read(LineBuffer^, FRowSize);
              If TopDown Then  YY := Y Else YY := MaxHeight - Y;
              DstLine := GetPixelPtr(MaxWidth,YY);
              X:= Width;
            end;
          end;
        Finally
          FreeMem(LineBuffer);
          LineBuffer := nil;
        End;
      End;
     // Else If (Compression = BMP_COMPRESSION_BITF) Then // HUFFMAN1D
    End;
    pf2Bits :
    begin
      if (Memory.position<>FBMPFileHeader.bfOffBits) then Memory.Seek(FBMPFileHeader.bfOffBits,soBeginning);

      If (Compression = BMP_COMPRESSION_NONE) Then
      Begin
        LineBuffer := nil;
        memReAlloc(LineBuffer, FRowSize);
        Try
          Y:=0;
          Memory.Read(LineBuffer^, FRowSize);
          If TopDown Then  YY := Y Else YY := MaxHeight - Y;
          DstLine := GetPixelPtr(MaxWidth,YY);
          X:= Width;
          While (Y<Height) do
          begin
            Idx := ExtractPixel2Bits(PByte(LineBuffer+(X div 4))^,X);

            With Color1 do // on va chercher la couleur dans la palette
            Begin
              Red := ImageDescription.PaletteEntries^[Idx].Red;
              Green := ImageDescription.PaletteEntries^[Idx].Green;
              Blue := ImageDescription.PaletteEntries^[Idx].Blue;
              Alpha := ImageDescription.PaletteEntries^[Idx].Alpha;
            end;
            DstLine^:= Color1;
            FIgnoreAlpha := FIgnoreAlpha and (DstLine^.alpha = 0);
            Dec(X);
            Dec(DstLine);

            If X<=0 then
            begin
              Inc(Y);
              AdvanceProgress(Delta,0,1,False);
              Memory.Read(LineBuffer^, FRowSize);
              If TopDown Then  YY := Y Else YY := MaxHeight - Y;
              DstLine := GetPixelPtr(MaxWidth,YY);
              X:= Width;
            end;
          end;
        Finally
          FreeMem(LineBuffer);
          LineBuffer := nil;
        End;
      End;
    End;
    pf4bits: // Compression RLE possible Utilisation palette 16 couleurs
    Begin
      if (Memory.position<>FBMPFileHeader.bfOffBits) then Memory.Seek(FBMPFileHeader.bfOffBits,soBeginning);
      If (Compression = BMP_COMPRESSION_NONE) Then
      Begin
        LineBuffer := nil;
        memReAlloc(LineBuffer, FRowSize);
         Try
           Y := 0;
           Repeat
             Memory.Read(LineBuffer^, FRowSize);
             If TopDown Then  YY := Y Else YY := MaxHeight - Y;
             DstLine := GetPixelPtr(MaxWidth,YY);
             X:= MaxWidth;
             While (X>=0) Do
             Begin
               Idx := ExtractPixel4Bits(PByte(LineBuffer+(X shr 1))^,X);
               if (Idx<ImageDescription.PaletteCount) then
               begin
                 DstLine^.AsInteger:= ImageDescription.PaletteEntries^[Idx].AsInteger;
                 FIgnoreAlpha := FIgnoreAlpha and (DstLine^.alpha = 0);
               end;
              // else //GlobalLogger.LogWarning('Index : '+IntToStr(Idx)+' dans la pelette introuvable');
               Dec(DstLine);
               Dec(X);
             end;
             Inc(Y);
             AdvanceProgress(Delta,0,1,False);
           Until (Y > MaxHeight);
         Finally
           FreeMem(LineBuffer);
           LineBuffer := nil;
         End;
      End
      Else If (Compression = BMP_COMPRESSION_RLE4) Then
      Begin
        LineBuffer := nil;
        memReAlloc(LineBuffer, FRowSize);
        Y:=0;
        Try
          if TopDown Then  YY := Y Else YY := MaxHeight - Y;
          DstLine := GetScanLine(YY);
          X:=0;
          Done := False;
          OpCodeSize := sizeof(opcode);
          While not(Done) do
          begin
            aCount := Memory.Read(OpCode, OpCodeSize);
            if aCount <> OpCodeSize then
            begin
              RaiseInvalidImageFile(rsBMPRLE4BitDataCorrupted); //AddError
              //Continue;
            End;
            if aCount = OpCodeSize then
              if OpCode.Count = 0 then
              begin
                Case OpCode.Command of
                  0 : // Fin de ligne
                  begin
                    Inc(Y);
                    AdvanceProgress(Delta,0,1,False);
                    if TopDown Then  YY := Y Else YY := MaxHeight - Y;
                    DstLine := GetScanLine(YY);
                    X:=0;
                  end;
                  1 : Done := True; // Fin des données
                  2 :  // Déplacement
                  begin
                    Dx:= Memory.ReadByte;
                    Dy:= Memory.ReadByte;
                    Inc(X,Dx);
                    Dec(YY,Dy);
                    DstLine := GetPixelPtr(X,YY);
                  end;
                  else
                  begin
                    // OpCode.Command =  nombre de pixel
                    if (y > MaxHeight) or (X + OpCode.Command > Width) then
                    begin
                      AddError(rsBMPRLE4BitDataCorrupted); //RaiseInvalidImageFile();
                      //OpCode.Command := OpCode.Command - Width -(X + OpCode.Command);
                      Continue;
                    End;
                    Data:=0;
                    if (Y <= MaxHeight) or (X + OpCode.Command <= Width) then
                    for I := 0 to OpCode.Command - 1 do
                    begin
                      if (I and 1) = 0 then
                      begin
                        aCount := Memory.Read(Data,1);
                        if aCount <1 then
                        begin
                          RaiseInvalidImageFile(rsBMPRLE4BitInvalidInputBufferSize);
                          //continue;
                        End;
                        hi := (data and $F0) shr 4;
                        if (Hi<ImageDescription.PaletteCount) then
                        begin
                          DstLine^.AsInteger:= ImageDescription.PaletteEntries^[Hi].AsInteger;
                          FIgnoreAlpha := FIgnoreAlpha and (DstLine^.alpha = 0);
                        End;
                      end
                      else
                      begin
                        low := (data and $F);
                        if (Low<ImageDescription.PaletteCount) then
                        begin
                          DstLine^.AsInteger:= ImageDescription.PaletteEntries^[Low].AsInteger;
                          FIgnoreAlpha := FIgnoreAlpha and (DstLine^.alpha = 0);
                        End;
                      end;
                      Inc(X);
                      Inc(DstLine);
                    end;
                    // Un nombre impair d'octets est suivi d'un "padding" d'octet
                    if (byte(OpCode.command mod 4) in [1..2]) then
                    begin
                      aCount := Memory.Read(Data,1);
                      if aCount <1 then  RaiseInvalidImageFile(rsBMPRLE4BitInvalidInputBufferSize); // );
                    end;
                  end;
                end;
              end
              else
              begin
              if (y > MaxHeight) or (X + OpCode.Count > Width) then
              begin
                AddError(rsBMPRLE4BitDataCorrupted); //RaiseInvalidImageFile();
                //OpCode.Count := OpCode.Count - (Width -(X + OpCode.Count));
                Continue;
              End;
              if (y <= MaxHeight) or (X + OpCode.Count <= Width) then
              begin
                hi := (OpCode.Command and $F0) shr 4;
                low := (OpCode.Command and $F);
                for I := 0 to OpCode.Count - 1 do
                begin
                  if (I and 1) = 0 then
                  begin
                    if (Hi<ImageDescription.PaletteCount) then
                    begin
                      DstLine^.AsInteger:= ImageDescription.PaletteEntries^[Hi].AsInteger;
                      FIgnoreAlpha := FIgnoreAlpha and (DstLine^.alpha = 0);
                    End;
                  End
                  else
                  begin
                    if (low<ImageDescription.PaletteCount) then
                    begin
                      DstLine^.AsInteger:= ImageDescription.PaletteEntries^[Low].AsInteger;
                      FIgnoreAlpha := FIgnoreAlpha and (DstLine^.alpha = 0);
                    End;
                  End;

                  Inc(X);
                  Inc(DstLine);
                end;
              end;
            End;
          end;
        Finally
           FreeMem(LineBuffer);
           LineBuffer := nil;
         End;
      End;
    End;
    pf8Bits: // Compression RLE possible. Utilisation palette 256 couleurs
    Begin
      { Le GapSize existe ou est plus grand que la taille de la palette de couleurs
        On doit donc se repositionner sur le debut des données }
      if (Memory.position<>FBMPFileHeader.bfOffBits) then Memory.Seek(FBMPFileHeader.bfOffBits,soBeginning);

      If (Compression = BMP_COMPRESSION_NONE) Then
      Begin
         LineBuffer := nil;
         memReAlloc(LineBuffer, FRowSize);
         Try
           Y := 0;
           Repeat
             Memory.Read(LineBuffer^, FRowSize);
             If TopDown Then  YY := Y Else YY := MaxHeight - Y;
             DstLine := GetScanLine(YY);
             SrcPtrIdx:=PByte(LineBuffer);
             X:= Width;
             While (X>0) Do
             Begin
               Idx:=SrcPtrIdx^;
               if (Idx<ImageDescription.PaletteCount) then
               begin
                 DstLine^.AsInteger:= ImageDescription.PaletteEntries^[Idx].AsInteger;
                 FIgnoreAlpha := FIgnoreAlpha and (DstLine^.alpha = 0);
               end else AddError(Format(rsBitmapBadPaletteIndex,[Idx]));
               Inc(SrcPtrIdx);
               Inc(DstLine);
               Dec(X);
             end;
             Inc(Y);
             AdvanceProgress(Delta,0,1,False);
           Until (Y > MaxHeight);
         Finally
           FreeMem(LineBuffer);
           LineBuffer := nil;
         End;
      End
      Else If (Compression = BMP_COMPRESSION_RLE8) Then
      Begin
        LineBuffer := nil;
        memReAlloc(LineBuffer, FRowSize);
        Y:=0;
        Try
          if TopDown Then  YY := Y Else YY := MaxHeight - Y;
          DstLine := GetScanLine(YY);
          X:=0;
          Done := False;
          OpCodeSize := sizeof(opcode);
          While not(Done) do
          begin
            aCount := Memory.Read(OpCode, OpCodeSize);
            if acount<1 then
            begin
              Done := true;
              Continue;
            End;
            if aCount <> OpCodeSize then AddError(rsBMPRLE8BitInvalidInputBufferSize); //RaiseInvalidImageFile();
            if OpCode.Count = 0 then
            begin
              Case OpCode.Command of
                0 : // Fin de ligne
                begin
                  Inc(Y);
                  AdvanceProgress(Delta,0,1,False);
                  if TopDown Then  YY := Y Else YY := MaxHeight - Y;
                  DstLine := GetScanLine(YY);
                  X:=0;
                end;
                1 : Done := True; // Fin des données
                2 :  // Déplacement
                begin
                  Dx:= Memory.ReadByte;
                  Dy:= Memory.ReadByte;
                  Inc(X,Dx);
                  Dec(YY,Dy);
                  DstLine := GetPixelPtr(X,YY);
                end;
                else
                begin
                  if (Y > MaxHeight) or (X + OpCode.Command > Width) then
                  begin
                    AddError(rsBMPRLE8BitInvalidInputBufferSize);  //RaiseInvalidImageFile);
                    OpCode.Command :=OpCode.Command - (Width - (X + OpCode.Command));
                  end;

                  for I := 0 to OpCode.Command - 1 do
                  begin
                    aCount := Memory.Read(Data,1);
                    if aCount <1 then
                    Begin
                      AddError(rsBMPRLE8BitInvalidInputBufferSize); //RaiseInvalidImageFile);
                      Inc(X);
                      Inc(DstLine);
                      continue;
                    End
                    else
                    begin
                      if (Data<ImageDescription.PaletteCount) then
                      begin
                         if (X<Width) then
                         begin
                           DstLine^.AsInteger:= ImageDescription.PaletteEntries^[Data].AsInteger;
                           FIgnoreAlpha := FIgnoreAlpha and (DstLine^.alpha = 0);
                         End else AddError(rsBMPRLE8BitDataCorrupted);
                      End else AddError(rsBMPRLE8BitDataCorrupted);
                      Inc(X);
                      Inc(DstLine);
                    End;
                  end;
                  // An odd number of bytes is followed by a pad byte.
                  if ((OpCode.command mod 2) = 1) then
                  begin
                    aCount := Memory.Read(Data,1);
                    if aCount <1 then
                    begin
                      //RaiseInvalidImageFile(rsBMPRLE8BitInvalidInputBufferSize);
                      AddError(rsBMPRLE8BitInvalidInputBufferSize);//
                      //Memory.SeekBackward(1);
                    End;
                  end;
                end;
              end;
            end
            else
            begin
              if (y > MaxHeight) or (X + OpCode.Count > Width) then
              begin
                AddError(rsBMPRLE8BitDataCorrupted); //RaiseInvalidImageFile();
                OpCode.Count := Width;
              End;
              for I := 0 to OpCode.Count - 1 do
              begin
                if (OpCode.Command<ImageDescription.PaletteCount) then
                begin
                  if (X<Width) then
                  begin
                    DstLine^.AsInteger:= ImageDescription.PaletteEntries^[OpCode.Command].AsInteger;
                    FIgnoreAlpha := FIgnoreAlpha and (DstLine^.alpha = 0);
                  End else AddError(rsBMPRLE8BitDataCorrupted);
                End else AddError(rsBMPRLE8BitDataCorrupted);
                Inc(X);
                Inc(DstLine);
              end;
            end;
          end;
        Finally
           FreeMem(LineBuffer);
           LineBuffer := nil;
         End;
      End;
    End;
    pf16Bits: // Pas de compression, formats couleurs suivant "BitField" XRGB_1555, ARGB_4444, RGB/BGR_565
    Begin
      if (Memory.position<>FBMPFileHeader.bfOffBits) then Memory.Seek(FBMPFileHeader.bfOffBits,soBeginning);
      LineBuffer := nil;
      memReAlloc(LineBuffer, FRowSize);
      Try
        Y := 0;
        Repeat
          Memory.Read(LineBuffer^, FRowSize);
          If TopDown Then YY := Y Else YY := MaxHeight - Y;
          DstLine := GetScanLine(YY);
          SrcPtr16 := PWord(LineBuffer);
          For X := 0 To MaxWidth Do
          Begin
            SrcColor16 := PWord(SrcPtr16+X)^;
            DstColor := ConvertBitFieldsToBZColor(ImageDescription.BitFields, SrcColor16);
            FIgnoreAlpha := FIgnoreAlpha and (DstColor.alpha = 0);
            IsOpaque := IsOpaque and (DstColor.alpha = 255);
            //DstColor.Alpha := 255;
            DstLine^ := DstColor;
            Inc(DstLine);
          End;
          Inc(Y);
          AdvanceProgress(Delta,0,1,False);
        Until (Y > MaxHeight);
      finally
        FreeMem(LineBuffer);
        LineBuffer := nil;
      end;
    End;
    pf24Bits: // Pas de compression sauf si en-tête OS2x V2 alors compresion RLE. Formats couleurs suivant "BitField" sinon BGR
    Begin
      If (FHeaderType = bmpht_OS21x) or (FHeaderType = bmpht_OS22x) Then
      Begin
        if (FHeaderType = bmpht_OS21x) then  // --> Lecture Format BGR
        begin
          if (Memory.position<>FBMPFileHeader.bfOffBits) then Memory.Seek(FBMPFileHeader.bfOffBits,soBeginning);
          LineBuffer := nil;
          GetMem(LineBuffer, FRowSize);
          Try
            Y := 0;
            Repeat
              Memory.Read(LineBuffer^, FRowSize);
              If TopDown Then  YY := Y Else YY := MaxHeight - Y;
              DstLine := GetScanLine(YY);
              SrcPtrRGB24 := PBZColor24(LineBuffer);  //a Remplacer par --> PBZColorBGR_24  ???
              X:= Width;
              While (X>0) Do
              Begin
                SrcColorRGB24 := SrcPtrRGB24^;  // ! \ Les données sont au format BGR et pas RGB
                {$IFDEF WINDOWS} // BGRA
                DstColor.Blue := SrcColorRGB24.Red;
                DstColor.Green := SrcColorRGB24.Green;
                DstColor.Red :=  SrcColorRGB24.Blue;
                {$ELSE}
                DstColor.Red :=  SrcColorRGB24.Red;
                DstColor.Green := SrcColorRGB24.Green;
                DstColor.Blue := SrcColorRGB24.Blue;
                {$ENDIF}
                DstColor.Alpha := 255;
                DstLine^ := DstColor;
                Inc(SrcPtrRGB24);
                Inc(DstLine);
                Dec(X);
              End;
              Inc(Y);
              AdvanceProgress(Delta,0,1,False);
            Until (Y > MaxHeight);
          Finally
            FreeMem(LineBuffer);
            LineBuffer := nil;
          End;
        end
        else if (FHeaderType = bmpht_OS22x) then
        begin
          if ImageDescription.PaletteCount > 0 then Memory.Read(ImageDescription.PaletteEntries^, ImageDescription.PaletteCount*4);
          if (Compression = BMP_COMPRESSION_JPEG) then
          begin
          // Compression RLE 24 bits
          end
          else
          begin  // --> Lecture format BGR
            //GlobalLogger.LogNotice('Taille du Stream : ' + Memory.Size.ToString);
            //GlobalLogger.LogNotice('Taille d''une ligne : ' + FRowSize.ToString);
            LineBuffer := nil;
            GetMem(LineBuffer, FRowSize);
            Try
              Y := 0;
              Repeat
                Memory.Read(LineBuffer^, FRowSize);
                //GlobalLogger.LogNotice('Postion Memory After Read : ' + Memory.Position.ToString);
                If TopDown Then  YY := Y Else YY := MaxHeight - Y;
                DstLine := GetScanLine(YY);
                SrcPtrRGB24 := PBZColor24(LineBuffer);
                X:= 0;
                While (X<=MaxWidth) Do
                Begin
                  SrcColorRGB24 := SrcPtrRGB24^;
                  {$IFDEF WINDOWS} // BGRA
                  DstColor.Blue := SrcColorRGB24.Red;
                  DstColor.Green := SrcColorRGB24.Green;
                  DstColor.Red :=  SrcColorRGB24.Blue;
                  {$ELSE}
                  DstColor.Red :=  SrcColorRGB24.Blue;
                  DstColor.Green := SrcColorRGB24.Green;
                  DstColor.Blue := SrcColorRGB24.Red;
                  {$ENDIF}
                  DstColor.Alpha := 255;
                  DstLine^ := DstColor;

                  Inc(SrcPtrRGB24);
                  Inc(DstLine);
                  Inc(X);
                End;

                Inc(Y);

                AdvanceProgress(Delta,0,1,False);
              Until (Y > MaxHeight);
            Finally
              FreeMem(LineBuffer);
              LineBuffer := nil;
            End;
          end;
        end;
      End
      Else
      Begin // --> Formats, Windows - Lecture Format BGR
        if (Memory.position<>FBMPFileHeader.bfOffBits) then Memory.Seek(FBMPFileHeader.bfOffBits,soBeginning);
        LineBuffer := nil;
        GetMem(LineBuffer, FRowSize);
        Try
          Y := 0;
          Repeat
            Memory.Read(LineBuffer^, FRowSize);
            If TopDown Then  YY := Y Else YY := MaxHeight - Y;
            DstLine := GetScanLine(YY);
            SrcPtrRGB24 := PBZColor24(LineBuffer);
            X:= 0;
            While (X<=MaxWidth) Do
            Begin
              SrcColorRGB24 := SrcPtrRGB24^;
              {$IFDEF WINDOWS} // BGRA
              DstColor.Blue := SrcColorRGB24.Red;
              DstColor.Green := SrcColorRGB24.Green;
              DstColor.Red :=  SrcColorRGB24.Blue;
              {$ELSE}
              DstColor.Blue:=  SrcColorRGB24.Red;
              DstColor.Green := SrcColorRGB24.Green;
              DstColor.Red := SrcColorRGB24.Blue;
              {$ENDIF}
              DstColor.Alpha := 255;
              DstLine^ := DstColor;
              Inc(SrcPtrRGB24);
              Inc(DstLine);
              Inc(X);
            End;
            Inc(Y);
            AdvanceProgress(Delta,0,1,False);
          Until (Y > MaxHeight);
        Finally
          FreeMem(LineBuffer);
          LineBuffer := nil;
        End;
      End;
    End;
    pf32Bits:  // Pas de compression, formats couleurs suivant "BitField" sinon format couleur par defaut BGRA
    Begin
      //GlobalLogger.LogNotice('Taille du Stream : ' + Memory.Size.ToString);
      //GlobalLogger.LogNotice('Taille d''une ligne : ' + FRowSize.ToString);
      LineBuffer := nil;
      GetMem(LineBuffer, FRowSize);
      Try
        Y := 0;
        //DstColor.Alpha:= 255;
        Repeat
          //With GlobalLogger do
          //begin
          //  LogNotice('Y = '+Y.ToString());
          //  LogNotice('Adresse_Y = '+ Inttostr(Y * (Width * 4)));
          //  LogNotice('Postion Memory Before Read : ' + Memory.Position.ToString);
          //End;
          Memory.Read(LineBuffer^, FRowSize);
          //GlobalLogger.LogNotice('Postion Memory After Read : ' + Memory.Position.ToString);
          If TopDown Then  YY := Y Else YY := MaxHeight - Y;
          DstLine := GetScanLine(YY);
          SrcPtr := PLongWord(LineBuffer);
          X:= Width;
          //GlobalLogger.LogNotice('Adresse_Dernier_pixel_ligne = '+ Inttostr(((Y * (Width * 4)) + (X-1)*4)));
          While (X>0) Do
          Begin
            SrcColor := SrcPtr^;
            DstColor := ConvertBitFieldsToBZColor(ImageDescription.BitFields, SrcColor);
            FIgnoreAlpha := FIgnoreAlpha and (DstColor.alpha = 0);
            IsOpaque := IsOpaque and (DstColor.alpha = 255);
            DstLine^ := DstColor;
            Inc(SrcPtr);
            Inc(DstLine);
            Dec(X);
          End;

          Inc(Y);
          AdvanceProgress(Delta,0,1,False);
        Until (Y > MaxHeight);
      Finally
        FreeMem(LineBuffer);
        LineBuffer := nil;
      End;
    End;
  End;



  // Dans le cas ou tous les pixels sont transparent
  If FIgnoreAlpha Then
  Begin
    //GlobalLogger.LogNotice('IgnoreAlpha');
    MakeOpaque;
    if (ImageDescription.PixelFormat = pf32bits) then
    begin
      if ((ImageDescription.BitFields.AlphaSize=0) and (ImageDescription.BitFields.Size=16)) then ImageDescription.ColorFormat := cfXBGR;
    End
    else  ImageDescription.ColorFormat := cfBGR;

    ImageDescription.HasAlpha := False;
  End
  else
  begin
    if ImageDescription.PixelFormat = pf32bits then
      if ((ImageDescription.BitFields.AlphaSize=0) and (ImageDescription.BitFields.Size=16)) then
      begin
        ImageDescription.ColorFormat := cfXBGR;
      End;

    if IsOpaque then ImageDescription.HasAlpha := False
    else ImageDescription.HasAlpha := True;
  End;

  FinishProgressSection(False);
  FinishProgressSection(True);
End;

function TBZBitmapBMPImage.ComputePaletteOrMaskSize : Integer;
begin
  Result := 0;
  if UsePalette and (ABitCount <= 8) then
  begin
    Result := ColorManager.ColorCount;
    if Result = 0 then Result := 1 shl ABitCount;
  end
  else
  begin
    Case AbitCount of
      15,16 : result := 3;  //GapSize1
      32 : if DataFormatDesc.Encoding = etBitfields then result := 4 else result:=0;
    End;
  End;
  Result := Result * 4;
End;

function TBZBitmapBMPImage.ComputeCompressionType : LongWord;
begin
  //BMP_COMPRESSION_HUF1D
  //BMP_COMPRESSION_JPEG
  //BMP_COMPRESSION_PNG
  //BMP_COMPRESSION_ALPHABITF
  Result  := BMP_COMPRESSION_NONE;
  if (ABitCount = 16) then Result  := BMP_COMPRESSION_BITF;
  Case DataFormatDesc.Encoding of
    etBitFields : if (ABitCount = 16) or (ABitCount>=32) then Result:=BMP_COMPRESSION_BITF;
    etRLE :
    begin
      Case ABitCount of
        4 : Result := BMP_COMPRESSION_RLE4;
        8 : Result := BMP_COMPRESSION_RLE8;
      end;
    End;
  end;
End;

function TBZBitmapBMPImage.getInfoHeaderSize(aVersion : TBZBMPHeaderType): Integer;
begin
  Result := 0;
  Case aVersion of
    bmpht_WindowsV1 : Result:= 36;
    bmpht_WindowsV4 : Result:= 108;
    bmpht_WindowsV5 : Result:= 124;
  End;
End;

procedure TBZBitmapBMPImage.InitBitFieldMasks;
begin
  RedMask   := $00000000;
  GreenMask := $00000000;
  BlueMask  := $00000000;
  AlphaMask := $00000000;
  if ABitCount >8 then
  begin
    Case ABitCount of
      15:
      begin
        //format XRGB 1555
        AlphaMask := $00008000;
        RedMask   := $00007C00;
        GreenMask := $000003E0;
        BlueMask  := $0000001F;
        ABitCount := 16;
      End;
      16:
      begin
        // format RGB 565
        RedMask   := $00007C00;
        GreenMask := $000003E0;
        BlueMask  := $0000001F;
        AlphaMask := $00000000;
      End;
      24: //BGR
      begin
        RedMask   := $00FF0000;
        GreenMask := $0000FF00;
        BlueMask  := $000000FF;
        AlphaMask := $00000000;
      End;
      32: //BGRA
      begin
        RedMask   := $00FF0000;
        GreenMask := $0000FF00;
        BlueMask  := $000000FF;
        AlphaMask := $FF000000;
      End;
      //64;
    End;
  End;
End;

procedure TBZBitmapBMPImage.InitSaving(aVersion : TBZBMPHeaderType);
begin
  // On initialise quelques variables et paramètres
  With ImageDescription do
  begin
    RowStrideType := bleDWordBoundary; //Alignement des lignes sur 32bits
    bmpWidth := Width;
    bmpHeight := Height;
    ABitCount := BitsPerPixel;
     //GlobalLogger.LogStatus('BitsPerPixel    : '+Inttostr(BitsPerPixel));
  End;
  //GlobalLogger.LogStatus('Width  : '+Inttostr(bmpWidth));
  //GlobalLogger.LogStatus('Height : '+Inttostr(bmpHeight));
  //GlobalLogger.LogStatus('Bpp    : '+Inttostr(ABitCount));

 // id:='MB';
  FBmpFileHeader.bfType:=BMP_MAGIC_WINBMP;

  If DataFormatDesc.Encoding = etRLE then
  begin
    FNeedAdjustBFH:=True;
    FBmpFileHeader.bfSize := 14 + 4+ getInfoHeaderSize(aVersion) + ComputePaletteOrMaskSize;      //Ce paramètre est à recalculer un peu plus tard si compression RLE
  End
  Else FBmpFileHeader.bfSize := 14 + 4+ getInfoHeaderSize(aVersion) + ComputePaletteOrMaskSize + ((bmpWidth*bmpHeight)*ImageDescription.PixelSize);

  With FBmpFileHeader do
  begin
    bfReserved1:=0;
    bfReserved2:=0;
    bfOffBits:= 14 + 4+getInfoHeaderSize(aVersion)+ComputePaletteOrMaskSize; //sizeof(TBZBmpFileHeader)
  End;
End;

Function TBZBitmapBMPImage.CreateInfoHeader(aVersion : TBZBMPHeaderType):TBZBitmapBMPInfoHeader;
begin
  Case aVersion of
    bmpht_unknown : raise EBZSaveImageException.Create('Erreur lors de la sauvegarde de l''image BMP : En-tête inconnue');
    bmpht_Os21x, bmpht_Os22x : raise EBZSaveImageException.Create('Erreur lors de la sauvegarde de l''image BMP : '+#13+#10+
                                                                   ' Type Os21x et Os22x non supportés');
    bmpht_WindowsV2, bmpht_WindowsV3 : EBZSaveImageException.Create('Erreur lors de la sauvegarde de l''image BMP : '+#13+#10+
                                                                       ' Type Windows Version 2 et 3 non supportés');
    bmpht_WindowsV1 :
    Begin
      With Result.WindowsV1 Do
      begin
        biWidth           := bmpWidth;
        biHeight          := bmpHeight;
        biPlanes          := 1;
        if ABitCount = 15 then biBitCount:=16 else   biBitCount:= ABitCount;
        biCompression     := ComputeCompressionType;
        biSizeImage:=(bmpWidth*bmpHeight)*(ABitCount Div 8);
        biXPixelsPerMeter:=100;
        biYPixelsPerMeter:=100;
        if ABitCount<=8 then biClrUsed:=ComputePaletteOrMAskSize else biClrUsed:=0;
        biClrImportant:=0;
      End;
    End;
    bmpht_WindowsV4 :
    Begin
      With Result.WindowsV4 Do
      begin
        biWidth           := bmpWidth;
        biHeight          := bmpHeight;
        biPlanes          := 1;
        biBitCount        := ABitCount;

        biCompression     := ComputeCompressionType;
        //biSizeImage:=0;          //Ce paramètre est calculer un peu plus tard
        biXPixelsPerMeter:=100;
        biYPixelsPerMeter:=100;
        if ABitCount<=8 then biClrUsed:=ComputePaletteOrMaskSize else biClrUsed:=0;
        biClrImportant:=0;

       (* biRedMask: Longword;             //< Masque Couleur Rouge
        biGreenMask: Longword;           //< Masque Couleur Vert
        biBlueMask: Longword;            //< Masque Couleur Bleu
        biAlphaMask: Longword;           //< Masque Couleur Alpha

        biCSType: Longword;              //< Type de l'espace de couleur CIE
        biEndpoints: TBZBMP_CIEXYZCoordTriple; //< "Color space endpoints"

        biGammaRed: Longword;            //< Correction Gamma Rouge
        biGammaGreen: Longword;          //< Correction Gamma Vert
        biGammaBlue: Longword;           //< Correction Gamma Bleu  *)
      End;
    End;
    bmpht_WindowsV5 :
    begin
      With Result.WindowsV5 Do
      begin
        biWidth           := bmpWidth;
        biHeight          := bmpHeight;
        biPlanes          := 1;
        biBitCount        := ABitCount;

        biCompression     := ComputeCompressionType;
        //biSizeImage:=0;          //Ce paramètre est calculer un peu plus tard
        biXPixelsPerMeter:=100;
        biYPixelsPerMeter:=100;
        if ABitCount<=8 then biClrUsed:=ComputePaletteOrMaskSize else biClrUsed:=0;
        biClrImportant:=0;

       (* biRedMask: Longword;             //< Masque Couleur Rouge
        biGreenMask: Longword;           //< Masque Couleur Vert
        biBlueMask: Longword;            //< Masque Couleur Bleu
        biAlphaMask: Longword;           //< Masque Couleur Alpha

        biCSType: Longword;              //< Type de l'espace de couleur CIE
        biEndpoints: TBZBMP_CIEXYZCoordTriple; //< "Color space endpoints"

        biGammaRed: Longword;            //< Correction Gamma Rouge
        biGammaGreen: Longword;          //< Correction Gamma Vert
        biGammaBlue: Longword;           //< Correction Gamma Bleu

        biIntent: Longword;              //< "rendering intent"

        biProfileData: Longword;         //< Position des données du profile de couleur
        biProfileSize: Longword;         //< Taille des données du profile de couleur

        biReserved: Longword;            //< Réservé *)
      End;
    End;
  end;
end;

procedure TBZBitmapBMPImage.SaveHeader;
var
  BIH : TBZBitmapBMPInfoHeader;
begin
  if not(DataFormatDesc.VersionAsInteger in [1,4,5]) then DataFormatDesc.VersionAsInteger:=1;

  Case DataFormatDesc.VersionAsInteger of
    1 :
    begin
      InitSaving(bmpht_WindowsV1);
      InitBitFieldMasks;
      BIH := CreateInfoHeader(bmpht_WindowsV1);
      //ShowMessage('ID = '+FBmpFileHeader.bfType);
      Memory.Write(FBmpFileHeader,14);
      FHeaderSize := 40;
      Memory.WriteLongWord(FHeaderSize);
      Memory.Write(BIH.WindowsV1,36);
      if (ABitCount>8) and (BIH.WindowsV1.biCompression = BMP_COMPRESSION_BITF) then
      begin
        Case ABitCOunt of
          15:
          begin
            With Memory do
            begin
              WriteLongWord(RedMask);
              WriteLongWord(GreenMask);
              WriteLongWord(BlueMask);
              WriteLongWord(AlphaMask);
            End;
          End;
          16:
          begin
            With Memory do
            begin
              WriteLongWord(RedMask);
              WriteLongWord(GreenMask);
              WriteLongWord(BlueMask);
            End;
          End;
          32:
          begin
            With Memory do
            begin
              WriteLongWord(RedMask);
              WriteLongWord(GreenMask);
              WriteLongWord(BlueMask);
              WriteLongWord(AlphaMask);
            End;
          End;
          //64;
        End;
      End;
    End;
    4 :
    begin
      InitSaving(bmpht_WindowsV4);
      InitBitFieldMasks;
      BIH := CreateInfoHeader(bmpht_WindowsV4);
      Memory.Write(FBmpFileHeader,14);
      Memory.WriteLongWord(FHeaderSize);
      Memory.Write(BIH.WindowsV4,108);
    End;
    5 :
    begin
      InitSaving(bmpht_WindowsV4);
      InitBitFieldMasks;
      BIH := CreateInfoHeader(bmpht_WindowsV4);
      Memory.Write(FBmpFileHeader,14);
      Memory.WriteLongWord(FHeaderSize);
      Memory.Write(BIH.WindowsV5,124);
    End;
  End;
End;

procedure TBZBitmapBMPImage.SavePalette;
begin
  if ImageDescription.UsePalette then
  begin
    if (ABitCount<=8) then
    begin

    End
    else
    begin

    End;
  End;
End;

procedure TBZBitmapBMPImage.SaveImageData;
var
  x,y : Integer;
  AColor : TBZColor;
  ColorBGR24 : TBZColorBGR_24;
  ColorBGRA32 : TBZColor;
begin
  Case ABitCount of
    1:
    begin

    End;
    2:
    begin

    End;
    4:
    begin

    End;
    8:
    begin

    End;
    15 :
    begin

    End;
    24 :
    begin
      For y:=MaxHeight downto 0 do
      begin
        For X:=0 to MaxWidth do
        begin
          AColor := GetPixel(X,Y);
          //Format BGR
          //ColorBGR24 :=AColor.SwapRBChannels.; SwapChannels(scmToBGR). .AsVector3b; //BZColorToBGR24(AColor);
          ColorBGR24.Blue := AColor.Blue;
          ColorBGR24.Green := AColor.Green;
          ColorBGR24.Red := AColor.Red;
          Memory.Write(ColorBGR24,3);
          //if ImageDescription.RowStrideLength>0 then  // padding de fin de ligne
          //for i:=1 to ImageDescription.RowStrideLength do Memory.WriteByte(0);
        End;
      End;
    End;
    32 :
    begin
      For y:=MaxHeight downto 0 do
      begin
        For X:=0 to MaxWidth do
        begin
          AColor := GetPixel(X,Y);
          {$IFDEF LINUX}
          AColor := AColor.SwapRBChannels;
          {$ENDIF}
          Memory.Write(AColor,4);
        End;
      End;
    End;
    // 64 :
  End;
End;

Procedure TBZBitmapBMPImage.SaveToMemory();
begin
  DataFormatDesc.VersionAsInteger := 1;
  DataFormatDesc.Encoding := etBitFields;
  AbitCount := 32;
  SaveHeader;
  if ImageDescription.UsePalette then SavePalette;
  SaveImageData;
End;

{%endregion%}

Initialization

  RegisterRasterFormat('bmp', 'Windows OS2 Bitmap', TBZBitmapBMPImage);
  RegisterRasterFormat('dib', 'Windows DIB Bitmap', TBZBitmapBMPImage);
  //RegisterRasterFormat('ico', 'Windows OS2 Icon', TBZBitmapBMPImage);
  //RegisterRasterFormat('cur', 'Windows OS2 Cursor', TBZBitmapBMPImage);
  RegisterRasterFormat('rle', 'Windows OS2 Bitmap', TBZBitmapBMPImage);

Finalization
  UnregisterRasterFormat(TBZBitmapBMPImage);
End.
