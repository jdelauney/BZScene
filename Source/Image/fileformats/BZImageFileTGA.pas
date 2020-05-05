(*
  @abstract(Prise en charge des Images au format TrueVision TARGA en lecture et écriture. Version 1 et 2.)

  Spécifications : @br
  @unorderedList(
    @item(Méthode de compression    : Aucune, RLE, non-compressé
    @item(Nombre de couleurs	      : 8 bits, 16 bits format BGR 555, 24 bits ou 32 bits BGR(A)
    @item(Supporte plusieurs images : Non, une seule image dans un même fichier
    @item(Format des nombres	      : Big-endian
    @item(Auteur	                  : Truevision, Inc.
    @item(Extensons                 : *.tga; *.vst; *.icb; *.vda
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
  Informations sur le format TGA :
  @unorderedList(
     @item(http://tfcduke.developpez.com/tutoriel/format/tga/)
     @item(http://files.avanquest.com/file-extension-tga/fr/)
     @item(https://en.wikipedia.org/wiki/Truevision_TGA)
     @item(https://fr.wikipedia.org/wiki/Truevision_Targa)
     @item(http://www.fileformat.info/format/tga/corion.htm)
     @item(http://fileformats.archiveteam.org/wiki/TGA)
     @item(http://www.paulbourke.net/dataformats/tga/)
     @item(https://www.loc.gov/preservation/digital/formats/fdd/fdd000180.shtml)
  )

  Fichiers test :@br
  @unorderedList(
    @item(https://github.com/npedotnet/TGAReader)
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Dépendances) : BZClasses, BZColors, BZGraphic, BZBitmap, BZImageFileIO, BZUtils

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)
    - Tous les liens au dessus

  -------------------------------------------------------------------------------------------------------------

  @bold(Licence) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
Unit BZImageFileTGA;

//------------------------------------------------------------------------------
//----------------------------[ TODO LIST ]-------------------------------------

{ TODO 0  -oBZBitmap -cSupport_Images_TGA : Ecriture Format TGA }
{ TODO 0  -oBZBitmap -cSupport_Images_TGA : Ameliorer le support de la lecture des Version 2.0 }

//------------------------------------------------------------------------------

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils,
  BZClasses, BZColors, BZGraphic, BZBitmap, BZImageFileIO;

{ Constantes pour les types d'images
  @groupbegin  }
const
  TARGA_NO_COLORMAP = 0;
  TARGA_COLORMAP = 1;

  TARGA_EMPTY_IMAGE = 0;
  TARGA_INDEXED_IMAGE = 1;
  TARGA_TRUECOLOR_IMAGE = 2;
  TARGA_BW_IMAGE = 3;
  TARGA_INDEXED_RLE_IMAGE = 9;
  TARGA_TRUECOLOR_RLE_IMAGE = 10;
  TARGA_BW_RLE_IMAGE = 11;

  TARGA_DESCRIPTOR_RIGHT_ORIGIN = $10;
  TARGA_DESCRIPTOR_UPPER_ORIGIN = $20;
  {@groupend }

Type
  { TBZTGAFileHeader : Description de l'en-tête des fichiers TGA }
  TBZTGAFileHeader = packed record
    IDLen          : Byte;     // Nombre de caractères dans le "ImageID"
    ColorMapType   : Byte;     // Type de la palette
    ImageType      : Byte;     // Type d'image
    ColorMapOrigin : Word;     // Début dans la palette
    ColorMapCount  : Word;     // Nombre d'éléments  palette
    ColorMapEntrySize : Byte;  // Taille d'un élément dans la palette
    OriginX        : Word;
    OriginY        : Word;
    Width          : Word;     // Largeur
    Height         : Word;     // Hauteur
    PixelSize      : Byte;     // Taile d'un Pixel
    Descriptor : Byte;
  end;
  PBZTGAFileHeader = ^TBZTGAFileHeader;

  { TBZTGAFileFooter : Description de l'en-tête de fin des fichiers TGA }
  TBZTGAFileFooter = packed record
    ExtAreaOffset : Longint;
    DevDirOffset  : Longint;
    Signature     : array[1..16] of Char;
    ReservedChar  : Char;
    NullByte      : Char;
  end;

  { TBZTGAExtensionArea  : Description des informations des fichiers TGA (Version 2.0 uniquement) }
  TBZTGAExtensionArea = packed record
    Size : Word;
    AuthorName : Array[0..40] of char;
    AuthorComment : Array[0..383] of char;
    Day : Integer;
    Month : Integer;
    Year : Integer;
    JobeName : Array[0..40] of char;
    JobTimeH : Word;
    JobTimeM : Word;
    JobTimeS : Word;
    SoftwareID : Array[0..40] of char;
    SoftwareVersionMinor : Byte;
    SoftwareVersionMajor : Byte;
    SoftwareVersionBuild : Byte;
    KeyColor : LongWord;
    PixelAspectRatio : LongWord;
    ColorCorrectionOffset : Integer;
    StampOffset : LongWord;
    ScanLineOffset : LongWord;
    Attribut : Byte;
  end;
  { Suivi de :
    Scan Line Table - Field 25 (Variable)
    Postage Stamp Image - Field 26 (Variable)
    Color Correction Table - Field 27 (2K Bytes) }

Type

  { TBZBitmapTGASavingOptions }

  TBZBitmapTGASavingOptions = Class(TBZUpdateAbleObject)
  private
    FCompressed : Boolean;
    FBitsPerPixel : TBZPixelFormat;
  public
    Constructor Create; override;

    property Commpressed : Boolean read FCompressed write FCompressed;
    property BitsPerPixel : TBZPixelFormat read FBitsPerPixel write FBitsPerPixel;
  end;

  { TBZBitmapTGAImage }

  TBZBitmapTGAImage = Class(TBZCustomImageFileIO)
  private
    FTGAFileHeader: TBZTGAFileHeader;
    FSavingOptions : TBZBitmapTGASavingOptions;
  protected
    FlipDataV,FlipDataH : Boolean;
    AlphaBits : Byte;

    Procedure LoadFromMemory(); override;
    procedure SaveToMemory(); override;
    Function CheckFormat(): Boolean; override;
    Function ReadImageProperties: Boolean; override;
  public

    Constructor Create(AOwner: TPersistent; AWidth, AHeight: Integer); override;
    Destructor Destroy; override;

    Class Function Capabilities: TBZDataFileCapabilities; override;

    Function getImagePropertiesAsString: String; override;

    property SavingOptions : TBZBitmapTGASavingOptions read FSavingOptions;
  end;

implementation

Uses
  BZUtils
  {$IFDEF DEBUG}
  , Dialogs, BZLogger, BZTypesHelpers
  {$ENDIF};

{ TBZBitmapTGASavingOptions }

Constructor TBZBitmapTGASavingOptions.Create;
begin
  inherited Create;
  FCompressed := False;
  FBitsPerPixel := pf32bits;
end;


{%region%=====[ TBZBitmapTGAImage ]============================================}

Constructor TBZBitmapTGAImage.Create(AOwner: TPersistent; AWidth, AHeight: Integer);
Begin
  Inherited Create(aOwner, AWidth, AHeight);
  With DataFormatDesc Do
  Begin
    Name := 'TARGA';
    Desc := 'Targa TrueVision image';
    FileMask := '*.tga;*.icb;*.vda;*.vst';
    Version := '1.0';
    Encoding := etNone;
  End;

  SupportedColorFormat := SupportedColorFormat + [cfRGB_565, cfBGR_565, cfBGR, cfBGRA];

  FlipDataV:=False;
  FlipDataH:=False;

  FSavingOptions := TBZBitmapTGASavingOptions.Create;
end;

Destructor TBZBitmapTGAImage.Destroy;
Begin
  SupportedColorFormat := [];
  FreeAndNil(FSavingOptions);
  Inherited Destroy;
End;

Class Function TBZBitmapTGAImage.Capabilities: TBZDataFileCapabilities;
Begin
  Result := [dfcRead, dfcWrite];
End;

Function TBZBitmapTGAImage.getImagePropertiesAsString: String;
Var
  S: String;
Begin
  S := '';
  With DataFormatDesc Do
  Begin
    S := 'Format de fichier : ' + Name + #13 + #10; //+'('+Desc+')'+#13+#10;
  End;

  Result:=S;
end;

Function TBZBitmapTGAImage.ReadImageProperties: Boolean;
Var
  bmpWidth, bmpHeight : Integer;
Begin
  Result:=True;
  // On sauvegarde en local les dimensions de l'image
  bmpWidth := FTGAFileHeader.Width;
  bmpHeight := FTGAFileHeader.Height;

  //GlobalLogger.LogStatus('Width : '+Inttostr(bmpWidth)+ 'Height : '+Inttostr(bmpHeight));
  //GlobalLogger.LogStatus('PixelSize : '+Inttostr(FTGAFileHeader.PixelSize));
  With ImageDescription Do
  Begin
    // On initialise la descritption du "ImageDescription"
    InitDefault(bmpWidth, bmpHeight, FTGAFileHeader.PixelSize);
    // On Indique qu'il n'y a pas de padding de fin de ligne
    RowStrideType:=bleNoBoundary;
    // De Haut en Bas ?
    FlipDataV:=(FTGAFileHeader.Descriptor and TARGA_DESCRIPTOR_UPPER_ORIGIN) <> 0;
    // De Droite à Gauche ?
    FlipDataH:=(FTGAFileHeader.Descriptor and TARGA_DESCRIPTOR_RIGHT_ORIGIN) <> 0;
    //GlobalLogger.LogStatus('FlipV : '+FlipDataV.ToString());
    //GlobalLogger.LogStatus('FlipH : '+FlipDataH.ToString());
    if FlipDataV then
      LineOrder := bloTopToBottom
    Else
      LineOrder := bloBottomToTop ;
    // Transparence ?
    AlphaBits := FTGAFileHeader.Descriptor and $0F;
    if alphaBits = 8 then HasAlpha:=True;
    //GlobalLogger.LogStatus('AlphaBits : '+Inttostr(AlphaBits));
  end;

  if FTGAFileHeader.ImageType in [TARGA_BW_RLE_IMAGE, TARGA_INDEXED_RLE_IMAGE, TARGA_TRUECOLOR_RLE_IMAGE] then
    DataFormatDesc.Encoding := etRLE //RLE
  else
    DataFormatDesc.Encoding := etNone; //Aucune

  Result := True;
End;

Function TBZBitmapTGAImage.CheckFormat(): Boolean;
Begin
  Result :=  False;
  // Chargement de l'en-tête
  Memory.Read(FTGAFileHeader, SizeOf(TBZTGAFileHeader));
  // Vérification de quelques paramètres
  Result := (FTGAFileHeader.ImageType in [TARGA_EMPTY_IMAGE, TARGA_INDEXED_IMAGE, TARGA_TRUECOLOR_IMAGE, TARGA_BW_IMAGE,
        TARGA_INDEXED_RLE_IMAGE, TARGA_TRUECOLOR_RLE_IMAGE, TARGA_BW_RLE_IMAGE]) and
        (FTGAFileHeader.ColorMapType in [TARGA_NO_COLORMAP, TARGA_COLORMAP]) and
        (FTGAFileHeader.ColorMapEntrySize in [0, 15, 16, 24, 32]) and
        (FTGAFileHeader.PixelSize in [8, 15, 16, 24, 32]);
  if Result then
  begin
     // Le fichier est valide ont lit les propriétés
     Result := ReadImageProperties;
     //GlobalLogger.LogStatus('TGA is Valid :'+Result.ToString());
  end;
End;



Procedure TBZBitmapTGAImage.LoadFromMemory();
Var
  LineSize, I  : Integer;
  LineBuffer, idx : PByte;
  X,Y, YY : Integer;
  aCol24 : TBZColor24;
  aCol24BGR : TBZColorBGR_24;
  aCol32 : TBZColor;
  aCol32BGRA : TBZColor;
  //aCol16 : TBZColorBGR16_565;
  DstLine : PBZColor;
  DstColor : TBZColor;
  SrcPtrBGR24 : PBZColorBGR_24;
  SrcPtrBGRA32: PBZColor;
  SrcPtr16 : PWord;
  cw:Word;
  r,g,b : byte;
  rleOpCode : Byte;
  rleCount : Byte;
  rleIdx : Byte;
  Delta:Single;
  IgnoreAlpha, IsOpaque : Boolean;

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
  IgnoreAlpha := True;
  IsOpaque := True;
  // On Saute l'ID
  //Memory.SkipNextByte(FTGAFileHeader.IDLen);
  if FTGAFileHeader.IDLen>0 then Memory.Seek(FTGAFileHeader.IDLen,soCurrent);
  //GlobalLogger.LogStatus('IDLen : '+Inttostr(FTGAFileHeader.IDLen));
  if (FTGAFileHeader.ColorMapType = TARGA_COLORMAP) then
  begin
     if FTGAFileHeader.ImageType in [TARGA_BW_IMAGE, TARGA_BW_RLE_IMAGE] then
     begin
       //GlobalLogger.LogStatus('Create Gray Palette HAS COLOR MAP');
       CreateGrayRawPalette;
     end
     else
     begin
       //GlobalLogger.LogStatus('Chargement de la palette : '+Inttostr(FTGAFileHeader.ColorMapCount));
       //GlobalLogger.LogStatus('Taille d''un élément de la palette : '+Inttostr(FTGAFileHeader.ColorMapEntrySize));
       ImageDescription.PaletteCount:=FTGAFileHeader.ColorMapCount;
       Case FTGAFileHeader.ColorMapEntrySize Of
         16, 24:
         begin
           For I:=FTGAFileHeader.ColorMapOrigin to pred(FTGAFileHeader.ColorMapOrigin+FTGAFileHeader.ColorMapCount)  do
           begin
             if FTGAFileHeader.ColorMapEntrySize = 24 then
             begin
               Memory.Read(aCol24,3);
               With ImageDescription.PaletteEntries^[I-FTGAFileHeader.ColorMapOrigin] do
               begin
                 {$IFDEF WINDOWS}
                   Red:=aCol24.Blue;
                   Green:=aCol24.Green;
                   Blue:=aCol24.Red;
                 {$ELSE}
                   Red:=aCol24.Red;
                   Green:=aCol24.Green;
                   Blue:=aCol24.Blue;
                 {$ENDIF}
                 Alpha:=255;
               end;
             end
             else if FTGAFileHeader.ColorMapEntrySize = 16 then
             begin
               cw:=0;
               Memory.Read(cw,2);
               With ImageDescription.PaletteEntries^[I-FTGAFileHeader.ColorMapOrigin] do
               begin
                r:=((cw shr 10) and $1F) shl 3;
                g:=((cw shr 5) and $1F) shl 3;
                b:=((cw shr 0) and $1F) shl 3;

                // Format de la palette = BGR
                Red:=r;
                Green := g;
                Blue := b;
                Alpha := 255;

               end;
             end;
           end;
         end;
         32:
         begin
           For I:=FTGAFileHeader.ColorMapOrigin to pred(FTGAFileHeader.ColorMapOrigin+FTGAFileHeader.ColorMapCount)  do
           begin
             aCol32.AsInteger:=$000000FF;
             Memory.Read(aCol32,4);
             With ImageDescription.PaletteEntries^[I-FTGAFileHeader.ColorMapOrigin] do
             begin
               Red:=aCol32.Red;
               Green:=aCol32.Green;
               Blue:=aCol32.Blue;
               if AlphaBits = 8 then
               begin
                  if (aCol32.Alpha and $80) <> 0 then
                    Alpha:=0
                  else
                    Alpha:=255;
               end else Alpha:=aCol32.Alpha;
             end
           end;
         end;
       end;
     end;
  end
  else
  begin
    if FTGAFileHeader.ImageType in [TARGA_BW_IMAGE, TARGA_BW_RLE_IMAGE] then
    begin
       //GlobalLogger.LogStatus('Create Gray Palette NO COLOR MAP');
       CreateGrayRawPalette;
     end;
  end;
  LineSize := ImageDescription.BytesPerLine;
  //imageDataOffset := int64(18 + (FTGAFileHeader.colormapEntrySize div 8) * int64(FTGAFileHeader.ColorMapCount));

  SetSize(ImageDescription.Width, ImageDescription.Height);
  InitProgress(Width,Height);
  StartProgressSection(0, ''); // On debute une nouvelle section //Globale

  Delta := 100 / Height;
//  StartProgressSection(100 ,rsLoadingBitmapData);

  Case FTGAFileHeader.ImageType of
    TARGA_EMPTY_IMAGE: ; // rien à faire
    TARGA_BW_IMAGE :  // Format Noir et Blanc / Niveau de gris
    begin
      //GlobalLogger.LogStatus('ImageType : TARGA_BW_IMAGE ');

      LineBuffer := nil;
      GetMem(LineBuffer, LineSize);
      Try
        Y := 0;
        Repeat
          Memory.Read(LineBuffer^, LineSize);
          { NOTE : Le format TARGA peux inverser l'image horizontalement et ou verticalment à
            l'enregistrement. Par défaut nous remettons l'image à l'endroit }
          If FlipDataV Then YY := Y Else YY := MaxHeight - Y;
          if FlipDataH then DstLine := GetPixelPtr(MaxWidth,YY)
          else DstLine := GetScanLine(YY);
          Idx := PByte(LineBuffer);
          X:= Width;
          While (X>0) Do
          Begin
            Case ImageDescription.BitCount of
              8 :  // Format 8bits chaque Byte = Index dans la palette
              begin
                {$IFDEF WINDOWS}
                   DstLine^.AsInteger:= ImageDescription.PaletteEntries^[Idx^].AsInteger;
                {$ELSE}
                   DstLine^.AsInteger:= ImageDescription.PaletteEntries^[Idx^].FastSwapRedBlue;
                {$ENDIF}
              end;
              16 :
              {Format "GrayScale" niveau de gris avec support Alpha
               Byte 1 = Niveau de gris ou Index dans la palette
               Byte 2 = Alpha }
              begin
                With DstColor do
                begin
                  Blue:=ImageDescription.PaletteEntries^[Idx^].blue;
                  Green:=ImageDescription.PaletteEntries^[Idx^].green;
                  Red:=ImageDescription.PaletteEntries^[Idx^].red;
                  Alpha:=255;
                  if AlphaBits = 8 then
                  begin
                    inc(Idx);
                    // On le dernier bit
                    if (idx^ and $80) = 0 then Alpha:=0;
                  end else inc(Idx);
                end;
                DstLine^ := DstColor;
              end;
            end;
            Inc(Idx);
            if FlipDataH then Dec(DstLine) else Inc(DstLine);
            Dec(X);
          end;
          Inc(Y);
          AdvanceProgress(Delta,0,1,False);
        Until (Y > MaxHeight);
      Finally
        FreeMem(LineBuffer);
        LineBuffer := nil;
      End;
    end;
    TARGA_INDEXED_IMAGE :  // Format indexé.
    Begin
      //GlobalLogger.LogStatus('ImageType : TARGA_INDEXED_IMAGE ');
      LineBuffer := nil;
      GetMem(LineBuffer, LineSize);
      Try
        Y := 0;
        Repeat
          Memory.Read(LineBuffer^, LineSize);
          If FlipDataV Then YY := Y Else YY := MaxHeight - Y;
          if FlipDataH then DstLine := GetPixelPtr(MaxWidth,YY)
          else DstLine := GetScanLine(YY);
          Idx := PByte(LineBuffer);
          X:= Width;
          While (X>0) Do
          Begin
            {$IFDEF UNIX}
               DstLine^.AsInteger:= ImageDescription.PaletteEntries^[Idx^].FastSwapRedBlue;  // A VERIFIER
            {$ELSE}
               DstLine^:= ImageDescription.PaletteEntries^[Idx^];
           {$ENDIF}
            if FlipDataH then Dec(DstLine) else Inc(DstLine);
            Inc(Idx);
            Dec(X);
          end;
          Inc(Y);
          AdvanceProgress(Delta,0,1,False);
        Until (Y > MaxHeight);
      Finally
        FreeMem(LineBuffer);
        LineBuffer := nil;
      End;
    end;
    TARGA_TRUECOLOR_IMAGE:
    Begin
      //GlobalLogger.LogStatus('ImageType : TARGA_TRUECOLOR_IMAGE ');
      //GlobalLogger.LogStatus('ImageDescription.BitCount = '+Inttostr(ImageDescription.BitCount));
      Case ImageDescription.BitCount of
        16:
        Begin
          LineBuffer := nil;
          GetMem(LineBuffer, LineSize);
          Try
            Y := 0;
            Repeat
              Memory.Read(LineBuffer^, LineSize);
              If FlipDataV Then YY := Y Else YY := MaxHeight - Y;
              if FlipDataH then DstLine := GetPixelPtr(MaxWidth,YY)
              else DstLine := GetScanLine(YY);
              SrcPtr16 := PWord(LineBuffer);
              X:= Width;
              While (X>0) Do
              Begin
                cw:= SrcPtr16^;
              { pour les formats 15 et 16 bit, chaque pixel est stocké sur 5 bits par couleurs.
                Si la profondeur de couleur est de 16 bits, le bit le plus élevé est réservé à la transparence.
                On Convertie les valeurs entre 0 et 255
                Seulement, l'image sera plus sombre, et il y aura des couleurs "hors champ"
                Exemple le blanc $FFFF = F8,FC,F8 (R,G,B)
                A la place on multiplie et on divise, pour redimensionner corectement les valeurs
                       exemple Pour le format RGB/BGR 565
                       r := r * 255 div 31;
                       g := g * 255 div 63;
                       b := b * 255 div 31;
              }

                // Vieux code TP7, nickel rien à dire même apres plus de 20 ans.
                // Format 16bits XBGR 1555
                r:=((cw shr 10) and $1F) shl 3;
                g:=((cw shr 5) and $1F) shl 3;
                b:=((cw shr 0) and $1F) shl 3;
                //a:=cw shl $1F
                DstColor.Create(R,G,B,255);
                DstLine^ := DstColor;
                Inc(SrcPtr16);
                if FlipDataH then Dec(DstLine) else Inc(DstLine);
                Dec(X);
              end;
              Inc(Y);
              AdvanceProgress(Delta,0,1,False);
            Until (Y > MaxHeight);
          Finally
            FreeMem(LineBuffer);
            LineBuffer := nil;
          End;
        end;
        24:
        Begin
          LineBuffer := nil;
          GetMem(LineBuffer, LineSize);
          Try
            Y := 0;
            Repeat
              Memory.Read(LineBuffer^, LineSize);
              If FlipDataV Then YY := Y Else YY := MaxHeight - Y;
              if FlipDataH then DstLine := GetPixelPtr(MaxWidth,YY)
              else DstLine := GetScanLine(YY);
              SrcPtrBGR24 := PBZColorBGR_24(LineBuffer);
              X:= Width;
              While (X>0) Do
              Begin
                //aCol24BGR := SrcPtrRGB24^.ToBGR;
                DstColor.Create(SrcPtrBGR24^,255);
                //{$IFDEF LINUX}
                //  DstColor.AsInteger := DstColor.FastSwapRedBlue;
                //{$ENDIF}

                //DstColor.Red :=  aCol24BGR.Red;
                //DstColor.Green := aCol24BGR.Green;
                //DstColor.Blue := aCol24BGR.Blue;
                //DstColor.Alpha := 255;
                DstLine^ := DstColor;
                Inc(SrcPtrBGR24);
                if FlipDataH then Dec(DstLine) else Inc(DstLine);
                Dec(X);
              end;
              Inc(Y);
              AdvanceProgress(Delta,0,1,False);
            Until (Y > MaxHeight);
          Finally
            FreeMem(LineBuffer);
            LineBuffer := nil;
          End;
        End;
        32:
        begin
          LineBuffer := nil;
          GetMem(LineBuffer, LineSize);
          Try
            Y := 0;
            Repeat
             // GlobalLogger.LogStatus('Read line =  '+y.ToString);
              Memory.Read(LineBuffer^, LineSize);
              If FlipDataV Then YY := Y Else YY := MaxHeight - Y;
              if FlipDataH then DstLine := GetPixelPtr(MaxWidth,YY)
              else DstLine := GetScanLine(YY);
              SrcPtrBGRA32 := PBZColor(LineBuffer);
              X:= Width;
              While (X>0) Do
              Begin
                //aCol32BGRA := SrcPtrBGRA32^;
                DstColor := SrcPtrBGRA32^;
                {$IFDEF LINUX}
                DstColor.AsInteger :=  SrcPtrBGRA32^.FastSwapRedBlue;
                {$ENDIF}
                IgnoreAlpha := IgnoreAlpha and (DstColor.alpha = 0);
                IsOpaque := IsOpaque and (DstColor.alpha = 255);
                //DstColor.Red :=  aCol32BGRA.Blue;
                //DstColor.Green := aCol32BGRA.Green;
                //DstColor.Blue := aCol32BGRA.Red;
                //DstColor.Alpha := aCol32BGRA.Alpha;
                //GlobalLogger.LogStatus('Read Pixel =  '+DstColor.ToString);
                DstLine^ := DstColor;
                Inc(SrcPtrBGRA32);

                if FlipDataH then Dec(DstLine) else Inc(DstLine);
                Dec(X);
              end;
              Inc(Y);
              AdvanceProgress(Delta,0,1,False);
            Until (Y > MaxHeight);
          Finally
            FreeMem(LineBuffer);
            LineBuffer := nil;
          End;
        end;
      end;
    end;
    TARGA_BW_RLE_IMAGE,
    TARGA_INDEXED_RLE_IMAGE,
    TARGA_TRUECOLOR_RLE_IMAGE:
    Begin
      //GlobalLogger.LogStatus('ImageType : TARGA_COMPRESSED_IMAGE - BitCount =  '+ImageDescription.BitCount.ToString);
      Delta := 100 / MaxSize;
      rleOpCode:=0;
      rleCount:=0;
      I:=0;
      DstLine := GetScanLine(0);
      DstColor.Alpha := 255;
      repeat
        rleOpCode:=Memory.ReadByte;
        rleCount:=(rleOpCode And $7F) + 1;
        //GlobalLogger.LogStatus('RLE COUNT =  '+rleCount.ToString);
        //Inc(I,rleCount);
        if rleOpCode and $80 <>0 then //Compression
        begin
          //GlobalLogger.LogStatus('Data Compression found at : '+i.ToString);
          Case ImageDescription.BitCount of
            8 :
            begin
              rleIdx := Memory.ReadByte;
  //            DstLine^.AsInteger

              //{$IFDEF WINDOWS}
              //   DstColor.AsInteger := ImageDescription.PaletteEntries^[rleIdx].FastSwapRedBlue;
              //{$ELSE}
                  DstColor := ImageDescription.PaletteEntries^[rleIdx];
              //{$ENDIF}

              //DstColor.AsInteger := Dst
            end;
            16:
            begin
              cw := Memory.ReadWord;
              r:=((cw shr 10) and $1F) shl 3;
              g:=((cw shr 5) and $1F) shl 3;
              b:=((cw shr 0) and $1F) shl 3;
              //a:=cw shl $1F
              DstColor.Create(R,G,B,255);
            end;
            24:
            begin
              //GlobalLogger.LogStatus('READ 24 bit BGR =  '+rleCount.ToString);
              Memory.Read(aCol24BGR,3);
              DstColor.Create(aCol24BGR);
              //GlobalLogger.LogStatus('READ 24 bit BGR - Color =  '+DstColor.ToString);
              {$IFDEF LINUX}
                DstColor.AsInteger := DstColor.FastSwapRedBlue;
              {$ENDIF}
            end;
            32:
            begin
              Memory.Read(aCol32BGRA,4);
              //GlobalLogger.LogStatus('-----> Read Compressed Color : '+aCol32BGRA.ToString);
              //DstColor.Red := aCol32BGRA.Red;
              //DstColor.Green := aCol32BGRA.Green;
              //DstColor.Blue := aCol32BGRA.Blue;
              //DstColor.Alpha := aCol32BGRA.Alpha;
              DstColor := aCol32BGRA;
              {$IFDEF LINUX}
                DstColor.AsInteger := DstColor.FastSwapRedBlue;
              {$ENDIF}
              IgnoreAlpha := IgnoreAlpha and (DstColor.alpha = 0);
              IsOpaque := IsOpaque and (DstColor.alpha = 255);
            end;
          end;

          For X:=1 to rleCount do
          begin
            DstLine^ := DstColor;
            Inc(DstLine);
            //AdvanceProgress(Delta,1,0,False);
            Inc(I);
          end;
        end
        else
        begin
          Case ImageDescription.BitCount of
            8 :
            begin
              For X:=1 to rleCount do
              begin
                rleIdx := Memory.ReadByte;
                DstLine^ := ImageDescription.PaletteEntries^[rleIdx];//.FastSwapRedBlue;//.AsInteger;
                Inc(DstLine);
                //AdvanceProgress(Delta,1,0,False);
                inc(I);
              end;
            end;
            16:
            begin
              For X:=1 to rleCount do
              begin
                cw := Memory.ReadWord;
                r:=((cw shr 10) and $1F) shl 3;
                g:=((cw shr 5) and $1F) shl 3;
                b:=((cw shr 0) and $1F) shl 3;
                //a:=cw shl $1F
                DstColor.Create(R,G,B,255);
                DstLine^:=DstColor;
                Inc(DstLine);
                //AdvanceProgress(Delta,1,0,False);
                Inc(I);
              end;
            end;
            24:
            begin
              For X:=1 to rleCount do
              begin
                //GlobalLogger.LogStatus('Not Compressed Data found at : '+i.ToString);
                Memory.Read(aCol24BGR,3);
                DstColor.Create(aCol24BGR);
                {$IFDEF LINUX}
                  DstColor.AsInteger := DstColor.FastSwapRedBlue;
                {$ENDIF}
                DstLine^:=DstColor;
                Inc(DstLine);
                //AdvanceProgress(Delta,1,0,False);
                Inc(I);
              end;
            end;
            32:
            begin
              For X:=1 to rleCount do
              begin
                Memory.Read(aCol32BGRA,4);
                //GlobalLogger.LogStatus('-----> Read Color : '+aCol32BGRA.ToString);
                DstColor := aCol32BGRA;
                {$IFDEF LINUX}
                  DstColor.AsInteger := DstColor.FastSwapRedBlue;
                {$ENDIF}
                //DstColor.Red := aCol32BGRA.Red;
                //DstColor.Green := aCol32BGRA.Green;
                //DstColor.Blue := aCol32BGRA.Blue;
                //DstColor.Alpha := aCol32BGRA.Alpha;
                DstLine^:=DstColor;
                IgnoreAlpha := IgnoreAlpha and (DstColor.alpha = 0);
                IsOpaque := IsOpaque and (DstColor.alpha = 255);
                Inc(DstLine);
                Inc(I);
              end;
            end;
          end;
        end;
        //GlobalLogger.LogStatus('Progress : '+(i*Delta).ToString+'%  --> '+i.ToString+'/'+MaxSize.ToString);
        AdvanceProgress((I*Delta),I,0,False);
        //if (I>MaxSize) then ShowMessage('Reading end');
      until I>MaxSize;
      If not(FlipDataV) Then self.FlipY; // /!\ Pour le rle la parametre  de retournement vertical est inversé
      if FlipDataH Then self.FlipX;
    end;
  end;

  // Dans le cas ou tous les pixels sont transparent
  If IgnoreAlpha Then
  Begin
    //GlobalLogger.LogNotice('IgnoreAlpha');
    MakeOpaque;
    ImageDescription.HasAlpha := False;
  End
  else
  begin
    if IsOpaque then ImageDescription.HasAlpha := False
    else ImageDescription.HasAlpha := True;
  End;

  //ReadExtension;
  //ReadFooter;

  FinishProgressSection(False);
//  FinishProgressSection(True);  // ERROR
End;

procedure TBZBitmapTGAImage.SaveToMemory();
Var
  Delta : Single;

  procedure SaveHeader;
  Var
    Header : TBZTGAFileHeader;
  begin
    With Header do
    begin
      IDLen          := 0;
      ColorMapType   := TARGA_NO_COLORMAP;

      if FSavingOptions.Commpressed then
        ImageType := TARGA_TRUECOLOR_RLE_IMAGE
      else
        ImageType := TARGA_TRUECOLOR_IMAGE;

      ColorMapOrigin    := 0;
      ColorMapCount     := 0;
      ColorMapEntrySize := 0;
      OriginX           := 0;
      OriginY           := 0;
      Width             := Self.Width;
      Height            := Self.Height;

      if FSavingOptions.BitsPerPixel = pf24bits then
        PixelSize      := 24
      else
        PixelSize      := 32;

      Descriptor := $20; // Non-flipped, non-mirrored image.
    end;
    Memory.Write(Header, Sizeof(TBZTGAFileHeader));
  end;

  { Compression RLE D'après GraphicEx }
  procedure SaveData;
  Var
  {$IFDEF LINUX}
    TmpBuffer : PBZColor;
  {$ENDIF}
    BufferBGR : PBZColorBGR_24; //PBZColor24;
    BufferData : PBZColor;
    BufferSize, PixelCount : Int64;
    BPP : Byte;
    RLEBuffer : PByte;
    WriteLength : Integer;

    function CountDiff(P: PBZColor; Count: Integer) : Integer;
    var
      N: Integer;
      Pixel,
      NextPixel: TBZColor;
    begin
      N := 0;
      NextPixel := clrTransparent;
      if Count = 1 then
      begin
        Result := Count
      end
      else
      begin
        Pixel := P^;
        while Count > 1 do
        begin
          Inc(P);
          NextPixel := P^;
          if NextPixel = Pixel then Break;
          Pixel := NextPixel;
          Inc(N);
          Dec(Count);
        end;
        if NextPixel = Pixel then
        begin
          Result := N
        end
        else
        begin
          Result := N + 1;
        end;
      end;
    end;

    function CountSame(P: PBZColor; Count: Integer) : Integer;
    var
      Pixel,
      NextPixel: TBZColor;
    begin
      Result := 1;
      Pixel := P^;
      Dec(Count);
      while Count > 0 do
      begin
        Inc(P);
        NextPixel := P^;
        if NextPixel <> Pixel then Break;
        Inc(Result);
        Dec(Count);
      end;
    end;

    procedure EncodeLineRLE(Source : PBZColor; Dest : PByte; Count : Integer; var BytesStored : Integer);
    var
      DiffCount, // pixel count until two identical
      SameCount: Integer; // number of identical adjacent pixels
      SourcePtr : PBZColor;
      TargetPtr: PByte;
    begin
      SourcePtr := Source;
      TargetPtr := Dest;
      BytesStored := 0;
      while Count > 0 do
      begin
        DiffCount := CountDiff(SourcePtr, Count);
        SameCount := CountSame(SourcePtr, Count);
        if DiffCount > 128 then DiffCount := 128;
        if SameCount > 128  then SameCount := 128;

        if DiffCount > 0 then
        begin
          // create a raw packet
          TargetPtr^ := DiffCount - 1;
          Inc(TargetPtr);
          Dec(Count, DiffCount);
          Inc(BytesStored, (DiffCount * BPP) + 1);
          while DiffCount > 0 do
          begin
            if BPP = 3 then
            begin
              TargetPtr^ := SourcePtr^.Blue;
              Inc(TargetPtr);
              TargetPtr^ := SourcePtr^.Green;
              Inc(TargetPtr);
              TargetPtr^ := SourcePtr^.Red;
              Inc(TargetPtr);
              Inc(SourcePtr);
            end
            else if BPP = 4 then
            begin
              TargetPtr^ := SourcePtr^.Blue;
              Inc(TargetPtr);
              TargetPtr^ := SourcePtr^.Green;
              Inc(TargetPtr);
              TargetPtr^ := SourcePtr^.Red;
              Inc(TargetPtr);
              TargetPtr^ := SourcePtr^.Alpha;
              Inc(TargetPtr);
              Inc(SourcePtr);
            end;
            Dec(DiffCount);
          end;
        end;

        if SameCount > 1 then
        begin
          // create a RLE packet
          TargetPtr^ := (SameCount - 1) or $80;
          Inc(TargetPtr);
          Dec(Count, SameCount);
          Inc(BytesStored, BPP + 1);
          Inc(SourcePtr, (SameCount - 1)); // * 4);
          if BPP = 3 then
          begin
            TargetPtr^ := SourcePtr^.Blue;
            Inc(TargetPtr);
            TargetPtr^ := SourcePtr^.Green;
            Inc(TargetPtr);
            TargetPtr^ := SourcePtr^.Red;
            Inc(TargetPtr);
            Inc(SourcePtr);
          end
          else if BPP = 4 then
          begin
            TargetPtr^ := SourcePtr^.Blue;
            Inc(TargetPtr);
            TargetPtr^ := SourcePtr^.Green;
            Inc(TargetPtr);
            TargetPtr^ := SourcePtr^.Red;
            Inc(TargetPtr);
            TargetPtr^ := SourcePtr^.Alpha;
            Inc(TargetPtr);
            Inc(SourcePtr);
          end;
        end;
      end;
    end;

    procedure WriteDataRLE;
    Var i : Integer;
    begin
      RLEBuffer := nil;
      Try
        memAlloc(RLEBuffer, 2 * Self.LineSizeInBytes);
        for i := 0 to Self.MaxHeight do
        begin
          BufferData := Self.GetScanLine(i);
          EncodeLineRLE(BufferData, RLEBuffer, Self.Width, WriteLength);
          Memory.Write(RLEBuffer^, WriteLength);
          AdvanceProgress(Delta,0,1,False);
        end;
      finally
        if RLEBuffer<>nil then
        begin
          FreeMem(RLEBuffer);
          RLEBuffer := nil;
        end;
      end;
    end;

  begin
    BufferSize := 0;

    if FSavingOptions.Commpressed then
    begin
      if FSavingOptions.BitsPerPixel = pf24bits then
      begin
        BPP := 3;
      end
      else
      begin
        BPP := 4;
      end;
      WriteDataRLE;
    end
    else
    begin
      if FSavingOptions.BitsPerPixel = pf24bits then
      begin
        BufferBGR := Self.ContvertToBGR24(BufferSize);
        Memory.Write(BufferBGR^,BufferSize);
        FreeMem(BufferBGR);
      end
      else
      begin
        BufferData := getSurfaceBuffer;
        {$IFDEF LINUX}
          BufferSize := Width * Height;
          GetMem(TmpBuffer, Self.Size);
          Move(BufferData^, TmpBuffer^, Self.Size);
          SwapRBBuffer(TmpBuffer, BufferSize ;
          Memory.Write(TmpBuffer,Self.Size);
          FreeMem(TmpBuffer);
        {$ELSE}
          Memory.Write(BufferData^,Self.Size);
          AdvanceProgress(100,0,1,False);
        {$ENDIF}
      end;
    end;
  end;

  procedure SaveFooter;
  Var
    Footer : TBZTGAFileFooter;
  begin
    With Footer do
    begin
      ExtAreaOffset := 0;
      DevDirOffset  := 0;
      Signature     := 'TRUEVISION-XFILE'; //['T','R','U','E','V','I','S','I','O','N','-','X','F','I','L','E'];
      ReservedChar  := #0;
      NullByte      := #0;
    end;
    Memory.Write(Footer,Sizeof(TBZTGAFileFooter));
  end;

  procedure SaveExtension;
  begin
   {$message warn not implmented yet}
  end;
begin
  InitProgress(Self.Width,Self.Height);
  StartProgressSection(0, ''); // On debute une nouvelle section globale
  Delta := 100 / Self.Height;
  StartProgressSection(100 ,'Enregistrement de l''image au format TGA');
  SaveHeader;
  SaveData;
  SaveExtension;
  SaveFooter;
  FinishProgressSection(False);
  FinishProgressSection(True);
end;

{%endregion%}

Initialization

  RegisterRasterFormat('tga', 'TARGA True Vision image', TBZBitmapTGAImage);
  RegisterRasterFormat('vst', 'TARGA ATVista image', TBZBitmapTGAImage);
  RegisterRasterFormat('icb', 'TARGA ICB image', TBZBitmapTGAImage);
  RegisterRasterFormat('vda', 'TARGA VDA/D image', TBZBitmapTGAImage);

Finalization
  UnregisterRasterFormat(TBZBitmapTGAImage);


end.

