(*
  @abstract(Prise en charge des Images au format "Portable pixmap")

  Spécifications : @br
  @unorderedList(
    @item(Méthode de compression    : Aucune)
    @item(Nombre de couleurs        : 1, 8 bits format RGB, 24 bits, RGBA 32 bits)
    @item(Supporte plusieurs images : Non, une seule image dans un même fichier)
    @item(Format des nombres        : texte et binaire)
    @item(Auteur                    : ???)
    @item(Extensions                : *.pbm, *.pgm, *.pnm, *.ppm, *.pam, *.pfm, *.rpbm, *.rpgm, *.rppm, *.ppma)
  )

  -------------------------------------------------------------------------------------------------------------

  @created(2017-04-30)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(30/04/2017 : Creation  )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) : @br
    Informations sur le format PPM : @br
    @unorderedList(
      @item(https://en.wikipedia.org/wiki/Netpbm_format)
      @item(https://fr.wikipedia.org/wiki/Portable_pixmap)
      @item(http://www.fileformat.info/format/pbm/egff.htm)
      @item(http://paulbourke.net/dataformats/ppm/)
      @item(http://www.jchr.be/python/pnm-pam.htm)
      @item(https://en.wikipedia.org/wiki/Netpbm#PAM_graphics_format)
      @item(https://www.enseignement.polytechnique.fr/informatique/profs/Philippe.Chassignet/PGM/index.html)
      @item(https://wiki.multimedia.cx/index.php/Netpbm)
      @item(http://www.pauldebevec.com/Research/HDR/PFM/)
    )

    Autres informations utiles : @br
    @unorderedList(
      @item(http://www.fileformat.info/format/xbm/egff.htm)
      @item(https://datatypes.net)
    )

   Fichiers test : @br
   @unorderedList(
     @item(http://www.cs.cornell.edu/courses/cs664/2003fa/images/)
     @item(http://userpages.umbc.edu/~rostamia/2003-09-math625/images.html)
     @item(http://www-igm.univ-mlv.fr/~incerti/IMAGES/PPM.htm)
   )

  -------------------------------------------------------------------------------------------------------------

  @bold(Dépendances) : BZClasses, BZMath, BZColors, BZGraphic, BZBitmap, BZImageFileIO, BZImageStrConsts, BZUtils

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)
  @unorderedList(
    @item(Tous les liens ci-dessus)
    @item(FPC & Lazarus)
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Licence) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
Unit BZImageFilePPM;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\bzscene_options.inc}
//==============================================================================

//------------------------------------------------------------------------------
//----------------------------[ TODO LIST ]-------------------------------------

{ TODO 0  -oBZBitmap -cSupport_Images_PPM : Finir Prise en charge format PAM}
{ TODO 0  -oBZBitmap -cSupport_Images_PPM : Prise en charge format RGB 48bits (pnm/ppm binaire V6)}
{ TODO 2  -oBZBitmap -cSupport_Images_PPM : Ecriture Formats PixMap}

//------------------------------------------------------------------------------

Interface

Uses
  Classes, SysUtils,
  BZClasses, BZMath, BZColors, BZGraphic, BZBitmap, BZImageFileIO;

Type
  { Type des données pour le format PAM}
  TTupleType = (ttInvalid, ttBlackAndWhite, ttGrayScale, ttRGB, ttBlackAndWhiteAlpha,
    ttGrayScaleAlpha, ttRGBAlpha, ttGrayScaleFP, ttRGBFP);

Type
  { TBZBitmapPPMImage : Classe de lecture et d"ecriture des images au format PixMap }
  TBZBitmapPPMImage = Class(TBZCustomImageFileIO)
  Private

  Protected
    isBinary:  Boolean;
    HasPAMHeader: Boolean;
    IsBigEndian: Boolean;
    ImageType: Char;
    BmpWidth, BmpHeight: Integer;
    Depth:     Longint;
    MaxVal:    Longint;
    ABitCount:  Byte;
    TupleType: TTupleType;


    Function ReadTokenString: String;
    Function ReadInteger: Integer;

    Procedure LoadFromMemory(); Override;
    Function CheckFormat(): Boolean; Override;
    Function ReadImageProperties: Boolean; Override;
  Public

    Constructor Create(AOwner: TPersistent; AWidth, AHeight: Integer); Override;
    Destructor Destroy; Override;

    Class Function Capabilities: TBZDataFileCapabilities; Override;

    Function getImagePropertiesAsString: String; Override;
  End;

Implementation

Uses
  BZImageStrConsts,
  BZUtils
  {$IFDEF DEBUG}
  , Dialogs, BZLogger
  {$ENDIF};

Const
  WhiteSpaces = [#9, #10, #13, #32]; // Les tabulations, fins de ligne et espace sont considérés comme des séparateurs

  // Constatnte pour le format PAM
  TagPAMWidth     = 'WIDTH';
  TagPAMHeight    = 'HEIGHT';
  TagPAMDepth     = 'DEPTH';
  TagPAMMaxVal    = 'MAXVAL';
  TagPAMTupleType = 'TUPLTYPE';
  TagPAMEndHdr    = 'ENDHDR';
  TupleTypeNames: Array[TTupleType] Of String = (
    'INVALID', 'BLACKANDWHITE', 'GRAYSCALE', 'RGB',
    'BLACKANDWHITE_ALPHA', 'GRAYSCALE_ALPHA', 'RGB_ALPHA', 'GRAYSCALEFP',
    'RGBFP');

{%region%=====[ TBZBitmapXXXImage ]============================================}

Constructor TBZBitmapPPMImage.Create(AOwner: TPersistent; AWidth, AHeight: Integer);
Begin
  Inherited Create(aOwner, AWidth, AHeight);
  With DataFormatDesc Do
  Begin
    Name := 'Portable PixMap';
    Desc := 'Netpbm format';
    FileMask := '*.pbm; *.pgm; *.pnm; *.ppm; *.pam; *.pfm';
    Version := '0.0';
    Encoding := etNone;
  End;
End;

Destructor TBZBitmapPPMImage.Destroy;
Begin
  SupportedColorFormat := [];
  Inherited Destroy;
End;

Class Function TBZBitmapPPMImage.Capabilities: TBZDataFileCapabilities;
Begin
  Result := [dfcRead]; //[dfcRead, dfcWrite]
End;

Function TBZBitmapPPMImage.getImagePropertiesAsString: String;
Var
  S: String;
Begin
  S := 'na';
  Result := S;
End;

Function TBZBitmapPPMImage.ReadTokenString: String;
Var
  S: String;
  C: Char;
Begin
  S := '';
  With Memory Do
  Begin
    // On saute les "espaces" et les commentaires
    Repeat
      C := ReadChar;
      If (C = '#') Then // Commentaires
      Begin
        Repeat
          C := ReadChar;
        Until C = #10;  // Fin de ligne
      End;
    Until Not (C In WhiteSpaces);
    // On se replace
    GotoPreviousByte;
    // On lit la chaine
    Repeat
      C := ReadChar;
      If Not (C In WhiteSpaces) Then
      Begin
        SetLength(S, Length(S) + 1);
        S[Length(S)] := C;
      End
      Else
        break;
    Until (C In WhiteSpaces);
  End;
  Result := S;
End;

Function TBZBitmapPPMImage.ReadInteger: Integer;
Var
  S: String;
Begin
  Try
    S := ReadTokenString;
    Result := StrToInt(S);
  Except
    On E: EConvertError Do
    Begin
      raise Exception.Create('Lecture d''un nombre invalide : ' + S);
      Result := 0;
      Exit;
    End;
  End;
End;

Function TBZBitmapPPMImage.ReadImageProperties: Boolean;
Var
  pamTag:    String;
  OldSeparator: Char;
  Scale:     Single;
  LineBreak: String;

  Procedure FindLineBreak;
  Var
    C:      Char;
    OldPos: Int64;
  Begin
    OldPos := Memory.position;
    LineBreak := #10;
    Repeat
      C := Memory.ReadChar;
      If C = #13 Then
        LineBreak := #13#10;
    Until C = #10;
    Memory.Seek(OldPos, soBeginning);
  End;

Begin
  Result := True;
  // Pour determiner plus tard si le fichier est encodé en Big ou Little Endian
  FindLineBreak;

  If (ImageType In ['1'..'6']) Then  //PBM, PGM, PNM, PPM
  Begin
    // Lecture des dimensions
    BmpWidth := ReadInteger;
    BmpHeight := ReadInteger;

    If (ImageType In ['1', '4']) Then
      MaxVal := 1  //PBM  (1=Ascii, 4=Binaire)
    Else
      MaxVal := ReadInteger;   // PGM, PNM, PPM

    If (bmpWidth <= 0) Or (bmpHeight <= 0) Or (MaxVal <= 0) Then
    Begin
      RaiseInvalidImageFile(Format(rsBadImageSize,[bmpWidth,bmpHeight]));
      Result := False;
      exit;
    End;
    // On definit quelques infos en fonction de
    Case ImageType Of
      '1', '4':
      Begin
        ABitCount := 1; // Noir et Blanc
        Depth := 1;
      End;
      '2':
      Begin
        ABitCount := 8; // Niveaux de gris Ascii
        Depth := 1;
      End;
      '5':        // Niveaux de gris binaire
      Begin
        If (MaxVal > 255) Then  // GrayAlpha ou Gray16
        Begin
          ABitCount := 16;
          depth := 2;
        End
        Else
        Begin
          ABitCount := 8;
          Depth := 1;
        End;

      End;
      '3', '6':
      Begin
        ABitCount := 24;// Couleurs Ascii/binaire
        Depth := 3;
      End;
    End;

  End
  Else If (ImageType = '7') Then  // PAM
  Begin
    // Lecture de l'en-tête du format PAM
    pamTag := ReadTokenString;
    If (pamTag <> TagPAMWidth) Then
    Begin
      RaiseInvalidImageFile('Portable Arbitrary Map : En-tête "WIDTH" invalide');
      Result := False;
      exit;
    End
    Else
      bmpWidth := ReadInteger;

    pamTag := ReadTokenString;
    If (pamTag <> TagPAMHeight) Then
    Begin
      RaiseInvalidImageFile('Portable Arbitrary Map : En-tête "HEIGHT" invalide');
      Result := False;
      exit;
    End
    Else
      bmpHeight := ReadInteger;

    pamTag := ReadTokenString;
    If (pamTag <> TagPAMDepth) Then
    Begin
      RaiseInvalidImageFile('Portable Arbitrary Map : En-tête "DEPTH" invalide');
      Result := False;
      exit;
    End
    Else
      Depth := ReadInteger;

    pamTag := ReadTokenString;
    If (pamTag <> TagPAMMaxVal) Then
    Begin
      RaiseInvalidImageFile('Portable Arbitrary Map : En-tête "MAXVAL" invalide');
      Result := False;
      exit;
    End
    Else
      MaxVal := ReadInteger;

    pamTag := ReadTokenString;
    If (pamTag <> TagPAMTupleType) Then
    Begin
      RaiseInvalidImageFile('Portable Arbitrary Map : En-tête "TUPLETYPE" invalide');
      Result := False;
      exit;
    End
    Else
      bmpHeight := ReadInteger;

    pamTag := ReadTokenString;
    If (pamTag <> TagPAMEndHdr) Then
    Begin
      RaiseInvalidImageFile('Portable Arbitrary Map : En-tête "ENDHDR" invalide');
      Result := False;
      exit;
    End;
    If (bmpWidth <= 0) Or (bmpHeight <= 0) Or (MaxVal <= 0) Then
    Begin
      RaiseInvalidImageFile(Format(rsBadImageSize,[bmpWidth,bmpHeight]));
      Result := False;
      exit;
    End;
    // Fin de la lecture de l'en-tête PAM

    // On definit quelques infos
    Case TupleType Of
      ttBlackAndWhite:
      Begin
        Depth := 1;
        ABitCount := 1;
      End;
      ttGrayScale:
      Begin
        Depth := 1;
        ABitCount := 8;
      End;
      ttBlackAndWhiteAlpha, ttGrayScaleAlpha:
      Begin
        Depth := 2;
        ABitCount := 16;
      End;
      ttRGB:
      Begin
        Depth := 3;
        ABitCount := 24;
      End;
      ttRGBAlpha:
      Begin
        Depth := 4;
        ABitCount := 32;
      End;
    End;
  End
  Else If (ImageType In ['F', 'f']) Then //PFM
  Begin
    // Lecture des dimensions
    BmpWidth := ReadInteger;
    BmpHeight := ReadInteger;
    If (bmpWidth <= 0) Or (bmpHeight <= 0) Then
    Begin
      RaiseInvalidImageFile(Format(rsBadImageSize,[bmpWidth,bmpHeight]));
      Result := False;
      //exit;
    End;
    OldSeparator := DefaultFormatSettings.DecimalSeparator;
    DefaultFormatSettings.DecimalSeparator := '.';
    Scale := StrToFloatDef(ReadTokenString, 0);
    DefaultFormatSettings.DecimalSeparator := OldSeparator;
    IsBigEndian := Scale > 0.0;
    If ImageType = 'F' Then
    Begin
      Depth := 3;
      ABitCount := 96;
    End
    Else
    Begin
      Depth := 1;
      ABitCount := 32;
    End;
  End;

  // On complète les informations
  // Le format PAM et PFM sont toujours encodés en binaire
  IsBinary := (ImageType In ['4', '5', '6', '7', 'F', 'f']);

  // Litlle ou Big Endian ?
  If isBinary And Not (ImageType In ['F', 'f']) Then
  Begin
    { Imite le comportement de Photoshop et autres éditeurs :
      Si la fin des lignes est de la forme CR / LF alors les données sont codées en
      Little Endian. Si non, pour  Unix par exemple, fin des lignes =LF les données
      sont codées en BigEndian.
    }
    IsBigEndian := LineBreak = #10;
  End;

  Result := Result And (ABitCount In [1, 8, 16, 24, 32, 48, 96]);

  // if not(result) then showmessage('Error');
  // On met à jour les informations
  If Result Then
  Begin
    With ImageDescription Do
    Begin
      // On initialise la descritption du "ImageDescription"
      // //GlobalLogger.LogNotice('Description.InitDefault(bmpWidth, bmpHeight, BitCount)'+InttoStr(bmpWidth)+'x'+InttoStr(bmpHeight)+'x'+InttoStr(BitCount));
      InitDefault(bmpWidth, bmpHeight, ABitCount);
    End;
    With DataFormatDesc Do
    Begin
      If (ImageType In ['F', 'f']) Then
        Version := '8'
      Else
        Version := ImageType;
      Encoding := etNone;
    End;
  End;
End;

Function TBZBitmapPPMImage.CheckFormat(): Boolean;
Var
  C: Char;
Begin
  Result := False;
  C := Memory.ReadChar;
  If (C = 'P') Then
  Begin
    ImageType := Memory.ReadChar;
    C := Memory.ReadChar;
    If (((ImageType In ['1'..'7']) Or (ImageType In ['F', 'f'])) And (C In WhiteSpaces)) Then Result := ReadImageProperties;
  End;
End;

Procedure TBZBitmapPPMImage.LoadFromMemory();
Const
  icWhiteSpaces = '#9#10#13#32';
Var
  X, Y: Integer;
  db:     Byte;
  dw:     Word;
  C, Idx, R, G, B: Integer;
  sR, sG, sB: Single;

  DstPtr: PBZColor;
  DstColor: TBZColor;
  ColorRgb24: TBZColor24;
  TempBuffer: PByte;
  SrcPtr: PByte;
  SrcPtrF: pSingle;
  bSize:  Int64;
  isFPGray: Boolean;
  Delta : Single;
  // FPColorBGR : TBZFPColorRGB_96;
Begin
  // On initialise notre bitmap
  SetSize(bmpWidth, bmpHeight);
  InitProgress(bmpWidth, bmpHeight);
  StartProgressSection(0, ''); // On debute une nouvelle section globale

  Delta := 100 / Height;
  StartProgressSection(100, rsLoadingBitmapData);
  // Niveaux de gris on utilise la palette
  If (ImageType In ['2', '5']) Then CreateGrayRawPalette(False);

  DstPtr := nil;
  TempBuffer := nil;

  // On lit les données
  If Not (IsBinary) Then // Formats Ascii
  Begin
    //GlobalLogger.LogNotice('Read  Ascii Pixmap');
    For Y := 0 To MaxHeight Do
    Begin
      DstPtr := GetScanLine(Y);
      For X := 0 To MaxWidth Do
      Begin
        C := ReadInteger;
        Case ImageType Of
          '1':  // PBM Ascii
          Begin
            // 0 = Blanc, 1 =  Noir
            If C >= 1 Then
              DstPtr^ := ClrBlack
            Else
              DstPtr^ := ClrWhite;
          End;
          '2':  // PGM Ascii
          Begin
            If MaxVal <> 255 Then
              Idx := C * 255 Div MaxVal
            Else
              Idx := C;
            With DstColor Do     // on va chercher la couleur dans la palette
            Begin
              Red := ImageDescription.PaletteEntries^[Idx].Red;
              Green := ImageDescription.PaletteEntries^[Idx].Green;
              Blue := ImageDescription.PaletteEntries^[Idx].Blue;
              Alpha := 255;//ImageDescription.PaletteEntries^[Idx].Alpha;
            End;
            DstPtr^ := DstColor;
          End;
          '3':  // PNM/PPM Ascii
          Begin
            R := C;
            G := ReadInteger;
            B := ReadInteger;
            If MaxVal <> 255 Then
            Begin
              R := R * 255 Div MaxVal;
              G := G * 255 Div MaxVal;
              B := B * 255 Div MaxVal;
            End;
            With DstColor Do
            Begin
              Red := R;
              Green := G;
              Blue := B;
              Alpha := 255;
            End;
            DstPtr^ := DstColor;
          End;
        End;
        Inc(DstPtr);
      End;
      AdvanceProgress(Delta, 0, 1, False);
    End;
  End
  Else
  Begin // Formats Binaire
    Memory.SkipChar(icWhiteSpaces); // On saute les caractères de séparation
    // On initialise un buffer temporaire pour la lecture des données
    bSize := ImageDescription.Size;
    ReAllocMem(TempBuffer, bSize);
    Try
      // On lit toutes les données d'un coup
      Memory.Read(TempBuffer^, bSize);
      SrcPtr := TempBuffer;
      // On traite les données suivant le format
      Case ImageType Of
        '4': //PBM Binaire
        Begin
          DstPtr := GetScanLine(0); // GetBuffer
          X := 0;
          Y := 0;
          While (Y <= MaxHeight) Do
          Begin
            // Blanc ou Noir ?
            If ((PByte(SrcPtr + (X Div 8))^ Shr (7 - (X And 7))) And 1) <> 0 Then
              Idx := 1
            Else
              Idx := 0;
            If Idx = 1 Then
              DstPtr^ := ClrBlack
            Else
              DstPtr^ := ClrWhite;
            Inc(X);
            Inc(DstPtr);
            If X > MaxWidth Then
            Begin
              X := 0;
              Inc(Y);
              AdvanceProgress(Delta, 0, 1, False);
              If (Y <= MaxHeight) Then
                SrcPtr := PByte(TempBuffer + (Y * ImageDescription.BytesPerLine)); // On se place au debut de la ligne
            End;
          End;
        End;
        '5':  // PGM Binaire
        Begin
          DstPtr := GetScanLine(0); // GetBuffer
          X := 0;
          Y := 0;
          While (Y <= MaxHeight) Do
          Begin
            If MaxVal > 255 Then  // 16 bits
            Begin
              DW := PWord(SrcPtr + (X * 2))^ * 65535 Div MaxVal;
              Idx := DW;
              //Idx:= dw * 255 div MaxVal;
            End
            Else
            Begin   // 8 Bits
              db := PByte(SrcPtr + X)^;
              Idx := db;
            End;
            With DstPtr^ Do     // on va chercher la couleur dans la palette
            Begin
              Red := ImageDescription.PaletteEntries^[Idx].Red;
              Green := ImageDescription.PaletteEntries^[Idx].Green;
              Blue := ImageDescription.PaletteEntries^[Idx].Blue;
              Alpha := 255;//ImageDescription.PaletteEntries^[Idx].Alpha;
            End;
            Inc(X);
            Inc(DstPtr);
            If X > MaxWidth Then
            Begin
              X := 0;
              Inc(Y);
              If (Y <= MaxHeight) Then
              Begin
                SrcPtr := PByte(TempBuffer + (Y * ImageDescription.BytesPerLine)); // On se place au debut de la ligne
                AdvanceProgress(Delta, 0, 1, False);
              End;
            End;
          End;
        End;
        '6': //PPM/PNM Binaire
        Begin
          DstPtr := GetScanLine(0); // GetBuffer
          X := 0;
          Y := 0;
          While (Y <= MaxHeight) Do
          Begin
            If MaxVal > 255 Then      // Format RGB 48Bits
            Begin
              //DW := PWord(SrcPtr + (X * 2))^ * 65535 Div MaxVal;
              //Idx := DW;
              //Idx := Int64(dw * 255) Div Int64(MaxVal);
            End
            Else
            Begin
              ColorRgb24 := PBZColor24(SrcPtr + (X * 3))^; //Memory.ReadByte;
              Idx := db;
            End;
            DstPtr^.Create(ColorRGB24);
            Inc(X);
            Inc(DstPtr);
            If X > MaxWidth Then
            Begin
              X := 0;
              Inc(Y);
              If (Y <= MaxHeight) Then
              Begin
                SrcPtr := PByte(TempBuffer + (Y * ImageDescription.BytesPerLine)); // On se place au debut de la ligne
                AdvanceProgress(Delta, 0, 1, False);
              End;
            End;
          End;
        End;
        '7': // PAM Toujours binaire
        Begin
          DstPtr := GetScanLine(0);
          X := 0;
          Y := 0;
          While (Y <= MaxHeight) Do
          Begin
            Case TupleType Of
              ttBlackAndWhite:    // 8 bits 0 = blanc 1 = noir
              Begin

              End;
              ttGrayScale:   // 8 bits
              Begin

              End;
              ttBlackAndWhiteAlpha: // 16 bits
              Begin

              End;
              ttGrayScaleAlpha: // 16 bits
              Begin

              End;
              ttRGB: // 24 Bits
              Begin

              End;
              ttRGBAlpha:   // 32 bits
              Begin

              End;
            End;
            Inc(X);
            Inc(DstPtr);
            If X > MaxWidth Then
            Begin
              X := 0;
              Inc(Y);
              If (Y <= MaxHeight) Then
              Begin
                SrcPtr := PByte(TempBuffer + (Y * ImageDescription.BytesPerLine)); // On se place au debut de la ligne
                AdvanceProgress(Delta, 0, 1, False);
              End;
            End;
          End;
        End;
        'F', 'f':  // Float PixMap toujours binaire
        Begin
          isFPGray := ImageType = 'f';
          SrcPtrF := PSingle(TempBuffer);
          // Une majeur partie des logiciels enregistrent l'image de bas en haut
          // DstPtr:= PBZColor(GetScanLine(MaxHeight)+MaxWidth); // GetBuffer
          DstPtr := PBZColor(GetScanLine(MaxHeight));
          X := 0;
          Y := 0;
          While (Y <= MaxHeight) Do
          Begin

            If IsFPGray Then
            Begin
              db := ClampByte(Round(PSingle(SrcPtr + X)^ * 255));
              DstPtr^.Create(db, db, db, 255);
            End
            Else
            Begin
              // Les couleurs sont stockées au format RGB
              //FPColorRGB := PBZFPColorRGB_96(SrcPtrF+X)^
              sR := PSingle(SrcPtrF + (X * 3))^;
              sG := PSingle(SrcPtrF + (X * 3 + 1))^;
              sB := PSingle(SrcPtrF + (X * 3 + 2))^;

              If isBigEndian Then
              Begin
                sR := Swap(Longword(sR));
                sG := Swap(Longword(sG));
                sB := Swap(Longword(sB));
              End;

              With DstPtr^ Do
              Begin
                Red := ClampByte(Round(sR * 255));
                Green := ClampByte(Round(sG * 255));
                Blue := ClampByte(Round(sB * 255));
                Alpha := 255;
              End;
            End;
            Inc(X);
            Inc(DstPtr);
            If X > MaxWidth Then
            Begin
              X := 0;
              Inc(Y);
              If (Y <= MaxHeight) Then
              Begin
                SrcPtrF := PSingle(TempBuffer + (Y * (12 * Width))); // On se place au debut de la ligne
                DstPtr := PBZColor(GetScanLine(MaxHeight - Y));
                AdvanceProgress(Delta, 0, 1, False);
              End;
            End;
          End;
        End;
      End;
    Finally
      FreeMem(TempBuffer);
      TempBuffer := nil;
    End;
  End;
  FinishProgressSection(False);
  FinishProgressSection(True);
End;

{%endregion%}

Initialization

  RegisterRasterFormat('pbm', 'Portable BitMap image', TBZBitmapPPMImage);
  RegisterRasterFormat('pgm', 'Portable GrayMap image', TBZBitmapPPMImage);
  RegisterRasterFormat('pnm', 'Portable AnyMap image', TBZBitmapPPMImage);
  RegisterRasterFormat('ppm', 'Portable PixMap image', TBZBitmapPPMImage);
  RegisterRasterFormat('pam', 'Portable Arbitrary Map image', TBZBitmapPPMImage);
  RegisterRasterFormat('pfm', 'Portable FloatMap image', TBZBitmapPPMImage);


Finalization
  UnregisterRasterFormat(TBZBitmapPPMImage);

End.
