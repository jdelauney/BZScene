(*
  @abstract(Prise en charge des Images au format GIF en lecture. Version 87a et 89a)

  Spécifications :
  @unorderedList(
    @item(Méthode de compression     : LZW
    @item(Nombre de couleurs	       : 8 bits, 16 bits format BGR 555, 24 bits ou 32 bits BGR(A)
    @item(Supporte plusieurs images  : Oui
    @item(Format des nombres	       : Big-endian
    @item(Auteur	                   : Compuserve
    @item(Extensons                  : *.gif
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
    Informations sur le format GIF :
    @unorderedList(
       @item(https://www.developpez.net/forums/d1854871/autres-langages/pascal/lazarus/1-8-2-linux-windows-bugs-l-openpicturedialog-edit-plutot-l-unite-graphics/)
       @item(https://www.developpez.net/forums/d1867467/autres-langages/pascal/lazarus/composant-tgifviewer/)
       @item(http://netghost.narod.ru/gff/graphics/summary/gif.htm)
       @item(http://www.fileformat.info/format/gif/egff.htm)
       @item(http://delphiaccess.com/foros/index.php/topic/2589-formato-de-imagen-gif/?p=51612)
       @item(http://melander.dk/delphi/gifimage/)
       @item(https://github.com/mike-lischke/GraphicEx)
       @item(https://github.com/aducom/gifanim)
       @item(http://consume.o2switch.net/lazarus/)
    )

    Encodage/Decodage : LZW ou RLE

    Autres informations utiles :
         Fichiers test : http\

  -------------------------------------------------------------------------------------------------------------

  @bold(Dépendances) : BZClasses, BZColors, BZGraphic, BZBitmap, BZImageFileIO, BZImageStrConsts, BZUtils

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits) :
    Quelques parties, proviennent de TGIFImage, Copyright (c) 1997-99 Anders Melander
    et de GraphicEx par Mike Lischke et d'autres provenant d'obscures sources trouvées sur le web. @br
    ( Avec ces sources, c'est comme l'histoire de l'oeuf et la poule.......)


  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
Unit BZImageFileGIF;

//------------------------------------------------------------------------------
//----------------------------[ TODO LIST ]-------------------------------------

{ TODO 0  -oBZBitmap -cSupport_Images_GIF : Ecriture Format GIF }


//------------------------------------------------------------------------------

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\bzscene_options.inc}
//==============================================================================

{.$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils,
  BZClasses, BZColors, BZGraphic, BZBitmap, BZImageFileIO;

{%region=====[ Définitions des types et constantes utiles ]=====================================================================================================}
const
   GIF_MaxColors  : integer	= 256;		// Nombre de couleurs maximum supportées. NE PAS TOUCHER A CETTE VALEUR
   GIF_DelayFactor : Integer  = 10;
   GIF_DefaultDelay : Integer = 100;
   GIF_ImageDefaultCompression: TBZEncodingType = etLZW; // Methode compression par defaut

type
  TBZGIFVersion = (gvUnknown, gv87a, gv89a);
  TBZGIFVersionRec = array[0..2] of AnsiChar;

const
  cBZGIFVersions : array[gv87a..gv89a] of TBZGIFVersionRec = ('87a', '89a');

type
  { En-tête }
  TBZGIFFileHeader = packed record
    Signature: array[0..2] of AnsiChar; // 'GIF'
    Version: TBZGIFVersionRec;   // '87a' ou '89a' }
  end;

  { Description globale de l'image }
  TBZGIFLogicalScreenDescriptorRec = packed record
    ScreenWidth: word;              // Largeur de l'image en pixels
    ScreenHeight: word;             // Hauteur de l'image en pixels
    PackedFields: byte;             // champs compactés
    BackgroundColorIndex: byte;     // Index globale de la couleur de fond
    AspectRatio: byte;              // Ratio d'échelle = (AspectRatio + 15) / 64
  end;

  { Description d'une image }
  TBZGIFImageDescriptorRec = packed record
    //Separator: byte;  // On lis toujours un bytee avant
    Left: word;		      // Colonne en pixels par rapport au bord gauche de l'écran
    Top: word;		      // Rangée en pixels par rapport au haut de l'écran
    Width: word;	      // Largeur de l'image en cours en pixels
    Height: word;	      // Hauteur de l'image en cours pixels
    PackedFields: byte;	// Champs compactés
  end;

  { Graphic Control Extension bloc a.k.a GCE }
  TBZGIFGraphicControlExtensionRec = packed record
    // BlockSize: byte;           // Normalement toujours 4 octets
    PackedFields: Byte;           // Champs compacté
    DelayTime: Word;              // Délai entre chaque image en centième de secondes
    TransparentColorIndex: Byte;  // Index dans la palette si plus petit ou égale
   // Terminator: Byte;           // Normalement toujours ZERO
  end;

  TBZGIFDisposalFlag = (dmNone, dmKeep, dmErase, dmRestore); // Methodes pour l'affichage des images lors de l'animation

  { Plain Text Extension }
  TBZGIFPlainTextExtensionRec = packed record
     // BlockSize: byte;             // Normalement égal à 12 octets
    Left, Top, Width, Height: Word;  // Positions et dimensions du texte
    CellWidth, CellHeight: Byte;     // Dimensions d'une cellule dans l'image
    TextFGColorIndex,                // Index de la couleur de fond dans la palette
    TextBGColorIndex: Byte;          // Index de la couleur du texte dans la palette
  end;

  { Application Extension }
  TBZGIFApplicationExtensionRec = packed record
    AppID: array [0..7] of AnsiChar;                  // Identification de l'application majoritairement 'NETSCAPE' ou ''
    AppAuthenticationCode: array [0..2] of AnsiChar;  // Code d'authentification ou numero de version
  end;

  { Informations de "l'application extension" si disponible }
  TBZGIFNSLoopExtensionRec = packed record
    Loops : Word;        // Nombre de boucle de l'animation 0 = infinie
    BufferSize : DWord;  // Taille du tampon. Usage ?????
  End;
  
//-----------------------------------------------------------------------------------
  TBZGIFDataBlockRec = packed record
      BlockSize : Byte;
      Data : Pointer;
    End;


  { Informations sur une image de l'animation }
  TBZGifFrameInformations = record
    Left, Top,                     // Position de l'image
    Width, Height : Integer;       // Dimension de l'image
    HasLocalPalette : Boolean;     // Palette locale disponible
    IsTransparent : Boolean;         // Image transparente
    UserInput : Boolean;           // Données personnelle
    BackgroundColorIndex: Byte;    // Normalement seulement valide si une palette globale existe
    TransparentColorIndex: Byte;   // Index de la couleur transparente
    DelayTime: Word;               // Délai d'animation
    Disposal: TBZGIFDisposalFlag;  // Methode d'affichage
    Interlaced : Boolean;          // Image entrelacée
  end;
  PBZGifFrameInformations = ^TBZGifFrameInformations;


//  TBZGIFFrameList = class(specialize TBZArrayInt<TBZGifFrameInformations>);


const
  // Description des masques pour la description globale de l'image
  GIF_GLOBALCOLORTABLE = $80;       // Défini si la table de couleurs globale suit la description globale
  GIF_COLORRESOLUTION = $70;        // Résolution de la couleur (BitsPerPixel) - 3 bits
  GIF_GLOBALCOLORTABLESORTED = $08; // Définit si la palette globale est triée - 1 bit
  GIF_COLORTABLESIZE = $07;         // Taille de la palette - 3 bits
  GIF_RESERVED		= $0C;            // Réservé - doit être défini avec $00 - Taille des données = 2^value+1 - 3 bits

  // Descption des masques pour les images
  GIF_LOCALCOLORTABLE = $80;       // Défini si la table de couleurs locale suit la description de l'image
  GIF_INTERLACED = $40;            // Défini si l'image est entrelacée
  GIF_LOCALCOLORTABLESORTED= $20;  // Définit si la palette locale est triée

  // Identification des blocs
  GIF_PLAINTEXT = $01;
  GIF_GRAPHICCONTROLEXTENSION = $F9;
  GIF_COMMENTEXTENSION = $FE;
  GIF_APPLICATIONEXTENSION = $FF;
  GIF_IMAGEDESCRIPTOR = $2C;       // ','
  GIF_EXTENSIONINTRODUCER = $21;   // '!'
  GIF_TRAILER = $3B;               // ';'

  // Graphic Control Extension - Définition des masques pour les paramètres
  GIF_NO_DISPOSAL              = $00;  // 0
  GIF_DO_NOT_DISPOSE           = $04;  // 1
  GIF_RESTORE_BACKGROUND_COLOR = $08;  // 2
  GIF_RESTORE_PREVIOUS         = $12;  // 3
  GIF_DISPOSAL_ALL             = $1C;  // bits 2-4 ($1C)
  GIF_USER_INPUT_FLAG          = $02;
  GIF_TRANSPARENT_FLAG         = $01;
  GIF_RESERVED_FLAG            = $E0;

  // Identification des sous-blocs pour "Application Extension"
  GIF_LOOPEXTENSION	= 1;
  GIF_BUFFEREXTENSION	= 2;

const
   GifGCEDisposalModeStr : array[TBZGIFDisposalFlag] of string =
     ('None', 'Keep', 'Erase', 'Restore');

{%endregion%}

Type
  { TBZBitmapGIFImage }
  TBZBitmapGIFImage = Class(TBZCustomImageFileIO)
  private
    FGIFFIleHeader : TBZGIFFileHeader;
    FLogicalScreenChunk : TBZGIFLogicalScreenDescriptorRec;
    FHasGlobalPalette : Boolean;
    FGlobalPalette : TBZColorList;
    FBackgroundColor : TBZColor;
    //FExtensions: TStringList;

  protected
    CurrentFrameInfos : TBZGifFrameInformations;
    CurrentFrameIndex : Integer;

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
{.$DEFINE DEBUG}
Uses
  BZImageStrConsts,
  BZUtils
  {$IFDEF DEBUG}
  , Dialogs, BZLogger, BZTypesHelpers
  {$ENDIF};

type
  // Statut de décodage / encodage LZW
  TBZLZWDecoderStatus = (
    dsOK,                     // Everything ok
    dsNotEnoughInput,         // Tampon d'entrée trop petit
    dsOutputBufferTooSmall,   // Tampon de sortie trop petit
    dsInvalidInput,           // Donnée corrompue
    dsBufferOverflow,         // débordement de tampon
    dsInvalidBufferSize,      // Taille d'un des tampons invalide
    dsInvalidInputBufferSize, // Taille du tampon d'entrée invalide
    dsInvalidOutputBufferSize,// Taille du tampon de sortie invalide
    dsInternalError           // Erreur interne signifiant qu'il y a un défaut dans le code
  );

{%region%=====[ TBZBitmapGIFImage ]============================================}

Constructor TBZBitmapGIFImage.Create(AOwner: TPersistent; AWidth, AHeight: Integer);
Begin
  Inherited Create(aOwner, AWidth, AHeight);
  With DataFormatDesc Do
  Begin
    Name := 'GIF';
    Desc := 'Graphic Interchange Format';
    FileMask := '*.gif';
    Version := '---';
    Encoding := etLZW;
  End;
  //FApplicationExtensions := TStringList.Create;
  SupportedColorFormat := SupportedColorFormat + [cfIndexed, cfRGB];
end;

Destructor TBZBitmapGIFImage.Destroy;
Begin
  SupportedColorFormat := [];
  //FApplicationExtensions.Free;
  Inherited Destroy;
End;

Class Function TBZBitmapGIFImage.Capabilities: TBZDataFileCapabilities;
Begin
  Result := [dfcRead]; //[dfcRead, dfcWrite]
End;

Function TBZBitmapGIFImage.getImagePropertiesAsString: String;
Var
  S: String;
Begin
  S := '';
  With DataFormatDesc Do
  Begin
    S := 'Format de fichier : ' + Name + #13 + #10; //+Desc
  End;
  Result:=S;
end;

Function TBZBitmapGIFImage.CheckFormat(): Boolean;
Var
  GIFVersion : String;
Begin
  Result :=  False;
  // Chargement de l'en-tête
  Memory.Read(FGIFFileHeader, SizeOf(TBZGIFFileHeader));
  // Vérification de quelques paramètres
  Result := uppercase(string(FGIFFileHeader.Signature)) = 'GIF';
  if Result then
  begin
    // Le fichier est valide, on teste la version
    GIFVersion := String(FGIFFileHeader.Version);
    If (GIFVersion = cBZGIFVersions[gv87a]) Or (GIFVersion = cBZGIFVersions[gv89a]) Then
    begin
     // On sauvegarde la version du GIF
     With DataFormatDesc Do
     Begin
       Version :=  string(FGIFFileHeader.Version);
     End;
     // On lit les propriétés
     Result := ReadImageProperties;
    End
    Else
     Raise Exception.Create(rsUnknownVersion);
  End
  Else
  Begin
    // Signature du fichier GIF Invalide. On lève une exception
    Raise Exception.Create(Format(rsBadSignature,[uppercase(String(FGIFFileHeader.Signature))]));
  End;
End;

Function TBZBitmapGIFImage.ReadImageProperties: Boolean;
Var
  bmpWidth, bmpHeight : Integer;
  BitsPerSample, BitsPerPixel : Integer;
Begin
  Result :=  False;
  {$IFDEF DEBUG}
    GlobalLogger.LogStatus('-------------------------------------------------------------------------------------------------------------------------------------');
    GlobalLogger.LogStatus('Read GIF Image : '+ Self.FullFileName);
    GlobalLogger.LogStatus('Version : '+DataFormatDesc.Version);
    GlobalLogger.LogStatus('=====================================================================================================================================');
  {$ENDIF}

  Memory.Read(FLogicalScreenChunk, SizeOf(TBZGIFLogicalScreenDescriptorRec));

  // On sauvegarde en local les dimensions de l'image, pour plus tard
  bmpWidth := FLogicalScreenChunk.ScreenWidth;
  bmpHeight := FLogicalScreenChunk.ScreenHeight;

  {$IFDEF DEBUG}
    GlobalLogger.LogStatus('------[ LOGICAL SCREEN ]-------------------------------------------------------------------------------------------------------------');
    GlobalLogger.LogStatus('Width : '+BmpWidth.ToString);
    GlobalLogger.LogStatus('Height : '+BmpHeight.ToString);
    GlobalLogger.LogStatus('=====================================================================================================================================');
  {$ENDIF}
  if (BmpWidth<1) or (BmpHeight<1) then
  begin
    // Dimensions incorrectes on lève une exception
    RaiseInvalidImageHeader(Format(rsBadImageSize,[BmpWidth,BmpHeight]));
    exit;
  End;
  FHasGlobalPalette := (FLogicalScreenChunk.PackedFields and GIF_GLOBALCOLORTABLE) <> 0;

  // Initialisation de la description de notre image
  With ImageDescription Do
  Begin
    // On verifie que le nombre de bits par composant est valide
    //if (BitsPerSample<1) or (BitsPerSample>8) then
    //   RaiseInvalidImageData(ssInvalidBitsPerSample + ' : ' + BitsPerSample.ToString());
    // BitsPerPixel := 3 * BitsPerSample;
    //if BitsPerSample = 1 then ImageDescription.ColorFormat :=cfRGB
    //else BitsPerSample = 8 then ImageDescription.ColorFormat :=cfIndexed;

    InitDefault(bmpWidth, bmpHeight, 8); // 8 = BPP
    // On Indique qu'il n'y a pas de padding de fin de ligne
    RowStrideType:=bleNoBoundary;
  end;
  Result := True;
End;

Procedure TBZBitmapGIFImage.LoadFromMemory();
Var
  Delta : Single;
  aRGBColor : TBZColorRGB_24;
  aColor : TBZColor;
  PaletteCount : Integer;
  Done : Boolean;
  BlockID : Byte;
  BlockSize : Byte;
  Terminator{%H-} : Byte;

  CurrentLayer : TBZBitmapLayerItem;

  ImageDescriptor : TBZGIFImageDescriptorRec;
  GraphicControlExtensionChunk : TBZGIFGraphicControlExtensionRec;
  ApplicationExtensionChunk : TBZGIFApplicationExtensionRec;
  NSLoopExtensionChunk : TBZGIFNSLoopExtensionRec;
  PlainTextChunk : TBZGIFPlainTextExtensionRec;

  LocalPalette : TBZColorList;
  ColorCount : Integer;
  DMode : Byte;
  ret : TBZLZWDecoderStatus;

  { Chargement palette globale }
  procedure LoadGlobalPalette;
  Var
    J : Byte;
  begin
    If FHasGlobalPalette then
    Begin
      if FGlobalPalette =  nil then FGlobalPalette := TBZColorList.Create
      else FGlobalPalette.Clear;

      PaletteCount :=2 SHL (FLogicalScreenChunk.PackedFields AND GIF_COLORTABLESIZE);
      // Le cas ou le nombre de couleurs serait plus grand que 256. On prend en charge.
      if (PaletteCount<2) then //or (PaletteCount>256) then
         RaiseInvalidImageFile(rsScreenBadColorSize + ' : ' + IntToStr(PaletteCount));

      {$IFDEF DEBUG} GlobalLogger.LogStatus('Load Global palette : '+PaletteCount.ToString);{$ENDIF}
      // On charge la palette
      Delta := 100 / PaletteCount;
      StartProgressSection(100 ,rsLoadingPalette);
      For J := 0 to PaletteCount-1 do
      begin
        Memory.Read(aRGBColor, SizeOF(TBZColorRGB_24));
        aColor.Create(aRGBColor.Red,aRGBColor.Green,aRGBColor.Blue);
        FGlobalPalette.AddColor(aColor);
        AdvanceProgress((J*Delta),0,1,False);
      End;
      FinishProgressSection(False);
    End;
  End;

  { Chargement palette locale }
  procedure LoadLocalPalette;
  Var
    J : Byte;
  begin
    // Aucune palette locale n'a été assignée. On en créer une nouvelle. Sinon on efface simplement son contenu.
    if LocalPalette =  nil then LocalPalette := TBZColorList.Create
    else LocalPalette.Clear;

    // On verifie que le nombre de couleur dans la palette est correcte
    ColorCount :=(2 SHL (ImageDescriptor.PackedFields AND GIF_COLORTABLESIZE));
    // Le cas ou le nombre de couleurs serait plus grand que 256. On prend en charge.
    if (ColorCount<2) then //or (ColorCount>256) then
       RaiseInvalidImageFile(rsImageBadColorSize + ' : ' + IntToStr(ColorCount));

    {$IFDEF DEBUG} GlobalLogger.LogStatus('Load Locale palette : '+ColorCount.ToString+' At '+Memory.Position.ToString);{$ENDIF}
    // On charge la palette
    Delta := 100 / ColorCount;
    StartProgressSection(100 ,'Chargement de la palette Locale');
    For J := 0 to ColorCount-1 do
    begin
      Memory.Read(aRGBColor, SizeOF(TBZColorRGB_24));
      aColor.Create(aRGBColor.Red,aRGBColor.Green,aRGBColor.Blue);
      //{$IFDEF DEBUG} GlobalLogger.LogStatus('Add local color #'+j.ToString+' : '+aColor.ToString);{$ENDIF}
      LocalPalette.AddColor(aColor);
      AdvanceProgress((J*Delta),0,1,False);
    End;
    FinishProgressSection(False);
  End;

  { Lecture des extensions }
  procedure ReadExtension;
  Var
    ExtensionID, BlockType : Byte;
    BufStr : array[0..255] of char; //String;
    Loops : Word;
	CurrentExtension : String;
  begin
    {$IFDEF DEBUG} GlobalLogger.LogStatus('Read Extensions at '+Memory.position.ToString);{$ENDIF}
    // On lit les extension jusqu'a ce qu'un bloc de description d'une image soit détecter ou que jusqu'a la fin du fichier
    repeat
      ExtensionID := Memory.ReadByte;
	  CurrentExtension :='';
      // Si c'est un  nouveau marqueur d'introduction d'extension. On lit le nouvel ID
      if (ExtensionID = GIF_EXTENSIONINTRODUCER) then ExtensionID := Memory.ReadByte;
      {$IFDEF DEBUG} GlobalLogger.LogStatus('Extension ID = '+ExtensionID.ToString+ ' At ' + Memory.position.ToString);{$ENDIF}
      if (ExtensionID = 0) then
      begin
        // On Saute les ID Nul
        {$IFDEF DEBUG} GlobalLogger.LogStatus('----> FOUND ID NULL ');{$ENDIF}
        Repeat
          ExtensionID:=Memory.ReadByte;
        until  (ExtensionID <> 0);
      End;
      Case ExtensionID of
        GIF_PLAINTEXT :
          begin
            {$IFDEF DEBUG} GlobalLogger.LogStatus('Read Extension Plain Text at '+Memory.position.ToString);{$ENDIF}
            BlockSize := Memory.ReadByte;
            Memory.Read(PlainTextChunk, SizeOf(TBZGIFPlainTextExtensionRec));
            repeat
              // On lit la taille du bloc. Si Zero alors fin des données de l'extension
              BlockSize := Memory.ReadByte;
              // On lit la chaine de caractères
              if (BlockSize>0) then
              begin
                fillchar({%H-}BufStr, 256, 0);
                Memory.Read(BufStr,BlockSize);
                BufStr[BlockSize]:=#0;
                // On place le texte dans les commentaires
                CurrentLayer.Comment.Add(String(BufStr));
                {$IFDEF DEBUG} GlobalLogger.LogStatus(String(BufStr));{$ENDIF}
              End;
            until (BlockSize = 0);
            // On ajoute une ligne vide de séparation
            CurrentLayer.Comment.Add('');
            {$IFDEF DEBUG} GlobalLogger.LogStatus('-----');{$ENDIF}
          End;
        GIF_COMMENTEXTENSION :
          begin
            {$IFDEF DEBUG} GlobalLogger.LogStatus('Read Extension Comment : ');{$ENDIF}
            repeat
              // On lit la taille du commentaire. Si Zero alors fin des données de l'extension
              BlockSize := Memory.ReadByte;
              // On lit la chaine de caractères
              if (BlockSize>0) then
              begin
                //fillchar({%H-}BufStr, 256, 0);
                Memory.Read(BufStr,BlockSize);
                BufStr[BlockSize]:=#0;
                // On place le texte dans les commentaires
                CurrentLayer.Comment.Add(String(BufStr));
                {$IFDEF DEBUG} GlobalLogger.LogStatus(String(BufStr));{$ENDIF}
              end;
            Until (BlockSize<=0);
             // On ajoute une ligne vide de séparation
            CurrentLayer.Comment.Add('');
            {$IFDEF DEBUG} GlobalLogger.LogStatus('-----');{$ENDIF}
          End;
        GIF_APPLICATIONEXTENSION:
          begin
            {$IFDEF DEBUG} GlobalLogger.LogStatus('Read Extension Application');{$ENDIF}
            BlockSize := Memory.ReadByte;
            {$IFDEF DEBUG} GlobalLogger.LogStatus('Read Extension Application --> BlockSize : '+BlockSize.ToString);{$ENDIF}
            // Certains vieux filtres d'exportation Adobe, ou d'autres logiciels utilisent par erreur une valeur de 10, ou plus petite ou trop grande
            if (BlockSize <> 11) then
            begin
              FillChar(ApplicationExtensionChunk, SizeOf(TBZGIFApplicationExtensionRec),0);
            End;

            //else if (BlockSize<11) then
            //   RaiseInvalidImageFile(sBadApplicationExtensionBlockSize + ' : ' + BlockSize.ToString+' octets. ( Taille valide = 11 octets )'); }

            Memory.Read(ApplicationExtensionChunk, SizeOf(TBZGIFApplicationExtensionRec));
			CurrentExtension := ApplicationExtensionChunk.AppAuthenticationCode;
            Repeat
               // On lit la taille du  bloc. Zero si il n'y a pas de données supplémentaires
              BlockSize := Memory.ReadByte;
            If (BlockSize > 0) Then
            Begin
              if UpperCase(CurrentExtension) = 'NETSCAPE' then
              begin
                BlockType := Memory.ReadByte;
                Dec(BlockSize);
                Case (BlockType And $07) Of
                  GIF_LOOPEXTENSION:
                  Begin
                    // Lecture du nombre de boucle, Si Zero alors boucle infinie
                    Loops := Memory.ReadWord;
                    If Loops > 0 Then Inc(NSLoopExtensionChunk.Loops);
                    Dec(BlockSize, SizeOf(Loops));
                  End;
                  GIF_BUFFEREXTENSION:
                  Begin
                    // Lecture de la taille du tampon. Utilisé pour ??????
                    NSLoopExtensionChunk.BufferSize := Memory.ReadDWord;
                    Dec(BlockSize, SizeOF(NSLoopExtensionChunk.BufferSize));
                  End;
                  else // Extension NETSCAPE inconnue
                    begin
                      Memory.SeekForward(BlockSize);
                      //BlockSize := 0;
                    end;
                End;
              end
              else
              // On saute et on ignore les donnée non lues
              If (BlockSize > 0) Then
              Begin
                Memory.SeekForward(BlockSize);
                //BlockSize := 0;
              End;
            End;
            Until (BlockSize=0);
          End;
        GIF_GRAPHICCONTROLEXTENSION :
          begin
            {$IFDEF DEBUG} GlobalLogger.LogStatus('Read Extension Graphic Control');{$ENDIF}
            // On lit la taille de l'extension. Normalement 4 Octets. Cette valeur peut-être erronée. On en tient pas compte.
            BlockSize := Memory.ReadByte;
            //if BlockSize = 4 then
            //begin
            Memory.Read(GraphicControlExtensionChunk, SizeOf(TBZGIFGraphicControlExtensionRec));
            // On renseigne notre tampon d'informations pour les prochaines images décodées
            DMode := ((GraphicControlExtensionChunk.PackedFields and GIF_DISPOSAL_ALL) SHR 2);
            With CurrentFrameInfos do
            begin
              // Ces valeurs peuvent être utilisées pour plusieurs image. Elles restent valides jusqu'a la lecture du prochain "GCE" trouvé.
              Disposal    := TBZGIFDisposalFlag(DMode);
              IsTransparent := (GraphicControlExtensionChunk.PackedFields and GIF_TRANSPARENT_FLAG) <> 0;
              UserInput   := (GraphicControlExtensionChunk.PackedFields and GIF_USER_INPUT_FLAG) <> 0;
              TransparentColorIndex := GraphicControlExtensionChunk.TransparentColorIndex;
              BackgroundColorIndex := FLogicalScreenChunk.BackgroundColorIndex;
              DelayTime   := GraphicControlExtensionChunk.DelayTime;
            End;
            // Lecture de l'octet de fin de l'extension
            Terminator := Memory.ReadByte;
          end;
      End;
    Until  (ExtensionID = GIF_IMAGEDESCRIPTOR) or Memory.EOS;
    // Si l'ID pour la description de l'image est détecter on revient en arrière pour la prise ne charge par le traitement des données
    if (ExtensionID = GIF_IMAGEDESCRIPTOR) then Memory.GotoPreviousByte();
  End;
  { Chargement d'une image }
  procedure LoadImage;
  var
    DecoderStatus{%H-} : TBZLZWDecoderStatus;
    BufferSize, TargetBufferSize, BytesRead : Int64;
    InitCodeSize : Byte;
    OldPosition : Int64;
    Buffer, BufferPtr : PByte;
    TargetBuffer, TargetBufferPtr : PByte;
    LinePtr : PBZColor;
    Pass, Increment : Byte;
    x : Integer;
    TargetColor : TBZColor;
    ColIdx : Byte;
    CurrentLine : Integer;
    OutBmp : TBZCustomBitmap;

    // Decodeur GIF LZW. Basé sour le code source de la bibliothèque GraphicEX pour Delphi
    function DecodeLZW(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer):TBZLZWDecoderStatus;
    const
      { Constantes pour la décompression LZW }
      _BZLZWGIFCodeBits		= 12;	// Nombre maximal de bits par code d'un jeton (12 bits = 4095)
      _BZLZWGIFCodeMax		= 4096;//(1 SHL _BZLZWGIFCodeBits); // Nombre maximum de jeton
      _BZLZWGIFStackSize  = (2 SHL _BZLZWGIFCodeBits);	 // Taille de la pile de décompression
      _BZLZWGIFTableSize	= (1 SHL _BZLZWGIFCodeBits);	 // Taille de la table de décompression

    var
      J: Integer;
      Data,           // Données actuelle
      Bits,           // Compteur de bit
      Code: Cardinal; // Valeur courrante du Code
      SourcePtr: PByte;
      InCode: Cardinal; // Tampon pour passé le Code

      CodeSize: Cardinal;
      CodeMask: Cardinal;
      FreeCode: Cardinal;
      OldCode: Cardinal;
      Prefix: array[0.._BZLZWGIFTableSize] of Cardinal; // LZW prefix
      Suffix,                                           // LZW suffix
      Stack: array [0.._BZLZWGIFStackSize] of Byte;
      StackPointer: PByte;
      MaxStackPointer: PBYte;
      Target: PByte;
      FirstChar: Byte;  // Tampon de décodage d'un octet
      ClearCode,
      EOICode: Word;
      MaxCode: Boolean;

    Begin
      Result := dsOk;
      DecoderStatus := dsOk;
      if (PackedSize <= 0) or (UnpackedSize <= 0) then
      begin
        // Taille des tampons invalides
        if (PackedSize <= 0) and (UnpackedSize <= 0) then result := dsInvalidBufferSize
        else if PackedSize<=0 then result := dsInvalidInputBufferSize
        else if UnpackedSize<=0 then result := dsInvalidOutputBufferSize;
        Exit;
      End;

      // Initialisation  des paramètres pour la décompression
      CodeSize := InitCodeSize + 1;
      ClearCode := 1 shl InitCodeSize;
      EOICode := ClearCode + 1;
      FreeCode := ClearCode +2;
      OldCode := _BZLZWGIFCodeMax-1;
      CodeMask := (1 shl CodeSize) - 1; //CodeMasks[CodeSize];
      MaxCode := False;
      Code := 0;
      Target := PByte(Dest);
      SourcePtr := PByte(Source);

      // Initialisation des tables de Code
      for J := 0 to _BZLZWGIFTableSize do //ClearCode - 1 do
      begin
        Prefix[J] := _BZLZWGIFCodeMax;
        Suffix[J] := J;
      end;

      // Initalisation de la pile
      StackPointer := @Stack;
      MaxStackPointer := @Stack[_BZLZWGIFStackSize];
      FirstChar := 0;

      Data := 0;
      Bits := 0;
      while (UnpackedSize > 0) and (PackedSize > 0) do
      begin
        // On lit le "Code" dans le tampon d'entrée
        Inc(Data, SourcePtr^ shl Bits);
        Inc(Bits, 8);
        while (Bits > CodeSize) and (UnpackedSize > 0) do
        begin
          // Code actuel
          Code := Data and CodeMask;
          // Préparation pour la donnée suivante
          Data := Data shr CodeSize;
          Dec(Bits, CodeSize);

          // Décompression finie ?
          if Code = EOICode then
          begin
            // Si nous arrivons ici, il y a probablement quelque chose de suspect avec l'image GIF
            // Car normalement on stoppe dès que le tampon de sortie est plein.
            // Cela signifie que nous ne lirons jamais l'EOICode de fermeture dans les images normales.
            // Comme l'état du buffer est déjà vérifié après la boucle principale, nous ne le ferons pas ici.
            Break;
          end;

          // On vérifie s'il s'agit d'un code valide déjà enregistré
          if Code > FreeCode then
          begin
             // Code ne peux à être supérieur à FreeCode. Nous avons donc une image cassée.
             // On notifie l'erreur à l'utilisateur. Et on considère qu'il n'ya pas d'erreur.
             DecoderStatus := dsInvalidInput;
             AddError(Format(rsLZWInvalidInput,[CurrentFrameIndex]));
             //NotifyUser('Le décodeur a rencontré une entrée invalide (données corrompues)');
             Code := ClearCode;
             {$IFDEF DEBUG}
               GlobalLogger.LogWarning('Le décodeur a rencontré une entrée invalide (données corrompues)');
               GlobalLogger.LogStatus('Code = '+Code.ToString+' > FreeCode = '+FreeCode.ToString);
             {$ENDIF}
            //Break; //Ici, on continue le chargement du reste de l'image au lieu de le stopper
          end;

          // RAZ
          if Code = ClearCode then
          begin
            // réinitialisation de toutes les variables
            CodeSize := InitCodeSize + 1;
            CodeMask := (1 shl CodeSize) - 1; //CodeMasks[CodeSize];
            FreeCode := ClearCode + 2;
            OldCode := _BZLZWGIFCodeMax;
            MaxCode := False;
          end
          else if OldCode = _BZLZWGIFCodeMax then
          begin
            // Gestion du premier Code LZW : On le définit dans le tampon de sortie et on le conserve
            FirstChar := Suffix[Code];
            Target^ := FirstChar;
            Inc(Target);
            Dec(UnpackedSize);
            OldCode := Code;
          end
          else
          begin
            //On conserve le Code LZW actuel
            InCode := Code;

            // On place le nouveau code LZW sur la pile sauf quand nous avons déjà utilisé tous les codes disponibles
            if (Code = FreeCode) and not MaxCode then
            begin
              StackPointer^ := FirstChar;
              Inc(StackPointer);
              Code := OldCode;
            end;

            // boucle pour placer les octets décodés sur la pile
            while Code > ClearCode do
            begin
              StackPointer^ := Suffix[Code];
              if StackPointer >= MaxStackPointer then
              begin
                // Ne doit jamais arriver, c'est juste une précaution au cas ou.
                Result := dsBufferOverflow;
                break;
              end;
              Inc(StackPointer);
              Code := Prefix[Code];
            end;
            if Result <> dsOK then break; // Si il ya eu des erreurs on ne va pas plus loin

            // Place le nouveau Code dans la table
            FirstChar := Suffix[Code];
            StackPointer^ := FirstChar;
            Inc(StackPointer);

            //Transfert des données décodées vers notre tampon de sortie
            repeat
              if UnpackedSize <= 0 then
              begin
                // Le tampon de sortie est trop petit. On ne va pas plus loin
                // On notifie l'erreur à l'utilisateur. Et on considère qu'il n'ya pas d'erreur.
                // Afin de pouvoir afficher le GIF et continuer le chargement des images suivantes
                Result := dsOutputBufferTooSmall;
                AddError(Format(rsLZWOutputBufferTooSmall,[CurrentFrameIndex]));
                break;
              end;
              Dec(StackPointer);
              Target^ := StackPointer^;
              Inc(Target);
              Dec(UnpackedSize);
            until StackPointer = @Stack;
            if Result <> dsOK then break;

            if not MaxCode then
            begin
              if FreeCode<=_BZLZWGIFCodeMax then
              begin
                Prefix[FreeCode] := OldCode;
                Suffix[FreeCode] := FirstChar;
              end
              else  if FreeCode>_BZLZWGIFCodeMax then
              begin
                // On a intercepté une donnée corrompue. On continue quand la même décompression sans en tenir compte.
                // On notifie juste l'erreur à l'utilisateur
                DecoderStatus := dsInvalidInput;
                AddError(Format(rsLZWInvalidInput,[CurrentFrameIndex]));
                //NotifyUser('Le décodeur a rencontré une entrée invalide (données corrompues)');
               {$IFDEF DEBUG}
                 GlobalLogger.LogWarning('Le décodeur a rencontré une entrée invalide (données corrompues)');
                 GlobalLogger.LogStatus('FreeCode = '+FreeCode.ToString+' > CodeMax ='+_BZLZWGIFCodeMax.ToString);
               {$ENDIF}
			          FreeCode := _BZLZWGIFCodeMax;
                Prefix[FreeCode] := OldCode;
                Suffix[FreeCode] := FirstChar;
                //MaxCode := true;
              end;

              // On augmente la taille du Code si nécessaire
              if (FreeCode = CodeMask) and not(MaxCode) then
              begin
                if (CodeSize < _BZLZWGIFCodeBits) then
                begin
                  Inc(CodeSize);
                  CodeMask := (1 shl CodeSize) - 1;//CodeMasks[CodeSize];
                end
                else //On a atteind la limite maximum
                  MaxCode := True;
              end;

              if FreeCode < _BZLZWGIFTableSize then Inc(FreeCode);
            end;
            OldCode := InCode;
          end;
        end;
        Inc(SourcePtr);
        Dec(PackedSize);
        if (Result <> dsOK) or (Code = EOICode) then Break;
      end;

      if Result = dsOK then
      begin
        // On vérifie seulement si il n'ya pas eu d'erreur. Si ce n'est pas le cas, nous savons déjà que quelque chose ne va pas.
        // Notez qu'il est normal que PackedSize soit un peu> 0 parce que nous pouvons
        // pas lire l'EOICode mais arrêter dès que notre tampon de sortie est plein et
        // qui devrait normalement être le code juste avant l'EOICode.
        if PackedSize < 0 then
        begin
          Result := dsInternalError;
          // C'est une erreur sérieuse : nous avons eu un dépassement de tampon d'entrée que nous aurions dû intercepter. Nous devons arrêter maintenant.
          RaiseInvalidImageFile(rsLZWInternalErrorInputBufferOverflow);
          Exit;
        end;
        if UnpackedSize <> 0 then
        begin
          //if UnpackedSize > 0 then
          //begin
          //  //  Image corrompue
          //  DecoderStatus := dsNotEnoughInput;
          //  AddError('Image #'+CurrentFrameIndex.ToString+' : Le décodeur n''a pas pu décoder toutes les données car le tampon d''entrée est trop petit');
          //  //NotifyUser('Le décodeur  n''a pas pu décoder toutes les données car le tampon d''entrée est trop petit');
          //End
          //else
          if UnpackedSize < 0 then
          begin
              Result := dsInternalError;
              // C'est une erreur sérieuse : nous avons eu un dépassement de tampon de sortie que nous aurions dû intercepter. Nous devons arrêter maintenant.
              RaiseInvalidImageFile(rsLZWInternalErrorOutputBufferOverFlow);
          end;
        end;
      end
    end;

  begin

    BufferSize := 0;
    TargetBufferSize := 0;

    // On lit la description de l'image
    {$IFDEF DEBUG} GlobalLogger.LogStatus('Load Image at '+Memory.position.ToString);{$ENDIF}
    Memory.Read(ImageDescriptor, SizeOf(TBZGIFImageDescriptorRec));

    // On vérifie que les dimensions sont correctes.
    // Si on trouve des dimensions à zero, il se peut qu'il faudra traiter
    // une extension PlainText et dessiner ce texte en fonction des paramètres
    if (ImageDescriptor.Height = 0) or (ImageDescriptor.Width = 0) then
    begin
      // On assigne les dimensions par défaut du GIF
      ImageDescriptor.Width := FLogicalScreenChunk.ScreenWidth;
      ImageDescriptor.Height := FLogicalScreenChunk.ScreenHeight;
      // On notifie à l'utilisateur que les dimensions de l'image sont erronée. Mais on tente le chargement quand même
      // ShowMessage
    end;

    // Dans le cas ou les dimensions de l'image sont incorrectes dans "l'image descriptor". Ou que la taille des données compressées soit erronée.
    if (ImageDescriptor.Width > FLogicalScreenChunk.ScreenWidth) or  (ImageDescriptor.Height > FLogicalScreenChunk.ScreenHeight) then
    begin
      // On assigne les dimensions par défaut du GIF
      if (ImageDescriptor.Width > FLogicalScreenChunk.ScreenWidth) then ImageDescriptor.Width := FLogicalScreenChunk.ScreenWidth;
      if (ImageDescriptor.Height > FLogicalScreenChunk.ScreenHeight) then ImageDescriptor.Height := FLogicalScreenChunk.ScreenHeight;
      // On notifie à l'utilisateur que les dimensions de l'image sont erronée. Mais on tente le chargement quand même
      // ShowMessage
    end;

    // On renseigne notre tampon d'informations
    With CurrentFrameInfos do
    begin
      Left       := ImageDescriptor.Left;
      Top        := ImageDescriptor.Top;
      Width      := ImageDescriptor.Width;
      Height     := ImageDescriptor.Height;
      Interlaced := (ImageDescriptor.PackedFields And GIF_INTERLACED) = GIF_INTERLACED;
      HasLocalPalette := (ImageDescriptor.PackedFields And GIF_LOCALCOLORTABLE) = GIF_LOCALCOLORTABLE;
      {$IFDEF DEBUG}
        GlobalLogger.LogStatus('Image Descriptor : ');
        GlobalLogger.LogStatus('Top/Left : '+Top.ToString+' / '+Left.ToString);
        GlobalLogger.LogStatus('WidthxHeight : '+Width.ToString+'x'+Height.ToString);
        GlobalLogger.LogStatus('HasLocalPalette : '+HasLocalPalette.ToString());
        GlobalLogger.LogStatus('Interlaced : '+Interlaced.ToString);
        GlobalLogger.LogStatus('Transparent : '+IsTransparent.ToString);
        GlobalLogger.LogStatus('DrawMode : '+GifGCEDisposalModeStr[Disposal]);
      {$ENDIF}
    End;


    // L'image possède-t-elle sa propre palette de couleur ? Si oui on la charge.
    If CurrentFrameInfos.HasLocalPalette then LoadLocalPalette;

    Delta := 100 / CurrentFrameInfos.Height;
    StartProgressSection(100 ,Format(rsDecompressFrameData,[CurrentFrameIndex]));

    // Decompression de l'image
    // On ajoute une nouvelle image si besoin
    if (CurrentFrameIndex>0) and (CurrentFrameIndex>Layers.Count-1) then Layers.AddNewImage;
    // On assigne la nouvelle image au Bitmap de travail
    OutBmp := Layers.Items[CurrentFrameIndex].Bitmap;


    // On met à jour les informations
    With Layers.Items[CurrentFrameIndex] do
    begin
      Case CurrentFrameInfos.Disposal of
        dmNone    : DrawMode := 0;
        dmKeep    : DrawMode := 1;
        dmErase   : DrawMode := 2;
        dmRestore : DrawMode := 3;
      end;
      Left := CurrentFrameInfos.Left;
      Top  := CurrentFrameInfos.Top;
      If CurrentFrameInfos.DelayTime = 0 Then DelayTime := GIF_DefaultDelay
      Else
        DelayTime := CurrentFrameInfos.DelayTime * GIF_DelayFactor;
    end;

    // On lit le code d'initalisation de la compression LZW
    InitCodeSize := Memory.ReadByte;
    if InitCodeSize<2 then InitCodeSize := 2;
    if InitCodeSize>8 then InitCodeSize := 8;
    {$IFDEF DEBUG} GlobalLogger.LogStatus('LZW InitCodeSize : '+InitCodeSize.ToString+' At '+pred(Memory.position).ToString);{$ENDIF}

    // On sauve la position actuelle dans le flux
    OldPosition := Memory.position;

    BufferSize := 0;

    {$IFDEF DEBUG} GlobalLogger.LogStatus('Debut des données : '+' At '+Memory.position.ToString);{$ENDIF}
    // 1) On comptabilise la taille totale des données compresser. Afin de les décompresser en une seule fois.
    // On lit la taille du premier bloc
    BlockSize := Memory.ReadByte;
    While (BlockSize>0) and not(Memory.EOS)  do
    begin
      Inc(BufferSize,BlockSize);
      // On saute les données
      Memory.SeekForward(BlockSize);
      // On lit la taille des données
      if not(Memory.EOS) then BlockSize := Memory.ReadByte else blocksize :=0;
    end;

    {$IFDEF DEBUG} GlobalLogger.LogStatus('Decompress Image = BufferSize : '+BufferSize.ToString+' At '+Memory.position.ToString);{$ENDIF}
    // 2) On initalise notre bitmap avec les bonnes dimensions
    {$IFDEF DEBUG} GlobalLogger.LogStatus('Set SubImage #'+CurrentFrameIndex.ToString+' --> '+CurrentFrameInfos.Width.toString+'x'+ CurrentFrameInfos.Height.ToString);{$ENDIF}   
    OutBmp.SetSize(CurrentFrameInfos.Width, CurrentFrameInfos.Height);

    BufferPtr := nil;
    Buffer := nil;
    // 3) On alloue notre tampon pour les données compressées
    if (BufferSize>0) then Reallocmem(Buffer,BufferSize);

    // 4) On charge toutes les données dans notre tampon
    // On se replace au début des données
    Memory.Seek(OldPosition, soBeginning);
    {$IFDEF DEBUG} GlobalLogger.LogStatus('Verification Debut des données : '+' At '+Memory.position.ToString);{$ENDIF}
    // On travail toujours sur une copie du "pointer"
    BufferPtr := Buffer;
    // On lit la taille du premier bloque
    BlockSize := Memory.ReadByte;
    While (BlockSize>0) and not(Memory.EOS) do
    begin
      // On charge les données dans le tampon. On previent des erreurs en cas de dépassements
      BytesRead := Memory.Read(BufferPtr^,BlockSize);
      Inc(BufferPtr,BytesRead);
      if not(Memory.EOS) then BlockSize := Memory.ReadByte else blocksize :=0;
    end;
    // On se replace au debut du tampon
    BufferPtr := Buffer;
    // 5) On decompresse les données
    //  On initialise notre buffer ou seront décompressées les données
    {$IFDEF DEBUG} GlobalLogger.LogStatus('TargetBuffer : '+CurrentFrameInfos.Width.ToString+'x'+CurrentFrameInfos.Height.ToString);{$ENDIF}
    TargetBufferSize := CurrentFrameInfos.Width*CurrentFrameInfos.Height;
    {$IFDEF DEBUG}
      GlobalLogger.LogStatus('TargetBufferSize : '+TargetBufferSize.ToString);
      GlobalLogger.LogStatus('Layers Count : '+Layers.Count.ToString);
      GlobalLogger.LogStatus('Current Frame Index : '+CurrentFrameIndex.ToString);
    {$ENDIF}

    TargetBufferPtr := nil;
    TargetBuffer := nil;
    // Si la taille est plus grande que zero, on alloue l'espace nécessaire à notre tampon
    if (TargetBufferSize>0) then Reallocmem(TargetBuffer,TargetBufferSize);

    // Décodage des données compressées
    Ret := DecodeLZW(Buffer,TargetBuffer,BufferSize,TargetBufferSize);

    // 6) On transfert les données de l'image vers notre bitmap. Si il n'y a pas eu d'erreurs
    if (Ret=dsOk)  then
    begin
      TargetBufferPtr := TargetBuffer;
      OutBmp.Clear(Layers.BackgroundColor);
      // Image non entrelacée
      if not(CurrentFrameInfos.Interlaced) then
      begin
        CurrentLine := 0;
        While (CurrentLine<CurrentFrameInfos.Height) do
        begin
          LinePtr := OutBmp.GetScanLine(CurrentLine);
          For x:=0 to OutBmp.MaxWidth do
          begin
            // Lecture de l'index de la couleur dans la palette
            ColIdx := TargetBufferPtr^;
            // On utilise la palette de couleur locale
            If CurrentFrameInfos.HasLocalPalette Then
            Begin
              If LocalPalette <> nil Then // La palette est-elle chargée ?
              Begin
                //if (ColIdx> ColorCount-1) then ColIdx := ColorCount -1;
                If (ColIdx < ColorCount) Then TargetColor := LocalPalette.Colors[ColIdx].Value
                Else
                  TargetColor := clrTransparent;
              End
              Else If FGlobalPalette <> nil Then // Non, alors on utilise la palette globale si elle est présente
              Begin
               //if (ColIdx> PaletteCount-1) then ColIdx := PaletteCount -1;
                If (ColIdx < PaletteCount) Then TargetColor := FGlobalPalette.Colors[ColIdx].Value
                Else
                  TargetColor := clrTransparent;
              End
              Else
              Begin
                AddError(rsEmptyColorMap);
                Exit;
              End;
            End
            else // On utilise la palette de couleur globale
            begin
              If FGlobalPalette <> nil Then
              Begin
                //if (ColIdx> PaletteCount-1) then ColIdx := PaletteCount -1;
                If (ColIdx < PaletteCount) Then TargetColor := FGlobalPalette.Colors[ColIdx].Value
                Else
                  TargetColor := clrTransparent;
              End
              Else If LocalPalette <> nil Then
              Begin
                //if (ColIdx> ColorCount-1) then ColIdx := ColorCount -1;
                If (ColIdx > ColorCount - 1) Then //ColIdx := ColorCount -1;
                  TargetColor := LocalPalette.Colors[ColIdx].Value
                Else
                  TargetColor := clrTransparent;
              End
              Else
              Begin
                AddError(rsEmptyColorMap);
                Exit;
              End;
            End;

            If CurrentFrameInfos.IsTransparent Then
            Begin
              If FHasGlobalPalette Then If ColIdx < FGlobalPalette.Count Then OutBmp.TransparentColor := FGlobalPalette.Colors[ColIdx].Value.AsColor
              Else If ColIdx < LocalPalette.Count Then OutBmp.TransparentColor := LocalPalette.Colors[ColIdx].Value.AsColor;

              If (Self.ImageDescription.HasAlpha) then //Transparent) Then
              Begin
                If (ColIdx = CurrentFrameInfos.TransparentColorIndex) Then
                begin
                  TargetColor.Alpha := 0; // clrTransparent;
                end;
                If (CurrentFrameInfos.TransparentColorIndex = CurrentFrameInfos.BackgroundColorIndex) Then FbackgroundColor.Alpha := 0; //clrTransparent;
              End;
            End;

            LinePtr^ := TargetColor;  //SetPixel( X,CurrentLine,TargetColor);
            // On avance de 1 élément dans nos "pointer"
            inc(TargetBufferPtr);
            inc(LinePtr);
          End;
          Inc(CurrentLine);
          AdvanceProgress(Delta,0,1,False);
        End;
      End
      else // Image entrelacée
      begin
        CurrentLine := 0;
        For pass:=0 to 3 do
        begin
          Case Pass of
            0 :
              begin
                CurrentLine :=  0;
                Increment   := 8;
              End;
            1 :
              begin
                CurrentLine :=  4;
                Increment   := 8;
              End;
            2 :
              begin
                CurrentLine :=  2;
                Increment   := 4;
              End;
            else
              begin
                CurrentLine :=  1;
                Increment   := 2;
              End;
          End;
          While (CurrentLine<CurrentFrameInfos.Height) do
          begin
            LinePtr := OutBmp.GetScanLine(CurrentLine);
            For x:=0 to OutBmp.MaxWidth do
            begin
              // Lecture de l'index de la couleur dans la palette
              ColIdx := TargetBufferPtr^;
              // On utilise la palette de couleur locale
              If CurrentFrameInfos.HasLocalPalette Then
              Begin
                If LocalPalette <> nil Then // La palette est-elle chargée ?
                Begin
                  If (ColIdx < ColorCount) Then // Dans le cas contraire il s'agit d'un index pour la transparence
                    TargetColor := LocalPalette.Colors[ColIdx].Value;
                End
                Else If FGlobalPalette <> nil Then // Non, alors on utilise la palette globale si elle est présente
                Begin
                  If (ColIdx < PaletteCount) Then //if (ColIdx< PaletteCount-1) then ColIdx := PaletteCount -1;
                    TargetColor := FGlobalPalette.Colors[ColIdx].Value;
                End
                Else
                Begin
                  AddError(rsEmptyColorMap);
                  Exit;
                End;
              End
              Else // On utilise la palette de couleur globale
              Begin
                If FGlobalPalette <> nil Then
                Begin
                  If (ColIdx > PaletteCount - 1) Then ColIdx := PaletteCount - 1;
                  TargetColor := FGlobalPalette.Colors[ColIdx].Value;
                End
                Else If LocalPalette <> nil Then
                Begin
                  If (ColIdx > ColorCount - 1) Then ColIdx := ColorCount - 1;
                  TargetColor := LocalPalette.Colors[ColIdx].Value;
                End
                Else
                Begin
                  AddError(rsEmptyColorMap);
                  Exit;
                End;
              End;

              If CurrentFrameInfos.IsTransparent Then
              Begin
                If FHasGlobalPalette Then If ColIdx < FGlobalPalette.Count Then OutBmp.TransparentColor := FGlobalPalette.Colors[ColIdx].Value.AsColor
                Else If ColIdx < LocalPalette.Count Then OutBmp.TransparentColor := LocalPalette.Colors[ColIdx].Value.AsColor;
                If (Self.ImageDescription.HasAlpha) Then
                Begin
                  If CurrentFrameInfos.TransparentColorIndex = colIdx Then
                  begin
                    TargetColor.Alpha := 0; // := clrTransparent;
                  End;
                  If (CurrentFrameInfos.TransparentColorIndex = CurrentFrameInfos.BackgroundColorIndex) Then  TargetColor.Alpha := 0;//FBackgroundColor.Alpha := 0;
                End;
              End;
              LinePtr^ := TargetColor;
              inc(TargetBufferPtr);
              If (CurrentLine < CurrentFrameInfos.Height - 1) Then inc(LinePtr);
            End;
            Inc(CurrentLine, Increment);
            //if Pass = 3 then
            //begin
               AdvanceProgress(Delta*Increment,0,1,False);
            //end;
          end;
        End;
      End;
      // On met à jour la description du Bitmap
      //globallogger.LogNotice('OutBmp : '+Assigned(OutBmp).ToString());
      With Layers.Items[CurrentFrameIndex].Bitmap.ImageDescription do // OutBmp.ImageDescription Do
      Begin
        Interlaced :=  CurrentFrameInfos.Interlaced;
        HasAlpha :=  CurrentFrameInfos.IsTransparent;
        if  CurrentFrameInfos.IsTransparent then
        begin
          TransparentColor := clrTransparent;
          if FHasGlobalPalette then
          begin
            TransparentColor := FGlobalPalette.Colors[CurrentFrameInfos.TransparentColorIndex].Value
          End
          else if CurrentFrameInfos.HasLocalPalette then
          begin
            If LocalPalette <> nil Then // La palette est-elle chargée ?
              TransparentColor := LocalPalette.Colors[CurrentFrameInfos.TransparentColorIndex].Value
          end;
        end;
      End;
	  // Image corrompue on le signal afin de l'exclure plus tard pour un affichage correct de l'animation
	  if DecoderStatus <> dsOk then
      begin
        //outBmp.Clear(ClrTransparent);
        Layers.Items[CurrentFrameIndex].IsCorrupted := True;
        Layers.Items[CurrentFrameIndex].DelayTime:= 1;
      End;

      Inc(CurrentFrameIndex);   // Index pour la prochaine image

    End
    else
    begin
      Case Ret Of
        dsInvalidBufferSize: AddError(Format(rsGIFInvalidBufferSize,[CurrentFrameIndex]));
        dsInvalidInputBufferSize: AddError(Format(rsGIFInvalidInputBufferSize,[CurrentFrameIndex]));
        dsInvalidOutputBufferSize: AddError(Format(rsGIFInvalidOutputBufferSize,[CurrentFrameIndex]));
        dsBufferOverflow: AddError(Format(rsGIFBufferOverFlow,[CurrentFrameIndex]));
        dsOutputBufferTooSmall :
         (* begin
            // On supprime l'image. Le tampon de sortie étant trop petit, cela va générer des erreurs lors du transfert des données décompressées vers l'image
            //FFrames.Delete(CurrentFrameIndex);

          end;*)
          dec(CurrentFrameIndex);
      End;
	   if Ret<>dsOutputBufferTooSmall then
      begin
        Layers.Items[CurrentFrameIndex].IsCorrupted := True;
        Layers.Items[CurrentFrameIndex].DelayTime:= 1;;
      end;
    End;

    // On libére la mémoire allouée pour nos tampons
    if (TargetBufferSize>0) and (targetBuffer<>nil) then FreeMem(TargetBuffer);
    if (BufferSize>0) and (Buffer<>nil) then FreeMem(Buffer);


    FinishProgressSection(False);
  End;

Begin
  PaletteCount := 0;
  ColorCount := 0;
  LocalPalette := nil;
  SetSize(ImageDescription.Width, ImageDescription.Height);
  Layers.Clear;
  InitProgress(Width,Height);
  StartProgressSection(0, ''); // On debute une nouvelle section globale

  // Par defaut, on considère que la couleur de fond est totalement transparente
  Layers.BackgroundColor := clrTransparent;
  // Si une palette globale existe, alors on charge
  LoadGlobalPalette;
  {$IFDEF DEBUG} GlobalLogger.LogStatus('Background color Index = '+FLogicalScreenChunk.BackgroundColorIndex.ToString);{$ENDIF}
  If FHasGlobalPalette Then
  Begin
    If FLogicalScreenChunk.BackgroundColorIndex < PaletteCount - 1 Then Layers.BackgroundColor := FGlobalPalette.Colors[FLogicalScreenChunk.BackgroundColorIndex].Value
    Else
    Begin
      Layers.BackgroundColor := clrTransparent; //FGlobalPalette.Colors[FLogicalScreenChunk.BackgroundColorIndex].Value;
    End;
  End;

  // Les valeurs suivante seront renseignées lors du chargement d'une image
  // On réinitialise juste les valeurs par défaut des informations de l'image en cours au cas ou il n'y aurait pas de GCE
  With CurrentFrameInfos do
  begin
    Left       := 0;
    Top        := 0;
    Width      := FLogicalScreenChunk.ScreenWidth;
    Height     := FLogicalScreenChunk.ScreenHeight;
    Interlaced := False;
    HasLocalPalette := False;
    IsTransparent := false;
  End;
  // On ajoute l'image de départ afin de pouvoir assigner les valeurs des premières extensions (Extensions déclarées avant l'image)
  CurrentLayer := Layers.AddNewImage;
  CurrentFrameIndex := 0;
  // On lit le 1er octet
  Done := False;
  While not(Done) do
  begin
    // On verifie l'existence d'extensions avant les données de l'image (Application, Graphic Control, PlainText, Comment)
    if not(Memory.EOS) then BlockID := Memory.ReadByte else BlockID := GIF_Trailer;
    if (BlockID = GIF_Trailer)  then
    begin
      {$IFDEF DEBUG} GlobalLogger.LogStatus('GIF_Trailer-----------------------------------------------');{$ENDIF}
      Done := True;
    End;
    if (BlockID = 0) then
    begin
      // On Saute les ID Nul
      {$IFDEF DEBUG} GlobalLogger.LogStatus('NULL DATA-------------------------------------------------');{$ENDIF}
      While (BlockId = 0) do BlockId:=Memory.ReadByte;
    End
    else if (BlockID =  GIF_IMAGEDESCRIPTOR) then  // C'est une image
    begin
      // On charge l'image
      {$IFDEF DEBUG} GlobalLogger.LogStatus('GIF_IMAGEDESCRIPTOR---------------------------------------');{$ENDIF}
      LoadImage;
    End
    else if (BlockID = GIF_EXTENSIONINTRODUCER) then // c'est une extension
    begin
       {$IFDEF DEBUG} GlobalLogger.LogStatus('GIF_EXTENSIONINTRODUCER----------------------------------');{$ENDIF}
      ReadExtension; // On charge toutes les extensions qui sont à la suite
    End
    else
    begin
      // Extension inconnue on saute jusqu'a trouver un ZERO.
      // A Verifier avec le flag UseInput dans le "Graphic Control Extension"
      // Ici on ignore simplement les données
       {$IFDEF DEBUG} GlobalLogger.LogStatus('------> EXTENSION INCONNUE : '+Inttostr(BlockID));{$ENDIF}
      While BlockID<>0 do
      begin
        BlockID := Memory.ReadByte;
      End;
    End;
  End;
  // Si il y a des erreurs elles seront notifier à l'utilisateur
  NotifyError;
  // On doit effacer la derniere image créer en plus lors du traitement des données. Dans le cas ou le GIF contient plusieurs images
  //if (CurrentFrameIndex>1) then Layers.Delete(Layers.Count-1);
  {$IFDEF DEBUG} GlobalLogger.LogStatus('Total Layers : '+Layers.Count.ToString);{$ENDIF}


  // Il n'y a aucune images on notifie l'erreur
  if (Layers.Count = 0) then RaiseInvalidImageFile(rsEmptyImage);;

  ImageDescription.Assign(Layers.Items[0].Bitmap.ImageDescription);
  ImageDescription.FrameCount := Layers.Count;

  FinishProgressSection(False);

  // On libere la mémoire, prise par nos palettes de couleurs si besoin
  if (LocalPalette<>nil) then FreeAndNil(LocalPalette);
  if (FGlobalPalette<>nil) then FreeAndNil(FGlobalPalette);
  // On efface l'image
  //clear(Layers.BackgroundColor);
  // On assigne la 1ere image
  PutImage(Layers.Items[0].Bitmap,0,0,Width, Height,0,0);
End;

{%endregion%}

Initialization

  RegisterRasterFormat('gif', 'Graphics Interchange Format', TBZBitmapGIFImage);

Finalization

  UnregisterRasterFormat(TBZBitmapGIFImage);

end.


