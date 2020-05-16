(*
  @abstract(Prise en charge des Images au format WEBP proposé par Google.)

  Spécifications : @br
  @unorderedList(
    @item(Version librairie         : 1.0.3 )
    @item(Méthode de compression    : VP8 or VP8L )
    @item(Nombre de couleurs	       : 24 / 32 bits  : (RGB/A, BGR/A, YUV) )
    @item(Supporte plusieurs images : Non )
    @item(Format des nombres	       : - )
    @item(Auteur	                   : Google)
    @item(Extensons                 : *.webp; ( *.webm = video) )
  )

  -------------------------------------------------------------------------------------------------------------

  @created(2020-02-06)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(06/02/2020 : Creation  )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) :@br
  Basé sur la bibliothèque externe "LiWebp"

  Informations sur le format WEBP : @br
  @unorderedList(
     @item(https://developers.google.com/speed/webp)
     @item(https://en.wikipedia.org/wiki/WebP)
     @item(https://www.webmproject.org/code/#libwebp-webp-image-library)
     @item(https://developers.google.com/speed/webp/download)
  )

  Fichiers test :@br
    - https://developers.google.com/speed/webp/gallery1

  -------------------------------------------------------------------------------------------------------------

  @bold(Dépendances) :

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)
    - Tous les liens au dessus

  -------------------------------------------------------------------------------------------------------------

  @bold(Licence) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZImageFileWEBP;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils,
  BZClasses, BZColors, BZGraphic, BZBitmap, BZImageFileIO;

{ TODO 1 -oBZBitmap -cSupport_Image_WEBP : Sauvegarde au format WebP }

type
  TBZWebPHeader = record
    RIFFCode: array[1..4] of char;
    FileSize: DWord;
    WebPCode: array[1..4] of char;
  end;

  TBZWebPPreset = (
    WEBP_PRESET_CUSTOM,       // Options de sauvegarde personalisées
    WEBP_PRESET_DEFAULT,      // Options par défaut
    WEBP_PRESET_LOSSLESS,
    WEBP_PRESET_PICTURE,      // Image digital. Comme portrait, plan intérieur
    WEBP_PRESET_PHOTO,        // Photographie extérieur avec lumière naturelle
    WEBP_PRESET_DRAWING,      // Dessin à la main ou au trait, avec des détails très contrastés
    WEBP_PRESET_ICON,         // Images colorées de petite taille
    WEBP_PRESET_TEXT          // Texte
  );

  TBZWEBP_MethodeRange = 0..6;
  TBZWEBP_SharpnessRange = 0..7;
  TBZWEBP_SegmentRange = 1..4;
  TBZWEBP_PartitionRange = 0..3;
  TBZWEBP_EntropyAnalysisPassesRange = 1..10;
  TBZWEBP_FilterType = (wftSimple, wftStrong);


Type

  { TBZBitmapWEBPImage }

  TBZBitmapWEBPImage = Class(TBZCustomImageFileIO)
  private
    FHeader : TBZWebPHeader;
    FBmpWidth, FBmpHeight : Integer;
    FTotalSize : DWord;


    //FQualityPercent : Byte;
    //
    //FMethod : TBZWEBP_MethodeRange;
    //FSegments : TBZWEBP_SegmentRange;
    //
    //FFitlerType : TBZWEBP_FilterType;
    //FFilterSharpness : TBZWEBP_SharpnessRange;
    //FFilterStrengthPercent : Integer;
    //FAutoAdjustFilterStrength : Boolean;
    //FPreprocessingFilter : Boolean;
    //
    //FSpatialNoiseShapingPercent : Integer;
    //FPartitions : TBZWEBP_PartitionRange;
    //FEntropyAnalysisPasses : TBZWEBP_EntropyAnalysisPassesRange;
    //
    //FTargetSize: Integer;       // if non-zero, set the desired target size in bytes.
    //                            // Takes precedence over the 'compression' parameter.
    //FTargetPSNR: Single;        // if non-zero, specifies the minimal distortion
    //
    //FSavingOptionsType : TBZWebPPreset;

  protected
    MemStream: TMemoryStream;

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
  BZImageStrConsts, BZLibWEBP{$ifdef linux}, linux, xlib{$endif},
  BZUtils;

var
  LibWebPIsLoaded: boolean;

procedure LoadLibWebP;
begin
  if not LibWebPIsLoaded then
  begin
    //{$ifdef linux}FindLinuxLibrary(LibWebPFilename, 6){$endif}
    if not LibWebPLoad() then
      raise exception.Create('Cannot find libwebp library ('+LibWebPFilename+')');
    LibWebPIsLoaded:= true;
  end;
end;

{ TBZBitmapWEBPImage }

Constructor TBZBitmapWEBPImage.Create(AOwner: TPersistent; AWidth, AHeight: Integer);
Begin
  Inherited Create(AOwner, AWidth, AHeight);
  With DataFormatDesc Do
  Begin
    Name := 'WEBP';
    Desc := 'Goolgle WebP';  // , icone, curseur';
    FileMask := '*.webp'; //*.ico;*.cur;
    Version := '';
    Encoding := etVP8;
  End;

  SupportedColorFormat := SupportedColorFormat + [cfBGR, cfRGB, cfRGBA, cfBGRA];
End;

Destructor TBZBitmapWEBPImage.Destroy;
Begin
  SupportedColorFormat := [];
  Inherited Destroy;
End;

Procedure TBZBitmapWEBPImage.LoadFromMemory();
Var
  Ok, IgnoreAlpha : Boolean;
  //TmpBuf : PByte;
begin
  IgnoreAlpha := False;
  Self.SetSize(FBmpWidth, FBmpHeight);
  {$PUSH}{$WARNINGS OFF}
   {.$IFDEF UNIX}
    //ok := WebPDecodeRGBAInto(Memory.GetBuffer, FTotalSize, PByte(Self.getSurfaceBuffer), loadInto.RowSize*h, loadInto.RowSize)<>nil;
  {.$ELSE}
    //TmpBuf :=
    Ok := WebPDecodeBGRAInto(Memory.GetBuffer, FTotalSize, PByte(Self.getSurfaceBuffer), Self.Size, Self.Width * 4) <> nil;
    //Self.ImageDescription.RowStrideSize
    //Self.ImageDescription.BytesPerLine)<>nil;
  {.$ENDIF}
  {$POP}
  if not(Ok) then RaiseInvalidImageFile(rsFileDecodingError);

  ImageDescription.HasAlpha := Self.CheckIfTransparent(IgnoreAlpha);
  if IgnoreAlpha then Self.MakeOpaque;
  //if Ok then FreeMem(TmpBuf);
end;

Procedure TBZBitmapWEBPImage.SaveToMemory;
begin
  inherited SaveToMemory;
end;

Function TBZBitmapWEBPImage.CheckFormat() : Boolean;
begin
  Result := False;
  Memory.Read(FHeader{%H-}, SizeOf(TBZWebPHeader));
  if (FHeader.RIFFCode = 'RIFF') or (FHeader.WebPCode = 'WEBP') then
  begin
    FHeader.FileSize := LEToN(FHeader.FileSize);
    if (FHeader.FileSize > $FFFFFFF6) and (FHeader.FileSize > Memory.Size - Memory.Position+4) then
    begin
      Result := False;
      RaiseInvalidImageFile(Format(rsBadFileSize,[FormatByteSize(FHeader.FileSize)]));
    end;

    Result := ReadImageProperties;
  end
  else
  begin
    Result := False;
    RaiseInvalidImageFile(Format(rsBadSignature,[String(FHeader.RIFFCode)]));
  end;
end;

Function TBZBitmapWEBPImage.ReadImageProperties : Boolean;
Var
  DecoderVer : Longint;
  minVer, majVer, buildVer : Byte;
begin
  Result := True;
  FTotalSize := FHeader.FileSize+8;
  LoadLibWebP;
  if WebPGetInfo(Memory.GetBuffer, FTotalSize, @FBmpWidth, @FBmpHeight) = 0 then
  begin
    Result := False;
    RaiseInvalidImageFile(Format(rsBadImageSize,[FBmpWidth,FBmpHeight]));
  end;
  DecoderVer := WebPGetDecoderVersion();

  majVer  := (DecoderVer shr 16) and $000000ff;
  minVer := (DecoderVer shr 8) and $000000ff;
  buildVer   := DecoderVer and $000000ff;

  With DataFormatDesc do
  begin
    Version := MajVer.ToString +'.' + MinVer.ToString + '.' + BuildVer.ToString;
  end;
  ImageDescription.InitDefault(FBmpWidth, FBmpHeight, 32);


end;

Class Function TBZBitmapWEBPImage.Capabilities : TBZDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

Initialization

  RegisterRasterFormat('webp', 'Webp Image format', TBZBitmapWEBPImage);

Finalization

  if LibWebPIsLoaded then
  begin
    LibWebPUnload;
    LibWebPIsLoaded:= false;
  end;

  UnregisterRasterFormat(TBZBitmapWEBPImage);

end.

