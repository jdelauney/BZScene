(*
  @abstract(Prise en charge des images au format JPEG en lecture et écriture.)

  Spécifications : @br
  @unorderedList(
    @item(Méthode de compression    :
    @item(Nombre de couleurs	      : 24 bits
    @item(Supporte plusieurs images : Non
    @item(Format des nombres	      : Big-endian
    @item(Auteur	                  : JPEG Group
    @item(Extensons                 : *.jpg; *.jfif; *.jpeg; *.jpe
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
  Basé sur la bibliothèque PasJPEG de FPC et les unités FPReadJPEG et FPWriteJPG du package FCL-Image

  Informations sur le format JPEG : @br
  @unorderedList(
    @item(https://en.wikipedia.org/wiki/JPEG)
    @item(https://jpeg.org/jpeg/index.html)
    @item(https://www.w3.org/Graphics/JPEG/jfif3.pdf)
    @item(https://en.wikipedia.org/wiki/JPEG_File_Interchange_Format)
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Dépendances :)

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits) : @br
    - Tous les liens au dessus

  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
Unit BZImageFileJPEG;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils,
  BZClasses, BZColors, BZGraphic, BZBitmap, BZImageFileIO,
  JPEGLib, JdAPImin, JcAPImin, JDataSrc, JDataDst, JdAPIstd, JcAPIstd,
  JcParam, JError;

Type
  TBZJPEGQualityRange = 0..100;
  PBZJPEGProgressManager = ^TBZJPEGProgressManager;
  TBZJPEGProgressManager = record
    pub : jpeg_progress_mgr;
    instance: TObject;
    last_pass: Integer;
    last_pct: Integer;
    last_time: Integer;
    last_scanline: Integer;
  end;

  TBZJPEGScale = (jsFullSize, jsHalf, jsQuarter, jsEighth);
  TBZJPEGReadPerformance = (jpBestQuality, jpBestSpeed);

Type

  { TBZBitmapJPEGSavingOptions }

  TBZBitmapJPEGSavingOptions = Class(TBZUpdateAbleObject)
  private
    FQuality : TBZJPEGQualityRange;
    FGrayscale: boolean;
    FProgressiveEncoding: boolean;
  public
    Constructor Create; override;

    property GrayScale: boolean read FGrayscale;
    property ProgressiveEncoding: boolean read FProgressiveEncoding;
    property Quality : TBZJPEGQualityRange read FQuality write FQuality;
  end;

  { TBZBitmapJPEGImage }

  TBZBitmapJPEGImage = Class(TBZCustomImageFileIO)
  private
    FSavingOptions :TBZBitmapJPEGSavingOptions;

    FSmoothing: boolean;
    FMinHeight:integer;
    FMinWidth:integer;

    FGrayscale: boolean;
    FProgressiveEncoding: boolean;
    FScale: TBZJPEGScale;
    FPerformance: TBZJPEGReadPerformance;
    FQuality: TBZJPEGQualityRange;

    FError: jpeg_error_mgr;
    FProgressMgr: TBZJPEGProgressManager;
    FInfo: jpeg_decompress_struct;
    FSavingInfo: jpeg_compress_struct;
    bmpWidth, bmpHeight : Integer;

    procedure SetPerformance(const AValue: TBZJPEGReadPerformance);
    procedure SetSmoothing(const AValue: boolean);
  protected
    MemStream: TMemoryStream;

    Procedure LoadFromMemory(); override;
    procedure SaveToMemory(); override;
    Function CheckFormat(): Boolean; override;
    Function ReadImageProperties: Boolean; override;
  public

    Constructor Create(AOwner: TPersistent; AWidth, AHeight: Integer); override;
    Destructor Destroy; override;

    Class Function Capabilities: TBZDataFileCapabilities; override;

    Function getImagePropertiesAsString: String; override;

    property GrayScale: boolean read FGrayscale;
    property ProgressiveEncoding: boolean read FProgressiveEncoding;
    property Smoothing: boolean read FSmoothing write SetSmoothing;
    property Performance: TBZJPEGReadPerformance read FPerformance write SetPerformance;
    property Scale: TBZJPEGScale read FScale write FScale;
    property MinWidth:integer read FMinWidth write FMinWidth;
    property MinHeight:integer read FMinHeight write FMinHeight;
    property CompressionQuality: TBZJPEGQualityRange read FQuality write FQuality;
  end;

implementation

Uses
  BZUtils, jDefErr
  //{$IFDEF DEBUG}
  , Dialogs, BZLogger;
  //{$ENDIF};

var
  jpeg_std_error: jpeg_error_mgr;

procedure JPEGError(CurInfo: j_common_ptr);
Var
  ErrCode : Integer;
begin
  if CurInfo=nil then exit;
  ErrCode := CurInfo^.err^.msg_code;
  raise EInvalidImageFile.Create('Erreur lors du chargement du fichier JPEG.'+#13+#10+
                                 Format('Erreur  : #%d',[ErrCode])+#13+#10+
                                 Format('Message : %s',[jpeg_std_message_table[J_MESSAGE_CODE(ErrCode)]]));
end;

procedure EmitMessage(CurInfo: j_common_ptr; msg_level: Integer);
begin
  if CurInfo=nil then exit;
  if msg_level=0 then ;
end;

procedure OutputMessage(CurInfo: j_common_ptr);
begin
  if CurInfo=nil then exit;
end;

procedure FormatMessage(CurInfo: j_common_ptr; var buffer: string);
begin
  if CurInfo=nil then exit;
end;

procedure ResetErrorMgr(CurInfo: j_common_ptr);
begin
  if CurInfo=nil then exit;
  CurInfo^.err^.num_warnings := 0;
  CurInfo^.err^.msg_code := 0;
end;

procedure ProgressCallback(CurInfo: j_common_ptr);
begin
  if CurInfo=nil then exit;
  // ToDo
end;

{ TBZBitmapJPEGSavingOptions }

Constructor TBZBitmapJPEGSavingOptions.Create;
begin
  inherited Create;
  FGrayscale := False;
  FProgressiveEncoding := False;
  FQuality := 75;
end;

{%region%=====[ TBZBitmapJPEGImage ]============================================}

Constructor TBZBitmapJPEGImage.Create(AOwner: TPersistent; AWidth, AHeight: Integer);
Begin
  Inherited Create(aOwner, AWidth, AHeight);
  With DataFormatDesc Do
  Begin
    Name := 'JPEG';
    Desc := ' Joint Photographic Experts Group';
    FileMask := '*.jpg;*.jpeg;*.jpe;*.jfif';
    Version := '1.0';
    Encoding := etJPEG;
  End;

  SupportedColorFormat := SupportedColorFormat + [cfRGB];

  FScale:= jsFullSize;
  FPerformance:= jpBestQuality;
  FGrayScale := False;
  FSavingOptions := TBZBitmapJPEGSavingOptions.Create;
  FQuality := 75;
end;

Destructor TBZBitmapJPEGImage.Destroy;
Begin
  SupportedColorFormat := [];
  FreeAndNil(FSavingOptions);
  Inherited Destroy;
End;

Class Function TBZBitmapJPEGImage.Capabilities: TBZDataFileCapabilities;
Begin
  Result := [dfcRead, dfcWrite];
End;

Function TBZBitmapJPEGImage.getImagePropertiesAsString: String;
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

procedure TBZBitmapJPEGImage.SetSmoothing(const AValue: boolean);
begin
  if FSmoothing=AValue then exit;
  FSmoothing:=AValue;
end;

procedure TBZBitmapJPEGImage.SetPerformance(const AValue: TBZJPEGReadPerformance);
begin
  if FPerformance=AValue then exit;
  FPerformance:=AValue;
end;

Function TBZBitmapJPEGImage.ReadImageProperties: Boolean;
Var
  Src, Dst : PByte;

  procedure InitReadingPixels;
  var d1,d2:integer;

    function DToScale(inp:integer):TBZJPEGScale;
    begin
      if inp>7 then Result:=jsEighth else
      if inp>3 then Result:=jsQuarter else
      if inp>1 then Result:=jsHalf else
      Result:=jsFullSize;
    end;

  begin
    FInfo.scale_num := 1;

    if (FMinWidth>0) and (FMinHeight>0) then
      if (FInfo.image_width>FMinWidth) or (FInfo.image_height>FMinHeight) then
        begin
        d1:=Round((FInfo.image_width / FMinWidth)-0.5);
        d2:=Round((FInfo.image_height /  FMinHeight)-0.5);
        if d1>d2 then fScale:=DToScale(d2) else fScale:=DtoScale(d1);
        end;

    FInfo.scale_denom :=1 shl Byte(FScale); //1
    FInfo.do_block_smoothing := FSmoothing;

    if FGrayscale then FInfo.out_color_space := JCS_GRAYSCALE;
    if (FInfo.out_color_space = JCS_GRAYSCALE) then
    begin
      FInfo.quantize_colors := True;
      FInfo.desired_number_of_colors := 256;
    end;

    if FPerformance = jpBestSpeed then
    begin
      FInfo.dct_method := JDCT_IFAST;
      FInfo.two_pass_quantize := False;
      FInfo.dither_mode := JDITHER_ORDERED;
      // FInfo.do_fancy_upsampling := False;  can create an AV inside jpeglib
    end;

    if FProgressiveEncoding then
    begin
      FInfo.enable_2pass_quant := FInfo.two_pass_quantize;
      FInfo.buffered_image := True;
    end;
  end;

Begin
  Result:=True;
  MemStream := nil;
  Memory.Seek(0,soBeginning);
  MemStream := TMemoryStream.Create;
  MemStream.SetSize(Memory.Size);


 // Globallogger.LogStatus('MemStream Size = '+inttostr(MemStream.Size));
 // Globallogger.LogStatus('Memory    Size = '+inttostr(Memory.Size));
 (* GetMem(Src,Memory.Size);
  Src := nil;
  Src := Memory.GetBuffer;
  Move(Src^,MemStream.Memory^,Memory.Size);

  FreeMem(Src); *)
  Memory.Seek(0,soBeginning);
  Memory.Read(MemStream.Memory^, Memory.Size);
  MemStream.Position:=0;

  FillChar(FInfo,SizeOf(FInfo),0);

  FError:=jpeg_std_error;
  FInfo.err := @FError;
  jpeg_CreateDecompress(@FInfo, JPEG_LIB_VERSION, SizeOf(FInfo));
  FProgressMgr.pub.progress_monitor := @ProgressCallback;
  FProgressMgr.instance := Self;
  FInfo.progress := @FProgressMgr.pub;

  jpeg_stdio_src(@FInfo, @MemStream);
  jpeg_read_header(@FInfo, TRUE);

  bmpWidth := FInfo.image_width;
  bmpHeight := FInfo.image_height;
  FGrayscale := FInfo.jpeg_color_space = JCS_GRAYSCALE;
  FProgressiveEncoding := jpeg_has_multiple_scans(@FInfo);

  // On sauvegarde en local les dimensions de l'image
  With ImageDescription Do
  Begin
    // On initialise la descritption du "ImageDescription"
    InitDefault(bmpWidth, bmpHeight,24);
    Init(cfRGB,pf24bits);
    HasAlpha := False;
    // On Indique qu'il n'y a pas de padding de fin de ligne
    RowStrideType:=bleNoBoundary;
    LineOrder := bloTopToBottom
  end;

  InitReadingPixels;
  Result := (MemStream.Size>0);
End;

Function TBZBitmapJPEGImage.CheckFormat(): Boolean;
Var
  Magic : Word;
Begin
  Result := False;
  Magic := Memory.ReadWord;
  Result := Magic = NtoLE($D8FF);
  if Result then
  begin
     Result := ReadImageProperties;
  end;
End;

Procedure TBZBitmapJPEGImage.LoadFromMemory();
var
  SampArray: JSAMPARRAY;
  SampRow: JSAMPROW;
  Color: TBZColor;
  LinesRead: Cardinal;
  y: Integer;
  c: word;
  Status,Scan: integer;
  ReturnValue,RestartLoop: Boolean;
  Delta : Single;

  function CorrectColor(const C: TBZColor): TBZColor;
  var
    MinColor: word;
  begin
    if C.red<C.green then MinColor:=C.red else MinColor:= C.green;
    if C.blue<MinColor then MinColor:= C.blue;
    if (MinColor + C.alpha)>$FF then MinColor:=$FF-C.alpha;
    Result.red:=(C.red-MinColor);
    Result.green:=(C.green-MinColor);
    Result.blue:=(C.blue-MinColor);
    Result.alpha:=255;
  end;

  procedure OutputScanLines();
  var
    x: integer;
  begin
    Color.Alpha:=255;
    y:=0;
    while (FInfo.output_scanline < FInfo.output_height) do
    begin
      // read one line per call
      LinesRead := jpeg_read_scanlines(@FInfo, SampArray, 1);
      if LinesRead<1 then
      begin
        ReturnValue:=false;
        break;
      end;

      if (FInfo.jpeg_color_space = JCS_CMYK) then
      for x:=0 to FInfo.output_width-1 do
      begin
        Color.Red:=SampRow^[x*4+0];
        Color.Green:=SampRow^[x*4+1];
        Color.Blue:=SampRow^[x*4+2];
        Color.alpha:=SampRow^[x*4+3];
        Pixels[x,y]:=CorrectColor(Color);
      end
      else if (FInfo.jpeg_color_space = JCS_YCCK) then
      begin
        for x:=0 to FInfo.output_width-1 do
        begin
          Color.Red:=SampRow^[x*4+0];
          Color.Green:=SampRow^[x*4+1];
          Color.Blue:=SampRow^[x*4+2];
          Color.alpha:=SampRow^[x*4+3];
          Pixels[x,y]:=CorrectColor(Color);
        end;
      end
      else if fgrayscale then
      begin
       for x:=0 to FInfo.output_width-1 do
       begin
         c:= SampRow^[x];
         Color.Red:=c;
         Color.Green:=c;
         Color.Blue:=c;
         Color.Alpha := 255;
         Pixels[x,y]:=Color;
       end;
      end
      else
      begin
        for x:=0 to FInfo.output_width-1 do
        begin
         Color.Red:=SampRow^[x*3+0];
         Color.Green:=SampRow^[x*3+1];
         Color.Blue:=SampRow^[x*3+2];
         Color.Alpha:= 255;
         Pixels[x,y]:=Color;
        end;
      end;
      inc(y);
      AdvanceProgress(Delta,0,1,False);
    end;
  end;

Begin
  jpeg_start_decompress(@FInfo);

  bmpWidth := FInfo.output_width;
  bmpHeight := FInfo.output_height;

  GetMem(SampArray,SizeOf(JSAMPROW));
  GetMem(SampRow,FInfo.output_width*FInfo.output_components);
  SampArray^[0]:=SampRow;

  SetSize(bmpWidth, bmpHeight);

  InitProgress(bmpWidth,bmpHeight);
  StartProgressSection(0, ''); // On debute une nouvelle section globale
  Delta := 100 / Height;
  StartProgressSection(100 ,'Chargement de l''image');

  try
    case FProgressiveEncoding of
      false:
      begin
        ReturnValue:=true;
        OutputScanLines();
        if FInfo.buffered_image then jpeg_finish_output(@FInfo);
      end;
      true:
      begin
        while true do
        begin
          (* The RestartLoop variable drops a placeholder for suspension
             mode, or partial jpeg decode, return and continue. In case
             of support this suspension, the RestartLoop:=True should be
             changed by an Exit and in the routine enter detects that it
             is being called from a suspended state to not
             reinitialize some buffer *)
          RestartLoop:=false;
          repeat
            status := jpeg_consume_input(@FInfo);
          until (status=JPEG_SUSPENDED) or (status=JPEG_REACHED_EOI);
          ReturnValue:=true;
          if FInfo.output_scanline = 0 then
          begin
            Scan := FInfo.input_scan_number;
            (* if we haven't displayed anything yet (output_scan_number==0)
              and we have enough data for a complete scan, force output
              of the last full scan *)
            if (FInfo.output_scan_number = 0) and (Scan > 1) and (status <> JPEG_REACHED_EOI) then Dec(Scan);

            if not jpeg_start_output(@FInfo, Scan) then
            begin
              RestartLoop:=true; (* I/O suspension *)
            end;
          end;

          if not RestartLoop then
          begin
            if (FInfo.output_scanline = $ffffff) then FInfo.output_scanline := 0;

            OutputScanLines();

            if ReturnValue=false then begin
              if (FInfo.output_scanline = 0) then
              begin
                 (* didn't manage to read any lines - flag so we don't call
                    jpeg_start_output() multiple times for the same scan *)
                 FInfo.output_scanline := $ffffff;
              end;
              RestartLoop:=true; (* I/O suspension *)
            end;

            if not RestartLoop then
            begin
              if (FInfo.output_scanline = FInfo.output_height) then
              begin
                if not jpeg_finish_output(@FInfo) then
                begin
                  RestartLoop:=true; (* I/O suspension *)
                end;

                if not RestartLoop then
                begin
                  if (jpeg_input_complete(@FInfo) and
                     (FInfo.input_scan_number = FInfo.output_scan_number)) then
                    break;

                  FInfo.output_scanline := 0;
                end;
              end;
            end;
          end;
          if RestartLoop then
          begin
            (* Suspension mode, but as not supported by this implementation
               it will simple break the loop to avoid endless looping. *)
            break;
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(MemStream);
    FreeMem(SampArray);
    SampArray := nil;
    FreeMem(SampRow);
    SampRow := nil;
    jpeg_finish_decompress(@FInfo);
    jpeg_destroy_decompress(@FInfo);
//    jpeg_finish_output
  end;
  //
  FinishProgressSection(False);
  FinishProgressSection(True);
End;

procedure TBZBitmapJPEGImage.SaveToMemory();
var
  Delta : Single;

  procedure SaveHeader;
  begin
    FSavingInfo.image_width := Self.Width;
    FSavingInfo.image_height := Self.Height;
    if FSavingOptions.Grayscale then
    begin
      FSavingInfo.input_components := 1;
      FSavingInfo.in_color_space := JCS_GRAYSCALE;
    end
    else
    begin
      FSavingInfo.input_components := 3; // RGB has 3 components
      FSavingInfo.in_color_space := JCS_RGB;
    end;

    jpeg_set_defaults(@FSavingInfo);
    jpeg_set_quality(@FSavingInfo, FSavingOptions.Quality, True);

    if FSavingOptions.ProgressiveEncoding then jpeg_simple_progression(@FSavingInfo);
  end;

  procedure SaveData;
  Var
    LinesWritten: Cardinal;
    SampArray: JSAMPARRAY;
    SampRow: JSAMPROW;
    Color: TBZColor;
    x: Integer;
    y: Integer;
  begin
    jpeg_start_compress(@FSavingInfo, True);

    // write one line per call
    GetMem(SampArray,SizeOf(JSAMPROW));
    GetMem(SampRow,FSavingInfo.image_width*FSavingInfo.input_components);
    SampArray^[0]:=SampRow;
    try
      y:=0;
      while (FSavingInfo.next_scanline < FSavingInfo.image_height) do
      begin
        if FSavingOptions.Grayscale then
        begin
          for x:=0 to FSavingInfo.image_width-1 do
          begin
            SampRow^[x]:= Self.getPixel(x,y).Luminance;
          end;
        end
        else
        begin
          for x:=0 to FSavingInfo.image_width-1 do
          begin
            Color:=Self.getPixel(x,y);
            SampRow^[x*3+0]:=Color.Red;
            SampRow^[x*3+1]:=Color.Green;
            SampRow^[x*3+2]:=Color.Blue;
          end;
        end;
        LinesWritten := jpeg_write_scanlines(@FSavingInfo, SampArray, 1);
        if LinesWritten<1 then break;
        inc(y);
        AdvanceProgress(Delta,0,1,False);
      end;
    finally
      FreeMem(SampRow);
      SampRow := nil;
      FreeMem(SampArray);
      SampArray := nil;
      jpeg_finish_compress(@FSavingInfo);
      jpeg_destroy_compress(@FSavingInfo);
    end;
  end;

begin
  InitProgress(Self.Width,Self.Height);
  StartProgressSection(0, ''); // On debute une nouvelle section globale
  Delta := 100 / Self.Height;
  StartProgressSection(100 ,'Enregistrement de l''image au format JPEG');

  FillChar(FSavingInfo, sizeof(FSavingInfo), 0);
  FError := jpeg_std_error;
  FSavingInfo.err := jerror.jpeg_std_error(FError);

  jpeg_create_compress(@FSavingInfo);
  FProgressMgr.pub.progress_monitor := @ProgressCallback;
  FProgressMgr.instance := Self;
  FSavingInfo.progress := @FProgressMgr.pub;

  MemStream := TMemoryStream.Create;
  jpeg_stdio_dest(@FSavingInfo, @MemStream);

  SaveHeader;
  SaveData;

  MemStream.Position := 0;
  Memory.Write(MemStream.Memory^,MemStream.Size);

  FreeAndNil(MemStream);

  FinishProgressSection(False);
  FinishProgressSection(True);
end;

{%endregion%}

Initialization

  with jpeg_std_error do
  begin
    error_exit:=@JPEGError;
    emit_message:=@EmitMessage;
    output_message:=@OutputMessage;
    format_message:=@FormatMessage;
    reset_error_mgr:=@ResetErrorMgr;
  end;

  RegisterRasterFormat('jpg', 'JPEG Graphics', TBZBitmapJPEGImage);
  RegisterRasterFormat('jpeg', 'JPEG Graphics', TBZBitmapJPEGImage);
  RegisterRasterFormat('jpe', 'JPEG Graphics', TBZBitmapJPEGImage);
  RegisterRasterFormat('jfif', 'JPEG Graphics', TBZBitmapJPEGImage);

Finalization
  UnregisterRasterFormat(TBZBitmapJPEGImage);


end.

