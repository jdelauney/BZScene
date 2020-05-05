(*
  @abstract( Similaire TBZCustomBitmapScanner, mais spécialisé dans le rendu
  de "Shader" alla GLSL mais en software via TBZBitmap.)

  --------------------------------------------------------------------------------

  @created(2018-07-11)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(09/02/2018 : Creation)
  )

  --------------------------------------------------------------------------------

  @bold(Notes) :

  --------------------------------------------------------------------------------

  @bold(Dependances) : BZClasses, BZVectorMath, BZColors, BZGraphic, BZBitmap, BZCustomShader, BZParallelThread

  --------------------------------------------------------------------------------

  @bold(Credits :)
   @unorderedList(
     @item (basé sur GLScene http://www.sourceforge.net/glscene))
     @item(J.Delauney (BeanzMaster))
   )

  --------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  -------------------------------------------------------------------------------- *)

unit BZBitmapRasterizer;  //BZBitmapShaderRasterizer

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}

//------------------------
{$ALIGN 16}
{$CODEALIGN CONSTMIN=16}
{$CODEALIGN LOCALMIN=16}
{$CODEALIGN VARMIN=16}
//------------------------
//==============================================================================

interface

uses
  Classes, SysUtils, dialogs, syncobjs,
  BZClasses, BZVectorMath, BZColors, BZGraphic, BZBitmap, BZCustomShader,
  BZParallelThread;

Type
  { Paramètres du SHader sofware }
  TBZRasterizerParameters = packed record
    StartX, EndX : LongWord;
    DeltaProgress : Single;
    Surface : TBZBitmap;
    // Shader : TBZShader;
    // Area : TBZRect;
  end;
  { Pointeur vers un TBZRasterizerParameters}
  PBZRasterizerParameters = ^TBZRasterizerParameters;

  { Classe de base à hériter pour le rendu de shader en software dans un TBZBitmap }
  TBZCustomBitmapRasterizer = class(TBZProgressAbleObject)
  private
    {$CODEALIGN RECORDMIN=16}
    FShader : TBZCustomSoftwareShader;
    {$CODEALIGN RECORDMIN=4}

    procedure SetShader(AValue: TBZCustomSoftwareShader);
  protected
    procedure DoRasterize(Dst: TBZBitmap; DstRect: TBZRect); virtual; abstract;

    procedure InternalRasterize(Dst: TBZBitmap); overload;
    procedure InternalRasterize(Dst: TBZBitmap; const DstRect: TBZRect); overload;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
    procedure Rasterize;virtual;abstract;
  published
    property Shader :  TBZCustomSoftwareShader read FShader Write SetShader;
  end;

  {Classe spécialisée dans le rendu de shader en software }
  TBZBitmapShaderRasterizer = Class(TBZCustomBitmapRasterizer)
  private
    FUpdateRowCount: Integer;
    //FTextureSamples : Array of TBZBitmap
    //FTextureCount : Integer;
    FBuffer: TBZBitmap;
    //CS : TCriticalSection;
    procedure SetBuffer(Const AValue: TBZBitmap);
  protected
    FInternalRCI : PBZRenderingContextInfos;

    procedure DoRasterizeLineProc(Sender: TObject; Index: Integer ; Data : Pointer);
    procedure DoRasterize(Dst: TBZBitmap; DstRect: TBZRect); override;
  public
    constructor Create; override;
    Destructor Destroy; override;
    procedure Rasterize;override;
    property UpdateRowCount: Integer read FUpdateRowCount write FUpdateRowCount;
  published
    property Shader;
    property Buffer:TBZBitmap read FBuffer Write SetBuffer;
  end;

  { Paramètres pour le rendu dans les threads }
  TBZThreadShaderSettings = packed record
    Shader    : TBZCustomSoftwareShader;
    Area      : TBZRect;
    Buffer    : TBZBitmap;
  end;
  { Liste de paramètres pour les threads (Max 4)}
  TBZRasterShaderRenders = array[0..3] of TBZThreadShaderSettings;

  {Classe spécialisée dans le rendu de shader en software. @br
  @bold(Note) : Afin d'optimiser la vitesse du rendu, celui s'effectue dans des threads}
  TBZBitmapShaderThreadRasterizer = class(TBZBitmapShaderRasterizer)
  private
    FRenderThreads : Array [0..3] of TBZThreadShaderSettings;
  protected
    procedure DoRasterizeProc(Sender: TObject; Index: Integer ; Data : Pointer);
    procedure MergeBitmapToBuffer(aBmp :TBZBitmap; aRect : TBZRect);
  public
    constructor Create(aBuffer : TBZBitmap; aShader:TBZCustomSoftwareShader);reintroduce;
    Destructor Destroy;Override;
    procedure Rasterize;override;

  published
    property Shader;
    property Buffer;
  end;

implementation

uses BZLogger;

{%region%=====[ TBZBitmapShaderThreadRasterizer ]==============================}

constructor TBZBitmapShaderThreadRasterizer.Create(aBuffer : TBZBitmap; aShader : TBZCustomSoftwareShader);
Var
  i : Byte;
  w,h : Integer;
begin
  inherited Create;

  Buffer := aBuffer;
  Shader := aShader;

  w := Buffer.Width div 2;
  h := Buffer.Height div 2;
  FInternalRCI^.ViewPortSize.Create(Buffer.Width, Buffer.Height);

  For i := 0 to 3 do
  begin

    FRenderThreads[i].Shader := aShader.Clone;
    FRenderThreads[i].Buffer := TBZBitmap.Create(w,h);

    Case i of
      0 : FRenderThreads[0].Area.Create(0, 0, w-1, h-1);
      1 : FRenderThreads[1].Area.Create(w, 0, Buffer.MaxWidth, h-1);
      2 : FRenderThreads[2].Area.Create(0, h, w-1, Buffer.MaxHeight);
      3 : FRenderThreads[3].Area.Create(w, h, Buffer.MaxWidth, Buffer.MaxHeight);
    end;
  end;
end;

Destructor TBZBitmapShaderThreadRasterizer.Destroy;
var
  i : byte;
begin
  For i := 3 downto 0 do
  begin
    FreeAndNil(FRenderThreads[i].Buffer);
    FreeAndNil(FRenderThreads[i].Shader);
  end;

  inherited Destroy;
end;

procedure TBZBitmapShaderThreadRasterizer.DoRasterizeProc(Sender : TObject; Index : Integer; Data : Pointer);
var
  MaxS, LineWidth, i,xx,yy,
  mw,mh,hh,th :Longword;
  C : TBZColor;
  DstPix : PBZColor;
  OutColor : TBZColor;
  //{$CODEALIGN VARMIN=16}
  //cv : TBZColorVector;
  //{$CODEALIGN VARMIN=4}
begin

 // GlobalLogger.LogStatus('TBZBitmapShaderThreadRasterizer.DoRasterizeProc : ' + Index.ToString);
 // GlobalLogger.LogStatus('Area = ' + TBZRasterShaderRenders(Data^)[Index].Area.ToString);

  xx := TBZRasterShaderRenders(Data^)[Index].Area.Left;
  yy := TBZRasterShaderRenders(Data^)[Index].Area.Bottom;

  // TODO : A placer dans vars globales
  mh := TBZRasterShaderRenders(Data^)[Index].Area.Height;
  th := mh-1;
  mw := TBZRasterShaderRenders(Data^)[Index].Area.Width;
  Maxs := mw*mh-1;
  hh := FInternalRCI^.ViewPortSize.Height - 1;

  i := 0;

  TBZRasterShaderRenders(Data^)[Index].Shader.Apply(FInternalRCI,nil);
  DstPix := TBZRasterShaderRenders(Data^)[Index].Buffer.GetScanLine(th);
  While (i<=MaxS) do
  begin
    TBZRasterShaderRenders(Data^)[Index].Shader.FragCoords.Create(xx,hh-yy);
    OutColor := TBZRasterShaderRenders(Data^)[Index].Shader.ShadePixel;
    DstPix^:= OutColor;
    inc(xx);
    if (xx<=TBZRasterShaderRenders(Data^)[Index].Area.Right) then
    begin
      inc(DstPix);
    end
    else
    begin
      dec(yy);
      xx:=TBZRasterShaderRenders(Data^)[Index].Area.Left;
      Dec(th);
      DstPix := TBZRasterShaderRenders(Data^)[Index].Buffer.GetScanLine(th);
    end;
    inc(i);
  end;
  TBZRasterShaderRenders(Data^)[Index].Shader.UnApply(FInternalRCI);
end;

procedure TBZBitmapShaderThreadRasterizer.MergeBitmapToBuffer(aBmp : TBZBitmap; aRect : TBZRect);
begin
  Buffer.CopyBlock(aBmp, 0,0, aRect.Width, aRect.Height, aRect.Left, aRect.Top);
end;

procedure TBZBitmapShaderThreadRasterizer.Rasterize;
Var
  i : Byte;
begin

  ParallelFor(0,3,@DoRasterizeProc, @FRenderThreads);

  For i := 0 to 3 do
  begin
    MergeBitmapToBuffer(FRenderThreads[i].Buffer, FRenderThreads[i].Area);
  end;

end;

{%endregion%}

{%region%=====[ TBZCustomBitmapRasterizer ]====================================}

constructor TBZCustomBitmapRasterizer.Create;
begin
  inherited Create;
  FShader := nil;
end;

procedure TBZCustomBitmapRasterizer.SetShader(AValue: TBZCustomSoftwareShader);
begin
  if FShader=AValue then Exit;
  FShader:=AValue;
end;

procedure TBZCustomBitmapRasterizer.Assign(Source: TPersistent);
begin
  if (Source is TBZCustomBitmapRasterizer) then
  begin
    inherited Assign(Source);
    FShader := TBZCustomBitmapRasterizer(Source).Shader;
  end
  else inherited Assign(Source);
end;

procedure TBZCustomBitmapRasterizer.InternalRasterize(Dst: TBZBitmap);
begin
 // GLobalLogger.LogNotice('TBZCustomBitmapRasterizer.InternalRasterize');
  InternalRasterize(Dst,Dst.ClipRect);
end;

procedure TBZCustomBitmapRasterizer.InternalRasterize(Dst: TBZBitmap;const DstRect: TBZRect);
begin
  DoRasterize(Dst,DstRect);
end;

{%endregion%}

{%region%=====[ TBZBitmapShaderRasterizer ]====================================}

constructor TBZBitmapShaderRasterizer.Create;
begin
  inherited Create;
  GetMem(FInternalRCI, Sizeof(TBZRenderingContextInfos));
  FInternalRCI^.ViewPortSize.Create(0,0);
  FInternalRCI^.Scene := nil;
  FInternalRCI^.Engine := nil;
  //FBuffer:=TBZBitmap.Create;
end;

destructor TBZBitmapShaderRasterizer.Destroy;
begin
  //FreeAndNil(FBuffer);
  FreeMem(FInternalRCI);
  //FInternalRCI := nil;
  Inherited Destroy;
end;

procedure TBZBitmapShaderRasterizer.Rasterize;
begin
  //InternalRasterize(FBuffer, FBuffer.ClipRect);
  DoRasterize(FBuffer, FBuffer.ClipRect);
end;

procedure TBZBitmapShaderRasterizer.SetBuffer(const AValue : TBZBitmap);
begin
  if FBuffer=AValue then Exit;
  //FBuffer.Assign(AValue);
  FBuffer := AValue;
  //ShowMessage('FBuffer : '+inttostr(FBuffer.Width)+', '+inttostr(FBuffer.Height));
  //PBZRenderingContextInfos(@FInternalRCI)^.ViewPortSize.Create(FBuffer.Width,FBuffer.Height);

  PBZRenderingContextInfos(@FInternalRCI)^.ViewPortSize.X:=FBuffer.Width;
  PBZRenderingContextInfos(@FInternalRCI)^.ViewPortSize.Y:=FBuffer.Height;
end;

procedure TBZBitmapShaderRasterizer.DoRasterizeLineProc(Sender : TObject; Index : Integer; Data : Pointer);
Var
  DstPix : PBZColor;
  i, x, xx  : LongWord;
  {$CODEALIGN VARMIN=16}
  cv : TBZColorVector;
  {$CODEALIGN VARMIN=4}
  OutColor : TBZColor;
begin

  X := PBZRasterizerParameters(Data)^.StartX;
  XX := PBZRasterizerParameters(Data)^.EndX;
  DstPix := PBZRasterizerParameters(Data)^.Surface.GetPixelPtr(X ,PBZRasterizerParameters(Data)^.Surface.MaxHeight - Index);
  //While  (X <= XX) do
  //cs.Acquire;
  for i:= x to xx do
  begin

    FShader.FragCoords.Create(i,Index);
    //cv := FShader.ShadePixel;

    OutColor:= FShader.ShadePixel;

    // need Renormalize RED to 0..1. RED is on X axis (coords go from -1.0 to 1.0). Not need for y
    //if cv.Red <0 then cv.Red := -cv.Red; // div -1.0;
    //OutColor.Create(cv);
    //if OutColor.Alpha > 0 then
    DstPix^:= OutColor; //FShader.ShadePixel; //

    //Inc(X);
    Inc(DstPix);
  end;
  //cs.Release;

  //AdvanceProgress(PBZRasterizerParameters(Data)^.DeltaProgress,0,1,False);
end;

procedure TBZBitmapShaderRasterizer.DoRasterize(Dst: TBZBitmap;DstRect: TBZRect);
var
  MaxS, LineWidth, i,xx,yy,
  mw,mh :Longword;
  C : TBZColor;
  MultiPass : Boolean;
  DstPix : PBZColor;
  Delta : Single;
  {$CODEALIGN VARMIN=16}
  cv : TBZColorVector;
  {$CODEALIGN VARMIN=4}
  RasterizerParams : PBZRasterizerParameters;


begin
  //GLobalLogger.LogNotice('TBZCustomBitmapRasterizer.Rasterize');
  //GlobalLogger.Log('Raster Dest Rect : '+inttostr(DstRect.Left)+', '+inttostr(DstRect.Top)+', '+inttostr(DstRect.Right)+', '+inttostr(DstRect.Bottom));
  if not(FShader.Enabled) then exit;
  Multipass := False;
  //xx := DstRect.Left;
  //yy := DstRect.Bottom;
  //mh := DstRect.Height;
  //mw := (DstRect.Width);
  //LineWidth := (DstRect.Right+1) - mw;

  RasterizerParams := nil;
  GetMem(RasterizerParams, Sizeof(TBZRasterizerParameters));
  RasterizerParams^.StartX := DstRect.Left;
  RasterizerParams^.EndX := DstRect.Right;
  //RasterizerParams^.DeltaProgress :=  100 / mh;
  RasterizerParams^.Surface := Dst;


  //InitProgress(mw,mh);
  //StartProgressSection(0, 'Calcul de l''image : '); // On debute une nouvelle section globale

  //Delta := 100 / mh;
  //StartProgressSection(100 ,'Calcul de l''image : ');

  i := 0;
  DstPix := Dst.GetPixelPtr(DstRect.Left,DstRect.Top);

  //PBZRenderingContextInfos(@FInternalRCI)^.ViewPortSize.X:=Dst.Width;
  //PBZRenderingContextInfos(@FInternalRCI)^.ViewPortSize.Y:=Dst.Height;

  FShader.Apply(FInternalRCI,nil);

 // MaxS := (PBZRenderingContextInfos(@FInternalRCI)^.ViewPortSize.Width
 //        * PBZRenderingContextInfos(@FInternalRCI)^.ViewPortSize.Height)-1;

 // globallogger.LogNotice('DstRect =  ' + DstRect.ToString);
  ParallelFor(DstRect.Top,DstRect.Bottom,@DoRasterizeLineProc, Pointer(RasterizerParams));

 // Maxs := mw*mh-1;
 // While (MultiPass=true) do
 // begin
    //While i<=MaxS do
    //begin
    //  //FShader.FragCoords.Create(xx,dstRect.Height-yy);
    //  FShader.FragCoords.Create(xx,yy);
    //
    // // cv := FShader.ShadePixelFloat;
    //
    //  // need Renormalize RED to 0..1. RED is on X axis (coords go from -1.0 to 1.0)
    //  // not need for y
    //  //if cv.Red <0 then cv.Red := -cv.Red; // div -1.0;
    //  //if cv.Green <0 then cv.Green := cv.Green / -1.0;
    //  //if cv.Blue <0 then cv.Blue := cv.Blue / -1.0;
    //  //if cv.Alpha <0 then cv.Alpha := cv.Alpha / -1.0;
    //
    //  //C.Create(cv);
    //  //if c<>clrTransparent then
    //  DstPix^:= FShader.ShadePixel; // C;
    //  inc(xx);
    //  if xx<=DstRect.Right then inc(DstPix)
    //  else
    //  begin
    //    dec(yy);
    //    xx:=DstRect.Left;
    //    //if LineWidth>0 then
    //    //begin
    //    //  Inc(DstPix, LineWidth);
    //    //End
    //    //else
    //    inc(DstPix) ;
    //    //AdvanceProgress(Delta,0,1,False);
    //  end;
    //  inc(i);
    //end;
    //MultiPass := FShader.UnApply(FInternalRCI);
  //end;
  FreeMem(RasterizerParams);
  //RasterizerParams := nil;
  //FinishProgressSection(False);
  //FinishProgressSection(True);
end;

{%endregion%}

end.

