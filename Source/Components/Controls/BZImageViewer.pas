(*
  @abstract(Contient un composant visuel TBZImageViewer similaire au TImage.)

  Il dispose de quelques options supplÃ©mentaires, tel que la prise en charge du dÃ©placement, du zoom. L'affichage d'un fond de type echiquier. @br
  Supporte plusieur filtre pour la mise Ã  l'Ã©chelle et diffÃ©rents comportements
  en fonction de la taille de l'image chargÃ©e.

  -------------------------------------------------------------------------------------------------------------

  @created(2017-08-22)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(22/08/2017 : Creation)
    @item(18/06/2019 : Mise Ã  jour)
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes :)@br

  -------------------------------------------------------------------------------------------------------------

  @bold(Dépendances) : BZClasses, BZControlClasses, BZGraphic, BZBitmap, BZInterpolationFilters

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)
     @unorderedList(
       @item(FPC/Lazarus)
     )

  -------------------------------------------------------------------------------------------------------------

  @bold(Licence) : MPL / LGPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZImageViewer;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils, LMessages,
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  {$IFDEF LINUX} Unix,{$ENDIF}
  Graphics, IntfGraphics, Controls, Dialogs,
  BZClasses, BZControlClasses,
  BZColors, BZGraphic, BZBitmap, BZInterpolationFilters;

type
  { Classe de base d'un control de visualisation }
  TBZCustomViewer = class(TBZCustomControl)
  private
  protected
    procedure Paint; override;
    procedure DoPaint;Virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    Property Transparent;

    Property Align;
    Property Anchors;
    Property AutoSize;

    Property BorderWidth;
    Property Constraints;
    property Color;

    Property UseDockManager Default True;
    Property DockSite;
    Property DragCursor;
    Property DragKind;
    Property DragMode;
    Property Enabled;
    //property Font;
    //property ParentFont;
    Property ParentBiDiMode;

    Property ParentShowHint;
    Property PopupMenu;
    Property ShowHint;
    Property TabOrder;
    Property TabStop;
    Property Visible;

    Property OnClick;
    Property OnPaint; // TNotifyEvent read FOnPaint write FOnPaint;

    Property OnConstrainedResize;
    Property OnContextPopup;

    Property OnDockDrop;
    Property OnDockOver;

    Property OnDblClick;
    Property OnDragDrop;
    Property OnDragOver;
    Property OnEndDock;
    Property OnEndDrag;

    Property OnEnter;
    Property OnExit;

    Property OnGetSiteInfo;

    Property OnMouseDown;
    Property OnMouseEnter;
    Property OnMouseLeave;
    Property OnMouseMove;
    Property OnMouseUp;
    Property OnMouseWheel;
    Property OnMouseWheelDown;
    Property OnMouseWheelUp;

    Property OnResize;

    Property OnStartDock;
    Property OnStartDrag;
    Property OnUnDock;

    Property OnKeyDown;
    Property OnKeyPress;
    Property OnKeyUp;
  end;

  { Control visuel de base à hériter pour la visualisation et la manipulation d'un TBZBitmap }
  TBZCustomImageViewer = class(TBZCustomViewer)
  private
    FBackBuffer : TBZBitmap;
    FBackGroundChecker : Boolean;

    procedure SetBackgroundChecker(const AValue: Boolean);
  protected
    FNeedUpdate : Boolean;

    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd);
    procedure Paint; override;
   // procedure Resize; override;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; KeepBase: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Invalidate; override;

    { Tampon bitmap de l'arrière plan }
    property BackBuffer: TBZBitmap read FBackBuffer;
    { Dessine un échiquier en arrière-plan }
    property BackGroundChecker : Boolean Read FBackGroundChecker Write SetBackgroundChecker;

  end;

  //TBZBitmapViewMouseOps = (moNone, moZoom, moMove, moSelect);

  { Mode de redimensionnement }
  TBZImageStretchMode = (ismDraft, ismSmooth, ismBicubic {, ismSmart} , ismResample); //, ismXBRz, ismSeamCarving,ismLiquidRescale

  { TBZImageViewer }
  { TBZBackgroundCheckerProperty = class(TBZCustomControlProperty)
    private
      FCellSize : Byte;
      FCellEvenColor,
      FCellOddColor : TBZColor
    protected
    public
    published
      properrty CellSize : Byte
      property CellEvenColor  : TBZColor
      property CellOddColor : TBZColor
    end;
  }

  { Evenement déclencher pour dessiner sur le canvas virtuel }
  TBZOnAfterPaintEvent = procedure(VirtualCanvas : TBZBitmapCanvas) of object;
  { Control visuel pour l'affiche et la gestion d'un TBZBitmap. @br
    Identique à TImage mais en mieux }
  TBZImageViewer = Class(TBZCustomImageViewer)
  private
    //
    //FNeedUpdate : Boolean;
    FStretch,
    FProportional,
    FCenter : Boolean;
    FStretchMode : TBZImageStretchMode;
    FResampleFilter : TBZInterpolationFilterMethod;

    // Variables pour le déplacement du et dans le bitmap (scroll)
    FOffsetLeft,FOffsetTop:Integer;
    FScrollBounds : TBZRect;
    FVirtualViewPort : TBZRect;


    FZoomSmooth : Boolean;
    FZoomFactor:Integer; // En pourcentage 100 = taille réelle
    FZoomGrid : Boolean; // Affiche une grille lors de zoom élévé pour mieux différencier les pixels


    FPainting : Boolean;

    // Un Bitmap temporaire ou sera effectué quelques opérations de transformations
    FVirtualBuffer : TBZBitmap;
    FVirtualCanvasBuffer : TBZBitmap;
    FDrawWithTransparency : Boolean;

    FOnAfterPaint : TBZOnAfterPaintEvent;
    // FCheckerProps : TBZBackgroundCheckerProperty;

    Function GetCanScrollHorizontal: Boolean;
    Function GetCanScrollVertical: Boolean;
    procedure SetProportional(const AValue: Boolean);
    procedure SetStretch(const AValue : Boolean);
    procedure SetResampleFilter(const AValue: TBZInterpolationFilterMethod);
   // procedure SetSmoothStretch(const AValue: Boolean);
    procedure SetZoomFactor(Const AValue: Integer);
    procedure SetZoomSmooth(const AValue: Boolean);
    procedure SetOffsetTop(Value: integer);
    procedure SetOffsetLeft(Value: integer);
    procedure SetDrawWithTransparency(const AValue: Boolean);

    procedure ComputeScrollBounds;
    procedure SetStretchMode(const AValue : TBZImageStretchMode);
    procedure SetZoomGrid(const AValue : Boolean);
    function GetScrollBounds : TBZRect;
    function getVirtualCanvas : TBZBitmapCanvas;
    // procedure SetStretchMode(const AValue: TBZImageStretchMode);
  protected

    FPicture : TBZPicture;

    Procedure CalculatePreferredSize(Var PreferredWidth, PreferredHeight : Integer; {%H-}WithThemeSpace : Boolean); Override;
    {$IFDEF WINDOWS}
    Class Function GetControlClassDefaultSize: TSize; Override;
    {$ENDIF}

    function GetCanScroll : Boolean;
    procedure SetCenter(const AValue : Boolean);
    procedure SetPicture(const AValue: TBZPicture);

    procedure DoOnSetTransparent; override;
    procedure Paint; override;

    procedure DoOnResize; override;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; KeepBase: boolean); override;

    procedure DoOnPictureChange(Sender : TObject); virtual;
  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    procedure Repaint; override;
    { Portion de l'image actuellement affichée }
    property VirtualViewPort:TBZRect read FVirtualViewPort;
    { Retourne si le controle peut scroller }
    property CanScroll : Boolean read GetCanScroll;
    { Retourne si le controle peut scroller à la vertical }
    property CanScrollVertical : Boolean read GetCanScrollVertical;
    { Retourne si le controle peut scroller à l'horizontal }
    property CanScrollHorizontal : Boolean read GetCanScrollHorizontal;
    { Retourne les limites du scroll }
    property ScrollBounds : TBZRect Read GetScrollBounds;
  published
    { Stock l'image affichée }
    property Picture: TBZPicture read FPicture Write SetPicture;
    { Activer / désactiver le Redimensionnement }
    property Stretch: Boolean read FStretch write SetStretch default False;
    { Conserver les proportions lors du redimensionnement }
    property Proportional: Boolean read FProportional write SetProportional default False;
    { Centre l'image }
    property Center: Boolean read FCenter write SetCenter default False;
    { Mode de redimensionnement }
    property StretchMode : TBZImageStretchMode read FStretchMode write SetStretchMode;
    { Sélection du filtre de rééchantillognage }
    property ResampleFilter : TBZInterpolationFilterMethod read FResampleFilter write SetResampleFilter;
    { Zoom lissé }
    property ZoomSmooth : Boolean read FZoomSmooth write SetZoomSmooth default false;
    { Facteur de zoom en pourcentage}
    property ZoomFactor : integer read FZoomFactor write SetZoomFactor;
    { Afficher une grille lors du zoom (s'affiche à partir d'un facteur de 800% }
    property ZoomGrid : Boolean Read FZoomGrid Write SetZoomGrid default True;
    { Affichage de l'image à partir da position gauche (scrolling) }
    property OffsetLeft:integer read FOffsetLeft Write SetOffsetLeft;
    { Affichage de l'image à partir de sa position haute (scrolling) }
    property OffsetTop:integer read FOffsetTop Write SetOffsetTop;
    { Affiche l'image avec la prise en charge de la transparence }
    property DrawWithTransparency : Boolean Read FDrawWithTransparency Write SetDrawWithTransparency;

    property VirtualCanvas : TBZBitmapCanvas read getVirtualCanvas;

    property OnAfterPaint : TBZOnAfterPaintEvent read FOnAfterPaint write FOnAfterPaint;

    property BackGroundChecker;

    property Transparent;
    property OnPaint;
    //property OnResize;
    property Align;
    property Anchors;
    property AutoSize;

    property BorderWidth;
    property Constraints;

    property UseDockManager;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    //property Font;
    //property ParentFont;
    property ParentBiDiMode;

    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;

    property OnGetSiteInfo;

    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;

    property OnResize;

    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;


implementation

uses Math, fpCanvas, BZLogger, BZTypesHelpers;

{%region==== [ TBZCustomViewer ]===============================================}

procedure TBZCustomViewer.Paint;
begin
  inherited Paint;
  DoPaint;
end;

procedure TBZCustomViewer.DoPaint;
begin
  // Do Nothing here
end;

constructor TBZCustomViewer.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
end;

destructor TBZCustomViewer.Destroy;
begin
  inherited Destroy;
end;

{%endregion%}

{%region% ===[ TBZCustomImageViewer ]==========================================}

constructor TBZCustomImageViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
 // ControlStyle := ControlStyle - [csDoubleClicks, csTripleClicks, csQuadClick];  // - csOpaque pour que l'affichage avec transparence soit correcte
  //FNeedUpdate := True;
  Color := clBlack;
  //Transparent := False;
  FBackBuffer:=TBZBitmap.Create(0,0);
end;

destructor TBZCustomImageViewer.Destroy;
begin
  FreeAndNil(FBackBuffer);
  inherited;
end;

procedure TBZCustomImageViewer.SetBackgroundChecker(const AValue: Boolean);
begin
  if FBackgroundChecker = AVAlue then exit;
  FBackgroundChecker := AValue;
  Invalidate;
end;

{$hints off}
procedure TBZCustomImageViewer.WMEraseBkgnd(var Message : TLMEraseBkgnd);
begin
  // Ne fait rien
end;
{$hints on}

//procedure TBZCustomImageViewer.Resize;
procedure TBZCustomImageViewer.ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; KeepBase: boolean);
var
  c1, c2 : TBZColor;
begin
  ////GlobalLogger.LogNotice('Resize ImageView '+Inttostr(AWidth)+'x'+Inttostr(AHeight));
  if (AWidth <= 1) and (AHeight <= 1) then exit;
  FBackBuffer.SetSize(AWidth,AHeight);

    //if FBackgroundChecker then
    //begin
    //  c1 := clrWhite;
    //  c2.Create(220,220,220,255);
    //  FBackBuffer.RenderFilter.DrawChecker(0,0,BackBuffer.MaxWidth,BackBuffer.MaxHeight,c1, c2)
    //end
    //else
    //If Transparent then
    //  FBackBuffer.Clear(clrTransparent)
    //else
    //begin
    //  c1.Create(Color);
    //  FBackBuffer.Clear(c1);
    //end;

//  FNeededUpdate := True;
  inherited ChangeBounds(ALeft, ATop, AWidth, AHeight, KeepBase);
end;

procedure TBZCustomImageViewer.Invalidate;
begin
  //FNeedUpdate := True;
  //Paint;
  inherited Invalidate;
end;

procedure TBZCustomImageViewer.Paint;
var
  ARect:TRect;

  procedure DrawBackGround(ARect:TRect);
  begin
    with inherited Canvas do
    begin
      Brush.Bitmap:=TBitmap.Create;
      Brush.Bitmap.Width:=Width;
      Brush.Bitmap.Height:=Height;
      with Brush.Bitmap.Canvas do
      begin
        Brush.Color:=$707070;//clBtnShadow;
        FillRect(ClipRect);
        Pen.Color:=$505050;
        Pen.Width:=0;
        MoveTo(0,0);
        LineTo(8,8);
        MoveTo(0,7);
        LineTo(8,-1);
      end;
      FillRect(ARect);
      Brush.Bitmap.Free;
      Brush.Bitmap:=nil;{}
    end;
  end;

  procedure DrawFrame;
  begin
    with inherited Canvas do
    begin
      Pen.Color := clRed;
      Pen.Style := fpCanvas.psDash;
      MoveTo(0, 0);
      LineTo(Self.Width-1, 0);
      LineTo(Self.Width-1, Self.Height-1);
      LineTo(0, Self.Height-1);
      LineTo(0, 0);
    end;
  end;

begin
  //;
  ARect := Classes.Rect(0,0,Self.ClientWidth-1,Self.ClientHeight-1);

 // if FNeedUpdate then
 // begin
//    //GlobalLogger.LogNotice('PAINT WITH TRANSPARENCY '+bool2strFR[Transparent]);
   FBackBuffer.DrawToCanvas(Canvas,ARect,not(Transparent),not(Transparent));
  //  FNeedUpdate := False;
 // End;
  if csDesigning in ComponentState then
  begin
    //DrawBackground(ARect);
    DrawFrame;
  end;
  //inherited paint;
end;

{%endregion%}

{%region% ===[ TBZImageViewer ]================================================}

constructor TBZImageViewer.Create(AOwner : TComponent);
begin
  Inherited;
  ////GlobalLogger.LogNotice('TBZImageViewer.Create');
  FStretch := False;
  FProportional := False;
  FCenter := False;
  FStretchMode := ismDraft;
  FResampleFilter := ifmLanczos3; // Filtre par défaut
  FOffsetLeft:=0;
  FOffsetTop:=0;
  ////GlobalLogger.LogStatus(' ----> Create FPicture');
  FPicture:= TBZPicture.Create;
  FPicture.Bitmap.OnChange := @DoOnPictureChange;

  With FVirtualViewPort do
  begin
    Left:=0;
    Top:=0;
    Right:=ClientWidth;
    Bottom:=ClientHeight;
  end;

  FZoomFactor:=100;
  FZoomSmooth:=false;
  FZoomGrid := True;

  FPainting:=False;
  FOnAfterPaint := nil;
  ////GlobalLogger.LogStatus(' ----> Create VirtualBuffer');
  FVirtualBuffer := TBZBitmap.Create(nil,0,0);
  FVirtualCanvasBuffer := nil;
  BorderStyle := bsNone;
  BorderWidth:= 0;
  Width := 90;
  Height := 90;
  FNeedUpdate := True;
end;

destructor TBZImageViewer.Destroy;
begin
  FPainting:=true; //Pour eviter un rafraichissement de l'affichage hasardeux
  FreeAndNil(FPicture);
  FreeAndNil(FVirtualBuffer);
  if FVirtualCanvasBuffer<>nil then FreeAndNil(FVirtualCanvasBuffer);
  Inherited Destroy;
end;

procedure TBZImageViewer.SetPicture(const AValue : TBZPicture);
begin
  if FPicture=AValue then exit;

  FPicture.Assign(AValue);

  With FVirtualViewPort do
  begin
    Left:=0;
    Top:=0;
    Right:=math.min(ClientWidth,FPicture.Bitmap.Width);
    Bottom:=math.min(ClientHeight,FPicture.Bitmap.Height);
  end;

  FOffsetLeft:=0;
  FOffsetTop:=0;
  FNeedUpdate := True;
  Invalidate;
  FPicture.Bitmap.OnChange := @DoOnPictureChange;
end;

procedure TBZImageViewer.DoOnSetTransparent;
Begin
  FPicture.Bitmap.Layers.Transparent := Transparent;
End;

procedure TBZImageViewer.SetStretch(const AValue : Boolean);
begin
  if FStretch = AValue then exit;
  FStretch := AValue;
  // Reinitalise le scroll
  FOffsetLeft:=0;
  FOffsetTop:=0;
  if not(FStretch) then FVirtualBuffer.SetSize(FPicture.Bitmap.Width,FPicture.Bitmap.Height);
  //ComputeScrollBounds;
  FNeedUpdate := True;
  Invalidate;
end;

procedure TBZImageViewer.SetResampleFilter(const AValue : TBZInterpolationFilterMethod);
begin
  if FResampleFilter = AValue then exit;
  FResampleFilter := AValue;
  FNeedUpdate := True;
  Invalidate;
end;

procedure TBZImageViewer.SetOffsetLeft(Value : integer);
BEGIN
  if fOffsetLeft <> Value then
  begin
    if (Value > FScrollBounds.Right) then Value :=  FScrollBounds.Right;
    if (Value < FScrollBounds.Left) then Value :=  FScrollBounds.Left;
    fOffsetLeft := Value;
    FNeedUpdate := True;
    Invalidate;
  end;
END;

procedure TBZImageViewer.SetOffsetTop(Value : integer);
BEGIN
  if fOffsetTop <> Value then
  begin
    if (Value > FScrollBounds.Bottom) then Value := FScrollBounds.Bottom;
    if (Value < FScrollBounds.Top) then Value := FScrollBounds.Top;
    fOffsetTop := Value;
    FNeedUpdate := True;
    Invalidate;
  end;
end;

procedure TBZImageViewer.SetCenter(const AValue : Boolean);
begin
  if FCenter = AValue then exit;
  FCenter := AValue;
//  if FCenter then
//  begin
    FOffsetLeft:=0;
    FOffsetTop:=0;
 // end;
 // ComputeScrollBounds;
  Invalidate;
end;

procedure TBZImageViewer.SetProportional(const AValue : Boolean);
begin
  if FProportional = AValue then exit;
  FProportional := AValue;
  FNeedUpdate := True;
  Invalidate;
end;

Function TBZImageViewer.GetCanScrollHorizontal : Boolean;
Begin
  Result := False;
  if Not(FStretch) then
  begin
    Result := (FVirtualBuffer.Width > ClientWidth);
  End;
end;

Function TBZImageViewer.GetCanScrollVertical : Boolean;
Begin
  Result := False;
  if Not(FStretch) then
  begin
    Result := (FVirtualBuffer.Height > ClientHeight);
  End;
end;

function TBZImageViewer.GetCanScroll : Boolean;
Begin
  Result := False;
  if Not(FStretch) then
  begin
    Result := (FVirtualBuffer.Width > ClientWidth) or (FVirtualBuffer.Height > ClientHeight);
  End;
  //GlobalLogger.LogStatus('CAN SCROLL = ' + Result.ToString());
End;

procedure TBZImageViewer.SetZoomFactor(Const AValue : Integer);
begin
  if FZoomFactor = AValue then exit;
  FZoomFactor:=AVAlue;
  if FZoomFactor > 10000 then FZoomFactor:=10000;
  if FZoomFactor < 1 then FZoomFactor:=1;
  FNeedUpdate := true;
  Invalidate;
end;

procedure TBZImageViewer.SetZoomSmooth(const AValue : Boolean);
begin
  if FZoomSmooth = AVAlue then exit;
  FZoomSmooth := AValue;
  FNeedUpdate := true;
  Invalidate;
end;

procedure TBZImageViewer.SetDrawWithTransparency(const AValue : Boolean);
begin
  if FDrawWithTransparency = AVAlue then exit;
  FDrawWithTransparency := AValue;
  Invalidate;
end;

procedure TBZImageViewer.ComputeScrollBounds;
var
  hmax, vmax : Integer;
Begin
  if FCenter then
  begin
    vmax := (FVirtualBuffer.Height - ClientHeight) shr 1;
    hmax := (FVirtualBuffer.Width - ClientWidth) shr 1;
    With FScrollBounds do
    begin
      if vmax > 0 then Top    := - vmax else Top    := 0;
      if hmax > 0 then Left   := - hmax else Left   := 0;
      if vmax > 0 then Bottom := vmax   else Bottom := 0;
      if hmax > 0 then Right  := hmax   else Right  := 0;
    End;
  End
  else
  begin
    vmax := (FVirtualBuffer.Height - ClientHeight);
    hmax := (FVirtualBuffer.Width - ClientWidth);
    With FScrollBounds do
    begin
      Top := 0;
      Left := 0;
      if vmax > 0 then Bottom := vmax else Bottom := 0;
      if hmax > 0 then Right := hmax else Right := 0;
    End;
  End;
  {.$IFDEF DEBUG}
  //Globallogger.LogNotice('ComputeScrollBounds -----------------------------------------------');
  //Globallogger.LogStatus(' - Dimension    : ' + FVirtualBuffer.Width.ToString()+'x'+FVirtualBuffer.Height.ToString());
  //Globallogger.LogStatus(' - ScrollBounds : ' + FScrollBounds.ToString);
  //Globallogger.LogNotice('ComputeScrollBounds -----------------------------------------------');
  {.$ENDIF}
End;

procedure TBZImageViewer.SetStretchMode(const AValue : TBZImageStretchMode);
begin
  if FStretchMode = AValue then Exit;
  FStretchMode := AValue;
  Invalidate;
end;

procedure TBZImageViewer.SetZoomGrid(const AValue : Boolean);
begin
  if FZoomGrid = AValue then Exit;
  FZoomGrid := AValue;
  FNeedUpdate := True;
  Invalidate;
end;

function TBZImageViewer.GetScrollBounds : TBZRect;
begin
  ComputeScrollBounds;
  Result := FScrollBounds;
end;

function TBZImageViewer.getVirtualCanvas : TBZBitmapCanvas;
begin
  FNeedUpdate := True;
  Result := FVirtualBuffer.Canvas;
  //if FVirtualCanvasBuffer = nil then
  //begin
  //  FVirtualCanvasBuffer := TBZBitmap.Create(nil, FBackBuffer.Width, FBackBuffer.Height);
  //  FVirtualCanvasBuffer.Clear(clrTransparent);
  //end;
  //Result := FVirtualCanvasBuffer.Canvas;
end;

Procedure TBZImageViewer.CalculatePreferredSize(Var PreferredWidth, PreferredHeight : Integer; WithThemeSpace : Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);
  if (FPicture.Bitmap.Width > 1) then PreferredWidth := FPicture.Bitmap.Width else PreferredWidth := 90;
  if (FPicture.Bitmap.Height > 1) then PreferredHeight := FPicture.Bitmap.Height else PreferredHeight := 90;
end;

{$IFDEF WINDOWS}
Class Function TBZImageViewer.GetControlClassDefaultSize : TSize;
begin
  Result.CX := 90;
  Result.CY := 90;
end;
{$ENDIF}

procedure TBZImageViewer.Invalidate;
begin
  if FPainting then exit;
  FNeedUpdate := True;
  inherited Invalidate;
end;

procedure TBZImageViewer.Repaint;
begin
  FNeedUpdate := True;
  inherited Repaint;
end;

procedure TBZImageViewer.Paint;
var
  DW, DH : Integer; //Dimension de la surface d'affichage
  RW,RH: Integer; // Nouvelle Dimension
  SH,SW : Integer; //Source
  dX,dY : Integer; // Position de l'affichage du buffer virtuel dans le buffer d'affichage
  cRX, cRY, cDX, cDY : Integer; // centre en x et y du buffer virtuel et du buffer d'affichage
  W,H, I : Integer; // pour la grille
  sRX, sRY,  rGX, rGY  : Single;
  AColor, c1, c2 : TBZColor;
  //ZoomBmp : TBZBitmap; // Tampon pour le zoom
  vpw, vph : integer;


begin
  //if csDesigning in ComponentState then
  //begin
  //  inherited paint;
  //  exit;
  //end;
    //if not(Assigned(FPicture.Bitmap)) then exit;
    if (FPicture.Bitmap.Height<1) or (FPicture.Bitmap.Width<1) then exit;
    //GlobalLogger.LogNotice('ImageView PAINT : ');

    FPainting:=true;

    dX := 0;
    dY := 0;

    SH := FPicture.Bitmap.Height;
    SW := FPicture.Bitmap.Width;

    DW := BackBuffer.Width;
    DH := BackBuffer.Height;

    If FNeedUpdate then
    begin
      if not(FStretch) then // Pas d'adaption de l'image
      begin
        RW := SW;
        RH := SH;

        // On met à jour le "ViewPort"
        With FVirtualViewPort do
        begin
          Left:=0;
          Top:=0;
          Right:=DW;
          Bottom:=DH;
        end;

        if  (FZoomFactor <> 100) then // doit-on effectuer un zoom
        Begin
          rGX := FZoomFactor / 100;// Largeur d'un pixel
          rGY := rGx;  // Hauteur d'un pixel
          RW := Round(rGX*SW);
          RH := Round(rGy*SH);

          //RW:=round((FZoomFactor/100)*SW);
          //RH:=round((FZoomFactor/100)*SH);

          //rGX := RW / SW;// Largeur d'un pixel
          //rGY := RH / SH;  // Hauteur d'un pixel

          W := Ceil(RW / rGX);  // Nombre de colonnes
          H := Ceil(RH / rGY); // Nombre lignes

          FVirtualBuffer.SetSize(RW,RH);
          FVirtualBuffer.Clear(clrTransparent);
          if FZoomSmooth then
            FPicture.Bitmap.Transformation.StretchSmoothTo(FVirtualBuffer, False)
          else FPicture.Bitmap.Transformation.StretchTo(FVirtualBuffer, False);
         //FVirtualBuffer.Transformation.Stretch(RW,RH,false);

          // Affichage de la grille du Zoom a partir d'un facteur 8 (800% =  8 pixels sur 8 pixels )
          if (FZoomFactor >= 800) and FZoomGrid then
          begin
            With FVirtualBuffer.Canvas Do
            begin
              With DrawMode do
              begin
                PixelMode := dmSet;
                AlphaMode := amNone;
                CombineMode := cmAverage;
              End;
              AColor :=clrSilver;
              AColor.Alpha := 245;
              Pen.Color := AColor;
              // Lignes  Verticale
              //if  (FVirtualViewPort.Left mod round(rGX)) <> 0 then sRX := Round(FVirtualViewPort.Left/rGX)*rGX
              //else sRX := FVirtualViewPort.Left;
              //cDX := min(Round(sRX)-1,FVirtualBuffer.MaxWidth);
              cDX := 0;
              For I:=0 to W do
              begin
                if cDX > FVirtualBuffer.MaxWidth then cDX := FVirtualBuffer.MaxWidth;
                MoveTo(cDX,0);
                LineTo(cDX,FVirtualBuffer.MaxHeight);
                cDX := Round((cDX + rGX));
                //if ((cDX=0) and (I=0)) then sRX:=(sRX+rGX)-1 else  sRX:=(sRX+rGX);
                //cDX := Round(sRX)-1;
              End;
              // Lignes Horizontale
              //if  (FVirtualViewPort.Top mod round(rGY)) <> 0 then sRY := Round(FVirtualViewPort.Top/rGY)*rGY
              //else sRY := FVirtualViewPort.Top;
              //cDY := Min(Round(sRY),FVirtualBuffer.MaxHeight);
              cDy := 0;
              For I:=0 to H do
              begin
                if cDY > FVirtualBuffer.MaxHeight then cDy := FVirtualBuffer.MaxHeight;
                MoveTo(0,cDY);
                LineTo(FVirtualBuffer.MaxWidth,cDY);
                //if ((cDY=0) and (I=0)) then sRY:=(sRY+rGY)-1 else  sRY:=(sRY+rGY);
                //cDY := Round(sRY)-1;
                cDY := Round((cDY + rGY));
              End;
            End;
          End;
        End
        else
        begin
          FVirtualBuffer.SetSize(SW,SH);
          FVirtualBuffer.PutImage(FPicture.Bitmap,0,0,SW,SH,0,0,dmSet, amAlpha);
        end;

      End
      Else
      Begin
        RW := DW;
        RH := DH;
        if FProportional and (SW > 1) and (SH > 1) then // On calcul le ratio par rapport aux dimensions du buffer d'affichage
        begin
          W:=DW;
          H:=(SH*W) div SW;
          if (H>DH) then
          begin
            H:=DH;
            W:=(SW*H) div SH;
          end;
          RW:=W;
          RH:=H;
        end;
        // On adapte l'image

        //Case FStretchMode of
        //  ismDraft  : FVirtualBuffer.Transformation.Stretch(RW,RH,False);
        //  ismSmooth : FVirtualBuffer.Transformation.StretchSmooth(RW,RH,False);
        //  ismBicubic : FVirtualBuffer.Transformation.StretchBicubic(RW,RH,False);
        ////  ismSmart : FVirtualBuffer.Transformation.StretchSmart(RW,RH,2,False);
        //  ismResample : FVirtualBuffer.Transformation.ReSample(RW,RH,FResampleFilter,False);
        // // else
        //end;
        FVirtualBuffer.SetSize(RW,RH);
        Case FStretchMode of
          ismDraft  : FPicture.Bitmap.Transformation.StretchTo(FVirtualBuffer, False);
          ismSmooth : FPicture.Bitmap.Transformation.StretchSmoothTo(FVirtualBuffer, False);
          ismBicubic : FPicture.Bitmap.Transformation.StretchBicubicTo(FVirtualBuffer, False);
          //ismSmart : FVirtualBuffer.Transformation.StretchSmart(RW,RH,2,False);
          ismResample : FPicture.Bitmap.Transformation.ReSampleTo(FVirtualBuffer,FResampleFilter,False);
         // else
        end;

        // On met à jour le "ViewPort"
        With FVirtualViewPort do
        begin
          Left:=0;
          Top:=0;
          Right:=RW;
          Bottom:=RH;
        end;
      End;

    end;

    // On efface le fond
    if FBackgroundChecker and not(Transparent) then
    begin
       c1 := clrWhite;
       c2.Create(192,192,192,255);
       BackBuffer.RenderFilter.DrawChecker(0,0,BackBuffer.MaxWidth,BackBuffer.MaxHeight,c1, c2)
    end
    else If Transparent then
      BackBuffer.Clear(clrTransparent)
    else
    begin
      if FVirtualBuffer.ImageDescription.FrameCount > 0 then c1 := FVirtualBuffer.Layers.BackgroundColor // Si c'est une Multi-Image
      else c1.Create(Color);
      BackBuffer.Clear(c1);
    end;

    // Centrage et Scrolling;
    if (RW <> DW) or (RH<> DH) then   // Les dimensions du buffer virtuel ne sont pas les même que celle du buffer d'affichage
    Begin
      // Centres des bitmap
      cRX := FVirtualBuffer.CenterX; // (RW shr 1)-1;
      cRY := FVirtualBuffer.CenterY; //(RH shr 1)-1;
      cDX := BackBuffer.CenterX; // (DW shr 1)-1;
      cDY := BackBuffer.CenterY; //(DH shr 1)-1;

      // On ajuste sur l'axe des X
      if RW > DW then // La largeur du buffer virtuel est plus grande de celle du buffer d'affichage
      begin
        dX := 0;
        // Centrage, On ajuste le viewport du buffer virtuel
        if FCenter then
        begin
          FVirtualViewPort.Left := cRX - cDX; //(RW-DW) div 2
          FVirtualViewPort.Right := FVirtualViewPort.Left + DW;
        End;
        // Le scrolling n'est possible que si le buffer virtuel est plus grand que le buffer d'affichage
        // Scroll Horizontal
        if FOffsetLeft<>0 then
        begin
          FVirtualViewPort.Left := FVirtualViewPort.Left+FOffsetLeft;
          FVirtualViewPort.Right := FVirtualViewPort.Left + DW;
        End;
      end
      else if RW < DW then // La largeur du buffer virtuel est plus petite de celle du buffer d'affichage
      begin
       // Centrage, On ajuste le position d'affichage du buffer virtuel
       if FCenter then dX := cDX - cRX;
       if dX<0 then dX:=0;
       if dX>DW then dX:=DW-1;
      end;
      // On ajuste sur l'axe des Y
      if RH > DH then // La hauteur du buffer virtuel est plus grande de celle du buffer d'affichage
      begin
       dY:=0;
       // Centrage, On ajuste le viewport du buffer virtuel
       if FCenter then
       begin
         FVirtualViewPort.Top := cRY - cDY;
         FVirtualViewPort.Bottom := FVirtualViewPort.Top + DH;
       End;
       // Scroll Vertical
       if FOffsetTop<>0 then
       begin
         FVirtualViewPort.Top:= FVirtualViewPort.Top+OffsetTop;
         FVirtualViewPort.Bottom := FVirtualViewPort.Top + DH;
       end;
      end
      else if RH < DH then // La hauteur du buffer virtuel est plus petite de celle du buffer d'affichage
      begin
       // On ajuste la position d'affichage du buffer virtuel
        if FCenter then dY := cDY - cRY;
        if dY<0 then dY:=0;
        if dY>DH then dY:=DH-1;
      End;
    End;

    (* if Center then
      begin
        // On met à jour le "ViewPort"
        With FVirtualViewPort do
        begin
          if RW > DW then Left := ((DW div 2) - (RW div 2) + FOffsetLeft)
          else
          begin
            Left:= 0 ;
            dX := Abs((DW div 2) - (RW div 2)+ FOffsetLeft);
          End;

          if RH > DH then Top := ((DH div 2) - (RH div 2) + FOffsetTop)
          else
          begin
            Top:= 0;
            dY := Abs((DH div 2) - (RH div 2) + FOffsetTop);
          End;
          Right  := Left + DW;
          Bottom := Top  + DH;
        end;
      end
      else
      begin
        // On met à jour le "ViewPort"
        With FVirtualViewPort do
        begin
          Left   := Round(FOffsetLeft * rGX); //(RW - DW)+FOffsetLeft;
          Top    := Round(FOffsetTop * rGY); //(RH - DH)+FOffsetTop;
          Right  := Left + DW;
          Bottom := Top  + DH;
        end;
      end;
*)

    vpw := FVirtualViewPort.Right - FVirtualViewPort.Left;
    vph := FVirtualViewPort.Bottom - FVirtualViewPort.Top;

    //Globallogger.LogStatus(' END PAINT =  Dimension    : ' + FVirtualBuffer.Width.ToString()+'x'+FVirtualBuffer.Height.ToString());
    ComputeScrollBounds;

    if Assigned(FOnAfterPaint) then
    begin
      //FOnAfterPaint(FVirtualBitmapCanvas);
      FOnAfterPaint(FVirtualBuffer.Canvas);
      FNeedUpdate := True;
    end;

    If DrawWithTransparency or (Transparent) then BackBuffer.PutImage(FVirtualBuffer,FVirtualViewPort.Left,FVirtualViewPort.Top,vpw,vph,dX,dY,dmSet,amAlpha)
    else  BackBuffer.PutImage(FVirtualBuffer,FVirtualViewPort.Left,FVirtualViewPort.Top,vpw,vph,dX,dY,dmSet,amOpaque);

    //if FNeedUpdate then
    //begin
    //  if FVirtualCanvasBuffer <> nil then
    //  begin
    //    BackBuffer.PutImage(FVirtualCanvasBuffer,FVirtualViewPort.Left,FVirtualViewPort.Top,vpw,vph,dX,dY,dmSet,amAlpha);
    //    FVirtualCanvasBuffer.Clear(clrTransparent);
    //  end;
    //end;

    FPainting:=false;
    FNeedUpdate := False;
    inherited Paint;
   // BackBuffer.DrawTo(Canvas);
  //end;

//  End;
end;

procedure TBZImageViewer.DoOnResize;
begin
  ComputeScrollBounds;
  FNeedUpdate := True;
  inherited DoOnResize;
end;

procedure TBZImageViewer.ChangeBounds(ALeft, ATop, AWidth, AHeight : integer; KeepBase : boolean);
Begin
  Inherited ChangeBounds(ALeft, ATop, AWidth, AHeight, KeepBase);
  ComputeScrollBounds;
End;

procedure TBZImageViewer.DoOnPictureChange(Sender : TObject);
Var
  SH,SW : LongWord;
Begin
  if (FPicture.Bitmap.Height<=1) or (FPicture.Bitmap.Width<=1) then exit;
  //SH := FPicture.Bitmap.Height;
  //SW := FPicture.Bitmap.Width;
  //FVirtualBuffer.SetSize(SW,SH);

  FNeedUpdate := True;
  Invalidate;
  //ComputeScrollBounds;
End;

{%endregion%}

//==============================================================================

Initialization
  // On enregistre nos classes pour la persitence des objets
  RegisterClasses([TBZImageViewer]);


Finalization

  UnRegisterClasses([TBZImageViewer]);

  //==============================================================================
end.

