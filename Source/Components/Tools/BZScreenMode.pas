(*
  @abstract(Contient un composant TBZScreenMode pour interagir avec l'écran / bureau (changement du mode video, passage en plein écran).)

  -------------------------------------------------------------------------------------------------------------

  @created(2017-06-11)
    @author(J.Delauney (BeanzMaster))
    Historique : @br
    @unorderedList(
      @item(11/06/2017 : Creation  )
    )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) :

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)@br
    @unorderedList(
      @item(Basé sur le code de GLScene http://www.sourceforge.net/glscene)
      @item(J.Delauney (BeanzMaster))
    )

  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZScreenMode;

interface

{$i ..\..\bzscene_options.inc}

uses
  {$IFDEF WINDOWS} 
    Windows, 
  {$ENDIF}
  {$IFDEF LINUX}
    x, xlib, xf86vmode, 
  {$ENDIF}
  Forms, Controls,
  LCLVersion,
  Classes, BZSystem, BZGraphic;

const
  MaxVideoModes = 200;

type
  { Définition des bornes de l'ensemble des modes vidéo }
  TBZScreenModeResolution = 0 .. MaxVideoModes;

  { Descritption d'un mode vidéo }
  TBZVideoMode = packed record
    Width: Word;
    Height: Word;
    ColorDepth: Byte;
    MaxFrequency: Byte;
    Description: String;
  end;
  { Pointeur sur un mode video }
  PVideoMode = ^TBZVideoMode;

  { Evènement déclancher lors d'un changement de mode video }
  TBZScreenModeChangeVideoModeEvent = procedure (Sender : TObject; ScreenWidth, ScreenHeight, ScreenDepth, ScreenFrequency: Integer; VideoModeIndex : TBZScreenModeResolution ) of object;

  { Composant pour controler le mode video de(s) écran(s) et le comportement de la fen^tre principale de l'application }
  TBZScreenMode = Class(TComponent)
  private
    FForm : TForm;
    FInitialFormWindowState : TWindowState ;
    FInitialFormPosition : TPosition;
    FInitialFormStyle : TFormStyle;
    FInitialFormBorderStyle : TFormBorderStyle;

    FAvailableVideoMode : TStrings;
    FActualScreenWidth : Integer;
    FActualScreenHeight : Integer;
    FFullScreenMode : Boolean;
    FXRes : Integer;
    FYRes : Integer;
    FBPP : Integer;
    FFrequency : Integer;
    FFullScreenForm : Boolean;
    FScreenModeChanged : Boolean;
    FAvailableVideoModeCount : Integer;
    FCurrentVideoModeIndex : TBZScreenModeResolution;

    FOnBeforeRestoreVideoMode, FOnAfterRestoreVideoMode : TNotifyEvent;
    FOnBeforeChangeVideoMode, FOnAfterChangeVideoMode : TBZScreenModeChangeVideoModeEvent;

    procedure SetAvailableVideoMode(const AValue : TStrings);
    procedure SetXRes(const AValue : Integer);
    procedure SetYRes(const AValue : Integer);
    procedure SetBPP(const AValue : Integer);
    procedure SetFrequency(const AValue : Integer);
    procedure SetFullScreenForm(const AValue : Boolean);

    procedure SetOnBeforeRestoreVideoMode(const AValue : TNotifyEvent);
    procedure SetOnBeforeChangeVideoMode(const AValue : TBZScreenModeChangeVideoModeEvent);
    procedure SetOnAfterRestoreVideoMode(const AValue : TNotifyEvent);
    procedure SetOnAfterChangeVideoMode(const AValue : TBZScreenModeChangeVideoModeEvent);

  protected
    function GetNativeMousePos : TPoint;
  public
    { Création }
    Constructor Create(AOwner: TComponent); override;
    { Destruction }
    Destructor Destroy; override;

    { Change vers le mode video choisit }
    procedure ChangeVideoMode;
    { Restore le mode video par defaut }
    procedure RestoreVideoMode;

    { Montre le curseur de la souris }
    procedure ShowMouseCursor;
    { Cache le curseur de la souris }
    procedure HideMouseCursor;

    { Retourne la position de la souris }
    function GetMousePos : TBZPoint;

    { Définit la position  de la souris }
    procedure SetMousePos(MouseScreenX, MouseScreenY : Integer); overload;
    { Définit la position  de la souris }
    procedure SetMousePos(P : TPoint); overload;
    { Définit la position  de la souris }
    procedure SetMousePos(P : TBZPoint); overload;

    { Retourne la liste des modes video disponibles }
    property AvailableVideoMode : TStrings read FAvailableVideoMode write SetAvailableVideoMode;
    { Retourne la largeur actuelle du mode video }
    property ActualScreenWidth : Integer read FActualScreenWidth;
    { Retourne la hauteur actuelle du mode video }
    property ActualScreenHeight : Integer read FActualScreenHeight;
    { Retourne le nombre de mode video disponible }
    property AvailableVideoModeCount : Integer read FAvailableVideoModeCount;
    { Retourne l'index du mode video actuel }
    property CurrentVideoModeIndex : TBZScreenModeResolution read FCurrentVideoModeIndex;
    { Retourne @True si on est en plein ecran }
    property FullScreenMode : Boolean read FFullScreenMode;
    { Retourne @True si le mode video a changé }
    property ScreenModeChanged : Boolean read FScreenModeChanged;

  published
    { Choix de la resolution en largeur du mode video }
    property XRes : Integer read FXRes write SetXRes;
    { Choix de la resolution en hauteur du mode video }
    property YRes : Integer read FYRes write SetYRes;
    { Choix du nombre de bit par pixel du mode video }
    property BPP : Integer read FBPP write SetBPP;
    { Choix de la fréquence de rafraichissement du mode video }
    property Frequency : Integer read FFrequency write SetFrequency;
    { Choix si la fenêtre de notre application doit être affichée en plein écran }
    property FullScreenForm : Boolean read FFullScreenForm write SetFullScreenForm;

    { Evenement déclencher avant la restauration du mode video }
    property OnBeforeRestoreVideoMode : TNotifyEvent read FOnBeforeRestoreVideoMode write SetOnBeforeRestoreVideoMode stored;
    { Evenement déclencher avant le changement du mode video }
    property OnBeforeChangeVideoMode : TBZScreenModeChangeVideoModeEvent read FOnBeforeChangeVideoMode write SetOnBeforeChangeVideoMode stored;
    { Evenement déclencher après la restauration du mode video }
    property OnAfterRestoreVideoMode : TNotifyEvent read FOnAfterRestoreVideoMode write SetOnAfterRestoreVideoMode stored;
    { Evenement déclencher après le changement du mode video }
    property OnAfterChangeVideoMode : TBZScreenModeChangeVideoModeEvent read FOnAfterChangeVideoMode write SetOnAfterChangeVideoMode stored;
  end;

{ Détermine l'index d'une résolution d'écran disponible la plus proche en fonction des valeurs données.
  La résolution d'écran renvoyée est toujours supérieure ou égal à XRes et YRes.
  Si la résolution n'est pas prise en charge, la valeur 0 est retournée, ce qui indique le mode par défaut. }
function GetIndexFromResolution(XRes, YRes, BPP: Integer): TBZScreenModeResolution;

{ Change le mode vidéo de l'écran en fonction de 'ModeIndex' dans la liste des modes videos disponibles }
function SetFullscreenMode(modeIndex: TBZScreenModeResolution;  displayFrequency: Integer = 0): Boolean;

{ Restaure le mode vidéo par défaut }
procedure RestoreDefaultScreenMode;

//procedure ReadVideoModes;

var
  { Retourne le nombre de mode video disponible }
  vNumberVideoModes : Integer = 0;
  { Retourne l'index du mode video actuel de la liste }
  vCurrentVideoMode : Integer = 0;
  { Drapeau indiquant si le mode video à été changé }
  vScreenModeChanged : Boolean = false;
{$IFDEF WINDOWS}
  { Liste des modes videos disponibles (Windows) }
  vVideoModes: array of TBZVideoMode;
{$ENDIF}
{$IFDEF LINUX}
  { Pointeur vers les propriétés d'affichage (Linux) }
  vDisplay: PDisplay;
  { Liste des modes videos disponibles (Linux) }
  vVideoModes: array of PXF86VidModeModeInfo;
  { Informations video du bureau (Linux uniquement) }
  vDesktop: TXF86VidModeModeInfo;
{$ENDIF}


implementation

uses
  SysUtils;

{%region=====[ General tools ]=========================================================================}

type
  TLowResMode = packed record
    Width: Word;
    Height: Word;
    ColorDepth: Byte;
  end;

const
  NumberLowResModes = 15;
{$IFDEF WINDOWS}
  LowResModes: array [0 .. NumberLowResModes - 1] of TLowResMode = (
    (Width: 320; Height: 200; ColorDepth: 8), 
	  (Width: 320; Height: 200; ColorDepth: 15),
    (Width: 320; Height: 200; ColorDepth: 16), 
	  (Width: 320; Height: 200; ColorDepth: 24),
	  (Width: 320; Height: 200; ColorDepth: 32),
	  (Width: 400; Height: 300; ColorDepth: 8),
	  (Width: 400; Height: 300; ColorDepth: 15),
    (Width: 400; Height: 300; ColorDepth: 16), 
	  (Width: 400; Height: 300; ColorDepth: 24),
	  (Width: 400; Height: 300; ColorDepth: 32),
	  (Width: 512; Height: 384; ColorDepth: 8),
	  (Width: 512; Height: 384; ColorDepth: 15),
    (Width: 512; Height: 384; ColorDepth: 16), 
	  (Width: 512; Height: 384; ColorDepth: 24),
	  (Width: 512; Height: 384; ColorDepth: 32));
{$ENDIF}

function GetIndexFromResolution(XRes, YRes, BPP: Integer): TBZScreenModeResolution;
var
  I: Integer;
  XDiff, YDiff: Integer;
{$IFDEF WINDOWS}
  CDiff: Integer;
{$ENDIF}
begin
  //ReadVideoModes;
  // prepare result in case we don't find a valid mode
  Result := 0;
  // set differences to maximum
  XDiff := 9999;
  YDiff := 9999;
  {$IFDEF WINDOWS}
  CDiff := 99;
  {$ENDIF}
  for I := 1 to vNumberVideoModes - 1 do
  {$IFDEF WINDOWS}
    with vVideoModes[I] do
    begin
      if (Width >= XRes) and ((Width - XRes) <= XDiff) and (Height >= YRes) and
        ((Height - YRes) <= YDiff) and (ColorDepth >= BPP) and
        ((ColorDepth - BPP) <= CDiff) then
      begin
        XDiff := Width - XRes;
        YDiff := Height - YRes;
        CDiff := ColorDepth - BPP;
        Result := I;
      end;
    end;
  {$ENDIF}
  {$IFDEF LINUX}
  with vVideoModes[I]^ do
  begin
    if (hDisplay >= XRes) and ((hDisplay - XRes) <= XDiff) and
      (vDisplay >= YRes) and ((vDisplay - YRes) <= YDiff) then
    begin
      XDiff := hDisplay - XRes;
      YDiff := vDisplay - YRes;
      Result := I;
    end;
  end;
 {$ENDIF}
 {$IFDEF Darwin}
  begin
   {$MESSAGE Warn 'Needs to be implemented'}
  end;
{$ENDIF}
end;

{$IFDEF WINDOWS}
procedure TryToAddToList(deviceMode: TDevMode);
// Adds a video mode to the list if it's not a duplicate and can actually be set.
var
  I: Integer;
  vm: PVideoMode;
begin
  // See if this is a duplicate mode (can happen because of refresh
  // rates, or because we explicitly try all the low-res modes)
  for I := 1 to vNumberVideoModes - 1 do
  begin
    with deviceMode do
    begin
      vm := @vVideoModes[I];
      if ((dmBitsPerPel = vm^.ColorDepth) and (dmPelsWidth = vm^.Width) and (dmPelsHeight = vm^.Height)) then
      begin
        // it's a duplicate mode, higher frequency?
        if dmDisplayFrequency > vm^.MaxFrequency then vm^.MaxFrequency := dmDisplayFrequency;
        Exit;
      end;
    end;
  end;

  // do a mode set test (doesn't actually do the mode set, but reports whether it would have succeeded).
  if ChangeDisplaySettings(deviceMode, CDS_TEST or CDS_FULLSCREEN) <> DISP_CHANGE_SUCCESSFUL then Exit;

  // it's a new, valid mode, so add this to the list
  vm := @vVideoModes[vNumberVideoModes];
  with deviceMode do
  begin
    vm^.ColorDepth := dmBitsPerPel;
    vm^.Width := dmPelsWidth;
    vm^.Height := dmPelsHeight;
    vm^.MaxFrequency := dmDisplayFrequency;
    vm^.Description := Format('%d x %d, %d bpp', [dmPelsWidth, dmPelsHeight, dmBitsPerPel]);
  end;
  Inc(vNumberVideoModes);
end;
{$ENDIF}
{$IFDEF LINUX}
  procedure TryToAddToList(); // Without input parameters.
  begin
    XF86VidModeGetAllModeLines(vDisplay, vCurrentVideoMode, @vNumberVideoModes,@vVideoModes[0]);
  end;
{$ENDIF}
{$IFDEF Darwin}
procedure TryToAddToList(); // Without input parameters.
begin
  {$MESSAGE Warn 'Needs to be implemented. Same as Linux X11 ????'}
end;
{$ENDIF}


procedure ReadVideoModes;
{$IFDEF WINDOWS}
var
  I, ModeNumber: Integer;
  done: Boolean;
  deviceMode: TDevMode;
  DeskDC: HDC;
begin
  if vNumberVideoModes > 0 then Exit;

  SetLength(vVideoModes, MaxVideoModes);
  vNumberVideoModes := 1;

  // prepare 'default' entry
  DeskDC := GetDC(0);
  with vVideoModes[0] do
  try
    ColorDepth := GetDeviceCaps(DeskDC, BITSPIXEL) * GetDeviceCaps(DeskDC, PLANES);
    Width := Screen.Width;
    Height := Screen.Height;
    Description := 'default';
  finally
    ReleaseDC(0, DeskDC);
  end;

  // enumerate all available video modes
  ModeNumber := 0;
  repeat
  done := not EnumDisplaySettings(nil, ModeNumber, deviceMode{%H-});
  TryToAddToList(deviceMode);
  Inc(ModeNumber);
  until (done or (vNumberVideoModes >= MaxVideoModes));

  // low-res modes don't always enumerate, ask about them explicitly
  with deviceMode do
  begin
    dmBitsPerPel := 8;
    dmPelsWidth := 42;
    dmPelsHeight := 37;
    dmFields := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
  // make sure the driver doesn't just answer yes to all tests
    if ChangeDisplaySettings(deviceMode, CDS_TEST or CDS_FULLSCREEN) <> DISP_CHANGE_SUCCESSFUL then
    begin
      I := 0;
      while (I < NumberLowResModes - 1) and
        (vNumberVideoModes < MaxVideoModes) do
      begin
        dmSize := Sizeof(deviceMode);
        with LowResModes[I] do
        begin
          dmBitsPerPel := ColorDepth;
          dmPelsWidth := Width;
          dmPelsHeight := Height;
        end;
        dmFields := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
        TryToAddToList(deviceMode);
        Inc(I);
      end;
    end;
  end;
end;
{$ENDIF}
{$IFDEF LINUX}
var
  I, j: Integer;
begin
  SetLength(vVideoModes, MaxVideoModes);
  // if error usr/bin/ld: cannot find -lXxf86vm
  // then sudo apt-get install libXxf86vm-dev

  // Connect to XServer
  vDisplay := XOpenDisplay(nil);
  if not Assigned(vDisplay) Then
    Assert(False, 'Not conected with X Server');

  vCurrentVideoMode := DefaultScreen(vDisplay);

// Check support XF86VidMode Extension
// {$IF (FPC_VERSION = 2) and (FPC_RELEASE < 5)}
// if XF86VidModeQueryExtension( vDisplay, @i, @j )=0 then
// {$ELSE}
   if not XF86VidModeQueryExtension(vDisplay, @I, @j) then
     Assert(False, 'XF86VidMode Extension not support');

  // Get Current Settings
  if not vScreenModeChanged then
    if XF86VidModeGetModeLine(vDisplay, vCurrentVideoMode,
      @vDesktop.dotclock, PXF86VidModeModeLine(PtrUInt(@vDesktop) + Sizeof(vDesktop.dotclock))) then TryToAddToList;

  XCloseDisplay(vDisplay);
end;
{$ENDIF}
{$IFDEF Darwin}
begin
  {$MESSAGE Warn 'ReadVideoModes not yet implemented for Darwin platforms'}
end;
{$ENDIF}


function SetFullscreenMode(modeIndex: TBZScreenModeResolution; displayFrequency: Integer = 0): Boolean;
{$IFDEF WINDOWS}
var
  deviceMode: TDevMode;
begin
  //ReadVideoModes;
  FillChar(deviceMode{%H-}, Sizeof(deviceMode), 0);
  with deviceMode do
  begin
    dmSize := Sizeof(deviceMode);
    dmBitsPerPel := vVideoModes[modeIndex].ColorDepth;
    dmPelsWidth := vVideoModes[modeIndex].Width;
    dmPelsHeight := vVideoModes[modeIndex].Height;
    dmFields := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
    if displayFrequency > 0 then
    begin
      dmFields := dmFields or DM_DISPLAYFREQUENCY;
      if displayFrequency > vVideoModes[modeIndex].MaxFrequency then displayFrequency := vVideoModes[modeIndex].MaxFrequency;
      dmDisplayFrequency := displayFrequency;
    end;
  end;
  Result := ChangeDisplaySettings(deviceMode, CDS_FULLSCREEN) = DISP_CHANGE_SUCCESSFUL;
  if Result then
  begin
    vCurrentVideoMode := modeIndex;
    vScreenModeChanged := true;
  end;
end;
{$ENDIF}
{$IFDEF LINUX}
var
  vSettings: TXF86VidModeModeInfo;
  wnd: TWindow;
begin
  //ReadVideoModes;
  vDisplay := XOpenDisplay(nil);
  vSettings := vVideoModes[modeIndex]^;
  if (vSettings.hDisplay <> vDesktop.hDisplay) and (vSettings.vDisplay <> vDesktop.vDisplay) then
  begin
    // vsettings.vtotal:=vsettings.vdisplay;
    XF86VidModeSwitchToMode(vDisplay, vCurrentVideoMode, @vSettings);
    XF86VidModeSetViewPort(vDisplay, vCurrentVideoMode, 0, 0);
    wnd := XDefaultRootWindow(vDisplay);
    XGrabPointer(vDisplay, wnd, true, PointerMotionMask + ButtonReleaseMask, GrabModeAsync, GrabModeAsync, wnd, none, 0);
    vScreenModeChanged := true;
  end
  else
  begin
    // Restore
    XF86VidModeSwitchToMode(vDisplay, vCurrentVideoMode, @vDesktop);
    vScreenModeChanged := False;
  end;
  // Disconnect to XServer else settings not accept
  XCloseDisplay(vDisplay);
  Result := vScreenModeChanged;
end;
{$ENDIF}
{$IFDEF Darwin}
begin
  {$MESSAGE Warn 'Needs to be implemented'}
end;
{$ENDIF}

procedure RestoreDefaultScreenMode;
{$IFDEF WINDOWS}
var
  t: PDevMode;
begin
  t := nil;
  ChangeDisplaySettings(t^, CDS_FULLSCREEN);
  vScreenModeChanged := False;
  vCurrentVideoMode := 0;
end;
{$ENDIF}
{$IFDEF LINUX}
begin
  // if vCurrentVideoMode=0 then
  //ReadVideoModes;
  vDisplay := XOpenDisplay(nil);
  XF86VidModeSwitchToMode(vDisplay, vCurrentVideoMode, @vDesktop);
  vScreenModeChanged := False;
  XCloseDisplay(vDisplay);
end;
{$ENDIF}
{$IFDEF Darwin}
begin
  {$MESSAGE Warn 'Needs to be implemented'}
end;
{$ENDIF}

{%endregion%}

{%region=====[ TBZScreenMode]==============================================================================}

constructor TBZScreenMode.Create(AOwner : TComponent);
Var
  i : Integer;
begin
  inherited Create(AOwner);
  Assert(not(AOwner is TForm),'Owner must be a FORM');
  FForm := TForm(AOwner);
  FInitialFormWindowState := FForm.WindowState;
  FInitialFormPosition := FForm.Position;
  FInitialFormStyle := FForm.FormStyle;
  FInitialFormBorderStyle := FForm.BorderStyle;
  FFullScreenForm := True;
  FFullScreenMode := False;
  FCurrentVideoModeIndex := 0;
  FActualScreenWidth := Screen.Width;
  FActualScreenHeight := Screen.Height;
  FAvailableVideoModeCount := vNumberVideoModes;
  FAvailableVideoMode := TStringList.Create;
  FXRes := FForm.Width;
  FYRes := FForm.Height;
  FFrequency := 60;
  FBPP := 32;
  if FAvailableVideoModeCount>0 then
  begin
    for i := 0 to FAvailableVideoModeCount-1 do
    begin
      {$IFDEF WINDOWS}
        FAvailableVideoMode.Add(vVideoModes[i].Width.ToString + 'x' + vVideoModes[i].Height.ToString + 'x' +vVideoModes[i].ColorDepth.ToString);
      {$ENDIF}
      {$IFDEF LINUX}
        FAvailableVideoMode.Add(vVideoModes[i]^.hTotal.ToString + 'x' + vVideoModes[i]^.vTotal.ToString + 'x24' );
      {$ENDIF}
    end;
  end;
end;

destructor TBZScreenMode.Destroy;
begin
  if FFullScreenMode then RestoreVideoMode;
  FreeAndNil(FAvailableVideoMode);
  inherited Destroy;
end;

procedure TBZScreenMode.SetAvailableVideoMode(const AValue : TStrings);
begin
  if FAvailableVideoMode = AValue then Exit;
  FAvailableVideoMode := AValue;
end;

procedure TBZScreenMode.SetBPP(const AValue : Integer);
begin
  if FBPP = AValue then Exit;
  FBPP := AValue;
end;

procedure TBZScreenMode.SetFrequency(const AValue : Integer);
begin
  if FFrequency = AValue then Exit;
  FFrequency := AValue;
end;

procedure TBZScreenMode.SetFullScreenForm(const AValue : Boolean);
begin
  if FFullScreenForm = AValue then Exit;
  FFullScreenForm := AValue;
end;

procedure TBZScreenMode.SetOnBeforeChangeVideoMode(const AValue : TBZScreenModeChangeVideoModeEvent);
begin
  if FOnBeforeChangeVideoMode = AValue then Exit;
  FOnBeforeChangeVideoMode := AValue;
end;

procedure TBZScreenMode.SetOnAfterChangeVideoMode(const AValue : TBZScreenModeChangeVideoModeEvent);
begin
  if FOnAfterChangeVideoMode = AValue then Exit;
  FOnAfterChangeVideoMode := AValue;
end;

procedure TBZScreenMode.SetOnAfterRestoreVideoMode(const AValue : TNotifyEvent);
begin
  if FOnAfterRestoreVideoMode = AValue then Exit;
  FOnAfterRestoreVideoMode := AValue;
end;

procedure TBZScreenMode.SetOnBeforeRestoreVideoMode(const AValue : TNotifyEvent);
begin
  if FOnBeforeRestoreVideoMode = AValue then Exit;
  FOnBeforeRestoreVideoMode := AValue;
end;

procedure TBZScreenMode.SetXRes(const AValue : Integer);
begin
  if FXRes = AValue then Exit;
  FXRes := AValue;
end;

procedure TBZScreenMode.SetYRes(const AValue : Integer);
begin
  if FYRes = AValue then Exit;
  FYRes := AValue;
end;

procedure TBZScreenMode.ChangeVideoMode;
var
  idx : Integer;
begin
  idx := GetIndexFromResolution(FXRes,FYRes,FBPP);
  if Assigned(FOnBeforeChangeVideoMode) then OnBeforeChangeVideoMode(Self,FXRes, FYRes, FBpp, FFrequency, Idx);
  if idx>0 then
  begin
    if SetFullscreenMode(idx,FFrequency) then
    begin
      if FFullScreenForm then
      begin
        FForm.WindowState := wsMaximized; //wsFullScreen;
        FForm.Position := poScreenCenter;
        FForm.FormStyle := fsSystemStayOnTop; //fsNormal
        FForm.BorderStyle := bsNone;
        FFullScreenMode := True;
        FCurrentVideoModeIndex := idx;
        if Assigned(FOnAfterChangeVideoMode) then OnAfterChangeVideoMode(Self,FXRes, FYRes, FBpp, FFrequency, Idx);
      end;
      FScreenModeChanged := true;
    end;
  end;
end;

procedure TBZScreenMode.RestoreVideoMode;
begin
  if FFullScreenMode then
  begin
    if Assigned(FOnBeforeRestoreVideoMode) then OnBeforeRestoreVideoMode(Self);
    if FFullScreenForm then
    begin
      FForm.WindowState := FInitialFormWindowState;
      FForm.Position := FInitialFormPosition;
      FForm.FormStyle := FInitialFormStyle;
      FForm.BorderStyle := FInitialFormBorderStyle;
      FFullScreenMode := False;
    end;
    RestoreDefaultScreenMode;
    FScreenModeChanged := False;
    FCurrentVideoModeIndex := 0;
    if Assigned(FOnAfterRestoreVideoMode) then OnAfterRestoreVideoMode(Self);
  end;
end;

procedure TBZScreenMode.ShowMouseCursor;
begin
  {$IFDEF WINDOWS}
    ShowCursor(True);
  {$ENDIF}
  {$IFDEF UNIX}
    FForm.Cursor:=crDefault;
  {$ENDIF}
end;

procedure TBZScreenMode.HideMouseCursor;
begin
  {$IFDEF WINDOWS}
    ShowCursor(False);
  {$ENDIF}
   FForm.Cursor:=crNone;

end;

function TBZScreenMode.GetNativeMousePos : TPoint;
Var
  p : TPoint;
  {$IFDEF LINUX}
    dpy: PDisplay;
    root, child: TWindow;
    rootX, rootY, winX, winY: Integer;
    xstate: Word;
    Res: Boolean;
  {$ENDIF}

begin
  p.x := 0;
  p.y := 0;
  {$IFDEF WINDOWS}
    GetCursorPos(p);
  {$ENDIF}
  {$IFDEF LINUX}
   dpy := XOpenDisplay(nil);
   Res := LongBool(XQueryPointer(dpy, XDefaultRootWindow(dpy), @root, @child, @rootX, @rootY, @winX, @winY, @xstate));
   If Res then
   begin
     p.x := rootX;
     p.y := rootY;
   end;
   XCloseDisplay(dpy);
  {$ENDIF}
  {$IFDEF Darwin}
    {$MESSAGE Warn 'Needs to be implemented'}
  {$ENDIF}
  Result := p;
end;

function TBZScreenMode.GetMousePos : TBZPoint;
Var
  p : TPoint;
begin
  p := Self.GetNativeMousePos;
  Result.Create(p.x, p.y);
end;

procedure TBZScreenMode.SetMousePos(MouseScreenX, MouseScreenY : Integer);
{$IFDEF WINDOWS}
begin
  SetCursorPos(MouseScreenX, MouseScreenY);
end;
{$ENDIF}
{$IFDEF LINUX}
var
  dpy: PDisplay;
  root: TWindow;
begin
  dpy := XOpenDisplay(nil);
  root := RootWindow(dpy, DefaultScreen(dpy));
  XWarpPointer(dpy, none, root, 0, 0, 0, 0, MouseScreenX, MouseScreenY);
  XCloseDisplay(dpy);
end;
{$ENDIF}
{$IFDEF Darwin}
begin
  {$MESSAGE Warn 'Needs to be implemented'}
end;
{$ENDIF}

procedure TBZScreenMode.SetMousePos(P : TBZPoint);
begin
  Self.SetMousePos(p.X, p.Y);
end;

procedure TBZScreenMode.SetMousePos(P : TPoint);
begin
  Self.SetMousePos(p.X, p.Y);
end;


{%endregion%}

initialization

   ReadVideoModes;

finalization

if vScreenModeChanged then
{$IFDEF WINDOWS}
  if vCurrentVideoMode <> 0 then
{$ENDIF}
    RestoreDefaultScreenMode; // set default video mode
end.
