(*
  @abstract(Contient un composant pour gérer et interagir avec les raccourcis clavier. @br
  TBZHotKeyManager permet d'intercepter les raccourcis clavier depuis le système (Application minimisée) et dans la fenêtre active en cours.)

  --------------------------------------------------------------------------------

  @created(2016-11-16)
  @author(J.Delauney (BeanzMaster))
  Historique :
  @unorderedList(
    @item(10/07/2019 : Creation  )
  )

  --------------------------------------------------------------------------------

  @bold(Notes) :
    - Compatible Windows et Linux  (Gnome, KDE, XFCE...)
    - Sous MacOS fonctionne si vous utiliser GTK et X11

  --------------------------------------------------------------------------------

  @bold(Dependances) :

  --------------------------------------------------------------------------------

  @bold(Credits :)
   @unorderedList(
     @item ()
     @item(J.Delauney (BeanzMaster))
   )

  --------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  -------------------------------------------------------------------------------- *)
unit BZHotKeyManager;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\bzscene_options.inc}

//==============================================================================

interface

//==============================================================================

uses

  Classes, SysUtils, LCLProc, LCLType, LCLIntf, LResources, LMessages,
  Forms, Controls, Graphics, Dialogs,
  {$IFDEF WINDOWS}
   windows
  {$ENDIF}
  {$IFDEF LINUX}
   Unix, x, xlib, gdk2, gdk2x
  {$ENDIF};


{.$IFDEF DARWIN}
  // Il semblrerait qu'en utilisant "NSEvent" il est possible d'intercepter les evenements clavier
  // à la fois sous Carbon et Cocoa. Mais aucune idées comment implémenter ça.
{.$IFDEF CARBON}
  // FPCMacOSAll,
{.$ELSE}

{.$ENDIF}
{.$ENDIF}

{$IFDEF UNIX}
Const
  MOD_SHIFT   = $2000;  //< scShift
  MOD_CONTROL = $4000;  //< scCtrl
  MOD_ALT     = $8000;  //< scAlt
  MOD_WIN     = $10000; //< scWin
{$ENDIF}
// ==============================================================================

Type
  { Methode de déclenchement d'une action, lors de l'appui d'un raccourci clavier }
  TOnHotKeyEvent = procedure(Index: Integer) of object;

Type
  { Permet de définir un raccourci clavier }
  TBZHotKeyItem = class(TCollectionItem)
  private
    FHotKey: TShortCut; // Cardinal;
    FKeyIndex: Integer;
    FOnExecute: TNotifyEvent;
  protected
    procedure setKeyIndex(AValue: Integer);
  public
    constructor Create(ACollection: TCollection); override;

  published
    { Definition du raccourci }
    property HotKey: TShortCut read FHotKey write FHotKey;
    { Index du raccourci dans la liste}
    property Index: Integer read FKeyIndex;
    { Evènement déclencher à l'appui du raccourci }
    property OnExecute: TNotifyEvent Read FOnExecute Write FOnExecute stored;
  end;

  { Liste des raccourcis clavier }
  TBZHotKeyList = class(TCollection)
  private
    function GetItems(Index: Integer): TBZHotKeyItem;
    procedure SetItems(Index: Integer; AValue: TBZHotKeyItem);
  public
    constructor Create;
  public
    { Ajout d'un raccourci à la liste }
    function Add: TBZHotKeyItem;
    { Acces aux raccourcis de la liste }
    property Items[Index: Integer]: TBZHotKeyItem read GetItems  write SetItems; default;
  end;

{$IFDEF LINUX}
Type
  { TBZHotKeyManager : Gestionnaire de raccourcis clavier }
  TBZHotKeyManager = class;


Type
  { TBZXEventListener : Ecoute des évènements clavier sous Linux }
  TBZXEventListener = class(TThread)
  private
    FApplicationHotKeys: TBZHotKeyManager;
  protected
    procedure Execute; override;
  public
    constructor Create(AAppHotKey: TBZHotKeyManager);
  end;
{$ENDIF}

type
  { Gestionnaire de raccourcis clavier standard }
  TBZHotKeyManager = class(TComponent)
  private
    FHotKeyList: TBZHotKeyList;
    // FOnHotKey : TOnHotKeyEvent; //TNotifyEvent;

    // ----------------------------------------------------------------------------
    // Variables pour intercepter les "HotKeys" sous Linux
    {$IFDEF LINUX}
    FXEventListener: TThread;
    // Un thread pour intercepter les messages du systeme
    {$ENDIF}
    // --------------------------------------------------------------------------
    // Fonctions pour intercepter les "HotKeys" sous Windows
    {$IFDEF WINDOWS}
    function CreateAppWindow: boolean;
    {$ENDIF}
    // --------------------------------------------------------------------------
    // Fonctions internes du composant pour construire et detruire les raccourcis
    function DoRegisterHotKey(hk: Cardinal): Integer;
    function DoUnregisterHotKey(KeyIndex: Integer): boolean;

  protected
    FActive: boolean;

    // Lancement de notre evenement à l'interception d'un raccourci
    procedure DoOnHotKey(Index: Integer); virtual;

    procedure Loaded; override;

    procedure SetActive(Value: boolean); virtual;

    // --------------------------------------------------------------------------
    // Fonctions pour intercepter les "HotKeys" sous Linux
   {$IFDEF LINUX}
    procedure WaitForXevent;
   {$ENDIF}


    // Fonctions pour intercepter les "HotKeys" sous Mac
    {$IFDEF DARWIN}
    {$ENDIF}
  public
    { Public declarations }
   {$IFDEF LINUX}
    Display: PDisplay;
   {$ENDIF}

    { Creation du composant TBZHotKeyManager }
    constructor Create(AOwner: TComponent); override;
    { Destruction du composant TBZHotKeyManager }
    destructor Destroy; override;

    { Creation d'un raccourci clavier. Exemple : CreateHotKey(MOD_CONTROL+MOD_ALT,VK_G); Correspond à CTRL+ALT+G  }
    function CreateHotKey(Modifiers, Key: Word): Cardinal;

    { Ajoute un raccourci clavier à la liste }
    function AddHotKey(HotKey: Cardinal): Integer;
    { Modifie un raccourci clavier à la liste }
    function ChangeHotKey(Index: Integer; NewHotKey: Cardinal): Integer;
    { Efface un raccourci clavier à la liste }
    function RemoveHotKey(HotKey: Cardinal): boolean;
    { Efface le raccourci clavier à l'index "Index" de la liste  }
    function RemoveHotKeyByIndex(Index: Integer): boolean;
    { Efface tous les raccourcis clavier de la liste }
    procedure ClearHotKeys;
    { Retourne @True si le raccourci clavier est valide }
    function HotKeyValid(HotKey: Cardinal): boolean;
    { Vérifie si un raccourci est deja inscrit dans le systeme. Retourne @True si le raccourci clavier peu être utilisé }
    function HotKeyExist(Index: Integer): Integer;
    { Efface et libère le raccourci clavier de la liste }
    function FreeHotKey(Index: Integer): boolean;

  published
    { Gestion des raccourcis actif ou pas }
    property Active: boolean read FActive write SetActive default false;
    { Liste des raccourcis }
    property HotKeys: TBZHotKeyList read FHotKeyList write FHotKeyList;

    // property OnHotKey : TOnHotKeyEvent read FOnHotKey write FOnHotKey;
  end;

  { Gestionnaire de raccourcis clavier au niveau d'une fenêtre }
  TBZFormHotKeyManager = class(TBZHotKeyManager)
  private

  protected
    FInitialized: boolean;
    FParentForm: TForm;

    FSaveProc: TShortCutEvent;

    procedure DoOnFormShortCut(var Msg: TLMKey; var Handled: boolean); virtual;
    procedure Loaded; override;
    procedure SetActive(Value: boolean); override;

  public
    { Creation du composant TBZHotKeyManager }
    constructor Create(AOwner: TComponent); override;
    { Destruction du composant TBZHotKeyManager }
    destructor Destroy; override;

  published
    { Published declarations }
    property Active;
    property HotKeys;
  end;

  { Gestionnaire de raccourcis clavier au niveau de l'application }
  TBZAppHotKeyManager = class(TBZFormHotKeyManager)
  private

  protected
    procedure DoOnApplicationShortCut(var Msg: TLMKey; var Handled: boolean); virtual;
    procedure SetActive(Value: boolean); override;
    procedure Loaded; override;

  public
    { Creation du composant TBZHotKeyManager }
    constructor Create(AOwner: TComponent); override;
    { Destruction du composant TBZHotKeyManager }
    destructor Destroy; override;

  published
    property Active;
    property HotKeys;
  end;

//==============================================================================

//procedure SeparateHotKey(HotKey: Cardinal; var Modifiers, Key: Word);

//==============================================================================

implementation

// ==============================================================================
{$IFDEF WINDOWS}

const
  WinClassName: string = 'HOTKEYBZAPPLICATIONHOTKEY';
  HotKeyAtomPrefix: string = 'BeanzHotKeyAtom';

var
  // ----------------------------------------------------------------------------
  // Variables pour intercepter les "HotKeys" sous Windows
  HWindow: HWND;
  WindowClassAtom: ATOM;       // Renvoyer par RegisterWindowClass en cas de succes.
  WindowClassInfo: WNDCLASSEX; // Structure des infos d'une fenetre
{$ENDIF}

{%region=====[ Fonctions Internes ]=============================================================}

{ Separe les touches normal et les touches de controle du Hotkeys pour les utiliser avec RegisterHotKey }
procedure SeparateHotKey(HotKey: Cardinal; var Modifiers, Key: Word);
{$IFDEF WINDOWS}
const
  VK2_SHIFT = 32;
  VK2_CONTROL = 64;
  VK2_ALT = 128;
  VK2_WIN = 256;
{$ENDIF}
var
  Virtuals: Integer;
  V: Word;
  x: Word;

  {$IFDEF LINUX}
  i: Integer;

  function correctModifiers(Modifiers: Word; flags: Integer): Integer;
  var
    ret: Word;
  begin
    ret := Modifiers;
    if ((flags and 1) <> 0) then
      ret := ret OR LockMask;
    if ((flags and 2) <> 0) then
      ret := ret OR Mod2Mask;
    if ((flags and 4) <> 0) then
      ret := ret OR Mod3Mask;
    if ((flags and 8) <> 0) then
      ret := ret OR Mod5Mask;
    Result := ret;
  end;
  {$ENDIF}

begin
  Key := Byte(HotKey);
  x := HotKey shr 8;
  Virtuals := x;

  {$IFDEF WINDOWS}
  V := 0;
  if (Virtuals and VK2_WIN) <> 0 then Inc(V, MOD_WIN);
  if (Virtuals and VK2_ALT) <> 0 then Inc(V, MOD_ALT);
  if (Virtuals and VK2_CONTROL) <> 0 then Inc(V, MOD_CONTROL);
  if (Virtuals and VK2_SHIFT) <> 0 then Inc(V, MOD_SHIFT);
  Modifiers := V;
  {$ENDIF}

  {$IFDEF LINUX}
  V := Virtuals;
  for i := 0 to 15 do
  begin
    V := correctModifiers(V, i);
  end;
  Modifiers := V;
  {$ENDIF}
  {$IFDEF DARWIN}
  {$ENDIF}
end;

{$IFDEF WINDOWS}

{ Handler pour Intercepter les message WM_HOTKEY
  Nb: Vous pouvez intercepter d'autre messages comme ceux de la souris, suffit d'ajouter
  des conditions au CASE ce qui permettrai avec un peu d'ingéniosité de faire des HotKeys
  genre : CRTL + ALT + G + SOURIS_BOUTON_GAUCHE }
function WinProc(hw: HWND; uMsg: UINT; wp: WPARAM; lp: LPARAM): LRESULT;
  stdcall; export;
var
  obj: TBZHotKeyManager;
  idx: Integer;
begin
  Result := 0;
  case uMsg of
    WM_HOTKEY:
      begin
        obj := TBZHotKeyManager(GetWindowLongPtr(HWindow, GWL_USERDATA));
        idx := obj.HotKeyExist(Longint(wp));
        if (idx > -1) and (obj.Active) then
          obj.DoOnHotKey(idx);
      end
  else
    Result := DefWindowProc(hw, uMsg, wp, lp);
  end;
end;
{$ENDIF}

{ Récupère la fenètre propriétaire du composant }
function GetOwnerForm(AComponent: TComponent): TCustomForm;
var
  LOwner: TComponent;
begin
  LOwner := AComponent.Owner;
  if (LOwner = nil) or (LOwner is TCustomForm) then
    Result := TCustomForm(LOwner)
  else
    Result := GetOwnerForm(LOwner);
end;

{%endregion%}

{%region=====[ TBZHotKeyItem ]==================================================================}

constructor TBZHotKeyItem.Create(ACollection: TCollection);
begin
  if Assigned(ACollection) and (ACollection is TBZHotKeyList) then
    inherited Create(ACollection);
end;

procedure TBZHotKeyItem.setKeyIndex(AValue: Integer);
begin
  if FKeyIndex = AValue then
    exit;
  FKeyIndex := AValue;
end;

{%endregion%}

{%region=====[ TBZHotKeyList ]==================================================================}

constructor TBZHotKeyList.Create;
begin
  inherited Create(TBZHotKeyItem);
end;

function TBZHotKeyList.GetItems(Index: Integer): TBZHotKeyItem;
begin
  Result := TBZHotKeyItem(inherited Items[Index]);
end;

procedure TBZHotKeyList.SetItems(Index: Integer; AValue: TBZHotKeyItem);
begin
  Items[Index].Assign(AValue);
end;

function TBZHotKeyList.Add: TBZHotKeyItem;
begin
  Result := inherited Add as TBZHotKeyItem;
end;

{%endregion%}

{%region=====[ TBZXEventListener ]==============================================================}

{$IFDEF LINUX}
constructor TBZXEventListener.Create(AAppHotKey: TBZHotKeyManager);
begin
  inherited Create(True);
  FApplicationHotKeys := AAppHotKey;
end;

{ Ecoute les evenements }
procedure TBZXEventListener.Execute;
begin
  while not(Terminated) do
  begin
    //if not Terminated then
    //begin
     Synchronize(@FApplicationHotKeys.WaitForXevent);
    //end;
    //if Terminated then exit;
  end;
end;

{$ENDIF}
{%endregion%}

{%region=====[ TBZHotKeyManager ]===============================================================}

constructor TBZHotKeyManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.SetSubComponent(True);
  // add this line if you want to put this class inside other and also be streamed
  FHotKeyList := TBZHotKeyList.Create;

  if not(csDesigning in ComponentState) then
  begin
  {$IFDEF WINDOWS}
    CreateAppWindow;
  {$ENDIF}

  {$IFDEF LINUX}
    FXEventListener := TBZXEventListener.Create(Self);
    Display := XOpenDisplay(gdk_get_display);
  {$ENDIF}
  {$IFDEF DARWIN}
  {$ENDIF}
  end;
end;

destructor TBZHotKeyManager.Destroy;
begin
  ClearHotKeys;

  {$IFDEF WINDOWS}
  DestroyWindow(HWindow);
  {$ENDIF}

  {$IFDEF LINUX}
  FXEventListener.Terminate;
  FXEventListener.WaitFor;
  FXEventListener.Free;
  XCloseDisplay(Display);
  {$ENDIF}
  {$IFDEF DARWIN}
  {$ENDIF}
  FHotKeyList.Free;
  inherited Destroy;
end;

{$IFDEF WINDOWS}
{ Creation d'une fenetre virtuelle avec notre Hook }
function TBZHotKeyManager.CreateAppWindow: boolean;

  function RegisterWindowClass: boolean;
  begin
    WindowClassInfo.cbSize := sizeof(WindowClassInfo);
    WindowClassInfo.Style := 0;
    WindowClassInfo.lpfnWndProc := @WinProc;
    WindowClassInfo.cbClsExtra := 0;
    WindowClassInfo.cbWndExtra := 0;
    WindowClassInfo.hInstance := hInstance;
    WindowClassInfo.hIcon := 0;
    WindowClassInfo.hCursor := 0;
    WindowClassInfo.hbrBackground := 0;
    WindowClassInfo.lpszMenuName := nil;
    WindowClassInfo.lpszClassName := PChar(WinClassName);
    WindowClassInfo.hIconSm := 0;
    WindowClassAtom := RegisterClassEx(WindowClassInfo);
    Result := WindowClassAtom <> 0;
  end;

begin
  Result := false;
  if not RegisterWindowClass then
  begin
    exit;
  end;
  HWindow := CreateWindowEx(WS_EX_NOACTIVATE or WS_EX_TRANSPARENT,
    PChar(WinClassName), PChar(WinClassName), Ws_popup or WS_CLIPSIBLINGS, 0, 0,
    0, 0, 0, 0, hInstance, nil);
  if HWindow <> 0 then
  begin
    ShowWindow(HWindow, SW_HIDE);
    SetWindowLongPtr(HWindow, GWL_USERDATA, PtrInt(Self));

    UpdateWindow(HWindow);
    Result := True;
    exit;
  end;
end;
{$ENDIF}

{ Ajoute un HotKey Global dans le systeme d'exploitation }
function TBZHotKeyManager.DoRegisterHotKey(hk: Cardinal): Integer;
var
  Modifiers, Key: Word;
  id: Integer;

  {$IFDEF LINUX}
  root: PGdkWindow;
  {$ENDIF}
begin
  Result := 0;
  Modifiers := 0;
  Key := 0;
  SeparateHotKey(hk, Modifiers, Key);

  {$IFDEF WINDOWS}
  id := GlobalAddAtom(PChar(HotKeyAtomPrefix + IntToStr(hk)));
  RegisterHotKey(HWindow, Longint(id), Modifiers, Key);
  Result := id;
  {$ENDIF}

  {$IFDEF LINUX}
  root := gdk_get_default_root_window();
  Display := GDK_WINDOW_XDISPLAY(root);
  gdk_error_trap_push;
  id := XGrabKey(Display, Key, Modifiers, gdk_x11_drawable_get_xid(root), 1, GrabModeAsync, GrabModeAsync);
  gdk_flush;
  Result := id; //(gdk_error_trap_pop); //= 0;
  {$ENDIF}
  {$IFDEF DARWIN}
  {$ENDIF}
end;

{ Supprime un HotKey Global dans le systeme d'exploitation }
function TBZHotKeyManager.DoUnregisterHotKey(KeyIndex: Integer): boolean;
{$IFDEF LINUX}
var
  root: PGdkWindow;
  Modifiers, Key: Word;
  hk: Cardinal;
{$ENDIF}
begin
{$IFDEF WINDOWS}
  Result := UnRegisterHotkey(HWindow, Longint(KeyIndex));
  GlobalDeleteAtom(KeyIndex);
{$ENDIF}

  {$IFDEF LINUX}
  root := gdk_get_default_root_window();
  Display := GDK_WINDOW_XDISPLAY(root);
  hk := Cardinal(FHotKeyList[HotKeyExist(KeyIndex)].HotKey);
  SeparateHotKey(hk, Modifiers, Key);
  XUngrabKey(Display, Key, Modifiers, gdk_x11_drawable_get_xid(root));
  Result := (gdk_error_trap_pop)= 0;
  {$ENDIF }
  {$IFDEF DARWIN}
  {$ENDIF}
end;

{ Lance une action lorsque le HotKey est intercepté }
procedure TBZHotKeyManager.DoOnHotKey(Index: Integer);
begin
  if Assigned(FHotKeyList[Index].OnExecute) then
    FHotKeyList[Index].OnExecute(Self);
end;

{ Active/Désactive l'interception des HotKeys
  Est utilisé par la version Linux et n'a aucun effet sous Windows }
procedure TBZHotKeyManager.SetActive(Value: boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
  {$IFDEF LINUX}
    if not(csDesigning in ComponentState) then
    begin
      if FActive then
      begin
        FXEventListener.Suspended := false;
      end
      else
      begin
        FXEventListener.Suspended := True;
      end
    end;
  {$ENDIF}
  end;
end;

{ Effectue une action lorsque le composant est chargé }
procedure TBZHotKeyManager.Loaded;
var
  i, j: Integer;
  // hk: TBZHotKeyItem;
  id: Integer;
begin
  inherited;
  if not(csDesigning in ComponentState) then
  begin
    j := FHotKeyList.Count;
    if j > 0 then
    begin
      for i := 0 to j - 1 do
      begin
        // hk:=;
        id := DoRegisterHotKey(Cardinal(FHotKeyList.Items[i].HotKey));
        if id > 0 then
        begin
          FHotKeyList.Items[i].setKeyIndex(id);
        end
        else
          Showmessage('Erreur');
      end;
    end;
  end;
end;

{$IFDEF LINUX}
{ Attente d'interception des raccourcis sous Linux }
procedure TBZHotKeyManager.WaitForXevent;
var
  event: TXEvent;
  i, j, state: Integer;
  Modifiers, Key: Word;
  xkey: TXKeyEvent;
  hk: TBZHotKeyItem;
begin
  // event := XEvent.Create();
  while (XPending(Display) > 0) do
  begin
    XNextEvent(Display, @event);
    if (event._type = KeyPress) then
    begin
      xkey := TXKeyEvent(event.xkey);
      state := xkey.state and (ShiftMask or ControlMask or Mod1Mask or
        Mod4Mask);
      j := FHotKeyList.Count - 1;
      for i := 0 to j do
      begin
        hk := FHotKeyList[i]; // ;^.HotKey;
          SeparateHotKey(Cardinal(hk.HotKey), Modifiers, Key);
        if (xkey.keycode = Key) and (state = Modifiers) then
        begin
          DoOnHotKey(i);
        end;
      end;
    end;
  end;
end;

{$ENDIF}

function TBZHotKeyManager.CreateHotKey(Modifiers, Key: Word): Cardinal;
const
  VK2_SHIFT = 32;
  VK2_CONTROL = 64;
  VK2_ALT = 128;
  VK2_WIN = 256;
var
  hk: Cardinal;
begin
  hk := 0;
  if (Modifiers and MOD_ALT) <> 0 then Inc(hk, VK2_ALT);
  if (Modifiers and MOD_CONTROL) <> 0 then Inc(hk, VK2_CONTROL);
  if (Modifiers and MOD_SHIFT) <> 0 then Inc(hk, VK2_SHIFT);
  if (Modifiers and MOD_WIN) <> 0 then Inc(hk, VK2_WIN);
  hk := hk shl 8;
  Inc(hk, Key);
  Result := hk;
end;

function TBZHotKeyManager.AddHotKey(HotKey: Cardinal): Integer;
var
  hk: TBZHotKeyItem; // PHotKey;
  id: Integer;
begin
  // Creation d'un ID Unique

  id := DoRegisterHotKey(HotKey);

  if id > 0 then
  begin
    hk := FHotKeyList.Add;
    hk.HotKey := TShortCut(HotKey);
    hk.setKeyIndex(id);

    // hk := New(PHotKey);
    // hk^.HotKey := HotKey;
    // hk^.KeyIndex := id;
    // FHotKeyList.Add(hk);

    Result := FHotKeyList.Count - 1;
  end
  else
  begin
    Result := -1;
  end;
end;

function TBZHotKeyManager.ChangeHotKey(Index: Integer;
  NewHotKey: Cardinal): Integer;
var
  i, j: Integer;
  hk: TBZHotKeyItem; // PHotKey;
begin
  Result := 0;
  j := FHotKeyList.Count - 1;
  for i := 0 to j do
  begin
    hk := FHotKeyList[i];
    if hk.Index = Index then
    begin
      RemoveHotKeyByIndex(Index);
      Result := AddHotKey(NewHotKey);
      exit;
    end;
  end;
end;

function TBZHotKeyManager.RemoveHotKey(HotKey: Cardinal): boolean;
var
  i, j: Integer;
  hk: TBZHotKeyItem; // PHotKey;
begin
  Result := false;
  j := FHotKeyList.Count - 1;
  for i := 0 to j do
  begin
    hk := FHotKeyList[i];
    if hk.HotKey = TShortCut(HotKey) then
    begin
      Result := FreeHotKey(i);
      FHotKeyList.Delete(i);
      exit;
    end;
  end;
end;

function TBZHotKeyManager.RemoveHotKeyByIndex(Index: Integer): boolean;
var
  i, j: Integer;
  hk: TBZHotKeyItem;
begin
  Result := false;
  j := FHotKeyList.Count - 1;
  for i := 0 to j do
  begin
    hk := FHotKeyList[i];
    if hk.Index = Index then
    begin
      Result := FreeHotKey(i);
      FHotKeyList.Delete(i);
      exit;
    end;
  end;
end;

function TBZHotKeyManager.HotKeyValid(HotKey: Cardinal): boolean;
var
  M, K: Word;
  WasRegistered: boolean;
{$IFDEF WINDOWS}
  ATOM: Word;
{$ENDIF}
begin
{$IFDEF WINDOWS}
  K := 0;
  M := 0;
  ATOM := GlobalAddAtom(PChar(HotKeyAtomPrefix + IntToStr(HotKey)));
  SeparateHotKey(HotKey, M, K);
  WasRegistered := UnRegisterHotkey(HWindow, ATOM);
  if WasRegistered then
  begin
    RegisterHotKey(HWindow, ATOM, M, K);
    Result := True;
  end
  else
  begin
    Result := RegisterHotKey(HWindow, ATOM, M, K);
    if Result then
      UnRegisterHotkey(HWindow, ATOM);
  end;
  GlobalDeleteAtom(ATOM);
{$ENDIF}

{$IFDEF LINUX}
  // @TODO : A Compléter
  Result := True;
{$ENDIF}
{$IFDEF DARWIN}
{$ENDIF}
end;

function TBZHotKeyManager.HotKeyExist(Index: Integer): Integer;
var
  i, j: Integer;
begin
  Result := -1;
  j := FHotKeyList.Count - 1;
  for i := 0 to j do
    if FHotKeyList[i].Index = Index then
    begin
      Result := i;
      Break;
    end;
end;

procedure TBZHotKeyManager.ClearHotKeys;
var
  i, j: Integer;
  // hk: PHotKey;
begin
  j := FHotKeyList.Count - 1;
  for i := j downto 0 do
  begin
    // hk := PHotKey(FHotKeyList[I]);
    FreeHotKey(i);
    FHotKeyList.Delete(i);
  end;
  // FHotKeyList.Clear;
end;

function TBZHotKeyManager.FreeHotKey(Index: Integer): boolean;
begin
  Result := DoUnregisterHotKey(FHotKeyList[Index].Index);

  // if Result then
  // Dispose(hk);
end;

{%endregion%}

{%region=====[ TBZFormHotKeyManager ]===========================================================}

constructor TBZFormHotKeyManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInitialized := false;
  FParentForm := TForm(GetOwnerForm(self));
  //self.SetSubComponent(true);
end;

destructor TBZFormHotKeyManager.Destroy;
begin
  inherited Destroy;
end;

{  Handler OnShortCut de la form }
procedure TBZFormHotKeyManager.DoOnFormShortCut(var Msg: TLMKey;var Handled: boolean);
var
  wShortCut: TShortCut;
  i: Integer;
  ShiftState: TShiftState; // Word;
begin
  Handled := false;

  ShiftState := MsgKeyDataToShiftState(Msg.KeyData);
  wShortCut := KeyToShortcut(Msg.CharCode, ShiftState);

  i := HotKeyExist(wShortCut);
  if i > -1 then
  begin
    DoOnHotKey(i);
    Handled := true;
  end;
  if Assigned(FSaveProc) then FSaveProc(Msg, Handled);
end;

procedure TBZFormHotKeyManager.SetActive(Value: boolean);
begin
  if FActive = Value then exit;
  FActive := Value;
  if not(csDesigning in ComponentState) and FInitialized then
  begin
    if FActive then
    begin
      FParentForm.OnShortcut := @DoOnFormShortCut;
    end
    else
    begin
      FParentForm.OnShortcut := FSaveProc;
    end
  end;
end;

{  Effectue une action lorsque le composant est chargé }
procedure TBZFormHotKeyManager.Loaded;
begin
  inherited;
  if Assigned(FParentForm.OnShortcut) then FSaveProc := FParentForm.OnShortcut
  else FSaveProc := nil;

  if Active then
  begin
    FParentForm.OnShortcut := @DoOnFormShortCut;
    FInitialized := true;
  end;
end;

{%endregion%}

{%region=====[ TBZAppHotKeyManager ]============================================================}

constructor TBZAppHotKeyManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

{  Destruction de notre composant }
destructor TBZAppHotKeyManager.Destroy;
begin
  inherited Destroy;
end;

{ Handler OnShortCut de la form }
procedure TBZAppHotKeyManager.DoOnApplicationShortCut(var Msg: TLMKey; var Handled: boolean);
var
  wShortCut: TShortCut;
  ShiftState: TShiftState;
  i: Integer;
begin
  Handled := false;

  ShiftState := MsgKeyDataToShiftState(Msg.KeyData);
  wShortCut := KeyToShortcut(Msg.CharCode, ShiftState);
  {$IFDEF WINDOWS}
  case Msg.Msg of
    WM_HOTKEY:
      begin
        // Get modifier keys status
        if (lo(Msg.KeyData) and MOD_SHIFT) <> 0 then  Include(ShiftState,ssShift);
        if (lo(Msg.KeyData) and MOD_CONTROL) <> 0 then Include(ShiftState, ssCtrl);
        if (lo(Msg.KeyData) and MOD_ALT) <> 0 then Include(ShiftState, ssAlt);
        if (lo(Msg.KeyData) and MOD_WIN) <> 0 then Include(ShiftState, ssSuper);
      end;
  end;
  {$ENDIF}
  wShortCut := KeyToShortcut(Msg.CharCode, ShiftState);

  i := HotKeyExist(wShortCut);
  if i > -1 then
  begin
    DoOnHotKey(i);
    Handled := true;
  end;
  if Assigned(FSaveProc) then
    FSaveProc(Msg, Handled);
end;

procedure TBZAppHotKeyManager.SetActive(Value: boolean);
begin
  if FActive = Value then exit;
  FActive := Value;
  if not(csDesigning in ComponentState) and FInitialized then
  begin
    if FActive then
    begin
      Application.OnShortcut := @DoOnApplicationShortCut;
    end
    else
    begin
      Application.OnShortcut := FSaveProc;
    end
  end;
end;

{ Effectue une action lorsque le composant est chargé }
procedure TBZAppHotKeyManager.Loaded;
begin
  inherited;
  if Assigned(FParentForm.OnShortcut) then FSaveProc := Application.OnShortcut
  else FSaveProc := nil;
  if Active then
  begin
    Application.OnShortcut := @DoOnApplicationShortCut;
    FInitialized := true;
  end;
end;

{%endregion%}

end.
