(*
  @abstract(Contient des méthodes qui Fournissent l'état à la demande de toute touche appuyée sur le clavier.@br
  Et un ensemble de fonctions pour travailler avec des codes de touches virtuelles.)

  Notez que Windows mappe les boutons de la souris avec des codes de touches virtuelles et que vous
  utiliser les fonctions / classes de cette unité pour vérifier également les boutons de la souris.@br
  Reportez-vous à la section 'Codes Touches virtuelles' dans les révisions des programmeurs Win32 pour une liste de
  constantes de code de touches (les constantes VK_ * sont déclarées dans l'unité 'Windows').

  --------------------------------------------------------------------------------

  @created(2018-05-04)
  @author(J.Delauney)
  Historique : @br
  @unorderedList(
    @item(04/05/2018 : Creation  )
  )

  --------------------------------------------------------------------------------

  @bold(Notes) :

  --------------------------------------------------------------------------------

  @bold(Dependances) : Aucune

  --------------------------------------------------------------------------------

  @bold(Credits :)@br
    @unorderedList(
      @item(J.Delauney (BeanzMaster))
    )

  --------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / LGPL

  -------------------------------------------------------------------------------- *)
unit BZKeyboard;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================

interface

uses
  lcltype, lclintf, classes
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  {$IFDEF LINUX}
   ,x, xlib, KeySym

  {$ENDIF}
  {$IFDEF DARWIN}
    { fatal: 'not implemented yet' }
    // Il semblerait qu'en utilisant "NSEvent" il soit possible d'intercepter les evenements clavier
    // à la fois sous Carbon et Cocoa. Mais aucunes idées de comment implémenter ça.
    // FPCMacOSAll
  {$ENDIF};

//==============================================================================

type
   { Code d'une touche virtuelle }
   TBZVirtualKeyCode = Integer;
   { Tampon de stockage, de l'état des touches pressées (code de 0 à 255) }
   TBZKeyBufferState = array[0..255] of byte;

const
   { pseudo touche pour la roulette de la souris vers le haut (nous réaffectons la touche F23), voir "KeyboardNotifyWheelMoved" }
   VK_MOUSEWHEELUP   = VK_F23;
   { pseudo touche pour la roulette de la souris vers le bas (nous réaffectons la touche F23), voir "KeyboardNotifyWheelMoved" }
   VK_MOUSEWHEELDOWN = VK_F24;

{ @abstract(Vérifie si la touche correspondant au "char" donné est appuyée.)

  Le caractère est mappé sur le "clavier principal"  uniquement, et non sur le clavier numérique.(virtuel) @br
  Les combinaisons Shift / Ctrl / Alt qui peuvent être nécessaires pour taper les caractères spéciaux sont ignorés. @br
  C'est à dire que 'a' équivaut à 'A' et sur mon clavier français '5' = '(' = '[' car ils ont tous le même Code de touche physique).}
function IsKeyDown(c : Char) : Boolean; overload;

{ Vérifie si la touche correspondant au "Code Virtuel" donné est appuyée. @br
  Cette fonction est juste là pour englober "GetKeyState".}
function IsKeyDown(vk : TBZVirtualKeyCode) : Boolean; overload;

{ Renvoie la première touche enfoncée dont le code virtuelle est >= à minVkCode. @br
  Si aucune touche n'est enfoncée, la valeur de retour est -1. Cette fonction n'attend PAS la saisie de l'utilisateur. @br
  Si vous ne vous souciez pas de plusieurs pressions de touches, n'utilisez pas le paramètre. }
function KeyPressed(minVkCode : TBZVirtualKeyCode = 0) : TBZVirtualKeyCode;

{ Convertit un code de touche virtuel vers son nom
  Le nom est exprimé à l'aide des paramètres locale de l'OS. }
function VirtualKeyCodeToKeyName(vk : TBZVirtualKeyCode) : String;

{ Convertit un nom de touche en son code virtuel.@br
  La comparaison n'est PAS sensible à la casse. Si aucune correspondance n'est trouvée, retourne -1.@br
  Le nom est exprimé à l'aide des paramètres locale de l'OS., sauf pour les boutons de la souris
  qui sont traduits en 'LBUTTON', 'MBUTTON' et 'RBUTTON'.}
function KeyNameToVirtualKeyCode(const keyName : String) : TBZVirtualKeyCode;


{ Renvoie le code virtuel correspondant au caractère donné. @br
  Le code renvoyé n'est pas traduit, exemple : 'a' et 'A' donneront le même résultat. @br
  Une valeur de retour de -1 signifie que le caractère n'est peut être pas entré en utilisant le clavier physique.}
function CharToVirtualKeyCode(c : Char) : TBZVirtualKeyCode;

{ Utilisez cette procédure pour informer le mouvement de la roue de la souris
  et simuler l'appui d'une touche (VK_MOUSEWHEELUP/VK_MOUSEWHEELDOWN), qui sera intercepté par IsKeyDown et/ou KeyPressed. @br
  A placer dans les évènement OnMouseWheel des controles graphique (form, tpanel ect...)}
procedure KeyboardNotifyWheelMoved(wheelDelta : Integer);

{ @abstract(Renvoie le nombre de touche pressées silmutanément et le buffer contenant l'etat des touche.)
  Example : @br
  @longcode(#
  HitKeyCount := GetBufferKeyState(Buffer);
  if HitKeyCount>0 then
  begin
    if ((Buffer[VK_NUMPAD1] and $80) <> 0) and ((Buffer[VK_RSHIFT] and $80) <> 0) then inputkey := 1
    else if ((Buffer[VK_NUMPAD2] and $80) <> 0) then inputkey := 2;
  end;
  #)}
function GetBufferKeyState(Var Buffer: TBZKeyBufferState):Byte;

//{$IFDEF LINUX}function VirtualKeyToXKeySym(Key: Word): TKeySym;{$ENDIF}

//==============================================================================

var
 { Variable globale du dernier état de la roulette de la souris }
  vLastWheelDelta : Integer;

//==============================================================================

implementation

uses
  SysUtils;

//==============================================================================

const
   cLBUTTON = 'Left Mouse Button';
   cMBUTTON = 'Middle Mouse Button';
   cRBUTTON = 'Right Mouse Button';

   cUP = 'Up';
   cDOWN = 'Down';
   cRIGHT = 'Right';
   cLEFT = 'Left';
   cPAGEUP = 'Page up';
   cPAGEDOWN = 'Page down';
   cHOME = 'Home';
   cEND = 'End';
   cMOUSEWHEELUP = 'Mouse Wheel Up';
   cMOUSEWHEELDOWN = 'Mouse Wheel Down';

   cPAUSE = 'Pause';
   cSNAPSHOT = 'Print Screen';
   cNUMLOCK = 'Num Lock';
   cINSERT = 'Insert';
   cDELETE = 'Delete';
   cDIVIDE = 'Num /';


   cLWIN	= 'Left Win';
   cRWIN	= 'Right Win';
   cAPPS	= 'Application Key';

   c0 = '~';
   c1 = '[';
   c2 = ']';
   c3 = ';';
   c4 = '''';
   c5 = '<';
   c6 = '>';
   c7 = '/';
   c8 = '\';
   //c9 = '^';

{$IFDEF LINUX}
function VirtualKeyToXKeySym(Key: Word): TKeySym;
begin
  case Key of
    VK_BACK: Result := XK_BackSpace;
    VK_TAB: Result := XK_Tab;
    VK_CLEAR: Result := XK_Clear;
    VK_RETURN: Result := XK_Return;
    VK_SHIFT: Result := XK_Shift_L;
    VK_CONTROL: Result := XK_Control_L;
    VK_MENU: Result := XK_VoidSymbol; // alt key crashes app, XK_Alt_R;
    VK_CAPITAL: Result := XK_Caps_Lock;

    VK_ESCAPE: Result := XK_Escape;
    VK_SPACE: Result := XK_space;
    VK_PRIOR: Result := XK_Prior;
    VK_NEXT: Result := XK_Next;
    VK_END: Result := XK_End;
    VK_HOME: Result := XK_Home;
    VK_LEFT: Result := XK_Left;
    VK_UP: Result := XK_Up;
    VK_RIGHT: Result := XK_Right;
    VK_DOWN: Result := XK_Down;
    VK_SELECT: Result := XK_Select;
    VK_PRINT: Result := XK_Print;
    VK_EXECUTE: Result := XK_Execute;

    VK_INSERT: Result := XK_Insert;
    VK_DELETE: Result := XK_Delete;
    VK_HELP: Result := XK_Help;
    VK_0: Result := XK_0;
    VK_1: Result := XK_1;
    VK_2: Result := XK_2;
    VK_3: Result := XK_3;
    VK_4: Result := XK_4;
    VK_5: Result := XK_5;
    VK_6: Result := XK_6;
    VK_7: Result := XK_7;
    VK_8: Result := XK_8;
    VK_9: Result := XK_9;

    VK_A: Result := XK_a;
    VK_B: Result := XK_b;
    VK_C: Result := XK_c;
    VK_D: Result := XK_d;
    VK_E: Result := XK_e;
    VK_F: Result := XK_f;
    VK_G: Result := XK_g;
    VK_H: Result := XK_h;
    VK_I: Result := XK_i;
    VK_J: Result := XK_j;
    VK_K: Result := XK_k;
    VK_L: Result := XK_l;
    VK_M: Result := XK_m;
    VK_N: Result := XK_n;
    VK_O: Result := XK_o;
    VK_P: Result := XK_p;
    VK_Q: Result := XK_q;
    VK_R: Result := XK_r;
    VK_S: Result := XK_s;
    VK_T: Result := XK_t;
    VK_U: Result := XK_u;
    VK_V: Result := XK_v;
    VK_W: Result := XK_w;
    VK_X: Result := XK_x;
    VK_Y: Result := XK_y;
    VK_Z: Result := XK_z;

    VK_NUMPAD0: Result := XK_KP_0;
    VK_NUMPAD1: Result := XK_KP_1;
    VK_NUMPAD2: Result := XK_KP_2;
    VK_NUMPAD3: Result := XK_KP_3;
    VK_NUMPAD4: Result := XK_KP_4;
    VK_NUMPAD5: Result := XK_KP_5;
    VK_NUMPAD6: Result := XK_KP_6;
    VK_NUMPAD7: Result := XK_KP_7;
    VK_NUMPAD8: Result := XK_KP_8;
    VK_NUMPAD9: Result := XK_KP_9;
    VK_MULTIPLY: Result := XK_KP_Multiply;
    VK_ADD: Result := XK_KP_Add;
    VK_SEPARATOR: Result := XK_KP_Separator;
    VK_SUBTRACT: Result := XK_KP_Subtract;
    VK_DECIMAL: Result := XK_KP_Decimal;
    VK_DIVIDE: Result := XK_KP_Divide;
    VK_F1: Result := XK_F1;
    VK_F2: Result := XK_F2;
    VK_F3: Result := XK_F3;
    VK_F4: Result := XK_F4;
    VK_F5: Result := XK_F5;
    VK_F6: Result := XK_F6;
    VK_F7: Result := XK_F7;
    VK_F8: Result := XK_F8;
    VK_F9: Result := XK_F9;
    VK_F10: Result := XK_F10;
    VK_F11: Result := XK_F11;
    VK_F12: Result := XK_F12;
    VK_F13: Result := XK_F13;
    VK_F14: Result := XK_F14;
    VK_F15: Result := XK_F15;
    VK_F16: Result := XK_F16;
    VK_F17: Result := XK_F17;
    VK_F18: Result := XK_F18;
    VK_F19: Result := XK_F19;
    VK_F20: Result := XK_F20;
    VK_F21: Result := XK_F21;
    VK_F22: Result := XK_F22;
    VK_F23: Result := XK_F23;
    VK_F24: Result := XK_F24;
    VK_NUMLOCK: Result := XK_Num_Lock;
    VK_SCROLL: Result := XK_Scroll_Lock;
  else
    Result := XK_VoidSymbol;
  end;
end;
{$ENDIF}

function VirtualKeyCodeToKeyName(vk : TBZVirtualKeyCode) : String;
var
   nSize : Integer;

begin
   // Win32 API can't translate mouse button virtual keys to string
  case vk of
    VK_LBUTTON : Result:=cLBUTTON;
    VK_MBUTTON : Result:=cMBUTTON;
    VK_RBUTTON : Result:=cRBUTTON;
    VK_UP : Result:=cUP;
    VK_DOWN : Result:=cDOWN;
    VK_LEFT : Result:=cLEFT;
    VK_RIGHT : Result:=cRIGHT;
    VK_PRIOR : Result:=cPAGEUP;
    VK_NEXT : Result:=cPAGEDOWN;
    VK_HOME : Result:=cHOME;
    VK_END : Result:=cEND;
    VK_MOUSEWHEELUP : Result:=cMOUSEWHEELUP;
    VK_MOUSEWHEELDOWN : Result:=cMOUSEWHEELDOWN;

    VK_PAUSE : Result := cPAUSE;
    VK_SNAPSHOT : Result := cSNAPSHOT;
    VK_NUMLOCK : Result := cNUMLOCK;
    VK_INSERT : Result := cINSERT;
    VK_DELETE : Result := cDELETE;

    VK_DIVIDE : Result := cDIVIDE;

    VK_LWIN : Result := cLWIN;
    VK_RWIN : Result := cRWIN;
    VK_APPS : Result := cAPPS;

    192 : Result := c0;
    219 : Result := c1;
    221 : Result := c2;
    186 : Result := c3;
    222 : Result := c4;
    188 : Result := c5;
    190 : Result := c6;
    191 : Result := c7;
    220 : Result := c8;
   // 221 : Result := c9;
    else
    begin
      {$IFDEF MSWINDOWS}
        nSize:=32; // should be enough
        SetLength(Result, nSize);
        vk:=MapVirtualKey(vk, 0);
        nSize:=GetKeyNameText((vk and $FF) shl 16, PChar(Result), nSize);
        SetLength(Result, nSize);
      {$ELSE}
        nSize:=32; // should be enough
        SetLength(Result, nSize);
        Result := XKeysymToString(VirtualKeyToXKeySym(vk));
      {$ENDIF}
    end;
  end;
end;

function KeyNameToVirtualKeyCode(const keyName : String) : TBZVirtualKeyCode;
var
   i : Integer;
begin
  // ok, I admit this is plain ugly. 8)
  Result:=-1;
  for i:=0 to 255 do
  begin
     if SameText(VirtualKeyCodeToKeyName(i), keyName) then
     begin
        Result:=i;
        Break;
     end;
  end;
end;

function GetBufferKeyState(Var Buffer: TBZKeyBufferState):Byte;
Var
  iLoop, Count : Byte;
  HitKey : SmallInt;
begin
  Count := 0;
  FillByte(Buffer,255,0);
  For iLoop := 0 to 255 do
  begin
     HitKey :=  GetKeyState(iLoop);
     if HitKey<>0 then
     begin
        Buffer[iLoop]:=HitKey;
        inc(Count);
     End;
     Result:=Count;
  End;
End;

function KeyPressed(minVkCode : TBZVirtualKeyCode = 0) : TBZVirtualKeyCode;
var
   i : Integer;
   buf : TBZKeyBufferState;
begin
  Result:=-1;
  //FillByte(Buf,255,0);
  if GetBufferKeyState({%H-}Buf)>0 then
  begin
    for i:=minVkCode to 255 do
    begin
      if (buf[i] and $80)<>0 then
      begin
         Result:=i;
         Exit;
      end;
    end;
  End;
  if vLastWheelDelta<>0 then
  begin
    if vLastWheelDelta>0 then Result:=VK_MOUSEWHEELUP
    else Result:=VK_MOUSEWHEELDOWN;
    //vLastWheelDelta:=0;
  end;

  //if ((Buffer[VK_NUMPAD1] and $80) <> 0) then
  //inputkey := 1
  //else if ((Buffer[VK_NUMPAD2] and $80) <> 0) then
  //inputkey := 2
End;

function IsKeyDown(C:Char):Boolean;
begin
  c := UpperCase(c)[1];
  Result := GetKeyState(Ord(c)) < 0;
End;

function IsKeyDown(vk : TBZVirtualKeyCode) : Boolean;
begin
   case vk of
      VK_MOUSEWHEELUP:
      begin
         Result := vLastWheelDelta > 0;
         if Result then
           vLastWheelDelta := 0;
      end;

      VK_MOUSEWHEELDOWN:
      begin
         Result := vLastWheelDelta < 0;
         if Result then
           vLastWheelDelta := 0;
      end;
   else
      Result := GetKeyState(vk) < 0;
   end;
end;

procedure KeyboardNotifyWheelMoved(wheelDelta : Integer);
begin
   vLastWheelDelta:=wheelDelta;
end;

function CharToVirtualKeyCode(c : Char) : TBZVirtualKeyCode;
begin
{$IFDEF MSWINDOWS}
   Result:=VkKeyScan(c) and $FF;
   if Result=$FF then Result:=-1;
{$ELSE}
   c := UpperCase(c)[1];
   Result := Ord(c);
{$ENDIF}
end;

//==============================================================================

initialization

finalization

//==============================================================================
end.
