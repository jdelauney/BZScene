unit BZSnapForm;
//==============================================================================
// @NAME    : BZSnapForm
// @VERSION : 0.2
// @DATE    : 22/10/2015
// @UPDATED : 23/10/2015
// @PACKAGE : BEANZ
// @DESC    : TBZSnapForm est un composant non visuel ajoutant quelques possibilités aux Forms comme :
//            - La magnetisation sur les bord de l'ecran
//            - Le snapping des "form" enfants sur la form principal
//            - Connaitre la position de la souris à l'écran
//            - Prendre une photo du contenu de la fenêtre ou d'une fenêtre enfant
//            - Ouvrir et fermer une fenêtre enfant avec effet de fading
//
// @AUTHOR  : J.DELAUNEY Aka BeanzMaster
// @LICENCE : WTFPL
//------------------------------------------------------------------------------
// @NOTE    : Ce composant à uniquement été tester sous environnement WINDOWS 7
//
//------------------------------------------------------------------------------
// @TODO    :
// - Sauvegarde et restauration de la position et dimensions des fenêtres
// - Magnetisation des fenêtre enfant sur les bord de l'écran
// - Gestion du redimensionnement auto des fenêtres
//==============================================================================

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\bzscene_options.inc}
//==============================================================================

interface

//==============================================================================

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BZThreadTimer;

//==============================================================================

//==============================================================================
// Définitions des paramètres des propriétés pour notre composant
type
  TSnappingActiveBorders      = set of (abLeft, abTop, abRight, abBottom);
  TSnappingTargets    = set of (stLeft, stTop, stRight, stBottom);
  TSnappingTarget     = (stToNone,stToLeft, stToTop, stToRight, stToBottom);
  //TActiveMovingArea   = (amClient, amTitleBar);

const
  cSnapTargetAll = [stLeft, stTop, stRight, stBottom];
  cSnapActiveBorderAll = [abLeft, abTop, abRight, abBottom];
 // FormPropKeywords : array of string = ['left','top','right','bottom','min-height','min-width','max-height','max-width','visible','snap','snapto'];
const
  cMaxSnapForms = 19;

Type TSnapFormChildRec = record
  name:String;
  aform : TForm;
  SnapTo: TSnappingTarget;
end;

//==============================================================================
//==== [ TBZSnapForm ]============================================================
// @Desc : Composant ajoutont des fonctions aux fenêtres
//==============================================================================
type
  TBZSnapForm = class(TComponent)
  private
    FProcessForm     : TForm;
    FChildForms      : Array[0..cMaxSnapForms] of TSnapFormChildRec;
    //FChildsForm        : TList;
    FChildFormsCount : Byte;

    FTimer           : TBZThreadTimer;
    //FPropsStorage    : TXMstorage;

    FActive          : Boolean;

    FMouseX,
    FMouseY : Integer;

    FScreenMagnet    : Boolean;  // Colle la fenetre aux bords de l'ecran
    FMagneticScreen  : TSnappingActiveBorders;
    FMagnetOnScreen  : TSnappingTargets;

    FSnapping : Boolean;  // Colle les fenetres enfants a la fenetre principale
    FSnappingBorders : TSnappingActiveBorders;
    // FChildDockable : Boolean;  // Les fenetres enfants peuvent être attachés un control (panel,scrollbox) de la fenetre principale

    FMagneticField : cardinal; // default 15px;

    FOldFormWidth    : Integer;
    FOldFormHeight   : Integer;
    FOldFormPos      : TPoint;

    FActiveMoving    : Boolean;  // Possiblité a la fenetre de bouger
    FActiveSizing    : Boolean;  // Possiblité de redimensionner la fenetre
    // FSizingBorders   : TSnappingActiveBorders;
    // FAutoResizing    : Boolean;  // Redimensionnement de la fenetre et de ces composants automatiquement
    // FAutoLayout      : Boolean;   // Sauve et restore la position et dimension des fenetres automatiquement
    // FExtendMove   : Boolean;  // Pour deplacer la fenetre en selectionnant son contenu
    FFullScreen    : Boolean;

    procedure SetActive(AValue:Boolean);
    procedure SetFullScreen(AValue:Boolean);
    procedure SetActiveMoving(AValue:Boolean);
    procedure SetActiveSizing(AValue:Boolean);
    procedure SetSnapping(AValue:Boolean);
    //procedure SetChildDockable(AValue:Boolean);

  protected
    procedure ProcessScreenMagnet();
    procedure ProcessFormSnapping();
    procedure ProcessMovingSizing();

    procedure DoProcess(Sender:TObject);

    procedure Loaded;override;

  public

    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    { Ajoute une fenêtre enfant à gérer dans la liste }
    procedure addChildForm(AName:string;AForm:TForm);
    { Retourne la fenêtre enfant et son Index de la liste }
    function getChildForm(AName:String;var Index:Integer):TForm;
    { Retourne la fenêtre enfant }
    function getChildForm(AName:String):TForm;overload;
    { Retourne l'index de la fenêtre enfant }
    function getChildFormIndex(AName:String):Integer;
    { Magnetise notre fenêtre sur les bords de l'écran }
    procedure MagnetToScreen(ATarget: TSnappingTarget);

    //procedure MagnetChildToScreen(AName:String;ATarget: TSnappingTarget);

    { Attache une fenêtre enfant sur le bord Gauche de notre fenêtre principale }
    procedure SnapChildToLeft(AName:String);
    { Attache une fenêtre enfant sur le bord Haut de notre fenêtre principale }
    procedure SnapChildToTop(AName:String);
    { Attache une fenêtre enfant sur le bord Droit de notre fenêtre principale }
    procedure SnapChildToRight(AName:String);
    { Attache une fenêtre enfant sur le bord Bas de notre fenêtre principale }
    procedure SnapChildToBottom(AName:String);
    { Attache une fenêtre enfant sur un bord de notre fenêtre principale  }
    procedure SnapChildTo(AName:String;ATarget: TSnappingTarget);
    { Détache une fenêtre enfant d'un des bords de notre fenêtre principale }
    procedure UnSnapChild(AName:String);
    { Passe en mode plein écran }
    procedure EnterFullScreen;
    { Sort du mode plein ecran }
    procedure ExitFullScreen;

 //   procedure DockChildFormTo(AName:String,ATargetName: string);
 //   procedure UnDockChildForm(AName:String);
    { Affiche une fenêtre enfant avec un effet de fading }
    procedure ShowForm(AName:String;Fade:Boolean);
    { Cache une fenêtre enfant avec un effet de fading }
    procedure HideForm(AName:String;Fade:Boolean);

    //procedure SaveLayout();
    //procedure RestoreLayout();

    { Capture de la fenetre vers un Bitmap }
    function TakeFormScreenShot:TBitmap;
    { Capture d'une fen^tre enfant vers un Bitmap }
    function TakeChildFormScreenShot(AName:String):TBitmap;

    { Débute la gestion de nos évênements }
    procedure Start;
    { Arrête la gestion de nos évênements }
    procedure Stop;

  published

    property Active : Boolean read FActive write setActive;

    property ScreenMagnet : Boolean read FScreenMagnet write FScreenMagnet;
    property MagnetOnScreen : TSnappingTargets read FMagnetOnScreen write FMagnetOnScreen;

    property SnappingBorders : TSnappingActiveBorders Read FSnappingBorders Write FSnappingBorders;
    property MagneticScreen : TSnappingActiveBorders Read FMagneticScreen  Write FMagneticScreen;

    property Snapping : Boolean read FSnapping write FSnapping;

    property MagneticField : Cardinal read FMagneticField   write FMagneticField;

    property ActiveMoving : Boolean read FActiveMoving write SetActiveMoving;
    property ActiveSizing : Boolean read FActiveSizing write SetActiveSizing;

    property FullScreen : Boolean read FFullScreen Write SetFullScreen;

    //property ActiveSizing      : Boolean           read FActiveSizing      write FActiveSizing    default True;
    //property AutoResizing      : Boolean  read FAutoResizing      write FAutoResizing    default False;
    //property AutoLayout        : Boolean  read FAutoLayout        write FAutoLayout      default False;

  public
    property MousePosX : Integer Read FMouseX;
    property MousePosY : Integer Read FMouseY;
  end;

//==============================================================================


//==============================================================================
implementation

//==============================================================================


//==============================================================================

//==============================================================================
//==============================================================================
// 1. TBZSnapForm
//==============================================================================

//==== PRIVATE =================================================================

{**------------------------------------------------------------------------------
 * Setters pour les differentes propriétés
------------------------------------------------------------------------------**}
procedure TBZSnapForm.setActive(AValue:Boolean);
begin
  if FActive = AValue then exit;
  FActive:=AValue;
  if FActive then
  begin
     if not (csDesigning in ComponentState)  then
     begin
       Start;
     end;
  end
  else Stop;

end;

procedure TBZSnapForm.SetFullScreen(AValue:Boolean);
begin
  if FFullScreen = AValue then exit;
  FFullScreen := AValue;
  if FFullScreen then EnterFullScreen else ExitFullScreen;
end;

procedure TBZSnapForm.SetActiveMoving(AValue:Boolean);
begin
  if FActiveMoving = AValue then exit;
  FActiveMoving := AValue;
  if not(FActiveMoving) then
  begin
    FOldFormPos:=Point(FProcessForm.Left,FProcessForm.Top);
  end;
end;

procedure TBZSnapForm.SetActiveSizing(AValue:Boolean);
begin
  if FActiveSizing = AValue then exit;
  FActiveSizing := AValue;
  if not(FActiveSizing) then
  begin
    FOldFormWidth := FProcessForm.Width;
    FOldFormHeight := FProcessForm.Height;
  end;
end;

procedure TBZSnapForm.SetSnapping(AValue:Boolean);
begin
  if FSnapping = AValue then exit;
  FSnapping := AValue;
end;

//==== PROTECTED ===============================================================

{**-----------------------------------------------------------------------------
 * @Name        : Loaded
 * @Description :
 -----------------------------------------------------------------------------**}
procedure TBZSnapForm.Loaded;
begin
  if not (csDesigning in ComponentState) and Active  then
  begin
    FOldFormHeight := FProcessForm.Height;
    FOldFormWidth  := FProcessForm.Width;
    FOldFormPos.x  := FProcessForm.Left;
    FOldFormPos.y  := FProcessForm.Top;
    Start;
  end;
end;

{**-----------------------------------------------------------------------------
 * @Name        : ProcessScreenMagnet
 * @Description : Gestion du magnetisme au bords de l'écran
 -----------------------------------------------------------------------------**}
procedure TBZSnapForm.ProcessScreenMagnet();
var
    wWidth,wHeight:Integer;
    x,y,nx,ny:integer;
begin
  If ActiveMoving then
  begin
    If (FScreenMagnet) and (FProcessForm.WindowState = wsNormal)  then
    begin
      wWidth := FProcessForm.Width;
      wHeight := FProcessForm.Height;
      nx:= FProcessForm.Left;
      ny:= FProcessForm.Top;
      x:=nx;
      y:=ny;
      if (stTop in MagnetOnScreen) and (abTop in MagneticScreen) then
      begin
        if y <= MagneticField then ny:= 2;
      end;
      if (stLeft in MagnetOnScreen) and (abLeft in MagneticScreen) then
      begin
        if x <= MagneticField then nx := 2;
      end;
      if (stBottom in MagnetOnScreen) and (abBottom in MagneticScreen) then
      begin
        if (y+wHeight) >= (Screen.Height-78-MagneticField) then ny := Screen.Height-wHeight-78;
      end;
      if (stRight in MagnetOnScreen) and (abRight in MagneticScreen) then
      begin
        if (x+wWidth) >= (Screen.Width-MagneticField) then nx := Screen.Width-wWidth-4;
      end;
      FProcessForm.Top:=ny;
      FProcessForm.Left:=nx;
    end;
  end;
end;

{**-----------------------------------------------------------------------------
 * @Name        : ProcessFormSnapping
 * @Description : Gestion du "snapping" des fenêtres enfant
 -----------------------------------------------------------------------------**}
procedure TBZSnapForm.ProcessFormSnapping();
var I:integer;
    fx,fy,fw,fh : integer;
begin
  { A la place d'utiliser un tableau on peut faire :

   if Assigned(FProcessForm) then begin
    for I := 0 to Application.ComponentCount - 1 do begin
      if Application.Components[I] is TForm then begin
       // with Application.Components[I] as TForm do begin
   }

  if FChildFormsCount>0 then
  begin
    for I:=0 to FChildFormsCount-1 do
    begin
      if FChildForms[i].AForm.Visible then
      begin
        If Snapping and (FProcessForm.WindowState = wsNormal)  then
        begin
          fx := FChildForms[i].AForm.Left;
          fy := FChildForms[i].AForm.Top;
          fw := FChildForms[i].AForm.Width;
          fh := FChildForms[i].AForm.Height;
          FChildForms[i].SnapTo:=stToNone;
          if (abTop in SnappingBorders)  then
          begin
            if  ( ( ((fx+fw) <= (FOldFormPos.X+FOldFormWidth)) and ((fx+fw) >= (FOldFormPos.X)) ) or ( (fx >= FOldFormPos.X) and (fx <= FOldFormPos.X+FOldFormWidth) ) ) then
              if ((fy+fh)>=FOldFormPos.Y-MagneticField) and ((fy+fh)<=FOldFormPos.Y+MagneticField) then
              begin
                FChildForms[i].AForm.Left := fx;
                FChildForms[i].AForm.Top  := FOldFormPos.y-fh-34; // le -34 corresponspond à la taille de la barre de titre (24+2 ou 2 est la marge) + la taille de la bordure des fenêtres (4*2)
                FChildForms[i].SnapTo:=stToTop;
              end;
              //else FChildForms[i].SnapTo:=stToNone;
          end;

          if (abBottom in SnappingBorders) then
          begin
            if  ( ( ((fx+fw) <= (FOldFormPos.X+FOldFormWidth)) and ((fx+fw) >= (FOldFormPos.X)) ) or ( (fx >= FOldFormPos.X) and (fx <= FOldFormPos.X+FOldFormWidth) ) ) then
              if (fy<=(FOldFormPos.Y+FOldFormHeight)+MagneticField) and (fy>=(FOldFormPos.Y+FOldFormHeight)-MagneticField) then
              begin
                FChildForms[i].AForm.Left := fx;
                FChildForms[i].AForm.Top  := FOldFormPos.y+FOldFormHeight+34;
                FChildForms[i].SnapTo:=stToBottom;
              end;
              //else FChildForms[i].SnapTo:=stToNone;
          end;

          if (abLeft in SnappingBorders)  then
          begin
            if ((fy<=(FOldFormPos.Y+FOldFormHeight)) and (fy>=FOldFormPos.Y)) or (((fy+fh)<=(FOldFormPos.Y+FOldFormHeight)) and ((fy+fh)>=FOldFormPos.Y)) then
              if  ( ( (fx+fw) <= (FOldFormPos.X+MagneticField)) and ( (fx+fw) >= (FOldFormPos.X-MagneticField))) then
              begin
                FChildForms[i].AForm.Left := FOldFormPos.x-fw-8;
                FChildForms[i].AForm.Top  := fy;
                FChildForms[i].SnapTo:=stToLeft;
              end;
              //else FChildForms[i].SnapTo:=stToNone;
          end;

          if (abRight in SnappingBorders) then
          begin
            if ((fy<=(FOldFormPos.Y+FOldFormHeight)) and (fy>=FOldFormPos.Y)) or (((fy+fh)<=(FOldFormPos.Y+FOldFormHeight)) and ((fy+fh)>=FOldFormPos.Y)) then
              if ( (fx >= ((FOldFormPos.X+FOldFormWidth)-MagneticField)) and (fx <= ((FOldFormPos.X+FOldFormWidth)+MagneticField))) then
              begin
                FChildForms[i].AForm.Left := FOldFormPos.x+FOldFormWidth+8;
                FChildForms[i].AForm.Top  := fy;
                FChildForms[i].SnapTo:=stToRight;
              end;
              //else FChildForms[i].SnapTo:=stToNone;
          end;
        end;
      end;
    end;
  end;
end;

{**-----------------------------------------------------------------------------
 * @Name        : ProcessMoving
 * @Description : Gestion du déplacement de la fenêtre
 -----------------------------------------------------------------------------**}
procedure TBZSnapForm.ProcessMovingSizing();
var dx, dy,I :integer;
begin

  if not(ActiveMoving) then
  begin
    FProcessForm.Left := FOldFormPos.X;
    FProcessForm.Top  := FOldFormPos.Y;
  end;

  if not(ActiveSizing) then
  begin
    FProcessForm.Width := FOldFormWidth;
    FProcessForm.Height:= FOldFormHeight;
  end;

  if (ActiveSizing) or (ActiveMoving) then
  begin
    if FChildFormsCount>0 then
    begin
      for I:=0 to FChildFormsCount-1 do
      begin
        If Snapping and (FProcessForm.WindowState = wsNormal) and (FChildForms[i].AForm.Visible) then
           if (FChildForms[i].SnapTo<>stToNone) then
           begin
             dx := FOldFormPos.X - FProcessForm.Left;
             dy := FOldFormPos.Y - FProcessForm.Top;
             FChildForms[i].AForm.Left := FChildForms[i].AForm.Left - dx;
             FChildForms[i].AForm.Top := FChildForms[i].AForm.Top - dy;
           end;
      end;
    end;
  end;
end;

{**-----------------------------------------------------------------------------
 * @Name        : DoProcess
 * @Description : Gestion des êvenments dans le timer
 -----------------------------------------------------------------------------**}
procedure TBZSnapForm.DoProcess(Sender:TObject);
begin
  FMouseX:=Mouse.CursorPos.x;
  FMouseY:=Mouse.CursorPos.Y;

  if (ActiveSizing) then
  begin
    FOldFormHeight := FProcessForm.Height;
    FOldFormWidth  := FProcessForm.Width;
  end;

  ProcessScreenMagnet;
  ProcessFormSnapping;

  ProcessMovingSizing;

  FOldFormPos.x  := FProcessForm.Left;
  FOldFormPos.y  := FProcessForm.Top;

end;

//==== PUBLIC ==================================================================

{**-----------------------------------------------------------------------------
 * @Name        : Create
 * @Description : Creation de notre bouton personnalisé
 -----------------------------------------------------------------------------**}
constructor TBZSnapForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FProcessForm   := TForm(AOwner);

  FActive        := False;

  FActiveMoving  := True;
  FActiveSizing  := True;

  FScreenMagnet   := True;
  {
  Magneticfield doit être au mini à 64 car sinon le snapping des fenêtre en haut et bas ne se fais pas correctement
  Sous Windows 7. J'ai remarquer que la position Left et Top correspondent aux positionx de la fenêtre mais sans
  les bordures et la barre de titre. Ces positions sont relative au contenu de la form
  }
  FMagneticField  := 64;
  FMagnetOnScreen := cSnapTargetAll;
  FMagneticScreen := cSnapActiveBorderAll;

  FSnapping        := True;
  FSnappingBorders := cSnapActiveBorderAll;

  FChildFormsCount := 0;

  FTimer := TBZThreadTimer.Create(self);
  FTimer.Enabled     := False;
  FTimer.KeepAlive   := True;
  FTimer.Synchronize := True;
  FTimer.Accurate    := True;
  FTimer.Interval    := 100;
  FTimer.OnTimer     := @DoProcess;
 // FMovingBorders := mbAllBorders;
 //  FSizingBorders := cSnapTargetAll;
end;

{**-----------------------------------------------------------------------------
 * @Name        : Destroy
 * @Description : Destruction de notre composant
 -----------------------------------------------------------------------------**}
destructor TBZSnapForm.Destroy;
begin
  Stop;
  FreeAndNil(FTimer);
  inherited Destroy;
end;

procedure TBZSnapForm.addChildForm(AName:string;AForm:TForm);
var i:integer;
    canAdd:Boolean;
begin
  canAdd:=false;
  if FChildFormsCount>0 then
  begin
    for i:=0 to FChildFormsCount-1 do
    begin
      if AName =  FChildForms[FChildFormsCount-1].Name then
      begin
        canAdd := False;
        break;
      end
      else canAdd:=true;
    end;
  end else canAdd := True;

  if canAdd then
  begin
    inc(FChildFormsCount);
    FChildForms[FChildFormsCount-1].Name            := AName;
    FChildForms[FChildFormsCount-1].AForm           := AForm;
    FChildForms[FChildFormsCount-1].SnapTo          := stToNone;
  end;
end;

function TBZSnapForm.getChildForm(AName:String;var Index:Integer):TForm;
var I:integer;
begin
  Result := nil;
  Index:=-1;
  if FChildFormsCount>0 then
  begin
    for I:=0 to FChildFormsCount-1 do
    begin
      if FChildForms[I].Name = AName then
      begin
        Result :=FChildForms[i].AForm;
        Index := I;
        Break;
      end;
    end;
  end;
end;

function TBZSnapForm.getChildForm(AName:String):TForm;
var I:integer;
begin
  Result := nil;
  if FChildFormsCount>0 then
  begin
    for I:=0 to FChildFormsCount-1 do
    begin
      if FChildForms[I].Name = AName then
      begin
        Result :=FChildForms[i].AForm;
        Break;
      end;
    end;
  end;
end;

function TBZSnapForm.getChildFormIndex(AName:String):Integer;
var I:integer;
begin
  Result := -1;
  if FChildFormsCount>0 then
  begin
    for I:=0 to FChildFormsCount-1 do
    begin
      if FChildForms[I].Name = AName then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

procedure TBZSnapForm.MagnetToScreen(ATarget: TSnappingTarget);
var
    wWidth,wHeight:Integer;
    x,y,nx,ny:int64;
begin
  If (ScreenMagnet) and (FProcessForm.WindowState = wsNormal)  then
  begin
    //CurrentRect:=FProcessForm.BoundsRect;
    wWidth := FProcessForm.Width; //CurrentRect.Right - CurrentRect.Left;
    wHeight := FProcessForm.Height; //CurrentRect.Bottom - CurrentRect.Top;
    nx:= FProcessForm.Left;
    ny:= FProcessForm.Top;
    x:=nx;
    y:=ny;
    Case ATarget of
      stToTop :
      begin
        if (stTop in MagnetOnScreen) and (abTop in MagneticScreen) then
        begin
          if y <= MagneticField then ny:= 2;
        end;
      end;
      stToLeft :
      Begin
        if (stLeft in MagnetOnScreen) and (abLeft in MagneticScreen) then
        begin
          if x <= MagneticField then nx := 2;
        end;
      end;
      stToBottom :
      begin
        if (stBottom in MagnetOnScreen) and (abBottom in MagneticScreen) then
        begin
          if (y+wHeight) >= (Screen.Height-78-MagneticField) then ny := Screen.Height-wHeight-78;
        end;
      end;
      stToRight :
      begin
        if (stRight in MagnetOnScreen) and (abRight in MagneticScreen) then
        begin
          if (x+wWidth) >= (Screen.Width-MagneticField) then nx := Screen.Width-wWidth-4;
        end;
      end;
      stToNone :
      begin

      end;
    end;
    FProcessForm.Top:=ny;
    FProcessForm.Left:=nx;
  end;
end;

{**-----------------------------------------------------------------------------
 * @Name        : SnapChildTo
 * @Description :
 -----------------------------------------------------------------------------**}
procedure TBZSnapForm.SnapChildTo(AName:String;ATarget: TSnappingTarget);
var fh,fw,i:integer;
begin
  I:= getChildFormIndex(AName);
  if I>-1 then
  begin
    fw := FChildForms[i].AForm.Width;
    fh := FChildForms[i].AForm.Height;
    Case ATarget of
      stToTop:
      begin
        FChildForms[i].AForm.Left := FOldFormPos.x;
        FChildForms[i].AForm.Top  := FOldFormPos.y-fh-34; // le -34 corresponspond à la taille de la barre de titre (24+2 ou 2 est la marge) + la taille de la bordure des fenêtres (4*2)
        FChildForms[i].SnapTo:=stToTop;
      end;
      stToBottom :
      begin
        FChildForms[i].AForm.Left := FOldFormPos.x;
        FChildForms[i].AForm.Top  := FOldFormPos.y+FOldFormHeight+34;
        FChildForms[i].SnapTo:=stToBottom;
      end;
      stToLeft :
      Begin
        FChildForms[i].AForm.Left := FOldFormPos.x-fw-8;
        FChildForms[i].AForm.Top  := FOldFormPos.y;
        FChildForms[i].SnapTo:=stToLeft;
      end;
      stToRight :
      begin
        FChildForms[i].AForm.Left := FOldFormPos.x+FOldFormWidth+8;
        FChildForms[i].AForm.Top  := FOldFormPos.y;
        FChildForms[i].SnapTo:=stToRight;
      end;
      stToNone : FChildForms[i].SnapTo:=stToNone;
    end;

  end;
end;

procedure TBZSnapForm.SnapChildToLeft(AName:String);
begin
  SnapChildTo(AName,stToLeft);
end;

procedure TBZSnapForm.SnapChildToTop(AName:String);
begin
  SnapChildTo(AName,stToTop);
end;

procedure TBZSnapForm.SnapChildToRight(AName:String);
begin
  SnapChildTo(AName,stToRight);
end;

procedure TBZSnapForm.SnapChildToBottom(AName:String);
begin
  SnapChildTo(AName,stToBottom);
end;

procedure TBZSnapForm.UnSnapChild(AName:String);
begin
  SnapChildTo(AName,stToNone);
end;

// Magnetise une fenêtre enfant sur les bords de l'écran
//procedure TBZSnapForm.MagnetChildToScreen(AName:String;ATarget: TSnappingTarget);
//begin
//
//end;

procedure TBZSnapForm.EnterFullScreen;
begin
 FProcessForm.BorderStyle:=bsNone;
 FProcessForm.FormStyle:=fsSystemStayOnTop;
 FProcessForm.WindowState:=wsMaximized; //wsFullScreen;
 FFullScreen:=True;
end;

procedure TBZSnapForm.ExitFullScreen;
begin
  FProcessForm.WindowState:=wsNormal;
  FProcessForm.Position:=poDesigned;
  FProcessForm.FormStyle:=fsNormal;
  FProcessForm.BorderStyle:=bsSizeable;
  FFullScreen:=False;
end;

procedure TBZSnapForm.ShowForm(AName:String;Fade:Boolean);
var
  i: Integer;
  AForm:TForm;
begin
  AForm:=getChildForm(AName);
  if Fade then
  begin
    AForm.AlphaBlend:=True;
    AForm.Show;
    for i := 1 to 51 do begin
      AForm.AlphaBlendValue:=i*5;
      Sleep(10);
    end;
    AForm.AlphaBlend:=False;
  end
  else AForm.Show;
end;

procedure TBZSnapForm.HideForm(AName:String;Fade:Boolean);
var
  i: Integer;
  AForm:TForm;
begin
  AForm:=getChildForm(AName);
  if Fade then
  begin
    AForm.AlphaBlend:=True;
    for i := 51 downto 1 do begin
      AForm.AlphaBlendValue:=i*5;
      Sleep(10);
    end;
    AForm.Hide;
    AForm.AlphaBlend:=False;
  end
  else AForm.Hide;
end;

function TBZSnapForm.TakeFormScreenShot:TBitmap;
begin
  Result := FProcessForm.GetFormImage;
end;

function TBZSnapForm.TakeChildFormScreenShot(AName:String):TBitmap;
Var
  AForm : TForm;
begin
  Result:=nil;
  AForm:=getChildForm(AName);
  if AForm.Visible then Result := AForm.GetFormImage;
end;

procedure TBZSnapForm.Start;
begin
  FTimer.Enabled:=True;
end;

procedure TBZSnapForm.Stop;
begin
  FTimer.Enabled:=False;
end;

end.
