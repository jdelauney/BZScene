(*
  @abstract(Contient des classes de bases conçues à des fins génériques
  pour la création de composants visuel et le control de certaine propriétés dans des classes.)

  -------------------------------------------------------------------------------------------------------------

  @created(2016-11-16)
  @author(J.Delauney (BeanzMaster))
  Historique :
  @unorderedList(
    @item(18/06/2019 : Creation  )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) :

  -------------------------------------------------------------------------------------------------------------

  @bold(Dependances) : BZClasses

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)
   @unorderedList(
     @item(J.Delauney (BeanzMaster))
   )

  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZControlClasses;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================

interface

Uses
  Classes, SysUtils, Controls, Graphics,
  LCLType,
  BZClasses, BZMAth, BZVectorMath, BZVectorMathUtils, BZColors, BZGraphic, BZBitmap;

Type

  { TBZCoordinatesStyle : Identifie le type de données stockées au sein d'un TBZCustomCoordinates.
      csPoint2D : a simple 2D point (Z=0, W=0)
      csPoint   : un point (W=1)
      csVector  : un vecteur (W=0)
      csUnknown : aucune contrainte }
  TBZCoordinatesStyle = (CsPoint2D, CsPoint, CsVector, CsUnknown);

  { TBZCustomCoordinates :  Cette classe est fondamentalement un conteneur pour
    un TBZVector, permettant une utilisation correcte de l'éditeurs de propriétés et l'édition dans l'EDI.
    les méthodes de manipulation sont seulement minimes.
    Gère les valeurs par défaut dynamiquememnt pour l'enregistrement dans le fichier de ressources. }
  TBZCustomCoordinates = class(TBZUpdateAbleObject)
  private

    FCoords: TBZVector;
    FStyle: TBZCoordinatesStyle; // NOT Persistent
    FPDefaultCoords: PBZVector;
    procedure SetAsPoint2D(const Value: TBZVector2f);
    procedure SetAsVector(const Value: TBZVector);
    procedure SetAsAffineVector(const Value: TBZAffineVector);
    function GetAsAffineVector: TBZAffineVector;
    function GetAsPoint2D: TBZVector2f;
    function GetAsString: String;
    function GetCoordinate(const AIndex: Integer): Single;
    procedure SetCoordinate(const AIndex: Integer; const AValue: Single);
    function GetDirectCoordinate(const Index: Integer): Single;
    procedure SetDirectCoordinate(const Index: Integer; const AValue: Single);

  protected

    procedure SetDirecVector(const V: TBZVector);

    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);

  public

    constructor CreateInitialized(AOwner: TPersistent; const AValue: TBZVector; const AStyle: TBZCoordinatesStyle = CsUnknown);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(Writer: TWriter);
    procedure ReadFromFiler(Reader: TReader);

    procedure Initialize(const Value: TBZVector);
    procedure NotifyChange(Sender: TObject); override;

    { Style : Identifie les styles de coordonnées.
      La propriété n'est pas persistante. csUnknown par défaut.
      Il devrait être géré par l'objet propriétaire uniquement (en interne).
      Il est utilisé par TBZCustomCoordinates pour les vérifications internes d ''assertion'
      détecter les «abus» ou les «malentendus» que Le système de coordonnées homogène implique. }
    property Style: TBZCoordinatesStyle read FStyle write FStyle;

    procedure Translate(const TranslationVector: TBZVector); overload;
    procedure Translate(const TranslationVector: TBZAffineVector); overload;
    procedure AddScaledVector(const Factor: Single; const TranslationVector: TBZVector); overload;
    procedure AddScaledVector(const Factor: Single; const TranslationVector: TBZAffineVector); overload;
    procedure Rotate(const AnAxis: TBZAffineVector; AnAngle: Single); overload;
    procedure Rotate(const AnAxis: TBZVector; AnAngle: Single); overload;
    procedure Normalize;
    procedure Invert;
    procedure Scale(Factor: Single);
    function VectorLength: Single;
    function VectorNorm: Single;
    function MaxXYZ: Single;
    function Equals(const AVector: TBZVector): Boolean; reintroduce;

    procedure SetVector(const X, Y: Single; Z: Single = 0); overload;
    procedure SetVector(const X, Y, Z, W: Single); overload;
    procedure SetVector(const V: TBZAffineVector); overload;
    procedure SetVector(const V: TBZVector); overload;

    procedure SetPoint(const X, Y, Z: Single); overload;
    procedure SetPoint(const V: TBZAffineVector); overload;
    procedure SetPoint(const V: TBZVector); overload;

    procedure SetPoint2D(const X, Y: Single); overload;
    procedure SetPoint2D(const Vector: TBZAffineVector); overload;
    procedure SetPoint2D(const Vector: TBZVector); overload;
    procedure SetPoint2D(const Vector: TBZVector2f); overload;

    procedure SetToZero;

    { AsVector : Les coordonnées sont vues comme un vecteur.
      L'attribution d'une valeur à cette propriété déclenchera des événements de notification,
      Si vous ne le souhaitez pas, utilisez plutôt DirecVector. }
    property AsVector: TBZVector read FCoords write SetAsVector;

    { AsAffineVector : Les coordonnées sont vues comme un vecteur affine.
      L'attribution d'une valeur à cette propriété déclenchera des événements de notification,
      Si vous ne le souhaitez pas, utilisez plutôt DirecVector.
      Le composant W est automatiquement ajusté en fonction du style. }
    property AsAffineVector: TBZAffineVector read GetAsAffineVector write SetAsAffineVector;

    { AsPoint2D : Les coordonnées considérées comme un point 2D.
      L'attribution d'une valeur à cette propriété déclenchera des événements de notification,
      Si vous ne le souhaitez pas, utilisez DirecVector à la place }
    property AsPoint2D: TBZVector2f read GetAsPoint2D write SetAsPoint2D;

    property X: Single index 0 read GetCoordinate write SetCoordinate;
    property Y: Single index 1 read GetCoordinate write SetCoordinate;
    property Z: Single index 2 read GetCoordinate write SetCoordinate;
    property W: Single index 3 read GetCoordinate write SetCoordinate;

    property Coordinate[const AIndex: Integer]: Single read GetCoordinate write SetCoordinate; default;

    { AsString : Retourne Les coordonnées, entre parenthèses, séparées par des points-virgules. }
    property AsString: String read GetAsString;

    // : Similaire à AsVector mais ne déclenche pas d'événements de notification
    property DirecVector: TBZVector read FCoords write SetDirecVector;
    property DirectX: Single index 0 read GetDirectCoordinate write SetDirectCoordinate;
    property DirectY: Single index 1 read GetDirectCoordinate write SetDirectCoordinate;
    property DirectZ: Single index 2 read GetDirectCoordinate write SetDirectCoordinate;
    property DirectW: Single index 3 read GetDirectCoordinate write SetDirectCoordinate;
  end;

  { TBZCoordinates2 : Un TBZCustomCoordinates qui publie les propriétés X, Y }
  TBZCoordinates2D = class(TBZCustomCoordinates)
  published
    property X stored False;
    property Y stored False;
  end;

  { TBZCoordinates3  : Un TBZCustomCoordinates qui publie les propriétés X, Y, Z }
  TBZCoordinates3D = class(TBZCustomCoordinates)
  published
    property X stored False;
    property Y stored False;
    property Z stored False;
  end;

  { TBZCoordinates4 : Un TBZCustomCoordinates qui publie les propriétés X, Y, Z, W }
  TBZCoordinates4D = class(TBZCustomCoordinates)
  published
    property X stored False;
    property Y stored False;
    property Z stored False;
    property W stored False;
  end;

  TBZCoordinates = TBZCoordinates3D;

  { IBZCoordinatesUpdateAble:  En fait, Sender devrait être TBZCoordinates, mais cela nécessiterait
    des changements dans d'autres unités et d'autres projets qui l'utilisent }
  IBZCoordinatesUpdateAble = interface(IInterface)
    ['{021D8542-6E59-4CB8-8DF7-BDD19CC920D7}']
    procedure CoordinateChanged(Sender: TBZCustomCoordinates);
  end;

  { TBZCoordinatesUpdateAbleComponent : Composant de base à hériter disposant d'un systeme de coordonées }
  TBZCoordinatesUpdateAbleComponent = class(TBZUpdateAbleComponent, IBZCoordinatesUpdateAble)
  public
    procedure CoordinateChanged(Sender: TBZCustomCoordinates); virtual; abstract;
  end;

  { TBZCustomControlObjectDrawer }

  TBZCustomControlObjectDrawer = Class(TBZUpdateAbleObject)//class(TBZCoordinatesUpdateAbleComponent)
  private
    //FPosition : TBZCoordinates2D;
    //FSize     : TBZCoordinates2D;
    FSurface  : TBZBitmap;
    FNeedUpdate : Boolean;
    FWidth : Integer;
    FHeight : Integer;
    //FRotationAngle : Single;
    //FOnClick : TNotifyEvent;

    //FOnEnter      : TNotifyEvent;
    //FOnExit       : TNotifyEvent;
    //FOnMouseEnter : TNotifyEvent;
    //FOnMouseLeave : TNotifyEvent;
    //FOnMouseDown  : TMouseEvent;
    //FOnMouseUp    : TMouseEvent;
    //FOnMouseMove  : TMouseMoveEvent;
    //FOnMouseWheel : TMouseWheelEvent;
    //FOnKeypress   : TKeyPressEvent;

    //procedure SetPosition(const AValue : TBZCoordinates2D);
    //procedure SetSize(const AValue : TBZCoordinates2D);
    procedure SetWidth(const AValue : Integer);
    procedure SetHeight(const AValue : Integer);
    //procedure SetRotationAngle(const AValue : Single);

  protected
    procedure Render; virtual;
    procedure NotifyChange(Sender : TObject); override;
  public
    Constructor Create(AOwner : TComponent); //override;
    Destructor Destroy; override;

    //procedure CoordinateChanged(Sender: TBZCustomCoordinates); override;

    function GetSurface :  TBZBitmap;

    property Width : Integer read FWidth write SetWidth;
    property Height : Integer read FHeight write SetHeight;
    //procedure HandleMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    //procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); virtual;
    //procedure HandleMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    //
    //procedure RenderToBitmap(DestBmp : TBZBitmap);

  published
    //property Position :  TBZCoordinates2D Read FPosition write SetPosition;
    //property Size     :  TBZCoordinates2D Read FSize write SetSize;
    //property RotationAngle : Single read FRotationAngle write SetRotationAngle;
  end;

  { Classe abstraite de base pour les composants visuel. @br
    Active le DoubleBuffer sous Windows et désactive l'effacement du fond du "Canvas". @br
    Offre la gestion de l'affichage avec prise en charge de la transparence. }
  TBZCustomControl = Class(TCustomControl)
  Private
    // FStyleResources : TBZStyleResources
    // FResourceName : String;
    // FTagObject : TObject;
    // FTagFloat : Single;
    // FTagString : String;
    // FBindingObjects : TList
    // FBindingSource : TControl;
    // FBindingName : String;
    FTransparent: Boolean;
    Procedure SetTransparent(Const AValue: Boolean);
  Protected
   // FOnPaint: TNotifyEvent;
    procedure DoOnSetTransparent; virtual;
    Procedure Paint; Override;
   // procedure CreateParams(var Params: TCreateParams); override;
  Public
    Constructor Create(AOwner: TComponent); Override;

    //Procedure EraseBackground(DC: HDC); Override;

    Property DockManager;
    Property Canvas;
    Property WindowHandle;
  Published
    { Affiche le control avec transparence }
    Property Transparent: Boolean read FTransparent write SetTransparent Default False;

    property Left;
    property Top;
    property Width;
    property Height;
    Property Align;
    Property Anchors;
    Property AutoSize;

    Property BorderWidth;
    Property Constraints;

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
  End;

  { TBZCustomViewer }

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

  { TBZCustomGraphicControl }

  TBZCustomGraphicControl = Class(TGraphicControl)
  private

  protected
    FBackBuffer : TBZBitmap;
    //procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); virtual;
    //procedure MouseMove(Shift: TShiftState; X,Y: Integer); virtual;
    //procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); virtual;
    //function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; virtual;
    //function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; virtual;
    //function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; virtual;
    procedure RenderControl; Virtual;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property Constraints;
    property BorderSpacing;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property ParentShowHint;

    //property AnchorSideLeft;
    //property AnchorSideTop;
    //property AnchorSideRight;
    //property AnchorSideBottom;
    property Cursor;
    property Left;
    property Height;
    property Top;
    property Width;
    property Hint;
    property HelpType;
    property HelpKeyword;
    property HelpContext;
  end;

Type


  { TBZColorProperty }
  TBZColorProperty = Class(TBZCustomProperty)
  private
    FColor : TBZColor;
    function GetColorRed : Byte;
    procedure SetColorRed(const AValue : Byte);
    function GetColorGreen : Byte;
    procedure SetColorGreen(const AValue : Byte);
    function GetColorBlue : Byte;
    procedure SetColorBlue(const AValue : Byte);
    function GetColorAlpha : Byte;
    procedure SetColorAlpha(const AValue : Byte);
  protected
  public
    Constructor CreateInitialized(AControl: TComponent; AColor : TBZColor);

    function GetColor : TBZColor;
    function AsColorVector : TBZColorVector;

  published
    property Red   : Byte read GetColorRed write SetColorRed;
    property Green : Byte read GetColorGreen write SetColorGreen;
    property Blue  : Byte read GetColorBlue write SetColorBlue;
    property Alpha : Byte read GetColorAlpha write SetColorAlpha;

    property OnChange;
  end;


  TBZRectProperty  = Class(TBZUpdateAbleObject)

  end;


implementation


{ TBZColorProperty }

Constructor TBZColorProperty.CreateInitialized(AControl : TComponent; AColor : TBZColor);
begin
  inherited Create(AControl);
  FColor := AColor;
end;

function TBZColorProperty.GetColor : TBZColor;
begin
  Result := FColor;
end;

function TBZColorProperty.AsColorVector : TBZColorVector;
begin
  Result := FColor.AsColorVector;
end;

function TBZColorProperty.GetColorAlpha : Byte;
begin
  Result := FColor.Alpha;
end;

function TBZColorProperty.GetColorBlue : Byte;
begin
  Result := FColor.Blue;
end;

function TBZColorProperty.GetColorGreen : Byte;
begin
  Result := FColor.Green;
end;

function TBZColorProperty.GetColorRed : Byte;
begin
  Result := FColor.Red;
end;

procedure TBZColorProperty.SetColorAlpha(const AValue : Byte);
begin
  FColor.Alpha := AValue;
end;

procedure TBZColorProperty.SetColorBlue(const AValue : Byte);
begin
  FColor.Blue := AValue;
end;

procedure TBZColorProperty.SetColorGreen(const AValue : Byte);
begin
  FColor.Green := AValue;
end;

procedure TBZColorProperty.SetColorRed(const AValue : Byte);
begin
  FColor.Red := AValue;
end;

{%region==== [ TBZCustomControlObjectDrawer ]=====================================}

Constructor TBZCustomControlObjectDrawer.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  //FPostition := TBZCoordinates2D.CreateInitialized(Self, Vec2(0,0), csPoint2D);
  FHeight := 90;
  FWidth := 90;
  FSurface := TBZBitmap.Create(Width, Height);
  FSurface.Clear(clrTransparent);
  FNeedUpdate := True;
end;

Destructor TBZCustomControlObjectDrawer.Destroy;
begin
  FreeAndNil(FSurface);
  inherited Destroy;
end;

//procedure TBZCustomControlObjectDrawer.SetPosition(const AValue : TBZCoordinates2D);
//begin
//  if FPosition = AValue then Exit;
//  FPosition := AValue;
//end;

procedure TBZCustomControlObjectDrawer.SetHeight(const AValue : Integer);
begin
  if FHeight = AValue then Exit;
  FHeight := AValue;
  FSurface.SetSize(FWidth, FHeight);
  FSurface.Clear(clrTransparent);
  FNeedUpdate := True;
end;

procedure TBZCustomControlObjectDrawer.SetWidth(const AValue : Integer);
begin
  if FWidth = AValue then Exit;
  FWidth := AValue;
  FSurface.SetSize(FWidth, FHeight);
  FSurface.Clear(clrTransparent);
  FNeedUpdate := True;
end;

//procedure TBZCustomControlObjectDrawer.SetRotationAngle(const AValue : Single);
//begin
//  if FRotationAngle = AValue then Exit;
//  FRotationAngle := AValue;
//end;

procedure TBZCustomControlObjectDrawer.Render;
begin
  FNeedUpdate := False;
end;

procedure TBZCustomControlObjectDrawer.NotifyChange(Sender : TObject);
begin
  inherited NotifyChange(Sender);
  FNeedUpdate := True;
end;

//procedure TBZCustomControlObjectDrawer.HandleMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
//begin
//  // Ne fait rien ici
//end;
//
//procedure TBZCustomControlObjectDrawer.HandleMouseMove(Sender : TObject; Shift : TShiftState; X, Y : Integer);
//begin
//  // Ne fait rien ici
//end;
//
//procedure TBZCustomControlObjectDrawer.HandleMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
//begin
//  // Ne fait rien ici
//end;

//procedure TBZCustomControlObjectDrawer.CoordinateChanged(Sender : TBZCustomCoordinates);
//begin
//  if (Sender = Size) then
//  begin
//    FSurface.SetSize(Round(FSize.X), Round(FSize.Y));
//    FSurface.Clear(clrTransparent);
//    Render;
//  end;
//end;

function TBZCustomControlObjectDrawer.GetSurface : TBZBitmap;
begin
  if FNeedUpdate then Render;
  Result := FSurface;
end;

//procedure TBZCustomControlObjectDrawer.RenderToBitmap(DestBmp : TBZBitmap);
//begin
//  DestBmp.PutImage(FSurface,Round(FPosition.X), Round(FPosition.Y),255 , dmSet, amAlpha);
//end;

{%endregion%}

{%region=====[ TBZCustomCoordinates ]============================================================================}

constructor TBZCustomCoordinates.CreateInitialized(AOwner: TPersistent;
  const AValue: TBZVector; const AStyle: TBZCoordinatesStyle = CsUnknown);
begin
  Create(AOwner);
  Initialize(AValue);
  FStyle := AStyle;
end;

destructor TBZCustomCoordinates.Destroy;
begin
  if Assigned(FPDefaultCoords) then
    Dispose(FPDefaultCoords);
  inherited;
end;

procedure TBZCustomCoordinates.Initialize(const Value: TBZVector);
begin
  FCoords := Value;
  //if vUseDefaultCoordinateSets then
  //begin
  //  if not Assigned(FPDefaultCoords) then New(FPDefaultCoords);
  //  FPDefaultCoords^ := Value;
  //end;
end;

procedure TBZCustomCoordinates.Assign(Source: TPersistent);
begin
  if Source is TBZCustomCoordinates then
    FCoords := TBZCustomCoordinates(Source).FCoords
  else
    inherited;
end;

procedure TBZCustomCoordinates.WriteToFiler(Writer: TWriter);
var
  WriteCoords: Boolean;
begin
  with Writer do
  begin
    WriteInteger(0); // Archive Version 0
    //if vUseDefaultCoordinateSets then WriteCoords := (FPDefaultCoords^ <> FCoords) //not VectorEquals(FPDefaultCoords^, FCoords)
    //else
      WriteCoords := True;
    WriteBoolean(WriteCoords);
    if WriteCoords then Write(FCoords.V[0], SizeOf(FCoords));
  end;
end;

procedure TBZCustomCoordinates.ReadFromFiler(Reader: TReader);
var
  N: Integer;
begin
  with Reader do
  begin
    ReadInteger; // Ignore ArchiveVersion
    if ReadBoolean then
    begin
      N := SizeOf(FCoords);
      Assert(N = 4 * SizeOf(Single));
      Read(FCoords.V[0], N);
    end
    else if Assigned(FPDefaultCoords) then FCoords := FPDefaultCoords^;
  end;
end;

procedure TBZCustomCoordinates.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Coordinates', @ReadData, @WriteData,
    not(Assigned(FPDefaultCoords) and (FPDefaultCoords^ = FCoords))); //VectorEquals(FPDefaultCoords^, FCoords)));
end;

procedure TBZCustomCoordinates.ReadData(Stream: TStream);
begin
  Stream.Read(FCoords, SizeOf(FCoords));
end;

procedure TBZCustomCoordinates.WriteData(Stream: TStream);
begin
  Stream.Write(FCoords, SizeOf(FCoords));
end;

procedure TBZCustomCoordinates.NotifyChange(Sender: TObject);
var
  Int: IBZCoordinatesUpdateAble;
begin
  if Supports(Owner, IBZCoordinatesUpdateAble, Int) then Int.CoordinateChanged(TBZCoordinates(Self));
  inherited NotifyChange(Sender);
end;

procedure TBZCustomCoordinates.Translate(const TranslationVector: TBZVector);
begin
  FCoords := FCoords + TranslationVector;
  NotifyChange(Self);
end;

procedure TBZCustomCoordinates.Translate(const TranslationVector : TBZAffineVector);
Var
  Tmp:TBZVector;
begin
  Tmp.AsVector3f := TranslationVector;
  FCoords := FCoords + Tmp;
  NotifyChange(Self);
end;

procedure TBZCustomCoordinates.AddScaledVector(const Factor: Single; const TranslationVector: TBZVector);
begin
  //CombineVector(FCoords, TranslationVector, F);
  FCoords := FCoords.Combine(TranslationVector, Factor);
  NotifyChange(Self);
end;

procedure TBZCustomCoordinates.AddScaledVector(const Factor: Single; const TranslationVector: TBZAffineVector);
Var
  Tmp:TBZVector;
begin
  Tmp.AsVector3f := TranslationVector;

  //CombineVector(FCoords, TranslationVector, F);
  FCoords := FCoords.Combine(Tmp, Factor);
  NotifyChange(Self);
end;

procedure TBZCustomCoordinates.Rotate(const AnAxis: TBZAffineVector; AnAngle: Single);
Var
  Tmp : TBZVector;
begin
  Tmp.AsVector3f := AnAxis;
  FCoords := FCoords.Rotate(Tmp, AnAngle);
  NotifyChange(Self);
end;

procedure TBZCustomCoordinates.Rotate(const AnAxis: TBZVector; AnAngle: Single);
begin
  //RotateVector(FCoords, AnAxis, AnAngle);
  FCoords := FCoords.Rotate(AnAxis, AnAngle);
  NotifyChange(Self);
end;

procedure TBZCustomCoordinates.Normalize;
begin
  //NormalizeVector(FCoords);
  FCoords := FCoords.Normalize;
  NotifyChange(Self);
end;

procedure TBZCustomCoordinates.Invert;
begin
  //NegateVector(FCoords);
  FCoords := - FCoords;
  NotifyChange(Self);
end;

procedure TBZCustomCoordinates.Scale(Factor: Single);
begin
  //ScaleVector(PAffineVector(@FCoords)^, Factor);
  FCoords:= FCoords * Factor;
  NotifyChange(Self);
end;

function TBZCustomCoordinates.VectorLength: Single;
begin
  Result := FCoords.Length; //GLVectorGeometry.VectorLength(FCoords);
end;

function TBZCustomCoordinates.VectorNorm: Single;
begin
  Result := FCoords.Norm; //GLVectorGeometry.VectorNorm(FCoords);
end;

function TBZCustomCoordinates.MaxXYZ: Single;
begin
  Result := FCoords.MaxXYZComponent; //GLVectorGeometry.MaxXYZComponent(FCoords);
end;

function TBZCustomCoordinates.Equals(const AVector: TBZVector): Boolean;
begin
  Result := (FCoords = AVector); // VectorEquals(FCoords, AVector);
end;

procedure TBZCustomCoordinates.SetVector(const X, Y: Single; Z: Single = 0);
begin
  //Assert(FStyle = CsVector, CsVectorHelp);
  //GLVectorGeometry.SetVector(FCoords, X, Y, Z);
  FCoords.Create(X,Y,Z);
  NotifyChange(Self);
end;

procedure TBZCustomCoordinates.SetVector(const V: TBZAffineVector);
begin
  //Assert(FStyle = CsVector, CsVectorHelp);
  //GLVectorGeometry.SetVector(FCoords, V);
  FCoords.Create(V);
  NotifyChange(Self);
end;

procedure TBZCustomCoordinates.SetVector(const V: TBZVector);
begin
  //Assert(FStyle = CsVector, CsVectorHelp);
  //GLVectorGeometry.SetVector(FCoords, V);
  FCoords := V;
  NotifyChange(Self);
end;

procedure TBZCustomCoordinates.SetVector(const X, Y, Z, W: Single);
begin
  //Assert(FStyle = CsVector, CsVectorHelp);
  //GLVectorGeometry.SetVector(FCoords, X, Y, Z, W);
  FCoords.Create(X, Y, Z, W);
  NotifyChange(Self);
end;

procedure TBZCustomCoordinates.SetDirectCoordinate(const Index: Integer; const AValue: Single);
begin
  FCoords.V[index] := AValue;
end;

procedure TBZCustomCoordinates.SetDirecVector(const V: TBZVector);
begin
  FCoords.X := V.X;
  FCoords.Y := V.Y;
  FCoords.Z := V.Z;
  FCoords.W := V.W;
end;

procedure TBZCustomCoordinates.SetToZero;
begin
  FCoords.X := 0;
  FCoords.Y := 0;
  FCoords.Z := 0;
  if FStyle = CsPoint then FCoords.W := 1
  else FCoords.W := 0;
  NotifyChange(Self);
end;

procedure TBZCustomCoordinates.SetPoint(const X, Y, Z: Single);
begin
  //Assert(FStyle = CsPoint, CsPointHelp);
  //GLVectorGeometry.MakePoint(FCoords, X, Y, Z);
  FCoords.Create(X, Y, Z, 1);
  NotifyChange(Self);
end;

procedure TBZCustomCoordinates.SetPoint(const V: TBZAffineVector);
begin
  //Assert(FStyle = CsPoint, CsPointHelp);
  //GLVectorGeometry.MakePoint(FCoords, V);
  FCoords.Create(V, 1);
  NotifyChange(Self);
end;

procedure TBZCustomCoordinates.SetPoint(const V: TBZVector);
begin
 // Assert(FStyle = CsPoint, CsPointHelp);
  //GLVectorGeometry.MakePoint(FCoords, V);
  FCoords := V;
  FCoords.W := 1;
  NotifyChange(Self);
end;

procedure TBZCustomCoordinates.SetPoint2D(const X, Y: Single);
begin
  //Assert(FStyle = CsPoint2D, CsPoint2DHelp);
  //GLVectorGeometry.MakeVector(FCoords, X, Y, 0);
  FCoords.Create(X, Y, 0, 1);
  NotifyChange(Self);
end;

procedure TBZCustomCoordinates.SetPoint2D(const Vector: TBZAffineVector);
begin
 // Assert(FStyle = CsPoint2D, CsPoint2DHelp);
  //GLVectorGeometry.MakeVector(FCoords, Vector);
  FCoords.Create(Vector.X, Vector.Y, 0, 1);
  NotifyChange(Self);
end;

procedure TBZCustomCoordinates.SetPoint2D(const Vector: TBZVector);
begin
 // Assert(FStyle = CsPoint2D, CsPoint2DHelp);
  //GLVectorGeometry.MakeVector(FCoords, Vector);
  FCoords.Create(Vector.X, Vector.Y, 0, 1);
  NotifyChange(Self);
end;

procedure TBZCustomCoordinates.SetPoint2D(const Vector: TBZVector2f);
begin
  //Assert(FStyle = CsPoint2D, CsPoint2DHelp);
  //GLVectorGeometry.MakeVector(FCoords, Vector.V[0], Vector.V[1], 0);
  FCoords.Create(Vector.X, Vector.Y, 0, 1);
  NotifyChange(Self);
end;

//function TBZCustomCoordinates.AsAddress: PGLFloat;
//begin
//  Result := @FCoords;
//end;

procedure TBZCustomCoordinates.SetAsVector(const Value: TBZVector);
begin
  FCoords := Value;
  case FStyle of
    CsPoint2D:
      begin
        FCoords.Z := 0;
        FCoords.W := 0;
      end;
    CsPoint:
      FCoords.W := 1;
    CsVector:
      FCoords.W := 0;
  else
    Assert(False);
  end;
  NotifyChange(Self);
end;

procedure TBZCustomCoordinates.SetAsAffineVector(const Value: TBZAffineVector);
begin
  case FStyle of
    CsPoint2D:
      FCoords.Create(Value.X,Value.Y,0,0);
    CsPoint:
      FCoords.Create(Value,1);
    CsVector:
      FCoords.Create(Value);
  else
    Assert(False);
  end;
  NotifyChange(Self);
end;

procedure TBZCustomCoordinates.SetAsPoint2D(const Value: TBZVector2f);
begin
  case FStyle of
    CsPoint2D, CsPoint, CsVector:
      begin
        FCoords.X := Value.X;
        FCoords.Y := Value.Y;
        FCoords.Z := 0;
        FCoords.W := 0;
      end;
  else
    Assert(False);
  end;
  NotifyChange(Self);
end;

function TBZCustomCoordinates.GetAsAffineVector: TBZAffineVector;
begin
  Result := FCoords.AsVector3f;// GLVectorGeometry.SetVector(Result, FCoords);
end;

function TBZCustomCoordinates.GetAsPoint2D: TBZVector2f;
begin
  Result.X := FCoords.X;
  Result.Y := FCoords.Y;
end;

procedure TBZCustomCoordinates.SetCoordinate(const AIndex: Integer;const AValue: Single);
begin
  FCoords.V[AIndex] := AValue;
  NotifyChange(Self);
end;

function TBZCustomCoordinates.GetCoordinate(const AIndex: Integer): Single;
begin
  Result := FCoords.V[AIndex];
end;

function TBZCustomCoordinates.GetDirectCoordinate(const Index: Integer): Single;
begin
  Result := FCoords.V[index]
end;

function TBZCustomCoordinates.GetAsString: String;
begin
  case Style of
    CsPoint2D:
      Result := Format('(%g; %g)', [FCoords.X, FCoords.Y]);
    CsPoint:
      Result := Format('(%g; %g; %g)', [FCoords.X, FCoords.Y, FCoords.Z]);
    CsVector:
      Result := Format('(%g; %g; %g; %g)', [FCoords.X, FCoords.Y, FCoords.Z, FCoords.W]);
  else
    Assert(False);
  end;
end;

{%endregion%}

{%region==== [ TBZCustomGraphicControl ]======================================================}

procedure TBZCustomGraphicControl.RenderControl;
begin
  // ne fait rien ici
end;

procedure TBZCustomGraphicControl.Paint;
begin
  //if Assigned(FOnPaint) then FOnPaint(Self);
  FBackBuffer.DrawToCanvas(Canvas, ClientRect, True, False);
end;

constructor TBZCustomGraphicControl.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  // Dimension minimum du control
  Width := 60;
  Height := 60;
  FBackBuffer := TBZBitmap.Create(Width, Height);
  FBackBuffer.Clear(clrTransparent);
end;

destructor TBZCustomGraphicControl.Destroy;
begin
  inherited Destroy;
end;

{%endregion%}

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

{%region==== [ TBZCustomControl ]==============================================}

Constructor TBZCustomControl.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  Self.Parent := TWinControl(AOwner);
  Self.DoubleBuffered:=True;

  ControlStyle := [csCaptureMouse, csAcceptsControls, csClickEvents, csSetCaption,
                   csDoubleClicks, csReplicatable, csNoFocus, csOpaque, csParentBackground];

  FTransparent := False;
  //FOnPaint := nil;
End;



Procedure TBZCustomControl.SetTransparent(Const AValue: Boolean);
Begin
  If FTransparent = AValue Then exit;
  FTransparent := AValue;
  If FTransparent Then
    ControlStyle := ControlStyle - [csOpaque]
  Else
    ControlStyle := ControlStyle + [csOpaque];
  DoOnSetTransparent;
  Paint;
End;

Procedure TBZCustomControl.DoOnSetTransparent;
Begin
  // Do Nothing
End;

// Prevent panel from clearing background
(*Procedure TBZCustomControl.EraseBackground(DC: HDC);
Begin
  // Ne fait rien
  //Inherited EraseBackground(DC);
End; *)


Procedure TBZCustomControl.Paint;
Begin
  inherited paint;
  //If Assigned(FOnPaint) Then FOnPaint(Self);
End;

{%endregion%}

end.

