(*
  @abstract(Contient des classes de bases conçues à des fins génériques pour gérer
  l'affichage de primitives et de textes sur un "Canvas".)

  Il vous faudras hériter ces classes afin de prendre en charge différents type de surface (Bitmap, OpenGL, Vulkan, DirectX etc...) @br
  Pour la gestion des textes, vous devrez heriter les classes de l'unité BZFontClasses afin d'adapter l'affichage
  des textes en fonction du type de "Surface". Puis dans votre classe finale, vous devrez assigner votre moteur de rendu de texte

  --------------------------------------------------------------------------------

  @created(2016-11-16)
  @author(J.Delauney (BeanzMaster))
  Historique :
  @unorderedList(
    @item(06/06/2019 : Creation  )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) :

  -------------------------------------------------------------------------------------------------------------

  @bold(Dependances) : BZClasses, BZGraphic, BZUtils, BZMath, BZVectorMath, BZGeoTools, BZFontClasses

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)
   @unorderedList(
     @item(J.Delauney (BeanzMaster))
   )

  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZCanvasClasses;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}

//==============================================================================

interface

uses
  Classes, SysUtils,
  BZClasses, BZColors, BZGraphic, BZVectorMath, BZGeoTools, BZFontClasses;

Type
  { Exception levée en cas d'erreur avec le canvas }
  EBZCanvasException = Class(EBZBaseException);

  { Definition de base des propriétés du "Crayon" pour un Canvas }
  TBZCustomCanvasPen =  Class(TBZUpdateAbleObject)
  Private
    FWidth: Word;
    FColor: TBZColor;
    FStyle: TBZStrokeStyle;
    FPatternStyle : TBZPatternStyle;
    FStartCap, FEndCap : TBZCapsMode;
    FJoinStyle : TBZJoinStyle;

    //FGradient : TBZGradientProperty
    //FTexture

    procedure SetWidth(Value : Word);
    function GetWidth : Word;
    procedure SetColor(Value : TBZColor);
    function GetColor : TBZColor;
    procedure SetStyle(Value : TBZStrokeStyle);
    function GetStyle : TBZStrokeStyle;
    procedure SetPatternStyle(Value : TBZPatternStyle);
    function GetPatternStyle : TBZPatternStyle;
    procedure SetStartCap(Value : TBZCapsMode);
    function GetStartCap : TBZCapsMode;
    procedure SetEndCap(Value : TBZCapsMode);
    function GetEndCap : TBZCapsMode;
    procedure SetJoinStyle(Value : TBZJoinStyle);
    function GetStartStyle : TBZJoinStyle;
  Protected
    StrokePattern: TBZCanvasPenStrokePattern;
  Public

    CustomStrokePattern : TBZCanvasPenStrokePattern;

    { Création }
    Constructor Create; Override;

    { Assignation des propriétés d'un autre TBZCustomCanvasPen }
    Procedure Assign(Source: TPersistent); Override;

    { Style du tracé }
    Property Style: TBZStrokeStyle read GetStyle write SetStyle;
    { Epaisseur du tracé }
    Property Width: Word read GetWidth write SetWidth;
    { Couleur du tracé }
    Property Color: TBZColor read GetColor write SetColor;
    { Style de fin de tracé }
    property EndCap : TBZCapsMode Read FEndCap Write FEndCap;
    { Style du début de tracé }
    property StartCap : TBZCapsMode Read FStartCap write FStartCap;
    { Type de jointure pour les contours entre plusieurs tracés (pour les Polygones par exemple) }
    property JoinStyle : TBZJoinStyle Read FJoinStyle Write FJoinStyle;
    { Type de pattern (motif) à utiliser }
    property PatternStyle : TBZPatternStyle read  GetPatternStyle write SetPatternStyle;

  End;
  { Classe de type TBZCustomCanvasPen }
  TBZCanvasPenClass = class of TBZCustomCanvasPen;

  { Definition de base des propriétés pour le remplisage de surface avec une texture }
  TBZBrushTextureProperty = Class(TBZUpdateableObject)
  private
    FBitmap : TBZCustomBitmap;
    FMappingKind : TBZTextureMappingKind;
    FTileX, FTileY : Word;
    procedure SetTileX(const AValue : Word);
    procedure SetTileY(const AValue : Word);
  protected
  public
    { Création }
    Constructor Create; override;
    { Destruction }
    Destructor Destroy; Override;

    { Texture }
    property Bitmap : TBZCustomBitmap read FBitmap write FBitmap;
    { Mode d'application de la texture }
    property MappingKind : TBZTextureMappingKind read FMappingKind write FMappingKind;
    { Nombre de répétition horizontale }
    property TileX : Word read FTileX write SetTileX;
    { Nombre de répétition verticale }
    property TileY : Word read FTileY write SetTileY;
  end;

  { Définition de base des propriétés de la brosse pour le remplisage de surface }
  TBZCustomCanvasBrush =  Class(TBZUpdateAbleObject)
  Private
    FStyle:    TBZBrushStyle;
    FColor:    TBZColor;
    FPatternStyle : TBZPatternStyle;
    FGradient : TBZGradientProperty;
    FTexture : TBZBrushTextureProperty;
    FBounds : TBZRectItemProperty;
    FAutoBounds, FAutoCenter : Boolean;
    FCenterPoint : TBZPoint;


    procedure SetStyle(Value : TBZBrushStyle);
    function GetStyle : TBZBrushStyle;
    procedure SetColor(Value : TBZColor);
    function GetColor : TBZColor;
    procedure SetPatternStyle(Value : TBZPatternStyle);
    function GetPatternStyle : TBZPatternStyle;
  Protected
    FBrushPattern:  TBZBrushPattern;
    FMaxDist : Integer;  //Single
  Public
    CustomPattern:  TBZBrushPattern;

    { Création }
    Constructor Create; Override;
    { Destruction }
    Destructor Destroy; Override;

    { Assignation des propriétés d'un autre TBZCustomCanvasBrush}
    Procedure Assign(Source: TPersistent); Override;

    { Initialisation du dégradé }
    procedure InitGradient; overload;
    procedure InitGradient(rx,ry : Single); overload;

    { Initialisation d'une texture }
    procedure InitTexture;
    function GetMaxDist : Integer;

    { Style de la brosse }
    Property Style: TBZBrushStyle read FStyle write SetStyle;
    { Couleur de la brosse }
    Property Color: TBZColor read FColor write FColor;
    { Motif de la brosse }
    Property PatternStyle : TBZPatternStyle read GetPatternStyle write SetPatternStyle;
    { Propriété d'un dégradé de couleurs }
    Property Gradient : TBZGradientProperty read FGradient;
    { Propriété d'une texture }
    Property Texture : TBZBrushTextureProperty read FTexture;

    { Active / Désactive le calcul automatique des bornes du remplissage. @br
      Est utilisé avec les dégradés }
    property AutoBounds : Boolean Read FAutoBounds Write FAutoBounds;
    { Définie les limites de remplissage. @br
      Est utilisé avec les dégradés }
    Property Bounds : TBZRectItemProperty Read FBounds Write FBounds;
    { Active / Désactive le calcul automatique du centre de remplissage. @br
      Est utilisé avec les dégradés }
    property AutoCenter : Boolean Read FAutoCenter Write FAutoCenter;
    { Définie le centre de remplissage. @br
      Est utilisé avec les dégradés }
    property CenterPoint : TBZPoint read FCenterPoint write FCenterPoint;
  End;
  { Classe de type TBZCustomCanvasBrush }
  TBZCanvasBrushClass = class of TBZCustomCanvasBrush;

  { @abstract(Définition de base des propriétiés liées au mode d'affichage des pixels.)

    Voir également : @br
    @unorderedlist(
      @item(PixelMode: TBZBitmapDrawMode)
      @item(AlphaMode: TBZBitmapAlphaMode)
      @item(SrcFactor: TBZBlendingFactor)
      @item(DstFactor: TBZBlendingFactor)) }
  TBZCanvasDrawModeProperty = Class(TBZUpdateAbleObject)
  Private
    FPixelMode: TBZBitmapDrawMode;
    FAlphaMode: TBZBitmapAlphaMode;
    FMasterAlpha : Byte;
    FCombineMode : TBZColorCombineMode;
    FSrcFactor: TBZBlendingFactor;
    FDstFactor: TBZBlendingFactor;
    //FBlendExMode : TBZBlendExMode;
  Public
    { Création }
    Constructor Create; override;

    { Mode d'écriture d'un pixel }
    Property PixelMode: TBZBitmapDrawMode read FPixelMode write FPixelMode;
    { Mode d'application du canal Alpha }
    Property AlphaMode: TBZBitmapAlphaMode read FAlphaMode write FAlphaMode;
    { Valeur Alpha principale }
    property MasterAlpha : Byte read FMasterAlpha write FMasterAlpha;
    { Mode de fusion d'un pixel }
    property CombineMode : TBZColorCombineMode read FCombineMode write FCombineMode;
    { Facteur de fusion pour la source d'un pixel }
    Property BlendSrcFactor: TBZBlendingFactor read FSrcFactor write FSrcFactor;
    { Facteur de fusion pour la destination d'un pixel }
    Property BlendDstFactor: TBZBlendingFactor read FDstFactor write FDstFactor;
  End;

  { @abstract(Classe abstraite à hérité pour le dessin de primitive "vectorielles" sur une surface graphique).

    Les positions et autres paramètres, par défaut, sont exprimés en virgule flottante }

  { TBZCustomCanvas }

  TBZCustomCanvas = Class(TBZUpdateAbleObject)
  private
    FSurfaceWidth, FSurfaceHeight:integer;

    FSurfaceViewPort : TBZRect;

    FClipping : Boolean;
    FClipRect : TBZFloatRect;

    {$CODEALIGN RECORDMIN=16}
    FCurrentPos : TBZVector2f;
    {$CODEALIGN RECORDMIN=4}
    FCurrentAngle: Single;

    FAntialias : Boolean;
    FDrawMode: TBZCanvasDrawModeProperty;

    procedure SetCanvasDrawMode(Value : TBZCanvasDrawModeProperty);
    function GetCanvasDrawMode : TBZCanvasDrawModeProperty;

    function GetFont : TBZCustomFont;
  protected
    FDefaultFont, FFont : TBZCustomFont;
    FPen : TBZCustomCanvasPen;
    FBrush : TBZCustomCanvasBrush;

    //FMatrix : TBZMatrix;
    function CreateDefaultFont : TBZCustomFont;

    function Internal_CreateDefaultFont : TBZCustomFont; virtual; abstract;
    procedure SetFont (AValue:TBZCustomFont); virtual;

    procedure Internal_TextOut (x,y:integer;text:string); virtual; abstract;
    procedure Internal_GetTextSize (text:string; var w,h:integer); virtual; abstract;
    function  Internal_GetTextHeight (text:string) : integer; virtual; abstract;
    function  Internal_GetTextWidth (text:string) : integer; virtual; abstract;

    procedure Internal_TextOut (x,y:integer;text:unicodestring); virtual;
    procedure Internal_GetTextSize (text:unicodestring; var w,h:integer); virtual;
    function  Internal_GetTextHeight (text:unicodestring) : integer; virtual;
    function  Internal_GetTextWidth (text:unicodestring) : integer; virtual;

    procedure DrawVLine(x1,y1,h : Single); virtual; abstract;
    procedure DrawHLine(x1,y1,l : Single); virtual; abstract;
    procedure DrawLine(x1,y1,x2,y2 : Single); virtual; abstract;
    procedure DrawAntiAliasLine(x1,y1,x2,y2 : Single); virtual; abstract;

    procedure DrawPolyLine(Pts : TBZArrayOfFloatPoints; Const Closed : Boolean = False); virtual; abstract;
    procedure DrawAntiaAliasPolyLine(Pts : TBZArrayOfFloatPoints; Const Closed : Boolean = False); virtual; abstract;
    // procedure DrawBezierLine; virtual; abstract;
    // procedure DrawSplineLine; virtual; abstract;

    procedure DrawPolygon(Pts : TBZArrayOfFloatPoints); virtual; abstract;

    procedure DrawRectangle(x1,y1,x2,y2 : Single); virtual; abstract;
    procedure DrawRoundedRect(x1,y1,x2,y2 : Single;  Const Rx : Single = 3; Const Ry : Single = 3); virtual; abstract;

    procedure DrawArc(cx, cy, rx, ry, StartAngle, EndAngle: Single; Const ClockWise : Boolean = True); virtual; abstract;
    //procedure DrawAntiAliasArc(cx, cy, rx, ry, StartAngle, EndAngle: Single); virtual; abstract;

    procedure DrawCircle(cx,cy, Radius : Single); virtual; abstract;
    procedure DrawAntiAliasCircle(cx, cy, Radius : Single); virtual; abstract;

    procedure DrawEllipse(cx, cy, Rx, Ry: Single); virtual; abstract;
    procedure DrawAntiAliasEllipse(cx, cy, Rx, Ry: Single); virtual; abstract;

    procedure DrawTriangle(x1,y1,x2,y2,x3,y3 : Single); virtual; abstract;

    //procedure DrawQuadraticBezierCurve(P1, C1, P2 : TBZFloatPoint); virtual; abstract;


    //procedure DrawPath; virtual; abstract;
    //procedure DrawText; virtual; abstract;

    procedure FillPolygon(Pts : TBZArrayOfFloatPoints); virtual; abstract;
    procedure FillTriangle(x1, y1, x2, y2, x3, y3 : Single);  virtual; abstract;
    procedure FillRectangle(x1,y1,x2,y2 : Single); virtual; abstract;
    procedure FillRoundedRectangle(x1,y1,x2,y2 : Single; Const Rx : Single = 3; Const Ry : Single = 3); virtual; abstract;

    procedure FillArc(StartX, StartY, EndX, EndY, StartAngle, EndAngle : Single); virtual; abstract;
    procedure FillCircle(cx,cy, Radius : Single); virtual; abstract;
    procedure FillEllipse(cx, cy, Rx, Ry : Single);  virtual; abstract;


    //procedure FillPath; virtual; abstract;
  public
    { Création }
    constructor Create; override;
    { Création avec définition de la largeur et hauteur }
    constructor Create(const AWidth, AHeight: integer);  virtual; overload;
    { Destruction }
    destructor Destroy; override;

    { Création d'un objet TBZCustomFont }
    function CreateFont : TBZCustomFont;

    { Début des mises à jour de la surface }
    procedure BeginUpdate; virtual;
    { Fin des mises à jour de la surface }
    procedure EndUpdate; Virtual;

    { Libération du tampon d'affichage }
    procedure FreeBuffer;  virtual; abstract;
    { Redimensionnement du tampon d'affichage }
    procedure ResizeBuffer(const AWidth, AHeight: integer);  virtual; abstract;

    { Affiche un texte à la position x,y. Note la position se réfère au coin bas et gauche du texte }
    procedure TextOut (x,y:integer;text:string); virtual;
    { Retourne les dimensions du texte en pixel avec la police en cours d'utilisation }
    procedure GetTextSize (text:string; var w,h:integer);
    { Retourne la hauteur du texte en pixel avec la police en cours d'utilisation }
    function GetTextHeight (text:string) : integer;
    { Retourne la longueur du texte en pixel avec la police en cours d'utilisation }
    function GetTextWidth (text:string) : integer;

    // function TextExtent(const Text: string): TSize; virtual;
    { Retourne la hauteur d'un texte en pixel }
    function TextHeight(const Text: string): Integer; virtual;
    { Retourne la largeur d'un texte en pixel }
    function TextWidth(const Text: string): Integer; virtual;

    { Ecrit un texte UNICODE au coordonnées X et Y }
    //procedure TextOut (x,y:integer;text:String); virtual;

    // TODO : A placer dans protected ???
    { Retourne la hauteur et la largeur d'un texte UNICODE en pixel }
    procedure GetTextSize (text:unicodestring; var w,h:integer);
    { Retourne la hauteur d'un texte UNICODE en pixel }
    function GetTextHeight (text:unicodestring) : integer;
    { Retourne la largeur d'un texte UNICODE en pixel }
    function GetTextWidth (text:unicodestring) : integer;

    // function TextExtent(const Text: unicodestring): TSize; virtual;
    { Retourne la largeur d'un texte UNICODE en pixel }
    function TextHeight(const Text: unicodestring): Integer; virtual;
    { Retourne la largeur d'un texte UNICODE en pixel }
    function TextWidth(const Text: unicodestring): Integer; virtual;
    { Ecrit un texte UNICODE au coordonnées X et Y }
    procedure TextOut (x,y:integer;text:unicodestring); virtual;

    { Dessine une ligne verticale }
    Procedure VLine(x1, y1, y2: Single); overload;
    { Dessine une ligne verticale }
    Procedure VLine(PtFrom : TBZFloatPoint; h: Single); overload;
    { Dessine une ligne horizontale }
    Procedure HLine(x1, y1, x2: Single); overload;
    { Dessine une ligne horizontale }
    Procedure HLine(PtFrom : TBZFloatPoint; L: Single); overload;
    { Dessine une ligne }
    procedure Line(x1,y1,x2,y2 : Single); overload;
    { Dessine une ligne }
    procedure Line(PtFrom, PtTo : TBZFloatPoint); overload;
    { Dessine une polyligne }
    Procedure PolyLine(pnts: TBZArrayOfFloatPoints; Const Closed : Boolean = False);
    { Dessine un triangle }
    Procedure Triangle(x1, y1, x2, y2, x3, y3: Single); overload;
    { Dessine un triangle }
    Procedure Triangle(Pt1,Pt2,Pt3 : TBZFloatPoint); overload;
    { Dessine un polygone }
    Procedure Polygon(Const Pnts: TBZArrayOfFloatPoints);
    { Dessine un rectangle }
    Procedure Rectangle(x1, y1, x2, y2: Single); Overload;
    { Dessine un rectangle }
    Procedure Rectangle(ARect: TBZFloatRect); Overload;
    { Dessine un rectangle }
    Procedure Rectangle(ARect: TBZRect); Overload;
    { Dessine un rectangle }
    Procedure Rectangle(ptl, pbr : TBZFloatPoint); Overload;
    { Dessine un rectangle aux coins arrondis }
    Procedure RoundedRect(x1, y1, x2, y2, Rx, Ry: Single); Overload;
    { Dessine un rectangle aux coins arrondis }
    Procedure RoundedRect(ARect: TBZFloatRect; Rx, Ry: Single); Overload;
    { Dessine un cercle }
    Procedure Circle(cx, cy, r: Single);
    Procedure Circle(CP : TBZFloatPoint; r: Single);
    { Dessine une ellipse }
    Procedure Ellipse(cx, cy, Rx, Ry: Single);
    { Dessine une ellipse }
    Procedure Ellipse(pc : TBZFloatPoint; Rx, Ry: Single);
    { Dessine une ellipse }
    Procedure Ellipse(ptl, pbr : TBZFloatPoint);
    { Dessine un arc de cercle }
    Procedure Arc(cx, cy, rx, ry, StartAngle, EndAngle: Single);

    { Dessine une courbe de Bezier Quadratic }
    procedure BezierCurve(P1, C1, P2 : TBZFloatPoint; Const nbSteps : Integer = -1); overload;
    { Dessine une courbe de Bezier Quadratic }
    procedure BezierCurve(P1x, P1y, C1x, C1Y, P2x, P2y : Single; Const nbSteps : Integer = -1); overload;
    { Dessine une courbe de Bezier Cubique }
    procedure BezierCurve(P1, C1, C2, P2 : TBZFloatPoint; Const nbSteps : Integer = -1); overload;
    procedure BezierCurve(P1x, P1y, C1x, C1Y, C2x, C2Y, P2x, P2y : Single; Const nbSteps : Integer = -1); overload;


    { Dessine une courbe de Bezier Quadratique }
    //procedure BezierCurve(P1, C1, C2, P2 : TBZFloatPoint); overload;
    { Dessine une courbe de Bezier Quadratique }
    //procedure BezierCurve(P1x, P1y, C1x, C1Y, C2x, C2Y, P2x, P2y : Single); overload;

    { Déplace la position du curseur dans le Canvas }
    procedure MoveTo(x,y : Single); overload;
    { Déplace la position du curseur dans le Canvas }
    procedure MoveTo(pt : TBZFloatPoint); overload;
    { Effectue une rotation du curseur de "A" angle en degré }
    Procedure TurnTo(A: Single);
    { Dessine une ligne de la position du curseur vers la position "x,y" }
    procedure LineTo(x,y:Single); overload;
    { Dessine une ligne de la position du curseur vers la position définie par le point "PtTo" }
    procedure LineTo(PtTo : TBZFloatPoint); overload;
    { Trace une ligne de longueur "L" depuis la position du curseur et dans la direction chosie par la methode "Turn" }
    Procedure TraceTo(L: Single);

    //procedure BezierCurveTo(C1, P2 : TBZFloatPoint);
    //procedure BezierCurveTo(C1, C2, P2 : TBZFloatPoint);

    { Rempli une surface }
    procedure FloodFill(px,py : Single; Const SearchColor, NewColor: TBZColor);  virtual; abstract;

    { Clip le canvas }
    property Clipping : Boolean Read FClipping write FClipping;
    { Definie la zone de clipping }
    property ClipRect : TBZFloatRect read FClipRect write FClipRect;
    { Définie la police et ses attributs pour l'affichage des textes }
    property Font : TBZCustomFont read GetFont write SetFont;
    { Propriété du mode d'affichage. cf : TBZCanvasDrawModeProperty }
    property DrawMode : TBZCanvasDrawModeProperty read FDrawMode write FDrawMode;
    { Dimension de la surface du cnavas }
    property SurfaceViewPort : TBZRect read FSurfaceViewPort;
    { Active ou désactive  "l'anti-aliasing" }
    property AntiAlias : Boolean read FAntialias write FAntiAlias;
  end;
  { Classe de type TBZCustomCanvas }
  TBZCanvasClass = Class of TBZCustomCanvas;

Var
  { Definitions de quleques patternes par défaut }
  //ClearStrokePattern,
  {@groupbegin}
  SolidPenStrokePattern,
  DashPenStrokePattern,
  DotPenStrokePattern,
  DashDotPenStrokePattern,
  DashDotDotPenStrokePattern : TBZCanvasPenStrokePattern;
  {@groupend}

{ Creation d'un masque de "Pattern"}
Function CreateBZStrokePattern(d1, s1, d2, s2, d3, s3, d4, s4: Byte): TBZCanvasPenStrokePattern;

implementation

uses BZMath, BZUtils; //, BZGeoTools;

Function CreateBZStrokePattern(d1, s1, d2, s2, d3, s3, d4, s4: Byte): TBZCanvasPenStrokePattern;
Var
  i, patternSize: Integer;
  id1, is1, id2, is2, id3, is3, id4, is4: Int64;
  dx2, dx3, dx4:  Int64;

Begin
  PatternSize := 0;
  If d4 <> 0 Then
  Begin
    PatternSize := (d1 + s1 + d2 + s2 + d3 + s3 + d4 + s4);
    id1 := d1 - 1;
    is1 := s1 - 1;
    id2 := d2 - 1;
    is2 := s2 - 1;
    id3 := d3 - 1;
    is3 := s3 - 1;
    id4 := d4 - 1;
    is4 := s4 - 1;
    SetLength(Result, PatternSize);

    For i := 0 To id1 Do Result[i] := True;
    For i := 0 To is1 Do Result[i + d1] := False;

    dx2 := (d1 + s1);
    //sx2:=s1;
    For i := 0 To id2 Do Result[dx2 + i] := True;
    For i := 0 To is2 Do Result[dx2 + d2 + i] := False;

    dx3 := dx2 + (d2 + s2);

    For i := 0 To id3 Do Result[dx3 + i] := True;
    For i := 0 To is3 Do Result[dx3 + d3 + i] := False;

    dx4 := dx3 + (d3 + s3);
    For i := 0 To id4 Do Result[dx4 + i] := True;
    For i := 0 To is4 Do Result[dx4 + d4 + i] := False;
  End
  Else If d3 <> 0 Then
  Begin
    PatternSize := (d1 + s1 + d2 + s2 + d3 + s3);
    id1 := d1 - 1;
    is1 := s1 - 1;
    id2 := d2 - 1;
    is2 := s2 - 1;
    id3 := d3 - 1;
    is3 := s3 - 1;
    SetLength(Result, PatternSize);

    For i := 0 To id1 Do
      Result[i] := True;
    For i := 0 To is1 Do
      Result[i + d1] := False;

    dx2 := (d1 + s1);
    //sx2:=s1;
    For i := 0 To id2 Do Result[dx2 + i] := True;
    For i := 0 To is2 Do Result[dx2 + d2 + i] := False;

    dx3 := dx2 + (d2 + s2);

    For i := 0 To id3 Do Result[dx3 + i] := True;
    For i := 0 To is3 Do Result[dx3 + d3 + i] := False;

  End
  Else If d2 <> 0 Then
  Begin
    PatternSize := (d1 + s1 + d2 + s2);
    id1 := d1 - 1;
    is1 := s1 - 1;
    id2 := d2 - 1;
    is2 := s2 - 1;
    SetLength(Result, PatternSize);

    For i := 0 To id1 Do Result[i] := True;
    For i := 0 To is1 Do Result[i + d1] := False;

    dx2 := (d1 + s1);
    //sx2:=s1;
    For i := 0 To id2 Do Result[dx2 + i] := True;
    For i := 0 To is2 Do Result[dx2 + d2 + i] := False;
  End
  Else If d1 <> 0 Then
  Begin
    PatternSize := (d1 + s1);
    id1 := d1 - 1;
    is1 := s1 - 1;

    SetLength(Result, PatternSize);

    For i := 0 To id1 Do Result[i] := True;
    For i := 0 To is1 Do Result[i + d1] := False;
  End;
End;

{ TBZBrushTextureProperty }

procedure TBZBrushTextureProperty.SetTileX(const AValue : Word);
begin
  if FTileX = AValue then Exit;
  FTileX := AValue;
  if FTileX = 0 then FTileX := 1;
end;

procedure TBZBrushTextureProperty.SetTileY(const AValue : Word);
begin
  if FTileY = AValue then Exit;
  FTileY := AValue;
  if FTileY = 0 then FTileY := 1;
end;

constructor TBZBrushTextureProperty.Create;
begin
  inherited Create;
  FBitmap := TBZCustomBitmap.Create;
  FMappingKind := tmkDefault;
  FTileX := 1;
  FTileY := 1;
end;

destructor TBZBrushTextureProperty.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

{ TBZCanvasDrawModeProperty }

constructor TBZCanvasDrawModeProperty.Create;
begin
  inherited Create;
  FPixelMode   := dmSet;
  FAlphaMode   := amNone;
  FMasterAlpha := 255;
  FCombineMode := cmNormal;
  FSrcFactor   := bfDstAlpha;
  FDstFactor   := bfOne;
end;

{%region=====[ TBZCustomCanvasPen ]===============================================}

constructor TBZCustomCanvasPen.Create;
begin
  inherited Create;
  FWidth := 1;
  FStartCap := cmNone;
  FEndCap := cmNone;
  FJoinStyle := jsMitter;
  FColor := clrWhite;
  FStyle := ssSolid;
end;

procedure TBZCustomCanvasPen.Assign(Source : TPersistent);
begin
  if Source is TBZCustomCanvas  then
  begin

  end;
  inherited Assign(Source);
end;

procedure TBZCustomCanvasPen.SetWidth(Value : Word);
begin
  if FWidth = Value then Exit;
  FWidth := Value;
end;

function TBZCustomCanvasPen.GetWidth : Word;
begin
  Result := FWidth;
end;

procedure TBZCustomCanvasPen.SetColor(Value : TBZColor);
begin
  if FColor = Value then Exit;
  FColor := Value;
end;

function TBZCustomCanvasPen.GetColor : TBZColor;
begin
  Result := FColor;
end;

procedure TBZCustomCanvasPen.SetStyle(Value : TBZStrokeStyle);
begin
  if FStyle = Value then Exit;
  FStyle := Value;
end;

function TBZCustomCanvasPen.GetStyle : TBZStrokeStyle;
begin
  Result := FStyle;
end;

procedure TBZCustomCanvasPen.SetPatternStyle(Value : TBZPatternStyle);
begin
  if FPatternStyle = Value then Exit;
  FPatternStyle := Value;
end;

function TBZCustomCanvasPen.GetPatternStyle : TBZPatternStyle;
begin
  Result := FPatternStyle;
end;

procedure TBZCustomCanvasPen.SetStartCap(Value : TBZCapsMode);
begin
  if FStartCap = Value then Exit;
  FStartCap := Value;
end;

function TBZCustomCanvasPen.GetStartCap : TBZCapsMode;
begin
  Result := FStartCap;
end;

procedure TBZCustomCanvasPen.SetEndCap(Value : TBZCapsMode);
begin
  if FEndCap = Value then Exit;
  FEndCap := Value;
end;

function TBZCustomCanvasPen.GetEndCap : TBZCapsMode;
begin
  Result := FEndCap;
end;

procedure TBZCustomCanvasPen.SetJoinStyle(Value : TBZJoinStyle);
begin
  if FJoinStyle = Value then Exit;
  FJoinStyle := Value;
end;

function TBZCustomCanvasPen.GetStartStyle : TBZJoinStyle;
begin
  Result := FJoinStyle;
end;

{%endregion}

{%region=====[ TBZCustomCanvasBrush ]=============================================}

constructor TBZCustomCanvasBrush.Create;
begin
  inherited Create;
  FStyle := bsClear;
  FColor := clrWhite;
  FBounds := TBZRectItemProperty.Create;
  FGradient := TBZGradientProperty.Create;
  FTexture := TBZBrushTextureProperty.create;
  FAutoBounds := True;
  FAutoCenter := True;
end;

destructor TBZCustomCanvasBrush.Destroy;
begin
  FreeAndNil(FTexture);
  FreeAndNil(FGradient);
  FreeAndNil(FBounds);
  inherited Destroy;
end;

procedure TBZCustomCanvasBrush.SetStyle(Value : TBZBrushStyle);
begin
  if FStyle = Value then Exit;
  FStyle := Value;
end;

function TBZCustomCanvasBrush.GetStyle : TBZBrushStyle;
begin
  Result := FStyle;
end;

procedure TBZCustomCanvasBrush.SetColor(Value : TBZColor);
begin
  if FColor = Value then Exit;
  FColor := Value;
end;

function TBZCustomCanvasBrush.GetColor : TBZColor;
begin
  Result := FColor;
end;

procedure TBZCustomCanvasBrush.SetPatternStyle(Value : TBZPatternStyle);
begin
  if FPatternStyle = Value then Exit;
  FPatternStyle := Value;
end;

function TBZCustomCanvasBrush.GetPatternStyle : TBZPatternStyle;
begin
  Result := FPatternStyle;
end;

procedure TBZCustomCanvasBrush.Assign(Source : TPersistent);
begin
  if (Source is TBZCustomCanvasBrush) then
  begin

  end;
  inherited Assign(Source);
end;

procedure TBZCustomCanvasBrush.InitGradient;
Var
  p1,p2 : TBZPoint;
Begin
  if FAutoCenter then
  begin
    FCenterPoint.Create(FBounds.Width div 2, FBounds.Height div 2);
    FCenterPoint.X :=  FCenterPoint.X + FBounds.Left;
    FCenterPoint.Y :=  FCenterPoint.Y + FBounds.Top;
  end;
  case FGradient.Kind of
    gkVertical    : FMaxDist := FBounds.Height;
    gkHorizontal  : FMaxDist := FBounds.Width;
    gkReflectVert : FMaxDist := FBounds.CenterY;
    gkReflectHorz : FMaxDist := FBounds.CenterX;
    gkFreeAngle :
    Begin
      P1.Create(FBounds.Left, FBounds.Top);
      P2.Create(FBounds.Right, FBounds.Bottom);
      FMaxDist := Round(P1.Distance(P2));
      //FMaxDist :=FBounds.;
      //FMaxDist := Round((System.Sqrt(Sqr(FBounds.Width)+ Sqr(FBounds.Height))));
      //if FBounds.Width > FBounds.Height then FMaxDist := FBounds.Width
      //else FMaxDist := FBounds.Height;
      if FMaxDist = 0 then FMaxDist:=1;
    end;
    gkFromTopLeft,gkFromTopRight:
    begin
      if FBounds.Width > FBounds.Height then FMaxDist := FBounds.Width
      else FMaxDist := FBounds.Height;
    end;
   gkRadial :
   begin
    // FMaxDist := Round((System.Sqrt(Sqr(FBounds.Width)+ Sqr(FBounds.Height))-FBounds.Left)*0.5);
    // FMaxDist := Round(System.Sqrt(Sqr(FCenterPoint.X)+ Sqr(FCenterPoint.Y)) * 0.5);
    // FMaxDist := Round(FCenterPoint.Length * 0.5);
     P1.Create(FBounds.Left, FBounds.Top);
     P2.Create(FBounds.Right, FBounds.Bottom);
     FMaxDist := Round(P1.Distance(P2) * 0.5);
   end;
   gkPolar:
   begin
     FMaxDist := 360;
   end;
   gkPyramid :
   begin
     if FBounds.Width > FBounds.Height then
     begin
       FMaxDist := FBounds.Width;
     end
     else
     begin
       FMaxDist := FBounds.Height;
     end;
   end;
  end;
  FMaxDist := System.abs(FMaxDist);
  //if FMaxDist = 0 then FMaxDist:=1;
end;

procedure TBZCustomCanvasBrush.InitGradient(rx,ry : Single);
Var
  p1,p2 : TBZPoint;
Begin
  if FAutoCenter then
  begin
    FCenterPoint.Create(FBounds.Width div 2, FBounds.Height div 2);
    FCenterPoint.X :=  FCenterPoint.X + FBounds.Left;
    FCenterPoint.Y :=  FCenterPoint.Y + FBounds.Top;
  end
  else
  begin
    FCenterPoint.Create(Round(rx), Round(ry));
  end;
  case FGradient.Kind of
    gkVertical    : FMaxDist := Round(ry + ry);
    gkHorizontal  : FMaxDist := Round(rx + rx);
    gkReflectVert : FMaxDist := FBounds.CenterY;
    gkReflectHorz : FMaxDist := FBounds.CenterX;
    gkFreeAngle :
    Begin
      P1.Create(FBounds.Left, FBounds.Top);
      P2.Create(FBounds.Right, FBounds.Bottom);
      FMaxDist := Abs(Round(P1.Distance(P2)));
      //FMaxDist :=FBounds.;
      //FMaxDist := Round((System.Sqrt(Sqr(FBounds.Width)+ Sqr(FBounds.Height))));
      //if FBounds.Width > FBounds.Height then FMaxDist := FBounds.Width
      //else FMaxDist := FBounds.Height;
      if FMaxDist = 0 then FMaxDist:=1;
    end;
    gkFromTopLeft,gkFromTopRight:
    begin
      if FBounds.Width > FBounds.Height then FMaxDist := FBounds.Width
      else FMaxDist := FBounds.Height;
    end;
   gkRadial :
   begin
     FMaxDist := Round(System.Sqrt(Sqr(rx)+ Sqr(ry)));
     //FMaxDist := Round(FCenterPoint.Length * 0.5);
   end;
   gkPolar:
   begin
     FMaxDist := 360;
   end;
   gkPyramid :
   begin
     if FBounds.Width > FBounds.Height then
     begin
       FMaxDist := Round(rx + rx);
     end
     else
     begin
       FMaxDist := Round(ry + ry);
     end;
   end;
  end;
  FMaxDist := System.abs(FMaxDist);
  //if FMaxDist = 0 then FMaxDist:=1;
end;

procedure TBZCustomCanvasBrush.InitTexture;
Var
  p1, p2 : TBZPoint;
begin
  FCenterPoint.Create(FBounds.Width div 2, FBounds.Height div 2);
  FCenterPoint.X :=  FCenterPoint.X + FBounds.Left;
  FCenterPoint.Y :=  FCenterPoint.Y + FBounds.Top;
  (*p1.Create(FBounds.Left, FBounds.Top);
  p2.Create(FBounds.Right, FBounds.Bottom);
  FMaxDist := Round((p1.Distance(p2)); *)
  if FBounds.Width > FBounds.Height then FMaxDist := FBounds.Height else FMaxDist := FBounds.Width;
end;

function TBZCustomCanvasBrush.GetMaxDist : Integer;
begin
  Result := FMaxDist;
end;

{%endregion}

{%region=====[ TBZCustomCanvas ]==================================================}

procedure TBZCustomCanvas.SetCanvasDrawMode(Value : TBZCanvasDrawModeProperty);
begin
  if FDrawMode = Value then Exit;
  FDrawMode := Value;
end;

function TBZCustomCanvas.GetCanvasDrawMode : TBZCanvasDrawModeProperty;
begin
  Result := FDrawMode;
end;

constructor TBZCustomCanvas.Create;
begin
  inherited Create;
  FSurfaceWidth := 100;
  FSurfaceHeight := 100;
  FSurfaceViewPort.Create(0,0,100,100);
  FAntiAlias := False;
  FDrawMode := TBZCanvasDrawModeProperty.Create;
  FFont := nil;
end;

constructor TBZCustomCanvas.Create(const AWidth, AHeight : integer);
begin
  Create;
  FSurfaceWidth := AWidth;
  FSurfaceHeight := AHeight;
  FSurfaceViewPort.Create(0,0, AWidth-1, AHeight-1);
end;

destructor TBZCustomCanvas.Destroy;
begin
  FreeAndNil(FDrawMode);
  inherited Destroy;
end;

function TBZCustomCanvas.CreateDefaultFont : TBZCustomFont;
begin
  result := Internal_CreateDefaultFont;
  if not assigned(result) then
    raise Exception.Create('Impossible d''initialiser la police de caractères');
end;

function TBZCustomCanvas.CreateFont : TBZCustomFont;
begin
  result := Internal_CreateDefaultFont;
end;

//function TFPCustomCanvas.AllowFont (AFont : TFPCustomFont) : boolean;
//begin
//  if AFont is TFPCustomDrawFont then
//    result := true
//  else
//    result := DoAllowFont (AFont);
//end;

procedure TBZCustomCanvas.SetFont (AValue:TBZCustomFont);
begin
  if (AValue <> FFont) then //and AllowFont(AValue) then
    begin
      FFont.Assign(AValue)
    end;
end;

function TBZCustomCanvas.GetFont : TBZCustomFont;
begin
  if assigned(FFont) then
    result := FFont
  else
    result := FDefaultFont;
end;

procedure TBZCustomCanvas.TextOut (x,y:integer;text:string);
begin
  if Font is TBZCustomFontDrawer then
    TBZCustomFontDrawer(Font).DrawText(x,y, text)
  else
    Internal_TextOut (x,y, text);
end;

procedure TBZCustomCanvas.GetTextSize (text:string; var w,h:integer);
begin
  if Font is TBZCustomFontDrawer then
    TBZCustomFontDrawer(Font).GetTextSize (text, w, h)
  else
    Internal_GetTextSize (Text, w, h);
end;

function TBZCustomCanvas.GetTextHeight (text:string) : integer;
begin
  Result := TextHeight(Text);
end;

function TBZCustomCanvas.GetTextWidth (text:string) : integer;
begin
  Result := TextWidth(Text);
end;

//function TBZCustomCanvas.TextExtent(const Text: string): TSize;
//begin
//  GetTextSize(Text, Result.cx, Result.cy);
//end;

function TBZCustomCanvas.TextHeight(const Text: string): Integer;
begin
  if Font is TBZCustomFontDrawer then
    result := TBZCustomFontDrawer(Font).GetTextHeight (text)
  else
    result := Internal_GetTextHeight (Text);
end;

function TBZCustomCanvas.TextWidth(const Text: string): Integer;
begin
  if Font is TBZCustomFontDrawer then
    result := TBZCustomFontDrawer(Font).GetTextWidth (text)
  else
    result := Internal_GetTextWidth (Text);
end;

procedure TBZCustomCanvas.TextOut (x,y:integer;text:unicodestring);
begin
  if Font is TBZCustomFontDrawer then
    TBZCustomFontDrawer(Font).DrawText(x,y, text)
  else
    Internal_TextOut (x,y, text);
end;

procedure TBZCustomCanvas.GetTextSize (text:unicodestring; var w,h:integer);
begin
  if Font is TBZCustomFontDrawer then
    TBZCustomFontDrawer(Font).GetTextSize (text, w, h)
  else
    Internal_GetTextSize (Text, w, h);
end;

function TBZCustomCanvas.GetTextHeight (text:unicodestring) : integer;
begin
  Result := TextHeight(Text);
end;

function TBZCustomCanvas.GetTextWidth (text:unicodestring) : integer;
begin
  Result := TextWidth(Text);
end;

//function TBZCustomCanvas.TextExtent(const Text: unicodestring): TSize;
//begin
//  GetTextSize(Text, Result.cx, Result.cy);
//end;

function TBZCustomCanvas.TextHeight(const Text: unicodestring): Integer;
begin
  if Font is TBZCustomFontDrawer then
    result := TBZCustomFontDrawer(Font).GetTextHeight (text)
  else
    result := Internal_GetTextHeight (Text);
end;

function TBZCustomCanvas.TextWidth(const Text: unicodestring): Integer;
begin
  if Font is TBZCustomFontDrawer then
    result := TBZCustomFontDrawer(Font).GetTextWidth (text)
  else
    result := Internal_GetTextWidth (Text);
end;

procedure TBZCustomCanvas.Internal_TextOut (x,y:integer;text:unicodestring);
begin
  Internal_TextOut(x,y,string(text));
end;

procedure TBZCustomCanvas.Internal_GetTextSize (text:unicodestring; var w,h:integer);
begin
  Internal_GetTextSize(String(Text),w,h);
end;

function TBZCustomCanvas.Internal_GetTextHeight (text:unicodestring) : integer;
begin
  Result:=Internal_GetTextHeight(String(text));
end;

function TBZCustomCanvas.Internal_GetTextWidth (text:unicodestring) : integer;
begin
  Result:=Internal_GetTextWidth(String(text));
end;


procedure TBZCustomCanvas.BeginUpdate;
begin

end;

procedure TBZCustomCanvas.EndUpdate;
begin

end;

Procedure TBZCustomCanvas.VLine(x1, y1, y2 : Single);
begin
  if y1 > y2 Then Swap(y1,y2);
  if FPen.Style<>ssClear then DrawVLine(x1,y1,y2);
end;

Procedure TBZCustomCanvas.VLine(PtFrom : TBZFloatPoint; h : Single);
begin
  VLine(PtFrom.x,PtFrom.y,PtFrom.y+h);
end;

Procedure TBZCustomCanvas.HLine(x1, y1, x2 : Single);
begin
  If x1 > x2 Then Swap(x1,x2);
  if FPen.Style<>ssClear then DrawHLine(x1,y1,x2);
end;

Procedure TBZCustomCanvas.HLine(PtFrom : TBZFloatPoint; L : Single);
begin
  HLine(PtFrom.x,PtFrom.y,PtFrom.x+L);
end;

procedure TBZCustomCanvas.Line(x1, y1, x2, y2 : Single);
begin
  if FPen.Style<>ssClear then DrawLine(x1, y1, x2, y2);
end;

procedure TBZCustomCanvas.Line(PtFrom, PtTo : TBZFloatPoint);
begin
  Line(PtFrom.x, PtFrom.y, PtTo.x, PtTo.y);
end;

Procedure TBZCustomCanvas.PolyLine(pnts : TBZArrayOfFloatPoints; Const Closed : Boolean);
begin
  if FPen.Style<>ssClear then DrawPolyLine(Pnts, Closed);
end;

Procedure TBZCustomCanvas.Triangle(x1, y1, x2, y2, x3, y3 : Single);
begin
  if FBrush.Style<>bsClear then FillTriangle(x1, y1, x2, y2, x3, y3);
  if FPen.Style<>ssClear then DrawTriangle(x1, y1, x2, y2, x3, y3);
end;

Procedure TBZCustomCanvas.Triangle(Pt1, Pt2, Pt3 : TBZFloatPoint);
begin
  Triangle(Pt1.x, Pt1.y, Pt2.x, Pt2.y, Pt3.x, Pt3.y);
end;

Procedure TBZCustomCanvas.Polygon(Const Pnts : TBZArrayOfFloatPoints);
begin
  if FBrush.Style<>bsClear then FillPolygon(Pnts);
  if FPen.Style<>ssClear then DrawPolygon(Pnts);
end;

Procedure TBZCustomCanvas.Rectangle(x1, y1, x2, y2 : Single);
begin
  if x1 > x2 then swap(x1,x2);
  if y1 > y2 then swap(y1,y2);
  if FBrush.Style<>bsClear then
  begin
    if (FBrush.Style = bsGradient) or (FBrush.Style = bsTexture) then
    begin
      if FBrush.AutoBounds then
      begin
        With FBrush.Bounds do
        begin
          Left := Round(x1);
          Top := Round(y1);
          Right := Round(x2);
          Bottom := Round(y2);
        end;
      end;
    end;
    if (FBrush.Style = bsGradient) then FBrush.InitGradient
    else if (FBrush.Style = bsTexture) then FBrush.InitTexture;

    FillRectangle(x1, y1, x2, y2);
  end;
  if FPen.Style<>ssClear then DrawRectangle(x1, y1, x2, y2);
end;

Procedure TBZCustomCanvas.Rectangle(ARect : TBZFloatRect);
begin
  Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
end;

Procedure TBZCustomCanvas.Rectangle(ARect : TBZRect);
begin
  Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
end;

Procedure TBZCustomCanvas.Rectangle(ptl, pbr : TBZFloatPoint);
Var
  ARect : TBZFloatRect;
begin
  ARect.Create(ptl,pbr);
  Rectangle(ARect);
end;

Procedure TBZCustomCanvas.RoundedRect(x1, y1, x2, y2, Rx, Ry : Single);
begin
  if x1 > x2 then swap(x1,x2);
  if y1 > y2 then swap(y1,y2);
  if FBrush.Style<>bsClear then FillRoundedRectangle(x1, y1, x2, y2, Rx, Ry);
  if FPen.Style<>ssClear then DrawRoundedRect(x1, y1, x2, y2, Rx, Ry);
end;

Procedure TBZCustomCanvas.RoundedRect(ARect : TBZFloatRect; Rx, Ry : Single);
begin
  RoundedRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, Rx, Ry);
end;

Procedure TBZCustomCanvas.Circle(cx, cy, r : Single);
begin
  if FBrush.Style<>bsClear then
  begin
    begin
      if FBrush.AutoBounds then
      begin
        With FBrush.Bounds do
        begin
          Left := Round(cx-r);
          Top := Round(cy-r);
          Right := Round(cx+r);
          Bottom := Round(cy+r);
        end;
      end;
    end;
    if (FBrush.Style = bsGradient) then FBrush.InitGradient(r,r)
    else if (FBrush.Style = bsTexture) then FBrush.InitTexture;
    FillCircle(cx, cy, r);
  end;
  if FPen.Style<>ssClear then DrawCircle(cx, cy, r);
end;

Procedure TBZCustomCanvas.Circle(CP : TBZFloatPoint; r : Single);
begin
  Circle(CP.X, CP.Y,r);
end;

Procedure TBZCustomCanvas.Ellipse(cx, cy, Rx, Ry : Single);
begin
  if FBrush.Style<>bsClear then
  begin
    if (FBrush.Style = bsGradient) or (FBrush.Style = bsTexture) then
    begin
      if FBrush.AutoBounds then
      begin
        With FBrush.Bounds do
        begin
          Left := Round(cx-rx);
          Top := Round(cy-ry);
          Right := Round(cx+rx);
          Bottom := Round(cy+ry);
        end;
      end;
    end;
    if (FBrush.Style = bsGradient) then FBrush.InitGradient(rx,ry)
    else if (FBrush.Style = bsTexture) then FBrush.InitTexture;
    FillEllipse(cx, cy, Rx, Ry);
  end;
  if FPen.Style<>ssClear then DrawEllipse(cx, cy, Rx, Ry);
end;

Procedure TBZCustomCanvas.Ellipse(pc : TBZFloatPoint; Rx, Ry : Single);
begin
  Ellipse(pc.x, pc.y, Rx, Ry);
end;

Procedure TBZCustomCanvas.Ellipse(ptl, pbr : TBZFloatPoint);
Var
  cx, cy, rx, ry : Single;
  Diff, pc : TBZFloatPoint;
begin
  Diff := (pbr - ptl) * 0.5;
  pc := ptl + Diff;
  Ellipse(pc, Diff.x, Diff.y);
end;

Procedure TBZCustomCanvas.Arc(cx, cy, rx, ry, StartAngle, EndAngle : Single);
begin
  if FPen.Style<>ssClear then DrawArc(cx, cy, rx, ry, StartAngle, EndAngle);
end;

procedure TBZCustomCanvas.BezierCurve(P1, C1, P2 : TBZFloatPoint; Const nbSteps : Integer);
Var
  BezierCurveTool : TBZ2DQuadraticBezierCurve;
  PolyLinePoints : TBZArrayOfFloatPoints;
begin
  BezierCurveTool := CreateQuadraticBezierCurve(P1,C1,P2);
  PolyLinePoints := BezierCurveTool.ComputePolyLinePoints(nbSteps);
  PolyLine(PolyLinePoints);
  FreeAndNil(PolyLinePoints);
  FreeAndNil(BezierCurveTool);
end;

procedure TBZCustomCanvas.BezierCurve(P1, C1, C2, P2 : TBZFloatPoint; Const nbSteps : Integer);
Var
  BezierCurveTool : TBZ2DCubicBezierCurve;
  PolyLinePoints : TBZArrayOfFloatPoints;
begin
  BezierCurveTool := CreateCubicBezierCurve(P1,C1,C2, P2);
  PolyLinePoints := BezierCurveTool.ComputePolyLinePoints(nbSteps);
  PolyLine(PolyLinePoints);
  FreeAndNil(PolyLinePoints);
  FreeAndNil(BezierCurveTool);
end;

procedure TBZCustomCanvas.BezierCurve(P1x, P1y, C1x, C1Y, C2x, C2Y, P2x, P2y : Single; Const nbSteps : Integer);
Var
  P1, C1, C2, P2 : TBZFloatPoint;
begin
  P1.Create(P1x, P1y);
  C1.Create(C1x, C1y);
  C2.Create(C2x, C2y);
  P2.Create(P2x, P2y);
  BezierCurve(P1, C1, C2, P2, nbSteps);
end;

procedure TBZCustomCanvas.BezierCurve(P1x, P1y, C1x, C1Y, P2x, P2y : Single; Const nbSteps : Integer);
Var
  P1, C1, P2 : TBZFloatPoint;
begin
  P1.Create(P1x, P1y);
  C1.Create(C1x, C1y);
  P2.Create(P2x, P2y);
  BezierCurve(P1,C1,P2, nbSteps);
end;


procedure TBZCustomCanvas.MoveTo(x, y : Single);
begin
  FCurrentPos.Create(x,y);
end;

procedure TBZCustomCanvas.MoveTo(pt : TBZFloatPoint);
begin
  FCurrentPos := Pt;
end;

Procedure TBZCustomCanvas.TurnTo(A : Single);
begin
 FCurrentAngle := FCurrentAngle + DegToRadian(A);
end;

procedure TBZCustomCanvas.LineTo(x, y : Single);
begin
  Line(FCurrentPos.x, FCurrentPos.y, x, y);
  FCurrentPos.X := x;
  FCurrentPos.Y := y
end;

procedure TBZCustomCanvas.LineTo(PtTo : TBZFloatPoint);
begin
  Line(FCurrentPos.x, FCurrentPos.y, PtTo.x, PtTo.y);
  FCurrentPos := PtTo;
end;

Procedure TBZCustomCanvas.TraceTo(L : Single);
Var
  dX, dY: Integer;
  s,c : Single;
  {$CODEALIGN VARMIN=16}
  d, p1 : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
Begin
  S := System.Sin(FCurrentAngle);
  C := System.Cos(FCurrentAngle);
  p1.Create(c,s);
  //p2.Create(FCurrentPos.X,FCurrentPos.Y);
  D.Create(L,L);
  D:=(p1 * L) + FCurrentPos;
  //dp := d.Round;
  LineTo(D);
  //FCurrentPos := D
end;

{%endregion}

Initialization
  SolidPenStrokePattern := nil;
  // Setlength(ClearPenStrokePattern,1);
  // ClearStrokePattern[0] := 0;
  DashPenStrokePattern := CreateBZStrokePattern(3, 1, 0, 0, 0, 0, 0, 0);        // ___ ___ ___ ___ ___ ___ ___
  DotPenStrokePattern := CreateBZStrokePattern(1, 1, 0, 0, 0, 0, 0, 0);         // _ _ _ _ _ _ _ _ _ _ _ _ _ _
  DashDotPenStrokePattern := CreateBZStrokePattern(3, 1, 1, 1, 0, 0, 0, 0);     // ___ _ ___ _ ___ _ ___ _ ___
  DashDotDotPenStrokePattern := CreateBZStrokePattern(3, 1, 1, 1, 1, 1, 0, 0);  // ___ _ _ ___ _ _ ___ _ _ ___

finalization

end.

