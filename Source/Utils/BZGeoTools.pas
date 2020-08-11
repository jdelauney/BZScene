(*
  @abstract(Contient des classes d'aide à la manipulation de primitives géométrique
  tel que les lignes, les polylignes, les polygones, les courbes de bezier, les courbes BSpline.)

  --------------------------------------------------------------------------------

  @created(2016-11-16)
  @author(J.Delauney (BeanzMaster))
  Historique :
  @unorderedList(
    @item(10/07/2019 : Creation  )
  )

  --------------------------------------------------------------------------------

  @bold(Notes) :

    TODO : Les classes de cette unité seraient elles mieux si ce sont des Enregistrements Avancés ????? @br
    A voir. Mais dans ce cas on perdrait la notfication des changements de valeur.

    TODO : CETTE UNITE EST A COMPLETER POUR ETRE PLEINEMENT FONCTIONNELLE

  --------------------------------------------------------------------------------

  @bold(Dependances) : BZClasses, BZGraphic, BZMath, BZVectorMath

  --------------------------------------------------------------------------------

  @bold(Credits :)
   @unorderedList(
     @item ()
     @item(J.Delauney (BeanzMaster))
   )

  ------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  -------------------------------------------------------------------------------- *)
unit BZGeoTools;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils, Dialogs, Math,
  BZClasses, BZArrayClasses, BZGraphic, BZMath, BZVectorMath;

Type
  TBZ2DLineTool = Class;
  TBZ2DCircleTool = Class;
  //TBZ2DArcTool = Class;
  TBZ2DEdgeList = Class;
  TBZ2DPolyLineTool = Class;
  TBZ2DPolygonTool  = Class;
  TBZ2DPolygonList = Class;
  TBZ2DQuadraticBezierCurve = class;
  TBZ2DCubicBezierCurve = class;

  { Définit le type d'intersection de deux lignes }
  TBZ2DLineIntersecType = (ilNone, ilIntersect, ilOverlap);

  TBZCurveType = (ctBezier, ctSmoothBezier, ctBezierSpline, ctUniformSpline, ctCatmullRomSpline);

  TBZRasterItem = Packed Record
    xStart: Integer;
    xEnd:   Integer;
    xStartf,
    xEndf : Single;
  End;

  //PBZRasterItem = TBZRasterHLineRec;
  TBZRasterItems = Array Of TBZRasterItem;
  TBZRastersList = Array Of TBZRasterItems;


  { TBZFloatPointsContainer }
  TBZFloatPointsContainer = class(TBZUpdateAbleObject)
  private
    FAutoFree : Boolean;
    FForceFree : Boolean;
  protected
    FPoints : TBZArrayOfFloatPoints;

    Function GetCount: Integer;
    Procedure SetCount(AValue: Integer);
    Function GetPoint(Index: Integer): TBZFloatPoint;
    Procedure SetPoint(Index: Integer; APoint: TBZFloatPoint);
    function GetPointsList : TBZArrayOfFloatPoints;
    procedure SetPointsList(Value : TBZArrayOfFloatPoints);
  public
    Constructor Create(Const AutoFreePoints : Boolean = False); overload;
    Constructor Create(aPoints : TBZArrayOfFloatPoints; Const AutoFreePoints : Boolean = False); overload;
    Constructor Create(ReservedPoints : Integer); overload;
    Destructor Destroy; override;

    Procedure NotifyChange(Sender: TObject); override;

    { Assigne une liste de points }
    Procedure AssignPoints(Const aPoints: TBZArrayofFloatPoints);


    //procedure Translate;
    //procedure Rotate;
    //procedure Scale;
    //procedure Shear

    { Retourne le nombre de point }
    Property Count: Integer read GetCount write SetCount;
    { Retourne la liste des points }
    Property Points[Index: Integer]: TBZFloatPoint read GetPoint write SetPoint;
    { Retourne la liste des points }
    property PointsList : TBZArrayOfFloatPoints read GetPointsList write SetPointsList;
    { Libère automatiquement la liste des points }
    property AutoFree : Boolean read FAutoFree write FAutoFree;
  end;

  { TBZ2DLineTool : Classe d'aide à la manipulation et au calcul de formules d'une ligne dans un repère cartésien }
  TBZ2DLineTool = Class(TBZUpdateAbleObject)
  Private
    FStartPoint, FEndPoint   : TBZFloatPoint;
    FTagInt : Integer;

    //procedure SetStartPoint(Const Value : TBZFloatPoint);
    //procedure SetEndPoint(Const Value : TBZFloatPoint);
  public
    { Creation de la classe TBZ2DLineTool }
    constructor Create; override;
    //constructor Create(Const aStartPoint, aEndPoint : TBZFloatPoint); overload;

    procedure Assign(source: TPersistent); override;

    { Définit les point de début et de fin du segment de ligne }
    procedure SetLineFromPoints(Const aStartPoint, aEndPoint : TBZFloatPoint);
    { Définit le point de début, la direction et sa longueur de la ligne }
    procedure SetInfiniteLine(aStartPoint, aDirection : TBZFloatPoint; aDistance : Single);
    { Clone la ligne }
    function Clone : TBZ2DLineTool;
    { Vérifie si la ligne croise une autre ligne.
      Retourne :
        - ilNone si pas d'intersection
        - ilIntersect si les lignes se croise en un seul point
      Si il y a intersection  la position du point d'intersection est retourné dans "IntersectPoint" }
    function GetInfiniteIntersectPoint(aLine : TBZ2DLineTool; out IntersectPoint : TBZFloatPoint) :  TBZ2DLineIntersecType;
    { Vérifie si le segment de ligne croise un autre segment de ligne.
      Retourne :
        - ilNone si pas d'intersection
        - ilIntersect si les segments de ligne se croise en un seul point
        - ilOverlap si les deux segment de ligne se chevauche
      Si il y a intersection  la position du point d'intersection est retourné dans "IntersectPoint"
      Si les deux segment de ligne se chevauche alors les 2 points d'intersection sont retourné respectivement dans "IntersectPoint" et "IntersectEndPoint" }
    function GetIntersectPoint(aLine : TBZ2DLineTool; out IntersectPoint, IntersectEndPoint : TBZFloatPoint) : TBZ2DLineIntersecType;

    { Retourne @True si le segment de ligne croise un autre segment de ligne. }
    function IntersectWithLine(aLine : TBZ2DLineTool) : Boolean;

    { Retourne @True si un des points du segment de la ligne se trouve sur ou croise le cercle }
    function IntersectSegmentWithCircle(aCenterPoint : TBZFloatPoint; aRadius : Single) : Boolean;
    { Retourne @True si un des points de la ligne se trouve sur ou croise le cercle et renvois la position des points d'intersection }
    function GetInfiniteIntersectPointsWithCircle(aCenterPoint : TBZFloatPoint; aRadius : Single; Out IntersectPointA, IntersectPointB : TBZFloatPoint) : Boolean;
    { Retourne @True si un des points du segment de ligne se trouve sur ou croise le rectangle et renvois la position des points d'intersection }
    function GetSegmentIntersectPointsWithRect(aRect : TBZFloatRect; Out IntersectPointA, IntersectPointB : TBZFloatPoint) : Boolean;
    { Vérifie si le point "aPoint" se trouve sur le segment de ligne }
    function IsPointOnLine(aPoint : TBZFloatPoint) : Boolean;
    { Verifie si la ligne "aLine" est perpendiculaire avec la ligne }
    function IsPerpendicular(aLine : TBZ2DLineTool) : Boolean;
    { Verifie si la ligne "aLine" est parallel avec la ligne }
    function IsParallel(aLine : TBZ2DLineTool) : Boolean;
    { Verifie si la ligne est Vertical }
    function IsVertical : Boolean;
    { Verifie si la ligne est Horizontal }
    function IsHorizontal : Boolean;
    { Retourne les points perpendiculaire Gauche "LeftPoint" et Droit "RightPoint" a une distance "aDistance" du point de début du segment "StartPoint" }
    procedure GetPerpendicularPointsAtStartPoint(aDistance : Single; out  LeftPoint, RightPoint : TBZFloatPoint);
    { Retourne les points perpendiculaire Gauche "LeftPoint" et Droit "RightPoint" a une distance "aDistance" du point de fin du segment "StartPoint" }
    procedure GetPerpendicularPointsAtEndPoint(aDistance : Single; out LeftPoint, RightPoint  : TBZFloatPoint);
    { Retourne la position du point perpendiculaire à la ligne par rapport au point "aPoint" }
    function  GetPerpendicularPointFromPoint(aPoint : TBZFloatPoint)  : TBZFloatPoint;
    { Verifie que le point "aPoint" se trouve sur la ligne et les points perpendiculaire Gauche "LeftPoint" et Droit "RightPoint" a une distance "aDistance" }
    function GetPerpendicularPointsAtPoint(aPoint : TBZFloatPoint; aDistance : Single; out LeftPoint, RightPoint  : TBZFloatPoint) : Boolean;
    { Retourne la direction de la ligne }
    function GetDirection : TBZFloatPoint;
    { Retourne la direction normalisé de la ligne }
    function GetNormalizeDirection : TBZFloatPoint;
    { Retourne la position du point médian du segment de ligne }
    function GetMidPoint : TBZFloatPoint;
    { Retoure la position du point sur la ligne se trouvant à une distance "aDistance" depuis le point de début du segment de ligne "StartPoint" }
    function GetPointOnLineFromStartPoint(aDistance : Single) : TBZFloatPoint;
    { Retoure la position du point sur la ligne se trouvant à une distance "aDistance" depuis le point de fin du segment de ligne "EndPoint" }
    function GetPointOnLineFromEndPoint(aDistance : Single) : TBZFloatPoint;
    { Retourne le facteur de la pente (slope) de la ligne }
    function getSlope : Single;
    { Retourne la réciprocité inverse du facteur de la pente (InvSlope) de la ligne }
    function getReciprocalInvSlope : Single;
    { Retourne le "Y-Intercept" de la ligne }
    function GetYIntercept : Single;
    { Retourne la longueur du segment de ligne }
    function GetLength : Single;
    { Retourne les normales gauche et droite de la ligne }
    procedure GetNormal(Out LeftNormal, RightNormal : TBZFloatPoint);
    { Retourne la distance la plus proche de la ligne au point "aPoint" }
    function DistanceLineToPoint(aPoint : TBZFloatPoint): Single; overload;
    { Retourne la distance la plus proche de la ligne au point "aPoint" et retourne le vecteur directeur du point vers la ligne }
    function DistanceLineToPoint(aPoint : TBZFloatPoint; out aDir : TBZFloatPoint ): Single; overload;
    { Retourne la distance la plus proche du segment de ligne au point "aPoint" }
    function DistanceSegmentToPoint(aPoint : TBZFloatPoint): Single; overload;
    { Retourne la distance la plus proche du segment de ligne au point "aPoint" et retourne le vecteur directeur du point vers la ligne }
    function DistanceSegmentToPoint(aPoint : TBZFloatPoint; out aDir : TBZFloatPoint ): Single; overload;
    { Retourne la distance entre le point perpendiculaire au segment de ligne }
    function PerpendicularDistance(aPoint : TBZFloatPoint) : Single;

    { Changement d'echelle du segment de ligne en fonction de "Factor" }
    procedure Scale(Factor : Single);
    { Déplacement du segment de ligne en fonction de "dx","dy" }
    procedure Translate(dx,dy : Single); overload;
    { Déplacement du segment de ligne en fonction du vecteur "aVector" }
    procedure Translate(aVector : TBZFloatPoint); overload;
    { Rotation de la ligne de "Angle" par rapport au centre "aCenterPoint" }
    procedure Rotate(anAngle : Single; aCenterPoint : TBZFloatPoint); overload;
    { Rotation de la ligne sur elle même de "Angle"  }
    procedure Rotate(anAngle : Single); overload;

    function  BuildStroke(Width : Integer; StrokeMode : TBZStrokeMode; StartCap, EndCap : TBZCapsMode) : TBZArrayOfFloatPoints;
    // function BuildPattern

    { Position du point de début du segment de ligne }
    property StartPoint : TBZFloatPoint read FStartPoint write FStartPoint;
    { Position du point de fin du segment de ligne }
    property EndPoint : TBZFloatPoint read FEndPoint write FEndPoint;
    { Tag }
    property Tag : Integer read FTagInt write FTagInt;
  end;

  { Stocke un liste de segment }
  TBZ2DEdgeList = Class(TBZPersistentObjectList)
  private
  Protected
    function CompareEdge(item1, item2: TObject): Integer;

    Function GetEdgeItem(index: Integer): TBZ2DLineTool;
    Procedure SetEdgeItem(index: Integer; val: TBZ2DLineTool);
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;

    { Assigne une autre liste de segment }
    procedure Assign(source: TPersistent); override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    { Efface la liste de segment}
    Procedure Clear; Override;
    { Ajoute un segment à la liste }
    function AddEdge(Edge : TBZ2DLineTool) : Integer;
    { Trie la liste des segment par ordre croissant par rapport à la position Y puis en X }
    procedure Sort; overload;

    { Acces aux  éléments de la liste }
    Property Items[Index: Integer]: TBZ2DLineTool read GetEdgeItem write setEdgeItem;

  end;

  { Classe pour la manipulation de cercle }
  TBZ2DCircleTool = Class(TBZUpdateAbleObject)
  private
    FRadius : Single;
    {$CODEALIGN RECORDMIN=16}
    FCenter : TBZFloatPoint;
    {$CODEALIGN RECORDMIN=4}
  public
    function GetPointOnCircumference(Angle : Single) : TBZFloatPoint;
    function IsPointInCircle(pt : TBZFloatPoint) : Boolean;
    function IsPointOnCircle(pt : TBZFloatPoint) : Boolean;
    function GetTangentLineAtAngle(Angle, TangentLineLength : Single) : TBZ2DLineTool;
    function GetArea : Single;
    function GetPerimeter : Single;
    function LineIsChord(aLine : TBZ2DLineTool) : Boolean;
    function LineIsSecant(aLine : TBZ2DLineTool) : Boolean;
    //function LineIsTangent(aLine : TBZ2DLineTool) : Boolean;
    // function IntersectCircle(aCircle : TBZ2DCircleTool) : Boolean;

    //function Build : TBZArrayofPoints;

    property Radius : Single read FRadius write FRadius;
    property Center : TBZFloatPoint read FCenter write FCenter;
  end;

  { Classe pour la manipuation et la gestion d'une polyline }
  TBZ2DPolyLineTool = Class(TBZFloatPointsContainer)
  private
    FStrokeMode : TBZStrokeMode;
    FInnerJoinType, FOuterJoinType : TBZJoinStyle;
    FStartCap, FEndCap : TBZCapsMode;
    FStrokeWidth : Word;
    FCount : Integer;
    FClosePath : Boolean;

  protected
    procedure SimplifyDist(InPts : TBZArrayOfFloatPoints; Tolerance : Single; out OutPts : TBZArrayOfFloatPoints);
    procedure Internal_RamerDouglasPeucker(InPts : TBZArrayOfFloatPoints; Tolerance : Single; StartIndex, EndIndex : Integer; Var KeepPoints : Array of Boolean);
    procedure SimplifyRamerDouglasPeucker(InPts : TBZArrayOfFloatPoints; Tolerance : Single; out OutPts : TBZArrayOfFloatPoints);
  public

    constructor Create(aPoints : TBZArrayOfFloatPoints);
   // constructor Create(aPoints : TBZArrayOfFloatPoints; Const AutoClosePath : Boolean = false);

   //procedure AssignPoints(aPoints : TBZArrayOfFloatPoints);

    function BuildStroke : TBZArrayOfFloatPoints;
    //function BuildStrokeToPolyPolygon : TBZ2DPolygonList;
    { Construit la liste des segments dans "EdgeList" et retourne le nombre de segment }
    function GetEdgeList(Var EdgeList : TBZ2DEdgeList) : Integer;
    //function GetLength : Single;

    function GetBounds : TBZFloatRect;
    //procedure BuildRectangle;
    //procedure BuildTriangle
    //procedure BuildArc;
    //procedure BuildCircle;
    //procedure BuildCurve;
    //procedure BuildStar;
    // ToPolygone

    { Retourne un tableau de coordonnées, de la poly ligne simplifiée en utilisant un algorithme simplification simple par distance @br
      combiner à l'algorithme de Ramer Douglas Peucker (https://fr.wikipedia.org/wiki/Algorithme_de_Douglas-Peucker) }
    function SimplifyTo(Const Tolerance : Single = 1.0) : TBZArrayOfFloatPoints;
    { Simplification de la poly ligne simplifiée en utilisant un algorithme simplification simple par distancede combiner à l'algorithme de Ramer Douglas Peucker }
    procedure Simplify(Const Tolerance : Single = 1.0);

    property Path : TBZArrayOfFloatPoints read FPoints write FPoints;
    property StrokeMode : TBZStrokeMode read FStrokeMode write FStrokeMode;
    property InnerJoinType  : TBZJoinStyle read FInnerJoinType write FInnerJoinType;
    property OuterJoinType : TBZJoinStyle read FOuterJoinType write FOuterJoinType;
    property StartCap  : TBZCapsMode read FStartCap write FStartCap;
    property EndCap : TBZCapsMode read FEndCap write FEndCap;
    property StrokeWidth : Word read FStrokeWidth write FStrokeWidth;
    property ClosePath : Boolean read FClosePath write FClosePath;
  end;

  { Type de polygone }
  TBZ2DPolygonType = (ptConvex, ptConcave, ptComplex);

  { @abstract(Classe pour la gestion et la manipulation de polygone.)

    @unorderedlist(
      @item(http://geomalgorithms.com/a09-_intersect-3.html#simple_Polygon())
      @item(http://www.jagregory.com/abrash-black-book/#chapter-38-the-polygon-primeval)
      @item(http://alienryderflex.com/polygon_fill/)
      @item(https://www.scratchapixel.com/lessons/3d-basic-rendering/rasterization-practical-implementation)
      @item(https://cs.stackexchange.com/questions/2197/how-do-i-test-if-a-polygon-is-monotone-with-respect-to-an-arbitrary-line)
      @item(https://stackoverflow.com/questions/471962/how-do-i-efficiently-determine-if-a-polygon-is-convex-non-convex-or-complex)
      @item(http://www.tutorialspoint.com/computer_graphics/polygon_filling_algorithm.htm)
      @item(http://paulbourke.net/geometry/polygonmesh/)
    )
  }
  TBZ2DPolygonTool = Class(TBZFloatPointsContainer)
  private
    FRasters : TBZRastersList;
    FRastersNeedUpdate : Boolean;

    FStartY: Integer;
    FFloatStartY : Single;
    FBounds : TBZFloatRect;

    FRastersLineCount : Integer;

    Function getRastersList :TBZRastersList;
    function GetBounds : TBZFloatRect;
  protected
    function ComputeBounds : TBZFloatRect;
    procedure ComputeRasters;

  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure NotifyChange(Sender: TObject); override;

    { Construit la liste des segments dans "EdgeList" et retourne le nombre de segment }
    function GetEdgeList(Var EdgeList : TBZ2DEdgeList) : Integer;
    { Retourne les informations de rasterization }
    Function GetRastersAtLine(Const Y: Integer):  TBZRasterItems;
    { Retourne TRUE si le point se trouve à l'intérieur du polygone }
    Function PointInside(Const pt: TBZFloatPoint): Boolean;
    { Retourne le type de polygone }
    Function GetPolygonType: TBZ2DPolygonType;
    { Le polygone est-il convex ? }
    Function IsConvex: Boolean;
    { Le polygone est-il complexe ? }
    Function IsComplex: Boolean;
    { Le polygone est-il concave ? }
    Function IsConcave: Boolean;
    { Retourne le centre du polygone }
    function GetCenter : TBZFloatPoint;
    { Retourne @True si les points tournent dans le sens horaire. @False dans le sens anti-horaire }
    function IsCounterClockWise : Boolean;
    { Agrandit ou réduit le polygone en déplaçant les points en fonction de "Ratio" }
    function OffsetPolygon(Ratio:Single): TBZArrayOfFloatPoints;
    { Retourne @True si une des lignes du polygone croise celle d'un autre polygone }
    function IntersectWithPolygon(const P2: TBZ2DPolygonTool): Boolean;
    { Retourne @True si le polygone est monotone par rapport à une ligne verticale }
    Function IsMonotoneVertical: Boolean;
    { Retourne le périmètre du polygone.
      Note : cette méthode peut retourner des résultats faux dans des cas spéciaux.
      Par exemple quand le polygone a des côtés colinéaires qui se chevauchent.
      Cela peut être évité en filtrant le polygone pour détecter les défauts avant d'appeler cette méthode }
    function GetPerimeter : Single;
    { Retourne l'aire du polygone }
    function GetArea : Single;

    { Convertie un polygone complexe en un polygone simple (convexe ou concave) }
    //procedure ConvertComplexToSimple; overload;
    //procedure ConvertComplexToSimple(Var NewPolygon : TBZ2DPolygonTool); overload;

    { Retourne le centroide (centre de gravité) du polygone }
    function GetCentroid : TBZFloatPoint;
    { Retourne les points d'intersection entre le polygone et une ligne}
    function IntersectWithLine(aLine : TBZ2DLineTool; out IntersectPoints : TBZArrayOfFloatPoints) : Boolean;

    //procedure ClipLine(var aLine : TBZ2DLinteTool);
    //procedure ClipWithPolygon(aPolygon : TBZ2DPolygonTool); overload;
    //procedure ClipWithPolygon(aPolygon : TBZ2DPolygonTool; out NewPolygon : TBZ2DPolygonTool); overload;
    //procedure Triangulate;

    //function GetIntersectPointsWithLine(P1,P2 : TBZFloatPoint; out IntersectPoints :TBZArrayofFloatPoints) : Boolean;
    //function GetIntersectPointsWithLine(aLine : TBZ2DLineTool; out IntersectPoints :TBZArrayofFloatPoints) : Boolean;

    //function SimplifyTo(Const Tolerance : Single  = 1.0) : TBZArrayOfFloatPoints;
    //procedure Simplify(Const Tolerance : Single  = 1.0);

    { Retourne les limites absolues du polygone }
    property Bounds: TBZFloatRect read GetBounds;
    { Retourne la coordonée Y minimum }
    property StartY :  Integer read FStartY;
    property FloatStartY :  Single read FFloatStartY;
    { Retourne le nombre de ligne scannées }
    property RastersLineCount : Integer read FRastersLineCount;
    { Retourne la liste des lignes scannées }
    property Rasters : TBZRastersList read getRastersList;

    { Retourne la liste des lignes scannées est mise en cache }
   // property RastersListCached : Boolean read FRastersListCached;

  end;

  { Gestion d'une liste de polygone }
  TBZ2DPolygonList = Class(TBZPersistentObjectList)
  private
  Protected
    Function GetPolygonItem(index: Integer): TBZ2DPolygonTool;
    Procedure SetPolygonItem(index: Integer; val: TBZ2DPolygonTool);
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;

    { Assigne une autre liste de segment }
    procedure Assign(source: TPersistent); override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    { Efface la liste de segment}
    Procedure Clear; Override;
    { Ajoute un segment à la liste }
    function AddPolygon(Polygon : TBZ2DPolygonTool) : Integer;

    { Acces aux  éléments de la liste }
    Property Items[Index: Integer]: TBZ2DPolygonTool read GetPolygonItem write setPolygonItem;

  end;

  { Classe de manipulation de courbe de bezier quadratic
    @unorderdlist(
      @item(https://pomax.github.io/bezierinfo/)
      @item(https://pomax.github.io/BezierInfo-2/)
      @item(https://www.f-legrand.fr/scidoc/docmml/graphie/geometrie/bezier/bezier.html)
  }
  TBZ2DQuadraticBezierCurve = class(TBZUpdateAbleObject)
  private
    FStartPoint,
    FControlPoint,
    FEndPoint : TBZFloatPoint;
    FTolerance : Single;
    //FAngleTolerance
  protected
    function ComputeSteps(Const Tolerance : Single = 0.1) : Integer;
  public
    Constructor Create; override;

    function ComputePointAt(t: single): TBZFloatPoint;
    function ComputePolyLinePoints(Const nbStep : Integer = -1) : TBZArrayOfFloatPoints; //(Const Smooth : Boolean = false);

    procedure ComputeCoefficients(Out CoefA, CoefB : TBZFloatPoint);

    procedure Split(out LeftCurve, RightCurve :  TBZ2DQuadraticBezierCurve);
    function SplitAt(Position : Single; FromStart : Boolean) : TBZ2DQuadraticBezierCurve;

    function GetLength : Single;
    function GetBounds : TBZFloatRect;

    function ConvertToCubic : TBZ2DCubicBezierCurve;

    function GetDerivativeAt(t : Single) : TBZFloatPoint;

    function GetNormalAt(t : Single) : TBZFloatPoint;
    { Renvoie la pente de la tangente en radian à la position T par rapport au point de debut de la courbe. @br
      Si Full = @True alors le résultat renvoyé sera compris entre 0 et 2*PI (360) si non entre -PI(90) et +Pi(90) }
    //GetTangentFormStart(T : Single; Const Full : Boolean) : Single;
    { Renvoie la pente de la tangente en radian à la position T par rapport au point de fin de la courbe. @br
      Si Full = @True alors le résultat renvoyé sera compris entre 0 et 2*PI (360) si non entre -PI(90) et +Pi(90) }
    //GetTangentFormEnd(T : Single; Const Full : Boolean) : Single;

    //function PointOnCurve(pt : TBZFloatPoint; Const Tolerance : Single = 0.0) : Boolean;

    property StartPoint : TBZFloatPoint read FStartPoint write FStartPoint;
    property ControlPoint : TBZFloatPoint read FControlPoint write FControlPoint;
    property EndPoint : TBZFloatPoint read FEndPoint write FEndPoint;
    property Tolerance : Single read FTolerance write FTolerance;
  end;

  { Tableau de courbes de bezier quadratic}
  TBZQuadraticBezierCurves = array of TBZ2DQuadraticBezierCurve;

{%region=====[ TBZ2DCubicBezierCurve ]================================================================================}

  { Classe de manipulation de courbe de bezier cubic  }
  TBZ2DCubicBezierCurve = class(TBZUpdateAbleObject)
  private
    FControlPoints : Array[0..3] of TBZFloatPoint;
    //FStartPoint,
    //FControlPointA,
    //FControlPointB,
    //FEndPoint : TBZFloatPoint;
    FTolerance : Single;
    function GetControlPoint(AIndex : Integer) : TBZFloatPoint;
    procedure SetControlPoint(AIndex : Integer; const AValue : TBZFloatPoint);
      //FAngleTolerance
  protected

  public
    Constructor Create; override;

    function ComputeOptimalSteps(Const Tolerance : Single = 0.1) : Integer;
    function ComputePointAt(t: single): TBZFloatPoint;
    function ComputePolyLinePoints(Const nbStep : Integer = -1) : TBZArrayOfFloatPoints;

    function GetX(t : Single) : Single;
    function GetY(t : Single) : Single;

    function GetTFromX(x : Single) : Single;
    function GetYFromX(x : Single) : Single;


    //B-Spline
    //function ComputeSmoothPolyLinePoints : TBZArrayOfFloatPoints;

    procedure ComputeCoefficients(Out CoefA, CoefB, CoefC : TBZFloatPoint);

    procedure Split(out LeftCurve, RightCurve :  TBZ2DCubicBezierCurve);
    function SplitAt(Position : Single; FromStart : Boolean) : TBZ2DCubicBezierCurve;

    function GetLength : Single;
    function GetBounds : TBZFloatRect;

    function GetDerivativeAt(t : Single) : TBZFloatPoint;
    function GetNormalAt(t : Single) : TBZFloatPoint;
    function GetTangentAt(t : Single) : TBZFloatPoint;
    function GetTangentAngleAt(t : Single) : Single;

    procedure AlignToLine(lp1, lp2 : TBZFloatPoint);

    property StartPoint : TBZFloatPoint index 0 read GetControlPoint write SetControlPoint;
    property ControlPointA : TBZFloatPoint index 1 read GetControlPoint write SetControlPoint;
    property ControlPointB : TBZFloatPoint index 2 read GetControlPoint write SetControlPoint;
    property EndPoint : TBZFloatPoint index 3 read GetControlPoint write SetControlPoint;
    property Tolerance : Single read FTolerance write FTolerance;
  end;

  //TBZCubicBezierCurves = array of TBZ2DCubicBezierCurve;

  { Classe de gestion de courbes de Bezier cubic }
  TBZCubicBezierCurves = Class(TBZPersistentObjectList)
  private
  Protected
    Function GetCubicBezierCurveItem(index: Integer): TBZ2DCubicBezierCurve;
    Procedure SetCubicBezierCurveItem(index: Integer; val: TBZ2DCubicBezierCurve);
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;

    { Assigne une autre liste de segment }
    procedure Assign(source: TPersistent); override;

    procedure WriteToFiler(writer: TVirtualWriter); override;
    procedure ReadFromFiler(reader: TVirtualReader); override;

    { Efface la liste de segment}
    Procedure Clear; Override;
    { Ajoute un segment à la liste }
    function AddCurve(aCurve : TBZ2DCubicBezierCurve) : Integer;

    { Acces aux  éléments de la liste }
    Property Items[Index: Integer]: TBZ2DCubicBezierCurve read GetCubicBezierCurveItem write setCubicBezierCurveItem;

  end;
{%endregion}

//  TBZ2DPolyBezierCurve = class(TBZFloatPointsContainer)

{%region=====[ TBZ2DBezierSplineCurve ]================================================================================}


{%endregion}
//  TBZ2DCubicSplineCurve = class(TBZFloatPointsContainer)
//  TBZ2DCatmullRomSplineCurve = class(TBZFloatPointsContainer)

{%region=====[ TBZ2DCurves ]==========================================================================================}

  { Classe de gestion de courbes de bezier et spline }

  { TBZ2DCurve }

  TBZ2DCurve = class(TBZFloatPointsContainer)
  private
    FCurveType : TBZCurveType;
    FCurves : TBZCubicBezierCurves;
  protected
    function ComputeUniformSplineCurve : TBZArrayOfFloatPoints;
    function ComputeCatmullRomSplineCurve : TBZArrayOfFloatPoints;

    function ComputeBezierCurve : TBZArrayOfFloatPoints;
    function ComputeSmoothBezierCurve : TBZArrayOfFloatPoints;
    function ComputeBezierSplineCurve : TBZArrayOfFloatPoints;
  public
    Constructor Create(aCurveType : TBZCurveType); overload; virtual;
    Constructor Create(aCurveType : TBZCurveType; aControlPoints : TBZArrayOfFloatPoints); overload; virtual;

    function ComputePolylinePoints : TBZArrayOfFloatPoints;

    property CurveType : TBZCurveType read FCurveType write FCurveType;
  end;

  {%endregion}

  //TBZ2DPolyPolygons

{ Creation d'une courbe de bezier quadratic }
function CreateQuadraticBezierCurve(StartPoint, ControlPoint, EndPoint : TBZFloatPoint) : TBZ2DQuadraticBezierCurve;

//function ComputePolyLinesQuadraticBezierCurve( Curves : Array of TBZ2DCubicBezierCurve): TBZArrayOfFloatPoints;

{ Creation d'une courbe de bezier cubic }
function CreateCubicBezierCurve(StartPoint, ControlPointA, ControlPointB, EndPoint : TBZFloatPoint) : TBZ2DCubicBezierCurve;

{ Creation d'un polygone en forme de cercle }
procedure BuildPolyCircle(ptc : TBZFloatPoint; Radius : Single; out ArrayOfPoints : TBZArrayOfFloatPoints; Const Steps : Integer = -1);
{ Creation d'un polygone en forme d'ellipse }
procedure BuildPolyEllipse(ptc : TBZFloatPoint; RadiusX, RadiusY : Single; out ArrayOfPoints : TBZArrayOfFloatPoints; Const Steps : Integer = -1);
{ Creation d'un polygone en forme d'arc de cercle }
procedure BuildPolyArc(cx, cy, rx, ry, StartAngle, EndAngle : Single; const ClockWise : Boolean; out ArrayOfPoints : TBZArrayOfFloatPoints);
{ Creation d'un polygone de type "supershape". cf : http://paulbourke.net/geometry/supershape/  }
procedure BuildPolySuperShape(Coords : TBZFloatPoint; n1, n2, n3, m, a, b, ScaleFactor : Single; nbPoints : Integer; out ArrayOfPoints : TBZArrayOfFloatPoints);
{ Creation d'un polygone en forme d'étoile }
procedure BuildPolyStar(Coords : TBZFloatPoint; InnerRadius, OuterRadius : Single; nbBranches : Integer; out ArrayOfPoints : TBZArrayOfFloatPoints);

//function ComputePolyLinesCubicBezierCurve( Curves : Array of TBZ2DCubicBezierCurve): TBZArrayOfFloatPoints;

//function CreateCubicBezierFromThreePoints(StartPoint, CentezPoint, EndPoint);

//function CreateCubicBezierArc(Cx, Cy, Radius, StartAngle, SweepAngle : Single) : TBZ2DQuadraticBezierCurve;

//procedure BuildPolygonRectangle;
//procedure BuildPolygonRoundedRectangle;
//procedure BuildPolygonCircle;
//procedure BuildPolygonEllipse;
//procedure BuildPolygonPie;
//procedure BuildPolygonChord;
//procedure BuildPolygonTriangle;
//procedure BuildPolygonStar;

//procedure BuildPolyLineRectangle;
//procedure BuildPolyLineRoundedRectangle;
//procedure BuildPolyLineCircle;
//procedure BuildPolyLineEllipse;
//procedure BuildPolyLinePie;
//procedure BuildPolyLineChord;
//procedure BuildPolyLineTriangle;
//procedure BuildPolyLineStar;
//procedure BuildPolyLineSuperShape;


implementation

uses BZUtils, BZTypesHelpers, BZLogger;

{%region=====[ TBZFloatPointsContainer ]===================================================================}

constructor TBZFloatPointsContainer.Create(const AutoFreePoints : Boolean);
begin
  inherited Create;
  FPoints := nil;
  FAutoFree := AutoFreePoints;
  FForceFree := False;
end;

constructor TBZFloatPointsContainer.Create(aPoints : TBZArrayOfFloatPoints; const AutoFreePoints : Boolean);
begin
  inherited Create;
  FPoints := aPoints;
  FAutoFree := AutoFreePoints;
end;

constructor TBZFloatPointsContainer.Create(ReservedPoints : Integer);
begin
  inherited Create;
  FPoints := TBZArrayOfFloatPoints.Create(ReservedPoints);
  FForceFree := True;
end;

destructor TBZFloatPointsContainer.Destroy;
begin
  If FAutoFree or FForceFree then
  begin
    if Assigned(FPoints) then
    begin
      FreeAndNil(FPoints);
    end;
  end;
  inherited Destroy;
end;

procedure TBZFloatPointsContainer.NotifyChange(Sender : TObject);
begin
  inherited NotifyChange(Sender);
end;

procedure TBZFloatPointsContainer.AssignPoints(const aPoints : TBZArrayofFloatPoints);
begin
  if Not(Assigned(FPoints)) then
  begin
    FPoints := TBZArrayOfFloatPoints.Create(aPoints.Count);
    FForceFree := True;
  end;
  FPoints.Count := APoints.Count;
  Move(APoints.DataArray[0], FPoints.DataArray[0], Apoints.Count * SizeOf(TBZFloatPoint));
  NotifyChange(Self);
end;

function TBZFloatPointsContainer.GetCount : Integer;
begin
  Result := -1;
  if Assigned(FPoints) then Result := FPoints.Count;
end;

procedure TBZFloatPointsContainer.SetCount(AValue : Integer);
begin
  if Assigned(FPoints) then
  begin
    FPoints.Count := AValue;
    NotifyChange(Self);
  end;
end;

function TBZFloatPointsContainer.GetPoint(Index : Integer) : TBZFloatPoint;
begin
  Result :=cNullVector2f;
  if Assigned(FPoints) then
  begin
    if (Index>=0) and (Index<FPoints.Count) then Result := FPoints.Items[Index];
  end;
end;

procedure TBZFloatPointsContainer.SetPoint(Index : Integer; APoint : TBZFloatPoint);
begin
  if Assigned(FPoints) then
  begin
    if FPoints.Count>Index then
    begin
      FPoints.Items[Index] := APoint;
      NotifyChange(Self);
    end;
  end;
end;

function TBZFloatPointsContainer.GetPointsList : TBZArrayOfFloatPoints;
begin
  Result := FPoints;
end;

procedure TBZFloatPointsContainer.SetPointsList(Value : TBZArrayOfFloatPoints);
Var
  i : Integer;
begin
  if Assigned(Value) then
  begin
    if Assigned(FPoints) and FAutoFree then
    begin
      FreeAndNil(FPoints);
      FPoints := Value;
    end
    else
    begin
      FPoints.Clear;
      For i := 0 to Value.Count - 1 do
      begin
        FPoints.Add(Value.Items[i]);
      end;
    end;

  end;
end;

{%endregion}

{%region=====[ TBZ2DLineTool ]=============================================================================}

constructor TBZ2DLineTool.Create;
begin
  inherited Create;
  FTagInt := 0;
  FStartPoint.Create(0,0);
  FEndPoint.Create(0,0);
end;

procedure TBZ2DLineTool.Assign(source : TPersistent);
begin
  if (Source is TBZ2DLineTool) then
  begin
    FStartPoint := TBZ2DLineTool(Source).StartPoint;
    FEndPoint := TBZ2DLineTool(Source).EndPoint;
   // FTag := TBZ2DLineTool(Source).Tag;
    inherited Assign(source);
  end
  else inherited Assign(source);
end;

procedure TBZ2DLineTool.SetLineFromPoints(Const aStartPoint, aEndPoint : TBZFloatPoint);
begin
  FStartPoint := aStartPoint;
  FEndPoint := aEndPoint;
end;

procedure TBZ2DLineTool.SetInfiniteLine(aStartPoint, aDirection : TBZFloatPoint; aDistance : Single);
Var
  {$CODEALIGN VARMIN=16}
  DN : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin
  FStartPoint := aStartPoint;
  DN := aDirection.Normalize;
  FEndPoint := FStartPoint + (DN * aDistance);
end;

function TBZ2DLineTool.Clone : TBZ2DLineTool;
Var
  NewLine : TBZ2DLineTool;
begin
  NewLine := TBZ2DLineTool.Create;
  NewLine.SetLineFromPoints(FStartPoint, FEndPoint);
  Result := NewLine;
end;

function TBZ2DLineTool.GetInfiniteIntersectPoint(aLine : TBZ2DLineTool; out IntersectPoint : TBZFloatPoint) :  TBZ2DLineIntersecType;
Var
  {$CODEALIGN VARMIN=16}
  p,v,q,u, d, a,b : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  t, az, bz : Single;
begin
   Result := ilNone;
   p := FStartPoint;
   v := Self.GetDirection;
   q := aLine.StartPoint;
   u := aLine.GetDirection;

    a := v.CrossProduct(u);  // cross product 3D (x,y)
    az := v.Perp(u);         // cross product 3D (z)

   // Les ligne sont paralelles, pas d'intersection
   if ((a.x = 0) and (a.y = 0)) or  (az=0) then
   begin
     Exit;
   end
   else
   begin
     d := q - p;
     b  := d.CrossProduct(u);
     bz := d.Perp(u);

     t := 0;
     if (az<>0) then t := bz / az
     else if (a.x <> 0) then t := b.x / a.x
     else if (a.y <> 0) then  t := b.y / a.y;
      // intersection par rapport a la ligne
     IntersectPoint := P + (v*t);
     Result := ilIntersect;
   end;

end;

function TBZ2DLineTool.GetIntersectPoint(aLine : TBZ2DLineTool; out IntersectPoint, IntersectEndPoint : TBZFloatPoint) : TBZ2DLineIntersecType;
var
  {$CODEALIGN VARMIN=16}
  u,v,w, w2 : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  d, du, dv, t, t0, t1 : Single;
begin
  Result := ilNone;

  u := Self.EndPoint - Self.StartPoint;
  v := aLine.EndPoint - aLine.StartPoint;
  w := Self.StartPoint - aLine.StartPoint;
  d := u.perp(v);

  if (abs(d)<cEpsilon) then // Si les lignes sont "presque" paralelles
  begin
    if (u.perp(w)<>0) or (v.perp(w)<>0) then exit; // Les lignes ne sont pas colinéaires

    // On verifie si les lignes sont colinéaires ou dégénérées
    du := u.DotProduct(u);
    dv := v.DotProduct(v);
    if (du = 0) and (dv = 0) then  // Les 2 Lignes sont des points
    begin
      if (Self.StartPoint.x = aLine.StartPoint.x) and (Self.StartPoint.y = aLine.StartPoint.y) then // Les points sont les mêmes
      begin
         IntersectPoint := Self.StartPoint;
         Result := ilIntersect;
         Exit;
      end
      else Exit;
    end;

    if (du = 0) then  // La ligne 1 est un seul point
    begin
      if aLine.IsPointOnLine(Self.StartPoint) then  // Le point est sur la 2eme ligne
      begin
        IntersectPoint := Self.StartPoint;
        Result := ilIntersect;
        Exit;
      end
      else Exit;
    end;

    if (dv = 0) then  // La ligne 2 est un seul point
    begin
      if Self.IsPointOnLine(aLine.StartPoint) then  // Le point est sur la 1eme ligne
      begin
        IntersectPoint := aLine.StartPoint;
        Result := ilIntersect;
        Exit;
      end
      else Exit;
    end;

    // Les lignes sont colinéaires. Est-ce que les deux lignes se chevauche ?
    w2 := Self.EndPoint - aLine.StartPoint;
    if (v.x <> 0) then
    begin
      t0 := w.x / v.x;
      t1 := w2.x / v.x;
    end
    else
    begin
      t0 := w.y / v.y;
      t1 := w2.y / v.y;
    end;
    if (t0 > t1) then   // t0 doit être plus petit que t1. On echange
    begin
      t := t0;
      t0 := t1;
      t1 := t;
    end;
    if (t0 > 1) or (t1 < 0) then Exit; // Les ligne ne se chevauchent pas

    // On borne les valeurs
    if (t0<0) then t0 := 0;    // min 0
    if (t1>1) then t1 := 1;    // max 1
    if (t0 = t1) then     // Il y a intersection
    begin
      IntersectPoint := aLine.StartPoint + (v * t0);
      Result := ilIntersect;
      Exit;
    end;

    // Les 2 points de la deuxieme ligne sont sur la 1ere
    IntersectPoint := aLine.StartPoint + (v * t0);
    IntersectEndPoint := aLine.StartPoint + (v * t1);
    result := ilOverlap;
    Exit;
  end;

  // les lignes sont obliques et peuvent se croiser en un point
  // Intersection pour la ligne 1
  t0 := v.perp(w) / D;
  if (t0 < 0) or ( t0 > 1) then Exit; // Pas d'intersection avec la ligne 1

  // Intersection pour la ligne 2
  t1 := u.perp(w) / D;
  if (t1 < 0) or (t1 > 1) then Exit; // Pas d'intersection avec la  ligne 2

  IntersectPoint := Self.StartPoint + (u * t0); // Calcul du point d'intersection sur la ligne 1
  Result := ilIntersect;
end;

function TBZ2DLineTool.IntersectWithLine(aLine : TBZ2DLineTool) : Boolean;
Var
  {$CODEALIGN VARMIN=16}
  p1, p2 : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin
  Result := (Self.GetIntersectPoint(aLine, p1, p2) <> ilNone);
end;


function TBZ2DLineTool.IntersectSegmentWithCircle(aCenterPoint : TBZFloatPoint; aRadius : Single) : Boolean;
Var
  dx, dy, a, b, c,d : Single;
  {$CODEALIGN VARMIN=16}
  dl,cl, mc : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin
  dl := FEndPoint - FStartPoint;
  cl := FStartPoint- - aCenterPoint;
  a := dl.LengthSquare;
  b := (dl.x * cl.x) + (dl.y * cl.y);
  b := b + b;
  c := aCenterpoint.LengthSquare;
  c := c + FStartPoint.LengthSquare;
  mc := FStartPoint * aCenterPoint;
  d := mc.x + mc.y;
  d := d + d;
  c := c - d;
  Result := ((b * b - 4 * a * c) >= 0);
end;

function TBZ2DLineTool.GetInfiniteIntersectPointsWithCircle(aCenterPoint : TBZFloatPoint; aRadius : Single; Out IntersectPointA, IntersectPointB : TBZFloatPoint) : Boolean;
//Note that the function for line-sphere intersection is exactly the same, except in aRadius*aRadius*aRadius instead of aRadius*aRadius.
Var
  a, b, c,d,  InvA, f1,f2, Dist, SqrtDist {,InvRadius}: Single;
  {$CODEALIGN VARMIN=16}
  dl, dir: TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin
  dl := FStartPoint - aCenterPoint;
  dir := Self.GetDirection;
  d := aRadius * aRadius;
  a := Dir.DotProduct(Dir);
  b := dl.DotProduct(dir);
  c := dl.DotProduct(dl) - d;
  Dist := (b * b - a * c) ;
  if Dist < 0.0 then
  begin
    Result := False;
  end
  else
  begin
    //InvRadius := 1/aRadius;
    SqrtDist := System.Sqrt(Dist);
    InvA := 1/a;
    f1 := (-b - sqrtDist) * invA;
    f2 := (-b + sqrtDist) * invA;
    IntersectPointA := FStartPoint + (Dir * f1);
    IntersectPointB := FStartPoint + (Dir * f2);
    //normal = (point - aCenterPoint) * invRadius;
    Result := True;
  end;

end;

function TBZ2DLineTool.GetSegmentIntersectPointsWithRect(aRect : TBZFloatRect; Out IntersectPointA, IntersectPointB : TBZFloatPoint) : Boolean;
var
  l1,l2,l3,l4 : TBZ2DLineTool;
  i1, i2, i3, i4 : TBZ2DLineIntersecType;
  {$CODEALIGN VARMIN=16}
  P1,P2 : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin
  Result := False;

  l1 := TBZ2DLineTool.Create;
  l1.SetLineFromPoints(aRect.TopLeft, aRect.TopRight);

  l2 := TBZ2DLineTool.Create;
  l2.SetLineFromPoints(aRect.TopRight, aRect.BottomRight);

  l3 := TBZ2DLineTool.Create;
  l3.SetLineFromPoints(aRect.BottomRight, aRect.BottomLeft);

  l4 := TBZ2DLineTool.Create;
  l4.SetLineFromPoints(aRect.BottomLeft, aRect.TopLeft);

  i1 := Self.GetIntersectPoint(l1, P1, P2);
  i2 := Self.GetIntersectPoint(l1, P1, P2);
  i3 := Self.GetIntersectPoint(l1, P1, P2);
  i4 := Self.GetIntersectPoint(l1, P1, P2);

  Result := ((i1<>ilNone) or (i2<>ilNone) or (i3<>ilNone) or (i4<>ilNone));

  FreeAndNil(l1);
  FreeAndNil(l2);
  FreeAndNil(l3);
  FreeAndNil(l4);
end;

function TBZ2DLineTool.IsPointOnLine(aPoint : TBZFloatPoint) : Boolean;
begin
 Result := False;
  if (Self.StartPoint.x <> Self.EndPoint.x) then    // La ligne n'est pas vertical
  begin
    if ((Self.StartPoint.x <= aPoint.x) and ( aPoint.x <= Self.EndPoint.x))
       or ((Self.StartPoint.x >= aPoint.x) and ( aPoint.x >= Self.EndPoint.x)) then result := true;
  end
  else
  begin
     if ((Self.StartPoint.y <= aPoint.y) and ( aPoint.y <= Self.EndPoint.y))
        or ((Self.StartPoint.y >= aPoint.y) and ( aPoint.y >= Self.EndPoint.y)) then result := true;
  end;
end;

procedure TBZ2DLineTool.GetPerpendicularPointsAtStartPoint(aDistance : Single; out LeftPoint, RightPoint  : TBZFloatPoint);
var
  {$CODEALIGN VARMIN=16}
  D, P, T : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  Slope, Dist: Single;
begin
  D := FEndPoint - FStartPoint;
  Dist := Self.GetLength;
  Slope := aDistance / Dist;
  P := D * Slope;
  T.Create(-P.y, P.x);
  LeftPoint := FStartPoint + T;
  //Second
  T.Create(P.y, -P.x);
  RightPoint := FStartPoint + T;
  //GlobalLogger.LogStatus('GetPerpendicularPointsAtStartPoint : Left = ' + LeftPoint.ToString + 'Right : ' + RightPoint.ToString);
end;

procedure TBZ2DLineTool.GetPerpendicularPointsAtEndPoint(aDistance : Single; out LeftPoint, RightPoint  : TBZFloatPoint);
var
  {$CODEALIGN VARMIN=16}
  D, P, T : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  Slope, Dist: Single;
begin
  D := FEndPoint - FStartPoint;
  Dist := Self.GetLength;
  Slope := aDistance / Dist;
  P := D * Slope;
  T.Create(-P.y, P.x);
  LeftPoint := FEndPoint + T;
  //Second
  T.Create(P.y, -P.x);
  RightPoint := FEndPoint + T;
  //GlobalLogger.LogStatus('GetPerpendicularPointsAtEndPoint : Left = ' + LeftPoint.ToString + 'Right : ' + RightPoint.ToString);
end;

function TBZ2DLineTool.GetPerpendicularPointFromPoint(aPoint : TBZFloatPoint) : TBZFloatPoint;
var
  {$CODEALIGN VARMIN=16}
  D1, D2, V,P : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  Dist, t : Single;
begin
  D1 := FEndPoint - FStartPoint;
  Dist := D1.LengthSquare;
  D2 := aPoint - FStartPoint;
  V.Create(D2.Y,D2.x);
  V := D1 * V;
  t := (V.x - V.Y) / Dist;
  P.Create(-D1.Y, D1.x);
  Result := aPoint + (P * t);
end;

function TBZ2DLineTool.GetPerpendicularPointsAtPoint(aPoint : TBZFloatPoint; aDistance : Single; out LeftPoint, RightPoint : TBZFloatPoint) : Boolean;
var
  {$CODEALIGN VARMIN=16}
  LP,RP : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  L : TBZ2DLineTool;
begin
  Result := False;
  if Self.IsPointOnLine(aPoint) then
  begin
    L := TBZ2DLineTool.Create;
    L.SetLineFromPoints(aPoint, FEndPoint);
    L.GetPerpendicularPointsAtStartPoint(aDistance, LP,RP);
    RightPoint := RP;
    LeftPoint := LP;
    L.Free;
    Result := True;
  end;
end;

function TBZ2DLineTool.GetDirection : TBZFloatPoint;
begin
  Result := FEndPoint - FStartPoint;
 // Result := -Result;
end;

function TBZ2DLineTool.GetNormalizeDirection : TBZFloatPoint;
Var
  {$CODEALIGN VARMIN=16}
  P : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin
  P := FEndPoint - FStartPoint;
 // P := -P;
  Result := P.Normalize;
end;

function TBZ2DLineTool.GetMidPoint : TBZFloatPoint;
begin
  Result := (FStartPoint + FEndPoint) * 0.5;
end;

function TBZ2DLineTool.GetPointOnLineFromStartPoint(aDistance : Single) : TBZFloatPoint;
Var
  {$CODEALIGN VARMIN=16}
  Dir : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin
  Dir := Self.GetNormalizeDirection;
  Result :=  FStartPoint + (Dir * aDistance);
end;

function TBZ2DLineTool.GetPointOnLineFromEndPoint(aDistance : Single) : TBZFloatPoint;
Var
  {$CODEALIGN VARMIN=16}
  Dir : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin
  Dir := Self.GetNormalizeDirection;
  Result :=  FEndPoint + (-Dir * aDistance);
end;

function TBZ2DLineTool.getSlope : Single;
Var
  S, x1, y1: Single;
begin
  result := 0;
  x1 := FEndPoint.x - FStartPoint.x;
  if (x1<>0) then
  begin
    y1 := (FEndPoint.y - FStartPoint.y);
    S :=  y1 / x1;
    if (y1<>0) then
    begin
      if (Abs(S)<>1) then S:=-S;
    end;
    Result := S;
  end
  else Result := 0;
end;

function TBZ2DLineTool.getReciprocalInvSlope : Single;
Var
  S : Single;
begin
  Result := 0;
  S := Self.GetSlope;
  if S<>0 then Result :=-(1/S);
end;

function TBZ2DLineTool.GetYIntercept : Single;
begin
  Result := FStartPoint.y - (Self.GetSlope * FStartPoint.x);
end;

function TBZ2DLineTool.GetLength : Single;
begin
  Result := FStartPoint.Distance(FEndPoint);
end;

procedure TBZ2DLineTool.GetNormal(Out LeftNormal, RightNormal : TBZFloatPoint);
var
  {$CODEALIGN VARMIN=16}
  P, T : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin
  P := FEndPoint - FStartPoint;
  T.Create(-P.y, P.x);
  RightNormal :=  T;

  T.Create(P.y, -P.x);
  LeftNormal := T;
end;

function TBZ2DLineTool.DistanceLineToPoint(aPoint : TBZFloatPoint) : Single;
Var
  {$CODEALIGN VARMIN=16}
  p, v, w : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  d1,d2,t : Single;
begin
  v := FEndPoint - FStartPoint;
  w := aPoint - FStartPoint;
  d1 := w.DotProduct(v);
  d2 := v.DotProduct(v);
  t := d1 / d2;
  p := FStartPoint + (v * t);
  Result := (aPoint - P).Length;
end;

function TBZ2DLineTool.DistanceLineToPoint(aPoint : TBZFloatPoint; out aDir : TBZFloatPoint) : Single;
Var
  {$CODEALIGN VARMIN=16}
  p, v, w : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  d1,d2,t : Single;
begin
  v := FEndPoint - FStartPoint;
  w := aPoint - FStartPoint;
  d1 := w.DotProduct(v);
  d2 := v.DotProduct(v);
  t := d1 / d2;
  p := FStartPoint + (v * t);
  aDir := (aPoint - P);
  Result := aDir.Length;
end;

function TBZ2DLineTool.DistanceSegmentToPoint(aPoint : TBZFloatPoint) : Single;
Var
  {$CODEALIGN VARMIN=16}
  p, v, w : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  d1,d2,t : Single;
begin
  v := FEndPoint - FStartPoint;
  w := aPoint - FStartPoint;
  d1 := w.DotProduct(v);

  if (d1<=0) then
  begin
    Result := (aPoint - FStartPoint).Length;
    Exit;
  end;

  d2 := v.DotProduct(v);
  if (d2 <= d1) then
  begin
    Result := (aPoint - FEndPoint).Length;
    Exit;
  end;

  t := d1 / d2;
  p := FStartPoint + (v * t);
  Result := (aPoint - P).Length;
end;

function TBZ2DLineTool.DistanceSegmentToPoint(aPoint : TBZFloatPoint; out aDir : TBZFloatPoint) : Single;
Var
  {$CODEALIGN VARMIN=16}
  p, v, w : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  d1,d2,t : Single;
begin
  v := FEndPoint - FStartPoint;
  w := aPoint - FStartPoint;
  d1 := w.DotProduct(v);

  if (d1<=0) then
  begin
    aDir := (aPoint - FStartPoint);
    Result := aDir.Length;
    Exit;
  end;

  d2 := v.DotProduct(v);
  if (d2 <= d1) then
  begin
    aDir := (aPoint - FEndPoint);
    Result := aDir.Length;
    Exit;
  end;

  t := d1 / d2;
  p := FStartPoint + (v * t);
  aDir := (aPoint - P);
  Result := aDir.Length;
end;

function TBZ2DLineTool.PerpendicularDistance(aPoint : TBZFloatPoint) : Single;
Var
  {$CODEALIGN VARMIN=16}
  Delta, DeltaSquare, vMag, pv, Dir, vDot : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  mag, pvDot : Single;
begin
  Delta := FEndPoint - FStartPoint;
  DeltaSquare := Delta * Delta;
	//Normalise
	//mag := Math.power(Math.power(Delta.x,2.0)+Math.Power(Delta.y,2.0),0.5);
  mag := Math.power(DeltaSquare.X + DeltaSquare.Y ,0.5);
	if (mag > 0.0) then
	begin
    vMag.Create(mag,mag);
		Delta := Delta / vMag;
  end;

  pv := aPoint - FStartPoint;

	//Get dot product (project pv onto normalized direction)
	//pvDot := Delta.x * pv.x + Delta.y * pv.y;
  pvDot := Delta.DotProduct(pv);

  vDot.Create(pvDot, pvDot);
	//Scale line direction vector
  Dir := vDot * Delta;

	//Subtract this from pv
  vDot := pv - Dir;
  vDot := vDot * vDot;
	//Result := Math.power(Math.power(vDot.x,2.0)+Math.power(vDot.y,2.0),0.5);
  Result := Math.power(vDot.X + vDot.Y,0.5);
end;

procedure TBZ2DLineTool.Scale(Factor : Single);
Var
  {$CODEALIGN VARMIN=16}
  Dir : TBZFloatPoint;
  P1, P2 : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin
  Dir := Self.GetDirection;
  P1 :=  FStartPoint + (-Dir * Factor);
  P2 :=  FEndPoint + (Dir * Factor);
  FStartPoint := P1;
  FEndPoint := P2;
end;

procedure TBZ2DLineTool.Translate(dx, dy : Single);
Var
  {$CODEALIGN VARMIN=16}
  V : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin
  V.Create(dx,dy);
  Self.Translate(V);
end;

procedure TBZ2DLineTool.Translate(aVector : TBZFloatPoint);
begin
  FStartPoint := FStartPoint + aVector;
  FEndPoint := FEndPoint + aVector;
end;

procedure TBZ2DLineTool.Rotate(anAngle : Single; aCenterPoint : TBZFloatPoint);
begin
  FStartPoint := FStartPoint.Rotate(anAngle, aCenterPoint);
  FEndPoint := FEndPoint.Rotate(anAngle, aCenterPoint);
end;

procedure TBZ2DLineTool.Rotate(anAngle : Single);
begin
  Self.Rotate(anAngle, Self.GetMidPoint);
end;

function TBZ2DLineTool.BuildStroke(Width : Integer; StrokeMode : TBZStrokeMode; StartCap, EndCap : TBZCapsMode) : TBZArrayOfFloatPoints;
Var
  w : Integer;
  {$CODEALIGN VARMIN=16}
  EndLeftPerp, EndRightPerp : TBZFloatPoint;
  StartLeftPerp, StartRightPerp : TBZFloatPoint;
  OutPoints : TBZArrayOfFloatPoints;
  {$CODEALIGN VARMIN=4}
begin
  OutPoints := TBZArrayOfFloatPoints.Create(4);
  Case StrokeMode of
    smInner:
    begin
      w := Width - 1;
      StartRightPerp := FStartPoint;
    end;
    smOuter:
    begin
      w := Width - 1;
    end;
    smAround:
    begin
      w := (Width - 1) div 2;
    end;
  end;

  Self.GetPerpendicularPointsAtStartPoint(w,StartLeftPerp, StartRightPerp);
  Self.GetPerpendicularPointsAtEndPoint(w,EndLeftPerp, EndRightPerp);

  Case StartCap of
    cmNone:
    begin
      OutPoints.Add(StartRightPerp);
      OutPoints.Add(EndRightPerp);
    end;
    cmSquare :
    begin

    end;
    cmRounded :
    begin

    end;
    cmArrow :
    begin

    end;
    cCircle :
    begin

    end;
    cmCustom :
    begin

    end;
  end;

  Case EndCap of
    cmNone:
    begin
      OutPoints.Add(FEndPoint);
      OutPoints.Add(FStartPoint);
    end;
    cmSquare :
    begin

    end;
    cmRounded :
    begin

    end;
    cmArrow :
    begin

    end;
    cCircle :
    begin

    end;
    cmCustom :
    begin

    end;
  end;

  Result := OutPoints;
end;

function TBZ2DLineTool.IsPerpendicular(aLine : TBZ2DLineTool) : Boolean;
var
  InvS, S : Single;
begin
  if (Self.IsVertical and aLine.IsHorizontal) or (Self.IsHorizontal and aLine.IsVertical) then Result := True
  else
  begin
    InvS := Self.GetReciprocalInvSlope;
    S := aLine.getSlope;
    if (InvS = 0) and  (S=0) then result := False
    else Result := ((InvS - S)=0); //((InvS * S) = -1);
    // Autre solution :  Est perpendiculaire si Line1.Slope * Line2.Slope = -1
  end;
end;

function TBZ2DLineTool.IsParallel(aLine : TBZ2DLineTool) : Boolean;
var
  S1, S2 : Single;
begin
  if (Self.IsHorizontal and aLine.IsHorizontal) or (Self.IsVertical and aLine.IsVertical) then Result := True
  else
  begin
    S1 := Self.GetSlope;
    S2 := aLine.GetSlope;
    if (S1 = 0) and (S2 =0) then Result := False
    else Result := (S1 = S2);
  end;
end;

function TBZ2DLineTool.IsVertical : Boolean;
begin
  Result := (FEndPoint.x = FStartPoint.x);
end;

function TBZ2DLineTool.IsHorizontal : Boolean;
begin
  Result := (FEndPoint.y = FStartPoint.y);
end;

{%endregion}

{%region=====[ TBZ2DEdgeList ]=============================================================================}

Constructor TBZ2DEdgeList.Create;
begin
  inherited Create;
end;

Destructor TBZ2DEdgeList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TBZ2DEdgeList.CompareEdge(item1, item2 : TObject) : Integer;
Var
  Edge1, Edge2, TempEdge : TBZ2DLineTool;
  R , dx1, dx2, dy1, dy2 : Single;
  FactorSwap, Sgn : Single;

  function HorizLine : Single;
  begin
    if (Edge1.StartPoint.Y = Edge1.EndPoint.Y) then
    begin
      Result := (Edge1.StartPoint.X - Edge2.StartPoint.X) * FactorSwap;
      Exit;
    end;

    if ((Edge1.StartPoint.X = Edge2.StartPoint.X) or (Edge1.EndPoint.X = Edge2.StartPoint.X)) then
    begin
      Result := -1 * FactorSwap;
      Exit;
    end;

    if ((Edge1.StartPoint.X = Edge2.EndPoint.X) or (Edge1.EndPoint.X = Edge2.EndPoint.X)) then
    begin
      Result := 1 * FactorSwap;
      Exit;
    end;

    R := (Edge1.StartPoint.X - Edge2.StartPoint.X) * FactorSwap;
    if R>0 then
    begin
      Result := R;
    end
    else
    begin
      Result := (Edge1.EndPoint.X - Edge2.EndPoint.X) * FactorSwap;
    end;
  end;

begin
  Edge1 := TBZ2DLineTool(Item1);
  Edge2 := TBZ2DLineTool(Item2);

  //R := Edge1.StartPoint.Y - Edge2.StartPoint.Y
  if (Edge1.StartPoint.Y - Edge2.StartPoint.Y) = 0 then
  begin
    Result := 0;
    Exit;
  end;

  if (Edge2.StartPoint.Y = Edge2.EndPoint.Y) then
  begin
    FactorSwap := 1;
    Result := Round(HorizLine);
    Exit;
  end;

  if (Edge1.StartPoint.Y = Edge1.EndPoint.Y) then
  begin
    FactorSwap := -1;
    TempEdge := Edge1;
    Edge1 := Edge2;
    Edge2 := TempEdge;
    Result := Round(HorizLine);
    Exit;
  end;

  R := Edge1.StartPoint.X - Edge2.StartPoint.X;
  if R > 0 then
  begin
    Result := Round(R);
    Exit;
  end;

  dx1 := Edge1.EndPoint.X - Edge1.StartPoint.X;
  dx2 := Edge2.EndPoint.X - Edge2.StartPoint.X;

  if (Sign(dx1) <> Sign(dx2)) then
  begin
    Result := Round(Edge1.EndPoint.X - Edge2.EndPoint.X);
    Exit;
  end;

  Sgn := Sign(dx1);
  dy1 := Edge1.EndPoint.Y - Edge1.StartPoint.Y;
  dy2 := Edge2.EndPoint.Y - Edge2.StartPoint.Y;

  dx1 := dx1 * Sgn;
  dx2 := dx2 * Sgn;

  if ((dx1 * dy2) / dy1) < dx2 then Result := -Round(Sgn)
  else Result := Round(sgn);
end;

Function TBZ2DEdgeList.GetEdgeItem(index : Integer) : TBZ2DLineTool;
begin
   Result := TBZ2DLineTool(Get(Index));
end;

Procedure TBZ2DEdgeList.SetEdgeItem(index : Integer; val : TBZ2DLineTool);
begin
  Put(Index, Val);
end;

procedure TBZ2DEdgeList.Assign(source : TPersistent);
Var
  I : Integer;
  NewItem : TBZ2DLineTool;
Begin
  if (Source Is TBZ2DEdgeList) then
  begin
    If TBZ2DEdgeList(Source).Count > 0 then
    begin
      Clear;
      For I := 0 to TBZ2DEdgeList(Source).Count-1 do
      begin
        NewItem := TBZ2DLineTool.Create;
        NewItem.Assign(TBZ2DEdgeList(Source).Items[I]);
        AddEdge(NewItem);
      End;
    end;
  End
  else
    Inherited Assign(source);
end;

procedure TBZ2DEdgeList.WriteToFiler(writer : TVirtualWriter);
begin
  Inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0

  end;
end;

procedure TBZ2DEdgeList.ReadFromFiler(reader : TVirtualReader);
var
  archiveVersion: integer;
Begin
  Inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
  begin
    with reader do
    begin

    end;
  End
  else
    RaiseFilerException(archiveVersion);
  //for i := 0 to Count - 1 do Items[i].FOwner := Self;
end;

Procedure TBZ2DEdgeList.Clear;
Var
  i:  Integer;
  pm: TBZ2DLineTool;
Begin
  if Count < 1 then exit;
  For i := Count - 1 downTo 0 Do
  Begin
    pm := GetEdgeItem(i);
    //If Assigned(pm) Then
    FreeAndNil(pm); //.Free;
  End;
  Inherited Clear;
End;

function TBZ2DEdgeList.AddEdge(Edge : TBZ2DLineTool) : Integer;
begin
  Result := Add(Edge);
end;

procedure TBZ2DEdgeList.Sort;
begin
  inherited Sort(@CompareEdge);
end;

{%endregion%}

{%region=====[ TBZ2DPolyLineTool ]=========================================================================}

procedure TBZ2DPolyLineTool.SimplifyDist(InPts : TBZArrayOfFloatPoints; Tolerance : Single; out OutPts : TBZArrayOfFloatPoints);
Var
  KeepPoints : Array of Boolean;
  i, j, k : Integer;
  //SqTolerance : Single;
  {$CODEALIGN VARMIN=16}
  PrevPoint, CurrentPoint : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin
  //SqTolerance := Tolerance* Tolerance;
  k := InPts.Count;
  SetLength(KeepPoints{%H-}, k);
  KeepPoints[0] := True;
  KeepPoints[k-1] := True;
  For i := 2 to k-2 do KeepPoints[i] := False;
  PrevPoint  := InPts.Items[0];
  j := 1;
  For i := 1 to k-1 do
  begin
    CurrentPoint := PointsList.Items[i];
    //if (CurrentPoint.DistanceSquare(PrevPoint) > SqTolerance) then
    if (CurrentPoint.Distance(PrevPoint) > Tolerance) then
    begin
      KeepPoints[i] := True;
      PrevPoint := CurrentPoint;
      Inc(j);
    end;
  end;
  OutPts := TBZArrayofFloatPoints.Create(j);
  For i := 0 to k - 1 do
  begin
    if KeepPoints[i] then
    begin
      OutPts.Add(PointsList.Items[i]);
    end;
  end;
end;

procedure TBZ2DPolyLineTool.Internal_RamerDouglasPeucker(InPts : TBZArrayOfFloatPoints; Tolerance : Single; StartIndex, EndIndex : Integer; var KeepPoints : array of Boolean);
Var
  {$CODEALIGN VARMIN=16}
  p1, p2, p : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  aLine : TBZ2DLineTool;
  i, MaxIndex : Integer;
  MaxDist, Dist : Single;
begin
  MaxIndex := 0;
  MaxDist := -1.0;
  p1 := InPts.Items[StartIndex];
  p2 := InPts.Items[EndIndex];
  aLine := TBZ2DLineTool.Create;
  aLine.StartPoint := p1;
  aLine.EndPoint := p2;
  for i := StartIndex + 1 to EndIndex - 1 do
  begin
    p := InPts.Items[i];
    Dist := aLine.DistanceSegmentToPoint(p);

    if Dist > MaxDist then
    begin
      MaxIndex := i;
      MaxDist := Dist;
    end;
  end;
  FreeAndNil(aLine);

  if MaxDist > Tolerance then
  begin
    KeepPoints[MaxIndex] := True;

    if (MaxIndex - StartIndex) > 1 then Internal_RamerDouglasPeucker(InPts, Tolerance, StartIndex, MaxIndex, KeepPoints);
    if (EndIndex - MaxIndex) > 1 then Internal_RamerDouglasPeucker(InPts, Tolerance, MaxIndex, EndIndex, KeepPoints);
  end;
end;

procedure TBZ2DPolyLineTool.SimplifyRamerDouglasPeucker(InPts : TBZArrayOfFloatPoints; Tolerance : Single; out OutPts : TBZArrayOfFloatPoints);
Var
  KeepPoints : Array of Boolean;
  i, j, k : Integer;
  //SqTolerance : Single;
begin
  //SqTolerance := Tolerance * Tolerance;
  k := Inpts.Count;
  SetLength(KeepPoints{%H-}, k);
  KeepPoints[0] := True;
  KeepPoints[k-1] := True;
  For i := 2 to k-2 do KeepPoints[i] := False;

  Internal_RamerDouglasPeucker(InPts, Tolerance, 0, k, KeepPoints);

  j := 0;

  for i:= 0 to k - 1 do
  begin
    if KeepPoints[i] then Inc(j);
  end;

  OutPts := TBZArrayOfFloatPoints.Create(j);

  for i := 0 to k-1 do
  begin
    if KeepPoints[i] then
    begin
      OutPts.Add(InPts.Items[i]);
    end;
  end;
end;

constructor TBZ2DPolyLineTool.Create(aPoints : TBZArrayOfFloatPoints);
begin
  Inherited Create;
  FPoints := aPoints;
  FCount := FPoints.Count;
 // FStrokeMode := ssOuter;
  FInnerJoinType  := jsMitter;
  FOuterJoinType := jsMitter;
  FStartCap  := cmNone;
  FEndCap := cmNone;
  FStrokeWidth := 2;
  FStrokeMode := smAround;
  FClosePath := False;
end;

function TBZ2DPolyLineTool.BuildStroke : TBZArrayOfFloatPoints;
Type
  TStrokeLineInfos = packed record
    {$CODEALIGN RECORDMIN=16}
    LeftPerp  : Array[0..1] of TBZVector2f;
    RightPerp : Array[0..1] of TBZVector2f;
    StartPoint, EndPoint : TBZVector2f;
    LeftIntersectPoint : TBZVector2f;
    RightIntersectPoint : TBZVector2f;
    //Slope : Single;
    //Dir : Byte; // Dir : 0= LeftRight, 1= RigthLeft, 2= TopBottom, 3= BottomTop, 4=TopLeft, 5=TopRight, 6=BottomLeft, 7=BottomRight
    NormalizeDirection : TBZVector2f;
    {$CODEALIGN RECORDMIN=4}
  end;
  TArrayOfLines = Array of TBZ2DLineTool;
var
  Width : Single;
  //OutPoints : TBZArrayOfFloatPoints; // Closed path for polygon
  {$CODEALIGN VARMIN=16}
  LinesInfos : Array of TStrokeLineInfos;
  {$CODEALIGN VARMIN=4}

  Lines : TArrayOfLines;

  procedure FreeLines;
  var
    n, i : Integer;
  begin
    n := High(Lines);
    For i := n downto 0 do
    begin
      FreeAndNil(Lines[i]);
    end;
    Lines := nil;
  end;

  function ConvertPointsToLines : TArrayOfLines;
  Var
    n, i, k: Integer;

  Begin
    n := FCount;
    if not(FClosePath) then
    begin
      n := FCount-1;
    end
    else
    begin
      FPoints.Add(FPoints.Items[0]);
      FCount := FPoints.Count;
      n:= FCount -1;
    end;

    SetLength(Result,n);

    if (n=1) then
    begin
      Result[0] := TBZ2DLineTool.Create;
      Result[0].SetLineFromPoints(FPoints.Items[0], FPoints.Items[1]);
    end
    else
    begin
      For i := 0 To n-1 Do
      Begin
        k := (i + 1) Mod n;

        if not(FClosePath) then if k=0 then k:=n;
        Result[i] := TBZ2DLineTool.Create;
        Result[i].SetLineFromPoints(FPoints.Items[i], FPoints.Items[k]);
      End;
    end;
  End;

  procedure ComputeLinesInfos;
  Var
    n, i : Integer;
    CloseLeftPoint, CloseRightPoint : TBZVector2f;
    LineSlope : Single;
    LineA, LineB : TBZ2DLineTool;
  begin
    //GlobalLogger.LogNotice('========== COMPUTE LINE INFOS =============================');
    n := High(Lines)+1;

    Setlength(LinesInfos,n);
   // LineP := TLine2D.Create;

    if (n=1) then
    begin
      //GlobalLogger.LogNotice('Single Line');
      LinesInfos[0].StartPoint := Lines[0].StartPoint;
      LinesInfos[0].EndPoint   := Lines[0].EndPoint;
      LinesInfos[0].NormalizeDirection := Lines[0].GetNormalizeDirection;
      Lines[0].GetPerpendicularPointsAtStartPoint(Width,LinesInfos[0].LeftPerp[0],LinesInfos[0].RightPerp[0]);
      Lines[0].GetPerpendicularPointsAtEndPoint(Width,LinesInfos[0].LeftPerp[1],LinesInfos[0].RightPerp[1]);
      LinesInfos[0].LeftIntersectPoint := cNullVector2f;
      LinesInfos[0].RightIntersectPoint := cNullVector2f;

    end
    else
    begin

      for i := 0 to n-1 do
      begin

        LinesInfos[i].StartPoint := Lines[i].StartPoint;
        LinesInfos[i].EndPoint   := Lines[i].EndPoint;

        LinesInfos[i].NormalizeDirection := Lines[i].GetNormalizeDirection;
        //LinesInfos[i].Slope := Lines[i].getSlope;
        {if Lines[i].IsHorizontal then
        begin
          if (Lines[i].StartPoint.x < Lines[i].EndPoint.x) then LinesInfos[i].Dir := 0 // Left --> Right
          else LinesInfos[i].Dir := 1;  // Right --> Left
        end
        else if Lines[i].IsVertical then
        begin
          if (Lines[i].StartPoint.y < Lines[i].EndPoint.y) then LinesInfos[i].Dir := 2  // Top --> Bottom
          else LinesInfos[i].Dir := 3; // Bottom --> Top
        end
        else
        begin
          LineSlope := Lines[i].getSlope;
          if (LineSlope > 0) then
          begin
            if (Lines[i].StartPoint.x < Lines[i].EndPoint.x) then LinesInfos[i].Dir := 5  // Top --> Right
            else LinesInfos[i].Dir := 4;  // Top --> Left
          end
          else
          begin
            if (Lines[i].StartPoint.x < Lines[i].EndPoint.x) then LinesInfos[i].Dir := 7 // Bottom --> Right
            else LinesInfos[i].Dir := 6; // Bottom --> Left
          end;
        end; }

        Lines[i].GetPerpendicularPointsAtStartPoint(Width,LinesInfos[i].LeftPerp[0],LinesInfos[i].RightPerp[0]);
        Lines[i].GetPerpendicularPointsAtEndPoint(Width,LinesInfos[i].LeftPerp[1],LinesInfos[i].RightPerp[1]);
        LinesInfos[i].LeftIntersectPoint := cNullVector2f;
        LinesInfos[i].RightIntersectPoint := cNullVector2f;
      end;

      // Trouve les points d'intersection
      LineA := TBZ2DLineTool.Create;
      LineB := TBZ2DLineTool.Create;
      For i := 1 to n-1 do
      begin
        // Left
        LineA.SetLineFromPoints(LinesInfos[i-1].LeftPerp[0], LinesInfos[i-1].LeftPerp[1]);
        LineB.SetLineFromPoints(LinesInfos[i].LeftPerp[0], LinesInfos[i].LeftPerp[1]);
        LineA.GetInfiniteIntersectPoint(LineB,LinesInfos[i].LeftIntersectPoint);

        // Right
        LineA.SetLineFromPoints(LinesInfos[i-1].RightPerp[0], LinesInfos[i-1].RightPerp[1]);
        LineB.SetLineFromPoints(LinesInfos[i].RightPerp[0], LinesInfos[i].RightPerp[1]);
        LineA.GetInfiniteIntersectPoint(LineB,LinesInfos[i].RightIntersectPoint);
      end;

      if FClosePath then
      begin
        //Left
        LineA.SetLineFromPoints(LinesInfos[0].LeftPerp[0], LinesInfos[0].LeftPerp[1]);
        LineB.SetLineFromPoints(LinesInfos[n-1].LeftPerp[0], LinesInfos[n-1].LeftPerp[1]);
        LineA.GetInfiniteIntersectPoint(LineB,CloseLeftPoint);

        // Right
        LineA.SetLineFromPoints(LinesInfos[0].RightPerp[0], LinesInfos[0].RightPerp[1]);
        LineB.SetLineFromPoints(LinesInfos[n-1].RightPerp[0], LinesInfos[n-1].RightPerp[1]);
        LineA.GetInfiniteIntersectPoint(LineB,CloseRightPoint);

        LinesInfos[0].LeftPerp[0] := CloseLeftPoint;
        LinesInfos[0].RightPerp[0] := CloseRightPoint;
        LinesInfos[n-1].LeftPerp[1] := CloseLeftPoint;
        LinesInfos[n-1].RightPerp[1] := CloseRightPoint;
      end;
      FreeAndNil(LineB);
      FreeAndNil(LineA);
    end;
  end;

  function ComputeOuterStroke : TBZArrayOfFloatPoints;
  Var
    idx, n, i,nn : Integer;
    OutPoints :TBZArrayOfFloatPoints;
  begin
    nn := FCount * 2;
    OutPoints := TBZArrayOfFloatPoints.Create(nn);
    //SetLength(OutPoints,nn);
    //Case FJoinType of
    //  jtMitter : SetLength(OutPoints,nn);
    //  jtRounded :; //+??? points
    //  jtBevel :;  // + (FCount / 2) si around +((FCount/2)*2)
    //end;
   //GlobalLogger.LogNotice('Outer ComputeStroke');
    n := High(LinesInfos);

    // Premiers Points --> LIGNE OUVERTE
    //
    //OutPoints.Add(LinesInfos[0].StartPoint);
    OutPoints.Add(LinesInfos[0].RightPerp[0]);

    //GlobalLogger.LogNotice('FIRST POINT :');
    //GlobalLogger.LogStatus('START AT : ' + LinesInfos[0].StartPoint.ToString);
    //GlobalLogger.LogStatus('Right PERP : ' + LinesInfos[0].RightPerp[0].ToString);
    //
    //idx := 1;
    //for i := FCount-2 downto 0 do
    //begin
    //  OutPoints.Add(FPoints.Items[i]);
    //  GlobalLogger.LogStatus('---> ADD SOURCE PTS : ('+i.ToString +')'+FPoints.Items[i].ToString);
    //  //inc(idx);
    //end;
    //OutPoints.Add(LinesInfos[0].RightPerp[0]);

    for i:=1 to n do
    begin
      OutPoints.Add(LinesInfos[i].RightIntersectPoint);
      //GlobalLogger.LogStatus('---> Right PERP : ' + LinesInfos[i].RightIntersectPoint.ToString);
        //Case FJoinType of
        // jtMitter : OutPoints[Idx] := LinesInfos[i].LeftIntersectPoint;
        // jtRounded, jtBevel :
        // begin
        //
        // end;
        //end;
       //inc(idx);
    end;
    // Derniers Points
    OutPoints.Add(LinesInfos[n].RightPerp[1]);
    //inc(idx);
    OutPoints.Add(LinesInfos[n].EndPoint);
    for i := FCount-2 downto 0 do
    begin
      OutPoints.Add(FPoints.Items[i]);
      //GlobalLogger.LogStatus('---> ADD SOURCE PTS : ('+i.ToString +')'+FPoints.Items[i].ToString);
      //inc(idx);
    end;
   // OutPoints.Add(LinesInfos[0].RightPerp[0]);

    //inc(idx);
    //GlobalLogger.LogNotice('LAST POINT :');
    //GlobalLogger.LogStatus('RIGHT PERP : ' + LinesInfos[n].RightPerp[1].ToString);
    //GlobalLogger.LogStatus('END: ' + LinesInfos[n].EndPoint.ToString);

    //OutPoints.Add(LinesInfos[0].RightPerp[0]);
    Result := OutPoints;
  end;

  function ComputeInnerStroke : TBZArrayOfFloatPoints;
  Var
    idx, n, i,nn : Integer;
    OutPoints :TBZArrayOfFloatPoints;
  begin
    nn := FCount * 2;
    //SetLength(OutPoints,nn);
    OutPoints := TBZArrayOfFloatPoints.Create(nn);
    n := High(LinesInfos);

    // Premiers Points --> LIGNE OUVERTE
    OutPoints.Items[0] := LinesInfos[0].StartPoint;
    OutPoints.Items[1] := LinesInfos[0].RightPerp[0];

    idx := 2;
    for i:=1 to n do
    begin
       OutPoints.Items[Idx] := LinesInfos[i].RightIntersectPoint;
       inc(idx);
    end;
    // Derniers Points
    OutPoints.Items[idx] := LinesInfos[n].RightPerp[1];
    inc(idx);
    OutPoints.Items[idx] := LinesInfos[n].EndPoint;

    for i := FCount-2 downto 1 do
    begin
      inc(idx);
      OutPoints.Items[idx] := FPoints.Items[i];
    end;
    Result := OutPoints;
  end;

  function ComputeAroundStroke : TBZArrayOfFloatPoints;
  Var
    idx, n, i,nn : Integer;
    OutPoints :TBZArrayOfFloatPoints;
  begin
    nn := FCount * 2;
    OutPoints := TBZArrayOfFloatPoints.Create(nn);
    //SetLength(OutPoints,nn);
    n := High(LinesInfos);

    // Premiers Points --> LIGNE OUVERTE
    //OutPoints.Items[0] := LinesInfos[0].RightPerp[0];
    //OutPoints.Items[1] := LinesInfos[0].LeftPerp[0];

    OutPoints.Add(LinesInfos[0].RightPerp[0]);
    OutPoints.Add(LinesInfos[0].LeftPerp[0]);

    //GlobalLogger.LogNotice('FIRST POINT :');
    //GlobalLogger.LogStatus('LEFT PERP : ' + LinesInfos[0].LeftPerp[0].ToString);
    //GlobalLogger.LogStatus('RIGHT PERP : ' + LinesInfos[0].RightPerp[0].ToString);


    idx := 2;
    for i:=1 to n do
    begin
      //OutPoints.Items[Idx] := LinesInfos[i].LeftIntersectPoint;
      OutPoints.Add(LinesInfos[i].LeftIntersectPoint);
      inc(idx);
    end;

    // Points Liaison Outer/Inner
    //OutPoints.Items[idx] := LinesInfos[n].LeftPerp[1];
    OutPoints.Add(LinesInfos[n].LeftPerp[1]);
    inc(idx);
    //OutPoints.Items[idx] := LinesInfos[n].RightPerp[1];
    OutPoints.Add(LinesInfos[n].RightPerp[1]);
    inc(idx);

    //GlobalLogger.LogNotice('LAST POINT :');
    //GlobalLogger.LogStatus('LEFT PERP : ' + LinesInfos[n].LeftPerp[1].ToString);
    //GlobalLogger.LogStatus('RIGHT PERP : ' + LinesInfos[n].RightPerp[1].ToString);

    for i:=n downto 1 do
    begin
      //OutPoints.Items[Idx] := LinesInfos[i].RightIntersectPoint;
      OutPoints.Add(LinesInfos[i].RightIntersectPoint);
      inc(idx);
    end;
    Result := OutPoints;
  end;

begin

  //CountPoints := High(
  Lines := ConvertPointsToLines;

  Case FStrokeMode of
    smInner :
    begin
      Width := FStrokeWidth;
      ComputeLinesInfos;
      Result := ComputeInnerStroke;
    end;
    smOuter :
    begin
      Width := FStrokeWidth - 1;
      ComputeLinesInfos;
      Result := ComputeOuterStroke;
    end;
    smAround :
    begin
      Width := (FStrokeWidth * 0.5);
      ComputeLinesInfos;
      Result := ComputeAroundStroke;
    end;
  end;

  FreeLines;
  Setlength(LinesInfos,0);
  LinesInfos := nil;



end;

function TBZ2DPolyLineTool.GetEdgeList(var EdgeList : TBZ2DEdgeList) : Integer;
Var
  n, i, k: Integer;
  Edge : TBZ2DLineTool;
Begin
  Assert(Not(Assigned(EdgeList)), 'Vous devez initialiser EdgeList avant d''appeler cette méthode');
  if EdgeList.Count > 0 then EdgeList.Clear;
  n := FPoints.Count;
  if not(FClosePath) then
  begin
    n := n-1;
  end
  else
  begin
    FPoints.Add(FPoints.Items[0]);
    FCount := FPoints.Count;
    n:= FCount - 1;
  end;

  //SetLength(Result,n);

  if (n=1) then
  begin
    Edge := TBZ2DLineTool.Create;
    Edge.SetLineFromPoints(FPoints.Items[0], FPoints.Items[1]);
    EdgeList.AddEdge(Edge);
  end
  else
  begin
    For i := 0 To n-1 Do
    Begin
      k := (i + 1) Mod n;

      if not(FClosePath) then if k=0 then k:=n;
      Edge := TBZ2DLineTool.Create;
      Edge.SetLineFromPoints(FPoints.Items[i], FPoints.Items[k]);
      EdgeList.AddEdge(Edge);
    End;
  end;
  Result := EdgeList.Count;
end;

function TBZ2DPolyLineTool.GetBounds : TBZFloatRect;
Var
  minx, miny, maxx, maxy : Single;
  i : Integer;
begin
  minx := 5000; //Single.MaxValue;
  miny := 5000; //Single.MaxValue;
  maxx := 0;
  maxy := 0;
  for i := 0 to Self.Count - 1 do
  begin
    if Self.Points[i].x < minx then minx := Self.Points[i].x;
    if Self.Points[i].y < miny then miny := Self.Points[i].y;
    if Self.Points[i].x > maxx then maxx := Self.Points[i].x;
    if Self.Points[i].y > maxy then maxy := Self.Points[i].y;
  end;
  Result.Create(minx, miny, maxx, maxy);
end;

function TBZ2DPolyLineTool.SimplifyTo(const Tolerance : Single) : TBZArrayOfFloatPoints;
Var
  SimplifyDistPoints : TBZArrayOfFloatPoints;
begin
  SimplifyDist(PointsList, Tolerance, SimplifyDistPoints);
  SimplifyRamerDouglasPeucker(SimplifyDistPoints, Tolerance, Result);
  FreeAndNil(SimplifyDistPoints);
end;

procedure TBZ2DPolyLineTool.Simplify(const Tolerance : Single);
Var
  NewPoints : TBZArrayOfFloatPoints;
begin
  NewPoints := Self.SimplifyTo(Tolerance);
  PointsList := NewPoints;
end;

{%endregion%}

{%region=====[ TBZ2DCircleTool ]===========================================================================}

function TBZ2DCircleTool.GetPointOnCircumference(Angle : Single) : TBZFloatPoint;
Var
  {$CODEALIGN VARMIN=16}
  VSinCos, VRadius : TBZFLoatPoint;
  {$CODEALIGN VARMIN=16}
  AngRad : Single;
begin
  VRadius.Create(FRadius, FRadius);
  AngRad := DegToRadian(Angle);
  VSinCos.Create(Cos(AngRad), Sin(AngRad));
  Result :=  ( VRadius * VSinCos) + FCenter;
end;

function TBZ2DCircleTool.IsPointInCircle(pt : TBZFloatPoint) : Boolean;
var
  {$CODEALIGN VARMIN=16}
  p1 : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin
  p1 := FCenter - pt;
  p1 := p1.Abs;
  result := p1.LengthSquare < (FRadius * FRadius);
  //result := p1.Length < FRadius ;
end;

function TBZ2DCircleTool.IsPointOnCircle(pt : TBZFloatPoint) : Boolean;
var
  d : Single;
begin
  d := Abs(pt.Distance(FCenter));
  result := ( d =  FRadius);
end;

function TBZ2DCircleTool.GetTangentLineAtAngle(Angle, TangentLineLength : Single) : TBZ2DLineTool;
Var
  {$CODEALIGN VARMIN=16}
  p1,ps,pe : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  RadiusLine : TBZ2DLineTool;
  TangentLine : TBZ2DLineTool;
begin
  p1 := Self.GetPointOnCircumference(Angle);
  RadiusLine := TBZ2DLineTool.Create;
  RadiusLine.StartPoint := FCenter;
  RadiusLine.EndPoint := p1;
  TangentLine := TBZ2DLineTool.Create;
  RadiusLine.GetPerpendicularPointsAtEndPoint(TangentLineLength, ps, pe);
  TangentLine.StartPoint := ps;
  TangentLine.EndPoint := pe;
  Result := TangentLine;
end;

function TBZ2DCircleTool.GetArea : Single;
begin
{  The area of a circle is:
  (Pi) times the Radius squared:	A = PI r2
  or, when you know the Diameter:	A = (PI/4) × D2
  or, when you know the Circumference:	A = C2 / 4PI }

  Result := cPI * (FRadius * FRadius);

end;

function TBZ2DCircleTool.GetPerimeter : Single;
begin
  Result := cPI * (FRadius + FRadius);
end;

function TBZ2DCircleTool.LineIsChord(aLine : TBZ2DLineTool) : Boolean;
begin
  Result := False;
  if Self.IsPointOnCircle(aLine.StartPoint) and Self.IsPointOnCircle(aLine.EndPoint) then Result := True;
end;

function TBZ2DCircleTool.LineIsSecant(aLine : TBZ2DLineTool) : Boolean;
var
  {$CODEALIGN VARMIN=16}
  p1, p2 : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  Intersect : Boolean;
begin
  Result := False;
  if not(Self.LineIsChord(aLine)) then
  begin
    Intersect := aLIne.GetInfiniteIntersectPointsWithCircle(FCenter, FRadius, p1, p2);
    if Intersect then
    begin
      if Self.IsPointOnCircle(p1) and Self.IsPointOnCircle(p2) then Result := True;
    end;
  end;
end;

{%endregion%}

{%region=====[ TBZ2DPolygonTool ]==========================================================================}

Constructor TBZ2DPolygonTool.Create;
begin
  inherited Create;
  ////GlobalLogger.LogNotice('TBZ2DPolygonTool.Create');
  FRasters := nil;
  FRastersNeedUpdate := True;

  FStartY := -1;
  FFloatStartY :=-1.0;
  FRastersLineCount := -1;
end;

Destructor TBZ2DPolygonTool.Destroy;
begin
  ////GlobalLogger.LogNotice('TBZ2DPolygonTool.Destroy');
  if Assigned(FRasters) then
  begin
     ////GlobalLogger.LogNotice('---> Free FRasters');
     SetLength(FRasters, 0, 0);
     FRasters := nil;
  end;
  inherited Destroy;
end;

Procedure TBZ2DPolygonTool.NotifyChange(Sender : TObject);
begin
  ////GlobalLogger.LogNotice('TBZ2DPolygonTool.NotifyChange');
  if Assigned(FRasters) then
  begin
    SetLength(FRasters, 0, 0);
    FRasters := nil;

    FRastersNeedUpdate := True;
    FStartY := -1;
    FFloatStartY :=-1.0;
    FRastersLineCount := -1;
  end;
  inherited NotifyChange(Sender);
end;

function TBZ2DPolygonTool.GetEdgeList(Var EdgeList : TBZ2DEdgeList) : Integer;
Var
  n, i, k: Integer;
  Edge : TBZ2DLineTool;
Begin
  Assert(Not(Assigned(EdgeList)), 'Vous devez initialiser EdgeList avant d''appeler cette méthode');
  Assert((FPoints.Count<3), 'Une polygone ne peut pas avoir moins de 3 points');
  if EdgeList.Count > 0 then EdgeList.Clear;
  n := FPoints.Count-1;
  For i := 0 To n-1 Do
  Begin
    k := (i + 1) Mod n;
    Edge := TBZ2DLineTool.Create;
    Edge.SetLineFromPoints(FPoints.Items[i], FPoints.Items[k]);
    EdgeList.AddEdge(Edge);
  End;
  Result := EdgeList.Count;
end;

Function TBZ2DPolygonTool.getRastersList : TBZRastersList;
begin
  if (FRastersNeedUpdate) then
  begin
    //ComputeRasterBuckets; //AET;
    ComputeRasters;
    //ComputeComplexRasters;
  end;
  Result := FRasters;
end;

function TBZ2DPolygonTool.GetBounds : TBZFloatRect;
begin
  if (FRastersNeedUpdate) then
  begin
    //FRasters := ComputeRasters;
    FBounds := ComputeBounds;
  end;
  Result := FBounds;
end;

function TBZ2DPolygonTool.ComputeBounds : TBZFloatRect;
Var
  i, j: Integer;
Begin
  //GlobalLogger.LogNotice('TBZ2DPolygonTool.ComputeBounds');
  Result.Create(0, 0, 0, 0);
  j := FPoints.Count - 1;

  Result.Create(FPoints.Items[0].X, FPoints.Items[0].Y, FPoints.Items[0].X, FPoints.Items[0].Y);
  For i := 1 To J Do
  Begin
    If FPoints.Items[i].X < Result.Left Then Result.Left := FPoints.Items[i].X;
    If FPoints.Items[i].Y < Result.Top Then Result.Top := FPoints.Items[i].Y;
    If FPoints.Items[i].X > Result.Right Then Result.Right := FPoints.Items[i].X;
    If FPoints.Items[i].Y > Result.Bottom Then Result.Bottom := FPoints.Items[i].Y;
  End;
  FStartY := Round(Result.Top);
end;

procedure TBZ2DPolygonTool.ComputeRasters;
Var
  Y, I, K, X, TmpEdge, Idx,  CurrentLine : Integer;
  LineIntersect : TBZIntegerList;
  RasterLine : TBZRasterItems;
  {$CODEALIGN VARMIN=16}
  p1, p2, Diff: TBZFloatPoint;
  {$CODEALIGN VARMIN=4}

  procedure SwapPoint(var A, B : TBZFloatPoint);
  Var
    {$CODEALIGN VARMIN=16}
    Temp : TBZFloatPoint;
    {$CODEALIGN VARMIN=4}
  begin
    Temp := A;
    A := B;
    B := Temp;
  end;

begin
  FBounds := ComputeBounds;
  FRastersLineCount  := FBounds.AsRect.Height;
  FStartY := FBounds.AsRect.Top;
  LineIntersect := TBZIntegerList.Create(12);

  SetLength(FRasters, FRastersLineCount);

  CurrentLine := 0;
  for  Y := FBounds.AsRect.Top to FBounds.AsRect.Bottom do
  begin
    if (Y > FStartY) then LineIntersect.Clear;

    for I := 0 to FPoints.Count - 1 do
    begin

      p1 := FPoints.Items[I];
      if (I = (FPoints.Count - 1)) then p2 := FPoints.Items[0] else p2 := FPoints.Items[I + 1];

      // Prise en charge des lignes horizontales
      if (Y > FBounds.AsRect.Top) and (Y < FBounds.AsRect.Bottom) then
      begin
        if (Y = p1.Y) and (p1.Y = p2.Y) then
        begin
          if (p1.x = p2.x) then
          begin
            LineIntersect.Add(Round(p1.x));
            LineIntersect.Add(Round(p1.x));
            Continue;
          end
          else if (p1.x > p2.x) then
          begin
            LineIntersect.Add(Round(p2.x));
            LineIntersect.Add(Round(p1.x));
            Continue;
          end
          else //if (p1.x < p2.x) then
          begin
            LineIntersect.Add(Round(p1.x));
            LineIntersect.Add(Round(p1.x));
            LineIntersect.Add(Round(p2.x));
            LineIntersect.Add(Round(p2.x));
            Continue;
          end
        end;
      end;

      if (p1.y > p2.y) then SwapPoint(p1,p2);

      if ((y >= p1.y) And (y < p2.y)) or ((y = FBounds.AsRect.Bottom) And (y > p1.y) And (y <= p2.y)) then
      begin
        Diff := p2 - p1;
        X := Round(((y-p1.y) * (Diff.X / Diff.Y)) + p1.x);
        LineIntersect.Add(X);
      end;

    end;
    //Le trie QuickSort n'est pas "stable" on utilise donc un tri à bulle
    //Pour de grande quantité de point l'algorithme "MergeSort" serait plus indiqué
    For I := 0 to LineIntersect.Count-1 do
    begin
      Idx := I;
      For K := LineIntersect.Count-1 downto I do
      begin
        if LineIntersect.Items[K] <= LineIntersect.Items[Idx] then Idx := K;
        if (I<>Idx) then
        begin
          TmpEdge := LineIntersect.Items[I];
          LineIntersect.Items[I] := LineIntersect.Items[Idx];
          LineIntersect.Items[Idx] := TmpEdge;
        end;
      end;
    end;
    //LineIntersect.Sort(0,@CompareInteger);

    SetLength(RasterLine, (LineIntersect.Count shr 1) );
    I := 0;
    K := 0;
    While (I < LineIntersect.Count-1) do
    begin
      RasterLine[K].xStart := LineIntersect.Items[I];
      RasterLine[K].xEnd := LineIntersect.Items[I+1];
      Inc(K);
      Inc(I,2);
    end;
    FRasters[CurrentLine] := RasterLine;
    SetLength(RasterLine,0);
    Inc(CurrentLine);
  end;
  FreeAndNil(LineIntersect);
  FRastersNeedUpdate := False;
end;

Function TBZ2DPolygonTool.IsMonotoneVertical : Boolean;
//https://www.jagregory.com/abrash-black-book/#chapter-38-the-polygon-primeval
Var
  i, Len, DeltaYSign, PreviousDeltaYSign, NumYReversals : Integer;
begin
  //GlobalLogger.LogNotice('TBZ2DPolygonTool.IsMonotoneVertical');
  NumYReversals := 0;
  Result := False;
  Len := FPoints.Count;
  //GlobalLogger.LogStatus('Len = ' + Len.ToString);
  // Trois points ou moins ne peuvent pas créer un polygone non vertical monotone
  if ((Len) < 4) then
  begin
    Result := True;
    exit;
  end;
  // Recherche du premier segment non horizontal
  PreviousDeltaYSign := Sign(FPoints.Items[Len-1].Y - FPoints.Items[0].Y);
  //GlobalLogger.LogStatus('PreviousDeltaYSign = ' + PreviousDeltaYSign.ToString);
  i := 0;
  while ((PreviousDeltaYSign = 0) or (i < (Len-1))) do
  begin
    PreviousDeltaYSign := Sign(FPoints.Items[i].Y - FPoints.Items[i+1].Y);
    //GlobalLogger.LogStatus('Loop I = ' + I.ToString);
    //GlobalLogger.LogStatus('PreviousDeltaYSign = ' + PreviousDeltaYSign.ToString);
    inc(i);
  end;

  if (i = (Len-1)) then
  begin
    // Le polygone est une ligne "plate"
    Result := True;
    Exit;
  end;
  //GlobalLogger.LogNotice('Compte Inversion');
  // Maintenant on compte les inversions Y.
  // Peut manquer un retournement, au dernier sommet, parce que le nombre d'inversion doit être pair
  // Etre en retrait de 1 n'est pas un problème
  i := 0;
  PreviousDeltaYSign := 0;
  Result := True;  // C'est un polygone monotone vertical
  while (i < (Len-1)) or (Result = False) do
  begin
    DeltaYSign := Sign(FPoints.Items[i].Y - FPoints.Items[i+1].Y);
    //GlobalLogger.LogStatus('Loop I = ' + I.ToString);
    //GlobalLogger.LogStatus('DeltaYSign = ' +DeltaYSign.ToString);
    NumYReversals := 0;
    if (DeltaYSign <> 0) then
    begin
      if (DeltaYSign <> PreviousDeltaYSign) then
      begin
        // Direction Y inversée ; n'est Pas monotone vertical si Y est inversée plus de 3 fois
        if (NumYReversals > 2) then Result := False;
        Inc(NumYReversals);
        //GlobalLogger.LogStatus('NumYReversals = ' + NumYReversals.ToString);
        PreviousDeltaYSign := DeltaYSign;
        //GlobalLogger.LogStatus('PreviousDeltaYSign = ' + PreviousDeltaYSign.ToString);
      end;
    end;
    Inc(i);
  end;
end;

Function TBZ2DPolygonTool.GetRastersAtLine(Const Y : Integer) : TBZRasterItems;
Var
  PY : Integer;
begin
  Result := nil;
  if not(Assigned(FRasters)) or FRastersNeedUpdate then
  begin
    ComputeRasters;
  end;
  PY := Y - FStartY;
  if (Y>=0) and (Y<FRastersLineCount) then Result := FRasters[PY];
end;

Function TBZ2DPolygonTool.PointInside(Const pt : TBZFloatPoint) : Boolean;
Var
  RL: TBZRasterItems;
  i, j:  Integer;
Begin
  Result := False;
  RL := GetRastersAtLine(Round(Pt.Y));
  j := High(RL);
  For i := 0 To J Do
  Begin
    Result := (Pt.X >= RL[i].XStart) And (Pt.X <= RL[i].XEnd);
    If Result Then Exit;
  End;
end;

{ NOTE: last points of polygons must be the first one ( P1[0] = P1[N]  ; P2[0] = P2[N] ) }
function TBZ2DPolygonTool.IntersectWithPolygon(const P2: TBZ2DPolygonTool): Boolean;
var
  Poly1, Poly2 : TBZArrayOfFloatPoints; //@TODO utiliser directement TBZ2DPolygonTool ???. A voir
  I, J : Integer;
  xx , yy : Single;
  StartP, EndP : Integer;
  Found : Boolean;
  {$CODEALIGN VARMIN=16}
  Point2 : TBZVector2f;
  {$CODEALIGN VARMIN=4}

  { algorithm by Paul Bourke } // @TODO : Voir si TBZ2DPolygonTool.PointInside est plus rapide
  function PointInPolygon(const Pt: TBZVector2f; const Pg: TBZArrayOfFloatPoints): Boolean;
  var
    N, Counter , I : Integer;
    XInters : Real;
    P1, P2 : TBZVector2f;
  begin
    N := Pg.Count;
    Counter := 0;
    P1 := Pg.Items[0];
    for I := 1 to N do
    begin
      P2 := Pg.Items[I mod N];
      if Pt.y > Min(P1.y, P2.y) then
        if Pt.y <= Max(P1.y, P2.y) then
          if Pt.x <= Max(P1.x, P2.x) then
            if P1.y <> P2.y then
            begin
              XInters := (Pt.y - P1.y) * (P2.x - P1.x) / (P2.y - P1.y) + P1.x;
              if (P1.x = P2.x) or (Pt.x <= XInters) then Inc(Counter);
            end;
      P1 := P2;
    end;
    Result := (Counter mod 2 <> 0);
  end;

begin
  Found := False;
  { Find polygon with fewer points }
  if (Self.Count < P2.Count) then
  begin
    Poly1 := Self.PointsList;
    Poly2 := P2.PointsList;
  end
  else
  begin
    Poly1 := P2.PointsList;
    Poly2 := Self.PointsList;
  end;

  for I := 0 to Poly1.Count - 1 do
  begin
    { Trace new line }
    StartP := Round(Min(Poly1.Items[I].x, Poly1.Items[I+1].x));
    EndP   := Round(Max(Poly1.Items[I].x, Poly1.Items[I+1].x));


    if StartP = EndP then
    { A vertical line (ramp = inf) }
    begin
      xx := StartP;
      StartP := Round(Min(Poly1.Items[I].y, Poly1.Items[I+1].y));
      EndP   := Round(Max(Poly1.Items[I].y, Poly1.Items[I+1].y));
      { Follow a vertical line }
      for J := StartP to EndP do
      begin
        { line equation }
        Point2.X := Round(xx);
        Point2.Y := J;
        if PointInPolygon(Point2, Poly2) then
        begin
          Found := True;
          Break;
        end;
      end;
    end
    else
    { Follow a usual line (ramp <> inf) }
    begin
      { A Line which X is its variable i.e. Y = f(X) }
      if Abs(Poly1.Items[I].x -  Poly1.Items[I+1].x) >= Abs(Poly1.Items[I].y -  Poly1.Items[I+1].y) then
      begin
        StartP := Round(Min(Poly1.Items[I].x, Poly1.Items[I+1].x));
        EndP   := Round(Max(Poly1.Items[I].x, Poly1.Items[I+1].x));
        for J := StartP to EndP do
        begin
          xx := J;
          { line equation }
          yy := (Poly1.Items[I+1].y - Poly1.Items[I].y) / (Poly1.Items[I+1].x - Poly1.Items[I].x) * (xx - Poly1.Items[I].x) + Poly1.Items[I].y;
          Point2.X := Round(xx);
          Point2.Y := Round(yy);
          if PointInPolygon(Point2, Poly2) then
          begin
            Found := True;
            Break;
          end;
        end;
      end
      { A Line which Y is its variable i.e. X = f(Y) }
      else
      begin
        StartP := Round(Min(Poly1.Items[I].y, Poly1.Items[I+1].y));
        EndP   := Round(Max(Poly1.Items[I].y, Poly1.Items[I+1].y));
        for J := StartP to EndP do
        begin
          yy := J;
          { line equation }
          xx := (Poly1.Items[I+1].x - Poly1.Items[I].x) / (Poly1.Items[I+1].y - Poly1.Items[I].y) * (yy - Poly1.Items[I].y) + Poly1.Items[I].x;
          Point2.X := Round(xx);
          Point2.Y := Round(yy);
          if PointInPolygon(Point2, Poly2) then
          begin
            Found := True;
            Break;
          end;
        end;
      end;
    end;
    if Found then Break;
  end;

  { Maybe one polygon is completely inside another }
  if not Found then
    Found := PointInPolygon(Poly1.Items[0], Poly2) or PointInPolygon(Poly2.Items[0], Poly1);

  Result := Found;
end;

function TBZ2DPolygonTool.GetPerimeter : Single;
Var
  {$CODEALIGN VARMIN=16}
  P1, P2 : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  i : Integer;
  d : Single;
begin

  d := 0;
  For i := 0 to (FPoints.Count - 2) do
  begin
    P1 := FPoints.Items[i];
    P2 := FPoints.Items[i+1];
    d := d + P1.Distance(P2);
  end;
  P1 := FPoints.Items[FPoints.Count-1];
  P2 := FPoints.Items[0];
  d := d + P1.Distance(P2);
  Result := d;
end;

function TBZ2DPolygonTool.GetArea : Single;
Var
  Area : Single;
  i, j : Integer;
begin
   j := FPoints.Count - 1 ;
   Area := 0;
   for i := 0 to FPoints.Count - 1 do
   begin
     Area := Area + (FPoints.Items[j].X + FPoints.Items[i].x) * ( FPoints.Items[j].Y - FPoints.Items[i].Y);
     j:=i;
   end;
   Area := Area * 0.5;
   Result := Abs(Area)
end;

function TBZ2DPolygonTool.GetCentroid : TBZFloatPoint;
Var
  Area : Single;
  N, i,j:integer;
  Mag:Single;
  {$CODEALIGN VARMIN=16}
  P, D, M, A : TBZVector2f;
  {$CODEALIGN VARMIN=4}
begin
  P.Create(0,0);
  Area := 6 * Self.GetArea;
  D.Create(Area, Area);
  N := FPoints.Count;
  For i := 0 to N-1 do
  begin
    j:=(i + 1) mod N;
    Mag := (FPoints.Items[i].X * FPoints.Items[j].Y) - (FPoints.Items[j].X * FPoints.Items[i].Y);
    M.Create(Mag, Mag);
    A := FPoints.Items[i] +  FPoints.Items[j];
    P := (P + A) * M;
  end;
  P := P / D;
  Result := P;
end;

function TBZ2DPolygonTool.IntersectWithLine(aLine : TBZ2DLineTool; out IntersectPoints : TBZArrayOfFloatPoints) : Boolean;
Var
  OutPoints : TBZArrayOfFloatPoints;
  EdgeList : TBZ2DEdgeList;
  I, K : Integer;
  {$CODEALIGN VARMIN=16}
  PA, PB : TBZVector2f;
  {$CODEALIGN VARMIN=4}
  Intersect : TBZ2DLineIntersecType;
begin
  Result := False;
  EdgeList := TBZ2DEdgeList.Create;
  OutPoints := TBZArrayOfFloatPoints.Create(FPoints.Count);
  K := Self.GetEdgeList(EdgeList);
  for I := 0 to K-1 do
  begin
    Intersect := EdgeList.Items[I].GetIntersectPoint(aLine, PA, PB);
    if (Intersect = ilIntersect) or (Intersect = ilOverlap) then
    begin
      OutPoints.Add(PA);
      Result := True;
      if (Intersect = ilOverlap) then OutPoints.Add(PB);
    end;
  end;
  if Result then IntersectPoints := OutPoints
  else
  begin
    IntersectPoints := nil;
    FreeAndNil(OutPoints);
  end;
  FreeAndNil(EdgeList);
end;

Function TBZ2DPolygonTool.GetPolygonType : TBZ2DPolygonType;
Begin
  if not(IsComplex) then
  begin
    if IsConvex then Result := ptConvex else result := ptConcave;
  end
  else result := ptComplex;
End;

Function TBZ2DPolygonTool.IsConvex : Boolean;
var
  a: PtrInt;
  {$CODEALIGN VARMIN=16}
  PA, PB : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  i,i1, i2, n, nn : Integer;
  zCrossProduct : Single;
  Sign : Boolean;
Begin
  Result := True;
  n := FPoints.Count;

  if (n<4) then exit;// C'est un triangle donc forcément convex

  nn:=n-1; // Nombre de point a scanner
  n:=n-2; // Une polygone est fermé, on ignore le dernier point

  for i := 0 to n do
  begin
    i1 := (i + 1) mod nn;
    i2 := (i + 2) mod nn;
    PA := FPoints.Items[i2] -  FPoints.Items[i1];
    PB := FPoints.Items[i] -  FPoints.Items[i1];
    zCrossProduct := PA.Perp(PB);

    if (i=0) then
    begin
      if (zcrossproduct > 0) then
      begin
        Result := False;
        Exit;
      end;
    end
    else if (zcrossproduct < 0) then
    begin
      Result := False;
      Exit;
    end;
  end;
End;

Function TBZ2DPolygonTool.IsComplex : Boolean;
var
  EdgeList : TBZ2DEdgeList;
  I, J, K : Integer;
begin
  Result := False;
  EdgeList := TBZ2DEdgeList.Create;
  K := GetEdgeList(EdgeList);
  For I := 0 to K-1 do
  begin
    For J := 1 to K - 2 do
    begin
      if (I <> J) then
      begin
        if EdgeList.Items[I].IntersectWithLine(EdgeList.Items[J]) then
        begin
          Result := True;
          FreeAndNil(EdgeList);
          Exit;
        end;
      end;
    end;
  end;
end;

Function TBZ2DPolygonTool.IsConcave : Boolean;
begin
  Result := (Not(IsComplex) and Not(IsConvex));
end;

function TBZ2DPolygonTool.GetCenter : TBZFloatPoint;
Var
  i, n : Integer;
  {$CODEALIGN VARMIN=16}
  CenterPoint : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  d : Single;
begin
  n := FPoints.Count;
  d := n;
  n := n - 1;
  d := d.Reciprocal;
  CenterPoint.Create(0,0);
  For i:=0 to n do
  begin
    CenterPoint := CenterPoint + FPoints.Items[i];
  end;
  Result := CenterPoint * d;
end;

function TBZ2DPolygonTool.IsCounterClockWise : Boolean;
var
  i, j, k,n : Integer;
  {$CODEALIGN VARMIN=16}
  p1, p2, p3, d1, d2 : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin
  n := FPoints.Count;
  for i := 0 to n - 1 do
  begin
    if i < (n - 1) then j := i + 1 else j := 0;
    if i < (n - 2) then k := i + 2 else k := 1;
    p1 := FPoints.Items[j];
    p2 := FPoints.Items[k];
    p3 := FPoints.Items[i];
    d1 := p1 - p3;
    d2 := p2 - p3;
    if ((d1.x * d2.y) - (d1.y * d2.x)) < 0 then
    begin
       result := false;
       exit;
    end;
  end;
  result := true;
end;

function TBZ2DPolygonTool.OffsetPolygon(Ratio : Single) : TBZArrayOfFloatPoints;
var
  i, n : Integer;
  OffsetPoints : TBZArrayOfFloatPoints;
  {$CODEALIGN VARMIN=16}
  CenterPoint, NewPoint : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin
  n := FPoints.Count;
  CenterPoint := Self.GetCenter;
  OffsetPoints := TBZArrayOfFloatPoints.Create(n);
  n := n-1;
  for i := 0 to n do
  begin
    NewPoint := CenterPoint +  (FPoints.Items[i] * ratio);
    //NewPoint := (FPoints.Items[i] * ratio); // - ((CenterPoint * (Ratio * 0.5)) * 0.5);
    OffsetPoints.Add(NewPoint);
  end;
  Result := OffsetPoints;
end;

//function TBZ2DPolygonTool.IsMonotone : Boolean;
//Begin
//  Result := False;
//End;

{%endregion}

{%region=====( TBZ2DPolygonList ]==========================================================================}

Constructor TBZ2DPolygonList.Create;
begin
  inherited Create;
end;

Destructor TBZ2DPolygonList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

Function TBZ2DPolygonList.GetPolygonItem(index : Integer) : TBZ2DPolygonTool;
begin
  Result := TBZ2DPolygonTool(Get(Index));
end;

Procedure TBZ2DPolygonList.SetPolygonItem(index : Integer; val : TBZ2DPolygonTool);
begin
  Put(Index, Val);
end;

procedure TBZ2DPolygonList.Assign(source : TPersistent);
Var
  I : Integer;
  NewItem : TBZ2DPolygonTool;
Begin
  if (Source Is TBZ2DPolygonList) then
  begin
    If TBZ2DPolygonList(Source).Count > 0 then
    begin
      Clear;
      For I := 0 to TBZ2DPolygonList(Source).Count-1 do
      begin
        NewItem := TBZ2DPolygonTool.Create;
        NewItem.Assign(TBZ2DPolygonList(Source).Items[I]);
        AddPolygon(NewItem);
      End;
    end;
  End
  else
    Inherited Assign(source);
end;

procedure TBZ2DPolygonList.WriteToFiler(writer : TVirtualWriter);
begin
  inherited WriteToFiler(writer);
end;

procedure TBZ2DPolygonList.ReadFromFiler(reader : TVirtualReader);
begin
  inherited ReadFromFiler(reader);
end;

Procedure TBZ2DPolygonList.Clear;
begin
  inherited Clear;
end;

function TBZ2DPolygonList.AddPolygon(Polygon: TBZ2DPolygonTool) : Integer;
begin
  Result := Add(Polygon);
end;

{%endregion%}

{%region=====[ TBZ2DQuadraticBezierCurve ]=================================================================}

function TBZ2DQuadraticBezierCurve.ComputeSteps(Const Tolerance : Single) : Integer;
Var
 DistA, DistB, Dist : Single;
 {$CODEALIGN VARMIN=16}
 Diff : TBZFloatPoint;
 {$CODEALIGN VARMIN=4}
begin
  DistA  := FStartPoint.DistanceSquare(FControlPoint);
  DistB  := FControlPoint.DistanceSquare(FEndPoint);
  Dist   := Max(DistA,DistB);
  Result := round(System.Sqrt(System.Sqrt(Dist) / Tolerance));
  if Result<=0 then Result:=1;
end;

Constructor TBZ2DQuadraticBezierCurve.Create;
begin
  inherited Create;
  FStartPoint := cNullVector2f;
  FControlPoint := cNullVector2f;
  FEndPoint := cNullVector2f;
  FTolerance := 0.1;
end;

function TBZ2DQuadraticBezierCurve.ComputePointAt(t : single) : TBZFloatPoint;
var
  t1,coef1,coef2,t2: single;
begin
  t1:=  1-t;
  coef1 := t1 * t1;
  coef2 := t1 * (t + t);
  t2 := t*t;
  Result := (FStartPoint * Coef1) + (FControlPoint * Coef2) + (FEndPoint * t2);
end;

function TBZ2DQuadraticBezierCurve.ComputePolyLinePoints(Const nbStep : Integer) : TBZArrayOfFloatPoints;
var
  nbSteps,i : Integer;
  Delta : Single;
  aTime : Single;
  {$CODEALIGN VARMIN=16}
  APoint : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  Points : TBZArrayOfFloatPoints;
begin
  if nbStep <= 0 then nbSteps := ComputeSteps(FTolerance)
  else nbSteps := nbStep;

  if nbSteps > 1 then
  begin
    Points := TBZArrayOfFloatPoints.Create(nbSteps+1);

    Delta := 1 / nbSteps;
    aTime := 0;
    For i := 0 to nbSteps-1 do
    begin
      APoint := ComputePointAt(aTime);
      Points.Add(APoint);
      aTime := aTime + Delta;
    end;
    Points.Add(FEndPoint);
  end
  else
  begin
    Points := TBZArrayOfFloatPoints.Create(1);
    Points.Add(FStartPoint);
  end;
  Result := Points;
end;

procedure TBZ2DQuadraticBezierCurve.ComputeCoefficients(Out CoefA, CoefB : TBZFloatPoint);
begin
  CoefB := (FControlPoint - FStartPoint) * 2.0;
  CoefA := (FEndPoint - FStartPoint) - CoefB;
end;

procedure TBZ2DQuadraticBezierCurve.Split(out LeftCurve, RightCurve : TBZ2DQuadraticBezierCurve);
begin
  LeftCurve := Self.SplitAt(0.5,true);
  RightCurve := Self.SplitAt(0.5,false);
end;

{ https://stackoverflow.com/questions/37082744/split-one-quadratic-bezier-curve-into-two }
function TBZ2DQuadraticBezierCurve.SplitAt(Position : Single; FromStart : Boolean) : TBZ2DQuadraticBezierCurve;
Var
  CutOff : Single;
  {$CODEALIGN VARMIN=16}
  P0, P1, P2, PT : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  ACurve : TBZ2DQuadraticBezierCurve;
begin
  ACurve := TBZ2DQuadraticBezierCurve.Create;
  CutOff := Clamp(Position, 0, 1.0);

  if FromStart then
  begin
    PT := Self.ComputePointAt(CutOff);
    ACurve.StartPoint := Self.StartPoint;
    ACurve.EndPoint := PT;
  end
  else
  begin
    PT := Self.ComputePointAt(CutOff);
    ACurve.StartPoint := PT;
    ACurve.EndPoint := Self.EndPoint;
  end;
  P1 := Self.StartPoint; //ACurve.StartPoint;
  P2 := Self.ControlPoint;
  if FromStart then
  begin
    P0 := (P2 - P1) * CutOff;
    P1 := P1 + P0;
    ACurve.ControlPoint := P1;
    //P0 := (Self.EndPoint - P2) * CutOff;
    //P2 := P2 + P0;
    //P0 := (P2 - P1) * CutOff;
    //PT := P1 + P0;
    //ACurve.EndPoint := PT;
  end
  else
  begin
    P0 := (P2 - P1) * CutOff;
    P1 := P1 + P0;
    P0 := (Self.EndPoint - P2) * CutOff;
    P2 := P2 + P0;
    ACurve.ControlPoint := P2;
    //P0 := (P2 - P1) * CutOff;
    //PT := P1 + P0;
    //ACurve.StartPoint := PT;
  end;
  Result := ACurve;
end;

function TBZ2DQuadraticBezierCurve.GetLength : Single;
Var
  {$CODEALIGN VARMIN=16}
  Curve : TBZArrayOfFloatPoints;
  P1, P2 : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  i : Integer;
  d : Single;
begin
  Curve := Self.ComputePolyLinePoints;
  d := 0;
  For i := 0 to (Curve.Count - 2) do
  begin
    P1 := Curve.Items[i];
    P2 := Curve.Items[i+1];
    d := d + P1.Distance(P2);
  end;
  Result := d;
  FreeAndNil(Curve);
end;

function TBZ2DQuadraticBezierCurve.GetBounds : TBZFloatRect;
Var
  aPolyLine : TBZ2DPolyLineTool;
  Points : TBZArrayOfFloatPoints;
begin
  Points := Self.ComputePolyLinePoints;
  aPolyLine := TBZ2DPolyLineTool.Create(Points);
  Result := aPolyLine.GetBounds;
  FreeAndNil(aPolyLine);
  FreeAndNil(Points);
end;

function TBZ2DQuadraticBezierCurve.ConvertToCubic : TBZ2DCubicBezierCurve;
Const
  _Coef2 : Single = 2/3;
begin
  Result := TBZ2DCubicBezierCurve.Create;
  Result.StartPoint := FStartPoint;
  Result.ControlPointA := (FStartPoint * cInvThree) + (FControlPoint * _Coef2);
  Result.ControlPointB := (FControlPoint * _Coef2) + (FEndPoint * cInvThree);
  Result.EndPoint := FEndPoint;
end;

function TBZ2DQuadraticBezierCurve.GetDerivativeAt(t : Single) : TBZFloatPoint;
var
  f, tm, tm2 : Single;
  {$CODEALIGN VARMIN=16}
  t1,t2,t3 : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin

  //dt(t) /t := P1 * (2t-2) + (2*P3-4*P2) * t + 2 * P2

  tm := 1.0 - t;
  t1 := (FControlPoint - FStartPoint);
  t1 := (t1 + t1) * tm;
  t2 := (FEndPoint - FControlPoint);
  t2 := (t2 + t2) * t;
  Result := t1 + t2;
end;

function TBZ2DQuadraticBezierCurve.GetNormalAt(t : Single) : TBZFloatPoint;
Var
{$CODEALIGN VARMIN=16}
   dr : TBZFloatPoint;
{$CODEALIGN VARMIN=4}
begin
  dr := Self.getDerivativeAt(t);
  Dr := dr.Normalize;
  Result.X := -dr.Y;
  Result.Y := dr.X;
end;

function CreateQuadraticBezierCurve(StartPoint, ControlPoint, EndPoint : TBZFloatPoint) : TBZ2DQuadraticBezierCurve;
begin
  Result := TBZ2DQuadraticBezierCurve.Create;
  Result.StartPoint := StartPoint;
  Result.ControlPoint := ControlPoint;
  Result.EndPoint := EndPoint;
end;

{%endregion}

{%region=====[ TBZ2DCubicBezierCurve ]=====================================================================}

Constructor TBZ2DCubicBezierCurve.Create;
begin
  inherited Create;
  FControlPoints[0] := cNullVector2f;
  FControlPoints[1] := cNullVector2f;
  FControlPoints[2] := cNullVector2f;
  FControlPoints[3] := cNullVector2f;
  FTolerance := 0.1;
end;

function TBZ2DCubicBezierCurve.GetControlPoint(AIndex : Integer) : TBZFloatPoint;
begin
  Result := FControlPoints[AIndex];
end;

procedure TBZ2DCubicBezierCurve.SetControlPoint(AIndex : Integer; const AValue : TBZFloatPoint);
begin
  FControlPoints[AIndex] := AValue;
end;


function TBZ2DCubicBezierCurve.ComputeOptimalSteps(Const Tolerance : Single) : Integer;
Var
 DistA, DistB, DistC, Dist : Single;
 {$CODEALIGN VARMIN=16}
 Diff : TBZFloatPoint;
 {$CODEALIGN VARMIN=4}
begin
  DistA  := FControlPoints[0].DistanceSquare(FControlPoints[1]);
  DistB  := FControlPoints[1].DistanceSquare(FControlPoints[2]);
  DistC  := FControlPoints[2].DistanceSquare(FControlPoints[3]);
  Dist   := Max(DistA, Max(DistB,DistC));
  Result := round(System.Sqrt(System.Sqrt(Dist) / Tolerance) * 0.5) ;
  if Result<=0 then Result:=1;
end;

function TBZ2DCubicBezierCurve.ComputePointAt(t : single) : TBZFloatPoint;
Var
  ti1, t2, t3, ti2, ti3 : Single;
  {$CODEALIGN VARMIN=16}
  p,p1,p2,p3 : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin

  ti1 := 1.0 - t;
  t2 := t * t;
  t3 := t2 * t;
  ti2 := ti1 * ti1;
  ti3 := ti1 * ti2;

  p :=  FControlPoints[0] * ti3;
  p1 := FControlPoints[1] * (3.0 * ti2 * t);
  p2 := FControlPoints[2] * (3.0 * ti1 * t2);
  p3 := FControlPoints[3] * t3;
  p := p + (p1 + p2 + p3);

  Result := p;
end;

function TBZ2DCubicBezierCurve.ComputePolyLinePoints(Const nbStep : Integer) : TBZArrayOfFloatPoints;
var
  nbSteps,i : Integer;
  Delta : Single;
  aTime : Single;
  {$CODEALIGN VARMIN=16}
  APoint : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  Points : TBZArrayOfFloatPoints;
begin

  if nbStep <= 0 then nbSteps := ComputeOptimalSteps(FTolerance)
  else nbSteps := nbStep;

  if nbSteps = 1 then inc(nbSteps);

  Points := TBZArrayOfFloatPoints.Create(nbSteps+1);

  Delta := 1 / nbSteps;
  aTime := 0;
  For i := 0 to nbSteps-1 do
  begin
    APoint := ComputePointAt(aTime);
    Points.Add(APoint);
    aTime := aTime + Delta;
  end;
  Points.Add(FControlPoints[3]);

  Result := Points;
end;

function TBZ2DCubicBezierCurve.GetX(t : Single) : Single;
Var
  {$CODEALIGN VARMIN=16}
  PT : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin
  Pt := Self.ComputePointAt(t);
  Result := Pt.X;
end;

function TBZ2DCubicBezierCurve.GetY(t : Single) : Single;
Var
  {$CODEALIGN VARMIN=16}
  PT : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin
  Pt := Self.ComputePointAt(t);
  Result := Pt.Y;
end;

function TBZ2DCubicBezierCurve.GetTFromX(x : Single) : Single;
Var
  BBox : TBZFloatRect;
begin
  BBox := Self.GetBounds;
  if (x >= BBox.Left) and (x <= BBox.Right) then
    Result := x / BBox.Width;
end;

function TBZ2DCubicBezierCurve.GetYFromX(x : Single) : Single;
Var
  t : Single;
begin
  t := Self.GetTFromX(x);
  Result := GetY(t);
end;

procedure TBZ2DCubicBezierCurve.ComputeCoefficients(Out CoefA, CoefB, CoefC : TBZFloatPoint);
begin
 CoefC := (FControlPoints[1] - FControlPoints[0]) * 3.0;
 CoefB := ((FControlPoints[2] - FControlPoints[1]) * 3.0) - CoefC;
 CoefA := ((FControlPoints[3] - FControlPoints[0]) - CoefC) - CoefB;
end;

procedure TBZ2DCubicBezierCurve.Split(out LeftCurve, RightCurve : TBZ2DCubicBezierCurve);
begin
  LeftCurve := Self.SplitAt(0.5,true);
  RightCurve := Self.SplitAt(0.5,false);
end;

function TBZ2DCubicBezierCurve.SplitAt(Position : Single; FromStart : Boolean) : TBZ2DCubicBezierCurve;
Var
  CutOff : Single;
  {$CODEALIGN VARMIN=16}
  P0, P1, P2, P3, PT : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  ACurve : TBZ2DCubicBezierCurve;
begin
  ACurve := TBZ2DCubicBezierCurve.Create;
  CutOff := Clamp(Position, 0, 1.0);
  P1 := Self.StartPoint;
  if FromStart then
  begin
    PT := Self.ComputePointAt(CutOff);
    ACurve.StartPoint := FControlPoints[0];
    ACurve.EndPoint := PT;
  end
  else
  begin
    PT := Self.ComputePointAt(CutOff);
    ACurve.StartPoint := PT;
    ACurve.EndPoint := FControlPoints[3];
  end;
  P2 := FControlPoints[1];
  P3 := FControlPoints[2];
  if FromStart then
  begin
    P0 := (P2 - P1) * CutOff;
    P1 := P1 + P0;
    ACurve.ControlPointA := P1;

    P0 := (P3 - P2) * CutOff;
    P2 := P2 + P0;
    P2.X := P2.X + ((P3.X - P2.X) * CutOff);

    //P0 := (FEndPoint - P3) * CutOff;
    //P3 := P3 + P0;

    P0 := (P2 - P1) * CutOff;
    P1 := P1 + P0;
    ACurve.ControlPointB := P1;

    //P0 := (P2 - P1) * CutOff;
    //ACurve.EndPoint := P1 + P0;

  end
  else
  begin
    P0 := (P2 - P1) * CutOff;
    P1 := P1 + P0;

    P0 := (P3 - P2) * CutOff;
    P2 := P2 + P0;

    P0 := (FControlPoints[3] - P3) * CutOff;
    P3 := P3 + P0;
    ACurve.ControlPointB := P3;

    P0 := (P2 - P1) * CutOff;
    P1 := P1 + P0;

    P0 := (P3 - P2) * CutOff;
    P2 := P2 + P0;
    ACurve.ControlPointA := P2;

    //P0 := (P2 - P1) * CutOff;
    //P1 := P1 + P0;
    //ACurve.StartPoint := P1;

  end;
  Result := ACurve;
end;

function TBZ2DCubicBezierCurve.GetLength : Single;
Var
  {$CODEALIGN VARMIN=16}
  Curve : TBZArrayOfFloatPoints;
  P1, P2 : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  i : Integer;
  d : Single;
begin
  Curve := Self.ComputePolyLinePoints;
  d := 0;
  For i := 0 to (Curve.Count - 2) do
  begin
    P1 := Curve.Items[i];
    P2 := Curve.Items[i+1];
    d := d + P1.Distance(P2);
  end;
  Result := d;
  FreeAndNil(Curve);
end;

function TBZ2DCubicBezierCurve.GetBounds : TBZFloatRect;
Var
 aPolyLine : TBZ2DPolyLineTool;
 Points : TBZArrayOfFloatPoints;
begin
  Points := Self.ComputePolyLinePoints;
  aPolyLine := TBZ2DPolyLineTool.Create(Points);
  Result := aPolyLine.GetBounds;
  FreeAndNil(aPolyLine);
  FreeAndNil(Points);
end;

function TBZ2DCubicBezierCurve.GetDerivativeAt(t : Single) : TBZFloatPoint;
var
  f, tm, tm2 : Single;
  {$CODEALIGN VARMIN=16}
  t1,t2,t3 : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin

  // cubic derivative = dP(t) / dt =  -3(1-t)^2 * P0 +  3(1-t)^2 * P1
  //                                   - 6t(1-t) * P1 - 3t^2 * P2 + 6t(1-t) * P2
  //                                  + 3t^2 * P3


  tm := 1.0 - t;
  f := 3.0 * tm * tm;
  t1 := (FControlPoints[1] - FControlPoints[0]) * f;
  f := 6.0 * t * tm;
  t2 := (FControlPoints[2] - FControlPoints[1]) * f;
  f := 3.0 * t * t;
  t3 := (FControlPoints[3] - FControlPoints[2]) * f;

  Result := t1 + t2 +t3;
end;

function TBZ2DCubicBezierCurve.GetNormalAt(t : Single) : TBZFloatPoint;
Var
  {$CODEALIGN VARMIN=16}
   dr : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin
  dr := Self.getDerivativeAt(t);
  Dr := dr.Normalize;
  Result.X := -dr.Y;
  Result.Y := dr.X;
end;

function TBZ2DCubicBezierCurve.GetTangentAt(t : Single) : TBZFloatPoint;
begin
  Result := Self.getDerivativeAt(t);
  Result := Result.Normalize;
end;

function TBZ2DCubicBezierCurve.GetTangentAngleAt(t : Single) : Single;
begin
  Result := -1;
end;

procedure TBZ2DCubicBezierCurve.AlignToLine(lp1, lp2 : TBZFloatPoint);
var
  a, al, ab : Single;
  //SinA, CosA, lx, ly : Single;
  {$CODEALIGN VARMIN=16}
  Pivot, OffsetA, OffsetB : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  aLine : TBZ2DLineTool;

  //procedure Rotate(var pt : TBZFloatPoint);
  //begin
  //  OffsetA := pt - lp1;
  //  pt.X := (Dist.X * CosA) - (Dist.Y * SinA);
  //  pt.Y := (Dist.X * SinA) + (Dist.Y * CosA);
  //end;

begin
  aLine := TBZ2DLineTool.Create;
  aLine.SetLineFromPoints(lp1, lp2);

  //Dist := (lp2 - lp1); //.Abs;
  //a := aLine.getSlope; //-Math.ArcTan2(Dist.Y, Dist.X);
  al := aLine.StartPoint.AngleBetweenPointsInDeg(aLine.EndPoint);
  ab := FControlPoints[0].AngleBetweenPointsInDeg(FControlPoints[3]);
  a := (ab - al);
  Pivot := Self.ComputePointAt(0.5);
  FControlPoints[0] := FControlPoints[0].Rotate(a, Pivot);
  FControlPoints[1] := FControlPoints[1].Rotate(a, Pivot);
  FControlPoints[2] := FControlPoints[2].Rotate(a, Pivot);
  FControlPoints[3] := FControlPoints[3].Rotate(a, Pivot);

  aLine.DistanceLineToPoint(FControlPoints[0], OffsetA);
  aLine.DistanceLineToPoint(FControlPoints[3], OffsetB);
  OffsetA := OffsetA.Min(OffsetB);
  FControlPoints[0] := FControlPoints[0] - OffsetA;
  FControlPoints[1] := FControlPoints[1] - OffsetA;
  FControlPoints[2] := FControlPoints[2] - OffsetA;
  FControlPoints[3] := FControlPoints[3] - OffsetA;
  FreeAndNil(aLine);
  //CosA := System.cos(a);
  //SinA := System.Sin(a);
  //Rotate(FStartPoint);
  //Rotate(FControlPointA);
  //Rotate(FControlPointB);
  //Rotate(FEndPoint);
end;

{%endregion}

{%region=====( TBZCubicBezierCurves ]======================================================================}

Constructor TBZCubicBezierCurves.Create;
begin
  inherited Create;
end;

Destructor TBZCubicBezierCurves.Destroy;
begin
  Clear;
  inherited Destroy;
end;

Function TBZCubicBezierCurves.GetCubicBezierCurveItem(index : Integer) : TBZ2DCubicBezierCurve;
begin
  Result := TBZ2DCubicBezierCurve(Get(Index));
end;

Procedure TBZCubicBezierCurves.SetCubicBezierCurveItem(index : Integer; val : TBZ2DCubicBezierCurve);
begin
  Put(Index, Val);
end;

procedure TBZCubicBezierCurves.Assign(source : TPersistent);
Var
  I : Integer;
  NewItem : TBZ2DCubicBezierCurve;
Begin
  if (Source Is TBZCubicBezierCurves) then
  begin
    If TBZCubicBezierCurves(Source).Count > 0 then
    begin
      Clear;
      For I := 0 to TBZCubicBezierCurves(Source).Count-1 do
      begin
        NewItem := TBZ2DCubicBezierCurve.Create;
        NewItem.Assign(TBZCubicBezierCurves(Source).Items[I]);
        AddCurve(NewItem);
      End;
    end;
  End
  else
    Inherited Assign(source);
end;

procedure TBZCubicBezierCurves.WriteToFiler(writer : TVirtualWriter);
begin
  inherited WriteToFiler(writer);
end;

procedure TBZCubicBezierCurves.ReadFromFiler(reader : TVirtualReader);
begin
  inherited ReadFromFiler(reader);
end;

Procedure TBZCubicBezierCurves.Clear;
begin
  inherited Clear;
end;

function TBZCubicBezierCurves.AddCurve(aCurve: TBZ2DCubicBezierCurve) : Integer;
begin
  Result := Add(aCurve);
end;

{%endregion%}

{%region=====[ TBZ2DCurve ]================================================================================}

Constructor TBZ2DCurve.Create(aCurveType : TBZCurveType);
begin
  Inherited Create;
  FCurveType := aCurveType;
end;

Constructor TBZ2DCurve.Create(aCurveType : TBZCurveType; aControlPoints : TBZArrayOfFloatPoints);
begin
  Create(aCurveType);
  Self.AssignPoints(aControlPoints);
end;

function TBZ2DCurve.ComputeUniformSplineCurve : TBZArrayOfFloatPoints;
begin

end;

function TBZ2DCurve.ComputeCatmullRomSplineCurve : TBZArrayOfFloatPoints;
begin

end;

function TBZ2DCurve.ComputeSmoothBezierCurve : TBZArrayOfFloatPoints;
Var
  i,j, k  : Integer;
  OutControlPoints, OutAnchorPoints, RenderingPoints : TBZArrayOfFloatPoints;
  CubicCurve : TBZ2DCubicBezierCurve;
  {$CODEALIGN VARMIN=16}
  p0, p1, p2, p3, p4, tn : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  d, d2 : single;
begin

  if (PointsList.Count < 2) then exit;

  CubicCurve := TBZ2DCubicBezierCurve.Create;
  RenderingPoints := TBZArrayOfFloatPoints.Create(32);
  OutControlPoints := TBZArrayOfFloatPoints.Create(32);
  OutAnchorPoints := TBZArrayOfFloatPoints.Create(32);
  Result := TBZArrayOfFloatPoints.Create(32);

  // Controls points
  for i := 0 to (Self.PointsList.Count - 2) do
  begin
    p0 := Self.Points[i];
    p1 := Self.Points[i + 1];
    tn := p1 - p0;
    p1 := tn / 3;
    p2 := (tn + tn) / 3;
    p3 := p0 + p1;
    p4 := p0 + p2;
    OutControlPoints.Add(p3);
    OutControlPoints.Add(p4);
  end;
  // if FClosed then
  //begin
  //end;

  // Anchor Points
  //if FClosed then
  //begin
    //p0 := OutControlPoints.Items[0];
  //end;
  //else
  OutAnchorPoints.Add(Self.Points[0]);
  j := 1;
  for i := 0 to  (Self.PointsList.Count - 3) do
  begin
    p1 := OutControlPoints.Items[j];
    p2 := OutControlPoints.Items[j + 1];
    p3 := (p1 + p2) * 0.5;
    OutAnchorPoints.Add(p3);
    inc(j, 2);
  end;
  // if FClosed then OutAnchorPoints.Add(Self.Points[0])
  //else
  OutAnchorPoints.Add(Self.Points[(Self.PointsList.Count - 1)]);

  // Update curve control points
  j := 0;
  for i := 0 to  (OutAnchorPoints.Count - 2) do
  begin
    //Result.Add(OutAnchorPoints.Items[i]);
    //Result.Add(OutControlPoints.Items[j]);
    //Result.Add(OutControlPoints.Items[j + 1]);
    //Result.Add(OutAnchorPoints.Items[i + 1]);
    RenderingPoints.Clear;
    CubicCurve.StartPoint := OutAnchorPoints.Items[i];
    CubicCurve.ControlPointA := OutControlPoints.Items[j];
    CubicCurve.ControlPointB := OutControlPoints.Items[j + 1];
    CubicCurve.EndPoint := OutAnchorPoints.Items[i + 1];
    RenderingPoints := CubicCurve.ComputePolyLinePoints();
    for k := 0 to RenderingPoints.Count - 1 do
    begin
      Result.Add(RenderingPoints.Items[k]);
    end;
    inc(j, 2);
  end;

  FreeAndNil(RenderingPoints);
  FreeAndNil(OutAnchorPoints);
  FreeAndNil(OutControlPoints);
  FreeAndNil(CubicCurve);
end;

function TBZ2DCurve.ComputeBezierSplineCurve: TBZArrayOfFloatPoints;
Var
  i,j, n  : Integer;
  OutControlPointsA, OutControlPointsB, OutAnchorPoints : TBZArrayOfFloatPoints;
  TmpA, TmpB, TmpC, TmpR : TBZArrayOfFloatPoints;
  RenderingPoints : TBZArrayOfFloatPoints;
  {$CODEALIGN VARMIN=16}
  p1, p2, p3, p4 : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  m : Single;
begin
  n := Self.PointsList.Count - 1;

  if (n < 2) then exit;

  TmpA := TBZArrayOfFloatPoints.Create(n);
  TmpB := TBZArrayOfFloatPoints.Create(n);
  TmpC := TBZArrayOfFloatPoints.Create(n);
  TmpR := TBZArrayOfFloatPoints.Create(n);

  p1.Create(1.0, 1.0);
  p2.Create(4.0, 4.0);
  p3.Create(1.0, 1.0);
  p4 := Self.Points[0] + (Self.Points[1]  + Self.Points[1]);
  TmpA.Add(p1);
  TmpB.Add(p2);
  TmpC.Add(p3);
  TmpR.Add(p4);

  for i := 1 to (n - 2) do
  begin
    p1.Create(1.0, 1.0);
    p2.Create(4.0, 4.0);
    p3.Create(1.0, 1.0);
    p4 := (Self.Points[i] * 4) + (Self.Points[i + 1]  + Self.Points[i + 1]);
    TmpA.Add(p1);
    TmpB.Add(p2);
    TmpC.Add(p3);
    TmpR.Add(p4);
  end;

  p1.Create(2.0, 2.0);
  p2.Create(7.0, 7.0);
  p3.Create(0.0, 0.0);
  p4 := (Self.Points[n - 1] * 8) + Self.Points[n];
  TmpA.Add(p1);
  TmpB.Add(p2);
  TmpC.Add(p3);
  TmpR.Add(p4);

  for i := 1 to (n - 1) do
  begin
    p1 := TmpA.Items[i] / TmpB.Items[i-1];
    TmpB.Items[i] := TmpB.Items[i] - (p1 * TmpC.Items[i - 1]);
    TmpR.Items[i] := TmpR.Items[i] - (p1 * TmpR.Items[i - 1]);
  end;

  p1.Create(0,0);
  OutControlPointsA := TBZArrayOfFloatPoints.Create(n);
  for i := 0 to (n - 1) do
  begin
    OutControlPointsA.Add(p1);
  end;
  p4 := TmpR.Items[N - 1] /  TmpB.Items[N - 1];
  OutControlPointsA.Items[(n - 1)] := p4;

  for i := (n - 2) downto 0  do
  begin
    p1 := (TmpR.Items[i] - (TmpC.Items[i] * OutControlPointsA.Items[i + 1])) / TmpB.Items[i];
    OutControlPointsA.Items[i] := p1;
  end;

  OutControlPointsB := TBZArrayOfFloatPoints.Create(n);
  for i := 0  to (n - 1) do
  begin
    p1 := Self.Points[i + 1];
    p2 := (p1 + p1) - OutControlPointsA.Items[i + 1];
    OutControlPointsB.Add(p2);
  end;

  p1 :=  (Self.Points[N] + OutControlPointsA.Items[N - 1]) * 0.5;
  OutControlPointsB.Add(p1);




  RenderingPoints := TBZArrayOfFloatPoints.Create(32);
  Result := TBZArrayOfFloatPoints.Create(32);

  FreeAndNil(OutControlPointsB);
  FreeAndNil(OutControlPointsA);
  FreeAndNil(TmpR);
  FreeAndNil(TmpC);
  FreeAndNil(TmpB);
  FreeAndNil(TmpA);
end;

function TBZ2DCurve.ComputeBezierCurve : TBZArrayOfFloatPoints;
Var
  OutControlPoints, RenderingPoints : TBZArrayOfFloatPoints;
  QuadraticCurve : TBZ2DQuadraticBezierCurve;

  CubicCurve : TBZ2DCubicBezierCurve;
  {$CODEALIGN VARMIN=16}
  p0, p1, p2, p3 : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
  i, j : Integer;
begin
  QuadraticCurve := TBZ2DQuadraticBezierCurve.Create;
  CubicCurve := TBZ2DCubicBezierCurve.Create;
  if (odd(Self.PointsList.Count)) then Self.PointsList.Add(Self.Points[(Self.PointsList.Count - 1)]);

  RenderingPoints := TBZArrayOfFloatPoints.Create(32);
  OutControlPoints := TBZArrayOfFloatPoints.Create(32);
  Result := TBZArrayOfFloatPoints.Create(32);

  i := 0;
  While (i < (Self.PointsList.Count - 2)) do
  begin
    p0 := Self.Points[i];
    p1 := Self.Points[i + 1];
    p2 := Self.Points[i + 2];
 		if ( (i + 3) > (Self.PointsList.Count - 1) ) then
    begin
      QuadraticCurve.StartPoint := p0;
      QuadraticCurve.ControlPoint := p1;
      QuadraticCurve.EndPoint := p2;
      RenderingPoints := QuadraticCurve.ComputePolyLinePoints();
      for j := 0 to RenderingPoints.Count - 1 do
      begin
        Result.Add(RenderingPoints.Items[j]);
      end;
      inc(i, 2);
    end
    else
    begin
      p3 := Self.Points[i + 3];
      CubicCurve.StartPoint := p0;
      CubicCurve.ControlPointA := p1;
      CubicCurve.ControlPointB := p2;
      CubicCurve.EndPoint := p3;
      RenderingPoints := CubicCurve.ComputePolyLinePoints();
      for j := 0 to RenderingPoints.Count - 1 do
      begin
        Result.Add(RenderingPoints.Items[j]);
      end;
      inc(i, 3);
    end;
  end;

  FreeAndNil(RenderingPoints);
  FreeAndNil(OutControlPoints);
  FreeAndNil(CubicCurve);
  FreeAndNil(QuadraticCurve);
end;

function TBZ2DCurve.ComputePolylinePoints : TBZArrayOfFloatPoints;
begin
  Case FCurveType of
    ctBezier: Result := ComputeBezierCurve;
    ctSmoothBezier : Result := ComputeSmoothBezierCurve;
    ctBezierSpline : Result := ComputeBezierSplineCurve;
    ctUniformSpline: ;
    ctCatmullRomSpline: ;
  end;
end;


//for i := 1 to (PointsList.Count - 2) do
    //p0 := Self.Points[i - 1];
    //p1 := Self.Points[i];
    //p2 := Self.Points[i + 1];
    //tn := (p2 - p0).Normalize;
    //p3 := p1 - (tn *  (p0.Distance(p1) * 0.25)); //(p1 - p0).Length;
    //p4 := p1 + (tn *  (p1.Distance(p2) * 0.25)); //(p2 - p1).Length;
    //GlobalLogger.LogNotice('Set control point at ' + i.ToString);
    //OutControlPoints.Add(p3);
    //OutControlPoints.Add(p4);
    ////OutControlPoints.Add(p2);
    //if  (Self.PointsList.Count > 3) and ((i + 1) < (Self.PointsList.Count - 1)) then
    //begin
    //  //if (i > 1) and (i < (PointsList.Count - 2)) then
    //  OutControlPoints.Add(Self.Points[i + 1]);
    //end;


//OutControlPoints.Add(Self.Points[(Self.PointsList.Count - 1)]);
//GlobalLogger.LogNotice('Set control point Count ' + OutControlPoints.Count.ToString);
//i := 0;
//While (i < (OutControlPoints.Count - 2)) do
//  begin
//    p0 := OutControlPoints.Items[i];
//    p1 := OutControlPoints.Items[i + 1];
//    p2 := OutControlPoints.Items[i + 2];
// 		if ( (i + 3) > (Self.PointsList.Count - 1) ) then
//    begin
//      QuadraticCurve.StartPoint := p0;
//      QuadraticCurve.ControlPoint := p1;
//      QuadraticCurve.EndPoint := p2;
//      RenderingPoints := QuadraticCurve.ComputePolyLinePoints();
//      for j := 0 to RenderingPoints.Count - 1 do
//      begin
//        Result.Add(RenderingPoints.Items[j]);
//      end;
//      //inc(i, 2);
//    end
//    else
//    begin
//      p3 := OutControlPoints.Items[i + 3];
//      CubicCurve.StartPoint := p0;
//      CubicCurve.ControlPointA := p1;
//      CubicCurve.ControlPointB := p2;
//      CubicCurve.EndPoint := p3;
//      RenderingPoints := CubicCurve.ComputePolyLinePoints();
//      for j := 0 to RenderingPoints.Count - 1 do
//      begin
//        Result.Add(RenderingPoints.Items[j]);
//      end;
//      //inc(i, 3);
//    end;
//    inc(i, 4);
//  end;

//for j := 0 to OutControlPoints.Count - 1 do
//begin
//  Result.Add(OutControlPoints.Items[j]);
//end;

//PointsList.Clear;
//Self.AssignPoints(OutControlPoints);
//Result := OutControlPoints; //ComputeContiniousBezierCurve;

//OutControlPoints.Add(Self.Points[0]);
// i := 1;
//While (i <= (Self.PointsList.Count - 1)) do
// begin
//   OutControlPoints.Add(Self.Points[i - 1].Center(Self.Points[i]));
//   OutControlPoints.Add(Self.Points[i]);
//   OutControlPoints.Add(Self.Points[i + 1]);
//
//	if ( (i + 2) < (Self.PointsList.Count - 1) ) then
//   begin
//     OutControlPoints.Add(Self.Points[i + 1].Center(Self.Points[i + 2]));
//   end;
//
//   inc(i, 2);
// end;
//
// i := 0;
// While (i < (OutControlPoints.Count - 2)) do
//   begin
//     p0 := OutControlPoints.Items[i];
//     p1 := OutControlPoints.Items[i + 1];
//     p2 := OutControlPoints.Items[i + 2];
//  		if ( (i + 3) > (Self.PointsList.Count - 1) ) then
//     begin
//       QuadraticCurve.StartPoint := p0;
//       QuadraticCurve.ControlPoint := p1;
//       QuadraticCurve.EndPoint := p2;
//       RenderingPoints := QuadraticCurve.ComputePolyLinePoints();
//       for j := 0 to RenderingPoints.Count - 1 do
//       begin
//         Result.Add(RenderingPoints.Items[j]);
//       end;
//       //inc(i, 2);
//     end
//     else
//     begin
//       p3 := OutControlPoints.Items[i + 3];
//       CubicCurve.StartPoint := p0;
//       CubicCurve.ControlPointA := p1;
//       CubicCurve.ControlPointB := p2;
//       CubicCurve.EndPoint := p3;
//       RenderingPoints := CubicCurve.ComputePolyLinePoints();
//       for j := 0 to RenderingPoints.Count - 1 do
//       begin
//         Result.Add(RenderingPoints.Items[j]);
//       end;
//       //inc(i, 3);
//     end;
//     inc(i, 4);
//   end;




{%endregion%}

{%region=====[ Globale ]===================================================================================}

function CreateCubicBezierCurve(StartPoint, ControlPointA, ControlPointB, EndPoint : TBZFloatPoint) : TBZ2DCubicBezierCurve;
begin
  Result := TBZ2DCubicBezierCurve.Create;
  Result.StartPoint := StartPoint;
  Result.ControlPointA := ControlPointA;
  Result.ControlPointB := ControlPointB;
  Result.EndPoint := EndPoint;
end;

procedure BuildPolyCircle(ptc : TBZFloatPoint; Radius : Single; out ArrayOfPoints : TBZArrayOfFloatPoints; const Steps : Integer);
Var
  nbSteps : Integer;
  Delta : Single;
  i : Integer;
  {$CODEALIGN VARMIN=16}
  pt, pd : TBZFloatPoint;
  sc : TBZFloatPoint;
  pr : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}

  function ComputeSteps : Integer;
  var
    R: Single;
  begin
    R := Abs(Radius);
    Result := Trunc(cPi / (Math.ArcCos(R / (R + 0.125))));
  end;

begin
  if (Steps = -1) or (Steps < 3) then nbSteps := ComputeSteps
  else nbSteps := Steps;

  Delta := c2PI / nbSteps;
  sc.Create(System.Cos(Delta), System.Sin(Delta));
  ArrayOfPoints := TBZArrayOfFloatPoints.Create(nbSteps);

  pt.Create(Radius + ptc.X, ptc.Y);
  ArrayofPoints.Add(pt);
  pr.Create(Radius, Radius);

  pt := pr * sc;
  pd := pt + ptc;
  ArrayofPoints.Add(pd);
  pt := sc;
  for i := 2 to nbSteps - 1 do
  begin
    pt.Create(pt.X * sc.X - pt.Y * sc.Y, pt.Y * sc.X + pt.X * sc.Y);
    pd := (pr * pt) + ptc;
    ArrayofPoints.Add(pd);
  end;

end;

procedure BuildPolyEllipse(ptc : TBZFloatPoint; RadiusX, RadiusY : Single; out ArrayOfPoints : TBZArrayOfFloatPoints; Const Steps : Integer = -1);
Var
  nbSteps : Integer;
  Delta, Theta, SqrtRadius : Single;
  i : Integer;
  {$CODEALIGN VARMIN=16}
  pt, pd : TBZFloatPoint;
  sc : TBZFloatPoint;
  pr : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}

  function ComputeSteps : Integer;
  var
    R: Single;
  begin
    R := (Abs(RadiusX) + Abs(RadiusY)) * 0.5 ;
    Result := Trunc((cPi / (Math.ArcCos(R / (R + 0.125)))));// * 2);
  end;
begin
  if (Steps = -1) or (Steps < 3) then nbSteps := ComputeSteps
  else nbSteps := Steps;
  Delta := c2PI / nbSteps;
  sc.Create(System.Cos(Delta), System.Sin(Delta));

  //SqrtRadius := Sqrt(RadiusX/RadiusY);

  ArrayOfPoints := TBZArrayOfFloatPoints.Create(nbSteps);

  pt.Create(RadiusX + ptc.X, ptc.Y);
  ArrayofPoints.Add(pt);
  pr.Create(RadiusX, RadiusY);

  pt := pr * sc;
  pd := pt + ptc;
  ArrayofPoints.Add(pd);
  pt := sc;

  pt.Create(pt.X * sc.X - pt.Y * sc.Y, pt.Y * sc.X + pt.X * sc.Y);
  for i := 2 to nbSteps - 1 do
  begin
    //Delta := cPiDiv2 * i / nbSteps;
    //Theta := cPiDiv2 - System.ArcTan(Math.Tan(Delta) * SqrtRadius);
    //sc.Create(System.Cos(Theta), System.Sin(Theta));
    //pd := (pr * sc) + ptc; // Return NAN ????????
    //ArrayofPoints.Add(pd);

    pd := (pr * pt) + ptc;
    ArrayofPoints.Add(pd);
  end;
end;

procedure BuildPolyArc(cx, cy, rx, ry, StartAngle, EndAngle : Single; const ClockWise : Boolean; out ArrayOfPoints : TBZArrayOfFloatPoints);
{  cf : https://www.w3schools.com/tags/canvas_arc.asp
               270° (1.5*PI)
                |
(1*PI)  180° ---X--- 0° (0)
                |
               90° (0.5*PI)

 | = Ry
 --- = Rx
 X = (Cx, Cy)
}
Var
  Range, s,c: Single;
  OptimalSteps,Tolerance, Xs, Ys: Single;
  //i,
  Dx, Dy {,StrokePatternSize}, delta: Integer;
  AngleCurrent, AngleDiff, AngleStep, AngleBegin, AngleEnd: Single;
  PT: TBZVector2f;
Begin

  //StrokePatternSize := 0;
  Tolerance := 0.1;

  if (StartAngle = EndAngle) then exit;
  //if (StartAngle > EndAngle) then Swap(StartAngle, EndAngle);

  if ClockWise then  // Sens aiguille d'une montre (CW)
  begin
    AngleBegin := DegToRad(EndAngle);
    AngleEnd := DegToRad(StartAngle);
  end
  else
  begin  // Sens inverse aiguille d'une montre (CCW)
    AngleBegin := DegToRad(StartAngle);
    AngleEnd := DegToRad(EndAngle);
  end;

  if (AngleEnd >= AngleBegin) then
  begin // if end sup to begin, remove 2*Pi (360°)
    AngleEnd := AngleEnd - c2PI;
  end;

  ArrayOfPoints := TBZArrayOfFloatPoints.Create(64);
  //Premier Point
  s := Sin(AngleBegin);
  c := Cos(AngleBegin);
  Pt.x := cX + (Rx * c);
  Pt.y := cY + (Ry * s);

  ArrayOfPoints.Add(PT);

  AngleDiff := Abs(AngleEnd - AngleBegin); // the amount radian to travel

  AngleCurrent := AngleBegin;
  Range := AngleCurrent - AngleDiff;

  OptimalSteps := Trunc((RadToDeg(AngleDiff) / (2 * ArcCos(1 - Tolerance / Abs(Max(Rx, Ry))))));
  if OptimalSteps < 2 then OptimalSteps := 2;
  AngleStep := AngleDiff /  OptimalSteps; // granulity of drawing, not too much, not too less

  //i:=0;
  while AngleCurrent >= Range do
  begin
    s := Sin(AngleCurrent);
    c := Cos(AngleCurrent);
    Pt.x := cX + (Rx * c);
    Pt.y := cY + (Ry * s);

    ArrayOfPoints.Add(PT);
    AngleCurrent := AngleCurrent - (AngleStep); // always step down, rotate only one way to draw it
  end;

  //Dernier Point
  s := Sin(AngleEnd);
  c := Cos(AngleEnd);
  Pt.x := cX + (Rx * c);
  Pt.y := cY + (Ry * s);

  ArrayOfPoints.Add(PT);

End;

procedure BuildPolySuperShape(Coords : TBZFloatPoint; n1, n2, n3, m, a, b, ScaleFactor : Single; nbPoints : Integer; out ArrayOfPoints : TBZArrayOfFloatPoints);
Var
 angle : Single;
 delta : Single;
 f : Single;
 {$CODEALIGN VARMIN=16}
 pt, scaleF, sc : TBZFloatPoint;
 {$CODEALIGN VARMIN=4}

  function Evaluate(Theta : Single) : Single;
  Var
    r, p1, p2, p3, mm : Single;
  begin
    mm := m / 4;
    p1 := Math.Power( abs((1 / a) * Cos(Theta * mm)), n2);
    p2 := Math.Power( abs((1 / b) * Sin(Theta * mm)), n3);
    p3 := Math.Power(p1 + p2, 1 / n1);
    if p3 = 0 then r := 0
    else r := 1 / p3;
    Result := r;
  end;

begin
  ScaleF.Create(ScaleFactor,ScaleFactor);

  angle := 0;
  Delta := c2PI / nbPoints;

  ArrayOfPoints := TBZArrayOfFloatPoints.Create(nbPoints);

  While Angle < c2PI do
  begin
    f := Evaluate(angle);
    if f<>0 then
    begin
      pt.Create(f,f);
      sc.Create(Cos(Angle), Sin(Angle));
      pt := (ScaleF * pt) * sc;
      pt := pt + Coords;
      ArrayOfPoints.Add(pt);
    end;
    angle := angle + Delta;
  end;
end;

procedure BuildPolyStar(Coords : TBZFloatPoint; InnerRadius, OuterRadius : Single; nbBranches : Integer; out ArrayOfPoints : TBZArrayOfFloatPoints);
var
  i, nbPoints: Integer;
  sinA, cosA, a : Single;
  delta: TBZFloatPoint;
  {$CODEALIGN VARMIN=16}
  pt, pri, pro : TBZFloatPoint;
  {$CODEALIGN VARMIN=4}
begin
  if (innerRadius <= 0) or (outerRadius <= 0) then Exit;
  if nbBranches <= 5 then nbPoints := 10
  else nbPoints := nbBranches * 2;
  a := c2PI / nbPoints;
  SinA := Sin(a);
  CosA := Cos(a);
  Delta.Create(CosA, SinA);

  ArrayOfPoints := TBZArrayOfFloatPoints.Create(nbPoints);
  pt := Coords;
  pt.X := pt.X + InnerRadius;
  ArrayOfPoints.Add(pt);
  pri.Create(InnerRadius, InnerRadius);
  pro.Create(OuterRadius, OuterRadius);
  for i := 1 to nbPoints -1 do
  begin
    if Odd(i) then
    begin
       pt := (pro * Delta) + Coords;
    end
    else
    begin
      pt := (pri * Delta) + Coords;
    end;
    ArrayOfPoints.Add(pt);

    delta.Create(delta.X * cosA - delta.Y * sinA,
                 delta.Y * cosA + delta.X * sinA);


  end;
end;

{%endregion}

end.

