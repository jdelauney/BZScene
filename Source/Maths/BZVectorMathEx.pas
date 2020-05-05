(*

  @abstract(Contient des classes optimisées supplémentaires utilisant l'accélération SIMD (SSE, SSE3, SS4, AVX, AVX2) @br
  Elles peuvent être utilisés dans les graphiques 2D / 3D, moteur physique, jeux etc...)

  -------------------------------------------------------------------------------------------------------------

  @created(2017-11-25)
  @author(J.Delauney (BeanzMaster))
  @author(Peter Dyson (Dicepd))

  Historique : @br
  @unorderedList(
    @item(Last Update : 13/03/2018  )
    @item(12/03/2018 : Creation  )
  )

  Support :@br
    @unorderedlist (
     @item(Frustum)
     @item(Boite englobante orienté en 3D (OBB)
     @item(Sphere englobante 3D)
     @item(Boite englobante 3D avec alignement des axes (Axis Aligned Bounding Box))
     @item(Raycast)
   )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) :

  -------------------------------------------------------------------------------------------------------------

  @bold(Dependances) : BZVectorMath

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)
    @unorderedList(
      @item(FPC/Lazarus)
      @item(GLScene)
      @item(All authors of papers and web links)
    )

  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
Unit BZVectorMathEx;

{$WARN 5036 on : Local variable "$1" does not seem to be initialized}
{$WARN 5025 on : Local variable "$1" not used}
{$WARN 5028 on : Local const "$1" not used}

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}

//-----------------------------
{$INLINE ON}
{$MODESWITCH ADVANCEDRECORDS}
//-----------------------------

//----------------------- DATA ALIGNMENT ---------------------------------------
{$ALIGN 16}
{$CODEALIGN CONSTMIN=16}
{$CODEALIGN LOCALMIN=16}
{$CODEALIGN VARMIN=16}

//==============================================================================


Interface

Uses
  Classes, Sysutils, BZVectorMath;

{%region%----[ BoundingBox ]----------------------------------------------------}

Type
  { @abstract(Represente une transformation affine 2D avec une matrix 3x3)

  Les éléments de la matrice sont stocker dans l'ordre de type "Row-major"

  Plus d'informations : @br
  @unorderedlist(
    @item(https://computergraphics.stackexchange.com/questions/391/what-are-affine-transformations)
    @item(https://en.wikipedia.org/wiki/Affine_transformation)
    @item(http://blog.lotech.org/2013/05/the-beauty-of-using-matrices-to-apply.html)
    @item(https://webglfundamentals.org/webgl/lessons/fr/webgl-2d-matrices.html)
    @item(https://www.tutorialspoint.com/computer_graphics/2d_transformation.htm)
    @item(https://code.tutsplus.com/tutorials/understanding-affine-transformations-with-matrix-mathematics--active-10884)
    @item(http://negativeprobability.blogspot.in/2011/11/affine-transformations-and-their.html)
    @item(https://homepages.inf.ed.ac.uk/rbf/HIPR2/affine.htm)
    @item(http://ncase.me/matrix/)
   )}

  { TBZ2DSingleAffineMatrix }

  TBZ2DSingleAffineMatrix = record
  private
    function GetComponent(const ARow, AColumn: Integer): Single; inline;
    procedure SetComponent(const ARow, AColumn: Integer; const Value: Single); inline;
    function GetRow(const AIndex: Integer): TBZVector3f; inline;
    procedure SetRow(const AIndex: Integer; const Value: TBZVector3f); inline;

    function GetDeterminant: Single;
  public

    { Creation d'une matrice d'identité }
    procedure CreateIdentityMatrix;
    { Creation d'une matrice nulle }
    procedure CreateNullMatrix;
    { Creation d'une matrice de translation déterminée par les valeurs "OffsetX et OffsetY" }
    procedure CreateTranslationMatrix(Const OffsetX, OffsetY : Single); overload;
    { Creation d'une matrice de translation déterminée par un vecteur de type TBZVector2f }
    procedure CreateTranslationMatrix(Constref Offset : TBZVector2f); overload;
    { Creation d'une matrice d'homotéthie (mise à l'echelle) déterminée par les valeurs "ScaleX et ScaleY" }
    procedure CreateScaleMatrix(Const ScaleX, ScaleY : Single);overload;
    { Creation d'une matrice d'homotéthie (mise à l'echelle) déterminée par un vecteur de type TBZVector2f }
    procedure CreateScaleMatrix(Constref Scale : TBZVector2f);overload;
    { Creation d'une matrice de rotation déterminée par un Angle (en radian) }
    procedure CreateRotationMatrix(Const anAngle : Single);overload;
    { Creation d'une matrice de cisaillement déterminée par les valeurs "ShearX et ShearY" }
    procedure CreateShearMatrix(const ShearX, ShearY : Single);overload;
    { Creation d'une matrice de cisaillement déterminée par un vecteur de type TBZVector2f }
    procedure CreateShearMatrix(constref Shear : TBZVector2f);overload;
    { Creation d'une matrice de réflection dans l'axe des X }
    procedure CreateReflectXMatrix;
    { Creation d'une matrice de réflection dans l'axe des Y }
    procedure CreateReflectYMatrix;
    { Creation d'une matrice de réflection pour les axes X et Y }
    procedure CreateReflectMatrix;
    { Creation d'une matrice en fonctions des paramètres de type Single }
    procedure Create(ConstRef ScaleX, ScaleY, ShearX, ShearY, anAngle, OffsetX, OffsetY  : Single); overload;
    { Creation d'une matrice en fonctions des paramètres de type TBZVector2f et de type Single pour l'angle }
    procedure Create(Constref Scale, Shear, Offset : TBZVector2f; Const anAngle : Single); overload;

    procedure CreatePerspectiveMatrix(ConstRef StartX, StartY , AWidth, AHeight, x1, y1, x2, y2, x3, y3, x4, y4 : Single); overload;
    procedure CreatePerspectiveMatrix(ConstRef StartPoint , ASize, ptA, ptB, ptC, ptD : TBZVector2f); overload;

    {  Retourne une chaine de caractères formaté représentant la matrice affine : @br
	     "("x, y, Z")" @br
       "("x, y, Z")" @br
       "("0, 0, 1")"}
    function ToString : String;

    {Multiplie deux matrices affines }
    class operator *(constref A, B: TBZ2DSingleAffineMatrix): TBZ2DSingleAffineMatrix; overload;
    { Transforme un vecteur en le multipliant par la matrice dans l'ordre "Matrice * Point" }
    class operator *(constref A: TBZ2DSingleAffineMatrix; constref B: TBZVector2f): TBZVector2f; overload;
    { Transforme un vecteur en le multipliant par la matrice dans l'ordre "Point * Matrice }
    class operator *(constref A: TBZVector2f; constref B: TBZ2DSingleAffineMatrix): TBZVector2f; overload;

    { Retourne la matrice transposée }
    function Transpose: TBZ2DSingleAffineMatrix;
    { Retourne la matrice inverse }
    function Invert : TBZ2DSingleAffineMatrix;

    { Effectue une translation sur l'axe des X et Y }
    procedure Translate(x,y : Single); overload;
    { Effectue une translation sur l'axe des X }
    procedure TranslateX(x : Single); overload;
    { Effectue une translation sur l'axe des Y }
    procedure TranslateY(y : Single); overload;
    { Effectue une translation sur l'axe des X et Y représenté par un vecteur de type TBZVector2f }
    procedure Translate(aVector : TBZVector2f); overload;
    { Effectue une rotation de Angle (en radian) }
    procedure Rotate(Angle : Single);

    { Effectue une mise à l'échelle sur l'axe des X et Y }
    procedure Scale(x,y : Single); overload;
    { Effectue une mise à l'échelle sur l'axe des X}
    procedure ScaleX(x : Single); overload;
    { Effectue une mise à l'échelle sur l'axe des Y }
    procedure ScaleY(y : Single); overload;
    { Effectue une mise à l'échelle sur l'axe des X et Y représenté par un vecteur de type TBZVector2f }
    procedure Scale(aVector : TBZVector2f); overload;

    { Effectue un cisaillement sur l'axes des X et Y }
    procedure Shear(x,y : Single); overload;
    { Effectue un cisaillement sur l'axes des X }
    procedure ShearX(x : Single); overload;
    { Effectue un cisaillement sur l'axes des Y }
    procedure ShearY(y : Single); overload;
    { Effectue un cisaillement sur l'axe des X et Y représenté par un vecteur de type TBZVector2f }
    procedure Shear(aVector : TBZVector2f); overload;

    function TransformPoint(ConstRef A : TBZVector2f) : TBZVector2f;

    { Acces aux valeur de la matrice (Row-major) }
    property Components[const ARow, AColumn: Integer]: Single read GetComponent write SetComponent; default;
    { Retourne le coefficiant "Determinant" de la matrice }
    property Determinant: Single read GetDeterminant;

    { Acces au propriétés }
    case Byte of
      0: (M: array [0..2, 0..2] of Single); //< Acces via tableau 2D
      1: (V: array [0..2] of TBZVector3f);  //< Acces via tableau de TBZVector3f
      2: (X,Y,Z : TBZVector3f);             //< Acces par ligne de type TBZVector3f
      3: (m11, m12, m13 : Single;           //< Acces par défaut
          m21, m22, m23 : Single;
          m31, m32, m33 : Single;
         );
  End;

  { Type de convenance pour décrire une matrice de transformation 2D affine }
  TBZAffineMatrix = TBZ2DSingleAffineMatrix;

{%endregion%}

 { Note :
   Les enregistrements ci-dessus sont le plus utilisés dans des moteurs de rendus, Physiques, collisions.
   Pour les utiliser, il est recommandé de les envelopper (wrapper) dans des classes personnalisées. }

{%region%----[ BoundingBox ]----------------------------------------------------}

Type
  { @abstract(Décris une boite englobante orienté 3D "OBB"@br
    Une OBB est définis par huit points décrivant la position de chaque coins de la boite))

    Plus d'informations :@br
    @unorderedlist(
     @item(https://en.wikipedia.org/wiki/Minimum_bounding_box)
     @item(https://geidav.wordpress.com/tag/oriented-bounding-box/)
     @item(https://graphics.ethz.ch/teaching/former/seminar02/handouts/collisiondetection.pdf)
     @item(http://www.idt.mdh.se/~tla/publ/FastOBBs.pdf)
     @item(http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.717.9566&rep=rep1&type=pdf)
     @item(https://www.geometrictools.com/Documentation/DynamicCollisionDetection.pdf)
     @item(http://gamma.cs.unc.edu/users/gottschalk/main.pdf)) }
  TBZBoundingBox = record
  private
  public
    { Création d'une boite englobante orienté à partir d'un point }
    procedure Create(Const AValue : TBZVector);

    { Ajoute deux boites englobante }
    class operator +(ConstRef A, B : TBZBoundingBox):TBZBoundingBox;overload;
    { Ajoute à la boites englobante  un point (Etend la boite englobante ) }
    class operator +(ConstRef A: TBZBoundingBox; ConstRef B : TBZVector):TBZBoundingBox;overload;
    { Compare si deux boite englobante sont égales }
    class operator =(ConstRef A, B : TBZBoundingBox):Boolean;overload;

    { Applique une matrice de transformation affine 4x4 }
    function Transform(ConstRef M:TBZMatrix):TBZBoundingBox;

    { Retourne la valeur minimale des X }
    function MinX : Single;
    { Retourne la valeur maximale des X }
    function MaxX : Single;
    { Retourne la valeur minimale des Y }
    function MinY : Single;
    { Retourne la valeur maximale des Y }
    function MaxY : Single;
    { Retourne la valeur minimale des Z }
    function MinZ : Single;
    { Retourne la valeur maximale des Z }
    function MaxZ : Single;

    { Acces aux propriétés }
    Case Integer of
     0 : (Points : Array[0..7] of TBZVector); //< Acces via tableau de TBZVector
     1 : (pt1, pt2, pt3, pt4 :TBZVector;      //< Acces par défaut
          pt5, pt6, pt7, pt8 :TBZVector);
  end;

{%endregion%}

  { Description pour le test des intersections dans l'espace avec les boites englobantes
    Voir aussi : TBZBoundingSphere, TBZAxisAlignedBoundingBox, TBZBoundingCircle, TBZAxisAlignedBoundingRect (2D) }
  TBZSpaceContains = (ScNoOverlap, ScContainsFully, ScContainsPartially);

{%region%----[ BoundingSphere ]-------------------------------------------------}

  //TBZBoundingCircle

  { @abstract(Décris une sphere englobante simple)

    plus d'informations :@br
    @unorderedlist(
     @item(https://en.wikipedia.org/wiki/Bounding_sphere)
     @item(https://www.ep.liu.se/ecp/034/009/ecp083409.pdf)
     @item(https://www.mvps.org/directx/articles/using_bounding_spheres.htm))}
  TBZBoundingSphere = record
  public
    { Creation d'une sphere englobante au coordonnées x,y et z. le rayon est par défaut de 1.0 unité }
    procedure Create(Const x,y,z: Single;Const r: single = 1.0); overload;
    { Creation d'une sphere englobante au coordonnées définies par un TBZAffineVector. le rayon est par défaut de 1.0 unité }
    procedure Create(Const AValue : TBZAffineVector;Const r: single = 1.0); overload;
    { Creation d'une sphere englobante au coordonnées définies par un TBZVector. le rayon est par défaut de 1.0 unité }
    procedure Create(Const AValue : TBZVector;Const r: single = 1.0); overload;
	
    { Retourne une chaine de caractères formaté représentant la sphere englobante : '(x, y, z, radius)' }
    function ToString: String;

    { Determine l'espace d'intersection TBZSpaceContains de la sphere englobante avec une autre }
    function Contains(const TestBSphere: TBZBoundingSphere) : TBZSpaceContains;

    { Determine si il y a intersection dans l'espace avec la sphere englobante et une autre }
    function Intersect(const TestBSphere: TBZBoundingSphere): Boolean;

    { Access aux propriétés }
    Case Integer of
      0 : (Center: TBZVector;  //< Centre de la sphere englobante
          Radius: Single);     //< Rayon de la sphere englobante
  end;

{%endregion%}

{%region%----[ Axis Aligned BoundingBox ]---------------------------------------}

  { Structure pour stocker une AABB. @br
    Utilisé par TBZAxisAlignedBoundingBox.ExtractCorners }
  TBZAABBCorners = array [0 .. 7] of TBZVector;

  // TBZAABRCorners = array [0 .. 3] of TBZVector2f;
  // TBZAxisAlignedBoundingRect

  { Décrisption d'une boite englobante alignée en 3D (AABB)

  @bold(Note) : @br
  'Le calcul de base sous-jacent du lancer de rayons est celui de l'intersection d'une ligne avec
   les surfaces d'un objet. @br
   Est une méthode pour effectuer ce calcul, pour une nouvelle et puissante classe d'objets, ceux définis en balayant une sphère de rayon variable le long d'une trajectoire 3D. @br
   Lorsque des polynômes sont utilisés pour le paramétrage du centre et du rayon de la sphère, @br
   Le problème d'intersection se réduit à l'emplacement des racines d'un polynôme. ' @br
   @italic(Source : https://www.sciencedirect.com/science/article/pii/009784938590055X)

   Plus d'informations :@br
   @unorderedlist(
     @item(https://en.wikipedia.org/wiki/Bounding_volume)
     @item(https://www.gamasutra.com/view/feature/131833/when_two_hearts_collide_.php)
     @item(https://developer.mozilla.org/kab/docs/Games/Techniques/2D_collision_detection)
     @item(https://www.azurefromthetrenches.com/introductory-guide-to-aabb-tree-collision-detection/)
     @item(http://www.gamefromscratch.com/post/2012/11/26/GameDev-math-recipes-Collision-detection-using-an-axis-aligned-bounding-box.aspx)
     @item(https://www.researchgate.net/publication/303703971_An_intensity_recovery_algorithm_IRA_for_minimizing_the_edge_effect_of_LIDAR_data)
     @item(http://www-ljk.imag.fr/Publications/Basilic/com.lmc.publi.PUBLI_Inproceedings@117681e94b6_1860ffd/bounding_volume_hierarchies.pdf))}
  TBZAxisAlignedBoundingBox =  record
  public
    { Création d'une boite englobante alignée depuis un point }
    procedure Create(const AValue: TBZVector);overload;
    { Création d'une boite englobante alignée depuis deux points décrivant les coordonées minimum et maximum de la boite }
    procedure Create(const AMin, AMax: TBZVector);overload;
    { Création d'une boite englobante alignée depuis une boite englobante orienté (TBZBoundingBox) }
    procedure Create(const ABB: TBZBoundingBox);
    { @abstract(Création d'une boite englobante alignée, qui sera formée en balayant une sphère de Start à Dest.)

      Plus d'informations :@br
        @unorderedlist(
         @item(http://physicsforgames.blogspot.in/2010/03/narrow-phase-sweeping-sphere-against.html)
         @item(https://www.gamasutra.com/view/feature/131790/simple_intersection_tests_for_games.php)) }
    procedure CreateFromSweep(const Start, Dest: TBZVector;const Radius: Single);
    { Création d'une boite englobante alignée depuis une sphere englobante (TBZBoundingSphere) }
    procedure Create(const BSphere: TBZBoundingSphere); overload;
    { Création d'une boite englobante alignée depuis une sphere englobante définie par une centre et un rayon }
    procedure Create(const Center: TBZVector; Radius: Single); overload;

    { Ajoute deux boite englobante alignée }
    class operator +(ConstRef A, B : TBZAxisAlignedBoundingBox):TBZAxisAlignedBoundingBox;overload;
    { Ajoute un point à la boite englobante (Etend la boite) }
    class operator +(ConstRef A: TBZAxisAlignedBoundingBox; ConstRef B : TBZVector):TBZAxisAlignedBoundingBox;overload;
    { Multiplie une boite englobante par un vecteur TBZVecteur (Mise à l'echelle) }
    class operator *(ConstRef A: TBZAxisAlignedBoundingBox; ConstRef B : TBZVector):TBZAxisAlignedBoundingBox;overload;

    { Compare si deux boite englobante alignée sont égales }
    class operator =(ConstRef A, B : TBZAxisAlignedBoundingBox):Boolean;overload;

    { Applique une matrice de transformation affine 4x4 }
    function Transform(Constref M:TBZMatrix):TBZAxisAlignedBoundingBox;
    { Etend la boite englobante avec le point P de type TBZVector }
    function Include(Constref P:TBZVector):TBZAxisAlignedBoundingBox;
	
    { Retourne l'intersection de la boite englobante alignée avec une autre. @br
      Si les AABB ne se coupent pas, alors le résultat sera une AABB dégénéré (plan, ligne ou point). }
    function Intersection(const B: TBZAxisAlignedBoundingBox): TBZAxisAlignedBoundingBox;

    { Convertit l'AABB en sa TBZBoundingBox canonique.}
    function ToBoundingBox: TBZBoundingBox; overload;

    { Convertit l'AABB transformée par une matrice en TBZBoundingBox }
    function ToBoundingBox(const M: TBZMatrix) : TBZBoundingBox; overload;

    { Convertit l'AABB en une sphere englobante TBZBoundingSphere }
    function ToBoundingSphere: TBZBoundingSphere;

	// Not needed at this stage @TODO MOVE IN A TBZAABoundingBoxObject class
    //function ToClipRect(ModelViewProjection: TBZMatrix; ViewportSizeX, ViewportSizeY: Integer): TBZClipRect;
	
    { @abstract(Retourne @True si il y a Intersection en la boite englobante alignée et une autre boite englobante alignée.)

      Les matrices servent à convertir un point en système AABB vers l'autre avec : @br
	    @unorderedlist(
        @item(m1 : matrice de rotation matrix)
        @item(m2 : matrice de Translation et/ou de mise à l'echelle) )}
    function Intersect(const B: TBZAxisAlignedBoundingBox;const M1, M2: TBZMatrix):Boolean;
    { Vérifie et retourne @True si la boite englobante alignée entre en collision avec une autre, alignée sur l'axe dans le plan XY. }
    function IntersectAbsoluteXY(const B: TBZAxisAlignedBoundingBox): Boolean;
    { Vérifie et retourne @True si la boite englobante alignée entre en collision avec une autre, alignée sur l'axe dans le plan XZ. }
    function IntersectAbsoluteXZ(const B: TBZAxisAlignedBoundingBox): Boolean;
    { Vérifie et retourne @True si la boite englobante alignée entre en collision avec une autre, alignée sur l'axe du monde virtuel. }
    function IntersectAbsolute(const B: TBZAxisAlignedBoundingBox): Boolean;
    { Vérifie et retourne @True si la boite englobante alignée s'inscrit dans une autre, alignée avec les axes du monde virtuel.}
    function FitsInAbsolute(const B: TBZAxisAlignedBoundingBox): Boolean;
    { Retourne @True si le point P de type TBZVector se trouve à l'intérieur de la boite englobante alignée }
    function PointIn(const P: TBZVector): Boolean;
    { Extrait les coins de la boite englobante alignée }
    function ExtractCorners: TBZAABBCorners;
    { Détermine dans quel espace (TBZSpaceContains) l'AABB contient un autre AABB. }
    function Contains(const TestAABB: TBZAxisAlignedBoundingBox): TBZSpaceContains; overload;
    { Détermine dans quel espace (TBZSpaceContains) l'AABB contient une sphere englobante TBZBoundingSphere }
    function Contains(const TestBSphere: TBZBoundingSphere): TBZSpaceContains; overload;

    // @TODO MOVE TO TBZVectorHelper
    { : Clips a position to the AABB }
    //function Clip(const V: TBZAffineVector): TBZAffineVector;

	// @TODO MOVE TO TBZRayCast
    { : Finds the intersection between a ray and an axis aligned bounding box. }
    //function RayCastIntersect(const RayOrigin, RayDirection: TBZVector; out TNear, TFar: Single): Boolean; overload;
    //function RayCastIntersect(const RayOrigin, RayDirection: TBZVector; IntersectPoint: PBZVector = nil): Boolean; overload;

    Case Integer of
      0 : (Min, Max : TBZVector);
  end;

{%endregion%}

{%region%----[ Frustum ]--------------------------------------------------------}

  { Structure pour stocker les plan d'un "frustum" (champ de vue de la caméra) }
  TBZFrustrumPlanesArray = Array[0..5] of TBZHmgPlane;
  { Description d'un champ de vue pour une caméra }
  TBZFrustum =  record
  public
    //procedure Create(vLeft, vTop, vRight, vBottom, vNear, vFar : TBZHmgPlane);
  //procedure Create(vLeft, vTop, vRight, vBottom : TBZHmgPlane; vNearDistance, vFarDistance : Single );
     //procedure CreateFromPlane(aPlane : TBZHmgPlane; vNear, vFar : Single); overload;
     //procedure CreateFromPlane(vLeft, vTop, vRight, vBottom, vNear, vFar : Single); overload;

    { Creation d'un TBZFrustum depuis une  matrice décrivant le "model-view-projection". }
    procedure CreateFromModelViewProjectionMatrix(constref modelViewProj : TBZMatrix);

    // function ExtractMatrix : TBZMatrix;
    { Détermine dans quel espace (TBZSpaceContains) du "Frustrum" une sphere englobante fait partie. }
    function Contains(constref TestBSphere: TBZBoundingSphere): TBZSpaceContains;overload;

    { Détermine dans quel espace (TBZSpaceContains) du "Frustrum" une boite englobante alignée fait partie. @br
      voir : http://www.flipcode.com/articles/article_frustumculling.shtml }
    function Contains(constref TestAABB: TBZAxisAlignedBoundingBox) : TBZSpaceContains;overload;

    { Determine si un volume (une sphere englobante) est "coupé" ou pas }
    function IsVolumeClipped(constref objPos : TBZVector; const objRadius : Single):Boolean;overload; //const Frustum : TFrustum) : Boolean; overload;
    //function IsVolumeClipped(const min, max : TBZVector):Boolean;overload;

    //function GetNearDistance : Single;
    //function GetFarDistance : Single;

   { Acces aux propriétés }
   case byte of
     0: (pLeft, pTop, pRight, pBottom, pNear, pFar : TBZHmgPlane;
         BoundingSphere: TBZBoundingSphere);  //< Acces par défaut. Ajoute une sphere englobante pour décrire le "frustum" et qui sera utilisé pour des tests d'intersections rapides
     1: (Planes : TBZFrustrumPlanesArray);    //< Plans des délimitation du "frustum"

 end;

{%endregion%}

{%region%----[ TBZBoundingBoxHelper ]------------------------------------------}
{%endregion%}

{%region%----[ TBZAxisAlignedBoundingBoxHelper ]-------------------------------}
{%endregion%}

{%region%----[ Frustum ]--------------------------------------------------------}


  { Frustum management class }
 (* TBZFrustum =  Class
  private
    // FpNear, FpFar
    // FpLeft,FpTop,
    // FpRight,FpBottom : TBZHmgPlane;

    // Boundary planes (front, back, left, top, right, bottom)
    FPlanes : TBZFrustrumPlanesArray;
    // Extended frustum, used for fast intersection testing
    FBoundingSphere: TBZBoundingSphere;
    // View matrix
    FViewMatrix: TBZMatrix;
    // Projection matrix
    FProjectionMatrix: TBZMatrix;
    // Field of view on Y
    FFOVY: Single;
    // Field of view on X
    FFOVX: Single;
    // Clipping distance
    FClipDistance : TBZVector2f; //x = Near, Y = Far

    // Theta ????
    FTheta: Single;

    // View width.
    FViewWidth: Single;
    // View height.
    FViewHeight: Single;
    // View Aspect Ratio
    FAspectRatio: Single;

    procedure SetPlane(Const Idx:Integer; aPlane : TBZHmgPlane);
    function  GetPlane(Const Idx:Integer) : TBZHmgPlane;
    procedure SetViewPort( const aWidth, aHeight: Single );
    procedure SetFOVy( const AFOVy: Single );
    function GetFOVy( ): Single; inline;
    procedure SetAspectRatio( const fRatio: Single );
    function GetAspectRatio( ): Single;
    procedure SetNearClipDistance( const DistNear: Single );
    function GetNearClipDistance : Single;
    procedure SetFarClipDistance( const DistFar: Single );
    function GetFarClipDistance : Single;
  protected
    // Set to TRUE if Datas need to be updated
    FNeedUpdate: Boolean;
    // Updates the planes.
    procedure DoUpdate();
  public
    // Create
    constructor Create( const VpWidth, VpHeight : Single; const DistNear, DistFar, FOVY: Single); overload;
    // Create from a view matrix
    constructor Create( const ViewMatrix: TBZMatrix ); overload;

    Destructor Destroy;

    { Extracts a TFrustum for combined modelview and projection matrices. }
    Procedure ExtractFromModelViewProjectionMatrix(constref modelViewProj : TBZMatrix);
    //  Compute  and return  projection matrix
    function GetProjectionMatrix( ): TBZMatrix;

    // Returns True, if frustum needs an update.
    function NeedUpdate( ): Boolean; inline;

    // Tests against a bounding sphere.
    function Contains(constref TestBSphere: TBZBoundingSphere): TBZSpaceContains;overload;

    // Tests against an axis-aligned bounding box.
    // function ContainsAABB( AABB: TPGAABB ): TBZHmgPlaneHalfSpace;
    // see http://www.flipcode.com/articles/article_frustumculling.shtml
    function Contains(constref TestAABB: TBZAxisAlignedBoundingBox) : TBZSpaceContains;overload;

    // Determines if volume is clipped or not
    function IsVolumeClipped(constref objPos : TBZVector; const objRadius : Single):Boolean;overload; //const Frustum : TFrustum) : Boolean; overload;
    //function IsVolumeClipped(const min, max : TBZVector):Boolean;overload; //const Frustum : TFrustum) : Boolean; overload;

    property Planes : TBZFrustrumPlanesArray read FPlanes;
    property Front  : TBZHmgPlane index 0 read GetPlane write SetPlane;
    property Back   : TBZHmgPlane index 1 read GetPlane write SetPlane;
    property Left   : TBZHmgPlane index 2 read GetPlane write SetPlane;
    property Top    : TBZHmgPlane index 3 read GetPlane write SetPlane;
    property Right  : TBZHmgPlane index 4 read GetPlane write SetPlane;
    property Bottom : TBZHmgPlane index 5 read GetPlane write SetPlane;
 end;  *)

{%endregion%}

{%region%----[ TBZHmgPlaneHelper ]---------------------------------------------}

  { @abstract(Assistant TBZHmgPlane)

    @italic(Utilisé pour des fonctions où nous utilisons des types non déclaré avant TBZHmgPlane) }
  TBZHmgPlaneHelper = record helper for TBZHmgPlane
  public
    { Détermine dans quel espace (TBZSpaceContains) du plan un sphere englobante est contenue }
    function Contains(const TestBSphere: TBZBoundingSphere): TBZSpaceContains;

    //function PlaneContains(const Location, Normal: TBZVector; const TestBSphere: TBZBoundingSphere): TBZSpaceContains;

    { Calcule le produit croisé (Crossproduct) entre la normal du plan et le plan à un point donné. @br
      Cette fonction donne une indication sur la position du point, si le point est dans le demi-espace pointé par le vecteur, le résultat est positif. @br
      @bold(Note) : Cette fonction effectue un produit scalaire spatial homogène. }
    function EvaluatePoint(constref Point : TBZVector) : Single;
  end;

{%endregion%}

{%region%----[ OBB Const ]------------------------------------------------------}

const
   { Définie une boite englobante orienté (OBB) nulle }
   NullBoundingBox: TBZBoundingBox =
   (Points:((X: 0; Y: 0; Z: 0; W: 1),
            (X: 0; Y: 0; Z: 0; W: 1),
            (X: 0; Y: 0; Z: 0; W: 1),
            (X: 0; Y: 0; Z: 0; W: 1),
            (X: 0; Y: 0; Z: 0; W: 1),
            (X: 0; Y: 0; Z: 0; W: 1),
            (X: 0; Y: 0; Z: 0; W: 1),
            (X: 0; Y: 0; Z: 0; W: 1)));

{%endregion%}

Implementation

Uses Math, BZMath;

// ---- Used by Bounding box functions -----------------------------------------

type
  TPlanIndices = array [0 .. 3] of Integer;
  TPlanBB = array [0 .. 5] of TPlanIndices;
  TDirPlan = array [0 .. 5] of Integer;

const
  CBBFront: TPlanIndices = (0, 1, 2, 3);
  CBBBack: TPlanIndices = (4, 5, 6, 7);
  CBBLeft: TPlanIndices = (0, 4, 7, 3);
  CBBRight: TPlanIndices = (1, 5, 6, 2);
  CBBTop: TPlanIndices = (0, 1, 5, 4);
  CBBBottom: TPlanIndices = (2, 3, 7, 6);
  CBBPlans: TPlanBB = ((0, 1, 2, 3), (4, 5, 6, 7), (0, 4, 7, 3), (1, 5, 6, 2),
    (0, 1, 5, 4), (2, 3, 7, 6));
  CDirPlan: TDirPlan = (0, 0, 1, 1, 2, 2);


procedure SetPlanBB(Var A:TBZBoundingBox;const NumPlan: Integer; const Valeur: Double);
var
  I: Integer;
begin
  for I := 0 to 3 do
  begin
    A.Points[CBBPlans[NumPlan][I]].V[CDirPlan[NumPlan]] := Valeur;
    A.Points[CBBPlans[NumPlan][I]].V[3] := 1;
  end;
end;

//-----[ INCLUDE IMPLEMENTATION ]-----------------------------------------------

{$ifdef USE_ASM}
  {$ifdef CPU64}
    {$ifdef UNIX}
      {$IFDEF USE_ASM_AVX}
         {$I vectormathex_boundingbox_native_imp.inc}
         {$I vectormathex_boundingsphere_native_imp.inc}
         {$I vectormathex_axisaligned_boundingbox_native_imp.inc}
         {.$I vectormathex_boundingboxhelper_native_imp.inc}
         {.$I vectormathex_axisaligned_boundingBoxhelper_native_imp.inc}
         {$I vectormathex_frustum_native_imp.inc}
         {$I vectormathex_hmgplanehelper_native_imp.inc}
	 {.$I vectormathex_hmgplanehelper_unix64_avx_imp.inc}
      {$ELSE}
         {$I vectormathex_2DSingleAffineMatrix_native_imp.inc}
         {$I vectormathex_boundingbox_native_imp.inc}
         {$I vectormathex_boundingsphere_native_imp.inc}
         {$I vectormathex_axisaligned_boundingbox_native_imp.inc}
         {.$I vectormathex_boundingboxhelper_native_imp.inc}
         {.$I vectormathex_axisaligned_boundingBoxhelper_native_imp.inc}
         {$I vectormathex_frustum_native_imp.inc}
         {$I vectormathex_hmgplanehelper_native_imp.inc}
	 {$I vectormathex_hmgplanehelper_unix64_sse_imp.inc}
      {$ENDIF}
    {$else} // win64
      {$IFDEF USE_ASM_AVX}
	 {$I vectormathex_boundingbox_native_imp.inc}
         {$I vectormathex_boundingsphere_native_imp.inc}
         {$I vectormathex_axisaligned_boundingbox_native_imp.inc}
         {.$I vectormathex_boundingboxhelper_native_imp.inc}
         {.$I vectormathex_axisaligned_boundingBoxhelper_native_imp.inc}
         {$I vectormathex_frustum_native_imp.inc}
         {$I vectormathex_hmgplanehelper_native_imp.inc}
	 {.$I vectormathex_hmgplanehelper_win64_avx_imp.inc}
       {$ELSE}
         {$I vectormathex_2DSingleAffineMatrix_native_imp.inc}
      	 {$I vectormathex_boundingbox_native_imp.inc}
         {$I vectormathex_boundingsphere_native_imp.inc}
         {$I vectormathex_axisaligned_boundingbox_native_imp.inc}
         {.$I vectormathex_boundingboxhelper_native_imp.inc}
         {.$I vectormathex_axisaligned_boundingBoxhelper_native_imp.inc}
         {$I vectormathex_frustum_native_imp.inc}
         {$I vectormathex_hmgplanehelper_native_imp.inc}
	       {$I vectormathex_hmgplanehelper_win64_sse_imp.inc}
       {$ENDIF}
    {$endif}  //unix
  {$else} // CPU32
     {$IFDEF USE_ASM_AVX}
	 {$I vectormathex_boundingbox_native_imp.inc}
         {$I vectormathex_boundingsphere_native_imp.inc}
         {$I vectormathex_axisaligned_boundingbox_native_imp.inc}
         {.$I vectormathex_boundingboxhelper_native_imp.inc}
         {.$I vectormathex_axisaligned_boundingBoxhelper_native_imp.inc}
         {$I vectormathex_frustum_native_imp.inc}
         {$I vectormathex_hmgplanehelper_native_imp.inc}
     {$ELSE}
         {$I vectormathex_boundingbox_native_imp.inc}
         {$I vectormathex_boundingsphere_native_imp.inc}
         {$I vectormathex_axisaligned_boundingbox_native_imp.inc}
         {.$I vectormathex_boundingboxhelper_native_imp.inc}
         {.$I vectormathex_axisaligned_boundingBoxhelper_native_imp.inc}
         {$I vectormathex_frustum_native_imp.inc}
         {$I vectormathex_hmgplanehelper_native_imp.inc}
     {$ENDIF}
  {$endif}
  {$else}  // pascal
     {$I vectormathex_2DSingleAffineMatrix_native_imp.inc}
     {$I vectormathex_boundingbox_native_imp.inc}
     {$I vectormathex_boundingsphere_native_imp.inc}
     {$I vectormathex_axisaligned_boundingbox_native_imp.inc}
     {.$I vectormathex_boundingboxhelper_native_imp.inc}
     {.$I vectormathex_axisaligned_boundingBoxhelper_native_imp.inc}
     {$I vectormathex_frustum_native_imp.inc}
     {$I vectormathex_hmgplanehelper_native_imp.inc}
  {$endif}

initialization
finalization

End.
