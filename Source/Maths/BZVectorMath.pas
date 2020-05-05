(*
  @abstract(Contient une bibliothèque de classes mathématique optimisée pour les vecteurs utilisant l'accélération SIMD (SSE, SSE3, SS4, AVX, AVX2) @br
  Elles peuvent être utilisées dans les graphiques 2D / 3D et tout autre application nécessitant des calculs mathématique avec des vecteurs.)

  Types de vecteurs supportés :@br
  @unorderedlist(
    @item(Vecteur 2D Integer, Single et Double)
    @item(Vecteur 3D Byte, Integer, and Single)
    @item(Vecteur 4D Byte, Integer, Single)
    @item(Matrice 2D Single)
    @item(Matrice 4D Single Matrix (@TODO Integer ????))
    @item(Quaternion)
    @item(Plan Homogène)
  )

  Contient également des fonctions identique au script GLSL / HLSL

  -------------------------------------------------------------------------------------------------------------

    @created(2017-11-25)
    @author(J.Delauney (BeanzMaster))
    @author(Peter Dyson (Dicepd))

    Historique : @br
    @unorderedList(
      @item(Last Update : 13/03/2018  )
      @item(25/11/2017 : Creation  )
    )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) : @br
  Remarquez, ici, nous devons redéfinir le mode d'arrondi FPC comme identique à notre code SSE. @br
  Dans FPC, la 'fonction Round' utilise l'algorithme 'd'arrondissement bancaire'. @br
  Il ne fait pas un "RoundUp" ou "RoundDown" si le paramètre d'entré est exactement x.50.

  Exemples : @br
    @code(Round(2.5) Resultat = 2) @br
    @code(Round(3.5) Resultat = 4)
  
  Pour plus d'informations voir : https://www.freepascal.org/docs-html/rtl/system/round.html @br

  @bold(Alignement des données sur la limite de 16 bits) : @br
  Sous x64, pour utiliser les fonctions vectorielle, vous aurez besoin d'ajouter des commandes de préprocesseur pour obtenir des données alignées appropriées.
  Normalement, ce n'est pas nécessaire sous x86. Mais c'est recommandé pour plus de sécurité.

  Dans l'en-tête des unité ajoutez : @br
  @longcode(#
  Unit MyUnit;
  {$mode objfpc}{$H+}
  {$ALIGN 16}
  {$CODEALIGN CONSTMIN=16}
  {$CODEALIGN LOCALMIN=16}
  {$CODEALIGN VARMIN=16}
  #)
  @OrderedList(
    @item(Pour les constantes : @br
    Vous devez d'entourer votre variable comme ceci : @br
    @longcode(#
    Const
      {$CODEALIGN CONSTMIN=16}
      cMyVector : TBZVector = (X:0.5;Y:0.5;Z:0.5;W:1.0);
      {$CODEALIGN CONSTMIN=4}
    #))
    @item(Pour des Variables ou champs à l'intérieur d'une classe : @br
    Vous devez d'entourer votre variable comme ceci : @br
    @longcode(#
    Type
      TMyClass = Class
      private
        {$CODEALIGN RECORMIN=16}
        FVec : TBZVector;
        FMatrix : TBZMatrix;
        {$CODEALIGN RECORMIN=4}
        FFactor : Single;
      public
        property Vec : TBZVector read FVec write FVec
      end;
    #))
    @item(Pour des variables dans des fonctions ou procédures : @br
    Vous devez d'entourer votre variable comme ceci :
    @longcode(#
      function MyFunc(A,B,C:Single):Single;
      var
        {$CODEALIGN VARMIN=16}
        MyTempVec : TBZVector
        {$CODEALIGN VARMIN=4}
      begin
        //...
      end;
    #))
   )
  -------------------------------------------------------------------------------------------------------------

  Quelques liens de référence : @br
  @unorderedList(
     @item(http://forum.lazarus.freepascal.org/index.php/topic,32741.0.html)
     @item(http://agner.org/optimize/)
     @item(http://www.songho.ca/misc/sse/sse.html)
     @item(https://godbolt.org);
     @item(http://softpixel.com/~cwright/programming/simd/sse.php)
     @item(https://www.gamasutra.com/view/feature/132636/designing_fast_crossplatform_simd_.php?page=3)
     @item(https://butterflyofdream.wordpress.com/2016/07/05/converting-rotation-matrices-of-left-handed-coordinate-system/)
     @item(http://shybovycha.tumblr.com/post/122400740651/speeding-up-algorithms-with-sse)
     @item(https://www.scratchapixel.com/index.php)
     @item(http://mark.masmcode.com)
     @item(https://www.cs.uaf.edu/courses/cs441/notes/sse-avx/)
     @item(http://www.euclideanspace.com)
     @item(https://www.3dgep.com/category/math/)
   )

   Quelques liens en français (in french) : @br
   @unorderedList(
     @item(http://villemin.gerard.free.fr/Wwwgvmm/Nombre.htm)
     @item(https://ljk.imag.fr/membres/Bernard.Ycart/mel/)
     @item(https://www.gladir.com/CODER/ASM8086/)
   )

   Autres articles, papiers intéressant sur certains points :@br
   @unorderedList(
     @item(https://conkerjo.wordpress.com/2009/06/13/spatial-hashing-implementation-for-fast-2d-collisions/)
     @item(https://realhet.wordpress.com/2016/11/02/fast-sse-3x3-median-filter-for-rgb24-images/)
     @item(http://www.jagregory.com/abrash-black-book/)
     @item(http://x86asm.net/articles/fixed-point-arithmetic-and-tricks/)
     @item(http://lolengine.net/blog/2011/3/20/understanding-fast-float-integer-conversions)
     @item(http://chrishecker.com/Miscellaneous_Technical_Articles)
     @item(http://catlikecoding.com/unity/tutorials/rendering/part-1/)
   )

   Vous trouverez d'autres article dans le dossier @bold(DocRefs) @br

  -------------------------------------------------------------------------------------------------------------

  @bold(Dependances) : BZMath, BZArrayClasses

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)@br
    @unorderedList(
      @item(FPC/Lazarus)
      @item(GLScene)
      @item(Tous les auteurs des liens et articles)
    )

  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
Unit BZVectorMath;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}


{.$INLINE ON}

//----------------------- DATA ALIGNMENT ---------------------------------------
{$ALIGN 16}
{$CODEALIGN CONSTMIN=16}
{$CODEALIGN LOCALMIN=16}
{$CODEALIGN VARMIN=16}
//------------------------------------------------------------------------------

{$MODESWITCH ADVANCEDRECORDS}
//==============================================================================

Interface

Uses
  Classes, Sysutils,
  BZArrayClasses;

//==============================================================================

Const
  { Constante pour la  normalisation d'un composant de couleur en byte vers un npmbre flottant }
  cColorFloatRatio : Single = 1/255;

{%region%----[ SSE States Flags Const ]----------------------------------------}

Type
  { Mode d'arrondissement du SIMD }
  sse_Rounding_Mode = (rmNearestSSE, rmFloorSSE, rmCeilSSE, rmDefaultSSE);

Const
  { SIMD mxcsr register bits }
  {@exclude}
  sse_FlagInvalidOp = %0000000000000001; 
  {@exclude}
  sse_FlagDenorm    = %0000000000000010;
  {@exclude}
  sse_FlagDivZero   = %0000000000000100;
  {@exclude}
  sse_FlagOverflow  = %0000000000001000;
  {@exclude}
  sse_FlagUnderflow = %0000000000010000;
  {@exclude}
  sse_FlagPrecision = %0000000000100000;
  {@exclude}
  sse_FlagDenormZero= %0000000001000000;
  {@exclude}
  sse_MaskInvalidOp = %0000000010000000;
  {@exclude}
  sse_MaskDenorm    = %0000000100000000;
  {@exclude}
  sse_MaskDivZero   = %0000001000000000;
  {@exclude}
  sse_MaskOverflow  = %0000010000000000;
  {@exclude}
  sse_MaskUnderflow = %0000100000000000;
  {@exclude}
  sse_MaskPrecision = %0001000000000000;
  {@exclude}
  sse_MaskNegRound  = %0010000000000000;
  {@exclude}
  sse_MaskPosRound  = %0100000000000000;
  {@exclude}
  sse_MaskZeroFlush = %1000000000000000;

  {@exclude} //masque pour enlever les anciens bits d'arrondi pour définir de nouveaux bits
  sse_no_round_bits_mask= $ffffffff-sse_MaskNegRound-sse_MaskPosRound;

  { Valeur par défaut du registre mxcsr après le démarrage du PC @ br
    Paramétrage par défaut du registre mxscr; désactivation de toutes les exceptions }
  mxcsr_default : dword =sse_MaskInvalidOp or sse_MaskDenorm  or sse_MaskDivZero or sse_MaskOverflow
                      or sse_MaskUnderflow or sse_MaskPrecision or $00000000;// ; //sse_MaskPosRound; //
  {@exclude}
  mxcsr_default_TEST : dword =sse_MaskInvalidOp and sse_MaskDenorm  or sse_MaskDivZero or sse_MaskOverflow
                      or sse_MaskUnderflow or sse_MaskPrecision or $00000000 and sse_MaskZeroFlush; //sse_MaskPosRound;

  { Table de conversion du nom du mode d'arrondissement vers bits d'arrondi }
  sse_Rounding_Flags: array [sse_Rounding_Mode] of longint = (0,sse_MaskNegRound,sse_MaskPosRound,0);


  sse_align=16; //< Constant d'alignement des données SIMD en bits
  sse_align_mask=sse_align-1; //< Mask pour l'alignement des données

{%endregion%}

{%region%----[ Vectors ]-------------------------------------------------------}

type
  { Tableau aligné pour les vecteur 2D Single }
  TBZVector2fType = packed array[0..1] of Single;
  { Tableau aligné pour les vecteur 2D Double }
  TBZVector2dType = packed array[0..1] of Double;
  { Tableau aligné pour les vecteur 2D Integer }
  TBZVector2iType = packed array[0..1] of Integer;

  { Tableau aligné pour les vecteur 3D Single }
  TBZVector3fType = packed array[0..2] of Single;
  { Tableau aligné pour les vecteur 3D Integer }
  TBZVector3iType = packed Array[0..2] of Longint;
  { Tableau aligné pour les vecteur 3D Byte }
  TBZVector3bType = packed Array[0..2] of Byte;

  { Tableau aligné pour les vecteur 4D Single }
  TBZVector4fType = packed array[0..3] of Single;
  { Tableau aligné pour les vecteur 4D Integer }
  TBZVector4iType = packed array[0..3] of Longint;
  { Tableau aligné pour les vecteur 4D Byte }
  TBZVector4bType = packed Array[0..3] of Byte;

  { Référence pour le mélange des composants d'un vecteur (swizzle/shuffle) 2D }
  TBZVector2SwizzleRef = (swDefaultSwizzle2, swXX, swYY, swXY, swYX);

  { Référence pour le mélange des composants d'un vecteur (swizzle/shuffle) 3D }
  TBZVector3SwizzleRef = (swDefaultSwizzle3,
    swXXX, swYYY, swZZZ, swXYZ, swXZY, swZYX, swZXY, swYXZ, swYZX,
    swRRR, swGGG, swBBB, swRGB, swRBG, swBGR, swBRG, swGRB, swGBR);

  { Référence pour le mélange des composants d'un vecteur (swizzle/shuffle) 4D }
  TBZVector4SwizzleRef = (swDefaultSwizzle4,
    swXXXX, swYYYY, swZZZZ, swWWWW,
    swXYZW, swXZYW, swZYXW, swZXYW, swYXZW, swYZXW,
    swWXYZ, swWXZY, swWZYX, swWZXY, swWYXZ, swWYZX,
    swRRRR, swGGGG, swBBBB, swAAAA,
    swRGBA, swRBGA, swBGRA, swBRGA, swGRBA, swGBRA,
    swARGB, swARBG, swABGR, swABRG, swAGRB, swAGBR);

  { Définition d'un Vecteur 2D de type Integer }
  TBZVector2i = record

    { Initialisation des valeurs X et Y }
    procedure Create(aX, aY:Integer); overload;

    { Retourne une chaine de caractères formaté représentant le vecteur : '(x, y)' }
    function ToString : String;

    { Ajoute deux vecteurs TBZVector2i}
    class operator +(Constref A, B: TBZVector2i): TBZVector2i;overload;
    { Soustrait deux vecteurs TBZVector2i}
    class operator -(constref A, B: TBZVector2i): TBZVector2i; overload;
    { Multiplie deux vecteurs TBZVector2i}
    class operator *(constref A, B: TBZVector2i): TBZVector2i; overload;
    { Divise deux vecteurs TBZVector2i}
    class operator Div(constref A, B: TBZVector2i): TBZVector2i; overload;
    { Divise un vecteurs TBZVector2i avec une variable de type Integer}
    class operator Div(constref A: TBZVector2i; Constref B:Integer): TBZVector2i; overload;
    { Ajoute un vecteurs TBZVector2i avec une variable de type Integer}
    class operator +(Constref A: TBZVector2i; constref B:Integer): TBZVector2i;overload;
    { Ajoute une variable de type Single a un vecteurs TBZVector2i}
    class operator +(Constref A: TBZVector2i; constref B:Single): TBZVector2i; overload;
    { Soustrait une variable de type Integer a un vecteurs TBZVector2i}
    class operator -(constref A: TBZVector2i; constref B:Integer): TBZVector2i; overload;
    { Soustrait une variable de type Single a un vecteurs TBZVector2i }
    class operator -(constref A: TBZVector2i; constref B:Single): TBZVector2i; overload;
    { Multiplie un vecteur TBZVector2i avec une variable de type Integer }
    //class operator *(constref A: TBZVector2i; constref B:Integer): TBZVector2i; overload;
    { Multiplie un vecteurs TBZVector2i avec une variable de type Single}
    class operator *(constref A: TBZVector2i; constref B:Single): TBZVector2i; overload;
    { Divise un vecteurs TBZVector2i avec une variable de type Single }
    class operator /(constref A: TBZVector2i; constref B:Single): TBZVector2i; overload;
    { Inverse le signe des composantes du vecteur }
    class operator -(constref A: TBZVector2i): TBZVector2i; overload;

    { Compare si deux variables de type  TBZVector2i sont égales.
      Retourne @True si la condition est vérifiée.
      Chaque composant du vecteur est vérifié}
    class operator =(constref A, B: TBZVector2i): Boolean; overload;
    { Compare si  deux variables de type  TBZVector2i ne sont pas égales.
      Retourne @True si la condition est vérifiée.
      Chaque composant du vecteur est vérifié}
    class operator <>(constref A, B: TBZVector2i): Boolean; overload;

    { Retourne le reste de chaque composante de la division de deux vecteur }
    class operator mod(constref A, B : TBZVector2i): TBZVector2i; overload;

    { Retourne le minimum de chaque composants entre le vecteur courrant et un autre TBZVector2i }
    function Min(constref B: TBZVector2i): TBZVector2i; overload;
    { Retourne le minimum de chaque composants entre vecteur courrant et une valeur de type Integer }
    function Min(constref B: Integer): TBZVector2i; overload;
    { Retourne le maximum de chaque composants entre vecteur courrant et une valeur de type TBZVector2i }
    function Max(constref B: TBZVector2i): TBZVector2i; overload;
    { Retourne le maximum de chaque composants entre le vecteur courrant et une valeur de type Integer }
    function Max(constref B: Integer): TBZVector2i; overload;
    { S'assure que chaque composant du vecteur courrant soit compris en les valeur définit par les valeurs de type TBZVector2i "AMin" et "AMax" }
    function Clamp(constref AMin, AMax: TBZVector2i): TBZVector2i;overload;
    { S'assure que chaque composant du vecteur courrant soit compris en les valeur de type Integer "AMin" et "AMax" }
    function Clamp(constref AMin, AMax: Integer): TBZVector2i;overload;

    { Multiplie le vecteur courrant par un autre TBZVector2i et ajoute un deuxieme TBZVector2i au résultat }
    function MulAdd(constref A,B:TBZVector2i): TBZVector2i;
    { Multiplie le vecteur courrant  par un autre TBZVector2i et divise le resultat par un deuxieme TBZVector2i }
    function MulDiv(constref A,B:TBZVector2i): TBZVector2i;

    { Retourne la longueur du vecteur }
    function Length:Single;
    { Retourne la longueur carré du vecteur }
    function LengthSquare:Single;
    { Retourne la distance entre le vecteur courrant et un autre TBZVector2i }
    function Distance(constref A:TBZVector2i):Single;
    { Retourne la distance carré entre le vecteur courrant et un autre TBZVector2i }
    function DistanceSquare(constref A:TBZVector2i):Single;

    { Retourne le produit scalaire du vecteur courrent avec un autre TBZVector2i }
    function DotProduct(A:TBZVector2i):Single;

    { Retourne l'angle en "Degré" entre le vecteur courrant et un autre TBZVector2i relatif a un autre TBZVector2i décrivant le centre de rotation }
    function AngleBetween(Constref A, ACenterPoint : TBZVector2i): Single;
    { Retourne le cosinus de l'angle entre le vecteur courrant et un autre TBZVector2i }
    function AngleCosine(constref A: TBZVector2i): Single;

    { Retourne la valeur absolue de chaque composant du vecteur courrant }
    function Abs:TBZVector2i;overload;

    { Accès aux valeurs du vecteur }
    case Byte of
      0: (V: TBZVector2iType);      //< Acces via tableau
      1: (X, Y : Integer);          //< Acces par défaut
      2: (Width, Height : Integer); //< Propriétés de convenance pour la manipulation de taille
  end;
  { Pointeur vers un TBZVector2i }
  PBZVector2i = ^TBZVector2i;

  { Définition d'un vecteur 2D de type Single }

  { TBZVector2f }

  TBZVector2f =  record
    { Initialisation des valeurs X et Y }
    procedure Create(aX,aY: single);

    { Retourne une chaine de caractères formaté représentant le vecteur : '(x, y)' }
    function ToString : String;

    { Ajoute deux vecteurs de type TBZVector2f }
    class operator +(constref A, B: TBZVector2f): TBZVector2f; overload;
    { Ajoute un vecteur de type TBZVector2f à un vecteur de type TBZVector2i }
    class operator +(constref A: TBZVector2f; constref B: TBZVector2i): TBZVector2f; overload;
    { Soustrait deux vecteurs de type TBZVector2f }
    class operator -(constref A, B: TBZVector2f): TBZVector2f; overload;
    { Soustrait un vecteur de type TBZVector2f à un vecteur de type TBZVector2i }
    class operator -(constref A: TBZVector2f; constref B: TBZVector2i): TBZVector2f; overload;
    { Multiplie deux vecteurs de type TBZVector2f }
    class operator *(constref A, B: TBZVector2f): TBZVector2f; overload;
    { Multiplie un vecteur de type TBZVector2f avec un vecteur de type TBZVector2i }
    class operator *(constref A:TBZVector2f; Constref B: TBZVector2i): TBZVector2f; overload;
    { Divise deux vecteurs de type TBZVector2f }
    class operator /(constref A, B: TBZVector2f): TBZVector2f; overload;
    { Ajoute chaque composant du vecteur de type TBZVector2f avec une valeur de type Single }
    class operator +(constref A: TBZVector2f; constref B:Single): TBZVector2f; overload;
    { Soustrait chaque composant du vecteur de type TBZVector2f avec une valeur de type Single }
    class operator -(constref A: TBZVector2f; constref B:Single): TBZVector2f; overload;
    { Multiplie chaque composant du vecteur de type TBZVector2f par une valeur de type Single }
    class operator *(constref A: TBZVector2f; constref B:Single): TBZVector2f; overload;
    { Divise chaque composant du vecteur de type TBZVector2f une valeur de type Single }
    class operator /(constref A: TBZVector2f; constref B:Single): TBZVector2f; overload;
    { Divise un vecteur de type TBZVector2f avec un vecteur de type TBZVector2i }
    class operator /(constref A: TBZVector2f; constref B: TBZVector2i): TBZVector2f; overload;
    { Inverse le signe des composants du vecteur }
    class operator -(constref A: TBZVector2f): TBZVector2f; overload;

    { Compare si deux variables de type TBZVector2f sont égales.
      Retourne @True si la condition est vérifiée.
      Chaque composant du vecteur est vérifié}
    class operator =(constref A, B: TBZVector2f): Boolean;
    { Compare si deux variables de type TBZVector2f ne sont pas égales.
      Retourne @True si la condition est vérifiée.
      Chaque composant du vecteur est vérifié}
    class operator <>(constref A, B: TBZVector2f): Boolean;

    //class operator mod(const a,b:TBZVector2f): TBZVector2f;

    { Retourne le minimum de chaque composant entre le vecteur courrant et un autre TBZVector2f }
    function Min(constref B: TBZVector2f): TBZVector2f; overload;
    { Retourne le minimum de chaque composant entre vecteur courrant et un valeur de type Single }
    function Min(constref B: Single): TBZVector2f; overload;
    { Retourne le maximum de chaque composant entre le vecteur courrant et un autre TBZVector2f }
    function Max(constref B: TBZVector2f): TBZVector2f; overload;
    { Retourne le maximum de chaque composant entre vecteur courrant et un valeur de type Single }
    function Max(constref B: Single): TBZVector2f; overload;
    { S'assure que chaque composant du vecteur courrant soit compris en les valeur définit par les valeurs de type TBZVector2f "AMin" et "AMax" }
    function Clamp(constref AMin, AMax: TBZVector2f): TBZVector2f;overload;
    { S'assure que chaque composant du vecteur courrant soit compris en les valeur de type Single "AMin" et "AMax" }
    function Clamp(constref AMin, AMax: Single): TBZVector2f;overload;

    { Multiplie le vecteur courrant par un autre TBZVector2f et ajoute un deuxieme TBZVector2f au résultat }
    function MulAdd(constref A,B:TBZVector2f): TBZVector2f;
    { Multiplie le vecteur courrant par un autre TBZVector2f et soustrait un deuxieme TBZVector2f au résultat }
    function MulSub(constref A,B:TBZVector2f): TBZVector2f;
    { Multiplie le vecteur courrant par un autre TBZVector2f et divise le résultat par un deuxieme TBZVector2f }
    function MulDiv(constref A,B:TBZVector2f): TBZVector2f;

    { Retourne la longueur du vecteur }
    function Length:Single;
    { Retourne la longueur carré du vecteur }
    function LengthSquare:Single;
    { Retourne la distance (Cartésienne) entre le vecteur courrant et un autre TBZVector2f }
    function Distance(constref A:TBZVector2f):Single;
    { Retourne la distance (Cartésienne) carré entre le vecteur courrant et un autre TBZVector2f }
    function DistanceSquare(constref A:TBZVector2f):Single;
    { Retourne la distance de Manhattan. @br
      @bold(Note) : La distance de Manahattan est la distance de "marche" entre deux points. @br
      Aucun déplacement angulaire n'est autorisé. (applicable uniquement pour des mouvements horizontaux et verticaux)}
    function ManhattanDistance(constref A : TBZVector2f) : Single;
    { Retourne la distance de Chebyshev. Similaire à ManhattanDistance. @br
      Sauf que le mouvement diagonal est autorisé. @br
      @bold(Note) : Cette fonction est aussi appelée "distance de l'échiquier" car elle correspond au nombre de mouvements
       qu'une pièce du roi doit effectuer pour se déplacer entre deux points.}
    function ChebyshevDistance(constref A : TBZVector2f) : Single;


    function MinkovskiDistance(constref A : TBZVector2f): Single;

    { Retourne le barycentre pondéré. @br
      cf : https://fr.wikibooks.org/wiki/Manuel_de_géométrie_vectorielle/Coordonnées_du_barycentre }
    function BaryCenter(constRef B : TBZVector2f; const WeightA : Single = 1.0; Const WeightB : Single = 1.0) : TBZVector2f;

    function Center(ConstRef B : TBZVector2f) : TBZVector2f;

    { Retourne le vecteur courrant normalisé }
    function Normalize : TBZVector2f;

    //----------------------------------------------------------------------------------------------------------------
    // NOTE : Le produit en croix d'un vecteur 2D n'existe pas.
    // Nous substituons cette opération en deux fonctions afin de pouvoir simuler le produit en croix d'un vecteur 3D

    { Retourne la composante Z (Magnitude) d'un produit en croix d'un vecteur 3D. Nous supposons ici que la valeur "Z" est egale à 0 }
    function Perp(A : TBZVector2f) : Single;

    { Retourne le produit en croix du vecteur
      Equivalent au produit en croix d'un vecteur 3D. (Pour simuler le calcul et obtenir un résultat, on "suppose" que la valeur "Z" est égale à 1)
      Pour obtenir la valeur de "Z" (magnitude) du produit en croix d'un vecteur 3D il faut utiliser la fonction Perp }
    function CrossProduct(A : TBZVector2f): TBZVector2f;

    //----------------------------------------------------------------------------------------------------------------

    { Retourne le produit scalaire du vecteur courrant avec un autre TBZVector2f }
    function DotProduct(A:TBZVector2f):Single;

    { Retourne l'angle en "Degré" entre le vecteur courrant et un autre TBZVector2f relatif a un autre TBZVector2f décrivant le centre de rotation }
    function AngleBetween(Constref A, ACenterPoint : TBZVector2f): Single;
    { Retourne l'angle en "Degré" entre le vecteur courrant et un autre TBZVector2f. Note le centre de rotation se trouve à la position (0,0) }
    function AngleBetweenPointsInDeg(C : TBZVector2f):Single;
    { Retourne le cosinus de l'angle entre le vecteur courrant et un autre TBZVector2f }
    function AngleCosine(constref A: TBZVector2f): Single;

    { Retourne le point après rotation du vecteur de "anAngle" en degré par rapport au centre de rotation "aCenterPoint }
    function Rotate(anAngle : Single; aCenterPoint : TBZVector2f) : TBZVector2f;

    // function Reflect(I, NRef : TVector2f):TVector2f
    // function Edge(ConstRef A, B : TBZVector2f):Single; // @TODO : a passer dans TBZVector2fHelper ???

    { Retourne la valeur absolue de chaque composant du vecteur courrant }
    function Abs:TBZvector2f;overload;

    { Retourne la valeur arrondit de chaque composant du vecteur courrant dans un vecteur de type TBZVector2i }
    function Round: TBZVector2i; overload;
    { Retourne la valeur tronqué de chaque composant du vecteur courrant dans un vecteur de type TBZVector2i }
    function Trunc: TBZVector2i; overload;
    { Retourne la valeur arrondit tendant vers le négatif de chaque composant du vecteur courrant dans un vecteur de type TBZVector2i }
    function Floor: TBZVector2i; overload;
    { Retourne la valeur arrondit tendant vers le positif de chaque composant du vecteur courrant dans un vecteur de type TBZVector2i }
    function Ceil : TBZVector2i; overload;

    { Retourne la partie fractionnaire de chaque composant du vecteur courrant }
    function Fract : TBZVector2f; overload;

    { Retourne le dividende de chaque composant du vecteur courrant avec comme diviseur un autre vecteur de type TBZVector2f }
    function Modf(constref A : TBZVector2f): TBZVector2f;
    { Retourne le dividende de chaque composant du vecteur courrant avec comme diviseur un autre vecteur de type TBZVector2i ( TODO : Overload modf ou fmod ???) }
    function fMod(Constref A : TBZVector2f): TBZVector2i;

    { Retourne la racine carré de chaque composant du vecteur courrant }
    function Sqrt : TBZVector2f; overload;

    { Retourne la racine carré inverse de chaque composant du vecteur courrant }
    function InvSqrt : TBZVector2f; overload;

    { Accès aux valeurs du vecteur }
    case Byte of
      0: (V: TBZVector2fType);     //< Acces via tableau
      1: (X, Y : Single);          //< Acces par défaut
      2: (Width, Height : Single); //< Propriétés de convenance pour la manipulation de taille
  End;

  { Définition de convenance pour la représentation d'un point 2D en virgule flottante }
  TBZFloatPoint = TBZVector2f;

  { Définition d'un vecteur 2D de type Double }
  TBZVector2d =  record
    { Initialisation des valeurs X et Y }
    procedure Create(aX,aY: Double);

    { Retourne une chaine de caractères formaté représentant le vecteur : '(x, y)' }
    function ToString : String;

    { Ajoute deux vecteurs de type TBZVector2d }
    class operator +(constref A, B: TBZVector2d): TBZVector2d; overload;
    { Ajoute un vecteur de type TBZVector2d à un vecteur de type TBZVector2i }
    class operator +(constref A: TBZVector2d; constref B: TBZVector2i): TBZVector2d; overload;
    { Soustrait deux vecteurs de type TBZVector2f }
    class operator -(constref A, B: TBZVector2d): TBZVector2d; overload;
    { Soustrait un vecteur de type TBZVector2d à un vecteur de type TBZVector2i }
    class operator -(constref A: TBZVector2d; constref B: TBZVector2i): TBZVector2d; overload;
    { Multiplie deux vecteurs de type TBZVector2d }
    class operator *(constref A, B: TBZVector2d): TBZVector2d; overload;
    { Multiplie un vecteur de type TBZVector2d avec un vecteur de type TBZVector2i }
    class operator *(constref A:TBZVector2d; Constref B: TBZVector2i): TBZVector2d; overload;
    { Divise deux vecteurs de type TBZVector2d }
    class operator /(constref A, B: TBZVector2d): TBZVector2d; overload;
    { Ajoute chaque composant du vecteur de type TBZVector2d avec une valeur de type Double }
    class operator +(constref A: TBZVector2d; constref B:Double): TBZVector2d; overload;
    { Soustrait chaque composant du vecteur de type TBZVector2d avec une valeur de type Double }
    class operator -(constref A: TBZVector2d; constref B:Double): TBZVector2d; overload;
    { Multiplie chaque composant du vecteur de type TBZVector2d par une valeur de type Double }
    class operator *(constref A: TBZVector2d; constref B:Double): TBZVector2d; overload;
    { Divise chaque composant du vecteur de type TBZVector2d par une valeur de type Double }
    class operator /(constref A: TBZVector2d; constref B:Double): TBZVector2d; overload;
    { Divise un vecteur de type TBZVector2d avec un vecteur de type TBZVector2i }
    class operator /(constref A: TBZVector2d; constref B: TBZVector2i): TBZVector2d; overload;
    { Inverse le signe des composants du vecteur }
    class operator -(constref A: TBZVector2d): TBZVector2d; overload;

    { Compare si deux variables de type TBZVector2d sont égales.
      Retourne @True si la condition est vérifiée. @br
      Chaque composant du vecteur est vérifié }
    class operator =(constref A, B: TBZVector2d): Boolean;
    { Compare si deux variables de type TBZVector2d ne sont pas égales.
      Retourne @True si la condition est vérifiée. @br
      Chaque composant du vecteur est vérifié }
    class operator <>(constref A, B: TBZVector2d): Boolean;

    //class operator mod(const a,b:TBZVector2d): TBZVector2d;

    { Retourne le minimum de chaque composant entre le vecteur courrant et un autre TBZVector2d }
    function Min(constref B: TBZVector2d): TBZVector2d; overload;
    { Retourne le minimum de chaque composant entre vecteur courrant et un valeur de type Double }
    function Min(constref B: Double): TBZVector2d; overload;
    { Retourne le maximum de chaque composant entre le vecteur courrant et un autre TBZVector2d }
    function Max(constref B: TBZVector2d): TBZVector2d; overload;
    { Retourne le maximum de chaque composant entre vecteur courrant et un valeur de type Double }
    function Max(constref B: Double): TBZVector2d; overload;
    { S'assure que chaque composant du vecteur courrant soit compris en les valeur définit par les valeurs de type TBZVector2d "AMin" et "AMax" }
    function Clamp(constref AMin, AMax: TBZVector2d): TBZVector2d;overload;
    { S'assure que chaque composant du vecteur courrant soit compris en les valeur de type Double "AMin" et "AMax" }
    function Clamp(constref AMin, AMax: Double): TBZVector2d;overload;

    { Multiplie le vecteur courrant par un autre TBZVector2d et ajoute un deuxieme TBZVector2d au résultat }
    function MulAdd(constref A,B:TBZVector2d): TBZVector2d;
    { Multiplie le vecteur courrant par un autre TBZVector2d et soustrait un deuxieme TBZVector2d au résultat }
    function MulSub(constref A,B:TBZVector2d): TBZVector2d;
    { Multiplie le vecteur courrant par un autre TBZVector2d et divise le résultat par un deuxieme TBZVector2d }
    function MulDiv(constref A,B:TBZVector2d): TBZVector2d;

    { Retourne la longueur du vecteur }
    function Length:Double;
    { Retourne la longueur carré du vecteur }
    function LengthSquare:Double;
    { Retourne la distance entre le vecteur courrant et un autre TBZVector2d }
    function Distance(constref A:TBZVector2d):Double;
    { Retourne la distance carré entre le vecteur courrant et un autre TBZVector2d }
    function DistanceSquare(constref A:TBZVector2d):Double;
    { Retourne le vecteur courrant normalisé }
    function Normalize : TBZVector2d;
    { Retourne le produit scalaire du vecteur courrant avec un autre TBZVector2d }
    function DotProduct(A:TBZVector2d):Double;

    { Retourne l'angle en "Degré" entre le vecteur courrant et un autre TBZVector2d relatif a un autre TBZVector2d décrivant le centre de rotation }
    function AngleBetween(Constref A, ACenterPoint : TBZVector2d): Double;
    { Retourne l'angle en "Degré" entre le vecteur courrant et un autre TBZVector2f. Note le centre de rotation se trouve à la position (0,0) }
    //function AngleBetweenPointsInDeg(C : TBZVector2f):Single;
    { Retourne le cosinus de l'angle entre le vecteur courrant et un autre TBZVector2d }
    function AngleCosine(constref A: TBZVector2d): Double;

    // function Reflect(I, NRef : TVector2f):TVector2f
    // function Edge(ConstRef A, B : TBZVector2d):Single; // @TODO : a passer dans TBZVector2dHelper ???

    { Retourne la valeur absolue de chaque composant du vecteur courrant }
    function Abs:TBZVector2d;overload;

    { Retourne la valeur arrondie de chaque composant du vecteur courrant dans un vecteur de type TBZVector2i }
    function Round: TBZVector2i; overload;
    { Retourne la valeur tronquée de chaque composant du vecteur courrant dans un vecteur de type TBZVector2i }
    function Trunc: TBZVector2i; overload;
    { Retourne la valeur arrondie tendant vers le négatif de chaque composant du vecteur courrant dans un vecteur de type TBZVector2i }
    function Floor: TBZVector2i; overload;
    { Retourne la valeur arrondie tendant vers le positif de chaque composant du vecteur courrant dans un vecteur de type TBZVector2i }
    function Ceil : TBZVector2i; overload;
    { Retourne la partie fractionnaire de chaque composant du vecteur courrant }
    function Fract : TBZVector2d; overload;

    { Retourne le dividende de chque composant du vecteur courrant avec comme diviseur un autre vecteur de type TBZVector2d }
    function Modf(constref A : TBZVector2d): TBZVector2d;
    { Retourne le dividende de chque composant du vecteur courrant avec comme diviseur un autre vecteur de type TBZVector2i ( TODO : Overload modf ou fmod ???) }
    function fMod(Constref A : TBZVector2d): TBZVector2i;

    { Retourne la racine carré de chaque composant du vecteur courrant }
    function Sqrt : TBZVector2d; overload;
    { Retourne la racine carré inverse de chaque composant du vecteur courrant }
    function InvSqrt : TBZVector2d; overload;

    { Accès aux valeurs du vecteur }
    case Byte of
      0: (V: TBZVector2dType);     //< Acces via tableau
      1: (X, Y : Double);          //< Acces par défaut
      2: (Width, Height : Double); //< Propriétés de convenance pour la manipulation de taille
  End;

  { Représentation d'un nombre complex }

  { TBZComplexVector }

  TBZComplexVector = record
    { Creation d'un nombre complex }
    procedure Create(ARealPart, AnImaginaryPart   : Double);
    { Creation d'un nombre complex depuis un TBZVector2d ou X est la partie Reelle et Y la partie Imaginaire }
    procedure Create(AValue : TBZVector2d);

    { Retourne une chaine de caractères formaté représentant le vecteur : '(RealPart, ImaginaryPart)' }
    function ToString : String;

    { Ajoute deux nombre complex }
    class operator +(constref A, B: TBZComplexVector): TBZComplexVector; overload;
    { Soustrait deux nombre complex }
    class operator -(constref A, B: TBZComplexVector): TBZComplexVector; overload;
    { Multiplie deux nombre complex }
    class operator *(constref A, B: TBZComplexVector): TBZComplexVector; overload;
    { Multiplie le nombre complex avec une valeur de type Single (Mise à l'echelle/Scale)}
    class operator *(constref A : TBZComplexVector; constref B : Single): TBZComplexVector; overload;
    { Divise deux nombre complex }
    class operator /(constref A, B: TBZComplexVector): TBZComplexVector; overload;

    { Compare si deux nombre complex sont égales.
      Retourne @True si la condition est vérifiée. @br
      Chaque composant du vecteur est vérifié }
    class operator =(constref A, B: TBZComplexVector): Boolean;
    { Compare si deux nombre complex ne sont pas égales.
      Retourne @True si la condition est vérifiée. @br
      Chaque composant du vecteur est vérifié }
    class operator <>(constref A, B: TBZComplexVector): Boolean;

    { Negation de la partie Reel }
    function Conjugate : TBZComplexVector;
    { Retourne la longueur (magnitude) du nombre complex }
    function Length : Double;
    { Retourne la phase du nombre complex (Z) (en radian, dans l'interval [-pi, pi]) }
    function Phase : Double;

    { Retourne le cosinus du nombre complexe }
    function Cosinus : TBZComplexVector;
    { Retourne le sinus du nombre complexe }
    function Sinus : TBZComplexVector;

    { Retourne le polynome de "Degree" du nombre complexe }
    function Polynome(Degree : Integer) : TBZComplexVector;

    Case Byte of
      0: (V: TBZVector2dType);        //< Acces via tableau
      1: (RealPart, ImaginaryPart  : Double);  //< Acces par défaut
      2: (AsVector2D : TBZVector2D);  //< Comme une TBZVector2D
  end;

  { Définition d'un vecteur 3D de type Byte. }
  TBZVector3b = Record
  public 
    { Initialisation des valeurs X, Y et Z}
    procedure Create(const aX, aY, aZ: Byte);

    { Retourne une chaine de caractères formaté représentant le vecteur : '(x, y, z)' }
    function ToString : String;
    
    { Ajoute deux vecteurs de type TBZVector3b }
    class operator +(constref A, B: TBZVector3b): TBZVector3b; overload;
    { Soustrait deux vecteurs de type TBZVector3b }
    class operator -(constref A, B: TBZVector3b): TBZVector3b; overload;
    { Multiplie deux vecteurs de type TBZVector3b }
    class operator *(constref A, B: TBZVector3b): TBZVector3b; overload;
    { Divise deux vecteurs de type TBZVector3b }
    class operator Div(constref A, B: TBZVector3b): TBZVector3b; overload;
    { Ajoute chaque composant du vecteur de type TBZVector2d avec une valeur de type Byte }
    class operator +(constref A: TBZVector3b; constref B:Byte): TBZVector3b; overload;
    { Soustrait chaque composant du vecteur de type TBZVector2d avec une valeur de type Byte }
    class operator -(constref A: TBZVector3b; constref B:Byte): TBZVector3b; overload;
    { Multiplie chaque composant du vecteur de type TBZVector2d avec une valeur de type Byte }
    class operator *(constref A: TBZVector3b; constref B:Byte): TBZVector3b; overload;
    { Multiplie chaque composant du vecteur de type TBZVector2d avec une valeur de type Single }
    class operator *(constref A: TBZVector3b; constref B:Single): TBZVector3b; overload;
    { Divise chaque composant du vecteur de type TBZVector2d avec une valeur de type Byte }
    class operator Div(constref A: TBZVector3b; constref B:Byte): TBZVector3b; overload;

    { Compare si deux variables de type TBZVector3b sont égales.
      Retourne @True si la condition est vérifiée. @br
      Chaque composant du vecteur est vérifié }
    class operator =(constref A, B: TBZVector3b): Boolean;
    { Compare si deux variables de type TBZVector3b ne sont pas égales.
      Retourne @True si la condition est vérifiée. @br
      Chaque composant du vecteur est vérifié }
    class operator <>(constref A, B: TBZVector3b): Boolean;

    { Opérateur logique ET (AND) entre deux TBZVector3b }
    class operator And(constref A, B: TBZVector3b): TBZVector3b; overload;
    { Opérateur logique OU (OR) entre deux TBZVector3b }
    class operator Or(constref A, B: TBZVector3b): TBZVector3b; overload;
    {  Opérateur logique OU EXCLUSIF (XOR) entre deux TBZVector3b }
    class operator Xor(constref A, B: TBZVector3b): TBZVector3b; overload;
    { Opérateur logique ET (AND) entre un TBZVector3b et une valeur de type Byte }
    class operator And(constref A: TBZVector3b; constref B:Byte): TBZVector3b; overload;
    { Opérateur logique OR (OR) entre un TBZVector3b et une valeur de type Byte }
    class operator or(constref A: TBZVector3b; constref B:Byte): TBZVector3b; overload;
    {  Opérateur logique OU EXCLUSIF (XOR) entre un TBZVector3b et une valeur de type Byte }
    class operator Xor(constref A: TBZVector3b; constref B:Byte): TBZVector3b; overload;

    { Mélange les composants (swizzle)en fonction du paramètre de type TBZVector3SwizzleRef }
    function Swizzle(Const ASwizzle : TBZVector3SwizzleRef): TBZVector3b;

    { Acces aux propriétés }
    Case Byte of
      0 : (V:TBZVector3bType);      //< Acces via un tableau
      1 : (X, Y, Z:Byte);           //< Acces par défaut
      2 : (Red, Green, Blue:Byte);  //< Acces comme une couleur au format RGB
  end;

  { Définition de convenace d'un vecteur 3D de type Integer non étendu. Juste pour les accès aux valeurs.
    Pour les manipulations, en lieu et place utilisez plutôt un vecteur de type TBZVector4i }
  TBZVector3i = record
  { Acces aux propriétés }
  case Byte of
    0: (V: TBZVector3iType);          //< Acces via tableau
    1: (X, Y, Z : Integer);           //< Acces par défaut
    2: (Red, Green, Blue : Integer);  //< Acces comme une couleur au format RGB
  end;

  { Définition de convenace d'un vecteur 3D de type Single non étendu. Juste pour les accès aux valeurs.
    Pour les manipulations, en lieu et place utilisez plutôt un vecteur de type TBZVector4f }
  TBZVector3f =  record
  public
    { Retourne une chaine de caractères formaté représentant le vecteur : '(x, y, z)' }
    function ToString : String;

    { Acces aux propriétés }
    case Byte of
      0: (V: TBZVector3fType);        //< Acces via tableau
      1: (X, Y, Z: Single);           //< Acces par défaut
      2: (Red, Green, Blue: Single);  //< Acces comme une couleur au format RGB
  End;

  { Définition de convenance pratique pour décrire un vecteur affine de type TBZVector3f }
  TBZAffineVector = TBZVector3f;
  PBZAffineVector = ^TBZAffineVector; //< Pointeur vers TBZAffineVector

  { Définition d'un vecteur 4D de type Byte. }
  TBZVector4b = Record
  public
    
    { Initialisation des valeurs X, Y, Z et W @br
      Par défaut la valeur W est mise à 255 }
    procedure Create(Const aX,aY,aZ: Byte; const aW : Byte = 255); overload;
    { Creation depuis un TBZVector3b et W @br
      Par défaut la valeur W est mise à 255 }
    procedure Create(Const aValue : TBZVector3b; const aW : Byte = 255); overload;
   
    { Retourne une chaine de caractères formaté représentant le vecteur : '(x, y, z, w)' }
    function ToString : String;
    
    { Ajoute deux TBZVector4b }
    class operator +(constref A, B: TBZVector4b): TBZVector4b; overload;
    { Soustrait deux TBZVector4b }
    class operator -(constref A, B: TBZVector4b): TBZVector4b; overload;
    { Multiplie deux TBZVector4b }
    class operator *(constref A, B: TBZVector4b): TBZVector4b; overload;
    { Divise deux TBZVector4b }
    class operator Div(constref A, B: TBZVector4b): TBZVector4b; overload;
    { Ajoute à chaque composant une variable de type Byte }
    class operator +(constref A: TBZVector4b; constref B:Byte): TBZVector4b; overload;
    { Soustrait à chaque composant une variable de type Byte }
    class operator -(constref A: TBZVector4b; constref B:Byte): TBZVector4b; overload;
    { Multiplie chaque composant par variable de type Byte }
    class operator *(constref A: TBZVector4b; constref B:Byte): TBZVector4b; overload;
    { Multiplie chaque composant par variable de type Single. Les valeurs sont arrondis et leur interval vérifié }
    class operator *(constref A: TBZVector4b; constref B:Single): TBZVector4b; overload;
    { Divise chaque composant par variable de type Byte }
    class operator Div(constref A: TBZVector4b; constref B:Byte): TBZVector4b; overload;
    
    { Retourne @True si le vecteur A est égale au vecteur B }
    class operator =(constref A, B: TBZVector4b): Boolean;
    { Retourne @True si le vecteur A n'est pas égale au vecteur B }
    class operator <>(constref A, B: TBZVector4b): Boolean;
    
    { Opérateur logique ET (AND) entre deux TBZVector4b }
    class operator And(constref A, B: TBZVector4b): TBZVector4b; overload;
    { Opérateur logique OU (OR) entre deux TBZVector4b }
    class operator Or(constref A, B: TBZVector4b): TBZVector4b; overload;
    { Opérateur logique OU EXCLUSIF (XOR) entre deux TBZVector4b }
    class operator Xor(constref A, B: TBZVector4b): TBZVector4b; overload;
    { Opérateur logique ET (AND) entre un TBZVector4b et une variable de type Byte }
    class operator And(constref A: TBZVector4b; constref B:Byte): TBZVector4b; overload;
    { Opérateur logique OU (OR) entre un TBZVector4b et une variable de type Byte }
    class operator or(constref A: TBZVector4b; constref B:Byte): TBZVector4b; overload;
    { Opérateur logique OU EXCLUSIF (XOR) entre un TBZVector4b et une variable de type Byte }
    class operator Xor(constref A: TBZVector4b; constref B:Byte): TBZVector4b; overload;
    
    { Division rapide par 2}
    function DivideBy2 : TBZVector4b;
    
    { Retourne le minimum de chaque composant entre le vecteur courrant et un autre TBZVector4b }
    function Min(Constref B : TBZVector4b):TBZVector4b; overload;
    { Retourne le minimum de chaque composant entre le vecteur courrant et un valeur de type Byte }
    function Min(Constref B : Byte):TBZVector4b; overload;
    { Retourne le maximum de chaque composant entre le vecteur courrant et un autre TBZVector4b }
    function Max(Constref B : TBZVector4b):TBZVector4b; overload;
    { Retourne le maximum de chaque composant entre le vecteur courrant et un valeur de type Byte }
    function Max(Constref B : Byte):TBZVector4b; overload;
    { S'assure que chaque composant du vecteur courrant soit compris en les valeur définit par les valeurs de type TBZVector4b "AMin" et "AMax" }
    function Clamp(Constref AMin, AMax : TBZVector4b):TBZVector4b; overload;
    { S'assure que chaque composant du vecteur courrant soit compris en les valeur de type Byte "AMin" et "AMax" }
    function Clamp(Constref AMin, AMax : Byte):TBZVector4b; overload;
    
    { Multiplie le vecteur courrant par un autre TBZVector2d et ajoute un deuxieme TBZVector4b au résultat }
    function MulAdd(Constref B, C : TBZVector4b):TBZVector4b;
    { Multiplie le vecteur courrant par un autre TBZVector2d et divise le resultat par un deuxieme TBZVector4b au résultat }
    function MulDiv(Constref B, C : TBZVector4b):TBZVector4b;
    
    { Mélange (Shuffle) les composants par rapport à l'odre des paramètres }
    function Shuffle(const x,y,z,w : Byte):TBZVector4b;
    { Mélange (Swizzle) les composants en fonction du masque de type TBZVector4SwizzleRef }
    function Swizzle(const ASwizzle: TBZVector4SwizzleRef ): TBZVector4b;

    { Retourne la combinaison = Self + (V2 * F2) }
    function Combine(constref V2: TBZVector4b; constref F1: Single): TBZVector4b;
    { Retourne la combinaison = (Self * F1) + (V2 * F2) }
    function Combine2(constref V2: TBZVector4b; const F1, F2: Single): TBZVector4b;
    { Retourne la combinaison = (Self * F1) + (V2 * F2) + (V3 * F3) }
    function Combine3(constref V2, V3: TBZVector4b; const F1, F2, F3: Single): TBZVector4b;

    { Retourne la valeurs minimum dans les composants XYZ }
    function MinXYZComponent : Byte;
    { Retourne la valeurs maximale dans les composants XYZ }
    function MaxXYZComponent : Byte;

    { Acces aux propriétés }
    Case Integer of
     0 : (V:TBZVector4bType);               //< Acces via tableau
     1 : (X, Y, Z, W:Byte);                 //< Acces par défaut
     2 : (Red, Green, Blue,  Alpha:Byte);   //< Acces comme une couleur au format RGBA
     3 : (AsVector3b : TBZVector3b);        //< Acces comme un TBZVector3b
     4 : (AsInteger : Integer);             //< Acces comme valeur 32 bit Integer
  end;

  { Définition d'un vecteur 4D de type Integer. }
  TBZVector4i = Record
  public
    { Initialisation des valeurs X, Y, Z et W }
    procedure Create(Const aX,aY,aZ: Longint; const aW : Longint = 0); overload;
    { Creation d'un vecteur TBZVector4i à partir d'un TBZVector3i et W }
    procedure Create(Const aValue : TBZVector3i; const aW : Longint = 0); overload;
    { Creation d'un vecteur TBZVector4i à partir d'un TBZVector3b et W }
    procedure Create(Const aValue : TBZVector3b; const aW : Longint = 0); overload;

    //procedure Create(Const aX,aY,aZ: Longint); overload; @TODO ADD as Affine creation
    //procedure Create(Const aValue : TBZVector3i); overload;
    //procedure Create(Const aValue : TBZVector3b); overload;

    { Retourne une chaine de caractères formaté représentant le vecteur : '(x, y, z, w)' }
    function ToString : String;

    { Ajoute deux TBZVector4i }
    class operator +(constref A, B: TBZVector4i): TBZVector4i; overload;
    { Soustrait deux TBZVector4i }
    class operator -(constref A, B: TBZVector4i): TBZVector4i; overload;
    { Multiplie deux TBZVector4i }
    class operator *(constref A, B: TBZVector4i): TBZVector4i; overload;
    { Divise deux TBZVector4i }
    class operator Div(constref A, B: TBZVector4i): TBZVector4i; overload;
    { Ajoute à chaque composant une variable de type Longint }
    class operator +(constref A: TBZVector4i; constref B:Longint): TBZVector4i; overload;
    { Soustrait à chaque composant une variable de type Longint }
    class operator -(constref A: TBZVector4i; constref B:Longint): TBZVector4i; overload;
    { Multiplie chaque composant par une variable de type Longint }
    class operator *(constref A: TBZVector4i; constref B:Longint): TBZVector4i; overload;
    { Multiplie chaque composant par une variable de type Single et arrondie les valeurs }
    class operator *(constref A: TBZVector4i; constref B:Single): TBZVector4i; overload;
    { Divise chaque composant par une variable de type Longint }
    class operator Div(constref A: TBZVector4i; constref B:Longint): TBZVector4i; overload;
    { Negation }
    class operator -(constref A: TBZVector4i): TBZVector4i; overload;

    { Retourne @True si le vecteur A est égale au vecteur B }
    class operator =(constref A, B: TBZVector4i): Boolean;
    { Retourne @True si deux TBZVector4f ne sont pas égale }
    class operator <>(constref A, B: TBZVector4i): Boolean;

    (* class operator And(constref A, B: TBZVector4i): TBZVector4i; overload;
    class operator Or(constref A, B: TBZVector4i): TBZVector4i; overload;
    class operator Xor(constref A, B: TBZVector4i): TBZVector4i; overload;
    class operator And(constref A: TBZVector4i; constref B:LongInt): TBZVector4i; overload;
    class operator or(constref A: TBZVector4i; constref B:LongInt): TBZVector4i; overload;
    class operator Xor(constref A: TBZVector4i; constref B:LongInt): TBZVector4i; overload; *)

    { Division rapide par 2 }
    function DivideBy2 : TBZVector4i;
    { Retourne les valeur absolue de chaque composant }
    function Abs: TBZVector4i;

    { Retourne la valeur minimum de chaque composant du vecteur par rapport à un autre TBZVector4i }
    function Min(Constref B : TBZVector4i):TBZVector4i; overload;
    { Retourne la valeur minimum de chaque composant du vecteur par rapport à une valeur de type Longint }
    function Min(Constref B : LongInt):TBZVector4i; overload;
    { Retourne la valeur maximum de chaque composant du vecteur par rapport à un autre TBZVector4i }
    function Max(Constref B : TBZVector4i):TBZVector4i; overload;
    { Retourne la valeur maximum de chaque composant du vecteur par rapport à une valeur de type Longint }
    function Max(Constref B : LongInt):TBZVector4i; overload;
    { S'assure que les valeurs de chaque composants se trouve dans l'interval des valeurs de type TBZVector4i AMin et AMax }
    function Clamp(Constref AMin, AMax : TBZVector4i):TBZVector4i; overload;
    { S'assure que les valeurs de chaque composants se trouve dans l'interval des valeurs de type Longint AMin et AMax }
    function Clamp(Constref AMin, AMax : LongInt):TBZVector4i; overload;

    { Multiplie le vecteur avec un autre TBZVector4i (B) et ajoute un deuxieme vecteur (C) }
    function MulAdd(Constref B, C : TBZVector4i):TBZVector4i;
    { Multiplie le vecteur avec un autre TBZVector4f (B) et soustrait un deuxieme vecteur (C) }
    //function MulSub(Constref B, C : TBZVector4i):TBZVector4i;
    { Multiplie le vecteur avec un autre TBZVector4i (B) et divise avec deuxieme vecteur (C) }
    function MulDiv(Constref B, C : TBZVector4i):TBZVector4i;

    { Mélange (Shuffle) les composants par rapport à l'odre des paramètres }
    function Shuffle(const x,y,z,w : Byte):TBZVector4i;
       
    { Mélange (Swizzle) les composants en fonction du masque de type TBZVector4SwizzleRef }
    function Swizzle(const ASwizzle: TBZVector4SwizzleRef ): TBZVector4i;

    { Retourne la combinaison = Self + (V2 * F2) }
    function Combine(constref V2: TBZVector4i; constref F1: Single): TBZVector4i;
    { Retourne la combinaison = (Self * F1) + (V2 * F2) }
    function Combine2(constref V2: TBZVector4i; const F1, F2: Single): TBZVector4i;
    { Retourne la combinaison = (Self * F1) + (V2 * F2) + (V3 * F3)  }
    function Combine3(constref V2, V3: TBZVector4i; const F1, F2, F3: Single): TBZVector4i;

    { Retourne la valeur minimum dans les composants XYZ }
    function MinXYZComponent : LongInt;
    { Retourne la valeur maximale dans les composants XYZ }
    function MaxXYZComponent : LongInt;

    { Acces aux propriétés }
    case Byte of
      0 : (V: TBZVector4iType);                 //< Acces par tableau
      1 : (X,Y,Z,W: longint);                   //< Acces par défaut
      2 : (Red, Green, Blue, Alpha : Longint);  //< Acces comme une couleur dans l'ordre RGBA
      3 : (AsVector3i : TBZVector3i);           //< Acces comme un TBZVector3i
      4 : (ST,UV : TBZVector2i);                //< Acces comme un coordonnées de Texture
      5 : (Left, Top, Right, Bottom: Longint);  //< Acces comme un rectangle
      6 : (TopLeft,BottomRight : TBZVector2i);  //< Acces comme les coins haut-gauche et bas-droit d'un rectangle
  end;

  { Définition d'un vecteur 4D de type Single. }
  TBZVector4f =  record  
  public  
    { Création d'un vecteur homogène à partir d'une variable de type Single. @br
      La valeur W est mise à 0.0 par défaut }
    procedure Create(Const AValue: single); overload;
    { Création d'un vecteur homogène à partir de valeurs de type Single. @br
      La valeur W est mise à 0.0 par défaut }
    procedure Create(Const aX,aY,aZ: single; const aW : Single = 0); overload;
    { Création d'un vecteur homogène à partir d'un TBZVector3f. @br
      La valeur W est mise à 0.0 par défaut }
    procedure Create(Const anAffineVector: TBZVector3f; const aW : Single = 0); overload;
    { Création d'un vecteur comme une Point  à partir d'une variable de type Single. @br
      La valeur W est mise à 1.0 par défaut }
    procedure CreatePoint(Const AValue: single); overload;
    { Création d'un vecteur comme une Point à partir de valeurs de type Single. @br
      La valeur W est mise à 1.0 par défaut }
    procedure CreatePoint(Const aX,aY,aZ: single); overload;
    { Création d'un vecteur comme une Point à partir d'un TBZVector3f. @br
      La valeur W est mise à 1.0 par défaut  }
    procedure CreatePoint(Const anAffineVector: TBZVector3f); overload;

    { Retourne une chaine de caractères formaté représentant le vecteur : '(x, y, z, w)' }
    function ToString : String;

    { Ajoute deux TBZVector4f }
    class operator +(constref A, B: TBZVector4f): TBZVector4f; overload;
    { Soustrait deux TBZVector4f }
    class operator -(constref A, B: TBZVector4f): TBZVector4f; overload;
    { Multiplie deux TBZVector4f }
    class operator *(constref A, B: TBZVector4f): TBZVector4f; overload;
    { Divise deux TBZVector4f }
    class operator /(constref A, B: TBZVector4f): TBZVector4f; overload;
    { Ajoute à chaque composant une variable de type Single }
    class operator +(constref A: TBZVector4f; constref B:Single): TBZVector4f; overload;
    { Soustrait à chaque composant une variable de type Single }
    class operator -(constref A: TBZVector4f; constref B:Single): TBZVector4f; overload;
    { Multiplie chaque composant par une variable de type Single }
    class operator *(constref A: TBZVector4f; constref B:Single): TBZVector4f; overload;
    { Divise chaque composant par une variable de type Single }
    class operator /(constref A: TBZVector4f; constref B:Single): TBZVector4f; overload;
    { Retourne la valeur négative }
    class operator -(constref A: TBZVector4f): TBZVector4f; overload;
    { Retourne @True si le vecteur A est égale au vecteur B }
    class operator =(constref A, B: TBZVector4f): Boolean;
    { Retourne @True si le vecteur A est plus grand ou égale que le vecteur B }
    class operator >=(constref A, B: TBZVector4f): Boolean;
    { Retourne @True si le vecteur A est plus petit ou égale que le vecteur B }
    class operator <=(constref A, B: TBZVector4f): Boolean;
    { Retourne @True si le vecteur A est plus grand que le vecteur B }
    class operator >(constref A, B: TBZVector4f): Boolean;
    { Retourne @True si le vecteur A est plus petit que le vecteur B }
    class operator <(constref A, B: TBZVector4f): Boolean;
    { Retourne @True si deux TBZVector4f ne sont pas égale }
    class operator <>(constref A, B: TBZVector4f): Boolean;

    class operator xor (constref A, B: TBZVector4f) : TBZVector4f;

    { Mélange (Shuffle) les composants par rapport à l'odre des paramètres }
    function Shuffle(const x,y,z,w : Byte): TBZVector4f;
    { Mélange (Swizzle) les composants en fonction du masque de type TBZVector4SwizzleRef }
    function Swizzle(const ASwizzle: TBZVector4SwizzleRef ): TBZVector4f;

    { Retourne la valeur minimum dans les composants XYZ }
    function MinXYZComponent : Single;
    { Retourne la valeur maximale dans les composants XYZ }
    function MaxXYZComponent : Single;

    { Retourne les valeurs absolue de chaque composant }
    function Abs:TBZVector4f;overload;
    { Negation }
    function Negate:TBZVector4f;
    { Division rapide par deux }
    function DivideBy2:TBZVector4f;

    { Retourne la longueur du vecteur }
    function Length:Single;
    { Retourne la longueur du vecteur au carré }
    function LengthSquare:Single;
    { Retourne la distance entre le vecteur et un autre TBZVector4f }
    function Distance(constref A: TBZVector4f):Single;
    { Retourne la distance au carré entre le vecteur et un autre TBZVector4f }
    function DistanceSquare(constref A: TBZVector4f):Single;
    { Calcules Abs(v1[x]-v2[x])+Abs(v1[y]-v2[y])+..., aussi connu sous le nom de  "Norm1". }
    function Spacing(constref A: TBZVector4f):Single;

    { Retourne le produit scalaire du vecteur avec un autre TBZVector4f }
    function DotProduct(constref A: TBZVector4f):Single;
    { Retourne le produit en croix du vecteur avec un autre TBZVector4f }
    function CrossProduct(constref A: TBZVector4f): TBZVector4f;
    { Normalise le vecteur }
    function Normalize: TBZVector4f;
    { Retourne la normal du vecteur }
    function Norm:Single;

    { Retourne la valeur minimum de chaque composant du vecteur par rapport à un autre TBZVector4f }
    function Min(constref B: TBZVector4f): TBZVector4f; overload;
    { Retourne la valeur minimum de chaque composant du vecteur par rapport à une valeur de type Single }
    function Min(constref B: Single): TBZVector4f; overload;
    { Retourne la valeur maximum de chaque composant du vecteur par rapport à un autre TBZVector4f }
    function Max(constref B: TBZVector4f): TBZVector4f; overload;
    { Retourne la valeur maximum de chaque composant du vecteur par rapport à une valeur de type Single }
    function Max(constref B: Single): TBZVector4f; overload;
    { S'assure que les valeurs de chaque composants se trouve dans l'interval des valeurs de type TBZVector4f AMin et AMax }
    function Clamp(Constref AMin, AMax: TBZVector4f): TBZVector4f; overload;
    { S'assure que les valeurs de chaque composants se trouve dans l'interval des valeurs de type Single AMin et AMax }
    function Clamp(constref AMin, AMax: Single): TBZVector4f; overload;

    { Multiplie le vecteur avec un autre TBZVector4f (B) et ajoute un deuxieme vecteur (C) }
    function MulAdd(Constref B, C: TBZVector4f): TBZVector4f;
    { Multiplie le vecteur avec un autre TBZVector4f (B) et soustrait un deuxieme vecteur (C) }
    function MulSub(Constref B, C: TBZVector4f): TBZVector4f;
    { Multiplie le vecteur avec un autre TBZVector4f (B) et divise avec deuxieme vecteur (C) }
    function MulDiv(Constref B, C: TBZVector4f): TBZVector4f;

    { Retourne la valeur interpolée linéarie à un moment T (compris entre 0 et 1.0) entre le vecteur et un autre vecteur }
    function Lerp(Constref B: TBZVector4f; Constref T:Single): TBZVector4f;
    { Retourne l'angle Cosine entre le vecteur et un autre TBZVector4f }
    function AngleCosine(constref A : TBZVector4f): Single;
    { Retourne l'angle entre le vecteur et un autre TBZVector4f, relatif à un centre de rotation TBZVector4f }
    function AngleBetween(Constref A, ACenterPoint : TBZVector4f): Single;

    { Retourne la combinaison = Self + (V2 * F2) }
    function Combine(constref V2: TBZVector4f; constref F1: Single): TBZVector4f;
    { Retourne la combinaison = (Self * F1) + (V2 * F2) }
    function Combine2(constref V2: TBZVector4f; const F1, F2: Single): TBZVector4f;
    { Retourne la combinaison = (Self * F1) + (V2 * F2) + (V3 * F3) }
    function Combine3(constref V2, V3: TBZVector4f; const F1, F2, F3: Single): TBZVector4f;

    { Retourne les valeurs arrondi dans un TBZVector4i }
    function Round: TBZVector4i;
    { Retourne les valeurs tronqué dans un TBZVector4i }
    function Trunc: TBZVector4i;
    { Retourne les valeurs arrondi vers l'infini négatif dans un TBZVector4i }
    function Floor: TBZVector4i; overload;
    { Retourne les valeurs arrondi vers l'infini positif dans un TBZVector4i }
    function Ceil : TBZVector4i; overload;
    { Retourne la partie fractionnaire de chaque composant }
    function Fract : TBZVector4f; overload;
    { Retourne la racine carré de chaque composant }
    function Sqrt : TBZVector4f; overload;
    { Retourne la racine carré inverse de chaque composant }
    function InvSqrt : TBZVector4f; overload;

    { Acces aux propriétés }
    case Byte of
      0: (V: TBZVector4fType);                 //< Acces par tableau
      1: (X, Y, Z, W: Single);                 //< Acces par défaut
      2: (Red, Green, Blue, Alpha: Single);    //< Acces comme une couleur dans l'ordre RGBA
      3: (AsVector3f : TBZVector3f);           //< Acces comme un TBZVector3f
      4: (ST, UV : TBZVector2f);               //< Acces comme un coordonnées de Texture
      5: (Left, Top, Right, Bottom: Single);   //< Acces comme un rectangle
      6: (TopLeft,BottomRight : TBZVector2f);  //< Acces comme les coins haut-gauche et bas-droit d'un rectangle
  end;

  { Définition de convenance pratique pour décrire un vecteur homogène de type TBZVector4f }
  TBZVector = TBZVector4f;
  { Pointeur vers TBZVector }
  PBZVector = ^TBZVector;

  TBZClipRect = TBZVector; //< TODO : Rendre Independant ou extraire TBZFloatRect de l'unité BZGraphic

{%endregion%}

{%region%----[ Plane ]---------------------------------------------------------}

Const
   {@groupbegin Constantes de representation de la "moitié du plan l'espace" (half space) }
   cPlaneFront   = 0;
   cPlaneBack    = 1;
   cPlanePlanar  = 2;
   cPlaneClipped = 3;
   cPlaneCulled  = 4;
   cPlaneVisible = 5;
   {@groupend}

type
  { Description du plan dans l'espace. @br
    La Position des point par rapport à l'axe normal d'un plan. @br
    La définition positive est relative à la direction normale du vecteur }
  TBZHmgPlaneHalfSpace = (
    phsIsOnNegativeSide,     //< est sur la face negative
    phsIsOnPositiveSide,     //< est sur la face positive
    phsIsOnPlane,            //< est sur le plan
    phsIsClipped,            //< est dans le plan
    phsIsAway                //< est en dehors du plan
  );

  { @abstract(Définition d'un plan homogène.)

    Une équation plane est défini par son équation A.x + B.y + C.z + D, un plan peut être mappé sur les
    Coordonnées d'espace homogènes, et c'est ce que nous faisons ici. @br
    Le plan est normalisé et contient donc une unité normale dans ABC (XYZ) et la distance de l'origine du plan.

    @bold(AVERTISSEMENT) : la méthode Create(Point, Normal) autorisera les vecteurs non-unitaires mais fondamentalement @bold(NE LE FAITES PAS). @br
    Un vecteur non-unitaire calculera la mauvaise distance au plan.

    C’est un moyen rapide de créer un plan quand on a un point et une Normale sans encore un autre appel à la méthode sqrt.

    Plus d'informations : @br
    @unorderedlist(
      @item(https://en.wikipedia.org/wiki/Plane_(geometry) )
      @item(http://www.songho.ca/math/plane/plane.html)
      @item(https://brilliant.org/wiki/3d-coordinate-geometry-equation-of-a-plane/ )
      @item(http://tutorial.math.lamar.edu/Classes/CalcII/EqnsOfPlanes.aspx )
    )
  }
  TBZHmgPlane = record
  private
    procedure CalcNormal(constref p1, p2, p3 : TBZVector);
  public
    { Création d'un plan homogène. @br
      Autorise les vecteurs non unitaires mais essentiellement @bold(NE PAS LE FAIRE). @Br
      Un vecteur non unitaire calculera la mauvaise distance au plan.}
    procedure Create(constref point, normal : TBZVector); overload;
    { Création et calcule des paramètres du plan définis par trois points. }
    procedure Create(constref p1, p2, p3 : TBZVector); overload;

    { Normalise le plan }
    procedure Normalize;
    { Retourne le plan normalisé }
    function Normalized : TBZHmgPlane;

    { Retourne la distance absolue entre le plan et un point }
    function AbsDistance(constref point : TBZVector) : Single;
    { Retourne la distance entre le plan et un point }
    function Distance(constref point : TBZVector) : Single; overload;
    { Retourne la distance entre le plan et une sphere de centre "Center" et de rayon "Radius" }
    function Distance(constref Center : TBZVector; constref Radius:Single) : Single; overload;
    { Calcul et retourne le vecteur perpendiculaire au plan. @br
      Le plan courrant est supposé être de longueur unitaire, pour pouvoir soustraire tout composant parallèle à lui même }
    function Perpendicular(constref P : TBZVector) : TBZVector;
    { Reflète le vecteur V par rapport le  plan (on suppose que le plan est normalisé) }
    function Reflect(constref V: TBZVector): TBZVector;

    { Retourne @True si un point est dans la moitié de l'espace }
    function IsInHalfSpace(constref point: TBZVector) : Boolean;

    { Return the position for a point relative to the normal axis of the plane.
      @param( : TBZVector)
      @return(TBZHmgPlaneHalfSpace) }
  	//function GetHalfSpacePosition(constref point: TBZVector) : TBZHmgPlaneHalfSpace;
	
	  { Computes point to plane projection. Plane and direction have to be normalized }
	   //function PointOrthoProjection(const point: TAffineVector; const plane : THmgPlane; var inter : TAffineVector; bothface : Boolean = True) : Boolean;
	   //function PointProjection(const point, direction : TAffineVector; const plane : THmgPlane; var inter : TAffineVector; bothface : Boolean = True) : Boolean;

    { Computes segment / plane intersection return false if there isn't an intersection}
    // function SegmentIntersect(const ptA, ptB : TAffineVector; const plane : THmgPlane; var inter : TAffineVector) : Boolean;

  	{ Access aux propriétés }
    case Byte of
       0: (V: TBZVector4fType);          //< Doit être compatible avec le type TBZVector4f
       1: (A, B, C, D: Single);          //< Paramètres du plan
       2: (AsNormal3: TBZAffineVector);  //< Accès super rapide à la Normale en tant que vecteur affine.
       3: (AsVector: TBZVector);         //< Accès comme un TBZVector4f
       4: (X, Y, Z, W: Single);          //< Acces légal pour que le code existant fonctionne
  end;

{%endregion%}

{%region%----[ Matrix ]--------------------------------------------------------}

  { Type d'action de transformation d'une matrice }
  TBZMatrixTransType = (ttScaleX, ttScaleY, ttScaleZ,
                        ttShearXY, ttShearXZ, ttShearYZ,
                        ttRotateX, ttRotateY, ttRotateZ,
                        ttTranslateX, ttTranslateY, ttTranslateZ,
                        ttPerspectiveX, ttPerspectiveY, ttPerspectiveZ, ttPerspectiveW);

  { TBZMatrixTransformations : Est utilisé pour décrire une séquence de transformation en suivant l'odre définis tel que :
    [Sx][Sy][Sz][ShearXY][ShearXZ][ShearZY][Rx][Ry][Rz][Tx][Ty][Tz][P(x,y,z,w)]
    Constants are declared for easier access (see MatrixDecompose below) }
  TBZMatrixTransformations  = array [TBZMatrixTransType] of Single;

{%region%----[ TBZMatrix4f ]---------------------------------------------------}

  { @abstract(Définition d'un matrice 4D de type Single @br
    Les éléments de la matrice sont dans l'ordre des lignes principales (Row-major) )

    Plus d'informations :@br
    @unorderedlist(
      @item(http://www.euclideanspace.com/maths/algebra/matrix/index.htm)
      @item(http://www.euclideanspace.com/maths/differential/other/matrixcalculus/index.htm)
      @item(http://www.fastgraph.com/makegames/3drotation/)
    ) }
  TBZMatrix4f = record
  private
    function GetComponent(const ARow, AColumn: Integer): Single; inline;
    procedure SetComponent(const ARow, AColumn: Integer; const Value: Single); inline;
    function GetRow(const AIndex: Integer): TBZVector; inline;
    procedure SetRow(const AIndex: Integer; const Value: TBZVector); inline;

    function GetDeterminant: Single;

    function MatrixDetInternal(const a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single;
    procedure Transpose_Scale_M33(constref src : TBZMatrix4f; Constref ascale : Single);
  public
  
    { Création d'une matrice homogène identitaire }
    procedure CreateIdentityMatrix;
    { Création d'une matrice d'hommotéthie depuis un TBZAffineVector }
    procedure CreateScaleMatrix(const v : TBZAffineVector); overload;
    { Création d'une matrice d'hommotéthie depuis un TBZVector }
    procedure CreateScaleMatrix(const v : TBZVector); overload;
    { Creation d'une matrice de translation depuis un TBZAffineVector }
    procedure CreateTranslationMatrix(const V : TBZAffineVector); overload;
    { Creation d'une matrice de translation depuis un TBZVector }
    procedure CreateTranslationMatrix(const V : TBZVector); overload;
    { Creation d'une matrice d'hommotéthie + de translation. @br
      L'hommotéthie est appliqué avant la translation. }
    procedure CreateScaleAndTranslationMatrix(const aScale, Offset : TBZVector); overload;
    { Création d'une matrice de rotation de type "Main droite"  autours de l'axe des X depuis Sin et Cos }
    procedure CreateRotationMatrixX(const sine, cosine: Single); overload;
    { Création d'une matrice de rotation de type "Main droite"  autours de l'axe des X depuis un Angle (en radian) }
    procedure CreateRotationMatrixX(const angle: Single); overload;
    { Création d'une matrice de rotation de type "Main droite"  autours de l'axe des Y depuis Sin et Cos }
    procedure CreateRotationMatrixY(const sine, cosine: Single); overload;
    { Création d'une matrice de rotation de type "Main droite"  autours de l'axe des X depuis un Angle (en radian) }
    procedure CreateRotationMatrixY(const angle: Single); overload;
    { Création d'une matrice de rotation de type "Main droite"  autours de l'axe des Z depuis Sin et Cos }
    procedure CreateRotationMatrixZ(const sine, cosine: Single); overload;
    { Création d'une matrice de rotation de type "Main droite"  autours de l'axe des Z depuis un Angle (en radian) }
    procedure CreateRotationMatrixZ(const angle: Single); overload;
    { Creatin d'une matrice de rotation le long de l'axe donné de type TBZAffineVector "anAxis" par Angle (en radians). }
    procedure CreateRotationMatrix(const anAxis : TBZAffineVector; angle : Single); overload;
    { Creatin d'une matrice de rotation le long de l'axe donné de type TBZVector "anAxis" par Angle (en radians). }
    procedure CreateRotationMatrix(const anAxis : TBZVector; angle : Single); overload;
    { Creatin d'une matrice de rotation depuis les angles d'euler Yaw, Pitch et Roll }
    procedure CreateRotationMatrixYawPitchRoll(yawAngle, pitchAngle, rollAngle : Single);
    { Creation d'un matrice "regarder (Look at) de type "Main droite") }
    procedure CreateLookAtMatrix(const eye, center, normUp: TBZVector);
    { Creation d'une matrice de "vue" depuis les  plan d'un "Frustum"(Left, Right, Bottom, Top, ZNear, ZFar) }
    procedure CreateMatrixFromFrustumPlane(Left, Right, Bottom, Top, ZNear, ZFar: Single);

    // Par defaut le plan utiliser est le plan defini par les point LeftTop et BottomRight avec les valeur en z les plus grandes
    // procedure CreateMatrixFromFrustum(vFrustum : TBZFrustum);

    { Creation d'une matrice de "Perspective" }
    procedure CreatePerspectiveMatrix(FOV, Aspect, ZNear, ZFar: Single);
    { Creation d'une matrice de vue othogonale à partir des paramètre du Frustum }
    procedure CreateOrthoMatrix(Left, Right, Bottom, Top, ZNear, ZFar: Single);
    { Creation d'une matrice de sélection }
    procedure CreatePickMatrix(x, y, deltax, deltay: Single; const viewport: TBZVector4i);
    { Creation d'une matrice de projection parallele. @br
      Les points transformés  seront projetés sur le plan le long de la direction spécifiée. }
    procedure CreateParallelProjectionMatrix(const plane : TBZHmgPlane; const dir : TBZVector);
    { Creation d'une matrice de projection "Ombre" (shadow). @br
      L'ombre sera projetésur le plan définit par "planePoint"  et "planeNormal", depuis la position de la lumière "lightPos".}
    procedure CreateShadowMatrix(const planePoint, planeNormal, lightPos : TBZVector);
    { Creation d'une matrice de reflection pour le plan donné. @br
      La matrice de réflexion permet  la mise en œuvre de planes réflecteurs (miroirs) }
    procedure CreateReflectionMatrix(const planePoint, planeNormal : TBZVector);

    { Retourne une chaine de caractères formaté représentant la matrice : @br
  	  "("x, y, z, w")" @br
	    "("x, y, z, w")" @br
	    "("x, y, z, w")" @br
	    "("x, y, z, w")" }
    function ToString : String;
	
    { Ajoute deux matrices }
    class operator +(constref A, B: TBZMatrix4f): TBZMatrix4f; overload;
    { Ajoute à chaque composant de la matrice une variable de type Single }
    class operator +(constref A: TBZMatrix4f; constref B: Single): TBZMatrix4f; overload;
    { Soustrait deux matrices }
    class operator -(constref A, B: TBZMatrix4f): TBZMatrix4f; overload;
    { Soustrait à chaque composant de la matrice une variable de type Single }
    class operator -(constref A: TBZMatrix4f; constref B: Single): TBZMatrix4f; overload;
    { Multiplie deux matrices }
    class operator *(constref A, B: TBZMatrix4f): TBZMatrix4f; overload;
    { Multiplie chaque composant de la matrice par une variable de type Single }
    class operator *(constref A: TBZMatrix4f; constref B: Single): TBZMatrix4f; overload;
    { Transforme un vecteur homogene en le multipliant par la matrice }
    class operator *(constref A: TBZMatrix4f; constref B: TBZVector): TBZVector; overload;
    { Transforme un vecteur homogene en le multipliant par la matrice }
    class operator *(constref A: TBZVector; constref B: TBZMatrix4f): TBZVector; overload;
    { Divise chaque composant de la matrice par une variable de type Single }
    class operator /(constref A: TBZMatrix4f; constref B: Single): TBZMatrix4f; overload;
    { Negation de chaque composant de la matrice }
    class operator -(constref A: TBZMatrix4f): TBZMatrix4f; overload;

    //class operator =(constref A, B: TBZMatrix4): Boolean;overload;
    //class operator <>(constref A, B: TBZMatrix4): Boolean;overload;

    { Transpose la matrice }
    function Transpose: TBZMatrix4f;
    { Retourne la matrice inverse }
    function Invert : TBZMatrix4f;
    { Normalise la matrice et supprime les composant de translation. @br
      Le résultat est une matrice orthonormal (la direction Y direction est preservé, puis Z) }
    function Normalize : TBZMatrix4f;

    { Adjugate and Determinant functionality.@br
      Is used in the computation of the inverse of a matrix@br
      So far has only been used in Invert and eliminated from  assembler algo might
      not need to do this just keep it for pascal do what it says on the tin anyway as it has combined. }

    { Fonctionnalité adjuvante et déterminante. @Br
      Est utilisé dans le calcul de la matrice inverse. @br
      Jusqu'à présent, a seulement été utilisé dans "Invert" }
    procedure Adjoint;
    { Trouve la matrice inverse enpréservant l'angle .@br
      La préservation de l'angle peut permettre le calcul de la matrice inverse des matrices combiner de la translation,
      de la rotation, de la mise à l'échelle isotrope. @br
      Les autres matrices ne seront pas correctement inversées par cette fonction. }
    procedure AnglePreservingMatrixInvert(constref mat : TBZMatrix4f);
    { Décompose une matrice de transformation non dégénérée en la séquence de transformations qui l'a produite. @Br      
      Auteur original : Spencer W. Thomas, Université du Michigan. @br
      Le coefficient de chaque transformation est retourné dans l'élément correspondant du vecteur Translation. @br
      Retourne @true en cas de success, si non @false la matrice est singulière. }
    function Decompose(var Tran: TBZMatrixTransformations): Boolean;

    { Ajoute un vecteur de translation à la matrice }
    function Translate( constref v : TBZVector):TBZMatrix4f;
    { Multiplication par composants (Component-wise multiplication) }
    function Multiply(constref M2: TBZMatrix4f):TBZMatrix4f;  

    // function getCoFactor : single;

    { Acces aux lignes de la matrice par vecteur }
    property Rows[const AIndex: Integer]: TBZVector read GetRow write SetRow;
  	{ Acces aux composants de la matrice }
    property Components[const ARow, AColumn: Integer]: Single read GetComponent write SetComponent; default;	
  	{ Retourne le déterminant de la matrice }
    property Determinant: Single read GetDeterminant;

    { Access aux propriétés }
    case Byte of
    
      0: (M: array [0..3, 0..3] of Single);           //< Acces par tableau 2D
      1: (V: array [0..3] of TBZVector);              //< Acces via tableau de TBZVector
      2: (VX : Array[0..1] of array[0..7] of Single); //< acces pour l'AVX
      3: (X,Y,Z,W : TBZVector);                       //< Acces aux lignes
      4: (m11, m12, m13, m14: Single;                 //< Acces par défaut
          m21, m22, m23, m24: Single;
          m31, m32, m33, m34: Single;
          m41, m42, m43, m44: Single);
  End;

  { Définition de convenance pratique pour décrire une matrice de type TBZMatrix4f }
  TBZMatrix = TBZMatrix4f;
  { Pointeur vers TBZMatrix }
  PBZMatrix = ^TBZMatrix;

{%endregion%}
{%endregion%}

{%region%----[ TBZEulerAngles ]------------------------------------------------}

  { Ordre de rotation des angles d'Euler. @br
    Ici, nous utilisons les conventions d'angles de Tait-Bryan pour la définition des axes de rotation. @br
    (x-y-z, y-z-x, z-x-y, x-z-y, z-y-x, y-x-z). }
  TBZEulerOrder = (eulXYZ, eulXZY, eulYXZ, eulYZX, eulZXY, eulZYX);

  { @abstract(Descrition d'angles d'Euler)

    Les angles d'Euler sont trois angles pour décrire l'orientation d'un corps rigide par rapport à un repère fixe. @br
    Ils peuvent également représenter l'orientation d'un référentiel mobile en physique.
    Ou l'orientation d'une base générale en algèbre linéaire tridimensionnelle.
    Est utilisé par le type TBZQuaternion.

    @bold(A RETENIR) :
    @unorderedlist(
      @item(X = Roll  = Bank     = Tilt)
      @item(Y = Yaw   = Heading  = Azimuth)
      @item(Z = Pitch = Attitude = Elevation)
    )

    Source : https://en.wikipedia.org/wiki/Euler_angles

    Plus d'informations :@br
    @unorderedlist(
      @item(https://en.wikipedia.org/wiki/Euler%27s_rotation_theorem)
      @item(http://ressources.univ-lemans.fr/AccesLibre/UM/Pedago/physique/02/meca/angleeuler.html)
      @item(https://en.wikipedia.org/wiki/Orientation_(geometry))
      @item(https://en.wikipedia.org/wiki/Inertial_navigation_system)
      @item(http://www.euclideanspace.com/maths/geometry/rotations/euler/index.htm)
      @item(http://www.starlino.com/imu_guide.html)
    ) }
  TBZEulerAngles = record
  public

    { Creation des angles d'Euler depuis les angles XYZ (en degré) }
    procedure Create(X,Y,Z:Single); // Roll, Yaw, Pitch
    { CCreation des angles d'Euler depuis les angles définie dans un TBZVector (en degré) }
    procedure Create(Const Angles: TBZVector);

    { Convertis les angles d'Euler en angle et en un axe selon l'ordre de rotation. @br
      Ordre des angles de rotation par défaut YZX (TBZEulerOrder) }
    procedure ConvertToAngleAxis(Out Angle : Single; Out Axis : TBZVector;Const EulerOrder : TBZEulerOrder = eulYZX);
    { Convertis les angles d'Euler vers une matrice de rotation suivant l'ordre de rotation. @br
      Ordre des angles de rotation par défaut YZX (TBZEulerOrder) }
    function ConvertToRotationMatrix(Const  EulerOrder:TBZEulerOrder = eulYZX): TBZMatrix;

    { Acces aux propriétés }
    Case Byte of
      0 : ( V : TBZVector3fType );                //< Accès via tableau
      1 : ( X, Y, Z : Single );                   //< Acces par défaut
      2 : ( Roll, Yaw, Pitch : Single );          //< Acces par angle
      3 : ( Bank, Heading, Attitude : Single );   //< Acces par angle "avionique"
      4 : ( Tilt, Azimuth, Elevation : Single );  //< Acces par angle "topographie"
  end;

{%endregion%}

{%region%----[ Quaternion ]----------------------------------------------------}

  { @abstract(Description et manipulation de quaternions)

    Les quaternions sont un système numérique qui étend les nombres complexes
    et sont appliqués à la mécanique dans l'espace tridimensionnel. @br
    Une des caractéristiques des quaternions est que la multiplication de deux quaternions n'est pas commutative.

    Plus informations :@br
    @unorderedlist(
       @item(https://en.wikipedia.org/wiki/Quaternion)
       @item(http://developerblog.myo.com/quaternions/)
       @item(http://www.chrobotics.com/library/understanding-quaternions)
       @item(https://en.wikipedia.org/wiki/Quaternions_and_spatial_rotation)
       @item(http://www.euclideanspace.com/maths/algebra/realNormedAlgebra/quaternions/)
       @item(http://mathworld.wolfram.com/Quaternion.html)
     ) }
  TBZQuaternion = record
  public  
    { Creation d'un quaternion }
    procedure Create(x,y,z: Single; Real : Single);overload;
    { Creation d'un quaternion }
    procedure Create(const Imag: array of Single; Real : Single); overload;
    { Création d'un quaternion unitaire depuis deux vecteurs de type TBZAffineVector }
    procedure Create(const V1, V2: TBZAffineVector); overload;
    { Creation d'un quaternion unitaire à partir de deux vecteurs unitaires ou de deux points ou sur une sphère unitaire }
    procedure Create(const V1, V2: TBZVector); overload;
    { Creation d'un quaternion unitaire à partir d'une matrice. @br
	    @bold(Note) : La matrice doit-être une matrice de rotation ! }
    procedure Create(const mat : TBZMatrix); overload;
    { Création d'un quaternion depuis un angle (en degré) et un axe de type TBZAffineVector }
    procedure Create(const angle  : Single; const axis : TBZAffineVector); overload;
    //procedure Create(const angle  : Single; const axis : TBZVector); overload;

    { Création d'un quaternion depuis les angles d'Euler. ( Ordre standard des angles d'Euler = YZX ) }
    procedure Create(const Yaw, Pitch, Roll : Single); overload;
    { Création d'un quaternion depuis les angles d'Euler dans un ordre arbitraire TBZEulerOrder (angles en degré) }
    procedure Create(const x, y, z: Single; eulerOrder : TBZEulerOrder); overload;
	
    { Création d'un quaternion depuis les angles d'Euler TBZEulerAngles dans un ordre arbitraire TBZEulerOrder (angles en degré) }
    procedure Create(const EulerAngles : TBZEulerAngles; eulerOrder : TBZEulerOrder); overload;
    
    { Retourne le produite de deux quaternion. @br
      "quatrenion_Left * quaternion_Right"  est la concaténation d'une rotation de A suivie de la rotation B @br
      @bold(Note) :  L'ordre inversé est important  A * B car B * A  donne de mauvais résultats ! }
    class operator *(constref A, B: TBZQuaternion): TBZQuaternion;
    { Retourne @True si le quaternion A est égale au quaternion B  }
    class operator =(constref A, B: TBZQuaternion): Boolean;
    { Retourne @True si le quaternion A n'est pas égale au quaternion B  }
    class operator <>(constref A, B: TBZQuaternion): Boolean;
    
    { Retourne une chaine de caractères formaté représentant le quaternion : "("x, y, z, w")"  }
    function ToString : String;


    (*PD : CONVERTS A UNIT QUATERNION INTO TWO POINTS ON A UNIT SPHERE
     PD THIS IS A NONSENSE FUNCTION. IT DOES NOT DO THIS. IT MAKES ASSUMTIONS
     THERE IS NO Z COMPONENT IN THE CALCS. IT TRIES TO USE IMAGINARY PART
     AS A VECTOR WHICH YOU CANNOT DO WITH A QUAT, IT IS A 4D OBJECT WHICH MUST
     USE OTHER METHODS TO TRANSFORM 3D OBJECTS. IT HOLDS NO POSITION INFO.

     CONVERTIR UN QUATERNION UNITAIRE EN DEUX POINTS SUR UNE SPHÈRE UNITAIRE N'A PAS DE SENS ET ON NE DOIT PAS LE FAIRE.
     IL N'Y A PAS DE COMPOSANT Z DANS LES CALCULS ET ON NE PEUX PAS TENTER D'UTILISER UNE PARTIE IMAGINAIRE.
     EN TANT QUE VECTEUR ON NE PEUX PAS FAIRE AVEC UN QUATERNION.
     C'EST UN OBJET 4D QUI DOIT UTILISER D'AUTRES MÉTHODES POUR TRANSFORMER DES OBJETS 3D.
     IL NE CONTIENT AUCUNE INFORMATION DE POSITION.
    
     THIS FUNCTION IS USE FOR "ARCBALL" GIZMO. CAN BE REMOVE FROM HERE.
     IT CAN TAKE PLACE IN THE "GIZMOARCBALL OBJECT"
    PROCEDURE CONVERTTOPOINTS(VAR ARCFROM, ARCTO: TBZAFFINEVECTOR); //OVERLOAD;
    PROCEDURE CONVERTTOPOINTS(VAR ARCFROM, ARCTO: TBZVECTOR); //OVERLOAD; *)

    { Convertis le quaternion en une matrice de rotation (possibilité au quaternion d'être non-unitaire). @br
      On suppose que la matrice sera utilisé pour multiplié la colonne du vecteur sur la gauche : @br
      vnew = mat * vold. @br
      Fonctionne correctement pour les coordonées "Main droite".}
    function ConvertToMatrix : TBZMatrix;
    { Convertis le quaternion vers l'ordre de rotation  définis par TBZEulerOrder }
    function ConvertToEuler(Const EulerOrder : TBZEulerOrder) : TBZEulerAngles;
    { Convertis le quaternion vers un angle (en degré) et un axe de type TBZAffineVector }
    procedure ConvertToAngleAxis(out angle  : Single; out axis : TBZAffineVector);
    { Retourne le quaternion "conjuguer" (conjugate) }
    function Conjugate : TBZQuaternion;
    { Retourne la magnitude du quaternion }
    function Magnitude : Single;
    { Normalise le quaternion }
    procedure Normalize;
    { Applique la rotation au vecteur V de type TBZVector autours l'axe local. }
    function Transform(constref V: TBZVector): TBZVector;

    { Redimensionne le quaternion. @br
      Si le facteur de mise à l'échelle Un facteur d'échelle est appliqué à un quaternion puis la rotation mettra à l'échelle les vecteurs lors de leur transformation. @br
	    On suppose que le quaternion est déja normalisé. }
    procedure Scale(ScaleVal: single);

    { Retourne le produit de deux quaternion : quaternion_Right * quaternion_Left. @br
      Pour combiner les rotations, utilisez Muliply(qSecond, qFirst), ce qui aura pour effet de faire une rotation par qFirst puis qSecond. @br
	    @bold(Note) : l'ordre de la multiplication est importante ! }
    function MultiplyAsSecond(const qFirst : TBZQuaternion): TBZQuaternion;
          
    { Interpolation linéaire sphérique de quaternions unitaires avec des tours (spins). @br
      @bold(Note) : @br
       - la valeur de début QStart correspond au quaternion courrant Self. @br
       - t est une valeur comprise entre 0.0 et 1.0 @br
       - Spin est le nombre de rotation supplémentaire à effectuer. }
    function Slerp(const QEnd: TBZQuaternion; Spin: Integer; t: Single): TBZQuaternion; overload;

    { Interpolation linéaire sphérique de quaternions unitaires. @br
      @bold(Note) : @br
       - la valeur de début QStart correspond au quaternion courrant Self. @br
       - t est une valeur comprise entre 0.0 et 1.0 @br }
    function Slerp(const QEnd: TBZQuaternion; const t : Single) : TBZQuaternion; overload;

    { Acces aux propriétés }
    case Byte of
      0: (V: TBZVector4fType);                           //< Acces via tableau
      1: (X, Y, Z, W: Single);                           //< Acces par défaut
      2: (AsVector4f : TBZVector4f);                     //< Acces comme un TBZVector
      3: (ImagePart : TBZVector3f; RealPart : Single);   //< Acces comme un nombre complexe
  End;
  
{%endregion%}

{%region%----[ Helpers ]-------------------------------------------------------}

{%region%----[ TBZVector2iHelper ]---------------------------------------------}

  {Assistant pour le type TBZVector2i }
  TBZVector2iHelper = record helper for TBZVector2i
  private
    function GetYX : TBZVector2i;
  public
   { Retourne le vecteur courrant normalisé }
   function Normalize : TBZVector2f;
   { Acces comme un TBZVector2f }
   function AsVector2f : TBZVector2f;

   { Retourne les valeurs dans l'ordre YX }
   property YX : TBZVector2i read GetYX;
  end;

{%endregion%}

{%region%----[ TBZVector2fHelper ]---------------------------------------------}

  { Assistant pour le type TBZVector2f }
  TBZVector2fHelper = record helper for TBZVector2f
  private
    // Swizling access
    function GetXY : TBZVector2f;
    function GetYX : TBZVector2f;
    function GetXX : TBZVector2f;
    function GetYY : TBZVector2f;

    function GetXXY : TBZVector4f;
    function GetYYX : TBZVector4f;

    function GetXYY : TBZVector4f;
    function GetYXX : TBZVector4f;

    function GetXYX : TBZVector4f;
    function GetYXY : TBZVector4f;

    function GetXXX : TBZVector4f;
    function GetYYY : TBZVector4f;

  public

    { @abstract(Implémente une fonction d'étape renvoyant une pour chaque composant de Self qui est
      supérieur ou égal au composant correspondant dans la référence du Vecteur B, et zéro si non)
	  
      Plus d'informations : @br
      @unorderedlist(
	      @item(http://developer.download.nvidia.com/cg/step.html)
	      @item(https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/step.xhtml )) }
    function Step(ConstRef B : TBZVector2f):TBZVector2f;

    { @abstract(Retourne une normale telle quelle si le vecteur de position dans l'espace occulaire d'un sommet pointe dans la direction opposée d'une normale géométrique,
      sinon renvoie la version inversée de la normale.)

      Le vecteur courrant représente le vecteur de la normale pertubée. @br
      Le vecteur A représente un vecteur d'incidence (généralement un vecteur de direction de l'œil vers un sommet) @br
      Le vecteur B représente le vecteur normal géométrique (pour certaines facettes, la normale péturbée est identique).

      Plus d'informations : @br
      @unorderedlist(
	      @item(http://developer.download.nvidia.com/cg/faceforward.html)
	      @item(https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/faceforward.xhtml )) }
    //function FaceForward(constref A, B: TBZVector2f): TBZVector2f;

    { @abstract(Renvoie le plus petit entier non inférieur à un scalaire ou à chaque composante vectorielle.)

      Retourne la valeur de la saturation dans la plage [0,1] comme suit: @br
      @orderedlist(
        @item(Retourne 0 si le vecteur courrant est plus petit que 0)
        @item(Retourne 1 si le vecteur courrant est plus grand que 1)
        @item(Dans les autres cas retourne les valeurs courrantes.))

      Plus d'informations : http://developer.download.nvidia.com/cg/saturate.html ) }
    function Saturate : TBZVector2f;

    { @abstract(Interpolation douce entre deux valeurs basé sur le troisième paramètre.)

	  @unorderedlist(
  	  @item(Retourne 0 si Self < A < B ou que Self > A > A)
		  @item(Retourne 1 si Self < B < A ou que Self > B > A)
		  @item(La valeur interpolée retournée est comprise dans l'intervalle 0.0 et 1.0 pour le domaina [A,B].))

      Si A = Self alors, la pente de Self.smoothstep(a, b) et b.smoothstep(a, b) est nulle.@br

      Plus d'information : @br
      @unorderedlist(
        @item(http://developer.download.nvidia.com/cg/smoothstep.html)
	      @item(https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/smoothstep.xhtml )) }
    function SmoothStep(ConstRef A,B : TBZVector2f): TBZVector2f;

    { @abstract(Retourne la valeur de l'interpolation linéaire du vecteur courrant et de B en fonction du poid T. @br
       Si T est une valeur en dehors de l'interval [0.0, 1.0], il sera actuellement extrapolé. )
			  
	  Plus d'informations : @br
    @unorderedlist(
      @item(http://developer.download.nvidia.com/cg/lerp.html)
      @item(https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/mix.xhtml) }
    function Lerp(Constref B: TBZVector2f; Constref T:Single): TBZVector2f;

    { Accès rapide aux propriétés de "Swizzling" comme dans HLSL et GLSL }
    {@groupbegin}
    property XY : TBZVector2f read GetXY;
    property YX : TBZVector2f read GetYX;
    property XX : TBZVector2f read GetXX;
    property YY : TBZVector2f read GetYY;

    property XXY : TBZVector4f read GetXXY;
    property YYX : TBZVector4f read GetYYX;

    property XYY : TBZVector4f read GetXYY;
    property YXX : TBZVector4f read GetYXX;

    property XYX : TBZVector4f read GetXYX;
    property YXY : TBZVector4f read GetYXY;

    property XXX : TBZVector4f read GetXXX;
    property YYY : TBZVector4f read GetYYY;
    {@groupend}

  end;

{%endregion%}

{%region%----[ TBZVector4fHelper ]---------------------------------------------}

  { Assistant pour le type TBZVector eg : TBZVector4f }
  TBZVectorHelper = record helper for TBZVector
  private
    // Swizling access
    function GetXY : TBZVector2f;
    procedure SetXY(Const Value: TBZVector2f);

    function GetYX : TBZVector2f;
    function GetXZ : TBZVector2f;
    function GetZX : TBZVector2f;
    function GetYZ : TBZVector2f;
    function GetZY : TBZVector2f;
    function GetXX : TBZVector2f;
    function GetYY : TBZVector2f;
    function GetZZ : TBZVector2f;

    function GetXYZ : TBZVector4f;
    function GetXZY : TBZVector4f;

    function GetYXZ : TBZVector4f;
    function GetYZX : TBZVector4f;

    function GetZXY : TBZVector4f;
    function GetZYX : TBZVector4f;

    function GetXXX : TBZVector4f;
    function GetYYY : TBZVector4f;
    function GetZZZ : TBZVector4f;

    function GetYYX : TBZVector4f;
    function GetXYY : TBZVector4f;
    function GetYXY : TBZVector4f;

  public
    { @abstract(Effectue une rotation du point selon un axe arbitraire et d'un angle (en radians))

      Le système de coordonnées utilisé ets "Main droite".@br
      Les rotations positives sont dans le sens inverse des aiguilles d'une montre avec l'axe positif vers vous. @br
      Les rotations positives sont vues dans le sens horaire depuis l'origine le long de l'axe positif : @br
      @unorderedlist(
        @item(Orientation de l'axe pour visualiser les positifs dans le quadrant supérieur droit [sous forme d'axes de graphique] )
        @item(Avec + Z pointé vers vous (vers l'écran), +X est à droite, +Y est vers le haut)
        @item(Avec + Y pointant vers vous, +Z est à droite, +X est vers le haut)
        @item(Avec + X pointant vers vous +Y est à gauche +Z est vers le haut)) }
    function Rotate(constref axis : TBZVector; angle : Single):TBZVector;

    { Effectue une rotation matricielle du point autours l'axe des X et d'un angle "Alpha" (en radians) }
    function RotateWithMatrixAroundX(alpha : Single) : TBZVector;

    { Effectue une rotation matricielle du point autours l'axe des Y et d'un angle "Alpha" (en radians) }
    function RotateWithMatrixAroundY(alpha : Single) : TBZVector;

    { Effectue une rotation matricielle du point autours l'axe des Z et d'un angle "Alpha" (en radians) }
    function RotateWithMatrixAroundZ(alpha : Single) : TBZVector;

    { @abstract(Effectue une rotation du point autours l'axe des X et d'un angle "Alpha" (en radians))

      Avec +X pointant vers vous, +Y est à gauche, +Z est vers le haut. @br
      Une rotation positive autour de x, Y devient négatif.	}
    function RotateAroundX(alpha : Single) : TBZVector;

    { @abstract(Effectue une rotation du point autours l'axe des Y et d'un angle "Alpha" (en radians))

      Avec +Y pointant vers vous, +Z est à droite, +X est vers le haut. @br
      Une rotation positive autour de y, Z devient négatif.	}
    function RotateAroundY(alpha : Single) : TBZVector;

    { @abstract(Effectue une rotation du point autours l'axe des Y et d'un angle "Alpha" (en radians))

      Avec +Z pointant vers vous, +X est à droite, +Y est vers le haut. @br
      Une rotation positive autour de z, X devient négatif.	}
    function RotateAroundZ(alpha : Single) : TBZVector;

    { @abstract(Projection du point sur la ligne définie par "Origin" et "Direction".)

    Effectue un produit scalaire : DotProduct((p - origine), direction) qui, si la direction est normalisée,
    calcule la distance entre l'origine et la projection deu point sur la ligne (origine, direction). }
    function PointProject(constref origin, direction : TBZVector4f) : Single;


    // Returns true if line intersect ABCD quad. Quad have to be flat and convex
    //function IsLineIntersectQuad(const direction, ptA, ptB, ptC, ptD : TBZVector) : Boolean;
    // Computes closest point on a segment (a segment is a limited line).
    //function PointSegmentClosestPoint(segmentStart, segmentStop : TBZVector) : TBZVector;
    { Computes algebraic distance between segment and line (a segment is a limited line).}
    //function PointSegmentDistance(const point, segmentStart, segmentStop : TAffineVector) : single;
    { Computes closest point on a line.}
    //function PointLineClosestPoint(const linePoint, lineDirection : TBZVector) : TBZVector;
    { Computes algebraic distance between point and line.}
    //function PointLineDistance(const linePoint, lineDirection : TBZVector) : Single;
  
    { @abstract(Modifie la position du vecteur (caméra) pour le déplacer autour de sa cible. @br
      Cette méthode permet de mettre en œuvre rapidement les commandes d'une caméra. @br
      @bold(Note) : Angle deltas est en degré.)
		 
      @bold(Astuce) :@br
      Les coordonnées du parent vecteur (caméra) doivent être une "identité". @br
      Faire du vecteur de la camera  un enfant d'un objet, et en déplacant simplement la cible,
      permet de changer l'angle de vue. (style FPS) }
    function MoveAround(constref AMovingObjectUp, ATargetPosition: TBZVector; pitchDelta, turnDelta: Single): TBZVector;

    { @abstract(Translation du point depuis le centre "Center"  a une distance "ADistance")

      Si "AFromCenterSpot" est à :  @br
      @unordredlist(
	      @item( @true : la distance, est la distance que le point doit garder du centre)
	      @item( @false : la distance, est la distance que le point doit se déplacer de sa position actuelle par rapport au centre. )) }
    function ShiftObjectFromCenter(Constref ACenter: TBZVector; const ADistance: Single; const AFromCenterSpot: Boolean): TBZVector;

    { Retourne la moyenne de quatre point représentant un plan }
    function AverageNormal4(constref up, left, down, right: TBZVector): TBZVector;

    { @abstract(Implémente une fonction d'étape renvoyant une pour chaque composant de Self qui est
      supérieur ou égal au composant correspondant dans la référence du Vecteur B, et zéro si non)

      Plus d'informations : @br
      @unorderedlist(
	      @item(http://developer.download.nvidia.com/cg/step.html)
	      @item(https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/step.xhtml )) }
    function Step(ConstRef B : TBZVector4f):TBZVector4f;

    { @abstract(Retourne une normale telle quelle si le vecteur de position dans l'espace occulaire d'un sommet pointe dans la direction opposée d'une normale géométrique,
      sinon renvoie la version inversée de la normale.)

      Le vecteur courrant représente le vecteur de la normale pertubée. @br
      Le vecteur A représente un vecteur d'incidence (généralement un vecteur de direction de l'œil vers un sommet) @br
      Le vecteur B représente le vecteur normal géométrique (pour certaines facettes, la normale péturbée est identique).

      Plus d'informations : @br
      @unorderedlist(
	      @item(http://developer.download.nvidia.com/cg/faceforward.html)
	      @item(https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/faceforward.xhtml )) }
    function FaceForward(constref A, B: TBZVector4f): TBZVector4f;

   { @abstract(Renvoie le plus petit entier non inférieur à un scalaire ou à chaque composante vectorielle.)

     Retourne la valeur de la saturation dans la plage [0,1] comme suit: @br
     @orderedlist(
       @item(Retourne 0 si le vecteur courrant est plus petit que 0)
       @item(Retourne 1 si le vecteur courrant est plus grand que 1)
       @item(Dans les autres cas retourne les valeurs courrantes.))

     Plus d'informations : http://developer.download.nvidia.com/cg/saturate.html ) }
   function Saturate : TBZVector4f;

   { @abstract(Interpolation douce entre deux valeurs basé sur le troisième paramètre.)

	  @unorderedlist(
  	  @item(Retourne 0 si Self < A < B ou que Self > A > A)
		  @item(Retourne 1 si Self < B < A ou que Self > B > A)
		  @item(La valeur interpolée retournée est comprise dans l'intervalle 0.0 et 1.0 pour le domaina [A,B].))

      Si A = Self alors, la pente de Self.smoothstep(a, b) et b.smoothstep(a, b) est nulle.@br

      Plus d'information : @br
      @unorderedlist(
        @item(http://developer.download.nvidia.com/cg/smoothstep.html)
	      @item(https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/smoothstep.xhtml )) }
    function SmoothStep(ConstRef A,B : TBZVector4f): TBZVector4f;

    { @name :
      @param(N : TBZVector4f)
      @return(TBZVector4f ) }
    function Reflect(ConstRef N: TBZVector4f): TBZVector4f;


   // Project3dVectorOnToPlane.cs
   ////taken from http://answers.unity3d.com/questions/1097270/project-a-vector3-onto-a-plane-orthographically.html
   ////You simply project your vector onto the normal and subtract the result from your original vector. That will give you the projected vector on the plane. If you want to project a point to a plane you first need to calculate the relative vector from a point on that plane.
   //
   // Vector3 planeOrigin;
   // Vector3 planeNormal;
   // Vector3 point;
   //
   // Vector3 v = point - planeOrigin;
   // Vector3 d = Vector3.Project(v, planeNormal.normalized);
   // Vector3 projectedPoint = point - d;

    { Accès rapide aux propriétés de "Swizzling" comme dans HLSL et GLSL }
    {@groupbegin}
    property XY : TBZVector2f read GetXY Write SetXY;
    property YX : TBZVector2f read GetYX;
    property XZ : TBZVector2f read GetXZ;
    property ZX : TBZVector2f read GetZX;
    property YZ : TBZVector2f read GetYZ;
    property ZY : TBZVector2f read GetZY;
    property XX : TBZVector2f read GetXX;
    property YY : TBZVector2f read GetYY;
    property ZZ : TBZVector2f read GetZZ;

    property XYZ : TBZVector4f read GetXYZ;
    property XZY : TBZVector4f read GetXZY;
    property YXZ : TBZVector4f read GetYXZ;
    property YZX : TBZVector4f read GetYZX;
    property ZXY : TBZVector4f read GetZXY;
    property ZYX : TBZVector4f read GetZYX;

    property XXX : TBZVector4f read GetXXX;
    property YYY : TBZVector4f read GetYYY;
    property ZZZ : TBZVector4f read GetZZZ;

    property YYX : TBZVector4f read GetYYX;
    property XYY : TBZVector4f read GetXYY;
    property YXY : TBZVector4f read GetYXY;
	  {@groupend}
  end;

{%endregion%}

{%region%----[ TBZMatrixHelper ]-----------------------------------------------}

  { Assistant pour le type TBZMatrix eg : TBZMatrix4f }
  TBZMatrixHelper = record helper for TBZMatrix
  public

    // Create rotation matrix from "Euler angles" (Tait–Bryan angles) Yaw = Y, Pitch = Z, Roll = X  (angles in deg)
    { @name :
      @param( : )
      @param( : ) }
   // procedure CreateRotationMatrix(angleX, angleY, aangleZ : Single;Const  EulerOrder:TBZEulerOrder = eulYZX);overload;

    // Self is ViewProjMatrix
    //function Project(Const objectVector: TBZVector; const viewport: TVector4i; out WindowVector: TBZVector): Boolean;
    //function UnProject(Const WindowVector: TBZVector; const viewport: TVector4i; out objectVector: TBZVector): Boolean;
    // coordinate system manipulation functions

    { Retourne la matrice de rotation d'une rotation sur l'axe des Y de angle ( en radian) dans le système de coordonnées définie }
    function Turn(angle : Single) : TBZMatrix; overload;

    { Retourne la matrice de rotation d'une rotation sur l'axe des Y en fonction de la direction de l'axe représenté par "MasterUp" de angle ( en radian) dans le système de coordonnées définie }
    function Turn(constref MasterUp : TBZVector; Angle : Single) : TBZMatrix; overload;

    { Retourne la matrice de rotation d'une rotation sur l'axe des X de angle ( en radian) dans le système de coordonnées définie }
    function Pitch(Angle: Single): TBZMatrix; overload;

    { Retourne la matrice de rotation d'une rotation sur l'axe X en fonction de la direction de l'axe représenté par "MasterRight" de angle ( en radian) dans le système de coordonnées définie }
    function Pitch(constref MasterRight: TBZVector; Angle: Single): TBZMatrix; overload;

    { Retourne la matrice de rotation d'une rotation sur l'axe des Z de angle ( en radian) dans le système de coordonnées définie }
    function Roll(Angle: Single): TBZMatrix; overload;

    { Retourne la matrice de rotation d'une rotation sur l'axe Z en fonction de la direction de l'axe représenté par "MasterDirection" de angle ( en radian) dans le système de coordonnées définie }
    function Roll(constref MasterDirection: TBZVector; Angle: Single): TBZMatrix; overload;

    { @name :Convert matrix to Eulers Angles according Euler Order @br
       	     @bold(Note :) Assume matrix is a rotation matrix.
      @param( : )
      @param( : )
      @return(TBZ ) }
    //function ConvertToEulerAngles(Const EulerOrder : TBZEulerOrder):TBZEulerAngles;


  end;

{%endregion%}

{%endregion%}

{%region%----[ Vectors Const ]-------------------------------------------------}

Const
  { Vecteurs 2D standard }
  {@groupbegin}
  NullVector2f   : TBZVector2f = (x:0;y:0);
  OneVector2f   : TBZVector2f = (x:1;y:1);

  NullVector2d   : TBZVector2d = (x:0;y:0);
  OneVector2d   : TBZVector2d = (x:1;y:1);

  NullVector2i   : TBZVector2i = (x:0;y:0);
  OneVector2i   : TBZVector2i = (x:1;y:1);
  {@groupend}

  { Vecteurs affines 3D standard }
  {@groupbegin}
  XVector :    TBZAffineVector = (X:1; Y:0; Z:0);
  YVector :    TBZAffineVector = (X:0; Y:1; Z:0);
  ZVector :    TBZAffineVector = (X:0; Y:0; Z:1);
  XYVector :   TBZAffineVector = (X:1; Y:1; Z:0);
  XZVector :   TBZAffineVector = (X:1; Y:0; Z:1);
  YZVector :   TBZAffineVector = (X:0; Y:1; Z:1);
  XYZVector :  TBZAffineVector = (X:1; Y:1; Z:1);
  NullVector : TBZAffineVector = (X:0; Y:0; Z:0);
  {@groupend}

  { Vecteurs homogènes 4D standard }
  {@groupbegin}
  XHmgVector : TBZVector = (X:1; Y:0; Z:0; W:0);
  YHmgVector : TBZVector = (X:0; Y:1; Z:0; W:0);
  ZHmgVector : TBZVector = (X:0; Y:0; Z:1; W:0);
  WHmgVector : TBZVector = (X:0; Y:0; Z:0; W:1);
  NullHmgVector : TBZVector = (X: 0; Y: 0; Z: 0; W: 0);
  XYHmgVector   : TBZVector = (X: 1; Y: 1; Z: 0; W: 0);
  YZHmgVector   : TBZVector = (X: 0; Y: 1; Z: 1; W: 0);
  XZHmgVector   : TBZVector = (X: 1; Y: 0; Z: 1; W: 0);
  XYZHmgVector  : TBZVector = (X: 1; Y: 1; Z: 1; W: 0);
  XYZWHmgVector : TBZVector = (X: 1; Y: 1; Z: 1; W: 1);
  {@groupend}

  { Points standard homogènes 4D}
  {@groupbegin}
  XHmgPoint :  TBZVector = (X:1; Y:0; Z:0; W:1);
  YHmgPoint :  TBZVector = (X:0; Y:1; Z:0; W:1);
  ZHmgPoint :  TBZVector = (X:0; Y:0; Z:1; W:1);
  WHmgPoint :  TBZVector = (X:0; Y:0; Z:0; W:1);
  NullHmgPoint : TBZVector = (X:0; Y:0; Z:0; W:1);
  {@groupend}

  { Vecteur homogène unitaire négatif }
  NegativeUnitVector : TBZVector = (X:-1; Y:-1; Z:-1; W:-1);

  { Constantes de vecteur utiles }
  {@groupbegin}
  cNullVector2f   : TBZVector2f  = (x:0;y:0);
  cHalfOneVector2f     : TBZVector2f  = (X:0.5; Y:0.5);
  cNullVector4f   : TBZVector = (x:0;y:0;z:0;w:0);
  cNullVector4i   : TBZVector4i = (x:0;y:0;z:0;w:0);
  cOneVector4f    : TBZVector = (x:1;y:1;z:1;w:1);
  cOneMinusVector4f    : TBZVector = (x:-1;y:-1;z:-1;w:-1);
  cNegateVector4f_PNPN : TBZVector = (x:1;y:-1;z:1;w:-1);
  cWOneVector4f        : TBZVector = (x:0;y:0;z:0;w:1);
  cWOneSSEVector4f     : TBZVector = (X:0; Y:0; Z:0; W:1);
  cHalfOneVector4f     : TBZVector  = (X:0.5; Y:0.5; Z:0.5; W:0.5); //  00800000h,00800000h,00800000h,00800000h
  {@groupend}

{%endregion%}

{%region%----[ Matrix Const ]--------------------------------------------------}

Const
  { Matrice homogène d'identité }
  IdentityHmgMatrix : TBZMatrix = (V:((X:1; Y:0; Z:0; W:0),
                                      (X:0; Y:1; Z:0; W:0),
                                      (X:0; Y:0; Z:1; W:0),
                                      (X:0; Y:0; Z:0; W:1)));
  { Matrice homogène vide }
  EmptyHmgMatrix : TBZMatrix = (V:((X:0; Y:0; Z:0; W:0),
                                   (X:0; Y:0; Z:0; W:0),
                                   (X:0; Y:0; Z:0; W:0),
                                   (X:0; Y:0; Z:0; W:0)));
{%endregion%}

{%region%----[ Quaternion Const ]----------------------------------------------}

Const
 { Quaternion d'identité }
 IdentityQuaternion: TBZQuaternion = (ImagePart:(X:0; Y:0; Z:0); RealPart: 1);

{%endregion%}

{%region%----[ SSE Register States and utils funcs ]---------------------------}

// Retourne le mode actuel d'arrondissement SIMD
function sse_GetRoundMode: sse_Rounding_Mode;
// Définit le mode actuel d'arrondissement SIMD
procedure sse_SetRoundMode(Round_Mode: sse_Rounding_Mode);

{%endregion%}

{%region%----[ Vectors Arrays ]------------------------------------------------}

Type
  { Liste dynamique 2D contenant des TBZVector2f }
  TBZVector2f2DMap = class(specialize TBZArrayMap2DFloat<TBZVector2f>);
  { Liste dynamique 2D contenant des TBZVector4f }
  TBZVector4f2DMap = class(specialize TBZArrayMap2DFloat<TBZVector4f>);

  { Liste dynamique 1D contenant des TBZVector2i }
  TBZVector2iList = class(specialize TBZArrayInt<TBZVector2i>);
  { Liste dynamique 1D contenant des TBZVector2f }

  { TBZVector2fList }

  TBZVector2fList = class(specialize TBZArrayFloat<TBZVector2f>)
  protected
    function CompareValue(Const elem1, elem2) : Integer;  override;
  end;

  { Liste dynamique 1D contenant des TBZVector3B }
  TBZVector3bList = class(specialize TBZArrayByte<TBZVector3b>);
  //TBZVector3iList = class(specialize TBZArrayInt<TBZVector3i>);
  //TBZVector3fList = class(specialize TBZArrayFloat<TBZVector3f>);

  { Liste dynamique 1D contenant des TBZVector4b }
  TBZVector4bList = class(specialize TBZArrayByte<TBZVector4b>);
  { Liste dynamique 1D contenant des TBZVector4i }
  TBZVector4iList = class(specialize TBZArrayInt<TBZVector4i>);
  { Liste dynamique 1D contenant des TBZVector4f }
  TBZVector4fList = class(specialize TBZArrayFloat<TBZVector4f>);

  { Type de convenance pour un tableau 1D conteant des TBZVector2f }
  TBZArrayOfFloatPoints = TBZVector2fList;
  { Type de convenance pour un tableau 1D conteant des TBZVector2i }
  TBZArrayOfPoints  = TBZVector2iList;



  //  //---- Must be outside here ------------------------
      //  TBZVectorList = class(TBZVector4fList);
      //  TBZTexCoordList = class(TBZVector2fList);
      //  TBZColorVectorList = class(TBZVector4fList); //Class(TBZColorVectorList)
      //  TBZIndiceList = class(TBZIntegerList);

{%endregion%}

Implementation

Uses Math, BZMath; //, BZLogger; //, BZUtils;

{%region%----[ Internal Types and Const ]---------------------------------------}

Const
     { SSE rounding modes (bits in MXCSR register) }

  cSSE_ROUND_MASK         : DWord = $00009FFF;   // never risk a stray bit being set in MXCSR reserved
  cSSE_ROUND_MASK_NEAREST : DWord = $00000000;
  cSSE_ROUND_MASK_TRUNC   : DWord = $00006000;

//  cSSE_ROUND_MASK_DOWN    = $00002000;
//  cSSE_ROUND_MASK_UP      = $00004000;
//  cNullVector2i   : TBZVector2i = (x:0;y:0);


{$IFDEF USE_ASM}
  cSSE_MASK_ABS    : array [0..3] of UInt32 = ($7FFFFFFF, $7FFFFFFF, $7FFFFFFF, $7FFFFFFF);
  cSSE_MASK_NEGATE : array [0..3] of UInt32 = ($80000000, $80000000, $80000000, $80000000);
  cSSE_MASK_ONE :    array [0..3] of UInt32 = ($00000001, $00000001, $00000001, $00000001);
  cSSE_SIGN_MASK_NPPP   : array [0..3] of UInt32 = ($80000000, $00000000, $00000000, $00000000);
  cSSE_SIGN_MASK_PPPN   : array [0..3] of UInt32 = ($00000000, $00000000, $00000000, $80000000);
  cSSE_SIGN_MASK_NPNP   : array [0..3] of UInt32 = ($80000000, $00000000, $80000000, $00000000);
  cSSE_SIGN_MASK_PNPN   : array [0..3] of UInt32 = ($00000000, $80000000, $00000000, $80000000);
  cSSE_SIGN_MASK_PNNP   : array [0..3] of UInt32 = ($00000000, $80000000, $80000000, $00000000);
  cSSE_MASK_NO_W   : array [0..3] of UInt32 = ($FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $00000000);
  cSSE_MASK_ONLY_W : array [0..3] of UInt32 = ($00000000, $00000000, $00000000, $FFFFFFFF);
  //cSSE_MASK_NO_XYZ : array [0..3] of UInt32 = ($00000000, $00000000, $00000000, $FFFFFFFF);

  cSSE_OPERATOR_EQUAL             = 0;
  cSSE_OPERATOR_LESS              = 1;
  cSSE_OPERATOR_LESS_OR_EQUAL     = 2;
  cSSE_OPERATOR_ORDERED           = 3;
  cSSE_OPERATOR_NOT_EQUAL         = 4;
  cSSE_OPERATOR_NOT_LESS          = 5;
  cSSE_OPERATOR_NOT_LESS_OR_EQUAL = 6;
  cSSE_OPERATOR_NOT_ORDERED       = 7;
{$ENDIF}

const
   { cEulerOrderRef : Tableau représentant les différents ordre des angles d'Euler }
   cEulerOrderRef : array [Low(TBZEulerOrder)..High(TBZEulerOrder)] of array [1..3] of Byte =
      ( (0, 1, 2), (0, 2, 1), (1, 0, 2),     // eulXYZ, eulXZY, eulYXZ,
        (1, 2, 0), (2, 0, 1), (2, 1, 0) );   // eulYZX, eulZXY, eulZYX




// ---- Used by ASM Round & Trunc functions ------------------------------------
//var
//  _bakMXCSR, _tmpMXCSR : DWord;

//------------------------------------------------------------------------------

{%endregion%}

Var
  _oldMXCSR: DWord; // FLAGS SSE

//-----[ INTERNAL FUNCTIONS ]---------------------------------------------------

function AffineVectorMake(const x, y, z : Single) : TBZAffineVector; inline;
begin
   Result.X:=x;
   Result.Y:=y;
   Result.Z:=z;
end;

{ TBZVector2fList }

function TBZVector2fList.CompareValue(const elem1, elem2) : Integer;
var
  {$CODEALIGN VARMIN=16}
  i1 : TBZVector2f absolute elem1;
  i2 : TBZVector2f absolute elem2;
  {$CODEALIGN VARMIN=4}
begin
  if (i1.y = i2.y) then
  begin
    if (i1.x = i2.x) then Result := 0
    else if (i1.x < i2.x) then Result := -1 else Result := 1;
  end
  else if (i1.y < i2.y) then Result := -1 else Result := 1;
end;

//-----[ FUNCTIONS For TBZVector3f ]-------------------------------------------

function TBZVector3f.ToString : String;
begin
 Result := '(X: '+FloattoStrF(Self.X,fffixed,5,5)+
          ' ,Y: '+FloattoStrF(Self.Y,fffixed,5,5)+
          ' ,Z: '+FloattoStrF(Self.Z,fffixed,5,5);
end;


//-----[ INCLUDE IMPLEMENTATION ]-----------------------------------------------

{$ifdef USE_ASM}
  {$ifdef CPU64}
    {$ifdef UNIX}
      {$IFDEF USE_ASM_AVX}
         {$I vectormath_vector2i_native_imp.inc}
         {.$I vectormath_vector2i_unix64_avx_imp.inc}

         {$I vectormath_vector2f_native_imp.inc}
         {$I vectormath_vector2f_unix64_avx_imp.inc}

         {$I vectormath_vector2d_native_imp.inc}
         {$I vectormath_complex_native_imp.inc}

         {$I vectormath_vector3b_native_imp.inc}
         {$I vectormath_vector4b_native_imp.inc}

         {$I vectormath_vector4f_native_imp.inc}
         {$I vectormath_vector4f_unix64_avx_imp.inc}

         {$I vectormath_vector4i_native_imp.inc}
         {$I vectormath_vector4i_unix64_avx_imp.inc}

         {$I vectormath_quaternion_native_imp.inc}
         {$I vectormath_quaternion_unix64_avx_imp.inc}

         {$I vectormath_matrix4f_native_imp.inc}
         {$I vectormath_matrix4f_unix64_avx_imp.inc}
         {$I vectormath_matrixhelper_native_imp.inc}


         {$I vectormath_vectorhelper_native_imp.inc}
         {$I vectormath_vectorhelper_unix64_avx_imp.inc}

         {$I vectormath_hmgplane_native_imp.inc}
         {$I vectormath_hmgplane_unix64_avx_imp.inc}


      {$ELSE}
         {$I vectormath_vector2i_native_imp.inc}
         {$I vectormath_vector2i_unix64_sse_imp.inc}

         {$I vectormath_vector2f_native_imp.inc}
         {$I vectormath_vector2f_unix64_sse_imp.inc}

         {$I vectormath_vector2d_native_imp.inc}
         {$I vectormath_vector2d_unix64_sse_imp.inc}

         {$I vectormath_complex_native_imp.inc}
         {$I vectormath_complex_unix64_sse_imp.inc}

         {$I vectormath_vector3b_native_imp.inc}
         {$I vectormath_vector4b_native_imp.inc}

         {$I vectormath_vector4i_native_imp.inc}
         {$I vectormath_vector4i_unix64_sse_imp.inc}

         {$I vectormath_vector4f_native_imp.inc}
         {$I vectormath_vector4f_unix64_sse_imp.inc}

         {$I vectormath_quaternion_native_imp.inc}
         {$I vectormath_quaternion_unix64_sse_imp.inc}

         {$I vectormath_matrix4f_native_imp.inc}
         {$I vectormath_matrix4f_unix64_sse_imp.inc}
         {$I vectormath_matrixhelper_native_imp.inc}

         {$I vectormath_vector2fhelper_native_imp.inc}
         {$I vectormath_vector2fhelper_unix64_sse_imp.inc}

         {$I vectormath_vectorhelper_native_imp.inc}
         {$I vectormath_vectorhelper_unix64_sse_imp.inc}

         {$I vectormath_hmgplane_native_imp.inc}
         {$I vectormath_hmgplane_unix64_sse_imp.inc}

      {$ENDIF}
    {$else} // win64
      {$IFDEF USE_ASM_AVX}
          {$I vectormath_vector2i_native_imp.inc}
          {$I vectormath_vector2i_win64_avx_imp.inc}

          {$I vectormath_vector2f_native_imp.inc}
          {$I vectormath_vector2f_win64_avx_imp.inc}
          {$I vectormath_vector2f_win64_avx_imp.inc}

          {$I vectormath_vector2d_native_imp.inc}
          {$I vectormath_complex_native_imp.inc}

          {$I vectormath_vector3b_native_imp.inc}
          {$I vectormath_vector4b_native_imp.inc}

          {$I vectormath_vector4f_native_imp.inc}
          {$I vectormath_vector4f_win64_avx_imp.inc}

          {$I vectormath_vector4i_native_imp.inc}
          {$I vectormath_vector4i_win64_avx_imp.inc}

          {$I vectormath_quaternion_native_imp.inc}
          {$I vectormath_quaternion_win64_avx_imp.inc}

          {$I vectormath_matrix4f_native_imp.inc}
          {$I vectormath_matrix4f_win64_avx_imp.inc}
          {$I vectormath_matrixhelper_native_imp.inc}


          {$I vectormath_vectorhelper_native_imp.inc}
          {$I vectormath_vectorhelper_win64_avx_imp.inc}

          {$I vectormath_hmgplane_native_imp.inc}
          {$I vectormath_hmgplane_win64_avx_imp.inc}

       {$ELSE}
          {$I vectormath_vector2i_native_imp.inc}
          {$I vectormath_vector2i_win64_sse_imp.inc}

          {$I vectormath_vector2f_native_imp.inc}
          {$I vectormath_vector2f_win64_sse_imp.inc}

          {$I vectormath_vector2d_native_imp.inc}
          {$I vectormath_vector2d_win64_sse_imp.inc}

          {$I vectormath_complex_native_imp.inc}
          {$I vectormath_complex_win64_sse_imp.inc}

          {$I vectormath_vector3b_native_imp.inc}
          {.$I vectormath_vector3i_native_imp.inc}
          {.$I vectormath_vector3f_native_imp.inc}

          {$I vectormath_vector4b_native_imp.inc}
          {$I vectormath_vector4i_native_imp.inc}
          {$I vectormath_vector4i_win64_sse_imp.inc}

          {$I vectormath_vector4f_native_imp.inc}
          {$I vectormath_vector4f_win64_sse_imp.inc}



          {$I vectormath_quaternion_native_imp.inc}
          {$I vectormath_quaternion_win64_sse_imp.inc}

          {$I vectormath_matrix4f_native_imp.inc}
          {$I vectormath_matrix4f_win64_sse_imp.inc}


          {$I vectormath_hmgplane_native_imp.inc}
          {$I vectormath_hmgplane_win64_sse_imp.inc}

          {$I vectormath_vector2fhelper_native_imp.inc}
          {$I vectormath_vector2fhelper_win64_sse_imp.inc}

          {$I vectormath_vectorhelper_native_imp.inc}
          {$I vectormath_vectorhelper_win64_sse_imp.inc}

          {$I vectormath_matrixhelper_native_imp.inc}
          {.$I vectormath_matrixhelper_win64_sse_imp.inc}

       {$ENDIF}
    {$endif}  //unix
  {$else} // CPU32
     {$IFDEF USE_ASM_AVX}
         {$I vectormath_vector2f_native_imp.inc}
         {$I vectormath_vector2f_intel32_avx_imp.inc}

         {$I vectormath_vector2d_native_imp.inc}
         {$I vectormath_complex_native_imp.inc}

         {$I vectormath_vector3b_native_imp.inc}
         {$I vectormath_vector4b_native_imp.inc}

         {$I vectormath_vector4f_native_imp.inc}
         {$I vectormath_vector4f_intel32_avx_imp.inc}

         {$I vectormath_vectorhelper_native_imp.inc}
         {$I vectormath_vectorhelper_intel32_avx_imp.inc}

         {$I vectormath_hmgplane_native_imp.inc}
         {$I vectormath_hmgplane_intel32_avx_imp.inc}

         {$I vectormath_matrix4f_native_imp.inc}
         {$I vectormath_matrix4f_intel32_avx_imp.inc}
         {$I vectormath_matrixhelper_native_imp.inc}

         {$I vectormath_quaternion_native_imp.inc}
         {$I vectormath_quaternion_intel32_avx_imp.inc}


     {$ELSE}
        {$I vectormath_vector2f_native_imp.inc}
        {$I vectormath_vector2f_intel32_sse_imp.inc}

        {$I vectormath_vector2d_native_imp.inc}
        {$I vectormath_complex_native_imp.inc}

        {$I vectormath_vector3b_native_imp.inc}
        {$I vectormath_vector4b_native_imp.inc}

        {$I vectormath_vector4f_native_imp.inc}
        {$I vectormath_vector4f_intel32_sse_imp.inc}
        {$I vectormath_vector4i_native_imp.inc}
        {$I vectormath_vector4i_intel32_sse_imp.inc}

        {$I vectormath_vectorhelper_native_imp.inc}
        {$I vectormath_vectorhelper_intel32_sse_imp.inc}

        {$I vectormath_hmgplane_native_imp.inc}
        {$I vectormath_hmgplane_intel32_sse_imp.inc}

        {$I vectormath_matrix4f_native_imp.inc}
        {$I vectormath_matrix4f_intel32_sse_imp.inc}
        {$I vectormath_matrixhelper_native_imp.inc}

        {$I vectormath_quaternion_native_imp.inc}
        {$I vectormath_quaternion_intel32_sse_imp.inc}


     {$ENDIF}
  {$endif}

{$else}  // pascal
  {$I vectormath_vector2i_native_imp.inc}
  {$I vectormath_vector2f_native_imp.inc}
  {$I vectormath_vector2d_native_imp.inc}
  {$I vectormath_complex_native_imp.inc}

  {$I vectormath_vector3b_native_imp.inc}
  {.$I vectormath_vector3i_native_imp.inc}
  {.$I vectormath_vector3f_native_imp.inc}

  {$I vectormath_vector4b_native_imp.inc}
  {$I vectormath_vector4i_native_imp.inc}
  {$I vectormath_vector4f_native_imp.inc}

  {$I vectormath_quaternion_native_imp.inc}

  {$I vectormath_matrix4f_native_imp.inc}
  {$I vectormath_matrixhelper_native_imp.inc}

  {$I vectormath_hmgplane_native_imp.inc}
  {$I vectormath_vector2fhelper_native_imp.inc}
  {$I vectormath_vectorhelper_native_imp.inc}

{$endif}

{%region%----[ SSE Register States and utils Funcs ]----------------------------}

function get_mxcsr:dword;
var _flags:dword;
begin
     asm    stmxcsr _flags end;
     get_mxcsr:=_flags;
end;

procedure set_mxcsr(flags:dword);
var _flags:dword;
begin
     _flags:=flags;
     asm    ldmxcsr _flags end;
end;

function sse_GetRoundMode: sse_Rounding_Mode;
begin
    Result := sse_Rounding_Mode((get_MXCSR and (sse_MaskNegRound or sse_MaskPosRound)) shr 13);
end;

procedure sse_SetRoundMode(Round_Mode: sse_Rounding_Mode);
begin
 set_mxcsr ((get_mxcsr and sse_no_round_bits_mask) {%H-}or sse_Rounding_Flags[Round_Mode] );
end;

{%endregion%}

//==============================================================================

var
  _oldFPURoundingMode : TFPURoundingMode;

initialization

  { Nous devons définir le mode d'arrondi de FPC sur le même mode que notre code SSE
    Dans FPC, la fonction Round utilise l'algorithme 'd'arrondissement du banquier'.
    En gros, il ne fait pas de d'arrondissement supérieur ou inférieur si la valeur fractionnel est exactement égale à x.50
    Exemple : Round(2.5) = 2 et Round(3.5) = 4
    Pour plus infos voir : https://www.freepascal.org/docs-html/rtl/system/round.html }
 
  // Store Default FPC "Rounding Mode"
  _oldFPURoundingMode := GetRoundMode;
  Math.SetRoundMode(rmNearest);

  // Store MXCSR SSE Flags
  _oldMXCSR := get_mxcsr;
  set_mxcsr (mxcsr_default);

finalization

  // Restore MXCSR SSE Flags to default value
  set_mxcsr(_oldMXCSR);
  // Restore Default FPC "Rounding Mode"
  Math.SetRoundMode(_oldFPURoundingMode);

End.

