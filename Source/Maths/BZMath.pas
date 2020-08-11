(*
  @abstract(Contient des fonctions mathématiques optimisées. @br
  Des fonctions appoximatives peuvent être utilisées si désiré (utile en temps réel).)

  Par exemple: @br
  Les fonctions 'approximatives' de Sin et Cos ont une précision moyenne de 4E-8.

  Elle comprend des également des fonctions trigonométriques, calcul d'interpolations, @br
  Des fonctionnalités avancées comme Bessel, BlackMan. Et aussi quelques fonctions utiles

  Elle ajoute également des fonctions non définies dans l'unité Math FPC tel que : @br
  ArcCsc, ArcSec, ArcCot, CscH, SecH ect ...

  Pour plus de fonctions mathématiques, voir les unités: @br
  BZVectorMath, BZFastMath, BZRayMarchMath, BZGeoTools

  -------------------------------------------------------------------------------------------------------------

  @created(24/02/2019)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(Creation : 24/02/2019)
    @item(Mise à jour : )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes :)@br
  Quelques liens :
   @unorderedList(
       @item(http://www.i-logic.com/utilities/trig.htm)
       @item(http://jacksondunstan.com/articles/1217)
       @item(http://mathworld.wolfram.com)
     )

  -------------------------------------------------------------------------------------------------------------

  @bold(Dependances) : BZFastMath

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)@br
    @unorderedList(
      @item(J.Delauney (BeanzMaster))
    )

  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / LGPL

  ------------------------------------------------------------------------------------------------------------- *)
Unit BZMath;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================

{.$DEFINE USE_DOUBLE}

Interface

Uses
  Classes, SysUtils, Math;

//==============================================================================

{%region%-----[ Constantes et types mathématique utiles ]----------------------}

Const
  cInfinity = 1e1000;                                                 //< Nombre infini
  EpsilonFuzzFactor = 1000;                                           //< Facteur Epsilon pour précision étendue
  EpsilonXTResolution = 1E-19 * EpsilonFuzzFactor;                    //<  Résolution Epsilon étendue
  cPI: Double = 3.1415926535897932384626433832795;                    //< Valeur de PI
  cInvPI: Double = 1.0 / 3.1415926535897932384626433832795;           //< Valeur inverse de PI   = 0.31830988618379067153776752674503
  c2DivPI: Double = 2.0 / 3.1415926535897932384626433832795;          //< 2 Diviser par PI       = 0.63661977236758134307553505349006
  cPIdiv180: Single = 3.1415926535897932384626433832795 / 180;        //< PI diviser par 180     = 0.01745329251994329576923690768489‬
  cPI180 : Single =  3.1415926535897932384626433832795 * 180;         //< PI multiplier par 180  = 565.48667764616278292327580899031‬
  c180divPI: Single = 180 / 3.1415926535897932384626433832795;        //< 180 diviser par PI     = 57.295779513082320876798154814105‬
  c2PI: Single = 3.1415926535897932384626433832795*2;                 //< PI multiplier par 2    = 6.283185307179586476925286766559
  cInv2PI : Single = 1.0 / (3.1415926535897932384626433832795*2);     //< Inverse de PI multiplier par 2 = 0.15915494309189533576888376337251
  c4PI : Single = 3.1415926535897932384626433832795*4;                //< PI multiplier par 4    = 12.566370614359172953850573533118
  cPIdiv2: Single = 3.1415926535897932384626433832795 / 2;            //< PI diviser par 2       = 1.5707963267948966192313216916398
  cPIdiv3: Single = 3.1415926535897932384626433832795 / 3;            //< PI diviser par 3       = 1.0471975511965977461542144610932
  cPIdiv4: Single = 3.1415926535897932384626433832795 / 4;            //< PI diviser par 4       = 0.78539816339744830961566084581988
  c3PIdiv2: Single = 3*3.1415926535897932384626433832795/2;           //< 3 multiplier par PI diviser par 2  = 4.7123889803846898576939650749193
  c3PIdiv4: Single = 3*3.1415926535897932384626433832795/4;           //< 4 multiplier par PI diviser par 2  = 2.3561944901923449288469825374596

  cInv360: Single = 1 / 360;                                          //< Inverse de 360 = 0.00277777777777777777777777777778
  c180: Single = 180;                                                 //< 180
  c360: Single = 360;                                                 //< 360
  cOneHalf: Single = 0.5;                                             //< 0.5
  cMinusOneHalf: Single = -0.5;                                       //< -0.5
  cZero: Single = 0.0;                                                //< Zero
  cOne: Single = 1.0;                                                 //< 1.0
  cLn10: Single = 2.302585093;                                        //< Logarythme naturel de 10
  cLowEpsilon : Single = 1e-4;                                        //< Epsilon de très faible précision
  cEpsilon: Single = 1e-10;                                           //< Epsilon faible précision
  cEpsilon40 : Single = 1E-40;                                        //< Epsilon moyenne précision
  cEpsilon30 : Single = 1E-30;                                        //< Epsilon grande précision
  cFullEpsilon: Double = 1e-12;                                       //< Epsilon précision par défaut
  cColinearBias = 1E-8;                                               //< Bias colinéaire
  cEulerNumber = 2.71828182846;                                       //< Nombre d'Euler
  cInvSqrt2   = 1.0 / sqrt(2.0);                                      //< Inverse de la racine carré de 2 = 1.4142135623730950488016887242097
  cInvThree   = 1.0 / 3.0;                                            //< Inverse de 3 = 0.33333333333333333333333333333333
  cInv255 = 1/255;                                                    //< Inverse de 255 = 0.0039215686274509803921568627451
  cRadian = 180 / 3.1415926535897932384626433832795;                  //< 57.29577951;

  cBernstein: Single = 0.2801694990238691330364364912307;  //< Constante de Bernstein
  cCbrt2: Single     = 1.2599210498948731647672106072782;  //< Racine cubique de 2
  cCbrt3: Single     = 1.4422495703074083823216383107801;  //< Racine cubique de 3
  cCbrt10: Single    = 2.1544346900318837217592935665194;  //< Racine cubique de 10
  cCbrtPi: Single    = 1.4645918875615232630201425272638;  //< Racine cubique de PI

  // portées maximal les types de points flottants IEEE
  // Compatible avec Math.pas
  MinSingle   = 1.5e-45;                                             //< Nombre minimum Single
  MaxSingle   = 3.4e+38;                                             //< Nombre maximum Single
  MinDouble   = 5.0e-324;                                            //< Nombre minimum Double
  MaxDouble   = 1.7e+308;                                            //< Nombre maximum Double
  MinExtended = 3.4e-4932;                                           //< Nombre minimum Extended
  MaxExtended = 1.1e+4932;                                           //< Nombre maximuum Extended

  // Complex
  MinComp     = -9.223372036854775807e+18;                          //< Nombre minimum Complex
  MaxComp     = 9.223372036854775807e+18;                           //< Nombre maximum Complex


Type
  { Types réels:
    A point flottant: @br
    @unorderedlist(
      @item(Simple (Single)   32 bits : 7-8 chiffres significatifs)
      @item(Double (Double)   64 bits : 15-16 chiffres significatifs)
      @item(Etendu (Extended) 80 bits : 19-20 chiffres significatifs)
    )

    A point fixe: @br
      Devise (Currency) 64 bits : 19-20 chiffres significatifs, 4 après la virgule décimale. }
  FloatX  = Extended;
  PFloatX = ^FloatX;

  { TExtended80Rec : }
  TExtended80Rec = Packed Record
    Case Integer Of
      1: (Bytes: Array[0..9] Of Byte);
      2: (Float: Extended);
  End;

type
  { Type utilisé pour stocker des Sinus et Cosinus }
  {$ifdef USE_DOUBLE}
  TSinCos = record
    sin: double;
    cos: double;
  end;
  PSinCos = ^TSinCos;
  {$else}
  TSinCos = record
    sin: single;
    cos: single;
  end;
  PSinCos = ^TSinCos;
  {$endif}
  TBZCubicRootSolution = Array[0..2] of Single;

Type
  { Enumération des types d'interpolation pour l'unité BZMath : @br
    @unorderedlist(
      @item(itLinear : Interpolation Lineaire)
      @item(itPower  : Interpolation par puissance)
      @item(itSin    : Interpolation Sinuosidale)
      @item(itSinAlt : Interpolation Sinuosidale Alternative)
      @item(itTan    : Interpolation par tangente)
      @item(itLn     : Interpolation logarithmique)
      @item(itExp    : Interpolation exponentielle)
    )

    Voir : InterpolateValue }
  TBZInterpolationType = (itLinear, itPower, itSin, itSinAlt, itTan, itLn, itExp);

{%endregion%}

{%region%-----[ fonctions d'arrondissement ]-----------------------------------}

{ Arrondit une valeur vers son entier le plus proche }
Function Round(v: Single): Integer; Overload;
{ Arrondit une valeur vers zero }
Function Trunc(v: Single): Integer; Overload;
{ Arrondit une valeur vers l'infini négatif }
Function Floor(v: Single): Integer; Overload;
{ Retourne la partie fractionnaire d'un nombre }
Function Fract(v:Single):Single;
{ Arrondit une valeur vers l'infini positif }
Function Ceil(v: Single): Integer; Overload;
{ Arrondit la valeur en virgule flottante à l'entier le plus proche. @br
  Se comporte comme Round mais Retourne une valeur en virgule flottante. }
Function RoundFloat(v: Single): Single;
{ arrondit une valeur vers son entier le plus proche }
Function NewRound(x: Single): Integer;

{%endregion%}

{%region%-----[ fonctions utiles générales ]-----------------------------------}

//operator mod(const a,b:Single):Single;inline;overload;

{ calcule le reste d'une division en virgule flottante }
function fmod(const A,B:Single):Single;
{ Assure que valeur donnée soit dans une plage de deux valeurs entières }
function Clamp(const V : Integer; Min,Max : integer) : integer; overload;
{ Assure que valeur donnée soit dans une plage de deux valeurs en virgule flottante }
function Clamp(const V,Min,Max : Single):  Single; overload;
{ Assure que valeur donnée soit d'un minimum du paramètre en virgule flottante "aMin" }
function ClampMin(const V,aMin:Single):  Single;
{ Assure que la valeur donnée soit dans la plage de 0..255 }
function ClampByte(Const Value:Integer):Byte;
{ Vérifie si la valeur est proche de zéro ou zéro absolu }
Function IsZero(Const A: Extended; Const Epsilon: Extended = 0.0): Boolean;
{ Retourne le signe de la valeur x en utilisant la convention (-1, 0, +1) }
Function Sign(x: Single): Integer;
{ Retourne le signe strict de la valeur x en utilisant la convention (-1, +1) }
Function SignStrict(x: Single): Integer;
{ Retourne le maximum de trois valeurs entières }
Function Max(A, B, C: Integer): Integer; overload;
//Function Max(Const A, B, C: Byte): Byte; overload;

{ Retourne le minimum de trois valeurs entières }
Function Min(A, B, C: Integer): Integer; overload;
//Function Min(Const A, B, C: Byte): Byte; overload;

{ Retourne le maximum de deux valeurs en virgule flottante }
Function Max(v1, v2 : Single): Single; overload;
{ Retourne le minimum de deux valeurs en virgule flottante }
Function Min(v1, v2 : Single): Single; overload;
{ Retourne le maximum de deux valeurs Byte }
Function Max(v1, v2 : Byte): Byte; overload;
{ Retourne le minimum de deux valeurs Byte }
Function Min(v1, v2 : Byte): Byte; overload;
{ Retourne le maximum de deux valeurs Integer }
Function Max(v1, v2 : Integer): Integer; overload;
{ Retourne le minimum de deux valeurs Integer }
Function Min(v1, v2 : Integer): Integer; overload;
{ Retourne le maximum de trois valeurs en virgule flottante }
Function Max(v1, v2, v3: Single): Single; overload;
{ Retourne le minimum de trois valeurs en virgule flottante }
Function Min(v1, v2, v3: Single): Single; overload;
{ Calcule la valeur "réciproque" (1 / x) }
Function ComputeReciprocal(x: Single): Single;
{ Vérifie si la valeur donnée se trouve dans une plage de deux valeurs }
Function IsInRange(const X, a, b: Single): Boolean;
//Function IsInRange(const X, a, b: Integer): Boolean;overload;

{ Une fonction 'cercle vers le haut'. Renvoie y sur un cercle unitaire donné 1-x. Utile pour former des biseaux. @br
  x compris entre 0.0 et 1.0 }
function CircleUp(x : Single) : Single;
{ Une fonction 'cercle vers le bas'. Renvoie 1-y sur un cercle unitaire donné x. Utile pour former des biseaux. @br
  x compris entre 0.0 et 1.0 }
function CircleDown(x : Single) : Single;

{ Retourne 1.0 - x }
function flip(x : Single) : Single;

{ Similaire à Mod et fmod mais avec le respect des nombres négatif }
function Modulus(a, b : Single) : Single; overload;
{ Similaire à Mod et fmod mais avec le respect des nombres négatif }
function Modulus(a, b : Integer) : Integer; overload;

{%endregion%}

{%region%-----[ Fonctions sur les angles ]-------------------------------------}

{ Normalise un angle en radian }
Function NormalizeRadAngle(angle: Single): Single;
{ Normalise un angle en degree }
Function NormalizeDegAngle(angle: Single): Single;
{ Calcul la distance entre deux angles en degrée }
Function DistanceBetweenAngles(angle1, angle2: Single): Single;
{ Convertie un angle en degrée vers un angle en radian }
Function DegToRadian(Const Degrees: Single): Single;
{ Convertie un angle en radian vers un angle en degrée }
Function RadianToDeg(Const Radians: Single): Single;
{ Interpolation lineaire entre deux angles }
Function InterpolateAngleLinear(start, stop, t: Single): Single;

{%endregion%}

{%region%-----[ Fonctions sur les puissances ]---------------------------------}

{ Vérifie si une valeur données est de puissance de deux }
Function IsPowerOfTwo(Value: Longword): Boolean;
{ Retourne la puissance de deux suivante, d'une valeur donnée }
Function NextPowerOfTwo(Value: Cardinal): Cardinal;
{ Retourne la puissance de deux précédente, d'une valeur donnée }
Function PreviousPowerOfTwo(Value: Cardinal): Cardinal;
{ Elève la "base" à n'importe quelle puissance. Pour les exposants fractionnaires, ou | exposants | > MaxInt, la base doit être> 0. }
function Pow(const Base, Exponent: Single): Single; overload;
{ Elève une valeur donnée en virgule flottante, par une puissance en valeur entière }
function PowerInteger(Const Base: Single; Exponent: Integer): Single; overload;
{ Elève une valeur donnée entière, par une puissance en valeur entière }
Function PowerInt(Const Base, Power: Integer): Single;
{ Elève une valeur donnée à la puissance 3 }
Function pow3(x: Single): Single;

{%endregion%}

{%region%-----[ Fonctions trigonométrique ]------------------------------------}

{ Retourne le Sinus d'une valeur donnée }
function Sin(x:Single):Single; overload;
{ Retourne le Cosinus d'une valeur donnée }
function Cos(x:Single):Single; overload;
{ Retourne la tangente d'une valeur donnée }
function  Tan(const X : Single) : Single; overload;
{ Retourne le Sinus ety Cosinus d'une valeur donnée }
procedure SinCos(const Theta: Single; out Sin, Cos: Single); overload;
{ Retourne le sinus et le cosinus d'un angle donné et d'un rayon. @br
  @bold(Note) : les valeurs du sinus et du cosinus calculés sont multipliés par le rayon.}
procedure SinCos(const theta, radius : Single; out Sin, Cos: Single); overload;
{ Calcul de l'Hypotenus }
Function Hypot(Const X, Y: Single): Single;
//Function Hypot(Const X, Y, Z: Single): Single;

{ Retourne la cotangente d'un angle donné }
function  CoTan(const X : Single) : Single; overload;

{%endregion%}

{%region%-----[ Fonctions trigonométrique inverse ]----------------------------}

{ Retourne l' arc cosinus d'un angle donné }
function  ArcCos(const x : Single) : Single; overload;
{ Retourne l' arc sinus d'un angle donné }
Function  ArcSin(Const x: Single): Single;
{ Retourne l'arc tangente d'un angle et d'un quadrant }
function  ArcTan2(const Y, X : Single) : Single; overload;
{ Retourne l'inverse Cosecant d'une valeur donnée }
Function ArcCsc(Const X: Single): Single;
{ Retourne l'inverse secant d'une valeur donnée }
Function ArcSec(Const X: Single): Single;
{ Retourne l'inverse Cotangnete d'une valeur donnée }
Function ArcCot(Const X: Single): Single;

{%endregion%}

{%region%-----[ Fonctions hyperbolique ]---------------------------------------}

{ Retourne le sinus hyperbolique d'une valeur donnée }
function Sinh(const x : Single) : Single; overload;
{ Retourne le cosinus hyperbolique d'une valeur donnée }
function Cosh(const x : Single) : Single; overload;
{ Retourne la cosecante hyperbolique d'une valeur donnée }
Function CscH(Const X: Single): Single;
{ Retourne la secante hyperbolique d'une valeur donnée }
Function SecH(Const X: Single): Single;
{ Retourne la cotangente hyperbolique d'une valeur donnée }
Function CotH(Const X: Extended): Extended;
{ Retourne le sinus, cosinus hyperbolique d'une valeur donnée }
Function SinCosh(Const x: Single): Single;

{%endregion%}

{%region%-----[ Fonctions hyperbolique inverse ]-------------------------------}

{ Retourne la cosecante hyperbolique inverse d'une valeur donnée }
Function ArcCscH(Const X: Single): Single;
{ Retourne la secante hyperbolique inverse d'une valeur donnée }
Function ArcSecH(Const X: Single): Single;
{ Retourne la cotangnente hyperbolique inverse d'une valeur donnée }
Function ArcCotH(Const X: Single): Single;

{%endregion%}

{%region%-----[ Fonctions racine carré ]---------------------------------------}

{ Retourne la racine carré d'une valeur donnée }
function Sqrt(const A: Single): Single; overload;
{ Retourne la racine carré inverse d'une valeur donnée }
Function InvSqrt(v: Single): Single;

{%endregion%}

{%region%-----[ Fonctions logarithmique ]--------------------------------------}

{ Retourne le logarithme de base 2 d'une valeur donnée }
Function Log2(X: Single): Single; Overload;
{ Retourne le logarithme de base 10 d'une valeur donnée }
function Log10(X: Single): Single;
{ Retourne le logarithme de base N d'une valeur donnée }
function LogN(Base, X: Single): Single;

{%endregion%}

{%region%-----[ Fonctions logarithmique naturel ]------------------------------}

{ Retourne le logarithme naturel d'une valeur donnée }
Function Ln(X: Single): Single; overload;
{ Retourne ln (1 + X), précis pour X près de 0. }
Function LnXP1(x: Single): Single;

{%endregion%}

{%region%-----[ Fonctions exponentielles ]-------------------------------------}

{ Retourne exponentiation naturelle d'une valeur donnée }
Function Exp(Const X: Single): Single; overload;
{ multiplie la valeur donnée X par 2 puissance de N.}
Function ldExp(x: Single; N: Integer): Single;

{%endregion%}

{%region%-----[ Fonctions d'interpolations ]-----------------------------------}

{ Calcul du facteur d'ordre un de Bessel }
Function BesselOrderOne(x: Double): Double;
{ Calcul du facteur de Bessel }
Function Bessel(x: Double): Double;
{ Calcul du facteur IO de Bessel }
Function BesselIO(x: Double): Double;
{ Calcul du facteur de Blackman }
Function Blackman(x: Double): Double;
{ Une courbe de phase déphasée peut être utile si elle commence à zéro et se termine à zéro,
  pour certains comportements de rebond (suggéré par Hubert-Jan). @br
  Donner à "x" différentes valeurs entières pour ajuster la quantité de rebonds. Il culmine à 1.0. @br
  Elle peuvent prendre des valeurs négatives, mais elles peuvent rendre le calcul inutilisable dans certaines applications }
Function Sinc(x: Single): Single;
{ Calcul d'une interpolation linéaire avec distortion }
Function InterpolateValue(Const Start, Stop, Delta: Single; Const DistortionDegree: Single; Const InterpolationType: TBZInterpolationType): Single;
{ Calcul rapide d'une interpolation linéaire avec distortion }
Function InterpolateValueFast(Const OriginalStart, OriginalStop, OriginalCurrent: Single; Const TargetStart, TargetStop: Single;Const DistortionDegree: Single; Const InterpolationType: TBZInterpolationType): Single;
{ Calcul d'une interpolation linéaire avec distortion avec vérification des valeurs }
Function InterpolateValueSafe(Const OriginalStart, OriginalStop, OriginalCurrent: Single; Const TargetStart, TargetStop: Single;Const DistortionDegree: Single; Const InterpolationType: TBZInterpolationType): Single;
{ Calcul d'une interpolation bilinéaire }
function InterpolateBilinear(x,y : Single; nw, ne, sw, se : Single) : Single;

{%endregion%}

{%region%-----[ fonctions émulées "HL/GL Shader script" ]----------------------}

{ Calul d'une interpolation linéraire de type Hermite entre deux valeurs }
function SmoothStep(Edge0,Edge1,x: Single): Single;
{ Calul d'une interpolation linéraire de type Quintic entre deux valeurs }
function SmoothStepQuintic(Edge0,Edge1,x: Single): Single;
{ Calul d'une interpolation linéraire standard entre deux valeurs }
function Lerp(Edge0,Edge1,x: Single): Single;
{ Calul d'une interpolation Cubic }
function CubicLerp(Edge0,Edge1,Edge2, Edge3, x: Single): Single;
{ Calul d'une interpolation Cosine entre deux valeurs }
function CosineLerp(Edge0,Edge1,x: Single): Single;

//function SinLerp(Edge0,Edge1,x: Single): Single;
//function SinAltLerp(Edge0,Edge1,x: Single): Single;
//function ExpLerp(Edge0,Edge1,x: Single): Single;
//function LnLerp(Edge0,Edge1,x: Single): Single;
//function TanLerp(Edge0,Edge1,x: Single): Single;
//function PowerLerp(Edge0,Edge1,x: Single): Single;

{ Génére une fonction d'étape en comparant deux valeurs }
function Step(Edge,x: Single): Single;
{ Retourne la longueur d'un vecteur 1D }
Function Length1D(x:Single):Single;
{ Mixe deux valeurs en fonction du poid "a" }
function Mix(x,y,a : Double) : Double;


{%endregion%}

{%region%-----[ Fonctions utiles pour les animations ou interpolations ]-------}

{ Mélange la valeur avec un seuil et lisse avec un polynôme cubique. @br
  Voir aussi : http://www.iquilezles.org/www/articles/functions/functions.htm }
Function AlmostIdentity( x,m,n : single ):single;
{ Parfait pour déclencher des comportements ou créer des enveloppes pour la musique ou l'animation. @br
  Voir aussi : http://www.iquilezles.org/www/articles/functions/functions.htm }
function Impulse(k,x : Single):Single;
{ Interpolation cubique Identique à smoothstep (c-w, c, x) -smoothstep (c, c + w, x) @br
  Vous pouvez l'utiliser comme un remplacement bon marché pour une interpolation gaussienne. @br
  Voir aussi : http://www.iquilezles.org/www/articles/functions/functions.htm }
Function CubicPulse(c,w,x : Single) : Single;
{ Calcul une atténuation naturelle @br
  Voir aussi : http://www.iquilezles.org/www/articles/functions/functions.htm }
Function ExpStep(x,k,n:Single):Single;
{ Remappe l'intervalle  0..1 en 0..1, de sorte que les coins soient remappés à 0 et le centre à 1 @br
  @bold(Note) : parabola(0) = parabola(1) = 0, et parabola(1/2) = 1 @br
  Voir aussi : http://www.iquilezles.org/www/articles/functions/functions.htm }
Function Parabola(x,k:Single):Single;
{ Remappe l'intervalle 0..1 en 0..1, de sorte que les coins soient remappés à 0. @br
  Très utile pour incliner la forme d'un côté ou de l'autre afin de faire des feuilles, des yeux, et bien d'autres formes intéressantes. @br
  Voir aussi : http://www.iquilezles.org/www/articles/functions/functions.htm }
Function pcurve(x,a,b:Single):Single;
{ Remappe l'intervalle de l'unité dans l'intervalle de l'unité en élargissant les côtés et en comprimant le centre, et en gardant 1/2 mappé à 1/2. @br
  C'était une fonction courante dans les tutoriels RSL (Renderman Shading Language). @br
  K : k=1 est la courbe d'identité, k <1 produit la forme classique gain(), et k > 1 produit des courbes en forme de 's'. @br
  @bold(Note) :  Les courbes sont symétriques (et inverses) pour k = a et k = 1 / a. @br
  Voir aussi : http://www.iquilezles.org/www/articles/functions/functions.htm }
Function pGain(x,k:Single):Single;
{ Une variante de la fonction gamma. @br
  A = le nombre auquel appliquer le gain @br
  B = le paramètre du gain. @
  @unorderedlist(
    @item(0.5 signifie pas de changement,)
    @item(Des valeurs plus petites réduisent le gain,)
    @item(Des valeurs plus élevées augmentent le gain.)) }
function vGain(a, b : Single) : Single;
{ Applique une polarisation (bias) à un nombre dans l'intervalle unitaire,
  en déplaçant les nombres vers 0 ou 1 selon le paramètre de biais. @br
  A = le nombre à polariser @br
  B = le paramètre de polarisation. @br
  0.5 signifie pas de changement, des valeurs plus petites biaisent vers 0, plus grandes vers 1. }
function bias(a, b : Single) : Single;
{ Un déphaseur de courbe sinc @br
  Voir aussi : http://mathworld.wolfram.com/SincFunction.html }
Function pSinc(x,k:Single):Single;overload;

{%endregion%}

{%region%-----[ AUtres fonctions utiles ]--------------------------------------}

{ Convertir une valeur en pourcentage à partir de [min..max]}
Function Val2Percent(min,  max,val: Single): Integer;

{ Convertis un nombre de pixel "nbPixels" en centimètre }
function PixelsToCentimeters(nbPixels : Integer) : Double;
{ Convertis un nombre de centimètre "nbCm" en pixels }
function CentimetersToPixels(nbCm : Double) : Integer;

{ Convertis un nombre de pouce "nbInch" en centimètre }
function InchToCentimeters(nbInch : Double) : Double;
{ Convertis un nombre de centimètre "nbCm" en pouce }
function CentimetersToInch(nbCm : Double) : Double;

{ Convertis un nombre de pixel "nbPixels" en millimètre }
function PixelsToMillimeters(nbPixels : Integer) : Double;
{ Convertis un nombre de millimètre "nbMm" en pixels }
function MillimetersToPixels(nbMm : Double) : Integer;

{ Convertis un nombre de pouce "nbInch" en pixels }
function InchToPixels(nbInch : Double) : Integer;
{ Convertis un nombre de Pixels "nbPixels" en pouce }
function PixelsToInch(nbPixels: Integer) : Double;

{ @abstract(Convertis un nombre de pixel en centimètre en fonction de la résolution en DPI. @br
  @bold(Note) : DPI = "Dot Per Inch" (point par pouce) )

 @bold(Exemple) : Vous avez une image de dimension 512x512 pixels. @br
                  Sur un écran d'ordinateur de 72 dpi de résolution, cette image fera 18cm de côté. @br
                  Si vous imprimez cette image avec une résolution de 300dpi, cette image fera alors 4,33cm de coté }
function PixelsResolutionToCentimeters(nbPixels, DPI : Integer) : Double;

{ @abstract(Convertis un nombre de centimètre en fonction de la résolution en DPI en pixels
  @bold(Note) : DPI = "Dot Per Inch" (point par pouce) )

  @bold(Exemple) : Vous désirez imprimer une image de 5cm de côté avec une imprimante ayant une
                   résolution de 600 dpi. L'image devra alors avoir une dimension de 1181x1181 pixels }
function CentimetersToPixelsResolution(nbCm : Double; DPI : Integer) : Integer;

{ Retourne la racine cubique de x }
function CubeRoot(x : Single) : Single;

{ Retourne les solutions cubique pour les nombre a, b, c, d }
function CubicRoot(a, b, c, d : Single) : TBZCubicRootSolution;

{ Calcule la valeur en x de la fonction gaussienne }
function ComputeGaussian(x, Sigma : Single) : Single;

{ Calcule la valeur en x de la fonction gaussienne centrée }
function ComputeGaussianCenter(x, Sigma : Single) : Single;

{ Calcule la dérivé de la valeur en x de la fonction gaussienne }
function ComputeGaussianDerivative(x, Sigma : Single) : Single;
{ Calcul la moyenne de trois valeurs gaussiennes autour de "x" }
function ComputeMeanGaussian(x, Sigma : Single) : Single;

{ Calcul une distribution gaussienne de sigma pour x suivant le poid weight }
function ComputeGaussianDistribution(x, weight, sigma : Single) : Single;

{ Calcul la valeur en x du Laplacien gaussien }
function ComputeLoG(x, Sigma : Single) : Single;

{ Calcul la valeur en x du Laplacien gaussien centré }
function ComputeLoGCenter(x, Sigma : Single) : Single;

{ Calcul du nombre d'Euler en fonction de "weight" }
function ComputeEuler(Weight : Single) : Single;

{ Retourne la valeur factorielle de x }
function Factorial(x : Int64) : Int64;

{ Retourne le x ème polynome de Berstein d'ordre n+1 évalué en u (appartenant à 0..1) }
function ComputeBerstein(x, n : integer; u : Single) : Single;

{ Normalise une valeur en virgule flotantte comprise entre l'interval [InMin, InMax] vers l'interval [OutMin, OutMax] }
function RangeMap(value, InMin, InMax, OutMin, OutMax : Single) : Single; overload;

{ Normalise une valeur entière comprise entre l'interval [InMin, InMax] vers l'interval [OutMin, OutMax] }
function RangeMap(value, InMin, InMax, OutMin, OutMax : Integer) : integer; overload;

{%endregion%}

//==============================================================================

Implementation

//==============================================================================

{$IFDEF USE_FASTMATH} uses  BZFastMath;{$ENDIF}

//==============================================================================

{%region%=====[ fonctions d'arrondissement ]===================================}



Function Round(v : Single) : Integer;
Begin
  {$HINTS OFF}
  Result := System.round(v);
  {$HINTS ON}
End;

Function Trunc(v : Single) : Integer;
Begin
  {$HINTS OFF}
  Result := System.Trunc(v);
  {$HINTS ON}
End;

Function Fract(v : Single) : Single;
begin
  result := v - trunc(v);
  //result := frac(v); //v-Int(v);
end;

Function RoundFloat(v : Single) : Single;
Begin
  {$HINTS OFF}
  //Result := system.int
  Result := System.Trunc(v + cOneHalf);
  {$HINTS ON}
End;

Function NewRound(x : Single) : Integer;
Var
  y: Integer;
Begin
  y := 0;
  If (x - floor(x) < 0.5) Then
    y := floor(x)
  Else If (x - floor(x) = 0) Then
    y := Trunc(x)
  Else
    y := Trunc(x) + 1;
  Result := y;
End;

Function Ceil(v : Single) : Integer;
Begin
  {$HINTS OFF}
  //If Fract(v) > 0 Then
  //  Result := Trunc(v) + 1
  //Else
  //  Result := Trunc(v);
  Result := Trunc(v);
  if (v - Result) > 0 then Inc(Result);
  {$HINTS ON}
End;

Function Floor(v : Single) : Integer;
Begin
  {$HINTS OFF}
  result :=0;
  if (v=0.0) then exit
  else If (v > 0) Then
    Result := System.Trunc(v)
  Else
    Result := System.Trunc(v-0.999999999);
  {$HINTS ON}
End;

{%endregion%}

{%region%=====[ fonctions utiles générales ]===================================}

//operator mod(const a,b:Single):Single;
function fmod(const A, B : Single) : Single;
begin
  result := 0;
  if b=0 then exit;
  Result := a - b *  System.trunc(a / b);//a-b * Int(a/b); (a/b);//Int(a/b);//
end;

function Clamp(const V : Integer; Min,Max : integer) : integer;
begin
  Result := V;
  if Result > Max then begin result := Max; exit; end;
  if Result < Min then result := Min;
end;

function Clamp(const V,Min,Max : Single) : Single;
begin
  Result := V;
  if V > Max then result := Max
  else if V < Min then result := Min;
end;

function ClampMin(const V,aMin:Single):  Single;
begin
  if V < aMin then result := aMin
  else result := V;
end;

{$IFNDEF NO_ASM_OPTIMIZATIONS}
function ClampByte(Const Value : Integer) : Byte; assembler; nostackframe;
asm
{$IFDEF CPU64}
  {$IFDEF UNIX}
     MOV     EAX,EDI
  {$ELSE}
     // in win x64 calling convention parameters are passed in ECX, EDX, R8 & R9
     MOV     EAX,ECX
  {$ENDIF}
{$ENDIF}
        TEST    EAX,$FFFFFF00
        JNZ     @above
        RET
@above:
        JS      @below
        MOV     EAX,$FF
        RET
@Below:     XOR     EAX,EAX
end;
{$ELSE}
function ClampByte(const Value: Integer): Byte; inline;
begin
 Result := Value;
 if Value > 255 then Result := 255
 else if Value < 0 then Result := 0;
end;
{$ENDIF}

Function IsZero(Const A : Extended; Const Epsilon : Extended) : Boolean;
Var
  e: Extended;
Begin
  If Epsilon = 0 Then
    E := EpsilonXTResolution
  Else
    E := Epsilon;
  Result := FastAbs(A) <= E;
End;

Function Sign(x : Single) : Integer;
Begin
  {$IFDEF USE_FASTMATH}
    Result := FastSign(x);
  {$ELSE}
    If x < 0 Then
      Result := -1
    Else If x > 0 Then
      Result := 1
    Else
      Result := 0;
  {$ENDIF}
End;

Function SignStrict(x : Single) : Integer;
Begin
  If x < 0 Then
    Result := -1
  Else
    Result := 1;
End;

Function Min(v1, v2 : Single) : Single;
begin
   if v1<v2 then Result:=v1
   else Result:=v2;
end;

Function Max(v1, v2 : Byte) : Byte;
begin
  Result:=v1;
  if v1<v2 then Result:=v2;
end;

Function Min(v1, v2 : Byte) : Byte;
begin
  Result:=v1;
  if v1>v2 then Result:=v2;
end;

Function Max(v1, v2 : Integer) : Integer;
begin
  Result:=v1;
   if v1<v2 then Result:=v2;
end;

Function Min(v1, v2 : Integer) : Integer;
begin
  Result:=v1;
  if v1>v2 then Result:=v2;
end;

Function Max(v1, v2 : Single) : Single;
begin
  Result:=v2;
  if v1>v2 then Result:=v1;
end;

Function Min(v1, v2, v3 : Single) : Single;
Var
  N: Single;
Begin
  N := v3;
  If v1 < N Then N := v1;
  If v2 < N Then N := v2;
  Result := N;
End;

Function Max(v1, v2, v3 : Single) : Single;
Var
  N: Single;
Begin
  N := v3;
  If v1 > N Then N := v1;
  If v2 > N Then N := v2;
  Result := N;
End;

{$IFDEF USE_ASM_OPTIMIZATIONS}
Function Max(A, B, C : Integer) : Integer;  Assembler;
asm
  {$IFDEF CPU64}
    {$IFDEF UNIX}
        MOV       EAX, EDI
        CMP       ESI, EAX
        CMOVG     EAX, ESI
        CMP       EDX, EAX
        CMOVG     EAX, EDX
    {$ELSE}
        MOV       RAX, RCX
        MOV       RCX, R8
        CMP       EDX, EAX
        CMOVG     EAX, EDX
        CMP       ECX, EAX
        CMOVG     EAX, ECX
    {$ENDIF}
  {$ELSE}
        CMP       EDX, EAX
        CMOVG     EAX, EDX
        CMP       ECX, EAX
        CMOVG     EAX, ECX
  {$ENDIF}
End;
{$else}
function Max(A, B, C : Integer) : Integer; //Inline;
Var
  n: Integer;
Begin
  If A > C Then
    N := A
  Else
    N := C;
  If B > N Then
    N := B;
  Result := N;
End;
{$endif}

{$IFDEF USE_ASM_OPTIMIZATIONS}
Function Min(A, B, C : Integer) : Integer; Assembler;
Asm
  {$IFDEF CPU64}
    {$IFDEF UNIX}
        MOV       EAX, EDI
        CMP       ESI, EAX
        CMOVL     EAX, ESI
        CMP       EDX, EAX
        CMOVL     EAX, EDX
    {$ELSE}
        MOV       RAX, RCX
        MOV       RCX, R8
        CMP       EDX, EAX
        CMOVL     EAX, EDX
        CMP       ECX, EAX
        CMOVL     EAX, ECX
    {$ENDIF}
{$ELSE}
        CMP       EDX, EAX
        CMOVL     EAX, EDX
        CMP       ECX, EAX
        CMOVL     EAX, ECX
{$ENDIF}
End;
{$else}
function Min(A, B, C : Integer) : Integer; Inline;
Var
  n: Integer;
Begin
  If A < C Then
    N := A
  Else
    N := C;
  If B < N Then
    N := B;
  Result := N;
End;
{$endif}

Function ComputeReciprocal(x : Single) : Single;
Var
  a: Integer;
Begin
  Result := 0;
  If x = 0 Then exit;
  a := Sign(x);
  If ((a * x) >= cEpsilon) Then
    Result := 1.0 / x
  Else
    Result := a * (1.0 / cEpsilon);
End;

Function IsInRange(const X, a, b : Single) : Boolean;
begin
  if a < b then
    result := (a <= X) and (X <= b)
  else
    result := (b <= X) and (X <= a);
end;

function CircleUp(x : Single) : Single;
var
  xx : Single;
begin
  xx := 1-x;
  Result := System.sqrt(1-xx*xx);
end;

function CircleDown(x : Single) : Single;
begin
  Result := 1.0-System.sqrt(1-x*x);
end;

function flip(x : Single) : Single;
begin
  Result := 1 - x;
end;

function Modulus(a, b : Single) : Single;
Var
  n : Integer;
  r : Single;
begin
  n := System.Trunc(a/b);
  r := a - (b * n);
  if r<0 then result := r + b
  else result := r;
end;

function Modulus(a, b : Integer) : Integer;
Var
  n : Integer;
  r : Integer;
begin
  n := System.Trunc(a/b);
  r := a - (b * n);
  if r<0 then result := r + b
  else result := r;
end;

{%endregion%}

{%region%=====[ Fonctions sur les angles ]=====================================}

Function NormalizeRadAngle(angle : Single) : Single;
Begin
  Result := angle - RoundFloat(angle * cInv2PI) * c2PI;
  If Result > cPI Then Result := Result -  c2PI
  Else If Result < -PI Then Result := Result +  c2PI;
End;

Function NormalizeDegAngle(angle : Single) : Single;
Begin
  Result := angle - RoundFloat(angle * cInv360) * c360;
  If Result > c180 Then Result := Result - c360
  Else If Result < -c180 Then Result := Result + c360;
End;

Function DistanceBetweenAngles(angle1, angle2 : Single) : Single;
Begin
  angle1 := NormalizeRadAngle(angle1);
  angle2 := NormalizeRadAngle(angle2);
  Result := Abs(angle2 - angle1);
  If Result > cPI Then Result := c2PI - Result;
End;

Function DegToRadian(Const Degrees : Single) : Single;
Begin
  Result := Degrees * cPIdiv180;
End;

Function RadianToDeg(Const Radians : Single) : Single;
Begin
  Result := Radians * c180divPI;
End;

Function InterpolateAngleLinear(start, stop, t : Single) : Single;
Var
  d: Single;
Begin
  start := NormalizeRadAngle(start);
  stop := NormalizeRadAngle(stop);
  d := stop - start;
  If d > PI Then
  Begin
    // positive d, angle on opposite side, becomes negative i.e. changes direction
    d := -d - c2PI;
  End
  Else If d < -PI Then
  Begin
    // negative d, angle on opposite side, becomes positive i.e. changes direction
    d := d + c2PI;
  End;
  Result := start + d * t;
End;

{%endregion}

{%region%=====[ Fonctions sur les puissances ]=================================}

Function IsPowerOfTwo(Value : Longword) : Boolean;
Const
  BitCountTable: Array[0..255] Of Byte =
    (0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8);

  Function BitCount(Value: Longword): Longword; inline;
  Var
    V: Array[0..3] Of Byte absolute Value;
  Begin
    Result := BitCountTable[V[0]] + BitCountTable[V[1]] + BitCountTable[V[2]] + BitCountTable[V[3]];
  End;
Begin
  Result := BitCount(Value) = 1;
End;

Function PreviousPowerOfTwo(Value : Cardinal) : Cardinal;
Var
  I, N: Cardinal;
Begin
  Result := 0;
  For I := 14 Downto 2 Do
  Begin
    N := (1 Shl I);
    If N < Value Then
      Break
    Else
      Result := N;
  End;
End;

Function NextPowerOfTwo(Value : Cardinal) : Cardinal;
Begin
  If (Value > 0) Then
  Begin
    Dec(Value);
    Value := Value Or (Value Shr 1);
    Value := Value Or (Value Shr 2);
    Value := Value Or (Value Shr 4);
    Value := Value Or (Value Shr 8);
    Value := Value Or (Value Shr 16);
  End;

  Result := Value + 1;
End;

function Pow(const Base, Exponent : Single) : Single;
begin
  {$IFDEF USE_FASTMATH}
    if exponent=cZero then Result:=cOne
    else if (base=cZero) and (exponent>cZero) then Result:=cZero
    else if RoundFloat(exponent)=exponent then Result:=FastPower(base, Integer(Round(exponent)))
    else Result:=FastExp(exponent*FastLn(base));
  {$ELSE}
    {$HINTS OFF}
    if exponent=cZero then Result:=cOne
    else if (base=cZero) and (exponent>cZero) then Result:=cZero
    else if RoundFloat(exponent)=exponent then Result:=Power(base, Integer(Round(exponent)))
    else Result:=Exp(exponent*Ln(base));
    {$HINTS ON}
   {$ENDIF}
end;

function PowerInteger(Const Base : Single; Exponent : Integer) : Single;
begin
  {$IFDEF USE_FASTMATH}
    result := FastPower(Base,Exponent);
  {$ELSE}
    {$HINTS OFF}
    Result:=Math.Power(Base, Exponent);
    {$HINTS ON}
  {$ENDIF}
end;

Function PowerInt(Const Base, Power : Integer) : Single;
Var
  I:    Integer;
  Temp: Double;
Begin
  Temp := 1;
  For I := 0 To Pred(Power) Do
    Temp := Temp * Base;

  Result := Temp;
End;

Function pow3(x : Single) : Single;
Begin
  If x = 0.0 Then
    Result := 0.0
  Else
    Result := x * x * x;
End;

{%endregion}

{%region%=====[ Fonctions trigonométrique ]====================================}

function Sin(x:Single):Single;Inline;
begin
  {$IFDEF USE_FASTMATH}
    result := FastSinLUT(x);//RemezSin(x);
  {$ELSE}
    result := System.Sin(x);
  {$ENDIF}
end;

function Cos(x:Single):Single; Inline;
begin
  {$IFDEF USE_FASTMATH}
    result := FastCosLUT(x); //RemezCos(x);
  {$ELSE}
    result := System.Cos(x);
  {$ENDIF}
end;

function Tan(const X : Single) : Single;
begin
  {$IFDEF USE_FASTMATH}
    Result := FastTan(x);
  {$ELSE}
    {$HINTS OFF}
    Result:=Math.Tan(x);
    {$HINTS ON}
  {$ENDIF}
end;

procedure SinCos(const Theta: Single; out Sin, Cos: Single);
var
   s, c : Single;
begin
  {$ifdef USE_FASTMATH}
    //C := RemezCos(Theta);
    //S := RemezCos(cPIdiv2-Theta);
    S := FastSinLUT(Theta);
    C := FastCosLUT(Theta);
  {$else}
     Math.SinCos(Theta, s, c);
  {$endif}
  {$HINTS OFF}
     Sin:=s; Cos:=c;
  {$HINTS ON}
end;

procedure SinCos(const theta, radius : Single; out Sin, Cos: Single);
var
   s, c : Single;
begin
  {$ifdef USE_FASTMATH}
    S := FastSinLUT(Theta);
    C := FastCosLUT(Theta);
  {$else}
     Math.SinCos(Theta, s, c);
  {$endif}
  {$HINTS OFF}
   //  Sin:=s; Cos:=c;
   Sin:=s*radius;
   Cos:=c*radius;
  {$HINTS ON}
end;

function CoTan(const X : Single) : Single;
begin
   {$HINTS OFF}
   Result:=Math.CoTan(x);
   {$HINTS ON}
end;

Function Hypot(Const X, Y : Single) : Single;
Begin
  {$IFDEF USE_FASTMATH}
    Result := FastSqrt((X*X) + (Y*Y));
  {$ELSE}
    Result := System.Sqrt(Sqr(X) + Sqr(Y));
  {$ENDIF}
End;

{%endregion%}

{%region%=====[ Fonctions trigonométrique inverse ]============================}

function ArcCos(const x : Single): Single;
begin
  {$IFDEF USE_FASTMATH}
    if FastAbs(X) > 1.0 then
       Result := FastArcCosine(FastSign(X))
     else
     Result:=FastArcCosine(X);
  {$ELSE}
    {$HINTS OFF}
     if Abs(X) > 1.0 then
       Result := Math.ArcCos(Sign(X))
     else
     Result:=Math.ArcCos(X);
   {$HINTS ON}
  {$ENDIF}
end;

Function ArcSin(Const x : Single) : Single;
begin
  {$IFDEF USE_FASTMATH}
    Result:= FastArcTan2(X, FastSqrt(1 - (x*x)))
  {$ELSE}
    Result:= Math.ArcTan2(X, System.Sqrt(1 - Sqr(X)))
  {$ENDIF}
end;

function ArcTan2(const Y, X : Single) : Single;
begin
  {$IFDEF USE_FASTMATH}
    Result:= FastArcTan2(y, x)
  {$ELSE}
    Result:= Math.ArcTan2(x,y)
  {$ENDIF}
end;

Function ArcCsc(Const X : Single) : Single;
Begin
  If IsZero(X) Then
    Result := cInfinity
  Else
    Result := ArcSin(1 / X);
End;

Function ArcSec(Const X : Single) : Single;
Begin
  If IsZero(X) Then
    Result := cInfinity
  Else
    Result := ArcCos(1 / X);
End;

Function ArcCot(Const X : Single) : Single;
Begin
  If IsZero(X) Then Result := cPIDiv2
  Else
    {$IFDEF USE_FASTMATH}
      Result := FastArcTan(1 / X);
    {$ELSE}
      Result := ArcTan(1 / X);
    {$ENDIF}
End;

{%endregion%}

{%region%=====[ Fonctions hyperbolique ]=======================================}

function Sinh(const x : Single) : Single;
begin
   {$IFDEF USE_FASTMATH}
      Result:=0.5*(FastExp(x) - FastExp(-x));
   {$ELSE}
     Result:=0.5*(Exp(x)- Exp(-x));
   {$ENDIF}
end;

function Cosh(const x : Single) : Single;
begin
  {$IFDEF USE_FASTMATH}
     Result:=0.5*(FastExp(x)+ FastExp(-x));
  {$ELSE}
    Result:=0.5*(Exp(x)+ Exp(-x));
  {$ENDIF}
end;

Function SinCosh(Const x : Single) : Single;
Begin
  {$IFDEF USE_FASTMATH}
    Result := 0.5 * (FastExp(x) - FastExp(-x));
  {$ELSE}
    Result := 0.5 * (Exp(x) - Exp(-x));
  {$ENDIF}
End;

Function CscH(Const X : Single) : Single;
Begin
  Result := 1 / SinH(X);
End;

Function SecH(Const X : Single) : Single;
Begin
  Result := 1 / CosH(X);
End;

Function CotH(Const X : Extended) : Extended;
Begin
  Result := 1 / TanH(X);
End;

{%endregion%}

{%region%=====[ Fonctions hyperbolique inverse ]===============================}

Function ArcCscH(Const X : Single) : Single;
Begin
  {$IFDEF USE_FASTMATH}
    If IsZero(X) Then Result := cInfinity
    Else
    If X < 0 Then
      Result := FastLn((1 - FastInvSqrt(1 + X * X)) * X)
    Else
      Result := FastLn((1 + FastInvSqrt(1 + X * X)) * X);
  {$ELSE}
    If IsZero(X) Then
      Result := Infinity
    Else
    If X < 0 Then
      Result := System.Ln((1 - System.Sqrt(1 + X * X)) / X)
    Else
      Result := System.Ln((1 + System.Sqrt(1 + X * X)) / X);
  {$ENDIF}
End;

Function ArcSecH(Const X : Single) : Single;
Begin
  {$IFDEF USE_FASTMATH}
  If IsZero(X) Then Result := cInfinity
  Else If SameValue(X, 1) Then Result := 0
  Else
    Result := FastLn((FastInvSqrt(1 - X * X) + 1) * X);
  {$ELSE}
    If IsZero(X) Then Result := cInfinity
    Else If SameValue(X, 1) Then Result := 0
    Else
      Result := System.Ln((System.Sqrt(1 - X * X) + 1) / X);
  {$ENDIF}
End;

Function ArcCotH(Const X : Single) : Single;
Begin
  {$IFDEF USE_FASTMATH}
    If SameValue(X, 1) Then Result := cInfinity // 1.0 / 0.0
    Else If SameValue(X, -1) Then Result := -cInfinity // -1.0 / 0.0
    Else
      Result := 0.5 * FastLn((X + 1) / (X - 1));
  {$ELSE}
    If SameValue(X, 1) Then Result := cInfinity // 1.0 / 0.0
    Else If SameValue(X, -1) Then Result := -cInfinity // -1.0 / 0.0
    Else
      Result := 0.5 * Ln((X + 1) / (X - 1));
  {$ENDIF}
End;

{%endregion%}

{%region%=====[ Fonctions racine carré ]=======================================}

function Sqrt(const A: Single): Single;
Begin
  {$IFDEF USE_FASTMATH}
    {$IFDEF CPU64}
    Result := FastSqrt(A);
    {$ELSE}
    Result := System.Sqrt(A);
    {$ENDIF}
  {$ELSE}
    Result := System.Sqrt(A);
  {$ENDIF}
End;

Function InvSqrt(v : Single) : Single;
{$IFNDEF USE_FASTMATH}  var s : single; {$ENDIF}
Begin
  {$IFDEF USE_FASTMATH}
    Result := FastInvSqrt(v);
  {$ELSE}
    s:= System.sqrt(v); //+ EpsilonXTResolution;
    Result := 1 / s;
  {$ENDIF}
End;

{%endregion%}

{%region%=====[ Fonctions logarithmique ]======================================}

function Log10(X : Single) : Single;
// Log10(X):=Log2(X) * Log10(2)
Begin
  {$IFDEF USE_FASTMATH}
     Result := FastLog10(x);
  {$ELSE}
     Result := Math.Log10(X);
    //Result := Ln(x) * 0.4342944819;    // 1/ln(10)
  {$ENDIF}
End;

Function Log2(X : Single) : Single;
Begin
  {$IFDEF USE_FASTMATH}
     Result := FastLog2(x);
  {$ELSE}
    Result := Math.Log2(X);
   //Result := Ln(x) * 1.4426950408889634079;    // 1/ln(2)
  {$ENDIF}
End;

function LogN(Base, X : Single) : Single;
Begin
  {$IFDEF USE_FASTMATH}
    // LogN(X):=Log2(X) / Log2(N)
    Result := FastLog2(Base) / FastLog2(X);
  {$ELSE}
    Result := Math.LogN(Base, X);
  {$ENDIF}
End;

{%endregion%}

{%region%=====[ Fonctions logarithmique naturel ]==============================}

Function Ln(X : Single) : Single;
Var
  aLo, aHi, aMid, aVal: Single;
Begin
  If (X < 0) Then
  Begin
    Result := 0;
    Exit;
  End;

  // use recursion to get approx range
  If (X < 1) Then
  Begin
    Result := -BZMath.ln(1 / X);
    Exit;
  End;

  If (X > cEulerNumber) Then
  Begin
    Result := BZMath.Ln(X / cEulerNumber) + 1;
    Exit;
  End;

  // X is now between 1 and e
  // Y is between 0 and 1
  alo := 0.0;
  ahi := 1.0;

  While (True) Do
  Begin
    amid := (alo + ahi) / 2;
    aval := BZMath.exp(amid);
    If (aval > X) Then ahi := amid;

    If (aval < X) Then alo := amid;

    If (abs(aval - X) < 0.0001) Then //cEpsilon
    Begin
      Result := amid;
      Exit;
    End;
  End;
End;

Function LnXP1(x : Single) : Single;
{$IFDEF USE_FASTMATH}   Var  y: Single; {$ENDIF}
Begin
  {$IFDEF USE_FASTMATH}
    If (x >= 4.0) Then
      Result := FastLn(1.0 + x)
    Else
    Begin
      y := 1.0 + x;
      If (y = 1.0) Then
        Result := x
      Else
      Begin
        Result := FastLn(y);     // lnxp1(-1) = ln(0) = -Inf
        If y > 0.0 Then
          Result := Result + (x - (y - 1.0)) / y;
      End;
    End;
  {$ELSE}
    Result := Math.LnXP1(X);
  {$ENDIF}
End;

{%endregion%}

{%region%=====[ Fonctions exponentielles ]=====================================}

Function Exp(Const X : Single) : Single;
Var
  I, N: Integer;
  D:    Double;
Begin
  If (X = 1.0) Then
    Result := cEulerNumber
  Else
  If (x < 0) Then
    Result := 1.0 / Exp(-X)
  Else
  Begin
    N := 2;
    Result := 1.0 + X;
    Repeat
      D := X;
      For I := 2 To N Do
      Begin
        D := D * (X / I);
      End;

      Result := Result + D;
      Inc(N);
    Until (d <= cEpsilon);
  End;
End;

Function ldExp(x : Single; N : Integer) : Single;
Var
  r: Single;
Begin
  R := 1;
  If N > 0 Then
  Begin
    While N > 0 Do
    Begin
      R := R * 2;
      Dec(N);
    End;
  End
  Else
  Begin
    While N < 0 Do
    Begin
      R := R / 2;
      Inc(N);
    End;
  End;

  Result := x * R;
End;

{%endregion%}

{%region%=====[ Fonctions d'interpolations ]===================================}

Function BesselOrderOne(x : Double) : Double;
Var
  p, q: Double;

  Function J1(x: Double): Double;
  Const
    Pone: Array[0..8] Of Double =
      (
      0.581199354001606143928050809e+21, -0.6672106568924916298020941484e+20,
      0.2316433580634002297931815435e+19, -0.3588817569910106050743641413e+17,
      0.2908795263834775409737601689e+15, -0.1322983480332126453125473247e+13,
      0.3413234182301700539091292655e+10, -0.4695753530642995859767162166e+7,
      0.270112271089232341485679099e+4
      );
    Qone: Array[0..8] Of Double =
      (
      0.11623987080032122878585294e+22,
      0.1185770712190320999837113348e+20,
      0.6092061398917521746105196863e+17,
      0.2081661221307607351240184229e+15,
      0.5243710262167649715406728642e+12,
      0.1013863514358673989967045588e+10,
      0.1501793594998585505921097578e+7,
      0.1606931573481487801970916749e+4,
      0.1e+1
      );
  Var
    pj, qj: Double;
    i:      Byte;
  Begin
    pj := 0.0;
    qj := 0.0;
    pj := Pone[8];
    qj := Qone[8];
    For i := 7 Downto 0 Do
    Begin
      pj := pj * x * x + Pone[i];
      qj := qj * x * x + Qone[i];
    End;
    Result := (pj / qj);
  End;

  Function P1(x: Double): Double;
  Const
    Pone: Array[0..5] Of Double =
      (
      0.352246649133679798341724373e+5,
      0.62758845247161281269005675e+5,
      0.313539631109159574238669888e+5,
      0.49854832060594338434500455e+4,
      0.2111529182853962382105718e+3,
      0.12571716929145341558495e+1
      );
    Qone: Array[0..5] Of Double =
      (
      0.352246649133679798068390431e+5,
      0.626943469593560511888833731e+5,
      0.312404063819041039923015703e+5,
      0.4930396490181088979386097e+4,
      0.2030775189134759322293574e+3,
      0.1e+1
      );
  Var
    xx, pp, qp: Double;
    i: Byte;
  Begin
    pp := 0.0;
    qp := 0.0;
    pp := Pone[5];
    qp := Qone[5];
    xx := 8.0 / x;
    xx := xx * xx;
    For i := 4 Downto 0 Do
    Begin
      pp := pp * xx + Pone[i];
      qp := qp * xx + Qone[i];
    End;
    Result := (pp / qp);
  End;

  Function Q1(x: Double): Double;
  Const
    Pone: Array[0..5] Of Double =
      (
      0.3511751914303552822533318e+3,
      0.7210391804904475039280863e+3,
      0.4259873011654442389886993e+3,
      0.831898957673850827325226e+2,
      0.45681716295512267064405e+1,
      0.3532840052740123642735e-1
      );
    Qone: Array[0..5] Of Double =
      (
      0.74917374171809127714519505e+4,
      0.154141773392650970499848051e+5,
      0.91522317015169922705904727e+4,
      0.18111867005523513506724158e+4,
      0.1038187585462133728776636e+3,
      0.1e+1
      );
  Var
    xx, pq, qq: Double;
    i: Byte;
  Begin
    pq := 0.0;
    qq := 0.0;
    pq := Pone[5];
    qq := Qone[5];
    xx := 8.0 / x;
    xx := xx * xx;
    For i := 4 Downto 0 Do
    Begin
      pq := pq * xx + Pone[i];
      qq := qq * xx + Qone[i];
    End;
    Result := (pq / qq);
  End;

Begin
  Result := 0.0;
  If x = 0.0 Then
    exit;
  p := x;
  If x < 0.0 Then
    x := -x;
  If x < 8.0 Then
    Result := p * J1(x)
  Else
  Begin
    q := Sqrt(2.0 / (cPI * x)) * (P1(x) * (cInvSqrt2 * (System.sin(x) - System.cos(x))) - 8.0 / x * Q1(x) * (-cInvSqrt2 * (System.sin(x) + System.cos(x))));
    If p < 0.0 Then q := -q;
    Result := q;
  End;
End;

Function Bessel(x : Double) : Double;
Begin
  If x = 0.0 Then
    Result := cPIdiv4
  Else
    Result := BesselOrderOne(cPI * x) / (2.0 * x);
End;

Function BesselIO(x : Double) : Double;
Var
  I: Integer;
  Sum, Y, T: Double;
Begin
  Y := Sqr(0.5 * X);
  T := Y;
  I := 2;
  Sum := 0.0;
  While T > cFullEpsilon Do
  Begin
    Sum := Sum + T;
    T := T * (Y / (I * I));
    Inc(I);
  End;
  Result := Sum;
End;

Function Blackman(x : Double) : Double;
Var
  xpi: Double;
Begin
  xpi := PI * x;
  Result := 0.42 + 0.5 * System.cos(xpi) + 0.08 * System.cos(2 * xpi);
End;

Function Sinc(x : Single) : Single;
{$IFNDEF USE_FASTMATH} Var xx: Single; {$ENDIF}
Begin
  result :=0.0;
  {$IFDEF USE_FASTMATH}
  If x = 0.0 Then Result := 1.0
  Else FastSinC(x);
  {$ELSE}
    If x = 0.0 Then
      Result := 1.0
    Else
    Begin
      xx := cPI * x;
      Result := System.Sin(xx) / (xx);
    End;
  {$ENDIF}
End;

Function InterpolateLinear(Const start, stop, Delta: Single): Single;
Begin
  Result := start + (stop - start) * Delta;
End;

Function InterpolateLn(Const Start, Stop, Delta: Single; Const DistortionDegree: Single): Single;
Begin
  Result := (Stop - Start) * Ln(1 + Delta * DistortionDegree) / Ln(1 + DistortionDegree) + Start;
End;

Function InterpolateExp(Const Start, Stop, Delta: Single; Const DistortionDegree: Single): Single;
Begin
  Result := (Stop - Start) * Exp(-DistortionDegree * (1 - Delta)) + Start;
End;

Function InterpolatePower(Const Start, Stop, Delta: Single; Const DistortionDegree: Single): Single;
Var
  i: Integer;
Begin
  If (Round(DistortionDegree) <> DistortionDegree) And (Delta < 0) Then
  Begin
    i := Round(DistortionDegree);
    Result := (Stop - Start) * PowerInteger(Delta, i) + Start;
  End
  Else
    Result := (Stop - Start) * Power(Delta, DistortionDegree) + Start;
End;

Function InterpolateSinAlt(Const Start, Stop, Delta: Single): Single;
Begin
  Result := (Stop - Start) * Delta * Sin(Delta * cPIDiv2) + Start;
End;

Function InterpolateSin(Const Start, Stop, Delta: Single): Single;
Begin
  Result := (Stop - Start) * Sin(Delta * cPIDiv2) + Start;
End;

Function InterpolateTan(Const Start, Stop, Delta: Single): Single;
Begin
  Result := (Stop - Start) * Tan(Delta * cPIDiv4) + Start;
End;

Function InterpolateValue(Const Start, Stop, Delta : Single; Const DistortionDegree : Single; Const InterpolationType : TBZInterpolationType) : Single;
Begin
  Case InterpolationType Of
    itLinear: Result := InterpolateLinear(Start, Stop, Delta);
    itPower: Result := InterpolatePower(Start, Stop, Delta, DistortionDegree);
    itSin: Result := InterpolateSin(Start, Stop, Delta);
    itSinAlt: Result := InterpolateSinAlt(Start, Stop, Delta);
    itTan: Result := InterpolateTan(Start, Stop, Delta);
    itLn: Result := InterpolateLn(Start, Stop, Delta, DistortionDegree);
    itExp: Result := InterpolateExp(Start, Stop, Delta, DistortionDegree);
    //itHermit
    //itQuintic
    Else
    Begin
      Result := -1;
    End;
  End;
End;

Function InterpolateValueSafe(Const OriginalStart, OriginalStop, OriginalCurrent : Single; Const TargetStart, TargetStop : Single; Const DistortionDegree : Single; Const InterpolationType : TBZInterpolationType) : Single;
Var
  ChangeDelta: Single;
Begin
  If OriginalStop = OriginalStart Then
    Result := TargetStart
  Else
  Begin
    ChangeDelta := (OriginalCurrent - OriginalStart) / (OriginalStop - OriginalStart);
    Result := InterpolateValue(TargetStart, TargetStop, ChangeDelta, DistortionDegree, InterpolationType);
  End;
End;

Function InterpolateValueFast(Const OriginalStart, OriginalStop, OriginalCurrent : Single; Const TargetStart, TargetStop : Single; Const DistortionDegree : Single; Const InterpolationType : TBZInterpolationType) : Single;
Var
  ChangeDelta: Single;
Begin
  ChangeDelta := (OriginalCurrent - OriginalStart) / (OriginalStop - OriginalStart);
  Result := InterpolateValue(TargetStart, TargetStop, ChangeDelta, DistortionDegree, InterpolationType);
End;

function InterpolateBilinear(x,y : Single; nw, ne, sw, se : Single) : Single;
var
  cx, cy, m1, m2 : Single;
begin
  cx := 1.0 - x;
  cy := 1.0 - y;
	m1 := cx * nw + x * ne;
	m2 := cx * sw + x * se;
	Result := (cy * m1 + y * m2);
end;

{%endregion%}

{%region%=====[ fonctions émulées "HL/GL Shader script" ]======================}

function SmoothStep(Edge0,Edge1,x: Single): Single;Inline;
var
  t:single;
begin
  t:= Clamp((x-Edge0) / (Edge1 - Edge0),0.0,1.0);
  result := t * t * (3.0 - 2.0 * t); //t*t * ((t*2.0)*3.0);
end;

function SmoothStepQuintic(Edge0,Edge1,x: Single): Single;Inline;
var
  t:single;
begin
  t:= Clamp((x-Edge0) / (Edge1 - Edge0),0.0,1.0);
  result := t * t * t * (t * (t * 6 - 15) + 10);
end;


function Lerp(Edge0,Edge1,x: Single): Single; Inline;
begin
  // Fast method = Edge0 + x * (Edge1 - Edge0);
  result := Edge0 * (1 - x) + (Edge1 * x);  // S'assure que le resultat = Edge1 quand x = 1.
end;

function CubicLerp(Edge0, Edge1, Edge2, Edge3, x : Single) : Single;
var
  P, Q, R, S: Double;
begin
  P := (Edge3 - Edge2) - (Edge0 - Edge1);
  Q := (Edge0 - Edge1) - P;
  R := Edge2 - Edge0;
  S := Edge1;

  Result := P * x * x * x + Q * x * x + R * x + S;
end;

function CosineLerp(Edge0, Edge1, x : Single) : Single;
var
  f, ft: Double;
begin
  ft := x * cPi;
  f := (1.0 - cos(ft)) * 0.5;
  Result := Edge0 * (1 - f) + Edge1 * f;
end;

function Step(Edge,x: Single): Single;
begin
  if x<Edge then result := 0.0 else result := 1.0;
end;

Function Length1D(x : Single) : Single;
begin
  Result := Sqrt(x*x);
end;

function Mix(x, y, a : Double) : Double;
begin
  Result := x * (1 - a) + y * a;
end;

{%endregion%}

{%region%=====[ Fonctions utiles pour les animations ou interpolations ]=======}

// Converted from http://www.iquilezles.org/www/articles/functions/functions.htm

// Useful for animation

Function AlmostIdentity(x, m, n : single) : single;
var
  a,b,t : Single;
begin
    if (x>m) then
    begin
      result:= x;
      exit;
    end;
    a := 2.0*n - m;
    b := 2.0*m - 3.0*n;
    t := x/m;
    result := (a*t + b)*t*t + n;
end;

function Impulse(k,x : Single):Single; Inline;
var
  h:Single;
begin
  h := k*x;
  result := h * exp(1.0-h);
end;

Function CubicPulse(c, w, x : Single) : Single;
begin
  result := 0.0;
  x := abs(x - c);
  if(x>w ) then exit;
  x := x / w;
  Result := 1.0 - x * x * (3.0-2.0 * x);
end;

Function ExpStep(x, k, n : Single) : Single;
begin
  result := Exp(-k * pow(x,n));
end;

Function Parabola(x, k : Single) : Single;
begin
  result := pow( 4.0*x*(1.0-x), k );
end;

Function pcurve(x, a, b : Single) : Single;
var
  k : Single;
begin
    k := pow(a+b,a+b) / (pow(a,a) * pow(b,b));
    result := k * pow(x, a) * pow(1.0-x, b);
end;

Function pSinc(x, k : Single) : Single;
var
  a : Single;
begin
  a := cPI * (k*x-1.0);
  Result := sin(a)/a;
end;

Function pGain(x, k : Single) : Single;
var
  a : Single;
begin
   if x<0.5 then
   begin
     a := 0.5 * Pow(2.0*x,k);
     Result := a;
   end
   else
   begin
    a := 0.5 * Pow(2.0*(1.0-x),k);
    result :=1.0-a;
   end;
end;

function vGain(a, b : Single) : Single;
Var
  c : Single;
begin
  c := (1.0/b-2.0) * (1.0-2.0*a);
  if (a < 0.5) then Result := a/(c+1.0)
  else Result := (c-a)/(c-1.0);
end;

function bias(a, b : Single) : Single;
Begin
	Result := a /((1.0/b-2)*(1.0-a)+1);
End;

{%endregion%}

{%region%=====[ AUtres fonctions utiles ]======================================}

Function Val2Percent(min, max, val : Single) : Integer;
Var
  S: Single;
Begin
  If max = min Then
    S := 0
  Else If max < min Then
    S := 100 * ((val - max) / (min - max))
  Else
    S := 100 * ((val - min) / (max - min));
  If S < 0 Then
    S := 0;
  If S > 100 Then
    S := 100;
  Result := round(S);
End;

{ NOTE :
   1 cm =  37.79527559055 pixels
   1 pixel = 0.02645833333333416 cm
}
function PixelsToCentimeters(nbPixels : Integer) : Double;
begin
  Result := (nbPixels * 0.02645833333333416)
end;

function CentimetersToPixels(nbCm : Double) : Integer;
begin
  Result := Round((nbCm / 0.02645833333333416));
end;

{ NOTE :

  1 pouce (inch) = 2,54 cm
  1 cm = 0,3937 pouce (inch)
}
function InchToCentimeters(nbInch : Double) : Double;
begin
  Result := nbInch / 0.3937;
end;

function CentimetersToInch(nbCm : Double) : Double;
begin
  Result := nbCm * 0.3937;
end;

function PixelsToMillimeters(nbPixels : Integer) : Double;
begin
  Result := PixelsToCentimeters(nbPixels) * 10;
end;

function MillimetersToPixels(nbMm : Double) : Integer;
begin
  Result := CentimetersToPixels(nbMm / 10);
end;

function InchToPixels(nbInch : Double) : Integer;
begin
  Result := CentimetersToPixels(InchToCentimeters(NbInch));
end;

function PixelsToInch(nbPixels : Integer) : Double;
begin
  Result := CentimetersToInch(PixelsToCentimeters(nbPixels));
end;

function PixelsResolutionToCentimeters(nbPixels, DPI : Integer) : Double;
begin
  Result := nbPixels * 2.54 / DPI;
end;

function CentimetersToPixelsResolution(nbCm : Double; DPI : Integer) : Integer;
begin
  Result := Round(nbCm * DPI / 2.54);
end;

function CubeRoot(x : Single) : Single;
begin
  if x<0 then Result := -Math.power(-x, 1/3.0)
  else Result := Math.power(x, 1/3.0)
end;

// https://www.particleincell.com/2013/cubic-line-intersection/
function CubicRoot(a, b, c, d : Single) : TBZCubicRootSolution;
Const
  _Coef : Single = 1/3;
  _Coef2 : Single = 1.732050807568877; // Sqrt(3)
var
  tA, tB, tC, tQ, tQ3, tR, tD, s, t, v, v1 : Single;
  aR : TBZCubicRootSolution;
  i : Byte;
begin
  tA := b / a;
  tB := c / a;
  tC := d / a;
  tQ := (3.0 * tb - (tA *tA)) / 9.0;
  v := tA * tA * tA;
  tR := (9.0 * tA * tB - 27.0 * tC - (v + v)) / 54.0;
  tQ3 := (tQ * tQ * tQ);
  tD := tQ3 + (tR * tR);
  if (tD >= 0) then // complex or duplicate roots
  begin
    v := System.Sqrt(tD);
    v1 := tR + v;
    s := Sign(v1) * Math.Power(abs(v1), _Coef);
    v1 := tR - v;
    t := Sign(v1) * Math.Power(abs(v1), _Coef);
    v := -tA * _Coef;
    v1 := s + t;
    Result[0] := v + v1;
    v1 := v1 * 0.5;
    v1 := v - v1;
    Result[1] := v1;
    Result[2] := v1;

    // discard complex roots
    v1 := abs(_Coef2 * ((s - t) *0.5));
    if (v1 <> 0.0) then
    begin
      Result[1] := -1.0;
      Result[2] := -1.0;
    end;
  end
  else  // distinct real roots
  begin
     v := arcCos(tR / System.Sqrt(-tQ3));
     v1 := tA * _Coef;
     t := System.Sqrt(-tQ);
     t := t + t;

     Result[0] := t * System.cos(v * _Coef)- v1;
     Result[1] := t * System.cos((v + c2PI) * _Coef) - v1;
     Result[2] := t * System.cos((v + c4PI) * _Coef) - v1;
  end;

  // discard out of spec roots
  for i := 0 to 2 do
  begin
    if (Result[i] < 0.0) or (Result[i] > 1.0) then Result[i] := -1.0;
  end;
end;

function ComputeGaussian(x, Sigma : Single) : Single;
begin
  Result := System.exp(((-x * x) / ( 2 * Sigma * Sigma)));
end;

function ComputeGaussianCenter(x, Sigma : Single) : Single;
Var
  u : Single;
begin
  u:= Sqr(x / Sigma);
  Result := System.exp(-u * 0.5)
end;

function ComputeGaussianDerivative(x, Sigma : Single) : Single;
begin
  Result := -x /(Sigma * Sigma) * ComputeGaussian(x,Sigma);
end;

function ComputeMeanGaussian(x, Sigma : Single) : Single;
Var
  t : Single;
begin
	t := (ComputeGaussian(x,Sigma) + ComputeGaussian((x + 0.5),Sigma) + ComputeGaussian((x - 0.5),Sigma)) * cInvThree;
	Result := t / (c2PI * Sigma * Sigma);
end;

function ComputeGaussianDistribution(x, weight, sigma : Single) : Single;
Var
  d, n : Single;
begin
  d := x - weight;
  n := 1.0 / (System.sqrt(cPI) * sigma);
  Result := System.exp(-d * d / ( 2 * sigma * sigma)) * n;
end;

function ComputeLoG(x, Sigma : Single) : Single;
var
  g : Single;
begin
	g := ComputeGaussian(x,Sigma);
	Result := (x * x - 2 * Sigma * Sigma) / (Math.power(Sigma,4)) * g;
end;

function ComputeLoGCenter(x, Sigma : Single) : Single;
var
  g : Single;
begin
	g := ComputeGaussianCenter(x,Sigma);
	Result := (x * x - 2 * Sigma * Sigma) / (Math.power(Sigma,4)) * g;
end;

function ComputeEuler(Weight : Single) : Single;
begin
  Result := 1.0 / (2 * cPI * Math.Power(Weight, 2.0));
end;

function Factorial(x: Int64): Int64;
Var
 i, r : integer;
begin
 r:= 1;
 for i:= 2 to x do
 begin
   r := r * i;
 end;
 result := r;
end;

function ComputeBerstein(x, n: integer; u: Single): Single;
var
 f : Single;
begin
 f := 1.0 - u;
 result := factorial(n) / (factorial(x) * factorial(n-x)) * power(u, x) * power(f, n-x);
end;

//https://stackoverflow.com/questions/5731863/mapping-a-numeric-range-onto-another
function RangeMap(value, InMin, InMax, OutMin, OutMax : Single) : Single;
begin
  Result := OutMin + (OutMax - OutMin) * ((Value - InMin) / (InMax - InMin));
end;

function RangeMap(value, InMin, InMax, OutMin, OutMax : Integer) : integer;
begin
  Result := OutMin + (OutMax - OutMin) * ((Value - InMin) div (InMax - InMin));
end;

//function
//function RandomRange(Mini, Maxi : Integer) : Integer;
//begin
//  result := Random(Maxi - Mini + 1) + Mini;
//end;

{%endregion%}

//==============================================================================

initialization
 {$IFDEF USE_FASTMATH}
  _InitSinCosTanLUT;
 {$ENDIF}

finalization
 {$IFDEF USE_FASTMATH}
 _DoneSinCosTanLUT;
 {$ENDIF}

 //==============================================================================
End.
