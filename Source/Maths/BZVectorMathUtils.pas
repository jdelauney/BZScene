(*
  @abstract(Contient des fonctions utilises dans la manipulation des vecteurs)

  -------------------------------------------------------------------------------------------------------------

  @created(2017-11-25)
  @author(J.Delauney (BeanzMaster))
  @author(Peter Dyson (Dicepd))

  Historique : @br
  @unorderedList(
    @item(Last Update : 13/03/2018  )
    @item(12/03/2018 : Creation  )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) :
	
  -------------------------------------------------------------------------------------------------------------

  @bold(Dependance) : BZMath, BZVectorMath, BZVectorMathEx

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)@br
    @unorderedList(
      @item(FPC/Lazarus)
      @item(GLScene)
      @item(All authors of papers and web links)
    )

  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZVectorMathUtils;

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
//------------------------------------------------------------------------------

//==============================================================================

interface

uses
  Classes, SysUtils, BZMath, BZVectorMath;

{%region%----[ Inline Vector Helpers functions ]--------------------------------}

{ Creation d'un point 3D affine }
function AffineVectorMake(const x, y, z : Single) : TBZAffineVector;overload;
{ Creation d'un point 3D affine depuis un point homogène }
function AffineVectorMake(const v : TBZVector) : TBZAffineVector;overload;
{ Creation d'un point 2D en valeurs flottantes }
function vec2(vx,vy:single):TBZVector2f;
{ Creation d'un point homogène en valeurs flottantes }
function vec4(vx,vy,vz,vw:single):TBZVector4f;overload;
{ Creation d'un point homogène en fonction d'une valeur de type single }
function vec4(vx:single):TBZVector4f; overload;
{ Creation d'un point homogène en fonction des paramètres (W = 0.0) }
function PointVec4(vx,vy,vz:single):TBZVector4f;overload;
{ Creation d'un point affine dans un vecteur homogène en fonction d'une valeur de type single (W = 1.0) }
function PointVec4(vx:single):TBZVector4f;overload;

//function PointVec4(Const anAffineVector: TBZVector3f):TBZVector4f; overload;

{ Creation d'un point 2D en valeur entière }
function vec2i(vx,vy:Integer) : TBZVector2i;

{%endregion%}

{%region%-----[ Inline Algebra and Trigonometric functions for vectors ]--------}

//--- For TBZVector2f ---------------------------------------------------------

{ Retourne les valeur tronquées d'un vecteur 2D en virgule flottante, dans un vecteur de type TBZVector2i }
function Trunc(Constref v:TBZVector2f):TBZVector2i; overload;
{ Retourne les valeur arrondies d'un vecteur 2D en virgule flottante dans un vecteur de type TBZVector2i }
function Round(Constref v:TBZVector2f):TBZVector2i; overload;
{ Retourne les valeur arrondies d'un vecteur 2D en virgule flottante, tendant vers l'infini négatif dans un vecteur de type TBZVector2i }
function Floor(Constref v:TBZVector2f):TBZVector2i; overload;

{ Retourne les valeur arrondies d'un vecteur 2D en virgule flottante, tendant vers l'infini positif dans un vecteur de type TBZVector2i }
//function Ceil(Constref v:TBZVector2f):TBZVector2i; overload;

{ Retourne la partie fractionnaire de chaque valeur contenues dans le vecteur 2D en virgule flottante }
function Fract(Constref v:TBZVector2f):TBZVector2f; overload;

//function Length(v:TBZVector2f):Single;overload;
//function Distance(v1,v2:TBZVector2f):Single;
//function Normalize(v:TBZVector2f):TBZVector2f;

//function fma(v,m,a:TBZVector2f): TBZVector2f; overload;
//function fma(v:TBZVector2f; m,a:Single): TBZVector2f; overload;
//function fma(v,m:TBZVector2f;a:Single): TBZVector2f; overload;
//function fma(v:TBZVector2f;m:Single; a:TBZvector2f): TBZVector2f; overload;

//function faceforward
//function SmoothStep;
//function Step;
//function Saturate;

//--- Trigonometrics functions

{ Retourne la racine carré d'un point 2D en virgule flottante }
function Sqrt(Constref v:TBZVector2f):TBZVector2f; overload;
{ Retourne la racine carré inverse d'un point 2D en virgule flottante }
function InvSqrt(Constref v:TBZVector2f):TBZVector2f; overload;
{ Retourne le sinus de chaque valeur d'un point 2D en virgule flottante }
function Sin(v:TBZVector2f):TBZVector2f; overload;
{ Retourne le cosinus de chaque valeur d'un point 2D en virgule flottante }
function Cos(v:TBZVector2f):TBZVector2f; overload;
{ Retourne le sinus et le cosinus d'une valeur de type Single dans un vecteur 2D en virgule flottante }
function SinCos(x:Single):TBZvector2f; overload;
{ Retourne le sinus et le cosinus respectif des valeurs contenues dans vecteur 2D en virgule flottante }
function SinCos(v:TBZVector2f):TBZvector2f; overload;

// function Exp;
// function Ln;

//--- For TBZVector4f ---------------------------------------------------------

{ Retourne la partie fractionnaire de chaque valeur contenues dans le vecteur 4D en virgule flottante }
function Fract(Constref v:TBZVector4f):TBZVector4f; overload;

//function Trunc(v:TBZVector4f):TBZVector4i; overload;
//function Round(v:TBZVector4f):TBZVector4i; overload;
//function Floor(v:TBZVector4f):TBZVector4i; overload;
//function Fract(v:TBZVector4f):TBZVector4i; overload;

//function Sqrt(v:TBZVector2f):TBZVector4f; overload;
//function InvSqrt(v:TBZVector2f):TBZVector4f; overload;

//function Sin(v:TBZVector4f):TBZVector4f; overload;
//function Cos(v:TBZVector4f):TBZVector4f; overload;

// retun SinCos, SinCos (x and z = sin, y and w = cos)
//function SinCos(v:TBZVector4f):TBZVector4f; overload;


{%endregion%}

{%region%-----[ Misc Inline utilities functions for vectors ]-------------------}

{ Retourne @True si deux vecteur de type TBZVector sont colinéaire }
function VectorIsColinear(constref v1, v2: TBZVector) : Boolean;

//function PlaneContains(const Location, Normal: TBZVector; const TestBSphere: TBZBoundingSphere): TBZSpaceContains;

{ Calculates the barycentric coordinates for the point p on the triangle
   defined by the vertices v1, v2 and v3. That is, solves
     p = u * v1 + v * v2 + (1-u-v) * v3
   for u,v.
   Returns true if the point is inside the triangle, false otherwise.
   NOTE: This function assumes that the point lies on the plane defined by the triangle.
   If this is not the case, the function will not work correctly! }
//function BarycentricCoordinates(const v1, v2, v3, p: TBZAffineVector; var u, v: single): boolean;

{ Computes the triangle's area. }
//function TriangleArea(const p1, p2, p3 : TBZAffineVector) : Single; overload;

{ Computes the polygons's area.
   Points must be coplanar. Polygon needs not be convex. }
//function PolygonArea(const p : PAffineVectorArray; nSides : Integer) : Single; overload;

{ Computes a 2D triangle's signed area.
   Only X and Y coordinates are used, Z is ignored. }
//function TriangleSignedArea(const p1, p2, p3 : TBZAffineVector) : Single; overload;

{ Computes a 2D polygon's signed area.
   Only X and Y coordinates are used, Z is ignored. Polygon needs not be convex. }
//function PolygonSignedArea(const p : PAffineVectorArray; nSides : Integer) : Single; overload;

{ Generates a random point on the unit sphere.
   Point repartition is correctly isotropic with no privilegied direction. }
//function RandomPointOnSphere:TBZVector;

{%endregion%}

implementation

//-----[ INCLUDE IMPLEMENTATION ]-----------------------------------------------

{$ifdef USE_ASM}
  {$ifdef CPU64}
    {$ifdef UNIX}
      {$IFDEF USE_ASM_AVX}
	 {$I vectormath_utils_native_imp.inc}
         {$I vectormath_utils_avx_imp.inc}	
      {$ELSE}         
         {$I vectormath_utils_native_imp.inc}
         {$I vectormath_utils_sse_imp.inc}		 
      {$ENDIF}
    {$else} // win64
      {$IFDEF USE_ASM_AVX}
         {$I vectormath_utils_native_imp.inc}
         {$I vectormath_utils_avx_imp.inc}
       {$ELSE}
	 {$I vectormath_utils_native_imp.inc}
         {$I vectormath_utils_sse_imp.inc}
       {$ENDIF}
    {$endif}  //unix
  {$else} // CPU32
     {$IFDEF USE_ASM_AVX}
	{$I vectormath_utils_native_imp.inc}
        {$I vectormath_utils_avx_imp.inc}
     {$ELSE}
        {$I vectormath_utils_native_imp.inc}
        {$I vectormath_utils_sse_imp.inc}
     {$ENDIF}
  {$endif}

{$else}  // pascal
  {$I vectormath_utils_native_imp.inc}
{$endif}



end.

