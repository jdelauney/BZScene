(*
  Contient des fonctions mathématiques, basiques et trigonométriques optimisées.

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
       @item(http://lab.polygonal.de/2007/07/18/fast-and-accurate-sinecosine-approximation/)
       @item(https://en.wikipedia.org/wiki/Fast_inverse_square_root)
       @item(https://en.wikipedia.org/wiki/Taylor_series)
       @item(https://stackoverflow.com/questions/18662261/fastest-implementation-of-sine-cosine-and-square-root-in-c-doesnt-need-to-b)
       @item(https://en.wikipedia.org/wiki/Newton%27s_method)
       @item(http://allenchou.net/2014/02/game-math-faster-sine-cosine-with-polynomial-curves/)
       @item(http://www.ue.eti.pg.gda.pl/~wrona/lab_dsp/cw04/fun_aprox.pdf)
       @item(http://www.netlib.org/cephes/)
       @MatLab
       @WolFram
     )

  -------------------------------------------------------------------------------------------------------------

  @bold(Dependances) : Aucune

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)@br
    @unorderedList(
      @item(J.Delauney (BeanzMaster))
    )

  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZFastMath;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils;

//==============================================================================

{ Retourne le Cosinus de x. Précision 4E-8 }
function FastCosLUT(x:Single):Single;
{ Retourne le Sinus de x. Précision 4E-8 }
function FastSinLUT(x:Single):Single;
{ Retourne la tangente de x. Précision 4E-8 }
function FastTanLUT(x:Single):Single;
{ Retourne le Sinus Invers de X }
Function FastSinc(x: Single): Single;
{ Retourne la tangente Inverse de X }
Function FastTan(x:Single):Single;
{ Retourne l'arc tangente de x }
Function FastArcTan(x:Single):Single;
{ Retourne l'arc Tangente de y par x. Approximation, précis à 0.07 rads. }
Function FastArcTan2(y, x: Single): Single;
{ Retourne l'arc Sinus  de X }
Function FastArcSine(Const x: Single): Single;
{ Retourne l'arc Cosinus  de X }
Function FastArcCosine(const X: Single): Single;
{ Retourne la racine carré inverse de Value }
Function FastInvSqrt(Const Value: Single): Single;
{ Retourne la racine carré de Value }
Function FastSqrt(Const Value: Single): Single;
//Function FastSqrt(Const Value: Double): Double;
{ Retourne le logarithme naturel de X }
Function FastLn(Const X: Single): Single;
{ Retourne l'exponentiel de X }
Function FastExp(x:Single):Single;
{ Retourne le logarithme naturel de X }
Function FastLog(x:Single):Single;
{ Retourne le logarithme naturel de X base 2 }
Function FastLog2(X: Single): Single;
{ Retourne le logarithme naturel de X base 10 }
Function FastLog10(X: Single): Single;
{ Retourne le logarithme exponentiel de X par N }
Function FastLDExp(x: Single; N: Integer): Single;
{ Retourne le logarithme exponentiel de X par N }
Function FastLNXP1(Const x: Single): Single;
{ Retourne la valeur absolue de X }
Function FastAbs(f:single):single;
{ Retourne le négatif de X }
Function FastNeg(f:single):single;
{ Retourne le signe de f. @br
  @unorderedlist(
    @item(0 si null)
    @item(+1 si positif)
    @item(-1 si négatif)) }
Function FastSign(f:single):longint;

//function FastPower(i:single;n:integer):single;

{ Retourne la puissance de base par Power }
Function FastPower(Base: Single; Power: Integer): Single;

{ Initalisation pour les tables de Sinus et Cosinus précalculés }
procedure _InitSinCosTanLUT;
{ Libération des tables de Sinus et Cosinus précalculés. Initialisées Par _InitSinCosLUT }
procedure _DoneSinCosTanLUT;

//==============================================================================

implementation

Uses Math;
//==============================================================================

// On defini quelques valeurs internes en vue d'optimiser les calculs pour une utilisation strictement en interne
Const
  _cInfinity = 1e1000;
  _cPI: double = 3.1415926535897932384626433832795; //3.141592654;
  _c2PI: Single = 6.283185307;
  _cPIdiv4: Single = 0.785398163;
  _c3PIdiv4: Single = 2.35619449;
  _cZero: Single = 0.0;
  _cOne: Single = 1.0;
  _cEpsilon: Single = 1e-10;
  _cEulerNumber = 2.71828182846;

//-----[ INCLUDE IMPLEMENTATION ]-----------------------------------------------

{$ifdef USE_ASM}
  {$ifdef CPU64}
    {$ifdef UNIX}
      {$ifdef USE_ASM_AVX}
        {$I fastmath_native_imp.inc}
        {$I fastmath_unix64_avx_imp.inc}
      {$else}
        {$I fastmath_native_imp.inc}
        {$I fastmath_unix64_sse_imp.inc}
      {$endif}
    {$else} // windows
      {$ifdef USE_ASM_AVX}
        {$I fastmath_native_imp.inc}
        {$I fastmath_unix64_avx_imp.inc}
      {$else}
        {$I fastmath_native_imp.inc}
        {$I fastmath_win64_sse_imp.inc}
      {$endif}
    {$endif}
  {$else} //cpu32
    {$ifdef USE_ASM_AVX}
      {$I fastmath_native_imp.inc}
      {$I fastmath_intel32_avx_imp.inc}
    {$else}
      {$I fastmath_native_imp.inc}
      {$I fastmath_intel32_sse_imp.inc}
    {$endif}
  {$endif}
{$else}
  {$I fastmath_native_imp.inc}
{$endif}

end.

