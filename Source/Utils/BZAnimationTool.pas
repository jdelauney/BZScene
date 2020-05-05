(*

  @abstract(Contient une classe qui permet de faire des animations ("Easing" / "Tweening") entre deux valeurs facilement. @br
  Pour animer les valeurs, la classe TBZAnimationTool se sert de fonctions paramétriques non-linéaire 1D)

  -------------------------------------------------------------------------------------------------------------

  @created(2017-09-09)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(09/09/2017 : Creation  )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes :) Quelques liens intéressants sur le sujet qui m'ont servis de référence : @br
  @unorderedList(
    @item(https://en.wikipedia.org/wiki/Parametric_equation)
    @item(http://robertpenner.com/easing/)
    @item(https://easings.net)
    @item(https://www.febucci.com/2018/08/easing-functions/)
    @item(http://gizma.com/easing/)
    @item(https://marionettestudio.com/easing-functions-in-the-animation-process/)
    @item(https://cubic-bezier.com/#.17,.67,.83,.67)
    @item(http://www.timotheegroleau.com/Flash/experiments/easing_function_generator.htm)
    @item(https://www.youtube.com/watch?v=mr5xkf6zSzk)
    @item(https://joshondesign.com/2013/03/01/improvedEasingEquations)
    @item(https://rechneronline.de/function-graphs/)
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Dépendances) : BZClasses, BZMath, BZInterpolationFilters;

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)
   @unorderedList(
     @item (Tous les lien ci-dessus)
   )

  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MP / GPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZAnimationTool;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils, Math,
  BZClasses, BZMath, BZVectorMath, BZInterpolationFilters;

type
  { Definition d'une function paramétrique 1D }
  TBZParametric1DFunc = function(t:Single) : Single;

  { Enumération des mode d'animation disponibles }
  TBZAnimationMode = (amIn, amInInverted,
                      amOut, amOutInverted,
                      amInOut, amOutIn,
                      amSpike, amSpikeInverted,
                      amArch, amArchInverted,
                      amSpikeAndArch, amSpikeAndArchInverted,
                      amArchAndSpike,
                      aemCustom);

  { Enumération des type d'animation disponibles }
  TBZAnimationType = (atLinear,
                      atQuadratic, atCubic, atQuartic, atQuintic, atExponantial,
                      atSine, atCircle,
                      atElastic, atBack, atBounce,
                      atBell,
                      atPower,
                      atStep,
                      atJitterWave,
                      atSwing,
                      atTardis,
                      atDamping,
                      atWave,
                      atCubicBezier,
                      atBezier7,
                      atCubicSpline,
                      atCustom);

  TBZAnimationClampMode = (acmNone, acmTop, acmBottom, acmBoth);

  //TBZAnimationExtraParams = Array of Single; // Array[0..15] of Single; // Maximum 16 valeurs de paramètres supplémentaires
Type

  { TBZAnimationTool }

  TBZAnimationTool = Class(TBZThreadableObject)
  private
    FDuration : Single;
    //FCurrentStep : Single;
    FStartValue : Single;
    FEndValue : Single;
    //FRangeValue : Single;

    FAnimationMode : TBZAnimationMode;
    FAnimationType : TBZAnimationType;
    FCustomEaseFunc : TBZParametric1DFunc;

    FClampMode : TBZAnimationClampMode;

    FInterpolate : Boolean;
    FInterpolationFilter : TBZInterpolationFilterMethod;

    FDecay : Single;
    FMomentum : Single;

  protected
    FExtraParamsSingle1 : Single;

    function GetEaseFunc(EaseType :TBZAnimationType) : TBZParametric1DFunc;
  public
    Constructor Create; override;
    Destructor Destroy; override;

    { Retourne le facteur compris entre 0.0 et 1.0 ,calculé de l'animation à un moment T compris entre 0.0 et 1.0 en fonction du mode et du type }
    function Ease(EaseMode : TBZAnimationMode; EaseType : TBZAnimationType; t : Single) : Single; overload;
    { Retourne le facteur compris entre 0.0 et 1.0 ,calculé de l'animation à un moment T compris entre 0.0 et 1.0 en fonction du mode et du type. @br
      Avec en plus le passage d'un paramètre supplémentaire pour les types d'animation qui le demande. @br
      cf : atPower, atStep, atSwing }
    function Ease(EaseMode : TBZAnimationMode; EaseType : TBZAnimationType; t : Single; Param1 : Single) : Single; overload;
    //function Ease(EaseMode : TBZAnimationMode; EaseFunc : TBZParametric1DFunc; t : Single) : Single; overload;

    { Retourne le facteur d'animation compris entre "rangeStart" et "rangeEnd" à un temps donné "Current" de durée "Duration" }
    function Tween(Current, Duration, rangeStart, rangeEnd : Single; EaseMode : TBZAnimationMode; EaseType : TBZAnimationType) : Single; overload;
    function Tween(Current, Duration, rangeStart, rangeEnd : Single; EaseMode : TBZAnimationMode; EaseType : TBZAnimationType; Param1 : Single) : Single; overload;
    //function Tween(Current, Duration, rangeStart, rangeEnd : Single; EaseMode : TBZAnimationMode; EaseFunc : TBZParametric1DFunc; ) : Single;

    { Retourne le facteur de type "Single" d'animation en fonction des propriété }
    function Animate(CurrentStep : Single; Const Reverse : Boolean = False) : Single; overload;
    function Animate(CurrentStep : Single; Param1 : Single; Const Reverse : Boolean = False) : Single; overload;
    function Animate(CurrentStep : Single; Param1, Param2 : Single; Const Reverse : Boolean = False) : Single; overload;
    function Animate(CurrentStep : Single; Param1, Param2 : TBZFloatPoint; Const Reverse : Boolean = False) : Single; overload;

    { Retourne le facteur de type "Integer" d'animation en fonction des propriété }
    function AnimateInt(CurrentStep : Single; Const Reverse : Boolean = False) : Integer; overload;
    function AnimateInt(CurrentStep : Single; Param1 : Single; Const Reverse : Boolean = False) : Integer; overload;


    function AnimateRemap(CurrentStep : Single; inStart, inEnd: Single) : Single;

    { Durée total de l'animation en frame, seconde ou tout autre unité de mesure }
    property Duration : Single read FDuration write FDuration;
    { Valeur de début }
    property StartValue : Single read FStartValue write FStartValue;
    { Valeur de fin }
    property EndValue : Single read FEndValue write FEndValue;

    { Mode de l'animation }
    property AnimationMode : TBZAnimationMode read FAnimationMode write FAnimationMode;
    { Type de l'animation }
    property AnimationType : TBZAnimationType read FAnimationType write FAnimationType;

    { Mode de restriction des valeurs }
    property ClampMode : TBZAnimationClampMode read FClampMode write FClampMode;

    { Active l'interpolation des valeurs }
    property Interpolate : Boolean read FInterpolate write FInterpolate;
    { Filtre d'interpolation à appliquer }
    property InterpolationFilter : TBZInterpolationFilterMethod read FInterpolationFilter write FInterpolationFilter;

    //property Decay : Single read FDecay write FDecay;
    //property Momentum : Single read FMomentum write FMomentum;

    { Fonction paramétrique 1D personnalisée pour le calcul des valeurs de l'animation }
    property CustomEaseFunc : TBZParametric1DFunc read FCustomEaseFunc write FCustomEaseFunc;
  end;

{ Animation linéaire }
function easeLinear(t : Single) : Single;
{ Animation carré }
function easeSquare(t : Single) : Single;
{ Animation cubique }
function easeCubic(t : Single) : Single;
{ Animation quartique }
function easeQuartic(t : Single) : Single;
{ Animation quintique }
function easeQuintic(t : Single) : Single;
{ Animation exponentielle }
function easeExponantial(t : Single) : Single;
{ Animation Sinus }
function easeSine(t : Single) : Single;
{ Animation cercle }
function easeCircle(t : Single) : Single;
{ Animation elastique }
function easeElastic(t : Single) : Single;
{ Animation retour }
function easeBack(t : Single) : Single;
{ Animation rebond }
function easeBounce(t : Single) : Single;
{ Animation courbe de type Bell }
function easeBell(t : Single) : Single;
{ Animation "Jitter" }
function easeJitterWave(t : Single) : Single;
{ Animation "Dance" }
function easeSwing(t : Single) : Single;
{ Animation sinuosidale }
function EaseWave(t : Single) : Single; Inline;
{ Animation puissance }
function EasePower(t, Exponent : Single) : Single;
{ Animation étapes }
function EaseStep(t, Steps : Single) : Single;
{ Animation tardis }
function EaseTardis(t : Single) : Single;
{ Animation amortissement }
function EaseDamping(t : Single) : Single; Inline;
//function easeWave(t : Single; Amplitude, Periode : Single) : TBZParametric1DFunc;

{ Inversion }
function easeFlip(t : Single) : Single;
{ Redimensionnement }
function easeScale(EaseFunc : TBZParametric1DFunc;t : Single) : Single;
{ Redimensionnement inverse }
function easeReverseScale(EaseFunc : TBZParametric1DFunc;t : Single) : Single;
{ Mixage }
function easeMix(EaseStartFunc, EaseStopFunc : TBZParametric1DFunc; BlendFactor, t : Single) : Single;
{ Fondu }
function easeCrossFade(EaseStartFunc, EaseStopFunc : TBZParametric1DFunc; t : Single) : Single;
{ Arche }
function easeArch(t : Single) : Single; inline;
{ Restraint les valeurs du bas }
function easeClampBottom(t : Single) : Single; inline;
{ Restraint les valeurs du haut }
function easeClampTop(t : Single) : Single; inline;
{ Restraint les valeurs du bas et du haut }
function easeClampBottomTop(t : Single) : Single; inline;

{ Calcul les valeurs d'interpolation en fonction des paramètres}
function Tweener(Start, Target: Single; Time, Duration: Single; EaseMode : TBZAnimationMode; EaseType: TBZAnimationType): Single; //overload;

implementation

Uses
  BZSceneStrConsts,
  Dialogs;

Var
  vCustomEasePowerExponent : Single;
  vCustomEaseStep : Single;


function easeLinear(t : Single) : Single; inline;
begin
  result := t;
end;

function easeSquare(t : Single) : Single; inline;
begin
  result := t * t;
end;

function easeCubic(t : Single) : Single; inline;
begin
  result := t * t * t;
end;

function easeQuartic(t : Single) : Single; inline;
begin
  result := t * t * t * t;
end;

function easeQuintic(t : Single) : Single; inline;
begin
  result := t * t * t * t * t;
end;

function easeExponantial(t : Single) : Single; inline;
begin
  if t = 0.0 then result := 0.0
  else result := Math.Power(2, 10 * (t - 1));
end;

function easeSine(t : Single) : Single; inline;
begin
  Result := Sin((t - 1.0) * cPiDiv2) + 1.0;
  //Result := 1 - cos((t * cPI) * 0.5);
  //Result := 1 - cos(t * cPiDiv2);
end;

function easeCircle(t : Single) : Single; inline;
begin
  Result := 1.0 - Sqrt(1.0 - (t * t));
end;

{ Cette fonction peut-être personnalisée a souhait, elle est la a titre empirique

Const
  amplitude = 1.0;
  periode = 0.3;

  periodeFactor := Periode / c2Pi;
  Func = ArcSin, (Sin, Cos etc....)
  if Amplitude <= 1.0 then f = Func(1 / Amplitude) * periodeFactor else  f = arcsin(1 * Amplitude) * periodeFactor;

  // Autre exemple depuis Easing.net
  	if t = 0 then Result := 0
    else if t = 1 then Result := 1
    else result := -Math.Power(2, 10 * t - 10) * sin((t * 10 - 10.75) * (c2PI / 3));
}
function easeElastic(t : Single) : Single; inline;
begin
  Result := Sin(13.0 * cPiDiv2 * t) * Math.Power(2.0, 10.0 * (t - 1.0));
end;

{ Cette fonction peut-être personnalisée a souhait, elle est la a titre empirique }
function easeBack(t : Single) : Single; inline;
begin
  //Const s = 1.70158;
  // s = CustomFunc(t)
  //Result := t * t * ((s + 1) * t - s);

  //Result := EaseFunc(t) - t * Sin(t * cPI);
  // Cubic
  Result := t * t * t - t * Sin(t * cPI);

end;

{ Cette fonction peut-être personnalisée a souhait, elle est la a titre empirique }
function easeBounce(t : Single) : Single; inline;

(* Extrait de Kute.js : Licensed under MIT-License
  if ( t < ( 1 / 2.75 ) ) { return 7.5625 * t * t; }
   else if ( t < ( 2 / 2.75 ) ) { return 7.5625 * ( t -= ( 1.5 / 2.75 ) ) * t + 0.75; }
   else if ( t < ( 2.5 / 2.75 ) ) { return 7.5625 * ( t -= ( 2.25 / 2.75 ) ) * t + 0.9375; }
   else {return 7.5625 * ( t -= ( 2.625 / 2.75 ) ) * t + 0.984375;

   from David Hüttig. Licensed unter MPL v1.1.
// http://sourceforge.net/projects/scriptengine2/
   function BounceEase(Progress:Double):Double; export;
   const
     Base:Double = 7.5625;
   begin
     if Progress < (1 / 2.75) then
       Result:=Base * Progress * Progress
     else if Progress < (2 / 2.75) then
     begin
       Progress:=Progress - (1.5 / 2.75);
       Result:=(Base * Progress) * Progress + 0.75;
     end
     else if Progress < (2.5 / 2.75) then
     begin
       Progress:=Progress - (2.25 / 2.75);
       Result:=(Base * Progress) * Progress + 0.9375;
     end
     else
     begin
       Progress:=Progress - (2.625/2.75);
       Result:=(7.5625 * Progress) * Progress + 0.984375;
     end;
   end;

   // Easing.net --> GPL
   function bounceOut(x: number) {
   	const n1 = 7.5625;
   	const d1 = 2.75;

   	if (x < 1 / d1) {
   		return n1 * x * x;
   	} else if (x < 2 / d1) {
   		return n1 * (x -= 1.5 / d1) * x + 0.75;
   	} else if (x < 2.5 / d1) {
   		return n1 * (x -= 2.25 / d1) * x + 0.9375;
   	} else {
   		return n1 * (x -= 2.625 / d1) * x + 0.984375;
   	}
   }

NDMM :   du coup je fais quoi avec TOUTES ces licences complètement différentes. C'est la poule avant l'oeuf ou l'oeuf avant la poule ? PTDR !!!
*ndmm : note de moi même :D)
*)
Const
  cFactorA = 0.363636;
  cFactorB = 0.727272;
  cFactorC = 3.4;
  cFactorD = 0.9;
  cFactorE = 8.898060;
  cFactorF = 10.72;
begin
  if (t < cFactorA) then
  begin
    Result := (121 * t * t) / 16.0;
  end
  else if (t < cFactorB) then
  begin
  	Result := (363/40.0 * t * t) - (99.0 / 10.0 * t) + cFactorC;
  end
  else if (t < cFactorD) then
  begin
  	Result := (4356/361.0 * t * t) - (35442/1805.0 * t) + cFactorE;
  end
  else
  begin
    Result :=  (54/5.0 * t * t) - (513/25.0 * t) + cFactorF;
  end;
end;

function easeBell(t : Single) : Single;
begin
  Result := (1.0 - EaseCubic(1.0 - t)) * EaseCubic(t);
end;

function EasePower(t, Exponent : Single) : Single;
Var
  f : Single;
  u : Integer;
begin
  Result := 0;
  if  Exponent = 0 then Exit;


  u := Trunc(Exponent);
  f := Exponent - u; // Frac(Exponent);
  //ShowMessage('Int = ' + u.ToString + 'Frac = ' + f.ToString);
  if (f = 0.0)  then
  begin
    if (u<=5) then
    begin
      Case u of
        1 : Result := EaseLinear(t);
        2 : Result := EaseSquare(t);
        3 : Result := EaseCubic(t);
        4 : Result := EaseQuartic(t);
        5 : Result := EaseQuintic(t);
      end;
    end
    else
    begin
      Result := Math.power(t, Exponent);
    end;
  end
  else
  begin
    if (u<5) then
    begin
      case u of
        1 : Result := easeMix(@easeLinear, @easeSquare, f,t);
        2 : Result := easeMix(@easeSquare, @easeCubic, f,t);
        3 : Result := easeMix(@easeCubic, @easeQuartic, f, t);
        4 : Result := easeMix(@easeQuartic, @easeQuintic, f, t);
      end;
    end
    else
    begin
      Result := Math.power(t, Exponent);
    end;
  end;
end;

function EasePowerProcess(t : Single) : Single;
begin
  Result := 0;
  if  vCustomEasePowerExponent = 0 then Exit;
  Result := EasePower(t, vCustomEasePowerExponent);
end;

function easePowerGetFunc(Exponent : Single) : TBZParametric1DFunc;
begin
  vCustomEasePowerExponent := Exponent;
  Result := @EasePowerProcess;
end;

// Animation basique par étapes :  https://developer.mozilla.org/fr/docs/Web/CSS/transition-timing-function
function EaseStep(t, Steps : Single) : Single;
begin
  Result := Round(t * Steps) * (1.0 / Steps);
end;

function EaseStepProcess(t : Single) : Single;
begin
  Result := 0;
  if  vCustomEaseStep = 0 then vCustomEaseStep := 10;
  Result := EaseStep(t, vCustomEaseStep);
end;

function easeStepGetFunc(Steps : Single) : TBZParametric1DFunc;
begin
  vCustomEaseStep := Steps;
  Result := @EaseStepProcess;
end;

//function easeSwing(t : Single; Swing : Single) : Single; inline;
function easeSwing(t : Single) : Single; inline;
Const
  Swings = 10;
begin
  Result:= t + (Sin(t * Swings * cPi) * (1 / Swings));
end;

// Adpaté de http://sourceforge.net/projects/scriptengine2/
//function easeJitterWave(t : Single; Amplitude, ScaleFactor : Single) : Single; inline;
function easeJitterWave(t : Single) : Single; inline;
begin
  // 50 = Amplitude
  // 0.03 = ScaleFactor
  Result:= t + (Sin(t * 3 * cPi) * 0.1);
end;

function EaseWave(t : Single) : Single; Inline;
Const
  Amplitude = 0.5;
  VerticalShift = 0.5;
  Frequency = 4.0;
  PhaseShift = 0.2;
begin
  //Result := Amplitude * t * (Sin(Frequency * t * c2PI) - 1.0) * 0.5 + 0.5;
  //Result :=  Amplitude * (Sin( (c2PI/Periode) * (t + PhaseShift) )) + VerticalShift;// + 0.5;
  Result :=  Amplitude * Sin( c2PI * Frequency * t + PhaseShift)  + VerticalShift;// + 0.5;
end;

function easeTardis(t : Single) : Single; Inline;
Const
  Steps = 9;
Var
  v, CurStep, StepDiff, NewPerc, newEasing : Single;
Begin
  curStep := (t * steps);
  stepDiff := curStep / steps;
  newPerc := (t - stepDiff) * steps;
  // InOut
  if (newPerc < 0.5) then
  begin
    v := newPerc + newPerc;
    newEasing := easeSine(v) * 0.5;
  end
  else
  begin
    v := 1.0 - newPerc;
    v := v + v; //x * 2.0
    newEasing := 1.0 - easeSine(v) * 0.5;
  end;

  if ((Round(curStep) Mod 2) = 0) then newEasing := 1.0 - newEasing;
  Result :=  newEasing * 0.5 + t * 0.5;
end;

// Extrait de GLScene
function EaseDamping(t : Single) : Single; Inline;
Const
  //damping = constant + linear * Speed + quadratic * Speed^2
  //accel = damping / Mass
  MassFactor = 2.0;
  ConstantFactor = 0.0; // 1.0; //1.0;     // Utilisé pour une friction solide (arrêtera brusquement un objet après. En diminuant sa vitesse.
  LinearFactor = 0.0; //1.0;               // Amortissement linéaire par friction
  QuadraticFactor = 0.01;//0.0;            // Exprime la viscosité

  SpeedFactor = 0.7;                       // Vitesse
begin
  Result := SpeedFactor - t * ((QuadraticFactor * SpeedFactor + LinearFactor) * SpeedFactor + ConstantFactor);
  // Result := (ConstantFactor + (LinearFactor * SpeedFactor) + (QuadraticFactor * (SpeedFactor * SpeedFactor)));
  // Result := (Result / MassFactor) * t;
end;


function EaseInertia(t : Single) : Single; Inline;
begin
  //
end;

function easeFlip(t : Single) : Single;
begin
  result := 1.0 - t;
end;

function easeMix(EaseStartFunc, EaseStopFunc : TBZParametric1DFunc; BlendFactor, t : Single) : Single;
Var
  a, b : Single;
begin
  a := EaseStartFunc(t);
  b := EaseStopFunc(t);
  Result := a + BlendFactor * (b - a);
end;

function easeCrossFade(EaseStartFunc, EaseStopFunc : TBZParametric1DFunc; t : Single) : Single;
begin
  result := EaseMix(EaseStartFunc, EaseStopFunc, t, t);
end;

function easeScale(EaseFunc : TBZParametric1DFunc; t : Single) : Single;
begin
  Result := t * EaseFunc(t);
end;

function easeReverseScale(EaseFunc : TBZParametric1DFunc; t : Single) : Single;
begin
  Result := (1.0 - t) * EaseFunc(t);
end;

function easeArch(t : Single) : Single; inline;
begin
  // Result := EaseScale(EaseFlip,t));
  Result := t * (1.0 - t);
end;

function easeClampBottom(t : Single) : Single; inline;
begin
  if (t < 0.0) then Result := abs(t) else result := t;
end;

function easeClampTop(t : Single) : Single; inline;
begin
  if (t > 1.0) then Result :=1.0 - abs(1.0 - t) else result := t;
end;

function easeClampBottomTop(t : Single) : Single; inline;
begin
  Result := EaseClampTop(EaseClampBottom(t));
end;

function Tweener(Start, Target : Single; Time, Duration : Single; EaseMode : TBZAnimationMode; EaseType : TBZAnimationType) : Single;
Var
  AnimationController : TBZAnimationTool;
begin
  AnimationController := TBZAnimationTool.Create;
  Result := AnimationController.Tween(Time, Duration, Start, Target, EaseMode, EaseType);
  FreeAndNil(AnimationController);
end;



{ TBZAnimationTool }

Constructor TBZAnimationTool.Create;
begin
  inherited Create;
  FDuration := 30;
  FStartValue := 0.0;
  FEndValue := 1.0;
  FAnimationMode := amIn;
  FAnimationType := atLinear;
  FCustomEaseFunc := nil;

  FClampMode := acmNone;

  FInterpolate := False;
  FInterpolationFilter := ifmBox;

  FDecay := 0.0;
  FMomentum := 0.0;

  FExtraParamsSingle1 := 2.0;
end;

Destructor TBZAnimationTool.Destroy;
begin
  inherited Destroy;
end;

function TBZAnimationTool.GetEaseFunc(EaseType : TBZAnimationType) : TBZParametric1DFunc;
begin
  Case EaseType of
    atLinear :  Result := @EaseLinear;
    atQuadratic :  Result := @EaseSquare;
    atCubic :  Result := @EaseCubic;
    atQuartic :  Result := @EaseQuartic;
    atQuintic :  Result := @EaseQuintic;
    atExponantial :  Result := @EaseExponantial;
    atSine :  Result := @EaseSine;
    atCircle :  Result := @EaseCircle;
    atElastic :  Result := @EaseElastic;
    atBack :  Result := @EaseBack;
    atBounce :  Result := @EaseBounce;
    atBell :  Result := @EaseBell;
    atPower : Result := EasePowerGetFunc(FExtraParamsSingle1);
    atStep : Result := EaseStepGetFunc(FExtraParamsSingle1);
    atJitterWave : Result := @EaseJitterWave; //EaseJitterWaveGetFunc(FExtraParamsSingle1, FExtraParamsSingle2);
    atSwing : Result := @EaseSwing; //EaseSwingGetFunc(FExtraParamsSingle1);
    atTardis : Result := @EaseTardis; //EaseTardisGetFunc(FExtraParamsSingle1);
    atDamping : Result := @EaseDamping; //EaseTardisGetFunc(FExtraParamsSingle1, FExtraParamsSingle2, FExtraParamsSingle3, FExtraParamsSingle4);
    atWave : Result := @EaseWave;
    //atStep
    //atSpring
    //atCubicBezier
    //atBezier7
    //atCubicSpline
    atCustom : Result := FCustomEaseFunc;
    else Result := @EaseLinear;
  end;
end;

function TBZAnimationTool.Ease(EaseMode : TBZAnimationMode; EaseType : TBZAnimationType; t : Single) : Single;
var
  v : Single;
  EaseInFunc : TBZParametric1DFunc;
  InterpolateFilterClass : TBZInterpolationFilterClass;
  InterpolateFilter : TBZCustomInterpolationFilter;
begin


  EaseInFunc := GetEaseFunc(EaseType);

  Case EaseMode of
    amIn          : Result := EaseInFunc(t);
    amInInverted  : Result := EaseInFunc(1.0 - t);
    amOut         : Result := 1.0 - EaseInFunc(1.0 - t);
    amOutInverted : Result := 1.0 - EaseInFunc(t);
    amInOut:
    begin
      if (t < 0.5) then
      begin
        v := t + t;
        Result := EaseInFunc(v) * 0.5;
      end
      else
      begin
        v := 1.0 - t;
        v := v + v; //x * 2.0
        Result := 1.0 - EaseInFunc(v) * 0.5;
      end;
    end;
    amOutIn:
    begin
      if (t < 0.5) then
      begin
        v := t + t;
        v := 1.0 - v;
        Result := (1.0 - EaseInFunc(v)) * 0.5;
      end
      else
      begin
        v := 1.0 - t;
        v := v + v; //x * 2.0
        v := 1.0 - v;
        Result :=  1.0 - ( (1.0 - EaseInFunc(v) ) * 0.5);
      end;
    end;
    amSpike :
    begin
      if t < 0.5 then
      begin
        v := t + t;  //t / 0.5;
      end
      else
      begin
        v := (t - 0.5);
        v := v + v;
        v := 1.0 - v;
        //v := 1 - ((t - 0.5) / 0.5);
      end;
      Result := EaseInFunc(v);
    end;
    amSpikeInverted :
    begin
      if t < 0.5 then
      begin
          v := t + t //v := t / 0.5;
      end
      else
      begin
        v := (t - 0.5);
        v := v + v;
        v := 1.0 - v;
        //v := 1.0 - ((t - 0.5) / 0.5);
      end;
      Result := 1.0 - EaseInFunc(v);
    end;
    amArch :
    begin
      if t < 0.5 then
      begin
        //v := t / 0.5;
        v := t + t;
      end
      else
      begin
        v := (t - 0.5);
        v := v + v;
        v := 1.0 - v;
        //v := 1.0 - ((t - 0.5) / 0.5);
      end;
      Result := 1.0 - EaseInFunc(1.0 - v);
    end;
    amArchInverted :
    begin
      if t < 0.5 then
      begin
        // v := t / 0.5;
        v := t + t;
      end
      else
      begin
        v := (t - 0.5);
        v := v + v;
        v := 1.0 - v;
        //v := 1.0 - ((t - 0.5) / 0.5);
      end;
      Result := EaseInFunc(1.0 - v);
    end;
    amSpikeAndArch :
    begin
      if t < 0.5 then
      begin
         //v := t / 0.5;
         v := t + t;
         Result := EaseInFunc(v);
      end
      else
      begin
        //v := (t - 0.5) / 0.5;
        v := (t - 0.5);
        v := v + v;
        Result := 1.0 - EaseInFunc(v);
      end;
    end;
    amSpikeAndArchInverted :
    begin
      if t < 0.5 then
      begin
         //v := 1.0 - (t / 0.5);
         v := 1.0 - (t + t);
         Result := EaseInFunc(v);
      end
      else
      begin
        v := (t - 0.5);
        v := v + v;
        v := 1.0 - v;
        //v := 1.0 - ((t - 0.5) / 0.5);
        Result := 1.0 - EaseInFunc(v);
      end;
    end;
    amArchAndSpike :
    begin
      Result := easeCrossFade(@EaseCubic, @easeExponantial, t);
    end;
  end;

  if FInterpolate then
  begin
    InterpolateFilterClass := GetBZInterpolationFilter(FInterpolationFilter);
    InterpolateFilter := InterpolateFilterClass.Create;
    Result := InterpolateFilter.Filter(1.0 - Result);
    FreeAndNil(InterpolateFilter);
  end;

  if FClampMode <> acmNone then
  begin
    Case FClampMode of
      acmTop : Result := easeClampTop(Result);
      acmBottom : Result := easeClampBottom(Result);
      acmBoth :  Result := easeClampBottomTop(Result);
    end;
  end;
end;

function TBZAnimationTool.Ease(EaseMode : TBZAnimationMode; EaseType : TBZAnimationType; t : Single; Param1 : Single) : Single;
begin
  if EaseType in [atPower, atStep] then // in [atPower, atCubicBezier, atBezier7, atCubicSpline] then
  begin
    //Case EaseType of
    //  atPower:
    //  begin
        FExtraParamsSingle1 := Param1;
    //  end;
    //  atWave: ;
    //  atCubicBezier: ;
    //  atBezier7: ;
    //  atCubicSpline: ;
    //  atCustom: ;
    //end;
  end;
  Result := Ease(EaseMode, EaseType, t);
end;

function TBZAnimationTool.Tween(Current, Duration, rangeStart, rangeEnd : Single; EaseMode : TBZAnimationMode; EaseType : TBZAnimationType) : Single;
Var
   t , x : Single;
begin
  t := Current / Duration;
  x := (rangeEnd - rangeStart) * Ease(EaseMode, EaseType, t);
  Result := rangeStart + x;
end;

function TBZAnimationTool.Tween(Current, Duration, rangeStart, rangeEnd : Single; EaseMode : TBZAnimationMode; EaseType : TBZAnimationType; Param1 : Single) : Single;
Var
   t , x : Single;
begin
  t := Current / Duration;
  x := (rangeEnd - rangeStart) * Ease(EaseMode, EaseType, t, Param1);
  Result := rangeStart + x;
end;

function TBZAnimationTool.Animate(CurrentStep : Single; Const Reverse : Boolean) : Single;
begin
  Result := Tween(CurrentStep, FDuration, FStartValue, FEndValue, FAnimationMode, FAnimationType);

  if Reverse then Result := FStartValue + FEndValue - Result;
end;

function TBZAnimationTool.Animate(CurrentStep : Single; Param1 : Single; Const Reverse : Boolean) : Single;
begin
  Result := Tween(CurrentStep, FDuration, FStartValue, FEndValue, FAnimationMode, FAnimationType, param1);

  if Reverse then Result := FStartValue + FEndValue - Result;
end;

function TBZAnimationTool.Animate(CurrentStep : Single; Param1, Param2 : Single; Const Reverse : Boolean) : Single;
begin
  // Wave + Spring
end;

function TBZAnimationTool.Animate(CurrentStep : Single; Param1, Param2 : TBZFloatPoint; Const Reverse : Boolean) : Single;
begin
  // CubicBezier + CubicSpline
end;

function TBZAnimationTool.AnimateInt(CurrentStep : Single; Const Reverse : Boolean) : Integer;
begin
  Result := Round(Animate(CurrentStep, Reverse));
end;

function TBZAnimationTool.AnimateInt(CurrentStep : Single; Param1 : Single; Const Reverse : Boolean) : Integer;
begin
  Result := Round(Animate(CurrentStep, Param1, Reverse ));
end;

{ Exemple un personnage de jeu à une vitesse (in) d'attaque entre 4 et 12 et les dommages infligés (out) entre 1 et 4 }
function TBZAnimationTool.AnimateRemap(CurrentStep : Single; inStart, inEnd : Single) : Single;
var
  v : Single;
begin
  v := CurrentStep - inStart;
  v := v / (inEnd - inStart);
  v := Ease(FAnimationMode, FAnimationType, v);
  v := v * (FEndValue - FStartValue);

  Result := v + FStartValue;
end;


end.

