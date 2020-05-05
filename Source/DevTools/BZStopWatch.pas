(*
  Contient un composant "TBZStopWatch" servant à effectuer un chronometrage
  de vos procedures ou fonctions en micro, nano, milli secondes et secondes.@br
  Il peux également servir calculer le FPS dans des applications graphique.

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
    Une variable globale "GlobalPerformanceTimer" est créée automatiquement. V

  Si vous démarrer le chronometre avec le parametre "AUseTickCount" de la procedure
  "Start" à "TRUE" (par defaut à FALSE) alors TBZStopWatch utilisera  l'horloge
  temps réel (RTC) (Déconseiller sous FPC en mode 32bit).@br
  Elle est précise sur de longues périodes, mais n'est pas exacte à la milliseconde.

  Sinon TBZStopWatch utilisera le compteur de performance "Windows". @br
  Pour Linux un "hack" est utilisé.@br
  Le compteur de performance a une meilleure précision, mais il peut quand même
  dériver sur de longues périodes. C'est l'option par défaut car elle permet une
  plus grande précision sur les systèmes rapides.@br
  Mais si la procedure est trop rapide il vaut mieux utilser la RTC pour obtenir un résultat cohérent.

  -------------------------------------------------------------------------------------------------------------

  @bold(Dependances) : BZSystem

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)@br
    @unorderedList(
      @item(J.Delauney (BeanzMaster))
    )

  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZStopWatch;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils,
  BZSystem;

type
  { Unité de mesure du chronomètre }
  TBZTimeUnit = (
    tuNanosecond,       //< 1/1000000000 s
    tuMicrosecond,      //< 1/1000000 s
    tuMillisecond,      //< 1/1000 s
    tuSecond            //< 1 s
//    tuMinute,           // 60 s
//    tuHour              // 3600 s
  );

type

  { Composant non-visuel permettant de chronométrer une action }
  TBZStopWatch = class(TComponent)
  private
    FFrequency, FPerformanceCountStart, FPerformanceCountStop: Int64;
    FFrameCounter : Int64;
    FFPSCurrentTime : Double;
  protected

    FNanoSecPerTick: Double;
    FMicroSecPerTick: Double;
    FMilliSecPerTick: Double;
    FSecPerTick: Double;
    FInvPerformanceCounterFrequencyReady: Boolean;
    FFPS: Single;
    FPrecision : Byte;
    FUseRTC, FStarted : Boolean;
    FCPUClock :Double;

  public
    { Creation de TBZStopWatch }
    constructor Create(AOwner: TComponent); override;

    { Démarre le chronomètre avec utilisation l'horloge RTC ou non en fonction du paramètre. Par défaut : FALSE}
    procedure Start(Const AUseRTC:boolean = false);
    { Arrête le chronomètre }
    procedure Stop;
    { Retourne le temps écoulé en ticks}
    function getTimerLap : Double;
    { Convertis les "Ticks" dans l'unité de temps choisi }
    function TickToTimeUnit(const ATick: Int64; AUnit: TBZTimeUnit): Extended;
    { Retourne la valeur sous forme de chaine de caractères au format TDateTime}
    function GetValueAsTime: String;
    { Retourne le temps écoulé en microsecondes sous forme de chaine de caratères }
    function getValueAsMicroSeconds: string;
    { Retourne le temps écoulé en nanosecondes sous forme de chaine de caratères }
    function getValueAsNanoSeconds: string;
    { Retourne le temps écoulé en millisecondes sous forme de chaine de caratères }
    function getValueAsMilliSeconds: string;
    { Retourne le temps écoulé en secondes sous forme de chaine de caratères }
    function getValueAsSeconds: string;

    //function GetValueAsText(TimeUnit:TBZTime): String;

    { Retourne une valeur entiere du temps écoulé microsecondes }
    function getValue: Int64;
    { Retourne une valeur double du temps écoulé secondes }
    function getAsSecond : Double;
    { Retourne une valeur double du temps écoulé millisecondes }
    function getAsMilliSeconds : Double;
    { Retourne une valeur Extended du temps écoulé ticks }
    function getTicks: Extended;
    { Retourne le nombre de FPS en fonction du nombre de "Frame" }
    function getFPS(FrameCounter:Integer):Single; overload;
    { Retourne le nombre de FPS en fonction du nombre de "Frame" }
    function getFPS:Single; overload;
    { Retourne le nombre de FPS en fonction du nombre de "Frame" sous forme de caractères }
    function getFPSAsString(FrameCounter:Integer):String;
    { Convertis un nombre de ticks en microsecondes sous forme de chaine de caractères }
    function TickToMicroSecond(const ATick: Int64): String;

    { Utiliser l'horloge RTC }
    property UseRTC : Boolean read FUseRTC write FUseRTC;
    { Retourne la valeur entiere du temps au démarrage du chronomètre }
    property PerformanceCountStart:Int64 read FPerformanceCountStart;
    { Retourne la valeur entiere du temps a l'arrêt du chronomètre }
    property PerformanceCountStop:Int64 read FPerformanceCountStop;
    { Frequence de l'horloge interne }
    property Frequency : Int64 read FFrequency;
    { Nombre de chiffre après la virgule pour les resultats }
    property Precision : Byte read FPrecision write FPrecision default 12;
  end;

//==============================================================================

var
  { Variable globale décrivant un TBZStopWatch. Elle initialisée et détruite automatiquement }
  GlobalStopWatch: TBZStopWatch;

//==============================================================================

implementation

//==============================================================================
const
  SecsPerMin = 60;
  SecsPerHour = SecsPerMin * 60;
  SecsPerDay = SecsPerHour * 24;

  DefaultDisplayFormat = '#,##0.0';
  TimeUnitName: array[TBZTimeUnit] of String =
    (' ns', ' µs', ' ms', ' s');//, 'm', 'h');
  TimeUnitCoefficient: array[TBZTimeUnit] of Extended =
    (1000000000, 1000000, 1000, 1);//, 1/60, 1/3600);

//===[ TBZStopWatch ]===========================================================

Type
  TTimeItemNames = Array[0..5] of string;

function SecondsToTimeString(Seconds: Int64; const itemNames: TTimeItemNames): string;overload;
const
  divisors: array [0..5] of Int64 = (SecsPerDay * 365, SecsPerDay * 31, SecsPerDay, SecsPerHour, SecsPerMin, 1);

var
  resCount: integer;
  I: Integer;
  C, V: Int64;
begin
  result := '';
  resCount := 0;
  C := Seconds;
  for I := 0 to 5 do
  begin
    V := C div divisors[I];
    if V > 0 then
    begin
      if resCount > 0 then  result := result + ' ';
      result := result + IntToStr(V) + itemNames[I];
      Inc(resCount);
      //if resCount > 4 then break;
      C := C mod divisors[I];
    end;
  end;
end;

function SecondsToTimeString(Seconds: Int64): string; overload;
const
  itemNames: TTimeItemNames = ('year', 'month', 'day', 'h', 'm', 's');
begin
  result := SecondsToTimeString(Seconds, itemNames);
end;

function TBZStopWatch.GetValueAsTime : String;
begin
  if FUseRTC then
  begin
    Result := SecondsToTimeString(round((getTimerLap/ FCPUClock)*FSecPerTick));
  end
  else
  begin
    Result := SecondsToTimeString(round(getTimerLap*FSecPerTick));
  end;
end;

function TBZStopWatch.TickToTimeUnit(const ATick : Int64; AUnit : TBZTimeUnit) : Extended;
begin
  if FUseRTC then
  begin
    Result := (ATick /FCPUClock) * TimeUnitCoefficient[AUnit];
  end
  else
  begin
    Result := ATick * FSecPerTick * TimeUnitCoefficient[AUnit];
  end;
end;

function TBZStopWatch.TickToMicroSecond(const ATick : Int64) : String;
begin
  if FUseRTC then
  begin
     Result := FloatToStrF((ATick/FCPUClock) / (TimeUnitCoefficient[tuMilliSecond]), ffGeneral, 15, 3)+TimeUnitName[tuMilliSecond];
  end
  else
  begin
   // Result := ATick * FSecPerTick * TimeUnitCoefficient[AUnit];
    result:=FloatToStrF(ATick*( TimeUnitCoefficient[tuMilliSecond]/FFrequency), ffGeneral, 15, 3)+TimeUnitName[tuMilliSecond];
  end;
end;

constructor TBZStopWatch.Create(AOwner : TComponent);
begin
  inherited;
  FPerformanceCountStart:=0;
  FPerformanceCountStop:=0;
  FPrecision:=12;
  QueryPerformanceFrequency(FFrequency);
  Assert(FFrequency > 0);

  FNanoSecPerTick:=1000000000.0/FFrequency; // Resolution du timer en nanosecondes;
  FMicroSecPerTick:=1000000.0/FFrequency;   // Resolution du timer en microsecondes
  FMilliSecPerTick:=1000.0/FFrequency;      // Resolution du timer en millisecondes
  FSecPerTick:=1.0/FFrequency;              // Resolution du timer en secondes
                                            // minute = (1/60)/FFrequency,  heure = (1/3600)/FFrequency

  FFPS:=0.0;
  FUseRTC:=False;
  FStarted:=false;
  FCPUClock := CPU_Speed;
end;

function TBZStopWatch.getTimerLap : Double;
var
  StopTime:Int64;
begin
  StopTime :=0;
  if FPerformanceCountStop > 0 then StopTime := FPerformanceCountStop
  else
  begin
    if FUseRTC then
    begin
      StopTime:=GetClockCycleTickCount;
    end
    else
    begin
      QueryPerformanceCounter(StopTime);
    end;
  end;

  if FUseRTC then
  begin
    Result := (StopTime - FPerformanceCountStart);
  end
  else
  begin
    Result := (StopTime - FPerformanceCountStart);
  end;
end;

function TBZStopWatch.getValueAsMicroSeconds : string;
begin
  if FUseRTC then
  begin
    Result := FloatToStrF((getTimerLap/ FCPUClock)*FMicroSecPerTick, ffGeneral, 15, FPrecision)+' µs';
  end
  else
  begin
    Result:= FloatToStrF(getTimerLap*FMicroSecPerTick, ffGeneral, 15, FPrecision)+' µs';
  end;
end;

function TBZStopWatch.getValueAsNanoSeconds : string;
begin
  if FUseRTC then
  begin
    Result := FloatToStrF((getTimerLap/ FCPUClock)*FNanoSecPerTick, ffFixed, 15, FPrecision)+' ns';
  end
  else
  begin
    Result:= FloatToStrF(getTimerLap*FNanoSecPerTick, ffFixed, 15, FPrecision)+' ns';
  end;
end;

function TBZStopWatch.getValueAsMilliSeconds : string;
begin
  if FUseRTC then
  begin
    Result := FloatToStrF((getTimerLap/ FCPUClock)*FMilliSecPerTick, ffFixed, 15, FPrecision)+' ms';
  end
  else
  begin
    Result := FloatToStrF(getTimerLap*FMilliSecPerTick, ffFixed, 15, FPrecision)+' ms';
  end;
end;

function TBZStopWatch.getValueAsSeconds : string;
begin
  if FUseRTC then
  begin
    Result := FloatToStrF((getTimerLap/ FCPUClock)*FSecPerTick, ffFixed, 15, FPrecision)+' sec';
  end
  else
  begin
    Result := FloatToStrF((getTimerLap)*FSecPerTick  , ffFixed, 15, FPrecision)+' sec';
  end;
end;

function TBZStopWatch.getValue : Int64;
begin
  if FUseRTC then
  begin
    Result := Round((getTimerLap/ FCPUClock));
  end
  else
  begin
    Result := Round(getTimerLap);
  end;
end;

function TBZStopWatch.getAsSecond : Double;
Begin
  if FUseRTC then
  begin
    Result := (getTimerLap/ FCPUClock)*FSecPerTick;
  end
  else
  begin
    Result := getTimerLap*FSecPerTick;
  end;
End;

function TBZStopWatch.getAsMilliSeconds : Double;
begin
  if FUseRTC then
  begin
    Result := (getTimerLap/ FCPUClock)*0.0001;
  end
  else
  begin
    Result := (getTimerLap*FMilliSecPerTick);
  end;
end;

function TBZStopWatch.getTicks : Extended;
begin
  if FUseRTC then
  begin
    Result := (getTimerLap/ FCPUClock);
  end
  else
  begin
    Result := getTimerLap;
  end;
end;

procedure TBZStopWatch.Start(const AUseRTC : boolean);
begin
  FPerformanceCountStop := 0;
  FPerformanceCountStart:=0;
  FFPS:=0.0;
  FFrameCounter := 0;
  FUseRTC := AUseRTC;
  if FUseRTC then
  begin
   // FUseRTC:=True;
    FPerformanceCountStart:=GetClockCycleTickCount;
  end
  else
  begin
    QueryPerformanceCounter(FPerformanceCountStart);
  end;
  FStarted:=true;
end;

procedure TBZStopWatch.Stop;
begin
  if FUseRTC then
  begin
    FPerformanceCountStop:=GetClockCycleTickCount;
  end
  else
  begin
    QueryPerformanceCounter(FPerformanceCountStop);
  end;
  FStarted:=False;

end;

function TBZStopWatch.getFPS(FrameCounter : Integer) : Single;
begin

  if FrameCounter > 0 then
  begin
    if FUseRTC then
    begin
      FFPS := FrameCounter / ((getTimerLap/ FCPUClock)*0.000001);  //1000.0
    end
    else
    begin
      FFPS := FrameCounter / (getTimerLap*FSecPerTick);
    end;
  end;
  result:=FFPS;
end;

function TBZStopWatch.getFPS : Single;
begin

  FFPSCurrentTime := getAsMilliSeconds;
  if (FFPSCurrentTime >= 1000) then
  begin
    if FFrameCounter > 0 then
    begin
      FFPS := FFrameCounter / (FFPSCurrentTime*0.001);
      //FFPS := FFrameCounter / (FFPSCurrentTime / 1000);
      FFPSCurrentTime := 0;
      FPerformanceCountStop := 0;
      FPerformanceCountStart:=0;
      FFrameCounter := 0;
      if FUseRTC then
      begin
        FPerformanceCountStart:=GetClockCycleTickCount;
      end
      else
      begin
        QueryPerformanceCounter(FPerformanceCountStart);
      end;
    end;
  end;
  inc(FFrameCounter);
  result:=FFPS;

  //    FPS := 1000/FPSCurrentTime*FFrameCounter;
end;

function TBZStopWatch.getFPSAsString(FrameCounter : Integer) : String;
begin
  result:=Format('%.*f FPS', [3, getFPS(FrameCounter)]);
end;

//==============================================================================

initialization

  GlobalStopWatch := TBZStopWatch.Create(nil);

finalization

  FreeAndNil(GlobalStopWatch);


//==============================================================================
end.

