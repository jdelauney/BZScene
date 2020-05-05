(*
  @abstract(Contient un composant non visuel pour facilité le traitement d'evenements de progression)

  -------------------------------------------------------------------------------------------------------------

  @created(2012-11-10)
  @author(GLScene)
  Historique : @br
  @unorderedList(
    @item(10/11/2012 : Creation  )
  )


  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) :

  -------------------------------------------------------------------------------------------------------------

  @bold(Dépendances) : BZSystem

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)
   @unorderedList(
     @item (Basé sur le code de GLScene http://www.sourceforge.net/glscene)
   )

  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL/GPL

  ------------------------------------------------------------------------------------------------------------- *)
Unit BZCadencer;

//==============================================================================
{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
{$i ..\..\bzscene_options.inc}
//==============================================================================

Interface

Uses
  Classes, Forms, lmessages, SyncObjs,
  BZClasses;

Type


  { Détermine comment fonctionne le TBZCadencer.@Br
    @unorderedlist(
      @item(cmManual : vous devez déclencher la progression manuellement (dans votre code).)
      @item(cmASAP : la progression est déclenchée aussi tôt que possible après une précédente progression (utilise les messages de Windows).)
      @item(cmApplicationIdle : va accrocher Application.OnIdle, cela va écraser toute manipulation d'événement précédente, et un seul cadencer peut être utilisé dans ce mode.)) }
  TBZCadencerMode = (cmManual, cmASAP, cmApplicationIdle);

  { Determines à quel moment le TBZCadencer doit "progesser".
    @unorderedlist(
      @item(cmRTC : l'horloge en temps réel est utilisée (précise sur de longues périodes, mais pas précis à la milliseconde. @br
                   Peut limiter la vitesse (efficace à moins de 50 FPS sur certains systèmes) )
      @item(cmPerformanceCounter : le compteur de performances a une bonne précision. Il peut dériver sur de longues périodes. @br
                                  C'est l'option par défaut car il permet l'animation la plus douce sur les systèmes rapides.)
      @item(cmExternal : La propriété CurrentTime est utilisée)) }
  TBZCadencerTimeReference = (cmRTC, cmPerformanceCounter, cmExternal);

  { @abstract(Composant permettant une progression automatique d'une animation.)

   Envoie les événements de progression en temps réel (le temps sera mesuré en secondes). @br
   Ou il gardera la CPU occupée à 100% si possible (c.-à-d. si les choses changent dans votre scène). @br
   Le temps de progression (celui que vous verrez dans vos événements de progression)
   est calculé en utilisant (CurrentTime-OriginTime) * TimeMultiplier.@br
   "CurrentTime" est mis manuellement ou automatiquement à jour en utilisant
   "TimeReference" (le paramètre "CurrentTime" NE déclenche PAS la progression). }
  TBZCadencer = Class(TComponent)
  Private

    FSubscribedCadenceableComponents: TList;
    // FScene: TBZScene;
    FTimeMultiplier: Double;
    FInvTimeMultiplier : Double;
    lastTime, downTime, lastMultiplier: Double;
    FEnabled: Boolean;
    FSleepLength: Integer;
    FMode: TBZCadencerMode;
    FTimeReference: TBZCadencerTimeReference;
    FCurrentTime: Double;
    FOriginTime: Double;
    FMaxDeltaTime, FMinDeltaTime, FFixedDeltaTime: Double;
    FOnProgress, FOnTotalProgress: TBZCadencerProgressEvent;
    FProgressing: Integer;
    Procedure SetCurrentTime(Const Value: Double);

  Protected

    Procedure Notification(AComponent: TComponent; Operation: TOperation); Override;
    Function StoreTimeMultiplier: Boolean;
    Procedure SetEnabled(Const val: Boolean);
    // procedure SetScene(const val: TBZScene);
    Procedure SetMode(Const val: TBZCadencerMode);
    Procedure SetTimeReference(Const val: TBZCadencerTimeReference);
    Procedure SetTimeMultiplier(Const val: Double);

    { Renvoie le temps de réponse brut (pas de multiplicateur, pas d'offset) }
    Function GetRawReferenceTime: Double;
    Procedure RestartASAP;
    Procedure Loaded; Override;

    Procedure OnIdleEvent(Sender: TObject; Var Done: Boolean);

  Public

    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure Subscribe(aComponent: TBZCadenceAbleComponent);
    Procedure UnSubscribe(aComponent: TBZCadenceAbleComponent);

    { @abstract(Permet de déclencher manuellement une progression.) @br
      Le temps est traité automatiquement. Si cadencer est désactivé, cette fonction ne fait rien. }
    Procedure Progress;

    { Ajuste "CurrentTime" si nécessaire, puis renvoie sa valeur. }
    Function GetCurrenttime: Double;

    { @abstract(Renvoie True si une Progression est en cours.) @br
      Soyez conscient que, tant que IsBusy est Vrai, le Cadencer peut envoyer de messages et
      appeller d'autres progressions vers des autres scènes et composants cadencés. }
    Function IsBusy: Boolean;

    { Remise à zero des paramètres et du temps.}
    Procedure Reset;

    { Valeur soustraite au temps actuel pour obtenir le temps de progression. }
    Property OriginTime: Double read FOriginTime write FOriginTime;
    { Temps actuel (réglé manuellement ou automatiquement, voir TimeReference). }
    Property CurrentTime: Double read FCurrentTime write SetCurrentTime;

  Published

    { The TBZScene that will be cadenced (progressed). }
    //    property Scene: TBZScene read FScene write SetScene;

    { @abstract(Activer / Désactiver la Candence.)

      La désactivation ne provoquera pas de saut lors du redémarrage, mais fonctionne comme
      une lecture / pause (c.-à-d. peut modifier OriginTime pour garder une progression douce). }
    Property Enabled: Boolean read FEnabled write SetEnabled Default True;

    { @abstract(Définit comment CurrentTime est mis à jour. Voir TBZCadencerTimeReference.)

      Le changement dynamique de TimeReference peut provoquer un 'saut'. }
    Property TimeReference: TBZCadencerTimeReference read FTimeReference write SetTimeReference Default cmPerformanceCounter;

    { @abstract(Multiplicateur appliqué à la référence de temps.)

      Zéro n'est pas une valeur autorisée, et sachez que si des valeurs négatives
      sont acceptés, elles peuvent ne pas être pris en charge par d'autres objets.@br
      La modification du TimeMultiplier modifiera OriginTime. }
    Property TimeMultiplier: Double read FTimeMultiplier write SetTimeMultiplier Stored StoreTimeMultiplier;

    { @abstract(Valeur maximale pour deltaTime dans les événements de progression.)

      Si null ou négatif, aucun deltaTime max est défini. Sinon, chaque fois qu'un événement
      dont le deltaTime réel est supérieur à MaxDeltaTime, le deltaTime sera à son maximum, et le temps supplémentaire est mise en cache
      par le cadencer (il n'est pas encore pris en compte dans CurrentTime).@br
      Cette option permet de limiter le taux de progression dans les simulations où Des valeurs élevées entraîneraient des erreurs / comportement aléatoire. }
    Property MaxDeltaTime: Double read FMaxDeltaTime write FMaxDeltaTime;

    { @abstract(Valeur minimale pour deltaTime dans les événements de progression.)

      Si supérieur à zéro, cette valeur spécifie le "pas" du temps minimum entre deux événements de progression.@br
      Cette option permet de limiter le taux de progression dans les simulations où les valeurs faibles entraîneraient des erreurs / comportement aléatoire. }
    Property MinDeltaTime: Double read FMinDeltaTime write FMinDeltaTime;

    { @abstract(Valeur temporelle fixe pour les événements de progression.)

      Si supérieur à zéro, des étapes de progression se produiront avec celles fixées au temps
      delta. La progression reste basée sur le temps, donc zéro vers N événements
      peut être déclenché en fonction du deltaTime réel (si deltaTime est
      inférieur à FixedDeltaTime, aucun événement ne sera déclenché s'il est supérieur
      à deux fois FixedDeltaTime, deux événements seront déclenchés, etc.).@br
      Cette option permet d'utiliser des étapes de temps fixes dans les simulations (pendant
      l'animation et le rendu peuvent se produire à un niveau de fractionnement inférieur ou supérieur). }
    Property FixedDeltaTime: Double read FFixedDeltaTime write FFixedDeltaTime;

    { Ajuste comment les évènement de progression doivent être déclenchés. Voir TBZCadencerMode. }
    Property Mode: TBZCadencerMode read FMode write SetMode Default cmASAP;

    { @abstract(Permet de laisser du "temps" à d'autres threads / processus.)

      Si SleepLength> = 0 alors AVANT chaque progression une pause se produit (voir
      la procédure "Sleep" dans FPC pour plus de détails). }
    Property SleepLength: Integer read FSleepLength write FSleepLength Default -1;

    { Evenement déclenché apres le progression. }
    Property OnProgress: TBZCadencerProgressEvent read FOnProgress write FOnProgress;
    { Evenement déclenché quand toutes les iterations avec DeltaTime fixe sont finis. }
    Property OnTotalProgress: TBZCadencerProgressEvent read FOnTotalProgress write FOnTotalProgress;
  End;

  { Ajoute une propriété "protégée" pour se connecter à un TBZCadencer. }
  TBZCustomCadencedComponent = Class(TBZUpdateAbleComponent)
  Private
    FCadencer: TBZCadencer;
  Protected
    Procedure SetCadencer(Const val: TBZCadencer);
    Property Cadencer: TBZCadencer read FCadencer write SetCadencer;
  Public
    Destructor Destroy; Override;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); Override;
  End;

  { Composant à hériter dont la propriété "Cadencer" est publiée }
  TBZCadencedComponent = Class(TBZCustomCadencedComponent)
  Published
    Property Cadencer;
  End;

Implementation


Uses
  SysUtils, BZSystem;

Const
  LM_GLTIMER = LM_INTERFACELAST + 326;


Type
  TASAPHandler = Class;
  // TTimerThread
  TTimerThread = Class(TThread)
  Private
    FOwner:    TASAPHandler;
    FInterval: Word;
  Protected
    Procedure Execute; Override;
  Public
    Constructor Create(CreateSuspended: Boolean); Virtual;
  End;

  { TASAPHandler }
  TASAPHandler = Class
  Private
    FTimerThread: TThread;
    FMutex: TCriticalSection;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure TimerProc;
    Procedure Cadence(Var Msg: TLMessage); Message LM_GLTIMER;
  End;

Var
  vASAPCadencerList: TList;
  vHandler: TASAPHandler;
  vCounterFrequency: Int64;

Procedure RegisterASAPCadencer(aCadencer: TBZCadencer);
Begin
  If aCadencer.Mode = cmASAP Then
  Begin
    If Not Assigned(vASAPCadencerList) Then vASAPCadencerList := TList.Create;
    If vASAPCadencerList.IndexOf(aCadencer) < 0 Then
    Begin
      vASAPCadencerList.Add(aCadencer);
      If Not Assigned(vHandler) Then vHandler := TASAPHandler.Create;
    End;
  End
  Else If aCadencer.Mode = cmApplicationIdle Then Application.OnIdle := @aCadencer.OnIdleEvent;
End;

Procedure UnRegisterASAPCadencer(aCadencer: TBZCadencer);
Var
  i: Integer;
Begin
  If aCadencer.Mode = cmASAP Then
  Begin
    If Assigned(vASAPCadencerList) Then
    Begin
      i := vASAPCadencerList.IndexOf(aCadencer);
      If i >= 0 Then vASAPCadencerList[i] := nil;
    End;
  End
  Else If aCadencer.Mode = cmApplicationIdle Then Application.OnIdle := nil;
End;

{%region%=== [ TTimerThread ]==================================================}

Constructor TTimerThread.Create(CreateSuspended: Boolean);
Begin
  Inherited Create(CreateSuspended);
End;

Procedure TTimerThread.Execute;
Var
  lastTick, nextTick, curTick, perfFreq: Int64;
Begin
  LastTick := 0; // Happy Compilo
  PerfFreq := 0;
  CurTick := 0;

  //QueryPerformanceFrequency(perfFreq);
  perfFreq := vCounterFrequency;
  QueryPerformanceCounter(lastTick);
  nextTick := lastTick + (FInterval * perfFreq) Div 1000;
  While Not Terminated Do
  Begin
    FOwner.FMutex.Acquire;
    FOwner.FMutex.Release;
    While Not Terminated Do
    Begin
      QueryPerformanceCounter(lastTick);
      If lastTick >= nextTick Then break;
      //Sleep(1);
    End;
    If Not Terminated Then
    Begin
      // if time elapsed run user-event
      Synchronize(@FOwner.TimerProc);
      //FOwner.TimerProc;
      QueryPerformanceCounter(curTick);
      nextTick := lastTick + (FInterval * perfFreq) Div 1000;
      If nextTick <= curTick Then
      Begin
        // CPU too slow... delay to avoid monopolizing what's left
        nextTick := curTick + (FInterval * perfFreq) Div 1000;
      End;
    End;
  End;
End;

{%endregion%}

{%region%=== [ TASAPHandler ]==================================================}

Constructor TASAPHandler.Create;
Begin
  Inherited Create;

  // create timer thread
  FMutex := TCriticalSection.Create;
  FMutex.Acquire;
  FTimerThread := TTimerThread.Create(False);

  With TTimerThread(FTimerThread) Do
  Begin
    FOwner := Self;
    FreeOnTerminate := False;
    Priority := tpTimeCritical;
    FInterval := 1;
    FMutex.Release;
  End;

End;

Destructor TASAPHandler.Destroy;
Begin
  FMutex.Acquire;
  FTimerThread.Terminate;
  CheckSynchronize;
  // wait & free
  FTimerThread.WaitFor;
  FTimerThread.Free;
  FMutex.Free;

  Inherited Destroy;
End;

Procedure TASAPHandler.TimerProc;
Var
  NewMsg: TLMessage;
Begin
  NewMsg.Msg := LM_GLTIMER;
  Cadence(NewMsg);
End;

Procedure TASAPHandler.Cadence(Var Msg: TLMessage);
Var
  i,c:   Integer;
  cad: TBZCadencer;
Begin

  If Assigned(vHandler) And Assigned(vASAPCadencerList) And (vASAPCadencerList.Count <> 0) Then
  begin
    c := vASAPCadencerList.Count;// - 1;
    i:=0;
    repeat
    Begin
      cad := TBZCadencer(vASAPCadencerList[i]);
      If Assigned(cad) And (cad.Mode = cmASAP) And cad.Enabled And (cad.FProgressing = 0) Then
      Begin
        If not(Application.Terminated) Then
        Begin
          Try
            // do stuff
            cad.Progress;
          Except
            Application.HandleException(Self);
            // it faulted, stop it
            cad.Enabled := False;
          End;
        End
        Else
        Begin
          // force stop
          cad.Enabled := False;
        End;
      End;
      //dec(c);
      Inc(i);
    End;
    until i=c;
  end;
End;

{%endregion%}

{%region%=== [ TBZCadencer ]==================================================}

Constructor TBZCadencer.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  FTimeReference := cmPerformanceCounter;
  downTime := GetRawReferenceTime;
  FOriginTime := downTime;
  FTimeMultiplier := 1;
  FInvTimeMultiplier := 0;
  FSleepLength := -1;
  Mode := cmASAP;
  Enabled := False;
End;

Destructor TBZCadencer.Destroy;
Begin
  Assert(FProgressing = 0);
  UnRegisterASAPCadencer(Self);
  FSubscribedCadenceableComponents.Free;
  FSubscribedCadenceableComponents := nil;
  Inherited Destroy;
End;

Procedure TBZCadencer.Subscribe(aComponent: TBZCadenceAbleComponent);
Begin
  If Not Assigned(FSubscribedCadenceableComponents) Then FSubscribedCadenceableComponents := TList.Create;
  If FSubscribedCadenceableComponents.IndexOf(aComponent) < 0 Then
  Begin
    FSubscribedCadenceableComponents.Add(aComponent);
    aComponent.FreeNotification(Self);
  End;
End;

Procedure TBZCadencer.UnSubscribe(aComponent: TBZCadenceAbleComponent);
Var
  i: Integer;
Begin
  If Assigned(FSubscribedCadenceableComponents) Then
  Begin
    i := FSubscribedCadenceableComponents.IndexOf(aComponent);
    If i >= 0 Then
    Begin
      FSubscribedCadenceableComponents.Delete(i);
      aComponent.RemoveFreeNotification(Self);
    End;
  End;
End;

Procedure TBZCadencer.Notification(AComponent: TComponent; Operation: TOperation);
Begin
  If Operation = opRemove Then
  Begin
   // If AComponent = FScene Then FScene := nil;
    If Assigned(FSubscribedCadenceableComponents) Then FSubscribedCadenceableComponents.Remove(AComponent);
  End;
  Inherited;
End;

Procedure TBZCadencer.Loaded;
Begin
  Inherited Loaded;
  RestartASAP;
End;

Procedure TBZCadencer.OnIdleEvent(Sender: TObject; Var Done: Boolean);
Begin
  Progress;
  Done := False;
End;

Procedure TBZCadencer.RestartASAP;
Begin
  If Not (csLoading In ComponentState) Then
  Begin
    If (Mode In [cmASAP, cmApplicationIdle]) And (Not (csDesigning In ComponentState)) And Enabled Then
    //And Assigned(FScene)
      RegisterASAPCadencer(Self)
    Else
      UnRegisterASAPCadencer(Self);
  End;
End;

Procedure TBZCadencer.SetEnabled(Const val: Boolean);
Begin
  If FEnabled <> val Then
  Begin
    FEnabled := val;
    If Not (csDesigning In ComponentState) Then
    Begin
      If Enabled Then
        FOriginTime := FOriginTime + GetRawReferenceTime - downTime
      Else
      begin
         downTime := GetRawReferenceTime;
      end;
      RestartASAP;
    End;
  End;
End;

(* procedure TBZCadencer.SetScene(const val: TBZScene);
begin
  if FScene <> val then
  begin
    if Assigned(FScene) then FScene.RemoveFreeNotification(Self);
    FScene := val;
    if Assigned(FScene) then FScene.FreeNotification(Self);
    RestartASAP;
  end;
end; *)

Procedure TBZCadencer.SetTimeMultiplier(Const val: Double);
Var
  rawRef: Double;
  invVal : Double;
Begin
  If val <> FTimeMultiplier Then
  Begin
    invVal :=0;
    If val = 0 Then
    Begin
      lastMultiplier := FTimeMultiplier;
      Enabled := False;
    End
    Else
    Begin
      invVal := 1/val;
      rawRef := GetRawReferenceTime;
      If FTimeMultiplier = 0 Then
      Begin
        Enabled := True;
        // continuity of time:
        // (rawRef-newOriginTime)*val = (rawRef-FOriginTime)*lastMultiplier
        FOriginTime := rawRef - (rawRef - FOriginTime) * lastMultiplier * InvVal;
      End
      Else
      Begin
        // continuity of time:
        // (rawRef-newOriginTime)*val = (rawRef-FOriginTime)*FTimeMultiplier
        FOriginTime := rawRef - (rawRef - FOriginTime) * FTimeMultiplier * InvVal;
      End;
    End;
    FTimeMultiplier := val;
    FInvTimeMultiplier := InvVal;
  End;
End;

Function TBZCadencer.StoreTimeMultiplier: Boolean;
Begin
  Result := (FTimeMultiplier <> 1);
End;

Procedure TBZCadencer.SetMode(Const val: TBZCadencerMode);
Begin
  If FMode <> val Then
  Begin
    If FMode <> cmManual Then UnRegisterASAPCadencer(Self);
    FMode := val;
    RestartASAP;
  End;
End;

Procedure TBZCadencer.SetTimeReference(Const val: TBZCadencerTimeReference);
Begin
  // nothing more, yet
  FTimeReference := val;
End;

Procedure TBZCadencer.Progress;
Var
  deltaTime, newTime, totalDelta: Double;
  fullTotalDelta, firstLastTime: Double;
  i:  Integer;
  pt: TBZProgressTimes;

Begin
  // basic protection against infinite loops,
  // shall never happen, unless there is a bug in user code
  If FProgressing < 0 Then Exit;
  If Enabled Then
  Begin
    // avoid stalling everything else...
    If SleepLength > 0 Then Sleep(SleepLength);
    // in manual mode, the user is supposed to make sure messages are handled
    // in Idle mode, this processing is implicit
    If Mode = cmASAP Then
    Begin
      //Application.ProcessMessages;
      If (Not Assigned(vASAPCadencerList)) Or (vASAPCadencerList.IndexOf(Self) < 0) Then Exit;
    End;

  End;
  Inc(FProgressing);
  Try
    If Enabled Then
    Begin
      // One of the processed messages might have disabled us
      If Enabled Then
      Begin
        // ...and progress !
        newTime := GetCurrenttime;
        deltaTime := newTime - lastTime;
        If (deltaTime >= MinDeltaTime) And (deltaTime >= FixedDeltaTime) Then
        Begin
          If FMaxDeltaTime > 0 Then
          Begin
            If deltaTime > FMaxDeltaTime Then
            Begin
              FOriginTime := FOriginTime + (deltaTime - FMaxDeltaTime) * FInvTimeMultiplier;
              deltaTime := FMaxDeltaTime;
              newTime := lastTime + deltaTime;
            End;
          End;
          totalDelta := deltaTime;
          fullTotalDelta := totalDelta;
          firstLastTime := lastTime;
          If FixedDeltaTime > 0 Then deltaTime := FixedDeltaTime;
          While totalDelta >= deltaTime Do
          Begin
            lastTime := lastTime + deltaTime;
            If (deltaTime <> 0) Then   //Assigned(FScene) And
            Begin
              FProgressing := -FProgressing;
              Try
               // FScene.Progress(deltaTime, lastTime);
              Finally
                FProgressing := -FProgressing;
              End;
            End;
            pt.deltaTime := deltaTime;
            pt.newTime := lastTime;
            i := 0;
            While Assigned(FSubscribedCadenceableComponents) And (i <= FSubscribedCadenceableComponents.Count - 1) Do
            Begin
              TBZCadenceAbleComponent(FSubscribedCadenceableComponents[i]).DoProgress(pt);
              inc(i); //i := i + 1;
            End;
            If Assigned(FOnProgress) And (Not (csDesigning In ComponentState)) Then FOnProgress(Self, deltaTime, newTime);
            If deltaTime <= 0 Then Break;
            totalDelta := totalDelta - deltaTime;
          End;
          If Assigned(FOnTotalProgress) And (Not (csDesigning In ComponentState)) Then FOnTotalProgress(Self, fullTotalDelta, firstLastTime);
        End;
      End;
    End;
  Finally
    Dec(FProgressing);
  End;
End;

Function TBZCadencer.GetRawReferenceTime: Double;
Var
  counter: Int64;
Begin
  counter := 0;
  Case FTimeReference Of
    cmRTC: // Real Time Clock
      Result := Now * (3600 * 24);
    cmPerformanceCounter:
    Begin // HiRes Performance Counter
      QueryPerformanceCounter(counter);
      Result := counter / vCounterFrequency;
    End;
    cmExternal: // User defined value
      Result := FCurrentTime;
    Else
      Result := 0;
      Assert(False);
  End;
End;

Function TBZCadencer.GetCurrenttime: Double;
Begin
  Result := (GetRawReferenceTime - FOriginTime) * FTimeMultiplier;
  FCurrentTime := Result;
End;

Function TBZCadencer.IsBusy: Boolean;
Begin
  Result := (FProgressing <> 0);
End;

Procedure TBZCadencer.Reset;
Begin
  lasttime := 0;
  downTime := GetRawReferenceTime;
  FOriginTime := downTime;
End;

Procedure TBZCadencer.SetCurrentTime(Const Value: Double);
Begin
  LastTime := Value - (FCurrentTime - LastTime);
  FOriginTime := FOriginTime + (FCurrentTime - Value);
  FCurrentTime := Value;
End;

{%endregion%}

{%region%=== [ TBZCustomCadencedComponent ]===================================}

Destructor TBZCustomCadencedComponent.Destroy;
Begin
  Cadencer := nil;
  Inherited Destroy;
End;

Procedure TBZCustomCadencedComponent.Notification(AComponent: TComponent; Operation: TOperation);
Begin
  If (Operation = opRemove) And (AComponent = FCadencer) Then Cadencer := nil;
  Inherited;
End;

Procedure TBZCustomCadencedComponent.SetCadencer(Const val: TBZCadencer);
Begin
  If FCadencer <> val Then
  Begin
    If Assigned(FCadencer) Then
      FCadencer.UnSubscribe(Self);
    FCadencer := val;
    If Assigned(FCadencer) Then
      FCadencer.Subscribe(Self);
  End;
End;

{%endregion%}

Initialization

  RegisterClasses([TBZCadencer]);
  vCounterFrequency := 0;
  // Preparation pour le "timer" haute resolution
  If Not QueryPerformanceFrequency(vCounterFrequency) Then vCounterFrequency := 0;

Finalization
  FreeAndNil(vHandler);
  FreeAndNil(vASAPCadencerList);
End.










