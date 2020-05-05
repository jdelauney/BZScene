(*
  @abstract(Contient un composant non-visuel TBZThreadTimer qui est un Timer qui s'execute dans Thread.)

  -------------------------------------------------------------------------------------------------------------

  @created(24/02/2019)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(Creation : 24/02/2019)
    @item(Mise à jour : )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) :

  -------------------------------------------------------------------------------------------------------------

  @bold(Dependances) : Aucune

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits :)@br
  @unorderedList(
    @item(J.Delauney (BeanzMaster)
    @item(Adaptation du composant TJvThreadTimer de la librairie JVCL https://github.com/project-jedi/jvcl))
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / LGPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZThreadTimer;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils,
  {$IFDEF LINUX}
  cthreads,cmem,
  {$ENDIF}
  LResources, LCLProc, LCLType, LCLIntf;

//==============================================================================

type
  { @abstract(Remplacant au composant TTimer compatible Windows et Unix avec une meilleure précision (1 milliseconde).)

    TBZThreadTimer utilise un thread interne et peut avoir une précision d'une milliseconde. @br
    @bold(Note) : Il consomme plus de CPU qu'un TTimer traditionnel }
  TBZThreadTimer = class(TComponent)
  private
    FEnabled: Boolean;
    FInterval: Cardinal;
    FKeepAlive: Boolean;
    FOnTimer: TNotifyEvent;

    FSynchronize: Boolean;
    // FUseCriticalSection:Boolean;
    FAccurate: Boolean;
    // FUseWaitfor:boolean;

    FStreamedEnabled: Boolean;
    FThread: TThread;
{$IFDEF WINDOWS}
    FPriority: TThreadPriority;
{$ENDIF WINDOWS}
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Cardinal);
    procedure SetSynchronize(const Value: Boolean);
    // procedure SetUseCriticalSection(const Value: Boolean);
    procedure SetOnTimer(const Value: TNotifyEvent);
{$IFDEF WINDOWS}
    procedure SetPriority(const Value: TThreadPriority);
{$ENDIF WINDOWS}
    procedure SetKeepAlive(const Value: Boolean);
  protected
    procedure DoOnTimer;
    procedure Loaded; override;
    procedure StopTimer;
    procedure UpdateTimer;
  public
    { Creation }
    constructor Create(AOwner: TComponent); override;
    { Destruction }
    destructor Destroy; override;
    { Accès direct au Thread }
    property Thread: TThread read FThread;

  published
    { Marche/ Arrêt du timer }
    property Enabled: Boolean Read FEnabled Write SetEnabled default False;
    { Interval en milliseconde entre chaque appel à l'évènement OnTimer }
    property Interval: Cardinal Read FInterval Write SetInterval default 1000;
    { Si le drapeau "KeepAlive" est sur True alors l'évènement OnTimer est exécuté en bloucle si non celui si est stoppé à la fin de la tâche à effectuée }
    property KeepAlive: Boolean Read FKeepAlive Write SetKeepAlive default False;
    { Synchronisation de l'évènement OnTimer avec l'application, si non l'appel à l'évènement OnTimer est asynchrone }
    property Synchronize: Boolean Read FSynchronize write SetSynchronize default False;
    { Utiliser la méthode précise pour la calcul du temps }
    property Accurate: Boolean Read FAccurate Write FAccurate default False;
{$IFDEF WINDOWS}
    { Uniquement sous Windows : Définition de la priorité du Thread }
    property Priority: TThreadPriority read FPriority write SetPriority default tpNormal;
{$ENDIF WINDOWS}
    { Evènement déclenché toutes les "Interval" millisecondes }
    property OnTimer: TNotifyEvent Read FOnTimer Write SetOnTimer;

  end;


implementation

// uses SyncObjs;


// =============================================================================
type
  { Le Thread ou l'on va executer notre Timer }
  TBZTimerThread = class(TThread)
  private
    FEvent: PRtlEvent;
    FHasBeenSuspended: Boolean;
    FInterval: Cardinal;
    FTimer: TBZThreadTimer;
{$IFDEF WINDOWS}
    FPriority: TThreadPriority;
{$ENDIF WINDOWS}
    FSynchronizing: Boolean;
    FThreadName: string;
  protected
    procedure DoSuspend;
    procedure Execute; override;
  public
    constructor Create(ATimer: TBZThreadTimer);
    destructor Destroy; override;
    procedure Stop;
    property Interval: Cardinal read FInterval;
    property Timer: TBZThreadTimer read FTimer;
    property ThreadName: String read FThreadName write FThreadName;
    property Synchronizing: Boolean read FSynchronizing;
  end;


function SubtractMin0(const Big, Small: Cardinal): Cardinal;
begin
  if Big <= Small then
    Result := 0
  else
    Result := Big - Small;
end;

procedure TBZTimerThread.DoSuspend;
begin
  FHasBeenSuspended := True;
  Suspended := True;
end;

procedure TBZTimerThread.Execute;
var
  Offset, TickCount: Cardinal;
begin
  // NameThread(ThreadName);
{$IFDEF WINDOWS}
  Priority := FPriority;
{$ENDIF WINDOWS}

  if Not(FTimer.Accurate) then RTLEventWaitFor(FEvent, FInterval);

  while (not Self.Terminated) and (FTimer.Enabled) do
  begin
    FHasBeenSuspended := False;

    TickCount := GetTickCount64;

    if not Terminated then
    begin
      FSynchronizing := True;
      try
        If FTimer.Enabled then
        begin
          if FTimer.Synchronize then
            Synchronize(@FTimer.DoOnTimer)
          else
            FTimer.DoOnTimer;
        end;
      finally
        FSynchronizing := False;
      end;
    end;

    // Determine how much time it took to execute OnTimer event handler. Take a care
    // of wrapping the value returned by GetTickCount API around zero if Windows is
    // run continuously for more than 49.7 days.
    if FHasBeenSuspended then Offset := 0
    else
    begin
      Offset := GetTickCount64;

      if Offset >= TickCount then
        Dec(Offset, TickCount)
      else
        Inc(Offset, High(Cardinal) - TickCount);
    end;
    // RtlEventSetEvent(FEvent);
    // Make sure Offset is less than or equal to FInterval.
    // (rb) Ensure it's atomic, because of KeepAlive
    if Not(FTimer.Accurate) then RTLEventWaitFor(FEvent, SubtractMin0(Interval, Offset));

    if Terminated then  Exit;
  end;
end;


constructor TBZTimerThread.Create(ATimer: TBZThreadTimer);
begin
  inherited Create(False);

  { Manually reset = false; Initial State = false }
  if Not(ATimer.Accurate) then
  begin
    FEvent := RTLEventCreate;
    if FEvent = nil then RaiseLastOSError;

  end;
  FInterval := ATimer.FInterval;
  FTimer := ATimer;
{$IFDEF WINDOWS}
  FPriority := ATimer.Priority; // setting the priority is deferred to Execute()
{$ENDIF WINDOWS}
  FThreadName := Format('%s: %s', [ClassName, ATimer.Name]);
end;

destructor TBZTimerThread.Destroy;
begin
  Stop;
  if Not(FTimer.Accurate) then
    if FEvent <> nil then RTLeventDestroy(FEvent);

  inherited Destroy;
end;

procedure TBZTimerThread.Stop;
begin
  Terminate;
  if Suspended then Suspended := False;
end;

procedure TBZThreadTimer.SetEnabled(const Value: Boolean);
begin
 // if csLoading in ComponentState then FStreamedEnabled := Value
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if FEnabled then UpdateTimer else StopTimer;
  end;
end;

procedure TBZThreadTimer.SetSynchronize(const Value: Boolean);
begin
  if csLoading in ComponentState then  FStreamedEnabled := Value
  else if FSynchronize <> Value then
  begin
   // StopTimer;
    FSynchronize := Value;
    UpdateTimer;
  end;
end;

procedure TBZThreadTimer.SetInterval(const Value: Cardinal);
begin
  if FInterval <> Value then
  begin
    FInterval := Value;
    UpdateTimer;
  end;
end;

procedure TBZThreadTimer.SetKeepAlive(const Value: Boolean);
begin
  if FKeepAlive <> Value then
  begin
    StopTimer;
    FKeepAlive := Value;
    UpdateTimer;
  end;
end;

procedure TBZThreadTimer.SetOnTimer(const Value: TNotifyEvent);
begin
  if @FOnTimer <> @Value then
  begin
    FOnTimer := Value;
    UpdateTimer;
  end;
end;

{$IFDEF WINDOWS}
procedure TBZThreadTimer.SetPriority(const Value: TThreadPriority);
begin
  if FPriority <> Value then
  begin
    FPriority := Value;
    if FThread <> nil then FThread.Priority := FPriority;
  end;
end;
{$ENDIF WINDOWS}

procedure TBZThreadTimer.DoOnTimer;
begin
  if csDestroying in ComponentState then Exit;
  try
    if FEnabled and Assigned(FOnTimer) then FOnTimer(Self);
  except
    if Assigned(ApplicationHandleException) then ApplicationHandleException(Self);
  end;
end;

procedure TBZThreadTimer.Loaded;
begin
  inherited Loaded;
  SetEnabled(FStreamedEnabled);
end;

procedure TBZThreadTimer.StopTimer;
begin
  if FThread <> nil then
  begin
    TBZTimerThread(FThread).Stop;
    if not TBZTimerThread(FThread).Synchronizing then FreeAndNil(FThread)
    else
    begin
      // We can't destroy the thread because it called us through Synchronize()
      // and is waiting for our return. But we need to destroy it after it returned.
      TBZTimerThread(FThread).FreeOnTerminate := True;
      FThread := nil
    end;
  end;
end;

procedure TBZThreadTimer.UpdateTimer;
var
  DoEnable: Boolean;
begin
  if ComponentState  <> [] then Exit;

  if not KeepAlive then StopTimer;

  DoEnable := FEnabled and (Assigned(FOnTimer) and (FInterval > 0));

  if DoEnable then
  begin
    if FThread <> nil then
    begin
      TBZTimerThread(FThread).FInterval := FInterval;
      if FThread.Suspended then FThread.Suspended := False;
    end
    else
      FThread := TBZTimerThread.Create(Self);

    if Assigned(FThread.FatalException) then raise FThread.FatalException;
  end
  else
  begin
    if FThread <> nil then
    begin
      if not (FThread.Suspended) then TBZTimerThread(FThread).DoSuspend;
      if not(FThread.CheckTerminated) then TBZTimerThread(FThread).FInterval := FInterval;
    end;
  end;
end;

constructor TBZThreadTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := False;
  FInterval := 1000;
{$IFDEF WINDOWS}
  FPriority := tpNormal;
{$ENDIF WINDOWS}
  FKeepAlive := True;
  FAccurate := False;
  FSynchronize := False;
end;

destructor TBZThreadTimer.Destroy;
begin
  StopTimer;
  inherited Destroy;
end;

End.
