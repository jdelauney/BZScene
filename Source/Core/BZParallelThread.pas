(*
  Contient un objet TBZParallelThread qui permet d'exécuter des opérations
  en parallele facilement grâce au multithreading.

  -------------------------------------------------------------------------------------------------------------

  @created(24/02/2019)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(Creation : 24/06/2019)
    @item(Mise à jour : )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes :)@br
  Lazarus dispose de cette possibilité nativement, mais est un peu plus compliqué à mettre en place.
  cf : https://wiki.freepascal.org/Parallel_procedures/fr

  Inspirer du code source de Paul Toth pour Delphi http://lookinside.free.fr/delphi.php?Multithreading)

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
unit BZParallelThread;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils;

Type

  { Type de procedure simple d'execution du tratement à effectuer dans les threads }
  TBZParallelProc = procedure(Index: Integer; Data : Pointer);
  { Type de procedure objet d'execution du tratement à effectuer dans les threads }
  TBZParallelProcEvent = procedure(Sender: TObject; Index: Integer ; Data : Pointer) of object;
  { Type de procedure objet pour la progression de l'execution }
  TBZParallelProgressEvent = procedure(Sender: TObject; Index: Integer) of object;

  { Direction de traitement des données. @br
    Vers l'avant (Incrementation de l'index) ou vers l'arrière (Décrementation de l'index). }
  TBZParallelProgressMode = (ppmForward, ppmBackward);
  { Objet servant à l'initialisation du code à paralleliser dans des threads en mode objet }
  TBZParallelThread = class
  private
    FThreadCount : Integer;
    FSystemThreadCount : Integer;
    FTaskCount   : Integer;

    FCurrentIndex : Integer;

    FExecuteMode : TBZParallelProgressMode;

    //FFromIndex, FToIndex : Int64;
    FCount  : Integer;

    FData : Pointer;

    FOnExecute : TBZParallelProcEvent;
    FOnProgress : TBZParallelProgressEvent;


    procedure SetThreadCount(const AValue : Integer);

  protected
    procedure ExecuteProgressProc; virtual;
    procedure ExecuteProgressProcWithData; virtual;
  public
    { Creation }
    constructor Create; overload;
    { Destruction }
    constructor Create(DataPtr : Pointer);

    { Renvoie un pointeur sur les données secondaire }
    function GetData : Pointer;
    { Lancement du traitement à affectuer. Le paramètre "Count" désigne le nombre de donnée totale à traiter }
    procedure Run(Count: Int64); overload;
    { Lancement du traitement à affectuer à partir de l'index "FromIndex" jusqu'à "ToIndex". @br
      Si FromIndex est plus grand que ToIndex alors l'avancer du traitement se fera vers l'arrière (de ToIndex jusqu'à FromIndex) }
    procedure Run(FromIndex, ToIndex : Int64); overload;

    { Retourne le nombre de threads maximum que le processeur peux gérer }
    property SystemThreadCount : Integer read FSystemThreadCount;
    { Nombre de threads à allouer au traitement. Note : il est préférable de ne pas dépasser SystemThreadCount }
    property ThreadCount : Integer Read FThreadCount write SetThreadCount;

    { Evenement déclencher pour le traitement de chaque données }
    property OnExecute: TBZParallelProcEvent read FOnExecute write FOnExecute;
    { Evenement pour controler la progression de la tâche }
    property OnProgress: TBZParallelProgressEvent read FOnProgress write FOnProgress;

  end;

{ Procedure de lancement d'un traitement paralléliser simple. @br
  "Data" est un pointer permettant d'acceder à des paramètres ou valeurs nécessaire au traitement  }
procedure ParallelFor(FromIndex, ToIndex : Int64; ParallelProc : TBZParallelProc;Const DataPtr : Pointer = nil; Const ThreadCount : Byte = 4); overload;

{ Procedure de lancement d'un traitement paralléliser objet }
procedure ParallelFor(FromIndex, ToIndex : Int64; ParallelProc : TBZParallelProcEvent; const ThreadCount : Byte = 4); overload;
{ Procedure de lancement d'un traitement paralléliser objet avec data }
procedure ParallelFor(FromIndex, ToIndex : Int64; ParallelProc : TBZParallelProcEvent; Const DataPtr : Pointer; const ThreadCount : Byte = 4); overload;
{ Procedure de lancement d'un traitement paralléliser objet avec data et évènement pour controler la progression de la tâche }
procedure ParallelFor(FromIndex, ToIndex : Int64; ParallelProc : TBZParallelProcEvent; ProgressEvent : TBZParallelProgressEvent; Const DataPtr : Pointer; const ThreadCount : Byte = 4); overload;

threadvar
  { Variable globale servant à contenir le thread en cours d'execution. @br
    Utile dans le cas de synchronisation. Vous pouvez la définir dans vos propres descendants TThread }
  BZCurrentParallelThread : TThread;

implementation

uses BZSystem, BZLogger;


type
  // Thread pour la parallelisation en mode objet

  { TBZParallelTask }

  TBZParallelTask = class(TThread)
  private
    FData : Pointer;
    FID : Integer;
    FParallelThread : TBZParallelThread;
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    constructor Create(AParallelThread : TBZParallelThread; anID : Integer); overload;
  end;

type
  // Thread pour la parallelisation en mode simple
  TBZDirectParallelTask = Class;

  { TBZDirectParallelRunner : Gestionnaire de threads pour la parallelisation en mode simple}
  TBZDirectParallelRunner = Class
  private

    FData : Pointer;
    FThreadCount : Byte;
    FWorkers : array of TBZDirectParallelTask;
  public
    MaxIndex, CurrentIndex : Integer;

    Constructor Create(Count : Integer; Const Data : Pointer = nil; Const ThreadCount : Byte = 4);
    Destructor Destroy; override;

    procedure Run(FromIndex : Integer; aParallelProc : TBZParallelProc);


    property Data : Pointer read FData write FData;
  end;

  { TBZDirectParallelTask }
  TBZDirectParallelTask = class(TThread)
  private
    FID : Integer;
    FRunner : TBZDirectParallelRunner;
    FParallelProc : TBZParallelProc;
  protected
    procedure Execute; override;
  public
    constructor Create(Runner : TBZDirectParallelRunner; AParallelProc : TBZParallelProc; anID : Integer);
  end;

{%region=====[ TBZDirectParallelRunner ]================================================}

constructor TBZDirectParallelRunner.Create(Count : Integer; const Data : Pointer; const ThreadCount : Byte);
begin
  MaxIndex := Count;
  FData := Data;
  FThreadCount := ThreadCount;
  SetLength(FWorkers, ThreadCount);
end;

destructor TBZDirectParallelRunner.Destroy;
Var
  i : Byte;
begin
  SetLength(FWorkers, 0);
  FWorkers := nil;
  inherited Destroy;
end;

procedure TBZDirectParallelRunner.Run(FromIndex : Integer; aParallelProc : TBZParallelProc);
Var
  I: integer;
begin
  CurrentIndex := FromIndex;

  for I := 0 to FThreadCount - 1 do
    FWorkers[I] := TBZDirectParallelTask.Create(Self, aParallelProc, I+1);

  while CurrentIndex < MaxIndex do sleep(0);  //sleep(100);
end;

{%endregion%}

{%region=====[ TBZDirectParallelTask ===================================================}

constructor TBZDirectParallelTask.Create(Runner : TBZDirectParallelRunner; AParallelProc : TBZParallelProc; anID : Integer);
begin
  inherited Create(false);
  FParallelProc := AParallelProc;
  FRunner := Runner;
  FID := anID;
  FreeOnTerminate := True;
end;

procedure TBZDirectParallelTask.Execute;
var
  Index: integer;
begin
  BZCurrentParallelThread := Self;
  while not Terminated do
  begin
    Index := InterlockedIncrement(FRunner.CurrentIndex);
    if Index > FRunner.MaxIndex then break;
    FParallelProc(Index, FRunner.Data);
  end;
end;

{%endregion%}

{%region=====[ TBZParallelTask =========================================================}

constructor TBZParallelTask.Create(AParallelThread : TBZParallelThread; anID : Integer);
begin
  inherited Create(False);
  FParallelThread := AParallelThread;
  FID := anID;
  FData := FParallelThread.GetData;
  FreeOnTerminate := True;
end;

procedure TBZParallelTask.Execute;
begin
  BZCurrentParallelThread := Self;
  if FData = nil then FParallelThread.ExecuteProgressProc else FParallelThread.ExecuteProgressProcWithData;
end;

procedure TBZParallelTask.DoTerminate;
begin
  inherited DoTerminate;
 // BZCurrentParallelThread := nil;
end;

{%endregion%}

{%region=====[ TBZParallelThread ]======================================================}

constructor TBZParallelThread.Create;
begin
  inherited;
  FSystemThreadCount := GetProcessorCount;
  FThreadCount := FSystemThreadCount;
  FData := nil;
end;

constructor TBZParallelThread.Create(DataPtr : Pointer);
begin
  Create;
  FData := DataPtr;
end;

function TBZParallelThread.GetData : Pointer;
begin
  Result := FData;
end;

procedure TBZParallelThread.SetThreadCount(const AValue : Integer);
begin
  if FThreadCount = AValue then Exit;
  Assert((AValue < FSystemThreadCount),'It is not recommended to have more threads than your cpu can support');
  FThreadCount := AValue;
  if FThreadCount < 1 then FThreadCount := 1; //FSystemThreadCount;
end;

procedure TBZParallelThread.Run(FromIndex, ToIndex : Int64);
var
  I: Word;
begin
  if not Assigned(FOnExecute) then Exit;
  FTaskCount := FThreadCount;

  if ToIndex > FromIndex then
  begin
    FExecuteMode := ppmForward;
    FCurrentIndex := FromIndex-1;
    FCount := Abs(ToIndex - FromIndex)+1; // 319 - 0 = 320 ; 20 - 10 = 11
  end
  else
  begin
    FExecuteMode := ppmBackward;
    FCurrentIndex := ToIndex+1;
    FCount := FromIndex+1;
  end;

  for I := 1 to FThreadCount - 1 do
  begin
    TBZParallelTask.Create(Self, I+1);
  end;

  if FData = nil then ExecuteProgressProc else ExecuteProgressProcWithData;

  while FTaskCount > 0 do Sleep(0);
end;

procedure TBZParallelThread.Run(Count: Int64);
var
  I: Word;
begin
  if not Assigned(FOnExecute) then Exit;
  FTaskCount := FThreadCount;
  FCurrentIndex := -1;
  FCount := Count;
  FExecuteMode := ppmForward;
  for I := 1 to FThreadCount - 1 do
  begin
    TBZParallelTask.Create(Self,i+1);
  end;

  if FData = nil then ExecuteProgressProc else ExecuteProgressProcWithData;

  while FTaskCount > 0 do Sleep(0);
end;

procedure TBZParallelThread.ExecuteProgressProc;
var
  Index: Integer;
begin
  try
    if FExecuteMode = ppmForward then
    begin
      Index := InterlockedIncrement(FCurrentIndex);
      while Index < FCount do
      begin
        If Index > FCount then Break;
        if FOnProgress<>nil then  FOnProgress(Self, Index);
        //TThread.Synchronize(BZCurrentParallelThread,@FOnProgress);
        FOnExecute(Self, Index,nil);
        Index := InterlockedIncrement(FCurrentIndex);
      end;
    end
    else
    Begin
      Index := InterlockedDecrement(FCurrentIndex);
      while Index > FCount do
      begin
        If Index < FCount then Break;
        if FOnProgress<>nil then FOnProgress(Self, FCount-Index);
        FOnExecute(Self, Index,nil);
        Index := InterlockedDecrement(FCurrentIndex);
      end;
    end;
  finally
    InterlockedDecrement(FTaskCount);
  end;
end;

procedure TBZParallelThread.ExecuteProgressProcWithData;
var
  Index: Integer;
begin
  try
    if FExecuteMode = ppmForward then
    begin
      Index := InterlockedIncrement(FCurrentIndex);
      while Index < FCount do
      begin
        If Index > FCount then Break;
        if FOnProgress<>nil then FOnProgress(Self, Index);
        FOnExecute(Self, Index,FData);
        Index := InterlockedIncrement(FCurrentIndex);
      end;
    end
    else
    Begin
      Index := InterlockedDecrement(FCurrentIndex);
      while Index > FCount do
      begin
        If Index < FCount then Break;
        if FOnProgress<>nil then FOnProgress(Self, FCount-Index);
        FOnExecute(Self, Index,FData);
        Index := InterlockedDecrement(FCurrentIndex);
      end;
    end;
  finally
    InterlockedDecrement(FTaskCount);
  end;
end;

{%endregion%}

procedure ParallelFor(FromIndex, ToIndex : Int64; ParallelProc : TBZParallelProc; const DataPtr : Pointer; const ThreadCount : Byte);
Var
  ThreadRunner : TBZDirectParallelRunner;
begin
  ThreadRunner := TBZDirectParallelRunner.Create(ToIndex,DataPtr,ThreadCount);
  Try
    ThreadRunner.Run(FromIndex, ParallelProc);
  finally
    FreeAndNil(ThreadRunner);
  end;
end;

procedure ParallelFor(FromIndex, ToIndex : Int64; ParallelProc : TBZParallelProcEvent; const ThreadCount : Byte);
Var
  Parallelizer : TBZParallelThread;
begin
  Parallelizer := TBZParallelThread.Create;
  try
    Parallelizer.ThreadCount := ThreadCount;
    Parallelizer.OnExecute := ParallelProc;
    Parallelizer.Run(FromIndex, ToIndex);
  finally
    Parallelizer.Free;
  end;
end;

procedure ParallelFor(FromIndex, ToIndex : Int64; ParallelProc : TBZParallelProcEvent; const DataPtr : Pointer; const ThreadCount : Byte);
Var
  Parallelizer : TBZParallelThread;
begin
  Parallelizer := TBZParallelThread.Create(DataPtr);
  try
    Parallelizer.ThreadCount := ThreadCount;
    Parallelizer.OnExecute := ParallelProc;
    Parallelizer.Run(FromIndex, ToIndex);
  finally
    Parallelizer.Free;
  end;
end;

procedure ParallelFor(FromIndex, ToIndex : Int64; ParallelProc : TBZParallelProcEvent; ProgressEvent : TBZParallelProgressEvent; Const DataPtr : Pointer; const ThreadCount : Byte);
Var
  Parallelizer : TBZParallelThread;
begin
  Parallelizer := TBZParallelThread.Create(DataPtr);
  try
    Parallelizer.ThreadCount := ThreadCount;
    Parallelizer.OnExecute := ParallelProc;
    Parallelizer.OnProgress := ProgressEvent;
    Parallelizer.Run(FromIndex, ToIndex);
  finally
    Parallelizer.Free;
  end;
end;

//======================================================================================

initialization
  BZCurrentParallelThread:=nil;


finalization



end.

