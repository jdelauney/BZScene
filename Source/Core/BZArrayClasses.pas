(*
  @abstract(Contient des classes génériques pour manipuler des
  tableaux "Array" a travers un pointeur.)

  Contient des classes pour manipuler des tableaux 1D de type Byte, Integer, Single et Double. @br
  Contient des classes pour manipuler des tableaux 2D de type Byte, Integer, Single et Double.

  -------------------------------------------------------------------------------------------------------------

  @created(24/02/2019)
  @author(J.Delauney (BeanzMaster))
  @bold(Historique) : @br
  @unorderedList(
    @item(Creation : 24/02/2019)
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes :)@br

  -------------------------------------------------------------------------------------------------------------

  @bold(Dependances) : BZSceneStrConsts, BZLogger si DEBUGLOG est activé @br

  -------------------------------------------------------------------------------------------------------------

   @bold(Credits :)@br
     @unorderedList(
       @item(J.Delauney (BeanzMaster))
     )

  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / LGPL @br

  ------------------------------------------------------------------------------------------------------------- *)
unit BZArrayClasses;

//==============================================================================
{$mode objfpc}{$H+}

// Options de débogage pour que les logs soient fonctionnels, nous devons définir
// les options de compilations dans les unités des paquets. Car les options relatif à
// un projet ne sont pas propagé dans les unités d'un paquet.
{.$DEFINE DEBUG}
{.$DEFINE DEBUGLOG}

//{$i ..\bzscene_options.inc}  // L'inclusion de ce fichier provoque une erreur de compilation avec cette unité ?????
//==============================================================================

interface

uses
  Classes, SysUtils;

//==============================================================================

const
  { Granularité de croissance minimum d'un tableau }
  cDefaultListGrowthDelta = 16;

  { Lorsque la liste est plus petite que cInsertionSort_CutOff, nous utilisons InsertionSort au lieu d'appeler
   tout autre algorithme de tri, de manière récursive.
   8 et 64 semblent être les limites inférieures et supérieures où les performances se dégradent, donc
   quelque chose entre 16 et 48 ​​donne probablement les meilleures performances
   D'après mes tests 42 est une bonne valeur médiane. }
  cInsertionSort_CutOff   = 42;

  { Lorsque la liste est plus petite que cQuickSort_CutOff, nous utilisons l'algorithme QuickSort au lieu de DualQuickSort }
  cQuickSort_CutOff       = 500;

type
  { Déclaration pour définir un tableau dynamique de single }
  TBZDynSingleArray = Array of Single;
  { Déclaration pour définir un tableau dynamique de single }
  TBZDynIntegerArray = Array of Integer;
  { Déclaration pour définir un tableau dynamique de single }
  TBZDynByteArray = Array of Byte;

  { Ordre de trie Ascendant ou descendant }
  TBZSortOrder = (soAscending, soDescending);

  { Classe de base générique pour gérer un tableau de n'importe quel type, tableau statique et pointeur.
    Fournis quelques fonctions utiles et communes  ajouter, supprimer, accès aux données.
    Prend également en charge l'accès en mode LIFO (Stack)  }
  generic TBZBaseArray<T> = Class
  private
    //F: boolean;
    FTagString : string;

    type
      PT = ^ T;
      TArr = array of T;
      //PArr = ^TArr;

    procedure SetCount(AValue : Int64);

  protected
    {$CODEALIGN RECORDMIN=16}
    FData: TArr;  //Le pointeur de liste de base (non typé)
    {$CODEALIGN RECORDMIN=4}

    FCapacity:Int64;
    FDataSize:Int64;
    FItemSize:Int64; // Doit être défini dans les sous-classes
    FGrowthDelta: Integer;
    FParentData: Pointer;
    FHandle: Int64;

    FRevision: LongWord;
    FCount: Int64;

    FPosition: Int64;
    FFirstDone: Boolean;

    Function GetData: Pointer; inline;
    Function GetDataArray: TArr; inline;

    function GetValue(Position: Int64): T;
    procedure SetValue(Position : Int64; const AValue : T);

    function GetMutable(Position: Int64): PT;
    procedure IncreaseCapacity;

    procedure SetCapacity(NewCapacity: Int64); virtual;

    function CompareValue(Const {%H-}elem1, {%H-}elem2) : Integer; virtual;

    procedure AnyQuickSort( idxL, idxH : Integer; Dir : TBZSortOrder); //CompareValue: TBZArraySortCompareValue); //var SwapBuf : pByte);
    procedure AnyInsertionSort(idxL, idxH : Integer; Dir : TBZSortOrder); //CompareValue: TBZArraySortCompareValue);
    procedure AnyMergeSort(idxL, idxH : Integer; Dir : TBZSortOrder; Stride : Longint; var SwapBuf : PT); //CompareValue: TBZArraySortCompareValue; var SwapBuf : PT);
    procedure AnyDualPivotQuickSort(idxL, idxH : Integer; Dir : TBZSortOrder); // CompareValue : TBZArraySortCompareValue);

  public
    { Creation d'un nouveau tableau vide}
    constructor Create; //override;
    { Creation d'un nouveau tableau de "Reserved" éléments }
    constructor Create(Reserved: Int64); overload;
    { Créer un nouveau tableau avec un nombre d'éléments Réservés et un propriétaire }
    constructor CreateParented(AParentData: Pointer; Reserved: Int64); overload;
    { Libère et détruit le tableau }
    destructor Destroy; override;

    { Renvoie la taille totale du tableau }
    function DataSize: Int64; // size of the list
    { Renvoie la taille d'un élément }
    function ItemSize: Longint;

    { Ajoute un nouvel élément }
    function Add(const Value: T):Int64;
    { Ajoute un nouvel élément si celui-ci n'es pas déja présent dans le tableau. Retourne -1 si l'élément n'est pas ajouté }
    function AddNoDup(const Value: T):Int64;
    { Insert un nouvel élément à la position "Position" }
    procedure Insert(Position: Int64; const Value: T);
    { Efface un  élément à la position "Position" }
    procedure Delete(Position : Int64);
    { Echange la posiotion des éléments "Index1" et "Index2" }
    procedure Exchange(index1, index2: Int64);
    { Inverse le tableau }
    procedure Reverse; inline;

    //procedure AddNulls(nbVals: Cardinal); inline;
    //procedure InsertNulls(Position : Int64; nbVals: Cardinal); inline;

    { Vide la liste sans altérer la capacité. }
    procedure Flush;
    { Vide la liste et libère les éléments }
    procedure Clear;
    { Ajoute un nouvel élément en haut tableau }
    procedure Push(const Value: T);
    { Récupère l'élément en haut du tableau }
    function Pop: T;
    { Retourne le premier élément du tableau }
    function First: T;
    { Retourne le dernier élément du tableau }
    function Last: T;
    { Renvoie l'élément suivant du tableau, relatif à la position actuelle }
    function Next: T;
    { Renvoie l'élément précédent du tableau, relatif à la position actuelle }
    function Prev: T;
    { Renvoie l'élément actuel du tableau, relatif à la position actuelle }
    function Current : T;
    { Déplacer la position actuelle dans le tableau à la prochaine }
    function MoveNext:Boolean;
    { Déplacer la position actuelle dans le tableau à la précédente }
    function MovePrev:Boolean;
    { Déplacer la position actuelle dans le tableau vers le premier élément }
    function MoveFirst:Boolean;
    { Déplacer la position actuelle dans le tableau vers le dernier élément }
    function MoveLast:Boolean;
    { Renvoie la position actuelle dans le tableau }
    function GetPosition : Int64;
    { Se placer à la postion "Pos" en fonction de StartAt@br
      @param(Valeurs possible pour StartAt  :@br
      @unorderedlist(
        @item( 0 = Depuis le début (defaut) )
        @item( 1 = Depuis la position actuelle)
        @item( 2 = En partant de la fin)
      )}
    function Seek(const pos : Int64; const StartAt : Byte = 0) : boolean;
    { Se placer à n'importe quelle position dans le tableau (equivalent a seek(position, 0)}
    function MoveTo(Position:Int64) : Boolean;
    { Vérifie si la fin du tableau est atteinte}
    function IsEndOfArray : Boolean;
    { Retourne l'index de l'élément "SearchItem". Dans le cas ou celui n'existe pas retourne -1 }
    function IndexOf(SearchItem : T): Integer;
    { Renvoie l'élélement suivant de l'élément "anItem", Si l'élément n'existe pas le premier élément du tableau est retourné }
    function GetNextFrom(anItem : T) : T;

    // Array Rasterizer
    // function Scan(CallBack):Boolean;
    // function ScanNext(CallBack):Boolean;
    // function ScanPrev(CallBack):Boolean;

    // function ScanMany(nbItem,CallBack):Boolean;
    // function ScanTo(Position,CallBack):Boolean;

    // function ScanAll(CallBack):Boolean;
    // function ScanRange(From, To, CallBack):Boolean;

    // Array Utils

   // function CompareItems(Index1, index2, CompareValue): Integer; virtual;

    { Trie le tableaux par ordre croissant ou décroissant.
      L'algorithme de tri utilisé dépend du nombre d'éléments en fonction des constantes  cInsertion_CutOff et cQuickSort_CutOff. @br
      Si la liste à un nombre d'éléments inférieur à 42 le tri par insertion est utilisé. @br
      Si la liste à un nombre d'éléments inférieur ou égale à 500 le tri rapide "QuickSort" est utilisé. @br
      Si non, c'est le tri rapide à double pivot "DualPivotQuickSort" qui est utilisé. }
    procedure Sort(Const Direction : TBZSortOrder = soAscending);  virtual;
    { Trie le tableau avec l'algorithme de trie rapide "QuickSort" version "Stable" }
    procedure QuickSort(Const Direction : TBZSortOrder = soAscending); virtual;
    { Trie le tableau avec l'algorithme de trie rapide à double pivot version "Stable", c'est l'algorithme utilisé par défaut par la méthode Sort }
    procedure DualQuickSort(Const Direction : TBZSortOrder = soAscending); virtual;
    { Trie le tableau avec l'algorithme de fusion :  MergeSort version "Stable". Un peu moins rapide que le quicksort. }
    procedure MergeSort(Const Direction : TBZSortOrder = soAscending);  virtual;
    { Trie le tableau avec l'algorithme d'insertion }
    procedure InsertionSort(Const Direction : TBZSortOrder = soAscending); virtual;

    { Mélange les éléments du tableau aléatoirement. Il vous faudra appeler la méthode Randomize en premier }
    procedure Shuffle;

    // procedure Merge(AnotherArray: TBZBaseArray<T>);
    // function Clone : TBZBaseArray<T>;
    // function Extract(From, Nb : Int64): TBZBaseArray<T>;

    // Extra funcs for management
    // function InsertItemsAt(Pos:Int64; AnArray : TBZBaseArray<T>):Boolean;
    // function InsertItemsAtEnd
    // function InsertItemsAtFirst
    // procedure DeleteItems(Index: Int64r; nbVals: Cardinal); inline;

    { Nombre d'éléments dans la liste. Lors de l'attribution d'un nombre, les éléments ajoutés sont réinitialisés à zéro. }
    property Count: Int64 read FCount write SetCount;
    { Capacité du tableau actuel. Non persistant. }
    property Capacity: Int64 read FCapacity write SetCapacity;
    { Granularité de croissance. Pas persistant. }
    property GrowthDelta: Integer read FGrowthDelta write FGrowthDelta;
    { Augmenter de un après chaque changement de contenu. }
    property Revision: LongWord read FRevision write FRevision;
    { Renvoie le propriétaire s'il existe }
    property ParentData : Pointer read FParentData;
    { Renvoie le tableau sous forme de pointeur }
    property Data : Pointer read GetData;
    property DataArray : TArr read GetDataArray;
    { Renvoie le handle du tableau }
    property Handle : Int64 read FHandle;

    { Accès aux éléments du tableau }
    property Items[i : Int64]: T read getValue write SetValue;// default;
    { Accès à l'élément dans le tableau en tant que pointeur générique }
    property Mutable[i : Int64]: PT read getMutable;
    { Tag utilisateur }
    property TagString: string read FTagString write FTagString;
  end;

  { Extension de TBZBaseArray générique pour la gestion de tableaux de 2 dimensions.
    Attention avec des enregistrements, cette clsse peux retourné des index faux. Et je ne sais pas pourquoi !}
  generic TBZBaseArrayMap2D<T> = class(specialize TBZBaseArray<T>)
  private
    function GetValue2D(x, y : Int64): T;
    procedure SetValue2D(x, y : Int64; AValue: T);

  protected
    FRows, FCols : Int64;

  public
    { Creation d'un nouveau tableau vide de "Rows" lignes et "Cols" colonnes }
    constructor Create(Cols, Rows: Int64); overload;
    { Créer un nouveau tableau avec un nombre d'éléments Réservés de "Rows" lignes et "Cols" colonnes et des données propriétaires }
    constructor CreateParented(AParentData: Pointer; Rows, Cols: Int64); overload;

    { Déplacer la position actuelle dans le tableau vers "Row", "Position" }
    function MoveTo(Row : Integer; Position : Integer) : Boolean; overload;

    { Accès aux éléments du tableau }
    property Items[x,y : Int64]: T read GetValue2D write SetValue2D;
    { Retourne le nombre de ligne du tableau }
    property RowCount : Int64 read FRows;
    { Retourne le nombre de colonne du tableau }
    property ColCount : Int64 read FCols;

  end;

  //generic TBZBaseArrayMap3D<T> = class(specialize TBZBaseArray<T>)
  //private
  //  function GetValue3D(x, y, z : Int64): T;
  //  procedure SetValue3D(x, y, z : Int64; AValue: T);
  //published
  //public
  //  constructor Create(Rows, Cols, DCols : Int64); overload;
  //  constructor CreateParented(AParentData: Pointer; Rows, Cols, DCols: Int64); overload;
  //  property Items[x,y,z : Int64]: T read GetValue3D write SetValue3D;
  //end;
  //
  //generic TBZBaseArrayMap4D<T> = class(specialize TBZBaseArray<T>)
  //private
  //  function GetValue4D(x, y, z, w : Int64): T;
  //  procedure SetValue4D(x, y, z, w : Int64; AValue: T);
  //published
  //public
  //  constructor Create(Rows, Cols, DCols, TCols: Int64); overload;
  //  constructor CreateParented(AParentData: Pointer; Rows, Cols, DCols, TCols: Int64); overload;
  //  property Items[x,y,z,w : Int64]: T read GetValue4D write SetValue4D;
  //end;

Type
  { Tableau générique de type Byte à une Dimension }
  generic TBZArrayByte<T> = class(specialize TBZBaseArray<T>);

  { Tableau générique de type Integer à une Dimension }
  generic TBZArrayInt<T> = class(specialize TBZBaseArray<T>);

  { Tableau générique de type Virgule flottante à une Dimension }
  generic TBZArrayFloat<T> = class(specialize TBZBaseArray<T>);

  { Tableau générique de type Byte à deux Dimensions }
  generic TBZArrayMap2DByte<T> = class(specialize TBZBaseArrayMap2D<T>);

  { Tableau générique de type Integer à deux Dimensions }
  generic TBZArrayMap2DInt<T> = class(specialize TBZBaseArrayMap2D<T>);

  { Tableau générique de type Virgule flottante à deux Dimensions }
  generic TBZArrayMap2DFloat<T> = class(specialize TBZBaseArrayMap2D<T>);

  { Liste spécialisée dans le stockage de type Byte }
  TBZByteList = class(specialize TBZArrayByte<Byte>)
  protected
    function CompareValue(Const elem1, elem2) : Integer;  override;
  end;

  { Liste spécialisée dans le stockage de type Integer }
  TBZIntegerList = class(specialize TBZArrayInt<Integer>)
  protected
    function CompareValue(Const elem1, elem2) : Integer;  override;
  end;

  { Liste spécialisée dans le stockage de type Single }
  TBZSingleList = class(specialize TBZArrayFloat<Single>)
  protected
    function CompareValue(Const elem1, elem2) : Integer;  override;
  end;

  { Liste spécialisée dans le stockage de type Double }
  TBZDoubleList = class(specialize TBZArrayFloat<Double>)
  protected
    function CompareValue(Const elem1, elem2) : Integer;  override;
  end;

  { Liste spécialisée à deux dimension dans le stockage de type Byte }
  TBZByte2DMap = class(specialize TBZArrayMap2DByte<Byte>);

  { Liste spécialisée à deux dimension dans le stockage de type Integer }
  TBZInteger2DMap = class(specialize TBZArrayMap2DInt<Integer>);

  { Liste spécialisée à deux dimension dans le stockage de type Single }
  TBZSingle2DMap = class(specialize TBZArrayMap2DFloat<Single>);

  { Liste spécialisée à deux dimension dans le stockage de type Double }
  TBZDouble2DMap = class(specialize TBZArrayMap2DFloat<Double>);

//==============================================================================

implementation

//==============================================================================

  {$IFDEF DEBUG}
  uses
    BZSceneStrConsts
    {$IFDEF DEBUGLOG}
      ,BZLogger, BZTypesHelpers
    {$ENDIF}
    ;
  {$ENDIF}

{%region%=====[ TBZBaseArray ]=================================================}

constructor TBZBaseArray.Create;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.Create');
    {$ENDIF}
  {$ENDIF}
  inherited Create;
  FCapacity:=0;
 // FItemSize:=Sizeof(T); // Must be defined in subclasses  ????
  FGrowthDelta:= cDefaultListGrowthDelta;
  FParentData:=nil;
  FHandle:=0;
  //FIsDirty:=false;
  FRevision:=0;
  FCount:=0;
  FPosition:=0;
  FFirstDone:=false;
end;

constructor TBZBaseArray.Create(Reserved : Int64);
begin
  Create;
  FDataSize:=Reserved;//*ItemSize;
  SetCapacity(Reserved);
  //SetCount(Reserved);
end;

constructor TBZBaseArray.CreateParented(AParentData : Pointer; Reserved : Int64);
begin
  Create(Reserved);
  FParentData := AParentData;
end;

destructor TBZBaseArray.Destroy;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.Destroy');
    {$ENDIF}
  {$ENDIF}
  Clear;
  //SetLength(FData, 0);
  FData := nil;
  inherited Destroy;
end;

procedure TBZBaseArray.SetCount(AValue : Int64);
begin
  {$ifdef DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.SetCount');
      GlobalLogger.LogStatus('>>> AValue = ' + AValue.ToString);
    {$ENDIF}
    Assert(AValue >= 0);
  {$endif}
  if FCount = AValue then Exit;
  if AValue> FCapacity then SetCapacity(AValue);
  FCount := AValue;
  Inc(FRevision);
end;

Function TBZBaseArray.GetData : Pointer;
begin
  Result := @FData;
end;

Function TBZBaseArray.GetDataArray : TArr;
begin
  Result := FData;
end;

function TBZBaseArray.GetValue(Position : Int64) : T;
begin
  {$ifdef DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.GetValue');
      GlobalLogger.LogStatus('>>> Position = ' + Position.ToString);
      if (position > FCount) and (position < 0) then GlobalLogger.LogError(rsIndexOutOfRange);
    {$ENDIF}
    Assert((position < FCount) and (position>=0), rsIndexOutOfRange);
  {$endif}
  Result := FData[Position];
end;

procedure TBZBaseArray.SetValue(Position : Int64; const AValue : T);
begin
  {$ifdef DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.SetValue');
      GlobalLogger.LogStatus('>>> Position = ' + Position.ToString);
      if (position > FCount) and (position < 0) then GlobalLogger.LogError(rsIndexOutOfRange);
    {$ENDIF}
    Assert((position < FCount) and (position>=0), rsIndexOutOfRange);
  {$endif}
  //if FData[Position] = AValue then exit;
  if Position >= FCapacity then IncreaseCapacity;
  FData[Position] := AValue;
end;

function TBZBaseArray.GetMutable(Position : Int64) : PT;
begin
  {$ifdef DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.GetMutable');
      GlobalLogger.LogStatus('>>> Position = ' + Position.ToString);
      if (position > FCount) and (position < 0) then GlobalLogger.LogError(rsIndexOutOfRange);
    {$ENDIF}
    Assert((position < FCount) and (position>=0), rsIndexOutOfRange);
  {$endif}
  Result := @FData[Position];
end;

procedure TBZBaseArray.IncreaseCapacity;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.IncreaseCapacity');
      GlobalLogger.LogStatus('>>>> FCapacity   = '+ FCapacity.ToString);
      GlobalLogger.LogStatus('>>>> FCount      = '+ FCount.ToString);
    {$ENDIF}
  {$ENDIF}
  if FCapacity=0 then SetCapacity(1)
  else
  begin
    if FCount >= FCapacity then SetCapacity(FCapacity+FGrowthDelta);
  end;
end;

procedure TBZBaseArray.SetCapacity(NewCapacity : Int64);
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.SetCapacity');
      GlobalLogger.LogStatus('>>>> NewCapacity = '+ NewCapacity.ToString);
    {$ENDIF}
  {$ENDIF}
  if FCapacity = newCapacity then exit;
  //if FCount < FCapacity then exit;
  FCapacity := newCapacity;
  SetLength(FData, FCapacity);
  Inc(FRevision);
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogStatus('>>>> Revision = ' + FRevision.ToString + ' Capacity = ' + FCapacity.ToString);
    {$ENDIF}
  {$ENDIF}
end;

function TBZBaseArray.CompareValue(Const elem1, elem2) : Integer;
begin
  result := 0;
end;

function TBZBaseArray.DataSize : Int64;
begin
  Result := FCount * ItemSize; //FDataSize;
end;

function TBZBaseArray.ItemSize : Longint;
begin
  Result := Sizeof(T); //FItemSize;
end;

function TBZBaseArray.Add(const Value : T) : Int64;
begin
  Result := FCount;
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.Add');
    {$ENDIF}
  {$ENDIF}
  if Result >= FCapacity then IncreaseCapacity;
  FData[Result] := Value;
  Inc(FCount);
end;

function TBZBaseArray.AddNoDup(const Value : T) : Int64;
Var
  pos : Integer;
  isNew : Boolean;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.AddNoDup');
    {$ENDIF}
  {$ENDIF}
  Result := -1;
  pos := 0;
  isNew := True;
  while ((pos < FCount) and (isNew = true)) do
  begin
    if FData[pos] = Value then isNew := False;
    inc(pos);
  end;
  if IsNew then
  begin
    {$IFDEF DEBUG}
      {$IFDEF DEBUGLOG}
        GlobalLogger.LogHint('>>>> New value');
      {$ENDIF}
    {$ENDIF}
    Result := FCount;
    if Result >= FCapacity then IncreaseCapacity;
    FData[Result] := Value;
    Inc(FCount);
  end;
end;

procedure TBZBaseArray.Insert(Position : Int64; const Value : T);
begin
  {$ifdef DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.Insert');
      GlobalLogger.LogStatus('>>> Position = ' + Position.ToString);
      if (position >= FCount) then GlobalLogger.LogError(rsIndexOutOfRange);
    {$ENDIF}
    Assert(Position < FCount);
  {$endif}

  if FCount = FCapacity then IncreaseCapacity;
  if Position < FCount then
    System.Move(FData[Position], FData[Position + 1], (FCount - Position) * ItemSize);
  FData[Position] := Value;
  Inc(FCount);
end;

procedure TBZBaseArray.Delete(Position : Int64);
begin
  {$ifdef DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.Delete');
      GlobalLogger.LogStatus('>>> Position = ' + Position.ToString);
      if (position > FCount - 1) then GlobalLogger.LogError(rsIndexOutOfRange);
    {$ENDIF}
    Assert(Position < FCount-1);
  {$endif}
  if (Position < (FCount - 1)) then
  begin
    Dec(FCount);
    System.Move(FData[(Position + 1)], FData[Position], (FCount - Position) * ItemSize);
  end
  else Dec(FCount);
  Inc(FRevision);
end;

procedure TBZBaseArray.Exchange(index1, index2 : Int64);
var
  temp : T;
begin
  {$ifdef DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.Exchange');
      GlobalLogger.LogStatus('>>> Index 1 = ' + Index1.ToString);
      GlobalLogger.LogStatus('>>> Index 2 = ' + Index2.ToString);
      if (Index1 > FCount - 1) or (Index2 > FCount - 1) then GlobalLogger.LogError(rsIndexOutOfRange);
    {$ENDIF}
    Assert((Index1 < FCount) and (Index2 < FCount));
  {$endif}
  if Index1 = Index2 then Exit;
  temp := FData[index1];
  FData[index1] := FData[index2];
  FData[index2] := temp;
  Inc(FRevision);
end;

procedure TBZBaseArray.Reverse;
var
  s, e: Integer;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.Reverse');
    {$ENDIF}
  {$ENDIF}
  s := 0;
  e := FCount - 1;
  while s < e do
  begin
    Exchange(s, e);
    Inc(s);
    Dec(e);
  end;
  Inc(FRevision);
end;

//procedure TBZBaseArray.AddNulls(nbVals : Cardinal);
//begin
//
//end;
//
//procedure TBZBaseArray.InsertNulls(Position : Int64; nbVals : Cardinal);
//begin
//
//end;

procedure TBZBaseArray.Flush;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.Flush');
    {$ENDIF}
  {$ENDIF}
  SetCount(0);
end;

procedure TBZBaseArray.Clear;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.Clear');
    {$ENDIF}
  {$ENDIF}
  SetCount(0);
  SetCapacity(0);
end;

//procedure TBZBaseArray.AdjustCapacityToAtLeast(const size: Integer);
//begin
//  if FCapacity < Size then SetCapacity(size);
//end;

procedure TBZBaseArray.Push(const Value : T);
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.Push');
    {$ENDIF}
  {$ENDIF}
  Add(Value);
end;

function TBZBaseArray.Pop : T;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.Pop');
    {$ENDIF}
  {$ENDIF}
  Result := FData[FCount-1];
 Delete(FCount-1);
end;

function TBZBaseArray.First : T;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.First');
    {$ENDIF}
  {$ENDIF}
  Result := FData[0];
end;

function TBZBaseArray.Last : T;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.Last');
    {$ENDIF}
  {$ENDIF}
  Result := Pop;
end;

function TBZBaseArray.Next : T;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.Next');
    {$ENDIF}
  {$ENDIF}
  if (FPosition < FCount) then //Inc(FPosition);
    Result := FData[FPosition + 1]
  else Result := FData[FPosition];
end;

function TBZBaseArray.Prev : T;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.Prev');
    {$ENDIF}
  {$ENDIF}
  if (FPosition > 0) then  //Dec(FPosition);
   Result := FData[FPosition - 1]
  else Result := FData[FPosition];
end;

function TBZBaseArray.Current : T;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.Current');
    {$ENDIF}
  {$ENDIF}
 Result := FData[FPosition];
end;

function TBZBaseArray.MoveNext : Boolean;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.MoveNext');
    {$ENDIF}
  {$ENDIF}
  Result := false;
  if (FPosition >= FCount-1) then exit;
  Result := True;
  Inc(FPosition);
end;

function TBZBaseArray.MovePrev : Boolean;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.MovePrev');
    {$ENDIF}
  {$ENDIF}
  Result := false;
  if (FPosition <= 0 ) then exit;
  Result := True;
  Dec(FPosition);
end;

function TBZBaseArray.MoveFirst : Boolean;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.MoveFirst');
      if FCount <= 0 then GlobalLogger.LogError(rsListOrArrayIsEmpty);
    {$ENDIF}
     Assert(FCount>0);
  {$ENDIF}
  result := true;
  FPosition := 0;
end;

function TBZBaseArray.MoveLast : Boolean;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.MoveLast');
      if FCount <= 0 then GlobalLogger.LogError(rsListOrArrayIsEmpty);
    {$ENDIF}
     Assert(FCount>0);
  {$ENDIF}
  result := true;
  FPosition := FCount-1;
end;

function TBZBaseArray.GetPosition : Int64;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.GetPosition');
      GlobalLogger.LogStatus('>>> Position = ' + FPosition.ToString);
    {$ENDIF}
  {$ENDIF}
  Result := FPosition;
end;

function TBZBaseArray.Seek(const pos : Int64; const StartAt : Byte=0) : boolean;
var
  newpos : Int64;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.Seek');
      GlobalLogger.LogStatus('>>> Position = ' + Pos.ToString);
      if (pos> FCount - 1) then GlobalLogger.LogError(rsIndexOutOfRange);
    {$ENDIF}
     Assert(Pos < FCount);
  {$ENDIF}

  result := true;
  Case StartAt of
    0: newpos := Pos; // From Beginning
    1:
    begin
      newpos := (FPosition-1) + Pos; // From Current positon
      if newpos >= FCount then
      begin
        //newpos := FCount-1;
        result := false;
      end;
    end;
    2:
    begin
      newpos := (FCount-1) - Pos; // From End;
      if newpos=0 then
      begin
        //newpos := 0;
        result := false;
      end;
    end;
    else newpos := pos;
  end;
  if result then FPosition := newpos;
end;

function TBZBaseArray.MoveTo(Position:Int64) : Boolean;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.MoveTo');
    {$ENDIF}
  {$ENDIF}
  result:= Self.Seek(Position, 0);
end;

function TBZBaseArray.IsEndOfArray : Boolean;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.IsEndOfArray');
      GlobalLogger.LogStatus('>>> Position = ' + FPosition.ToString);
      GlobalLogger.LogStatus('>>> Count    = ' + FCount.ToString);
    {$ENDIF}
  {$ENDIF}
  result := (FPosition >= FCount);
end;

function TBZBaseArray.IndexOf(SearchItem : T) : Integer;
var
  i: Integer;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.IndexOf');
    {$ENDIF}
  {$ENDIF}
  if FCount <= 0 then Result := -1
  else
  begin
    Result := -1;
    for i  := 0 to FCount - 1 do
      if FData[i] = SearchItem then
      begin
        Result := i;
        Exit;
      end;
  end;
end;

function TBZBaseArray.GetNextFrom(anItem : T) : T;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.GetNextFrom');
    {$ENDIF}
  {$ENDIF}
  if FCount <= 0 then exit;
  Result := FData[((Self.indexOf(anItem) + 1) mod Self.Count)];
end;

{ Le "Dual Pivot QuickSort" est plus légèrement plus rapide que le QuickSort classique (dans la majorité des cas).
Sur la papier, le DualPivotQuickSort à 1.9nlnn+O(n) comparaisons, ce qui est 5% de moins que les comparaisons 2nlnn+O(n)  de l'algorithme Quicksort classique à pivot unique.
Cependant, il a besoin de 0.6nlnn+O(n) échanges de valeurs alors qu'avec le Quicksort classique il en faut 1/3nlnn+O(n).
Il existe une version en Java optimisée de cet algorithmequi donne de meilleurs résultats, mais celle-ci est complexe à mettre en place
cf : http://hg.openjdk.java.net/jdk8/jdk8/jdk/file/tip/src/share/classes/java/util/DualPivotQuicksort.java }
procedure TBZBaseArray.AnyDualPivotQuickSort(idxL, idxH : Integer; Dir : TBZSortOrder);
var
  idx, lpi, rpi : Integer;
  nb, li, hi  : Integer;
  SwapTemp, p, q, ak : T;

begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.AnyDualPivotQuickSort');
      GlobalLogger.LogStatus('>>> idxL = ' + idxL.ToString);
      GlobalLogger.LogStatus('>>> idxH = ' + idxH.ToString);
    {$ENDIF}
  {$ENDIF}

  if (idxH <= idxL) then Exit;
  nb := idxH - idxL;

  if nb < cInsertionSort_CutOff then
  begin
    AnyInsertionSort(idxL, idxH, Dir);
    Exit;
  end;
  //else if nb <= cQuickSort_CutOff then
  //begin
  //  AnyQuickSort(idxL, idxH, Dir);
  //  Exit;
  //end;

  li := idxL;
  hi := idxH;

  // Debut de la partition
  if Dir = soAscending then
  begin
    if (CompareValue(FData[hi], FData[li]) < 0) then
    begin
      SwapTemp := FData[li];
      FData[li] := FData[hi];
      FData[hi] := SwapTemp;
    end;
  end
  else
  begin
    if (CompareValue(FData[hi], FData[li]) > 0) then
    begin
      SwapTemp := FData[li];
      FData[li] := FData[hi];
      FData[hi] := SwapTemp;
    end;
  end;

  p := FData[li];
  q := FData[hi];

  lpi := li + 1;
  rpi := hi - 1;
  idx := lpi;

  if Dir = soAscending then
  begin
    While (idx <= rpi) do
    begin
      ak := FData[idx];
      if (CompareValue(ak, p) < 0) then
      begin
        FData[idx] := FData[lpi];
        FData[lpi] := ak;
        inc(lpi);
      end
      else if (CompareValue(ak, q) >= 0) then
      begin
        While ((CompareValue(FData[rpi], q) > 0) and (idx < rpi)) do Dec(rpi);
        FData[idx] := FData[rpi];
        FData[rpi] := ak;
        dec(rpi);
        if (CompareValue(ak, p) < 0) then
        begin
          FData[idx] := FData[lpi];
          FData[lpi] := ak;
          inc(lpi);
        end;
      end;
      inc(idx);
    end;
  end
  else
  begin
    While (idx <= rpi) do
    begin
      ak := FData[idx];
      if (CompareValue(ak, p) > 0) then
      begin
        FData[idx] := FData[lpi];
        FData[lpi] := ak;
        inc(lpi);
      end
      else if (CompareValue(ak, q) <= 0) then
      begin
        While ((CompareValue(FData[rpi], q) < 0) and (idx < rpi)) do Dec(rpi);
        FData[idx] := FData[rpi];
        FData[rpi] := ak;
        dec(rpi);
        if (CompareValue(ak, p) > 0) then
        begin
          FData[idx] := FData[lpi];
          FData[lpi] := ak;
          inc(lpi);
        end;
      end;
      inc(idx);
    end;
  end;

  Dec(lpi);
  Inc(rpi);

  SwapTemp := FData[lpi];
  FData[lpi] := FData[li];
  FData[li] := SwapTemp;

  SwapTemp := FData[rpi];
  FData[rpi] := FData[hi];
  FData[hi] := SwapTemp;
  // Fin de la partition

  AnyDualPivotQuickSort(li,lpi - 1, Dir);
  if DIr = soAscending then
  begin
    if (CompareValue(FData[lpi], FData[rpi]) < 0) then AnyDualPivotQuickSort(lpi + 1, rpi - 1, Dir);
  end
  else
  begin
    if (CompareValue(FData[lpi], FData[rpi]) > 0) then AnyDualPivotQuickSort(lpi + 1, rpi - 1, Dir);
  end;
  AnyDualPivotQuickSort(rpi + 1, hi, Dir);
end;

procedure TBZBaseArray.AnyQuickSort(idxL, idxH : Integer; Dir : TBZSortOrder);
var
  li, hi : Integer;
  TP : Integer;
  mi    : Integer;
  SwapBuf : T;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.AnyQuickSort');
      GlobalLogger.LogStatus('>>> idxL = ' + idxL.ToString);
      GlobalLogger.LogStatus('>>> idxH = ' + idxH.ToString);
    {$ENDIF}
  {$ENDIF}

  if (idxL >= idxH) then exit;

  if (idxH - idxL) < cInsertionSort_CutOff then
  begin
    AnyInsertionSort(IdxL, idxH, Dir);//, CompareValue);
    Exit;
  end;

  if idxL<>0 then mi := (idxL + idxH) div 2
  else mi := (((idxL+1) + idxH) div 2) - 1;
  li := idxL;
  hi := idxH;

  SwapBuf := FData[li];
  FData[li] := FData[mi];
  FData[mi] := SwapBuf;

  TP := idxL;
  inc(li);


  if dir = soAscending then
  begin
    repeat
      if (CompareValue( FData[li], FData[idxL] ) < 0) then
      begin
        inc(TP);
        SwapBuf := FData[TP];
        FData[TP] := FData[li];
        FData[li] := SwapBuf;
      end;
      inc(li);
    until li>hi;
  end
  else
  begin
    repeat
      if (CompareValue( FData[li], FData[idxL] ) > 0) then
      begin
        inc(TP);
        SwapBuf := FData[TP];
        FData[TP] := FData[li];
        FData[li] := SwapBuf;
      end;
      inc(li);
    until li>hi;
  end;

  SwapBuf := FData[idxL];
  FData[idxL] := FData[TP];
  FData[TP] := SwapBuf;

  AnyQuickSort(idxL, TP-1, Dir);//, CompareValue);
  AnyQuickSort(TP+1, idxH, Dir);//, CompareValue);
end;

procedure TBZBaseArray.AnyInsertionSort(idxL, idxH : Integer; Dir : TBZSortOrder);
var
  ps, cs : Integer;
  li,hi : Integer;
  SwapBuf : T;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.AnyInsertionSort');
      GlobalLogger.LogStatus('>>> idxL = ' + idxL.ToString);
      GlobalLogger.LogStatus('>>> idxH = ' + idxH.ToString);
    {$ENDIF}
  {$ENDIF}

  if FCount<2 then exit;

  if FCount = 2 then
  begin
    if (CompareValue(FData[1], FData[0]) < 0) then
    begin
      If Dir = soAscending then
      begin
        SwapBuf := FData[0];
        FData[0] := FData[1];
        FData[1] := SwapBuf;
        Exit;
      end
      else
      begin
        Exit;
      end;
    end;
  end;

  li :=idxL + 1;
  hi :=idxH;

  Repeat
   SwapBuf := FData[li]; //Move(pb[ls], SwapBuf^, Stride);
   ps := li;
   cs := ps - 1;
   If Dir = soAscending then
   begin
     While (ps >= 1) and  (CompareValue(SwapBuf, FData[cs]) < 0) do
     begin
       FData[ps] := FData[cs];
       dec(ps);
       dec(cs);
     end;
   end
   else
   begin
     While (ps >= 1) and  (CompareValue(SwapBuf, FData[cs]) > 0) do
     begin
       FData[ps] := FData[cs];
       dec(ps);
       dec(cs);
     end;
   end;
   FData[ps] := SwapBuf;
   inc(li);
  until li > hi;

end;

{ Merge sort : Adapté de : http://alexandrecmachado.blogspot.com/2015/03/merge-sort-for-delphi-revisited.html
  L'algorithme de tri par fusion est STABLE

  "et cette implémentation fonctionne encore mieux que l'algorithme de tri rapide standard (QuickSort) (~ 10% plus rapide dans la plupart des cas)"
  Soit disant l'auteur originel du code mais dans tous mes test celui-ci est toujours resté moins permorfant que le quicksort. }
procedure TBZBaseArray.AnyMergeSort(idxL, idxH : Integer; Dir : TBZSortOrder; Stride : Longint; var SwapBuf : PT);
var
  li,hi : Integer;
  mi    : Integer;
  FirstCount: Integer;
  i, j: Integer;
  ToInx: Integer;
begin

  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.AnyMergeSort');
      GlobalLogger.LogStatus('>>> idxL = ' + idxL.ToString);
      GlobalLogger.LogStatus('>>> idxH = ' + idxH.ToString);
    {$ENDIF}
  {$ENDIF}

  li := idxL;
  hi := idxH;

  // Point pivot milieu
  if li<>0 then mi := (li + hi) div 2
  else mi := (((li+1) + hi) div 2) - 1;

  // Trie la 1ère moitié de la liste, soit avec un tri par fusion, soit,
  // s'il y a assez peu d'éléments, avec un tri par insertion
  if (li < mi) then
  begin
    if (mi - li) < cInsertionSort_CutOff then
    begin
       AnyInsertionSort(li, mi, Dir);//, CompareValue);
    end
    else
    begin
      AnyMergeSort(li, mi, Dir, Stride, SwapBuf); //, CompareValue
    end;
  end;

  // Trie la 2e moitié de la liste
  if (mi < hi) then
  begin
    if (hi - mi) < cInsertionSort_CutOff then
    begin
      AnyInsertionSort(mi, hi, Dir);//, CompareValue);
    end
    else
    begin
      AnyMergeSort(mi, hi, Dir, Stride, SwapBuf); //, CompareValue
    end;
  end;

  // Copier la première moitié de la liste dans notre liste temporaire
  FirstCount := (mi - li);

  Move(FData[li], SwapBuf^, FirstCount * Stride);

  // configure les index: i est l'index de la liste temporaire (c'est-à-dire la
  // première moitié de la liste), j est l'indice de la seconde moitié de la
  // liste, ToInx est l'index dans la fusion où les éléments seront copiés
  i := 0;
  j := mi + 1;
  ToInx := li;

  // maintenant on fusionne les deux listes
  // On répète jusqu'à ce que l'une des listes soit vide...
  while (i < FirstCount) and (j <= hi) do
  begin
    // calcule le plus petit élément à partir des éléments suivants dans les deux listes et le recopie.
    // Et incrémente l'index pertinent.
    if Dir = soAscending then
    begin
      if (CompareValue(SwapBuf[i], FData[j]) < 0) then
      begin
        FData[ToInx] := SwapBuf[i];
        inc(i);
      end
      else
      begin
        FData[ToInx] := FData[j];
        inc(j);
      end;
    end
    else
    begin
      if (CompareValue(SwapBuf[i], FData[j]) > 0) then
      begin
        FData[ToInx] := SwapBuf[i];
        inc(i);
      end
      else
      begin
        FData[ToInx] := FData[j];
        inc(j);
      end;
    end;
    // il y a un élément de plus dans la liste fusionnée
    inc(ToInx);
  end;
  // s'il y a d'autres éléments dans la première liste, on les recopie.
  if (i < FirstCount) then
  begin
    Move(Swapbuf[i], FData[ToInx], (FirstCount - i) *  Stride);
  end;
  // S'il y a d'autres éléments dans la deuxième liste, ils sont déjà en place et nous avons terminé.
  // S'il n'y en a pas, nous avons également fini.
end;

procedure TBZBaseArray.Sort(Const Direction : TBZSortOrder);
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.Sort');
    {$ENDIF}
  {$ENDIF}
  if FCount<2 then exit;
  if FCount < cInsertionSort_CutOff then AnyInsertionSort(0, FCount-1, Direction)
  else if FCount <= cQuickSort_CutOff then AnyQuickSort(0, FCount-1, Direction)
  else AnyDualPivotQuickSort(0, FCount-1, Direction);
end;

procedure TBZBaseArray.QuickSort(Const Direction : TBZSortOrder);
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.QuickSort');
    {$ENDIF}
  {$ENDIF}
  if FCount<2 then exit;
  AnyQuickSort(0, FCount-1, Direction);
end;

procedure TBZBaseArray.DualQuickSort(Const Direction : TBZSortOrder);
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.DualQuickSort');
    {$ENDIF}
  {$ENDIF}
  if FCount<2 then exit;
  AnyDualPivotQuickSort(0, FCount-1, Direction);
end;

procedure TBZBaseArray.MergeSort(Const Direction : TBZSortOrder);
var
 SwapBuf : PT;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.MergeSort');
    {$ENDIF}
  {$ENDIF}
  if FCount<2 then exit;
  SwapBuf := nil;
  GetMem(SwapBuf, (FCount * Sizeof(T)));
  AnyMergeSort(0, FCount-1, Direction, Sizeof(T), SwapBuf);
  FreeMem(SwapBuf);
end;

procedure TBZBaseArray.InsertionSort(Const Direction : TBZSortOrder);
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.InsertionSort');
    {$ENDIF}
  {$ENDIF}
  AnyInsertionSort(0, FCount-1, Direction);
end;

procedure TBZBaseArray.Shuffle;
Var
 SwapBuffer : T;
 I, K, RandIdx : Integer;
begin
  {$IFDEF DEBUG}
    {$IFDEF DEBUGLOG}
      GlobalLogger.LogNotice('>>> TBZBaseArray.Shuffle');
    {$ENDIF}
  {$ENDIF}
  K := FCount - 1;
  for i := 1 to  K do
  begin
    RandIdx := random(K - I + 1) + I; // Distribution uniforme
    SwapBuffer := FData[RandIdx];
    FData[RandIdx] := FData[I];
    FData[I] := SwapBuffer;
  end;
end;

{%endregion%}

{%region%=====[ TBZBaseArrayMap2D ]============================================}

function TBZBaseArrayMap2D.GetValue2D(x, y : Int64) : T;
var
  pos : Int64;
begin
  {$ifdef DEBUG}
    assert((x<FCols) and (y<FRows));
  {$endif}
  //GlobalLogger.LogStatus('x,y : '+Inttostr(x)+', '+Inttostr(y));
  //GlobalLogger.LogStatus('Set to : '+Inttostr(x)+', '+Inttostr(y));
  pos := (y*FCols+x);
  //GlobalLogger.LogStatus('Get at : '+Inttostr(pos));
  Result := FData[pos];
end;

procedure TBZBaseArrayMap2D.SetValue2D(x, y : Int64; AValue : T);
var
  pos : Int64;
begin
  {$ifdef DEBUG}
    assert((x<FCols) and (y<FRows));
  {$endif}
  pos := (y*FCols+x);
  {$ifdef DEBUG}
    //GlobalLogger.LogStatus('NbRows, NbCols : '+Inttostr(FRows)+', '+Inttostr(FCols));
    //GlobalLogger.LogStatus('Set to : '+Inttostr(x)+', '+Inttostr(y));
    //GlobalLogger.LogStatus('Set at : '+Inttostr(pos));
  {$endif}
  //if FData[pos] = AValue then exit;
  FData[pos] := AValue;
end;

constructor TBZBaseArrayMap2D.Create(Cols, Rows : Int64);
begin
  Inherited Create(Rows*Cols);
  FRows := Rows;
  FCols := Cols;
end;

constructor TBZBaseArrayMap2D.CreateParented(AParentData : Pointer; Rows, Cols : Int64);
begin
  Inherited CreateParented(AParentData, Rows*Cols);
end;

function TBZBaseArrayMap2D.MoveTo(Row : Integer; Position : Integer) : Boolean;
var
  pos : Int64;
begin
  pos := (Row*FCols+Position);
  result := Inherited MoveTo(Pos);
end;

{%endregion%}

{%region%=====[ TBZDoubleList ]================================================}

function TBZDoubleList.CompareValue(Const elem1, elem2) : Integer;
var
  i1 : Double absolute elem1;
  i2 : Double absolute elem2;
begin

  if i1 = i2 then Result:=0
  else if i1 < i2 then Result:=-1
  else Result:=1;
end;

{%endregion%}

{%region%=====[ TBZSingleList ]================================================}

function TBZSingleList.CompareValue(Const elem1, elem2) : Integer;
var
  i1 : Single absolute elem1;
  i2 : Single absolute elem2;
begin

  if i1 = i2 then Result:=0
  else if i1 < i2 then Result:=-1
  else Result:=1;
end;

{%endregion%}

{%region%=====[ TBZByteList ]==================================================}

function TBZByteList.CompareValue(Const elem1, elem2) : Integer;
var
  i1 : Byte absolute elem1;
  i2 : Byte absolute elem2;
begin

  if i1 = i2 then Result:=0
  else if i1 < i2 then Result:=-1
  else Result:=1;
end;

{%endregion%}

{%region%=====[ TBZIntegerList ]===============================================}

function TBZIntegerList.CompareValue(Const elem1, elem2) : Integer;
var
  i1 : integer absolute elem1;
  i2 : integer absolute elem2;
begin

  if i1 = i2 then Result:=0
  else if i1 < i2 then Result:=-1
  else Result:=1;
end;

{%endregion%}

//==============================================================================
end.

