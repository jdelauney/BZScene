(*

  @abstract(Contient un ensemble de classes polymorphique ressemblant à 'TCollection'. Ces classes sont à surcharger.)

  -------------------------------------------------------------------------------------------------------------

  @created(2016-11-16)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(16/11/2016 : Creation  )
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes) :

  -------------------------------------------------------------------------------------------------------------

  @bold(Dependances) : BZClasses, BZSystem, BZSceneStrConsts

  -------------------------------------------------------------------------------------------------------------

  @bold(Credits) : @br
   @unorderedList(
     @item (Codé sur une base de GLScene http://www.sourceforge.net/glscene)
     @item ()
   )

  -------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZXCollection;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes,
  SysUtils,
  BZSystem,
  BZClasses;

type
  TBZXCollection = class;

  { Exception levée en cas d'erreur de lecture ou d'ecriture des donnée dans un flux }
  EBZXCollectionFilerException = class(Exception);
 
  { @abstract(Classe de base pour implémenté des objets pour la classe XCollection)

    @bold(NOTES) : @br
    N'oubliez pas de surcharger les méthodes de persistance ReadFromFiler / WriteToFiler
    si vous ajoutez des données dans une sous-classe! @br
    Les sous-classes doivent être "enregistrées" à l'aide de la fonction RegisterXCollectionItemClass
    pour un fonctionnement correct }
  TBZXCollectionItem = class(TBZInterfacedPersistent)
  private
     
    FOwner: TBZXCollection;
    FName: string;

  protected
     
    function GetName: string; virtual;
    procedure SetName(const val: string); virtual;
    function GetOwner: TPersistent; override;

    { : A Surcharger pour écrire les données persistantes de la sous-classe. }
    procedure WriteToFiler(writer: TWriter); virtual;
    { : A Surcharger pour lire les données persistantes de la sous-classe. }
    procedure ReadFromFiler(reader: TReader); virtual;
    { : A surcharger pour effectuer des tâches lorsque le  propriétaire a été chargé. }
    procedure Loaded; dynamic;

    procedure RaiseFilerException(const archiveVersion: integer);

  public
    { Création }
    constructor Create(aOwner: TBZXCollection); virtual;
    { Destruction }
    destructor Destroy; override;

    { Retourne le chemin de la collection }
    function GetNamePath: string; override;
    { Assigne un autre TBZXCollectionItem }
    procedure Assign(Source: TPersistent); override;
    { Déplace l'item vers le haut dans la collection }
    procedure MoveUp;
    { Déplace l'item vers le bas dans la collection }
    procedure MoveDown;
    { Retourne l'index de l'item dans la collection }
    function Index: integer;

    { Retourne une dénomination conviviale pour la classe. @br
      Cette dénomination est utilisée pour choisir une classe dans l'expert IDE. }
    class function FriendlyName: String; virtual; abstract;
    { Retourne une description conviviale de la classe. @br
      Cette dénomination est utilisée pour aider l'utilisateur lors du choix d'une
      Classe dans l'expert IDE. Si elle est vide, elle tire sa valeur de FriendlyName.}
    class function FriendlyDescription: String; virtual;
    { Retourne la catégorie de la classe d'élément. @br
      C'est est une chaîne libre, elle sera utilisée par l'expert IDE pour
      regrouper les éléments de collection et les éléments dans un menu}
    class function ItemCategory: string; virtual;
    { Si @True, un seul TBXCollectionItem est autorisé.

      L'héritage est pris en compte pour la résolution Unique de BXCollectionItem, c'est-à-dire
      qu si TClassA est unique et TClassB est une sous-classe de TClassA, alors,
      Quelle que soit l'unicité de TClassB, TClassA et TClassB ne seront pas autorisée
      à être mélangé (puisque TClassB est un TClassA, et TClassA est unique). @br
      Tenter d'enfreindre les règles d'unicité ne sera pas possible
      au moment du design (dans l'IDE) et déclenchera une exception au moment de l'exécution. }
    class function UniqueItem: Boolean; virtual;
    { Permet à la classe TBXCollectionItem de déterminer si elle doit être autorisée
      à être ajouter à la collection }
    class function CanAddTo(collection: TBZXCollection): Boolean; virtual;

    { Retourne la collection propriétaire }
    property Owner: TBZXCollection read FOwner;
  published
    { Nom de l'item }
    property Name: string read FName write SetName;
  end;

  { Classe de type TBZXCollectionItem }
  TBZXCollectionItemClass = class of TBZXCollectionItem;


  { @abstract(Contient une liste d'objets TBZXCollectionItem.)

    Cette classe ressemble beaucoup à un TCollection, mais est polymorphe. @br
    Il y existe une version très dépouillée d'un TObjectList propriétaire et de la persistance
    classes (XClasses & XLists), si les droits d'auteur sont partiellement levés
    sur les originaux, je vais baser ce code sur eux car ils sont beaucoup plus rapides
    que les listes et les mécanismes de persistance standard. @br
    TBZXCollection supporte le polymorphisme et une compatibilité descendante complète. }
  TBZXCollection = class(TPersistent)
  private
     
    FOwner: TPersistent;
    FList: TList;
    FCount: integer;

    { : Archive Version est utilisé pour mettre à jour la façon dont les éléments de données sont chargés. }
    FArchiveVersion: integer;
  protected
     
    function GetItems(Index: integer): TBZXCollectionItem;
    function GetOwner: TPersistent; override;

    procedure ReadFromFiler(reader: TReader);
    procedure WriteToFiler(writer: TWriter);

  public
    { Création }
    constructor Create(aOwner: TPersistent); virtual;
    { Destruction }
    destructor Destroy; override;

    { Assigne un autre TBZXCollection }
    procedure Assign(Source: TPersistent); override;

    { Méthode exécuter lorsque le chargement de la collection est terminée }
    procedure Loaded;

    { Retoune le chemin de la collection }
    function GetNamePath: string; override;

    { Classe des objets.@br
      Contrairement à TCollection, les éléments peuvent être de ItemsClass ou de TOUTES ses
      sous-classes, c.-à-d. cette fonction est utilisée uniquement pour affirmer votre ajout.
      objets de la bonne classe, et non pour la persistance. }
    class function ItemsClass: TBZXCollectionItemClass; virtual;

    { Ajoute un item à la collection et renvois son Index }
    function Add(anItem: TBZXCollectionItem): integer;
    { Retourne l'item trouvé ou création d'un nouvel item le cas contraire }
    function GetOrCreate(anItem: TBZXCollectionItemClass): TBZXCollectionItem;
    { Supprime l'item dans la collection à l'index donné }
    procedure Delete(Index: integer);
    { Supprime un item dans la collection }
    procedure Remove(anItem: TBZXCollectionItem);
    { Efface la collection }
    procedure Clear;
    { Renvoie l'index du BZXCollectionItem trouvé (ou -1) }
    function IndexOf(anItem: TBZXCollectionItem): integer;
    { Renvoie l'index du premier BZXCollectionItem de la classe donnée trouvée (ou -1) }
    function IndexOfClass(aClass: TBZXCollectionItemClass): integer;
    { Renvoie la première BZXCollection de la classe donnée (ou nil) }
    function GetByClass(aClass: TBZXCollectionItemClass): TBZXCollectionItem;
    { Renvoie l'index du premier BZXCollectionItem du nom donné (ou -1) }
    function IndexOfName(const aName: string): integer;
    {Indique si un objet de la classe donnée peut être ajouté.
     Cette fonction est utilisée pour être appliquer dans un Unique BZXCollection.}
    function CanAdd(aClass: TBZXCollectionItemClass): Boolean; virtual;

    { Acces aux items de la collection }
    property Items[index: integer]: TBZXCollectionItem read GetItems; default;
    { Retourne le nombre d'item présent dans la collection }
    property Count: integer read FCount;
    { Retourne le numero de version de l'archive. Utilisé dans la lecture et l'écriture des données persistantes }
    property archiveVersion: integer read FArchiveVersion;
    { Retourne le propriétaire de la collection }
    property Owner: TPersistent read FOwner write FOwner;
  end;


{ Enregistre un événement à appeler lorsqu'un BZXCollection est détruit. }
procedure RegisterXCollectionDestroyEvent(notifyEvent: TNotifyEvent);

{ Supprime l'événement appeler lorsqu'un BZXCollection est détruit.  }
procedure DeRegisterXCollectionDestroyEvent(notifyEvent: TNotifyEvent);

{ Enregistre une sous-classe TBZXCollectionItem pour les besoins de persistance. }
procedure RegisterXCollectionItemClass(aClass: TBZXCollectionItemClass);

{ Supprime une sous-classe TBZXCollectionItem de la liste. }
procedure UnregisterXCollectionItemClass(aClass: TBZXCollectionItemClass);

{ Récupère un TBZXCollectionItemClass enregistré à partir de son nom de classe. }
function FindXCollectionItemClass(const ClassName: string) : TBZXCollectionItemClass;

{ Crée et retourne une copie de la liste interne des classes TBZXCollectionItem.@br
  La liste retournée doit être libérée par l'appelant, le paramètre définit un ancêtre de@br
  filtre de classe. Si baseClass est vide, TBZXCollectionItem est utilisé comme ancêtre. }
function GetXCollectionItemClassesList(baseClass : TBZXCollectionItemClass = nil): TList;
{ Retourne la liste des classes spécifiée par "baseClass" }
procedure GetXCollectionClassesList(var ClassesList: TList; baseClass: TBZXCollectionItemClass = nil);


implementation

uses
  BZSceneStrConsts;

const
  { "Magic" est une solution de contournement qui nous permettra de savoir quand les
    numero de version sont égal à 0 (équivalent à: archiveVersion mais, n'est pas stockée dans le fichier LFM/DFM }
  MAGIC: array [0 .. 3] of AnsiChar = 'XCOL';

var
  vXCollectionItemClasses: TList;
  vXCollectionDestroyEvent: TNotifyEvent;


{%region=====[ Routines Internes ]=============================================}
//(Utilisés par l'éditeur de BZXCollection)

procedure RegisterXCollectionDestroyEvent(notifyEvent: TNotifyEvent);
begin
  vXCollectionDestroyEvent := notifyEvent;
end;

procedure DeRegisterXCollectionDestroyEvent(notifyEvent: TNotifyEvent);
begin
  vXCollectionDestroyEvent := nil;
end;

procedure RegisterXCollectionItemClass(aClass: TBZXCollectionItemClass);
begin
  if not Assigned(vXCollectionItemClasses) then vXCollectionItemClasses := TList.Create;
  if vXCollectionItemClasses.IndexOf(aClass) < 0 then vXCollectionItemClasses.Add(aClass);
end;

procedure UnregisterXCollectionItemClass(aClass: TBZXCollectionItemClass);
begin
  if not Assigned(vXCollectionItemClasses) then exit;
  if vXCollectionItemClasses.IndexOf(aClass) >= 0 then vXCollectionItemClasses.Remove(aClass);
end;

function FindXCollectionItemClass(const ClassName: string) : TBZXCollectionItemClass;
var
  i: integer;
begin
  result := nil;
  if Assigned(vXCollectionItemClasses) then
  begin
    for i := 0 to vXCollectionItemClasses.Count - 1 do
    begin
      if TBZXCollectionItemClass(vXCollectionItemClasses[i]).ClassName = ClassName then
      begin
        result := TBZXCollectionItemClass(vXCollectionItemClasses[i]);
        Break;
      end;
    end;
  end;
end;

function GetXCollectionItemClassesList(baseClass : TBZXCollectionItemClass = nil): TList;
begin
  result := TList.Create;
  GetXCollectionClassesList(result, baseClass);
end;

procedure GetXCollectionClassesList(var ClassesList: TList; baseClass: TBZXCollectionItemClass = nil);
var
  i: integer;
begin
  if not Assigned(baseClass) then baseClass := TBZXCollectionItem;
  if Assigned(vXCollectionItemClasses) then
  begin
    for i := 0 to vXCollectionItemClasses.Count - 1 do
    begin
      if TBZXCollectionItemClass(vXCollectionItemClasses[i]).InheritsFrom(baseClass) then ClassesList.Add(vXCollectionItemClasses[i]);
    end;
  end;
end;

{%endregion%}

{%region=====[ TBZXCollectionItem ]===========================================}

constructor TBZXCollectionItem.Create(aOwner: TBZXCollection);
begin
  inherited Create;
  FOwner := aOwner;
  if Assigned(aOwner) then
  begin
    Assert(aOwner.CanAdd(TBZXCollectionItemClass(Self.ClassType)),
      'Ajout de ' + Self.ClassName + ' dans ' + aOwner.ClassName +' rejeté.');
    aOwner.FList.Add(Self);
    aOwner.FCount := aOwner.FList.Count;
  end;
end;

destructor TBZXCollectionItem.Destroy;
begin
  if Assigned(FOwner) then
  begin
    FOwner.FList.Remove(Self);
    FOwner.FCount := FOwner.FList.Count;
  end;
  inherited Destroy;
end;

procedure TBZXCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TBZXCollectionItem then
  begin
    FName := TBZXCollectionItem(Source).Name;
  end
  else
    inherited Assign(Source);
end;

procedure TBZXCollectionItem.SetName(const val: string);
begin
  FName := val;
end;

function TBZXCollectionItem.GetOwner: TPersistent;
begin
  result := FOwner;
end;

procedure TBZXCollectionItem.WriteToFiler(writer: TWriter);
begin
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteString(FName);
  end;
end;

procedure TBZXCollectionItem.ReadFromFiler(reader: TReader);
var
  ver: integer;
begin
  with reader do
  begin
    ver := ReadInteger;
    Assert(ver = 0);
    FName := ReadString;
  end;
end;

procedure TBZXCollectionItem.Loaded;
begin
  // does nothing by default
end;

function TBZXCollectionItem.GetName: string;
begin
  result := FName;
end;

function TBZXCollectionItem.GetNamePath: string;
begin
  if FOwner <> nil then
    result := Format('%s[%d]', [FOwner.GetNamePath, Index])
  else
    result := inherited GetNamePath;
end;

procedure TBZXCollectionItem.MoveUp;
var
  i: integer;
begin
  if Assigned(Owner) then
  begin
    i := Owner.FList.IndexOf(Self);
    if i > 0 then Owner.FList.Exchange(i, i - 1);
  end;
end;

procedure TBZXCollectionItem.MoveDown;
var
  i: integer;
begin
  if Assigned(Owner) then
  begin
    i := Owner.FList.IndexOf(Self);
    if cardinal(i) < cardinal(Owner.FList.Count - 1) then Owner.FList.Exchange(i, i + 1);
  end;
end;

function TBZXCollectionItem.Index: integer;
begin
  if Assigned(Owner) then
    result := Owner.FList.IndexOf(Self)
  else
    result := -1;
end;

procedure TBZXCollectionItem.RaiseFilerException(const archiveVersion: integer);
begin
  raise EBZXCollectionFilerException.Create(ClassName + cUnknownArchiveVersion + IntToStr(archiveVersion));
end;

class function TBZXCollectionItem.FriendlyDescription: string;
begin
  result := FriendlyName;
end;

class function TBZXCollectionItem.ItemCategory: string;
begin
  result := '';
end;

class function TBZXCollectionItem.UniqueItem: Boolean;
begin
  result := False;
end;

class function TBZXCollectionItem.CanAddTo(collection: TBZXCollection): Boolean;
begin
  result := True;
end;

{%endregion%}

{%region=====[ TBZXCollection ]===============================================}

constructor TBZXCollection.Create(aOwner: TPersistent);
begin
  inherited Create;
  FOwner := aOwner;
  FList := TList.Create;
end;

destructor TBZXCollection.Destroy;
begin
  if Assigned(vXCollectionDestroyEvent) then vXCollectionDestroyEvent(Self);
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TBZXCollection.Assign(Source: TPersistent);
var
  i: integer;
  srcItem, newItem: TBZXCollectionItem;
begin
  if not Assigned(Source) then
  begin
    Clear;
  end
  else if Source.ClassType = Self.ClassType then
  begin
    Clear;
    FList.Capacity := TBZXCollection(Source).FList.Count;
    for i := 0 to TBZXCollection(Source).Count - 1 do
    begin
      srcItem := TBZXCollectionItem(TBZXCollection(Source).FList[i]);
      newItem := TBZXCollectionItemClass(srcItem.ClassType).Create(Self);
      newItem.Assign(srcItem);
    end;
  end
  else
    inherited Assign(Source);
  FCount := FList.Count;
end;

procedure TBZXCollection.Loaded;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    TBZXCollectionItem(FList[i]).Loaded;
  end;
end;

procedure TBZXCollection.WriteToFiler(writer: TWriter);
var
  i, n: integer;
  classList: TList;
  XCollectionItem: TBZXCollectionItem;
begin
  { Ici, nous écrivons toutes les classes TBZXCollection listés à travers leurs méthodes WriteToFiler, @br
   mais pour pouvoir les restaurer, nous écrivons aussi leur nom de classe, et pour @br
   éviter de gaspiller de l'espace si la même classe apparaît plusieurs fois, nous construisons @br
   une table de recherche en les écrivant, si la classe est nouvelle, le nom est écrit,  @br
   sinon, seul l'index de la table est écrit. @br
   L'utilisation d'une table de recherche globale (au lieu d'une table 'per-WriteData') @br
   pourrait sauver plus d'espace, mais augmenterait aussi les dépendances, et c'est que je ne veux pas. }
  FArchiveVersion := 1;
  classList := TList.Create;
  try
    with writer do
    begin
      // L'en-tête magique et la version d'archive sont toujours écrits à cet endroit
      WriteInteger(PInteger(@MAGIC[0])^);
      WriteInteger(FArchiveVersion);

      WriteInteger(FList.Count);
      for i := 0 to FList.Count - 1 do
      begin
        XCollectionItem := TBZXCollectionItem(FList[i]);
        n := classList.IndexOf(XCollectionItem.ClassType);
        if n < 0 then
        begin
          WriteString(XCollectionItem.ClassName);
          classList.Add(XCollectionItem.ClassType);
        end
        else
          WriteInteger(n);
        XCollectionItem.WriteToFiler(writer);
      end;
    end;
  finally
    classList.Free;
  end;
end;


procedure TBZXCollection.ReadFromFiler(reader: TReader);
var
  vt: TValueType;
  Header: array [0 .. 3] of AnsiChar;
  n, lc, lcnum: integer;
  classList: TList;
  cName: string;
  XCollectionItemClass: TBZXCollectionItemClass;
  XCollectionItem: TBZXCollectionItem;
begin
  Clear;
  classList := TList.Create;
  try
    with reader do
    begin
      // sauvegarder la position actuelle du lecteur, il sera utilisé pour rembobiner le lecteur si le LFM est trop vieux
      try
        vt := NextValue;
        if vt in [vaInt32, vaInt16, vaInt8] then
          PInteger(@Header[0])^ := ReadInteger
        else
        begin
          Read(Header[0], Length(Header));
        end;
      except
        Header[0] := #0;
        Header[1] := #0;
        Header[2] := #0;
        Header[3] := #0;
      end;

      // après avoir lu l'en-tête, nous devons le comparer avec la référence MAGIC
      if (Header[0] = MAGIC[0]) and (Header[1] = MAGIC[1]) and
        (Header[2] = MAGIC[2]) and (Header[3] = MAGIC[3]) then
      begin
        // Si c'est bon on lit juste le numéro de version de l'archive
        FArchiveVersion := ReadInteger;
        lc := ReadInteger;
      end
      else
      begin
        // si l'en-tête est invalide (ancien LFM),
        // On suppose que la version de l'archive est 0 et on revient à la lecture normale
        FArchiveVersion := 0;
        lc := PInteger(@Header[0])^;
      end;

      for n := 1 to lc do
      begin
        if NextValue in [vaString, vaLString] then
        begin
          cName := ReadString;
          XCollectionItemClass := FindXCollectionItemClass(cName);
          Assert(Assigned(XCollectionItemClass),
            'Class ' + cName + ' unknown. Add the relevant unit to your "uses".');
          classList.Add(XCollectionItemClass);
        end
        else
        begin
          lcnum := ReadInteger;
          Assert((lcnum >= 0) and (lcnum < classList.Count), 'Invalid classlistIndex: ' + IntToStr(lcnum));
          XCollectionItemClass := TBZXCollectionItemClass(classList[lcnum]);
        end;

        if Assigned(XCollectionItemClass) then
        begin
          XCollectionItem := XCollectionItemClass.Create(Self);
          XCollectionItem.ReadFromFiler(reader);
        end;
      end;
    end;
  finally
    classList.Free;
  end;
  FCount := FList.Count;
end;

class function TBZXCollection.ItemsClass: TBZXCollectionItemClass;
begin
  result := TBZXCollectionItem;
end;

function TBZXCollection.GetItems(Index: integer): TBZXCollectionItem;
begin
  result := TBZXCollectionItem(FList[index]);
end;

function TBZXCollection.GetOwner: TPersistent;
begin
  result := FOwner;
end;

function TBZXCollection.GetNamePath: string;
var
  s: string;
begin
  result := ClassName;
  if GetOwner = nil then
    exit;
  s := GetOwner.GetNamePath;
  if s = '' then
    exit;
  result := s + '.XCollection';
end;

function TBZXCollection.Add(anItem: TBZXCollectionItem): integer;
begin
  Assert(anItem.InheritsFrom(ItemsClass));
  Assert(CanAdd(TBZXCollectionItemClass(anItem.ClassType)));
  if Assigned(anItem.FOwner) then
  begin
    anItem.FOwner.FList.Remove(anItem);
    anItem.FOwner.FCount := anItem.FOwner.FList.Count;
  end;
  anItem.FOwner := Self;
  result := FList.Add(anItem);
  FCount := FList.Count;
end;

function TBZXCollection.GetOrCreate(anItem: TBZXCollectionItemClass) : TBZXCollectionItem;
var
  i: integer;
begin
  Assert(anItem.InheritsFrom(ItemsClass));
  i := Self.IndexOfClass(anItem);
  if i >= 0 then
    result := TBZXCollectionItem(Self[i])
  else
    result := anItem.Create(Self);
end;


procedure TBZXCollection.Delete(Index: integer);
begin
  Assert(cardinal(index) < cardinal(FList.Count));
  with TBZXCollectionItem(FList[index]) do
  begin
    FOwner := nil;
    Free;
  end;
  FList.Delete(index);
  FCount := FList.Count;
end;

procedure TBZXCollection.Remove(anItem: TBZXCollectionItem);
var
  i: integer;
begin
  i := IndexOf(anItem);
  if i >= 0 then Delete(i);
end;

procedure TBZXCollection.Clear;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    with TBZXCollectionItem(FList[i]) do
    begin
      FOwner := nil;
      Free;
    end;
  end;
  FList.Clear;
  FCount := 0;
end;

function TBZXCollection.IndexOf(anItem: TBZXCollectionItem): integer;
begin
  result := FList.IndexOf(anItem);
end;

function TBZXCollection.IndexOfClass(aClass: TBZXCollectionItemClass): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to FList.Count - 1 do
  begin
    if TBZXCollectionItem(FList[i]) is aClass then
    begin
      result := i;
      Break;
    end;
  end;
end;

function TBZXCollection.GetByClass(aClass: TBZXCollectionItemClass) : TBZXCollectionItem;
var
  i: integer;
begin
  result := nil;
  for i := 0 to FList.Count - 1 do
  begin
    if TBZXCollectionItem(FList[i]) is aClass then
    begin
      result := TBZXCollectionItem(FList[i]);
      Break;
    end;
  end;
end;

function TBZXCollection.IndexOfName(const aName: string): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to FList.Count - 1 do
  begin
    if TBZXCollectionItem(FList[i]).Name = aName then
    begin
      result := i;
      Break;
    end;
  end;
end;

function TBZXCollection.CanAdd(aClass: TBZXCollectionItemClass): Boolean;
var
  i: integer;
  XCollectionItemClass: TBZXCollectionItemClass;
begin
  result := True;

  // Teste si la classe se permet d'être ajoutée à cette collection
  if not aClass.CanAddTo(Self) then
  begin
    result := False;
    exit;
  end;

  // la classe donnée est-elle compatible avec les classes "parente"?
  if aClass.UniqueItem then
  begin
    for i := 0 to Count - 1 do
    begin
      if Items[i] is aClass then
      begin
        result := False;
        Break;
      end;
    end;
  end;
  // les classes parentes sont-elles compatibles avec la classe donnée ?
  if result then
  begin
    for i := 0 to Count - 1 do
    begin
      XCollectionItemClass := TBZXCollectionItemClass(Items[i].ClassType);
      if (XCollectionItemClass.UniqueItem) and aClass.InheritsFrom(XCollectionItemClass) then
      begin
        result := False;
        Break;
      end;
    end;
  end;
end;

{%endregion%}

initialization

finalization

  if Assigned(vXCollectionItemClasses) then vXCollectionItemClasses.Free;

end.
