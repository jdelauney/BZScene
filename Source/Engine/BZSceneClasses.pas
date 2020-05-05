unit BZSceneClasses; //VectorObjectClasses;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}

//==============================================================================

interface

Uses
  Classes, SysUtils,
  BZClasses, BZGraphic, BZColors, BZVectorMath, BZRenderContextInfo;

Type

  { TBZVectorObjectChange utilisé pour décrire uniquement les modifications d'un objet, qui doivent se refléter dans la scène }
  TBZVectorObjectChange = (ocTransformation, ocAbsoluteMatrix, ocInvAbsoluteMatrix, ocStructure);
  TBZVectorObjectChanges = set of TBZVectorObjectChange;

  TBZScene = class;
  TBZCustomVectorObject = class;
  TBZVectorObjectClass = class of  TBZCustomVectorObject;
  TBZBehaviour = class;
  TBZBehaviourClass = class of TGLBehaviour;
  TBZBehaviours   = class;
  TBZObjectEffect = class;
  TBZObjectEffectClass = class of TBZObjectEffect;
  TBZObjectEffects = class;



  { TBZCustomVectorObject : Classe de base pour tous les objets de la scène.
    Un objet de la scène fait partie de la hiérarchie de la scène (chaque objet de la scène peut avoir plusieurs enfants).
    Cette hiérarchie définit principalement les transformations (chaque coordonnée d'un enfant est relative à son parent),
    mais est également utilisée pour le tri en profondeur, la délimitation et  la détermination de la visibilité de l'objet.
    
    Les sous-classes implémentent des objets de scène visuels (conçus pour être visible au moment de l'exécution, comme un cube en 3D ou un rectangle en 2D)
    ou des objets structurels (qui influencent le rendu ou sont utilisés pour diverses manipulations structurelles, comme les ProxyObject).

    Pour ajouter des enfants à l'exécution, utilisez la méthode AddNewChild de TBZCustomVectorObject.
    D’autres méthodes et propriétés de manipulation par les enfants sont fournies (pour parcourir, les déplacer et les supprimer).

     L'utilisation des méthodes habituelles de TComponent n'est pas encouragé. }
  TBZCustomVectorObject = class(TBZCoordinatesUpdateAbleComponent)
  private
    FScene : TBZScene;
    FParent : TBZCustomVectorObject;
	  FChildrens : TBZPersitentObjectList;

    FUpdateCount : Integer;
    FIsCalculating : Boolean;

    FObjectsSorting: TBZSceneObjectsSorting;
    FChanges: TBZVectorObjectChanges;

    FAbsoluteMatrix, FInvAbsoluteMatrix: PMatrix;
    FLocalMatrix: PMatrix;

    //FBehaviours
    //FObjectEffects

	  FVisible : Boolean;
    FPickable: Boolean;
	
    //FBehaviours: TBZBehaviours;
    //FObjectEffects: TBZObjectEffects;
	
	  FTagObject : TObject;
	  FTagFloat : Single;
	  FTagInt : Integer;
	  FHint : String;

	  FOnProgress: TBZProgressEvent;
    FOnAddedToParent: TNotifyEvent;  
    FOnPicked: TNotifyEvent;
  protected
    function GetVisible: Boolean; virtual;
    function GetPickable: Boolean; virtual;
    procedure SetVisible(aValue: Boolean); virtual;
    procedure SetPickable(aValue: Boolean); virtual;

    procedure DoCoordinateChanged(Sender: TGLCustomCoordinates); virtual;
  public
    constructor Create(AOwner: TComponent); override;

    constructor CreateAsChild(aParentOwner: TBZCustomSceneObject);

    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    { Create a new scene object and add it to this object as new child }
    function AddNewChild(AChild: TBZVectorObjectClass): TBZCustomVectorObject; dynamic;
    { Create a new scene object and add it to this object as first child }
    function AddNewChildFirst(AChild: TBZVectorObjectClass): TBZCustomVectorObject; dynamic;

    procedure AddChild(AChild: TBZCustomVectorObject); dynamic;
      
    function HasSubChildren: Boolean;

    procedure DeleteChildren; dynamic;

    procedure Insert(AIndex: Integer; AChild: TBZCustomVectorObject); dynamic;

    {Extrait un objet de la liste des enfants, mais ne le détruit pas.
     Si "KeepChildren" est VRAI, ses enfants seront conservés en tant que nouveaux enfants dans cet objet de la scène. }
    procedure Remove(aChild: TBZCustomVectorObject; keepChildren: Boolean); dynamic;

    function IndexOfChild(aChild: TBZCustomVectorObject): Integer;

    function FindChild(const aName: string; ownChildrenOnly: Boolean):  TBZCustomVectorObject;

    {Cette procédure n'effectue aucune vérification
     Elle appelle directement FChildren.Exchange.
     L'utilisateur doit / peut effectuer des vérifications manuellement. }
    procedure ExchangeChildren(anIndex1, anIndex2: Integer);

    { La version "sécurisée" de la "ExchengeChildren".
      Cette procédure vérifie si les index sont dans la liste. Et si non, aucune exception est levée. }
    procedure ExchangeChildrenSafe(anIndex1, anIndex2: Integer);

    procedure DoProgress(const progressTime: TProgressTimes); override;

    procedure MoveChildUp(anIndex: Integer);
    procedure MoveChildDown(anIndex: Integer);
    procedure MoveChildFirst(anIndex: Integer);
    procedure MoveChildLast(anIndex: Integer);

    procedure MoveTo(newParent: TBZCustomVectorObject); dynamic;
    procedure MoveUp;
    procedure MoveDown;
    procedure MoveFirst;
    procedure MoveLast;

    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;

    procedure StructureChanged; dynamic;
    procedure ClearStructureChanged; virtual;
    procedure TransformationChanged; virtual;
    procedure CoordinateChanged(Sender: TBZCustomCoordinates); virtual;

    property Scene: TBZScene read FScene;
	  property Parent: TBZCustomVectorObject  read FParent write SetParent;
    property Children[Index: Integer]: TBZCustomVectorObject read GetChildObject; default;
    property Count: Integer read GetCount;
    property Index: Integer read GetIndex write SetIndex;	

    property ObjectsSorting: TGLObjectsSorting read FObjectsSorting write

	  property Visible: Boolean read GetVisible write SetVisible default True;
    property Pickable: Boolean read GetPickable write SetPickable default True;
	
	  property OnProgress: TGLProgressEvent read FOnProgress write FOnProgress;
    property OnPicked: TNotifyEvent read FOnPicked write FOnPicked;
    property OnAddedToParent: TNotifyEvent read FOnAddedToParent write FOnAddedToParent;
	
  published
    property TagFloat: Single read FTagFloat write FTagFloat;	
  	property Tag : Integer read FTagInt write FTagInt;
	  property Hint: string read FHint write FHint;
  end;
  
 { TBZCustom2DVectorObject = class(TBZCustomVectorObject)
  private
    FPosition  : TBZCoordinates2D;
	  FScale   : TBZCoordinates2D;
	  //FDirection : TBZCoordinates2D;
    FRotation  : Single

  protected
    function GetPosition : TBZCoordinates2D; virtual;
    procedure SetPosition(Const Value : TBZCoordinates2D); virtual;
    function GetScale : TBZCoordinates2D; virtual;
    procedure SetScale(Const Value : TBZCoordinates2D); virtual;
    function GetRotation : Single; virtual;
    procedure SetRotation(Const Value : Single); virtual;

    procedure DoCoordinateChanged(Sender: TGLCustomCoordinates); override;

  public
    property Position : TBZCoordinates2D read GetPosition write SetPosition;
    property Scale    : TBZCoordinates2D read GetScale write SetScale;
    property Rotation : Single read GetPosition write SetRotation;
  end;

  TBZCustom3DVectorObject
  private
      FPosition  : TBZCoordinates3D;
  	  FScaling   : TBZCoordinates3D;
  	  FDirection : TBZCoordinates3D;
      FRotation  : TBZCoordinates3D;
      FUp        : TBZCoordinates3D;
    protected
       procedure DoCoordinateChanged(Sender: TGLCustomCoordinates); override;
    public

    end;

  {Base class for implementing behaviours in TGLScene.
     Behaviours are regrouped in a collection attached to a TGLBaseSceneObject,
     and are part of the "Progress" chain of events. Behaviours allows clean
     application of time-based alterations to objects (movements, shape or
     texture changes...).
     Since behaviours are implemented as classes, there are basicly two kinds
     of strategies for subclasses :
      stand-alone : the subclass does it all, and holds all necessary data
        (covers animation, inertia etc.)
      proxy : the subclass is an interface to and external, shared operator
        (like gravity, force-field effects etc.)

     Some behaviours may be cooperative (like force-fields affects inertia)
     or unique (e.g. only one inertia behaviour per object).
     NOTES :
      Don't forget to override the ReadFromFiler/WriteToFiler persistence
        methods if you add data in a subclass !
      Subclasses must be registered using the RegisterXCollectionItemClass
        function }
  TGLBaseBehaviour = class(TGLXCollectionItem)
  protected
    procedure SetName(const val :String); override;
    {Override this function to write subclass data. }
    procedure WriteToFiler(writer :TWriter); override;
    {Override this function to read subclass data. }
    procedure ReadFromFiler(reader :TReader); override;
    {Returns the TGLBaseSceneObject on which the behaviour should be applied.
       Does NOT check for nil owners. }
    function OwnerBaseSceneObject :TGLBaseSceneObject;
  public
    constructor Create(aOwner :TGLXCollection); override;
    destructor Destroy; override;
    procedure DoProgress(const progressTime :TProgressTimes); virtual;
  end;

  {Ancestor for non-rendering behaviours.
     This class shall never receive any properties, it's just here to differentiate
     rendereing and non-rendering behaviours. Rendereing behaviours are named
     "TGLObjectEffect", non-rendering effects (like inertia) are simply named
     "TGLBehaviour". }
  TGLBehaviour = class(TGLBaseBehaviour)
  end;

  {Holds a list of TGLBehaviour objects.
     This object expects itself to be owned by a TGLBaseSceneObject.
     As a TGLXCollection (and contrary to a TCollection), this list can contain
     objects of varying class, the only constraint being that they should all
     be TGLBehaviour subclasses. }
  TGLBehaviours = class(TGLXCollection)
  protected
    function GetBehaviour(index :Integer) :TGLBehaviour;
  public
    constructor Create(aOwner :TPersistent); override;
    function GetNamePath :String; override;
    class function ItemsClass :TGLXCollectionItemClass; override;
    property Behaviour[index :Integer] :TGLBehaviour read GetBehaviour; default;
    function CanAdd(aClass :TGLXCollectionItemClass) :Boolean; override;
    procedure DoProgress(const progressTimes :TProgressTimes);
  end;

  {A rendering effect that can be applied to SceneObjects.
     ObjectEffect is a subclass of behaviour that gets a chance to Render
     an object-related special effect.
     TGLObjectEffect should not be used as base class for custom effects,
     instead you should use the following base classes :
      TGLObjectPreEffect is rendered before owner object render
      TGLObjectPostEffect is rendered after the owner object render
      TGLObjectAfterEffect is rendered at the end of the scene rendering
       NOTES :
      Don't forget to override the ReadFromFiler/WriteToFiler persistence
        methods if you add data in a subclass !
      Subclasses must be registered using the RegisterXCollectionItemClass
        function }

  //   TGLObjectEffectClass = class of TGLObjectEffect;

  TGLObjectEffect = class(TGLBaseBehaviour)
  protected
    {Override this function to write subclass data. }
    procedure WriteToFiler(writer :TWriter); override;
    {Override this function to read subclass data. }
    procedure ReadFromFiler(reader :TReader); override;
  public

    procedure Render(var rci :TGLRenderContextInfo); virtual;
  end;

  {An object effect that gets rendered before owner object's render.
     The current OpenGL matrices and material are that of the owner object. }
  TGLObjectPreEffect = class(TGLObjectEffect)
  end;

  {An object effect that gets rendered after owner object's render.
     The current OpenGL matrices and material are that of the owner object. }
  TGLObjectPostEffect = class(TGLObjectEffect)
  end;

  {An object effect that gets rendered at scene's end.
     No particular OpenGL matrices or material should be assumed. }
  TGLObjectAfterEffect = class(TGLObjectEffect)
  end;

  {Holds a list of object effects.
     This object expects itself to be owned by a TGLBaseSceneObject.  }
  TGLObjectEffects = class(TGLXCollection)
  protected
    function GetEffect(index :Integer) :TGLObjectEffect;
  public
    constructor Create(aOwner :TPersistent); override;
    function GetNamePath :String; override;
    class function ItemsClass :TGLXCollectionItemClass; override;
    property ObjectEffect[index :Integer] :TGLObjectEffect read GetEffect; default;
    function CanAdd(aClass :TGLXCollectionItemClass) :Boolean; override;
    procedure DoProgress(const progressTime :TProgressTimes);
    procedure RenderPreEffects(var rci :TGLRenderContextInfo);
    { Also take care of registering after effects with the GLSceneViewer. }
    procedure RenderPostEffects(var rci :TGLRenderContextInfo);
  end;

  TBZCustom2DBaseSceneObject = Class(TBZCustom2DVectorObject)
  private
	  FMaterial : TBZCustomMaterial;
  protected
  public    
    property Scene: TBZBitmapScene read FScene;
    property Position : TBZCoordinates2D read GetPositionCoordinates write SetPositionCoordinates
    property Scale : TBZCoordinates2D read GetScaleCoordinates write SetScaleCoordinates
  	property RotationAngle: Single read GetRotationAngle write SetRotationAngle;
    property Material: TBZBitmapMaterial read GetMaterial write SetMaterial;
  end;

  TBZCustom3DBaseSceneObject = Class(TBZCustom3DVectorObject)
  private
	  FMaterial : TBZCustomMaterial;
  protected
  public
    property Scene: TBZBitmapScene read FScene;
    property Position : TBZCoordinates3D read GetPositionCoordinates write SetPositionCoordinates
    property Scale : TBZCoordinates3D read GetScaleCoordinates write SetScaleCoordinates
  	property RotationAngle: Single read GetRotationAngle write SetRotationAngle;
    property Material: TBZMaterial read GetMaterial write SetMaterial;
  end;
  
  TBZSceneRootObject =  Class(TBZCustomVectorObject)
  public
    constructor Create(AOwner: TComponent); override;
  end;
  
  TBZ2DBaseSceneObject = Class(TBZCustom2DBaseSceneObject)
  published
    property Material;

    property Position;
    property Scale;
    property Rotation;
   
    property Visible;
    property Pickable;
    
	  property Hint;
  	property TagFloat;
	  property Tag;
	
  	property OnProgress;
    property OnPicked;       
  end;
  
  TBZ2DProxyObject = Class(TBZ2DBaseSceneObject)
  private
    FMasterObject : TBZ2DBaseSceneObject;
  protected
  public
  published
    {Specifies the Master object which will be proxy'ed. }
    property MasterObject: TBZ2DBaseSceneObject read FMasterObject write SetMasterObject;
        
    property Position;
    property RotationAngle;
    property Scale;
	
    property Visible;
    property Pickable;
    
	property OnProgress;
    property OnPicked;    
  end;
  
  TBZ2DProxyObjectClass = class of TBZ2DProxyObject;

  TBZCustomCamera = Class(TBZ2DBaseSceneObject)
  private
  protected
  public
  end;
  TBZCameraClasses = class of TBZCustomCamera;

  TBZCamera2D = Class(TBZCustomCamera)
  private
    FPosition : TBZCoordinates2D;
  protected
  public
  published
  end;

  TBZCamera3D = Class(TBZCustomCamera)
  private
    FPosition : TBZCoordinates3D;
  protected
  public
  published
  end;

}

  //TBZCustom3DBaseSceneObject
  //TBZ3DProxyObject
  //TBZ3DProxyObjectClass = class of TBZ3DProxyObject;


  TBZScene = Class(TBZUpdatableObjectComponent)
  private
  protected
  public
  end;

  TBZCustomSceneRender = Class(TBZUpdatableObject)
  private
    // Rendering State

    FRendering : Boolean;
    FBackgroundColor : TBZColor;
    FRenderDPI :Integer;

    // Matrix
    FViewMatrixStack : Array of TMatrix;
    FProjectionMatrixStack : Array of TBZMatrix;
    FBaseProjectionMatrix : TBZMatrix;

    // Camera
    FCameraMode : TBZCameraMode;
    FCamera : TBZCustomCamera;
    FCameraAbsolutePosition : TBZVector;
    FViewPort : TBZRect;

    // Effects
    FAfterRenderEffects : TBZPersistentObjectList;

    // Monitoring
    FFrameCount       : Longint;
    FFramesPerSecond  : Single;
    FFirstPerfCounter : Int64;
    FLastFrameTime    : Single;

    // Events
    FOnChange : TNotifyEvent;
    FOnStructuralChange : TNotifyEvent;
    FOnPrepareContext : TNotifyEvent;

    FOnBeforeRender    : TNotifyEvent;
    FOnViewerBeforeRender : TNotifyEvent;
    FOnPostRender      : TNotifyEvent;
    FOnAfterRender     : TNotifyEvent;
  protected
    procedure DrawLine; virtual; abstract;
    procedure DrawPolygone ;virtual; abstract;
  public
  end;

//   TBZCustomNonVisualSceneRender = class(TComponent)
//   TBZSceneMemoryRender = Class(TBZNonVisualSceneRender)
  
implementation

//const
//  CsVectorHelp =
//    'If you are getting assertions here, consider using the SetPoint procedure';
//  CsPointHelp =
//    'If you are getting assertions here, consider using the SetVector procedure';
//  CsPoint2DHelp =
//    'If you are getting assertions here, consider using one of the SetVector or SetPoint procedures';


{%region=====[ TBZCustomVectorObject ]===========================================================================}

constructor TBZCustomVectorObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  //FPosition := TGLCoordinates.CreateInitialized(Self, NullHmgPoint, csPoint);
  //FRotation := TGLCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
  //FDirection := TGLCoordinates.CreateInitialized(Self, ZHmgVector, csVector);

  FVisible := True;
  FPickable := True;
  FObjectsSorting := osInherited;
  FIsCalculating := False;

end;

constructor TBZCustomVectorObject.CreateAsChild(aParentOwner: TBZCustomVectorObject);
begin
  Create(aParentOwner);
  aParentOwner.AddChild(Self);
end;

destructor TBZCustomVectorObject.Destroy;
begin
  DeleteChildCameras;
  //FGLObjectEffects.Free;
  //FGLBehaviours.Free;
  //FListHandle.Free;
  //FPosition.Free;
  //FRotation.Free;
  //FDirection.Free;
  //FUp.Free;
  //FScaling.Free;
  if Assigned(FParent) then FParent.Remove(Self, False);
  if Assigned(FChildren) then
  begin
    DeleteChildren;
    FChildren.Free;
  end;
  inherited Destroy;
end;

procedure TBZCustomVectorObject.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TBZCustomVectorObject.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then NotifyChange(Self);
  end
  else
    Assert(False, bzsUnBalancedBeginEndUpdate);
end;

procedure TBZCustomVectorObject.DeleteChildCameras;
var
  i: Integer;
  child: TBZCustomVectorObject;
begin
  i := 0;
  if Assigned(FChildren) then
  begin
    while i < FChildren.Count do
    begin
      child := TBZCustomVectorObject(FChildren.List^[i]);
      child.DeleteChildCameras;
      if (child is TBZCamera2D) or (child is TBZCamera3D) then
      begin
        Remove(child, True);
        child.Free;
      end
      else
        Inc(i);
    end;
  end;
end;

procedure TBZCustomVectorObject.DeleteChildren;
var
  child: TBZCustomVectorObject;
begin
  DeleteChildCameras;
  if Assigned(FScene) then FScene.RemoveLights(Self);
  if Assigned(FChildren) then
  begin
    while FChildren.Count > 0 do
    begin
      child := TBZCustomVectorObject(FChildren.Pop);
      child.FParent := nil;
      child.Free;
    end;
  end;
  //BBChanges := BBChanges + [oBBcChild];
end;

procedure TBZCustomVectorObject.Loaded;
begin
  inherited;
  //FPosition.W := 1;
  //if Assigned(FGLBehaviours) then
  //  FGLBehaviours.Loaded;
  //if Assigned(FGLObjectEffects) then
  //  FGLObjectEffects.Loaded;
end;

procedure TBZCustomVectorObject.DefineProperties(Filer: TFiler);
begin
  inherited;
  {FOriginalFiler := Filer;}

  //Filer.DefineBinaryProperty('BehavioursData',
  //  ReadBehaviours, WriteBehaviours,
  //  (Assigned(FGLBehaviours) and (FGLBehaviours.Count > 0)));
  //Filer.DefineBinaryProperty('EffectsData',
  //  ReadEffects, WriteEffects,
  //  (Assigned(FGLObjectEffects) and (FGLObjectEffects.Count > 0)));
  {FOriginalFiler:=nil;}
end;

//procedure TBZCustomVectorObject.WriteBehaviours(stream: TStream);
//var
//  writer: TWriter;
//begin
//  writer := TWriter.Create(stream, 16384);
//  try
//    Behaviours.WriteToFiler(writer);
//  finally
//    writer.Free;
//  end;
//end;

//procedure TBZCustomVectorObject.ReadBehaviours(stream: TStream);
//var
//  reader: TReader;
//begin
//  reader := TReader.Create(stream, 16384);
//  { with TReader(FOriginalFiler) do  }
//  try
//    {  reader.Root                 := Root;
//      reader.OnError              := OnError;
//      reader.OnFindMethod         := OnFindMethod;
//      reader.OnSetName            := OnSetName;
//      reader.OnReferenceName      := OnReferenceName;
//      reader.OnAncestorNotFound   := OnAncestorNotFound;
//      reader.OnCreateComponent    := OnCreateComponent;
//      reader.OnFindComponentClass := OnFindComponentClass;}
//    Behaviours.ReadFromFiler(reader);
//  finally
//    reader.Free;
//  end;
//end;

//procedure TBZCustomVectorObject.WriteEffects(stream: TStream);
//var
//  writer: TWriter;
//begin
//  writer := TWriter.Create(stream, 16384);
//  try
//    Effects.WriteToFiler(writer);
//  finally
//    writer.Free;
//  end;
//end;

//procedure TBZCustomVectorObject.ReadEffects(stream: TStream);
//var
//  reader: TReader;
//begin
//  reader := TReader.Create(stream, 16384);
//  {with TReader(FOriginalFiler) do }
//  try
//    { reader.Root                 := Root;
//     reader.OnError              := OnError;
//     reader.OnFindMethod         := OnFindMethod;
//     reader.OnSetName            := OnSetName;
//     reader.OnReferenceName      := OnReferenceName;
//     reader.OnAncestorNotFound   := OnAncestorNotFound;
//     reader.OnCreateComponent    := OnCreateComponent;
//     reader.OnFindComponentClass := OnFindComponentClass;   }
//    Effects.ReadFromFiler(reader);
//  finally
//    reader.Free;
//  end;
//end;

procedure TBZCustomVectorObject.GetChildren(AProc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
  if Assigned(FChildren) then
  begin
    for i := 0 to FChildren.Count - 1 do
    begin
      if not IsSubComponent(TComponent(FChildren.List^[i])) then AProc(TComponent(FChildren.List^[i]));
    end;
  end;
end;

function TBZCustomVectorObject.Get(Index: Integer): TBZCustomVectorObject;
begin
  if Assigned(FChildren) then Result := TBZCustomVectorObject(FChildren[Index])
  else Result := nil;
end;

function TBZCustomVectorObject.GetCount: Integer;
begin
  if Assigned(FChildren) then Result := FChildren.Count
  else Result := 0;
end;

function TBZCustomVectorObject.HasSubChildren: Boolean;
var
  I: Integer;
begin
  Result := False;
  if Count <> 0 then
  begin
    for I := 0 to Count - 1 do
    begin
      if IsSubComponent(Children[i]) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

procedure TBZCustomVectorObject.AddChild(aChild: TBZCustomVectorObject);
begin
  //if Assigned(FScene) then FScene.AddLights(aChild);
  if not Assigned(FChildren) then FChildren := TBZPersistentObjectList.Create;
  FChildren.Add(aChild);
  aChild.FParent := Self;
  aChild.SetScene(FScene);
  TransformationChanged;
  aChild.TransformationChanged;
  aChild.DoOnAddedToParent;
end;

function TBZCustomVectorObject.AddNewChild(aChild: TBZVectorObjectClass): TBZCustomVectorObject;
begin
  Result := aChild.Create(Owner);
  AddChild(Result);
end;

function TBZCustomVectorObject.AddNewChildFirst(aChild: TBZVectorObjectClass): TBZCustomVectorObject;
begin
  Result := aChild.Create(Owner);
  Insert(0, Result);
end;

//function TBZCustomVectorObject.GetOrCreateBehaviour(aBehaviour: TGLBehaviourClass):
//  TGLBehaviour;
//begin
//  Result := TGLBehaviour(Behaviours.GetOrCreate(aBehaviour));
//end;

//function TBZCustomVectorObject.AddNewBehaviour(aBehaviour: TGLBehaviourClass):
//  TGLBehaviour;
//begin
//  Assert(Behaviours.CanAdd(aBehaviour));
//  result := aBehaviour.Create(Behaviours)
//end;

//function TBZCustomVectorObject.GetOrCreateEffect(anEffect: TGLObjectEffectClass):
//  TGLObjectEffect;
//begin
//  Result := TGLObjectEffect(Effects.GetOrCreate(anEffect));
//end;

//function TBZCustomVectorObject.AddNewEffect(anEffect: TGLObjectEffectClass):
//  TGLObjectEffect;
//begin
//  Assert(Effects.CanAdd(anEffect));
//  result := anEffect.Create(Effects)
//end;

procedure TBZCustomVectorObject.Assign(Source: TPersistent);
var
  i: Integer;
  child, newChild: TBZCustomVectorObject;
begin
  if Assigned(Source) and (Source is TBZCustomVectorObject) then
  begin

    FVisible := TBZCustomVectorObject(Source).FVisible;

    FObjectsSorting := TBZCustomVectorObject(Source).FObjectsSorting;
    DeleteChildren;
    if Assigned(Scene) then Scene.BeginUpdate;
    if Assigned(TBZCustomVectorObject(Source).FChildren) then
    begin
      for i := 0 to TBZCustomVectorObject(Source).FChildren.Count - 1 do
      begin
        child := TBZCustomVectorObject(TBZCustomVectorObject(Source).FChildren[i]);
        newChild := AddNewChild(TGLSceneObjectClass(child.ClassType));
        newChild.Assign(child);
      end;
    end;
    if Assigned(Scene) then Scene.EndUpdate;
    OnProgress := TBZCustomVectorObject(Source).OnProgress;

    //if Assigned(TBZCustomVectorObject(Source).FGLBehaviours) then
    //  Behaviours.Assign(TBZCustomVectorObject(Source).Behaviours)
    //else
    //  FreeAndNil(FGLBehaviours);
    //if Assigned(TBZCustomVectorObject(Source).FGLObjectEffects) then
    //  Effects.Assign(TBZCustomVectorObject(Source).Effects)
    //else
    //  FreeAndNil(FGLObjectEffects);

    Tag := TBZCustomVectorObject(Source).Tag;
    FTagFloat := TBZCustomVectorObject(Source).FTagFloat;
    FHint :=  := TBZCustomVectorObject(Source).FHint;
  end
  else
    inherited Assign(Source);
end;

function TBZCustomVectorObject.IsUpdating: Boolean;
begin
  Result := (FUpdateCount <> 0) or (csReading in ComponentState);
end;

function TBZCustomVectorObject.GetParentComponent: TComponent;
begin
  if FParent is TBZSceneRootObject then Result := FScene
  else Result := FParent;
end;

function TBZCustomVectorObject.HasParent: Boolean;
begin
  Result := assigned(FParent);
end;

procedure TBZCustomVectorObject.SetName(const NewName: TComponentName);
begin
  if Name <> NewName then
  begin
    inherited SetName(NewName);
    //if Assigned(vGLBaseSceneObjectNameChangeEvent) then
    //  vGLBaseSceneObjectNameChangeEvent(Self);
  end;
end;

procedure TBZCustomVectorObject.SetParent(const val: TBZCustomVectorObject);
begin
  MoveTo(val);
end;

function TBZCustomVectorObject.GetIndex: Integer;
begin
  if Assigned(FParent) then Result := FParent.FChildren.IndexOf(Self)
  else Result := -1;
end;

procedure TBZCustomVectorObject.SetIndex(aValue: Integer);
var
  LCount: Integer;
  parentBackup: TBZCustomVectorObject;
begin
  if Assigned(FParent) then
  begin
    if aValue < 0 then aValue := 0;
    LCount := FParent.Count;
    if aValue >= LCount then aValue := LCount - 1;
    if aValue <> Index then
    begin
      if Assigned(FScene) then FScene.BeginUpdate;
      parentBackup := FParent;
      parentBackup.Remove(Self, False);
      parentBackup.Insert(AValue, Self);
      if Assigned(FScene) then FScene.EndUpdate;
    end;
  end;
end;

procedure TBZCustomVectorObject.SetParentComponent(Value: TComponent);
begin
  inherited;
  if Value = FParent then Exit;

  if Value is TGLScene then SetParent(TBZScene(Value).Objects)
  else if Value is TBZCustomVectorObject then SetParent(TBZCustomVectorObject(Value))
  else SetParent(nil);
end;

procedure TBZCustomVectorObject.MoveTo(newParent: TBZCustomVectorObject);
begin
  if newParent = FParent then Exit;
  if Assigned(FParent) then
  begin
    FParent.Remove(Self, False);
    FParent := nil;
  end;
  if Assigned(newParent) then newParent.AddChild(Self)
  else
    SetScene(nil);
end;

procedure TBZCustomVectorObject.MoveUp;
begin
  if Assigned(parent) then parent.MoveChildUp(parent.IndexOfChild(Self));
end;

procedure TBZCustomVectorObject.MoveDown;
begin
  if Assigned(parent) then parent.MoveChildDown(parent.IndexOfChild(Self));
end;

procedure TBZCustomVectorObject.MoveFirst;
begin
  if Assigned(parent) then parent.MoveChildFirst(parent.IndexOfChild(Self));
end;

procedure TBZCustomVectorObject.MoveLast;
begin
  if Assigned(parent) then parent.MoveChildLast(parent.IndexOfChild(Self));
end;

procedure TBZCustomVectorObject.StructureChanged;
begin
  if not (ocStructure in FChanges) then
  begin
    Include(FChanges, ocStructure);
    NotifyChange(Self);
  end
//  else if osDirectDraw in ObjectStyle then NotifyChange(Self);
end;

procedure TBZCustomVectorObject.ClearStructureChanged;
begin
  Exclude(FChanges, ocStructure);
  //SetBBChanges(BBChanges + [oBBcStructure]);
end;

procedure TBZCustomVectorObject.RecTransformationChanged;
var
  i: Integer;
  list: PPointerObjectList;
  matSet: TBZVectorObjectChanges;
begin
  matSet := [ocAbsoluteMatrix, ocInvAbsoluteMatrix];
  if ((matSet * FChanges) <> matSet) then
  begin
    FChanges := FChanges + matSet;
    if Assigned(FChildren) then
    begin
      list := FChildren.List;
      for i := 0 to FChildren.Count - 1 do
      begin
        TBZCustomVectorObject(list^[i]).RecTransformationChanged;
      end;
    end;
  end;
end;

procedure TBZCustomVectorObject.TransformationChanged;
begin
  if not (ocTransformation in FChanges) then
  begin
    Include(FChanges, ocTransformation);
    RecTransformationChanged;
    if not (csLoading in ComponentState) then NotifyChange(Self);
  end;
end;

procedure TBZCustomVectorObject.DoCoordinateChanged(Sender: TGLCustomCoordinates);
begin
  // ne fait rien ici
end;

procedure TBZCustomVectorObject.CoordinateChanged(Sender: TGLCustomCoordinates);
begin
  if FIsCalculating then Exit;
  FIsCalculating := True;
  try
    DoCoordinateChanged(Sender);
  finally
    FIsCalculating := False;
  end;
end;

procedure TBZCustomVectorObject.DoProgress(const progressTime: TProgressTimes);
var
  i: Integer;
begin
  if Assigned(FChildren) then
  begin
    for i := FChildren.Count - 1 downto 0 do
    begin
      TBZCustomVectorObject(FChildren.List^[i]).DoProgress(progressTime);
    end;
  end;

  //if Assigned(FGLBehaviours) then
  //  FGLBehaviours.DoProgress(progressTime);
  //if Assigned(FGLObjectEffects) then
  //  FGLObjectEffects.DoProgress(progressTime);

  if Assigned(FOnProgress) then
  begin
    with progressTime do
    begin
      FOnProgress(Self, deltaTime, newTime);
    end;
  end;
end;

procedure TBZCustomVectorObject.Insert(aIndex: Integer; aChild: TBZCustomVectorObject);
begin
  if not Assigned(FChildren) then FChildren := TBZPersistentObjectList.Create;
  with FChildren do
  begin
    if Assigned(aChild.FParent) then aChild.FParent.Remove(aChild, False);
    Insert(aIndex, aChild);
  end;
  aChild.FParent := Self;
  if AChild.FScene <> FScene then AChild.DestroyHandles;
  AChild.SetScene(FScene);
//  if Assigned(FScene) then FScene.AddLights(aChild);
  AChild.TransformationChanged;

  aChild.DoOnAddedToParent;
end;

procedure TBZCustomVectorObject.Remove(aChild: TBZCustomVectorObject; keepChildren:
  Boolean);
var
  I: Integer;
begin
  if not Assigned(FChildren) then Exit;
  if aChild.Parent = Self then
  begin
    //if Assigned(FScene) then FScene.RemoveLights(aChild);
    if aChild.Owner = Self then RemoveComponent(aChild);
    FChildren.Remove(aChild);
    aChild.FParent := nil;
    if keepChildren then
    begin
      BeginUpdate;
      if aChild.Count <> 0 then
      begin
        for I := aChild.Count - 1 downto 0 do
        begin
          if not IsSubComponent(aChild.Children[I]) then aChild.Children[I].MoveTo(Self);
        end;
      end;
      EndUpdate;
    end
    else
      NotifyChange(Self);
  end;
end;

function TBZCustomVectorObject.IndexOfChild(aChild: TBZCustomVectorObject): Integer;
begin
  if Assigned(FChildren) then Result := FChildren.IndexOf(aChild)
  else Result := -1;
end;

function TBZCustomVectorObject.FindChild(const aName: string; ownChildrenOnly: Boolean): TBZCustomVectorObject;
var
  i: integer;
  res: TBZCustomVectorObject;
begin
  res := nil;
  Result := nil;
  if not(Assigned(FChildren)) then Exit;

  for i := 0 to FChildren.Count - 1 do
  begin
    if CompareText(TBZCustomVectorObject(FChildren[i]).Name, aName) = 0 then
    begin
      res := TBZCustomVectorObject(FChildren[i]);
      Break;
    end;
  end;

  if not(ownChildrenOnly) then
  begin
    for i := 0 to FChildren.Count - 1 do
    begin
      with TBZCustomVectorObject(FChildren[i]) do
      begin
        Result := FindChild(aName, ownChildrenOnly);
        if Assigned(Result) then Break;
      end;
    end;
  end;

  if not(Assigned(Result)) then Result := res;
end;

procedure TBZCustomVectorObject.ExchangeChildren(anIndex1, anIndex2: Integer);
begin
  Assert(Assigned(FChildren), 'No children found!');
  FChildren.Exchange(anIndex1, anIndex2);
  NotifyChange(Self);
end;

procedure TBZCustomVectorObject.ExchangeChildrenSafe(anIndex1, anIndex2: Integer);
begin
  Assert(Assigned(FChildren), 'No children found!');
  if (anIndex1 < FChildren.Count) and (anIndex2 < FChildren.Count) and
    (anIndex1 > -1) and (anIndex2 > -1) and (anIndex1 <> anIndex2) then
  begin
    FChildren.Exchange(anIndex1, anIndex2);
    NotifyChange(Self);
  end;
end;

procedure TBZCustomVectorObject.MoveChildUp(anIndex: Integer);
begin
  Assert(Assigned(FChildren), 'No children found!');
  if (anIndex > 0) then
  begin
    FChildren.Exchange(anIndex, anIndex - 1);
    NotifyChange(Self);
  end;
end;

procedure TBZCustomVectorObject.MoveChildDown(anIndex: Integer);
begin
  Assert(Assigned(FChildren), 'No children found!');
  if (anIndex < FChildren.Count - 1) then
  begin
    FChildren.Exchange(anIndex, anIndex + 1);
    NotifyChange(Self);
  end;
end;

procedure TBZCustomVectorObject.MoveChildFirst(anIndex: Integer);
begin
  Assert(Assigned(FChildren), 'No children found!');
  if (anIndex <> 0) then
  begin
    FChildren.Move(anIndex, 0);
    NotifyChange(Self);
  end;
end;

procedure TBZCustomVectorObject.MoveChildLast(anIndex: Integer);
begin
  Assert(Assigned(FChildren), 'No children found!');
  if (anIndex <> FChildren.Count - 1) then
  begin
    FChildren.Move(anIndex, FChildren.Count - 1);
    NotifyChange(Self);
  end;
end;

procedure TBZCustomVectorObject.NotifyChange(Sender: TObject);
begin
  if Assigned(FScene) and (not IsUpdating) then FScene.NotifyChange(Self);
end;

function TBZCustomVectorObject.GetVisible: Boolean;
begin
  Result := FVisible;
end;

function TBZCustomVectorObject.GetPickable: Boolean;
begin
  Result := FPickable;
end;

procedure TBZCustomVectorObject.SetVisible(aValue: Boolean);
begin
  if FVisible = aValue then exit;
  FVisible := AValue;
  NotifyChange(Self);
end;

procedure TBZCustomVectorObject.SetPickable(aValue: Boolean);
begin
  if FPickable = aValue then exit;
  FPickable := AValue;
  NotifyChange(Self);
end;

procedure TBZCustomVectorObject.SetObjectsSorting(const val: TGLObjectsSorting);
begin
  if FObjectsSorting = val then exit;
  FObjectsSorting := val;
  NotifyChange(Self);
end;

//procedure TBZCustomVectorObject.SetBehaviours(const val: TGLBehaviours);
//begin
//  Behaviours.Assign(val);
//end;

//function TBZCustomVectorObject.GetBehaviours: TGLBehaviours;
//begin
//  if not Assigned(FGLBehaviours) then
//    FGLBehaviours := TGLBehaviours.Create(Self);
//  Result := FGLBehaviours;
//end;

//procedure TBZCustomVectorObject.SetEffects(const val: TGLObjectEffects);
//begin
//  Effects.Assign(val);
//end;

//function TBZCustomVectorObject.GetEffects: TGLObjectEffects;
//begin
//  if not Assigned(FGLObjectEffects) then
//    FGLObjectEffects := TGLObjectEffects.Create(Self);
//  Result := FGLObjectEffects;
//end;

procedure TBZCustomVectorObject.SetScene(const value: TBZScene);
var
  i: Integer;
begin
  if value <> FScene then
  begin
    // must be freed, the new scene may be using a non-compatible RC
    //if FScene <> nil then DestroyHandles;
    FScene := value;
    // propagate for childs
    if Assigned(FChildren) then
    begin
      for i := 0 to FChildren.Count - 1 do
      begin
        Children[I].SetScene(FScene);
      end;
    end;
  end;
end;

procedure TBZCustomVectorObject.DoOnAddedToParent;
begin
  if Assigned(FOnAddedToParent) then FOnAddedToParent(self);
end;

{%endregion%}

initialization

  RegisterClasses([TBZCoordinates2D, TBZCoordinates3D, TBZCoordinates4D,
                   TBZCamera2D, TBZCamera3D,
                   TBZ2DProxyObject, TBZ3DProxyObject,
                   TBZScene]);


end.

end.
