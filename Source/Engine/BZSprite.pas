(*
  @abstract(Contient des classes permettant de gérer
    un univers de sprites.)

  --------------------------------------------------------------------------------

  @created(2018-07-11)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(11/07/2018 : Creation)
    @item(05/05/2020 : Dernière mise à jour)
  )

  --------------------------------------------------------------------------------

  @bold(Notes) : Unité encore expérimentale et non définitive

  --------------------------------------------------------------------------------

  @bold(Dependances) : BZClasses,  BZMath, BZVectorMath, BZGraphic, BZGeoTools, BZBitmap, BZBitmapIO

  --------------------------------------------------------------------------------

  @bold(Credits :)
   @unorderedList(
      @item(Basé sur le code de feux, la bibliothèque DelphiX sur laquelle j'avais également participer au développement à l'époque. @br
            Puis, repris par la suite par les bibliothèques Asphyre/PLX et la 1ere version de ZenGL si je me souviens bien )
       @item(J.Delauney (BeanzMaster))
   )

  --------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  -------------------------------------------------------------------------------- *)
Unit BZSprite;


//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================

Interface

Uses
  Classes, Sysutils,
  BZClasses,  BZMath, BZVectorMath, BZGraphic, BZGeoTools, BZBitmap, {%H-}BZBitmapIO;

Type
  EBZSpriteError = Class(EBZBaseException);

  TBZCustomSpriteEngine = Class;
  TBZCustomBaseSprite = class;

  TBZSpriteCollideMode = (cmNone, cmCircle, cmRect, cmQuadrangle, cmPolygon);
  TBZSpriteAnimationMode =(amForward, amBackWard, amPingPong);

  TBZSpriteAnchor = (saTopLeft,saCenter,saBottomMiddle);
  TBZSpriteHookPosition = ( hpTop, hpBottom, hpLeft, hpRight, hpMiddle );

  TBZSpriteList = class(TBZPersistentObjectList)
  protected
    function  Get( Index : Integer ) : TBZCustomBaseSprite;
    procedure Put( Index : Integer; Item : TBZCustomBaseSprite );
  public
    property Items[ Index : Integer ] : TBZCustomBaseSprite read Get write Put; default;
    constructor Create; override;
  end;

  TBZCustomBaseSprite = class(TBZUpdateAbleObject)
  private
    FEngine : TBZCustomSpriteEngine; //Reference au moteur de sprite
    FParent : TBZCustomBaseSprite;
    FWidth, FHeight : LongWord; //Dimension du sprite
    FX, FY : Single; //Position relative à la position dans le monde
    FWorldX, FWorldY : Single; // Position dans le monde
    FLayer : Integer; // Position en Z, si < 0 invisible
    FVisible : Boolean; // Sprite visible
    FDead : Boolean; // Sprite mort
    FEnabledMove : Boolean; // Le sprite se déplace
    //FHotSpotX, FHotSpotY : Single; //position de reference pour dessiner le sprite

    FEnabledCollision : Boolean; //On gère les collisions
    FCollideMode : TBZSpriteCollideMode;
    FCollidePos : TBZVector2f;
    FCollideRect: TBZFloatRect;
    FCollideRadius : Integer;
    FCollidePolygon : TBZ2DPolygonTool;
    FCollideQuadrangle : TBZQuadrangle2D;

    FSpriteList : TBZSpriteList;
    FTag : Integer;


    procedure SetLayer(Value: Integer);
    function GetItem(Index: Integer): TBZCustomBaseSprite;
    function GetCount: Integer;

  protected
    function GetWorldX: Single; virtual;
    function GetWorldY: Single; virtual;

    procedure DoMove(MoveCount: Integer); virtual;
    procedure DoCollision(Sprite: TBZCustomBaseSprite); virtual;
    procedure DoDraw; virtual;


    function GetBoundsRect: TBZFloatRect; virtual;

  public
    constructor Create(AParent: TBZCustomBaseSprite); virtual;
    destructor Destroy; override;
    procedure Assign(const Value: TBZCustomBaseSprite); override;

    procedure Add(Sprite: TBZCustomBaseSprite);
    procedure Remove(Sprite: TBZCustomBaseSprite);
    procedure Clear;

    procedure Dead;
    procedure Move(MoveCount: Integer);
    procedure Collision(const Other: TBZCustomBaseSprite); overload; virtual;
    procedure Collision; overload; virtual;


    procedure Draw; virtual;

    procedure SetPos(X, Y: Single); overload;
    procedure SetPos(X, Y: Single; Z: Integer); overload;

    property Parent: TBZCustomBaseSprite read FParent;
    property Engine: TBZCustomSpriteEngine read FEngine;

    property Visible: Boolean read FVisible write FVisible;
    property EnabledMove: Boolean read FEnabledMove write FEnabledMove;
    property IsDead: Boolean read FDead write FDead;

    property EnabledCollision: Boolean read FEnabledCollision write FEnabledCollision;
    property CollideMode: TBZSpriteCollideMode read FCollideMode write FCollideMode;
    property CollidePos: TBZVector2f read FCollidePos write FCollidePos;
    property CollideRect: TBZFloatRect read FCollideRect write FCollideRect;
    property CollideRadius: Integer read FCollideRadius write FCollideRadius;
    property CollideQuadrangle: TBZQuadrangle2D read FCollideQuadrangle write FCollideQuadrangle;
    property CollidePolygon : TBZ2DPolygonTool read FCollidePolygon write FCollidePolygon;

    property Width: LongWord read FWidth write FWidth;
    property Height: LongWord read FHeight write FHeight;
    property X: Single read FX write FX;
    property Y: Single read FY write FY;
    property WorldX: Single read GetWorldX;
    property WorldY: Single read GetWorldY;
    property Layer: Integer read FLayer write SetLayer;
    property BoundsRect: TBZFloatRect read GetBoundsRect;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TBZCustomBaseSprite read GetItem; default;
    property Tag: Integer read FTag write FTag;
  end;

  TBZSpriteClass = class of TBZCustomBaseSprite;

  (* TBZAtlasPatternItem = class
     private
       FOwnerBitmap : TBZBitmap;
       FPatternRect : TRect;
       FPatternWidth, FPatternHeight : Integer;
       FPatternBitmap : TBZBitmap;

       function GetPatternWidth:Integer;
       function GetPatternHeight:Integer;
       procedure SetPatternRect(aRect : TRect);
     protected
     public
       Property PatternRect : TRect read FPatternRect Write SetPatternRect;
       Property PatternWidth : Integer read GetPatternWidth;
       Property PatternHeight : Integer read GetPatternHeight;
       property PatternBitmap : TBZBitmap read FPatternBitmap;
    end;

    TBZAtlasImage = class(TBZPersistentObjectList)
    protected
      function  Get( Index : Integer ) : TBZAtlasPatternItem ;
      procedure Put( Index : Integer; Item : TBZAtlasPatternItem  );
    public
      property Items[ Index : Integer ] : TBZAtlasPatternItem  read Get write Put; default;
      constructor Create;
    end;

  *)

  TBZBitmapSprite = Class(TBZCustomBaseSprite)
  private
    FImage : TBZBitmap;

    FAnimCount: Integer;
    FAnimLooped: Boolean;
    FAnimIndex: Integer;
    FAnimSpeed: Single;
    FAnimStart: Integer;
    FAnimEnd: Integer;
    FAnimMode : TBZSpriteAnimationMode;
    FAnimated : Boolean;
    FAnimSens : Integer;

    FPixelCheck: Boolean;
    FAngle: Integer;
    FAlpha: Integer;
    FScaleX, FScaleY : Integer;  //Single
    FMoveX, FMoveY : Integer;    //Single

    FAnchor : TBZSpriteAnchor;
    FAnchorX, FAnchorY : Integer;

    FEnabledHook: boolean;
    FHookPosition: TBZSpriteHookPosition;
    FHookTarget: TBZCustomBaseSprite;
    FHookDistance: integer;
    //FDrawEffects
    //FDrawMode
    //FAltasImage : Boolean;
    //FPatternWidth, FPatternHeight : Integer;
    //FPatternCount : Integer;
    //FPatternList : TBZAtlasPattern
    procedure SetHookTarget(P: TBZCustomBaseSprite);
    function GetAnimCount : Integer;
    procedure SetAnimStart(Value: Integer);
    procedure SetAnimEnd(Value: Integer);
    procedure SetAnchor(Value:TBZSpriteAnchor);
  protected
    function GetWorldX: Single; override;
    function GetWorldY: Single; override;

    procedure DoDraw; override;
    procedure DoMove(MoveCount: Integer); override;
    function GetBoundsRect: TBZFloatRect; override;


  public
    constructor Create(AParent: TBZCustomBaseSprite); override;
    destructor Destroy; override;

    procedure AddImage(anImage: TBZBitmap);
    procedure AddImageFromFile(aFileName: String);

    procedure DoCollision(Sprite: TBZCustomBaseSprite);override;

    procedure OnAnimStart; virtual;
    procedure OnAnimEnd; virtual;

    property Image : TBZBitmap read FImage write FImage;

    property AnimCount: Integer read GetAnimCount;
    property AnimLooped: Boolean read FAnimLooped write FAnimLooped;
    property AnimIndex: Integer read FAnimIndex write FAnimIndex;
    property AnimSpeed: Single read FAnimSpeed write FAnimSpeed;
    property AnimStart: Integer read FAnimStart write FAnimStart;
    property AnimMode: TBZSpriteAnimationMode read FAnimMode write FAnimMode;
    property Animated : Boolean Read FAnimated Write FAnimated;

    property PixelCheck: Boolean read FPixelCheck write FPixelCheck;

    property MoveX: Integer read FMoveX write FMoveX;
    property MoveY: Integer read FMoveY write FMoveY;
   // property Angle: Integer  read FAngle write SetAngle default 0;
   // property Alpha: Integer read FAlpha write SetAlpha default 255;
    property ScaleX: Integer read FScaleX write FScaleX;
    property ScaleY: Integer read FScaleY write FScaleY;

    property Anchor : TBZSpriteAnchor Read FAnchor Write SetAnchor;

    property EnabledHook: boolean read FEnabledHook write FEnabledHook;
    property HookPosition: TBZSpriteHookPosition read FHookPosition write FHookPosition;
    property HookTarget: TBZCustomBaseSprite read FHookTarget write SetHookTarget;
    property HookDistance: integer read FHookDistance write FHookDistance;
  End;

  TBZBackgroundMapSprite = class(TBZBitmapSprite)
  private
     FCollisionMap: Pointer;
     FMap: Pointer;
     FMapW:Integer;
     FMapH: Integer;
     FMapWidth: Integer;
     FMapHeight: Integer;
     FTiled: Boolean;
     function GetCollisionMapItem(vX, vY: Integer): Boolean;
     function GetCell(vX, vY: Integer): Integer;

     procedure SetCell(vX, vY: Integer; Value: Integer);
     procedure SetCollisionMapItem(vX, vY: Integer; Value: Boolean);
     procedure SetMapHeight(Value: Integer);
     procedure SetMapWidth(Value: Integer);
  protected
     procedure DoDraw; override;
     function GetBoundsRect: TBZFloatRect; override;
     function TestCollision(Sprite: TBZCustomBaseSprite): Boolean;
  public
     constructor Create(AParent: TBZCustomBaseSprite); override;
     destructor Destroy; override;

     procedure Draw; override;

     property BoundsRect: TBZFloatRect read GetBoundsRect;
     procedure SetMapSize(AMapWidth, AMapHeight: Integer);
     property Cells[XX, YY: Integer]: Integer read GetCell write SetCell;
     property CollisionMap[XX, YY: Integer]: Boolean read GetCollisionMapItem write SetCollisionMapItem;
     property MapHeight: Integer read FMapHeight write SetMapHeight;
     property MapWidth: Integer read FMapWidth write SetMapWidth;
     property Tiled: Boolean read FTiled write FTiled;
  end;

 (* TBZBitmapParticleSprite = class(TBZBitmapSprite)
  private
    FCX,FCY: double;                // Position courante
    FDiffuseAngle: double;          // Angle de diffusion
    FVelocityX, FVelocityY: double; // Vitesse de la particule
    FSpeedX, FSpeedY: double;       // Vitesse d'accélération de la particule
    FGravX, FGravY: double;         // Gravité
    FMass : Double;                 // Masse de la particule
    FEnergy: double;                // Puissance de propagation
    FAge, FMaxAge: integer;         // Temps de vie
    FDelay: integer;                // Délai avant regénération
    FAlphaVar,
    FAlphaMin, FAlphaMax  : Integer; // Alpha Blending

  protected
  public
  End; *)

  TBZCustomSpriteEngine = class(TBZCustomBaseSprite)
  private
    FAllCount: Integer;
    //FCollisionCount: Integer;

    FDeadList: TList;
    FDrawCount: Integer;
  protected
    FSurface : TBZBitmap;  // A remplacer par la suite par TBZCustomCanvas, cela permettra d'afficher les sprites sur une Surface OpenGL/Vulkan/SDL/DirectX ect...
    FSurfaceRect : TBZFloatRect;
    procedure SetSurface(Value: TBZBitmap); Virtual;
  public
    constructor Create(AParent: TBZCustomBaseSprite); override;
    destructor Destroy; override;
    procedure Collision; override;
    procedure Dead;
    procedure Draw;override;
    property AllCount: Integer read FAllCount;
    property DrawCount: Integer read FDrawCount write FDrawCount;
    property Surface : TBZBitmap read FSurface Write SetSurface;
    property SurfaceRect : TBZFloatRect read FSurfaceRect Write FSurfaceRect;
  end;

  TBZBitmapSpriteEngine = class(TBZCustomSpriteEngine);

Implementation

Uses Math, Dialogs;

Const
   SListIndexError = 'L''index de la liste dépassé. (%d)';


{  function rotatePoint(point, center, angle)
   begin
     angle = (angle ) * (Math.PI/180); // Convert to radians
     var rotatedX = Math.cos(angle) * (point.x - center.x) - Math.sin(angle) * (point.y-center.y) + center.x;
     var rotatedY = Math.sin(angle) * (point.x - center.x) + Math.cos(angle) * (point.y - center.y) + center.y;

     return new createjs.Point(rotatedX,rotatedY);
   end;
           }

function Mod2(i, i2: Integer): Integer;
begin
  Result := i mod i2;
  if Result<0 then
    Result := i2+Result;
end;

function Mod2f(i: Double; i2: Integer): Double;
begin
  if i2=0 then
    Result := i
  else
  begin
    Result := i-Trunc(i/i2)*i2;
    if Result<0 then
      Result := i2+Result;
  end;
end;

{%region=====[ TBZSpriteList ]=================================================}

constructor TBZSpriteList.Create;
begin
  inherited Create;
end;

function TBZSpriteList.Get( Index : Integer ) : TBZCustomBaseSprite;
begin
  Result := TBZCustomBaseSprite(inherited Get( Index ));
end;

procedure TBZSpriteList.Put( Index : Integer; Item : TBZCustomBaseSprite );
begin
  inherited Put( Index, Item );
end;

{%endregion%}

{%region=====[ TBZCustomBaseSprite ]===========================================}

constructor TBZCustomBaseSprite.Create(AParent: TBZCustomBaseSprite);
begin
  Inherited Create;
  FParent := AParent;
  FEngine := nil;
  FWidth := 0;
  FHeight := 0;
  FX := 0;
  FY := 0;
  FWorldX := 0;
  FWorldY := 0;
  FLayer := 0;
  FVisible := True;
  FDead := False;
  FEnabledCollision := False;
  FEnabledMove := True;
  FCollideMode := cmNone;
  FSpriteList := TBZSpriteList.Create;
  FTag := 0;
  if FParent<>nil then
  begin
    FParent.Add(Self);
    if FParent is TBZCustomSpriteEngine then
    begin
      FEngine := TBZCustomSpriteEngine(FParent)
   //   FEngine.Add(Self);
    End
    else
      FEngine := FParent.Engine;

    Inc(FEngine.FAllCount);
  end;

End;

destructor TBZCustomBaseSprite.Destroy;
Begin
  Clear;
  FreeAndNil(FSpriteList);
  Inherited Destroy;
End;

procedure TBZCustomBaseSprite.Assign(const Value: TBZCustomBaseSprite);
begin
     //FName := Value.Name;
    // FImageName := Value.ImageName;
     FX  := Value.X;
     FY  := Value.Y;
     FLayer  := Value.Layer;
     FWorldX  := Value.WorldX;
     FWorldY  := Value.WorldY;
   //  FPatternIndex := Value.PatternIndex;
   //  FImageIndex := Value.ImageIndex;
     FCollideMode := Value.CollideMode;
     FEnabledCollision := Value.EnabledCollision;
     FCollidePos := Value.CollidePos;
     FCollideRadius := Value.CollideRadius;
     FCollideRect := Value.CollideRect;
     FCollideQuadrangle := Value.CollideQuadrangle;
     FEnabledMove := Value.EnabledMove;
    // FEffect := Value.Effect;
     FVisible := Value.Visible;
     FTag := Value.Tag;
end;

function TBZCustomBaseSprite.GetWorldX: Single;
begin
  if Parent<>nil then
    Result := Parent.WorldX+FX
  else
    Result := FX;
end;

function TBZCustomBaseSprite.GetWorldY: Single;
begin
  if Parent<>nil then
    Result := Parent.WorldY+FY
  else
    Result := FY;
end;

procedure TBZCustomBaseSprite.SetLayer(Value: Integer);
begin
  if FLayer=Value then exit;
  FLayer := Value;
  if Parent<>nil then
  begin
    Parent.Remove(Self);
    Parent.Add(Self);
  end;
end;

function TBZCustomBaseSprite.GetItem(Index: Integer): TBZCustomBaseSprite;
begin
  if FSpriteList<>nil then
    Result := FSpriteList.Items[Index]
  else
    raise EBZSpriteError.CreateFmt(SListIndexError, [Index]);
End;

function TBZCustomBaseSprite.GetCount: Integer;
begin
  if FSpriteList<>nil then
    Result := FSpriteList.Count
  else
    Result := 0;
End;

procedure TBZCustomBaseSprite.Add(Sprite: TBZCustomBaseSprite);
var
  L, H, I, C: Integer;
begin
  if FSpriteList=nil then FSpriteList := TBZSpriteList.Create;
  L := 0;
  H := FSpriteList.Count - 1;
  while L <= H do
  begin
    I := (L + H) div 2;
    C := TBZCustomBaseSprite(FSpriteList[I]).Layer-Sprite.Layer;
    if C < 0 then L := I + 1 else H := I - 1;
  end;
  FSpriteList.Insert(L, Sprite);
End;

procedure TBZCustomBaseSprite.Remove(Sprite: TBZCustomBaseSprite);
begin
  FSpriteList.Remove(Sprite);
  if FSpriteList.Count=0 then
  begin
    FSpriteList.Free;
    FSpriteList := nil;
  end;
end;

procedure TBZCustomBaseSprite.Clear;
begin
  while Count>0 do
  begin
    if Items[Count-1]<>nil then Items[Count-1].Free;
  End;
End;

procedure TBZCustomBaseSprite.DoMove(MoveCount: Integer);
begin

End;

procedure TBZCustomBaseSprite.DoDraw;
begin

End;

procedure TBZCustomBaseSprite.DoCollision(Sprite: TBZCustomBaseSprite);
begin

End;

function TBZCustomBaseSprite.GetBoundsRect: TBZFloatRect;
begin
  Result.CreateBounds(WorldX, WorldY, FWidth, FHeight);
End;

procedure TBZCustomBaseSprite.Dead;
begin
  if (FEngine<>nil) and (not FDead) then
  begin
    FDead := True;
   // FEngine.DeadList.Add(Self);
  end;
End;

procedure TBZCustomBaseSprite.Move(MoveCount: Integer);
var
  i: Integer;
begin
  if FEnabledMove then
  begin
    DoMove(MoveCount);
    for i:=0 to Count-1 do
      Items[i].Move(MoveCount);
  end;
End;

procedure TBZCustomBaseSprite.Collision;
var
   i: Integer;
begin
  if (FEngine <> nil) and (not FDead) and (FEnabledCollision) then
  begin
    for i := 0 to FEngine.Count - 1 do
      Self.Collision(FEngine.Items[i]);
  end;
end;


procedure TBZCustomBaseSprite.Collision(const Other: TBZCustomBaseSprite);
var
   Delta: Real;
   IsCollid: Boolean;
begin
  IsCollid := False;

//  if (FEnabledCollision) and (Other.FEnabledCollision) and
  if (not FDead) and (not Other.FDead) then
  begin
    case FCollideMode of
      cmCircle:
      begin
        Delta := Sqrt(Sqr(Self.WorldX - Other.WorldX) +
                      Sqr(Self.WorldY - Other.WorldY));
        IsCollid := (Delta < (Self.FCollideRadius + Other.FCollideRadius));
      end;
      cmRect:
      begin
        IsCollid := Self.FCollideRect.OverlapRect(Other.FCollideRect);
      end;
      cmQuadrangle:
      begin
        IsCollid := OverlapQuadrangle(Self.FCollideQuadrangle, Other.FCollideQuadrangle);
      end;
      cmPolygon:
      begin
        IsCollid :=Self.FCollidePolygon.IntersectWithPolygon(Other.FCollidePolygon);
      end;
    end;

    if IsCollid then
    begin
      DoCollision(Other);
      Other.DoCollision(Self);
    end;
  end;
end;

procedure TBZCustomBaseSprite.Draw;
var
  i: Integer;
begin
  if (FVisible) and not(FDead) then
  begin
    if Assigned(FEngine) then
    begin
      if FEngine.SurfaceRect.OverlapRect(BoundsRect) then
      begin
        DoDraw;
        Inc(FEngine.FDrawCount);
      end;
    end;


    if (FSpriteList<>nil) and (FSpriteList.Count>0) then
    begin
      for i:=0 to FSpriteList.Count-1 do
        TBZCustomBaseSprite(FSpriteList[i]).Draw;
    end;
  end;
End;

procedure TBZCustomBaseSprite.SetPos(X, Y: Single);
begin
   FX := X;
   FY := Y;
end;

procedure TBZCustomBaseSprite.SetPos(X, Y: Single; Z: Integer);
begin
   FX := X;
   FY := Y;
   SetLayer(Z);
end;

{%endregion%}

{%region=====[ TBZBitmapSprite ]==============================================}

constructor TBZBitmapSprite.Create(AParent: TBZCustomBaseSprite);
begin
  Inherited Create(AParent);
  FImage :=nil;

  FAnimCount:=0;
  FAnimLooped:=false;
  FAnimIndex:=-1;
  FAnimSpeed:=1.0;
  FAnimStart:=0;
  FAnimEnd:=0;
  FAnimMode :=amForward;
  FAnimated :=false;
  FAnimSens :=1;;

  FPixelCheck:=false;
  FAngle:=0;
  FAlpha:=255;
  FScaleX := 1;
  FScaleY := 1;
  FMoveX:=0;
  FMoveY:=0;

  FAnchorX := 0;
  FAnchorY := 0;

  FEnabledHook := false;
  FHookTarget := nil;
  FHookPosition := hpMiddle;
  FHookDistance := 0;
end;

destructor TBZBitmapSprite.Destroy;
Begin
  if FImage<>Nil then FreeAndNil(FImage);
  Inherited Destroy;
End;

function TBZBitmapSprite.GetAnimCount : Integer;
Begin
  Result := FAnimEnd - FAnimStart;
End;

procedure TBZBitmapSprite.SetAnimStart(Value: Integer);
Begin
  if FAnimStart=Value then exit;
  FAnimStart := Value;
End;

procedure TBZBitmapSprite.SetAnimEnd(Value: Integer);
Begin
  if FAnimEnd=Value then exit;
  FAnimEnd := Value;
End;

procedure TBZBitmapSprite.SetAnchor(Value: TBZSpriteAnchor);
begin
  if FAnchor = Value then exit;
  FAnchor := Value;
  Case FAnchor of
    saTopLeft :
    begin
      FAnchorX := 0;
      FAnchorY := 0;
    End;
    saCenter :
    begin
      if FImage<>nil then
      begin
        FAnchorX := FImage.CenterX;
        FAnchorY := FImage.CenterY;
      End
      else
      begin
        FAnchorX := (Width div 2)-1;
        FAnchorY := (Height div 2)-1;
      End;
    End;
    saBottomMiddle :
    begin
      if FImage<>nil then
      begin
        FAnchorX := FImage.CenterX;
        FAnchorY := FImage.MaxHeight;
      End
      else
      begin
        FAnchorX := (Width div 2)-1;
        FAnchorY := Height-1;
      End;
    End;
  End;
End;

procedure TBZBitmapSprite.SetHookTarget(P: TBZCustomBaseSprite);
begin
  if (P = nil) then
  begin
    FHookTarget := nil;
    FEnabledHook := false;
  end
  else
  begin
    FEnabledHook := true;
    FHookTarget := P;
  end;
end;

procedure TBZBitmapSprite.DoDraw;
var
  px,py : Integer;
  tmpBmp : TBZBitmap;
Begin
  if not(FAnimated) then
  begin
    if FImage<>nil then
    begin
      if Visible and not(IsDead) then
      begin//+ OffsetX
        px := Round(WorldX);
        py := Round(WorldY);
        Engine.Surface.PutImage(FImage,0,0,FImage.Width, FImage.Height,px,py,dmSet,amAlphaCheck);
        //Round(X + WorldX - Engine.WorldX), Round(Y + WorldY - Engine.WorldY),dmSet,amAlpha);
        Engine.DrawCount:=Engine.DrawCount+1;
      end;
    End;
  End
  else
  begin
    if FAnimCount>0 then
    begin
      if not(IsDead) then
      begin
        px := Round(WorldX);
        py := Round(WorldY);
        tmpBmp:=TBZBitmap.Create;
        tmpBmp:=TBZBitmapSprite(Items[FAnimIndex]).Image;
        Engine.Surface.PutImage(tmpBmp,0,0,TmpBmp.Width, TmpBmp.Height,px, py,dmSet,amAlphaCheck);
        Engine.DrawCount:=Engine.DrawCount+1;
        FreeAndNil(TmpBmp);
      end;
    End;
  End;
End;

procedure TBZBitmapSprite.DoMove(MoveCount: Integer);
Begin
  if not(IsDead) and (EnabledMove) then
  begin
    X:=X+FMoveX*MoveCount;
    Y:=Y+FMoveY*MoveCount;

    if FEnabledHook then
    begin
      case FHookPosition of
        hpTop : begin
          X := (FHookTarget.X + (FHookTarget.width div 2)) - (width div 2);
          Y := FHookTarget.Y + FHookTarget.Height + FHookDistance;
        end;
        hpBottom : begin
          X := (FHookTarget.X + (FHookTarget.width div 2)) - (width div 2);
          Y := FHookTarget.Y - Height - FHookDistance;
        end;
        hpLeft : begin
          X := FHookTarget.X + FHookTarget.Width + FHookDistance;
          Y := (FHookTarget.Y + (FHookTarget.Height div 2)) - (Height div 2);
        end;
        hpRight : begin
          X := FHookTarget.X - Width - FHookDistance;
          Y := (FHookTarget.Y + (FHookTarget.Height div 2)) - (Height div 2);
        end;
        hpMiddle :
        begin
          X := (FHookTarget.X + (FHookTarget.width div 2)) - (width div 2);
          Y := (FHookTarget.Y + (FHookTarget.Height div 2)) - (Height div 2);
        end;
      end;
    End;

    if FAnimated then
    begin
      if FAnimIndex=FAnimStart then OnAnimStart;
      Case FAnimMode of
        amForward :
        begin
          if (FAnimIndex<FAnimEnd) then Inc(FAnimIndex,round(FAnimSpeed*MoveCount));
        End;
        amBackWard :
        begin
          if FAnimIndex<FAnimEnd then Dec(FAnimIndex,Round(FAnimSpeed*MoveCount));
        End;
        amPingPong :
        begin
          if FAnimIndex >= FAnimEnd then FAnimSpeed := -FAnimSpeed;
          if FAnimIndex<= FAnimStart then FAnimSpeed := Abs(FAnimSpeed);
          Inc(FAnimIndex,Round(FAnimSpeed*MoveCount));
          if FAnimIndex<FAnimStart then FAnimIndex:=FAnimStart
          else if FAnimIndex>FAnimEnd then FAnimIndex:=FAnimEnd;
        End;
      End;
      if FAnimIndex=FAnimEnd then OnAnimEnd;
    End;
  End;

  //Inherited DoMove;
End;

function TBZBitmapSprite.GetWorldX: Single;
begin
  Result := Inherited GetWorldX - FAnchorX;
End;

function TBZBitmapSprite.GetWorldY: Single;
begin
  Result := Inherited GetWorldY - FAnchorY;
End;

function TBZBitmapSprite.GetBoundsRect: TBZFloatRect;
Var
  xx, yy : Integer;
Begin
  xx:= Round(WorldX);
  yy:= Round(WorldY);
  if FAnimated then
  begin
    result.Create(xx,yy,xx+Items[FAnimIndex].Width,yy+Items[FAnimIndex].Height);
  End
  else
  begin
    result.Create(xx,yy,xx+Width,yy+Height);
  End;

End;

procedure TBZBitmapSprite.DoCollision( Sprite: TBZCustomBaseSprite);
Begin
End;

procedure TBZBitmapSprite.AddImage(anImage: TBZBitmap);
Begin
  if FAnimated then
  begin

  End
  else
  begin
    FImage := anImage;
  End;
End;

procedure TBZBitmapSprite.AddImageFromFile(aFileName: String);
Begin

End;

procedure TBZBitmapSprite.OnAnimStart;
Begin
End;

procedure TBZBitmapSprite.OnAnimEnd;
Begin
End;

{%endregion%}

{%region=====[ TBZBackgroundBitmapSprite ]====================================}

Constructor TBZBackgroundMapSprite.Create(AParent: TBZCustomBaseSprite);
begin
     inherited Create(AParent);
     X := 0;
     Y := 0;
     EnabledCollision := False;
end;

destructor TBZBackgroundMapSprite.Destroy;
begin
     SetMapSize(0, 0);
     inherited Destroy;
end;

procedure TBZBackgroundMapSprite.Draw;
var
  i: Integer;
begin
     if FVisible then
     begin
          if FEngine <> nil then
          begin
               if  FTiled then
               begin
                    {
                    if (X > FEngine.WorldX - Width + X) and
                    (Y > FEngine.WorldY - Height + Y) and
                    (X < FEngine.WorldX + FEngine.VisibleWidth + X )  and
                    (Y < FEngine.WorldY + FEngine.VisibleHeight+ Y) then
                    }
                    if (X > FEngine.WorldX - Width -Engine.WorldX) and
                    (Y > FEngine.WorldY - Height -Engine.WorldY) and
                    (X < FEngine.WorldX + FEngine.Surface.MaxWidth -Engine.WorldX )  and
                    (Y < FEngine.WorldY + FEngine.Surface.MaxHeight-Engine.WorldY) then
                    begin
                         DoDraw;
                         Inc(FEngine.FDrawCount);
                    end;
               end
               else
               begin
                    if (X > FEngine.WorldX - Width - FMapW) and
                    (Y > FEngine.WorldY - Height - FMapH)   and
                    (X < FEngine.WorldX + FEngine.Surface.MaxWidth + 200)  and
                    (Y < FEngine.WorldY + FEngine.Surface.MaxHeight + 200) then
                    begin
                         DoDraw;
                         Inc(FEngine.FDrawCount);
                    end;
               end;
          end;

          if FSpriteList <> nil then
          begin
               for i := 0 to FSpriteList.Count - 1 do
                   TBZCustomBaseSprite(FSpriteList[i]).Draw;
          end;
     end;
end;


procedure TBZBackgroundMapSprite.DoDraw;
var
   px,py, _x, _y, cx, cy, cx2, cy2, c, ChipWidth, ChipHeight: Integer;
   StartX, StartY, EndX, EndY, StartX_, StartY_, OfsX, OfsY, dWidth, dHeight: Integer;
begin
     if (FMapWidth <= 0) or (FMapHeight <= 0) then Exit;
     ChipWidth := Self.Width;
     ChipHeight := Self.Height;

     dWidth := (Engine.Surface.MaxWidth + ChipWidth) div ChipWidth + 1;
     dHeight := (Engine.Surface.MaxHeight + ChipHeight) div ChipHeight + 1;

     _x := Trunc(-Engine.WorldX - X);
     _y := Trunc(-Engine.WorldY - Y);

     OfsX := _x mod ChipWidth;
     OfsY := _y mod ChipHeight;

     StartX := _x div ChipWidth;
     StartX_ := 0;

     if StartX < 0 then
     begin
          StartX_ := -StartX;
          StartX := 0;
     end;

     StartY := _y div ChipHeight;
     StartY_ := 0;

     if StartY < 0 then
     begin
          StartY_ := -StartY;
          StartY := 0;
     end;

     EndX := Min(StartX + FMapWidth - StartX_, dWidth);
     EndY := Min(StartY + FMapHeight - StartY_, dHeight);

     if FTiled then
     begin
          for cy := -1 to dHeight do
          begin
               cy2 := Mod2((cy - StartY + StartY_), FMapHeight);
               for cx := -1 to dWidth do
               begin
                    cx2 := Mod2((cx - StartX + StartX_), FMapWidth);
                    c := Cells[cx2, cy2];
                    if c >= 0 then
                    begin
                      px:= cx * ChipWidth + OfsX;
                      py:=cy * ChipHeight + OfsY;
                      Engine.Surface.PutImage(Image,0,0,Image.Width, Image.Height,px, py,dmSet,amAlphaCheck);
                    end;
                     (*  DrawColor1(
                       MyCanvas, FImage,
                       c,
                       cx * ChipWidth + OfsX,
                       cy * ChipHeight + OfsY,
                       FScaleX, FScaleY, FDoCenter,
                       FMirrorX, FMirrorY,
                       FRed, FGreen, FBlue, FAlpha, FEffect); *)
               end;
          end;
     end
     else
     begin
          for cy := StartY to EndY - 1 do
          begin
               for cx := StartX to EndX - 1 do
               begin
                    c := Cells[cx - StartX + StartX_, cy - StartY + StartY_];
                    if c >= 0 then
                       (*  DrawColor1(
                         MyCanvas, FImage,
                         c,
                         cx * ChipWidth + OfsX ,
                         cy * ChipHeight + OfsY,
                         FScaleX, FScaleY, FDoCenter,
                         FMirrorX, FMirrorY,
                         FRed, FGreen, FBlue, FAlpha, FEffect); *)
               end;
          end;
     end;
end;

function TBZBackgroundMapSprite.TestCollision(Sprite: TBZCustomBaseSprite): Boolean;
var
  b, b1, b2, b3: TBZFloatRect;
  cx, cy, ChipWidth, ChipHeight: Integer;
begin
     Result := True;
     if Image = nil then Exit;
     if (FMapWidth <= 0) or (FMapHeight <= 0) then Exit;
     ChipWidth := Self.Width;
     ChipHeight := Self.Height;
     b1.Create(Trunc(Sprite.X), Trunc(Sprite.Y), Trunc(Sprite.X) + Width,Trunc(Sprite.Y) + Height);
     b2 := BoundsRect;

     B.IntersectWithRect(b1, b2);

     b.OffsetRect( -Trunc(Engine.WorldX), - Trunc(Engine.WorldY));
     b1.OffsetRect(-Trunc(Engine.WorldX), - Trunc(Engine.WorldY));

     for cy := Round((b.Top - ChipHeight + 1)) div ChipHeight to Round(b.Bottom) div ChipHeight do
     begin
          for cx := Round((b.Left - ChipWidth+1)) div ChipWidth to Round(b.Right) div ChipWidth do
          begin
              if CollisionMap[Mod2(cx, MapWidth), Mod2(cy, MapHeight)] then
              begin
                B3.CreateBounds(cx * ChipWidth, cy * ChipHeight, ChipWidth, ChipHeight);
                if b3.OverlapRect(b1) then Exit;
              end;
          end;
     end;

     Result := False;
end;

function TBZBackgroundMapSprite.GetCell(vX, vY: Integer): Integer;
begin
     if (vX >= 0) and (vX < FMapWidth) and (vY >= 0) and (vY < FMapHeight) then
         Result := PInteger(PInteger(FMap) + (vY * FMapWidth + vX ) * SizeOf(Integer))^
     else
         Result := -1;
end;

type
  PBoolean = ^Boolean;

function TBZBackgroundMapSprite.GetCollisionMapItem(vX, vY: Integer): Boolean;
begin
     if (vX >= 0) and (vX < FMapWidth) and (vY >= 0) and (vY < FMapHeight) then
         Result := PBoolean(PtrInt(FCollisionMap) + (vY * FMapWidth+vX) * SizeOf(Boolean))^
     else
         Result := False;
end;

function TBZBackgroundMapSprite.GetBoundsRect: TBZFloatRect;
begin
    // if FDoTile then
    //      Result := Rect(0, 0,Engine.VisibleWidth, Engine.VisibleHeight)
    // else
     begin
          if Image <> nil then
               Result.CreateBounds(Trunc(-Engine.WorldX - X), Trunc(-Engine.WorldY - Y),
               Width * FMapWidth, Height * FMapHeight)
          else
               Result.Create(0, 0, 0, 0);
     end;
end;

procedure TBZBackgroundMapSprite.SetCell(vX, vY: Integer; Value: Integer);
begin
     if (vX >= 0) and (vX < FMapWidth) and (vY >= 0) and (vY < FMapHeight) then
         PInteger(PInteger(FMap)^ + (vY * FMapWidth + vX) * SizeOf(Integer))^ := Value;
end;

procedure TBZBackgroundMapSprite.SetCollisionMapItem(vX, vY: Integer; Value: Boolean);
begin
     if (vX >= 0) and (vX < FMapWidth) and (vY >= 0) and (vY < FMapHeight) then
         PBoolean(PInteger(FCollisionMap)^ + (vY * FMapWidth + vX) * SizeOf(Boolean))^ := Value;
end;

procedure TBZBackgroundMapSprite.SetMapHeight(Value: Integer);
begin
     SetMapSize(FMapWidth, Value);
end;

procedure TBZBackgroundMapSprite.SetMapWidth(Value: Integer);
begin
     SetMapSize(Value, FMapHeight);
end;

procedure TBZBackgroundMapSprite.SetMapSize(AMapWidth, AMapHeight: Integer);
begin
     FMapW := Width * AMapWidth;
     FMapH := Height * AMapHeight;
     if (FMapWidth <> AMapWidth) or (FMapHeight <> AMapHeight) then
     begin
          if (AMapWidth <= 0) or (AMapHeight <= 0) then
          begin
               AMapWidth := 0;
               AMapHeight := 0;
          end;
          {else
          begin
              FWidth:=AMapWidth*Image.Width;
              FHeight:=AMapHeight*Image.Height;
          end;
          }
          FMapWidth := AMapWidth;
          FMapHeight := AMapHeight;

          ReAllocMem(FMap, FMapWidth * FMapHeight * SizeOf(Integer));
          FillChar(FMap^, FMapWidth * FMapHeight * SizeOf(Integer), 0);

          ReAllocMem(FCollisionMap, FMapWidth * FMapHeight * SizeOf(Boolean));
          FillChar(FCollisionMap^, FMapWidth * FMapHeight * SizeOf(Boolean), 1);
     end;
end;

{%endregion%}

{%region=====[ TBZCustomSpriteEngine ]========================================}

constructor TBZCustomSpriteEngine.Create(AParent: TBZCustomBaseSprite);
begin
  inherited Create(AParent);
  FDeadList := TList.Create;
end;

destructor TBZCustomSpriteEngine.Destroy;
begin
  FDeadList.Free;
  inherited Destroy;
end;

procedure TBZCustomSpriteEngine.Dead;
begin
  while FDeadList.Count>0 do
    TBZCustomBaseSprite(FDeadList[FDeadList.Count-1]).Free;
end;

procedure TBZCustomSpriteEngine.Draw;
begin
  FDrawCount := 0;
  inherited Draw;
end;

procedure TBZCustomSpriteEngine.SetSurface(Value: TBZBitmap);
begin
  FSurface := Value;
  if FSurface<>nil then
  begin
    Width := FSurface.Width;
    Height := FSurface.Height;
  end;
end;

procedure TBZCustomSpriteEngine.Collision;
var
  i, j: Integer;
  s1,s2 : TBZCustomBaseSprite;
begin
  for i := 0 to FSpriteList.Count - 1 do
  begin
    s1 :=  TBZCustomBaseSprite(FSpriteList.Items[i]);
    for j := i + 1 to FSpriteList.Count - 1 do
    begin
      s2 := TBZCustomBaseSprite(FSpriteList.Items[j]);
      if (s1.EnabledCollision) and (s2.EnabledCollision) then s1.Collision(s2);
    end;
  end;
End;

{%endregion%}

Initialization

Finalization

End.

