(*
  @abstract(Regroupe les classes de base à hériter pour la gestion de shader hardware (OpenGL/Vulkan) ou software.)

  --------------------------------------------------------------------------------

  @created(2018-07-11)
  @author(J.Delauney (BeanzMaster))
  Historique : @br
  @unorderedList(
    @item(07/02/2018 : Creation)
  )

  --------------------------------------------------------------------------------

  @bold(Notes) :

  --------------------------------------------------------------------------------

  @bold(Dependances) : BZClasses, BZVectorMath, BZColors, BZGraphic, BZBitmap, BZCadencer

  --------------------------------------------------------------------------------

  @bold(Credits :)
   @unorderedList(
     @item (basé sur GLScene http://www.sourceforge.net/glscene))
     @item(J.Delauney (BeanzMaster))
   )

  --------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  -------------------------------------------------------------------------------- *)
unit BZCustomShader;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}

//-----------------------------
{$MODESWITCH ADVANCEDRECORDS}
//------------------------
{$ALIGN 16}
{$CODEALIGN CONSTMIN=16}
{$CODEALIGN LOCALMIN=16}
{$CODEALIGN VARMIN=16}
//------------------------

//==============================================================================



interface

uses
  Classes, SysUtils,
  BZClasses, BZVectorMath, BZColors, BZGraphic, BZBitmap, BZCadencer;

Type
  { Exception de base pour les shaders }
  EBZCustomShaderException = Class(EBZBaseException);
  { Exception pour les shaders }
  EBZShaderException = class(EBZCustomShaderException);

  // MOVE IN GLContextClasses. Just here for testing purpose
  TBZRenderingContextInfos = record
    Scene  : TObject;
    Engine : TObject;
    ViewPortSize : TBZVector2i;
  end;
  PBZRenderingContextInfos = ^TBZRenderingContextInfos;

  { TBZShaderFailedInitAction : Définit quoi faire si, pour une raison quelconque, le shader n'a pas pu s'initialiser.
     @unorderedlis(
       @item(@bold(fiaSilentdisable) :          On le désactive tout simplement)
       @item(@bold(fiaRaiseHandledException) :  Lever une exception et la gére tout de suite
                                                (utile, lors du débogage))
       @item(@bold(fiaRaiseStandardException) : Lève l'exception avec une chaîne provenant de la méthode GetStardardNotSupportedMessage
       @item(@bold(fiaReRaiseException) :       Soulève une deuxième exception
       @item(@bold(fiaGenerateEvent) :          Gère l'exception, et génère un événement auquel l'utilisateur peut répondre.
                                                Par exemple, il peut essayez de compiler un shader de substitution, ou le remplacer
                                                par un matériau.
                                                @bold (Remarque): HandleFailedInitialization ne doit pas
                                                créer cet événement, il doit être laissé a l'utilisateur des shaders.
                                                Qui pourra choisir de remplacer cette procédure. C'est pourquoi elle est commenté.
                                                Car je ne sais pas si cette methode devrait exister. Donc à voir avec les générations futures de développeurs)) }
  TBZShaderFailedInitAction = ( fiaSilentDisable, fiaRaiseStandardException,
                                fiaRaiseHandledException, fiaReRaiseException
                                 {,fiaGenerateEvent});

  { Définition d'un cube de texture}
  TBZTextureCube = record
  type
    TFace=(
        POSITIVE_X,
        NEGATIVE_X,
        POSITIVE_Y,
        NEGATIVE_Y,
        POSITIVE_Z,
        NEGATIVE_Z,
        FACE_MAX
    );
    var
      Empty:Boolean;
      Faces:Array[TFace] of TBZBitmap;
  end;

  {%region%-----[ Shader's parameters types ]-----------------------------------}
  { Type des données d'un shader }
  TBZShaderDataType = (
    TypeUndefined,
    Type1F,
    Type2F,
    Type3F,
    Type4F,
    Type1I,
    Type2I,
    Type3I,
    Type4I,
    Type1UI,
    Type2UI,
    Type3UI,
    Type4UI,
    Type4UB, // ??????
    TypeMat2F,
    TypeMat3F,
    TypeMat4F,
    TypeVoid);

  { Type d'un "Sample" (texture) d'un shader }
  TBZShaderSamplerType = (
    SamplerUndefined,
    Sampler1D,
    Sampler2D,
    Sampler3D,
    SamplerCube,
    Sampler1DShadow,
    Sampler2DShadow,
    Sampler1DArray,
    Sampler2DArray,
    Sampler1DArrayShadow,
    Sampler2DArrayShadow,
    SamplerCubeShadow,
    IntSampler1D,
    IntSampler2D,
    IntSampler3D,
    IntSamplerCube,
    IntSampler1DArray,
    IntSampler2DArray,
    UIntSampler1D,
    UIntSampler2D,
    UIntSampler3D,
    UIntSamplerCube,
    UIntSampler1DArray,
    UIntSampler2DArray,
    SamplerRect,
    SamplerRectShadow,
    SamplerBuffer,
    IntSamplerRect,
    IntSamplerBuffer,
    UIntSamplerRect,
    UIntSamplerBuffer,
    SamplerMS,
    IntSamplerMS,
    UIntSamplerMS,
    SamplerMSArray,
    IntSamplerMSArray,
    UIntSamplerMSArray);
  {%endregion%}

  TBZCustomShader = Class;
  TBZCustomShaderProgram = Class;
 // TBZVertexShaderProgram = Class;
 // TBZFragmentShaderProgram = Class;


  TBZShaderEvent = procedure(Shader: TBZCustomShader; rci:Pointer) of object;
  TBZShaderEventOnUnApply = procedure(Shader: TBZCustomShader; rci:Pointer; var ThereAreMorePasses: Boolean) of object;
  TBZShaderEventEx = procedure(Shader: TBZCustomShader; Sender: TObject; rci:Pointer) of object;
  TBZShaderEventOnUniformInitialize = procedure(Sender: TBZCustomShader) of object;
  TBZShaderEventOnBindAttribLocation = procedure(Sender: TBZCustomShader) of object;
  //TOnShaderUniformSetting    = procedure(Sender: TBZCustomShader; var ARci: TRenderContextInfo) of object;

  { Classe à hériter pour représenter un Shader }
  TBZAbstractShader = Class(TBZUpdateAbleObject)
  private
    FName : String;
    FEnabled: Boolean;

    FUpdateCount: Integer;
    FShaderActive: Boolean;
    FDebugMode : Boolean;
    FFailedInitAction: TBZShaderFailedInitAction;
    FTagObject: TObject;

    procedure SetDebugMode(const Value: Boolean);
  protected
    FPassCount:Integer;
    FCurrentPass : Integer;
    FDataNeedUpdate : Boolean;
    FInitialized : Boolean;

    FOnInitialize: TBZShaderEvent;
    FOnApply: TBZShaderEvent;
    FOnUnApply: TBZShaderEventOnUnApply;
    FOnInitializeEx: TBZShaderEventEx;
    FOnApplyEx: TBZShaderEventEx;

    {: Invoked once, before the first call to DoApply.<p>
       The call happens with the OpenGL context being active. }
    procedure DoInitialize(var rci: Pointer; Sender: TObject);virtual;
    {: Request to apply the shader.<p>
     Always followed by a DoUnApply when the shader is no longer needed. }
    procedure DoApply(var rci: Pointer; Sender: TObject); virtual;
    {: Request to un-apply the shader.<p>
     Subclasses can assume the shader has been applied previously.<br>
     Return True to request a multipass. }
    function DoUnApply(var rci: Pointer): Boolean; virtual;
    {: Invoked once, before the destruction of context or release of shader.<p>
     The call happens with the OpenGL context being active. }
    procedure DoFinalize; dynamic;

    function GetShaderInitialized: Boolean;

    procedure InitializeShader(var rci: Pointer; Sender: TObject); Virtual;
    procedure FinalizeShader;

   // procedure OnVirtualHandleAllocate(sender: TBZVirtualHandle; var handle: Cardinal);
   // procedure OnVirtualHandleDestroy(sender: TBZVirtualHandle; var handle: Cardinal);

    procedure SetEnabled(val: Boolean);
    {: Used by the DoInitialize procedure of descendant classes to raise errors. }
    procedure HandleFailedInitialization(const LastErrorMessage: string = ''); virtual;

    class function ComputeNameHashKey(const name: string): Integer;

  public

    // constructor Create(AOwner: TComponent); override;
    constructor Create; override; //(AOwner: TComponent);// override;
    destructor Destroy; override;

    {: Subclasses should invoke this function when shader properties are altered.
      This procedure can also be used to reset/recompile the shader. }
    procedure NotifyChange(Sender: TObject);override;
    procedure BeginUpdate;
    procedure EndUpdate;


    {: Apply shader to state machine.}
    procedure Apply(var rci: Pointer; Sender: TObject); virtual; abstract;

    {: UnApply shader.<p>
     When returning True, the caller is expected to perform a multipass
     rendering by re-rendering then invoking UnApply again, until a
     "False" is returned. }
    function UnApply(var rci: Pointer): Boolean; virtual; abstract;

    procedure Assign(Source: TPersistent); override;

    {: Defines if shader is supported by hardware/drivers.
     Default - always supported. Descendants are encouraged to override
     this function. }
    function HardwareShaderSupported: Boolean; virtual;
    function SoftwareShaderSupported: Boolean; virtual;

    {: Defines what to do if for some reason shader failed to initialize.
     Note, that in some cases it cannon be determined by just checking the
     required OpenGL extentions. You need to try to compile and link the
     shader - only at that stage you might catch an error }
    property FailedInitAction: TBZShaderFailedInitAction read FFailedInitAction write FFailedInitAction default fiaRaiseStandardException;

    property ShaderInitialized: Boolean read GetShaderInitialized;
    property ShaderActive: Boolean read FShaderActive;
    property PassCount:Integer read FPassCount;
    property DataNeedUpdate : Boolean read FdataNeedUpdate Write FDataNeedUpdate;
    property TagObject: TObject read FTagObject write FTagObject default nil;
    property DebugMode : Boolean read FDebugMode Write SetDebugMode;
  published
    {: Turns on/off shader application.}
    property Name : String Read FName Write FName;
    property Enabled: Boolean read FEnabled write SetEnabled default True;

    property OnApply: TBZShaderEvent read FOnApply write FOnApply;
    property OnUnApply: TBZShaderEventOnUnApply read FOnUnApply write FOnUnApply;
    property OnInitialize: TBZShaderEvent read FOnInitialize write FOnInitialize;
    property OnInitializeEx: TBZShaderEventEx read FOnInitializeEx write FOnInitializeEx;
    property OnApplyEx: TBZShaderEventEx read FOnApplyEx write FOnApplyEx;

    //    property Name:String read FName write FName;
  end;

  { Classe de type TBZAbstractShader }
  TBZShaderClass = class of TBZAbstractShader;

  { Classe à hériter représentant le programme d'un Shader }
  TBZCustomShaderProgram = class(TPersistent)
  private
    FParent: TBZCustomShader;
    FEnabled: Boolean;
    //FParameters
    //FTextureSamples

    procedure SetEnabled(const Value: Boolean);

  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(const AParent: TBZCustomShader); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Apply; virtual;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
  end;

  { Classe de base à hériter pour gérer un Shader }
  TBZCustomShader = Class(TBZAbstractShader)
  private
    FAttribs  : TBZPersistentObjectList;
    FUniforms : TBZPersistentObjectList;

  protected
    procedure InitializeShader(var rci: Pointer; Sender: TObject);Override;
    //function GetCurrentAttribs: TGLSLShaderParameter; virtual;
    //function GetCurrentUniforms: TGLSLShaderParameter; virtual;
  public
    constructor Create;override;//(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure Apply(var rci: Pointer; Sender: TObject); Override;
    function UnApply(var rci: Pointer): Boolean; Override;

    //function AddAttribItem : TBZShaderAttribItem;
    //function AddUniformItem : TBZShaderUniformItem;
  //published
    //property Attrib[Index: Integer] : TBZShaderAttribItem;
    //property Uniform[Index: Integer] : TBZShaderUniformItem;
  end;


  TBZCustomSoftwareShader = Class;
  { Classe de type TBZCustomSoftwareShader }
  TBZCustomSoftwareShaderClass = class of TBZCustomSoftwareShader;

  { Classe de base à hériter pour la gestion d'un Shader en mode software }
  TBZCustomSoftwareShader = Class(TBZCustomShader)
  private

  protected
    {$IFDEF CPU64}
      {$CODEALIGN RECORDMIN=16}
      FCameraPosition, FCameraDirection, FLightPosition : TBZVector4f;
      FResolution : TBZVector2i;
      FInvResolution : TBZVector2f;
      FFragCoords : TBZVector2f;
      FMousePos : TBZVector2f;
     {$CODEALIGN RECORDMIN=4}
    {$ELSE}
      FCameraPosition, FCameraDirection, FLightPosition : TBZVector4f;
      FResolution, FInvResolution : TBZVector2f;
      FFragCoords : TBZVector2f;
      FMousePos : TBZVector2f;
    {$ENDIF}
    FiChannel0, FiChannel1, FiChannel2, FiChannel3 : TBZBitmap;
    FiTime, FiGlobalTime, FiMouse : Double;

    FCadencer : TBZCadencer;

    procedure DoApply(var rci: Pointer; Sender: TObject); override;
  public

    constructor Create;override;
    destructor Destroy; override;

    function ShadePixel:TBZColor; virtual;// for bitmap software rendering
    function ShadePixelFloat:TBZColorVector; virtual;

    function Clone : TBZCustomSoftwareShader;  virtual;
    procedure Assign(Source: TPersistent); override;

    property iTime : Double read FiTime write FiTime;

    property InvResolution : TBZVector2f read FInvResolution;
    property FragCoords : TBZVector2f read FFragCoords write FFragCoords;

  //published
    property Resolution : TBZVector2i read FResolution write FResolution;

    property CameraPosition : TBZVector4f read FCameraPosition write FCameraPosition;
    property CameraDirection : TBZVector4f read FCameraDirection write FCameraDirection;
    property LightPosition : TBZVector4f read FLightPosition write FLightPosition;

    property iChannel0 : TBZBitmap read FiChannel0 write FiChannel0;
    property iChannel1 : TBZBitmap read FiChannel1 write FiChannel1;
    property iChannel2 : TBZBitmap read FiChannel2 write FiChannel2;
    property iChannel3 : TBZBitmap read FiChannel3 write FiChannel3;

    property Cadencer : TBZCadencer read FCadencer write FCadencer;
  end;

  //TBZCustomGLSLShader = Class(TBZCustomSoftwareShader)
  { private
      FVertexProgram    : TBZVertexShaderProgram;
      FFragmentProgram  : TBZFragmentShaderProgram;
      FGeometryProgram  : TBZGeometryShaderProgram;
      //FTesselateProgram : TBZTesselateShaderProgram;

      procedure SetFragmentProgram(const Value: TDGLFragmentProgram);
      procedure SetVertexProgram(const Value: TDGLVertexProgram);

      function StoreFragmentProgram: Boolean;
      function StoreVertexProgram: Boolean;
    protected
      property FragmentProgram: TDGLFragmentProgram read FFragmentProgram write SetFragmentProgram stored StoreFragmentProgram;
      property VertexProgram: TDGLVertexProgram read FVertexProgram write SetVertexProgram stored StoreVertexProgram;
    public
    end;

  }


// Quelques fonction utiles pour les "Software shader"
function texture2DLQ(tex:TBZBitmap;const Coords:TBZVector2f):TBZColorVector;
function texture2DHQ(tex:TBZBitmap;const Coords:TBZVector2f):TBZColorVector;
function texture2D(tex:TBZBitmap;const Coords:TBZVector2f; Const HQ:Boolean =true):TBZColorVector;
function textureCube(tex: TBZBitmap; const Coords: TBZVector4f): TBZColorVector;
function textureCube(const tex: TBZTextureCube; const Coords: TBZVector4f): TBZColorVector;

implementation

uses BZLogger, BZTypesHelpers;

{%region%=====[ Shader Utilities functions ]====================================}

function texture2DHQ(tex:TBZBitmap;const Coords:TBZVector2f):TBZColorVector;
var
{$IFDEF CPU64}
  {$CODEALIGN VARMIN=16}
  ts,t : TBZVector2f;
  st : TBZVector2i;
  {$CODEALIGN VARMIN=4}
//  c: TBZColor;
{$ELSE}
  t, ts : TBZVector2f;
  st : TBZVector2i;
  //c: TBZColor;
{$ENDIF}
begin
  ts.Create(Tex.Width, Tex.Height);
  t := Coords.abs;
  t := t * ts;
  st := t.fMod(ts);
  //C:= Tex.getPixel(st.x, Tex.Height-st.y-1); //Flip vert
  Result := Tex.getPixel(st.x, st.y).AsColorVector;
  //Result := C.AsColorVector;
End;

function texture2DLQ(tex:TBZBitmap;const Coords:TBZVector2f):TBZColorVector;
var
{$IFDEF CPU64}
  {$CODEALIGN VARMIN=16}
  t,ts : TBZVector2i;
  p : TBZVector2f;
  c: TBZColor;
  {$CODEALIGN VARMIN=4}
{$ELSE}
  t, ts : TBZVector2i;
  p : TBZVector2f;
  c: TBZColor;
{$ENDIF}
begin
  ts.Create(Tex.Width, Tex.Height);
  p := Coords;
  p := Coords.Abs;
  p := (p * ts);
  t:=p.Round;
  //t := Coords.Round;
  t.x := t.x mod ts.Width;
  t.y := t.y mod ts.Height;
  //uv.Create(t.x,ts.Height-t.Y-1);
  C:= Tex.getPixel(t.x, t.y);
  Result := C.AsColorVector;
end;

function texture2D(tex:TBZBitmap;const Coords:TBZVector2f; Const HQ:Boolean =true):TBZColorVector;
begin
  if HQ then Result := texture2DHQ(Tex,Coords)
  else Result := texture2DLQ(Tex,Coords)
end;

function textureCube(tex:TBZBitmap;const Coords:TBZVector4f):TBZColorVector;
var
{$IFDEF CPU64}
  {$CODEALIGN VARMIN=16}
  cx,cy,cz  :TBZColorVector;
  {$CODEALIGN VARMIN=4}
{$ELSE}
  cx,cy,cz  :TBZColorVector;
{$ENDIF}
begin

  //Result.x  := texture2D( tex, Coords.yz ).x;
  //Result.y  := texture2D( tex, Coords.zx ).y;
  //Result.z  := texture2D( tex, Coords.xy ).z;
  //Result.w  := 1;

  cx  := texture2D( tex, Coords.yz );
  cy  := texture2D( tex, Coords.zx );
  cz  := texture2D( tex, Coords.xy );

  Result := (cx+cy+cz)*0.33;

end;

function textureCube(const tex:TBZTextureCube;const Coords:TBZVector4f):TBZColorVector;
var
  MaxVal:Double;
  f:TBZTextureCube.TFace;
  {$IFDEF CPU64}
    {$CODEALIGN VARMIN=16}
    t : TBZVector4f;
    c,c1,c2  :TBZVector2f;
    {$CODEALIGN VARMIN=4}
  {$ELSE}
    t : TBZVector4f;
    c,c1,c2  :TBZVector2f;
  {$ENDIF}
begin
//  Result := texture2D( tex.Faces[ ttexturecube.TFace.POSITIVE_X ], -coords.xy );

(* Texture Cube Mapping
  http://www.ozone3d.net/tutorials/glsl_texturing_p04.php

  the function to fetch cubemap texels is textureCube().
  The first parameter is a samplerCube and the second is a XYZ vector
  that allows the function textureCube() to select the right face of
  the cubemap and then to extract the texel.

  The functioning of textureCube() could be as follow:
    the coordinate with the largest magnitude selects the face.
    The remaining two coordinates are divided by the absolute value of the
    largest coordinate and are mapped to the range [0.0 - 1.0].

    Example: the vector R = {0.287, -0.944, 0.164}

    selects the NEG_Y face.
    The texture coordinates {s, t} are calculated as follow:
    s = (0.287/0.944*0.5) + 0.5 and
    t = (0.164/0.944*0.5) + 0.5 then
    {s, t} = {0.65, 0.58}.

    The vector R is the same as for the DPEM ( Dual Paraboloid Environment Mapping).
*)
   t:=Coords.Abs;
   MaxVal := t.MaxXYZComponent; //Math.Max(Math.Max(t.x, t.y)), t.z));

   c.Create(0,0);
   f := TBZTextureCube.TFace.POSITIVE_X;

   if (t.x = maxVal) then
   begin
     if t.x<>0 then
       if t.x > 0 then
       begin
         f := TBZTextureCube.TFace.POSITIVE_X;
         c1 := Coords.xx * 0.5;
         c2 := Coords.zy;
         c:= (c2 / c1) + 0.5;
       end
       else
       begin
         f := TBZTextureCube.TFace.NEGATIVE_X;
         c1 := -Coords.xx * 0.5;
         c2 := Coords.zy;
         c:= (c2 / c1) + 0.5;
       end;
   end
   else if (t.y = maxVal) then
   begin
     if t.y<>0 then
       if t.y > 0 then
       begin
         f := TBZTextureCube.TFace.POSITIVE_Y;
         c1 := Coords.yy * 0.5;
         c2 := Coords.xz;
         c := (c2 / c1) + 0.5;
       end
       else
       begin
         f := TBZTextureCube.TFace.NEGATIVE_Y;
         c1 := -Coords.yy * 0.5;
         c2 := Coords.xz;
         c := (c2 / c1) + 0.5;
       end;
   end
   else if (t.z = maxVal) then
   begin
     if t.z<>0 then
       if t.z > 0 then
       begin
         f := TBZTextureCube.TFace.POSITIVE_Z;
         c1 := Coords.zz * 0.5;
         c2 := Coords.xy;
         c := (c2 / c1) + 0.5;
       end
       else
       begin
         f := TBZTextureCube.TFace.NEGATIVE_Z;
         c1 := -Coords.zz * 0.5;
         c2 := Coords.xy;
         c := (c2 / c1) + 0.5;
       end;
   end;

   Result := texture2D(tex.Faces[f], c );
end;

//function TexCube(sam: TBZBitmap; const p:TBZVector4f;const n :TBZVector4f ):TBZVector4f;
//var
//{$IFDEF CPU64}
//  {$CODEALIGN VARMIN=16}
//  cx,cy,cz,nv :TBZVector4f;
//  {$CODEALIGN VARMIN=4}
//{$ELSE}
//  cx,cy,cz,nv :TBZVector4f;
//{$ENDIF}
//begin
//  cx  := texture2D( sam, p.yz );
//  cy  := texture2D( sam, p.zx );
//  cz  := texture2D( sam, p.xy );
//  nv := n.Abs;
//  Result := cx*nv;
//  //Exit( x*abs(n.x) + y*abs(n.y) + z*abs(n.z) );
//end;

{%endregion%}

{%region%=====[ TBZAbstractShader ]============================================}

constructor TBZAbstractShader.Create;//(AOwner: TComponent);
begin
  inherited Create; //(AOwner);
  FEnabled := False;
  FUpdateCount := 0;
  FShaderActive := False;
  FDebugMode := False;
  FDataNeedUpdate := True;
  FInitialized := False;
  FName := '';
end;

destructor TBZAbstractShader.Destroy;
begin
  FinalizeShader;
  inherited Destroy;
end;

procedure TBZAbstractShader.SetDebugMode(const Value: Boolean);
begin
  if FDebugMode = Value then exit;
  FDebugMode := Value;
  if FDebugMode then
    FailedInitAction := fiaReRaiseException
  else
    FailedInitAction := fiaRaiseStandardException;
end;

procedure TBZAbstractShader.DoInitialize(var rci: Pointer;Sender: TObject);
begin
 // FInitialized := true;
end;

procedure TBZAbstractShader.DoApply(var rci: Pointer;Sender: TObject);
begin
  // Nothing to do here
end;

function TBZAbstractShader.DoUnApply(var rci: Pointer): Boolean;
begin
  // Nothing to do Here
  result := true;
end;

procedure TBZAbstractShader.DoFinalize;
begin
 // Nothing to do here
end;

function TBZAbstractShader.GetShaderInitialized: Boolean;
begin
  result := FInitialized;
end;

procedure TBZAbstractShader.InitializeShader(var rci: Pointer; Sender: TObject);
begin
  // Nothing to do here
  FInitialized := True;
end;

procedure TBZAbstractShader.FinalizeShader;
begin
  DoFinalize;
end;

//procedure TBZAbstractShader.OnVirtualHandleAllocate(sender: TBZVirtualHandle;var handle: Cardinal);
//begin
//
//end;
//
//procedure TBZAbstractShader.OnVirtualHandleDestroy(sender: TBZVirtualHandle;var handle: Cardinal);
//begin
//
//end;

procedure TBZAbstractShader.SetEnabled(val: Boolean);
begin
  if FEnabled = val then exit;
  FEnabled := val;
  NotifyChange(Self);
end;

procedure TBZAbstractShader.HandleFailedInitialization( const LastErrorMessage: string);
  function GetStandardNotSupportedMessage: string;
  begin
    if Name <> '' then
      Result := 'Shader Error : unsupported shader "' + Name + '"!'
    else
      Result := 'Shader Error : unsupported shader "' + ClassName + '"!';
  end;
begin
  case FailedInitAction of
    fiaSilentdisable: ; // Do nothing ;)
    fiaRaiseHandledException:
      try
        //Logger.LogError(GetStandardNotSupportedMessage);
        raise EBZCustomShaderException.Create(GetStandardNotSupportedMessage);
      except
      end;
    fiaRaiseStandardException:
    begin
      //Logger.LogError(GetStandardNotSupportedMessage);
      raise EBZCustomShaderException.Create(GetStandardNotSupportedMessage);
    end;
    fiaReRaiseException:
      begin
        if LastErrorMessage <> '' then
        begin
          //Logger.LogError(LastErrorMessage);
          raise EBZCustomShaderException.Create(LastErrorMessage);
        end
        else
        begin
          //Logger.LogError(GetStandardNotSupportedMessage);
          raise EBZCustomShaderException.Create(GetStandardNotSupportedMessage)
        end;
      end;
    //    fiaGenerateEvent:; // Do nothing. Event creation is left up to user shaders
    //                       // which may choose to override this procedure.
  else
    Assert(False, 'Shader : Error Unknown');
  end;
end;

class function TBZAbstractShader.ComputeNameHashKey(const name: string): Integer;
var
  i, n: Integer;
begin
  n := Length(name);
  Result := n;
  for i := 1 to n do
    Result := (Result shl 1) + Byte(name[i]);
end;

procedure TBZAbstractShader.NotifyChange(Sender: TObject);
begin
  inherited NotifyChange(Sender);
end;

procedure TBZAbstractShader.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TBZAbstractShader.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then NotifyChange(Self);
end;

procedure TBZAbstractShader.Assign(Source: TPersistent);
begin
  if Source = nil then Exit;
  if Source is TBZAbstractShader then
  begin
    // GlobalLogger.LogNotice(' TBZAbstractShader.Assign : Enabled = '+ TBZAbstractShader(Source).Enabled.ToString());
    FTagObject := TBZAbstractShader(Source).TagObject;
    FEnabled := TBZAbstractShader(Source).Enabled;
    FName := TBZAbstractShader(Source).Name;
    FDebugMode := TBZAbstractShader(Source).DebugMode;
    FOnInitialize  := TBZAbstractShader(Source).OnInitialize;
    FOnApply := TBZAbstractShader(Source).OnApply;
    FOnUnApply := TBZAbstractShader(Source).OnUnApply;
    FOnInitializeEx := TBZAbstractShader(Source).OnInitializeEx;
    FOnApplyEx := TBZAbstractShader(Source).OnApplyEx;
    //inherited Assign(Source);
  end;
  //else
  //  inherited Assign(Source);
end;

function TBZAbstractShader.HardwareShaderSupported: Boolean;
begin
  result := false;
end;

function TBZAbstractShader.SoftwareShaderSupported: Boolean;
begin
  result := false;
end;

{%endregion%}

{%region%=====[ TBZCustomShaderProgram ]=======================================}

constructor TBZCustomShaderProgram.Create(const AParent: TBZCustomShader);
begin
  inherited Create;
  FParent := AParent;
  FEnabled := False;
end;

destructor TBZCustomShaderProgram.Destroy;
begin
  inherited Destroy;
end;

procedure TBZCustomShaderProgram.SetEnabled(const Value: Boolean);
begin
 if Value = FEnabled then Exit;
 FEnabled := Value;
 if FEnabled then FParent.FinalizeShader;
end;

function TBZCustomShaderProgram.GetOwner: TPersistent;
begin
  Result := FParent;
end;

procedure TBZCustomShaderProgram.Assign(Source: TPersistent);
begin
  if Source = nil then Exit;
  if (Source is TBZCustomShaderProgram) then
  begin
    inherited Assign(Source);
   // Enabled := TBZCustomShaderProgram(Source).Enabled;
  end
  else
    inherited Assign(Source);
end;

procedure TBZCustomShaderProgram.Apply;
begin
  FParent.FinalizeShader;
end;

{%endregion%}

{%region%=====[ TBZCustomShader ]==============================================}

constructor TBZCustomShader.Create; //(AOwner: TComponent);
begin
  inherited Create; //(AOwner);
end;

destructor TBZCustomShader.Destroy;
begin
  inherited Destroy;
end;

procedure TBZCustomShader.Assign(Source: TPersistent);
begin
  if Source = nil then Exit;
  if Source is TBZCustomShader then
  begin
    // FAttribs
    // FUniforms
    //GlobalLogger.logNotice('TBZCustomShader.Assign ==> Enabled : ' + TBZCustomShader(Source).Enabled.ToString());
    inherited Assign(Source);
  end
  else
    inherited Assign(Source);
end;

procedure TBZCustomShader.InitializeShader(var rci: Pointer; Sender: TObject);
begin
  //DoInitialize(rci,Sender);
  try
    if (not(HardwareShaderSupported) and not(SoftwareShaderSupported)) then HandleFailedInitialization
    else
    try
      if Not(FInitialized) then DoInitialize(rci,Sender);
    except
      on E: Exception do
      begin
        Enabled := False;
        HandleFailedInitialization(E.Message);
      end;
    end;
  finally
    if Enabled and DataNeedUpdate then
    try
      if Assigned(FOnInitialize) then FOnInitialize(Self,rci);
      if Assigned(FOnInitializeEx) then FOnInitializeEx(Self, Sender,rci);
    except
      on E: Exception do
      begin
        Enabled := False;
        HandleFailedInitialization(E.Message);
      end;
    end;
  end;
  Inherited InitializeShader(rci,Sender);
end;

procedure TBZCustomShader.Apply(var rci: Pointer; Sender: TObject);
begin
  if FEnabled then
  begin
   // GLobalLogger.LogNotice('TBZCustomShader.Apply');
    //GlobalLogger.logNotice('TBZCustomShader.Apply ==> Resolution : '+inttostr(PBZRenderingContextInfos(RCI)^.ViewPortSize.Width)+', '+inttostr(PBZRenderingContextInfos(RCI)^.ViewPortSize.Height));
    if (FDataNeedUpdate=True) or (FInitialized= false) then InitializeShader(rci, Sender);
    DoApply(rci, Sender);

    if Assigned(FOnApply) then FOnApply(Self, rci);
    if Assigned(FOnApplyEx) then FOnApplyEx(Self, Sender, rci);
    FShaderActive := True;
  end
  else FShaderActive := False;
 // Inherited Apply(rci,Sender);
end;

function TBZCustomShader.UnApply(var rci: Pointer): Boolean;
begin
  Result := False;
  if Enabled then
  begin
    Result :=DoUnApply(rci);
      //Logger.LogInfo('------> Shader Enabled');
    if not Result then FShaderActive := False;
    if Assigned(FOnUnApply) then FOnUnApply(Self, rci, Result);
  end
  else
  begin
    //Logger.LogInfo('------> Shader Disabled');
    FShaderActive := False;
  end;
end;

{%endregion%}

{%region%=====[ TBZCustomSoftwareShader ]================================================}

constructor TBZCustomSoftwareShader.Create;
begin
  inherited Create;
  FiChannel0 := nil;
  FiChannel1 := nil;
  FiChannel2 := nil;
  FiChannel3 := nil;
  FCadencer := nil;
  FResolution.Create(0,0);
  FInvResolution.Create(0,0);
  FCameraPosition.CreatePoint(0, 0, -5);
  FLightPosition.CreatePoint( 2.0, 4.5, -2.0); //(2.0, -5.0, 3.0);
  FiTime := 0;
end;

destructor TBZCustomSoftwareShader.Destroy;
begin
  if FiChannel0<>nil then FreeAndNil(FiChannel0);
  if FiChannel1<>nil then FreeAndNil(FiChannel1);
  if FiChannel2<>nil then FreeAndNil(FiChannel2);
  if FiChannel3<>nil then FreeAndNil(FiChannel3);
  inherited Destroy;
end;

procedure TBZCustomSoftwareShader.DoApply(var rci : Pointer; Sender : TObject);
var
  {$CODEALIGN VARMIN=16}
  r : TBZVector2f;
  {$CODEALIGN VARMIN=4}
begin
  if Assigned(FCadencer) then FiTime := FCadencer.CurrentTime
  else FiTime := 0;
  FResolution := PBZRenderingContextInfos(rci)^.ViewPortSize;
  FInvResolution.Create(1,1);
  r.Create(FResolution.X, FResolution.Y);
  FInvResolution := InvResolution / r; //FResolution;
end;

function TBZCustomSoftwareShader.Clone : TBZCustomSoftwareShader;
begin
  Result := TBZCustomSoftwareShader.Create;
  Result.Assign(Self);
end;

procedure TBZCustomSoftwareShader.Assign(Source : TPersistent);
begin
  if Source = nil then exit;
  if (Source is  TBZCustomSoftwareShader) then
  begin
   // GlobalLogger.LogNotice(' TBZCustomSoftwareShader.Assign : Enabled = '+ TBZCustomSoftwareShader(Source).Enabled.ToString());
    FFragCoords := TBZCustomSoftwareShader(Source).FragCoords;
    FCameraPosition := TBZCustomSoftwareShader(Source).CameraPosition;
    FCameraDirection := TBZCustomSoftwareShader(Source).CameraDirection;
    FLightPosition  := TBZCustomSoftwareShader(Source).LightPosition;
    FCadencer  := TBZCustomSoftwareShader(Source).Cadencer;
    FResolution := TBZCustomSoftwareShader(Source).Resolution;
    FInvResolution  := TBZCustomSoftwareShader(Source).InvResolution;
    if (TBZCustomSoftwareShader(Source).iChannel0<>nil) then
    begin
      FiChannel0  := TBZBitmap.Create;
      FiChannel0.Assign(TBZCustomSoftwareShader(Source).iChannel0);
    end;
    if (TBZCustomSoftwareShader(Source).iChannel1<>nil) then
    begin
      FiChannel1  := TBZBitmap.Create;
      FiChannel1.Assign(TBZCustomSoftwareShader(Source).iChannel1);
    end;
    if (TBZCustomSoftwareShader(Source).iChannel2<>nil) then
    begin
      FiChannel2  := TBZBitmap.Create;
      FiChannel2.Assign(TBZCustomSoftwareShader(Source).iChannel2);
    end;
    if (TBZCustomSoftwareShader(Source).iChannel3<>nil) then
    begin
      FiChannel3  := TBZBitmap.Create;
      FiChannel3.Assign(TBZCustomSoftwareShader(Source).iChannel3);
    end;
    FiTime  := TBZCustomSoftwareShader(Source).iTime;
    //FMousePos
    inherited Assign(Source);
  end
  else
    inherited Assign(Source);
end;

function TBZCustomSoftwareShader.ShadePixel : TBZColor;
begin
  Result.Create(ShadePixelFloat); // Transparent;
end;

function TBZCustomSoftwareShader.ShadePixelFloat : TBZColorVector;
begin
 // GlobalLogger.LogStatus('TBZCustomSoftwareShader.ShadePixelFloat');
 // Result.Create(0,0,0,0); // Transparent;
end;


{%endregion%}

end.

