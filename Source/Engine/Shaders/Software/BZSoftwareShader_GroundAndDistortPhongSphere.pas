(*
@created(2019-12-27)
@author(J.Delauney (BeanzMaster))
Historique : @br
@unorderedList(
  @item(27/12/2019 : Creation  )
)
--------------------------------------------------------------------------------@br
  ------------------------------------------------------------------------------
  Description :  Simulation d'un shader GLSL

  Code source de référence de Wouter Van Nifterick


  ------------------------------------------------------------------------------
  @bold(Notes :)

  Quelques liens :
   @unorderedList(
       @item(http://www.shadertoy.com)
       @item(http://glslsandbox.com)
     )
  ------------------------------------------------------------------------------@br

  @bold(Credits :)
    @unorderedList(
      @item(J.Delauney (BeanzMaster))
      @item(https://github.com/WouterVanNifterick/delphi-shader)
    )

  ------------------------------------------------------------------------------@br
  LICENCE : GPL/MPL
  @br
  ------------------------------------------------------------------------------
 *==============================================================================*)
unit BZSoftwareShader_GroundAndDistortPhongSphere;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\..\..\bzscene_options.inc}

//------------------------
{$ALIGN 16}
{$CODEALIGN CONSTMIN=16}
{$CODEALIGN LOCALMIN=16}
{$CODEALIGN VARMIN=16}
//------------------------
//==============================================================================

interface

uses
  Classes, SysUtils, Math,
  BZClasses, BZMath, BZVectorMath, BZRayMarchMath,
  BZColors, BZGraphic, BZBitmap,
  BZCadencer, BZCustomShader;

Type

  { TBZSoftShader_GroundAndDistortPhongSphere }
  TBZSoftShader_GroundAndDistortPhongSphere = Class(TBZCustomSoftwareShader)
  protected
  public
    Constructor Create; override;
    Destructor Destroy; override;

    function Clone : TBZCustomSoftwareShader; override;

    function ShadePixelFloat:TBZColorVector; override;
  end;

implementation

uses BZLogger;

{ TBZSoftShader_GroundAndDistortPhongSphere }

Constructor TBZSoftShader_GroundAndDistortPhongSphere.Create;
begin
  inherited Create;
end;

Destructor TBZSoftShader_GroundAndDistortPhongSphere.Destroy;
begin
  inherited Destroy;
end;

function TBZSoftShader_GroundAndDistortPhongSphere.Clone : TBZCustomSoftwareShader;
begin
  Result := TBZSoftShader_GroundAndDistortPhongSphere.Create;
  Result.Assign(Self);
end;

function TBZSoftShader_GroundAndDistortPhongSphere.ShadePixelFloat : TBZColorVector;
var
  ttime : single;

  // Converted from articles :
  // - http://xdpixel.com/ray-marching-101-part-1/
  // - http://www.michaelwalczyk.com/blog/2017/5/25/ray-marching

  // The map function is the function that defines our scene.
  // Here we can define the relationship between various objects
  // in our scene.
  // To keep things simple for now, we only have a single sphere in our scene.
  function RayMarch_Map(p : TBZVector4f): TBZVector2f;
  var
    displacement,tt: Single;
    {$CODEALIGN VARMIN=16}
    t:TBZVector4f;
    SphereD : TBZVector2f;
    {$CODEALIGN VARMIN=4}
  begin

    //s:=Sin(2.0+iTime);
    //s:=Clamp(s,-cPi,cPi);
    tt:=40*Sin(iTime) * 0.15;
    //ttime :=0.5*itime;
    t := p * tt; //fmodf(tt,cPI);

   // t:=t.Clamp(-cPIdiv2,cPIdiv2);
    //displacement := (Sin(t.X)) * (Sin(t.Y)) * (Sin(t.Z)) * 0.25 ;
    displacement := (Sin(t.X) * (Sin(t.Y*0.5)) * Sin(t.Z)) * 0.38 ;
    //displacement:= Clamp(displacement,-cPi,cPI);
    SphereD.X := DistanceSphere(p,3.0);
    SphereD.x := SphereD.x + Displacement;
    SphereD.Y := 1.0;
    result := opUnion(DistancePlane(p,10), SphereD);
    //if  (displacement>0.25) and (displacement<0.50)then
   // result := result + displacement;

  end;

  function RayMarch_ComputeNormal(p : TBZVector4f; aDist : TBZVector2f):TBZVector4f;
  {$CODEALIGN CONSTMIN=16}
  const small_step : TBZVector4f = (x:0.002; y:0.0; z:0.0;w:0.0);
  {$CODEALIGN CONSTMIN=4}
  var
    {$CODEALIGN VARMIN=16}
    aNormal : TBZVector4f;
    {$CODEALIGN VARMIN=4}
    gradient_X,gradient_Y,gradient_Z : Single;

  begin
    // If you are unfamiliar with swizzling in GLSL, we are basically using some syntactic
    // "sugar" to add and subtract the X-coordinate of the variable small_step (which is 0.001)
    // to each of the X/Y/Z coordinates of our original point p in succession.
    // So the value of gradient_y, for example, is calculated by adding and subtracting 0.001
    // from only the Y-coordinate of p, then calling map_the_world at these two new points.
    // Now, back inside of our ray marching loop, if we hit an object, we can calculate the normal
    // at that point. Let's visualize our normals as RGB colors to verify that the code is working
    // as expected:

    //gradient_X := RayMarch_Map(p + small_step.xyy) - RayMarch_Map(p - small_step.xyy);
    //gradient_Y := RayMarch_Map(p + small_step.yxy) - RayMarch_Map(p - small_step.yxy);
    //gradient_Z := RayMarch_Map(p + small_step.yyx) - RayMarch_Map(p - small_step.yyx);

    // optimized version
    gradient_X := aDist.x - RayMarch_Map(p - small_step.xyy).X;
    gradient_Y := aDist.x - RayMarch_Map(p - small_step.yxy).X;
    gradient_Z := aDist.x - RayMarch_Map(p - small_step.yyx).X;

    anormal.CreatePoint(gradient_x, gradient_y, gradient_z);

    result := aNormal.Normalize;
  end;

  // Standard Blinn lighting model.
  // This model computes the diffuse and specular components of the final surface color.
  function ComputeBlinnPhongLighting(MaterialColor : TBZColorVector; pointOnSurface, surfaceNormal, lightPosition, cameraPosition:TBZVector4f):TBZColorVector;
  var
    {$CODEALIGN VARMIN=16}
    t, reflectedLightVector, fromPointToLight, fromPointToCamera : TBZVector4f;
    diffuseColor,
    finalColor,
    specularColor  : TBZColorVector;

    {$CODEALIGN VARMIN=4}
    f,diffuseStrength, specularStrength : Single;

    function DoReflect(ConstRef I, N: TBZVector4f): TBZVector4f;
    var
      d : Single;
    begin
    //Self - (N*(N.DotProduct(Self)*2));
    d:= N.DotProduct(I);
    d:=d+d;
    Result := I - (N*d);
    end;

    function DoDotProduct(constref I, A: TBZVector4f) : Single;
    var
      {$CODEALIGN VARMIN=16}
      V2:TBZVector4f;
      {$CODEALIGN VARMIN=4}
    begin
      V2.X := I.X*A.X;
      V2.Y := I.Y*A.Y;
      V2.Z := I.Z*A.Z;
      //V2.W := Self.W*A.W;
      Result := V2.X + V2.Y + V2.Z;// + V2.W;
    end;

    function DoNormalize(ConstRef A : TBZVector4f) : TBZVector4f;
    var
      invLen : Single;
      vn : single;
    begin
      vn:=A.Norm;
      if vn=0 then
      begin
        result := A;
    //    result.W := 0; //cZero;   // either passed in a Vec or point ensure Result is of same type,
      end
      else
      begin
        invLen:=1/System.Sqrt(vn);
        result.X:=A.X*invLen;
        result.Y:=A.Y*invLen;
        result.Z:=A.Z*invLen;
        result.W:=A.W;
      end;
    end;

  begin
    fromPointToLight := lightPosition - pointOnSurface;
    fromPointToLight := fromPointToLight.Normalize;
    //fromPointToLight := DoNormalize(fromPointToLight);
    //f := DoDotProduct(surfaceNormal, fromPointToLight); //
    f:=surfaceNormal.DotProduct(fromPointToLight);
    diffuseStrength := clamp(f, 0.0, 1.0 );

    diffuseColor := MaterialColor * diffuseStrength;
    t:= -fromPointToLight;
    //t:= DoReflect(t,SurfaceNormal); //
    t:=t.Reflect(surfaceNormal);
    reflectedLightVector := t.normalize;
    fromPointToCamera :=  cameraPosition - pointOnSurface;
    fromPointToCamera := fromPointToCamera.Normalize;
    specularStrength := power( clamp( reflectedLightVector.DotProduct(fromPointToCamera), 0.0, 1.0 ), 10.0 );
    //specularStrength := power( clamp( DoDotProduct(reflectedLightVector,fromPointToCamera), 0.0, 1.0 ), 10.0 );

    // Ensure that there is no specular lighting when there is no diffuse lighting.
    specularStrength := min( diffuseStrength, specularStrength );
    specularColor.Create(specularStrength);

    finalColor := diffuseColor + specularColor;
    finalColor.Alpha :=1.0;
    result := finalColor;
  end;

  // The trace function is our integration function.
  // Given a starting point and a direction, the trace
  // function will return the distance from a point on the ray
  // to the closest point on an object in the scene.  In order for
  // the trace function to work properly, we need functions that
  // describe how to calculate the distance from a point to a point
  // on a geometric object.
  // NOTE : Here we calculate the material and lighting and we return the render color
  // instead of the distance
  function RayMarch_Trace( origin, direction : TBZVector4f): TBZColorVector;
  var
    {$CODEALIGN VARMIN=16}
    p, n : TBZVector4f;
    c : TBZColorVector;
    dist : TBZVector2f;
    r : TBZColorVector;
    {$CODEALIGN VARMIN=4}
    totalDistanceTraveled : Single;
    //distanceFromPointOnRayToClosestObjectInScene : Single;

    i : Integer;

    function getfloor_color(p:TBZVector4f):TBZColorVector;
    var
      f, fz:single;
    begin
      f:= fract(p.x*0.2);
      if (f>0.2) or (f<-0.2) then
      begin
        fz := fract(p.z*0.2);
        if (fz>0.2) then result.Create(0,0.1,0.2,1.0)
        else result.Create(1,1,1,1);
      end
      else
      begin
        fz := fract(p.z*0.2);
        if (fz>0.2) then result.Create(1,1,1,1)
        else result.Create(0.3,0,0,1.0);

      end;
    end;

  Begin
    result.Create(0,0,0,0); //return transparent; //totalDistanceTraveled;
    //n.Create(0,0,0,0);
    totalDistanceTraveled := 0.0;

    // When ray marching,  you need to determine how many times you
    // want to step along your ray.  The more steps you take, the better
    // image quality you will have however it will also take longer to render.
    // 32 steps is a pretty decent number for software rendering (64 and more for hardware).
    // This Step depend of the complexity of scene and on the depth and quality you want.
    // You can play with step count in other ray marchign examples to get an intuitive
    // feel for how this will affect your final image render.
    for i:=0 to 63 do
    begin
      // Here we march along our ray and store the new point
      // on the ray in the "p" variable.
      if (i>0) or (totalDistanceTraveled<>0.0) then p := origin + direction * totalDistanceTraveled
      else p:=origin;

      // "distanceFromPointOnRayToClosestObjectInScene" is the
      // distance traveled from our current position along
      // our ray to the closest point on any object in our scene.
      // Remember that we use "totalDistanceTraveled" to calculate the new
      // point along our ray.
      // We could just increment the "totalDistanceTraveled" by some fixed amount.
      // However we can improve the performance of our shader by incrementing
      // the "totalDistanceTraveled" by the distance returned by our map function.
      // This works because our map function simply returns the distance from some
      // arbitrary point "p" to the closest point on any geometric object in our scene.
      // We know we are probably about to intersect with an object in the scene
      // if the resulting distance is very small.

      //distanceFromPointOnRayToClosestObjectInScene
      dist := RayMarch_Map(p);
      totalDistanceTraveled := totalDistanceTraveled + dist.x;//distanceFromPointOnRayToClosestObjectInScene;

      // If on the other hand our totalDistanceTraveled is a really huge distance,
      // we are probably marching along a ray pointing to empty space.  Again,
      // to improve performance,  we should just exit early.  We really only want
      // the trace function to tell us how far we have to march along our ray
      // to intersect with some geometry.  In this case we won't intersect with any
      // geometry so we will set our totalDistanceTraveled to 0.00.
      if( totalDistanceTraveled > 95.0 ) then //10000.0 --> for hardware
      begin
        //totalDistanceTraveled :=0;
        exit;
      end;

      // If our last step was very small, that means we are probably very close to
      // intersecting an object in our scene.  Therefore we can improve our performance
      // by just pretending that we hit the object and exiting early.
      //if( distanceFromPointOnRayToClosestObjectInScene < 0.001 ) then
      if( Dist.x < 0.05 ) then
      begin
        // We hit something!

        // y is use for generate material
        if (Dist.y=0) then
          c:=getfloor_color(p)
        else
          c.Create(1,0,0,1);

        n := RayMarch_ComputeNormal(p,Dist);//distanceFromPointOnRayToClosestObjectInScene);
        n := n.Normalize * 0.5 + 0.5;  //(*0.5+0.5 ===> Illumination global)

        r := ComputeBlinnPhongLighting(c, p, n, lightPosition, cameraPosition );

        r.Alpha:= 1.0;
       // Dist.x := totalDistanceTraveled;
        result := r;
        exit;
      end;

    end;

  end;

  {
    Coord is the coordinate of the current pixel being rendered.
    It is in screen space.  For example if your resolution is 800x600,
    Coord could be (300,400).  By dividing the Coord by the Resolution, we get normalized
    coordinates between 0.0 and 1.0.  I would like to work in a -1.0 to 1.0 space
    so I multiply the result by 2.0 and subtract 1.0 from it.
    if (Coord / Resolution) equals 0.0, then 0.0 * 2.0 - 1.0 = -1.0
    if (Coord / Resolution) equals 1.0, then 1.0 * 2.0 - 1.0 =  1.0
  }
  function ComputePixel(Coord:TBZVector2f; aTime:Single) : TBZColorVector;
  var
    {$CODEALIGN VARMIN=16}
    uv :TBZVector2f;
    cam, t : TBZVector4f;
    finalColor : TBZColorVector;
    {$CODEALIGN VARMIN=4}
    //distanceToClosestPointInScene : Single


  begin
     //ResSize.Create(0.003125, 0.0041666); //(Resolution.Width, Resolution.Height);
     uv :=(Coord * FInvResolution) * 2.0 - 1.0; // (Coord * InvResolution) * 2.0 - 1.0; /// resolution) * 2.0 - 1.0;
     // I am assuming you have more pixels horizontally than vertically so I am multiplying
     // the x coordinate by the aspect ratio.  This means that the magnitude of x coordinate
     //will probably be larger than 1.0.  This allows our image to not look squashed.
     uv.x := uv.x * 1.33333; // (resolution.x / resolution.y);

     cam.CreatePoint(0.0, 0.0, -5);
     // We will need to shoot a ray from our camera's position through each pixel.  To do this,
     // we will exploit the uv variable we calculated earlier, which describes the pixel we are
     // currently rendering, and make that our direction vector.
     t.CreatePoint(uv.x, uv.y, 1.0);
     FCameraDirection := t.normalize;

    // Now that we have our ray defined,  we need to trace it to see how far the closest point
    // in our world is to this ray.  We will simply shade our scene.
    //distanceToClosestPointInScene :=

    finalColor := RayMarch_Trace( FCameraPosition, FCameraDirection );

    // Set the Red, Green, and Blue channels of our final color to be the
    // the distance To the closest point in our scene to our ray.
    // Color channel values range between 0.0 and 1.0.  So even if our
    // distance is greater than 1.0,  the color value will essentially be treated
    // as 1.0.  Since we are setting each of the red, green, and blue channels to
    // the same distance value,  our sphere will be white.
    //finalColor.CreateAffine(distanceToClosestPointInScene);

    Result := finalColor;
  end;

begin
  //ttime := max(0.0,iTime-2.0);
  Result := ComputePixel(FragCoords, iTime);
end;

end.

