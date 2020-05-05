Unit BZVectorObjects

uses
  BZBitmapSceneClasses;
  
interface

Type
  TBZ2DDummyObject = Class(TBZ2DInvarianceObject)
  private
  protected
  public
  published
  end;
  
  
  TBZ2DPointObject = class(TBZ2DBaseSceneObject)
  TBZ2DLineObject = class(TBZ2DBaseSceneObject)
  TBZ2DPolyLineObject = class(TBZ2DBaseSceneObject)
  TBZ2DPolygoneObject = class(TBZ2DBaseSceneObject)    
  
  TBZ2DCurveObject = class(TBZ2DBaseSceneObject)  
  TBZ2DRectangleObject = class(TBZ2DBaseSceneObject)
  TBZ2DCircleObject = class(TBZ2DBaseSceneObject)
  TBZ2DEllipseObject = class(TBZ2DBaseSceneObject)
  TBZ2DTriangleObject = class(TBZ2DBaseSceneObject)
  
  TBZ2DStarObject = class(TBZ2DBaseSceneObject)
  
  TBZ2DTextObject = class(TBZ2DBaseSceneObject)
  TBZ2DSpriteObject = class(TBZ2DBaseSceneObject)
  
  TBZ3DDummyObject = Class(TBZ3DInvarianceObject)
  TBZCustomMesh = class(TBZ3DBaseSceneObject)
  TBZPlane  = class(TBZCustomMesh)
  TBZCube  = class(TBZCustomMesh)
  TBZSphere = class(TBZCustomMesh)
  TBZCylinder = class(TBZCustomMesh)
  TBZTorus = class(TBZCustomMesh)
  TBZMesh = class(TBZCustomMesh)
  TBZActorMesh = class(TBZCustomMesh)
  TBZTerrain = class(TBZCustomMesh)
  
  
  
implementation

end.