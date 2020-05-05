unit BZRenderContextInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  BZClasses, BZVectorMath;

Type
  TBZViewportSize = Record
    width  : Longint;
    Height : Longint;
  end;

  TBZRenderState = (dsRendering, dsPicking, dsPrinting);

  { TBZCameraType : Pour definir le type de camera 2D ou 3D }
  TBZCameraType = (ct2D, ct3D);

  { TBZSceneObjectsSorting :
    Détermine si les objets sont triés et comment.
    Le tri est effectué niveau par niveau (et non pour toutes les entités).
    Les valeurs sont les suivantes:
      - osInherited           : utilise le mode de tri hérité, par défaut, osRenderFarthestFirst
      - osNone                : ne pas trier les objets.
      - osRenderFarthestFirst : affiche en premier les objets dont la position est la plus éloignée de la caméra.
      - osRenderBlendedLast   : les objets opaques ne sont ni triés ni rendus en premier, ceux fusionnés sont ensuite rendus et leur profondeur est triée.
      - osRenderNearestFirst  : affiche en premier les objets dont la position est la plus proche de la caméra. }
  TBZSceneObjectsSorting = (osInherited, osNone, osRenderFarthestFirst, osRenderBlendedLast, osRenderNearestFirst);

  // TBZRender3DContextInfo = record
  // TBZClippingInfo = record

  TBZRenderContextInfo = record
    Scene         : TObject;
    RenderEngine  : TObject;
    ViewportSize  : TBZViewportSize;
    RenderState   : TBZRenderState;
    ObjectSorting : TBZSceneObjectsSorting;
    CameraType    : TBZCameraType;
    //ClippingInfo : TBZClippingInfo
    //PipelineTransformation: TBZMatrixTransformation;

    // Render3DContext : TBZRender3DContextInfo;

    AfterRenderEffects: TBZPersistentObjectList;
    orderCounter: Integer;
  end;
  PBZRenderContextInfo = ^TBZRenderContextInfo;

implementation

end.

