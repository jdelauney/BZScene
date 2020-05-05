{%region%----[ Pure Pascal will never Convert ]---------------------------------}
// Temp comment this region is where we keep pascal routines for which it does
// not make sense to use SSE/AVX

{%endregion}

{%region%----[ Pure Pascal may Convert at some point ]--------------------------}

procedure TBZFrustum.CreateFromModelViewProjectionMatrix(constref modelViewProj : TBZMatrix);
begin
  with Self do
  begin
      // extract left plane
      pLeft.V[0]:=modelViewProj.V[0].V[3]+modelViewProj.V[0].V[0];
      pLeft.V[1]:=modelViewProj.V[1].V[3]+modelViewProj.V[1].V[0];
      pLeft.V[2]:=modelViewProj.V[2].V[3]+modelViewProj.V[2].V[0];
      pLeft.V[3]:=modelViewProj.V[3].V[3]+modelViewProj.V[3].V[0];


      // extract top plane
      pTop.V[0]:=modelViewProj.V[0].V[3]-modelViewProj.V[0].V[1];
      pTop.V[1]:=modelViewProj.V[1].V[3]-modelViewProj.V[1].V[1];
      pTop.V[2]:=modelViewProj.V[2].V[3]-modelViewProj.V[2].V[1];
      pTop.V[3]:=modelViewProj.V[3].V[3]-modelViewProj.V[3].V[1];
      pTop.Normalize;

      // extract right plane
      pRight.V[0]:=modelViewProj.V[0].V[3]-modelViewProj.V[0].V[0];
      pRight.V[1]:=modelViewProj.V[1].V[3]-modelViewProj.V[1].V[0];
      pRight.V[2]:=modelViewProj.V[2].V[3]-modelViewProj.V[2].V[0];
      pRight.V[3]:=modelViewProj.V[3].V[3]-modelViewProj.V[3].V[0];
      pRight.Normalize;

      // extract bottom plane
      pBottom.V[0]:=modelViewProj.V[0].V[3]+modelViewProj.V[0].V[1];
      pBottom.V[1]:=modelViewProj.V[1].V[3]+modelViewProj.V[1].V[1];
      pBottom.V[2]:=modelViewProj.V[2].V[3]+modelViewProj.V[2].V[1];
      pBottom.V[3]:=modelViewProj.V[3].V[3]+modelViewProj.V[3].V[1];
      pBottom.Normalize;;

      // extract far plane
      pFar.V[0]:=modelViewProj.V[0].V[3]-modelViewProj.V[0].V[2];
      pFar.V[1]:=modelViewProj.V[1].V[3]-modelViewProj.V[1].V[2];
      pFar.V[2]:=modelViewProj.V[2].V[3]-modelViewProj.V[2].V[2];
      pFar.V[3]:=modelViewProj.V[3].V[3]-modelViewProj.V[3].V[2];
      pFar.Normalize;

      // extract near plane
      pNear.V[0]:=modelViewProj.V[0].V[3]+modelViewProj.V[0].V[2];
      pNear.V[1]:=modelViewProj.V[1].V[3]+modelViewProj.V[1].V[2];
      pNear.V[2]:=modelViewProj.V[2].V[3]+modelViewProj.V[2].V[2];
      pNear.V[3]:=modelViewProj.V[3].V[3]+modelViewProj.V[3].V[2];
      pNear.Normalize;
   end;
end;

function TBZFrustum.IsVolumeClipped(constref objPos : TBZVector; const objRadius : Single) : Boolean;
var
   negRadius : Single;
begin
   negRadius:=-objRadius;
   Result:=   (Self.pLeft.EvaluatePoint(objPos)<negRadius)
           or (Self.pTop.EvaluatePoint(objPos)<negRadius)
           or (Self.pRight.EvaluatePoint(objPos)<negRadius)
           or (Self.pBottom.EvaluatePoint(objPos)<negRadius)
           or (Self.pNear.EvaluatePoint(objPos)<negRadius)
           or (Self.pFar.EvaluatePoint(objPos)<negRadius);
end;

function TBZFrustum.Contains(constref TestBSphere: TBZBoundingSphere): TBZSpaceContains;
var
  NegRadius: Single;
  HitCount: Integer;
  Distance: Single;
  I: Integer;

begin
  NegRadius := -TestBSphere.Radius;

  HitCount := 0;
  result := ScNoOverlap;
  // Totally unroll Loop (for p:=0 to 5 do..)
   Distance :=  Self.pLeft.EvaluatePoint(TestBSphere.Center);
   if Distance > NegRadius then
   begin
      if Distance >= TestBSphere.Radius then Inc(HitCount);
   end else exit;

   Distance :=  Self.pTop.EvaluatePoint(TestBSphere.Center);
   if Distance > NegRadius then
   begin
      if Distance >= TestBSphere.Radius then Inc(HitCount);
   end else exit;

   Distance :=  Self.pRight.EvaluatePoint(TestBSphere.Center);
   if Distance > NegRadius then
   begin
      if Distance >= TestBSphere.Radius then Inc(HitCount);
   end else exit;

   Distance :=  Self.pBottom.EvaluatePoint(TestBSphere.Center);
   if Distance > NegRadius then
   begin
      if Distance >= TestBSphere.Radius then Inc(HitCount);
   end else exit;

   Distance :=  Self.pNear.EvaluatePoint(TestBSphere.Center);
   if Distance > NegRadius then
   begin
      if Distance >= TestBSphere.Radius then Inc(HitCount);
   end else exit;

   Distance :=  Self.pFar.EvaluatePoint(TestBSphere.Center);
   if Distance > NegRadius then
   begin
      if Distance >= TestBSphere.Radius then Inc(HitCount);
   end else exit;

  if HitCount = 6 then
    Result := ScContainsFully
  else
    Result := ScContainsPartially;
end;

function TBZFrustum.Contains(constref TestAABB: TBZAxisAlignedBoundingBox) : TBZSpaceContains;
var
  IPlane, ICorner: Integer;
  PointIn: Boolean;
  AABBCorners: TBZAABBCorners;
  InCount: Integer;
  TotalIn: Integer;
begin

  AABBCorners := TestAABB.ExtractCorners;
  TotalIn := 0;
  // test all 8 corners against the 6 sides
  // if all points are behind 1 specific plane, we are out
  // if we are in with all points, then we are fully in

  // For each plane
  for IPlane := 0 to 5 do
  begin
    // We're about to test 8 corners
    InCount := 8;
    PointIn := True;

    // For each corner
    for ICorner := Low(AABBCorners) to High(AABBCorners) do
    begin
      if Self.Planes[IPlane].EvaluatePoint(AABBCorners[ICorner]) < 0 then
      begin
        PointIn := False;
        Dec(InCount);
      end;
    end;

    if InCount = 0 then
    begin
      Result := ScNoOverlap;
      Exit;
    end
    else if PointIn then
      Inc(TotalIn);
  end;

  if TotalIn = 6 then
    Result := ScContainsFully
  else
    Result := ScContainsPartially;
end;

//Function TBZFrustum.ContainBoxInPlane(Constref Box:TBZBoundingBox; PlaneIndex:Integer):Boolean;
//Begin
//  With Box Do
//  Result:=(PlaneGetDistance(Planes[PlaneIndex], StartVertex.X, StartVertex.Y, StartVertex.Z) >= 0)
//    Or (PlaneGetDistance(PlanePlanes[index], EndVertex.X, StartVertex.Y, StartVertex.Z) >= 0)
//    Or (PlaneGetDistance(PlanePlanes[index], StartVertex.X, EndVertex.Y, StartVertex.Z) >= 0)
//    Or (PlaneGetDistance(PlanePlanes[index], EndVertex.X, EndVertex.Y, StartVertex.Z) >= 0)
//    Or (PlaneGetDistance(PlanePlanes[index], StartVertex.X, StartVertex.Y, EndVertex.Z) >= 0)
//    Or (PlaneGetDistance(PlanePlanes[index], EndVertex.X, StartVertex.Y, EndVertex.Z) >= 0)
//    Or (PlaneGetDistance(PlanePlanes[index], StartVertex.X, EndVertex.Y, EndVertex.Z) >= 0)
//    Or (PlaneGetDistance(PlanePlanes[index], EndVertex.X, EndVertex.Y, EndVertex.Z) >= 0);
//End;

// check if a point is inside the cameras field of vision
// inside or outside frustum
//function TBZFrustrum.Contains(APoint : TPoint3D) : boolean;
//Var
//  LPlane : byte;
//begin
//  for LPlane := 0 to 5 do begin
//    if not PointPlaneSide(LPlane, APoint) then begin
//      Result := False;
//      Exit; // outside frustum - return false
//    end;
//  end;
//
//  Result := True; // inside frustum
//end;

// determine if a point is on the postive or negative side of a frustum plane
//function TFrustrum.IsPointContainedInPlaneSide(AFrustumPlane : byte ; APoint : TPoint3D) : boolean;
//begin
// Result := aPlane.Contains(APoint);
//
//  if DotProduct(Frustum[AFrustumPlane].Normal, APoint) >
//     Frustum[AFrustumPlane].Distance then Result := False
//                                     else Result := True;
//end;


{%endregion}

{$IFNDEF USE_ASM}

{%region----[ PASCAL VERSION OF ASSEMBLER ROUTINES BELOW ]----------------------}

{%region%----[ Operators ]------------------------------------------------------}

{%endregion%}

{%region%----[ Functions ]------------------------------------------------------}

{%endregion%}

{%endregion%}

{$ENDIF}
