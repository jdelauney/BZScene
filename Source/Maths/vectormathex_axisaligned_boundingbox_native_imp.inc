{%region%----[ Pure Pascal will never Convert ]---------------------------------}
// Temp comment this region is where we keep pascal routines for which it does
// not make sense to use SSE/AVX

procedure TBZAxisAlignedBoundingBox.Create(const AValue: TBZVector);
begin
  Self.Max := AValue.Abs;
  Self.Min := - Self.Max;

(*  Bb.Max.V[0] := Abs(V.V[0]);
  Bb.Max.V[1] := Abs(V.V[1]);
  Bb.Max.V[2] := Abs(V.V[2]);
  Bb.Min.V[0] := -Bb.Max.V[0];
  Bb.Min.V[1] := -Bb.Max.V[1];
  Bb.Min.V[2] := -Bb.Max.V[2]; *)
end;

procedure TBZAxisAlignedBoundingBox.Create(const AMin, AMax: TBZVector);overload;
begin
  if AMin<AMax then
  begin
    Self.Min := AMin.Abs;
    Self.Max := AMax.abs;
  end
  else
  begin
    Self.Max := AMin.Abs;
    Self.Min := AMax.abs;
  end;
end;

{ : Extract the AABB information from a BB. }
procedure TBZAxisAlignedBoundingBox.Create(const ABB: TBZBoundingBox);
var
   I: Integer;
 begin
   //SetVector(Result.Min, ABB.BBox[0]);
   Self.Min := ABB.Points[0];
   //SetVector(Self.Max, ABB.Points[0]);
   Self.Max := ABB.Points[0];

   for I := 1 to 7 do
   begin
     if ABB.Points[I].X < Self.Min.X then Self.Min.X := ABB.Points[I].X;
     if ABB.Points[I].X > Self.Max.X then Self.Max.X := ABB.Points[I].X;

     if ABB.Points[I].Y < Self.Min.Y then Self.Min.Y := ABB.Points[I].Y;
     if ABB.Points[I].Y > Self.Max.Y then Self.Max.Y := ABB.Points[I].Y;

     if ABB.Points[I].Z < Self.Min.Z then Self.Min.Z := ABB.Points[I].Z;
     if ABB.Points[I].Z > Self.Max.Z then Self.Max.Z := ABB.Points[I].Z;
   end;
end;

{ : Make the AABB that is formed by sweeping a sphere (or AABB) from Start to Dest }
procedure TBZAxisAlignedBoundingBox.CreateFromSweep(const Start, Dest: TBZVector;const Radius: Single);
begin
  if Start.X < Dest.X then
  begin
    Self.Min.X := Start.X - Radius;
    Self.Max.X := Dest.X + Radius;
  end
  else
  begin
    Self.Min.X := Dest.X - Radius;
    Self.Max.X := Start.X + Radius;
  end;

  if Start.Y < Dest.Y then
  begin
    Self.Min.Y := Start.Y - Radius;
    Self.Max.Y := Dest.Y + Radius;
  end
  else
  begin
    Self.Min.Y := Dest.Y - Radius;
    Self.Max.Y := Start.Y + Radius;
  end;

  if Start.Z < Dest.Z then
  begin
    Self.Min.Z := Start.Z - Radius;
    Self.Max.Z := Dest.Z + Radius;
  end
  else
  begin
    Self.Min.Z := Dest.Z - Radius;
    Self.Max.Z := Start.Z + Radius;
  end;
end;

{ : Convert a BSphere to the AABB }
procedure TBZAxisAlignedBoundingBox.Create(const BSphere: TBZBoundingSphere); overload;
begin
 Self.Min := BSphere.Center - BSphere.Radius; // VectorSubtract(BSphere.Center, BSphere.Radius);
 Self.Max := BSphere.Center + BSphere.Radius; //VectorAdd(BSphere.Center, BSphere.Radius);
end;

procedure TBZAxisAlignedBoundingBox.Create(const Center: TBZVector; Radius: Single); overload;
begin
 Self.Min := Center - Radius; // VectorSubtract(Center, Radius);
 Self.Max := Center + Radius; //VectorAdd(Center, Radius);
end;

class operator TBZAxisAlignedBoundingBox.+(ConstRef A, B : TBZAxisAlignedBoundingBox):TBZAxisAlignedBoundingBox;overload;
begin
  Result := A;

  if B.Min.X < A.Min.X then Result.Min.X := B.Min.X;
  if B.Min.Y < A.Min.Y then Result.Min.Y := B.Min.Y;
  if B.Min.Z < A.Min.Z then Result.Min.Z := B.Min.Z;
  if B.Max.X > A.Max.X then Result.Max.X := B.Max.X;
  if B.Max.Y > A.Max.Y then Result.Max.Y := B.Max.Y;
  if B.Max.Z > A.Max.Z then Result.Max.Z := B.Max.Z;

  (*Result.Min := A.Min;
    Result.Max := A.Max;
    Result.Min := Result.Min.Min(B.Min);
    Result.Max := Result.Max.Max(B.Max);
  *)
end;

class operator TBZAxisAlignedBoundingBox.+(ConstRef A: TBZAxisAlignedBoundingBox; ConstRef B : TBZVector):TBZAxisAlignedBoundingBox;overload;
begin
  Result.Min := A.Min + B;
  Result.Max := A.Max + B;
end;

class operator TBZAxisAlignedBoundingBox.*(ConstRef A: TBZAxisAlignedBoundingBox; ConstRef B : TBZVector):TBZAxisAlignedBoundingBox;overload;
begin
  Result.Min := A.Min * B;
  Result.Max := A.Max * B;
end;

class operator TBZAxisAlignedBoundingBox.=(ConstRef A, B : TBZAxisAlignedBoundingBox):Boolean;overload;
begin
  result := (A.Min = B.Min) and (A.Max = B.Max);
end;

function TBZAxisAlignedBoundingBox.Transform(Constref M:TBZMatrix):TBZAxisAlignedBoundingBox;
var
  Temp, OldMin, OldMax: TBZVector;
begin
  OldMin := Self.Min;
  OldMax := Self.Max;
  Result.Min := M * OldMin; //VectorTransform(OldMin, M);
  Result.Max := Self.Min;
  //AABBInclude(Bb, VectorTransform(AffineVectorMake(OldMin.V[0], OldMin.V[1], OldMax.V[2]), M));
  Temp.Create(OldMin.X, OldMin.Y, OldMax.Z,1);
  Temp := M * Temp;
  Result := Result.Include(Temp);
  //AABBInclude(Bb, VectorTransform(AffineVectorMake(OldMin.V[0], OldMax.V[1], OldMin.V[2]), M));
  Temp.Create(OldMin.X, OldMax.Y, OldMin.Z,1);
  Temp := M * Temp;
  Result := Result.Include(Temp);
  //AABBInclude(Bb, VectorTransform(AffineVectorMake(OldMin.V[0], OldMax.V[1], OldMax.V[2]), M));
  Temp.Create(OldMin.X, OldMax.Y, OldMax.Z,1);
  Temp := M * Temp;
  Result := Result.Include(Temp);
  //AABBInclude(Bb, VectorTransform(AffineVectorMake(OldMax.V[0], OldMin.V[1], OldMin.V[2]), M));
  Temp.Create(OldMax.X, OldMin.Y, OldMin.Z,1);
  Temp := M * Temp;
  Result := Result.Include(Temp);
  //AABBInclude(Bb, VectorTransform(AffineVectorMake(OldMax.V[0], OldMin.V[1], OldMax.V[2]), M));
  Temp.Create(OldMax.X, OldMin.Y, OldMax.Z,1);
  Temp := M * Temp;
  Result := Result.Include(Temp);
  //AABBInclude(Bb, VectorTransform(AffineVectorMake(OldMax.V[0], OldMax.V[1], OldMin.V[2]), M));
  Temp.Create(OldMax.X, OldMax.Y, OldMin.Z ,1);
  Temp := M * Temp;
  Result := Result.Include(Temp);
  //AABBInclude(Bb, VectorTransform(OldMax, M));
  Temp := M * OldMax;
  Result := Result.Include(Temp);
end;

function TBZAxisAlignedBoundingBox.Include(Constref P:TBZVector):TBZAxisAlignedBoundingBox;
begin
  Result := Self;
  if P.X < Result.Min.X then Result.Min.X := P.X;
  if P.X > Result.Max.X then Result.Max.X := P.X;
  if P.Y < Result.Min.Y then Result.Min.Y := P.Y;
  if P.Y > Result.Max.Y then Result.Max.Y := P.Y;
  if P.Z < Result.Min.Z then Result.Min.Z := P.Z;
  if P.Z > Result.Max.Z then Result.Max.Z := P.Z;
end;

{ : Returns the intersection of the AABB with second AABBs.
  If the AABBs don't intersect, will return a degenerated AABB (plane, line or point). }
function TBZAxisAlignedBoundingBox.Intersection(const B: TBZAxisAlignedBoundingBox): TBZAxisAlignedBoundingBox;
var
  I: Integer;
begin
  for I := 0 to 2 do
  begin
    Result.Min.V[I] := Math.Max(Self.Min.V[I], B.Min.V[I]);
    Result.Max.V[I] := Math.Min(Self.Max.V[I], B.Max.V[I]);
  end;
end;

{ : Converts the AABB to its canonical BB. }
function TBZAxisAlignedBoundingBox.ToBoundingBox: TBZBoundingBox; overload;
begin
  result:=NullBoundingBox;
  with Self do
  begin
    SetPlanBB(Result, 0, Max.X);
    SetPlanBB(Result, 1, Min.X);
    SetPlanBB(Result, 2, Max.Y);
    SetPlanBB(Result, 3, Min.Y);
    SetPlanBB(Result, 4, Max.Z);
    SetPlanBB(Result, 5, Min.Z);
  end;
end;

{ : Transforms the AABB to a BB. }
function TBZAxisAlignedBoundingBox.ToBoundingBox(const M: TBZMatrix) : TBZBoundingBox; overload;
begin
  Result := Self.ToBoundingBox;
  Result := Result.Transform(M);
end;

{ : Convert the AABB to a BSphere }
function TBZAxisAlignedBoundingBox.ToBoundingSphere: TBZBoundingSphere;
begin
 Result.Center := (Self.Min + Self.Max) * 0.5;// VectorScale(VectorAdd(AABB.Min, AABB.Max), 0.5);
 Result.Radius := Self.Min.Distance(Self.Max) * 0.5; // VectorDistance(AABB.Min, AABB.Max) * 0.5;
end;

//function TBZAxisAlignedBoundingBox.ToClipRect(ModelViewProjection: TBZMatrix; ViewportSizeX, ViewportSizeY: Integer): TBZClipRect;
//var
//  I: Integer;
//  v, Vt: TBZVector;
//  Minmax: array [0 .. 1] of PBZAffineVector;
//begin
//
//  Minmax[0] := @Self.Min;
//  Minmax[1] := @Self.Max;
//  v.W := 1;
//  for I := 0 to 7 do
//  begin
//    v.X := Minmax[I and 1]^.V[0];
//    v.Y := Minmax[(I shr 1) and 1]^.V[1];
//    v.Z := Minmax[(I shr 2) and 1]^.V[2];
//
//    // Project
//    Vt := ModelViewProjection * v; //VectorTransform(V, ModelViewProjection);
//    vt := vt * (1/vt.W); // ScaleVector(Vt, 1 / Vt.V[3]);
//
//    // Convert to screen coordinates
//    if I > 0 then
//      //IncludeInClipRect(Result, ViewportSizeX * (Vt.V[0] + 1) * 0.5, ViewportSizeY * (Vt.V[1] + 1) * 0.5)
//      Result := Result.ExtendClipRect(ViewportSizeX * (Vt.V[0] + 1) * 0.5, ViewportSizeY * (Vt.V[1] + 1) * 0.5)
//    else
//    begin
//      Result.Left := ViewportSizeX * (Vt.X + 1) * 0.5;
//      Result.Top := ViewportSizeY * (Vt.Y + 1) * 0.5;
//      Result.Right := Result.Left;
//      Result.Bottom := Result.Top;
//    end;
//  end;
//end;

{ : Determines if two AxisAlignedBoundingBoxes intersect.
  The matrices are the ones that convert one point to the other's AABB system }
function TBZAxisAlignedBoundingBox.Intersect(const B: TBZAxisAlignedBoundingBox;const M1, M2: TBZMatrix):Boolean;
begin
  result := false;
 (* const
  CWires: array [0 .. 11, 0 .. 1] of Integer // Points of the wire
    = ((0, 1), (1, 2), (2, 3), (3, 0), (4, 5), (5, 6), (6, 7), (7, 4), (0, 4),
    (1, 5), (2, 6), (3, 7));
  CPlanes: array [0 .. 5, 0 .. 3] of Integer // points of the planes
    = ((1, 2, 6, 5), (2, 3, 7, 6), (0, 1, 2, 3), (0, 3, 7, 4), (0, 1, 5, 4),
    (5, 6, 7, 4));

  procedure MakeAABBPoints(const AABB: TBZAxisAlignedBoundingBox; var Pt: array of TBZVector);
  begin
    with AABB do
    begin
      Pt[0].Create(Min.X, Min.Y, Min.Z,1);
      Pt[1].Create(Max.X, Min.Y, Min.Z,1);
      Pt[2].Create(Max.X, Max.Y, Min.Z,1);
      Pt[3].Create(Min.X, Max.Y, Min.Z,1);
      Pt[4].Create(Min.X, Min.Y, Max.Z,1);
      Pt[5].Create(Max.X, Min.Y, Max.Z,1);
      Pt[6].Create(Max.X, Max.Y, Max.Z,1);
      Pt[7].Create(Min.X, Max.Y, Max.Z,1);
    end;
  end;

  procedure MakePlanes(const Pt: array of TBZVector; var Planes: array of TBZHmgPlane);
  var
    I: Integer;
  begin
    for I := 0 to 5 do
      Planes[I] := PlaneMake(Pt[CPlanes[I, 0]], Pt[CPlanes[I, 1]],Pt[CPlanes[I, 2]]);
  end;

var
  Pt1, Pt2: array [0 .. 7] of TBZVector;
  Pt: TBZVector;
  Planes2: array [0 .. 5] of TBZHmgPlane;
  I, T: Integer;
  V: TBZVector;
  P: TBZVector;
begin
  Result := False;

  // Build Points
  MakeAABBPoints(AABB1, Pt1);
  MakeAABBPoints(AABB2, Pt2);
  for I := 0 to 7 do
  begin
    Pt := M2 * Pt2[I]; //VectorTransform(Pt2[I], M2To1);
    // check for inclusion (points of Obj2 in Obj1)
    if IsInRange(Pt.V[0], AABB1.Min.V[0], AABB1.Max.V[0]) and
      IsInRange(Pt.V[1], AABB1.Min.V[1], AABB1.Max.V[1]) and
      IsInRange(Pt.V[2], AABB1.Min.V[2], AABB1.Max.V[2]) then
    begin
      Result := True;
      Exit;
    end;
  end;

  for I := 0 to 7 do
  begin
    Pt1[I] := M2 * Pt1[I]; // VectorTransform(Pt1[I], M1To2);
    // check for inclusion (points of Obj1 in Obj2)
    if IsInRange(Pt1[I].V[0], AABB2.Min.V[0], AABB2.Max.V[0]) and
      IsInRange(Pt1[I].V[1], AABB2.Min.V[1], AABB2.Max.V[1]) and
      IsInRange(Pt1[I].V[2], AABB2.Min.V[2], AABB2.Max.V[2]) then
    begin
      Result := True;
      Exit;
    end;
  end;

  // Build Planes

  MakePlanes(Pt2, Planes2);

  // Wire test
  for I := 0 to 11 do
  begin
    for T := 0 to 5 do
    begin
      // Build Vector of Ray
      V := Pt1[CWires[I, 0]] - Pt1[CWires[I, 1]]; // VectorSubtract(Pt1[CWires[I, 0]], Pt1[CWires[I, 1]]);
      if IntersectLinePlane(VectorMake(Pt1[CWires[I, 0]]), VectorMake(V),
        Planes2[T], @P) = 1 then
      begin
        // check point in Wire
        if IsInRange(P.V[0], Pt1[CWires[I, 0]].V[0],
          Pt1[CWires[I, 1]].V[0]) and
          IsInRange(P.V[1], Pt1[CWires[I, 0]].V[1],
          Pt1[CWires[I, 1]].V[1]) and
          IsInRange(P.V[2], Pt1[CWires[I, 0]].V[2],
          Pt1[CWires[I, 1]].V[2]) then
        begin
          // check point in Plane
          if IsInRange(P.V[0], Pt2[CPlanes[T, 0]].V[0],
            Pt2[CPlanes[T, 2]].V[0]) and
            IsInRange(P.V[1], Pt2[CPlanes[T, 0]].V[1],
            Pt2[CPlanes[T, 2]].V[1]) and
            IsInRange(P.V[2], Pt2[CPlanes[T, 0]].V[2],
            Pt2[CPlanes[T, 2]].V[2]) then
          begin
            Result := True;
            Exit;
          end;
        end;
      end;
    end;
  end;   *)
end;

{ : Checks whether two Bounding boxes aligned with the world axes collide in the XY plane. }
function TBZAxisAlignedBoundingBox.IntersectAbsoluteXY(const B: TBZAxisAlignedBoundingBox): Boolean;
begin
 Result := False;

 if (B.Min.X > Self.Max.X) or (B.Min.Y > Self.Max.Y) then Exit
 else if (B.Max.X < Self.Min.X) or (B.Max.Y < Self.Min.Y) then Exit
 else
   Result := True;

end;

{ : Checks whether two Bounding boxes aligned with the world axes collide in the XZ plane. }
function TBZAxisAlignedBoundingBox.IntersectAbsoluteXZ(const B: TBZAxisAlignedBoundingBox): Boolean;
begin
 Result := ((Self.Min.X < B.Max.X) and (Self.Min.Z < B.Max.Z) and
            (B.Min.X < Self.Max.X) and (B.Min.Z < Self.Max.Z));
end;

{ : Checks whether two Bounding boxes aligned with the world axes collide. }
function TBZAxisAlignedBoundingBox.IntersectAbsolute(const B: TBZAxisAlignedBoundingBox): Boolean;
begin
 Result := not((Self.Min.X > B.Max.X) or (Self.Min.Y > B.Max.Y) or
               (Self.Min.Z > B.Max.Z) or (B.Min.X > Self.Max.X) or
               (B.Min.Y > Self.Max.Y) or (B.Min.Z > Self.Max.Z));
end;

{ : Checks whether one Bounding box aligned with the world axes fits within
  another Bounding box. }
function TBZAxisAlignedBoundingBox.FitsInAbsolute(const B: TBZAxisAlignedBoundingBox): Boolean;
begin
 // AABB1 fits completely inside AABB2?
 // AABB1 min must be >= to AABB2 min
 // AABB1 max must be <= to AABB2 max

 Result := (Self.Min.X >= B.Min.X) and (Self.Min.Y >= B.Min.Y) and
           (Self.Min.Z >= B.Min.Z) and (Self.Max.X <= B.Max.X) and
           (Self.Max.Y <= B.Max.Y) and (Self.Max.Z <= B.Max.Z);
end;

{ : Checks if a point "p" is inside the AABB }
function TBZAxisAlignedBoundingBox.PointIn(const P: TBZVector): Boolean;
begin
   Result := (P.X <= Self.Max.X) and (P.X >= Self.Min.X) and
             (P.Y <= Self.Max.Y) and (P.Y >= Self.Min.Y) and
             (P.Z <= Self.Max.Z) and (P.Z >= Self.Min.Z);
end;

{ : Extract the corners from the AABB }
function TBZAxisAlignedBoundingBox.ExtractCorners: TBZAABBCorners;
begin
  Result{%H-}[0].Create(Self.Min.X, Self.Min.Y, Self.Min.Z,1);
  Result[1].Create(Self.Min.X, Self.Min.Y, Self.Max.Z,1);
  Result[2].Create(Self.Min.X, Self.Max.Y, Self.Min.Z,1);
  Result[3].Create(Self.Min.X, Self.Max.Y, Self.Max.Z,1);
  Result[4].Create(Self.Max.X, Self.Min.Y, Self.Min.Z,1);
  Result[5].Create(Self.Max.X, Self.Min.Y, Self.Max.Z,1);
  Result[6].Create(Self.Max.X, Self.Max.Y, Self.Min.Z,1);
  Result[7].Create(Self.Max.X, Self.Max.Y, Self.Max.Z,1);
end;

{ : Determines to which extent the AABB contains another AABB }
function TBZAxisAlignedBoundingBox.Contains(const TestAABB: TBZAxisAlignedBoundingBox): TBZSpaceContains; overload;
begin
 // AABB1 fits completely inside AABB2?
  // AABB1 min must be >= to AABB2 min
  // AABB1 max must be <= to AABB2 max

  if ((Self.Min.X < TestAABB.Max.X) and (Self.Min.Y < TestAABB.Max.Y) and
      (Self.Min.Z < TestAABB.Max.Z) and (TestAABB.Min.X < Self.Max.X) and
      (TestAABB.Min.Y < Self.Max.Y) and (TestAABB.Min.Z < Self.Max.Z)) then
  begin
    if (TestAABB.Min.X >= Self.Min.X) and (TestAABB.Min.Y >= Self.Min.Y) and
       (TestAABB.Min.Z >= Self.Min.Z) and (TestAABB.Max.X <= Self.Max.X) and
       (TestAABB.Max.Y <= Self.Max.Y) and (TestAABB.Max.Z <= Self.Max.Z) then
      Result := ScContainsFully
    else
      Result := ScContainsPartially;
  end
  else
    Result := ScNoOverlap;
end;

{ : Determines to which extent the AABB contains a BSphere }
function TBZAxisAlignedBoundingBox.Contains(const TestBSphere: TBZBoundingSphere): TBZSpaceContains; overload;
var
  TestAABB: TBZAxisAlignedBoundingBox;
begin
  TestAABB.Create(TestBSphere);
  Result := Self.Contains(TestAABB);
end;

{ : Clips a position to the AABB }
//function TBZAxisAlignedBoundingBox.Clip(const V: TBZAffineVector): TBZAffineVector;
//begin
// Result := V;
//
// if Result.X < Self.Min.X then Result.X := Self.Min.X;
// if Result.Y < Self.Min.Y then Result.Y := Self.Min.Y;
// if Result.Z < Self.Min.Z then Result.Z := Self.Min.Z;
//
// if Result.X > Self.Max.X then Result.X := Self.Max.X;
// if Result.Y > Self.Max.Y then Result.Y := Self.Max.Y;
// if Result.Z > Self.Max.Z then Result.Z := Self.Max.Z;
//end;

{ : Finds the intersection between a ray and an axis aligned bounding box. }
{ TODO 1 -oASM -cBZAxisAlignedBoundingBox : RayCastIntersect Add ASM version }
//function TBZAxisAlignedBoundingBox.RayCastIntersect(const RayOrigin, RayDirection: TBZVector; out TNear, TFar: Single): Boolean; overload;
//const
//  Infinity = 1.0 / 0.0;
//var
//  P: Integer;
//  InvDir: Double;
//  T0, T1, Tmp: Single;
//begin
//  Result := False;
//
//  TNear := -Infinity;
//  TFar := Infinity;
//
//  for P := 0 to 2 do
//  begin
//    if (RayDirection.V[P] = 0) then
//    begin
//      if ((RayOrigin.V[P] < Self.Min.V[P]) or
//          (RayOrigin.V[P] > Self.Max.V[P])) then Exit;
//    end
//    else
//    begin
//      // T0 := Self.Min.SubAndMul(
//      InvDir := 1 / RayDirection.V[P];
//      T0 := (Self.Min.V[P] - RayOrigin.V[P]) * InvDir;
//      T1 := (Self.Max.V[P] - RayOrigin.V[P]) * InvDir;
//
//      if (T0 > T1) then
//      begin
//        Tmp := T0;
//        T0 := T1;
//        T1 := Tmp;
//      end;
//
//      if (T0 > TNear) then TNear := T0;
//      if (T1 < TFar) then TFar := T1;
//
//      if ((TNear > TFar) or (TFar < 0)) then Exit;
//    end;
//  end;
//
//  Result := True;
//end;
//
//function TBZAxisAlignedBoundingBox.RayCastIntersect(const RayOrigin, RayDirection: TBZVector; IntersectPoint: PBZVector = nil): Boolean; overload;
//var
//  TNear, TFar: Single;
//begin
//  Result := RayCastIntersect(RayOrigin, RayDirection, TNear, TFar);
//
//  if Result and Assigned(IntersectPoint) then
//  begin
//    if TNear >= 0 then
//        // origin outside the box
//        IntersectPoint^ :=RayOrigin.Combine2(RayDirection, 1, TNear) // VectorCombine(RayOrigin, RayDirection, 1, TNear)
//    else
//       // origin inside the box, near is "behind", use far
//       IntersectPoint^ := RayOrigin.Combine2(RayDirection, 1, TFar); // VectorCombine(RayOrigin, RayDirection, 1, TFar);
//  end;
//end;

{%endregion}

{%region%----[ Pure Pascal may Convert at some point ]--------------------------}

{%endregion}

{$IFNDEF USE_ASM}

{%region----[ PASCAL VERSION OF ASSEMBLER ROUTINES BELOW ]----------------------}

{%region%----[ Operators ]------------------------------------------------------}

{%endregion%}

{%region%----[ Functions ]------------------------------------------------------}

{%endregion%}

{%endregion%}

{$ENDIF}
