{%region%----[ Pure Pascal will never Convert ]---------------------------------}
// Temp comment this region is where we keep pascal routines for which it does
// not make sense to use SSE/AVX

function TBZVectorHelper.Rotate(constref axis : TBZVector; angle : Single):TBZVector;
var
   rotMatrix : TBZMatrix;
begin
   rotMatrix.CreateRotationMatrix(axis, Angle);
   Result := rotMatrix * Self;
end;

function TBZVectorHelper.RotateWithMatrixAroundX(alpha : Single) : TBZVector;
var
   rotMatrix : TBZMatrix;
begin
   rotMatrix.CreateRotationMatrixX(Alpha);
   Result:= rotMatrix * Self;
end;

function TBZVectorHelper.RotateWithMatrixAroundY(alpha : Single) : TBZVector;
var
   rotMatrix : TBZMatrix;
begin
   rotMatrix.CreateRotationMatrixY(Alpha);
   Result:= rotMatrix * Self;
end;

function TBZVectorHelper.RotateWithMatrixAroundZ(alpha : Single) : TBZVector;
var
   rotMatrix : TBZMatrix;
begin
   rotMatrix.CreateRotationMatrixZ(Alpha);
   Result:= rotMatrix * Self;
end;

function TBZVectorHelper.RotateAroundX( alpha : Single) : TBZVector;
var
   c, s : Single;
begin
   //SinCos(alpha, s, c);
   s := System.Sin(Alpha);
   c := System.Cos(Alpha);
   Result.X := Self.X;
   Result.Y := c * Self.Y - s * Self.Z;
   Result.Z := c * Self.Z + s * Self.Y;
   Result.W := Self.W
end;

function TBZVectorHelper.RotateAroundY(alpha : Single) : TBZVector;
var
   c, s : Single;
begin
   //SinCos(alpha, s, c);
  s := System.Sin(Alpha);
  c := System.Cos(Alpha);
   Result.X := c * Self.X + s * Self.Z;
   Result.Y := Self.Y;
   Result.Z := c * Self.Z - s * Self.X;
   Result.W := Self.W
end;

function TBZVectorHelper.RotateAroundZ(alpha : Single) : TBZVector;
var
   c, s : Single;
begin
   //SinCos(alpha, s, c);
  s := System.Sin(Alpha);
  c := System.Cos(Alpha);
   Result.X := c * Self.Y - s * Self.X;
   Result.Y := c * Self.X + s * Self.Y;
   Result.Z := Self.Z;
   Result.W := Self.W
end;

function TBZVectorHelper.MoveAround(constref AMovingObjectUp, ATargetPosition: TBZVector; pitchDelta, turnDelta: Single): TBZVector;
var
  originalT2C, normalT2C, normalCameraRight: TBZVector;
  pitchNow, dist: Single;
begin
    // normalT2C points away from the direction the camera is looking
    originalT2C := Self - ATargetPosition;
    normalT2C := originalT2C;
    dist := normalT2C.Length;
    normalT2C := normalT2C.Normalize;
    // normalRight points to the camera's right
    // the camera is pitching around this axis.
    normalCameraRight := AMovingObjectUp.CrossProduct(normalT2C);
    if normalCameraRight.Length < 0.001 then
      normalCameraRight:= XHmgVector // arbitrary vector
    else
      normalCameraRight := normalCameraRight.Normalize;
    // calculate the current pitch.
    // 0 is looking down and PI is looking up
    pitchNow := ArcCos(AMovingObjectUp.DotProduct(normalT2C));
    pitchNow := BZMath.Clamp(pitchNow - DegToRadian(pitchDelta), 0 + 0.025, cPI - 0.025);
    // create a new vector pointing up and then rotate it down
    // into the new position
    normalT2C := AMovingObjectUp;
    normalT2C := normalT2C.Rotate(normalCameraRight, pitchNow);
    normalT2C := normalT2C.Rotate(AMovingObjectUp, DegToRadian(turnDelta));
    normalT2C := normalT2C * dist;
    Result := Self + (normalT2C - originalT2C);
end;


function TBZVectorHelper.ShiftObjectFromCenter(Constref ACenter : TBZVector;
  const ADistance : Single; const AFromCenterSpot : Boolean) : TBZVector;
var
  lDirection: TBZVector;
begin
  lDirection := Self - ACenter;
  lDirection := lDirection.Normalize;
  if AFromCenterSpot then Result := ACenter + lDirection * ADistance
  else Result := Self + lDirection * ADistance;
end;

(* function TBZVectorHelper.ExtendClipRect(vX, vY: Single) : TBZClipRect;
begin
  Result := Self;
  with Result do
  begin
    if vX < Left then Left := vX;
    if vY < Top then Top := vY;

    if vX > Right then Right := vX;
    if vY > Bottom then Bottom := vY;
  end;
end; *)

function TBZVectorHelper.GetXY : TBZVector2f;
begin
  result.Create(Self.X,Self.Y);
end;

procedure TBZVectorHelper.SetXY(Const Value: TBZVector2f);
begin
  Self.X := Value.X;
  Self.Y := Value.Y
End;

function TBZVectorHelper.GetYX : TBZVector2f;
begin
  result.Create(Self.Y,Self.X);
end;

function TBZVectorHelper.GetXZ : TBZVector2f;
begin
  result.Create(Self.X,Self.Z);
end;

function TBZVectorHelper.GetZX : TBZVector2f;
begin
  result.Create(Self.Z,Self.X);
end;

function TBZVectorHelper.GetYZ : TBZVector2f;
begin
  result.Create(Self.Y,Self.Z);
end;

function TBZVectorHelper.GetZY : TBZVector2f;
begin
  result.Create(Self.Z,Self.Y);
end;

function TBZVectorHelper.GetXX : TBZVector2f;
begin
  result.Create(Self.X,Self.X);
end;

function TBZVectorHelper.GetYY : TBZVector2f;
begin
  result.Create(Self.Y,Self.Y);
end;

function TBZVectorHelper.GetZZ : TBZVector2f;
begin
  result.Create(Self.Z,Self.Z);
end;

function TBZVectorHelper.GetXYZ : TBZVector4f;
begin
  result.CreatePoint(Self.X,Self.Y,Self.Z);
end;

function TBZVectorHelper.GetXZY : TBZVector4f;
begin
  result.CreatePoint(Self.X,Self.Z,Self.Y);
end;

function TBZVectorHelper.GetYXZ : TBZVector4f;
begin
  result.CreatePoint(Self.Y,Self.X,Self.Z);
end;

function TBZVectorHelper.GetYZX : TBZVector4f;
begin
  result.CreatePoint(Self.Y,Self.Z,Self.X);
end;

function TBZVectorHelper.GetZXY : TBZVector4f;
begin
  result.CreatePoint(Self.Z,Self.X,Self.Y);
end;

function TBZVectorHelper.GetZYX : TBZVector4f;
begin
  result.CreatePoint(Self.Z,Self.Y,Self.X);
end;

function TBZVectorHelper.GetXXX : TBZVector4f;
begin
  result.CreatePoint(Self.X,Self.X,Self.X);
end;

function TBZVectorHelper.GetYYY : TBZVector4f;
begin
  result.CreatePoint(Self.Y,Self.Y,Self.Y);
end;

function TBZVectorHelper.GetZZZ : TBZVector4f;
begin
  result.CreatePoint(Self.Z,Self.Z,Self.Z);
end;

function TBZVectorHelper.GetYYX : TBZVector4f;
begin
  result.CreatePoint(Self.Y,Self.Y,Self.X);
end;

function TBZVectorHelper.GetXYY : TBZVector4f;
begin
  result.CreatePoint(Self.X,Self.Y,Self.Y);
end;

function TBZVectorHelper.GetYXY : TBZVector4f;
begin
  result.CreatePoint(Self.Y,Self.X,Self.Y);
end;


{%endregion}

{%region%----[ Pure Pascal may Convert at some point ]--------------------------}

{%endregion}

{$IFNDEF USE_ASM}

{%region----[ PASCAL VERSION OF ASSEMBLER ROUTINES BELOW ]----------------------}

{%region%----[ Operators ]------------------------------------------------------}

{%endregion%}

{%region%----[ Functions ]------------------------------------------------------}

function TBZVectorHelper.PointProject(constref origin, direction : TBZVector) : Single;
begin
   Result:= direction.X*(Self.X-origin.X)
           +direction.Y*(Self.Y-origin.Y)
           +direction.Z*(Self.Z-origin.Z);
end;

{gets the normal at cen based on the connected quad mesh vectors}
function TBZVectorHelper.AverageNormal4(constref up, left, down,right: TBZVector): TBZVector;
var
  s,t,u,r: TBZVector4f;
begin
  s := up - self;
  t := left - self;
  u := down - self;
  r := right - self;
  Result.X := s.Y*t.Z - s.Z*t.Y + t.Y*u.Z - t.Z*u.Y + u.Y*r.Z - u.Z*r.Y + r.Y*s.Z - r.Z*s.Y;
  Result.Y := s.Z*t.X - s.X*t.Z + t.Z*u.X - t.x*u.Z + u.Z*r.X - u.X*r.Z + r.Z*s.X - r.X*s.Z;
  Result.Z := s.X*t.Y - s.Y*t.X + t.X*u.Y - t.Y*u.X + u.X*r.Y - u.Y*r.X + r.X*s.Y - r.Y*s.X;
  Result.W := 0;
  Result := Result.Normalize;
end;

function TBZVectorHelper.Step(ConstRef B : TBZVector4f):TBZVector4f;
begin
  result := Self;
  if self.X <= B.X then result.X := 0;
  if self.Y <= B.Y then result.Y := 0;
  if self.Z <= B.Z then result.Z := 0;
  if self.W <= B.W then result.W := 0;
end;

function TBZVectorHelper.FaceForward(constref A, B: TBZVector4f): TBZVector4f;
begin
  if (B.DotProduct(A) < 0) then
     Result := -Self
   else
     Result := Self;
end;

function TBZVectorHelper.Saturate : TBZVector4f;
begin
  Result := Self.Min(XYZWHmgVector);
  Result := Result.Max(NullHmgVector);
end;

function TBZVectorHelper.SmoothStep(ConstRef A,B : TBZVector4f): TBZVector4f;
var
  t: TBZVector;
begin
  t := (Self-a)/(b-a);
  t := t.Saturate;
  // t * t * (3.0 - (t * 2.0)); <--- no overload for single - vector
  result := t * t * -((t * 2.0) + -3);
end;

function TBZVectorHelper.Reflect(ConstRef N: TBZVector4f): TBZVector4f;
var
  d : Single;
begin
  //Self - (N*(N.DotProduct(Self)*2));
  d:= N.DotProduct(Self);
  d:=d+d;
  Result := Self - (N*d);
end;

{%endregion%}

{%endregion%}

{$ENDIF}
