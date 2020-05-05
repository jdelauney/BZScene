{%region%----[ Pure Pascal will never Convert ]---------------------------------}
// Temp comment this region is where we keep pascal routines for which it does
// not make sense to use SSE/AVX

// Internal for pascal only
procedure TBZHmgPlane.CalcNormal(constref p1, p2, p3 : TBZVector);
var
   v1, v2 : TBZVector;
begin
   v1:=p2-p1;
   v2:=p3-p1;
   v1 := v1.CrossProduct(v2);
   Self.AsVector := v1.Normalize;
end;

{%endregion}

{%region%----[ Pure Pascal may Convert at some point ]--------------------------}

{ TODO 1 -oASM -cTHmgPlane : Create(point, normal) AddASM Version }
procedure TBZHmgPlane.Create(constref point, normal : TBZVector);
begin
   Self.AsNormal3:= normal.AsVector3f;
   Self.W:=-(Point.DotProduct(normal));
end;

function TBZHmgPlane.Distance(constref Center : TBZVector; constref Radius:Single) : Single;
var dist : single;
begin
  dist := Self.Distance(Center);
  result := 0.0;
  if(Abs(dist) > Radius) then
    if Dist > 0 then
      result := dist - radius
    else
      result := dist + radius;
end;

function TBZHmgPlane.IsInHalfSpace(constref point: TBZVector): Boolean;
begin
  //Result := (EvaluatePoint(point) > 0);
  Result := ((Self.X*Point.X) + (Self.Y*Point.Y) + (Self.Z*Point.Z) + Self.W) > 0;
end;



{%endregion}

{$IFNDEF USE_ASM}

{%region----[ PASCAL VERSION OF ASSEMBLER ROUTINES BELOW ]----------------------}

procedure TBZHmgPlane.Create(constref p1, p2, p3 : TBZVector);
begin
  CalcNormal(p1, p2, p3);
  Self.W:=-(p1.DotProduct(Self.AsVector));
end;


{%region%----[ Operators ]------------------------------------------------------}

{%endregion%}

{%region%----[ Functions ]------------------------------------------------------}

function TBZHmgPlane.Perpendicular(constref P : TBZVector4f) : TBZVector4f;
var
   dot : Single;
begin
   dot:= Self.AsVector.DotProduct(P);
   Result.X:=P.X-Dot * Self.X;
   Result.Y:=P.Y-Dot * Self.Y;
   Result.Z:=P.Z-Dot * Self.Z;
   Result.W:=P.W;
end;

function TBZHmgPlane.Reflect(constref V: TBZVector4f): TBZVector4f;
begin
  //  Result:=Self.Combine(Self, N, 1, -2*VectorDotProduct(Self, N));
//  Result := Self - (N*(2 * N.Dotproduct(Self)));
  Result :=  V - (Self.AsVector * (2 * V.DotProduct(Self.AsVector)));
end;

function TBZHmgPlane.Distance(constref point : TBZVector) : Single;
begin
  result:= (Self.X*Point.X) + (Self.Y*Point.Y) + (Self.Z*Point.Z) + Self.W;
end;

function TBZHmgPlane.AbsDistance(constref point : TBZVector) : Single;
begin
  result:= system.abs((Self.X*Point.X) + (Self.Y*Point.Y) + (Self.Z*Point.Z) + Self.W);
end;

function TBZHmgPlane.Normalized:TBZHmgPlane;
begin
   Result := self;
   Result.Normalize;
end;

procedure TBZHmgPlane.Normalize;
var
   n : Single;
begin
   n:=1/Sqrt(Self.X*Self.X+Self.Y*Self.Y+Self.Z*Self.Z);
   Self.AsVector := Self.AsVector * n;
end;

{%endregion%}

//function TBZHmgPlaneHelper.EvaluatePoint(constref Point : TBZVector) : Single;
//begin
//  Result := Self.X*point.X+Self.Y*point.Y+Self.Z*point.Z+Self.W;
//end;

{%endregion%}

{$ENDIF}
