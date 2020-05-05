{%region%----[ Pure Pascal will never Convert ]---------------------------------}
// Temp comment this region is where we keep pascal routines for which it does
// not make sense to use SSE/AVX

procedure TBZVector2f.Create(aX,aY: single);
begin
  Self.X := aX;
  Self.Y := aY;
end;

function TBZVector2f.ToString : String;
begin
   Result := '(X: '+FloattoStrF(Self.X,fffixed,5,5)+
            ' ,Y: '+FloattoStrF(Self.Y,fffixed,5,5)+')';
End;

class operator TBZVector2f.-(constref A: TBZVector2f): TBZVector2f;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
end;

class operator TBZVector2f.=(constref A, B: TBZVector2f): Boolean;
begin
 result := ((A.X = B.X) And (A.Y = B.Y));
end;

(*class operator >=(constref A, B: TVector4f): Boolean;
class operator <=(constref A, B: TVector4f): Boolean;
class operator >(constref A, B: TVector4f): Boolean;
class operator <(constref A, B: TVector4f): Boolean; *)

class operator TBZVector2f.<>(constref A, B: TBZVector2f): Boolean;
begin
  result := ((A.X <> B.X) or (A.Y <> B.Y));
end;

function TBZVector2f.DotProduct(A:TBZVector2f):Single;
begin
  Result := (Self.X * A.X) + (Self.Y * A.Y);
end;

function TBZVector2f.CrossProduct(A:TBZVector2f):TBZVector2f;
begin
  Result.x := Self.y - A.y; // (Self.y*A.z) - (A.y*Self.z) // Z = 1;
  Result.y := A.x - Self.x; // (A.x*Self.z) - (Self.x*A.z) // Z = 1
  // Result.Z := (Self.x*A.y) - (Self.y*A.x)
end;

function TBZVector2f.Perp(A : TBZVector2f) : Single;
begin
  Result := (Self.x * A.y) - (Self.y * A.x);
end;

function TBZVector2f.AngleCosine(constref A: TBZVector2f): Single;
begin
   Result:=Self.DotProduct(A)/(Self.Length*A.Length);
   //result := arctan2(
   //    A.x * Self.y - A.y * Self.x,
   //    A.x * Self.x + A.y * Self.y); // = ANGLE
end;

function TBZVector2f.AngleBetween(Constref A, ACenterPoint : TBZVector2f): Single;
Var
  vt1,vt2  :  TBZVector2f;
begin
  vt1 := Self - ACenterPoint;
  vt2 := A - ACenterPoint;
  vt1 := vt1.Normalize;
  vt2 := vt2.Normalize;
  Result := ArcCos(vt1.AngleCosine(vt2));
end;

function TBZVector2f.AngleBetweenPointsInDeg(C: TBZVector2f):Single;
var
  Angle : Single;
begin
   //Angle :=  Math.arctan2(P.y - C.y, P.x - C.x)
   //         -Math.arctan2(Self.y - C.y, Self.x - C.x);
   Angle := Math.Arctan2((C.y - Self.y), (C.x - Self.x));
   result := (Angle * cRadian);
   if Result < 0 then result := result + 360;
   //if Result<=180 then Result := System.Abs(Result- 180);
end;

function TBZVector2f.Rotate(anAngle : Single; aCenterPoint : TBZVector2f) : TBZVector2f;
Var
   Angle, S, C, nx, ny : Single;
   P : TBZVector2f;
begin
  Angle := anAngle * cPIdiv180;
  P := aCenterPoint - Self;
  Math.SinCos(Angle, S, C);
  // Sens Anti-horaire
  nx := (P.x * C) + (P.y * S) + aCenterPoint.x;
  ny := (-P.x * S) + (P.y * C) + aCenterPoint.y;
  // Sens horaire
  //nx := (P.x * C) - (P.y * S) + aCenterPoint.x;
  //ny := (P.x * S) + (P.y * C) + aCenterPoint.y;
  Result.Create(nx,ny);
end;

{%endregion}

{%region%----[ Pure Pascal may Convert at some point ]--------------------------}

//class operator TBZVector2f.mod(const a,b:TBZVector2f): TBZVector2f;
//var
//  c: TBZVector2f;
//begin
//  c:=a/b;
//  result := a-b * c.Trunc;//BZVectorMath.trunc(c);
//end;

function TBZVector2f.ChebyshevDistance(constref A : TBZVector2f) : Single;
var
  t : TBZVector2f;
begin
  t := (Self - A).abs;
  if (t.x > t.y) then result := t.x
  else result := t.y;
end;

function TBZVector2f.MinkovskiDistance(constref A : TBZVector2f): Single;
Var
  {$CODEALIGN VARMIN=16}
  p1, p2, p3 : TBZVector2f;
  {$CODEALIGN VARMIN=4}
begin
  //Result := Power(Power(Abs(x1 - x2), 3) + Power(Abs(y1 - y2), 3), (1 / 3));
  p1 :=  (Self - A).Abs;
  p2 := p1 * p1 * p1;
  Result := power((p2.x + p2.y), cInvThree);
end;

function TBZVector2f.BaryCenter(constRef B : TBZVector2f; const WeightA : Single; Const WeightB : Single) : TBZVector2f;
Var
  {$CODEALIGN VARMIN=16}
  p1, p2, p3 : TBZVector2f;
  {$CODEALIGN VARMIN=4}
begin
  Result := Self;
  if (WeightA + WeightB <> 0.0) then
  begin
    p1.Create(WeightA, WeightA);
    p2.Create(WeightB, WeightB);
    Result := Result * p1;
    p3 := B * p2;
    Result := Result + p3;
    p3 := p1 + p2;
    Result := Result / p3;
  end;
end;



{%endregion}

{$IFNDEF USE_ASM}

{%region----[ PASCAL VERSION OF ASSEMBLER ROUTINES BELOW ]----------------------}

{%region%----[ Operators ]------------------------------------------------------}
class operator TBZVector2f.+(constref A, B: TBZVector2f): TBZVector2f;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

class operator TBZVector2f.-(constref A, B: TBZVector2f): TBZVector2f;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

class operator TBZVector2f.*(constref A, B: TBZVector2f): TBZVector2f;
begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
end;

class operator TBZVector2f.*(constref A:TBZVector2f; Constref B: TBZVector2i): TBZVector2f;
begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
end;

class operator TBZVector2f./(constref A, B: TBZVector2f): TBZVector2f;
begin
  Result.X := A.X / B.X;
  Result.Y := A.Y / B.Y;
end;

class operator TBZVector2f.+(constref A: TBZVector2f; constref B:Single): TBZVector2f;
begin
  Result.X := A.X + B;
  Result.Y := A.Y + B;
end;

class operator TBZVector2f.-(constref A: TBZVector2f; constref B:Single): TBZVector2f;
begin
  Result.X := A.X - B;
  Result.Y := A.Y - B;
end;

class operator TBZVector2f.*(constref A: TBZVector2f; constref B:Single): TBZVector2f;
begin
  Result.X := A.X * B;
  Result.Y := A.Y * B;
end;

class operator TBZVector2f./(constref A: TBZVector2f; constref B:Single): TBZVector2f;
Var
  d : Single;
begin
  d :=1/B;
  Result.X := A.X * d;
  Result.Y := A.Y * d;
end;

class operator TBZVector2f./(constref A: TBZVector2f; constref B:TBZVector2i): TBZVector2f;
begin
  Result.X := A.X / B.X;
  Result.Y := A.Y / B.Y;
end;

class operator TBZVector2f.+(constref A: TBZVector2f; constref B: TBZVector2i): TBZVector2f;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

class operator TBZVector2f.-(constref A: TBZVector2f; constref B: TBZVector2i): TBZVector2f;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

{%endregion%}

{%region%----[ Functions ]------------------------------------------------------}

function TBZVector2f.Min(constref B: TBZVector2f): TBZVector2f;
begin
  Result := Self;
  if Result.X > B.X then Result.X := B.X;
  //:= Math.Min(Self.X, B.X);
  //Result.Y := Math.Min(Self.Y, B.Y);
  if Result.Y > B.Y then Result.Y := B.Y;
end;

function TBZVector2f.Min(constref B: Single): TBZVector2f;
Begin
  Result := Self;
  Result.X := Math.Min(Self.X, B);
  Result.Y := Math.Min(Self.Y, B);
end;

function TBZVector2f.Max(constref B: TBZVector2f): TBZVector2f;
begin
  Result := Self;
  //Result.X := Math.Max(Self.X, B.X);
  //Result.Y := Math.Max(Self.Y, B.Y);
  Result := Self;
  if Result.X < B.X then Result.X := B.X;
  if Result.Y < B.Y then Result.Y := B.Y;
end;

function TBZVector2f.Max(constref B: Single): TBZVector2f;
begin
  Result := Self;
  Result.X := Math.Max(Self.X, B);
  Result.Y := Math.Max(Self.Y, B);
end;

function TBZVector2f.Clamp(constref AMin, AMax: TBZVector2f): TBZVector2f;
begin
  Result.X := Math.Min(Math.Max(Self.X, AMin.X),AMax.X);
  Result.Y := Math.Min(Math.Max(Self.Y, AMin.Y),AMax.Y);
end;

function TBZVector2f.Clamp(constref AMin, AMax: Single): TBZVector2f;
begin
  Result.X := Math.Min(Math.Max(Self.X, AMin),AMax);
  Result.Y := Math.Min(Math.Max(Self.Y, AMin),AMax);
end;

function TBZVector2f.MulAdd(constref A,B:TBZVector2f): TBZVector2f;
begin
  Result := (Self*A) + B;
end;

function TBZVector2f.MulSub(constref A,B:TBZVector2f): TBZVector2f;
begin
  Result := (Self*A) - B;
end;

function TBZVector2f.MulDiv(constref A,B:TBZVector2f): TBZVector2f;
begin
  Result := (Self*A) / B;
end;

function TBZVector2f.Length:Single;
begin
  Result := System.Sqrt((Self.X * Self.X) + (Self.Y * Self.Y));
end;

function TBZVector2f.LengthSquare:Single;
begin
  Result := (Self.X * Self.X) + (Self.Y * Self.Y);
end;

function TBZVector2f.Distance(constref A:TBZVector2f):Single;
begin
  Result := (Self - A).Length;
end;

function TBZVector2f.DistanceSquare(constref A:TBZVector2f):Single;
var
  t : TBZVector2f;
begin
   t := Self - A;
   Result := t.LengthSquare;
end;

function TBZVector2f.ManhattanDistance(constref A : TBZVector2f) : Single;
var
  t : TBZVector2f;
begin
  t := (Self - A).abs;
  Result := t.x + t.y;
end;

function TBZVector2f.Normalize : TBZVector2f;
begin
 Result := Self * (1/Self.Length);
end;

function TBZVector2f.Center(ConstRef B : TBZVector2f) : TBZVector2f;
begin
 Result := (Self + B) * 0.5;
end;

function TBZVector2f.Abs: TBZvector2f;
begin
  Result.X := System.abs(Self.X);
  Result.Y := System.abs(Self.Y);
end;

function TBZVector2f.Round : TBZVector2i;
begin
 result.X := System.Round(Self.X);
 result.Y := System.Round(Self.Y);
end;

function TBZVector2f.Trunc : TBZVector2i;
begin
 result.X := System.Trunc(Self.X);
 result.Y := System.Trunc(Self.Y);
end;

function TBZVector2f.Floor : TBZVector2i;
begin
 result.X := BZMath.Floor(Self.X);
 result.Y := BZMath.Floor(Self.Y);
end;

function TBZVector2f.Ceil : TBZVector2i;
begin
 result.X := BZMath.Ceil(Self.X);
 result.Y := BZMath.Ceil(Self.Y);
end;

function TBZVector2f.Fract : TBZVector2f;
begin
 result.X := BZMath.Fract(Self.X);
 result.Y := BZMath.Fract(Self.Y);
end;

function TBZVector2f.Sqrt : TBZVector2f;
begin
 result.X := System.Sqrt(Self.X);
 result.Y := System.Sqrt(Self.Y);
end;

function TBZVector2f.InvSqrt : TBZVector2f;
begin
 result.X := BZMath.InvSqrt(Self.X);
 result.Y := BZMath.InvSqrt(Self.Y);
end;

function TBZVector2f.ModF(Constref A : TBZVector2f): TBZVector2f;
var
  t : TBZVector2i;
  vt : TBZVector2f;
begin
  vt := (Self / a);
  t := vt.floor;
  vt.x:=t.x;
  vt.y:=t.y;
  result := (Self - (a*vt));
end;

function TBZVector2f.fMod(Constref A : TBZVector2f): TBZVector2i;
var
  t : TBZVector2i;
  vt : TBZVector2f;
begin
  vt := (Self / A);
  t := vt.floor;
  vt.x:=t.x;
  vt.y:=t.y;
  vt := (Self - (A*vt));
  result := vt.Trunc;
end;

{%endregion%}

{%endregion%}

{$ENDIF}
