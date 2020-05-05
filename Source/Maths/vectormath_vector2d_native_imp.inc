{%region%----[ Pure Pascal will never Convert ]---------------------------------}
// Temp comment this region is where we keep pascal routines for which it does
// not make sense to use SSE/AVX

procedure TBZVector2d.Create(aX,aY: Double);
begin
  Self.X := aX;
  Self.Y := aY;
end;

function TBZVector2d.ToString : String;
begin
   Result := '(X: '+FloattoStrF(Self.X,fffixed,5,5)+
            ' ,Y: '+FloattoStrF(Self.Y,fffixed,5,5)+')';
End;

class operator TBZVector2d.-(constref A: TBZVector2d): TBZVector2d;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
end;

class operator TBZVector2d.=(constref A, B: TBZVector2d): Boolean;
begin
 result := ((A.X = B.X) And (A.Y = B.Y));
end;

(*class operator >=(constref A, B: TVector4f): Boolean;
class operator <=(constref A, B: TVector4f): Boolean;
class operator >(constref A, B: TVector4f): Boolean;
class operator <(constref A, B: TVector4f): Boolean; *)

class operator TBZVector2d.<>(constref A, B: TBZVector2d): Boolean;
begin
  result := ((A.X <> B.X) or (A.Y <> B.Y));
end;

function TBZVector2d.DotProduct(A:TBZVector2d):Double;
begin
  Result := (X * A.X) + (Y * A.Y);
end;

function TBZVector2d.AngleCosine(constref A: TBZVector2d): Double;
begin
   Result:=Self.DotProduct(A)/(Self.Length*A.Length);
end;

function TBZVector2d.AngleBetween(Constref A, ACenterPoint : TBZVector2d): Double;
Var
  vt1,vt2  :  TBZVector2d;
begin
  vt1 := Self - ACenterPoint;
  vt2 := A - ACenterPoint;
  vt1 := vt1.Normalize;
  vt2 := vt2.Normalize;
  Result := ArcCos(vt1.AngleCosine(vt2));
end;

class operator TBZVector2d.+(constref A: TBZVector2d; constref B:Double): TBZVector2d;
begin
  Result.X := A.X + B;
  Result.Y := A.Y + B;
end;

class operator TBZVector2d.-(constref A: TBZVector2d; constref B:Double): TBZVector2d;
begin
  Result.X := A.X - B;
  Result.Y := A.Y - B;
end;

class operator TBZVector2d.*(constref A: TBZVector2d; constref B:Double): TBZVector2d;
begin
  Result.X := A.X * B;
  Result.Y := A.Y * B;
end;

class operator TBZVector2d./(constref A: TBZVector2d; constref B:Double): TBZVector2d;
Var
  d : Double;
begin
  d :=1/B;
  Result.X := A.X * d;
  Result.Y := A.Y * d;
end;

{%endregion}

{%region%----[ Pure Pascal may Convert at some point ]--------------------------}

//class operator TBZVector2d.mod(const a,b:TBZVector2d): TBZVector2d;
//var
//  c: TBZVector2d;
//begin
//  c:=a/b;
//  result := a-b * c.Trunc;//BZVectorMath.trunc(c);
//end;

function TBZVector2d.InvSqrt : TBZVector2d;
begin
 result.X := BZMath.InvSqrt(Self.X);
 result.Y := BZMath.InvSqrt(Self.Y);
end;

function TBZVector2d.Fract : TBZVector2d;
begin
 result.X := BZMath.Fract(Self.X);
 result.Y := BZMath.Fract(Self.Y);
end;

{%endregion}

{$IFNDEF USE_ASM}

{%region----[ PASCAL VERSION OF ASSEMBLER ROUTINES BELOW ]----------------------}

{%region%----[ Operators ]------------------------------------------------------}
class operator TBZVector2d.+(constref A, B: TBZVector2d): TBZVector2d;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

class operator TBZVector2d.-(constref A, B: TBZVector2d): TBZVector2d;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

class operator TBZVector2d.*(constref A, B: TBZVector2d): TBZVector2d;
begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
end;

class operator TBZVector2d.*(constref A:TBZVector2d; Constref B: TBZVector2i): TBZVector2d;
begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
end;

class operator TBZVector2d./(constref A, B: TBZVector2d): TBZVector2d;
begin
  Result.X := A.X / B.X;
  Result.Y := A.Y / B.Y;
end;

class operator TBZVector2d./(constref A: TBZVector2d; constref B:TBZVector2i): TBZVector2d;
begin
  Result.X := A.X / B.X;
  Result.Y := A.Y / B.Y;
end;

class operator TBZVector2d.+(constref A: TBZVector2d; constref B: TBZVector2i): TBZVector2d;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

class operator TBZVector2d.-(constref A: TBZVector2d; constref B: TBZVector2i): TBZVector2d;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

{%endregion%}

{%region%----[ Functions ]------------------------------------------------------}

function TBZVector2d.Min(constref B: TBZVector2d): TBZVector2d;
begin
  Result := Self;
  if Result.X > B.X then Result.X := B.X;
  //:= Math.Min(Self.X, B.X);
  //Result.Y := Math.Min(Self.Y, B.Y);
  if Result.Y > B.Y then Result.Y := B.Y;
end;

function TBZVector2d.Min(constref B: Double): TBZVector2d;
Begin
  Result := Self;
  Result.X := Math.Min(Self.X, B);
  Result.Y := Math.Min(Self.Y, B);
end;

function TBZVector2d.Max(constref B: TBZVector2d): TBZVector2d;
begin
  Result := Self;
  //Result.X := Math.Max(Self.X, B.X);
  //Result.Y := Math.Max(Self.Y, B.Y);
  Result := Self;
  if Result.X < B.X then Result.X := B.X;
  if Result.Y < B.Y then Result.Y := B.Y;
end;

function TBZVector2d.Max(constref B: Double): TBZVector2d;
begin
  Result := Self;
  Result.X := Math.Max(Self.X, B);
  Result.Y := Math.Max(Self.Y, B);
end;

function TBZVector2d.Clamp(constref AMin, AMax: TBZVector2d): TBZVector2d;
begin
  Result.X := Math.Min(Math.Max(Self.X, AMin.X),AMax.X);
  Result.Y := Math.Min(Math.Max(Self.Y, AMin.Y),AMax.Y);
end;

function TBZVector2d.Clamp(constref AMin, AMax: Double): TBZVector2d;
begin
  Result.X := Math.Min(Math.Max(Self.X, AMin),AMax);
  Result.Y := Math.Min(Math.Max(Self.Y, AMin),AMax);
end;

function TBZVector2d.MulAdd(constref A,B:TBZVector2d): TBZVector2d;
begin
  Result := (Self*A) + B;
end;

function TBZVector2d.MulSub(constref A,B:TBZVector2d): TBZVector2d;
begin
  Result := (Self*A) - B;
end;

function TBZVector2d.MulDiv(constref A,B:TBZVector2d): TBZVector2d;
begin
  Result := (Self*A) / B;
end;

function TBZVector2d.Length:Double;
begin
  Result := System.Sqrt((Self.X * Self.X) + (Self.Y * Self.Y));
end;

function TBZVector2d.LengthSquare:Double;
begin
  Result := (Self.X * Self.X) + (Self.Y * Self.Y);
end;

function TBZVector2d.Distance(constref A:TBZVector2d):Double;
begin
  Result := (Self - A).Length;
end;

function TBZVector2d.DistanceSquare(constref A:TBZVector2d):Double;
var
  t : TBZVector2d;
begin
   t := Self - A;
   Result := t.LengthSquare;
end;

function TBZVector2d.Normalize : TBZVector2d;
begin
 Result := Self * (1/Self.Length);
end;

function TBZVector2d.Abs: TBZVector2d;
begin
  Result.X := System.abs(Self.X);
  Result.Y := System.abs(Self.Y);
end;

function TBZVector2d.Round : TBZVector2i;
begin
 result.X := System.Round(Self.X);
 result.Y := System.Round(Self.Y);
end;

function TBZVector2d.Trunc : TBZVector2i;
begin
 result.X := System.Trunc(Self.X);
 result.Y := System.Trunc(Self.Y);
end;

function TBZVector2d.Floor : TBZVector2i;
begin
 result.X := BZMath.Floor(Self.X);
 result.Y := BZMath.Floor(Self.Y);
end;

function TBZVector2d.Ceil : TBZVector2i;
begin
 result.X := BZMath.Ceil(Self.X);
 result.Y := BZMath.Ceil(Self.Y);
end;

//function TBZVector2d.Fract : TBZVector2d;
//begin
// result.X := BZMath.Fract(Self.X);
// result.Y := BZMath.Fract(Self.Y);
//end;

function TBZVector2d.Sqrt : TBZVector2d;
begin
 result.X := System.Sqrt(Self.X);
 result.Y := System.Sqrt(Self.Y);
end;

//function TBZVector2d.InvSqrt : TBZVector2d;
//begin
// result.X := BZMath.InvSqrt(Self.X);
// result.Y := BZMath.InvSqrt(Self.Y);
//end;

function TBZVector2d.ModF(Constref A : TBZVector2d): TBZVector2d;
var
  t : TBZVector2i;
  vt : TBZVector2d;
begin
  vt := (Self / a);
  t := vt.floor;
  vt.x:=t.x;
  vt.y:=t.y;
  result := (Self - (a*vt));
end;

function TBZVector2d.fMod(Constref A : TBZVector2d): TBZVector2i;
var
  t : TBZVector2i;
  vt : TBZVector2d;
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
