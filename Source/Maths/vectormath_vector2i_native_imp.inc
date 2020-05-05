{%region%----[ Pure Pascal will never Convert ]---------------------------------}
// Temp comment this region is where we keep pascal routines for which it does
// not make sense to use SSE/AVX

procedure TBZVector2i.Create(aX,aY: Integer);
begin
  Self.X := aX;
  Self.Y := aY;
end;

function TBZVector2i.ToString : String;
begin
   Result := '(X: '+FloattoStrF(Self.X,fffixed,5,5)+
            ' ,Y: '+FloattoStrF(Self.Y,fffixed,5,5)+')';
End;

class operator TBZVector2i.-(constref A: TBZVector2i): TBZVector2i;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
end;

class operator TBZVector2i.=(constref A, B: TBZVector2i): Boolean;
begin
 result := ((A.X = B.X) And (A.Y = B.Y));
end;

(*class operator >=(constref A, B: TVector4f): Boolean;
class operator <=(constref A, B: TVector4f): Boolean;
class operator >(constref A, B: TVector4f): Boolean;
class operator <(constref A, B: TVector4f): Boolean; *)

class operator TBZVector2i.<>(constref A, B: TBZVector2i): Boolean;
begin
  result := ((A.X <> B.X) or (A.Y <> B.Y));
end;

function TBZVector2i.DotProduct(A:TBZVector2i):Single;
begin
  Result := (X * A.X) + (Y * A.Y);
end;

function TBZVector2i.AngleCosine(constref A: TBZVector2i): Single;
begin
   Result:=Self.DotProduct(A)/(Self.Length*A.Length);
end;

function TBZVector2i.AngleBetween(Constref A, ACenterPoint : TBZVector2i): Single;
Var
  vt1,vt2: TBZVector2i;
  vf1,vf2: TBZVector2f;
begin
  vt1 := Self - ACenterPoint;
  vt2 := A - ACenterPoint;
  vf1 := vt1.Normalize;
  vf2 := vt2.Normalize;
  Result := ArcCos(vf1.AngleCosine(vf2));
end;

{%endregion}

{%region%----[ Pure Pascal may Convert at some point ]--------------------------}

class operator TBZVector2i.Mod(Constref A, B : TBZVector2i): TBZVector2i;
begin
  Result.x := A.X Mod B.X;
  Result.Y := A.Y Mod B.Y;
end;

{%endregion}

{$IFNDEF USE_ASM}

{%region----[ PASCAL VERSION OF ASSEMBLER ROUTINES BELOW ]----------------------}

{%region%----[ Operators ]------------------------------------------------------}

class operator TBZVector2i.+(constref A, B: TBZVector2i): TBZVector2i;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

class operator TBZVector2i.+(constref A: TBZVector2i; constref B:Integer): TBZVector2i;
begin
  Result.X := A.X + B;
  Result.Y := A.Y + B;
end;

class operator TBZVector2i.-(constref A, B: TBZVector2i): TBZVector2i;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

class operator TBZVector2i.-(constref A: TBZVector2i; constref B:Integer): TBZVector2i;
begin
  Result.X := A.X - B;
  Result.Y := A.Y - B;
end;

class operator TBZVector2i.-(constref A: TBZVector2i; constref B:Single): TBZVector2i;
begin
  Result.X := Round(A.X - B);
  Result.Y := Round(A.Y - B);
end;

class operator TBZVector2i.*(constref A, B: TBZVector2i): TBZVector2i;
begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
end;

//class operator TBZVector2i.*(constref A:TBZVector2i; Constref B: TBZVector2f): TBZVector2i;
//begin
//  Result.X := Round(A.X * B.X);
//  Result.Y := Round(A.Y * B.Y);
//end;

class operator TBZVector2i.div(constref A, B: TBZVector2i): TBZVector2i;
begin
  Result.X := A.X div B.X;
  Result.Y := A.Y div B.Y;
end;

class operator TBZVector2i.div(constref A : TBZVector2i;Constref B:Integer ): TBZVector2i;
begin
  Result.X := A.X div B;
  Result.Y := A.Y div B;
end;

class operator TBZVector2i.+(constref A: TBZVector2i; constref B:Single): TBZVector2i;
begin
  Result.X := Round(A.X + B);
  Result.Y := Round(A.Y + B);
end;

class operator TBZVector2i.*(constref A: TBZVector2i; constref B:Single): TBZVector2i;
begin
  Result.X := Round(A.X * B);
  Result.Y := Round(A.Y * B);
end;

//For Increase performance add this to an TBZVector2iHelper and you TBZVector2f for compute values
class operator TBZVector2i./(constref A: TBZVector2i; constref B:Single): TBZVector2i;
Var
  d : Single;
  //v2 : TBZVector2f;
begin
  d :=1/B;
  //v2.Create(self);
  //v2 := v2 * d
  //result := v2.Trunc;
  Result.X := Round(A.X * d);
  Result.Y := Round(A.Y * d);
end;



{%endregion%}

{%region%----[ Functions ]------------------------------------------------------}

function TBZVector2i.Min(constref B: TBZVector2i): TBZVector2i;
begin
  Result := Self;
  if Result.X > B.X then Result.X := B.X;
  //:= Math.Min(Self.X, B.X);
  //Result.Y := Math.Min(Self.Y, B.Y);
  if Result.Y > B.Y then Result.Y := B.Y;
end;

function TBZVector2i.Min(constref B: Integer): TBZVector2i;
Begin
  Result := Self;
  Result.X := Math.Min(Self.X, B);
  Result.Y := Math.Min(Self.Y, B);
end;

function TBZVector2i.Max(constref B: TBZVector2i): TBZVector2i;
begin
  Result := Self;
  if Result.X < B.X then Result.X := B.X;
  if Result.Y < B.Y then Result.Y := B.Y;
end;

function TBZVector2i.Max(constref B: Integer): TBZVector2i;
begin
  Result := Self;
  Result.X := Math.Max(Self.X, B);
  Result.Y := Math.Max(Self.Y, B);
end;

function TBZVector2i.Clamp(constref AMin, AMax: TBZVector2i): TBZVector2i;
begin
  Result.X := Math.Min(Math.Max(Self.X, AMin.X),AMax.X);
  Result.Y := Math.Min(Math.Max(Self.Y, AMin.Y),AMax.Y);
end;

function TBZVector2i.Clamp(constref AMin, AMax: Integer): TBZVector2i;
begin
  Result.X := Math.Min(Math.Max(Self.X, AMin),AMax);
  Result.Y := Math.Min(Math.Max(Self.Y, AMin),AMax);
end;

function TBZVector2i.MulAdd(constref A,B:TBZVector2i): TBZVector2i;
begin
  Result := (Self*A) + B;
end;

function TBZVector2i.MulDiv(constref A,B:TBZVector2i): TBZVector2i;
begin
  Result := (Self*A) div B;
end;

function TBZVector2i.Length:Single;
begin
  Result := System.Sqrt((Self.X * Self.X) + (Self.Y * Self.Y));
end;

function TBZVector2i.LengthSquare:Single;
begin
  Result := (Self.X * Self.X) + (Self.Y * Self.Y);
end;

function TBZVector2i.Distance(constref A:TBZVector2i):Single;
begin
  Result := (Self - A).Length;
end;

function TBZVector2i.DistanceSquare(constref A:TBZVector2i):Single;
var
  t : TBZVector2i;
begin
   t := Self - A;
   Result := t.LengthSquare;
end;

function TBZVector2i.Abs: TBZVector2i;
begin
  Result.X := System.abs(Self.X);
  Result.Y := System.abs(Self.Y);
end;


{%endregion%}

{%region%----[ 2i Helper ]------------------------------------------------------}

function TBZVector2iHelper.GetYX : TBZVector2i;
begin
  Result.X := Self.Y;
  Result.Y := Self.X;
end;

function TBZVector2iHelper.Normalize : TBZVector2f;
begin
  Result.Create(Self.X,Self.Y);
  Result := Result.Normalize;
end;
{%endregion%}

{%endregion%}

{$ENDIF}

function TBZVector2iHelper.AsVector2f : TBZVector2f;
begin
  Result.X := Self.X;
  Result.Y := Self.Y;
end;

