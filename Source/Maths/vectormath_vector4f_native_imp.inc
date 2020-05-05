{%region%----[ Pure Pascal will never Convert ]---------------------------------}
// Temp comment this region is where we keep pascal routines for which it does
// not make sense to use SSE/AVX

procedure TBZVector4f.Create(const AValue: single);
begin
  Self.X := AValue;
  Self.Y := AValue;
  Self.Z := AValue;
  Self.W := AValue;
end;

procedure TBZVector4f.CreatePoint(const AValue: single);
begin
  Self.X := AValue;
  Self.Y := AValue;
  Self.Z := AValue;
  Self.W := 1.0;
end;

procedure TBZVector4f.CreatePoint(Const aX,aY,aZ: single);
begin
  Self.X := aX;
  Self.Y := aY;
  Self.Z := aZ;
  Self.W := 1.0;
end;

procedure TBZVector4f.Create(Const aX,aY,aZ: single; const aW : Single = 0);
begin
   Self.X := AX;
   Self.Y := AY;
   Self.Z := AZ;
   Self.W := AW;
end;

procedure TBZVector4f.Create(Const anAffineVector: TBZVector3f; const aW : Single = 0); //W=1 for a point
begin
   Self.X := anAffineVector.X;
   Self.Y := anAffineVector.Y;
   Self.Z := anAffineVector.Z;
   Self.W := AW;
end;

procedure TBZVector4f.CreatePoint(Const anAffineVector: TBZVector3f);
begin
   Self.X := anAffineVector.X;
   Self.Y := anAffineVector.Y;
   Self.Z := anAffineVector.Z;
   Self.W := 1.0;
end;

function TBZVector4f.ToString : String;
begin
   Result := '(X: '+FloattoStrF(Self.X,fffixed,5,5)+
            ' ,Y: '+FloattoStrF(Self.Y,fffixed,5,5)+
            ' ,Z: '+FloattoStrF(Self.Z,fffixed,5,5)+
            ' ,W: '+FloattoStrF(Self.W,fffixed,5,5)+')';
End;

function TBZVector4f.Shuffle(const x,y,z,w : Byte):TBZVector4f;
begin
  Result.X:=Self.V[x];
  Result.Y:=Self.V[y];
  Result.Z:=Self.V[z];
  Result.W:=Self.V[w];
End;

function TBZVector4f.MinXYZComponent : Single;
begin
   Result:=BZMath.Min(Self.X, Self.Y, Self.Z);
end;

function TBZVector4f.MaxXYZComponent : Single;
begin
   Result:=BZMath.Max(Self.X, Self.Y, Self.Z);
end;

// Horizontal min/max.
(* VM_INLINE float hmin(float3 v)
 v  = xyz
 v2 = yxz
 v  = min(v,v2)
 v2 = zxy
 result = min(v,v2);

	v = min(v, SHUFFLE3(v, 1, 0, 2));
	return min(v, SHUFFLE3(v, 2, 0, 1)).x();


VM_INLINE float hmax(float3 v)
	v = max(v, SHUFFLE3(v, 1, 0, 2));
	return max(v, SHUFFLE3(v, 2, 0, 1)).x();
*)

{%endregion}

{%region%----[ Pure Pascal may Convert at some point ]--------------------------}

function TBZVector4f.Swizzle(const ASwizzle: TBZVector4SwizzleRef ): TBZVector4f;
begin
   //FSwizzleMode :=  ASwizzle;
   case ASwizzle of
     //swXYXY movlhps
     //swZWZW movhlps
     //swXXYY unpacklo
     //swZZWW unpackhi
     //swXXZZ movldup
     //swYYWW movhdup
     swXXXX, swRRRR :  // Shufps ,00000000b
     begin
       Result.X := Self.X;
       Result.Y := Self.X;
       Result.Z := Self.X;
       Result.W := Self.X;
     end;
     swYYYY, swGGGG :  // Shufps ,01010101b
     begin
       Result.X := Self.Y;
       Result.Y := Self.Y;
       Result.Z := Self.Y;
       Result.W := Self.Y;
     end;
     swZZZZ, swBBBB : // Shufps ,10101010b
     begin
       Result.X := Self.Z;
       Result.Y := Self.Z;
       Result.Z := Self.Z;
       Result.W := Self.Z;
     end;
     swWWWW, swAAAA :  // Shufps ,11111111b
     begin
       Result.X := Self.W;
       Result.Y := Self.W;
       Result.Z := Self.W;
       Result.W := Self.W;
     end;
     swZYXW, swBGRA : // Shufps ,11000110b ==>  ARGB
     begin
       Result.X := Self.Z;
       Result.Y := Self.Y;
       Result.Z := Self.X;
       Result.W := Self.W;
     end;
     swXZYW, swRBGA :  // Shufps ,11011000b
     begin
       Result.X := Self.X;
       Result.Y := Self.Z;
       Result.Z := Self.Y;
       Result.W := Self.W;
     end;
     swZXYW, swBRGA :
     begin
       Result.X := Self.Z;
       Result.Y := Self.X;
       Result.Z := Self.Y;
       Result.W := Self.W;
     end;
     swYXZW, swGRBA :
     begin
       Result.X := Self.Y;
       Result.Y := Self.X;
       Result.Z := Self.Z;
       Result.W := Self.W;
     end;
     swYZXW, swGBRA :
     begin
       Result.X := Self.Y;
       Result.Y := Self.Z;
       Result.Z := Self.X;
       Result.W := Self.W;
     end;
     swWXYZ, swARGB :
     begin
       Result.X := Self.W;
       Result.Y := Self.X;
       Result.Z := Self.Y;
       Result.W := Self.Z;
     end;
     swWZYX, swABGR :
     begin
       Result.X := Self.W;
       Result.Y := Self.Z;
       Result.Z := Self.Y;
       Result.W := Self.X;
     end;
     swWXZY, swARBG :
     begin
       Result.X := Self.W;
       Result.Y := Self.X;
       Result.Z := Self.Z;
       Result.W := Self.Y;
     end;
     swWZXY, swABRG :
     begin
       Result.X := Self.W;
       Result.Y := Self.Z;
       Result.Z := Self.X;
       Result.W := Self.Y;
     end;
     swWYXZ, swAGRB :
     begin
       Result.X := Self.W;
       Result.Y := Self.Y;
       Result.Z := Self.X;
       Result.W := Self.Z;
     end;
     swWYZX, swAGBR :
     begin
       Result.X := Self.W;
       Result.Y := Self.Y;
       Result.Z := Self.Z;
       Result.W := Self.X;
     end;

     else result := self; //swXYZW, swRGBA
   end;
end;

{%endregion}

{$IFNDEF USE_ASM}

{%region----[ PASCAL VERSION OF ASSEMBLER ROUTINES BELOW ]----------------------}

{%region%----[ Operators ]------------------------------------------------------}

class operator TBZVector4f.+(constref A, B: TBZVector4f): TBZVector4f;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
  Result.W := A.W + B.W;
end;

class operator TBZVector4f.-(constref A, B: TBZVector4f): TBZVector4f;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
  Result.W := A.W - B.W;
end;

class operator TBZVector4f.*(constref A, B: TBZVector4f): TBZVector4f;
begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
  Result.Z := A.Z * B.Z;
  Result.W := A.W * B.W;
end;

class operator TBZVector4f./(constref A, B: TBZVector4f): TBZVector4f;
begin
  Result.X := A.X / B.X;
  Result.Y := A.Y / B.Y;
  Result.Z := A.Z / B.Z;
  Result.W := A.W / B.W;
end;

class operator TBZVector4f.+(constref A: TBZVector4f; constref B:Single): TBZVector4f;
begin
  Result.X := A.X + B;
  Result.Y := A.Y + B;
  Result.Z := A.Z + B;
  Result.W := A.W + B;
end;

class operator TBZVector4f.-(constref A: TBZVector4f; constref B:Single): TBZVector4f;
begin
  Result.X := A.X - B;
  Result.Y := A.Y - B;
  Result.Z := A.Z - B;
  Result.W := A.W - B;
end;

class operator TBZVector4f.*(constref A: TBZVector4f; constref B:Single): TBZVector4f;
begin
  Result.X := A.X * B;
  Result.Y := A.Y * B;
  Result.Z := A.Z * B;
  Result.W := A.W * B;
end;

class operator TBZVector4f./(constref A: TBZVector4f; constref B:Single): TBZVector4f;
begin
  Result.X := A.X / B;
  Result.Y := A.Y / B;
  Result.Z := A.Z / B;
  Result.W := A.W / B;
end;

class operator TBZVector4f.-(constref A : TBZVector4f): TBZVector4f;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
  Result.Z := -A.Z;
  Result.W := -A.W;
end;

class operator TBZVector4f.=(constref A, B: TBZVector4f): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y) and (A.Z = B.Z) and (A.W = B.W);
end;

class operator TBZVector4f.>=(constref A, B: TBZVector4f): Boolean;
begin
  Result := (A.X >= B.X) and (A.Y >= B.Y) and (A.Z >= B.Z) and (A.W >= B.W);
end;

class operator TBZVector4f.<=(constref A, B: TBZVector4f): Boolean;
begin
  Result := (A.X <= B.X) and (A.Y <= B.Y) and (A.Z <= B.Z) and (A.W <= B.W);
end;

class operator TBZVector4f.>(constref A, B: TBZVector4f): Boolean;
begin
  Result := (A.X > B.X) and (A.Y > B.Y) and (A.Z > B.Z) and (A.W > B.W);
end;

class operator TBZVector4f.<(constref A, B: TBZVector4f): Boolean;
begin
  Result := (A.X < B.X) and (A.Y < B.Y) and (A.Z < B.Z) and (A.W < B.W);
end;

class operator TBZVector4f.<>(constref A, B: TBZVector4f): Boolean;
begin
  Result := (A.X <> B.X) or (A.Y <> B.Y) or (A.Z <> B.Z) or (A.W <> B.W);
end;

class operator TBZVector4f.xor (constref A, B: TBZVector4f) : TBZVector4f;
begin
  result.X := System.Round(A.X) xor System.Round(B.X);
  result.Y := System.Round(A.Y) xor System.Round(B.Y);
  result.Z := System.Round(A.Z) xor System.Round(B.Z);
  result.W := System.Round(A.W) xor System.Round(B.W);
End;

{%endregion%}

{%region%----[ Functions ]------------------------------------------------------}

function TBZVector4f.Abs:TBZVector4f;
begin
  result.X:=System.Abs(Self.X);
  result.Y:=System.Abs(Self.Y);
  result.Z:=System.Abs(Self.Z);
  result.W:=System.Abs(Self.W);
End;

function TBZVector4f.Negate:TBZVector4f;
begin
  Result:=Self;
  Result.X := -Result.X;
  Result.Y := -Result.Y;
  Result.Z := -Result.Z;
  Result.W := -Result.W;
end;

function TBZVector4f.DivideBy2 : TBZVector4f;
begin
  Result.x := Self.X*0.5;
  Result.y := Self.Y*0.5;
  Result.z := Self.Z*0.5;
  Result.w := Self.W*0.5;
end;

function TBZVector4f.Distance(constref A: TBZVector4f):Single;
begin
  result := System.sqrt(sqr(Self.X-A.X)+ sqr(Self.Y-A.Y) + sqr(Self.Z-A.Z));
end;

function TBZVector4f.Length : Single;
//var
//X1 : Single;
begin
  Result := System.Sqrt((Self.X * Self.X) +(Self.Y * Self.Y) +(Self.Z * Self.Z));
  { Ensure result is non-zero to avoid divide-by-zero errors }
  //if X1 = 0 then X1 := 0.0000000001;
  //Result := X1;
end;

function TBZVector4f.DistanceSquare(constref A: TBZVector4f):Single;
begin
  result := sqr(Self.X-A.X)+ sqr(Self.Y-A.Y) + sqr(Self.Z-A.Z);
end;

function TBZVector4f.LengthSquare: Single;
//var
//X1 : Single;
begin
  Result := (Self.X * Self.X) +(Self.Y * Self.Y) +(Self.Z * Self.Z);
  { Ensure result is non-zero to avoid divide-by-zero errors }
  //if X1 = 0 then X1 := 0.0000000001;
  //Result := X1;
end;

function TBZVector4f.Spacing(constref A : TBZVector4f) : Single;
//var v : TBZVector4f;
begin
  Result:=System.Abs(A.X-Self.X)+System.Abs(A.Y-Self.Y)+System.Abs(A.Z-Self.Z)+System.Abs(A.W-Self.W);
end;

function TBZVector4f.DotProduct(constref A: TBZVector4f) : Single;
var
 V2:TBZVector4f;
begin
  V2.X := Self.X*A.X;
  V2.Y := Self.Y*A.Y;
  V2.Z := Self.Z*A.Z;
  //V2.W := Self.W*A.W;
  Result := V2.X + V2.Y + V2.Z;// + V2.W;
end;

function TBZVector4f.CrossProduct(constref A: TBZVector4f): TBZVector4f;
begin
  result.X:=Self.Y*A.Z-Self.Z*A.Y;
  result.Y:=Self.Z*A.X-Self.X*A.Z;
  result.Z:=Self.X*A.Y-Self.Y*A.X;
  result.W:=0;
end;

function TBZVector4f.Norm : Single;
var v2:TBZVector4f;
begin
  v2.x := Self.X*Self.X;
  v2.y := Self.Y*Self.Y;
  v2.z := Self.Z*Self.Z;
//  v2.w := Self.W*Self.W;
  Result := v2.X + v2.Y + v2.Z; //+ v2.w;
end;

function TBZVector4f.Normalize : TBZVector4f;
var
  invLen : Single;
  vn : single;
begin
  vn:=Self.Norm;
  if vn=0 then
  begin
    result := Self;
//    result.W := 0; //cZero;   // either passed in a Vec or point ensure Result is of same type,
  end
  else
  begin
    invLen:=1/System.Sqrt(vn);
    result.X:=Self.X*invLen;
    result.Y:=Self.Y*invLen;
    result.Z:=Self.Z*invLen;
    result.W:=Self.W;
  end;
end;

function TBZVector4f.Min(constref B: Single): TBZVector4f;
begin
  Result := Self;
  if Self.X>B then Result.X := B;
  if Self.Y>B then Result.Y := B;
  if Self.Z>B then Result.Z := B;
  if Self.W>B then Result.W := B;
End;

function TBZVector4f.Min(constref B: TBZVector4f): TBZVector4f;
begin
  Result := Self;
  if Self.X>B.X then Result.X := B.X;
  if Self.Y>B.Y then Result.Y := B.Y;
  if Self.Z>B.Z then Result.Z := B.Z;
  if Self.W>B.W then Result.W := B.W;
End;

function TBZVector4f.Max(constref B: TBZVector4f): TBZVector4f;
begin
  Result := Self;
  if Self.X<B.X then Result.X := B.X;
  if Self.Y<B.Y then Result.Y := B.Y;
  if Self.Z<B.Z then Result.Z := B.Z;
  if Self.W<B.W then Result.W := B.W;
End;

function TBZVector4f.Max(constref B: Single): TBZVector4f;
begin
  Result := Self;
  if Self.X<B then Result.X := B;
  if Self.Y<B then Result.Y := B;
  if Self.Z<B then Result.Z := B;
  if Self.W<B then Result.W := B;
End;

function TBZVector4f.Clamp(Constref AMin, AMax: TBZVector4f): TBZVector4f;
begin
  Result := Self.Max(AMin);
  Result := Result.Min(AMax);
End;

function TBZVector4f.Clamp(constref AMin, AMax: Single): TBZVector4f;
begin
  Result := Self.Max(AMin);
  Result := Result.Min(AMax);
End;

function TBZVector4f.MulAdd(Constref B, C: TBZVector4f): TBZVector4f;
begin
  result := (self*B)+C;
end;

function TBZVector4f.MulSub(Constref B, C: TBZVector4f): TBZVector4f;
begin
  result := (self*B)-C;
end;

function TBZVector4f.MulDiv(Constref B, C: TBZVector4f): TBZVector4f;
begin
  result := (self*B)/C;
end;

function TBZVector4f.Lerp(Constref B: TBZVector4f; Constref T:Single): TBZVector4f;
begin
   Result.X:=Self.X+(B.X-Self.X)*t;
   Result.Y:=Self.Y+(B.Y-Self.Y)*t;
   Result.Z:=Self.Z+(B.Z-Self.Z)*t;
   Result.W := Self.W;
end;

function TBZVector4f.AngleCosine(constref A: TBZVector4f): Single;
begin
   Result:=Self.DotProduct(A)/(Self.Length*A.Length);
end;

function TBZVector4f.AngleBetween(Constref A, ACenterPoint : TBZVector4f): Single;
Var
  vt1,vt2  :  TBZVector4f;
begin
  vt1 := Self - ACenterPoint;
  vt2 := A - ACenterPoint;
  vt1 := vt1.Normalize;
  vt2 := vt2.Normalize;
  Result := ArcCos(vt1.AngleCosine(vt2));
end;

function TBZVector4f.Combine(constref V2: TBZVector4f; constref F1: Single): TBZVector4f;
begin
  Result.X:=Self.X+v2.X*F1;
  Result.Y:=Self.Y+v2.Y*F1;
  Result.Z:=Self.Z+v2.Z*F1;
//  Result.W:=Self.W+v2.W*F1;
  Result.W:=0;
End;

function TBZVector4f.Combine2(constref V2: TBZVector4f; const F1, F2: Single): TBZVector4f;
begin
   Result.X:=( Self.X*F1) + (V2.X*F2);
   Result.Y:=( Self.Y*F1) + (V2.Y*F2);
   Result.Z:=( Self.Z*F1) + (V2.Z*F2);
   Result.W:=0;
end;

function TBZVector4f.Combine3(constref V2, V3: TBZVector4f; const F1, F2, F3: Single): TBZVector4f;
begin
  Result.X:=( Self.X*F1) + (V2.X*F2) + (V3.X*F3);
  Result.Y:=( Self.Y*F1) + (V2.Y*F2) + (V3.Y*F3);
  Result.Z:=( Self.Z*F1) + (V2.Z*F2) + (V3.Z*F3);
  Result.W:=0;
end;

function TBZVector4f.Round:TBZVector4i;
begin
  result.X:=System.Round(Self.X);
  result.Y:=System.Round(Self.Y);
  result.Z:=System.Round(Self.Z);
  result.W:=System.Round(Self.W);
end;

function TBZVector4f.Trunc:TBZVector4i;
begin
  result.X:=System.Trunc(Self.X);
  result.Y:=System.Trunc(Self.Y);
  result.Z:=System.Trunc(Self.Z);
  result.W:=System.Trunc(Self.W);
end;

function TBZVector4f.Floor : TBZVector4i;
begin
 result.X := BZMath.Floor(Self.X);
 result.Y := BZMath.Floor(Self.Y);
 result.Z := BZMath.Floor(Self.Z);
 result.W := BZMath.Floor(Self.W);
end;

function TBZVector4f.Ceil : TBZVector4i;
begin
 result.X := BZMath.Ceil(Self.X);
 result.Y := BZMath.Ceil(Self.Y);
 result.Z := BZMath.Ceil(Self.Z);
 result.W := BZMath.Ceil(Self.W);
end;

function TBZVector4f.Fract : TBZVector4f;
begin
 result.X := BZMath.Fract(Self.X);
 result.Y := BZMath.Fract(Self.Y);
 result.Z := BZMath.Fract(Self.X);
 result.W := BZMath.Fract(Self.Y);
end;

function TBZVector4f.Sqrt:TBZVector4f;
begin
  result.X:=System.Sqrt(Self.X);
  result.Y:=System.Sqrt(Self.Y);
  result.Z:=System.Sqrt(Self.Z);
  result.W:=System.Sqrt(Self.W);
end;

function TBZVector4f.InvSqrt:TBZVector4f;
begin
  result.X:=BZMath.InvSqrt(Self.X);
  result.Y:=BZMath.InvSqrt(Self.Y);
  result.Z:=BZMath.InvSqrt(Self.Z);
  result.W:=BZMath.InvSqrt(Self.W);
end;

{%endregion%}

{%endregion%}

{$ENDIF}
