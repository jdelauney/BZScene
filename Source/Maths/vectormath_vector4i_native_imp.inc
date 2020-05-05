{%region%----[ Pure Pascal will never Convert ]---------------------------------}
// Temp comment this region is where we keep pascal routines for which it does
// not make sense to use SSE/AVX

procedure TBZVector4i.Create(const aX, aY, aZ : Longint; const aW : Longint);
begin
  Self.X := aX;
  Self.Y := aY;
  Self.Z := aZ;
  Self.W := aW;
end;

procedure TBZVector4i.Create(const aValue : TBZVector3i; const aW : Longint);
begin
  Self.X := aValue.X;
  Self.Y := aValue.Y;
  Self.Z := aValue.Z;
  Self.W := aW;
end;

procedure TBZVector4i.Create(const aValue : TBZVector3b; const aW : Longint);
begin
  Self.X := aValue.X;
  Self.Y := aValue.Y;
  Self.Z := aValue.Z;
  Self.W := aW;
end;

function TBZVector4i.ToString : String;
begin
  Result := '(X: '+IntToStr(Self.X)+
           ' ,Y: '+IntToStr(Self.Y)+
           ' ,Z: '+IntToStr(Self.Z)+
           ' ,W: '+IntToStr(Self.W)+')';
end;

{%endregion}

{%region%----[ Pure Pascal may Convert at some point ]--------------------------}

function TBZVector4i.Shuffle(const x, y, z, w : Byte) : TBZVector4i;
begin
  Result.X:=Self.V[x];
  Result.Y:=Self.V[y];
  Result.Z:=Self.V[z];
  Result.W:=Self.V[w];
end;

function TBZVector4i.Swizzle(const ASwizzle : TBZVector4SwizzleRef) : TBZVector4i;
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

function TBZVector4i.MinXYZComponent : LongInt;
begin
  Result:=BZMath.Min(Self.X, Self.Y, Self.Z);
end;

function TBZVector4i.MaxXYZComponent : LongInt;
begin
  Result:=BZMath.Max(Self.X, Self.Y, Self.Z);
end;

{%endregion}

{$IFNDEF USE_ASM}

{%region----[ PASCAL VERSION OF ASSEMBLER ROUTINES BELOW ]----------------------}

{%region%----[ Operators ]------------------------------------------------------}

class operator TBZVector4i.+ (constref A, B : TBZVector4i) : TBZVector4i;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
  Result.W := A.W + B.W;
end;

class operator TBZVector4i.- (constref A, B : TBZVector4i) : TBZVector4i;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
  Result.W := A.W - B.W;
end;

class operator TBZVector4i.*(constref A, B : TBZVector4i) : TBZVector4i;
begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
  Result.Z := A.Z * B.Z;
  Result.W := A.W * B.W;
end;

class operator TBZVector4i.div(constref A, B : TBZVector4i) : TBZVector4i;
begin
  Result.X := A.X div B.X;
  Result.Y := A.Y div B.Y;
  Result.Z := A.Z div B.Z;
  Result.W := A.W div B.W;
end;

class operator TBZVector4i. + (constref A : TBZVector4i; constref B : Longint) : TBZVector4i;
begin
  Result.X := A.X + B;
  Result.Y := A.Y + B;
  Result.Z := A.Z + B;
  Result.W := A.W + B;
end;

class operator TBZVector4i. - (constref A : TBZVector4i; constref B : Longint) : TBZVector4i;
begin
  Result.X := A.X - B;
  Result.Y := A.Y - B;
  Result.Z := A.Z - B;
  Result.W := A.W - B;
end;

class operator TBZVector4i. * (constref A : TBZVector4i; constref B : Longint) : TBZVector4i;
begin
  Result.X := A.X * B;
  Result.Y := A.Y * B;
  Result.Z := A.Z * B;
  Result.W := A.W * B;
end;

class operator TBZVector4i. * (constref A : TBZVector4i; constref B : Single) : TBZVector4i;
begin
  Result.X := Round(A.X * B);
  Result.Y := Round(A.Y * B);
  Result.Z := Round(A.Z * B);
  Result.W := Round(A.W * B);
end;

class operator TBZVector4i.div(constref A : TBZVector4i; constref B : Longint) : TBZVector4i;
begin
  Result.X := A.X div B;
  Result.Y := A.Y div B;
  Result.Z := A.Z div B;
  Result.W := A.W div B;
end;

class operator TBZVector4i.= (constref A, B : TBZVector4i) : Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y) and (A.Z = B.Z) and (A.W = B.W);
end;

class operator TBZVector4i.<> (constref A, B : TBZVector4i) : Boolean;
begin
  Result := (A.X <> B.X) or (A.Y <> B.Y) or (A.Z <> B.Z) or (A.W <> B.W);
end;

class operator TBZVector4i.-(constref A : TBZVector4i) : TBZVector4i;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
  Result.Z := -A.Z;
  Result.W := -A.W;
end;

(* class operator TBZVector4i.and(constref A, B : TBZVector4i) : TBZVector4i;
begin
  Result.X := ClampByte((A.X and B.X));
  Result.Y := ClampByte((A.Y and B.Y));
  Result.Z := ClampByte((A.Z and B.Z));
  Result.W := ClampByte((A.W and B.W));
end;

class operator TBZVector4i.Or(constref A, B : TBZVector4i) : TBZVector4i;
begin
  Result.X := ClampByte((A.X or B.X));
  Result.Y := ClampByte((A.Y or B.Y));
  Result.Z := ClampByte((A.Z or B.Z));
  Result.W := ClampByte((A.W or B.W));
end;

class operator TBZVector4i.Xor(constref A, B : TBZVector4i) : TBZVector4i;
begin
  Result.X := ClampByte((A.X xor B.X));
  Result.Y := ClampByte((A.Y xor B.Y));
  Result.Z := ClampByte((A.Z xor B.Z));
  Result.W := ClampByte((A.W xor B.W));
end;

class operator TBZVector4i.and(constref A : TBZVector4i; constref B : LongInt) : TBZVector4i;
begin
  Result.X := ClampByte((A.X and B));
  Result.Y := ClampByte((A.Y and B));
  Result.Z := ClampByte((A.Z and B));
  Result.W := ClampByte((A.W and B));
end;

class operator TBZVector4i.or(constref A : TBZVector4i; constref B : LongInt) : TBZVector4i;
begin
  Result.X := ClampByte((A.X or B));
  Result.Y := ClampByte((A.Y or B));
  Result.Z := ClampByte((A.Z or B));
  Result.W := ClampByte((A.W or B));
end;

class operator TBZVector4i.Xor(constref A : TBZVector4i; constref B : LongInt) : TBZVector4i;
begin
  Result.X := ClampByte((A.X xor B));
  Result.Y := ClampByte((A.Y xor B));
  Result.Z := ClampByte((A.Z xor B));
  Result.W := ClampByte((A.W xor B));
end;       *)

function TBZVector4i.DivideBy2 : TBZVector4i;
begin
  Result.X := (Self.X shr 1);
  Result.Y := (Self.Y shr 1);
  Result.Z := (Self.Z shr 1);
  Result.W := (Self.W shr 1);
end;

function TBZVector4i.abs : TBZVector4i;
begin
  Result.X := system.Abs(Self.X);
  Result.Y := system.Abs(Self.Y);
  Result.Z := system.Abs(Self.Z);
  Result.W := system.Abs(Self.W);
end;

function TBZVector4i.Min(Constref B : TBZVector4i) : TBZVector4i;
begin
  Result := Self;
  if Self.X>B.X then Result.X := B.X;
  if Self.Y>B.Y then Result.Y := B.Y;
  if Self.Z>B.Z then Result.Z := B.Z;
  if Self.W>B.W then Result.W := B.W;
end;

function TBZVector4i.Min(Constref B : LongInt) : TBZVector4i;
begin
  Result := Self;
  if Self.X>B then Result.X := B;
  if Self.Y>B then Result.Y := B;
  if Self.Z>B then Result.Z := B;
  if Self.W>B then Result.W := B;
end;

function TBZVector4i.Max(Constref B : TBZVector4i) : TBZVector4i;
begin
  Result := Self;
  if Self.X<B.X then Result.X := B.X;
  if Self.Y<B.Y then Result.Y := B.Y;
  if Self.Z<B.Z then Result.Z := B.Z;
  if Self.W<B.W then Result.W := B.W;
end;

function TBZVector4i.Max(Constref B : LongInt) : TBZVector4i;
begin
  Result := Self;
  if Self.X<B then Result.X := B;
  if Self.Y<B then Result.Y := B;
  if Self.Z<B then Result.Z := B;
  if Self.W<B then Result.W := B;
end;

function TBZVector4i.Clamp(Constref AMin, AMax : TBZVector4i) : TBZVector4i;
begin
  Result := Self.Max(AMin);
  Result := Result.Min(AMax);
end;

function TBZVector4i.Clamp(Constref AMin, AMax : LongInt) : TBZVector4i;
begin
  Result := Self.Max(AMin);
  Result := Result.Min(AMax);
end;

function TBZVector4i.MulAdd(Constref B, C : TBZVector4i) : TBZVector4i;
begin
  result := (self*B)+C;
end;

function TBZVector4i.MulDiv(Constref B, C : TBZVector4i) : TBZVector4i;
begin
  result := (self*B) div C;
end;

function TBZVector4i.Combine(constref V2 : TBZVector4i; constref F1 : Single) : TBZVector4i;
Var
  v3 : TBZVector4i;
begin
  v3 := v2 * F1;
  result := Self + v3;
  result.W := Self.W;
end;  

function TBZVector4i.Combine2(constref V2: TBZVector4i; const F1, F2: Single): TBZVector4i;
begin
  result.X := Round(Self.X * F1 + V2.X * F2);
  result.Y := Round(Self.Y * F1 + V2.Y * F2);
  result.Z := Round(Self.Z * F1 + V2.Z * F2);
  result.W := Self.W;
end;

function TBZVector4i.Combine3(constref V2, V3: TBZVector4i; const F1, F2, F3: Single): TBZVector4i;
begin
  result.X := Round(Self.X * F1 + V2.X * F2 + V3.X * F3);
  result.Y := Round(Self.Y * F1 + V2.Y * F2 + V3.Y * F3);
  result.Z := Round(Self.Z * F1 + V2.Z * F2 + V3.Z * F3);
  result.W := Self.W;
end;
{%endregion%}

{%region%----[ Functions ]------------------------------------------------------}

{%endregion%}

{%endregion%}

{$ENDIF}
