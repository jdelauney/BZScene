{%region%----[ Pure Pascal will never Convert ]---------------------------------}
// Temp comment this region is where we keep pascal routines for which it does
// not make sense to use SSE/AVX

procedure TBZVector4b.Create(const aX, aY, aZ : Byte; const aW : Byte);
begin
  Self.X := aX;
  Self.Y := aY;
  Self.Z := aZ;
  Self.w := aW;
end;

procedure TBZVector4b.Create(const aValue : TBZVector3b; const aW : Byte);
begin
  Self.X := aValue.X;
  Self.Y := aValue.Y;
  Self.Z := aValue.Z;
  Self.W := aW;
end;

function TBZVector4b.ToString: String;
begin
  Result := '(X: '+IntToStr(Self.X)+
           ' ,Y: '+IntToStr(Self.Y)+
           ' ,Z: '+IntToStr(Self.Z)+
           ' ,W: '+IntToStr(Self.W)+')';
end;

class operator TBZVector4b.+(constref A, B : TBZVector4b): TBZVector4b;
begin
  Result.X := ClampByte((A.X + B.X));
  Result.Y := ClampByte((A.Y + B.Y));
  Result.Z := ClampByte((A.Z + B.Z));
  Result.W := ClampByte((A.W + B.W));
end;

class operator TBZVector4b.-(constref A, B : TBZVector4b): TBZVector4b;
begin
  Result.X := ClampByte((A.X - B.X));
  Result.Y := ClampByte((A.Y - B.Y));
  Result.Z := ClampByte((A.Z - B.Z));
  Result.W := ClampByte((A.W - B.W));
end;

class operator TBZVector4b.*(constref A, B : TBZVector4b): TBZVector4b;
begin
  Result.X := ClampByte((A.X * B.X));
  Result.Y := ClampByte((A.Y * B.Y));
  Result.Z := ClampByte((A.Z * B.Z));
  Result.W := ClampByte((A.W * B.W));
end;

class operator TBZVector4b.div(constref A, B : TBZVector4b): TBZVector4b;
begin
  Result := A;
  if B.X<>0 then Result.X := ClampByte((A.X div B.X));
  if B.Y<>0 then Result.Y := ClampByte((A.Y div B.Y));
  if B.Z<>0 then Result.Z := ClampByte((A.Z div B.Z));
  if B.W<>0 then Result.W := ClampByte((A.W div B.W));
end;

class operator TBZVector4b.+(constref A: TBZVector4b; constref B: Byte ): TBZVector4b;
begin
  Result.X := ClampByte((A.X + B));
  Result.Y := ClampByte((A.Y + B));
  Result.Z := ClampByte((A.Z + B));
  Result.W := ClampByte((A.W + B));
end;

class operator TBZVector4b.-(constref A: TBZVector4b; constref B: Byte ): TBZVector4b;
begin
  Result.X := ClampByte((A.X - B));
  Result.Y := ClampByte((A.Y - B));
  Result.Z := ClampByte((A.Z - B));
  Result.W := ClampByte((A.W - B));
end;

class operator TBZVector4b.*(constref A: TBZVector4b; constref B: Byte ): TBZVector4b;
begin
  Result.X := ClampByte((A.X * B));
  Result.Y := ClampByte((A.Y * B));
  Result.Z := ClampByte((A.Z * B));
  Result.W := ClampByte((A.W * B));
end;

class operator TBZVector4b.*(constref A: TBZVector4b; constref B: Single): TBZVector4b;
begin
  Result.X := ClampByte(Round((A.X * B)));
  Result.Y := ClampByte(Round((A.Y * B)));
  Result.Z := ClampByte(Round((A.Z * B)));
  Result.W := ClampByte(Round((A.W * B)));
end;

class operator TBZVector4b.div(constref A: TBZVector4b; constref B: Byte ): TBZVector4b;
begin
  Result := A;
  if B<>0 then Result.X := ClampByte(A.X div B);
  if B<>0 then Result.Y := ClampByte(A.Y div B);
  if B<>0 then Result.Z := ClampByte(A.Z div B);
  if B<>0 then Result.W := ClampByte(A.W div B);
end;

class operator TBZVector4b.=(constref A, B: TBZVector4b): Boolean;
begin
{$ifdef TEST}
  Result := (A.W = B.W) and (A.X = B.X) and (A.Y = B.Y) and (A.Z = B.Z);
{$else}
  Result := A.AsInteger = B.AsInteger;
{$endif}
end;

class operator TBZVector4b.<>(constref A, B: TBZVector4b): Boolean;
begin
{$ifdef TEST}
  Result := (A.W <> B.W) or (A.X <> B.X) or (A.Y <> B.Y) or (A.Z <> B.Z);
{$else}
  Result := A.AsInteger <> B.AsInteger;
{$endif}
end;

class operator TBZVector4b.and(constref A, B : TBZVector4b) : TBZVector4b;
begin
{$ifdef TEST}
  Result.X := A.X and B.X;
  Result.Y := A.Y and B.Y;
  Result.Z := A.Z and B.Z;
  Result.W := A.W and B.W;
{$else}
  Result.AsInteger := A.AsInteger and B.AsInteger;
{$endif}
end;

class operator TBZVector4b.Or(constref A, B : TBZVector4b) : TBZVector4b;
begin
{$ifdef TEST}
  Result.X := A.X or B.X;
  Result.Y := A.Y or B.Y;
  Result.Z := A.Z or B.Z;
  Result.W := A.W or B.W;
{$else}
    Result.AsInteger := A.AsInteger or B.AsInteger;
{$endif}
end;

class operator TBZVector4b.Xor(constref A, B : TBZVector4b) : TBZVector4b;
begin
{$ifdef TEST}
  Result.X := A.X xor B.X;
  Result.Y := A.Y xor B.Y;
  Result.Z := A.Z xor B.Z;
  Result.W := A.W xor B.W;
{$else}
  Result.AsInteger := A.AsInteger xor B.AsInteger;
{$endif}
end;

class operator TBZVector4b.and(constref A : TBZVector4b; constref B : Byte) : TBZVector4b;
var  tmp: integer;
begin
{$ifdef TEST}
  Result.X := A.X and B;
  Result.Y := A.Y and B;
  Result.Z := A.Z and B;
  Result.W := A.W and B;
{$else}
  tmp := (B shl 8) or B;
  tmp := (tmp shl 16) or tmp;
  Result.AsInteger := A.AsInteger and tmp;
{$endif}
end;

class operator TBZVector4b.or(constref A : TBZVector4b; constref B : Byte) : TBZVector4b;
var  tmp: integer;
begin
{$ifdef TEST}
  Result.X := A.X or B;
  Result.Y := A.Y or B;
  Result.Z := A.Z or B;
  Result.W := A.W or B;
{$else}
  tmp := (B shl 8) or B;
  tmp := (tmp shl 16) or tmp;
  Result.AsInteger := A.AsInteger or tmp;
{$endif}
end;

class operator TBZVector4b.Xor(constref A : TBZVector4b; constref B : Byte) : TBZVector4b;
var  tmp: integer;
begin
{$ifdef TEST}
  Result.X := A.X xor B;
  Result.Y := A.Y xor B;
  Result.Z := A.Z xor B;
  Result.W := A.W xor B;
{$else}
  tmp := (B shl 8) or B;
  tmp := (tmp shl 16) or tmp;
  Result.AsInteger := A.AsInteger xor tmp;
{$endif}
end;

function TBZVector4b.DivideBy2 : TBZVector4b;
//var  tmp: integer;
begin
  Result.x := Round(Self.X*0.5);
  Result.y := Round(Self.Y*0.5);
  Result.z := Round(Self.Z*0.5);
  Result.w := Round(Self.W*0.5);
  //tmp := Self.AsInteger;
  //Result.AsInteger :=  (tmp and $FEFEFEFE) shr 1;
end;

function TBZVector4b.Min(Constref B : TBZVector4b) : TBZVector4b;
begin
  Result := Self;
  if Self.X>B.X then Result.X := B.X;
  if Self.Y>B.Y then Result.Y := B.Y;
  if Self.Z>B.Z then Result.Z := B.Z;
  if Self.W>B.W then Result.W := B.W;
end;

function TBZVector4b.Min(Constref B : Byte) : TBZVector4b;
begin
  Result := Self;
  if Self.X>B then Result.X := B;
  if Self.Y>B then Result.Y := B;
  if Self.Z>B then Result.Z := B;
  if Self.W>B then Result.W := B;
End;

function TBZVector4b.Max(Constref B : TBZVector4b) : TBZVector4b;
begin
  Result := Self;
  if Self.X<B.X then Result.X := B.X;
  if Self.Y<B.Y then Result.Y := B.Y;
  if Self.Z<B.Z then Result.Z := B.Z;
  if Self.W<B.W then Result.W := B.W;
end;

function TBZVector4b.Max(Constref B : Byte) : TBZVector4b;
begin
  Result := Self;
  if Self.X<B then Result.X := B;
  if Self.Y<B then Result.Y := B;
  if Self.Z<B then Result.Z := B;
  if Self.W<B then Result.W := B;
end;

function TBZVector4b.Clamp(Constref AMin, AMax : TBZVector4b) : TBZVector4b;
begin
  Result := Self.Max(AMin);
  Result := Result.Min(AMax);
end;

function TBZVector4b.Clamp(Constref AMin, AMax : Byte) : TBZVector4b;
begin
  Result := Self.Max(AMin);
  Result := Result.Min(AMax);
end;

function TBZVector4b.MulAdd(Constref B, C : TBZVector4b) : TBZVector4b;
begin
  result := (self*B)+C;
end;

function TBZVector4b.MulDiv(Constref B, C : TBZVector4b) : TBZVector4b;
var tmp: integer;
begin
  tmp := self.x * B.X;
  result.x := ClampByte(tmp div C.X);
  tmp := self.y * B.Y;
  result.y := ClampByte(tmp div C.Y);
  tmp := self.z * B.Z;
  result.z := ClampByte(tmp div C.Z);
  tmp := self.w * B.W;
  result.w := ClampByte(tmp div C.W);
end;

function TBZVector4b.Shuffle(const x,y,z,w : Byte):TBZVector4b;
begin
  Result.X := Self.V[x];
  Result.Y := Self.V[y];
  Result.Z := Self.V[z];
  Result.W := Self.V[w];
End;

function TBZVector4b.Swizzle(const ASwizzle: TBZVector4SwizzleRef ): TBZVector4b;
begin
   //FSwizzleMode :=  ASwizzle;
   case ASwizzle of
     swXXXX, swRRRR :
     begin
       Result.X := Self.X;
       Result.Y := Self.X;
       Result.Z := Self.X;
       Result.W := Self.X;
     end;
     swYYYY, swGGGG :
     begin
       Result.X := Self.Y;
       Result.Y := Self.Y;
       Result.Z := Self.Y;
       Result.W := Self.Y;
     end;
     swZZZZ, swBBBB :
     begin
       Result.X := Self.Z;
       Result.Y := Self.Z;
       Result.Z := Self.Z;
       Result.W := Self.Z;
     end;
     swWWWW, swAAAA :
     begin
       Result.X := Self.W;
       Result.Y := Self.W;
       Result.Z := Self.W;
       Result.W := Self.W;
     end;
     swZYXW, swBGRA :
     begin
       Result.X := Self.Z;
       Result.Y := Self.Y;
       Result.Z := Self.X;
       Result.W := Self.W;
     end;
     swXZYW, swRBGA :
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

function TBZVector4b.Combine(constref V2: TBZVector4b; constref F1: Single): TBZVector4b;
Var
  v3 : TBZVector4b;
begin
  v3 := v2 * F1;
  result := Self + v3;
  result.W := Self.W;
End;

function TBZVector4b.Combine2(constref V2: TBZVector4b; const F1, F2: Single): TBZVector4b;
begin
  result.X := ClampByte(Round(Self.X * F1 + V2.X * F2));
  result.Y := ClampByte(Round(Self.Y * F1 + V2.Y * F2));
  result.Z := ClampByte(Round(Self.Z * F1 + V2.Z * F2));
  result.W := Self.W;
end;

function TBZVector4b.Combine3(constref V2, V3: TBZVector4b; const F1, F2, F3: Single): TBZVector4b;
begin
  result.X := ClampByte(Round(Self.X * F1 + V2.X * F2 + V3.X * F3));
  result.Y := ClampByte(Round(Self.Y * F1 + V2.Y * F2 + V3.Y * F3));
  result.Z := ClampByte(Round(Self.Z * F1 + V2.Z * F2 + V3.Z * F3));
  result.W := Self.W;
end;

function TBZVector4b.MinXYZComponent : Byte;
begin
  Result:=BZMath.Min(Self.X, Self.Y, Self.Z);
end;

function TBZVector4b.MaxXYZComponent : Byte;
begin
  Result:=BZMath.Max(Self.X, Self.Y, Self.Z);
end;

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
