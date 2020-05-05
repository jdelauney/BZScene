{%region%----[ Pure Pascal will never Convert ]---------------------------------}
// Temp comment this region is where we keep pascal routines for which it does
// not make sense to use SSE/AVX

procedure TBZVector3b.Create(const aX, aY, aZ: Byte);
begin
  Self.X := aX;
  Self.Y := aY;
  Self.Z := aZ;
end;

function TBZVector3b.ToString: String;
begin
  Result := '(X: '+IntToStr(Self.X)+
           ' ,Y: '+IntToStr(Self.Y)+
           ' ,Z: '+IntToStr(Self.Z)+')';
end;


class operator TBZVector3b.+(constref A, B: TBZVector3b): TBZVector3b;
begin
  Result.X := ClampByte((A.X + B.X));
  Result.Y := ClampByte((A.Y + B.Y));
  Result.Z := ClampByte((A.Z + B.Z));
end;

class operator TBZVector3b.-(constref A, B: TBZVector3b): TBZVector3b;
begin
  Result.X := ClampByte((A.X - B.X));
  Result.Y := ClampByte((A.Y - B.Y));
  Result.Z := ClampByte((A.Z - B.Z));
end;

class operator TBZVector3b.*(constref A, B: TBZVector3b): TBZVector3b;
begin
  Result.X := ClampByte((A.X * B.X));
  Result.Y := ClampByte((A.Y * B.Y));
  Result.Z := ClampByte((A.Z * B.Z));
end;

class operator TBZVector3b.div(constref A, B: TBZVector3b): TBZVector3b;
begin
  Result.X := ClampByte((A.X div B.X));
  Result.Y := ClampByte((A.Y div B.Y));
  Result.Z := ClampByte((A.Z div B.Z));
end;

class operator TBZVector3b.+(constref A: TBZVector3b; constref B: Byte ): TBZVector3b;
begin
  Result.X := ClampByte((A.X + B));
  Result.Y := ClampByte((A.Y + B));
  Result.Z := ClampByte((A.Z + B));
end;

class operator TBZVector3b.-(constref A: TBZVector3b; constref B: Byte ): TBZVector3b;
begin
  Result.X := ClampByte((A.X - B));
  Result.Y := ClampByte((A.Y - B));
  Result.Z := ClampByte((A.Z - B));
end;

class operator TBZVector3b.*(constref A: TBZVector3b; constref B: Byte ): TBZVector3b;
begin
  Result.X := ClampByte((A.X * B));
  Result.Y := ClampByte((A.Y * B));
  Result.Z := ClampByte((A.Z * B));
end;

class operator TBZVector3b.*(constref A: TBZVector3b; constref B: Single): TBZVector3b;
begin
  Result.X := ClampByte(Round((A.X * B)));
  Result.Y := ClampByte(Round((A.Y * B)));
  Result.Z := ClampByte(Round((A.Z * B)));
end;

class operator TBZVector3b.div(constref A: TBZVector3b; constref B: Byte ): TBZVector3b;
begin
  Result.X := ClampByte((A.X div B));
  Result.Y := ClampByte((A.Y div B));
  Result.Z := ClampByte((A.Z div B));
end;

class operator TBZVector3b.=(constref A, B: TBZVector3b): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y) and (A.Z = B.Z);
end;

class operator TBZVector3b.<>(constref A, B: TBZVector3b): Boolean;
begin
  Result := (A.X <> B.X) or (A.Y <> B.Y) or (A.Z <> B.Z);
end;

class operator TBZVector3b.and(constref A, B: TBZVector3b): TBZVector3b;
begin
  Result.X := A.X and B.X;
  Result.Y := A.Y and B.Y;
  Result.Z := A.Z and B.Z;
end;

class operator TBZVector3b.Or(constref A, B: TBZVector3b): TBZVector3b;
begin
  Result.X := A.X or B.X;
  Result.Y := A.Y or B.Y;
  Result.Z := A.Z or B.Z;
end;

class operator TBZVector3b.Xor(constref A, B: TBZVector3b): TBZVector3b;
begin
  Result.X := A.X xor B.X;
  Result.Y := A.Y xor B.Y;
  Result.Z := A.Z xor B.Z;
end;

class operator TBZVector3b.and(constref A: TBZVector3b; constref B: Byte): TBZVector3b;
begin
  Result.X := A.X and B;
  Result.Y := A.Y and B;
  Result.Z := A.Z and B;
end;

class operator TBZVector3b.or(constref A: TBZVector3b; constref B: Byte): TBZVector3b;
begin
  Result.X := A.X or B;
  Result.Y := A.Y or B;
  Result.Z := A.Z or B;
end;

class operator TBZVector3b.Xor(constref A: TBZVector3b; constref B: Byte): TBZVector3b;
begin
  Result.X := A.X xor B;
  Result.Y := A.Y xor B;
  Result.Z := A.Z xor B;
end;

(* function TBZVector3b.AsVector3f: TBZVector3f;
begin
  Result.X := Self.X * cColorFloatRatio;
  Result.X := Self.Y * cColorFloatRatio;
  Result.X := Self.Z * cColorFloatRatio;
end;  *)

function TBZVector3b.Swizzle(const ASwizzle: TBZVector3SwizzleRef ): TBZVector3b;
begin
   //FSwizzleMode := ASwizzle;
   case ASwizzle of
     swXXX, swRRR :
     begin
       Result.X := Self.X;
       Result.Y := Self.X;
       Result.Z := Self.X;
     end;
     swYYY, swGGG :
     begin
       Result.X := Self.Y;
       Result.Y := Self.Y;
       Result.Z := Self.Y;
     end;
     swZZZ, swBBB :
     begin
       Result.X := Self.Z;
       Result.Y := Self.Z;
       Result.Z := Self.Z;
     end;
     swXZY, swRBG :
     begin
       Result.X := Self.X;
       Result.Y := Self.Z;
       Result.Z := Self.Y;
     end;
     swZYX, swBGR :
     begin
       Result.X := Self.Z;
       Result.Y := Self.Y;
       Result.Z := Self.X;
     end;
     swZXY, swBRG :
     begin
       Result.X := Self.Z;
       Result.Y := Self.X;
       Result.Z := Self.Y;
     end;
     swYXZ, swGRB :
     begin
       Result.X := Self.Y;
       Result.Y := Self.X;
       Result.Z := Self.Z;
     end;
     swYZX, swGBR :
     begin
       Result.X := Self.Y;
       Result.Y := Self.Z;
       Result.Z := Self.X;
     end;

     else result := self;
   end;
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
