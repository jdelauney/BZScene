{%region%----[ Pure Pascal will never Convert ]---------------------------------}
// Temp comment this region is where we keep pascal routines for which it does
// not make sense to use SSE/AVX

function TBZColor24.ToBGR : TBZColorBGR_24;
begin
  Result.Blue := Self.Blue;
  Result.Green := Self.Green;
  Result.Red := Self.Red;
End;

function TBZColor32.getColorComponent(Index : Integer) : byte;
begin
  result := v[Index];
End;

procedure TBZColor32.SetColorComponent(Index : Integer; aValue : Byte);
begin
  v[Index] := aValue;
End;

procedure TBZColor32.Create(const aRed, aGreen, aBlue : Byte; const aAlpha : Byte);
begin
(*  {$IFDEF WINDOWS}
  Self.X := aBlue;
  Self.Y := aGreen;
  Self.Z := aRed;
  Self.W := aAlpha;
  {$ELSE}
  Self.X := aRed;
  Self.Y := aGreen;
  Self.Z := aBlue;
  Self.W := aAlpha;
  {$ENDIF} *)
  DWord(Self.AsInteger) := (aRed shl cRedShift) or
                           (aGreen shl cGreenShift) or
                           (aBlue shl cBlueShift) or
                           (aAlpha shl cAlphaShift);
end;

procedure TBZColor32.Create(Const aValue : TBZColor24; const aW : Byte);
begin
  Self.Red := aValue.Red;
  Self.Green := aValue.Green;
  Self.Blue := aValue.Blue;
  Self.Alpha := aW;
end;

procedure TBZColor32.Create(Const aValue : TBZColorBGR_24; const aW : Byte);
begin
  Self.Red := aValue.Red;
  Self.Green := aValue.Green;
  Self.Blue := aValue.Blue;
  Self.Alpha := aW;
end;

procedure TBZColor32.Create(Const aValue : TBZVector3b; const aW : Byte);
begin
  Self.Red := aValue.Red;
  Self.Green := aValue.Green;
  Self.Blue := aValue.Blue;
  Self.Alpha := aW;
end;

function TBZColor32.ToString : String;
begin
  Result := '(Red: '+IntToStr(Self.Red)+
           ' ,Green: '+IntToStr(Self.Green)+
           ' ,Blue: '+IntToStr(Self.Blue)+
           ' ,Alpha: '+IntToStr(Self.Alpha)+')';
end;

function TBZColor32.ToHexString : String;
begin
 Result := '#'+Self.AsInteger.ToHexString;
end;


{%endregion}


{.$IFNDEF USE_ASM}

{%region----[ PASCAL VERSION OF ASSEMBLER ROUTINES BELOW ]----------------------}

{%region%----[ Operators ]------------------------------------------------------}

class operator TBZColor32.+(constref A, B : TBZColor32): TBZColor32;
begin
  Result.Red := ClampByte((A.Red + B.Red));
  Result.Green := ClampByte((A.Green + B.Green));
  Result.Blue := ClampByte((A.Blue + B.Blue));
  //Result.Alpha := ClampByte((A.Alpha + B.Alpha));
  Result.Alpha := A.Alpha;
end;

class operator TBZColor32.-(constref A, B : TBZColor32): TBZColor32;
begin
  Result.Red := ClampByte((A.Red - B.Red));
  Result.Green := ClampByte((A.Green - B.Green));
  Result.Blue := ClampByte((A.Blue - B.Blue));
  Result.Alpha := A.Alpha; //ClampByte((A.Alpha - B.Alpha));
end;

class operator TBZColor32.*(constref A, B : TBZColor32): TBZColor32;
begin
  Result.Red := ClampByte((A.Red * B.Red));
  Result.Green := ClampByte((A.Green * B.Green));
  Result.Blue := ClampByte((A.Blue * B.Blue));
  //Result.Alpha := ClampByte((A.Alpha * B.Alpha));
  Result.Alpha := A.Alpha;
end;

{.$ENDIF}

class operator TBZColor32.div(constref A, B : TBZColor32): TBZColor32;
begin
  Result := A;
  if B.Red<>0 then Result.Red := ClampByte((A.Red div B.Red));
  if B.Green<>0 then Result.Green := ClampByte((A.Green div B.Green));
  if B.Blue<>0 then Result.Blue := ClampByte((A.Blue div B.Blue));
  //if B.Alpha<>0 then Result.Alpha := ClampByte((A.Alpha div B.Alpha));
  Result.Alpha := A.Alpha;
end;

class operator TBZColor32.+(constref A: TBZColor32; constref B: Byte ): TBZColor32;
begin
  Result.Red := ClampByte((A.Red + B));
  Result.Green := ClampByte((A.Green + B));
  Result.Blue := ClampByte((A.Blue + B));
  //Result.Alpha := ClampByte((A.Alpha + B));
  Result.Alpha := A.Alpha;
end;

class operator TBZColor32.-(constref A: TBZColor32; constref B: Byte ): TBZColor32;
begin
  Result.Red := ClampByte((A.Red - B));
  Result.Green := ClampByte((A.Green - B));
  Result.Blue := ClampByte((A.Blue - B));
  //Result.Alpha := ClampByte((A.Alpha - B));
  Result.Alpha := A.Alpha;
end;

class operator TBZColor32.*(constref A: TBZColor32; constref B: Byte ): TBZColor32;
begin
  Result.Red := ClampByte((A.Red * B));
  Result.Green := ClampByte((A.Green * B));
  Result.Blue := ClampByte((A.Blue * B));
  //Result.Alpha := ClampByte((A.Alpha * B));
  Result.Alpha := A.Alpha;
end;

class operator TBZColor32.*(constref A: TBZColor32; constref B: Single): TBZColor32;
begin
  Result.Red := ClampByte(Round((A.Red * B)));
  Result.Green := ClampByte(Round((A.Green * B)));
  Result.Blue := ClampByte(Round((A.Blue * B)));
  //Result.Alpha := ClampByte(Round((A.Alpha * B)));
  Result.Alpha := A.Alpha;
end;

class operator TBZColor32.div(constref A: TBZColor32; constref B: Byte ): TBZColor32;
begin
  Result := A;
  if B<>0 then Result.Red := ClampByte(A.Red div B);
  if B<>0 then Result.Green := ClampByte(A.Green div B);
  if B<>0 then Result.Blue := ClampByte(A.Blue div B);
  //if B<>0 then Result.Alpha := ClampByte(A.Alpha div B);
  Result.Alpha := A.Alpha;
end;

class operator TBZColor32. = (constref A, B : TBZColor32) : Boolean;
begin
  Result := (A.Alpha = B.Alpha) and (A.Red = B.Red) and (A.Green = B.Green) and (A.Blue = B.Blue);
  //Result := A.AsInteger = B.AsInteger;
end;

class operator TBZColor32.<>(constref A, B : TBZColor32) : Boolean;
begin

  Result := (A.Alpha <> B.Alpha) or (A.Red <> B.Red) or (A.Green <> B.Green) or (A.Blue <> B.Blue);

//  Result := A.AsInteger <> B.AsInteger;

end;

class operator TBZColor32.and(constref A, B : TBZColor32) : TBZColor32;
begin
  Result.Red := A.Red and B.Red;
  Result.Green := A.Green and B.Green;
  Result.Blue := A.Blue and B.Blue;
  //Result.Alpha := A.Alpha and B.Alpha;
  Result.Alpha := A.Alpha;
end;

class operator TBZColor32.Or(constref A, B : TBZColor32) : TBZColor32;
begin
  Result.Red := A.Red or B.Red;
  Result.Green := A.Green or B.Green;
  Result.Blue := A.Blue or B.Blue;
  //Result.Alpha := A.Alpha or B.Alpha;
  Result.Alpha := A.Alpha;
end;

class operator TBZColor32.xor(constref A, B : TBZColor32) : TBZColor32;
begin
  Result.Red := A.Red xor B.Red;
  Result.Green := A.Green xor B.Green;
  Result.Blue := A.Blue xor B.Blue;
  //Result.Alpha := A.Alpha xor B.Alpha;
  Result.Alpha := A.Alpha;
end;

class operator TBZColor32.and(constref A : TBZColor32; constref B : Byte) : TBZColor32;
begin
  Result.Red := A.Red and B;
  Result.Green := A.Green and B;
  Result.Blue := A.Blue and B;
  //Result.Alpha := A.Alpha and B;
  Result.Alpha := A.Alpha;
end;

class operator TBZColor32.or(constref A : TBZColor32; constref B : Byte) : TBZColor32;
begin
  Result.Red := A.Red or B;
  Result.Green := A.Green or B;
  Result.Blue := A.Blue or B;
  //Result.Alpha := A.Alpha or B;
  Result.Alpha := A.Alpha;
end;

class operator TBZColor32.xor(constref A : TBZColor32; constref B : Byte) : TBZColor32;
begin
  Result.Red := A.Red xor B;
  Result.Green := A.Green xor B;
  Result.Blue := A.Blue xor B;
  //Result.Alpha := A.Alpha xor B;
  Result.Alpha := A.Alpha;
end;

function TBZColor32.DivideBy2 : TBZColor32;
//var  tmp: integer;
begin
  Result.Red := Round(Self.Red*0.5);
  Result.Green := Round(Self.Green*0.5);
  Result.Blue := Round(Self.Blue*0.5);
  //Result.Alpha := Round(Self.Alpha*0.5);
  Result.Alpha := Self.Alpha;
end;

function TBZColor32.Min(Constref B : TBZColor32) : TBZColor32;
begin
  Result := Self;
  if B.Red < Self.Red then Result.Red := B.Red;
  if B.Green < Self.Green then Result.Green := B.Green;
  if B.Blue < Self.Blue then Result.Blue := B.Blue;
  //if Self.Alpha>B.Alpha then Result.Alpha := B.Alpha;
  Result.Alpha := Self.Alpha;
end;

function TBZColor32.Min(Constref B : Byte) : TBZColor32;
begin
  Result := Self;
  if B < Self.Red then Result.Red := B;
  if B < Self.Green then Result.Green := B;
  if B < Self.Blue then Result.Blue := B;
  //if Self.Alpha>B then Result.Alpha := B;
  Result.Alpha := Self.Alpha;
End;

function TBZColor32.Max(Constref B : TBZColor32) : TBZColor32;
begin
  Result := Self;
  if B.Red > Self.Red then Result.Red := B.Red;
  if B.Green > Self.Green then Result.Green := B.Green;
  if B.Blue > Self.Blue then Result.Blue := B.Blue;
  //if Self.Alpha<B.Alpha then Result.Alpha := B.Alpha;
  Result.Alpha := Self.Alpha;
end;

function TBZColor32.Max(Constref B : Byte) : TBZColor32;
begin
  Result := Self;
  if Self.Red<B then Result.Red := B;
  if Self.Green<B then Result.Green := B;
  if Self.Blue<B then Result.Blue := B;
  //if Self.Alpha<B then Result.Alpha := B;
  Result.Alpha := Self.Alpha;
end;

function TBZColor32.Clamp(Constref AMin, AMax : TBZColor32) : TBZColor32;
begin
  Result := Self.Max(AMin);
  Result := Result.Min(AMax);
end;

function TBZColor32.Clamp(Constref AMin, AMax : Byte) : TBZColor32;
begin
  Result := Self.Max(AMin);
  Result := Result.Min(AMax);
end;

function TBZColor32.MulAdd(Constref B, C : TBZColor32) : TBZColor32;
begin
  result := (self*B)+C;
end;

function TBZColor32.MulDiv(Constref B, C : TBZColor32) : TBZColor32;
var tmp: integer;
begin
  tmp := self.Red * B.Red;
  result.Red := ClampByte(tmp div C.Red);
  tmp := self.Green * B.Green;
  result.Green := ClampByte(tmp div C.Green);
  tmp := self.Blue * B.Blue;
  result.Blue := ClampByte(tmp div C.Blue);
//  tmp := self.Alpha * B.Alpha;
//  result.Alpha := ClampByte(tmp div C.Alpha);
  Result.Alpha := Self.Alpha;
end;

function TBZColor32.Combine(constref V2 : TBZColor32; constref F1 : Single) : TBZColor32;
Var
  v3 : TBZColor32;
begin
  v3 := v2 * F1;
  result := Self + v3;
  result.Alpha := Self.Alpha;
End;

function TBZColor32.Combine2(constref V2 : TBZColor32; const F1, F2 : Single) : TBZColor32;
begin
  result.Red := ClampByte(Round(Self.Red * F1 + V2.Red * F2));
  result.Green := ClampByte(Round(Self.Green * F1 + V2.Green * F2));
  result.Blue := ClampByte(Round(Self.Blue * F1 + V2.Blue * F2));
  result.Alpha := Self.Alpha;
end;

function TBZColor32.Combine3(constref V2, V3 : TBZColor32; const F1, F2, F3 : Single) : TBZColor32;
begin
  result.Red := ClampByte(Round(Self.Red * F1 + V2.Red * F2 + V3.Red * F3));
  result.Green := ClampByte(Round(Self.Green * F1 + V2.Green * F2 + V3.Green * F3));
  result.Blue := ClampByte(Round(Self.Blue * F1 + V2.Blue * F2 + V3.Blue * F3));
  result.Alpha := Self.Alpha;
end;

function TBZColor32.Lerp(Constref A : TBZColor32; const Theta : Single; const Distortion : Single; const LerpType : TBZInterPolationType; const LerpAlpha : Boolean) : TBZColor32;
begin
 Result.Alpha:=Self.Alpha;
 Result.Red:=Round(InterpolateValue(Self.Red,A.Red,Theta,Distortion,LerpType));
 Result.Green:=Round(InterpolateValue(Self.Green,A.Green,Theta,Distortion,LerpType));
 Result.Blue:=Round(InterpolateValue(Self.Blue,A.Blue,Theta,Distortion,LerpType));
 If LerpAlpha then Result.Alpha:=Round(InterpolateValue(Self.Alpha,A.Alpha,Theta,Distortion,LerpType))
 else Result.Alpha:=Self.Alpha;
end;


function TBZColor32.MinXYZComponent : Byte;
begin
  Result:=BZMath.Min(Self.Red, Self.Green, Self.Blue);
end;

function TBZColor32.MaxXYZComponent : Byte;
begin
  Result:=BZMath.Max(Self.Red, Self.Green, Self.Blue);
end;

function TBZColor32.FastSwapRedBlue : Integer;
var
  AIntColor : Cardinal;
begin
  AIntColor := Self.AsInteger;
  Result := AIntColor and $FF00FF00 or (AintColor and $000000FF SHL 16) or (AIntColor and $00FF0000 SHR 16);
end;

function TBZColor32.IsInRange(AMinColor, AMaxColor : TBZColor32) : Boolean;
Begin
 Result := False;
  if ((Self.Red>=AMinColor.Red) and (Self.Red<=AMaxColor.Red)) and
     ((Self.Green>=AMinColor.Green) and (Self.Green<=AMaxColor.Green)) and
     ((Self.Blue>=AMinColor.Blue) and (Self.Blue<=AMaxColor.Blue)) then result := true;
End;

function TBZColor32.Shuffle(const x, y, z, w : Byte) : TBZColor32;
begin
  Result.Red := Self.V[x];
  Result.Green := Self.V[y];
  Result.Blue := Self.V[z];
  Result.Alpha := Self.V[w];
End;

function TBZColor32.Swizzle(const ASwizzle : TBZVector4SwizzleRef) : TBZColor32;
begin
   //FSwizzleMode :=  ASwizzle;
   case ASwizzle of
     swXXXX, swRRRR :
     begin
       Result.Red := Self.Red;
       Result.Green := Self.Red;
       Result.Blue := Self.Red;
       Result.Alpha := Self.Red;
     end;
     swYYYY, swGGGG :
     begin
       Result.Red := Self.Green;
       Result.Green := Self.Green;
       Result.Blue := Self.Green;
       Result.Alpha := Self.Green;
     end;
     swZZZZ, swBBBB :
     begin
       Result.Red := Self.Blue;
       Result.Green := Self.Blue;
       Result.Blue := Self.Blue;
       Result.Alpha := Self.Blue;
     end;
     swWWWW, swAAAA :
     begin
       Result.Red := Self.Alpha;
       Result.Green := Self.Alpha;
       Result.Blue := Self.Alpha;
       Result.Alpha := Self.Alpha;
     end;
     swZYXW, swBGRA :
     begin
       Result.Red := Self.Blue;
       Result.Green := Self.Green;
       Result.Blue := Self.Red;
       Result.Alpha := Self.Alpha;
     end;
     swXZYW, swRBGA :
     begin
       Result.Red := Self.Red;
       Result.Green := Self.Blue;
       Result.Blue := Self.Green;
       Result.Alpha := Self.Alpha;
     end;
     swZXYW, swBRGA :
     begin
       Result.Red := Self.Blue;
       Result.Green := Self.Red;
       Result.Blue := Self.Green;
       Result.Alpha := Self.Alpha;
     end;
     swYXZW, swGRBA :
     begin
       Result.Red := Self.Green;
       Result.Green := Self.Red;
       Result.Blue := Self.Blue;
       Result.Alpha := Self.Alpha;
     end;
     swYZXW, swGBRA :
     begin
       Result.Red := Self.Green;
       Result.Green := Self.Blue;
       Result.Blue := Self.Red;
       Result.Alpha := Self.Alpha;
     end;
     swWXYZ, swARGB :
     begin
       Result.Red := Self.Alpha;
       Result.Green := Self.Red;
       Result.Blue := Self.Green;
       Result.Alpha := Self.Blue;
     end;
     swWZYX, swABGR :
     begin
       Result.Red := Self.Alpha;
       Result.Green := Self.Blue;
       Result.Blue := Self.Green;
       Result.Alpha := Self.Red;
     end;
     swWXZY, swARBG :
     begin
       Result.Red := Self.Alpha;
       Result.Green := Self.Red;
       Result.Blue := Self.Blue;
       Result.Alpha := Self.Green;
     end;
     swWZXY, swABRG :
     begin
       Result.Red := Self.Alpha;
       Result.Green := Self.Blue;
       Result.Blue := Self.Red;
       Result.Alpha := Self.Green;
     end;
     swWYXZ, swAGRB :
     begin
       Result.Red := Self.Alpha;
       Result.Green := Self.Green;
       Result.Blue := Self.Red;
       Result.Alpha := Self.Blue;
     end;
     swWYZX, swAGBR :
     begin
       Result.Red := Self.Alpha;
       Result.Green := Self.Green;
       Result.Blue := Self.Blue;
       Result.Alpha := Self.Red;
     end;

     else result := self; //swXYZW, swRGBA
   end;
end;

{%endregion%}

{%region%----[ Functions ]------------------------------------------------------}

{%endregion%}

{%region%----[ Pure Pascal may Convert at some point ]--------------------------}

{%endregion%}

{%endregion%}

{.$ENDIF}
