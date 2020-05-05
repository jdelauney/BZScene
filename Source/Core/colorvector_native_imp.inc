{%region%----[ Pure Pascal will never Convert ]---------------------------------}
// Temp comment this region is where we keep pascal routines for which it does
// not make sense to use SSE/AVX


procedure TBZColorVector.Create(const AValue : single; const AlphaValue : Single);
begin
  Self.X := AValue;
  Self.Y := AValue;
  Self.Z := AValue;
  Self.W := AlphaValue;
end;

procedure TBZColorVector.Create(const aX, aY, aZ : single; const aW : Single);
begin
   Self.X := AX;
   Self.Y := AY;
   Self.Z := AZ;
   Self.W := AW;
end;

procedure TBZColorVector.Create(const aValue : TBZColor32);
begin
   Self.X := aValue.Red * _FloatColorRatio;
   Self.Y := aValue.Green * _FloatColorRatio;
   Self.Z := aValue.Blue * _FloatColorRatio;
   Self.W := aValue.Alpha * _FloatColorRatio;
end;

procedure TBZColorVector.Create(const aValue : TBZVector);
begin
   Self.X := aValue.Red;
   Self.Y := aValue.Green;
   Self.Z := aValue.Blue;
   Self.W := aValue.Alpha;
end;

function TBZColorVector.ToString : String;
begin
   Result := '(Red   : '+FloattoStrF(Self.X,fffixed,5,5)+
            ' ,Green : '+FloattoStrF(Self.Y,fffixed,5,5)+
            ' ,Blue  : '+FloattoStrF(Self.Z,fffixed,5,5)+
            ' ,Alpha : '+FloattoStrF(Self.W,fffixed,5,5)+')';
End;

function TBZColorVector.Shuffle(const x,y,z,w : Byte):TBZColorVector;
begin
  Result.X:=Self.V[x];
  Result.Y:=Self.V[y];
  Result.Z:=Self.V[z];
  Result.W:=Self.V[w];
End;

function TBZColorVector.MinXYZComponent : Single;
begin
   Result:=BZMath.Min(Self.X, Self.Y, Self.Z);
end;

function TBZColorVector.MaxXYZComponent : Single;
begin
   Result:=BZMath.Max(Self.X, Self.Y, Self.Z);
end;

function TBZColorVector.ConvertColorSpace(ColorMatrixConversion : TBZColorSpaceMatrixConversion):TBZColorVector;
Var
  CoefX, CoefY, CoefZ : TBZColorVector;
begin
  CoefX.Create(ColorMatrixConversion[0],ColorMatrixConversion[1],ColorMatrixConversion[2]);
  CoefY.Create(ColorMatrixConversion[3],ColorMatrixConversion[4],ColorMatrixConversion[5]);
  CoefZ.Create(ColorMatrixConversion[6],ColorMatrixConversion[7],ColorMatrixConversion[8]);

  Result := (CoefX * Self) + (CoefY * Self) + (CoefZ * Self);
end;

//function TBZColorVector.ApplyColorMatrix(ColorMatrix : TBZColorMatrix):TBZColorVector;
//Var
//  {$CODEALIGN VARMIN=16}
//  CoefX, CoefY, CoefZ, CoefW, vOffset : TBZColorVector;
//  //vWhite
//  {$CODEALIGN VARMIN=4}
//begin
//
//
//// Sepia
////FMatrix[0] := (0.272 + 0.728 * Coef); FMatrix[5] := (0.349 - 0.349 * Coef); FMatrix[10] := (0.393 - 0.393 * Coef); FMatrix[15] := 1.0;
////FMatrix[1] := (0.534 - 0.534 * Coef); FMatrix[6] := (0.686 + 0.314 * Coef); FMatrix[11] := (0.769 - 0.769 * Coef); FMatrix[16] := 1.0;
////FMatrix[2] := (0.131 - 0.131 * Coef); FMatrix[7] := (0.168 - 0.168 * Coef); FMatrix[12] := (0.189 + 0.811 * Coef); FMatrix[17] := 1.0;
////FMatrix[3] := 1.0;                    FMatrix[8] := 1.0;                    FMatrix[13] := 1.0;                    FMatrix[18] := 1.0;
////FMatrix[4] := 0.0;                    FMatrix[9] := 0.0;                    FMatrix[14] := 0.0;                    FMatrix[19] := 0.0;
//
//
//    //CoefX.Create(ColorMatrix[0],ColorMatrix[1],ColorMatrix[2],ColorMatrix[3]);
//    //CoefY.Create(ColorMatrix[5],ColorMatrix[6],ColorMatrix[7],ColorMatrix[8]);
//    //CoefZ.Create(ColorMatrix[10],ColorMatrix[11],ColorMatrix[12],ColorMatrix[13]);
//    //CoefW.Create(ColorMatrix[15],ColorMatrix[16],ColorMatrix[17],ColorMatrix[18]);
//
//    //CoefX.Create(ColorMatrix[0],ColorMatrix[5],ColorMatrix[10],ColorMatrix[15]);
//    //CoefY.Create(ColorMatrix[1],ColorMatrix[6],ColorMatrix[11],ColorMatrix[16]);
//    //CoefZ.Create(ColorMatrix[2],ColorMatrix[7],ColorMatrix[12],ColorMatrix[17]);
//    //CoefW.Create(ColorMatrix[3],ColorMatrix[8],ColorMatrix[13],ColorMatrix[18]);
//  //vOffset.Create(ColorMatrix[4],ColorMatrix[9],ColorMatrix[14],ColorMatrix[19]);
//
//  //Result := (CoefX * Self) + (CoefY * Self) + (CoefZ * Self) + (CoefW * Self);
//  ////Result := ((CoefX * Self) + vWhite) + ((CoefY * Self) + vWhite) + ((CoefZ * Self) + vWhite);// + ((CoefW * Self) + vWhite);
//  //Result := Result + vOffset;//+ ColorMatrix[24]
//
//  Result := ColorMatrix.Transform(Self);
//end;

{%endregion}

{%region%----[ Pure Pascal may Convert at some point ]--------------------------}

function TBZColorVector.Swizzle(const ASwizzle: TBZVector4SwizzleRef ): TBZColorVector;
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


function TBZColorVector.DotProduct(constref A: TBZColorVector) : Single;
var
 V2:TBZVector4f;
begin
  V2.X := Self.X*A.X;
  V2.Y := Self.Y*A.Y;
  V2.Z := Self.Z*A.Z;
  //V2.W := Self.W*A.W;
  Result := V2.X + V2.Y + V2.Z;// + V2.W;
end;

{%endregion}

{$IFNDEF USE_ASM}

{%region----[ PASCAL VERSION OF ASSEMBLER ROUTINES BELOW ]----------------------}

{%region%----[ Operators ]------------------------------------------------------}

class operator TBZColorVector.+(constref A, B: TBZColorVector): TBZColorVector;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
  Result.W := A.W + B.W;
end;

class operator TBZColorVector.-(constref A, B: TBZColorVector): TBZColorVector;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
  Result.W := A.W - B.W;
end;

class operator TBZColorVector.*(constref A, B: TBZColorVector): TBZColorVector;
begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
  Result.Z := A.Z * B.Z;
  Result.W := A.W * B.W;
end;

class operator TBZColorVector./(constref A, B: TBZColorVector): TBZColorVector;
begin
  Result.X := A.X / B.X;
  Result.Y := A.Y / B.Y;
  Result.Z := A.Z / B.Z;
  Result.W := A.W / B.W; ;
end;

class operator TBZColorVector.+(constref A: TBZColorVector; constref B:Single): TBZColorVector;
begin
  Result.X := A.X + B;
  Result.Y := A.Y + B;
  Result.Z := A.Z + B;
  Result.W := A.W + B;
end;

class operator TBZColorVector.-(constref A: TBZColorVector; constref B:Single): TBZColorVector;
begin
  Result.X := A.X - B;
  Result.Y := A.Y - B;
  Result.Z := A.Z - B;
  Result.W := A.W - B;
end;

class operator TBZColorVector.*(constref A: TBZColorVector; constref B:Single): TBZColorVector;
begin
  Result.X := A.X * B;
  Result.Y := A.Y * B;
  Result.Z := A.Z * B;
  Result.W := A.W * B;
end;

class operator TBZColorVector./(constref A: TBZColorVector; constref B:Single): TBZColorVector;
begin
  Result.X := A.X / B;
  Result.Y := A.Y / B;
  Result.Z := A.Z / B;
  Result.W := A.W / B;
end;

class operator TBZColorVector.=(constref A, B: TBZColorVector): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y) and (A.Z = B.Z) and (A.W = B.W);
end;

class operator TBZColorVector.>=(constref A, B: TBZColorVector): Boolean;
begin
  Result := (A.X >= B.X) and (A.Y >= B.Y) and (A.Z >= B.Z) and (A.W >= B.W);
end;

class operator TBZColorVector.<=(constref A, B: TBZColorVector): Boolean;
begin
  Result := (A.X <= B.X) and (A.Y <= B.Y) and (A.Z <= B.Z) and (A.W <= B.W);
end;

class operator TBZColorVector.>(constref A, B: TBZColorVector): Boolean;
begin
  Result := (A.X > B.X) and (A.Y > B.Y) and (A.Z > B.Z) and (A.W > B.W);
end;

class operator TBZColorVector.<(constref A, B: TBZColorVector): Boolean;
begin
  Result := (A.X < B.X) and (A.Y < B.Y) and (A.Z < B.Z) and (A.W < B.W);
end;

class operator TBZColorVector.<>(constref A, B: TBZColorVector): Boolean;
begin
  Result := (A.X <> B.X) or (A.Y <> B.Y) or (A.Z <> B.Z) or (A.W <> B.W);
end;

class operator TBZColorVector.xor(constref A, B : TBZColorVector) : TBZColorVector;
begin
  //Result.Red := A.Red * xor B.Red;
  //Result.Green := A.Green xor B.Green;
  //Result.Blue := A.Blue xor B.Blue;
  ////Result.Alpha := A.Alpha xor B.Alpha;
  //Result.Alpha := A.Alpha;
  Result := A;
end;

{%endregion%}

{%region%----[ Functions ]------------------------------------------------------}

function TBZColorVector.DivideBy2 : TBZColorVector;
begin
  Result.x := Self.X*0.5;
  Result.y := Self.Y*0.5;
  Result.z := Self.Z*0.5;
  Result.w := Self.W*0.5;
end;

function TBZColorVector.Min(constref B: Single): TBZColorVector;
begin
  Result := Self;
  if Self.X>B then Result.X := B;
  if Self.Y>B then Result.Y := B;
  if Self.Z>B then Result.Z := B;
  if Self.W>B then Result.W := B;
End;

function TBZColorVector.Min(constref B: TBZColorVector): TBZColorVector;
begin
  Result := Self;
  if Self.X>B.X then Result.X := B.X;
  if Self.Y>B.Y then Result.Y := B.Y;
  if Self.Z>B.Z then Result.Z := B.Z;
  if Self.W>B.W then Result.W := B.W;
End;

function TBZColorVector.Max(constref B: TBZColorVector): TBZColorVector;
begin
  Result := Self;
  if Self.X<B.X then Result.X := B.X;
  if Self.Y<B.Y then Result.Y := B.Y;
  if Self.Z<B.Z then Result.Z := B.Z;
  if Self.W<B.W then Result.W := B.W;
End;

function TBZColorVector.Max(constref B: Single): TBZColorVector;
begin
  Result := Self;
  if Self.X<B then Result.X := B;
  if Self.Y<B then Result.Y := B;
  if Self.Z<B then Result.Z := B;
  if Self.W<B then Result.W := B;
End;

function TBZColorVector.Clamp(Constref AMin, AMax: TBZColorVector): TBZColorVector;
begin
  Result := Self.Max(AMin);
  Result := Result.Min(AMax);
End;

function TBZColorVector.Clamp(constref AMin, AMax: Single): TBZColorVector;
begin
  Result := Self.Max(AMin);
  Result := Result.Min(AMax);
End;

function TBZColorVector.MulAdd(Constref B, C: TBZColorVector): TBZColorVector;
begin
  result := (self*B)+C;
end;

function TBZColorVector.MulSub(Constref B, C: TBZColorVector): TBZColorVector;
begin
  result := (self*B)-C;
end;

function TBZColorVector.MulDiv(Constref B, C: TBZColorVector): TBZColorVector;
begin
  result := (self*B)/C;
end;

function TBZColorVector.Lerp(Constref B: TBZColorVector; Constref T:Single): TBZColorVector;
begin
   Result.X:=Self.X+(B.X-Self.X)*t;
   Result.Y:=Self.Y+(B.Y-Self.Y)*t;
   Result.Z:=Self.Z+(B.Z-Self.Z)*t;
   Result.W := Self.W;
end;

function TBZColorVector.Combine(constref V2: TBZColorVector; constref F1: Single): TBZColorVector;
begin
  Result.X:=Self.X+v2.X*F1;
  Result.Y:=Self.Y+v2.Y*F1;
  Result.Z:=Self.Z+v2.Z*F1;
//  Result.W:=Self.W+v2.W*F1;
  Result.W:=0;
End;

function TBZColorVector.Combine2(constref V2: TBZColorVector; const F1, F2: Single): TBZColorVector;
begin
   Result.X:=( Self.X*F1) + (V2.X*F2);
   Result.Y:=( Self.Y*F1) + (V2.Y*F2);
   Result.Z:=( Self.Z*F1) + (V2.Z*F2);
   Result.W:=0;
end;

function TBZColorVector.Combine3(constref V2, V3: TBZColorVector; const F1, F2, F3: Single): TBZColorVector;
begin
  Result.X:=( Self.X*F1) + (V2.X*F2) + (V3.X*F3);
  Result.Y:=( Self.Y*F1) + (V2.Y*F2) + (V3.Y*F3);
  Result.Z:=( Self.Z*F1) + (V2.Z*F2) + (V3.Z*F3);
  Result.W:=0;
end;

function TBZColorVector.Round:TBZVector4i;
begin
  result.X:=System.Round(Self.X);
  result.Y:=System.Round(Self.Y);
  result.Z:=System.Round(Self.Z);
  result.W:=System.Round(Self.W);
end;

function TBZColorVector.Abs:TBZColorVector;
begin
  result.X := System.Abs(Self.X);
  result.Y := System.Abs(Self.Y);
  result.Z := System.Abs(Self.Z);
  result.W := System.Abs(Self.W);
end;

{%endregion%}

{%endregion%}

{$ENDIF}
