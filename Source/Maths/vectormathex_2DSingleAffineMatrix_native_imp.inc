{%region%----[ Pure Pascal will never Convert ]---------------------------------}
// Temp comment this region is where we keep pascal routines for which it does
// not make sense to use SSE/AVX

// Internal for pascal only

function TBZ2DSingleAffineMatrix.GetComponent(const ARow, AColumn: Integer): Single; inline;
begin
  Result := Self.M[ARow, AColumn];
End;

procedure TBZ2DSingleAffineMatrix.SetComponent(const ARow, AColumn: Integer; const Value: Single); inline;
begin
  Self.M[ARow, AColumn] := Value;
End;

function TBZ2DSingleAffineMatrix.GetRow(const AIndex: Integer): TBZVector3f; inline;
begin
  Result := V[AIndex];
End;

procedure TBZ2DSingleAffineMatrix.SetRow(const AIndex: Integer; const Value: TBZVector3f); inline;
begin
  Self.V[AIndex] := Value;
End;

procedure TBZ2DSingleAffineMatrix.CreateIdentityMatrix;
begin
  m11 := 1.0;
  m12 := 0.0;
  m13 := 0.0;

  m21 := 0.0;
  m22 := 1.0;
  m23 := 0.0;

  m31 := 0.0;
  m32 := 0.0;
  m33 := 1.0;
End;

procedure TBZ2DSingleAffineMatrix.CreateNullMatrix;
begin
  m11 := 0.0;
  m12 := 0.0;
  m13 := 0.0;

  m21 := 0.0;
  m22 := 0.0;
  m23 := 0.0;

  m31 := 0.0;
  m32 := 0.0;
  m33 := 0.0;
End;

procedure TBZ2DSingleAffineMatrix.CreateTranslationMatrix(Const OffsetX, OffsetY : Single);
begin
  CreateIdentityMatrix;
  m13 := OffsetX;
  m23 := OffsetY;
End;

procedure TBZ2DSingleAffineMatrix.CreateTranslationMatrix(Constref Offset : TBZVector2f);
begin
  CreateTranslationMatrix(Offset.X, Offset.Y);
End;

procedure TBZ2DSingleAffineMatrix.CreateScaleMatrix(Const ScaleX, ScaleY : Single);
begin
  CreateIdentityMatrix;
  m11 := ScaleX;
  m22 := ScaleY;
End;

procedure TBZ2DSingleAffineMatrix.CreateScaleMatrix(Constref Scale : TBZVector2f);
begin
  CreateTranslationMatrix(Scale.X, Scale.Y);
End;

procedure TBZ2DSingleAffineMatrix.CreateRotationMatrix(Const anAngle : Single);
Var s,c : Single;
begin
  Math.SinCos(RadianToDeg(anAngle),S,C);
  CreateIdentityMatrix;
  m11 := c;
  m12 := -s; //s

  m21 := s; //-s
  m22 := c;
End;

procedure TBZ2DSingleAffineMatrix.CreateShearMatrix(const ShearX, ShearY : Single);
begin
  CreateIdentityMatrix;
  m12 := ShearX; // Tan(ShearX);
  m21 := ShearY; //Tan(ShearY);
End;

procedure TBZ2DSingleAffineMatrix.CreateShearMatrix(constref Shear : TBZVector2f);
begin
  CreateShearMatrix(Shear.X,Shear.Y);
End;

procedure TBZ2DSingleAffineMatrix.Create(ConstRef ScaleX, ScaleY, ShearX, ShearY, anAngle, OffsetX, OffsetY : Single);
var
  mScale, mShear, mRotate, mTrans, mTransInv : TBZ2DSingleAffineMatrix;
begin
 CreateIdentityMatrix;
 mScale.CreateScaleMatrix(ScaleX,ScaleY);
 mShear.CreateShearMatrix(ShearX,ShearY);
 mRotate.CreateRotationMatrix(anAngle);
 mTrans.CreateTranslationMatrix(OffsetX, OffsetY);
 mTransInv.CreateTranslationMatrix(-OffsetX, -OffsetY);

 Self := mScale * mShear;
 Self := Self * mTrans;
 Self := Self * mRotate;


End;

procedure TBZ2DSingleAffineMatrix.Create(Constref Scale, Shear, Offset : TBZVector2f; Const anAngle : Single);
begin
  Create(Scale.X,Scale.Y, Shear.X, Shear.Y, anAngle, Offset.X, Offset.Y);
End;

procedure TBZ2DSingleAffineMatrix.CreatePerspectiveMatrix(ConstRef StartX, StartY, AWidth, AHeight, x1, y1, x2, y2, x3, y3, x4, y4 : Single);
Var
  {$CODEALIGN VARMIN=16}
  StartPoint, ImgSize : TBZVector2f;
  pt1, pt2, pt3, pt4 : TBZVector2f;
  {$CODEALIGN VARMIN=4}
begin
  StartPoint.Create(StartX, StartY);
  ImgSize.Create(AWidth, AHeight);
  pt1.Create(x1, y1);
  pt2.Create(x2, y2);
  pt3.Create(x3, y3);
  pt4.Create(x4, y4);
  CreatePerspectiveMatrix(StartPoint, ImgSize, pt1, pt2, pt3, pt4);
end;

procedure TBZ2DSingleAffineMatrix.CreatePerspectiveMatrix(ConstRef StartPoint, ASize, ptA, ptB, ptC, ptD : TBZVector2f);
Var
  {$CODEALIGN VARMIN=16}
  DiffBD, DiffCD, DiffAB, DiffDC, DiffDB, DiffBA,
  Tmp: TBZVector2f;
  {$CODEALIGN VARMIN=4}
  dScaleX, dScaleY, Det1, Det2 : Single;
  MatrixTemp : TBZ2DSingleAffineMatrix;

  function EpsilonEqual(A, B : Single) : Boolean;
  begin
    Result := Abs(A - B) <= cLowEpsilon;
  end;

begin
  dScaleX := 1.0;
  dScaleY := 1.0;

  if (ASize.x > 0) then
      dScaleX := 1.0 / ASize.x;

  if (ASize.y > 0) then
      dScaleY := 1.0 / ASize.y;

  MatrixTemp.CreateIdentityMatrix;
  MatrixTemp.Translate(-StartPoint.x, -StartPoint.y);
  MatrixTemp.Scale(dScaleX, dScaleY);

  DiffAB := ptA - ptB;
  DiffDC := ptD - ptC;
  Tmp := DiffAB + DiffDC; // dx3

  if (EpsilonEqual(Tmp.x,0)) and (EpsilonEqual(Tmp.y,0)) then
  begin
    DiffDB := ptD - ptB;
    DiffBA := ptB - ptA;

    m11 := DiffBA.x;
    m12 := DiffDB.x;
    m13 := ptA.x;

    m21 := DiffBA.y;
    m22 := DiffDB.y;
    m23 := ptA.y;

    m31 := 0.0;
    m32 := 0.0;
  end
  else
  begin
    DiffBD := ptB - ptD; // dx1
    DiffCD := ptC - ptD;  // dx2

    det1 := Tmp.x * DiffCD.y - Tmp.y * DiffCD.x;
    det2 := DiffBD.x * DiffCD.y - DiffBD.y * DiffCD.x;

    if EpsilonEqual(Det2,0) then m31 :=  1.0
    else m31 := det1 / det2;

    det1 := DiffBD.x * Tmp.y - DiffBD.y * Tmp.x;

    if EpsilonEqual(Det2,0) then m32 := 1.0
    else m32 := det1 / det2;

    m11 := ptB.x - ptA.x + m31 * ptB.x;
    m12 := ptC.x - ptA.x + m32 * ptC.x;
    m13 := ptA.x;

    m21 := ptB.y - ptA.y + m31 * ptB.y;
    m22 := ptC.y - ptA.y + m32 * ptC.y;
    m23 := ptA.y;
  end;
  m33 := 1.0;

  Self := Self * MatrixTemp;
end;

procedure TBZ2DSingleAffineMatrix.CreateReflectXMatrix;
begin
  CreateIdentityMatrix;
  //m11 := 1.0;
  m22 := -1.0;
End;

procedure TBZ2DSingleAffineMatrix.CreateReflectYMatrix;
begin
  CreateIdentityMatrix;
  m11 := -1.0;
  //m22 := 1.0;
End;

procedure TBZ2DSingleAffineMatrix.CreateReflectMatrix;
begin
  CreateIdentityMatrix;
  m11 := -1.0;
  m22 := -1.0;
End;

function TBZ2DSingleAffineMatrix.ToString : String;
begin
  Result :=#13+#10
          +'|'+V[0].ToString+'|'+#13+#10
          +'|'+V[1].ToString+'|'+#13+#10
          +'|'+V[2].ToString+'|'+#13+#10;
End;

{%endregion}

{%region%----[ Pure Pascal may Convert at some point ]--------------------------}

function TBZ2DSingleAffineMatrix.GetDeterminant: Single;
begin
  Result := m11 * m22 * m33 -
            m13 * m22 * m31 +
            m21 * m32 * m13 -
            m23 * m32 * m11 +
            m31 * m12 * m23 -
            m33 * m12 * m21;
End;

class operator TBZ2DSingleAffineMatrix.*(constref A, B: TBZ2DSingleAffineMatrix): TBZ2DSingleAffineMatrix;
begin
  Result.m11 := A.m11 * B.m11 + A.m12 * B.m21 + A.m13 * B.m31;
  Result.m12 := A.m11 * B.m12 + A.m12 * B.m22 + A.m13 * B.m32;
  Result.m13 := A.m11 * B.m13 + A.m12 * B.m23 + A.m13 * B.m33;
  Result.m21 := A.m21 * B.m11 + A.m22 * B.m21 + A.m23 * B.m31;
  Result.m22 := A.m21 * B.m12 + A.m22 * B.m22 + A.m23 * B.m32;
  Result.m23 := A.m21 * B.m13 + A.m22 * B.m23 + A.m23 * B.m33;
  Result.m31 := A.m31 * B.m11 + A.m32 * B.m21 + A.m33 * B.m31;
  Result.m32 := A.m31 * B.m12 + A.m32 * B.m22 + A.m33 * B.m32;
  Result.m33 := A.m31 * B.m13 + A.m32 * B.m23 + A.m33 * B.m33;
End;

class operator TBZ2DSingleAffineMatrix.*(constref A: TBZ2DSingleAffineMatrix; constref B: TBZVector2f): TBZVector2f;
var
  Coef : Single;
begin

  Coef := A.m31 * B.x + A.m32 * B.y + A.m33;

  if (Coef = 0.0) then Coef := 1.0
  else Coef := 1.0 / Coef;

  Result.x := (A.m11 * B.x + A.m12 * B.y + A.m13) * Coef;
  Result.y := (A.m21 * B.x + A.m22 * B.y + A.m23) * Coef;
End;

class operator TBZ2DSingleAffineMatrix.*(constref A: TBZVector2f; constref B: TBZ2DSingleAffineMatrix): TBZVector2f;
var
  Coef : Single;
begin

  Coef := B.m31 * A.x + B.m32 * A.y + B.m33;

  if (Coef = 0.0) then Coef := 1.0
  else Coef := 1.0 / Coef;

  Result.x := (B.m11 * A.x + B.m12 * A.y + B.m13) * Coef;
  Result.y := (B.m21 * A.x + B.m22 * A.y + B.m23) * Coef;
   //Result.w := B.m20 * A.x + B.m21 * A.y + B.m22 ;// * A.W = 1.0
End;

function TBZ2DSingleAffineMatrix.TransformPoint(ConstRef A : TBZVector2f) : TBZVector2f;
var
  Coef : Single;
begin

  Coef := m31 * A.x + m32 * A.y + m33;

  if (Coef = 0.0) then Coef := 1.0
  else Coef := 1.0 / Coef;

  Result.x := (m11 * A.x + m12 * A.y + m13) * Coef;
  Result.y := (m21 * A.x + m22 * A.y + m23) * Coef;
   //Result.w := B.m20 * A.x + B.m21 * A.y + B.m22 ;// * A.W = 1.0
End;

function TBZ2DSingleAffineMatrix.Transpose: TBZ2DSingleAffineMatrix;
var
  tmp : Single;
begin
  Result := Self;
  tmp := m12;
  Result.m12 := m21;
  Result.m21 := tmp;

  tmp := m13;
  Result.m13 := m31;
  Result.m31 := tmp;

  tmp := m32;
  Result.m32 := m23;
  Result.m23 := tmp;
End;

function TBZ2DSingleAffineMatrix.Invert : TBZ2DSingleAffineMatrix;
var
  det : Single;
begin
  Result.CreateNullMatrix;
  det := getDeterminant;
  if (det = 0) then exit;
  det := 1 / det;
  Result.m11 := (m22 * m33 - m23 * m32) * det;
  Result.m12 := (m13 * m32 - m12 * m33) * det;
  Result.m13 := (m12 * m23 - m13 * m22) * det;
  Result.m21 := (m23 * m31 - m21 * m33) * det;
  Result.m22 := (m11 * m33 - m13 * m31) * det;
  Result.m23 := (m13 * m21 - m11 * m23) * det;
  Result.m31 := (m21 * m32 - m22 * m31) * det;
  Result.m32 := (m12 * m31 - m11 * m32) * det;
  Result.m33 := (m11 * m22 - m12 * m21) * det;
End;

procedure TBZ2DSingleAffineMatrix.Translate(x, y : Single);
Var
  Mat : TBZ2DSingleAffineMatrix;
begin
  Mat.CreateTranslationMatrix(x,y);
  Self := Self * Mat;
end;

procedure TBZ2DSingleAffineMatrix.TranslateX(x : Single);
Var
  Mat : TBZ2DSingleAffineMatrix;
begin
  Mat.CreateTranslationMatrix(x,0);
  Self := Self * Mat;
end;

procedure TBZ2DSingleAffineMatrix.TranslateY(y : Single);
Var
  Mat : TBZ2DSingleAffineMatrix;
begin
  Mat.CreateTranslationMatrix(0,y);
  Self := Self * Mat;
end;

procedure TBZ2DSingleAffineMatrix.Translate(aVector : TBZVector2f);
begin
  Self.Translate(aVector.x, aVector.y);
end;

procedure TBZ2DSingleAffineMatrix.Rotate(Angle : Single);
Var
  Mat : TBZ2DSingleAffineMatrix;
begin
  Mat.CreateRotationMatrix(Angle);
  Self := Self * Mat;
end;

procedure TBZ2DSingleAffineMatrix.Scale(x, y : Single);
Var
  Mat : TBZ2DSingleAffineMatrix;
begin
  Mat.CreateScaleMatrix(x,y);
  Self := Self * Mat;
end;

procedure TBZ2DSingleAffineMatrix.ScaleX(x : Single);
Var
  Mat : TBZ2DSingleAffineMatrix;
begin
  Mat.CreateTranslationMatrix(x,1);
  Self := Self * Mat;
end;

procedure TBZ2DSingleAffineMatrix.ScaleY(y : Single);
Var
  Mat : TBZ2DSingleAffineMatrix;
begin
  Mat.CreateTranslationMatrix(1,y);
  Self := Self * Mat;
end;

procedure TBZ2DSingleAffineMatrix.Scale(aVector : TBZVector2f);
begin
  Self.Scale(aVector.x, aVector.y);
end;

procedure TBZ2DSingleAffineMatrix.Shear(x, y : Single);
Var
  Mat : TBZ2DSingleAffineMatrix;
begin
  Mat.CreateShearMatrix(x,y);
  Self := Self * Mat;
end;

procedure TBZ2DSingleAffineMatrix.ShearX(x : Single);
Var
  Mat : TBZ2DSingleAffineMatrix;
begin
  Mat.CreateShearMatrix(x,0);
  Self := Self * Mat;
end;

procedure TBZ2DSingleAffineMatrix.ShearY(y : Single);
Var
  Mat : TBZ2DSingleAffineMatrix;
begin
  Mat.CreateShearMatrix(0,y);
  Self := Self * Mat;
end;

procedure TBZ2DSingleAffineMatrix.Shear(aVector : TBZVector2f);
begin
  Self.Shear(aVector.x, aVector.y);
end;

{%endregion}

{$IFNDEF USE_ASM}

{%region----[ PASCAL VERSION OF ASSEMBLER ROUTINES BELOW ]----------------------}


{%endregion%}

{$ENDIF}
