
{%region%----[ Pure Pascal will never Convert ]---------------------------------}
// Temp comment this region is where we keep pascal routines for which it does
// not make sense to use SSE/AVX

function TBZQuaternion.ToString : String;
begin
   Result := '(ImagePart.X: '+FloattoStrF(Self.X,fffixed,5,5)+
            ' ,ImagePart.Y: '+FloattoStrF(Self.Y,fffixed,5,5)+
            ' ,ImagePart.Z: '+FloattoStrF(Self.Z,fffixed,5,5)+
            ' , RealPart.W: '+FloattoStrF(Self.W,fffixed,5,5)+')';
End;

procedure TBZQuaternion.Create(const Imag: array of Single; Real : Single);
var
   n : Integer;
begin
   n:=Length(Imag);
   if n>=1 then Self.ImagePart.X:=Imag[0];
   if n>=2 then Self.ImagePart.Y:=Imag[1];
   if n>=3 then Self.ImagePart.Z:=Imag[2];
   Self.RealPart:=real;
end;

procedure TBZQuaternion.Create(x,y,z: Single; Real : Single);
begin
  Self.ImagePart.X:=X;
  Self.ImagePart.Y:=Y;
  Self.ImagePart.Z:=Z;
  Self.RealPart:=real;
end;


function TBZQuaternion.Transform(constref V: TBZVector): TBZVector;
var
  res, v2, Conj: TBZQuaternion;
begin
    conj := self.Conjugate;
    v2.Create(V.X,V.Y,V.Z,0);
    res := Self * v2;
    res := res * conj;
    Result := Res.AsVector4f;
    Result.W := 1;
end;

procedure TBZQuaternion.Scale(ScaleVal: single);
begin
   Self.AsVector4f := Self.AsVector4f * Sqrt(ScaleVal);
end;

{%endregion%}

{%region%----[ Pure Pascal may Convert at some point ]--------------------------}

procedure TBZEulerAngles.Create(X,Y,Z:Single); // Roll, Yaw, Pitch
begin
  Self.X := X;
  Self.Y := Y;
  Self.Z := Z;
end;

procedure TBZEulerAngles.Create(Const Angles: TBZVector);
begin
  Self.X := Angles.X;
  Self.Y := Angles.Y;
  Self.Z := Angles.Z;
end;

function TBZEulerAngles.ConvertToRotationMatrix(Const  EulerOrder:TBZEulerOrder = eulYZX): TBZMatrix;
begin
  result := IdentityHmgMatrix;
end;

// http://www.euclideanspace.com/maths/geometry/rotations/conversions/eulerToAngle/index.htm
procedure TBZEulerAngles.ConvertToAngleAxis(Out Angle : Single; Out Axis : TBZVector;Const EulerOrder : TBZEulerOrder = eulYZX);
Var
 c1,c2,c3,s1,s2,s3, i1,i2,i3, c1c2, s1s2 : Single;
begin
 i1 := DegToRad(Self.V[cEulerOrderRef[eulerOrder][1]])/2;
 i2 := DegToRad(Self.V[cEulerOrderRef[eulerOrder][2]])/2;
 i3 := DegToRad(Self.V[cEulerOrderRef[eulerOrder][3]])/2;

 // Can be optimized with TBZvector2f and swizzling properties
 SinCos(i1, s1, c1);
 SinCos(i2, s2, c2);
 SinCos(i3, s3, c3);


 c1c2 := c1 * c2;
 s1s2 := s1 * s2;

 Axis.w := c1c2 * c3 - s1s2 * s3;
 Axis.x := c1c2 * s3 + s1s2 * c3;
 Axis.y := s1 * c2 * c3 + c1 * s2 * s3;
 Axis.z := c1 * s2 * c3 - s1 * c2 * s3;

 Angle := 2 * ArcCos(Axis.w);

 if (Axis.LengthSquare < cEpsilon40) then
 begin
    // when all euler angles are zero angle =0 so we can set axis to anything to avoid divide by zero
    Axis.Create(1.0,0.0,0.0);
 end
 else
 begin
   Axis := Axis.Normalize;
   //L = Sqrt(L);
   //Axis.X := Axis.X / L;
   //Axis.X := Axis.X / L;
   //Axis.X := Axis.X / L;
 end;
end;

// This region is for pascal routines that are required to create a working
// library of routines. We may or may not convert these to SSE/AVX.

procedure TBZQuaternion.Create(const V1, V2: TBZAffineVector);
Var
  vv1,vv2 : TBZVector;
begin
   vv1.AsVector3f := V1;
   vv2.AsVector3f := V2;
   Self.AsVector4f:=vv1.CrossProduct(vv2);
   Self.RealPart:= vv1.DotProduct(vv2) + 1;
   self.Normalize;
end;

procedure TBZQuaternion.Create(const V1, V2: TBZVector); overload;
begin
   Self.AsVector4f := v1.CrossProduct(v2);
   Self.RealPart := v1.DotProduct(v2) + 1;
   self.Normalize;
end;

//procedure TBZQuaternion.Create(const V1, V2: TBZVector);


procedure TBZQuaternion.Create(const mat : TBZMatrix);
// the matrix must be a rotation matrix!
var
   traceMat, s, invS : Double;
begin
   traceMat := mat.m11 + mat.m22 + mat.m33;
   if traceMat > cFullEpsilon then begin
      s := 2 * Sqrt(1 + traceMat);
      invS := 1 / s;
      Self.X := (mat.m32 - mat.m23) * invS;
      Self.Y := (mat.m13 - mat.m31) * invS;
      Self.Z := (mat.m21 - mat.m12) * invS;
      Self.W := 0.25 * s;
   end else if ((mat.m11 > mat.m22) and (mat.m11 > mat.m33)) then begin  // Row 0:
      s := 2 * Sqrt(Max(cEpsilon30, cOne + mat.m11 - mat.m22 - mat.m33));
      invS:=1/s;
      Self.X := 0.25 * s;
      Self.Y := (mat.m12 + mat.m21) * invS;
      Self.Z := (mat.m31 + mat.m13) * invS;
      Self.W := (mat.m32 - mat.m23) * invS;
   end else if (mat.m22 > mat.m33) then begin  // Row 1:
      s:= 2 * Sqrt(Max(cEpsilon30, cOne + mat.m22 - mat.m11 - mat.m33));
      invS := 1 / s;
      Self.X := (mat.m12 + mat.m21) * invS;
      Self.Y := 0.25 * s;
      Self.Z := (mat.m23 + mat.m32) * invS;
      Self.W := (mat.m13 - mat.m31) * invS;
   end else begin  // Row 2:
      s := 2 * Sqrt(Max(cEpsilon30, cOne + mat.m33 - mat.m11 - mat.m22));
      invS := 1 / s;
      Self.X := (mat.m13 + mat.m31) * invS;
      Self.Y := (mat.m23 + mat.m32) * invS;
      Self.Z := 0.25 * s;
      Self.W := (mat.m21 - mat.m12) * invS;
   end;
   Self.Normalize;
end;

procedure TBZQuaternion.Create(const angle  : Single; const axis : TBZAffineVector);
//procedure TBZQuaternion.Create(const angle  : Single; const axis : TBZVector);
var
   f, s, c : Single;
   vaxis : TBZVector;
begin
   BZMath.SinCos(DegToRadian(angle*cOneHalf), s, c);
   Self.RealPart:=c;
   vaxis.Create(axis);
   f:=s/vAxis.Length;
   Self.ImagePart.V[0]:=axis.V[0]*f;
   Self.ImagePart.V[1]:=axis.V[1]*f;
   Self.ImagePart.V[2]:=axis.V[2]*f;
end;

procedure TBZQuaternion.Create(const Yaw, Pitch, Roll : Single); //Yaw Pitch Roll (Euler order = YZX)
//var
//   qr,qp, qy : TBZQuaternion;
//begin
//   qr.Create(Roll, XVector); // Create From Angle Axis
//   qp.Create(Pitch, ZVector);
//   qy.Create(Yaw, YVector);
//
//   Self := qy * qp;
//   Self:= Self * qr;
begin
  Self.Create(Roll, Yaw, Pitch, eulYZX);
end;

procedure TBZQuaternion.Create(const x, y, z: Single; eulerOrder : TBZEulerOrder);
var
   q : array [0..2] of TBZQuaternion;
begin
   {%H-}q[0].Create(X, XVector); // Create From Angle Axis
   q[1].Create(Y, YVector);
   q[2].Create(Z, ZVector);
   Self := (q[cEulerOrderRef[eulerOrder][1]] * q[cEulerOrderRef[eulerOrder][2]]);
   Self := self *q[cEulerOrderRef[eulerOrder][3]];
end;

procedure TBZQuaternion.Create(const EulerAngles : TBZEulerAngles; eulerOrder : TBZEulerOrder);
begin
  Self.Create(EulerAngles.Roll,EulerAngles.Yaw,EulerAngles.Pitch, eulerOrder);
end;

{ Constructs a rotation matrix from (possibly non-unit) quaternion.
   Assumes matrix is used to multiply column vector on the left:
   vnew = mat vold.
   Works correctly for right-handed coordinate system and right-handed rotations. }
function TBZQuaternion.ConvertToMatrix : TBZMatrix;
 var
   xx, xy, xz, xw, yy, yz, yw, zz, zw: Single;
begin
   Self.Normalize;
   xx := x * x;
   xy := x * y;
   xz := x * z;
   xw := x * w;
   yy := y * y;
   yz := y * z;
   yw := y * w;
   zz := z * z;
   zw := z * w;
   With Result do
   begin

    // Col 1
    m11 := 1 - 2 * ( yy + zz );
    m21 :=     2 * ( xy + zw );
    m31 :=     2 * ( xz - yw );
    m41 := 0;

    // Col 2
    m12 :=     2 * ( xy - zw );
    m22 := 1 - 2 * ( xx + zz );
    m32 :=     2 * ( yz + xw );
    m42 := 0;

    // Col 3
    m13 :=     2 * ( xz + yw );
    m23 :=     2 * ( yz - xw );
    m33 := 1 - 2 * ( xx + yy );
    m43 := 0;

    // Col 4
    m14 := 0;
    m24 := 0;
    m34 := 0;
    m44 := 1;
  end;
end;

// Get euler angle from rotation matrix
// http://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToEuler/index.htm
// https://www.learnopencv.com/rotation-matrix-to-euler-angles/
// http://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToAngle/derivation/index.htm
// http://www.euclideanspace.com/maths/geometry/rotations/for/index.htm

// @TODO : Take care of singularity at pole SUD  <-0.999999
function TBZQuaternion.ConvertToEuler(Const EulerOrder : TBZEulerOrder) : TBZEulerAngles;
var
  rm : TBZMatrix;
  m11,m12,m13,m21,m22,m23,m31,m32,m33 : Double;
begin
  rm := Self.ConvertToMatrix;
  // Rotation X
  m11 := rm.m11;
  m12 := Clamp(rm.m12,-1,1);
  m13 := Clamp(rm.m13,-1,1);
  // Rotation Y
  m21 := Clamp(rm.m21,-1,1);
  m22 := rm.m22;
  m23 := Clamp(rm.m23,-1,1);
  // Rotation Z
  m31 := Clamp(rm.m31,-1,1);
  m32 := Clamp(rm.m32,-1,1);
  m33 := rm.m33;

  // Remember :
  // ROLL = X ; YAW = Y ; PITCH = Z
  Result.Roll:=0;
  Result.Yaw:=0;
  Result.Pitch:=0;
  case EulerOrder of
    eulXYZ :
      begin
        Result.Yaw := ArcSin(m31);
        if Abs(m31) < 0.99999 then
        begin
          Result.Roll := ArcTan2(-m32,m33);
          Result.Pitch := ArcTan2(-m21,m11)
          //Yaw := -cPIDiv2; //DegToRad(-90);
        end
        else
        // if abs(m20)> 0.99999
        begin
          Result.Roll := ArcTan2(m23,m22);
          Result.Pitch := 0;
        end;
      end;
    eulXZY :
      begin
        Result.Pitch := ArcSin(m21);
        if Abs(m21) < 0.99999 then
        begin
           Result.Roll := ArcTan2(m23,m22);
           Result.Yaw := ArcTan2(m31,m11);
        end
        else
        begin
          Result.Roll := ArcTan2(-m32,m33);
          Result.Yaw := 0;
        end;
      end;
    eulYXZ :
      begin
        Result.Roll := ArcSin(-m32);
        if Abs(m32)< 0.99999 then
        begin
          Result.Yaw :=ArcTan2(m31,m33);
          Result.Pitch := ArcTan2(m12,m22);
        end
        else
        begin
          Result.Yaw := ArcTan2(-m13,m11);
          Result.Pitch := 0;
        end;
      end;
    eulYZX :
      begin
        Result.Pitch := ArcSin(m12);
        if Abs(m12) < 0.99999 then
        begin
          Result.Roll := ArcTan2(-m32,m22);
          Result.Yaw := ArcTan2(-m13,m11);
        end
        else
        begin
          Result.Roll := 0;
          Result.Yaw := ArcTan2(m31,m33);
        end;
      end;
    eulZXY :
      begin
        Result.Roll := ArcSin(m23);
        if Abs(m21)< 0.99999 then
        begin
          Result.Yaw := ArcTan2(-m13,m33);
          Result.Pitch := ArcTan2(-m21,m22);
        end
        else
        begin
          Result.Yaw := 0;
          Result.Pitch := ArcTan2(m12,m11);
        end;
      end;
    eulZYX :
      begin
        Result.Yaw := ArcSin(m13);
        if Abs(m13) < 0.99999 then
        begin
          Result.Roll := ArcTan2(m23,m33);
          Result.Pitch := ArcTan2(m12,m11);
        end
        else
        begin
          Result.Roll := 0;
          Result.Pitch :=  ArcTan2(-m21,m22);
        end;
      end;
  end;
  Result.Roll := RadToDeg(Result.Roll);
  Result.Pitch := RadToDeg(Result.Pitch);
  Result.Yaw := RadToDeg(Result.Yaw);
end;

procedure TBZQuaternion.ConvertToAngleAxis(out Angle  : Single; out axis : TBZAffineVector);
var
  qw2: Double;
  den: Double;
begin
  qw2 := Self.RealPart*Self.RealPart;
  Angle := RadToDeg(2*ArcCos(Self.RealPart));
  den := Sqrt(1-qw2);
  if den <> 0 then
  begin
    Axis.x := Self.X/den;
    Axis.y := Self.Y/den;
    Axis.z := Self.Z/den;
  end
  else
  begin
    Axis.x := 0;
    Axis.y := 0;
    Axis.z := 0;
  end;
end;

{%endregion%}

{$IFNDEF USE_ASM}

{%region----[ PASCAL VERSION OF ASSEMBLER ROUTINES BELOW ]----------------------}

{%region%----[ Operators ]------------------------------------------------------}

// Returns the product A*B (which is the concatenation of a rotation Q1 followed by the rotation Q2)
// Order is important reverted to A prod B as B prod A gave wrong results.
class operator TBZQuaternion.*(constref A, B: TBZQuaternion): TBZQuaternion;
begin
  Result.X :=  (A.w * B.x) + (A.x * B.w) + (A.y * B.z) - (A.z * B.y);
  Result.Y :=  (A.w * B.y) - (A.x * B.z) + (A.y * B.w) + (A.z * B.x);
  Result.Z :=  (A.w * B.z) + (A.x * B.y) - (A.y * B.x) + (A.z * B.w);
  Result.W :=  (A.w * B.w) - (A.x * B.x) - (A.y * B.y) - (A.z * B.z);
end;
{*
class operator TBZQuaternion.*(constref A, B: TBZQuaternion): TBZQuaternion;
var
  Temp : TQuaternion;
begin
  Temp.RealPart := A.RealPart * B.RealPart - A.ImagePart.X * B.ImagePart.X
                 - A.ImagePart.Y * B.ImagePart.Y - A.ImagePart.Z * B.ImagePart.Z;

  Temp.ImagePart.X := A.RealPart * B.ImagePart.X + A.ImagePart.X * B.RealPart
                   + A.ImagePart.Y * B.ImagePart.Z - A.ImagePart.Z * B.ImagePart.Y;

  Temp.ImagePart.Y := A.RealPart * B.ImagePart.Y + A.ImagePart.Y * B.RealPart
                   + A.ImagePart.Z * B.ImagePart.X - A.ImagePart.X * B.ImagePart.Z;

  Temp.ImagePart.Z := A.RealPart * B.ImagePart.Z + A.ImagePart.Z * B.RealPart
                   + A.ImagePart.X * B.ImagePart.Y - A.ImagePart.Y * B.ImagePart.X;
  Result:=Temp;
end;
*}

class operator TBZQuaternion.=(constref A, B: TBZQuaternion): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y) and (A.Z = B.Z) and (A.W = B.W);
end;

class operator TBZQuaternion.<>(constref A, B: TBZQuaternion): Boolean;
begin
  Result := (A.X <> B.X) or (A.Y <> B.Y) or (A.Z <> B.Z) or (A.W <> B.W);
end;

{%endregion%}

{%region%----[ Functions ]------------------------------------------------------}

function TBZQuaternion.Conjugate : TBZQuaternion;
begin
  Result.X:=-Self.X;
  Result.Y:=-Self.Y;
  Result.Z:=-Self.Z;
  Result.W:= Self.W;
end;

function TBZQuaternion.Magnitude : Single;
begin
  Result:=Sqrt(Self.AsVector4f.Norm + Sqr(Self.RealPart));
end;

procedure TBZQuaternion.Normalize;
var
   m, f: Single;
begin
   m:=Self.Magnitude;
   if m>cEPSILON then
   begin
      f:=1/m;
      Self.AsVector4f := Self.AsVector4f * f;
   end
   else
   Self:=IdentityQuaternion;
end;

function TBZQuaternion.MultiplyAsSecond(const qFirst : TBZQuaternion): TBZQuaternion;
var
  Temp : TBZQuaternion;
begin
  Temp.RealPart := qFirst.RealPart * Self.RealPart - qFirst.ImagePart.X * Self.ImagePart.X
                 - qFirst.ImagePart.Y * Self.ImagePart.Y - qFirst.ImagePart.Z * Self.ImagePart.Z;

  Temp.ImagePart.X := qFirst.RealPart * Self.ImagePart.X + qFirst.ImagePart.X * Self.RealPart
                   + qFirst.ImagePart.Y * Self.ImagePart.Z - qFirst.ImagePart.Z * Self.ImagePart.Y;

  Temp.ImagePart.Y := qFirst.RealPart * Self.ImagePart.Y + qFirst.ImagePart.Y * Self.RealPart
                   + qFirst.ImagePart.Z * Self.ImagePart.X - qFirst.ImagePart.X * Self.ImagePart.Z;

  Temp.ImagePart.Z := qFirst.RealPart * Self.ImagePart.Z + qFirst.ImagePart.Z * Self.RealPart
                   + qFirst.ImagePart.X * Self.ImagePart.Y - qFirst.ImagePart.Y * Self.ImagePart.X;
  Result:=Temp;
end;

function TBZQuaternion.Slerp(const QEnd: TBZQuaternion; Spin: Integer; t: Single): TBZQuaternion;
begin
Result := Self.Slerp(Qend,t*spin);
end;

function TBZQuaternion.Slerp(const QEnd: TBZQuaternion; const t : Single) : TBZQuaternion;
var
   to1: array[0..4] of Single;
   omega, cosom, sinom, scale0, scale1: Extended;
// t goes from 0 to 1
// absolute rotations
begin
   // calc cosine
   cosom:= Self.ImagePart.V[0]*QEnd.ImagePart.X
          +Self.ImagePart.V[1]*QEnd.ImagePart.Y
          +Self.ImagePart.V[2]*QEnd.ImagePart.Z
	  +Self.RealPart   *QEnd.RealPart;
   // adjust signs (if necessary)
   if cosom<0 then
   begin
      cosom := -cosom;
      to1[0] := - QEnd.ImagePart.X;
      to1[1] := - QEnd.ImagePart.Y;
      to1[2] := - QEnd.ImagePart.Z;
      to1[3] := - QEnd.RealPart;
   end
   else
   begin
      to1[0] := QEnd.ImagePart.X;
      to1[1] := QEnd.ImagePart.Y;
      to1[2] := QEnd.ImagePart.Z;
      to1[3] := QEnd.RealPart;
   end;
   // calculate coefficients
   if ((1.0-cosom)>cEpsilon30) then // standard case (slerp)
   begin
      omega:=BZMath.ArcCos(cosom);
      sinom:=1/Sin(omega);
      scale0:=Sin((1.0-t)*omega)*sinom;
      scale1:=Sin(t*omega)*sinom;
   end
   else  // "from" and "to" quaternions are very close
   begin
      //  ... so we can do a linear interpolation
      scale0:=1.0-t;
      scale1:=t;
   end;
   // calculate final values
   Result.ImagePart.V[0] := scale0 * Self.ImagePart.V[0] + scale1 * to1[0];
   Result.ImagePart.V[1] := scale0 * Self.ImagePart.V[1] + scale1 * to1[1];
   Result.ImagePart.V[2] := scale0 * Self.ImagePart.V[2] + scale1 * to1[2];
   Result.RealPart := scale0 * Self.RealPart + scale1 * to1[3];
   //NormalizeQuaternion(Result);
   //Result.pNormalize;
   Result.Normalize;
end;


{%endregion%}

{%endregion%----Assembler----}

{$ENDIF USE_ASM}
