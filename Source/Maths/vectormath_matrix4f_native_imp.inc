{%region%----[ Pure Pascal will never Convert ]---------------------------------}
// Temp comment this region is where we keep pascal routines for which it does
// not make sense to use SSE/AVX

function TBZMatrix4f.GetComponent(const ARow, AColumn: Integer): Single;
begin
  Result := Self.M[ARow, AColumn];
end;

procedure TBZMatrix4f.SetComponent(const ARow, AColumn: Integer; const Value: Single);
begin
  Self.M[ARow, AColumn] := Value;
end;

procedure TBZMatrix4f.SetRow(const AIndex: Integer; const Value: TBZVector);
begin
  Self.V[AIndex] := Value;
end;

function TBZMatrix4f.GetRow(const AIndex: Integer): TBZVector;
begin
  Result := V[AIndex];
end;

function TBZMatrix4f.ToString : String;
begin
//  Result := '|'+V[0].ToString+'|'+V[1].ToString+'|'+V[2].ToString+'|'+V[3].ToString+'|';
  Result := #13#10
          +'|'+V[0].ToString+'|'+#13#10
          +'|'+V[1].ToString+'|'+#13#10
          +'|'+V[2].ToString+'|'+#13#10
          +'|'+V[3].ToString+'|';
End;

procedure TBZMatrix4f.CreateIdentityMatrix;
begin
  Self:=IdentityHmgMatrix;
End;

procedure TBZMatrix4f.CreateScaleMatrix(const v : TBZAffineVector);
begin
   Self:=IdentityHmgMatrix;
   Self.X.X:=v.X;
   Self.Y.Y:=v.Y;
   Self.Z.Z:=v.Z;
end;

procedure TBZMatrix4f.CreateScaleMatrix(const v : TBZVector);
begin
   Self:=IdentityHmgMatrix;
   Self.X.X:=v.X;
   Self.Y.Y:=v.Y;
   Self.Z.Z:=v.Z;
end;

procedure TBZMatrix4f.CreateTranslationMatrix(const V: TBZAffineVector);
begin
   Self:=IdentityHmgMatrix;
   Self.X.W:=V.X;
   Self.Y.W:=V.Y;
   Self.Z.W:=V.Z;
end;

procedure TBZMatrix4f.CreateTranslationMatrix(const V: TBZVector);
begin
   Self:=IdentityHmgMatrix;
   Self.X.W:=V.X;
   Self.Y.W:=V.Y;
   Self.Z.W:=V.Z;
end;

procedure TBZMatrix4f.CreateScaleAndTranslationMatrix(const aScale, Offset : TBZVector);
begin
   Self:=IdentityHmgMatrix;
   Self.X.X:=ascale.X;   Self.X.W:=offset.X;
   Self.Y.Y:=ascale.Y;   Self.Y.W:=offset.Y;
   Self.Z.Z:=ascale.Z;   Self.Z.W:=offset.Z;
end;

procedure TBZMatrix4f.CreateRotationMatrixX(const sine, cosine: Single);
begin
   Self:=IdentityHmgMatrix;

   // Right Hand Rotation Matrix around X
   // For Left Hand invert the sign of Sin
   {    X     Y     Z    W
    X | 1 |   0  |  0  | 0
    Y | 0 |  cos | sin | 0
    Z | 0 | -sin | cos | 0
    W | 0 |   0  |  0  | 1
   }

   Self.Y.Y:=cosine;
   Self.Y.Z:=-sine;

   Self.Z.Y:=sine;
   Self.Z.Z:=cosine;

end;

procedure TBZMatrix4f.CreateRotationMatrixX(const angle : Single);
var
   s, c : Single;
begin
   //BZMath.
   SinCos(angle, s, c);
   CreateRotationMatrixX(s, c);
end;

procedure TBZMatrix4f.CreateRotationMatrixY(const sine, cosine: Single);
begin
   Self:=IdentityHmgMatrix;

   // Right Hand Rotation Matrix around Y
   // For Left Hand invert the sign of Sin
   {     X      Y      Z     W
    X | Cos |   0  |  Sin  | 0
    Y |  0  |   1  |   0   | 0
    Z |-Sin |   0  |  cos  | 0
    W |  0  |   0  |   0   | 1
   }
   Self.X.X:=cosine;
   Self.X.Z:=sine;
   Self.Z.X:=-sine;
   Self.Z.Z:=cosine;
end;

procedure TBZMatrix4f.CreateRotationMatrixY(const angle : Single);
var
   s, c : Single;
begin
   //BZMath.
   SinCos(angle, s, c);
   CreateRotationMatrixY(s, c);
end;

procedure TBZMatrix4f.CreateRotationMatrixZ(const sine, cosine: Single);
begin
   Self:=IdentityHmgMatrix;

   // Right Hand Rotation Matrix around Z
   // For Left Hand invert the sign of Sin
   {     X      Y      Z     W
    X | Cos | -Sin |   0   | 0
    Y | Sin |  Cos |   0   | 0
    Z |  0  |   0  |   1   | 0
    W |  0  |   0  |   0   | 1
   }
   Self.X.X:=cosine;
   Self.X.Y:=-sine;
   Self.Y.X:=sine;
   Self.Y.Y:=cosine;
end;

procedure TBZMatrix4f.CreateRotationMatrixZ(const angle : Single);
var
   s, c : Single;
begin
   //BZMath.
   SinCos(angle, s, c);
   CreateRotationMatrixZ(s, c);
end;

procedure TBZMatrix4f.CreateRotationMatrix(const anAxis : TBZAffineVector; angle : Single);
var
   axis : TBZVector;
   cosine, sine, one_minus_cosine : Single;
   omcx,omcy,omcz, omcyz, omczx, omcxy, sineX, sineY, SineZ : Single;
begin

 // Notice the rotation matrice is for a left handed system.
 // To change them for a right handed system, just remember the sine function
 // is an odd function, so
 //    sin(-Angle) = -Sin(Angle)
 // Change the signs of all the sine terms to change the handedness.

//  [Rotation Matrix Left Hand] =
//  1 + (1-cos(angle))*(x*x-1)       | -z*sin(angle)+(1-cos(angle))*x*y | y*sin(angle)+(1-cos(angle))*x*z
//  z*sin(angle)+(1-cos(angle))*x*y  | 1 + (1-cos(angle))*(y*y-1)       | -x*sin(angle)+(1-cos(angle))*y*z
//  -y*sin(angle)+(1-cos(angle))*x*z | x*sin(angle)+(1-cos(angle))*y*z	| 1 + (1-cos(angle))*(z*z-1)
//  where: x,y and z represent a unit vector
//  angle is the angle of rotation about this unit vector.

// http://www.euclideanspace.com/maths/geometry/rotations/conversions/angleToMatrix/index.htm
// http://www.euclideanspace.com/maths/algebra/matrix/orthogonal/rotation/index.htm

   //axis.AsVector3f := anAxis;
   axis.CreatePoint(anAxis);
   //BZMath.
   //SinCos(angle, sine, cosine);
   Sine := System.Sin(angle);
   Cosine := System.Cos(angle);
   one_minus_cosine:=1-cosine;
   axis := axis.Normalize;

   omcx := one_minus_cosine * axis.V[0];
   omcy := one_minus_cosine * axis.V[1];
   omcz := one_minus_cosine * axis.V[2];
   omczx := omcz * axis.V[0];
   omcyz := omcy * axis.V[2];
   omcxy := omcx * axis.V[1];

   sineX := axis.V[0] * sine;
   sineY := axis.V[1] * sine;
   sineZ := axis.V[2] * sine;

   Self.X.X:=(omcx * axis.V[0]) + cosine;
   Self.X.Y:= omcxy - SineZ;
   Self.X.Z:= omczx + SineY;
   Self.X.W:=0;

   Self.Y.X:= omcxy + SineZ;
   Self.Y.Y:=(omcy * axis.V[1]) + cosine;
   Self.Y.Z:= omcyz - SineX;
   Self.Y.W:=0;

   Self.Z.X:= omczx - SineY;
   Self.Z.Y:= omcyz + SineX;
   Self.Z.Z:=(omcz * axis.V[2]) + cosine;
   Self.Z.W:=0;

   Self.W.X:=0;
   Self.W.Y:=0;
   Self.W.Z:=0;
   Self.W.W:=1;
end;

procedure TBZMatrix4f.CreateRotationMatrix(const anAxis : TBZVector; angle : Single);
begin
   CreateRotationMatrix(anAxis.AsVector3f, angle);
end;

procedure TBZMatrix4f.CreateRotationMatrixYawPitchRoll(yawAngle, pitchAngle, rollAngle : Single);
var
   rotx, roty, rotz : TBZMatrix4f;
begin
  rotX.CreateRotationMatrixX(DegToRadian(YawAngle));
  rotY.CreateRotationMatrixY(DegToRadian(PitchAngle));
  rotZ.CreateRotationMatrixZ(DegToRadian(RollAngle));
  Self := (rotZ * rotY) * rotX;
end;


(* procedure CreateRotationMatrix(angleX, angleY, aangleZ : Single;Const  EulerOrder:TBZEulerOrder = eulZXY);
var
  mt0, mr1, mr2, mr4, mr5, mr6, mr8, mr9, mr10 : Double;
  xs,xc,ys,yc,zs,zc : Double;
  t1,t2 : double;
begin

  SinCos(AngleX, xs,xc);
  SinCos(AngleY, ys,yc);
  SinCos(AngleZ, zs,zc);
  Case EulerOrder of
     eulXYZ : // XYZ order
     begin
	t1 := xc*ys;
	t2 := xs*ys;

	mr0 = c*e; //mt11
	mr1 = -c*f;//mt12
	mr2 = -d;  //mt13

	mr4 = -(bd*e) + (a*f); //mt21
	mr5 = (bd*f) + (a*e);  //mt22
	mr6 = -b*c;            //mt23

	mr8 = (ad*e) + (b*f);  //mt31
	mr9 = -(ad*f) + (b*e); //mt32
	mr10 = a*c;            //mt33


      end;
	case 11 : // XZY order
		float af = a*f;
		float bf = b*f;

		mr0 = c*e;
		mr1 = -f;
		mr2 = -d*e;

		mr4 = (af*c) - (b*d);
		mr5 = a*e;
		mr6 = -(af*d) - (b*c);

		mr8 = (bf*c) + (a*d);
		mr9 = b*e;
		mr10 = -(bf*d) + (a*c);

		break;

	case 12 : // YXZ order
		float bc = b*c;
		float db = d*b;

		mr0 = (c*e) - (db*f);
		mr1 = -(c*f) - (db*e);
		mr2 = -d*a;

		mr4 = a*f;
		mr5 = a*e;
		mr6 = -b;

		mr8 = (d*e) + (bc*f);
		mr9 = -(d*f) + (bc*e);
		mr10 = c*a;

		break;

	case 13 : // YZX order
		float cf = c*f;
		float df = d*f;

		mr0 = c*e;
		mr1 = -(cf*a) - (d*b);
		mr2 = (cf*b) - (d*a);

		mr4 = f;
		mr5 = a*e;
		mr6 = -b*e;

		mr8 = d*e;
		mr9 = -(df*a) + (b*c);
		mr10 = (df*b) + (a*c);

		break;

	case 14 : // ZXY order
		float eb = e*b;
		float fb = f*b;

		mr0 = (c*e) + (fb*d);
		mr1 = -f*a;
		mr2 = -(d*e) + (fb*c);

		mr4 = (c*f) - (eb*d);
		mr5 = e*a;
		mr6 = -(d*f) - (eb*c);

		mr8 = a*d;
		mr9 = b;
		mr10 = a*c;

		break;

	case 15 : // ZYX order
		float ed = e*d;
		float fd = f*d;

		mr0 = e*c;
		mr1 = -(f*a) - (ed*b);
		mr2 = (f*b) - (ed*a);

		mr4 = f*c;
		mr5 = (e*a) - (fd*b);
		mr6 = -(e*b) - (fd*a);

		mr8 = d;
		mr9 = c*b;
		mr10 = c*a;

		break;

	default : return;
}

//	( current matrix * rotation matrix ) multiplication
float mt0 = (m11*mr0) + (m12*mr4) + (m13*mr8);
float mt1 = (m11*mr1) + (m12*mr5) + (m13*mr9);
float mt2 = (m11*mr2) + (m12*mr6) + (m13*mr10);

float mt4 = (m21*mr0) + (m22*mr4) + (m23*mr8);
float mt5 = (m21*mr1) + (m22*mr5) + (m23*mr9);
float mt6 = (m21*mr2) + (m22*mr6) + (m23*mr10);

float mt8 = (m31*mr0) + (m32*mr4) + (m33*mr8);
float mt9 = (m31*mr1) + (m32*mr5) + (m33*mr9);
float mt10 = (m31*mr2) + (m32*mr6) + (m33*mr10);

m11 = mt0; m12 = mt1; m13 = mt2;
m21 = mt4; m22 = mt5; m23 = mt6;
m31 = mt8; m32 = mt9; m33 = mt10;
}
*)

(* function TBZMatrix4f.Invert:TBZMatrix4;
var
   det : Single;
begin
   det:=GetDeterminant;
   if Abs(Det)<cEPSILON then  result:=IdentityHmgMatrix
   else
   begin
      Self.Adjoint;
      result := Self * (1/det);
   end;
end; *)



function TBZMatrix4f.Decompose(var Tran: TBZMatrixTransformations): Boolean;
var
   I, J: Integer;
   LocMat, pmat, invpmat : TBZMatrix;
   prhs, psol: TBZVector;
   row0, row1, row2 : TBZVector;
   f : Single;
begin
  Result:=False;
  locmat:=Self;
  // normalize the matrix
  if LocMat.W.W = 0 then Exit;
  for I:=0 to 3 do
    for J:=0 to 3 do
      Locmat.V[I].V[J]:=locmat.V[I].V[J] / locmat.W.W;

  // pmat is used to solve for perspective, but it also provides
  // an easy way to test for singularity of the upper 3x3 component.

  pmat:=locmat;
  for I:=0 to 2 do pmat.V[I].W:=0;
  pmat.W.W:=1;

  if pmat.Determinant = 0 then Exit;

  // First, isolate perspective.  This is the messiest.
  if (locmat.X.W <> 0) or (locmat.Y.W <> 0) or (locmat.Z.W <> 0) then
  begin
    // prhs is the right hand side of the equation.
    prhs.X:=locmat.X.W;
    prhs.Y:=locmat.Y.W;
    prhs.Z:=locmat.Z.W;
    prhs.W:=locmat.W.W;

    // Solve the equation by inverting pmat and multiplying
    // prhs by the inverse.  (This is the easiest way, not
    // necessarily the best.)

    invpmat:=pmat;
    invpmat.Invert;
    invpmat.Transpose;
    psol:=  invpmat * prhs; //VectorTransform(prhs, invpmat);

    // stuff the answer away
    Tran[ttPerspectiveX]:=psol.X;
    Tran[ttPerspectiveY]:=psol.Y;
    Tran[ttPerspectiveZ]:=psol.Z;
    Tran[ttPerspectiveW]:=psol.W;

    // clear the perspective partition
    locmat.X.W:=0;
    locmat.Y.W:=0;
    locmat.Z.W:=0;
    locmat.W.W:=1;
  end
  else
  begin
    // no perspective
    Tran[ttPerspectiveX]:=0;
    Tran[ttPerspectiveY]:=0;
    Tran[ttPerspectiveZ]:=0;
    Tran[ttPerspectiveW]:=0;
  end;

  // next take care of translation (easy)
  for I:=0 to 2 do
  begin
    Tran[TBZMAtrixTransType(Ord(ttTranslateX) + I)]:=locmat.V[3].V[I];
    locmat.V[3].V[I]:=0;
  end;

  // now get scale and shear
  row0 := locmat.X;
  row1 := locmat.Y;
  row2 := locmat.Z;

  // compute X scale factor and normalize first row
  Tran[ttScaleX]:=Row0.Norm;
  Row0 := Row0 * InvSqrt(Tran[ttScaleX]); //VectorScale(row0, RSqrt(Tran[ttScaleX]));

  // compute XY shear factor and make 2nd row orthogonal to 1st
  Tran[ttShearXY]:=row0.DotProduct(row1);
  f:=-Tran[ttShearXY];
  Row1.Combine(row0, f);

  // now, compute Y scale and normalize 2nd row
  Tran[ttScaleY]:=Row1.Norm;
  Row1 := Row1 * InvSqrt(Tran[ttScaleY]); //VectorScale(row1, RSqrt(Tran[ttScaleY]));
  Tran[ttShearXY]:=Tran[ttShearXY]/Tran[ttScaleY];

  // compute XZ and YZ shears, orthogonalize 3rd row
  Tran[ttShearXZ]:=row0.DotProduct(row2);
  f:=-Tran[ttShearXZ];
  row2.Combine(row0, f);
  Tran[ttShearYZ]:=Row1.DotProduct(row2);
  f:=-Tran[ttShearYZ];
  Row2.Combine(row1, f);

  // next, get Z scale and normalize 3rd row
  Tran[ttScaleZ]:=Row2.Norm;
  Row2:=row2* InvSqrt(Tran[ttScaleZ]);
  Tran[ttShearXZ]:=Tran[ttShearXZ] / tran[ttScaleZ];
  Tran[ttShearYZ]:=Tran[ttShearYZ] / Tran[ttScaleZ];

  // At this point, the matrix (in rows[]) is orthonormal.
  // Check for a coordinate system flip.  If the determinant
  // is -1, then negate the matrix and the scaling factors.
  if row0.DotProduct(row1.CrossProduct(row2)) < 0 then
  begin
    for I:=0 to 2 do
      Tran[TBZMatrixTransType(Ord(ttScaleX) + I)]:=-Tran[TBZMatrixTransType(Ord(ttScaleX) + I)];
    row0 := -row0;
    row1 := -row1;
    row2 := -row2;
  end;

  // now, get the rotations out, as described in the gem
  Tran[ttRotateY]:= Math.ArcSin(-row0.Z);//  BZMath.ArcSin(-row0.Z);
  if cos(Tran[ttRotateY]) <> 0 then
  begin
    Tran[ttRotateX]:= Math.ArcTan2(row1.Z, row2.Z);
    Tran[ttRotateZ]:= Math.ArcTan2(row0.Y, row0.X);
  end else
  begin
    tran[ttRotateX]:= Math.ArcTan2(row1.X, row1.Y);
    tran[ttRotateZ]:=0;
  end;
  // All done!
  Result:=True;
end;

procedure TBZMatrix4f.CreateMatrixFromFrustumPlane(Left, Right, Bottom, Top, ZNear, ZFar : Single);
begin
  Self.X.X := 2 * ZNear / (Right - Left);
  Self.X.Y := 0;
  Self.X.Z := 0;
  Self.X.W := 0;

  Self.Y.X := 0;
  Self.Y.Y := 2 * ZNear / (Top - Bottom);
  Self.Y.Z := 0;
  Self.Y.W := 0;

  Self.Z.X := (Right + Left) / (Right - Left);
  Self.Z.Y := (Top + Bottom) / (Top - Bottom);
  Self.Z.Z := -(ZFar + ZNear) / (ZFar - ZNear);
  Self.Z.W := -1;

  Self.W.X := 0;
  Self.W.Y := 0;
  Self.W.Z := -2 * ZFar * ZNear / (ZFar - ZNear);
  Self.W.W := 0;

end;

//procedure TBZMatrix4f.CreateMatrixFromFrustum(vFrustum : TBZFrustum);
//var
//  Left, Right, Bottom, Top, ZNear, ZFar : Single;
//begin
//  //
//end;

procedure TBZMatrix4f.CreatePerspectiveMatrix(FOV, Aspect, ZNear, ZFar: Single);
var
  xx, yy: Single;
begin
  FOV := BZMath.Min(179.9, BZMath.Max(0, FOV));
  yy:= ZNear * Math.Tan(BZMath.DegToRadian(FOV) * 0.5);
  xx:= yy * Aspect;
  CreateMatrixFromFrustumPlane(-xx, xx, -yy, yy, ZNear, ZFar);
end;

procedure TBZMatrix4f.CreateOrthoMatrix(Left, Right, Bottom, Top, ZNear, ZFar: Single);
begin
  Self.V[0].V[0] := 2 / (Right - Left);
  Self.V[0].V[1] := 0;
  Self.V[0].V[2] := 0;
  Self.V[0].V[3] := 0;

  Self.V[1].V[0] := 0;
  Self.V[1].V[1] := 2 / (Top - Bottom);
  Self.V[1].V[2] := 0;
  Self.V[1].V[3] := 0;

  Self.V[2].V[0] := 0;
  Self.V[2].V[1] := 0;
  Self.V[2].V[2] := -2 / (ZFar - ZNear);
  Self.V[2].V[3] := 0;

  Self.V[3].V[0] := (Left + Right) / (Left - Right);
  Self.V[3].V[1] := (Bottom + Top) / (Bottom - Top);
  Self.V[3].V[2] := (ZNear + ZFar) / (ZNear - ZFar);
  Self.V[3].V[3] := 1;
end;

procedure TBZMatrix4f.CreatePickMatrix(x, y, deltax, deltay: Single; const viewport: TBZVector4i);
begin
  if (deltax <= 0) or (deltay <= 0) then
  begin
    Self := IdentityHmgMatrix;
    exit;
  end;
  // Translate and scale the picked region to the entire window
  CreateTranslationMatrix(AffineVectorMake( (viewport.V[2] - 2 * (x - viewport.V[0])) / deltax,
	                                    (viewport.V[3] - 2 * (y - viewport.V[1])) / deltay,
                                            0.0));
  Self.V[0].V[0] := viewport.V[2] / deltax;
  Self.V[1].V[1] := viewport.V[3] / deltay;
end;

{%endregion}

{%region%----[ Pure Pascal may Convert at some point ]--------------------------}

function TBZMatrix4f.MatrixDetInternal(const a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single;
// internal version for the determinant of a 3x3 matrix
begin
  Result:=  a1 * (b2 * c3 - b3 * c2)
          - b1 * (a2 * c3 - a3 * c2)
          + c1 * (a2 * b3 - a3 * b2);
end;




{ TODO 1 -oTMatrix4 -cASM : Adjoint : Add SSE/AVX Version }
// so far has only been used in Invert and eliminated from
// assembler algo might not need to do this just keep it for pascal
// It does not do what it says on the tin anyway as it has combined
//  Adjugate and Determinant functionality
procedure TBZMatrix4f.Adjoint;
var
   a1, a2, a3, a4,
   b1, b2, b3, b4,
   c1, c2, c3, c4,
   d1, d2, d3, d4: Single;
begin
    a1:= Self.X.X; b1:= Self.X.Y;
    c1:= Self.X.Z; d1:= Self.X.W;
    a2:= Self.Y.X; b2:= Self.Y.Y;
    c2:= Self.Y.Z; d2:= Self.Y.W;
    a3:= Self.Z.X; b3:= Self.Z.Y;
    c3:= Self.Z.Z; d3:= Self.Z.W;
    a4:= Self.W.X; b4:= Self.W.Y;
    c4:= Self.W.Z; d4:= Self.W.W;

    // row column labeling reversed since we transpose rows & columns
    Self.X.X:= MatrixDetInternal(b2, b3, b4, c2, c3, c4, d2, d3, d4);
    Self.X.Y:=-MatrixDetInternal(b1, b3, b4, c1, c3, c4, d1, d3, d4);
    Self.X.Z:= MatrixDetInternal(b1, b2, b4, c1, c2, c4, d1, d2, d4);
    Self.X.W:=-MatrixDetInternal(b1, b2, b3, c1, c2, c3, d1, d2, d3);

    Self.Y.X:=-MatrixDetInternal(a2, a3, a4, c2, c3, c4, d2, d3, d4);
    Self.Z.X:= MatrixDetInternal(a2, a3, a4, b2, b3, b4, d2, d3, d4);
    Self.W.X:=-MatrixDetInternal(a2, a3, a4, b2, b3, b4, c2, c3, c4);


    Self.Y.Y:= MatrixDetInternal(a1, a3, a4, c1, c3, c4, d1, d3, d4);
    Self.Z.Y:=-MatrixDetInternal(a1, a3, a4, b1, b3, b4, d1, d3, d4);
    Self.W.Y:= MatrixDetInternal(a1, a3, a4, b1, b3, b4, c1, c3, c4);


    Self.Y.Z:=-MatrixDetInternal(a1, a2, a4, c1, c2, c4, d1, d2, d4);
    Self.Z.Z:= MatrixDetInternal(a1, a2, a4, b1, b2, b4, d1, d2, d4);
    Self.W.Z:=-MatrixDetInternal(a1, a2, a4, b1, b2, b4, c1, c2, c4);


    Self.Y.W:= MatrixDetInternal(a1, a2, a3, c1, c2, c3, d1, d2, d3);
    Self.Z.W:=-MatrixDetInternal(a1, a2, a3, b1, b2, b3, d1, d2, d3);
    Self.W.W:= MatrixDetInternal(a1, a2, a3, b1, b2, b3, c1, c2, c3);
end;

{ TODO 1 -oTMatrix4 -cASM : Transpose_Scale_M33( Add SSE/AVX Version }
procedure TBZMatrix4f.Transpose_Scale_M33(constref src : TBZMatrix4f; Constref ascale : Single);
// EAX src
// EDX dest
// ECX scale
begin
   Self.V[0].V[0]:=ascale*src.V[0].V[0];
   Self.V[1].V[0]:=ascale*src.V[0].V[1];
   Self.V[2].V[0]:=ascale*src.V[0].V[2];

   Self.V[0].V[1]:=ascale*src.V[1].V[0];
   Self.V[1].V[1]:=ascale*src.V[1].V[1];
   Self.V[2].V[1]:=ascale*src.V[1].V[2];

   Self.V[0].V[2]:=ascale*src.V[2].V[0];
   Self.V[1].V[2]:=ascale*src.V[2].V[1];
   Self.V[2].V[2]:=ascale*src.V[2].V[2];
end;


{ TODO 1 -oTMatrix4 -cASM : AnglePreservingMatrixInvert Add SSE/AVX Version }
procedure TBZMatrix4f.AnglePreservingMatrixInvert(constref mat : TBZMatrix4f);
var
   ascale : Single;
begin
   ascale:=mat.V[0].Norm;

   // Is the submatrix A singular?
   if Abs(ascale)<cEPSILON then
   begin
      // Matrix M has no inverse
      Self:=IdentityHmgMatrix;
      Exit;
   end
   else
   begin
      // Calculate the inverse of the square of the isotropic scale factor
      ascale:=1.0/ascale;
   end;

   // Fill in last row while CPU is busy with the division
   Self.V[0].V[3]:=0.0;
   Self.V[1].V[3]:=0.0;
   Self.V[2].V[3]:=0.0;
   Self.V[3].V[3]:=1.0;

   // Transpose and scale the 3 by 3 upper-left submatrix
   Self.transpose_scale_m33(mat,ascale);

   // Calculate -(transpose(A) / s*s) C
   Self.V[3].V[0]:=-(Self.V[0].V[0]*mat.V[3].V[0]
                    +Self.V[1].V[0]*mat.V[3].V[1]
                    +Self.V[2].V[0]*mat.V[3].V[2]);
   Self.V[3].V[1]:=-(Self.V[0].V[1]*mat.V[3].V[0]
                    +Self.V[1].V[1]*mat.V[3].V[1]
                    +Self.V[2].V[1]*mat.V[3].V[2]);
   Self.V[3].V[2]:=-(Self.V[0].V[2]*mat.V[3].V[0]
                    +Self.V[1].V[2]*mat.V[3].V[1]
                    +Self.V[2].V[2]*mat.V[3].V[2]);
end;

{ TODO 1 -oTMatrix4 -cASM : CreateLookAtMatrix Add SSE/AVX Version }
procedure TBZMatrix4f.CreateLookAtMatrix(const eye, center, normUp: TBZVector); //LH
var
  {$CODEALIGN VARMIN=16}
  XAxis, YAxis, ZAxis, NegVec, TransVec : TBZVector;
  tmpMat : TBZMatrix4f;
  {$CODEALIGN VARMIN=4}
begin
  // RH :
  //ZAxis := (eye - center).Normalize; //(center - eye).Normalize;     //vec3 zaxis = normal(eye - target);    // The "forward" vector
  //XAxis := (normUp.CrossProduct(ZAxis)).Normalize; // (ZAxis.CrossProduct(normUp)).Normalize;
  //YAxis := (ZAxis.CrossProduct(XAxis)).Normalize; //(XAxis.CrossProduct(ZAxis)).Normalize;
  //
  //Self.V[0] := XAxis;
  //Self.V[1] := YAxis;
  //Self.V[2] := ZAxis; //-ZAxis;
  //Self.V[3] := NullHmgPoint;
  //// this is just a reverse tranlation of eye to world origin as origin was not passed in.
  //
  //self.m14 := -XAxis.DotProduct(Eye); //-Eye.X;
  //self.m24 := -YAxis.DotProduct(Eye); //-Eye.Y;
  //self.m34 := -YAxis.DotProduct(Eye); //-Eye.Z;

  // compute the forward vector
    // NOTE: the direction is reversed (target to camera pos) because of camera transform
  ZAxis := (eye - center).Normalize;//(center - eye).Normalize;     //vec3 zaxis = normal(eye - target);    // The "forward" vector

  //YAxis := (normUp.CrossProduct(ZAxis)).Normalize;
  XAxis := (normUp.CrossProduct(ZAxis)).Normalize; //(ZAxis.CrossProduct(normUp)).Normalize;  //Up
  //XAxis := (ZAxis.CrossProduct(XAxis)).Normalize;
  YAxis := (XAxis.CrossProduct(ZAxis)).Normalize;    //Left

  tmpMat.CreateIdentityMatrix;
  tmpMat.V[0] := XAxis;
  tmpMat.V[1] := -YAxis;
  tmpMat.V[2] := ZAxis; //-ZAxis
  //tmpMat.V[3] := NullHmgPoint;
  // this is just a reverse tranlation of eye to world origin as origin was not passed in.
  NegVec := eye;
  TransVec := tmpMat * NegVec;

  Self.v[0] := tmpMat.V[0];
  Self.v[1] := tmpMat.V[1];
  Self.v[2] := tmpMat.V[2];
  Self.v[3] := tmpMat.V[3];

  Self.v[0].V[3] := TransVec.x;
  Self.v[1].V[3] := TransVec.y;
  Self.v[2].V[3] := TransVec.z;
  //Self.V[3] :=  TransVec;

  //self.m14 := -Eye.X;
  //self.m24 := -Eye.Y;
  //self.m34 := -Eye.Z;
  //GlobalLogger.LogNotice('');
  //GlobalLogger.LogNotice('TBZMatrix4f.CreateLookAtMatrix = '+Self.toString);
end;

procedure TBZMatrix4f.CreateParallelProjectionMatrix(const plane : TBZHmgPlane;const dir : TBZVector);
// Based on material from a course by William D. Shoaff (www.cs.fit.edu)
var
   dot, invDot : Single;
begin
   dot:=plane.V[0]*dir.V[0]+plane.V[1]*dir.V[1]+plane.V[2]*dir.V[2];
   if Abs(dot)<1e-5 then
   begin
      Self:=IdentityHmgMatrix;
      Exit;
   end;
   invDot:=1/dot;

   Self.m11:=( plane.V[1] * dir.V[1] + plane.V[2] * dir.V[2]) * invDot;
   Self.m12:=(-plane.V[1] * dir.V[0]) * invDot;
   Self.m13:=(-plane.V[2] * dir.V[0]) * invDot;
   Self.m14:=(-plane.V[3] * dir.V[0]) * invDot;

   Self.m21:=(-plane.V[0] * dir.V[1]) * invDot;
   Self.m22:=( plane.V[0] * dir.V[0] + plane.V[2] * dir.V[2]) * invDot;
   Self.m23:=(-plane.V[2] * dir.V[1]) * invDot;
   Self.m24:=(-plane.V[3] * dir.V[1]) * invDot;

   Self.m31:=(-plane.V[0] * dir.V[2]) * invDot;
   Self.m32:=(-plane.V[1] * dir.V[2]) * invDot;
   Self.m33:=( plane.V[0] * dir.V[0] + plane.V[1] * dir.V[1]) * invDot;
   Self.m34:=(-plane.V[3] * dir.V[2]) * invDot;

   Self.m41:=0;
   Self.m42:=0;
   Self.m43:=0;
   Self.m44:=1;
end;

procedure TBZMatrix4f.CreateShadowMatrix(const planePoint, planeNormal, lightPos : TBZVector);
var
   planeNormal3, dot : Single;
   v1,v2  : TBZVector;
   //, vl, vpn
begin
  v1 := planeNormal;
  v1.W := 0;

  v2 := planePoint;
  v2.W := 0;
	// Find the last coefficient by back substitutions
  v2 := v1 * v2;
  planeNormal3 := -(v2.X+v2.Y+v2.Z);
(*	planeNormal3:=-( planeNormal.V[0]*planePoint.V[0]
                   +planeNormal.V[1]*planePoint.V[1]
                   +planeNormal.V[2]*planePoint.V[2]); *)
	// Dot product of plane and light position
  v2 := LightPos;
	// Find the last coefficient by back substitutions
  v1 := v1 * v2;
  dot := (v1.X+v1.Y+v1.Z+(planeNormal3 * v2.W));
(*	dot:= planeNormal.V[0]*lightPos.V[0]
        +planeNormal.V[1]*lightPos.V[1]
        +planeNormal.V[2]*lightPos.V[2]
        +planeNormal3  *lightPos.V[3]; *)

	// Now do the projection
	// First column


  //---- OPTIMIZATION ---------------------------
  //vpn1.Create(planeNormal.X,planeNormal.X,planeNormal.X,planeNormal.X);
  //vpn2.Create(planeNormal.Y,planeNormal.Y,planeNormal.Y,planeNormal.Y);
  //vpn3.Create(planeNormal.Z,planeNormal.Z,planeNormal.Z,planeNormal.Z);
  //vpn4.Create(planeNormal3,planeNormal3,planeNormal3,planeNormal3);
  //vl := lightPos;

  //v2 := v1 * vpn1
  //v3.Create(dot, 0, 0, 0)
  //Self.V[0] := v3 - v2;

  //v2 := v1 * vpn2
  //v3.Create(0, dot, 0, 0)
  //Self.V[1] := v3 - v2;

  //v2 := v1 * vpn3
  //v3.Create(0, 0, dot, 0)
  //Self.V[2] := v3 - v2;

  //v2 := v1 * vpn3
  //v3.Create(0, 0, 0, dot)
  //Self.V[3] := v3 - v2;
  //----------------------------------------------------

  Self.V[0].X:= dot - lightPos.X * planeNormal.X;
  Self.V[1].X:=     - lightPos.X * planeNormal.Y;
  Self.V[2].X:=     - lightPos.X * planeNormal.Z;
  Self.V[3].X:=     - lightPos.X * planeNormal3;
	// Second column
	Self.V[0].Y :=     - lightPos.Y * planeNormal.X;
	Self.V[1].Y := dot - lightPos.Y * planeNormal.Y;
	Self.V[2].Y :=     - lightPos.Y * planeNormal.Z;
	Self.V[3].Y :=     - lightPos.Y * planeNormal3;
	// Third Column
	Self.V[0].Z :=     - lightPos.Z * planeNormal.X;
	Self.V[1].Z :=     - lightPos.Z * planeNormal.Y;
	Self.V[2].Z := dot - lightPos.Z * planeNormal.Z;
	Self.V[3].Z :=     - lightPos.Z * planeNormal3;
	// Fourth Column
	Self.V[0].W :=     - lightPos.W * planeNormal.X;
	Self.V[1].W :=     - lightPos.W * planeNormal.Y;
	Self.V[2].W :=     - lightPos.W * planeNormal.Z;
	Self.V[3].W := dot - lightPos.W * planeNormal3;
end;

procedure TBZMatrix4f.CreateReflectionMatrix(const planePoint, planeNormal : TBZVector);
var
   pv2 : Single;
begin
   // Precalcs
   pv2:=2*planepoint.DotProduct(planeNormal);
   // 1st column
   Self.V[0].V[0]:=1-2*Sqr(planeNormal.V[0]);
   Self.V[0].V[1]:=-2*planeNormal.V[0]*planeNormal.V[1];
   Self.V[0].V[2]:=-2*planeNormal.V[0]*planeNormal.V[2];
   Self.V[0].V[3]:=0;
   // 2nd column
   Self.V[1].V[0]:=-2*planeNormal.V[1]*planeNormal.V[0];
   Self.V[1].V[1]:=1-2*Sqr(planeNormal.V[1]);
   Self.V[1].V[2]:=-2*planeNormal.V[1]*planeNormal.V[2];
   Self.V[1].V[3]:=0;
   // 3rd column
   Self.V[2].V[0]:=-2*planeNormal.V[2]*planeNormal.V[0];
   Self.V[2].V[1]:=-2*planeNormal.V[2]*planeNormal.V[1];
   Self.V[2].V[2]:=1-2*Sqr(planeNormal.V[2]);
   Self.V[2].V[3]:=0;
   // 4th column
   Self.V[3].V[0]:=pv2*planeNormal.V[0];
   Self.V[3].V[1]:=pv2*planeNormal.V[1];
   Self.V[3].V[2]:=pv2*planeNormal.V[2];
   Self.V[3].V[3]:=1;
end;

{%endregion}

{$IFNDEF USE_ASM}

{%region----[ PASCAL VERSION OF ASSEMBLER ROUTINES BELOW ]----------------------}

{%region%----[ Operators ]------------------------------------------------------}

class operator TBZMatrix4f.+(constref A, B: TBZMatrix4f): TBZMatrix4f;
begin
  Result.V[0] := A.V[0] + B.V[0];
  Result.V[1] := A.V[1] + B.V[1];
  Result.V[2] := A.V[2] + B.V[2];
  Result.V[3] := A.V[3] + B.V[3];
end;

class operator TBZMatrix4f.+(constref A: TBZMatrix4f; constref B: Single): TBZMatrix4f;
begin
  Result.V[0] := A.V[0] + B;
  Result.V[1] := A.V[1] + B;
  Result.V[2] := A.V[2] + B;
  Result.V[3] := A.V[3] + B;
end;

class operator TBZMatrix4f.-(constref A, B: TBZMatrix4f): TBZMatrix4f;
begin
  Result.V[0] := A.V[0] - B.V[0];
  Result.V[1] := A.V[1] - B.V[1];
  Result.V[2] := A.V[2] - B.V[2];
  Result.V[3] := A.V[3] - B.V[3];
end;

class operator TBZMatrix4f.-(constref A: TBZMatrix4f; constref B: Single): TBZMatrix4f;
begin
  Result.V[0] := A.V[0] - B;
  Result.V[1] := A.V[1] - B;
  Result.V[2] := A.V[2] - B;
  Result.V[3] := A.V[3] - B;
end;

class operator TBZMatrix4f.-(constref A: TBZMatrix4f): TBZMatrix4f;
begin
  Result.V[0] := -A.V[0];
  Result.V[1] := -A.V[1];
  Result.V[2] := -A.V[2];
  Result.V[3] := -A.V[3];
end;


class operator TBZMatrix4f.*(constref A: TBZMatrix4f; constref B: TBZVector4f): TBZVector4f;
begin
  Result.X := (B.X * A.M[0,0]) + (B.Y * A.M[0,1]) + (B.Z * A.M[0,2]) + (B.W * A.M[0,3]);
  Result.Y := (B.X * A.M[1,0]) + (B.Y * A.M[1,1]) + (B.Z * A.M[1,2]) + (B.W * A.M[1,3]);
  Result.Z := (B.X * A.M[2,0]) + (B.Y * A.M[2,1]) + (B.Z * A.M[2,2]) + (B.W * A.M[2,3]);
  Result.W := (B.X * A.M[3,0]) + (B.Y * A.M[3,1]) + (B.Z * A.M[3,2]) + (B.W * A.M[3,3]);
end;

class operator TBZMatrix4f.*(constref A: TBZVector4f; constref B: TBZMatrix4f): TBZVector4f;
begin
  Result.X := (A.X * B.M[0,0]) + (A.Y * B.M[1,0]) + (A.Z * B.M[2,0]) + (A.W * B.M[3,0]);
  Result.Y := (A.X * B.M[0,1]) + (A.Y * B.M[1,1]) + (A.Z * B.M[2,1]) + (A.W * B.M[3,1]);
  Result.Z := (A.X * B.M[0,2]) + (A.Y * B.M[1,2]) + (A.Z * B.M[2,2]) + (A.W * B.M[3,2]);
  Result.W := (A.X * B.M[0,3]) + (A.Y * B.M[1,3]) + (A.Z * B.M[2,3]) + (A.W * B.M[3,3]);
end;

class operator TBZMatrix4f.*(constref A, B: TBZMatrix4f): TBZMatrix4f;
begin
  Result.M[0,0] := (A.M[0,0] * B.M[0,0]) + (A.M[0,1] * B.M[1,0]) + (A.M[0,2] * B.M[2,0]) + (A.M[0,3] * B.M[3,0]);
  Result.M[0,1] := (A.M[0,0] * B.M[0,1]) + (A.M[0,1] * B.M[1,1]) + (A.M[0,2] * B.M[2,1]) + (A.M[0,3] * B.M[3,1]);
  Result.M[0,2] := (A.M[0,0] * B.M[0,2]) + (A.M[0,1] * B.M[1,2]) + (A.M[0,2] * B.M[2,2]) + (A.M[0,3] * B.M[3,2]);
  Result.M[0,3] := (A.M[0,0] * B.M[0,3]) + (A.M[0,1] * B.M[1,3]) + (A.M[0,2] * B.M[2,3]) + (A.M[0,3] * B.M[3,3]);

  Result.M[1,0] := (A.M[1,0] * B.M[0,0]) + (A.M[1,1] * B.M[1,0]) + (A.M[1,2] * B.M[2,0]) + (A.M[1,3] * B.M[3,0]);
  Result.M[1,1] := (A.M[1,0] * B.M[0,1]) + (A.M[1,1] * B.M[1,1]) + (A.M[1,2] * B.M[2,1]) + (A.M[1,3] * B.M[3,1]);
  Result.M[1,2] := (A.M[1,0] * B.M[0,2]) + (A.M[1,1] * B.M[1,2]) + (A.M[1,2] * B.M[2,2]) + (A.M[1,3] * B.M[3,2]);
  Result.M[1,3] := (A.M[1,0] * B.M[0,3]) + (A.M[1,1] * B.M[1,3]) + (A.M[1,2] * B.M[2,3]) + (A.M[1,3] * B.M[3,3]);

  Result.M[2,0] := (A.M[2,0] * B.M[0,0]) + (A.M[2,1] * B.M[1,0]) + (A.M[2,2] * B.M[2,0]) + (A.M[2,3] * B.M[3,0]);
  Result.M[2,1] := (A.M[2,0] * B.M[0,1]) + (A.M[2,1] * B.M[1,1]) + (A.M[2,2] * B.M[2,1]) + (A.M[2,3] * B.M[3,1]);
  Result.M[2,2] := (A.M[2,0] * B.M[0,2]) + (A.M[2,1] * B.M[1,2]) + (A.M[2,2] * B.M[2,2]) + (A.M[2,3] * B.M[3,2]);
  Result.M[2,3] := (A.M[2,0] * B.M[0,3]) + (A.M[2,1] * B.M[1,3]) + (A.M[2,2] * B.M[2,3]) + (A.M[2,3] * B.M[3,3]);

  Result.M[3,0] := (A.M[3,0] * B.M[0,0]) + (A.M[3,1] * B.M[1,0]) + (A.M[3,2] * B.M[2,0]) + (A.M[3,3] * B.M[3,0]);
  Result.M[3,1] := (A.M[3,0] * B.M[0,1]) + (A.M[3,1] * B.M[1,1]) + (A.M[3,2] * B.M[2,1]) + (A.M[3,3] * B.M[3,1]);
  Result.M[3,2] := (A.M[3,0] * B.M[0,2]) + (A.M[3,1] * B.M[1,2]) + (A.M[3,2] * B.M[2,2]) + (A.M[3,3] * B.M[3,2]);
  Result.M[3,3] := (A.M[3,0] * B.M[0,3]) + (A.M[3,1] * B.M[1,3]) + (A.M[3,2] * B.M[2,3]) + (A.M[3,3] * B.M[3,3]);
end;

class operator TBZMatrix4f.*(constref A: TBZMatrix4f; constref B: Single): TBZMatrix4f;
begin
  Result.V[0] := A.V[0] * B;
  Result.V[1] := A.V[1] * B;
  Result.V[2] := A.V[2] * B;
  Result.V[3] := A.V[3] * B;
end;

class operator TBZMatrix4f./(constref A: TBZMatrix4f; constref B: Single): TBZMatrix4f;
var
  InvB: Single;
begin
  InvB := 1 / B;
  Result.V[0] := A.V[0] * InvB;
  Result.V[1] := A.V[1] * InvB;
  Result.V[2] := A.V[2] * InvB;
  Result.V[3] := A.V[3] * InvB;
end;


{%endregion%}

{%region%----[ Functions ]------------------------------------------------------}
function TBZMatrix4f.GetDeterminant: Single;
begin
  //Result:= Self.V[0].X*MatrixDetInternal(Self.V[1].Y, Self.V[2].Y, Self.V[3].Y, Self.V[1].Z, Self.V[2].Z, Self.V[3].Z, Self.V[1].W, Self.V[2].W, Self.V[3].W)
  //        -Self.V[0].Y*MatrixDetInternal(Self.V[1].X, Self.V[2].X, Self.V[3].X, Self.V[1].Z, Self.V[2].Z, Self.V[3].Z, Self.V[1].W, Self.V[2].W, Self.V[3].W)
  //        +Self.V[0].Z*MatrixDetInternal(Self.V[1].X, Self.V[2].X, Self.V[3].X, Self.V[1].Y, Self.V[2].Y, Self.V[3].Y, Self.V[1].W, Self.V[2].W, Self.V[3].W)
  //        -Self.V[0].W*MatrixDetInternal(Self.V[1].X, Self.V[2].X, Self.V[3].X, Self.V[1].Y, Self.V[2].Y, Self.V[3].Y, Self.V[1].Z, Self.V[2].Z, Self.V[3].Z);
  with Self do
  begin
    Result := m14*m23*m32*m41 - m13*m24*m32*m41 - m14*m22*m33*m41 + m12*m24*m33*m41 +
              m13*m22*m34*m41 - m12*m23*m34*m41 - m14*m23*m31*m42 + m13*m24*m31*m42 +
              m14*m21*m33*m42 - m11*m24*m33*m42 - m13*m21*m34*m42 + m11*m23*m34*m42 +
              m14*m22*m31*m43 - m12*m24*m31*m43 - m14*m21*m32*m43 + m11*m24*m32*m43 +
              m12*m21*m34*m43 - m11*m22*m34*m43 - m13*m22*m31*m44 + m12*m23*m31*m44 +
              m13*m21*m32*m44 - m11*m23*m32*m44 - m12*m21*m33*m44 + m11*m22*m33*m44;
  end;
end;

function TBZMatrix4f.Invert:TBZMatrix4f;
var
  det: single;
begin
  det := Self.determinant;
  if Abs(Det)<cEPSILON then  result:=IdentityHmgMatrix
  else
  begin
    det := 1/det;
    with self do
    begin
      Result.m11 := (m23*m34*m42 - m24*m33*m42 + m24*m32*m43 - m22*m34*m43 - m23*m32*m44 + m22*m33*m44) * det;
      Result.m12 := (m14*m33*m42 - m13*m34*m42 - m14*m32*m43 + m12*m34*m43 + m13*m32*m44 - m12*m33*m44) * det;
      Result.m13 := (m13*m24*m42 - m14*m23*m42 + m14*m22*m43 - m12*m24*m43 - m13*m22*m44 + m12*m23*m44) * det;
      Result.m14 := (m14*m23*m32 - m13*m24*m32 - m14*m22*m33 + m12*m24*m33 + m13*m22*m34 - m12*m23*m34) * det;

      Result.m21 := (m24*m33*m41 - m23*m34*m41 - m24*m31*m43 + m21*m34*m43 + m23*m31*m44 - m21*m33*m44) * det;
      Result.m22 := (m13*m34*m41 - m14*m33*m41 + m14*m31*m43 - m11*m34*m43 - m13*m31*m44 + m11*m33*m44) * det;
      Result.m23 := (m14*m23*m41 - m13*m24*m41 - m14*m21*m43 + m11*m24*m43 + m13*m21*m44 - m11*m23*m44) * det;
      Result.m24 := (m13*m24*m31 - m14*m23*m31 + m14*m21*m33 - m11*m24*m33 - m13*m21*m34 + m11*m23*m34) * det;

      Result.m31 := (m22*m34*m41 - m24*m32*m41 + m24*m31*m42 - m21*m34*m42 - m22*m31*m44 + m21*m32*m44) * det;
      Result.m32 := (m14*m32*m41 - m12*m34*m41 - m14*m31*m42 + m11*m34*m42 + m12*m31*m44 - m11*m32*m44) * det;
      Result.m33 := (m12*m24*m41 - m14*m22*m41 + m14*m21*m42 - m11*m24*m42 - m12*m21*m44 + m11*m22*m44) * det;
      Result.m34 := (m14*m22*m31 - m12*m24*m31 - m14*m21*m32 + m11*m24*m32 + m12*m21*m34 - m11*m22*m34) * det;

      Result.m41 := (m23*m32*m41 - m22*m33*m41 - m23*m31*m42 + m21*m33*m42 + m22*m31*m43 - m21*m32*m43) * det;
      Result.m42 := (m12*m33*m41 - m13*m32*m41 + m13*m31*m42 - m11*m33*m42 - m12*m31*m43 + m11*m32*m43) * det;
      Result.m43 := (m13*m22*m41 - m12*m23*m41 - m13*m21*m42 + m11*m23*m42 + m12*m21*m43 - m11*m22*m43) * det;
      Result.m44 := (m12*m23*m31 - m13*m22*m31 + m13*m21*m32 - m11*m23*m32 - m12*m21*m33 + m11*m22*m33) * det;
    end;
  end;
end;

//function TBZMatrix4f.Invert:TBZMatrix4f;
//var
//   det : Single;
//begin
//   det:=GetDeterminant;
//   if Abs(Det)<cEPSILON then  result:=IdentityHmgMatrix
//   else
//   begin
//      Result := Self;
//      Result.Adjoint;
//      Result := Result * (1/det);
//   end;
//end;

function TBZMatrix4f.Translate(constref v : TBZVector4f):TBZMatrix4f;
begin
   Result := Self;
   Result.X.W:=Self.X.W+v.X;
   Result.Y.W:=Self.Y.W+v.Y;
   Result.Z.W:=Self.Z.W+v.Z;
end;

function TBZMatrix4f.Multiply(constref M2 : TBZMatrix4f): TBZMatrix4f;
var
  I: Integer;
begin
  for I := 0 to 3 do
    Result.V[I] := Self.V[I] * M2.V[I];
end;

function TBZMatrix4f.Normalize: TBZMatrix4f;
begin
   Result.X.W:=0;
   Result.X.Normalize;
   Result.Y.W:=0;
   Result.Y.Normalize;
   Result.Z:=Result.X.CrossProduct(Result.Y);
   Result.X:=Result.Y.CrossProduct(Result.Z);
   Result.W:=WHmgVector;
end;

function TBZMatrix4f.Transpose: TBZMatrix4f;
begin
  Result.M[0,0] := M[0,0];
  Result.M[0,1] := M[1,0];
  Result.M[0,2] := M[2,0];
  Result.M[0,3] := M[3,0];

  Result.M[1,0] := M[0,1];
  Result.M[1,1] := M[1,1];
  Result.M[1,2] := M[2,1];
  Result.M[1,3] := M[3,1];

  Result.M[2,0] := M[0,2];
  Result.M[2,1] := M[1,2];
  Result.M[2,2] := M[2,2];
  Result.M[2,3] := M[3,2];

  Result.M[3,0] := M[0,3];
  Result.M[3,1] := M[1,3];
  Result.M[3,2] := M[2,3];
  Result.M[3,3] := M[3,3];
end;


{%endregion%}

{%endregion%}

{$ENDIF}
