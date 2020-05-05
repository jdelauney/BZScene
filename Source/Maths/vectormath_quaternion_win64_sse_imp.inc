{.$I vectormath_quaternion_native_imp.inc}

{%region%----[ Operators ]--------------------------------------------------------}
//
//class operator TBZQuaternion.+(constref A, B: TBZQuaternion): TBZQuaternion; assembler; nostackframe; register;
//asm
//  movaps xmm0, [A]
//  movaps xmm1, [B]
//  addps  xmm0, xmm1
//  movaps [RESULT], xmm0
//end;
//
//class operator TBZQuaternion.+(constref A : TBZQuaternion; constref B:Single): TBZQuaternion;  assembler; nostackframe; register;
//asm
//  movaps xmm0, [A]
//  movss  xmm1, [B]
//  shufps xmm1, xmm1, $00
//  addps  xmm0, xmm1
//  movaps [RESULT], xmm0
//end;
//
//class operator TBZQuaternion.-(constref A, B: TBZQuaternion): TBZQuaternion; assembler; nostackframe; register;
//asm
//  movaps xmm0, [A]
//  movaps xmm1, [B]
//  subps  xmm0, xmm1
//  movaps [RESULT], xmm0
//end;
//
//class operator TBZQuaternion.-(constref A : TBZQuaternion; constref B:Single): TBZQuaternion; assembler; nostackframe; register;
//asm
//  movaps xmm0, [A]
//  movss  xmm1, [B]
//  shufps xmm1, xmm1, $00
//  subps  xmm0, xmm1
//  movaps [RESULT], xmm0
//end;
//
//class operator TBZQuaternion.-(constref A: TBZQuaternion): TBZQuaternion; assembler; nostackframe; register;
//asm
//  movups xmm1, [A]
//  movups xmm0, [RIP+cNullVector4f]
//  subps  xmm0, xmm1
//  movaps [RESULT],xmm0
//End;
//

class operator TBZQuaternion.*(constref A, B: TBZQuaternion): TBZQuaternion; assembler;
Const
 cControlWZYX : TBZQuaternion =(x:1.0;y:-1.0;z:1.0;w:-1.0);
 cControlZWXY : TBZQuaternion =(x:1.0;y:1.0;z:-1.0;w:-1.0);
 cControlYXWZ : TBZQuaternion =(x:-1.0;y:1.0;z:1.0;w:-1.0);
asm
 movaps xmm0, [B]
 movaps xmm1, [A]

 movaps xmm2, xmm1
 movaps xmm3, xmm1
 movaps xmm4, xmm1
 movaps xmm5, xmm1

 shufps xmm5, xmm5, 11111111b //w
 shufps xmm2, xmm2, 00000000b //x
 shufps xmm3, xmm3, 01010101b //y
 shufps xmm4, xmm4, 10101010b //z

 //(B.w * A.x)
 //(B.w * A.y)
 //(B.w * A.z)
 //(B.w * A.w)
 mulps xmm5, xmm0
 //(B.x * A.w)
 //-(B.x * A.z)
 //(B.x * A.y)
 //-(B.x * A.x)
 movaps xmm6, xmm0
 shufps xmm6, xmm6, 00011011b //wzyx
 mulps  xmm2, xmm6
 mulps  xmm2, [RIP+cControlWZYX]
 //(B.y * A.z)
 //(B.y * A.w)
 //-(B.y * A.x)
 //-(B.y * A.y)
 movaps xmm6, xmm0
 shufps xmm6, xmm6, 01001110b //zwxy
 mulps  xmm3, xmm6
 mulps  xmm3, [RIP+cControlZWXY]
 //(B.z * A.y)
 //(B.z * A.x)
 //(B.z * A.w)
 //(B.z * A.z)
 movaps xmm6, xmm0
 shufps xmm6, xmm6, 10110001b //yxwz
 mulps  xmm4, xmm6
 mulps  xmm4, [RIP+cControlYXWZ]

 addps  xmm5, xmm2
 addps  xmm5, xmm3
 addps  xmm5, xmm4

 movaps [RESULT], xmm5
end;

//class operator TBZQuaternion.*(constref A : TBZQuaternion; constref B:Single): TBZQuaternion; assembler; nostackframe; register;
//asm
//  movaps xmm0, [A]
//  movss  xmm1, [B]
//  shufps xmm1, xmm1, $00
//  mulps  xmm0, xmm1
//  movaps [RESULT], xmm0
//end;
//
//class operator TBZQuaternion./(constref A : TBZQuaternion; constref B:Single): TBZQuaternion; assembler; nostackframe; register;
//asm
//  movaps xmm0, [A]
//  movss  xmm1, [B]
//  shufps xmm1, xmm1, $00
//  divps  xmm0, xmm1
//  movaps [RESULT], xmm0
//end;

class operator TBZQuaternion.=(constref A, B: TBZQuaternion): Boolean; assembler; nostackframe; register;
asm
  movaps   xmm1, [A]
  movaps   xmm0, [B]
  cmpps    xmm0, xmm1, cSSE_OPERATOR_EQUAL    //  Yes: $FFFFFFFF, No: $00000000 ; 0 = Operator Equal
  movmskps eax,  xmm0
  xor eax, $F
  setz al
end;

class operator TBZQuaternion.<>(constref A, B: TBZQuaternion): Boolean; assembler; nostackframe; register;
asm
  movaps   xmm0, [A]
  movaps   xmm1, [B]
  cmpps    xmm0, xmm1, cSSE_OPERATOR_NOT_EQUAL    //  Yes: $FFFFFFFF, No: $00000000 ; 4 = Operator Not Equal
  movmskps eax,  xmm0
  or eax, eax
  setnz al
end;

{%endregion%}

{%region%----[ Functions ]--------------------------------------------------------}

function TBZQuaternion.Conjugate : TBZQuaternion;  assembler; nostackframe; register;
Const
 cSSE_SIGNMASK_XYZ : array [0..3] of UInt32 = ($80000000, $80000000, $80000000, $00000000);
asm
  movaps xmm0, [RCX]
  xorps  xmm0, [RIP+cSSE_SIGNMASK_XYZ]
  movaps [Result], xmm0
end;

function TBZQuaternion.Magnitude : Single; assembler;  nostackframe; register;
asm
  movaps   xmm0, [RCX]
  mulps    xmm0, xmm0
  //{$ifdef TEST}
  movshdup xmm1, xmm0
  addps    xmm0, xmm1
  movhlps  xmm1, xmm0
  addss    xmm0, xmm1
  sqrtss   xmm0, xmm0
  //{$else}
  //pshufd xmm1, xmm0, $0E
  //addps  xmm0, xmm1
  //pshufd xmm1, xmm0, $01
  //addss  xmm0, xmm1
  //sqrtss xmm0, xmm0


  //{$endif}
end;


procedure TBZQuaternion.Normalize; assembler; nostackframe; register;
asm
  movaps  xmm0, [RCX]
  movaps  xmm2, xmm0
  movlps  xmm1, [RCX]12
//  andps   xmm2, [RIP+cSSE_MASK_NO_W]

  mulps   xmm2, xmm2

  //movshdup xmm1, xmm2
  //addps    xmm2, xmm1
  //movhlps  xmm1, xmm2
  //addss    xmm2, xmm1


  pshufd  xmm1, xmm2, $0E
  addps   xmm2, xmm1
  pshufd  xmm1, xmm2, $01
  addss   xmm2, xmm1
  sqrtss  xmm2, xmm2

  movlps  xmm5, [RIP+cEpsilon30]
  comiss  xmm2, xmm5
  jb  @LessThan
  //rcpss xmm2,xmm2
  shufps  xmm2, xmm2, $00
  movaps  xmm3, [RIP+cOneVector4f]
  divps   xmm3, xmm2
  mulps   xmm0, xmm3
  jmp @Finish
@LessThan:
  movaps  xmm0, [RIP+IdentityQuaternion]
@Finish:
  movaps  [RCX], {%H-}xmm0
end;

function TBZQuaternion.MultiplyAsSecond(const qFirst : TBZQuaternion): TBZQuaternion; assembler; nostackframe; register;
Const
 cControlWZYX : TBZQuaternion =(x:1.0;y:-1.0;z:1.0;w:-1.0);
 cControlZWXY : TBZQuaternion =(x:1.0;y:1.0;z:-1.0;w:-1.0);
 cControlYXWZ : TBZQuaternion =(x:-1.0;y:1.0;z:1.0;w:-1.0);
asm
  movaps xmm0, [RCX]
  movaps xmm1, [qFirst]

  movaps xmm2, xmm1
  movaps xmm3, xmm1
  movaps xmm4, xmm1
  movaps xmm5, xmm1

  shufps xmm5, xmm5, 11111111b //w
  shufps xmm2, xmm2, 00000000b //x
  shufps xmm3, xmm3, 01010101b //y
  shufps xmm4, xmm4, 10101010b //z

  //(B.w * A.x)
  //(B.w * A.y)
  //(B.w * A.z)
  //(B.w * A.w)
  mulps  xmm5, xmm0
  //(B.x * A.w)
  //-(B.x * A.z)
  //(B.x * A.y)
  //-(B.x * A.x)
  movaps xmm6, xmm0
  shufps xmm6, xmm6, 00011011b //wzyx
  mulps  xmm2, xmm6
  mulps  xmm2, [RIP+cControlWZYX]
//  "xorps _POSNEGPOSNEG, %%xmm4\n"		/* C2 = { + - + - } */
//  "xorps _POSPOSNEGNEG, %%xmm5\n"		/* C3 = { + + - - } */
//  "xorps _NEGPOSPOSNEG, %%xmm6\n"		/* C4 = { - + + - } */
  //(B.y * A.z)
  //(B.y * A.w)
  //-(B.y * A.x)
  //-(B.y * A.y)
  movaps xmm6, xmm0
  shufps xmm6, xmm6, 01001110b //zwxy
  mulps  xmm3, xmm6
  mulps  xmm3, [RIP+cControlZWXY]
  //(B.z * A.y)
  //(B.z * A.x)
  //(B.z * A.w)
  //(B.z * A.z)
  movaps xmm6, xmm0
  shufps xmm6, xmm6, 10110001b //yxwz
  mulps  xmm4, xmm6
  mulps  xmm4, [RIP+cControlYXWZ]

  addps  xmm5, xmm2
  addps  xmm5, xmm3
  addps  xmm5, xmm4

  movaps [RESULT], xmm5
end;

function TBZQuaternion.Slerp(const QEnd: TBZQuaternion; const t : Single) : TBZQuaternion;
var
   to1: array[0..4] of Single;
   omega, cosom, sinom, scale0, scale1: Extended;
// t goes from 0 to 1
// absolute rotations
begin
   // calc cosine
   cosom:= Self.ImagePart.X*QEnd.ImagePart.X
          +Self.ImagePart.Y*QEnd.ImagePart.Y
          +Self.ImagePart.Z*QEnd.ImagePart.Z
	  +Self.RealPart*QEnd.RealPart;
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
   if ((1.0-cosom)>cEpsilon) then // standard case (slerp)
   begin
      omega:=BZMath.ArcCos(cosom);
      sinom:=1/Sin(omega);
      scale0:=Sin((1.0-t)*omega)*sinom;
      scale1:=Sin(t*omega)*sinom;
   end
   else  // "from" and "to" quaternions are very close
   begin
      //  ... so we can do a linear interpolation
      scale0:=1.0-t; //1.0-t;
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

function TBZQuaternion.Slerp(const QEnd: TBZQuaternion; Spin: Integer; t: Single): TBZQuaternion;
begin
  Result := Self.Slerp(Qend,t*spin);
end;


{%endregion%}
