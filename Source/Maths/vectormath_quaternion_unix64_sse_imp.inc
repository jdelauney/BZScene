{%region%----[ Operators ]--------------------------------------------------------}

class operator TBZQuaternion.*(constref A, B: TBZQuaternion): TBZQuaternion; assembler; nostackframe; register;
Const
 cControlWZYX : TBZQuaternion =(x:1.0;y:-1.0;z:1.0;w:-1.0);
 cControlZWXY : TBZQuaternion =(x:1.0;y:1.0;z:-1.0;w:-1.0);
 cControlYXWZ : TBZQuaternion =(x:-1.0;y:1.0;z:1.0;w:-1.0);
asm
 movaps xmm1, [A]
 movaps xmm0, [B]

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

 addps xmm5, xmm2
 addps xmm5, xmm3
 addps xmm5, xmm4
 movaps xmm0, xmm5
 movhlps xmm1,xmm0
// movaps [RESULT], xmm5
end;

class operator TBZQuaternion.=(constref A, B: TBZQuaternion): Boolean; assembler; nostackframe; register;
asm
  movaps   xmm1, [A]
  movaps   xmm0, [B]
  cmpps    xmm0, xmm1, cSSE_OPERATOR_EQUAL    //  Yes: $FFFFFFFF, No: $00000000 ; 0 = Operator Equal
  movmskps eax,  xmm0
  xor      eax,  $F
  setz     al
end;

class operator TBZQuaternion.<>(constref A, B: TBZQuaternion): Boolean; assembler; nostackframe; register;
asm
  movaps   xmm0, [A]
  movaps   xmm1, [B]
  cmpps    xmm0, xmm1, cSSE_OPERATOR_NOT_EQUAL    //  Yes: $FFFFFFFF, No: $00000000 ; 4 = Operator Not Equal
  movmskps eax,  xmm0
  or       eax,  eax
  setnz    al
end;

{%endregion%}

{%region%----[ Functions ]--------------------------------------------------------}

function TBZQuaternion.Conjugate : TBZQuaternion;  assembler; nostackframe; register;
Const
 cSSE_SIGNMASK_XYZ : array [0..3] of UInt32 = ($80000000, $80000000, $80000000, $00000000);
asm
  movaps  xmm0, [RDI]
  xorps   xmm0, [RIP+cSSE_SIGNMASK_XYZ]
  movhlps xmm1, xmm0
end;


// base before opts 0.939547 slightly slower that pascal
// add nostackframe 1.311267
// use Peter Cordes code 1.762932
function TBZQuaternion.Magnitude : Single; assembler; nostackframe; register;
asm
  movaps   xmm0, [RDI]
  mulps    xmm0, xmm0
{$ifdef USE_ASM_SSE_3}
  movshdup xmm1, xmm0
  addps    xmm0, xmm1
  movhlps  xmm1, xmm0
  addss    xmm0, xmm1
{$else}
  movaps   xmm1, xmm0
  shufps   xmm1, xmm0, 10110001b //177
  addps    xmm0, xmm1
  movhlps  xmm1, xmm0
  addss    xmm0, xmm1
{$endif}
  sqrtss    xmm0, xmm0
 // pshufd xmm1, xmm0, $0E
 // addps  xmm0, xmm1
 // pshufd xmm1, xmm0, $01
 // addss  xmm0, xmm1
 // sqrtss xmm0, xmm0
 //// movss [RESULT], {%H-}xmm0
end;

// three working regs + one for epsilon
// commis is non destructive :)
// loop unroll factor of 5 for 64bit 2 for 32bit.
procedure TBZQuaternion.Normalize; assembler; nostackframe; register;
asm
  movaps   xmm0, [RDI]
  movaps   xmm2, xmm0
  mulps    xmm2, xmm0
{$ifdef USE_ASM_SSE_3}
  movshdup xmm1, xmm2
  addps    xmm2, xmm1
  movhlps  xmm1, xmm2
  addss    xmm2, xmm1
{$else}
  movaps   xmm1, xmm2
  shufps   xmm1, xmm2, 10110001b //177
  addps    xmm2, xmm1
  movhlps  xmm1, xmm2
  addss    xmm2, xmm1
{$endif}
  sqrtss   xmm2, xmm2
  movlps   xmm1, [RIP+cEpsilon30]
  comiss   xmm2, xmm1
  jb  @LessThan
  shufps   xmm2, xmm2, $00
  divps    xmm0, xmm2
  jmp @Finish
@LessThan:
  movaps   xmm0, [RIP+IdentityQuaternion]
@Finish:
  movaps  [RDI], xmm0             // SF 2.88


//{$ifdef TEST}
//  // Normalize fast
//  movaps  xmm0, [RDI]
//  movaps  xmm2, xmm0
//
//  // Dot(A, A)
//  mulps   xmm0, xmm0
//  pshufd  xmm1, xmm0, $4E
//  addps   xmm0, xmm1
//  pshufd  xmm1, xmm0, $11
//  addps   xmm0, xmm1
//
//  rsqrtps xmm0, xmm0          <-- not needed slow and inaccurate
//  mulps   xmm0, xmm2
//  movhlps xmm1, xmm0
//{$else}
//  movaps  xmm0, [RDI]
//  movaps  xmm2, xmm0
//  movlps  xmm1, [RDI]12
////  andps   xmm2, [RIP+cSSE_MASK_NO_W]
//
//  mulps   xmm2, xmm2
//  pshufd  xmm1, xmm2, $4E
//  addps   xmm2, xmm1
//  pshufd  xmm1, xmm2, $11
//  addss   xmm2, xmm1
//
//
//  sqrtss  xmm2, xmm2
//  movlps  xmm5, [RIP+cEpsilon30]
//  comiss  xmm2, xmm5
//  jb  @LessThan
//  //rcpss xmm2,xmm2
//  shufps  xmm2, xmm2, $00
//  movaps  xmm3, [RIP+cOneVector4f]    <---- not needed wasteful
//  divps   xmm3, xmm2                  <---- not needed wasteful
//  mulps   xmm0, xmm3              <--- just div this instead x * 1/Y == x/y this is a scalar
//  jmp @Finish                          optimisation which does not hold for vectors.
//@LessThan:
//  movaps  xmm0, [RIP+IdentityQuaternion]
//@Finish:
//  movaps [RDI], xmm0                           SF 1.9
//{$endif}

end;


// TODO Note in unix qFirst is already in xmm0, xmm1
function TBZQuaternion.MultiplyAsSecond(const qFirst : TBZQuaternion): TBZQuaternion; assembler; nostackframe; register;
Const
 cControlWZYX : TBZQuaternion =(x:1.0;y:-1.0;z:1.0;w:-1.0);
 cControlZWXY : TBZQuaternion =(x:1.0;y:1.0;z:-1.0;w:-1.0);
 cControlYXWZ : TBZQuaternion =(x:-1.0;y:1.0;z:1.0;w:-1.0);
asm
  movlhps xmm0, xmm1     // get upper half from xmm1
  movaps  xmm2, xmm0
  movaps  xmm3, xmm0      //<-- todo do not need lots of aps
  movaps  xmm4, xmm0      //  can use the fact that xmm1 hold low and high
  movaps  xmm5, xmm0      // and 64 bit copy (faster) as items below
  movaps  xmm0, [RDI]     // need low or high bits

  shufps  xmm5, xmm5, 11111111b //w
  shufps  xmm2, xmm2, 00000000b //x
  shufps  xmm3, xmm3, 01010101b //y
  shufps  xmm4, xmm4, 10101010b //z

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

  movaps xmm0, xmm5
  movhlps xmm1,xmm0
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
