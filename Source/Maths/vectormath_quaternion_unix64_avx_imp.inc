{%region%----[ Operators ]------------------------------------------------------}

class operator TBZQuaternion.+(constref A, B: TBZQuaternion): TBZQuaternion;assembler; nostackframe; register;
asm
  vmovaps  xmm0, [A]
  vmovaps  xmm1, [B]
  vaddps   xmm0, xmm0, xmm1
  movhlps  xmm1, xmm0
end;

class operator TBZQuaternion.-(constref A, B: TBZQuaternion): TBZQuaternion; assembler; nostackframe; register;
asm
  vmovaps xmm0, [A]
  vmovaps xmm1, [B]
  vsubps  xmm0, xmm0, xmm1
  movhlps xmm1, xmm0
end;

class operator TBZQuaternion.-(constref A: TBZQuaternion): TBZQuaternion; assembler; nostackframe; register;
asm
  vmovaps xmm1, [A]
  vmovaps xmm0, [RIP+cNullVector4f]
  vsubps  xmm0, xmm0, xmm1
  movhlps xmm1, xmm0
end;


{ TODO 1 -oTBZQuaternion -cASM : Optimize to AVX still SSE code }
class operator TBZQuaternion.*(constref A, B: TBZQuaternion): TBZQuaternion;  assembler; nostackframe; register;
Const
 cControlWZYX : TBZQuaternion =(x:1.0;y:-1.0;z:1.0;w:-1.0);
 cControlZWXY : TBZQuaternion =(x:1.0;y:1.0;z:-1.0;w:-1.0);
 cControlYXWZ : TBZQuaternion =(x:-1.0;y:1.0;z:1.0;w:-1.0);
asm
 vmovaps xmm0, [A]
 vmovaps xmm1, [B]

 vmovaps xmm2, xmm1
 vmovaps xmm3, xmm1
 vmovaps xmm4, xmm1
 vmovaps xmm5, xmm1

 vshufps xmm5, xmm5, xmm5, 11111111b //w
 vshufps xmm2, xmm2, xmm2, 00000000b //x
 vshufps xmm3, xmm3, xmm3, 01010101b //y
 vshufps xmm4, xmm4, xmm4, 10101010b //z

 //(B.w * A.x)
 //(B.w * A.y)
 //(B.w * A.z)
 //(B.w * A.w)
 vmulps xmm5, xmm5, xmm0
 //(B.x * A.w)
 //-(B.x * A.z)
 //(B.x * A.y)
 //-(B.x * A.x)
 vmovaps xmm6, xmm0
 vshufps xmm6, xmm6, xmm6, 00011011b //wzyx
 vmulps  xmm2, xmm2, xmm6
 vmulps  xmm2, xmm2, [RIP+cControlWZYX]
 //(B.y * A.z)
 //(B.y * A.w)
 //-(B.y * A.x)
 //-(B.y * A.y)
 vmovaps xmm6, xmm0
 vshufps xmm6, xmm6, xmm6, 01001110b //zwxy
 vmulps  xmm3, xmm3, xmm6
 vmulps  xmm3, xmm3, [RIP+cControlZWXY]
 //(B.z * A.y)
 //(B.z * A.x)
 //(B.z * A.w)
 //(B.z * A.z)
 vmovaps xmm6, xmm0
 vshufps xmm6, xmm6, xmm6, 10110001b //yxwz
 vmulps  xmm4, xmm4, xmm6
 vmulps  xmm4, xmm4, [RIP+cControlYXWZ]

 vaddps  xmm0, xmm5, xmm2
 vaddps  xmm0, xmm0, xmm3
 vaddps  xmm0, xmm0, xmm4
 movhlps xmm1, xmm0
end;

class operator TBZQuaternion.+(constref A : TBZQuaternion; constref B:Single): TBZQuaternion; assembler; nostackframe; register;
asm
  vmovaps  xmm0, [A]
  vmovss   xmm1, [B]
  vshufps  xmm1, xmm1, xmm1, $00
  vaddps   xmm0, xmm0, xmm1
  movhlps  xmm1, xmm0
end;

class operator TBZQuaternion.-(constref A : TBZQuaternion; constref B:Single): TBZQuaternion; assembler; nostackframe; register;
asm
  vmovaps xmm0, [A]
  vmovss  xmm1, [B]
  vshufps xmm1, xmm1, xmm1, $00
  vsubps  xmm0, xmm0, xmm1
  movhlps xmm1, xmm0
end;

class operator TBZQuaternion.*(constref A : TBZQuaternion; constref B:Single): TBZQuaternion; assembler; nostackframe; register;
asm
  vmovaps xmm0, [A]
  vmovss  xmm1, [B]
  vshufps xmm1, xmm1, xmm1, $00
  vmulps  xmm0, xmm0, xmm1
  movhlps xmm1, xmm0
end;

class operator TBZQuaternion./(constref A : TBZQuaternion; constref B:Single): TBZQuaternion; assembler; nostackframe; register;
asm
  vmovaps xmm0, [A]
  vmovss  xmm1, [B]
  vshufps xmm1, xmm1, xmm1, $00
  vdivps  xmm0, xmm0, xmm1
  movhlps xmm1, xmm0
end;

class operator TBZQuaternion.=(constref A, B: TBZQuaternion): Boolean; assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

class operator TBZQuaternion.<>(constref A, B: TBZQuaternion): Boolean; assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

{%endregion%}

{%region%----[ Functions ]------------------------------------------------------}

function TBZQuaternion.Conjugate : TBZQuaternion; assembler; nostackframe; register;
Const
 cSSE_SIGNMASK_XYZ : array [0..3] of UInt32 = ($80000000, $80000000, $80000000, $00000000);
asm
  vmovaps xmm0, [RDI]
  vxorps  xmm0, xmm0, [RIP+cSSE_SIGNMASK_XYZ]
  movhlps xmm1, xmm0
end;

function TBZQuaternion.Magnitude : Single; assembler;
asm
  vmovaps xmm0,[RDI]
  vmulps  xmm0, xmm0, xmm0
  vhaddps xmm0, xmm0, xmm0
  vhaddps xmm0, xmm0, xmm0
  sqrtps  xmm0, xmm0
  movss  [RESULT], {%H-}xmm0
end;

{ TODO 1 -oTBZQuaternion -cASM : Optimize to AVX still SSE code }
function TBZQuaternion.Normalize : TBZQuaternion; assembler; nostackframe; register;
asm
  vmovaps xmm0, [RDI]
  vmovaps xmm2, xmm0
  movlps  xmm1, [RDI]12        //???? moving 8 bytes 4 past record should this be movss?????
  vandps  xmm2, xmm2, [RIP+cSSE_MASK_NO_W]

  vmulps  xmm2, xmm2, xmm2
  pshufd  xmm1, xmm2, $0E
  vaddps  xmm2, xmm2, xmm1
  pshufd  xmm1, xmm2, $01
  vaddss  xmm2, xmm2, xmm1
  vsqrtss xmm2, xmm2, xmm2
  movlps  xmm5, [RIP+cEpsilon30]
  vcomiss xmm2, xmm5
  jb  @LessThan
  //rcpss xmm2,xmm2
  vshufps xmm2, xmm2, xmm2, $00
  movaps  xmm3, [RIP+cOneVector4f]
  vdivps  xmm3, xmm3, xmm2
  vmulps  xmm0, xmm0, xmm3
  jmp @Finish
  @LessThan:
  vmovaps xmm0, [RIP+IdentityQuaternion]
  @Finish:
  movhlps xmm1,xmm0
end;

{ TODO 1 -oTBZQuaternion -cASM : Optimize to AVX still SSE code }
function TBZQuaternion.MultiplyAsSecond(const qFirst : TBZQuaternion): TBZQuaternion; assembler; nostackframe; register;
Const
 cControlWZYX : TBZQuaternion =(x:1.0;y:-1.0;z:1.0;w:-1.0);
 cControlZWXY : TBZQuaternion =(x:1.0;y:1.0;z:-1.0;w:-1.0);
 cControlYXWZ : TBZQuaternion =(x:-1.0;y:1.0;z:1.0;w:-1.0);
asm
  movlhps xmm0, xmm1     // get upper half from xmm1
  vmovaps xmm2, xmm0
  vmovaps xmm3, xmm0
  vmovaps xmm4, xmm0
  vmovaps xmm5, xmm0
  vmovaps xmm0, [RDI]

  vshufps xmm5, xmm5, xmm5, 11111111b //w
  vshufps xmm2, xmm2, xmm2, 00000000b //x
  vshufps xmm3, xmm3, xmm3, 01010101b //y
  vshufps xmm4, xmm4, xmm4, 10101010b //z

  //(B.w * A.x)
  //(B.w * A.y)
  //(B.w * A.z)
  //(B.w * A.w)
  vmulps xmm5, xmm5, xmm0
  //(B.x * A.w)
  //-(B.x * A.z)
  //(B.x * A.y)
  //-(B.x * A.x)
  vmovaps xmm6, xmm0
  vshufps xmm6, xmm6, xmm6, 00011011b //wzyx
  vmulps  xmm2, xmm2, xmm6
  vmulps  xmm2, xmm2, [RIP+cControlWZYX]
  //  "xorps _POSNEGPOSNEG, %%xmm4\n"		/* C2 = { + - + - } */
  //  "xorps _POSPOSNEGNEG, %%xmm5\n"		/* C3 = { + + - - } */
  //  "xorps _NEGPOSPOSNEG, %%xmm6\n"		/* C4 = { - + + - } */
  //(B.y * A.z)
  //(B.y * A.w)
  //-(B.y * A.x)
  //-(B.y * A.y)
  vmovaps xmm6, xmm0
  vshufps xmm6, xmm6, xmm6, 01001110b //zwxy
  vmulps  xmm3, xmm3, xmm6
  vmulps  xmm3, xmm3, [RIP+cControlZWXY]
  //(B.z * A.y)
  //(B.z * A.x)
  //(B.z * A.w)
  //(B.z * A.z)
  vmovaps xmm6, xmm0
  vshufps xmm6, xmm6, xmm6, 10110001b //yxwz
  vmulps  xmm4, xmm4, xmm6
  vmulps  xmm4, xmm4, [RIP+cControlYXWZ]

  vaddps  xmm0, xmm5, xmm2
  vaddps  xmm0, xmm0, xmm3
  vaddps  xmm0, xmm0, xmm4
  movhlps xmm1, xmm0
end;

function TBZQuaternion.Slerp(const QEnd: TBZQuaternion; Spin: Integer; t: Single): TBZQuaternion; assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

function TBZQuaternion.Slerp(const QEnd: TBZQuaternion; const t : Single) : TBZQuaternion; assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

{%endregion%}
