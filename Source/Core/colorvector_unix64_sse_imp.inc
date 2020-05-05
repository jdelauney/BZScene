{%region%----[ Operators ]--------------------------------------------------------}

class operator TBZColorVector.+(constref A, B: TBZColorVector): TBZColorVector; assembler; nostackframe; register;
asm
  movaps  xmm0, [A]
  movaps  xmm1, [B]
  addps   xmm0, xmm1
  movhlps xmm1, xmm0
end;

class operator TBZColorVector.-(constref A, B: TBZColorVector): TBZColorVector; assembler; nostackframe; register;
asm
  movaps  xmm0, [A]
  movaps  xmm1, [B]
  subps   xmm0, xmm1
  movhlps xmm1, xmm0
end;

class operator TBZColorVector.*(constref A, B: TBZColorVector): TBZColorVector; assembler; nostackframe; register;
asm
  movaps  xmm0, [A]
  movaps  xmm1, [B]
  mulps   xmm0, xmm1
  movhlps xmm1, xmm0
end;

class operator TBZColorVector./(constref A, B: TBZColorVector): TBZColorVector; assembler; nostackframe; register;
asm
  movaps  xmm0, [A]
  movaps  xmm1, [B]
  divps   xmm0, xmm1
  movhlps xmm1, xmm0
end;

class operator TBZColorVector.+(constref A: TBZColorVector; constref B:Single): TBZColorVector; assembler; nostackframe; register;
asm
  movaps  xmm0, [A]
  movss   xmm1, [B]
  shufps  xmm1, xmm1, $00
  addps   xmm0, xmm1
  movhlps xmm1, xmm0
end;

class operator TBZColorVector.-(constref A: TBZColorVector; constref B:Single): TBZColorVector; assembler; nostackframe; register;
asm
  movaps  xmm0, [A]
  movss   xmm1, [B]
  shufps  xmm1, xmm1, $00
  subps   xmm0, xmm1
  movhlps xmm1, xmm0
end;

class operator TBZColorVector.*(constref A: TBZColorVector; constref B:Single): TBZColorVector; assembler; nostackframe; register;
asm
  movaps  xmm0, [A]
  movss   xmm1, [B]
  shufps  xmm1, xmm1, $00
  mulps   xmm0, xmm1
  movhlps xmm1, xmm0
end;

class operator TBZColorVector./(constref A: TBZColorVector; constref B:Single): TBZColorVector; assembler; nostackframe; register;
asm
  movaps  xmm0, [A]
  movss   xmm1, [B]
  shufps  xmm1, xmm1, $00
  divps   xmm0, xmm1
  movhlps xmm1, xmm0
end;


class operator TBZColorVector.= (constref A, B: TBZColorVector): boolean; assembler; nostackframe; register;
asm
  movaps  xmm1, [A]
  movaps  xmm0, [B]
  cmpps   xmm0, xmm1, cSSE_OPERATOR_EQUAL    //  Yes: $FFFFFFFF, No: $00000000 ; 0 = Operator Equal
  movmskps eax, xmm0
  xor eax, $f
  setz al
end;

class operator TBZColorVector.<= (constref A, B: TBZColorVector): boolean; assembler; nostackframe; register;
asm
  movaps  xmm0, [A]
  movaps  xmm1, [B]
  cmpps   xmm0, xmm1, cSSE_OPERATOR_LESS_OR_EQUAL    //  Yes: $FFFFFFFF, No: $00000000 ; 2 = Operator Less or Equal
  movmskps eax, xmm0
  xor eax, $f
  setz al
end;

class operator TBZColorVector.>= (constref A, B: TBZColorVector): boolean; assembler; nostackframe; register;
asm
  movaps  xmm0, [A]
  movaps  xmm1, [B]
  cmpps   xmm0, xmm1, cSSE_OPERATOR_NOT_LESS    //  Yes: $FFFFFFFF, No: $00000000 ; 6 = Operator Not Less Or Equal
  movmskps eax, xmm0
  xor eax, $f
  setz al
end;

class operator TBZColorVector.< (constref A, B: TBZColorVector): boolean; assembler; nostackframe; register;
asm
  movaps  xmm0, [A]
  movaps  xmm1, [B]
  cmpps   xmm0, xmm1, cSSE_OPERATOR_LESS    //  Yes: $FFFFFFFF, No: $00000000 ; 1 = Operator Less Than
  movmskps eax, xmm0
  xor eax, $f
  setz al
end;

class operator TBZColorVector.>(constref A, B: TBZColorVector): boolean; assembler; nostackframe; register;
asm
  movaps  xmm0, [A]
  movaps  xmm1, [B]
  cmpps   xmm0, xmm1, cSSE_OPERATOR_NOT_LESS_OR_EQUAL     //  Yes: $FFFFFFFF, No: $00000000 ; 5 = Operator Not Less Than
  movmskps eax, xmm0
  xor eax, $f
  setz al
end;

class operator TBZColorVector.<> (constref A, B: TBZColorVector): boolean; assembler; nostackframe; register;
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

function TBZColorVector.DivideBy2:TBZColorVector;assembler; nostackframe; register;
asm
  movups  xmm0, [RDI]
  movups  xmm1, [RIP+cHalfOneVector4f]
  mulps   xmm0, xmm1
  movhlps xmm1, xmm0
end;

function TBZColorVector.Min(constref B: Single): TBZColorVector; assembler; nostackframe; register;
asm
  movaps  xmm0, [RDI]
  movss   xmm1, [B]
  shufps  xmm1, xmm1, $00 // Replicate B
  minps   xmm0, xmm1
  movhlps xmm1, xmm0
end;

function TBZColorVector.Min(constref B: TBZColorVector): TBZColorVector; assembler; nostackframe; register;
asm
  movaps  xmm0, [RDI]
  movaps  xmm1, [B]
  minps   xmm0, xmm1
  movhlps xmm1, xmm0
end;

function TBZColorVector.Max(constref B: Single): TBZColorVector; assembler; nostackframe; register;
asm
  movaps  xmm0, [RDI]
  movss   xmm1, [B]
  shufps  xmm1, xmm1, $00 // Replicate B
  maxps   xmm0, xmm1
  movhlps xmm1, xmm0
end;

function TBZColorVector.Max(constref B: TBZColorVector): TBZColorVector; assembler; nostackframe; register;
asm
  movaps  xmm0, [RDI]
  movaps  xmm1, [B]
  maxps   xmm0, xmm1
  movhlps xmm1, xmm0
end;

function TBZColorVector.Clamp(constref AMin, AMax: Single): TBZColorVector; assembler; nostackframe; register;
asm
  movaps  xmm0, [RDI]
  movss   xmm2, [AMin]
  movss   xmm3, [AMax]
  shufps  xmm2, xmm2, $00 // Replicate AMin
  shufps  xmm3, xmm3, $00 // Replicate AMax
  maxps   xmm0, xmm2
  minps   xmm0, xmm3
  movhlps xmm1, xmm0
end;

function TBZColorVector.Clamp(Constref AMin, AMax: TBZColorVector): TBZColorVector; assembler; nostackframe; register;
asm
  movaps  xmm0, [RDI]
  movaps  xmm1, [AMin]
  movaps  xmm2, [AMax]
  maxps   xmm0, xmm1
  minps   xmm0, xmm2
  movhlps xmm1, xmm0
end;

function TBZColorVector.MulAdd(Constref B, C: TBZColorVector): TBZColorVector; assembler; nostackframe; register;
asm
  movaps  xmm0, [RDI]
  movaps  xmm1, [B]
  movaps  xmm2, [C]
  mulps   xmm0, xmm1
  addps   xmm0, xmm2
  movhlps xmm1, xmm0
end;

function TBZColorVector.MulSub(Constref B, C: TBZColorVector): TBZColorVector; assembler; nostackframe; register;
asm
  movaps xmm0,[RDI]
  //movaps xmm1, [B]
  //movaps xmm2, [C]
  mulps  xmm0, XMMWORD PTR [B] //xmm1
  Subps  xmm0, XMMWORD PTR [C] //xmm2
  movhlps xmm1, xmm0
end;

function TBZColorVector.MulDiv(Constref B, C: TBZColorVector): TBZColorVector; assembler; nostackframe; register;
asm
  movaps  xmm0, [RDI]
  movaps  xmm1, [B]
  movaps  xmm2, [C]
  mulps   xmm0, xmm1
  Divps   xmm0, xmm2
  movhlps xmm1, xmm0
end;

function TBZColorVector.Lerp(Constref B: TBZColorVector; Constref T:Single): TBZColorVector; assembler; nostackframe; register;
asm
  movaps  xmm0, [RDI]
  movaps  xmm1, [B]
  movss   xmm2, [T]
  shufps  xmm2, xmm2, $0
  andps   xmm2, [RIP+cSSE_MASK_NO_W]
  subps   xmm1, xmm0
  mulps   xmm1, xmm2

  addps   xmm0, xmm1
  movhlps xmm1, xmm0
end;

function TBZColorVector.Combine(constref V2: TBZColorVector; constref F1: Single): TBZColorVector;assembler; nostackframe; register;
asm
  movaps  xmm0, [RDI]
  movaps  xmm1, [V2]
  movss   xmm2, [F1]
  shufps  xmm2, xmm2, $00 // replicate

  mulps   xmm1, xmm2      // V2*F1
  addps   xmm0, xmm1      // Self + (V2*F1)

  andps   xmm0, [RIP+cSSE_MASK_NO_W]
  movhlps xmm1,xmm0
end;

function TBZColorVector.Combine2(constref V2: TBZColorVector; const F1, F2: Single): TBZColorVector;assembler; nostackframe; register;
asm
  movaps  xmm2, [RDI]     // get in one hit V1
  movaps  xmm3, [V2]      // get in one hit V2

  shufps  xmm0, xmm0, $00 // replicate  F1
  shufps  xmm1, xmm1, $00 // replicate  F2

  mulps   xmm0, xmm2      // Self * F1
  mulps   xmm1, xmm3      // V2 * F2

  addps   xmm0, xmm1      // (Self * F1) + (V2 * F2)

  andps   xmm0, [RIP+cSSE_MASK_NO_W]
  movhlps xmm1, xmm0
end;

function TBZColorVector.Combine3(constref V2, V3: TBZColorVector; const F1, F2, F3: Single): TBZColorVector;  assembler; nostackframe; register;
asm
 movaps  xmm3, [RDI]

 movaps  xmm4, [V2]
 movaps  xmm5, [V3]

 shufps  xmm0, xmm0, $00 // replicate  F1
 shufps  xmm1, xmm1, $00 // replicate  F2
 shufps  xmm2, xmm2, $00 // replicate  F3

 mulps   xmm0, xmm3      // Self * F1
 mulps   xmm4, xmm1      // V2 * F2
 mulps   xmm5, xmm2      // V3 * F3

 addps   xmm0, xmm4      // (Self * F1) + (V2 * F2)
 addps   xmm0, xmm5      // ((Self * F1) + (V2 * F2)) + (V3 * F3)

 andps   xmm0, [RIP+cSSE_MASK_NO_W]
 movhlps xmm1,xmm0
end;

function TBZColorVector.Round: TBZVector4i;assembler;nostackframe;register;
asm
  // Rounding mode defaults to round-to-nearest
  movaps   xmm0, [RDI]
  cvtps2dq xmm0, xmm0
  movhlps  xmm1, xmm0
  movq     RAX,  xmm0
  movq     RDX,  xmm1
end;


{%endregion%}

