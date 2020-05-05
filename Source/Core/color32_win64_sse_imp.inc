{%region%----[ Operators ]--------------------------------------------------------}

class operator TBZColor32.+(constref A, B: TBZColor32): TBZColor32; assembler; nostackframe; register;
asm
  movd xmm0, [A]
  //movd xmm1, [B]
  paddusb  xmm0, XMMWORD PTR [B]
  movd [RESULT], xmm0
end;

class operator TBZColor32.-(constref A, B: TBZColor32): TBZColor32; assembler; nostackframe; register;
asm
  movd xmm0, [A]
  psubusb  xmm0, XMMWORD PTR [B]
  movd [RESULT], xmm0
end;

class operator TBZColor32.*(constref A, B: TBZColor32): TBZColor32; assembler; nostackframe; register;
asm
  movd xmm0, [A]
  pmuldq  xmm0, XMMWORD PTR [B]
  movd [RESULT], xmm0
end;

{ class operator TBZColor32./(constref A, B: TBZColor32): TBZColor32; assembler; nostackframe; register;
asm
  movaps xmm0, XMMWORD PTR [A]
  {$ifdef USE_ASM_SIMD_HIGHPRECISION}
     pdivp  xmm0, XMMWORD PTR [B]
  {$else}
    movaps xmm1,XMMWORD PTR [B]
    rcpps  xmm1, xmm1
    mulps  xmm0,xmm1
  {$endif}
movaps [RESULT], xmm0
end;

class operator TBZColor32.+(constref A: TBZColor32; constref B:Single): TBZColor32; assembler; nostackframe; register;
asm
  movaps xmm0, XMMWORD PTR [A]
  movlps  xmm1,[B]
  shufps xmm1, xmm1, $00
  paddsb  xmm0,xmm1
  movaps [RESULT], xmm0
end;

class operator TBZColor32.-(constref A: TBZColor32; constref B:Byte): TBZColor32; assembler; nostackframe; register;
asm
  movaps xmm0,XMMWORD PTR [A]
  {$ifdef TEST}
    movlps  xmm1,[B]
  {$else}
    movss  xmm1,[B]
  {$endif}
  shufps xmm1, xmm1, $00
  psubsb  xmm0, xmm1
  movaps [RESULT], xmm0
end;

class operator TBZColor32.*(constref A: TBZColor32; constref B:Single): TBZColor32; assembler; nostackframe; register;
asm
  movaps xmm0, [A]
  movlps  xmm1,[B]
  shufps xmm1, xmm1, $00
  pmulld  xmm0, xmm1
  movaps [RESULT], xmm0
end;

class operator TBZColor32./(constref A: TBZColor32; constref B:Single): TBZColor32; assembler; nostackframe; register;
asm
  movaps xmm0, [A]
  movlps  xmm1,[B]
  {$ifdef USE_ASM_SIMD_HIGHPRECISION}
    shufps xmm1, xmm1, $00
    divps  xmm0,xmm1
  {$else}
    shufps xmm1, xmm1, $00
    rcpps  xmm1, xmm1
    mulps  xmm0,xmm1
  {$endif}
  movaps [RESULT], xmm0
end;


class operator TBZColor32.= (constref A, B: TBZColor32): boolean; assembler; nostackframe; register;
asm
  movaps xmm0, XMMWORD PTR [A]
  movaps xmm1, XMMWORD PTR [B]
  cmpps  xmm0, xmm1, cSSE_OPERATOR_EQUAL    //  Yes: $FFFFFFFF, No: $00000000 ; 0 = Operator Equal
  movmskps eax, xmm0
  xor eax, $F
  setz al
end;

class operator TBZColor32.<= (constref A, B: TBZColor32): boolean; assembler; nostackframe; register;
asm
  movaps xmm0, XMMWORD PTR [A]
  movaps xmm1, XMMWORD PTR [B]
  cmpps  xmm0, xmm1, cSSE_OPERATOR_LESS_OR_EQUAL    //  Yes: $FFFFFFFF, No: $00000000 ; 2 = Operator Less or Equal
  movmskps eax, xmm0
  xor eax, $f
  setz al
end;

class operator TBZColor32.>= (constref A, B: TBZColor32): boolean; assembler; nostackframe; register;
asm
  movaps xmm0, XMMWORD PTR [A]
  movaps xmm1, XMMWORD PTR [B]
  cmpps  xmm0, xmm1, cSSE_OPERATOR_NOT_LESS     //  Yes: $FFFFFFFF, No: $00000000 ; 6 = Operator Not Less Or Equal
  movmskps eax, xmm0
  xor eax, $f
  setz al
end;

class operator TBZColor32.< (constref A, B: TBZColor32): boolean; assembler; nostackframe; register;
asm
  movaps xmm0, XMMWORD PTR [A]
  movaps xmm1, XMMWORD PTR [B]
  cmpps  xmm0, xmm1, cSSE_OPERATOR_LESS    //  Yes: $FFFFFFFF, No: $00000000 ; 1 = Operator Less Than
  movmskps eax, xmm0
  xor eax, $f
  setz al
end;

class operator TBZColor32.>(constref A, B: TBZColor32): boolean; assembler; nostackframe; register;
asm
  movaps xmm0, XMMWORD PTR [A]
  movaps xmm1, XMMWORD PTR [B]
  cmpps  xmm0, xmm1,  cSSE_OPERATOR_NOT_LESS_OR_EQUAL   //  Yes: $FFFFFFFF, No: $00000000 ; 5 = Operator Not Less Than
  movmskps eax, xmm0
  xor eax, $f
  setz al
end;

class operator TBZColor32.<> (constref A, B: TBZColor32): boolean; assembler; nostackframe; register;
asm
  movaps xmm0, XMMWORD PTR [A]
  movaps xmm1, XMMWORD PTR [B]
  cmpps  xmm0, xmm1, cSSE_OPERATOR_NOT_EQUAL    //  Yes: $FFFFFFFF, No: $00000000 ; 4 = Operator Not Equal
  movmskps eax, xmm0
  or eax, eax
  setnz al
end;  }

{%endregion%}

{%region%----[ Functions ]--------------------------------------------------------}

{ function TBZColor32.DivideBy2 : TBZColor32;  assembler; nostackframe; register;
asm
  movaps xmm0,[RCX]
  mulps xmm0, XMMWORD PTR [RIP+cHalfOneVector4f]
  movaps [RESULT],xmm0
end;


function TBZColor32.Min(constref B : Byte) : TBZColor32; assembler; nostackframe; register;
asm
  movaps xmm0,[RCX]
  //  movlps  xmm1,[B]
  movss  xmm1,[B]
  shufps xmm1, xmm1, $00 // Replicate B
  minps  xmm0, xmm1
  movaps [RESULT], xmm0
end;

function TBZColor32.Min(constref B : TBZColor32) : TBZColor32; assembler; nostackframe; register;
asm
  movaps xmm0,[RCX]
  minps  xmm0, XMMWORD PTR [B]// xmm1
  movaps [RESULT], xmm0
end;

function TBZColor32.Max(constref B : Byte) : TBZColor32; assembler; nostackframe; register;
asm
  movaps xmm0,[RCX]
  {$ifdef TEST}
    movlps  xmm1,[B]    //|0|0|0|x|
    unpcklps xmm1, xmm1	//|0|0|x|x|
    unpcklps xmm1, xmm1	//|x|x|x|x|
  {$else}
  movss  xmm1,[B]
  shufps xmm1, xmm1, $00 // Replicate B
  maxps  xmm0, xmm1
  movaps [RESULT], xmm0
  {$ENDIF}
end;

function TBZColor32.Max(constref B : TBZColor32) : TBZColor32; assembler; nostackframe; register;
asm
  movaps xmm0,[RCX]
  maxps  xmm0, XMMWORD PTR [B] //xmm1
  movaps [RESULT], xmm0
end;

function TBZColor32.Clamp(constref AMin, AMax : Byte) : TBZColor32; assembler; nostackframe; register;
asm
  movaps xmm0,[RCX]
  movlps xmm2, [AMin]
  movlps xmm3, [AMax]
  shufps xmm2, xmm2, $00
  shufps xmm3, xmm3, $00
  maxps  xmm0, xmm2
  minps  xmm0, xmm3
  movaps [RESULT], xmm0
end;

function TBZColor32.Clamp(Constref AMin, AMax : TBZColor32) : TBZColor32; assembler; nostackframe; register;
asm
  movaps xmm0,[RCX]
  maxps  xmm0, XMMWORD PTR [AMin]//xmm1
  minps  xmm0, XMMWORD PTR [AMax] //xmm2
  movaps [RESULT], xmm0
end;

function TBZColor32.MulAdd(Constref B, C : TBZColor32) : TBZColor32; assembler; nostackframe; register;
asm
  movaps xmm0,[RCX]
  mulps  xmm0, XMMWORD PTR [B] //xmm1
  addps  xmm0, XMMWORD PTR [C] //xmm2
  movaps [RESULT], xmm0
end;

function TBZColor32.MulSub(Constref B, C : TBZColor32) : TBZColor32; assembler; nostackframe; register;
asm
  movaps xmm0,[RCX]
  mulps  xmm0, XMMWORD PTR [B] //xmm1
  Subps  xmm0, XMMWORD PTR [C] //xmm2
  movaps [RESULT], xmm0
end;

function TBZColor32.MulDiv(Constref B, C : TBZColor32) : TBZColor32; assembler; nostackframe; register;
asm
  movaps xmm0,[RCX]
  mulps  xmm0, XMMWORD PTR [B] //xmm1
  Divps  xmm0, XMMWORD PTR [C] //xmm2
  movaps [RESULT], xmm0
end;

function TBZColor32.Lerp(Constref B : TBZColor32; Constref T : Single) : TBZColor32; assembler; nostackframe; register;
asm
  movaps xmm0,[RCX]
  movaps xmm1, XMMWORD PTR [B]
  movlps xmm2,[T]
  shufps xmm2,xmm2,0
  andps xmm2,XMMWORD PTR  [RIP+cSSE_MASK_NO_W]
  subps xmm1,xmm0
  mulps xmm1, xmm2
  addps xmm0, xmm1
  movaps [RESULT], xmm0
end;

function TBZColor32.Combine(constref V2 : TBZColor32; constref F1 : Single) : TBZColor32; assembler; //nostackframe; register;
asm
  movaps xmm0,[RCX]
  movaps xmm1, XMMWORD PTR [V2]
  movlps  xmm2,[F1]
  shufps xmm2, xmm2, $00 // replicate

  mulps xmm1, xmm2 //V2*F1
  addps xmm0, xmm1 // Self + (V2*F1)

  andps xmm0, XMMWORD PTR [RIP+cSSE_MASK_NO_W]
  movaps [RESULT], xmm0
end;

function TBZColor32.Combine2(constref V2 : TBZColor32; const F1, F2 : Single) : TBZColor32; assembler; //nostackframe; register;    ;
asm
  movaps xmm0,[RCX]
  movaps xmm1, XMMWORD PTR [V2]

  movlps xmm2, [F2{%H-}]

  shufps xmm2, xmm2, $00 // replicate
  shufps xmm3, xmm3, $00 // replicate F1 already there

  mulps xmm0, xmm3  // Self * F1
  mulps xmm1, xmm2  // V2 * F2

  addps xmm0, xmm1  // (Self * F1) + (V2 * F2)

  andps xmm0, XMMWORD PTR [RIP+cSSE_MASK_NO_W]
  //movhlps xmm1,xmm0
  movups [RESULT], xmm0

end;

function TBZColor32.Combine3(constref V2, V3 : TBZColor32; const F1, F2, F3 : Single) : TBZColor32;  assembler;//nostackframe; register;
asm
  movaps xmm0,[RCX]

  movaps xmm1, XMMWORD PTR [V2]
  movaps xmm4, XMMWORD PTR [V3]

  movlps xmm2, [F1{%H-}]
  movlps xmm3, [F2{%H-}]
  movlps xmm5, [F3{%H-}]

  shufps xmm2, xmm2, $00 // replicate
  shufps xmm3, xmm3, $00 // replicate
  shufps xmm5, xmm5, $00 // replicate

  mulps xmm0, xmm2 // Self * F1
  mulps xmm1, xmm3 // V2 * F2
  mulps xmm4, xmm5 // V3 * F3

  addps xmm0, xmm1 // (Self * F1) + (V2 * F2)
  addps xmm0, xmm4 // ((Self * F1) + (V2 * F2)) + (V3 * F3)

  andps xmm0, XMMWORD PTR [RIP+cSSE_MASK_NO_W]
  movups [RESULT], xmm0
end; }



{%endregion%}


