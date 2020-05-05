{%region%----[ Operators ]--------------------------------------------------------}

class operator TBZColorVector.+(constref A, B: TBZColorVector): TBZColorVector; assembler; nostackframe; register;
asm
  movaps xmm0, XMMWORD PTR [A]
  addps  xmm0, XMMWORD PTR [B]
  movaps [RESULT], xmm0
end;

class operator TBZColorVector.-(constref A, B: TBZColorVector): TBZColorVector; assembler; nostackframe; register;
asm
  movaps xmm0, XMMWORD PTR [A]
  subps  xmm0, XMMWORD PTR [B]
  movaps [RESULT], xmm0
end;

class operator TBZColorVector.*(constref A, B: TBZColorVector): TBZColorVector; assembler; nostackframe; register;
asm
  movaps xmm0, XMMWORD PTR [A]
  mulps  xmm0, XMMWORD PTR [B]
  movaps [RESULT], xmm0
end;

class operator TBZColorVector./(constref A, B: TBZColorVector): TBZColorVector; assembler; nostackframe; register;
asm
  movaps xmm0, XMMWORD PTR [A]
  {$ifdef USE_ASM_SIMD_HIGHPRECISION}
     divps  xmm0, XMMWORD PTR [B]
  {$else}
    movaps xmm1,XMMWORD PTR [B]
    rcpps  xmm1, xmm1
    mulps  xmm0,xmm1
  {$endif}
movaps [RESULT], xmm0
end;

class operator TBZColorVector.+(constref A: TBZColorVector; constref B:Single): TBZColorVector; assembler; nostackframe; register;
asm
  movaps xmm0, XMMWORD PTR [A]
  movlps  xmm1,[B]
  shufps xmm1, xmm1, $00
  addps  xmm0,xmm1
  movaps [RESULT], xmm0
end;

class operator TBZColorVector.-(constref A: TBZColorVector; constref B:Single): TBZColorVector; assembler; nostackframe; register;
asm
  movaps xmm0,XMMWORD PTR [A]
  {$ifdef TEST}
    movlps  xmm1,[B]
  {$else}
    movss  xmm1,[B]
  {$endif}
  shufps xmm1, xmm1, $00
  subps  xmm0, xmm1
  movaps [RESULT], xmm0
end;

class operator TBZColorVector.*(constref A: TBZColorVector; constref B:Single): TBZColorVector; assembler; nostackframe; register;
asm
  movaps xmm0, [A]
  movlps  xmm1,[B]
  shufps xmm1, xmm1, $00
  mulps  xmm0, xmm1
  movaps [RESULT], xmm0
end;

class operator TBZColorVector./(constref A: TBZColorVector; constref B:Single): TBZColorVector; assembler; nostackframe; register;
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


class operator TBZColorVector.= (constref A, B: TBZColorVector): boolean; assembler; nostackframe; register;
asm
  movaps xmm0, XMMWORD PTR [A]
  movaps xmm1, XMMWORD PTR [B]
  cmpps  xmm0, xmm1, cSSE_OPERATOR_EQUAL    //  Yes: $FFFFFFFF, No: $00000000 ; 0 = Operator Equal
  movmskps eax, xmm0
  xor eax, $F
  setz al
end;

class operator TBZColorVector.<= (constref A, B: TBZColorVector): boolean; assembler; nostackframe; register;
asm
  movaps xmm0, XMMWORD PTR [A]
  movaps xmm1, XMMWORD PTR [B]
  cmpps  xmm0, xmm1, cSSE_OPERATOR_LESS_OR_EQUAL    //  Yes: $FFFFFFFF, No: $00000000 ; 2 = Operator Less or Equal
  movmskps eax, xmm0
  xor eax, $f
  setz al
end;

class operator TBZColorVector.>= (constref A, B: TBZColorVector): boolean; assembler; nostackframe; register;
asm
  movaps xmm0, XMMWORD PTR [A]
  movaps xmm1, XMMWORD PTR [B]
  cmpps  xmm0, xmm1, cSSE_OPERATOR_NOT_LESS     //  Yes: $FFFFFFFF, No: $00000000 ; 6 = Operator Not Less Or Equal
  movmskps eax, xmm0
  xor eax, $f
  setz al
end;

class operator TBZColorVector.< (constref A, B: TBZColorVector): boolean; assembler; nostackframe; register;
asm
  movaps xmm0, XMMWORD PTR [A]
  movaps xmm1, XMMWORD PTR [B]
  cmpps  xmm0, xmm1, cSSE_OPERATOR_LESS    //  Yes: $FFFFFFFF, No: $00000000 ; 1 = Operator Less Than
  movmskps eax, xmm0
  xor eax, $f
  setz al
end;

class operator TBZColorVector.>(constref A, B: TBZColorVector): boolean; assembler; nostackframe; register;
asm
  movaps xmm0, XMMWORD PTR [A]
  movaps xmm1, XMMWORD PTR [B]
  cmpps  xmm0, xmm1,  cSSE_OPERATOR_NOT_LESS_OR_EQUAL   //  Yes: $FFFFFFFF, No: $00000000 ; 5 = Operator Not Less Than
  movmskps eax, xmm0
  xor eax, $f
  setz al
end;

class operator TBZColorVector.<> (constref A, B: TBZColorVector): boolean; assembler; nostackframe; register;
asm
  movaps xmm0, XMMWORD PTR [A]
  movaps xmm1, XMMWORD PTR [B]
  cmpps  xmm0, xmm1, cSSE_OPERATOR_NOT_EQUAL    //  Yes: $FFFFFFFF, No: $00000000 ; 4 = Operator Not Equal
  movmskps eax, xmm0
  or eax, eax
  setnz al
end;

class operator TBZColorVector.xor(constref A, B: TBZColorVector): TBZColorVector; assembler; nostackframe; register;
asm
  movaps xmm0, XMMWORD PTR [A]
  xorps  xmm0, XMMWORD PTR [B]
  movaps [RESULT], xmm0
end;

function TBZColorVector.Abs: TBZColorVector; assembler; nostackframe; register;
asm
  movaps xmm0,[RCX]
  andps    xmm0,  XMMWORD PTR [RIP+cSSE_MASK_ABS]
  movaps   [RESULT], xmm0
end;


{%endregion%}

{%region%----[ Functions ]--------------------------------------------------------}

function TBZColorVector.DivideBy2 : TBZColorVector;  assembler; nostackframe; register;
asm
  movaps xmm0,[RCX]
  mulps xmm0, XMMWORD PTR [RIP+cHalfOneVector4f]
  movaps [RESULT],xmm0
end;


function TBZColorVector.Min(constref B : Single) : TBZColorVector; assembler; nostackframe; register;
asm
  movaps xmm0,[RCX]
  //  movlps  xmm1,[B]
  movss  xmm1,[B]
  shufps xmm1, xmm1, $00 // Replicate B
  minps  xmm0, xmm1
  movaps [RESULT], xmm0
end;

function TBZColorVector.Min(constref B : TBZColorVector) : TBZColorVector; assembler; nostackframe; register;
asm
  movaps xmm0,[RCX]
  minps  xmm0, XMMWORD PTR [B]// xmm1
  movaps [RESULT], xmm0
end;

function TBZColorVector.Max(constref B : Single) : TBZColorVector; assembler; nostackframe; register;
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

function TBZColorVector.Max(constref B : TBZColorVector) : TBZColorVector; assembler; nostackframe; register;
asm
  movaps xmm0,[RCX]
  maxps  xmm0, XMMWORD PTR [B] //xmm1
  movaps [RESULT], xmm0
end;

function TBZColorVector.Clamp(constref AMin, AMax : Single) : TBZColorVector; assembler; nostackframe; register;
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

function TBZColorVector.Clamp(Constref AMin, AMax : TBZColorVector) : TBZColorVector; assembler; nostackframe; register;
asm
  movaps xmm0,[RCX]
  maxps  xmm0, XMMWORD PTR [AMin]//xmm1
  minps  xmm0, XMMWORD PTR [AMax] //xmm2
  movaps [RESULT], xmm0
end;

function TBZColorVector.MulAdd(Constref B, C : TBZColorVector) : TBZColorVector; assembler; nostackframe; register;
asm
  movaps xmm0,[RCX]
  mulps  xmm0, XMMWORD PTR [B] //xmm1
  addps  xmm0, XMMWORD PTR [C] //xmm2
  movaps [RESULT], xmm0
end;

function TBZColorVector.MulSub(Constref B, C : TBZColorVector) : TBZColorVector; assembler; nostackframe; register;
asm
  movaps xmm0,[RCX]
  mulps  xmm0, XMMWORD PTR [B] //xmm1
  Subps  xmm0, XMMWORD PTR [C] //xmm2
  movaps [RESULT], xmm0
end;

function TBZColorVector.MulDiv(Constref B, C : TBZColorVector) : TBZColorVector; assembler; nostackframe; register;
asm
  movaps xmm0,[RCX]
  mulps  xmm0, XMMWORD PTR [B] //xmm1
  Divps  xmm0, XMMWORD PTR [C] //xmm2
  movaps [RESULT], xmm0
end;

function TBZColorVector.Lerp(Constref B : TBZColorVector; Constref T : Single) : TBZColorVector; assembler; nostackframe; register;
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

function TBZColorVector.Combine(constref V2 : TBZColorVector; constref F1 : Single) : TBZColorVector; assembler; //nostackframe; register;
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

function TBZColorVector.Combine2(constref V2 : TBZColorVector; const F1, F2 : Single) : TBZColorVector; assembler; //nostackframe; register;    ;
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

function TBZColorVector.Combine3(constref V2, V3 : TBZColorVector; const F1, F2, F3 : Single) : TBZColorVector;  assembler;//nostackframe; register;
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
end;

(* function TBZColorVector.DotProduct(constref A: TBZColorVector):Single;assembler; nostackframe; register;
asm
  {$IFDEF USE_ASM_SSE_4}
    movaps xmm0,[RCX]
    movaps xmm1,XMMWORD PTR [A]
     // Becarefull at the order of mask Right to Left
     // 4 high bits: which elements should be summed. (w,z,y,x)
     // 4 low bits: which output slots should contain the result. (3,2,1,0)
     // mask =  0111b 0001;
     dpps xmm0, xmm1, 01110001b //or $F1
     //movss [RESULT], {%H-}xmm0
  {$ELSE}
    {$IFDEF USE_ASM_SSE_3}
       movaps xmm1,[RCX]
       movaps xmm0,XMMWORD PTR [A]
       andps xmm1, XMMWORD PTR [RIP+cSSE_MASK_NO_W]
       mulps xmm0, xmm1
       movshdup    xmm1, xmm0
       addps       xmm0, xmm1
       movhlps     xmm1, xmm0
       addss       xmm0, xmm1
    {$ELSE}
      movaps xmm0,[RCX]
      movaps xmm1, [A]
      andps xmm0, [RIP+cSSE_MASK_NO_W]
      mulps  xmm0, xmm1             //   xmm0 =  w | z  | y  | x  |
      movhlps xmm1, xmm0            //   xmm1 =  - | -  | w  | z  |
      addps xmm1, xmm0              // x + z
      // How to with shufps
      //  In xmm0 =  w | z  | y  | x  |
      //          = 11 | 10 | 01 | 00 |
      // Out xmm0 =  - | -  | -  | Y  |
      //          = 00 | 00 | 00 | 01 | ==> 00000001b
      shufps xmm0, xmm0, 00000001b  //   xmm1 =  - | -  | -  | y  |
      addps xmm0, xmm1              // (x + z ) + y
      // movss [RESULT], {%H-}xmm0
    {$ENDIF}
  {$ENDIF}
end; *)

function TBZColorVector.Round : TBZVector4i; assembler; nostackframe; register;
asm
  // Rounding mode defaults to round-to-nearest
  cvtps2dq xmm0, XMMWORD PTR [RCX]//xmm0
  movaps     [RESULT], xmm0
end;


{%endregion%}


