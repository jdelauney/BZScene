{%region%----[ Operators ]--------------------------------------------------------}

class operator TBZVector4i.+(constref A, B: TBZVector4i): TBZVector4i; assembler; register;  nostackframe;
asm
   movdqa  xmm0, [A]
  {$ifdef TEST}
    paddd  xmm0, [B]
  {$else}
    movdqa xmm1, [B]
    paddd  xmm0, xmm1
  {$endif}
  movhlps  xmm1, xmm0
  movq     RAX,  xmm0
  movq     RDX,  xmm1
end;

class operator TBZVector4i.-(constref A, B: TBZVector4i): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa   xmm0, [A]
  {$ifdef TEST}
    psubd  xmm0, [B]
  {$else}
    movdqa xmm1, [B]
    psubd  xmm0, xmm1
  {$endif}
  movhlps  xmm1, xmm0
  movq     RAX,  xmm0
  movq     RDX,  xmm1
end;

class operator TBZVector4i.*(constref A, B: TBZVector4i): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa   xmm0, [A]
  movdqa   xmm1, [B]
  cvtdq2ps xmm0, xmm0
  cvtdq2ps xmm1, xmm1
  mulps    xmm0, xmm1
  cvtps2dq xmm0, xmm0
  movhlps  xmm1, xmm0
  movq     RAX,  xmm0
  movq     RDX,  xmm1
end;

class operator TBZVector4i.div(constref A, B: TBZVector4i): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa xmm0,[A]
  movdqa xmm1,[B]
  cvtdq2ps xmm0,xmm0
  cvtdq2ps xmm1,xmm1
  divps  xmm0,xmm1
  cvtps2dq xmm0, xmm0
  movhlps  xmm1, xmm0
  movq     RAX,  xmm0
  movq     RDX,  xmm1
end;

class operator TBZVector4i.+(constref A: TBZVector4i; constref B:Longint): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa   xmm0, [A]
  movd     xmm1, [B]
  pshufd   xmm1, xmm1, $00
  paddd    xmm0, xmm1
  movhlps  xmm1, xmm0
  movq     RAX,  xmm0
  movq     RDX,  xmm1
end;

class operator TBZVector4i.-(constref A: TBZVector4i; constref B:Longint): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa   xmm0, [A]
  movd     xmm1, [B]
  pshufd   xmm1, xmm1, $00
  psubd    xmm0, xmm1
  movhlps  xmm1, xmm0
  movq     RAX,  xmm0
  movq     RDX,  xmm1
end;

class operator TBZVector4i.*(constref A: TBZVector4i; constref B:Single): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa   xmm0, [A]
  cvtdq2ps xmm0, xmm0
  movss    xmm1, [B]
  shufps   xmm1, xmm1, $00
  mulps    xmm0, xmm1
  cvtps2dq xmm0, xmm0
  movhlps  xmm1, xmm0
  movq     RAX,  xmm0
  movq     RDX,  xmm1
end;

class operator TBZVector4i.*(constref A: TBZVector4i; constref B:Longint): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa   xmm0, [A]
  movd     xmm1, [B]
  cvtdq2ps xmm0, xmm0
  cvtdq2ps xmm1, xmm1
  shufps   xmm1, xmm1, $00
  mulps    xmm0, xmm1
  cvtps2dq xmm0, xmm0
  movhlps  xmm1, xmm0
  movq     RAX,  xmm0
  movq     RDX,  xmm1
end;

class operator TBZVector4i.Div(constref A: TBZVector4i; constref B:Longint): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa   xmm0, [A]
  movd     xmm1, [B]
  cvtdq2ps xmm0, xmm0
  cvtdq2ps xmm1, xmm1
  shufps   xmm1, xmm1, $00
  divps    xmm0, xmm1
  cvtps2dq xmm0, xmm0
  movhlps  xmm1, xmm0
  movq     RAX,  xmm0
  movq     RDX,  xmm1
end;

class operator TBZVector4i.-(constref A: TBZVector4i): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa   xmm1, [A]
  movdqa   xmm0, [RIP+cNullVector4i]
  psubd    xmm0, xmm1
  movhlps  xmm1, xmm0
  movq     RAX,  xmm0
  movq     RDX,  xmm1
end;

class operator TBZVector4i.= (constref A, B: TBZVector4i): boolean; assembler; nostackframe; register;
asm
  movdqa   xmm0, [A]
  pcmpeqd  xmm0, [B]
  movmskps eax,  xmm0
  xor      eax,  $F
  setz     al
end;


class operator TBZVector4i.<> (constref A, B: TBZVector4i): boolean; assembler; nostackframe; register;
asm
  movdqa   xmm0, [A]
  pcmpeqd  xmm0, [B]
  movmskps eax,  xmm0
  movmskps eax,  xmm0
  or       eax,  $f
  setz     al
end;

{%endregion%}

{%region%----[ Functions ]--------------------------------------------------------}

function TBZVector4i.Abs: TBZVector4i; assembler; nostackframe; register;
asm
  movdqa   xmm0, [RDI]
  pand     xmm0, [RIP+cSSE_MASK_ABS]
  movhlps  xmm1, xmm0
  movq     RAX,  xmm0
  movq     RDX,  xmm1
end;

function TBZVector4i.DivideBy2:TBZVector4i;assembler; nostackframe; register;
asm
  movdqa   xmm0, [RDI]
  psrld    xmm0, 1
  movhlps  xmm1, xmm0
  movq     RAX,  xmm0
  movq     RDX,  xmm1
end;

function TBZVector4i.Min(constref B: Longint): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa   xmm0, [RDI]
  movd     xmm1, [B]
  cvtdq2ps xmm0, xmm0
  cvtdq2ps xmm1, xmm1
  shufps   xmm1, xmm1, $00
  minps    xmm0, xmm1
  cvtps2dq xmm0, xmm0
  movhlps  xmm1, xmm0
  movq     RAX,  xmm0
  movq     RDX,  xmm1
end;

function TBZVector4i.Min(constref B: TBZVector4i): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa   xmm0, [RDI]
  movdqa   xmm1, [B]
  cvtdq2ps xmm0, xmm0
  cvtdq2ps xmm1, xmm1
  minps    xmm0, xmm1
  cvtps2dq xmm0, xmm0
  movhlps  xmm1, xmm0
  movq     RAX,  xmm0
  movq     RDX,  xmm1
end;

function TBZVector4i.Max(constref B: Longint): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa   xmm0, [RDI]
  movd     xmm1, [B]
  cvtdq2ps xmm0, xmm0
  cvtdq2ps xmm1, xmm1
  shufps   xmm1, xmm1, $00
  maxps    xmm0, xmm1
  cvtps2dq xmm0, xmm0
  movhlps  xmm1, xmm0
  movq     RAX,  xmm0
  movq     RDX,  xmm1
end;

function TBZVector4i.Max(constref B: TBZVector4i): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa   xmm0, [RDI]
  movdqa   xmm1, [B]
  cvtdq2ps xmm0, xmm0
  cvtdq2ps xmm1, xmm1
  maxps    xmm0, xmm1
  cvtps2dq xmm0, xmm0
  movhlps  xmm1, xmm0
  movq     RAX,  xmm0
  movq     RDX,  xmm1
end;

function TBZVector4i.Clamp(constref AMin, AMax: Longint): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa    xmm0, [RDI]
  cvtdq2ps  xmm0, xmm0
  {$IFDEF TEST}
  movd      xmm1, [AMin] //|0|0|0|x|
  punpckldq xmm1, xmm1	 //|0|0|x|x|
  punpckldq xmm1, xmm1	 //|x|x|x|x|
  movd      xmm2, [AMax]
  punpckldq xmm2, xmm2
  punpckldq xmm2, xmm2
  cvtdq2ps  xmm1, xmm1
  cvtdq2ps  xmm2, xmm2
  {$else}
  movd      xmm1, [AMin]
  cvtdq2ps  xmm1, xmm1
  shufps    xmm1, xmm1, $00
  movd      xmm2, [AMax]
  cvtdq2ps  xmm2, xmm2
  shufps    xmm2, xmm2, $00
  {$endif}
  maxps     xmm0, xmm1
  minps     xmm0, xmm2
  cvtps2dq  xmm0, xmm0
  movhlps   xmm1, xmm0
  movq      RAX,  xmm0
  movq      RDX,  xmm1
end;

function TBZVector4i.Clamp(Constref AMin, AMax: TBZVector4i): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa   xmm0, [RDI]
  movdqa   xmm1, [AMin]
  movdqa   xmm2, [AMax]
  cvtdq2ps xmm0, xmm0
  cvtdq2ps xmm1, xmm1
  cvtdq2ps xmm2, xmm2
  maxps    xmm0, xmm1
  minps    xmm0, xmm2
  cvtps2dq xmm0, xmm0
  movhlps  xmm1, xmm0
  movq     RAX,  xmm0
  movq     RDX,  xmm1
end;

function TBZVector4i.MulAdd(Constref B, C: TBZVector4i): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa   xmm0, [RDI]
  movdqa   xmm1, [B]
  movdqa   xmm2, [C]
  cvtdq2ps xmm0, xmm0
  cvtdq2ps xmm1, xmm1
  cvtdq2ps xmm2, xmm2
  mulps    xmm0, xmm1
  addps    xmm0, xmm2
  cvtps2dq xmm0, xmm0
  movhlps  xmm1, xmm0
  movq     RAX,  xmm0
  movq     RDX,  xmm1
end;

function TBZVector4i.MulDiv(Constref B, C: TBZVector4i): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa   xmm0, [RDI]
  movdqa   xmm1, [B]
  movdqa   xmm2, [C]
  cvtdq2ps xmm0, xmm0
  cvtdq2ps xmm1, xmm1
  cvtdq2ps xmm2, xmm2
  mulps    xmm0, xmm1
  divps    xmm0, xmm2
  push     rax                               // create a mem area for mxcsr
  stmxcsr  DWORD PTR [rsp]                   //  store mxcsr
  mov      eax, DWORD PTR [rsp]              // get it
  mov      ecx, eax                          // make local copy   self is done with at this point
  and      eax,  $00009FFF
  or       eax,  $00006000                   // Set bits
  mov      DWORD PTR [rsp], eax              // mov bits to mem
  ldmxcsr  DWORD PTR [rsp]                   // set new bits
  cvtps2dq xmm0, xmm0
  mov      DWORD PTR [rsp], ecx              // put old bits in mem
  ldmxcsr  DWORD PTR [rsp]                   // Pop rounding mode
  pop      rax                               // free used stack
  movhlps  xmm1, xmm0
  movq     RAX,  xmm0
  movq     RDX,  xmm1
end;

function TBZVector4i.Combine(constref V2: TBZVector4i; constref F1: Single): TBZVector4i;assembler; nostackframe; register;
asm
  movdqa   xmm0, [RDI]
  movdqa   xmm1, [V2]
  cvtdq2ps xmm0, xmm0
  movlps   xmm2, [F1]
  shufps xmm2, xmm2, $00                     // replicate

  andps    xmm0, [RIP+cSSE_MASK_NO_W]
  mulps    xmm1, xmm2                        //V2*F1

  push     rax                               // create a mem area for mxcsr
  stmxcsr  DWORD PTR [rsp]                   //  store mxcsr
  mov      eax,  DWORD PTR [rsp]             // get it
  mov      ecx,  eax                         // make local copy
  and      eax,  $00009FFF
  or       eax,  $00006000                   // Set bits
  mov      DWORD PTR [rsp], eax              // mov bits to mem
  ldmxcsr  DWORD PTR [rsp]                   // set new bits
  cvtps2dq xmm0, xmm0
  mov      DWORD PTR [rsp], ecx              // put old bits in mem
  ldmxcsr  DWORD PTR [rsp]                   // Pop rounding mode
  pop      rax                               // free used stack

  paddd    xmm0, xmm1                        // Self + (V2*F1)

  movhlps  xmm1, xmm0
  movq     RAX,  xmm0
  movq     RDX,  xmm1
end;

function TBZVector4i.Combine2(constref V2: TBZVector4i; const F1, F2: Single): TBZVector4i;assembler; nostackframe; register;
asm
  movdqa   xmm2, [RDI]     // get in one hit V1
  movdqa   xmm3, [V2]      // get in one hit V2
  cvtdq2ps xmm2, xmm2
  cvtdq2ps xmm3, xmm3

  shufps   xmm0, xmm0, $00 // replicate  F1
  shufps   xmm1, xmm1, $00 // replicate  F2

  mulps    xmm0, xmm2      // Self * F1
  mulps    xmm1, xmm3      // V2 * F2

  addps    xmm0, xmm1      // (Self * F1) + (V2 * F2)

  andps    xmm0, [RIP+cSSE_MASK_NO_W]

  cvtps2dq xmm0, xmm0
  movhlps  xmm1, xmm0
  movq     RAX,  xmm0
  movq     RDX,  xmm1
end;

function TBZVector4i.Combine3(constref V2, V3: TBZVector4i; const F1, F2, F3: Single): TBZVector4i;  assembler; nostackframe; register;
asm
  movdqa   xmm3, [RDI]
  movdqa   xmm4, [V2]
  movdqa   xmm5, [V3]
  cvtdq2ps xmm3, xmm3
  cvtdq2ps xmm4, xmm4
  cvtdq2ps xmm5, xmm5

  shufps   xmm0, xmm0, $00 // replicate  F1
  shufps   xmm1, xmm1, $00 // replicate  F2
  shufps   xmm2, xmm2, $00 // replicate  F3

  mulps    xmm0, xmm3      // Self * F1
  mulps    xmm4, xmm1      // V2 * F2
  mulps    xmm5, xmm2      // V3 * F3

  addps    xmm0, xmm4      // (Self * F1) + (V2 * F2)
  addps    xmm0, xmm5      // ((Self * F1) + (V2 * F2)) + (V3 * F3)

  andps    xmm0, [RIP+cSSE_MASK_NO_W]
  cvtps2dq xmm0, xmm0
  movhlps  xmm1, xmm0
  movq     RAX,  xmm0
  movq     RDX,  xmm1
end;


{%endregion%}

{%region%----[ Operations on Self ]-----------------------------------------------}



{%endregion%}
