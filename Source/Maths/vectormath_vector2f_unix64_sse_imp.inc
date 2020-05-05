{%region%----[ Operators ]--------------------------------------------------------}

class operator TBZVector2f.+(constref A, B: TBZVector2f): TBZVector2f; assembler; nostackframe; register;
asm
  movq  xmm0, [A]
  movq  xmm1, [B]
  addps xmm0, xmm1
end;

class operator TBZVector2f.-(constref A, B: TBZVector2f): TBZVector2f; assembler; nostackframe; register;
asm
  movq  xmm0, [A]
  movq  xmm1, [B]
  subps xmm0, xmm1
end;

class operator TBZVector2f.*(constref A, B: TBZVector2f): TBZVector2f; assembler; nostackframe; register;
asm
  movq  xmm0, [A]
  movq  xmm1, [B]
  mulps xmm0, xmm1
end;

class operator TBZVector2f./(constref A, B: TBZVector2f): TBZVector2f; assembler; nostackframe; register;
asm
  movq    xmm0, [A]
  movq    xmm1, [B]
  movlhps xmm1, xmm1 // remove 0 from high
  divps   xmm0, xmm1
end;

class operator TBZVector2f.+(constref A: TBZVector2f; constref B:Single): TBZVector2f; assembler; nostackframe; register;
asm
  movq   xmm0, [A]
  movq   xmm1, [B]
  shufps xmm1, xmm1, $00
  addps  xmm0, xmm1
end;

class operator TBZVector2f.-(constref A: TBZVector2f; constref B:Single): TBZVector2f; assembler; nostackframe; register;
asm
  movq   xmm0, [A]
  movq   xmm1, [B]
  shufps xmm1, xmm1, $00
  subps  xmm0, xmm1
end;

class operator TBZVector2f.*(constref A: TBZVector2f; constref B:Single): TBZVector2f; assembler; nostackframe; register;
asm
  movq   xmm0, [A]
  movq   xmm1, [B]
  shufps xmm1, xmm1, $00
  mulps  xmm0, xmm1
end;

class operator TBZVector2f.*(constref A:TBZVector2f; Constref B: TBZVector2i): TBZVector2f; assembler; nostackframe; register;
asm
  movq     xmm0, [A]
  cvtpi2ps xmm2, [B]
  mulps    xmm0, xmm2
end;

class operator TBZVector2f./(constref A: TBZVector2f; constref B:Single): TBZVector2f; assembler; nostackframe; register;
asm
  movq   xmm0, [A]
  movq   xmm1, [B]
  shufps xmm1, xmm1, $00
  divps  xmm0, xmm1
end;


class operator TBZVector2f./(constref A: TBZVector2f; constref B: TBZVector2i): TBZVector2f;  assembler; nostackframe; register;
asm
  movq     xmm0, [A]
  cvtpi2ps xmm1, [B]
  divps    xmm0, xmm1
end;

class operator TBZVector2f.+(constref A: TBZVector2f; constref B: TBZVector2i): TBZVector2f; assembler; nostackframe; register;
asm
  movq   xmm0, [A]
  cvtdq2ps   xmm1, [B]
  addps  xmm0, xmm1
end;

class operator TBZVector2f.-(constref A: TBZVector2f; constref B: TBZVector2i): TBZVector2f; assembler; nostackframe; register;
asm
  movq   xmm0, [A]
  cvtdq2ps   xmm1, [B]
  subps  xmm0, xmm1
end;

{%endregion%}

{%region%----[ Functions ]--------------------------------------------------------}

function TBZVector2f.Min(constref B: Single): TBZVector2f; assembler; nostackframe; register;
asm
  movq   xmm0, [RDI]
  movq   xmm1, [B]
  shufps xmm1, xmm1, $00
  minps  xmm0, xmm1
end;

function TBZVector2f.Min(constref B: TBZVector2f): TBZVector2f; assembler; nostackframe; register;
asm
  movq   xmm0, [RDI]
  movq   xmm1, [B]
  minps  xmm0, xmm1
end;


function TBZVector2f.Max(constref B: Single): TBZVector2f;Assembler; nostackframe;register;
asm
  movq   xmm0, [RDI]
  movq   xmm1, [B]
  shufps xmm1, xmm1, $00
  maxps  xmm0, xmm1
end;

function TBZVector2f.Max(constref B: TBZVector2f): TBZVector2f;Assembler; nostackframe;register;
asm
  movq   xmm0, [RDI]
  movq   xmm1, [B]
  maxps  xmm0, xmm1
end;

function TBZVector2f.Clamp(constref AMin, AMax: Single): TBZVector2f;Assembler; nostackframe;register;
asm
  movq   xmm0, [RDI]
  movq   xmm1, [AMin]
  movq   xmm2, [AMax]
  shufps xmm1, xmm1, $00 // Replicate AMin
  shufps xmm2, xmm2, $00 // Replicate AMax
  maxps  xmm0, xmm1
  minps  xmm0, xmm2
end;

function TBZVector2f.Clamp(constref AMin, AMax: TBZVector2f): TBZVector2f;Assembler; nostackframe;register;
asm
  movq   xmm0, [RDI]
  movq   xmm1, [AMin]
  movq   xmm2, [AMax]
  maxps  xmm0, xmm1
  minps  xmm0, xmm2
end;

function TBZVector2f.MulAdd(constref A,B:TBZVector2f): TBZVector2f;Assembler; nostackframe; register;
asm
  movq   xmm0, [RDI]
  movq   xmm1, [A]
  movq   xmm2, [B]
  Mulps  xmm0, xmm1
  Addps  xmm0, xmm2
end;

function TBZVector2f.MulSub(constref A,B:TBZVector2f): TBZVector2f;Assembler; nostackframe;register;
asm
  movq   xmm0, [RDI]
  movq   xmm1, [A]
  movq   xmm2, [B]
  Mulps  xmm0, xmm1
  Subps  xmm0, xmm2
end;


function TBZVector2f.MulDiv(constref A,B:TBZVector2f): TBZVector2f;Assembler; nostackframe; register;
asm
  movq   xmm0, [RDI]
  movq   xmm1, [A]
  movq   xmm2, [B]
  Mulps  xmm0, xmm1
  Divps  xmm0, xmm2
end;

function TBZVector2f.Length:Single;Assembler;nostackframe;register;
asm
  movq     xmm0, [RDI]
  mulps    xmm0, xmm0
  movshdup xmm1,xmm0
  addps    xmm0, xmm1
  sqrtss   xmm0, xmm0
end;

function TBZVector2f.LengthSquare:Single;Assembler;nostackframe;register;
asm
  movq     xmm0, [RDI]
  mulps    xmm0, xmm0
  movshdup xmm1, xmm0
  addps    xmm0, xmm1
end;

function TBZVector2f.Distance(constref A:TBZVector2f):Single;Assembler; nostackframe;register;
asm
  movq   xmm0, [RDI]
  movq   xmm1, [A]
  subps  xmm0, xmm1
  mulps  xmm0, xmm0
  //haddps xmm0, xmm0
  movshdup xmm1, xmm0
  addps    xmm0, xmm1
  sqrtss xmm0, xmm0
end;

function TBZVector2f.DistanceSquare(Constref A:TBZVector2f):Single;Assembler; nostackframe;register;
asm
  movq   xmm0, [RDI]
  movq   xmm1, [A]
  subps  xmm0, xmm1
  mulps  xmm0, xmm0
  //haddps xmm0, xmm0
  movshdup xmm1, xmm0
  addps    xmm0, xmm1
end;

function TBZVector2f.Normalize : TBZVector2f;Assembler;nostackframe;register;
asm
  movq   xmm2, [RDI]    // Y X
  movaps xmm0, xmm2
  mulps  xmm2, xmm2      // Y*Y X*X
  pshufd xmm1, xmm2, $01 // X*X Y*Y
  addps  xmm2, xmm1      // (X*X+Y*Y) (2x)
  sqrtps xmm2, xmm2
  divps  xmm0, xmm2
end;

function TBZVector2f.Trunc: TBZVector2i;assembler; nostackframe; register;
asm
  push     rax                               // create a mem area for mxcsr
  stmxcsr  DWORD PTR [rsp]                   //  store mxcsr
  mov      eax,  DWORD PTR [rsp]              // get it
  mov      ecx,  eax                          // make local copy
  and      eax,  $00009FFF
  or       eax,  $00006000                   // Set bits
  mov      DWORD PTR [rsp], eax              // mov bits to mem
  ldmxcsr  DWORD PTR [rsp]                   // set new bits
  movq     xmm0, [RDI]
  cvtps2dq xmm0, xmm0
  mov      DWORD PTR [rsp], ecx              // put old bits in mem
  ldmxcsr  DWORD PTR [rsp]                   // Pop rounding mode
  pop      rax                               // free used stack
  movq     rax,  xmm0
end;

function TBZVector2f.Abs:TBZvector2f;assembler; nostackframe; register;
asm
  movq     xmm0, [RDI]
  andps    xmm0, XMMWORD PTR [RIP+cSSE_MASK_ABS]
end;

function TBZVector2f.Round: TBZVector2i;assembler; nostackframe; register;
asm
  movq     xmm0, [RDI]
  cvtps2dq xmm0, xmm0
  movq     rax,  xmm0
end;

function TBZVector2f.Floor: TBZVector2i; assembler; nostackframe; register;
asm
  movq     xmm0, [RDI]
  push     rax                               // create a mem area for mxcsr
  stmxcsr  DWORD PTR [rsp]                   //  store mxcsr
  mov      eax, DWORD PTR [rsp]              // get it
  mov      ecx, eax                          // make local copy   self is done with at this point
  // smaller opcode no mem access required.
  and      eax, $00009FFF
  or       eax, $00002000;                    // Set bits Round Mask Down
  //{$endif}
  mov      DWORD PTR [rsp], eax              // mov bits to mem
  ldmxcsr  DWORD PTR [rsp]                   // set new bits
  cvtps2dq xmm0, xmm0
  mov      DWORD PTR [rsp], ecx              // put old bits in mem
  ldmxcsr  DWORD PTR [rsp]                   // Pop rounding mode
  pop      rax
  movq     rax, xmm0
end;

function TBZVector2f.Ceil : TBZVector2i; assembler; nostackframe; register;
asm
  movq     xmm0, [RDI]
  push     rax                               // create a mem area for mxcsr
  stmxcsr  DWORD PTR [rsp]                   //  store mxcsr
  mov      eax,  DWORD PTR [rsp]              // get it
  mov      ecx,  eax                          // make local copy   self is done with at this point
  // smaller opcode no mem access required.
  and      eax, $00009FFF
  or       eax, $00004000;                    // Set bits Round Mask up
  //{$endif}
  mov      DWORD PTR [rsp], eax              // mov bits to mem
  ldmxcsr  DWORD PTR [rsp]                   // set new bits
  cvtps2dq xmm0, xmm0
  mov      DWORD PTR [rsp], ecx              // put old bits in mem
  ldmxcsr  DWORD PTR [rsp]                   // Pop rounding mode
  pop      rax
  movq     rax, xmm0
end;

function TBZVector2f.Fract : TBZVector2f; assembler; nostackframe; register;
asm
  movq     xmm0, [RDI]
  movaps   xmm1, xmm0
  push     rax                               // create a mem area for mxcsr
  stmxcsr  DWORD PTR [rsp]                   //  store mxcsr
  mov      eax,  DWORD PTR [rsp]              // get it
  mov      ecx,  eax                          // make local copy   self is done with at this point
  // smaller opcode no mem access required.
  and      eax, $00009FFF
  or       eax, $00006000                    // Set bits
  mov      DWORD PTR [rsp], eax              // mov bits to mem
  ldmxcsr  DWORD PTR [rsp]                   // set new bits
  cvtps2dq xmm0, xmm0
  mov      DWORD PTR [rsp], ecx              // put old bits in mem
  ldmxcsr  DWORD PTR [rsp]                   // Pop rounding mode
  pop      rax
  cvtdq2ps xmm0, xmm0
  subps    xmm1, xmm0 // A - Trunc(A)
end;

function TBZVector2f.Sqrt : TBZVector2f; overload; assembler; nostackframe; register;
asm
  movq   xmm0, [RDI]
  sqrtps xmm0, xmm0
end;

function TBZVector2f.InvSqrt : TBZVector2f; overload; assembler; nostackframe; register;
asm
  movq    xmm0,[RDI]
  rsqrtps xmm0, xmm0
end;

function TBZVector2f.Modf(constref A : TBZVector2f): TBZVector2f; assembler; nostackframe; register;
asm
  movq     xmm0, [RDI]
  movaps   xmm1, xmm0
  movq     xmm2, [A]
  divps    xmm1, xmm2
  push     rax                               // create a mem area for mxcsr
  stmxcsr  DWORD PTR [rsp]                   //  store mxcsr
  mov      eax, DWORD PTR [rsp]              // get it
  mov      ecx, eax                          // make local copy   self is done with at this point
  // smaller opcode no mem access required.
  and      eax, $00009FFF
  or       eax, $00006000                    // Set bits
  mov      DWORD PTR [rsp], eax              // mov bits to mem
  ldmxcsr  DWORD PTR [rsp]                   // set new bits
  cvtps2dq xmm1, xmm1
  mov      DWORD PTR [rsp], ecx              // put old bits in mem
  ldmxcsr  DWORD PTR [rsp]                   // Pop rounding mode
  pop      rax
  cvtdq2ps xmm1, xmm1
  mulps    xmm1, xmm2
  subps    xmm0, xmm1 // A - Trunc(A)
end;

function TBZVector2f.fMod(Constref A : TBZVector2f): TBZVector2i; assembler; nostackframe; register;
asm
  movq     xmm0, [RDI]
  movaps   xmm1, xmm0
  movq     xmm2, {%H-}[A]

  divps    xmm0, xmm2
  push     rax                               // create a mem area for mxcsr
  stmxcsr  DWORD PTR [rsp]                   //  store mxcsr
  mov      eax, DWORD PTR [rsp]              // get it
  mov      ecx, eax                          // make local copy   self is done with at this point
  // smaller opcode no mem access required.
  and      eax, $00009FFF
  or       eax, $00002000                    // Set bits
  mov      DWORD PTR [rsp], eax              // mov bits to mem
  ldmxcsr  DWORD PTR [rsp]                   // set new bits
  cvtps2dq xmm0, xmm0
  mov      DWORD PTR [rsp], ecx              // put old bits in mem
  ldmxcsr  DWORD PTR [rsp]                   // Pop rounding mode
  pop      rax

  cvtdq2ps xmm0, xmm0
  mulps    xmm0, xmm2
  subps    xmm1, xmm0 // A - Trunc(A)

  push     rax                               // create a mem area for mxcsr
  stmxcsr  DWORD PTR [rsp]                   //  store mxcsr
  mov      eax, DWORD PTR [rsp]              // get it
  mov      ecx, eax                          // make local copy   self is done with at this point
  // smaller opcode no mem access required.
  and      eax, $00009FFF
  or       eax, $00006000                   // Set bits
  mov      DWORD PTR [rsp], eax              // mov bits to mem
  ldmxcsr  DWORD PTR [rsp]                   // set new bits
  cvtps2dq xmm1, xmm1
  mov      DWORD PTR [rsp], ecx              // put old bits in mem
  ldmxcsr  DWORD PTR [rsp]                   // Pop rounding mode
  pop      rax

  movq rax, xmm1
end;



{%endregion%}
