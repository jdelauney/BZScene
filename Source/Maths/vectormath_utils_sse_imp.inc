{%region%----[ SSE Utils ]------------------------------------------------------}

function Trunc(Constref v:TBZVector2f): TBZVector2i;assembler; //nostackframe; register;
asm
  push     rax                               // create a mem area for mxcsr
  stmxcsr  DWORD PTR [rsp]                   //  store mxcsr
  mov      eax,  DWORD PTR [rsp]              // get it
  mov      ecx,  eax                          // make local copy
  // smaller opcode no mem access required.
  and      eax,  $00009FFF
  or       eax,  $00006000                   // Set bits
  mov      DWORD PTR [rsp], eax              // mov bits to mem
  ldmxcsr  DWORD PTR [rsp]                   // set new bits
  movq     xmm0, {%H-}[V]
  cvtps2dq xmm0, xmm0
  mov      DWORD PTR [rsp], ecx              // put old bits in mem
  ldmxcsr  DWORD PTR [rsp]                   // Pop rounding mode
  pop      rax                               // free used stack
  movq     rax,  xmm0
end;

function Round(Constref V : TBZVector2f): TBZVector2i;assembler; //nostackframe; register;
asm
  movq     xmm0, {%H-}[V]
  cvtps2dq xmm0, xmm0
  movq     rax,  xmm0
end;

function Floor(Constref V : TBZVector2f) : TBZVector2i; assembler;//nostackframe;register;
asm
  movq     xmm0, {%H-}[V]
  push     rax                               // create a mem area for mxcsr
  stmxcsr  DWORD PTR [rsp]                   //  store mxcsr
  mov      eax, DWORD PTR [rsp]              // get it
  mov      ecx, eax                          // make local copy   self is done with at this point
  // smaller opcode no mem access required.
  and      eax, $00009FFF
  or       eax, $00002000                    // Set bits Round Mask Down
  mov      DWORD PTR [rsp], eax              // mov bits to mem
  ldmxcsr  DWORD PTR [rsp]                   // set new bits
  cvtps2dq xmm0, xmm0
  mov      DWORD PTR [rsp], ecx              // put old bits in mem
  ldmxcsr  DWORD PTR [rsp]                   // Pop rounding mode
  pop      rax
  movq     rax,  xmm0
end;

function Ceil(Constref V : TBZVector2f) : TBZVector2i; assembler; //nostackframe;register;
asm
  movq     xmm0, {%H-}[V]
  push     rax                               // create a mem area for mxcsr
  stmxcsr  DWORD PTR [rsp]                   //  store mxcsr
  mov      eax, DWORD PTR [rsp]              // get it
  mov      ecx, eax                          // make local copy   self is done with at this point
  // smaller opcode no mem access required.
  and      eax, $00009FFF
  or       eax, $00004000                    // Set bits Round Mask up
  mov      DWORD PTR [rsp], eax              // mov bits to mem
  ldmxcsr  DWORD PTR [rsp]                   // set new bits
  cvtps2dq xmm0, xmm0
  mov      DWORD PTR [rsp], ecx              // put old bits in mem
  ldmxcsr  DWORD PTR [rsp]                   // Pop rounding mode
  pop      rax
  movq     rax,  xmm0
end;

function Fract(Constref V : TBZVector2f) : TBZVector2f; assembler; //nostackframe;register;

asm
  movq     xmm0, {%H-}[V]
  movaps   xmm1, xmm0
  push     rax                               // create a mem area for mxcsr
  stmxcsr  DWORD PTR [rsp]                   //  store mxcsr
  mov      eax, DWORD PTR [rsp]              // get it
  mov      ecx, eax                          // make local copy   self is done with at this point
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
  movq     rax, xmm1
end;


function Sqrt(constref V: TBZVector2f): TBZVector2f; assembler;//nostackframe;register;
asm
  movq   xmm0, {%H-}[V]
  sqrtps xmm0, xmm0
  movq   rax, xmm0
end;

function InvSqrt(constref V: TBZVector2f): TBZVector2f; assembler;//nostackframe;register;
asm
  movq   xmm0, {%H-}[V]
  rsqrtps xmm0, xmm0
  movq   rax, xmm0
end;

{%endregion%}


