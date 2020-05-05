{%region%----[ Operators ]--------------------------------------------------------}

class operator TBZVector2f.+(constref A, B: TBZVector2f): TBZVector2f; assembler; nostackframe; register;
asm
  vmovq  xmm0, [A]
  vmovq  xmm1, [B]
  vaddps xmm0, xmm0, xmm1
end;

class operator TBZVector2f.-(constref A, B: TBZVector2f): TBZVector2f; assembler; nostackframe; register;
asm
  vmovq  xmm0, [A]
  vmovq  xmm1, [B]
  vsubps xmm0, xmm0, xmm1
end;

class operator TBZVector2f.*(constref A, B: TBZVector2f): TBZVector2f; assembler; nostackframe; register;
asm
  vmovq  xmm0, [A]
  vmovq  xmm1, [B]
  vmulps xmm0, xmm0, xmm1
end;

class operator TBZVector2f./(constref A, B: TBZVector2f): TBZVector2f; assembler; nostackframe; register;
asm
  vmovq  xmm0, [A]
  vmovq  xmm1, [B]
  vdivps xmm0, xmm0, xmm1
end;

class operator TBZVector2f.+(constref A: TBZVector2f; constref B:Single): TBZVector2f; assembler; nostackframe; register;
asm
  vmovq   xmm0, [A]
  vmovss  xmm1, [B]
  vshufps xmm1, xmm1, xmm1, $00
  vaddps  xmm0, xmm0, xmm1
end;

class operator TBZVector2f.-(constref A: TBZVector2f; constref B:Single): TBZVector2f; assembler; nostackframe; register;
asm
  vmovq   xmm0, [A]
  vmovss  xmm1, [B]
  vshufps xmm1, xmm1, xmm1, $00
  vsubps  xmm0, xmm0, xmm1
end;

class operator TBZVector2f.*(constref A: TBZVector2f; constref B:Single): TBZVector2f; assembler; nostackframe; register;
asm
  vmovq   xmm0, [A]
  vmovss  xmm1, [B]
  vshufps xmm1, xmm1, xmm1, $00
  vmulps  xmm0, xmm0, xmm1
end;

class operator TBZVector2f./(constref A: TBZVector2f; constref B:Single): TBZVector2f; assembler; nostackframe; register;
asm
  vmovq   xmm0, [A]
  vmovq   xmm1, [B]
  vshufps xmm1, xmm1, xmm1, $00
  vdivps  xmm0, xmm0, xmm1
end;


{%endregion%}

{%region%----[ Functions ]--------------------------------------------------------}

function TBZVector2f.Min(constref B: Single): TBZVector2f; assembler; nostackframe; register;
asm
  vmovq   xmm0, [RDI]
  vmovss  xmm1, [B]
  vshufps xmm1, xmm1, xmm1, $00
  vminps  xmm0, xmm0, xmm1
end;

function TBZVector2f.Min(constref B: TBZVector2f): TBZVector2f; assembler; nostackframe; register;
asm
  vmovq  xmm0, [RDI]
  vmovq  xmm1, [B]
  vminps xmm0, xmm0, xmm1
end;


function TBZVector2f.Max(constref B: Single): TBZVector2f;Assembler; nostackframe;register;
asm
  vmovq   xmm0, [RDI]
  vmovss  xmm1, [B]
  vshufps xmm1, xmm1, xmm1, $00
  vmaxps  xmm0, xmm0, xmm1
end;

function TBZVector2f.Max(constref B: TBZVector2f): TBZVector2f;Assembler; nostackframe;register;
asm
  vmovq  xmm0, [RDI]
  vmovq  xmm1, [B]
  vmaxps xmm0, xmm0, xmm1
end;

function TBZVector2f.Clamp(constref AMin, AMax: Single): TBZVector2f;Assembler; nostackframe;register;
asm
  vmovq   xmm0, [RDI] //movq ????
  vmovss  xmm1, [AMin]
  vmovss  xmm2, [AMax]
  vshufps xmm1, xmm1, xmm1, $00 // Replicate AMin
  vshufps xmm2, xmm2, xmm2, $00 // Replicate AMax
  vmaxps  xmm0, xmm0, xmm1
  vminps  xmm0, xmm0, xmm2
end;

function TBZVector2f.Clamp(constref AMin, AMax: TBZVector2f): TBZVector2f;Assembler; nostackframe;register;
asm
  vmovq  xmm0, [RDI]
  vmovq  xmm1, [AMin]
  vmovq  xmm2, [AMax]
  vmaxps xmm0, xmm0, xmm1
  vminps xmm0, xmm0, xmm2
end;

function TBZVector2f.MulAdd(constref A,B:TBZVector2f): TBZVector2f;Assembler; nostackframe; register;
asm
  vmovq  xmm0, [RDI]
  vmovq  xmm1, [A]
  vmovq  xmm2, [B]
  vmulps xmm0, xmm0, xmm1
  vaddps xmm0, xmm0, xmm2
end;

function TBZVector2f.MulDiv(constref A,B:TBZVector2f): TBZVector2f;Assembler; nostackframe; register;
asm
  vmovq   xmm0, [RDI]
  vmovq   xmm1, [A]
  vmovq   xmm2, [B]
  vmulps  xmm0, xmm0, xmm1
  vdivps  xmm0, xmm0, xmm2
end;

function TBZVector2f.Length:Single;Assembler;nostackframe;register;
asm
  vmovq     xmm0, [RDI]
  vmulps    xmm0, xmm0, xmm0
  vmovshdup xmm1, xmm0
  vaddps    xmm0, xmm0, xmm1
  sqrtss    xmm0, xmm0
end;

function TBZVector2f.LengthSquare:Single;Assembler;nostackframe;register;
asm
  vmovq     xmm0, [RDI]
  vmulps    xmm0, xmm0, xmm0
  vmovshdup xmm1, xmm0
  vaddps    xmm0, xmm0, xmm1
end;

function TBZVector2f.Distance(constref A:TBZVector2f):Single;Assembler; nostackframe;register;
asm
  vmovq   xmm0, [RDI]
  vmovq   xmm1, [A]
  vsubps  xmm0, xmm0, xmm1
  vmulps  xmm0, xmm0, xmm0
  vhaddps xmm0, xmm0, xmm0
  vsqrtss xmm0, xmm0, xmm0
end;

function TBZVector2f.DistanceSquare(Constref A:TBZVector2f):Single;Assembler; nostackframe;register;
asm
  vmovq   xmm0, [RDI]
  vmovq   xmm1, [A]
  vsubps  xmm0, xmm0, xmm1
  vmulps  xmm0, xmm0, xmm0
  vhaddps xmm0, xmm0, xmm0
end;


// todo this looks like work in progress.
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
  cvtps2dq xmm0, [RDI]
  mov      DWORD PTR [rsp], ecx              // put old bits in mem
  ldmxcsr  DWORD PTR [rsp]                   // Pop rounding mode
  pop      rax                               // free used stack
  vmovq    rax,  xmm0
end;

function TBZVector2f.Round: TBZVector2i;assembler; nostackframe; register;
asm
  vcvtps2dq xmm0, [RDI]
  vmovq     rax,  xmm0
end;



{%endregion%}
