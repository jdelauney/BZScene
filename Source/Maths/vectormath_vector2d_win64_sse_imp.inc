{%region%----[ Operators ]--------------------------------------------------------}

class operator TBZVector2d.+(constref A, B: TBZVector2d): TBZVector2d; assembler; nostackframe; register;
asm
  movapd xmm0, [A]
  movapd xmm1, [B]
  addpd  xmm0, xmm1
  movapd [Result], xmm0 //Result = RCX
end;

class operator TBZVector2d.-(constref A, B: TBZVector2d): TBZVector2d; assembler; nostackframe; register;
asm
  movapd xmm0, [A]
  movapd xmm1, [B]
  subpd  xmm0, xmm1
  movapd [Result], xmm0
end;

class operator TBZVector2d.*(constref A, B: TBZVector2d): TBZVector2d; assembler; nostackframe; register;
asm
  movapd xmm0, [A]
  movapd xmm1, [B]
  mulpd  xmm0, xmm1
  movapd [Result], xmm0
end;

class operator TBZVector2d.*(constref A: TBZVector2d; constref B : TBZVector2i): TBZVector2d; assembler; nostackframe; register;
asm
  movapd   xmm0, [A]
  movapd   xmm1, [B]
  cvtdq2pd xmm2, xmm1
  mulpd    xmm0, xmm2
  movapd   [Result], xmm0
end;

class operator TBZVector2d./(constref A, B: TBZVector2d): TBZVector2d; assembler; nostackframe; register;
asm
  movapd xmm0, [A]
  movapd xmm1, [B]
  divpd  xmm0, xmm1
  movapd [Result], xmm0
end;

(* The 4 for above functions are pit of doom for performances, so we use native instead

class operator TBZVector2d.+(constref A: TBZVector2d; constref B:Double): TBZVector2d; assembler; nostackframe; register;
asm
  movapd xmm0, [A]
  movq   xmm1, [B]
  shufpd xmm1, xmm1, $00
  addpd  xmm0, xmm1
  movapd [Result], xmm0
end;

class operator TBZVector2d.-(constref A: TBZVector2d; constref B:Double): TBZVector2d; assembler; nostackframe; register;
asm
  movapd xmm0, [A]
  movq   xmm1, [B]
  shufpd xmm1, xmm1, $00
  subpd  xmm0, xmm1
  movapd [Result], xmm0
end;

class operator TBZVector2d.*(constref A: TBZVector2d; constref B:Double): TBZVector2d; assembler; nostackframe; register;
asm
  movapd xmm0, [A]
  movq   xmm1, [B]
  shufpd xmm1, xmm1, $00
  //movhps xmm1, [B]
  //movlps xmm1, [B]
  mulpd  xmm0, xmm1
  movapd [Result], xmm0
end;

class operator TBZVector2d./(constref A: TBZVector2d; constref B:Double): TBZVector2d; assembler; nostackframe; register;
asm
  movapd xmm0, [A]
  movq xmm1, [B]
  shufpd xmm1, xmm1, $00
  divpd  xmm0, xmm1
  movapd [Result], xmm0
end; *)

class operator TBZVector2d./(constref A: TBZVector2d; constref B:TBZVector2i): TBZVector2d; assembler; nostackframe; register;
asm
  movapd   xmm0, [A]
  movapd  xmm2, [B]
  cvtdq2pd xmm1, xmm2
  divpd    xmm0, xmm1
  movapd   [Result], xmm0
end;

class operator TBZVector2d.+(constref A: TBZVector2d; constref B: TBZVector2i): TBZVector2d; assembler; nostackframe; register;
asm
  movapd   xmm0, [A]
  movapd  xmm2, [B]
  cvtdq2pd xmm1, xmm2
  addpd    xmm0, xmm1
  movapd   [Result], xmm0
end;

class operator TBZVector2d.-(constref A: TBZVector2d; constref B: TBZVector2i): TBZVector2d; assembler; nostackframe; register;
asm
  movapd   xmm0, [A]
  movapd  xmm2, [B]
  cvtdq2pd xmm1, xmm2
  subpd    xmm0, xmm1
  movapd   [Result], xmm0
end;
{%endregion%}

{%region%----[ Functions ]--------------------------------------------------------}

function TBZVector2d.Min(constref B: Double): TBZVector2d; assembler; nostackframe; register;
asm
  movapd xmm0, [RCX]
  movq   xmm1, [B]
  shufpd xmm1, xmm1, $00
  minpd  xmm0, xmm1
  movapd [Result], xmm0
end;

function TBZVector2d.Min(constref B: TBZVector2d): TBZVector2d; assembler; nostackframe; register;
asm
  movapd xmm0, [RCX]
  movapd xmm1, [B]
  minpd  xmm0, xmm1
  movapd [Result], xmm0
end;


function TBZVector2d.Max(constref B: Double): TBZVector2d;Assembler; nostackframe;register;
asm
  movapd xmm0, [RCX]
  movq   xmm1, [B]
  shufpd xmm1, xmm1, $00
  maxpd  xmm0, xmm1
  movapd [Result], xmm0
end;

function TBZVector2d.Max(constref B: TBZVector2d): TBZVector2d;Assembler; nostackframe;register;
asm
  movapd xmm0, [RCX]
  movapd xmm1, [B]
  maxpd  xmm0, xmm1
  movapd [Result], xmm0
end;

function TBZVector2d.Clamp(constref AMin, AMax: Double): TBZVector2d;Assembler; nostackframe;register;
asm
  movapd xmm0, [RCX]
  movq   xmm1, [AMin]
  movq   xmm2, [AMax]
  shufpd xmm1, xmm1, $00 // Replicate AMin
  shufpd xmm2, xmm2, $00 // Replicate AMax
  maxpd  xmm0, xmm1
  minpd  xmm0, xmm2
  movapd [Result], xmm0
end;

function TBZVector2d.Clamp(constref AMin, AMax: TBZVector2d): TBZVector2d;Assembler; nostackframe;register;
asm
  movapd xmm0, [RCX]
  movapd xmm1, [AMin]
  movapd xmm2, [AMax]
  maxpd  xmm0, xmm1
  minpd  xmm0, xmm2
  movapd [Result], xmm0
end;

function TBZVector2d.MulAdd(constref A,B:TBZVector2d): TBZVector2d;Assembler; nostackframe;register;
asm
  movapd xmm0, [RCX]
  movapd xmm1, {%H-}[A]
  movapd xmm2, {%H-}[B]
  Mulpd  xmm0, xmm1
  Addpd  xmm0, xmm2
  movapd [Result], xmm0
end;

function TBZVector2d.MulSub(constref A,B:TBZVector2d): TBZVector2d;Assembler; nostackframe;register;
asm
  movapd xmm0, [RCX]
  movapd xmm1, [A]
  movapd xmm2, [B]
  Mulpd  xmm0, xmm1
  Subpd  xmm0, xmm2
  movapd [Result], xmm0
end;

function TBZVector2d.MulDiv(constref A,B:TBZVector2d): TBZVector2d;Assembler;nostackframe;register;
asm
  movapd xmm0, [RCX]
  movapd xmm1, [A]
  movapd xmm2, [B]
  Mulpd  xmm0, xmm1
  Divpd  xmm0, xmm2
  movapd [Result], xmm0
end;

function TBZVector2d.Length:Double;Assembler;nostackframe;register;
asm
  movapd xmm0, [RCX]
  mulpd  xmm0, xmm0
  haddpd xmm0, xmm0
  //movddup xmm1, xmm0
  //addpd    xmm0, xmm1
  sqrtsd   xmm0, xmm0
end;

function TBZVector2d.LengthSquare:Double;Assembler;nostackframe;register;
asm
  movapd xmm0, [RCX]
  mulpd  xmm0, xmm0
  haddpd xmm0, xmm0
  //movddup xmm1, xmm0
  //addpd    xmm0, xmm1
end;

function TBZVector2d.Distance(constref A:TBZVector2d):Double;Assembler;nostackframe;register;
asm
  movapd xmm0, [RCX]
  movapd xmm1, [A]
  subpd  xmm0, xmm1
  mulpd  xmm0, xmm0
  haddpd xmm0, xmm0
  //movddup xmm1, xmm0
  //addpd    xmm0, xmm1
  sqrtsd   xmm0, xmm0
end;

function TBZVector2d.DistanceSquare(Constref A:TBZVector2d):Double;Assembler;nostackframe;register;
asm
  movapd xmm0, [RCX]
  movapd xmm1, [A]
  subpd  xmm0, xmm1
  mulpd  xmm0, xmm0
  haddpd xmm0, xmm0
  //movddup xmm1, xmm0
  //addpd    xmm0, xmm1
end;

function TBZVector2d.Normalize : TBZVector2d;Assembler;nostackframe;register;
asm
  movapd xmm2, [RCX]
  movapd xmm0, xmm2
  mulpd  xmm2, xmm2
  haddpd xmm2, xmm2
  sqrtpd xmm2, xmm2
  divpd  xmm0, xmm2
  movapd   [Result], xmm0
end;

// function Reflect(I, NRef : TVector2f):TVector2f
function TBZVector2d.Round: TBZVector2i;assembler;nostackframe;register;
asm
  // Rounding mode defaults to round-to-nearest
  movapd   xmm0, [RCX]

  push     rax                               // create a mem area for mxcsr
  stmxcsr  DWORD PTR [rsp]                   //  store mxcsr
  mov      eax, DWORD PTR [rsp]              // get it
  mov      ecx, eax                          // make local copy   self is done with at this point
  // smaller opcode no mem access required.
  and      eax, $00009FFF
  or       eax, $00000000                    // Set bits

  mov      DWORD PTR [rsp], eax              // mov bits to mem
  ldmxcsr  DWORD PTR [rsp]                   // set new bits
  cvtpd2dq xmm0, xmm0
  mov      DWORD PTR [rsp], ecx              // put old bits in mem
  ldmxcsr  DWORD PTR [rsp]                   // Pop rounding mode
  pop      rax

  movq     rax, xmm0
end;

function TBZVector2d.Abs: TBZVector2d; assembler; nostackframe; register;
asm
  movapd   xmm0, [RCX]
  andpd    xmm0, XMMWORD PTR [RIP+cSSE_MASK_ABS]//xmm1
  movapd   [Result], xmm0
end;


function TBZVector2d.Trunc: TBZVector2i;assembler;//nostackframe;register;
asm
  movapd   xmm0, [RCX]
  push     rax                               // create a mem area for mxcsr
  stmxcsr  DWORD PTR [rsp]                   //  store mxcsr
  mov      eax, DWORD PTR [rsp]              // get it
  mov      ecx, eax                          // make local copy   self is done with at this point
  // smaller opcode no mem access required.
  and      eax, $00009FFF
  or       eax, $00006000                    // Set bits
  mov      DWORD PTR [rsp], eax              // mov bits to mem
  ldmxcsr  DWORD PTR [rsp]                   // set new bits
  cvtpd2dq xmm0, xmm0
  mov      DWORD PTR [rsp], ecx              // put old bits in mem
  ldmxcsr  DWORD PTR [rsp]                   // Pop rounding mode
  pop      rax
  movq     rax, xmm0
end;

function TBZVector2d.Floor : TBZVector2i; assembler;//nostackframe;register;
asm
  movapd   xmm0, [RCX]
  push     rax                               // create a mem area for mxcsr
  stmxcsr  DWORD PTR [rsp]                   //  store mxcsr
  mov      eax, DWORD PTR [rsp]              // get it
  mov      ecx, eax                          // make local copy   self is done with at this point
  // smaller opcode no mem access required.
  and      eax, $00009FFF
  or       eax, $00002000;                    // Set bits Round Mask Down
  mov      DWORD PTR [rsp], eax              // mov bits to mem
  ldmxcsr  DWORD PTR [rsp]                   // set new bits
  cvtpd2dq xmm0, xmm0
  mov      DWORD PTR [rsp], ecx              // put old bits in mem
  ldmxcsr  DWORD PTR [rsp]                   // Pop rounding mode
  pop      rax
  movq   rax, xmm0
end;

function TBZVector2d.Ceil : TBZVector2i; assembler;//nostackframe;register;
asm
  movapd   xmm0, [RCX]
  push     rax                               // create a mem area for mxcsr
  stmxcsr  DWORD PTR [rsp]                   //  store mxcsr
  mov      eax, DWORD PTR [rsp]              // get it
  mov      ecx, eax                          // make local copy   self is done with at this point
  // smaller opcode no mem access required.
  and      eax, $00009FFF
  or       eax, $00004000;                    // Set bits Round Mask up
  mov      DWORD PTR [rsp], eax              // mov bits to mem
  ldmxcsr  DWORD PTR [rsp]                   // set new bits
  cvtpd2dq xmm0, xmm0
  mov      DWORD PTR [rsp], ecx              // put old bits in mem
  ldmxcsr  DWORD PTR [rsp]                   // Pop rounding mode
  pop      rax
  movq     rax, xmm0
end;

// NOT WORKING GIVE WRONG RESULT. It return the entire part  :(

//function TBZVector2d.Fract: TBZVector2d; assembler;//nostackframe;register;
//asm
//  movapd   xmm0, [RCX]
//  movapd   xmm1, xmm0
//  push     rax                               // create a mem area for mxcsr
//  stmxcsr  DWORD PTR [rsp]                   //  store mxcsr
//  mov      eax, DWORD PTR [rsp]              // get it
//  mov      ecx, eax                          // make local copy   self is done with at this point
//  // smaller opcode no mem access required.
//  and      eax, $00009FFF
//  or       eax, $00006000                    // Set bits
//  mov      DWORD PTR [rsp], eax              // mov bits to mem
//  ldmxcsr  DWORD PTR [rsp]                   // set new bits
//  cvtpd2dq xmm0, xmm0
//  mov      DWORD PTR [rsp], ecx              // put old bits in mem
//  ldmxcsr  DWORD PTR [rsp]                   // Pop rounding mode
//  pop      rax
//  cvtdq2pd xmm0, xmm0
//  subpd    xmm1, xmm0 // A - Trunc(A)
//  movq     rdx, xmm1
//end;

function TBZVector2d.Sqrt: TBZVector2d; assembler;nostackframe;register;
asm
  movapd xmm0,[RCX]
  sqrtpd xmm0, xmm0
  movapd [Result], xmm0
end;

//function TBZVector2d.InvSqrt: TBZVector2d; assembler;nostackframe;register;
//asm
//  movapd   xmm0,[RCX]
//  //rsqrtpd  xmm0, xmm0
//  movapd   [Result], xmm0
//end;

function TBZVector2d.ModF(Constref A : TBZVector2d): TBZVector2d; assembler;nostackframe;register;
asm
  movapd   xmm0, [RCX]
  movapd   xmm1, xmm0
  movapd   xmm2, [A]
  divpd    xmm0, xmm2
  push     rax                               // create a mem area for mxcsr
  stmxcsr  DWORD PTR [rsp]                   //  store mxcsr
  mov      eax, DWORD PTR [rsp]              // get it
  mov      ecx, eax                          // make local copy   self is done with at this point
  // smaller opcode no mem access required.
  and      eax, $00009FFF
  or       eax, $00006000                   // Set bits
  mov      DWORD PTR [rsp], eax              // mov bits to mem
  ldmxcsr  DWORD PTR [rsp]                   // set new bits
  cvtpd2dq xmm0, xmm0
  mov      DWORD PTR [rsp], ecx              // put old bits in mem
  ldmxcsr  DWORD PTR [rsp]                   // Pop rounding mode
  pop      rax
  cvtdq2pd xmm0, xmm0
  mulpd    xmm0, xmm2
  subpd    xmm1, xmm0 // A - Trunc(A)
  movapd   [Result], xmm1
end;

function TBZVector2d.fMod(Constref A : TBZVector2d): TBZVector2i; assembler;nostackframe;register;
asm
  movapd   xmm0, [RCX]
  movapd   xmm1, xmm0
  movapd   xmm2, [A]

  divpd    xmm0, xmm2
  push     rax                               // create a mem area for mxcsr
  stmxcsr  DWORD PTR [rsp]                   //  store mxcsr
  mov      eax, DWORD PTR [rsp]              // get it
  mov      ecx, eax                          // make local copy   self is done with at this point
  // smaller opcode no mem access required.
  and      eax, $00009FFF
  or       eax, $00002000                    // Set bits
  mov      DWORD PTR [rsp], eax              // mov bits to mem
  ldmxcsr  DWORD PTR [rsp]                   // set new bits
  cvtpd2dq xmm0, xmm0
  mov      DWORD PTR [rsp], ecx              // put old bits in mem
  ldmxcsr  DWORD PTR [rsp]                   // Pop rounding mode
  pop      rax

  cvtdq2pd xmm0, xmm0
  mulpd    xmm0, xmm2
  subpd    xmm1, xmm0 // A - Trunc(A)

  push     rax                               // create a mem area for mxcsr
  stmxcsr  DWORD PTR [rsp]                   //  store mxcsr
  mov      eax, DWORD PTR [rsp]              // get it
  mov      ecx, eax                          // make local copy   self is done with at this point
  // smaller opcode no mem access required.
  and      eax, $00009FFF
  or       eax, $00006000                   // Set bits
  mov      DWORD PTR [rsp], eax              // mov bits to mem
  ldmxcsr  DWORD PTR [rsp]                   // set new bits
  cvtpd2dq xmm1, xmm1
  mov      DWORD PTR [rsp], ecx              // put old bits in mem
  ldmxcsr  DWORD PTR [rsp]                   // Pop rounding mode
  pop      rax

  movq rax,{%H-}xmm1
end;

{%endregion%}
