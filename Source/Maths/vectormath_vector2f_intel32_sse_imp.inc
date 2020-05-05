{%region%----[ Operators ]--------------------------------------------------------}

class operator TBZVector2f.+(constref A, B: TBZVector2f): TBZVector2f; assembler; nostackframe; register;
asm
  movlps  xmm0, [A]
  movlps  xmm1, [B]
  addps   xmm0, xmm1
  movlps  [ECX], xmm0
end;

class operator TBZVector2f.-(constref A, B: TBZVector2f): TBZVector2f; assembler; nostackframe; register;
asm
  movlps  xmm0, [A]
  movlps  xmm1, [B]
  subps   xmm0, xmm1
  movlps  [ECX], xmm0
end;

class operator TBZVector2f.*(constref A, B: TBZVector2f): TBZVector2f; assembler; nostackframe; register;
asm
  movlps  xmm0, [A]
  movlps  xmm1, [B]
  mulps   xmm0, xmm1
  movlps  [ECX], xmm0
end;

class operator TBZVector2f./(constref A, B: TBZVector2f): TBZVector2f; assembler; nostackframe; register;
asm
  movlps  xmm0, [A]
  movlps  xmm1, [B]
  divps   xmm0, xmm1
  movlps  [ECX], xmm0
end;

class operator TBZVector2f.+(constref A: TBZVector2f; constref B:Single): TBZVector2f; assembler; nostackframe; register;
asm
  movlps   xmm0, [A]
  movlps   xmm1, [B]
  shufps   xmm1, xmm1, $00
  addps    xmm0, xmm1
  movlps  [ECX], xmm0
end;

class operator TBZVector2f.-(constref A: TBZVector2f; constref B:Single): TBZVector2f; assembler; nostackframe; register;
asm
  movlps   xmm0, [A]
  movlps   xmm1, [B]
  shufps   xmm1, xmm1, $00
  subps    xmm0, xmm1
  movlps  [ECX], xmm0
end;

class operator TBZVector2f.*(constref A: TBZVector2f; constref B:Single): TBZVector2f; assembler; nostackframe; register;
asm
  movlps   xmm0, [A]
  movlps   xmm1, [B]
  shufps   xmm1, xmm1, $00
  mulps    xmm0, xmm1
  movlps  [ECX], xmm0
end;

class operator TBZVector2f./(constref A: TBZVector2f; constref B:Single): TBZVector2f; assembler; nostackframe; register;
asm
  movlps   xmm0, [A]
  movlps   xmm1, [B]
  shufps   xmm1, xmm1, $00
  divps    xmm0, xmm1
  movlps  [ECX], xmm0
end;


{%endregion%}

{%region%----[ Functions ]--------------------------------------------------------}

function TBZVector2f.Min(constref B: Single): TBZVector2f; assembler; nostackframe; register;
asm
  movlps   xmm0, [EAX]
  movlps   xmm1, [B]
  shufps   xmm1, xmm1, $00
  minps    xmm0, xmm1
  movlps  [ECX], xmm0
end;

function TBZVector2f.Min(constref B: TBZVector2f): TBZVector2f; assembler; nostackframe; register;
asm
  movlps  xmm0, [EAX]
  movlps  xmm1, [B]
  minps   xmm0, xmm1
  movlps [ECX], xmm0
end;


function TBZVector2f.Max(constref B: Single): TBZVector2f;Assembler; nostackframe;register;
asm
  movlps   xmm0, [EAX]
  movlps   xmm1, [B]
  shufps   xmm1, xmm1, $00
  maxps    xmm0, xmm1
  movlps  [ECX], xmm0
end;

function TBZVector2f.Max(constref B: TBZVector2f): TBZVector2f;Assembler; nostackframe;register;
asm
  movlps  xmm0, [EAX]
  movlps  xmm1, [B]
  maxps   xmm0, xmm1
  movlps [ECX], xmm0
end;

function TBZVector2f.Clamp(constref AMin, AMax: Single): TBZVector2f;Assembler; //nostackframe;register;
asm
  movlps   xmm0, [EAX] //movq ????
  movlps   xmm1, [AMin]
  movlps   xmm2, [AMax]
  shufps   xmm1, xmm1, $00 // Replicate AMin
  shufps   xmm2, xmm2, $00 // Replicate AMax
  maxps    xmm0, xmm1
  minps    xmm0, xmm2
  mov      EDX,  [RESULT]
  movlps  [EDX], xmm0
end;

function TBZVector2f.Clamp(constref AMin, AMax: TBZVector2f): TBZVector2f;Assembler; //nostackframe;register;
asm
  movlps   xmm0, [EAX]
  movlps   xmm1, [AMin]
  movlps   xmm2, [AMax]
  maxps    xmm0, xmm1
  minps    xmm0, xmm2
  mov      EDX,  [RESULT]
  movlps  [EDX], xmm0
end;

function TBZVector2f.MulAdd(constref A,B:TBZVector2f): TBZVector2f;Assembler; //nostackframe; register;
asm
  movlps   xmm0, [EAX]
  movlps   xmm1, [A]
  movlps   xmm2, [B]
  Mulps    xmm0, xmm1
  Addps    xmm0, xmm2
  mov      EDX,  [RESULT]
  movlps  [EDX], xmm0
end;

function TBZVector2f.MulDiv(constref A,B:TBZVector2f): TBZVector2f;Assembler; //nostackframe; register;
asm
  movlps   xmm0, [EAX]
  movlps   xmm1, [A]
  movlps   xmm2, [B]
  Mulps    xmm0, xmm1
  Divps    xmm0, xmm2
  mov      EDX,  [RESULT]
  movlps  [EDX], xmm0
end;

function TBZVector2f.Length:Single;Assembler;  //nostackframe;register;
asm
  movlps   xmm0, [EAX]
  mulps    xmm0, xmm0
  movshdup xmm1, xmm0
  addps    xmm0, xmm1
  sqrtss   xmm0, xmm0
  movss [Result], {%H-}xmm0
end;

function TBZVector2f.LengthSquare:Single;Assembler;  //nostackframe;register;
asm
  movlps   xmm0, [EAX]
  mulps    xmm0, xmm0
  movshdup xmm1,xmm0
  addps    xmm0, xmm1
  movss [Result], {%H-}xmm0
end;

function TBZVector2f.Distance(constref A:TBZVector2f):Single;Assembler; //nostackframe;register;
asm
  movlps xmm0, [EAX]
  movlps xmm1, [A]
  subps  xmm0, xmm1
  mulps  xmm0, xmm0
  haddps xmm0, xmm0
  sqrtss xmm0, xmm0
  movss [Result], {%H-}xmm0
end;

function TBZVector2f.DistanceSquare(Constref A:TBZVector2f):Single;Assembler; //nostackframe;register;
asm
  movlps xmm0, [EAX]
  movlps xmm1, [A]
  subps  xmm0, xmm1
  mulps  xmm0, xmm0
  haddps xmm0, xmm0
  movss [Result], {%H-}xmm0
end;

function TBZVector2f.Normalize : TBZVector2f;Assembler; nostackframe; register;
asm
  movq    xmm2, [EAX]     // Y X
  movaps  xmm0, xmm2
  mulps   xmm2, xmm2      // Y*Y X*X
  pshufd  xmm1, xmm2, $01 // X*X Y*Y
  addps   xmm2, xmm1      // (X*X+Y*Y) (2x)
  sqrtps  xmm2, xmm2
  divps   xmm0, xmm2
  movlps [EDX], xmm0
end;

function TBZVector2f.Round: TBZVector2i; assembler; nostackframe; register;
asm
  // Rounding mode defaults to round-to-nearest
  movlps    xmm0, [EAX]
  cvtps2dq  xmm0, xmm0
  movlps   [EDX], xmm0
end;

function TBZVector2f.Trunc: TBZVector2i;assembler; nostackframe;register;
asm
  movlps     xmm0, [EAX]
  {$i macro_sse_win64_trunc_xmm0-xmm0.inc}
  movlps   [EDX], xmm0
end;


{%endregion%}
