{%region%----[ Operators ]--------------------------------------------------------}

class operator TBZVector2i.+(constref A, B: TBZVector2i): TBZVector2i; assembler; nostackframe; register;
asm
  movq  xmm0, [A]
  movq  xmm1, [B]
  paddd xmm0, xmm1
  movq  RAX, {%H-}xmm0
end;

class operator TBZVector2i.-(constref A, B: TBZVector2i): TBZVector2i; assembler; nostackframe; register;
asm
  movq  xmm0, [A]
  movq  xmm1, [B]
  psubd xmm0, xmm1
  movq  RAX, {%H-}xmm0
end;

class operator TBZVector2i.*(constref A, B: TBZVector2i): TBZVector2i; assembler; nostackframe; register;
asm
  movq     xmm0, [A]
  movq     xmm1, [B]
  cvtdq2ps xmm0, xmm0
  cvtdq2ps xmm1, xmm1
  mulps    xmm0, xmm1
  cvtps2dq xmm0, xmm0 //Round
  movq     RAX, {%H-}xmm0
end;


class operator TBZVector2i.Div(constref A, B: TBZVector2i): TBZVector2i; assembler; nostackframe; register;
asm
  movq     xmm0, [A]
  movq     xmm1, [B]
  cvtdq2ps xmm0, xmm0
  cvtdq2ps xmm1, xmm1
  divps    xmm0, xmm1
  {$i macro_sse_win64_trunc_xmm0-xmm0.inc}
  movq  RAX, {%H-}xmm0
end;

class operator TBZVector2i.+(constref A: TBZVector2i; constref B:Single): TBZVector2i; assembler; nostackframe; register;
asm
  movq     xmm0, [B]
  movq     xmm1, [A]
  shufps   xmm0, xmm0, $00
  cvtps2dq xmm0, xmm0
  paddd    xmm0, xmm1
  movq     RAX, {%H-}xmm0
end;

class operator TBZVector2i.+(constref A: TBZVector2i; constref B:Integer): TBZVector2i; assembler; nostackframe; register;
asm
  movq   xmm0, [B]
  movq   xmm1, [A]
  shufps xmm0, xmm0, $00
  paddd  xmm0, xmm1
  movq   RAX, {%H-}xmm0
end;

class operator TBZVector2i.-(constref A: TBZVector2i; constref B:Single): TBZVector2i; assembler; nostackframe; register;
asm
  movq   xmm0, [A]
  movq   xmm1, [B]
  pshufd xmm1, xmm1, $00
  psubd  xmm0, xmm1
  movq   RAX, {%H-}xmm0
end;

class operator TBZVector2i.-(constref A: TBZVector2i; constref B:Integer): TBZVector2i; assembler; nostackframe; register;
asm
  movq   xmm0, [A]
  movq   xmm1, [B]
  pshufd xmm1, xmm1, $00
  psubd  xmm0, xmm1
  movq   RAX, {%H-}xmm0
end;

class operator TBZVector2i.*(constref A: TBZVector2i; constref B:Single): TBZVector2i; assembler; nostackframe; register;
asm
  movq     xmm0, [A]
  movq     xmm1, [B]
  cvtdq2ps xmm0, xmm0
  shufps   xmm1, xmm1, $00
  mulps    xmm0, xmm1
  cvtps2dq xmm0, xmm0
  movq     RAX, {%H-}xmm0
end;

class operator TBZVector2i./(constref A: TBZVector2i; constref B:Single): TBZVector2i; assembler; nostackframe; register;
asm
  movq     xmm0, [A]
  movq     xmm1, [B]
  cvtdq2ps xmm0, xmm0
  shufps   xmm1, xmm1, $00
  divps    xmm0, xmm1
  cvtps2dq xmm0, xmm0
  movq     RAX, {%H-}xmm0
end;

class operator TBZVector2i.Div(constref A: TBZVector2i; constref B:Integer): TBZVector2i; assembler; nostackframe; register;
asm
  movq     xmm0, [A]
  movq     xmm1, [B]
  cvtdq2ps xmm0, xmm0
  cvtdq2ps xmm1, xmm1
  shufps   xmm1, xmm1, $00
  divps    xmm0, xmm1
  {$i macro_sse_win64_trunc_xmm0-xmm0.inc}
  movq   RAX, {%H-}xmm0
end;


{%endregion%}

{%region%----[ Functions ]--------------------------------------------------------}

function TBZVector2i.Min(constref B: Integer): TBZVector2i; assembler; nostackframe; register;
asm
  movq     xmm0, [RDI]
  movd     xmm1, [B]
  cvtdq2ps xmm0, xmm0
  cvtdq2ps xmm1, xmm1
  shufps   xmm1, xmm1, $00
  minps    xmm0,xmm1
  cvtps2dq xmm0, xmm0
  movq     RAX, {%H-}xmm0
end;

function TBZVector2i.Min(constref B: TBZVector2i): TBZVector2i;assembler; nostackframe; register;
asm
  movq     xmm0, [RDI]
  movq     xmm1, [B]
  cvtdq2ps xmm0, xmm0
  cvtdq2ps xmm1, xmm1
  minps    xmm0, xmm1
  cvtps2dq xmm0, xmm0
  movq     RAX, {%H-}xmm0
end;


function TBZVector2i.Max(constref B: Integer): TBZVector2i;assembler; nostackframe; register;
asm
  movq     xmm0, [RDI]
  movq     xmm1, [B]
  cvtdq2ps xmm0, xmm0
  cvtdq2ps xmm1, xmm1
  shufps   xmm1, xmm1, $00
  maxps    xmm0, xmm1
  cvtps2dq xmm0, xmm0
  movq     RAX, {%H-}xmm0
end;

function TBZVector2i.Max(constref B: TBZVector2i): TBZVector2i;assembler; nostackframe; register;
asm
  movq     xmm0, [RDI]
  movq     xmm1, [B]
  cvtdq2ps xmm0, xmm0
  cvtdq2ps xmm1, xmm1
  maxps    xmm0, xmm1
  cvtps2dq xmm0, xmm0
  movq     RAX,  {%H-}xmm0
end;

function TBZVector2i.Clamp(constref AMin, AMax: Integer): TBZVector2i; assembler; nostackframe; register;
asm
  movq   xmm0, [RDI]
  movq   xmm1, [AMin]
  movq   xmm2, [AMax]
  shufps xmm1, xmm1, $00 // Replicate AMin
  shufps xmm2, xmm2, $00 // Replicate AMax
  maxps  xmm0, xmm1
  minps  xmm0, xmm2
  movq   RAX,  {%H-}xmm0
end;

function TBZVector2i.Clamp(constref AMin, AMax: TBZVector2i): TBZVector2i;assembler; nostackframe; register;
asm
  movq   xmm0, [RDI]
  movq   xmm1, [AMin]
  movq   xmm2, [AMax]
  maxps  xmm0, xmm1
  minps  xmm0, xmm2
  movq   RAX,  {%H-}xmm0
end;

function TBZVector2i.MulAdd(constref A, B: TBZVector2i): TBZVector2i;assembler; nostackframe; register;
asm
  movq     xmm0, [RDI]
  movq     xmm1, [A]
  movq     xmm2, [B]
  cvtdq2ps xmm0, xmm0
  cvtdq2ps xmm1, xmm1
  cvtdq2ps xmm2, xmm2
  Mulps    xmm0, xmm1
  Addps    xmm0, xmm2
  cvtps2dq xmm0, xmm0
  movq     RAX,  {%H-}xmm0
end;

function TBZVector2i.MulDiv(constref A, B: TBZVector2i): TBZVector2i; assembler; nostackframe; register;
asm
  movq     xmm0, [RDI]
  movq     xmm1, [A]
  movq     xmm2, [B]
  cvtdq2ps xmm0, xmm0
  cvtdq2ps xmm1, xmm1
  cvtdq2ps xmm2, xmm2
  Mulps    xmm0, xmm1
  Divps    xmm0, xmm2
  {$i macro_sse_win64_trunc_xmm0-xmm0.inc}
  movq     RAX, {%H-}xmm0
end;

function TBZVector2i.Length: Single; assembler; nostackframe; register;
asm
  movq     xmm0, [RDI]
  cvtdq2ps xmm0, xmm0
  mulps    xmm0, xmm0
  movshdup xmm1, xmm0
  addps    xmm0, xmm1
  sqrtss   xmm0, xmm0
end;

function TBZVector2i.LengthSquare: Single;assembler; nostackframe; register;
asm
  movq     xmm0, [RDI]
  cvtdq2ps xmm0, xmm0
  mulps    xmm0, xmm0
  movshdup xmm1, xmm0
  addps    xmm0, xmm1
end;

function TBZVector2i.Distance(constref A: TBZVector2i): Single;assembler; nostackframe; register;
asm
  movq     xmm0, [RDI]
  movq     xmm1, [A]
  cvtdq2ps xmm0, xmm0
  cvtdq2ps xmm1, xmm1
  subps    xmm0, xmm1
  mulps    xmm0, xmm0
  movshdup xmm1, xmm0
  addps    xmm0, xmm1
  sqrtss   xmm0, xmm0
end;

function TBZVector2i.DistanceSquare(constref A: TBZVector2i): Single;assembler; nostackframe; register;
asm
  movq     xmm0, [RDI]
  movq     xmm1, [A]
  cvtdq2ps xmm0, xmm0
  cvtdq2ps xmm1, xmm1
  subps    xmm0, xmm1
  mulps    xmm0, xmm0
  movshdup xmm1, xmm0
  addps    xmm0, xmm1
end;

function TBZVector2iHelper.Normalize: TBZVector2f;assembler; nostackframe; register;
asm
  movq     xmm2, [RDI]    // Y X
  cvtdq2ps xmm2, xmm2
  movaps   xmm0, xmm2
  mulps    xmm2, xmm2      // Y*Y X*X
  pshufd   xmm1, xmm2, $01 // X*X Y*Y
  addps    xmm2, xmm1      // (X*X+Y*Y) (2x)
  sqrtps   xmm2, xmm2
  divps    xmm0, xmm2
end;

// function Reflect(I, NRef : TVector2i):TVector2i

function TBZVector2i.Abs: TBZVector2i; assembler; nostackframe; register;
asm
  movq      xmm0, [RDI]
  pxor      xmm1, xmm1     // 0 out xmm1
  pcmpgtd   xmm1, xmm0     // set all 1s where neg
  movdqa    xmm2, xmm1     // make copy of above
  pand      xmm2, [RIP+cSSE_MASK_ONE]  // mask off one where negative
  pxor      xmm0, xmm1     // flip bit if negative
  paddd     xmm0, xmm2     // +1 if negative
  movq      RAX,  xmm0
end;

function TBZVector2iHelper.GetYX : TBZVector2i;
begin
   Result.Create(Self.Y,Self.X);
end;

{%endregion%}
