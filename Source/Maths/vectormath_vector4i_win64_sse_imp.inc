{%region%----[ Operators ]--------------------------------------------------------}

class operator TBZVector4i.+(constref A, B: TBZVector4i): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa xmm0,[A]
  {$ifdef TEST}
    paddd  xmm0,[B]
  {$else}
    movaps xmm1,[B]
    paddd  xmm0, xmm1
  {$endif}
  movdqa [RESULT], xmm0
end;

class operator TBZVector4i.-(constref A, B: TBZVector4i): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa xmm0,[A]
  {$ifdef TEST}
    psubd  xmm0,[B]
  {$else}
    movdqa xmm1,[B]
    psubd  xmm0, xmm1
  {$endif}
  movdqa [RESULT], xmm0
end;

class operator TBZVector4i.*(constref A, B: TBZVector4i): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa xmm0,[A]
  movdqa  xmm1,[B]
  cvtdq2ps xmm0,xmm0
  cvtdq2ps xmm1,xmm1
  mulps  xmm0,xmm1
  cvtps2dq xmm0, xmm0
  movdqa [RESULT], xmm0
end;

class operator TBZVector4i.div(constref A, B: TBZVector4i): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa xmm0,[A]
  movdqa xmm1,[B]
  cvtdq2ps xmm0,xmm0
  cvtdq2ps xmm1,xmm1
  divps  xmm0,xmm1

 //cvtps2dq xmm0, xmm0
  push rcx
  {$i macro_sse_win64_trunc_xmm0-xmm0.inc}
  pop rcx

  movdqa [RCX], xmm0
end;

class operator TBZVector4i.+(constref A: TBZVector4i; constref B:Longint): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa xmm0,[A]
  movd  xmm1,[B]
  pshufd xmm1, xmm1, $00
  paddd  xmm0,xmm1
  movdqa [RESULT], xmm0
end;

class operator TBZVector4i.-(constref A: TBZVector4i; constref B:Longint): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa xmm0,[A]
  movd  xmm1,[B]
  pshufd xmm1, xmm1, $00
  psubd  xmm0, xmm1
  movdqa [RESULT], xmm0
end;

class operator TBZVector4i.*(constref A: TBZVector4i; constref B:Single): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa xmm0,[A]
  cvtdq2ps xmm0,xmm0
  movlps  xmm1,[B]
  shufps xmm1, xmm1, $00
  mulps  xmm0,xmm1
  cvtps2dq xmm0, xmm0
  movdqa [RESULT], xmm0
end;

class operator TBZVector4i.*(constref A: TBZVector4i; constref B:Longint): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa xmm0,[A]
  movd  xmm1,[B]
  cvtdq2ps xmm0,xmm0
  cvtdq2ps xmm1,xmm1
  shufps xmm1, xmm1, $00
  mulps  xmm0,xmm1
  cvtps2dq xmm0, xmm0
  movdqa [RESULT], xmm0
end;

class operator TBZVector4i.Div(constref A: TBZVector4i; constref B:Longint): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa xmm0,[A]
  movd  xmm1,[B]
  cvtdq2ps xmm0,xmm0
  cvtdq2ps xmm1,xmm1
  shufps xmm1, xmm1, $00
  divps xmm0,xmm1
  //cvtps2dq xmm0, xmm0
  push rcx
  {$i macro_sse_win64_trunc_xmm0-xmm0.inc}
  pop rcx
  movdqa [RESULT], xmm0
end;

class operator TBZVector4i.-(constref A: TBZVector4i): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa xmm1,[A]
  movdqa xmm0,[RIP+cNullVector4i]
  psubd  xmm0,xmm1
  movdqa [RESULT],xmm0
End;

class operator TBZVector4i.= (constref A, B: TBZVector4i): boolean; assembler; nostackframe; register;
asm
  movdqa xmm0,[A]
  pcmpeqd   xmm0, [B]
  movmskps eax, xmm0
  xor eax, $F
  setz al
end;

class operator TBZVector4i.<> (constref A, B: TBZVector4i): boolean; assembler; nostackframe; register;
asm
  movdqa xmm0,[A]
  pcmpeqd   xmm0, [B]
  movmskps eax, xmm0
  //movmskps eax, xmm0
  xor eax, $f  //xor
  setnz al
end;

{%endregion%}

{%region%----[ Functions ]--------------------------------------------------------}

function TBZVector4i.Abs: TBZVector4i; assembler; nostackframe; register;
asm
  movdqa    xmm0, [RCX]
  pxor      xmm1, xmm1     // 0 out xmm1
  pcmpgtd   xmm1, xmm0     // set all 1s where neg
  movdqa    xmm2, xmm1     // make copy of above
  pand      xmm2, [RIP+cSSE_MASK_ONE]  // mask off one where negative
  pxor      xmm0, xmm1     // flip bit if negative
  paddd     xmm0, xmm2     // +1 if negative
  movdqa   [RESULT], xmm0
end;

function TBZVector4i.DivideBy2:TBZVector4i;assembler; nostackframe; register;
asm
  movdqa xmm0,[RCX]
  PSRLD xmm0, 1
  movdqa [RESULT],xmm0
end;

(*function TBZVector4i.Make(constref x,y,z:Integer;Constref w:Integer =1); assembler; nostackframe; register;
asm
  movd        xmm0,dword ptr [C3]
  movd        xmm1,dword ptr[C2]
  movd        xmm2,dword ptr[C1]
  movd        xmm3,dword ptr[C0]
  punpckldq   xmm3,xmm1
  punpckldq   xmm2,xmm0
  punpckldq   xmm3,xmm2 // XMM3 = vTemp
  // Mask off the low bits
  PAND    XMM3, [g_vMask1] // vTemp = _mm_and_si128(vTemp,g_vMask1);
  // 0xFFFFFFFF on true bits
  PCMPEQD XMM3, [g_vMask1] // vTemp = _mm_cmpeq_epi32(vTemp,g_vMask1);
  // 0xFFFFFFFF -> 1.0f, 0x00000000 -> 0.0f
  PAND    XMM3, [g_XMOne] // vTemp = _mm_and_si128(vTemp,g_XMOne);
  MOVUPS  TXMVECTOR([result]), XMM3// return _mm_castsi128_ps(vTemp);
end;  *)

function TBZVector4i.Min(constref B: Longint): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa xmm0,[RCX]
  movd  xmm1,[B]
  cvtdq2ps xmm0,xmm0
  cvtdq2ps xmm1,xmm1
  shufps xmm1, xmm1, $00
  minps  xmm0,xmm1
  cvtps2dq xmm0, xmm0
  movdqa [RESULT], xmm0
end;

function TBZVector4i.Min(constref B: TBZVector4i): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa xmm0,[RCX]
  movdqa  xmm1,[B]
  cvtdq2ps xmm0,xmm0
  cvtdq2ps xmm1,xmm1
  minps  xmm0,xmm1
  cvtps2dq xmm0, xmm0
  movdqa [RESULT], xmm0
end;

function TBZVector4i.Max(constref B: Longint): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa xmm0,[RCX]
  movd  xmm1,[B]
  cvtdq2ps xmm0,xmm0
  cvtdq2ps xmm1,xmm1
  shufps xmm1, xmm1, $00
  maxps  xmm0,xmm1
  cvtps2dq xmm0, xmm0
  movdqa [RESULT], xmm0
end;

function TBZVector4i.Max(constref B: TBZVector4i): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa xmm0,[RCX]
  movdqa  xmm1,[B]
  cvtdq2ps xmm0,xmm0
  cvtdq2ps xmm1,xmm1
  maxps  xmm0,xmm1
  cvtps2dq xmm0, xmm0
  movdqa [RESULT], xmm0
end;

function TBZVector4i.Clamp(constref AMin, AMax: Longint): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa xmm0,[RCX]
  cvtdq2ps xmm0,xmm0
  {$IFDEF TEST}
  movd  xmm1,[AMin]     //|0|0|0|x|
  punpckldq xmm1, xmm1	//|0|0|x|x|
  punpckldq xmm1, xmm1	//|x|x|x|x|
  movd  xmm2,[AMax]
  punpckldq xmm2, xmm2
  punpckldq xmm2, xmm2
  cvtdq2ps xmm1,xmm1
  cvtdq2ps xmm2,xmm2
  {$else}
  movd  xmm1,[AMin]
  cvtdq2ps xmm1,xmm1
  shufps xmm1, xmm1, $00
  movd  xmm2,[AMax]
  cvtdq2ps xmm2,xmm2
  shufps xmm2, xmm2, $00
  {$endif}
  maxps  xmm0,xmm1
  minps xmm0,xmm2
  cvtps2dq xmm0, xmm0
  movdqa [RESULT], xmm0
end;

function TBZVector4i.Clamp(Constref AMin, AMax: TBZVector4i): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa xmm0,[RCX]
  movdqa  xmm1,[AMin]
  movdqa  xmm2,[AMax]
  cvtdq2ps xmm0,xmm0
  cvtdq2ps xmm1,xmm1
  cvtdq2ps xmm2,xmm2
  maxps  xmm0,xmm1
  minps xmm0,xmm2
  cvtps2dq xmm0, xmm0
  movdqa [RESULT], xmm0
end;

function TBZVector4i.MulAdd(Constref B, C: TBZVector4i): TBZVector4i; assembler; nostackframe; register;
asm
  movdqa xmm0,[RCX]
  movdqa  xmm1,[B]
  movdqa  xmm2,[C]
  cvtdq2ps xmm0,xmm0
  cvtdq2ps xmm1,xmm1
  mulps  xmm0, xmm1
  cvtps2dq xmm0, xmm0
  paddd xmm0, xmm2
  movdqa [RESULT], xmm0
end;

function TBZVector4i.MulDiv(Constref B, C: TBZVector4i): TBZVector4i; assembler; register; nostackframe;
asm
  movdqa xmm0,[RCX]
  movdqa  xmm1,[B]
  movdqa  xmm2,[C]
  cvtdq2ps xmm0,xmm0
  cvtdq2ps xmm1,xmm1
  cvtdq2ps xmm2,xmm2
  // andps xmm2,[RIP+ cOneVector4f]
  mulps  xmm0, xmm1
  divps  xmm0, xmm2

  {$i macro_sse_win64_trunc_xmm0-xmm0.inc}

  movdqa   [RESULT], xmm0
end;

 function TBZVector4i.Combine(constref V2: TBZVector4i; constref F1: Single): TBZVector4i;assembler; nostackframe; register;
asm
  movdqa xmm1,[RCX]
  movdqa  xmm0,[V2]
  cvtdq2ps xmm0,xmm0
  movlps  xmm2,[F1]
  shufps xmm2, xmm2, $00 // replicate
  //unpcklps xmm2, xmm2
  //unpcklps xmm2, xmm2
  andps xmm0, [RIP+cSSE_MASK_NO_W]
  mulps xmm0, xmm2 //V2*F1
  //andps xmm0, [RIP+cSSE_MASK_NO_W]
  cvtps2dq xmm0, xmm0
  {.$i macro_sse_win64_trunc_xmm0-xmm0.inc} //RAISE A SIGFPE WITH TIMING TEST
  paddd xmm0, xmm1
  movdqa [RESULT], xmm0
end;

function TBZVector4i.Combine2(constref V2: TBZVector4i; const F1, F2: Single): TBZVector4i;assembler; //nostackframe; register;
asm
  movdqa  xmm0, [RCX]
  movd    xmm4, [RCX]12
  movdqa  xmm1, [V2]

  cvtdq2ps xmm0,xmm0
  cvtdq2ps xmm1,xmm1

  movss  xmm2,[F2{%H-}]
  shufps xmm2, xmm2, $00 // replicate F2
  shufps xmm3, xmm3, $00 // replicate F1

  mulps xmm0, xmm3  // Self * F1
  mulps xmm1, xmm2  // V2 * F2
  addps xmm0, xmm1  // (Self * F1) + (V2 * F2)
  andps xmm0, [RIP+cSSE_MASK_NO_W]
  cvtps2dq xmm0, xmm0
  shufps xmm4, xmm4, 00111111b
  paddd   xmm0, xmm4

  movdqa  [RESULT],  xmm0

end;

function TBZVector4i.Combine3(constref V2, V3: TBZVector4i; const F1, F2, F3: Single): TBZVector4i;  assembler;// nostackframe; register;
asm
  movdqa xmm0,[RCX]
  movdqa  xmm1,[V2]
  movdqa  xmm4,[V3]
  movd    xmm6, [RCX]12
  cvtdq2ps xmm0,xmm0
  cvtdq2ps xmm1,xmm1
  cvtdq2ps xmm4,xmm4

  movss xmm2, [F1{%H-}]
  movss xmm3, [F2{%H-}]
  movss xmm5, [F3{%H-}]

  shufps xmm2, xmm2, $00 // replicate F2
  shufps xmm3, xmm3, $00 // replicate F2
  shufps xmm5, xmm5, $00 // replicate F2

  mulps xmm0, xmm2 // Self * F1
  mulps xmm1, xmm3 // V2 * F2
  mulps xmm4, xmm5 // V3 * F3

  addps xmm0, xmm1 // (Self * F1) + (V2 * F2)
  addps xmm0, xmm4 // ((Self * F1) + (V2 * F2)) + (V3 * F3)

  andps xmm0, [RIP+cSSE_MASK_NO_W]
  cvtps2dq xmm0, xmm0
  shufps xmm6, xmm6, 00111111b
  paddd   xmm0, xmm6
  movdqa  [RESULT],  xmm0
end;



{%endregion%}

{%region%----[ Operations on Self ]-----------------------------------------------}



{%endregion%}
