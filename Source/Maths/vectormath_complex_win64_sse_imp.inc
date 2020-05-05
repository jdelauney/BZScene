{%region%----[ Operators ]--------------------------------------------------------}

class operator TBZComplexVector.+(constref A, B: TBZComplexVector): TBZComplexVector; assembler; nostackframe; register;
asm
  movapd xmm0, [A]
  movapd xmm1, [B]
  addpd  xmm0, xmm1
  movapd [Result], xmm0 //Result = RCX
end;

class operator TBZComplexVector.-(constref A, B: TBZComplexVector): TBZComplexVector; assembler; nostackframe; register;
asm
  movapd xmm0, [A]
  movapd xmm1, [B]
  subpd  xmm0, xmm1
  movapd [Result], xmm0
end;


{%endregion%}

{%region%----[ Functions ]--------------------------------------------------------}


function TBZComplexVector.Length : Double; assembler; nostackframe; register;
asm
  movapd xmm0, [RCX]
  mulpd  xmm0, xmm0
  haddpd xmm0, xmm0
  //movddup xmm1, xmm0
  //addpd    xmm0, xmm1
  sqrtsd   xmm0, xmm0
end;


{%endregion%}
