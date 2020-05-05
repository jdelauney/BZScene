


function TBZVector2fHelper.Step(ConstRef B : TBZVector2f) : TBZVector2f;assembler; //register; nostackframe;
asm
  //if self.X <= B.X then result.X := 0;
  movss xmm0, [RCX]
  movlps xmm1, [B]
  movhps [Result], xmm0  // insure we have 0s in result
  comiss xmm0, xmm1
  jbe @NextStep
  movss [RESULT]0, {%H-}xmm0
@NextStep:
  //if self.Y <= B.Y then result.Y := 0;
  movlps xmm0, [RCX]4
  movlps xmm1, [B]4
  comiss xmm0, xmm1
  jbe @Finish
  movss [RESULT]4, {%H-}xmm0
@finish:
end;

function TBZVector2fHelper.Saturate : TBZVector2f;assembler; register; nostackframe;
asm
  movq xmm0, [RCX]
  movaps xmm1, [RIP+cOneVector4f]
  movaps xmm2, [RIP+cNullVector4f]
  maxps xmm0,xmm2
  minps xmm0,xmm1
  movq   rax,  {%H-}xmm0
end;

function TBZVector2fHelper.SmoothStep(ConstRef A, B : TBZVector2f) : TBZVector2f; assembler; register; nostackframe;
Const
  cVecSet3 : TBZVector4f = (x:3.0;y:3.0;z:3.0;w:3.0);
asm
  movq   xmm0, [RCX]
  //Saturate((self-a)/(b-a))
  movq   xmm1, {%H-}[A]
  subps  xmm0, xmm1
  movq   xmm2, {%H-}[B]
  subps  xmm2, xmm1
  divps  xmm0, xmm2
  maxps  xmm0, XMMWORD PTR [RIP+cNullVector4f]
  minps  xmm0, XMMWORD PTR [RIP+cOneVector4f]


  //result :=t*t * ((3.0 - t*2.0))
  movaps  xmm1, xmm0

  mulps   xmm0, xmm0
  movaps  xmm2, XMMWORD PTR [RIP+cVecSet3]
  addps   xmm1, xmm1            //t*2
  subps   xmm2, xmm1
  mulps   xmm0, xmm2
  movq   rax,  {%H-}xmm0
end;

function TBZVector2fHelper.Lerp(Constref B: TBZVector2f; Constref T:Single): TBZVector2f;assembler; register; nostackframe;
asm
  movq xmm0, [RCX]
  movq xmm1, {%H-}[B]
  movlps xmm2, [T]
  shufps xmm2, xmm2, 0
  subps xmm1, xmm0
  mulps xmm1,xmm2
  addps xmm0,xmm1
  movq   rax,  {%H-}xmm0
end;
