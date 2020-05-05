function TBZVectorHelper.PointProject(constref origin, direction : TBZVector) : Single; assembler; register; nostackframe;
asm
  vmovaps xmm0, [RDI]
  vmovaps xmm1, [Origin]
  vmovaps xmm2, [Direction]
  vsubps  xmm0, xmm0, xmm1
  vmulps  xmm0, xmm0, xmm2
  vandps  xmm0, xmm0, [RIP+cSSE_MASK_NO_W]
  vshufps xmm1, xmm0, xmm0, 01010101b
  vshufps xmm2, xmm0, xmm0, 10101010b
  vshufps xmm0, xmm0, xmm0, 00000000b
  vaddss  xmm0, xmm0, xmm1
  vaddss  xmm0, xmm0, xmm2
end;

function TBZVectorHelper.AverageNormal4(constref up, left, down,right: TBZVector): TBZVector; assembler; register; nostackframe;
asm
  vmovaps xmm2, [RDI]
  vsubps  xmm2, xmm2, [up]
  vmovaps xmm3, [RDI]
  vsubps  xmm3, xmm3, [left]
  vmovaps xmm4, [RDI]
  vsubps  xmm4, xmm4, [down]
  vmovaps xmm5, [RDI]
  vsubps  xmm5, xmm5, [right]

  vshufps xmm6,  xmm2,  xmm2, 11001001b
  vshufps xmm7,  xmm3,  xmm3, 11010010b
  vmulps  xmm0,  xmm6,  xmm7
  vshufps xmm8,  xmm2,  xmm2, 11010010b
  vshufps xmm9,  xmm3,  xmm3, 11001001b
  vmulps  xmm8,  xmm8,  xmm9
  vshufps xmm10, xmm3,  xmm3, 11001001b
  vshufps xmm11, xmm4,  xmm4, 11010010b
  vmulps  xmm10, xmm10, xmm11
  vshufps xmm12, xmm3,  xmm3, 11010010b
  vshufps xmm13, xmm4,  xmm4, 11001001b
  vmulps  xmm12, xmm12, xmm13

  vsubps  xmm10, xmm10, xmm12
  vsubps  xmm0,  xmm0,  xmm8
  vaddps  xmm0,  xmm0,  xmm10

  vshufps xmm6,  xmm4,  xmm4, 11001001b
  vshufps xmm7,  xmm5,  xmm5, 11010010b
  vshufps xmm8,  xmm4,  xmm4, 11010010b
  vshufps xmm9,  xmm5,  xmm5, 11001001b
  vshufps xmm10, xmm5,  xmm5, 11001001b
  vshufps xmm11, xmm2,  xmm2, 11010010b
  vshufps xmm12, xmm5,  xmm5, 11010010b
  vshufps xmm13, xmm2,  xmm2, 11001001b

  vmulps  xmm6,  xmm6,  xmm7
  vmulps  xmm8,  xmm8,  xmm9
  vmulps  xmm10, xmm10, xmm11
  vmulps  xmm12, xmm12, xmm13

  vaddps  xmm0, xmm0, xmm6
  vsubps  xmm0, xmm0, xmm8
  vaddps  xmm0, xmm0, xmm10
  vsubps  xmm0, xmm0, xmm12

  vmovaps xmm2, xmm0
  vmulps  xmm0, xmm0, xmm0
  vhaddps xmm0, xmm0, xmm0
  vhaddps xmm0, xmm0, xmm0

  vsqrtps xmm0, xmm0
  vdivps  xmm0, xmm2, xmm0
  movhlps xmm1, xmm0
end;





