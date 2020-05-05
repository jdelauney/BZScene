function TBZVectorHelper.AverageNormal4(constref up, left, down, right: TBZVector4f): TBZVector4f; assembler; register;
asm
  vmovups xmm1, [EAX]
  vmovups xmm2, [up]          //in edx
  vsubps  xmm2, xmm2, xmm1
  vmovups xmm3, [left]        //in ecx
  vsubps  xmm3, xmm3, xmm1
  mov     ecx,  [down]
  vmovups xmm4, [ecx]
  vsubps  xmm4, xmm4, xmm1
  mov     ecx,  [right]
  vmovups xmm5, [ecx]
  vsubps  xmm5, xmm5, xmm1

  vmovups xmm0, [cSSE_MASK_NO_W]
  vandps  xmm2, xmm2, xmm0
  vandps  xmm3, xmm3, xmm0
  vandps  xmm4, xmm4, xmm0
  vandps  xmm5, xmm5, xmm0

  vshufps xmm6, xmm2, xmm2, 11001001b
  vshufps xmm7, xmm3, xmm3, 11010010b
  vmulps  xmm6, xmm6, xmm7

  vshufps xmm7, xmm7, xmm7, 11010010b
  vshufps xmm1, xmm2, xmm2, 11010010b
  vmulps  xmm7, xmm7, xmm1

  vsubps  xmm0, xmm6, xmm7

  vshufps xmm6, xmm3, xmm3, 11001001b
  vshufps xmm7, xmm4, xmm4, 11010010b
  vmulps  xmm6, xmm6, xmm7

  vshufps xmm7, xmm7, xmm7, 11010010b
  vshufps xmm1, xmm3, xmm3, 11010010b
  vmulps  xmm7, xmm7, xmm1

  vsubps  xmm6, xmm6, xmm7
  vaddps  xmm0, xmm0, xmm6

  vshufps xmm6, xmm4, xmm4, 11001001b
  vshufps xmm7, xmm5, xmm5, 11010010b
  vmulps  xmm6, xmm6, xmm7

  vshufps xmm7, xmm7, xmm7, 11010010b
  vshufps xmm1, xmm4, xmm4, 11010010b
  vmulps  xmm7, xmm7, xmm1

  vsubps  xmm6, xmm6, xmm7
  vaddps  xmm0, xmm0, xmm6

  vshufps xmm6, xmm5, xmm5, 11001001b
  vshufps xmm7, xmm2, xmm2, 11010010b
  vmulps  xmm6, xmm6, xmm7

  vshufps xmm7, xmm7, xmm7, 11010010b
  vshufps xmm1, xmm5, xmm5, 11010010b
  vmulps  xmm7, xmm7, xmm1

  vsubps  xmm6, xmm6, xmm7
  vaddps  xmm0, xmm0, xmm6

  vmovaps xmm2, xmm0
  vmulps  xmm0, xmm0, xmm0
  vhaddps xmm0, xmm0, xmm0
  vhaddps xmm0, xmm0, xmm0

  vsqrtps xmm0, xmm0
  vdivps  xmm0, xmm2, xmm0

  mov       ebx, [Result]
  vmovups [ebx], xmm0
end;
