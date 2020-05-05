function TBZVectorHelper.PointProject(constref origin, direction : TBZVector) : Single; assembler;
asm
  movups xmm0, [EAX]
  movups xmm1, [Origin]
  movups xmm2, [Direction]
  subps  xmm0, xmm1
  mulps  xmm0, xmm2
  andps  xmm0, [cSSE_MASK_NO_W]
  movaps xmm1, xmm0
  movaps xmm2, xmm0
  shufps xmm0, xmm0, 00000000b
  shufps xmm1, xmm1, 01010101b
  shufps xmm2, xmm2, 10101010b
  addss  xmm0, xmm1
  addss  xmm0, xmm2
  movss  [Result], xmm0
end;

function TBZVectorHelper.AverageNormal4(constref up, left, down,right: TBZVector): TBZVector; assembler; register;
asm
  movups xmm1, [EAX]

  movups xmm2, [up]          //in edx
  subps  xmm2, xmm1
  movups xmm3, [left]        //in ecx
  subps  xmm3, xmm1
  mov    ebx,  [down]
  movups xmm4, [ebx]
  subps  xmm4, xmm1
  mov    ebx,  [right]
  movups xmm5, [ebx]
  subps  xmm5, xmm1

  //movups xmm0, [cSSE_MASK_NO_W]
  //andps  xmm2, xmm0
  //andps  xmm3, xmm0
  //andps  xmm4, xmm0
  //andps  xmm5, xmm0

  movaps xmm6, xmm2
  shufps xmm6, xmm6, 11001001b
  movaps xmm7, xmm3
  shufps xmm7, xmm7, 11010010b
  mulps  xmm6, xmm7

  shufps xmm7, xmm7, 11010010b
  movaps xmm1, xmm2
  shufps xmm1, xmm1, 11010010b
  mulps  xmm7, xmm1

  subps  xmm6, xmm7
  movaps xmm0, xmm6
  movaps xmm6, xmm3
  shufps xmm6, xmm6, 11001001b
  movaps xmm7, xmm4
  shufps xmm7, xmm7, 11010010b
  mulps  xmm6, xmm7                  // s gone t in 7

  shufps xmm7, xmm7, 11010010b
  movaps xmm1, xmm3
  shufps xmm1, xmm1, 11010010b
  mulps  xmm7, xmm1

  subps  xmm6, xmm7
  addps  xmm0, xmm6

  movaps xmm6, xmm4
  shufps xmm6, xmm6, 11001001b
  movaps xmm7, xmm5
  shufps xmm7, xmm7, 11010010b
  mulps  xmm6, xmm7                  // s gone t in 7

  shufps xmm7, xmm7, 11010010b
  movaps xmm1, xmm4
  shufps xmm1, xmm1, 11010010b
  mulps  xmm7, xmm1

  subps  xmm6, xmm7
  addps  xmm0, xmm6

  movaps xmm6, xmm5
  shufps xmm6, xmm6, 11001001b
  movaps xmm7, xmm2
  shufps xmm7, xmm7, 11010010b
  mulps  xmm6, xmm7                  // s gone t in 7

  shufps xmm7, xmm7, 11010010b
  movaps xmm1, xmm5
  shufps xmm1, xmm1, 11010010b
  mulps  xmm7, xmm1

  subps  xmm6, xmm7
  addps  xmm0, xmm6

  movaps xmm2, xmm0
  mulps  xmm0, xmm0
  movaps xmm1, xmm0
  shufps xmm0, xmm1, $4e
  addps  xmm0, xmm1
  movaps xmm1, xmm0
  shufps xmm1, xmm1, $11
  addps  xmm0, xmm1
  sqrtps xmm0, xmm0
  divps  xmm2, xmm0
  movaps xmm0, xmm2
  mov    ebx,  [Result]
  vmovups [ebx], xmm0
end;








