procedure TBZHmgPlane.Create(constref p1, p2, p3 : TBZVector); assembler; register; nostackframe;
asm
  vmovaps  xmm0, [p1]
  vmovaps  xmm1, [p2]
  vmovaps  xmm2, [p3]

  vshufps  xmm4, xmm0, xmm0, 11001001b   //|p1 T1|
  vshufps  xmm5, xmm2, xmm2, 11001001b   //|p3 T1|
  vshufps  xmm6, xmm1, xmm1, 11001001b   //|p2 T1|

  vsubps   xmm3, xmm6, xmm5
  vsubps   xmm5, xmm5, xmm4
  vsubps   xmm4, xmm4, xmm6

  vmovaps  xmm6, xmm0

  vmulps   xmm0, xmm0, xmm3
  vmulps   xmm1, xmm1, xmm5
  vmulps   xmm2, xmm2, xmm4

  vaddps   xmm0, xmm0, xmm1
  vaddps   xmm0, xmm0, xmm2

  vdpps    xmm1, xmm0, xmm0, 01111111b
  vsqrtps  xmm1, xmm1
  vdivps   xmm0, xmm0, xmm1
  vshufps  xmm0, xmm0, xmm0, 11001001b

  vdpps    xmm1, xmm0, xmm6, 01110011b
  vxorps   xmm1, xmm1, [RIP+cSSE_MASK_NEGATE] // xmm1 now holds |-|-|D|D|
  vshufps  xmm1, xmm1, xmm0, 10100000b         // xmm1 now holds |C|C|D|D|
  vshufps  xmm0, xmm0, xmm1, 00110100b
  vmovaps [RDI], xmm0
end;


procedure TBZHmgPlane.Normalize; assembler; register; nostackframe;
asm
  vmovaps  xmm0, [RDI]
  vdpps    xmm1, xmm0, xmm0, 01111111b
  vsqrtps  xmm1, xmm1
  vdivps   xmm0, xmm0, xmm1
  vmovaps [RDI], xmm0
end;

function TBZHmgPlane.Normalized : TBZHmgPlane; assembler; register; nostackframe;
asm
  vmovaps xmm0, [RDI]
  vdpps   xmm1, xmm0, xmm0, 01111111b
  vsqrtps xmm1, xmm1
  vdivps  xmm0, xmm0, xmm1
  movhlps xmm1, xmm0
end;

function TBZHmgPlane.Distance(constref point : TBZVector) : Single;assembler; register; nostackframe;
asm
  vmovaps xmm0, [RDI]
  vdpps   xmm0, xmm0, [POINT], 01110001b
  vaddps  xmm0, xmm0, [RDI]12
end;

function TBZHmgPlane.AbsDistance(constref point : TBZVector) : Single; assembler; register; nostackframe;
asm
  vmovaps xmm0, [RDI]
  vdpps   xmm0, xmm0, [POINT], 01110001b
  vaddps  xmm0, xmm0, [RDI]12
  vandps  xmm0, xmm0, [RIP+cSSE_MASK_ABS]
end;


function TBZHmgPlane.Perpendicular(constref P : TBZVector4f) : TBZVector4f; assembler; nostackframe; register;
asm
  vmovaps xmm0, [P]
  vmovaps xmm1, [RDI]
  vandps  xmm0, xmm0, [RIP+cSSE_MASK_NO_W]
  vmovaps xmm2, xmm0
   // Self.Dot(N) Result in xmm0
  vdpps   xmm0, xmm0, xmm1, 01110001b //or $F1
  vShufps xmm0, xmm0, xmm0, $0
  vmulps  xmm0, xmm0, xmm2    // (Dot * Self)
  vsubps  xmm0, xmm0, xmm2    //(Dot * Self) * Self
  xorps   xmm0, [RIP+cSSE_MASK_NEGATE]
  movhlps xmm1, xmm0
end;

function TBZHmgPlane.Reflect(constref V: TBZVector4f): TBZVector4f; assembler; nostackframe; register;
asm
 vmovaps xmm0, [V]
 vmovaps xmm1, [RDI]
 vmovaps xmm2, xmm1
 vmovaps xmm3, xmm0
 // N.Dot(SELF) Result in xmm1
 vdpps   xmm1, xmm1, xmm0, 01110001b //or $F1
 vShufps xmm1, xmm1, xmm1, $0  // Replicate
 vaddps  xmm1, xmm1, xmm1  // (2 * N.Dot(Self))
 vmulps  xmm1, xmm1, xmm2 // ((2 * N.Dot(Self)) * N)
 vsubps  xmm3, xmm3, xmm1 // Self - ((2 * N.Dot(Self)) * N)
 vmovups xmm0, xmm3
 movhlps xmm1, xmm0
end;



