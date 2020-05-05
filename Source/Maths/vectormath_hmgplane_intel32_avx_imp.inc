procedure TBZVectorHelper.CreatePlane(constref p1, p2, p3 : TBZVector);assembler;
asm
vxorps xmm0, xmm0, xmm0
end;

function TBZVectorHelper.NormalizePlane:TBZHmgPlane;assembler;
//   n:=1/Sqrt(Self.X*Self.X+Self.Y*Self.Y+Self.Z*Self.Z);
//   Result :=Self * n;
asm
vxorps xmm0, xmm0, xmm0
end;

function TBZVectorHelper.DistancePlaneToPoint(constref point : TBZVector) : Single; assembler;
asm
vxorps xmm0, xmm0, xmm0
movss [RESULT], {%H-}xmm0
end;

function TBZVectorHelper.DistancePlaneToSphere(constref Center : TBZVector; constref Radius:Single) : Single; assembler;
asm
vxorps xmm0, xmm0, xmm0
movss [RESULT], {%H-}xmm0
end;

function TBZVectorHelper.PointProject(constref origin, direction : TBZVector) : Single; assembler;
asm
vxorps xmm0, xmm0, xmm0
movss [RESULT], {%H-}xmm0
end;

function TBZHmgPlane.Perpendicular(constref P : TBZVector4f) : TBZVector4f; assembler; nostackframe; register;
asm
  vmovups xmm0, [P]
  vmovups xmm1, [EAX]
  vandps  xmm0, xmm0, [cSSE_MASK_NO_W]
  movaps  xmm2, xmm0
 // Self.Dot(N) RESULT in xmm0
  vdpps   xmm0, xmm0, xmm1, 01110001b //or $F1
  vShufps xmm0, xmm0, xmm0, $0
  vmulps  xmm0, xmm0, xmm2    // (Dot * Self)
  vsubps  xmm0, xmm2, xmm0    // self - (Dot * Self)
  vmovups [RESULT], xmm0
end;

function TBZHmgPlane.Reflect(constref V: TBZVector4f): TBZVector4f; assembler; nostackframe; register;
asm
 vmovups xmm0, [V]
 vmovups xmm1, [EAX]
 vmovaps xmm2, xmm1
 vmovaps xmm3, xmm0
 // N.Dot(SELF) RESULT in xmm1
 vdpps   xmm1, xmm1, xmm0, 01110001b //or $F1
 vShufps xmm1, xmm1, xmm1, $0        // Replicate
 vaddps  xmm1, xmm1, xmm1            // (2 * N.Dot(Self))
 vmulps  xmm1, xmm1, xmm2            // ((2 * N.Dot(Self)) * N)
 vsubps  xmm3, xmm3, xmm1            // Self - ((2 * N.Dot(Self)) * N)
 vmovups [RESULT], xmm3
end;

