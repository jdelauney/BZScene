
function TBZHmgPlaneHelper.EvaluatePoint(constref Point : TBZVector) : Single; Assembler; register; nostackframe;
const
  cSSE_MASK_NO_W   : array [0..3] of UInt32 = ($FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $00000000);
asm
  movaps xmm0,[RDI]
  movaps xmm1, [Point]
  movaps xmm2, xmm1
  shufps xmm2, xmm2, $FF  // store plane.W
  andps xmm1, [RIP+cSSE_MASK_NO_W]
  mulps xmm0, xmm1
  // Horizontal add
  movshdup xmm3,xmm0
  addps  xmm0,xmm3
  movhlps xmm3,xmm0
  addss  xmm0,xmm3

  addss xmm0, xmm2 //+Plane.W
end;


