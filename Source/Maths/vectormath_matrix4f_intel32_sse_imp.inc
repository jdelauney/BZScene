
{%region%----[ Operators ]------------------------------------------------------}

class operator TBZMatrix4f.+(constref A, B: TBZMatrix4f): TBZMatrix4f; assembler;
asm
  movups xmm0, [A]$00   // Load 4 rows of A  //0
  movups xmm1, [A]$10                        //16
  movups xmm2, [A]$20                        //32
  movups xmm3, [A]$30                        //48
  movups xmm4, [B]$00   // Load 2 rows of B
  movups xmm5, [B]$10
  addps  xmm0, xmm4     // Add rows
  addps  xmm1, xmm5
  movups xmm4, [B]$20   // Load 2 rows of B
  movups xmm5, [B]$30
  addps  xmm2, xmm4     // Add rows
  addps  xmm3, xmm5
  movups [Result]$00, xmm0
  movups [Result]$10, xmm1
  movups [Result]$20, xmm2
  movups [Result]$30, xmm3
end;

class operator TBZMatrix4f.-(constref A, B: TBZMatrix4f): TBZMatrix4f; assembler;
asm
  movups xmm0, [A]$00
  movups xmm1, [A]$10
  movups xmm2, [A]$20
  movups xmm3, [A]$30
  movups xmm4, [B]$00
  movups xmm5, [B]$10
  subps  xmm0, xmm4
  subps  xmm1, xmm5
  movups xmm4, [B]$20
  movups xmm5, [B]$30
  subps  xmm2, xmm4
  subps  xmm3, xmm5
  movups [Result]$00, xmm0
  movups [Result]$10, xmm1
  movups [Result]$20, xmm2
  movups [Result]$30, xmm3
end;

class operator TBZMatrix4f.+(constref A: TBZMatrix4f; constref B: Single): TBZMatrix4f; assembler;
asm
  movups xmm0, [A]$00     // Load 4 rows
  movups xmm1, [A]$10
  movups xmm2, [A]$20
  movups xmm3, [A]$30
  movss  xmm4, [B]
  shufps xmm4, xmm4, 0    // Replicate B

  addps  xmm0, xmm4       // Add B to each row
  addps  xmm1, xmm4
  addps  xmm2, xmm4
  addps  xmm3, xmm4

  movups [Result]$00, xmm0
  movups [Result]$10, xmm1
  movups [Result]$20, xmm2
  movups [Result]$30, xmm3
end;

class operator TBZMatrix4f.-(constref A: TBZMatrix4f; constref B: Single): TBZMatrix4f; assembler;
asm
  movups xmm0, [A]$00
  movups xmm1, [A]$10
  movups xmm2, [A]$20
  movups xmm3, [A]$30
  movss  xmm4, [B]
  shufps xmm4, xmm4, 0

  subps  xmm0, xmm4
  subps  xmm1, xmm4
  subps  xmm2, xmm4
  subps  xmm3, xmm4

  movups [Result]$00, xmm0
  movups [Result]$10, xmm1
  movups [Result]$20, xmm2
  movups [Result]$30, xmm3
end;

class operator TBZMatrix4f.*(constref A: TBZMatrix4f; constref B: Single): TBZMatrix4f; assembler;
asm
  movups xmm0, [A]$00
  movups xmm1, [A]$10
  movups xmm2, [A]$20
  movups xmm3, [A]$30
  movss  xmm4, [B]
  shufps xmm4, xmm4, 0

  mulps  xmm0, xmm4
  mulps  xmm1, xmm4
  mulps  xmm2, xmm4
  mulps  xmm3, xmm4

  movups [Result]$00, xmm0
  movups [Result]$10, xmm1
  movups [Result]$20, xmm2
  movups [Result]$30, xmm3
end;

// https://github.com/WojciechMula/toys/blob/master/sse/sse-matvecmult.c
class operator TBZMatrix4f.*(constref A: TBZMatrix4f; constref B: TBZVector4f): TBZVector4f; assembler;
asm
  movups   xmm0, [B]      // Load vector
  movups   xmm4, [A]$00   // Load 1st row
  movaps   xmm1, xmm0
  movaps   xmm2, xmm0
  movaps   xmm3, xmm0
  movups   xmm5, [A]$10  // Load 2nd row
  mulps    xmm0, xmm4    // (Bx * A00), (By * A01), (Bz * A02), (Bw * A03)
  mulps    xmm1, xmm5    // (Bx * A10), (By * A11), (Bz * A12), (Bw * A13)
  movups   xmm4, [A]$20  // Load 3rd row
  movups   xmm5, [A]$30  // Load 4st row
  mulps    xmm2, xmm4    // (Bx * A20), (By * A21), (Bz * A22), (Bw * A23)
  mulps    xmm3, xmm5    // (Bx * A30), (By * A31), (Bz * A32), (Bw * A33)

  { Transpose }
  movaps   xmm4, xmm2
  unpcklps xmm2, xmm3    // A32 A22 A33 A23
  unpckhps xmm4, xmm3    // A30 A20 A31 A21

  movaps   xmm3, xmm0
  unpcklps xmm0, xmm1    // A12 A02 A13 A03
  unpckhps xmm3, xmm1    // A10 A00 A11 A01

  movaps   xmm1, xmm0
  unpcklpd xmm0, xmm2    // A33 A23 A13 A03
  unpckhpd xmm1, xmm2    // A32 A22 A12 A02

  movaps   xmm2, xmm3
  unpcklpd xmm2, xmm4    // A31 A21 A11 A01
  unpckhpd xmm3, xmm4    // A30 A20 A10 A00

  addps    xmm0, xmm1    // Add rows
  addps    xmm2, xmm3
  addps    xmm0, xmm2
  movups   [Result], xmm0
end;


// https://stackoverflow.com/questions/18499971/efficient-4x4-matrix-multiplication-c-vs-assembly
// http://www.mindfruit.co.uk/2012/02/avx-matrix-multiplication-or-something.html
// https://stackoverflow.com/questions/14967969/efficient-4x4-matrix-vector-multiplication-with-sse-horizontal-add-and-dot-prod
class operator TBZMatrix4f.*(constref A, B: TBZMatrix4f): TBZMatrix4f; assembler;
{ Code below consists of 4 Vector*Matrix calculations }
asm
(*  snip from gcc
"movaps   (%0), %%xmm0\n"c	/* xmm0 = pRight[0..3] */
"movaps 16(%0), %%xmm1\n"	/* xmm1 = pRight[5..7] */
"movaps 32(%0), %%xmm2\n"	/* xmm2 = pRight[8..11] */
"movaps 48(%0), %%xmm3\n"	/* xmm3 = pRight[12..15] */

/* Processes 1/2 of the matrix at a time (2x4), unrolled loop */
"movss    (%1), %%xmm4\n"
"movss   4(%1), %%xmm6\n"
"movss  16(%1), %%xmm5\n"
"movss  20(%1), %%xmm7\n"
"shufps $0x00, %%xmm4, %%xmm4\n"
"shufps $0x00, %%xmm5, %%xmm5\n"
"shufps $0x00, %%xmm6, %%xmm6\n"
"shufps $0x00, %%xmm7, %%xmm7\n"
"mulps %%xmm0, %%xmm4\n"
"mulps %%xmm0, %%xmm5\n"
"mulps %%xmm1, %%xmm6\n"
"mulps %%xmm1, %%xmm7\n"
"addps %%xmm7, %%xmm5\n"
"addps %%xmm6, %%xmm4\n"


"movss  8(%1), %%xmm6\n"
"movss 24(%1), %%xmm7\n"
"shufps $0x00, %%xmm6, %%xmm6\n"
"shufps $0x00, %%xmm7, %%xmm7\n"
"mulps %%xmm2, %%xmm6\n"
"mulps %%xmm2, %%xmm7\n"
"addps %%xmm6, %%xmm4\n"
"addps %%xmm7, %%xmm5\n"

"movss  12(%1), %%xmm6\n"
"movss  28(%1), %%xmm7\n"
"shufps $0x00, %%xmm6, %%xmm6\n"
"shufps $0x00, %%xmm7, %%xmm7\n"
"mulps %%xmm3, %%xmm6\n"
"mulps %%xmm3, %%xmm7\n"
"addps %%xmm6, %%xmm4\n"
"addps %%xmm7, %%xmm5\n"

"movaps %%xmm4, (%1)\n"
"movaps %%xmm5, 16(%1)\n"

/* second half of the matrix */
"movss  32(%1), %%xmm4\n"
"movss  36(%1), %%xmm6\n"
"movss  48(%1), %%xmm5\n"
"movss  52(%1), %%xmm7\n"
"shufps $0x00, %%xmm4, %%xmm4\n"
"shufps $0x00, %%xmm5, %%xmm5\n"
"mulps %%xmm0, %%xmm4\n"
"mulps %%xmm0, %%xmm5\n"

"shufps $0x00, %%xmm6, %%xmm6\n"
"shufps $0x00, %%xmm7, %%xmm7\n"
"mulps %%xmm1, %%xmm6\n"
"mulps %%xmm1, %%xmm7\n"
"addps %%xmm6, %%xmm4\n"
"addps %%xmm7, %%xmm5\n"


"movss 40(%1), %%xmm6\n"
"movss 56(%1), %%xmm7\n"
"shufps $0x00, %%xmm6, %%xmm6\n"
"shufps $0x00, %%xmm7, %%xmm7\n"
"mulps %%xmm2, %%xmm6\n"
"mulps %%xmm2, %%xmm7\n"
"addps %%xmm6, %%xmm4\n"
"addps %%xmm7, %%xmm5\n"

"movss  44(%1), %%xmm6\n"
"movss  60(%1), %%xmm7\n"
"shufps $0x00, %%xmm6, %%xmm6\n"
"shufps $0x00, %%xmm7, %%xmm7\n"
"mulps %%xmm3, %%xmm6\n"
"mulps %%xmm3, %%xmm7\n"
"addps %%xmm6, %%xmm4\n"
"addps %%xmm7, %%xmm5\n"

"movaps %%xmm4, 32(%1)\n"
"movaps %%xmm5, 48(%1)\n"
*)

  { A.V[0] * B }
  movups xmm0, [A]$00
  movups xmm4, [B]$00
  movaps xmm1, xmm0
  movaps xmm2, xmm0
  movaps xmm3, xmm0
  shufps xmm0, xmm0, $00
  shufps xmm1, xmm1, $55
  shufps xmm2, xmm2, $AA
  shufps xmm3, xmm3, $FF
  movups xmm5, [B]$10
  movups xmm6, [B]$20
  movups xmm7, [B]$30
  mulps  xmm0, xmm4
  mulps  xmm1, xmm5
  mulps  xmm2, xmm6
  mulps  xmm3, xmm7
  addps  xmm0, xmm1
  addps  xmm2, xmm3
  addps  xmm0, xmm2
  movups [Result]$00, xmm0

  { A.V[1] * B }
  movups xmm0, [A]$10
  movaps xmm1, xmm0
  movaps xmm2, xmm0
  movaps xmm3, xmm0
  shufps xmm0, xmm0, $00
  shufps xmm1, xmm1, $55
  shufps xmm2, xmm2, $AA
  shufps xmm3, xmm3, $FF
  mulps  xmm0, xmm4
  mulps  xmm1, xmm5
  mulps  xmm2, xmm6
  mulps  xmm3, xmm7
  addps  xmm0, xmm1
  addps  xmm2, xmm3
  addps  xmm0, xmm2
  movups [Result]$10, xmm0

  { A.V[2] * B }
  movups xmm0, [A]$20
  movaps xmm1, xmm0
  movaps xmm2, xmm0
  movaps xmm3, xmm0
  shufps xmm0, xmm0, $00
  shufps xmm1, xmm1, $55
  shufps xmm2, xmm2, $AA
  shufps xmm3, xmm3, $FF
  mulps  xmm0, xmm4
  mulps  xmm1, xmm5
  mulps  xmm2, xmm6
  mulps  xmm3, xmm7
  addps  xmm0, xmm1
  addps  xmm2, xmm3
  addps  xmm0, xmm2
  movups [Result]$20, xmm0

  { A.V[3] * B }
  movups xmm0, [A]$30
  movaps xmm1, xmm0
  movaps xmm2, xmm0
  movaps xmm3, xmm0
  shufps xmm0, xmm0, $00
  shufps xmm1, xmm1, $55
  shufps xmm2, xmm2, $AA
  shufps xmm3, xmm3, $FF
  mulps  xmm0, xmm4
  mulps  xmm1, xmm5
  mulps  xmm2, xmm6
  mulps  xmm3, xmm7
  addps  xmm0, xmm1
  addps  xmm2, xmm3
  addps  xmm0, xmm2
  movups [Result]$30, xmm0
end;

class operator TBZMatrix4f./(constref A: TBZMatrix4f; constref B: Single): TBZMatrix4f; assembler;
asm
  movups xmm0, [A]$00
  movups xmm1, [A]$10
  movups xmm2, [A]$20
  movups xmm3, [A]$30
  movss  xmm4, [B]
  shufps xmm4, xmm4, 0

  divps  xmm0, xmm4
  divps  xmm1, xmm4
  divps  xmm2, xmm4
  divps  xmm3, xmm4

  movups [Result]$00, xmm0
  movups [Result]$10, xmm1
  movups [Result]$20, xmm2
  movups [Result]$30, xmm3
end;

class operator TBZMatrix4f.-(constref A: TBZMatrix4f): TBZMatrix4f; assembler;
asm
  movups xmm0, [cSSE_MASK_NEGATE]  // Load mask with 4 sign (upper) bits
  movups xmm1, [A]$00 // Load 4 rows
  movups xmm2, [A]$10
  movups xmm3, [A]$20
  movups xmm4, [A]$30
  xorps  xmm1, xmm0             // Flip sign bits of each element in each row
  xorps  xmm2, xmm0
  xorps  xmm3, xmm0
  xorps  xmm4, xmm0
  movups [Result]$00, xmm1
  movups [Result]$10, xmm2
  movups [Result]$20, xmm3
  movups [Result]$30, xmm4
end;

{%endregion%}

{%region%----[ Functions ]------------------------------------------------------}
function  TBZMatrix4f.GetDeterminant: single; assembler;
asm
  movups xmm0, [EAX]$20         //A2

  movups xmm3, [EAX]$30         //A3

  //XMVECTOR V4 = XMVectorSwizzle<XM_SWIZZLE_Z, XM_SWIZZLE_Z, XM_SWIZZLE_Y, XM_SWIZZLE_Y>(M.r[2]);
  movaps xmm2, xmm0            // A2
  shufps xmm2, xmm2, 01011010b // V4   ZZYY

  //XMVECTOR V0 = XMVectorSwizzle<XM_SWIZZLE_Y, XM_SWIZZLE_X, XM_SWIZZLE_X, XM_SWIZZLE_X>(M.r[2]);
  //XMVECTOR V2 = XMVectorSwizzle<XM_SWIZZLE_Y, XM_SWIZZLE_X, XM_SWIZZLE_X, XM_SWIZZLE_X>(M.r[2]);
  shufps xmm0, xmm0,00000001b   // V0 YXXX  A2
  movaps xmm1, xmm0            // V2 YXXX  A2

  //XMVECTOR V3 = XMVectorSwizzle<XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_Z>(M.r[3]);
  //XMVECTOR V5 = XMVectorSwizzle<XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_Z>(M.r[3]);
  movaps xmm4, xmm3            // A3
  shufps xmm4, xmm4, 10111111b //  WWWZ  // BOTH v£ v%

  //XMVECTOR V1 = XMVectorSwizzle<XM_SWIZZLE_Z, XM_SWIZZLE_Z, XM_SWIZZLE_Y, XM_SWIZZLE_Y>(M.r[3]);
  shufps xmm3, xmm3, 01011010b //   ZZYY

  mulps  xmm0, xmm3      // V0 * V1   P0
  mulps  xmm1, xmm4      // V2 * V3   P1
  mulps  xmm2, xmm4      // V4 * V5   P2

  movups xmm3, [EAX]$20

  //V2 = XMVectorSwizzle<XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_Z>(M.r[2]);
  //V4 = XMVectorSwizzle<XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_Z>(M.r[2]);
  movaps xmm4, xmm3
  shufps xmm4, xmm4, 10111111b
  movaps xmm5, xmm4

  //V0 = XMVectorSwizzle<XM_SWIZZLE_Z, XM_SWIZZLE_Z, XM_SWIZZLE_Y, XM_SWIZZLE_Y>(M.r[2]);
  shufps xmm3, xmm3, 01011010b

  movups xmm6, [EAX]$30

  //V3 = XMVectorSwizzle<XM_SWIZZLE_Y, XM_SWIZZLE_X, XM_SWIZZLE_X, XM_SWIZZLE_X>(M.r[3]);
  //V1 = XMVectorSwizzle<XM_SWIZZLE_Y, XM_SWIZZLE_X, XM_SWIZZLE_X, XM_SWIZZLE_X>(M.r[3]);
  movaps xmm7, xmm6
  shufps xmm7, xmm7, 00000001b

  //V5 = XMVectorSwizzle<XM_SWIZZLE_Z, XM_SWIZZLE_Z, XM_SWIZZLE_Y, XM_SWIZZLE_Y>(M.r[3]);
  shufps xmm6, xmm6, 01011010b

  mulps  xmm3, xmm7    //V0 * V1
  subps  xmm0, xmm3    // P0 - V0 * V1  IN XMM0 = P0
  mulps  xmm4, xmm7    // V2 * V3
  subps  xmm1, xmm4    // P1 - V2 * V3  IN XMM1 = P1
  mulps  xmm5, xmm6    // V4 * V5
  subps  xmm2, xmm5    // P2 - V4 * V5  IN XMM2 = P2   only 0,1,2 used at this point

  movups xmm3, [EAX]$10
  movaps xmm4, xmm3
  movaps xmm5, xmm3

  //V0 = XMVectorSwizzle<XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_Z>(M.r[1]);
  shufps xmm3, xmm3, 10111111b
  //V1 = XMVectorSwizzle<XM_SWIZZLE_Z, XM_SWIZZLE_Z, XM_SWIZZLE_Y, XM_SWIZZLE_Y>(M.r[1]);
  shufps xmm4, xmm4, 01011010b
  //V2 = XMVectorSwizzle<XM_SWIZZLE_Y, XM_SWIZZLE_X, XM_SWIZZLE_X, XM_SWIZZLE_X>(M.r[1]);
  shufps xmm5, xmm5, 00000001b


  //XMVECTOR R = XMVectorMultiply(V0, P0);
  mulps  xmm3, xmm0    // XMM3 = R

  //R = XMVectorNegativeMultiplySubtract(V1, P1, R);
  mulps  xmm1, xmm4    // P1 * V1
  subps  xmm3, xmm1    // R = R - P1 * V1

  //R = XMVectorMultiplyAdd(V2, P2, R);
  mulps  xmm2, xmm5   // P2 * V2
  addps  xmm3, xmm2   // R = R + P2 * V2

  //XMVECTOR S = XMVectorMultiply(M.r[0], Sign.v);
  movups xmm0, [EAX]$00        //A0
  mulps  xmm0, [cNegateVector4f_PNPN]    // S

// Dot  S.R
{$IFDEF USE_ASM_SSE_4}
   dpps xmm0, xmm3, 11110001b //or $F1
{$ELSE}
  {$IFDEF USE_ASM_SSE_3}
     mulps xmm0, xmm3
     {$ifdef TEST}
       movshdup    xmm4, xmm0
       addps       xmm0, xmm4
       movhlps     xmm4, xmm0
       addss       xmm0, xmm4
     {$else}
       haddps xmm0, xmm0
       haddps xmm0, xmm0
     {$endif}
  {$ELSE}
    mulps   xmm0, xmm3
    movhlps xmm4, xmm0
    addps   xmm4, xmm0     //|W*Y|Z*X|
    movaps  xmm0, xmm4
    shufps xmm0, xmm0, 00000001b
    addps  xmm0, xmm4
  {$ENDIF}
{$ENDIF}
 movss [Result], {%H-}xmm0
end;

function TBZMatrix4f.Transpose: TBZMatrix4f; assembler;
asm
movups [Result]$00, xmm1
movups [Result]$10, xmm2
movups [Result]$20, xmm3
movups [Result]$30, xmm4
end;

function TBZMatrix4f.Invert : TBZMatrix4f; assembler;
asm
movups [Result]$00, xmm1
movups [Result]$10, xmm2
movups [Result]$20, xmm3
movups [Result]$30, xmm4
end;

function TBZMatrix4f.Normalize : TBZMatrix4f; assembler;
asm
movups [Result]$00, xmm1
movups [Result]$10, xmm2
movups [Result]$20, xmm3
movups [Result]$30, xmm4
end;

function TBZMatrix4f.Translate( constref v : TBZVector):TBZMatrix4f; assembler;
asm
movups [Result]$00, xmm1
movups [Result]$10, xmm2
movups [Result]$20, xmm3
movups [Result]$30, xmm4
end;
function TBZMatrix4f.Multiply(constref M2: TBZMatrix4f):TBZMatrix4f; assembler;
asm
  movups xmm0, [EAX]$00   // Self[0]
  movups xmm1, [EAX]$10   // Self[1]
  movups xmm2, [EAX]$20   // Self[2]
  movups xmm3, [EAX]$30   // Self[3]

  movups xmm4, [M2]$00
  movups xmm5, [M2]$10
  movups xmm6, [M2]$20
  movups xmm7, [M2]$30
  // Component-wise multiplication
  mulps  xmm0, xmm4
  mulps  xmm1, xmm5
  mulps  xmm2, xmm6
  mulps  xmm3, xmm7

  movups [Result]$00, xmm0
  movups [Result]$10, xmm1
  movups [Result]$20, xmm2
  movups [Result]$30, xmm3
end;

{%endregion%}
