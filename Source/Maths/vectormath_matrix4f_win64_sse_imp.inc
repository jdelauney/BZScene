
{%region%----[ Operators ]--------------------------------------------------------}

class operator TBZMatrix4f.+(constref A, B: TBZMatrix4f): TBZMatrix4f; assembler; register; nostackframe;
asm
  movaps xmm0, [A]$00   // Load 4 rows of A  //0
  movaps xmm1, [A]$10                        //16
  movaps xmm2, [A]$20                        //32
  movaps xmm3, [A]$30                        //48
  movaps xmm4, [B]$00   // Load 2 rows of B
  movaps xmm5, [B]$10
  addps  xmm0, xmm4     // Add rows
  addps  xmm1, xmm5
  movaps xmm4, [B]$20   // Load 2 rows of B
  movaps xmm5, [B]$30
  addps  xmm2, xmm4     // Add rows
  addps  xmm3, xmm5
  movaps [Result]$00, xmm0
  movaps [Result]$10, xmm1
  movaps [Result]$20, xmm2
  movaps [Result]$30, xmm3
end;

class operator TBZMatrix4f.-(constref A, B: TBZMatrix4f): TBZMatrix4f; assembler; 
asm
  movaps xmm0, [A]$00
  movaps xmm1, [A]$10
  movaps xmm2, [A]$20
  movaps xmm3, [A]$30
  movaps xmm4, [B]$00
  movaps xmm5, [B]$10
  subps  xmm0, xmm4
  subps  xmm1, xmm5
  movaps xmm4, [B]$20
  movaps xmm5, [B]$30
  subps  xmm2, xmm4
  subps  xmm3, xmm5
  movaps [Result]$00, xmm0
  movaps [Result]$10, xmm1
  movaps [Result]$20, xmm2
  movaps [Result]$30, xmm3
end;

class operator TBZMatrix4f.+(constref A: TBZMatrix4f; constref B: Single): TBZMatrix4f; assembler;
asm
  movaps xmm0, [A]$00     // Load 4 rows
  movaps xmm1, [A]$10
  movaps xmm2, [A]$20
  movaps xmm3, [A]$30
  movss xmm4, [B]
  shufps xmm4, xmm4, 0    // Replicate B

  addps  xmm0, xmm4       // Add B to each row
  addps  xmm1, xmm4
  addps  xmm2, xmm4
  addps  xmm3, xmm4

  movaps [Result]$00, xmm0
  movaps [Result]$10, xmm1
  movaps [Result]$20, xmm2
  movaps [Result]$30, xmm3
end;

class operator TBZMatrix4f.-(constref A: TBZMatrix4f; constref B: Single): TBZMatrix4f; assembler; 
asm
  movaps xmm0, [A]$00
  movaps xmm1, [A]$10
  movaps xmm2, [A]$20
  movaps xmm3, [A]$30
  movss xmm4, [B]
  shufps xmm4, xmm4, 0

  subps  xmm0, xmm4
  subps  xmm1, xmm4
  subps  xmm2, xmm4
  subps  xmm3, xmm4

  movaps [Result]$00, xmm0
  movaps [Result]$10, xmm1
  movaps [Result]$20, xmm2
  movaps [Result]$30, xmm3
end;

class operator TBZMatrix4f.*(constref A: TBZMatrix4f; constref B: Single): TBZMatrix4f; assembler;
asm
  movaps xmm0, [A]$00
  movaps xmm1, [A]$10
  movaps xmm2, [A]$20
  movaps xmm3, [A]$30
  movss xmm4, [B]
  shufps xmm4, xmm4, 0

  mulps  xmm0, xmm4
  mulps  xmm1, xmm4
  mulps  xmm2, xmm4
  mulps  xmm3, xmm4

  movaps [Result]$00, xmm0
  movaps [Result]$10, xmm1
  movaps [Result]$20, xmm2
  movaps [Result]$30, xmm3
end;

class operator TBZMatrix4f.*(constref A: TBZMatrix4f; constref B: TBZVector4f): TBZVector4f; assembler; register; nostackframe;
asm
  movaps   xmm0, [B]      // Load vector
  movaps   xmm4, [A]$00   // Load 1st row
  movaps   xmm1, xmm0
  movaps   xmm2, xmm0
  movaps   xmm3, xmm0
  movaps   xmm5, [A]$10  // Load 2nd row
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
  movaps   [Result], xmm0
end;


// https://github.com/WojciechMula/toys/blob/master/sse/sse-matvecmult.c
// pd reverse this to make array ops simpler do transpose first
class operator TBZMatrix4f.*(constref A: TBZVector4f; constref B: TBZMatrix4f): TBZVector4f; assembler; register; nostackframe;
asm
  movaps   xmm0, [B]$00  // A03 A02 A01 A00
  movaps   xmm1, [B]$10  // A13 A12 A11 A10
  movaps   xmm2, [B]$20  // A23 A22 A21 A20
  movaps   xmm3, [B]$30  // A33 A32 A31 A30
  movss    xmm4, [A]         // changed to use parallel ops
  movss    xmm5, [A]4
  movss    xmm6, [A]8
  movlps   xmm7, [A]12
  shufps   xmm4, xmm4, 0
  shufps   xmm5, xmm5, 0
  shufps   xmm6, xmm6, 0
  shufps   xmm7, xmm7, 0
  mulps    xmm0, xmm4
  mulps    xmm1, xmm5
  mulps    xmm2, xmm6
  mulps    xmm3, xmm7
  addps    xmm0, xmm1    // Add rows
  addps    xmm2, xmm3
  addps    xmm0, xmm2
 // movhlps  xmm1, xmm0

  //movaps   xmm0, [B]      // Load vector
  //movaps   xmm4, [A]$00   // Load 1st row
  //movaps   xmm1, xmm0
  //movaps   xmm2, xmm0
  //movaps   xmm3, xmm0
  //movaps   xmm5, [A]$10  // Load 2nd row
  //mulps    xmm0, xmm4    // (Bx * A00), (By * A01), (Bz * A02), (Bw * A03)
  //mulps    xmm1, xmm5    // (Bx * A10), (By * A11), (Bz * A12), (Bw * A13)
  //movups   xmm4, [A]$20  // Load 3rd row
  //movups   xmm5, [A]$30  // Load 4st row
  //mulps    xmm2, xmm4    // (Bx * A20), (By * A21), (Bz * A22), (Bw * A23)
  //mulps    xmm3, xmm5    // (Bx * A30), (By * A31), (Bz * A32), (Bw * A33)
  //
  //{ Transpose }
  //movaps   xmm4, xmm2
  //unpcklps xmm2, xmm3    // A32 A22 A33 A23
  //unpckhps xmm4, xmm3    // A30 A20 A31 A21
  //
  //movaps   xmm3, xmm0
  //unpcklps xmm0, xmm1    // A12 A02 A13 A03
  //unpckhps xmm3, xmm1    // A10 A00 A11 A01
  //
  //movaps   xmm1, xmm0
  //unpcklpd xmm0, xmm2    // A33 A23 A13 A03
  //unpckhpd xmm1, xmm2    // A32 A22 A12 A02
  //
  //movaps   xmm2, xmm3
  //unpcklpd xmm2, xmm4    // A31 A21 A11 A01
  //unpckhpd xmm3, xmm4    // A30 A20 A10 A00
  //
  //addps    xmm0, xmm1    // Add rows
  //addps    xmm2, xmm3
  //addps    xmm0, xmm2
  movaps   [Result], xmm0
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
  movaps xmm0, [A]$00
  movaps xmm4, [B]$00
  movaps xmm1, xmm0
  movaps xmm2, xmm0
  movaps xmm3, xmm0
  shufps xmm0, xmm0, $00
  shufps xmm1, xmm1, $55
  shufps xmm2, xmm2, $AA
  shufps xmm3, xmm3, $FF
  movaps xmm5, [B]$10
  movaps xmm6, [B]$20
  movaps xmm7, [B]$30
  mulps  xmm0, xmm4
  mulps  xmm1, xmm5
  mulps  xmm2, xmm6
  mulps  xmm3, xmm7
  addps  xmm0, xmm1
  addps  xmm2, xmm3
  addps  xmm0, xmm2
  movaps [Result]$00, xmm0

  { A.V[1] * B }
  movaps xmm0, [A]$10
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
  movaps [Result]$10, xmm0

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
  movaps xmm0, [A]$30
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
  movaps [Result]$30, xmm0
end;

class operator TBZMatrix4f./(constref A: TBZMatrix4f; constref B: Single): TBZMatrix4f; assembler;
asm
  movaps xmm0, [A]$00
  movaps xmm1, [A]$10
  movaps xmm2, [A]$20
  movaps xmm3, [A]$30
  movss xmm4, [B]
  shufps xmm4, xmm4, 0

  divps  xmm0, xmm4
  divps  xmm1, xmm4
  divps  xmm2, xmm4
  divps  xmm3, xmm4

  movaps [Result]$00, xmm0
  movaps [Result]$10, xmm1
  movaps [Result]$20, xmm2
  movaps [Result]$30, xmm3
end;

class operator TBZMatrix4f.-(constref A: TBZMatrix4f): TBZMatrix4f; assembler; 
asm
  movaps xmm0, [RIP+cSSE_MASK_NEGATE]  // Load mask with 4 sign (upper) bits
  movaps xmm1, [A]$00 // Load 4 rows
  movaps xmm2, [A]$10
  movaps xmm3, [A]$20
  movaps xmm4, [A]$30
  xorps  xmm1, xmm0             // Flip sign bits of each element in each row
  xorps  xmm2, xmm0
  xorps  xmm3, xmm0
  xorps  xmm4, xmm0
  movaps [Result]$00, xmm1
  movaps [Result]$10, xmm2
  movaps [Result]$20, xmm3
  movaps [Result]$30, xmm4
end;

{%endregion%}

{%region%----[ Functions ]--------------------------------------------------------}

function TBZMatrix4f.Multiply(constref M2 : TBZMatrix4f) : TBZMatrix4f; assembler;
asm
   movaps xmm0, [RCX]$00   // Self[0]
   movaps xmm1, [RCX]$10   // Self[1]
   movaps xmm2, [RCX]$20   // Self[2]
   movaps xmm3, [RCX]$30   // Self[3]

   movaps xmm4, [M2]$00
   movaps xmm5, [M2]$10
   movaps xmm6, [M2]$20
   movaps xmm7, [M2]$30
   // Component-wise multiplication
   mulps  xmm0, xmm4
   mulps  xmm1, xmm5
   mulps  xmm2, xmm6
   mulps  xmm3, xmm7

   movaps [Result]$00, xmm0
   movaps [Result]$10, xmm1
   movaps [Result]$20, xmm2
   movaps [Result]$30, xmm3
end;

// https://www.gamedev.net/forums/topic/621951-sse-4x4-matrix-transpose-and-invert/
// http://www.mersenneforum.org/showthread.php?t=17555
// https://github.com/floodyberry/supercop/blob/master/crypto_hash/groestl512/avx/groestl-asm-avx.h
function TBZMatrix4f.Transpose : TBZMatrix4f; assembler;
asm
  {$ifdef TEST}
    movlps	xmm1, [RCX]
    movlps	xmm3, [RCX]8
    movhps	xmm1, [RCX]16
    movhps	xmm3, [RCX]24
    movlps	xmm5, [RCX]32
    movlps	xmm4, [RCX]40
    movhps	xmm5, [RCX]48
    movhps	xmm4, [RCX]56
    movaps	xmm0, xmm1
    movaps	xmm2, xmm3
    shufps	xmm1, xmm5, $DD
    shufps	xmm3, xmm4, $DD
    shufps	xmm0, xmm5, $88
    shufps	xmm2, xmm4, $88
    movaps [RDX]$00, xmm0
    movaps [RDX]$10,xmm1 //16
    movaps [RDX]$20,xmm2 //32
    movaps [RDX]$30,xmm3 //48
  {$else}
    movaps   xmm0, [RCX]$00  // A03 A02 A01 A00
    movaps   xmm1, [RCX]$10 // A13 A12 A11 A10
    movaps   xmm2, [RCX]$20 // A23 A22 A21 A20
    movaps   xmm3, [RCX]$30 // A33 A32 A31 A30

    movaps   xmm4, xmm2
    unpcklps xmm2, xmm3               // A31 A21 A30 A20
    unpckhps xmm4, xmm3               // A33 A23 A32 A22

    movaps   xmm3, xmm0
    unpcklps xmm0, xmm1               // A11 A01 A10 A00
    unpckhps xmm3, xmm1               // A13 A03 A12 A02

    movaps   xmm1, xmm0
    unpcklpd xmm0, xmm2               // A30 A20 A10 A00
    unpckhpd xmm1, xmm2               // A31 A21 A11 A01

    movaps   xmm2, xmm3
    unpcklpd xmm2, xmm4               // A32 A22 A12 A02
    unpckhpd xmm3, xmm4               // A33 A23 A13 A03

    movaps  [RDX]$00, xmm0
    movaps  [RDX]$10, xmm1
    movaps  [RDX]$20, xmm2
    movaps  [RDX]$30, xmm3
  {$endif}
end;


// Translated from DirectXMathMatrix.nl
function TBZMatrix4f.GetDeterminant : Single; assembler; register; nostackframe;
asm
  movaps xmm0, [RCX]$00
  movaps xmm1, [RCX]$10
  movaps xmm2, [RCX]$20
  movaps xmm3, [RCX]$30


  //XMVECTOR V0 = XMVectorSwizzle<XM_SWIZZLE_Y, XM_SWIZZLE_X, XM_SWIZZLE_X, XM_SWIZZLE_X>(M.r[2]);
  //XMVECTOR V2 = XMVectorSwizzle<XM_SWIZZLE_Y, XM_SWIZZLE_X, XM_SWIZZLE_X, XM_SWIZZLE_X>(M.r[2]);
    movaps xmm4,xmm2             // A2
    shufps xmm4,xmm4,00000001b   // V0 YXXX  A2
    movaps xmm5, xmm4            // V2 YXXX  A2

  //XMVECTOR V4 = XMVectorSwizzle<XM_SWIZZLE_Z, XM_SWIZZLE_Z, XM_SWIZZLE_Y, XM_SWIZZLE_Y>(M.r[2]);
    movaps xmm6, xmm2            // A2
    shufps xmm6, xmm6, 01011010b //    ZZYY

  //XMVECTOR V1 = XMVectorSwizzle<XM_SWIZZLE_Z, XM_SWIZZLE_Z, XM_SWIZZLE_Y, XM_SWIZZLE_Y>(M.r[3]);
    movaps xmm7, xmm3            // A3
    shufps xmm7, xmm7, 01011010b //   ZZYY

  //XMVECTOR V3 = XMVectorSwizzle<XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_Z>(M.r[3]);
  //XMVECTOR V5 = XMVectorSwizzle<XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_Z>(M.r[3]);
    movaps xmm8, xmm3            // A3
    shufps xmm8, xmm8, 10111111b //  WWWZ  // BOTH v£ v%

    mulps xmm4, xmm7      // V0 * V1   P0
    mulps xmm5, xmm8      // V2 * V3   P1
    mulps xmm6, xmm8      // V4 * V5   P2


  //V0 = XMVectorSwizzle<XM_SWIZZLE_Z, XM_SWIZZLE_Z, XM_SWIZZLE_Y, XM_SWIZZLE_Y>(M.r[2]);
    movaps xmm7, xmm2
    shufps xmm7, xmm7, 01011010b

    //V3 = XMVectorSwizzle<XM_SWIZZLE_Y, XM_SWIZZLE_X, XM_SWIZZLE_X, XM_SWIZZLE_X>(M.r[3]);
    //V1 = XMVectorSwizzle<XM_SWIZZLE_Y, XM_SWIZZLE_X, XM_SWIZZLE_X, XM_SWIZZLE_X>(M.r[3]);
    movaps xmm8, xmm3
    shufps xmm8, xmm8, 00000001b

    //V2 = XMVectorSwizzle<XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_Z>(M.r[2]);
    //V4 = XMVectorSwizzle<XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_Z>(M.r[2]);
    movaps xmm10, xmm2
    shufps xmm10, xmm10, 10111111b
    movaps xmm11, xmm10

    //V5 = XMVectorSwizzle<XM_SWIZZLE_Z, XM_SWIZZLE_Z, XM_SWIZZLE_Y, XM_SWIZZLE_Y>(M.r[3]);
    movaps xmm12, xmm3
    shufps xmm12, xmm12, 01011010b

    mulps xmm7, xmm8     //V0 * V1
    subps xmm4, xmm7     // P0 - V0 * V1  IN XMM4 = P0
    mulps xmm10, xmm8    // V2 * V3
    subps xmm5, xmm10    // P1 - V2 * V3  IN XMM5 = P1
    mulps xmm11, xmm12   // V4 * V5
    subps xmm6, xmm11    // P2 - V4 * V5  IN XMM6 = P2

    movaps xmm7, xmm1
    movaps xmm8, xmm1
    movaps xmm9, xmm1

    //V0 = XMVectorSwizzle<XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_W, XM_SWIZZLE_Z>(M.r[1]);
    shufps xmm7, xmm7, 10111111b
    //V1 = XMVectorSwizzle<XM_SWIZZLE_Z, XM_SWIZZLE_Z, XM_SWIZZLE_Y, XM_SWIZZLE_Y>(M.r[1]);
    shufps xmm8, xmm8, 01011010b
    //V2 = XMVectorSwizzle<XM_SWIZZLE_Y, XM_SWIZZLE_X, XM_SWIZZLE_X, XM_SWIZZLE_X>(M.r[1]);
    shufps xmm9, xmm9, 00000001b

    //XMVECTOR S = XMVectorMultiply(M.r[0], Sign.v);
    mulps xmm0, [RIP+cNegateVector4f_PNPN]    // S

    //XMVECTOR R = XMVectorMultiply(V0, P0);
    mulps xmm4, xmm7    // XMM4 = R

    //R = XMVectorNegativeMultiplySubtract(V1, P1, R);
    mulps xmm5, xmm8    // P1 * V1
    subps xmm4, xmm5    // R = R - P1 * V1

    //R = XMVectorMultiplyAdd(V2, P2, R);
    mulps xmm6, xmm9   // P2 * V2
    addps xmm4, xmm6   // R = R - P2 * V2

    // Dot
    {$IFDEF USE_ASM_SSE_4}
       dpps xmm0, xmm4, 11110001b //or $F1
    {$ELSE}
      {$IFDEF USE_ASM_SSE_3}
         mulps xmm0, xmm4
         //haddps xmm0, xmm0
         //haddps xmm0, xmm0
         movshdup xmm1,xmm0
         addps  xmm0,xmm1
         movhlps xmm1,xmm0
         addss  xmm0,xmm1
      {$ELSE}
        mulps   xmm0, xmm4
        movaps xmm1,xmm0
        movhlps xmm1, xmm1
	      addps xmm1, xmm0
	      shufps xmm0, xmm0, 1
	      addss xmm0, xmm1
        //movhlps xmm4, xmm0
        //addps   xmm4, xmm0     //|W*Y|Z*X|
        //movaps  xmm0, xmm4
        //shufps xmm0, xmm0, 00000001b
        //addss  xmm0, xmm4
      {$ENDIF}
    {$ENDIF}
end;

function TBZMatrix4f.Translate(constref v : TBZVector) : TBZMatrix4f;// assembler;
begin
  Result := Self;
  Result.X.W := Result.X.W+v.X;
  Result.Y.W := Result.Y.W+v.Y;
  Result.Z.W := Result.Z.W+v.Z;
end;

//asm
//  movaps xmm2, [RCX]$00
//  movaps xmm3, [RCX]$10
//  movaps xmm4, [RCX]$20
//  movaps xmm0, [RCX]$30
//  movaps xmm1, [V]
//  andps xmm1, [RIP+cSSE_MASK_NO_W]
//  addps xmm0, xmm1
//  movaps [RESULT]$00, xmm2
//  movaps [RESULT]$10, xmm3
//  movaps [RESULT]$20, xmm4
//  movaps [RESULT]$30, xmm0
//end;

// https://software.intel.com/en-us/articles/optimized-matrix-library-for-use-with-the-intel-pentiumr-4-processors-sse2-instructions/
// https://lxjk.github.io/2017/09/03/Fast-4x4-Matrix-Inverse-with-SSE-SIMD-Explained.html


// https://software.intel.com/en-us/articles/optimized-matrix-library-for-use-with-the-intel-pentiumr-4-processors-sse2-instructions/
// https://lxjk.github.io/2017/09/03/Fast-4x4-Matrix-Inverse-with-SSE-SIMD-Explained.html
function TBZMatrix4f.Invert : TBZMatrix4f; assembler; register; nostackframe;
const
  cAdjugateSignMult : TBZVector4f =(x:1.0; y:-1.0; z:-1.0; w:1.0);
  cSSE_Epsilon: Single = 1e-10;
asm
// Convert from Intel Math Lib
// https://software.intel.com/en-us/articles/optimized-matrix-library-for-use-with-the-intel-pentiumr-4-processors-sse2-instructions/
// The inverse is calculated using "Divide and Conquer" technique. The
// original matrix is divide into four 2x2 sub-matrices. Since each
// register holds four matrix element, the smaller matrices are
// represented as a registers. Hence we get a better locality of the calculations.

 // Testing array  has Det 81
 //{ 1,  2,  2,  4},    A { 1,  2,    B { 2,  4,
 //{ 2,  1,  4,  2},       2,  1 }        4,  2 }
 //{ 2,  4,  1,  2},    C { 2,  4     D { 1,  2,
 //{ 4,  2,  2,  1}}        4,  2}        2,  1}
 // Inverse of above i.e. the result we want.
 // 0.111111111111111 -0.222222222222222 -0.222222222222222  0.444444444444444
 //-0.222222222222222  0.111111111111111  0.444444444444444 -0.222222222222222
 //-0.222222222222222  0.444444444444444  0.111111111111111 -0.222222222222222
 // 0.444444444444444 -0.222222222222222 -0.222222222222222  0.111111111111111

  movaps  xmm0, [RCX]$00
  movaps  xmm1, [RCX]$10
  movaps  xmm2, [RCX]$20
  movaps  xmm3, [RCX]$30
  movaps  xmm4, xmm0      //  needed temp storage for next hlps
  movaps  xmm5, xmm2
  movlhps xmm0, xmm1  //A        // 1 2 2 1
  movhlps xmm1, xmm4  //B        // 2 4 4 2
  movlhps xmm2, xmm3  //C        // 2 4 4 2
  movhlps xmm3, xmm5  //D        // 1 2 2 1

  //  AB = A# * B
  //         T1        T2    T1-T2Sshuf  |Z|w|X|Y| -> |Y|X|W|Z|  01001110b = $4E
  // A#B ( A3 * B0 − A1 * B2      1 * 2 - 2 * 4 = -6
  //       A3 * B1 − A1 * B3      1 * 4 - 2 * 2 =  0
  //       A0 * B2 − A2 * B0      1 * 4 - 2 * 2 =  0
  //       A0 * B3 − A2 * B1      1 * 2 - 2 * 4 = -6
  //)
  //T1 = B * A3,A3,A0,A0   T1shuf      |W|W|X|X| -> |X|X|W|W|  00001111b = $0F
  //T2 = B * A2,A2,A1,A1   T2shuf      |Z|Z|Y|Y| -> |Y|Y|Z|Z|  10100101b = $A5
  movaps xmm4, xmm0        // A
  shufps xmm4, xmm4, $0F   // A T1 SHUF
  mulps  xmm4, xmm1        // T1
  movaps xmm8, xmm0        // A
  shufps xmm8, xmm8, $A5   // A T2 SHUF
  movaps xmm5,xmm1         // T2
  shufps xmm5, xmm5, $4E
  mulps  xmm5, xmm8
  subps  xmm4, xmm5         // XMM4 = A#B  = -6,0,0,-6

  // D#C ( D3 * C0 − D1 * C2       1 * 2 - 2 * 4 = -6
  //       D3 * C1 − D1 * C3       1 * 4 - 2 * 2 =  0
  //       D0 * C2 − D2 * C0       1 * 4 - 2 * 2 =  0
  //       D0 * C3 − D2 * C1       1 * 2 - 2 * 4 = -6

  //  DC = D# * C
  movaps xmm5, xmm3  //xmm5 = DC
  shufps xmm5, xmm5, $0F
  mulps  xmm5, xmm2
  movaps xmm8, xmm3
  shufps xmm8, xmm8, $A5
  movaps xmm6,xmm2
  shufps xmm6, xmm6, $4E
  mulps  xmm6, xmm8
  subps  xmm5, xmm6       // XXM5 = D#C =  -6,0,0,-6
  // for a start lets do this the long way
  // I am sure there is a shortcut for doing just det
  // but get it working first as we need these terms later.
  // A = { 1 2
  //       2 1 } det -3
  //  dA = |A|                    // det of 2x2 = x*w - y*z
  movaps xmm6, xmm0
  shufps xmm6, xmm6, 01011111b    // $5F |Y|Y|W|W|
  mulps  xmm6, xmm0               // |X*W|Y*W|Z*Y|Z*W|
  movhlps xmm7,xmm6
  subss  xmm6, xmm7               //xmm6 = detA   = -3
  //  dB = |B|
  movaps xmm7, xmm1
  shufps xmm7, xmm7, $5F
  mulps  xmm7, xmm1
  movhlps xmm8,xmm7
  subss  xmm7, xmm8              //xmm7 = detB   = -12
  //  dC = |C|
  movaps xmm8, xmm2
  shufps xmm8, xmm8, $5F
  mulps  xmm8, xmm2
  movhlps xmm9,xmm8
  subss  xmm8, xmm9             //xmm8 = detC   = -12
  //  dD = |D|
  movaps xmm9, xmm3
  shufps xmm9, xmm9, $5F
  mulps  xmm9, xmm3
  movhlps xmm10,xmm9
  subss  xmm9, xmm10            //xmm9 = detD   = -3
  // The above all need to fill the regs later on, but do not do now
  // waste of time if det is 0

  //  d = trace((A#B) * (D#C))
  // tr(AB)= A0*B0 + A1*B2 + A2*B1 + A3 * B3
  //         -6 * -6 + 0 + 0 + -6 *-6   = 72
  movaps xmm10,xmm5               // tr(xmm4,xmm5)
  shufps xmm10, xmm10, 11011000b  // B8
  mulps xmm10,xmm4
  {$ifdef USE_ASM_SSE_3}
    movshdup xmm11, xmm10
    addps    xmm10, xmm11
    movhlps  xmm11, xmm10
    addss    xmm10, xmm11
  {$else}
    movaps   xmm11, xmm10
    shufps   xmm11, xmm10, 10110001b //177
    addps    xmm10, xmm11
    movhlps  xmm11, xmm10
    addss    xmm10, xmm11
  {$endif}
  // final bit for det do it here as we may have to get out asap if it is 0
  // |A||D|+|B||C|−tr((A#B)(D#C))  xmm6 * xmm9 + xcmm7 + xmm8
  movss xmm11, xmm6     // only want singles
  movss xmm12, xmm7
  mulss xmm11, xmm9
  mulss xmm12, xmm8
  addss xmm11, xmm12     // xmm11 mow hold det
  subss xmm11, xmm10     //

  // now need to test xmm11 for 0 xmm10 done with for the moment

  movlps  xmm10, [RIP+cSSE_Epsilon]
  movss   xmm12, xmm11
  movss   xmm13, [RIP+cSSE_MASK_ABS]
  andps   xmm11, xmm13
  comiss  xmm11, xmm10             // non destructive test
  jb @ZeroDet

  movaps xmm10, [RIP+cOneVector4f]
  divps  xmm10, xmm12     // xmm10 now hold 1/det
  shufps  xmm6, xmm6,  $00  // fill 2x2 dets
  shufps  xmm7, xmm7,  $00
  shufps  xmm8, xmm8,  $00
  shufps  xmm9, xmm9,  $00
  shufps xmm10, xmm10, $00    // and matrix 1/det

  // We now have enough to calc 2x2 one at a time
  // only have 5 regs left for intermediates
  //          xmm5-9   xmm0-3
  // invA  =  (|D|A − B * (D#C)) #      * 1/det
  // invB  =  (|B|C − D * ((A#B)#) )#   * 1/det
  // invC  =  (|C|B − A * ((D#C)#) )#   * 1/det
  // invD  =  (|A|D − C * (A#B)) #      * 1/det
  // this is the only calc that uses |A| to |D| so these will be our working reg.

  // Scalar mults
  mulps  xmm9, xmm0   // |D| A   1 2 2 1 * -3 = -3 -6 -6 -3
  mulps  xmm7, xmm2   // |B| C   2 4 4 2 * -12 = -24 -48 -48 -24
  mulps  xmm8, xmm1   // |C| B   2 4 4 2 * -12 = -24 -48 -48 -24
  mulps  xmm6, xmm3   // |A| D   1 2 2 1 * -3 = -3 -6 -6 -3

 // Mat Mul expanded to something useful
 // __m128 Mat2Mul(__m128 vec1, __m128 vec2)
 //{
 //        __m128 t1 = VecSwizzle(vec2, 0,3,0,3);
 //        __m128 t2 = VecSwizzle(vec1, 1,0,3,2);
 //        __m128 t3 = VecSwizzle(vec2, 2,1,2,1);
 //        __m128 t4 = _mm_mul_ps(vec1,t1 );
 //        __m128 t5 = _mm_mul_ps( t2, t3 );
 //      return _mm_add_ps(t4,t5  );
 //}
  // all items need a final adjugate mult
  // save three mults by multiplying invDet with sign mask
  movaps xmm15, [RIP+cAdjugateSignMult]
  mulps  xmm10, xmm15
  // invA
  //  B * (D#C)   mat mul order is important. =  -12 -24 -24 -12
  // B in xmm1, D#C in xmm5 need two copies of D#C one copy B
  movaps xmm11, xmm5
  movaps xmm12, xmm5
  movaps xmm13, xmm1
  shufps  xmm11, xmm11, 11001100b  // t1
  shufps  xmm12, xmm12, 01100110b  // t3
  shufps  xmm13, xmm13, 10110001b  // t2
  mulps   xmm11, xmm1              // t1 * vec1
  mulps   xmm12, xmm13
  addps   xmm11, xmm12             //  B * (D#C)

  subps   xmm9, xmm11            // |D|A − B * (D#C) = 9 18 18 9 in xmm9
  shufps  xmm9, xmm9, 00100111b  // adjunct shuffle     4 3 2 1 -> 1 3 2 4 = 00100111
  mulps   xmm9, xmm10            // adjunct negate * det    .111 .222 .222 .111 in xmm9

  // invD  do this first so we can adjugate A#B and D#C in place without a copy
  //  C * (A#B))
  // C in xmm2  A#B in xmm4
  movaps xmm11, xmm4
  movaps xmm12, xmm4
  movaps xmm13, xmm2
  shufps  xmm11, xmm11, 11001100b  // t1
  shufps  xmm12, xmm12, 01100110b  // t3
  shufps  xmm13, xmm13, 10110001b  // t2
  mulps   xmm11, xmm2              // t1 * vec1
  mulps   xmm12, xmm13
  addps   xmm11, xmm12             //  C * (A#B)

  subps   xmm6, xmm11            // |A|D − C * (A#B) = 9 18 18 9 in xmm6
  shufps  xmm6, xmm6, 00100111b  // adjunct shuffle     4 3 2 1 -> 1 3 2 4 = 00100111
  mulps   xmm6, xmm10            // adjunct negate * det    .111 .222 .222 .111 in xmm6

  // adjugate A#B and D#C in place
  shufps  xmm4, xmm4, 00100111b
  shufps  xmm5, xmm5, 00100111b
  mulps   xmm4, xmm15
  mulps   xmm5, xmm15

  // invB
  // D * ((A#B)#)
  // D in xmm3  (A#B)# in xmm4  only need two copies this time as we are done with xmm4 after this
  movaps xmm12, xmm4
  movaps xmm13, xmm3
  shufps  xmm4, xmm4, 11001100b  // t1
  shufps  xmm12, xmm12, 01100110b  // t3
  shufps  xmm13, xmm13, 10110001b  // t2
  mulps   xmm4, xmm3              // t1 * vec1
  mulps   xmm12, xmm13
  addps   xmm4, xmm12             //  D * ((A#B)#)  |B| C in xmm7

  subps   xmm7, xmm4            // |B| C − C * (A#B) = -18 -36 -36 -18 in xmm7
  shufps  xmm7, xmm7, 00100111b  // adjunct shuffle     4 3 2 1 -> 1 3 2 4 = 00100111
  mulps   xmm7, xmm10            // adjunct negate * det    .222 .444 .444 .222 in xmm6

  // invC
  // A * ((D#C)#)
  // A in xmm0  (D#C)# in xmm5
  movaps  xmm12, xmm5
  movaps  xmm13, xmm0
  shufps   xmm5, xmm5, 11001100b  // t1
  shufps  xmm12, xmm12, 01100110b  // t3
  shufps  xmm13, xmm13, 10110001b  // t2
  mulps   xmm5, xmm0              // t1 * vec1
  mulps   xmm12, xmm13
  addps   xmm5, xmm12             //  A * ((D#C)#)  |C|B in xmm8

  subps   xmm8, xmm5            // |C|B − A * ((D#C)#) = -18 -36 -36 -18 in xmm8
  shufps  xmm8, xmm8, 00100111b  // adjunct shuffle     4 3 2 1 -> 1 3 2 4 = 00100111
  mulps   xmm8, xmm10            // adjunct negate * det    .222 .444 .444 .222 in xmm6

  // now have result but in wrong order
  // reverse the start shuffling
  // invA in xmm9 invB in xmm7 invC in xmm8 invD in xmm6
  movaps xmm10, xmm9  // copy inv A
  movaps xmm11, xmm8  // copy inv C
  movlhps xmm9, xmm7
  movhlps xmm7, xmm10
  movlhps xmm8, xmm6
  movhlps xmm6, xmm11

  movaps [RESULT]$00, xmm9
  movaps [RESULT]$10, xmm7
  movaps [RESULT]$20, xmm8
  movaps [RESULT]$30, xmm6
  jmp @Finish
@ZeroDet:
  // load and store IdentityMatrix
  movaps xmm0, [RIP+cWOneSSEVector4f]
  movaps xmm1, xmm0
  movaps xmm2, xmm0
  movaps xmm3, xmm0
  shufps xmm1, xmm1, 00110000b
  shufps xmm2, xmm2, 00001100b
  shufps xmm3, xmm3, 00000011b
  movaps [RESULT]$00, xmm3
  movaps [RESULT]$10, xmm2
  movaps [RESULT]$20, xmm1
  movaps [RESULT]$30, xmm0

@Finish:
end;


(*function TBZMatrix4f.Normalize:TBZMatrix4; assembler;
asm


 movaps  [RCX]$30, [RIP+WHmgVector]
end; *)

function TBZMatrix4f.Normalize:TBZMatrix4f;
begin
   result :=self;
   Result.X.W:=0;
   Result.X.Normalize;
   Result.Y.W:=0;
   Result.Y.Normalize;
   Result.Z:=Result.X.CrossProduct(Result.Y);
   Result.X:=Result.Y.CrossProduct(Result.Z);
   Result.W:=WHmgVector;
end;

{%endregion%}


