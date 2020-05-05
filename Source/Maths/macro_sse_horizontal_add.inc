// Horizontal add: Calculates the sum of all vector elements.
// Vector4f
{$IFDEF USE_ASM_AVX}
  vmovaps xmm1,xmm0
  vextractf128 xmm0,ymm0,0x1
  vaddps xmm0,xmm1,xmm0
  vmovshdup xmm1,xmm0
  vaddps xmm0,xmm1,xmm0
  vmovhlps xmm1,xmm1,xmm0
  vaddss xmm0,xmm0,xmm1
  vzeroupper
  {$ELSE IFDEF USE_ASM_SSE_3}
    haddps xmm0, xmm0  //Slow
	  haddps xmm0, xmm0

    { movshdup xmm1,xmm0
    addps  xmm0,xmm1
    movhlps xmm1,xmm0
    addss  xmm0,xmm1 }

    {AVX
     vmovshdup xmm1,xmm0
     vaddps xmm0,xmm0,xmm1
     vmovhlps xmm1,xmm1,xmm0
     vaddss xmm0,xmm0,xmm1
    }
    {$else}

    { movaps xmm1,xmm0
     shufps xmm1,xmm0,1
     addps  xmm0,xmm1
     movhlps xmm1,xmm0
     addss  xmm0,xmm1 }

      movaps xmm1,xmm0
      movhlps xmm1, xmm1
	    addps xmm1, xmm0
	    shufps xmm0, xmm0, 1
	    addss xmm0, xmm1

    (* movaps xmm1,xmm0
    movhlps xmm1,xmm0
    addps  xmm0,xmm1
    movaps xmm1,xmm0
    shufps xmm1,xmm0,1
    addss  xmm0,xmm1 *)

  {$endif}
{$endif}

// Vector4i
{$IFDEF USE_ASM_AVX}
  vpunpckhqdq xmm1,xmm0,xmm0
  vpaddd xmm0,xmm1,xmm0
  vpshuflw xmm1,xmm0,0x4e
  vpaddd xmm0,xmm0,xmm1
  vmovd  eax,xmm0
  {$ELSE IFDEF USE_ASM_SSE_3}
   //SSSE3 (slow)
   phaddd xmm0,xmm0
   phaddd xmm0,xmm0
  {$else}
   pshufd xmm1,xmm0,0x4e
   paddd  xmm0,xmm1
   pshuflw xmm1,xmm0,0x4e
   paddd  xmm0,xmm1
   //movd   eax,xmm0
  {$endif}
{$endif}

