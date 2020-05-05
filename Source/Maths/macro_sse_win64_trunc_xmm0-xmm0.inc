{ Macro SSE for TBZVector2f and TBZVector4f : "Trunc"
  Input  : xmm0
  Output : xmm0
}
{$ifdef cpu64}
  push     rax                               // create a mem area for mxcsr
  stmxcsr  DWORD PTR [rsp]                   //  store mxcsr
  mov      eax, DWORD PTR [rsp]              // get it
  mov      ecx, eax                          // make local copy   self is done with at this point
  //{$ifdef TEST}
  //and      eax, [RIP+cSSE_ROUND_MASK]
  //or       eax, [RIP+cSSE_ROUND_MASK_TRUNC]  // Set bits
  //{$else}
                    // smaller opcode no mem access required.
  and      eax, $00009FFF
  or       eax, $00006000                    // Set bits
  //{$endif}
  mov      DWORD PTR [rsp], eax              // mov bits to mem
  ldmxcsr  DWORD PTR [rsp]                   // set new bits
  cvtps2dq xmm0, xmm0
  mov      DWORD PTR [rsp], ecx              // put old bits in mem
  ldmxcsr  DWORD PTR [rsp]                   // Pop rounding mode
  pop      rax
{$else}
  push     eax                               // create a mem area for mxcsr
  push     ecx
  stmxcsr  DWORD PTR [esp]                   //  store mxcsr
  mov      eax, DWORD PTR [esp]              // get it
  mov      ecx, eax                          // make local copy   self is done with at this point
  and      eax, $00009FFF
  or       eax, $00006000                    // Set bits
  mov      DWORD PTR [esp], eax              // mov bits to mem
  ldmxcsr  DWORD PTR [esp]                   // set new bits
  cvtps2dq xmm0, xmm0
  mov      DWORD PTR [esp], ecx              // put old bits in mem
  ldmxcsr  DWORD PTR [esp]                   // Pop rounding mode
  pop      ecx
  pop      eax
{$endif}
