{%region%----[ Operators ]--------------------------------------------------------}

class operator TBZVector4f.+(constref A, B: TBZVector4f): TBZVector4f; assembler; nostackframe; register;
asm
  vmovups xmm0, [A]
  vmovups xmm1, [B]
  vaddps  xmm0, xmm0, xmm1
  vmovups [RESULT], xmm0
end;

class operator TBZVector4f.-(constref A, B: TBZVector4f): TBZVector4f; assembler; nostackframe; register;
asm
  vmovups xmm0, [A]
  vmovups xmm1, [B]
  vsubps  xmm0, xmm0, xmm1
  vmovups [RESULT], xmm0
end;

class operator TBZVector4f.*(constref A, B: TBZVector4f): TBZVector4f; assembler; nostackframe; register;
asm
  vmovups xmm0, [A]
  vmovups xmm1, [B]
  vmulps  xmm0, xmm0, xmm1
  vmovups [RESULT], xmm0
end;

class operator TBZVector4f./(constref A, B: TBZVector4f): TBZVector4f; assembler; nostackframe; register;
asm
  vmovups xmm0, [A]
  vmovups xmm1, [B]
  vdivps  xmm0, xmm0, xmm1
  vmovups [RESULT], xmm0
end;

class operator TBZVector4f.+(constref A: TBZVector4f; constref B:Single): TBZVector4f; assembler; nostackframe; register;
asm
  vmovups xmm0, [A]
  vmovss  xmm1, [B]
  vshufps xmm1, xmm1, xmm1, 0
  vaddps  xmm0, xmm1, xmm0
  vmovups [RESULT], xmm0
end;

class operator TBZVector4f.-(constref A: TBZVector4f; constref B:Single): TBZVector4f; assembler; nostackframe; register;
asm
  vmovups xmm0, [A]
  vmovss  xmm1, [B]
  vshufps xmm1, xmm1, xmm1, 0
  vsubps  xmm0, xmm0, xmm1
  vmovups [RESULT], xmm0
end;

class operator TBZVector4f.*(constref A: TBZVector4f; constref B:Single): TBZVector4f; assembler; nostackframe; register;
asm
  vmovups xmm0, [A]
  vmovss  xmm1, [B]
  vshufps xmm1, xmm1, xmm1, 0
  vmulps  xmm0, xmm1, xmm0
  vmovups [RESULT], xmm0
end;

class operator TBZVector4f./(constref A: TBZVector4f; constref B:Single): TBZVector4f; assembler; nostackframe; register;
asm
  vmovups xmm0, [A]
  vmovss  xmm1, [B]
  vshufps xmm1, xmm1, xmm1,0
  vdivps  xmm0, xmm0, xmm1
  vmovups [RESULT], xmm0
end;

class operator TBZVector4f.-(constref A: TBZVector4f): TBZVector4f; assembler; nostackframe; register;
asm
  vmovaps xmm1, [A]
  vmovaps xmm0, [cNullVector4f]
  vsubps  xmm0, xmm0, xmm1
  vmovups [RESULT],xmm0 //RDX = RESULT
End;

class operator TBZVector4f.=(constref A, B: TBZVector4f): Boolean; assembler; nostackframe; register;
asm
  vmovups xmm0, [A]
  vmovups xmm1, [B]
  vcmpps  xmm0, xmm0, xmm1, cSSE_OPERATOR_EQUAL
  movmskps eax, xmm0
  xor eax, $f
  setz al
end;

class operator TBZVector4f.<=(constref A, B: TBZVector4f): Boolean; assembler; nostackframe; register;
asm
  vmovups xmm0, [A]
  vmovups xmm1, [B]
  vcmpps  xmm0, xmm0,xmm1, cSSE_OPERATOR_LESS_OR_EQUAL
  movmskps eax, xmm0
  xor eax, $f
  setz al
end;

class operator TBZVector4f.>=(constref A, B: TBZVector4f): Boolean; assembler; nostackframe; register;
asm
  vmovups xmm0, [A]
  vmovups xmm1, [B]
  vcmpps  xmm0, xmm0,xmm1, cSSE_OPERATOR_NOT_LESS_OR_EQUAL
  movmskps eax, xmm0
  xor eax, $f
  setz al
end;

class operator TBZVector4f.>(constref A, B: TBZVector4f): Boolean; assembler; nostackframe; register;
asm
  vmovups xmm0, [A]
  vmovups xmm1, [B]
  vcmpps  xmm0, xmm0,xmm1, cSSE_OPERATOR_NOT_LESS
  movmskps eax, xmm0
  xor eax, $f
  setz al
end;

class operator TBZVector4f.<(constref A, B: TBZVector4f): Boolean; assembler; nostackframe; register;
asm
  vmovups xmm0, [A]
  vmovups xmm1, [B]
  vcmpps  xmm0, xmm0,xmm1, cSSE_OPERATOR_LESS
  movmskps eax, xmm0
  xor eax, $f
  setz al
end;

class operator TBZVector4f.<>(constref A, B: TBZVector4f): Boolean; assembler; nostackframe; register;
asm
  vmovups xmm0, [A]
  vmovups xmm1, [B]
  vcmpps  xmm0, xmm0,xmm1, cSSE_OPERATOR_NOT_EQUAL
  movmskps eax, xmm0
  xor eax, $f
  setz al
end;

{%endregion%}

{%region%----[ Functions ]--------------------------------------------------------}

function TBZVector4f.Abs: TBZVector4f; assembler; nostackframe; register;
asm
  vmovups xmm0, [EAX]
  vandps  xmm0, xmm0, [cSSE_MASK_ABS]
  vmovups [RESULT], xmm0
end;

function TBZVector4f.Negate:TBZVector4f; assembler; nostackframe; register;
asm
  vmovups xmm0, [EAX]
  vxorps  xmm0, xmm0, [cSSE_MASK_NEGATE]
  vmovups [RESULT], xmm0
End;

function TBZVector4f.DivideBy2:TBZVector4f;assembler; nostackframe; register;
asm
  vmovups xmm0, [EAX]
  vmulps  xmm0, xmm0,[cHalfOneVector4f]
  vmovups [RESULT], xmm0
end;

function TBZVector4f.Distance(constref A: TBZVector4f):Single;assembler;
//nostackframe;register; we cannot use this because the result is ST register
Asm
  vmovups xmm0, [EAX]
  vmovups xmm1, [A]
  vsubps  xmm0, xmm0, xmm1
  vandps  xmm0, xmm0, [cSSE_MASK_NO_W]
  vdpps   xmm0, xmm0, xmm0, $FF
  vsqrtss xmm0, xmm0 ,xmm0
  movss [RESULT], {%H-}xmm0
end;

function TBZVector4f.DistanceSquare(constref A: TBZVector4f):Single;assembler;
//nostackframe;register; we cannot use this because the result is ST register
Asm
  vmovups xmm0, [EAX]
  vmovups xmm1, [A]
  vsubps  xmm0, xmm0, xmm1
  vandps  xmm0, xmm0, [cSSE_MASK_NO_W]
  vdpps   xmm0, xmm0, xmm0, $FF
  movss [RESULT], {%H-}xmm0
end;

function TBZVector4f.Length:Single;assembler;
Asm
  vmovups xmm0, [EAX]
  vandps  xmm0, xmm0, [cSSE_MASK_NO_W]
  vdpps   xmm0, xmm0, xmm0, $FF
  vsqrtss xmm0, xmm0, xmm0
  movss [RESULT], {%H-}xmm0
end;

function TBZVector4f.LengthSquare:Single;assembler;
Asm
  vmovups xmm0,[EAX]
  vandps  xmm0, xmm0, [cSSE_MASK_NO_W]
  vdpps   xmm0, xmm0, xmm0, $FF
  movss [RESULT], {%H-}xmm0
end;

function TBZVector4f.Spacing(constref A : TBZVector4f) : Single; assembler;
asm
  //Result:=Abs(v2.X-Self.X)+Abs(v2.Y-Self.Y)+Abs(v2.Z-Self.Z)+Abs(v2.W-Self.W);
  vmovups xmm1, [EAX]
  vmovups xmm0, [A]
  vsubps  xmm0, xmm0, xmm1
  vandps  xmm0, xmm0, [cSSE_MASK_ABS]
  vhaddps xmm0, xmm0, xmm0
  vhaddps xmm0, xmm0, xmm0
  movss [RESULT], {%H-}xmm0
end;

function TBZVector4f.CrossProduct(constref A: TBZVector4f): TBZVector4f;assembler; nostackframe; register;
asm
  vmovups xmm0, [EAX]
  vmovups xmm1, [A]
  vmovaps xmm2, xmm0
  vmovaps xmm3, xmm1

  vshufps xmm0, xmm0, xmm0, $c9

  vshufps xmm1, xmm1, xmm1, $d2
  vshufps xmm2, xmm2, xmm2, $d2
  vshufps xmm3, xmm3, xmm3, $c9

  vmulps  xmm0, xmm0, xmm1
  vmulps  xmm2, xmm2, xmm3

  vsubps  xmm0, xmm0, xmm2
  vaddps  xmm0, xmm0, [cWOneVector4f] // it would better change by logical operator
  movups [RESULT], xmm0
end;

function TBZVector4f.DotProduct(constref A: TBZVector4f):Single;assembler; //nostackframe; register;
asm
  vmovups xmm0, [EAX]
  vmovups xmm1, [A]
  vdpps   xmm0, xmm0, xmm1, 01110001b //or $F1
  movss [RESULT], {%H-}xmm0
end;

function TBZVector4f.Norm:Single;assembler; //nostackframe; register;
asm
  vmovups xmm0, [EAX]
  vmulps  xmm0, xmm0, xmm0
  vmovaps xmm1, xmm0
  vshufps xmm0, xmm0, xmm1, $4e
  vaddps  xmm0, xmm0, xmm1
  vshufps xmm1, xmm1, xmm0, $11
  vaddps  xmm0, xmm0, xmm1
  movss [RESULT], {%H-}xmm0
end;

function TBZVector4f.Normalize: TBZVector4f;  assembler; nostackframe; register;
asm
  vmovups xmm0,[EAX]
  vmovaps xmm3,xmm0
  vandps  xmm3, xmm3, [cSSE_MASK_ONLY_W]
  vandps  xmm0, xmm0, [cSSE_MASK_NO_W]
  vmovaps xmm2, xmm0
  vmulps  xmm0, xmm0, xmm0
  vhaddps xmm0, xmm0, xmm0
  vhaddps xmm0, xmm0, xmm0
{$IFDEF USE_ASM_SIMD_HIGHPRECISION}
  // High Precision
  vsqrtps xmm0, xmm0
  vdivps  xmm0, xmm2, xmm0
{$ELSE}
  // Low precision
  rsqrtps xmm0, xmm0       //approximate reciprocal
  mulps   xmm2, xmm0
{$ENDIF}
  vaddps  xmm0, xmm0, xmm3
  vmovups [RESULT], xmm0
end;

function TBZVector4f.Min(constref B: Single): TBZVector4f; assembler; nostackframe; register;
asm
  vmovups xmm0, [EAX]
  vmovss  xmm1, [B]
  vshufps xmm1, xmm1, xmm1, $00 // Replicate B
  vminps  xmm0, xmm0, xmm1
  vmovups [RESULT], xmm0
end;

function TBZVector4f.Min(constref B: TBZVector4f): TBZVector4f; assembler; nostackframe; register;
asm
  vmovups xmm0, [EAX]
  vmovups xmm1, [B]
  vminps  xmm0, xmm0, xmm1
  vmovups [RESULT], xmm0
end;

function TBZVector4f.Max(constref B: Single): TBZVector4f; assembler; nostackframe; register;
asm
  vmovups xmm0, [EAX]
  vmovss  xmm1, [B]
  vshufps xmm1, xmm1, xmm1, $00 // Replicate B
  vmaxps  xmm0, xmm0, xmm1
  vmovups [RESULT], xmm0
end;

function TBZVector4f.Max(constref B: TBZVector4f): TBZVector4f; assembler; nostackframe; register;
asm
  vmovups xmm0, [EAX]
  vmovups xmm1, [B]
  vmaxps  xmm0, xmm0, xmm1
  vmovups [RESULT], xmm0
end;

function TBZVector4f.Clamp(constref AMin, AMax: Single): TBZVector4f; assembler; nostackframe; register;
asm
  vmovups xmm0, [EAX]
  vmovaps xmm2, [AMin]
  vmovaps xmm3, [AMax]
  vshufps xmm2, xmm2, xmm2, $00
  vshufps xmm3, xmm3, xmm3, $00
  vmaxps  xmm0, xmm0, xmm2
  vminps  xmm0, xmm0, xmm3
  //vmovups [RESULT], xmm0
  movhlps xmm1,xmm0
end;

function TBZVector4f.Clamp(Constref AMin, AMax: TBZVector4f): TBZVector4f; assembler; nostackframe; register;
asm
  vmovups xmm0, [EAX]
  vmovups xmm1, [AMin]
  vmovups xmm2, [AMax]
  vmaxps  xmm0, xmm0, xmm1
  vminps  xmm0, xmm0, xmm2
  //vmovups [RESULT], xmm0
  movhlps xmm1,xmm0
end;

function TBZVector4f.MulAdd(Constref B, C: TBZVector4f): TBZVector4f; assembler; nostackframe; register;
asm
  vmovups xmm0, [EAX]
  vmovups xmm1, [B]
  vmovups xmm2, [C]
  vmulps  xmm0, xmm0, xmm1
  vaddps  xmm0, xmm0, xmm2
  //vmovups [RESULT], xmm0
  movhlps xmm1,xmm0
end;

function TBZVector4f.MulDiv(Constref B, C: TBZVector4f): TBZVector4f; assembler; nostackframe; register;
asm
  vmovups xmm0, [EAX]
  vmovups xmm1, [B]
  vmovups xmm2, [C]
  vmulps  xmm0, xmm0, xmm1
  vdivps  xmm0,xmm0, xmm2
  //vmovups [RESULT], xmm0
  movhlps xmm1, xmm0
end;

function TBZVector4f.Lerp(Constref B: TBZVector4f; Constref T:Single): TBZVector4f;assembler; nostackframe; register;
asm
  vmovups xmm0, [EAX]
  vmovups xmm1, [B]
  vmovss  xmm2, [T]
  vshufps xmm2, xmm2, xmm2, $0
  vandps  xmm2, xmm2, [cSSE_MASK_NO_W]
  vsubps  xmm1, xmm1, xmm0
  vmulps  xmm1, xmm1, xmm2

  vaddps xmm0, xmm0, xmm1
  //vmovups [RESULT], xmm0
  movhlps xmm1, xmm0
end;

function TBZVector4f.AngleCosine(constref A : TBZVector4f): Single;assembler;
// Result = xmm0
asm
  vmovups xmm2, [EAX]
  vmovups xmm1, [A]
//  vmovaps xmm2, xmm0 // Coypy Self
  vmovaps xmm4, xmm1 // Copy A

  // DotProd in xmm2 RESULT in xmm3
  //andps xmm0, [cSSE_MASK_NO_W]
  vdpps  xmm3, xmm2, xmm1, 01110001b //or $F1

  // Length Self in xmm2 RESULT in xmm2
  vandps  xmm2, xmm2, [cSSE_MASK_NO_W]
  vmulps  xmm2, xmm2, xmm2
  vhaddps xmm2, xmm2, xmm2
  vhaddps xmm2, xmm2, xmm2
  vsqrtss xmm2, xmm2, xmm2

  // Lenght A in xmm4 RESULT in xmm4
  vandps  xmm4, xmm4, [cSSE_MASK_NO_W]
  vmulps  xmm4, xmm4, xmm4
  vhaddps xmm4, xmm4, xmm4
  vhaddps xmm4, xmm4, xmm4
  vsqrtss xmm4, xmm4, xmm4

  vmulps xmm2, xmm2, xmm4
  vdivps xmm0, xmm3, xmm2
  movss [RESULT], {%H-}xmm0
end;

function TBZVector4f.AngleBetween(Constref A, ACenterPoint : TBZVector4f): Single;
Var
  vResult  :  Single;
begin
  asm
    vmovups xmm0, [EAX]
    mov     ecx,  [A]            // A is a pointer on the stack
    vmovups xmm1, [ECX]
    mov     ecx,  [ACenterPoint] // ACenterPoint is a pointer on the stack
    vmovups xmm2, [ECX]
    vsubps  xmm0, xmm0, xmm2
    vsubps  xmm1, xmm1, xmm2

    // Normalize xmm0 result in xmm2
    vmovaps xmm3,xmm0

    vandps  xmm3, xmm3, [cSSE_MASK_ONLY_W]
    vandps  xmm0, xmm0, [cSSE_MASK_NO_W]
    vmovaps xmm2, xmm0

    vmulps  xmm0, xmm0, xmm0
    vhaddps xmm0, xmm0, xmm0
    vhaddps xmm0, xmm0, xmm0

    {$IFDEF USE_ASM_SIMD_HIGHPRECISION}
      // High Precision
      vsqrtps xmm0, xmm0
      vdivps  xmm2, xmm2, xmm0
    {$ELSE}
      // Low precision
      rsqrtps xmm0, xmm0       //approximate reciprocal
      mulps   xmm2, xmm0
    {$ENDIF}
    vaddps  xmm2, xmm2,xmm3
    // Normalize xmm1 result in xmm4
    vmovaps xmm3, xmm1
    vandps  xmm3, xmm3, [cSSE_MASK_ONLY_W]
    vandps  xmm1, xmm1, [cSSE_MASK_NO_W]
    vmovaps xmm4, xmm1

    vmulps  xmm1, xmm1, xmm1
    vhaddps xmm1, xmm1, xmm1
    vhaddps xmm1, xmm1, xmm1
    {$IFDEF USE_ASM_SIMD_HIGHPRECISION}
      // High Precision
      vsqrtps xmm1, xmm1
      vdivps  xmm4, xmm4, xmm1
    {$ELSE}
      // Low precision
      rsqrtps xmm1, xmm1       //approximate reciprocal
      mulps   xmm4, xmm1
    {$ENDIF}
    vaddps xmm4, xmm4, xmm3

    // AngleCosine
    vmovaps xmm1, xmm4
    vmovaps xmm0, xmm2 // Copy A

    // DotProd  xmm0/xmm1 Result in xmm3
    vdpps xmm3, xmm0, xmm1, 01110001b //or $F1

    // Length xmm2 result in xmm2
    vandps  xmm2, xmm2, [cSSE_MASK_NO_W]
    vdpps   xmm2, xmm2, xmm2, $FF;
    vsqrtss xmm2, xmm2, xmm2
    // Lenght  xmm4 result in xmm4
    vandps  xmm4, xmm4, [cSSE_MASK_NO_W]
    vdpps   xmm4, xmm4, xmm4, $FF;
    vsqrtss xmm4, xmm4, xmm4
    vmulps  xmm2, xmm2, xmm4
    vdivps  xmm3, xmm3, xmm2
    movss [vResult], {%H-}xmm3
  end;
  Result := ArcCos(vResult);
end;

function TBZVector4f.Combine(constref V2: TBZVector4f; constref F1: Single): TBZVector4f;assembler; nostackframe; register;
asm
  vmovups xmm0, [EAX]
  vmovups xmm1, [V2]

  movlps  xmm2, [F1]
  vshufps xmm2, xmm2, xmm2, $00 // replicate

  vmulps  xmm1, xmm1, xmm2 //V2*F1
  vaddps  xmm0, xmm0, xmm1 // Self + (V2*F1)

  vandps xmm0, xmm0, [cSSE_MASK_NO_W]
  //vmovups [RESULT], xmm0
  movhlps xmm1, xmm0
end;

function TBZVector4f.Combine2(constref V2: TBZVector4f; Const F1, F2: Single): TBZVector4f;assembler; nostackframe; register;
// F1 = xmm3
asm
//  push RBP
//  mov RBP, RSP

  vmovups xmm0,[EAX]
  vmovups xmm1, [V2]
  movlps  xmm2, {%H-}[F2]  // BZVectorMath.pas(3465,3) Error: Invalid register used in memory reference expression: "xmm3"

  vshufps xmm2, xmm2, xmm2, $00 // replicate
  vshufps xmm3, xmm3, xmm3, $00 // replicate

  vmulps  xmm0, xmm0, xmm3  // Self * F1
  vmulps  xmm1, xmm1, xmm2  // V2 * F2

  vaddps  xmm0, xmm0, xmm1  // (Self * F1) + (V2 * F2)

  vandps  xmm0, xmm0, [cSSE_MASK_NO_W]
  //vmovups [RESULT], xmm0
  movhlps xmm1, xmm0

//  pop RBP
end;

function TBZVector4f.Combine3(constref V2, V3: TBZVector4f; const F1, F2, F3: Single): TBZVector4f;  assembler; nostackframe; register;
asm
//  push RBP
//  mov RBP, RSP

  vmovups xmm0,[EAX]

  movlps  xmm2, [F1]
  movlps  xmm3, [F2]
  movlps  xmm5, [F3]

  vmovups xmm1, [V2]
  vmovaps xmm4, [V3]

  vshufps xmm2, xmm2, xmm2, $00 // replicate
  vshufps xmm3, xmm3, xmm3, $00 // replicate
  vshufps xmm5, xmm5, xmm5, $00 // replicate

  vmulps  xmm0, xmm0, xmm2 // Self * F1
  vmulps  xmm1, xmm1, xmm3 // V2 * F2
  vmulps  xmm4, xmm4, xmm5 // V3 * F3

  vaddps  xmm0, xmm0, xmm1 // (Self * F1) + (V2 * F2)
  vaddps  xmm0, xmm0, xmm4 // ((Self * F1) + (V2 * F2)) + (V3 * F3)

  vandps  xmm0, xmm0, [cSSE_MASK_NO_W]
  //vmovups [RESULT], xmm0
  movhlps xmm1,xmm0
//  pop RBP
end;


{%endregion%}

{%region%----[ Operations on Self ]-----------------------------------------------}

procedure TBZVector4f.pAdd(constref A: TBZVector4f); assembler; nostackframe; register;
asm
  vmovups  xmm0, [EAX]
  vmovups  xmm1, [A]
  vAddps   xmm0, xmm0, xmm1
  vmovups [EAX], xmm0
End;

procedure TBZVector4f.pSub(constref A: TBZVector4f); assembler; nostackframe; register;
asm
  vmovups  xmm0, [EAX]
  vmovups  xmm1, [A]
  vsubps   xmm0, xmm0, xmm1
  vmovups [EAX], xmm0
End;

procedure TBZVector4f.pMul(constref A: TBZVector4f); assembler; nostackframe; register;
asm
  vmovups  xmm0, [EAX]
  vmovups  xmm1, [A]
  vmulps   xmm0, xmm0, xmm1
  vmovups [EAX], xmm0
End;

procedure TBZVector4f.pDiv(constref A: TBZVector4f); assembler; nostackframe; register;
asm
  vmovups  xmm0, [EAX]
  vmovups  xmm1, [A]
  vdivps   xmm0, xmm0, xmm1
  vmovups [EAX], xmm0
End;

procedure TBZVector4f.pAdd(constref A: Single); assembler; nostackframe; register;
asm
  vmovups  xmm0, [EAX]
  vmovss   xmm1, [A]
  vshufps  xmm1, xmm1, xmm1, 0
  vaddps   xmm0, xmm0, xmm1
  vmovups [EAX], xmm0
End;

procedure TBZVector4f.pSub(constref A: Single); assembler; nostackframe; register;
asm
  vmovups  xmm0, [EAX]
  vmovss   xmm1, [A]
  vshufps  xmm1, xmm1, xmm1, 0
  vsubps   xmm0, xmm0, xmm1
  vmovups [EAX], xmm0
End;

procedure TBZVector4f.pMul(constref A: Single); assembler; nostackframe; register;
asm
  vmovups  xmm0, [EAX]
  vmovss   xmm1, [A]
  vshufps  xmm1, xmm1, xmm1, 0
  vmulps   xmm0, xmm0, xmm1
  vmovups [EAX], xmm0
End;

procedure TBZVector4f.pDiv(constref A: Single); assembler; nostackframe; register;
asm
  vmovups  xmm0, [EAX]
  vmovss   xmm1, [A]
  vshufps  xmm1, xmm1, xmm1, 0
  vdivps   xmm0, xmm0, xmm1
  vmovups [EAX], xmm0
End;

procedure TBZVector4f.pInvert;assembler; nostackframe; register;
asm
  vmovups  xmm1, [EAX]
  vmovups  xmm0, [cNullVector4f]
  vsubps   xmm0, xmm0, xmm1
  vmovups [EAX], xmm0
End;

procedure TBZVector4f.pNegate; assembler; nostackframe; register;
asm
  vmovups  xmm0, [EAX]
  vxorps   xmm0, xmm0, [cSSE_MASK_NEGATE]  //xmm1
  vmovups [EAX], xmm0
End;

procedure TBZVector4f.pAbs; assembler; nostackframe; register;
asm
  vmovups  xmm0, [EAX]
  vandps   xmm0, xmm0, [cSSE_MASK_ABS]    // xmm1
  vmovups [EAX], xmm0
end;

procedure TBZVector4f.pDivideBy2;assembler; nostackframe; register;
asm
  vmovups  xmm0, [EAX]
  vmulps   xmm0, xmm0, [cHalfOneVector4f]   //xmm1
  vmovups [EAX], xmm0
end;

procedure TBZVector4f.pCrossProduct(constref A: TBZVector4f);assembler; nostackframe; register;
asm
  //v1:x,y,z,w
  vmovups xmm0, [EAX]
  //v2:x,y,z,w
  vmovups xmm1, [A]
  vmovaps xmm2, xmm0
  vmovaps xmm3, xmm1

  // shuffle members
  vshufps xmm0, xmm0, xmm0, $c9  //w z y x -> $c9 = 11 00 10 01 -> w x z y
  vshufps xmm1, xmm1, xmm1, $d2  //w z y x -> $d2 = 11 01 00 10 -> w y x z
  vshufps xmm2, xmm2, xmm2, $d2
  vshufps xmm3, xmm3, xmm3, $c9
  // scale
  vmulps  xmm0, xmm0, xmm1
  vmulps  xmm2, xmm2, xmm3
  // sub the results
  vsubps  xmm0, xmm0, xmm2
  vaddps  xmm0, xmm0, [cWOneVector4f] // it would better change by logical operator
  vmovups [EAX],xmm0
end;

procedure TBZVector4f.pNormalize;  assembler; nostackframe; register;
asm
  vmovups xmm0, [EAX]
  vmovaps xmm3, xmm0
  vandps  xmm3, xmm3, [cSSE_MASK_ONLY_W]
  vandps  xmm0, xmm0, [cSSE_MASK_NO_W]
  vmovaps xmm2, xmm0
  vmulps  xmm0, xmm0, xmm0
  vhaddps xmm0, xmm0, xmm0
  vhaddps xmm0, xmm0, xmm0
{$IFDEF USE_ASM_SIMD_HIGHPRECISION}
  // High Precision
  vsqrtps xmm0, xmm0
  vdivps  xmm0, xmm2, xmm0
{$ELSE}
  // Low precision
  rsqrtps xmm0, xmm0       //approximate reciprocal
  mulps   xmm2, xmm0
{$ENDIF}
  vaddps  xmm0, xmm0, xmm3
  vmovups [EAX], xmm0
end;

procedure TBZVector4f.pMin(constref B: TBZVector4f); assembler; nostackframe; register;
asm
  vmovups  xmm0, [EAX]
  vmovups  xmm1, [B]
  vminps   xmm0, xmm0, xmm1
  vmovups [EAX], xmm0
End;

procedure TBZVector4f.pMin(constref B: Single); assembler; nostackframe; register;
asm
  vmovups  xmm0, [EAX]
  vmovss   xmm1, [B]
  vshufps  xmm1, xmm1, xmm1, $00
  vminps   xmm0, xmm0, xmm1
  vmovups [EAX], xmm0
End;

procedure TBZVector4f.pMax(constref B: TBZVector4f); assembler; nostackframe; register;
asm
  vmovups  xmm0, [EAX]
  vmovups  xmm1, [B]
  vmaxps   xmm0, xmm0, xmm1
  vmovups [EAX], xmm0
End;

procedure TBZVector4f.pMax(constref B: Single); assembler; nostackframe; register;
asm
  vmovups  xmm0, [EAX]
  vmovss   xmm1, [B]
  vshufps  xmm1, xmm1, xmm1, $00
  vmaxps   xmm0, xmm0, xmm1
  vmovups [EAX], xmm0
End;

procedure TBZVector4f.pClamp(Constref AMin, AMax: TBZVector4f); assembler; nostackframe; register;
asm
  vmovups  xmm0, [EAX]
  vmovups  xmm2, [AMin]
  vmovups  xmm3, [AMax]
  vmaxps   xmm0, xmm0, xmm2
  vminps   xmm0, xmm0, xmm3
  vmovups [EAX], xmm0
End;

procedure TBZVector4f.pClamp(constref AMin, AMax: Single); assembler; nostackframe; register;
asm
  vmovups  xmm0, [EAX]
  vmovss   xmm2, [AMin]
  vmovss   xmm3, [AMax]
  vshufps  xmm2, xmm2, xmm2, $00 // Replicate AMin
  vshufps  xmm3, xmm3, xmm3, $00 // Replicate AMax
  vmaxps   xmm0, xmm0, xmm2
  vminps   xmm0, xmm0, xmm3
  vmovups [EAX], xmm0
End;

procedure TBZVector4f.pMulAdd(Constref B, C: TBZVector4f); assembler; nostackframe; register; // (Self*B)+c
asm
  vmovups  xmm0, [EAX]
  vmovups  xmm1, [B]
  vmovups  xmm2, [C]
  vmulps   xmm0, xmm0, xmm1
  vaddps   xmm0, xmm0, xmm2
  vmovups [EAX], xmm0
End;

procedure TBZVector4f.pMulDiv(Constref B, C: TBZVector4f); assembler; nostackframe; register; // (Self*B)-c
asm
  vmovups  xmm0, [EAX]
  vmovups  xmm1, [B]
  vmovups  xmm2, [C]
  vmulps   xmm0, xmm0, xmm1
  vdivps   xmm0, xmm0, xmm2
  vmovups [EAX], xmm0
End;

{%endregion%}

