{%region%====[ SSE Vectors Operators ]==========================================}

class operator TBZVector4f.+(constref A, B: TBZVector4f): TBZVector4f; assembler; nostackframe; register;
asm
  movups xmm0, [A]
  movups xmm1, [B]
  addps  xmm0, xmm1
  movups [RESULT], xmm0
end;

class operator TBZVector4f.-(constref A, B: TBZVector4f): TBZVector4f; assembler; nostackframe; register;
asm
  movups xmm0, [A]
  movups xmm1, [B]
  subps  xmm0, xmm1
  movups [RESULT], xmm0
end;

class operator TBZVector4f.*(constref A, B: TBZVector4f): TBZVector4f; assembler; nostackframe; register;
asm
  movups xmm0, [A]
  movups xmm1, [B]
  mulps  xmm0, xmm1
  movups [RESULT], xmm0
end;

class operator TBZVector4f./(constref A, B: TBZVector4f): TBZVector4f; assembler; nostackframe; register;
asm
  movups xmm0, [A]
  movups xmm1, [B]
  divps  xmm0, xmm1
  movups [RESULT], xmm0
end;

class operator TBZVector4f.+(constref A: TBZVector4f; constref B:Single): TBZVector4f; assembler; nostackframe; register;
asm
  movups xmm0, [A]
  movss  xmm1, [B]
  shufps xmm1, xmm1, $00
  addps  xmm0, xmm1
  movups [RESULT], xmm0
end;

class operator TBZVector4f.-(constref A: TBZVector4f; constref B:Single): TBZVector4f; assembler; nostackframe; register;
asm
  movups xmm0, [A]
  movss  xmm1, [B]
  shufps xmm1, xmm1, $00
  subps  xmm0, xmm1
  movups [RESULT], xmm0
end;

class operator TBZVector4f.*(constref A: TBZVector4f; constref B:Single): TBZVector4f; assembler; nostackframe; register;
asm
  movups xmm0, [A]
  movss  xmm1, [B]
  shufps xmm1, xmm1, $00
  mulps  xmm0, xmm1
  movups [RESULT], xmm0
end;

class operator TBZVector4f./(constref A: TBZVector4f; constref B:Single): TBZVector4f; assembler; nostackframe; register;
asm
  movups xmm0, [A]
  movss  xmm1, [B]
  shufps xmm1, xmm1, $00
  divps  xmm0, xmm1
  movups [RESULT], xmm0
end;

class operator TBZVector4f.-(constref A: TBZVector4f): TBZVector4f; assembler; nostackframe; register;
asm
  movups xmm1, [A]
  movups xmm0, [cNullVector4f]
  subps  xmm0, xmm1
  movups [RESULT], xmm0
End;

class operator TBZVector4f.= (constref A, B: TBZVector4f): boolean; assembler; nostackframe; register;
asm
  movups  xmm0, [A]
  movups  xmm1, [B]
  cmpps   xmm0, xmm1, cSSE_OPERATOR_EQUAL    //  Yes: $FFFFFFFF, No: $00000000 ; 0 = Operator Equal
  movmskps eax, xmm0
  movmskps eax, xmm0
  xor eax, $F
  setz al
end;

class operator TBZVector4f.<= (constref A, B: TBZVector4f): boolean; assembler; nostackframe; register;
asm
  movups  xmm0, [A]
  movups  xmm1, [B]
  cmpps   xmm0, xmm1, cSSE_OPERATOR_LESS_OR_EQUAL    //  Yes: $FFFFFFFF, No: $00000000 ; 2 = Operator Less or Equal
  movmskps eax, xmm0
  xor eax, $F
  setz al
end;

class operator TBZVector4f.>= (constref A, B: TBZVector4f): boolean; assembler; nostackframe; register;
asm
  movups  xmm0, [A]
  movups  xmm1, [B]
  cmpps   xmm0, xmm1, cSSE_OPERATOR_NOT_LESS_OR_EQUAL
  movmskps eax, xmm0
  xor eax, $F
  setz al
end;

class operator TBZVector4f.< (constref A, B: TBZVector4f): boolean; assembler; nostackframe; register;
asm
  movups  xmm0, [A]
  movups  xmm1, [B]
  cmpps   xmm0, xmm1, cSSE_OPERATOR_LESS
  movmskps eax, xmm0
  xor eax, $F
  setz al
end;

class operator TBZVector4f.>(constref A, B: TBZVector4f): boolean; assembler; nostackframe; register;
asm
  movups  xmm0, [A]
  movups  xmm1, [B]
  cmpps   xmm0, xmm1, cSSE_OPERATOR_NOT_LESS
  movmskps eax, xmm0
  xor eax, $F
  setz al
end;

class operator TBZVector4f.<> (constref A, B: TBZVector4f): boolean; assembler; nostackframe; register;
asm
  movups  xmm0, [A]
  movups  xmm1, [B]
  cmpps   xmm0, xmm1, cSSE_OPERATOR_NOT_EQUAL
  movmskps eax, xmm0
  xor eax, $F
  setz al
end;
{%endregion}
{%region%====[ SSE Vectors Functions ]==========================================}


function TBZVector4f.Abs: TBZVector4f; assembler; nostackframe; register;
asm
  movups xmm0, [EAX]
  movups xmm1, [cSSE_MASK_ABS]
  andps  xmm0, xmm1
  movups [RESULT], xmm0
end;

function TBZVector4f.Negate:TBZVector4f; assembler; nostackframe; register;
asm
  movups xmm0, [EAX]
  movups xmm1, [cSSE_MASK_NEGATE]
  xorps  xmm0, xmm1
  movups [RESULT], xmm0
End;

function TBZVector4f.DivideBy2:TBZVector4f;assembler; nostackframe; register;
asm
  movups xmm0, [EAX]
  movups xmm1, [cHalfOneVector4f]
  mulps  xmm0, xmm1
  movups [RESULT], xmm0
end;

function TBZVector4f.Distance(constref A: TBZVector4f):Single;assembler;
//nostackframe;register; we cannot use this because the result is ST register
//result := sqrt(sqr(Self.X-A.X)+ sqr(Self.Y-A.Y) + sqr(Self.Z-A.Z));
Asm
  {$IFDEF USE_ASM_SSE_4}
    movups xmm0, [EAX]
    movups xmm1, [A]
    subps  xmm0, xmm1
    andps  xmm0, [cSSE_MASK_NO_W]
    dpps   xmm0, xmm0, $FF;
    sqrtss xmm0, xmm0
  {$ELSE}
    {$IFDEF USE_ASM_SSE_3}
       movups xmm0, [EAX]
       movups xmm1, [A]
       subps  xmm0, xmm1
       andps  xmm0, [cSSE_MASK_NO_W]
       mulps  xmm0, xmm0
       haddps xmm0, xmm0
       haddps xmm0, xmm0
       sqrtss xmm0, xmm0
    {$ELSE}
       movups  xmm1,[EAX]
       movups  xmm0, [A]
       subps   xmm1, xmm0
       andps   xmm1, [cSSE_MASK_NO_W]
       mulps   xmm1, xmm1
       movhlps xmm0, xmm1
       addss   xmm0, xmm1
       shufps  xmm1, xmm1, $55
       addss   xmm0, xmm1
       {.$IFDEF USE_ASM_SIMD_HIGHPRECISION}
       // High Precision
       sqrtss  xmm0, xmm0
       {.$ELSE
           // Low precision - note : may be very inaccurate
           rsqrtss xmm0, xmm0
           rcpss xmm0, xmm0
       .$ENDIF}
     {$ENDIF}
  {$ENDIF}
  movss [RESULT], {%H-}xmm0
end;

function TBZVector4f.DistanceSquare(constref A: TBZVector4f):Single;assembler;
//nostackframe;register; we cannot use this because the result is ST register
//result := sqrt(sqr(Self.X-A.X)+ sqr(Self.Y-A.Y) + sqr(Self.Z-A.Z));
Asm
  {$IFDEF USE_ASM_SSE_4}
    movups xmm0, [EAX]
    movups xmm1, [A]
    subps  xmm0, xmm1
    andps  xmm0, [cSSE_MASK_NO_W]
    dpps   xmm0, xmm0, $FF;
  {$ELSE}
    {$IFDEF USE_ASM_SSE_3}
       movups xmm0, [EAX]
       movups xmm1, [A]
       subps  xmm0, xmm1
       andps  xmm0, [cSSE_MASK_NO_W]
       mulps  xmm0, xmm0
       haddps xmm0, xmm0
       haddps xmm0, xmm0
    {$ELSE}
       movups  xmm1, [EAX]
       movups  xmm0, [A]
       subps   xmm1, xmm0
       andps   xmm1, [cSSE_MASK_NO_W]
       mulps   xmm1, xmm1
       movhlps xmm0, xmm1
       addss   xmm0, xmm1
       shufps  xmm1, xmm1, $55
       addss   xmm0, xmm1
     {$ENDIF}
  {$ENDIF}
  movss [RESULT], {%H-}xmm0
end;

function TBZVector4f.Length:Single;assembler;
//nostackframe;register; we cannot use this because the result is ST register
Asm
  {$IFDEF USE_ASM_SSE_4}
    movups xmm0, [EAX]
    andps  xmm0, [cSSE_MASK_NO_W]
    dpps   xmm0, xmm0, $FF;
    sqrtss xmm0, xmm0
  {$ELSE}
    {$IFDEF USE_ASM_SSE_3}
      movups xmm0, [EAX]
      andps  xmm0, [cSSE_MASK_NO_W]
      mulps  xmm0, xmm0
      haddps xmm0, xmm0
      haddps xmm0, xmm0
      sqrtss xmm0, xmm0
    {$ELSE}
      movups  xmm1, [EAX]
      andps   xmm1, [cSSE_MASK_NO_W]
      mulps   xmm1, xmm1
      movhlps xmm0, xmm1
      addss   xmm0, xmm1
      shufps  xmm1, xmm1, $55
      addss   xmm0, xmm1
      {.$IFDEF USE_ASM_SIMD_HIGHPRECISION}
      // High Precision
      sqrtss  xmm0, xmm0
      {.$ELSE
          // Low precision - note : may be very inaccurate
          rsqrtss xmm0, xmm0
          rcpss xmm0, xmm0
      .$ENDIF}
    {$ENDIF}
  {$ENDIF}
  movss [Result], {%H-}xmm0
end;

function TBZVector4f.LengthSquare:Single;assembler;
//nostackframe;register; we cannot use this because the result is ST register
Asm
  {$IFDEF USE_ASM_SSE_4}
    movups xmm0, [EAX]
    andps  xmm0, [cSSE_MASK_NO_W]
    dpps   xmm0, xmm0, $FF;
  {$ELSE}
    {$IFDEF USE_ASM_SSE_3}
      movups xmm0, [EAX]
      andps  xmm0, [cSSE_MASK_NO_W]
      mulps  xmm0, xmm0
      haddps xmm0, xmm0
      haddps xmm0, xmm0
    {$ELSE}
      movups  xmm1, [EAX]
      andps   xmm1, [cSSE_MASK_NO_W]
      mulps   xmm1, xmm1
      movhlps xmm0, xmm1
      addss   xmm0, xmm1
      shufps  xmm1, xmm1, $55
      addss   xmm0, xmm1
    {$ENDIF}
  {$ENDIF}
  movss [Result], {%H-}xmm0
end;

function TBZVector4f.Spacing(constref A : TBZVector4f) : Single; assembler; //nostackframe; register;
asm
  //Result:=Abs(v2.X-Self.X)+Abs(v2.Y-Self.Y)+Abs(v2.Z-Self.Z)+Abs(v2.W-Self.W);
  movups xmm1, [EAX]
  movups xmm0, [A]
  subps  xmm0, xmm1
  andps  xmm0, [cSSE_MASK_ABS]
  {$IFDEF USE_ASM_SSE_3}
    {$ifdef TEST}
      movshdup xmm1, xmm0
      addps    xmm0, xmm1
      movhlps  xmm1, xmm0
      addss    xmm0, xmm1
    {$else}
      haddps xmm0, xmm0
      haddps xmm0, xmm0
    {$endif}
  {$else}
     movhlps xmm1, xmm0             // xmm1 =  - | -  | w  | z  |
     addss   xmm1, xmm0             // x + z
     shufps  xmm0, xmm0, 00000001b  // xmm0 =  - | -  | -  | y  |
     addss   xmm0, xmm1             // (x + z ) + y
     shufps  xmm1, xmm1, 01010101b  // xmm1 =  - | -  | -  | w  |
     addss   xmm0, xmm1             // (x + z  + y) + w
  {$endif}
  movss [Result], {%H-}xmm0
end;


function TBZVector4f.CrossProduct(constref A: TBZVector4f): TBZVector4f;assembler; nostackframe; register;
asm
  // Place unaligned data to registers
  //v1:x,y,z,w
  movups xmm0, [EAX]
  //v2:x,y,z,w
  movups xmm1, [A]                // xmm1 = v2
  movups xmm2, xmm0                // xmm2 = v1 // move aligned data
  movups xmm3, xmm1                // xmm3 = v2
  // shuffle members
  // becarefull at the order in the register (right to left )= w z x y )
  // 1st arg xmm = w z y x = 11 10 01 00
  // 2nd arg xmm = w z y x = 11 10 01 00
  // 3rd arg where to place members of 1st arg in 2nd arg
  // becarefull reading from right to left
  shufps xmm0, xmm0, $c9  //w z y x -> $c9 = 11 00 10 01 -> w x z y

  shufps xmm1, xmm1, $d2  //w z y x -> $d2 = 11 01 00 10 -> w y x z
  shufps xmm2, xmm2, $d2
  shufps xmm3, xmm3, $c9
  // scale
  mulps  xmm0, xmm1
  mulps  xmm2, xmm3
  // sub the results
  subps  xmm0, xmm2
  addps  xmm0, [cWOneVector4f] // it would better change by logical operator
  movups [RESULT], xmm0      // return result
end;

// if the return is > 0 then the angle between the 2 vectors is  > 90° else is < 90°
// Usefull for conputing backface culling and lighting
function TBZVector4f.DotProduct(constref A: TBZVector4f):Single;assembler;
asm
  {$IFDEF USE_ASM_SSE_4}
    movups xmm0, [EAX]
    movups xmm1, [A]
     // Becarefull at the order of mask Right to Left
     // 4 high bits: which elements should be summed. (w,z,y,x)
     // 4 low bits: which output slots should contain the result. (3,2,1,0)
     // mask =  0111b 0001;
     dpps  xmm0, xmm1, 01110001b //or $F1
  {$ELSE}
    {$IFDEF USE_ASM_SSE_3}
      movups  xmm1, [EAX]
      movups  xmm0, [A]
      andps   xmm1, [cSSE_MASK_NO_W]
      mulps   xmm0, xmm1
      haddps  xmm0, xmm0
      haddps  xmm0, xmm0
    {$ELSE}
      movups  xmm0, [EAX]
      movups  xmm1, [A]
      andps   xmm0, [cSSE_MASK_NO_W]
      mulps   xmm0, xmm1             //   xmm0 =  w | z  | y  | x  |
      movhlps xmm1, xmm0             //   xmm1 =  - | -  | w  | z  |
      addps   xmm1, xmm0             // x + z
      // How to with shufps
      //  In xmm0 =  w | z  | y  | x  |
      //          = 11 | 10 | 01 | 00 |
      // Out xmm0 =  - | -  | -  | Y  |
      //          = 00 | 00 | 00 | 01 | ==> 00000001b
      shufps xmm0, xmm0, 00000001b   //   xmm1 =  - | -  | -  | y  |
      addps  xmm0, xmm1              // (x + z ) + y
    {$ENDIF}
  {$ENDIF}
  movss [RESULT], {%H-}xmm0
end;

function TBZVector4f.Norm:Single;assembler;
asm
  movups xmm0, [EAX]
  //andps xmm0, [cSSE_MASK_NO_W]
  mulps  xmm0, xmm0
  movups xmm1, xmm0
  shufps xmm0, xmm1, $4e
  addps  xmm0, xmm1
  shufps xmm1, xmm0, $11
  addps  xmm0, xmm1
  movss [RESULT], {%H-}xmm0
end;

function TBZVector4f.Normalize: TBZVector4f;  assembler; nostackframe; register;
asm
  movups xmm0, [EAX]
  movups xmm3, xmm0
  andps  xmm3, [cSSE_MASK_ONLY_W]
  andps  xmm0, [cSSE_MASK_NO_W]
  movups xmm2, xmm0
  {$IFDEF USE_ASM_SSE_3}
    mulps  xmm0, xmm0
    haddps xmm0, xmm0
    haddps xmm0, xmm0
  {$ELSE}
    mulps  xmm0, xmm0
    movups xmm1, xmm0
    shufps xmm0, xmm1, $4e
    addps  xmm0, xmm1
    movups xmm1, xmm0
    shufps xmm1, xmm1, $11
    addps  xmm0, xmm1
  {$ENDIF}
  {$IFDEF USE_ASM_SIMD_HIGHPRECISION}
    // High Precision
    sqrtps xmm0, xmm0
    divps  xmm2, xmm0
  {$ELSE}
    // Low precision
    rsqrtps xmm0, xmm0       //approximate reciprocal
    mulps   xmm2, xmm0
  {$ENDIF}
  addps xmm2, xmm3
  movups [RESULT], xmm2
end;

function TBZVector4f.Min(constref B: Single): TBZVector4f; assembler; nostackframe; register;
asm
  movups xmm0, [EAX]
  movss  xmm1, [B]
  shufps xmm1, xmm1, $00 // Replicate B
  minps  xmm0, xmm1
  movups [RESULT], xmm0
end;

function TBZVector4f.Min(constref B: TBZVector4f): TBZVector4f; assembler; nostackframe; register;
asm
  movups xmm0, [EAX]
  movups xmm1, [B]
  minps  xmm0, xmm1
  movups [RESULT], xmm0
end;

function TBZVector4f.Max(constref B: Single): TBZVector4f; assembler; nostackframe; register;
asm
  movups xmm0, [EAX]
  movss  xmm1, [B]
  shufps xmm1, xmm1, $00 // Replicate B
  maxps  xmm0, xmm1
  movups [RESULT], xmm0
end;

function TBZVector4f.Max(constref B: TBZVector4f): TBZVector4f; assembler; nostackframe; register;
asm
  movups xmm0, [EAX]
  movups xmm1, [B]
  maxps  xmm0, xmm1
  movups [RESULT], xmm0
end;

function TBZVector4f.Clamp(constref AMin, AMax: Single): TBZVector4f; assembler; nostackframe; register;
asm
  movups  xmm0, [EAX]
  movss   xmm2, [AMin]
  movss   xmm3, [AMax]
  shufps  xmm2, xmm2, $00 // Replicate AMin
  shufps  xmm3, xmm3, $00 // Replicate AMax
  maxps   xmm0, xmm2
  minps   xmm0, xmm3
  movhlps xmm1, xmm0
end;

function TBZVector4f.Clamp(Constref AMin, AMax: TBZVector4f): TBZVector4f; assembler; nostackframe; register;
asm
  movups  xmm0, [EAX]
  movups  xmm1, [AMin]
  movups  xmm2, [AMax]
  maxps   xmm0, xmm1
  minps   xmm0, xmm2
  movhlps xmm1, xmm0
end;

function TBZVector4f.MulAdd(Constref B, C: TBZVector4f): TBZVector4f; assembler; nostackframe; register;
asm
  movups  xmm0, [EAX]
  movups  xmm1, [B]
  movups  xmm2, [C]
  mulps   xmm0, xmm1
  addps   xmm0, xmm2
  movhlps xmm1, xmm0
end;

function TBZVector4f.MulDiv(Constref B, C: TBZVector4f): TBZVector4f; assembler; nostackframe; register;
asm
  movups  xmm0, [EAX]
  movups  xmm1, [B]
  movups  xmm2, [C]
  mulps   xmm0, xmm1
  Divps   xmm0, xmm2
  movhlps xmm1, xmm0
end;

function TBZVector4f.Lerp(Constref B: TBZVector4f; Constref T:Single): TBZVector4f; assembler; nostackframe; register;
asm
  movups  xmm0, [EAX]
  movups  xmm1, [B]
  movss   xmm2, [T]
  shufps  xmm2, xmm2, $0
  andps   xmm2, [cSSE_MASK_NO_W]
  subps   xmm1, xmm0
  mulps   xmm1, xmm2

  addps   xmm0, xmm1
  movhlps xmm1, xmm0
end;

function TBZVector4f.AngleCosine(constref A : TBZVector4f): Single; assembler;
//Result:=Self.DotProduct(A)/(Self.Length*A.Length);
asm
  movups xmm0, [EAX]

  movups xmm1, [A]
  movups xmm2, xmm0 // Coypy Self
  movups xmm4, xmm1 // Copy A

 // DotProd in xmm0 Result in xmm3
 {$IFDEF USE_ASM_SSE_4}
    dpps   xmm0, xmm1, 01110001b //or $F1
    movups xmm3, xmm0
 {$ELSE}
   {$IFDEF USE_ASM_SSE_3}
     andps   xmm0, [cSSE_MASK_NO_W]
     mulps   xmm1, xmm0
     haddps  xmm1, xmm1
     haddps  xmm1, xmm1
     movups  xmm3, xmm1
   {$ELSE}
     mulps   xmm1, xmm0
     movhlps xmm0, xmm1
     addps   xmm0, xmm1
     shufps  xmm1, xmm1, 00000001b
     addps   xmm1, xmm0
     movups  xmm3, xmm1
   {$ENDIF}
 {$ENDIF}
  // Length Self in xmm2 result in xmm2
  andps xmm2, [cSSE_MASK_NO_W]
  {$IFDEF USE_ASM_SSE_4}
    dpps   xmm2, xmm2, $FF;
    sqrtss xmm2, xmm2
  {$ELSE}
    mulps   xmm2, xmm2
    {$IFDEF USE_ASM_SSE_3}
      haddps xmm2, xmm2
      haddps xmm2, xmm2
      sqrtss xmm2, xmm2
    {$ELSE}
      movhlps xmm1, xmm2
      addss   xmm1, xmm2
      shufps  xmm2, xmm2, $55
      addss   xmm1, xmm2
      sqrtss  xmm2, xmm1
    {$ENDIF}
  {$ENDIF}
  // Lenght A in xmm4 result in xmm4
  andps xmm4, [cSSE_MASK_NO_W]
  {$IFDEF USE_ASM_SSE_4}
    dpps   xmm4, xmm4, $FF;
    sqrtss xmm4, xmm4
  {$ELSE}
    mulps    xmm4, xmm4
    {$IFDEF USE_ASM_SSE_3}
      haddps xmm4, xmm4
      haddps xmm4, xmm4
      sqrtss xmm4, xmm4
    {$ELSE}
      movhlps xmm1, xmm4
      addss   xmm1, xmm4
      shufps  xmm4, xmm4, $55
      addss   xmm1, xmm4
      sqrtss  xmm4, xmm1
    {$ENDIF}
  {$ENDIF}
  mulps xmm2, xmm4
  divps xmm3, xmm2
  movss [RESULT], {%H-}xmm3
end;

function TBZVector4f.AngleBetween(Constref A, ACenterPoint : TBZVector4f): Single;
Var
  vResult  :  Single;
begin
  asm
    movups xmm0, [EAX]
    mov    ecx,  [A]            // A is a pointer on the stack
    movups xmm1, [ecx]
    mov    ecx,  [ACenterPoint] // ACenterPoint is a pointer on the stack
    movups xmm2, [ecx]
    subps  xmm0, xmm2
    subps  xmm1, xmm2
    // Normalize xmm0 result in xmm2
    movups xmm3, xmm0
    andps  xmm3, [cSSE_MASK_ONLY_W]
    andps  xmm0, [cSSE_MASK_NO_W]
    movups xmm2, xmm0
    {$IFDEF USE_ASM_SSE_3}
      mulps  xmm0, xmm0
      haddps xmm0, xmm0
      haddps xmm0, xmm0
    {$ELSE}
      mulps  xmm0, xmm0
      movups xmm5, xmm0
      shufps xmm0, xmm5, $4e
      addps  xmm0, xmm5
      movups xmm5, xmm0
      shufps xmm5, xmm5, $11
      addps  xmm0, xmm5
    {$ENDIF}
    {$IFDEF USE_ASM_SIMD_HIGHPRECISION}
      // High Precision
      sqrtps xmm0, xmm0
      divps  xmm2, xmm0
    {$ELSE}
      // Low precision
      rsqrtps xmm0, xmm0       //approximate reciprocal
      mulps   xmm2, xmm0
    {$ENDIF}
    addps  xmm2, xmm3
    // Normalize xmm1 result in xmm4
    movups xmm3, xmm1
    andps  xmm3, [cSSE_MASK_ONLY_W]
    andps  xmm1, [cSSE_MASK_NO_W]
    movups xmm4, xmm1
    {$IFDEF USE_ASM_SSE_3}
      mulps  xmm1, xmm1
      haddps xmm1, xmm1
      haddps xmm1, xmm1
    {$ELSE}
      mulps  xmm1, xmm1
      movups xmm5, xmm1
      shufps xmm1, xmm5, $4e
      addps  xmm1, xmm5
      movups xmm5, xmm1
      shufps xmm5, xmm5, $11
      addps  xmm1, xmm5
    {$ENDIF}
    {$IFDEF USE_ASM_SIMD_HIGHPRECISION}
      // High Precision
      sqrtps xmm1, xmm1
      divps  xmm4, xmm1
    {$ELSE}
      // Low precision
      rsqrtps xmm1, xmm1       //approximate reciprocal
      mulps   xmm4, xmm1
    {$ENDIF}
    addps xmm4,xmm3

    // AngleCosine
    movups xmm1, xmm4
    movups xmm0, xmm2 // Copy A

    // DotProd  xmm0/xmm1 Result in xmm3
    {$IFDEF USE_ASM_SSE_4}
      dpps   xmm0, xmm1, 01110001b //or $F1
      movups xmm3, xmm0
    {$ELSE}
      {$IFDEF USE_ASM_SSE_3}
        andps  xmm0, [cSSE_MASK_NO_W]
        mulps  xmm1, xmm0
        haddps xmm1, xmm1
        haddps xmm1, xmm1
        movups xmm3, xmm1
      {$ELSE}
        mulps   xmm1, xmm0
        movhlps xmm0, xmm1
        addps   xmm0, xmm1
        shufps  xmm1, xmm1, 00000001b
        addps   xmm1, xmm0
        movups  xmm3, xmm1
     {$ENDIF}
   {$ENDIF}
    // Length xmm2 result in xmm2
    andps xmm2, [cSSE_MASK_NO_W]
    {$IFDEF USE_ASM_SSE_4}
      dpps   xmm2, xmm2, $FF;
      sqrtss xmm2, xmm2
    {$ELSE}
      mulps   xmm2, xmm2
      {$IFDEF USE_ASM_SSE_3}
        haddps xmm2, xmm2
        haddps xmm2, xmm2
        sqrtss xmm2, xmm2
      {$ELSE}
        movhlps xmm1, xmm2
        addss   xmm1, xmm2
        shufps  xmm2, xmm2, $55
        addss   xmm1, xmm2
        sqrtss  xmm2, xmm1
      {$ENDIF}
    {$ENDIF}
    // Lenght  xmm4 result in xmm4
    andps xmm4, [cSSE_MASK_NO_W]
    {$IFDEF USE_ASM_SSE_4}
      dpps   xmm4, xmm4, $FF;
      sqrtss xmm4, xmm4
    {$ELSE}
      mulps   xmm4, xmm4
      {$IFDEF USE_ASM_SSE_3}
        haddps xmm4, xmm4
        haddps xmm4, xmm4
        sqrtss xmm4, xmm4
      {$ELSE}
        movhlps xmm1, xmm4
        addss   xmm1, xmm4
        shufps  xmm4, xmm4, $55
        addss   xmm1, xmm4
        sqrtss  xmm4, xmm1
      {$ENDIF}
    {$ENDIF}
    mulps xmm2, xmm4
    divps xmm3, xmm2
    movss [vResult], {%H-}xmm3
  end;
  Result := ArcCos(vResult);
end;

function TBZVector4f.Combine(constref V2: TBZVector4f; constref F1: Single): TBZVector4f;assembler; nostackframe; register;
asm
  movups  xmm0, [EAX]
  movups  xmm1, [V2]
  movss   xmm2, [F1]
  shufps  xmm2, xmm2, $00        // replicate
  mulps   xmm1, xmm2             // V2*F1
  addps   xmm0, xmm1             // Self + (V2*F1)
  andps   xmm0, [cSSE_MASK_NO_W]
  movhlps xmm1, xmm0
end;

function TBZVector4f.Combine2(constref V2: TBZVector4f; const F1, F2: Single): TBZVector4f;assembler; nostackframe; register;
asm

  movups xmm0, [EAX]
  movups xmm1, [V2]

  movss  xmm3, [F1{%H-}]
  movss  xmm2, [F2{%H-}]

  shufps xmm2, xmm2, $00 // replicate
  shufps xmm3, xmm3, $00 // replicate F1 already there

  mulps  xmm0, xmm3  // Self * F1
  mulps  xmm1, xmm2  // V2 * F2

  addps  xmm0, xmm1  // (Self * F1) + (V2 * F2)
  andps  xmm0, [cSSE_MASK_NO_W]

  movhlps xmm1, xmm0
end;

function TBZVector4f.Combine3(constref V2, V3: TBZVector4f; const F1, F2, F3: Single): TBZVector4f;  assembler; nostackframe; register;
asm
  movups  xmm0, [EAX]
  movups  xmm1, [V2]
  movups  xmm4, [V3]

  movss   xmm2, [F1{%H-}]
  movss   xmm3, [F2{%H-}]
  movss   xmm5, [F3{%H-}]

  shufps  xmm2, xmm2, $00 // replicate
  shufps  xmm3, xmm3, $00 // replicate
  shufps  xmm5, xmm5, $00 // replicate

  mulps   xmm0, xmm2      // Self * F1
  mulps   xmm1, xmm3      // V2 * F2
  mulps   xmm4, xmm5      // V3 * F3

  addps   xmm0, xmm1      // (Self * F1) + (V2 * F2)
  addps   xmm0, xmm4      // ((Self * F1) + (V2 * F2)) + (V3 * F3)

  andps   xmm0, [cSSE_MASK_NO_W]
  movhlps xmm1, xmm0
end;


function TBZVector4f.Round: TBZVector4i;assembler;nostackframe;register;
asm
  // Rounding mode defaults to round-to-nearest
  movups   xmm0, [EAX]
  cvtps2dq xmm0, xmm0
  movdqu   [Result], xmm0
end;


function TBZVector4f.Trunc: TBZVector4i;assembler;nostackframe;register;
asm
  movups   xmm0, [EAX]
  {$i macro_sse_win64_trunc_xmm0-xmm0.inc}
  movdqu   [Result], xmm0
end;


{%endregion%}
{%region%====[ SSE Operations on Self ]=========================================}

{ procedure TBZVector4f.pAdd(constref A: TBZVector4f); assembler; nostackframe; register;
asm
  movups  xmm0, [EAX]
  movups  xmm1, [A]
  Addps   xmm0, xmm1
  movups [EAX], xmm0
End;

procedure TBZVector4f.pSub(constref A: TBZVector4f); assembler; nostackframe; register;
asm
  movups  xmm0, [EAX]
  movups  xmm1, [A]
  subps   xmm0, xmm1
  movups [EAX], xmm0
End;

procedure TBZVector4f.pMul(constref A: TBZVector4f); assembler; nostackframe; register;
asm
  movups  xmm0, [EAX]
  movups  xmm1, [A]
  mulps   xmm0, xmm1
  movups [EAX], xmm0
End;

procedure TBZVector4f.pDiv(constref A: TBZVector4f); assembler; nostackframe; register;
asm
  movups  xmm0, [EAX]
  movups  xmm1, [A]
  divps   xmm0, xmm1
  movups [EAX], xmm0
End;

procedure TBZVector4f.pAdd(constref A: Single); assembler; nostackframe; register;
asm
  movups  xmm0, [EAX]
  movss   xmm1, [A]
  shufps  xmm1, xmm1, 0
  addps   xmm0, xmm1
  movups [EAX], xmm0
End;

procedure TBZVector4f.pSub(constref A: Single); assembler; nostackframe; register;
asm
  movups  xmm0, [EAX]
  movss   xmm1, [A]
  shufps  xmm1, xmm1, 0
  subps   xmm0, xmm1
  movups [EAX], xmm0
End;

procedure TBZVector4f.pMul(constref A: Single); assembler; nostackframe; register;
asm
  movups  xmm0, [EAX]
  movss   xmm1, [A]
  shufps  xmm1, xmm1, 0
  mulps   xmm0, xmm1
  movups [EAX], xmm0
End;

procedure TBZVector4f.pDiv(constref A: Single); assembler; nostackframe; register;
asm
  movups  xmm0, [EAX]
  movss   xmm1, [A]
  shufps  xmm1, xmm1, 0
  divps   xmm0, xmm1
  movups [EAX], xmm0
End;

procedure TBZVector4f.pInvert;assembler; nostackframe; register;
asm
  movups  xmm0, [EAX]
  movups  xmm1, [cSSE_MASK_NEGATE]
  xorps   xmm0, xmm1
  movups [EAX], xmm0
End;

procedure TBZVector4f.pNegate; assembler; nostackframe; register;
asm
  movups  xmm0, [EAX]
  xorps   xmm0, [cSSE_MASK_NEGATE]
  movups [EAX], xmm0
End;

procedure TBZVector4f.pAbs; assembler; nostackframe; register;
asm
  movups  xmm0, [EAX]
  andps   xmm0, [cSSE_MASK_ABS]
  movups [EAX], xmm0
end;

procedure TBZVector4f.pDivideBy2;assembler; nostackframe; register;
asm
  movups  xmm0, [EAX]
  movups  xmm1, [cHalfOneVector4f]
  mulps   xmm0, xmm1
  movups [EAX], xmm0
end;

procedure TBZVector4f.pCrossProduct(constref A: TBZVector4f);assembler; nostackframe; register;
asm
 // Place unaligned data to registers
 //v1:x,y,z,w
  movups xmm0, [EAX]

 //v2:x,y,z,w
  movups xmm1, [A]                 // xmm1 = v2
  movups xmm2, xmm0                // xmm2 = v1 // move aligned data
  movups xmm3, xmm1                // xmm3 = v2

  // shuffle members
  // becarefull at the order in the register (right to left )= w z x y )
  // 1st arg xmm = w z y x = 11 10 01 00
  // 2nd arg xmm = w z y x = 11 10 01 00
  // 3rd arg where to place members of 1st arg in 2nd arg
  // becarefull reading from right to left
  shufps xmm0, xmm0, $C9  //w z y x -> $c9 = 11 00 10 01 -> w x z y

  shufps xmm1, xmm1, $D2  //w z y x -> $d2 = 11 01 00 10 -> w y x z
  shufps xmm2, xmm2, $D2
  shufps xmm3, xmm3, $C9
  // scale
  mulps  xmm0, xmm1
  mulps  xmm2, xmm3
  // sub the results
  subps  xmm0, xmm2
  andps  xmm0, [cSSE_MASK_NO_W]
  addps  xmm0, [cWOnevector4f] // it would better change by logical operator
  movups [EAX], xmm0
end;

procedure TBZVector4f.pNormalize;  assembler; nostackframe; register;
asm
  movups xmm0, [EAX]
  movaps xmm3, xmm0
  andps  xmm3, [cSSE_MASK_ONLY_W]
  andps  xmm0, [cSSE_MASK_NO_W]
  movaps xmm2, xmm0
  {$IFDEF USE_ASM_SSE_3}
    mulps  xmm0, xmm0
    haddps xmm0, xmm0
    haddps xmm0, xmm0
  {$ELSE}
    mulps  xmm0, xmm0
    movaps xmm1, xmm0
    shufps xmm0, xmm1, $4e
    addps  xmm0, xmm1
    movaps xmm1, xmm0
    shufps xmm1, xmm1, $11
    addps  xmm0, xmm1
  {$ENDIF}
  {$IFDEF USE_ASM_SIMD_HIGHPRECISION}
    // High Precision
    sqrtps xmm0, xmm0
    divps  xmm2, xmm0
  {$ELSE}
    // Low precision
    rsqrtps xmm0, xmm0       //approximate reciprocal
    mulps   xmm2, xmm0
  {$ENDIF}
  addps   xmm2, xmm3
  movups [EAX], xmm2
end;

procedure TBZVector4f.pMin(constref B: TBZVector4f); assembler; nostackframe; register;
asm
  movups  xmm0, [EAX]
  movups  xmm1, [B]
  minps   xmm0, xmm1
  movups [EAX], xmm0
End;

procedure TBZVector4f.pMin(constref B: Single); assembler; nostackframe; register;
asm
  movups  xmm0, [EAX]
  movlps  xmm1, [B]
  shufps  xmm1, xmm1, $00
  minps   xmm0, xmm1
  movups [EAX], xmm0
End;

procedure TBZVector4f.pMax(constref B: TBZVector4f); assembler; nostackframe; register;
asm
  movups  xmm0, [EAX]
  movups  xmm1, [B]
  maxps   xmm0, xmm1
  movups [EAX], xmm0
End;

procedure TBZVector4f.pMax(constref B: Single); assembler; nostackframe; register;
asm
  movups  xmm0, [EAX]
  movlps  xmm1, [B]
  shufps  xmm1, xmm1, $00
  maxps   xmm0, xmm1
  movups [EAX], xmm0
End;

procedure TBZVector4f.pClamp(Constref AMin, AMax: TBZVector4f); assembler; nostackframe; register;
asm
  movups  xmm0, [EAX]
  movups  xmm2, [AMin]
  movups  xmm3, [AMax]
  maxps   xmm0, xmm2
  minps   xmm0, xmm3
  movups [EAX], xmm0
End;

procedure TBZVector4f.pClamp(constref AMin, AMax: Single); assembler; nostackframe; register;
asm
  movups  xmm0, [EAX]
  movss   xmm2, [AMin]
  movss   xmm3, [AMax]
  shufps  xmm2, xmm2, $00 // Replicate AMin
  shufps  xmm3, xmm3, $00 // Replicate AMax
  maxps   xmm0, xmm2
  minps   xmm0, xmm3
  movups [EAX], xmm0
End;

procedure TBZVector4f.pMulAdd(Constref B, C: TBZVector4f); assembler; nostackframe; register; // (Self*B)+c
asm
  movups  xmm0, [EAX]
  movups  xmm1, [B]
  movups  xmm2, [C]
  mulps   xmm0, xmm1
  addps   xmm0, xmm2
  movups [EAX], xmm0
End;

procedure TBZVector4f.pMulDiv(Constref B, C: TBZVector4f); assembler; nostackframe; register; // (Self*B)-c
asm
  movups  xmm0, [EAX]
  movups  xmm1, [B]
  movups  xmm2, [C]
  mulps   xmm0, xmm1
  divps   xmm0, xmm2
  movups [EAX], xmm0
End; }

{%endregion%}
