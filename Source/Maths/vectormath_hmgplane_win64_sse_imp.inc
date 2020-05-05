procedure TBZHmgPlane.Create(constref p1, p2, p3 : TBZVector);assembler; register; nostackframe;
asm
  //A := p1y * (p2Z - p3Z) + p2Y * (p3Z - p1Z) + p3Y * (p1Z - p2Z);
  //B := p1Z * (p2X - p3X) + p2Z * (p3X - p1X) + p3Z * (p1X - p2X);
  //C := p1X * (p2Y - p3Y) + p2X * (p3Y - p1y) + p3X * (p1y - p2Y);
  //     NT1     T1(2-3)     NT2     T1(3-1)     NT3     T1(1-2)   NT order currently |-|B|A|C|
  // shuffle Z -> X  Y -> Z   X ->Y    NT = |W|Z|Y|X|  T1 = |W|X|Z|Y| = 11001001b
  // keep  p1 p2 p3 for later

  movaps xmm0, [p1]
  movaps xmm1, [p2]
  movaps xmm2, [p3]

  movaps xmm4, xmm0
  movaps xmm5, xmm2
  movaps xmm6, xmm1

  shufps xmm4, xmm4, 11001001b   //|p1 T1|
  shufps xmm5, xmm5, 11001001b   //|p3 T1|
  shufps xmm6, xmm6, 11001001b   //|p2 T1|

  movaps xmm3, xmm6
  subps  xmm3, xmm5              //|T1(2-3)|
  subps  xmm5, xmm4              //|T1(3-1)|
  subps  xmm4, xmm6              //|T1(1-2)|

  movaps xmm6, xmm0              // stash p1 for later

  mulps  xmm0, xmm3              // NT1*(T1(2-3))
  mulps  xmm1, xmm5              // NT2*(T1(3-1))
  mulps  xmm2, xmm4              // NT3*(T1(1-2))
  addps  xmm1, xmm0              // NT1*(T1(2-3)) + NT2*(T1(3-1))
  addps  xmm1, xmm2              // NT1*(T1(2-3)) + NT2*(T1(3-1)) + NT3*(T1(1-2))
                                 // xmm1 now holds |-|B|A|C|
  movaps xmm0, xmm1              // stash for result

  {$ifdef USE_ASM_SSE_4}
    dpps     xmm1, xmm1, 01111111b  // result in all bytes
    sqrtps   xmm1, xmm1
  {$else}
    mulps    xmm1, xmm1
    {$ifdef USE_ASM_SSE_3}
      //movhlps xmm2, xmm1
      //haddps  xmm1, xmm1
      //addss   xmm1, xmm2
      movshdup xmm2,xmm1
      addps  xmm1,xmm2
      movhlps xmm2,xmm1
      addss  xmm1,xmm2
    {$else}
      movhlps xmm2, xmm1
      addss   xmm2, xmm1
      shufps  xmm1, xmm1, 01010101b
      addss   xmm1, xmm2
    {$endif}
    sqrtss  xmm1, xmm1
    shufps  xmm1, xmm1, 00000000b // fill all byte with result.
  {$endif}
    divps   xmm0, xmm1            // xmm0 now holds (|-|B|A|C|)unitvector
    shufps  xmm0, xmm0, 11001001b // xmm0 now holds (|-|C|B|A|)unitvector

  // now a choice of two
  // D := -(p1X * (p2Y * p3Z - p3Y * p2Z) + p2X * (p3Y * p1Z - p1y * p3Z) + p3X * (p1y * p2Z - p2Y * p1Z)
  // or
  // D := -(p1.DotProduct(Self))  <-------

  {$ifdef USE_ASM_SSE_4}
    dpps     xmm6, xmm0, 01110001b
  {$else}
    mulps    xmm6, xmm0
    {$ifdef USE_ASM_SSE_3}
      //movhlps xmm2, xmm6
      //haddps  xmm6, xmm6
      //addss   xmm6, xmm2

      movshdup xmm2,xmm6
      addps  xmm6,xmm2
      movhlps xmm2,xmm6
      addss  xmm6,xmm2
    {$else}
      movhlps xmm2, xmm6
      addss   xmm2, xmm6
      shufps  xmm6, xmm6, 01010101b
      addss   xmm6, xmm2
    {$endif}
  {$endif}

    xorps   xmm6, [RIP+cSSE_MASK_NEGATE] // xmm6 now holds |-|-|-|D|
    shufps  xmm6, xmm0, 10100000b        // xmm6 now holds |C|C|D|D|
    shufps  xmm6, xmm6, 00000011b        // xmm6 now holds |D|D|D|C|
    movlhps xmm0, xmm6
    movaps [RCX], xmm0                      //SF 7.14

end;


function TBZHmgPlane.Normalized:TBZHmgPlane;assembler; register; nostackframe;
//   n:=1/Sqrt(Self.X*Self.X+Self.Y*Self.Y+Self.Z*Self.Z);
//   Result :=Self * n; have to do the lot as dist was wrongly calced when created.
asm
  movaps   xmm0, [RCX]
  movaps   xmm2, xmm0
{$ifdef USE_ASM_SSE_4}
  dpps     xmm2, xmm2, 01110111b
{$else}
  mulps    xmm2, xmm2
  {$ifdef USE_ASM_SSE_3}
    //movhlps xmm3, xmm2
    //haddps  xmm2, xmm2
    //addss   xmm2, xmm3
    movhlps xmm3, xmm2
    movaps xmm5, xmm2
    andps xmm5, [RIP+cSSE_MASK_NO_W]
    movshdup xmm4,xmm5
    addps  xmm2,xmm4
    addss   xmm2, xmm3 // x + w
  {$else}
    movhlps xmm1, xmm2
    addss   xmm1, xmm2
    shufps  xmm2, xmm2, 01010101b
    addss   xmm2, xmm1
  {$endif}
{$endif}
  sqrtss  xmm2, xmm2
  shufps  xmm2, xmm2, 00000000b
  divps   xmm0, xmm2
  movaps [RESULT], xmm0
end;

procedure TBZHmgPlane.Normalize; assembler; register; nostackframe;
//   n:=1/Sqrt(Self.X*Self.X+Self.Y*Self.Y+Self.Z*Self.Z);
//   Result := Self * n;  have to do the lot as dist was wrongly calced when created.
asm
  movaps   xmm0, [RCX]
  movaps   xmm2, xmm0
  {$ifdef USE_ASM_SSE_4}
  dpps     xmm2, xmm2, 01110111b
  {$else}
  mulps    xmm2, xmm2
  {$ifdef USE_ASM_SSE_3}
    movhlps xmm3, xmm2  //zwzw
   // haddps  xmm2, xmm2 //x+y+z
    movaps xmm5, xmm2
    andps xmm5, [RIP+cSSE_MASK_NO_W]
    movshdup xmm4,xmm5
    addps  xmm2,xmm4

    addss   xmm2, xmm3 // x + w


  {$else}
    movhlps xmm1, xmm2
    addss   xmm1, xmm2
    shufps  xmm2, xmm2, 01010101b
    addss   xmm2, xmm1
  {$endif}
  {$endif}
  sqrtss  xmm2, xmm2
  shufps  xmm2, xmm2, 00000000b
  divps   xmm0, xmm2
  movaps [RCX], xmm0
end;

function TBZHmgPlane.Distance(constref point : TBZVector) : Single; assembler; register; nostackframe;
asm
{.$ifdef TEST}
  movaps xmm0, [RCX]     // xmm0 = Plane->d | Plane->c | Plane->b | Plane->a
  movaps xmm1, [POINT]   // xmm1 = ???????? | Point->z | Point->y | Point->x

  //andps xmm1, [RIP+cSSE_MASK_NO_W] // xmm1 = 0 | ...
  {$ifdef USE_ASM_SSE_4}
    dpps    xmm0, xmm1, 01110111b    //12% slower (6% slower with while..do loop)
    addss   xmm0, [RCX]12
  {$else}
    {$ifdef USE_ASM_SSE_3}  // (1,2%<>4.800% fastest with while..do loop)
      movss xmm4, [RCX]12
      //movaps xmm4, xmm0 WHY WE MUST USE THIS INSTRUCTION BEFORE SHUFFLE ?????????
      //shufps xmm4, xmm0, $FF
      mulps xmm0, xmm1
      movshdup xmm2,xmm0
      movhlps xmm3, xmm0
      addps  xmm0, xmm2    //x+y
      addps  xmm0,xmm3     //(x+y)+z
      addss  xmm0, xmm4    // (x+y+z) + plane.w
    {$else}
      movss xmm0, [RCX]           // fastest 0-4% slower --> +4,249% with while..do loop
      mulss xmm0, [POINT]
      movss xmm1, [RCX]4
      mulss xmm1, [POINT]4
      addss xmm0, xmm1
      movss xmm2, [RCX]8
      mulss xmm2, [POINT]8
      addss xmm0, [RCX]12
      addss xmm0, xmm2
    {$endif}
  {$endif}
end;

function TBZHmgPlane.AbsDistance(constref point : TBZVector) : Single; assembler; register; nostackframe;
asm
  movaps xmm0, [RCX]     // xmm0 = Plane->d | Plane->c | Plane->b | Plane->a
  movaps xmm1, [POINT]   // xmm1 = ???????? | Point->z | Point->y | Point->x


  andps xmm1, [RIP+cSSE_MASK_NO_W] // xmm1 = 0 | ...
  movaps xmm7, xmm0                // xmm7 = Plane...

  mulps xmm0, xmm1        // xmm1 = d*0.0 | c*z | b*y | a*x
  shufps xmm7, xmm7, $FF  // xmm7 = d | d | d | d

  {$ifdef USE_ASM_SSE_3}
    movshdup xmm1,xmm0
    addps  xmm0,xmm1
    movhlps xmm1,xmm0
    addss  xmm0,xmm1
    //haddps xmm0, xmm0               // xmm1 = ... | ... | z*c + 0 | x*a + y*b
    //haddps xmm0, xmm0               // xmm1 = ... | ... | dot(Plane, Point)
    addss xmm0, xmm7                // xmm1 = ? | ? | ? | fabsf(dot(Plane, Point)) + Plane->d
    andps xmm0, [RIP+cSSE_MASK_ABS] // xmm1 = ??? | ??? | ??? | abs(dot(Plane, Point))
  {$else}
    movhlps xmm2, xmm0              // xmm2 = ???? | ???? | d*0.0 | z*c */
    addss xmm2, xmm0                // xmm2 = ???? | ???? | ????  | x*a + z*c*/
    shufps xmm0, xmm0, $55          // xmm1 = ???? | ???? | ????  | y*b */
    addss xmm0, xmm2
    addss xmm0, xmm7                // xmm1 = + Plane.W
    andps xmm0, [RIP+cSSE_MASK_ABS] // xmm1 = abs(dot(Plane, Point))
  {$endif}
end;

function TBZHmgPlane.Perpendicular(constref P : TBZVector4f) : TBZVector4f; assembler; nostackframe; register;
asm
  movaps xmm0, [RCX]
  movaps xmm1, [P]
//  andps  xmm1, [RIP+cSSE_MASK_NO_W]  // use this to zero out final W if required
  movaps xmm2, xmm0
  {$IFDEF USE_ASM_SSE_4}
     dpps xmm0, xmm1, 01110111b // |dot|dot|dot|0|
  {$ELSE}
    {$IFDEF USE_ASM_SSE_3}
      mulps   xmm0, xmm1
      movhlps xmm3, xmm0
      haddps  xmm0, xmm0
      addss   xmm0, xmm3
    {$ELSE}
      mulps   xmm0, xmm1             //   xmm0 =  w | z  | y  | x  |
      movhlps xmm3, xmm0             //   xmm1 =  - | -  | w  | z  |
      addps   xmm3, xmm0             // x + z
      shufps  xmm0, xmm0, 00000001b  //   xmm1 =  - | -  | -  | y  |
      addps   xmm0, xmm3             // (x + z ) + y
      andps   xmm0, [RIP+cSSE_MASK_NO_W]   // only SSE2 needs one of these.
    {$ENDIF}
    shufps  xmm0, xmm0, 11000000b    // SSE4 does not need this.
  {$ENDIF}
  mulps   xmm0, xmm2                 // (Dot * Self)
  subps   xmm1, xmm0                 // Self - (Dot * Self) xxx should be P-Dot*Self
  movaps  xmm0, xmm1
 // movhlps xmm1, xmm0
  movaps [RESULT], xmm0
end;

function TBZHmgPlane.Reflect(constref V: TBZVector4f): TBZVector4f; assembler; nostackframe; register;
asm
  movaps xmm0,[V]
  movaps xmm1, [RCX]
  movaps xmm2, xmm1
  movaps xmm3, xmm0
  // Self.Dot(V) Result in xmm1
  {$IFDEF USE_ASM_SSE_4}
     dpps xmm1, xmm0, 01110001b //or $F1
  {$ELSE}
    {$IFDEF USE_ASM_SSE_3}
    {$ifdef CPU64}
      andps xmm0, [RIP+cSSE_MASK_NO_W]
    {$else}
      andps xmm0, [cSSE_MASK_NO_W]
    {$endif}
      mulps xmm1, xmm0
      haddps xmm1, xmm1
      haddps xmm1, xmm1
    {$ELSE}
      andps xmm0, [RIP+cSSE_MASK_NO_W]
      mulps  xmm1, xmm0             //   xmm0 =  w | z  | y  | x  |
      movhlps xmm0, xmm1            //   xmm1 =  - | -  | w  | z  |
      addps xmm0, xmm1              // x + z
      shufps xmm1, xmm1, 00000001b  //   xmm1 =  - | -  | -  | y  |
      addps xmm1, xmm0              // (x + z ) + y
    {$ENDIF}
  {$ENDIF}
  Shufps xmm1,xmm1, $0
  addps xmm1,xmm1  // (2 * N.Dot(Self))
  mulps xmm1, xmm2 // ((2 * N.Dot(Self)) * N)
  subps xmm3, xmm1 // Self - ((2 * N.Dot(Self)) * N)
  movaps [RESULT], xmm3
end;

