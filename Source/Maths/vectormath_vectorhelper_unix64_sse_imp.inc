function TBZVectorHelper.PointProject(constref origin, direction : TBZVector) : Single; assembler; register; nostackframe;
asm
  movaps xmm0, [RDI]
  movaps xmm1, [Origin]
  movaps xmm2, [Direction]
  subps  xmm0, xmm1
  mulps  xmm0, xmm2
  andps  xmm0, [RIP+cSSE_MASK_NO_W]
  movaps xmm1, xmm0
  movaps xmm2, xmm0
  shufps xmm0, xmm0, 00000000b
  shufps xmm1, xmm1, 01010101b
  shufps xmm2, xmm2, 10101010b
  addss  xmm0, xmm1
  addss  xmm0, xmm2
end;

function TBZVectorHelper.AverageNormal4(constref up, left, down,right: TBZVector): TBZVector; assembler; register; nostackframe;
asm
  movaps xmm1, [RDI] // cen

  //VectorSubtract(up^,cen^,s{%H-});
  movaps xmm2, [up]   //s
  subps  xmm2, xmm1
  //VectorSubtract(left^,cen^,t{%H-});
  movaps xmm3, [left]  //t
  subps  xmm3, xmm1
  //VectorSubtract(down^,cen^,u{%H-});
  movaps xmm4, [down]   //u
  subps  xmm4, xmm1
  //VectorSubtract(right^,cen^,v{%H-});
  movaps xmm5, [right]  //v
  subps  xmm5, xmm1

  movaps xmm0, [RIP+cSSE_MASK_NO_W]
  andps  xmm2, xmm0
  andps  xmm3, xmm0
  andps  xmm4, xmm0
  andps  xmm5, xmm0
                                   // state for result
  //------------------------------------
  // X := s.Y*t.Z,    1*-0.34
  // Y := s.Z*t.X,    -0.34 * -1
  // Z := s.X*t.Y     0 * 0
  // S =   ,x,z,Y
  // T =  -,y,x,z
  movaps xmm6, xmm2
  shufps xmm6, xmm6, 11001001b
  movaps xmm7, xmm3
  shufps xmm7, xmm7, 11010010b
  mulps  xmm6, xmm7                  // s gone t in 7


  // X := s.Z*t.Y
  // Y := s.X*t.Z
  // Z := s.Y*t.X
  // S =   w,y,x,z
  // t = from -,y,x,z to -,x,z,y
  shufps xmm7, xmm7, 11010010b
  movaps xmm8, xmm2
  shufps xmm8, xmm8, 11010010b
  mulps  xmm7, xmm8

  subps  xmm6, xmm7
  movaps xmm0, xmm6
  //-------------------------------------
  // same again for t and u
  //------------------------------------
  // X := t.Y*u.Z,
  // Y := t.Z*u.X,
  // Z := t.X*u.Y
  // T =   w,z,y,x
  // U = * -,x,z,y
  movaps xmm6, xmm3
  shufps xmm6, xmm6, 11001001b
  movaps xmm7, xmm4
  shufps xmm7, xmm7, 11010010b
  mulps  xmm6, xmm7                  // s gone t in 7

  // X := t.Z*u.Y
  // Y := t.X*u.Z
  // Z := t.Y*u.X
  // T =   w,z,y,x
  // U = * -,y,x,z
  shufps xmm7, xmm7, 11010010b
  movaps xmm8, xmm3
  shufps xmm8, xmm8, 11010010b
  mulps  xmm7, xmm8

  subps  xmm6, xmm7
  addps  xmm0, xmm6

  //-------------------------------------
  // same again for u and v
  //------------------------------------
  // X := u.Y*v.Z,
  // Y := u.Z*v.X,
  // Z := u.X*v.Y
  // U =   w,z,y,x
  // V = * -,x,z,y
  movaps xmm6, xmm4
  shufps xmm6, xmm6, 11001001b
  movaps xmm7, xmm5
  shufps xmm7, xmm7, 11010010b
  mulps  xmm6, xmm7                  // s gone t in 7

  // X := u.Z*v.Y
  // Y := u.X*v.Z
  // Z := u.Y*v.X
  // U =   w,z,y,x
  // V = * -,y,x,z
  shufps xmm7, xmm7, 11010010b
  movaps xmm8, xmm4
  shufps xmm8, xmm8, 11010010b
  mulps  xmm7, xmm8

  subps  xmm6, xmm7
  addps  xmm0, xmm6

  //-------------------------------------
  // same again for v and s
  //------------------------------------
  // X := v.Y*s.Z,
  // Y := v.Z*s.X,
  // Z := v.X*s.Y
  // V =   w,z,y,x
  // S = * -,x,z,y
  movaps xmm6, xmm5
  shufps xmm6, xmm6, 11001001b
  movaps xmm7, xmm2
  shufps xmm7, xmm7, 11010010b
  mulps  xmm6, xmm7                  // s gone t in 7

  // X := v.Z*s.Y
  // Y := v.X*s.Z
  // Z := v.Y*s.X
  // V =   w,z,y,x
  // S = * -,y,x,z
  shufps xmm7, xmm7, 11010010b
  movaps xmm8, xmm5
  shufps xmm8, xmm8, 11010010b
  mulps  xmm7, xmm8

  subps  xmm6, xmm7
  addps  xmm0, xmm6

  //  xmm0        =      xmm6       +        xmm7         +         xmm8        +         xmm2
  //Result.X := (s.Y*t.Z - s.Z*t.Y) + (t.Y*u.Z - t.Z*u.Y) + (u.Y*v.Z - u.Z*v.Y) + (v.Y*s.Z - v.Z*s.Y);
  //Result.Y := (s.Z*t.X - s.X*t.Z) + (t.Z*u.X - t.x*u.Z) + (u.Z*v.X - u.X*v.Z) + (v.Z*s.X - v.X*s.Z);
  //Result.Z := (s.X*t.Y - s.Y*t.X) + (t.X*u.Y - t.Y*u.X) + (u.X*v.Y - u.Y*v.X) + (v.X*s.Y - v.Y*s.X);

  movaps xmm2, xmm0
  mulps  xmm0, xmm0
  movaps xmm1, xmm0
  shufps xmm0, xmm1, $4e
  addps  xmm0, xmm1
  movaps xmm1, xmm0
  shufps xmm1, xmm1, $11
  addps  xmm0, xmm1
  sqrtps xmm0, xmm0
  divps  xmm2, xmm0
  movaps xmm0, xmm2
  movhlps xmm1,xmm0
end;

function TBZVectorHelper.Step(ConstRef B : TBZVector4f):TBZVector4f;  assembler; register; nostackframe;
asm
  movaps   xmm1, [RDI]
  movaps   xmm0, xmm1
  cmpnleps xmm1, XMMWORD PTR [B] //xmm1
  andps    xmm1, XMMWORD PTR [RIP+cOneVector4f]
  mulps    xmm0, xmm1
  movhlps  xmm1, xmm0
end;

function TBZVectorHelper.FaceForward(constref A, B: TBZVector4f): TBZVector4f;assembler; register; nostackframe;
asm
  movaps   xmm0, [RDI]
  movaps   xmm1, [A]
  movaps   xmm2, [B]
  //andps    xmm2, [RIP+cSSE_MASK_NO_W]
  // Dotproduct(A, B)
  mulps  xmm2, xmm1
  {$IFDEF USE_ASM_SSE_3}
     movshdup    xmm1, xmm2
     addps       xmm2, xmm1
     movhlps     xmm1, xmm2
     addss       xmm2, xmm1
  {$ELSE}
     movhlps xmm1, xmm2
     addps xmm1, xmm2
     shufps xmm2, xmm2, 00000001b
     addps xmm2, xmm1
  {$ENDIF}
  shufps xmm2, xmm2, $00 // Broadcast
  // get sign of xmm2
  movaps xmm3, xmm2
  movaps xmm5, [RIP+cOneMinusVector4f]
  movaps xmm4, [RIP+cOneVector4f]

  xorps xmm1, xmm1   // reset to 0
  cmpltps  xmm2, xmm1 // < 0 ???
  cmpnltps xmm3, xmm1 // > 0 ???

  andps xmm2, xmm5
  andps xmm3, xmm4
  orps  xmm2, xmm3   // xmm2 contain signs -1.0 or 1.0
  //---------------
  //mulps xmm2, xmm5
  mulps xmm0, xmm2  // Flip sign >= 0
  movhlps  xmm1, xmm0
end;


function TBZVectorHelper.Saturate : TBZVector4f; assembler; register; nostackframe;
asm
  movaps  xmm0, [RDI]
  minps   xmm0, [RIP+cOneVector4f]
  maxps   xmm0, [RIP+cNullVector4f]
  movhlps xmm1, xmm0
end;

function TBZVectorHelper.SmoothStep(ConstRef A,B : TBZVector4f): TBZVector4f;Assembler; register; nostackframe;
Const
  cVecSet3 : TBZVector4f = (x:3.0;y:3.0;z:3.0;w:3.0);
asm
  movaps xmm0, [RDI]
  //Saturate((self-a)/(b-a))
  movaps  xmm1, XMMWORD PTR [A]
  subps   xmm0, xmm1
  movaps  xmm2, XMMWORD PTR [B]
  subps   xmm2, xmm1
  divps   xmm0, xmm2
  minps   xmm0, XMMWORD PTR [RIP+cOneVector4f]
  maxps   xmm0, XMMWORD PTR [RIP+cNullVector4f]

  //result :=t*t * ((3.0 - t*2.0))
  movaps  xmm1, xmm0

  mulps   xmm0, xmm0
  movaps  xmm2, XMMWORD PTR [RIP+cVecSet3]
  addps   xmm1, xmm1            //t*2
  subps   xmm2, xmm1
  mulps   xmm0, xmm2
  movhlps xmm1, xmm0
end;

function TBZVectorHelper.Reflect(ConstRef N: TBZVector4f): TBZVector4f; assembler; register; nostackframe;
asm
  movaps   xmm1, [RDI]
  movaps   xmm3, XMMWORD PTR [N]
  movaps   xmm2, xmm3
  movaps   xmm0, xmm1

  // Dot(N, I)
  andps xmm1, XMMWORD PTR [RIP+cSSE_MASK_NO_W]
  mulps xmm3, xmm1
  movshdup    xmm1, xmm3
  addps       xmm3, xmm1
  movhlps     xmm1, xmm3
  addss       xmm3, xmm1

  // (2 * Dot(N, I)) * N
  addss  xmm3, xmm3
  mulps  xmm3, xmm2

  // I - ((2 * Dot(N, I)) * N)
  subps   xmm0, xmm3
  movhlps xmm1, xmm0
end;
