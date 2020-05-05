{%region%----[ Operators ]------------------------------------------------------}

{%region%----[ Operators ]------------------------------------------------------}

class operator TBZMatrix4f.+(constref A, B: TBZMatrix4): TBZMatrix4; assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

class operator TBZMatrix4f.+(constref A: TBZMatrix4; constref B: Single): TBZMatrix4; assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

class operator TBZMatrix4f.-(constref A, B: TBZMatrix4): TBZMatrix4; assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

class operator TBZMatrix4f.-(constref A: TBZMatrix4; constref B: Single): TBZMatrix4; assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

class operator TBZMatrix4f.*(constref A, B: TBZMatrix4): TBZMatrix4; assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

class operator TBZMatrix4f.*(constref A: TBZMatrix4; constref B: Single): TBZMatrix4; assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

class operator TBZMatrix4f.*(constref A: TBZMatrix4; constref B: TBZVector): TBZVector; assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

class operator TBZMatrix4f./(constref A: TBZMatrix4; constref B: Single): TBZMatrix4; assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

class operator TBZMatrix4f.-(constref A: TBZMatrix4): TBZMatrix4; assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

{%endregion%}

{%region%----[ Functions ]------------------------------------------------------}

function TBZMatrix4f.GetDeterminant: Single;assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

//function TBZMatrix4f.MatrixDetInternal(const a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single; assembler; nostackframe; register;
//asm
//  vxorps xmm0, xmm0, xmm0
//end;

function TBZMatrix4f.Transpose: TBZMatrix4;assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

function TBZMatrix4f.Invert : TBZMatrix4;assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

function TBZMatrix4f.Normalize : TBZMatrix4;assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

//procedure TBZMatrix4f.Adjoint;assembler; nostackframe; register;
//asm
//  vxorps xmm0, xmm0, xmm0
//end;
//procedure TBZMatrix4f.AnglePreservingMatrixInvert(constref mat : TBZMatrix4);assembler; nostackframe; register;
//asm
//  vxorps xmm0, xmm0, xmm0
//end;
//
//function TBZMatrix4f.Decompose(var Tran: TBZMatrixTransformations): Boolean;assembler; nostackframe; register;
//asm
//  vxorps xmm0, xmm0, xmm0
//end;

function TBZMatrix4f.Translate( constref v : TBZVector):TBZMatrix4;assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;
function TBZMatrix4f.Multiply(constref M2: TBZMatrix4):TBZMatrix4;assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

{%endregion%}

{%region%----[ Operations on Self ]---------------------------------------------}

//procedure TBZMatrix4f.Transpose_Scale_M33(constref src : TBZMatrix4; Constref ascale : Single); assembler; nostackframe; register;
//asm
//  vxorps xmm0, xmm0, xmm0
//end;
//
//procedure TBZMatrix4f.CreateLookAtMatrix(const eye, center, normUp: TBZVector);assembler; nostackframe; register;
//asm
//  vxorps xmm0, xmm0, xmm0
//end;
//procedure TBZMatrix4f.CreateMatrixFromFrustum(Left, Right, Bottom, Top, ZNear, ZFar: Single);assembler; nostackframe; register;
//asm
//  vxorps xmm0, xmm0, xmm0
//end;
//procedure TBZMatrix4f.CreatePerspectiveMatrix(FOV, Aspect, ZNear, ZFar: Single); assembler; nostackframe; register;
//asm
//  vxorps xmm0, xmm0, xmm0
//end;
//procedure TBZMatrix4f.CreateOrthoMatrix(Left, Right, Bottom, Top, ZNear, ZFar: Single); assembler; nostackframe; register;
//asm
//  vxorps xmm0, xmm0, xmm0
//end;
//procedure TBZMatrix4f.CreatePickMatrix(x, y, deltax, deltay: Single; const viewport: TBZVector4i); assembler; nostackframe; register;
//asm
//  vxorps xmm0, xmm0, xmm0
//end;
//
//
//procedure TBZMatrix4f.CreateParallelProjectionMatrix(const plane : TBZHmgPlane; const dir : TBZVector); assembler; nostackframe; register;
//asm
//  vxorps xmm0, xmm0, xmm0
//end;
//
//procedure TBZMatrix4f.CreateShadowMatrix(const planePoint, planeNormal, lightPos : TBZVector);assembler; nostackframe; register;
//asm
// vxorps xmm0, xmm0, xmm0
//end;
//
//
//procedure TBZMatrix4f.CreateReflectionMatrix(const planePoint, planeNormal : TBZVector);assembler; nostackframe; register;
//asm
//  vxorps xmm0, xmm0, xmm0
//end;


{%endregion%}


