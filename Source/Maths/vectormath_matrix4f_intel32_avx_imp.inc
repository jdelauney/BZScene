{%region%----[ Operators ]------------------------------------------------------}

{%region%----[ Operators ]------------------------------------------------------}

class operator TBZMatrix4f.+(constref A, B: TBZMatrix4f): TBZMatrix4f; assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

class operator TBZMatrix4f.+(constref A: TBZMatrix4f; constref B: Single): TBZMatrix4f; assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

class operator TBZMatrix4f.-(constref A, B: TBZMatrix4f): TBZMatrix4f; assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

class operator TBZMatrix4f.-(constref A: TBZMatrix4f; constref B: Single): TBZMatrix4f; assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

class operator TBZMatrix4f.*(constref A, B: TBZMatrix4f): TBZMatrix4f; assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

class operator TBZMatrix4f.*(constref A: TBZMatrix4f; constref B: Single): TBZMatrix4f; assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

class operator TBZMatrix4f.*(constref A: TBZMatrix4f; constref B: TBZVector4f): TBZVector4f; assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

class operator TBZMatrix4f./(constref A: TBZMatrix4f; constref B: Single): TBZMatrix4f; assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

class operator TBZMatrix4f.-(constref A: TBZMatrix4f): TBZMatrix4f; assembler; nostackframe; register;
asm
  vxorps xmm0, xmm0, xmm0
end;

{%endregion%}

{%region%----[ Functions ]------------------------------------------------------}


