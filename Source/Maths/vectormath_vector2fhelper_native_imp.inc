{%region%----[ Pure Pascal will never Convert ]---------------------------------}
// Temp comment this region is where we keep pascal routines for which it does
// not make sense to use SSE/AVX


// Swizzling

function TBZVector2fHelper.GetXY : TBZVector2f;
begin
   Result.Create(Self.X,Self.Y);
end;

function TBZVector2fHelper.GetYX : TBZVector2f;
begin
   Result.Create(Self.Y,Self.X);
end;

function TBZVector2fHelper.GetXX : TBZVector2f;
begin
   Result.Create(Self.X,Self.X);
end;

function TBZVector2fHelper.GetYY : TBZVector2f;
begin
   Result.Create(Self.Y,Self.Y);
end;


function TBZVector2fHelper.GetXXY : TBZVector4f;
begin
  Result.X := Self.X;
  Result.Y := Self.X;
  Result.Z := Self.Y;
  Result.W := 1;
end;

function TBZVector2fHelper.GetYYY : TBZVector4f;
begin
  Result.X := Self.Y;
  Result.Y := Self.Y;
  Result.Z := Self.Y;
  Result.W := 1;
end;

function TBZVector2fHelper.GetXXX : TBZVector4f;
begin
  Result.X := Self.X;
  Result.Y := Self.X;
  Result.Z := Self.X;
  Result.W := 1;
end;

function TBZVector2fHelper.GetYYX : TBZVector4f;
begin
  Result.X := Self.Y;
  Result.Y := Self.Y;
  Result.Z := Self.X;
  Result.W := 1;
end;

function TBZVector2fHelper.GetXYY : TBZVector4f;
begin
  Result.X := Self.X;
  Result.Y := Self.Y;
  Result.Z := Self.Y;
  Result.W := 1;
end;

function TBZVector2fHelper.GetYXY : TBZVector4f;
begin
  Result.X := Self.Y;
  Result.Y := Self.X;
  Result.Z := Self.Y;
  Result.W := 1;
end;

function TBZVector2fHelper.GetXYX : TBZVector4f;
begin
  Result.X := Self.X;
  Result.Y := Self.Y;
  Result.Z := Self.X;
  Result.W := 1;
end;

function TBZVector2fHelper.GetYXX : TBZVector4f;
begin
  Result.X := Self.Y;
  Result.Y := Self.X;
  Result.Z := Self.X;
  Result.W := 1;
end;

{%endregion}

{%region%----[ Pure Pascal may Convert at some point ]--------------------------}



{%endregion}

{$IFNDEF USE_ASM}

{%region----[ PASCAL VERSION OF ASSEMBLER ROUTINES BELOW ]----------------------}

{%region%----[ Functions ]------------------------------------------------------}

function TBZVector2fHelper.Step(ConstRef B : TBZVector2f):TBZVector2f;
begin
  result := Self;
  if self.X <= B.X then result.X := 0;
  if self.Y <= B.Y then result.Y := 0;
end;

//function TBZVector2fHelper.FaceForward(constref A, B: TBZVector4f): TBZVector4f;
//begin
//  if (B.DotProduct(A) < 0) then
//     Result := -Self
//   else
//     Result := Self;
//end;

function TBZVector2fHelper.Saturate : TBZVector2f;
begin
  Result := self;
  Result := Result.Max(NullVector2f);
  Result := Result.Min(OneVector2f);
end;

function TBZVector2fHelper.SmoothStep(ConstRef A,B : TBZVector2f): TBZVector2f;
var
  t: TBZVector2f;
begin
  t := (Self-a)/(b-a);
  t := t.Saturate;
  // t * t * (3.0 - (t * 2.0)); <--- no overload for single - vector
  result := t * t * -((t * 2.0) + -3);
end;

function TBZVector2fHelper.Lerp(Constref B: TBZVector2f; Constref T:Single): TBZVector2f;
begin
  result := Self + (b-self)*t;
end;

{%endregion%}



{%endregion%}

{$ENDIF}
