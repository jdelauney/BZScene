{%region%----[ Pure Pascal will never Convert ]---------------------------------}
// Temp comment this region is where we keep pascal routines for which it does
// not make sense to use SSE/AVX


// Turn (Y axis)
function TBZMatrixHelper.Turn( Angle: Single): TBZMatrix;
var
  m : TBZMatrix;
begin
  m.CreateRotationMatrix(AffineVectorMake(Self.V[1].V[0], Self.V[1].V[1], Self.V[1].V[2]), Angle);
  Result:=Self * m;
end;

// Turn (direction)
function TBZMatrixHelper.Turn(constref MasterUp: TBZVector; Angle: Single): TBZMatrix;
var
  m : TBZMatrix;
begin
  m.CreateRotationMatrix(MasterUp, Angle);
  Result:=Self * m;
end;

// Pitch (X axis)
function TBZMatrixHelper.Pitch(Angle: Single): TBZMatrix;
var
  m : TBZMatrix;
begin
  m.CreateRotationMatrix(AffineVectorMake(Self.V[0].V[0], Self.V[0].V[1], Self.V[0].V[2]), Angle);
  Result:=Self * m;
end;

// Pitch (direction)
function TBZMatrixHelper.Pitch(constref MasterRight: TBZVector; Angle: Single): TBZMatrix;
var
  m : TBZMatrix;
begin
  m.CreateRotationMatrix(MasterRight, Angle);
  Result := Self * m;
end;

// Roll (Z axis)
function TBZMatrixHelper.Roll(Angle: Single): TBZMatrix;
var
  m : TBZMatrix;
begin
  m.CreateRotationMatrix(AffineVectorMake(Self.V[2].V[0], Self.V[2].V[1], Self.V[2].V[2]), Angle);
  Result := Self * m;
end;

// Roll (direction)
function TBZMatrixHelper.Roll(constref MasterDirection: TBZVector; Angle: Single): TBZMatrix;
var
  m : TBZMatrix;
begin
  m.CreateRotationMatrix(MasterDirection, Angle);
  Result := Self * m;
end;

{%endregion}

{%region%----[ Pure Pascal may Convert at some point ]--------------------------}

{%endregion}

{$IFNDEF USE_ASM}

{%region----[ PASCAL VERSION OF ASSEMBLER ROUTINES BELOW ]----------------------}

{%region%----[ Operators ]------------------------------------------------------}

{%endregion%}

{%region%----[ Functions ]------------------------------------------------------}

{%endregion%}

{%endregion%}

{$ENDIF}
