{%region%----[ Pure Pascal will never Convert ]---------------------------------}
// Temp comment this region is where we keep pascal routines for which it does
// not make sense to use SSE/AVX

procedure TBZComplexVector.Create(ARealPart, AnImaginaryPart  : Double);
begin
  Self.ImaginaryPart := AnImaginaryPart;
  Self.RealPart := ARealPart;
end;

procedure TBZComplexVector.Create(AValue : TBZVector2d);
begin
  Self.ImaginaryPart := AValue.X;
  Self.RealPart := AValue.Y;
end;

function TBZComplexVector.ToString : String;
begin
   Result := '(Real: '+FloattoStrF(Self.RealPart,fffixed,5,5)+
            ' ,Imaginary: '+FloattoStrF(Self.ImaginaryPart,fffixed,5,5)+')';
End;


function TBZComplexVector.Conjugate : TBZComplexVector;
begin
  Result.ImaginaryPart := -Self.ImaginaryPart;
  Result.RealPart := Self.RealPart;
end;

{ if (rel = 0) and (img > 0) then
  result := pi / 2
else if (rel = 0) and (img < 0) then
  result := -pi / 2
else if (rel < 0) and (img >= 0) then
  result := ArcTan(img / rel) + pi
else if (rel < 0) and (img < 0) then
  result := ArcTan(img / rel) - pi
else if (rel > 0) then
  result := ArcTan(img / rel)
else if (rel = 0) and (img = 0) then
  raise Exception.create('undefined Arg (rel=0 and img=0)'); }
function TBZComplexVector.Phase : Double;
begin
  // Both zero
  if (Self.RealPart = 0) and (Self.ImaginaryPart = 0) then
  begin
    Result := 0;
    exit;
  end;

  // Non-zero case
  if abs(Self.RealPart) > abs(Self.ImaginaryPart) then
  begin
    Result := ArcTan(Self.ImaginaryPart / Self.RealPart); {-45 to 45 deg, 135 to -135 deg}
    if Self.RealPart < 0 then Result := Result + cPi;
  end else
  begin
    Result := cPiDiv2 - ArcTan(Self.RealPart / Self.ImaginaryPart); {45 to 135, -45 to -135}
    if Self.ImaginaryPart < 0 then Result := Result + cPi;
  end;
  if Result > cPi then Result := Result - c2Pi;
end;

function TBZComplexVector.Cosinus : TBZComplexVector;
begin
  Result.RealPart := Cos(Self.RealPart) * Cosh(Self.ImaginaryPart);
  Result.ImaginaryPart := -Sin(Self.RealPart) * Sinh(Self.ImaginaryPart);
end;

function TBZComplexVector.Sinus : TBZComplexVector;
begin
  Result.RealPart := Sin(Self.RealPart) * Cosh(Self.ImaginaryPart);
  Result.ImaginaryPart := Cos(Self.RealPart) * Sinh(Self.ImaginaryPart);
end;

function TBZComplexVector.Polynome(Degree : Integer) : TBZComplexVector;
var
  Index : Integer;
  Tmp, Tmp2 : TBZComplexVector;
begin
  Result := Self;
  Tmp := Self;
  for Index := 2 to Degree do
  begin
    Tmp2.RealPart := Result.RealPart * Tmp.RealPArt - Result.ImaginaryPart * Tmp.ImaginaryPart;
    Tmp2.ImaginaryPArt := Result.RealPart * Tmp.ImaginaryPart + Result.ImaginaryPart * Tmp.RealPart;
    Result := Tmp2;
  end;
end;

{%endregion}

{%region%----[ Pure Pascal may Convert at some point ]--------------------------}

class operator TBZComplexVector.*(constref A, B: TBZComplexVector): TBZComplexVector;
begin
  Result.ImaginaryPart := A.ImaginaryPart * B.RealPart + A.RealPart * B.ImaginaryPart;
  Result.RealPart := A.RealPart * B.RealPart - A.ImaginaryPart * B.ImaginaryPart;
end;

class operator TBZComplexVector./(constref A, B: TBZComplexVector): TBZComplexVector;
var
  c, d : TBZComplexVector;
begin
  c := B.Conjugate;
  d := c * B;
  Result := A * c;
  Result.ImaginaryPart := Result.ImaginaryPart / d.RealPart;
  Result.RealPart := Result.RealPart / d.RealPart;
end;

class operator TBZComplexVector.*(constref A : TBZComplexVector; constref B : Single): TBZComplexVector;
begin
  Result.ImaginaryPart := A.ImaginaryPart * B;
  Result.RealPart := A.RealPart * B;
end;

class operator TBZComplexVector.=(constref A, B: TBZComplexVector): Boolean;
begin
 result := ((A.RealPart = B.RealPart) And (A.ImaginaryPart = B.ImaginaryPart));
end;

class operator TBZComplexVector.<>(constref A, B: TBZComplexVector): Boolean;
begin
  result := ((A.RealPart <> B.RealPart) or (A.ImaginaryPart <> B.ImaginaryPart));
end;

{%endregion}

{$IFNDEF USE_ASM}

{%region----[ PASCAL VERSION OF ASSEMBLER ROUTINES BELOW ]----------------------}

{%region%----[ Operators ]------------------------------------------------------}

class operator TBZComplexVector.+(constref A, B: TBZComplexVector): TBZComplexVector;
begin
  Result.ImaginaryPart := A.ImaginaryPart + B.ImaginaryPart;
  Result.RealPart := A.RealPart + B.RealPart;
end;

class operator TBZComplexVector.-(constref A, B: TBZComplexVector): TBZComplexVector;
begin
  Result.ImaginaryPart := A.ImaginaryPart - B.ImaginaryPart;
  Result.RealPart := A.RealPart - B.RealPart;
end;

{%endregion%}

{%region%----[ Functions ]------------------------------------------------------}

function TBZComplexVector.Length:Double;
begin
  Result := System.Sqrt((Self.RealPart * Self.RealPart) + (Self.ImaginaryPart * Self.ImaginaryPart));
end;

{%endregion%}

{%endregion%}

{$ENDIF}
