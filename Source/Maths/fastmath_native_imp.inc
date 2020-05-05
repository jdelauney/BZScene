
{%region%-----[ Fast Trigonometric Functions ]----------------------------------}

(* Optimization technique

2 divides at once
Suppose you want to compute:
	a/b    AND    c/d
In the real number system, this is the same as:
	ad/bd    AND    cb/bd
In case this isn't getting through, that's:
	inv := 1 / (b * d)
	inv * a * d    AND    inv * b * d

On perd 1.5bits de precision mais on passe de environ 20 cycle à 10

*)

var
  _SinLUT : array of double;
  _CosLUT : array of double;
  _TanLUT : array of double;
  _sincoslookupBits : Integer;
  _sincoslookupTableSize : Integer;
  _sincoslookupTableSizeMinus1 : Integer;
  _sincoslookupTableSizeWithMargin : Integer;
  _sincospi2OverLookupSize : Single;
  _sincoslookupSizeOverPi2  : Single;

procedure _InitSinCosTanLUT;
var
  i:integer;
  d:double;
begin
  _sincoslookupBits := 16;  // 16bits accuracy
  _sincoslookupTableSize := 1 shl _sincoslookupBits;
  _sincoslookupTableSizeMinus1 := _sincoslookupTableSize - 1;
  _sincoslookupTableSizeWithMargin := _sincoslookupTableSize + 1;
  _sincospi2OverLookupSize := _c2PI / _sincoslookupTableSize;
  _sincoslookupSizeOverPi2  := _sincoslookupTableSize / _c2PI;
  setlength(_sinLUT,_sincoslookupTableSizeWithMargin);
  setlength(_cosLUT,_sincoslookupTableSizeWithMargin);
  setlength(_tanLUT,_sincoslookupTableSizeWithMargin);
  for i:=0 to _sincoslookupTableSizeMinus1 do
  begin
    d := i * _sincospi2OverLookupSize;
    _SinLUT[i]:=System.Sin(d);
    _CosLUT[i]:=System.Cos(d);
    _TanLUT[i]:=Math.Tan(d);
  end;
end;

procedure _DoneSinCosTanLUT;
begin
  setLength(_SinLUT,0);
  _SinLUT := nil;
  setLength(_CosLUT,0);
  _CosLUT := nil;
  setLength(_TanLUT,0);
  _TanLUT := nil;
end;

function FastSinLUT(x:Single):Single; //x in radian
var
  Index, Alpha, Sin1, Sin2 : Single;
  i, ii : Integer;
begin
  index := x * _sincoslookupSizeOverPi2;
  ii := Round(index);
  alpha := index - ii;
  i := ii and _sincoslookupTableSizeMinus1;
  sin1 := _SinLUT[i];
  sin2 := _SinLUT[i + 1];
  // Do a lerp for better accuracy
  result := Sin1+alpha*(sin2-sin1); //sin1 * (1.0 - alpha) + sin2 * alpha;
end;

function FastCosLUT(x:Single):Single;
var
  Index, Alpha, Sin1, Sin2 : Single;
  i, ii : Integer;
begin
 // x := x +_c2PI;
  index := x * _sincoslookupSizeOverPi2;
  ii := Round(index);
  alpha := index - ii;
  i := ii and _sincoslookupTableSizeMinus1;
  sin1 := _CosLUT[i];
  sin2 := _CosLUT[i + 1];
  result := Sin1+alpha*(sin2-sin1); //sin1 * (1.0 - alpha) + sin2 * alpha;
end;

function FastTanLUT(x:Single):Single;
var
  Index, Alpha, Sin1, Sin2 : Single;
  i, ii : Integer;
begin
 // x := x +_c2PI;
  index := x * _sincoslookupSizeOverPi2;
  ii := Round(index);
  alpha := index - ii;
  i := ii and _sincoslookupTableSizeMinus1;
  sin1 := _TanLUT[i];
  sin2 := _TanLUT[i + 1];
  result := Sin1+alpha*(sin2-sin1); //sin1 * (1.0 - alpha) + sin2 * alpha;
end;

function FastTan(x:Single):Single;
//Var
//  xx:Single;
begin
 //if (x = _cPIdiv2) then Exit(0);
 //xx:=x*x;
 //Result := x + (xx * x * 0.3333333333)   + (xx * xx * x * 0.1333333333333) + (xx * xx * xx * x * 0.0539682539);
  Result := FastTanLut(x);
end;

function FastArcTan(x:Single):Single;
Const
  _A = 0.0776509570923569;
  _B = -0.287434475393028;
var
  xx: Double;
  //a,b : single;
begin
  xx := x * x;
  // Very fast approximation
  result := ((_A*xx + _B)* xx + (_cPIDiv4 - _A - _B )*x);
  // More accurate approximation
  //a := xx*x;
  //Result := x - (a * 0.333333333333);
  //b:= a*xx; // xx * xx*x
  //Result := Result + (b * 0.2);
  //b:=b*xx;  // xx * xx*xx*x
  //Result := Result - (b * 0.1428571429);
  //b:=b*xx;  // xx * xx*xx*xx*x
  //Result := Result + (b * 0.111111111111);
  //b:=b*xx;
  //Result := Result - (b * 0.0909090909);
end;

Function FastArcTan2(y, x: Single): Single; // accuracy of about 0.07 rads
Var
  abs_y: Single;
Begin
  abs_y := FastAbs(y) + _cEpsilon;      // prevent 0/0 condition
  If y < 0 Then
  Begin
    If x >= 0 Then
      Result := _cPIdiv4 * (x - abs_y) / (x + abs_y) - _cPIdiv4
    Else
      Result := _cPIdiv4 * (x + abs_y) / (abs_y - x) - _c3PIdiv4;
  End
  Else
  Begin
    If x >= 0 Then
      Result := _cPIdiv4 - _cPIdiv4 * (x - abs_y) / (x + abs_y)
    Else
      Result := _c3PIdiv4 - _cPIdiv4 * (x + abs_y) / (abs_y - x);
  End;
End;

(* Function atan2(y, x: Single): Single;
Begin
  If x > 0 Then
    Result := arctan(y / x)
  Else
  If x < 0 Then
    Result := arctan(y / x) + PI
  Else
    Result := cPIdiv2 * sign(y);
End;  *)

Function FastArcSine(Const x: Single): Single;
Begin
  Result := FastArcTan2(X, FastSqrt(1 - (X*X)));
End;

Function FastArcCosine(const X: Single): Single;
var ret, t:single;
begin
  // cubic approximation, the Lagrange polynomial for x  {-1, -½, 0, ½, 1},
  // not enough accurate in some case
  //result:= (-0.69813170079773212 * x * x - 0.87266462599716477) * x + 1.5707963267948966;

  t:=x;
  // From NVidia accuracy 0.0001
   ret := -0.0187293 * x;
   ret := ret + 0.0742610;
   ret := ret * x;
   ret := ret - 0.2121144;
   ret := ret * x;
   ret := ret + 1.5707288;
   ret := ret * System.Sqrt(1.0-x); // Do not use FastSqrt here
   ret := ret - 2 * t * ret;
   result := t * _cPI + ret;
end;

Function FastSinc(x: Single): Single;  //Source ImageMagick

  //  Max. abs. rel. error 1.2e-12 < 1/2^39.
Const
  c0: Double = 0.173611111110910715186413700076827593074e-2;
  c1: Double = -0.289105544717893415815859968653611245425e-3;
  c2: Double = 0.206952161241815727624413291940849294025e-4;
  c3: Double = -0.834446180169727178193268528095341741698e-6;
  c4: Double = 0.207010104171026718629622453275917944941e-7;
  c5: Double = -0.319724784938507108101517564300855542655e-9;
  c6: Double = 0.288101675249103266147006509214934493930e-11;
  c7: Double = -0.118218971804934245819960233886876537953e-13;
  d0: Double = 1.0;
  d1: Double = 0.547981619622284827495856984100563583948e-1;
  d2: Double = 0.134226268835357312626304688047086921806e-2;
  d3: Double = 0.178994697503371051002463656833597608689e-4;
  d4: Double = 0.114633394140438168641246022557689759090e-6;
Var
  xx, p, q: Double;
Begin
{
  Approximations of the sinc function sin(pi x)/(pi x) over the interval
  [-4,4] constructed by Nicolas Robidoux and Chantal Racette with funding
  from the Natural Sciences and Engineering Research Council of Canada.

  Although the approximations are polynomials (for low order of
  approximation) and quotients of polynomials (for higher order of
  approximation) and consequently are similar in form to Taylor polynomials /
  Pade approximants, the approximations are computed with a completely
  different technique.

  Summary: These approximations are "the best" in terms of bang (accuracy)
  for the buck (flops). More specifically: Among the polynomial quotients
  that can be computed using a fixed number of flops (with a given "+ - * /
  budget"), the chosen polynomial quotient is the one closest to the
  approximated function with respect to maximum absolute relative error over
  the given interval.

  The Remez algorithm, as implemented in the boost library's minimax package,
  is the key to the construction: http://www.boost.org/doc/libs/1_36_0/libs/
  math/doc/sf_and_dist/html/math_toolkit/backgrounders/remez.html

  If outside of the interval of approximation, use the standard trig formula.
}
  If x > 4.0 Then
  Begin
    p := _cPI * x;
    Result := sin(p) / p;
  End
  Else
  Begin
    xx := x;
    p := c0 + xx * (c1 + xx * (c2 + xx * (c3 + xx * (c4 + xx * (c5 + xx * (c6 + xx * c7))))));
    q := d0 + xx * (d1 + xx * (d2 + xx * (d3 + xx * d4)));
    Result := ((xx - 1.0) * (xx - 4.0) * (xx - 9.0) * (xx - 16.0) / q * p);
  End;
End;

Function FastInvSqrt(Const Value: Single): Single; Inline;
Var
  IntCast: Cardinal absolute Result;
Begin
  Result := Value;
  IntCast := ($BE6EB50C - IntCast) Shr 1;
  Result := 0.5 * Result * (3 - Value * (Result*Result));
End;

Function FastSqrt(Const Value: Single): Single; assembler; nostackframe; register;
asm
  //movaps   xmm0, [Value]
  sqrtps xmm0, xmm0
  //rsqrtps xmm1, xmm0;
  //mulps xmm0, xmm1;
end;

// Ne fonctionne pas en 32 bits
//Function FastSqrt(Const Value: Single): Single; Inline;
//Var
//  IntCast: Cardinal; // absolute Result;
//Begin
//  IntCast := Round(Value);
//  IntCast := ($5f3759DF - (IntCast Shr 1));
//  Result := IntCast * (3 - (Value * (IntCast*IntCast)));   // Double Newton Step
//End;




Function FastExp(x:Single):Single;
Const
 cExp_cst1 : Single = 2139095040.0;
 cExp_cst2 : Single = 0.0;
var
  i1,i2: Int64;
  val2, val3, val4 : Single;
  val4i : Int64;
begin
  val2 := 12102203.1615614*x+1065353216.0;
  if val2 < cExp_cst1 then val3 :=  val2 else val3 := cExp_cst1;
  if val3 > cExp_cst2 then val4 :=  val3 else val4 := cExp_cst2;
  val4i:= Round(val4);
  i1 := val4i and $7F800000;
  i2 := (val4i and $7FFFFF) or $3F800000;

  (* Generated in Sollya with:
     > f=remez(1-x*exp(-(x-1)*log(2)),
               [|(x-1)*(x-2), (x-1)*(x-2)*x, (x-1)*(x-2)*x*x|],
               [1.000001,1.999999], exp(-(x-1)*log(2)));
     > plot(exp((x-1)*log(2))/(f+x)-1, [1,2]);
     > f+x;
  *)
  Result :=
    i1 * (0.509871020343597804469416 + i2 *
            (0.312146713032169896138863 + i2 *
             (0.166617139319965966118107 + i2 *
              (-2.19061993049215080032874e-3 + i2 *
               1.3555747234758484073940937e-2))));

//return (362880+x*(362880+x*(181440+x*(60480+x*(15120+x*(3024+x*(504+x*(72+x*(9+x)))))))))*2.75573192e-6;
end;

Function FastLog(x:Single):Single; Inline;
var
  i : Int64;
  //f,
  aExp, addcst, xx : single;
begin
  //f := x;
  i := Round(x);
  AExp:= i shr 23;
  // 89.970756366 = 127 * log(2) - constant term of polynomial
  if x>0 then addcst := -89.970756366 else addCst := -_cInfinity;
  i := (i and $7FFFFF) or $3F800000;
  xx := i;


  (* Generated in Sollya using:
    > f = remez(log(x)-(x-1)*log(2),
            [|1,(x-1)*(x-2), (x-1)*(x-2)*x, (x-1)*(x-2)*x*x,
              (x-1)*(x-2)*x*x*x|], [1,2], 1, 1e-8);
    > plot(f+(x-1)*log(2)-log(x), [1,2]);
    > f+(x-1)*log(2)
 *)
  Result :=
    xx * (3.529304993 + xx * (-2.461222105 +
      xx * (1.130626167 + xx * (-0.288739945 +
        xx * 3.110401639e-2))))
    + (addcst + 0.69314718055995*AExp);
end;

Function FastLog2(X: Single): Single;
Begin
    {$HINTS OFF}
  // Result := Math.Log2(X);
  Result := FastLog(x) * 1.4426950408889634079;    // 1/ln(2)
    {$HINTS ON}
End;


Function FastLog10(X: Single): Single; //Inline;
  // Log10(X):=Log2(X) * Log10(2)
Begin
  //    Result := Math.Log10(X);
  Result := FastLog(x) * 0.4342944819;    // 1/ln(10)
End;

(* Function FastExp(Const X: Single): Single;
Var
  I, N: Integer;
  D:    Double;
Begin
  If (X = 1.0) Then
    Result := cEulerNumber
  Else
  If (x < 0) Then
    Result := 1.0 / Exp(-X)
  Else
  Begin
    N := 2;
    Result := 1.0 + X;
    Repeat
      D := X;
      For I := 2 To N Do
      Begin
        D := D * (X / I);
      End;

      Result := Result + D;
      Inc(N);
    Until (d <= cEpsilon);
  End;
End;  *)

Function FastLn(Const X: Single): Single;
Var
  Lo, Hi, Mid, Val: Single;
Begin
  If (X < 0) Then
  Begin
    Result := 0;
    Exit;
  End;

  // use recursion to get approx range
  If (X < 1) Then
  Begin
    Result := -FastLn(1 / X);
    Exit;
  End;

  If (X > _cEulerNumber) Then
  Begin
    Result := FastLn(X / _cEulerNumber) + 1;
    Exit;
  End;

  // X is now between 1 and e
  // Y is between 0 and 1
  lo := 0.0;
  hi := 1.0;

  While (True) Do
  Begin
    mid := (lo + hi) / 2;
    val := exp(mid);
    If (val > X) Then hi := mid;

    If (val < X) Then lo := mid;

    If (FastAbs(val - X) < _cEpsilon) Then
    Begin
      Result := mid;
      Exit;
    End;
  End;
End;

// ldexp() multiplies x by 2**n.
Function FastLDExp(x: Single; N: Integer): Single;
Var
  r: Single;
Begin
  R := 1;
  If N > 0 Then
  Begin
    While N > 0 Do
    Begin
      R := R * 2;
      Dec(N);
    End;
  End
  Else
  Begin
    While N < 0 Do
    Begin
      R := R / 2;
      Inc(N);
    End;
  End;

  Result := x * R;
End;

Function FastLNXP1(Const x: Single): Single;
Var
  y: Single;
Begin
  If (x >= 4.0) Then
    Result := FastLn(1.0 + x)
  Else
  Begin
    y := 1.0 + x;
    If (y = 1.0) Then
      Result := x
    Else
    Begin
      Result := FastLn(y);     // lnxp1(-1) = ln(0) = -Inf

      If y > 0.0 Then
        Result := Result + (x - (y - 1.0)) / y;
    End;
  End;
End;

{%endregion%}

{%region%-----[ Usefull Functions ]---------------------------------------------}

Function FastAbs(f:single):single;
var i : longint;
begin
  i:=longint((@f)^) and $7FFFFFFF;
  result:=single((@i)^)
end;

Function FastNeg(f:single):single;
var i : Int64;
begin
  i:=longint((@f)^) xor $80000000;
  result:=single((@i)^)
end;

Function FastSign(f:single):longint;
begin
  result:=1+((longint((@f)^) shr 31)shl 1)
end;

//function FastPower(i:single;n:integer):single;   //--> This is not enougth accurate
//var l:longint;
//begin
//  l:=longint((@i)^);
//  l:=l-$3F800000;l:=l shl (n-1);l:=l+$3F800000;
//  result:=single((@l)^);
//end;

Function FastPower(Base: Single; Power: Integer): Single;
Var
  Y:    Integer;
  Temp: Double;
Begin
  Y := Abs(Power);
  Temp := _cOne;
  While Y > _cZero Do
  Begin
    While Not Odd(Y) Do
    Begin
      Y := Y Shr 1;
      Base := Base * Base;
    End;
    Dec(Y);
    Temp := Temp * Base;
  End;

  If Power < _cZero Then
    Temp := _cOne / Temp;

  Result := Temp;
End;

{%endregion%}

End.




