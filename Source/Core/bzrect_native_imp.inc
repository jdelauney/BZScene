{%region%----[ Pure Pascal will never Convert ]---------------------------------}
{ This region is where we keep pascal routines for which it does not make sense to use SSE/AVX }

{%region%-----[ TBZRect ]-------------------------------------------------------}

procedure TBZRect.Create(Const aLeft,aTop,aRight, aBottom : Longint );
begin
  Self.Left := aLeft;
  Self.Top := aTop;
  Self.Right := aRight;
  Self.Bottom := aBottom;
end;

procedure TBZRect.Create(Const aLeft,aTop,aRight,aBottom : Longint; Const DoNormalize : Boolean );
begin
  Self.Left := aLeft;
  Self.Top := aTop;
  Self.Right := aRight;
  Self.Bottom := aBottom;
  if DoNormalize then Self.Normalize;
end;

procedure TBZRect.Create(Const aLeftTop,aRightBottom : TBZPoint; Const DoNormalize : Boolean);
begin
  Self.TopLeft := aLeftTop;
  Self.BottomRight := aRightBottom;
  if DoNormalize then Self.Normalize;
end;

procedure TBZRect.Create(Const ARect : TRect );
begin
  With Self do
  begin
    Left := ARect.Left;
    Top := ARect.Top;
    Right := ARect.Right;
    Bottom := ARect.Bottom;
  End;
End;

procedure TBZRect.Create(Origin: TBZPoint; AWidth, AHeight: Longint);
Var
  Size : TBZPoint;
begin
  Size.Create(AWidth, AHeight);
  TopLeft := Origin;
  BottomRight := TopLeft + Size;
end;

procedure TBZRect.CreateEmpty;
begin
  Self.Create(0,0,0,0, False);
end;

procedure TBZRect.setHeight(AValue: Longint);
begin
  bottom:=top+avalue;
end;

procedure TBZRect.setWidth(AValue: Longint);
begin
  Right:=Left+avalue;
end;

function TBZRect.ToString : String;
begin
  Result := '(Left : '+IntToStr(Self.Left)+
           ' ,Top : '+IntToStr(Self.Top)+
           ' ,Right : '+IntToStr(Self.Right)+
           ' ,Bottom : '+IntToStr(Self.Bottom)+')';
end;

Function TBZRect.PointOutRect(ConstRef x, y : LongInt) : Boolean;
begin
  result:=true;
  if x<Self.Left then exit;
  if y<Self.Top then exit;
  if x>Self.Right then exit;
  if y>Self.Bottom then exit;
  result:=false;
end;

Function TBZRect.PointOutRect(ConstRef p : TBZPoint):Boolean;
Begin
  result := Self.PointOutRect(P.X,P.Y);
End;

function TBZRect.PointInRect(ConstRef x, y : LongInt) : Boolean;
begin
  //if (Self.Left > Self.Rigth) or (Self.Top > Self.Bottom) then Self.Normalize;
  Result := ((X>=Self.Left) and (X<=Self.Right)) and ((Y>=Self.Top) and (Y<=Self.Bottom));
  //result:=false;
  //if x<Self.Left then exit;
  //if y<Self.Top then exit;
  //if x>Self.Right then exit;
  //if y>Self.Bottom then exit;
  //result:=true;
end;

function TBZRect.PointInRect(ConstRef  p : TBZPoint): Boolean;
begin
  result := Self.PointInRect(P.X,P.Y);
End;

function TBZRect.RectInRect(ConstRef Rect2 : TBZRect) : Boolean;
begin
 Result:= (Self.Left >= Rect2.Left) and (Self.Right <= Rect2.Right) and
  (Self.Top >= Rect2.Top) and (Self.Bottom <= Rect2.Bottom);
end;

function TBZRect.OverlapRect(ConstRef Rect2: TBZRect): Boolean; inline;
begin
 Result:= (Self.Left < Rect2.Right) and (Self.Right > Rect2.Left) and
  (Self.Top < Rect2.Bottom) and (Self.Bottom > Rect2.Top);
end;

function TBZRect.OffSetRect(ConstRef dx, dy : LongInt) : Boolean;
Var pt : TBZPoint;
Begin
  pt.Create(dx,dy);
  result := Self.OffsetRect(pt);
end;

function TBZRect.GetWidth : LongInt;
begin
  Result := abs(Self.Right - Self.Left) + 1;
//  if Self.Left = 0 then Result := Result +1;
end;

function TBZRect.GetHeight : LongInt;
begin
  Result := abs(Self.Bottom - Self.Top) + 1;
 // if Self.Top = 0 then Result := Result +1;
End;

function TBZRect.CenterPoint: TBZPoint;
begin
  Result := ((BottomRight - TopLeft) div 2) + TopLeft;
  //Result.X := (Right-Left) div 2 + Left;
  //Result.Y := (Bottom-Top) div 2 + Top;
end;

{%endregion}

{%region%-----[ TBZFloatRect ]--------------------------------------------------}

procedure TBZFloatRect.Create(const aLeft, aTop, aRight, aBottom : Single);
begin
  Self.Left := aLeft;
  Self.Top := aTop;
  Self.Right := aRight;
  Self.Bottom := aBottom;
end;

procedure TBZFloatRect.Create(const aLeftTop, aRightBottom : TBZFloatPoint; const DoNormalize : Boolean);
begin
  Self.TopLeft := aLeftTop;
  Self.BottomRight := aRightBottom;
  if DoNormalize then Self.Normalize;
end;

procedure TBZFloatRect.Create(const ARect : TRect);
begin
  With Self do
  begin
    Left := ARect.Left;
    Top := ARect.Top;
    Right := ARect.Right;
    Bottom := ARect.Bottom;
  End;
End;

procedure TBZFloatRect.Create(Origin: TBZFloatPoint; AWidth, AHeight: Single);
Var
  Size : TBZFloatPoint;
begin
  Size.Create(AWidth, AHeight);
  TopLeft := Origin;
  BottomRight := TopLeft + Size;
end;

procedure TBZFloatRect.CreateEmpty;
begin
  Self.Create(0,0,0,0);
end;

procedure TBZFloatRect.CreateBounds(const aLeft, aTop, aWidth, aHeight : Single);
begin
 Self.Create(aLeft, aTop, aLeft + aWidth, aTop + aHeight);
end;

procedure TBZFloatRect.setHeight(AValue: Single);
begin
  bottom:=top+avalue;
end;

function TBZFloatRect.GetBottomLeft : TBZFloatPoint;
begin
  Result.Create(Self.Top, Self.Right);
end;

function TBZFloatRect.GetTopRight : TBZFloatPoint;
begin
  Result.Create(Self.Bottom, Self.Left);
end;

procedure TBZFloatRect.setWidth(AValue: Single);
begin
  Right:=Left+avalue;
end;

function TBZFloatRect.ToString : String;
begin
  Result := '(Left : '+FloatToStrf(Self.Left,fffixed,5,5)+
           ' ,Top : '+FloatToStrf(Self.Top,fffixed,5,5)+
           ' ,Right : '+FloatToStrf(Self.Right,fffixed,5,5)+
           ' ,Bottom : '+FloatToStrf(Self.Bottom,fffixed,5,5)+')';
end;

function TBZFloatRect.PointOutRect(ConstRef x, y : Single) : Boolean;
begin
  result:=true;
  if x<Self.Left then exit;
  if y<Self.Top then exit;
  if x>Self.Right then exit;
  if y>Self.Bottom then exit;
  result:=false;
end;

function TBZFloatRect.PointOutRect(ConstRef p : TBZFloatPoint) : Boolean;
Begin
  result := Self.PointOutRect(P.X,P.Y);
End;

function TBZFloatRect.PointInRect(ConstRef x, y : Single) : Boolean;
begin
  result:=false;
  if x<Self.Left then exit;
  if y<Self.Top then exit;
  if x>Self.Right then exit;
  if y>Self.Bottom then exit;
  result:=true;
end;

function TBZFloatRect.PointInRect(ConstRef  p : TBZFloatPoint): Boolean;
begin
  result := Self.PointInRect(P.X,P.Y);
End;

function TBZFloatRect.RectInRect(ConstRef Rect2 : TBZFloatRect) : Boolean;
begin
 Result:= (Self.Left >= Rect2.Left) and (Self.Right <= Rect2.Right) and
  (Self.Top >= Rect2.Top) and (Self.Bottom <= Rect2.Bottom);
end;

function TBZFloatRect.OverlapRect(ConstRef Rect2: TBZFloatRect): Boolean; inline;
begin
 Result:= (Self.Left < Rect2.Right) and (Self.Right > Rect2.Left) and
  (Self.Top < Rect2.Bottom) and (Self.Bottom > Rect2.Top);
end;

function TBZFloatRect.OffSetRect(ConstRef dx, dy : Single) : Boolean;
Var pt : TBZFloatPoint;
Begin
  pt.Create(dx,dy);
  result := Self.OffsetRect(pt);
end;

function TBZFloatRect.GetWidth : Single;
begin
  Result := abs(Self.Right - Self.Left) + 1;
end;

function TBZFloatRect.GetHeight : Single;
begin
  Result := abs(Self.Bottom - Self.Top) + 1;
End;

function TBZFloatRect.IntersectWithRect(ConstRef Rect2 : TBZFloatRect; var OutRect : TBZFloatRect) : Boolean;
Begin
  if Self.Left >= Rect2.Left then OutRect.Left := Self.Left else OutRect.Left := Rect2.Left;
  if Self.Right <= Rect2.Right then OutRect.Right := Self.Right else OutRect.Right := Rect2.Right;
  if Self.Top >= Rect2.Top then OutRect.Top := Self.Top else OutRect.Top := Rect2.Top;
  if Self.Bottom <= Rect2.Bottom then OutRect.Bottom := Self.Bottom else OutRect.Bottom := Rect2.Bottom;
  Result := (OutRect.Right >= OutRect.Left) and (OutRect.Bottom >= OutRect.Top);
end;

function TBZFloatRect.CenterPoint: TBZFloatPoint;
begin
  Result := ((BottomRight - TopLeft) * 0.5 ) + TopLeft;
  //Result.X := (Right-Left) div 2 + Left;
  //Result.Y := (Bottom-Top) div 2 + Top;
end;

{%endregion}

{%endregion%}

{%region%----[ Pure Pascal may Convert at some point ]--------------------------}

{%region%-----[ TBZRect ]-------------------------------------------------------}

function TBZRect.OffSetRect(ConstRef P: TBZPoint): Boolean;
Begin
  Self.TopLeft := Self.TopLeft + P;
  Self.BottomRight := Self.BottomRight + P;
  Result := (Self.Left >= 0) and (Self.Top >= 0);
End;

procedure TBZRect.MoveTo(ConstRef P : TBZPoint);
Var
  Diff : TBZPoint;
begin
  Diff := P - Self.TopLeft;
  Self.OffsetRect(Diff);
end;

procedure TBZRect.Normalize;
var
  x: LongInt;
begin
  if Top>Bottom then
  begin
    x := Top;
    Top := Bottom;
    Bottom := x;
  end;
  if Left>Right then
  begin
    x := Left;
    Left := Right;
    Right := x;
  end
end;

procedure TBZRect.Inflate(DL, DT, DR, DB: Longint);
begin
  Dec(Left, DL);
  Dec(Top, DT);
  Inc(Right, DR);
  Inc(Bottom, DB);
end;

procedure TBZRect.Inflate(DX, DY: Longint);
begin
  Inflate(DX, DY,DX, DY);
end;


procedure TBZRect.Contract(DL, DT, DR, DB: Longint);
begin
  Inc(Left, DL);
  Inc(Top, DT);
  Dec(Right, DR);
  Dec(Bottom, DB);
end;

procedure TBZRect.Contract(DX, DY: Longint);
begin
  Contract(DX, DY,DX, DY);
end;
{%endregion}

{%region%-----[ TBZFloatRect ]--------------------------------------------------}

function TBZFloatRect.OffSetRect(ConstRef P: TBZFloatPoint): Boolean;
Begin
  Self.TopLeft := Self.TopLeft + P;
  Self.BottomRight := Self.BottomRight + P;
  Result := (Self.Left >= 0) and (Self.Top >= 0);
End;

procedure TBZFloatRect.Normalize;
var
  x: Single;
begin
  if Top>Bottom then
  begin
    x := Top;
    Top := Bottom;
    Bottom := x;
  end;
  if Left>Right then
  begin
    x := Left;
    Left := Right;
    Right := x;
  end
end;

procedure TBZFloatRect.Inflate(DL, DT, DR, DB: Single);
begin
 Left   := Left - DL;
 Top    := Top - DT;
 Right  := Right + DR;
 Bottom := Bottom + DB;
end;

procedure TBZFloatRect.Inflate(DX, DY: Single);
begin
  Inflate(DX, DY,DX, DY);
end;

procedure TBZFloatRect.Contract(DL, DT, DR, DB: Single);
begin
  Left   := Left + DL;
  Top    := Top + DT;
  Right  := Right - DR;
  Bottom := Bottom - DB;
end;

procedure TBZFloatRect.Contract(DX, DY: Single);
begin
  Contract(DX, DY,DX, DY);
end;

procedure TBZFloatRect.UnionRect(ConstRef Rect2 : TBZFloatRect);
begin
 // if not IsRectEmpty(R2) then
 // begin
    if Rect2.Left < Self.Left then Self.Left := Rect2.Left;
    if Rect2.Top < Self.Top then Self.Top := Rect2.Top;
    if Rect2.Right > Self.Right then Self.Right := Rect2.Right;
    if Rect2.Bottom > Self.Bottom then Self.Bottom := Rect2.Bottom;
 // end;
//  Result := not IsRectEmpty(Rect);
//  if not Result then Rect := ZERO_RECT;
end;

{%endregion}

{%endregion}

{.$IFNDEF USE_ASM}

{%region----[ PASCAL VERSION OF ASSEMBLER ROUTINES BELOW ]----------------------}
//@TODO : WRITE ASM CODE

{%region%----[ Operators ]------------------------------------------------------}

{%region%-----[ TBZRect ]-------------------------------------------------------}

class operator TBZRect.+ (constref A, B : TBZRect) : TBZRect;
begin
  Result.Left := A.Left + B.Left;
  Result.Top := A.Top + B.Top;
  Result.Right := A.Right + B.Right;
  Result.Bottom := A.Bottom + B.Bottom;
end;

class operator TBZRect.- (constref A, B : TBZRect) : TBZRect;
begin
  Result.Left := A.Left - B.Left;
  Result.Top := A.Top - B.Top;
  Result.Right := A.Right - B.Right;
  Result.Bottom := A.Bottom - B.Bottom;
end;

class operator TBZRect. + (constref A : TBZRect; constref B : Longint) : TBZRect;
begin
  Result.Left := A.Left + B;
  Result.Top := A.Top + B;
  Result.Right := A.Right + B;
  Result.Bottom := A.Bottom + B;
end;

class operator TBZRect. - (constref A : TBZRect; constref B : Longint) : TBZRect;
begin
  Result.Left := A.Left - B;
  Result.Top := A.Top - B;
  Result.Right := A.Right - B;
  Result.Bottom := A.Bottom - B;
end;

class operator TBZRect. = (ConstRef A, B : TBZRect) : Boolean;
begin
  Result := (A.Left = B.Left) and (A.Top = B.Top) and (A.Right = B.Right) and (A.Bottom = B.Bottom);
end;

class operator TBZRect.<>(ConstRef A, B : TBZRect) : Boolean;
begin
  Result := (A.Left <> B.Left) or (A.Top <> B.Top) or (A.Right <> B.Right) or (A.Bottom <> B.Bottom);
end;

//function TBZRect.abs : TBZRect;
//begin
//  Result.Left := system.Abs(Self.Left);
//  Result.Top := system.Abs(Self.Top);
//  Result.Right := system.Abs(Self.Right);
//  Result.Bottom := system.Abs(Self.Bottom);
//end;

function TBZRect.Min(ConstRef B : TBZRect) : TBZRect;
begin
  Result := Self;
  if Self.Left>B.Left then Result.Left := B.Left;
  if Self.Top>B.Top then Result.Top := B.Top;
  if Self.Right>B.Right then Result.Right := B.Right;
  if Self.Bottom>B.Bottom then Result.Bottom := B.Bottom;
end;

function TBZRect.Min(Constref B : LongInt) : TBZRect;
begin
  Result := Self;
  if Self.Left>B then Result.Left := B;
  if Self.Top>B then Result.Top := B;
  if Self.Right>B then Result.Right := B;
  if Self.Bottom>B then Result.Bottom := B;
end;

function TBZRect.Max(ConstRef B : TBZRect) : TBZRect;
begin
  Result := Self;
  if Self.Left<B.Left then Result.Left := B.Left;
  if Self.Top<B.Top then Result.Top := B.Top;
  if Self.Right<B.Right then Result.Right := B.Right;
  if Self.Bottom<B.Bottom then Result.Bottom := B.Bottom;
end;

function TBZRect.Max(Constref B : LongInt) : TBZRect;
begin
  Result := Self;
  if Self.Left<B then Result.Left := B;
  if Self.Top<B then Result.Top := B;
  if Self.Right<B then Result.Right := B;
  if Self.Bottom<B then Result.Bottom := B;
end;

function TBZRect.Clamp(Constref AMin, AMax : TBZRect) : TBZRect;
begin
  Result := Self.Max(AMin);
  Result := Result.Min(AMax);
end;

function TBZRect.Clamp(Constref AMin, AMax : LongInt) : TBZRect;
begin
  Result := Self.Max(AMin);
  Result := Result.Min(AMax);
end;

function TBZRect.AsRect : TRect;
begin
  With Result do
  begin
    Left := Self.Left;
    Top := Self.Top;
    Right := Self.Right;
    Bottom := Self.Bottom;
  End;
End;

{%endregion%}

{%region%-----[ TBZFloatRect ]--------------------------------------------------}

class operator TBZFloatRect.+ (constref A, B : TBZFloatRect) : TBZFloatRect;
begin
  Result.Left := A.Left + B.Left;
  Result.Top := A.Top + B.Top;
  Result.Right := A.Right + B.Right;
  Result.Bottom := A.Bottom + B.Bottom;
end;

class operator TBZFloatRect.- (constref A, B : TBZFloatRect) : TBZFloatRect;
begin
  Result.Left := A.Left - B.Left;
  Result.Top := A.Top - B.Top;
  Result.Right := A.Right - B.Right;
  Result.Bottom := A.Bottom - B.Bottom;
end;

class operator TBZFloatRect. + (constref A : TBZFloatRect; constref B : Single) : TBZFloatRect;
begin
  Result.Left := A.Left + B;
  Result.Top := A.Top + B;
  Result.Right := A.Right + B;
  Result.Bottom := A.Bottom + B;
end;

class operator TBZFloatRect. - (constref A : TBZFloatRect; constref B : Single) : TBZFloatRect;
begin
  Result.Left := A.Left - B;
  Result.Top := A.Top - B;
  Result.Right := A.Right - B;
  Result.Bottom := A.Bottom - B;
end;

class operator TBZFloatRect. = (ConstRef A, B : TBZFloatRect) : Boolean;
begin
  Result := (A.Left = B.Left) and (A.Top = B.Top) and (A.Right = B.Right) and (A.Bottom = B.Bottom);
end;

class operator TBZFloatRect.<>(ConstRef A, B : TBZFloatRect) : Boolean;
begin
  Result := (A.Left <> B.Left) or (A.Top <> B.Top) or (A.Right <> B.Right) or (A.Bottom <> B.Bottom);
end;

//function TBZFloatRect.abs : TBZFloatRect;
//begin
//  Result.Left := system.Abs(Self.Left);
//  Result.Top := system.Abs(Self.Top);
//  Result.Right := system.Abs(Self.Right);
//  Result.Bottom := system.Abs(Self.Bottom);
//end;

function TBZFloatRect.Min(ConstRef B : TBZFloatRect) : TBZFloatRect;
begin
  Result := Self;
  if Self.Left>B.Left then Result.Left := B.Left;
  if Self.Top>B.Top then Result.Top := B.Top;
  if Self.Right>B.Right then Result.Right := B.Right;
  if Self.Bottom>B.Bottom then Result.Bottom := B.Bottom;
end;

function TBZFloatRect.Min(Constref B : Single) : TBZFloatRect;
begin
  Result := Self;
  if Self.Left>B then Result.Left := B;
  if Self.Top>B then Result.Top := B;
  if Self.Right>B then Result.Right := B;
  if Self.Bottom>B then Result.Bottom := B;
end;

function TBZFloatRect.Max(ConstRef B : TBZFloatRect) : TBZFloatRect;
begin
  Result := Self;
  if Self.Left<B.Left then Result.Left := B.Left;
  if Self.Top<B.Top then Result.Top := B.Top;
  if Self.Right<B.Right then Result.Right := B.Right;
  if Self.Bottom<B.Bottom then Result.Bottom := B.Bottom;
end;

function TBZFloatRect.Max(Constref B : Single) : TBZFloatRect;
begin
  Result := Self;
  if Self.Left<B then Result.Left := B;
  if Self.Top<B then Result.Top := B;
  if Self.Right<B then Result.Right := B;
  if Self.Bottom<B then Result.Bottom := B;
end;

function TBZFloatRect.Clamp(Constref AMin, AMax : TBZFloatRect) : TBZFloatRect;
begin
  Result := Self.Max(AMin);
  Result := Result.Min(AMax);
end;

function TBZFloatRect.Clamp(Constref AMin, AMax : Single) : TBZFloatRect;
begin
  Result := Self.Max(AMin);
  Result := Result.Min(AMax);
end;

function TBZFloatRect.AsNativeRect : TRect;
begin
  Result := Self.AsRect.AsRect;
End;

function TBZFloatRect.AsRect : TBZRect;
begin
  Result.Create(TopLeft.Round, BottomRight.Round);
End;

{%endregion%}

{%endregion%}

{%endregion%}

{.$ENDIF}
