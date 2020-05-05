{%region%----[ Pure Pascal will never Convert ]---------------------------------}
// Temp comment this region is where we keep pascal routines for which it does
// not make sense to use SSE/AVX

// Internal for pascal only
{%region%----[ Misc Vector Helpers functions ]----------------------------------}

function AffineVectorMake(const x, y, z : Single) : TBZAffineVector;
begin
   Result.X:=x;
   Result.Y:=y;
   Result.Z:=z;
end;

function AffineVectorMake(const v : TBZVector) : TBZAffineVector;
begin
   Result.X:=v.X;
   Result.Y:=v.Y;
   Result.Z:=v.Z;
end;

function vec2(vx,vy:single):TBZVector2f;
begin
  result.Create(vx,vy);
end;

function vec4(vx,vy,vz,vw:single):TBZVector4f;
begin
  result.Create(vx,vy,vz,vw);
end;

function vec4(vx:single):TBZVector4f;
begin
  result.Create(vx,vx,vx,vx);
end;

function PointVec4(vx:single):TBZVector4f;
begin
  result.Create(vx,vx,vx,1);
end;

function vec2i(vx, vy : Integer) : TBZVector2i;
begin
  Result.Create(vx,vy);
end;

function PointVec4(vx,vy,vz:single):TBZVector4f;
begin
  result.Create(vx,vy,vz,1);
end;

{%endregion%}

{%region%----[ Misc functions for non complete records ]------------------------}
// pascal only functions for simple records which have few or one method.
// items may be moved to full incs when requred.


{%endregion%}


{%endregion}

{%region%----[ Pure Pascal may Convert at some point ]--------------------------}

//----------- Algebra and Trigo fo TBZvector2f --------------------------------

//Type
//  TSinCosRemezArrayType = array[0..4] of TSinCos;
//
//Const
//  SinCos_Remez_const : TSinCosRemezArrayType = ((sin:0.99997937679290771484375;
//                                   cos:1.0),
//                                  (sin:-0.166624367237091064453125;
//                                   cos:-0.49985158205212948 ),
//                                  (sin:8.30897875130176544189453125e-3;
//                                   cos:4.151803521644389800e-2 ),
//                                  (sin:-1.92649182281456887722015380859375e-4;
//                                   cos:-1.34229470252267675e-3 ),
//                                  (sin:2.147840177713078446686267852783203125e-6 ;
//                                   cos:1.89298648243010005378029e-5 ));



function Sin(v:TBZVector2f):TBZVector2f; overload;
begin
  result.X := System.Sin(v.X);
  result.Y := System.Sin(v.Y);
end;

//function Sin(v:TBZVector4f):TBZVector4f; overload;
function Cos(v:TBZVector2f):TBZVector2f; overload;
begin
  result.X := System.Cos(v.X);
  result.Y := System.Sin(v.Y);
end;
//function Cos(v:TBZVector2f):TBZVector4f; overload;

function SinCos(x:Single):TBZvector2f;
begin
  result.X := System.Sin(x);
  result.Y := System.Cos(x);
end;

function SinCos(v:TBZVector2f):TBZvector2f;
begin
  result.X := System.Sin(v.X);
  result.Y := System.Cos(v.Y);
end;

function Fract(Constref v:TBZVector4f):TBZVector4f;
var
  vt : TBZVector4f;
  vi : TBZVector4i;
begin
  vi := v.Floor;
  vt.Create(vi.x,vi.y,vi.z,vi.W);
  vt := v - vt;
  result := vt;    //vt.Trunc
end;

function VectorIsColinear(constref v1, v2: TBZVector) : Boolean;
var
  a, b, c : Single;
begin
  a := v1.DotProduct(v1);
  b := v1.DotProduct(v2);
  c := v2.DotProduct(v2);
  Result :=  (a*c - b*b) < cColinearBias;
end;

(* function PlaneContains(const Location, Normal: TBZVector; const TestBSphere: TBZBoundingSphere): TBZSpaceContains;
var
  Plane: TBZHmgPlane;
begin
 Plane.Create(Location, Normal);
 Result := plane.Contains(TestBSphere);
end; *)


{%endregion}

{$IFNDEF USE_ASM}

{%region----[ PASCAL VERSION OF ASSEMBLER ROUTINES BELOW ]----------------------}

function Trunc(Constref v:TBZVector2f):TBZVector2i;
begin
  result := v.Trunc;
end;

function Round(Constref v:TBZVector2f):TBZVector2i;
begin
  result := v.Round;
end;

function Floor(Constref v:TBZVector2f):TBZVector2i;
begin
  result := v.Floor;
end;

function Ceil(Constref v:TBZVector2f):TBZVector2i;
begin
  result := v.Ceil;
end;

function Fract(Constref v:TBZVector2f):TBZVector2f;
var
  vt : TBZVector2f;
  vi : TBZVector2i;
begin
  vi := v.Floor;
  vt.Create(vi.x,vi.y);
  vt := v - vt;
  result := vt;    //vt.Trunc
end;

function Sqrt(Constref v:TBZVector2f):TBZVector2f;
begin
  result.Create(Sqrt(v.X), Sqrt(v.Y));
end;

function InvSqrt(Constref v:TBZVector2f):TBZVector2f;
begin
  result.Create(InvSqrt(v.X), InvSqrt(v.Y));
end;





(* function BarycentricCoordinates(const v1, v2, v3, p: TBZAffineVector; var u, v: single): boolean;
var
  a1, a2: integer;
  n, e1, e2, pt: TAffineVector;
begin
  // calculate edges
  VectorSubtract(v1, v3, e1);
  VectorSubtract(v2, v3, e2);

  // calculate p relative to v3
  VectorSubtract(p, v3, pt);

  // find the dominant axis
  n:= VectorCrossProduct(e1, e2);
  AbsVector(n);
  a1:= 0;
  if n.V[1] > n.V[a1] then
    a1:= 1;
  if n.V[2] > n.V[a1] then
    a1:= 2;

  // use dominant axis for projection
  case a1 of
    0: begin
      a1:= 1;
      a2:= 2;
    end;
    1: begin
      a1:= 0;
      a2:= 2;
    end;
  else // 2:
    a1:= 0;
    a2:= 1;
  end;

  // solve for u and v
  u:= (pt.V[a2] * e2.V[a1] - pt.V[a1] * e2.V[a2]) / (e1.V[a2] * e2.V[a1] - e1.V[a1] * e2.V[a2]);
  v:= (pt.V[a2] * e1.V[a1] - pt.V[a1] * e1.V[a2]) / (e2.V[a2] * e1.V[a1] - e2.V[a1] * e1.V[a2]);

  result:= (u >= 0) and (v >= 0) and (u+v <= 1);
end;


function TriangleArea(const p1, p2, p3 : TBZAffineVector) : Single;
begin
   Result:=0.5*VectorLength(VectorCrossProduct(VectorSubtract(p2, p1),
                                               VectorSubtract(p3, p1)));
end;

function PolygonArea(const p : PAffineVectorArray; nSides : Integer) : Single;
var
   r : TAffineVector;
   i : Integer;
   p1, p2, p3 : PAffineVector;
begin
   Result:=0;
   if nSides>2 then begin
      RstVector(r);
      p1:=@p[0];
      p2:=@p[1];
      for i:=2 to nSides-1 do begin
         p3:=@p[i];
         AddVector(r, VectorCrossProduct(VectorSubtract(p2^, p1^),
                                         VectorSubtract(p3^, p1^)));
         p2:=p3;
      end;
      Result:=VectorLength(r)*0.5;
   end;
end;


function TriangleSignedArea(const p1, p2, p3 : TBZAffineVector) : Single;
begin
   Result:=0.5*( (p2.V[0]-p1.V[0])*(p3.V[1]-p1.V[1])
                -(p3.V[0]-p1.V[0])*(p2.V[1]-p1.V[1]));
end;


 function PolygonSignedArea(const p : PAffineVectorArray; nSides : Integer) : Single;
var
   i : Integer;
   p1, p2, p3 : PAffineVector;
begin
   Result:=0;
   if nSides>2 then begin
      p1:=@(p^[0]);
      p2:=@(p^[1]);
      for i:=2 to nSides-1 do begin
         p3:=@(p^[i]);
         Result:=Result+(p2^.V[0]-p1^.V[0])*(p3^.V[1]-p1^.V[1])
                       -(p3^.V[0]-p1^.V[0])*(p2^.V[1]-p1^.V[1]);
         p2:=p3;
      end;
      Result:=Result*0.5;
   end;
end;

function RandomPointOnSphere: TBZVector;
var
   t, w : Single;
begin
   p.V[2]:=2*Random-1;
   t:=2*PI*Random;
   w:=Sqrt(1-p.V[2]*p.V[2]);
   GLVectorGeometry.SinCos(t, w, p.V[1], p.V[0]);
end;  *)

//procedure ConvertQuaternionToArcPoints(Constref Q : TBZQuaternion; var ArcFrom, ArcTo: TBZVector);
//var
//   s, invS : Single;
//begin
//   s:=Q.ImagePart.X*Q.ImagePart.X+Q.ImagePart.Y*Q.ImagePart.Y;
//   if s=0 then ArcFrom := AffineVectorMake( 0, 1, 0)
//   else
//   begin
//      invS:=InvSqrt(s);
//      ArcFrom := AffineVectorMake( -Q.ImagePart.Y*invS, Q.ImagePart.X*invS, 0);
//   end;
//   ArcTo.X:=Q.RealPart*ArcFrom.X-Q.ImagePart.Z*ArcFrom.Y;
//   ArcTo.Y:=Q.RealPart*ArcFrom.Y+Q.ImagePart.Z*ArcFrom.X;
//   ArcTo.Z:=Q.ImagePart.X*ArcFrom.Y-Q.ImagePart.Y*ArcFrom.X;
//   if Q.RealPart<0 then ArcFrom := AffineVectorMake( -ArcFrom.X, -ArcFrom.Y, 0);
//end;

{%endregion%}

{$ENDIF}
