{ ****************************************************************************** }
{ * Real/complex QR, LQ, bi(tri)diagonal, Hessenberg decompositions support    * }
{ * by QQ 600585@qq.com                                                        * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
procedure SymmetricMatrixVectorMultiply(const a: TLMatrix; IsUpper: Boolean;
  i1: TLInt; i2: TLInt; const x: TLVec;
  alpha: TLFloat; var y: TLVec);
var
  i  : TLInt;
  BA1: TLInt;
  BA2: TLInt;
  BY1: TLInt;
  BY2: TLInt;
  BX1: TLInt;
  BX2: TLInt;
  n  : TLInt;
  v  : TLFloat;
begin
  n := i2 - i1 + 1;
  if n <= 0 then
    begin
      Exit;
    end;

  //
  // Let A = L + D + U, where
  // L is strictly lower triangular (main diagonal is zero)
  // D is diagonal
  // U is strictly upper triangular (main diagonal is zero)
  //
  // A*x = L*x + D*x + U*x
  //
  // Calculate D*x first
  //
  i := i1;
  while i <= i2 do
    begin
      y[i - i1 + 1] := a[i, i] * x[i - i1 + 1];
      inc(i);
    end;

  //
  // Add L*x + U*x
  //
  if IsUpper then
    begin
      i := i1;
      while i <= i2 - 1 do
        begin

          //
          // Add L*x to the result
          //
          v := x[i - i1 + 1];
          BY1 := i - i1 + 2;
          BY2 := n;
          BA1 := i + 1;
          BA2 := i2;
          APVAdd(@y[0], BY1, BY2, @a[i][0], BA1, BA2, v);

          //
          // Add U*x to the result
          //
          BX1 := i - i1 + 2;
          BX2 := n;
          BA1 := i + 1;
          BA2 := i2;
          v := APVDotProduct(@x[0], BX1, BX2, @a[i][0], BA1, BA2);
          y[i - i1 + 1] := y[i - i1 + 1] + v;
          inc(i);
        end;
    end
  else
    begin
      i := i1 + 1;
      while i <= i2 do
        begin

          //
          // Add L*x to the result
          //
          BX1 := 1;
          BX2 := i - i1;
          BA1 := i1;
          BA2 := i - 1;
          v := APVDotProduct(@x[0], BX1, BX2, @a[i][0], BA1, BA2);
          y[i - i1 + 1] := y[i - i1 + 1] + v;

          //
          // Add U*x to the result
          //
          v := x[i - i1 + 1];
          BY1 := 1;
          BY2 := i - i1;
          BA1 := i1;
          BA2 := i - 1;
          APVAdd(@y[0], BY1, BY2, @a[i][0], BA1, BA2, v);
          inc(i);
        end;
    end;
  APVMul(@y[0], 1, n, alpha);
end;

procedure SymmetricRank2Update(var a: TLMatrix; IsUpper: Boolean;
  i1: TLInt; i2: TLInt; const x: TLVec;
  const y: TLVec; var t: TLVec; alpha: TLFloat);
var
  i  : TLInt;
  TP1: TLInt;
  TP2: TLInt;
  v  : TLFloat;
begin
  if IsUpper then
    begin
      i := i1;
      while i <= i2 do
        begin
          TP1 := i + 1 - i1;
          TP2 := i2 - i1 + 1;
          v := x[i + 1 - i1];
          APVMove(@t[0], TP1, TP2, @y[0], TP1, TP2, v);
          v := y[i + 1 - i1];
          APVAdd(@t[0], TP1, TP2, @x[0], TP1, TP2, v);
          APVMul(@t[0], TP1, TP2, alpha);
          APVAdd(@a[i][0], i, i2, @t[0], TP1, TP2);
          inc(i);
        end;
    end
  else
    begin
      i := i1;
      while i <= i2 do
        begin
          TP1 := 1;
          TP2 := i + 1 - i1;
          v := x[i + 1 - i1];
          APVMove(@t[0], TP1, TP2, @y[0], TP1, TP2, v);
          v := y[i + 1 - i1];
          APVAdd(@t[0], TP1, TP2, @x[0], TP1, TP2, v);
          APVMul(@t[0], TP1, TP2, alpha);
          APVAdd(@a[i][0], i1, i, @t[0], TP1, TP2);
          inc(i);
        end;
    end;
end;

(* ************************************************************************
  Generation of an elementary TLComplex reflection transformation

  The subroutine generates elementary TLComplex reflection H of  order  N,  so
  that, for a given X, the following equality holds true:

  ( X(1) )   ( Beta )
  H' * (  ..  ) = (  0   ),   H'*H = I,   Beta is a real number
  ( X(n) )   (  0   )

  where

  ( V(1) )
  H = 1 - Tau * (  ..  ) * ( conj(V(1)), ..., conj(V(n)) )
  ( V(n) )

  where the first component of vector V equals 1.

  Input parameters:
  X   -   vector. Array with elements [1..N].
  N   -   reflection order.

  Output parameters:
  X   -   components from 2 to N are replaced by vector V.
  The first component is replaced with parameter Beta.
  Tau -   scalar value Tau.

  This subroutine is the modification of CLARFG subroutines  from the LAPACK
  library. It has similar functionality except for the fact that it  doesnæŠ?
  handle errors when intermediate results cause an overflow.
  ************************************************************************ *)
procedure ComplexGenerateReflection(var x: TLComplexVec; n: TLInt;
  var TAU: TLComplex);
var
  j    : TLInt;
  alpha: TLComplex;
  ALPHI: TLFloat;
  ALPHR: TLFloat;
  beta : TLFloat;
  XNORM: TLFloat;
  mx   : TLFloat;
  t    : TLComplex;
  s    : TLFloat;
  v    : TLComplex;
  i_   : TLInt;
begin
  if n <= 0 then
    begin
      TAU := C_Complex(0);
      Exit;
    end;

  //
  // Scale if needed (to avoid overflow/underflow during intermediate
  // calculations).
  //
  mx := 0;
  j := 1;
  while j <= n do
    begin
      mx := Max(AbsComplex(x[j]), mx);
      inc(j);
    end;
  s := 1;
  if AP_FP_NEq(mx, 0) then
    begin
      if AP_FP_Less(mx, 1) then
        begin
          s := Sqrt(MinRealNumber);
          v := C_Complex(1 / s);
          for i_ := 1 to n do
            begin
              x[i_] := C_Mul(v, x[i_]);
            end;
        end
      else
        begin
          s := Sqrt(MaxRealNumber);
          v := C_Complex(1 / s);
          for i_ := 1 to n do
            begin
              x[i_] := C_Mul(v, x[i_]);
            end;
        end;
    end;

  //
  // calculate
  //
  alpha := x[1];
  mx := 0;
  j := 2;
  while j <= n do
    begin
      mx := Max(AbsComplex(x[j]), mx);
      inc(j);
    end;
  XNORM := 0;
  if AP_FP_NEq(mx, 0) then
    begin
      j := 2;
      while j <= n do
        begin
          t := C_DivR(x[j], mx);
          XNORM := XNORM + C_Mul(t, Conj(t)).x;
          inc(j);
        end;
      XNORM := Sqrt(XNORM) * mx;
    end;
  ALPHR := alpha.x;
  ALPHI := alpha.y;
  if AP_FP_Eq(XNORM, 0) and AP_FP_Eq(ALPHI, 0) then
    begin
      TAU := C_Complex(0);
      x[1] := C_MulR(x[1], s);
      Exit;
    end;
  mx := Max(AbsReal(ALPHR), AbsReal(ALPHI));
  mx := Max(mx, AbsReal(XNORM));
  beta := -mx * Sqrt(AP_Sqr(ALPHR / mx) + AP_Sqr(ALPHI / mx) +
    AP_Sqr(XNORM / mx));
  if AP_FP_Less(ALPHR, 0) then
    begin
      beta := -beta;
    end;
  TAU.x := (beta - ALPHR) / beta;
  TAU.y := -ALPHI / beta;
  alpha := C_RDiv(1, C_SubR(alpha, beta));
  if n > 1 then
    begin
      for i_ := 2 to n do
        begin
          x[i_] := C_Mul(alpha, x[i_]);
        end;
    end;
  alpha := C_Complex(beta);
  x[1] := alpha;

  //
  // Scale back
  //
  x[1] := C_MulR(x[1], s);
end;

(* ************************************************************************
  Application of an elementary reflection to a rectangular matrix of size MxN

  The  algorithm  pre-multiplies  the  matrix  by  an  elementary reflection
  transformation  which  is  given  by  column  V  and  scalar  Tau (see the
  description of the GenerateReflection). Not the whole matrix  but  only  a
  part of it is transformed (rows from M1 to M2, columns from N1 to N2). Only
  the elements of this submatrix are changed.

  Note: the matrix is multiplied by H, not by H'.   If  it  is  required  to
  multiply the matrix by H', it is necessary to pass Conj(Tau) instead of Tau.

  Input parameters:
  C       -   matrix to be transformed.
  Tau     -   scalar defining transformation.
  V       -   column defining transformation.
  Array whose index ranges within [1..M2-M1+1]
  M1, M2  -   range of rows to be transformed.
  N1, N2  -   range of columns to be transformed.
  WORK    -   working array whose index goes from N1 to N2.

  Output parameters:
  C       -   the result of multiplying the input matrix C by the
  transformation matrix which is given by Tau and V.
  If N1>N2 or M1>M2, C is not modified.
  ************************************************************************ *)
procedure ComplexApplyReflectionFromTheLeft(var c: TLComplexMatrix;
  TAU: TLComplex; const v: TLComplexVec; m1: TLInt; m2: TLInt;
  n1: TLInt; n2: TLInt; var Work: TLComplexVec);
var
  t : TLComplex;
  i : TLInt;
  VM: TLInt;
  i_: TLInt;
begin
  if C_EqualR(TAU, 0) or (n1 > n2) or (m1 > m2) then
    begin
      Exit;
    end;

  //
  // w := C^T * conj(v)
  //
  VM := m2 - m1 + 1;
  i := n1;
  while i <= n2 do
    begin
      Work[i] := C_Complex(0);
      inc(i);
    end;
  i := m1;
  while i <= m2 do
    begin
      t := Conj(v[i + 1 - m1]);
      for i_ := n1 to n2 do
        begin
          Work[i_] := C_Add(Work[i_], C_Mul(t, c[i, i_]));
        end;
      inc(i);
    end;

  //
  // C := C - tau * v * w^T
  //
  i := m1;
  while i <= m2 do
    begin
      t := C_Mul(v[i - m1 + 1], TAU);
      for i_ := n1 to n2 do
        begin
          c[i, i_] := C_Sub(c[i, i_], C_Mul(t, Work[i_]));
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Application of an elementary reflection to a rectangular matrix of size MxN

  The  algorithm  post-multiplies  the  matrix  by  an elementary reflection
  transformation  which  is  given  by  column  V  and  scalar  Tau (see the
  description  of  the  GenerateReflection). Not the whole matrix but only a
  part  of  it  is  transformed (rows from M1 to M2, columns from N1 to N2).
  Only the elements of this submatrix are changed.

  Input parameters:
  C       -   matrix to be transformed.
  Tau     -   scalar defining transformation.
  V       -   column defining transformation.
  Array whose index ranges within [1..N2-N1+1]
  M1, M2  -   range of rows to be transformed.
  N1, N2  -   range of columns to be transformed.
  WORK    -   working array whose index goes from M1 to M2.

  Output parameters:
  C       -   the result of multiplying the input matrix C by the
  transformation matrix which is given by Tau and V.
  If N1>N2 or M1>M2, C is not modified.
  ************************************************************************ *)
procedure ComplexApplyReflectionFromTheRight(var c: TLComplexMatrix;
  TAU: TLComplex; var v: TLComplexVec; m1: TLInt; m2: TLInt;
  n1: TLInt; n2: TLInt; var Work: TLComplexVec);
var
  t  : TLComplex;
  i  : TLInt;
  VM : TLInt;
  i_ : TLInt;
  i1_: TLInt;
begin
  if C_EqualR(TAU, 0) or (n1 > n2) or (m1 > m2) then
    begin
      Exit;
    end;

  //
  // w := C * v
  //
  VM := n2 - n1 + 1;
  i := m1;
  while i <= m2 do
    begin
      i1_ := (1) - (n1);
      t := C_Complex(0.0);
      for i_ := n1 to n2 do
        begin
          t := C_Add(t, C_Mul(c[i, i_], v[i_ + i1_]));
        end;
      Work[i] := t;
      inc(i);
    end;

  //
  // C := C - w * conj(v^T)
  //
  for i_ := 1 to VM do
    begin
      v[i_] := Conj(v[i_]);
    end;
  i := m1;
  while i <= m2 do
    begin
      t := C_Mul(Work[i], TAU);
      i1_ := (1) - (n1);
      for i_ := n1 to n2 do
        begin
          c[i, i_] := C_Sub(c[i, i_], C_Mul(t, v[i_ + i1_]));
        end;
      inc(i);
    end;
  for i_ := 1 to VM do
    begin
      v[i_] := Conj(v[i_]);
    end;
end;

(* ************************************************************************
  Generation of an elementary reflection transformation

  The subroutine generates elementary reflection H of order N, so that, for
  a given X, the following equality holds true:

  ( X(1) )   ( Beta )
  H * (  ..  ) = (  0   )
  ( X(n) )   (  0   )

  where
  ( V(1) )
  H = 1 - Tau * (  ..  ) * ( V(1), ..., V(n) )
  ( V(n) )

  where the first component of vector V equals 1.

  Input parameters:
  X   -   vector. Array whose index ranges within [1..N].
  N   -   reflection order.

  Output parameters:
  X   -   components from 2 to N are replaced with vector V.
  The first component is replaced with parameter Beta.
  Tau -   scalar value Tau. If X is a null vector, Tau equals 0,
  otherwise 1 <= Tau <= 2.

  This subroutine is the modification of the DLARFG subroutines from
  the LAPACK library.

  MODIFICATIONS:
  24.12.2005 sign(Alpha) was replaced with an analogous to the Fortran SIGN code.
  ************************************************************************ *)
procedure GenerateReflection(var x: TLVec; n: TLInt;
  var TAU: TLFloat);
var
  j    : TLInt;
  alpha: TLFloat;
  XNORM: TLFloat;
  v    : TLFloat;
  beta : TLFloat;
  mx   : TLFloat;
  s    : TLFloat;
begin
  if n <= 1 then
    begin
      TAU := 0;
      Exit;
    end;

  //
  // Scale if needed (to avoid overflow/underflow during intermediate
  // calculations).
  //
  mx := 0;
  j := 1;
  while j <= n do
    begin
      mx := Max(AbsReal(x[j]), mx);
      inc(j);
    end;
  s := 1;
  if AP_FP_NEq(mx, 0) then
    begin
      if AP_FP_Less_Eq(mx, MinRealNumber / MachineEpsilon) then
        begin
          s := MinRealNumber / MachineEpsilon;
          v := 1 / s;
          APVMul(@x[0], 1, n, v);
          mx := mx * v;
        end
      else
        begin
          if AP_FP_Greater_Eq(mx, MaxRealNumber * MachineEpsilon) then
            begin
              s := MaxRealNumber * MachineEpsilon;
              v := 1 / s;
              APVMul(@x[0], 1, n, v);
              mx := mx * v;
            end;
        end;
    end;

  //
  // XNORM = DNRM2( N-1, X, INCX )
  //
  alpha := x[1];
  XNORM := 0;
  if AP_FP_NEq(mx, 0) then
    begin
      j := 2;
      while j <= n do
        begin
          XNORM := XNORM + AP_Sqr(x[j] / mx);
          inc(j);
        end;
      XNORM := Sqrt(XNORM) * mx;
    end;
  if AP_FP_Eq(XNORM, 0) then
    begin

      //
      // H  =  I
      //
      TAU := 0;
      x[1] := x[1] * s;
      Exit;
    end;

  //
  // general case
  //
  mx := Max(AbsReal(alpha), AbsReal(XNORM));
  beta := -mx * Sqrt(AP_Sqr(alpha / mx) + AP_Sqr(XNORM / mx));
  if AP_FP_Less(alpha, 0) then
    begin
      beta := -beta;
    end;
  TAU := (beta - alpha) / beta;
  v := 1 / (alpha - beta);
  APVMul(@x[0], 2, n, v);
  x[1] := beta;

  //
  // Scale back outputs
  //
  x[1] := x[1] * s;
end;

(* ************************************************************************
  Application of an elementary reflection to a rectangular matrix of size MxN

  The algorithm pre-multiplies the matrix by an elementary reflection transformation
  which is given by column V and scalar Tau (see the description of the
  GenerateReflection procedure). Not the whole matrix but only a part of it
  is transformed (rows from M1 to M2, columns from N1 to N2). Only the elements
  of this submatrix are changed.

  Input parameters:
  C       -   matrix to be transformed.
  Tau     -   scalar defining the transformation.
  V       -   column defining the transformation.
  Array whose index ranges within [1..M2-M1+1].
  M1, M2  -   range of rows to be transformed.
  N1, N2  -   range of columns to be transformed.
  WORK    -   working array whose indexes goes from N1 to N2.

  Output parameters:
  C       -   the result of multiplying the input matrix C by the
  transformation matrix which is given by Tau and V.
  If N1>N2 or M1>M2, C is not modified.
  ************************************************************************ *)
procedure ApplyReflectionFromTheLeft(var c: TLMatrix; TAU: TLFloat;
  const v: TLVec; m1: TLInt; m2: TLInt;
  n1: TLInt; n2: TLInt; var Work: TLVec);
var
  t : TLFloat;
  i : TLInt;
  VM: TLInt;
begin
  if AP_FP_Eq(TAU, 0) or (n1 > n2) or (m1 > m2) then
    begin
      Exit;
    end;

  //
  // w := C' * v
  //
  VM := m2 - m1 + 1;
  i := n1;
  while i <= n2 do
    begin
      Work[i] := 0;
      inc(i);
    end;
  i := m1;
  while i <= m2 do
    begin
      t := v[i + 1 - m1];
      APVAdd(@Work[0], n1, n2, @c[i][0], n1, n2, t);
      inc(i);
    end;

  //
  // C := C - tau * v * w'
  //
  i := m1;
  while i <= m2 do
    begin
      t := v[i - m1 + 1] * TAU;
      APVSub(@c[i][0], n1, n2, @Work[0], n1, n2, t);
      inc(i);
    end;
end;

(* ************************************************************************
  Application of an elementary reflection to a rectangular matrix of size MxN

  The algorithm post-multiplies the matrix by an elementary reflection transformation
  which is given by column V and scalar Tau (see the description of the
  GenerateReflection procedure). Not the whole matrix but only a part of it
  is transformed (rows from M1 to M2, columns from N1 to N2). Only the
  elements of this submatrix are changed.

  Input parameters:
  C       -   matrix to be transformed.
  Tau     -   scalar defining the transformation.
  V       -   column defining the transformation.
  Array whose index ranges within [1..N2-N1+1].
  M1, M2  -   range of rows to be transformed.
  N1, N2  -   range of columns to be transformed.
  WORK    -   working array whose indexes goes from M1 to M2.

  Output parameters:
  C       -   the result of multiplying the input matrix C by the
  transformation matrix which is given by Tau and V.
  If N1>N2 or M1>M2, C is not modified.
  ************************************************************************ *)
procedure ApplyReflectionFromTheRight(var c: TLMatrix; TAU: TLFloat;
  const v: TLVec; m1: TLInt; m2: TLInt;
  n1: TLInt; n2: TLInt; var Work: TLVec);
var
  t : TLFloat;
  i : TLInt;
  VM: TLInt;
begin
  if AP_FP_Eq(TAU, 0) or (n1 > n2) or (m1 > m2) then
    begin
      Exit;
    end;
  VM := n2 - n1 + 1;
  i := m1;
  while i <= m2 do
    begin
      t := APVDotProduct(@c[i][0], n1, n2, @v[0], 1, VM);
      t := t * TAU;
      APVSub(@c[i][0], n1, n2, @v[0], 1, VM, t);
      inc(i);
    end;
end;

procedure HermitianMatrixVectorMultiply(const a: TLComplexMatrix;
  IsUpper: Boolean; i1: TLInt; i2: TLInt;
  const x: TLComplexVec; alpha: TLComplex; var y: TLComplexVec);
var
  i  : TLInt;
  BA1: TLInt;
  BA2: TLInt;
  BY1: TLInt;
  BY2: TLInt;
  BX1: TLInt;
  BX2: TLInt;
  n  : TLInt;
  v  : TLComplex;
  i_ : TLInt;
  i1_: TLInt;
begin
  n := i2 - i1 + 1;
  if n <= 0 then
    begin
      Exit;
    end;

  //
  // Let A = L + D + U, where
  // L is strictly lower triangular (main diagonal is zero)
  // D is diagonal
  // U is strictly upper triangular (main diagonal is zero)
  //
  // A*x = L*x + D*x + U*x
  //
  // Calculate D*x first
  //
  i := i1;
  while i <= i2 do
    begin
      y[i - i1 + 1] := C_Mul(a[i, i], x[i - i1 + 1]);
      inc(i);
    end;

  //
  // Add L*x + U*x
  //
  if IsUpper then
    begin
      i := i1;
      while i <= i2 - 1 do
        begin

          //
          // Add L*x to the result
          //
          v := x[i - i1 + 1];
          BY1 := i - i1 + 2;
          BY2 := n;
          BA1 := i + 1;
          BA2 := i2;
          i1_ := (BA1) - (BY1);
          for i_ := BY1 to BY2 do
            begin
              y[i_] := C_Add(y[i_], C_Mul(v, Conj(a[i, i_ + i1_])));
            end;

          //
          // Add U*x to the result
          //
          BX1 := i - i1 + 2;
          BX2 := n;
          BA1 := i + 1;
          BA2 := i2;
          i1_ := (BA1) - (BX1);
          v := C_Complex(0.0);
          for i_ := BX1 to BX2 do
            begin
              v := C_Add(v, C_Mul(x[i_], a[i, i_ + i1_]));
            end;
          y[i - i1 + 1] := C_Add(y[i - i1 + 1], v);
          inc(i);
        end;
    end
  else
    begin
      i := i1 + 1;
      while i <= i2 do
        begin

          //
          // Add L*x to the result
          //
          BX1 := 1;
          BX2 := i - i1;
          BA1 := i1;
          BA2 := i - 1;
          i1_ := (BA1) - (BX1);
          v := C_Complex(0.0);
          for i_ := BX1 to BX2 do
            begin
              v := C_Add(v, C_Mul(x[i_], a[i, i_ + i1_]));
            end;
          y[i - i1 + 1] := C_Add(y[i - i1 + 1], v);

          //
          // Add U*x to the result
          //
          v := x[i - i1 + 1];
          BY1 := 1;
          BY2 := i - i1;
          BA1 := i1;
          BA2 := i - 1;
          i1_ := (BA1) - (BY1);
          for i_ := BY1 to BY2 do
            begin
              y[i_] := C_Add(y[i_], C_Mul(v, Conj(a[i, i_ + i1_])));
            end;
          inc(i);
        end;
    end;
  for i_ := 1 to n do
    begin
      y[i_] := C_Mul(alpha, y[i_]);
    end;
end;

procedure HermitianRank2Update(var a: TLComplexMatrix; IsUpper: Boolean;
  i1: TLInt; i2: TLInt; const x: TLComplexVec;
  const y: TLComplexVec; var t: TLComplexVec; alpha: TLComplex);
var
  i  : TLInt;
  TP1: TLInt;
  TP2: TLInt;
  v  : TLComplex;
  i_ : TLInt;
  i1_: TLInt;
begin
  if IsUpper then
    begin
      i := i1;
      while i <= i2 do
        begin
          TP1 := i + 1 - i1;
          TP2 := i2 - i1 + 1;
          v := C_Mul(alpha, x[i + 1 - i1]);
          for i_ := TP1 to TP2 do
            begin
              t[i_] := C_Mul(v, Conj(y[i_]));
            end;
          v := C_Mul(Conj(alpha), y[i + 1 - i1]);
          for i_ := TP1 to TP2 do
            begin
              t[i_] := C_Add(t[i_], C_Mul(v, Conj(x[i_])));
            end;
          i1_ := (TP1) - (i);
          for i_ := i to i2 do
            begin
              a[i, i_] := C_Add(a[i, i_], t[i_ + i1_]);
            end;
          inc(i);
        end;
    end
  else
    begin
      i := i1;
      while i <= i2 do
        begin
          TP1 := 1;
          TP2 := i + 1 - i1;
          v := C_Mul(alpha, x[i + 1 - i1]);
          for i_ := TP1 to TP2 do
            begin
              t[i_] := C_Mul(v, Conj(y[i_]));
            end;
          v := C_Mul(Conj(alpha), y[i + 1 - i1]);
          for i_ := TP1 to TP2 do
            begin
              t[i_] := C_Add(t[i_], C_Mul(v, Conj(x[i_])));
            end;
          i1_ := (TP1) - (i1);
          for i_ := i1 to i do
            begin
              a[i, i_] := C_Add(a[i, i_], t[i_ + i1_]);
            end;
          inc(i);
        end;
    end;
end;

procedure RMatrixQR(var a: TLMatrix; M: TLInt; n: TLInt; var TAU: TLVec); forward;
procedure RMatrixLQ(var a: TLMatrix; M: TLInt; n: TLInt; var TAU: TLVec); forward;
procedure CMatrixQR(var a: TLComplexMatrix; M: TLInt; n: TLInt; var TAU: TLComplexVec); forward;
procedure CMatrixLQ(var a: TLComplexMatrix; M: TLInt; n: TLInt; var TAU: TLComplexVec); forward;
procedure RMatrixQRUnpackQ(const a: TLMatrix; M: TLInt; n: TLInt; const TAU: TLVec; QColumns: TLInt; var q: TLMatrix); forward;

procedure RMatrixQRUnpackR(const a: TLMatrix; M: TLInt; n: TLInt; var r: TLMatrix); forward;
procedure RMatrixLQUnpackQ(const a: TLMatrix; M: TLInt; n: TLInt; const TAU: TLVec; QRows: TLInt; var q: TLMatrix); forward;

procedure RMatrixLQUnpackL(const a: TLMatrix; M: TLInt; n: TLInt; var L: TLMatrix); forward;
procedure CMatrixQRUnpackQ(const a: TLComplexMatrix; M: TLInt; n: TLInt; const TAU: TLComplexVec; QColumns: TLInt; var q: TLComplexMatrix); forward;
procedure CMatrixQRUnpackR(const a: TLComplexMatrix; M: TLInt; n: TLInt; var r: TLComplexMatrix); forward;
procedure CMatrixLQUnpackQ(const a: TLComplexMatrix; M: TLInt; n: TLInt; const TAU: TLComplexVec; QRows: TLInt; var q: TLComplexMatrix); forward;
procedure CMatrixLQUnpackL(const a: TLComplexMatrix; M: TLInt; n: TLInt; var L: TLComplexMatrix); forward;
procedure RMatrixBD(var a: TLMatrix; M: TLInt; n: TLInt; var TauQ: TLVec; var TauP: TLVec); forward;
procedure RMatrixBDUnpackQ(const qp: TLMatrix; M: TLInt; n: TLInt; const TauQ: TLVec; QColumns: TLInt; var q: TLMatrix); forward;

procedure RMatrixBDMultiplyByQ(const qp: TLMatrix; M: TLInt;
  n: TLInt; const TauQ: TLVec; var z: TLMatrix;
  ZRows: TLInt; ZColumns: TLInt; FromTheRight: Boolean; DoTranspose: Boolean); forward;

procedure RMatrixBDUnpackPT(const qp: TLMatrix; M: TLInt;
  n: TLInt; const TauP: TLVec; PTRows: TLInt; var pt: TLMatrix); forward;

procedure RMatrixBDMultiplyByP(const qp: TLMatrix; M: TLInt;
  n: TLInt; const TauP: TLVec; var z: TLMatrix;
  ZRows: TLInt; ZColumns: TLInt; FromTheRight: Boolean; DoTranspose: Boolean); forward;

procedure RMatrixBDUnpackDiagonals(const b: TLMatrix; M: TLInt;
  n: TLInt; var IsUpper: Boolean; var d: TLVec; var E: TLVec); forward;

procedure RMatrixHessenberg(var a: TLMatrix; n: TLInt; var TAU: TLVec); forward;

procedure RMatrixHessenbergUnpackQ(const a: TLMatrix; n: TLInt; const TAU: TLVec; var q: TLMatrix); forward;
procedure RMatrixHessenbergUnpackH(const a: TLMatrix; n: TLInt; var h: TLMatrix); forward;
procedure SMatrixTD(var a: TLMatrix; n: TLInt; IsUpper: Boolean; var TAU: TLVec; var d: TLVec; var E: TLVec); forward;
procedure SMatrixTDUnpackQ(const a: TLMatrix; const n: TLInt; const IsUpper: Boolean; const TAU: TLVec; var q: TLMatrix); forward;
procedure HMatrixTD(var a: TLComplexMatrix; n: TLInt; IsUpper: Boolean; var TAU: TLComplexVec; var d: TLVec; var E: TLVec); forward;
procedure HMatrixTDUnpackQ(const a: TLComplexMatrix; const n: TLInt; const IsUpper: Boolean; const TAU: TLComplexVec; var q: TLComplexMatrix); forward;

procedure RMatrixQRBaseCase(var a: TLMatrix; M: TLInt;
  n: TLInt; var Work: TLVec; var t: TLVec;
  var TAU: TLVec); forward;
procedure RMatrixLQBaseCase(var a: TLMatrix; M: TLInt;
  n: TLInt; var Work: TLVec; var t: TLVec;
  var TAU: TLVec); forward;
procedure CMatrixQRBaseCase(var a: TLComplexMatrix; M: TLInt;
  n: TLInt; var Work: TLComplexVec; var t: TLComplexVec;
  var TAU: TLComplexVec); forward;
procedure CMatrixLQBaseCase(var a: TLComplexMatrix; M: TLInt;
  n: TLInt; var Work: TLComplexVec; var t: TLComplexVec;
  var TAU: TLComplexVec); forward;
procedure RMatrixBlockReflector(var a: TLMatrix; var TAU: TLVec;
  ColumnwiseA: Boolean; LengthA: TLInt; BlockSize: TLInt;
  var t: TLMatrix; var Work: TLVec); forward;
procedure CMatrixBlockReflector(var a: TLComplexMatrix;
  var TAU: TLComplexVec; ColumnwiseA: Boolean; LengthA: TLInt;
  BlockSize: TLInt; var t: TLComplexMatrix;
  var Work: TLComplexVec); forward;


(* ************************************************************************
  QR decomposition of a rectangular matrix of size MxN

  Input parameters:
  A   -   matrix A whose indexes range within [0..M-1, 0..N-1].
  M   -   number of rows in matrix A.
  N   -   number of columns in matrix A.

  Output parameters:
  A   -   matrices Q and R in compact form (see below).
  Tau -   array of scalar factors which are used to form
  matrix Q. Array whose index ranges within [0.. Min(M-1,N-1)].

  Matrix A is represented as A = QR, where Q is an orthogonal matrix of size
  MxM, R - upper triangular (or upper trapezoid) matrix of size M x N.

  The elements of matrix R are located on and above the main diagonal of
  matrix A. The elements which are located in Tau array and below the main
  diagonal of matrix A are used to form matrix Q as follows:

  Matrix Q is represented as a product of elementary reflections

  Q = H(0)*H(2)*...*H(k-1),

  where k = min(m,n), and each H(i) is in the form

  H(i) = 1 - tau * v * (v^T)

  where tau is a scalar stored in Tau[I]; v - real vector,
  so that v(0:i-1) = 0, v(i) = 1, v(i+1:m-1) stored in A(i+1:m-1,i).
  ************************************************************************ *)
procedure RMatrixQR(var a: TLMatrix; M: TLInt; n: TLInt;
  var TAU: TLVec);
var
  Work      : TLVec;
  t         : TLVec;
  TauBuf    : TLVec;
  MinMN     : TLInt;
  TmpA      : TLMatrix;
  TmpT      : TLMatrix;
  TmpR      : TLMatrix;
  BlockStart: TLInt;
  BlockSize : TLInt;
  RowsCount : TLInt;
  i         : TLInt;
  j         : TLInt;
  k         : TLInt;
  v         : TLFloat;
  i_        : TLInt;
  i1_       : TLInt;
begin
  if (M <= 0) or (n <= 0) then
    begin
      Exit;
    end;
  MinMN := Min(M, n);
  SetLength(Work, Max(M, n) + 1);
  SetLength(t, Max(M, n) + 1);
  SetLength(TAU, MinMN);
  SetLength(TauBuf, MinMN);
  SetLength(TmpA, M, ABLASBlockSize(a));
  SetLength(TmpT, ABLASBlockSize(a), 2 * ABLASBlockSize(a));
  SetLength(TmpR, 2 * ABLASBlockSize(a), n);

  //
  // Blocked code
  //
  BlockStart := 0;
  while BlockStart <> MinMN do
    begin

      //
      // Determine block size
      //
      BlockSize := MinMN - BlockStart;
      if BlockSize > ABLASBlockSize(a) then
        begin
          BlockSize := ABLASBlockSize(a);
        end;
      RowsCount := M - BlockStart;

      //
      // QR decomposition of submatrix.
      // Matrix is copied to temporary storage to solve
      // some TLB issues arising from non-contiguous memory
      // access pattern.
      //
      RMatrixCopy(RowsCount, BlockSize, a, BlockStart, BlockStart, TmpA, 0, 0);
      RMatrixQRBaseCase(TmpA, RowsCount, BlockSize, Work, t, TauBuf);
      RMatrixCopy(RowsCount, BlockSize, TmpA, 0, 0, a, BlockStart, BlockStart);
      APVMove(@TAU[0], BlockStart, BlockStart + BlockSize - 1, @TauBuf[0], 0,
        BlockSize - 1);

      //
      // Update the rest, choose between:
      // a) Level 2 algorithm (when the rest of the matrix is small enough)
      // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
      // representation for products of Householder transformations',
      // by R. Schreiber and C. Van Loan.
      //
      if BlockStart + BlockSize <= n - 1 then
        begin
          if (n - BlockStart - BlockSize >= 2 * ABLASBlockSize(a)) or
            (RowsCount >= 4 * ABLASBlockSize(a)) then
            begin

              //
              // Prepare block reflector
              //
              RMatrixBlockReflector(TmpA, TauBuf, True, RowsCount, BlockSize,
                TmpT, Work);

              //
              // Multiply the rest of A by Q'.
              //
              // Q  = E + Y*T*Y'  = E + TmpA*TmpT*TmpA'
              // Q' = E + Y*T'*Y' = E + TmpA*TmpT'*TmpA'
              //
              RMatrixGEMM(BlockSize, n - BlockStart - BlockSize, RowsCount, 1.0, TmpA,
                0, 0, 1, a, BlockStart, BlockStart + BlockSize, 0, 0.0, TmpR, 0, 0);
              RMatrixGEMM(BlockSize, n - BlockStart - BlockSize, BlockSize, 1.0, TmpT,
                0, 0, 1, TmpR, 0, 0, 0, 0.0, TmpR, BlockSize, 0);
              RMatrixGEMM(RowsCount, n - BlockStart - BlockSize, BlockSize, 1.0, TmpA,
                0, 0, 0, TmpR, BlockSize, 0, 0, 1.0, a, BlockStart,
                BlockStart + BlockSize);
            end
          else
            begin

              //
              // Level 2 algorithm
              //
              i := 0;
              while i <= BlockSize - 1 do
                begin
                  i1_ := (i) - (1);
                  for i_ := 1 to RowsCount - i do
                    begin
                      t[i_] := TmpA[i_ + i1_, i];
                    end;
                  t[1] := 1;
                  ApplyReflectionFromTheLeft(a, TauBuf[i], t, BlockStart + i, M - 1,
                    BlockStart + BlockSize, n - 1, Work);
                  inc(i);
                end;
            end;
        end;

      //
      // Advance
      //
      BlockStart := BlockStart + BlockSize;
    end;
end;

(* ************************************************************************
  LQ decomposition of a rectangular matrix of size MxN

  Input parameters:
  A   -   matrix A whose indexes range within [0..M-1, 0..N-1].
  M   -   number of rows in matrix A.
  N   -   number of columns in matrix A.

  Output parameters:
  A   -   matrices L and Q in compact form (see below)
  Tau -   array of scalar factors which are used to form
  matrix Q. Array whose index ranges within [0..Min(M,N)-1].

  Matrix A is represented as A = LQ, where Q is an orthogonal matrix of size
  MxM, L - lower triangular (or lower trapezoid) matrix of size M x N.

  The elements of matrix L are located on and below  the  main  diagonal  of
  matrix A. The elements which are located in Tau array and above  the  main
  diagonal of matrix A are used to form matrix Q as follows:

  Matrix Q is represented as a product of elementary reflections

  Q = H(k-1)*H(k-2)*...*H(1)*H(0),

  where k = min(m,n), and each H(i) is of the form

  H(i) = 1 - tau * v * (v^T)

  where tau is a scalar stored in Tau[I]; v - real vector, so that v(0:i-1)=0,
  v(i) = 1, v(i+1:n-1) stored in A(i,i+1:n-1).
  ************************************************************************ *)
procedure RMatrixLQ(var a: TLMatrix; M: TLInt; n: TLInt;
  var TAU: TLVec);
var
  Work        : TLVec;
  t           : TLVec;
  TauBuf      : TLVec;
  MinMN       : TLInt;
  TmpA        : TLMatrix;
  TmpT        : TLMatrix;
  TmpR        : TLMatrix;
  BlockStart  : TLInt;
  BlockSize   : TLInt;
  ColumnsCount: TLInt;
  i           : TLInt;
  j           : TLInt;
  k           : TLInt;
  v           : TLFloat;
begin
  if (M <= 0) or (n <= 0) then
    begin
      Exit;
    end;
  MinMN := Min(M, n);
  SetLength(Work, Max(M, n) + 1);
  SetLength(t, Max(M, n) + 1);
  SetLength(TAU, MinMN);
  SetLength(TauBuf, MinMN);
  SetLength(TmpA, ABLASBlockSize(a), n);
  SetLength(TmpT, ABLASBlockSize(a), 2 * ABLASBlockSize(a));
  SetLength(TmpR, M, 2 * ABLASBlockSize(a));

  //
  // Blocked code
  //
  BlockStart := 0;
  while BlockStart <> MinMN do
    begin

      //
      // Determine block size
      //
      BlockSize := MinMN - BlockStart;
      if BlockSize > ABLASBlockSize(a) then
        begin
          BlockSize := ABLASBlockSize(a);
        end;
      ColumnsCount := n - BlockStart;

      //
      // LQ decomposition of submatrix.
      // Matrix is copied to temporary storage to solve
      // some TLB issues arising from non-contiguous memory
      // access pattern.
      //
      RMatrixCopy(BlockSize, ColumnsCount, a, BlockStart, BlockStart, TmpA, 0, 0);
      RMatrixLQBaseCase(TmpA, BlockSize, ColumnsCount, Work, t, TauBuf);
      RMatrixCopy(BlockSize, ColumnsCount, TmpA, 0, 0, a, BlockStart, BlockStart);
      APVMove(@TAU[0], BlockStart, BlockStart + BlockSize - 1, @TauBuf[0], 0,
        BlockSize - 1);

      //
      // Update the rest, choose between:
      // a) Level 2 algorithm (when the rest of the matrix is small enough)
      // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
      // representation for products of Householder transformations',
      // by R. Schreiber and C. Van Loan.
      //
      if BlockStart + BlockSize <= M - 1 then
        begin
          if M - BlockStart - BlockSize >= 2 * ABLASBlockSize(a) then
            begin

              //
              // Prepare block reflector
              //
              RMatrixBlockReflector(TmpA, TauBuf, False, ColumnsCount, BlockSize,
                TmpT, Work);

              //
              // Multiply the rest of A by Q.
              //
              // Q  = E + Y*T*Y'  = E + TmpA'*TmpT*TmpA
              //
              RMatrixGEMM(M - BlockStart - BlockSize, BlockSize, ColumnsCount, 1.0, a,
                BlockStart + BlockSize, BlockStart, 0, TmpA, 0, 0, 1, 0.0,
                TmpR, 0, 0);
              RMatrixGEMM(M - BlockStart - BlockSize, BlockSize, BlockSize, 1.0, TmpR,
                0, 0, 0, TmpT, 0, 0, 0, 0.0, TmpR, 0, BlockSize);
              RMatrixGEMM(M - BlockStart - BlockSize, ColumnsCount, BlockSize, 1.0,
                TmpR, 0, BlockSize, 0, TmpA, 0, 0, 0, 1.0, a, BlockStart + BlockSize,
                BlockStart);
            end
          else
            begin

              //
              // Level 2 algorithm
              //
              i := 0;
              while i <= BlockSize - 1 do
                begin
                  APVMove(@t[0], 1, ColumnsCount - i, @TmpA[i][0], i, ColumnsCount - 1);
                  t[1] := 1;
                  ApplyReflectionFromTheRight(a, TauBuf[i], t, BlockStart + BlockSize,
                    M - 1, BlockStart + i, n - 1, Work);
                  inc(i);
                end;
            end;
        end;

      //
      // Advance
      //
      BlockStart := BlockStart + BlockSize;
    end;
end;

(* ************************************************************************
  QR decomposition of a rectangular TLComplex matrix of size MxN

  Input parameters:
  A   -   matrix A whose indexes range within [0..M-1, 0..N-1]
  M   -   number of rows in matrix A.
  N   -   number of columns in matrix A.

  Output parameters:
  A   -   matrices Q and R in compact form
  Tau -   array of scalar factors which are used to form matrix Q. Array
  whose indexes range within [0.. Min(M,N)-1]

  Matrix A is represented as A = QR, where Q is an orthogonal matrix of size
  MxM, R - upper triangular (or upper trapezoid) matrix of size MxN.
  ************************************************************************ *)
procedure CMatrixQR(var a: TLComplexMatrix; M: TLInt; n: TLInt;
  var TAU: TLComplexVec);
var
  Work      : TLComplexVec;
  t         : TLComplexVec;
  TauBuf    : TLComplexVec;
  MinMN     : TLInt;
  TmpA      : TLComplexMatrix;
  TmpT      : TLComplexMatrix;
  TmpR      : TLComplexMatrix;
  BlockStart: TLInt;
  BlockSize : TLInt;
  RowsCount : TLInt;
  i         : TLInt;
  j         : TLInt;
  k         : TLInt;
  v         : TLComplex;
  i_        : TLInt;
  i1_       : TLInt;
begin
  if (M <= 0) or (n <= 0) then
    begin
      Exit;
    end;
  MinMN := Min(M, n);
  SetLength(Work, Max(M, n) + 1);
  SetLength(t, Max(M, n) + 1);
  SetLength(TAU, MinMN);
  SetLength(TauBuf, MinMN);
  SetLength(TmpA, M, ABLASComplexBlockSize(a));
  SetLength(TmpT, ABLASComplexBlockSize(a), ABLASComplexBlockSize(a));
  SetLength(TmpR, 2 * ABLASComplexBlockSize(a), n);

  //
  // Blocked code
  //
  BlockStart := 0;
  while BlockStart <> MinMN do
    begin

      //
      // Determine block size
      //
      BlockSize := MinMN - BlockStart;
      if BlockSize > ABLASComplexBlockSize(a) then
        begin
          BlockSize := ABLASComplexBlockSize(a);
        end;
      RowsCount := M - BlockStart;

      //
      // QR decomposition of submatrix.
      // Matrix is copied to temporary storage to solve
      // some TLB issues arising from non-contiguous memory
      // access pattern.
      //
      CMatrixCopy(RowsCount, BlockSize, a, BlockStart, BlockStart, TmpA, 0, 0);
      CMatrixQRBaseCase(TmpA, RowsCount, BlockSize, Work, t, TauBuf);
      CMatrixCopy(RowsCount, BlockSize, TmpA, 0, 0, a, BlockStart, BlockStart);
      i1_ := (0) - (BlockStart);
      for i_ := BlockStart to BlockStart + BlockSize - 1 do
        begin
          TAU[i_] := TauBuf[i_ + i1_];
        end;

      //
      // Update the rest, choose between:
      // a) Level 2 algorithm (when the rest of the matrix is small enough)
      // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
      // representation for products of Householder transformations',
      // by R. Schreiber and C. Van Loan.
      //
      if BlockStart + BlockSize <= n - 1 then
        begin
          if n - BlockStart - BlockSize >= 2 * ABLASComplexBlockSize(a) then
            begin

              //
              // Prepare block reflector
              //
              CMatrixBlockReflector(TmpA, TauBuf, True, RowsCount, BlockSize,
                TmpT, Work);

              //
              // Multiply the rest of A by Q'.
              //
              // Q  = E + Y*T*Y'  = E + TmpA*TmpT*TmpA'
              // Q' = E + Y*T'*Y' = E + TmpA*TmpT'*TmpA'
              //
              CMatrixGEMM(BlockSize, n - BlockStart - BlockSize, RowsCount,
                C_Complex(1.0), TmpA, 0, 0, 2, a, BlockStart, BlockStart + BlockSize,
                0, C_Complex(0.0), TmpR, 0, 0);
              CMatrixGEMM(BlockSize, n - BlockStart - BlockSize, BlockSize,
                C_Complex(1.0), TmpT, 0, 0, 2, TmpR, 0, 0, 0, C_Complex(0.0), TmpR,
                BlockSize, 0);
              CMatrixGEMM(RowsCount, n - BlockStart - BlockSize, BlockSize,
                C_Complex(1.0), TmpA, 0, 0, 0, TmpR, BlockSize, 0, 0, C_Complex(1.0),
                a, BlockStart, BlockStart + BlockSize);
            end
          else
            begin

              //
              // Level 2 algorithm
              //
              i := 0;
              while i <= BlockSize - 1 do
                begin
                  i1_ := (i) - (1);
                  for i_ := 1 to RowsCount - i do
                    begin
                      t[i_] := TmpA[i_ + i1_, i];
                    end;
                  t[1] := C_Complex(1);
                  ComplexApplyReflectionFromTheLeft(a, Conj(TauBuf[i]), t,
                    BlockStart + i, M - 1, BlockStart + BlockSize, n - 1, Work);
                  inc(i);
                end;
            end;
        end;

      //
      // Advance
      //
      BlockStart := BlockStart + BlockSize;
    end;
end;

(* ************************************************************************
  LQ decomposition of a rectangular TLComplex matrix of size MxN

  Input parameters:
  A   -   matrix A whose indexes range within [0..M-1, 0..N-1]
  M   -   number of rows in matrix A.
  N   -   number of columns in matrix A.

  Output parameters:
  A   -   matrices Q and L in compact form
  Tau -   array of scalar factors which are used to form matrix Q. Array
  whose indexes range within [0.. Min(M,N)-1]

  Matrix A is represented as A = LQ, where Q is an orthogonal matrix of size
  MxM, L - lower triangular (or lower trapezoid) matrix of size MxN.
  ************************************************************************ *)
procedure CMatrixLQ(var a: TLComplexMatrix; M: TLInt; n: TLInt;
  var TAU: TLComplexVec);
var
  Work        : TLComplexVec;
  t           : TLComplexVec;
  TauBuf      : TLComplexVec;
  MinMN       : TLInt;
  TmpA        : TLComplexMatrix;
  TmpT        : TLComplexMatrix;
  TmpR        : TLComplexMatrix;
  BlockStart  : TLInt;
  BlockSize   : TLInt;
  ColumnsCount: TLInt;
  i           : TLInt;
  j           : TLInt;
  k           : TLInt;
  v           : TLComplex;
  i_          : TLInt;
  i1_         : TLInt;
begin
  if (M <= 0) or (n <= 0) then
    begin
      Exit;
    end;
  MinMN := Min(M, n);
  SetLength(Work, Max(M, n) + 1);
  SetLength(t, Max(M, n) + 1);
  SetLength(TAU, MinMN);
  SetLength(TauBuf, MinMN);
  SetLength(TmpA, ABLASComplexBlockSize(a), n);
  SetLength(TmpT, ABLASComplexBlockSize(a), ABLASComplexBlockSize(a));
  SetLength(TmpR, M, 2 * ABLASComplexBlockSize(a));

  //
  // Blocked code
  //
  BlockStart := 0;
  while BlockStart <> MinMN do
    begin

      //
      // Determine block size
      //
      BlockSize := MinMN - BlockStart;
      if BlockSize > ABLASComplexBlockSize(a) then
        begin
          BlockSize := ABLASComplexBlockSize(a);
        end;
      ColumnsCount := n - BlockStart;

      //
      // LQ decomposition of submatrix.
      // Matrix is copied to temporary storage to solve
      // some TLB issues arising from non-contiguous memory
      // access pattern.
      //
      CMatrixCopy(BlockSize, ColumnsCount, a, BlockStart, BlockStart, TmpA, 0, 0);
      CMatrixLQBaseCase(TmpA, BlockSize, ColumnsCount, Work, t, TauBuf);
      CMatrixCopy(BlockSize, ColumnsCount, TmpA, 0, 0, a, BlockStart, BlockStart);
      i1_ := (0) - (BlockStart);
      for i_ := BlockStart to BlockStart + BlockSize - 1 do
        begin
          TAU[i_] := TauBuf[i_ + i1_];
        end;

      //
      // Update the rest, choose between:
      // a) Level 2 algorithm (when the rest of the matrix is small enough)
      // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
      // representation for products of Householder transformations',
      // by R. Schreiber and C. Van Loan.
      //
      if BlockStart + BlockSize <= M - 1 then
        begin
          if M - BlockStart - BlockSize >= 2 * ABLASComplexBlockSize(a) then
            begin

              //
              // Prepare block reflector
              //
              CMatrixBlockReflector(TmpA, TauBuf, False, ColumnsCount, BlockSize,
                TmpT, Work);

              //
              // Multiply the rest of A by Q.
              //
              // Q  = E + Y*T*Y'  = E + TmpA'*TmpT*TmpA
              //
              CMatrixGEMM(M - BlockStart - BlockSize, BlockSize, ColumnsCount,
                C_Complex(1.0), a, BlockStart + BlockSize, BlockStart, 0, TmpA, 0, 0,
                2, C_Complex(0.0), TmpR, 0, 0);
              CMatrixGEMM(M - BlockStart - BlockSize, BlockSize, BlockSize,
                C_Complex(1.0), TmpR, 0, 0, 0, TmpT, 0, 0, 0, C_Complex(0.0), TmpR, 0,
                BlockSize);
              CMatrixGEMM(M - BlockStart - BlockSize, ColumnsCount, BlockSize,
                C_Complex(1.0), TmpR, 0, BlockSize, 0, TmpA, 0, 0, 0, C_Complex(1.0),
                a, BlockStart + BlockSize, BlockStart);
            end
          else
            begin

              //
              // Level 2 algorithm
              //
              i := 0;
              while i <= BlockSize - 1 do
                begin
                  i1_ := (i) - (1);
                  for i_ := 1 to ColumnsCount - i do
                    begin
                      t[i_] := Conj(TmpA[i, i_ + i1_]);
                    end;
                  t[1] := C_Complex(1);
                  ComplexApplyReflectionFromTheRight(a, TauBuf[i], t,
                    BlockStart + BlockSize, M - 1, BlockStart + i, n - 1, Work);
                  inc(i);
                end;
            end;
        end;

      //
      // Advance
      //
      BlockStart := BlockStart + BlockSize;
    end;
end;

(* ************************************************************************
  Partial unpacking of matrix Q from the QR decomposition of a matrix A

  Input parameters:
  A       -   matrices Q and R in compact form.
  Output of RMatrixQR subroutine.
  M       -   number of rows in given matrix A. M>=0.
  N       -   number of columns in given matrix A. N>=0.
  Tau     -   scalar factors which are used to form Q.
  Output of the RMatrixQR subroutine.
  QColumns -  required number of columns of matrix Q. M>=QColumns>=0.

  Output parameters:
  Q       -   first QColumns columns of matrix Q.
  Array whose indexes range within [0..M-1, 0..QColumns-1].
  If QColumns=0, the array remains unchanged.
  ************************************************************************ *)
procedure RMatrixQRUnpackQ(const a: TLMatrix; M: TLInt;
  n: TLInt; const TAU: TLVec; QColumns: TLInt;
  var q: TLMatrix);
var
  Work      : TLVec;
  t         : TLVec;
  TauBuf    : TLVec;
  MinMN     : TLInt;
  RefCnt    : TLInt;
  TmpA      : TLMatrix;
  TmpT      : TLMatrix;
  TmpR      : TLMatrix;
  BlockStart: TLInt;
  BlockSize : TLInt;
  RowsCount : TLInt;
  i         : TLInt;
  j         : TLInt;
  k         : TLInt;
  v         : TLFloat;
  i_        : TLInt;
  i1_       : TLInt;
begin
  Assert(QColumns <= M, 'UnpackQFromQR: QColumns>M!');
  if (M <= 0) or (n <= 0) or (QColumns <= 0) then
    begin
      Exit;
    end;

  //
  // init
  //
  MinMN := Min(M, n);
  RefCnt := Min(MinMN, QColumns);
  SetLength(q, M, QColumns);
  i := 0;
  while i <= M - 1 do
    begin
      j := 0;
      while j <= QColumns - 1 do
        begin
          if i = j then
            begin
              q[i, j] := 1;
            end
          else
            begin
              q[i, j] := 0;
            end;
          inc(j);
        end;
      inc(i);
    end;
  SetLength(Work, Max(M, QColumns) + 1);
  SetLength(t, Max(M, QColumns) + 1);
  SetLength(TauBuf, MinMN);
  SetLength(TmpA, M, ABLASBlockSize(a));
  SetLength(TmpT, ABLASBlockSize(a), 2 * ABLASBlockSize(a));
  SetLength(TmpR, 2 * ABLASBlockSize(a), QColumns);

  //
  // Blocked code
  //
  BlockStart := ABLASBlockSize(a) * (RefCnt div ABLASBlockSize(a));
  BlockSize := RefCnt - BlockStart;
  while BlockStart >= 0 do
    begin
      RowsCount := M - BlockStart;

      //
      // Copy current block
      //
      RMatrixCopy(RowsCount, BlockSize, a, BlockStart, BlockStart, TmpA, 0, 0);
      APVMove(@TauBuf[0], 0, BlockSize - 1, @TAU[0], BlockStart,
        BlockStart + BlockSize - 1);

      //
      // Update, choose between:
      // a) Level 2 algorithm (when the rest of the matrix is small enough)
      // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
      // representation for products of Householder transformations',
      // by R. Schreiber and C. Van Loan.
      //
      if QColumns >= 2 * ABLASBlockSize(a) then
        begin

          //
          // Prepare block reflector
          //
          RMatrixBlockReflector(TmpA, TauBuf, True, RowsCount, BlockSize,
            TmpT, Work);

          //
          // Multiply matrix by Q.
          //
          // Q  = E + Y*T*Y'  = E + TmpA*TmpT*TmpA'
          //
          RMatrixGEMM(BlockSize, QColumns, RowsCount, 1.0, TmpA, 0, 0, 1, q,
            BlockStart, 0, 0, 0.0, TmpR, 0, 0);
          RMatrixGEMM(BlockSize, QColumns, BlockSize, 1.0, TmpT, 0, 0, 0, TmpR, 0,
            0, 0, 0.0, TmpR, BlockSize, 0);
          RMatrixGEMM(RowsCount, QColumns, BlockSize, 1.0, TmpA, 0, 0, 0, TmpR,
            BlockSize, 0, 0, 1.0, q, BlockStart, 0);
        end
      else
        begin

          //
          // Level 2 algorithm
          //
          i := BlockSize - 1;
          while i >= 0 do
            begin
              i1_ := (i) - (1);
              for i_ := 1 to RowsCount - i do
                begin
                  t[i_] := TmpA[i_ + i1_, i];
                end;
              t[1] := 1;
              ApplyReflectionFromTheLeft(q, TauBuf[i], t, BlockStart + i, M - 1, 0,
                QColumns - 1, Work);
              dec(i);
            end;
        end;

      //
      // Advance
      //
      BlockStart := BlockStart - ABLASBlockSize(a);
      BlockSize := ABLASBlockSize(a);
    end;
end;

(* ************************************************************************
  Unpacking of matrix R from the QR decomposition of a matrix A

  Input parameters:
  A       -   matrices Q and R in compact form.
  Output of RMatrixQR subroutine.
  M       -   number of rows in given matrix A. M>=0.
  N       -   number of columns in given matrix A. N>=0.

  Output parameters:
  R       -   matrix R, array[0..M-1, 0..N-1].
  ************************************************************************ *)
procedure RMatrixQRUnpackR(const a: TLMatrix; M: TLInt;
  n: TLInt; var r: TLMatrix);
var
  i: TLInt;
  k: TLInt;
begin
  if (M <= 0) or (n <= 0) then
    begin
      Exit;
    end;
  k := Min(M, n);
  SetLength(r, M, n);
  i := 0;
  while i <= n - 1 do
    begin
      r[0, i] := 0;
      inc(i);
    end;
  i := 1;
  while i <= M - 1 do
    begin
      APVMove(@r[i][0], 0, n - 1, @r[0][0], 0, n - 1);
      inc(i);
    end;
  i := 0;
  while i <= k - 1 do
    begin
      APVMove(@r[i][0], i, n - 1, @a[i][0], i, n - 1);
      inc(i);
    end;
end;

(* ************************************************************************
  Partial unpacking of matrix Q from the LQ decomposition of a matrix A

  Input parameters:
  A       -   matrices L and Q in compact form.
  Output of RMatrixLQ subroutine.
  M       -   number of rows in given matrix A. M>=0.
  N       -   number of columns in given matrix A. N>=0.
  Tau     -   scalar factors which are used to form Q.
  Output of the RMatrixLQ subroutine.
  QRows   -   required number of rows in matrix Q. N>=QRows>=0.

  Output parameters:
  Q       -   first QRows rows of matrix Q. Array whose indexes range
  within [0..QRows-1, 0..N-1]. If QRows=0, the array remains
  unchanged.
  ************************************************************************ *)
procedure RMatrixLQUnpackQ(const a: TLMatrix; M: TLInt;
  n: TLInt; const TAU: TLVec; QRows: TLInt;
  var q: TLMatrix);
var
  Work        : TLVec;
  t           : TLVec;
  TauBuf      : TLVec;
  MinMN       : TLInt;
  RefCnt      : TLInt;
  TmpA        : TLMatrix;
  TmpT        : TLMatrix;
  TmpR        : TLMatrix;
  BlockStart  : TLInt;
  BlockSize   : TLInt;
  ColumnsCount: TLInt;
  i           : TLInt;
  j           : TLInt;
  k           : TLInt;
  v           : TLFloat;
begin
  Assert(QRows <= n, 'RMatrixLQUnpackQ: QRows>N!');
  if (M <= 0) or (n <= 0) or (QRows <= 0) then
    begin
      Exit;
    end;

  //
  // init
  //
  MinMN := Min(M, n);
  RefCnt := Min(MinMN, QRows);
  SetLength(Work, Max(M, n) + 1);
  SetLength(t, Max(M, n) + 1);
  SetLength(TauBuf, MinMN);
  SetLength(TmpA, ABLASBlockSize(a), n);
  SetLength(TmpT, ABLASBlockSize(a), 2 * ABLASBlockSize(a));
  SetLength(TmpR, QRows, 2 * ABLASBlockSize(a));
  SetLength(q, QRows, n);
  i := 0;
  while i <= QRows - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          if i = j then
            begin
              q[i, j] := 1;
            end
          else
            begin
              q[i, j] := 0;
            end;
          inc(j);
        end;
      inc(i);
    end;

  //
  // Blocked code
  //
  BlockStart := ABLASBlockSize(a) * (RefCnt div ABLASBlockSize(a));
  BlockSize := RefCnt - BlockStart;
  while BlockStart >= 0 do
    begin
      ColumnsCount := n - BlockStart;

      //
      // Copy submatrix
      //
      RMatrixCopy(BlockSize, ColumnsCount, a, BlockStart, BlockStart, TmpA, 0, 0);
      APVMove(@TauBuf[0], 0, BlockSize - 1, @TAU[0], BlockStart,
        BlockStart + BlockSize - 1);

      //
      // Update matrix, choose between:
      // a) Level 2 algorithm (when the rest of the matrix is small enough)
      // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
      // representation for products of Householder transformations',
      // by R. Schreiber and C. Van Loan.
      //
      if QRows >= 2 * ABLASBlockSize(a) then
        begin

          //
          // Prepare block reflector
          //
          RMatrixBlockReflector(TmpA, TauBuf, False, ColumnsCount, BlockSize,
            TmpT, Work);

          //
          // Multiply the rest of A by Q'.
          //
          // Q'  = E + Y*T'*Y'  = E + TmpA'*TmpT'*TmpA
          //
          RMatrixGEMM(QRows, BlockSize, ColumnsCount, 1.0, q, 0, BlockStart, 0,
            TmpA, 0, 0, 1, 0.0, TmpR, 0, 0);
          RMatrixGEMM(QRows, BlockSize, BlockSize, 1.0, TmpR, 0, 0, 0, TmpT, 0, 0,
            1, 0.0, TmpR, 0, BlockSize);
          RMatrixGEMM(QRows, ColumnsCount, BlockSize, 1.0, TmpR, 0, BlockSize, 0,
            TmpA, 0, 0, 0, 1.0, q, 0, BlockStart);
        end
      else
        begin

          //
          // Level 2 algorithm
          //
          i := BlockSize - 1;
          while i >= 0 do
            begin
              APVMove(@t[0], 1, ColumnsCount - i, @TmpA[i][0], i, ColumnsCount - 1);
              t[1] := 1;
              ApplyReflectionFromTheRight(q, TauBuf[i], t, 0, QRows - 1,
                BlockStart + i, n - 1, Work);
              dec(i);
            end;
        end;

      //
      // Advance
      //
      BlockStart := BlockStart - ABLASBlockSize(a);
      BlockSize := ABLASBlockSize(a);
    end;
end;

(* ************************************************************************
  Unpacking of matrix L from the LQ decomposition of a matrix A

  Input parameters:
  A       -   matrices Q and L in compact form.
  Output of RMatrixLQ subroutine.
  M       -   number of rows in given matrix A. M>=0.
  N       -   number of columns in given matrix A. N>=0.

  Output parameters:
  L       -   matrix L, array[0..M-1, 0..N-1].
  ************************************************************************ *)
procedure RMatrixLQUnpackL(const a: TLMatrix; M: TLInt;
  n: TLInt; var L: TLMatrix);
var
  i: TLInt;
  k: TLInt;
begin
  if (M <= 0) or (n <= 0) then
    begin
      Exit;
    end;
  SetLength(L, M, n);
  i := 0;
  while i <= n - 1 do
    begin
      L[0, i] := 0;
      inc(i);
    end;
  i := 1;
  while i <= M - 1 do
    begin
      APVMove(@L[i][0], 0, n - 1, @L[0][0], 0, n - 1);
      inc(i);
    end;
  i := 0;
  while i <= M - 1 do
    begin
      k := Min(i, n - 1);
      APVMove(@L[i][0], 0, k, @a[i][0], 0, k);
      inc(i);
    end;
end;

(* ************************************************************************
  Partial unpacking of matrix Q from QR decomposition of a TLComplex matrix A.

  Input parameters:
  A           -   matrices Q and R in compact form.
  Output of CMatrixQR subroutine .
  M           -   number of rows in matrix A. M>=0.
  N           -   number of columns in matrix A. N>=0.
  Tau         -   scalar factors which are used to form Q.
  Output of CMatrixQR subroutine .
  QColumns    -   required number of columns in matrix Q. M>=QColumns>=0.

  Output parameters:
  Q           -   first QColumns columns of matrix Q.
  Array whose index ranges within [0..M-1, 0..QColumns-1].
  If QColumns=0, array isn't changed.
  ************************************************************************ *)
procedure CMatrixQRUnpackQ(const a: TLComplexMatrix; M: TLInt;
  n: TLInt; const TAU: TLComplexVec; QColumns: TLInt;
  var q: TLComplexMatrix);
var
  Work      : TLComplexVec;
  t         : TLComplexVec;
  TauBuf    : TLComplexVec;
  MinMN     : TLInt;
  RefCnt    : TLInt;
  TmpA      : TLComplexMatrix;
  TmpT      : TLComplexMatrix;
  TmpR      : TLComplexMatrix;
  BlockStart: TLInt;
  BlockSize : TLInt;
  RowsCount : TLInt;
  i         : TLInt;
  j         : TLInt;
  k         : TLInt;
  v         : TLComplex;
  i_        : TLInt;
  i1_       : TLInt;
begin
  Assert(QColumns <= M, 'UnpackQFromQR: QColumns>M!');
  if (M <= 0) or (n <= 0) then
    begin
      Exit;
    end;

  //
  // init
  //
  MinMN := Min(M, n);
  RefCnt := Min(MinMN, QColumns);
  SetLength(Work, Max(M, n) + 1);
  SetLength(t, Max(M, n) + 1);
  SetLength(TauBuf, MinMN);
  SetLength(TmpA, M, ABLASComplexBlockSize(a));
  SetLength(TmpT, ABLASComplexBlockSize(a), ABLASComplexBlockSize(a));
  SetLength(TmpR, 2 * ABLASComplexBlockSize(a), QColumns);
  SetLength(q, M, QColumns);
  i := 0;
  while i <= M - 1 do
    begin
      j := 0;
      while j <= QColumns - 1 do
        begin
          if i = j then
            begin
              q[i, j] := C_Complex(1);
            end
          else
            begin
              q[i, j] := C_Complex(0);
            end;
          inc(j);
        end;
      inc(i);
    end;

  //
  // Blocked code
  //
  BlockStart := ABLASComplexBlockSize(a) *
    (RefCnt div ABLASComplexBlockSize(a));
  BlockSize := RefCnt - BlockStart;
  while BlockStart >= 0 do
    begin
      RowsCount := M - BlockStart;

      //
      // QR decomposition of submatrix.
      // Matrix is copied to temporary storage to solve
      // some TLB issues arising from non-contiguous memory
      // access pattern.
      //
      CMatrixCopy(RowsCount, BlockSize, a, BlockStart, BlockStart, TmpA, 0, 0);
      i1_ := (BlockStart) - (0);
      for i_ := 0 to BlockSize - 1 do
        begin
          TauBuf[i_] := TAU[i_ + i1_];
        end;

      //
      // Update matrix, choose between:
      // a) Level 2 algorithm (when the rest of the matrix is small enough)
      // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
      // representation for products of Householder transformations',
      // by R. Schreiber and C. Van Loan.
      //
      if QColumns >= 2 * ABLASComplexBlockSize(a) then
        begin

          //
          // Prepare block reflector
          //
          CMatrixBlockReflector(TmpA, TauBuf, True, RowsCount, BlockSize,
            TmpT, Work);

          //
          // Multiply the rest of A by Q.
          //
          // Q  = E + Y*T*Y'  = E + TmpA*TmpT*TmpA'
          //
          CMatrixGEMM(BlockSize, QColumns, RowsCount, C_Complex(1.0), TmpA, 0, 0, 2,
            q, BlockStart, 0, 0, C_Complex(0.0), TmpR, 0, 0);
          CMatrixGEMM(BlockSize, QColumns, BlockSize, C_Complex(1.0), TmpT, 0, 0, 0,
            TmpR, 0, 0, 0, C_Complex(0.0), TmpR, BlockSize, 0);
          CMatrixGEMM(RowsCount, QColumns, BlockSize, C_Complex(1.0), TmpA, 0, 0, 0,
            TmpR, BlockSize, 0, 0, C_Complex(1.0), q, BlockStart, 0);
        end
      else
        begin

          //
          // Level 2 algorithm
          //
          i := BlockSize - 1;
          while i >= 0 do
            begin
              i1_ := (i) - (1);
              for i_ := 1 to RowsCount - i do
                begin
                  t[i_] := TmpA[i_ + i1_, i];
                end;
              t[1] := C_Complex(1);
              ComplexApplyReflectionFromTheLeft(q, TauBuf[i], t, BlockStart + i,
                M - 1, 0, QColumns - 1, Work);
              dec(i);
            end;
        end;

      //
      // Advance
      //
      BlockStart := BlockStart - ABLASComplexBlockSize(a);
      BlockSize := ABLASComplexBlockSize(a);
    end;
end;

(* ************************************************************************
  Unpacking of matrix R from the QR decomposition of a matrix A

  Input parameters:
  A       -   matrices Q and R in compact form.
  Output of CMatrixQR subroutine.
  M       -   number of rows in given matrix A. M>=0.
  N       -   number of columns in given matrix A. N>=0.

  Output parameters:
  R       -   matrix R, array[0..M-1, 0..N-1].
  ************************************************************************ *)
procedure CMatrixQRUnpackR(const a: TLComplexMatrix; M: TLInt;
  n: TLInt; var r: TLComplexMatrix);
var
  i : TLInt;
  k : TLInt;
  i_: TLInt;
begin
  if (M <= 0) or (n <= 0) then
    begin
      Exit;
    end;
  k := Min(M, n);
  SetLength(r, M, n);
  i := 0;
  while i <= n - 1 do
    begin
      r[0, i] := C_Complex(0);
      inc(i);
    end;
  i := 1;
  while i <= M - 1 do
    begin
      for i_ := 0 to n - 1 do
        begin
          r[i, i_] := r[0, i_];
        end;
      inc(i);
    end;
  i := 0;
  while i <= k - 1 do
    begin
      for i_ := i to n - 1 do
        begin
          r[i, i_] := a[i, i_];
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Partial unpacking of matrix Q from LQ decomposition of a TLComplex matrix A.

  Input parameters:
  A           -   matrices Q and R in compact form.
  Output of CMatrixLQ subroutine .
  M           -   number of rows in matrix A. M>=0.
  N           -   number of columns in matrix A. N>=0.
  Tau         -   scalar factors which are used to form Q.
  Output of CMatrixLQ subroutine .
  QRows       -   required number of rows in matrix Q. N>=QColumns>=0.

  Output parameters:
  Q           -   first QRows rows of matrix Q.
  Array whose index ranges within [0..QRows-1, 0..N-1].
  If QRows=0, array isn't changed.
  ************************************************************************ *)
procedure CMatrixLQUnpackQ(const a: TLComplexMatrix; M: TLInt;
  n: TLInt; const TAU: TLComplexVec; QRows: TLInt;
  var q: TLComplexMatrix);
var
  Work        : TLComplexVec;
  t           : TLComplexVec;
  TauBuf      : TLComplexVec;
  MinMN       : TLInt;
  RefCnt      : TLInt;
  TmpA        : TLComplexMatrix;
  TmpT        : TLComplexMatrix;
  TmpR        : TLComplexMatrix;
  BlockStart  : TLInt;
  BlockSize   : TLInt;
  ColumnsCount: TLInt;
  i           : TLInt;
  j           : TLInt;
  k           : TLInt;
  v           : TLComplex;
  i_          : TLInt;
  i1_         : TLInt;
begin
  if (M <= 0) or (n <= 0) then
    begin
      Exit;
    end;

  //
  // Init
  //
  MinMN := Min(M, n);
  RefCnt := Min(MinMN, QRows);
  SetLength(Work, Max(M, n) + 1);
  SetLength(t, Max(M, n) + 1);
  SetLength(TauBuf, MinMN);
  SetLength(TmpA, ABLASComplexBlockSize(a), n);
  SetLength(TmpT, ABLASComplexBlockSize(a), ABLASComplexBlockSize(a));
  SetLength(TmpR, QRows, 2 * ABLASComplexBlockSize(a));
  SetLength(q, QRows, n);
  i := 0;
  while i <= QRows - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          if i = j then
            begin
              q[i, j] := C_Complex(1);
            end
          else
            begin
              q[i, j] := C_Complex(0);
            end;
          inc(j);
        end;
      inc(i);
    end;

  //
  // Blocked code
  //
  BlockStart := ABLASComplexBlockSize(a) *
    (RefCnt div ABLASComplexBlockSize(a));
  BlockSize := RefCnt - BlockStart;
  while BlockStart >= 0 do
    begin
      ColumnsCount := n - BlockStart;

      //
      // LQ decomposition of submatrix.
      // Matrix is copied to temporary storage to solve
      // some TLB issues arising from non-contiguous memory
      // access pattern.
      //
      CMatrixCopy(BlockSize, ColumnsCount, a, BlockStart, BlockStart, TmpA, 0, 0);
      i1_ := (BlockStart) - (0);
      for i_ := 0 to BlockSize - 1 do
        begin
          TauBuf[i_] := TAU[i_ + i1_];
        end;

      //
      // Update matrix, choose between:
      // a) Level 2 algorithm (when the rest of the matrix is small enough)
      // b) blocked algorithm, see algorithm 5 from  'A storage efficient WY
      // representation for products of Householder transformations',
      // by R. Schreiber and C. Van Loan.
      //
      if QRows >= 2 * ABLASComplexBlockSize(a) then
        begin

          //
          // Prepare block reflector
          //
          CMatrixBlockReflector(TmpA, TauBuf, False, ColumnsCount, BlockSize,
            TmpT, Work);

          //
          // Multiply the rest of A by Q'.
          //
          // Q'  = E + Y*T'*Y'  = E + TmpA'*TmpT'*TmpA
          //
          CMatrixGEMM(QRows, BlockSize, ColumnsCount, C_Complex(1.0), q, 0,
            BlockStart, 0, TmpA, 0, 0, 2, C_Complex(0.0), TmpR, 0, 0);
          CMatrixGEMM(QRows, BlockSize, BlockSize, C_Complex(1.0), TmpR, 0, 0, 0,
            TmpT, 0, 0, 2, C_Complex(0.0), TmpR, 0, BlockSize);
          CMatrixGEMM(QRows, ColumnsCount, BlockSize, C_Complex(1.0), TmpR, 0,
            BlockSize, 0, TmpA, 0, 0, 0, C_Complex(1.0), q, 0, BlockStart);
        end
      else
        begin

          //
          // Level 2 algorithm
          //
          i := BlockSize - 1;
          while i >= 0 do
            begin
              i1_ := (i) - (1);
              for i_ := 1 to ColumnsCount - i do
                begin
                  t[i_] := Conj(TmpA[i, i_ + i1_]);
                end;
              t[1] := C_Complex(1);
              ComplexApplyReflectionFromTheRight(q, Conj(TauBuf[i]), t, 0, QRows - 1,
                BlockStart + i, n - 1, Work);
              dec(i);
            end;
        end;

      //
      // Advance
      //
      BlockStart := BlockStart - ABLASComplexBlockSize(a);
      BlockSize := ABLASComplexBlockSize(a);
    end;
end;

(* ************************************************************************
  Unpacking of matrix L from the LQ decomposition of a matrix A

  Input parameters:
  A       -   matrices Q and L in compact form.
  Output of CMatrixLQ subroutine.
  M       -   number of rows in given matrix A. M>=0.
  N       -   number of columns in given matrix A. N>=0.

  Output parameters:
  L       -   matrix L, array[0..M-1, 0..N-1].
  ************************************************************************ *)
procedure CMatrixLQUnpackL(const a: TLComplexMatrix; M: TLInt;
  n: TLInt; var L: TLComplexMatrix);
var
  i : TLInt;
  k : TLInt;
  i_: TLInt;
begin
  if (M <= 0) or (n <= 0) then
    begin
      Exit;
    end;
  SetLength(L, M, n);
  i := 0;
  while i <= n - 1 do
    begin
      L[0, i] := C_Complex(0);
      inc(i);
    end;
  i := 1;
  while i <= M - 1 do
    begin
      for i_ := 0 to n - 1 do
        begin
          L[i, i_] := L[0, i_];
        end;
      inc(i);
    end;
  i := 0;
  while i <= M - 1 do
    begin
      k := Min(i, n - 1);
      for i_ := 0 to k do
        begin
          L[i, i_] := a[i, i_];
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Reduction of a rectangular matrix to  bidiagonal form

  The algorithm reduces the rectangular matrix A to  bidiagonal form by
  orthogonal transformations P and Q: A = Q*B*P.

  Input parameters:
  A       -   source matrix. array[0..M-1, 0..N-1]
  M       -   number of rows in matrix A.
  N       -   number of columns in matrix A.

  Output parameters:
  A       -   matrices Q, B, P in compact form (see below).
  TauQ    -   scalar factors which are used to form matrix Q.
  TauP    -   scalar factors which are used to form matrix P.

  The main diagonal and one of the  secondary  diagonals  of  matrix  A  are
  replaced with bidiagonal  matrix  B.  Other  elements  contain  elementary
  reflections which form MxM matrix Q and NxN matrix P, respectively.

  If M>=N, B is the upper  bidiagonal  MxN  matrix  and  is  stored  in  the
  corresponding  elements  of  matrix  A.  Matrix  Q  is  represented  as  a
  product   of   elementary   reflections   Q = H(0)*H(1)*...*H(n-1),  where
  H(i) = 1-tau*v*v'. Here tau is a scalar which is stored  in  TauQ[i],  and
  vector v has the following  structure:  v(0:i-1)=0, v(i)=1, v(i+1:m-1)  is
  stored   in   elements   A(i+1:m-1,i).   Matrix   P  is  as  follows:  P =
  G(0)*G(1)*...*G(n-2), where G(i) = 1 - tau*u*u'. Tau is stored in TauP[i],
  u(0:i)=0, u(i+1)=1, u(i+2:n-1) is stored in elements A(i,i+2:n-1).

  If M<N, B is the  lower  bidiagonal  MxN  matrix  and  is  stored  in  the
  corresponding   elements  of  matrix  A.  Q = H(0)*H(1)*...*H(m-2),  where
  H(i) = 1 - tau*v*v', tau is stored in TauQ, v(0:i)=0, v(i+1)=1, v(i+2:m-1)
  is    stored    in   elements   A(i+2:m-1,i).    P = G(0)*G(1)*...*G(m-1),
  G(i) = 1-tau*u*u', tau is stored in  TauP,  u(0:i-1)=0, u(i)=1, u(i+1:n-1)
  is stored in A(i,i+1:n-1).

  EXAMPLE:

  m=6, n=5 (m > n):               m=5, n=6 (m < n):

  (  d   e   u1  u1  u1 )         (  d   u1  u1  u1  u1  u1 )
  (  v1  d   e   u2  u2 )         (  e   d   u2  u2  u2  u2 )
  (  v1  v2  d   e   u3 )         (  v1  e   d   u3  u3  u3 )
  (  v1  v2  v3  d   e  )         (  v1  v2  e   d   u4  u4 )
  (  v1  v2  v3  v4  d  )         (  v1  v2  v3  e   d   u5 )
  (  v1  v2  v3  v4  v5 )

  Here vi and ui are vectors which form H(i) and G(i), and d and e -
  are the diagonal and off-diagonal elements of matrix B.
  ************************************************************************ *)
procedure RMatrixBD(var a: TLMatrix; M: TLInt; n: TLInt;
  var TauQ: TLVec; var TauP: TLVec);
var
  Work : TLVec;
  t    : TLVec;
  MinMN: TLInt;
  MaxMN: TLInt;
  i    : TLInt;
  LTau : TLFloat;
  i_   : TLInt;
  i1_  : TLInt;
begin

  //
  // Prepare
  //
  if (n <= 0) or (M <= 0) then
    begin
      Exit;
    end;
  MinMN := Min(M, n);
  MaxMN := Max(M, n);
  SetLength(Work, MaxMN + 1);
  SetLength(t, MaxMN + 1);
  if M >= n then
    begin
      SetLength(TauQ, n);
      SetLength(TauP, n);
    end
  else
    begin
      SetLength(TauQ, M);
      SetLength(TauP, M);
    end;
  if M >= n then
    begin

      //
      // Reduce to upper bidiagonal form
      //
      i := 0;
      while i <= n - 1 do
        begin

          //
          // Generate elementary reflector H(i) to annihilate A(i+1:m-1,i)
          //
          i1_ := (i) - (1);
          for i_ := 1 to M - i do
            begin
              t[i_] := a[i_ + i1_, i];
            end;
          GenerateReflection(t, M - i, LTau);
          TauQ[i] := LTau;
          i1_ := (1) - (i);
          for i_ := i to M - 1 do
            begin
              a[i_, i] := t[i_ + i1_];
            end;
          t[1] := 1;

          //
          // Apply H(i) to A(i:m-1,i+1:n-1) from the left
          //
          ApplyReflectionFromTheLeft(a, LTau, t, i, M - 1, i + 1, n - 1, Work);
          if i < n - 1 then
            begin

              //
              // Generate elementary reflector G(i) to annihilate
              // A(i,i+2:n-1)
              //
              APVMove(@t[0], 1, n - i - 1, @a[i][0], i + 1, n - 1);
              GenerateReflection(t, n - 1 - i, LTau);
              TauP[i] := LTau;
              APVMove(@a[i][0], i + 1, n - 1, @t[0], 1, n - 1 - i);
              t[1] := 1;

              //
              // Apply G(i) to A(i+1:m-1,i+1:n-1) from the right
              //
              ApplyReflectionFromTheRight(a, LTau, t, i + 1, M - 1, i + 1,
                n - 1, Work);
            end
          else
            begin
              TauP[i] := 0;
            end;
          inc(i);
        end;
    end
  else
    begin

      //
      // Reduce to lower bidiagonal form
      //
      i := 0;
      while i <= M - 1 do
        begin

          //
          // Generate elementary reflector G(i) to annihilate A(i,i+1:n-1)
          //
          APVMove(@t[0], 1, n - i, @a[i][0], i, n - 1);
          GenerateReflection(t, n - i, LTau);
          TauP[i] := LTau;
          APVMove(@a[i][0], i, n - 1, @t[0], 1, n - i);
          t[1] := 1;

          //
          // Apply G(i) to A(i+1:m-1,i:n-1) from the right
          //
          ApplyReflectionFromTheRight(a, LTau, t, i + 1, M - 1, i, n - 1, Work);
          if i < M - 1 then
            begin

              //
              // Generate elementary reflector H(i) to annihilate
              // A(i+2:m-1,i)
              //
              i1_ := (i + 1) - (1);
              for i_ := 1 to M - 1 - i do
                begin
                  t[i_] := a[i_ + i1_, i];
                end;
              GenerateReflection(t, M - 1 - i, LTau);
              TauQ[i] := LTau;
              i1_ := (1) - (i + 1);
              for i_ := i + 1 to M - 1 do
                begin
                  a[i_, i] := t[i_ + i1_];
                end;
              t[1] := 1;

              //
              // Apply H(i) to A(i+1:m-1,i+1:n-1) from the left
              //
              ApplyReflectionFromTheLeft(a, LTau, t, i + 1, M - 1, i + 1,
                n - 1, Work);
            end
          else
            begin
              TauQ[i] := 0;
            end;
          inc(i);
        end;
    end;
end;

(* ************************************************************************
  Unpacking matrix Q which reduces a matrix to bidiagonal form.

  Input parameters:
  QP          -   matrices Q and P in compact form.
  Output of ToBidiagonal subroutine.
  M           -   number of rows in matrix A.
  N           -   number of columns in matrix A.
  TAUQ        -   scalar factors which are used to form Q.
  Output of ToBidiagonal subroutine.
  QColumns    -   required number of columns in matrix Q.
  M>=QColumns>=0.

  Output parameters:
  Q           -   first QColumns columns of matrix Q.
  Array[0..M-1, 0..QColumns-1]
  If QColumns=0, the array is not modified.
  ************************************************************************ *)
procedure RMatrixBDUnpackQ(const qp: TLMatrix; M: TLInt;
  n: TLInt; const TauQ: TLVec; QColumns: TLInt;
  var q: TLMatrix);
var
  i: TLInt;
  j: TLInt;
begin
  Assert(QColumns <= M, 'RMatrixBDUnpackQ: QColumns>M!');
  Assert(QColumns >= 0, 'RMatrixBDUnpackQ: QColumns<0!');
  if (M = 0) or (n = 0) or (QColumns = 0) then
    begin
      Exit;
    end;

  //
  // prepare Q
  //
  SetLength(q, M, QColumns);
  i := 0;
  while i <= M - 1 do
    begin
      j := 0;
      while j <= QColumns - 1 do
        begin
          if i = j then
            begin
              q[i, j] := 1;
            end
          else
            begin
              q[i, j] := 0;
            end;
          inc(j);
        end;
      inc(i);
    end;

  //
  // Calculate
  //
  RMatrixBDMultiplyByQ(qp, M, n, TauQ, q, M, QColumns, False, False);
end;

(* ************************************************************************
  Multiplication by matrix Q which reduces matrix A to  bidiagonal form.

  The algorithm allows pre- or post-multiply by Q or Q'.

  Input parameters:
  QP          -   matrices Q and P in compact form.
  Output of ToBidiagonal subroutine.
  M           -   number of rows in matrix A.
  N           -   number of columns in matrix A.
  TAUQ        -   scalar factors which are used to form Q.
  Output of ToBidiagonal subroutine.
  Z           -   multiplied matrix.
  array[0..ZRows-1,0..ZColumns-1]
  ZRows       -   number of rows in matrix Z. If FromTheRight=False,
  ZRows=M, otherwise ZRows can be arbitrary.
  ZColumns    -   number of columns in matrix Z. If FromTheRight=True,
  ZColumns=M, otherwise ZColumns can be arbitrary.
  FromTheRight -  pre- or post-multiply.
  DoTranspose -   multiply by Q or Q'.

  Output parameters:
  Z           -   product of Z and Q.
  Array[0..ZRows-1,0..ZColumns-1]
  If ZRows=0 or ZColumns=0, the array is not modified.
  ************************************************************************ *)
procedure RMatrixBDMultiplyByQ(const qp: TLMatrix; M: TLInt;
  n: TLInt; const TauQ: TLVec; var z: TLMatrix;
  ZRows: TLInt; ZColumns: TLInt; FromTheRight: Boolean;
  DoTranspose: Boolean);
var
  i    : TLInt;
  i1   : TLInt;
  i2   : TLInt;
  IStep: TLInt;
  v    : TLVec;
  Work : TLVec;
  mx   : TLInt;
  i_   : TLInt;
  i1_  : TLInt;
begin
  if (M <= 0) or (n <= 0) or (ZRows <= 0) or (ZColumns <= 0) then
    begin
      Exit;
    end;
  Assert(FromTheRight and (ZColumns = M) or not FromTheRight and (ZRows = M),
    'RMatrixBDMultiplyByQ: incorrect Z size!');

  //
  // init
  //
  mx := Max(M, n);
  mx := Max(mx, ZRows);
  mx := Max(mx, ZColumns);
  SetLength(v, mx + 1);
  SetLength(Work, mx + 1);
  if M >= n then
    begin

      //
      // setup
      //
      if FromTheRight then
        begin
          i1 := 0;
          i2 := n - 1;
          IStep := +1;
        end
      else
        begin
          i1 := n - 1;
          i2 := 0;
          IStep := -1;
        end;
      if DoTranspose then
        begin
          i := i1;
          i1 := i2;
          i2 := i;
          IStep := -IStep;
        end;

      //
      // Process
      //
      i := i1;
      repeat
        i1_ := (i) - (1);
        for i_ := 1 to M - i do
          begin
            v[i_] := qp[i_ + i1_, i];
          end;
        v[1] := 1;
        if FromTheRight then
          begin
            ApplyReflectionFromTheRight(z, TauQ[i], v, 0, ZRows - 1, i,
              M - 1, Work);
          end
        else
          begin
            ApplyReflectionFromTheLeft(z, TauQ[i], v, i, M - 1, 0,
              ZColumns - 1, Work);
          end;
        i := i + IStep;
      until i = i2 + IStep;
    end
  else
    begin

      //
      // setup
      //
      if FromTheRight then
        begin
          i1 := 0;
          i2 := M - 2;
          IStep := +1;
        end
      else
        begin
          i1 := M - 2;
          i2 := 0;
          IStep := -1;
        end;
      if DoTranspose then
        begin
          i := i1;
          i1 := i2;
          i2 := i;
          IStep := -IStep;
        end;

      //
      // Process
      //
      if M - 1 > 0 then
        begin
          i := i1;
          repeat
            i1_ := (i + 1) - (1);
            for i_ := 1 to M - i - 1 do
              begin
                v[i_] := qp[i_ + i1_, i];
              end;
            v[1] := 1;
            if FromTheRight then
              begin
                ApplyReflectionFromTheRight(z, TauQ[i], v, 0, ZRows - 1, i + 1,
                  M - 1, Work);
              end
            else
              begin
                ApplyReflectionFromTheLeft(z, TauQ[i], v, i + 1, M - 1, 0,
                  ZColumns - 1, Work);
              end;
            i := i + IStep;
          until i = i2 + IStep;
        end;
    end;
end;

(* ************************************************************************
  Unpacking matrix P which reduces matrix A to bidiagonal form.
  The subroutine returns transposed matrix P.

  Input parameters:
  QP      -   matrices Q and P in compact form.
  Output of ToBidiagonal subroutine.
  M       -   number of rows in matrix A.
  N       -   number of columns in matrix A.
  TAUP    -   scalar factors which are used to form P.
  Output of ToBidiagonal subroutine.
  PTRows  -   required number of rows of matrix P^T. N >= PTRows >= 0.

  Output parameters:
  PT      -   first PTRows columns of matrix P^T
  Array[0..PTRows-1, 0..N-1]
  If PTRows=0, the array is not modified.
  ************************************************************************ *)
procedure RMatrixBDUnpackPT(const qp: TLMatrix; M: TLInt;
  n: TLInt; const TauP: TLVec; PTRows: TLInt;
  var pt: TLMatrix);
var
  i: TLInt;
  j: TLInt;
begin
  Assert(PTRows <= n, 'RMatrixBDUnpackPT: PTRows>N!');
  Assert(PTRows >= 0, 'RMatrixBDUnpackPT: PTRows<0!');
  if (M = 0) or (n = 0) or (PTRows = 0) then
    begin
      Exit;
    end;

  //
  // prepare PT
  //
  SetLength(pt, PTRows, n);
  i := 0;
  while i <= PTRows - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          if i = j then
            begin
              pt[i, j] := 1;
            end
          else
            begin
              pt[i, j] := 0;
            end;
          inc(j);
        end;
      inc(i);
    end;

  //
  // Calculate
  //
  RMatrixBDMultiplyByP(qp, M, n, TauP, pt, PTRows, n, True, True);
end;

(* ************************************************************************
  Multiplication by matrix P which reduces matrix A to  bidiagonal form.

  The algorithm allows pre- or post-multiply by P or P'.

  Input parameters:
  QP          -   matrices Q and P in compact form.
  Output of RMatrixBD subroutine.
  M           -   number of rows in matrix A.
  N           -   number of columns in matrix A.
  TAUP        -   scalar factors which are used to form P.
  Output of RMatrixBD subroutine.
  Z           -   multiplied matrix.
  Array whose indexes range within [0..ZRows-1,0..ZColumns-1].
  ZRows       -   number of rows in matrix Z. If FromTheRight=False,
  ZRows=N, otherwise ZRows can be arbitrary.
  ZColumns    -   number of columns in matrix Z. If FromTheRight=True,
  ZColumns=N, otherwise ZColumns can be arbitrary.
  FromTheRight -  pre- or post-multiply.
  DoTranspose -   multiply by P or P'.

  Output parameters:
  Z - product of Z and P.
  Array whose indexes range within [0..ZRows-1,0..ZColumns-1].
  If ZRows=0 or ZColumns=0, the array is not modified.
  ************************************************************************ *)
procedure RMatrixBDMultiplyByP(const qp: TLMatrix; M: TLInt;
  n: TLInt; const TauP: TLVec; var z: TLMatrix;
  ZRows: TLInt; ZColumns: TLInt; FromTheRight: Boolean;
  DoTranspose: Boolean);
var
  i    : TLInt;
  v    : TLVec;
  Work : TLVec;
  mx   : TLInt;
  i1   : TLInt;
  i2   : TLInt;
  IStep: TLInt;
begin
  if (M <= 0) or (n <= 0) or (ZRows <= 0) or (ZColumns <= 0) then
    begin
      Exit;
    end;
  Assert(FromTheRight and (ZColumns = n) or not FromTheRight and (ZRows = n),
    'RMatrixBDMultiplyByP: incorrect Z size!');

  //
  // init
  //
  mx := Max(M, n);
  mx := Max(mx, ZRows);
  mx := Max(mx, ZColumns);
  SetLength(v, mx + 1);
  SetLength(Work, mx + 1);
  if M >= n then
    begin

      //
      // setup
      //
      if FromTheRight then
        begin
          i1 := n - 2;
          i2 := 0;
          IStep := -1;
        end
      else
        begin
          i1 := 0;
          i2 := n - 2;
          IStep := +1;
        end;
      if not DoTranspose then
        begin
          i := i1;
          i1 := i2;
          i2 := i;
          IStep := -IStep;
        end;

      //
      // Process
      //
      if n - 1 > 0 then
        begin
          i := i1;
          repeat
            APVMove(@v[0], 1, n - 1 - i, @qp[i][0], i + 1, n - 1);
            v[1] := 1;
            if FromTheRight then
              begin
                ApplyReflectionFromTheRight(z, TauP[i], v, 0, ZRows - 1, i + 1,
                  n - 1, Work);
              end
            else
              begin
                ApplyReflectionFromTheLeft(z, TauP[i], v, i + 1, n - 1, 0,
                  ZColumns - 1, Work);
              end;
            i := i + IStep;
          until i = i2 + IStep;
        end;
    end
  else
    begin

      //
      // setup
      //
      if FromTheRight then
        begin
          i1 := M - 1;
          i2 := 0;
          IStep := -1;
        end
      else
        begin
          i1 := 0;
          i2 := M - 1;
          IStep := +1;
        end;
      if not DoTranspose then
        begin
          i := i1;
          i1 := i2;
          i2 := i;
          IStep := -IStep;
        end;

      //
      // Process
      //
      i := i1;
      repeat
        APVMove(@v[0], 1, n - i, @qp[i][0], i, n - 1);
        v[1] := 1;
        if FromTheRight then
          begin
            ApplyReflectionFromTheRight(z, TauP[i], v, 0, ZRows - 1, i,
              n - 1, Work);
          end
        else
          begin
            ApplyReflectionFromTheLeft(z, TauP[i], v, i, n - 1, 0,
              ZColumns - 1, Work);
          end;
        i := i + IStep;
      until i = i2 + IStep;
    end;
end;

(* ************************************************************************
  Unpacking of the main and secondary diagonals of bidiagonal decomposition
  of matrix A.

  Input parameters:
  B   -   output of RMatrixBD subroutine.
  M   -   number of rows in matrix B.
  N   -   number of columns in matrix B.

  Output parameters:
  IsUpper -   True, if the matrix is upper bidiagonal.
  otherwise IsUpper is False.
  D       -   the main diagonal.
  Array whose index ranges within [0..Min(M,N)-1].
  E       -   the secondary diagonal (upper or lower, depending on
  the value of IsUpper).
  Array index ranges within [0..Min(M,N)-1], the last
  element is not used.
  ************************************************************************ *)
procedure RMatrixBDUnpackDiagonals(const b: TLMatrix; M: TLInt;
  n: TLInt; var IsUpper: Boolean; var d: TLVec;
  var E: TLVec);
var
  i: TLInt;
begin
  IsUpper := M >= n;
  if (M <= 0) or (n <= 0) then
    begin
      Exit;
    end;
  if IsUpper then
    begin
      SetLength(d, n);
      SetLength(E, n);
      i := 0;
      while i <= n - 2 do
        begin
          d[i] := b[i, i];
          E[i] := b[i, i + 1];
          inc(i);
        end;
      d[n - 1] := b[n - 1, n - 1];
    end
  else
    begin
      SetLength(d, M);
      SetLength(E, M);
      i := 0;
      while i <= M - 2 do
        begin
          d[i] := b[i, i];
          E[i] := b[i + 1, i];
          inc(i);
        end;
      d[M - 1] := b[M - 1, M - 1];
    end;
end;

(* ************************************************************************
  Reduction of a square matrix to  upper Hessenberg form: Q'*A*Q = H,
  where Q is an orthogonal matrix, H - Hessenberg matrix.

  Input parameters:
  A       -   matrix A with elements [0..N-1, 0..N-1]
  N       -   size of matrix A.

  Output parameters:
  A       -   matrices Q and P in  compact form (see below).
  Tau     -   array of scalar factors which are used to form matrix Q.
  Array whose index ranges within [0..N-2]

  Matrix H is located on the main diagonal, on the lower secondary  diagonal
  and above the main diagonal of matrix A. The elements which are used to
  form matrix Q are situated in array Tau and below the lower secondary
  diagonal of matrix A as follows:

  Matrix Q is represented as a product of elementary reflections

  Q = H(0)*H(2)*...*H(n-2),

  where each H(i) is given by

  H(i) = 1 - tau * v * (v^T)

  where tau is a scalar stored in Tau[I]; v - is a real vector,
  so that v(0:i) = 0, v(i+1) = 1, v(i+2:n-1) stored in A(i+2:n-1,i).
  ************************************************************************ *)
procedure RMatrixHessenberg(var a: TLMatrix; n: TLInt;
  var TAU: TLVec);
var
  i   : TLInt;
  v   : TLFloat;
  t   : TLVec;
  Work: TLVec;
  i_  : TLInt;
  i1_ : TLInt;
begin
  Assert(n >= 0, 'RMatrixHessenberg: incorrect N!');

  //
  // Quick return if possible
  //
  if n <= 1 then
    begin
      Exit;
    end;
  SetLength(TAU, n - 2 + 1);
  SetLength(t, n + 1);
  SetLength(Work, n  );
  i := 0;
  while i <= n - 2 do
    begin

      //
      // Compute elementary reflector H(i) to annihilate A(i+2:ihi,i)
      //
      i1_ := (i + 1) - (1);
      for i_ := 1 to n - i - 1 do
        begin
          t[i_] := a[i_ + i1_, i];
        end;
      GenerateReflection(t, n - i - 1, v);
      i1_ := (1) - (i + 1);
      for i_ := i + 1 to n - 1 do
        begin
          a[i_, i] := t[i_ + i1_];
        end;
      TAU[i] := v;
      t[1] := 1;

      //
      // Apply H(i) to A(1:ihi,i+1:ihi) from the right
      //
      ApplyReflectionFromTheRight(a, v, t, 0, n - 1, i + 1, n - 1, Work);

      //
      // Apply H(i) to A(i+1:ihi,i+1:n) from the left
      //
      ApplyReflectionFromTheLeft(a, v, t, i + 1, n - 1, i + 1, n - 1, Work);
      inc(i);
    end;
end;

(* ************************************************************************
  Unpacking matrix Q which reduces matrix A to upper Hessenberg form

  Input parameters:
  A   -   output of RMatrixHessenberg subroutine.
  N   -   size of matrix A.
  Tau -   scalar factors which are used to form Q.
  Output of RMatrixHessenberg subroutine.

  Output parameters:
  Q   -   matrix Q.
  Array whose indexes range within [0..N-1, 0..N-1].
  ************************************************************************ *)
procedure RMatrixHessenbergUnpackQ(const a: TLMatrix; n: TLInt;
  const TAU: TLVec; var q: TLMatrix);
var
  i   : TLInt;
  j   : TLInt;
  v   : TLVec;
  Work: TLVec;
  i_  : TLInt;
  i1_ : TLInt;
begin
  if n = 0 then
    begin
      Exit;
    end;

  //
  // init
  //
  SetLength(q, n  , n  );
  SetLength(v, n  );
  SetLength(Work, n  );
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          if i = j then
            begin
              q[i, j] := 1;
            end
          else
            begin
              q[i, j] := 0;
            end;
          inc(j);
        end;
      inc(i);
    end;

  //
  // unpack Q
  //
  i := 0;
  while i <= n - 2 do
    begin

      //
      // Apply H(i)
      //
      i1_ := (i + 1) - (1);
      for i_ := 1 to n - i - 1 do
        begin
          v[i_] := a[i_ + i1_, i];
        end;
      v[1] := 1;
      ApplyReflectionFromTheRight(q, TAU[i], v, 0, n - 1, i + 1, n - 1, Work);
      inc(i);
    end;
end;

(* ************************************************************************
  Unpacking matrix H (the result of matrix A reduction to upper Hessenberg form)

  Input parameters:
  A   -   output of RMatrixHessenberg subroutine.
  N   -   size of matrix A.

  Output parameters:
  H   -   matrix H. Array whose indexes range within [0..N-1, 0..N-1].
  ************************************************************************ *)
procedure RMatrixHessenbergUnpackH(const a: TLMatrix; n: TLInt;
  var h: TLMatrix);
var
  i   : TLInt;
  j   : TLInt;
  v   : TLVec;
  Work: TLVec;
begin
  if n = 0 then
    begin
      Exit;
    end;
  SetLength(h, n  , n  );
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= i - 2 do
        begin
          h[i, j] := 0;
          inc(j);
        end;
      j := Max(0, i - 1);
      APVMove(@h[i][0], j, n - 1, @a[i][0], j, n - 1);
      inc(i);
    end;
end;

(* ************************************************************************
  Reduction of a symmetric matrix which is given by its higher or lower
  triangular part to a tridiagonal matrix using orthogonal similarity
  transformation: Q'*A*Q=T.

  Input parameters:
  A       -   matrix to be transformed
  array with elements [0..N-1, 0..N-1].
  N       -   size of matrix A.
  IsUpper -   storage format. If IsUpper = True, then matrix A is given
  by its upper triangle, and the lower triangle is not used
  and not modified by the algorithm, and vice versa
  if IsUpper = False.

  Output parameters:
  A       -   matrices T and Q in  compact form (see lower)
  Tau     -   array of factors which are forming matrices H(i)
  array with elements [0..N-2].
  D       -   main diagonal of symmetric matrix T.
  array with elements [0..N-1].
  E       -   secondary diagonal of symmetric matrix T.
  array with elements [0..N-2].


  If IsUpper=True, the matrix Q is represented as a product of elementary
  reflectors

  Q = H(n-2) . . . H(2) H(0).

  Each H(i) has the form

  H(i) = I - tau * v * v'

  where tau is a real scalar, and v is a real vector with
  v(i+1:n-1) = 0, v(i) = 1, v(0:i-1) is stored on exit in
  A(0:i-1,i+1), and tau in TAU(i).

  If IsUpper=False, the matrix Q is represented as a product of elementary
  reflectors

  Q = H(0) H(2) . . . H(n-2).

  Each H(i) has the form

  H(i) = I - tau * v * v'

  where tau is a real scalar, and v is a real vector with
  v(0:i) = 0, v(i+1) = 1, v(i+2:n-1) is stored on exit in A(i+2:n-1,i),
  and tau in TAU(i).

  The contents of A on exit are illustrated by the following examples
  with n = 5:

  if UPLO = 'U':                       if UPLO = 'L':

  (  d   e   v1  v2  v3 )              (  d                  )
  (      d   e   v2  v3 )              (  e   d              )
  (          d   e   v3 )              (  v0  e   d          )
  (              d   e  )              (  v0  v1  e   d      )
  (                  d  )              (  v0  v1  v2  e   d  )

  where d and e denote diagonal and off-diagonal elements of T, and vi
  denotes an element of the vector defining H(i).
  ************************************************************************ *)
procedure SMatrixTD(var a: TLMatrix; n: TLInt; IsUpper: Boolean;
  var TAU: TLVec; var d: TLVec; var E: TLVec);
var
  i    : TLInt;
  alpha: TLFloat;
  TAUI : TLFloat;
  v    : TLFloat;
  t    : TLVec;
  t2   : TLVec;
  t3   : TLVec;
  i_   : TLInt;
  i1_  : TLInt;
begin
  if n <= 0 then
    begin
      Exit;
    end;
  SetLength(t, n + 1);
  SetLength(t2, n + 1);
  SetLength(t3, n + 1);
  if n > 1 then
    begin
      SetLength(TAU, n - 2 + 1);
    end;
  SetLength(d, n  );
  if n > 1 then
    begin
      SetLength(E, n - 2 + 1);
    end;
  if IsUpper then
    begin

      //
      // Reduce the upper triangle of A
      //
      i := n - 2;
      while i >= 0 do
        begin

          //
          // Generate elementary reflector H() = E - tau * v * v'
          //
          if i >= 1 then
            begin
              i1_ := (0) - (2);
              for i_ := 2 to i + 1 do
                begin
                  t[i_] := a[i_ + i1_, i + 1];
                end;
            end;
          t[1] := a[i, i + 1];
          GenerateReflection(t, i + 1, TAUI);
          if i >= 1 then
            begin
              i1_ := (2) - (0);
              for i_ := 0 to i - 1 do
                begin
                  a[i_, i + 1] := t[i_ + i1_];
                end;
            end;
          a[i, i + 1] := t[1];
          E[i] := a[i, i + 1];
          if AP_FP_NEq(TAUI, 0) then
            begin

              //
              // Apply H from both sides to A
              //
              a[i, i + 1] := 1;

              //
              // Compute  x := tau * A * v  storing x in TAU
              //
              i1_ := (0) - (1);
              for i_ := 1 to i + 1 do
                begin
                  t[i_] := a[i_ + i1_, i + 1];
                end;
              SymmetricMatrixVectorMultiply(a, IsUpper, 0, i, t, TAUI, t3);
              APVMove(@TAU[0], 0, i, @t3[0], 1, i + 1);

              //
              // Compute  w := x - 1/2 * tau * (x'*v) * v
              //
              v := 0.0;
              for i_ := 0 to i do
                begin
                  v := v + TAU[i_] * a[i_, i + 1];
                end;
              alpha := -0.5 * TAUI * v;
              for i_ := 0 to i do
                begin
                  TAU[i_] := TAU[i_] + alpha * a[i_, i + 1];
                end;

              //
              // Apply the transformation as a rank-2 update:
              // A := A - v * w' - w * v'
              //
              i1_ := (0) - (1);
              for i_ := 1 to i + 1 do
                begin
                  t[i_] := a[i_ + i1_, i + 1];
                end;
              APVMove(@t3[0], 1, i + 1, @TAU[0], 0, i);
              SymmetricRank2Update(a, IsUpper, 0, i, t, t3, t2, -1);
              a[i, i + 1] := E[i];
            end;
          d[i + 1] := a[i + 1, i + 1];
          TAU[i] := TAUI;
          dec(i);
        end;
      d[0] := a[0, 0];
    end
  else
    begin

      //
      // Reduce the lower triangle of A
      //
      i := 0;
      while i <= n - 2 do
        begin

          //
          // Generate elementary reflector H = E - tau * v * v'
          //
          i1_ := (i + 1) - (1);
          for i_ := 1 to n - i - 1 do
            begin
              t[i_] := a[i_ + i1_, i];
            end;
          GenerateReflection(t, n - i - 1, TAUI);
          i1_ := (1) - (i + 1);
          for i_ := i + 1 to n - 1 do
            begin
              a[i_, i] := t[i_ + i1_];
            end;
          E[i] := a[i + 1, i];
          if AP_FP_NEq(TAUI, 0) then
            begin

              //
              // Apply H from both sides to A
              //
              a[i + 1, i] := 1;

              //
              // Compute  x := tau * A * v  storing y in TAU
              //
              i1_ := (i + 1) - (1);
              for i_ := 1 to n - i - 1 do
                begin
                  t[i_] := a[i_ + i1_, i];
                end;
              SymmetricMatrixVectorMultiply(a, IsUpper, i + 1, n - 1, t, TAUI, t2);
              APVMove(@TAU[0], i, n - 2, @t2[0], 1, n - i - 1);

              //
              // Compute  w := x - 1/2 * tau * (x'*v) * v
              //
              i1_ := (i + 1) - (i);
              v := 0.0;
              for i_ := i to n - 2 do
                begin
                  v := v + TAU[i_] * a[i_ + i1_, i];
                end;
              alpha := -0.5 * TAUI * v;
              i1_ := (i + 1) - (i);
              for i_ := i to n - 2 do
                begin
                  TAU[i_] := TAU[i_] + alpha * a[i_ + i1_, i];
                end;

              //
              // Apply the transformation as a rank-2 update:
              // A := A - v * w' - w * v'
              //
              //
              i1_ := (i + 1) - (1);
              for i_ := 1 to n - i - 1 do
                begin
                  t[i_] := a[i_ + i1_, i];
                end;
              APVMove(@t2[0], 1, n - i - 1, @TAU[0], i, n - 2);
              SymmetricRank2Update(a, IsUpper, i + 1, n - 1, t, t2, t3, -1);
              a[i + 1, i] := E[i];
            end;
          d[i] := a[i, i];
          TAU[i] := TAUI;
          inc(i);
        end;
      d[n - 1] := a[n - 1, n - 1];
    end;
end;

(* ************************************************************************
  Unpacking matrix Q which reduces symmetric matrix to a tridiagonal
  form.

  Input parameters:
  A       -   the result of a SMatrixTD subroutine
  N       -   size of matrix A.
  IsUpper -   storage format (a parameter of SMatrixTD subroutine)
  Tau     -   the result of a SMatrixTD subroutine

  Output parameters:
  Q       -   transformation matrix.
  array with elements [0..N-1, 0..N-1].
  ************************************************************************ *)
procedure SMatrixTDUnpackQ(const a: TLMatrix; const n: TLInt;
  const IsUpper: Boolean; const TAU: TLVec; var q: TLMatrix);
var
  i   : TLInt;
  j   : TLInt;
  v   : TLVec;
  Work: TLVec;
  i_  : TLInt;
  i1_ : TLInt;
begin
  if n = 0 then
    begin
      Exit;
    end;

  //
  // init
  //
  SetLength(q, n  , n  );
  SetLength(v, n + 1);
  SetLength(Work, n  );
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          if i = j then
            begin
              q[i, j] := 1;
            end
          else
            begin
              q[i, j] := 0;
            end;
          inc(j);
        end;
      inc(i);
    end;

  //
  // unpack Q
  //
  if IsUpper then
    begin
      i := 0;
      while i <= n - 2 do
        begin

          //
          // Apply H(i)
          //
          i1_ := (0) - (1);
          for i_ := 1 to i + 1 do
            begin
              v[i_] := a[i_ + i1_, i + 1];
            end;
          v[i + 1] := 1;
          ApplyReflectionFromTheLeft(q, TAU[i], v, 0, i, 0, n - 1, Work);
          inc(i);
        end;
    end
  else
    begin
      i := n - 2;
      while i >= 0 do
        begin

          //
          // Apply H(i)
          //
          i1_ := (i + 1) - (1);
          for i_ := 1 to n - i - 1 do
            begin
              v[i_] := a[i_ + i1_, i];
            end;
          v[1] := 1;
          ApplyReflectionFromTheLeft(q, TAU[i], v, i + 1, n - 1, 0, n - 1, Work);
          dec(i);
        end;
    end;
end;

(* ************************************************************************
  Reduction of a Hermitian matrix which is given  by  its  higher  or  lower
  triangular part to a real  tridiagonal  matrix  using  unitary  similarity
  transformation: Q'*A*Q = T.

  Input parameters:
  A       -   matrix to be transformed
  array with elements [0..N-1, 0..N-1].
  N       -   size of matrix A.
  IsUpper -   storage format. If IsUpper = True, then matrix A is  given
  by its upper triangle, and the lower triangle is not  used
  and not modified by the algorithm, and vice versa
  if IsUpper = False.

  Output parameters:
  A       -   matrices T and Q in  compact form (see lower)
  Tau     -   array of factors which are forming matrices H(i)
  array with elements [0..N-2].
  D       -   main diagonal of real symmetric matrix T.
  array with elements [0..N-1].
  E       -   secondary diagonal of real symmetric matrix T.
  array with elements [0..N-2].


  If IsUpper=True, the matrix Q is represented as a product of elementary
  reflectors

  Q = H(n-2) . . . H(2) H(0).

  Each H(i) has the form

  H(i) = I - tau * v * v'

  where tau is a TLComplex scalar, and v is a TLComplex vector with
  v(i+1:n-1) = 0, v(i) = 1, v(0:i-1) is stored on exit in
  A(0:i-1,i+1), and tau in TAU(i).

  If IsUpper=False, the matrix Q is represented as a product of elementary
  reflectors

  Q = H(0) H(2) . . . H(n-2).

  Each H(i) has the form

  H(i) = I - tau * v * v'

  where tau is a TLComplex scalar, and v is a TLComplex vector with
  v(0:i) = 0, v(i+1) = 1, v(i+2:n-1) is stored on exit in A(i+2:n-1,i),
  and tau in TAU(i).

  The contents of A on exit are illustrated by the following examples
  with n = 5:

  if UPLO = 'U':                       if UPLO = 'L':

  (  d   e   v1  v2  v3 )              (  d                  )
  (      d   e   v2  v3 )              (  e   d              )
  (          d   e   v3 )              (  v0  e   d          )
  (              d   e  )              (  v0  v1  e   d      )
  (                  d  )              (  v0  v1  v2  e   d  )

  where d and e denote diagonal and off-diagonal elements of T, and vi
  denotes an element of the vector defining H(i).
  ************************************************************************ *)
procedure HMatrixTD(var a: TLComplexMatrix; n: TLInt; IsUpper: Boolean;
  var TAU: TLComplexVec; var d: TLVec; var E: TLVec);
var
  i    : TLInt;
  alpha: TLComplex;
  TAUI : TLComplex;
  v    : TLComplex;
  t    : TLComplexVec;
  t2   : TLComplexVec;
  t3   : TLComplexVec;
  i_   : TLInt;
  i1_  : TLInt;
begin
  if n <= 0 then
    begin
      Exit;
    end;
  i := 0;
  while i <= n - 1 do
    begin
      Assert(AP_FP_Eq(a[i, i].y, 0));
      inc(i);
    end;
  if n > 1 then
    begin
      SetLength(TAU, n - 2 + 1);
      SetLength(E, n - 2 + 1);
    end;
  SetLength(d, n  );
  SetLength(t, n  );
  SetLength(t2, n  );
  SetLength(t3, n  );
  if IsUpper then
    begin

      //
      // Reduce the upper triangle of A
      //
      a[n - 1, n - 1] := C_Complex(a[n - 1, n - 1].x);
      i := n - 2;
      while i >= 0 do
        begin

          //
          // Generate elementary reflector H = I+1 - tau * v * v'
          //
          alpha := a[i, i + 1];
          t[1] := alpha;
          if i >= 1 then
            begin
              i1_ := (0) - (2);
              for i_ := 2 to i + 1 do
                begin
                  t[i_] := a[i_ + i1_, i + 1];
                end;
            end;
          ComplexGenerateReflection(t, i + 1, TAUI);
          if i >= 1 then
            begin
              i1_ := (2) - (0);
              for i_ := 0 to i - 1 do
                begin
                  a[i_, i + 1] := t[i_ + i1_];
                end;
            end;
          alpha := t[1];
          E[i] := alpha.x;
          if C_NotEqualR(TAUI, 0) then
            begin

              //
              // Apply H(I+1) from both sides to A
              //
              a[i, i + 1] := C_Complex(1);

              //
              // Compute  x := tau * A * v  storing x in TAU
              //
              i1_ := (0) - (1);
              for i_ := 1 to i + 1 do
                begin
                  t[i_] := a[i_ + i1_, i + 1];
                end;
              HermitianMatrixVectorMultiply(a, IsUpper, 0, i, t, TAUI, t2);
              i1_ := (1) - (0);
              for i_ := 0 to i do
                begin
                  TAU[i_] := t2[i_ + i1_];
                end;

              //
              // Compute  w := x - 1/2 * tau * (x'*v) * v
              //
              v := C_Complex(0.0);
              for i_ := 0 to i do
                begin
                  v := C_Add(v, C_Mul(Conj(TAU[i_]), a[i_, i + 1]));
                end;
              alpha := C_Opposite(C_Mul(C_MulR(TAUI, 0.5), v));
              for i_ := 0 to i do
                begin
                  TAU[i_] := C_Add(TAU[i_], C_Mul(alpha, a[i_, i + 1]));
                end;

              //
              // Apply the transformation as a rank-2 update:
              // A := A - v * w' - w * v'
              //
              i1_ := (0) - (1);
              for i_ := 1 to i + 1 do
                begin
                  t[i_] := a[i_ + i1_, i + 1];
                end;
              i1_ := (0) - (1);
              for i_ := 1 to i + 1 do
                begin
                  t3[i_] := TAU[i_ + i1_];
                end;
              HermitianRank2Update(a, IsUpper, 0, i, t, t3, t2, C_Complex(-1));
            end
          else
            begin
              a[i, i] := C_Complex(a[i, i].x);
            end;
          a[i, i + 1] := C_Complex(E[i]);
          d[i + 1] := a[i + 1, i + 1].x;
          TAU[i] := TAUI;
          dec(i);
        end;
      d[0] := a[0, 0].x;
    end
  else
    begin

      //
      // Reduce the lower triangle of A
      //
      a[0, 0] := C_Complex(a[0, 0].x);
      i := 0;
      while i <= n - 2 do
        begin

          //
          // Generate elementary reflector H = I - tau * v * v'
          //
          i1_ := (i + 1) - (1);
          for i_ := 1 to n - i - 1 do
            begin
              t[i_] := a[i_ + i1_, i];
            end;
          ComplexGenerateReflection(t, n - i - 1, TAUI);
          i1_ := (1) - (i + 1);
          for i_ := i + 1 to n - 1 do
            begin
              a[i_, i] := t[i_ + i1_];
            end;
          E[i] := a[i + 1, i].x;
          if C_NotEqualR(TAUI, 0) then
            begin

              //
              // Apply H(i) from both sides to A(i+1:n,i+1:n)
              //
              a[i + 1, i] := C_Complex(1);

              //
              // Compute  x := tau * A * v  storing y in TAU
              //
              i1_ := (i + 1) - (1);
              for i_ := 1 to n - i - 1 do
                begin
                  t[i_] := a[i_ + i1_, i];
                end;
              HermitianMatrixVectorMultiply(a, IsUpper, i + 1, n - 1, t, TAUI, t2);
              i1_ := (1) - (i);
              for i_ := i to n - 2 do
                begin
                  TAU[i_] := t2[i_ + i1_];
                end;

              //
              // Compute  w := x - 1/2 * tau * (x'*v) * v
              //
              i1_ := (i + 1) - (i);
              v := C_Complex(0.0);
              for i_ := i to n - 2 do
                begin
                  v := C_Add(v, C_Mul(Conj(TAU[i_]), a[i_ + i1_, i]));
                end;
              alpha := C_Opposite(C_Mul(C_MulR(TAUI, 0.5), v));
              i1_ := (i + 1) - (i);
              for i_ := i to n - 2 do
                begin
                  TAU[i_] := C_Add(TAU[i_], C_Mul(alpha, a[i_ + i1_, i]));
                end;

              //
              // Apply the transformation as a rank-2 update:
              // A := A - v * w' - w * v'
              //
              i1_ := (i + 1) - (1);
              for i_ := 1 to n - i - 1 do
                begin
                  t[i_] := a[i_ + i1_, i];
                end;
              i1_ := (i) - (1);
              for i_ := 1 to n - i - 1 do
                begin
                  t2[i_] := TAU[i_ + i1_];
                end;
              HermitianRank2Update(a, IsUpper, i + 1, n - 1, t, t2, t3,
                C_Complex(-1));
            end
          else
            begin
              a[i + 1, i + 1] := C_Complex(a[i + 1, i + 1].x);
            end;
          a[i + 1, i] := C_Complex(E[i]);
          d[i] := a[i, i].x;
          TAU[i] := TAUI;
          inc(i);
        end;
      d[n - 1] := a[n - 1, n - 1].x;
    end;
end;

(* ************************************************************************
  Unpacking matrix Q which reduces a Hermitian matrix to a real  tridiagonal
  form.

  Input parameters:
  A       -   the result of a HMatrixTD subroutine
  N       -   size of matrix A.
  IsUpper -   storage format (a parameter of HMatrixTD subroutine)
  Tau     -   the result of a HMatrixTD subroutine

  Output parameters:
  Q       -   transformation matrix.
  array with elements [0..N-1, 0..N-1].
  ************************************************************************ *)
procedure HMatrixTDUnpackQ(const a: TLComplexMatrix; const n: TLInt;
  const IsUpper: Boolean; const TAU: TLComplexVec; var q: TLComplexMatrix);
var
  i   : TLInt;
  j   : TLInt;
  v   : TLComplexVec;
  Work: TLComplexVec;
  i_  : TLInt;
  i1_ : TLInt;
begin
  if n = 0 then
    begin
      Exit;
    end;

  //
  // init
  //
  SetLength(q, n  , n  );
  SetLength(v, n + 1);
  SetLength(Work, n  );
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          if i = j then
            begin
              q[i, j] := C_Complex(1);
            end
          else
            begin
              q[i, j] := C_Complex(0);
            end;
          inc(j);
        end;
      inc(i);
    end;

  //
  // unpack Q
  //
  if IsUpper then
    begin
      i := 0;
      while i <= n - 2 do
        begin

          //
          // Apply H(i)
          //
          i1_ := (0) - (1);
          for i_ := 1 to i + 1 do
            begin
              v[i_] := a[i_ + i1_, i + 1];
            end;
          v[i + 1] := C_Complex(1);
          ComplexApplyReflectionFromTheLeft(q, TAU[i], v, 0, i, 0, n - 1, Work);
          inc(i);
        end;
    end
  else
    begin
      i := n - 2;
      while i >= 0 do
        begin

          //
          // Apply H(i)
          //
          i1_ := (i + 1) - (1);
          for i_ := 1 to n - i - 1 do
            begin
              v[i_] := a[i_ + i1_, i];
            end;
          v[1] := C_Complex(1);
          ComplexApplyReflectionFromTheLeft(q, TAU[i], v, i + 1, n - 1, 0,
            n - 1, Work);
          dec(i);
        end;
    end;
end;

(* ************************************************************************
  Base case for real QR
  ************************************************************************ *)
procedure RMatrixQRBaseCase(var a: TLMatrix; M: TLInt;
  n: TLInt; var Work: TLVec; var t: TLVec;
  var TAU: TLVec);
var
  i    : TLInt;
  k    : TLInt;
  MinMN: TLInt;
  tmp  : TLFloat;
  i_   : TLInt;
  i1_  : TLInt;
begin
  MinMN := Min(M, n);

  //
  // Test the input arguments
  //
  k := MinMN;
  i := 0;
  while i <= k - 1 do
    begin

      //
      // Generate elementary reflector H(i) to annihilate A(i+1:m,i)
      //
      i1_ := (i) - (1);
      for i_ := 1 to M - i do
        begin
          t[i_] := a[i_ + i1_, i];
        end;
      GenerateReflection(t, M - i, tmp);
      TAU[i] := tmp;
      i1_ := (1) - (i);
      for i_ := i to M - 1 do
        begin
          a[i_, i] := t[i_ + i1_];
        end;
      t[1] := 1;
      if i < n then
        begin

          //
          // Apply H(i) to A(i:m-1,i+1:n-1) from the left
          //
          ApplyReflectionFromTheLeft(a, TAU[i], t, i, M - 1, i + 1, n - 1, Work);
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Base case for real LQ
  ************************************************************************ *)
procedure RMatrixLQBaseCase(var a: TLMatrix; M: TLInt;
  n: TLInt; var Work: TLVec; var t: TLVec;
  var TAU: TLVec);
var
  i    : TLInt;
  k    : TLInt;
  MinMN: TLInt;
  tmp  : TLFloat;
begin
  MinMN := Min(M, n);
  k := Min(M, n);
  i := 0;
  while i <= k - 1 do
    begin

      //
      // Generate elementary reflector H(i) to annihilate A(i,i+1:n-1)
      //
      APVMove(@t[0], 1, n - i, @a[i][0], i, n - 1);
      GenerateReflection(t, n - i, tmp);
      TAU[i] := tmp;
      APVMove(@a[i][0], i, n - 1, @t[0], 1, n - i);
      t[1] := 1;
      if i < n then
        begin

          //
          // Apply H(i) to A(i+1:m,i:n) from the right
          //
          ApplyReflectionFromTheRight(a, TAU[i], t, i + 1, M - 1, i, n - 1, Work);
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Base case for TLComplex QR
  ************************************************************************ *)
procedure CMatrixQRBaseCase(var a: TLComplexMatrix; M: TLInt;
  n: TLInt; var Work: TLComplexVec; var t: TLComplexVec;
  var TAU: TLComplexVec);
var
  i    : TLInt;
  k    : TLInt;
  MMI  : TLInt;
  MinMN: TLInt;
  tmp  : TLComplex;
  i_   : TLInt;
  i1_  : TLInt;
begin
  MinMN := Min(M, n);
  if MinMN <= 0 then
    begin
      Exit;
    end;

  //
  // Test the input arguments
  //
  k := Min(M, n);
  i := 0;
  while i <= k - 1 do
    begin

      //
      // Generate elementary reflector H(i) to annihilate A(i+1:m,i)
      //
      MMI := M - i;
      i1_ := (i) - (1);
      for i_ := 1 to MMI do
        begin
          t[i_] := a[i_ + i1_, i];
        end;
      ComplexGenerateReflection(t, MMI, tmp);
      TAU[i] := tmp;
      i1_ := (1) - (i);
      for i_ := i to M - 1 do
        begin
          a[i_, i] := t[i_ + i1_];
        end;
      t[1] := C_Complex(1);
      if i < n - 1 then
        begin

          //
          // Apply H'(i) to A(i:m,i+1:n) from the left
          //
          ComplexApplyReflectionFromTheLeft(a, Conj(TAU[i]), t, i, M - 1, i + 1,
            n - 1, Work);
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Base case for TLComplex LQ
  ************************************************************************ *)
procedure CMatrixLQBaseCase(var a: TLComplexMatrix; M: TLInt;
  n: TLInt; var Work: TLComplexVec; var t: TLComplexVec;
  var TAU: TLComplexVec);
var
  i    : TLInt;
  MinMN: TLInt;
  tmp  : TLComplex;
  i_   : TLInt;
  i1_  : TLInt;
begin
  MinMN := Min(M, n);
  if MinMN <= 0 then
    begin
      Exit;
    end;

  //
  // Test the input arguments
  //
  i := 0;
  while i <= MinMN - 1 do
    begin

      //
      // Generate elementary reflector H(i)
      //
      // NOTE: ComplexGenerateReflection() generates left reflector,
      // i.e. H which reduces x by applyiong from the left, but we
      // need RIGHT reflector. So we replace H=E-tau*v*v' by H^H,
      // which changes v to conj(v).
      //
      i1_ := (i) - (1);
      for i_ := 1 to n - i do
        begin
          t[i_] := Conj(a[i, i_ + i1_]);
        end;
      ComplexGenerateReflection(t, n - i, tmp);
      TAU[i] := tmp;
      i1_ := (1) - (i);
      for i_ := i to n - 1 do
        begin
          a[i, i_] := Conj(t[i_ + i1_]);
        end;
      t[1] := C_Complex(1);
      if i < M - 1 then
        begin

          //
          // Apply H'(i)
          //
          ComplexApplyReflectionFromTheRight(a, TAU[i], t, i + 1, M - 1, i,
            n - 1, Work);
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Generate block reflector:
  * fill unused parts of reflectors matrix by zeros
  * fill diagonal of reflectors matrix by ones
  * generate triangular factor T

  PARAMETERS:
  A           -   either LengthA*BlockSize (if ColumnwiseA) or
  BlockSize*LengthA (if not ColumnwiseA) matrix of
  elementary reflectors.
  Modified on exit.
  Tau         -   scalar factors
  ColumnwiseA -   reflectors are stored in rows or in columns
  LengthA     -   length of largest reflector
  BlockSize   -   number of reflectors
  T           -   array[BlockSize,2*BlockSize]. Left BlockSize*BlockSize
  submatrix stores triangular factor on exit.
  WORK        -   array[BlockSize]
  ************************************************************************ *)
procedure RMatrixBlockReflector(var a: TLMatrix; var TAU: TLVec;
  ColumnwiseA: Boolean; LengthA: TLInt; BlockSize: TLInt;
  var t: TLMatrix; var Work: TLVec);
var
  i  : TLInt;
  j  : TLInt;
  k  : TLInt;
  v  : TLFloat;
  i_ : TLInt;
  i1_: TLInt;
begin

  //
  // fill beginning of new column with zeros,
  // load 1.0 in the first non-zero element
  //
  k := 0;
  while k <= BlockSize - 1 do
    begin
      if ColumnwiseA then
        begin
          i := 0;
          while i <= k - 1 do
            begin
              a[i, k] := 0;
              inc(i);
            end;
        end
      else
        begin
          i := 0;
          while i <= k - 1 do
            begin
              a[k, i] := 0;
              inc(i);
            end;
        end;
      a[k, k] := 1;
      inc(k);
    end;

  //
  // Calculate Gram matrix of A
  //
  i := 0;
  while i <= BlockSize - 1 do
    begin
      j := 0;
      while j <= BlockSize - 1 do
        begin
          t[i, BlockSize + j] := 0;
          inc(j);
        end;
      inc(i);
    end;
  k := 0;
  while k <= LengthA - 1 do
    begin
      j := 1;
      while j <= BlockSize - 1 do
        begin
          if ColumnwiseA then
            begin
              v := a[k, j];
              if AP_FP_NEq(v, 0) then
                begin
                  APVAdd(@t[j][0], BlockSize, BlockSize + j - 1, @a[k][0], 0, j - 1, v);
                end;
            end
          else
            begin
              v := a[j, k];
              if AP_FP_NEq(v, 0) then
                begin
                  i1_ := (0) - (BlockSize);
                  for i_ := BlockSize to BlockSize + j - 1 do
                    begin
                      t[j, i_] := t[j, i_] + v * a[i_ + i1_, k];
                    end;
                end;
            end;
          inc(j);
        end;
      inc(k);
    end;

  //
  // Prepare Y (stored in TmpA) and T (stored in TmpT)
  //
  k := 0;
  while k <= BlockSize - 1 do
    begin

      //
      // fill non-zero part of T, use pre-calculated Gram matrix
      //
      APVMove(@Work[0], 0, k - 1, @t[k][0], BlockSize, BlockSize + k - 1);
      i := 0;
      while i <= k - 1 do
        begin
          v := APVDotProduct(@t[i][0], i, k - 1, @Work[0], i, k - 1);
          t[i, k] := -TAU[k] * v;
          inc(i);
        end;
      t[k, k] := -TAU[k];

      //
      // Rest of T is filled by zeros
      //
      i := k + 1;
      while i <= BlockSize - 1 do
        begin
          t[i, k] := 0;
          inc(i);
        end;
      inc(k);
    end;
end;

(* ************************************************************************
  Generate block reflector (TLComplex):
  * fill unused parts of reflectors matrix by zeros
  * fill diagonal of reflectors matrix by ones
  * generate triangular factor T
  ************************************************************************ *)
procedure CMatrixBlockReflector(var a: TLComplexMatrix;
  var TAU: TLComplexVec; ColumnwiseA: Boolean; LengthA: TLInt;
  BlockSize: TLInt; var t: TLComplexMatrix; var Work: TLComplexVec);
var
  i : TLInt;
  k : TLInt;
  v : TLComplex;
  i_: TLInt;
begin

  //
  // Prepare Y (stored in TmpA) and T (stored in TmpT)
  //
  k := 0;
  while k <= BlockSize - 1 do
    begin

      //
      // fill beginning of new column with zeros,
      // load 1.0 in the first non-zero element
      //
      if ColumnwiseA then
        begin
          i := 0;
          while i <= k - 1 do
            begin
              a[i, k] := C_Complex(0);
              inc(i);
            end;
        end
      else
        begin
          i := 0;
          while i <= k - 1 do
            begin
              a[k, i] := C_Complex(0);
              inc(i);
            end;
        end;
      a[k, k] := C_Complex(1);

      //
      // fill non-zero part of T,
      //
      i := 0;
      while i <= k - 1 do
        begin
          if ColumnwiseA then
            begin
              v := C_Complex(0.0);
              for i_ := k to LengthA - 1 do
                begin
                  v := C_Add(v, C_Mul(Conj(a[i_, i]), a[i_, k]));
                end;
            end
          else
            begin
              v := C_Complex(0.0);
              for i_ := k to LengthA - 1 do
                begin
                  v := C_Add(v, C_Mul(a[i, i_], Conj(a[k, i_])));
                end;
            end;
          Work[i] := v;
          inc(i);
        end;
      i := 0;
      while i <= k - 1 do
        begin
          v := C_Complex(0.0);
          for i_ := i to k - 1 do
            begin
              v := C_Add(v, C_Mul(t[i, i_], Work[i_]));
            end;
          t[i, k] := C_Opposite(C_Mul(TAU[k], v));
          inc(i);
        end;
      t[k, k] := C_Opposite(TAU[k]);

      //
      // Rest of T is filled by zeros
      //
      i := k + 1;
      while i <= BlockSize - 1 do
        begin
          t[i, k] := C_Complex(0);
          inc(i);
        end;
      inc(k);
    end;
end; 
 
 
