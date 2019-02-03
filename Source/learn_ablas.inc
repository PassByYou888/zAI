{ ****************************************************************************** }
{ * Level 2 and Level 3 BLAS operations support, by QQ 600585@qq.com           * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

procedure ABLASInternalSplitLength(n: TLInt; nb: TLInt; var n1: TLInt; var n2: TLInt); forward;

procedure CMatrixRightTRSM2(M: TLInt; n: TLInt;
  const a: TLComplexMatrix; i1: TLInt; j1: TLInt; IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var x: TLComplexMatrix; i2: TLInt; j2: TLInt); forward;

procedure CMatrixLeftTRSM2(M: TLInt; n: TLInt;
  const a: TLComplexMatrix; i1: TLInt; j1: TLInt; IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var x: TLComplexMatrix; i2: TLInt; j2: TLInt); forward;

procedure RMatrixRightTRSM2(M: TLInt; n: TLInt;
  const a: TLMatrix; i1: TLInt; j1: TLInt; IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt; var x: TLMatrix; i2: TLInt; j2: TLInt); forward;

procedure RMatrixLeftTRSM2(M: TLInt; n: TLInt;
  const a: TLMatrix; i1: TLInt; j1: TLInt; IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt; var x: TLMatrix; i2: TLInt; j2: TLInt); forward;

procedure CMatrixSYRK2(n: TLInt; k: TLInt; alpha: TLFloat;
  const a: TLComplexMatrix; IA: TLInt; ja: TLInt; OpTypeA: TLInt; beta: TLFloat; var c: TLComplexMatrix; IC: TLInt; JC: TLInt; IsUpper: Boolean); forward;

procedure RMatrixSYRK2(n: TLInt; k: TLInt; alpha: TLFloat;
  const a: TLMatrix; IA: TLInt; ja: TLInt;
  OpTypeA: TLInt; beta: TLFloat; var c: TLMatrix;
  IC: TLInt; JC: TLInt; IsUpper: Boolean); forward;

procedure CMatrixGEMMK(M: TLInt; n: TLInt; k: TLInt;
  alpha: TLComplex; const a: TLComplexMatrix; IA: TLInt;
  ja: TLInt; OpTypeA: TLInt; const b: TLComplexMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt; beta: TLComplex;
  var c: TLComplexMatrix; IC: TLInt; JC: TLInt); forward;

procedure RMatrixGEMMK(M: TLInt; n: TLInt; k: TLInt;
  alpha: TLFloat; const a: TLMatrix; IA: TLInt;
  ja: TLInt; OpTypeA: TLInt; const b: TLMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt;
  beta: TLFloat; var c: TLMatrix; IC: TLInt;
  JC: TLInt); forward;

(* ************************************************************************
  Splits matrix length in two parts, left part should match ABLAS block size

  INPUT PARAMETERS
  A   -   real matrix, is passed to ensure that we didn't split TLComplex matrix using real splitting subroutine. matrix itself is not changed.
  N   -   length, N>0

  OUTPUT PARAMETERS
  N1  -   length
  N2  -   length

  N1+N2=N, N1>=N2, N2 may be zero
  ************************************************************************ *)
procedure ABLASSplitLength(const a: TLMatrix; n: TLInt;
  var n1: TLInt; var n2: TLInt);
begin
  if n > ABLASBlockSize(a) then
    begin
      ABLASInternalSplitLength(n, ABLASBlockSize(a), n1, n2);
    end
  else
    begin
      ABLASInternalSplitLength(n, ABLASMicroBlockSize, n1, n2);
    end;
end;

(* ************************************************************************
  TLComplex ABLASSplitLength
  ************************************************************************ *)
procedure ABLASComplexSplitLength(const a: TLComplexMatrix; n: TLInt;
  var n1: TLInt; var n2: TLInt);
begin
  if n > ABLASComplexBlockSize(a) then
    begin
      ABLASInternalSplitLength(n, ABLASComplexBlockSize(a), n1, n2);
    end
  else
    begin
      ABLASInternalSplitLength(n, ABLASMicroBlockSize, n1, n2);
    end;
end;

(* ************************************************************************
  Returns block size - subdivision size where  cache-oblivious  soubroutines switch to the optimized kernel.

  INPUT PARAMETERS
  A   -   real matrix, is passed to ensure that we didn't split TLComplex matrix using real splitting subroutine. matrix itself is not changed.
  ************************************************************************ *)
function ABLASBlockSize(const a: TLMatrix): TLInt;
begin
  Result := 32;
end;

(* ************************************************************************
  Block size for TLComplex subroutines.
  ************************************************************************ *)
function ABLASComplexBlockSize(const a: TLComplexMatrix): TLInt;
begin
  Result := 24;
end;

(* ************************************************************************
  Microblock size
  ************************************************************************ *)
function ABLASMicroBlockSize(): TLInt;
begin
  Result := 8;
end;

(* ************************************************************************
  Cache-oblivous TLComplex "copy-and-transpose"

  Input parameters:
  M   -   number of rows
  N   -   number of columns
  A   -   source matrix, MxN submatrix is copied and transposed
  IA  -   submatrix offset (row index)
  JA  -   submatrix offset (column index)
  A   -   destination matrix
  IB  -   submatrix offset (row index)
  JB  -   submatrix offset (column index)
  ************************************************************************ *)
procedure CMatrixTranspose(M: TLInt; n: TLInt;
  const a: TLComplexMatrix; IA: TLInt; ja: TLInt;
  var b: TLComplexMatrix; IB: TLInt; JB: TLInt);
var
  i: TLInt;
  s1: TLInt;
  s2: TLInt;
  i_: TLInt;
  i1_: TLInt;
begin
  if (M <= 2 * ABLASComplexBlockSize(a)) and (n <= 2 * ABLASComplexBlockSize(a))
  then
    begin

      //
      // base case
      //
      i := 0;
      while i <= M - 1 do
        begin
          i1_ := (ja) - (IB);
          for i_ := IB to IB + n - 1 do
            begin
              b[i_, JB + i] := a[IA + i, i_ + i1_];
            end;
          inc(i);
        end;
    end
  else
    begin

      //
      // Cache-oblivious recursion
      //
      if M > n then
        begin
          ABLASComplexSplitLength(a, M, s1, s2);
          CMatrixTranspose(s1, n, a, IA, ja, b, IB, JB);
          CMatrixTranspose(s2, n, a, IA + s1, ja, b, IB, JB + s1);
        end
      else
        begin
          ABLASComplexSplitLength(a, n, s1, s2);
          CMatrixTranspose(M, s1, a, IA, ja, b, IB, JB);
          CMatrixTranspose(M, s2, a, IA, ja + s1, b, IB + s1, JB);
        end;
    end;
end;

(* ************************************************************************
  Cache-oblivous real "copy-and-transpose"

  Input parameters:
  M   -   number of rows
  N   -   number of columns
  A   -   source matrix, MxN submatrix is copied and transposed
  IA  -   submatrix offset (row index)
  JA  -   submatrix offset (column index)
  A   -   destination matrix
  IB  -   submatrix offset (row index)
  JB  -   submatrix offset (column index)
  ************************************************************************ *)
procedure RMatrixTranspose(M: TLInt; n: TLInt;
  const a: TLMatrix; IA: TLInt; ja: TLInt;
  var b: TLMatrix; IB: TLInt; JB: TLInt);
var
  i: TLInt;
  s1: TLInt;
  s2: TLInt;
  i_: TLInt;
  i1_: TLInt;
begin
  if (M <= 2 * ABLASBlockSize(a)) and (n <= 2 * ABLASBlockSize(a)) then
    begin

      //
      // base case
      //
      i := 0;
      while i <= M - 1 do
        begin
          i1_ := (ja) - (IB);
          for i_ := IB to IB + n - 1 do
            begin
              b[i_, JB + i] := a[IA + i, i_ + i1_];
            end;
          inc(i);
        end;
    end
  else
    begin

      //
      // Cache-oblivious recursion
      //
      if M > n then
        begin
          ABLASSplitLength(a, M, s1, s2);
          RMatrixTranspose(s1, n, a, IA, ja, b, IB, JB);
          RMatrixTranspose(s2, n, a, IA + s1, ja, b, IB, JB + s1);
        end
      else
        begin
          ABLASSplitLength(a, n, s1, s2);
          RMatrixTranspose(M, s1, a, IA, ja, b, IB, JB);
          RMatrixTranspose(M, s2, a, IA, ja + s1, b, IB + s1, JB);
        end;
    end;
end;

(* ************************************************************************
  Copy

  Input parameters:
  M   -   number of rows
  N   -   number of columns
  A   -   source matrix, MxN submatrix is copied and transposed
  IA  -   submatrix offset (row index)
  JA  -   submatrix offset (column index)
  B   -   destination matrix
  IB  -   submatrix offset (row index)
  JB  -   submatrix offset (column index)
  ************************************************************************ *)
procedure CMatrixCopy(M: TLInt; n: TLInt;
  const a: TLComplexMatrix; IA: TLInt; ja: TLInt;
  var b: TLComplexMatrix; IB: TLInt; JB: TLInt);
var
  i: TLInt;
  i_: TLInt;
  i1_: TLInt;
begin
  i := 0;
  while i <= M - 1 do
    begin
      i1_ := (ja) - (JB);
      for i_ := JB to JB + n - 1 do
        begin
          b[IB + i, i_] := a[IA + i, i_ + i1_];
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Copy

  Input parameters:
  M   -   number of rows
  N   -   number of columns
  A   -   source matrix, MxN submatrix is copied and transposed
  IA  -   submatrix offset (row index)
  JA  -   submatrix offset (column index)
  B   -   destination matrix
  IB  -   submatrix offset (row index)
  JB  -   submatrix offset (column index)
  ************************************************************************ *)
procedure RMatrixCopy(M: TLInt; n: TLInt; const a: TLMatrix;
  IA: TLInt; ja: TLInt; var b: TLMatrix; IB: TLInt;
  JB: TLInt);
var
  i: TLInt;
begin
  i := 0;
  while i <= M - 1 do
    begin
      APVMove(@b[IB + i][0], JB, JB + n - 1, @a[IA + i][0], ja, ja + n - 1);
      inc(i);
    end;
end;

(* ************************************************************************
  Rank-1 correction: A := A + u*v'

  INPUT PARAMETERS:
  M   -   number of rows
  N   -   number of columns
  A   -   target matrix, MxN submatrix is updated
  IA  -   submatrix offset (row index)
  JA  -   submatrix offset (column index)
  U   -   vector #1
  IU  -   subvector offset
  V   -   vector #2
  IV  -   subvector offset
  ************************************************************************ *)
procedure CMatrixRank1(M: TLInt; n: TLInt;
  var a: TLComplexMatrix; IA: TLInt; ja: TLInt;
  var u: TLComplexVec; IU: TLInt; var v: TLComplexVec;
  IV: TLInt);
var
  i: TLInt;
  s: TLComplex;
  i_: TLInt;
  i1_: TLInt;
begin
  if (M = 0) or (n = 0) then
      Exit;
  i := 0;
  while i <= M - 1 do
    begin
      s := u[IU + i];
      i1_ := (IV) - (ja);
      for i_ := ja to ja + n - 1 do
        begin
          a[IA + i, i_] := C_Add(a[IA + i, i_], C_Mul(s, v[i_ + i1_]));
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Rank-1 correction: A := A + u*v'

  INPUT PARAMETERS:
  M   -   number of rows
  N   -   number of columns
  A   -   target matrix, MxN submatrix is updated
  IA  -   submatrix offset (row index)
  JA  -   submatrix offset (column index)
  U   -   vector #1
  IU  -   subvector offset
  V   -   vector #2
  IV  -   subvector offset
  ************************************************************************ *)
procedure RMatrixRank1(M: TLInt; n: TLInt; var a: TLMatrix;
  IA: TLInt; ja: TLInt; var u: TLVec; IU: TLInt;
  var v: TLVec; IV: TLInt);
var
  i: TLInt;
  s: TLFloat;
begin
  if (M = 0) or (n = 0) then
      Exit;
  i := 0;
  while i <= M - 1 do
    begin
      s := u[IU + i];
      APVAdd(@a[IA + i][0], ja, ja + n - 1, @v[0], IV, IV + n - 1, s);
      inc(i);
    end;
end;

(* ************************************************************************
  Matrix-vector product: y := op(A)*x

  INPUT PARAMETERS:
  M   -   number of rows of op(A)
  M>=0
  N   -   number of columns of op(A)
  N>=0
  A   -   target matrix
  IA  -   submatrix offset (row index)
  JA  -   submatrix offset (column index)
  OpA -   operation type:
  * OpA=0     =>  op(A) = A
  * OpA=1     =>  op(A) = A^T
  * OpA=2     =>  op(A) = A^H
  X   -   input vector
  IX  -   subvector offset
  IY  -   subvector offset

  OUTPUT PARAMETERS:
  Y   -   vector which stores result

  if M=0, then subroutine does nothing.
  if N=0, Y is filled by zeros.
  ************************************************************************ *)
procedure CMatrixMV(M: TLInt; n: TLInt; var a: TLComplexMatrix;
  IA: TLInt; ja: TLInt; OpA: TLInt;
  var x: TLComplexVec; ix: TLInt; var y: TLComplexVec;
  iy: TLInt);
var
  i: TLInt;
  v: TLComplex;
  i_: TLInt;
  i1_: TLInt;
begin
  if M = 0 then
      Exit;
  if n = 0 then
    begin
      i := 0;
      while i <= M - 1 do
        begin
          y[iy + i] := C_Complex(0);
          inc(i);
        end;
      Exit;
    end;
  if OpA = 0 then
    begin

      //
      // y = A*x
      //
      i := 0;
      while i <= M - 1 do
        begin
          i1_ := (ix) - (ja);
          v := C_Complex(0.0);
          for i_ := ja to ja + n - 1 do
            begin
              v := C_Add(v, C_Mul(a[IA + i, i_], x[i_ + i1_]));
            end;
          y[iy + i] := v;
          inc(i);
        end;
      Exit;
    end;
  if OpA = 1 then
    begin

      //
      // y = A^T*x
      //
      i := 0;
      while i <= M - 1 do
        begin
          y[iy + i] := C_Complex(0);
          inc(i);
        end;
      i := 0;
      while i <= n - 1 do
        begin
          v := x[ix + i];
          i1_ := (ja) - (iy);
          for i_ := iy to iy + M - 1 do
            begin
              y[i_] := C_Add(y[i_], C_Mul(v, a[IA + i, i_ + i1_]));
            end;
          inc(i);
        end;
      Exit;
    end;
  if OpA = 2 then
    begin

      //
      // y = A^H*x
      //
      i := 0;
      while i <= M - 1 do
        begin
          y[iy + i] := C_Complex(0);
          inc(i);
        end;
      i := 0;
      while i <= n - 1 do
        begin
          v := x[ix + i];
          i1_ := (ja) - (iy);
          for i_ := iy to iy + M - 1 do
            begin
              y[i_] := C_Add(y[i_], C_Mul(v, Conj(a[IA + i, i_ + i1_])));
            end;
          inc(i);
        end;
      Exit;
    end;
end;

(* ************************************************************************
  Matrix-vector product: y := op(A)*x

  INPUT PARAMETERS:
  M   -   number of rows of op(A)
  N   -   number of columns of op(A)
  A   -   target matrix
  IA  -   submatrix offset (row index)
  JA  -   submatrix offset (column index)
  OpA -   operation type:
  * OpA=0     =>  op(A) = A
  * OpA=1     =>  op(A) = A^T
  X   -   input vector
  IX  -   subvector offset
  IY  -   subvector offset

  OUTPUT PARAMETERS:
  Y   -   vector which stores result

  if M=0, then subroutine does nothing.
  if N=0, Y is filled by zeros.
  ************************************************************************ *)
procedure RMatrixMV(M: TLInt; n: TLInt; var a: TLMatrix;
  IA: TLInt; ja: TLInt; OpA: TLInt; var x: TLVec;
  ix: TLInt; var y: TLVec; iy: TLInt);
var
  i: TLInt;
  v: TLFloat;
begin
  if M = 0 then
      Exit;
  if n = 0 then
    begin
      i := 0;
      while i <= M - 1 do
        begin
          y[iy + i] := 0;
          inc(i);
        end;
      Exit;
    end;
  if OpA = 0 then
    begin

      //
      // y = A*x
      //
      i := 0;
      while i <= M - 1 do
        begin
          v := APVDotProduct(@a[IA + i][0], ja, ja + n - 1, @x[0], ix, ix + n - 1);
          y[iy + i] := v;
          inc(i);
        end;
      Exit;
    end;
  if OpA = 1 then
    begin

      //
      // y = A^T*x
      //
      i := 0;
      while i <= M - 1 do
        begin
          y[iy + i] := 0;
          inc(i);
        end;
      i := 0;
      while i <= n - 1 do
        begin
          v := x[ix + i];
          APVAdd(@y[0], iy, iy + M - 1, @a[IA + i][0], ja, ja + M - 1, v);
          inc(i);
        end;
      Exit;
    end;
end;

(* ************************************************************************
  This subroutine calculates X*op(A^-1) where:
  * X is MxN general matrix
  * A is NxN upper/lower triangular/unitriangular matrix
  * "op" may be identity transformation, transposition, conjugate transposition

  Multiplication result replaces X.
  Cache-oblivious algorithm is used.

  INPUT PARAMETERS
  N   -   matrix size, N>=0
  M   -   matrix size, N>=0
  A       -   matrix, actial matrix is stored in A[I1:I1+N-1,J1:J1+N-1]
  I1      -   submatrix offset
  J1      -   submatrix offset
  IsUpper -   whether matrix is upper triangular
  IsUnit  -   whether matrix is unitriangular
  OpType  -   transformation type:
  * 0 - no transformation
  * 1 - transposition
  * 2 - conjugate transposition
  C   -   matrix, actial matrix is stored in C[I2:I2+M-1,J2:J2+N-1]
  I2  -   submatrix offset
  J2  -   submatrix offset
  ************************************************************************ *)
procedure CMatrixRightTRSM(M: TLInt; n: TLInt;
  const a: TLComplexMatrix; i1: TLInt; j1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var x: TLComplexMatrix; i2: TLInt; j2: TLInt);
var
  s1: TLInt;
  s2: TLInt;
  bs: TLInt;
begin
  bs := ABLASComplexBlockSize(a);
  if (M <= bs) and (n <= bs) then
    begin
      CMatrixRightTRSM2(M, n, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2);
      Exit;
    end;
  if M >= n then
    begin

      //
      // Split X: X*A = (X1 X2)^T*A
      //
      ABLASComplexSplitLength(a, M, s1, s2);
      CMatrixRightTRSM(s1, n, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2);
      CMatrixRightTRSM(s2, n, a, i1, j1, IsUpper, IsUnit, OpType, x, i2 + s1, j2);
    end
  else
    begin

      //
      // Split A:
      // (A1  A12)
      // X*op(A) = X*op(       )
      // (     A2)
      //
      // Different variants depending on
      // IsUpper/OpType combinations
      //
      ABLASComplexSplitLength(a, n, s1, s2);
      if IsUpper and (OpType = 0) then
        begin

          //
          // (A1  A12)-1
          // X*A^-1 = (X1 X2)*(       )
          // (     A2)
          //
          CMatrixRightTRSM(M, s1, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2);
          CMatrixGEMM(M, s2, s1, C_Complex(-1.0), x, i2, j2, 0, a, i1, j1 + s1, 0, C_Complex(1.0), x, i2, j2 + s1);
          CMatrixRightTRSM(M, s2, a, i1 + s1, j1 + s1, IsUpper, IsUnit, OpType, x, i2, j2 + s1);
          Exit;
        end;
      if IsUpper and (OpType <> 0) then
        begin

          //
          // (A1'     )-1
          // X*A^-1 = (X1 X2)*(        )
          // (A12' A2')
          //
          CMatrixRightTRSM(M, s2, a, i1 + s1, j1 + s1, IsUpper, IsUnit, OpType, x, i2, j2 + s1);
          CMatrixGEMM(M, s1, s2, C_Complex(-1.0), x, i2, j2 + s1, 0, a, i1, j1 + s1, OpType, C_Complex(1.0), x, i2, j2);
          CMatrixRightTRSM(M, s1, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2);
          Exit;
        end;
      if not IsUpper and (OpType = 0) then
        begin

          //
          // (A1     )-1
          // X*A^-1 = (X1 X2)*(       )
          // (A21  A2)
          //
          CMatrixRightTRSM(M, s2, a, i1 + s1, j1 + s1, IsUpper, IsUnit, OpType, x, i2, j2 + s1);
          CMatrixGEMM(M, s1, s2, C_Complex(-1.0), x, i2, j2 + s1, 0, a, i1 + s1, j1, 0, C_Complex(1.0), x, i2, j2);
          CMatrixRightTRSM(M, s1, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2);
          Exit;
        end;
      if not IsUpper and (OpType <> 0) then
        begin

          //
          // (A1' A21')-1
          // X*A^-1 = (X1 X2)*(        )
          // (     A2')
          //
          CMatrixRightTRSM(M, s1, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2);
          CMatrixGEMM(M, s2, s1, C_Complex(-1.0), x, i2, j2, 0, a, i1 + s1, j1, OpType, C_Complex(1.0), x, i2, j2 + s1);
          CMatrixRightTRSM(M, s2, a, i1 + s1, j1 + s1, IsUpper, IsUnit, OpType, x, i2, j2 + s1);
          Exit;
        end;
    end;
end;

(* ************************************************************************
  This subroutine calculates op(A^-1)*X where:
  * X is MxN general matrix
  * A is MxM upper/lower triangular/unitriangular matrix
  * "op" may be identity transformation, transposition, conjugate transposition

  Multiplication result replaces X.
  Cache-oblivious algorithm is used.

  INPUT PARAMETERS
  N   -   matrix size, N>=0
  M   -   matrix size, N>=0
  A       -   matrix, actial matrix is stored in A[I1:I1+M-1,J1:J1+M-1]
  I1      -   submatrix offset
  J1      -   submatrix offset
  IsUpper -   whether matrix is upper triangular
  IsUnit  -   whether matrix is unitriangular
  OpType  -   transformation type:
  * 0 - no transformation
  * 1 - transposition
  * 2 - conjugate transposition
  C   -   matrix, actial matrix is stored in C[I2:I2+M-1,J2:J2+N-1]
  I2  -   submatrix offset
  J2  -   submatrix offset
  ************************************************************************ *)
procedure CMatrixLeftTRSM(M: TLInt; n: TLInt;
  const a: TLComplexMatrix; i1: TLInt; j1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var x: TLComplexMatrix; i2: TLInt; j2: TLInt);
var
  s1: TLInt;
  s2: TLInt;
  bs: TLInt;
begin
  bs := ABLASComplexBlockSize(a);
  if (M <= bs) and (n <= bs) then
    begin
      CMatrixLeftTRSM2(M, n, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2);
      Exit;
    end;
  if n >= M then
    begin

      //
      // Split X: op(A)^-1*X = op(A)^-1*(X1 X2)
      //
      ABLASComplexSplitLength(x, n, s1, s2);
      CMatrixLeftTRSM(M, s1, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2);
      CMatrixLeftTRSM(M, s2, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2 + s1);
    end
  else
    begin

      //
      // Split A
      //
      ABLASComplexSplitLength(a, M, s1, s2);
      if IsUpper and (OpType = 0) then
        begin
          CMatrixLeftTRSM(s2, n, a, i1 + s1, j1 + s1, IsUpper, IsUnit, OpType, x, i2 + s1, j2);
          CMatrixGEMM(s1, n, s2, C_Complex(-1.0), a, i1, j1 + s1, 0, x, i2 + s1, j2, 0, C_Complex(1.0), x, i2, j2);
          CMatrixLeftTRSM(s1, n, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2);
          Exit;
        end;
      if IsUpper and (OpType <> 0) then
        begin
          CMatrixLeftTRSM(s1, n, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2);
          CMatrixGEMM(s2, n, s1, C_Complex(-1.0), a, i1, j1 + s1, OpType, x, i2, j2, 0, C_Complex(1.0), x, i2 + s1, j2);
          CMatrixLeftTRSM(s2, n, a, i1 + s1, j1 + s1, IsUpper, IsUnit, OpType, x, i2 + s1, j2);
          Exit;
        end;
      if not IsUpper and (OpType = 0) then
        begin
          CMatrixLeftTRSM(s1, n, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2);
          CMatrixGEMM(s2, n, s1, C_Complex(-1.0), a, i1 + s1, j1, 0, x, i2, j2, 0, C_Complex(1.0), x, i2 + s1, j2);
          CMatrixLeftTRSM(s2, n, a, i1 + s1, j1 + s1, IsUpper, IsUnit, OpType, x, i2 + s1, j2);
          Exit;
        end;
      if not IsUpper and (OpType <> 0) then
        begin
          CMatrixLeftTRSM(s2, n, a, i1 + s1, j1 + s1, IsUpper, IsUnit, OpType, x, i2 + s1, j2);
          CMatrixGEMM(s1, n, s2, C_Complex(-1.0), a, i1 + s1, j1, OpType, x, i2 + s1, j2, 0, C_Complex(1.0), x, i2, j2);
          CMatrixLeftTRSM(s1, n, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2);
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Same as CMatrixRightTRSM, but for real matrices

  OpType may be only 0 or 1.
  ************************************************************************ *)
procedure RMatrixRightTRSM(M: TLInt; n: TLInt;
  const a: TLMatrix; i1: TLInt; j1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var x: TLMatrix;
  i2: TLInt; j2: TLInt);
var
  s1: TLInt;
  s2: TLInt;
  bs: TLInt;
begin
  bs := ABLASBlockSize(a);
  if (M <= bs) and (n <= bs) then
    begin
      RMatrixRightTRSM2(M, n, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2);
      Exit;
    end;
  if M >= n then
    begin

      //
      // Split X: X*A = (X1 X2)^T*A
      //
      ABLASSplitLength(a, M, s1, s2);
      RMatrixRightTRSM(s1, n, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2);
      RMatrixRightTRSM(s2, n, a, i1, j1, IsUpper, IsUnit, OpType, x, i2 + s1, j2);
    end
  else
    begin
      // Different variants depending on
      // IsUpper/OpType combinations
      //
      ABLASSplitLength(a, n, s1, s2);
      if IsUpper and (OpType = 0) then
        begin
          RMatrixRightTRSM(M, s1, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2);
          RMatrixGEMM(M, s2, s1, -1.0, x, i2, j2, 0, a, i1, j1 + s1, 0, 1.0, x, i2, j2 + s1);
          RMatrixRightTRSM(M, s2, a, i1 + s1, j1 + s1, IsUpper, IsUnit, OpType, x, i2, j2 + s1);
          Exit;
        end;
      if IsUpper and (OpType <> 0) then
        begin
          RMatrixRightTRSM(M, s2, a, i1 + s1, j1 + s1, IsUpper, IsUnit, OpType, x, i2, j2 + s1);
          RMatrixGEMM(M, s1, s2, -1.0, x, i2, j2 + s1, 0, a, i1, j1 + s1, OpType, 1.0, x, i2, j2);
          RMatrixRightTRSM(M, s1, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2);
          Exit;
        end;
      if not IsUpper and (OpType = 0) then
        begin
          RMatrixRightTRSM(M, s2, a, i1 + s1, j1 + s1, IsUpper, IsUnit, OpType, x, i2, j2 + s1);
          RMatrixGEMM(M, s1, s2, -1.0, x, i2, j2 + s1, 0, a, i1 + s1, j1, 0, 1.0, x, i2, j2);
          RMatrixRightTRSM(M, s1, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2);
          Exit;
        end;
      if not IsUpper and (OpType <> 0) then
        begin
          RMatrixRightTRSM(M, s1, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2);
          RMatrixGEMM(M, s2, s1, -1.0, x, i2, j2, 0, a, i1 + s1, j1, OpType, 1.0, x, i2, j2 + s1);
          RMatrixRightTRSM(M, s2, a, i1 + s1, j1 + s1, IsUpper, IsUnit, OpType, x, i2, j2 + s1);
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Same as CMatrixLeftTRSM, but for real matrices

  OpType may be only 0 or 1.
  ************************************************************************ *)
procedure RMatrixLeftTRSM(M: TLInt; n: TLInt;
  const a: TLMatrix; i1: TLInt; j1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var x: TLMatrix;
  i2: TLInt; j2: TLInt);
var
  s1: TLInt;
  s2: TLInt;
  bs: TLInt;
begin
  bs := ABLASBlockSize(a);
  if (M <= bs) and (n <= bs) then
    begin
      RMatrixLeftTRSM2(M, n, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2);
      Exit;
    end;
  if n >= M then
    begin

      //
      // Split X: op(A)^-1*X = op(A)^-1*(X1 X2)
      //
      ABLASSplitLength(x, n, s1, s2);
      RMatrixLeftTRSM(M, s1, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2);
      RMatrixLeftTRSM(M, s2, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2 + s1);
    end
  else
    begin

      //
      // Split A
      //
      ABLASSplitLength(a, M, s1, s2);
      if IsUpper and (OpType = 0) then
        begin
          RMatrixLeftTRSM(s2, n, a, i1 + s1, j1 + s1, IsUpper, IsUnit, OpType, x, i2 + s1, j2);
          RMatrixGEMM(s1, n, s2, -1.0, a, i1, j1 + s1, 0, x, i2 + s1, j2, 0, 1.0, x, i2, j2);
          RMatrixLeftTRSM(s1, n, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2);
          Exit;
        end;
      if IsUpper and (OpType <> 0) then
        begin
          RMatrixLeftTRSM(s1, n, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2);
          RMatrixGEMM(s2, n, s1, -1.0, a, i1, j1 + s1, OpType, x, i2, j2, 0, 1.0, x, i2 + s1, j2);
          RMatrixLeftTRSM(s2, n, a, i1 + s1, j1 + s1, IsUpper, IsUnit, OpType, x, i2 + s1, j2);
          Exit;
        end;
      if not IsUpper and (OpType = 0) then
        begin
          RMatrixLeftTRSM(s1, n, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2);
          RMatrixGEMM(s2, n, s1, -1.0, a, i1 + s1, j1, 0, x, i2, j2, 0, 1.0, x, i2 + s1, j2);
          RMatrixLeftTRSM(s2, n, a, i1 + s1, j1 + s1, IsUpper, IsUnit, OpType, x, i2 + s1, j2);
          Exit;
        end;
      if not IsUpper and (OpType <> 0) then
        begin
          RMatrixLeftTRSM(s2, n, a, i1 + s1, j1 + s1, IsUpper, IsUnit, OpType, x, i2 + s1, j2);
          RMatrixGEMM(s1, n, s2, -1.0, a, i1 + s1, j1, OpType, x, i2 + s1, j2, 0, 1.0, x, i2, j2);
          RMatrixLeftTRSM(s1, n, a, i1, j1, IsUpper, IsUnit, OpType, x, i2, j2);
          Exit;
        end;
    end;
end;

(* ************************************************************************
  This subroutine calculates  C=alpha*A*A^H+beta*C  or  C=alpha*A^H*A+beta*C
  where:
  * C is NxN Hermitian matrix given by its upper/lower triangle
  * A is NxK matrix when A*A^H is calculated, KxN matrix otherwise

  Additional info:
  * cache-oblivious algorithm is used.
  * multiplication result replaces C. If Beta=0, C elements are not used in calculations (not multiplied by zero - just not referenced)
  * if Alpha=0, A is not used (not multiplied by zero - just not referenced)
  * if both Beta and Alpha are zero, C is filled by zeros.

  INPUT PARAMETERS
  N       -   matrix size, N>=0
  K       -   matrix size, K>=0
  Alpha   -   coefficient
  A       -   matrix
  IA      -   submatrix offset
  JA      -   submatrix offset
  OpTypeA -   multiplication type:
  * 0 - A*A^H is calculated
  * 2 - A^H*A is calculated
  Beta    -   coefficient
  C       -   matrix
  IC      -   submatrix offset
  JC      -   submatrix offset
  IsUpper -   whether C is upper triangular or lower triangular
  ************************************************************************ *)
procedure CMatrixSYRK(n: TLInt; k: TLInt; alpha: TLFloat;
  const a: TLComplexMatrix; IA: TLInt; ja: TLInt;
  OpTypeA: TLInt; beta: TLFloat; var c: TLComplexMatrix;
  IC: TLInt; JC: TLInt; IsUpper: Boolean);
var
  s1: TLInt;
  s2: TLInt;
  bs: TLInt;
begin
  bs := ABLASComplexBlockSize(a);
  if (n <= bs) and (k <= bs) then
    begin
      CMatrixSYRK2(n, k, alpha, a, IA, ja, OpTypeA, beta, c, IC, JC, IsUpper);
      Exit;
    end;
  if k >= n then
    begin

      //
      // Split K
      //
      ABLASComplexSplitLength(a, k, s1, s2);
      if OpTypeA = 0 then
        begin
          CMatrixSYRK(n, s1, alpha, a, IA, ja, OpTypeA, beta, c, IC, JC, IsUpper);
          CMatrixSYRK(n, s2, alpha, a, IA, ja + s1, OpTypeA, 1.0, c, IC, JC, IsUpper);
        end
      else
        begin
          CMatrixSYRK(n, s1, alpha, a, IA, ja, OpTypeA, beta, c, IC, JC, IsUpper);
          CMatrixSYRK(n, s2, alpha, a, IA + s1, ja, OpTypeA, 1.0, c, IC, JC, IsUpper);
        end;
    end
  else
    begin

      //
      // Split N
      //
      ABLASComplexSplitLength(a, n, s1, s2);
      if (OpTypeA = 0) and IsUpper then
        begin
          CMatrixSYRK(s1, k, alpha, a, IA, ja, OpTypeA, beta, c, IC, JC, IsUpper);
          CMatrixGEMM(s1, s2, k, C_Complex(alpha), a, IA, ja, 0, a, IA + s1, ja, 2, C_Complex(beta), c, IC, JC + s1);
          CMatrixSYRK(s2, k, alpha, a, IA + s1, ja, OpTypeA, beta, c, IC + s1, JC + s1, IsUpper);
          Exit;
        end;
      if (OpTypeA = 0) and not IsUpper then
        begin
          CMatrixSYRK(s1, k, alpha, a, IA, ja, OpTypeA, beta, c, IC, JC, IsUpper);
          CMatrixGEMM(s2, s1, k, C_Complex(alpha), a, IA + s1, ja, 0, a, IA, ja, 2, C_Complex(beta), c, IC + s1, JC);
          CMatrixSYRK(s2, k, alpha, a, IA + s1, ja, OpTypeA, beta, c, IC + s1, JC + s1, IsUpper);
          Exit;
        end;
      if (OpTypeA <> 0) and IsUpper then
        begin
          CMatrixSYRK(s1, k, alpha, a, IA, ja, OpTypeA, beta, c, IC, JC, IsUpper);
          CMatrixGEMM(s1, s2, k, C_Complex(alpha), a, IA, ja, 2, a, IA, ja + s1, 0, C_Complex(beta), c, IC, JC + s1);
          CMatrixSYRK(s2, k, alpha, a, IA, ja + s1, OpTypeA, beta, c, IC + s1, JC + s1, IsUpper);
          Exit;
        end;
      if (OpTypeA <> 0) and not IsUpper then
        begin
          CMatrixSYRK(s1, k, alpha, a, IA, ja, OpTypeA, beta, c, IC, JC, IsUpper);
          CMatrixGEMM(s2, s1, k, C_Complex(alpha), a, IA, ja + s1, 2, a, IA, ja, 0, C_Complex(beta), c, IC + s1, JC);
          CMatrixSYRK(s2, k, alpha, a, IA, ja + s1, OpTypeA, beta, c, IC + s1, JC + s1, IsUpper);
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Same as CMatrixSYRK, but for real matrices

  OpType may be only 0 or 1.
  ************************************************************************ *)
procedure RMatrixSYRK(n: TLInt; k: TLInt; alpha: TLFloat;
  const a: TLMatrix; IA: TLInt; ja: TLInt;
  OpTypeA: TLInt; beta: TLFloat; var c: TLMatrix;
  IC: TLInt; JC: TLInt; IsUpper: Boolean);
var
  s1: TLInt;
  s2: TLInt;
  bs: TLInt;
begin
  bs := ABLASBlockSize(a);
  if (n <= bs) and (k <= bs) then
    begin
      RMatrixSYRK2(n, k, alpha, a, IA, ja, OpTypeA, beta, c, IC, JC, IsUpper);
      Exit;
    end;
  if k >= n then
    begin

      //
      // Split K
      //
      ABLASSplitLength(a, k, s1, s2);
      if OpTypeA = 0 then
        begin
          RMatrixSYRK(n, s1, alpha, a, IA, ja, OpTypeA, beta, c, IC, JC, IsUpper);
          RMatrixSYRK(n, s2, alpha, a, IA, ja + s1, OpTypeA, 1.0, c, IC, JC, IsUpper);
        end
      else
        begin
          RMatrixSYRK(n, s1, alpha, a, IA, ja, OpTypeA, beta, c, IC, JC, IsUpper);
          RMatrixSYRK(n, s2, alpha, a, IA + s1, ja, OpTypeA, 1.0, c, IC, JC, IsUpper);
        end;
    end
  else
    begin

      //
      // Split N
      //
      ABLASSplitLength(a, n, s1, s2);
      if (OpTypeA = 0) and IsUpper then
        begin
          RMatrixSYRK(s1, k, alpha, a, IA, ja, OpTypeA, beta, c, IC, JC, IsUpper);
          RMatrixGEMM(s1, s2, k, alpha, a, IA, ja, 0, a, IA + s1, ja, 1, beta, c, IC, JC + s1);
          RMatrixSYRK(s2, k, alpha, a, IA + s1, ja, OpTypeA, beta, c, IC + s1, JC + s1, IsUpper);
          Exit;
        end;
      if (OpTypeA = 0) and not IsUpper then
        begin
          RMatrixSYRK(s1, k, alpha, a, IA, ja, OpTypeA, beta, c, IC, JC, IsUpper);
          RMatrixGEMM(s2, s1, k, alpha, a, IA + s1, ja, 0, a, IA, ja, 1, beta, c, IC + s1, JC);
          RMatrixSYRK(s2, k, alpha, a, IA + s1, ja, OpTypeA, beta, c, IC + s1, JC + s1, IsUpper);
          Exit;
        end;
      if (OpTypeA <> 0) and IsUpper then
        begin
          RMatrixSYRK(s1, k, alpha, a, IA, ja, OpTypeA, beta, c, IC, JC, IsUpper);
          RMatrixGEMM(s1, s2, k, alpha, a, IA, ja, 1, a, IA, ja + s1, 0, beta, c, IC, JC + s1);
          RMatrixSYRK(s2, k, alpha, a, IA, ja + s1, OpTypeA, beta, c, IC + s1, JC + s1, IsUpper);
          Exit;
        end;
      if (OpTypeA <> 0) and not IsUpper then
        begin
          RMatrixSYRK(s1, k, alpha, a, IA, ja, OpTypeA, beta, c, IC, JC, IsUpper);
          RMatrixGEMM(s2, s1, k, alpha, a, IA, ja + s1, 1, a, IA, ja, 0, beta, c, IC + s1, JC);
          RMatrixSYRK(s2, k, alpha, a, IA, ja + s1, OpTypeA, beta, c, IC + s1, JC + s1, IsUpper);
          Exit;
        end;
    end;
end;

(* ************************************************************************
  This subroutine calculates C = alpha*op1(A)*op2(B) +beta*C where:
  * C is MxN general matrix
  * op1(A) is MxK matrix
  * op2(B) is KxN matrix
  * "op" may be identity transformation, transposition, conjugate transposition

  Additional info:
  * cache-oblivious algorithm is used.
  * multiplication result replaces C. If Beta=0, C elements are not used in
  calculations (not multiplied by zero - just not referenced)
  * if Alpha=0, A is not used (not multiplied by zero - just not referenced)
  * if both Beta and Alpha are zero, C is filled by zeros.

  INPUT PARAMETERS
  N       -   matrix size, N>0
  M       -   matrix size, N>0
  K       -   matrix size, K>0
  Alpha   -   coefficient
  A       -   matrix
  IA      -   submatrix offset
  JA      -   submatrix offset
  OpTypeA -   transformation type:
  * 0 - no transformation
  * 1 - transposition
  * 2 - conjugate transposition
  B       -   matrix
  IB      -   submatrix offset
  JB      -   submatrix offset
  OpTypeB -   transformation type:
  * 0 - no transformation
  * 1 - transposition
  * 2 - conjugate transposition
  Beta    -   coefficient
  C       -   matrix
  IC      -   submatrix offset
  JC      -   submatrix offset
  ************************************************************************ *)
procedure CMatrixGEMM(M: TLInt; n: TLInt; k: TLInt;
  alpha: TLComplex; const a: TLComplexMatrix; IA: TLInt;
  ja: TLInt; OpTypeA: TLInt; const b: TLComplexMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt; beta: TLComplex;
  var c: TLComplexMatrix; IC: TLInt; JC: TLInt);
var
  s1: TLInt;
  s2: TLInt;
  bs: TLInt;
begin
  bs := ABLASComplexBlockSize(a);
  if (M <= bs) and (n <= bs) and (k <= bs) then
    begin
      CMatrixGEMMK(M, n, k, alpha, a, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta, c, IC, JC);
      Exit;
    end;
  if (M >= n) and (M >= k) then
    begin

      //
      // A*B = (A1 A2)^T*B
      //
      ABLASComplexSplitLength(a, M, s1, s2);
      CMatrixGEMM(s1, n, k, alpha, a, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta, c, IC, JC);
      if OpTypeA = 0 then
        begin
          CMatrixGEMM(s2, n, k, alpha, a, IA + s1, ja, OpTypeA, b, IB, JB, OpTypeB, beta, c, IC + s1, JC);
        end
      else
        begin
          CMatrixGEMM(s2, n, k, alpha, a, IA, ja + s1, OpTypeA, b, IB, JB, OpTypeB, beta, c, IC + s1, JC);
        end;
      Exit;
    end;
  if (n >= M) and (n >= k) then
    begin

      //
      // A*B = A*(B1 B2)
      //
      ABLASComplexSplitLength(a, n, s1, s2);
      if OpTypeB = 0 then
        begin
          CMatrixGEMM(M, s1, k, alpha, a, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta, c, IC, JC);
          CMatrixGEMM(M, s2, k, alpha, a, IA, ja, OpTypeA, b, IB, JB + s1, OpTypeB, beta, c, IC, JC + s1);
        end
      else
        begin
          CMatrixGEMM(M, s1, k, alpha, a, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta, c, IC, JC);
          CMatrixGEMM(M, s2, k, alpha, a, IA, ja, OpTypeA, b, IB + s1, JB, OpTypeB, beta, c, IC, JC + s1);
        end;
      Exit;
    end;
  if (k >= M) and (k >= n) then
    begin

      //
      // A*B = (A1 A2)*(B1 B2)^T
      //
      ABLASComplexSplitLength(a, k, s1, s2);
      if (OpTypeA = 0) and (OpTypeB = 0) then
        begin
          CMatrixGEMM(M, n, s1, alpha, a, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta, c, IC, JC);
          CMatrixGEMM(M, n, s2, alpha, a, IA, ja + s1, OpTypeA, b, IB + s1, JB, OpTypeB, C_Complex(1.0), c, IC, JC);
        end;
      if (OpTypeA = 0) and (OpTypeB <> 0) then
        begin
          CMatrixGEMM(M, n, s1, alpha, a, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta, c, IC, JC);
          CMatrixGEMM(M, n, s2, alpha, a, IA, ja + s1, OpTypeA, b, IB, JB + s1, OpTypeB, C_Complex(1.0), c, IC, JC);
        end;
      if (OpTypeA <> 0) and (OpTypeB = 0) then
        begin
          CMatrixGEMM(M, n, s1, alpha, a, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta, c, IC, JC);
          CMatrixGEMM(M, n, s2, alpha, a, IA + s1, ja, OpTypeA, b, IB + s1, JB, OpTypeB, C_Complex(1.0), c, IC, JC);
        end;
      if (OpTypeA <> 0) and (OpTypeB <> 0) then
        begin
          CMatrixGEMM(M, n, s1, alpha, a, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta, c, IC, JC);
          CMatrixGEMM(M, n, s2, alpha, a, IA + s1, ja, OpTypeA, b, IB, JB + s1, OpTypeB, C_Complex(1.0), c, IC, JC);
        end;
      Exit;
    end;
end;

(* ************************************************************************
  Same as CMatrixGEMM, but for real numbers.
  OpType may be only 0 or 1.
  ************************************************************************ *)
procedure RMatrixGEMM(M: TLInt; n: TLInt; k: TLInt;
  alpha: TLFloat; const a: TLMatrix; IA: TLInt;
  ja: TLInt; OpTypeA: TLInt; const b: TLMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt;
  beta: TLFloat; var c: TLMatrix; IC: TLInt; JC: TLInt);
var
  s1: TLInt;
  s2: TLInt;
  bs: TLInt;
begin
  bs := ABLASBlockSize(a);
  if (M <= bs) and (n <= bs) and (k <= bs) then
    begin
      RMatrixGEMMK(M, n, k, alpha, a, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta, c, IC, JC);
      Exit;
    end;
  if (M >= n) and (M >= k) then
    begin

      //
      // A*B = (A1 A2)^T*B
      //
      ABLASSplitLength(a, M, s1, s2);
      if OpTypeA = 0 then
        begin
          RMatrixGEMM(s1, n, k, alpha, a, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta, c, IC, JC);
          RMatrixGEMM(s2, n, k, alpha, a, IA + s1, ja, OpTypeA, b, IB, JB, OpTypeB, beta, c, IC + s1, JC);
        end
      else
        begin
          RMatrixGEMM(s1, n, k, alpha, a, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta, c, IC, JC);
          RMatrixGEMM(s2, n, k, alpha, a, IA, ja + s1, OpTypeA, b, IB, JB, OpTypeB, beta, c, IC + s1, JC);
        end;
      Exit;
    end;
  if (n >= M) and (n >= k) then
    begin

      //
      // A*B = A*(B1 B2)
      //
      ABLASSplitLength(a, n, s1, s2);
      if OpTypeB = 0 then
        begin
          RMatrixGEMM(M, s1, k, alpha, a, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta, c, IC, JC);
          RMatrixGEMM(M, s2, k, alpha, a, IA, ja, OpTypeA, b, IB, JB + s1, OpTypeB, beta, c, IC, JC + s1);
        end
      else
        begin
          RMatrixGEMM(M, s1, k, alpha, a, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta, c, IC, JC);
          RMatrixGEMM(M, s2, k, alpha, a, IA, ja, OpTypeA, b, IB + s1, JB, OpTypeB, beta, c, IC, JC + s1);
        end;
      Exit;
    end;
  if (k >= M) and (k >= n) then
    begin

      //
      // A*B = (A1 A2)*(B1 B2)^T
      //
      ABLASSplitLength(a, k, s1, s2);
      if (OpTypeA = 0) and (OpTypeB = 0) then
        begin
          RMatrixGEMM(M, n, s1, alpha, a, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta, c, IC, JC);
          RMatrixGEMM(M, n, s2, alpha, a, IA, ja + s1, OpTypeA, b, IB + s1, JB, OpTypeB, 1.0, c, IC, JC);
        end;
      if (OpTypeA = 0) and (OpTypeB <> 0) then
        begin
          RMatrixGEMM(M, n, s1, alpha, a, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta, c, IC, JC);
          RMatrixGEMM(M, n, s2, alpha, a, IA, ja + s1, OpTypeA, b, IB, JB + s1, OpTypeB, 1.0, c, IC, JC);
        end;
      if (OpTypeA <> 0) and (OpTypeB = 0) then
        begin
          RMatrixGEMM(M, n, s1, alpha, a, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta, c, IC, JC);
          RMatrixGEMM(M, n, s2, alpha, a, IA + s1, ja, OpTypeA, b, IB + s1, JB, OpTypeB, 1.0, c, IC, JC);
        end;
      if (OpTypeA <> 0) and (OpTypeB <> 0) then
        begin
          RMatrixGEMM(M, n, s1, alpha, a, IA, ja, OpTypeA, b, IB, JB, OpTypeB, beta, c, IC, JC);
          RMatrixGEMM(M, n, s2, alpha, a, IA + s1, ja, OpTypeA, b, IB, JB + s1, OpTypeB, 1.0, c, IC, JC);
        end;
      Exit;
    end;
end;

(* ************************************************************************
  TLComplex ABLASSplitLength
  ************************************************************************ *)
procedure ABLASInternalSplitLength(n: TLInt; nb: TLInt;
  var n1: TLInt; var n2: TLInt);
var
  r: TLInt;
begin
  if n <= nb then
    begin

      //
      // Block size, no further splitting
      //
      n1 := n;
      n2 := 0;
    end
  else
    begin

      //
      // Greater than block size
      //
      if n mod nb <> 0 then
        begin

          //
          // Split remainder
          //
          n2 := n mod nb;
          n1 := n - n2;
        end
      else
        begin

          //
          // Split on block boundaries
          //
          n2 := n div 2;
          n1 := n - n2;
          if n1 mod nb = 0 then
              Exit;
          r := nb - n1 mod nb;
          n1 := n1 + r;
          n2 := n2 - r;
        end;
    end;
end;

(* ************************************************************************
  Level 2 variant of CMatrixRightTRSM
  ************************************************************************ *)
procedure CMatrixRightTRSM2(M: TLInt; n: TLInt;
  const a: TLComplexMatrix; i1: TLInt; j1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var x: TLComplexMatrix; i2: TLInt; j2: TLInt);
var
  i: TLInt;
  j: TLInt;
  VC: TLComplex;
  VD: TLComplex;
  i_: TLInt;
  i1_: TLInt;
begin

  //
  // Special case
  //
  if n * M = 0 then
      Exit;

  //
  // General case
  //
  if IsUpper then
    begin

      //
      // Upper triangular matrix
      //
      if OpType = 0 then
        begin

          //
          // X*A^(-1)
          //
          i := 0;
          while i <= M - 1 do
            begin
              j := 0;
              while j <= n - 1 do
                begin
                  if IsUnit then
                    begin
                      VD := C_Complex(1);
                    end
                  else
                    begin
                      VD := a[i1 + j, j1 + j];
                    end;
                  x[i2 + i, j2 + j] := C_Div(x[i2 + i, j2 + j], VD);
                  if j < n - 1 then
                    begin
                      VC := x[i2 + i, j2 + j];
                      i1_ := (j1 + j + 1) - (j2 + j + 1);
                      for i_ := j2 + j + 1 to j2 + n - 1 do
                        begin
                          x[i2 + i, i_] := C_Sub(x[i2 + i, i_], C_Mul(VC, a[i1 + j, i_ + i1_]));
                        end;
                    end;
                  inc(j);
                end;
              inc(i);
            end;
          Exit;
        end;
      if OpType = 1 then
        begin

          //
          // X*A^(-T)
          //
          i := 0;
          while i <= M - 1 do
            begin
              j := n - 1;
              while j >= 0 do
                begin
                  VC := C_Complex(0);
                  VD := C_Complex(1);
                  if j < n - 1 then
                    begin
                      i1_ := (j1 + j + 1) - (j2 + j + 1);
                      VC := C_Complex(0.0);
                      for i_ := j2 + j + 1 to j2 + n - 1 do
                        begin
                          VC := C_Add(VC, C_Mul(x[i2 + i, i_], a[i1 + j, i_ + i1_]));
                        end;
                    end;
                  if not IsUnit then
                    begin
                      VD := a[i1 + j, j1 + j];
                    end;
                  x[i2 + i, j2 + j] := C_Div(C_Sub(x[i2 + i, j2 + j], VC), VD);
                  dec(j);
                end;
              inc(i);
            end;
          Exit;
        end;
      if OpType = 2 then
        begin

          //
          // X*A^(-H)
          //
          i := 0;
          while i <= M - 1 do
            begin
              j := n - 1;
              while j >= 0 do
                begin
                  VC := C_Complex(0);
                  VD := C_Complex(1);
                  if j < n - 1 then
                    begin
                      i1_ := (j1 + j + 1) - (j2 + j + 1);
                      VC := C_Complex(0.0);
                      for i_ := j2 + j + 1 to j2 + n - 1 do
                        begin
                          VC := C_Add(VC, C_Mul(x[i2 + i, i_], Conj(a[i1 + j, i_ + i1_])));
                        end;
                    end;
                  if not IsUnit then
                    begin
                      VD := Conj(a[i1 + j, j1 + j]);
                    end;
                  x[i2 + i, j2 + j] := C_Div(C_Sub(x[i2 + i, j2 + j], VC), VD);
                  dec(j);
                end;
              inc(i);
            end;
          Exit;
        end;
    end
  else
    begin

      //
      // Lower triangular matrix
      //
      if OpType = 0 then
        begin

          //
          // X*A^(-1)
          //
          i := 0;
          while i <= M - 1 do
            begin
              j := n - 1;
              while j >= 0 do
                begin
                  if IsUnit then
                    begin
                      VD := C_Complex(1);
                    end
                  else
                    begin
                      VD := a[i1 + j, j1 + j];
                    end;
                  x[i2 + i, j2 + j] := C_Div(x[i2 + i, j2 + j], VD);
                  if j > 0 then
                    begin
                      VC := x[i2 + i, j2 + j];
                      i1_ := (j1) - (j2);
                      for i_ := j2 to j2 + j - 1 do
                        begin
                          x[i2 + i, i_] := C_Sub(x[i2 + i, i_], C_Mul(VC, a[i1 + j, i_ + i1_]));
                        end;
                    end;
                  dec(j);
                end;
              inc(i);
            end;
          Exit;
        end;
      if OpType = 1 then
        begin

          //
          // X*A^(-T)
          //
          i := 0;
          while i <= M - 1 do
            begin
              j := 0;
              while j <= n - 1 do
                begin
                  VC := C_Complex(0);
                  VD := C_Complex(1);
                  if j > 0 then
                    begin
                      i1_ := (j1) - (j2);
                      VC := C_Complex(0.0);
                      for i_ := j2 to j2 + j - 1 do
                        begin
                          VC := C_Add(VC, C_Mul(x[i2 + i, i_], a[i1 + j, i_ + i1_]));
                        end;
                    end;
                  if not IsUnit then
                    begin
                      VD := a[i1 + j, j1 + j];
                    end;
                  x[i2 + i, j2 + j] := C_Div(C_Sub(x[i2 + i, j2 + j], VC), VD);
                  inc(j);
                end;
              inc(i);
            end;
          Exit;
        end;
      if OpType = 2 then
        begin

          //
          // X*A^(-H)
          //
          i := 0;
          while i <= M - 1 do
            begin
              j := 0;
              while j <= n - 1 do
                begin
                  VC := C_Complex(0);
                  VD := C_Complex(1);
                  if j > 0 then
                    begin
                      i1_ := (j1) - (j2);
                      VC := C_Complex(0.0);
                      for i_ := j2 to j2 + j - 1 do
                        begin
                          VC := C_Add(VC, C_Mul(x[i2 + i, i_], Conj(a[i1 + j, i_ + i1_])));
                        end;
                    end;
                  if not IsUnit then
                    begin
                      VD := Conj(a[i1 + j, j1 + j]);
                    end;
                  x[i2 + i, j2 + j] := C_Div(C_Sub(x[i2 + i, j2 + j], VC), VD);
                  inc(j);
                end;
              inc(i);
            end;
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Level-2 subroutine
  ************************************************************************ *)
procedure CMatrixLeftTRSM2(M: TLInt; n: TLInt;
  const a: TLComplexMatrix; i1: TLInt; j1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var x: TLComplexMatrix; i2: TLInt; j2: TLInt);
var
  i: TLInt;
  j: TLInt;
  VC: TLComplex;
  VD: TLComplex;
  i_: TLInt;
begin

  //
  // Special case
  //
  if n * M = 0 then
      Exit;

  //
  // General case
  //
  if IsUpper then
    begin

      //
      // Upper triangular matrix
      //
      if OpType = 0 then
        begin

          //
          // A^(-1)*X
          //
          i := M - 1;
          while i >= 0 do
            begin
              j := i + 1;
              while j <= M - 1 do
                begin
                  VC := a[i1 + i, j1 + j];
                  for i_ := j2 to j2 + n - 1 do
                    begin
                      x[i2 + i, i_] := C_Sub(x[i2 + i, i_], C_Mul(VC, x[i2 + j, i_]));
                    end;
                  inc(j);
                end;
              if not IsUnit then
                begin
                  VD := C_RDiv(1, a[i1 + i, j1 + i]);
                  for i_ := j2 to j2 + n - 1 do
                    begin
                      x[i2 + i, i_] := C_Mul(VD, x[i2 + i, i_]);
                    end;
                end;
              dec(i);
            end;
          Exit;
        end;
      if OpType = 1 then
        begin

          //
          // A^(-T)*X
          //
          i := 0;
          while i <= M - 1 do
            begin
              if IsUnit then
                begin
                  VD := C_Complex(1);
                end
              else
                begin
                  VD := C_RDiv(1, a[i1 + i, j1 + i]);
                end;
              for i_ := j2 to j2 + n - 1 do
                begin
                  x[i2 + i, i_] := C_Mul(VD, x[i2 + i, i_]);
                end;
              j := i + 1;
              while j <= M - 1 do
                begin
                  VC := a[i1 + i, j1 + j];
                  for i_ := j2 to j2 + n - 1 do
                    begin
                      x[i2 + j, i_] := C_Sub(x[i2 + j, i_], C_Mul(VC, x[i2 + i, i_]));
                    end;
                  inc(j);
                end;
              inc(i);
            end;
          Exit;
        end;
      if OpType = 2 then
        begin

          //
          // A^(-H)*X
          //
          i := 0;
          while i <= M - 1 do
            begin
              if IsUnit then
                begin
                  VD := C_Complex(1);
                end
              else
                begin
                  VD := C_RDiv(1, Conj(a[i1 + i, j1 + i]));
                end;
              for i_ := j2 to j2 + n - 1 do
                begin
                  x[i2 + i, i_] := C_Mul(VD, x[i2 + i, i_]);
                end;
              j := i + 1;
              while j <= M - 1 do
                begin
                  VC := Conj(a[i1 + i, j1 + j]);
                  for i_ := j2 to j2 + n - 1 do
                    begin
                      x[i2 + j, i_] := C_Sub(x[i2 + j, i_], C_Mul(VC, x[i2 + i, i_]));
                    end;
                  inc(j);
                end;
              inc(i);
            end;
          Exit;
        end;
    end
  else
    begin

      //
      // Lower triangular matrix
      //
      if OpType = 0 then
        begin

          //
          // A^(-1)*X
          //
          i := 0;
          while i <= M - 1 do
            begin
              j := 0;
              while j <= i - 1 do
                begin
                  VC := a[i1 + i, j1 + j];
                  for i_ := j2 to j2 + n - 1 do
                    begin
                      x[i2 + i, i_] := C_Sub(x[i2 + i, i_], C_Mul(VC, x[i2 + j, i_]));
                    end;
                  inc(j);
                end;
              if IsUnit then
                begin
                  VD := C_Complex(1);
                end
              else
                begin
                  VD := C_RDiv(1, a[i1 + j, j1 + j]);
                end;
              for i_ := j2 to j2 + n - 1 do
                begin
                  x[i2 + i, i_] := C_Mul(VD, x[i2 + i, i_]);
                end;
              inc(i);
            end;
          Exit;
        end;
      if OpType = 1 then
        begin

          //
          // A^(-T)*X
          //
          i := M - 1;
          while i >= 0 do
            begin
              if IsUnit then
                begin
                  VD := C_Complex(1);
                end
              else
                begin
                  VD := C_RDiv(1, a[i1 + i, j1 + i]);
                end;
              for i_ := j2 to j2 + n - 1 do
                begin
                  x[i2 + i, i_] := C_Mul(VD, x[i2 + i, i_]);
                end;
              j := i - 1;
              while j >= 0 do
                begin
                  VC := a[i1 + i, j1 + j];
                  for i_ := j2 to j2 + n - 1 do
                    begin
                      x[i2 + j, i_] := C_Sub(x[i2 + j, i_], C_Mul(VC, x[i2 + i, i_]));
                    end;
                  dec(j);
                end;
              dec(i);
            end;
          Exit;
        end;
      if OpType = 2 then
        begin

          //
          // A^(-H)*X
          //
          i := M - 1;
          while i >= 0 do
            begin
              if IsUnit then
                begin
                  VD := C_Complex(1);
                end
              else
                begin
                  VD := C_RDiv(1, Conj(a[i1 + i, j1 + i]));
                end;
              for i_ := j2 to j2 + n - 1 do
                begin
                  x[i2 + i, i_] := C_Mul(VD, x[i2 + i, i_]);
                end;
              j := i - 1;
              while j >= 0 do
                begin
                  VC := Conj(a[i1 + i, j1 + j]);
                  for i_ := j2 to j2 + n - 1 do
                    begin
                      x[i2 + j, i_] := C_Sub(x[i2 + j, i_], C_Mul(VC, x[i2 + i, i_]));
                    end;
                  dec(j);
                end;
              dec(i);
            end;
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Level 2 subroutine
  ************************************************************************ *)
procedure RMatrixRightTRSM2(M: TLInt; n: TLInt;
  const a: TLMatrix; i1: TLInt; j1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var x: TLMatrix;
  i2: TLInt; j2: TLInt);
var
  i: TLInt;
  j: TLInt;
  vr: TLFloat;
  VD: TLFloat;
begin

  //
  // Special case
  //
  if n * M = 0 then
      Exit;

  //
  // General case
  //
  if IsUpper then
    begin

      //
      // Upper triangular matrix
      //
      if OpType = 0 then
        begin

          //
          // X*A^(-1)
          //
          i := 0;
          while i <= M - 1 do
            begin
              j := 0;
              while j <= n - 1 do
                begin
                  if IsUnit then
                    begin
                      VD := 1;
                    end
                  else
                    begin
                      VD := a[i1 + j, j1 + j];
                    end;
                  x[i2 + i, j2 + j] := x[i2 + i, j2 + j] / VD;
                  if j < n - 1 then
                    begin
                      vr := x[i2 + i, j2 + j];
                      APVSub(@x[i2 + i][0], j2 + j + 1, j2 + n - 1, @a[i1 + j][0], j1 + j + 1, j1 + n - 1, vr);
                    end;
                  inc(j);
                end;
              inc(i);
            end;
          Exit;
        end;
      if OpType = 1 then
        begin

          //
          // X*A^(-T)
          //
          i := 0;
          while i <= M - 1 do
            begin
              j := n - 1;
              while j >= 0 do
                begin
                  vr := 0;
                  VD := 1;
                  if j < n - 1 then
                    begin
                      vr := APVDotProduct(@x[i2 + i][0], j2 + j + 1, j2 + n - 1, @a[i1 + j][0], j1 + j + 1, j1 + n - 1);
                    end;
                  if not IsUnit then
                    begin
                      VD := a[i1 + j, j1 + j];
                    end;
                  x[i2 + i, j2 + j] := (x[i2 + i, j2 + j] - vr) / VD;
                  dec(j);
                end;
              inc(i);
            end;
          Exit;
        end;
    end
  else
    begin

      //
      // Lower triangular matrix
      //
      if OpType = 0 then
        begin

          //
          // X*A^(-1)
          //
          i := 0;
          while i <= M - 1 do
            begin
              j := n - 1;
              while j >= 0 do
                begin
                  if IsUnit then
                    begin
                      VD := 1;
                    end
                  else
                    begin
                      VD := a[i1 + j, j1 + j];
                    end;
                  x[i2 + i, j2 + j] := x[i2 + i, j2 + j] / VD;
                  if j > 0 then
                    begin
                      vr := x[i2 + i, j2 + j];
                      APVSub(@x[i2 + i][0], j2, j2 + j - 1, @a[i1 + j][0], j1, j1 + j - 1, vr);
                    end;
                  dec(j);
                end;
              inc(i);
            end;
          Exit;
        end;
      if OpType = 1 then
        begin

          //
          // X*A^(-T)
          //
          i := 0;
          while i <= M - 1 do
            begin
              j := 0;
              while j <= n - 1 do
                begin
                  vr := 0;
                  VD := 1;
                  if j > 0 then
                    begin
                      vr := APVDotProduct(@x[i2 + i][0], j2, j2 + j - 1, @a[i1 + j][0], j1, j1 + j - 1);
                    end;
                  if not IsUnit then
                    begin
                      VD := a[i1 + j, j1 + j];
                    end;
                  x[i2 + i, j2 + j] := (x[i2 + i, j2 + j] - vr) / VD;
                  inc(j);
                end;
              inc(i);
            end;
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Level 2 subroutine
  ************************************************************************ *)
procedure RMatrixLeftTRSM2(M: TLInt; n: TLInt;
  const a: TLMatrix; i1: TLInt; j1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var x: TLMatrix;
  i2: TLInt; j2: TLInt);
var
  i: TLInt;
  j: TLInt;
  vr: TLFloat;
  VD: TLFloat;
begin

  //
  // Special case
  //
  if n * M = 0 then
      Exit;

  //
  // General case
  //
  if IsUpper then
    begin

      //
      // Upper triangular matrix
      //
      if OpType = 0 then
        begin

          //
          // A^(-1)*X
          //
          i := M - 1;
          while i >= 0 do
            begin
              j := i + 1;
              while j <= M - 1 do
                begin
                  vr := a[i1 + i, j1 + j];
                  APVSub(@x[i2 + i][0], j2, j2 + n - 1, @x[i2 + j][0], j2, j2 + n - 1, vr);
                  inc(j);
                end;
              if not IsUnit then
                begin
                  VD := 1 / a[i1 + i, j1 + i];
                  APVMul(@x[i2 + i][0], j2, j2 + n - 1, VD);
                end;
              dec(i);
            end;
          Exit;
        end;
      if OpType = 1 then
        begin

          //
          // A^(-T)*X
          //
          i := 0;
          while i <= M - 1 do
            begin
              if IsUnit then
                begin
                  VD := 1;
                end
              else
                begin
                  VD := 1 / a[i1 + i, j1 + i];
                end;
              APVMul(@x[i2 + i][0], j2, j2 + n - 1, VD);
              j := i + 1;
              while j <= M - 1 do
                begin
                  vr := a[i1 + i, j1 + j];
                  APVSub(@x[i2 + j][0], j2, j2 + n - 1, @x[i2 + i][0], j2, j2 + n - 1, vr);
                  inc(j);
                end;
              inc(i);
            end;
          Exit;
        end;
    end
  else
    begin

      //
      // Lower triangular matrix
      //
      if OpType = 0 then
        begin

          //
          // A^(-1)*X
          //
          i := 0;
          while i <= M - 1 do
            begin
              j := 0;
              while j <= i - 1 do
                begin
                  vr := a[i1 + i, j1 + j];
                  APVSub(@x[i2 + i][0], j2, j2 + n - 1, @x[i2 + j][0], j2, j2 + n - 1, vr);
                  inc(j);
                end;
              if IsUnit then
                begin
                  VD := 1;
                end
              else
                begin
                  VD := 1 / a[i1 + j, j1 + j];
                end;
              APVMul(@x[i2 + i][0], j2, j2 + n - 1, VD);
              inc(i);
            end;
          Exit;
        end;
      if OpType = 1 then
        begin

          //
          // A^(-T)*X
          //
          i := M - 1;
          while i >= 0 do
            begin
              if IsUnit then
                begin
                  VD := 1;
                end
              else
                begin
                  VD := 1 / a[i1 + i, j1 + i];
                end;
              APVMul(@x[i2 + i][0], j2, j2 + n - 1, VD);
              j := i - 1;
              while j >= 0 do
                begin
                  vr := a[i1 + i, j1 + j];
                  APVSub(@x[i2 + j][0], j2, j2 + n - 1, @x[i2 + i][0], j2, j2 + n - 1, vr);
                  dec(j);
                end;
              dec(i);
            end;
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Level 2 subroutine
  ************************************************************************ *)
procedure CMatrixSYRK2(n: TLInt; k: TLInt; alpha: TLFloat;
  const a: TLComplexMatrix; IA: TLInt; ja: TLInt;
  OpTypeA: TLInt; beta: TLFloat; var c: TLComplexMatrix;
  IC: TLInt; JC: TLInt; IsUpper: Boolean);
var
  i: TLInt;
  j: TLInt;
  j1: TLInt;
  j2: TLInt;
  v: TLComplex;
  i_: TLInt;
  i1_: TLInt;
begin

  //
  // Fast exit (nothing to be done)
  //
  if (AP_FP_Eq(alpha, 0) or (k = 0)) and AP_FP_Eq(beta, 1) then
    begin
      Exit;
    end;

  //
  // SYRK
  //
  if OpTypeA = 0 then
    begin

      //
      // C=alpha*A*A^H+beta*C
      //
      i := 0;
      while i <= n - 1 do
        begin
          if IsUpper then
            begin
              j1 := i;
              j2 := n - 1;
            end
          else
            begin
              j1 := 0;
              j2 := i;
            end;
          j := j1;
          while j <= j2 do
            begin
              if AP_FP_NEq(alpha, 0) and (k > 0) then
                begin
                  v := C_Complex(0.0);
                  for i_ := ja to ja + k - 1 do
                    begin
                      v := C_Add(v, C_Mul(a[IA + i, i_], Conj(a[IA + j, i_])));
                    end;
                end
              else
                begin
                  v := C_Complex(0);
                end;
              if AP_FP_Eq(beta, 0) then
                begin
                  c[IC + i, JC + j] := C_MulR(v, alpha);
                end
              else
                begin
                  c[IC + i, JC + j] := C_Add(C_MulR(c[IC + i, JC + j], beta), C_MulR(v, alpha));
                end;
              inc(j);
            end;
          inc(i);
        end;
      Exit;
    end
  else
    begin

      //
      // C=alpha*A^H*A+beta*C
      //
      i := 0;
      while i <= n - 1 do
        begin
          if IsUpper then
            begin
              j1 := i;
              j2 := n - 1;
            end
          else
            begin
              j1 := 0;
              j2 := i;
            end;
          if AP_FP_Eq(beta, 0) then
            begin
              j := j1;
              while j <= j2 do
                begin
                  c[IC + i, JC + j] := C_Complex(0);
                  inc(j);
                end;
            end
          else
            begin
              for i_ := JC + j1 to JC + j2 do
                begin
                  c[IC + i, i_] := C_MulR(c[IC + i, i_], beta);
                end;
            end;
          inc(i);
        end;
      i := 0;
      while i <= k - 1 do
        begin
          j := 0;
          while j <= n - 1 do
            begin
              if IsUpper then
                begin
                  j1 := j;
                  j2 := n - 1;
                end
              else
                begin
                  j1 := 0;
                  j2 := j;
                end;
              v := C_MulR(Conj(a[IA + i, ja + j]), alpha);
              i1_ := (ja + j1) - (JC + j1);
              for i_ := JC + j1 to JC + j2 do
                begin
                  c[IC + j, i_] := C_Add(c[IC + j, i_], C_Mul(v, a[IA + i, i_ + i1_]));
                end;
              inc(j);
            end;
          inc(i);
        end;
      Exit;
    end;
end;

(* ************************************************************************
  Level 2 subrotuine
  ************************************************************************ *)
procedure RMatrixSYRK2(n: TLInt; k: TLInt; alpha: TLFloat;
  const a: TLMatrix; IA: TLInt; ja: TLInt;
  OpTypeA: TLInt; beta: TLFloat; var c: TLMatrix;
  IC: TLInt; JC: TLInt; IsUpper: Boolean);
var
  i: TLInt;
  j: TLInt;
  j1: TLInt;
  j2: TLInt;
  v: TLFloat;
begin

  //
  // Fast exit (nothing to be done)
  //
  if (AP_FP_Eq(alpha, 0) or (k = 0)) and AP_FP_Eq(beta, 1) then
    begin
      Exit;
    end;

  //
  // SYRK
  //
  if OpTypeA = 0 then
    begin

      //
      // C=alpha*A*A^H+beta*C
      //
      i := 0;
      while i <= n - 1 do
        begin
          if IsUpper then
            begin
              j1 := i;
              j2 := n - 1;
            end
          else
            begin
              j1 := 0;
              j2 := i;
            end;
          j := j1;
          while j <= j2 do
            begin
              if AP_FP_NEq(alpha, 0) and (k > 0) then
                begin
                  v := APVDotProduct(@a[IA + i][0], ja, ja + k - 1, @a[IA + j][0], ja, ja + k - 1);
                end
              else
                begin
                  v := 0;
                end;
              if AP_FP_Eq(beta, 0) then
                begin
                  c[IC + i, JC + j] := alpha * v;
                end
              else
                begin
                  c[IC + i, JC + j] := beta * c[IC + i, JC + j] + alpha * v;
                end;
              inc(j);
            end;
          inc(i);
        end;
      Exit;
    end
  else
    begin

      //
      // C=alpha*A^H*A+beta*C
      //
      i := 0;
      while i <= n - 1 do
        begin
          if IsUpper then
            begin
              j1 := i;
              j2 := n - 1;
            end
          else
            begin
              j1 := 0;
              j2 := i;
            end;
          if AP_FP_Eq(beta, 0) then
            begin
              j := j1;
              while j <= j2 do
                begin
                  c[IC + i, JC + j] := 0;
                  inc(j);
                end;
            end
          else
            begin
              APVMul(@c[IC + i][0], JC + j1, JC + j2, beta);
            end;
          inc(i);
        end;
      i := 0;
      while i <= k - 1 do
        begin
          j := 0;
          while j <= n - 1 do
            begin
              if IsUpper then
                begin
                  j1 := j;
                  j2 := n - 1;
                end
              else
                begin
                  j1 := 0;
                  j2 := j;
                end;
              v := alpha * a[IA + i, ja + j];
              APVAdd(@c[IC + j][0], JC + j1, JC + j2, @a[IA + i][0], ja + j1, ja + j2, v);
              inc(j);
            end;
          inc(i);
        end;
      Exit;
    end;
end;

(* ************************************************************************
  GEMM kernel
  ************************************************************************ *)
procedure CMatrixGEMMK(M: TLInt; n: TLInt; k: TLInt;
  alpha: TLComplex; const a: TLComplexMatrix; IA: TLInt;
  ja: TLInt; OpTypeA: TLInt; const b: TLComplexMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt; beta: TLComplex;
  var c: TLComplexMatrix; IC: TLInt; JC: TLInt);
var
  i: TLInt;
  j: TLInt;
  v: TLComplex;
  i_: TLInt;
  i1_: TLInt;
begin

  //
  // Special case
  //
  if M * n = 0 then
    begin
      Exit;
    end;

  //
  // Another special case
  //
  if k = 0 then
    begin
      if C_NotEqualR(beta, 0) then
        begin
          i := 0;
          while i <= M - 1 do
            begin
              j := 0;
              while j <= n - 1 do
                begin
                  c[IC + i, JC + j] := C_Mul(beta, c[IC + i, JC + j]);
                  inc(j);
                end;
              inc(i);
            end;
        end
      else
        begin
          i := 0;
          while i <= M - 1 do
            begin
              j := 0;
              while j <= n - 1 do
                begin
                  c[IC + i, JC + j] := C_Complex(0);
                  inc(j);
                end;
              inc(i);
            end;
        end;
      Exit;
    end;

  //
  // General case
  //
  if (OpTypeA = 0) and (OpTypeB <> 0) then
    begin

      //
      // A*B'
      //
      i := 0;
      while i <= M - 1 do
        begin
          j := 0;
          while j <= n - 1 do
            begin
              if (k = 0) or C_EqualR(alpha, 0) then
                begin
                  v := C_Complex(0);
                end
              else
                begin
                  if OpTypeB = 1 then
                    begin
                      i1_ := (JB) - (ja);
                      v := C_Complex(0.0);
                      for i_ := ja to ja + k - 1 do
                        begin
                          v := C_Add(v, C_Mul(a[IA + i, i_], b[IB + j, i_ + i1_]));
                        end;
                    end
                  else
                    begin
                      i1_ := (JB) - (ja);
                      v := C_Complex(0.0);
                      for i_ := ja to ja + k - 1 do
                        begin
                          v := C_Add(v, C_Mul(a[IA + i, i_], Conj(b[IB + j, i_ + i1_])));
                        end;
                    end;
                end;
              if C_EqualR(beta, 0) then
                begin
                  c[IC + i, JC + j] := C_Mul(alpha, v);
                end
              else
                begin
                  c[IC + i, JC + j] := C_Add(C_Mul(beta, c[IC + i, JC + j]), C_Mul(alpha, v));
                end;
              inc(j);
            end;
          inc(i);
        end;
      Exit;
    end;
  if (OpTypeA = 0) and (OpTypeB = 0) then
    begin

      //
      // A*B
      //
      i := 0;
      while i <= M - 1 do
        begin
          if C_NotEqualR(beta, 0) then
            begin
              for i_ := JC to JC + n - 1 do
                begin
                  c[IC + i, i_] := C_Mul(beta, c[IC + i, i_]);
                end;
            end
          else
            begin
              j := 0;
              while j <= n - 1 do
                begin
                  c[IC + i, JC + j] := C_Complex(0);
                  inc(j);
                end;
            end;
          if C_NotEqualR(alpha, 0) then
            begin
              j := 0;
              while j <= k - 1 do
                begin
                  v := C_Mul(alpha, a[IA + i, ja + j]);
                  i1_ := (JB) - (JC);
                  for i_ := JC to JC + n - 1 do
                    begin
                      c[IC + i, i_] := C_Add(c[IC + i, i_], C_Mul(v, b[IB + j, i_ + i1_]));
                    end;
                  inc(j);
                end;
            end;
          inc(i);
        end;
      Exit;
    end;
  if (OpTypeA <> 0) and (OpTypeB <> 0) then
    begin

      //
      // A'*B'
      //
      i := 0;
      while i <= M - 1 do
        begin
          j := 0;
          while j <= n - 1 do
            begin
              if C_EqualR(alpha, 0) then
                begin
                  v := C_Complex(0);
                end
              else
                begin
                  if OpTypeA = 1 then
                    begin
                      if OpTypeB = 1 then
                        begin
                          i1_ := (JB) - (IA);
                          v := C_Complex(0.0);
                          for i_ := IA to IA + k - 1 do
                            begin
                              v := C_Add(v, C_Mul(a[i_, ja + i], b[IB + j, i_ + i1_]));
                            end;
                        end
                      else
                        begin
                          i1_ := (JB) - (IA);
                          v := C_Complex(0.0);
                          for i_ := IA to IA + k - 1 do
                            begin
                              v := C_Add(v, C_Mul(a[i_, ja + i], Conj(b[IB + j, i_ + i1_])));
                            end;
                        end;
                    end
                  else
                    begin
                      if OpTypeB = 1 then
                        begin
                          i1_ := (JB) - (IA);
                          v := C_Complex(0.0);
                          for i_ := IA to IA + k - 1 do
                            begin
                              v := C_Add(v, C_Mul(Conj(a[i_, ja + i]), b[IB + j, i_ + i1_]));
                            end;
                        end
                      else
                        begin
                          i1_ := (JB) - (IA);
                          v := C_Complex(0.0);
                          for i_ := IA to IA + k - 1 do
                            begin
                              v := C_Add(v, C_Mul(Conj(a[i_, ja + i]), Conj(b[IB + j, i_ + i1_])));
                            end;
                        end;
                    end;
                end;
              if C_EqualR(beta, 0) then
                begin
                  c[IC + i, JC + j] := C_Mul(alpha, v);
                end
              else
                begin
                  c[IC + i, JC + j] := C_Add(C_Mul(beta, c[IC + i, JC + j]), C_Mul(alpha, v));
                end;
              inc(j);
            end;
          inc(i);
        end;
      Exit;
    end;
  if (OpTypeA <> 0) and (OpTypeB = 0) then
    begin

      //
      // A'*B
      //
      if C_EqualR(beta, 0) then
        begin
          i := 0;
          while i <= M - 1 do
            begin
              j := 0;
              while j <= n - 1 do
                begin
                  c[IC + i, JC + j] := C_Complex(0);
                  inc(j);
                end;
              inc(i);
            end;
        end
      else
        begin
          i := 0;
          while i <= M - 1 do
            begin
              for i_ := JC to JC + n - 1 do
                begin
                  c[IC + i, i_] := C_Mul(beta, c[IC + i, i_]);
                end;
              inc(i);
            end;
        end;
      if C_NotEqualR(alpha, 0) then
        begin
          j := 0;
          while j <= k - 1 do
            begin
              i := 0;
              while i <= M - 1 do
                begin
                  if OpTypeA = 1 then
                    begin
                      v := C_Mul(alpha, a[IA + j, ja + i]);
                    end
                  else
                    begin
                      v := C_Mul(alpha, Conj(a[IA + j, ja + i]));
                    end;
                  i1_ := (JB) - (JC);
                  for i_ := JC to JC + n - 1 do
                    begin
                      c[IC + i, i_] := C_Add(c[IC + i, i_], C_Mul(v, b[IB + j, i_ + i1_]));
                    end;
                  inc(i);
                end;
              inc(j);
            end;
        end;
      Exit;
    end;
end;

(* ************************************************************************
  GEMM kernel
  ************************************************************************ *)
procedure RMatrixGEMMK(M: TLInt; n: TLInt; k: TLInt;
  alpha: TLFloat; const a: TLMatrix; IA: TLInt;
  ja: TLInt; OpTypeA: TLInt; const b: TLMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt;
  beta: TLFloat; var c: TLMatrix; IC: TLInt; JC: TLInt);
var
  i: TLInt;
  j: TLInt;
  v: TLFloat;
  i_: TLInt;
  i1_: TLInt;
begin

  //
  // if matrix size is zero
  //
  if M * n = 0 then
    begin
      Exit;
    end;

  //
  // if K=0, then C=Beta*C
  //
  if k = 0 then
    begin
      if AP_FP_NEq(beta, 1) then
        begin
          if AP_FP_NEq(beta, 0) then
            begin
              i := 0;
              while i <= M - 1 do
                begin
                  j := 0;
                  while j <= n - 1 do
                    begin
                      c[IC + i, JC + j] := beta * c[IC + i, JC + j];
                      inc(j);
                    end;
                  inc(i);
                end;
            end
          else
            begin
              i := 0;
              while i <= M - 1 do
                begin
                  j := 0;
                  while j <= n - 1 do
                    begin
                      c[IC + i, JC + j] := 0;
                      inc(j);
                    end;
                  inc(i);
                end;
            end;
        end;
      Exit;
    end;

  //
  // General case
  //
  if (OpTypeA = 0) and (OpTypeB <> 0) then
    begin

      //
      // A*B'
      //
      i := 0;
      while i <= M - 1 do
        begin
          j := 0;
          while j <= n - 1 do
            begin
              if (k = 0) or AP_FP_Eq(alpha, 0) then
                begin
                  v := 0;
                end
              else
                begin
                  v := APVDotProduct(@a[IA + i][0], ja, ja + k - 1, @b[IB + j][0], JB, JB + k - 1);
                end;
              if AP_FP_Eq(beta, 0) then
                begin
                  c[IC + i, JC + j] := alpha * v;
                end
              else
                begin
                  c[IC + i, JC + j] := beta * c[IC + i, JC + j] + alpha * v;
                end;
              inc(j);
            end;
          inc(i);
        end;
      Exit;
    end;
  if (OpTypeA = 0) and (OpTypeB = 0) then
    begin

      //
      // A*B
      //
      i := 0;
      while i <= M - 1 do
        begin
          if AP_FP_NEq(beta, 0) then
            begin
              APVMul(@c[IC + i][0], JC, JC + n - 1, beta);
            end
          else
            begin
              j := 0;
              while j <= n - 1 do
                begin
                  c[IC + i, JC + j] := 0;
                  inc(j);
                end;
            end;
          if AP_FP_NEq(alpha, 0) then
            begin
              j := 0;
              while j <= k - 1 do
                begin
                  v := alpha * a[IA + i, ja + j];
                  APVAdd(@c[IC + i][0], JC, JC + n - 1, @b[IB + j][0], JB, JB + n - 1, v);
                  inc(j);
                end;
            end;
          inc(i);
        end;
      Exit;
    end;
  if (OpTypeA <> 0) and (OpTypeB <> 0) then
    begin

      //
      // A'*B'
      //
      i := 0;
      while i <= M - 1 do
        begin
          j := 0;
          while j <= n - 1 do
            begin
              if AP_FP_Eq(alpha, 0) then
                begin
                  v := 0;
                end
              else
                begin
                  i1_ := (JB) - (IA);
                  v := 0.0;
                  for i_ := IA to IA + k - 1 do
                    begin
                      v := v + a[i_, ja + i] * b[IB + j, i_ + i1_];
                    end;
                end;
              if AP_FP_Eq(beta, 0) then
                begin
                  c[IC + i, JC + j] := alpha * v;
                end
              else
                begin
                  c[IC + i, JC + j] := beta * c[IC + i, JC + j] + alpha * v;
                end;
              inc(j);
            end;
          inc(i);
        end;
      Exit;
    end;
  if (OpTypeA <> 0) and (OpTypeB = 0) then
    begin

      //
      // A'*B
      //
      if AP_FP_Eq(beta, 0) then
        begin
          i := 0;
          while i <= M - 1 do
            begin
              j := 0;
              while j <= n - 1 do
                begin
                  c[IC + i, JC + j] := 0;
                  inc(j);
                end;
              inc(i);
            end;
        end
      else
        begin
          i := 0;
          while i <= M - 1 do
            begin
              APVMul(@c[IC + i][0], JC, JC + n - 1, beta);
              inc(i);
            end;
        end;
      if AP_FP_NEq(alpha, 0) then
        begin
          j := 0;
          while j <= k - 1 do
            begin
              i := 0;
              while i <= M - 1 do
                begin
                  v := alpha * a[IA + j, ja + i];
                  APVAdd(@c[IC + i][0], JC, JC + n - 1, @b[IB + j][0], JB, JB + n - 1, v);
                  inc(i);
                end;
              inc(j);
            end;
        end;
      Exit;
    end;
end;
