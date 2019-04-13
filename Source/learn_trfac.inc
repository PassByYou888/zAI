{ ****************************************************************************** }
{ * LU and Cholesky decompositions support, by QQ 600585@qq.com                * }
{ ****************************************************************************** }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }

procedure CMatrixLUPRec(var a: TLComplexMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec; var tmp: TLComplexVec); forward;

procedure RMatrixLUPRec(var a: TLMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec; var tmp: TLVec); forward;

procedure CMatrixPLURec(var a: TLComplexMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec; var tmp: TLComplexVec); forward;

procedure RMatrixPLURec(var a: TLMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec; var tmp: TLVec); forward;

procedure CMatrixLUP2(var a: TLComplexMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec; var tmp: TLComplexVec); forward;

procedure RMatrixLUP2(var a: TLMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec; var tmp: TLVec); forward;

procedure CMatrixPLU2(var a: TLComplexMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec; var tmp: TLComplexVec); forward;

procedure RMatrixPLU2(var a: TLMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec; var tmp: TLVec); forward;

function HPDMatrixCholeskyRec(var a: TLComplexMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; var tmp: TLComplexVec): Boolean; forward;

function SPDMatrixCholeskyRec(var a: TLMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; var tmp: TLVec): Boolean; forward;

function HPDMatrixCholesky2(var aaa: TLComplexMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; var tmp: TLComplexVec): Boolean; forward;

function SPDMatrixCholesky2(var aaa: TLMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; var tmp: TLVec): Boolean; forward;


(* ************************************************************************
  LU decomposition of a general real matrix with row pivoting

  A is represented as A = P*L*U, where:
  * L is lower unitriangular matrix
  * U is upper triangular matrix
  * P = P0*P1*...*PK, K=min(M,N)-1,
  Pi - permutation matrix for I and Pivots[I]

  This is cache-oblivous implementation

  of LU decomposition.
  It is optimized for square matrices. As for rectangular matrices:
  * best case - M>>N
  * worst case - N>>M, small M, large N, matrix does not fit in CPU cache

  INPUT PARAMETERS:
  A       -   array[0..M-1, 0..N-1].
  M       -   number of rows in matrix A.
  N       -   number of columns in matrix A.


  OUTPUT PARAMETERS:
  A       -   matrices L and U in compact form:
  * L is stored under main diagonal
  * U is stored on and above main diagonal
  Pivots  -   permutation matrix in compact form. array[0..Min(M-1,N-1)].
  ************************************************************************ *)
procedure RMatrixLU(var a: TLMatrix; M: TLInt; n: TLInt;
  var Pivots: TLIVec);
begin
  Assert(M > 0, 'RMatrixLU: incorrect M!');
  Assert(n > 0, 'RMatrixLU: incorrect N!');
  RMatrixPLU(a, M, n, Pivots);
end;

(* ************************************************************************
  LU decomposition of a general TLComplex matrix with row pivoting

  A is represented as A = P*L*U, where:
  * L is lower unitriangular matrix
  * U is upper triangular matrix
  * P = P0*P1*...*PK, K=min(M,N)-1,
  Pi - permutation matrix for I and Pivots[I]

  This is cache-oblivous implementation

  of LU decomposition. It is optimized
  for square matrices. As for rectangular matrices:
  * best case - M>>N
  * worst case - N>>M, small M, large N, matrix does not fit in CPU cache

  INPUT PARAMETERS:
  A       -   array[0..M-1, 0..N-1].
  M       -   number of rows in matrix A.
  N       -   number of columns in matrix A.


  OUTPUT PARAMETERS:
  A       -   matrices L and U in compact form:
  * L is stored under main diagonal
  * U is stored on and above main diagonal
  Pivots  -   permutation matrix in compact form. array[0..Min(M-1,N-1)].
  ************************************************************************ *)
procedure CMatrixLU(var a: TLComplexMatrix; M: TLInt; n: TLInt;
  var Pivots: TLIVec);
begin
  Assert(M > 0, 'CMatrixLU: incorrect M!');
  Assert(n > 0, 'CMatrixLU: incorrect N!');
  CMatrixPLU(a, M, n, Pivots);
end;

(* ************************************************************************
  Cache-oblivious Cholesky decomposition

  The algorithm computes Cholesky decomposition  of  a  Hermitian  positive-
  definite matrix. The result of an algorithm is a representation  of  A  as
  A=U'*U  or A=L*L' (here X' detones conj(X^T)).

  INPUT PARAMETERS:
  A       -   upper or lower triangle of a factorized matrix.
  array with elements [0..N-1, 0..N-1].
  N       -   size of matrix A.
  IsUpper -   if IsUpper=True, then A contains an upper triangle of
  a symmetric matrix, otherwise A contains a lower one.

  OUTPUT PARAMETERS:
  A       -   the result of factorization. If IsUpper=True, then
  the upper triangle contains matrix U, so that A = U'*U,
  and the elements below the main diagonal are not modified.
  Similarly, if IsUpper = False.

  RESULT:
  If  the  matrix  is  positive-definite,  the  function  returns  True.
  Otherwise, the function returns False. Contents of A is not determined
  in such case.
  ************************************************************************ *)
function HPDMatrixCholesky(var a: TLComplexMatrix; n: TLInt;
  IsUpper: Boolean): Boolean;
var
  tmp: TLComplexVec;
begin
  if n < 1 then
    begin
      Result := False;
      Exit;
    end;
  SetLength(tmp, 2 * n);
  Result := HPDMatrixCholeskyRec(a, 0, n, IsUpper, tmp);
end;

(* ************************************************************************
  Cache-oblivious Cholesky decomposition

  The algorithm computes Cholesky decomposition  of  a  symmetric  positive-
  definite matrix. The result of an algorithm is a representation  of  A  as
  A=U^T*U  or A=L*L^T

  INPUT PARAMETERS:
  A       -   upper or lower triangle of a factorized matrix.
  array with elements [0..N-1, 0..N-1].
  N       -   size of matrix A.
  IsUpper -   if IsUpper=True, then A contains an upper triangle of
  a symmetric matrix, otherwise A contains a lower one.

  OUTPUT PARAMETERS:
  A       -   the result of factorization. If IsUpper=True, then
  the upper triangle contains matrix U, so that A = U^T*U,
  and the elements below the main diagonal are not modified.
  Similarly, if IsUpper = False.

  RESULT:
  If  the  matrix  is  positive-definite,  the  function  returns  True.
  Otherwise, the function returns False. Contents of A is not determined
  in such case.
  ************************************************************************ *)
function SPDMatrixCholesky(var a: TLMatrix; n: TLInt;
  IsUpper: Boolean): Boolean;
var
  tmp: TLVec;
begin
  if n < 1 then
    begin
      Result := False;
      Exit;
    end;
  SetLength(tmp, 2 * n);
  Result := SPDMatrixCholeskyRec(a, 0, n, IsUpper, tmp);
end;

procedure RMatrixLUP(var a: TLMatrix; M: TLInt; n: TLInt;
  var Pivots: TLIVec);
var
  tmp: TLVec;
  i: TLInt;
  j: TLInt;
  mx: TLFloat;
  v: TLFloat;
begin

  //
  // Internal LU decomposition subroutine.
  // Never call it directly.
  //
  Assert(M > 0, 'RMatrixLUP: incorrect M!');
  Assert(n > 0, 'RMatrixLUP: incorrect N!');

  //
  // Scale matrix to avoid overflows,
  // decompose it, then scale back.
  //
  mx := 0;
  i := 0;
  while i <= M - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          mx := Max(mx, AbsReal(a[i, j]));
          inc(j);
        end;
      inc(i);
    end;
  if AP_FP_NEq(mx, 0) then
    begin
      v := 1 / mx;
      i := 0;
      while i <= M - 1 do
        begin
          APVMul(@a[i][0], 0, n - 1, v);
          inc(i);
        end;
    end;
  SetLength(Pivots, Min(M, n));
  SetLength(tmp, 2 * Max(M, n));
  RMatrixLUPRec(a, 0, M, n, Pivots, tmp);
  if AP_FP_NEq(mx, 0) then
    begin
      v := mx;
      i := 0;
      while i <= M - 1 do
        begin
          APVMul(@a[i][0], 0, Min(i, n - 1), v);
          inc(i);
        end;
    end;
end;

procedure CMatrixLUP(var a: TLComplexMatrix; M: TLInt; n: TLInt;
  var Pivots: TLIVec);
var
  tmp: TLComplexVec;
  i: TLInt;
  j: TLInt;
  mx: TLFloat;
  v: TLFloat;
  i_: TLInt;
begin

  //
  // Internal LU decomposition subroutine.
  // Never call it directly.
  //
  Assert(M > 0, 'CMatrixLUP: incorrect M!');
  Assert(n > 0, 'CMatrixLUP: incorrect N!');

  //
  // Scale matrix to avoid overflows,
  // decompose it, then scale back.
  //
  mx := 0;
  i := 0;
  while i <= M - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          mx := Max(mx, AbsComplex(a[i, j]));
          inc(j);
        end;
      inc(i);
    end;
  if AP_FP_NEq(mx, 0) then
    begin
      v := 1 / mx;
      i := 0;
      while i <= M - 1 do
        begin
          for i_ := 0 to n - 1 do
            begin
              a[i, i_] := C_MulR(a[i, i_], v);
            end;
          inc(i);
        end;
    end;
  SetLength(Pivots, Min(M, n));
  SetLength(tmp, 2 * Max(M, n));
  CMatrixLUPRec(a, 0, M, n, Pivots, tmp);
  if AP_FP_NEq(mx, 0) then
    begin
      v := mx;
      i := 0;
      while i <= M - 1 do
        begin
          for i_ := 0 to Min(i, n - 1) do
            begin
              a[i, i_] := C_MulR(a[i, i_], v);
            end;
          inc(i);
        end;
    end;
end;

procedure RMatrixPLU(var a: TLMatrix; M: TLInt; n: TLInt;
  var Pivots: TLIVec);
var
  tmp: TLVec;
  i: TLInt;
  j: TLInt;
  mx: TLFloat;
  v: TLFloat;
begin

  //
  // Internal LU decomposition subroutine.
  // Never call it directly.
  //
  Assert(M > 0, 'RMatrixPLU: incorrect M!');
  Assert(n > 0, 'RMatrixPLU: incorrect N!');
  SetLength(tmp, 2 * Max(M, n));
  SetLength(Pivots, Min(M, n));

  //
  // Scale matrix to avoid overflows,
  // decompose it, then scale back.
  //
  mx := 0;
  i := 0;
  while i <= M - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          mx := Max(mx, AbsReal(a[i, j]));
          inc(j);
        end;
      inc(i);
    end;
  if AP_FP_NEq(mx, 0) then
    begin
      v := 1 / mx;
      i := 0;
      while i <= M - 1 do
        begin
          APVMul(@a[i][0], 0, n - 1, v);
          inc(i);
        end;
    end;
  RMatrixPLURec(a, 0, M, n, Pivots, tmp);
  if AP_FP_NEq(mx, 0) then
    begin
      v := mx;
      i := 0;
      while i <= Min(M, n) - 1 do
        begin
          APVMul(@a[i][0], i, n - 1, v);
          inc(i);
        end;
    end;
end;

procedure CMatrixPLU(var a: TLComplexMatrix; M: TLInt; n: TLInt;
  var Pivots: TLIVec);
var
  tmp: TLComplexVec;
  i: TLInt;
  j: TLInt;
  mx: TLFloat;
  v: TLComplex;
  i_: TLInt;
begin

  //
  // Internal LU decomposition subroutine.
  // Never call it directly.
  //
  Assert(M > 0, 'CMatrixPLU: incorrect M!');
  Assert(n > 0, 'CMatrixPLU: incorrect N!');
  SetLength(tmp, 2 * Max(M, n));
  SetLength(Pivots, Min(M, n));

  //
  // Scale matrix to avoid overflows,
  // decompose it, then scale back.
  //
  mx := 0;
  i := 0;
  while i <= M - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          mx := Max(mx, AbsComplex(a[i, j]));
          inc(j);
        end;
      inc(i);
    end;
  if AP_FP_NEq(mx, 0) then
    begin
      v := C_Complex(1 / mx);
      i := 0;
      while i <= M - 1 do
        begin
          for i_ := 0 to n - 1 do
            begin
              a[i, i_] := C_Mul(v, a[i, i_]);
            end;
          inc(i);
        end;
    end;
  CMatrixPLURec(a, 0, M, n, Pivots, tmp);
  if AP_FP_NEq(mx, 0) then
    begin
      v := C_Complex(mx);
      i := 0;
      while i <= Min(M, n) - 1 do
        begin
          for i_ := i to n - 1 do
            begin
              a[i, i_] := C_Mul(v, a[i, i_]);
            end;
          inc(i);
        end;
    end;
end;

(* ************************************************************************
  Recurrent TLComplex LU subroutine.
  Never call it directly.
  ************************************************************************ *)
procedure CMatrixLUPRec(var a: TLComplexMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec;
  var tmp: TLComplexVec);
var
  i: TLInt;
  m1: TLInt;
  m2: TLInt;
  i_: TLInt;
  i1_: TLInt;
begin

  //
  // Kernel case
  //
  if Min(M, n) <= ABLASComplexBlockSize(a) then
    begin
      CMatrixLUP2(a, Offs, M, n, Pivots, tmp);
      Exit;
    end;

  //
  // Preliminary step, make N>=M
  //
  // ( A1 )
  // A = (    ), where A1 is square
  // ( A2 )
  //
  // Factorize A1, update A2
  //
  if M > n then
    begin
      CMatrixLUPRec(a, Offs, n, n, Pivots, tmp);
      i := 0;
      while i <= n - 1 do
        begin
          i1_ := (Offs + n) - (0);
          for i_ := 0 to M - n - 1 do
            begin
              tmp[i_] := a[i_ + i1_, Offs + i];
            end;
          for i_ := Offs + n to Offs + M - 1 do
            begin
              a[i_, Offs + i] := a[i_, Pivots[Offs + i]];
            end;
          i1_ := (0) - (Offs + n);
          for i_ := Offs + n to Offs + M - 1 do
            begin
              a[i_, Pivots[Offs + i]] := tmp[i_ + i1_];
            end;
          inc(i);
        end;
      CMatrixRightTRSM(M - n, n, a, Offs, Offs, True, True, 0, a, Offs + n, Offs);
      Exit;
    end;

  //
  // Non-kernel case
  //
  ABLASComplexSplitLength(a, M, m1, m2);
  CMatrixLUPRec(a, Offs, m1, n, Pivots, tmp);
  if m2 > 0 then
    begin
      i := 0;
      while i <= m1 - 1 do
        begin
          if Offs + i <> Pivots[Offs + i] then
            begin
              i1_ := (Offs + m1) - (0);
              for i_ := 0 to m2 - 1 do
                begin
                  tmp[i_] := a[i_ + i1_, Offs + i];
                end;
              for i_ := Offs + m1 to Offs + M - 1 do
                begin
                  a[i_, Offs + i] := a[i_, Pivots[Offs + i]];
                end;
              i1_ := (0) - (Offs + m1);
              for i_ := Offs + m1 to Offs + M - 1 do
                begin
                  a[i_, Pivots[Offs + i]] := tmp[i_ + i1_];
                end;
            end;
          inc(i);
        end;
      CMatrixRightTRSM(m2, m1, a, Offs, Offs, True, True, 0, a, Offs + m1, Offs);
      CMatrixGEMM(M - m1, n - m1, m1, C_Complex(-1.0), a, Offs + m1, Offs, 0, a,
        Offs, Offs + m1, 0, C_Complex(+1.0), a, Offs + m1, Offs + m1);
      CMatrixLUPRec(a, Offs + m1, M - m1, n - m1, Pivots, tmp);
      i := 0;
      while i <= m2 - 1 do
        begin
          if Offs + m1 + i <> Pivots[Offs + m1 + i] then
            begin
              i1_ := (Offs) - (0);
              for i_ := 0 to m1 - 1 do
                begin
                  tmp[i_] := a[i_ + i1_, Offs + m1 + i];
                end;
              for i_ := Offs to Offs + m1 - 1 do
                begin
                  a[i_, Offs + m1 + i] := a[i_, Pivots[Offs + m1 + i]];
                end;
              i1_ := (0) - (Offs);
              for i_ := Offs to Offs + m1 - 1 do
                begin
                  a[i_, Pivots[Offs + m1 + i]] := tmp[i_ + i1_];
                end;
            end;
          inc(i);
        end;
    end;
end;

(* ************************************************************************
  Recurrent real LU subroutine.
  Never call it directly.
  ************************************************************************ *)
procedure RMatrixLUPRec(var a: TLMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec;
  var tmp: TLVec);
var
  i: TLInt;
  m1: TLInt;
  m2: TLInt;
  i_: TLInt;
  i1_: TLInt;
begin

  //
  // Kernel case
  //
  if Min(M, n) <= ABLASBlockSize(a) then
    begin
      RMatrixLUP2(a, Offs, M, n, Pivots, tmp);
      Exit;
    end;

  //
  // Preliminary step, make N>=M
  //
  // ( A1 )
  // A = (    ), where A1 is square
  // ( A2 )
  //
  // Factorize A1, update A2
  //
  if M > n then
    begin
      RMatrixLUPRec(a, Offs, n, n, Pivots, tmp);
      i := 0;
      while i <= n - 1 do
        begin
          if Offs + i <> Pivots[Offs + i] then
            begin
              i1_ := (Offs + n) - (0);
              for i_ := 0 to M - n - 1 do
                begin
                  tmp[i_] := a[i_ + i1_, Offs + i];
                end;
              for i_ := Offs + n to Offs + M - 1 do
                begin
                  a[i_, Offs + i] := a[i_, Pivots[Offs + i]];
                end;
              i1_ := (0) - (Offs + n);
              for i_ := Offs + n to Offs + M - 1 do
                begin
                  a[i_, Pivots[Offs + i]] := tmp[i_ + i1_];
                end;
            end;
          inc(i);
        end;
      RMatrixRightTRSM(M - n, n, a, Offs, Offs, True, True, 0, a, Offs + n, Offs);
      Exit;
    end;

  //
  // Non-kernel case
  //
  ABLASSplitLength(a, M, m1, m2);
  RMatrixLUPRec(a, Offs, m1, n, Pivots, tmp);
  if m2 > 0 then
    begin
      i := 0;
      while i <= m1 - 1 do
        begin
          if Offs + i <> Pivots[Offs + i] then
            begin
              i1_ := (Offs + m1) - (0);
              for i_ := 0 to m2 - 1 do
                begin
                  tmp[i_] := a[i_ + i1_, Offs + i];
                end;
              for i_ := Offs + m1 to Offs + M - 1 do
                begin
                  a[i_, Offs + i] := a[i_, Pivots[Offs + i]];
                end;
              i1_ := (0) - (Offs + m1);
              for i_ := Offs + m1 to Offs + M - 1 do
                begin
                  a[i_, Pivots[Offs + i]] := tmp[i_ + i1_];
                end;
            end;
          inc(i);
        end;
      RMatrixRightTRSM(m2, m1, a, Offs, Offs, True, True, 0, a, Offs + m1, Offs);
      RMatrixGEMM(M - m1, n - m1, m1, -1.0, a, Offs + m1, Offs, 0, a, Offs,
        Offs + m1, 0, +1.0, a, Offs + m1, Offs + m1);
      RMatrixLUPRec(a, Offs + m1, M - m1, n - m1, Pivots, tmp);
      i := 0;
      while i <= m2 - 1 do
        begin
          if Offs + m1 + i <> Pivots[Offs + m1 + i] then
            begin
              i1_ := (Offs) - (0);
              for i_ := 0 to m1 - 1 do
                begin
                  tmp[i_] := a[i_ + i1_, Offs + m1 + i];
                end;
              for i_ := Offs to Offs + m1 - 1 do
                begin
                  a[i_, Offs + m1 + i] := a[i_, Pivots[Offs + m1 + i]];
                end;
              i1_ := (0) - (Offs);
              for i_ := Offs to Offs + m1 - 1 do
                begin
                  a[i_, Pivots[Offs + m1 + i]] := tmp[i_ + i1_];
                end;
            end;
          inc(i);
        end;
    end;
end;

(* ************************************************************************
  Recurrent TLComplex LU subroutine.
  Never call it directly.
  ************************************************************************ *)
procedure CMatrixPLURec(var a: TLComplexMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec;
  var tmp: TLComplexVec);
var
  i: TLInt;
  n1: TLInt;
  n2: TLInt;
  i_: TLInt;
  i1_: TLInt;
begin

  //
  // Kernel case
  //
  if Min(M, n) <= ABLASComplexBlockSize(a) then
    begin
      CMatrixPLU2(a, Offs, M, n, Pivots, tmp);
      Exit;
    end;

  //
  // Preliminary step, make M>=N.
  //
  // A = (A1 A2), where A1 is square
  // Factorize A1, update A2
  //
  if n > M then
    begin
      CMatrixPLURec(a, Offs, M, M, Pivots, tmp);
      i := 0;
      while i <= M - 1 do
        begin
          i1_ := (Offs + M) - (0);
          for i_ := 0 to n - M - 1 do
            begin
              tmp[i_] := a[Offs + i, i_ + i1_];
            end;
          for i_ := Offs + M to Offs + n - 1 do
            begin
              a[Offs + i, i_] := a[Pivots[Offs + i], i_];
            end;
          i1_ := (0) - (Offs + M);
          for i_ := Offs + M to Offs + n - 1 do
            begin
              a[Pivots[Offs + i], i_] := tmp[i_ + i1_];
            end;
          inc(i);
        end;
      CMatrixLeftTRSM(M, n - M, a, Offs, Offs, False, True, 0, a, Offs, Offs + M);
      Exit;
    end;

  //
  // Non-kernel case
  //
  ABLASComplexSplitLength(a, n, n1, n2);
  CMatrixPLURec(a, Offs, M, n1, Pivots, tmp);
  if n2 > 0 then
    begin
      i := 0;
      while i <= n1 - 1 do
        begin
          if Offs + i <> Pivots[Offs + i] then
            begin
              i1_ := (Offs + n1) - (0);
              for i_ := 0 to n2 - 1 do
                begin
                  tmp[i_] := a[Offs + i, i_ + i1_];
                end;
              for i_ := Offs + n1 to Offs + n - 1 do
                begin
                  a[Offs + i, i_] := a[Pivots[Offs + i], i_];
                end;
              i1_ := (0) - (Offs + n1);
              for i_ := Offs + n1 to Offs + n - 1 do
                begin
                  a[Pivots[Offs + i], i_] := tmp[i_ + i1_];
                end;
            end;
          inc(i);
        end;
      CMatrixLeftTRSM(n1, n2, a, Offs, Offs, False, True, 0, a, Offs, Offs + n1);
      CMatrixGEMM(M - n1, n - n1, n1, C_Complex(-1.0), a, Offs + n1, Offs, 0, a,
        Offs, Offs + n1, 0, C_Complex(+1.0), a, Offs + n1, Offs + n1);
      CMatrixPLURec(a, Offs + n1, M - n1, n - n1, Pivots, tmp);
      i := 0;
      while i <= n2 - 1 do
        begin
          if Offs + n1 + i <> Pivots[Offs + n1 + i] then
            begin
              i1_ := (Offs) - (0);
              for i_ := 0 to n1 - 1 do
                begin
                  tmp[i_] := a[Offs + n1 + i, i_ + i1_];
                end;
              for i_ := Offs to Offs + n1 - 1 do
                begin
                  a[Offs + n1 + i, i_] := a[Pivots[Offs + n1 + i], i_];
                end;
              i1_ := (0) - (Offs);
              for i_ := Offs to Offs + n1 - 1 do
                begin
                  a[Pivots[Offs + n1 + i], i_] := tmp[i_ + i1_];
                end;
            end;
          inc(i);
        end;
    end;
end;

(* ************************************************************************
  Recurrent real LU subroutine.
  Never call it directly.
  ************************************************************************ *)
procedure RMatrixPLURec(var a: TLMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec;
  var tmp: TLVec);
var
  i: TLInt;
  n1: TLInt;
  n2: TLInt;
begin

  //
  // Kernel case
  //
  if Min(M, n) <= ABLASBlockSize(a) then
    begin
      RMatrixPLU2(a, Offs, M, n, Pivots, tmp);
      Exit;
    end;

  //
  // Preliminary step, make M>=N.
  //
  // A = (A1 A2), where A1 is square
  // Factorize A1, update A2
  //
  if n > M then
    begin
      RMatrixPLURec(a, Offs, M, M, Pivots, tmp);
      i := 0;
      while i <= M - 1 do
        begin
          APVMove(@tmp[0], 0, n - M - 1, @a[Offs + i][0], Offs + M, Offs + n - 1);
          APVMove(@a[Offs + i][0], Offs + M, Offs + n - 1, @a[Pivots[Offs + i]][0],
            Offs + M, Offs + n - 1);
          APVMove(@a[Pivots[Offs + i]][0], Offs + M, Offs + n - 1, @tmp[0], 0,
            n - M - 1);
          inc(i);
        end;
      RMatrixLeftTRSM(M, n - M, a, Offs, Offs, False, True, 0, a, Offs, Offs + M);
      Exit;
    end;

  //
  // Non-kernel case
  //
  ABLASSplitLength(a, n, n1, n2);
  RMatrixPLURec(a, Offs, M, n1, Pivots, tmp);
  if n2 > 0 then
    begin
      i := 0;
      while i <= n1 - 1 do
        begin
          if Offs + i <> Pivots[Offs + i] then
            begin
              APVMove(@tmp[0], 0, n2 - 1, @a[Offs + i][0], Offs + n1, Offs + n - 1);
              APVMove(@a[Offs + i][0], Offs + n1, Offs + n - 1,
                @a[Pivots[Offs + i]][0], Offs + n1, Offs + n - 1);
              APVMove(@a[Pivots[Offs + i]][0], Offs + n1, Offs + n - 1, @tmp[0],
                0, n2 - 1);
            end;
          inc(i);
        end;
      RMatrixLeftTRSM(n1, n2, a, Offs, Offs, False, True, 0, a, Offs, Offs + n1);
      RMatrixGEMM(M - n1, n - n1, n1, -1.0, a, Offs + n1, Offs, 0, a, Offs,
        Offs + n1, 0, +1.0, a, Offs + n1, Offs + n1);
      RMatrixPLURec(a, Offs + n1, M - n1, n - n1, Pivots, tmp);
      i := 0;
      while i <= n2 - 1 do
        begin
          if Offs + n1 + i <> Pivots[Offs + n1 + i] then
            begin
              APVMove(@tmp[0], 0, n1 - 1, @a[Offs + n1 + i][0], Offs, Offs + n1 - 1);
              APVMove(@a[Offs + n1 + i][0], Offs, Offs + n1 - 1,
                @a[Pivots[Offs + n1 + i]][0], Offs, Offs + n1 - 1);
              APVMove(@a[Pivots[Offs + n1 + i]][0], Offs, Offs + n1 - 1, @tmp[0],
                0, n1 - 1);
            end;
          inc(i);
        end;
    end;
end;

(* ************************************************************************
  TLComplex LUP kernel
  ************************************************************************ *)
procedure CMatrixLUP2(var a: TLComplexMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec;
  var tmp: TLComplexVec);
var
  i: TLInt;
  j: TLInt;
  JP: TLInt;
  s: TLComplex;
  i_: TLInt;
  i1_: TLInt;
begin

  //
  // Quick return if possible
  //
  if (M = 0) or (n = 0) then
    begin
      Exit;
    end;

  //
  // main cycle
  //
  j := 0;
  while j <= Min(M - 1, n - 1) do
    begin

      //
      // Find pivot, swap columns
      //
      JP := j;
      i := j + 1;
      while i <= n - 1 do
        begin
          if AP_FP_Greater(AbsComplex(a[Offs + j, Offs + i]),
            AbsComplex(a[Offs + j, Offs + JP])) then
            begin
              JP := i;
            end;
          inc(i);
        end;
      Pivots[Offs + j] := Offs + JP;
      if JP <> j then
        begin
          i1_ := (Offs) - (0);
          for i_ := 0 to M - 1 do
            begin
              tmp[i_] := a[i_ + i1_, Offs + j];
            end;
          for i_ := Offs to Offs + M - 1 do
            begin
              a[i_, Offs + j] := a[i_, Offs + JP];
            end;
          i1_ := (0) - (Offs);
          for i_ := Offs to Offs + M - 1 do
            begin
              a[i_, Offs + JP] := tmp[i_ + i1_];
            end;
        end;

      //
      // LU decomposition of 1x(N-J) matrix
      //
      if C_NotEqualR(a[Offs + j, Offs + j], 0) and (j + 1 <= n - 1) then
        begin
          s := C_RDiv(1, a[Offs + j, Offs + j]);
          for i_ := Offs + j + 1 to Offs + n - 1 do
            begin
              a[Offs + j, i_] := C_Mul(s, a[Offs + j, i_]);
            end;
        end;

      //
      // Update trailing (M-J-1)x(N-J-1) matrix
      //
      if j < Min(M - 1, n - 1) then
        begin
          i1_ := (Offs + j + 1) - (0);
          for i_ := 0 to M - j - 2 do
            begin
              tmp[i_] := a[i_ + i1_, Offs + j];
            end;
          i1_ := (Offs + j + 1) - (M);
          for i_ := M to M + n - j - 2 do
            begin
              tmp[i_] := C_Opposite(a[Offs + j, i_ + i1_]);
            end;
          CMatrixRank1(M - j - 1, n - j - 1, a, Offs + j + 1, Offs + j + 1, tmp,
            0, tmp, M);
        end;
      inc(j);
    end;
end;

(* ************************************************************************
  Real LUP kernel
  ************************************************************************ *)
procedure RMatrixLUP2(var a: TLMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec;
  var tmp: TLVec);
var
  i: TLInt;
  j: TLInt;
  JP: TLInt;
  s: TLFloat;
  i_: TLInt;
  i1_: TLInt;
begin

  //
  // Quick return if possible
  //
  if (M = 0) or (n = 0) then
    begin
      Exit;
    end;

  //
  // main cycle
  //
  j := 0;
  while j <= Min(M - 1, n - 1) do
    begin

      //
      // Find pivot, swap columns
      //
      JP := j;
      i := j + 1;
      while i <= n - 1 do
        begin
          if AP_FP_Greater(AbsReal(a[Offs + j, Offs + i]),
            AbsReal(a[Offs + j, Offs + JP])) then
            begin
              JP := i;
            end;
          inc(i);
        end;
      Pivots[Offs + j] := Offs + JP;
      if JP <> j then
        begin
          i1_ := (Offs) - (0);
          for i_ := 0 to M - 1 do
            begin
              tmp[i_] := a[i_ + i1_, Offs + j];
            end;
          for i_ := Offs to Offs + M - 1 do
            begin
              a[i_, Offs + j] := a[i_, Offs + JP];
            end;
          i1_ := (0) - (Offs);
          for i_ := Offs to Offs + M - 1 do
            begin
              a[i_, Offs + JP] := tmp[i_ + i1_];
            end;
        end;

      //
      // LU decomposition of 1x(N-J) matrix
      //
      if AP_FP_NEq(a[Offs + j, Offs + j], 0) and (j + 1 <= n - 1) then
        begin
          s := 1 / a[Offs + j, Offs + j];
          APVMul(@a[Offs + j][0], Offs + j + 1, Offs + n - 1, s);
        end;

      //
      // Update trailing (M-J-1)x(N-J-1) matrix
      //
      if j < Min(M - 1, n - 1) then
        begin
          i1_ := (Offs + j + 1) - (0);
          for i_ := 0 to M - j - 2 do
            begin
              tmp[i_] := a[i_ + i1_, Offs + j];
            end;
          APVMoveNeg(@tmp[0], M, M + n - j - 2, @a[Offs + j][0], Offs + j + 1,
            Offs + n - 1);
          RMatrixRank1(M - j - 1, n - j - 1, a, Offs + j + 1, Offs + j + 1, tmp,
            0, tmp, M);
        end;
      inc(j);
    end;
end;

(* ************************************************************************
  TLComplex PLU kernel
  ************************************************************************ *)
procedure CMatrixPLU2(var a: TLComplexMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec;
  var tmp: TLComplexVec);
var
  i: TLInt;
  j: TLInt;
  JP: TLInt;
  s: TLComplex;
  i_: TLInt;
  i1_: TLInt;
begin

  //
  // Quick return if possible
  //
  if (M = 0) or (n = 0) then
    begin
      Exit;
    end;
  j := 0;
  while j <= Min(M - 1, n - 1) do
    begin

      //
      // Find pivot and test for singularity.
      //
      JP := j;
      i := j + 1;
      while i <= M - 1 do
        begin
          if AP_FP_Greater(AbsComplex(a[Offs + i, Offs + j]),
            AbsComplex(a[Offs + JP, Offs + j])) then
            begin
              JP := i;
            end;
          inc(i);
        end;
      Pivots[Offs + j] := Offs + JP;
      if C_NotEqualR(a[Offs + JP, Offs + j], 0) then
        begin

          //
          // Apply the interchange to rows
          //
          if JP <> j then
            begin
              i := 0;
              while i <= n - 1 do
                begin
                  s := a[Offs + j, Offs + i];
                  a[Offs + j, Offs + i] := a[Offs + JP, Offs + i];
                  a[Offs + JP, Offs + i] := s;
                  inc(i);
                end;
            end;

          //
          // Compute elements J+1:M of J-th column.
          //
          if j + 1 <= M - 1 then
            begin
              s := C_RDiv(1, a[Offs + j, Offs + j]);
              for i_ := Offs + j + 1 to Offs + M - 1 do
                begin
                  a[i_, Offs + j] := C_Mul(s, a[i_, Offs + j]);
                end;
            end;
        end;
      if j < Min(M, n) - 1 then
        begin

          //
          // Update trailing submatrix.
          //
          i1_ := (Offs + j + 1) - (0);
          for i_ := 0 to M - j - 2 do
            begin
              tmp[i_] := a[i_ + i1_, Offs + j];
            end;
          i1_ := (Offs + j + 1) - (M);
          for i_ := M to M + n - j - 2 do
            begin
              tmp[i_] := C_Opposite(a[Offs + j, i_ + i1_]);
            end;
          CMatrixRank1(M - j - 1, n - j - 1, a, Offs + j + 1, Offs + j + 1, tmp,
            0, tmp, M);
        end;
      inc(j);
    end;
end;

(* ************************************************************************
  Real PLU kernel
  ************************************************************************ *)
procedure RMatrixPLU2(var a: TLMatrix; Offs: TLInt;
  M: TLInt; n: TLInt; var Pivots: TLIVec;
  var tmp: TLVec);
var
  i: TLInt;
  j: TLInt;
  JP: TLInt;
  s: TLFloat;
  i_: TLInt;
  i1_: TLInt;
begin

  //
  // Quick return if possible
  //
  if (M = 0) or (n = 0) then
    begin
      Exit;
    end;
  j := 0;
  while j <= Min(M - 1, n - 1) do
    begin

      //
      // Find pivot and test for singularity.
      //
      JP := j;
      i := j + 1;
      while i <= M - 1 do
        begin
          if AP_FP_Greater(AbsReal(a[Offs + i, Offs + j]),
            AbsReal(a[Offs + JP, Offs + j])) then
            begin
              JP := i;
            end;
          inc(i);
        end;
      Pivots[Offs + j] := Offs + JP;
      if AP_FP_NEq(a[Offs + JP, Offs + j], 0) then
        begin

          //
          // Apply the interchange to rows
          //
          if JP <> j then
            begin
              i := 0;
              while i <= n - 1 do
                begin
                  s := a[Offs + j, Offs + i];
                  a[Offs + j, Offs + i] := a[Offs + JP, Offs + i];
                  a[Offs + JP, Offs + i] := s;
                  inc(i);
                end;
            end;

          //
          // Compute elements J+1:M of J-th column.
          //
          if j + 1 <= M - 1 then
            begin
              s := 1 / a[Offs + j, Offs + j];
              for i_ := Offs + j + 1 to Offs + M - 1 do
                begin
                  a[i_, Offs + j] := s * a[i_, Offs + j];
                end;
            end;
        end;
      if j < Min(M, n) - 1 then
        begin

          //
          // Update trailing submatrix.
          //
          i1_ := (Offs + j + 1) - (0);
          for i_ := 0 to M - j - 2 do
            begin
              tmp[i_] := a[i_ + i1_, Offs + j];
            end;
          APVMoveNeg(@tmp[0], M, M + n - j - 2, @a[Offs + j][0], Offs + j + 1,
            Offs + n - 1);
          RMatrixRank1(M - j - 1, n - j - 1, a, Offs + j + 1, Offs + j + 1, tmp,
            0, tmp, M);
        end;
      inc(j);
    end;
end;

(* ************************************************************************
  Recursive computational subroutine for HPDMatrixCholesky
  ************************************************************************ *)
function HPDMatrixCholeskyRec(var a: TLComplexMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; var tmp: TLComplexVec): Boolean;
var
  n1: TLInt;
  n2: TLInt;
begin

  //
  // check N
  //
  if n < 1 then
    begin
      Result := False;
      Exit;
    end;

  //
  // special cases
  //
  if n = 1 then
    begin
      if AP_FP_Greater(a[Offs, Offs].x, 0) then
        begin
          a[Offs, Offs] := C_Complex(Sqrt(a[Offs, Offs].x));
          Result := True;
        end
      else
        begin
          Result := False;
        end;
      Exit;
    end;
  if n <= ABLASComplexBlockSize(a) then
    begin
      Result := HPDMatrixCholesky2(a, Offs, n, IsUpper, tmp);
      Exit;
    end;

  //
  // general case: split task in cache-oblivious manner
  //
  Result := True;
  ABLASComplexSplitLength(a, n, n1, n2);
  Result := HPDMatrixCholeskyRec(a, Offs, n1, IsUpper, tmp);
  if not Result then
    begin
      Exit;
    end;
  if n2 > 0 then
    begin
      if IsUpper then
        begin
          CMatrixLeftTRSM(n1, n2, a, Offs, Offs, IsUpper, False, 2, a, Offs,
            Offs + n1);
          CMatrixSYRK(n2, n1, -1.0, a, Offs, Offs + n1, 2, +1.0, a, Offs + n1,
            Offs + n1, IsUpper);
        end
      else
        begin
          CMatrixRightTRSM(n2, n1, a, Offs, Offs, IsUpper, False, 2, a,
            Offs + n1, Offs);
          CMatrixSYRK(n2, n1, -1.0, a, Offs + n1, Offs, 0, +1.0, a, Offs + n1,
            Offs + n1, IsUpper);
        end;
      Result := HPDMatrixCholeskyRec(a, Offs + n1, n2, IsUpper, tmp);
      if not Result then
        begin
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Recursive computational subroutine for SPDMatrixCholesky
  ************************************************************************ *)
function SPDMatrixCholeskyRec(var a: TLMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; var tmp: TLVec): Boolean;
var
  n1: TLInt;
  n2: TLInt;
begin

  //
  // check N
  //
  if n < 1 then
    begin
      Result := False;
      Exit;
    end;

  //
  // special cases
  //
  if n = 1 then
    begin
      if AP_FP_Greater(a[Offs, Offs], 0) then
        begin
          a[Offs, Offs] := Sqrt(a[Offs, Offs]);
          Result := True;
        end
      else
        begin
          Result := False;
        end;
      Exit;
    end;
  if n <= ABLASBlockSize(a) then
    begin
      Result := SPDMatrixCholesky2(a, Offs, n, IsUpper, tmp);
      Exit;
    end;

  //
  // general case: split task in cache-oblivious manner
  //
  Result := True;
  ABLASSplitLength(a, n, n1, n2);
  Result := SPDMatrixCholeskyRec(a, Offs, n1, IsUpper, tmp);
  if not Result then
    begin
      Exit;
    end;
  if n2 > 0 then
    begin
      if IsUpper then
        begin
          RMatrixLeftTRSM(n1, n2, a, Offs, Offs, IsUpper, False, 1, a, Offs,
            Offs + n1);
          RMatrixSYRK(n2, n1, -1.0, a, Offs, Offs + n1, 1, +1.0, a, Offs + n1,
            Offs + n1, IsUpper);
        end
      else
        begin
          RMatrixRightTRSM(n2, n1, a, Offs, Offs, IsUpper, False, 1, a,
            Offs + n1, Offs);
          RMatrixSYRK(n2, n1, -1.0, a, Offs + n1, Offs, 0, +1.0, a, Offs + n1,
            Offs + n1, IsUpper);
        end;
      Result := SPDMatrixCholeskyRec(a, Offs + n1, n2, IsUpper, tmp);
      if not Result then
        begin
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Level-2 Hermitian Cholesky subroutine.
  ************************************************************************ *)
function HPDMatrixCholesky2(var aaa: TLComplexMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; var tmp: TLComplexVec): Boolean;
var
  i: TLInt;
  j: TLInt;
  k: TLInt;
  j1: TLInt;
  j2: TLInt;
  AJJ: TLFloat;
  v: TLComplex;
  r: TLFloat;
  i_: TLInt;
  i1_: TLInt;
begin
  Result := True;
  if n < 0 then
    begin
      Result := False;
      Exit;
    end;

  //
  // Quick return if possible
  //
  if n = 0 then
    begin
      Exit;
    end;
  if IsUpper then
    begin

      //
      // Compute the Cholesky factorization A = U'*U.
      //
      j := 0;
      while j <= n - 1 do
        begin

          //
          // Compute U(J,J) and test for non-positive-definiteness.
          //
          v := C_Complex(0.0);
          for i_ := Offs to Offs + j - 1 do
            begin
              v := C_Add(v, C_Mul(Conj(aaa[i_, Offs + j]), aaa[i_, Offs + j]));
            end;
          AJJ := C_Sub(aaa[Offs + j, Offs + j], v).x;
          if AP_FP_Less_Eq(AJJ, 0) then
            begin
              aaa[Offs + j, Offs + j] := C_Complex(AJJ);
              Result := False;
              Exit;
            end;
          AJJ := Sqrt(AJJ);
          aaa[Offs + j, Offs + j] := C_Complex(AJJ);

          //
          // Compute elements J+1:N-1 of row J.
          //
          if j < n - 1 then
            begin
              if j > 0 then
                begin
                  i1_ := (Offs) - (0);
                  for i_ := 0 to j - 1 do
                    begin
                      tmp[i_] := C_Opposite(Conj(aaa[i_ + i1_, Offs + j]));
                    end;
                  CMatrixMV(n - j - 1, j, aaa, Offs, Offs + j + 1, 1, tmp, 0, tmp, n);
                  i1_ := (n) - (Offs + j + 1);
                  for i_ := Offs + j + 1 to Offs + n - 1 do
                    begin
                      aaa[Offs + j, i_] := C_Add(aaa[Offs + j, i_], tmp[i_ + i1_]);
                    end;
                end;
              r := 1 / AJJ;
              for i_ := Offs + j + 1 to Offs + n - 1 do
                begin
                  aaa[Offs + j, i_] := C_MulR(aaa[Offs + j, i_], r);
                end;
            end;
          inc(j);
        end;
    end
  else
    begin

      //
      // Compute the Cholesky factorization A = L*L'.
      //
      j := 0;
      while j <= n - 1 do
        begin

          //
          // Compute L(J+1,J+1) and test for non-positive-definiteness.
          //
          v := C_Complex(0.0);
          for i_ := Offs to Offs + j - 1 do
            begin
              v := C_Add(v, C_Mul(Conj(aaa[Offs + j, i_]), aaa[Offs + j, i_]));
            end;
          AJJ := C_Sub(aaa[Offs + j, Offs + j], v).x;
          if AP_FP_Less_Eq(AJJ, 0) then
            begin
              aaa[Offs + j, Offs + j] := C_Complex(AJJ);
              Result := False;
              Exit;
            end;
          AJJ := Sqrt(AJJ);
          aaa[Offs + j, Offs + j] := C_Complex(AJJ);

          //
          // Compute elements J+1:N of column J.
          //
          if j < n - 1 then
            begin
              if j > 0 then
                begin
                  i1_ := (Offs) - (0);
                  for i_ := 0 to j - 1 do
                    begin
                      tmp[i_] := Conj(aaa[Offs + j, i_ + i1_]);
                    end;
                  CMatrixMV(n - j - 1, j, aaa, Offs + j + 1, Offs, 0, tmp, 0, tmp, n);
                  i := 0;
                  while i <= n - j - 2 do
                    begin
                      aaa[Offs + j + 1 + i, Offs + j] :=
                        C_DivR(C_Sub(aaa[Offs + j + 1 + i, Offs + j], tmp[n + i]), AJJ);
                      inc(i);
                    end;
                end
              else
                begin
                  i := 0;
                  while i <= n - j - 2 do
                    begin
                      aaa[Offs + j + 1 + i, Offs + j] :=
                        C_DivR(aaa[Offs + j + 1 + i, Offs + j], AJJ);
                      inc(i);
                    end;
                end;
            end;
          inc(j);
        end;
    end;
end;

(* ************************************************************************
  Level-2 Cholesky subroutine
  ************************************************************************ *)
function SPDMatrixCholesky2(var aaa: TLMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; var tmp: TLVec): Boolean;
var
  i: TLInt;
  j: TLInt;
  k: TLInt;
  j1: TLInt;
  j2: TLInt;
  AJJ: TLFloat;
  v: TLFloat;
  r: TLFloat;
  i_: TLInt;
  i1_: TLInt;
begin
  Result := True;
  if n < 0 then
    begin
      Result := False;
      Exit;
    end;

  //
  // Quick return if possible
  //
  if n = 0 then
    begin
      Exit;
    end;
  if IsUpper then
    begin

      //
      // Compute the Cholesky factorization A = U'*U.
      //
      j := 0;
      while j <= n - 1 do
        begin

          //
          // Compute U(J,J) and test for non-positive-definiteness.
          //
          v := 0.0;
          for i_ := Offs to Offs + j - 1 do
            begin
              v := v + aaa[i_, Offs + j] * aaa[i_, Offs + j];
            end;
          AJJ := aaa[Offs + j, Offs + j] - v;
          if AP_FP_Less_Eq(AJJ, 0) then
            begin
              aaa[Offs + j, Offs + j] := AJJ;
              Result := False;
              Exit;
            end;
          AJJ := Sqrt(AJJ);
          aaa[Offs + j, Offs + j] := AJJ;

          //
          // Compute elements J+1:N-1 of row J.
          //
          if j < n - 1 then
            begin
              if j > 0 then
                begin
                  i1_ := (Offs) - (0);
                  for i_ := 0 to j - 1 do
                    begin
                      tmp[i_] := -aaa[i_ + i1_, Offs + j];
                    end;
                  RMatrixMV(n - j - 1, j, aaa, Offs, Offs + j + 1, 1, tmp, 0, tmp, n);
                  APVAdd(@aaa[Offs + j][0], Offs + j + 1, Offs + n - 1, @tmp[0], n,
                    2 * n - j - 2);
                end;
              r := 1 / AJJ;
              APVMul(@aaa[Offs + j][0], Offs + j + 1, Offs + n - 1, r);
            end;
          inc(j);
        end;
    end
  else
    begin

      //
      // Compute the Cholesky factorization A = L*L'.
      //
      j := 0;
      while j <= n - 1 do
        begin

          //
          // Compute L(J+1,J+1) and test for non-positive-definiteness.
          //
          v := APVDotProduct(@aaa[Offs + j][0], Offs, Offs + j - 1,
            @aaa[Offs + j][0], Offs, Offs + j - 1);
          AJJ := aaa[Offs + j, Offs + j] - v;
          if AP_FP_Less_Eq(AJJ, 0) then
            begin
              aaa[Offs + j, Offs + j] := AJJ;
              Result := False;
              Exit;
            end;
          AJJ := Sqrt(AJJ);
          aaa[Offs + j, Offs + j] := AJJ;

          //
          // Compute elements J+1:N of column J.
          //
          if j < n - 1 then
            begin
              if j > 0 then
                begin
                  APVMove(@tmp[0], 0, j - 1, @aaa[Offs + j][0], Offs, Offs + j - 1);
                  RMatrixMV(n - j - 1, j, aaa, Offs + j + 1, Offs, 0, tmp, 0, tmp, n);
                  i := 0;
                  while i <= n - j - 2 do
                    begin
                      aaa[Offs + j + 1 + i, Offs + j] :=
                        (aaa[Offs + j + 1 + i, Offs + j] - tmp[n + i]) / AJJ;
                      inc(i);
                    end;
                end
              else
                begin
                  i := 0;
                  while i <= n - j - 2 do
                    begin
                      aaa[Offs + j + 1 + i, Offs + j] :=
                        aaa[Offs + j + 1 + i, Offs + j] / AJJ;
                      inc(i);
                    end;
                end;
            end;
          inc(j);
        end;
    end;
end; 
 
 
