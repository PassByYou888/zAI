{ ****************************************************************************** }
{ * Matrix inverse support, by QQ 600585@qq.com                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

procedure RMatrixTRInverseRec(var a: TLMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; IsUnit: Boolean; var tmp: TLVec;
  var Info: TLInt; var Rep: TMatInvReport); forward;
procedure CMatrixTRInverseRec(var a: TLComplexMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; IsUnit: Boolean; var tmp: TLComplexVec;
  var Info: TLInt; var Rep: TMatInvReport); forward;
procedure RMatrixLUInverseRec(var a: TLMatrix; Offs: TLInt;
  n: TLInt; var Work: TLVec; var Info: TLInt;
  var Rep: TMatInvReport); forward;
procedure CMatrixLUInverseRec(var a: TLComplexMatrix; Offs: TLInt;
  n: TLInt; var Work: TLComplexVec; var Info: TLInt;
  var Rep: TMatInvReport); forward;
procedure SPDMatrixCholeskyInverseRec(var a: TLMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; var tmp: TLVec); forward;
procedure HPDMatrixCholeskyInverseRec(var a: TLComplexMatrix;
  Offs: TLInt; n: TLInt; IsUpper: Boolean;
  var tmp: TLComplexVec); forward;


(* ************************************************************************
  Inversion of a matrix given by its LU decomposition.

  INPUT PARAMETERS:
  A       -   LU decomposition of the matrix (output of RMatrixLU subroutine).
  Pivots  -   table of permutations which were made during the LU decomposition
  (the output of RMatrixLU subroutine).
  N       -   size of matrix A.

  OUTPUT PARAMETERS:
  Info    -   return code:
  * -3    A is singular, or VERY close to singular.
  it is filled by zeros in such cases.
  * -1    N<=0 was passed, or incorrect Pivots was passed
  *  1    task is solved (but matrix A may be ill-conditioned,
  check R1/RInf parameters for condition numbers).
  Rep     -   solver report, see below for more info
  A       -   inverse of matrix A.
  Array whose indexes range within [0..N-1, 0..N-1].

  SOLVER REPORT

  Subroutine sets following fields of the Rep structure:
  * R1        reciprocal of condition number: 1/cond(A), 1-norm.
  * RInf      reciprocal of condition number: 1/cond(A), inf-norm.
  ************************************************************************ *)
procedure RMatrixLUInverse(var a: TLMatrix; const Pivots: TLIVec;
  n: TLInt; var Info: TLInt; var Rep: TMatInvReport);
var
  Work: TLVec;
  i: TLInt;
  j: TLInt;
  k: TLInt;
  v: TLFloat;
begin
  Info := 1;

  //
  // Quick return if possible
  //
  if n = 0 then
    begin
      Info := -1;
      Exit;
    end;
  i := 0;
  while i <= n - 1 do
    begin
      if (Pivots[i] > n - 1) or (Pivots[i] < i) then
        begin
          Info := -1;
          Exit;
        end;
      inc(i);
    end;

  //
  // calculate condition numbers
  //
  Rep.r1 := RMatrixLURCond1(a, n);
  Rep.RInf := RMatrixLURCondInf(a, n);
  if AP_FP_Less(Rep.r1, RCondThreshold) or AP_FP_Less(Rep.RInf, RCondThreshold)
  then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          j := 0;
          while j <= n - 1 do
            begin
              a[i, j] := 0;
              inc(j);
            end;
          inc(i);
        end;
      Rep.r1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;

  //
  // Call cache-oblivious code
  //
  SetLength(Work, n);
  RMatrixLUInverseRec(a, 0, n, Work, Info, Rep);

  //
  // apply permutations
  //
  i := 0;
  while i <= n - 1 do
    begin
      j := n - 2;
      while j >= 0 do
        begin
          k := Pivots[j];
          v := a[i, j];
          a[i, j] := a[i, k];
          a[i, k] := v;
          dec(j);
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Inversion of a general matrix.

  Input parameters:
  A   -   matrix. Array whose indexes range within [0..N-1, 0..N-1].
  N   -   size of matrix A.

  Output parameters:
  Info    -   return code, same as in RMatrixLUInverse
  Rep     -   solver report, same as in RMatrixLUInverse
  A       -   inverse of matrix A, same as in RMatrixLUInverse

  Result:
  True, if the matrix is not singular.
  False, if the matrix is singular.
  ************************************************************************ *)
procedure RMatrixInverse(var a: TLMatrix; n: TLInt;
  var Info: TLInt; var Rep: TMatInvReport);
var
  Pivots: TLIVec;
begin
  RMatrixLU(a, n, n, Pivots);
  RMatrixLUInverse(a, Pivots, n, Info, Rep);
end;

(* ************************************************************************
  Inversion of a matrix given by its LU decomposition.

  INPUT PARAMETERS:
  A       -   LU decomposition of the matrix (output of CMatrixLU subroutine).
  Pivots  -   table of permutations which were made during the LU decomposition
  (the output of CMatrixLU subroutine).
  N       -   size of matrix A.

  OUTPUT PARAMETERS:
  Info    -   return code, same as in RMatrixLUInverse
  Rep     -   solver report, same as in RMatrixLUInverse
  A       -   inverse of matrix A, same as in RMatrixLUInverse
  ************************************************************************ *)
procedure CMatrixLUInverse(var a: TLComplexMatrix;
  const Pivots: TLIVec; n: TLInt; var Info: TLInt;
  var Rep: TMatInvReport);
var
  Work: TLComplexVec;
  i: TLInt;
  j: TLInt;
  k: TLInt;
  v: TLComplex;
begin
  Info := 1;

  //
  // Quick return if possible
  //
  if n = 0 then
    begin
      Info := -1;
      Exit;
    end;
  i := 0;
  while i <= n - 1 do
    begin
      if (Pivots[i] > n - 1) or (Pivots[i] < i) then
        begin
          Info := -1;
          Exit;
        end;
      inc(i);
    end;

  //
  // calculate condition numbers
  //
  Rep.r1 := CMatrixLURCond1(a, n);
  Rep.RInf := CMatrixLURCondInf(a, n);
  if AP_FP_Less(Rep.r1, RCondThreshold) or AP_FP_Less(Rep.RInf, RCondThreshold)
  then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          j := 0;
          while j <= n - 1 do
            begin
              a[i, j] := C_Complex(0);
              inc(j);
            end;
          inc(i);
        end;
      Rep.r1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;

  //
  // Call cache-oblivious code
  //
  SetLength(Work, n);
  CMatrixLUInverseRec(a, 0, n, Work, Info, Rep);

  //
  // apply permutations
  //
  i := 0;
  while i <= n - 1 do
    begin
      j := n - 2;
      while j >= 0 do
        begin
          k := Pivots[j];
          v := a[i, j];
          a[i, j] := a[i, k];
          a[i, k] := v;
          dec(j);
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Inversion of a general matrix.

  Input parameters:
  A   -   matrix, array[0..N-1,0..N-1].
  N   -   size of A.

  Output parameters:
  Info    -   return code, same as in RMatrixLUInverse
  Rep     -   solver report, same as in RMatrixLUInverse
  A       -   inverse of matrix A, same as in RMatrixLUInverse
  ************************************************************************ *)
procedure CMatrixInverse(var a: TLComplexMatrix; n: TLInt;
  var Info: TLInt; var Rep: TMatInvReport);
var
  Pivots: TLIVec;
begin
  CMatrixLU(a, n, n, Pivots);
  CMatrixLUInverse(a, Pivots, n, Info, Rep);
end;

(* ************************************************************************
  Inversion of a symmetric positive definite matrix which is given
  by Cholesky decomposition.

  Input parameters:
  A       -   Cholesky decomposition of the matrix to be inverted:
  A=U?U or A = L*L'.
  Output of  SPDMatrixCholesky subroutine.
  N       -   size of matrix A.
  IsUpper ?  storage format.
  If IsUpper = True, then matrix A is given as A = U'*U
  (matrix contains upper triangle).
  Similarly, if IsUpper = False, then A = L*L'.

  Output parameters:
  Info    -   return code, same as in RMatrixLUInverse
  Rep     -   solver report, same as in RMatrixLUInverse
  A       -   inverse of matrix A, same as in RMatrixLUInverse
  ************************************************************************ *)
procedure SPDMatrixCholeskyInverse(var a: TLMatrix; n: TLInt;
  IsUpper: Boolean; var Info: TLInt; var Rep: TMatInvReport);
var
  i: TLInt;
  j: TLInt;
  k: TLInt;
  v: TLFloat;
  AJJ: TLFloat;
  AII: TLFloat;
  tmp: TLVec;
  Info2: TLInt;
  Rep2: TMatInvReport;
begin
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;
  Info := 1;

  //
  // calculate condition numbers
  //
  Rep.r1 := SPDMatrixCholeskyRCond(a, n, IsUpper);
  Rep.RInf := Rep.r1;
  if AP_FP_Less(Rep.r1, RCondThreshold) or AP_FP_Less(Rep.RInf, RCondThreshold)
  then
    begin
      if IsUpper then
        begin
          i := 0;
          while i <= n - 1 do
            begin
              j := i;
              while j <= n - 1 do
                begin
                  a[i, j] := 0;
                  inc(j);
                end;
              inc(i);
            end;
        end
      else
        begin
          i := 0;
          while i <= n - 1 do
            begin
              j := 0;
              while j <= i do
                begin
                  a[i, j] := 0;
                  inc(j);
                end;
              inc(i);
            end;
        end;
      Rep.r1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;

  //
  // Inverse
  //
  SetLength(tmp, n);
  SPDMatrixCholeskyInverseRec(a, 0, n, IsUpper, tmp);
end;

(* ************************************************************************
  Inversion of a symmetric positive definite matrix.

  Given an upper or lower triangle of a symmetric positive definite matrix,
  the algorithm generates matrix A^-1 and saves the upper or lower triangle
  depending on the input.

  Input parameters:
  A       -   matrix to be inverted (upper or lower triangle). Array with elements [0..N-1,0..N-1].
  N       -   size of matrix A.
  IsUpper -   storage format.
  If IsUpper = True, then the upper triangle of matrix A is
  given, otherwise the lower triangle is given.

  Output parameters:
  Info    -   return code, same as in RMatrixLUInverse
  Rep     -   solver report, same as in RMatrixLUInverse
  A       -   inverse of matrix A, same as in RMatrixLUInverse
  ************************************************************************ *)
procedure SPDMatrixInverse(var a: TLMatrix; n: TLInt;
  IsUpper: Boolean; var Info: TLInt; var Rep: TMatInvReport);
begin
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;
  Info := 1;
  if SPDMatrixCholesky(a, n, IsUpper) then
    begin
      SPDMatrixCholeskyInverse(a, n, IsUpper, Info, Rep);
    end
  else
    begin
      Info := -3;
    end;
end;

(* ************************************************************************
  Inversion of a Hermitian positive definite matrix which is given
  by Cholesky decomposition.
  ************************************************************************ *)
procedure HPDMatrixCholeskyInverse(var a: TLComplexMatrix; n: TLInt;
  IsUpper: Boolean; var Info: TLInt; var Rep: TMatInvReport);
var
  i: TLInt;
  j: TLInt;
  Info2: TLInt;
  Rep2: TMatInvReport;
  tmp: TLComplexVec;
  v: TLComplex;
begin
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;
  Info := 1;

  //
  // calculate condition numbers
  //
  Rep.r1 := HPDMatrixCholeskyRCond(a, n, IsUpper);
  Rep.RInf := Rep.r1;
  if AP_FP_Less(Rep.r1, RCondThreshold) or AP_FP_Less(Rep.RInf, RCondThreshold)
  then
    begin
      if IsUpper then
        begin
          i := 0;
          while i <= n - 1 do
            begin
              j := i;
              while j <= n - 1 do
                begin
                  a[i, j] := C_Complex(0);
                  inc(j);
                end;
              inc(i);
            end;
        end
      else
        begin
          i := 0;
          while i <= n - 1 do
            begin
              j := 0;
              while j <= i do
                begin
                  a[i, j] := C_Complex(0);
                  inc(j);
                end;
              inc(i);
            end;
        end;
      Rep.r1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;

  //
  // Inverse
  //
  SetLength(tmp, n);
  HPDMatrixCholeskyInverseRec(a, 0, n, IsUpper, tmp);
end;

(* ************************************************************************
  Inversion of a Hermitian positive definite matrix.

  Given an upper or lower triangle of a Hermitian positive definite matrix,
  the algorithm generates matrix A^-1 and saves the upper or lower triangle
  depending on the input.

  Input parameters:
  A       -   matrix to be inverted (upper or lower triangle). Array with elements [0..N-1,0..N-1].
  N       -   size of matrix A.
  IsUpper -   storage format.
  If IsUpper = True, then the upper triangle of matrix A is
  given, otherwise the lower triangle is given.

  Output parameters:
  Info    -   return code, same as in RMatrixLUInverse
  Rep     -   solver report, same as in RMatrixLUInverse
  A       -   inverse of matrix A, same as in RMatrixLUInverse
  ************************************************************************ *)
procedure HPDMatrixInverse(var a: TLComplexMatrix; n: TLInt;
  IsUpper: Boolean; var Info: TLInt; var Rep: TMatInvReport);
begin
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;
  Info := 1;
  if HPDMatrixCholesky(a, n, IsUpper) then
    begin
      HPDMatrixCholeskyInverse(a, n, IsUpper, Info, Rep);
    end
  else
    begin
      Info := -3;
    end;
end;

(* ************************************************************************
  Triangular matrix inverse (real)

  The subroutine inverts the following types of matrices:
  * upper triangular
  * upper triangular with unit diagonal
  * lower triangular
  * lower triangular with unit diagonal

  In case of an upper (lower) triangular matrix,  the  inverse  matrix  will
  also be upper (lower) triangular, and after the end of the algorithm,  the
  inverse matrix replaces the source matrix. The elements  below (above) the
  main diagonal are not changed by the algorithm.

  If  the matrix  has a unit diagonal, the inverse matrix also  has  a  unit
  diagonal, and the diagonal elements are not passed to the algorithm.

  Input parameters:
  A       -   matrix, array[0..N-1, 0..N-1].
  N       -   size of A.
  IsUpper -   True, if the matrix is upper triangular.
  IsUnit  -   True, if the matrix has a unit diagonal.

  Output parameters:
  Info    -   same as for RMatrixLUInverse
  Rep     -   same as for RMatrixLUInverse
  A       -   same as for RMatrixLUInverse.
  ************************************************************************ *)
procedure RMatrixTRInverse(var a: TLMatrix; n: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; var Info: TLInt;
  var Rep: TMatInvReport);
var
  i: TLInt;
  j: TLInt;
  tmp: TLVec;
begin
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;
  Info := 1;

  //
  // calculate condition numbers
  //
  Rep.r1 := RMatrixTRRCond1(a, n, IsUpper, IsUnit);
  Rep.RInf := RMatrixTRRCondInf(a, n, IsUpper, IsUnit);
  if AP_FP_Less(Rep.r1, RCondThreshold) or AP_FP_Less(Rep.RInf, RCondThreshold)
  then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          j := 0;
          while j <= n - 1 do
            begin
              a[i, j] := 0;
              inc(j);
            end;
          inc(i);
        end;
      Rep.r1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;

  //
  // Invert
  //
  SetLength(tmp, n);
  RMatrixTRInverseRec(a, 0, n, IsUpper, IsUnit, tmp, Info, Rep);
end;

(* ************************************************************************
  Triangular matrix inverse (TLComplex)

  The subroutine inverts the following types of matrices:
  * upper triangular
  * upper triangular with unit diagonal
  * lower triangular
  * lower triangular with unit diagonal

  In case of an upper (lower) triangular matrix,  the  inverse  matrix  will
  also be upper (lower) triangular, and after the end of the algorithm,  the
  inverse matrix replaces the source matrix. The elements  below (above) the
  main diagonal are not changed by the algorithm.

  If  the matrix  has a unit diagonal, the inverse matrix also  has  a  unit
  diagonal, and the diagonal elements are not passed to the algorithm.

  Input parameters:
  A       -   matrix, array[0..N-1, 0..N-1].
  N       -   size of A.
  IsUpper -   True, if the matrix is upper triangular.
  IsUnit  -   True, if the matrix has a unit diagonal.

  Output parameters:
  Info    -   same as for RMatrixLUInverse
  Rep     -   same as for RMatrixLUInverse
  A       -   same as for RMatrixLUInverse.
  ************************************************************************ *)
procedure CMatrixTRInverse(var a: TLComplexMatrix; n: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; var Info: TLInt;
  var Rep: TMatInvReport);
var
  i: TLInt;
  j: TLInt;
  tmp: TLComplexVec;
begin
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;
  Info := 1;

  //
  // calculate condition numbers
  //
  Rep.r1 := CMatrixTRRCond1(a, n, IsUpper, IsUnit);
  Rep.RInf := CMatrixTRRCondInf(a, n, IsUpper, IsUnit);
  if AP_FP_Less(Rep.r1, RCondThreshold) or AP_FP_Less(Rep.RInf, RCondThreshold)
  then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          j := 0;
          while j <= n - 1 do
            begin
              a[i, j] := C_Complex(0);
              inc(j);
            end;
          inc(i);
        end;
      Rep.r1 := 0;
      Rep.RInf := 0;
      Info := -3;
      Exit;
    end;

  //
  // Invert
  //
  SetLength(tmp, n);
  CMatrixTRInverseRec(a, 0, n, IsUpper, IsUnit, tmp, Info, Rep);
end;

(* ************************************************************************
  Triangular matrix inversion, recursive subroutine
  ************************************************************************ *)
procedure RMatrixTRInverseRec(var a: TLMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; IsUnit: Boolean; var tmp: TLVec;
  var Info: TLInt; var Rep: TMatInvReport);
var
  n1: TLInt;
  n2: TLInt;
  i: TLInt;
  j: TLInt;
  v: TLFloat;
  AJJ: TLFloat;
  i_: TLInt;
  i1_: TLInt;
begin
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;

  //
  // Base case
  //
  if n <= ABLASBlockSize(a) then
    begin
      if IsUpper then
        begin

          //
          // Compute inverse of upper triangular matrix.
          //
          j := 0;
          while j <= n - 1 do
            begin
              if not IsUnit then
                begin
                  if AP_FP_Eq(a[Offs + j, Offs + j], 0) then
                    begin
                      Info := -3;
                      Exit;
                    end;
                  a[Offs + j, Offs + j] := 1 / a[Offs + j, Offs + j];
                  AJJ := -a[Offs + j, Offs + j];
                end
              else
                begin
                  AJJ := -1;
                end;

              //
              // Compute elements 1:j-1 of j-th column.
              //
              if j > 0 then
                begin
                  i1_ := (Offs + 0) - (0);
                  for i_ := 0 to j - 1 do
                    begin
                      tmp[i_] := a[i_ + i1_, Offs + j];
                    end;
                  i := 0;
                  while i <= j - 1 do
                    begin
                      if i < j - 1 then
                        begin
                          v := APVDotProduct(@a[Offs + i][0], Offs + i + 1, Offs + j - 1,
                            @tmp[0], i + 1, j - 1);
                        end
                      else
                        begin
                          v := 0;
                        end;
                      if not IsUnit then
                        begin
                          a[Offs + i, Offs + j] := v + a[Offs + i, Offs + i] * tmp[i];
                        end
                      else
                        begin
                          a[Offs + i, Offs + j] := v + tmp[i];
                        end;
                      inc(i);
                    end;
                  for i_ := Offs + 0 to Offs + j - 1 do
                    begin
                      a[i_, Offs + j] := AJJ * a[i_, Offs + j];
                    end;
                end;
              inc(j);
            end;
        end
      else
        begin

          //
          // Compute inverse of lower triangular matrix.
          //
          j := n - 1;
          while j >= 0 do
            begin
              if not IsUnit then
                begin
                  if AP_FP_Eq(a[Offs + j, Offs + j], 0) then
                    begin
                      Info := -3;
                      Exit;
                    end;
                  a[Offs + j, Offs + j] := 1 / a[Offs + j, Offs + j];
                  AJJ := -a[Offs + j, Offs + j];
                end
              else
                begin
                  AJJ := -1;
                end;
              if j < n - 1 then
                begin

                  //
                  // Compute elements j+1:n of j-th column.
                  //
                  i1_ := (Offs + j + 1) - (j + 1);
                  for i_ := j + 1 to n - 1 do
                    begin
                      tmp[i_] := a[i_ + i1_, Offs + j];
                    end;
                  i := j + 1;
                  while i <= n - 1 do
                    begin
                      if i > j + 1 then
                        begin
                          v := APVDotProduct(@a[Offs + i][0], Offs + j + 1, Offs + i - 1,
                            @tmp[0], j + 1, i - 1);
                        end
                      else
                        begin
                          v := 0;
                        end;
                      if not IsUnit then
                        begin
                          a[Offs + i, Offs + j] := v + a[Offs + i, Offs + i] * tmp[i];
                        end
                      else
                        begin
                          a[Offs + i, Offs + j] := v + tmp[i];
                        end;
                      inc(i);
                    end;
                  for i_ := Offs + j + 1 to Offs + n - 1 do
                    begin
                      a[i_, Offs + j] := AJJ * a[i_, Offs + j];
                    end;
                end;
              dec(j);
            end;
        end;
      Exit;
    end;

  //
  // Recursive case
  //
  ABLASSplitLength(a, n, n1, n2);
  if n2 > 0 then
    begin
      if IsUpper then
        begin
          i := 0;
          while i <= n1 - 1 do
            begin
              APVMul(@a[Offs + i][0], Offs + n1, Offs + n - 1, -1);
              inc(i);
            end;
          RMatrixLeftTRSM(n1, n2, a, Offs, Offs, IsUpper, IsUnit, 0, a, Offs,
            Offs + n1);
          RMatrixRightTRSM(n1, n2, a, Offs + n1, Offs + n1, IsUpper, IsUnit, 0, a,
            Offs, Offs + n1);
        end
      else
        begin
          i := 0;
          while i <= n2 - 1 do
            begin
              APVMul(@a[Offs + n1 + i][0], Offs, Offs + n1 - 1, -1);
              inc(i);
            end;
          RMatrixRightTRSM(n2, n1, a, Offs, Offs, IsUpper, IsUnit, 0, a,
            Offs + n1, Offs);
          RMatrixLeftTRSM(n2, n1, a, Offs + n1, Offs + n1, IsUpper, IsUnit, 0, a,
            Offs + n1, Offs);
        end;
      RMatrixTRInverseRec(a, Offs + n1, n2, IsUpper, IsUnit, tmp, Info, Rep);
    end;
  RMatrixTRInverseRec(a, Offs, n1, IsUpper, IsUnit, tmp, Info, Rep);
end;

(* ************************************************************************
  Triangular matrix inversion, recursive subroutine
  ************************************************************************ *)
procedure CMatrixTRInverseRec(var a: TLComplexMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; IsUnit: Boolean; var tmp: TLComplexVec;
  var Info: TLInt; var Rep: TMatInvReport);
var
  n1: TLInt;
  n2: TLInt;
  i: TLInt;
  j: TLInt;
  v: TLComplex;
  AJJ: TLComplex;
  i_: TLInt;
  i1_: TLInt;
begin
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;

  //
  // Base case
  //
  if n <= ABLASComplexBlockSize(a) then
    begin
      if IsUpper then
        begin

          //
          // Compute inverse of upper triangular matrix.
          //
          j := 0;
          while j <= n - 1 do
            begin
              if not IsUnit then
                begin
                  if C_EqualR(a[Offs + j, Offs + j], 0) then
                    begin
                      Info := -3;
                      Exit;
                    end;
                  a[Offs + j, Offs + j] := C_RDiv(1, a[Offs + j, Offs + j]);
                  AJJ := C_Opposite(a[Offs + j, Offs + j]);
                end
              else
                begin
                  AJJ := C_Complex(-1);
                end;

              //
              // Compute elements 1:j-1 of j-th column.
              //
              if j > 0 then
                begin
                  i1_ := (Offs + 0) - (0);
                  for i_ := 0 to j - 1 do
                    begin
                      tmp[i_] := a[i_ + i1_, Offs + j];
                    end;
                  i := 0;
                  while i <= j - 1 do
                    begin
                      if i < j - 1 then
                        begin
                          i1_ := (i + 1) - (Offs + i + 1);
                          v := C_Complex(0.0);
                          for i_ := Offs + i + 1 to Offs + j - 1 do
                            begin
                              v := C_Add(v, C_Mul(a[Offs + i, i_], tmp[i_ + i1_]));
                            end;
                        end
                      else
                        begin
                          v := C_Complex(0);
                        end;
                      if not IsUnit then
                        begin
                          a[Offs + i, Offs + j] :=
                            C_Add(v, C_Mul(a[Offs + i, Offs + i], tmp[i]));
                        end
                      else
                        begin
                          a[Offs + i, Offs + j] := C_Add(v, tmp[i]);
                        end;
                      inc(i);
                    end;
                  for i_ := Offs + 0 to Offs + j - 1 do
                    begin
                      a[i_, Offs + j] := C_Mul(AJJ, a[i_, Offs + j]);
                    end;
                end;
              inc(j);
            end;
        end
      else
        begin

          //
          // Compute inverse of lower triangular matrix.
          //
          j := n - 1;
          while j >= 0 do
            begin
              if not IsUnit then
                begin
                  if C_EqualR(a[Offs + j, Offs + j], 0) then
                    begin
                      Info := -3;
                      Exit;
                    end;
                  a[Offs + j, Offs + j] := C_RDiv(1, a[Offs + j, Offs + j]);
                  AJJ := C_Opposite(a[Offs + j, Offs + j]);
                end
              else
                begin
                  AJJ := C_Complex(-1);
                end;
              if j < n - 1 then
                begin

                  //
                  // Compute elements j+1:n of j-th column.
                  //
                  i1_ := (Offs + j + 1) - (j + 1);
                  for i_ := j + 1 to n - 1 do
                    begin
                      tmp[i_] := a[i_ + i1_, Offs + j];
                    end;
                  i := j + 1;
                  while i <= n - 1 do
                    begin
                      if i > j + 1 then
                        begin
                          i1_ := (j + 1) - (Offs + j + 1);
                          v := C_Complex(0.0);
                          for i_ := Offs + j + 1 to Offs + i - 1 do
                            begin
                              v := C_Add(v, C_Mul(a[Offs + i, i_], tmp[i_ + i1_]));
                            end;
                        end
                      else
                        begin
                          v := C_Complex(0);
                        end;
                      if not IsUnit then
                        begin
                          a[Offs + i, Offs + j] :=
                            C_Add(v, C_Mul(a[Offs + i, Offs + i], tmp[i]));
                        end
                      else
                        begin
                          a[Offs + i, Offs + j] := C_Add(v, tmp[i]);
                        end;
                      inc(i);
                    end;
                  for i_ := Offs + j + 1 to Offs + n - 1 do
                    begin
                      a[i_, Offs + j] := C_Mul(AJJ, a[i_, Offs + j]);
                    end;
                end;
              dec(j);
            end;
        end;
      Exit;
    end;

  //
  // Recursive case
  //
  ABLASComplexSplitLength(a, n, n1, n2);
  if n2 > 0 then
    begin
      if IsUpper then
        begin
          i := 0;
          while i <= n1 - 1 do
            begin
              for i_ := Offs + n1 to Offs + n - 1 do
                begin
                  a[Offs + i, i_] := C_MulR(a[Offs + i, i_], -1);
                end;
              inc(i);
            end;
          CMatrixLeftTRSM(n1, n2, a, Offs, Offs, IsUpper, IsUnit, 0, a, Offs,
            Offs + n1);
          CMatrixRightTRSM(n1, n2, a, Offs + n1, Offs + n1, IsUpper, IsUnit, 0, a,
            Offs, Offs + n1);
        end
      else
        begin
          i := 0;
          while i <= n2 - 1 do
            begin
              for i_ := Offs to Offs + n1 - 1 do
                begin
                  a[Offs + n1 + i, i_] := C_MulR(a[Offs + n1 + i, i_], -1);
                end;
              inc(i);
            end;
          CMatrixRightTRSM(n2, n1, a, Offs, Offs, IsUpper, IsUnit, 0, a,
            Offs + n1, Offs);
          CMatrixLeftTRSM(n2, n1, a, Offs + n1, Offs + n1, IsUpper, IsUnit, 0, a,
            Offs + n1, Offs);
        end;
      CMatrixTRInverseRec(a, Offs + n1, n2, IsUpper, IsUnit, tmp, Info, Rep);
    end;
  CMatrixTRInverseRec(a, Offs, n1, IsUpper, IsUnit, tmp, Info, Rep);
end;

procedure RMatrixLUInverseRec(var a: TLMatrix; Offs: TLInt;
  n: TLInt; var Work: TLVec; var Info: TLInt;
  var Rep: TMatInvReport);
var
  i: TLInt;
  IWS: TLInt;
  j: TLInt;
  JB: TLInt;
  JJ: TLInt;
  JP: TLInt;
  k: TLInt;
  v: TLFloat;
  n1: TLInt;
  n2: TLInt;
begin
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;

  //
  // Base case
  //
  if n <= ABLASBlockSize(a) then
    begin

      //
      // Form inv(U)
      //
      RMatrixTRInverseRec(a, Offs, n, True, False, Work, Info, Rep);
      if Info <= 0 then
        begin
          Exit;
        end;

      //
      // Solve the equation inv(A)*L = inv(U) for inv(A).
      //
      j := n - 1;
      while j >= 0 do
        begin

          //
          // Copy current column of L to WORK and replace with zeros.
          //
          i := j + 1;
          while i <= n - 1 do
            begin
              Work[i] := a[Offs + i, Offs + j];
              a[Offs + i, Offs + j] := 0;
              inc(i);
            end;

          //
          // Compute current column of inv(A).
          //
          if j < n - 1 then
            begin
              i := 0;
              while i <= n - 1 do
                begin
                  v := APVDotProduct(@a[Offs + i][0], Offs + j + 1, Offs + n - 1,
                    @Work[0], j + 1, n - 1);
                  a[Offs + i, Offs + j] := a[Offs + i, Offs + j] - v;
                  inc(i);
                end;
            end;
          dec(j);
        end;
      Exit;
    end;

  //
  // Recursive code:
  //
  // ( L1      )   ( U1  U12 )
  // A    =  (         ) * (         )
  // ( L12  L2 )   (     U2  )
  //
  // ( W   X )
  // A^-1 =  (       )
  // ( Y   Z )
  //
  ABLASSplitLength(a, n, n1, n2);
  Assert(n2 > 0, 'LUInverseRec: internal error!');

  //
  // X := inv(U1)*U12*inv(U2)
  //
  RMatrixLeftTRSM(n1, n2, a, Offs, Offs, True, False, 0, a, Offs, Offs + n1);
  RMatrixRightTRSM(n1, n2, a, Offs + n1, Offs + n1, True, False, 0, a, Offs,
    Offs + n1);

  //
  // Y := inv(L2)*L12*inv(L1)
  //
  RMatrixLeftTRSM(n2, n1, a, Offs + n1, Offs + n1, False, True, 0, a,
    Offs + n1, Offs);
  RMatrixRightTRSM(n2, n1, a, Offs, Offs, False, True, 0, a, Offs + n1, Offs);

  //
  // W := inv(L1*U1)+X*Y
  //
  RMatrixLUInverseRec(a, Offs, n1, Work, Info, Rep);
  if Info <= 0 then
    begin
      Exit;
    end;
  RMatrixGEMM(n1, n1, n2, 1.0, a, Offs, Offs + n1, 0, a, Offs + n1, Offs, 0,
    1.0, a, Offs, Offs);

  //
  // X := -X*inv(L2)
  // Y := -inv(U2)*Y
  //
  RMatrixRightTRSM(n1, n2, a, Offs + n1, Offs + n1, False, True, 0, a, Offs,
    Offs + n1);
  i := 0;
  while i <= n1 - 1 do
    begin
      APVMul(@a[Offs + i][0], Offs + n1, Offs + n - 1, -1);
      inc(i);
    end;
  RMatrixLeftTRSM(n2, n1, a, Offs + n1, Offs + n1, True, False, 0, a,
    Offs + n1, Offs);
  i := 0;
  while i <= n2 - 1 do
    begin
      APVMul(@a[Offs + n1 + i][0], Offs, Offs + n1 - 1, -1);
      inc(i);
    end;

  //
  // Z := inv(L2*U2)
  //
  RMatrixLUInverseRec(a, Offs + n1, n2, Work, Info, Rep);
end;

procedure CMatrixLUInverseRec(var a: TLComplexMatrix; Offs: TLInt;
  n: TLInt; var Work: TLComplexVec; var Info: TLInt;
  var Rep: TMatInvReport);
var
  i: TLInt;
  IWS: TLInt;
  j: TLInt;
  JB: TLInt;
  JJ: TLInt;
  JP: TLInt;
  k: TLInt;
  v: TLComplex;
  n1: TLInt;
  n2: TLInt;
  i_: TLInt;
  i1_: TLInt;
begin
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;

  //
  // Base case
  //
  if n <= ABLASComplexBlockSize(a) then
    begin

      //
      // Form inv(U)
      //
      CMatrixTRInverseRec(a, Offs, n, True, False, Work, Info, Rep);
      if Info <= 0 then
        begin
          Exit;
        end;

      //
      // Solve the equation inv(A)*L = inv(U) for inv(A).
      //
      j := n - 1;
      while j >= 0 do
        begin

          //
          // Copy current column of L to WORK and replace with zeros.
          //
          i := j + 1;
          while i <= n - 1 do
            begin
              Work[i] := a[Offs + i, Offs + j];
              a[Offs + i, Offs + j] := C_Complex(0);
              inc(i);
            end;

          //
          // Compute current column of inv(A).
          //
          if j < n - 1 then
            begin
              i := 0;
              while i <= n - 1 do
                begin
                  i1_ := (j + 1) - (Offs + j + 1);
                  v := C_Complex(0.0);
                  for i_ := Offs + j + 1 to Offs + n - 1 do
                    begin
                      v := C_Add(v, C_Mul(a[Offs + i, i_], Work[i_ + i1_]));
                    end;
                  a[Offs + i, Offs + j] := C_Sub(a[Offs + i, Offs + j], v);
                  inc(i);
                end;
            end;
          dec(j);
        end;
      Exit;
    end;

  //
  // Recursive code:
  //
  // ( L1      )   ( U1  U12 )
  // A    =  (         ) * (         )
  // ( L12  L2 )   (     U2  )
  //
  // ( W   X )
  // A^-1 =  (       )
  // ( Y   Z )
  //
  ABLASComplexSplitLength(a, n, n1, n2);
  Assert(n2 > 0, 'LUInverseRec: internal error!');

  //
  // X := inv(U1)*U12*inv(U2)
  //
  CMatrixLeftTRSM(n1, n2, a, Offs, Offs, True, False, 0, a, Offs, Offs + n1);
  CMatrixRightTRSM(n1, n2, a, Offs + n1, Offs + n1, True, False, 0, a, Offs,
    Offs + n1);

  //
  // Y := inv(L2)*L12*inv(L1)
  //
  CMatrixLeftTRSM(n2, n1, a, Offs + n1, Offs + n1, False, True, 0, a,
    Offs + n1, Offs);
  CMatrixRightTRSM(n2, n1, a, Offs, Offs, False, True, 0, a, Offs + n1, Offs);

  //
  // W := inv(L1*U1)+X*Y
  //
  CMatrixLUInverseRec(a, Offs, n1, Work, Info, Rep);
  if Info <= 0 then
    begin
      Exit;
    end;
  CMatrixGEMM(n1, n1, n2, C_Complex(1.0), a, Offs, Offs + n1, 0, a, Offs + n1,
    Offs, 0, C_Complex(1.0), a, Offs, Offs);

  //
  // X := -X*inv(L2)
  // Y := -inv(U2)*Y
  //
  CMatrixRightTRSM(n1, n2, a, Offs + n1, Offs + n1, False, True, 0, a, Offs,
    Offs + n1);
  i := 0;
  while i <= n1 - 1 do
    begin
      for i_ := Offs + n1 to Offs + n - 1 do
        begin
          a[Offs + i, i_] := C_MulR(a[Offs + i, i_], -1);
        end;
      inc(i);
    end;
  CMatrixLeftTRSM(n2, n1, a, Offs + n1, Offs + n1, True, False, 0, a,
    Offs + n1, Offs);
  i := 0;
  while i <= n2 - 1 do
    begin
      for i_ := Offs to Offs + n1 - 1 do
        begin
          a[Offs + n1 + i, i_] := C_MulR(a[Offs + n1 + i, i_], -1);
        end;
      inc(i);
    end;

  //
  // Z := inv(L2*U2)
  //
  CMatrixLUInverseRec(a, Offs + n1, n2, Work, Info, Rep);
end;

(* ************************************************************************
  Recursive subroutine for SPD inversion.
  ************************************************************************ *)
procedure SPDMatrixCholeskyInverseRec(var a: TLMatrix; Offs: TLInt;
  n: TLInt; IsUpper: Boolean; var tmp: TLVec);
var
  i: TLInt;
  j: TLInt;
  v: TLFloat;
  n1: TLInt;
  n2: TLInt;
  Info2: TLInt;
  Rep2: TMatInvReport;
  i_: TLInt;
  i1_: TLInt;
begin
  if n < 1 then
    begin
      Exit;
    end;

  //
  // Base case
  //
  if n <= ABLASBlockSize(a) then
    begin
      RMatrixTRInverseRec(a, Offs, n, IsUpper, False, tmp, Info2, Rep2);
      if IsUpper then
        begin

          //
          // Compute the product U * U'.
          // NOTE: we never assume that diagonal of U is real
          //
          i := 0;
          while i <= n - 1 do
            begin
              if i = 0 then
                begin

                  //
                  // 1x1 matrix
                  //
                  a[Offs + i, Offs + i] := AP_Sqr(a[Offs + i, Offs + i]);
                end
              else
                begin

                  //
                  // (I+1)x(I+1) matrix,
                  //
                  // ( A11  A12 )   ( A11^H        )   ( A11*A11^H+A12*A12^H  A12*A22^H )
                  // (          ) * (              ) = (                                )
                  // (      A22 )   ( A12^H  A22^H )   ( A22*A12^H            A22*A22^H )
                  //
                  // A11 is IxI, A22 is 1x1.
                  //
                  i1_ := (Offs) - (0);
                  for i_ := 0 to i - 1 do
                    begin
                      tmp[i_] := a[i_ + i1_, Offs + i];
                    end;
                  j := 0;
                  while j <= i - 1 do
                    begin
                      v := a[Offs + j, Offs + i];
                      APVAdd(@a[Offs + j][0], Offs + j, Offs + i - 1, @tmp[0], j,
                        i - 1, v);
                      inc(j);
                    end;
                  v := a[Offs + i, Offs + i];
                  for i_ := Offs to Offs + i - 1 do
                    begin
                      a[i_, Offs + i] := v * a[i_, Offs + i];
                    end;
                  a[Offs + i, Offs + i] := AP_Sqr(a[Offs + i, Offs + i]);
                end;
              inc(i);
            end;
        end
      else
        begin

          //
          // Compute the product L' * L
          // NOTE: we never assume that diagonal of L is real
          //
          i := 0;
          while i <= n - 1 do
            begin
              if i = 0 then
                begin

                  //
                  // 1x1 matrix
                  //
                  a[Offs + i, Offs + i] := AP_Sqr(a[Offs + i, Offs + i]);
                end
              else
                begin

                  //
                  // (I+1)x(I+1) matrix,
                  //
                  // ( A11^H  A21^H )   ( A11      )   ( A11^H*A11+A21^H*A21  A21^H*A22 )
                  // (              ) * (          ) = (                                )
                  // (        A22^H )   ( A21  A22 )   ( A22^H*A21            A22^H*A22 )
                  //
                  // A11 is IxI, A22 is 1x1.
                  //
                  APVMove(@tmp[0], 0, i - 1, @a[Offs + i][0], Offs, Offs + i - 1);
                  j := 0;
                  while j <= i - 1 do
                    begin
                      v := a[Offs + i, Offs + j];
                      APVAdd(@a[Offs + j][0], Offs, Offs + j, @tmp[0], 0, j, v);
                      inc(j);
                    end;
                  v := a[Offs + i, Offs + i];
                  APVMul(@a[Offs + i][0], Offs, Offs + i - 1, v);
                  a[Offs + i, Offs + i] := AP_Sqr(a[Offs + i, Offs + i]);
                end;
              inc(i);
            end;
        end;
      Exit;
    end;

  //
  // Recursive code: triangular factor inversion merged with
  // UU' or L'L multiplication
  //
  ABLASSplitLength(a, n, n1, n2);

  //
  // form off-diagonal block of trangular inverse
  //
  if IsUpper then
    begin
      i := 0;
      while i <= n1 - 1 do
        begin
          APVMul(@a[Offs + i][0], Offs + n1, Offs + n - 1, -1);
          inc(i);
        end;
      RMatrixLeftTRSM(n1, n2, a, Offs, Offs, IsUpper, False, 0, a, Offs,
        Offs + n1);
      RMatrixRightTRSM(n1, n2, a, Offs + n1, Offs + n1, IsUpper, False, 0, a,
        Offs, Offs + n1);
    end
  else
    begin
      i := 0;
      while i <= n2 - 1 do
        begin
          APVMul(@a[Offs + n1 + i][0], Offs, Offs + n1 - 1, -1);
          inc(i);
        end;
      RMatrixRightTRSM(n2, n1, a, Offs, Offs, IsUpper, False, 0, a,
        Offs + n1, Offs);
      RMatrixLeftTRSM(n2, n1, a, Offs + n1, Offs + n1, IsUpper, False, 0, a,
        Offs + n1, Offs);
    end;

  //
  // invert first diagonal block
  //
  SPDMatrixCholeskyInverseRec(a, Offs, n1, IsUpper, tmp);

  //
  // update first diagonal block with off-diagonal block,
  // update off-diagonal block
  //
  if IsUpper then
    begin
      RMatrixSYRK(n1, n2, 1.0, a, Offs, Offs + n1, 0, 1.0, a, Offs, Offs,
        IsUpper);
      RMatrixRightTRSM(n1, n2, a, Offs + n1, Offs + n1, IsUpper, False, 1, a,
        Offs, Offs + n1);
    end
  else
    begin
      RMatrixSYRK(n1, n2, 1.0, a, Offs + n1, Offs, 1, 1.0, a, Offs, Offs,
        IsUpper);
      RMatrixLeftTRSM(n2, n1, a, Offs + n1, Offs + n1, IsUpper, False, 1, a,
        Offs + n1, Offs);
    end;

  //
  // invert second diagonal block
  //
  SPDMatrixCholeskyInverseRec(a, Offs + n1, n2, IsUpper, tmp);
end;

(* ************************************************************************
  Recursive subroutine for HPD inversion.
  ************************************************************************ *)
procedure HPDMatrixCholeskyInverseRec(var a: TLComplexMatrix;
  Offs: TLInt; n: TLInt; IsUpper: Boolean;
  var tmp: TLComplexVec);
var
  i: TLInt;
  j: TLInt;
  v: TLComplex;
  n1: TLInt;
  n2: TLInt;
  Info2: TLInt;
  Rep2: TMatInvReport;
  i_: TLInt;
  i1_: TLInt;
begin
  if n < 1 then
    begin
      Exit;
    end;

  //
  // Base case
  //
  if n <= ABLASComplexBlockSize(a) then
    begin
      CMatrixTRInverseRec(a, Offs, n, IsUpper, False, tmp, Info2, Rep2);
      if IsUpper then
        begin

          //
          // Compute the product U * U'.
          // NOTE: we never assume that diagonal of U is real
          //
          i := 0;
          while i <= n - 1 do
            begin
              if i = 0 then
                begin

                  //
                  // 1x1 matrix
                  //
                  a[Offs + i, Offs + i] := C_Complex(AP_Sqr(a[Offs + i, Offs + i].x) +
                    AP_Sqr(a[Offs + i, Offs + i].y));
                end
              else
                begin

                  //
                  // (I+1)x(I+1) matrix,
                  //
                  // ( A11  A12 )   ( A11^H        )   ( A11*A11^H+A12*A12^H  A12*A22^H )
                  // (          ) * (              ) = (                                )
                  // (      A22 )   ( A12^H  A22^H )   ( A22*A12^H            A22*A22^H )
                  //
                  // A11 is IxI, A22 is 1x1.
                  //
                  i1_ := (Offs) - (0);
                  for i_ := 0 to i - 1 do
                    begin
                      tmp[i_] := Conj(a[i_ + i1_, Offs + i]);
                    end;
                  j := 0;
                  while j <= i - 1 do
                    begin
                      v := a[Offs + j, Offs + i];
                      i1_ := (j) - (Offs + j);
                      for i_ := Offs + j to Offs + i - 1 do
                        begin
                          a[Offs + j, i_] := C_Add(a[Offs + j, i_],
                            C_Mul(v, tmp[i_ + i1_]));
                        end;
                      inc(j);
                    end;
                  v := Conj(a[Offs + i, Offs + i]);
                  for i_ := Offs to Offs + i - 1 do
                    begin
                      a[i_, Offs + i] := C_Mul(v, a[i_, Offs + i]);
                    end;
                  a[Offs + i, Offs + i] := C_Complex(AP_Sqr(a[Offs + i, Offs + i].x) +
                    AP_Sqr(a[Offs + i, Offs + i].y));
                end;
              inc(i);
            end;
        end
      else
        begin

          //
          // Compute the product L' * L
          // NOTE: we never assume that diagonal of L is real
          //
          i := 0;
          while i <= n - 1 do
            begin
              if i = 0 then
                begin

                  //
                  // 1x1 matrix
                  //
                  a[Offs + i, Offs + i] := C_Complex(AP_Sqr(a[Offs + i, Offs + i].x) +
                    AP_Sqr(a[Offs + i, Offs + i].y));
                end
              else
                begin

                  //
                  // (I+1)x(I+1) matrix,
                  //
                  // ( A11^H  A21^H )   ( A11      )   ( A11^H*A11+A21^H*A21  A21^H*A22 )
                  // (              ) * (          ) = (                                )
                  // (        A22^H )   ( A21  A22 )   ( A22^H*A21            A22^H*A22 )
                  //
                  // A11 is IxI, A22 is 1x1.
                  //
                  i1_ := (Offs) - (0);
                  for i_ := 0 to i - 1 do
                    begin
                      tmp[i_] := a[Offs + i, i_ + i1_];
                    end;
                  j := 0;
                  while j <= i - 1 do
                    begin
                      v := Conj(a[Offs + i, Offs + j]);
                      i1_ := (0) - (Offs);
                      for i_ := Offs to Offs + j do
                        begin
                          a[Offs + j, i_] := C_Add(a[Offs + j, i_],
                            C_Mul(v, tmp[i_ + i1_]));
                        end;
                      inc(j);
                    end;
                  v := Conj(a[Offs + i, Offs + i]);
                  for i_ := Offs to Offs + i - 1 do
                    begin
                      a[Offs + i, i_] := C_Mul(v, a[Offs + i, i_]);
                    end;
                  a[Offs + i, Offs + i] := C_Complex(AP_Sqr(a[Offs + i, Offs + i].x) +
                    AP_Sqr(a[Offs + i, Offs + i].y));
                end;
              inc(i);
            end;
        end;
      Exit;
    end;

  //
  // Recursive code: triangular factor inversion merged with
  // UU' or L'L multiplication
  //
  ABLASComplexSplitLength(a, n, n1, n2);

  //
  // form off-diagonal block of trangular inverse
  //
  if IsUpper then
    begin
      i := 0;
      while i <= n1 - 1 do
        begin
          for i_ := Offs + n1 to Offs + n - 1 do
            begin
              a[Offs + i, i_] := C_MulR(a[Offs + i, i_], -1);
            end;
          inc(i);
        end;
      CMatrixLeftTRSM(n1, n2, a, Offs, Offs, IsUpper, False, 0, a, Offs,
        Offs + n1);
      CMatrixRightTRSM(n1, n2, a, Offs + n1, Offs + n1, IsUpper, False, 0, a,
        Offs, Offs + n1);
    end
  else
    begin
      i := 0;
      while i <= n2 - 1 do
        begin
          for i_ := Offs to Offs + n1 - 1 do
            begin
              a[Offs + n1 + i, i_] := C_MulR(a[Offs + n1 + i, i_], -1);
            end;
          inc(i);
        end;
      CMatrixRightTRSM(n2, n1, a, Offs, Offs, IsUpper, False, 0, a,
        Offs + n1, Offs);
      CMatrixLeftTRSM(n2, n1, a, Offs + n1, Offs + n1, IsUpper, False, 0, a,
        Offs + n1, Offs);
    end;

  //
  // invert first diagonal block
  //
  HPDMatrixCholeskyInverseRec(a, Offs, n1, IsUpper, tmp);

  //
  // update first diagonal block with off-diagonal block,
  // update off-diagonal block
  //
  if IsUpper then
    begin
      CMatrixSYRK(n1, n2, 1.0, a, Offs, Offs + n1, 0, 1.0, a, Offs, Offs,
        IsUpper);
      CMatrixRightTRSM(n1, n2, a, Offs + n1, Offs + n1, IsUpper, False, 2, a,
        Offs, Offs + n1);
    end
  else
    begin
      CMatrixSYRK(n1, n2, 1.0, a, Offs + n1, Offs, 2, 1.0, a, Offs, Offs,
        IsUpper);
      CMatrixLeftTRSM(n2, n1, a, Offs + n1, Offs + n1, IsUpper, False, 2, a,
        Offs + n1, Offs);
    end;

  //
  // invert second diagonal block
  //
  HPDMatrixCholeskyInverseRec(a, Offs + n1, n2, IsUpper, tmp);
end; 
 
 
