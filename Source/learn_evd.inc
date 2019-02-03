{ ****************************************************************************** }
{ * Eigensolvers support, by QQ 600585@qq.com                                  * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

function TridiagonalEVD(var d: TLVec; E: TLVec; n: TLInt;
  ZNeeded: TLInt; var z: TLMatrix): Boolean; forward;

procedure TdEVDE2(const a: TLFloat; const b: TLFloat;
  const c: TLFloat; var RT1: TLFloat; var RT2: TLFloat); forward;

procedure TdEVDEV2(const a: TLFloat; const b: TLFloat;
  const c: TLFloat; var RT1: TLFloat; var RT2: TLFloat;
  var CS1: TLFloat; var SN1: TLFloat); forward;

function TdEVDPythag(a: TLFloat; b: TLFloat): TLFloat; forward;
function TdEVDExtSign(a: TLFloat; b: TLFloat): TLFloat; forward;

procedure TDINInternalDLAGTF(const n: TLInt; var a: TLVec;
  const LAMBDA: TLFloat; var b: TLVec; var c: TLVec;
  const TOL: TLFloat; var d: TLVec; var IIN: TLIVec;
  var Info: TLInt); forward;

procedure TDINInternalDLAGTS(const n: TLInt; const a: TLVec;
  const b: TLVec; const c: TLVec; const d: TLVec;
  const IIN: TLIVec; var y: TLVec; var TOL: TLFloat;
  var Info: TLInt); forward;

procedure InternalDLAEBZ(const IJOB: TLInt; const NITMAX: TLInt;
  const n: TLInt; const MMAX: TLInt; const MINP: TLInt;
  const ABSTOL: TLFloat; const RELTOL: TLFloat;
  const PIVMIN: TLFloat; const d: TLVec; const E: TLVec;
  const e2: TLVec; var NVAL: TLIVec; var AB: TLMatrix;
  var c: TLVec; var MOUT: TLInt; var NAB: TLIMatrix;
  var Work: TLVec; var IWORK: TLIVec;
  var Info: TLInt); forward;

procedure InternalTREVC(const t: TLMatrix; n: TLInt;
  SIDE: TLInt; HOWMNY: TLInt; VSELECT: TLBVec;
  var vl: TLMatrix; var vr: TLMatrix; var M: TLInt;
  var Info: TLInt); forward;

procedure InternalHSEVDLALN2(const LTRANS: Boolean; const NA: TLInt;
  const NW: TLInt; const SMIN: TLFloat; const ca: TLFloat;
  const a: TLMatrix; const d1: TLFloat; const d2: TLFloat;
  const b: TLMatrix; const WR: TLFloat; const WI: TLFloat;
  var RSWAP4: TLBVec; var ZSWAP4: TLBVec;
  var IPIVOT44: TLIMatrix; var CIV4: TLVec; var CRV4: TLVec;
  var x: TLMatrix; var SCL: TLFloat; var XNORM: TLFloat;
  var Info: TLInt); forward;

procedure InternalHSEVDLADIV(const a: TLFloat; const b: TLFloat;
  const c: TLFloat; const d: TLFloat; var p: TLFloat;
  var q: TLFloat); forward;

function NonSymmetricEVD(a: TLMatrix; n: TLInt;
  VNeeded: TLInt; var WR: TLVec; var WI: TLVec;
  var vl: TLMatrix; var vr: TLMatrix): Boolean; forward;

procedure ToUpperHessenberg(var a: TLMatrix; n: TLInt;
  var TAU: TLVec); forward;

procedure UnpackQFromUpperHessenberg(const a: TLMatrix; n: TLInt;
  const TAU: TLVec; var q: TLMatrix); forward;

procedure UnpackHFromUpperHessenberg(const a: TLMatrix; n: TLInt;
  const TAU: TLVec; var h: TLMatrix); forward;


(* ************************************************************************
  Finding the eigenvalues and eigenvectors of a symmetric matrix

  The algorithm finds eigen pairs of a symmetric matrix by reducing it to
  tridiagonal form and using the QL/QR algorithm.

  Input parameters:
  A       -   symmetric matrix which is given by its upper or lower triangular part. Array whose indexes range within [0..N-1, 0..N-1].
  N       -   size of matrix A.
  IsUpper -   storage format.

  ZNeeded -   flag controlling whether the eigenvectors are needed or not.
  If ZNeeded is equal to:
  * 0, the eigenvectors are not returned;
  * 1, the eigenvectors are returned.

  Output parameters:
  D       -   eigenvalues in ascending order. Array whose index ranges within [0..N-1].

  Z       -   if ZNeeded is equal to:
  * 0, Z has changed;
  * 1, Z contains the eigenvectors.
  Array whose indexes range within [0..N-1, 0..N-1].
  The eigenvectors are stored in the matrix columns.

  Result:
  True, if the algorithm has converged.
  False, if the algorithm hasn't converged (rare case).
  ************************************************************************ *)
function SMatrixEVD(a: TLMatrix; n: TLInt; ZNeeded: TLInt;
  IsUpper: Boolean; var d: TLVec; var z: TLMatrix): Boolean;
var
  TAU: TLVec;
  E: TLVec;
begin
  a := DynamicArrayCopy(a);
  Assert((ZNeeded = 0) or (ZNeeded = 1), 'SMatrixEVD: incorrect ZNeeded');
  SMatrixTD(a, n, IsUpper, TAU, d, E);
  if ZNeeded = 1 then
    begin
      SMatrixTDUnpackQ(a, n, IsUpper, TAU, z);
    end;
  Result := SMatrixTDEVD(d, E, n, ZNeeded, z);
end;

(* ************************************************************************
  Subroutine for finding the eigenvalues (and eigenvectors) of  a  symmetric
  matrix  in  a  given half open interval (A, B] by using  a  bisection  and
  inverse iteration

  Input parameters:
  A       -   symmetric matrix which is given by its upper or lower triangular part. Array [0..N-1, 0..N-1].
  N       -   size of matrix A.

  ZNeeded -   flag controlling whether the eigenvectors are needed or not.
  If ZNeeded is equal to:
  * 0, the eigenvectors are not returned;
  * 1, the eigenvectors are returned.

  IsUpperA -  storage format of matrix A.
  B1, B2   -    half open interval (B1, B2] to search eigenvalues in.

  Output parameters:
  M       -   number of eigenvalues found in a given half-interval (M>=0).
  W       -   array of the eigenvalues found.
  Array whose index ranges within [0..M-1].
  Z       -   if ZNeeded is equal to:
  * 0, Z hasn't changed;
  * 1, Z contains eigenvectors.
  Array whose indexes range within [0..N-1, 0..M-1].
  The eigenvectors are stored in the matrix columns.

  Result:
  True, if successful. M contains the number of eigenvalues in the given
  half-interval (could be equal to 0), W contains the eigenvalues,
  Z contains the eigenvectors (if needed).

  False, if the bisection method subroutine wasn't able to find the
  eigenvalues in the given interval or if the inverse iteration subroutine
  wasn't able to find all the corresponding eigenvectors.
  In that case, the eigenvalues and eigenvectors are not returned,
  M is equal to 0.
  ************************************************************************ *)
function SMatrixEVDR(a: TLMatrix; n: TLInt; ZNeeded: TLInt;
  IsUpper: Boolean; b1: TLFloat; b2: TLFloat; var M: TLInt;
  var w: TLVec; var z: TLMatrix): Boolean;
var
  TAU: TLVec;
  E: TLVec;
begin
  a := DynamicArrayCopy(a);
  Assert((ZNeeded = 0) or (ZNeeded = 1), 'SMatrixTDEVDR: incorrect ZNeeded');
  SMatrixTD(a, n, IsUpper, TAU, w, E);
  if ZNeeded = 1 then
    begin
      SMatrixTDUnpackQ(a, n, IsUpper, TAU, z);
    end;
  Result := SMatrixTDEVDR(w, E, n, ZNeeded, b1, b2, M, z);
end;

(* ************************************************************************
  Subroutine for finding the eigenvalues and  eigenvectors  of  a  symmetric
  matrix with given indexes by using bisection and inverse iteration methods.

  Input parameters:
  A        -   symmetric matrix which is given by its upper or lower triangular part. Array whose indexes range within [0..N-1, 0..N-1].
  N        -   size of matrix A.
  ZNeeded  -   flag controlling whether the eigenvectors are needed or not.

  If ZNeeded is equal to:
  * 0, the eigenvectors are not returned;
  * 1, the eigenvectors are returned.

  IsUpperA -  storage format of matrix A.
  I1, I2   -    index interval for searching (from I1 to I2).
  0 <= I1 <= I2 <= N-1.

  Output parameters:
  W       -   array of the eigenvalues found.
  Array whose index ranges within [0..I2-I1].
  Z       -   if ZNeeded is equal to:
  * 0, Z hasn't changed;
  * 1, Z contains eigenvectors.
  Array whose indexes range within [0..N-1, 0..I2-I1].
  In that case, the eigenvectors are stored in the matrix columns.

  Result:
  True, if successful. W contains the eigenvalues, Z contains the eigenvectors (if needed).

  False, if the bisection method subroutine wasn't able to find the
  eigenvalues in the given interval or if the inverse iteration subroutine
  wasn't able to find all the corresponding eigenvectors.
  In that case, the eigenvalues and eigenvectors are not returned.
  ************************************************************************ *)
function SMatrixEVDI(a: TLMatrix; n: TLInt; ZNeeded: TLInt;
  IsUpper: Boolean; i1: TLInt; i2: TLInt; var w: TLVec;
  var z: TLMatrix): Boolean;
var
  TAU: TLVec;
  E: TLVec;
begin
  a := DynamicArrayCopy(a);
  Assert((ZNeeded = 0) or (ZNeeded = 1), 'SMatrixEVDI: incorrect ZNeeded');
  SMatrixTD(a, n, IsUpper, TAU, w, E);
  if ZNeeded = 1 then
    begin
      SMatrixTDUnpackQ(a, n, IsUpper, TAU, z);
    end;
  Result := SMatrixTDEVDI(w, E, n, ZNeeded, i1, i2, z);
end;

(* ************************************************************************
  Finding the eigenvalues and eigenvectors of a Hermitian matrix

  The algorithm finds eigen pairs of a Hermitian matrix by  reducing  it  to
  real tridiagonal form and using the QL/QR algorithm.

  Input parameters:
  A       -   Hermitian matrix which is given  by  its  upper  or  lower
  triangular part.
  Array whose indexes range within [0..N-1, 0..N-1].
  N       -   size of matrix A.
  IsUpper -   storage format.
  ZNeeded -   flag controlling whether the eigenvectors  are  needed  or
  not. If ZNeeded is equal to:
  * 0, the eigenvectors are not returned;
  * 1, the eigenvectors are returned.

  Output parameters:
  D       -   eigenvalues in ascending order.
  Array whose index ranges within [0..N-1].
  Z       -   if ZNeeded is equal to:
  * 0, Z hasn't changed;
  * 1, Z contains the eigenvectors.
  Array whose indexes range within [0..N-1, 0..N-1].
  The eigenvectors are stored in the matrix columns.

  Result:
  True, if the algorithm has converged.
  False, if the algorithm hasn't converged (rare case).

  Note:
  eigenvectors of Hermitian matrix are defined up to  multiplication  by
  a complex number L, such that |L|=1.
  ************************************************************************ *)
function HMatrixEVD(a: TLComplexMatrix; n: TLInt;
  ZNeeded: TLInt; IsUpper: Boolean; var d: TLVec;
  var z: TLComplexMatrix): Boolean;
var
  TAU: TLComplexVec;
  E: TLVec;
  Work: TLVec;
  t: TLMatrix;
  q: TLComplexMatrix;
  i: TLInt;
  k: TLInt;
  v: TLFloat;
begin
  a := DynamicArrayCopy(a);
  Assert((ZNeeded = 0) or (ZNeeded = 1), 'HermitianEVD: incorrect ZNeeded');

  //
  // Reduce to tridiagonal form
  //
  HMatrixTD(a, n, IsUpper, TAU, d, E);
  if ZNeeded = 1 then
    begin
      HMatrixTDUnpackQ(a, n, IsUpper, TAU, q);
      ZNeeded := 2;
    end;

  //
  // TDEVD
  //
  Result := SMatrixTDEVD(d, E, n, ZNeeded, t);

  //
  // Eigenvectors are needed
  // Calculate Z = Q*T = Re(Q)*T + i*Im(Q)*T
  //
  if Result and (ZNeeded <> 0) then
    begin
      SetLength(Work, n  );
      SetLength(z, n  , n  );
      i := 0;
      while i <= n - 1 do
        begin

          //
          // Calculate real part
          //
          k := 0;
          while k <= n - 1 do
            begin
              Work[k] := 0;
              inc(k);
            end;
          k := 0;
          while k <= n - 1 do
            begin
              v := q[i, k].x;
              APVAdd(@Work[0], 0, n - 1, @t[k][0], 0, n - 1, v);
              inc(k);
            end;
          k := 0;
          while k <= n - 1 do
            begin
              z[i, k].x := Work[k];
              inc(k);
            end;

          //
          // Calculate imaginary part
          //
          k := 0;
          while k <= n - 1 do
            begin
              Work[k] := 0;
              inc(k);
            end;
          k := 0;
          while k <= n - 1 do
            begin
              v := q[i, k].y;
              APVAdd(@Work[0], 0, n - 1, @t[k][0], 0, n - 1, v);
              inc(k);
            end;
          k := 0;
          while k <= n - 1 do
            begin
              z[i, k].y := Work[k];
              inc(k);
            end;
          inc(i);
        end;
    end;
end;

(* ************************************************************************
  Subroutine for finding the eigenvalues (and eigenvectors) of  a  Hermitian
  matrix  in  a  given half-interval (A, B] by using a bisection and inverse
  iteration

  Input parameters:
  A       -   Hermitian matrix which is given by its upper or lower triangular part. Array whose indexes range within [0..N-1, 0..N-1].
  N       -   size of matrix A.

 ZNeeded  -  flag controlling whether the eigenvectors are needed or not. If ZNeeded is equal to:
  * 0, the eigenvectors are not returned;
  * 1, the eigenvectors are returned.

  IsUpperA -  storage format of matrix A.
  B1, B2   -    half-interval (B1, B2] to search eigenvalues in.

  Output parameters:
  M       -   number of eigenvalues found in a given half-interval, M>=0
  W       -   array of the eigenvalues found.
  Array whose index ranges within [0..M-1].
  Z       -   if ZNeeded is equal to:
  * 0, Z hasn't changed;
  * 1, Z contains eigenvectors.
  Array whose indexes range within [0..N-1, 0..M-1].
  The eigenvectors are stored in the matrix columns.

  Result:
  True, if successful. M contains the number of eigenvalues in the given
  half-interval (could be equal to 0), W contains the eigenvalues,
  Z contains the eigenvectors (if needed).

  False, if the bisection method subroutine  wasn't  able  to  find  the
  eigenvalues  in  the  given  interval  or  if  the  inverse  iteration
  subroutine  wasn't  able  to  find all the corresponding eigenvectors.
  In that case, the eigenvalues and eigenvectors are not returned, M  is
  equal to 0.

  Note:
  eigen vectors of Hermitian matrix are defined up to multiplication by a complex number L, such as |L|=1.
  ************************************************************************ *)
function HMatrixEVDR(a: TLComplexMatrix; n: TLInt;
  ZNeeded: TLInt; IsUpper: Boolean; b1: TLFloat; b2: TLFloat;
  var M: TLInt; var w: TLVec; var z: TLComplexMatrix): Boolean;
var
  q: TLComplexMatrix;
  t: TLMatrix;
  TAU: TLComplexVec;
  E: TLVec;
  Work: TLVec;
  i: TLInt;
  k: TLInt;
  v: TLFloat;
begin
  a := DynamicArrayCopy(a);
  Assert((ZNeeded = 0) or (ZNeeded = 1),
    'HermitianEigenValuesAndVectorsInInterval: incorrect ZNeeded');

  //
  // Reduce to tridiagonal form
  //
  HMatrixTD(a, n, IsUpper, TAU, w, E);
  if ZNeeded = 1 then
    begin
      HMatrixTDUnpackQ(a, n, IsUpper, TAU, q);
      ZNeeded := 2;
    end;

  //
  // Bisection and inverse iteration
  //
  Result := SMatrixTDEVDR(w, E, n, ZNeeded, b1, b2, M, t);

  //
  // Eigenvectors are needed
  // Calculate Z = Q*T = Re(Q)*T + i*Im(Q)*T
  //
  if Result and (ZNeeded <> 0) and (M <> 0) then
    begin
      SetLength(Work, M  );
      SetLength(z, n  , M  );
      i := 0;
      while i <= n - 1 do
        begin

          //
          // Calculate real part
          //
          k := 0;
          while k <= M - 1 do
            begin
              Work[k] := 0;
              inc(k);
            end;
          k := 0;
          while k <= n - 1 do
            begin
              v := q[i, k].x;
              APVAdd(@Work[0], 0, M - 1, @t[k][0], 0, M - 1, v);
              inc(k);
            end;
          k := 0;
          while k <= M - 1 do
            begin
              z[i, k].x := Work[k];
              inc(k);
            end;

          //
          // Calculate imaginary part
          //
          k := 0;
          while k <= M - 1 do
            begin
              Work[k] := 0;
              inc(k);
            end;
          k := 0;
          while k <= n - 1 do
            begin
              v := q[i, k].y;
              APVAdd(@Work[0], 0, M - 1, @t[k][0], 0, M - 1, v);
              inc(k);
            end;
          k := 0;
          while k <= M - 1 do
            begin
              z[i, k].y := Work[k];
              inc(k);
            end;
          inc(i);
        end;
    end;
end;

(* ************************************************************************
  Subroutine for finding the eigenvalues and  eigenvectors  of  a  Hermitian
  matrix with given indexes by using bisection and inverse iteration methods

  Input parameters:
  A       -   Hermitian matrix which is given by its upper or lower triangular part. Array whose indexes range within [0..N-1, 0..N-1].
  N       -   size of matrix A.

  ZNeeded -   flag controlling whether the eigenvectors  are  needed  or
  not. If ZNeeded is equal to:
  * 0, the eigenvectors are not returned;
  * 1, the eigenvectors are returned.

  IsUpperA -  storage format of matrix A.
  I1, I2   -    index interval for searching (from I1 to I2). 0 <= I1 <= I2 <= N-1.

  Output parameters:
  W       -   array of the eigenvalues found.
  Array whose index ranges within [0..I2-I1].
  Z       -   if ZNeeded is equal to:
  * 0, Z hasn't changed;
  * 1, Z contains eigenvectors.
  Array whose indexes range within [0..N-1, 0..I2-I1].
  In that case, the eigenvectors are stored in the matrix columns.

  Result:
  True, if successful. W contains the eigenvalues, Z contains the
  eigenvectors (if needed).

  False, if the bisection method subroutine  wasn't  able  to  find  the
  eigenvalues  in  the  given  interval  or  if  the  inverse  iteration
  subroutine wasn't able to find  all  the  corresponding  eigenvectors.
  In that case, the eigenvalues and eigenvectors are not returned.

  Note:
  eigen vectors of Hermitian matrix are defined up to multiplication by a complex number L, such as |L|=1.
  ************************************************************************ *)
function HMatrixEVDI(a: TLComplexMatrix; n: TLInt;
  ZNeeded: TLInt; IsUpper: Boolean; i1: TLInt;
  i2: TLInt; var w: TLVec; var z: TLComplexMatrix): Boolean;
var
  q: TLComplexMatrix;
  t: TLMatrix;
  TAU: TLComplexVec;
  E: TLVec;
  Work: TLVec;
  i: TLInt;
  k: TLInt;
  v: TLFloat;
  M: TLInt;
begin
  a := DynamicArrayCopy(a);
  Assert((ZNeeded = 0) or (ZNeeded = 1),
    'HermitianEigenValuesAndVectorsByIndexes: incorrect ZNeeded');

  //
  // Reduce to tridiagonal form
  //
  HMatrixTD(a, n, IsUpper, TAU, w, E);
  if ZNeeded = 1 then
    begin
      HMatrixTDUnpackQ(a, n, IsUpper, TAU, q);
      ZNeeded := 2;
    end;

  //
  // Bisection and inverse iteration
  //
  Result := SMatrixTDEVDI(w, E, n, ZNeeded, i1, i2, t);

  //
  // Eigenvectors are needed
  // Calculate Z = Q*T = Re(Q)*T + i*Im(Q)*T
  //
  M := i2 - i1 + 1;
  if Result and (ZNeeded <> 0) then
    begin
      SetLength(Work, M  );
      SetLength(z, n  , M  );
      i := 0;
      while i <= n - 1 do
        begin

          //
          // Calculate real part
          //
          k := 0;
          while k <= M - 1 do
            begin
              Work[k] := 0;
              inc(k);
            end;
          k := 0;
          while k <= n - 1 do
            begin
              v := q[i, k].x;
              APVAdd(@Work[0], 0, M - 1, @t[k][0], 0, M - 1, v);
              inc(k);
            end;
          k := 0;
          while k <= M - 1 do
            begin
              z[i, k].x := Work[k];
              inc(k);
            end;

          //
          // Calculate imaginary part
          //
          k := 0;
          while k <= M - 1 do
            begin
              Work[k] := 0;
              inc(k);
            end;
          k := 0;
          while k <= n - 1 do
            begin
              v := q[i, k].y;
              APVAdd(@Work[0], 0, M - 1, @t[k][0], 0, M - 1, v);
              inc(k);
            end;
          k := 0;
          while k <= M - 1 do
            begin
              z[i, k].y := Work[k];
              inc(k);
            end;
          inc(i);
        end;
    end;
end;

(* ************************************************************************
  Finding the eigenvalues and eigenvectors of a tridiagonal symmetric matrix

  The algorithm finds the eigen pairs of a tridiagonal symmetric matrix by
  using an QL/QR algorithm with implicit shifts.

  Input parameters:
  D       -   the main diagonal of a tridiagonal matrix. Array whose index ranges within [0..N-1].
  E       -   the secondary diagonal of a tridiagonal matrix. Array whose index ranges within [0..N-2].
  N       -   size of matrix A.

  ZNeeded -   flag controlling whether the eigenvectors are needed or not.
  If ZNeeded is equal to:
  * 0, the eigenvectors are not needed;
  * 1, the eigenvectors of a tridiagonal matrix are multiplied by the square matrix Z.
  *    It is used if the tridiagonal matrix is obtained by the similarity transformation of a symmetric matrix;
  * 2, the eigenvectors of a tridiagonal matrix replace the square matrix Z;
  * 3, matrix Z contains the first row of the eigenvectors matrix.

  Z       -   if ZNeeded=1, Z contains the square matrix by which the eigenvectors are multiplied. Array whose indexes range within [0..N-1, 0..N-1].

  Output parameters:
  D       -   eigenvalues in ascending order. Array whose index ranges within [0..N-1].

  Z       -   if ZNeeded is equal to:
  * 0, Z hasn't changed;
  * 1, Z contains the product of a given matrix (from the left)
  and the eigenvectors matrix (from the right);
  * 2, Z contains the eigenvectors.
  * 3, Z contains the first row of the eigenvectors matrix.
  If ZNeeded<3, Z is the array whose indexes range within [0..N-1, 0..N-1].
  In that case, the eigenvectors are stored in the matrix columns.
  If ZNeeded=3, Z is the array whose indexes range within [0..0, 0..N-1].

  Result:
  True, if the algorithm has converged.
  False, if the algorithm hasn't converged.
  ************************************************************************ *)
function SMatrixTDEVD(var d: TLVec; E: TLVec; n: TLInt;
  ZNeeded: TLInt; var z: TLMatrix): Boolean;
var
  d1: TLVec;
  e1: TLVec;
  z1: TLMatrix;
  i: TLInt;
begin
  E := DynamicArrayCopy(E);

  //
  // Prepare 1-based task
  //
  SetLength(d1, n + 1);
  SetLength(e1, n + 1);
  APVMove(@d1[0], 1, n, @d[0], 0, n - 1);
  if n > 1 then
    begin
      APVMove(@e1[0], 1, n - 1, @E[0], 0, n - 2);
    end;
  if ZNeeded = 1 then
    begin
      SetLength(z1, n + 1, n + 1);
      i := 1;
      while i <= n do
        begin
          APVMove(@z1[i][0], 1, n, @z[i - 1][0], 0, n - 1);
          inc(i);
        end;
    end;

  //
  // Solve 1-based task
  //
  Result := TridiagonalEVD(d1, e1, n, ZNeeded, z1);
  if not Result then
    begin
      Exit;
    end;

  //
  // Convert back to 0-based result
  //
  APVMove(@d[0], 0, n - 1, @d1[0], 1, n);
  if ZNeeded <> 0 then
    begin
      if ZNeeded = 1 then
        begin
          i := 1;
          while i <= n do
            begin
              APVMove(@z[i - 1][0], 0, n - 1, @z1[i][0], 1, n);
              inc(i);
            end;
          Exit;
        end;
      if ZNeeded = 2 then
        begin
          SetLength(z, n  , n  );
          i := 1;
          while i <= n do
            begin
              APVMove(@z[i - 1][0], 0, n - 1, @z1[i][0], 1, n);
              inc(i);
            end;
          Exit;
        end;
      if ZNeeded = 3 then
        begin
          SetLength(z, 0 + 1, n  );
          APVMove(@z[0][0], 0, n - 1, @z1[1][0], 1, n);
          Exit;
        end;
      Assert(False, 'SMatrixTDEVD: Incorrect ZNeeded!');
    end;
end;

(* ************************************************************************
  Subroutine for finding the tridiagonal matrix eigenvalues/vectors in a
  given half-interval (A, B] by using bisection and inverse iteration.

  Input parameters:
  D       -   the main diagonal of a tridiagonal matrix. Array whose index ranges within [0..N-1].
  E       -   the secondary diagonal of a tridiagonal matrix. Array whose index ranges within [0..N-2].
  N       -   size of matrix, N>=0.

  ZNeeded -   flag controlling whether the eigenvectors are needed or not.
  If ZNeeded is equal to:
  * 0, the eigenvectors are not needed;
  * 1, the eigenvectors of a tridiagonal matrix are multiplied by the square matrix Z.
  *    It is used if the tridiagonal matrix is obtained by the similarity transformation of a symmetric matrix.
  * 2, the eigenvectors of a tridiagonal matrix replace matrix Z.

  A, B    -   half-interval (A, B] to search eigenvalues in.

  Z       -   if ZNeeded is equal to:
  * 0, Z isn't used and remains unchanged;
  * 1, Z contains the square matrix (array whose indexes range within [0..N-1, 0..N-1]) which reduces the given symmetric matrix to tridiagonal form;
  * 2, Z isn't used (but changed on the exit).

  Output parameters:
  D       -   array of the eigenvalues found.
  Array whose index ranges within [0..M-1].
  M       -   number of eigenvalues found in the given half-interval (M>=0).

  Z       -   if ZNeeded is equal to:
  * 0, doesn't contain any information;
  * 1, contains the product of a given NxN matrix Z (from the left) and NxM matrix of the eigenvectors found (from the right).
  *    Array whose indexes range within [0..N-1, 0..M-1].
  * 2, contains the matrix of the eigenvectors found. Array whose indexes range within [0..N-1, 0..M-1].

  Result:
  True, if successful. In that case, M contains the number of eigenvalues
  in the given half-interval (could be equal to 0), D contains the eigenvalues,
  Z contains the eigenvectors (if needed).
  It should be noted that the subroutine changes the size of arrays D and Z.

  False, if the bisection method subroutine wasn't able to find the
  eigenvalues in the given interval or if the inverse iteration subroutine
  wasn't able to find all the corresponding eigenvectors. In that case,
  the eigenvalues and eigenvectors are not returned, M is equal to 0.
  ************************************************************************ *)
function SMatrixTDEVDR(var d: TLVec; const E: TLVec;
  n: TLInt; ZNeeded: TLInt; a: TLFloat; b: TLFloat;
  var M: TLInt; var z: TLMatrix): Boolean;
var
  ErrorCode: TLInt;
  NSPLIT: TLInt;
  i: TLInt;
  j: TLInt;
  k: TLInt;
  CR: TLInt;
  IBLOCK: TLIVec;
  ISPLIT: TLIVec;
  IFAIL: TLIVec;
  d1: TLVec;
  e1: TLVec;
  w: TLVec;
  z2: TLMatrix;
  z3: TLMatrix;
  v: TLFloat;
  i_: TLInt;
  i1_: TLInt;
begin
  Assert((ZNeeded >= 0) and (ZNeeded <= 2),
    'SMatrixTDEVDR: incorrect ZNeeded!');

  //
  // Special cases
  //
  if AP_FP_Less_Eq(b, a) then
    begin
      M := 0;
      Result := True;
      Exit;
    end;
  if n <= 0 then
    begin
      M := 0;
      Result := True;
      Exit;
    end;

  //
  // Copy D,E to D1, E1
  //
  SetLength(d1, n + 1);
  APVMove(@d1[0], 1, n, @d[0], 0, n - 1);
  if n > 1 then
    begin
      SetLength(e1, n  );
      APVMove(@e1[0], 1, n - 1, @E[0], 0, n - 2);
    end;

  //
  // No eigen vectors
  //
  if ZNeeded = 0 then
    begin
      Result := InternalBisectionEigenValues(d1, e1, n, 2, 1, a, b, 0, 0, -1, w,
        M, NSPLIT, IBLOCK, ISPLIT, ErrorCode);
      if not Result or (M = 0) then
        begin
          M := 0;
          Exit;
        end;
      SetLength(d, M  );
      APVMove(@d[0], 0, M - 1, @w[0], 1, M);
      Exit;
    end;

  //
  // Eigen vectors are multiplied by Z
  //
  if ZNeeded = 1 then
    begin

      //
      // Find eigen pairs
      //
      Result := InternalBisectionEigenValues(d1, e1, n, 2, 2, a, b, 0, 0, -1, w,
        M, NSPLIT, IBLOCK, ISPLIT, ErrorCode);
      if not Result or (M = 0) then
        begin
          M := 0;
          Exit;
        end;
      InternalDSTEIN(n, d1, e1, M, w, IBLOCK, ISPLIT, z2, IFAIL, CR);
      if CR <> 0 then
        begin
          M := 0;
          Result := False;
          Exit;
        end;

      //
      // Sort eigen values and vectors
      //
      i := 1;
      while i <= M do
        begin
          k := i;
          j := i;
          while j <= M do
            begin
              if AP_FP_Less(w[j], w[k]) then
                begin
                  k := j;
                end;
              inc(j);
            end;
          v := w[i];
          w[i] := w[k];
          w[k] := v;
          j := 1;
          while j <= n do
            begin
              v := z2[j, i];
              z2[j, i] := z2[j, k];
              z2[j, k] := v;
              inc(j);
            end;
          inc(i);
        end;

      //
      // Transform Z2 and overwrite Z
      //
      SetLength(z3, M + 1, n + 1);
      i := 1;
      while i <= M do
        begin
          for i_ := 1 to n do
            begin
              z3[i, i_] := z2[i_, i];
            end;
          inc(i);
        end;
      i := 1;
      while i <= n do
        begin
          j := 1;
          while j <= M do
            begin
              v := APVDotProduct(@z[i - 1][0], 0, n - 1, @z3[j][0], 1, n);
              z2[i, j] := v;
              inc(j);
            end;
          inc(i);
        end;
      SetLength(z, n  , M  );
      i := 1;
      while i <= M do
        begin
          i1_ := (1) - (0);
          for i_ := 0 to n - 1 do
            begin
              z[i_, i - 1] := z2[i_ + i1_, i];
            end;
          inc(i);
        end;

      //
      // Store W
      //
      SetLength(d, M  );
      i := 1;
      while i <= M do
        begin
          d[i - 1] := w[i];
          inc(i);
        end;
      Exit;
    end;

  //
  // Eigen vectors are stored in Z
  //
  if ZNeeded = 2 then
    begin

      //
      // Find eigen pairs
      //
      Result := InternalBisectionEigenValues(d1, e1, n, 2, 2, a, b, 0, 0, -1, w,
        M, NSPLIT, IBLOCK, ISPLIT, ErrorCode);
      if not Result or (M = 0) then
        begin
          M := 0;
          Exit;
        end;
      InternalDSTEIN(n, d1, e1, M, w, IBLOCK, ISPLIT, z2, IFAIL, CR);
      if CR <> 0 then
        begin
          M := 0;
          Result := False;
          Exit;
        end;

      //
      // Sort eigen values and vectors
      //
      i := 1;
      while i <= M do
        begin
          k := i;
          j := i;
          while j <= M do
            begin
              if AP_FP_Less(w[j], w[k]) then
                begin
                  k := j;
                end;
              inc(j);
            end;
          v := w[i];
          w[i] := w[k];
          w[k] := v;
          j := 1;
          while j <= n do
            begin
              v := z2[j, i];
              z2[j, i] := z2[j, k];
              z2[j, k] := v;
              inc(j);
            end;
          inc(i);
        end;

      //
      // Store W
      //
      SetLength(d, M  );
      i := 1;
      while i <= M do
        begin
          d[i - 1] := w[i];
          inc(i);
        end;
      SetLength(z, n  , M  );
      i := 1;
      while i <= M do
        begin
          i1_ := (1) - (0);
          for i_ := 0 to n - 1 do
            begin
              z[i_, i - 1] := z2[i_ + i1_, i];
            end;
          inc(i);
        end;
      Exit;
    end;
  Result := False;
end;

(* ************************************************************************
  Subroutine for finding tridiagonal matrix eigenvalues/vectors with given
  indexes (in ascending order) by using the bisection and inverse iteraion.

  Input parameters:
  D       -   the main diagonal of a tridiagonal matrix. Array whose index ranges within [0..N-1].
  E       -   the secondary diagonal of a tridiagonal matrix. Array whose index ranges within [0..N-2].
  N       -   size of matrix. N>=0.

  ZNeeded -   flag controlling whether the eigenvectors are needed or not.
  If ZNeeded is equal to:
  * 0, the eigenvectors are not needed;
  * 1, the eigenvectors of a tridiagonal matrix are multiplied by the square matrix Z.
  *    It is used if the tridiagonal matrix is obtained by the similarity transformation of a symmetric matrix.
  * 2, the eigenvectors of a tridiagonal matrix replace matrix Z.

  I1, I2  -   index interval for searching (from I1 to I2). 0 <= I1 <= I2 <= N-1.

  Z       -   if ZNeeded is equal to:
  * 0, Z isn't used and remains unchanged;
  * 1, Z contains the square matrix (array whose indexes range within [0..N-1, 0..N-1]) which reduces the given symmetric matrix to  tridiagonal form;
  * 2, Z isn't used (but changed on the exit).

  Output parameters:
  D       -   array of the eigenvalues found. Array whose index ranges within [0..I2-I1].
  Z       -   if ZNeeded is equal to:
  * 0, doesn't contain any information;
  * 1, contains the product of a given NxN matrix Z (from the left) and Nx(I2-I1) matrix of the eigenvectors found (from the right).
  *    Array whose indexes range within [0..N-1, 0..I2-I1].
  * 2, contains the matrix of the eigenvalues found. Array whose indexes range within [0..N-1, 0..I2-I1].


  Result:
  True, if successful. In that case, D contains the eigenvalues,
  Z contains the eigenvectors (if needed).
  It should be noted that the subroutine changes the size of arrays D and Z.

  False, if the bisection method subroutine wasn't able to find the eigenvalues
  in the given interval or if the inverse iteration subroutine wasn't able
  to find all the corresponding eigenvectors. In that case, the eigenvalues
  and eigenvectors are not returned.
  ************************************************************************ *)
function SMatrixTDEVDI(var d: TLVec; const E: TLVec;
  n: TLInt; ZNeeded: TLInt; i1: TLInt;
  i2: TLInt; var z: TLMatrix): Boolean;
var
  ErrorCode: TLInt;
  NSPLIT: TLInt;
  i: TLInt;
  j: TLInt;
  k: TLInt;
  M: TLInt;
  CR: TLInt;
  IBLOCK: TLIVec;
  ISPLIT: TLIVec;
  IFAIL: TLIVec;
  w: TLVec;
  d1: TLVec;
  e1: TLVec;
  z2: TLMatrix;
  z3: TLMatrix;
  v: TLFloat;
  i_: TLInt;
  i1_: TLInt;
begin
  Assert((0 <= i1) and (i1 <= i2) and (i2 < n),
    'SMatrixTDEVDI: incorrect I1/I2!');

  //
  // Copy D,E to D1, E1
  //
  SetLength(d1, n + 1);
  APVMove(@d1[0], 1, n, @d[0], 0, n - 1);
  if n > 1 then
    begin
      SetLength(e1, n  );
      APVMove(@e1[0], 1, n - 1, @E[0], 0, n - 2);
    end;

  //
  // No eigen vectors
  //
  if ZNeeded = 0 then
    begin
      Result := InternalBisectionEigenValues(d1, e1, n, 3, 1, 0, 0, i1 + 1,
        i2 + 1, -1, w, M, NSPLIT, IBLOCK, ISPLIT, ErrorCode);
      if not Result then
        begin
          Exit;
        end;
      if M <> i2 - i1 + 1 then
        begin
          Result := False;
          Exit;
        end;
      SetLength(d, M  );
      i := 1;
      while i <= M do
        begin
          d[i - 1] := w[i];
          inc(i);
        end;
      Exit;
    end;

  //
  // Eigen vectors are multiplied by Z
  //
  if ZNeeded = 1 then
    begin

      //
      // Find eigen pairs
      //
      Result := InternalBisectionEigenValues(d1, e1, n, 3, 2, 0, 0, i1 + 1,
        i2 + 1, -1, w, M, NSPLIT, IBLOCK, ISPLIT, ErrorCode);
      if not Result then
        begin
          Exit;
        end;
      if M <> i2 - i1 + 1 then
        begin
          Result := False;
          Exit;
        end;
      InternalDSTEIN(n, d1, e1, M, w, IBLOCK, ISPLIT, z2, IFAIL, CR);
      if CR <> 0 then
        begin
          Result := False;
          Exit;
        end;

      //
      // Sort eigen values and vectors
      //
      i := 1;
      while i <= M do
        begin
          k := i;
          j := i;
          while j <= M do
            begin
              if AP_FP_Less(w[j], w[k]) then
                begin
                  k := j;
                end;
              inc(j);
            end;
          v := w[i];
          w[i] := w[k];
          w[k] := v;
          j := 1;
          while j <= n do
            begin
              v := z2[j, i];
              z2[j, i] := z2[j, k];
              z2[j, k] := v;
              inc(j);
            end;
          inc(i);
        end;

      //
      // Transform Z2 and overwrite Z
      //
      SetLength(z3, M + 1, n + 1);
      i := 1;
      while i <= M do
        begin
          for i_ := 1 to n do
            begin
              z3[i, i_] := z2[i_, i];
            end;
          inc(i);
        end;
      i := 1;
      while i <= n do
        begin
          j := 1;
          while j <= M do
            begin
              v := APVDotProduct(@z[i - 1][0], 0, n - 1, @z3[j][0], 1, n);
              z2[i, j] := v;
              inc(j);
            end;
          inc(i);
        end;
      SetLength(z, n  , M  );
      i := 1;
      while i <= M do
        begin
          i1_ := (1) - (0);
          for i_ := 0 to n - 1 do
            begin
              z[i_, i - 1] := z2[i_ + i1_, i];
            end;
          inc(i);
        end;

      //
      // Store W
      //
      SetLength(d, M  );
      i := 1;
      while i <= M do
        begin
          d[i - 1] := w[i];
          inc(i);
        end;
      Exit;
    end;

  //
  // Eigen vectors are stored in Z
  //
  if ZNeeded = 2 then
    begin

      //
      // Find eigen pairs
      //
      Result := InternalBisectionEigenValues(d1, e1, n, 3, 2, 0, 0, i1 + 1,
        i2 + 1, -1, w, M, NSPLIT, IBLOCK, ISPLIT, ErrorCode);
      if not Result then
        begin
          Exit;
        end;
      if M <> i2 - i1 + 1 then
        begin
          Result := False;
          Exit;
        end;
      InternalDSTEIN(n, d1, e1, M, w, IBLOCK, ISPLIT, z2, IFAIL, CR);
      if CR <> 0 then
        begin
          Result := False;
          Exit;
        end;

      //
      // Sort eigen values and vectors
      //
      i := 1;
      while i <= M do
        begin
          k := i;
          j := i;
          while j <= M do
            begin
              if AP_FP_Less(w[j], w[k]) then
                begin
                  k := j;
                end;
              inc(j);
            end;
          v := w[i];
          w[i] := w[k];
          w[k] := v;
          j := 1;
          while j <= n do
            begin
              v := z2[j, i];
              z2[j, i] := z2[j, k];
              z2[j, k] := v;
              inc(j);
            end;
          inc(i);
        end;

      //
      // Store Z
      //
      SetLength(z, n  , M  );
      i := 1;
      while i <= M do
        begin
          i1_ := (1) - (0);
          for i_ := 0 to n - 1 do
            begin
              z[i_, i - 1] := z2[i_ + i1_, i];
            end;
          inc(i);
        end;

      //
      // Store W
      //
      SetLength(d, M  );
      i := 1;
      while i <= M do
        begin
          d[i - 1] := w[i];
          inc(i);
        end;
      Exit;
    end;
  Result := False;
end;

(* ************************************************************************
  Finding eigenvalues and eigenvectors of a general matrix

  The algorithm finds eigenvalues and eigenvectors of a general matrix by
  using the QR algorithm with multiple shifts. The algorithm can find
  eigenvalues and both left and right eigenvectors.

  The right eigenvector is a vector x such that A*x = w*x, and the left
  eigenvector is a vector y such that y'*A = w*y' (here y' implies a complex
  conjugate transposition of vector y).

  Input parameters:
  A       -   matrix. Array whose indexes range within [0..N-1, 0..N-1].
  N       -   size of matrix A.
  VNeeded -   flag controlling whether eigenvectors are needed or not.
  If VNeeded is equal to:
  * 0, eigenvectors are not returned;
  * 1, right eigenvectors are returned;
  * 2, left eigenvectors are returned;
  * 3, both left and right eigenvectors are returned.

  Output parameters:
  WR      -   real parts of eigenvalues. Array whose index ranges within [0..N-1].
  WR      -   imaginary parts of eigenvalues. Array whose index ranges within [0..N-1].

  VL, VR  -   arrays of left and right eigenvectors (if they are needed).
  If WI[i]=0, the respective eigenvalue is a real number,
  and it corresponds to the column number I of matrices VL/VR.
  If WI[i]>0, we have a pair of complex conjugate numbers with
  positive and negative imaginary parts:
  the first eigenvalue WR[i] + sqrt(-1)*WI[i];
  the second eigenvalue WR[i+1] + sqrt(-1)*WI[i+1];
  WI[i]>0
  WI[i+1] = -WI[i] < 0
  In that case, the eigenvector  corresponding to the first
  eigenvalue is located in i and i+1 columns of matrices
  VL/VR (the column number i contains the real part, and the
  column number i+1 contains the imaginary part), and the vector
  corresponding to the second eigenvalue is a complex conjugate to
  the first vector.
  Arrays whose indexes range within [0..N-1, 0..N-1].

  Result:
  True, if the algorithm has converged.
  False, if the algorithm has not converged.

  Note 1:
  Some users may ask the following question: what if WI[N-1]>0?
  WI[N] must contain an eigenvalue which is complex conjugate to the
  N-th eigenvalue, but the array has only size N?
  The answer is as follows: such a situation cannot occur because the
  algorithm finds a pairs of eigenvalues, therefore, if WI[i]>0, I is
  strictly less than N-1.

  Note 2:
  The algorithm performance depends on the value of the internal parameter
  NS of the InternalSchurDecomposition subroutine which defines the number
  of shifts in the QR algorithm (similarly to the block width in block-matrix
  algorithms of linear algebra). If you require maximum performance
  on your machine, it is recommended to adjust this parameter manually.

  See also the InternalTREVC subroutine.
  ************************************************************************ *)
function RMatrixEVD(a: TLMatrix; n: TLInt; VNeeded: TLInt;
  var WR: TLVec; var WI: TLVec; var vl: TLMatrix;
  var vr: TLMatrix): Boolean;
var
  a1: TLMatrix;
  VL1: TLMatrix;
  VR1: TLMatrix;
  WR1: TLVec;
  WI1: TLVec;
  i: TLInt;
begin
  a := DynamicArrayCopy(a);
  Assert((VNeeded >= 0) and (VNeeded <= 3), 'RMatrixEVD: incorrect VNeeded!');
  SetLength(a1, n + 1, n + 1);
  i := 1;
  while i <= n do
    begin
      APVMove(@a1[i][0], 1, n, @a[i - 1][0], 0, n - 1);
      inc(i);
    end;
  Result := NonSymmetricEVD(a1, n, VNeeded, WR1, WI1, VL1, VR1);
  if Result then
    begin
      SetLength(WR, n  );
      SetLength(WI, n  );
      APVMove(@WR[0], 0, n - 1, @WR1[0], 1, n);
      APVMove(@WI[0], 0, n - 1, @WI1[0], 1, n);
      if (VNeeded = 2) or (VNeeded = 3) then
        begin
          SetLength(vl, n  , n  );
          i := 0;
          while i <= n - 1 do
            begin
              APVMove(@vl[i][0], 0, n - 1, @VL1[i + 1][0], 1, n);
              inc(i);
            end;
        end;
      if (VNeeded = 1) or (VNeeded = 3) then
        begin
          SetLength(vr, n  , n  );
          i := 0;
          while i <= n - 1 do
            begin
              APVMove(@vr[i][0], 0, n - 1, @VR1[i + 1][0], 1, n);
              inc(i);
            end;
        end;
    end;
end;

function InternalBisectionEigenValues(d: TLVec; E: TLVec;
  n: TLInt; IRANGE: TLInt; IORDER: TLInt;
  vl: TLFloat; VU: TLFloat; IL: TLInt; IU: TLInt;
  ABSTOL: TLFloat; var w: TLVec; var M: TLInt;
  var NSPLIT: TLInt; var IBLOCK: TLIVec;
  var ISPLIT: TLIVec; var ErrorCode: TLInt): Boolean;
var
  FUDGE: TLFloat;
  RELFAC: TLFloat;
  NCNVRG: Boolean;
  TOOFEW: Boolean;
  IB: TLInt;
  IBEGIN: TLInt;
  IDISCL: TLInt;
  IDISCU: TLInt;
  IE: TLInt;
  IEND: TLInt;
  IINFO: TLInt;
  IM: TLInt;
  IIN: TLInt;
  IOFF: TLInt;
  IOUT: TLInt;
  ITMAX: TLInt;
  IW: TLInt;
  IWOFF: TLInt;
  j: TLInt;
  ITMP1: TLInt;
  JB: TLInt;
  JDISC: TLInt;
  JE: TLInt;
  NWL: TLInt;
  NWU: TLInt;
  ATOLI: TLFloat;
  BNORM: TLFloat;
  GL: TLFloat;
  GU: TLFloat;
  PIVMIN: TLFloat;
  RTOLI: TLFloat;
  SAFEMN: TLFloat;
  TMP1: TLFloat;
  TMP2: TLFloat;
  TNORM: TLFloat;
  ULP: TLFloat;
  WKILL: TLFloat;
  WL: TLFloat;
  WLU: TLFloat;
  WU: TLFloat;
  WUL: TLFloat;
  ScaleFactor: TLFloat;
  t: TLFloat;
  IDUMMA: TLIVec;
  Work: TLVec;
  IWORK: TLIVec;
  IA1S2: TLIVec;
  RA1S2: TLVec;
  RA1S2X2: TLMatrix;
  IA1S2X2: TLIMatrix;
  RA1SIIN: TLVec;
  RA2SIIN: TLVec;
  RA3SIIN: TLVec;
  RA4SIIN: TLVec;
  RA1SIINX2: TLMatrix;
  IA1SIINX2: TLIMatrix;
  IWORKSPACE: TLIVec;
  RWORKSPACE: TLVec;
  TmpI: TLInt;
begin
  d := DynamicArrayCopy(d);
  E := DynamicArrayCopy(E);

  //
  // Quick return if possible
  //
  M := 0;
  if n = 0 then
    begin
      Result := True;
      Exit;
    end;

  //
  // Get machine constants
  // NB is the minimum vector length for vector bisection, or 0
  // if only scalar is to be done.
  //
  FUDGE := 2;
  RELFAC := 2;
  SAFEMN := MinRealNumber;
  ULP := 2 * MachineEpsilon;
  RTOLI := ULP * RELFAC;
  SetLength(IDUMMA, 1 + 1);
  SetLength(Work, 4 * n + 1);
  SetLength(IWORK, 3 * n + 1);
  SetLength(w, n + 1);
  SetLength(IBLOCK, n + 1);
  SetLength(ISPLIT, n + 1);
  SetLength(IA1S2, 2 + 1);
  SetLength(RA1S2, 2 + 1);
  SetLength(RA1S2X2, 2 + 1, 2 + 1);
  SetLength(IA1S2X2, 2 + 1, 2 + 1);
  SetLength(RA1SIIN, n + 1);
  SetLength(RA2SIIN, n + 1);
  SetLength(RA3SIIN, n + 1);
  SetLength(RA4SIIN, n + 1);
  SetLength(RA1SIINX2, n + 1, 2 + 1);
  SetLength(IA1SIINX2, n + 1, 2 + 1);
  SetLength(IWORKSPACE, n + 1);
  SetLength(RWORKSPACE, n + 1);

  //
  // Check for Errors
  //
  Result := False;
  ErrorCode := 0;
  if (IRANGE <= 0) or (IRANGE >= 4) then
    begin
      ErrorCode := -4;
    end;
  if (IORDER <= 0) or (IORDER >= 3) then
    begin
      ErrorCode := -5;
    end;
  if n < 0 then
    begin
      ErrorCode := -3;
    end;
  if (IRANGE = 2) and AP_FP_Greater_Eq(vl, VU) then
    begin
      ErrorCode := -6;
    end;
  if (IRANGE = 3) and ((IL < 1) or (IL > Max(1, n))) then
    begin
      ErrorCode := -8;
    end;
  if (IRANGE = 3) and ((IU < Min(n, IL)) or (IU > n)) then
    begin
      ErrorCode := -9;
    end;
  if ErrorCode <> 0 then
    begin
      Exit;
    end;

  //
  // Initialize error flags
  //
  NCNVRG := False;
  TOOFEW := False;

  //
  // Simplifications:
  //
  if (IRANGE = 3) and (IL = 1) and (IU = n) then
    begin
      IRANGE := 1;
    end;

  //
  // Special Case when N=1
  //
  if n = 1 then
    begin
      NSPLIT := 1;
      ISPLIT[1] := 1;
      if (IRANGE = 2) and (AP_FP_Greater_Eq(vl, d[1]) or AP_FP_Less(VU, d[1]))
      then
        begin
          M := 0;
        end
      else
        begin
          w[1] := d[1];
          IBLOCK[1] := 1;
          M := 1;
        end;
      Result := True;
      Exit;
    end;

  //
  // Scaling
  //
  t := AbsReal(d[n]);
  j := 1;
  while j <= n - 1 do
    begin
      t := Max(t, AbsReal(d[j]));
      t := Max(t, AbsReal(E[j]));
      inc(j);
    end;
  ScaleFactor := 1;
  if AP_FP_NEq(t, 0) then
    begin
      if AP_FP_Greater(t, Sqrt(Sqrt(MinRealNumber)) * Sqrt(MaxRealNumber)) then
        begin
          ScaleFactor := t;
        end;
      if AP_FP_Less(t, Sqrt(Sqrt(MaxRealNumber)) * Sqrt(MinRealNumber)) then
        begin
          ScaleFactor := t;
        end;
      j := 1;
      while j <= n - 1 do
        begin
          d[j] := d[j] / ScaleFactor;
          E[j] := E[j] / ScaleFactor;
          inc(j);
        end;
      d[n] := d[n] / ScaleFactor;
    end;

  //
  // Compute Splitting Points
  //
  NSPLIT := 1;
  Work[n] := 0;
  PIVMIN := 1;
  j := 2;
  while j <= n do
    begin
      TMP1 := AP_Sqr(E[j - 1]);
      if AP_FP_Greater(AbsReal(d[j] * d[j - 1]) * AP_Sqr(ULP) + SAFEMN, TMP1) then
        begin
          ISPLIT[NSPLIT] := j - 1;
          NSPLIT := NSPLIT + 1;
          Work[j - 1] := 0;
        end
      else
        begin
          Work[j - 1] := TMP1;
          PIVMIN := Max(PIVMIN, TMP1);
        end;
      inc(j);
    end;
  ISPLIT[NSPLIT] := n;
  PIVMIN := PIVMIN * SAFEMN;

  //
  // Compute Interval and ATOLI
  //
  if IRANGE = 3 then
    begin

      //
      // RANGE='I': Compute the interval containing eigenvalues
      // IL through IU.
      //
      // Compute Gershgorin interval for entire (split) matrix
      // and use it as the initial interval
      //
      GU := d[1];
      GL := d[1];
      TMP1 := 0;
      j := 1;
      while j <= n - 1 do
        begin
          TMP2 := Sqrt(Work[j]);
          GU := Max(GU, d[j] + TMP1 + TMP2);
          GL := Min(GL, d[j] - TMP1 - TMP2);
          TMP1 := TMP2;
          inc(j);
        end;
      GU := Max(GU, d[n] + TMP1);
      GL := Min(GL, d[n] - TMP1);
      TNORM := Max(AbsReal(GL), AbsReal(GU));
      GL := GL - FUDGE * TNORM * ULP * n - FUDGE * 2 * PIVMIN;
      GU := GU + FUDGE * TNORM * ULP * n + FUDGE * PIVMIN;

      //
      // Compute Iteration parameters
      //
      ITMAX := Ceil((ln(TNORM + PIVMIN) - ln(PIVMIN)) / ln(2)) + 2;
      if AP_FP_Less_Eq(ABSTOL, 0) then
        begin
          ATOLI := ULP * TNORM;
        end
      else
        begin
          ATOLI := ABSTOL;
        end;
      Work[n + 1] := GL;
      Work[n + 2] := GL;
      Work[n + 3] := GU;
      Work[n + 4] := GU;
      Work[n + 5] := GL;
      Work[n + 6] := GU;
      IWORK[1] := -1;
      IWORK[2] := -1;
      IWORK[3] := n + 1;
      IWORK[4] := n + 1;
      IWORK[5] := IL - 1;
      IWORK[6] := IU;

      //
      // Calling DLAEBZ
      //
      // DLAEBZ( 3, ITMAX, N, 2, 2, NB, ATOLI, RTOLI, PIVMIN, D, E,
      // WORK, IWORK( 5 ), WORK( N+1 ), WORK( N+5 ), IOUT,
      // IWORK, W, IBLOCK, IINFO )
      //
      IA1S2[1] := IWORK[5];
      IA1S2[2] := IWORK[6];
      RA1S2[1] := Work[n + 5];
      RA1S2[2] := Work[n + 6];
      RA1S2X2[1, 1] := Work[n + 1];
      RA1S2X2[2, 1] := Work[n + 2];
      RA1S2X2[1, 2] := Work[n + 3];
      RA1S2X2[2, 2] := Work[n + 4];
      IA1S2X2[1, 1] := IWORK[1];
      IA1S2X2[2, 1] := IWORK[2];
      IA1S2X2[1, 2] := IWORK[3];
      IA1S2X2[2, 2] := IWORK[4];
      InternalDLAEBZ(3, ITMAX, n, 2, 2, ATOLI, RTOLI, PIVMIN, d, E, Work, IA1S2,
        RA1S2X2, RA1S2, IOUT, IA1S2X2, w, IBLOCK, IINFO);
      IWORK[5] := IA1S2[1];
      IWORK[6] := IA1S2[2];
      Work[n + 5] := RA1S2[1];
      Work[n + 6] := RA1S2[2];
      Work[n + 1] := RA1S2X2[1, 1];
      Work[n + 2] := RA1S2X2[2, 1];
      Work[n + 3] := RA1S2X2[1, 2];
      Work[n + 4] := RA1S2X2[2, 2];
      IWORK[1] := IA1S2X2[1, 1];
      IWORK[2] := IA1S2X2[2, 1];
      IWORK[3] := IA1S2X2[1, 2];
      IWORK[4] := IA1S2X2[2, 2];
      if IWORK[6] = IU then
        begin
          WL := Work[n + 1];
          WLU := Work[n + 3];
          NWL := IWORK[1];
          WU := Work[n + 4];
          WUL := Work[n + 2];
          NWU := IWORK[4];
        end
      else
        begin
          WL := Work[n + 2];
          WLU := Work[n + 4];
          NWL := IWORK[2];
          WU := Work[n + 3];
          WUL := Work[n + 1];
          NWU := IWORK[3];
        end;
      if (NWL < 0) or (NWL >= n) or (NWU < 1) or (NWU > n) then
        begin
          ErrorCode := 4;
          Result := False;
          Exit;
        end;
    end
  else
    begin

      //
      // RANGE='A' or 'V' -- Set ATOLI
      //
      TNORM := Max(AbsReal(d[1]) + AbsReal(E[1]),
        AbsReal(d[n]) + AbsReal(E[n - 1]));
      j := 2;
      while j <= n - 1 do
        begin
          TNORM := Max(TNORM, AbsReal(d[j]) + AbsReal(E[j - 1]) + AbsReal(E[j]));
          inc(j);
        end;
      if AP_FP_Less_Eq(ABSTOL, 0) then
        begin
          ATOLI := ULP * TNORM;
        end
      else
        begin
          ATOLI := ABSTOL;
        end;
      if IRANGE = 2 then
        begin
          WL := vl;
          WU := VU;
        end
      else
        begin
          WL := 0;
          WU := 0;
        end;
    end;

  //
  // Find Eigenvalues -- Loop Over Blocks and recompute NWL and NWU.
  // NWL accumulates the number of eigenvalues .le. WL,
  // NWU accumulates the number of eigenvalues .le. WU
  //
  M := 0;
  IEND := 0;
  ErrorCode := 0;
  NWL := 0;
  NWU := 0;
  JB := 1;
  while JB <= NSPLIT do
    begin
      IOFF := IEND;
      IBEGIN := IOFF + 1;
      IEND := ISPLIT[JB];
      IIN := IEND - IOFF;
      if IIN = 1 then
        begin

          //
          // Special Case -- IIN=1
          //
          if (IRANGE = 1) or AP_FP_Greater_Eq(WL, d[IBEGIN] - PIVMIN) then
            begin
              NWL := NWL + 1;
            end;
          if (IRANGE = 1) or AP_FP_Greater_Eq(WU, d[IBEGIN] - PIVMIN) then
            begin
              NWU := NWU + 1;
            end;
          if (IRANGE = 1) or AP_FP_Less(WL, d[IBEGIN] - PIVMIN) and
            AP_FP_Greater_Eq(WU, d[IBEGIN] - PIVMIN) then
            begin
              M := M + 1;
              w[M] := d[IBEGIN];
              IBLOCK[M] := JB;
            end;
        end
      else
        begin

          //
          // General Case -- IIN > 1
          //
          // Compute Gershgorin Interval
          // and use it as the initial interval
          //
          GU := d[IBEGIN];
          GL := d[IBEGIN];
          TMP1 := 0;
          j := IBEGIN;
          while j <= IEND - 1 do
            begin
              TMP2 := AbsReal(E[j]);
              GU := Max(GU, d[j] + TMP1 + TMP2);
              GL := Min(GL, d[j] - TMP1 - TMP2);
              TMP1 := TMP2;
              inc(j);
            end;
          GU := Max(GU, d[IEND] + TMP1);
          GL := Min(GL, d[IEND] - TMP1);
          BNORM := Max(AbsReal(GL), AbsReal(GU));
          GL := GL - FUDGE * BNORM * ULP * IIN - FUDGE * PIVMIN;
          GU := GU + FUDGE * BNORM * ULP * IIN + FUDGE * PIVMIN;

          //
          // Compute ATOLI for the current submatrix
          //
          if AP_FP_Less_Eq(ABSTOL, 0) then
            begin
              ATOLI := ULP * Max(AbsReal(GL), AbsReal(GU));
            end
          else
            begin
              ATOLI := ABSTOL;
            end;
          if IRANGE > 1 then
            begin
              if AP_FP_Less(GU, WL) then
                begin
                  NWL := NWL + IIN;
                  NWU := NWU + IIN;
                  inc(JB);
                  Continue;
                end;
              GL := Max(GL, WL);
              GU := Min(GU, WU);
              if AP_FP_Greater_Eq(GL, GU) then
                begin
                  inc(JB);
                  Continue;
                end;
            end;

          //
          // Set Up Initial Interval
          //
          Work[n + 1] := GL;
          Work[n + IIN + 1] := GU;

          //
          // Calling DLAEBZ
          //
          // CALL DLAEBZ( 1, 0, IN, IN, 1, NB, ATOLI, RTOLI, PIVMIN,
          // D( IBEGIN ), E( IBEGIN ), WORK( IBEGIN ),
          // IDUMMA, WORK( N+1 ), WORK( N+2*IN+1 ), IM,
          // IWORK, W( M+1 ), IBLOCK( M+1 ), IINFO )
          //
          TmpI := 1;
          while TmpI <= IIN do
            begin
              RA1SIIN[TmpI] := d[IBEGIN - 1 + TmpI];
              if IBEGIN - 1 + TmpI < n then
                begin
                  RA2SIIN[TmpI] := E[IBEGIN - 1 + TmpI];
                end;
              RA3SIIN[TmpI] := Work[IBEGIN - 1 + TmpI];
              RA1SIINX2[TmpI, 1] := Work[n + TmpI];
              RA1SIINX2[TmpI, 2] := Work[n + TmpI + IIN];
              RA4SIIN[TmpI] := Work[n + 2 * IIN + TmpI];
              RWORKSPACE[TmpI] := w[M + TmpI];
              IWORKSPACE[TmpI] := IBLOCK[M + TmpI];
              IA1SIINX2[TmpI, 1] := IWORK[TmpI];
              IA1SIINX2[TmpI, 2] := IWORK[TmpI + IIN];
              inc(TmpI);
            end;
          InternalDLAEBZ(1, 0, IIN, IIN, 1, ATOLI, RTOLI, PIVMIN, RA1SIIN, RA2SIIN,
            RA3SIIN, IDUMMA, RA1SIINX2, RA4SIIN, IM, IA1SIINX2, RWORKSPACE,
            IWORKSPACE, IINFO);
          TmpI := 1;
          while TmpI <= IIN do
            begin
              Work[n + TmpI] := RA1SIINX2[TmpI, 1];
              Work[n + TmpI + IIN] := RA1SIINX2[TmpI, 2];
              Work[n + 2 * IIN + TmpI] := RA4SIIN[TmpI];
              w[M + TmpI] := RWORKSPACE[TmpI];
              IBLOCK[M + TmpI] := IWORKSPACE[TmpI];
              IWORK[TmpI] := IA1SIINX2[TmpI, 1];
              IWORK[TmpI + IIN] := IA1SIINX2[TmpI, 2];
              inc(TmpI);
            end;
          NWL := NWL + IWORK[1];
          NWU := NWU + IWORK[IIN + 1];
          IWOFF := M - IWORK[1];

          //
          // Compute Eigenvalues
          //
          ITMAX := Ceil((ln(GU - GL + PIVMIN) - ln(PIVMIN)) / ln(2)) + 2;

          //
          // Calling DLAEBZ
          //
          // CALL DLAEBZ( 2, ITMAX, IN, IN, 1, NB, ATOLI, RTOLI, PIVMIN,
          // D( IBEGIN ), E( IBEGIN ), WORK( IBEGIN ),
          // IDUMMA, WORK( N+1 ), WORK( N+2*IN+1 ), IOUT,
          // IWORK, W( M+1 ), IBLOCK( M+1 ), IINFO )
          //
          TmpI := 1;
          while TmpI <= IIN do
            begin
              RA1SIIN[TmpI] := d[IBEGIN - 1 + TmpI];
              if IBEGIN - 1 + TmpI < n then
                begin
                  RA2SIIN[TmpI] := E[IBEGIN - 1 + TmpI];
                end;
              RA3SIIN[TmpI] := Work[IBEGIN - 1 + TmpI];
              RA1SIINX2[TmpI, 1] := Work[n + TmpI];
              RA1SIINX2[TmpI, 2] := Work[n + TmpI + IIN];
              RA4SIIN[TmpI] := Work[n + 2 * IIN + TmpI];
              RWORKSPACE[TmpI] := w[M + TmpI];
              IWORKSPACE[TmpI] := IBLOCK[M + TmpI];
              IA1SIINX2[TmpI, 1] := IWORK[TmpI];
              IA1SIINX2[TmpI, 2] := IWORK[TmpI + IIN];
              inc(TmpI);
            end;
          InternalDLAEBZ(2, ITMAX, IIN, IIN, 1, ATOLI, RTOLI, PIVMIN, RA1SIIN,
            RA2SIIN, RA3SIIN, IDUMMA, RA1SIINX2, RA4SIIN, IOUT, IA1SIINX2,
            RWORKSPACE, IWORKSPACE, IINFO);
          TmpI := 1;
          while TmpI <= IIN do
            begin
              Work[n + TmpI] := RA1SIINX2[TmpI, 1];
              Work[n + TmpI + IIN] := RA1SIINX2[TmpI, 2];
              Work[n + 2 * IIN + TmpI] := RA4SIIN[TmpI];
              w[M + TmpI] := RWORKSPACE[TmpI];
              IBLOCK[M + TmpI] := IWORKSPACE[TmpI];
              IWORK[TmpI] := IA1SIINX2[TmpI, 1];
              IWORK[TmpI + IIN] := IA1SIINX2[TmpI, 2];
              inc(TmpI);
            end;

          //
          // Copy Eigenvalues Into W and IBLOCK
          // Use -JB for block number for unconverged eigenvalues.
          //
          j := 1;
          while j <= IOUT do
            begin
              TMP1 := 0.5 * (Work[j + n] + Work[j + IIN + n]);

              //
              // Flag non-convergence.
              //
              if j > IOUT - IINFO then
                begin
                  NCNVRG := True;
                  IB := -JB;
                end
              else
                begin
                  IB := JB;
                end;
              JE := IWORK[j] + 1 + IWOFF;
              while JE <= IWORK[j + IIN] + IWOFF do
                begin
                  w[JE] := TMP1;
                  IBLOCK[JE] := IB;
                  inc(JE);
                end;
              inc(j);
            end;
          M := M + IM;
        end;
      inc(JB);
    end;

  //
  // If RANGE='I', then (WL,WU) contains eigenvalues NWL+1,...,NWU
  // If NWL+1 < IL or NWU > IU, discard extra eigenvalues.
  //
  if IRANGE = 3 then
    begin
      IM := 0;
      IDISCL := IL - 1 - NWL;
      IDISCU := NWU - IU;
      if (IDISCL > 0) or (IDISCU > 0) then
        begin
          JE := 1;
          while JE <= M do
            begin
              if AP_FP_Less_Eq(w[JE], WLU) and (IDISCL > 0) then
                begin
                  IDISCL := IDISCL - 1;
                end
              else
                begin
                  if AP_FP_Greater_Eq(w[JE], WUL) and (IDISCU > 0) then
                    begin
                      IDISCU := IDISCU - 1;
                    end
                  else
                    begin
                      IM := IM + 1;
                      w[IM] := w[JE];
                      IBLOCK[IM] := IBLOCK[JE];
                    end;
                end;
              inc(JE);
            end;
          M := IM;
        end;
      if (IDISCL > 0) or (IDISCU > 0) then
        begin

          //
          // Code to deal with effects of bad arithmetic:
          // Some low eigenvalues to be discarded are not in (WL,WLU],
          // or high eigenvalues to be discarded are not in (WUL,WU]
          // so just kill off the smallest IDISCL/largest IDISCU
          // eigenvalues, by simply finding the smallest/largest
          // eigenvalue(s).
          //
          // (If N(w) is monotone non-decreasing, this should never
          // happen.)
          //
          if IDISCL > 0 then
            begin
              WKILL := WU;
              JDISC := 1;
              while JDISC <= IDISCL do
                begin
                  IW := 0;
                  JE := 1;
                  while JE <= M do
                    begin
                      if (IBLOCK[JE] <> 0) and (AP_FP_Less(w[JE], WKILL) or (IW = 0)) then
                        begin
                          IW := JE;
                          WKILL := w[JE];
                        end;
                      inc(JE);
                    end;
                  IBLOCK[IW] := 0;
                  inc(JDISC);
                end;
            end;
          if IDISCU > 0 then
            begin
              WKILL := WL;
              JDISC := 1;
              while JDISC <= IDISCU do
                begin
                  IW := 0;
                  JE := 1;
                  while JE <= M do
                    begin
                      if (IBLOCK[JE] <> 0) and (AP_FP_Greater(w[JE], WKILL) or (IW = 0))
                      then
                        begin
                          IW := JE;
                          WKILL := w[JE];
                        end;
                      inc(JE);
                    end;
                  IBLOCK[IW] := 0;
                  inc(JDISC);
                end;
            end;
          IM := 0;
          JE := 1;
          while JE <= M do
            begin
              if IBLOCK[JE] <> 0 then
                begin
                  IM := IM + 1;
                  w[IM] := w[JE];
                  IBLOCK[IM] := IBLOCK[JE];
                end;
              inc(JE);
            end;
          M := IM;
        end;
      if (IDISCL < 0) or (IDISCU < 0) then
        begin
          TOOFEW := True;
        end;
    end;

  //
  // If ORDER='B', do nothing -- the eigenvalues are already sorted
  // by block.
  // If ORDER='E', sort the eigenvalues from smallest to largest
  //
  if (IORDER = 1) and (NSPLIT > 1) then
    begin
      JE := 1;
      while JE <= M - 1 do
        begin
          IE := 0;
          TMP1 := w[JE];
          j := JE + 1;
          while j <= M do
            begin
              if AP_FP_Less(w[j], TMP1) then
                begin
                  IE := j;
                  TMP1 := w[j];
                end;
              inc(j);
            end;
          if IE <> 0 then
            begin
              ITMP1 := IBLOCK[IE];
              w[IE] := w[JE];
              IBLOCK[IE] := IBLOCK[JE];
              w[JE] := TMP1;
              IBLOCK[JE] := ITMP1;
            end;
          inc(JE);
        end;
    end;
  j := 1;
  while j <= M do
    begin
      w[j] := w[j] * ScaleFactor;
      inc(j);
    end;
  ErrorCode := 0;
  if NCNVRG then
    begin
      ErrorCode := ErrorCode + 1;
    end;
  if TOOFEW then
    begin
      ErrorCode := ErrorCode + 2;
    end;
  Result := ErrorCode = 0;
end;

procedure InternalDSTEIN(const n: TLInt; const d: TLVec;
  E: TLVec; const M: TLInt; w: TLVec;
  const IBLOCK: TLIVec; const ISPLIT: TLIVec;
  var z: TLMatrix; var IFAIL: TLIVec; var Info: TLInt);
var
  MAXITS: TLInt;
  EXTRA: TLInt;
  b1: TLInt;
  BLKSIZ: TLInt;
  BN: TLInt;
  GPIND: TLInt;
  i: TLInt;
  IINFO: TLInt;
  ITS: TLInt;
  j: TLInt;
  j1: TLInt;
  JBLK: TLInt;
  JMAX: TLInt;
  NBLK: TLInt;
  NRMCHK: TLInt;
  DTPCRT: TLFloat;
  Eps: TLFloat;
  EPS1: TLFloat;
  NRM: TLFloat;
  ONENRM: TLFloat;
  ORTOL: TLFloat;
  PERTOL: TLFloat;
  SCL: TLFloat;
  SEP: TLFloat;
  TOL: TLFloat;
  XJ: TLFloat;
  XJM: TLFloat;
  ZTR: TLFloat;
  Work1: TLVec;
  Work2: TLVec;
  WORK3: TLVec;
  WORK4: TLVec;
  WORK5: TLVec;
  IWORK: TLIVec;
  TmpCriterion: Boolean;
  TI: TLInt;
  i1: TLInt;
  i2: TLInt;
  v: TLFloat;
  i_: TLInt;
  i1_: TLInt;
begin
  E := DynamicArrayCopy(E);
  w := DynamicArrayCopy(w);
  MAXITS := 5;
  EXTRA := 2;
  SetLength(Work1, Max(n, 1) + 1);
  SetLength(Work2, Max(n - 1, 1) + 1);
  SetLength(WORK3, Max(n, 1) + 1);
  SetLength(WORK4, Max(n, 1) + 1);
  SetLength(WORK5, Max(n, 1) + 1);
  SetLength(IWORK, Max(n, 1) + 1);
  SetLength(IFAIL, Max(M, 1) + 1);
  SetLength(z, Max(n, 1) + 1, Max(M, 1) + 1);

  //
  // Test the input parameters.
  //
  Info := 0;
  i := 1;
  while i <= M do
    begin
      IFAIL[i] := 0;
      inc(i);
    end;
  if n < 0 then
    begin
      Info := -1;
      Exit;
    end;
  if (M < 0) or (M > n) then
    begin
      Info := -4;
      Exit;
    end;
  j := 2;
  while j <= M do
    begin
      if IBLOCK[j] < IBLOCK[j - 1] then
        begin
          Info := -6;
          Break;
        end;
      if (IBLOCK[j] = IBLOCK[j - 1]) and AP_FP_Less(w[j], w[j - 1]) then
        begin
          Info := -5;
          Break;
        end;
      inc(j);
    end;
  if Info <> 0 then
    begin
      Exit;
    end;

  //
  // Quick return if possible
  //
  if (n = 0) or (M = 0) then
    begin
      Exit;
    end;
  if n = 1 then
    begin
      z[1, 1] := 1;
      Exit;
    end;

  //
  // Some preparations
  //
  TI := n - 1;
  APVMove(@Work1[0], 1, TI, @E[0], 1, TI);
  SetLength(E, n + 1);
  APVMove(@E[0], 1, TI, @Work1[0], 1, TI);
  APVMove(@Work1[0], 1, M, @w[0], 1, M);
  SetLength(w, n + 1);
  APVMove(@w[0], 1, M, @Work1[0], 1, M);

  //
  // Get machine constants.
  //
  Eps := MachineEpsilon;

  //
  // Compute eigenvectors of matrix blocks.
  //
  j1 := 1;
  NBLK := 1;
  while NBLK <= IBLOCK[M] do
    begin

      //
      // Find starting and ending indices of block nblk.
      //
      if NBLK = 1 then
        begin
          b1 := 1;
        end
      else
        begin
          b1 := ISPLIT[NBLK - 1] + 1;
        end;
      BN := ISPLIT[NBLK];
      BLKSIZ := BN - b1 + 1;
      if BLKSIZ <> 1 then
        begin

          //
          // Compute reorthogonalization criterion and stopping criterion.
          //
          GPIND := b1;
          ONENRM := AbsReal(d[b1]) + AbsReal(E[b1]);
          ONENRM := Max(ONENRM, AbsReal(d[BN]) + AbsReal(E[BN - 1]));
          i := b1 + 1;
          while i <= BN - 1 do
            begin
              ONENRM := Max(ONENRM, AbsReal(d[i]) + AbsReal(E[i - 1]) +
                AbsReal(E[i]));
              inc(i);
            end;
          ORTOL := 0.001 * ONENRM;
          DTPCRT := Sqrt(0.1 / BLKSIZ);
        end;

      //
      // Loop through eigenvalues of block nblk.
      //
      JBLK := 0;
      j := j1;
      while j <= M do
        begin
          if IBLOCK[j] <> NBLK then
            begin
              j1 := j;
              Break;
            end;
          JBLK := JBLK + 1;
          XJ := w[j];
          if BLKSIZ = 1 then
            begin

              //
              // Skip all the work if the block size is one.
              //
              Work1[1] := 1;
            end
          else
            begin

              //
              // If eigenvalues j and j-1 are too close, add a relatively
              // small perturbation.
              //
              if JBLK > 1 then
                begin
                  EPS1 := AbsReal(Eps * XJ);
                  PERTOL := 10 * EPS1;
                  SEP := XJ - XJM;
                  if AP_FP_Less(SEP, PERTOL) then
                    begin
                      XJ := XJM + PERTOL;
                    end;
                end;
              ITS := 0;
              NRMCHK := 0;

              //
              // Get random starting vector.
              //
              TI := 1;
              while TI <= BLKSIZ do
                begin
                  Work1[TI] := 2 * RandomReal - 1;
                  inc(TI);
                end;

              //
              // Copy the matrix T so it won't be destroyed in factorization.
              //
              TI := 1;
              while TI <= BLKSIZ - 1 do
                begin
                  Work2[TI] := E[b1 + TI - 1];
                  WORK3[TI] := E[b1 + TI - 1];
                  WORK4[TI] := d[b1 + TI - 1];
                  inc(TI);
                end;
              WORK4[BLKSIZ] := d[b1 + BLKSIZ - 1];

              //
              // Compute LU factors with partial pivoting  ( PT = LU )
              //
              TOL := 0;
              TDINInternalDLAGTF(BLKSIZ, WORK4, XJ, Work2, WORK3, TOL, WORK5,
                IWORK, IINFO);

              //
              // Update iteration count.
              //
              repeat
                ITS := ITS + 1;
                if ITS > MAXITS then
                  begin

                    //
                    // If stopping criterion was not satisfied, update info and
                    // store eigenvector number in array ifail.
                    //
                    Info := Info + 1;
                    IFAIL[Info] := j;
                    Break;
                  end;

                //
                // Normalize and scale the righthand side vector Pb.
                //
                v := 0;
                TI := 1;
                while TI <= BLKSIZ do
                  begin
                    v := v + AbsReal(Work1[TI]);
                    inc(TI);
                  end;
                SCL := BLKSIZ * ONENRM * Max(Eps, AbsReal(WORK4[BLKSIZ])) / v;
                APVMul(@Work1[0], 1, BLKSIZ, SCL);

                //
                // Solve the system LU = Pb.
                //
                TDINInternalDLAGTS(BLKSIZ, WORK4, Work2, WORK3, WORK5, IWORK, Work1,
                  TOL, IINFO);

                //
                // Reorthogonalize by modified Gram-Schmidt if eigenvalues are
                // close enough.
                //
                if JBLK <> 1 then
                  begin
                    if AP_FP_Greater(AbsReal(XJ - XJM), ORTOL) then
                      begin
                        GPIND := j;
                      end;
                    if GPIND <> j then
                      begin
                        i := GPIND;
                        while i <= j - 1 do
                          begin
                            i1 := b1;
                            i2 := b1 + BLKSIZ - 1;
                            i1_ := (i1) - (1);
                            ZTR := 0.0;
                            for i_ := 1 to BLKSIZ do
                              begin
                                ZTR := ZTR + Work1[i_] * z[i_ + i1_, i];
                              end;
                            i1_ := (i1) - (1);
                            for i_ := 1 to BLKSIZ do
                              begin
                                Work1[i_] := Work1[i_] - ZTR * z[i_ + i1_, i];
                              end;
                            inc(i);
                          end;
                      end;
                  end;

                //
                // Check the infinity norm of the iterate.
                //
                JMAX := VectorIdxAbsMax(Work1, 1, BLKSIZ);
                NRM := AbsReal(Work1[JMAX]);

                //
                // Continue for additional iterations after norm reaches
                // stopping criterion.
                //
                TmpCriterion := False;
                if AP_FP_Less(NRM, DTPCRT) then
                  begin
                    TmpCriterion := True;
                  end
                else
                  begin
                    NRMCHK := NRMCHK + 1;
                    if NRMCHK < EXTRA + 1 then
                      begin
                        TmpCriterion := True;
                      end;
                  end;
              until not TmpCriterion;

              //
              // Accept iterate as jth eigenvector.
              //
              SCL := 1 / VectorNorm2(Work1, 1, BLKSIZ);
              JMAX := VectorIdxAbsMax(Work1, 1, BLKSIZ);
              if AP_FP_Less(Work1[JMAX], 0) then
                begin
                  SCL := -SCL;
                end;
              APVMul(@Work1[0], 1, BLKSIZ, SCL);
            end;
          i := 1;
          while i <= n do
            begin
              z[i, j] := 0;
              inc(i);
            end;
          i := 1;
          while i <= BLKSIZ do
            begin
              z[b1 + i - 1, j] := Work1[i];
              inc(i);
            end;

          //
          // Save the shift to check eigenvalue spacing at next
          // iteration.
          //
          XJM := XJ;
          inc(j);
        end;
      inc(NBLK);
    end;
end;

function TridiagonalEVD(var d: TLVec; E: TLVec; n: TLInt;
  ZNeeded: TLInt; var z: TLMatrix): Boolean;
var
  MAXIT: TLInt;
  i: TLInt;
  II: TLInt;
  ISCALE: TLInt;
  j: TLInt;
  JTOT: TLInt;
  k: TLInt;
  t: TLInt;
  L: TLInt;
  L1: TLInt;
  LEND: TLInt;
  LENDM1: TLInt;
  LENDP1: TLInt;
  LENDSV: TLInt;
  LM1: TLInt;
  LSV: TLInt;
  M: TLInt;
  MM: TLInt;
  MM1: TLInt;
  NM1: TLInt;
  NMAXIT: TLInt;
  TmpInt: TLInt;
  ANORM: TLFloat;
  b: TLFloat;
  c: TLFloat;
  Eps: TLFloat;
  EPS2: TLFloat;
  f: TLFloat;
  g: TLFloat;
  p: TLFloat;
  r: TLFloat;
  RT1: TLFloat;
  RT2: TLFloat;
  s: TLFloat;
  SAFMAX: TLFloat;
  SAFMIN: TLFloat;
  SSFMAX: TLFloat;
  SSFMIN: TLFloat;
  TST: TLFloat;
  tmp: TLFloat;
  Work1: TLVec;
  Work2: TLVec;
  WORKC: TLVec;
  WORKS: TLVec;
  WTEMP: TLVec;
  GotoFlag: Boolean;
  ZRows: TLInt;
  WasTranspose: Boolean;
  i_: TLInt;
begin
  E := DynamicArrayCopy(E);
  Assert((ZNeeded >= 0) and (ZNeeded <= 3),
    'TridiagonalEVD: Incorrent ZNeeded');

  //
  // Quick return if possible
  //
  if (ZNeeded < 0) or (ZNeeded > 3) then
    begin
      Result := False;
      Exit;
    end;
  Result := True;
  if n = 0 then
    begin
      Exit;
    end;
  if n = 1 then
    begin
      if (ZNeeded = 2) or (ZNeeded = 3) then
        begin
          SetLength(z, 1 + 1, 1 + 1);
          z[1, 1] := 1;
        end;
      Exit;
    end;
  MAXIT := 30;

  //
  // Initialize arrays
  //
  SetLength(WTEMP, n + 1);
  SetLength(Work1, n  );
  SetLength(Work2, n  );
  SetLength(WORKC, n + 1);
  SetLength(WORKS, n + 1);

  //
  // Determine the unit roundoff and over/underflow thresholds.
  //
  Eps := MachineEpsilon;
  EPS2 := AP_Sqr(Eps);
  SAFMIN := MinRealNumber;
  SAFMAX := MaxRealNumber;
  SSFMAX := Sqrt(SAFMAX) / 3;
  SSFMIN := Sqrt(SAFMIN) / EPS2;

  //
  // Prepare Z
  //
  // Here we are using transposition to get rid of column operations
  //
  //
  WasTranspose := False;
  if ZNeeded = 0 then
    begin
      ZRows := 0;
    end;
  if ZNeeded = 1 then
    begin
      ZRows := n;
    end;
  if ZNeeded = 2 then
    begin
      ZRows := n;
    end;
  if ZNeeded = 3 then
    begin
      ZRows := 1;
    end;
  if ZNeeded = 1 then
    begin
      WasTranspose := True;
      InplaceTranspose(z, 1, n, 1, n, WTEMP);
    end;
  if ZNeeded = 2 then
    begin
      WasTranspose := True;
      SetLength(z, n + 1, n + 1);
      i := 1;
      while i <= n do
        begin
          j := 1;
          while j <= n do
            begin
              if i = j then
                begin
                  z[i, j] := 1;
                end
              else
                begin
                  z[i, j] := 0;
                end;
              inc(j);
            end;
          inc(i);
        end;
    end;
  if ZNeeded = 3 then
    begin
      WasTranspose := False;
      SetLength(z, 1 + 1, n + 1);
      j := 1;
      while j <= n do
        begin
          if j = 1 then
            begin
              z[1, j] := 1;
            end
          else
            begin
              z[1, j] := 0;
            end;
          inc(j);
        end;
    end;
  NMAXIT := n * MAXIT;
  JTOT := 0;

  //
  // Determine where the matrix splits and choose QL or QR iteration
  // for each block, according to whether top or bottom diagonal
  // element is smaller.
  //
  L1 := 1;
  NM1 := n - 1;
  while True do
    begin
      if L1 > n then
        begin
          Break;
        end;
      if L1 > 1 then
        begin
          E[L1 - 1] := 0;
        end;
      GotoFlag := False;
      if L1 <= NM1 then
        begin
          M := L1;
          while M <= NM1 do
            begin
              TST := AbsReal(E[M]);
              if AP_FP_Eq(TST, 0) then
                begin
                  GotoFlag := True;
                  Break;
                end;
              if AP_FP_Less_Eq(TST, Sqrt(AbsReal(d[M])) * Sqrt(AbsReal(d[M + 1]))
                * Eps) then
                begin
                  E[M] := 0;
                  GotoFlag := True;
                  Break;
                end;
              inc(M);
            end;
        end;
      if not GotoFlag then
        begin
          M := n;
        end;

      //
      // label 30:
      //
      L := L1;
      LSV := L;
      LEND := M;
      LENDSV := LEND;
      L1 := M + 1;
      if LEND = L then
        begin
          Continue;
        end;

      //
      // Scale submatrix in rows and columns L to LEND
      //
      if L = LEND then
        begin
          ANORM := AbsReal(d[L]);
        end
      else
        begin
          ANORM := Max(AbsReal(d[L]) + AbsReal(E[L]), AbsReal(E[LEND - 1]) +
            AbsReal(d[LEND]));
          i := L + 1;
          while i <= LEND - 1 do
            begin
              ANORM := Max(ANORM, AbsReal(d[i]) + AbsReal(E[i]) + AbsReal(E[i - 1]));
              inc(i);
            end;
        end;
      ISCALE := 0;
      if AP_FP_Eq(ANORM, 0) then
        begin
          Continue;
        end;
      if AP_FP_Greater(ANORM, SSFMAX) then
        begin
          ISCALE := 1;
          tmp := SSFMAX / ANORM;
          TmpInt := LEND - 1;
          APVMul(@d[0], L, LEND, tmp);
          APVMul(@E[0], L, TmpInt, tmp);
        end;
      if AP_FP_Less(ANORM, SSFMIN) then
        begin
          ISCALE := 2;
          tmp := SSFMIN / ANORM;
          TmpInt := LEND - 1;
          APVMul(@d[0], L, LEND, tmp);
          APVMul(@E[0], L, TmpInt, tmp);
        end;

      //
      // Choose between QL and QR iteration
      //
      if AP_FP_Less(AbsReal(d[LEND]), AbsReal(d[L])) then
        begin
          LEND := LSV;
          L := LENDSV;
        end;
      if LEND > L then
        begin

          //
          // QL Iteration
          //
          // Look for small subdiagonal element.
          //
          while True do
            begin
              GotoFlag := False;
              if L <> LEND then
                begin
                  LENDM1 := LEND - 1;
                  M := L;
                  while M <= LENDM1 do
                    begin
                      TST := AP_Sqr(AbsReal(E[M]));
                      if AP_FP_Less_Eq(TST, EPS2 * AbsReal(d[M]) * AbsReal(d[M + 1]) +
                        SAFMIN) then
                        begin
                          GotoFlag := True;
                          Break;
                        end;
                      inc(M);
                    end;
                end;
              if not GotoFlag then
                begin
                  M := LEND;
                end;
              if M < LEND then
                begin
                  E[M] := 0;
                end;
              p := d[L];
              if M <> L then
                begin

                  //
                  // If remaining matrix is 2-by-2, use DLAE2 or SLAEV2
                  // to compute its eigensystem.
                  //
                  if M = L + 1 then
                    begin
                      if ZNeeded > 0 then
                        begin
                          TdEVDEV2(d[L], E[L], d[L + 1], RT1, RT2, c, s);
                          Work1[L] := c;
                          Work2[L] := s;
                          WORKC[1] := Work1[L];
                          WORKS[1] := Work2[L];
                          if not WasTranspose then
                            begin
                              ApplyRotationsFromTheRight(False, 1, ZRows, L, L + 1, WORKC,
                                WORKS, z, WTEMP);
                            end
                          else
                            begin
                              ApplyRotationsFromTheLeft(False, L, L + 1, 1, ZRows, WORKC,
                                WORKS, z, WTEMP);
                            end;
                        end
                      else
                        begin
                          TdEVDE2(d[L], E[L], d[L + 1], RT1, RT2);
                        end;
                      d[L] := RT1;
                      d[L + 1] := RT2;
                      E[L] := 0;
                      L := L + 2;
                      if L <= LEND then
                        begin
                          Continue;
                        end;

                      //
                      // GOTO 140
                      //
                      Break;
                    end;
                  if JTOT = NMAXIT then
                    begin

                      //
                      // GOTO 140
                      //
                      Break;
                    end;
                  JTOT := JTOT + 1;

                  //
                  // Form shift.
                  //
                  g := (d[L + 1] - p) / (2 * E[L]);
                  r := TdEVDPythag(g, 1);
                  g := d[M] - p + E[L] / (g + TdEVDExtSign(r, g));
                  s := 1;
                  c := 1;
                  p := 0;

                  //
                  // Inner loop
                  //
                  MM1 := M - 1;
                  i := MM1;
                  while i >= L do
                    begin
                      f := s * E[i];
                      b := c * E[i];
                      GenerateRotation(g, f, c, s, r);
                      if i <> M - 1 then
                        begin
                          E[i + 1] := r;
                        end;
                      g := d[i + 1] - p;
                      r := (d[i] - g) * s + 2 * c * b;
                      p := s * r;
                      d[i + 1] := g + p;
                      g := c * r - b;

                      //
                      // If eigenvectors are desired, then save rotations.
                      //
                      if ZNeeded > 0 then
                        begin
                          Work1[i] := c;
                          Work2[i] := -s;
                        end;
                      dec(i);
                    end;

                  //
                  // If eigenvectors are desired, then apply saved rotations.
                  //
                  if ZNeeded > 0 then
                    begin
                      i := L;
                      while i <= M - 1 do
                        begin
                          WORKC[i - L + 1] := Work1[i];
                          WORKS[i - L + 1] := Work2[i];
                          inc(i);
                        end;
                      if not WasTranspose then
                        begin
                          ApplyRotationsFromTheRight(False, 1, ZRows, L, M, WORKC, WORKS,
                            z, WTEMP);
                        end
                      else
                        begin
                          ApplyRotationsFromTheLeft(False, L, M, 1, ZRows, WORKC, WORKS,
                            z, WTEMP);
                        end;
                    end;
                  d[L] := d[L] - p;
                  E[L] := g;
                  Continue;
                end;

              //
              // Eigenvalue found.
              //
              d[L] := p;
              L := L + 1;
              if L <= LEND then
                begin
                  Continue;
                end;
              Break;
            end;
        end
      else
        begin

          //
          // QR Iteration
          //
          // Look for small superdiagonal element.
          //
          while True do
            begin
              GotoFlag := False;
              if L <> LEND then
                begin
                  LENDP1 := LEND + 1;
                  M := L;
                  while M >= LENDP1 do
                    begin
                      TST := AP_Sqr(AbsReal(E[M - 1]));
                      if AP_FP_Less_Eq(TST, EPS2 * AbsReal(d[M]) * AbsReal(d[M - 1]) +
                        SAFMIN) then
                        begin
                          GotoFlag := True;
                          Break;
                        end;
                      dec(M);
                    end;
                end;
              if not GotoFlag then
                begin
                  M := LEND;
                end;
              if M > LEND then
                begin
                  E[M - 1] := 0;
                end;
              p := d[L];
              if M <> L then
                begin

                  //
                  // If remaining matrix is 2-by-2, use DLAE2 or SLAEV2
                  // to compute its eigensystem.
                  //
                  if M = L - 1 then
                    begin
                      if ZNeeded > 0 then
                        begin
                          TdEVDEV2(d[L - 1], E[L - 1], d[L], RT1, RT2, c, s);
                          Work1[M] := c;
                          Work2[M] := s;
                          WORKC[1] := c;
                          WORKS[1] := s;
                          if not WasTranspose then
                            begin
                              ApplyRotationsFromTheRight(True, 1, ZRows, L - 1, L, WORKC,
                                WORKS, z, WTEMP);
                            end
                          else
                            begin
                              ApplyRotationsFromTheLeft(True, L - 1, L, 1, ZRows, WORKC,
                                WORKS, z, WTEMP);
                            end;
                        end
                      else
                        begin
                          TdEVDE2(d[L - 1], E[L - 1], d[L], RT1, RT2);
                        end;
                      d[L - 1] := RT1;
                      d[L] := RT2;
                      E[L - 1] := 0;
                      L := L - 2;
                      if L >= LEND then
                        begin
                          Continue;
                        end;
                      Break;
                    end;
                  if JTOT = NMAXIT then
                    begin
                      Break;
                    end;
                  JTOT := JTOT + 1;

                  //
                  // Form shift.
                  //
                  g := (d[L - 1] - p) / (2 * E[L - 1]);
                  r := TdEVDPythag(g, 1);
                  g := d[M] - p + E[L - 1] / (g + TdEVDExtSign(r, g));
                  s := 1;
                  c := 1;
                  p := 0;

                  //
                  // Inner loop
                  //
                  LM1 := L - 1;
                  i := M;
                  while i <= LM1 do
                    begin
                      f := s * E[i];
                      b := c * E[i];
                      GenerateRotation(g, f, c, s, r);
                      if i <> M then
                        begin
                          E[i - 1] := r;
                        end;
                      g := d[i] - p;
                      r := (d[i + 1] - g) * s + 2 * c * b;
                      p := s * r;
                      d[i] := g + p;
                      g := c * r - b;

                      //
                      // If eigenvectors are desired, then save rotations.
                      //
                      if ZNeeded > 0 then
                        begin
                          Work1[i] := c;
                          Work2[i] := s;
                        end;
                      inc(i);
                    end;

                  //
                  // If eigenvectors are desired, then apply saved rotations.
                  //
                  if ZNeeded > 0 then
                    begin
                      MM := L - M + 1;
                      i := M;
                      while i <= L - 1 do
                        begin
                          WORKC[i - M + 1] := Work1[i];
                          WORKS[i - M + 1] := Work2[i];
                          inc(i);
                        end;
                      if not WasTranspose then
                        begin
                          ApplyRotationsFromTheRight(True, 1, ZRows, M, L, WORKC, WORKS,
                            z, WTEMP);
                        end
                      else
                        begin
                          ApplyRotationsFromTheLeft(True, M, L, 1, ZRows, WORKC, WORKS,
                            z, WTEMP);
                        end;
                    end;
                  d[L] := d[L] - p;
                  E[LM1] := g;
                  Continue;
                end;

              //
              // Eigenvalue found.
              //
              d[L] := p;
              L := L - 1;
              if L >= LEND then
                begin
                  Continue;
                end;
              Break;
            end;
        end;

      //
      // Undo scaling if necessary
      //
      if ISCALE = 1 then
        begin
          tmp := ANORM / SSFMAX;
          TmpInt := LENDSV - 1;
          APVMul(@d[0], LSV, LENDSV, tmp);
          APVMul(@E[0], LSV, TmpInt, tmp);
        end;
      if ISCALE = 2 then
        begin
          tmp := ANORM / SSFMIN;
          TmpInt := LENDSV - 1;
          APVMul(@d[0], LSV, LENDSV, tmp);
          APVMul(@E[0], LSV, TmpInt, tmp);
        end;

      //
      // Check for no convergence to an eigenvalue after a total
      // of N*MAXIT iterations.
      //
      if JTOT >= NMAXIT then
        begin
          Result := False;
          if WasTranspose then
            begin
              InplaceTranspose(z, 1, n, 1, n, WTEMP);
            end;
          Exit;
        end;
    end;

  //
  // Order eigenvalues and eigenvectors.
  //
  if ZNeeded = 0 then
    begin

      //
      // Sort
      //
      if n = 1 then
        begin
          Exit;
        end;
      if n = 2 then
        begin
          if AP_FP_Greater(d[1], d[2]) then
            begin
              tmp := d[1];
              d[1] := d[2];
              d[2] := tmp;
            end;
          Exit;
        end;
      i := 2;
      repeat
        t := i;
        while t <> 1 do
          begin
            k := t div 2;
            if AP_FP_Greater_Eq(d[k], d[t]) then
              begin
                t := 1;
              end
            else
              begin
                tmp := d[k];
                d[k] := d[t];
                d[t] := tmp;
                t := k;
              end;
          end;
        i := i + 1;
      until not(i <= n);
      i := n - 1;
      repeat
        tmp := d[i + 1];
        d[i + 1] := d[1];
        d[+1] := tmp;
        t := 1;
        while t <> 0 do
          begin
            k := 2 * t;
            if k > i then
              begin
                t := 0;
              end
            else
              begin
                if k < i then
                  begin
                    if AP_FP_Greater(d[k + 1], d[k]) then
                      begin
                        k := k + 1;
                      end;
                  end;
                if AP_FP_Greater_Eq(d[t], d[k]) then
                  begin
                    t := 0;
                  end
                else
                  begin
                    tmp := d[k];
                    d[k] := d[t];
                    d[t] := tmp;
                    t := k;
                  end;
              end;
          end;
        i := i - 1;
      until not(i >= 1);
    end
  else
    begin

      //
      // Use Selection Sort to minimize swaps of eigenvectors
      //
      II := 2;
      while II <= n do
        begin
          i := II - 1;
          k := i;
          p := d[i];
          j := II;
          while j <= n do
            begin
              if AP_FP_Less(d[j], p) then
                begin
                  k := j;
                  p := d[j];
                end;
              inc(j);
            end;
          if k <> i then
            begin
              d[k] := d[i];
              d[i] := p;
              if WasTranspose then
                begin
                  APVMove(@WTEMP[0], 1, n, @z[i][0], 1, n);
                  APVMove(@z[i][0], 1, n, @z[k][0], 1, n);
                  APVMove(@z[k][0], 1, n, @WTEMP[0], 1, n);
                end
              else
                begin
                  for i_ := 1 to ZRows do
                    begin
                      WTEMP[i_] := z[i_, i];
                    end;
                  for i_ := 1 to ZRows do
                    begin
                      z[i_, i] := z[i_, k];
                    end;
                  for i_ := 1 to ZRows do
                    begin
                      z[i_, k] := WTEMP[i_];
                    end;
                end;
            end;
          inc(II);
        end;
      if WasTranspose then
        begin
          InplaceTranspose(z, 1, n, 1, n, WTEMP);
        end;
    end;
end;

(* ************************************************************************
  DLAE2  computes the eigenvalues of a 2-by-2 symmetric matrix
  [  A   B  ]
  [  B   C  ].
  On return, RT1 is the eigenvalue of larger absolute value, and RT2
  is the eigenvalue of smaller absolute value.
  ************************************************************************ *)
procedure TdEVDE2(const a: TLFloat; const b: TLFloat;
  const c: TLFloat; var RT1: TLFloat; var RT2: TLFloat);
var
  AB: TLFloat;
  ACMN: TLFloat;
  ACMX: TLFloat;
  ADF: TLFloat;
  df: TLFloat;
  RT: TLFloat;
  SM: TLFloat;
  TB: TLFloat;
begin
  SM := a + c;
  df := a - c;
  ADF := AbsReal(df);
  TB := b + b;
  AB := AbsReal(TB);
  if AP_FP_Greater(AbsReal(a), AbsReal(c)) then
    begin
      ACMX := a;
      ACMN := c;
    end
  else
    begin
      ACMX := c;
      ACMN := a;
    end;
  if AP_FP_Greater(ADF, AB) then
    begin
      RT := ADF * Sqrt(1 + AP_Sqr(AB / ADF));
    end
  else
    begin
      if AP_FP_Less(ADF, AB) then
        begin
          RT := AB * Sqrt(1 + AP_Sqr(ADF / AB));
        end
      else
        begin

          //
          // Includes case AB=ADF=0
          //
          RT := AB * Sqrt(2);
        end;
    end;
  if AP_FP_Less(SM, 0) then
    begin
      RT1 := 0.5 * (SM - RT);

      //
      // Order of execution important.
      // To get fully accurate smaller eigenvalue,
      // next line needs to be executed in higher precision.
      //
      RT2 := ACMX / RT1 * ACMN - b / RT1 * b;
    end
  else
    begin
      if AP_FP_Greater(SM, 0) then
        begin
          RT1 := 0.5 * (SM + RT);

          //
          // Order of execution important.
          // To get fully accurate smaller eigenvalue,
          // next line needs to be executed in higher precision.
          //
          RT2 := ACMX / RT1 * ACMN - b / RT1 * b;
        end
      else
        begin

          //
          // Includes case RT1 = RT2 = 0
          //
          RT1 := 0.5 * RT;
          RT2 := -0.5 * RT;
        end;
    end;
end;

(* ************************************************************************
  DLAEV2 computes the eigendecomposition of a 2-by-2 symmetric matrix

  [  A   B  ]
  [  B   C  ].

  On return, RT1 is the eigenvalue of larger absolute value, RT2 is the
  eigenvalue of smaller absolute value, and (CS1,SN1) is the unit right
  eigenvector for RT1, giving the decomposition

  [ CS1  SN1 ] [  A   B  ] [ CS1 -SN1 ]  =  [ RT1  0  ]
  [-SN1  CS1 ] [  B   C  ] [ SN1  CS1 ]     [  0  RT2 ].
  ************************************************************************ *)
procedure TdEVDEV2(const a: TLFloat; const b: TLFloat;
  const c: TLFloat; var RT1: TLFloat; var RT2: TLFloat;
  var CS1: TLFloat; var SN1: TLFloat);
var
  SGN1: TLInt;
  SGN2: TLInt;
  AB: TLFloat;
  ACMN: TLFloat;
  ACMX: TLFloat;
  ACS: TLFloat;
  ADF: TLFloat;
  cs: TLFloat;
  ct: TLFloat;
  df: TLFloat;
  RT: TLFloat;
  SM: TLFloat;
  TB: TLFloat;
  tN: TLFloat;
begin

  //
  // Compute the eigenvalues
  //
  SM := a + c;
  df := a - c;
  ADF := AbsReal(df);
  TB := b + b;
  AB := AbsReal(TB);
  if AP_FP_Greater(AbsReal(a), AbsReal(c)) then
    begin
      ACMX := a;
      ACMN := c;
    end
  else
    begin
      ACMX := c;
      ACMN := a;
    end;
  if AP_FP_Greater(ADF, AB) then
    begin
      RT := ADF * Sqrt(1 + AP_Sqr(AB / ADF));
    end
  else
    begin
      if AP_FP_Less(ADF, AB) then
        begin
          RT := AB * Sqrt(1 + AP_Sqr(ADF / AB));
        end
      else
        begin

          //
          // Includes case AB=ADF=0
          //
          RT := AB * Sqrt(2);
        end;
    end;
  if AP_FP_Less(SM, 0) then
    begin
      RT1 := 0.5 * (SM - RT);
      SGN1 := -1;

      //
      // Order of execution important.
      // To get fully accurate smaller eigenvalue,
      // next line needs to be executed in higher precision.
      //
      RT2 := ACMX / RT1 * ACMN - b / RT1 * b;
    end
  else
    begin
      if AP_FP_Greater(SM, 0) then
        begin
          RT1 := 0.5 * (SM + RT);
          SGN1 := 1;

          //
          // Order of execution important.
          // To get fully accurate smaller eigenvalue,
          // next line needs to be executed in higher precision.
          //
          RT2 := ACMX / RT1 * ACMN - b / RT1 * b;
        end
      else
        begin

          //
          // Includes case RT1 = RT2 = 0
          //
          RT1 := 0.5 * RT;
          RT2 := -0.5 * RT;
          SGN1 := 1;
        end;
    end;

  //
  // Compute the eigenvector
  //
  if AP_FP_Greater_Eq(df, 0) then
    begin
      cs := df + RT;
      SGN2 := 1;
    end
  else
    begin
      cs := df - RT;
      SGN2 := -1;
    end;
  ACS := AbsReal(cs);
  if AP_FP_Greater(ACS, AB) then
    begin
      ct := -TB / cs;
      SN1 := 1 / Sqrt(1 + ct * ct);
      CS1 := ct * SN1;
    end
  else
    begin
      if AP_FP_Eq(AB, 0) then
        begin
          CS1 := 1;
          SN1 := 0;
        end
      else
        begin
          tN := -cs / TB;
          CS1 := 1 / Sqrt(1 + tN * tN);
          SN1 := tN * CS1;
        end;
    end;
  if SGN1 = SGN2 then
    begin
      tN := CS1;
      CS1 := -SN1;
      SN1 := tN;
    end;
end;

(* ************************************************************************
  Internal routine
  ************************************************************************ *)
function TdEVDPythag(a: TLFloat; b: TLFloat): TLFloat;
begin
  if AP_FP_Less(AbsReal(a), AbsReal(b)) then
    begin
      Result := AbsReal(b) * Sqrt(1 + AP_Sqr(a / b));
    end
  else
    begin
      Result := AbsReal(a) * Sqrt(1 + AP_Sqr(b / a));
    end;
end;

(* ************************************************************************
  Internal routine
  ************************************************************************ *)
function TdEVDExtSign(a: TLFloat; b: TLFloat): TLFloat;
begin
  if AP_FP_Greater_Eq(b, 0) then
    begin
      Result := AbsReal(a);
    end
  else
    begin
      Result := -AbsReal(a);
    end;
end;

procedure TDINInternalDLAGTF(const n: TLInt; var a: TLVec;
  const LAMBDA: TLFloat; var b: TLVec; var c: TLVec;
  const TOL: TLFloat; var d: TLVec; var IIN: TLIVec;
  var Info: TLInt);
var
  k: TLInt;
  Eps: TLFloat;
  MULT: TLFloat;
  PIV1: TLFloat;
  PIV2: TLFloat;
  scale1: TLFloat;
  SCALE2: TLFloat;
  Temp: TLFloat;
  TL: TLFloat;
begin
  Info := 0;
  if n < 0 then
    begin
      Info := -1;
      Exit;
    end;
  if n = 0 then
    begin
      Exit;
    end;
  a[1] := a[1] - LAMBDA;
  IIN[n] := 0;
  if n = 1 then
    begin
      if AP_FP_Eq(a[1], 0) then
        begin
          IIN[1] := 1;
        end;
      Exit;
    end;
  Eps := MachineEpsilon;
  TL := Max(TOL, Eps);
  scale1 := AbsReal(a[1]) + AbsReal(b[1]);
  k := 1;
  while k <= n - 1 do
    begin
      a[k + 1] := a[k + 1] - LAMBDA;
      SCALE2 := AbsReal(c[k]) + AbsReal(a[k + 1]);
      if k < n - 1 then
        begin
          SCALE2 := SCALE2 + AbsReal(b[k + 1]);
        end;
      if AP_FP_Eq(a[k], 0) then
        begin
          PIV1 := 0;
        end
      else
        begin
          PIV1 := AbsReal(a[k]) / scale1;
        end;
      if AP_FP_Eq(c[k], 0) then
        begin
          IIN[k] := 0;
          PIV2 := 0;
          scale1 := SCALE2;
          if k < n - 1 then
            begin
              d[k] := 0;
            end;
        end
      else
        begin
          PIV2 := AbsReal(c[k]) / SCALE2;
          if AP_FP_Less_Eq(PIV2, PIV1) then
            begin
              IIN[k] := 0;
              scale1 := SCALE2;
              c[k] := c[k] / a[k];
              a[k + 1] := a[k + 1] - c[k] * b[k];
              if k < n - 1 then
                begin
                  d[k] := 0;
                end;
            end
          else
            begin
              IIN[k] := 1;
              MULT := a[k] / c[k];
              a[k] := c[k];
              Temp := a[k + 1];
              a[k + 1] := b[k] - MULT * Temp;
              if k < n - 1 then
                begin
                  d[k] := b[k + 1];
                  b[k + 1] := -MULT * d[k];
                end;
              b[k] := Temp;
              c[k] := MULT;
            end;
        end;
      if AP_FP_Less_Eq(Max(PIV1, PIV2), TL) and (IIN[n] = 0) then
        begin
          IIN[n] := k;
        end;
      inc(k);
    end;
  if AP_FP_Less_Eq(AbsReal(a[n]), scale1 * TL) and (IIN[n] = 0) then
    begin
      IIN[n] := n;
    end;
end;

procedure TDINInternalDLAGTS(const n: TLInt; const a: TLVec;
  const b: TLVec; const c: TLVec; const d: TLVec;
  const IIN: TLIVec; var y: TLVec; var TOL: TLFloat;
  var Info: TLInt);
var
  k: TLInt;
  ABSAK: TLFloat;
  AK: TLFloat;
  BIGNUM: TLFloat;
  Eps: TLFloat;
  PERT: TLFloat;
  SFMIN: TLFloat;
  Temp: TLFloat;
begin
  Info := 0;
  if n < 0 then
    begin
      Info := -1;
      Exit;
    end;
  if n = 0 then
    begin
      Exit;
    end;
  Eps := MachineEpsilon;
  SFMIN := MinRealNumber;
  BIGNUM := 1 / SFMIN;
  if AP_FP_Less_Eq(TOL, 0) then
    begin
      TOL := AbsReal(a[1]);
      if n > 1 then
        begin
          TOL := Max(TOL, Max(AbsReal(a[2]), AbsReal(b[1])));
        end;
      k := 3;
      while k <= n do
        begin
          TOL := Max(TOL, Max(AbsReal(a[k]), Max(AbsReal(b[k - 1]),
            AbsReal(d[k - 2]))));
          inc(k);
        end;
      TOL := TOL * Eps;
      if AP_FP_Eq(TOL, 0) then
        begin
          TOL := Eps;
        end;
    end;
  k := 2;
  while k <= n do
    begin
      if IIN[k - 1] = 0 then
        begin
          y[k] := y[k] - c[k - 1] * y[k - 1];
        end
      else
        begin
          Temp := y[k - 1];
          y[k - 1] := y[k];
          y[k] := Temp - c[k - 1] * y[k];
        end;
      inc(k);
    end;
  k := n;
  while k >= 1 do
    begin
      if k <= n - 2 then
        begin
          Temp := y[k] - b[k] * y[k + 1] - d[k] * y[k + 2];
        end
      else
        begin
          if k = n - 1 then
            begin
              Temp := y[k] - b[k] * y[k + 1];
            end
          else
            begin
              Temp := y[k];
            end;
        end;
      AK := a[k];
      PERT := AbsReal(TOL);
      if AP_FP_Less(AK, 0) then
        begin
          PERT := -PERT;
        end;
      while True do
        begin
          ABSAK := AbsReal(AK);
          if AP_FP_Less(ABSAK, 1) then
            begin
              if AP_FP_Less(ABSAK, SFMIN) then
                begin
                  if AP_FP_Eq(ABSAK, 0) or AP_FP_Greater(AbsReal(Temp) * SFMIN, ABSAK)
                  then
                    begin
                      AK := AK + PERT;
                      PERT := 2 * PERT;
                      Continue;
                    end
                  else
                    begin
                      Temp := Temp * BIGNUM;
                      AK := AK * BIGNUM;
                    end;
                end
              else
                begin
                  if AP_FP_Greater(AbsReal(Temp), ABSAK * BIGNUM) then
                    begin
                      AK := AK + PERT;
                      PERT := 2 * PERT;
                      Continue;
                    end;
                end;
            end;
          Break;
        end;
      y[k] := Temp / AK;
      dec(k);
    end;
end;

procedure InternalDLAEBZ(const IJOB: TLInt; const NITMAX: TLInt;
  const n: TLInt; const MMAX: TLInt; const MINP: TLInt;
  const ABSTOL: TLFloat; const RELTOL: TLFloat;
  const PIVMIN: TLFloat; const d: TLVec; const E: TLVec;
  const e2: TLVec; var NVAL: TLIVec; var AB: TLMatrix;
  var c: TLVec; var MOUT: TLInt; var NAB: TLIMatrix;
  var Work: TLVec; var IWORK: TLIVec; var Info: TLInt);
var
  ITMP1: TLInt;
  ITMP2: TLInt;
  j: TLInt;
  JI: TLInt;
  JIT: TLInt;
  JP: TLInt;
  KF: TLInt;
  KFNEW: TLInt;
  KL: TLInt;
  KLNEW: TLInt;
  TMP1: TLFloat;
  TMP2: TLFloat;
begin
  Info := 0;
  if (IJOB < 1) or (IJOB > 3) then
    begin
      Info := -1;
      Exit;
    end;

  //
  // Initialize NAB
  //
  if IJOB = 1 then
    begin

      //
      // Compute the number of eigenvalues in the initial intervals.
      //
      MOUT := 0;

      //
      // DIR$ NOVECTOR
      //
      JI := 1;
      while JI <= MINP do
        begin
          JP := 1;
          while JP <= 2 do
            begin
              TMP1 := d[1] - AB[JI, JP];
              if AP_FP_Less(AbsReal(TMP1), PIVMIN) then
                begin
                  TMP1 := -PIVMIN;
                end;
              NAB[JI, JP] := 0;
              if AP_FP_Less_Eq(TMP1, 0) then
                begin
                  NAB[JI, JP] := 1;
                end;
              j := 2;
              while j <= n do
                begin
                  TMP1 := d[j] - e2[j - 1] / TMP1 - AB[JI, JP];
                  if AP_FP_Less(AbsReal(TMP1), PIVMIN) then
                    begin
                      TMP1 := -PIVMIN;
                    end;
                  if AP_FP_Less_Eq(TMP1, 0) then
                    begin
                      NAB[JI, JP] := NAB[JI, JP] + 1;
                    end;
                  inc(j);
                end;
              inc(JP);
            end;
          MOUT := MOUT + NAB[JI, 2] - NAB[JI, 1];
          inc(JI);
        end;
      Exit;
    end;

  //
  // Initialize for loop
  //
  // KF and KL have the following meaning:
  // Intervals 1,...,KF-1 have converged.
  // Intervals KF,...,KL  still need to be refined.
  //
  KF := 1;
  KL := MINP;

  //
  // If IJOB=2, initialize C.
  // If IJOB=3, use the user-supplied starting point.
  //
  if IJOB = 2 then
    begin
      JI := 1;
      while JI <= MINP do
        begin
          c[JI] := 0.5 * (AB[JI, 1] + AB[JI, 2]);
          inc(JI);
        end;
    end;

  //
  // Iteration loop
  //
  JIT := 1;
  while JIT <= NITMAX do
    begin

      //
      // Loop over intervals
      //
      //
      // Serial Version of the loop
      //
      KLNEW := KL;
      JI := KF;
      while JI <= KL do
        begin

          //
          // Compute N(w), the number of eigenvalues less than w
          //
          TMP1 := c[JI];
          TMP2 := d[1] - TMP1;
          ITMP1 := 0;
          if AP_FP_Less_Eq(TMP2, PIVMIN) then
            begin
              ITMP1 := 1;
              TMP2 := Min(TMP2, -PIVMIN);
            end;

          //
          // A series of compiler directives to defeat vectorization
          // for the next loop
          //
          // *$PL$ CMCHAR=' '
          // CDIR$          NEXTSCALAR
          // C$DIR          SCALAR
          // CDIR$          NEXT SCALAR
          // CVD$L          NOVECTOR
          // CDEC$          NOVECTOR
          // CVD$           NOVECTOR
          // *VDIR          NOVECTOR
          // *VOCL          LOOP,SCALAR
          // CIBM           PREFER SCALAR
          // *$PL$ CMCHAR='*'
          //
          j := 2;
          while j <= n do
            begin
              TMP2 := d[j] - e2[j - 1] / TMP2 - TMP1;
              if AP_FP_Less_Eq(TMP2, PIVMIN) then
                begin
                  ITMP1 := ITMP1 + 1;
                  TMP2 := Min(TMP2, -PIVMIN);
                end;
              inc(j);
            end;
          if IJOB <= 2 then
            begin

              //
              // IJOB=2: Choose all intervals containing eigenvalues.
              //
              // Insure that N(w) is monotone
              //
              ITMP1 := Min(NAB[JI, 2], Max(NAB[JI, 1], ITMP1));

              //
              // Update the Queue -- add intervals if both halves
              // contain eigenvalues.
              //
              if ITMP1 = NAB[JI, 2] then
                begin

                  //
                  // No eigenvalue in the upper interval:
                  // just use the lower interval.
                  //
                  AB[JI, 2] := TMP1;
                end
              else
                begin
                  if ITMP1 = NAB[JI, 1] then
                    begin

                      //
                      // No eigenvalue in the lower interval:
                      // just use the upper interval.
                      //
                      AB[JI, 1] := TMP1;
                    end
                  else
                    begin
                      if KLNEW < MMAX then
                        begin

                          //
                          // Eigenvalue in both intervals -- add upper to queue.
                          //
                          KLNEW := KLNEW + 1;
                          AB[KLNEW, 2] := AB[JI, 2];
                          NAB[KLNEW, 2] := NAB[JI, 2];
                          AB[KLNEW, 1] := TMP1;
                          NAB[KLNEW, 1] := ITMP1;
                          AB[JI, 2] := TMP1;
                          NAB[JI, 2] := ITMP1;
                        end
                      else
                        begin
                          Info := MMAX + 1;
                          Exit;
                        end;
                    end;
                end;
            end
          else
            begin

              //
              // IJOB=3: Binary search.  Keep only the interval
              // containing  w  s.t. N(w) = NVAL
              //
              if ITMP1 <= NVAL[JI] then
                begin
                  AB[JI, 1] := TMP1;
                  NAB[JI, 1] := ITMP1;
                end;
              if ITMP1 >= NVAL[JI] then
                begin
                  AB[JI, 2] := TMP1;
                  NAB[JI, 2] := ITMP1;
                end;
            end;
          inc(JI);
        end;
      KL := KLNEW;

      //
      // Check for convergence
      //
      KFNEW := KF;
      JI := KF;
      while JI <= KL do
        begin
          TMP1 := AbsReal(AB[JI, 2] - AB[JI, 1]);
          TMP2 := Max(AbsReal(AB[JI, 2]), AbsReal(AB[JI, 1]));
          if AP_FP_Less(TMP1, Max(ABSTOL, Max(PIVMIN, RELTOL * TMP2))) or
            (NAB[JI, 1] >= NAB[JI, 2]) then
            begin

              //
              // Converged -- Swap with position KFNEW,
              // then increment KFNEW
              //
              if JI > KFNEW then
                begin
                  TMP1 := AB[JI, 1];
                  TMP2 := AB[JI, 2];
                  ITMP1 := NAB[JI, 1];
                  ITMP2 := NAB[JI, 2];
                  AB[JI, 1] := AB[KFNEW, 1];
                  AB[JI, 2] := AB[KFNEW, 2];
                  NAB[JI, 1] := NAB[KFNEW, 1];
                  NAB[JI, 2] := NAB[KFNEW, 2];
                  AB[KFNEW, 1] := TMP1;
                  AB[KFNEW, 2] := TMP2;
                  NAB[KFNEW, 1] := ITMP1;
                  NAB[KFNEW, 2] := ITMP2;
                  if IJOB = 3 then
                    begin
                      ITMP1 := NVAL[JI];
                      NVAL[JI] := NVAL[KFNEW];
                      NVAL[KFNEW] := ITMP1;
                    end;
                end;
              KFNEW := KFNEW + 1;
            end;
          inc(JI);
        end;
      KF := KFNEW;

      //
      // Choose Midpoints
      //
      JI := KF;
      while JI <= KL do
        begin
          c[JI] := 0.5 * (AB[JI, 1] + AB[JI, 2]);
          inc(JI);
        end;

      //
      // If no more intervals to refine, quit.
      //
      if KF > KL then
        begin
          Break;
        end;
      inc(JIT);
    end;

  //
  // Converged
  //
  Info := Max(KL + 1 - KF, 0);
  MOUT := KL;
end;

(* ************************************************************************
  Internal subroutine
  ************************************************************************ *)
procedure InternalTREVC(const t: TLMatrix; n: TLInt;
  SIDE: TLInt; HOWMNY: TLInt; VSELECT: TLBVec;
  var vl: TLMatrix; var vr: TLMatrix; var M: TLInt;
  var Info: TLInt);
var
  ALLV: Boolean;
  BOTHV: Boolean;
  LEFTV: Boolean;
  OVER: Boolean;
  PAIR: Boolean;
  RIGHTV: Boolean;
  SOMEV: Boolean;
  i: TLInt;
  IERR: TLInt;
  II: TLInt;
  IP: TLInt;
  IIS: TLInt;
  j: TLInt;
  j1: TLInt;
  j2: TLInt;
  JNXT: TLInt;
  k: TLInt;
  KI: TLInt;
  n2: TLInt;
  beta: TLFloat;
  BIGNUM: TLFloat;
  EMAX: TLFloat;
  OVFL: TLFloat;
  REC: TLFloat;
  REMAX: TLFloat;
  SCL: TLFloat;
  SMIN: TLFloat;
  SMLNUM: TLFloat;
  ULP: TLFloat;
  UNFL: TLFloat;
  VCRIT: TLFloat;
  VMAX: TLFloat;
  WI: TLFloat;
  WR: TLFloat;
  XNORM: TLFloat;
  x: TLMatrix;
  Work: TLVec;
  Temp: TLVec;
  TEMP11: TLMatrix;
  TEMP22: TLMatrix;
  TEMP11B: TLMatrix;
  TEMP21B: TLMatrix;
  TEMP12B: TLMatrix;
  TEMP22B: TLMatrix;
  SkipFlag: Boolean;
  k1: TLInt;
  k2: TLInt;
  k3: TLInt;
  K4: TLInt;
  VT: TLFloat;
  RSWAP4: TLBVec;
  ZSWAP4: TLBVec;
  IPIVOT44: TLIMatrix;
  CIV4: TLVec;
  CRV4: TLVec;
  i_: TLInt;
  i1_: TLInt;
begin
  VSELECT := DynamicArrayCopy(VSELECT);
  SetLength(x, 2 + 1, 2 + 1);
  SetLength(TEMP11, 1 + 1, 1 + 1);
  SetLength(TEMP11B, 1 + 1, 1 + 1);
  SetLength(TEMP21B, 2 + 1, 1 + 1);
  SetLength(TEMP12B, 1 + 1, 2 + 1);
  SetLength(TEMP22B, 2 + 1, 2 + 1);
  SetLength(TEMP22, 2 + 1, 2 + 1);
  SetLength(Work, 3 * n + 1);
  SetLength(Temp, n + 1);
  SetLength(RSWAP4, 4 + 1);
  SetLength(ZSWAP4, 4 + 1);
  SetLength(IPIVOT44, 4 + 1, 4 + 1);
  SetLength(CIV4, 4 + 1);
  SetLength(CRV4, 4 + 1);
  if HOWMNY <> 1 then
    begin
      if (SIDE = 1) or (SIDE = 3) then
        begin
          SetLength(vr, n + 1, n + 1);
        end;
      if (SIDE = 2) or (SIDE = 3) then
        begin
          SetLength(vl, n + 1, n + 1);
        end;
    end;

  //
  // Decode and test the input parameters
  //
  BOTHV := SIDE = 3;
  RIGHTV := (SIDE = 1) or BOTHV;
  LEFTV := (SIDE = 2) or BOTHV;
  ALLV := HOWMNY = 2;
  OVER := HOWMNY = 1;
  SOMEV := HOWMNY = 3;
  Info := 0;
  if n < 0 then
    begin
      Info := -2;
      Exit;
    end;
  if not RIGHTV and not LEFTV then
    begin
      Info := -3;
      Exit;
    end;
  if not ALLV and not OVER and not SOMEV then
    begin
      Info := -4;
      Exit;
    end;

  //
  // Set M to the number of columns required to store the selected
  // eigenvectors, standardize the array SELECT if necessary, and
  // test MM.
  //
  if SOMEV then
    begin
      M := 0;
      PAIR := False;
      j := 1;
      while j <= n do
        begin
          if PAIR then
            begin
              PAIR := False;
              VSELECT[j] := False;
            end
          else
            begin
              if j < n then
                begin
                  if AP_FP_Eq(t[j + 1, j], 0) then
                    begin
                      if VSELECT[j] then
                        begin
                          M := M + 1;
                        end;
                    end
                  else
                    begin
                      PAIR := True;
                      if VSELECT[j] or VSELECT[j + 1] then
                        begin
                          VSELECT[j] := True;
                          M := M + 2;
                        end;
                    end;
                end
              else
                begin
                  if VSELECT[n] then
                    begin
                      M := M + 1;
                    end;
                end;
            end;
          inc(j);
        end;
    end
  else
    begin
      M := n;
    end;

  //
  // Quick return if possible.
  //
  if n = 0 then
    begin
      Exit;
    end;

  //
  // Set the constants to control overflow.
  //
  UNFL := MinRealNumber;
  OVFL := 1 / UNFL;
  ULP := MachineEpsilon;
  SMLNUM := UNFL * (n / ULP);
  BIGNUM := (1 - ULP) / SMLNUM;

  //
  // Compute 1-norm of each column of strictly upper triangular
  // part of T to control overflow in triangular solver.
  //
  Work[1] := 0;
  j := 2;
  while j <= n do
    begin
      Work[j] := 0;
      i := 1;
      while i <= j - 1 do
        begin
          Work[j] := Work[j] + AbsReal(t[i, j]);
          inc(i);
        end;
      inc(j);
    end;

  //
  // Index IP is used to specify the real or complex eigenvalue:
  // IP = 0, real eigenvalue,
  // 1, first of conjugate complex pair: (wr,wi)
  // -1, second of conjugate complex pair: (wr,wi)
  //
  n2 := 2 * n;
  if RIGHTV then
    begin

      //
      // Compute right eigenvectors.
      //
      IP := 0;
      IIS := M;
      KI := n;
      while KI >= 1 do
        begin
          SkipFlag := False;
          if IP = 1 then
            begin
              SkipFlag := True;
            end
          else
            begin
              if KI <> 1 then
                begin
                  if AP_FP_NEq(t[KI, KI - 1], 0) then
                    begin
                      IP := -1;
                    end;
                end;
              if SOMEV then
                begin
                  if IP = 0 then
                    begin
                      if not VSELECT[KI] then
                        begin
                          SkipFlag := True;
                        end;
                    end
                  else
                    begin
                      if not VSELECT[KI - 1] then
                        begin
                          SkipFlag := True;
                        end;
                    end;
                end;
            end;
          if not SkipFlag then
            begin

              //
              // Compute the KI-th eigenvalue (WR,WI).
              //
              WR := t[KI, KI];
              WI := 0;
              if IP <> 0 then
                begin
                  WI := Sqrt(AbsReal(t[KI, KI - 1])) * Sqrt(AbsReal(t[KI - 1, KI]));
                end;
              SMIN := Max(ULP * (AbsReal(WR) + AbsReal(WI)), SMLNUM);
              if IP = 0 then
                begin

                  //
                  // Real right eigenvector
                  //
                  Work[KI + n] := 1;

                  //
                  // Form right-hand side
                  //
                  k := 1;
                  while k <= KI - 1 do
                    begin
                      Work[k + n] := -t[k, KI];
                      inc(k);
                    end;

                  //
                  // Solve the upper quasi-triangular system:
                  // (T(1:KI-1,1:KI-1) - WR)*X = SCALE*WORK.
                  //
                  JNXT := KI - 1;
                  j := KI - 1;
                  while j >= 1 do
                    begin
                      if j > JNXT then
                        begin
                          dec(j);
                          Continue;
                        end;
                      j1 := j;
                      j2 := j;
                      JNXT := j - 1;
                      if j > 1 then
                        begin
                          if AP_FP_NEq(t[j, j - 1], 0) then
                            begin
                              j1 := j - 1;
                              JNXT := j - 2;
                            end;
                        end;
                      if j1 = j2 then
                        begin

                          //
                          // 1-by-1 diagonal block
                          //
                          TEMP11[1, 1] := t[j, j];
                          TEMP11B[1, 1] := Work[j + n];
                          InternalHSEVDLALN2(False, 1, 1, SMIN, 1, TEMP11, 1.0, 1.0,
                            TEMP11B, WR, 0.0, RSWAP4, ZSWAP4, IPIVOT44, CIV4, CRV4, x, SCL,
                            XNORM, IERR);

                          //
                          // Scale X(1,1) to avoid overflow when updating
                          // the right-hand side.
                          //
                          if AP_FP_Greater(XNORM, 1) then
                            begin
                              if AP_FP_Greater(Work[j], BIGNUM / XNORM) then
                                begin
                                  x[1, 1] := x[1, 1] / XNORM;
                                  SCL := SCL / XNORM;
                                end;
                            end;

                          //
                          // Scale if necessary
                          //
                          if AP_FP_NEq(SCL, 1) then
                            begin
                              k1 := n + 1;
                              k2 := n + KI;
                              APVMul(@Work[0], k1, k2, SCL);
                            end;
                          Work[j + n] := x[1, 1];

                          //
                          // Update right-hand side
                          //
                          k1 := 1 + n;
                          k2 := j - 1 + n;
                          k3 := j - 1;
                          VT := -x[1, 1];
                          i1_ := (1) - (k1);
                          for i_ := k1 to k2 do
                            begin
                              Work[i_] := Work[i_] + VT * t[i_ + i1_, j];
                            end;
                        end
                      else
                        begin

                          //
                          // 2-by-2 diagonal block
                          //
                          TEMP22[1, 1] := t[j - 1, j - 1];
                          TEMP22[1, 2] := t[j - 1, j];
                          TEMP22[2, 1] := t[j, j - 1];
                          TEMP22[2, 2] := t[j, j];
                          TEMP21B[1, 1] := Work[j - 1 + n];
                          TEMP21B[2, 1] := Work[j + n];
                          InternalHSEVDLALN2(False, 2, 1, SMIN, 1.0, TEMP22, 1.0, 1.0,
                            TEMP21B, WR, 0, RSWAP4, ZSWAP4, IPIVOT44, CIV4, CRV4, x, SCL,
                            XNORM, IERR);

                          //
                          // Scale X(1,1) and X(2,1) to avoid overflow when
                          // updating the right-hand side.
                          //
                          if AP_FP_Greater(XNORM, 1) then
                            begin
                              beta := Max(Work[j - 1], Work[j]);
                              if AP_FP_Greater(beta, BIGNUM / XNORM) then
                                begin
                                  x[1, 1] := x[1, 1] / XNORM;
                                  x[2, 1] := x[2, 1] / XNORM;
                                  SCL := SCL / XNORM;
                                end;
                            end;

                          //
                          // Scale if necessary
                          //
                          if AP_FP_NEq(SCL, 1) then
                            begin
                              k1 := 1 + n;
                              k2 := KI + n;
                              APVMul(@Work[0], k1, k2, SCL);
                            end;
                          Work[j - 1 + n] := x[1, 1];
                          Work[j + n] := x[2, 1];

                          //
                          // Update right-hand side
                          //
                          k1 := 1 + n;
                          k2 := j - 2 + n;
                          k3 := j - 2;
                          K4 := j - 1;
                          VT := -x[1, 1];
                          i1_ := (1) - (k1);
                          for i_ := k1 to k2 do
                            begin
                              Work[i_] := Work[i_] + VT * t[i_ + i1_, K4];
                            end;
                          VT := -x[2, 1];
                          i1_ := (1) - (k1);
                          for i_ := k1 to k2 do
                            begin
                              Work[i_] := Work[i_] + VT * t[i_ + i1_, j];
                            end;
                        end;
                      dec(j);
                    end;

                  //
                  // Copy the vector x or Q*x to VR and normalize.
                  //
                  if not OVER then
                    begin
                      k1 := 1 + n;
                      k2 := KI + n;
                      i1_ := (k1) - (1);
                      for i_ := 1 to KI do
                        begin
                          vr[i_, IIS] := Work[i_ + i1_];
                        end;
                      II := ColumnIdxAbsMax(vr, 1, KI, IIS);
                      REMAX := 1 / AbsReal(vr[II, IIS]);
                      for i_ := 1 to KI do
                        begin
                          vr[i_, IIS] := REMAX * vr[i_, IIS];
                        end;
                      k := KI + 1;
                      while k <= n do
                        begin
                          vr[k, IIS] := 0;
                          inc(k);
                        end;
                    end
                  else
                    begin
                      if KI > 1 then
                        begin
                          for i_ := 1 to n do
                            begin
                              Temp[i_] := vr[i_, KI];
                            end;
                          MatrixVectorMultiply(vr, 1, n, 1, KI - 1, False, Work, 1 + n,
                            KI - 1 + n, 1.0, Temp, 1, n, Work[KI + n]);
                          for i_ := 1 to n do
                            begin
                              vr[i_, KI] := Temp[i_];
                            end;
                        end;
                      II := ColumnIdxAbsMax(vr, 1, n, KI);
                      REMAX := 1 / AbsReal(vr[II, KI]);
                      for i_ := 1 to n do
                        begin
                          vr[i_, KI] := REMAX * vr[i_, KI];
                        end;
                    end;
                end
              else
                begin

                  //
                  // Complex right eigenvector.
                  //
                  // Initial solve
                  // [ (T(KI-1,KI-1) T(KI-1,KI) ) - (WR + I* WI)]*X = 0.
                  // [ (T(KI,KI-1)   T(KI,KI)   )               ]
                  //
                  if AP_FP_Greater_Eq(AbsReal(t[KI - 1, KI]), AbsReal(t[KI, KI - 1]))
                  then
                    begin
                      Work[KI - 1 + n] := 1;
                      Work[KI + n2] := WI / t[KI - 1, KI];
                    end
                  else
                    begin
                      Work[KI - 1 + n] := -WI / t[KI, KI - 1];
                      Work[KI + n2] := 1;
                    end;
                  Work[KI + n] := 0;
                  Work[KI - 1 + n2] := 0;

                  //
                  // Form right-hand side
                  //
                  k := 1;
                  while k <= KI - 2 do
                    begin
                      Work[k + n] := -Work[KI - 1 + n] * t[k, KI - 1];
                      Work[k + n2] := -Work[KI + n2] * t[k, KI];
                      inc(k);
                    end;

                  //
                  // Solve upper quasi-triangular system:
                  // (T(1:KI-2,1:KI-2) - (WR+i*WI))*X = SCALE*(WORK+i*WORK2)
                  //
                  JNXT := KI - 2;
                  j := KI - 2;
                  while j >= 1 do
                    begin
                      if j > JNXT then
                        begin
                          dec(j);
                          Continue;
                        end;
                      j1 := j;
                      j2 := j;
                      JNXT := j - 1;
                      if j > 1 then
                        begin
                          if AP_FP_NEq(t[j, j - 1], 0) then
                            begin
                              j1 := j - 1;
                              JNXT := j - 2;
                            end;
                        end;
                      if j1 = j2 then
                        begin

                          //
                          // 1-by-1 diagonal block
                          //
                          TEMP11[1, 1] := t[j, j];
                          TEMP12B[1, 1] := Work[j + n];
                          TEMP12B[1, 2] := Work[j + n + n];
                          InternalHSEVDLALN2(False, 1, 2, SMIN, 1.0, TEMP11, 1.0, 1.0,
                            TEMP12B, WR, WI, RSWAP4, ZSWAP4, IPIVOT44, CIV4, CRV4, x, SCL,
                            XNORM, IERR);

                          //
                          // Scale X(1,1) and X(1,2) to avoid overflow when
                          // updating the right-hand side.
                          //
                          if AP_FP_Greater(XNORM, 1) then
                            begin
                              if AP_FP_Greater(Work[j], BIGNUM / XNORM) then
                                begin
                                  x[1, 1] := x[1, 1] / XNORM;
                                  x[1, 2] := x[1, 2] / XNORM;
                                  SCL := SCL / XNORM;
                                end;
                            end;

                          //
                          // Scale if necessary
                          //
                          if AP_FP_NEq(SCL, 1) then
                            begin
                              k1 := 1 + n;
                              k2 := KI + n;
                              APVMul(@Work[0], k1, k2, SCL);
                              k1 := 1 + n2;
                              k2 := KI + n2;
                              APVMul(@Work[0], k1, k2, SCL);
                            end;
                          Work[j + n] := x[1, 1];
                          Work[j + n2] := x[1, 2];

                          //
                          // Update the right-hand side
                          //
                          k1 := 1 + n;
                          k2 := j - 1 + n;
                          k3 := 1;
                          K4 := j - 1;
                          VT := -x[1, 1];
                          i1_ := (k3) - (k1);
                          for i_ := k1 to k2 do
                            begin
                              Work[i_] := Work[i_] + VT * t[i_ + i1_, j];
                            end;
                          k1 := 1 + n2;
                          k2 := j - 1 + n2;
                          k3 := 1;
                          K4 := j - 1;
                          VT := -x[1, 2];
                          i1_ := (k3) - (k1);
                          for i_ := k1 to k2 do
                            begin
                              Work[i_] := Work[i_] + VT * t[i_ + i1_, j];
                            end;
                        end
                      else
                        begin

                          //
                          // 2-by-2 diagonal block
                          //
                          TEMP22[1, 1] := t[j - 1, j - 1];
                          TEMP22[1, 2] := t[j - 1, j];
                          TEMP22[2, 1] := t[j, j - 1];
                          TEMP22[2, 2] := t[j, j];
                          TEMP22B[1, 1] := Work[j - 1 + n];
                          TEMP22B[1, 2] := Work[j - 1 + n + n];
                          TEMP22B[2, 1] := Work[j + n];
                          TEMP22B[2, 2] := Work[j + n + n];
                          InternalHSEVDLALN2(False, 2, 2, SMIN, 1.0, TEMP22, 1.0, 1.0,
                            TEMP22B, WR, WI, RSWAP4, ZSWAP4, IPIVOT44, CIV4, CRV4, x, SCL,
                            XNORM, IERR);

                          //
                          // Scale X to avoid overflow when updating
                          // the right-hand side.
                          //
                          if AP_FP_Greater(XNORM, 1) then
                            begin
                              beta := Max(Work[j - 1], Work[j]);
                              if AP_FP_Greater(beta, BIGNUM / XNORM) then
                                begin
                                  REC := 1 / XNORM;
                                  x[1, 1] := x[1, 1] * REC;
                                  x[1, 2] := x[1, 2] * REC;
                                  x[2, 1] := x[2, 1] * REC;
                                  x[2, 2] := x[2, 2] * REC;
                                  SCL := SCL * REC;
                                end;
                            end;

                          //
                          // Scale if necessary
                          //
                          if AP_FP_NEq(SCL, 1) then
                            begin
                              APVMul(@Work[0], 1 + n, KI + n, SCL);
                              APVMul(@Work[0], 1 + n2, KI + n2, SCL);
                            end;
                          Work[j - 1 + n] := x[1, 1];
                          Work[j + n] := x[2, 1];
                          Work[j - 1 + n2] := x[1, 2];
                          Work[j + n2] := x[2, 2];

                          //
                          // Update the right-hand side
                          //
                          VT := -x[1, 1];
                          i1_ := (1) - (n + 1);
                          for i_ := n + 1 to n + j - 2 do
                            begin
                              Work[i_] := Work[i_] + VT * t[i_ + i1_, j - 1];
                            end;
                          VT := -x[2, 1];
                          i1_ := (1) - (n + 1);
                          for i_ := n + 1 to n + j - 2 do
                            begin
                              Work[i_] := Work[i_] + VT * t[i_ + i1_, j];
                            end;
                          VT := -x[1, 2];
                          i1_ := (1) - (n2 + 1);
                          for i_ := n2 + 1 to n2 + j - 2 do
                            begin
                              Work[i_] := Work[i_] + VT * t[i_ + i1_, j - 1];
                            end;
                          VT := -x[2, 2];
                          i1_ := (1) - (n2 + 1);
                          for i_ := n2 + 1 to n2 + j - 2 do
                            begin
                              Work[i_] := Work[i_] + VT * t[i_ + i1_, j];
                            end;
                        end;
                      dec(j);
                    end;

                  //
                  // Copy the vector x or Q*x to VR and normalize.
                  //
                  if not OVER then
                    begin
                      i1_ := (n + 1) - (1);
                      for i_ := 1 to KI do
                        begin
                          vr[i_, IIS - 1] := Work[i_ + i1_];
                        end;
                      i1_ := (n2 + 1) - (1);
                      for i_ := 1 to KI do
                        begin
                          vr[i_, IIS] := Work[i_ + i1_];
                        end;
                      EMAX := 0;
                      k := 1;
                      while k <= KI do
                        begin
                          EMAX := Max(EMAX, AbsReal(vr[k, IIS - 1]) + AbsReal(vr[k, IIS]));
                          inc(k);
                        end;
                      REMAX := 1 / EMAX;
                      for i_ := 1 to KI do
                        begin
                          vr[i_, IIS - 1] := REMAX * vr[i_, IIS - 1];
                        end;
                      for i_ := 1 to KI do
                        begin
                          vr[i_, IIS] := REMAX * vr[i_, IIS];
                        end;
                      k := KI + 1;
                      while k <= n do
                        begin
                          vr[k, IIS - 1] := 0;
                          vr[k, IIS] := 0;
                          inc(k);
                        end;
                    end
                  else
                    begin
                      if KI > 2 then
                        begin
                          for i_ := 1 to n do
                            begin
                              Temp[i_] := vr[i_, KI - 1];
                            end;
                          MatrixVectorMultiply(vr, 1, n, 1, KI - 2, False, Work, 1 + n,
                            KI - 2 + n, 1.0, Temp, 1, n, Work[KI - 1 + n]);
                          for i_ := 1 to n do
                            begin
                              vr[i_, KI - 1] := Temp[i_];
                            end;
                          for i_ := 1 to n do
                            begin
                              Temp[i_] := vr[i_, KI];
                            end;
                          MatrixVectorMultiply(vr, 1, n, 1, KI - 2, False, Work, 1 + n2,
                            KI - 2 + n2, 1.0, Temp, 1, n, Work[KI + n2]);
                          for i_ := 1 to n do
                            begin
                              vr[i_, KI] := Temp[i_];
                            end;
                        end
                      else
                        begin
                          VT := Work[KI - 1 + n];
                          for i_ := 1 to n do
                            begin
                              vr[i_, KI - 1] := VT * vr[i_, KI - 1];
                            end;
                          VT := Work[KI + n2];
                          for i_ := 1 to n do
                            begin
                              vr[i_, KI] := VT * vr[i_, KI];
                            end;
                        end;
                      EMAX := 0;
                      k := 1;
                      while k <= n do
                        begin
                          EMAX := Max(EMAX, AbsReal(vr[k, KI - 1]) + AbsReal(vr[k, KI]));
                          inc(k);
                        end;
                      REMAX := 1 / EMAX;
                      for i_ := 1 to n do
                        begin
                          vr[i_, KI - 1] := REMAX * vr[i_, KI - 1];
                        end;
                      for i_ := 1 to n do
                        begin
                          vr[i_, KI] := REMAX * vr[i_, KI];
                        end;
                    end;
                end;
              IIS := IIS - 1;
              if IP <> 0 then
                begin
                  IIS := IIS - 1;
                end;
            end;
          if IP = 1 then
            begin
              IP := 0;
            end;
          if IP = -1 then
            begin
              IP := 1;
            end;
          dec(KI);
        end;
    end;
  if LEFTV then
    begin

      //
      // Compute left eigenvectors.
      //
      IP := 0;
      IIS := 1;
      KI := 1;
      while KI <= n do
        begin
          SkipFlag := False;
          if IP = -1 then
            begin
              SkipFlag := True;
            end
          else
            begin
              if KI <> n then
                begin
                  if AP_FP_NEq(t[KI + 1, KI], 0) then
                    begin
                      IP := 1;
                    end;
                end;
              if SOMEV then
                begin
                  if not VSELECT[KI] then
                    begin
                      SkipFlag := True;
                    end;
                end;
            end;
          if not SkipFlag then
            begin

              //
              // Compute the KI-th eigenvalue (WR,WI).
              //
              WR := t[KI, KI];
              WI := 0;
              if IP <> 0 then
                begin
                  WI := Sqrt(AbsReal(t[KI, KI + 1])) * Sqrt(AbsReal(t[KI + 1, KI]));
                end;
              SMIN := Max(ULP * (AbsReal(WR) + AbsReal(WI)), SMLNUM);
              if IP = 0 then
                begin

                  //
                  // Real left eigenvector.
                  //
                  Work[KI + n] := 1;

                  //
                  // Form right-hand side
                  //
                  k := KI + 1;
                  while k <= n do
                    begin
                      Work[k + n] := -t[KI, k];
                      inc(k);
                    end;

                  //
                  // Solve the quasi-triangular system:
                  // (T(KI+1:N,KI+1:N) - WR)'*X = SCALE*WORK
                  //
                  VMAX := 1;
                  VCRIT := BIGNUM;
                  JNXT := KI + 1;
                  j := KI + 1;
                  while j <= n do
                    begin
                      if j < JNXT then
                        begin
                          inc(j);
                          Continue;
                        end;
                      j1 := j;
                      j2 := j;
                      JNXT := j + 1;
                      if j < n then
                        begin
                          if AP_FP_NEq(t[j + 1, j], 0) then
                            begin
                              j2 := j + 1;
                              JNXT := j + 2;
                            end;
                        end;
                      if j1 = j2 then
                        begin

                          //
                          // 1-by-1 diagonal block
                          //
                          // Scale if necessary to avoid overflow when forming
                          // the right-hand side.
                          //
                          if AP_FP_Greater(Work[j], VCRIT) then
                            begin
                              REC := 1 / VMAX;
                              APVMul(@Work[0], KI + n, n + n, REC);
                              VMAX := 1;
                              VCRIT := BIGNUM;
                            end;
                          i1_ := (KI + 1 + n) - (KI + 1);
                          VT := 0.0;
                          for i_ := KI + 1 to j - 1 do
                            begin
                              VT := VT + t[i_, j] * Work[i_ + i1_];
                            end;
                          Work[j + n] := Work[j + n] - VT;

                          //
                          // Solve (T(J,J)-WR)'*X = WORK
                          //
                          TEMP11[1, 1] := t[j, j];
                          TEMP11B[1, 1] := Work[j + n];
                          InternalHSEVDLALN2(False, 1, 1, SMIN, 1.0, TEMP11, 1.0, 1.0,
                            TEMP11B, WR, 0, RSWAP4, ZSWAP4, IPIVOT44, CIV4, CRV4, x, SCL,
                            XNORM, IERR);

                          //
                          // Scale if necessary
                          //
                          if AP_FP_NEq(SCL, 1) then
                            begin
                              APVMul(@Work[0], KI + n, n + n, SCL);
                            end;
                          Work[j + n] := x[1, 1];
                          VMAX := Max(AbsReal(Work[j + n]), VMAX);
                          VCRIT := BIGNUM / VMAX;
                        end
                      else
                        begin

                          //
                          // 2-by-2 diagonal block
                          //
                          // Scale if necessary to avoid overflow when forming
                          // the right-hand side.
                          //
                          beta := Max(Work[j], Work[j + 1]);
                          if AP_FP_Greater(beta, VCRIT) then
                            begin
                              REC := 1 / VMAX;
                              APVMul(@Work[0], KI + n, n + n, REC);
                              VMAX := 1;
                              VCRIT := BIGNUM;
                            end;
                          i1_ := (KI + 1 + n) - (KI + 1);
                          VT := 0.0;
                          for i_ := KI + 1 to j - 1 do
                            begin
                              VT := VT + t[i_, j] * Work[i_ + i1_];
                            end;
                          Work[j + n] := Work[j + n] - VT;
                          i1_ := (KI + 1 + n) - (KI + 1);
                          VT := 0.0;
                          for i_ := KI + 1 to j - 1 do
                            begin
                              VT := VT + t[i_, j + 1] * Work[i_ + i1_];
                            end;
                          Work[j + 1 + n] := Work[j + 1 + n] - VT;

                          //
                          // Solve
                          // [T(J,J)-WR   T(J,J+1)     ]'* X = SCALE*( WORK1 )
                          // [T(J+1,J)    T(J+1,J+1)-WR]             ( WORK2 )
                          //
                          TEMP22[1, 1] := t[j, j];
                          TEMP22[1, 2] := t[j, j + 1];
                          TEMP22[2, 1] := t[j + 1, j];
                          TEMP22[2, 2] := t[j + 1, j + 1];
                          TEMP21B[1, 1] := Work[j + n];
                          TEMP21B[2, 1] := Work[j + 1 + n];
                          InternalHSEVDLALN2(True, 2, 1, SMIN, 1.0, TEMP22, 1.0, 1.0,
                            TEMP21B, WR, 0, RSWAP4, ZSWAP4, IPIVOT44, CIV4, CRV4, x, SCL,
                            XNORM, IERR);

                          //
                          // Scale if necessary
                          //
                          if AP_FP_NEq(SCL, 1) then
                            begin
                              APVMul(@Work[0], KI + n, n + n, SCL);
                            end;
                          Work[j + n] := x[1, 1];
                          Work[j + 1 + n] := x[2, 1];
                          VMAX := Max(AbsReal(Work[j + n]),
                            Max(AbsReal(Work[j + 1 + n]), VMAX));
                          VCRIT := BIGNUM / VMAX;
                        end;
                      inc(j);
                    end;

                  //
                  // Copy the vector x or Q*x to VL and normalize.
                  //
                  if not OVER then
                    begin
                      i1_ := (KI + n) - (KI);
                      for i_ := KI to n do
                        begin
                          vl[i_, IIS] := Work[i_ + i1_];
                        end;
                      II := ColumnIdxAbsMax(vl, KI, n, IIS);
                      REMAX := 1 / AbsReal(vl[II, IIS]);
                      for i_ := KI to n do
                        begin
                          vl[i_, IIS] := REMAX * vl[i_, IIS];
                        end;
                      k := 1;
                      while k <= KI - 1 do
                        begin
                          vl[k, IIS] := 0;
                          inc(k);
                        end;
                    end
                  else
                    begin
                      if KI < n then
                        begin
                          for i_ := 1 to n do
                            begin
                              Temp[i_] := vl[i_, KI];
                            end;
                          MatrixVectorMultiply(vl, 1, n, KI + 1, n, False, Work, KI + 1 + n,
                            n + n, 1.0, Temp, 1, n, Work[KI + n]);
                          for i_ := 1 to n do
                            begin
                              vl[i_, KI] := Temp[i_];
                            end;
                        end;
                      II := ColumnIdxAbsMax(vl, 1, n, KI);
                      REMAX := 1 / AbsReal(vl[II, KI]);
                      for i_ := 1 to n do
                        begin
                          vl[i_, KI] := REMAX * vl[i_, KI];
                        end;
                    end;
                end
              else
                begin

                  //
                  // Complex left eigenvector.
                  //
                  // Initial solve:
                  // ((T(KI,KI)    T(KI,KI+1) )' - (WR - I* WI))*X = 0.
                  // ((T(KI+1,KI) T(KI+1,KI+1))                )
                  //
                  if AP_FP_Greater_Eq(AbsReal(t[KI, KI + 1]), AbsReal(t[KI + 1, KI]))
                  then
                    begin
                      Work[KI + n] := WI / t[KI, KI + 1];
                      Work[KI + 1 + n2] := 1;
                    end
                  else
                    begin
                      Work[KI + n] := 1;
                      Work[KI + 1 + n2] := -WI / t[KI + 1, KI];
                    end;
                  Work[KI + 1 + n] := 0;
                  Work[KI + n2] := 0;

                  //
                  // Form right-hand side
                  //
                  k := KI + 2;
                  while k <= n do
                    begin
                      Work[k + n] := -Work[KI + n] * t[KI, k];
                      Work[k + n2] := -Work[KI + 1 + n2] * t[KI + 1, k];
                      inc(k);
                    end;

                  //
                  // Solve complex quasi-triangular system:
                  // ( T(KI+2,N:KI+2,N) - (WR-i*WI) )*X = WORK1+i*WORK2
                  //
                  VMAX := 1;
                  VCRIT := BIGNUM;
                  JNXT := KI + 2;
                  j := KI + 2;
                  while j <= n do
                    begin
                      if j < JNXT then
                        begin
                          inc(j);
                          Continue;
                        end;
                      j1 := j;
                      j2 := j;
                      JNXT := j + 1;
                      if j < n then
                        begin
                          if AP_FP_NEq(t[j + 1, j], 0) then
                            begin
                              j2 := j + 1;
                              JNXT := j + 2;
                            end;
                        end;
                      if j1 = j2 then
                        begin

                          //
                          // 1-by-1 diagonal block
                          //
                          // Scale if necessary to avoid overflow when
                          // forming the right-hand side elements.
                          //
                          if AP_FP_Greater(Work[j], VCRIT) then
                            begin
                              REC := 1 / VMAX;
                              APVMul(@Work[0], KI + n, n + n, REC);
                              APVMul(@Work[0], KI + n2, n + n2, REC);
                              VMAX := 1;
                              VCRIT := BIGNUM;
                            end;
                          i1_ := (KI + 2 + n) - (KI + 2);
                          VT := 0.0;
                          for i_ := KI + 2 to j - 1 do
                            begin
                              VT := VT + t[i_, j] * Work[i_ + i1_];
                            end;
                          Work[j + n] := Work[j + n] - VT;
                          i1_ := (KI + 2 + n2) - (KI + 2);
                          VT := 0.0;
                          for i_ := KI + 2 to j - 1 do
                            begin
                              VT := VT + t[i_, j] * Work[i_ + i1_];
                            end;
                          Work[j + n2] := Work[j + n2] - VT;

                          //
                          // Solve (T(J,J)-(WR-i*WI))*(X11+i*X12)= WK+I*WK2
                          //
                          TEMP11[1, 1] := t[j, j];
                          TEMP12B[1, 1] := Work[j + n];
                          TEMP12B[1, 2] := Work[j + n + n];
                          InternalHSEVDLALN2(False, 1, 2, SMIN, 1.0, TEMP11, 1.0, 1.0,
                            TEMP12B, WR, -WI, RSWAP4, ZSWAP4, IPIVOT44, CIV4, CRV4, x, SCL,
                            XNORM, IERR);

                          //
                          // Scale if necessary
                          //
                          if AP_FP_NEq(SCL, 1) then
                            begin
                              APVMul(@Work[0], KI + n, n + n, SCL);
                              APVMul(@Work[0], KI + n2, n + n2, SCL);
                            end;
                          Work[j + n] := x[1, 1];
                          Work[j + n2] := x[1, 2];
                          VMAX := Max(AbsReal(Work[j + n]),
                            Max(AbsReal(Work[j + n2]), VMAX));
                          VCRIT := BIGNUM / VMAX;
                        end
                      else
                        begin

                          //
                          // 2-by-2 diagonal block
                          //
                          // Scale if necessary to avoid overflow when forming
                          // the right-hand side elements.
                          //
                          beta := Max(Work[j], Work[j + 1]);
                          if AP_FP_Greater(beta, VCRIT) then
                            begin
                              REC := 1 / VMAX;
                              APVMul(@Work[0], KI + n, n + n, REC);
                              APVMul(@Work[0], KI + n2, n + n2, REC);
                              VMAX := 1;
                              VCRIT := BIGNUM;
                            end;
                          i1_ := (KI + 2 + n) - (KI + 2);
                          VT := 0.0;
                          for i_ := KI + 2 to j - 1 do
                            begin
                              VT := VT + t[i_, j] * Work[i_ + i1_];
                            end;
                          Work[j + n] := Work[j + n] - VT;
                          i1_ := (KI + 2 + n2) - (KI + 2);
                          VT := 0.0;
                          for i_ := KI + 2 to j - 1 do
                            begin
                              VT := VT + t[i_, j] * Work[i_ + i1_];
                            end;
                          Work[j + n2] := Work[j + n2] - VT;
                          i1_ := (KI + 2 + n) - (KI + 2);
                          VT := 0.0;
                          for i_ := KI + 2 to j - 1 do
                            begin
                              VT := VT + t[i_, j + 1] * Work[i_ + i1_];
                            end;
                          Work[j + 1 + n] := Work[j + 1 + n] - VT;
                          i1_ := (KI + 2 + n2) - (KI + 2);
                          VT := 0.0;
                          for i_ := KI + 2 to j - 1 do
                            begin
                              VT := VT + t[i_, j + 1] * Work[i_ + i1_];
                            end;
                          Work[j + 1 + n2] := Work[j + 1 + n2] - VT;

                          //
                          // Solve 2-by-2 complex linear equation
                          // ([T(j,j)   T(j,j+1)  ]'-(wr-i*wi)*I)*X = SCALE*B
                          // ([T(j+1,j) T(j+1,j+1)]             )
                          //
                          TEMP22[1, 1] := t[j, j];
                          TEMP22[1, 2] := t[j, j + 1];
                          TEMP22[2, 1] := t[j + 1, j];
                          TEMP22[2, 2] := t[j + 1, j + 1];
                          TEMP22B[1, 1] := Work[j + n];
                          TEMP22B[1, 2] := Work[j + n + n];
                          TEMP22B[2, 1] := Work[j + 1 + n];
                          TEMP22B[2, 2] := Work[j + 1 + n + n];
                          InternalHSEVDLALN2(True, 2, 2, SMIN, 1.0, TEMP22, 1.0, 1.0,
                            TEMP22B, WR, -WI, RSWAP4, ZSWAP4, IPIVOT44, CIV4, CRV4, x, SCL,
                            XNORM, IERR);

                          //
                          // Scale if necessary
                          //
                          if AP_FP_NEq(SCL, 1) then
                            begin
                              APVMul(@Work[0], KI + n, n + n, SCL);
                              APVMul(@Work[0], KI + n2, n + n2, SCL);
                            end;
                          Work[j + n] := x[1, 1];
                          Work[j + n2] := x[1, 2];
                          Work[j + 1 + n] := x[2, 1];
                          Work[j + 1 + n2] := x[2, 2];
                          VMAX := Max(AbsReal(x[1, 1]), VMAX);
                          VMAX := Max(AbsReal(x[1, 2]), VMAX);
                          VMAX := Max(AbsReal(x[2, 1]), VMAX);
                          VMAX := Max(AbsReal(x[2, 2]), VMAX);
                          VCRIT := BIGNUM / VMAX;
                        end;
                      inc(j);
                    end;

                  //
                  // Copy the vector x or Q*x to VL and normalize.
                  //
                  if not OVER then
                    begin
                      i1_ := (KI + n) - (KI);
                      for i_ := KI to n do
                        begin
                          vl[i_, IIS] := Work[i_ + i1_];
                        end;
                      i1_ := (KI + n2) - (KI);
                      for i_ := KI to n do
                        begin
                          vl[i_, IIS + 1] := Work[i_ + i1_];
                        end;
                      EMAX := 0;
                      k := KI;
                      while k <= n do
                        begin
                          EMAX := Max(EMAX, AbsReal(vl[k, IIS]) + AbsReal(vl[k, IIS + 1]));
                          inc(k);
                        end;
                      REMAX := 1 / EMAX;
                      for i_ := KI to n do
                        begin
                          vl[i_, IIS] := REMAX * vl[i_, IIS];
                        end;
                      for i_ := KI to n do
                        begin
                          vl[i_, IIS + 1] := REMAX * vl[i_, IIS + 1];
                        end;
                      k := 1;
                      while k <= KI - 1 do
                        begin
                          vl[k, IIS] := 0;
                          vl[k, IIS + 1] := 0;
                          inc(k);
                        end;
                    end
                  else
                    begin
                      if KI < n - 1 then
                        begin
                          for i_ := 1 to n do
                            begin
                              Temp[i_] := vl[i_, KI];
                            end;
                          MatrixVectorMultiply(vl, 1, n, KI + 2, n, False, Work, KI + 2 + n,
                            n + n, 1.0, Temp, 1, n, Work[KI + n]);
                          for i_ := 1 to n do
                            begin
                              vl[i_, KI] := Temp[i_];
                            end;
                          for i_ := 1 to n do
                            begin
                              Temp[i_] := vl[i_, KI + 1];
                            end;
                          MatrixVectorMultiply(vl, 1, n, KI + 2, n, False, Work,
                            KI + 2 + n2, n + n2, 1.0, Temp, 1, n, Work[KI + 1 + n2]);
                          for i_ := 1 to n do
                            begin
                              vl[i_, KI + 1] := Temp[i_];
                            end;
                        end
                      else
                        begin
                          VT := Work[KI + n];
                          for i_ := 1 to n do
                            begin
                              vl[i_, KI] := VT * vl[i_, KI];
                            end;
                          VT := Work[KI + 1 + n2];
                          for i_ := 1 to n do
                            begin
                              vl[i_, KI + 1] := VT * vl[i_, KI + 1];
                            end;
                        end;
                      EMAX := 0;
                      k := 1;
                      while k <= n do
                        begin
                          EMAX := Max(EMAX, AbsReal(vl[k, KI]) + AbsReal(vl[k, KI + 1]));
                          inc(k);
                        end;
                      REMAX := 1 / EMAX;
                      for i_ := 1 to n do
                        begin
                          vl[i_, KI] := REMAX * vl[i_, KI];
                        end;
                      for i_ := 1 to n do
                        begin
                          vl[i_, KI + 1] := REMAX * vl[i_, KI + 1];
                        end;
                    end;
                end;
              IIS := IIS + 1;
              if IP <> 0 then
                begin
                  IIS := IIS + 1;
                end;
            end;
          if IP = -1 then
            begin
              IP := 0;
            end;
          if IP = 1 then
            begin
              IP := -1;
            end;
          inc(KI);
        end;
    end;
end;

(* ************************************************************************
  DLALN2 solves a system of the form  (ca A - w D ) X = s B
  or (ca A' - w D) X = s B   with possible scaling ("s") and
  perturbation of A.  (A' means A-transpose.)

  A is an NA x NA real matrix, ca is a real scalar, D is an NA x NA
  real diagonal matrix, w is a real or complex value, and X and B are
  NA x 1 matrices -- real if w is real, complex if w is complex.  NA
  may be 1 or 2.

  If w is complex, X and B are represented as NA x 2 matrices,
  the first column of each being the real part and the second
  being the imaginary part.

  "s" is a scaling factor (.LE. 1), computed by DLALN2, which is
  so chosen that X can be computed without overflow.  X is further
  scaled if necessary to assure that norm(ca A - w D)*norm(X) is less
  than overflow.

  If both singular values of (ca A - w D) are less than SMIN,
  SMIN*identity will be used instead of (ca A - w D).  If only one
  singular value is less than SMIN, one element of (ca A - w D) will be
  perturbed enough to make the smallest singular value roughly SMIN.
  If both singular values are at least SMIN, (ca A - w D) will not be
  perturbed.  In any case, the perturbation will be at most some small
  multiple of max( SMIN, ulp*norm(ca A - w D) ).  The singular values
  are computed by infinity-norm approximations, and thus will only be
  correct to a factor of 2 or so.

  Note: all input quantities are assumed to be smaller than overflow
  by a reasonable factor. (See BIGNUM.)
  ************************************************************************ *)
procedure InternalHSEVDLALN2(const LTRANS: Boolean; const NA: TLInt;
  const NW: TLInt; const SMIN: TLFloat; const ca: TLFloat;
  const a: TLMatrix; const d1: TLFloat; const d2: TLFloat;
  const b: TLMatrix; const WR: TLFloat; const WI: TLFloat;
  var RSWAP4: TLBVec; var ZSWAP4: TLBVec;
  var IPIVOT44: TLIMatrix; var CIV4: TLVec; var CRV4: TLVec;
  var x: TLMatrix; var SCL: TLFloat; var XNORM: TLFloat;
  var Info: TLInt);
var
  ICMAX: TLInt;
  j: TLInt;
  BBND: TLFloat;
  BI1: TLFloat;
  BI2: TLFloat;
  BIGNUM: TLFloat;
  BNORM: TLFloat;
  BR1: TLFloat;
  BR2: TLFloat;
  CI21: TLFloat;
  CI22: TLFloat;
  CMAX: TLFloat;
  CNORM: TLFloat;
  CR21: TLFloat;
  CR22: TLFloat;
  CSI: TLFloat;
  CSR: TLFloat;
  LI21: TLFloat;
  LR21: TLFloat;
  SMINI: TLFloat;
  SMLNUM: TLFloat;
  Temp: TLFloat;
  U22ABS: TLFloat;
  UI11: TLFloat;
  UI11R: TLFloat;
  UI12: TLFloat;
  UI12S: TLFloat;
  UI22: TLFloat;
  UR11: TLFloat;
  UR11R: TLFloat;
  UR12: TLFloat;
  UR12S: TLFloat;
  UR22: TLFloat;
  XI1: TLFloat;
  XI2: TLFloat;
  XR1: TLFloat;
  XR2: TLFloat;
  TMP1: TLFloat;
  TMP2: TLFloat;
begin
  ZSWAP4[1] := False;
  ZSWAP4[2] := False;
  ZSWAP4[3] := True;
  ZSWAP4[4] := True;
  RSWAP4[1] := False;
  RSWAP4[2] := True;
  RSWAP4[3] := False;
  RSWAP4[4] := True;
  IPIVOT44[1, 1] := 1;
  IPIVOT44[2, 1] := 2;
  IPIVOT44[3, 1] := 3;
  IPIVOT44[4, 1] := 4;
  IPIVOT44[1, 2] := 2;
  IPIVOT44[2, 2] := 1;
  IPIVOT44[3, 2] := 4;
  IPIVOT44[4, 2] := 3;
  IPIVOT44[1, 3] := 3;
  IPIVOT44[2, 3] := 4;
  IPIVOT44[3, 3] := 1;
  IPIVOT44[4, 3] := 2;
  IPIVOT44[1, 4] := 4;
  IPIVOT44[2, 4] := 3;
  IPIVOT44[3, 4] := 2;
  IPIVOT44[4, 4] := 1;
  SMLNUM := 2 * MinRealNumber;
  BIGNUM := 1 / SMLNUM;
  SMINI := Max(SMIN, SMLNUM);

  //
  // Don't check for input errors
  //
  Info := 0;

  //
  // Standard Initializations
  //
  SCL := 1;
  if NA = 1 then
    begin

      //
      // 1 x 1  (i.e., scalar) system   C X = B
      //
      if NW = 1 then
        begin

          //
          // Real 1x1 system.
          //
          // C = ca A - w D
          //
          CSR := ca * a[1, 1] - WR * d1;
          CNORM := AbsReal(CSR);

          //
          // If | C | < SMINI, use C = SMINI
          //
          if AP_FP_Less(CNORM, SMINI) then
            begin
              CSR := SMINI;
              CNORM := SMINI;
              Info := 1;
            end;

          //
          // Check scaling for  X = B / C
          //
          BNORM := AbsReal(b[1, 1]);
          if AP_FP_Less(CNORM, 1) and AP_FP_Greater(BNORM, 1) then
            begin
              if AP_FP_Greater(BNORM, BIGNUM * CNORM) then
                begin
                  SCL := 1 / BNORM;
                end;
            end;

          //
          // Compute X
          //
          x[1, 1] := b[1, 1] * SCL / CSR;
          XNORM := AbsReal(x[1, 1]);
        end
      else
        begin

          //
          // Complex 1x1 system (w is complex)
          //
          // C = ca A - w D
          //
          CSR := ca * a[1, 1] - WR * d1;
          CSI := -WI * d1;
          CNORM := AbsReal(CSR) + AbsReal(CSI);

          //
          // If | C | < SMINI, use C = SMINI
          //
          if AP_FP_Less(CNORM, SMINI) then
            begin
              CSR := SMINI;
              CSI := 0;
              CNORM := SMINI;
              Info := 1;
            end;

          //
          // Check scaling for  X = B / C
          //
          BNORM := AbsReal(b[1, 1]) + AbsReal(b[1, 2]);
          if AP_FP_Less(CNORM, 1) and AP_FP_Greater(BNORM, 1) then
            begin
              if AP_FP_Greater(BNORM, BIGNUM * CNORM) then
                begin
                  SCL := 1 / BNORM;
                end;
            end;

          //
          // Compute X
          //
          InternalHSEVDLADIV(SCL * b[1, 1], SCL * b[1, 2], CSR, CSI, TMP1, TMP2);
          x[1, 1] := TMP1;
          x[1, 2] := TMP2;
          XNORM := AbsReal(x[1, 1]) + AbsReal(x[1, 2]);
        end;
    end
  else
    begin

      //
      // 2x2 System
      //
      // Compute the real part of  C = ca A - w D  (or  ca A' - w D )
      //
      CRV4[1 + 0] := ca * a[1, 1] - WR * d1;
      CRV4[2 + 2] := ca * a[2, 2] - WR * d2;
      if LTRANS then
        begin
          CRV4[1 + 2] := ca * a[2, 1];
          CRV4[2 + 0] := ca * a[1, 2];
        end
      else
        begin
          CRV4[2 + 0] := ca * a[2, 1];
          CRV4[1 + 2] := ca * a[1, 2];
        end;
      if NW = 1 then
        begin

          //
          // Real 2x2 system  (w is real)
          //
          // Find the largest element in C
          //
          CMAX := 0;
          ICMAX := 0;
          j := 1;
          while j <= 4 do
            begin
              if AP_FP_Greater(AbsReal(CRV4[j]), CMAX) then
                begin
                  CMAX := AbsReal(CRV4[j]);
                  ICMAX := j;
                end;
              inc(j);
            end;

          //
          // If norm(C) < SMINI, use SMINI*identity.
          //
          if AP_FP_Less(CMAX, SMINI) then
            begin
              BNORM := Max(AbsReal(b[1, 1]), AbsReal(b[2, 1]));
              if AP_FP_Less(SMINI, 1) and AP_FP_Greater(BNORM, 1) then
                begin
                  if AP_FP_Greater(BNORM, BIGNUM * SMINI) then
                    begin
                      SCL := 1 / BNORM;
                    end;
                end;
              Temp := SCL / SMINI;
              x[1, 1] := Temp * b[1, 1];
              x[2, 1] := Temp * b[2, 1];
              XNORM := Temp * BNORM;
              Info := 1;
              Exit;
            end;

          //
          // Gaussian elimination with complete pivoting.
          //
          UR11 := CRV4[ICMAX];
          CR21 := CRV4[IPIVOT44[2, ICMAX]];
          UR12 := CRV4[IPIVOT44[3, ICMAX]];
          CR22 := CRV4[IPIVOT44[4, ICMAX]];
          UR11R := 1 / UR11;
          LR21 := UR11R * CR21;
          UR22 := CR22 - UR12 * LR21;

          //
          // If smaller pivot < SMINI, use SMINI
          //
          if AP_FP_Less(AbsReal(UR22), SMINI) then
            begin
              UR22 := SMINI;
              Info := 1;
            end;
          if RSWAP4[ICMAX] then
            begin
              BR1 := b[2, 1];
              BR2 := b[1, 1];
            end
          else
            begin
              BR1 := b[1, 1];
              BR2 := b[2, 1];
            end;
          BR2 := BR2 - LR21 * BR1;
          BBND := Max(AbsReal(BR1 * (UR22 * UR11R)), AbsReal(BR2));
          if AP_FP_Greater(BBND, 1) and AP_FP_Less(AbsReal(UR22), 1) then
            begin
              if AP_FP_Greater_Eq(BBND, BIGNUM * AbsReal(UR22)) then
                begin
                  SCL := 1 / BBND;
                end;
            end;
          XR2 := BR2 * SCL / UR22;
          XR1 := SCL * BR1 * UR11R - XR2 * (UR11R * UR12);
          if ZSWAP4[ICMAX] then
            begin
              x[1, 1] := XR2;
              x[2, 1] := XR1;
            end
          else
            begin
              x[1, 1] := XR1;
              x[2, 1] := XR2;
            end;
          XNORM := Max(AbsReal(XR1), AbsReal(XR2));

          //
          // Further scaling if  norm(A) norm(X) > overflow
          //
          if AP_FP_Greater(XNORM, 1) and AP_FP_Greater(CMAX, 1) then
            begin
              if AP_FP_Greater(XNORM, BIGNUM / CMAX) then
                begin
                  Temp := CMAX / BIGNUM;
                  x[1, 1] := Temp * x[1, 1];
                  x[2, 1] := Temp * x[2, 1];
                  XNORM := Temp * XNORM;
                  SCL := Temp * SCL;
                end;
            end;
        end
      else
        begin

          //
          // Complex 2x2 system  (w is complex)
          //
          // Find the largest element in C
          //
          CIV4[1 + 0] := -WI * d1;
          CIV4[2 + 0] := 0;
          CIV4[1 + 2] := 0;
          CIV4[2 + 2] := -WI * d2;
          CMAX := 0;
          ICMAX := 0;
          j := 1;
          while j <= 4 do
            begin
              if AP_FP_Greater(AbsReal(CRV4[j]) + AbsReal(CIV4[j]), CMAX) then
                begin
                  CMAX := AbsReal(CRV4[j]) + AbsReal(CIV4[j]);
                  ICMAX := j;
                end;
              inc(j);
            end;

          //
          // If norm(C) < SMINI, use SMINI*identity.
          //
          if AP_FP_Less(CMAX, SMINI) then
            begin
              BNORM := Max(AbsReal(b[1, 1]) + AbsReal(b[1, 2]),
                AbsReal(b[2, 1]) + AbsReal(b[2, 2]));
              if AP_FP_Less(SMINI, 1) and AP_FP_Greater(BNORM, 1) then
                begin
                  if AP_FP_Greater(BNORM, BIGNUM * SMINI) then
                    begin
                      SCL := 1 / BNORM;
                    end;
                end;
              Temp := SCL / SMINI;
              x[1, 1] := Temp * b[1, 1];
              x[2, 1] := Temp * b[2, 1];
              x[1, 2] := Temp * b[1, 2];
              x[2, 2] := Temp * b[2, 2];
              XNORM := Temp * BNORM;
              Info := 1;
              Exit;
            end;

          //
          // Gaussian elimination with complete pivoting.
          //
          UR11 := CRV4[ICMAX];
          UI11 := CIV4[ICMAX];
          CR21 := CRV4[IPIVOT44[2, ICMAX]];
          CI21 := CIV4[IPIVOT44[2, ICMAX]];
          UR12 := CRV4[IPIVOT44[3, ICMAX]];
          UI12 := CIV4[IPIVOT44[3, ICMAX]];
          CR22 := CRV4[IPIVOT44[4, ICMAX]];
          CI22 := CIV4[IPIVOT44[4, ICMAX]];
          if (ICMAX = 1) or (ICMAX = 4) then
            begin

              //
              // Code when off-diagonals of pivoted C are real
              //
              if AP_FP_Greater(AbsReal(UR11), AbsReal(UI11)) then
                begin
                  Temp := UI11 / UR11;
                  UR11R := 1 / (UR11 * (1 + AP_Sqr(Temp)));
                  UI11R := -Temp * UR11R;
                end
              else
                begin
                  Temp := UR11 / UI11;
                  UI11R := -1 / (UI11 * (1 + AP_Sqr(Temp)));
                  UR11R := -Temp * UI11R;
                end;
              LR21 := CR21 * UR11R;
              LI21 := CR21 * UI11R;
              UR12S := UR12 * UR11R;
              UI12S := UR12 * UI11R;
              UR22 := CR22 - UR12 * LR21;
              UI22 := CI22 - UR12 * LI21;
            end
          else
            begin

              //
              // Code when diagonals of pivoted C are real
              //
              UR11R := 1 / UR11;
              UI11R := 0;
              LR21 := CR21 * UR11R;
              LI21 := CI21 * UR11R;
              UR12S := UR12 * UR11R;
              UI12S := UI12 * UR11R;
              UR22 := CR22 - UR12 * LR21 + UI12 * LI21;
              UI22 := -UR12 * LI21 - UI12 * LR21;
            end;
          U22ABS := AbsReal(UR22) + AbsReal(UI22);

          //
          // If smaller pivot < SMINI, use SMINI
          //
          if AP_FP_Less(U22ABS, SMINI) then
            begin
              UR22 := SMINI;
              UI22 := 0;
              Info := 1;
            end;
          if RSWAP4[ICMAX] then
            begin
              BR2 := b[1, 1];
              BR1 := b[2, 1];
              BI2 := b[1, 2];
              BI1 := b[2, 2];
            end
          else
            begin
              BR1 := b[1, 1];
              BR2 := b[2, 1];
              BI1 := b[1, 2];
              BI2 := b[2, 2];
            end;
          BR2 := BR2 - LR21 * BR1 + LI21 * BI1;
          BI2 := BI2 - LI21 * BR1 - LR21 * BI1;
          BBND := Max((AbsReal(BR1) + AbsReal(BI1)) *
            (U22ABS * (AbsReal(UR11R) + AbsReal(UI11R))),
            AbsReal(BR2) + AbsReal(BI2));
          if AP_FP_Greater(BBND, 1) and AP_FP_Less(U22ABS, 1) then
            begin
              if AP_FP_Greater_Eq(BBND, BIGNUM * U22ABS) then
                begin
                  SCL := 1 / BBND;
                  BR1 := SCL * BR1;
                  BI1 := SCL * BI1;
                  BR2 := SCL * BR2;
                  BI2 := SCL * BI2;
                end;
            end;
          InternalHSEVDLADIV(BR2, BI2, UR22, UI22, XR2, XI2);
          XR1 := UR11R * BR1 - UI11R * BI1 - UR12S * XR2 + UI12S * XI2;
          XI1 := UI11R * BR1 + UR11R * BI1 - UI12S * XR2 - UR12S * XI2;
          if ZSWAP4[ICMAX] then
            begin
              x[1, 1] := XR2;
              x[2, 1] := XR1;
              x[1, 2] := XI2;
              x[2, 2] := XI1;
            end
          else
            begin
              x[1, 1] := XR1;
              x[2, 1] := XR2;
              x[1, 2] := XI1;
              x[2, 2] := XI2;
            end;
          XNORM := Max(AbsReal(XR1) + AbsReal(XI1), AbsReal(XR2) + AbsReal(XI2));

          //
          // Further scaling if  norm(A) norm(X) > overflow
          //
          if AP_FP_Greater(XNORM, 1) and AP_FP_Greater(CMAX, 1) then
            begin
              if AP_FP_Greater(XNORM, BIGNUM / CMAX) then
                begin
                  Temp := CMAX / BIGNUM;
                  x[1, 1] := Temp * x[1, 1];
                  x[2, 1] := Temp * x[2, 1];
                  x[1, 2] := Temp * x[1, 2];
                  x[2, 2] := Temp * x[2, 2];
                  XNORM := Temp * XNORM;
                  SCL := Temp * SCL;
                end;
            end;
        end;
    end;
end;

(* ************************************************************************
  performs complex division in  real arithmetic

  a + i*b
  p + i*q = ---------
  c + i*d

  The algorithm is due to Robert L. Smith and can be found
  in D. Knuth, The art of Computer Programming, Vol.2, p.195
  ************************************************************************ *)
procedure InternalHSEVDLADIV(const a: TLFloat; const b: TLFloat;
  const c: TLFloat; const d: TLFloat; var p: TLFloat;
  var q: TLFloat);
var
  E: TLFloat;
  f: TLFloat;
begin
  if AP_FP_Less(AbsReal(d), AbsReal(c)) then
    begin
      E := d / c;
      f := c + d * E;
      p := (a + b * E) / f;
      q := (b - a * E) / f;
    end
  else
    begin
      E := c / d;
      f := d + c * E;
      p := (b + a * E) / f;
      q := (-a + b * E) / f;
    end;
end;

function NonSymmetricEVD(a: TLMatrix; n: TLInt;
  VNeeded: TLInt; var WR: TLVec; var WI: TLVec;
  var vl: TLMatrix; var vr: TLMatrix): Boolean;
var
  s: TLMatrix;
  TAU: TLVec;
  SEL: TLBVec;
  i: TLInt;
  Info: TLInt;
  M: TLInt;
begin
  a := DynamicArrayCopy(a);
  Assert((VNeeded >= 0) and (VNeeded <= 3),
    'NonSymmetricEVD: incorrect VNeeded!');
  if VNeeded = 0 then
    begin

      //
      // Eigen values only
      //
      ToUpperHessenberg(a, n, TAU);
      InternalSchurDecomposition(a, n, 0, 0, WR, WI, s, Info);
      Result := Info = 0;
      Exit;
    end;

  //
  // Eigen values and vectors
  //
  ToUpperHessenberg(a, n, TAU);
  UnpackQFromUpperHessenberg(a, n, TAU, s);
  InternalSchurDecomposition(a, n, 1, 1, WR, WI, s, Info);
  Result := Info = 0;
  if not Result then
    begin
      Exit;
    end;
  if (VNeeded = 1) or (VNeeded = 3) then
    begin
      SetLength(vr, n + 1, n + 1);
      i := 1;
      while i <= n do
        begin
          APVMove(@vr[i][0], 1, n, @s[i][0], 1, n);
          inc(i);
        end;
    end;
  if (VNeeded = 2) or (VNeeded = 3) then
    begin
      SetLength(vl, n + 1, n + 1);
      i := 1;
      while i <= n do
        begin
          APVMove(@vl[i][0], 1, n, @s[i][0], 1, n);
          inc(i);
        end;
    end;
  InternalTREVC(a, n, VNeeded, 1, SEL, vl, vr, M, Info);
  Result := Info = 0;
end;

procedure ToUpperHessenberg(var a: TLMatrix; n: TLInt;
  var TAU: TLVec);
var
  i: TLInt;
  IP1: TLInt;
  NMI: TLInt;
  v: TLFloat;
  t: TLVec;
  Work: TLVec;
  i_: TLInt;
  i1_: TLInt;
begin
  Assert(n >= 0, 'ToUpperHessenberg: incorrect N!');

  //
  // Quick return if possible
  //
  if n <= 1 then
    begin
      Exit;
    end;
  SetLength(TAU, n  );
  SetLength(t, n + 1);
  SetLength(Work, n + 1);
  i := 1;
  while i <= n - 1 do
    begin

      //
      // Compute elementary reflector H(i) to annihilate A(i+2:ihi,i)
      //
      IP1 := i + 1;
      NMI := n - i;
      i1_ := (IP1) - (1);
      for i_ := 1 to NMI do
        begin
          t[i_] := a[i_ + i1_, i];
        end;
      GenerateReflection(t, NMI, v);
      i1_ := (1) - (IP1);
      for i_ := IP1 to n do
        begin
          a[i_, i] := t[i_ + i1_];
        end;
      TAU[i] := v;
      t[1] := 1;

      //
      // Apply H(i) to A(1:ihi,i+1:ihi) from the right
      //
      ApplyReflectionFromTheRight(a, v, t, 1, n, i + 1, n, Work);

      //
      // Apply H(i) to A(i+1:ihi,i+1:n) from the left
      //
      ApplyReflectionFromTheLeft(a, v, t, i + 1, n, i + 1, n, Work);
      inc(i);
    end;
end;

procedure UnpackQFromUpperHessenberg(const a: TLMatrix; n: TLInt;
  const TAU: TLVec; var q: TLMatrix);
var
  i: TLInt;
  j: TLInt;
  v: TLVec;
  Work: TLVec;
  IP1: TLInt;
  NMI: TLInt;
  i_: TLInt;
  i1_: TLInt;
begin
  if n = 0 then
    begin
      Exit;
    end;

  //
  // init
  //
  SetLength(q, n + 1, n + 1);
  SetLength(v, n + 1);
  SetLength(Work, n + 1);
  i := 1;
  while i <= n do
    begin
      j := 1;
      while j <= n do
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
  i := 1;
  while i <= n - 1 do
    begin

      //
      // Apply H(i)
      //
      IP1 := i + 1;
      NMI := n - i;
      i1_ := (IP1) - (1);
      for i_ := 1 to NMI do
        begin
          v[i_] := a[i_ + i1_, i];
        end;
      v[1] := 1;
      ApplyReflectionFromTheRight(q, TAU[i], v, 1, n, i + 1, n, Work);
      inc(i);
    end;
end;

procedure UnpackHFromUpperHessenberg(const a: TLMatrix; n: TLInt;
  const TAU: TLVec; var h: TLMatrix);
var
  i: TLInt;
  j: TLInt;
  v: TLVec;
  Work: TLVec;
begin
  if n = 0 then
    begin
      Exit;
    end;
  SetLength(h, n + 1, n + 1);
  i := 1;
  while i <= n do
    begin
      j := 1;
      while j <= i - 2 do
        begin
          h[i, j] := 0;
          inc(j);
        end;
      j := Max(1, i - 1);
      APVMove(@h[i][0], j, n, @a[i][0], j, n);
      inc(i);
    end;
end; 
 
 
