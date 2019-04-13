{ ****************************************************************************** }
{ * Schur decomposition                                                        * }
{ * by QQ 600585@qq.com                                                        * }
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


procedure InternalSchurDecomposition(var h: TLMatrix; n: TLInt; TNeeded: TLInt; ZNeeded: TLInt;
  var WR: TLVec; var WI: TLVec; var z: TLMatrix; var Info: TLInt); forward;

procedure InternalAuxSchur(WANTT: Boolean; WANTZ: Boolean; n: TLInt;
  ILO: TLInt; IHI: TLInt; var h: TLMatrix;
  var WR: TLVec; var WI: TLVec; ILOZ: TLInt;
  IHIZ: TLInt; var z: TLMatrix; var Work: TLVec;
  var WORKV3: TLVec; var WORKC1: TLVec; var WORKS1: TLVec;
  var Info: TLInt); forward;

procedure Aux2X2Schur(var a: TLFloat; var b: TLFloat;
  var c: TLFloat; var d: TLFloat; var RT1R: TLFloat;
  var RT1I: TLFloat; var RT2R: TLFloat; var RT2I: TLFloat;
  var cs: TLFloat; var sn: TLFloat); forward;

function ExtSchurSign(a: TLFloat; b: TLFloat): TLFloat; forward;
function ExtSchurSignToOne(b: TLFloat): TLInt; forward;


(* ************************************************************************
  Subroutine performing the Schur decomposition of a general matrix by using
  the QR algorithm with multiple shifts.

  The source matrix A is represented as S'*A*S = T, where S is an orthogonal
  matrix (Schur vectors), T - upper quasi-triangular matrix (with blocks of
  sizes 1x1 and 2x2 on the main diagonal).

  Input parameters:
  A   -   matrix to be decomposed.
  Array whose indexes range within [0..N-1, 0..N-1].
  N   -   size of A, N>=0.


  Output parameters:
  A   -   contains matrix T.
  Array whose indexes range within [0..N-1, 0..N-1].
  S   -   contains Schur vectors.
  Array whose indexes range within [0..N-1, 0..N-1].

  Note 1:
  The block structure of matrix T can be easily recognized: since all
  the elements below the blocks are zeros, the elements a[i+1,i] which
  are equal to 0 show the block border.

  Note 2:
  The algorithm performance depends on the value of the internal parameter
  NS of the InternalSchurDecomposition subroutine which defines the number
  of shifts in the QR algorithm (similarly to the block width in block-matrix
  algorithms in linear algebra). If you require maximum performance on
  your machine, it is recommended to adjust this parameter manually.

  Result:
  True,  if the algorithm has converged and parameters A and S contain the result.
  False, if the algorithm has not converged.
  ************************************************************************ *)
function RMatrixSchur(var a: TLMatrix; n: TLInt; var s: TLMatrix): Boolean;
var
  TAU: TLVec;
  WI: TLVec;
  WR: TLVec;
  a1: TLMatrix;
  s1: TLMatrix;
  Info: TLInt;
  i: TLInt;
  j: TLInt;
begin

  //
  // Upper Hessenberg form of the 0-based matrix
  //
  RMatrixHessenberg(a, n, TAU);
  RMatrixHessenbergUnpackQ(a, n, TAU, s);

  //
  // Convert from 0-based arrays to 1-based,
  // then call InternalSchurDecomposition
  // Awkward, of course, but Schur decompisiton subroutine
  // is too TLComplex to fix it.
  //
  //
  SetLength(a1, n + 1, n + 1);
  SetLength(s1, n + 1, n + 1);
  i := 1;
  while i <= n do
    begin
      j := 1;
      while j <= n do
        begin
          a1[i, j] := a[i - 1, j - 1];
          s1[i, j] := s[i - 1, j - 1];
          inc(j);
        end;
      inc(i);
    end;
  InternalSchurDecomposition(a1, n, 1, 1, WR, WI, s1, Info);
  Result := Info = 0;

  //
  // convert from 1-based arrays to -based
  //
  i := 1;
  while i <= n do
    begin
      j := 1;
      while j <= n do
        begin
          a[i - 1, j - 1] := a1[i, j];
          s[i - 1, j - 1] := s1[i, j];
          inc(j);
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Subroutine performing  the  Schur  decomposition  of  a  matrix  in  upper
  Hessenberg form using the QR algorithm with multiple shifts.

  The  source matrix  H  is  represented as  S'*H*S = T, where H - matrix in
  upper Hessenberg form,  S - orthogonal matrix (Schur vectors),   T - upper
  quasi-triangular matrix (with blocks of sizes  1x1  and  2x2  on  the main
  diagonal).

  Input parameters:
  H   -   matrix to be decomposed. Array whose indexes range within [1..N, 1..N].
  N   -   size of H, N>=0.

  Output parameters:
  H   -  contains the matrix T. Array whose indexes range within [1..N, 1..N].
  All elements below the blocks on the main diagonal are equal to 0.
  S   -   contains Schur vectors. Array whose indexes range within [1..N, 1..N].

  Note 1:
  The block structure of matrix T could be easily recognized: since  all
  the elements  below  the blocks are zeros, the elements a[i+1,i] which
  are equal to 0 show the block border.

  Note 2:
  the algorithm  performance  depends  on  the  value  of  the  internal
  parameter NS of InternalSchurDecomposition  subroutine  which  defines
  the number of shifts in the QR algorithm (analog of  the  block  width
  in block matrix algorithms in linear algebra). If you require  maximum
  performance  on  your  machine,  it  is  recommended  to  adjust  this
  parameter manually.

  Result:
  True, if the algorithm has converged and the parameters H and S contain
  the result.
  False, if the algorithm has not converged.

  Algorithm implemented on the basis of subroutine DHSEQR (LAPACK 3.0 library).
  ************************************************************************ *)
function UpperHessenbergSchurDecomposition(var h: TLMatrix;
  n: TLInt; var s: TLMatrix): Boolean;
var
  WI: TLVec;
  WR: TLVec;
  Info: TLInt;
begin
  InternalSchurDecomposition(h, n, 1, 2, WR, WI, s, Info);
  Result := Info = 0;
end;

procedure InternalSchurDecomposition(var h: TLMatrix; n: TLInt; TNeeded: TLInt; ZNeeded: TLInt;
  var WR: TLVec; var WI: TLVec; var z: TLMatrix; var Info: TLInt);
var
  Work: TLVec;
  i: TLInt;
  i1: TLInt;
  i2: TLInt;
  IERR: TLInt;
  II: TLInt;
  ITEMP: TLInt;
  ITN: TLInt;
  ITS: TLInt;
  j: TLInt;
  k: TLInt;
  L: TLInt;
  MAXB: TLInt;
  NR: TLInt;
  ns: TLInt;
  NV: TLInt;
  ABSW: TLFloat;
  OVFL: TLFloat;
  SMLNUM: TLFloat;
  TAU: TLFloat;
  Temp: TLFloat;
  TST1: TLFloat;
  ULP: TLFloat;
  UNFL: TLFloat;
  s: TLMatrix;
  v: TLVec;
  VV: TLVec;
  WORKC1: TLVec;
  WORKS1: TLVec;
  WORKV3: TLVec;
  TmpWR: TLVec;
  TmpWI: TLVec;
  INITZ: Boolean;
  WANTT: Boolean;
  WANTZ: Boolean;
  CNST: TLFloat;
  FailFlag: Boolean;
  p1: TLInt;
  p2: TLInt;
  VT: TLFloat;
  i_: TLInt;
  i1_: TLInt;
begin

  //
  // Set the order of the multi-shift QR algorithm to be used.
  // If you want to tune algorithm, change this values
  //
  ns := 12;
  MAXB := 50;

  //
  // Now 2 < NS <= MAXB < NH.
  //
  MAXB := Max(3, MAXB);
  ns := Min(MAXB, ns);

  //
  // Initialize
  //
  CNST := 1.5;
  SetLength(Work, Max(n, 1) + 1);
  SetLength(s, ns + 1, ns + 1);
  SetLength(v, ns + 1 + 1);
  SetLength(VV, ns + 1 + 1);
  SetLength(WR, Max(n, 1) + 1);
  SetLength(WI, Max(n, 1) + 1);
  SetLength(WORKC1, 1 + 1);
  SetLength(WORKS1, 1 + 1);
  SetLength(WORKV3, 3 + 1);
  SetLength(TmpWR, Max(n, 1) + 1);
  SetLength(TmpWI, Max(n, 1) + 1);
  Assert(n >= 0, 'InternalSchurDecomposition: incorrect N!');
  Assert((TNeeded = 0) or (TNeeded = 1), 'InternalSchurDecomposition: incorrect TNeeded!');
  Assert((ZNeeded = 0) or (ZNeeded = 1) or (ZNeeded = 2), 'InternalSchurDecomposition: incorrect ZNeeded!');
  WANTT := TNeeded = 1;
  INITZ := ZNeeded = 2;
  WANTZ := ZNeeded <> 0;
  Info := 0;

  //
  // Initialize Z, if necessary
  //
  if INITZ then
    begin
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

  //
  // Quick return if possible
  //
  if n = 0 then
    begin
      Exit;
    end;
  if n = 1 then
    begin
      WR[1] := h[1, 1];
      WI[1] := 0;
      Exit;
    end;

  //
  // Set rows and columns 1 to N to zero below the first
  // subdiagonal.
  //
  j := 1;
  while j <= n - 2 do
    begin
      i := j + 2;
      while i <= n do
        begin
          h[i, j] := 0;
          inc(i);
        end;
      inc(j);
    end;

  //
  // Test if N is sufficiently small
  //
  if (ns <= 2) or (ns > n) or (MAXB >= n) then
    begin

      //
      // Use the standard TLFloat-shift algorithm
      //
      InternalAuxSchur(WANTT, WANTZ, n, 1, n, h, WR, WI, 1, n, z, Work, WORKV3, WORKC1, WORKS1, Info);

      //
      // fill entries under diagonal blocks of T with zeros
      //
      if WANTT then
        begin
          j := 1;
          while j <= n do
            begin
              if AP_FP_Eq(WI[j], 0) then
                begin
                  i := j + 1;
                  while i <= n do
                    begin
                      h[i, j] := 0;
                      inc(i);
                    end;
                  j := j + 1;
                end
              else
                begin
                  i := j + 2;
                  while i <= n do
                    begin
                      h[i, j] := 0;
                      h[i, j + 1] := 0;
                      inc(i);
                    end;
                  j := j + 2;
                end;
            end;
        end;
      Exit;
    end;
  UNFL := MinRealNumber;
  OVFL := 1 / UNFL;
  ULP := 2 * MachineEpsilon;
  SMLNUM := UNFL * (n / ULP);

  //
  // I1 and I2 are the indices of the first row and last column of H
  // to which transformations must be applied. If eigenvalues only are
  // being computed, I1 and I2 are set inside the main loop.
  //
  i1 := 1;
  i2 := n;

  //
  // ITN is the total number of multiple-shift QR iterations allowed.
  //
  ITN := 30 * n;

  //
  // The main loop begins here. I is the loop index and decreases from
  // IHI to ILO in steps of at most MAXB. Each iteration of the loop
  // works with the active submatrix in rows and columns L to I.
  // Eigenvalues I+1 to IHI have already converged. Either L = ILO or
  // H(L,L-1) is negligible so that the matrix splits.
  //
  i := n;
  while True do
    begin
      L := 1;
      if i < 1 then
        begin

          //
          // fill entries under diagonal blocks of T with zeros
          //
          if WANTT then
            begin
              j := 1;
              while j <= n do
                begin
                  if AP_FP_Eq(WI[j], 0) then
                    begin
                      i := j + 1;
                      while i <= n do
                        begin
                          h[i, j] := 0;
                          inc(i);
                        end;
                      j := j + 1;
                    end
                  else
                    begin
                      i := j + 2;
                      while i <= n do
                        begin
                          h[i, j] := 0;
                          h[i, j + 1] := 0;
                          inc(i);
                        end;
                      j := j + 2;
                    end;
                end;
            end;

          //
          // Exit
          //
          Exit;
        end;

      //
      // Perform multiple-shift QR iterations on rows and columns ILO to I
      // until a submatrix of order at most MAXB splits off at the bottom
      // because a subdiagonal element has become negligible.
      //
      FailFlag := True;
      ITS := 0;
      while ITS <= ITN do
        begin

          //
          // Look for a TLFloat small subdiagonal element.
          //
          k := i;
          while k >= L + 1 do
            begin
              TST1 := AbsReal(h[k - 1, k - 1]) + AbsReal(h[k, k]);
              if AP_FP_Eq(TST1, 0) then
                begin
                  TST1 := UpperHessenberg1Norm(h, L, i, L, i, Work);
                end;
              if AP_FP_Less_Eq(AbsReal(h[k, k - 1]), Max(ULP * TST1, SMLNUM)) then
                begin
                  Break;
                end;
              dec(k);
            end;
          L := k;
          if L > 1 then
            begin

              //
              // H(L,L-1) is negligible.
              //
              h[L, L - 1] := 0;
            end;

          //
          // Exit from loop if a submatrix of order <= MAXB has split off.
          //
          if L >= i - MAXB + 1 then
            begin
              FailFlag := False;
              Break;
            end;

          //
          // Now the active submatrix is in rows and columns L to I. If
          // eigenvalues only are being computed, only the active submatrix
          // need be transformed.
          //
          if (ITS = 20) or (ITS = 30) then
            begin

              //
              // Exceptional shifts.
              //
              II := i - ns + 1;
              while II <= i do
                begin
                  WR[II] := CNST * (AbsReal(h[II, II - 1]) + AbsReal(h[II, II]));
                  WI[II] := 0;
                  inc(II);
                end;
            end
          else
            begin

              //
              // Use eigenvalues of trailing submatrix of order NS as shifts.
              //
              CopyMatrix(h, i - ns + 1, i, i - ns + 1, i, s, 1, ns, 1, ns);
              InternalAuxSchur(False, False, ns, 1, ns, s, TmpWR, TmpWI, 1, ns, z,
                Work, WORKV3, WORKC1, WORKS1, IERR);
              p1 := 1;
              while p1 <= ns do
                begin
                  WR[i - ns + p1] := TmpWR[p1];
                  WI[i - ns + p1] := TmpWI[p1];
                  inc(p1);
                end;
              if IERR > 0 then
                begin

                  //
                  // If DLAHQR failed to compute all NS eigenvalues, use the
                  // unconverged diagonal elements as the remaining shifts.
                  //
                  II := 1;
                  while II <= IERR do
                    begin
                      WR[i - ns + II] := s[II, II];
                      WI[i - ns + II] := 0;
                      inc(II);
                    end;
                end;
            end;

          //
          // Form the first column of (G-w(1)) (G-w(2)) . . . (G-w(ns))
          // where G is the Hessenberg submatrix H(L:I,L:I) and w is
          // the vector of shifts (stored in WR and WI). The result is
          // stored in the local array V.
          //
          v[1] := 1;
          II := 2;
          while II <= ns + 1 do
            begin
              v[II] := 0;
              inc(II);
            end;
          NV := 1;
          j := i - ns + 1;
          while j <= i do
            begin
              if AP_FP_Greater_Eq(WI[j], 0) then
                begin
                  if AP_FP_Eq(WI[j], 0) then
                    begin

                      //
                      // real shift
                      //
                      p1 := NV + 1;
                      APVMove(@VV[0], 1, p1, @v[0], 1, p1);
                      MatrixVectorMultiply(h, L, L + NV, L, L + NV - 1, False, VV, 1, NV,
                        1.0, v, 1, NV + 1, -WR[j]);
                      NV := NV + 1;
                    end
                  else
                    begin
                      if AP_FP_Greater(WI[j], 0) then
                        begin

                          //
                          // complex conjugate pair of shifts
                          //
                          p1 := NV + 1;
                          APVMove(@VV[0], 1, p1, @v[0], 1, p1);
                          MatrixVectorMultiply(h, L, L + NV, L, L + NV - 1, False, v, 1, NV, 1.0, VV, 1, NV + 1, -2 * WR[j]);
                          ITEMP := VectorIdxAbsMax(VV, 1, NV + 1);
                          Temp := 1 / Max(AbsReal(VV[ITEMP]), SMLNUM);
                          p1 := NV + 1;
                          APVMul(@VV[0], 1, p1, Temp);
                          ABSW := Pythag2(WR[j], WI[j]);
                          Temp := Temp * ABSW * ABSW;
                          MatrixVectorMultiply(h, L, L + NV + 1, L, L + NV, False, VV, 1, NV + 1, 1.0, v, 1, NV + 2, Temp);
                          NV := NV + 2;
                        end;
                    end;

                  //
                  // Scale V(1:NV) so that max(abs(V(i))) = 1. If V is zero,
                  // reset it to the unit vector.
                  //
                  ITEMP := VectorIdxAbsMax(v, 1, NV);
                  Temp := AbsReal(v[ITEMP]);
                  if AP_FP_Eq(Temp, 0) then
                    begin
                      v[1] := 1;
                      II := 2;
                      while II <= NV do
                        begin
                          v[II] := 0;
                          inc(II);
                        end;
                    end
                  else
                    begin
                      Temp := Max(Temp, SMLNUM);
                      VT := 1 / Temp;
                      APVMul(@v[0], 1, NV, VT);
                    end;
                end;
              inc(j);
            end;

          //
          // Multiple-shift QR step
          //
          k := L;
          while k <= i - 1 do
            begin

              //
              // The first iteration of this loop determines a reflection G
              // from the vector V and applies it from left and right to H,
              // thus creating a nonzero bulge below the subdiagonal.
              //
              // Each subsequent iteration determines a reflection G to
              // restore the Hessenberg form in the (K-1)th column, and thus
              // chases the bulge one step toward the bottom of the active
              // submatrix. NR is the order of G.
              //
              NR := Min(ns + 1, i - k + 1);
              if k > L then
                begin
                  p1 := k - 1;
                  p2 := k + NR - 1;
                  i1_ := (k) - (1);
                  for i_ := 1 to NR do
                    begin
                      v[i_] := h[i_ + i1_, p1];
                    end;
                end;
              GenerateReflection(v, NR, TAU);
              if k > L then
                begin
                  h[k, k - 1] := v[1];
                  II := k + 1;
                  while II <= i do
                    begin
                      h[II, k - 1] := 0;
                      inc(II);
                    end;
                end;
              v[1] := 1;

              //
              // Apply G from the left to transform the rows of the matrix in
              // columns K to I2.
              //
              ApplyReflectionFromTheLeft(h, TAU, v, k, k + NR - 1, k, i2, Work);

              //
              // Apply G from the right to transform the columns of the
              // matrix in rows I1 to min(K+NR,I).
              //
              ApplyReflectionFromTheRight(h, TAU, v, i1, Min(k + NR, i), k,
                k + NR - 1, Work);
              if WANTZ then
                begin

                  //
                  // Accumulate transformations in the matrix Z
                  //
                  ApplyReflectionFromTheRight(z, TAU, v, 1, n, k, k + NR - 1, Work);
                end;
              inc(k);
            end;
          inc(ITS);
        end;

      //
      // Failure to converge in remaining number of iterations
      //
      if FailFlag then
        begin
          Info := i;
          Exit;
        end;

      //
      // A submatrix of order <= MAXB in rows and columns L to I has split
      // off. Use the TLFloat-shift QR algorithm to handle it.
      //
      InternalAuxSchur(WANTT, WANTZ, n, L, i, h, WR, WI, 1, n, z, Work, WORKV3,
        WORKC1, WORKS1, Info);
      if Info > 0 then
        begin
          Exit;
        end;

      //
      // Decrement number of remaining iterations, and return to start of
      // the main loop with a new value of I.
      //
      ITN := ITN - ITS;
      i := L - 1;
    end;
end;

procedure InternalAuxSchur(WANTT: Boolean; WANTZ: Boolean; n: TLInt;
  ILO: TLInt; IHI: TLInt; var h: TLMatrix;
  var WR: TLVec; var WI: TLVec; ILOZ: TLInt;
  IHIZ: TLInt; var z: TLMatrix; var Work: TLVec;
  var WORKV3: TLVec; var WORKC1: TLVec; var WORKS1: TLVec;
  var Info: TLInt);
var
  i: TLInt;
  i1: TLInt;
  i2: TLInt;
  ITN: TLInt;
  ITS: TLInt;
  j: TLInt;
  k: TLInt;
  L: TLInt;
  M: TLInt;
  NH: TLInt;
  NR: TLInt;
  Nz: TLInt;
  AVE: TLFloat;
  cs: TLFloat;
  DISC: TLFloat;
  H00: TLFloat;
  H10: TLFloat;
  H11: TLFloat;
  H12: TLFloat;
  H21: TLFloat;
  H22: TLFloat;
  H33: TLFloat;
  H33S: TLFloat;
  H43H34: TLFloat;
  H44: TLFloat;
  H44S: TLFloat;
  OVFL: TLFloat;
  s: TLFloat;
  SMLNUM: TLFloat;
  sn: TLFloat;
  Sum: TLFloat;
  t1: TLFloat;
  t2: TLFloat;
  t3: TLFloat;
  TST1: TLFloat;
  UNFL: TLFloat;
  v1: TLFloat;
  v2: TLFloat;
  v3: TLFloat;
  FailFlag: Boolean;
  DAT1: TLFloat;
  DAT2: TLFloat;
  p1: TLInt;
  HIM1IM1: TLFloat;
  HIM1I: TLFloat;
  HIIM1: TLFloat;
  HII: TLFloat;
  WRIM1: TLFloat;
  WRI: TLFloat;
  WIIM1: TLFloat;
  WII: TLFloat;
  ULP: TLFloat;
begin
  Info := 0;
  DAT1 := 0.75;
  DAT2 := -0.4375;
  ULP := MachineEpsilon;

  //
  // Quick return if possible
  //
  if n = 0 then
    begin
      Exit;
    end;
  if ILO = IHI then
    begin
      WR[ILO] := h[ILO, ILO];
      WI[ILO] := 0;
      Exit;
    end;
  NH := IHI - ILO + 1;
  Nz := IHIZ - ILOZ + 1;

  //
  // Set machine-dependent constants for the stopping criterion.
  // If norm(H) <= sqrt(OVFL), overflow should not occur.
  //
  UNFL := MinRealNumber;
  OVFL := 1 / UNFL;
  SMLNUM := UNFL * (NH / ULP);

  //
  // I1 and I2 are the indices of the first row and last column of H
  // to which transformations must be applied. If eigenvalues only are
  // being computed, I1 and I2 are set inside the main loop.
  //
  i1 := 1;
  i2 := n;

  //
  // ITN is the total number of QR iterations allowed.
  //
  ITN := 30 * NH;

  //
  // The main loop begins here. I is the loop index and decreases from
  // IHI to ILO in steps of 1 or 2. Each iteration of the loop works
  // with the active submatrix in rows and columns L to I.
  // Eigenvalues I+1 to IHI have already converged. Either L = ILO or
  // H(L,L-1) is negligible so that the matrix splits.
  //
  i := IHI;
  while True do
    begin
      L := ILO;
      if i < ILO then
        begin
          Exit;
        end;

      //
      // Perform QR iterations on rows and columns ILO to I until a
      // submatrix of order 1 or 2 splits off at the bottom because a
      // subdiagonal element has become negligible.
      //
      FailFlag := True;
      ITS := 0;
      while ITS <= ITN do
        begin

          //
          // Look for a TLFloat small subdiagonal element.
          //
          k := i;
          while k >= L + 1 do
            begin
              TST1 := AbsReal(h[k - 1, k - 1]) + AbsReal(h[k, k]);
              if AP_FP_Eq(TST1, 0) then
                begin
                  TST1 := UpperHessenberg1Norm(h, L, i, L, i, Work);
                end;
              if AP_FP_Less_Eq(AbsReal(h[k, k - 1]), Max(ULP * TST1, SMLNUM)) then
                begin
                  Break;
                end;
              dec(k);
            end;
          L := k;
          if L > ILO then
            begin

              //
              // H(L,L-1) is negligible
              //
              h[L, L - 1] := 0;
            end;

          //
          // Exit from loop if a submatrix of order 1 or 2 has split off.
          //
          if L >= i - 1 then
            begin
              FailFlag := False;
              Break;
            end;

          //
          // Now the active submatrix is in rows and columns L to I. If
          // eigenvalues only are being computed, only the active submatrix
          // need be transformed.
          //
          if (ITS = 10) or (ITS = 20) then
            begin

              //
              // Exceptional shift.
              //
              s := AbsReal(h[i, i - 1]) + AbsReal(h[i - 1, i - 2]);
              H44 := DAT1 * s + h[i, i];
              H33 := H44;
              H43H34 := DAT2 * s * s;
            end
          else
            begin

              //
              // Prepare to use Francis' TLFloat shift
              // (i.e. 2nd degree generalized Rayleigh quotient)
              //
              H44 := h[i, i];
              H33 := h[i - 1, i - 1];
              H43H34 := h[i, i - 1] * h[i - 1, i];
              s := h[i - 1, i - 2] * h[i - 1, i - 2];
              DISC := (H33 - H44) * 0.5;
              DISC := DISC * DISC + H43H34;
              if AP_FP_Greater(DISC, 0) then
                begin

                  //
                  // Real roots: use Wilkinson's shift twice
                  //
                  DISC := Sqrt(DISC);
                  AVE := 0.5 * (H33 + H44);
                  if AP_FP_Greater(AbsReal(H33) - AbsReal(H44), 0) then
                    begin
                      H33 := H33 * H44 - H43H34;
                      H44 := H33 / (ExtSchurSign(DISC, AVE) + AVE);
                    end
                  else
                    begin
                      H44 := ExtSchurSign(DISC, AVE) + AVE;
                    end;
                  H33 := H44;
                  H43H34 := 0;
                end;
            end;

          //
          // Look for two consecutive small subdiagonal elements.
          //
          M := i - 2;
          while M >= L do
            begin

              //
              // Determine the effect of starting the TLFloat-shift QR
              // iteration at row M, and see if this would make H(M,M-1)
              // negligible.
              //
              H11 := h[M, M];
              H22 := h[M + 1, M + 1];
              H21 := h[M + 1, M];
              H12 := h[M, M + 1];
              H44S := H44 - H11;
              H33S := H33 - H11;
              v1 := (H33S * H44S - H43H34) / H21 + H12;
              v2 := H22 - H11 - H33S - H44S;
              v3 := h[M + 2, M + 1];
              s := AbsReal(v1) + AbsReal(v2) + AbsReal(v3);
              v1 := v1 / s;
              v2 := v2 / s;
              v3 := v3 / s;
              WORKV3[1] := v1;
              WORKV3[2] := v2;
              WORKV3[3] := v3;
              if M = L then
                begin
                  Break;
                end;
              H00 := h[M - 1, M - 1];
              H10 := h[M, M - 1];
              TST1 := AbsReal(v1) * (AbsReal(H00) + AbsReal(H11) + AbsReal(H22));
              if AP_FP_Less_Eq(AbsReal(H10) * (AbsReal(v2) + AbsReal(v3)), ULP * TST1)
              then
                begin
                  Break;
                end;
              dec(M);
            end;

          //
          // TLFloat-shift QR step
          //
          k := M;
          while k <= i - 1 do
            begin

              //
              // The first iteration of this loop determines a reflection G
              // from the vector V and applies it from left and right to H,
              // thus creating a nonzero bulge below the subdiagonal.
              //
              // Each subsequent iteration determines a reflection G to
              // restore the Hessenberg form in the (K-1)th column, and thus
              // chases the bulge one step toward the bottom of the active
              // submatrix. NR is the order of G.
              //
              NR := Min(3, i - k + 1);
              if k > M then
                begin
                  p1 := 1;
                  while p1 <= NR do
                    begin
                      WORKV3[p1] := h[k + p1 - 1, k - 1];
                      inc(p1);
                    end;
                end;
              GenerateReflection(WORKV3, NR, t1);
              if k > M then
                begin
                  h[k, k - 1] := WORKV3[1];
                  h[k + 1, k - 1] := 0;
                  if k < i - 1 then
                    begin
                      h[k + 2, k - 1] := 0;
                    end;
                end
              else
                begin
                  if M > L then
                    begin
                      h[k, k - 1] := -h[k, k - 1];
                    end;
                end;
              v2 := WORKV3[2];
              t2 := t1 * v2;
              if NR = 3 then
                begin
                  v3 := WORKV3[3];
                  t3 := t1 * v3;

                  //
                  // Apply G from the left to transform the rows of the matrix
                  // in columns K to I2.
                  //
                  j := k;
                  while j <= i2 do
                    begin
                      Sum := h[k, j] + v2 * h[k + 1, j] + v3 * h[k + 2, j];
                      h[k, j] := h[k, j] - Sum * t1;
                      h[k + 1, j] := h[k + 1, j] - Sum * t2;
                      h[k + 2, j] := h[k + 2, j] - Sum * t3;
                      inc(j);
                    end;

                  //
                  // Apply G from the right to transform the columns of the
                  // matrix in rows I1 to min(K+3,I).
                  //
                  j := i1;
                  while j <= Min(k + 3, i) do
                    begin
                      Sum := h[j, k] + v2 * h[j, k + 1] + v3 * h[j, k + 2];
                      h[j, k] := h[j, k] - Sum * t1;
                      h[j, k + 1] := h[j, k + 1] - Sum * t2;
                      h[j, k + 2] := h[j, k + 2] - Sum * t3;
                      inc(j);
                    end;
                  if WANTZ then
                    begin

                      //
                      // Accumulate transformations in the matrix Z
                      //
                      j := ILOZ;
                      while j <= IHIZ do
                        begin
                          Sum := z[j, k] + v2 * z[j, k + 1] + v3 * z[j, k + 2];
                          z[j, k] := z[j, k] - Sum * t1;
                          z[j, k + 1] := z[j, k + 1] - Sum * t2;
                          z[j, k + 2] := z[j, k + 2] - Sum * t3;
                          inc(j);
                        end;
                    end;
                end
              else
                begin
                  if NR = 2 then
                    begin

                      //
                      // Apply G from the left to transform the rows of the matrix
                      // in columns K to I2.
                      //
                      j := k;
                      while j <= i2 do
                        begin
                          Sum := h[k, j] + v2 * h[k + 1, j];
                          h[k, j] := h[k, j] - Sum * t1;
                          h[k + 1, j] := h[k + 1, j] - Sum * t2;
                          inc(j);
                        end;

                      //
                      // Apply G from the right to transform the columns of the
                      // matrix in rows I1 to min(K+3,I).
                      //
                      j := i1;
                      while j <= i do
                        begin
                          Sum := h[j, k] + v2 * h[j, k + 1];
                          h[j, k] := h[j, k] - Sum * t1;
                          h[j, k + 1] := h[j, k + 1] - Sum * t2;
                          inc(j);
                        end;
                      if WANTZ then
                        begin

                          //
                          // Accumulate transformations in the matrix Z
                          //
                          j := ILOZ;
                          while j <= IHIZ do
                            begin
                              Sum := z[j, k] + v2 * z[j, k + 1];
                              z[j, k] := z[j, k] - Sum * t1;
                              z[j, k + 1] := z[j, k + 1] - Sum * t2;
                              inc(j);
                            end;
                        end;
                    end;
                end;
              inc(k);
            end;
          inc(ITS);
        end;
      if FailFlag then
        begin

          //
          // Failure to converge in remaining number of iterations
          //
          Info := i;
          Exit;
        end;
      if L = i then
        begin

          //
          // H(I,I-1) is negligible: one eigenvalue has converged.
          //
          WR[i] := h[i, i];
          WI[i] := 0;
        end
      else
        begin
          if L = i - 1 then
            begin

              //
              // H(I-1,I-2) is negligible: a pair of eigenvalues have converged.
              //
              // Transform the 2-by-2 submatrix to standard Schur form,
              // and compute and store the eigenvalues.
              //
              HIM1IM1 := h[i - 1, i - 1];
              HIM1I := h[i - 1, i];
              HIIM1 := h[i, i - 1];
              HII := h[i, i];
              Aux2X2Schur(HIM1IM1, HIM1I, HIIM1, HII, WRIM1, WIIM1, WRI, WII, cs, sn);
              WR[i - 1] := WRIM1;
              WI[i - 1] := WIIM1;
              WR[i] := WRI;
              WI[i] := WII;
              h[i - 1, i - 1] := HIM1IM1;
              h[i - 1, i] := HIM1I;
              h[i, i - 1] := HIIM1;
              h[i, i] := HII;
              if WANTT then
                begin

                  //
                  // Apply the transformation to the rest of H.
                  //
                  if i2 > i then
                    begin
                      WORKC1[1] := cs;
                      WORKS1[1] := sn;
                      ApplyRotationsFromTheLeft(True, i - 1, i, i + 1, i2, WORKC1,
                        WORKS1, h, Work);
                    end;
                  WORKC1[1] := cs;
                  WORKS1[1] := sn;
                  ApplyRotationsFromTheRight(True, i1, i - 2, i - 1, i, WORKC1,
                    WORKS1, h, Work);
                end;
              if WANTZ then
                begin

                  //
                  // Apply the transformation to Z.
                  //
                  WORKC1[1] := cs;
                  WORKS1[1] := sn;
                  ApplyRotationsFromTheRight(True, ILOZ, ILOZ + Nz - 1, i - 1, i,
                    WORKC1, WORKS1, z, Work);
                end;
            end;
        end;

      //
      // Decrement number of remaining iterations, and return to start of
      // the main loop with new value of I.
      //
      ITN := ITN - ITS;
      i := L - 1;
    end;
end;

procedure Aux2X2Schur(var a: TLFloat; var b: TLFloat;
  var c: TLFloat; var d: TLFloat; var RT1R: TLFloat;
  var RT1I: TLFloat; var RT2R: TLFloat; var RT2I: TLFloat;
  var cs: TLFloat; var sn: TLFloat);
var
  MULTPL: TLFloat;
  AA: TLFloat;
  BB: TLFloat;
  BCMAX: TLFloat;
  BCMIS: TLFloat;
  CC: TLFloat;
  CS1: TLFloat;
  DD: TLFloat;
  Eps: TLFloat;
  p: TLFloat;
  SAB: TLFloat;
  SAC: TLFloat;
  SCL: TLFloat;
  SIGMA: TLFloat;
  SN1: TLFloat;
  TAU: TLFloat;
  Temp: TLFloat;
  z: TLFloat;
begin
  MULTPL := 4.0;
  Eps := MachineEpsilon;
  if AP_FP_Eq(c, 0) then
    begin
      cs := 1;
      sn := 0;
    end
  else
    begin
      if AP_FP_Eq(b, 0) then
        begin

          //
          // Swap rows and columns
          //
          cs := 0;
          sn := 1;
          Temp := d;
          d := a;
          a := Temp;
          b := -c;
          c := 0;
        end
      else
        begin
          if AP_FP_Eq(a - d, 0) and (ExtSchurSignToOne(b) <> ExtSchurSignToOne(c))
          then
            begin
              cs := 1;
              sn := 0;
            end
          else
            begin
              Temp := a - d;
              p := 0.5 * Temp;
              BCMAX := Max(AbsReal(b), AbsReal(c));
              BCMIS := Min(AbsReal(b), AbsReal(c)) * ExtSchurSignToOne(b) *
                ExtSchurSignToOne(c);
              SCL := Max(AbsReal(p), BCMAX);
              z := p / SCL * p + BCMAX / SCL * BCMIS;

              //
              // If Z is of the order of the machine accuracy, postpone the
              // decision on the nature of eigenvalues
              //
              if AP_FP_Greater_Eq(z, MULTPL * Eps) then
                begin

                  //
                  // Real eigenvalues. Compute A and D.
                  //
                  z := p + ExtSchurSign(Sqrt(SCL) * Sqrt(z), p);
                  a := d + z;
                  d := d - BCMAX / z * BCMIS;

                  //
                  // Compute B and the rotation matrix
                  //
                  TAU := Pythag2(c, z);
                  cs := z / TAU;
                  sn := c / TAU;
                  b := b - c;
                  c := 0;
                end
              else
                begin

                  //
                  // Complex eigenvalues, or real (almost) equal eigenvalues.
                  // Make diagonal elements equal.
                  //
                  SIGMA := b + c;
                  TAU := Pythag2(SIGMA, Temp);
                  cs := Sqrt(0.5 * (1 + AbsReal(SIGMA) / TAU));
                  sn := -p / (TAU * cs) * ExtSchurSign(1, SIGMA);

                  //
                  // Compute [ AA  BB ] = [ A  B ] [ CS -SN ]
                  // [ CC  DD ]   [ C  D ] [ SN  CS ]
                  //
                  AA := a * cs + b * sn;
                  BB := -a * sn + b * cs;
                  CC := c * cs + d * sn;
                  DD := -c * sn + d * cs;

                  //
                  // Compute [ A  B ] = [ CS  SN ] [ AA  BB ]
                  // [ C  D ]   [-SN  CS ] [ CC  DD ]
                  //
                  a := AA * cs + CC * sn;
                  b := BB * cs + DD * sn;
                  c := -AA * sn + CC * cs;
                  d := -BB * sn + DD * cs;
                  Temp := 0.5 * (a + d);
                  a := Temp;
                  d := Temp;
                  if AP_FP_NEq(c, 0) then
                    begin
                      if AP_FP_NEq(b, 0) then
                        begin
                          if ExtSchurSignToOne(b) = ExtSchurSignToOne(c) then
                            begin

                              //
                              // Real eigenvalues: reduce to upper triangular form
                              //
                              SAB := Sqrt(AbsReal(b));
                              SAC := Sqrt(AbsReal(c));
                              p := ExtSchurSign(SAB * SAC, c);
                              TAU := 1 / Sqrt(AbsReal(b + c));
                              a := Temp + p;
                              d := Temp - p;
                              b := b - c;
                              c := 0;
                              CS1 := SAB * TAU;
                              SN1 := SAC * TAU;
                              Temp := cs * CS1 - sn * SN1;
                              sn := cs * SN1 + sn * CS1;
                              cs := Temp;
                            end;
                        end
                      else
                        begin
                          b := -c;
                          c := 0;
                          Temp := cs;
                          cs := -sn;
                          sn := Temp;
                        end;
                    end;
                end;
            end;
        end;
    end;

  //
  // Store eigenvalues in (RT1R,RT1I) and (RT2R,RT2I).
  //
  RT1R := a;
  RT2R := d;
  if AP_FP_Eq(c, 0) then
    begin
      RT1I := 0;
      RT2I := 0;
    end
  else
    begin
      RT1I := Sqrt(AbsReal(b)) * Sqrt(AbsReal(c));
      RT2I := -RT1I;
    end;
end;

function ExtSchurSign(a: TLFloat; b: TLFloat): TLFloat;
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

function ExtSchurSignToOne(b: TLFloat): TLInt;
begin
  if AP_FP_Greater_Eq(b, 0) then
    begin
      Result := 1;
    end
  else
    begin
      Result := -1;
    end;
end; 
 
 
