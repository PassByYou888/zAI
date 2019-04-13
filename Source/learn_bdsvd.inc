{ ****************************************************************************** }
{ * Bidiagonal SVD support, by QQ 600585@qq.com                                * }
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

function BidiagonalSVDDecompositionInternal(var d: TLVec;
  E: TLVec; n: TLInt; IsUpper: Boolean;
  IsFractionalAccuracyRequired: Boolean; var u: TLMatrix;
  UStart: TLInt; NRU: TLInt; var c: TLMatrix;
  CStart: TLInt; NCC: TLInt; var VT: TLMatrix;
  VStart: TLInt; NCVT: TLInt): Boolean; forward;

function ExtSignBDSQR(a: TLFloat; b: TLFloat): TLFloat; forward;

procedure Svd2X2(f: TLFloat; g: TLFloat; h: TLFloat;
  var SSMIN: TLFloat; var SSMAX: TLFloat); forward;

procedure SvdV2X2(f: TLFloat; g: TLFloat; h: TLFloat;
  var SSMIN: TLFloat; var SSMAX: TLFloat; var SNR: TLFloat;
  var CSR: TLFloat; var SNL: TLFloat; var CSL: TLFloat); forward;


(* ************************************************************************
  Singular value decomposition of a bidiagonal matrix (extended algorithm)

  The algorithm performs the singular value decomposition  of  a  bidiagonal
  matrix B (upper or lower) representing it as B = Q*S*P^T, where Q and  P -
  orthogonal matrices, S - diagonal matrix with non-negative elements on the
  main diagonal, in descending order.

  The  algorithm  finds  singular  values.  In  addition,  the algorithm can
  calculate  matrices  Q  and P (more precisely, not the matrices, but their
  product  with  given  matrices U and VT - U*Q and (P^T)*VT)).  Of  course,
  matrices U and VT can be of any type, including identity. Furthermore, the
  algorithm can calculate Q'*C (this product is calculated more  effectively
  than U*Q,  because  this calculation operates with rows instead  of matrix
  columns).

  The feature of the algorithm is its ability to find  all  singular  values
  including those which are arbitrarily close to 0  with  relative  accuracy
  close to  machine precision. If the parameter IsFractionalAccuracyRequired
  is set to True, all singular values will have high relative accuracy close
  to machine precision. If the parameter is set to False, only  the  biggest
  singular value will have relative accuracy  close  to  machine  precision.
  The absolute error of other singular values is equal to the absolute error
  of the biggest singular value.

  Input parameters:
  D       -   main diagonal of matrix B.
  Array whose index ranges within [0..N-1].
  E       -   superdiagonal (or subdiagonal) of matrix B.
  Array whose index ranges within [0..N-2].
  N       -   size of matrix B.
  IsUpper -   True, if the matrix is upper bidiagonal.
  IsFractionalAccuracyRequired -
  accuracy to search singular values with.
  U       -   matrix to be multiplied by Q.
  Array whose indexes range within [0..NRU-1, 0..N-1].
  The matrix can be bigger, in that case only the  submatrix
  [0..NRU-1, 0..N-1] will be multiplied by Q.
  NRU     -   number of rows in matrix U.
  C       -   matrix to be multiplied by Q'.
  Array whose indexes range within [0..N-1, 0..NCC-1].
  The matrix can be bigger, in that case only the  submatrix
  [0..N-1, 0..NCC-1] will be multiplied by Q'.
  NCC     -   number of columns in matrix C.
  VT      -   matrix to be multiplied by P^T.
  Array whose indexes range within [0..N-1, 0..NCVT-1].
  The matrix can be bigger, in that case only the  submatrix
  [0..N-1, 0..NCVT-1] will be multiplied by P^T.
  NCVT    -   number of columns in matrix VT.

  Output parameters:
  D       -   singular values of matrix B in descending order.
  U       -   if NRU>0, contains matrix U*Q.
  VT      -   if NCVT>0, contains matrix (P^T)*VT.
  C       -   if NCC>0, contains matrix Q'*C.

  Result:
  True, if the algorithm has converged.
  False, if the algorithm hasn't converged (rare case).

  Additional information:
  The type of convergence is controlled by the internal  parameter  TOL.
  If the parameter is greater than 0, the singular values will have
  relative accuracy TOL. If TOL<0, the singular values will have
  absolute accuracy ABS(TOL)*norm(B).
  By default, |TOL| falls within the range of 10*Epsilon and 100*Epsilon,
  where Epsilon is the machine precision. It is not  recommended  to  use
  TOL less than 10*Epsilon since this will  considerably  slow  down  the
  algorithm and may not lead to error decreasing.
  ************************************************************************ *)
function RMatrixBDSVD(var d: TLVec; E: TLVec; n: TLInt;
  IsUpper: Boolean; IsFractionalAccuracyRequired: Boolean; var u: TLMatrix;
  NRU: TLInt; var c: TLMatrix; NCC: TLInt;
  var VT: TLMatrix; NCVT: TLInt): Boolean;
var
  d1: TLVec;
  e1: TLVec;
begin
  E := DynamicArrayCopy(E);
  SetLength(d1, n + 1);
  APVMove(@d1[0], 1, n, @d[0], 0, n - 1);
  if n > 1 then
    begin
      SetLength(e1, n  );
      APVMove(@e1[0], 1, n - 1, @E[0], 0, n - 2);
    end;
  Result := BidiagonalSVDDecompositionInternal(d1, e1, n, IsUpper,
    IsFractionalAccuracyRequired, u, 0, NRU, c, 0, NCC, VT, 0, NCVT);
  APVMove(@d[0], 0, n - 1, @d1[0], 1, n);
end;

function BidiagonalSVDDecomposition(var d: TLVec; E: TLVec;
  n: TLInt; IsUpper: Boolean; IsFractionalAccuracyRequired: Boolean;
  var u: TLMatrix; NRU: TLInt; var c: TLMatrix;
  NCC: TLInt; var VT: TLMatrix; NCVT: TLInt): Boolean;
begin
  E := DynamicArrayCopy(E);
  Result := BidiagonalSVDDecompositionInternal(d, E, n, IsUpper,
    IsFractionalAccuracyRequired, u, 1, NRU, c, 1, NCC, VT, 1, NCVT);
end;

(* ************************************************************************
  Internal working subroutine for bidiagonal decomposition
  ************************************************************************ *)
function BidiagonalSVDDecompositionInternal(var d: TLVec;
  E: TLVec; n: TLInt; IsUpper: Boolean;
  IsFractionalAccuracyRequired: Boolean; var u: TLMatrix;
  UStart: TLInt; NRU: TLInt; var c: TLMatrix;
  CStart: TLInt; NCC: TLInt; var VT: TLMatrix;
  VStart: TLInt; NCVT: TLInt): Boolean;
var
  i              : TLInt;
  IDIR           : TLInt;
  ISUB           : TLInt;
  ITER           : TLInt;
  j              : TLInt;
  LL             : TLInt;
  LLL            : TLInt;
  M              : TLInt;
  MAXIT          : TLInt;
  OLDLL          : TLInt;
  OLDM           : TLInt;
  ABSE           : TLFloat;
  ABSS           : TLFloat;
  COSL           : TLFloat;
  COSR           : TLFloat;
  cs             : TLFloat;
  Eps            : TLFloat;
  f              : TLFloat;
  g              : TLFloat;
  h              : TLFloat;
  MU             : TLFloat;
  OLDCS          : TLFloat;
  OLDSN          : TLFloat;
  r              : TLFloat;
  Shift          : TLFloat;
  SIGMN          : TLFloat;
  SIGMX          : TLFloat;
  SINL           : TLFloat;
  SINR           : TLFloat;
  SLL            : TLFloat;
  SMAX           : TLFloat;
  SMIN           : TLFloat;
  SMINL          : TLFloat;
  SMINLO         : TLFloat;
  SMINOA         : TLFloat;
  sn             : TLFloat;
  THRESH         : TLFloat;
  TOL            : TLFloat;
  TOLMUL         : TLFloat;
  UNFL           : TLFloat;
  WORK0          : TLVec;
  Work1          : TLVec;
  Work2          : TLVec;
  WORK3          : TLVec;
  MAXITR         : TLInt;
  MatrixSplitFlag: Boolean;
  IterFlag       : Boolean;
  UTemp          : TLVec;
  VTTemp         : TLVec;
  CTemp          : TLVec;
  ETemp          : TLVec;
  RightSide      : Boolean;
  FwdDir         : Boolean;
  tmp            : TLFloat;
  MM1            : TLInt;
  MM0            : TLInt;
  BChangeDir     : Boolean;
  UEnd           : TLInt;
  CEnd           : TLInt;
  VEnd           : TLInt;
  i_             : TLInt;
begin
  E := DynamicArrayCopy(E);
  Result := True;
  if n = 0 then
    begin
      Exit;
    end;
  if n = 1 then
    begin
      if AP_FP_Less(d[1], 0) then
        begin
          d[1] := -d[1];
          if NCVT > 0 then
            begin
              APVMul(@VT[VStart][0], VStart, VStart + NCVT - 1, -1);
            end;
        end;
      Exit;
    end;

  //
  // init
  //
  SetLength(WORK0, n  );
  SetLength(Work1, n  );
  SetLength(Work2, n  );
  SetLength(WORK3, n  );
  UEnd := UStart + Max(NRU - 1, 0);
  VEnd := VStart + Max(NCVT - 1, 0);
  CEnd := CStart + Max(NCC - 1, 0);
  SetLength(UTemp, UEnd + 1);
  SetLength(VTTemp, VEnd + 1);
  SetLength(CTemp, CEnd + 1);
  MAXITR := 12;
  RightSide := True;
  FwdDir := True;

  //
  // resize E from N-1 to N
  //
  SetLength(ETemp, n + 1);
  i := 1;
  while i <= n - 1 do
    begin
      ETemp[i] := E[i];
      inc(i);
    end;
  SetLength(E, n + 1);
  i := 1;
  while i <= n - 1 do
    begin
      E[i] := ETemp[i];
      inc(i);
    end;
  E[n] := 0;
  IDIR := 0;

  //
  // Get machine constants
  //
  Eps := MachineEpsilon;
  UNFL := MinRealNumber;

  //
  // If matrix lower bidiagonal, rotate to be upper bidiagonal
  // by applying Givens rotations on the left
  //
  if not IsUpper then
    begin
      i := 1;
      while i <= n - 1 do
        begin
          GenerateRotation(d[i], E[i], cs, sn, r);
          d[i] := r;
          E[i] := sn * d[i + 1];
          d[i + 1] := cs * d[i + 1];
          WORK0[i] := cs;
          Work1[i] := sn;
          inc(i);
        end;

      //
      // Update singular vectors if desired
      //
      if NRU > 0 then
        begin
          ApplyRotationsFromTheRight(FwdDir, UStart, UEnd, 1 + UStart - 1,
            n + UStart - 1, WORK0, Work1, u, UTemp);
        end;
      if NCC > 0 then
        begin
          ApplyRotationsFromTheLeft(FwdDir, 1 + CStart - 1, n + CStart - 1, CStart,
            CEnd, WORK0, Work1, c, CTemp);
        end;
    end;

  //
  // Compute singular values to relative accuracy TOL
  // (By setting TOL to be negative, algorithm will compute
  // singular values to absolute accuracy ABS(TOL)*norm(input matrix))
  //
  TOLMUL := Max(10, Min(100, Power(Eps, -0.125)));
  TOL := TOLMUL * Eps;
  if not IsFractionalAccuracyRequired then
    begin
      TOL := -TOL;
    end;

  //
  // Compute approximate maximum, minimum singular values
  //
  SMAX := 0;
  i := 1;
  while i <= n do
    begin
      SMAX := Max(SMAX, AbsReal(d[i]));
      inc(i);
    end;
  i := 1;
  while i <= n - 1 do
    begin
      SMAX := Max(SMAX, AbsReal(E[i]));
      inc(i);
    end;
  SMINL := 0;
  if AP_FP_Greater_Eq(TOL, 0) then
    begin

      //
      // Relative accuracy desired
      //
      SMINOA := AbsReal(d[1]);
      if AP_FP_NEq(SMINOA, 0) then
        begin
          MU := SMINOA;
          i := 2;
          while i <= n do
            begin
              MU := AbsReal(d[i]) * (MU / (MU + AbsReal(E[i - 1])));
              SMINOA := Min(SMINOA, MU);
              if AP_FP_Eq(SMINOA, 0) then
                begin
                  Break;
                end;
              inc(i);
            end;
        end;
      SMINOA := SMINOA / Sqrt(n);
      THRESH := Max(TOL * SMINOA, MAXITR * n * n * UNFL);
    end
  else
    begin

      //
      // Absolute accuracy desired
      //
      THRESH := Max(AbsReal(TOL) * SMAX, MAXITR * n * n * UNFL);
    end;

  //
  // Prepare for main iteration loop for the singular values
  // (MAXIT is the maximum number of passes through the inner
  // loop permitted before nonconvergence signalled.)
  //
  MAXIT := MAXITR * n * n;
  ITER := 0;
  OLDLL := -1;
  OLDM := -1;

  //
  // M points to last element of unconverged part of matrix
  //
  M := n;

  //
  // Begin main iteration loop
  //
  while True do
    begin

      //
      // Check for convergence or exceeding iteration count
      //
      if M <= 1 then
        begin
          Break;
        end;
      if ITER > MAXIT then
        begin
          Result := False;
          Exit;
        end;

      //
      // Find diagonal block of matrix to work on
      //
      if AP_FP_Less(TOL, 0) and AP_FP_Less_Eq(AbsReal(d[M]), THRESH) then
        begin
          d[M] := 0;
        end;
      SMAX := AbsReal(d[M]);
      SMIN := SMAX;
      MatrixSplitFlag := False;
      LLL := 1;
      while LLL <= M - 1 do
        begin
          LL := M - LLL;
          ABSS := AbsReal(d[LL]);
          ABSE := AbsReal(E[LL]);
          if AP_FP_Less(TOL, 0) and AP_FP_Less_Eq(ABSS, THRESH) then
            begin
              d[LL] := 0;
            end;
          if AP_FP_Less_Eq(ABSE, THRESH) then
            begin
              MatrixSplitFlag := True;
              Break;
            end;
          SMIN := Min(SMIN, ABSS);
          SMAX := Max(SMAX, Max(ABSS, ABSE));
          inc(LLL);
        end;
      if not MatrixSplitFlag then
        begin
          LL := 0;
        end
      else
        begin

          //
          // Matrix splits since E(LL) = 0
          //
          E[LL] := 0;
          if LL = M - 1 then
            begin

              //
              // Convergence of bottom singular value, return to top of loop
              //
              M := M - 1;
              Continue;
            end;
        end;
      LL := LL + 1;

      //
      // E(LL) through E(M-1) are nonzero, E(LL-1) is zero
      //
      if LL = M - 1 then
        begin

          //
          // 2 by 2 block, handle separately
          //
          SvdV2X2(d[M - 1], E[M - 1], d[M], SIGMN, SIGMX, SINR, COSR, SINL, COSL);
          d[M - 1] := SIGMX;
          E[M - 1] := 0;
          d[M] := SIGMN;

          //
          // Compute singular vectors, if desired
          //
          if NCVT > 0 then
            begin
              MM0 := M + (VStart - 1);
              MM1 := M - 1 + (VStart - 1);
              APVMove(@VTTemp[0], VStart, VEnd, @VT[MM1][0], VStart, VEnd, COSR);
              APVAdd(@VTTemp[0], VStart, VEnd, @VT[MM0][0], VStart, VEnd, SINR);
              APVMul(@VT[MM0][0], VStart, VEnd, COSR);
              APVSub(@VT[MM0][0], VStart, VEnd, @VT[MM1][0], VStart, VEnd, SINR);
              APVMove(@VT[MM1][0], VStart, VEnd, @VTTemp[0], VStart, VEnd);
            end;
          if NRU > 0 then
            begin
              MM0 := M + UStart - 1;
              MM1 := M - 1 + UStart - 1;
              for i_ := UStart to UEnd do
                begin
                  UTemp[i_] := COSL * u[i_, MM1];
                end;
              for i_ := UStart to UEnd do
                begin
                  UTemp[i_] := UTemp[i_] + SINL * u[i_, MM0];
                end;
              for i_ := UStart to UEnd do
                begin
                  u[i_, MM0] := COSL * u[i_, MM0];
                end;
              for i_ := UStart to UEnd do
                begin
                  u[i_, MM0] := u[i_, MM0] - SINL * u[i_, MM1];
                end;
              for i_ := UStart to UEnd do
                begin
                  u[i_, MM1] := UTemp[i_];
                end;
            end;
          if NCC > 0 then
            begin
              MM0 := M + CStart - 1;
              MM1 := M - 1 + CStart - 1;
              APVMove(@CTemp[0], CStart, CEnd, @c[MM1][0], CStart, CEnd, COSL);
              APVAdd(@CTemp[0], CStart, CEnd, @c[MM0][0], CStart, CEnd, SINL);
              APVMul(@c[MM0][0], CStart, CEnd, COSL);
              APVSub(@c[MM0][0], CStart, CEnd, @c[MM1][0], CStart, CEnd, SINL);
              APVMove(@c[MM1][0], CStart, CEnd, @CTemp[0], CStart, CEnd);
            end;
          M := M - 2;
          Continue;
        end;

      //
      // If working on new submatrix, choose shift direction
      // (from larger end diagonal element towards smaller)
      //
      // Previously was
      // "if (LL>OLDM) or (M<OLDLL) then"
      // fixed thanks to Michael Rolle < m@rolle.name >
      // Very strange that LAPACK still contains it.
      //
      BChangeDir := False;
      if (IDIR = 1) and AP_FP_Less(AbsReal(d[LL]), 1.0E-3 * AbsReal(d[M])) then
        begin
          BChangeDir := True;
        end;
      if (IDIR = 2) and AP_FP_Less(AbsReal(d[M]), 1.0E-3 * AbsReal(d[LL])) then
        begin
          BChangeDir := True;
        end;
      if (LL <> OLDLL) or (M <> OLDM) or BChangeDir then
        begin
          if AP_FP_Greater_Eq(AbsReal(d[LL]), AbsReal(d[M])) then
            begin

              //
              // Chase bulge from top (big end) to bottom (small end)
              //
              IDIR := 1;
            end
          else
            begin

              //
              // Chase bulge from bottom (big end) to top (small end)
              //
              IDIR := 2;
            end;
        end;

      //
      // Apply convergence tests
      //
      if IDIR = 1 then
        begin

          //
          // Run convergence test in forward direction
          // First apply standard test to bottom of matrix
          //
          if AP_FP_Less_Eq(AbsReal(E[M - 1]), AbsReal(TOL) * AbsReal(d[M])) or
            AP_FP_Less(TOL, 0) and AP_FP_Less_Eq(AbsReal(E[M - 1]), THRESH) then
            begin
              E[M - 1] := 0;
              Continue;
            end;
          if AP_FP_Greater_Eq(TOL, 0) then
            begin

              //
              // If relative accuracy desired,
              // apply convergence criterion forward
              //
              MU := AbsReal(d[LL]);
              SMINL := MU;
              IterFlag := False;
              LLL := LL;
              while LLL <= M - 1 do
                begin
                  if AP_FP_Less_Eq(AbsReal(E[LLL]), TOL * MU) then
                    begin
                      E[LLL] := 0;
                      IterFlag := True;
                      Break;
                    end;
                  SMINLO := SMINL;
                  MU := AbsReal(d[LLL + 1]) * (MU / (MU + AbsReal(E[LLL])));
                  SMINL := Min(SMINL, MU);
                  inc(LLL);
                end;
              if IterFlag then
                begin
                  Continue;
                end;
            end;
        end
      else
        begin

          //
          // Run convergence test in backward direction
          // First apply standard test to top of matrix
          //
          if AP_FP_Less_Eq(AbsReal(E[LL]), AbsReal(TOL) * AbsReal(d[LL])) or
            AP_FP_Less(TOL, 0) and AP_FP_Less_Eq(AbsReal(E[LL]), THRESH) then
            begin
              E[LL] := 0;
              Continue;
            end;
          if AP_FP_Greater_Eq(TOL, 0) then
            begin

              //
              // If relative accuracy desired,
              // apply convergence criterion backward
              //
              MU := AbsReal(d[M]);
              SMINL := MU;
              IterFlag := False;
              LLL := M - 1;
              while LLL >= LL do
                begin
                  if AP_FP_Less_Eq(AbsReal(E[LLL]), TOL * MU) then
                    begin
                      E[LLL] := 0;
                      IterFlag := True;
                      Break;
                    end;
                  SMINLO := SMINL;
                  MU := AbsReal(d[LLL]) * (MU / (MU + AbsReal(E[LLL])));
                  SMINL := Min(SMINL, MU);
                  dec(LLL);
                end;
              if IterFlag then
                begin
                  Continue;
                end;
            end;
        end;
      OLDLL := LL;
      OLDM := M;

      //
      // Compute shift.  First, test if shifting would ruin relative
      // accuracy, and if so set the shift to zero.
      //
      if AP_FP_Greater_Eq(TOL, 0) and AP_FP_Less_Eq(n * TOL * (SMINL / SMAX),
        Max(Eps, 0.01 * TOL)) then
        begin

          //
          // Use a zero shift to avoid loss of relative accuracy
          //
          Shift := 0;
        end
      else
        begin

          //
          // Compute the shift from 2-by-2 block at end of matrix
          //
          if IDIR = 1 then
            begin
              SLL := AbsReal(d[LL]);
              Svd2X2(d[M - 1], E[M - 1], d[M], Shift, r);
            end
          else
            begin
              SLL := AbsReal(d[M]);
              Svd2X2(d[LL], E[LL], d[LL + 1], Shift, r);
            end;

          //
          // Test if shift negligible, and if so set to zero
          //
          if AP_FP_Greater(SLL, 0) then
            begin
              if AP_FP_Less(AP_Sqr(Shift / SLL), Eps) then
                begin
                  Shift := 0;
                end;
            end;
        end;

      //
      // Increment iteration count
      //
      ITER := ITER + M - LL;

      //
      // If SHIFT = 0, do simplified QR iteration
      //
      if AP_FP_Eq(Shift, 0) then
        begin
          if IDIR = 1 then
            begin

              //
              // Chase bulge from top to bottom
              // Save cosines and sines for later singular vector updates
              //
              cs := 1;
              OLDCS := 1;
              i := LL;
              while i <= M - 1 do
                begin
                  GenerateRotation(d[i] * cs, E[i], cs, sn, r);
                  if i > LL then
                    begin
                      E[i - 1] := OLDSN * r;
                    end;
                  GenerateRotation(OLDCS * r, d[i + 1] * sn, OLDCS, OLDSN, tmp);
                  d[i] := tmp;
                  WORK0[i - LL + 1] := cs;
                  Work1[i - LL + 1] := sn;
                  Work2[i - LL + 1] := OLDCS;
                  WORK3[i - LL + 1] := OLDSN;
                  inc(i);
                end;
              h := d[M] * cs;
              d[M] := h * OLDCS;
              E[M - 1] := h * OLDSN;

              //
              // Update singular vectors
              //
              if NCVT > 0 then
                begin
                  ApplyRotationsFromTheLeft(FwdDir, LL + VStart - 1, M + VStart - 1,
                    VStart, VEnd, WORK0, Work1, VT, VTTemp);
                end;
              if NRU > 0 then
                begin
                  ApplyRotationsFromTheRight(FwdDir, UStart, UEnd, LL + UStart - 1,
                    M + UStart - 1, Work2, WORK3, u, UTemp);
                end;
              if NCC > 0 then
                begin
                  ApplyRotationsFromTheLeft(FwdDir, LL + CStart - 1, M + CStart - 1,
                    CStart, CEnd, Work2, WORK3, c, CTemp);
                end;

              //
              // Test convergence
              //
              if AP_FP_Less_Eq(AbsReal(E[M - 1]), THRESH) then
                begin
                  E[M - 1] := 0;
                end;
            end
          else
            begin

              //
              // Chase bulge from bottom to top
              // Save cosines and sines for later singular vector updates
              //
              cs := 1;
              OLDCS := 1;
              i := M;
              while i >= LL + 1 do
                begin
                  GenerateRotation(d[i] * cs, E[i - 1], cs, sn, r);
                  if i < M then
                    begin
                      E[i] := OLDSN * r;
                    end;
                  GenerateRotation(OLDCS * r, d[i - 1] * sn, OLDCS, OLDSN, tmp);
                  d[i] := tmp;
                  WORK0[i - LL] := cs;
                  Work1[i - LL] := -sn;
                  Work2[i - LL] := OLDCS;
                  WORK3[i - LL] := -OLDSN;
                  dec(i);
                end;
              h := d[LL] * cs;
              d[LL] := h * OLDCS;
              E[LL] := h * OLDSN;

              //
              // Update singular vectors
              //
              if NCVT > 0 then
                begin
                  ApplyRotationsFromTheLeft(not FwdDir, LL + VStart - 1, M + VStart - 1,
                    VStart, VEnd, Work2, WORK3, VT, VTTemp);
                end;
              if NRU > 0 then
                begin
                  ApplyRotationsFromTheRight(not FwdDir, UStart, UEnd, LL + UStart - 1,
                    M + UStart - 1, WORK0, Work1, u, UTemp);
                end;
              if NCC > 0 then
                begin
                  ApplyRotationsFromTheLeft(not FwdDir, LL + CStart - 1, M + CStart - 1,
                    CStart, CEnd, WORK0, Work1, c, CTemp);
                end;

              //
              // Test convergence
              //
              if AP_FP_Less_Eq(AbsReal(E[LL]), THRESH) then
                begin
                  E[LL] := 0;
                end;
            end;
        end
      else
        begin

          //
          // Use nonzero shift
          //
          if IDIR = 1 then
            begin

              //
              // Chase bulge from top to bottom
              // Save cosines and sines for later singular vector updates
              //
              f := (AbsReal(d[LL]) - Shift) *
                (ExtSignBDSQR(1, d[LL]) + Shift / d[LL]);
              g := E[LL];
              i := LL;
              while i <= M - 1 do
                begin
                  GenerateRotation(f, g, COSR, SINR, r);
                  if i > LL then
                    begin
                      E[i - 1] := r;
                    end;
                  f := COSR * d[i] + SINR * E[i];
                  E[i] := COSR * E[i] - SINR * d[i];
                  g := SINR * d[i + 1];
                  d[i + 1] := COSR * d[i + 1];
                  GenerateRotation(f, g, COSL, SINL, r);
                  d[i] := r;
                  f := COSL * E[i] + SINL * d[i + 1];
                  d[i + 1] := COSL * d[i + 1] - SINL * E[i];
                  if i < M - 1 then
                    begin
                      g := SINL * E[i + 1];
                      E[i + 1] := COSL * E[i + 1];
                    end;
                  WORK0[i - LL + 1] := COSR;
                  Work1[i - LL + 1] := SINR;
                  Work2[i - LL + 1] := COSL;
                  WORK3[i - LL + 1] := SINL;
                  inc(i);
                end;
              E[M - 1] := f;

              //
              // Update singular vectors
              //
              if NCVT > 0 then
                begin
                  ApplyRotationsFromTheLeft(FwdDir, LL + VStart - 1, M + VStart - 1,
                    VStart, VEnd, WORK0, Work1, VT, VTTemp);
                end;
              if NRU > 0 then
                begin
                  ApplyRotationsFromTheRight(FwdDir, UStart, UEnd, LL + UStart - 1,
                    M + UStart - 1, Work2, WORK3, u, UTemp);
                end;
              if NCC > 0 then
                begin
                  ApplyRotationsFromTheLeft(FwdDir, LL + CStart - 1, M + CStart - 1,
                    CStart, CEnd, Work2, WORK3, c, CTemp);
                end;

              //
              // Test convergence
              //
              if AP_FP_Less_Eq(AbsReal(E[M - 1]), THRESH) then
                begin
                  E[M - 1] := 0;
                end;
            end
          else
            begin

              //
              // Chase bulge from bottom to top
              // Save cosines and sines for later singular vector updates
              //
              f := (AbsReal(d[M]) - Shift) * (ExtSignBDSQR(1, d[M]) + Shift / d[M]);
              g := E[M - 1];
              i := M;
              while i >= LL + 1 do
                begin
                  GenerateRotation(f, g, COSR, SINR, r);
                  if i < M then
                    begin
                      E[i] := r;
                    end;
                  f := COSR * d[i] + SINR * E[i - 1];
                  E[i - 1] := COSR * E[i - 1] - SINR * d[i];
                  g := SINR * d[i - 1];
                  d[i - 1] := COSR * d[i - 1];
                  GenerateRotation(f, g, COSL, SINL, r);
                  d[i] := r;
                  f := COSL * E[i - 1] + SINL * d[i - 1];
                  d[i - 1] := COSL * d[i - 1] - SINL * E[i - 1];
                  if i > LL + 1 then
                    begin
                      g := SINL * E[i - 2];
                      E[i - 2] := COSL * E[i - 2];
                    end;
                  WORK0[i - LL] := COSR;
                  Work1[i - LL] := -SINR;
                  Work2[i - LL] := COSL;
                  WORK3[i - LL] := -SINL;
                  dec(i);
                end;
              E[LL] := f;

              //
              // Test convergence
              //
              if AP_FP_Less_Eq(AbsReal(E[LL]), THRESH) then
                begin
                  E[LL] := 0;
                end;

              //
              // Update singular vectors if desired
              //
              if NCVT > 0 then
                begin
                  ApplyRotationsFromTheLeft(not FwdDir, LL + VStart - 1, M + VStart - 1,
                    VStart, VEnd, Work2, WORK3, VT, VTTemp);
                end;
              if NRU > 0 then
                begin
                  ApplyRotationsFromTheRight(not FwdDir, UStart, UEnd, LL + UStart - 1,
                    M + UStart - 1, WORK0, Work1, u, UTemp);
                end;
              if NCC > 0 then
                begin
                  ApplyRotationsFromTheLeft(not FwdDir, LL + CStart - 1, M + CStart - 1,
                    CStart, CEnd, WORK0, Work1, c, CTemp);
                end;
            end;
        end;

      //
      // QR iteration finished, go back and check convergence
      //
      Continue;
    end;

  //
  // All singular values converged, so make them positive
  //
  i := 1;
  while i <= n do
    begin
      if AP_FP_Less(d[i], 0) then
        begin
          d[i] := -d[i];

          //
          // Change sign of singular vectors, if desired
          //
          if NCVT > 0 then
            begin
              APVMul(@VT[i + VStart - 1][0], VStart, VEnd, -1);
            end;
        end;
      inc(i);
    end;

  //
  // Sort the singular values into decreasing order (insertion sort on
  // singular values, but only one transposition per singular vector)
  //
  i := 1;
  while i <= n - 1 do
    begin

      //
      // Scan for smallest D(I)
      //
      ISUB := 1;
      SMIN := d[1];
      j := 2;
      while j <= n + 1 - i do
        begin
          if AP_FP_Less_Eq(d[j], SMIN) then
            begin
              ISUB := j;
              SMIN := d[j];
            end;
          inc(j);
        end;
      if ISUB <> n + 1 - i then
        begin

          //
          // Swap singular values and vectors
          //
          d[ISUB] := d[n + 1 - i];
          d[n + 1 - i] := SMIN;
          if NCVT > 0 then
            begin
              j := n + 1 - i;
              APVMove(@VTTemp[0], VStart, VEnd, @VT[ISUB + VStart - 1][0],
                VStart, VEnd);
              APVMove(@VT[ISUB + VStart - 1][0], VStart, VEnd, @VT[j + VStart - 1][0],
                VStart, VEnd);
              APVMove(@VT[j + VStart - 1][0], VStart, VEnd, @VTTemp[0], VStart, VEnd);
            end;
          if NRU > 0 then
            begin
              j := n + 1 - i;
              for i_ := UStart to UEnd do
                begin
                  UTemp[i_] := u[i_, ISUB + UStart - 1];
                end;
              for i_ := UStart to UEnd do
                begin
                  u[i_, ISUB + UStart - 1] := u[i_, j + UStart - 1];
                end;
              for i_ := UStart to UEnd do
                begin
                  u[i_, j + UStart - 1] := UTemp[i_];
                end;
            end;
          if NCC > 0 then
            begin
              j := n + 1 - i;
              APVMove(@CTemp[0], CStart, CEnd, @c[ISUB + CStart - 1][0],
                CStart, CEnd);
              APVMove(@c[ISUB + CStart - 1][0], CStart, CEnd, @c[j + CStart - 1][0],
                CStart, CEnd);
              APVMove(@c[j + CStart - 1][0], CStart, CEnd, @CTemp[0], CStart, CEnd);
            end;
        end;
      inc(i);
    end;
end;

function ExtSignBDSQR(a: TLFloat; b: TLFloat): TLFloat;
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

procedure Svd2X2(f: TLFloat; g: TLFloat; h: TLFloat;
  var SSMIN: TLFloat; var SSMAX: TLFloat);
var
  AAS : TLFloat;
  at  : TLFloat;
  AU  : TLFloat;
  c   : TLFloat;
  FA  : TLFloat;
  FHMN: TLFloat;
  FHMX: TLFloat;
  GA  : TLFloat;
  hA  : TLFloat;
begin
  FA := AbsReal(f);
  GA := AbsReal(g);
  hA := AbsReal(h);
  FHMN := Min(FA, hA);
  FHMX := Max(FA, hA);
  if AP_FP_Eq(FHMN, 0) then
    begin
      SSMIN := 0;
      if AP_FP_Eq(FHMX, 0) then
        begin
          SSMAX := GA;
        end
      else
        begin
          SSMAX := Max(FHMX, GA) * Sqrt(1 + AP_Sqr(Min(FHMX, GA) / Max(FHMX, GA)));
        end;
    end
  else
    begin
      if AP_FP_Less(GA, FHMX) then
        begin
          AAS := 1 + FHMN / FHMX;
          at := (FHMX - FHMN) / FHMX;
          AU := AP_Sqr(GA / FHMX);
          c := 2 / (Sqrt(AAS * AAS + AU) + Sqrt(at * at + AU));
          SSMIN := FHMN * c;
          SSMAX := FHMX / c;
        end
      else
        begin
          AU := FHMX / GA;
          if AP_FP_Eq(AU, 0) then
            begin

              //
              // Avoid possible harmful underflow if exponent range
              // asymmetric (true SSMIN may not underflow even if
              // AU underflows)
              //
              SSMIN := FHMN * FHMX / GA;
              SSMAX := GA;
            end
          else
            begin
              AAS := 1 + FHMN / FHMX;
              at := (FHMX - FHMN) / FHMX;
              c := 1 / (Sqrt(1 + AP_Sqr(AAS * AU)) + Sqrt(1 + AP_Sqr(at * AU)));
              SSMIN := FHMN * c * AU;
              SSMIN := SSMIN + SSMIN;
              SSMAX := GA / (c + c);
            end;
        end;
    end;
end;

procedure SvdV2X2(f: TLFloat; g: TLFloat; h: TLFloat;
  var SSMIN: TLFloat; var SSMAX: TLFloat; var SNR: TLFloat;
  var CSR: TLFloat; var SNL: TLFloat; var CSL: TLFloat);
var
  GASMAL: Boolean;
  SWP   : Boolean;
  PMAX  : TLInt;
  a     : TLFloat;
  CLT   : TLFloat;
  CRT   : TLFloat;
  d     : TLFloat;
  FA    : TLFloat;
  FT    : TLFloat;
  GA    : TLFloat;
  GT    : TLFloat;
  hA    : TLFloat;
  HT    : TLFloat;
  L     : TLFloat;
  M     : TLFloat;
  MM    : TLFloat;
  r     : TLFloat;
  s     : TLFloat;
  SLT   : TLFloat;
  SRT   : TLFloat;
  t     : TLFloat;
  Temp  : TLFloat;
  TSIGN : TLFloat;
  TT    : TLFloat;
  v     : TLFloat;
begin
  FT := f;
  FA := AbsReal(FT);
  HT := h;
  hA := AbsReal(h);

  //
  // PMAX points to the maximum absolute element of matrix
  // PMAX = 1 if F largest in absolute values
  // PMAX = 2 if G largest in absolute values
  // PMAX = 3 if H largest in absolute values
  //
  PMAX := 1;
  SWP := AP_FP_Greater(hA, FA);
  if SWP then
    begin

      //
      // Now FA .ge. HA
      //
      PMAX := 3;
      Temp := FT;
      FT := HT;
      HT := Temp;
      Temp := FA;
      FA := hA;
      hA := Temp;
    end;
  GT := g;
  GA := AbsReal(GT);
  if AP_FP_Eq(GA, 0) then
    begin

      //
      // Diagonal matrix
      //
      SSMIN := hA;
      SSMAX := FA;
      CLT := 1;
      CRT := 1;
      SLT := 0;
      SRT := 0;
    end
  else
    begin
      GASMAL := True;
      if AP_FP_Greater(GA, FA) then
        begin
          PMAX := 2;
          if AP_FP_Less(FA / GA, MachineEpsilon) then
            begin

              //
              // Case of very large GA
              //
              GASMAL := False;
              SSMAX := GA;
              if AP_FP_Greater(hA, 1) then
                begin
                  v := GA / hA;
                  SSMIN := FA / v;
                end
              else
                begin
                  v := FA / GA;
                  SSMIN := v * hA;
                end;
              CLT := 1;
              SLT := HT / GT;
              SRT := 1;
              CRT := FT / GT;
            end;
        end;
      if GASMAL then
        begin

          //
          // Normal case
          //
          d := FA - hA;
          if AP_FP_Eq(d, FA) then
            begin
              L := 1;
            end
          else
            begin
              L := d / FA;
            end;
          M := GT / FT;
          t := 2 - L;
          MM := M * M;
          TT := t * t;
          s := Sqrt(TT + MM);
          if AP_FP_Eq(L, 0) then
            begin
              r := AbsReal(M);
            end
          else
            begin
              r := Sqrt(L * L + MM);
            end;
          a := 0.5 * (s + r);
          SSMIN := hA / a;
          SSMAX := FA * a;
          if AP_FP_Eq(MM, 0) then
            begin

              //
              // Note that M is very tiny
              //
              if AP_FP_Eq(L, 0) then
                begin
                  t := ExtSignBDSQR(2, FT) * ExtSignBDSQR(1, GT);
                end
              else
                begin
                  t := GT / ExtSignBDSQR(d, FT) + M / t;
                end;
            end
          else
            begin
              t := (M / (s + t) + M / (r + L)) * (1 + a);
            end;
          L := Sqrt(t * t + 4);
          CRT := 2 / L;
          SRT := t / L;
          CLT := (CRT + SRT * M) / a;
          v := HT / FT;
          SLT := v * SRT / a;
        end;
    end;
  if SWP then
    begin
      CSL := SRT;
      SNL := CRT;
      CSR := SLT;
      SNR := CLT;
    end
  else
    begin
      CSL := CLT;
      SNL := SLT;
      CSR := CRT;
      SNR := SRT;
    end;

  //
  // Correct signs of SSMAX and SSMIN
  //
  if PMAX = 1 then
    begin
      TSIGN := ExtSignBDSQR(1, CSR) * ExtSignBDSQR(1, CSL) * ExtSignBDSQR(1, f);
    end;
  if PMAX = 2 then
    begin
      TSIGN := ExtSignBDSQR(1, SNR) * ExtSignBDSQR(1, CSL) * ExtSignBDSQR(1, g);
    end;
  if PMAX = 3 then
    begin
      TSIGN := ExtSignBDSQR(1, SNR) * ExtSignBDSQR(1, SNL) * ExtSignBDSQR(1, h);
    end;
  SSMAX := ExtSignBDSQR(SSMAX, TSIGN);
  SSMIN := ExtSignBDSQR(SSMIN, TSIGN * ExtSignBDSQR(1, f) * ExtSignBDSQR(1, h));
end; 
 
 
