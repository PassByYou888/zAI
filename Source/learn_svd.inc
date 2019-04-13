{ ****************************************************************************** }
{ * Singular value decomposition support, by QQ 600585@qq.com                  * }
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

(* ************************************************************************
  Singular value decomposition of a rectangular matrix.

  The algorithm calculates the singular value decomposition of a matrix of
  size MxN: A = U * S * V^T

  The algorithm finds the singular values and, optionally, matrices U and V^T.
  The algorithm can find both first min(M,N) columns of matrix U and rows of
  matrix V^T (singular vectors), and matrices U and V^T wholly (of sizes MxM
  and NxN respectively).

  Take into account that the subroutine does not return matrix V but V^T.

  Input parameters:
  A           -   matrix to be decomposed.
  Array whose indexes range within [0..M-1, 0..N-1].
  M           -   number of rows in matrix A.
  N           -   number of columns in matrix A.
  UNeeded     -   0, 1 or 2. See the description of the parameter U.
  VTNeeded    -   0, 1 or 2. See the description of the parameter VT.
  AdditionalMemory -
  If the parameter:
  * equals 0, the algorithm doesnæŠ?use additional
  memory (lower requirements, lower performance).
  * equals 1, the algorithm uses additional
  memory of size min(M,N)*min(M,N) of real numbers.
  It often speeds up the algorithm.
  * equals 2, the algorithm uses additional
  memory of size M*min(M,N) of real numbers.
  It allows to get a maximum performance.
  The recommended value of the parameter is 2.

  Output parameters:
  W           -   contains singular values in descending order.
  U           -   if UNeeded=0, U isn't changed, the left singular vectors
  are not calculated.
  if Uneeded=1, U contains left singular vectors (first
  min(M,N) columns of matrix U). Array whose indexes range
  within [0..M-1, 0..Min(M,N)-1].
  if UNeeded=2, U contains matrix U wholly. Array whose
  indexes range within [0..M-1, 0..M-1].
  VT          -   if VTNeeded=0, VT isnæŠ?changed, the right singular vectors
  are not calculated.
  if VTNeeded=1, VT contains right singular vectors (first
  min(M,N) rows of matrix V^T). Array whose indexes range
  within [0..min(M,N)-1, 0..N-1].
  if VTNeeded=2, VT contains matrix V^T wholly. Array whose
  indexes range within [0..N-1, 0..N-1].
  ************************************************************************ *)
function RMatrixSVD(a: TLMatrix; const M, n, UNeeded, VTNeeded, AdditionalMemory: TLInt;
  var w: TLVec; var u: TLMatrix; var VT: TLMatrix): Boolean;
var
  TauQ   : TLVec;
  TauP   : TLVec;
  TAU    : TLVec;
  E      : TLVec;
  Work   : TLVec;
  t2     : TLMatrix;
  IsUpper: Boolean;
  MinMN  : TLInt;
  NCU    : TLInt;
  NRVT   : TLInt;
  NRU    : TLInt;
  NCVT   : TLInt;
  i      : TLInt;
  j      : TLInt;
begin
  a := DynamicArrayCopy(a);
  Result := True;
  if (M = 0) or (n = 0) then
    begin
      Exit;
    end;
  Assert((UNeeded >= 0) and (UNeeded <= 2), 'SVDDecomposition: wrong parameters!');
  Assert((VTNeeded >= 0) and (VTNeeded <= 2), 'SVDDecomposition: wrong parameters!');
  Assert((AdditionalMemory >= 0) and (AdditionalMemory <= 2), 'SVDDecomposition: wrong parameters!');

  //
  // initialize
  //
  MinMN := Min(M, n);
  SetLength(w, MinMN + 1);
  NCU := 0;
  NRU := 0;
  if UNeeded = 1 then
    begin
      NRU := M;
      NCU := MinMN;
      SetLength(u, NRU  , NCU  );
    end;
  if UNeeded = 2 then
    begin
      NRU := M;
      NCU := M;
      SetLength(u, NRU  , NCU  );
    end;
  NRVT := 0;
  NCVT := 0;
  if VTNeeded = 1 then
    begin
      NRVT := MinMN;
      NCVT := n;
      SetLength(VT, NRVT  , NCVT  );
    end;
  if VTNeeded = 2 then
    begin
      NRVT := n;
      NCVT := n;
      SetLength(VT, NRVT  , NCVT  );
    end;

  //
  // M much larger than N
  // Use bidiagonal reduction with QR-decomposition
  //
  if AP_FP_Greater(M, 1.6 * n) then
    begin
      if UNeeded = 0 then
        begin

          //
          // No left singular vectors to be computed
          //
          RMatrixQR(a, M, n, TAU);
          i := 0;
          while i <= n - 1 do
            begin
              j := 0;
              while j <= i - 1 do
                begin
                  a[i, j] := 0;
                  inc(j);
                end;
              inc(i);
            end;
          RMatrixBD(a, n, n, TauQ, TauP);
          RMatrixBDUnpackPT(a, n, n, TauP, NRVT, VT);
          RMatrixBDUnpackDiagonals(a, n, n, IsUpper, w, E);
          Result := RMatrixBDSVD(w, E, n, IsUpper, False, u, 0, a, 0, VT, NCVT);
          Exit;
        end
      else
        begin

          //
          // Left singular vectors (may be full matrix U) to be computed
          //
          RMatrixQR(a, M, n, TAU);
          RMatrixQRUnpackQ(a, M, n, TAU, NCU, u);
          i := 0;
          while i <= n - 1 do
            begin
              j := 0;
              while j <= i - 1 do
                begin
                  a[i, j] := 0;
                  inc(j);
                end;
              inc(i);
            end;
          RMatrixBD(a, n, n, TauQ, TauP);
          RMatrixBDUnpackPT(a, n, n, TauP, NRVT, VT);
          RMatrixBDUnpackDiagonals(a, n, n, IsUpper, w, E);
          if AdditionalMemory < 1 then
            begin

              //
              // No additional memory can be used
              //
              RMatrixBDMultiplyByQ(a, n, n, TauQ, u, M, n, True, False);
              Result := RMatrixBDSVD(w, E, n, IsUpper, False, u, M, a, 0, VT, NCVT);
            end
          else
            begin

              //
              // Large U. Transforming intermediate matrix T2
              //
              SetLength(Work, Max(M, n) + 1);
              RMatrixBDUnpackQ(a, n, n, TauQ, n, t2);
              CopyMatrix(u, 0, M - 1, 0, n - 1, a, 0, M - 1, 0, n - 1);
              InplaceTranspose(t2, 0, n - 1, 0, n - 1, Work);
              Result := RMatrixBDSVD(w, E, n, IsUpper, False, u, 0, t2, n, VT, NCVT);
              MatrixMatrixMultiply(a, 0, M - 1, 0, n - 1, False, t2, 0, n - 1, 0,
                n - 1, True, 1.0, u, 0, M - 1, 0, n - 1, 0.0, Work);
            end;
          Exit;
        end;
    end;

  //
  // N much larger than M
  // Use bidiagonal reduction with LQ-decomposition
  //
  if AP_FP_Greater(n, 1.6 * M) then
    begin
      if VTNeeded = 0 then
        begin

          //
          // No right singular vectors to be computed
          //
          RMatrixLQ(a, M, n, TAU);
          i := 0;
          while i <= M - 1 do
            begin
              j := i + 1;
              while j <= M - 1 do
                begin
                  a[i, j] := 0;
                  inc(j);
                end;
              inc(i);
            end;
          RMatrixBD(a, M, M, TauQ, TauP);
          RMatrixBDUnpackQ(a, M, M, TauQ, NCU, u);
          RMatrixBDUnpackDiagonals(a, M, M, IsUpper, w, E);
          SetLength(Work, M + 1);
          InplaceTranspose(u, 0, NRU - 1, 0, NCU - 1, Work);
          Result := RMatrixBDSVD(w, E, M, IsUpper, False, a, 0, u, NRU, VT, 0);
          InplaceTranspose(u, 0, NRU - 1, 0, NCU - 1, Work);
          Exit;
        end
      else
        begin

          //
          // Right singular vectors (may be full matrix VT) to be computed
          //
          RMatrixLQ(a, M, n, TAU);
          RMatrixLQUnpackQ(a, M, n, TAU, NRVT, VT);
          i := 0;
          while i <= M - 1 do
            begin
              j := i + 1;
              while j <= M - 1 do
                begin
                  a[i, j] := 0;
                  inc(j);
                end;
              inc(i);
            end;
          RMatrixBD(a, M, M, TauQ, TauP);
          RMatrixBDUnpackQ(a, M, M, TauQ, NCU, u);
          RMatrixBDUnpackDiagonals(a, M, M, IsUpper, w, E);
          SetLength(Work, Max(M, n) + 1);
          InplaceTranspose(u, 0, NRU - 1, 0, NCU - 1, Work);
          if AdditionalMemory < 1 then
            begin

              //
              // No additional memory available
              //
              RMatrixBDMultiplyByP(a, M, M, TauP, VT, M, n, False, True);
              Result := RMatrixBDSVD(w, E, M, IsUpper, False, a, 0, u, NRU, VT, n);
            end
          else
            begin

              //
              // Large VT. Transforming intermediate matrix T2
              //
              RMatrixBDUnpackPT(a, M, M, TauP, M, t2);
              Result := RMatrixBDSVD(w, E, M, IsUpper, False, a, 0, u, NRU, t2, M);
              CopyMatrix(VT, 0, M - 1, 0, n - 1, a, 0, M - 1, 0, n - 1);
              MatrixMatrixMultiply(t2, 0, M - 1, 0, M - 1, False, a, 0, M - 1, 0,
                n - 1, False, 1.0, VT, 0, M - 1, 0, n - 1, 0.0, Work);
            end;
          InplaceTranspose(u, 0, NRU - 1, 0, NCU - 1, Work);
          Exit;
        end;
    end;

  //
  // M<=N
  // We can use inplace transposition of U to get rid of columnwise operations
  //
  if M <= n then
    begin
      RMatrixBD(a, M, n, TauQ, TauP);
      RMatrixBDUnpackQ(a, M, n, TauQ, NCU, u);
      RMatrixBDUnpackPT(a, M, n, TauP, NRVT, VT);
      RMatrixBDUnpackDiagonals(a, M, n, IsUpper, w, E);
      SetLength(Work, M + 1);
      InplaceTranspose(u, 0, NRU - 1, 0, NCU - 1, Work);
      Result := RMatrixBDSVD(w, E, MinMN, IsUpper, False, a, 0, u, NRU, VT, NCVT);
      InplaceTranspose(u, 0, NRU - 1, 0, NCU - 1, Work);
      Exit;
    end;

  //
  // Simple bidiagonal reduction
  //
  RMatrixBD(a, M, n, TauQ, TauP);
  RMatrixBDUnpackQ(a, M, n, TauQ, NCU, u);
  RMatrixBDUnpackPT(a, M, n, TauP, NRVT, VT);
  RMatrixBDUnpackDiagonals(a, M, n, IsUpper, w, E);
  if (AdditionalMemory < 2) or (UNeeded = 0) then
    begin

      //
      // We cant use additional memory or there is no need in such operations
      //
      Result := RMatrixBDSVD(w, E, MinMN, IsUpper, False, u, NRU, a, 0, VT, NCVT);
    end
  else
    begin

      //
      // We can use additional memory
      //
      SetLength(t2, MinMN  , M  );
      CopyAndTranspose(u, 0, M - 1, 0, MinMN - 1, t2, 0, MinMN - 1, 0, M - 1);
      Result := RMatrixBDSVD(w, E, MinMN, IsUpper, False, u, 0, t2, M, VT, NCVT);
      CopyAndTranspose(t2, 0, MinMN - 1, 0, M - 1, u, 0, M - 1, 0, MinMN - 1);
    end;
end; 
 
 
