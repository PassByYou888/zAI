{ ****************************************************************************** }
{ * test support, by QQ 600585@qq.com                                          * }
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

function InternalCMatrixTRInverse(var a: TLComplexMatrix; n: TLInt; IsUpper: Boolean; IsunitTriangular: Boolean): Boolean;
var
  NOunit: Boolean;
  i: TLInt;
  j: TLInt;
  v: TLComplex;
  AJJ: TLComplex;
  t: TLComplexVec;
  i_: TLInt;
begin
  Result := True;
  SetLength(t, n);

  //
  // Test the input parameters.
  //
  NOunit := not IsunitTriangular;
  if IsUpper then
    begin

      //
      // Compute inverse of upper triangular matrix.
      //
      j := 0;
      while j <= n - 1 do
        begin
          if NOunit then
            begin
              if C_EqualR(a[j, j], 0) then
                begin
                  Result := False;
                  exit;
                end;
              a[j, j] := C_RDiv(1, a[j, j]);
              AJJ := C_Opposite(a[j, j]);
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
              for i_ := 0 to j - 1 do
                begin
                  t[i_] := a[i_, j];
                end;
              i := 0;
              while i <= j - 1 do
                begin
                  if i + 1 < j then
                    begin
                      v := C_Complex(0.0);
                      for i_ := i + 1 to j - 1 do
                        begin
                          v := C_Add(v, C_Mul(a[i, i_], t[i_]));
                        end;
                    end
                  else
                    begin
                      v := C_Complex(0);
                    end;
                  if NOunit then
                    begin
                      a[i, j] := C_Add(v, C_Mul(a[i, i], t[i]));
                    end
                  else
                    begin
                      a[i, j] := C_Add(v, t[i]);
                    end;
                  inc(i);
                end;
              for i_ := 0 to j - 1 do
                begin
                  a[i_, j] := C_Mul(AJJ, a[i_, j]);
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
          if NOunit then
            begin
              if C_EqualR(a[j, j], 0) then
                begin
                  Result := False;
                  exit;
                end;
              a[j, j] := C_RDiv(1, a[j, j]);
              AJJ := C_Opposite(a[j, j]);
            end
          else
            begin
              AJJ := C_Complex(-1);
            end;
          if j + 1 < n then
            begin

              //
              // Compute elements j+1:n of j-th column.
              //
              for i_ := j + 1 to n - 1 do
                begin
                  t[i_] := a[i_, j];
                end;
              i := j + 1;
              while i <= n - 1 do
                begin
                  if i > j + 1 then
                    begin
                      v := C_Complex(0.0);
                      for i_ := j + 1 to i - 1 do
                        begin
                          v := C_Add(v, C_Mul(a[i, i_], t[i_]));
                        end;
                    end
                  else
                    begin
                      v := C_Complex(0);
                    end;
                  if NOunit then
                    begin
                      a[i, j] := C_Add(v, C_Mul(a[i, i], t[i]));
                    end
                  else
                    begin
                      a[i, j] := C_Add(v, t[i]);
                    end;
                  inc(i);
                end;
              for i_ := j + 1 to n - 1 do
                begin
                  a[i_, j] := C_Mul(AJJ, a[i_, j]);
                end;
            end;
          dec(j);
        end;
    end;
end;

function InternalRMatrixTRInverse(var a: TLMatrix; n: TLInt;
  IsUpper: Boolean; IsunitTriangular: Boolean): Boolean;
var
  NOunit: Boolean;
  i: TLInt;
  j: TLInt;
  v: TLFloat;
  AJJ: TLFloat;
  t: TLVec;
  i_: TLInt;
begin
  Result := True;
  SetLength(t, n);

  //
  // Test the input parameters.
  //
  NOunit := not IsunitTriangular;
  if IsUpper then
    begin

      //
      // Compute inverse of upper triangular matrix.
      //
      j := 0;
      while j <= n - 1 do
        begin
          if NOunit then
            begin
              if AP_FP_Eq(a[j, j], 0) then
                begin
                  Result := False;
                  exit;
                end;
              a[j, j] := 1 / a[j, j];
              AJJ := -a[j, j];
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
              for i_ := 0 to j - 1 do
                begin
                  t[i_] := a[i_, j];
                end;
              i := 0;
              while i <= j - 1 do
                begin
                  if i < j - 1 then
                    begin
                      v := APVDotProduct(@a[i][0], i + 1, j - 1, @t[0], i + 1, j - 1);
                    end
                  else
                    begin
                      v := 0;
                    end;
                  if NOunit then
                    begin
                      a[i, j] := v + a[i, i] * t[i];
                    end
                  else
                    begin
                      a[i, j] := v + t[i];
                    end;
                  inc(i);
                end;
              for i_ := 0 to j - 1 do
                begin
                  a[i_, j] := AJJ * a[i_, j];
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
          if NOunit then
            begin
              if AP_FP_Eq(a[j, j], 0) then
                begin
                  Result := False;
                  exit;
                end;
              a[j, j] := 1 / a[j, j];
              AJJ := -a[j, j];
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
              for i_ := j + 1 to n - 1 do
                begin
                  t[i_] := a[i_, j];
                end;
              i := j + 1;
              while i <= n - 1 do
                begin
                  if i > j + 1 then
                    begin
                      v := APVDotProduct(@a[i][0], j + 1, i - 1, @t[0], j + 1, i - 1);
                    end
                  else
                    begin
                      v := 0;
                    end;
                  if NOunit then
                    begin
                      a[i, j] := v + a[i, i] * t[i];
                    end
                  else
                    begin
                      a[i, j] := v + t[i];
                    end;
                  inc(i);
                end;
              for i_ := j + 1 to n - 1 do
                begin
                  a[i_, j] := AJJ * a[i_, j];
                end;
            end;
          dec(j);
        end;
    end;
end;

procedure RefCMatrixRightTRSM(M: TLInt; n: TLInt;
  const a: TLComplexMatrix; i1: TLInt; j1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var x: TLComplexMatrix; i2: TLInt; j2: TLInt);
var
  a1: TLComplexMatrix;
  a2: TLComplexMatrix;
  TX: TLComplexVec;
  i: TLInt;
  j: TLInt;
  VC: TLComplex;
  RUpper: Boolean;
  i_: TLInt;
  i1_: TLInt;
begin
  if n * M = 0 then
    begin
      exit;
    end;
  SetLength(a1, n, n);
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          a1[i, j] := C_Complex(0);
          inc(j);
        end;
      inc(i);
    end;
  if IsUpper then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          j := i;
          while j <= n - 1 do
            begin
              a1[i, j] := a[i1 + i, j1 + j];
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
              a1[i, j] := a[i1 + i, j1 + j];
              inc(j);
            end;
          inc(i);
        end;
    end;
  RUpper := IsUpper;
  if IsUnit then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          a1[i, i] := C_Complex(1);
          inc(i);
        end;
    end;
  SetLength(a2, n, n);
  if OpType = 0 then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          j := 0;
          while j <= n - 1 do
            begin
              a2[i, j] := a1[i, j];
              inc(j);
            end;
          inc(i);
        end;
    end;
  if OpType = 1 then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          j := 0;
          while j <= n - 1 do
            begin
              a2[i, j] := a1[j, i];
              inc(j);
            end;
          inc(i);
        end;
      RUpper := not RUpper;
    end;
  if OpType = 2 then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          j := 0;
          while j <= n - 1 do
            begin
              a2[i, j] := Conj(a1[j, i]);
              inc(j);
            end;
          inc(i);
        end;
      RUpper := not RUpper;
    end;
  InternalCMatrixTRInverse(a2, n, RUpper, False);
  SetLength(TX, n);
  i := 0;
  while i <= M - 1 do
    begin
      i1_ := (j2) - (0);
      for i_ := 0 to n - 1 do
        begin
          TX[i_] := x[i2 + i, i_ + i1_];
        end;
      j := 0;
      while j <= n - 1 do
        begin
          VC := C_Complex(0.0);
          for i_ := 0 to n - 1 do
            begin
              VC := C_Add(VC, C_Mul(TX[i_], a2[i_, j]));
            end;
          x[i2 + i, j2 + j] := VC;
          inc(j);
        end;
      inc(i);
    end;
end;

procedure RefCMatrixLeftTRSM(M: TLInt; n: TLInt;
  const a: TLComplexMatrix; i1: TLInt; j1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var x: TLComplexMatrix; i2: TLInt; j2: TLInt);
var
  a1: TLComplexMatrix;
  a2: TLComplexMatrix;
  TX: TLComplexVec;
  i: TLInt;
  j: TLInt;
  VC: TLComplex;
  RUpper: Boolean;
  i_: TLInt;
  i1_: TLInt;
begin
  if n * M = 0 then
    begin
      exit;
    end;
  SetLength(a1, M, M);
  i := 0;
  while i <= M - 1 do
    begin
      j := 0;
      while j <= M - 1 do
        begin
          a1[i, j] := C_Complex(0);
          inc(j);
        end;
      inc(i);
    end;
  if IsUpper then
    begin
      i := 0;
      while i <= M - 1 do
        begin
          j := i;
          while j <= M - 1 do
            begin
              a1[i, j] := a[i1 + i, j1 + j];
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
          while j <= i do
            begin
              a1[i, j] := a[i1 + i, j1 + j];
              inc(j);
            end;
          inc(i);
        end;
    end;
  RUpper := IsUpper;
  if IsUnit then
    begin
      i := 0;
      while i <= M - 1 do
        begin
          a1[i, i] := C_Complex(1);
          inc(i);
        end;
    end;
  SetLength(a2, M, M);
  if OpType = 0 then
    begin
      i := 0;
      while i <= M - 1 do
        begin
          j := 0;
          while j <= M - 1 do
            begin
              a2[i, j] := a1[i, j];
              inc(j);
            end;
          inc(i);
        end;
    end;
  if OpType = 1 then
    begin
      i := 0;
      while i <= M - 1 do
        begin
          j := 0;
          while j <= M - 1 do
            begin
              a2[i, j] := a1[j, i];
              inc(j);
            end;
          inc(i);
        end;
      RUpper := not RUpper;
    end;
  if OpType = 2 then
    begin
      i := 0;
      while i <= M - 1 do
        begin
          j := 0;
          while j <= M - 1 do
            begin
              a2[i, j] := Conj(a1[j, i]);
              inc(j);
            end;
          inc(i);
        end;
      RUpper := not RUpper;
    end;
  InternalCMatrixTRInverse(a2, M, RUpper, False);
  SetLength(TX, M);
  j := 0;
  while j <= n - 1 do
    begin
      i1_ := (i2) - (0);
      for i_ := 0 to M - 1 do
        begin
          TX[i_] := x[i_ + i1_, j2 + j];
        end;
      i := 0;
      while i <= M - 1 do
        begin
          VC := C_Complex(0.0);
          for i_ := 0 to M - 1 do
            begin
              VC := C_Add(VC, C_Mul(a2[i, i_], TX[i_]));
            end;
          x[i2 + i, j2 + j] := VC;
          inc(i);
        end;
      inc(j);
    end;
end;

procedure RefRMatrixRightTRSM(M: TLInt; n: TLInt;
  const a: TLMatrix; i1: TLInt; j1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var x: TLMatrix;
  i2: TLInt; j2: TLInt);
var
  a1: TLMatrix;
  a2: TLMatrix;
  TX: TLVec;
  i: TLInt;
  j: TLInt;
  vr: TLFloat;
  RUpper: Boolean;
  i_: TLInt;
begin
  if n * M = 0 then
    begin
      exit;
    end;
  SetLength(a1, n, n);
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          a1[i, j] := 0;
          inc(j);
        end;
      inc(i);
    end;
  if IsUpper then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          j := i;
          while j <= n - 1 do
            begin
              a1[i, j] := a[i1 + i, j1 + j];
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
              a1[i, j] := a[i1 + i, j1 + j];
              inc(j);
            end;
          inc(i);
        end;
    end;
  RUpper := IsUpper;
  if IsUnit then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          a1[i, i] := 1;
          inc(i);
        end;
    end;
  SetLength(a2, n, n);
  if OpType = 0 then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          j := 0;
          while j <= n - 1 do
            begin
              a2[i, j] := a1[i, j];
              inc(j);
            end;
          inc(i);
        end;
    end;
  if OpType = 1 then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          j := 0;
          while j <= n - 1 do
            begin
              a2[i, j] := a1[j, i];
              inc(j);
            end;
          inc(i);
        end;
      RUpper := not RUpper;
    end;
  InternalRMatrixTRInverse(a2, n, RUpper, False);
  SetLength(TX, n);
  i := 0;
  while i <= M - 1 do
    begin
      APVMove(@TX[0], 0, n - 1, @x[i2 + i][0], j2, j2 + n - 1);
      j := 0;
      while j <= n - 1 do
        begin
          vr := 0.0;
          for i_ := 0 to n - 1 do
            begin
              vr := vr + TX[i_] * a2[i_, j];
            end;
          x[i2 + i, j2 + j] := vr;
          inc(j);
        end;
      inc(i);
    end;
end;

procedure RefRMatrixLeftTRSM(M: TLInt; n: TLInt;
  const a: TLMatrix; i1: TLInt; j1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var x: TLMatrix;
  i2: TLInt; j2: TLInt);
var
  a1: TLMatrix;
  a2: TLMatrix;
  TX: TLVec;
  i: TLInt;
  j: TLInt;
  vr: TLFloat;
  RUpper: Boolean;
  i_: TLInt;
  i1_: TLInt;
begin
  if n * M = 0 then
    begin
      exit;
    end;
  SetLength(a1, M, M);
  i := 0;
  while i <= M - 1 do
    begin
      j := 0;
      while j <= M - 1 do
        begin
          a1[i, j] := 0;
          inc(j);
        end;
      inc(i);
    end;
  if IsUpper then
    begin
      i := 0;
      while i <= M - 1 do
        begin
          j := i;
          while j <= M - 1 do
            begin
              a1[i, j] := a[i1 + i, j1 + j];
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
          while j <= i do
            begin
              a1[i, j] := a[i1 + i, j1 + j];
              inc(j);
            end;
          inc(i);
        end;
    end;
  RUpper := IsUpper;
  if IsUnit then
    begin
      i := 0;
      while i <= M - 1 do
        begin
          a1[i, i] := 1;
          inc(i);
        end;
    end;
  SetLength(a2, M, M);
  if OpType = 0 then
    begin
      i := 0;
      while i <= M - 1 do
        begin
          j := 0;
          while j <= M - 1 do
            begin
              a2[i, j] := a1[i, j];
              inc(j);
            end;
          inc(i);
        end;
    end;
  if OpType = 1 then
    begin
      i := 0;
      while i <= M - 1 do
        begin
          j := 0;
          while j <= M - 1 do
            begin
              a2[i, j] := a1[j, i];
              inc(j);
            end;
          inc(i);
        end;
      RUpper := not RUpper;
    end;
  InternalRMatrixTRInverse(a2, M, RUpper, False);
  SetLength(TX, M);
  j := 0;
  while j <= n - 1 do
    begin
      i1_ := (i2) - (0);
      for i_ := 0 to M - 1 do
        begin
          TX[i_] := x[i_ + i1_, j2 + j];
        end;
      i := 0;
      while i <= M - 1 do
        begin
          vr := APVDotProduct(@a2[i][0], 0, M - 1, @TX[0], 0, M - 1);
          x[i2 + i, j2 + j] := vr;
          inc(i);
        end;
      inc(j);
    end;
end;

procedure RefCMatrixSYRK(n: TLInt; k: TLInt; alpha: TLFloat;
  const a: TLComplexMatrix; IA: TLInt; ja: TLInt;
  OpTypeA: TLInt; beta: TLFloat; var c: TLComplexMatrix;
  IC: TLInt; JC: TLInt; IsUpper: Boolean);
var
  AE: TLComplexMatrix;
  i: TLInt;
  j: TLInt;
  VC: TLComplex;
  i_: TLInt;
begin
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          if IsUpper and (j >= i) or not IsUpper and (j <= i) then
            begin
              if AP_FP_Eq(beta, 0) then
                begin
                  c[i + IC, j + JC] := C_Complex(0);
                end
              else
                begin
                  c[i + IC, j + JC] := C_MulR(c[i + IC, j + JC], beta);
                end;
            end;
          inc(j);
        end;
      inc(i);
    end;
  if AP_FP_Eq(alpha, 0) then
    begin
      exit;
    end;
  if n * k > 0 then
    begin
      SetLength(AE, n, k);
    end;
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= k - 1 do
        begin
          if OpTypeA = 0 then
            begin
              AE[i, j] := a[IA + i, ja + j];
            end;
          if OpTypeA = 2 then
            begin
              AE[i, j] := Conj(a[IA + j, ja + i]);
            end;
          inc(j);
        end;
      inc(i);
    end;
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          VC := C_Complex(0);
          if k > 0 then
            begin
              VC := C_Complex(0.0);
              for i_ := 0 to k - 1 do
                begin
                  VC := C_Add(VC, C_Mul(AE[i, i_], Conj(AE[j, i_])));
                end;
            end;
          VC := C_MulR(VC, alpha);
          if IsUpper and (j >= i) then
            begin
              c[IC + i, JC + j] := C_Add(VC, c[IC + i, JC + j]);
            end;
          if not IsUpper and (j <= i) then
            begin
              c[IC + i, JC + j] := C_Add(VC, c[IC + i, JC + j]);
            end;
          inc(j);
        end;
      inc(i);
    end;
end;

procedure RefRMatrixSYRK(n: TLInt; k: TLInt; alpha: TLFloat;
  const a: TLMatrix; IA: TLInt; ja: TLInt;
  OpTypeA: TLInt; beta: TLFloat; var c: TLMatrix;
  IC: TLInt; JC: TLInt; IsUpper: Boolean);
var
  AE: TLMatrix;
  i: TLInt;
  j: TLInt;
  vr: TLFloat;
begin
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          if IsUpper and (j >= i) or not IsUpper and (j <= i) then
            begin
              if AP_FP_Eq(beta, 0) then
                begin
                  c[i + IC, j + JC] := 0;
                end
              else
                begin
                  c[i + IC, j + JC] := c[i + IC, j + JC] * beta;
                end;
            end;
          inc(j);
        end;
      inc(i);
    end;
  if AP_FP_Eq(alpha, 0) then
    begin
      exit;
    end;
  if n * k > 0 then
    begin
      SetLength(AE, n, k);
    end;
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= k - 1 do
        begin
          if OpTypeA = 0 then
            begin
              AE[i, j] := a[IA + i, ja + j];
            end;
          if OpTypeA = 1 then
            begin
              AE[i, j] := a[IA + j, ja + i];
            end;
          inc(j);
        end;
      inc(i);
    end;
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          vr := 0;
          if k > 0 then
            begin
              vr := APVDotProduct(@AE[i][0], 0, k - 1, @AE[j][0], 0, k - 1);
            end;
          vr := alpha * vr;
          if IsUpper and (j >= i) then
            begin
              c[IC + i, JC + j] := vr + c[IC + i, JC + j];
            end;
          if not IsUpper and (j <= i) then
            begin
              c[IC + i, JC + j] := vr + c[IC + i, JC + j];
            end;
          inc(j);
        end;
      inc(i);
    end;
end;

procedure RefCMatrixGEMM(M: TLInt; n: TLInt; k: TLInt;
  alpha: TLComplex; const a: TLComplexMatrix; IA: TLInt;
  ja: TLInt; OpTypeA: TLInt; const b: TLComplexMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt; beta: TLComplex;
  var c: TLComplexMatrix; IC: TLInt; JC: TLInt);
var
  AE: TLComplexMatrix;
  BE: TLComplexMatrix;
  i: TLInt;
  j: TLInt;
  VC: TLComplex;
  i_: TLInt;
begin
  SetLength(AE, M, k);
  i := 0;
  while i <= M - 1 do
    begin
      j := 0;
      while j <= k - 1 do
        begin
          if OpTypeA = 0 then
            begin
              AE[i, j] := a[IA + i, ja + j];
            end;
          if OpTypeA = 1 then
            begin
              AE[i, j] := a[IA + j, ja + i];
            end;
          if OpTypeA = 2 then
            begin
              AE[i, j] := Conj(a[IA + j, ja + i]);
            end;
          inc(j);
        end;
      inc(i);
    end;
  SetLength(BE, k, n);
  i := 0;
  while i <= k - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          if OpTypeB = 0 then
            begin
              BE[i, j] := b[IB + i, JB + j];
            end;
          if OpTypeB = 1 then
            begin
              BE[i, j] := b[IB + j, JB + i];
            end;
          if OpTypeB = 2 then
            begin
              BE[i, j] := Conj(b[IB + j, JB + i]);
            end;
          inc(j);
        end;
      inc(i);
    end;
  i := 0;
  while i <= M - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          VC := C_Complex(0.0);
          for i_ := 0 to k - 1 do
            begin
              VC := C_Add(VC, C_Mul(AE[i, i_], BE[i_, j]));
            end;
          VC := C_Mul(alpha, VC);
          if C_NotEqualR(beta, 0) then
            begin
              VC := C_Add(VC, C_Mul(beta, c[IC + i, JC + j]));
            end;
          c[IC + i, JC + j] := VC;
          inc(j);
        end;
      inc(i);
    end;
end;

procedure RefRMatrixGEMM(M: TLInt; n: TLInt; k: TLInt;
  alpha: TLFloat; const a: TLMatrix; IA: TLInt;
  ja: TLInt; OpTypeA: TLInt; const b: TLMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt;
  beta: TLFloat; var c: TLMatrix; IC: TLInt; JC: TLInt);
var
  AE: TLMatrix;
  BE: TLMatrix;
  i: TLInt;
  j: TLInt;
  VC: TLFloat;
  i_: TLInt;
begin
  SetLength(AE, M, k);
  i := 0;
  while i <= M - 1 do
    begin
      j := 0;
      while j <= k - 1 do
        begin
          if OpTypeA = 0 then
            begin
              AE[i, j] := a[IA + i, ja + j];
            end;
          if OpTypeA = 1 then
            begin
              AE[i, j] := a[IA + j, ja + i];
            end;
          inc(j);
        end;
      inc(i);
    end;
  SetLength(BE, k, n);
  i := 0;
  while i <= k - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          if OpTypeB = 0 then
            begin
              BE[i, j] := b[IB + i, JB + j];
            end;
          if OpTypeB = 1 then
            begin
              BE[i, j] := b[IB + j, JB + i];
            end;
          inc(j);
        end;
      inc(i);
    end;
  i := 0;
  while i <= M - 1 do
    begin
      j := 0;
      while j <= n - 1 do
        begin
          VC := 0.0;
          for i_ := 0 to k - 1 do
            begin
              VC := VC + AE[i, i_] * BE[i_, j];
            end;
          VC := alpha * VC;
          if AP_FP_NEq(beta, 0) then
            begin
              VC := VC + beta * c[IC + i, JC + j];
            end;
          c[IC + i, JC + j] := VC;
          inc(j);
        end;
      inc(i);
    end;
end;

procedure NaiveMatrixMatrixMultiply(const a: TLMatrix; AI1: TLInt;
  AI2: TLInt; AJ1: TLInt; AJ2: TLInt; TransA: Boolean;
  const b: TLMatrix; BI1: TLInt; BI2: TLInt;
  BJ1: TLInt; BJ2: TLInt; TransB: Boolean; alpha: TLFloat;
  var c: TLMatrix; CI1: TLInt; CI2: TLInt;
  CJ1: TLInt; CJ2: TLInt; beta: TLFloat);
var
  ARows: TLInt;
  ACols: TLInt;
  BRows: TLInt;
  BCols: TLInt;
  i: TLInt;
  j: TLInt;
  k: TLInt;
  L: TLInt;
  r: TLInt;
  v: TLFloat;
  x1: TLVec;
  x2: TLVec;
  i_: TLInt;
  i1_: TLInt;
begin

  //
  // Setup
  //
  if not TransA then
    begin
      ARows := AI2 - AI1 + 1;
      ACols := AJ2 - AJ1 + 1;
    end
  else
    begin
      ARows := AJ2 - AJ1 + 1;
      ACols := AI2 - AI1 + 1;
    end;
  if not TransB then
    begin
      BRows := BI2 - BI1 + 1;
      BCols := BJ2 - BJ1 + 1;
    end
  else
    begin
      BRows := BJ2 - BJ1 + 1;
      BCols := BI2 - BI1 + 1;
    end;
  Assert(ACols = BRows, 'NaiveMatrixMatrixMultiply: incorrect matrix sizes!');
  if (ARows <= 0) or (ACols <= 0) or (BRows <= 0) or (BCols <= 0) then
    begin
      exit;
    end;
  L := ARows;
  r := BCols;
  k := ACols;
  SetLength(x1, k + 1);
  SetLength(x2, k + 1);
  i := 1;
  while i <= L do
    begin
      j := 1;
      while j <= r do
        begin
          if not TransA then
            begin
              if not TransB then
                begin
                  i1_ := (AJ1) - (BI1);
                  v := 0.0;
                  for i_ := BI1 to BI2 do
                    begin
                      v := v + b[i_, BJ1 + j - 1] * a[AI1 + i - 1, i_ + i1_];
                    end;
                end
              else
                begin
                  v := APVDotProduct(@b[BI1 + j - 1][0], BJ1, BJ2, @a[AI1 + i - 1][0],
                    AJ1, AJ2);
                end;
            end
          else
            begin
              if not TransB then
                begin
                  i1_ := (AI1) - (BI1);
                  v := 0.0;
                  for i_ := BI1 to BI2 do
                    begin
                      v := v + b[i_, BJ1 + j - 1] * a[i_ + i1_, AJ1 + i - 1];
                    end;
                end
              else
                begin
                  i1_ := (AI1) - (BJ1);
                  v := 0.0;
                  for i_ := BJ1 to BJ2 do
                    begin
                      v := v + b[BI1 + j - 1, i_] * a[i_ + i1_, AJ1 + i - 1];
                    end;
                end;
            end;
          if AP_FP_Eq(beta, 0) then
            begin
              c[CI1 + i - 1, CJ1 + j - 1] := alpha * v;
            end
          else
            begin
              c[CI1 + i - 1, CJ1 + j - 1] := beta * c[CI1 + i - 1, CJ1 + j - 1]
                + alpha * v;
            end;
          inc(j);
        end;
      inc(i);
    end;
end;

function TestTRSM(MinN: TLInt; MaxN: TLInt): Boolean;
var
  n: TLInt;
  M: TLInt;
  mx: TLInt;
  i: TLInt;
  j: TLInt;
  OpType: TLInt;
  UpperType: TLInt;
  UnitType: TLInt;
  XOffsI: TLInt;
  XOffsJ: TLInt;
  AOffsIType: TLInt;
  AOffsJType: TLInt;
  AOffsI: TLInt;
  AOffsJ: TLInt;
  RefRA: TLMatrix;
  RefRXL: TLMatrix;
  RefRXR: TLMatrix;
  RefCA: TLComplexMatrix;
  RefCXL: TLComplexMatrix;
  RefCXR: TLComplexMatrix;
  RA: TLMatrix;
  ca: TLComplexMatrix;
  RXR1: TLMatrix;
  RXL1: TLMatrix;
  CXR1: TLComplexMatrix;
  CXL1: TLComplexMatrix;
  RXR2: TLMatrix;
  RXL2: TLMatrix;
  CXR2: TLComplexMatrix;
  CXL2: TLComplexMatrix;
  Threshold: TLFloat;
begin
  Threshold := AP_Sqr(MaxN) * 100 * MachineEpsilon;
  Result := False;
  mx := MinN;
  while mx <= MaxN do
    begin

      //
      // Select random M/N in [1,MX] such that max(M,N)=MX
      //
      M := 1 + RandomInteger(mx);
      n := 1 + RandomInteger(mx);
      if AP_FP_Greater(RandomReal, 0.5) then
        begin
          M := mx;
        end
      else
        begin
          n := mx;
        end;

      //
      // Initialize RefRA/RefCA by random matrices whose upper
      // and lower triangle submatrices are non-degenerate
      // well-conditioned matrices.
      //
      // Matrix size is 2Mx2M (four copies of same MxM matrix
      // to test different offsets)
      //
      SetLength(RefRA, 2 * M, 2 * M);
      i := 0;
      while i <= M - 1 do
        begin
          j := 0;
          while j <= M - 1 do
            begin
              RefRA[i, j] := 0.2 * RandomReal - 0.1;
              inc(j);
            end;
          inc(i);
        end;
      i := 0;
      while i <= M - 1 do
        begin
          RefRA[i, i] := (2 * RandomInteger(1) - 1) * (2 * M + RandomReal);
          inc(i);
        end;
      i := 0;
      while i <= M - 1 do
        begin
          j := 0;
          while j <= M - 1 do
            begin
              RefRA[i + M, j] := RefRA[i, j];
              RefRA[i, j + M] := RefRA[i, j];
              RefRA[i + M, j + M] := RefRA[i, j];
              inc(j);
            end;
          inc(i);
        end;
      SetLength(RefCA, 2 * M, 2 * M);
      i := 0;
      while i <= M - 1 do
        begin
          j := 0;
          while j <= M - 1 do
            begin
              RefCA[i, j].x := 0.2 * RandomReal - 0.1;
              RefCA[i, j].y := 0.2 * RandomReal - 0.1;
              inc(j);
            end;
          inc(i);
        end;
      i := 0;
      while i <= M - 1 do
        begin
          RefCA[i, i].x := (2 * RandomInteger(2) - 1) * (2 * M + RandomReal);
          RefCA[i, i].y := (2 * RandomInteger(2) - 1) * (2 * M + RandomReal);
          inc(i);
        end;
      i := 0;
      while i <= M - 1 do
        begin
          j := 0;
          while j <= M - 1 do
            begin
              RefCA[i + M, j] := RefCA[i, j];
              RefCA[i, j + M] := RefCA[i, j];
              RefCA[i + M, j + M] := RefCA[i, j];
              inc(j);
            end;
          inc(i);
        end;

      //
      // Generate random XL/XR.
      //
      // XR is NxM matrix (matrix for 'Right' subroutines)
      // XL is MxN matrix (matrix for 'Left' subroutines)
      //
      SetLength(RefRXR, n, M);
      i := 0;
      while i <= n - 1 do
        begin
          j := 0;
          while j <= M - 1 do
            begin
              RefRXR[i, j] := 2 * RandomReal - 1;
              inc(j);
            end;
          inc(i);
        end;
      SetLength(RefRXL, M, n);
      i := 0;
      while i <= M - 1 do
        begin
          j := 0;
          while j <= n - 1 do
            begin
              RefRXL[i, j] := 2 * RandomReal - 1;
              inc(j);
            end;
          inc(i);
        end;
      SetLength(RefCXR, n, M);
      i := 0;
      while i <= n - 1 do
        begin
          j := 0;
          while j <= M - 1 do
            begin
              RefCXR[i, j].x := 2 * RandomReal - 1;
              RefCXR[i, j].y := 2 * RandomReal - 1;
              inc(j);
            end;
          inc(i);
        end;
      SetLength(RefCXL, M, n);
      i := 0;
      while i <= M - 1 do
        begin
          j := 0;
          while j <= n - 1 do
            begin
              RefCXL[i, j].x := 2 * RandomReal - 1;
              RefCXL[i, j].y := 2 * RandomReal - 1;
              inc(j);
            end;
          inc(i);
        end;

      //
      // test different types of operations, offsets, and so on...
      //
      // to avoid unnecessary slowdown we don't test ALL possible
      // combinations of operation types. We just generate one random
      // set of parameters and test it.
      //
      SetLength(RA, 2 * M, 2 * M);
      SetLength(RXR1, n, M);
      SetLength(RXR2, n, M);
      SetLength(RXL1, M, n);
      SetLength(RXL2, M, n);
      SetLength(ca, 2 * M, 2 * M);
      SetLength(CXR1, n, M);
      SetLength(CXR2, n, M);
      SetLength(CXL1, M, n);
      SetLength(CXL2, M, n);
      OpType := RandomInteger(3);
      UpperType := RandomInteger(2);
      UnitType := RandomInteger(2);
      XOffsI := RandomInteger(2);
      XOffsJ := RandomInteger(2);
      AOffsIType := RandomInteger(2);
      AOffsJType := RandomInteger(2);
      AOffsI := M * AOffsIType;
      AOffsJ := M * AOffsJType;

      //
      // copy A, XR, XL (fill unused parts with random garbage)
      //
      i := 0;
      while i <= 2 * M - 1 do
        begin
          j := 0;
          while j <= 2 * M - 1 do
            begin
              if (i >= AOffsI) and (i < AOffsI + M) and (j >= AOffsJ) and
                (j < AOffsJ + M) then
                begin
                  ca[i, j] := RefCA[i, j];
                  RA[i, j] := RefRA[i, j];
                end
              else
                begin
                  ca[i, j] := C_Complex(RandomReal);
                  RA[i, j] := RandomReal;
                end;
              inc(j);
            end;
          inc(i);
        end;
      i := 0;
      while i <= n - 1 do
        begin
          j := 0;
          while j <= M - 1 do
            begin
              if (i >= XOffsI) and (j >= XOffsJ) then
                begin
                  CXR1[i, j] := RefCXR[i, j];
                  CXR2[i, j] := RefCXR[i, j];
                  RXR1[i, j] := RefRXR[i, j];
                  RXR2[i, j] := RefRXR[i, j];
                end
              else
                begin
                  CXR1[i, j] := C_Complex(RandomReal);
                  CXR2[i, j] := CXR1[i, j];
                  RXR1[i, j] := RandomReal;
                  RXR2[i, j] := RXR1[i, j];
                end;
              inc(j);
            end;
          inc(i);
        end;
      i := 0;
      while i <= M - 1 do
        begin
          j := 0;
          while j <= n - 1 do
            begin
              if (i >= XOffsI) and (j >= XOffsJ) then
                begin
                  CXL1[i, j] := RefCXL[i, j];
                  CXL2[i, j] := RefCXL[i, j];
                  RXL1[i, j] := RefRXL[i, j];
                  RXL2[i, j] := RefRXL[i, j];
                end
              else
                begin
                  CXL1[i, j] := C_Complex(RandomReal);
                  CXL2[i, j] := CXL1[i, j];
                  RXL1[i, j] := RandomReal;
                  RXL2[i, j] := RXL1[i, j];
                end;
              inc(j);
            end;
          inc(i);
        end;

      //
      // Test CXR
      //
      CMatrixRightTRSM(n - XOffsI, M - XOffsJ, ca, AOffsI, AOffsJ, UpperType = 0,
        UnitType = 0, OpType, CXR1, XOffsI, XOffsJ);
      RefCMatrixRightTRSM(n - XOffsI, M - XOffsJ, ca, AOffsI, AOffsJ,
        UpperType = 0, UnitType = 0, OpType, CXR2, XOffsI, XOffsJ);
      i := 0;
      while i <= n - 1 do
        begin
          j := 0;
          while j <= M - 1 do
            begin
              Result := Result or
                AP_FP_Greater(AbsComplex(C_Sub(CXR1[i, j], CXR2[i, j])), Threshold);
              inc(j);
            end;
          inc(i);
        end;

      //
      // Test CXL
      //
      CMatrixLeftTRSM(M - XOffsI, n - XOffsJ, ca, AOffsI, AOffsJ, UpperType = 0,
        UnitType = 0, OpType, CXL1, XOffsI, XOffsJ);
      RefCMatrixLeftTRSM(M - XOffsI, n - XOffsJ, ca, AOffsI, AOffsJ,
        UpperType = 0, UnitType = 0, OpType, CXL2, XOffsI, XOffsJ);
      i := 0;
      while i <= M - 1 do
        begin
          j := 0;
          while j <= n - 1 do
            begin
              Result := Result or
                AP_FP_Greater(AbsComplex(C_Sub(CXL1[i, j], CXL2[i, j])), Threshold);
              inc(j);
            end;
          inc(i);
        end;
      if OpType < 2 then
        begin

          //
          // Test RXR
          //
          RMatrixRightTRSM(n - XOffsI, M - XOffsJ, RA, AOffsI, AOffsJ,
            UpperType = 0, UnitType = 0, OpType, RXR1, XOffsI, XOffsJ);
          RefRMatrixRightTRSM(n - XOffsI, M - XOffsJ, RA, AOffsI, AOffsJ,
            UpperType = 0, UnitType = 0, OpType, RXR2, XOffsI, XOffsJ);
          i := 0;
          while i <= n - 1 do
            begin
              j := 0;
              while j <= M - 1 do
                begin
                  Result := Result or AP_FP_Greater(AbsReal(RXR1[i, j] - RXR2[i, j]),
                    Threshold);
                  inc(j);
                end;
              inc(i);
            end;

          //
          // Test RXL
          //
          RMatrixLeftTRSM(M - XOffsI, n - XOffsJ, RA, AOffsI, AOffsJ, UpperType = 0,
            UnitType = 0, OpType, RXL1, XOffsI, XOffsJ);
          RefRMatrixLeftTRSM(M - XOffsI, n - XOffsJ, RA, AOffsI, AOffsJ,
            UpperType = 0, UnitType = 0, OpType, RXL2, XOffsI, XOffsJ);
          i := 0;
          while i <= M - 1 do
            begin
              j := 0;
              while j <= n - 1 do
                begin
                  Result := Result or AP_FP_Greater(AbsReal(RXL1[i, j] - RXL2[i, j]),
                    Threshold);
                  inc(j);
                end;
              inc(i);
            end;
        end;
      inc(mx);
    end;
end;

function TestSYRK(MinN: TLInt; MaxN: TLInt): Boolean;
var
  n: TLInt;
  k: TLInt;
  mx: TLInt;
  i: TLInt;
  j: TLInt;
  UpperType: TLInt;
  XOffsI: TLInt;
  XOffsJ: TLInt;
  AOffsIType: TLInt;
  AOffsJType: TLInt;
  AOffsI: TLInt;
  AOffsJ: TLInt;
  AlphaType: TLInt;
  BetaType: TLInt;
  RefRA: TLMatrix;
  RefRC: TLMatrix;
  RefCA: TLComplexMatrix;
  RefCC: TLComplexMatrix;
  alpha: TLFloat;
  beta: TLFloat;
  RA1: TLMatrix;
  RA2: TLMatrix;
  CA1: TLComplexMatrix;
  CA2: TLComplexMatrix;
  RC: TLMatrix;
  RCT: TLMatrix;
  CC: TLComplexMatrix;
  CCT: TLComplexMatrix;
  Threshold: TLFloat;
begin
  Threshold := MaxN * 100 * MachineEpsilon;
  Result := False;
  mx := MinN;
  while mx <= MaxN do
    begin

      //
      // Select random M/N in [1,MX] such that max(M,N)=MX
      //
      k := 1 + RandomInteger(mx);
      n := 1 + RandomInteger(mx);
      if AP_FP_Greater(RandomReal, 0.5) then
        begin
          k := mx;
        end
      else
        begin
          n := mx;
        end;

      //
      // Initialize RefRA/RefCA by random Hermitian matrices,
      // RefRC/RefCC by random matrices
      //
      // RA/CA size is 2Nx2N (four copies of same NxN matrix
      // to test different offsets)
      //
      SetLength(RefRA, 2 * n, 2 * n);
      SetLength(RefCA, 2 * n, 2 * n);
      i := 0;
      while i <= n - 1 do
        begin
          RefRA[i, i] := 2 * RandomReal - 1;
          RefCA[i, i] := C_Complex(2 * RandomReal - 1);
          j := i + 1;
          while j <= n - 1 do
            begin
              RefRA[i, j] := 2 * RandomReal - 1;
              RefCA[i, j].x := 2 * RandomReal - 1;
              RefCA[i, j].y := 2 * RandomReal - 1;
              RefRA[j, i] := RefRA[i, j];
              RefCA[j, i] := Conj(RefCA[i, j]);
              inc(j);
            end;
          inc(i);
        end;
      i := 0;
      while i <= n - 1 do
        begin
          j := 0;
          while j <= n - 1 do
            begin
              RefRA[i + n, j] := RefRA[i, j];
              RefRA[i, j + n] := RefRA[i, j];
              RefRA[i + n, j + n] := RefRA[i, j];
              RefCA[i + n, j] := RefCA[i, j];
              RefCA[i, j + n] := RefCA[i, j];
              RefCA[i + n, j + n] := RefCA[i, j];
              inc(j);
            end;
          inc(i);
        end;
      SetLength(RefRC, n, k);
      SetLength(RefCC, n, k);
      i := 0;
      while i <= n - 1 do
        begin
          j := 0;
          while j <= k - 1 do
            begin
              RefRC[i, j] := 2 * RandomReal - 1;
              RefCC[i, j].x := 2 * RandomReal - 1;
              RefCC[i, j].y := 2 * RandomReal - 1;
              inc(j);
            end;
          inc(i);
        end;

      //
      // test different types of operations, offsets, and so on...
      //
      // to avoid unnecessary slowdown we don't test ALL possible
      // combinations of operation types. We just generate one random
      // set of parameters and test it.
      //
      SetLength(RA1, 2 * n, 2 * n);
      SetLength(RA2, 2 * n, 2 * n);
      SetLength(CA1, 2 * n, 2 * n);
      SetLength(CA2, 2 * n, 2 * n);
      SetLength(RC, n, k);
      SetLength(RCT, k, n);
      SetLength(CC, n, k);
      SetLength(CCT, k, n);
      UpperType := RandomInteger(2);
      XOffsI := RandomInteger(2);
      XOffsJ := RandomInteger(2);
      AOffsIType := RandomInteger(2);
      AOffsJType := RandomInteger(2);
      AlphaType := RandomInteger(2);
      BetaType := RandomInteger(2);
      AOffsI := n * AOffsIType;
      AOffsJ := n * AOffsJType;
      alpha := AlphaType * (2 * RandomReal - 1);
      beta := BetaType * (2 * RandomReal - 1);

      //
      // copy A, C (fill unused parts with random garbage)
      //
      i := 0;
      while i <= 2 * n - 1 do
        begin
          j := 0;
          while j <= 2 * n - 1 do
            begin
              if (i >= AOffsI) and (i < AOffsI + n) and (j >= AOffsJ) and
                (j < AOffsJ + n) then
                begin
                  CA1[i, j] := RefCA[i, j];
                  CA2[i, j] := RefCA[i, j];
                  RA1[i, j] := RefRA[i, j];
                  RA2[i, j] := RefRA[i, j];
                end
              else
                begin
                  CA1[i, j] := C_Complex(RandomReal);
                  CA2[i, j] := CA1[i, j];
                  RA1[i, j] := RandomReal;
                  RA2[i, j] := RA1[i, j];
                end;
              inc(j);
            end;
          inc(i);
        end;
      i := 0;
      while i <= n - 1 do
        begin
          j := 0;
          while j <= k - 1 do
            begin
              if (i >= XOffsI) and (j >= XOffsJ) then
                begin
                  RC[i, j] := RefRC[i, j];
                  RCT[j, i] := RefRC[i, j];
                  CC[i, j] := RefCC[i, j];
                  CCT[j, i] := RefCC[i, j];
                end
              else
                begin
                  RC[i, j] := RandomReal;
                  RCT[j, i] := RC[i, j];
                  CC[i, j] := C_Complex(RandomReal);
                  CCT[j, i] := CCT[j, i];
                end;
              inc(j);
            end;
          inc(i);
        end;

      //
      // Test TLComplex
      // Only one of transform types is selected and tested
      //
      if AP_FP_Greater(RandomReal, 0.5) then
        begin
          CMatrixSYRK(n - XOffsI, k - XOffsJ, alpha, CC, XOffsI, XOffsJ, 0, beta,
            CA1, AOffsI, AOffsJ, UpperType = 0);
          RefCMatrixSYRK(n - XOffsI, k - XOffsJ, alpha, CC, XOffsI, XOffsJ, 0, beta,
            CA2, AOffsI, AOffsJ, UpperType = 0);
        end
      else
        begin
          CMatrixSYRK(n - XOffsI, k - XOffsJ, alpha, CCT, XOffsJ, XOffsI, 2, beta,
            CA1, AOffsI, AOffsJ, UpperType = 0);
          RefCMatrixSYRK(n - XOffsI, k - XOffsJ, alpha, CCT, XOffsJ, XOffsI, 2,
            beta, CA2, AOffsI, AOffsJ, UpperType = 0);
        end;
      i := 0;
      while i <= n - 1 do
        begin
          j := 0;
          while j <= n - 1 do
            begin
              Result := Result or AP_FP_Greater(AbsComplex(C_Sub(CA1[i, j], CA2[i, j])
                ), Threshold);
              inc(j);
            end;
          inc(i);
        end;

      //
      // Test real
      // Only one of transform types is selected and tested
      //
      if AP_FP_Greater(RandomReal, 0.5) then
        begin
          RMatrixSYRK(n - XOffsI, k - XOffsJ, alpha, RC, XOffsI, XOffsJ, 0, beta,
            RA1, AOffsI, AOffsJ, UpperType = 0);
          RefRMatrixSYRK(n - XOffsI, k - XOffsJ, alpha, RC, XOffsI, XOffsJ, 0, beta,
            RA2, AOffsI, AOffsJ, UpperType = 0);
        end
      else
        begin
          RMatrixSYRK(n - XOffsI, k - XOffsJ, alpha, RCT, XOffsJ, XOffsI, 1, beta,
            RA1, AOffsI, AOffsJ, UpperType = 0);
          RefRMatrixSYRK(n - XOffsI, k - XOffsJ, alpha, RCT, XOffsJ, XOffsI, 1,
            beta, RA2, AOffsI, AOffsJ, UpperType = 0);
        end;
      i := 0;
      while i <= n - 1 do
        begin
          j := 0;
          while j <= n - 1 do
            begin
              Result := Result or AP_FP_Greater(AbsReal(RA1[i, j] - RA2[i, j]),
                Threshold);
              inc(j);
            end;
          inc(i);
        end;
      inc(mx);
    end;
end;

function TestGEMM(MinN: TLInt; MaxN: TLInt): Boolean;
var
  M: TLInt;
  n: TLInt;
  k: TLInt;
  mx: TLInt;
  i: TLInt;
  j: TLInt;
  AOffsI: TLInt;
  AOffsJ: TLInt;
  AOpType: TLInt;
  AOpTypeR: TLInt;
  BOffsI: TLInt;
  BOffsJ: TLInt;
  BOpType: TLInt;
  BOpTypeR: TLInt;
  COffsI: TLInt;
  COffsJ: TLInt;
  RefRA: TLMatrix;
  RefRB: TLMatrix;
  RefRC: TLMatrix;
  RefCA: TLComplexMatrix;
  RefCB: TLComplexMatrix;
  RefCC: TLComplexMatrix;
  AlphaR: TLFloat;
  BetaR: TLFloat;
  alphaC: TLComplex;
  betaC: TLComplex;
  RC1: TLMatrix;
  RC2: TLMatrix;
  CC1: TLComplexMatrix;
  CC2: TLComplexMatrix;
  Threshold: TLFloat;
begin
  Threshold := MaxN * 100 * MachineEpsilon;
  Result := False;
  mx := MinN;
  while mx <= MaxN do
    begin

      //
      // Select random M/N/K in [1,MX] such that max(M,N,K)=MX
      //
      M := 1 + RandomInteger(mx);
      n := 1 + RandomInteger(mx);
      k := 1 + RandomInteger(mx);
      i := RandomInteger(3);
      if i = 0 then
        begin
          M := mx;
        end;
      if i = 1 then
        begin
          n := mx;
        end;
      if i = 2 then
        begin
          k := mx;
        end;

      //
      // Initialize A/B/C by random matrices with size (MaxN+1)*(MaxN+1)
      //
      SetLength(RefRA, MaxN + 1, MaxN + 1);
      SetLength(RefRB, MaxN + 1, MaxN + 1);
      SetLength(RefRC, MaxN + 1, MaxN + 1);
      SetLength(RefCA, MaxN + 1, MaxN + 1);
      SetLength(RefCB, MaxN + 1, MaxN + 1);
      SetLength(RefCC, MaxN + 1, MaxN + 1);
      i := 0;
      while i <= MaxN do
        begin
          j := 0;
          while j <= MaxN do
            begin
              RefRA[i, j] := 2 * RandomReal - 1;
              RefRB[i, j] := 2 * RandomReal - 1;
              RefRC[i, j] := 2 * RandomReal - 1;
              RefCA[i, j].x := 2 * RandomReal - 1;
              RefCA[i, j].y := 2 * RandomReal - 1;
              RefCB[i, j].x := 2 * RandomReal - 1;
              RefCB[i, j].y := 2 * RandomReal - 1;
              RefCC[i, j].x := 2 * RandomReal - 1;
              RefCC[i, j].y := 2 * RandomReal - 1;
              inc(j);
            end;
          inc(i);
        end;

      //
      // test different types of operations, offsets, and so on...
      //
      // to avoid unnecessary slowdown we don't test ALL possible
      // combinations of operation types. We just generate one random
      // set of parameters and test it.
      //
      SetLength(RC1, MaxN + 1, MaxN + 1);
      SetLength(RC2, MaxN + 1, MaxN + 1);
      SetLength(CC1, MaxN + 1, MaxN + 1);
      SetLength(CC2, MaxN + 1, MaxN + 1);
      AOffsI := RandomInteger(2);
      AOffsJ := RandomInteger(2);
      AOpType := RandomInteger(3);
      AOpTypeR := RandomInteger(2);
      BOffsI := RandomInteger(2);
      BOffsJ := RandomInteger(2);
      BOpType := RandomInteger(3);
      BOpTypeR := RandomInteger(2);
      COffsI := RandomInteger(2);
      COffsJ := RandomInteger(2);
      AlphaR := RandomInteger(2) * (2 * RandomReal - 1);
      BetaR := RandomInteger(2) * (2 * RandomReal - 1);
      if AP_FP_Greater(RandomReal, 0.5) then
        begin
          alphaC.x := 2 * RandomReal - 1;
          alphaC.y := 2 * RandomReal - 1;
        end
      else
        begin
          alphaC := C_Complex(0);
        end;
      if AP_FP_Greater(RandomReal, 0.5) then
        begin
          betaC.x := 2 * RandomReal - 1;
          betaC.y := 2 * RandomReal - 1;
        end
      else
        begin
          betaC := C_Complex(0);
        end;

      //
      // copy C
      //
      i := 0;
      while i <= MaxN do
        begin
          j := 0;
          while j <= MaxN do
            begin
              RC1[i, j] := RefRC[i, j];
              RC2[i, j] := RefRC[i, j];
              CC1[i, j] := RefCC[i, j];
              CC2[i, j] := RefCC[i, j];
              inc(j);
            end;
          inc(i);
        end;

      //
      // Test TLComplex
      //
      CMatrixGEMM(M, n, k, alphaC, RefCA, AOffsI, AOffsJ, AOpType, RefCB, BOffsI,
        BOffsJ, BOpType, betaC, CC1, COffsI, COffsJ);
      RefCMatrixGEMM(M, n, k, alphaC, RefCA, AOffsI, AOffsJ, AOpType, RefCB,
        BOffsI, BOffsJ, BOpType, betaC, CC2, COffsI, COffsJ);
      i := 0;
      while i <= MaxN do
        begin
          j := 0;
          while j <= MaxN do
            begin
              Result := Result or AP_FP_Greater(AbsComplex(C_Sub(CC1[i, j], CC2[i, j])
                ), Threshold);
              inc(j);
            end;
          inc(i);
        end;

      //
      // Test real
      //
      RMatrixGEMM(M, n, k, AlphaR, RefRA, AOffsI, AOffsJ, AOpTypeR, RefRB, BOffsI,
        BOffsJ, BOpTypeR, BetaR, RC1, COffsI, COffsJ);
      RefRMatrixGEMM(M, n, k, AlphaR, RefRA, AOffsI, AOffsJ, AOpTypeR, RefRB,
        BOffsI, BOffsJ, BOpTypeR, BetaR, RC2, COffsI, COffsJ);
      i := 0;
      while i <= MaxN do
        begin
          j := 0;
          while j <= MaxN do
            begin
              Result := Result or AP_FP_Greater(AbsReal(RC1[i, j] - RC2[i, j]),
                Threshold);
              inc(j);
            end;
          inc(i);
        end;
      inc(mx);
    end;
end;

function TestTrans(MinN: TLInt; MaxN: TLInt): Boolean;
var
  M: TLInt;
  n: TLInt;
  mx: TLInt;
  i: TLInt;
  j: TLInt;
  AOffsI: TLInt;
  AOffsJ: TLInt;
  BOffsI: TLInt;
  BOffsJ: TLInt;
  v1: TLFloat;
  v2: TLFloat;
  Threshold: TLFloat;
  RefRA: TLMatrix;
  RefRB: TLMatrix;
  RefCA: TLComplexMatrix;
  RefCB: TLComplexMatrix;
begin
  Result := False;
  Threshold := 1000 * MachineEpsilon;
  mx := MinN;
  while mx <= MaxN do
    begin

      //
      // Select random M/N in [1,MX] such that max(M,N)=MX
      // Generate random V1 and V2 which are used to fill
      // RefRB/RefCB with control values.
      //
      M := 1 + RandomInteger(mx);
      n := 1 + RandomInteger(mx);
      if RandomInteger(2) = 0 then
        begin
          M := mx;
        end
      else
        begin
          n := mx;
        end;
      v1 := RandomReal;
      v2 := RandomReal;

      //
      // Initialize A by random matrix with size (MaxN+1)*(MaxN+1)
      // Fill B with control values
      //
      SetLength(RefRA, MaxN + 1, MaxN + 1);
      SetLength(RefRB, MaxN + 1, MaxN + 1);
      SetLength(RefCA, MaxN + 1, MaxN + 1);
      SetLength(RefCB, MaxN + 1, MaxN + 1);
      i := 0;
      while i <= MaxN do
        begin
          j := 0;
          while j <= MaxN do
            begin
              RefRA[i, j] := 2 * RandomReal - 1;
              RefCA[i, j].x := 2 * RandomReal - 1;
              RefCA[i, j].y := 2 * RandomReal - 1;
              RefRB[i, j] := i * v1 + j * v2;
              RefCB[i, j] := C_Complex(i * v1 + j * v2);
              inc(j);
            end;
          inc(i);
        end;

      //
      // test different offsets (zero or one)
      //
      // to avoid unnecessary slowdown we don't test ALL possible
      // combinations of operation types. We just generate one random
      // set of parameters and test it.
      //
      AOffsI := RandomInteger(2);
      AOffsJ := RandomInteger(2);
      BOffsI := RandomInteger(2);
      BOffsJ := RandomInteger(2);
      RMatrixTranspose(M, n, RefRA, AOffsI, AOffsJ, RefRB, BOffsI, BOffsJ);
      i := 0;
      while i <= MaxN do
        begin
          j := 0;
          while j <= MaxN do
            begin
              if (i < BOffsI) or (i >= BOffsI + n) or (j < BOffsJ) or (j >= BOffsJ + M)
              then
                begin
                  Result := Result or
                    AP_FP_Greater(AbsReal(RefRB[i, j] - (v1 * i + v2 * j)), Threshold);
                end
              else
                begin
                  Result := Result or
                    AP_FP_Greater(AbsReal(RefRB[i, j] - RefRA[AOffsI + j - BOffsJ,
                    AOffsJ + i - BOffsI]), Threshold);
                end;
              inc(j);
            end;
          inc(i);
        end;
      CMatrixTranspose(M, n, RefCA, AOffsI, AOffsJ, RefCB, BOffsI, BOffsJ);
      i := 0;
      while i <= MaxN do
        begin
          j := 0;
          while j <= MaxN do
            begin
              if (i < BOffsI) or (i >= BOffsI + n) or (j < BOffsJ) or (j >= BOffsJ + M)
              then
                begin
                  Result := Result or
                    AP_FP_Greater(AbsComplex(C_SubR(RefCB[i, j], v1 * i + v2 * j)),
                    Threshold);
                end
              else
                begin
                  Result := Result or
                    AP_FP_Greater(AbsComplex(C_Sub(RefCB[i, j],
                    RefCA[AOffsI + j - BOffsJ, AOffsJ + i - BOffsI])), Threshold);
                end;
              inc(j);
            end;
          inc(i);
        end;
      inc(mx);
    end;
end;

function TestRANK1(MinN: TLInt; MaxN: TLInt): Boolean;
var
  M: TLInt;
  n: TLInt;
  mx: TLInt;
  i: TLInt;
  j: TLInt;
  AOffsI: TLInt;
  AOffsJ: TLInt;
  UOffs: TLInt;
  VOffs: TLInt;
  Threshold: TLFloat;
  RefRA: TLMatrix;
  RefRB: TLMatrix;
  RefCA: TLComplexMatrix;
  RefCB: TLComplexMatrix;
  RU: TLVec;
  RV: TLVec;
  CU: TLComplexVec;
  cv: TLComplexVec;
begin
  Result := False;
  Threshold := 1000 * MachineEpsilon;
  mx := MinN;
  while mx <= MaxN do
    begin

      //
      // Select random M/N in [1,MX] such that max(M,N)=MX
      //
      M := 1 + RandomInteger(mx);
      n := 1 + RandomInteger(mx);
      if RandomInteger(2) = 0 then
        begin
          M := mx;
        end
      else
        begin
          n := mx;
        end;

      //
      // Initialize A by random matrix with size (MaxN+1)*(MaxN+1)
      // Fill B with control values
      //
      SetLength(RefRA, MaxN + MaxN, MaxN + MaxN);
      SetLength(RefRB, MaxN + MaxN, MaxN + MaxN);
      SetLength(RefCA, MaxN + MaxN, MaxN + MaxN);
      SetLength(RefCB, MaxN + MaxN, MaxN + MaxN);
      i := 0;
      while i <= 2 * MaxN - 1 do
        begin
          j := 0;
          while j <= 2 * MaxN - 1 do
            begin
              RefRA[i, j] := 2 * RandomReal - 1;
              RefCA[i, j].x := 2 * RandomReal - 1;
              RefCA[i, j].y := 2 * RandomReal - 1;
              RefRB[i, j] := RefRA[i, j];
              RefCB[i, j] := RefCA[i, j];
              inc(j);
            end;
          inc(i);
        end;
      SetLength(RU, 2 * M);
      SetLength(CU, 2 * M);
      i := 0;
      while i <= 2 * M - 1 do
        begin
          RU[i] := 2 * RandomReal - 1;
          CU[i].x := 2 * RandomReal - 1;
          CU[i].y := 2 * RandomReal - 1;
          inc(i);
        end;
      SetLength(RV, 2 * n);
      SetLength(cv, 2 * n);
      i := 0;
      while i <= 2 * n - 1 do
        begin
          RV[i] := 2 * RandomReal - 1;
          cv[i].x := 2 * RandomReal - 1;
          cv[i].y := 2 * RandomReal - 1;
          inc(i);
        end;

      //
      // test different offsets (zero or one)
      //
      // to avoid unnecessary slowdown we don't test ALL possible
      // combinations of operation types. We just generate one random
      // set of parameters and test it.
      //
      AOffsI := RandomInteger(MaxN);
      AOffsJ := RandomInteger(MaxN);
      UOffs := RandomInteger(M);
      VOffs := RandomInteger(n);
      CMatrixRank1(M, n, RefCA, AOffsI, AOffsJ, CU, UOffs, cv, VOffs);
      i := 0;
      while i <= 2 * MaxN - 1 do
        begin
          j := 0;
          while j <= 2 * MaxN - 1 do
            begin
              if (i < AOffsI) or (i >= AOffsI + M) or (j < AOffsJ) or (j >= AOffsJ + n)
              then
                begin
                  Result := Result or
                    AP_FP_Greater(AbsComplex(C_Sub(RefCA[i, j], RefCB[i, j])),
                    Threshold);
                end
              else
                begin
                  Result := Result or
                    AP_FP_Greater(AbsComplex(C_Sub(RefCA[i, j], C_Add(RefCB[i, j],
                    C_Mul(CU[i - AOffsI + UOffs], cv[j - AOffsJ + VOffs])))),
                    Threshold);
                end;
              inc(j);
            end;
          inc(i);
        end;
      RMatrixRank1(M, n, RefRA, AOffsI, AOffsJ, RU, UOffs, RV, VOffs);
      i := 0;
      while i <= 2 * MaxN - 1 do
        begin
          j := 0;
          while j <= 2 * MaxN - 1 do
            begin
              if (i < AOffsI) or (i >= AOffsI + M) or (j < AOffsJ) or (j >= AOffsJ + n)
              then
                begin
                  Result := Result or AP_FP_Greater(AbsReal(RefRA[i, j] - RefRB[i, j]),
                    Threshold);
                end
              else
                begin
                  Result := Result or
                    AP_FP_Greater
                    (AbsReal(RefRA[i, j] - (RefRB[i, j] + RU[i - AOffsI + UOffs] *
                    RV[j - AOffsJ + VOffs])), Threshold);
                end;
              inc(j);
            end;
          inc(i);
        end;
      inc(mx);
    end;
end;

function TestMV(MinN: TLInt; MaxN: TLInt): Boolean;
var
  M: TLInt;
  n: TLInt;
  mx: TLInt;
  i: TLInt;
  j: TLInt;
  AOffsI: TLInt;
  AOffsJ: TLInt;
  XOffs: TLInt;
  YOffs: TLInt;
  OpCA: TLInt;
  OpRA: TLInt;
  Threshold: TLFloat;
  RV1: TLFloat;
  RV2: TLFloat;
  CV1: TLComplex;
  CV2: TLComplex;
  RefRA: TLMatrix;
  RefCA: TLComplexMatrix;
  RX: TLVec;
  RY: TLVec;
  Cx: TLComplexVec;
  Cy: TLComplexVec;
  i_: TLInt;
  i1_: TLInt;
begin
  Result := False;
  Threshold := 1000 * MachineEpsilon;
  mx := MinN;
  while mx <= MaxN do
    begin

      //
      // Select random M/N in [1,MX] such that max(M,N)=MX
      //
      M := 1 + RandomInteger(mx);
      n := 1 + RandomInteger(mx);
      if RandomInteger(2) = 0 then
        begin
          M := mx;
        end
      else
        begin
          n := mx;
        end;

      //
      // Initialize A by random matrix with size (MaxN+MaxN)*(MaxN+MaxN)
      // Initialize X by random vector with size (MaxN+MaxN)
      // Fill Y by control values
      //
      SetLength(RefRA, MaxN + MaxN, MaxN + MaxN);
      SetLength(RefCA, MaxN + MaxN, MaxN + MaxN);
      i := 0;
      while i <= 2 * MaxN - 1 do
        begin
          j := 0;
          while j <= 2 * MaxN - 1 do
            begin
              RefRA[i, j] := 2 * RandomReal - 1;
              RefCA[i, j].x := 2 * RandomReal - 1;
              RefCA[i, j].y := 2 * RandomReal - 1;
              inc(j);
            end;
          inc(i);
        end;
      SetLength(RX, 2 * MaxN);
      SetLength(Cx, 2 * MaxN);
      SetLength(RY, 2 * MaxN);
      SetLength(Cy, 2 * MaxN);
      i := 0;
      while i <= 2 * MaxN - 1 do
        begin
          RX[i] := 2 * RandomReal - 1;
          Cx[i].x := 2 * RandomReal - 1;
          Cx[i].y := 2 * RandomReal - 1;
          RY[i] := i;
          Cy[i] := C_Complex(i);
          inc(i);
        end;

      //
      // test different offsets (zero or one)
      //
      // to avoid unnecessary slowdown we don't test ALL possible
      // combinations of operation types. We just generate one random
      // set of parameters and test it.
      //
      AOffsI := RandomInteger(MaxN);
      AOffsJ := RandomInteger(MaxN);
      XOffs := RandomInteger(MaxN);
      YOffs := RandomInteger(MaxN);
      OpCA := RandomInteger(3);
      OpRA := RandomInteger(2);
      CMatrixMV(M, n, RefCA, AOffsI, AOffsJ, OpCA, Cx, XOffs, Cy, YOffs);
      i := 0;
      while i <= 2 * MaxN - 1 do
        begin
          if (i < YOffs) or (i >= YOffs + M) then
            begin
              Result := Result or C_NotEqualR(Cy[i], i);
            end
          else
            begin
              CV1 := Cy[i];
              if OpCA = 0 then
                begin
                  i1_ := (XOffs) - (AOffsJ);
                  CV2 := C_Complex(0.0);
                  for i_ := AOffsJ to AOffsJ + n - 1 do
                    begin
                      CV2 := C_Add(CV2, C_Mul(RefCA[AOffsI + i - YOffs, i_],
                        Cx[i_ + i1_]));
                    end;
                end;
              if OpCA = 1 then
                begin
                  i1_ := (XOffs) - (AOffsI);
                  CV2 := C_Complex(0.0);
                  for i_ := AOffsI to AOffsI + n - 1 do
                    begin
                      CV2 := C_Add(CV2, C_Mul(RefCA[i_, AOffsJ + i - YOffs],
                        Cx[i_ + i1_]));
                    end;
                end;
              if OpCA = 2 then
                begin
                  i1_ := (XOffs) - (AOffsI);
                  CV2 := C_Complex(0.0);
                  for i_ := AOffsI to AOffsI + n - 1 do
                    begin
                      CV2 := C_Add(CV2, C_Mul(Conj(RefCA[i_, AOffsJ + i - YOffs]),
                        Cx[i_ + i1_]));
                    end;
                end;
              Result := Result or AP_FP_Greater(AbsComplex(C_Sub(CV1, CV2)),
                Threshold);
            end;
          inc(i);
        end;
      RMatrixMV(M, n, RefRA, AOffsI, AOffsJ, OpRA, RX, XOffs, RY, YOffs);
      i := 0;
      while i <= 2 * MaxN - 1 do
        begin
          if (i < YOffs) or (i >= YOffs + M) then
            begin
              Result := Result or AP_FP_NEq(RY[i], i);
            end
          else
            begin
              RV1 := RY[i];
              if OpRA = 0 then
                begin
                  RV2 := APVDotProduct(@RefRA[AOffsI + i - YOffs][0], AOffsJ,
                    AOffsJ + n - 1, @RX[0], XOffs, XOffs + n - 1);
                end;
              if OpRA = 1 then
                begin
                  i1_ := (XOffs) - (AOffsI);
                  RV2 := 0.0;
                  for i_ := AOffsI to AOffsI + n - 1 do
                    begin
                      RV2 := RV2 + RefRA[i_, AOffsJ + i - YOffs] * RX[i_ + i1_];
                    end;
                end;
              Result := Result or AP_FP_Greater(AbsReal(RV1 - RV2), Threshold);
            end;
          inc(i);
        end;
      inc(mx);
    end;
end;

function TestCopy(MinN: TLInt; MaxN: TLInt): Boolean;
var
  M: TLInt;
  n: TLInt;
  mx: TLInt;
  i: TLInt;
  j: TLInt;
  AOffsI: TLInt;
  AOffsJ: TLInt;
  BOffsI: TLInt;
  BOffsJ: TLInt;
  Threshold: TLFloat;
  RV1: TLFloat;
  RV2: TLFloat;
  CV1: TLComplex;
  CV2: TLComplex;
  RA: TLMatrix;
  rb: TLMatrix;
  ca: TLComplexMatrix;
  CB: TLComplexMatrix;
begin
  Result := False;
  Threshold := 1000 * MachineEpsilon;
  mx := MinN;
  while mx <= MaxN do
    begin

      //
      // Select random M/N in [1,MX] such that max(M,N)=MX
      //
      M := 1 + RandomInteger(mx);
      n := 1 + RandomInteger(mx);
      if RandomInteger(2) = 0 then
        begin
          M := mx;
        end
      else
        begin
          n := mx;
        end;

      //
      // Initialize A by random matrix with size (MaxN+MaxN)*(MaxN+MaxN)
      // Initialize X by random vector with size (MaxN+MaxN)
      // Fill Y by control values
      //
      SetLength(RA, MaxN + MaxN, MaxN + MaxN);
      SetLength(ca, MaxN + MaxN, MaxN + MaxN);
      SetLength(rb, MaxN + MaxN, MaxN + MaxN);
      SetLength(CB, MaxN + MaxN, MaxN + MaxN);
      i := 0;
      while i <= 2 * MaxN - 1 do
        begin
          j := 0;
          while j <= 2 * MaxN - 1 do
            begin
              RA[i, j] := 2 * RandomReal - 1;
              ca[i, j].x := 2 * RandomReal - 1;
              ca[i, j].y := 2 * RandomReal - 1;
              rb[i, j] := 1 + 2 * i + 3 * j;
              CB[i, j] := C_Complex(1 + 2 * i + 3 * j);
              inc(j);
            end;
          inc(i);
        end;

      //
      // test different offsets (zero or one)
      //
      // to avoid unnecessary slowdown we don't test ALL possible
      // combinations of operation types. We just generate one random
      // set of parameters and test it.
      //
      AOffsI := RandomInteger(MaxN);
      AOffsJ := RandomInteger(MaxN);
      BOffsI := RandomInteger(MaxN);
      BOffsJ := RandomInteger(MaxN);
      CMatrixCopy(M, n, ca, AOffsI, AOffsJ, CB, BOffsI, BOffsJ);
      i := 0;
      while i <= 2 * MaxN - 1 do
        begin
          j := 0;
          while j <= 2 * MaxN - 1 do
            begin
              if (i < BOffsI) or (i >= BOffsI + M) or (j < BOffsJ) or (j >= BOffsJ + n)
              then
                begin
                  Result := Result or C_NotEqualR(CB[i, j], 1 + 2 * i + 3 * j);
                end
              else
                begin
                  Result := Result or
                    AP_FP_Greater(AbsComplex(C_Sub(ca[AOffsI + i - BOffsI,
                    AOffsJ + j - BOffsJ], CB[i, j])), Threshold);
                end;
              inc(j);
            end;
          inc(i);
        end;
      RMatrixCopy(M, n, RA, AOffsI, AOffsJ, rb, BOffsI, BOffsJ);
      i := 0;
      while i <= 2 * MaxN - 1 do
        begin
          j := 0;
          while j <= 2 * MaxN - 1 do
            begin
              if (i < BOffsI) or (i >= BOffsI + M) or (j < BOffsJ) or (j >= BOffsJ + n)
              then
                begin
                  Result := Result or AP_FP_NEq(rb[i, j], 1 + 2 * i + 3 * j);
                end
              else
                begin
                  Result := Result or
                    AP_FP_Greater(AbsReal(RA[AOffsI + i - BOffsI, AOffsJ + j - BOffsJ] -
                    rb[i, j]), Threshold);
                end;
              inc(j);
            end;
          inc(i);
        end;
      inc(mx);
    end;
end;

function TestABLAS(Silent: Boolean): Boolean;
var
  Threshold: TLFloat;
  TRSMErrors: Boolean;
  SYRKErrors: Boolean;
  GEMMErrors: Boolean;
  TRANSErrors: Boolean;
  RANK1Errors: Boolean;
  MVErrors: Boolean;
  CopyErrors: Boolean;
  WasErrors: Boolean;
  RA: TLMatrix;
begin
  TRSMErrors := False;
  SYRKErrors := False;
  GEMMErrors := False;
  TRANSErrors := False;
  RANK1Errors := False;
  MVErrors := False;
  CopyErrors := False;
  WasErrors := False;
  Threshold := 10000 * MachineEpsilon;
  TRSMErrors := TRSMErrors or TestTRSM(1, 3 * ABLASBlockSize(RA) + 1);
  SYRKErrors := SYRKErrors or TestSYRK(1, 3 * ABLASBlockSize(RA) + 1);
  GEMMErrors := GEMMErrors or TestGEMM(1, 3 * ABLASBlockSize(RA) + 1);
  TRANSErrors := TRANSErrors or TestTrans(1, 3 * ABLASBlockSize(RA) + 1);
  RANK1Errors := RANK1Errors or TestRANK1(1, 3 * ABLASBlockSize(RA) + 1);
  MVErrors := MVErrors or TestMV(1, 3 * ABLASBlockSize(RA) + 1);
  CopyErrors := CopyErrors or TestCopy(1, 3 * ABLASBlockSize(RA) + 1);

  //
  // report
  //
  WasErrors := TRSMErrors or SYRKErrors or GEMMErrors or TRANSErrors or
    RANK1Errors or MVErrors or CopyErrors;
  if not Silent then
    begin
      DoStatusNoLn(PFormat('TESTING ABLAS'#13#10'', []));
      DoStatusNoLn(PFormat('* TRSM:                                  ', []));
      if TRSMErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('* SYRK:                                  ', []));
      if SYRKErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('* GEMM:                                  ', []));
      if GEMMErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('* TRANS:                                 ', []));
      if TRANSErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('* RANK1:                                 ', []));
      if RANK1Errors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('* MV:                                    ', []));
      if MVErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('* COPY:                                  ', []));
      if CopyErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      if WasErrors then
        begin
          DoStatusNoLn(PFormat('TEST FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('TEST PASSED'#13#10'', []));
        end;
      DoStatusNoLn(PFormat(''#13#10''#13#10'', []));
    end;
  Result := not WasErrors;
end;

function TestXBLAS(Silent: Boolean): Boolean;
var
  ApproxErrors: Boolean;
  ExactnessErrors: Boolean;
  WasErrors: Boolean;
  ApproxThreshold: TLFloat;
  MaxN: TLInt;
  PassCount: TLInt;
  n: TLInt;
  i: TLInt;
  pass: TLInt;
  RV1: TLFloat;
  RV2: TLFloat;
  RV2Err: TLFloat;
  CV1: TLComplex;
  CV2: TLComplex;
  CV2Err: TLFloat;
  CV2ErrX: TLFloat;
  CV2ErrY: TLFloat;
  RX: TLVec;
  RY: TLVec;
  Cx: TLComplexVec;
  Cy: TLComplexVec;
  Temp: TLVec;
  b: TLFloat;
  s: TLFloat;
  i_: TLInt;
begin
  ApproxErrors := False;
  ExactnessErrors := False;
  WasErrors := False;
  ApproxThreshold := 1000 * MachineEpsilon;
  MaxN := 1000;
  PassCount := 10;

  //
  // tests:
  // 1. ability to calculate dot product
  // 2. higher precision
  //
  n := 1;
  while n <= MaxN do
    begin
      pass := 1;
      while pass <= PassCount do
        begin

          //
          // ability to approximately calculate real dot product
          //
          SetLength(RX, n);
          SetLength(RY, n);
          SetLength(Temp, n);
          i := 0;
          while i <= n - 1 do
            begin
              if AP_FP_Greater(RandomReal, 0.2) then
                begin
                  RX[i] := 2 * RandomReal - 1;
                end
              else
                begin
                  RX[i] := 0;
                end;
              if AP_FP_Greater(RandomReal, 0.2) then
                begin
                  RY[i] := 2 * RandomReal - 1;
                end
              else
                begin
                  RY[i] := 0;
                end;
              inc(i);
            end;
          RV1 := APVDotProduct(@RX[0], 0, n - 1, @RY[0], 0, n - 1);
          XDot(RX, RY, n, Temp, RV2, RV2Err);
          ApproxErrors := ApproxErrors or AP_FP_Greater(AbsReal(RV1 - RV2),
            ApproxThreshold);

          //
          // ability to approximately calculate TLComplex dot product
          //
          SetLength(Cx, n);
          SetLength(Cy, n);
          SetLength(Temp, 2 * n);
          i := 0;
          while i <= n - 1 do
            begin
              if AP_FP_Greater(RandomReal, 0.2) then
                begin
                  Cx[i].x := 2 * RandomReal - 1;
                  Cx[i].y := 2 * RandomReal - 1;
                end
              else
                begin
                  Cx[i] := C_Complex(0);
                end;
              if AP_FP_Greater(RandomReal, 0.2) then
                begin
                  Cy[i].x := 2 * RandomReal - 1;
                  Cy[i].y := 2 * RandomReal - 1;
                end
              else
                begin
                  Cy[i] := C_Complex(0);
                end;
              inc(i);
            end;
          CV1 := C_Complex(0.0);
          for i_ := 0 to n - 1 do
            begin
              CV1 := C_Add(CV1, C_Mul(Cx[i_], Cy[i_]));
            end;
          XCDot(Cx, Cy, n, Temp, CV2, CV2Err);
          ApproxErrors := ApproxErrors or AP_FP_Greater(AbsComplex(C_Sub(CV1, CV2)),
            ApproxThreshold);
          inc(pass);
        end;
      inc(n);
    end;

  //
  // test of precision: real
  //
  n := 50000;
  SetLength(RX, n);
  SetLength(RY, n);
  SetLength(Temp, n);
  pass := 0;
  while pass <= PassCount - 1 do
    begin
      Assert(n mod 2 = 0);

      //
      // First test: X + X + ... + X - X - X - ... - X = 1*X
      //
      s := Exp(Max(pass, 50));
      if (pass = PassCount - 1) and (pass > 1) then
        begin
          s := MaxRealNumber;
        end;
      RY[0] := (2 * RandomReal - 1) * s * Sqrt(2 * RandomReal);
      i := 1;
      while i <= n - 1 do
        begin
          RY[i] := RY[0];
          inc(i);
        end;
      i := 0;
      while i <= n div 2 - 1 do
        begin
          RX[i] := 1;
          inc(i);
        end;
      i := n div 2;
      while i <= n - 2 do
        begin
          RX[i] := -1;
          inc(i);
        end;
      RX[n - 1] := 0;
      XDot(RX, RY, n, Temp, RV2, RV2Err);
      ExactnessErrors := ExactnessErrors or AP_FP_Less(RV2Err, 0);
      ExactnessErrors := ExactnessErrors or
        AP_FP_Greater(RV2Err, 4 * MachineEpsilon * AbsReal(RY[0]));
      ExactnessErrors := ExactnessErrors or
        AP_FP_Greater(AbsReal(RV2 - RY[0]), RV2Err);

      //
      // First test: X + X + ... + X = N*X
      //
      s := Exp(Max(pass, 50));
      if (pass = PassCount - 1) and (pass > 1) then
        begin
          s := MaxRealNumber;
        end;
      RY[0] := (2 * RandomReal - 1) * s * Sqrt(2 * RandomReal);
      i := 1;
      while i <= n - 1 do
        begin
          RY[i] := RY[0];
          inc(i);
        end;
      i := 0;
      while i <= n - 1 do
        begin
          RX[i] := 1;
          inc(i);
        end;
      XDot(RX, RY, n, Temp, RV2, RV2Err);
      ExactnessErrors := ExactnessErrors or AP_FP_Less(RV2Err, 0);
      ExactnessErrors := ExactnessErrors or
        AP_FP_Greater(RV2Err, 4 * MachineEpsilon * AbsReal(RY[0]) * n);
      ExactnessErrors := ExactnessErrors or
        AP_FP_Greater(AbsReal(RV2 - n * RY[0]), RV2Err);
      inc(pass);
    end;

  //
  // test of precision: TLComplex
  //
  n := 50000;
  SetLength(Cx, n);
  SetLength(Cy, n);
  SetLength(Temp, 2 * n);
  pass := 0;
  while pass <= PassCount - 1 do
    begin
      Assert(n mod 2 = 0);

      //
      // First test: X + X + ... + X - X - X - ... - X = 1*X
      //
      s := Exp(Max(pass, 50));
      if (pass = PassCount - 1) and (pass > 1) then
        begin
          s := MaxRealNumber;
        end;
      Cy[0].x := (2 * RandomReal - 1) * s * Sqrt(2 * RandomReal);
      Cy[0].y := (2 * RandomReal - 1) * s * Sqrt(2 * RandomReal);
      i := 1;
      while i <= n - 1 do
        begin
          Cy[i] := Cy[0];
          inc(i);
        end;
      i := 0;
      while i <= n div 2 - 1 do
        begin
          Cx[i] := C_Complex(1);
          inc(i);
        end;
      i := n div 2;
      while i <= n - 2 do
        begin
          Cx[i] := C_Complex(-1);
          inc(i);
        end;
      Cx[n - 1] := C_Complex(0);
      XCDot(Cx, Cy, n, Temp, CV2, CV2Err);
      ExactnessErrors := ExactnessErrors or AP_FP_Less(CV2Err, 0);
      ExactnessErrors := ExactnessErrors or
        AP_FP_Greater(CV2Err, 4 * MachineEpsilon * AbsComplex(Cy[0]));
      ExactnessErrors := ExactnessErrors or
        AP_FP_Greater(AbsComplex(C_Sub(CV2, Cy[0])), CV2Err);

      //
      // First test: X + X + ... + X = N*X
      //
      s := Exp(Max(pass, 50));
      if (pass = PassCount - 1) and (pass > 1) then
        begin
          s := MaxRealNumber;
        end;
      Cy[0] := C_Complex((2 * RandomReal - 1) * s * Sqrt(2 * RandomReal));
      i := 1;
      while i <= n - 1 do
        begin
          Cy[i] := Cy[0];
          inc(i);
        end;
      i := 0;
      while i <= n - 1 do
        begin
          Cx[i] := C_Complex(1);
          inc(i);
        end;
      XCDot(Cx, Cy, n, Temp, CV2, CV2Err);
      ExactnessErrors := ExactnessErrors or AP_FP_Less(CV2Err, 0);
      ExactnessErrors := ExactnessErrors or
        AP_FP_Greater(CV2Err, 4 * MachineEpsilon * AbsComplex(Cy[0]) * n);
      ExactnessErrors := ExactnessErrors or
        AP_FP_Greater(AbsComplex(C_Sub(CV2, C_MulR(Cy[0], n))), CV2Err);
      inc(pass);
    end;

  //
  // report
  //
  WasErrors := ApproxErrors or ExactnessErrors;
  if not Silent then
    begin
      DoStatusNoLn(PFormat('TESTING XBLAS'#13#10'', []));
      DoStatusNoLn(PFormat('APPROX.TESTS:                            ', []));
      if ApproxErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('EXACT TESTS:                             ', []));
      if ExactnessErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      if WasErrors then
        begin
          DoStatusNoLn(PFormat('TEST FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('TEST PASSED'#13#10'', []));
        end;
      DoStatusNoLn(PFormat(''#13#10''#13#10'', []));
    end;

  //
  // end
  //
  Result := not WasErrors;
end;

function TestGaussKronrodQuadrature(Silent: Boolean): Boolean;
(* ************************************************************************
  Maps:
  0   =>  -0.9
  1   =>  -0.5
  2   =>  -0.1
  3   =>   0.0
  4   =>  +0.1
  5   =>  +0.5
  6   =>  +0.9
  7   =>  +1.0
  8   =>  +1.5
  9   =>  +2.0
  ************************************************************************ *)
  function MapKind(k: TLInt): TLFloat;
  begin
    Result := 0;
    if k = 0 then
      begin
        Result := -0.9;
      end;
    if k = 1 then
      begin
        Result := -0.5;
      end;
    if k = 2 then
      begin
        Result := -0.1;
      end;
    if k = 3 then
      begin
        Result := 0.0;
      end;
    if k = 4 then
      begin
        Result := +0.1;
      end;
    if k = 5 then
      begin
        Result := +0.5;
      end;
    if k = 6 then
      begin
        Result := +0.9;
      end;
    if k = 7 then
      begin
        Result := +1.0;
      end;
    if k = 8 then
      begin
        Result := +1.5;
      end;
    if k = 9 then
      begin
        Result := +2.0;
      end;
  end;

var
  PKind: TLInt;
  ErrTol: TLFloat;
  Eps: TLFloat;
  NonStrictErrTol: TLFloat;
  n: TLInt;
  i: TLInt;
  k: TLInt;
  Info: TLInt;
  Err: TLFloat;
  AKind: TLInt;
  BKind: TLInt;
  alphaC: TLFloat;
  betaC: TLFloat;
  x1: TLVec;
  WG1: TLVec;
  WK1: TLVec;
  x2: TLVec;
  WG2: TLVec;
  WK2: TLVec;
  Info1: TLInt;
  Info2: TLInt;
  SuccessAtLeastOnce: Boolean;
  InTblErrors: Boolean;
  VsTblErrors: Boolean;
  GenErrors: Boolean;
  WasErrors: Boolean;
begin
  InTblErrors := False;
  VsTblErrors := False;
  GenErrors := False;
  WasErrors := False;
  ErrTol := 10000 * MachineEpsilon;
  NonStrictErrTol := 1000 * ErrTol;

  //
  // test recurrence-based Legendre nodes against the precalculated table
  //
  PKind := 0;
  while PKind <= 5 do
    begin
      n := 0;
      if PKind = 0 then
        begin
          n := 15;
        end;
      if PKind = 1 then
        begin
          n := 21;
        end;
      if PKind = 2 then
        begin
          n := 31;
        end;
      if PKind = 3 then
        begin
          n := 41;
        end;
      if PKind = 4 then
        begin
          n := 51;
        end;
      if PKind = 5 then
        begin
          n := 61;
        end;
      GaussKronrodQuadratureLegendreCalc(n, Info, x1, WK1, WG1);
      GaussKronrodQuadratureLegendreTbl(n, x2, WK2, WG2, Eps);
      if Info <= 0 then
        begin
          GenErrors := True;
          Break;
        end;
      i := 0;
      while i <= n - 1 do
        begin
          VsTblErrors := VsTblErrors or
            AP_FP_Greater(AbsReal(x1[i] - x2[i]), ErrTol);
          VsTblErrors := VsTblErrors or
            AP_FP_Greater(AbsReal(WK1[i] - WK2[i]), ErrTol);
          VsTblErrors := VsTblErrors or
            AP_FP_Greater(AbsReal(WG1[i] - WG2[i]), ErrTol);
          inc(i);
        end;
      inc(PKind);
    end;

  //
  // Test recurrence-baced Gauss-Kronrod nodes against Gauss-only nodes
  // calculated with subroutines from GaussQuadrature unit.
  //
  k := 1;
  while k <= 30 do
    begin
      n := 2 * k + 1;

      //
      // Gauss-Legendre
      //
      Err := 0;
      GaussKronrodQuadratureGenerateGaussLegendre(n, Info1, x1, WK1, WG1);
      GaussQuadratureGenerateGaussLegendre(k, Info2, x2, WG2);
      if (Info1 > 0) and (Info2 > 0) then
        begin
          i := 0;
          while i <= k - 1 do
            begin
              Err := Max(Err, AbsReal(x1[2 * i + 1] - x2[i]));
              Err := Max(Err, AbsReal(WG1[2 * i + 1] - WG2[i]));
              inc(i);
            end;
        end
      else
        begin
          GenErrors := True;
        end;
      GenErrors := GenErrors or AP_FP_Greater(Err, ErrTol);
      inc(k);
    end;
  k := 1;
  while k <= 15 do
    begin
      n := 2 * k + 1;

      //
      // Gauss-Jacobi
      //
      SuccessAtLeastOnce := False;
      Err := 0;
      AKind := 0;
      while AKind <= 9 do
        begin
          BKind := 0;
          while BKind <= 9 do
            begin
              alphaC := MapKind(AKind);
              betaC := MapKind(BKind);
              GaussKronrodQuadratureGenerateGaussJacobi(n, alphaC, betaC, Info1, x1, WK1, WG1);
              GaussQuadratureGenerateGaussJacobi(k, alphaC, betaC, Info2, x2, WG2);
              if (Info1 > 0) and (Info2 > 0) then
                begin
                  SuccessAtLeastOnce := True;
                  i := 0;
                  while i <= k - 1 do
                    begin
                      Err := Max(Err, AbsReal(x1[2 * i + 1] - x2[i]));
                      Err := Max(Err, AbsReal(WG1[2 * i + 1] - WG2[i]));
                      inc(i);
                    end;
                end
              else
                begin
                  GenErrors := GenErrors or (Info1 <> -5);
                end;
              inc(BKind);
            end;
          inc(AKind);
        end;
      GenErrors := GenErrors or AP_FP_Greater(Err, ErrTol) or
        not SuccessAtLeastOnce;
      inc(k);
    end;

  //
  // end
  //
  WasErrors := InTblErrors or VsTblErrors or GenErrors;
  if not Silent then
    begin
      DoStatusNoLn(PFormat('TESTING GAUSS-KRONROD QUADRATURES'#13#10'', []));
      DoStatusNoLn(PFormat('FINAL RESULT:                             ', []));
      if WasErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('* PRE-CALCULATED TABLE:                   ', []));
      if InTblErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('* CALCULATED AGAINST THE TABLE:           ', []));
      if VsTblErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('* GENERAL PROPERTIES:                     ', []));
      if GenErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      if WasErrors then
        begin
          DoStatusNoLn(PFormat('TEST FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('TEST PASSED'#13#10'', []));
        end;
      DoStatusNoLn(PFormat(''#13#10''#13#10'', []));
    end;
  Result := not WasErrors;
end;

(* ************************************************************************
  Gauss-Legendre, another variant
  ************************************************************************ *)
procedure BuildGaussLegendreQuadrature(n: TLInt; var x: TLVec; var w: TLVec);
var
  i: TLInt;
  j: TLInt;
  r: TLFloat;
  r1: TLFloat;
  p1: TLFloat;
  p2: TLFloat;
  p3: TLFloat;
  dp3: TLFloat;
  tmp: TLFloat;
begin
  SetLength(x, n);
  SetLength(w, n);
  i := 0;
  while i <= (n + 1) div 2 - 1 do
    begin
      r := Cos(pi * (4 * i + 3) / (4 * n + 2));
      repeat
        p2 := 0;
        p3 := 1;
        j := 0;
        while j <= n - 1 do
          begin
            p1 := p2;
            p2 := p3;
            p3 := ((2 * j + 1) * r * p2 - j * p1) / (j + 1);
            inc(j);
          end;
        dp3 := n * (r * p3 - p2) / (r * r - 1);
        r1 := r;
        r := r - p3 / dp3;
      until AP_FP_Less(AbsReal(r - r1), MachineEpsilon * (1 + AbsReal(r)) * 100);
      x[i] := r;
      x[n - 1 - i] := -r;
      w[i] := 2 / ((1 - r * r) * dp3 * dp3);
      w[n - 1 - i] := 2 / ((1 - r * r) * dp3 * dp3);
      inc(i);
    end;
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= n - 2 - i do
        begin
          if AP_FP_Greater_Eq(x[j], x[j + 1]) then
            begin
              tmp := x[j];
              x[j] := x[j + 1];
              x[j + 1] := tmp;
              tmp := w[j];
              w[j] := w[j + 1];
              w[j + 1] := tmp;
            end;
          inc(j);
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Gauss-Jacobi, another variant
  ************************************************************************ *)
procedure BuildGaussJacobiQuadrature(n: TLInt; alpha: TLFloat; beta: TLFloat; var x: TLVec; var w: TLVec);
var
  i: TLInt;
  j: TLInt;
  r: TLFloat;
  r1: TLFloat;
  t1: TLFloat;
  t2: TLFloat;
  t3: TLFloat;
  p1: TLFloat;
  p2: TLFloat;
  p3: TLFloat;
  PP: TLFloat;
  AN: TLFloat;
  BN: TLFloat;
  a: TLFloat;
  b: TLFloat;
  c: TLFloat;
  tmpsgn: TLFloat;
  tmp: TLFloat;
  alfbet: TLFloat;
  Temp: TLFloat;
  ITS: TLInt;
begin
  SetLength(x, n);
  SetLength(w, n);
  i := 0;
  while i <= n - 1 do
    begin
      if i = 0 then
        begin
          AN := alpha / n;
          BN := beta / n;
          t1 := (1 + alpha) * (2.78 / (4 + n * n) + 0.768 * AN / n);
          t2 := 1 + 1.48 * AN + 0.96 * BN + 0.452 * AN * AN + 0.83 * AN * BN;
          r := (t2 - t1) / t2;
        end
      else
        begin
          if i = 1 then
            begin
              t1 := (4.1 + alpha) / ((1 + alpha) * (1 + 0.156 * alpha));
              t2 := 1 + 0.06 * (n - 8) * (1 + 0.12 * alpha) / n;
              t3 := 1 + 0.012 * beta * (1 + 0.25 * AbsReal(alpha)) / n;
              r := r - t1 * t2 * t3 * (1 - r);
            end
          else
            begin
              if i = 2 then
                begin
                  t1 := (1.67 + 0.28 * alpha) / (1 + 0.37 * alpha);
                  t2 := 1 + 0.22 * (n - 8) / n;
                  t3 := 1 + 8 * beta / ((6.28 + beta) * n * n);
                  r := r - t1 * t2 * t3 * (x[0] - r);
                end
              else
                begin
                  if i < n - 2 then
                    begin
                      r := 3 * x[i - 1] - 3 * x[i - 2] + x[i - 3];
                    end
                  else
                    begin
                      if i = n - 2 then
                        begin
                          t1 := (1 + 0.235 * beta) / (0.766 + 0.119 * beta);
                          t2 := 1 / (1 + 0.639 * (n - 4) / (1 + 0.71 * (n - 4)));
                          t3 := 1 / (1 + 20 * alpha / ((7.5 + alpha) * n * n));
                          r := r + t1 * t2 * t3 * (r - x[i - 2]);
                        end
                      else
                        begin
                          if i = n - 1 then
                            begin
                              t1 := (1 + 0.37 * beta) / (1.67 + 0.28 * beta);
                              t2 := 1 / (1 + 0.22 * (n - 8) / n);
                              t3 := 1 / (1 + 8 * alpha / ((6.28 + alpha) * n * n));
                              r := r + t1 * t2 * t3 * (r - x[i - 2]);
                            end;
                        end;
                    end;
                end;
            end;
        end;
      alfbet := alpha + beta;
      repeat
        Temp := 2 + alfbet;
        p1 := (alpha - beta + Temp * r) * 0.5;
        p2 := 1;
        j := 2;
        while j <= n do
          begin
            p3 := p2;
            p2 := p1;
            Temp := 2 * j + alfbet;
            a := 2 * j * (j + alfbet) * (Temp - 2);
            b := (Temp - 1) * (alpha * alpha - beta * beta + Temp * (Temp - 2) * r);
            c := 2 * (j - 1 + alpha) * (j - 1 + beta) * Temp;
            p1 := (b * p2 - c * p3) / a;
            inc(j);
          end;
        PP := (n * (alpha - beta - Temp * r) * p1 + 2 * (n + alpha) * (n + beta) *
          p2) / (Temp * (1 - r * r));
        r1 := r;
        r := r1 - p1 / PP;
      until AP_FP_Less(AbsReal(r - r1), MachineEpsilon * (1 + AbsReal(r)) * 100);
      x[i] := r;
      w[i] := Exp(LnGamma(alpha + n, tmpsgn) + LnGamma(beta + n, tmpsgn) -
        LnGamma(n + 1, tmpsgn) - LnGamma(n + alfbet + 1, tmpsgn)) * Temp *
        Power(2, alfbet) / (PP * p2);
      inc(i);
    end;
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= n - 2 - i do
        begin
          if AP_FP_Greater_Eq(x[j], x[j + 1]) then
            begin
              tmp := x[j];
              x[j] := x[j + 1];
              x[j + 1] := tmp;
              tmp := w[j];
              w[j] := w[j + 1];
              w[j + 1] := tmp;
            end;
          inc(j);
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Gauss-Laguerre, another variant
  ************************************************************************ *)
procedure BuildGaussLaguerreQuadrature(n: TLInt; alpha: TLFloat; var x: TLVec; var w: TLVec);
var
  i: TLInt;
  j: TLInt;
  r: TLFloat;
  r1: TLFloat;
  p1: TLFloat;
  p2: TLFloat;
  p3: TLFloat;
  dp3: TLFloat;
  tsg: TLFloat;
  tmp: TLFloat;
begin
  SetLength(x, n);
  SetLength(w, n);
  i := 0;
  while i <= n - 1 do
    begin
      if i = 0 then
        begin
          r := (1 + alpha) * (3 + 0.92 * alpha) / (1 + 2.4 * n + 1.8 * alpha);
        end
      else
        begin
          if i = 1 then
            begin
              r := r + (15 + 6.25 * alpha) / (1 + 0.9 * alpha + 2.5 * n);
            end
          else
            begin
              r := r + ((1 + 2.55 * (i - 1)) / (1.9 * (i - 1)) + 1.26 * (i - 1) *
                alpha / (1 + 3.5 * (i - 1))) / (1 + 0.3 * alpha) * (r - x[i - 2]);
            end;
        end;
      repeat
        p2 := 0;
        p3 := 1;
        j := 0;
        while j <= n - 1 do
          begin
            p1 := p2;
            p2 := p3;
            p3 := ((-r + 2 * j + alpha + 1) * p2 - (j + alpha) * p1) / (j + 1);
            inc(j);
          end;
        dp3 := (n * p3 - (n + alpha) * p2) / r;
        r1 := r;
        r := r - p3 / dp3;
      until AP_FP_Less(AbsReal(r - r1), MachineEpsilon * (1 + AbsReal(r)) * 100);
      x[i] := r;
      w[i] := -Exp(LnGamma(alpha + n, tsg) - LnGamma(n, tsg)) / (dp3 * n * p2);
      inc(i);
    end;
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= n - 2 - i do
        begin
          if AP_FP_Greater_Eq(x[j], x[j + 1]) then
            begin
              tmp := x[j];
              x[j] := x[j + 1];
              x[j + 1] := tmp;
              tmp := w[j];
              w[j] := w[j + 1];
              w[j + 1] := tmp;
            end;
          inc(j);
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Gauss-Hermite, another variant
  ************************************************************************ *)
procedure BuildGaussHermiteQuadrature(n: TLInt; var x: TLVec;
  var w: TLVec);
var
  i: TLInt;
  j: TLInt;
  r: TLFloat;
  r1: TLFloat;
  p1: TLFloat;
  p2: TLFloat;
  p3: TLFloat;
  dp3: TLFloat;
  pipm4: TLFloat;
  tmp: TLFloat;
begin
  SetLength(x, n);
  SetLength(w, n);
  pipm4 := Power(pi, -0.25);
  i := 0;
  while i <= (n + 1) div 2 - 1 do
    begin
      if i = 0 then
        begin
          r := Sqrt(2 * n + 1) - 1.85575 * Power(2 * n + 1, -AP_Float(1) / 6);
        end
      else
        begin
          if i = 1 then
            begin
              r := r - 1.14 * Power(n, 0.426) / r;
            end
          else
            begin
              if i = 2 then
                begin
                  r := 1.86 * r - 0.86 * x[0];
                end
              else
                begin
                  if i = 3 then
                    begin
                      r := 1.91 * r - 0.91 * x[1];
                    end
                  else
                    begin
                      r := 2 * r - x[i - 2];
                    end;
                end;
            end;
        end;
      repeat
        p2 := 0;
        p3 := pipm4;
        j := 0;
        while j <= n - 1 do
          begin
            p1 := p2;
            p2 := p3;
            p3 := p2 * r * Sqrt(AP_Float(2) / (j + 1)) - p1 *
              Sqrt(AP_Float(j) / (j + 1));
            inc(j);
          end;
        dp3 := Sqrt(2 * j) * p2;
        r1 := r;
        r := r - p3 / dp3;
      until AP_FP_Less(AbsReal(r - r1), MachineEpsilon * (1 + AbsReal(r)) * 100);
      x[i] := r;
      w[i] := 2 / (dp3 * dp3);
      x[n - 1 - i] := -x[i];
      w[n - 1 - i] := w[i];
      inc(i);
    end;
  i := 0;
  while i <= n - 1 do
    begin
      j := 0;
      while j <= n - 2 - i do
        begin
          if AP_FP_Greater_Eq(x[j], x[j + 1]) then
            begin
              tmp := x[j];
              x[j] := x[j + 1];
              x[j + 1] := tmp;
              tmp := w[j];
              w[j] := w[j + 1];
              w[j + 1] := tmp;
            end;
          inc(j);
        end;
      inc(i);
    end;
end;

function TestGaussQuadrature(Silent: Boolean): Boolean;
(* ************************************************************************
  Maps:
  0   =>  -0.9
  1   =>  -0.5
  2   =>  -0.1
  3   =>   0.0
  4   =>  +0.1
  5   =>  +0.5
  6   =>  +0.9
  7   =>  +1.0
  8   =>  +1.5
  9   =>  +2.0
  ************************************************************************ *)
  function MapKind(k: TLInt): TLFloat;
  begin
    Result := 0;
    if k = 0 then
      begin
        Result := -0.9;
      end;
    if k = 1 then
      begin
        Result := -0.5;
      end;
    if k = 2 then
      begin
        Result := -0.1;
      end;
    if k = 3 then
      begin
        Result := 0.0;
      end;
    if k = 4 then
      begin
        Result := +0.1;
      end;
    if k = 5 then
      begin
        Result := +0.5;
      end;
    if k = 6 then
      begin
        Result := +0.9;
      end;
    if k = 7 then
      begin
        Result := +1.0;
      end;
    if k = 8 then
      begin
        Result := +1.5;
      end;
    if k = 9 then
      begin
        Result := +2.0;
      end;
  end;

var
  alpha: TLVec;
  beta: TLVec;
  x: TLVec;
  w: TLVec;
  x2: TLVec;
  W2: TLVec;
  Err: TLFloat;
  n: TLInt;
  i: TLInt;
  Info: TLInt;
  AKind: TLInt;
  BKind: TLInt;
  alphaC: TLFloat;
  betaC: TLFloat;
  ErrTol: TLFloat;
  NonStrictErrTol: TLFloat;
  StrictErrTol: TLFloat;
  RecErrors: Boolean;
  SpecErrors: Boolean;
  WasErrors: Boolean;
begin
  RecErrors := False;
  SpecErrors := False;
  WasErrors := False;
  ErrTol := 1.0E-12;
  NonStrictErrTol := 1.0E-6;
  StrictErrTol := 1000 * MachineEpsilon;

  //
  // Three tests for rec-based Gauss quadratures with known weights/nodes:
  // 1. Gauss-Legendre with N=2
  // 2. Gauss-Legendre with N=5
  // 3. Gauss-Chebyshev with N=1, 2, 4, 8, ..., 512
  //
  Err := 0;
  SetLength(alpha, 2);
  SetLength(beta, 2);

  // Alpha[0] := 1;
  // Alpha[1] := 1;
  // Beta[0] := 1;
  // Beta[1] := 1;
  // GaussQuadratureGenerateRec(Alpha, Beta, 1.0, 2, Info, x, w);

  alpha[0] := 0;
  alpha[1] := 0;
  beta[1] := AP_Float(1) / (4 * 1 * 1 - 1);
  GaussQuadratureGenerateRec(alpha, beta, 2.0, 2, Info, x, w);
  if Info > 0 then
    begin
      Err := Max(Err, AbsReal(x[0] + Sqrt(3) / 3));
      Err := Max(Err, AbsReal(x[1] - Sqrt(3) / 3));
      Err := Max(Err, AbsReal(w[0] - 1));
      Err := Max(Err, AbsReal(w[1] - 1));
      i := 0;
      while i <= 0 do
        begin
          RecErrors := RecErrors or AP_FP_Greater_Eq(x[i], x[i + 1]);
          inc(i);
        end;
    end
  else
    begin
      RecErrors := True;
    end;
  SetLength(alpha, 5);
  SetLength(beta, 5);
  alpha[0] := 0;
  i := 1;
  while i <= 4 do
    begin
      alpha[i] := 0;
      beta[i] := AP_Sqr(i) / (4 * AP_Sqr(i) - 1);
      inc(i);
    end;
  GaussQuadratureGenerateRec(alpha, beta, 2.0, 5, Info, x, w);
  if Info > 0 then
    begin
      Err := Max(Err, AbsReal(x[0] + Sqrt(245 + 14 * Sqrt(70)) / 21));
      Err := Max(Err, AbsReal(x[0] + x[4]));
      Err := Max(Err, AbsReal(x[1] + Sqrt(245 - 14 * Sqrt(70)) / 21));
      Err := Max(Err, AbsReal(x[1] + x[3]));
      Err := Max(Err, AbsReal(x[2]));
      Err := Max(Err, AbsReal(w[0] - (322 - 13 * Sqrt(70)) / 900));
      Err := Max(Err, AbsReal(w[0] - w[4]));
      Err := Max(Err, AbsReal(w[1] - (322 + 13 * Sqrt(70)) / 900));
      Err := Max(Err, AbsReal(w[1] - w[3]));
      Err := Max(Err, AbsReal(w[2] - AP_Float(128) / 225));
      i := 0;
      while i <= 3 do
        begin
          RecErrors := RecErrors or AP_FP_Greater_Eq(x[i], x[i + 1]);
          inc(i);
        end;
    end
  else
    begin
      RecErrors := True;
    end;
  n := 1;
  while n <= 512 do
    begin
      SetLength(alpha, n);
      SetLength(beta, n);
      i := 0;
      while i <= n - 1 do
        begin
          alpha[i] := 0;
          if i = 0 then
            begin
              beta[i] := 0;
            end;
          if i = 1 then
            begin
              beta[i] := AP_Float(1) / 2;
            end;
          if i > 1 then
            begin
              beta[i] := AP_Float(1) / 4;
            end;
          inc(i);
        end;
      GaussQuadratureGenerateRec(alpha, beta, pi, n, Info, x, w);
      if Info > 0 then
        begin
          i := 0;
          while i <= n - 1 do
            begin
              Err := Max(Err, AbsReal(x[i] - Cos(pi * (n - i - 0.5) / n)));
              Err := Max(Err, AbsReal(w[i] - pi / n));
              inc(i);
            end;
          i := 0;
          while i <= n - 2 do
            begin
              RecErrors := RecErrors or AP_FP_Greater_Eq(x[i], x[i + 1]);
              inc(i);
            end;
        end
      else
        begin
          RecErrors := True;
        end;
      n := n * 2;
    end;
  RecErrors := RecErrors or AP_FP_Greater(Err, ErrTol);

  //
  // Three tests for rec-based Gauss-Lobatto quadratures with known weights/nodes:
  // 1. Gauss-Lobatto with N=3
  // 2. Gauss-Lobatto with N=4
  // 3. Gauss-Lobatto with N=6
  //
  Err := 0;
  SetLength(alpha, 2);
  SetLength(beta, 2);
  alpha[0] := 0;
  alpha[1] := 0;
  beta[0] := 0;
  beta[1] := AP_Float(1 * 1) / (4 * 1 * 1 - 1);
  GaussQuadratureGenerateGaussLobattoRec(alpha, beta, 2.0, -1, +1, 3, Info, x, w);
  if Info > 0 then
    begin
      Err := Max(Err, AbsReal(x[0] + 1));
      Err := Max(Err, AbsReal(x[1]));
      Err := Max(Err, AbsReal(x[2] - 1));
      Err := Max(Err, AbsReal(w[0] - AP_Float(1) / 3));
      Err := Max(Err, AbsReal(w[1] - AP_Float(4) / 3));
      Err := Max(Err, AbsReal(w[2] - AP_Float(1) / 3));
      i := 0;
      while i <= 1 do
        begin
          RecErrors := RecErrors or AP_FP_Greater_Eq(x[i], x[i + 1]);
          inc(i);
        end;
    end
  else
    begin
      RecErrors := True;
    end;
  SetLength(alpha, 3);
  SetLength(beta, 3);
  alpha[0] := 0;
  alpha[1] := 0;
  alpha[2] := 0;
  beta[0] := 0;
  beta[1] := AP_Float(1 * 1) / (4 * 1 * 1 - 1);
  beta[2] := AP_Float(2 * 2) / (4 * 2 * 2 - 1);
  GaussQuadratureGenerateGaussLobattoRec(alpha, beta, 2.0, -1, +1, 4, Info, x, w);
  if Info > 0 then
    begin
      Err := Max(Err, AbsReal(x[0] + 1));
      Err := Max(Err, AbsReal(x[1] + Sqrt(5) / 5));
      Err := Max(Err, AbsReal(x[2] - Sqrt(5) / 5));
      Err := Max(Err, AbsReal(x[3] - 1));
      Err := Max(Err, AbsReal(w[0] - AP_Float(1) / 6));
      Err := Max(Err, AbsReal(w[1] - AP_Float(5) / 6));
      Err := Max(Err, AbsReal(w[2] - AP_Float(5) / 6));
      Err := Max(Err, AbsReal(w[3] - AP_Float(1) / 6));
      i := 0;
      while i <= 2 do
        begin
          RecErrors := RecErrors or AP_FP_Greater_Eq(x[i], x[i + 1]);
          inc(i);
        end;
    end
  else
    begin
      RecErrors := True;
    end;
  SetLength(alpha, 5);
  SetLength(beta, 5);
  alpha[0] := 0;
  alpha[1] := 0;
  alpha[2] := 0;
  alpha[3] := 0;
  alpha[4] := 0;
  beta[0] := 0;
  beta[1] := AP_Float(1 * 1) / (4 * 1 * 1 - 1);
  beta[2] := AP_Float(2 * 2) / (4 * 2 * 2 - 1);
  beta[3] := AP_Float(3 * 3) / (4 * 3 * 3 - 1);
  beta[4] := AP_Float(4 * 4) / (4 * 4 * 4 - 1);
  GaussQuadratureGenerateGaussLobattoRec(alpha, beta, 2.0, -1, +1, 6, Info, x, w);
  if Info > 0 then
    begin
      Err := Max(Err, AbsReal(x[0] + 1));
      Err := Max(Err, AbsReal(x[1] + Sqrt((7 + 2 * Sqrt(7)) / 21)));
      Err := Max(Err, AbsReal(x[2] + Sqrt((7 - 2 * Sqrt(7)) / 21)));
      Err := Max(Err, AbsReal(x[3] - Sqrt((7 - 2 * Sqrt(7)) / 21)));
      Err := Max(Err, AbsReal(x[4] - Sqrt((7 + 2 * Sqrt(7)) / 21)));
      Err := Max(Err, AbsReal(x[5] - 1));
      Err := Max(Err, AbsReal(w[0] - AP_Float(1) / 15));
      Err := Max(Err, AbsReal(w[1] - (14 - Sqrt(7)) / 30));
      Err := Max(Err, AbsReal(w[2] - (14 + Sqrt(7)) / 30));
      Err := Max(Err, AbsReal(w[3] - (14 + Sqrt(7)) / 30));
      Err := Max(Err, AbsReal(w[4] - (14 - Sqrt(7)) / 30));
      Err := Max(Err, AbsReal(w[5] - AP_Float(1) / 15));
      i := 0;
      while i <= 4 do
        begin
          RecErrors := RecErrors or AP_FP_Greater_Eq(x[i], x[i + 1]);
          inc(i);
        end;
    end
  else
    begin
      RecErrors := True;
    end;
  RecErrors := RecErrors or AP_FP_Greater(Err, ErrTol);

  //
  // Three tests for rec-based Gauss-Radau quadratures with known weights/nodes:
  // 1. Gauss-Radau with N=2
  // 2. Gauss-Radau with N=3
  // 3. Gauss-Radau with N=3 (another case)
  //
  Err := 0;
  SetLength(alpha, 1);
  SetLength(beta, 2);
  alpha[0] := 0;
  beta[0] := 0;
  beta[1] := AP_Float(1 * 1) / (4 * 1 * 1 - 1);
  GaussQuadratureGenerateGaussRadauRec(alpha, beta, 2.0, -1, 2, Info, x, w);
  if Info > 0 then
    begin
      Err := Max(Err, AbsReal(x[0] + 1));
      Err := Max(Err, AbsReal(x[1] - AP_Float(1) / 3));
      Err := Max(Err, AbsReal(w[0] - 0.5));
      Err := Max(Err, AbsReal(w[1] - 1.5));
      i := 0;
      while i <= 0 do
        begin
          RecErrors := RecErrors or AP_FP_Greater_Eq(x[i], x[i + 1]);
          inc(i);
        end;
    end
  else
    begin
      RecErrors := True;
    end;
  SetLength(alpha, 2);
  SetLength(beta, 3);
  alpha[0] := 0;
  alpha[1] := 0;
  i := 0;
  while i <= 2 do
    begin
      beta[i] := AP_Sqr(i) / (4 * AP_Sqr(i) - 1);
      inc(i);
    end;
  GaussQuadratureGenerateGaussRadauRec(alpha, beta, 2.0, -1, 3, Info, x, w);
  if Info > 0 then
    begin
      Err := Max(Err, AbsReal(x[0] + 1));
      Err := Max(Err, AbsReal(x[1] - (1 - Sqrt(6)) / 5));
      Err := Max(Err, AbsReal(x[2] - (1 + Sqrt(6)) / 5));
      Err := Max(Err, AbsReal(w[0] - AP_Float(2) / 9));
      Err := Max(Err, AbsReal(w[1] - (16 + Sqrt(6)) / 18));
      Err := Max(Err, AbsReal(w[2] - (16 - Sqrt(6)) / 18));
      i := 0;
      while i <= 1 do
        begin
          RecErrors := RecErrors or AP_FP_Greater_Eq(x[i], x[i + 1]);
          inc(i);
        end;
    end
  else
    begin
      RecErrors := True;
    end;
  SetLength(alpha, 2);
  SetLength(beta, 3);
  alpha[0] := 0;
  alpha[1] := 0;
  i := 0;
  while i <= 2 do
    begin
      beta[i] := AP_Sqr(i) / (4 * AP_Sqr(i) - 1);
      inc(i);
    end;
  GaussQuadratureGenerateGaussRadauRec(alpha, beta, 2.0, +1, 3, Info, x, w);
  if Info > 0 then
    begin
      Err := Max(Err, AbsReal(x[2] - 1));
      Err := Max(Err, AbsReal(x[1] + (1 - Sqrt(6)) / 5));
      Err := Max(Err, AbsReal(x[0] + (1 + Sqrt(6)) / 5));
      Err := Max(Err, AbsReal(w[2] - AP_Float(2) / 9));
      Err := Max(Err, AbsReal(w[1] - (16 + Sqrt(6)) / 18));
      Err := Max(Err, AbsReal(w[0] - (16 - Sqrt(6)) / 18));
      i := 0;
      while i <= 1 do
        begin
          RecErrors := RecErrors or AP_FP_Greater_Eq(x[i], x[i + 1]);
          inc(i);
        end;
    end
  else
    begin
      RecErrors := True;
    end;
  RecErrors := RecErrors or AP_FP_Greater(Err, ErrTol);

  //
  // test recurrence-based special cases (Legendre, Jacobi, Hermite, ...)
  // against another implementation (polynomial root-finder)

  //
  n := 1;
  while n <= 20 do
    begin

      //
      // test gauss-legendre
      //
      Err := 0;
      GaussQuadratureGenerateGaussLegendre(n, Info, x, w);
      if Info > 0 then
        begin
          BuildGaussLegendreQuadrature(n, x2, W2);
          i := 0;
          while i <= n - 1 do
            begin
              Err := Max(Err, AbsReal(x[i] - x2[i]));
              Err := Max(Err, AbsReal(w[i] - W2[i]));
              inc(i);
            end;
        end
      else
        begin
          SpecErrors := True;
        end;
      SpecErrors := SpecErrors or AP_FP_Greater(Err, ErrTol);

      //
      // Test Gauss-Jacobi.
      // Since task is much more difficult we will use less strict
      // threshold.
      //
      Err := 0;
      AKind := 0;
      while AKind <= 9 do
        begin
          BKind := 0;
          while BKind <= 9 do
            begin
              alphaC := MapKind(AKind);
              betaC := MapKind(BKind);
              GaussQuadratureGenerateGaussJacobi(n, alphaC, betaC, Info, x, w);
              if Info > 0 then
                begin
                  BuildGaussJacobiQuadrature(n, alphaC, betaC, x2, W2);
                  i := 0;
                  while i <= n - 1 do
                    begin
                      Err := Max(Err, AbsReal(x[i] - x2[i]));
                      Err := Max(Err, AbsReal(w[i] - W2[i]));
                      inc(i);
                    end;
                end
              else
                begin
                  SpecErrors := True;
                end;
              inc(BKind);
            end;
          inc(AKind);
        end;
      SpecErrors := SpecErrors or AP_FP_Greater(Err, NonStrictErrTol);

      //
      // special test for Gauss-Jacobi (Chebyshev weight
      // function with analytically known nodes/weights)
      //
      Err := 0;
      GaussQuadratureGenerateGaussJacobi(n, -0.5, -0.5, Info, x, w);
      if Info > 0 then
        begin
          i := 0;
          while i <= n - 1 do
            begin
              Err := Max(Err, AbsReal(x[i] + Cos(pi * (i + 0.5) / n)));
              Err := Max(Err, AbsReal(w[i] - pi / n));
              inc(i);
            end;
        end
      else
        begin
          SpecErrors := True;
        end;
      SpecErrors := SpecErrors or AP_FP_Greater(Err, StrictErrTol);

      //
      // Test Gauss-Laguerre
      //
      Err := 0;
      AKind := 0;
      while AKind <= 9 do
        begin
          alphaC := MapKind(AKind);
          GaussQuadratureGenerateGaussLaguerre(n, alphaC, Info, x, w);
          if Info > 0 then
            begin
              BuildGaussLaguerreQuadrature(n, alphaC, x2, W2);
              i := 0;
              while i <= n - 1 do
                begin
                  Err := Max(Err, AbsReal(x[i] - x2[i]));
                  Err := Max(Err, AbsReal(w[i] - W2[i]));
                  inc(i);
                end;
            end
          else
            begin
              SpecErrors := True;
            end;
          inc(AKind);
        end;
      SpecErrors := SpecErrors or AP_FP_Greater(Err, NonStrictErrTol);

      //
      // Test Gauss-Hermite
      //
      Err := 0;
      GaussQuadratureGenerateGaussHermite(n, Info, x, w);
      if Info > 0 then
        begin
          BuildGaussHermiteQuadrature(n, x2, W2);
          i := 0;
          while i <= n - 1 do
            begin
              Err := Max(Err, AbsReal(x[i] - x2[i]));
              Err := Max(Err, AbsReal(w[i] - W2[i]));
              inc(i);
            end;
        end
      else
        begin
          SpecErrors := True;
        end;
      SpecErrors := SpecErrors or AP_FP_Greater(Err, NonStrictErrTol);
      inc(n);
    end;

  //
  // end
  //
  WasErrors := RecErrors or SpecErrors;
  if not Silent then
    begin
      DoStatusNoLn(PFormat('TESTING GAUSS QUADRATURES'#13#10'', []));
      DoStatusNoLn(PFormat('FINAL RESULT:                             ', []));
      if WasErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('* SPECIAL CASES (LEGENDRE/JACOBI/..)      ', []));
      if SpecErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      DoStatusNoLn(PFormat('* RECURRENCE-BASED:                       ', []));
      if RecErrors then
        begin
          DoStatusNoLn(PFormat('FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('OK'#13#10'', []));
        end;
      if WasErrors then
        begin
          DoStatusNoLn(PFormat('TEST FAILED'#13#10'', []));
        end
      else
        begin
          DoStatusNoLn(PFormat('TEST PASSED'#13#10'', []));
        end;
      DoStatusNoLn(PFormat(''#13#10''#13#10'', []));
    end;
  Result := not WasErrors;
end;

procedure LearnTest();
const
  test_InLen = 5;
  test_OutLen = 5;
  test_Depth = 10;

  RegressionLearnType: set of TLearnType = [
    ltKDT, ltKM,
    ltForest, ltLogit,
    ltLM, ltLM_MT, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod, ltMonteCarlo,
    ltLM_Ensemble, ltLM_Ensemble_MT, ltLBFGS_Ensemble, ltLBFGS_Ensemble_MT];

  ClassifierLearnType: set of TLearnType = [
    ltKDT, ltKM,
    ltForest, ltLogit,
    ltLM, ltLM_MT, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod, ltMonteCarlo,
    ltLM_Ensemble, ltLM_Ensemble_MT, ltLBFGS_Ensemble, ltLBFGS_Ensemble_MT];

var
  lt: TLearnType;
  lr: TLearn;
  i, j, LV: TLInt;
  f_In, f_Out: TLVec;
  f_out_list: TLMatrix;
  Search_Out: TLIVec;
  M: TLMatrix;
  m64: TMemoryStream64;
  md5_1, md5_2: TMD5;
begin
  TestABLAS(False);
  TestXBLAS(False);
  TestGaussKronrodQuadrature(False);
  TestGaussQuadrature(False);

  // Regression test
  for LV := 0 to 2 do
    for lt in RegressionLearnType do
      begin
        DoStatusNoLn('Level-%d test %s', [LV, CLearnString[lt]]);
        case LV of
          0: lr := TLearn.CreateRegression(lt, test_InLen, test_OutLen);
          1: lr := TLearn.CreateRegression1(lt, test_InLen, test_OutLen);
          else lr := TLearn.CreateRegression2(lt, test_InLen, test_OutLen);
        end;

        SetLength(f_In, lr.InLen);
        SetLength(f_Out, lr.OutLen);

        for j := 0 to test_Depth - 1 do
          begin
            for i := 0 to lr.InLen - 1 do
                f_In[i] := umlRandomRangeD(0, 100);
            for i := 0 to lr.OutLen - 1 do
                f_Out[i] := umlRandomRangeD(0, 10);
            lr.AddMemory(f_In, f_Out);
          end;

        DoStatusNoLn('...Train');
        if lr.Train then
          begin
            SetLength(f_out_list, lr.Count);
            for i := 0 to lr.Count - 1 do
              if not lr.process(@lr[i]^.m_in, @f_out_list[i]) then
                  DoStatusNoLn('...error');

            for i := 0 to lr.Count - 1 do
              begin
                lr.SearchMemoryPearson(lr[i]^.m_in);
                lr.SearchMemoryPearson(lr[i]^.m_in, Search_Out);
                SetLength(Search_Out, 0);

                lr.SearchMemorySpearman(lr[i]^.m_in);
                lr.SearchMemorySpearman(lr[i]^.m_in, Search_Out);
                SetLength(Search_Out, 0);

                lr.SearchMemoryDistance(lr[i]^.m_in);
                lr.SearchMemoryDistance(lr[i]^.m_in, Search_Out);
                SetLength(Search_Out, 0);
              end;

            DoStatusNoLn('...serialize');
            m64 := TMemoryStream64.Create;
            lr.SaveToStream(m64);
            md5_1 := umlStreamMD5(m64);
            m64.Position := 0;
            lr.LoadFromStream(m64);
            m64.Clear;
            lr.SaveToStream(m64);
            md5_2 := umlStreamMD5(m64);
            if not umlCompareMD5(md5_1, md5_2) then
                DoStatusNoLn('...error ');
            DisposeObject(m64);

            for i := 0 to lr.Count - 1 do
              if lr.process(@lr[i]^.m_in, @f_Out) then
                begin
                  if TKDTree.KDTreeDistance(f_Out, f_out_list[i]) <> 0 then
                      DoStatusNoLn('...verify data error ');
                end
              else
                  DoStatusNoLn('...verify data with process time error ');

            SetLength(f_out_list, 0, 0);
          end
        else
            DoStatusNoLn('...train error');

        DoStatusNoLn('...ok!');
        DoStatusNoLn;

        SetLength(f_In, 0);
        SetLength(f_Out, 0);

        DisposeObject(lr);
      end;

  // classifier test
  for LV := 0 to 2 do
    for lt in ClassifierLearnType do
      begin
        DoStatusNoLn('Classifier%d test %s ..', [LV, CLearnString[lt]]);
        case LV of
          0: lr := TLearn.CreateClassifier(lt, test_InLen);
          1: lr := TLearn.CreateClassifier1(lt, test_InLen);
          else lr := TLearn.CreateClassifier2(lt, test_InLen);
        end;

        SetLength(f_In, lr.InLen);
        SetLength(f_Out, lr.OutLen);

        for j := 0 to test_Depth - 1 do
          begin
            for i := 0 to lr.InLen - 1 do
                f_In[i] := umlRandomRangeD(0, 100);
            for i := 0 to lr.OutLen - 1 do
                f_Out[i] := umlRandomRangeD(0, 10);
            lr.AddMemory(f_In, f_Out);
          end;
        if lr.Train() then
          begin
            SetLength(f_out_list, lr.Count);
            for i := 0 to lr.Count - 1 do
              if not lr.process(@lr[i]^.m_in, @f_out_list[i]) then
                  DoStatusNoLn('process error ');

            m64 := TMemoryStream64.Create;
            lr.SaveToStream(m64);
            md5_1 := umlStreamMD5(m64);
            m64.Position := 0;
            lr.LoadFromStream(m64);
            m64.Clear;
            lr.SaveToStream(m64);
            md5_2 := umlStreamMD5(m64);
            if not umlCompareMD5(md5_1, md5_2) then
                DoStatusNoLn('encode error ');
            DisposeObject(m64);

            for i := 0 to lr.Count - 1 do
              if lr.process(@lr[i]^.m_in, @f_Out) then
                begin
                  if TKDTree.KDTreeDistance(f_Out, f_out_list[i]) <> 0 then
                      DoStatusNoLn('verify data error ');
                end
              else
                  DoStatusNoLn('process verify error ');

            SetLength(f_out_list, 0, 0);
          end
        else
            DoStatusNoLn('train error ');

        DoStatusNoLn;

        SetLength(f_In, 0);
        SetLength(f_Out, 0);

        DisposeObject(lr);
      end;

  DoStatus('all learn test finish!');
end;
