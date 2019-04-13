{ ****************************************************************************** }
{ * Descriptive statistics: mean, variance, etc                                * }
{ * Principal component analysis support, by QQ 600585@qq.com                  * }
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

procedure PCABuildBasis(const x: TLMatrix; NPoints: TLInt; NVars: TLInt; var Info: TLInt; var s2: TLVec; var v: TLMatrix); forward;

procedure InternalStatHeapSort(var Arr: TLVec; n: TLInt); forward;


(* ************************************************************************
  Calculation of the distribution moments: mean, variance, slewness, kurtosis.

  Input parameters:
  X       -   sample. Array with whose indexes range within [0..N-1]
  N       -   sample size.

  Output parameters:
  Mean    -   mean.
  Variance-   variance.
  Skewness-   skewness (if variance<>0; zero otherwise).
  Kurtosis-   kurtosis (if variance<>0; zero otherwise).
  ************************************************************************ *)
procedure CalculateMoments(const x: TLVec; n: TLInt; var Mean: TLFloat; var Variance: TLFloat; var Skewness: TLFloat; var Kurtosis: TLFloat);
var
  i     : TLInt;
  v     : TLFloat;
  v1    : TLFloat;
  v2    : TLFloat;
  StdDev: TLFloat;
begin
  Mean := 0;
  Variance := 0;
  Skewness := 0;
  Kurtosis := 0;
  StdDev := 0;
  if n <= 0 then
    begin
      Exit;
    end;

  //
  // Mean
  //
  i := 0;
  while i <= n - 1 do
    begin
      Mean := Mean + x[i];
      inc(i);
    end;
  Mean := Mean / n;

  //
  // Variance (using corrected two-pass algorithm)
  //
  if n <> 1 then
    begin
      v1 := 0;
      i := 0;
      while i <= n - 1 do
        begin
          v1 := v1 + AP_Sqr(x[i] - Mean);
          inc(i);
        end;
      v2 := 0;
      i := 0;
      while i <= n - 1 do
        begin
          v2 := v2 + (x[i] - Mean);
          inc(i);
        end;
      v2 := AP_Sqr(v2) / n;
      Variance := (v1 - v2) / (n - 1);
      if AP_FP_Less(Variance, 0) then
        begin
          Variance := 0;
        end;
      StdDev := Sqrt(Variance);
    end;

  //
  // Skewness and kurtosis
  //
  if AP_FP_NEq(StdDev, 0) then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          v := (x[i] - Mean) / StdDev;
          v2 := AP_Sqr(v);
          Skewness := Skewness + v2 * v;
          Kurtosis := Kurtosis + AP_Sqr(v2);
          inc(i);
        end;
      Skewness := Skewness / n;
      Kurtosis := Kurtosis / n - 3;
    end;
end;

(* ************************************************************************
  ADev

  Input parameters:
  X   -   sample (array indexes: [0..N-1])
  N   -   sample size

  Output parameters:
  ADev-   ADev
  ************************************************************************ *)
procedure CalculateADev(const x: TLVec; const n: TLInt; var ADev: TLFloat);
var
  i   : TLInt;
  Mean: TLFloat;
begin
  Mean := 0;
  ADev := 0;
  if n <= 0 then
    begin
      Exit;
    end;

  //
  // Mean
  //
  i := 0;
  while i <= n - 1 do
    begin
      Mean := Mean + x[i];
      inc(i);
    end;
  Mean := Mean / n;

  //
  // ADev
  //
  i := 0;
  while i <= n - 1 do
    begin
      ADev := ADev + AbsReal(x[i] - Mean);
      inc(i);
    end;
  ADev := ADev / n;
end;

(* ************************************************************************
  Median calculation.

  Input parameters:
  X   -   sample (array indexes: [0..N-1])
  N   -   sample size

  Output parameters:
  Median
  ************************************************************************ *)
procedure CalculateMedian(const x: TLVec; const n: TLInt; var Median: TLFloat);
var
  i   : TLInt;
  IR  : TLInt;
  j   : TLInt;
  L   : TLInt;
  midp: TLInt;
  k   : TLInt;
  a   : TLFloat;
  tval: TLFloat;
begin
  //
  // Some degenerate cases
  //
  Median := 0;
  if n <= 0 then
    begin
      Exit;
    end;
  if n = 1 then
    begin
      Median := x[0];
      Exit;
    end;
  if n = 2 then
    begin
      Median := 0.5 * (x[0] + x[1]);
      Exit;
    end;

  //
  // Common case, N>=3.
  // Choose X[(N-1)/2]
  //
  L := 0;
  IR := n - 1;
  k := (n - 1) div 2;
  while True do
    begin
      if IR <= L + 1 then
        begin

          //
          // 1 or 2 elements in partition
          //
          if (IR = L + 1) and AP_FP_Less(x[IR], x[L]) then
            begin
              tval := x[L];
              x[L] := x[IR];
              x[IR] := tval;
            end;
          Break;
        end
      else
        begin
          midp := (L + IR) div 2;
          tval := x[midp];
          x[midp] := x[L + 1];
          x[L + 1] := tval;
          if AP_FP_Greater(x[L], x[IR]) then
            begin
              tval := x[L];
              x[L] := x[IR];
              x[IR] := tval;
            end;
          if AP_FP_Greater(x[L + 1], x[IR]) then
            begin
              tval := x[L + 1];
              x[L + 1] := x[IR];
              x[IR] := tval;
            end;
          if AP_FP_Greater(x[L], x[L + 1]) then
            begin
              tval := x[L];
              x[L] := x[L + 1];
              x[L + 1] := tval;
            end;
          i := L + 1;
          j := IR;
          a := x[L + 1];
          while True do
            begin
              repeat
                  i := i + 1;
              until AP_FP_Greater_Eq(x[i], a);
              repeat
                  j := j - 1;
              until AP_FP_Less_Eq(x[j], a);
              if j < i then
                begin
                  Break;
                end;
              tval := x[i];
              x[i] := x[j];
              x[j] := tval;
            end;
          x[L + 1] := x[j];
          x[j] := a;
          if j >= k then
            begin
              IR := j - 1;
            end;
          if j <= k then
            begin
              L := i;
            end;
        end;
    end;

  //
  // If N is odd, return result
  //
  if n mod 2 = 1 then
    begin
      Median := x[k];
      Exit;
    end;
  a := x[n - 1];
  i := k + 1;
  while i <= n - 1 do
    begin
      if AP_FP_Less(x[i], a) then
        begin
          a := x[i];
        end;
      inc(i);
    end;
  Median := 0.5 * (x[k] + a);
end;

(* ************************************************************************
  Percentile calculation.

  Input parameters:
  X   -   sample (array indexes: [0..N-1])
  N   -   sample size, N>1
  P   -   percentile (0<=P<=1)

  Output parameters:
  V   -   percentile
  ************************************************************************ *)
procedure CalculatePercentile(const x: TLVec; const n: TLInt; const p: TLFloat; var v: TLFloat);
var
  Nx: TLVec;
  i1: TLInt;
  t : TLFloat;
begin
  Nx := DynamicArrayCopy(x);
  Assert(n > 1, 'CalculatePercentile: N<=1!');
  Assert(AP_FP_Greater_Eq(p, 0) and AP_FP_Less_Eq(p, 1),
    'CalculatePercentile: incorrect P!');
  InternalStatHeapSort(Nx, n);
  if AP_FP_Eq(p, 0) then
    begin
      v := Nx[0];
      Exit;
    end;
  if AP_FP_Eq(p, 1) then
    begin
      v := Nx[n - 1];
      Exit;
    end;
  t := p * (n - 1);
  i1 := Floor(t);
  t := t - Floor(t);
  v := Nx[i1] * (1 - t) + Nx[i1 + 1] * t;
end;

procedure InternalStatHeapSort(var Arr: TLVec; n: TLInt);
var
  i  : TLInt;
  k  : TLInt;
  t  : TLInt;
  tmp: TLFloat;
begin
  if n = 1 then
    begin
      Exit;
    end;
  i := 2;
  repeat
    t := i;
    while t <> 1 do
      begin
        k := t div 2;
        if AP_FP_Greater_Eq(Arr[k - 1], Arr[t - 1]) then
          begin
            t := 1;
          end
        else
          begin
            tmp := Arr[k - 1];
            Arr[k - 1] := Arr[t - 1];
            Arr[t - 1] := tmp;
            t := k;
          end;
      end;
    i := i + 1;
  until not(i <= n);
  i := n - 1;
  repeat
    tmp := Arr[i];
    Arr[i] := Arr[0];
    Arr[0] := tmp;
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
                if AP_FP_Greater(Arr[k], Arr[k - 1]) then
                  begin
                    k := k + 1;
                  end;
              end;
            if AP_FP_Greater_Eq(Arr[t - 1], Arr[k - 1]) then
              begin
                t := 0;
              end
            else
              begin
                tmp := Arr[k - 1];
                Arr[k - 1] := Arr[t - 1];
                Arr[t - 1] := tmp;
                t := k;
              end;
          end;
      end;
    i := i - 1;
  until not(i >= 1);
end;

(* ************************************************************************
  Principal components analysis

  Subroutine  builds  orthogonal  basis  where  first  axis  corresponds  to
  direction with maximum variance, second axis maximizes variance in subspace
  orthogonal to first axis and so on.

  It should be noted that, unlike LDA, PCA does not use class labels.

  INPUT PARAMETERS:
  X           -   dataset, array[0..NPoints-1,0..NVars-1]. matrix contains ONLY INDEPENDENT VARIABLES.
  NPoints     -   dataset size, NPoints>=0
  NVars       -   number of independent variables, NVars>=1

  Info        -   return code:
  * -4, if SVD subroutine haven't converged
  * -1, if wrong parameters has been passed (NPoints<0, NVars<1)
  *  1, if task is solved
  S2          -   array[0..NVars-1]. variance values corresponding to basis vectors.
  V           -   array[0..NVars-1,0..NVars-1] matrix, whose columns store basis vectors.
  ************************************************************************ *)
procedure PCABuildBasis(const x: TLMatrix; NPoints: TLInt; NVars: TLInt; var Info: TLInt; var s2: TLVec; var v: TLMatrix);
var
  a       : TLMatrix;
  u       : TLMatrix;
  VT      : TLMatrix;
  M       : TLVec;
  t       : TLVec;
  i       : TLInt;
  j       : TLInt;
  Mean    : TLFloat;
  Variance: TLFloat;
  Skewness: TLFloat;
  Kurtosis: TLFloat;
  i_      : TLInt;
begin

  //
  // Check input data
  //
  if (NPoints < 0) or (NVars < 1) then
    begin
      Info := -1;
      Exit;
    end;
  Info := 1;

  //
  // Special case: NPoints=0
  //
  if NPoints = 0 then
    begin
      SetLength(s2, NVars  );
      SetLength(v, NVars  , NVars  );
      i := 0;
      while i <= NVars - 1 do
        begin
          s2[i] := 0;
          inc(i);
        end;
      i := 0;
      while i <= NVars - 1 do
        begin
          j := 0;
          while j <= NVars - 1 do
            begin
              if i = j then
                begin
                  v[i, j] := 1;
                end
              else
                begin
                  v[i, j] := 0;
                end;
              inc(j);
            end;
          inc(i);
        end;
      Exit;
    end;

  //
  // Calculate means
  //
  SetLength(M, NVars  );
  SetLength(t, NPoints  );
  j := 0;
  while j <= NVars - 1 do
    begin
      for i_ := 0 to NPoints - 1 do
        begin
          t[i_] := x[i_, j];
        end;
      CalculateMoments(t, NPoints, Mean, Variance, Skewness, Kurtosis);
      M[j] := Mean;
      inc(j);
    end;

  //
  // Center, apply SVD, prepare output
  //
  SetLength(a, Max(NPoints, NVars)  , NVars  );
  i := 0;
  while i <= NPoints - 1 do
    begin
      APVMove(@a[i][0], 0, NVars - 1, @x[i][0], 0, NVars - 1);
      APVSub(@a[i][0], 0, NVars - 1, @M[0], 0, NVars - 1);
      inc(i);
    end;
  i := NPoints;
  while i <= NVars - 1 do
    begin
      j := 0;
      while j <= NVars - 1 do
        begin
          a[i, j] := 0;
          inc(j);
        end;
      inc(i);
    end;
  if not RMatrixSVD(a, Max(NPoints, NVars), NVars, 0, 1, 2, s2, u, VT) then
    begin
      Info := -4;
      Exit;
    end;
  if NPoints <> 1 then
    begin
      i := 0;
      while i <= NVars - 1 do
        begin
          s2[i] := AP_Sqr(s2[i]) / (NPoints - 1);
          inc(i);
        end;
    end;
  SetLength(v, NVars  , NVars  );
  CopyAndTranspose(VT, 0, NVars - 1, 0, NVars - 1, v, 0, NVars - 1, 0, NVars - 1);
end; 
 
 
