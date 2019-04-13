{ ****************************************************************************** }
{ * Mann-Whitney U-test support, by QQ 600585@qq.com                           * }
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

procedure UCheb(const x, c: TLFloat; var TJ: TLFloat; var TJ1: TLFloat; var r: TLFloat); forward;
function UNInterpolate(const p1, p2, p3: TLFloat; const n: TLInt): TLFloat; forward;
function USigma000(const n1, n2: TLInt): TLFloat; forward;
function USigma075(const n1, n2: TLInt): TLFloat; forward;
function USigma150(const n1, n2: TLInt): TLFloat; forward;
function USigma225(const n1, n2: TLInt): TLFloat; forward;
function USigma300(const n1, n2: TLInt): TLFloat; forward;
function USigma333(const n1, n2: TLInt): TLFloat; forward;
function USigma367(const n1, n2: TLInt): TLFloat; forward;
function USigma400(const n1, n2: TLInt): TLFloat; forward;
function UTblN5N5(const s: TLFloat): TLFloat; forward;
function UTblN5N6(const s: TLFloat): TLFloat; forward;
function UTblN5N7(const s: TLFloat): TLFloat; forward;
function UTblN5N8(const s: TLFloat): TLFloat; forward;
function UTblN5N9(const s: TLFloat): TLFloat; forward;
function UTblN5N10(const s: TLFloat): TLFloat; forward;
function UTblN5N11(const s: TLFloat): TLFloat; forward;
function UTblN5N12(const s: TLFloat): TLFloat; forward;
function UTblN5N13(const s: TLFloat): TLFloat; forward;
function UTblN5N14(const s: TLFloat): TLFloat; forward;
function UTblN5N15(const s: TLFloat): TLFloat; forward;
function UTblN5N16(const s: TLFloat): TLFloat; forward;
function UTblN5N17(const s: TLFloat): TLFloat; forward;
function UTblN5N18(const s: TLFloat): TLFloat; forward;
function UTblN5N19(const s: TLFloat): TLFloat; forward;
function UTblN5N20(const s: TLFloat): TLFloat; forward;
function UTblN5N21(const s: TLFloat): TLFloat; forward;
function UTblN5N22(const s: TLFloat): TLFloat; forward;
function UTblN5N23(const s: TLFloat): TLFloat; forward;
function UTblN5N24(const s: TLFloat): TLFloat; forward;
function UTblN5N25(const s: TLFloat): TLFloat; forward;
function UTblN5N26(const s: TLFloat): TLFloat; forward;
function UTblN5N27(const s: TLFloat): TLFloat; forward;
function UTblN5N28(const s: TLFloat): TLFloat; forward;
function UTblN5N29(const s: TLFloat): TLFloat; forward;
function UTblN5N30(const s: TLFloat): TLFloat; forward;
function UTblN5N100(const s: TLFloat): TLFloat; forward;
function UTblN6N6(const s: TLFloat): TLFloat; forward;
function UTblN6N7(const s: TLFloat): TLFloat; forward;
function UTblN6N8(const s: TLFloat): TLFloat; forward;
function UTblN6N9(const s: TLFloat): TLFloat; forward;
function UTblN6N10(const s: TLFloat): TLFloat; forward;
function UTblN6N11(const s: TLFloat): TLFloat; forward;
function UTblN6N12(const s: TLFloat): TLFloat; forward;
function UTblN6N13(const s: TLFloat): TLFloat; forward;
function UTblN6N14(const s: TLFloat): TLFloat; forward;
function UTblN6N15(const s: TLFloat): TLFloat; forward;
function UTblN6N30(const s: TLFloat): TLFloat; forward;
function UTblN6N100(const s: TLFloat): TLFloat; forward;
function UTblN7N7(const s: TLFloat): TLFloat; forward;
function UTblN7N8(const s: TLFloat): TLFloat; forward;
function UTblN7N9(const s: TLFloat): TLFloat; forward;
function UTblN7N10(const s: TLFloat): TLFloat; forward;
function UTblN7N11(const s: TLFloat): TLFloat; forward;
function UTblN7N12(const s: TLFloat): TLFloat; forward;
function UTblN7N13(const s: TLFloat): TLFloat; forward;
function UTblN7N14(const s: TLFloat): TLFloat; forward;
function UTblN7N15(const s: TLFloat): TLFloat; forward;
function UTblN7N30(const s: TLFloat): TLFloat; forward;
function UTblN7N100(const s: TLFloat): TLFloat; forward;
function UTblN8N8(const s: TLFloat): TLFloat; forward;
function UTblN8N9(const s: TLFloat): TLFloat; forward;
function UTblN8N10(const s: TLFloat): TLFloat; forward;
function UTblN8N11(const s: TLFloat): TLFloat; forward;
function UTblN8N12(const s: TLFloat): TLFloat; forward;
function UTblN8N13(const s: TLFloat): TLFloat; forward;
function UTblN8N14(const s: TLFloat): TLFloat; forward;
function UTblN8N15(const s: TLFloat): TLFloat; forward;
function UTblN8N30(const s: TLFloat): TLFloat; forward;
function UTblN8N100(const s: TLFloat): TLFloat; forward;
function UTblN9N9(const s: TLFloat): TLFloat; forward;
function UTblN9N10(const s: TLFloat): TLFloat; forward;
function UTblN9N11(const s: TLFloat): TLFloat; forward;
function UTblN9N12(const s: TLFloat): TLFloat; forward;
function UTblN9N13(const s: TLFloat): TLFloat; forward;
function UTblN9N14(const s: TLFloat): TLFloat; forward;
function UTblN9N15(const s: TLFloat): TLFloat; forward;
function UTblN9N30(const s: TLFloat): TLFloat; forward;
function UTblN9N100(const s: TLFloat): TLFloat; forward;
function UTblN10N10(const s: TLFloat): TLFloat; forward;
function UTblN10N11(const s: TLFloat): TLFloat; forward;
function UTblN10N12(const s: TLFloat): TLFloat; forward;
function UTblN10N13(const s: TLFloat): TLFloat; forward;
function UTblN10N14(const s: TLFloat): TLFloat; forward;
function UTblN10N15(const s: TLFloat): TLFloat; forward;
function UTblN10N30(const s: TLFloat): TLFloat; forward;
function UTblN10N100(const s: TLFloat): TLFloat; forward;
function UTblN11N11(const s: TLFloat): TLFloat; forward;
function UTblN11N12(const s: TLFloat): TLFloat; forward;
function UTblN11N13(const s: TLFloat): TLFloat; forward;
function UTblN11N14(const s: TLFloat): TLFloat; forward;
function UTblN11N15(const s: TLFloat): TLFloat; forward;
function UTblN11N30(const s: TLFloat): TLFloat; forward;
function UTblN11N100(const s: TLFloat): TLFloat; forward;
function UTblN12N12(const s: TLFloat): TLFloat; forward;
function UTblN12N13(const s: TLFloat): TLFloat; forward;
function UTblN12N14(const s: TLFloat): TLFloat; forward;
function UTblN12N15(const s: TLFloat): TLFloat; forward;
function UTblN12N30(const s: TLFloat): TLFloat; forward;
function UTblN12N100(const s: TLFloat): TLFloat; forward;
function UTblN13N13(const s: TLFloat): TLFloat; forward;
function UTblN13N14(const s: TLFloat): TLFloat; forward;
function UTblN13N15(const s: TLFloat): TLFloat; forward;
function UTblN13N30(const s: TLFloat): TLFloat; forward;
function UTblN13N100(const s: TLFloat): TLFloat; forward;
function UTblN14N14(const s: TLFloat): TLFloat; forward;
function UTblN14N15(const s: TLFloat): TLFloat; forward;
function UTblN14N30(const s: TLFloat): TLFloat; forward;
function UTblN14N100(const s: TLFloat): TLFloat; forward;
function USigma(s: TLFloat; n1: TLInt; n2: TLInt): TLFloat; forward;


(* ************************************************************************
  Mann-Whitney U-test

  This test checks hypotheses about whether X  and  Y  are  samples  of  two
  continuous distributions of the same shape  and  same  median  or  whether
  their medians are different.

  The following tests are performed:
  * two-tailed test (null hypothesis - the medians are equal)
  * left-tailed test (null hypothesis - the median of the first sample is greater than or equal to the median of the second sample)
  * right-tailed test (null hypothesis - the median of the first sample is less than or equal to the median of the second sample).

  Requirements:
  * the samples are independent
  * X and Y are continuous distributions (or discrete distributions well-approximating continuous distributions)
  * distributions of X and Y have the same shape. The only possible difference is their position (i.e. the value of the median)
  * the number of elements in each sample is not less than 5
  * the scale of measurement should be ordinal, interval or ratio (i.e. the test could not be applied to nominal variables).

  The test is non-parametric and doesn't require distributions to be normal.

  Input parameters:
  X   -   sample 1. Array whose index goes from 0 to N-1.
  N   -   size of the sample. N>=5
  Y   -   sample 2. Array whose index goes from 0 to M-1.
  M   -   size of the sample. M>=5

  Output parameters:
  BothTails   -   p-value for two-tailed test. If BothTails is less than the given significance level the null hypothesis is rejected.
  LeftTail    -   p-value for left-tailed test. If LeftTail is less than the given significance level, the null hypothesis is rejected.
  RightTail   -   p-value for right-tailed test. If RightTail is less than the given significance level the null hypothesis is rejected.

  To calculate p-values, special approximation is used. This method lets  us
  calculate p-values with satisfactory  accuracy  in  interval  [0.0001, 1].
  There is no approximation outside the [0.0001, 1] interval. Therefore,  if
  the significance level outlies this interval, the test returns 0.0001.

  Relative precision of approximation of p-value:

  N          M          Max.err.   Rms.err.
  5..10      N..10      1.4e-02    6.0e-04
  5..10      N..100     2.2e-02    5.3e-06
  10..15     N..15      1.0e-02    3.2e-04
  10..15     N..100     1.0e-02    2.2e-05
  15..100    N..100     6.1e-03    2.7e-06

  For N,M>100 accuracy checks weren't put into  practice,  but  taking  into
  account characteristics of asymptotic approximation used, precision should
  not be sharply different from the values for interval [5, 100].
  ************************************************************************ *)
procedure MannWhitneyUTest(const x: TLVec; n: TLInt; const y: TLVec; M: TLInt;
  var BothTails, LeftTail, RightTail: TLFloat);
var
  i       : TLInt;
  j       : TLInt;
  k       : TLInt;
  t       : TLInt;
  tmp     : TLFloat;
  TmpI    : TLInt;
  ns      : TLInt;
  r       : TLVec;
  c       : TLIVec;
  u       : TLFloat;
  p       : TLFloat;
  MP      : TLFloat;
  s       : TLFloat;
  SIGMA   : TLFloat;
  MU      : TLFloat;
  TieCount: TLInt;
  TieSize : TLIVec;
begin

  //
  // Prepare
  //
  if (n <= 4) or (M <= 4) then
    begin
      BothTails := 1.0;
      LeftTail := 1.0;
      RightTail := 1.0;
      Exit;
    end;
  ns := n + M;
  SetLength(r, ns  );
  SetLength(c, ns  );
  i := 0;
  while i <= n - 1 do
    begin
      r[i] := x[i];
      c[i] := 0;
      inc(i);
    end;
  i := 0;
  while i <= M - 1 do
    begin
      r[n + i] := y[i];
      c[n + i] := 1;
      inc(i);
    end;

  //
  // sort {R, C}
  //
  if ns <> 1 then
    begin
      i := 2;
      repeat
        t := i;
        while t <> 1 do
          begin
            k := t div 2;
            if AP_FP_Greater_Eq(r[k - 1], r[t - 1]) then
              begin
                t := 1;
              end
            else
              begin
                tmp := r[k - 1];
                r[k - 1] := r[t - 1];
                r[t - 1] := tmp;
                TmpI := c[k - 1];
                c[k - 1] := c[t - 1];
                c[t - 1] := TmpI;
                t := k;
              end;
          end;
        i := i + 1;
      until not(i <= ns);
      i := ns - 1;
      repeat
        tmp := r[i];
        r[i] := r[0];
        r[0] := tmp;
        TmpI := c[i];
        c[i] := c[0];
        c[0] := TmpI;
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
                    if AP_FP_Greater(r[k], r[k - 1]) then
                      begin
                        k := k + 1;
                      end;
                  end;
                if AP_FP_Greater_Eq(r[t - 1], r[k - 1]) then
                  begin
                    t := 0;
                  end
                else
                  begin
                    tmp := r[k - 1];
                    r[k - 1] := r[t - 1];
                    r[t - 1] := tmp;
                    TmpI := c[k - 1];
                    c[k - 1] := c[t - 1];
                    c[t - 1] := TmpI;
                    t := k;
                  end;
              end;
          end;
        i := i - 1;
      until not(i >= 1);
    end;

  //
  // compute tied ranks
  //
  i := 0;
  TieCount := 0;
  SetLength(TieSize, ns  );
  while i <= ns - 1 do
    begin
      j := i + 1;
      while j <= ns - 1 do
        begin
          if AP_FP_NEq(r[j], r[i]) then
            begin
              Break;
            end;
          j := j + 1;
        end;
      k := i;
      while k <= j - 1 do
        begin
          r[k] := 1 + AP_Float((i + j - 1)) / 2;
          inc(k);
        end;
      TieSize[TieCount] := j - i;
      TieCount := TieCount + 1;
      i := j;
    end;

  //
  // Compute U
  //
  u := 0;
  i := 0;
  while i <= ns - 1 do
    begin
      if c[i] = 0 then
        begin
          u := u + r[i];
        end;
      inc(i);
    end;
  u := n * M + n * (n + 1) div 2 - u;

  //
  // Result
  //
  MU := AP_Float(n * M) / 2;
  tmp := ns * (AP_Sqr(ns) - 1) / 12;
  i := 0;
  while i <= TieCount - 1 do
    begin
      tmp := tmp - TieSize[i] * (AP_Sqr(TieSize[i]) - 1) / 12;
      inc(i);
    end;
  SIGMA := Sqrt(AP_Float(M * n) / ns / (ns - 1) * tmp);
  s := (u - MU) / SIGMA;
  if AP_FP_Less_Eq(s, 0) then
    begin
      p := Exp(USigma(-(u - MU) / SIGMA, n, M));
      MP := 1 - Exp(USigma(-(u - 1 - MU) / SIGMA, n, M));
    end
  else
    begin
      MP := Exp(USigma((u - MU) / SIGMA, n, M));
      p := 1 - Exp(USigma((u + 1 - MU) / SIGMA, n, M));
    end;
  BothTails := Max(2 * Min(p, MP), 1.0E-4);
  LeftTail := Max(MP, 1.0E-4);
  RightTail := Max(p, 1.0E-4);
end;

(* ************************************************************************
  Sequential Chebyshev interpolation.
  ************************************************************************ *)
procedure UCheb(const x, c: TLFloat; var TJ: TLFloat; var TJ1: TLFloat; var r: TLFloat);
var
  t: TLFloat;
begin
  r := r + c * TJ;
  t := 2 * x * TJ1 - TJ;
  TJ := TJ1;
  TJ1 := t;
end;

(* ************************************************************************
  Three-point polynomial interpolation.
  ************************************************************************ *)
function UNInterpolate(const p1, p2, p3: TLFloat; const n: TLInt): TLFloat;
var
  t1 : TLFloat;
  t2 : TLFloat;
  t3 : TLFloat;
  t  : TLFloat;
  P12: TLFloat;
  P23: TLFloat;
begin
  t1 := 1.0 / 15.0;
  t2 := 1.0 / 30.0;
  t3 := 1.0 / 100.0;
  t := 1.0 / n;
  P12 := ((t - t2) * p1 + (t1 - t) * p2) / (t1 - t2);
  P23 := ((t - t3) * p2 + (t2 - t) * p3) / (t2 - t3);
  Result := ((t - t3) * P12 + (t1 - t) * P23) / (t1 - t3);
end;

(* ************************************************************************
  Tail(0, N1, N2)
  ************************************************************************ *)
function USigma000(const n1, n2: TLInt): TLFloat;
var
  p1: TLFloat;
  p2: TLFloat;
  p3: TLFloat;
begin
  p1 := UNInterpolate(-6.76984E-01, -6.83700E-01, -6.89873E-01, n2);
  p2 := UNInterpolate(-6.83700E-01, -6.87311E-01, -6.90957E-01, n2);
  p3 := UNInterpolate(-6.89873E-01, -6.90957E-01, -6.92175E-01, n2);
  Result := UNInterpolate(p1, p2, p3, n1);
end;

(* ************************************************************************
  Tail(0.75, N1, N2)
  ************************************************************************ *)
function USigma075(const n1, n2: TLInt): TLFloat;
var
  p1: TLFloat;
  p2: TLFloat;
  p3: TLFloat;
begin
  p1 := UNInterpolate(-1.44500E+00, -1.45906E+00, -1.47063E+00, n2);
  p2 := UNInterpolate(-1.45906E+00, -1.46856E+00, -1.47644E+00, n2);
  p3 := UNInterpolate(-1.47063E+00, -1.47644E+00, -1.48100E+00, n2);
  Result := UNInterpolate(p1, p2, p3, n1);
end;

(* ************************************************************************
  Tail(1.5, N1, N2)
  ************************************************************************ *)
function USigma150(const n1, n2: TLInt): TLFloat;
var
  p1: TLFloat;
  p2: TLFloat;
  p3: TLFloat;
begin
  p1 := UNInterpolate(-2.65380E+00, -2.67352E+00, -2.69011E+00, n2);
  p2 := UNInterpolate(-2.67352E+00, -2.68591E+00, -2.69659E+00, n2);
  p3 := UNInterpolate(-2.69011E+00, -2.69659E+00, -2.70192E+00, n2);
  Result := UNInterpolate(p1, p2, p3, n1);
end;

(* ************************************************************************
  Tail(2.25, N1, N2)
  ************************************************************************ *)
function USigma225(const n1, n2: TLInt): TLFloat;
var
  p1: TLFloat;
  p2: TLFloat;
  p3: TLFloat;
begin
  p1 := UNInterpolate(-4.41465E+00, -4.42260E+00, -4.43702E+00, n2);
  p2 := UNInterpolate(-4.42260E+00, -4.41639E+00, -4.41928E+00, n2);
  p3 := UNInterpolate(-4.43702E+00, -4.41928E+00, -4.41030E+00, n2);
  Result := UNInterpolate(p1, p2, p3, n1);
end;

(* ************************************************************************
  Tail(3.0, N1, N2)
  ************************************************************************ *)
function USigma300(const n1, n2: TLInt): TLFloat;
var
  p1: TLFloat;
  p2: TLFloat;
  p3: TLFloat;
begin
  p1 := UNInterpolate(-6.89839E+00, -6.83477E+00, -6.82340E+00, n2);
  p2 := UNInterpolate(-6.83477E+00, -6.74559E+00, -6.71117E+00, n2);
  p3 := UNInterpolate(-6.82340E+00, -6.71117E+00, -6.64929E+00, n2);
  Result := UNInterpolate(p1, p2, p3, n1);
end;

(* ************************************************************************
  Tail(3.33, N1, N2)
  ************************************************************************ *)
function USigma333(const n1, n2: TLInt): TLFloat;
var
  p1: TLFloat;
  p2: TLFloat;
  p3: TLFloat;
begin
  p1 := UNInterpolate(-8.31272E+00, -8.17096E+00, -8.13125E+00, n2);
  p2 := UNInterpolate(-8.17096E+00, -8.00156E+00, -7.93245E+00, n2);
  p3 := UNInterpolate(-8.13125E+00, -7.93245E+00, -7.82502E+00, n2);
  Result := UNInterpolate(p1, p2, p3, n1);
end;

(* ************************************************************************
  Tail(3.66, N1, N2)
  ************************************************************************ *)
function USigma367(const n1, n2: TLInt): TLFloat;
var
  p1: TLFloat;
  p2: TLFloat;
  p3: TLFloat;
begin
  p1 := UNInterpolate(-9.98837E+00, -9.70844E+00, -9.62087E+00, n2);
  p2 := UNInterpolate(-9.70844E+00, -9.41156E+00, -9.28998E+00, n2);
  p3 := UNInterpolate(-9.62087E+00, -9.28998E+00, -9.11686E+00, n2);
  Result := UNInterpolate(p1, p2, p3, n1);
end;

(* ************************************************************************
  Tail(4.0, N1, N2)
  ************************************************************************ *)
function USigma400(const n1, n2: TLInt): TLFloat;
var
  p1: TLFloat;
  p2: TLFloat;
  p3: TLFloat;
begin
  p1 := UNInterpolate(-1.20250E+01, -1.14911E+01, -1.13231E+01, n2);
  p2 := UNInterpolate(-1.14911E+01, -1.09927E+01, -1.07937E+01, n2);
  p3 := UNInterpolate(-1.13231E+01, -1.07937E+01, -1.05285E+01, n2);
  Result := UNInterpolate(p1, p2, p3, n1);
end;

(* ************************************************************************
  Tail(S, 5, 5)
  ************************************************************************ *)
function UTblN5N5(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 2.611165E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -2.596264E+00, TJ, TJ1, Result);
  UCheb(x, -2.412086E+00, TJ, TJ1, Result);
  UCheb(x, -4.858542E-01, TJ, TJ1, Result);
  UCheb(x, -5.614282E-02, TJ, TJ1, Result);
  UCheb(x, 3.372686E-03, TJ, TJ1, Result);
  UCheb(x, 8.524731E-03, TJ, TJ1, Result);
  UCheb(x, 4.435331E-03, TJ, TJ1, Result);
  UCheb(x, 1.284665E-03, TJ, TJ1, Result);
  UCheb(x, 4.184141E-03, TJ, TJ1, Result);
  UCheb(x, 5.298360E-03, TJ, TJ1, Result);
  UCheb(x, 7.447272E-04, TJ, TJ1, Result);
  UCheb(x, -3.938769E-03, TJ, TJ1, Result);
  UCheb(x, -4.276205E-03, TJ, TJ1, Result);
  UCheb(x, -1.138481E-03, TJ, TJ1, Result);
  UCheb(x, 8.684625E-04, TJ, TJ1, Result);
  UCheb(x, 1.558104E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 6)
  ************************************************************************ *)
function UTblN5N6(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 2.738613E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -2.810459E+00, TJ, TJ1, Result);
  UCheb(x, -2.684429E+00, TJ, TJ1, Result);
  UCheb(x, -5.712858E-01, TJ, TJ1, Result);
  UCheb(x, -8.009324E-02, TJ, TJ1, Result);
  UCheb(x, -6.644391E-03, TJ, TJ1, Result);
  UCheb(x, 6.034173E-03, TJ, TJ1, Result);
  UCheb(x, 4.953498E-03, TJ, TJ1, Result);
  UCheb(x, 3.279293E-03, TJ, TJ1, Result);
  UCheb(x, 3.563485E-03, TJ, TJ1, Result);
  UCheb(x, 4.971952E-03, TJ, TJ1, Result);
  UCheb(x, 3.506309E-03, TJ, TJ1, Result);
  UCheb(x, -1.541406E-04, TJ, TJ1, Result);
  UCheb(x, -3.283205E-03, TJ, TJ1, Result);
  UCheb(x, -3.016347E-03, TJ, TJ1, Result);
  UCheb(x, -1.221626E-03, TJ, TJ1, Result);
  UCheb(x, -1.286752E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 7)
  ************************************************************************ *)
function UTblN5N7(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 2.841993E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -2.994677E+00, TJ, TJ1, Result);
  UCheb(x, -2.923264E+00, TJ, TJ1, Result);
  UCheb(x, -6.506190E-01, TJ, TJ1, Result);
  UCheb(x, -1.054280E-01, TJ, TJ1, Result);
  UCheb(x, -1.794587E-02, TJ, TJ1, Result);
  UCheb(x, 1.726290E-03, TJ, TJ1, Result);
  UCheb(x, 4.534180E-03, TJ, TJ1, Result);
  UCheb(x, 4.517845E-03, TJ, TJ1, Result);
  UCheb(x, 3.904428E-03, TJ, TJ1, Result);
  UCheb(x, 3.882443E-03, TJ, TJ1, Result);
  UCheb(x, 3.482988E-03, TJ, TJ1, Result);
  UCheb(x, 2.114875E-03, TJ, TJ1, Result);
  UCheb(x, -1.515082E-04, TJ, TJ1, Result);
  UCheb(x, -1.996056E-03, TJ, TJ1, Result);
  UCheb(x, -2.293581E-03, TJ, TJ1, Result);
  UCheb(x, -2.349444E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 8)
  ************************************************************************ *)
function UTblN5N8(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 2.927700E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.155727E+00, TJ, TJ1, Result);
  UCheb(x, -3.135078E+00, TJ, TJ1, Result);
  UCheb(x, -7.247203E-01, TJ, TJ1, Result);
  UCheb(x, -1.309697E-01, TJ, TJ1, Result);
  UCheb(x, -2.993725E-02, TJ, TJ1, Result);
  UCheb(x, -3.567219E-03, TJ, TJ1, Result);
  UCheb(x, 3.383704E-03, TJ, TJ1, Result);
  UCheb(x, 5.002188E-03, TJ, TJ1, Result);
  UCheb(x, 4.487322E-03, TJ, TJ1, Result);
  UCheb(x, 3.443899E-03, TJ, TJ1, Result);
  UCheb(x, 2.688270E-03, TJ, TJ1, Result);
  UCheb(x, 2.600339E-03, TJ, TJ1, Result);
  UCheb(x, 1.874948E-03, TJ, TJ1, Result);
  UCheb(x, 1.811593E-04, TJ, TJ1, Result);
  UCheb(x, -1.072353E-03, TJ, TJ1, Result);
  UCheb(x, -2.659457E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 9)
  ************************************************************************ *)
function UTblN5N9(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.000000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.298162E+00, TJ, TJ1, Result);
  UCheb(x, -3.325016E+00, TJ, TJ1, Result);
  UCheb(x, -7.939852E-01, TJ, TJ1, Result);
  UCheb(x, -1.563029E-01, TJ, TJ1, Result);
  UCheb(x, -4.222652E-02, TJ, TJ1, Result);
  UCheb(x, -9.195200E-03, TJ, TJ1, Result);
  UCheb(x, 1.445665E-03, TJ, TJ1, Result);
  UCheb(x, 5.204792E-03, TJ, TJ1, Result);
  UCheb(x, 4.775217E-03, TJ, TJ1, Result);
  UCheb(x, 3.527781E-03, TJ, TJ1, Result);
  UCheb(x, 2.221948E-03, TJ, TJ1, Result);
  UCheb(x, 2.242968E-03, TJ, TJ1, Result);
  UCheb(x, 2.607959E-03, TJ, TJ1, Result);
  UCheb(x, 1.771285E-03, TJ, TJ1, Result);
  UCheb(x, 6.694026E-04, TJ, TJ1, Result);
  UCheb(x, -1.481190E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 10)
  ************************************************************************ *)
function UTblN5N10(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.061862E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.425360E+00, TJ, TJ1, Result);
  UCheb(x, -3.496710E+00, TJ, TJ1, Result);
  UCheb(x, -8.587658E-01, TJ, TJ1, Result);
  UCheb(x, -1.812005E-01, TJ, TJ1, Result);
  UCheb(x, -5.427637E-02, TJ, TJ1, Result);
  UCheb(x, -1.515702E-02, TJ, TJ1, Result);
  UCheb(x, -5.406867E-04, TJ, TJ1, Result);
  UCheb(x, 4.796295E-03, TJ, TJ1, Result);
  UCheb(x, 5.237591E-03, TJ, TJ1, Result);
  UCheb(x, 3.654249E-03, TJ, TJ1, Result);
  UCheb(x, 2.181165E-03, TJ, TJ1, Result);
  UCheb(x, 2.011665E-03, TJ, TJ1, Result);
  UCheb(x, 2.417927E-03, TJ, TJ1, Result);
  UCheb(x, 2.534880E-03, TJ, TJ1, Result);
  UCheb(x, 1.791255E-03, TJ, TJ1, Result);
  UCheb(x, 1.871512E-05, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 11)
  ************************************************************************ *)
function UTblN5N11(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.115427E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.539959E+00, TJ, TJ1, Result);
  UCheb(x, -3.652998E+00, TJ, TJ1, Result);
  UCheb(x, -9.196503E-01, TJ, TJ1, Result);
  UCheb(x, -2.054363E-01, TJ, TJ1, Result);
  UCheb(x, -6.618848E-02, TJ, TJ1, Result);
  UCheb(x, -2.109411E-02, TJ, TJ1, Result);
  UCheb(x, -2.786668E-03, TJ, TJ1, Result);
  UCheb(x, 4.215648E-03, TJ, TJ1, Result);
  UCheb(x, 5.484220E-03, TJ, TJ1, Result);
  UCheb(x, 3.935991E-03, TJ, TJ1, Result);
  UCheb(x, 2.396191E-03, TJ, TJ1, Result);
  UCheb(x, 1.894177E-03, TJ, TJ1, Result);
  UCheb(x, 2.206979E-03, TJ, TJ1, Result);
  UCheb(x, 2.519055E-03, TJ, TJ1, Result);
  UCheb(x, 2.210326E-03, TJ, TJ1, Result);
  UCheb(x, 1.189679E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 12)
  ************************************************************************ *)
function UTblN5N12(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.162278E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.644007E+00, TJ, TJ1, Result);
  UCheb(x, -3.796173E+00, TJ, TJ1, Result);
  UCheb(x, -9.771177E-01, TJ, TJ1, Result);
  UCheb(x, -2.290043E-01, TJ, TJ1, Result);
  UCheb(x, -7.794686E-02, TJ, TJ1, Result);
  UCheb(x, -2.702110E-02, TJ, TJ1, Result);
  UCheb(x, -5.185959E-03, TJ, TJ1, Result);
  UCheb(x, 3.416259E-03, TJ, TJ1, Result);
  UCheb(x, 5.592056E-03, TJ, TJ1, Result);
  UCheb(x, 4.201530E-03, TJ, TJ1, Result);
  UCheb(x, 2.754365E-03, TJ, TJ1, Result);
  UCheb(x, 1.978945E-03, TJ, TJ1, Result);
  UCheb(x, 2.012032E-03, TJ, TJ1, Result);
  UCheb(x, 2.304579E-03, TJ, TJ1, Result);
  UCheb(x, 2.100378E-03, TJ, TJ1, Result);
  UCheb(x, 1.728269E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 13)
  ************************************************************************ *)
function UTblN5N13(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.203616E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.739120E+00, TJ, TJ1, Result);
  UCheb(x, -3.928117E+00, TJ, TJ1, Result);
  UCheb(x, -1.031605E+00, TJ, TJ1, Result);
  UCheb(x, -2.519403E-01, TJ, TJ1, Result);
  UCheb(x, -8.962648E-02, TJ, TJ1, Result);
  UCheb(x, -3.292183E-02, TJ, TJ1, Result);
  UCheb(x, -7.809293E-03, TJ, TJ1, Result);
  UCheb(x, 2.465156E-03, TJ, TJ1, Result);
  UCheb(x, 5.456278E-03, TJ, TJ1, Result);
  UCheb(x, 4.446055E-03, TJ, TJ1, Result);
  UCheb(x, 3.109490E-03, TJ, TJ1, Result);
  UCheb(x, 2.218256E-03, TJ, TJ1, Result);
  UCheb(x, 1.941479E-03, TJ, TJ1, Result);
  UCheb(x, 2.058603E-03, TJ, TJ1, Result);
  UCheb(x, 1.824402E-03, TJ, TJ1, Result);
  UCheb(x, 1.830947E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 14)
  ************************************************************************ *)
function UTblN5N14(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.240370E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.826559E+00, TJ, TJ1, Result);
  UCheb(x, -4.050370E+00, TJ, TJ1, Result);
  UCheb(x, -1.083408E+00, TJ, TJ1, Result);
  UCheb(x, -2.743164E-01, TJ, TJ1, Result);
  UCheb(x, -1.012030E-01, TJ, TJ1, Result);
  UCheb(x, -3.884686E-02, TJ, TJ1, Result);
  UCheb(x, -1.059656E-02, TJ, TJ1, Result);
  UCheb(x, 1.327521E-03, TJ, TJ1, Result);
  UCheb(x, 5.134026E-03, TJ, TJ1, Result);
  UCheb(x, 4.584201E-03, TJ, TJ1, Result);
  UCheb(x, 3.440618E-03, TJ, TJ1, Result);
  UCheb(x, 2.524133E-03, TJ, TJ1, Result);
  UCheb(x, 1.990007E-03, TJ, TJ1, Result);
  UCheb(x, 1.887334E-03, TJ, TJ1, Result);
  UCheb(x, 1.534977E-03, TJ, TJ1, Result);
  UCheb(x, 1.705395E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 15)
  ************************************************************************ *)
function UTblN5N15(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.250000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.851572E+00, TJ, TJ1, Result);
  UCheb(x, -4.082033E+00, TJ, TJ1, Result);
  UCheb(x, -1.095983E+00, TJ, TJ1, Result);
  UCheb(x, -2.814595E-01, TJ, TJ1, Result);
  UCheb(x, -1.073148E-01, TJ, TJ1, Result);
  UCheb(x, -4.420213E-02, TJ, TJ1, Result);
  UCheb(x, -1.517175E-02, TJ, TJ1, Result);
  UCheb(x, -2.344180E-03, TJ, TJ1, Result);
  UCheb(x, 2.371393E-03, TJ, TJ1, Result);
  UCheb(x, 2.711443E-03, TJ, TJ1, Result);
  UCheb(x, 2.228569E-03, TJ, TJ1, Result);
  UCheb(x, 1.683483E-03, TJ, TJ1, Result);
  UCheb(x, 1.267112E-03, TJ, TJ1, Result);
  UCheb(x, 1.156044E-03, TJ, TJ1, Result);
  UCheb(x, 9.131316E-04, TJ, TJ1, Result);
  UCheb(x, 1.301023E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 16)
  ************************************************************************ *)
function UTblN5N16(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.250000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.852210E+00, TJ, TJ1, Result);
  UCheb(x, -4.077482E+00, TJ, TJ1, Result);
  UCheb(x, -1.091186E+00, TJ, TJ1, Result);
  UCheb(x, -2.797282E-01, TJ, TJ1, Result);
  UCheb(x, -1.084994E-01, TJ, TJ1, Result);
  UCheb(x, -4.667054E-02, TJ, TJ1, Result);
  UCheb(x, -1.843909E-02, TJ, TJ1, Result);
  UCheb(x, -5.456732E-03, TJ, TJ1, Result);
  UCheb(x, -5.039830E-04, TJ, TJ1, Result);
  UCheb(x, 4.723508E-04, TJ, TJ1, Result);
  UCheb(x, 3.940608E-04, TJ, TJ1, Result);
  UCheb(x, 1.478285E-04, TJ, TJ1, Result);
  UCheb(x, -1.649144E-04, TJ, TJ1, Result);
  UCheb(x, -4.237703E-04, TJ, TJ1, Result);
  UCheb(x, -4.707410E-04, TJ, TJ1, Result);
  UCheb(x, -1.874293E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 17)
  ************************************************************************ *)
function UTblN5N17(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.250000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.851752E+00, TJ, TJ1, Result);
  UCheb(x, -4.071259E+00, TJ, TJ1, Result);
  UCheb(x, -1.084700E+00, TJ, TJ1, Result);
  UCheb(x, -2.758898E-01, TJ, TJ1, Result);
  UCheb(x, -1.073846E-01, TJ, TJ1, Result);
  UCheb(x, -4.684838E-02, TJ, TJ1, Result);
  UCheb(x, -1.964936E-02, TJ, TJ1, Result);
  UCheb(x, -6.782442E-03, TJ, TJ1, Result);
  UCheb(x, -1.956362E-03, TJ, TJ1, Result);
  UCheb(x, -5.984727E-04, TJ, TJ1, Result);
  UCheb(x, -5.196936E-04, TJ, TJ1, Result);
  UCheb(x, -5.558262E-04, TJ, TJ1, Result);
  UCheb(x, -8.690746E-04, TJ, TJ1, Result);
  UCheb(x, -1.364855E-03, TJ, TJ1, Result);
  UCheb(x, -1.401006E-03, TJ, TJ1, Result);
  UCheb(x, -1.546748E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 18)
  ************************************************************************ *)
function UTblN5N18(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.250000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.850840E+00, TJ, TJ1, Result);
  UCheb(x, -4.064799E+00, TJ, TJ1, Result);
  UCheb(x, -1.077651E+00, TJ, TJ1, Result);
  UCheb(x, -2.712659E-01, TJ, TJ1, Result);
  UCheb(x, -1.049217E-01, TJ, TJ1, Result);
  UCheb(x, -4.571333E-02, TJ, TJ1, Result);
  UCheb(x, -1.929809E-02, TJ, TJ1, Result);
  UCheb(x, -6.752044E-03, TJ, TJ1, Result);
  UCheb(x, -1.949464E-03, TJ, TJ1, Result);
  UCheb(x, -3.896101E-04, TJ, TJ1, Result);
  UCheb(x, -4.614460E-05, TJ, TJ1, Result);
  UCheb(x, 1.384357E-04, TJ, TJ1, Result);
  UCheb(x, -6.489113E-05, TJ, TJ1, Result);
  UCheb(x, -6.445725E-04, TJ, TJ1, Result);
  UCheb(x, -8.945636E-04, TJ, TJ1, Result);
  UCheb(x, -1.424653E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 19)
  ************************************************************************ *)
function UTblN5N19(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.250000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.850027E+00, TJ, TJ1, Result);
  UCheb(x, -4.059159E+00, TJ, TJ1, Result);
  UCheb(x, -1.071106E+00, TJ, TJ1, Result);
  UCheb(x, -2.669960E-01, TJ, TJ1, Result);
  UCheb(x, -1.022780E-01, TJ, TJ1, Result);
  UCheb(x, -4.442555E-02, TJ, TJ1, Result);
  UCheb(x, -1.851335E-02, TJ, TJ1, Result);
  UCheb(x, -6.433865E-03, TJ, TJ1, Result);
  UCheb(x, -1.514465E-03, TJ, TJ1, Result);
  UCheb(x, 1.332989E-04, TJ, TJ1, Result);
  UCheb(x, 8.606099E-04, TJ, TJ1, Result);
  UCheb(x, 1.341945E-03, TJ, TJ1, Result);
  UCheb(x, 1.402164E-03, TJ, TJ1, Result);
  UCheb(x, 1.039761E-03, TJ, TJ1, Result);
  UCheb(x, 5.512831E-04, TJ, TJ1, Result);
  UCheb(x, -3.284427E-05, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 20)
  ************************************************************************ *)
function UTblN5N20(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.250000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.849651E+00, TJ, TJ1, Result);
  UCheb(x, -4.054729E+00, TJ, TJ1, Result);
  UCheb(x, -1.065747E+00, TJ, TJ1, Result);
  UCheb(x, -2.636243E-01, TJ, TJ1, Result);
  UCheb(x, -1.003234E-01, TJ, TJ1, Result);
  UCheb(x, -4.372789E-02, TJ, TJ1, Result);
  UCheb(x, -1.831551E-02, TJ, TJ1, Result);
  UCheb(x, -6.763090E-03, TJ, TJ1, Result);
  UCheb(x, -1.830626E-03, TJ, TJ1, Result);
  UCheb(x, -2.122384E-04, TJ, TJ1, Result);
  UCheb(x, 8.108328E-04, TJ, TJ1, Result);
  UCheb(x, 1.557983E-03, TJ, TJ1, Result);
  UCheb(x, 1.945666E-03, TJ, TJ1, Result);
  UCheb(x, 1.965696E-03, TJ, TJ1, Result);
  UCheb(x, 1.493236E-03, TJ, TJ1, Result);
  UCheb(x, 1.162591E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 21)
  ************************************************************************ *)
function UTblN5N21(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.250000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.849649E+00, TJ, TJ1, Result);
  UCheb(x, -4.051155E+00, TJ, TJ1, Result);
  UCheb(x, -1.061430E+00, TJ, TJ1, Result);
  UCheb(x, -2.608869E-01, TJ, TJ1, Result);
  UCheb(x, -9.902788E-02, TJ, TJ1, Result);
  UCheb(x, -4.346562E-02, TJ, TJ1, Result);
  UCheb(x, -1.874709E-02, TJ, TJ1, Result);
  UCheb(x, -7.682887E-03, TJ, TJ1, Result);
  UCheb(x, -3.026206E-03, TJ, TJ1, Result);
  UCheb(x, -1.534551E-03, TJ, TJ1, Result);
  UCheb(x, -4.990575E-04, TJ, TJ1, Result);
  UCheb(x, 3.713334E-04, TJ, TJ1, Result);
  UCheb(x, 9.737011E-04, TJ, TJ1, Result);
  UCheb(x, 1.304571E-03, TJ, TJ1, Result);
  UCheb(x, 1.133110E-03, TJ, TJ1, Result);
  UCheb(x, 1.123457E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 22)
  ************************************************************************ *)
function UTblN5N22(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.250000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.849598E+00, TJ, TJ1, Result);
  UCheb(x, -4.047605E+00, TJ, TJ1, Result);
  UCheb(x, -1.057264E+00, TJ, TJ1, Result);
  UCheb(x, -2.579513E-01, TJ, TJ1, Result);
  UCheb(x, -9.749602E-02, TJ, TJ1, Result);
  UCheb(x, -4.275137E-02, TJ, TJ1, Result);
  UCheb(x, -1.881768E-02, TJ, TJ1, Result);
  UCheb(x, -8.177374E-03, TJ, TJ1, Result);
  UCheb(x, -3.981056E-03, TJ, TJ1, Result);
  UCheb(x, -2.696290E-03, TJ, TJ1, Result);
  UCheb(x, -1.886803E-03, TJ, TJ1, Result);
  UCheb(x, -1.085378E-03, TJ, TJ1, Result);
  UCheb(x, -4.675242E-04, TJ, TJ1, Result);
  UCheb(x, -5.426367E-05, TJ, TJ1, Result);
  UCheb(x, 1.039613E-04, TJ, TJ1, Result);
  UCheb(x, 2.662378E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 23)
  ************************************************************************ *)
function UTblN5N23(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.250000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.849269E+00, TJ, TJ1, Result);
  UCheb(x, -4.043761E+00, TJ, TJ1, Result);
  UCheb(x, -1.052735E+00, TJ, TJ1, Result);
  UCheb(x, -2.544683E-01, TJ, TJ1, Result);
  UCheb(x, -9.517503E-02, TJ, TJ1, Result);
  UCheb(x, -4.112082E-02, TJ, TJ1, Result);
  UCheb(x, -1.782070E-02, TJ, TJ1, Result);
  UCheb(x, -7.549483E-03, TJ, TJ1, Result);
  UCheb(x, -3.747329E-03, TJ, TJ1, Result);
  UCheb(x, -2.694263E-03, TJ, TJ1, Result);
  UCheb(x, -2.147141E-03, TJ, TJ1, Result);
  UCheb(x, -1.526209E-03, TJ, TJ1, Result);
  UCheb(x, -1.039173E-03, TJ, TJ1, Result);
  UCheb(x, -7.235615E-04, TJ, TJ1, Result);
  UCheb(x, -4.656546E-04, TJ, TJ1, Result);
  UCheb(x, -3.014423E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 24)
  ************************************************************************ *)
function UTblN5N24(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.250000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.848925E+00, TJ, TJ1, Result);
  UCheb(x, -4.040178E+00, TJ, TJ1, Result);
  UCheb(x, -1.048355E+00, TJ, TJ1, Result);
  UCheb(x, -2.510198E-01, TJ, TJ1, Result);
  UCheb(x, -9.261134E-02, TJ, TJ1, Result);
  UCheb(x, -3.915864E-02, TJ, TJ1, Result);
  UCheb(x, -1.627423E-02, TJ, TJ1, Result);
  UCheb(x, -6.307345E-03, TJ, TJ1, Result);
  UCheb(x, -2.732992E-03, TJ, TJ1, Result);
  UCheb(x, -1.869652E-03, TJ, TJ1, Result);
  UCheb(x, -1.494176E-03, TJ, TJ1, Result);
  UCheb(x, -1.047533E-03, TJ, TJ1, Result);
  UCheb(x, -7.178439E-04, TJ, TJ1, Result);
  UCheb(x, -5.424171E-04, TJ, TJ1, Result);
  UCheb(x, -3.829195E-04, TJ, TJ1, Result);
  UCheb(x, -2.840810E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 25)
  ************************************************************************ *)
function UTblN5N25(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.250000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.848937E+00, TJ, TJ1, Result);
  UCheb(x, -4.037512E+00, TJ, TJ1, Result);
  UCheb(x, -1.044866E+00, TJ, TJ1, Result);
  UCheb(x, -2.483269E-01, TJ, TJ1, Result);
  UCheb(x, -9.063682E-02, TJ, TJ1, Result);
  UCheb(x, -3.767778E-02, TJ, TJ1, Result);
  UCheb(x, -1.508540E-02, TJ, TJ1, Result);
  UCheb(x, -5.332756E-03, TJ, TJ1, Result);
  UCheb(x, -1.881511E-03, TJ, TJ1, Result);
  UCheb(x, -1.124041E-03, TJ, TJ1, Result);
  UCheb(x, -8.368456E-04, TJ, TJ1, Result);
  UCheb(x, -4.930499E-04, TJ, TJ1, Result);
  UCheb(x, -2.779630E-04, TJ, TJ1, Result);
  UCheb(x, -2.029528E-04, TJ, TJ1, Result);
  UCheb(x, -1.658678E-04, TJ, TJ1, Result);
  UCheb(x, -1.289695E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 26)
  ************************************************************************ *)
function UTblN5N26(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.250000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.849416E+00, TJ, TJ1, Result);
  UCheb(x, -4.035915E+00, TJ, TJ1, Result);
  UCheb(x, -1.042493E+00, TJ, TJ1, Result);
  UCheb(x, -2.466021E-01, TJ, TJ1, Result);
  UCheb(x, -8.956432E-02, TJ, TJ1, Result);
  UCheb(x, -3.698914E-02, TJ, TJ1, Result);
  UCheb(x, -1.465689E-02, TJ, TJ1, Result);
  UCheb(x, -5.035254E-03, TJ, TJ1, Result);
  UCheb(x, -1.674614E-03, TJ, TJ1, Result);
  UCheb(x, -9.492734E-04, TJ, TJ1, Result);
  UCheb(x, -7.014021E-04, TJ, TJ1, Result);
  UCheb(x, -3.944953E-04, TJ, TJ1, Result);
  UCheb(x, -2.255750E-04, TJ, TJ1, Result);
  UCheb(x, -2.075841E-04, TJ, TJ1, Result);
  UCheb(x, -1.989330E-04, TJ, TJ1, Result);
  UCheb(x, -2.134862E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 27)
  ************************************************************************ *)
function UTblN5N27(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.250000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.850070E+00, TJ, TJ1, Result);
  UCheb(x, -4.034815E+00, TJ, TJ1, Result);
  UCheb(x, -1.040650E+00, TJ, TJ1, Result);
  UCheb(x, -2.453117E-01, TJ, TJ1, Result);
  UCheb(x, -8.886426E-02, TJ, TJ1, Result);
  UCheb(x, -3.661702E-02, TJ, TJ1, Result);
  UCheb(x, -1.452346E-02, TJ, TJ1, Result);
  UCheb(x, -5.002476E-03, TJ, TJ1, Result);
  UCheb(x, -1.720126E-03, TJ, TJ1, Result);
  UCheb(x, -1.001400E-03, TJ, TJ1, Result);
  UCheb(x, -7.729826E-04, TJ, TJ1, Result);
  UCheb(x, -4.740640E-04, TJ, TJ1, Result);
  UCheb(x, -3.206333E-04, TJ, TJ1, Result);
  UCheb(x, -3.366093E-04, TJ, TJ1, Result);
  UCheb(x, -3.193471E-04, TJ, TJ1, Result);
  UCheb(x, -3.804091E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 28)
  ************************************************************************ *)
function UTblN5N28(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.250000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.850668E+00, TJ, TJ1, Result);
  UCheb(x, -4.033786E+00, TJ, TJ1, Result);
  UCheb(x, -1.038853E+00, TJ, TJ1, Result);
  UCheb(x, -2.440281E-01, TJ, TJ1, Result);
  UCheb(x, -8.806020E-02, TJ, TJ1, Result);
  UCheb(x, -3.612883E-02, TJ, TJ1, Result);
  UCheb(x, -1.420436E-02, TJ, TJ1, Result);
  UCheb(x, -4.787982E-03, TJ, TJ1, Result);
  UCheb(x, -1.535230E-03, TJ, TJ1, Result);
  UCheb(x, -8.263121E-04, TJ, TJ1, Result);
  UCheb(x, -5.849609E-04, TJ, TJ1, Result);
  UCheb(x, -2.863967E-04, TJ, TJ1, Result);
  UCheb(x, -1.391610E-04, TJ, TJ1, Result);
  UCheb(x, -1.720294E-04, TJ, TJ1, Result);
  UCheb(x, -1.952273E-04, TJ, TJ1, Result);
  UCheb(x, -2.901413E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 29)
  ************************************************************************ *)
function UTblN5N29(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.250000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.851217E+00, TJ, TJ1, Result);
  UCheb(x, -4.032834E+00, TJ, TJ1, Result);
  UCheb(x, -1.037113E+00, TJ, TJ1, Result);
  UCheb(x, -2.427762E-01, TJ, TJ1, Result);
  UCheb(x, -8.719146E-02, TJ, TJ1, Result);
  UCheb(x, -3.557172E-02, TJ, TJ1, Result);
  UCheb(x, -1.375498E-02, TJ, TJ1, Result);
  UCheb(x, -4.452033E-03, TJ, TJ1, Result);
  UCheb(x, -1.187516E-03, TJ, TJ1, Result);
  UCheb(x, -4.916936E-04, TJ, TJ1, Result);
  UCheb(x, -2.065533E-04, TJ, TJ1, Result);
  UCheb(x, 1.067301E-04, TJ, TJ1, Result);
  UCheb(x, 2.615824E-04, TJ, TJ1, Result);
  UCheb(x, 2.432244E-04, TJ, TJ1, Result);
  UCheb(x, 1.417795E-04, TJ, TJ1, Result);
  UCheb(x, 4.710038E-05, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 30)
  ************************************************************************ *)
function UTblN5N30(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.250000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.851845E+00, TJ, TJ1, Result);
  UCheb(x, -4.032148E+00, TJ, TJ1, Result);
  UCheb(x, -1.035679E+00, TJ, TJ1, Result);
  UCheb(x, -2.417758E-01, TJ, TJ1, Result);
  UCheb(x, -8.655330E-02, TJ, TJ1, Result);
  UCheb(x, -3.522132E-02, TJ, TJ1, Result);
  UCheb(x, -1.352106E-02, TJ, TJ1, Result);
  UCheb(x, -4.326911E-03, TJ, TJ1, Result);
  UCheb(x, -1.064969E-03, TJ, TJ1, Result);
  UCheb(x, -3.813321E-04, TJ, TJ1, Result);
  UCheb(x, -5.683881E-05, TJ, TJ1, Result);
  UCheb(x, 2.813346E-04, TJ, TJ1, Result);
  UCheb(x, 4.627085E-04, TJ, TJ1, Result);
  UCheb(x, 4.832107E-04, TJ, TJ1, Result);
  UCheb(x, 3.519336E-04, TJ, TJ1, Result);
  UCheb(x, 2.888530E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 5, 100)
  ************************************************************************ *)
function UTblN5N100(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.250000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.877940E+00, TJ, TJ1, Result);
  UCheb(x, -4.039324E+00, TJ, TJ1, Result);
  UCheb(x, -1.022243E+00, TJ, TJ1, Result);
  UCheb(x, -2.305825E-01, TJ, TJ1, Result);
  UCheb(x, -7.960119E-02, TJ, TJ1, Result);
  UCheb(x, -3.112000E-02, TJ, TJ1, Result);
  UCheb(x, -1.138868E-02, TJ, TJ1, Result);
  UCheb(x, -3.418164E-03, TJ, TJ1, Result);
  UCheb(x, -9.174520E-04, TJ, TJ1, Result);
  UCheb(x, -5.489617E-04, TJ, TJ1, Result);
  UCheb(x, -3.878301E-04, TJ, TJ1, Result);
  UCheb(x, -1.302233E-04, TJ, TJ1, Result);
  UCheb(x, 1.054113E-05, TJ, TJ1, Result);
  UCheb(x, 2.458862E-05, TJ, TJ1, Result);
  UCheb(x, -4.186591E-06, TJ, TJ1, Result);
  UCheb(x, -2.623412E-05, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 6, 6)
  ************************************************************************ *)
function UTblN6N6(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 2.882307E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.054075E+00, TJ, TJ1, Result);
  UCheb(x, -2.998804E+00, TJ, TJ1, Result);
  UCheb(x, -6.681518E-01, TJ, TJ1, Result);
  UCheb(x, -1.067578E-01, TJ, TJ1, Result);
  UCheb(x, -1.709435E-02, TJ, TJ1, Result);
  UCheb(x, 9.952661E-04, TJ, TJ1, Result);
  UCheb(x, 3.641700E-03, TJ, TJ1, Result);
  UCheb(x, 2.304572E-03, TJ, TJ1, Result);
  UCheb(x, 3.336275E-03, TJ, TJ1, Result);
  UCheb(x, 4.770385E-03, TJ, TJ1, Result);
  UCheb(x, 5.401891E-03, TJ, TJ1, Result);
  UCheb(x, 2.246148E-03, TJ, TJ1, Result);
  UCheb(x, -1.442663E-03, TJ, TJ1, Result);
  UCheb(x, -2.502866E-03, TJ, TJ1, Result);
  UCheb(x, -2.105855E-03, TJ, TJ1, Result);
  UCheb(x, -4.739371E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 6, 7)
  ************************************************************************ *)
function UTblN6N7(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.000000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.265287E+00, TJ, TJ1, Result);
  UCheb(x, -3.274613E+00, TJ, TJ1, Result);
  UCheb(x, -7.582352E-01, TJ, TJ1, Result);
  UCheb(x, -1.334293E-01, TJ, TJ1, Result);
  UCheb(x, -2.915502E-02, TJ, TJ1, Result);
  UCheb(x, -4.108091E-03, TJ, TJ1, Result);
  UCheb(x, 1.546701E-03, TJ, TJ1, Result);
  UCheb(x, 2.298827E-03, TJ, TJ1, Result);
  UCheb(x, 2.891501E-03, TJ, TJ1, Result);
  UCheb(x, 4.313717E-03, TJ, TJ1, Result);
  UCheb(x, 4.989501E-03, TJ, TJ1, Result);
  UCheb(x, 3.914594E-03, TJ, TJ1, Result);
  UCheb(x, 1.062372E-03, TJ, TJ1, Result);
  UCheb(x, -1.158841E-03, TJ, TJ1, Result);
  UCheb(x, -1.596443E-03, TJ, TJ1, Result);
  UCheb(x, -1.185662E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 6, 8)
  ************************************************************************ *)
function UTblN6N8(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.098387E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.450954E+00, TJ, TJ1, Result);
  UCheb(x, -3.520462E+00, TJ, TJ1, Result);
  UCheb(x, -8.420299E-01, TJ, TJ1, Result);
  UCheb(x, -1.604853E-01, TJ, TJ1, Result);
  UCheb(x, -4.165840E-02, TJ, TJ1, Result);
  UCheb(x, -1.008756E-02, TJ, TJ1, Result);
  UCheb(x, -6.723402E-04, TJ, TJ1, Result);
  UCheb(x, 1.843521E-03, TJ, TJ1, Result);
  UCheb(x, 2.883405E-03, TJ, TJ1, Result);
  UCheb(x, 3.720980E-03, TJ, TJ1, Result);
  UCheb(x, 4.301709E-03, TJ, TJ1, Result);
  UCheb(x, 3.948034E-03, TJ, TJ1, Result);
  UCheb(x, 2.776243E-03, TJ, TJ1, Result);
  UCheb(x, 8.623736E-04, TJ, TJ1, Result);
  UCheb(x, -3.742068E-04, TJ, TJ1, Result);
  UCheb(x, -9.796927E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 6, 9)
  ************************************************************************ *)
function UTblN6N9(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.181981E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.616113E+00, TJ, TJ1, Result);
  UCheb(x, -3.741650E+00, TJ, TJ1, Result);
  UCheb(x, -9.204487E-01, TJ, TJ1, Result);
  UCheb(x, -1.873068E-01, TJ, TJ1, Result);
  UCheb(x, -5.446794E-02, TJ, TJ1, Result);
  UCheb(x, -1.632286E-02, TJ, TJ1, Result);
  UCheb(x, -3.266481E-03, TJ, TJ1, Result);
  UCheb(x, 1.280067E-03, TJ, TJ1, Result);
  UCheb(x, 2.780687E-03, TJ, TJ1, Result);
  UCheb(x, 3.480242E-03, TJ, TJ1, Result);
  UCheb(x, 3.592200E-03, TJ, TJ1, Result);
  UCheb(x, 3.581019E-03, TJ, TJ1, Result);
  UCheb(x, 3.264231E-03, TJ, TJ1, Result);
  UCheb(x, 2.347174E-03, TJ, TJ1, Result);
  UCheb(x, 1.167535E-03, TJ, TJ1, Result);
  UCheb(x, -1.092185E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 6, 10)
  ************************************************************************ *)
function UTblN6N10(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.253957E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.764382E+00, TJ, TJ1, Result);
  UCheb(x, -3.942366E+00, TJ, TJ1, Result);
  UCheb(x, -9.939896E-01, TJ, TJ1, Result);
  UCheb(x, -2.137812E-01, TJ, TJ1, Result);
  UCheb(x, -6.720270E-02, TJ, TJ1, Result);
  UCheb(x, -2.281070E-02, TJ, TJ1, Result);
  UCheb(x, -5.901060E-03, TJ, TJ1, Result);
  UCheb(x, 3.824937E-04, TJ, TJ1, Result);
  UCheb(x, 2.802812E-03, TJ, TJ1, Result);
  UCheb(x, 3.258132E-03, TJ, TJ1, Result);
  UCheb(x, 3.233536E-03, TJ, TJ1, Result);
  UCheb(x, 3.085530E-03, TJ, TJ1, Result);
  UCheb(x, 3.212151E-03, TJ, TJ1, Result);
  UCheb(x, 3.001329E-03, TJ, TJ1, Result);
  UCheb(x, 2.226048E-03, TJ, TJ1, Result);
  UCheb(x, 1.035298E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 6, 11)
  ************************************************************************ *)
function UTblN6N11(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.316625E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.898597E+00, TJ, TJ1, Result);
  UCheb(x, -4.125710E+00, TJ, TJ1, Result);
  UCheb(x, -1.063297E+00, TJ, TJ1, Result);
  UCheb(x, -2.396852E-01, TJ, TJ1, Result);
  UCheb(x, -7.990126E-02, TJ, TJ1, Result);
  UCheb(x, -2.927977E-02, TJ, TJ1, Result);
  UCheb(x, -8.726500E-03, TJ, TJ1, Result);
  UCheb(x, -5.858745E-04, TJ, TJ1, Result);
  UCheb(x, 2.654590E-03, TJ, TJ1, Result);
  UCheb(x, 3.217736E-03, TJ, TJ1, Result);
  UCheb(x, 2.989770E-03, TJ, TJ1, Result);
  UCheb(x, 2.768493E-03, TJ, TJ1, Result);
  UCheb(x, 2.924364E-03, TJ, TJ1, Result);
  UCheb(x, 3.140215E-03, TJ, TJ1, Result);
  UCheb(x, 2.647914E-03, TJ, TJ1, Result);
  UCheb(x, 1.924802E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 6, 12)
  ************************************************************************ *)
function UTblN6N12(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.371709E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.020941E+00, TJ, TJ1, Result);
  UCheb(x, -4.294250E+00, TJ, TJ1, Result);
  UCheb(x, -1.128842E+00, TJ, TJ1, Result);
  UCheb(x, -2.650389E-01, TJ, TJ1, Result);
  UCheb(x, -9.248611E-02, TJ, TJ1, Result);
  UCheb(x, -3.578510E-02, TJ, TJ1, Result);
  UCheb(x, -1.162852E-02, TJ, TJ1, Result);
  UCheb(x, -1.746982E-03, TJ, TJ1, Result);
  UCheb(x, 2.454209E-03, TJ, TJ1, Result);
  UCheb(x, 3.128042E-03, TJ, TJ1, Result);
  UCheb(x, 2.936650E-03, TJ, TJ1, Result);
  UCheb(x, 2.530794E-03, TJ, TJ1, Result);
  UCheb(x, 2.665192E-03, TJ, TJ1, Result);
  UCheb(x, 2.994144E-03, TJ, TJ1, Result);
  UCheb(x, 2.662249E-03, TJ, TJ1, Result);
  UCheb(x, 2.368541E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 6, 13)
  ************************************************************************ *)
function UTblN6N13(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.420526E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.133167E+00, TJ, TJ1, Result);
  UCheb(x, -4.450016E+00, TJ, TJ1, Result);
  UCheb(x, -1.191088E+00, TJ, TJ1, Result);
  UCheb(x, -2.898220E-01, TJ, TJ1, Result);
  UCheb(x, -1.050249E-01, TJ, TJ1, Result);
  UCheb(x, -4.226901E-02, TJ, TJ1, Result);
  UCheb(x, -1.471113E-02, TJ, TJ1, Result);
  UCheb(x, -3.007470E-03, TJ, TJ1, Result);
  UCheb(x, 2.049420E-03, TJ, TJ1, Result);
  UCheb(x, 3.059074E-03, TJ, TJ1, Result);
  UCheb(x, 2.881249E-03, TJ, TJ1, Result);
  UCheb(x, 2.452780E-03, TJ, TJ1, Result);
  UCheb(x, 2.441805E-03, TJ, TJ1, Result);
  UCheb(x, 2.787493E-03, TJ, TJ1, Result);
  UCheb(x, 2.483957E-03, TJ, TJ1, Result);
  UCheb(x, 2.481590E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 6, 14)
  ************************************************************************ *)
function UTblN6N14(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.450000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.201268E+00, TJ, TJ1, Result);
  UCheb(x, -4.542568E+00, TJ, TJ1, Result);
  UCheb(x, -1.226965E+00, TJ, TJ1, Result);
  UCheb(x, -3.046029E-01, TJ, TJ1, Result);
  UCheb(x, -1.136657E-01, TJ, TJ1, Result);
  UCheb(x, -4.786757E-02, TJ, TJ1, Result);
  UCheb(x, -1.843748E-02, TJ, TJ1, Result);
  UCheb(x, -5.588022E-03, TJ, TJ1, Result);
  UCheb(x, 2.253029E-04, TJ, TJ1, Result);
  UCheb(x, 1.667188E-03, TJ, TJ1, Result);
  UCheb(x, 1.788330E-03, TJ, TJ1, Result);
  UCheb(x, 1.474545E-03, TJ, TJ1, Result);
  UCheb(x, 1.540494E-03, TJ, TJ1, Result);
  UCheb(x, 1.951188E-03, TJ, TJ1, Result);
  UCheb(x, 1.863323E-03, TJ, TJ1, Result);
  UCheb(x, 2.220904E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 6, 15)
  ************************************************************************ *)
function UTblN6N15(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.450000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.195689E+00, TJ, TJ1, Result);
  UCheb(x, -4.526567E+00, TJ, TJ1, Result);
  UCheb(x, -1.213617E+00, TJ, TJ1, Result);
  UCheb(x, -2.975035E-01, TJ, TJ1, Result);
  UCheb(x, -1.118480E-01, TJ, TJ1, Result);
  UCheb(x, -4.859142E-02, TJ, TJ1, Result);
  UCheb(x, -2.083312E-02, TJ, TJ1, Result);
  UCheb(x, -8.298720E-03, TJ, TJ1, Result);
  UCheb(x, -2.766708E-03, TJ, TJ1, Result);
  UCheb(x, -1.026356E-03, TJ, TJ1, Result);
  UCheb(x, -9.093113E-04, TJ, TJ1, Result);
  UCheb(x, -1.135168E-03, TJ, TJ1, Result);
  UCheb(x, -1.136376E-03, TJ, TJ1, Result);
  UCheb(x, -8.190870E-04, TJ, TJ1, Result);
  UCheb(x, -4.435972E-04, TJ, TJ1, Result);
  UCheb(x, 1.413129E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 6, 30)
  ************************************************************************ *)
function UTblN6N30(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.450000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.166269E+00, TJ, TJ1, Result);
  UCheb(x, -4.427399E+00, TJ, TJ1, Result);
  UCheb(x, -1.118239E+00, TJ, TJ1, Result);
  UCheb(x, -2.360847E-01, TJ, TJ1, Result);
  UCheb(x, -7.745885E-02, TJ, TJ1, Result);
  UCheb(x, -3.025041E-02, TJ, TJ1, Result);
  UCheb(x, -1.187179E-02, TJ, TJ1, Result);
  UCheb(x, -4.432089E-03, TJ, TJ1, Result);
  UCheb(x, -1.408451E-03, TJ, TJ1, Result);
  UCheb(x, -4.388774E-04, TJ, TJ1, Result);
  UCheb(x, -2.795560E-04, TJ, TJ1, Result);
  UCheb(x, -2.304136E-04, TJ, TJ1, Result);
  UCheb(x, -1.258516E-04, TJ, TJ1, Result);
  UCheb(x, -4.180236E-05, TJ, TJ1, Result);
  UCheb(x, -4.388679E-06, TJ, TJ1, Result);
  UCheb(x, 4.836027E-06, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 6, 100)
  ************************************************************************ *)
function UTblN6N100(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.450000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.181350E+00, TJ, TJ1, Result);
  UCheb(x, -4.417919E+00, TJ, TJ1, Result);
  UCheb(x, -1.094201E+00, TJ, TJ1, Result);
  UCheb(x, -2.195883E-01, TJ, TJ1, Result);
  UCheb(x, -6.818937E-02, TJ, TJ1, Result);
  UCheb(x, -2.514202E-02, TJ, TJ1, Result);
  UCheb(x, -9.125047E-03, TJ, TJ1, Result);
  UCheb(x, -3.022148E-03, TJ, TJ1, Result);
  UCheb(x, -7.284181E-04, TJ, TJ1, Result);
  UCheb(x, -1.157766E-04, TJ, TJ1, Result);
  UCheb(x, -1.023752E-04, TJ, TJ1, Result);
  UCheb(x, -1.127985E-04, TJ, TJ1, Result);
  UCheb(x, -5.221690E-05, TJ, TJ1, Result);
  UCheb(x, -3.516179E-06, TJ, TJ1, Result);
  UCheb(x, 9.501398E-06, TJ, TJ1, Result);
  UCheb(x, 9.380220E-06, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 7, 7)
  ************************************************************************ *)
function UTblN7N7(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.130495E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.501264E+00, TJ, TJ1, Result);
  UCheb(x, -3.584790E+00, TJ, TJ1, Result);
  UCheb(x, -8.577311E-01, TJ, TJ1, Result);
  UCheb(x, -1.617002E-01, TJ, TJ1, Result);
  UCheb(x, -4.145186E-02, TJ, TJ1, Result);
  UCheb(x, -1.023462E-02, TJ, TJ1, Result);
  UCheb(x, -1.408251E-03, TJ, TJ1, Result);
  UCheb(x, 8.626515E-04, TJ, TJ1, Result);
  UCheb(x, 2.072492E-03, TJ, TJ1, Result);
  UCheb(x, 3.722926E-03, TJ, TJ1, Result);
  UCheb(x, 5.095445E-03, TJ, TJ1, Result);
  UCheb(x, 4.842602E-03, TJ, TJ1, Result);
  UCheb(x, 2.751427E-03, TJ, TJ1, Result);
  UCheb(x, 2.008927E-04, TJ, TJ1, Result);
  UCheb(x, -9.892431E-04, TJ, TJ1, Result);
  UCheb(x, -8.772386E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 7, 8)
  ************************************************************************ *)
function UTblN7N8(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.240370E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.709965E+00, TJ, TJ1, Result);
  UCheb(x, -3.862154E+00, TJ, TJ1, Result);
  UCheb(x, -9.504541E-01, TJ, TJ1, Result);
  UCheb(x, -1.900195E-01, TJ, TJ1, Result);
  UCheb(x, -5.439995E-02, TJ, TJ1, Result);
  UCheb(x, -1.678028E-02, TJ, TJ1, Result);
  UCheb(x, -4.485540E-03, TJ, TJ1, Result);
  UCheb(x, -4.437047E-04, TJ, TJ1, Result);
  UCheb(x, 1.440092E-03, TJ, TJ1, Result);
  UCheb(x, 3.114227E-03, TJ, TJ1, Result);
  UCheb(x, 4.516569E-03, TJ, TJ1, Result);
  UCheb(x, 4.829457E-03, TJ, TJ1, Result);
  UCheb(x, 3.787550E-03, TJ, TJ1, Result);
  UCheb(x, 1.761866E-03, TJ, TJ1, Result);
  UCheb(x, 1.991911E-04, TJ, TJ1, Result);
  UCheb(x, -4.533481E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 7, 9)
  ************************************************************************ *)
function UTblN7N9(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.334314E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.896550E+00, TJ, TJ1, Result);
  UCheb(x, -4.112671E+00, TJ, TJ1, Result);
  UCheb(x, -1.037277E+00, TJ, TJ1, Result);
  UCheb(x, -2.181695E-01, TJ, TJ1, Result);
  UCheb(x, -6.765190E-02, TJ, TJ1, Result);
  UCheb(x, -2.360116E-02, TJ, TJ1, Result);
  UCheb(x, -7.695960E-03, TJ, TJ1, Result);
  UCheb(x, -1.780578E-03, TJ, TJ1, Result);
  UCheb(x, 8.963843E-04, TJ, TJ1, Result);
  UCheb(x, 2.616148E-03, TJ, TJ1, Result);
  UCheb(x, 3.852104E-03, TJ, TJ1, Result);
  UCheb(x, 4.390744E-03, TJ, TJ1, Result);
  UCheb(x, 4.014041E-03, TJ, TJ1, Result);
  UCheb(x, 2.888101E-03, TJ, TJ1, Result);
  UCheb(x, 1.467474E-03, TJ, TJ1, Result);
  UCheb(x, 4.004611E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 7, 10)
  ************************************************************************ *)
function UTblN7N10(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.415650E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.064844E+00, TJ, TJ1, Result);
  UCheb(x, -4.340749E+00, TJ, TJ1, Result);
  UCheb(x, -1.118888E+00, TJ, TJ1, Result);
  UCheb(x, -2.459730E-01, TJ, TJ1, Result);
  UCheb(x, -8.097781E-02, TJ, TJ1, Result);
  UCheb(x, -3.057688E-02, TJ, TJ1, Result);
  UCheb(x, -1.097406E-02, TJ, TJ1, Result);
  UCheb(x, -3.209262E-03, TJ, TJ1, Result);
  UCheb(x, 4.065641E-04, TJ, TJ1, Result);
  UCheb(x, 2.196677E-03, TJ, TJ1, Result);
  UCheb(x, 3.313994E-03, TJ, TJ1, Result);
  UCheb(x, 3.827157E-03, TJ, TJ1, Result);
  UCheb(x, 3.822284E-03, TJ, TJ1, Result);
  UCheb(x, 3.389090E-03, TJ, TJ1, Result);
  UCheb(x, 2.340850E-03, TJ, TJ1, Result);
  UCheb(x, 1.395172E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 7, 11)
  ************************************************************************ *)
function UTblN7N11(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.486817E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.217795E+00, TJ, TJ1, Result);
  UCheb(x, -4.549783E+00, TJ, TJ1, Result);
  UCheb(x, -1.195905E+00, TJ, TJ1, Result);
  UCheb(x, -2.733093E-01, TJ, TJ1, Result);
  UCheb(x, -9.428447E-02, TJ, TJ1, Result);
  UCheb(x, -3.760093E-02, TJ, TJ1, Result);
  UCheb(x, -1.431676E-02, TJ, TJ1, Result);
  UCheb(x, -4.717152E-03, TJ, TJ1, Result);
  UCheb(x, -1.032199E-04, TJ, TJ1, Result);
  UCheb(x, 1.832423E-03, TJ, TJ1, Result);
  UCheb(x, 2.905979E-03, TJ, TJ1, Result);
  UCheb(x, 3.302799E-03, TJ, TJ1, Result);
  UCheb(x, 3.464371E-03, TJ, TJ1, Result);
  UCheb(x, 3.456211E-03, TJ, TJ1, Result);
  UCheb(x, 2.736244E-03, TJ, TJ1, Result);
  UCheb(x, 2.140712E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 7, 12)
  ************************************************************************ *)
function UTblN7N12(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.500000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.235822E+00, TJ, TJ1, Result);
  UCheb(x, -4.564100E+00, TJ, TJ1, Result);
  UCheb(x, -1.190813E+00, TJ, TJ1, Result);
  UCheb(x, -2.686546E-01, TJ, TJ1, Result);
  UCheb(x, -9.395083E-02, TJ, TJ1, Result);
  UCheb(x, -3.967359E-02, TJ, TJ1, Result);
  UCheb(x, -1.747096E-02, TJ, TJ1, Result);
  UCheb(x, -8.304144E-03, TJ, TJ1, Result);
  UCheb(x, -3.903198E-03, TJ, TJ1, Result);
  UCheb(x, -2.134906E-03, TJ, TJ1, Result);
  UCheb(x, -1.175035E-03, TJ, TJ1, Result);
  UCheb(x, -7.266224E-04, TJ, TJ1, Result);
  UCheb(x, -1.892931E-04, TJ, TJ1, Result);
  UCheb(x, 5.604706E-04, TJ, TJ1, Result);
  UCheb(x, 9.070459E-04, TJ, TJ1, Result);
  UCheb(x, 1.427010E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 7, 13)
  ************************************************************************ *)
function UTblN7N13(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.500000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.222204E+00, TJ, TJ1, Result);
  UCheb(x, -4.532300E+00, TJ, TJ1, Result);
  UCheb(x, -1.164642E+00, TJ, TJ1, Result);
  UCheb(x, -2.523768E-01, TJ, TJ1, Result);
  UCheb(x, -8.531984E-02, TJ, TJ1, Result);
  UCheb(x, -3.467857E-02, TJ, TJ1, Result);
  UCheb(x, -1.483804E-02, TJ, TJ1, Result);
  UCheb(x, -6.524136E-03, TJ, TJ1, Result);
  UCheb(x, -3.077740E-03, TJ, TJ1, Result);
  UCheb(x, -1.745218E-03, TJ, TJ1, Result);
  UCheb(x, -1.602085E-03, TJ, TJ1, Result);
  UCheb(x, -1.828831E-03, TJ, TJ1, Result);
  UCheb(x, -1.994070E-03, TJ, TJ1, Result);
  UCheb(x, -1.873879E-03, TJ, TJ1, Result);
  UCheb(x, -1.341937E-03, TJ, TJ1, Result);
  UCheb(x, -8.706444E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 7, 14)
  ************************************************************************ *)
function UTblN7N14(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.500000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.211763E+00, TJ, TJ1, Result);
  UCheb(x, -4.507542E+00, TJ, TJ1, Result);
  UCheb(x, -1.143640E+00, TJ, TJ1, Result);
  UCheb(x, -2.395755E-01, TJ, TJ1, Result);
  UCheb(x, -7.808020E-02, TJ, TJ1, Result);
  UCheb(x, -3.044259E-02, TJ, TJ1, Result);
  UCheb(x, -1.182308E-02, TJ, TJ1, Result);
  UCheb(x, -4.057325E-03, TJ, TJ1, Result);
  UCheb(x, -5.724255E-04, TJ, TJ1, Result);
  UCheb(x, 8.303900E-04, TJ, TJ1, Result);
  UCheb(x, 1.113148E-03, TJ, TJ1, Result);
  UCheb(x, 8.102514E-04, TJ, TJ1, Result);
  UCheb(x, 3.559442E-04, TJ, TJ1, Result);
  UCheb(x, 4.634986E-05, TJ, TJ1, Result);
  UCheb(x, -8.776476E-05, TJ, TJ1, Result);
  UCheb(x, 1.054489E-05, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 7, 15)
  ************************************************************************ *)
function UTblN7N15(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.500000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.204898E+00, TJ, TJ1, Result);
  UCheb(x, -4.489960E+00, TJ, TJ1, Result);
  UCheb(x, -1.129172E+00, TJ, TJ1, Result);
  UCheb(x, -2.316741E-01, TJ, TJ1, Result);
  UCheb(x, -7.506107E-02, TJ, TJ1, Result);
  UCheb(x, -2.983676E-02, TJ, TJ1, Result);
  UCheb(x, -1.258013E-02, TJ, TJ1, Result);
  UCheb(x, -5.262515E-03, TJ, TJ1, Result);
  UCheb(x, -1.984156E-03, TJ, TJ1, Result);
  UCheb(x, -3.912108E-04, TJ, TJ1, Result);
  UCheb(x, 8.974023E-05, TJ, TJ1, Result);
  UCheb(x, 6.056195E-05, TJ, TJ1, Result);
  UCheb(x, -2.090842E-04, TJ, TJ1, Result);
  UCheb(x, -5.232620E-04, TJ, TJ1, Result);
  UCheb(x, -5.816339E-04, TJ, TJ1, Result);
  UCheb(x, -7.020421E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 7, 30)
  ************************************************************************ *)
function UTblN7N30(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.500000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.176536E+00, TJ, TJ1, Result);
  UCheb(x, -4.398705E+00, TJ, TJ1, Result);
  UCheb(x, -1.045481E+00, TJ, TJ1, Result);
  UCheb(x, -1.821982E-01, TJ, TJ1, Result);
  UCheb(x, -4.962304E-02, TJ, TJ1, Result);
  UCheb(x, -1.698132E-02, TJ, TJ1, Result);
  UCheb(x, -6.062667E-03, TJ, TJ1, Result);
  UCheb(x, -2.282353E-03, TJ, TJ1, Result);
  UCheb(x, -8.014836E-04, TJ, TJ1, Result);
  UCheb(x, -2.035683E-04, TJ, TJ1, Result);
  UCheb(x, -1.004137E-05, TJ, TJ1, Result);
  UCheb(x, 3.801453E-06, TJ, TJ1, Result);
  UCheb(x, -1.920705E-05, TJ, TJ1, Result);
  UCheb(x, -2.518735E-05, TJ, TJ1, Result);
  UCheb(x, -1.821501E-05, TJ, TJ1, Result);
  UCheb(x, -1.801008E-05, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 7, 100)
  ************************************************************************ *)
function UTblN7N100(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.500000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.188337E+00, TJ, TJ1, Result);
  UCheb(x, -4.386949E+00, TJ, TJ1, Result);
  UCheb(x, -1.022834E+00, TJ, TJ1, Result);
  UCheb(x, -1.686517E-01, TJ, TJ1, Result);
  UCheb(x, -4.323516E-02, TJ, TJ1, Result);
  UCheb(x, -1.399392E-02, TJ, TJ1, Result);
  UCheb(x, -4.644333E-03, TJ, TJ1, Result);
  UCheb(x, -1.617044E-03, TJ, TJ1, Result);
  UCheb(x, -5.031396E-04, TJ, TJ1, Result);
  UCheb(x, -8.792066E-05, TJ, TJ1, Result);
  UCheb(x, 2.675457E-05, TJ, TJ1, Result);
  UCheb(x, 1.673416E-05, TJ, TJ1, Result);
  UCheb(x, -6.258552E-06, TJ, TJ1, Result);
  UCheb(x, -8.174214E-06, TJ, TJ1, Result);
  UCheb(x, -3.073644E-06, TJ, TJ1, Result);
  UCheb(x, -1.349958E-06, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 8, 8)
  ************************************************************************ *)
function UTblN8N8(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.360672E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -3.940217E+00, TJ, TJ1, Result);
  UCheb(x, -4.168913E+00, TJ, TJ1, Result);
  UCheb(x, -1.051485E+00, TJ, TJ1, Result);
  UCheb(x, -2.195325E-01, TJ, TJ1, Result);
  UCheb(x, -6.775196E-02, TJ, TJ1, Result);
  UCheb(x, -2.385506E-02, TJ, TJ1, Result);
  UCheb(x, -8.244902E-03, TJ, TJ1, Result);
  UCheb(x, -2.525632E-03, TJ, TJ1, Result);
  UCheb(x, 2.771275E-04, TJ, TJ1, Result);
  UCheb(x, 2.332874E-03, TJ, TJ1, Result);
  UCheb(x, 4.079599E-03, TJ, TJ1, Result);
  UCheb(x, 4.882551E-03, TJ, TJ1, Result);
  UCheb(x, 4.407944E-03, TJ, TJ1, Result);
  UCheb(x, 2.769844E-03, TJ, TJ1, Result);
  UCheb(x, 1.062433E-03, TJ, TJ1, Result);
  UCheb(x, 5.872535E-05, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 8, 9)
  ************************************************************************ *)
function UTblN8N9(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.464102E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.147004E+00, TJ, TJ1, Result);
  UCheb(x, -4.446939E+00, TJ, TJ1, Result);
  UCheb(x, -1.146155E+00, TJ, TJ1, Result);
  UCheb(x, -2.488561E-01, TJ, TJ1, Result);
  UCheb(x, -8.144561E-02, TJ, TJ1, Result);
  UCheb(x, -3.116917E-02, TJ, TJ1, Result);
  UCheb(x, -1.205667E-02, TJ, TJ1, Result);
  UCheb(x, -4.515661E-03, TJ, TJ1, Result);
  UCheb(x, -7.618616E-04, TJ, TJ1, Result);
  UCheb(x, 1.599011E-03, TJ, TJ1, Result);
  UCheb(x, 3.457324E-03, TJ, TJ1, Result);
  UCheb(x, 4.482917E-03, TJ, TJ1, Result);
  UCheb(x, 4.488267E-03, TJ, TJ1, Result);
  UCheb(x, 3.469823E-03, TJ, TJ1, Result);
  UCheb(x, 1.957591E-03, TJ, TJ1, Result);
  UCheb(x, 8.058326E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 8, 10)
  ************************************************************************ *)
function UTblN8N10(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.554093E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.334282E+00, TJ, TJ1, Result);
  UCheb(x, -4.700860E+00, TJ, TJ1, Result);
  UCheb(x, -1.235253E+00, TJ, TJ1, Result);
  UCheb(x, -2.778489E-01, TJ, TJ1, Result);
  UCheb(x, -9.527324E-02, TJ, TJ1, Result);
  UCheb(x, -3.862885E-02, TJ, TJ1, Result);
  UCheb(x, -1.589781E-02, TJ, TJ1, Result);
  UCheb(x, -6.507355E-03, TJ, TJ1, Result);
  UCheb(x, -1.717526E-03, TJ, TJ1, Result);
  UCheb(x, 9.215726E-04, TJ, TJ1, Result);
  UCheb(x, 2.848696E-03, TJ, TJ1, Result);
  UCheb(x, 3.918854E-03, TJ, TJ1, Result);
  UCheb(x, 4.219614E-03, TJ, TJ1, Result);
  UCheb(x, 3.753761E-03, TJ, TJ1, Result);
  UCheb(x, 2.573688E-03, TJ, TJ1, Result);
  UCheb(x, 1.602177E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 8, 11)
  ************************************************************************ *)
function UTblN8N11(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.600000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.421882E+00, TJ, TJ1, Result);
  UCheb(x, -4.812457E+00, TJ, TJ1, Result);
  UCheb(x, -1.266153E+00, TJ, TJ1, Result);
  UCheb(x, -2.849344E-01, TJ, TJ1, Result);
  UCheb(x, -9.971527E-02, TJ, TJ1, Result);
  UCheb(x, -4.258944E-02, TJ, TJ1, Result);
  UCheb(x, -1.944820E-02, TJ, TJ1, Result);
  UCheb(x, -9.894685E-03, TJ, TJ1, Result);
  UCheb(x, -5.031836E-03, TJ, TJ1, Result);
  UCheb(x, -2.514330E-03, TJ, TJ1, Result);
  UCheb(x, -6.351660E-04, TJ, TJ1, Result);
  UCheb(x, 6.206748E-04, TJ, TJ1, Result);
  UCheb(x, 1.492600E-03, TJ, TJ1, Result);
  UCheb(x, 2.005338E-03, TJ, TJ1, Result);
  UCheb(x, 1.780099E-03, TJ, TJ1, Result);
  UCheb(x, 1.673599E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 8, 12)
  ************************************************************************ *)
function UTblN8N12(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.600000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.398211E+00, TJ, TJ1, Result);
  UCheb(x, -4.762214E+00, TJ, TJ1, Result);
  UCheb(x, -1.226296E+00, TJ, TJ1, Result);
  UCheb(x, -2.603837E-01, TJ, TJ1, Result);
  UCheb(x, -8.643223E-02, TJ, TJ1, Result);
  UCheb(x, -3.502438E-02, TJ, TJ1, Result);
  UCheb(x, -1.544574E-02, TJ, TJ1, Result);
  UCheb(x, -7.647734E-03, TJ, TJ1, Result);
  UCheb(x, -4.442259E-03, TJ, TJ1, Result);
  UCheb(x, -3.011484E-03, TJ, TJ1, Result);
  UCheb(x, -2.384758E-03, TJ, TJ1, Result);
  UCheb(x, -1.998259E-03, TJ, TJ1, Result);
  UCheb(x, -1.659985E-03, TJ, TJ1, Result);
  UCheb(x, -1.331046E-03, TJ, TJ1, Result);
  UCheb(x, -8.638478E-04, TJ, TJ1, Result);
  UCheb(x, -6.056785E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 8, 13)
  ************************************************************************ *)
function UTblN8N13(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.600000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.380670E+00, TJ, TJ1, Result);
  UCheb(x, -4.724511E+00, TJ, TJ1, Result);
  UCheb(x, -1.195851E+00, TJ, TJ1, Result);
  UCheb(x, -2.420511E-01, TJ, TJ1, Result);
  UCheb(x, -7.609928E-02, TJ, TJ1, Result);
  UCheb(x, -2.893999E-02, TJ, TJ1, Result);
  UCheb(x, -1.115919E-02, TJ, TJ1, Result);
  UCheb(x, -4.291410E-03, TJ, TJ1, Result);
  UCheb(x, -1.339664E-03, TJ, TJ1, Result);
  UCheb(x, -1.801548E-04, TJ, TJ1, Result);
  UCheb(x, 2.534710E-04, TJ, TJ1, Result);
  UCheb(x, 2.793250E-04, TJ, TJ1, Result);
  UCheb(x, 1.806718E-04, TJ, TJ1, Result);
  UCheb(x, 1.384624E-04, TJ, TJ1, Result);
  UCheb(x, 1.120582E-04, TJ, TJ1, Result);
  UCheb(x, 2.936453E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 8, 14)
  ************************************************************************ *)
function UTblN8N14(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.600000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.368494E+00, TJ, TJ1, Result);
  UCheb(x, -4.697171E+00, TJ, TJ1, Result);
  UCheb(x, -1.174440E+00, TJ, TJ1, Result);
  UCheb(x, -2.300621E-01, TJ, TJ1, Result);
  UCheb(x, -7.087393E-02, TJ, TJ1, Result);
  UCheb(x, -2.685826E-02, TJ, TJ1, Result);
  UCheb(x, -1.085254E-02, TJ, TJ1, Result);
  UCheb(x, -4.525658E-03, TJ, TJ1, Result);
  UCheb(x, -1.966647E-03, TJ, TJ1, Result);
  UCheb(x, -7.453388E-04, TJ, TJ1, Result);
  UCheb(x, -3.826066E-04, TJ, TJ1, Result);
  UCheb(x, -3.501958E-04, TJ, TJ1, Result);
  UCheb(x, -5.336297E-04, TJ, TJ1, Result);
  UCheb(x, -8.251972E-04, TJ, TJ1, Result);
  UCheb(x, -8.118456E-04, TJ, TJ1, Result);
  UCheb(x, -9.415959E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 8, 15)
  ************************************************************************ *)
function UTblN8N15(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.600000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.358397E+00, TJ, TJ1, Result);
  UCheb(x, -4.674485E+00, TJ, TJ1, Result);
  UCheb(x, -1.155941E+00, TJ, TJ1, Result);
  UCheb(x, -2.195780E-01, TJ, TJ1, Result);
  UCheb(x, -6.544830E-02, TJ, TJ1, Result);
  UCheb(x, -2.426183E-02, TJ, TJ1, Result);
  UCheb(x, -9.309902E-03, TJ, TJ1, Result);
  UCheb(x, -3.650956E-03, TJ, TJ1, Result);
  UCheb(x, -1.068874E-03, TJ, TJ1, Result);
  UCheb(x, 1.538544E-04, TJ, TJ1, Result);
  UCheb(x, 8.192525E-04, TJ, TJ1, Result);
  UCheb(x, 1.073905E-03, TJ, TJ1, Result);
  UCheb(x, 1.079673E-03, TJ, TJ1, Result);
  UCheb(x, 9.423572E-04, TJ, TJ1, Result);
  UCheb(x, 6.579647E-04, TJ, TJ1, Result);
  UCheb(x, 4.765904E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 8, 30)
  ************************************************************************ *)
function UTblN8N30(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.600000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.318823E+00, TJ, TJ1, Result);
  UCheb(x, -4.567159E+00, TJ, TJ1, Result);
  UCheb(x, -1.064864E+00, TJ, TJ1, Result);
  UCheb(x, -1.688413E-01, TJ, TJ1, Result);
  UCheb(x, -4.153712E-02, TJ, TJ1, Result);
  UCheb(x, -1.309389E-02, TJ, TJ1, Result);
  UCheb(x, -4.226861E-03, TJ, TJ1, Result);
  UCheb(x, -1.523815E-03, TJ, TJ1, Result);
  UCheb(x, -5.780987E-04, TJ, TJ1, Result);
  UCheb(x, -2.166866E-04, TJ, TJ1, Result);
  UCheb(x, -6.922431E-05, TJ, TJ1, Result);
  UCheb(x, -1.466397E-05, TJ, TJ1, Result);
  UCheb(x, -5.690036E-06, TJ, TJ1, Result);
  UCheb(x, -1.008185E-05, TJ, TJ1, Result);
  UCheb(x, -9.271903E-06, TJ, TJ1, Result);
  UCheb(x, -7.534751E-06, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 8, 100)
  ************************************************************************ *)
function UTblN8N100(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.600000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.324531E+00, TJ, TJ1, Result);
  UCheb(x, -4.547071E+00, TJ, TJ1, Result);
  UCheb(x, -1.038129E+00, TJ, TJ1, Result);
  UCheb(x, -1.541549E-01, TJ, TJ1, Result);
  UCheb(x, -3.525605E-02, TJ, TJ1, Result);
  UCheb(x, -1.044992E-02, TJ, TJ1, Result);
  UCheb(x, -3.085713E-03, TJ, TJ1, Result);
  UCheb(x, -1.017871E-03, TJ, TJ1, Result);
  UCheb(x, -3.459226E-04, TJ, TJ1, Result);
  UCheb(x, -1.092064E-04, TJ, TJ1, Result);
  UCheb(x, -2.024349E-05, TJ, TJ1, Result);
  UCheb(x, 7.366347E-06, TJ, TJ1, Result);
  UCheb(x, 6.385637E-06, TJ, TJ1, Result);
  UCheb(x, 8.321722E-08, TJ, TJ1, Result);
  UCheb(x, -1.439286E-06, TJ, TJ1, Result);
  UCheb(x, -3.058079E-07, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 9, 9)
  ************************************************************************ *)
function UTblN9N9(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.576237E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.372857E+00, TJ, TJ1, Result);
  UCheb(x, -4.750859E+00, TJ, TJ1, Result);
  UCheb(x, -1.248233E+00, TJ, TJ1, Result);
  UCheb(x, -2.792868E-01, TJ, TJ1, Result);
  UCheb(x, -9.559372E-02, TJ, TJ1, Result);
  UCheb(x, -3.894941E-02, TJ, TJ1, Result);
  UCheb(x, -1.643256E-02, TJ, TJ1, Result);
  UCheb(x, -7.091370E-03, TJ, TJ1, Result);
  UCheb(x, -2.285034E-03, TJ, TJ1, Result);
  UCheb(x, 6.112997E-04, TJ, TJ1, Result);
  UCheb(x, 2.806229E-03, TJ, TJ1, Result);
  UCheb(x, 4.150741E-03, TJ, TJ1, Result);
  UCheb(x, 4.509825E-03, TJ, TJ1, Result);
  UCheb(x, 3.891051E-03, TJ, TJ1, Result);
  UCheb(x, 2.485013E-03, TJ, TJ1, Result);
  UCheb(x, 1.343653E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 9, 10)
  ************************************************************************ *)
function UTblN9N10(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.650000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.516726E+00, TJ, TJ1, Result);
  UCheb(x, -4.939333E+00, TJ, TJ1, Result);
  UCheb(x, -1.305046E+00, TJ, TJ1, Result);
  UCheb(x, -2.935326E-01, TJ, TJ1, Result);
  UCheb(x, -1.029141E-01, TJ, TJ1, Result);
  UCheb(x, -4.420592E-02, TJ, TJ1, Result);
  UCheb(x, -2.053140E-02, TJ, TJ1, Result);
  UCheb(x, -1.065930E-02, TJ, TJ1, Result);
  UCheb(x, -5.523581E-03, TJ, TJ1, Result);
  UCheb(x, -2.544888E-03, TJ, TJ1, Result);
  UCheb(x, -1.813741E-04, TJ, TJ1, Result);
  UCheb(x, 1.510631E-03, TJ, TJ1, Result);
  UCheb(x, 2.536057E-03, TJ, TJ1, Result);
  UCheb(x, 2.833815E-03, TJ, TJ1, Result);
  UCheb(x, 2.189692E-03, TJ, TJ1, Result);
  UCheb(x, 1.615050E-03, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 9, 11)
  ************************************************************************ *)
function UTblN9N11(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.650000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.481308E+00, TJ, TJ1, Result);
  UCheb(x, -4.867483E+00, TJ, TJ1, Result);
  UCheb(x, -1.249072E+00, TJ, TJ1, Result);
  UCheb(x, -2.591790E-01, TJ, TJ1, Result);
  UCheb(x, -8.400128E-02, TJ, TJ1, Result);
  UCheb(x, -3.341992E-02, TJ, TJ1, Result);
  UCheb(x, -1.463680E-02, TJ, TJ1, Result);
  UCheb(x, -7.487211E-03, TJ, TJ1, Result);
  UCheb(x, -4.671196E-03, TJ, TJ1, Result);
  UCheb(x, -3.343472E-03, TJ, TJ1, Result);
  UCheb(x, -2.544146E-03, TJ, TJ1, Result);
  UCheb(x, -1.802335E-03, TJ, TJ1, Result);
  UCheb(x, -1.117084E-03, TJ, TJ1, Result);
  UCheb(x, -6.217443E-04, TJ, TJ1, Result);
  UCheb(x, -2.858766E-04, TJ, TJ1, Result);
  UCheb(x, -3.193687E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 9, 12)
  ************************************************************************ *)
function UTblN9N12(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.650000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.456776E+00, TJ, TJ1, Result);
  UCheb(x, -4.817037E+00, TJ, TJ1, Result);
  UCheb(x, -1.209788E+00, TJ, TJ1, Result);
  UCheb(x, -2.362108E-01, TJ, TJ1, Result);
  UCheb(x, -7.171356E-02, TJ, TJ1, Result);
  UCheb(x, -2.661557E-02, TJ, TJ1, Result);
  UCheb(x, -1.026141E-02, TJ, TJ1, Result);
  UCheb(x, -4.361908E-03, TJ, TJ1, Result);
  UCheb(x, -2.093885E-03, TJ, TJ1, Result);
  UCheb(x, -1.298389E-03, TJ, TJ1, Result);
  UCheb(x, -9.663603E-04, TJ, TJ1, Result);
  UCheb(x, -7.768522E-04, TJ, TJ1, Result);
  UCheb(x, -5.579015E-04, TJ, TJ1, Result);
  UCheb(x, -2.868677E-04, TJ, TJ1, Result);
  UCheb(x, -7.440652E-05, TJ, TJ1, Result);
  UCheb(x, 1.523037E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 9, 13)
  ************************************************************************ *)
function UTblN9N13(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.650000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.438840E+00, TJ, TJ1, Result);
  UCheb(x, -4.779308E+00, TJ, TJ1, Result);
  UCheb(x, -1.180614E+00, TJ, TJ1, Result);
  UCheb(x, -2.196489E-01, TJ, TJ1, Result);
  UCheb(x, -6.346621E-02, TJ, TJ1, Result);
  UCheb(x, -2.234857E-02, TJ, TJ1, Result);
  UCheb(x, -7.796211E-03, TJ, TJ1, Result);
  UCheb(x, -2.575715E-03, TJ, TJ1, Result);
  UCheb(x, -5.525647E-04, TJ, TJ1, Result);
  UCheb(x, 1.964651E-04, TJ, TJ1, Result);
  UCheb(x, 4.275235E-04, TJ, TJ1, Result);
  UCheb(x, 4.299124E-04, TJ, TJ1, Result);
  UCheb(x, 3.397416E-04, TJ, TJ1, Result);
  UCheb(x, 2.295781E-04, TJ, TJ1, Result);
  UCheb(x, 1.237619E-04, TJ, TJ1, Result);
  UCheb(x, 7.269692E-05, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 9, 14)
  ************************************************************************ *)
function UTblN9N14(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.650000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.425981E+00, TJ, TJ1, Result);
  UCheb(x, -4.751545E+00, TJ, TJ1, Result);
  UCheb(x, -1.159543E+00, TJ, TJ1, Result);
  UCheb(x, -2.086570E-01, TJ, TJ1, Result);
  UCheb(x, -5.917446E-02, TJ, TJ1, Result);
  UCheb(x, -2.120112E-02, TJ, TJ1, Result);
  UCheb(x, -8.175519E-03, TJ, TJ1, Result);
  UCheb(x, -3.515473E-03, TJ, TJ1, Result);
  UCheb(x, -1.727772E-03, TJ, TJ1, Result);
  UCheb(x, -9.070629E-04, TJ, TJ1, Result);
  UCheb(x, -5.677569E-04, TJ, TJ1, Result);
  UCheb(x, -3.876953E-04, TJ, TJ1, Result);
  UCheb(x, -3.233502E-04, TJ, TJ1, Result);
  UCheb(x, -3.508182E-04, TJ, TJ1, Result);
  UCheb(x, -3.120389E-04, TJ, TJ1, Result);
  UCheb(x, -3.847212E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 9, 15)
  ************************************************************************ *)
function UTblN9N15(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.650000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.414952E+00, TJ, TJ1, Result);
  UCheb(x, -4.727612E+00, TJ, TJ1, Result);
  UCheb(x, -1.140634E+00, TJ, TJ1, Result);
  UCheb(x, -1.981231E-01, TJ, TJ1, Result);
  UCheb(x, -5.382635E-02, TJ, TJ1, Result);
  UCheb(x, -1.853575E-02, TJ, TJ1, Result);
  UCheb(x, -6.571051E-03, TJ, TJ1, Result);
  UCheb(x, -2.567625E-03, TJ, TJ1, Result);
  UCheb(x, -9.214197E-04, TJ, TJ1, Result);
  UCheb(x, -2.448700E-04, TJ, TJ1, Result);
  UCheb(x, 1.712669E-04, TJ, TJ1, Result);
  UCheb(x, 4.015050E-04, TJ, TJ1, Result);
  UCheb(x, 5.438610E-04, TJ, TJ1, Result);
  UCheb(x, 6.301363E-04, TJ, TJ1, Result);
  UCheb(x, 5.309386E-04, TJ, TJ1, Result);
  UCheb(x, 5.164772E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 9, 30)
  ************************************************************************ *)
function UTblN9N30(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.650000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.370720E+00, TJ, TJ1, Result);
  UCheb(x, -4.615712E+00, TJ, TJ1, Result);
  UCheb(x, -1.050023E+00, TJ, TJ1, Result);
  UCheb(x, -1.504775E-01, TJ, TJ1, Result);
  UCheb(x, -3.318265E-02, TJ, TJ1, Result);
  UCheb(x, -9.646826E-03, TJ, TJ1, Result);
  UCheb(x, -2.741492E-03, TJ, TJ1, Result);
  UCheb(x, -8.735360E-04, TJ, TJ1, Result);
  UCheb(x, -2.966911E-04, TJ, TJ1, Result);
  UCheb(x, -1.100738E-04, TJ, TJ1, Result);
  UCheb(x, -4.348991E-05, TJ, TJ1, Result);
  UCheb(x, -1.527687E-05, TJ, TJ1, Result);
  UCheb(x, -2.917286E-06, TJ, TJ1, Result);
  UCheb(x, 3.397466E-07, TJ, TJ1, Result);
  UCheb(x, -2.360175E-07, TJ, TJ1, Result);
  UCheb(x, -9.892252E-07, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 9, 100)
  ************************************************************************ *)
function UTblN9N100(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.650000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.372506E+00, TJ, TJ1, Result);
  UCheb(x, -4.590966E+00, TJ, TJ1, Result);
  UCheb(x, -1.021758E+00, TJ, TJ1, Result);
  UCheb(x, -1.359849E-01, TJ, TJ1, Result);
  UCheb(x, -2.755519E-02, TJ, TJ1, Result);
  UCheb(x, -7.533166E-03, TJ, TJ1, Result);
  UCheb(x, -1.936659E-03, TJ, TJ1, Result);
  UCheb(x, -5.634913E-04, TJ, TJ1, Result);
  UCheb(x, -1.730053E-04, TJ, TJ1, Result);
  UCheb(x, -5.791845E-05, TJ, TJ1, Result);
  UCheb(x, -2.030682E-05, TJ, TJ1, Result);
  UCheb(x, -5.228663E-06, TJ, TJ1, Result);
  UCheb(x, 8.631175E-07, TJ, TJ1, Result);
  UCheb(x, 1.636749E-06, TJ, TJ1, Result);
  UCheb(x, 4.404599E-07, TJ, TJ1, Result);
  UCheb(x, -2.789872E-07, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 10, 10)
  ************************************************************************ *)
function UTblN10N10(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.650000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.468831E+00, TJ, TJ1, Result);
  UCheb(x, -4.844398E+00, TJ, TJ1, Result);
  UCheb(x, -1.231728E+00, TJ, TJ1, Result);
  UCheb(x, -2.486073E-01, TJ, TJ1, Result);
  UCheb(x, -7.781321E-02, TJ, TJ1, Result);
  UCheb(x, -2.971425E-02, TJ, TJ1, Result);
  UCheb(x, -1.215371E-02, TJ, TJ1, Result);
  UCheb(x, -5.828451E-03, TJ, TJ1, Result);
  UCheb(x, -3.419872E-03, TJ, TJ1, Result);
  UCheb(x, -2.430165E-03, TJ, TJ1, Result);
  UCheb(x, -1.740363E-03, TJ, TJ1, Result);
  UCheb(x, -1.049211E-03, TJ, TJ1, Result);
  UCheb(x, -3.269371E-04, TJ, TJ1, Result);
  UCheb(x, 2.211393E-04, TJ, TJ1, Result);
  UCheb(x, 4.232314E-04, TJ, TJ1, Result);
  UCheb(x, 3.016081E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 10, 11)
  ************************************************************************ *)
function UTblN10N11(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.650000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.437998E+00, TJ, TJ1, Result);
  UCheb(x, -4.782296E+00, TJ, TJ1, Result);
  UCheb(x, -1.184732E+00, TJ, TJ1, Result);
  UCheb(x, -2.219585E-01, TJ, TJ1, Result);
  UCheb(x, -6.457012E-02, TJ, TJ1, Result);
  UCheb(x, -2.296008E-02, TJ, TJ1, Result);
  UCheb(x, -8.481501E-03, TJ, TJ1, Result);
  UCheb(x, -3.527940E-03, TJ, TJ1, Result);
  UCheb(x, -1.953426E-03, TJ, TJ1, Result);
  UCheb(x, -1.563840E-03, TJ, TJ1, Result);
  UCheb(x, -1.574403E-03, TJ, TJ1, Result);
  UCheb(x, -1.535775E-03, TJ, TJ1, Result);
  UCheb(x, -1.338037E-03, TJ, TJ1, Result);
  UCheb(x, -1.002654E-03, TJ, TJ1, Result);
  UCheb(x, -5.852676E-04, TJ, TJ1, Result);
  UCheb(x, -3.318132E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 10, 12)
  ************************************************************************ *)
function UTblN10N12(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.650000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.416082E+00, TJ, TJ1, Result);
  UCheb(x, -4.737458E+00, TJ, TJ1, Result);
  UCheb(x, -1.150952E+00, TJ, TJ1, Result);
  UCheb(x, -2.036884E-01, TJ, TJ1, Result);
  UCheb(x, -5.609030E-02, TJ, TJ1, Result);
  UCheb(x, -1.908684E-02, TJ, TJ1, Result);
  UCheb(x, -6.439666E-03, TJ, TJ1, Result);
  UCheb(x, -2.162647E-03, TJ, TJ1, Result);
  UCheb(x, -6.451601E-04, TJ, TJ1, Result);
  UCheb(x, -2.148757E-04, TJ, TJ1, Result);
  UCheb(x, -1.803981E-04, TJ, TJ1, Result);
  UCheb(x, -2.731621E-04, TJ, TJ1, Result);
  UCheb(x, -3.346903E-04, TJ, TJ1, Result);
  UCheb(x, -3.013151E-04, TJ, TJ1, Result);
  UCheb(x, -1.956148E-04, TJ, TJ1, Result);
  UCheb(x, -2.438381E-05, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 10, 13)
  ************************************************************************ *)
function UTblN10N13(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.650000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.399480E+00, TJ, TJ1, Result);
  UCheb(x, -4.702863E+00, TJ, TJ1, Result);
  UCheb(x, -1.124829E+00, TJ, TJ1, Result);
  UCheb(x, -1.897428E-01, TJ, TJ1, Result);
  UCheb(x, -4.979802E-02, TJ, TJ1, Result);
  UCheb(x, -1.634368E-02, TJ, TJ1, Result);
  UCheb(x, -5.180461E-03, TJ, TJ1, Result);
  UCheb(x, -1.484926E-03, TJ, TJ1, Result);
  UCheb(x, -7.864376E-05, TJ, TJ1, Result);
  UCheb(x, 4.186576E-04, TJ, TJ1, Result);
  UCheb(x, 5.886925E-04, TJ, TJ1, Result);
  UCheb(x, 5.836828E-04, TJ, TJ1, Result);
  UCheb(x, 5.074756E-04, TJ, TJ1, Result);
  UCheb(x, 4.209547E-04, TJ, TJ1, Result);
  UCheb(x, 2.883266E-04, TJ, TJ1, Result);
  UCheb(x, 2.380143E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 10, 14)
  ************************************************************************ *)
function UTblN10N14(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.650000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.386924E+00, TJ, TJ1, Result);
  UCheb(x, -4.676124E+00, TJ, TJ1, Result);
  UCheb(x, -1.104740E+00, TJ, TJ1, Result);
  UCheb(x, -1.793826E-01, TJ, TJ1, Result);
  UCheb(x, -4.558886E-02, TJ, TJ1, Result);
  UCheb(x, -1.492462E-02, TJ, TJ1, Result);
  UCheb(x, -5.052903E-03, TJ, TJ1, Result);
  UCheb(x, -1.917782E-03, TJ, TJ1, Result);
  UCheb(x, -7.878696E-04, TJ, TJ1, Result);
  UCheb(x, -3.576046E-04, TJ, TJ1, Result);
  UCheb(x, -1.764551E-04, TJ, TJ1, Result);
  UCheb(x, -9.288778E-05, TJ, TJ1, Result);
  UCheb(x, -4.757658E-05, TJ, TJ1, Result);
  UCheb(x, -2.299101E-05, TJ, TJ1, Result);
  UCheb(x, -9.265197E-06, TJ, TJ1, Result);
  UCheb(x, -2.384503E-07, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 10, 15)
  ************************************************************************ *)
function UTblN10N15(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.650000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.376846E+00, TJ, TJ1, Result);
  UCheb(x, -4.654247E+00, TJ, TJ1, Result);
  UCheb(x, -1.088083E+00, TJ, TJ1, Result);
  UCheb(x, -1.705945E-01, TJ, TJ1, Result);
  UCheb(x, -4.169677E-02, TJ, TJ1, Result);
  UCheb(x, -1.317213E-02, TJ, TJ1, Result);
  UCheb(x, -4.264836E-03, TJ, TJ1, Result);
  UCheb(x, -1.548024E-03, TJ, TJ1, Result);
  UCheb(x, -6.633910E-04, TJ, TJ1, Result);
  UCheb(x, -3.505621E-04, TJ, TJ1, Result);
  UCheb(x, -2.658588E-04, TJ, TJ1, Result);
  UCheb(x, -2.320254E-04, TJ, TJ1, Result);
  UCheb(x, -2.175277E-04, TJ, TJ1, Result);
  UCheb(x, -2.122317E-04, TJ, TJ1, Result);
  UCheb(x, -1.675688E-04, TJ, TJ1, Result);
  UCheb(x, -1.661363E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 10, 30)
  ************************************************************************ *)
function UTblN10N30(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.650000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.333977E+00, TJ, TJ1, Result);
  UCheb(x, -4.548099E+00, TJ, TJ1, Result);
  UCheb(x, -1.004444E+00, TJ, TJ1, Result);
  UCheb(x, -1.291014E-01, TJ, TJ1, Result);
  UCheb(x, -2.523674E-02, TJ, TJ1, Result);
  UCheb(x, -6.828211E-03, TJ, TJ1, Result);
  UCheb(x, -1.716917E-03, TJ, TJ1, Result);
  UCheb(x, -4.894256E-04, TJ, TJ1, Result);
  UCheb(x, -1.433371E-04, TJ, TJ1, Result);
  UCheb(x, -4.522675E-05, TJ, TJ1, Result);
  UCheb(x, -1.764192E-05, TJ, TJ1, Result);
  UCheb(x, -9.140235E-06, TJ, TJ1, Result);
  UCheb(x, -5.629230E-06, TJ, TJ1, Result);
  UCheb(x, -3.541895E-06, TJ, TJ1, Result);
  UCheb(x, -1.944946E-06, TJ, TJ1, Result);
  UCheb(x, -1.726360E-06, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 10, 100)
  ************************************************************************ *)
function UTblN10N100(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.650000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.334008E+00, TJ, TJ1, Result);
  UCheb(x, -4.522316E+00, TJ, TJ1, Result);
  UCheb(x, -9.769627E-01, TJ, TJ1, Result);
  UCheb(x, -1.158110E-01, TJ, TJ1, Result);
  UCheb(x, -2.053650E-02, TJ, TJ1, Result);
  UCheb(x, -5.242235E-03, TJ, TJ1, Result);
  UCheb(x, -1.173571E-03, TJ, TJ1, Result);
  UCheb(x, -3.033661E-04, TJ, TJ1, Result);
  UCheb(x, -7.824732E-05, TJ, TJ1, Result);
  UCheb(x, -2.084420E-05, TJ, TJ1, Result);
  UCheb(x, -6.610036E-06, TJ, TJ1, Result);
  UCheb(x, -2.728155E-06, TJ, TJ1, Result);
  UCheb(x, -1.217130E-06, TJ, TJ1, Result);
  UCheb(x, -2.340966E-07, TJ, TJ1, Result);
  UCheb(x, 2.001235E-07, TJ, TJ1, Result);
  UCheb(x, 1.694052E-07, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 11, 11)
  ************************************************************************ *)
function UTblN11N11(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.700000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.519760E+00, TJ, TJ1, Result);
  UCheb(x, -4.880694E+00, TJ, TJ1, Result);
  UCheb(x, -1.200698E+00, TJ, TJ1, Result);
  UCheb(x, -2.174092E-01, TJ, TJ1, Result);
  UCheb(x, -6.072304E-02, TJ, TJ1, Result);
  UCheb(x, -2.054773E-02, TJ, TJ1, Result);
  UCheb(x, -6.506613E-03, TJ, TJ1, Result);
  UCheb(x, -1.813942E-03, TJ, TJ1, Result);
  UCheb(x, -1.223644E-04, TJ, TJ1, Result);
  UCheb(x, 2.417416E-04, TJ, TJ1, Result);
  UCheb(x, 2.499166E-04, TJ, TJ1, Result);
  UCheb(x, 1.194332E-04, TJ, TJ1, Result);
  UCheb(x, 7.369096E-05, TJ, TJ1, Result);
  UCheb(x, 1.968590E-04, TJ, TJ1, Result);
  UCheb(x, 2.630532E-04, TJ, TJ1, Result);
  UCheb(x, 5.061000E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 11, 12)
  ************************************************************************ *)
function UTblN11N12(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.700000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.495790E+00, TJ, TJ1, Result);
  UCheb(x, -4.832622E+00, TJ, TJ1, Result);
  UCheb(x, -1.165420E+00, TJ, TJ1, Result);
  UCheb(x, -1.987306E-01, TJ, TJ1, Result);
  UCheb(x, -5.265621E-02, TJ, TJ1, Result);
  UCheb(x, -1.723537E-02, TJ, TJ1, Result);
  UCheb(x, -5.347406E-03, TJ, TJ1, Result);
  UCheb(x, -1.353464E-03, TJ, TJ1, Result);
  UCheb(x, 6.613369E-05, TJ, TJ1, Result);
  UCheb(x, 5.102522E-04, TJ, TJ1, Result);
  UCheb(x, 5.237709E-04, TJ, TJ1, Result);
  UCheb(x, 3.665652E-04, TJ, TJ1, Result);
  UCheb(x, 1.626903E-04, TJ, TJ1, Result);
  UCheb(x, -1.167518E-05, TJ, TJ1, Result);
  UCheb(x, -8.564455E-05, TJ, TJ1, Result);
  UCheb(x, -1.047320E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 11, 13)
  ************************************************************************ *)
function UTblN11N13(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.700000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.477880E+00, TJ, TJ1, Result);
  UCheb(x, -4.796242E+00, TJ, TJ1, Result);
  UCheb(x, -1.138769E+00, TJ, TJ1, Result);
  UCheb(x, -1.851739E-01, TJ, TJ1, Result);
  UCheb(x, -4.722104E-02, TJ, TJ1, Result);
  UCheb(x, -1.548304E-02, TJ, TJ1, Result);
  UCheb(x, -5.176683E-03, TJ, TJ1, Result);
  UCheb(x, -1.817895E-03, TJ, TJ1, Result);
  UCheb(x, -5.842451E-04, TJ, TJ1, Result);
  UCheb(x, -8.935870E-05, TJ, TJ1, Result);
  UCheb(x, 8.421777E-05, TJ, TJ1, Result);
  UCheb(x, 1.238831E-04, TJ, TJ1, Result);
  UCheb(x, 8.867026E-05, TJ, TJ1, Result);
  UCheb(x, 1.458255E-05, TJ, TJ1, Result);
  UCheb(x, -3.306259E-05, TJ, TJ1, Result);
  UCheb(x, -8.961487E-05, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 11, 14)
  ************************************************************************ *)
function UTblN11N14(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.700000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.463683E+00, TJ, TJ1, Result);
  UCheb(x, -4.766969E+00, TJ, TJ1, Result);
  UCheb(x, -1.117082E+00, TJ, TJ1, Result);
  UCheb(x, -1.739574E-01, TJ, TJ1, Result);
  UCheb(x, -4.238865E-02, TJ, TJ1, Result);
  UCheb(x, -1.350306E-02, TJ, TJ1, Result);
  UCheb(x, -4.425871E-03, TJ, TJ1, Result);
  UCheb(x, -1.640172E-03, TJ, TJ1, Result);
  UCheb(x, -6.660633E-04, TJ, TJ1, Result);
  UCheb(x, -2.879883E-04, TJ, TJ1, Result);
  UCheb(x, -1.349658E-04, TJ, TJ1, Result);
  UCheb(x, -6.271795E-05, TJ, TJ1, Result);
  UCheb(x, -3.304544E-05, TJ, TJ1, Result);
  UCheb(x, -3.024201E-05, TJ, TJ1, Result);
  UCheb(x, -2.816867E-05, TJ, TJ1, Result);
  UCheb(x, -4.596787E-05, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 11, 15)
  ************************************************************************ *)
function UTblN11N15(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.700000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.452526E+00, TJ, TJ1, Result);
  UCheb(x, -4.743570E+00, TJ, TJ1, Result);
  UCheb(x, -1.099705E+00, TJ, TJ1, Result);
  UCheb(x, -1.650612E-01, TJ, TJ1, Result);
  UCheb(x, -3.858285E-02, TJ, TJ1, Result);
  UCheb(x, -1.187036E-02, TJ, TJ1, Result);
  UCheb(x, -3.689241E-03, TJ, TJ1, Result);
  UCheb(x, -1.294360E-03, TJ, TJ1, Result);
  UCheb(x, -5.072623E-04, TJ, TJ1, Result);
  UCheb(x, -2.278008E-04, TJ, TJ1, Result);
  UCheb(x, -1.322382E-04, TJ, TJ1, Result);
  UCheb(x, -9.131558E-05, TJ, TJ1, Result);
  UCheb(x, -7.305669E-05, TJ, TJ1, Result);
  UCheb(x, -6.825627E-05, TJ, TJ1, Result);
  UCheb(x, -5.332689E-05, TJ, TJ1, Result);
  UCheb(x, -6.120973E-05, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 11, 30)
  ************************************************************************ *)
function UTblN11N30(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.700000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.402621E+00, TJ, TJ1, Result);
  UCheb(x, -4.627440E+00, TJ, TJ1, Result);
  UCheb(x, -1.011333E+00, TJ, TJ1, Result);
  UCheb(x, -1.224126E-01, TJ, TJ1, Result);
  UCheb(x, -2.232856E-02, TJ, TJ1, Result);
  UCheb(x, -5.859347E-03, TJ, TJ1, Result);
  UCheb(x, -1.377381E-03, TJ, TJ1, Result);
  UCheb(x, -3.756709E-04, TJ, TJ1, Result);
  UCheb(x, -1.033230E-04, TJ, TJ1, Result);
  UCheb(x, -2.875472E-05, TJ, TJ1, Result);
  UCheb(x, -8.608399E-06, TJ, TJ1, Result);
  UCheb(x, -3.102943E-06, TJ, TJ1, Result);
  UCheb(x, -1.740693E-06, TJ, TJ1, Result);
  UCheb(x, -1.343139E-06, TJ, TJ1, Result);
  UCheb(x, -9.196878E-07, TJ, TJ1, Result);
  UCheb(x, -6.658062E-07, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 11, 100)
  ************************************************************************ *)
function UTblN11N100(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.700000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.398795E+00, TJ, TJ1, Result);
  UCheb(x, -4.596486E+00, TJ, TJ1, Result);
  UCheb(x, -9.814761E-01, TJ, TJ1, Result);
  UCheb(x, -1.085187E-01, TJ, TJ1, Result);
  UCheb(x, -1.766529E-02, TJ, TJ1, Result);
  UCheb(x, -4.379425E-03, TJ, TJ1, Result);
  UCheb(x, -8.986351E-04, TJ, TJ1, Result);
  UCheb(x, -2.214705E-04, TJ, TJ1, Result);
  UCheb(x, -5.360075E-05, TJ, TJ1, Result);
  UCheb(x, -1.260869E-05, TJ, TJ1, Result);
  UCheb(x, -3.033307E-06, TJ, TJ1, Result);
  UCheb(x, -7.727087E-07, TJ, TJ1, Result);
  UCheb(x, -3.393883E-07, TJ, TJ1, Result);
  UCheb(x, -2.242989E-07, TJ, TJ1, Result);
  UCheb(x, -1.111928E-07, TJ, TJ1, Result);
  UCheb(x, 3.898823E-09, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 12, 12)
  ************************************************************************ *)
function UTblN12N12(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.700000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.472616E+00, TJ, TJ1, Result);
  UCheb(x, -4.786627E+00, TJ, TJ1, Result);
  UCheb(x, -1.132099E+00, TJ, TJ1, Result);
  UCheb(x, -1.817523E-01, TJ, TJ1, Result);
  UCheb(x, -4.570179E-02, TJ, TJ1, Result);
  UCheb(x, -1.479511E-02, TJ, TJ1, Result);
  UCheb(x, -4.799492E-03, TJ, TJ1, Result);
  UCheb(x, -1.565350E-03, TJ, TJ1, Result);
  UCheb(x, -3.530139E-04, TJ, TJ1, Result);
  UCheb(x, 1.380132E-04, TJ, TJ1, Result);
  UCheb(x, 3.242761E-04, TJ, TJ1, Result);
  UCheb(x, 3.576269E-04, TJ, TJ1, Result);
  UCheb(x, 3.018771E-04, TJ, TJ1, Result);
  UCheb(x, 1.933911E-04, TJ, TJ1, Result);
  UCheb(x, 9.002799E-05, TJ, TJ1, Result);
  UCheb(x, -2.022048E-06, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 12, 13)
  ************************************************************************ *)
function UTblN12N13(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.700000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.454800E+00, TJ, TJ1, Result);
  UCheb(x, -4.750794E+00, TJ, TJ1, Result);
  UCheb(x, -1.105988E+00, TJ, TJ1, Result);
  UCheb(x, -1.684754E-01, TJ, TJ1, Result);
  UCheb(x, -4.011826E-02, TJ, TJ1, Result);
  UCheb(x, -1.262579E-02, TJ, TJ1, Result);
  UCheb(x, -4.044492E-03, TJ, TJ1, Result);
  UCheb(x, -1.478741E-03, TJ, TJ1, Result);
  UCheb(x, -5.322165E-04, TJ, TJ1, Result);
  UCheb(x, -1.621104E-04, TJ, TJ1, Result);
  UCheb(x, 4.068753E-05, TJ, TJ1, Result);
  UCheb(x, 1.468396E-04, TJ, TJ1, Result);
  UCheb(x, 2.056235E-04, TJ, TJ1, Result);
  UCheb(x, 2.327375E-04, TJ, TJ1, Result);
  UCheb(x, 1.914877E-04, TJ, TJ1, Result);
  UCheb(x, 1.784191E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 12, 14)
  ************************************************************************ *)
function UTblN12N14(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.700000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.440910E+00, TJ, TJ1, Result);
  UCheb(x, -4.722404E+00, TJ, TJ1, Result);
  UCheb(x, -1.085254E+00, TJ, TJ1, Result);
  UCheb(x, -1.579439E-01, TJ, TJ1, Result);
  UCheb(x, -3.563738E-02, TJ, TJ1, Result);
  UCheb(x, -1.066730E-02, TJ, TJ1, Result);
  UCheb(x, -3.129346E-03, TJ, TJ1, Result);
  UCheb(x, -1.014531E-03, TJ, TJ1, Result);
  UCheb(x, -3.129679E-04, TJ, TJ1, Result);
  UCheb(x, -8.000909E-05, TJ, TJ1, Result);
  UCheb(x, 1.996174E-05, TJ, TJ1, Result);
  UCheb(x, 6.377924E-05, TJ, TJ1, Result);
  UCheb(x, 8.936304E-05, TJ, TJ1, Result);
  UCheb(x, 1.051098E-04, TJ, TJ1, Result);
  UCheb(x, 9.025820E-05, TJ, TJ1, Result);
  UCheb(x, 8.730585E-05, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 12, 15)
  ************************************************************************ *)
function UTblN12N15(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.700000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.430123E+00, TJ, TJ1, Result);
  UCheb(x, -4.700008E+00, TJ, TJ1, Result);
  UCheb(x, -1.068971E+00, TJ, TJ1, Result);
  UCheb(x, -1.499725E-01, TJ, TJ1, Result);
  UCheb(x, -3.250897E-02, TJ, TJ1, Result);
  UCheb(x, -9.473145E-03, TJ, TJ1, Result);
  UCheb(x, -2.680008E-03, TJ, TJ1, Result);
  UCheb(x, -8.483350E-04, TJ, TJ1, Result);
  UCheb(x, -2.766992E-04, TJ, TJ1, Result);
  UCheb(x, -9.891081E-05, TJ, TJ1, Result);
  UCheb(x, -4.015140E-05, TJ, TJ1, Result);
  UCheb(x, -1.977756E-05, TJ, TJ1, Result);
  UCheb(x, -8.707414E-06, TJ, TJ1, Result);
  UCheb(x, 1.114786E-06, TJ, TJ1, Result);
  UCheb(x, 6.238865E-06, TJ, TJ1, Result);
  UCheb(x, 1.381445E-05, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 12, 30)
  ************************************************************************ *)
function UTblN12N30(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.700000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.380023E+00, TJ, TJ1, Result);
  UCheb(x, -4.585782E+00, TJ, TJ1, Result);
  UCheb(x, -9.838583E-01, TJ, TJ1, Result);
  UCheb(x, -1.103394E-01, TJ, TJ1, Result);
  UCheb(x, -1.834015E-02, TJ, TJ1, Result);
  UCheb(x, -4.635212E-03, TJ, TJ1, Result);
  UCheb(x, -9.948212E-04, TJ, TJ1, Result);
  UCheb(x, -2.574169E-04, TJ, TJ1, Result);
  UCheb(x, -6.747980E-05, TJ, TJ1, Result);
  UCheb(x, -1.833672E-05, TJ, TJ1, Result);
  UCheb(x, -5.722433E-06, TJ, TJ1, Result);
  UCheb(x, -2.181038E-06, TJ, TJ1, Result);
  UCheb(x, -1.206473E-06, TJ, TJ1, Result);
  UCheb(x, -9.716003E-07, TJ, TJ1, Result);
  UCheb(x, -7.476434E-07, TJ, TJ1, Result);
  UCheb(x, -7.217700E-07, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 12, 100)
  ************************************************************************ *)
function UTblN12N100(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.700000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.374567E+00, TJ, TJ1, Result);
  UCheb(x, -4.553481E+00, TJ, TJ1, Result);
  UCheb(x, -9.541334E-01, TJ, TJ1, Result);
  UCheb(x, -9.701907E-02, TJ, TJ1, Result);
  UCheb(x, -1.414757E-02, TJ, TJ1, Result);
  UCheb(x, -3.404103E-03, TJ, TJ1, Result);
  UCheb(x, -6.234388E-04, TJ, TJ1, Result);
  UCheb(x, -1.453762E-04, TJ, TJ1, Result);
  UCheb(x, -3.311060E-05, TJ, TJ1, Result);
  UCheb(x, -7.317501E-06, TJ, TJ1, Result);
  UCheb(x, -1.713888E-06, TJ, TJ1, Result);
  UCheb(x, -3.309583E-07, TJ, TJ1, Result);
  UCheb(x, -4.019804E-08, TJ, TJ1, Result);
  UCheb(x, 1.224829E-09, TJ, TJ1, Result);
  UCheb(x, -1.349019E-08, TJ, TJ1, Result);
  UCheb(x, -1.893302E-08, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 13, 13)
  ************************************************************************ *)
function UTblN13N13(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.750000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.541046E+00, TJ, TJ1, Result);
  UCheb(x, -4.859047E+00, TJ, TJ1, Result);
  UCheb(x, -1.130164E+00, TJ, TJ1, Result);
  UCheb(x, -1.689719E-01, TJ, TJ1, Result);
  UCheb(x, -3.950693E-02, TJ, TJ1, Result);
  UCheb(x, -1.231455E-02, TJ, TJ1, Result);
  UCheb(x, -3.976550E-03, TJ, TJ1, Result);
  UCheb(x, -1.538455E-03, TJ, TJ1, Result);
  UCheb(x, -7.245603E-04, TJ, TJ1, Result);
  UCheb(x, -4.142647E-04, TJ, TJ1, Result);
  UCheb(x, -2.831434E-04, TJ, TJ1, Result);
  UCheb(x, -2.032483E-04, TJ, TJ1, Result);
  UCheb(x, -1.488405E-04, TJ, TJ1, Result);
  UCheb(x, -1.156927E-04, TJ, TJ1, Result);
  UCheb(x, -7.949279E-05, TJ, TJ1, Result);
  UCheb(x, -7.532700E-05, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 13, 14)
  ************************************************************************ *)
function UTblN13N14(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.750000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.525655E+00, TJ, TJ1, Result);
  UCheb(x, -4.828341E+00, TJ, TJ1, Result);
  UCheb(x, -1.108110E+00, TJ, TJ1, Result);
  UCheb(x, -1.579552E-01, TJ, TJ1, Result);
  UCheb(x, -3.488307E-02, TJ, TJ1, Result);
  UCheb(x, -1.032328E-02, TJ, TJ1, Result);
  UCheb(x, -2.988741E-03, TJ, TJ1, Result);
  UCheb(x, -9.766394E-04, TJ, TJ1, Result);
  UCheb(x, -3.388950E-04, TJ, TJ1, Result);
  UCheb(x, -1.338179E-04, TJ, TJ1, Result);
  UCheb(x, -6.133440E-05, TJ, TJ1, Result);
  UCheb(x, -3.023518E-05, TJ, TJ1, Result);
  UCheb(x, -1.110570E-05, TJ, TJ1, Result);
  UCheb(x, 4.202332E-06, TJ, TJ1, Result);
  UCheb(x, 1.056132E-05, TJ, TJ1, Result);
  UCheb(x, 1.536323E-05, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 13, 15)
  ************************************************************************ *)
function UTblN13N15(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.750000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.513585E+00, TJ, TJ1, Result);
  UCheb(x, -4.803952E+00, TJ, TJ1, Result);
  UCheb(x, -1.090686E+00, TJ, TJ1, Result);
  UCheb(x, -1.495310E-01, TJ, TJ1, Result);
  UCheb(x, -3.160314E-02, TJ, TJ1, Result);
  UCheb(x, -9.073124E-03, TJ, TJ1, Result);
  UCheb(x, -2.480313E-03, TJ, TJ1, Result);
  UCheb(x, -7.478239E-04, TJ, TJ1, Result);
  UCheb(x, -2.140914E-04, TJ, TJ1, Result);
  UCheb(x, -5.311541E-05, TJ, TJ1, Result);
  UCheb(x, -2.677105E-06, TJ, TJ1, Result);
  UCheb(x, 1.115464E-05, TJ, TJ1, Result);
  UCheb(x, 1.578563E-05, TJ, TJ1, Result);
  UCheb(x, 2.044604E-05, TJ, TJ1, Result);
  UCheb(x, 1.888939E-05, TJ, TJ1, Result);
  UCheb(x, 2.395644E-05, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 13, 30)
  ************************************************************************ *)
function UTblN13N30(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.750000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.455999E+00, TJ, TJ1, Result);
  UCheb(x, -4.678434E+00, TJ, TJ1, Result);
  UCheb(x, -9.995491E-01, TJ, TJ1, Result);
  UCheb(x, -1.078100E-01, TJ, TJ1, Result);
  UCheb(x, -1.705220E-02, TJ, TJ1, Result);
  UCheb(x, -4.258739E-03, TJ, TJ1, Result);
  UCheb(x, -8.671526E-04, TJ, TJ1, Result);
  UCheb(x, -2.185458E-04, TJ, TJ1, Result);
  UCheb(x, -5.507764E-05, TJ, TJ1, Result);
  UCheb(x, -1.411446E-05, TJ, TJ1, Result);
  UCheb(x, -4.044355E-06, TJ, TJ1, Result);
  UCheb(x, -1.285765E-06, TJ, TJ1, Result);
  UCheb(x, -5.345282E-07, TJ, TJ1, Result);
  UCheb(x, -3.066940E-07, TJ, TJ1, Result);
  UCheb(x, -1.962037E-07, TJ, TJ1, Result);
  UCheb(x, -1.723644E-07, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 13, 100)
  ************************************************************************ *)
function UTblN13N100(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.750000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.446787E+00, TJ, TJ1, Result);
  UCheb(x, -4.640804E+00, TJ, TJ1, Result);
  UCheb(x, -9.671552E-01, TJ, TJ1, Result);
  UCheb(x, -9.364990E-02, TJ, TJ1, Result);
  UCheb(x, -1.274444E-02, TJ, TJ1, Result);
  UCheb(x, -3.047440E-03, TJ, TJ1, Result);
  UCheb(x, -5.161439E-04, TJ, TJ1, Result);
  UCheb(x, -1.171729E-04, TJ, TJ1, Result);
  UCheb(x, -2.562171E-05, TJ, TJ1, Result);
  UCheb(x, -5.359762E-06, TJ, TJ1, Result);
  UCheb(x, -1.275494E-06, TJ, TJ1, Result);
  UCheb(x, -2.747635E-07, TJ, TJ1, Result);
  UCheb(x, -5.700292E-08, TJ, TJ1, Result);
  UCheb(x, -2.565559E-09, TJ, TJ1, Result);
  UCheb(x, 5.005396E-09, TJ, TJ1, Result);
  UCheb(x, 3.335794E-09, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 14, 14)
  ************************************************************************ *)
function UTblN14N14(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.750000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.510624E+00, TJ, TJ1, Result);
  UCheb(x, -4.798584E+00, TJ, TJ1, Result);
  UCheb(x, -1.087107E+00, TJ, TJ1, Result);
  UCheb(x, -1.478532E-01, TJ, TJ1, Result);
  UCheb(x, -3.098050E-02, TJ, TJ1, Result);
  UCheb(x, -8.855986E-03, TJ, TJ1, Result);
  UCheb(x, -2.409083E-03, TJ, TJ1, Result);
  UCheb(x, -7.299536E-04, TJ, TJ1, Result);
  UCheb(x, -2.176177E-04, TJ, TJ1, Result);
  UCheb(x, -6.479417E-05, TJ, TJ1, Result);
  UCheb(x, -1.812761E-05, TJ, TJ1, Result);
  UCheb(x, -5.225872E-06, TJ, TJ1, Result);
  UCheb(x, 4.516521E-07, TJ, TJ1, Result);
  UCheb(x, 6.730551E-06, TJ, TJ1, Result);
  UCheb(x, 9.237563E-06, TJ, TJ1, Result);
  UCheb(x, 1.611820E-05, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 14, 15)
  ************************************************************************ *)
function UTblN14N15(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.750000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.498681E+00, TJ, TJ1, Result);
  UCheb(x, -4.774668E+00, TJ, TJ1, Result);
  UCheb(x, -1.070267E+00, TJ, TJ1, Result);
  UCheb(x, -1.399348E-01, TJ, TJ1, Result);
  UCheb(x, -2.807239E-02, TJ, TJ1, Result);
  UCheb(x, -7.845763E-03, TJ, TJ1, Result);
  UCheb(x, -2.071773E-03, TJ, TJ1, Result);
  UCheb(x, -6.261698E-04, TJ, TJ1, Result);
  UCheb(x, -2.011695E-04, TJ, TJ1, Result);
  UCheb(x, -7.305946E-05, TJ, TJ1, Result);
  UCheb(x, -3.879295E-05, TJ, TJ1, Result);
  UCheb(x, -2.999439E-05, TJ, TJ1, Result);
  UCheb(x, -2.904438E-05, TJ, TJ1, Result);
  UCheb(x, -2.944986E-05, TJ, TJ1, Result);
  UCheb(x, -2.373908E-05, TJ, TJ1, Result);
  UCheb(x, -2.140794E-05, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 14, 30)
  ************************************************************************ *)
function UTblN14N30(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.750000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.440378E+00, TJ, TJ1, Result);
  UCheb(x, -4.649587E+00, TJ, TJ1, Result);
  UCheb(x, -9.807829E-01, TJ, TJ1, Result);
  UCheb(x, -9.989753E-02, TJ, TJ1, Result);
  UCheb(x, -1.463646E-02, TJ, TJ1, Result);
  UCheb(x, -3.586580E-03, TJ, TJ1, Result);
  UCheb(x, -6.745917E-04, TJ, TJ1, Result);
  UCheb(x, -1.635398E-04, TJ, TJ1, Result);
  UCheb(x, -3.923172E-05, TJ, TJ1, Result);
  UCheb(x, -9.446699E-06, TJ, TJ1, Result);
  UCheb(x, -2.613892E-06, TJ, TJ1, Result);
  UCheb(x, -8.214073E-07, TJ, TJ1, Result);
  UCheb(x, -3.651683E-07, TJ, TJ1, Result);
  UCheb(x, -2.272777E-07, TJ, TJ1, Result);
  UCheb(x, -1.464988E-07, TJ, TJ1, Result);
  UCheb(x, -1.109803E-07, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 14, 100)
  ************************************************************************ *)
function UTblN14N100(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 3.750000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  UCheb(x, -4.429701E+00, TJ, TJ1, Result);
  UCheb(x, -4.610577E+00, TJ, TJ1, Result);
  UCheb(x, -9.482675E-01, TJ, TJ1, Result);
  UCheb(x, -8.605550E-02, TJ, TJ1, Result);
  UCheb(x, -1.062151E-02, TJ, TJ1, Result);
  UCheb(x, -2.525154E-03, TJ, TJ1, Result);
  UCheb(x, -3.835983E-04, TJ, TJ1, Result);
  UCheb(x, -8.411440E-05, TJ, TJ1, Result);
  UCheb(x, -1.744901E-05, TJ, TJ1, Result);
  UCheb(x, -3.318850E-06, TJ, TJ1, Result);
  UCheb(x, -7.692100E-07, TJ, TJ1, Result);
  UCheb(x, -1.536270E-07, TJ, TJ1, Result);
  UCheb(x, -3.705888E-08, TJ, TJ1, Result);
  UCheb(x, -7.999599E-09, TJ, TJ1, Result);
  UCheb(x, -2.908395E-09, TJ, TJ1, Result);
  UCheb(x, 1.546923E-09, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, N1, N2)
  ************************************************************************ *)
function USigma(s: TLFloat; n1: TLInt; n2: TLInt): TLFloat;
var
  F0: TLFloat;
  f1: TLFloat;
  f2: TLFloat;
  F3: TLFloat;
  F4: TLFloat;
  S0: TLFloat;
  s1: TLFloat;
  s2: TLFloat;
  s3: TLFloat;
  S4: TLFloat;
begin

  //
  // N1=5, N2 = 5, 6, 7, ...
  //
  if Min(n1, n2) = 5 then
    begin
      if Max(n1, n2) = 5 then
        begin
          Result := UTblN5N5(s);
        end;
      if Max(n1, n2) = 6 then
        begin
          Result := UTblN5N6(s);
        end;
      if Max(n1, n2) = 7 then
        begin
          Result := UTblN5N7(s);
        end;
      if Max(n1, n2) = 8 then
        begin
          Result := UTblN5N8(s);
        end;
      if Max(n1, n2) = 9 then
        begin
          Result := UTblN5N9(s);
        end;
      if Max(n1, n2) = 10 then
        begin
          Result := UTblN5N10(s);
        end;
      if Max(n1, n2) = 11 then
        begin
          Result := UTblN5N11(s);
        end;
      if Max(n1, n2) = 12 then
        begin
          Result := UTblN5N12(s);
        end;
      if Max(n1, n2) = 13 then
        begin
          Result := UTblN5N13(s);
        end;
      if Max(n1, n2) = 14 then
        begin
          Result := UTblN5N14(s);
        end;
      if Max(n1, n2) = 15 then
        begin
          Result := UTblN5N15(s);
        end;
      if Max(n1, n2) = 16 then
        begin
          Result := UTblN5N16(s);
        end;
      if Max(n1, n2) = 17 then
        begin
          Result := UTblN5N17(s);
        end;
      if Max(n1, n2) = 18 then
        begin
          Result := UTblN5N18(s);
        end;
      if Max(n1, n2) = 19 then
        begin
          Result := UTblN5N19(s);
        end;
      if Max(n1, n2) = 20 then
        begin
          Result := UTblN5N20(s);
        end;
      if Max(n1, n2) = 21 then
        begin
          Result := UTblN5N21(s);
        end;
      if Max(n1, n2) = 22 then
        begin
          Result := UTblN5N22(s);
        end;
      if Max(n1, n2) = 23 then
        begin
          Result := UTblN5N23(s);
        end;
      if Max(n1, n2) = 24 then
        begin
          Result := UTblN5N24(s);
        end;
      if Max(n1, n2) = 25 then
        begin
          Result := UTblN5N25(s);
        end;
      if Max(n1, n2) = 26 then
        begin
          Result := UTblN5N26(s);
        end;
      if Max(n1, n2) = 27 then
        begin
          Result := UTblN5N27(s);
        end;
      if Max(n1, n2) = 28 then
        begin
          Result := UTblN5N28(s);
        end;
      if Max(n1, n2) = 29 then
        begin
          Result := UTblN5N29(s);
        end;
      if Max(n1, n2) > 29 then
        begin
          F0 := UTblN5N15(s);
          f1 := UTblN5N30(s);
          f2 := UTblN5N100(s);
          Result := UNInterpolate(F0, f1, f2, Max(n1, n2));
        end;
      Exit;
    end;

  //
  // N1=6, N2 = 6, 7, 8, ...
  //
  if Min(n1, n2) = 6 then
    begin
      if Max(n1, n2) = 6 then
        begin
          Result := UTblN6N6(s);
        end;
      if Max(n1, n2) = 7 then
        begin
          Result := UTblN6N7(s);
        end;
      if Max(n1, n2) = 8 then
        begin
          Result := UTblN6N8(s);
        end;
      if Max(n1, n2) = 9 then
        begin
          Result := UTblN6N9(s);
        end;
      if Max(n1, n2) = 10 then
        begin
          Result := UTblN6N10(s);
        end;
      if Max(n1, n2) = 11 then
        begin
          Result := UTblN6N11(s);
        end;
      if Max(n1, n2) = 12 then
        begin
          Result := UTblN6N12(s);
        end;
      if Max(n1, n2) = 13 then
        begin
          Result := UTblN6N13(s);
        end;
      if Max(n1, n2) = 14 then
        begin
          Result := UTblN6N14(s);
        end;
      if Max(n1, n2) = 15 then
        begin
          Result := UTblN6N15(s);
        end;
      if Max(n1, n2) > 15 then
        begin
          F0 := UTblN6N15(s);
          f1 := UTblN6N30(s);
          f2 := UTblN6N100(s);
          Result := UNInterpolate(F0, f1, f2, Max(n1, n2));
        end;
      Exit;
    end;

  //
  // N1=7, N2 = 7, 8, ...
  //
  if Min(n1, n2) = 7 then
    begin
      if Max(n1, n2) = 7 then
        begin
          Result := UTblN7N7(s);
        end;
      if Max(n1, n2) = 8 then
        begin
          Result := UTblN7N8(s);
        end;
      if Max(n1, n2) = 9 then
        begin
          Result := UTblN7N9(s);
        end;
      if Max(n1, n2) = 10 then
        begin
          Result := UTblN7N10(s);
        end;
      if Max(n1, n2) = 11 then
        begin
          Result := UTblN7N11(s);
        end;
      if Max(n1, n2) = 12 then
        begin
          Result := UTblN7N12(s);
        end;
      if Max(n1, n2) = 13 then
        begin
          Result := UTblN7N13(s);
        end;
      if Max(n1, n2) = 14 then
        begin
          Result := UTblN7N14(s);
        end;
      if Max(n1, n2) = 15 then
        begin
          Result := UTblN7N15(s);
        end;
      if Max(n1, n2) > 15 then
        begin
          F0 := UTblN7N15(s);
          f1 := UTblN7N30(s);
          f2 := UTblN7N100(s);
          Result := UNInterpolate(F0, f1, f2, Max(n1, n2));
        end;
      Exit;
    end;

  //
  // N1=8, N2 = 8, 9, 10, ...
  //
  if Min(n1, n2) = 8 then
    begin
      if Max(n1, n2) = 8 then
        begin
          Result := UTblN8N8(s);
        end;
      if Max(n1, n2) = 9 then
        begin
          Result := UTblN8N9(s);
        end;
      if Max(n1, n2) = 10 then
        begin
          Result := UTblN8N10(s);
        end;
      if Max(n1, n2) = 11 then
        begin
          Result := UTblN8N11(s);
        end;
      if Max(n1, n2) = 12 then
        begin
          Result := UTblN8N12(s);
        end;
      if Max(n1, n2) = 13 then
        begin
          Result := UTblN8N13(s);
        end;
      if Max(n1, n2) = 14 then
        begin
          Result := UTblN8N14(s);
        end;
      if Max(n1, n2) = 15 then
        begin
          Result := UTblN8N15(s);
        end;
      if Max(n1, n2) > 15 then
        begin
          F0 := UTblN8N15(s);
          f1 := UTblN8N30(s);
          f2 := UTblN8N100(s);
          Result := UNInterpolate(F0, f1, f2, Max(n1, n2));
        end;
      Exit;
    end;

  //
  // N1=9, N2 = 9, 10, ...
  //
  if Min(n1, n2) = 9 then
    begin
      if Max(n1, n2) = 9 then
        begin
          Result := UTblN9N9(s);
        end;
      if Max(n1, n2) = 10 then
        begin
          Result := UTblN9N10(s);
        end;
      if Max(n1, n2) = 11 then
        begin
          Result := UTblN9N11(s);
        end;
      if Max(n1, n2) = 12 then
        begin
          Result := UTblN9N12(s);
        end;
      if Max(n1, n2) = 13 then
        begin
          Result := UTblN9N13(s);
        end;
      if Max(n1, n2) = 14 then
        begin
          Result := UTblN9N14(s);
        end;
      if Max(n1, n2) = 15 then
        begin
          Result := UTblN9N15(s);
        end;
      if Max(n1, n2) > 15 then
        begin
          F0 := UTblN9N15(s);
          f1 := UTblN9N30(s);
          f2 := UTblN9N100(s);
          Result := UNInterpolate(F0, f1, f2, Max(n1, n2));
        end;
      Exit;
    end;

  //
  // N1=10, N2 = 10, 11, ...
  //
  if Min(n1, n2) = 10 then
    begin
      if Max(n1, n2) = 10 then
        begin
          Result := UTblN10N10(s);
        end;
      if Max(n1, n2) = 11 then
        begin
          Result := UTblN10N11(s);
        end;
      if Max(n1, n2) = 12 then
        begin
          Result := UTblN10N12(s);
        end;
      if Max(n1, n2) = 13 then
        begin
          Result := UTblN10N13(s);
        end;
      if Max(n1, n2) = 14 then
        begin
          Result := UTblN10N14(s);
        end;
      if Max(n1, n2) = 15 then
        begin
          Result := UTblN10N15(s);
        end;
      if Max(n1, n2) > 15 then
        begin
          F0 := UTblN10N15(s);
          f1 := UTblN10N30(s);
          f2 := UTblN10N100(s);
          Result := UNInterpolate(F0, f1, f2, Max(n1, n2));
        end;
      Exit;
    end;

  //
  // N1=11, N2 = 11, 12, ...
  //
  if Min(n1, n2) = 11 then
    begin
      if Max(n1, n2) = 11 then
        begin
          Result := UTblN11N11(s);
        end;
      if Max(n1, n2) = 12 then
        begin
          Result := UTblN11N12(s);
        end;
      if Max(n1, n2) = 13 then
        begin
          Result := UTblN11N13(s);
        end;
      if Max(n1, n2) = 14 then
        begin
          Result := UTblN11N14(s);
        end;
      if Max(n1, n2) = 15 then
        begin
          Result := UTblN11N15(s);
        end;
      if Max(n1, n2) > 15 then
        begin
          F0 := UTblN11N15(s);
          f1 := UTblN11N30(s);
          f2 := UTblN11N100(s);
          Result := UNInterpolate(F0, f1, f2, Max(n1, n2));
        end;
      Exit;
    end;

  //
  // N1=12, N2 = 12, 13, ...
  //
  if Min(n1, n2) = 12 then
    begin
      if Max(n1, n2) = 12 then
        begin
          Result := UTblN12N12(s);
        end;
      if Max(n1, n2) = 13 then
        begin
          Result := UTblN12N13(s);
        end;
      if Max(n1, n2) = 14 then
        begin
          Result := UTblN12N14(s);
        end;
      if Max(n1, n2) = 15 then
        begin
          Result := UTblN12N15(s);
        end;
      if Max(n1, n2) > 15 then
        begin
          F0 := UTblN12N15(s);
          f1 := UTblN12N30(s);
          f2 := UTblN12N100(s);
          Result := UNInterpolate(F0, f1, f2, Max(n1, n2));
        end;
      Exit;
    end;

  //
  // N1=13, N2 = 13, 14, ...
  //
  if Min(n1, n2) = 13 then
    begin
      if Max(n1, n2) = 13 then
        begin
          Result := UTblN13N13(s);
        end;
      if Max(n1, n2) = 14 then
        begin
          Result := UTblN13N14(s);
        end;
      if Max(n1, n2) = 15 then
        begin
          Result := UTblN13N15(s);
        end;
      if Max(n1, n2) > 15 then
        begin
          F0 := UTblN13N15(s);
          f1 := UTblN13N30(s);
          f2 := UTblN13N100(s);
          Result := UNInterpolate(F0, f1, f2, Max(n1, n2));
        end;
      Exit;
    end;

  //
  // N1=14, N2 = 14, 15, ...
  //
  if Min(n1, n2) = 14 then
    begin
      if Max(n1, n2) = 14 then
        begin
          Result := UTblN14N14(s);
        end;
      if Max(n1, n2) = 15 then
        begin
          Result := UTblN14N15(s);
        end;
      if Max(n1, n2) > 15 then
        begin
          F0 := UTblN14N15(s);
          f1 := UTblN14N30(s);
          f2 := UTblN14N100(s);
          Result := UNInterpolate(F0, f1, f2, Max(n1, n2));
        end;
      Exit;
    end;

  //
  // N1 >= 15, N2 >= 15
  //
  if AP_FP_Greater(s, 4) then
    begin
      s := 4;
    end;
  if AP_FP_Less(s, 3) then
    begin
      S0 := 0.000000E+00;
      F0 := USigma000(n1, n2);
      s1 := 7.500000E-01;
      f1 := USigma075(n1, n2);
      s2 := 1.500000E+00;
      f2 := USigma150(n1, n2);
      s3 := 2.250000E+00;
      F3 := USigma225(n1, n2);
      S4 := 3.000000E+00;
      F4 := USigma300(n1, n2);
      f1 := ((s - S0) * f1 - (s - s1) * F0) / (s1 - S0);
      f2 := ((s - S0) * f2 - (s - s2) * F0) / (s2 - S0);
      F3 := ((s - S0) * F3 - (s - s3) * F0) / (s3 - S0);
      F4 := ((s - S0) * F4 - (s - S4) * F0) / (S4 - S0);
      f2 := ((s - s1) * f2 - (s - s2) * f1) / (s2 - s1);
      F3 := ((s - s1) * F3 - (s - s3) * f1) / (s3 - s1);
      F4 := ((s - s1) * F4 - (s - S4) * f1) / (S4 - s1);
      F3 := ((s - s2) * F3 - (s - s3) * f2) / (s3 - s2);
      F4 := ((s - s2) * F4 - (s - S4) * f2) / (S4 - s2);
      F4 := ((s - s3) * F4 - (s - S4) * F3) / (S4 - s3);
      Result := F4;
    end
  else
    begin
      S0 := 3.000000E+00;
      F0 := USigma300(n1, n2);
      s1 := 3.333333E+00;
      f1 := USigma333(n1, n2);
      s2 := 3.666667E+00;
      f2 := USigma367(n1, n2);
      s3 := 4.000000E+00;
      F3 := USigma400(n1, n2);
      f1 := ((s - S0) * f1 - (s - s1) * F0) / (s1 - S0);
      f2 := ((s - S0) * f2 - (s - s2) * F0) / (s2 - S0);
      F3 := ((s - S0) * F3 - (s - s3) * F0) / (s3 - S0);
      f2 := ((s - s1) * f2 - (s - s2) * f1) / (s2 - s1);
      F3 := ((s - s1) * F3 - (s - s3) * f1) / (s3 - s1);
      F3 := ((s - s2) * F3 - (s - s3) * f2) / (s3 - s2);
      Result := F3;
    end;
end; 
 
 
