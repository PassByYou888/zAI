{ ****************************************************************************** }
{ * Jarque-Bera test support, by QQ 600585@qq.com                              * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

procedure JarqueBeraStatistic(const x: TLVec; const n: TLInt; var s: TLFloat); forward;
function JarqueBeraApprox(n: TLInt; s: TLFloat): TLFloat; forward;
function JBTbl5(const s: TLFloat): TLFloat; forward;
function JBTbl6(const s: TLFloat): TLFloat; forward;
function JBTbl7(const s: TLFloat): TLFloat; forward;
function JBTbl8(const s: TLFloat): TLFloat; forward;
function JBTbl9(const s: TLFloat): TLFloat; forward;
function JBTbl10(const s: TLFloat): TLFloat; forward;
function JBTbl11(const s: TLFloat): TLFloat; forward;
function JBTbl12(const s: TLFloat): TLFloat; forward;
function JBTbl13(const s: TLFloat): TLFloat; forward;
function JBTbl14(const s: TLFloat): TLFloat; forward;
function JBTbl15(const s: TLFloat): TLFloat; forward;
function JBTbl16(const s: TLFloat): TLFloat; forward;
function JBTbl17(const s: TLFloat): TLFloat; forward;
function JBTbl18(const s: TLFloat): TLFloat; forward;
function JBTbl19(const s: TLFloat): TLFloat; forward;
function JBTbl20(const s: TLFloat): TLFloat; forward;
function JBTbl30(const s: TLFloat): TLFloat; forward;
function JBTbl50(const s: TLFloat): TLFloat; forward;
function JBTbl65(const s: TLFloat): TLFloat; forward;
function JBTbl100(const s: TLFloat): TLFloat; forward;
function JBTbl130(const s: TLFloat): TLFloat; forward;
function JBTbl200(const s: TLFloat): TLFloat; forward;
function JBTbl301(const s: TLFloat): TLFloat; forward;
function JBTbl501(const s: TLFloat): TLFloat; forward;
function JBTbl701(const s: TLFloat): TLFloat; forward;
function JBTbl1401(const s: TLFloat): TLFloat; forward;
procedure JBCheb(const x, c: TLFloat; var TJ: TLFloat; var TJ1: TLFloat; var r: TLFloat); forward;

(* ************************************************************************
  Jarque-Bera test

  This test checks hypotheses about the fact that a  given  sample  X  is  a
  sample of normal random variable.

  Requirements:
  * the number of elements in the sample is not less than 5.

  Input parameters:
  X   -   sample. Array whose index goes from 0 to N-1.
  N   -   size of the sample. N>=5

  Output parameters:
  BothTails   -   p-value for two-tailed test. If BothTails is less than the given significance level the null hypothesis is rejected.
  LeftTail    -   p-value for left-tailed test. If LeftTail is less than the given significance level, the null hypothesis is rejected.
  RightTail   -   p-value for right-tailed test. If RightTail is less than the given significance level the null hypothesis is rejected.

  Accuracy of the approximation used (5<=N<=1951):

  p-value  	    relative error (5<=N<=1951)
  [1, 0.1]            < 1%
  [0.1, 0.01]         < 2%
  [0.01, 0.001]       < 6%
  [0.001, 0]          wasn't measured

  For N>1951 accuracy wasn't measured but it shouldn't be sharply  different from table values.
  ************************************************************************ *)
procedure JarqueBeraTest(const x: TLVec; const n: TLInt; var p: TLFloat);
var
  s: TLFloat;
begin

  //
  // N is too small
  //
  if n < 5 then
    begin
      p := 1.0;
      Exit;
    end;

  //
  // N is large enough
  //
  JarqueBeraStatistic(x, n, s);
  p := JarqueBeraApprox(n, s);
end;

procedure JarqueBeraStatistic(const x: TLVec; const n: TLInt; var s: TLFloat);
var
  i       : TLInt;
  v       : TLFloat;
  v1      : TLFloat;
  v2      : TLFloat;
  StdDev  : TLFloat;
  Mean    : TLFloat;
  Variance: TLFloat;
  Skewness: TLFloat;
  Kurtosis: TLFloat;
begin
  Mean := 0;
  Variance := 0;
  Skewness := 0;
  Kurtosis := 0;
  StdDev := 0;
  Assert(n > 1);

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

  //
  // Statistic
  //
  s := AP_Float(n) / 6 * (AP_Sqr(Skewness) + AP_Sqr(Kurtosis) / 4);
end;

function JarqueBeraApprox(n: TLInt; s: TLFloat): TLFloat;
var
  Vx  : TLVec;
  Vy  : TLVec;
  CTbl: TLMatrix;
  t1  : TLFloat;
  t2  : TLFloat;
  t3  : TLFloat;
  t   : TLFloat;
  f1  : TLFloat;
  f2  : TLFloat;
  F3  : TLFloat;
  F12 : TLFloat;
  F23 : TLFloat;
  x   : TLFloat;
begin
  Result := 1;
  x := s;
  if n < 5 then
    begin
      Exit;
    end;

  //
  // N = 5..20 are tabulated
  //
  if (n >= 5) and (n <= 20) then
    begin
      if n = 5 then
        begin
          Result := Exp(JBTbl5(x));
        end;
      if n = 6 then
        begin
          Result := Exp(JBTbl6(x));
        end;
      if n = 7 then
        begin
          Result := Exp(JBTbl7(x));
        end;
      if n = 8 then
        begin
          Result := Exp(JBTbl8(x));
        end;
      if n = 9 then
        begin
          Result := Exp(JBTbl9(x));
        end;
      if n = 10 then
        begin
          Result := Exp(JBTbl10(x));
        end;
      if n = 11 then
        begin
          Result := Exp(JBTbl11(x));
        end;
      if n = 12 then
        begin
          Result := Exp(JBTbl12(x));
        end;
      if n = 13 then
        begin
          Result := Exp(JBTbl13(x));
        end;
      if n = 14 then
        begin
          Result := Exp(JBTbl14(x));
        end;
      if n = 15 then
        begin
          Result := Exp(JBTbl15(x));
        end;
      if n = 16 then
        begin
          Result := Exp(JBTbl16(x));
        end;
      if n = 17 then
        begin
          Result := Exp(JBTbl17(x));
        end;
      if n = 18 then
        begin
          Result := Exp(JBTbl18(x));
        end;
      if n = 19 then
        begin
          Result := Exp(JBTbl19(x));
        end;
      if n = 20 then
        begin
          Result := Exp(JBTbl20(x));
        end;
      Exit;
    end;

  //
  // N = 20, 30, 50 are tabulated.
  // In-between values are interpolated
  // using interpolating polynomial of the second degree.
  //
  if (n > 20) and (n <= 50) then
    begin
      t1 := -1.0 / 20.0;
      t2 := -1.0 / 30.0;
      t3 := -1.0 / 50.0;
      t := -1.0 / n;
      f1 := JBTbl20(x);
      f2 := JBTbl30(x);
      F3 := JBTbl50(x);
      F12 := ((t - t2) * f1 + (t1 - t) * f2) / (t1 - t2);
      F23 := ((t - t3) * f2 + (t2 - t) * F3) / (t2 - t3);
      Result := ((t - t3) * F12 + (t1 - t) * F23) / (t1 - t3);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Result := Exp(Result);
      Exit;
    end;

  //
  // N = 50, 65, 100 are tabulated.
  // In-between values are interpolated
  // using interpolating polynomial of the second degree.
  //
  if (n > 50) and (n <= 100) then
    begin
      t1 := -1.0 / 50.0;
      t2 := -1.0 / 65.0;
      t3 := -1.0 / 100.0;
      t := -1.0 / n;
      f1 := JBTbl50(x);
      f2 := JBTbl65(x);
      F3 := JBTbl100(x);
      F12 := ((t - t2) * f1 + (t1 - t) * f2) / (t1 - t2);
      F23 := ((t - t3) * f2 + (t2 - t) * F3) / (t2 - t3);
      Result := ((t - t3) * F12 + (t1 - t) * F23) / (t1 - t3);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Result := Exp(Result);
      Exit;
    end;

  //
  // N = 100, 130, 200 are tabulated.
  // In-between values are interpolated
  // using interpolating polynomial of the second degree.
  //
  if (n > 100) and (n <= 200) then
    begin
      t1 := -1.0 / 100.0;
      t2 := -1.0 / 130.0;
      t3 := -1.0 / 200.0;
      t := -1.0 / n;
      f1 := JBTbl100(x);
      f2 := JBTbl130(x);
      F3 := JBTbl200(x);
      F12 := ((t - t2) * f1 + (t1 - t) * f2) / (t1 - t2);
      F23 := ((t - t3) * f2 + (t2 - t) * F3) / (t2 - t3);
      Result := ((t - t3) * F12 + (t1 - t) * F23) / (t1 - t3);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Result := Exp(Result);
      Exit;
    end;

  //
  // N = 200, 301, 501 are tabulated.
  // In-between values are interpolated
  // using interpolating polynomial of the second degree.
  //
  if (n > 200) and (n <= 501) then
    begin
      t1 := -1.0 / 200.0;
      t2 := -1.0 / 301.0;
      t3 := -1.0 / 501.0;
      t := -1.0 / n;
      f1 := JBTbl200(x);
      f2 := JBTbl301(x);
      F3 := JBTbl501(x);
      F12 := ((t - t2) * f1 + (t1 - t) * f2) / (t1 - t2);
      F23 := ((t - t3) * f2 + (t2 - t) * F3) / (t2 - t3);
      Result := ((t - t3) * F12 + (t1 - t) * F23) / (t1 - t3);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Result := Exp(Result);
      Exit;
    end;

  //
  // N = 501, 701, 1401 are tabulated.
  // In-between values are interpolated
  // using interpolating polynomial of the second degree.
  //
  if (n > 501) and (n <= 1401) then
    begin
      t1 := -1.0 / 501.0;
      t2 := -1.0 / 701.0;
      t3 := -1.0 / 1401.0;
      t := -1.0 / n;
      f1 := JBTbl501(x);
      f2 := JBTbl701(x);
      F3 := JBTbl1401(x);
      F12 := ((t - t2) * f1 + (t1 - t) * f2) / (t1 - t2);
      F23 := ((t - t3) * f2 + (t2 - t) * F3) / (t2 - t3);
      Result := ((t - t3) * F12 + (t1 - t) * F23) / (t1 - t3);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Result := Exp(Result);
      Exit;
    end;

  //
  // Asymptotic expansion
  //
  if n > 1401 then
    begin
      Result := -0.5 * x + (JBTbl1401(x) + 0.5 * x) * Sqrt(AP_Float(1401) / n);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Result := Exp(Result);
      Exit;
    end;
end;

function JBTbl5(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 0.4000) then
    begin
      x := 2 * (s - 0.000000) / 0.400000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -1.097885E-20, TJ, TJ1, Result);
      JBCheb(x, -2.854501E-20, TJ, TJ1, Result);
      JBCheb(x, -1.756616E-20, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 1.1000) then
    begin
      x := 2 * (s - 0.400000) / 0.700000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -1.324545E+00, TJ, TJ1, Result);
      JBCheb(x, -1.075941E+00, TJ, TJ1, Result);
      JBCheb(x, -9.772272E-01, TJ, TJ1, Result);
      JBCheb(x, 3.175686E-01, TJ, TJ1, Result);
      JBCheb(x, -1.576162E-01, TJ, TJ1, Result);
      JBCheb(x, 1.126861E-01, TJ, TJ1, Result);
      JBCheb(x, -3.434425E-02, TJ, TJ1, Result);
      JBCheb(x, -2.790359E-01, TJ, TJ1, Result);
      JBCheb(x, 2.809178E-02, TJ, TJ1, Result);
      JBCheb(x, -5.479704E-01, TJ, TJ1, Result);
      JBCheb(x, 3.717040E-02, TJ, TJ1, Result);
      JBCheb(x, -5.294170E-01, TJ, TJ1, Result);
      JBCheb(x, 2.880632E-02, TJ, TJ1, Result);
      JBCheb(x, -3.023344E-01, TJ, TJ1, Result);
      JBCheb(x, 1.601531E-02, TJ, TJ1, Result);
      JBCheb(x, -7.920403E-02, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -5.188419E+02 * (s - 1.100000E+00) - 4.767297E+00;
end;

function JBTbl6(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 0.2500) then
    begin
      x := 2 * (s - 0.000000) / 0.250000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -2.274707E-04, TJ, TJ1, Result);
      JBCheb(x, -5.700471E-04, TJ, TJ1, Result);
      JBCheb(x, -3.425764E-04, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 1.3000) then
    begin
      x := 2 * (s - 0.250000) / 1.050000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -1.339000E+00, TJ, TJ1, Result);
      JBCheb(x, -2.011104E+00, TJ, TJ1, Result);
      JBCheb(x, -8.168177E-01, TJ, TJ1, Result);
      JBCheb(x, -1.085666E-01, TJ, TJ1, Result);
      JBCheb(x, 7.738606E-02, TJ, TJ1, Result);
      JBCheb(x, 7.022876E-02, TJ, TJ1, Result);
      JBCheb(x, 3.462402E-02, TJ, TJ1, Result);
      JBCheb(x, 6.908270E-03, TJ, TJ1, Result);
      JBCheb(x, -8.230772E-03, TJ, TJ1, Result);
      JBCheb(x, -1.006996E-02, TJ, TJ1, Result);
      JBCheb(x, -5.410222E-03, TJ, TJ1, Result);
      JBCheb(x, -2.893768E-03, TJ, TJ1, Result);
      JBCheb(x, 8.114564E-04, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 1.8500) then
    begin
      x := 2 * (s - 1.300000) / 0.550000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -6.794311E+00, TJ, TJ1, Result);
      JBCheb(x, -3.578700E+00, TJ, TJ1, Result);
      JBCheb(x, -1.394664E+00, TJ, TJ1, Result);
      JBCheb(x, -7.928290E-01, TJ, TJ1, Result);
      JBCheb(x, -4.813273E-01, TJ, TJ1, Result);
      JBCheb(x, -3.076063E-01, TJ, TJ1, Result);
      JBCheb(x, -1.835380E-01, TJ, TJ1, Result);
      JBCheb(x, -1.013013E-01, TJ, TJ1, Result);
      JBCheb(x, -5.058903E-02, TJ, TJ1, Result);
      JBCheb(x, -1.856915E-02, TJ, TJ1, Result);
      JBCheb(x, -6.710887E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -1.770029E+02 * (s - 1.850000E+00) - 1.371015E+01;
end;

function JBTbl7(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 1.4000) then
    begin
      x := 2 * (s - 0.000000) / 1.400000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -1.093681E+00, TJ, TJ1, Result);
      JBCheb(x, -1.695911E+00, TJ, TJ1, Result);
      JBCheb(x, -7.473192E-01, TJ, TJ1, Result);
      JBCheb(x, -1.203236E-01, TJ, TJ1, Result);
      JBCheb(x, 6.590379E-02, TJ, TJ1, Result);
      JBCheb(x, 6.291876E-02, TJ, TJ1, Result);
      JBCheb(x, 3.132007E-02, TJ, TJ1, Result);
      JBCheb(x, 9.411147E-03, TJ, TJ1, Result);
      JBCheb(x, -1.180067E-03, TJ, TJ1, Result);
      JBCheb(x, -3.487610E-03, TJ, TJ1, Result);
      JBCheb(x, -2.436561E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 3.0000) then
    begin
      x := 2 * (s - 1.400000) / 1.600000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -5.947854E+00, TJ, TJ1, Result);
      JBCheb(x, -2.772675E+00, TJ, TJ1, Result);
      JBCheb(x, -4.707912E-01, TJ, TJ1, Result);
      JBCheb(x, -1.691171E-01, TJ, TJ1, Result);
      JBCheb(x, -4.132795E-02, TJ, TJ1, Result);
      JBCheb(x, -1.481310E-02, TJ, TJ1, Result);
      JBCheb(x, 2.867536E-03, TJ, TJ1, Result);
      JBCheb(x, 8.772327E-04, TJ, TJ1, Result);
      JBCheb(x, 5.033387E-03, TJ, TJ1, Result);
      JBCheb(x, -1.378277E-03, TJ, TJ1, Result);
      JBCheb(x, -2.497964E-03, TJ, TJ1, Result);
      JBCheb(x, -3.636814E-03, TJ, TJ1, Result);
      JBCheb(x, -9.581640E-04, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 3.2000) then
    begin
      x := 2 * (s - 3.000000) / 0.200000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -7.511008E+00, TJ, TJ1, Result);
      JBCheb(x, -8.140472E-01, TJ, TJ1, Result);
      JBCheb(x, 1.682053E+00, TJ, TJ1, Result);
      JBCheb(x, -2.568561E-02, TJ, TJ1, Result);
      JBCheb(x, -1.933930E+00, TJ, TJ1, Result);
      JBCheb(x, -8.140472E-01, TJ, TJ1, Result);
      JBCheb(x, -3.895025E+00, TJ, TJ1, Result);
      JBCheb(x, -8.140472E-01, TJ, TJ1, Result);
      JBCheb(x, -1.933930E+00, TJ, TJ1, Result);
      JBCheb(x, -2.568561E-02, TJ, TJ1, Result);
      JBCheb(x, 1.682053E+00, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -1.824116E+03 * (s - 3.200000E+00) - 1.440330E+01;
end;

function JBTbl8(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 1.3000) then
    begin
      x := 2 * (s - 0.000000) / 1.300000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -7.199015E-01, TJ, TJ1, Result);
      JBCheb(x, -1.095921E+00, TJ, TJ1, Result);
      JBCheb(x, -4.736828E-01, TJ, TJ1, Result);
      JBCheb(x, -1.047438E-01, TJ, TJ1, Result);
      JBCheb(x, -2.484320E-03, TJ, TJ1, Result);
      JBCheb(x, 7.937923E-03, TJ, TJ1, Result);
      JBCheb(x, 4.810470E-03, TJ, TJ1, Result);
      JBCheb(x, 2.139780E-03, TJ, TJ1, Result);
      JBCheb(x, 6.708443E-04, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 2.0000) then
    begin
      x := 2 * (s - 1.300000) / 0.700000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -3.378966E+00, TJ, TJ1, Result);
      JBCheb(x, -7.802461E-01, TJ, TJ1, Result);
      JBCheb(x, 1.547593E-01, TJ, TJ1, Result);
      JBCheb(x, -6.241042E-02, TJ, TJ1, Result);
      JBCheb(x, 1.203274E-02, TJ, TJ1, Result);
      JBCheb(x, 5.201990E-03, TJ, TJ1, Result);
      JBCheb(x, -5.125597E-03, TJ, TJ1, Result);
      JBCheb(x, 1.584426E-03, TJ, TJ1, Result);
      JBCheb(x, 2.546069E-04, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 5.0000) then
    begin
      x := 2 * (s - 2.000000) / 3.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -6.828366E+00, TJ, TJ1, Result);
      JBCheb(x, -3.137533E+00, TJ, TJ1, Result);
      JBCheb(x, -5.016671E-01, TJ, TJ1, Result);
      JBCheb(x, -1.745637E-01, TJ, TJ1, Result);
      JBCheb(x, -5.189801E-02, TJ, TJ1, Result);
      JBCheb(x, -1.621610E-02, TJ, TJ1, Result);
      JBCheb(x, -6.741122E-03, TJ, TJ1, Result);
      JBCheb(x, -4.516368E-03, TJ, TJ1, Result);
      JBCheb(x, 3.552085E-04, TJ, TJ1, Result);
      JBCheb(x, 2.787029E-03, TJ, TJ1, Result);
      JBCheb(x, 5.359774E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -5.087028E+00 * (s - 5.000000E+00) - 1.071300E+01;
end;

function JBTbl9(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 1.3000) then
    begin
      x := 2 * (s - 0.000000) / 1.300000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -6.279320E-01, TJ, TJ1, Result);
      JBCheb(x, -9.277151E-01, TJ, TJ1, Result);
      JBCheb(x, -3.669339E-01, TJ, TJ1, Result);
      JBCheb(x, -7.086149E-02, TJ, TJ1, Result);
      JBCheb(x, -1.333816E-03, TJ, TJ1, Result);
      JBCheb(x, 3.871249E-03, TJ, TJ1, Result);
      JBCheb(x, 2.007048E-03, TJ, TJ1, Result);
      JBCheb(x, 7.482245E-04, TJ, TJ1, Result);
      JBCheb(x, 2.355615E-04, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 2.0000) then
    begin
      x := 2 * (s - 1.300000) / 0.700000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -2.981430E+00, TJ, TJ1, Result);
      JBCheb(x, -7.972248E-01, TJ, TJ1, Result);
      JBCheb(x, 1.747737E-01, TJ, TJ1, Result);
      JBCheb(x, -3.808530E-02, TJ, TJ1, Result);
      JBCheb(x, -7.888305E-03, TJ, TJ1, Result);
      JBCheb(x, 9.001302E-03, TJ, TJ1, Result);
      JBCheb(x, -1.378767E-03, TJ, TJ1, Result);
      JBCheb(x, -1.108510E-03, TJ, TJ1, Result);
      JBCheb(x, 5.915372E-04, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 7.0000) then
    begin
      x := 2 * (s - 2.000000) / 5.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -6.387463E+00, TJ, TJ1, Result);
      JBCheb(x, -2.845231E+00, TJ, TJ1, Result);
      JBCheb(x, -1.809956E-01, TJ, TJ1, Result);
      JBCheb(x, -7.543461E-02, TJ, TJ1, Result);
      JBCheb(x, -4.880397E-03, TJ, TJ1, Result);
      JBCheb(x, -1.160074E-02, TJ, TJ1, Result);
      JBCheb(x, -7.356527E-03, TJ, TJ1, Result);
      JBCheb(x, -4.394428E-03, TJ, TJ1, Result);
      JBCheb(x, 9.619892E-04, TJ, TJ1, Result);
      JBCheb(x, -2.758763E-04, TJ, TJ1, Result);
      JBCheb(x, 4.790977E-05, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -2.020952E+00 * (s - 7.000000E+00) - 9.516623E+00;
end;

function JBTbl10(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 1.2000) then
    begin
      x := 2 * (s - 0.000000) / 1.200000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -4.590993E-01, TJ, TJ1, Result);
      JBCheb(x, -6.562730E-01, TJ, TJ1, Result);
      JBCheb(x, -2.353934E-01, TJ, TJ1, Result);
      JBCheb(x, -4.069933E-02, TJ, TJ1, Result);
      JBCheb(x, -1.849151E-03, TJ, TJ1, Result);
      JBCheb(x, 8.931406E-04, TJ, TJ1, Result);
      JBCheb(x, 3.636295E-04, TJ, TJ1, Result);
      JBCheb(x, 1.178340E-05, TJ, TJ1, Result);
      JBCheb(x, -8.917749E-05, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 2.0000) then
    begin
      x := 2 * (s - 1.200000) / 0.800000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -2.537658E+00, TJ, TJ1, Result);
      JBCheb(x, -9.962401E-01, TJ, TJ1, Result);
      JBCheb(x, 1.838715E-01, TJ, TJ1, Result);
      JBCheb(x, 1.055792E-02, TJ, TJ1, Result);
      JBCheb(x, -2.580316E-02, TJ, TJ1, Result);
      JBCheb(x, 1.781701E-03, TJ, TJ1, Result);
      JBCheb(x, 3.770362E-03, TJ, TJ1, Result);
      JBCheb(x, -4.838983E-04, TJ, TJ1, Result);
      JBCheb(x, -6.999052E-04, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 7.0000) then
    begin
      x := 2 * (s - 2.000000) / 5.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -5.337524E+00, TJ, TJ1, Result);
      JBCheb(x, -1.877029E+00, TJ, TJ1, Result);
      JBCheb(x, 4.734650E-02, TJ, TJ1, Result);
      JBCheb(x, -4.249254E-02, TJ, TJ1, Result);
      JBCheb(x, 3.320250E-03, TJ, TJ1, Result);
      JBCheb(x, -6.432266E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -8.711035E-01 * (s - 7.000000E+00) - 7.212811E+00;
end;

function JBTbl11(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 1.2000) then
    begin
      x := 2 * (s - 0.000000) / 1.200000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -4.339517E-01, TJ, TJ1, Result);
      JBCheb(x, -6.051558E-01, TJ, TJ1, Result);
      JBCheb(x, -2.000992E-01, TJ, TJ1, Result);
      JBCheb(x, -3.022547E-02, TJ, TJ1, Result);
      JBCheb(x, -9.808401E-04, TJ, TJ1, Result);
      JBCheb(x, 5.592870E-04, TJ, TJ1, Result);
      JBCheb(x, 3.575081E-04, TJ, TJ1, Result);
      JBCheb(x, 2.086173E-04, TJ, TJ1, Result);
      JBCheb(x, 6.089011E-05, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 2.2500) then
    begin
      x := 2 * (s - 1.200000) / 1.050000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -2.523221E+00, TJ, TJ1, Result);
      JBCheb(x, -1.068388E+00, TJ, TJ1, Result);
      JBCheb(x, 2.179661E-01, TJ, TJ1, Result);
      JBCheb(x, -1.555524E-03, TJ, TJ1, Result);
      JBCheb(x, -3.238964E-02, TJ, TJ1, Result);
      JBCheb(x, 7.364320E-03, TJ, TJ1, Result);
      JBCheb(x, 4.895771E-03, TJ, TJ1, Result);
      JBCheb(x, -1.762774E-03, TJ, TJ1, Result);
      JBCheb(x, -8.201340E-04, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 8.0000) then
    begin
      x := 2 * (s - 2.250000) / 5.750000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -5.212179E+00, TJ, TJ1, Result);
      JBCheb(x, -1.684579E+00, TJ, TJ1, Result);
      JBCheb(x, 8.299519E-02, TJ, TJ1, Result);
      JBCheb(x, -3.606261E-02, TJ, TJ1, Result);
      JBCheb(x, 7.310869E-03, TJ, TJ1, Result);
      JBCheb(x, -3.320115E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -5.715445E-01 * (s - 8.000000E+00) - 6.845834E+00;
end;

function JBTbl12(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 1.0000) then
    begin
      x := 2 * (s - 0.000000) / 1.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -2.736742E-01, TJ, TJ1, Result);
      JBCheb(x, -3.657836E-01, TJ, TJ1, Result);
      JBCheb(x, -1.047209E-01, TJ, TJ1, Result);
      JBCheb(x, -1.319599E-02, TJ, TJ1, Result);
      JBCheb(x, -5.545631E-04, TJ, TJ1, Result);
      JBCheb(x, 9.280445E-05, TJ, TJ1, Result);
      JBCheb(x, 2.815679E-05, TJ, TJ1, Result);
      JBCheb(x, -2.213519E-05, TJ, TJ1, Result);
      JBCheb(x, 1.256838E-05, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 3.0000) then
    begin
      x := 2 * (s - 1.000000) / 2.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -2.573947E+00, TJ, TJ1, Result);
      JBCheb(x, -1.515287E+00, TJ, TJ1, Result);
      JBCheb(x, 3.611880E-01, TJ, TJ1, Result);
      JBCheb(x, -3.271311E-02, TJ, TJ1, Result);
      JBCheb(x, -6.495815E-02, TJ, TJ1, Result);
      JBCheb(x, 4.141186E-02, TJ, TJ1, Result);
      JBCheb(x, 7.180886E-04, TJ, TJ1, Result);
      JBCheb(x, -1.388211E-02, TJ, TJ1, Result);
      JBCheb(x, 4.890761E-03, TJ, TJ1, Result);
      JBCheb(x, 3.233175E-03, TJ, TJ1, Result);
      JBCheb(x, -2.946156E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 12.0000) then
    begin
      x := 2 * (s - 3.000000) / 9.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -5.947819E+00, TJ, TJ1, Result);
      JBCheb(x, -2.034157E+00, TJ, TJ1, Result);
      JBCheb(x, 6.878986E-02, TJ, TJ1, Result);
      JBCheb(x, -4.078603E-02, TJ, TJ1, Result);
      JBCheb(x, 6.990977E-03, TJ, TJ1, Result);
      JBCheb(x, -2.866215E-03, TJ, TJ1, Result);
      JBCheb(x, 3.897866E-03, TJ, TJ1, Result);
      JBCheb(x, 2.512252E-03, TJ, TJ1, Result);
      JBCheb(x, 2.073743E-03, TJ, TJ1, Result);
      JBCheb(x, 3.022621E-03, TJ, TJ1, Result);
      JBCheb(x, 1.501343E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -2.877243E-01 * (s - 1.200000E+01) - 7.936839E+00;
end;

function JBTbl13(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 1.0000) then
    begin
      x := 2 * (s - 0.000000) / 1.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -2.713276E-01, TJ, TJ1, Result);
      JBCheb(x, -3.557541E-01, TJ, TJ1, Result);
      JBCheb(x, -9.459092E-02, TJ, TJ1, Result);
      JBCheb(x, -1.044145E-02, TJ, TJ1, Result);
      JBCheb(x, -2.546132E-04, TJ, TJ1, Result);
      JBCheb(x, 1.002374E-04, TJ, TJ1, Result);
      JBCheb(x, 2.349456E-05, TJ, TJ1, Result);
      JBCheb(x, -7.025669E-05, TJ, TJ1, Result);
      JBCheb(x, -1.590242E-05, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 3.0000) then
    begin
      x := 2 * (s - 1.000000) / 2.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -2.454383E+00, TJ, TJ1, Result);
      JBCheb(x, -1.467539E+00, TJ, TJ1, Result);
      JBCheb(x, 3.270774E-01, TJ, TJ1, Result);
      JBCheb(x, -8.075763E-03, TJ, TJ1, Result);
      JBCheb(x, -6.611647E-02, TJ, TJ1, Result);
      JBCheb(x, 2.990785E-02, TJ, TJ1, Result);
      JBCheb(x, 8.109212E-03, TJ, TJ1, Result);
      JBCheb(x, -1.135031E-02, TJ, TJ1, Result);
      JBCheb(x, 5.915919E-04, TJ, TJ1, Result);
      JBCheb(x, 3.522390E-03, TJ, TJ1, Result);
      JBCheb(x, -1.144701E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 13.0000) then
    begin
      x := 2 * (s - 3.000000) / 10.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -5.736127E+00, TJ, TJ1, Result);
      JBCheb(x, -1.920809E+00, TJ, TJ1, Result);
      JBCheb(x, 1.175858E-01, TJ, TJ1, Result);
      JBCheb(x, -4.002049E-02, TJ, TJ1, Result);
      JBCheb(x, 1.158966E-02, TJ, TJ1, Result);
      JBCheb(x, -3.157781E-03, TJ, TJ1, Result);
      JBCheb(x, 2.762172E-03, TJ, TJ1, Result);
      JBCheb(x, 5.780347E-04, TJ, TJ1, Result);
      JBCheb(x, -1.193310E-03, TJ, TJ1, Result);
      JBCheb(x, -2.442421E-05, TJ, TJ1, Result);
      JBCheb(x, 2.547756E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -2.799944E-01 * (s - 1.300000E+01) - 7.566269E+00;
end;

function JBTbl14(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 1.0000) then
    begin
      x := 2 * (s - 0.000000) / 1.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -2.698527E-01, TJ, TJ1, Result);
      JBCheb(x, -3.479081E-01, TJ, TJ1, Result);
      JBCheb(x, -8.640733E-02, TJ, TJ1, Result);
      JBCheb(x, -8.466899E-03, TJ, TJ1, Result);
      JBCheb(x, -1.469485E-04, TJ, TJ1, Result);
      JBCheb(x, 2.150009E-05, TJ, TJ1, Result);
      JBCheb(x, 1.965975E-05, TJ, TJ1, Result);
      JBCheb(x, -4.710210E-05, TJ, TJ1, Result);
      JBCheb(x, -1.327808E-05, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 3.0000) then
    begin
      x := 2 * (s - 1.000000) / 2.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -2.350359E+00, TJ, TJ1, Result);
      JBCheb(x, -1.421365E+00, TJ, TJ1, Result);
      JBCheb(x, 2.960468E-01, TJ, TJ1, Result);
      JBCheb(x, 1.149167E-02, TJ, TJ1, Result);
      JBCheb(x, -6.361109E-02, TJ, TJ1, Result);
      JBCheb(x, 1.976022E-02, TJ, TJ1, Result);
      JBCheb(x, 1.082700E-02, TJ, TJ1, Result);
      JBCheb(x, -8.563328E-03, TJ, TJ1, Result);
      JBCheb(x, -1.453123E-03, TJ, TJ1, Result);
      JBCheb(x, 2.917559E-03, TJ, TJ1, Result);
      JBCheb(x, -1.151067E-05, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 15.0000) then
    begin
      x := 2 * (s - 3.000000) / 12.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -5.746892E+00, TJ, TJ1, Result);
      JBCheb(x, -2.010441E+00, TJ, TJ1, Result);
      JBCheb(x, 1.566146E-01, TJ, TJ1, Result);
      JBCheb(x, -5.129690E-02, TJ, TJ1, Result);
      JBCheb(x, 1.929724E-02, TJ, TJ1, Result);
      JBCheb(x, -2.524227E-03, TJ, TJ1, Result);
      JBCheb(x, 3.192933E-03, TJ, TJ1, Result);
      JBCheb(x, -4.254730E-04, TJ, TJ1, Result);
      JBCheb(x, 1.620685E-03, TJ, TJ1, Result);
      JBCheb(x, 7.289618E-04, TJ, TJ1, Result);
      JBCheb(x, -2.112350E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -2.590621E-01 * (s - 1.500000E+01) - 7.632238E+00;
end;

function JBTbl15(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 2.0000) then
    begin
      x := 2 * (s - 0.000000) / 2.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -1.043660E+00, TJ, TJ1, Result);
      JBCheb(x, -1.361653E+00, TJ, TJ1, Result);
      JBCheb(x, -3.009497E-01, TJ, TJ1, Result);
      JBCheb(x, 4.951784E-02, TJ, TJ1, Result);
      JBCheb(x, 4.377903E-02, TJ, TJ1, Result);
      JBCheb(x, 1.003253E-02, TJ, TJ1, Result);
      JBCheb(x, -1.271309E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 5.0000) then
    begin
      x := 2 * (s - 2.000000) / 3.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -3.582778E+00, TJ, TJ1, Result);
      JBCheb(x, -8.349578E-01, TJ, TJ1, Result);
      JBCheb(x, 9.476514E-02, TJ, TJ1, Result);
      JBCheb(x, -2.717385E-02, TJ, TJ1, Result);
      JBCheb(x, 1.222591E-02, TJ, TJ1, Result);
      JBCheb(x, -6.635124E-03, TJ, TJ1, Result);
      JBCheb(x, 2.815993E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 17.0000) then
    begin
      x := 2 * (s - 5.000000) / 12.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -6.115476E+00, TJ, TJ1, Result);
      JBCheb(x, -1.655936E+00, TJ, TJ1, Result);
      JBCheb(x, 8.404310E-02, TJ, TJ1, Result);
      JBCheb(x, -2.663794E-02, TJ, TJ1, Result);
      JBCheb(x, 8.868618E-03, TJ, TJ1, Result);
      JBCheb(x, 1.381447E-03, TJ, TJ1, Result);
      JBCheb(x, 9.444801E-04, TJ, TJ1, Result);
      JBCheb(x, -1.581503E-04, TJ, TJ1, Result);
      JBCheb(x, -9.468696E-04, TJ, TJ1, Result);
      JBCheb(x, 1.728509E-03, TJ, TJ1, Result);
      JBCheb(x, 1.206470E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -1.927937E-01 * (s - 1.700000E+01) - 7.700983E+00;
end;

function JBTbl16(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 2.0000) then
    begin
      x := 2 * (s - 0.000000) / 2.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -1.002570E+00, TJ, TJ1, Result);
      JBCheb(x, -1.298141E+00, TJ, TJ1, Result);
      JBCheb(x, -2.832803E-01, TJ, TJ1, Result);
      JBCheb(x, 3.877026E-02, TJ, TJ1, Result);
      JBCheb(x, 3.539436E-02, TJ, TJ1, Result);
      JBCheb(x, 8.439658E-03, TJ, TJ1, Result);
      JBCheb(x, -4.756911E-04, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 5.0000) then
    begin
      x := 2 * (s - 2.000000) / 3.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -3.486198E+00, TJ, TJ1, Result);
      JBCheb(x, -8.242944E-01, TJ, TJ1, Result);
      JBCheb(x, 1.020002E-01, TJ, TJ1, Result);
      JBCheb(x, -3.130531E-02, TJ, TJ1, Result);
      JBCheb(x, 1.512373E-02, TJ, TJ1, Result);
      JBCheb(x, -8.054876E-03, TJ, TJ1, Result);
      JBCheb(x, 3.556839E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 20.0000) then
    begin
      x := 2 * (s - 5.000000) / 15.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -6.241608E+00, TJ, TJ1, Result);
      JBCheb(x, -1.832655E+00, TJ, TJ1, Result);
      JBCheb(x, 1.340545E-01, TJ, TJ1, Result);
      JBCheb(x, -3.361143E-02, TJ, TJ1, Result);
      JBCheb(x, 1.283219E-02, TJ, TJ1, Result);
      JBCheb(x, 3.484549E-03, TJ, TJ1, Result);
      JBCheb(x, 1.805968E-03, TJ, TJ1, Result);
      JBCheb(x, -2.057243E-03, TJ, TJ1, Result);
      JBCheb(x, -1.454439E-03, TJ, TJ1, Result);
      JBCheb(x, -2.177513E-03, TJ, TJ1, Result);
      JBCheb(x, -1.819209E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -2.391580E-01 * (s - 2.000000E+01) - 7.963205E+00;
end;

function JBTbl17(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 3.0000) then
    begin
      x := 2 * (s - 0.000000) / 3.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -1.566973E+00, TJ, TJ1, Result);
      JBCheb(x, -1.810330E+00, TJ, TJ1, Result);
      JBCheb(x, -4.840039E-02, TJ, TJ1, Result);
      JBCheb(x, 2.337294E-01, TJ, TJ1, Result);
      JBCheb(x, -5.383549E-04, TJ, TJ1, Result);
      JBCheb(x, -5.556515E-02, TJ, TJ1, Result);
      JBCheb(x, -8.656965E-03, TJ, TJ1, Result);
      JBCheb(x, 1.404569E-02, TJ, TJ1, Result);
      JBCheb(x, 6.447867E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 6.0000) then
    begin
      x := 2 * (s - 3.000000) / 3.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -3.905684E+00, TJ, TJ1, Result);
      JBCheb(x, -6.222920E-01, TJ, TJ1, Result);
      JBCheb(x, 4.146667E-02, TJ, TJ1, Result);
      JBCheb(x, -4.809176E-03, TJ, TJ1, Result);
      JBCheb(x, 1.057028E-03, TJ, TJ1, Result);
      JBCheb(x, -1.211838E-04, TJ, TJ1, Result);
      JBCheb(x, -4.099683E-04, TJ, TJ1, Result);
      JBCheb(x, 1.161105E-04, TJ, TJ1, Result);
      JBCheb(x, 2.225465E-04, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 24.0000) then
    begin
      x := 2 * (s - 6.000000) / 18.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -6.594282E+00, TJ, TJ1, Result);
      JBCheb(x, -1.917838E+00, TJ, TJ1, Result);
      JBCheb(x, 1.455980E-01, TJ, TJ1, Result);
      JBCheb(x, -2.999589E-02, TJ, TJ1, Result);
      JBCheb(x, 5.604263E-03, TJ, TJ1, Result);
      JBCheb(x, -3.484445E-03, TJ, TJ1, Result);
      JBCheb(x, -1.819937E-03, TJ, TJ1, Result);
      JBCheb(x, -2.930390E-03, TJ, TJ1, Result);
      JBCheb(x, 2.771761E-04, TJ, TJ1, Result);
      JBCheb(x, -6.232581E-04, TJ, TJ1, Result);
      JBCheb(x, -7.029083E-04, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -2.127771E-01 * (s - 2.400000E+01) - 8.400197E+00;
end;

function JBTbl18(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 3.0000) then
    begin
      x := 2 * (s - 0.000000) / 3.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -1.526802E+00, TJ, TJ1, Result);
      JBCheb(x, -1.762373E+00, TJ, TJ1, Result);
      JBCheb(x, -5.598890E-02, TJ, TJ1, Result);
      JBCheb(x, 2.189437E-01, TJ, TJ1, Result);
      JBCheb(x, 5.971721E-03, TJ, TJ1, Result);
      JBCheb(x, -4.823067E-02, TJ, TJ1, Result);
      JBCheb(x, -1.064501E-02, TJ, TJ1, Result);
      JBCheb(x, 1.014932E-02, TJ, TJ1, Result);
      JBCheb(x, 5.953513E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 6.0000) then
    begin
      x := 2 * (s - 3.000000) / 3.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -3.818669E+00, TJ, TJ1, Result);
      JBCheb(x, -6.070918E-01, TJ, TJ1, Result);
      JBCheb(x, 4.277196E-02, TJ, TJ1, Result);
      JBCheb(x, -4.879817E-03, TJ, TJ1, Result);
      JBCheb(x, 6.887357E-04, TJ, TJ1, Result);
      JBCheb(x, 1.638451E-05, TJ, TJ1, Result);
      JBCheb(x, 1.502800E-04, TJ, TJ1, Result);
      JBCheb(x, -3.165796E-05, TJ, TJ1, Result);
      JBCheb(x, 5.034960E-05, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 20.0000) then
    begin
      x := 2 * (s - 6.000000) / 14.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -6.010656E+00, TJ, TJ1, Result);
      JBCheb(x, -1.496296E+00, TJ, TJ1, Result);
      JBCheb(x, 1.002227E-01, TJ, TJ1, Result);
      JBCheb(x, -2.338250E-02, TJ, TJ1, Result);
      JBCheb(x, 4.137036E-03, TJ, TJ1, Result);
      JBCheb(x, -2.586202E-03, TJ, TJ1, Result);
      JBCheb(x, -9.736384E-04, TJ, TJ1, Result);
      JBCheb(x, 1.332251E-03, TJ, TJ1, Result);
      JBCheb(x, 1.877982E-03, TJ, TJ1, Result);
      JBCheb(x, -1.160963E-05, TJ, TJ1, Result);
      JBCheb(x, -2.547247E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -1.684623E-01 * (s - 2.000000E+01) - 7.428883E+00;
end;

function JBTbl19(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 3.0000) then
    begin
      x := 2 * (s - 0.000000) / 3.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -1.490213E+00, TJ, TJ1, Result);
      JBCheb(x, -1.719633E+00, TJ, TJ1, Result);
      JBCheb(x, -6.459123E-02, TJ, TJ1, Result);
      JBCheb(x, 2.034878E-01, TJ, TJ1, Result);
      JBCheb(x, 1.113868E-02, TJ, TJ1, Result);
      JBCheb(x, -4.030922E-02, TJ, TJ1, Result);
      JBCheb(x, -1.054022E-02, TJ, TJ1, Result);
      JBCheb(x, 7.525623E-03, TJ, TJ1, Result);
      JBCheb(x, 5.277360E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 6.0000) then
    begin
      x := 2 * (s - 3.000000) / 3.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -3.744750E+00, TJ, TJ1, Result);
      JBCheb(x, -5.977749E-01, TJ, TJ1, Result);
      JBCheb(x, 4.223716E-02, TJ, TJ1, Result);
      JBCheb(x, -5.363889E-03, TJ, TJ1, Result);
      JBCheb(x, 5.711774E-04, TJ, TJ1, Result);
      JBCheb(x, -5.557257E-04, TJ, TJ1, Result);
      JBCheb(x, 4.254794E-04, TJ, TJ1, Result);
      JBCheb(x, 9.034207E-05, TJ, TJ1, Result);
      JBCheb(x, 5.498107E-05, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 20.0000) then
    begin
      x := 2 * (s - 6.000000) / 14.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -5.872768E+00, TJ, TJ1, Result);
      JBCheb(x, -1.430689E+00, TJ, TJ1, Result);
      JBCheb(x, 1.136575E-01, TJ, TJ1, Result);
      JBCheb(x, -1.726627E-02, TJ, TJ1, Result);
      JBCheb(x, 3.421110E-03, TJ, TJ1, Result);
      JBCheb(x, -1.581510E-03, TJ, TJ1, Result);
      JBCheb(x, -5.559520E-04, TJ, TJ1, Result);
      JBCheb(x, -6.838208E-04, TJ, TJ1, Result);
      JBCheb(x, 8.428839E-04, TJ, TJ1, Result);
      JBCheb(x, -7.170682E-04, TJ, TJ1, Result);
      JBCheb(x, -6.006647E-04, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -1.539373E-01 * (s - 2.000000E+01) - 7.206941E+00;
end;

function JBTbl20(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 4.0000) then
    begin
      x := 2 * (s - 0.000000) / 4.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -1.854794E+00, TJ, TJ1, Result);
      JBCheb(x, -1.948947E+00, TJ, TJ1, Result);
      JBCheb(x, 1.632184E-01, TJ, TJ1, Result);
      JBCheb(x, 2.139397E-01, TJ, TJ1, Result);
      JBCheb(x, -1.006237E-01, TJ, TJ1, Result);
      JBCheb(x, -3.810031E-02, TJ, TJ1, Result);
      JBCheb(x, 3.573620E-02, TJ, TJ1, Result);
      JBCheb(x, 9.951242E-03, TJ, TJ1, Result);
      JBCheb(x, -1.274092E-02, TJ, TJ1, Result);
      JBCheb(x, -3.464196E-03, TJ, TJ1, Result);
      JBCheb(x, 4.882139E-03, TJ, TJ1, Result);
      JBCheb(x, 1.575144E-03, TJ, TJ1, Result);
      JBCheb(x, -1.822804E-03, TJ, TJ1, Result);
      JBCheb(x, -7.061348E-04, TJ, TJ1, Result);
      JBCheb(x, 5.908404E-04, TJ, TJ1, Result);
      JBCheb(x, 1.978353E-04, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 15.0000) then
    begin
      x := 2 * (s - 4.000000) / 11.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -5.030989E+00, TJ, TJ1, Result);
      JBCheb(x, -1.327151E+00, TJ, TJ1, Result);
      JBCheb(x, 1.346404E-01, TJ, TJ1, Result);
      JBCheb(x, -2.840051E-02, TJ, TJ1, Result);
      JBCheb(x, 7.578551E-03, TJ, TJ1, Result);
      JBCheb(x, -9.813886E-04, TJ, TJ1, Result);
      JBCheb(x, 5.905973E-05, TJ, TJ1, Result);
      JBCheb(x, -5.358489E-04, TJ, TJ1, Result);
      JBCheb(x, -3.450795E-04, TJ, TJ1, Result);
      JBCheb(x, -6.941157E-04, TJ, TJ1, Result);
      JBCheb(x, -7.432418E-04, TJ, TJ1, Result);
      JBCheb(x, -2.070537E-04, TJ, TJ1, Result);
      JBCheb(x, 9.375654E-04, TJ, TJ1, Result);
      JBCheb(x, 5.367378E-04, TJ, TJ1, Result);
      JBCheb(x, 9.890859E-04, TJ, TJ1, Result);
      JBCheb(x, 6.679782E-04, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 25.0000) then
    begin
      x := 2 * (s - 15.000000) / 10.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -7.015854E+00, TJ, TJ1, Result);
      JBCheb(x, -7.487737E-01, TJ, TJ1, Result);
      JBCheb(x, 2.244254E-02, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -1.318007E-01 * (s - 2.500000E+01) - 7.742185E+00;
end;

function JBTbl30(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 4.0000) then
    begin
      x := 2 * (s - 0.000000) / 4.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -1.630822E+00, TJ, TJ1, Result);
      JBCheb(x, -1.724298E+00, TJ, TJ1, Result);
      JBCheb(x, 7.872756E-02, TJ, TJ1, Result);
      JBCheb(x, 1.658268E-01, TJ, TJ1, Result);
      JBCheb(x, -3.573597E-02, TJ, TJ1, Result);
      JBCheb(x, -2.994157E-02, TJ, TJ1, Result);
      JBCheb(x, 5.994825E-03, TJ, TJ1, Result);
      JBCheb(x, 7.394303E-03, TJ, TJ1, Result);
      JBCheb(x, -5.785029E-04, TJ, TJ1, Result);
      JBCheb(x, -1.990264E-03, TJ, TJ1, Result);
      JBCheb(x, -1.037838E-04, TJ, TJ1, Result);
      JBCheb(x, 6.755546E-04, TJ, TJ1, Result);
      JBCheb(x, 1.774473E-04, TJ, TJ1, Result);
      JBCheb(x, -2.821395E-04, TJ, TJ1, Result);
      JBCheb(x, -1.392603E-04, TJ, TJ1, Result);
      JBCheb(x, 1.353313E-04, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 15.0000) then
    begin
      x := 2 * (s - 4.000000) / 11.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -4.539322E+00, TJ, TJ1, Result);
      JBCheb(x, -1.197018E+00, TJ, TJ1, Result);
      JBCheb(x, 1.396848E-01, TJ, TJ1, Result);
      JBCheb(x, -2.804293E-02, TJ, TJ1, Result);
      JBCheb(x, 6.867928E-03, TJ, TJ1, Result);
      JBCheb(x, -2.768758E-03, TJ, TJ1, Result);
      JBCheb(x, 5.211792E-04, TJ, TJ1, Result);
      JBCheb(x, 4.925799E-04, TJ, TJ1, Result);
      JBCheb(x, 5.046235E-04, TJ, TJ1, Result);
      JBCheb(x, -9.536469E-05, TJ, TJ1, Result);
      JBCheb(x, -6.489642E-04, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 25.0000) then
    begin
      x := 2 * (s - 15.000000) / 10.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -6.263462E+00, TJ, TJ1, Result);
      JBCheb(x, -6.177316E-01, TJ, TJ1, Result);
      JBCheb(x, 2.590637E-02, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -1.028212E-01 * (s - 2.500000E+01) - 6.855288E+00;
end;

function JBTbl50(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 4.0000) then
    begin
      x := 2 * (s - 0.000000) / 4.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -1.436279E+00, TJ, TJ1, Result);
      JBCheb(x, -1.519711E+00, TJ, TJ1, Result);
      JBCheb(x, 1.148699E-02, TJ, TJ1, Result);
      JBCheb(x, 1.001204E-01, TJ, TJ1, Result);
      JBCheb(x, -3.207620E-03, TJ, TJ1, Result);
      JBCheb(x, -1.034778E-02, TJ, TJ1, Result);
      JBCheb(x, -1.220322E-03, TJ, TJ1, Result);
      JBCheb(x, 1.033260E-03, TJ, TJ1, Result);
      JBCheb(x, 2.588280E-04, TJ, TJ1, Result);
      JBCheb(x, -1.851653E-04, TJ, TJ1, Result);
      JBCheb(x, -1.287733E-04, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 15.0000) then
    begin
      x := 2 * (s - 4.000000) / 11.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -4.234645E+00, TJ, TJ1, Result);
      JBCheb(x, -1.189127E+00, TJ, TJ1, Result);
      JBCheb(x, 1.429738E-01, TJ, TJ1, Result);
      JBCheb(x, -3.058822E-02, TJ, TJ1, Result);
      JBCheb(x, 9.086776E-03, TJ, TJ1, Result);
      JBCheb(x, -1.445783E-03, TJ, TJ1, Result);
      JBCheb(x, 1.311671E-03, TJ, TJ1, Result);
      JBCheb(x, -7.261298E-04, TJ, TJ1, Result);
      JBCheb(x, 6.496987E-04, TJ, TJ1, Result);
      JBCheb(x, 2.605249E-04, TJ, TJ1, Result);
      JBCheb(x, 8.162282E-04, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 25.0000) then
    begin
      x := 2 * (s - 15.000000) / 10.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -5.921095E+00, TJ, TJ1, Result);
      JBCheb(x, -5.888603E-01, TJ, TJ1, Result);
      JBCheb(x, 3.080113E-02, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -9.313116E-02 * (s - 2.500000E+01) - 6.479154E+00;
end;

function JBTbl65(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 4.0000) then
    begin
      x := 2 * (s - 0.000000) / 4.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -1.360024E+00, TJ, TJ1, Result);
      JBCheb(x, -1.434631E+00, TJ, TJ1, Result);
      JBCheb(x, -6.514580E-03, TJ, TJ1, Result);
      JBCheb(x, 7.332038E-02, TJ, TJ1, Result);
      JBCheb(x, 1.158197E-03, TJ, TJ1, Result);
      JBCheb(x, -5.121233E-03, TJ, TJ1, Result);
      JBCheb(x, -1.051056E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 15.0000) then
    begin
      x := 2 * (s - 4.000000) / 11.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -4.148601E+00, TJ, TJ1, Result);
      JBCheb(x, -1.214233E+00, TJ, TJ1, Result);
      JBCheb(x, 1.487977E-01, TJ, TJ1, Result);
      JBCheb(x, -3.424720E-02, TJ, TJ1, Result);
      JBCheb(x, 1.116715E-02, TJ, TJ1, Result);
      JBCheb(x, -4.043152E-03, TJ, TJ1, Result);
      JBCheb(x, 1.718149E-03, TJ, TJ1, Result);
      JBCheb(x, -1.313701E-03, TJ, TJ1, Result);
      JBCheb(x, 3.097305E-04, TJ, TJ1, Result);
      JBCheb(x, 2.181031E-04, TJ, TJ1, Result);
      JBCheb(x, 1.256975E-04, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 25.0000) then
    begin
      x := 2 * (s - 15.000000) / 10.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -5.858951E+00, TJ, TJ1, Result);
      JBCheb(x, -5.895179E-01, TJ, TJ1, Result);
      JBCheb(x, 2.933237E-02, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -9.443768E-02 * (s - 2.500000E+01) - 6.419137E+00;
end;

function JBTbl100(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 4.0000) then
    begin
      x := 2 * (s - 0.000000) / 4.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -1.257021E+00, TJ, TJ1, Result);
      JBCheb(x, -1.313418E+00, TJ, TJ1, Result);
      JBCheb(x, -1.628931E-02, TJ, TJ1, Result);
      JBCheb(x, 4.264287E-02, TJ, TJ1, Result);
      JBCheb(x, 1.518487E-03, TJ, TJ1, Result);
      JBCheb(x, -1.499826E-03, TJ, TJ1, Result);
      JBCheb(x, -4.836044E-04, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 15.0000) then
    begin
      x := 2 * (s - 4.000000) / 11.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -4.056508E+00, TJ, TJ1, Result);
      JBCheb(x, -1.279690E+00, TJ, TJ1, Result);
      JBCheb(x, 1.665746E-01, TJ, TJ1, Result);
      JBCheb(x, -4.290012E-02, TJ, TJ1, Result);
      JBCheb(x, 1.487632E-02, TJ, TJ1, Result);
      JBCheb(x, -5.704465E-03, TJ, TJ1, Result);
      JBCheb(x, 2.211669E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 25.0000) then
    begin
      x := 2 * (s - 15.000000) / 10.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -5.866099E+00, TJ, TJ1, Result);
      JBCheb(x, -6.399767E-01, TJ, TJ1, Result);
      JBCheb(x, 2.498208E-02, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -1.080097E-01 * (s - 2.500000E+01) - 6.481094E+00;
end;

function JBTbl130(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 4.0000) then
    begin
      x := 2 * (s - 0.000000) / 4.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -1.207999E+00, TJ, TJ1, Result);
      JBCheb(x, -1.253864E+00, TJ, TJ1, Result);
      JBCheb(x, -1.618032E-02, TJ, TJ1, Result);
      JBCheb(x, 3.112729E-02, TJ, TJ1, Result);
      JBCheb(x, 1.210546E-03, TJ, TJ1, Result);
      JBCheb(x, -4.732602E-04, TJ, TJ1, Result);
      JBCheb(x, -2.410527E-04, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 15.0000) then
    begin
      x := 2 * (s - 4.000000) / 11.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -4.026324E+00, TJ, TJ1, Result);
      JBCheb(x, -1.331990E+00, TJ, TJ1, Result);
      JBCheb(x, 1.779129E-01, TJ, TJ1, Result);
      JBCheb(x, -4.674749E-02, TJ, TJ1, Result);
      JBCheb(x, 1.669077E-02, TJ, TJ1, Result);
      JBCheb(x, -5.679136E-03, TJ, TJ1, Result);
      JBCheb(x, 8.833221E-04, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 25.0000) then
    begin
      x := 2 * (s - 15.000000) / 10.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -5.893951E+00, TJ, TJ1, Result);
      JBCheb(x, -6.475304E-01, TJ, TJ1, Result);
      JBCheb(x, 3.116734E-02, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -1.045722E-01 * (s - 2.500000E+01) - 6.510314E+00;
end;

function JBTbl200(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 4.0000) then
    begin
      x := 2 * (s - 0.000000) / 4.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -1.146155E+00, TJ, TJ1, Result);
      JBCheb(x, -1.177398E+00, TJ, TJ1, Result);
      JBCheb(x, -1.297970E-02, TJ, TJ1, Result);
      JBCheb(x, 1.869745E-02, TJ, TJ1, Result);
      JBCheb(x, 1.717288E-04, TJ, TJ1, Result);
      JBCheb(x, -1.982108E-04, TJ, TJ1, Result);
      JBCheb(x, 6.427636E-05, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 15.0000) then
    begin
      x := 2 * (s - 4.000000) / 11.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -4.034235E+00, TJ, TJ1, Result);
      JBCheb(x, -1.455006E+00, TJ, TJ1, Result);
      JBCheb(x, 1.942996E-01, TJ, TJ1, Result);
      JBCheb(x, -4.973795E-02, TJ, TJ1, Result);
      JBCheb(x, 1.418812E-02, TJ, TJ1, Result);
      JBCheb(x, -3.156778E-03, TJ, TJ1, Result);
      JBCheb(x, 4.896705E-05, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 25.0000) then
    begin
      x := 2 * (s - 15.000000) / 10.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -6.086071E+00, TJ, TJ1, Result);
      JBCheb(x, -7.152176E-01, TJ, TJ1, Result);
      JBCheb(x, 3.725393E-02, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -1.132404E-01 * (s - 2.500000E+01) - 6.764034E+00;
end;

function JBTbl301(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 4.0000) then
    begin
      x := 2 * (s - 0.000000) / 4.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -1.104290E+00, TJ, TJ1, Result);
      JBCheb(x, -1.125800E+00, TJ, TJ1, Result);
      JBCheb(x, -9.595847E-03, TJ, TJ1, Result);
      JBCheb(x, 1.219666E-02, TJ, TJ1, Result);
      JBCheb(x, 1.502210E-04, TJ, TJ1, Result);
      JBCheb(x, -6.414543E-05, TJ, TJ1, Result);
      JBCheb(x, 6.754115E-05, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 15.0000) then
    begin
      x := 2 * (s - 4.000000) / 11.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -4.065955E+00, TJ, TJ1, Result);
      JBCheb(x, -1.582060E+00, TJ, TJ1, Result);
      JBCheb(x, 2.004472E-01, TJ, TJ1, Result);
      JBCheb(x, -4.709092E-02, TJ, TJ1, Result);
      JBCheb(x, 1.105779E-02, TJ, TJ1, Result);
      JBCheb(x, 1.197391E-03, TJ, TJ1, Result);
      JBCheb(x, -8.386780E-04, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 25.0000) then
    begin
      x := 2 * (s - 15.000000) / 10.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -6.311384E+00, TJ, TJ1, Result);
      JBCheb(x, -7.918763E-01, TJ, TJ1, Result);
      JBCheb(x, 3.626584E-02, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -1.293626E-01 * (s - 2.500000E+01) - 7.066995E+00;
end;

function JBTbl501(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 4.0000) then
    begin
      x := 2 * (s - 0.000000) / 4.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -1.067426E+00, TJ, TJ1, Result);
      JBCheb(x, -1.079765E+00, TJ, TJ1, Result);
      JBCheb(x, -5.463005E-03, TJ, TJ1, Result);
      JBCheb(x, 6.875659E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 15.0000) then
    begin
      x := 2 * (s - 4.000000) / 11.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -4.127574E+00, TJ, TJ1, Result);
      JBCheb(x, -1.740694E+00, TJ, TJ1, Result);
      JBCheb(x, 2.044502E-01, TJ, TJ1, Result);
      JBCheb(x, -3.746714E-02, TJ, TJ1, Result);
      JBCheb(x, 3.810594E-04, TJ, TJ1, Result);
      JBCheb(x, 1.197111E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 25.0000) then
    begin
      x := 2 * (s - 15.000000) / 10.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -6.628194E+00, TJ, TJ1, Result);
      JBCheb(x, -8.846221E-01, TJ, TJ1, Result);
      JBCheb(x, 4.386405E-02, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -1.418332E-01 * (s - 2.500000E+01) - 7.468952E+00;
end;

function JBTbl701(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 4.0000) then
    begin
      x := 2 * (s - 0.000000) / 4.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -1.050999E+00, TJ, TJ1, Result);
      JBCheb(x, -1.059769E+00, TJ, TJ1, Result);
      JBCheb(x, -3.922680E-03, TJ, TJ1, Result);
      JBCheb(x, 4.847054E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 15.0000) then
    begin
      x := 2 * (s - 4.000000) / 11.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -4.192182E+00, TJ, TJ1, Result);
      JBCheb(x, -1.860007E+00, TJ, TJ1, Result);
      JBCheb(x, 1.963942E-01, TJ, TJ1, Result);
      JBCheb(x, -2.838711E-02, TJ, TJ1, Result);
      JBCheb(x, -2.893112E-04, TJ, TJ1, Result);
      JBCheb(x, 2.159788E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 25.0000) then
    begin
      x := 2 * (s - 15.000000) / 10.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -6.917851E+00, TJ, TJ1, Result);
      JBCheb(x, -9.817020E-01, TJ, TJ1, Result);
      JBCheb(x, 5.383727E-02, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -1.532706E-01 * (s - 2.500000E+01) - 7.845715E+00;
end;

function JBTbl1401(const s: TLFloat): TLFloat;
var
  x  : TLFloat;
  TJ : TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  if AP_FP_Less_Eq(s, 4.0000) then
    begin
      x := 2 * (s - 0.000000) / 4.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -1.026266E+00, TJ, TJ1, Result);
      JBCheb(x, -1.030061E+00, TJ, TJ1, Result);
      JBCheb(x, -1.259222E-03, TJ, TJ1, Result);
      JBCheb(x, 2.536254E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 15.0000) then
    begin
      x := 2 * (s - 4.000000) / 11.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -4.329849E+00, TJ, TJ1, Result);
      JBCheb(x, -2.095443E+00, TJ, TJ1, Result);
      JBCheb(x, 1.759363E-01, TJ, TJ1, Result);
      JBCheb(x, -7.751359E-03, TJ, TJ1, Result);
      JBCheb(x, -6.124368E-03, TJ, TJ1, Result);
      JBCheb(x, -1.793114E-03, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  if AP_FP_Less_Eq(s, 25.0000) then
    begin
      x := 2 * (s - 15.000000) / 10.000000 - 1;
      TJ := 1;
      TJ1 := x;
      JBCheb(x, -7.544330E+00, TJ, TJ1, Result);
      JBCheb(x, -1.225382E+00, TJ, TJ1, Result);
      JBCheb(x, 5.392349E-02, TJ, TJ1, Result);
      if AP_FP_Greater(Result, 0) then
        begin
          Result := 0;
        end;
      Exit;
    end;
  Result := -2.019375E-01 * (s - 2.500000E+01) - 8.715788E+00;
end;

procedure JBCheb(const x, c: TLFloat; var TJ: TLFloat; var TJ1: TLFloat; var r: TLFloat);
var
  t: TLFloat;
begin
  r := r + c * TJ;
  t := 2 * x * TJ1 - TJ;
  TJ := TJ1;
  TJ1 := t;
end; 
 
 
