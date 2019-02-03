{ ****************************************************************************** }
{ * Student's t distribution support, by QQ 600585@qq.com                      * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

(* ************************************************************************
  *Student's t distribution
  *
  *Computes the integral from minus infinity to t of the Student
  *t distribution with integer k > 0 degrees of freedom:
  *
  *                                     t
  *                                     -
  *                                    | |
  *             -                      |         2   -(k+1)/2
  *            | ( (k+1)/2 )           |  (     x   )
  *      ----------------------        |  ( 1 + --- )        dx
  *                    -               |  (      k  )
  *      sqrt( k pi ) | ( k/2 )        |
  *                                  | |
  *                                   -
  *                                  -inf.
  *
  *Relation to incomplete beta integral:
  *
  *       1 - stdtr(k,t) = 0.5 * incbet( k/2, 1/2, z )
  *where
  *       z = k/(k + t**2).
  *
  *For t < -2, this is the method of computation.  For higher t,
  *a direct method is derived from integration by parts.
  *Since the function is symmetric about t=0, the area under the
  *right tail of the density is found by calling the function
  *with -t instead of t.
  *
  *ACCURACY:
  *
  *Tested at random 1 <= k <= 25.  The "domain" refers to t.
  *                     Relative error:
  *arithmetic   domain     # trials      peak         rms
  *   IEEE     -100,-2      50000       5.9e-15     1.4e-15
  *   IEEE     -2,100      500000       2.7e-15     4.9e-17
  *
  *Cephes Math Library Release 2.8:  June, 2000
  *Copyright 1984, 1987, 1995, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function StudentTDistribution(const k: TLInt; const t: TLFloat): TLFloat;
var
  x   : TLFloat;
  rk  : TLFloat;
  z   : TLFloat;
  f   : TLFloat;
  tz  : TLFloat;
  p   : TLFloat;
  xsqk: TLFloat;
  j   : TLInt;
begin
  Assert(k > 0, 'Domain error in StudentTDistribution');
  if AP_FP_Eq(t, 0) then
    begin
      Result := 0.5;
      Exit;
    end;
  if AP_FP_Less(t, -2.0) then
    begin
      rk := k;
      z := rk / (rk + t * t);
      Result := 0.5 * IncompleteBeta(0.5 * rk, 0.5, z);
      Exit;
    end;
  if AP_FP_Less(t, 0) then
    begin
      x := -t;
    end
  else
    begin
      x := t;
    end;
  rk := k;
  z := 1.0 + x * x / rk;
  if k mod 2 <> 0 then
    begin
      xsqk := x / Sqrt(rk);
      p := ArcTan(xsqk);
      if k > 1 then
        begin
          f := 1.0;
          tz := 1.0;
          j := 3;
          while (j <= k - 2) and AP_FP_Greater(tz / f, MachineEpsilon) do
            begin
              tz := tz * ((j - 1) / (z * j));
              f := f + tz;
              j := j + 2;
            end;
          p := p + f * xsqk / z;
        end;
      p := p * 2.0 / pi;
    end
  else
    begin
      f := 1.0;
      tz := 1.0;
      j := 2;
      while (j <= k - 2) and AP_FP_Greater(tz / f, MachineEpsilon) do
        begin
          tz := tz * ((j - 1) / (z * j));
          f := f + tz;
          j := j + 2;
        end;
      p := f * x / Sqrt(z * rk);
    end;
  if AP_FP_Less(t, 0) then
    begin
      p := -p;
    end;
  Result := 0.5 + 0.5 * p;
end;

(* ************************************************************************
  Functional inverse of Student's t distribution

  Given probability p, finds the argument t such that stdtr(k,t)
  is equal to p.

  ACCURACY:

  Tested at random 1 <= k <= 100.  The "domain" refers to p:
  Relative error:
  arithmetic   domain     # trials      peak         rms
  IEEE    .001,.999     25000       5.7e-15     8.0e-16
  IEEE    10^-6,.001    25000       2.0e-12     2.9e-14

  Cephes Math Library Release 2.8:  June, 2000
  Copyright 1984, 1987, 1995, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function InvStudentTDistribution(const k: TLInt; p: TLFloat): TLFloat;
var
  t   : TLFloat;
  rk  : TLFloat;
  z   : TLFloat;
  rflg: TLInt;
begin
  Assert((k > 0) and AP_FP_Greater(p, 0) and AP_FP_Less(p, 1),
    'Domain error in InvStudentTDistribution');
  rk := k;
  if AP_FP_Greater(p, 0.25) and AP_FP_Less(p, 0.75) then
    begin
      if AP_FP_Eq(p, 0.5) then
        begin
          Result := 0;
          Exit;
        end;
      z := 1.0 - 2.0 * p;
      z := InvIncompleteBeta(0.5, 0.5 * rk, AbsReal(z));
      t := Sqrt(rk * z / (1.0 - z));
      if AP_FP_Less(p, 0.5) then
        begin
          t := -t;
        end;
      Result := t;
      Exit;
    end;
  rflg := -1;
  if AP_FP_Greater_Eq(p, 0.5) then
    begin
      p := 1.0 - p;
      rflg := 1;
    end;
  z := InvIncompleteBeta(0.5 * rk, 0.5, 2.0 * p);
  if AP_FP_Less(MaxRealNumber * z, rk) then
    begin
      Result := rflg * MaxRealNumber;
      Exit;
    end;
  t := Sqrt(rk / z - rk);
  Result := rflg * t;
end;

(* ************************************************************************
  One-sample t-test

  This test checks three hypotheses about the mean of the given sample.  The
  following tests are performed:
  * two-tailed test (null hypothesis - the mean is equal to the given value)
  * left-tailed test (null hypothesis - the mean is greater than or equal to the given value)
  * right-tailed test (null hypothesis - the mean is less than or equal to the given value).

  The test is based on the assumption that  a  given  sample  has  a  normal
  distribution and  an  unknown  dispersion.  If  the  distribution  sharply
  differs from normal, the test will work incorrectly.

  Input parameters:
  X       -   sample. Array whose index goes from 0 to N-1.
  N       -   size of sample.
  Mean    -   assumed value of the mean.

  Output parameters:
  BothTails   -   p-value for two-tailed test. If BothTails is less than the given significance level the null hypothesis is rejected.
  LeftTail    -   p-value for left-tailed test. If LeftTail is less than the given significance level, the null hypothesis is rejected.
  RightTail   -   p-value for right-tailed test. If RightTail is less than the given significance level the null hypothesis is rejected.
  ************************************************************************ *)
procedure StudentTTest1(const x: TLVec; n: TLInt; Mean: TLFloat; var BothTails, LeftTail, RightTail: TLFloat);
var
  i        : TLInt;
  XMean    : TLFloat;
  XVariance: TLFloat;
  XStdDev  : TLFloat;
  v1       : TLFloat;
  v2       : TLFloat;
  Stat     : TLFloat;
  s        : TLFloat;
begin
  if n <= 1 then
    begin
      BothTails := 1.0;
      LeftTail := 1.0;
      RightTail := 1.0;
      Exit;
    end;

  //
  // Mean
  //
  XMean := 0;
  i := 0;
  while i <= n - 1 do
    begin
      XMean := XMean + x[i];
      inc(i);
    end;
  XMean := XMean / n;

  //
  // Variance (using corrected two-pass algorithm)
  //
  XVariance := 0;
  XStdDev := 0;
  if n <> 1 then
    begin
      v1 := 0;
      i := 0;
      while i <= n - 1 do
        begin
          v1 := v1 + AP_Sqr(x[i] - XMean);
          inc(i);
        end;
      v2 := 0;
      i := 0;
      while i <= n - 1 do
        begin
          v2 := v2 + (x[i] - XMean);
          inc(i);
        end;
      v2 := AP_Sqr(v2) / n;
      XVariance := (v1 - v2) / (n - 1);
      if AP_FP_Less(XVariance, 0) then
        begin
          XVariance := 0;
        end;
      XStdDev := Sqrt(XVariance);
    end;
  if AP_FP_Eq(XStdDev, 0) then
    begin
      BothTails := 1.0;
      LeftTail := 1.0;
      RightTail := 1.0;
      Exit;
    end;

  //
  // Statistic
  //
  Stat := (XMean - Mean) / (XStdDev / Sqrt(n));
  s := StudentTDistribution(n - 1, Stat);
  BothTails := 2 * Min(s, 1 - s);
  LeftTail := s;
  RightTail := 1 - s;
end;

(* ************************************************************************
  Two-sample pooled test

  This test checks three hypotheses about the mean of the given samples. The
  following tests are performed:
  * two-tailed test (null hypothesis - the means are equal)
  * left-tailed test (null hypothesis - the mean of the first sample  is greater than or equal to the mean of the second sample)
  * right-tailed test (null hypothesis - the mean of the first sample is less than or equal to the mean of the second sample).

  Test is based on the following assumptions:
  * given samples have normal distributions
  * dispersions are equal
  * samples are independent.

  Input parameters:
  X       -   sample 1. Array whose index goes from 0 to N-1.
  N       -   size of sample.
  Y       -   sample 2. Array whose index goes from 0 to M-1.
  M       -   size of sample.

  Output parameters:
  BothTails   -   p-value for two-tailed test. If BothTails is less than the given significance level the null hypothesis is rejected.
  LeftTail    -   p-value for left-tailed test. If LeftTail is less than the given significance level, the null hypothesis is rejected.
  RightTail   -   p-value for right-tailed test. If RightTail is less than the given significance level the null hypothesis is rejected.
  ************************************************************************ *)
procedure StudentTTest2(const x: TLVec; n: TLInt; const y: TLVec; M: TLInt; var BothTails, LeftTail, RightTail: TLFloat);
var
  i    : TLInt;
  XMean: TLFloat;
  YMean: TLFloat;
  Stat : TLFloat;
  s    : TLFloat;
  p    : TLFloat;
begin
  if (n <= 1) or (M <= 1) then
    begin
      BothTails := 1.0;
      LeftTail := 1.0;
      RightTail := 1.0;
      Exit;
    end;

  //
  // Mean
  //
  XMean := 0;
  i := 0;
  while i <= n - 1 do
    begin
      XMean := XMean + x[i];
      inc(i);
    end;
  XMean := XMean / n;
  YMean := 0;
  i := 0;
  while i <= M - 1 do
    begin
      YMean := YMean + y[i];
      inc(i);
    end;
  YMean := YMean / M;

  //
  // S
  //
  s := 0;
  i := 0;
  while i <= n - 1 do
    begin
      s := s + AP_Sqr(x[i] - XMean);
      inc(i);
    end;
  i := 0;
  while i <= M - 1 do
    begin
      s := s + AP_Sqr(y[i] - YMean);
      inc(i);
    end;
  s := Sqrt(s * (AP_Float(1) / n + AP_Float(1) / M) / (n + M - 2));
  if AP_FP_Eq(s, 0) then
    begin
      BothTails := 1.0;
      LeftTail := 1.0;
      RightTail := 1.0;
      Exit;
    end;

  //
  // Statistic
  //
  Stat := (XMean - YMean) / s;
  p := StudentTDistribution(n + M - 2, Stat);
  BothTails := 2 * Min(p, 1 - p);
  LeftTail := p;
  RightTail := 1 - p;
end;

(* ************************************************************************
  Two-sample unpooled test

  This test checks three hypotheses about the mean of the given samples. The
  following tests are performed:
  * two-tailed test (null hypothesis - the means are equal)
  * left-tailed test (null hypothesis - the mean of the first sample  is greater than or equal to the mean of the second sample)
  * right-tailed test (null hypothesis - the mean of the first sample is less than or equal to the mean of the second sample).

  Test is based on the following assumptions:
  * given samples have normal distributions
  * samples are independent.
  Dispersion equality is not required

  Input parameters:
  X - sample 1. Array whose index goes from 0 to N-1.
  N - size of the sample.
  Y - sample 2. Array whose index goes from 0 to M-1.
  M - size of the sample.

  Output parameters:
  BothTails   -   p-value for two-tailed test. If BothTails is less than the given significance level the null hypothesis is rejected.
  LeftTail    -   p-value for left-tailed test. If LeftTail is less than the given significance level, the null hypothesis is rejected.
  RightTail   -   p-value for right-tailed test. If RightTail is less than the given significance level the null hypothesis is rejected.
  ************************************************************************ *)
procedure UnequalVarianceTTest(const x: TLVec; n: TLInt; const y: TLVec; M: TLInt; var BothTails, LeftTail, RightTail: TLFloat);
var
  i    : TLInt;
  XMean: TLFloat;
  YMean: TLFloat;
  XVar : TLFloat;
  YVar : TLFloat;
  df   : TLFloat;
  p    : TLFloat;
  Stat : TLFloat;
  c    : TLFloat;
begin
  if (n <= 1) or (M <= 1) then
    begin
      BothTails := 1.0;
      LeftTail := 1.0;
      RightTail := 1.0;
      Exit;
    end;

  //
  // Mean
  //
  XMean := 0;
  i := 0;
  while i <= n - 1 do
    begin
      XMean := XMean + x[i];
      inc(i);
    end;
  XMean := XMean / n;
  YMean := 0;
  i := 0;
  while i <= M - 1 do
    begin
      YMean := YMean + y[i];
      inc(i);
    end;
  YMean := YMean / M;

  //
  // Variance (using corrected two-pass algorithm)
  //
  XVar := 0;
  i := 0;
  while i <= n - 1 do
    begin
      XVar := XVar + AP_Sqr(x[i] - XMean);
      inc(i);
    end;
  XVar := XVar / (n - 1);
  YVar := 0;
  i := 0;
  while i <= M - 1 do
    begin
      YVar := YVar + AP_Sqr(y[i] - YMean);
      inc(i);
    end;
  YVar := YVar / (M - 1);
  if AP_FP_Eq(XVar, 0) or AP_FP_Eq(YVar, 0) then
    begin
      BothTails := 1.0;
      LeftTail := 1.0;
      RightTail := 1.0;
      Exit;
    end;

  //
  // Statistic
  //
  Stat := (XMean - YMean) / Sqrt(XVar / n + YVar / M);
  c := XVar / n / (XVar / n + YVar / M);
  df := (n - 1) * (M - 1) / ((M - 1) * AP_Sqr(c) + (n - 1) * (1 - AP_Sqr(c)));
  if AP_FP_Greater(Stat, 0) then
    begin
      p := 1 - 0.5 * IncompleteBeta(df / 2, 0.5, df / (df + AP_Sqr(Stat)));
    end
  else
    begin
      p := 0.5 * IncompleteBeta(df / 2, 0.5, df / (df + AP_Sqr(Stat)));
    end;
  BothTails := 2 * Min(p, 1 - p);
  LeftTail := p;
  RightTail := 1 - p;
end; 
 
 
