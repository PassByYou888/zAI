{ ****************************************************************************** }
{ * F distribution support, by QQ 600585@qq.com                                * }
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
  F distribution

  Returns the area from zero to x under the F density
  function (also known as Snedcor's density or the
  variance ratio density).  This is the density
  of x = (u1/df1)/(u2/df2), where u1 and u2 are random
  variables having Chi square distributions with df1
  and df2 degrees of freedom, respectively.
  The incomplete beta integral is used, according to the
  formula

  P(x) = incbet( df1/2, df2/2, (df1*x/(df2 + df1*x) ).


  The arguments a and b are greater than zero, and x is
  nonnegative.

  ACCURACY:

  Tested at random points (a,b,x).

  x     a,b                     Relative error:
  arithmetic  domain  domain     # trials      peak         rms
  IEEE      0,1    0,100       100000      9.8e-15     1.7e-15
  IEEE      1,5    0,100       100000      6.5e-15     3.5e-16
  IEEE      0,1    1,10000     100000      2.2e-11     3.3e-12
  IEEE      1,5    1,10000     100000      1.1e-11     1.7e-13

  Cephes Math Library Release 2.8:  June, 2000
  Copyright 1984, 1987, 1995, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function FDistribution(const a: TLInt; const b: TLInt; const x: TLFloat): TLFloat;
var
  w: TLFloat;
begin
  Assert((a >= 1) and (b >= 1) and AP_FP_Greater_Eq(x, 0), 'Domain error in FDistribution');
  w := a * x;
  w := w / (b + w);
  Result := IncompleteBeta(0.5 * a, 0.5 * b, w);
end;

(* ************************************************************************
  * Complemented F distribution
  *
  * Returns the area from x to infinity under the F density
  * function (also known as Snedcor's density or the
  * variance ratio density).
  *
  *
  *                      inf.
  *                       -
  *              1       | |  a-1      b-1
  * 1-P(x)  =  ------    |   t    (1-t)    dt
  *            B(a,b)  | |
  *                     -
  *                      x
  *
  *
  * The incomplete beta integral is used, according to the
  * formula
  *
  * P(x) = incbet( df2/2, df1/2, (df2/(df2 + df1*x) ).
  *
  *
  * ACCURACY:
  *
  * Tested at random points (a,b,x) in the indicated intervals.
  *                x     a,b                     Relative error:
  * arithmetic  domain  domain     # trials      peak         rms
  *    IEEE      0,1    1,100       100000      3.7e-14     5.9e-16
  *    IEEE      1,5    1,100       100000      8.0e-15     1.6e-15
  *    IEEE      0,1    1,10000     100000      1.8e-11     3.5e-13
  *    IEEE      1,5    1,10000     100000      2.0e-11     3.0e-12
  *
  * Cephes Math Library Release 2.8:  June, 2000
  * Copyright 1984, 1987, 1995, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function FCDistribution(const a: TLInt; const b: TLInt; const x: TLFloat): TLFloat;
var
  w: TLFloat;
begin
  Assert((a >= 1) and (b >= 1) and AP_FP_Greater_Eq(x, 0), 'Domain error in FCDistribution');
  w := b / (b + a * x);
  Result := IncompleteBeta(0.5 * b, 0.5 * a, w);
end;

(* ************************************************************************
  Inverse of complemented F distribution

  Finds the F density argument x such that the integral
  from x to infinity of the F density is equal to the
  given probability p.

  This is accomplished using the inverse beta integral
  function and the relations

  z = incbi( df2/2, df1/2, p )
  x = df2 (1-z) / (df1 z).

  Note: the following relations hold for the inverse of
  the uncomplemented F distribution:

  z = incbi( df1/2, df2/2, p )
  x = df2 z / (df1 (1-z)).

  ACCURACY:

  Tested at random points (a,b,p).

  a,b                     Relative error:
  arithmetic  domain     # trials      peak         rms
  For p between .001 and 1:
  IEEE     1,100       100000      8.3e-15     4.7e-16
  IEEE     1,10000     100000      2.1e-11     1.4e-13
  For p between 10^-6 and 10^-3:
  IEEE     1,100        50000      1.3e-12     8.4e-15
  IEEE     1,10000      50000      3.0e-12     4.8e-14

  Cephes Math Library Release 2.8:  June, 2000
  Copyright 1984, 1987, 1995, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function InvFDistribution(const a: TLInt; const b: TLInt; const y: TLFloat): TLFloat;
var
  w: TLFloat;
begin
  Assert((a >= 1) and (b >= 1) and AP_FP_Greater(y, 0) and AP_FP_Less_Eq(y, 1), 'Domain error in InvFDistribution');

  //
  // Compute probability for x = 0.5
  //
  w := IncompleteBeta(0.5 * b, 0.5 * a, 0.5);

  //
  // If that is greater than y, then the solution w < .5
  // Otherwise, solve at 1-y to remove cancellation in (b - b*w)
  //
  if AP_FP_Greater(w, y) or AP_FP_Less(y, 0.001) then
    begin
      w := InvIncompleteBeta(0.5 * b, 0.5 * a, y);
      Result := (b - b * w) / (a * w);
    end
  else
    begin
      w := InvIncompleteBeta(0.5 * a, 0.5 * b, 1.0 - y);
      Result := b * w / (a * (1.0 - w));
    end;
end;

(* ************************************************************************
  Two-sample F-test

  This test checks three hypotheses about dispersions of the given samples.
  The following tests are performed:
  * two-tailed test (null hypothesis - the dispersions are equal)
  * left-tailed test (null hypothesis - the dispersion of the first sample is greater than or equal to the dispersion of the second sample).
  * right-tailed test (null hypothesis - the dispersion of the first sample is less than or equal to the dispersion of the second sample)

  The test is based on the following assumptions:
  * the given samples have normal distributions
  * the samples are independent.

  Input parameters:
  X   -   sample 1. Array whose index goes from 0 to N-1.
  N   -   sample size.
  Y   -   sample 2. Array whose index goes from 0 to M-1.
  M   -   sample size.

  Output parameters:
  BothTails   -   p-value for two-tailed test. If BothTails is less than the given significance level the null hypothesis is rejected.
  LeftTail    -   p-value for left-tailed test. If LeftTail is less than the given significance level, the null hypothesis is rejected.
  RightTail   -   p-value for right-tailed test. If RightTail is less than the given significance level the null hypothesis is rejected.
  ************************************************************************ *)
procedure FTest(const x: TLVec; n: TLInt; const y: TLVec; M: TLInt; var BothTails, LeftTail, RightTail: TLFloat);
var
  i    : TLInt;
  XMean: TLFloat;
  YMean: TLFloat;
  XVar : TLFloat;
  YVar : TLFloat;
  DF1  : TLInt;
  DF2  : TLInt;
  Stat : TLFloat;
begin
  if (n <= 2) or (M <= 2) then
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
  DF1 := n - 1;
  DF2 := M - 1;
  Stat := Min(XVar / YVar, YVar / XVar);
  BothTails := 1 - (FDistribution(DF1, DF2, 1 / Stat) - FDistribution(DF1,
    DF2, Stat));
  LeftTail := FDistribution(DF1, DF2, XVar / YVar);
  RightTail := 1 - LeftTail;
end; 
 
 
