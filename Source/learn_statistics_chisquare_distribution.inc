{ ****************************************************************************** }
{ * Chi-square distribution support, by QQ 600585@qq.com                       * }
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
  *Chi-square distribution
  *
  *Returns the area under the left hand tail (from 0 to x)
  *of the Chi square probability density function with
  *v degrees of freedom.
  *
  *
  *                                  x
  *                                   -
  *                       1          | |  v/2-1  -t/2
  * P( x | v )   =   -----------     |   t      e     dt
  *                   v/2  -       | |
  *                  2    | (v/2)   -
  *                                  0
  *
  *where x is the Chi-square variable.
  *
  *The incomplete gamma integral is used, according to the
  *formula
  *
  *y = chdtr( v, x ) = igam( v/2.0, x/2.0 ).
  *
  *The arguments must both be positive.
  *
  *ACCURACY:
  *
  *See incomplete gamma function
  *
  *
  *Cephes Math Library Release 2.8:  June, 2000
  *Copyright 1984, 1987, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function ChiSquareDistribution(const v, x: TLFloat): TLFloat;
begin
  Assert(AP_FP_Greater_Eq(x, 0) and AP_FP_Greater_Eq(v, 1), 'Domain error in ChiSquareDistribution');
  Result := IncompleteGamma(v / 2.0, x / 2.0);
end;

(* ************************************************************************
  *Complemented Chi-square distribution
  *
  *Returns the area under the right hand tail (from x to
  *infinity) of the Chi square probability density function
  *with v degrees of freedom:
  *
  *                                 inf.
  *                                   -
  *                       1          | |  v/2-1  -t/2
  * P( x | v )   =   -----------     |   t      e     dt
  *                   v/2  -       | |
  *                  2    | (v/2)   -
  *                                  x
  *
  *where x is the Chi-square variable.
  *
  *The incomplete gamma integral is used, according to the
  *formula
  *
  *y = chdtr( v, x ) = igamc( v/2.0, x/2.0 ).
  *
  *The arguments must both be positive.
  *
  *ACCURACY:
  *
  *See incomplete gamma function
  *
  *Cephes Math Library Release 2.8:  June, 2000
  *Copyright 1984, 1987, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function ChiSquareCDistribution(const v, x: TLFloat): TLFloat;
begin
  Assert(AP_FP_Greater_Eq(x, 0) and AP_FP_Greater_Eq(v, 1), 'Domain error in ChiSquareDistributionC');
  Result := IncompleteGammaC(v / 2.0, x / 2.0);
end;

(* ************************************************************************
  Inverse of complemented Chi-square distribution

  Finds the Chi-square argument x such that the integral
  from x to infinity of the Chi-square density is equal
  to the given cumulative probability y.

  This is accomplished using the inverse gamma integral
  function and the relation

  x/2 = igami( df/2, y );

  ACCURACY:

  See inverse incomplete gamma function


  Cephes Math Library Release 2.8:  June, 2000
  Copyright 1984, 1987, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function InvChiSquareDistribution(const v, y: TLFloat): TLFloat;
begin
  Assert(AP_FP_Greater_Eq(y, 0) and AP_FP_Less_Eq(y, 1) and AP_FP_Greater_Eq(v, 1), 'Domain error in InvChiSquareDistribution');
  Result := 2 * InvIncompleteGammaC(0.5 * v, y);
end;

(* ************************************************************************
  One-sample chi-square test

  This test checks three hypotheses about the dispersion of the given sample
  The following tests are performed:
  * two-tailed test (null hypothesis - the dispersion equals the given number)
  * left-tailed test (null hypothesis - the dispersion is greater than or equal to the given number)
  * right-tailed test (null hypothesis - dispersion is less than or equal to the given number).

  Test is based on the following assumptions:
  * the given sample has a normal distribution.

  Input parameters:
  X           -   sample 1. Array whose index goes from 0 to N-1.
  N           -   size of the sample.
  Variance    -   dispersion value to compare with.

  Output parameters:
  BothTails   -   p-value for two-tailed test. If BothTails is less than the given significance level the null hypothesis is rejected.
  LeftTail    -   p-value for left-tailed test. If LeftTail is less than the given significance level, the null hypothesis is rejected.
  RightTail   -   p-value for right-tailed test. If RightTail is less than the given significance level the null hypothesis is rejected.
  ************************************************************************ *)
procedure OneSampleVarianceTest(const x: TLVec; n: TLInt; Variance: TLFloat; var BothTails, LeftTail, RightTail: TLFloat);
var
  i    : TLInt;
  XMean: TLFloat;
  XVar : TLFloat;
  s    : TLFloat;
  Stat : TLFloat;
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
  // Variance
  //
  XVar := 0;
  i := 0;
  while i <= n - 1 do
    begin
      XVar := XVar + AP_Sqr(x[i] - XMean);
      inc(i);
    end;
  XVar := XVar / (n - 1);
  if AP_FP_Eq(XVar, 0) then
    begin
      BothTails := 1.0;
      LeftTail := 1.0;
      RightTail := 1.0;
      Exit;
    end;

  //
  // Statistic
  //
  Stat := (n - 1) * XVar / Variance;
  s := ChiSquareDistribution(n - 1, Stat);
  BothTails := 2 * Min(s, 1 - s);
  LeftTail := s;
  RightTail := 1 - LeftTail;
end; 
 
 
