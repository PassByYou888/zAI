{ ****************************************************************************** }
{ * Binomial distribution support, by QQ 600585@qq.com                         * }
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
  *Binomial distribution
  *
  *Returns the sum of the terms 0 through k of the Binomial
  *probability density:
  *
  *  k
  *  --  ( n )   j      n-j
  *  >   (   )  p  (1-p)
  *  --  ( j )
  * j=0
  *
  *The terms are not summed directly; instead the incomplete
  *beta integral is employed, according to the formula
  *
  *y = bdtr( k, n, p ) = incbet( n-k, k+1, 1-p ).
  *
  *The arguments must be positive, with p ranging from 0 to 1.
  *
  *ACCURACY:
  *
  *Tested at random points (a,b,p), with p between 0 and 1.
  *
  *              a,b                     Relative error:
  *arithmetic  domain     # trials      peak         rms
  * For p between 0.001 and 1:
  *   IEEE     0,100       100000      4.3e-15     2.6e-16
  *
  *Cephes Math Library Release 2.8:  June, 2000
  *Copyright 1984, 1987, 1995, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function BinomialDistribution(const k, n: TLInt; const p: TLFloat): TLFloat;
var
  dk: TLFloat;
  dn: TLFloat;
begin
  Assert(AP_FP_Greater_Eq(p, 0) and AP_FP_Less_Eq(p, 1), 'Domain error in BinomialDistribution');
  Assert((k >= -1) and (k <= n), 'Domain error in BinomialDistribution');
  if k = -1 then
    begin
      Result := 0;
      Exit;
    end;
  if k = n then
    begin
      Result := 1;
      Exit;
    end;
  dn := n - k;
  if k = 0 then
    begin
      dk := Power(1.0 - p, dn);
    end
  else
    begin
      dk := k + 1;
      dk := IncompleteBeta(dn, dk, 1.0 - p);
    end;
  Result := dk;
end;

(* ************************************************************************
  *Complemented binomial distribution
  *
  *Returns the sum of the terms k+1 through n of the Binomial
  *probability density:
  *
  *  n
  *  --  ( n )   j      n-j
  *  >   (   )  p  (1-p)
  *  --  ( j )
  * j=k+1
  *
  *The terms are not summed directly; instead the incomplete
  *beta integral is employed, according to the formula
  *
  *y = bdtrc( k, n, p ) = incbet( k+1, n-k, p ).
  *
  *The arguments must be positive, with p ranging from 0 to 1.
  *
  *ACCURACY:
  *
  *Tested at random points (a,b,p).
  *
  *              a,b                     Relative error:
  *arithmetic  domain     # trials      peak         rms
  * For p between 0.001 and 1:
  *   IEEE     0,100       100000      6.7e-15     8.2e-16
  * For p between 0 and .001:
  *   IEEE     0,100       100000      1.5e-13     2.7e-15
  *
  *Cephes Math Library Release 2.8:  June, 2000
  *Copyright 1984, 1987, 1995, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function BinomialCDistribution(const k, n: TLInt; const p: TLFloat): TLFloat;
var
  dk: TLFloat;
  dn: TLFloat;
begin
  Assert(AP_FP_Greater_Eq(p, 0) and AP_FP_Less_Eq(p, 1), 'Domain error in BinomialDistributionC');
  Assert((k >= -1) and (k <= n), 'Domain error in BinomialDistributionC');
  if k = -1 then
    begin
      Result := 1;
      Exit;
    end;
  if k = n then
    begin
      Result := 0;
      Exit;
    end;
  dn := n - k;
  if k = 0 then
    begin
      if AP_FP_Less(p, 0.01) then
        begin
          dk := -ExpM1(dn * Log1P(-p));
        end
      else
        begin
          dk := 1.0 - Power(1.0 - p, dn);
        end;
    end
  else
    begin
      dk := k + 1;
      dk := IncompleteBeta(dk, dn, p);
    end;
  Result := dk;
end;

(* ************************************************************************
  Inverse binomial distribution

  Finds the event probability p such that the sum of the
  terms 0 through k of the Binomial probability density
  is equal to the given cumulative probability y.

  This is accomplished using the inverse beta integral
  function and the relation

  1 - p = incbi( n-k, k+1, y ).

  ACCURACY:

  Tested at random points (a,b,p).

  a,b                     Relative error:
  arithmetic  domain     # trials      peak         rms
  For p between 0.001 and 1:
  IEEE     0,100       100000      2.3e-14     6.4e-16
  IEEE     0,10000     100000      6.6e-12     1.2e-13
  For p between 10^-6 and 0.001:
  IEEE     0,100       100000      2.0e-12     1.3e-14
  IEEE     0,10000     100000      1.5e-12     3.2e-14

  Cephes Math Library Release 2.8:  June, 2000
  Copyright 1984, 1987, 1995, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function InvBinomialDistribution(const k, n: TLInt; const y: TLFloat): TLFloat;
var
  dk: TLFloat;
  dn: TLFloat;
  p : TLFloat;
begin
  Assert((k >= 0) and (k < n), 'Domain error in InvBinomialDistribution');
  dn := n - k;
  if k = 0 then
    begin
      if AP_FP_Greater(y, 0.8) then
        begin
          p := -ExpM1(Log1P(y - 1.0) / dn);
        end
      else
        begin
          p := 1.0 - Power(y, 1.0 / dn);
        end;
    end
  else
    begin
      dk := k + 1;
      p := IncompleteBeta(dn, dk, 0.5);
      if AP_FP_Greater(p, 0.5) then
        begin
          p := InvIncompleteBeta(dk, dn, 1.0 - y);
        end
      else
        begin
          p := 1.0 - InvIncompleteBeta(dn, dk, y);
        end;
    end;
  Result := p;
end;

(* ************************************************************************
  Sign test

  This test checks three hypotheses about the median of the given sample.
  The following tests are performed:
  * two-tailed test (null hypothesis - the median is equal to the given value)
  * left-tailed test (null hypothesis - the median is greater than or equal to the given value)
  * right-tailed test (null hypothesis - the median is less than or equal to the given value)

  Requirements:
  * the scale of measurement should be ordinal, interval or ratio (i.e. the test could not be applied to nominal variables).

  The test is non-parametric and doesn't require distribution X to be normal

  Input parameters:
  X       -   sample. Array whose index goes from 0 to N-1.
  N       -   size of the sample.
  Median  -   assumed median value.

  Output parameters:
  BothTails   -   p-value for two-tailed test. If BothTails is less than the given significance level the null hypothesis is rejected.
  LeftTail    -   p-value for left-tailed test. If LeftTail is less than the given significance level, the null hypothesis is rejected.
  RightTail   -   p-value for right-tailed test. If RightTail is less than the given significance level the null hypothesis is rejected.

  While calculating p-values high-precision binomial distribution approximation is used, so significance levels have about 15 exact digits.
  ************************************************************************ *)
procedure OneSampleSignTest(const x: TLVec; n: TLInt; Median: TLFloat; var BothTails, LeftTail, RightTail: TLFloat);
var
  i    : TLInt;
  GTCnt: TLInt;
  NECnt: TLInt;
begin
  if n <= 1 then
    begin
      BothTails := 1.0;
      LeftTail := 1.0;
      RightTail := 1.0;
      Exit;
    end;

  //
  // Calculate:
  // GTCnt - count of x[i]>Median
  // NECnt - count of x[i]<>Median
  //
  GTCnt := 0;
  NECnt := 0;
  i := 0;
  while i <= n - 1 do
    begin
      if AP_FP_Greater(x[i], Median) then
        begin
          GTCnt := GTCnt + 1;
        end;
      if AP_FP_NEq(x[i], Median) then
        begin
          NECnt := NECnt + 1;
        end;
      inc(i);
    end;
  if NECnt = 0 then
    begin

      //
      // all x[i] are equal to Median.
      // So we can conclude that Median is a true median :)
      //
      BothTails := 0.0;
      LeftTail := 0.0;
      RightTail := 0.0;
      Exit;
    end;
  BothTails := 2 * BinomialDistribution(Min(GTCnt, NECnt - GTCnt), NECnt, 0.5);
  LeftTail := BinomialDistribution(GTCnt, NECnt, 0.5);
  RightTail := BinomialCDistribution(GTCnt - 1, NECnt, 0.5);
end; 
 
 
