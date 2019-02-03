{ ****************************************************************************** }
{ * Normal distribution support, by QQ 600585@qq.com                           * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

function Erf(const x: TLFloat): TLFloat; forward;
function ErfC(const x: TLFloat): TLFloat; forward;
function InvErf(const E: TLFloat): TLFloat; forward;

(* ************************************************************************
  * Error function
  *
  * The integral is
  *
  *                           x
  *                            -
  *                 2         | |          2
  *   erf(x)  =  --------     |    exp( - t  ) dt.
  *              sqrt(pi)   | |
  *                          -
  *                           0
  *
  * For 0 <= |x| < 1, erf(x) = x * P4(x**2)/Q5(x**2); otherwise
  * erf(x) = 1 - erfc(x).
  *
  *
  * ACCURACY:
  *
  *                      Relative error:
  * arithmetic   domain     # trials      peak         rms
  *    IEEE      0,1         30000       3.7e-16     1.0e-16
  *
  * Cephes Math Library Release 2.8:  June, 2000
  * Copyright 1984, 1987, 1988, 1992, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function Erf(const x: TLFloat): TLFloat;
var
  Nx: TLFloat;
  XSq: TLFloat;
  s: TLFloat;
  p: TLFloat;
  q: TLFloat;
begin
  Nx := AbsReal(x);
  s := Sign(Nx);
  if AP_FP_Less(Nx, 0.5) then
    begin
      XSq := Nx * Nx;
      p := 0.007547728033418631287834;
      p := 0.288805137207594084924010 + XSq * p;
      p := 14.3383842191748205576712 + XSq * p;
      p := 38.0140318123903008244444 + XSq * p;
      p := 3017.82788536507577809226 + XSq * p;
      p := 7404.07142710151470082064 + XSq * p;
      p := 80437.3630960840172832162 + XSq * p;
      q := 0.0;
      q := 1.00000000000000000000000 + XSq * q;
      q := 38.0190713951939403753468 + XSq * q;
      q := 658.070155459240506326937 + XSq * q;
      q := 6379.60017324428279487120 + XSq * q;
      q := 34216.5257924628539769006 + XSq * q;
      q := 80437.3630960840172826266 + XSq * q;
      Result := s * 1.1283791670955125738961589031 * Nx * p / q;
      Exit;
    end;
  if AP_FP_Greater_Eq(Nx, 10) then
    begin
      Result := s;
      Exit;
    end;
  Result := s * (1 - ErfC(Nx));
end;

(* ************************************************************************
  * Complementary error function
  *
  *  1 - erf(x) =
  *
  *                           inf.
  *                             -
  *                  2         | |          2
  *   erfc(x)  =  --------     |    exp( - t  ) dt
  *               sqrt(pi)   | |
  *                           -
  *                            x
  *
  *
  * For small x, erfc(x) = 1 - erf(x); otherwise rational
  * approximations are computed.
  *
  *
  * ACCURACY:
  *
  *                      Relative error:
  * arithmetic   domain     # trials      peak         rms
  *    IEEE      0,26.6417   30000       5.7e-14     1.5e-14
  *
  * Cephes Math Library Release 2.8:  June, 2000
  * Copyright 1984, 1987, 1988, 1992, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function ErfC(const x: TLFloat): TLFloat;
var
  p: TLFloat;
  q: TLFloat;
begin
  if AP_FP_Less(x, 0) then
    begin
      Result := 2 - ErfC(-x);
      Exit;
    end;
  if AP_FP_Less(x, 0.5) then
    begin
      Result := 1.0 - Erf(x);
      Exit;
    end;
  if AP_FP_Greater_Eq(x, 10) then
    begin
      Result := 0;
      Exit;
    end;
  p := 0.0;
  p := 0.5641877825507397413087057563 + x * p;
  p := 9.675807882987265400604202961 + x * p;
  p := 77.08161730368428609781633646 + x * p;
  p := 368.5196154710010637133875746 + x * p;
  p := 1143.262070703886173606073338 + x * p;
  p := 2320.439590251635247384768711 + x * p;
  p := 2898.0293292167655611275846 + x * p;
  p := 1826.3348842295112592168999 + x * p;
  q := 1.0;
  q := 17.14980943627607849376131193 + x * q;
  q := 137.1255960500622202878443578 + x * q;
  q := 661.7361207107653469211984771 + x * q;
  q := 2094.384367789539593790281779 + x * q;
  q := 4429.612803883682726711528526 + x * q;
  q := 6089.5424232724435504633068 + x * q;
  q := 4958.82756472114071495438422 + x * q;
  q := 1826.3348842295112595576438 + x * q;
  Result := Exp(-AP_Sqr(x)) * p / q;
end;

(* ************************************************************************
  * Normal distribution function
  *
  * Returns the area under the Gaussian probability density
  * function, integrated from minus infinity to x:
  *
  *                            x
  *                             -
  *                   1        | |          2
  *    ndtr(x)  = ---------    |    exp( - t /2 ) dt
  *               sqrt(2pi)  | |
  *                           -
  *                          -inf.
  *
  *             =  ( 1 + erf(z) ) / 2
  *             =  erfc(z) / 2
  *
  * where z = x/sqrt(2). Computation is via the functions
  * erf and erfc.
  *
  *
  * ACCURACY:
  *
  *                      Relative error:
  * arithmetic   domain     # trials      peak         rms
  *    IEEE     -13,0        30000       3.4e-14     6.7e-15
  *
  * Cephes Math Library Release 2.8:  June, 2000
  * Copyright 1984, 1987, 1988, 1992, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function NormalDistribution(const x: TLFloat): TLFloat;
begin
  Result := 0.5 * (Erf(x / 1.41421356237309504880) + 1);
end;

(* ************************************************************************
  Inverse of the error function

  Cephes Math Library Release 2.8:  June, 2000
  Copyright 1984, 1987, 1988, 1992, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function InvErf(const E: TLFloat): TLFloat;
begin
  Result := InvNormalDistribution(0.5 * (E + 1)) / Sqrt(2);
end;

(* ************************************************************************
  * Inverse of Normal distribution function
  *
  * Returns the argument, x, for which the area under the
  * Gaussian probability density function (integrated from
  * minus infinity to x) is equal to y.
  *
  *
  * For small arguments 0 < y < exp(-2), the program computes
  * z = sqrt( -2.0 * log(y) );  then the approximation is
  * x = z - log(z)/z  - (1/z) P(1/z) / Q(1/z).
  * There are two rational functions P/Q, one for 0 < y < exp(-32)
  * and the other for y up to exp(-2).  For larger arguments,
  * w = y - 0.5, and  x/sqrt(2pi) = w + w**3 R(w**2)/S(w**2)).
  *
  * ACCURACY:
  *
  * Relative error:
  * arithmetic   domain        # trials      peak         rms
  * IEEE     0.125, 1        20000       7.2e-16     1.3e-16
  * IEEE     3e-308, 0.135   50000       4.6e-16     9.8e-17
  *
  * Cephes Math Library Release 2.8:  June, 2000
  * Copyright 1984, 1987, 1988, 1992, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function InvNormalDistribution(const y0: TLFloat): TLFloat;
var
  Expm2: TLFloat;
  S2Pi: TLFloat;
  x: TLFloat;
  y: TLFloat;
  z: TLFloat;
  y2: TLFloat;
  x0: TLFloat;
  x1: TLFloat;
  Code: TLInt;
  P0: TLFloat;
  Q0: TLFloat;
  p1: TLFloat;
  q1: TLFloat;
  p2: TLFloat;
  q2: TLFloat;
begin
  Expm2 := 0.13533528323661269189;
  S2Pi := 2.50662827463100050242;
  if AP_FP_Less_Eq(y0, 0) then
    begin
      Result := -MaxRealNumber;
      Exit;
    end;
  if AP_FP_Greater_Eq(y0, 1) then
    begin
      Result := MaxRealNumber;
      Exit;
    end;
  Code := 1;
  y := y0;
  if AP_FP_Greater(y, 1.0 - Expm2) then
    begin
      y := 1.0 - y;
      Code := 0;
    end;
  if AP_FP_Greater(y, Expm2) then
    begin
      y := y - 0.5;
      y2 := y * y;
      P0 := -59.9633501014107895267;
      P0 := 98.0010754185999661536 + y2 * P0;
      P0 := -56.6762857469070293439 + y2 * P0;
      P0 := 13.9312609387279679503 + y2 * P0;
      P0 := -1.23916583867381258016 + y2 * P0;
      Q0 := 1;
      Q0 := 1.95448858338141759834 + y2 * Q0;
      Q0 := 4.67627912898881538453 + y2 * Q0;
      Q0 := 86.3602421390890590575 + y2 * Q0;
      Q0 := -225.462687854119370527 + y2 * Q0;
      Q0 := 200.260212380060660359 + y2 * Q0;
      Q0 := -82.0372256168333339912 + y2 * Q0;
      Q0 := 15.9056225126211695515 + y2 * Q0;
      Q0 := -1.18331621121330003142 + y2 * Q0;
      x := y + y * y2 * P0 / Q0;
      x := x * S2Pi;
      Result := x;
      Exit;
    end;
  x := Sqrt(-2.0 * ln(y));
  x0 := x - ln(x) / x;
  z := 1.0 / x;
  if AP_FP_Less(x, 8.0) then
    begin
      p1 := 4.05544892305962419923;
      p1 := 31.5251094599893866154 + z * p1;
      p1 := 57.1628192246421288162 + z * p1;
      p1 := 44.0805073893200834700 + z * p1;
      p1 := 14.6849561928858024014 + z * p1;
      p1 := 2.18663306850790267539 + z * p1;
      p1 := -1.40256079171354495875 * 0.1 + z * p1;
      p1 := -3.50424626827848203418 * 0.01 + z * p1;
      p1 := -8.57456785154685413611 * 0.0001 + z * p1;
      q1 := 1;
      q1 := 15.7799883256466749731 + z * q1;
      q1 := 45.3907635128879210584 + z * q1;
      q1 := 41.3172038254672030440 + z * q1;
      q1 := 15.0425385692907503408 + z * q1;
      q1 := 2.50464946208309415979 + z * q1;
      q1 := -1.42182922854787788574 * 0.1 + z * q1;
      q1 := -3.80806407691578277194 * 0.01 + z * q1;
      q1 := -9.33259480895457427372 * 0.0001 + z * q1;
      x1 := z * p1 / q1;
    end
  else
    begin
      p2 := 3.23774891776946035970;
      p2 := 6.91522889068984211695 + z * p2;
      p2 := 3.93881025292474443415 + z * p2;
      p2 := 1.33303460815807542389 + z * p2;
      p2 := 2.01485389549179081538 * 0.1 + z * p2;
      p2 := 1.23716634817820021358 * 0.01 + z * p2;
      p2 := 3.01581553508235416007 * 0.0001 + z * p2;
      p2 := 2.65806974686737550832 * 0.000001 + z * p2;
      p2 := 6.23974539184983293730 * 0.000000001 + z * p2;
      q2 := 1;
      q2 := 6.02427039364742014255 + z * q2;
      q2 := 3.67983563856160859403 + z * q2;
      q2 := 1.37702099489081330271 + z * q2;
      q2 := 2.16236993594496635890 * 0.1 + z * q2;
      q2 := 1.34204006088543189037 * 0.01 + z * q2;
      q2 := 3.28014464682127739104 * 0.0001 + z * q2;
      q2 := 2.89247864745380683936 * 0.000001 + z * q2;
      q2 := 6.79019408009981274425 * 0.000000001 + z * q2;
      x1 := z * p2 / q2;
    end;
  x := x0 - x1;
  if Code <> 0 then
    begin
      x := -x;
    end;
  Result := x;
end; 
 
 
