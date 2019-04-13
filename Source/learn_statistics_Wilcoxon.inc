{ ****************************************************************************** }
{ *  Wilcoxon signed-rank test support, by QQ 600585@qq.com                    * }
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

procedure WCheb(const x, c: TLFloat; var TJ: TLFloat; var TJ1: TLFloat; var r: TLFloat); forward;
function W5(const s: TLFloat): TLFloat; forward;
function W6(const s: TLFloat): TLFloat; forward;
function W7(const s: TLFloat): TLFloat; forward;
function W8(const s: TLFloat): TLFloat; forward;
function W9(const s: TLFloat): TLFloat; forward;
function W10(const s: TLFloat): TLFloat; forward;
function W11(const s: TLFloat): TLFloat; forward;
function W12(const s: TLFloat): TLFloat; forward;
function W13(const s: TLFloat): TLFloat; forward;
function W14(const s: TLFloat): TLFloat; forward;
function W15(const s: TLFloat): TLFloat; forward;
function W16(const s: TLFloat): TLFloat; forward;
function W17(const s: TLFloat): TLFloat; forward;
function W18(const s: TLFloat): TLFloat; forward;
function W19(const s: TLFloat): TLFloat; forward;
function W20(const s: TLFloat): TLFloat; forward;
function W21(const s: TLFloat): TLFloat; forward;
function W22(const s: TLFloat): TLFloat; forward;
function W23(const s: TLFloat): TLFloat; forward;
function W24(const s: TLFloat): TLFloat; forward;
function W25(const s: TLFloat): TLFloat; forward;
function W26(const s: TLFloat): TLFloat; forward;
function W27(const s: TLFloat): TLFloat; forward;
function W28(const s: TLFloat): TLFloat; forward;
function W29(const s: TLFloat): TLFloat; forward;
function W30(const s: TLFloat): TLFloat; forward;
function W40(const s: TLFloat): TLFloat; forward;
function W60(const s: TLFloat): TLFloat; forward;
function W120(const s: TLFloat): TLFloat; forward;
function W200(const s: TLFloat): TLFloat; forward;
function WSigma(const s: TLFloat; const n: TLInt): TLFloat; forward;

(* ************************************************************************
  Wilcoxon signed-rank test

  This test checks three hypotheses about the median of the given sample.
  The following tests are performed:
  * two-tailed test (null hypothesis - the median is equal to the given value)
  * left-tailed test (null hypothesis - the median is greater than or equal to the given value)
  * right-tailed test (null hypothesis - the median is less than or equal to the given value)

  Requirements:
  * the scale of measurement should be ordinal, interval or ratio (i.e. the test could not be applied to nominal variables).
  * the distribution should be continuous and symmetric relative to its median.
  * number of distinct values in the X array should be greater than 4

  The test is non-parametric and doesn't require distribution X to be normal

  Input parameters:
  X       -   sample. Array whose index goes from 0 to N-1.
  N       -   size of the sample.
  E       -   assumed median value.

  Output parameters:
  BothTails   -   p-value for two-tailed test. If BothTails is less than the given significance level the null hypothesis is rejected.
  LeftTail    -   p-value for left-tailed test. If LeftTail is less than the given significance level, the null hypothesis is rejected.
  RightTail   -   p-value for right-tailed test. If RightTail is less than the given significance level the null hypothesis is rejected.

  To calculate p-values, special approximation is used. This method lets us calculate p-values with two decimal places in interval [0.0001, 1].
  "Two decimal places" does not sound very impressive, but in practice the relative error of less than 1% is enough to make a decision.
  There is no approximation outside the [0.0001, 1] interval. Therefore,  if the significance level outlies this interval, the test returns 0.0001.
  ************************************************************************ *)
procedure WilcoxonSignedRankTest(const x: TLVec; n: TLInt; E: TLFloat; var BothTails, LeftTail, RightTail: TLFloat);
var
  i: TLInt;
  j: TLInt;
  k: TLInt;
  t: TLInt;
  tmp: TLFloat;
  TmpI: TLInt;
  ns: TLInt;
  r: TLVec;
  c: TLIVec;
  w: TLFloat;
  p: TLFloat;
  MP: TLFloat;
  s: TLFloat;
  SIGMA: TLFloat;
  MU: TLFloat;
begin
  //
  // Prepare
  //
  if n < 5 then
    begin
      BothTails := 1.0;
      LeftTail := 1.0;
      RightTail := 1.0;
      Exit;
    end;
  ns := 0;
  i := 0;
  while i <= n - 1 do
    begin
      if AP_FP_Eq(x[i], E) then
        begin
          inc(i);
          Continue;
        end;
      x[ns] := x[i];
      ns := ns + 1;
      inc(i);
    end;
  if ns < 5 then
    begin
      BothTails := 1.0;
      LeftTail := 1.0;
      RightTail := 1.0;
      Exit;
    end;
  SetLength(r, ns  );
  SetLength(c, ns  );
  i := 0;
  while i <= ns - 1 do
    begin
      r[i] := AbsReal(x[i] - E);
      c[i] := i;
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
      i := j;
    end;

  //
  // Compute W+
  //
  w := 0;
  i := 0;
  while i <= ns - 1 do
    begin
      if AP_FP_Greater(x[c[i]], E) then
        begin
          w := w + r[i];
        end;
      inc(i);
    end;

  //
  // Result
  //
  MU := AP_Float(ns * (ns + 1)) / 4;
  SIGMA := Sqrt(AP_Float(ns * (ns + 1) * (2 * ns + 1)) / 24);
  s := (w - MU) / SIGMA;
  if AP_FP_Less_Eq(s, 0) then
    begin
      p := Exp(WSigma(-(w - MU) / SIGMA, ns));
      MP := 1 - Exp(WSigma(-(w - 1 - MU) / SIGMA, ns));
    end
  else
    begin
      MP := Exp(WSigma((w - MU) / SIGMA, ns));
      p := 1 - Exp(WSigma((w + 1 - MU) / SIGMA, ns));
    end;
  BothTails := Max(2 * Min(p, MP), 1.0E-4);
  LeftTail := Max(p, 1.0E-4);
  RightTail := Max(MP, 1.0E-4);
end;

(* ************************************************************************
  Sequential Chebyshev interpolation.
  ************************************************************************ *)
procedure WCheb(const x, c: TLFloat; var TJ: TLFloat; var TJ1: TLFloat; var r: TLFloat);
var
  t: TLFloat;
begin
  r := r + c * TJ;
  t := 2 * x * TJ1 - TJ;
  TJ := TJ1;
  TJ1 := t;
end;

(* ************************************************************************
  Tail(S, 5)
  ************************************************************************ *)
function W5(const s: TLFloat): TLFloat;
var
  w: TLInt;
  r: TLFloat;
begin
  w := Round(-3.708099E+00 * s + 7.500000E+00);
  if w >= 7 then
    begin
      r := -6.931E-01;
    end;
  if w = 6 then
    begin
      r := -9.008E-01;
    end;
  if w = 5 then
    begin
      r := -1.163E+00;
    end;
  if w = 4 then
    begin
      r := -1.520E+00;
    end;
  if w = 3 then
    begin
      r := -1.856E+00;
    end;
  if w = 2 then
    begin
      r := -2.367E+00;
    end;
  if w = 1 then
    begin
      r := -2.773E+00;
    end;
  if w <= 0 then
    begin
      r := -3.466E+00;
    end;
  Result := r;
end;

(* ************************************************************************
  Tail(S, 6)
  ************************************************************************ *)
function W6(const s: TLFloat): TLFloat;
var
  w: TLInt;
  r: TLFloat;
begin
  w := Round(-4.769696E+00 * s + 1.050000E+01);
  if w >= 10 then
    begin
      r := -6.931E-01;
    end;
  if w = 9 then
    begin
      r := -8.630E-01;
    end;
  if w = 8 then
    begin
      r := -1.068E+00;
    end;
  if w = 7 then
    begin
      r := -1.269E+00;
    end;
  if w = 6 then
    begin
      r := -1.520E+00;
    end;
  if w = 5 then
    begin
      r := -1.856E+00;
    end;
  if w = 4 then
    begin
      r := -2.213E+00;
    end;
  if w = 3 then
    begin
      r := -2.549E+00;
    end;
  if w = 2 then
    begin
      r := -3.060E+00;
    end;
  if w = 1 then
    begin
      r := -3.466E+00;
    end;
  if w <= 0 then
    begin
      r := -4.159E+00;
    end;
  Result := r;
end;

(* ************************************************************************
  Tail(S, 7)
  ************************************************************************ *)
function W7(const s: TLFloat): TLFloat;
var
  w: TLInt;
  r: TLFloat;
begin
  w := Round(-5.916080E+00 * s + 1.400000E+01);
  if w >= 14 then
    begin
      r := -6.325E-01;
    end;
  if w = 13 then
    begin
      r := -7.577E-01;
    end;
  if w = 12 then
    begin
      r := -9.008E-01;
    end;
  if w = 11 then
    begin
      r := -1.068E+00;
    end;
  if w = 10 then
    begin
      r := -1.241E+00;
    end;
  if w = 9 then
    begin
      r := -1.451E+00;
    end;
  if w = 8 then
    begin
      r := -1.674E+00;
    end;
  if w = 7 then
    begin
      r := -1.908E+00;
    end;
  if w = 6 then
    begin
      r := -2.213E+00;
    end;
  if w = 5 then
    begin
      r := -2.549E+00;
    end;
  if w = 4 then
    begin
      r := -2.906E+00;
    end;
  if w = 3 then
    begin
      r := -3.243E+00;
    end;
  if w = 2 then
    begin
      r := -3.753E+00;
    end;
  if w = 1 then
    begin
      r := -4.159E+00;
    end;
  if w <= 0 then
    begin
      r := -4.852E+00;
    end;
  Result := r;
end;

(* ************************************************************************
  Tail(S, 8)
  ************************************************************************ *)
function W8(const s: TLFloat): TLFloat;
var
  w: TLInt;
  r: TLFloat;
begin
  w := Round(-7.141428E+00 * s + 1.800000E+01);
  if w >= 18 then
    begin
      r := -6.399E-01;
    end;
  if w = 17 then
    begin
      r := -7.494E-01;
    end;
  if w = 16 then
    begin
      r := -8.630E-01;
    end;
  if w = 15 then
    begin
      r := -9.913E-01;
    end;
  if w = 14 then
    begin
      r := -1.138E+00;
    end;
  if w = 13 then
    begin
      r := -1.297E+00;
    end;
  if w = 12 then
    begin
      r := -1.468E+00;
    end;
  if w = 11 then
    begin
      r := -1.653E+00;
    end;
  if w = 10 then
    begin
      r := -1.856E+00;
    end;
  if w = 9 then
    begin
      r := -2.079E+00;
    end;
  if w = 8 then
    begin
      r := -2.326E+00;
    end;
  if w = 7 then
    begin
      r := -2.601E+00;
    end;
  if w = 6 then
    begin
      r := -2.906E+00;
    end;
  if w = 5 then
    begin
      r := -3.243E+00;
    end;
  if w = 4 then
    begin
      r := -3.599E+00;
    end;
  if w = 3 then
    begin
      r := -3.936E+00;
    end;
  if w = 2 then
    begin
      r := -4.447E+00;
    end;
  if w = 1 then
    begin
      r := -4.852E+00;
    end;
  if w <= 0 then
    begin
      r := -5.545E+00;
    end;
  Result := r;
end;

(* ************************************************************************
  Tail(S, 9)
  ************************************************************************ *)
function W9(const s: TLFloat): TLFloat;
var
  w: TLInt;
  r: TLFloat;
begin
  w := Round(-8.440972E+00 * s + 2.250000E+01);
  if w >= 22 then
    begin
      r := -6.931E-01;
    end;
  if w = 21 then
    begin
      r := -7.873E-01;
    end;
  if w = 20 then
    begin
      r := -8.912E-01;
    end;
  if w = 19 then
    begin
      r := -1.002E+00;
    end;
  if w = 18 then
    begin
      r := -1.120E+00;
    end;
  if w = 17 then
    begin
      r := -1.255E+00;
    end;
  if w = 16 then
    begin
      r := -1.394E+00;
    end;
  if w = 15 then
    begin
      r := -1.547E+00;
    end;
  if w = 14 then
    begin
      r := -1.717E+00;
    end;
  if w = 13 then
    begin
      r := -1.895E+00;
    end;
  if w = 12 then
    begin
      r := -2.079E+00;
    end;
  if w = 11 then
    begin
      r := -2.287E+00;
    end;
  if w = 10 then
    begin
      r := -2.501E+00;
    end;
  if w = 9 then
    begin
      r := -2.742E+00;
    end;
  if w = 8 then
    begin
      r := -3.019E+00;
    end;
  if w = 7 then
    begin
      r := -3.294E+00;
    end;
  if w = 6 then
    begin
      r := -3.599E+00;
    end;
  if w = 5 then
    begin
      r := -3.936E+00;
    end;
  if w = 4 then
    begin
      r := -4.292E+00;
    end;
  if w = 3 then
    begin
      r := -4.629E+00;
    end;
  if w = 2 then
    begin
      r := -5.140E+00;
    end;
  if w = 1 then
    begin
      r := -5.545E+00;
    end;
  if w <= 0 then
    begin
      r := -6.238E+00;
    end;
  Result := r;
end;

(* ************************************************************************
  Tail(S, 10)
  ************************************************************************ *)
function W10(const s: TLFloat): TLFloat;
var
  w: TLInt;
  r: TLFloat;
begin
  w := Round(-9.810708E+00 * s + 2.750000E+01);
  if w >= 27 then
    begin
      r := -6.931E-01;
    end;
  if w = 26 then
    begin
      r := -7.745E-01;
    end;
  if w = 25 then
    begin
      r := -8.607E-01;
    end;
  if w = 24 then
    begin
      r := -9.551E-01;
    end;
  if w = 23 then
    begin
      r := -1.057E+00;
    end;
  if w = 22 then
    begin
      r := -1.163E+00;
    end;
  if w = 21 then
    begin
      r := -1.279E+00;
    end;
  if w = 20 then
    begin
      r := -1.402E+00;
    end;
  if w = 19 then
    begin
      r := -1.533E+00;
    end;
  if w = 18 then
    begin
      r := -1.674E+00;
    end;
  if w = 17 then
    begin
      r := -1.826E+00;
    end;
  if w = 16 then
    begin
      r := -1.983E+00;
    end;
  if w = 15 then
    begin
      r := -2.152E+00;
    end;
  if w = 14 then
    begin
      r := -2.336E+00;
    end;
  if w = 13 then
    begin
      r := -2.525E+00;
    end;
  if w = 12 then
    begin
      r := -2.727E+00;
    end;
  if w = 11 then
    begin
      r := -2.942E+00;
    end;
  if w = 10 then
    begin
      r := -3.170E+00;
    end;
  if w = 9 then
    begin
      r := -3.435E+00;
    end;
  if w = 8 then
    begin
      r := -3.713E+00;
    end;
  if w = 7 then
    begin
      r := -3.987E+00;
    end;
  if w = 6 then
    begin
      r := -4.292E+00;
    end;
  if w = 5 then
    begin
      r := -4.629E+00;
    end;
  if w = 4 then
    begin
      r := -4.986E+00;
    end;
  if w = 3 then
    begin
      r := -5.322E+00;
    end;
  if w = 2 then
    begin
      r := -5.833E+00;
    end;
  if w = 1 then
    begin
      r := -6.238E+00;
    end;
  if w <= 0 then
    begin
      r := -6.931E+00;
    end;
  Result := r;
end;

(* ************************************************************************
  Tail(S, 11)
  ************************************************************************ *)
function W11(const s: TLFloat): TLFloat;
var
  w: TLInt;
  r: TLFloat;
begin
  w := Round(-1.124722E+01 * s + 3.300000E+01);
  if w >= 33 then
    begin
      r := -6.595E-01;
    end;
  if w = 32 then
    begin
      r := -7.279E-01;
    end;
  if w = 31 then
    begin
      r := -8.002E-01;
    end;
  if w = 30 then
    begin
      r := -8.782E-01;
    end;
  if w = 29 then
    begin
      r := -9.615E-01;
    end;
  if w = 28 then
    begin
      r := -1.050E+00;
    end;
  if w = 27 then
    begin
      r := -1.143E+00;
    end;
  if w = 26 then
    begin
      r := -1.243E+00;
    end;
  if w = 25 then
    begin
      r := -1.348E+00;
    end;
  if w = 24 then
    begin
      r := -1.459E+00;
    end;
  if w = 23 then
    begin
      r := -1.577E+00;
    end;
  if w = 22 then
    begin
      r := -1.700E+00;
    end;
  if w = 21 then
    begin
      r := -1.832E+00;
    end;
  if w = 20 then
    begin
      r := -1.972E+00;
    end;
  if w = 19 then
    begin
      r := -2.119E+00;
    end;
  if w = 18 then
    begin
      r := -2.273E+00;
    end;
  if w = 17 then
    begin
      r := -2.437E+00;
    end;
  if w = 16 then
    begin
      r := -2.607E+00;
    end;
  if w = 15 then
    begin
      r := -2.788E+00;
    end;
  if w = 14 then
    begin
      r := -2.980E+00;
    end;
  if w = 13 then
    begin
      r := -3.182E+00;
    end;
  if w = 12 then
    begin
      r := -3.391E+00;
    end;
  if w = 11 then
    begin
      r := -3.617E+00;
    end;
  if w = 10 then
    begin
      r := -3.863E+00;
    end;
  if w = 9 then
    begin
      r := -4.128E+00;
    end;
  if w = 8 then
    begin
      r := -4.406E+00;
    end;
  if w = 7 then
    begin
      r := -4.680E+00;
    end;
  if w = 6 then
    begin
      r := -4.986E+00;
    end;
  if w = 5 then
    begin
      r := -5.322E+00;
    end;
  if w = 4 then
    begin
      r := -5.679E+00;
    end;
  if w = 3 then
    begin
      r := -6.015E+00;
    end;
  if w = 2 then
    begin
      r := -6.526E+00;
    end;
  if w = 1 then
    begin
      r := -6.931E+00;
    end;
  if w <= 0 then
    begin
      r := -7.625E+00;
    end;
  Result := r;
end;

(* ************************************************************************
  Tail(S, 12)
  ************************************************************************ *)
function W12(const s: TLFloat): TLFloat;
var
  w: TLInt;
  r: TLFloat;
begin
  w := Round(-1.274755E+01 * s + 3.900000E+01);
  if w >= 39 then
    begin
      r := -6.633E-01;
    end;
  if w = 38 then
    begin
      r := -7.239E-01;
    end;
  if w = 37 then
    begin
      r := -7.878E-01;
    end;
  if w = 36 then
    begin
      r := -8.556E-01;
    end;
  if w = 35 then
    begin
      r := -9.276E-01;
    end;
  if w = 34 then
    begin
      r := -1.003E+00;
    end;
  if w = 33 then
    begin
      r := -1.083E+00;
    end;
  if w = 32 then
    begin
      r := -1.168E+00;
    end;
  if w = 31 then
    begin
      r := -1.256E+00;
    end;
  if w = 30 then
    begin
      r := -1.350E+00;
    end;
  if w = 29 then
    begin
      r := -1.449E+00;
    end;
  if w = 28 then
    begin
      r := -1.552E+00;
    end;
  if w = 27 then
    begin
      r := -1.660E+00;
    end;
  if w = 26 then
    begin
      r := -1.774E+00;
    end;
  if w = 25 then
    begin
      r := -1.893E+00;
    end;
  if w = 24 then
    begin
      r := -2.017E+00;
    end;
  if w = 23 then
    begin
      r := -2.148E+00;
    end;
  if w = 22 then
    begin
      r := -2.285E+00;
    end;
  if w = 21 then
    begin
      r := -2.429E+00;
    end;
  if w = 20 then
    begin
      r := -2.581E+00;
    end;
  if w = 19 then
    begin
      r := -2.738E+00;
    end;
  if w = 18 then
    begin
      r := -2.902E+00;
    end;
  if w = 17 then
    begin
      r := -3.076E+00;
    end;
  if w = 16 then
    begin
      r := -3.255E+00;
    end;
  if w = 15 then
    begin
      r := -3.443E+00;
    end;
  if w = 14 then
    begin
      r := -3.645E+00;
    end;
  if w = 13 then
    begin
      r := -3.852E+00;
    end;
  if w = 12 then
    begin
      r := -4.069E+00;
    end;
  if w = 11 then
    begin
      r := -4.310E+00;
    end;
  if w = 10 then
    begin
      r := -4.557E+00;
    end;
  if w = 9 then
    begin
      r := -4.821E+00;
    end;
  if w = 8 then
    begin
      r := -5.099E+00;
    end;
  if w = 7 then
    begin
      r := -5.373E+00;
    end;
  if w = 6 then
    begin
      r := -5.679E+00;
    end;
  if w = 5 then
    begin
      r := -6.015E+00;
    end;
  if w = 4 then
    begin
      r := -6.372E+00;
    end;
  if w = 3 then
    begin
      r := -6.708E+00;
    end;
  if w = 2 then
    begin
      r := -7.219E+00;
    end;
  if w = 1 then
    begin
      r := -7.625E+00;
    end;
  if w <= 0 then
    begin
      r := -8.318E+00;
    end;
  Result := r;
end;

(* ************************************************************************
  Tail(S, 13)
  ************************************************************************ *)
function W13(const s: TLFloat): TLFloat;
var
  w: TLInt;
  r: TLFloat;
begin
  w := Round(-1.430909E+01 * s + 4.550000E+01);
  if w >= 45 then
    begin
      r := -6.931E-01;
    end;
  if w = 44 then
    begin
      r := -7.486E-01;
    end;
  if w = 43 then
    begin
      r := -8.068E-01;
    end;
  if w = 42 then
    begin
      r := -8.683E-01;
    end;
  if w = 41 then
    begin
      r := -9.328E-01;
    end;
  if w = 40 then
    begin
      r := -1.001E+00;
    end;
  if w = 39 then
    begin
      r := -1.072E+00;
    end;
  if w = 38 then
    begin
      r := -1.146E+00;
    end;
  if w = 37 then
    begin
      r := -1.224E+00;
    end;
  if w = 36 then
    begin
      r := -1.306E+00;
    end;
  if w = 35 then
    begin
      r := -1.392E+00;
    end;
  if w = 34 then
    begin
      r := -1.481E+00;
    end;
  if w = 33 then
    begin
      r := -1.574E+00;
    end;
  if w = 32 then
    begin
      r := -1.672E+00;
    end;
  if w = 31 then
    begin
      r := -1.773E+00;
    end;
  if w = 30 then
    begin
      r := -1.879E+00;
    end;
  if w = 29 then
    begin
      r := -1.990E+00;
    end;
  if w = 28 then
    begin
      r := -2.104E+00;
    end;
  if w = 27 then
    begin
      r := -2.224E+00;
    end;
  if w = 26 then
    begin
      r := -2.349E+00;
    end;
  if w = 25 then
    begin
      r := -2.479E+00;
    end;
  if w = 24 then
    begin
      r := -2.614E+00;
    end;
  if w = 23 then
    begin
      r := -2.755E+00;
    end;
  if w = 22 then
    begin
      r := -2.902E+00;
    end;
  if w = 21 then
    begin
      r := -3.055E+00;
    end;
  if w = 20 then
    begin
      r := -3.215E+00;
    end;
  if w = 19 then
    begin
      r := -3.380E+00;
    end;
  if w = 18 then
    begin
      r := -3.551E+00;
    end;
  if w = 17 then
    begin
      r := -3.733E+00;
    end;
  if w = 16 then
    begin
      r := -3.917E+00;
    end;
  if w = 15 then
    begin
      r := -4.113E+00;
    end;
  if w = 14 then
    begin
      r := -4.320E+00;
    end;
  if w = 13 then
    begin
      r := -4.534E+00;
    end;
  if w = 12 then
    begin
      r := -4.762E+00;
    end;
  if w = 11 then
    begin
      r := -5.004E+00;
    end;
  if w = 10 then
    begin
      r := -5.250E+00;
    end;
  if w = 9 then
    begin
      r := -5.514E+00;
    end;
  if w = 8 then
    begin
      r := -5.792E+00;
    end;
  if w = 7 then
    begin
      r := -6.066E+00;
    end;
  if w = 6 then
    begin
      r := -6.372E+00;
    end;
  if w = 5 then
    begin
      r := -6.708E+00;
    end;
  if w = 4 then
    begin
      r := -7.065E+00;
    end;
  if w = 3 then
    begin
      r := -7.401E+00;
    end;
  if w = 2 then
    begin
      r := -7.912E+00;
    end;
  if w = 1 then
    begin
      r := -8.318E+00;
    end;
  if w <= 0 then
    begin
      r := -9.011E+00;
    end;
  Result := r;
end;

(* ************************************************************************
  Tail(S, 14)
  ************************************************************************ *)
function W14(const s: TLFloat): TLFloat;
var
  w: TLInt;
  r: TLFloat;
begin
  w := Round(-1.592953E+01 * s + 5.250000E+01);
  if w >= 52 then
    begin
      r := -6.931E-01;
    end;
  if w = 51 then
    begin
      r := -7.428E-01;
    end;
  if w = 50 then
    begin
      r := -7.950E-01;
    end;
  if w = 49 then
    begin
      r := -8.495E-01;
    end;
  if w = 48 then
    begin
      r := -9.067E-01;
    end;
  if w = 47 then
    begin
      r := -9.664E-01;
    end;
  if w = 46 then
    begin
      r := -1.029E+00;
    end;
  if w = 45 then
    begin
      r := -1.094E+00;
    end;
  if w = 44 then
    begin
      r := -1.162E+00;
    end;
  if w = 43 then
    begin
      r := -1.233E+00;
    end;
  if w = 42 then
    begin
      r := -1.306E+00;
    end;
  if w = 41 then
    begin
      r := -1.383E+00;
    end;
  if w = 40 then
    begin
      r := -1.463E+00;
    end;
  if w = 39 then
    begin
      r := -1.546E+00;
    end;
  if w = 38 then
    begin
      r := -1.632E+00;
    end;
  if w = 37 then
    begin
      r := -1.722E+00;
    end;
  if w = 36 then
    begin
      r := -1.815E+00;
    end;
  if w = 35 then
    begin
      r := -1.911E+00;
    end;
  if w = 34 then
    begin
      r := -2.011E+00;
    end;
  if w = 33 then
    begin
      r := -2.115E+00;
    end;
  if w = 32 then
    begin
      r := -2.223E+00;
    end;
  if w = 31 then
    begin
      r := -2.334E+00;
    end;
  if w = 30 then
    begin
      r := -2.450E+00;
    end;
  if w = 29 then
    begin
      r := -2.570E+00;
    end;
  if w = 28 then
    begin
      r := -2.694E+00;
    end;
  if w = 27 then
    begin
      r := -2.823E+00;
    end;
  if w = 26 then
    begin
      r := -2.956E+00;
    end;
  if w = 25 then
    begin
      r := -3.095E+00;
    end;
  if w = 24 then
    begin
      r := -3.238E+00;
    end;
  if w = 23 then
    begin
      r := -3.387E+00;
    end;
  if w = 22 then
    begin
      r := -3.541E+00;
    end;
  if w = 21 then
    begin
      r := -3.700E+00;
    end;
  if w = 20 then
    begin
      r := -3.866E+00;
    end;
  if w = 19 then
    begin
      r := -4.038E+00;
    end;
  if w = 18 then
    begin
      r := -4.215E+00;
    end;
  if w = 17 then
    begin
      r := -4.401E+00;
    end;
  if w = 16 then
    begin
      r := -4.592E+00;
    end;
  if w = 15 then
    begin
      r := -4.791E+00;
    end;
  if w = 14 then
    begin
      r := -5.004E+00;
    end;
  if w = 13 then
    begin
      r := -5.227E+00;
    end;
  if w = 12 then
    begin
      r := -5.456E+00;
    end;
  if w = 11 then
    begin
      r := -5.697E+00;
    end;
  if w = 10 then
    begin
      r := -5.943E+00;
    end;
  if w = 9 then
    begin
      r := -6.208E+00;
    end;
  if w = 8 then
    begin
      r := -6.485E+00;
    end;
  if w = 7 then
    begin
      r := -6.760E+00;
    end;
  if w = 6 then
    begin
      r := -7.065E+00;
    end;
  if w = 5 then
    begin
      r := -7.401E+00;
    end;
  if w = 4 then
    begin
      r := -7.758E+00;
    end;
  if w = 3 then
    begin
      r := -8.095E+00;
    end;
  if w = 2 then
    begin
      r := -8.605E+00;
    end;
  if w = 1 then
    begin
      r := -9.011E+00;
    end;
  if w <= 0 then
    begin
      r := -9.704E+00;
    end;
  Result := r;
end;

(* ************************************************************************
  Tail(S, 15)
  ************************************************************************ *)
function W15(const s: TLFloat): TLFloat;
var
  w: TLInt;
  r: TLFloat;
begin
  w := Round(-1.760682E+01 * s + 6.000000E+01);
  if w >= 60 then
    begin
      r := -6.714E-01;
    end;
  if w = 59 then
    begin
      r := -7.154E-01;
    end;
  if w = 58 then
    begin
      r := -7.613E-01;
    end;
  if w = 57 then
    begin
      r := -8.093E-01;
    end;
  if w = 56 then
    begin
      r := -8.593E-01;
    end;
  if w = 55 then
    begin
      r := -9.114E-01;
    end;
  if w = 54 then
    begin
      r := -9.656E-01;
    end;
  if w = 53 then
    begin
      r := -1.022E+00;
    end;
  if w = 52 then
    begin
      r := -1.081E+00;
    end;
  if w = 51 then
    begin
      r := -1.142E+00;
    end;
  if w = 50 then
    begin
      r := -1.205E+00;
    end;
  if w = 49 then
    begin
      r := -1.270E+00;
    end;
  if w = 48 then
    begin
      r := -1.339E+00;
    end;
  if w = 47 then
    begin
      r := -1.409E+00;
    end;
  if w = 46 then
    begin
      r := -1.482E+00;
    end;
  if w = 45 then
    begin
      r := -1.558E+00;
    end;
  if w = 44 then
    begin
      r := -1.636E+00;
    end;
  if w = 43 then
    begin
      r := -1.717E+00;
    end;
  if w = 42 then
    begin
      r := -1.801E+00;
    end;
  if w = 41 then
    begin
      r := -1.888E+00;
    end;
  if w = 40 then
    begin
      r := -1.977E+00;
    end;
  if w = 39 then
    begin
      r := -2.070E+00;
    end;
  if w = 38 then
    begin
      r := -2.166E+00;
    end;
  if w = 37 then
    begin
      r := -2.265E+00;
    end;
  if w = 36 then
    begin
      r := -2.366E+00;
    end;
  if w = 35 then
    begin
      r := -2.472E+00;
    end;
  if w = 34 then
    begin
      r := -2.581E+00;
    end;
  if w = 33 then
    begin
      r := -2.693E+00;
    end;
  if w = 32 then
    begin
      r := -2.809E+00;
    end;
  if w = 31 then
    begin
      r := -2.928E+00;
    end;
  if w = 30 then
    begin
      r := -3.051E+00;
    end;
  if w = 29 then
    begin
      r := -3.179E+00;
    end;
  if w = 28 then
    begin
      r := -3.310E+00;
    end;
  if w = 27 then
    begin
      r := -3.446E+00;
    end;
  if w = 26 then
    begin
      r := -3.587E+00;
    end;
  if w = 25 then
    begin
      r := -3.732E+00;
    end;
  if w = 24 then
    begin
      r := -3.881E+00;
    end;
  if w = 23 then
    begin
      r := -4.036E+00;
    end;
  if w = 22 then
    begin
      r := -4.195E+00;
    end;
  if w = 21 then
    begin
      r := -4.359E+00;
    end;
  if w = 20 then
    begin
      r := -4.531E+00;
    end;
  if w = 19 then
    begin
      r := -4.707E+00;
    end;
  if w = 18 then
    begin
      r := -4.888E+00;
    end;
  if w = 17 then
    begin
      r := -5.079E+00;
    end;
  if w = 16 then
    begin
      r := -5.273E+00;
    end;
  if w = 15 then
    begin
      r := -5.477E+00;
    end;
  if w = 14 then
    begin
      r := -5.697E+00;
    end;
  if w = 13 then
    begin
      r := -5.920E+00;
    end;
  if w = 12 then
    begin
      r := -6.149E+00;
    end;
  if w = 11 then
    begin
      r := -6.390E+00;
    end;
  if w = 10 then
    begin
      r := -6.636E+00;
    end;
  if w = 9 then
    begin
      r := -6.901E+00;
    end;
  if w = 8 then
    begin
      r := -7.178E+00;
    end;
  if w = 7 then
    begin
      r := -7.453E+00;
    end;
  if w = 6 then
    begin
      r := -7.758E+00;
    end;
  if w = 5 then
    begin
      r := -8.095E+00;
    end;
  if w = 4 then
    begin
      r := -8.451E+00;
    end;
  if w = 3 then
    begin
      r := -8.788E+00;
    end;
  if w = 2 then
    begin
      r := -9.299E+00;
    end;
  if w = 1 then
    begin
      r := -9.704E+00;
    end;
  if w <= 0 then
    begin
      r := -1.040E+01;
    end;
  Result := r;
end;

(* ************************************************************************
  Tail(S, 16)
  ************************************************************************ *)
function W16(const s: TLFloat): TLFloat;
var
  w: TLInt;
  r: TLFloat;
begin
  w := Round(-1.933908E+01 * s + 6.800000E+01);
  if w >= 68 then
    begin
      r := -6.733E-01;
    end;
  if w = 67 then
    begin
      r := -7.134E-01;
    end;
  if w = 66 then
    begin
      r := -7.551E-01;
    end;
  if w = 65 then
    begin
      r := -7.986E-01;
    end;
  if w = 64 then
    begin
      r := -8.437E-01;
    end;
  if w = 63 then
    begin
      r := -8.905E-01;
    end;
  if w = 62 then
    begin
      r := -9.391E-01;
    end;
  if w = 61 then
    begin
      r := -9.895E-01;
    end;
  if w = 60 then
    begin
      r := -1.042E+00;
    end;
  if w = 59 then
    begin
      r := -1.096E+00;
    end;
  if w = 58 then
    begin
      r := -1.152E+00;
    end;
  if w = 57 then
    begin
      r := -1.210E+00;
    end;
  if w = 56 then
    begin
      r := -1.270E+00;
    end;
  if w = 55 then
    begin
      r := -1.331E+00;
    end;
  if w = 54 then
    begin
      r := -1.395E+00;
    end;
  if w = 53 then
    begin
      r := -1.462E+00;
    end;
  if w = 52 then
    begin
      r := -1.530E+00;
    end;
  if w = 51 then
    begin
      r := -1.600E+00;
    end;
  if w = 50 then
    begin
      r := -1.673E+00;
    end;
  if w = 49 then
    begin
      r := -1.748E+00;
    end;
  if w = 48 then
    begin
      r := -1.825E+00;
    end;
  if w = 47 then
    begin
      r := -1.904E+00;
    end;
  if w = 46 then
    begin
      r := -1.986E+00;
    end;
  if w = 45 then
    begin
      r := -2.071E+00;
    end;
  if w = 44 then
    begin
      r := -2.158E+00;
    end;
  if w = 43 then
    begin
      r := -2.247E+00;
    end;
  if w = 42 then
    begin
      r := -2.339E+00;
    end;
  if w = 41 then
    begin
      r := -2.434E+00;
    end;
  if w = 40 then
    begin
      r := -2.532E+00;
    end;
  if w = 39 then
    begin
      r := -2.632E+00;
    end;
  if w = 38 then
    begin
      r := -2.735E+00;
    end;
  if w = 37 then
    begin
      r := -2.842E+00;
    end;
  if w = 36 then
    begin
      r := -2.951E+00;
    end;
  if w = 35 then
    begin
      r := -3.064E+00;
    end;
  if w = 34 then
    begin
      r := -3.179E+00;
    end;
  if w = 33 then
    begin
      r := -3.298E+00;
    end;
  if w = 32 then
    begin
      r := -3.420E+00;
    end;
  if w = 31 then
    begin
      r := -3.546E+00;
    end;
  if w = 30 then
    begin
      r := -3.676E+00;
    end;
  if w = 29 then
    begin
      r := -3.810E+00;
    end;
  if w = 28 then
    begin
      r := -3.947E+00;
    end;
  if w = 27 then
    begin
      r := -4.088E+00;
    end;
  if w = 26 then
    begin
      r := -4.234E+00;
    end;
  if w = 25 then
    begin
      r := -4.383E+00;
    end;
  if w = 24 then
    begin
      r := -4.538E+00;
    end;
  if w = 23 then
    begin
      r := -4.697E+00;
    end;
  if w = 22 then
    begin
      r := -4.860E+00;
    end;
  if w = 21 then
    begin
      r := -5.029E+00;
    end;
  if w = 20 then
    begin
      r := -5.204E+00;
    end;
  if w = 19 then
    begin
      r := -5.383E+00;
    end;
  if w = 18 then
    begin
      r := -5.569E+00;
    end;
  if w = 17 then
    begin
      r := -5.762E+00;
    end;
  if w = 16 then
    begin
      r := -5.960E+00;
    end;
  if w = 15 then
    begin
      r := -6.170E+00;
    end;
  if w = 14 then
    begin
      r := -6.390E+00;
    end;
  if w = 13 then
    begin
      r := -6.613E+00;
    end;
  if w = 12 then
    begin
      r := -6.842E+00;
    end;
  if w = 11 then
    begin
      r := -7.083E+00;
    end;
  if w = 10 then
    begin
      r := -7.329E+00;
    end;
  if w = 9 then
    begin
      r := -7.594E+00;
    end;
  if w = 8 then
    begin
      r := -7.871E+00;
    end;
  if w = 7 then
    begin
      r := -8.146E+00;
    end;
  if w = 6 then
    begin
      r := -8.451E+00;
    end;
  if w = 5 then
    begin
      r := -8.788E+00;
    end;
  if w = 4 then
    begin
      r := -9.144E+00;
    end;
  if w = 3 then
    begin
      r := -9.481E+00;
    end;
  if w = 2 then
    begin
      r := -9.992E+00;
    end;
  if w = 1 then
    begin
      r := -1.040E+01;
    end;
  if w <= 0 then
    begin
      r := -1.109E+01;
    end;
  Result := r;
end;

(* ************************************************************************
  Tail(S, 17)
  ************************************************************************ *)
function W17(const s: TLFloat): TLFloat;
var
  w: TLInt;
  r: TLFloat;
begin
  w := Round(-2.112463E+01 * s + 7.650000E+01);
  if w >= 76 then
    begin
      r := -6.931E-01;
    end;
  if w = 75 then
    begin
      r := -7.306E-01;
    end;
  if w = 74 then
    begin
      r := -7.695E-01;
    end;
  if w = 73 then
    begin
      r := -8.097E-01;
    end;
  if w = 72 then
    begin
      r := -8.514E-01;
    end;
  if w = 71 then
    begin
      r := -8.946E-01;
    end;
  if w = 70 then
    begin
      r := -9.392E-01;
    end;
  if w = 69 then
    begin
      r := -9.853E-01;
    end;
  if w = 68 then
    begin
      r := -1.033E+00;
    end;
  if w = 67 then
    begin
      r := -1.082E+00;
    end;
  if w = 66 then
    begin
      r := -1.133E+00;
    end;
  if w = 65 then
    begin
      r := -1.185E+00;
    end;
  if w = 64 then
    begin
      r := -1.240E+00;
    end;
  if w = 63 then
    begin
      r := -1.295E+00;
    end;
  if w = 62 then
    begin
      r := -1.353E+00;
    end;
  if w = 61 then
    begin
      r := -1.412E+00;
    end;
  if w = 60 then
    begin
      r := -1.473E+00;
    end;
  if w = 59 then
    begin
      r := -1.536E+00;
    end;
  if w = 58 then
    begin
      r := -1.600E+00;
    end;
  if w = 57 then
    begin
      r := -1.666E+00;
    end;
  if w = 56 then
    begin
      r := -1.735E+00;
    end;
  if w = 55 then
    begin
      r := -1.805E+00;
    end;
  if w = 54 then
    begin
      r := -1.877E+00;
    end;
  if w = 53 then
    begin
      r := -1.951E+00;
    end;
  if w = 52 then
    begin
      r := -2.028E+00;
    end;
  if w = 51 then
    begin
      r := -2.106E+00;
    end;
  if w = 50 then
    begin
      r := -2.186E+00;
    end;
  if w = 49 then
    begin
      r := -2.269E+00;
    end;
  if w = 48 then
    begin
      r := -2.353E+00;
    end;
  if w = 47 then
    begin
      r := -2.440E+00;
    end;
  if w = 46 then
    begin
      r := -2.530E+00;
    end;
  if w = 45 then
    begin
      r := -2.621E+00;
    end;
  if w = 44 then
    begin
      r := -2.715E+00;
    end;
  if w = 43 then
    begin
      r := -2.812E+00;
    end;
  if w = 42 then
    begin
      r := -2.911E+00;
    end;
  if w = 41 then
    begin
      r := -3.012E+00;
    end;
  if w = 40 then
    begin
      r := -3.116E+00;
    end;
  if w = 39 then
    begin
      r := -3.223E+00;
    end;
  if w = 38 then
    begin
      r := -3.332E+00;
    end;
  if w = 37 then
    begin
      r := -3.445E+00;
    end;
  if w = 36 then
    begin
      r := -3.560E+00;
    end;
  if w = 35 then
    begin
      r := -3.678E+00;
    end;
  if w = 34 then
    begin
      r := -3.799E+00;
    end;
  if w = 33 then
    begin
      r := -3.924E+00;
    end;
  if w = 32 then
    begin
      r := -4.052E+00;
    end;
  if w = 31 then
    begin
      r := -4.183E+00;
    end;
  if w = 30 then
    begin
      r := -4.317E+00;
    end;
  if w = 29 then
    begin
      r := -4.456E+00;
    end;
  if w = 28 then
    begin
      r := -4.597E+00;
    end;
  if w = 27 then
    begin
      r := -4.743E+00;
    end;
  if w = 26 then
    begin
      r := -4.893E+00;
    end;
  if w = 25 then
    begin
      r := -5.047E+00;
    end;
  if w = 24 then
    begin
      r := -5.204E+00;
    end;
  if w = 23 then
    begin
      r := -5.367E+00;
    end;
  if w = 22 then
    begin
      r := -5.534E+00;
    end;
  if w = 21 then
    begin
      r := -5.706E+00;
    end;
  if w = 20 then
    begin
      r := -5.884E+00;
    end;
  if w = 19 then
    begin
      r := -6.066E+00;
    end;
  if w = 18 then
    begin
      r := -6.254E+00;
    end;
  if w = 17 then
    begin
      r := -6.451E+00;
    end;
  if w = 16 then
    begin
      r := -6.654E+00;
    end;
  if w = 15 then
    begin
      r := -6.864E+00;
    end;
  if w = 14 then
    begin
      r := -7.083E+00;
    end;
  if w = 13 then
    begin
      r := -7.306E+00;
    end;
  if w = 12 then
    begin
      r := -7.535E+00;
    end;
  if w = 11 then
    begin
      r := -7.776E+00;
    end;
  if w = 10 then
    begin
      r := -8.022E+00;
    end;
  if w = 9 then
    begin
      r := -8.287E+00;
    end;
  if w = 8 then
    begin
      r := -8.565E+00;
    end;
  if w = 7 then
    begin
      r := -8.839E+00;
    end;
  if w = 6 then
    begin
      r := -9.144E+00;
    end;
  if w = 5 then
    begin
      r := -9.481E+00;
    end;
  if w = 4 then
    begin
      r := -9.838E+00;
    end;
  if w = 3 then
    begin
      r := -1.017E+01;
    end;
  if w = 2 then
    begin
      r := -1.068E+01;
    end;
  if w = 1 then
    begin
      r := -1.109E+01;
    end;
  if w <= 0 then
    begin
      r := -1.178E+01;
    end;
  Result := r;
end;

(* ************************************************************************
  Tail(S, 18)
  ************************************************************************ *)
function W18(const s: TLFloat): TLFloat;
var
  w: TLInt;
  r: TLFloat;
begin
  w := Round(-2.296193E+01 * s + 8.550000E+01);
  if w >= 85 then
    begin
      r := -6.931E-01;
    end;
  if w = 84 then
    begin
      r := -7.276E-01;
    end;
  if w = 83 then
    begin
      r := -7.633E-01;
    end;
  if w = 82 then
    begin
      r := -8.001E-01;
    end;
  if w = 81 then
    begin
      r := -8.381E-01;
    end;
  if w = 80 then
    begin
      r := -8.774E-01;
    end;
  if w = 79 then
    begin
      r := -9.179E-01;
    end;
  if w = 78 then
    begin
      r := -9.597E-01;
    end;
  if w = 77 then
    begin
      r := -1.003E+00;
    end;
  if w = 76 then
    begin
      r := -1.047E+00;
    end;
  if w = 75 then
    begin
      r := -1.093E+00;
    end;
  if w = 74 then
    begin
      r := -1.140E+00;
    end;
  if w = 73 then
    begin
      r := -1.188E+00;
    end;
  if w = 72 then
    begin
      r := -1.238E+00;
    end;
  if w = 71 then
    begin
      r := -1.289E+00;
    end;
  if w = 70 then
    begin
      r := -1.342E+00;
    end;
  if w = 69 then
    begin
      r := -1.396E+00;
    end;
  if w = 68 then
    begin
      r := -1.452E+00;
    end;
  if w = 67 then
    begin
      r := -1.509E+00;
    end;
  if w = 66 then
    begin
      r := -1.568E+00;
    end;
  if w = 65 then
    begin
      r := -1.628E+00;
    end;
  if w = 64 then
    begin
      r := -1.690E+00;
    end;
  if w = 63 then
    begin
      r := -1.753E+00;
    end;
  if w = 62 then
    begin
      r := -1.818E+00;
    end;
  if w = 61 then
    begin
      r := -1.885E+00;
    end;
  if w = 60 then
    begin
      r := -1.953E+00;
    end;
  if w = 59 then
    begin
      r := -2.023E+00;
    end;
  if w = 58 then
    begin
      r := -2.095E+00;
    end;
  if w = 57 then
    begin
      r := -2.168E+00;
    end;
  if w = 56 then
    begin
      r := -2.244E+00;
    end;
  if w = 55 then
    begin
      r := -2.321E+00;
    end;
  if w = 54 then
    begin
      r := -2.400E+00;
    end;
  if w = 53 then
    begin
      r := -2.481E+00;
    end;
  if w = 52 then
    begin
      r := -2.564E+00;
    end;
  if w = 51 then
    begin
      r := -2.648E+00;
    end;
  if w = 50 then
    begin
      r := -2.735E+00;
    end;
  if w = 49 then
    begin
      r := -2.824E+00;
    end;
  if w = 48 then
    begin
      r := -2.915E+00;
    end;
  if w = 47 then
    begin
      r := -3.008E+00;
    end;
  if w = 46 then
    begin
      r := -3.104E+00;
    end;
  if w = 45 then
    begin
      r := -3.201E+00;
    end;
  if w = 44 then
    begin
      r := -3.301E+00;
    end;
  if w = 43 then
    begin
      r := -3.403E+00;
    end;
  if w = 42 then
    begin
      r := -3.508E+00;
    end;
  if w = 41 then
    begin
      r := -3.615E+00;
    end;
  if w = 40 then
    begin
      r := -3.724E+00;
    end;
  if w = 39 then
    begin
      r := -3.836E+00;
    end;
  if w = 38 then
    begin
      r := -3.950E+00;
    end;
  if w = 37 then
    begin
      r := -4.068E+00;
    end;
  if w = 36 then
    begin
      r := -4.188E+00;
    end;
  if w = 35 then
    begin
      r := -4.311E+00;
    end;
  if w = 34 then
    begin
      r := -4.437E+00;
    end;
  if w = 33 then
    begin
      r := -4.565E+00;
    end;
  if w = 32 then
    begin
      r := -4.698E+00;
    end;
  if w = 31 then
    begin
      r := -4.833E+00;
    end;
  if w = 30 then
    begin
      r := -4.971E+00;
    end;
  if w = 29 then
    begin
      r := -5.113E+00;
    end;
  if w = 28 then
    begin
      r := -5.258E+00;
    end;
  if w = 27 then
    begin
      r := -5.408E+00;
    end;
  if w = 26 then
    begin
      r := -5.561E+00;
    end;
  if w = 25 then
    begin
      r := -5.717E+00;
    end;
  if w = 24 then
    begin
      r := -5.878E+00;
    end;
  if w = 23 then
    begin
      r := -6.044E+00;
    end;
  if w = 22 then
    begin
      r := -6.213E+00;
    end;
  if w = 21 then
    begin
      r := -6.388E+00;
    end;
  if w = 20 then
    begin
      r := -6.569E+00;
    end;
  if w = 19 then
    begin
      r := -6.753E+00;
    end;
  if w = 18 then
    begin
      r := -6.943E+00;
    end;
  if w = 17 then
    begin
      r := -7.144E+00;
    end;
  if w = 16 then
    begin
      r := -7.347E+00;
    end;
  if w = 15 then
    begin
      r := -7.557E+00;
    end;
  if w = 14 then
    begin
      r := -7.776E+00;
    end;
  if w = 13 then
    begin
      r := -7.999E+00;
    end;
  if w = 12 then
    begin
      r := -8.228E+00;
    end;
  if w = 11 then
    begin
      r := -8.469E+00;
    end;
  if w = 10 then
    begin
      r := -8.715E+00;
    end;
  if w = 9 then
    begin
      r := -8.980E+00;
    end;
  if w = 8 then
    begin
      r := -9.258E+00;
    end;
  if w = 7 then
    begin
      r := -9.532E+00;
    end;
  if w = 6 then
    begin
      r := -9.838E+00;
    end;
  if w = 5 then
    begin
      r := -1.017E+01;
    end;
  if w = 4 then
    begin
      r := -1.053E+01;
    end;
  if w = 3 then
    begin
      r := -1.087E+01;
    end;
  if w = 2 then
    begin
      r := -1.138E+01;
    end;
  if w = 1 then
    begin
      r := -1.178E+01;
    end;
  if w <= 0 then
    begin
      r := -1.248E+01;
    end;
  Result := r;
end;

(* ************************************************************************
  Tail(S, 19)
  ************************************************************************ *)
function W19(const s: TLFloat): TLFloat;
var
  w: TLInt;
  r: TLFloat;
begin
  w := Round(-2.484955E+01 * s + 9.500000E+01);
  if w >= 95 then
    begin
      r := -6.776E-01;
    end;
  if w = 94 then
    begin
      r := -7.089E-01;
    end;
  if w = 93 then
    begin
      r := -7.413E-01;
    end;
  if w = 92 then
    begin
      r := -7.747E-01;
    end;
  if w = 91 then
    begin
      r := -8.090E-01;
    end;
  if w = 90 then
    begin
      r := -8.445E-01;
    end;
  if w = 89 then
    begin
      r := -8.809E-01;
    end;
  if w = 88 then
    begin
      r := -9.185E-01;
    end;
  if w = 87 then
    begin
      r := -9.571E-01;
    end;
  if w = 86 then
    begin
      r := -9.968E-01;
    end;
  if w = 85 then
    begin
      r := -1.038E+00;
    end;
  if w = 84 then
    begin
      r := -1.080E+00;
    end;
  if w = 83 then
    begin
      r := -1.123E+00;
    end;
  if w = 82 then
    begin
      r := -1.167E+00;
    end;
  if w = 81 then
    begin
      r := -1.213E+00;
    end;
  if w = 80 then
    begin
      r := -1.259E+00;
    end;
  if w = 79 then
    begin
      r := -1.307E+00;
    end;
  if w = 78 then
    begin
      r := -1.356E+00;
    end;
  if w = 77 then
    begin
      r := -1.407E+00;
    end;
  if w = 76 then
    begin
      r := -1.458E+00;
    end;
  if w = 75 then
    begin
      r := -1.511E+00;
    end;
  if w = 74 then
    begin
      r := -1.565E+00;
    end;
  if w = 73 then
    begin
      r := -1.621E+00;
    end;
  if w = 72 then
    begin
      r := -1.678E+00;
    end;
  if w = 71 then
    begin
      r := -1.736E+00;
    end;
  if w = 70 then
    begin
      r := -1.796E+00;
    end;
  if w = 69 then
    begin
      r := -1.857E+00;
    end;
  if w = 68 then
    begin
      r := -1.919E+00;
    end;
  if w = 67 then
    begin
      r := -1.983E+00;
    end;
  if w = 66 then
    begin
      r := -2.048E+00;
    end;
  if w = 65 then
    begin
      r := -2.115E+00;
    end;
  if w = 64 then
    begin
      r := -2.183E+00;
    end;
  if w = 63 then
    begin
      r := -2.253E+00;
    end;
  if w = 62 then
    begin
      r := -2.325E+00;
    end;
  if w = 61 then
    begin
      r := -2.398E+00;
    end;
  if w = 60 then
    begin
      r := -2.472E+00;
    end;
  if w = 59 then
    begin
      r := -2.548E+00;
    end;
  if w = 58 then
    begin
      r := -2.626E+00;
    end;
  if w = 57 then
    begin
      r := -2.706E+00;
    end;
  if w = 56 then
    begin
      r := -2.787E+00;
    end;
  if w = 55 then
    begin
      r := -2.870E+00;
    end;
  if w = 54 then
    begin
      r := -2.955E+00;
    end;
  if w = 53 then
    begin
      r := -3.042E+00;
    end;
  if w = 52 then
    begin
      r := -3.130E+00;
    end;
  if w = 51 then
    begin
      r := -3.220E+00;
    end;
  if w = 50 then
    begin
      r := -3.313E+00;
    end;
  if w = 49 then
    begin
      r := -3.407E+00;
    end;
  if w = 48 then
    begin
      r := -3.503E+00;
    end;
  if w = 47 then
    begin
      r := -3.601E+00;
    end;
  if w = 46 then
    begin
      r := -3.702E+00;
    end;
  if w = 45 then
    begin
      r := -3.804E+00;
    end;
  if w = 44 then
    begin
      r := -3.909E+00;
    end;
  if w = 43 then
    begin
      r := -4.015E+00;
    end;
  if w = 42 then
    begin
      r := -4.125E+00;
    end;
  if w = 41 then
    begin
      r := -4.236E+00;
    end;
  if w = 40 then
    begin
      r := -4.350E+00;
    end;
  if w = 39 then
    begin
      r := -4.466E+00;
    end;
  if w = 38 then
    begin
      r := -4.585E+00;
    end;
  if w = 37 then
    begin
      r := -4.706E+00;
    end;
  if w = 36 then
    begin
      r := -4.830E+00;
    end;
  if w = 35 then
    begin
      r := -4.957E+00;
    end;
  if w = 34 then
    begin
      r := -5.086E+00;
    end;
  if w = 33 then
    begin
      r := -5.219E+00;
    end;
  if w = 32 then
    begin
      r := -5.355E+00;
    end;
  if w = 31 then
    begin
      r := -5.493E+00;
    end;
  if w = 30 then
    begin
      r := -5.634E+00;
    end;
  if w = 29 then
    begin
      r := -5.780E+00;
    end;
  if w = 28 then
    begin
      r := -5.928E+00;
    end;
  if w = 27 then
    begin
      r := -6.080E+00;
    end;
  if w = 26 then
    begin
      r := -6.235E+00;
    end;
  if w = 25 then
    begin
      r := -6.394E+00;
    end;
  if w = 24 then
    begin
      r := -6.558E+00;
    end;
  if w = 23 then
    begin
      r := -6.726E+00;
    end;
  if w = 22 then
    begin
      r := -6.897E+00;
    end;
  if w = 21 then
    begin
      r := -7.074E+00;
    end;
  if w = 20 then
    begin
      r := -7.256E+00;
    end;
  if w = 19 then
    begin
      r := -7.443E+00;
    end;
  if w = 18 then
    begin
      r := -7.636E+00;
    end;
  if w = 17 then
    begin
      r := -7.837E+00;
    end;
  if w = 16 then
    begin
      r := -8.040E+00;
    end;
  if w = 15 then
    begin
      r := -8.250E+00;
    end;
  if w = 14 then
    begin
      r := -8.469E+00;
    end;
  if w = 13 then
    begin
      r := -8.692E+00;
    end;
  if w = 12 then
    begin
      r := -8.921E+00;
    end;
  if w = 11 then
    begin
      r := -9.162E+00;
    end;
  if w = 10 then
    begin
      r := -9.409E+00;
    end;
  if w = 9 then
    begin
      r := -9.673E+00;
    end;
  if w = 8 then
    begin
      r := -9.951E+00;
    end;
  if w = 7 then
    begin
      r := -1.023E+01;
    end;
  if w = 6 then
    begin
      r := -1.053E+01;
    end;
  if w = 5 then
    begin
      r := -1.087E+01;
    end;
  if w = 4 then
    begin
      r := -1.122E+01;
    end;
  if w = 3 then
    begin
      r := -1.156E+01;
    end;
  if w = 2 then
    begin
      r := -1.207E+01;
    end;
  if w = 1 then
    begin
      r := -1.248E+01;
    end;
  if w <= 0 then
    begin
      r := -1.317E+01;
    end;
  Result := r;
end;

(* ************************************************************************
  Tail(S, 20)
  ************************************************************************ *)
function W20(const s: TLFloat): TLFloat;
var
  w: TLInt;
  r: TLFloat;
begin
  w := Round(-2.678619E+01 * s + 1.050000E+02);
  if w >= 105 then
    begin
      r := -6.787E-01;
    end;
  if w = 104 then
    begin
      r := -7.078E-01;
    end;
  if w = 103 then
    begin
      r := -7.378E-01;
    end;
  if w = 102 then
    begin
      r := -7.686E-01;
    end;
  if w = 101 then
    begin
      r := -8.004E-01;
    end;
  if w = 100 then
    begin
      r := -8.330E-01;
    end;
  if w = 99 then
    begin
      r := -8.665E-01;
    end;
  if w = 98 then
    begin
      r := -9.010E-01;
    end;
  if w = 97 then
    begin
      r := -9.363E-01;
    end;
  if w = 96 then
    begin
      r := -9.726E-01;
    end;
  if w = 95 then
    begin
      r := -1.010E+00;
    end;
  if w = 94 then
    begin
      r := -1.048E+00;
    end;
  if w = 93 then
    begin
      r := -1.087E+00;
    end;
  if w = 92 then
    begin
      r := -1.128E+00;
    end;
  if w = 91 then
    begin
      r := -1.169E+00;
    end;
  if w = 90 then
    begin
      r := -1.211E+00;
    end;
  if w = 89 then
    begin
      r := -1.254E+00;
    end;
  if w = 88 then
    begin
      r := -1.299E+00;
    end;
  if w = 87 then
    begin
      r := -1.344E+00;
    end;
  if w = 86 then
    begin
      r := -1.390E+00;
    end;
  if w = 85 then
    begin
      r := -1.438E+00;
    end;
  if w = 84 then
    begin
      r := -1.486E+00;
    end;
  if w = 83 then
    begin
      r := -1.536E+00;
    end;
  if w = 82 then
    begin
      r := -1.587E+00;
    end;
  if w = 81 then
    begin
      r := -1.639E+00;
    end;
  if w = 80 then
    begin
      r := -1.692E+00;
    end;
  if w = 79 then
    begin
      r := -1.746E+00;
    end;
  if w = 78 then
    begin
      r := -1.802E+00;
    end;
  if w = 77 then
    begin
      r := -1.859E+00;
    end;
  if w = 76 then
    begin
      r := -1.916E+00;
    end;
  if w = 75 then
    begin
      r := -1.976E+00;
    end;
  if w = 74 then
    begin
      r := -2.036E+00;
    end;
  if w = 73 then
    begin
      r := -2.098E+00;
    end;
  if w = 72 then
    begin
      r := -2.161E+00;
    end;
  if w = 71 then
    begin
      r := -2.225E+00;
    end;
  if w = 70 then
    begin
      r := -2.290E+00;
    end;
  if w = 69 then
    begin
      r := -2.357E+00;
    end;
  if w = 68 then
    begin
      r := -2.426E+00;
    end;
  if w = 67 then
    begin
      r := -2.495E+00;
    end;
  if w = 66 then
    begin
      r := -2.566E+00;
    end;
  if w = 65 then
    begin
      r := -2.639E+00;
    end;
  if w = 64 then
    begin
      r := -2.713E+00;
    end;
  if w = 63 then
    begin
      r := -2.788E+00;
    end;
  if w = 62 then
    begin
      r := -2.865E+00;
    end;
  if w = 61 then
    begin
      r := -2.943E+00;
    end;
  if w = 60 then
    begin
      r := -3.023E+00;
    end;
  if w = 59 then
    begin
      r := -3.104E+00;
    end;
  if w = 58 then
    begin
      r := -3.187E+00;
    end;
  if w = 57 then
    begin
      r := -3.272E+00;
    end;
  if w = 56 then
    begin
      r := -3.358E+00;
    end;
  if w = 55 then
    begin
      r := -3.446E+00;
    end;
  if w = 54 then
    begin
      r := -3.536E+00;
    end;
  if w = 53 then
    begin
      r := -3.627E+00;
    end;
  if w = 52 then
    begin
      r := -3.721E+00;
    end;
  if w = 51 then
    begin
      r := -3.815E+00;
    end;
  if w = 50 then
    begin
      r := -3.912E+00;
    end;
  if w = 49 then
    begin
      r := -4.011E+00;
    end;
  if w = 48 then
    begin
      r := -4.111E+00;
    end;
  if w = 47 then
    begin
      r := -4.214E+00;
    end;
  if w = 46 then
    begin
      r := -4.318E+00;
    end;
  if w = 45 then
    begin
      r := -4.425E+00;
    end;
  if w = 44 then
    begin
      r := -4.534E+00;
    end;
  if w = 43 then
    begin
      r := -4.644E+00;
    end;
  if w = 42 then
    begin
      r := -4.757E+00;
    end;
  if w = 41 then
    begin
      r := -4.872E+00;
    end;
  if w = 40 then
    begin
      r := -4.990E+00;
    end;
  if w = 39 then
    begin
      r := -5.109E+00;
    end;
  if w = 38 then
    begin
      r := -5.232E+00;
    end;
  if w = 37 then
    begin
      r := -5.356E+00;
    end;
  if w = 36 then
    begin
      r := -5.484E+00;
    end;
  if w = 35 then
    begin
      r := -5.614E+00;
    end;
  if w = 34 then
    begin
      r := -5.746E+00;
    end;
  if w = 33 then
    begin
      r := -5.882E+00;
    end;
  if w = 32 then
    begin
      r := -6.020E+00;
    end;
  if w = 31 then
    begin
      r := -6.161E+00;
    end;
  if w = 30 then
    begin
      r := -6.305E+00;
    end;
  if w = 29 then
    begin
      r := -6.453E+00;
    end;
  if w = 28 then
    begin
      r := -6.603E+00;
    end;
  if w = 27 then
    begin
      r := -6.757E+00;
    end;
  if w = 26 then
    begin
      r := -6.915E+00;
    end;
  if w = 25 then
    begin
      r := -7.076E+00;
    end;
  if w = 24 then
    begin
      r := -7.242E+00;
    end;
  if w = 23 then
    begin
      r := -7.411E+00;
    end;
  if w = 22 then
    begin
      r := -7.584E+00;
    end;
  if w = 21 then
    begin
      r := -7.763E+00;
    end;
  if w = 20 then
    begin
      r := -7.947E+00;
    end;
  if w = 19 then
    begin
      r := -8.136E+00;
    end;
  if w = 18 then
    begin
      r := -8.330E+00;
    end;
  if w = 17 then
    begin
      r := -8.530E+00;
    end;
  if w = 16 then
    begin
      r := -8.733E+00;
    end;
  if w = 15 then
    begin
      r := -8.943E+00;
    end;
  if w = 14 then
    begin
      r := -9.162E+00;
    end;
  if w = 13 then
    begin
      r := -9.386E+00;
    end;
  if w = 12 then
    begin
      r := -9.614E+00;
    end;
  if w = 11 then
    begin
      r := -9.856E+00;
    end;
  if w = 10 then
    begin
      r := -1.010E+01;
    end;
  if w = 9 then
    begin
      r := -1.037E+01;
    end;
  if w = 8 then
    begin
      r := -1.064E+01;
    end;
  if w = 7 then
    begin
      r := -1.092E+01;
    end;
  if w = 6 then
    begin
      r := -1.122E+01;
    end;
  if w = 5 then
    begin
      r := -1.156E+01;
    end;
  if w = 4 then
    begin
      r := -1.192E+01;
    end;
  if w = 3 then
    begin
      r := -1.225E+01;
    end;
  if w = 2 then
    begin
      r := -1.276E+01;
    end;
  if w = 1 then
    begin
      r := -1.317E+01;
    end;
  if w <= 0 then
    begin
      r := -1.386E+01;
    end;
  Result := r;
end;

(* ************************************************************************
  Tail(S, 21)
  ************************************************************************ *)
function W21(const s: TLFloat): TLFloat;
var
  w: TLInt;
  r: TLFloat;
begin
  w := Round(-2.877064E+01 * s + 1.155000E+02);
  if w >= 115 then
    begin
      r := -6.931E-01;
    end;
  if w = 114 then
    begin
      r := -7.207E-01;
    end;
  if w = 113 then
    begin
      r := -7.489E-01;
    end;
  if w = 112 then
    begin
      r := -7.779E-01;
    end;
  if w = 111 then
    begin
      r := -8.077E-01;
    end;
  if w = 110 then
    begin
      r := -8.383E-01;
    end;
  if w = 109 then
    begin
      r := -8.697E-01;
    end;
  if w = 108 then
    begin
      r := -9.018E-01;
    end;
  if w = 107 then
    begin
      r := -9.348E-01;
    end;
  if w = 106 then
    begin
      r := -9.685E-01;
    end;
  if w = 105 then
    begin
      r := -1.003E+00;
    end;
  if w = 104 then
    begin
      r := -1.039E+00;
    end;
  if w = 103 then
    begin
      r := -1.075E+00;
    end;
  if w = 102 then
    begin
      r := -1.112E+00;
    end;
  if w = 101 then
    begin
      r := -1.150E+00;
    end;
  if w = 100 then
    begin
      r := -1.189E+00;
    end;
  if w = 99 then
    begin
      r := -1.229E+00;
    end;
  if w = 98 then
    begin
      r := -1.269E+00;
    end;
  if w = 97 then
    begin
      r := -1.311E+00;
    end;
  if w = 96 then
    begin
      r := -1.353E+00;
    end;
  if w = 95 then
    begin
      r := -1.397E+00;
    end;
  if w = 94 then
    begin
      r := -1.441E+00;
    end;
  if w = 93 then
    begin
      r := -1.486E+00;
    end;
  if w = 92 then
    begin
      r := -1.533E+00;
    end;
  if w = 91 then
    begin
      r := -1.580E+00;
    end;
  if w = 90 then
    begin
      r := -1.628E+00;
    end;
  if w = 89 then
    begin
      r := -1.677E+00;
    end;
  if w = 88 then
    begin
      r := -1.728E+00;
    end;
  if w = 87 then
    begin
      r := -1.779E+00;
    end;
  if w = 86 then
    begin
      r := -1.831E+00;
    end;
  if w = 85 then
    begin
      r := -1.884E+00;
    end;
  if w = 84 then
    begin
      r := -1.939E+00;
    end;
  if w = 83 then
    begin
      r := -1.994E+00;
    end;
  if w = 82 then
    begin
      r := -2.051E+00;
    end;
  if w = 81 then
    begin
      r := -2.108E+00;
    end;
  if w = 80 then
    begin
      r := -2.167E+00;
    end;
  if w = 79 then
    begin
      r := -2.227E+00;
    end;
  if w = 78 then
    begin
      r := -2.288E+00;
    end;
  if w = 77 then
    begin
      r := -2.350E+00;
    end;
  if w = 76 then
    begin
      r := -2.414E+00;
    end;
  if w = 75 then
    begin
      r := -2.478E+00;
    end;
  if w = 74 then
    begin
      r := -2.544E+00;
    end;
  if w = 73 then
    begin
      r := -2.611E+00;
    end;
  if w = 72 then
    begin
      r := -2.679E+00;
    end;
  if w = 71 then
    begin
      r := -2.748E+00;
    end;
  if w = 70 then
    begin
      r := -2.819E+00;
    end;
  if w = 69 then
    begin
      r := -2.891E+00;
    end;
  if w = 68 then
    begin
      r := -2.964E+00;
    end;
  if w = 67 then
    begin
      r := -3.039E+00;
    end;
  if w = 66 then
    begin
      r := -3.115E+00;
    end;
  if w = 65 then
    begin
      r := -3.192E+00;
    end;
  if w = 64 then
    begin
      r := -3.270E+00;
    end;
  if w = 63 then
    begin
      r := -3.350E+00;
    end;
  if w = 62 then
    begin
      r := -3.432E+00;
    end;
  if w = 61 then
    begin
      r := -3.515E+00;
    end;
  if w = 60 then
    begin
      r := -3.599E+00;
    end;
  if w = 59 then
    begin
      r := -3.685E+00;
    end;
  if w = 58 then
    begin
      r := -3.772E+00;
    end;
  if w = 57 then
    begin
      r := -3.861E+00;
    end;
  if w = 56 then
    begin
      r := -3.952E+00;
    end;
  if w = 55 then
    begin
      r := -4.044E+00;
    end;
  if w = 54 then
    begin
      r := -4.138E+00;
    end;
  if w = 53 then
    begin
      r := -4.233E+00;
    end;
  if w = 52 then
    begin
      r := -4.330E+00;
    end;
  if w = 51 then
    begin
      r := -4.429E+00;
    end;
  if w = 50 then
    begin
      r := -4.530E+00;
    end;
  if w = 49 then
    begin
      r := -4.632E+00;
    end;
  if w = 48 then
    begin
      r := -4.736E+00;
    end;
  if w = 47 then
    begin
      r := -4.842E+00;
    end;
  if w = 46 then
    begin
      r := -4.950E+00;
    end;
  if w = 45 then
    begin
      r := -5.060E+00;
    end;
  if w = 44 then
    begin
      r := -5.172E+00;
    end;
  if w = 43 then
    begin
      r := -5.286E+00;
    end;
  if w = 42 then
    begin
      r := -5.402E+00;
    end;
  if w = 41 then
    begin
      r := -5.520E+00;
    end;
  if w = 40 then
    begin
      r := -5.641E+00;
    end;
  if w = 39 then
    begin
      r := -5.763E+00;
    end;
  if w = 38 then
    begin
      r := -5.889E+00;
    end;
  if w = 37 then
    begin
      r := -6.016E+00;
    end;
  if w = 36 then
    begin
      r := -6.146E+00;
    end;
  if w = 35 then
    begin
      r := -6.278E+00;
    end;
  if w = 34 then
    begin
      r := -6.413E+00;
    end;
  if w = 33 then
    begin
      r := -6.551E+00;
    end;
  if w = 32 then
    begin
      r := -6.692E+00;
    end;
  if w = 31 then
    begin
      r := -6.835E+00;
    end;
  if w = 30 then
    begin
      r := -6.981E+00;
    end;
  if w = 29 then
    begin
      r := -7.131E+00;
    end;
  if w = 28 then
    begin
      r := -7.283E+00;
    end;
  if w = 27 then
    begin
      r := -7.439E+00;
    end;
  if w = 26 then
    begin
      r := -7.599E+00;
    end;
  if w = 25 then
    begin
      r := -7.762E+00;
    end;
  if w = 24 then
    begin
      r := -7.928E+00;
    end;
  if w = 23 then
    begin
      r := -8.099E+00;
    end;
  if w = 22 then
    begin
      r := -8.274E+00;
    end;
  if w = 21 then
    begin
      r := -8.454E+00;
    end;
  if w = 20 then
    begin
      r := -8.640E+00;
    end;
  if w = 19 then
    begin
      r := -8.829E+00;
    end;
  if w = 18 then
    begin
      r := -9.023E+00;
    end;
  if w = 17 then
    begin
      r := -9.223E+00;
    end;
  if w = 16 then
    begin
      r := -9.426E+00;
    end;
  if w = 15 then
    begin
      r := -9.636E+00;
    end;
  if w = 14 then
    begin
      r := -9.856E+00;
    end;
  if w = 13 then
    begin
      r := -1.008E+01;
    end;
  if w = 12 then
    begin
      r := -1.031E+01;
    end;
  if w = 11 then
    begin
      r := -1.055E+01;
    end;
  if w = 10 then
    begin
      r := -1.079E+01;
    end;
  if w = 9 then
    begin
      r := -1.106E+01;
    end;
  if w = 8 then
    begin
      r := -1.134E+01;
    end;
  if w = 7 then
    begin
      r := -1.161E+01;
    end;
  if w = 6 then
    begin
      r := -1.192E+01;
    end;
  if w = 5 then
    begin
      r := -1.225E+01;
    end;
  if w = 4 then
    begin
      r := -1.261E+01;
    end;
  if w = 3 then
    begin
      r := -1.295E+01;
    end;
  if w = 2 then
    begin
      r := -1.346E+01;
    end;
  if w = 1 then
    begin
      r := -1.386E+01;
    end;
  if w <= 0 then
    begin
      r := -1.456E+01;
    end;
  Result := r;
end;

(* ************************************************************************
  Tail(S, 22)
  ************************************************************************ *)
function W22(const s: TLFloat): TLFloat;
var
  w: TLInt;
  r: TLFloat;
begin
  w := Round(-3.080179E+01 * s + 1.265000E+02);
  if w >= 126 then
    begin
      r := -6.931E-01;
    end;
  if w = 125 then
    begin
      r := -7.189E-01;
    end;
  if w = 124 then
    begin
      r := -7.452E-01;
    end;
  if w = 123 then
    begin
      r := -7.722E-01;
    end;
  if w = 122 then
    begin
      r := -7.999E-01;
    end;
  if w = 121 then
    begin
      r := -8.283E-01;
    end;
  if w = 120 then
    begin
      r := -8.573E-01;
    end;
  if w = 119 then
    begin
      r := -8.871E-01;
    end;
  if w = 118 then
    begin
      r := -9.175E-01;
    end;
  if w = 117 then
    begin
      r := -9.486E-01;
    end;
  if w = 116 then
    begin
      r := -9.805E-01;
    end;
  if w = 115 then
    begin
      r := -1.013E+00;
    end;
  if w = 114 then
    begin
      r := -1.046E+00;
    end;
  if w = 113 then
    begin
      r := -1.080E+00;
    end;
  if w = 112 then
    begin
      r := -1.115E+00;
    end;
  if w = 111 then
    begin
      r := -1.151E+00;
    end;
  if w = 110 then
    begin
      r := -1.187E+00;
    end;
  if w = 109 then
    begin
      r := -1.224E+00;
    end;
  if w = 108 then
    begin
      r := -1.262E+00;
    end;
  if w = 107 then
    begin
      r := -1.301E+00;
    end;
  if w = 106 then
    begin
      r := -1.340E+00;
    end;
  if w = 105 then
    begin
      r := -1.381E+00;
    end;
  if w = 104 then
    begin
      r := -1.422E+00;
    end;
  if w = 103 then
    begin
      r := -1.464E+00;
    end;
  if w = 102 then
    begin
      r := -1.506E+00;
    end;
  if w = 101 then
    begin
      r := -1.550E+00;
    end;
  if w = 100 then
    begin
      r := -1.594E+00;
    end;
  if w = 99 then
    begin
      r := -1.640E+00;
    end;
  if w = 98 then
    begin
      r := -1.686E+00;
    end;
  if w = 97 then
    begin
      r := -1.733E+00;
    end;
  if w = 96 then
    begin
      r := -1.781E+00;
    end;
  if w = 95 then
    begin
      r := -1.830E+00;
    end;
  if w = 94 then
    begin
      r := -1.880E+00;
    end;
  if w = 93 then
    begin
      r := -1.930E+00;
    end;
  if w = 92 then
    begin
      r := -1.982E+00;
    end;
  if w = 91 then
    begin
      r := -2.034E+00;
    end;
  if w = 90 then
    begin
      r := -2.088E+00;
    end;
  if w = 89 then
    begin
      r := -2.142E+00;
    end;
  if w = 88 then
    begin
      r := -2.198E+00;
    end;
  if w = 87 then
    begin
      r := -2.254E+00;
    end;
  if w = 86 then
    begin
      r := -2.312E+00;
    end;
  if w = 85 then
    begin
      r := -2.370E+00;
    end;
  if w = 84 then
    begin
      r := -2.429E+00;
    end;
  if w = 83 then
    begin
      r := -2.490E+00;
    end;
  if w = 82 then
    begin
      r := -2.551E+00;
    end;
  if w = 81 then
    begin
      r := -2.614E+00;
    end;
  if w = 80 then
    begin
      r := -2.677E+00;
    end;
  if w = 79 then
    begin
      r := -2.742E+00;
    end;
  if w = 78 then
    begin
      r := -2.808E+00;
    end;
  if w = 77 then
    begin
      r := -2.875E+00;
    end;
  if w = 76 then
    begin
      r := -2.943E+00;
    end;
  if w = 75 then
    begin
      r := -3.012E+00;
    end;
  if w = 74 then
    begin
      r := -3.082E+00;
    end;
  if w = 73 then
    begin
      r := -3.153E+00;
    end;
  if w = 72 then
    begin
      r := -3.226E+00;
    end;
  if w = 71 then
    begin
      r := -3.300E+00;
    end;
  if w = 70 then
    begin
      r := -3.375E+00;
    end;
  if w = 69 then
    begin
      r := -3.451E+00;
    end;
  if w = 68 then
    begin
      r := -3.529E+00;
    end;
  if w = 67 then
    begin
      r := -3.607E+00;
    end;
  if w = 66 then
    begin
      r := -3.687E+00;
    end;
  if w = 65 then
    begin
      r := -3.769E+00;
    end;
  if w = 64 then
    begin
      r := -3.851E+00;
    end;
  if w = 63 then
    begin
      r := -3.935E+00;
    end;
  if w = 62 then
    begin
      r := -4.021E+00;
    end;
  if w = 61 then
    begin
      r := -4.108E+00;
    end;
  if w = 60 then
    begin
      r := -4.196E+00;
    end;
  if w = 59 then
    begin
      r := -4.285E+00;
    end;
  if w = 58 then
    begin
      r := -4.376E+00;
    end;
  if w = 57 then
    begin
      r := -4.469E+00;
    end;
  if w = 56 then
    begin
      r := -4.563E+00;
    end;
  if w = 55 then
    begin
      r := -4.659E+00;
    end;
  if w = 54 then
    begin
      r := -4.756E+00;
    end;
  if w = 53 then
    begin
      r := -4.855E+00;
    end;
  if w = 52 then
    begin
      r := -4.955E+00;
    end;
  if w = 51 then
    begin
      r := -5.057E+00;
    end;
  if w = 50 then
    begin
      r := -5.161E+00;
    end;
  if w = 49 then
    begin
      r := -5.266E+00;
    end;
  if w = 48 then
    begin
      r := -5.374E+00;
    end;
  if w = 47 then
    begin
      r := -5.483E+00;
    end;
  if w = 46 then
    begin
      r := -5.594E+00;
    end;
  if w = 45 then
    begin
      r := -5.706E+00;
    end;
  if w = 44 then
    begin
      r := -5.821E+00;
    end;
  if w = 43 then
    begin
      r := -5.938E+00;
    end;
  if w = 42 then
    begin
      r := -6.057E+00;
    end;
  if w = 41 then
    begin
      r := -6.177E+00;
    end;
  if w = 40 then
    begin
      r := -6.300E+00;
    end;
  if w = 39 then
    begin
      r := -6.426E+00;
    end;
  if w = 38 then
    begin
      r := -6.553E+00;
    end;
  if w = 37 then
    begin
      r := -6.683E+00;
    end;
  if w = 36 then
    begin
      r := -6.815E+00;
    end;
  if w = 35 then
    begin
      r := -6.949E+00;
    end;
  if w = 34 then
    begin
      r := -7.086E+00;
    end;
  if w = 33 then
    begin
      r := -7.226E+00;
    end;
  if w = 32 then
    begin
      r := -7.368E+00;
    end;
  if w = 31 then
    begin
      r := -7.513E+00;
    end;
  if w = 30 then
    begin
      r := -7.661E+00;
    end;
  if w = 29 then
    begin
      r := -7.813E+00;
    end;
  if w = 28 then
    begin
      r := -7.966E+00;
    end;
  if w = 27 then
    begin
      r := -8.124E+00;
    end;
  if w = 26 then
    begin
      r := -8.285E+00;
    end;
  if w = 25 then
    begin
      r := -8.449E+00;
    end;
  if w = 24 then
    begin
      r := -8.617E+00;
    end;
  if w = 23 then
    begin
      r := -8.789E+00;
    end;
  if w = 22 then
    begin
      r := -8.965E+00;
    end;
  if w = 21 then
    begin
      r := -9.147E+00;
    end;
  if w = 20 then
    begin
      r := -9.333E+00;
    end;
  if w = 19 then
    begin
      r := -9.522E+00;
    end;
  if w = 18 then
    begin
      r := -9.716E+00;
    end;
  if w = 17 then
    begin
      r := -9.917E+00;
    end;
  if w = 16 then
    begin
      r := -1.012E+01;
    end;
  if w = 15 then
    begin
      r := -1.033E+01;
    end;
  if w = 14 then
    begin
      r := -1.055E+01;
    end;
  if w = 13 then
    begin
      r := -1.077E+01;
    end;
  if w = 12 then
    begin
      r := -1.100E+01;
    end;
  if w = 11 then
    begin
      r := -1.124E+01;
    end;
  if w = 10 then
    begin
      r := -1.149E+01;
    end;
  if w = 9 then
    begin
      r := -1.175E+01;
    end;
  if w = 8 then
    begin
      r := -1.203E+01;
    end;
  if w = 7 then
    begin
      r := -1.230E+01;
    end;
  if w = 6 then
    begin
      r := -1.261E+01;
    end;
  if w = 5 then
    begin
      r := -1.295E+01;
    end;
  if w = 4 then
    begin
      r := -1.330E+01;
    end;
  if w = 3 then
    begin
      r := -1.364E+01;
    end;
  if w = 2 then
    begin
      r := -1.415E+01;
    end;
  if w = 1 then
    begin
      r := -1.456E+01;
    end;
  if w <= 0 then
    begin
      r := -1.525E+01;
    end;
  Result := r;
end;

(* ************************************************************************
  Tail(S, 23)
  ************************************************************************ *)
function W23(const s: TLFloat): TLFloat;
var
  w: TLInt;
  r: TLFloat;
begin
  w := Round(-3.287856E+01 * s + 1.380000E+02);
  if w >= 138 then
    begin
      r := -6.813E-01;
    end;
  if w = 137 then
    begin
      r := -7.051E-01;
    end;
  if w = 136 then
    begin
      r := -7.295E-01;
    end;
  if w = 135 then
    begin
      r := -7.544E-01;
    end;
  if w = 134 then
    begin
      r := -7.800E-01;
    end;
  if w = 133 then
    begin
      r := -8.061E-01;
    end;
  if w = 132 then
    begin
      r := -8.328E-01;
    end;
  if w = 131 then
    begin
      r := -8.601E-01;
    end;
  if w = 130 then
    begin
      r := -8.880E-01;
    end;
  if w = 129 then
    begin
      r := -9.166E-01;
    end;
  if w = 128 then
    begin
      r := -9.457E-01;
    end;
  if w = 127 then
    begin
      r := -9.755E-01;
    end;
  if w = 126 then
    begin
      r := -1.006E+00;
    end;
  if w = 125 then
    begin
      r := -1.037E+00;
    end;
  if w = 124 then
    begin
      r := -1.069E+00;
    end;
  if w = 123 then
    begin
      r := -1.101E+00;
    end;
  if w = 122 then
    begin
      r := -1.134E+00;
    end;
  if w = 121 then
    begin
      r := -1.168E+00;
    end;
  if w = 120 then
    begin
      r := -1.202E+00;
    end;
  if w = 119 then
    begin
      r := -1.237E+00;
    end;
  if w = 118 then
    begin
      r := -1.273E+00;
    end;
  if w = 117 then
    begin
      r := -1.309E+00;
    end;
  if w = 116 then
    begin
      r := -1.347E+00;
    end;
  if w = 115 then
    begin
      r := -1.384E+00;
    end;
  if w = 114 then
    begin
      r := -1.423E+00;
    end;
  if w = 113 then
    begin
      r := -1.462E+00;
    end;
  if w = 112 then
    begin
      r := -1.502E+00;
    end;
  if w = 111 then
    begin
      r := -1.543E+00;
    end;
  if w = 110 then
    begin
      r := -1.585E+00;
    end;
  if w = 109 then
    begin
      r := -1.627E+00;
    end;
  if w = 108 then
    begin
      r := -1.670E+00;
    end;
  if w = 107 then
    begin
      r := -1.714E+00;
    end;
  if w = 106 then
    begin
      r := -1.758E+00;
    end;
  if w = 105 then
    begin
      r := -1.804E+00;
    end;
  if w = 104 then
    begin
      r := -1.850E+00;
    end;
  if w = 103 then
    begin
      r := -1.897E+00;
    end;
  if w = 102 then
    begin
      r := -1.944E+00;
    end;
  if w = 101 then
    begin
      r := -1.993E+00;
    end;
  if w = 100 then
    begin
      r := -2.042E+00;
    end;
  if w = 99 then
    begin
      r := -2.093E+00;
    end;
  if w = 98 then
    begin
      r := -2.144E+00;
    end;
  if w = 97 then
    begin
      r := -2.195E+00;
    end;
  if w = 96 then
    begin
      r := -2.248E+00;
    end;
  if w = 95 then
    begin
      r := -2.302E+00;
    end;
  if w = 94 then
    begin
      r := -2.356E+00;
    end;
  if w = 93 then
    begin
      r := -2.412E+00;
    end;
  if w = 92 then
    begin
      r := -2.468E+00;
    end;
  if w = 91 then
    begin
      r := -2.525E+00;
    end;
  if w = 90 then
    begin
      r := -2.583E+00;
    end;
  if w = 89 then
    begin
      r := -2.642E+00;
    end;
  if w = 88 then
    begin
      r := -2.702E+00;
    end;
  if w = 87 then
    begin
      r := -2.763E+00;
    end;
  if w = 86 then
    begin
      r := -2.825E+00;
    end;
  if w = 85 then
    begin
      r := -2.888E+00;
    end;
  if w = 84 then
    begin
      r := -2.951E+00;
    end;
  if w = 83 then
    begin
      r := -3.016E+00;
    end;
  if w = 82 then
    begin
      r := -3.082E+00;
    end;
  if w = 81 then
    begin
      r := -3.149E+00;
    end;
  if w = 80 then
    begin
      r := -3.216E+00;
    end;
  if w = 79 then
    begin
      r := -3.285E+00;
    end;
  if w = 78 then
    begin
      r := -3.355E+00;
    end;
  if w = 77 then
    begin
      r := -3.426E+00;
    end;
  if w = 76 then
    begin
      r := -3.498E+00;
    end;
  if w = 75 then
    begin
      r := -3.571E+00;
    end;
  if w = 74 then
    begin
      r := -3.645E+00;
    end;
  if w = 73 then
    begin
      r := -3.721E+00;
    end;
  if w = 72 then
    begin
      r := -3.797E+00;
    end;
  if w = 71 then
    begin
      r := -3.875E+00;
    end;
  if w = 70 then
    begin
      r := -3.953E+00;
    end;
  if w = 69 then
    begin
      r := -4.033E+00;
    end;
  if w = 68 then
    begin
      r := -4.114E+00;
    end;
  if w = 67 then
    begin
      r := -4.197E+00;
    end;
  if w = 66 then
    begin
      r := -4.280E+00;
    end;
  if w = 65 then
    begin
      r := -4.365E+00;
    end;
  if w = 64 then
    begin
      r := -4.451E+00;
    end;
  if w = 63 then
    begin
      r := -4.539E+00;
    end;
  if w = 62 then
    begin
      r := -4.628E+00;
    end;
  if w = 61 then
    begin
      r := -4.718E+00;
    end;
  if w = 60 then
    begin
      r := -4.809E+00;
    end;
  if w = 59 then
    begin
      r := -4.902E+00;
    end;
  if w = 58 then
    begin
      r := -4.996E+00;
    end;
  if w = 57 then
    begin
      r := -5.092E+00;
    end;
  if w = 56 then
    begin
      r := -5.189E+00;
    end;
  if w = 55 then
    begin
      r := -5.287E+00;
    end;
  if w = 54 then
    begin
      r := -5.388E+00;
    end;
  if w = 53 then
    begin
      r := -5.489E+00;
    end;
  if w = 52 then
    begin
      r := -5.592E+00;
    end;
  if w = 51 then
    begin
      r := -5.697E+00;
    end;
  if w = 50 then
    begin
      r := -5.804E+00;
    end;
  if w = 49 then
    begin
      r := -5.912E+00;
    end;
  if w = 48 then
    begin
      r := -6.022E+00;
    end;
  if w = 47 then
    begin
      r := -6.133E+00;
    end;
  if w = 46 then
    begin
      r := -6.247E+00;
    end;
  if w = 45 then
    begin
      r := -6.362E+00;
    end;
  if w = 44 then
    begin
      r := -6.479E+00;
    end;
  if w = 43 then
    begin
      r := -6.598E+00;
    end;
  if w = 42 then
    begin
      r := -6.719E+00;
    end;
  if w = 41 then
    begin
      r := -6.842E+00;
    end;
  if w = 40 then
    begin
      r := -6.967E+00;
    end;
  if w = 39 then
    begin
      r := -7.094E+00;
    end;
  if w = 38 then
    begin
      r := -7.224E+00;
    end;
  if w = 37 then
    begin
      r := -7.355E+00;
    end;
  if w = 36 then
    begin
      r := -7.489E+00;
    end;
  if w = 35 then
    begin
      r := -7.625E+00;
    end;
  if w = 34 then
    begin
      r := -7.764E+00;
    end;
  if w = 33 then
    begin
      r := -7.905E+00;
    end;
  if w = 32 then
    begin
      r := -8.049E+00;
    end;
  if w = 31 then
    begin
      r := -8.196E+00;
    end;
  if w = 30 then
    begin
      r := -8.345E+00;
    end;
  if w = 29 then
    begin
      r := -8.498E+00;
    end;
  if w = 28 then
    begin
      r := -8.653E+00;
    end;
  if w = 27 then
    begin
      r := -8.811E+00;
    end;
  if w = 26 then
    begin
      r := -8.974E+00;
    end;
  if w = 25 then
    begin
      r := -9.139E+00;
    end;
  if w = 24 then
    begin
      r := -9.308E+00;
    end;
  if w = 23 then
    begin
      r := -9.481E+00;
    end;
  if w = 22 then
    begin
      r := -9.658E+00;
    end;
  if w = 21 then
    begin
      r := -9.840E+00;
    end;
  if w = 20 then
    begin
      r := -1.003E+01;
    end;
  if w = 19 then
    begin
      r := -1.022E+01;
    end;
  if w = 18 then
    begin
      r := -1.041E+01;
    end;
  if w = 17 then
    begin
      r := -1.061E+01;
    end;
  if w = 16 then
    begin
      r := -1.081E+01;
    end;
  if w = 15 then
    begin
      r := -1.102E+01;
    end;
  if w = 14 then
    begin
      r := -1.124E+01;
    end;
  if w = 13 then
    begin
      r := -1.147E+01;
    end;
  if w = 12 then
    begin
      r := -1.169E+01;
    end;
  if w = 11 then
    begin
      r := -1.194E+01;
    end;
  if w = 10 then
    begin
      r := -1.218E+01;
    end;
  if w = 9 then
    begin
      r := -1.245E+01;
    end;
  if w = 8 then
    begin
      r := -1.272E+01;
    end;
  if w = 7 then
    begin
      r := -1.300E+01;
    end;
  if w = 6 then
    begin
      r := -1.330E+01;
    end;
  if w = 5 then
    begin
      r := -1.364E+01;
    end;
  if w = 4 then
    begin
      r := -1.400E+01;
    end;
  if w = 3 then
    begin
      r := -1.433E+01;
    end;
  if w = 2 then
    begin
      r := -1.484E+01;
    end;
  if w = 1 then
    begin
      r := -1.525E+01;
    end;
  if w <= 0 then
    begin
      r := -1.594E+01;
    end;
  Result := r;
end;

(* ************************************************************************
  Tail(S, 24)
  ************************************************************************ *)
function W24(const s: TLFloat): TLFloat;
var
  w: TLInt;
  r: TLFloat;
begin
  w := Round(-3.500000E+01 * s + 1.500000E+02);
  if w >= 150 then
    begin
      r := -6.820E-01;
    end;
  if w = 149 then
    begin
      r := -7.044E-01;
    end;
  if w = 148 then
    begin
      r := -7.273E-01;
    end;
  if w = 147 then
    begin
      r := -7.507E-01;
    end;
  if w = 146 then
    begin
      r := -7.746E-01;
    end;
  if w = 145 then
    begin
      r := -7.990E-01;
    end;
  if w = 144 then
    begin
      r := -8.239E-01;
    end;
  if w = 143 then
    begin
      r := -8.494E-01;
    end;
  if w = 142 then
    begin
      r := -8.754E-01;
    end;
  if w = 141 then
    begin
      r := -9.020E-01;
    end;
  if w = 140 then
    begin
      r := -9.291E-01;
    end;
  if w = 139 then
    begin
      r := -9.567E-01;
    end;
  if w = 138 then
    begin
      r := -9.849E-01;
    end;
  if w = 137 then
    begin
      r := -1.014E+00;
    end;
  if w = 136 then
    begin
      r := -1.043E+00;
    end;
  if w = 135 then
    begin
      r := -1.073E+00;
    end;
  if w = 134 then
    begin
      r := -1.103E+00;
    end;
  if w = 133 then
    begin
      r := -1.135E+00;
    end;
  if w = 132 then
    begin
      r := -1.166E+00;
    end;
  if w = 131 then
    begin
      r := -1.198E+00;
    end;
  if w = 130 then
    begin
      r := -1.231E+00;
    end;
  if w = 129 then
    begin
      r := -1.265E+00;
    end;
  if w = 128 then
    begin
      r := -1.299E+00;
    end;
  if w = 127 then
    begin
      r := -1.334E+00;
    end;
  if w = 126 then
    begin
      r := -1.369E+00;
    end;
  if w = 125 then
    begin
      r := -1.405E+00;
    end;
  if w = 124 then
    begin
      r := -1.441E+00;
    end;
  if w = 123 then
    begin
      r := -1.479E+00;
    end;
  if w = 122 then
    begin
      r := -1.517E+00;
    end;
  if w = 121 then
    begin
      r := -1.555E+00;
    end;
  if w = 120 then
    begin
      r := -1.594E+00;
    end;
  if w = 119 then
    begin
      r := -1.634E+00;
    end;
  if w = 118 then
    begin
      r := -1.675E+00;
    end;
  if w = 117 then
    begin
      r := -1.716E+00;
    end;
  if w = 116 then
    begin
      r := -1.758E+00;
    end;
  if w = 115 then
    begin
      r := -1.800E+00;
    end;
  if w = 114 then
    begin
      r := -1.844E+00;
    end;
  if w = 113 then
    begin
      r := -1.888E+00;
    end;
  if w = 112 then
    begin
      r := -1.932E+00;
    end;
  if w = 111 then
    begin
      r := -1.978E+00;
    end;
  if w = 110 then
    begin
      r := -2.024E+00;
    end;
  if w = 109 then
    begin
      r := -2.070E+00;
    end;
  if w = 108 then
    begin
      r := -2.118E+00;
    end;
  if w = 107 then
    begin
      r := -2.166E+00;
    end;
  if w = 106 then
    begin
      r := -2.215E+00;
    end;
  if w = 105 then
    begin
      r := -2.265E+00;
    end;
  if w = 104 then
    begin
      r := -2.316E+00;
    end;
  if w = 103 then
    begin
      r := -2.367E+00;
    end;
  if w = 102 then
    begin
      r := -2.419E+00;
    end;
  if w = 101 then
    begin
      r := -2.472E+00;
    end;
  if w = 100 then
    begin
      r := -2.526E+00;
    end;
  if w = 99 then
    begin
      r := -2.580E+00;
    end;
  if w = 98 then
    begin
      r := -2.636E+00;
    end;
  if w = 97 then
    begin
      r := -2.692E+00;
    end;
  if w = 96 then
    begin
      r := -2.749E+00;
    end;
  if w = 95 then
    begin
      r := -2.806E+00;
    end;
  if w = 94 then
    begin
      r := -2.865E+00;
    end;
  if w = 93 then
    begin
      r := -2.925E+00;
    end;
  if w = 92 then
    begin
      r := -2.985E+00;
    end;
  if w = 91 then
    begin
      r := -3.046E+00;
    end;
  if w = 90 then
    begin
      r := -3.108E+00;
    end;
  if w = 89 then
    begin
      r := -3.171E+00;
    end;
  if w = 88 then
    begin
      r := -3.235E+00;
    end;
  if w = 87 then
    begin
      r := -3.300E+00;
    end;
  if w = 86 then
    begin
      r := -3.365E+00;
    end;
  if w = 85 then
    begin
      r := -3.432E+00;
    end;
  if w = 84 then
    begin
      r := -3.499E+00;
    end;
  if w = 83 then
    begin
      r := -3.568E+00;
    end;
  if w = 82 then
    begin
      r := -3.637E+00;
    end;
  if w = 81 then
    begin
      r := -3.708E+00;
    end;
  if w = 80 then
    begin
      r := -3.779E+00;
    end;
  if w = 79 then
    begin
      r := -3.852E+00;
    end;
  if w = 78 then
    begin
      r := -3.925E+00;
    end;
  if w = 77 then
    begin
      r := -4.000E+00;
    end;
  if w = 76 then
    begin
      r := -4.075E+00;
    end;
  if w = 75 then
    begin
      r := -4.151E+00;
    end;
  if w = 74 then
    begin
      r := -4.229E+00;
    end;
  if w = 73 then
    begin
      r := -4.308E+00;
    end;
  if w = 72 then
    begin
      r := -4.387E+00;
    end;
  if w = 71 then
    begin
      r := -4.468E+00;
    end;
  if w = 70 then
    begin
      r := -4.550E+00;
    end;
  if w = 69 then
    begin
      r := -4.633E+00;
    end;
  if w = 68 then
    begin
      r := -4.718E+00;
    end;
  if w = 67 then
    begin
      r := -4.803E+00;
    end;
  if w = 66 then
    begin
      r := -4.890E+00;
    end;
  if w = 65 then
    begin
      r := -4.978E+00;
    end;
  if w = 64 then
    begin
      r := -5.067E+00;
    end;
  if w = 63 then
    begin
      r := -5.157E+00;
    end;
  if w = 62 then
    begin
      r := -5.249E+00;
    end;
  if w = 61 then
    begin
      r := -5.342E+00;
    end;
  if w = 60 then
    begin
      r := -5.436E+00;
    end;
  if w = 59 then
    begin
      r := -5.531E+00;
    end;
  if w = 58 then
    begin
      r := -5.628E+00;
    end;
  if w = 57 then
    begin
      r := -5.727E+00;
    end;
  if w = 56 then
    begin
      r := -5.826E+00;
    end;
  if w = 55 then
    begin
      r := -5.927E+00;
    end;
  if w = 54 then
    begin
      r := -6.030E+00;
    end;
  if w = 53 then
    begin
      r := -6.134E+00;
    end;
  if w = 52 then
    begin
      r := -6.240E+00;
    end;
  if w = 51 then
    begin
      r := -6.347E+00;
    end;
  if w = 50 then
    begin
      r := -6.456E+00;
    end;
  if w = 49 then
    begin
      r := -6.566E+00;
    end;
  if w = 48 then
    begin
      r := -6.678E+00;
    end;
  if w = 47 then
    begin
      r := -6.792E+00;
    end;
  if w = 46 then
    begin
      r := -6.907E+00;
    end;
  if w = 45 then
    begin
      r := -7.025E+00;
    end;
  if w = 44 then
    begin
      r := -7.144E+00;
    end;
  if w = 43 then
    begin
      r := -7.265E+00;
    end;
  if w = 42 then
    begin
      r := -7.387E+00;
    end;
  if w = 41 then
    begin
      r := -7.512E+00;
    end;
  if w = 40 then
    begin
      r := -7.639E+00;
    end;
  if w = 39 then
    begin
      r := -7.768E+00;
    end;
  if w = 38 then
    begin
      r := -7.899E+00;
    end;
  if w = 37 then
    begin
      r := -8.032E+00;
    end;
  if w = 36 then
    begin
      r := -8.167E+00;
    end;
  if w = 35 then
    begin
      r := -8.305E+00;
    end;
  if w = 34 then
    begin
      r := -8.445E+00;
    end;
  if w = 33 then
    begin
      r := -8.588E+00;
    end;
  if w = 32 then
    begin
      r := -8.733E+00;
    end;
  if w = 31 then
    begin
      r := -8.881E+00;
    end;
  if w = 30 then
    begin
      r := -9.031E+00;
    end;
  if w = 29 then
    begin
      r := -9.185E+00;
    end;
  if w = 28 then
    begin
      r := -9.341E+00;
    end;
  if w = 27 then
    begin
      r := -9.501E+00;
    end;
  if w = 26 then
    begin
      r := -9.664E+00;
    end;
  if w = 25 then
    begin
      r := -9.830E+00;
    end;
  if w = 24 then
    begin
      r := -1.000E+01;
    end;
  if w = 23 then
    begin
      r := -1.017E+01;
    end;
  if w = 22 then
    begin
      r := -1.035E+01;
    end;
  if w = 21 then
    begin
      r := -1.053E+01;
    end;
  if w = 20 then
    begin
      r := -1.072E+01;
    end;
  if w = 19 then
    begin
      r := -1.091E+01;
    end;
  if w = 18 then
    begin
      r := -1.110E+01;
    end;
  if w = 17 then
    begin
      r := -1.130E+01;
    end;
  if w = 16 then
    begin
      r := -1.151E+01;
    end;
  if w = 15 then
    begin
      r := -1.172E+01;
    end;
  if w = 14 then
    begin
      r := -1.194E+01;
    end;
  if w = 13 then
    begin
      r := -1.216E+01;
    end;
  if w = 12 then
    begin
      r := -1.239E+01;
    end;
  if w = 11 then
    begin
      r := -1.263E+01;
    end;
  if w = 10 then
    begin
      r := -1.287E+01;
    end;
  if w = 9 then
    begin
      r := -1.314E+01;
    end;
  if w = 8 then
    begin
      r := -1.342E+01;
    end;
  if w = 7 then
    begin
      r := -1.369E+01;
    end;
  if w = 6 then
    begin
      r := -1.400E+01;
    end;
  if w = 5 then
    begin
      r := -1.433E+01;
    end;
  if w = 4 then
    begin
      r := -1.469E+01;
    end;
  if w = 3 then
    begin
      r := -1.503E+01;
    end;
  if w = 2 then
    begin
      r := -1.554E+01;
    end;
  if w = 1 then
    begin
      r := -1.594E+01;
    end;
  if w <= 0 then
    begin
      r := -1.664E+01;
    end;
  Result := r;
end;

(* ************************************************************************
  Tail(S, 25)
  ************************************************************************ *)
function W25(const s: TLFloat): TLFloat;
var
  x: TLFloat;
  TJ: TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 4.000000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  WCheb(x, -5.150509E+00, TJ, TJ1, Result);
  WCheb(x, -5.695528E+00, TJ, TJ1, Result);
  WCheb(x, -1.437637E+00, TJ, TJ1, Result);
  WCheb(x, -2.611906E-01, TJ, TJ1, Result);
  WCheb(x, -7.625722E-02, TJ, TJ1, Result);
  WCheb(x, -2.579892E-02, TJ, TJ1, Result);
  WCheb(x, -1.086876E-02, TJ, TJ1, Result);
  WCheb(x, -2.906543E-03, TJ, TJ1, Result);
  WCheb(x, -2.354881E-03, TJ, TJ1, Result);
  WCheb(x, 1.007195E-04, TJ, TJ1, Result);
  WCheb(x, -8.437327E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 26)
  ************************************************************************ *)
function W26(const s: TLFloat): TLFloat;
var
  x: TLFloat;
  TJ: TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 4.000000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  WCheb(x, -5.117622E+00, TJ, TJ1, Result);
  WCheb(x, -5.635159E+00, TJ, TJ1, Result);
  WCheb(x, -1.395167E+00, TJ, TJ1, Result);
  WCheb(x, -2.382823E-01, TJ, TJ1, Result);
  WCheb(x, -6.531987E-02, TJ, TJ1, Result);
  WCheb(x, -2.060112E-02, TJ, TJ1, Result);
  WCheb(x, -8.203697E-03, TJ, TJ1, Result);
  WCheb(x, -1.516523E-03, TJ, TJ1, Result);
  WCheb(x, -1.431364E-03, TJ, TJ1, Result);
  WCheb(x, 6.384553E-04, TJ, TJ1, Result);
  WCheb(x, -3.238369E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 27)
  ************************************************************************ *)
function W27(const s: TLFloat): TLFloat;
var
  x: TLFloat;
  TJ: TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 4.000000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  WCheb(x, -5.089731E+00, TJ, TJ1, Result);
  WCheb(x, -5.584248E+00, TJ, TJ1, Result);
  WCheb(x, -1.359966E+00, TJ, TJ1, Result);
  WCheb(x, -2.203696E-01, TJ, TJ1, Result);
  WCheb(x, -5.753344E-02, TJ, TJ1, Result);
  WCheb(x, -1.761891E-02, TJ, TJ1, Result);
  WCheb(x, -7.096897E-03, TJ, TJ1, Result);
  WCheb(x, -1.419108E-03, TJ, TJ1, Result);
  WCheb(x, -1.581214E-03, TJ, TJ1, Result);
  WCheb(x, 3.033766E-04, TJ, TJ1, Result);
  WCheb(x, -5.901441E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 28)
  ************************************************************************ *)
function W28(const s: TLFloat): TLFloat;
var
  x: TLFloat;
  TJ: TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 4.000000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  WCheb(x, -5.065046E+00, TJ, TJ1, Result);
  WCheb(x, -5.539163E+00, TJ, TJ1, Result);
  WCheb(x, -1.328939E+00, TJ, TJ1, Result);
  WCheb(x, -2.046376E-01, TJ, TJ1, Result);
  WCheb(x, -5.061515E-02, TJ, TJ1, Result);
  WCheb(x, -1.469271E-02, TJ, TJ1, Result);
  WCheb(x, -5.711578E-03, TJ, TJ1, Result);
  WCheb(x, -8.389153E-04, TJ, TJ1, Result);
  WCheb(x, -1.250575E-03, TJ, TJ1, Result);
  WCheb(x, 4.047245E-04, TJ, TJ1, Result);
  WCheb(x, -5.128555E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 29)
  ************************************************************************ *)
function W29(const s: TLFloat): TLFloat;
var
  x: TLFloat;
  TJ: TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 4.000000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  WCheb(x, -5.043413E+00, TJ, TJ1, Result);
  WCheb(x, -5.499756E+00, TJ, TJ1, Result);
  WCheb(x, -1.302137E+00, TJ, TJ1, Result);
  WCheb(x, -1.915129E-01, TJ, TJ1, Result);
  WCheb(x, -4.516329E-02, TJ, TJ1, Result);
  WCheb(x, -1.260064E-02, TJ, TJ1, Result);
  WCheb(x, -4.817269E-03, TJ, TJ1, Result);
  WCheb(x, -5.478130E-04, TJ, TJ1, Result);
  WCheb(x, -1.111668E-03, TJ, TJ1, Result);
  WCheb(x, 4.093451E-04, TJ, TJ1, Result);
  WCheb(x, -5.135860E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 30)
  ************************************************************************ *)
function W30(const s: TLFloat): TLFloat;
var
  x: TLFloat;
  TJ: TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 4.000000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  WCheb(x, -5.024071E+00, TJ, TJ1, Result);
  WCheb(x, -5.464515E+00, TJ, TJ1, Result);
  WCheb(x, -1.278342E+00, TJ, TJ1, Result);
  WCheb(x, -1.800030E-01, TJ, TJ1, Result);
  WCheb(x, -4.046294E-02, TJ, TJ1, Result);
  WCheb(x, -1.076162E-02, TJ, TJ1, Result);
  WCheb(x, -3.968677E-03, TJ, TJ1, Result);
  WCheb(x, -1.911679E-04, TJ, TJ1, Result);
  WCheb(x, -8.619185E-04, TJ, TJ1, Result);
  WCheb(x, 5.125362E-04, TJ, TJ1, Result);
  WCheb(x, -3.984370E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 40)
  ************************************************************************ *)
function W40(const s: TLFloat): TLFloat;
var
  x: TLFloat;
  TJ: TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 4.000000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  WCheb(x, -4.904809E+00, TJ, TJ1, Result);
  WCheb(x, -5.248327E+00, TJ, TJ1, Result);
  WCheb(x, -1.136698E+00, TJ, TJ1, Result);
  WCheb(x, -1.170982E-01, TJ, TJ1, Result);
  WCheb(x, -1.824427E-02, TJ, TJ1, Result);
  WCheb(x, -3.888648E-03, TJ, TJ1, Result);
  WCheb(x, -1.344929E-03, TJ, TJ1, Result);
  WCheb(x, 2.790407E-04, TJ, TJ1, Result);
  WCheb(x, -4.619858E-04, TJ, TJ1, Result);
  WCheb(x, 3.359121E-04, TJ, TJ1, Result);
  WCheb(x, -2.883026E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 60)
  ************************************************************************ *)
function W60(const s: TLFloat): TLFloat;
var
  x: TLFloat;
  TJ: TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 4.000000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  WCheb(x, -4.809656E+00, TJ, TJ1, Result);
  WCheb(x, -5.077191E+00, TJ, TJ1, Result);
  WCheb(x, -1.029402E+00, TJ, TJ1, Result);
  WCheb(x, -7.507931E-02, TJ, TJ1, Result);
  WCheb(x, -6.506226E-03, TJ, TJ1, Result);
  WCheb(x, -1.391278E-03, TJ, TJ1, Result);
  WCheb(x, -4.263635E-04, TJ, TJ1, Result);
  WCheb(x, 2.302271E-04, TJ, TJ1, Result);
  WCheb(x, -2.384348E-04, TJ, TJ1, Result);
  WCheb(x, 1.865587E-04, TJ, TJ1, Result);
  WCheb(x, -1.622355E-04, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 120)
  ************************************************************************ *)
function W120(const s: TLFloat): TLFloat;
var
  x: TLFloat;
  TJ: TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 4.000000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  WCheb(x, -4.729426E+00, TJ, TJ1, Result);
  WCheb(x, -4.934426E+00, TJ, TJ1, Result);
  WCheb(x, -9.433231E-01, TJ, TJ1, Result);
  WCheb(x, -4.492504E-02, TJ, TJ1, Result);
  WCheb(x, 1.673948E-05, TJ, TJ1, Result);
  WCheb(x, -6.077014E-04, TJ, TJ1, Result);
  WCheb(x, -7.215768E-05, TJ, TJ1, Result);
  WCheb(x, 9.086734E-05, TJ, TJ1, Result);
  WCheb(x, -8.447980E-05, TJ, TJ1, Result);
  WCheb(x, 6.705028E-05, TJ, TJ1, Result);
  WCheb(x, -5.828507E-05, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S, 200)
  ************************************************************************ *)
function W200(const s: TLFloat): TLFloat;
var
  x: TLFloat;
  TJ: TLFloat;
  TJ1: TLFloat;
begin
  Result := 0;
  x := Min(2 * (s - 0.000000E+00) / 4.000000E+00 - 1, 1.0);
  TJ := 1;
  TJ1 := x;
  WCheb(x, -4.700240E+00, TJ, TJ1, Result);
  WCheb(x, -4.883080E+00, TJ, TJ1, Result);
  WCheb(x, -9.132168E-01, TJ, TJ1, Result);
  WCheb(x, -3.512684E-02, TJ, TJ1, Result);
  WCheb(x, 1.726342E-03, TJ, TJ1, Result);
  WCheb(x, -5.189796E-04, TJ, TJ1, Result);
  WCheb(x, -1.628659E-06, TJ, TJ1, Result);
  WCheb(x, 4.261786E-05, TJ, TJ1, Result);
  WCheb(x, -4.002498E-05, TJ, TJ1, Result);
  WCheb(x, 3.146287E-05, TJ, TJ1, Result);
  WCheb(x, -2.727576E-05, TJ, TJ1, Result);
end;

(* ************************************************************************
  Tail(S,N), S>=0
  ************************************************************************ *)
function WSigma(const s: TLFloat; const n: TLInt): TLFloat;
var
  F0: TLFloat;
  f1: TLFloat;
  f2: TLFloat;
  F3: TLFloat;
  F4: TLFloat;
  x0: TLFloat;
  x1: TLFloat;
  x2: TLFloat;
  x3: TLFloat;
  x4: TLFloat;
  x: TLFloat;
begin
  Result := 0;
  if n = 5 then
    begin
      Result := W5(s);
    end;
  if n = 6 then
    begin
      Result := W6(s);
    end;
  if n = 7 then
    begin
      Result := W7(s);
    end;
  if n = 8 then
    begin
      Result := W8(s);
    end;
  if n = 9 then
    begin
      Result := W9(s);
    end;
  if n = 10 then
    begin
      Result := W10(s);
    end;
  if n = 11 then
    begin
      Result := W11(s);
    end;
  if n = 12 then
    begin
      Result := W12(s);
    end;
  if n = 13 then
    begin
      Result := W13(s);
    end;
  if n = 14 then
    begin
      Result := W14(s);
    end;
  if n = 15 then
    begin
      Result := W15(s);
    end;
  if n = 16 then
    begin
      Result := W16(s);
    end;
  if n = 17 then
    begin
      Result := W17(s);
    end;
  if n = 18 then
    begin
      Result := W18(s);
    end;
  if n = 19 then
    begin
      Result := W19(s);
    end;
  if n = 20 then
    begin
      Result := W20(s);
    end;
  if n = 21 then
    begin
      Result := W21(s);
    end;
  if n = 22 then
    begin
      Result := W22(s);
    end;
  if n = 23 then
    begin
      Result := W23(s);
    end;
  if n = 24 then
    begin
      Result := W24(s);
    end;
  if n = 25 then
    begin
      Result := W25(s);
    end;
  if n = 26 then
    begin
      Result := W26(s);
    end;
  if n = 27 then
    begin
      Result := W27(s);
    end;
  if n = 28 then
    begin
      Result := W28(s);
    end;
  if n = 29 then
    begin
      Result := W29(s);
    end;
  if n = 30 then
    begin
      Result := W30(s);
    end;
  if n > 30 then
    begin
      x := 1.0 / n;
      x0 := 1.0 / 30;
      F0 := W30(s);
      x1 := 1.0 / 40;
      f1 := W40(s);
      x2 := 1.0 / 60;
      f2 := W60(s);
      x3 := 1.0 / 120;
      F3 := W120(s);
      x4 := 1.0 / 200;
      F4 := W200(s);
      f1 := ((x - x0) * f1 - (x - x1) * F0) / (x1 - x0);
      f2 := ((x - x0) * f2 - (x - x2) * F0) / (x2 - x0);
      F3 := ((x - x0) * F3 - (x - x3) * F0) / (x3 - x0);
      F4 := ((x - x0) * F4 - (x - x4) * F0) / (x4 - x0);
      f2 := ((x - x1) * f2 - (x - x2) * f1) / (x2 - x1);
      F3 := ((x - x1) * F3 - (x - x3) * f1) / (x3 - x1);
      F4 := ((x - x1) * F4 - (x - x4) * f1) / (x4 - x1);
      F3 := ((x - x2) * F3 - (x - x3) * f2) / (x3 - x2);
      F4 := ((x - x2) * F4 - (x - x4) * f2) / (x4 - x2);
      F4 := ((x - x3) * F4 - (x - x4) * F3) / (x4 - x3);
      Result := F4;
    end;
end;
