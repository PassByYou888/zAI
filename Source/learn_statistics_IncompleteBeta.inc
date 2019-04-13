{ ****************************************************************************** }
{ * Incomplete beta integral support, by QQ 600585@qq.com                      * }
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
function IncompleteBetaFE(const a, b, x, big, biginv: TLFloat): TLFloat; forward;
function IncompleteBetaFE2(const a, b, x, big, biginv: TLFloat): TLFloat; forward;
function IncompleteBetaPS(const a, b, x, MAXGAM: TLFloat): TLFloat; forward;


(* ************************************************************************
  *Incomplete beta integral
  *
  *Returns incomplete beta integral of the arguments, evaluated
  *from zero to x.  The function is defined as
  *
  *                 x
  *    -            -
  *   | (a+b)      | |  a-1     b-1
  * -----------    |   t   (1-t)   dt.
  *  -     -     | |
  * | (a) | (b)   -
  *                0
  *
  *The domain of definition is 0 <= x <= 1.  In this
  *implementation a and b are restricted to positive values.
  *The integral from x to 1 may be obtained by the symmetry
  *relation
  *
  *   1 - incbet( a, b, x )  =  incbet( b, a, 1-x ).
  *
  *The integral is evaluated by a continued fraction expansion
  *or, when b*x is small, by a power series.
  *
  *ACCURACY:
  *
  *Tested at uniformly distributed random points (a,b,x) with a and b
  *in "domain" and x between 0 and 1.
  *                                       Relative error
  *arithmetic   domain     # trials      peak         rms
  *   IEEE      0,5         10000       6.9e-15     4.5e-16
  *   IEEE      0,85       250000       2.2e-13     1.7e-14
  *   IEEE      0,1000      30000       5.3e-12     6.3e-13
  *   IEEE      0,10000    250000       9.3e-11     7.1e-12
  *   IEEE      0,100000    10000       8.7e-10     4.8e-11
  *Outputs smaller than the IEEE gradual underflow threshold were excluded from these statistics.
  *
  *Cephes Math Library, Release 2.8:  June, 2000
  *Copyright 1984, 1995, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function IncompleteBeta(a, b, x: TLFloat): TLFloat;
var
  t     : TLFloat;
  XC    : TLFloat;
  w     : TLFloat;
  y     : TLFloat;
  flag  : TLInt;
  sg    : TLFloat;
  big   : TLFloat;
  biginv: TLFloat;
  MAXGAM: TLFloat;
  MINLOG: TLFloat;
  MAXLOG: TLFloat;
begin
  big := 4.503599627370496E15;
  biginv := 2.22044604925031308085E-16;
  MAXGAM := 171.624376956302725;
  MINLOG := ln(MinRealNumber);
  MAXLOG := ln(MaxRealNumber);
  Assert(AP_FP_Greater(a, 0) and AP_FP_Greater(b, 0),
    'Domain error in IncompleteBeta');
  Assert(AP_FP_Greater_Eq(x, 0) and AP_FP_Less_Eq(x, 1),
    'Domain error in IncompleteBeta');
  if AP_FP_Eq(x, 0) then
    begin
      Result := 0;
      Exit;
    end;
  if AP_FP_Eq(x, 1) then
    begin
      Result := 1;
      Exit;
    end;
  flag := 0;
  if AP_FP_Less_Eq(b * x, 1.0) and AP_FP_Less_Eq(x, 0.95) then
    begin
      Result := IncompleteBetaPS(a, b, x, MAXGAM);
      Exit;
    end;
  w := 1.0 - x;
  if AP_FP_Greater(x, a / (a + b)) then
    begin
      flag := 1;
      t := a;
      a := b;
      b := t;
      XC := x;
      x := w;
    end
  else
    begin
      XC := w;
    end;
  if (flag = 1) and AP_FP_Less_Eq(b * x, 1.0) and AP_FP_Less_Eq(x, 0.95) then
    begin
      t := IncompleteBetaPS(a, b, x, MAXGAM);
      if AP_FP_Less_Eq(t, MachineEpsilon) then
        begin
          Result := 1.0 - MachineEpsilon;
        end
      else
        begin
          Result := 1.0 - t;
        end;
      Exit;
    end;
  y := x * (a + b - 2.0) - (a - 1.0);
  if AP_FP_Less(y, 0.0) then
    begin
      w := IncompleteBetaFE(a, b, x, big, biginv);
    end
  else
    begin
      w := IncompleteBetaFE2(a, b, x, big, biginv) / XC;
    end;
  y := a * ln(x);
  t := b * ln(XC);
  if AP_FP_Less(a + b, MAXGAM) and AP_FP_Less(AbsReal(y), MAXLOG) and
    AP_FP_Less(AbsReal(t), MAXLOG) then
    begin
      t := Power(XC, b);
      t := t * Power(x, a);
      t := t / a;
      t := t * w;
      t := t * (Gamma(a + b) / (Gamma(a) * Gamma(b)));
      if flag = 1 then
        begin
          if AP_FP_Less_Eq(t, MachineEpsilon) then
            begin
              Result := 1.0 - MachineEpsilon;
            end
          else
            begin
              Result := 1.0 - t;
            end;
        end
      else
        begin
          Result := t;
        end;
      Exit;
    end;
  y := y + t + LnGamma(a + b, sg) - LnGamma(a, sg) - LnGamma(b, sg);
  y := y + ln(w / a);
  if AP_FP_Less(y, MINLOG) then
    begin
      t := 0.0;
    end
  else
    begin
      t := Exp(y);
    end;
  if flag = 1 then
    begin
      if AP_FP_Less_Eq(t, MachineEpsilon) then
        begin
          t := 1.0 - MachineEpsilon;
        end
      else
        begin
          t := 1.0 - t;
        end;
    end;
  Result := t;
end;

(* ************************************************************************
  Inverse of imcomplete beta integral

  Given y, the function finds x such that

  incbet( a, b, x ) = y .

  The routine performs interval halving or Newton iterations to find the
  root of incbet(a,b,x) - y = 0.


  ACCURACY:

  Relative error:
  x     a,b
  arithmetic   domain  domain  # trials    peak       rms
  IEEE      0,1    .5,10000   50000    5.8e-12   1.3e-13
  IEEE      0,1   .25,100    100000    1.8e-13   3.9e-15
  IEEE      0,1     0,5       50000    1.1e-12   5.5e-15
  With a and b constrained to half-integer or integer values:
  IEEE      0,1    .5,10000   50000    5.8e-12   1.1e-13
  IEEE      0,1    .5,100    100000    1.7e-14   7.9e-16
  With a = .5, b constrained to half-integer or integer values:
  IEEE      0,1    .5,10000   10000    8.3e-11   1.0e-11

  Cephes Math Library Release 2.8:  June, 2000
  Copyright 1984, 1996, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function InvIncompleteBeta(const a, b, y: TLFloat): TLFloat;
var
  aaa             : TLFloat;
  bbb             : TLFloat;
  y0              : TLFloat;
  d               : TLFloat;
  yyy             : TLFloat;
  x               : TLFloat;
  x0              : TLFloat;
  x1              : TLFloat;
  lgm             : TLFloat;
  yp              : TLFloat;
  di              : TLFloat;
  dithresh        : TLFloat;
  yl              : TLFloat;
  yh              : TLFloat;
  xt              : TLFloat;
  i               : TLInt;
  rflg            : TLInt;
  dir             : TLInt;
  nflg            : TLInt;
  s               : TLFloat;
  MainLoopPos     : TLInt;
  ihalve          : TLInt;
  ihalvecycle     : TLInt;
  newt            : TLInt;
  newtcycle       : TLInt;
  breaknewtcycle  : TLInt;
  breakihalvecycle: TLInt;
begin
  i := 0;
  Assert(AP_FP_Greater_Eq(y, 0) and AP_FP_Less_Eq(y, 1),
    'Domain error in InvIncompleteBeta');
  if AP_FP_Eq(y, 0) then
    begin
      Result := 0;
      Exit;
    end;
  if AP_FP_Eq(y, 1.0) then
    begin
      Result := 1;
      Exit;
    end;
  x0 := 0.0;
  yl := 0.0;
  x1 := 1.0;
  yh := 1.0;
  nflg := 0;
  MainLoopPos := 0;
  ihalve := 1;
  ihalvecycle := 2;
  newt := 3;
  newtcycle := 4;
  breaknewtcycle := 5;
  breakihalvecycle := 6;
  while True do
    begin

      //
      // start
      //
      if MainLoopPos = 0 then
        begin
          if AP_FP_Less_Eq(a, 1.0) or AP_FP_Less_Eq(b, 1.0) then
            begin
              dithresh := 1.0E-6;
              rflg := 0;
              aaa := a;
              bbb := b;
              y0 := y;
              x := aaa / (aaa + bbb);
              yyy := IncompleteBeta(aaa, bbb, x);
              MainLoopPos := ihalve;
              Continue;
            end
          else
            begin
              dithresh := 1.0E-4;
            end;
          yp := -InvNormalDistribution(y);
          if AP_FP_Greater(y, 0.5) then
            begin
              rflg := 1;
              aaa := b;
              bbb := a;
              y0 := 1.0 - y;
              yp := -yp;
            end
          else
            begin
              rflg := 0;
              aaa := a;
              bbb := b;
              y0 := y;
            end;
          lgm := (yp * yp - 3.0) / 6.0;
          x := 2.0 / (1.0 / (2.0 * aaa - 1.0) + 1.0 / (2.0 * bbb - 1.0));
          d := yp * Sqrt(x + lgm) / x -
            (1.0 / (2.0 * bbb - 1.0) - 1.0 / (2.0 * aaa - 1.0)) *
            (lgm + 5.0 / 6.0 - 2.0 / (3.0 * x));
          d := 2.0 * d;
          if AP_FP_Less(d, ln(MinRealNumber)) then
            begin
              x := 0;
              Break;
            end;
          x := aaa / (aaa + bbb * Exp(d));
          yyy := IncompleteBeta(aaa, bbb, x);
          yp := (yyy - y0) / y0;
          if AP_FP_Less(AbsReal(yp), 0.2) then
            begin
              MainLoopPos := newt;
              Continue;
            end;
          MainLoopPos := ihalve;
          Continue;
        end;

      //
      // ihalve
      //
      if MainLoopPos = ihalve then
        begin
          dir := 0;
          di := 0.5;
          i := 0;
          MainLoopPos := ihalvecycle;
          Continue;
        end;

      //
      // ihalvecycle
      //
      if MainLoopPos = ihalvecycle then
        begin
          if i <= 99 then
            begin
              if i <> 0 then
                begin
                  x := x0 + di * (x1 - x0);
                  if AP_FP_Eq(x, 1.0) then
                    begin
                      x := 1.0 - MachineEpsilon;
                    end;
                  if AP_FP_Eq(x, 0.0) then
                    begin
                      di := 0.5;
                      x := x0 + di * (x1 - x0);
                      if AP_FP_Eq(x, 0.0) then
                        begin
                          Break;
                        end;
                    end;
                  yyy := IncompleteBeta(aaa, bbb, x);
                  yp := (x1 - x0) / (x1 + x0);
                  if AP_FP_Less(AbsReal(yp), dithresh) then
                    begin
                      MainLoopPos := newt;
                      Continue;
                    end;
                  yp := (yyy - y0) / y0;
                  if AP_FP_Less(AbsReal(yp), dithresh) then
                    begin
                      MainLoopPos := newt;
                      Continue;
                    end;
                end;
              if AP_FP_Less(yyy, y0) then
                begin
                  x0 := x;
                  yl := yyy;
                  if dir < 0 then
                    begin
                      dir := 0;
                      di := 0.5;
                    end
                  else
                    begin
                      if dir > 3 then
                        begin
                          di := 1.0 - (1.0 - di) * (1.0 - di);
                        end
                      else
                        begin
                          if dir > 1 then
                            begin
                              di := 0.5 * di + 0.5;
                            end
                          else
                            begin
                              di := (y0 - yyy) / (yh - yl);
                            end;
                        end;
                    end;
                  dir := dir + 1;
                  if AP_FP_Greater(x0, 0.75) then
                    begin
                      if rflg = 1 then
                        begin
                          rflg := 0;
                          aaa := a;
                          bbb := b;
                          y0 := y;
                        end
                      else
                        begin
                          rflg := 1;
                          aaa := b;
                          bbb := a;
                          y0 := 1.0 - y;
                        end;
                      x := 1.0 - x;
                      yyy := IncompleteBeta(aaa, bbb, x);
                      x0 := 0.0;
                      yl := 0.0;
                      x1 := 1.0;
                      yh := 1.0;
                      MainLoopPos := ihalve;
                      Continue;
                    end;
                end
              else
                begin
                  x1 := x;
                  if (rflg = 1) and AP_FP_Less(x1, MachineEpsilon) then
                    begin
                      x := 0.0;
                      Break;
                    end;
                  yh := yyy;
                  if dir > 0 then
                    begin
                      dir := 0;
                      di := 0.5;
                    end
                  else
                    begin
                      if dir < -3 then
                        begin
                          di := di * di;
                        end
                      else
                        begin
                          if dir < -1 then
                            begin
                              di := 0.5 * di;
                            end
                          else
                            begin
                              di := (yyy - y0) / (yh - yl);
                            end;
                        end;
                    end;
                  dir := dir - 1;
                end;
              i := i + 1;
              MainLoopPos := ihalvecycle;
              Continue;
            end
          else
            begin
              MainLoopPos := breakihalvecycle;
              Continue;
            end;
        end;

      //
      // breakihalvecycle
      //
      if MainLoopPos = breakihalvecycle then
        begin
          if AP_FP_Greater_Eq(x0, 1.0) then
            begin
              x := 1.0 - MachineEpsilon;
              Break;
            end;
          if AP_FP_Less_Eq(x, 0.0) then
            begin
              x := 0.0;
              Break;
            end;
          MainLoopPos := newt;
          Continue;
        end;

      //
      // newt
      //
      if MainLoopPos = newt then
        begin
          if nflg <> 0 then
            begin
              Break;
            end;
          nflg := 1;
          lgm := LnGamma(aaa + bbb, s) - LnGamma(aaa, s) - LnGamma(bbb, s);
          i := 0;
          MainLoopPos := newtcycle;
          Continue;
        end;

      //
      // newtcycle
      //
      if MainLoopPos = newtcycle then
        begin
          if i <= 7 then
            begin
              if i <> 0 then
                begin
                  yyy := IncompleteBeta(aaa, bbb, x);
                end;
              if AP_FP_Less(yyy, yl) then
                begin
                  x := x0;
                  yyy := yl;
                end
              else
                begin
                  if AP_FP_Greater(yyy, yh) then
                    begin
                      x := x1;
                      yyy := yh;
                    end
                  else
                    begin
                      if AP_FP_Less(yyy, y0) then
                        begin
                          x0 := x;
                          yl := yyy;
                        end
                      else
                        begin
                          x1 := x;
                          yh := yyy;
                        end;
                    end;
                end;
              if AP_FP_Eq(x, 1.0) or AP_FP_Eq(x, 0.0) then
                begin
                  MainLoopPos := breaknewtcycle;
                  Continue;
                end;
              d := (aaa - 1.0) * ln(x) + (bbb - 1.0) * ln(1.0 - x) + lgm;
              if AP_FP_Less(d, ln(MinRealNumber)) then
                begin
                  Break;
                end;
              if AP_FP_Greater(d, ln(MaxRealNumber)) then
                begin
                  MainLoopPos := breaknewtcycle;
                  Continue;
                end;
              d := Exp(d);
              d := (yyy - y0) / d;
              xt := x - d;
              if AP_FP_Less_Eq(xt, x0) then
                begin
                  yyy := (x - x0) / (x1 - x0);
                  xt := x0 + 0.5 * yyy * (x - x0);
                  if AP_FP_Less_Eq(xt, 0.0) then
                    begin
                      MainLoopPos := breaknewtcycle;
                      Continue;
                    end;
                end;
              if AP_FP_Greater_Eq(xt, x1) then
                begin
                  yyy := (x1 - x) / (x1 - x0);
                  xt := x1 - 0.5 * yyy * (x1 - x);
                  if AP_FP_Greater_Eq(xt, 1.0) then
                    begin
                      MainLoopPos := breaknewtcycle;
                      Continue;
                    end;
                end;
              x := xt;
              if AP_FP_Less(AbsReal(d / x), 128.0 * MachineEpsilon) then
                begin
                  Break;
                end;
              i := i + 1;
              MainLoopPos := newtcycle;
              Continue;
            end
          else
            begin
              MainLoopPos := breaknewtcycle;
              Continue;
            end;
        end;

      //
      // breaknewtcycle
      //
      if MainLoopPos = breaknewtcycle then
        begin
          dithresh := 256.0 * MachineEpsilon;
          MainLoopPos := ihalve;
          Continue;
        end;
    end;

  //
  // done
  //
  if rflg <> 0 then
    begin
      if AP_FP_Less_Eq(x, MachineEpsilon) then
        begin
          x := 1.0 - MachineEpsilon;
        end
      else
        begin
          x := 1.0 - x;
        end;
    end;
  Result := x;
end;

(* ************************************************************************
  Continued fraction expansion #1 for incomplete beta integral

  Cephes Math Library, Release 2.8:  June, 2000
  Copyright 1984, 1995, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function IncompleteBetaFE(const a, b, x, big, biginv: TLFloat): TLFloat;
var
  xk    : TLFloat;
  pk    : TLFloat;
  pkm1  : TLFloat;
  pkm2  : TLFloat;
  qk    : TLFloat;
  qkm1  : TLFloat;
  qkm2  : TLFloat;
  k1    : TLFloat;
  k2    : TLFloat;
  k3    : TLFloat;
  K4    : TLFloat;
  k5    : TLFloat;
  k6    : TLFloat;
  k7    : TLFloat;
  k8    : TLFloat;
  r     : TLFloat;
  t     : TLFloat;
  ans   : TLFloat;
  THRESH: TLFloat;
  n     : TLInt;
begin
  k1 := a;
  k2 := a + b;
  k3 := a;
  K4 := a + 1.0;
  k5 := 1.0;
  k6 := b - 1.0;
  k7 := K4;
  k8 := a + 2.0;
  pkm2 := 0.0;
  qkm2 := 1.0;
  pkm1 := 1.0;
  qkm1 := 1.0;
  ans := 1.0;
  r := 1.0;
  n := 0;
  THRESH := 3.0 * MachineEpsilon;
  repeat
    xk := -x * k1 * k2 / (k3 * K4);
    pk := pkm1 + pkm2 * xk;
    qk := qkm1 + qkm2 * xk;
    pkm2 := pkm1;
    pkm1 := pk;
    qkm2 := qkm1;
    qkm1 := qk;
    xk := x * k5 * k6 / (k7 * k8);
    pk := pkm1 + pkm2 * xk;
    qk := qkm1 + qkm2 * xk;
    pkm2 := pkm1;
    pkm1 := pk;
    qkm2 := qkm1;
    qkm1 := qk;
    if AP_FP_NEq(qk, 0) then
      begin
        r := pk / qk;
      end;
    if AP_FP_NEq(r, 0) then
      begin
        t := AbsReal((ans - r) / r);
        ans := r;
      end
    else
      begin
        t := 1.0;
      end;
    if AP_FP_Less(t, THRESH) then
      begin
        Break;
      end;
    k1 := k1 + 1.0;
    k2 := k2 + 1.0;
    k3 := k3 + 2.0;
    K4 := K4 + 2.0;
    k5 := k5 + 1.0;
    k6 := k6 - 1.0;
    k7 := k7 + 2.0;
    k8 := k8 + 2.0;
    if AP_FP_Greater(AbsReal(qk) + AbsReal(pk), big) then
      begin
        pkm2 := pkm2 * biginv;
        pkm1 := pkm1 * biginv;
        qkm2 := qkm2 * biginv;
        qkm1 := qkm1 * biginv;
      end;
    if AP_FP_Less(AbsReal(qk), biginv) or AP_FP_Less(AbsReal(pk), biginv) then
      begin
        pkm2 := pkm2 * big;
        pkm1 := pkm1 * big;
        qkm2 := qkm2 * big;
        qkm1 := qkm1 * big;
      end;
    n := n + 1;
  until n = 300;
  Result := ans;
end;

(* ************************************************************************
  Continued fraction expansion #2
  for incomplete beta integral

  Cephes Math Library, Release 2.8:  June, 2000
  Copyright 1984, 1995, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function IncompleteBetaFE2(const a, b, x, big, biginv: TLFloat): TLFloat;
var
  xk    : TLFloat;
  pk    : TLFloat;
  pkm1  : TLFloat;
  pkm2  : TLFloat;
  qk    : TLFloat;
  qkm1  : TLFloat;
  qkm2  : TLFloat;
  k1    : TLFloat;
  k2    : TLFloat;
  k3    : TLFloat;
  K4    : TLFloat;
  k5    : TLFloat;
  k6    : TLFloat;
  k7    : TLFloat;
  k8    : TLFloat;
  r     : TLFloat;
  t     : TLFloat;
  ans   : TLFloat;
  z     : TLFloat;
  THRESH: TLFloat;
  n     : TLInt;
begin
  k1 := a;
  k2 := b - 1.0;
  k3 := a;
  K4 := a + 1.0;
  k5 := 1.0;
  k6 := a + b;
  k7 := a + 1.0;
  k8 := a + 2.0;
  pkm2 := 0.0;
  qkm2 := 1.0;
  pkm1 := 1.0;
  qkm1 := 1.0;
  z := x / (1.0 - x);
  ans := 1.0;
  r := 1.0;
  n := 0;
  THRESH := 3.0 * MachineEpsilon;
  repeat
    xk := -z * k1 * k2 / (k3 * K4);
    pk := pkm1 + pkm2 * xk;
    qk := qkm1 + qkm2 * xk;
    pkm2 := pkm1;
    pkm1 := pk;
    qkm2 := qkm1;
    qkm1 := qk;
    xk := z * k5 * k6 / (k7 * k8);
    pk := pkm1 + pkm2 * xk;
    qk := qkm1 + qkm2 * xk;
    pkm2 := pkm1;
    pkm1 := pk;
    qkm2 := qkm1;
    qkm1 := qk;
    if AP_FP_NEq(qk, 0) then
      begin
        r := pk / qk;
      end;
    if AP_FP_NEq(r, 0) then
      begin
        t := AbsReal((ans - r) / r);
        ans := r;
      end
    else
      begin
        t := 1.0;
      end;
    if AP_FP_Less(t, THRESH) then
      begin
        Break;
      end;
    k1 := k1 + 1.0;
    k2 := k2 - 1.0;
    k3 := k3 + 2.0;
    K4 := K4 + 2.0;
    k5 := k5 + 1.0;
    k6 := k6 + 1.0;
    k7 := k7 + 2.0;
    k8 := k8 + 2.0;
    if AP_FP_Greater(AbsReal(qk) + AbsReal(pk), big) then
      begin
        pkm2 := pkm2 * biginv;
        pkm1 := pkm1 * biginv;
        qkm2 := qkm2 * biginv;
        qkm1 := qkm1 * biginv;
      end;
    if AP_FP_Less(AbsReal(qk), biginv) or AP_FP_Less(AbsReal(pk), biginv) then
      begin
        pkm2 := pkm2 * big;
        pkm1 := pkm1 * big;
        qkm2 := qkm2 * big;
        qkm1 := qkm1 * big;
      end;
    n := n + 1;
  until n = 300;
  Result := ans;
end;

(* ************************************************************************
  Power series for incomplete beta integral.
  Use when b*x is small and x not too close to 1.

  Cephes Math Library, Release 2.8:  June, 2000
  Copyright 1984, 1995, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function IncompleteBetaPS(const a, b, x, MAXGAM: TLFloat): TLFloat;
var
  s : TLFloat;
  t : TLFloat;
  u : TLFloat;
  v : TLFloat;
  n : TLFloat;
  t1: TLFloat;
  z : TLFloat;
  ai: TLFloat;
  sg: TLFloat;
begin
  ai := 1.0 / a;
  u := (1.0 - b) * x;
  v := u / (a + 1.0);
  t1 := v;
  t := u;
  n := 2.0;
  s := 0.0;
  z := MachineEpsilon * ai;
  while AP_FP_Greater(AbsReal(v), z) do
    begin
      u := (n - b) * x / n;
      t := t * u;
      v := t / (a + n);
      s := s + v;
      n := n + 1.0;
    end;
  s := s + t1;
  s := s + ai;
  u := a * ln(x);
  if AP_FP_Less(a + b, MAXGAM) and AP_FP_Less(AbsReal(u), ln(MaxRealNumber))
  then
    begin
      t := Gamma(a + b) / (Gamma(a) * Gamma(b));
      s := s * t * Power(x, a);
    end
  else
    begin
      t := LnGamma(a + b, sg) - LnGamma(a, sg) - LnGamma(b, sg) + u + ln(s);
      if AP_FP_Less(t, ln(MinRealNumber)) then
        begin
          s := 0.0;
        end
      else
        begin
          s := Exp(t);
        end;
    end;
  Result := s;
end; 
 
 
