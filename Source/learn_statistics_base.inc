{ ****************************************************************************** }
{ * Gamma,Incomplete gamma integral,Poisson distribution support               * }
{ * by QQ 600585@qq.com                                                        * }
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

function Log1P(const x: TLFloat): TLFloat;
var
  z : TLFloat;
  LP: TLFloat;
  LQ: TLFloat;
begin
  z := 1.0 + x;
  if AP_FP_Less(z, 0.70710678118654752440) or
    AP_FP_Greater(z, 1.41421356237309504880) then
    begin
      Result := ln(z);
      Exit;
    end;
  z := x * x;
  LP := 4.5270000862445199635215E-5;
  LP := LP * x + 4.9854102823193375972212E-1;
  LP := LP * x + 6.5787325942061044846969E0;
  LP := LP * x + 2.9911919328553073277375E1;
  LP := LP * x + 6.0949667980987787057556E1;
  LP := LP * x + 5.7112963590585538103336E1;
  LP := LP * x + 2.0039553499201281259648E1;
  LQ := 1.0000000000000000000000E0;
  LQ := LQ * x + 1.5062909083469192043167E1;
  LQ := LQ * x + 8.3047565967967209469434E1;
  LQ := LQ * x + 2.2176239823732856465394E2;
  LQ := LQ * x + 3.0909872225312059774938E2;
  LQ := LQ * x + 2.1642788614495947685003E2;
  LQ := LQ * x + 6.0118660497603843919306E1;
  z := -0.5 * z + x * (z * LP / LQ);
  Result := x + z;
end;

function ExpM1(const x: TLFloat): TLFloat;
var
  r : TLFloat;
  xx: TLFloat;
  EP: TLFloat;
  EQ: TLFloat;
begin
  if AP_FP_Less(x, -0.5) or AP_FP_Greater(x, 0.5) then
    begin
      Result := Exp(x) - 1.0;
      Exit;
    end;
  xx := x * x;
  EP := 1.2617719307481059087798E-4;
  EP := EP * xx + 3.0299440770744196129956E-2;
  EP := EP * xx + 9.9999999999999999991025E-1;
  EQ := 3.0019850513866445504159E-6;
  EQ := EQ * xx + 2.5244834034968410419224E-3;
  EQ := EQ * xx + 2.2726554820815502876593E-1;
  EQ := EQ * xx + 2.0000000000000000000897E0;
  r := x * EP;
  r := r / (EQ - r);
  Result := r + r;
end;

function CosM1(const x: TLFloat): TLFloat;
var
  xx: TLFloat;
  c : TLFloat;
begin
  if AP_FP_Less(x, -0.25 * pi) or AP_FP_Greater(x, 0.25 * pi) then
    begin
      Result := Cos(x) - 1;
      Exit;
    end;
  xx := x * x;
  c := 4.7377507964246204691685E-14;
  c := c * xx - 1.1470284843425359765671E-11;
  c := c * xx + 2.0876754287081521758361E-9;
  c := c * xx - 2.7557319214999787979814E-7;
  c := c * xx + 2.4801587301570552304991E-5;
  c := c * xx - 1.3888888888888872993737E-3;
  c := c * xx + 4.1666666666666666609054E-2;
  Result := -0.5 * xx + xx * xx * c;
end;

function GammaStirF(const x: TLFloat): TLFloat; inline;
var
  y   : TLFloat;
  w   : TLFloat;
  v   : TLFloat;
  Stir: TLFloat;
begin
  w := 1 / x;
  Stir := 7.87311395793093628397E-4;
  Stir := -2.29549961613378126380E-4 + w * Stir;
  Stir := -2.68132617805781232825E-3 + w * Stir;
  Stir := 3.47222221605458667310E-3 + w * Stir;
  Stir := 8.33333333333482257126E-2 + w * Stir;
  w := 1 + w * Stir;
  y := Exp(x);
  if AP_FP_Greater(x, 143.01608) then
    begin
      v := Power(x, 0.5 * x - 0.25);
      y := v * (v / y);
    end
  else
    begin
      y := Power(x, x - 0.5) / y;
    end;
  Result := 2.50662827463100050242 * y * w;
end;

(* ************************************************************************
  Gamma function

  Input parameters:
  X   -   argument

  Domain:
  0 < X < 171.6
  -170 < X < 0, X is not an integer.

  Relative error:
  arithmetic   domain     # trials      peak         rms
  IEEE    -170,-33      20000       2.3e-15     3.3e-16
  IEEE     -33,  33     20000       9.4e-16     2.2e-16
  IEEE      33, 171.6   20000       2.3e-15     3.2e-16

  Cephes Math Library Release 2.8:  June, 2000
  Original copyright 1984, 1987, 1989, 1992, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function Gamma(const x: TLFloat): TLFloat;
var
  Nx    : TLFloat;
  p     : TLFloat;
  PP    : TLFloat;
  q     : TLFloat;
  QQ    : TLFloat;
  z     : TLFloat;
  i     : TLInt;
  SgnGam: TLFloat;
begin
  Nx := x;
  SgnGam := 1;
  q := AbsReal(Nx);
  if AP_FP_Greater(q, 33.0) then
    begin
      if AP_FP_Less(Nx, 0.0) then
        begin
          p := Floor(q);
          i := Round(p);
          if i mod 2 = 0 then
            begin
              SgnGam := -1;
            end;
          z := q - p;
          if AP_FP_Greater(z, 0.5) then
            begin
              p := p + 1;
              z := q - p;
            end;
          z := q * Sin(pi * z);
          z := AbsReal(z);
          z := pi / (z * GammaStirF(q));
        end
      else
        begin
          z := GammaStirF(Nx);
        end;
      Result := SgnGam * z;
      Exit;
    end;
  z := 1;
  while AP_FP_Greater_Eq(Nx, 3) do
    begin
      Nx := Nx - 1;
      z := z * Nx;
    end;
  while AP_FP_Less(Nx, 0) do
    begin
      if AP_FP_Greater(Nx, -0.000000001) then
        begin
          Result := z / ((1 + 0.5772156649015329 * Nx) * Nx);
          Exit;
        end;
      z := z / Nx;
      Nx := Nx + 1;
    end;
  while AP_FP_Less(Nx, 2) do
    begin
      if AP_FP_Less(Nx, 0.000000001) then
        begin
          Result := z / ((1 + 0.5772156649015329 * Nx) * Nx);
          Exit;
        end;
      z := z / Nx;
      Nx := Nx + 1.0;
    end;
  if AP_FP_Eq(Nx, 2) then
    begin
      Result := z;
      Exit;
    end;
  Nx := Nx - 2.0;
  PP := 1.60119522476751861407E-4;
  PP := 1.19135147006586384913E-3 + Nx * PP;
  PP := 1.04213797561761569935E-2 + Nx * PP;
  PP := 4.76367800457137231464E-2 + Nx * PP;
  PP := 2.07448227648435975150E-1 + Nx * PP;
  PP := 4.94214826801497100753E-1 + Nx * PP;
  PP := 9.99999999999999996796E-1 + Nx * PP;
  QQ := -2.31581873324120129819E-5;
  QQ := 5.39605580493303397842E-4 + Nx * QQ;
  QQ := -4.45641913851797240494E-3 + Nx * QQ;
  QQ := 1.18139785222060435552E-2 + Nx * QQ;
  QQ := 3.58236398605498653373E-2 + Nx * QQ;
  QQ := -2.34591795718243348568E-1 + Nx * QQ;
  QQ := 7.14304917030273074085E-2 + Nx * QQ;
  QQ := 1.00000000000000000320 + Nx * QQ;
  Result := z * PP / QQ;
  Exit;
end;

(* ************************************************************************
  Natural logarithm of gamma function

  Input parameters:
  X       -   argument

  Result:
  logarithm of the absolute value of the Gamma(X).

  Output parameters:
  SgnGam  -   sign(Gamma(X))

  Domain:
  0 < X < 2.55e305
  -2.55e305 < X < 0, X is not an integer.

  ACCURACY:
  arithmetic      domain        # trials     peak         rms
  IEEE    0, 3                 28000     5.4e-16     1.1e-16
  IEEE    2.718, 2.556e305     40000     3.5e-16     8.3e-17
  The error criterion was relative when the function magnitude
  was greater than one but absolute when it was less than one.

  The following test used the relative error criterion, though
  at certain points the relative error could be much higher than
  indicated.
  IEEE    -200, -4             10000     4.8e-16     1.3e-16

  Cephes Math Library Release 2.8:  June, 2000
  Copyright 1984, 1987, 1989, 1992, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function LnGamma(const x: TLFloat; var SgnGam: TLFloat): TLFloat;
var
  Nx   : TLFloat;
  a    : TLFloat;
  b    : TLFloat;
  c    : TLFloat;
  p    : TLFloat;
  q    : TLFloat;
  u    : TLFloat;
  w    : TLFloat;
  z    : TLFloat;
  i    : TLInt;
  LogPi: TLFloat;
  LS2PI: TLFloat;
  tmp  : TLFloat;
begin
  Nx := x;
  SgnGam := 1;
  LogPi := 1.14472988584940017414;
  LS2PI := 0.91893853320467274178;
  if AP_FP_Less(Nx, -34.0) then
    begin
      q := -Nx;
      w := LnGamma(q, tmp);
      p := Floor(q);
      i := Round(p);
      if i mod 2 = 0 then
        begin
          SgnGam := -1;
        end
      else
        begin
          SgnGam := 1;
        end;
      z := q - p;
      if AP_FP_Greater(z, 0.5) then
        begin
          p := p + 1;
          z := p - q;
        end;
      z := q * Sin(pi * z);
      Result := LogPi - ln(z) - w;
      Exit;
    end;
  if AP_FP_Less(Nx, 13) then
    begin
      z := 1;
      p := 0;
      u := Nx;
      while AP_FP_Greater_Eq(u, 3) do
        begin
          p := p - 1;
          u := Nx + p;
          z := z * u;
        end;
      while AP_FP_Less(u, 2) do
        begin
          z := z / u;
          p := p + 1;
          u := Nx + p;
        end;
      if AP_FP_Less(z, 0) then
        begin
          SgnGam := -1;
          z := -z;
        end
      else
        begin
          SgnGam := 1;
        end;
      if AP_FP_Eq(u, 2) then
        begin
          Result := ln(z);
          Exit;
        end;
      p := p - 2;
      Nx := Nx + p;
      b := -1378.25152569120859100;
      b := -38801.6315134637840924 + Nx * b;
      b := -331612.992738871184744 + Nx * b;
      b := -1162370.97492762307383 + Nx * b;
      b := -1721737.00820839662146 + Nx * b;
      b := -853555.664245765465627 + Nx * b;
      c := 1;
      c := -351.815701436523470549 + Nx * c;
      c := -17064.2106651881159223 + Nx * c;
      c := -220528.590553854454839 + Nx * c;
      c := -1139334.44367982507207 + Nx * c;
      c := -2532523.07177582951285 + Nx * c;
      c := -2018891.41433532773231 + Nx * c;
      p := Nx * b / c;
      Result := ln(z) + p;
      Exit;
    end;
  q := (Nx - 0.5) * ln(Nx) - Nx + LS2PI;
  if AP_FP_Greater(Nx, 100000000) then
    begin
      Result := q;
      Exit;
    end;
  p := 1 / (Nx * Nx);
  if AP_FP_Greater_Eq(Nx, 1000.0) then
    begin
      q := q + ((7.9365079365079365079365 * 0.0001 * p - 2.7777777777777777777778
        * 0.001) * p + 0.0833333333333333333333) / Nx;
    end
  else
    begin
      a := 8.11614167470508450300 * 0.0001;
      a := -5.95061904284301438324 * 0.0001 + p * a;
      a := 7.93650340457716943945 * 0.0001 + p * a;
      a := -2.77777777730099687205 * 0.001 + p * a;
      a := 8.33333333333331927722 * 0.01 + p * a;
      q := q + a / Nx;
    end;
  Result := q;
end;

(* ************************************************************************
  *Incomplete gamma integral
  *
  *The function is defined by
  *
  *                          x
  *                           -
  *                  1       | |  -t  a-1
  * igam(a,x)  =   -----     |   e   t   dt.
  *                 -      | |
  *                | (a)    -
  *                          0
  *
  *
  *In this implementation both arguments must be positive.
  *The integral is evaluated by either a power series or
  *continued fraction expansion, depending on the relative
  *values of a and x.
  *
  *ACCURACY:
  *
  *                     Relative error:
  *arithmetic   domain     # trials      peak         rms
  *   IEEE      0,30       200000       3.6e-14     2.9e-15
  *   IEEE      0,100      300000       9.9e-14     1.5e-14
  *
  *Cephes Math Library Release 2.8:  June, 2000
  *Copyright 1985, 1987, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function IncompleteGamma(const a, x: TLFloat): TLFloat;
var
  IGammaEpsilon: TLFloat;
  ans          : TLFloat;
  Ax           : TLFloat;
  c            : TLFloat;
  r            : TLFloat;
  tmp          : TLFloat;
begin
  IGammaEpsilon := 0.000000000000001;
  if AP_FP_Less_Eq(x, 0) or AP_FP_Less_Eq(a, 0) then
    begin
      Result := 0;
      Exit;
    end;
  if AP_FP_Greater(x, 1) and AP_FP_Greater(x, a) then
    begin
      Result := 1 - IncompleteGammaC(a, x);
      Exit;
    end;
  Ax := a * ln(x) - x - LnGamma(a, tmp);
  if AP_FP_Less(Ax, -709.78271289338399) then
    begin
      Result := 0;
      Exit;
    end;
  Ax := Exp(Ax);
  r := a;
  c := 1;
  ans := 1;
  repeat
    r := r + 1;
    c := c * x / r;
    ans := ans + c;
  until AP_FP_Less_Eq(c / ans, IGammaEpsilon);
  Result := ans * Ax / a;
end;

(* ************************************************************************
  *Complemented incomplete gamma integral
  *
  *The function is defined by
  *
  *
  * igamc(a,x)   =   1 - igam(a,x)
  *
  *                           inf.
  *                             -
  *                    1       | |  -t  a-1
  *              =   -----     |   e   t   dt.
  *                   -      | |
  *                  | (a)    -
  *                            x
  *
  *
  *In this implementation both arguments must be positive.
  *The integral is evaluated by either a power series or
  *continued fraction expansion, depending on the relative
  *values of a and x.
  *
  *ACCURACY:
  *
  *Tested at random a, x.
  *               a         x                      Relative error:
  *arithmetic   domain   domain     # trials      peak         rms
  *   IEEE     0.5,100   0,100      200000       1.9e-14     1.7e-15
  *   IEEE     0.01,0.5  0,100      200000       1.4e-13     1.6e-15
  *
  *Cephes Math Library Release 2.8:  June, 2000
  *Copyright 1985, 1987, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function IncompleteGammaC(const a, x: TLFloat): TLFloat;
var
  IGammaEpsilon     : TLFloat;
  IGammaBigNumber   : TLFloat;
  IGammaBigNumberInv: TLFloat;
  ans               : TLFloat;
  Ax                : TLFloat;
  c                 : TLFloat;
  yc                : TLFloat;
  r                 : TLFloat;
  t                 : TLFloat;
  y                 : TLFloat;
  z                 : TLFloat;
  pk                : TLFloat;
  pkm1              : TLFloat;
  pkm2              : TLFloat;
  qk                : TLFloat;
  qkm1              : TLFloat;
  qkm2              : TLFloat;
  tmp               : TLFloat;
begin
  IGammaEpsilon := 0.000000000000001;
  IGammaBigNumber := 4503599627370496.0;
  IGammaBigNumberInv := 2.22044604925031308085 * 0.0000000000000001;
  if AP_FP_Less_Eq(x, 0) or AP_FP_Less_Eq(a, 0) then
    begin
      Result := 1;
      Exit;
    end;
  if AP_FP_Less(x, 1) or AP_FP_Less(x, a) then
    begin
      Result := 1 - IncompleteGamma(a, x);
      Exit;
    end;
  Ax := a * ln(x) - x - LnGamma(a, tmp);
  if AP_FP_Less(Ax, -709.78271289338399) then
    begin
      Result := 0;
      Exit;
    end;
  Ax := Exp(Ax);
  y := 1 - a;
  z := x + y + 1;
  c := 0;
  pkm2 := 1;
  qkm2 := x;
  pkm1 := x + 1;
  qkm1 := z * x;
  ans := pkm1 / qkm1;
  repeat
    c := c + 1;
    y := y + 1;
    z := z + 2;
    yc := y * c;
    pk := pkm1 * z - pkm2 * yc;
    qk := qkm1 * z - qkm2 * yc;
    if AP_FP_NEq(qk, 0) then
      begin
        r := pk / qk;
        t := AbsReal((ans - r) / r);
        ans := r;
      end
    else
      begin
        t := 1;
      end;
    pkm2 := pkm1;
    pkm1 := pk;
    qkm2 := qkm1;
    qkm1 := qk;
    if AP_FP_Greater(AbsReal(pk), IGammaBigNumber) then
      begin
        pkm2 := pkm2 * IGammaBigNumberInv;
        pkm1 := pkm1 * IGammaBigNumberInv;
        qkm2 := qkm2 * IGammaBigNumberInv;
        qkm1 := qkm1 * IGammaBigNumberInv;
      end;
  until AP_FP_Less_Eq(t, IGammaEpsilon);
  Result := ans * Ax;
end;

(* ************************************************************************
  Inverse of complemented imcomplete gamma integral

  Given p, the function finds x such that

  igamc( a, x ) = p.

  Starting with the approximate value

  3
  x = a t

  where

  t = 1 - d - ndtri(p) sqrt(d)

  and

  d = 1/9a,

  the routine performs up to 10 Newton iterations to find the
  root of igamc(a,x) - p = 0.

  ACCURACY:

  Tested at random a, p in the intervals indicated.

  a        p                      Relative error:
  arithmetic   domain   domain     # trials      peak         rms
  IEEE     0.5,100   0,0.5       100000       1.0e-14     1.7e-15
  IEEE     0.01,0.5  0,0.5       100000       9.0e-14     3.4e-15
  IEEE    0.5,10000  0,0.5        20000       2.3e-13     3.8e-14

  Cephes Math Library Release 2.8:  June, 2000
  Copyright 1984, 1987, 1995, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function InvIncompleteGammaC(const a, y0: TLFloat): TLFloat;
var
  IGammaEpsilon     : TLFloat;
  IInvGammaBigNumber: TLFloat;
  x0                : TLFloat;
  x1                : TLFloat;
  x                 : TLFloat;
  yl                : TLFloat;
  yh                : TLFloat;
  y                 : TLFloat;
  d                 : TLFloat;
  lgm               : TLFloat;
  dithresh          : TLFloat;
  i                 : TLInt;
  dir               : TLInt;
  tmp               : TLFloat;
begin
  IGammaEpsilon := 0.000000000000001;
  IInvGammaBigNumber := 4503599627370496.0;
  x0 := IInvGammaBigNumber;
  yl := 0;
  x1 := 0;
  yh := 1;
  dithresh := 5 * IGammaEpsilon;
  d := 1 / (9 * a);
  y := 1 - d - InvNormalDistribution(y0) * Sqrt(d);
  x := a * y * y * y;
  lgm := LnGamma(a, tmp);
  i := 0;
  while i < 10 do
    begin
      if AP_FP_Greater(x, x0) or AP_FP_Less(x, x1) then
        begin
          d := 0.0625;
          Break;
        end;
      y := IncompleteGammaC(a, x);
      if AP_FP_Less(y, yl) or AP_FP_Greater(y, yh) then
        begin
          d := 0.0625;
          Break;
        end;
      if AP_FP_Less(y, y0) then
        begin
          x0 := x;
          yl := y;
        end
      else
        begin
          x1 := x;
          yh := y;
        end;
      d := (a - 1) * ln(x) - x - lgm;
      if AP_FP_Less(d, -709.78271289338399) then
        begin
          d := 0.0625;
          Break;
        end;
      d := -Exp(d);
      d := (y - y0) / d;
      if AP_FP_Less(AbsReal(d / x), IGammaEpsilon) then
        begin
          Result := x;
          Exit;
        end;
      x := x - d;
      i := i + 1;
    end;
  if AP_FP_Eq(x0, IInvGammaBigNumber) then
    begin
      if AP_FP_Less_Eq(x, 0) then
        begin
          x := 1;
        end;
      while AP_FP_Eq(x0, IInvGammaBigNumber) do
        begin
          x := (1 + d) * x;
          y := IncompleteGammaC(a, x);
          if AP_FP_Less(y, y0) then
            begin
              x0 := x;
              yl := y;
              Break;
            end;
          d := d + d;
        end;
    end;
  d := 0.5;
  dir := 0;
  i := 0;
  while i < 400 do
    begin
      x := x1 + d * (x0 - x1);
      y := IncompleteGammaC(a, x);
      lgm := (x0 - x1) / (x1 + x0);
      if AP_FP_Less(AbsReal(lgm), dithresh) then
        begin
          Break;
        end;
      lgm := (y - y0) / y0;
      if AP_FP_Less(AbsReal(lgm), dithresh) then
        begin
          Break;
        end;
      if AP_FP_Less_Eq(x, 0.0) then
        begin
          Break;
        end;
      if AP_FP_Greater_Eq(y, y0) then
        begin
          x1 := x;
          yh := y;
          if dir < 0 then
            begin
              dir := 0;
              d := 0.5;
            end
          else
            begin
              if dir > 1 then
                begin
                  d := 0.5 * d + 0.5;
                end
              else
                begin
                  d := (y0 - yl) / (yh - yl);
                end;
            end;
          dir := dir + 1;
        end
      else
        begin
          x0 := x;
          yl := y;
          if dir > 0 then
            begin
              dir := 0;
              d := 0.5;
            end
          else
            begin
              if dir < -1 then
                begin
                  d := 0.5 * d;
                end
              else
                begin
                  d := (y0 - yl) / (yh - yl);
                end;
            end;
          dir := dir - 1;
        end;
      i := i + 1;
    end;
  Result := x;
end;

(* ************************************************************************
  * Poisson distribution
  *
  * Returns the sum of the first k+1 terms of the Poisson
  * distribution:
  *
  *   k         j
  *   --   -m  m
  *   >   e    --
  *   --       j!
  *  j=0
  *
  * The terms are not summed directly; instead the incomplete
  * gamma integral is employed, according to the relation
  *
  * y = pdtr( k, m ) = igamc( k+1, m ).
  *
  * The arguments must both be positive.
  * ACCURACY:
  *
  * See incomplete gamma function
  *
  * Cephes Math Library Release 2.8:  June, 2000
  * Copyright 1984, 1987, 1995, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function PoissonDistribution(k: TLInt; M: TLFloat): TLFloat;
begin
  Assert((k >= 0) and AP_FP_Greater(M, 0), 'Domain error in PoissonDistribution');
  Result := IncompleteGammaC(k + 1, M);
end;

(* ************************************************************************
  * Complemented Poisson distribution
  *
  * Returns the sum of the terms k+1 to infinity of the Poisson
  * distribution:
  *
  *  inf.       j
  *   --   -m  m
  *   >   e    --
  *   --       j!
  *  j=k+1
  *
  * The terms are not summed directly; instead the incomplete
  * gamma integral is employed, according to the formula
  *
  * y = pdtrc( k, m ) = igam( k+1, m ).
  *
  * The arguments must both be positive.
  *
  * ACCURACY:
  *
  * See incomplete gamma function
  *
  * Cephes Math Library Release 2.8:  June, 2000
  * Copyright 1984, 1987, 1995, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function PoissonCDistribution(k: TLInt; M: TLFloat): TLFloat;
begin
  Assert((k >= 0) and AP_FP_Greater(M, 0), 'Domain error in PoissonDistributionC');
  Result := IncompleteGamma(k + 1, M);
end;

(* ************************************************************************
  Inverse Poisson distribution

  Finds the Poisson variable x such that the integral
  from 0 to x of the Poisson density is equal to the
  given probability y.

  This is accomplished using the inverse gamma integral
  function and the relation

  m = igami( k+1, y ).

  ACCURACY:

  See inverse incomplete gamma function

  Cephes Math Library Release 2.8:  June, 2000
  Copyright 1984, 1987, 1995, 2000 by Stephen L. Moshier
  ************************************************************************ *)
function InvPoissonDistribution(k: TLInt; y: TLFloat): TLFloat;
begin
  Assert((k >= 0) and AP_FP_Greater_Eq(y, 0) and AP_FP_Less(y, 1), 'Domain error in InvPoissonDistribution');
  Result := InvIncompleteGammaC(k + 1, y);
end; 
 
 
