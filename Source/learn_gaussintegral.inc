{ ****************************************************************************** }
{ * Gauss, by QQ 600585@qq.com                                                 * }
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

(* ************************************************************************
  Computation of nodes and weights for a Gauss quadrature formula

  The algorithm generates the N-point Gauss quadrature formula with weight
  function given by coefficients alpha and beta of a recurrence relation
  which generates a system of orthogonal polynomials:

  P-1(x)   =  0
  P0(x)    =  1
  Pn+1(x)  =  (x-alpha(n))*Pn(x)  -  beta(n)*Pn-1(x)

  and zeroth moment Mu0

  Mu0 = integral(W(x)dx,a,b)

  INPUT PARAMETERS:
  Alpha   :  array[0..N-1], alpha coefficients
  Beta    :  array[0..N-1], beta coefficients Zero-indexed element is not used and may be arbitrary. Beta[I]>0.
  Mu0     :  zeroth moment of the weight function.
  N       :  number of nodes of the quadrature formula, N>=1

  OUTPUT PARAMETERS:
  Info    -   error code:
  * -3    internal eigenproblem solver hasn't converged
  * -2    Beta[i]<=0
  * -1    incorrect N was passed
  *  1    OK
  X       -   array[0..N-1] - array of quadrature nodes, in ascending order.
  W       -   array[0..N-1] - array of quadrature weights.
  ************************************************************************ *)
procedure GaussQuadratureGenerateRec(const alpha, beta: TLVec; const Mu0: TLFloat; n: TLInt; var Info: TLInt; var x: TLVec; var w: TLVec);
var
  i: TLInt;
  d: TLVec;
  E: TLVec;
  z: TLMatrix;
begin
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;
  Info := 1;

  //
  // Initialize
  //
  SetLength(d, n);
  SetLength(E, n);
  i := 1;
  while i <= n - 1 do
    begin
      d[i - 1] := alpha[i - 1];
      if AP_FP_Less_Eq(beta[i], 0) then
        begin
          Info := -2;
          Exit;
        end;
      E[i - 1] := Sqrt(beta[i]);
      inc(i);
    end;
  d[n - 1] := alpha[n - 1];

  //
  // EVD
  //
  if not SMatrixTDEVD(d, E, n, 3, z) then
    begin
      Info := -3;
      Exit;
    end;

  //
  // Generate
  //
  SetLength(x, n);
  SetLength(w, n);
  i := 1;
  while i <= n do
    begin
      x[i - 1] := d[i - 1];
      w[i - 1] := Mu0 * AP_Sqr(z[0, i - 1]);
      inc(i);
    end;
end;

(* ************************************************************************
  Computation of nodes and weights for a Gauss-Lobatto quadrature formula

  The algorithm generates the N-point Gauss-Lobatto quadrature formula with
  weight function given by coefficients alpha and beta of a recurrence which
  generates a system of orthogonal polynomials.

  P-1(x)   =  0
  P0(x)    =  1
  Pn+1(x)  =  (x-alpha(n))*Pn(x)  -  beta(n)*Pn-1(x)

  and zeroth moment Mu0

  Mu0 = integral(W(x)dx,a,b)

  INPUT PARAMETERS:
  Alpha   :  array[0..N-2], alpha coefficients
  Beta    :  array[0..N-2], beta coefficients. Zero-indexed element is not used, may be arbitrary. Beta[I]>0
  Mu0     :  zeroth moment of the weighting function.
  A       :  left boundary of the integration interval.
  B       :  right boundary of the integration interval.
  N       :  number of nodes of the quadrature formula, N>=3 (including the left and right boundary nodes).

  OUTPUT PARAMETERS:
  Info    -   error code:
  * -3    internal eigenproblem solver hasn't converged
  * -2    Beta[i]<=0
  * -1    incorrect N was passed
  *  1    OK
  X       -   array[0..N-1] - array of quadrature nodes, in ascending order.
  W       -   array[0..N-1] - array of quadrature weights.
  ************************************************************************ *)
procedure GaussQuadratureGenerateGaussLobattoRec(const alpha, beta: TLVec; const Mu0, a, b: TLFloat; n: TLInt; var Info: TLInt; var x: TLVec; var w: TLVec);
var
  NewAlpha, NewBeta: TLVec;
  i: TLInt;
  d: TLVec;
  E: TLVec;
  z: TLMatrix;
  PIM1A: TLFloat;
  PIA: TLFloat;
  PIM1B: TLFloat;
  PIB: TLFloat;
  t: TLFloat;
  A11: TLFloat;
  A12: TLFloat;
  A21: TLFloat;
  A22: TLFloat;
  b1: TLFloat;
  b2: TLFloat;
  Alph: TLFloat;
  Bet: TLFloat;
begin
  if n <= 2 then
    begin
      Info := -1;
      Exit;
    end;

  NewAlpha := DynamicArrayCopy(alpha);
  NewBeta := DynamicArrayCopy(beta);
  Info := 1;

  //
  // Initialize, D[1:N+1], E[1:N]
  //
  n := n - 2;
  SetLength(d, n + 2);
  SetLength(E, n + 1);
  i := 1;
  while i <= n + 1 do
    begin
      d[i - 1] := NewAlpha[i - 1];
      inc(i);
    end;
  i := 1;
  while i <= n do
    begin
      if AP_FP_Less_Eq(NewBeta[i], 0) then
        begin
          Info := -2;
          Exit;
        end;
      E[i - 1] := Sqrt(NewBeta[i]);
      inc(i);
    end;

  //
  // Caclulate Pn(a), Pn+1(a), Pn(b), Pn+1(b)
  //
  NewBeta[0] := 0;
  PIM1A := 0;
  PIA := 1;
  PIM1B := 0;
  PIB := 1;
  i := 1;
  while i <= n + 1 do
    begin

      //
      // Pi(a)
      //
      t := (a - NewAlpha[i - 1]) * PIA - NewBeta[i - 1] * PIM1A;
      PIM1A := PIA;
      PIA := t;

      //
      // Pi(b)
      //
      t := (b - NewAlpha[i - 1]) * PIB - NewBeta[i - 1] * PIM1B;
      PIM1B := PIB;
      PIB := t;
      inc(i);
    end;

  //
  // Calculate NewAlpha'(n+1), NewBeta'(n+1)
  //
  A11 := PIA;
  A12 := PIM1A;
  A21 := PIB;
  A22 := PIM1B;
  b1 := a * PIA;
  b2 := b * PIB;
  if AP_FP_Greater(AbsReal(A11), AbsReal(A21)) then
    begin
      A22 := A22 - A12 * A21 / A11;
      b2 := b2 - b1 * A21 / A11;
      Bet := b2 / A22;
      Alph := (b1 - Bet * A12) / A11;
    end
  else
    begin
      A12 := A12 - A22 * A11 / A21;
      b1 := b1 - b2 * A11 / A21;
      Bet := b1 / A12;
      Alph := (b2 - Bet * A22) / A21;
    end;
  if AP_FP_Less(Bet, 0) then
    begin
      Info := -3;
      Exit;
    end;
  d[n + 1] := Alph;
  E[n] := Sqrt(Bet);

  //
  // EVD
  //
  if not SMatrixTDEVD(d, E, n + 2, 3, z) then
    begin
      Info := -3;
      Exit;
    end;

  //
  // Generate
  //
  SetLength(x, n + 2);
  SetLength(w, n + 2);
  i := 1;
  while i <= n + 2 do
    begin
      x[i - 1] := d[i - 1];
      w[i - 1] := Mu0 * AP_Sqr(z[0, i - 1]);
      inc(i);
    end;
end;

(* ************************************************************************
  Computation of nodes and weights for a Gauss-Radau quadrature formula

  The algorithm generates the N-point Gauss-Radau quadrature formula with
  weight function given by the coefficients alpha and beta of a recurrence
  which generates a system of orthogonal polynomials.

  P-1(x)   =  0
  P0(x)    =  1
  Pn+1(x)  =  (x-alpha(n))*Pn(x)  -  beta(n)*Pn-1(x)

  and zeroth moment Mu0

  Mu0 = integral(W(x)dx,a,b)

  INPUT PARAMETERS:
  Alpha   :  array[0..N-2], alpha coefficients.
  Beta    :  array[0..N-1], beta coefficients Zero-indexed element is not used. Beta[I]>0
  Mu0     :  zeroth moment of the weighting function.
  A       :  left boundary of the integration interval.
  N       :  number of nodes of the quadrature formula, N>=2 (including the left boundary node).

  OUTPUT PARAMETERS:
  Info    -   error code:
  * -3    internal eigenproblem solver hasn't converged
  * -2    Beta[i]<=0
  * -1    incorrect N was passed
  *  1    OK
  X       -   array[0..N-1] - array of quadrature nodes, in ascending order.
  W       -   array[0..N-1] - array of quadrature weights.
  ************************************************************************ *)
procedure GaussQuadratureGenerateGaussRadauRec(const alpha, beta: TLVec; const Mu0, a: TLFloat; n: TLInt; var Info: TLInt; var x: TLVec; var w: TLVec);
var
  NewAlpha, NewBeta: TLVec;
  i: TLInt;
  d: TLVec;
  E: TLVec;
  z: TLMatrix;
  PolIM1: TLFloat;
  PolI: TLFloat;
  t: TLFloat;
begin
  if n < 2 then
    begin
      Info := -1;
      Exit;
    end;
  NewAlpha := DynamicArrayCopy(alpha);
  NewBeta := DynamicArrayCopy(beta);
  Info := 1;

  //
  // Initialize, D[1:N], E[1:N]
  //
  n := n - 1;
  SetLength(d, n + 1);
  SetLength(E, n);
  i := 1;
  while i <= n do
    begin
      d[i - 1] := NewAlpha[i - 1];
      if AP_FP_Less_Eq(NewBeta[i], 0) then
        begin
          Info := -2;
          Exit;
        end;
      E[i - 1] := Sqrt(NewBeta[i]);
      inc(i);
    end;

  //
  // Caclulate Pn(a), Pn-1(a), and D[N+1]
  //
  NewBeta[0] := 0;
  PolIM1 := 0;
  PolI := 1;
  i := 1;
  while i <= n do
    begin
      t := (a - NewAlpha[i - 1]) * PolI - NewBeta[i - 1] * PolIM1;
      PolIM1 := PolI;
      PolI := t;
      inc(i);
    end;
  d[n] := a - NewBeta[n] * PolIM1 / PolI;

  //
  // EVD
  //
  if not SMatrixTDEVD(d, E, n + 1, 3, z) then
    begin
      Info := -3;
      Exit;
    end;

  //
  // Generate
  //
  SetLength(x, n + 1);
  SetLength(w, n + 1);
  i := 1;
  while i <= n + 1 do
    begin
      x[i - 1] := d[i - 1];
      w[i - 1] := Mu0 * AP_Sqr(z[0, i - 1]);
      inc(i);
    end;
end;

(* ************************************************************************
  Returns nodes/weights for Gauss-Legendre quadrature on [-1,1] with N nodes.

  INPUT PARAMETERS:
  N           -   number of nodes, >=1

  OUTPUT PARAMETERS:
  Info        -   error code:
  * -4    an error was detected when calculating weights/nodes. N is too large to obtain weights/nodes with high enough accuracy.
  Try to use multiple precision version.
  * -3    internal eigenproblem solver hasn't  converged
  * -1    incorrect N was passed
  * +1    OK
  X           -   array[0..N-1] - array of quadrature nodes, in ascending order.
  W           -   array[0..N-1] - array of quadrature weights.
  ************************************************************************ *)
procedure GaussQuadratureGenerateGaussLegendre(const n: TLInt; var Info: TLInt; var x: TLVec; var w: TLVec);
var
  alpha: TLVec;
  beta: TLVec;
  i: TLInt;
begin
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(alpha, n);
  SetLength(beta, n);
  i := 0;
  while i <= n - 1 do
    begin
      alpha[i] := 0;
      inc(i);
    end;
  beta[0] := 2;
  i := 1;
  while i <= n - 1 do
    begin
      beta[i] := 1 / (4 - 1 / AP_Sqr(i));
      inc(i);
    end;
  GaussQuadratureGenerateRec(alpha, beta, beta[0], n, Info, x, w);

  //
  // test basic properties to detect errors
  //
  if Info > 0 then
    begin
      if AP_FP_Less(x[0], -1) or AP_FP_Greater(x[n - 1], +1) then
        begin
          Info := -4;
        end;
      i := 0;
      while i <= n - 2 do
        begin
          if AP_FP_Greater_Eq(x[i], x[i + 1]) then
            begin
              Info := -4;
            end;
          inc(i);
        end;
    end;
end;

(* ************************************************************************
  Returns nodes/weights for Gauss-Jacobi quadrature on [-1,1] with weight function W(x)=Power(1-x,Alpha)*Power(1+x,Beta).

  INPUT PARAMETERS:
  N           -   number of nodes, >=1
  Alpha       -   power-law coefficient, Alpha>-1
  Beta        -   power-law coefficient, Beta>-1

  OUTPUT PARAMETERS:
  Info        -   error code:
  * -4 an error was detected when calculating weights/nodes. Alpha or Beta are too close to -1 to obtain weights/nodes with high enough accuracy, or may be,
  N is too large. Try to use multiple precision version.
  * -3    internal eigenproblem solver hasn't converged
  * -1    incorrect N/Alpha/Beta was passed
  * +1    OK
  X           -   array[0..N-1] - array of quadrature nodes, in ascending order.
  W           -   array[0..N-1] - array of quadrature weights.
  ************************************************************************ *)
procedure GaussQuadratureGenerateGaussJacobi(const n: TLInt; const alpha, beta: TLFloat; var Info: TLInt; var x: TLVec; var w: TLVec);
var
  a: TLVec;
  b: TLVec;
  Alpha2: TLFloat;
  Beta2: TLFloat;
  APB: TLFloat;
  t: TLFloat;
  i: TLInt;
  s: TLFloat;
begin
  if (n < 1) or AP_FP_Less_Eq(alpha, -1) or AP_FP_Less_Eq(beta, -1) then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(a, n);
  SetLength(b, n);
  APB := alpha + beta;
  a[0] := (beta - alpha) / (APB + 2);
  t := (APB + 1) * ln(2) + LnGamma(alpha + 1, s) + LnGamma(beta + 1, s) -
    LnGamma(APB + 2, s);
  if AP_FP_Greater(t, ln(MaxRealNumber)) then
    begin
      Info := -4;
      Exit;
    end;
  b[0] := Exp(t);
  if n > 1 then
    begin
      Alpha2 := AP_Sqr(alpha);
      Beta2 := AP_Sqr(beta);
      a[1] := (Beta2 - Alpha2) / ((APB + 2) * (APB + 4));
      b[1] := 4 * (alpha + 1) * (beta + 1) / ((APB + 3) * AP_Sqr(APB + 2));
      i := 2;
      while i <= n - 1 do
        begin
          a[i] := 0.25 * (Beta2 - Alpha2) /
            (i * i * (1 + 0.5 * APB / i) * (1 + 0.5 * (APB + 2) / i));
          b[i] := 0.25 * (1 + alpha / i) * (1 + beta / i) * (1 + APB / i) /
            ((1 + 0.5 * (APB + 1) / i) * (1 + 0.5 * (APB - 1) / i) *
            AP_Sqr(1 + 0.5 * APB / i));
          inc(i);
        end;
    end;
  GaussQuadratureGenerateRec(a, b, b[0], n, Info, x, w);

  //
  // test basic properties to detect errors
  //
  if Info > 0 then
    begin
      if AP_FP_Less(x[0], -1) or AP_FP_Greater(x[n - 1], +1) then
        begin
          Info := -4;
        end;
      i := 0;
      while i <= n - 2 do
        begin
          if AP_FP_Greater_Eq(x[i], x[i + 1]) then
            begin
              Info := -4;
            end;
          inc(i);
        end;
    end;
end;

(* ************************************************************************
  Returns nodes/weights for Gauss-Laguerre quadrature on (0,+inf) with weight function W(x)=Power(x,Alpha)*Exp(-x)

  INPUT PARAMETERS:
  N           -   number of nodes, >=1
  Alpha       -   power-law coefficient, Alpha>-1

  OUTPUT PARAMETERS:
  Info        -   error code:
  * -4    an error was detected when calculating weights/nodes. Alpha is too close to -1 to obtain weights/nodes with high enough accuracy or,
  may be N is too large. Try to use multiple precision version.
  * -3    internal eigenproblem solver hasn't converged
  * -1    incorrect N/Alpha was passed
  * +1    OK
  X           -   array[0..N-1] - array of quadrature nodes, in ascending order.
  W           -   array[0..N-1] - array of quadrature weights.
  ************************************************************************ *)
procedure GaussQuadratureGenerateGaussLaguerre(const n: TLInt; const alpha: TLFloat; var Info: TLInt; var x: TLVec; var w: TLVec);
var
  a: TLVec;
  b: TLVec;
  t: TLFloat;
  i: TLInt;
  s: TLFloat;
begin
  if (n < 1) or AP_FP_Less_Eq(alpha, -1) then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(a, n);
  SetLength(b, n);
  a[0] := alpha + 1;
  t := LnGamma(alpha + 1, s);
  if AP_FP_Greater_Eq(t, ln(MaxRealNumber)) then
    begin
      Info := -4;
      Exit;
    end;
  b[0] := Exp(t);
  if n > 1 then
    begin
      i := 1;
      while i <= n - 1 do
        begin
          a[i] := 2 * i + alpha + 1;
          b[i] := i * (i + alpha);
          inc(i);
        end;
    end;
  GaussQuadratureGenerateRec(a, b, b[0], n, Info, x, w);

  //
  // test basic properties to detect errors
  //
  if Info > 0 then
    begin
      if AP_FP_Less(x[0], 0) then
        begin
          Info := -4;
        end;
      i := 0;
      while i <= n - 2 do
        begin
          if AP_FP_Greater_Eq(x[i], x[i + 1]) then
            begin
              Info := -4;
            end;
          inc(i);
        end;
    end;
end;

(* ************************************************************************
  Returns nodes/weights for Gauss-Hermite quadrature on (-inf,+inf) with weight function W(x)=Exp(-x*x)

  INPUT PARAMETERS:
  N           -   number of nodes, >=1

  OUTPUT PARAMETERS:
  Info        -   error code:
  * -4    an error was detected when calculating weights/nodes. May be, N is too large. Try to use multiple precision version.
  * -3    internal eigenproblem solver hasn't converged
  * -1    incorrect N/Alpha was passed
  * +1    OK
  X           -   array[0..N-1] - array of quadrature nodes, in ascending order.
  W           -   array[0..N-1] - array of quadrature weights.
  ************************************************************************ *)
procedure GaussQuadratureGenerateGaussHermite(const n: TLInt; var Info: TLInt; var x: TLVec; var w: TLVec);
var
  a: TLVec;
  b: TLVec;
  i: TLInt;
begin
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(a, n);
  SetLength(b, n);
  i := 0;
  while i <= n - 1 do
    begin
      a[i] := 0;
      inc(i);
    end;
  b[0] := Sqrt(4 * ArcTan(1));
  if n > 1 then
    begin
      i := 1;
      while i <= n - 1 do
        begin
          b[i] := 0.5 * i;
          inc(i);
        end;
    end;
  GaussQuadratureGenerateRec(a, b, b[0], n, Info, x, w);

  //
  // test basic properties to detect errors
  //
  if Info > 0 then
    begin
      i := 0;
      while i <= n - 2 do
        begin
          if AP_FP_Greater_Eq(x[i], x[i + 1]) then
            begin
              Info := -4;
            end;
          inc(i);
        end;
    end;
end;

(* ************************************************************************
  Computation of nodes and weights of a Gauss-Kronrod quadrature formula

  The algorithm generates the N-point Gauss-Kronrod quadrature formula  with
  weight  function  given  by  coefficients  alpha  and beta of a recurrence
  relation which generates a system of orthogonal polynomials:

  P-1(x)   =  0
  P0(x)    =  1
  Pn+1(x)  =  (x-alpha(n))*Pn(x)  -  beta(n)*Pn-1(x)

  and zero moment Mu0

  Mu0 = integral(W(x)dx,a,b)

  INPUT PARAMETERS:
  Alpha       :  alpha coefficients, array[0..floor(3*K/2)].
  Beta        :  beta coefficients,  array[0..ceil(3*K/2)]. Beta[0] is not used and may be arbitrary. Beta[I]>0.
  Mu0         :  zeroth moment of the weight function.
  N           :  number of nodes of the Gauss-Kronrod quadrature formula, N >= 3, N = 2*K+1.

  OUTPUT PARAMETERS:
  Info        -   error code:
  * -5    no real and positive Gauss-Kronrod formula can be created for such a weight function with a given number of nodes.
  * -4    N is too large, task may be ill  conditioned - x[i]=x[i+1] found.
  * -3    internal eigenproblem solver hasn't converged
  * -2    Beta[i]<=0
  * -1    incorrect N was passed
  * +1    OK
  X           -   array[0..N-1] - array of quadrature nodes, in ascending order.
  WKronrod    -   array[0..N-1] - Kronrod weights
  WGauss      -   array[0..N-1] - Gauss weights (interleaved with zeros corresponding to extended Kronrod nodes).
  ************************************************************************ *)
procedure GaussKronrodQuadratureGenerateRec(const alpha, beta: TLVec; const Mu0: TLFloat; n: TLInt; var Info: TLInt; var x, WKronrod, WGauss: TLVec);
var
  NewAlpha, NewBeta: TLVec;
  TA: TLVec;
  i: TLInt;
  j: TLInt;
  t: TLVec;
  s: TLVec;
  WLen: TLInt;
  WOffs: TLInt;
  u: TLFloat;
  M: TLInt;
  L: TLInt;
  k: TLInt;
  XGTmp: TLVec;
  WGTmp: TLVec;
begin
  if (n mod 2 <> 1) or (n < 3) then
    begin
      Info := -1;
      Exit;
    end;
  i := 0;
  while i <= Ceil(AP_Float(3 * (n div 2)) / 2) do
    begin
      if AP_FP_Less_Eq(beta[i], 0) then
        begin
          Info := -2;
          Exit;
        end;
      inc(i);
    end;

  NewAlpha := DynamicArrayCopy(alpha);
  NewBeta := DynamicArrayCopy(beta);
  Info := 1;

  //
  // from external conventions about N/NewBeta/Mu0 to internal
  //
  n := n div 2;
  NewBeta[0] := Mu0;

  //
  // Calculate Gauss nodes/weights, save them for later processing
  //
  GaussQuadratureGenerateRec(NewAlpha, NewBeta, Mu0, n, Info, XGTmp, WGTmp);
  if Info < 0 then
    begin
      Exit;
    end;

  //
  // Resize:
  // * A from 0..floor(3*n/2) to 0..2*n
  // * B from 0..ceil(3*n/2)  to 0..2*n
  //
  SetLength(TA, Floor(AP_Float(3 * n) / 2) + 1);
  APVMove(@TA[0], 0, Floor(AP_Float(3 * n) / 2), @NewAlpha[0], 0, Floor(AP_Float(3 * n) / 2));
  SetLength(NewAlpha, 2 * n + 1);
  APVMove(@NewAlpha[0], 0, Floor(AP_Float(3 * n) / 2), @TA[0], 0, Floor(AP_Float(3 * n) / 2));
  i := Floor(AP_Float(3 * n) / 2) + 1;
  while i <= 2 * n do
    begin
      NewAlpha[i] := 0;
      inc(i);
    end;
  SetLength(TA, Ceil(AP_Float(3 * n) / 2) + 1);
  APVMove(@TA[0], 0, Ceil(AP_Float(3 * n) / 2), @NewBeta[0], 0, Ceil(AP_Float(3 * n) / 2));
  SetLength(NewBeta, 2 * n + 1);
  APVMove(@NewBeta[0], 0, Ceil(AP_Float(3 * n) / 2), @TA[0], 0, Ceil(AP_Float(3 * n) / 2));
  i := Ceil(AP_Float(3 * n) / 2) + 1;
  while i <= 2 * n do
    begin
      NewBeta[i] := 0;
      inc(i);
    end;

  //
  // Initialize T, S
  //
  WLen := 2 + n div 2;
  SetLength(t, WLen);
  SetLength(s, WLen);
  SetLength(TA, WLen);
  WOffs := 1;
  i := 0;
  while i <= WLen - 1 do
    begin
      t[i] := 0;
      s[i] := 0;
      inc(i);
    end;

  //
  // Algorithm from Dirk P. Laurie, "Calculation of Gauss-Kronrod quadrature rules", 1997.
  //
  t[WOffs + 0] := NewBeta[n + 1];
  M := 0;
  while M <= n - 2 do
    begin
      u := 0;
      k := (M + 1) div 2;
      while k >= 0 do
        begin
          L := M - k;
          u := u + (NewAlpha[k + n + 1] - NewAlpha[L]) * t[WOffs + k] + NewBeta[k + n + 1] * s[WOffs + k - 1] - NewBeta[L] * s[WOffs + k];
          s[WOffs + k] := u;
          dec(k);
        end;
      APVMove(@TA[0], 0, WLen - 1, @t[0], 0, WLen - 1);
      APVMove(@t[0], 0, WLen - 1, @s[0], 0, WLen - 1);
      APVMove(@s[0], 0, WLen - 1, @TA[0], 0, WLen - 1);
      inc(M);
    end;
  j := n div 2;
  while j >= 0 do
    begin
      s[WOffs + j] := s[WOffs + j - 1];
      dec(j);
    end;
  M := n - 1;
  while M <= 2 * n - 3 do
    begin
      u := 0;
      k := M + 1 - n;
      while k <= (M - 1) div 2 do
        begin
          L := M - k;
          j := n - 1 - L;
          u := u - (NewAlpha[k + n + 1] - NewAlpha[L]) * t[WOffs + j] - NewBeta[k + n + 1] * s[WOffs + j] + NewBeta[L] * s[WOffs + j + 1];
          s[WOffs + j] := u;
          inc(k);
        end;
      if M mod 2 = 0 then
        begin
          k := M div 2;
          NewAlpha[k + n + 1] := NewAlpha[k] + (s[WOffs + j] - NewBeta[k + n + 1] * s[WOffs + j + 1]) / t[WOffs + j + 1];
        end
      else
        begin
          k := (M + 1) div 2;
          NewBeta[k + n + 1] := s[WOffs + j] / s[WOffs + j + 1];
        end;
      APVMove(@TA[0], 0, WLen - 1, @t[0], 0, WLen - 1);
      APVMove(@t[0], 0, WLen - 1, @s[0], 0, WLen - 1);
      APVMove(@s[0], 0, WLen - 1, @TA[0], 0, WLen - 1);
      inc(M);
    end;
  NewAlpha[2 * n] := NewAlpha[n - 1] - NewBeta[2 * n] * s[WOffs + 0] / t[WOffs + 0];

  //
  // calculation of Kronrod nodes and weights, unpacking of Gauss weights
  //
  GaussQuadratureGenerateRec(NewAlpha, NewBeta, Mu0, 2 * n + 1, Info, x, WKronrod);
  if Info = -2 then
    begin
      Info := -5;
    end;
  if Info < 0 then
    begin
      Exit;
    end;
  i := 0;
  while i <= 2 * n - 1 do
    begin
      if AP_FP_Greater_Eq(x[i], x[i + 1]) then
        begin
          Info := -4;
        end;
      inc(i);
    end;
  if Info < 0 then
    begin
      Exit;
    end;
  SetLength(WGauss, 2 * n + 1);
  i := 0;
  while i <= 2 * n do
    begin
      WGauss[i] := 0;
      inc(i);
    end;
  i := 0;
  while i <= n - 1 do
    begin
      WGauss[2 * i + 1] := WGTmp[i];
      inc(i);
    end;
end;

(* ************************************************************************
  Returns Gauss and Gauss-Kronrod nodes/weights for Gauss-Legendre quadrature with N points.

  GKQLegendreCalc (calculation) or GKQLegendreTbl (precomputed table) is used depending on machine precision and number of nodes.

  INPUT PARAMETERS:
  N           -   number of Kronrod nodes, must be odd number, >=3.

  OUTPUT PARAMETERS:
  Info        -   error code:
  * -4    an error was detected when calculating weights/nodes. N is too large to obtain weights/nodes with high enough accuracy. Try to use multiple precision version.
  * -3    internal eigenproblem solver hasn't converged
  * -1    incorrect N was passed
  * +1    OK
  X           -   array[0..N-1] - array of quadrature nodes, ordered in ascending order.
  WKronrod    -   array[0..N-1] - Kronrod weights
  WGauss      -   array[0..N-1] - Gauss weights (interleaved with zeros corresponding to extended Kronrod nodes).
  ************************************************************************ *)
procedure GaussKronrodQuadratureGenerateGaussLegendre(const n: TLInt; var Info: TLInt; var x, WKronrod, WGauss: TLVec);
var
  Eps: TLFloat;
begin
  if AP_FP_Greater(MachineEpsilon, 1.0E-32) and
    ((n = 15) or (n = 21) or (n = 31) or (n = 41) or (n = 51) or (n = 61)) then
    begin
      Info := 1;
      GaussKronrodQuadratureLegendreTbl(n, x, WKronrod, WGauss, Eps);
    end
  else
    begin
      GaussKronrodQuadratureLegendreCalc(n, Info, x, WKronrod, WGauss);
    end;
end;

(* ************************************************************************
  Returns Gauss and Gauss-Kronrod nodes/weights for Gauss-Jacobi quadrature on [-1,1] with weight function
  W(x)=Power(1-x,Alpha)*Power(1+x,Beta).

  INPUT PARAMETERS:
  N           -   number of Kronrod nodes, must be odd number, >=3.
  Alpha       -   power-law coefficient, Alpha>-1
  Beta        -   power-law coefficient, Beta>-1

  OUTPUT PARAMETERS:
  Info        -   error code:
  * -5    no real and positive Gauss-Kronrod formula can be created for such a weight function  with  a given number of nodes.
  * -4 an error was detected when calculating weights/nodes. Alpha or Beta are too close to -1 to obtain weights/nodes with high enough accuracy, or, may be, N is too large. Try to use multiple precision version.
  * -3    internal eigenproblem solver hasn't converged
  * -1    incorrect N was passed
  * +1    OK
  * +2    OK, but quadrature rule have exterior  nodes, x[0]<-1 or x[n-1]>+1
  X           -   array[0..N-1] - array of quadrature nodes, ordered in ascending order.
  WKronrod    -   array[0..N-1] - Kronrod weights
  WGauss      -   array[0..N-1] - Gauss weights (interleaved with zeros corresponding to extended Kronrod nodes).
  ************************************************************************ *)
procedure GaussKronrodQuadratureGenerateGaussJacobi(const n: TLInt; const alpha, beta: TLFloat; var Info: TLInt; var x, WKronrod, WGauss: TLVec);
var
  clen: TLInt;
  a: TLVec;
  b: TLVec;
  Alpha2: TLFloat;
  Beta2: TLFloat;
  APB: TLFloat;
  t: TLFloat;
  i: TLInt;
  s: TLFloat;
begin
  if (n mod 2 <> 1) or (n < 3) then
    begin
      Info := -1;
      Exit;
    end;
  if AP_FP_Less_Eq(alpha, -1) or AP_FP_Less_Eq(beta, -1) then
    begin
      Info := -1;
      Exit;
    end;
  clen := Ceil(AP_Float(3 * (n div 2)) / 2) + 1;
  SetLength(a, clen);
  SetLength(b, clen);
  i := 0;
  while i <= clen - 1 do
    begin
      a[i] := 0;
      inc(i);
    end;
  APB := alpha + beta;
  a[0] := (beta - alpha) / (APB + 2);
  t := (APB + 1) * ln(2) + LnGamma(alpha + 1, s) + LnGamma(beta + 1, s) -
    LnGamma(APB + 2, s);
  if AP_FP_Greater(t, ln(MaxRealNumber)) then
    begin
      Info := -4;
      Exit;
    end;
  b[0] := Exp(t);
  if clen > 1 then
    begin
      Alpha2 := AP_Sqr(alpha);
      Beta2 := AP_Sqr(beta);
      a[1] := (Beta2 - Alpha2) / ((APB + 2) * (APB + 4));
      b[1] := 4 * (alpha + 1) * (beta + 1) / ((APB + 3) * AP_Sqr(APB + 2));
      i := 2;
      while i <= clen - 1 do
        begin
          a[i] := 0.25 * (Beta2 - Alpha2) /
            (i * i * (1 + 0.5 * APB / i) * (1 + 0.5 * (APB + 2) / i));
          b[i] := 0.25 * (1 + alpha / i) * (1 + beta / i) * (1 + APB / i) /
            ((1 + 0.5 * (APB + 1) / i) * (1 + 0.5 * (APB - 1) / i) *
            AP_Sqr(1 + 0.5 * APB / i));
          inc(i);
        end;
    end;
  GaussKronrodQuadratureGenerateRec(a, b, b[0], n, Info, x, WKronrod, WGauss);

  //
  // test basic properties to detect errors
  //
  if Info > 0 then
    begin
      if AP_FP_Less(x[0], -1) or AP_FP_Greater(x[n - 1], +1) then
        begin
          Info := 2;
        end;
      i := 0;
      while i <= n - 2 do
        begin
          if AP_FP_Greater_Eq(x[i], x[i + 1]) then
            begin
              Info := -4;
            end;
          inc(i);
        end;
    end;
end;

(* ************************************************************************
  Returns Gauss and Gauss-Kronrod nodes for quadrature with N points.
  Reduction to tridiagonal eigenproblem is used.

  INPUT PARAMETERS:
  N           -   number of Kronrod nodes, must be odd number, >=3.

  OUTPUT PARAMETERS:
  Info        -   error code:
  * -4    an error was detected when calculating weights/nodes. N is too large to obtain weights/nodes with high enough accuracy. Try to use multiple precision version.
  * -3    internal eigenproblem solver hasn't converged
  * -1    incorrect N was passed
  * +1    OK
  X           -   array[0..N-1] - array of quadrature nodes, ordered in ascending order.
  WKronrod    -   array[0..N-1] - Kronrod weights
  WGauss      -   array[0..N-1] - Gauss weights (interleaved with zeros corresponding to extended Kronrod nodes).
  ************************************************************************ *)
procedure GaussKronrodQuadratureLegendreCalc(const n: TLInt; var Info: TLInt; var x, WKronrod, WGauss: TLVec);
var
  alpha: TLVec;
  beta: TLVec;
  ALen: TLInt;
  BLen: TLInt;
  Mu0: TLFloat;
  k: TLInt;
  i: TLInt;
begin
  if (n mod 2 <> 1) or (n < 3) then
    begin
      Info := -1;
      Exit;
    end;
  Mu0 := 2;
  ALen := Floor(AP_Float(3 * (n div 2)) / 2) + 1;
  BLen := Ceil(AP_Float(3 * (n div 2)) / 2) + 1;
  SetLength(alpha, ALen);
  SetLength(beta, BLen);
  k := 0;
  while k <= ALen - 1 do
    begin
      alpha[k] := 0;
      inc(k);
    end;
  beta[0] := 2;
  k := 1;
  while k <= BLen - 1 do
    begin
      beta[k] := 1 / (4 - 1 / AP_Sqr(k));
      inc(k);
    end;
  GaussKronrodQuadratureGenerateRec(alpha, beta, Mu0, n, Info, x, WKronrod, WGauss);

  //
  // test basic properties to detect errors
  //
  if Info > 0 then
    begin
      if AP_FP_Less(x[0], -1) or AP_FP_Greater(x[n - 1], +1) then
        begin
          Info := -4;
        end;
      i := 0;
      while i <= n - 2 do
        begin
          if AP_FP_Greater_Eq(x[i], x[i + 1]) then
            begin
              Info := -4;
            end;
          inc(i);
        end;
    end;
end;

(* ************************************************************************
  Returns Gauss and Gauss-Kronrod nodes for quadrature with N  points  using pre-calculated table. Nodes/weights were computed with accuracy up to 1.0E-32.
  In standard TLFloat  precision accuracy reduces to something about 2.0E-16 (depending  on your compiler's handling of long floating point constants).

  INPUT PARAMETERS:
  N           -   number of Kronrod nodes.
  N can be 15, 21, 31, 41, 51, 61.

  OUTPUT PARAMETERS:
  X           -   array[0..N-1] - array of quadrature nodes, ordered in ascending order.
  WKronrod    -   array[0..N-1] - Kronrod weights
  WGauss      -   array[0..N-1] - Gauss weights (interleaved with zeros corresponding to extended Kronrod nodes).
  ************************************************************************ *)
procedure GaussKronrodQuadratureLegendreTbl(const n: TLInt; var x, WKronrod, WGauss: TLVec; var Eps: TLFloat);
var
  i: TLInt;
  NG: TLInt;
  p1: TLIVec;
  p2: TLIVec;
  tmp: TLFloat;
begin
  Assert((n = 15) or (n = 21) or (n = 31) or (n = 41) or (n = 51) or (n = 61), 'GKQNodesTbl: incorrect N!');
  SetLength(x, n  );
  SetLength(WKronrod, n  );
  SetLength(WGauss, n  );
  i := 0;
  while i <= n - 1 do
    begin
      x[i] := 0;
      WKronrod[i] := 0;
      WGauss[i] := 0;
      inc(i);
    end;
  Eps := Max(MachineEpsilon, 1.0E-32);
  if n = 15 then
    begin
      NG := 4;
      WGauss[0] := 0.129484966168869693270611432679082;
      WGauss[1] := 0.279705391489276667901467771423780;
      WGauss[2] := 0.381830050505118944950369775488975;
      WGauss[3] := 0.417959183673469387755102040816327;
      x[0] := 0.991455371120812639206854697526329;
      x[1] := 0.949107912342758524526189684047851;
      x[2] := 0.864864423359769072789712788640926;
      x[3] := 0.741531185599394439863864773280788;
      x[4] := 0.586087235467691130294144838258730;
      x[5] := 0.405845151377397166906606412076961;
      x[6] := 0.207784955007898467600689403773245;
      x[7] := 0.000000000000000000000000000000000;
      WKronrod[0] := 0.022935322010529224963732008058970;
      WKronrod[1] := 0.063092092629978553290700663189204;
      WKronrod[2] := 0.104790010322250183839876322541518;
      WKronrod[3] := 0.140653259715525918745189590510238;
      WKronrod[4] := 0.169004726639267902826583426598550;
      WKronrod[5] := 0.190350578064785409913256402421014;
      WKronrod[6] := 0.204432940075298892414161999234649;
      WKronrod[7] := 0.209482141084727828012999174891714;
    end;
  if n = 21 then
    begin
      NG := 5;
      WGauss[0] := 0.066671344308688137593568809893332;
      WGauss[1] := 0.149451349150580593145776339657697;
      WGauss[2] := 0.219086362515982043995534934228163;
      WGauss[3] := 0.269266719309996355091226921569469;
      WGauss[4] := 0.295524224714752870173892994651338;
      x[0] := 0.995657163025808080735527280689003;
      x[1] := 0.973906528517171720077964012084452;
      x[2] := 0.930157491355708226001207180059508;
      x[3] := 0.865063366688984510732096688423493;
      x[4] := 0.780817726586416897063717578345042;
      x[5] := 0.679409568299024406234327365114874;
      x[6] := 0.562757134668604683339000099272694;
      x[7] := 0.433395394129247190799265943165784;
      x[8] := 0.294392862701460198131126603103866;
      x[9] := 0.148874338981631210884826001129720;
      x[10] := 0.000000000000000000000000000000000;
      WKronrod[0] := 0.011694638867371874278064396062192;
      WKronrod[1] := 0.032558162307964727478818972459390;
      WKronrod[2] := 0.054755896574351996031381300244580;
      WKronrod[3] := 0.075039674810919952767043140916190;
      WKronrod[4] := 0.093125454583697605535065465083366;
      WKronrod[5] := 0.109387158802297641899210590325805;
      WKronrod[6] := 0.123491976262065851077958109831074;
      WKronrod[7] := 0.134709217311473325928054001771707;
      WKronrod[8] := 0.142775938577060080797094273138717;
      WKronrod[9] := 0.147739104901338491374841515972068;
      WKronrod[10] := 0.149445554002916905664936468389821;
    end;
  if n = 31 then
    begin
      NG := 8;
      WGauss[0] := 0.030753241996117268354628393577204;
      WGauss[1] := 0.070366047488108124709267416450667;
      WGauss[2] := 0.107159220467171935011869546685869;
      WGauss[3] := 0.139570677926154314447804794511028;
      WGauss[4] := 0.166269205816993933553200860481209;
      WGauss[5] := 0.186161000015562211026800561866423;
      WGauss[6] := 0.198431485327111576456118326443839;
      WGauss[7] := 0.202578241925561272880620199967519;
      x[0] := 0.998002298693397060285172840152271;
      x[1] := 0.987992518020485428489565718586613;
      x[2] := 0.967739075679139134257347978784337;
      x[3] := 0.937273392400705904307758947710209;
      x[4] := 0.897264532344081900882509656454496;
      x[5] := 0.848206583410427216200648320774217;
      x[6] := 0.790418501442465932967649294817947;
      x[7] := 0.724417731360170047416186054613938;
      x[8] := 0.650996741297416970533735895313275;
      x[9] := 0.570972172608538847537226737253911;
      x[10] := 0.485081863640239680693655740232351;
      x[11] := 0.394151347077563369897207370981045;
      x[12] := 0.299180007153168812166780024266389;
      x[13] := 0.201194093997434522300628303394596;
      x[14] := 0.101142066918717499027074231447392;
      x[15] := 0.000000000000000000000000000000000;
      WKronrod[0] := 0.005377479872923348987792051430128;
      WKronrod[1] := 0.015007947329316122538374763075807;
      WKronrod[2] := 0.025460847326715320186874001019653;
      WKronrod[3] := 0.035346360791375846222037948478360;
      WKronrod[4] := 0.044589751324764876608227299373280;
      WKronrod[5] := 0.053481524690928087265343147239430;
      WKronrod[6] := 0.062009567800670640285139230960803;
      WKronrod[7] := 0.069854121318728258709520077099147;
      WKronrod[8] := 0.076849680757720378894432777482659;
      WKronrod[9] := 0.083080502823133021038289247286104;
      WKronrod[10] := 0.088564443056211770647275443693774;
      WKronrod[11] := 0.093126598170825321225486872747346;
      WKronrod[12] := 0.096642726983623678505179907627589;
      WKronrod[13] := 0.099173598721791959332393173484603;
      WKronrod[14] := 0.100769845523875595044946662617570;
      WKronrod[15] := 0.101330007014791549017374792767493;
    end;
  if n = 41 then
    begin
      NG := 10;
      WGauss[0] := 0.017614007139152118311861962351853;
      WGauss[1] := 0.040601429800386941331039952274932;
      WGauss[2] := 0.062672048334109063569506535187042;
      WGauss[3] := 0.083276741576704748724758143222046;
      WGauss[4] := 0.101930119817240435036750135480350;
      WGauss[5] := 0.118194531961518417312377377711382;
      WGauss[6] := 0.131688638449176626898494499748163;
      WGauss[7] := 0.142096109318382051329298325067165;
      WGauss[8] := 0.149172986472603746787828737001969;
      WGauss[9] := 0.152753387130725850698084331955098;
      x[0] := 0.998859031588277663838315576545863;
      x[1] := 0.993128599185094924786122388471320;
      x[2] := 0.981507877450250259193342994720217;
      x[3] := 0.963971927277913791267666131197277;
      x[4] := 0.940822633831754753519982722212443;
      x[5] := 0.912234428251325905867752441203298;
      x[6] := 0.878276811252281976077442995113078;
      x[7] := 0.839116971822218823394529061701521;
      x[8] := 0.795041428837551198350638833272788;
      x[9] := 0.746331906460150792614305070355642;
      x[10] := 0.693237656334751384805490711845932;
      x[11] := 0.636053680726515025452836696226286;
      x[12] := 0.575140446819710315342946036586425;
      x[13] := 0.510867001950827098004364050955251;
      x[14] := 0.443593175238725103199992213492640;
      x[15] := 0.373706088715419560672548177024927;
      x[16] := 0.301627868114913004320555356858592;
      x[17] := 0.227785851141645078080496195368575;
      x[18] := 0.152605465240922675505220241022678;
      x[19] := 0.076526521133497333754640409398838;
      x[20] := 0.000000000000000000000000000000000;
      WKronrod[0] := 0.003073583718520531501218293246031;
      WKronrod[1] := 0.008600269855642942198661787950102;
      WKronrod[2] := 0.014626169256971252983787960308868;
      WKronrod[3] := 0.020388373461266523598010231432755;
      WKronrod[4] := 0.025882133604951158834505067096153;
      WKronrod[5] := 0.031287306777032798958543119323801;
      WKronrod[6] := 0.036600169758200798030557240707211;
      WKronrod[7] := 0.041668873327973686263788305936895;
      WKronrod[8] := 0.046434821867497674720231880926108;
      WKronrod[9] := 0.050944573923728691932707670050345;
      WKronrod[10] := 0.055195105348285994744832372419777;
      WKronrod[11] := 0.059111400880639572374967220648594;
      WKronrod[12] := 0.062653237554781168025870122174255;
      WKronrod[13] := 0.065834597133618422111563556969398;
      WKronrod[14] := 0.068648672928521619345623411885368;
      WKronrod[15] := 0.071054423553444068305790361723210;
      WKronrod[16] := 0.073030690332786667495189417658913;
      WKronrod[17] := 0.074582875400499188986581418362488;
      WKronrod[18] := 0.075704497684556674659542775376617;
      WKronrod[19] := 0.076377867672080736705502835038061;
      WKronrod[20] := 0.076600711917999656445049901530102;
    end;
  if n = 51 then
    begin
      NG := 13;
      WGauss[0] := 0.011393798501026287947902964113235;
      WGauss[1] := 0.026354986615032137261901815295299;
      WGauss[2] := 0.040939156701306312655623487711646;
      WGauss[3] := 0.054904695975835191925936891540473;
      WGauss[4] := 0.068038333812356917207187185656708;
      WGauss[5] := 0.080140700335001018013234959669111;
      WGauss[6] := 0.091028261982963649811497220702892;
      WGauss[7] := 0.100535949067050644202206890392686;
      WGauss[8] := 0.108519624474263653116093957050117;
      WGauss[9] := 0.114858259145711648339325545869556;
      WGauss[10] := 0.119455763535784772228178126512901;
      WGauss[11] := 0.122242442990310041688959518945852;
      WGauss[12] := 0.123176053726715451203902873079050;
      x[0] := 0.999262104992609834193457486540341;
      x[1] := 0.995556969790498097908784946893902;
      x[2] := 0.988035794534077247637331014577406;
      x[3] := 0.976663921459517511498315386479594;
      x[4] := 0.961614986425842512418130033660167;
      x[5] := 0.942974571228974339414011169658471;
      x[6] := 0.920747115281701561746346084546331;
      x[7] := 0.894991997878275368851042006782805;
      x[8] := 0.865847065293275595448996969588340;
      x[9] := 0.833442628760834001421021108693570;
      x[10] := 0.797873797998500059410410904994307;
      x[11] := 0.759259263037357630577282865204361;
      x[12] := 0.717766406813084388186654079773298;
      x[13] := 0.673566368473468364485120633247622;
      x[14] := 0.626810099010317412788122681624518;
      x[15] := 0.577662930241222967723689841612654;
      x[16] := 0.526325284334719182599623778158010;
      x[17] := 0.473002731445714960522182115009192;
      x[18] := 0.417885382193037748851814394594572;
      x[19] := 0.361172305809387837735821730127641;
      x[20] := 0.303089538931107830167478909980339;
      x[21] := 0.243866883720988432045190362797452;
      x[22] := 0.183718939421048892015969888759528;
      x[23] := 0.122864692610710396387359818808037;
      x[24] := 0.061544483005685078886546392366797;
      x[25] := 0.000000000000000000000000000000000;
      WKronrod[0] := 0.001987383892330315926507851882843;
      WKronrod[1] := 0.005561932135356713758040236901066;
      WKronrod[2] := 0.009473973386174151607207710523655;
      WKronrod[3] := 0.013236229195571674813656405846976;
      WKronrod[4] := 0.016847817709128298231516667536336;
      WKronrod[5] := 0.020435371145882835456568292235939;
      WKronrod[6] := 0.024009945606953216220092489164881;
      WKronrod[7] := 0.027475317587851737802948455517811;
      WKronrod[8] := 0.030792300167387488891109020215229;
      WKronrod[9] := 0.034002130274329337836748795229551;
      WKronrod[10] := 0.037116271483415543560330625367620;
      WKronrod[11] := 0.040083825504032382074839284467076;
      WKronrod[12] := 0.042872845020170049476895792439495;
      WKronrod[13] := 0.045502913049921788909870584752660;
      WKronrod[14] := 0.047982537138836713906392255756915;
      WKronrod[15] := 0.050277679080715671963325259433440;
      WKronrod[16] := 0.052362885806407475864366712137873;
      WKronrod[17] := 0.054251129888545490144543370459876;
      WKronrod[18] := 0.055950811220412317308240686382747;
      WKronrod[19] := 0.057437116361567832853582693939506;
      WKronrod[20] := 0.058689680022394207961974175856788;
      WKronrod[21] := 0.059720340324174059979099291932562;
      WKronrod[22] := 0.060539455376045862945360267517565;
      WKronrod[23] := 0.061128509717053048305859030416293;
      WKronrod[24] := 0.061471189871425316661544131965264;
      WKronrod[25] := 0.061580818067832935078759824240055;
    end;
  if n = 61 then
    begin
      NG := 15;
      WGauss[0] := 0.007968192496166605615465883474674;
      WGauss[1] := 0.018466468311090959142302131912047;
      WGauss[2] := 0.028784707883323369349719179611292;
      WGauss[3] := 0.038799192569627049596801936446348;
      WGauss[4] := 0.048402672830594052902938140422808;
      WGauss[5] := 0.057493156217619066481721689402056;
      WGauss[6] := 0.065974229882180495128128515115962;
      WGauss[7] := 0.073755974737705206268243850022191;
      WGauss[8] := 0.080755895229420215354694938460530;
      WGauss[9] := 0.086899787201082979802387530715126;
      WGauss[10] := 0.092122522237786128717632707087619;
      WGauss[11] := 0.096368737174644259639468626351810;
      WGauss[12] := 0.099593420586795267062780282103569;
      WGauss[13] := 0.101762389748405504596428952168554;
      WGauss[14] := 0.102852652893558840341285636705415;
      x[0] := 0.999484410050490637571325895705811;
      x[1] := 0.996893484074649540271630050918695;
      x[2] := 0.991630996870404594858628366109486;
      x[3] := 0.983668123279747209970032581605663;
      x[4] := 0.973116322501126268374693868423707;
      x[5] := 0.960021864968307512216871025581798;
      x[6] := 0.944374444748559979415831324037439;
      x[7] := 0.926200047429274325879324277080474;
      x[8] := 0.905573307699907798546522558925958;
      x[9] := 0.882560535792052681543116462530226;
      x[10] := 0.857205233546061098958658510658944;
      x[11] := 0.829565762382768397442898119732502;
      x[12] := 0.799727835821839083013668942322683;
      x[13] := 0.767777432104826194917977340974503;
      x[14] := 0.733790062453226804726171131369528;
      x[15] := 0.697850494793315796932292388026640;
      x[16] := 0.660061064126626961370053668149271;
      x[17] := 0.620526182989242861140477556431189;
      x[18] := 0.579345235826361691756024932172540;
      x[19] := 0.536624148142019899264169793311073;
      x[20] := 0.492480467861778574993693061207709;
      x[21] := 0.447033769538089176780609900322854;
      x[22] := 0.400401254830394392535476211542661;
      x[23] := 0.352704725530878113471037207089374;
      x[24] := 0.304073202273625077372677107199257;
      x[25] := 0.254636926167889846439805129817805;
      x[26] := 0.204525116682309891438957671002025;
      x[27] := 0.153869913608583546963794672743256;
      x[28] := 0.102806937966737030147096751318001;
      x[29] := 0.051471842555317695833025213166723;
      x[30] := 0.000000000000000000000000000000000;
      WKronrod[0] := 0.001389013698677007624551591226760;
      WKronrod[1] := 0.003890461127099884051267201844516;
      WKronrod[2] := 0.006630703915931292173319826369750;
      WKronrod[3] := 0.009273279659517763428441146892024;
      WKronrod[4] := 0.011823015253496341742232898853251;
      WKronrod[5] := 0.014369729507045804812451432443580;
      WKronrod[6] := 0.016920889189053272627572289420322;
      WKronrod[7] := 0.019414141193942381173408951050128;
      WKronrod[8] := 0.021828035821609192297167485738339;
      WKronrod[9] := 0.024191162078080601365686370725232;
      WKronrod[10] := 0.026509954882333101610601709335075;
      WKronrod[11] := 0.028754048765041292843978785354334;
      WKronrod[12] := 0.030907257562387762472884252943092;
      WKronrod[13] := 0.032981447057483726031814191016854;
      WKronrod[14] := 0.034979338028060024137499670731468;
      WKronrod[15] := 0.036882364651821229223911065617136;
      WKronrod[16] := 0.038678945624727592950348651532281;
      WKronrod[17] := 0.040374538951535959111995279752468;
      WKronrod[18] := 0.041969810215164246147147541285970;
      WKronrod[19] := 0.043452539701356069316831728117073;
      WKronrod[20] := 0.044814800133162663192355551616723;
      WKronrod[21] := 0.046059238271006988116271735559374;
      WKronrod[22] := 0.047185546569299153945261478181099;
      WKronrod[23] := 0.048185861757087129140779492298305;
      WKronrod[24] := 0.049055434555029778887528165367238;
      WKronrod[25] := 0.049795683427074206357811569379942;
      WKronrod[26] := 0.050405921402782346840893085653585;
      WKronrod[27] := 0.050881795898749606492297473049805;
      WKronrod[28] := 0.051221547849258772170656282604944;
      WKronrod[29] := 0.051426128537459025933862879215781;
      WKronrod[30] := 0.051494729429451567558340433647099;
    end;

  //
  // copy nodes
  //
  i := n - 1;
  while i >= n div 2 do
    begin
      x[i] := -x[n - 1 - i];
      dec(i);
    end;

  //
  // copy Kronrod weights
  //
  i := n - 1;
  while i >= n div 2 do
    begin
      WKronrod[i] := WKronrod[n - 1 - i];
      dec(i);
    end;

  //
  // copy Gauss weights
  //
  i := NG - 1;
  while i >= 0 do
    begin
      WGauss[n - 2 - 2 * i] := WGauss[i];
      WGauss[1 + 2 * i] := WGauss[i];
      dec(i);
    end;
  i := 0;
  while i <= n div 2 do
    begin
      WGauss[2 * i] := 0;
      inc(i);
    end;

  //
  // reorder
  //
  TagSort(x, n, p1, p2);
  i := 0;
  while i <= n - 1 do
    begin
      tmp := WKronrod[i];
      WKronrod[i] := WKronrod[p2[i]];
      WKronrod[p2[i]] := tmp;
      tmp := WGauss[i];
      WGauss[i] := WGauss[p2[i]];
      WGauss[p2[i]] := tmp;
      inc(i);
    end;
end; 
 
 
