{ ****************************************************************************** }
{ * fitting support: Least squares,Barycentric,Polynomial,Spline1D, by.600585  * }
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

(* ************************************************************************
  This function makes periodic mapping of X to [A,B].

  It accepts X, A, B (A>B). It returns T which lies in  [A,B] and integer K, such that X = T + K*(B-A).

  NOTES:
  * K is represented as real value, although actually it is integer
  * T is guaranteed to be in [A,B]
  * T replaces X
  ************************************************************************ *)
procedure APPeriodicMap(var x: TLFloat; a: TLFloat; b: TLFloat; var k: TLFloat);
begin
  Assert(AP_FP_Less(a, b), 'APPeriodicMap: internal error!');
  k := Floor((x - a) / (b - a));
  x := x - k * (b - a);
  while AP_FP_Less(x, a) do
    begin
      x := x + (b - a);
      k := k - 1;
    end;
  while AP_FP_Greater(x, b) do
    begin
      x := x - (b - a);
      k := k + 1;
    end;
  x := Max(x, a);
  x := Min(x, b);
end;

procedure LSFitLinearInternal(y: TLVec; w: TLVec; FMatrix: TLMatrix; n: TLInt; M: TLInt;
  var Info: TLInt; var c: TLVec; var Rep: TLSFitReport); forward;
procedure LSFitClearRequestFields(var State: TLSFitState); forward;

(* ************************************************************************
  Weighted linear least squares fitting.

  QR decomposition is used to reduce task to MxM, then triangular solver  or
  SVD-based solver is used depending on condition number of the  system.  It
  allows to maximize speed and retain decent accuracy.

  INPUT PARAMETERS:
  Y       -   array[0..N-1] Function values in  N  points.
  W       -   array[0..N-1]  Weights  corresponding to function  values.
  Each summand in square  sum  of  approximation  deviations
  from  given  values  is  multiplied  by  the   square   of
  corresponding weight.
  FMatrix -   a table of basis functions values, array[0..N-1, 0..M-1].
  FMatrix[I, J] - value of J-th basis function in I-th point.
  N       -   number of points used. N>=1.
  M       -   number of basis functions, M>=1.

  OUTPUT PARAMETERS:
  Info    -   error code:
  * -4    internal SVD decomposition subroutine failed (very rare and for degenerate systems only)
  * -1    incorrect N/M were specified
  *  1    task is solved
  C       -   decomposition coefficients, array[0..M-1]
  Rep     -   fitting report. Following fields are set:
  * Rep.TaskRCond     reciprocal of condition number
  * RMSError          rms error on the (X,Y).
  * AvgError          average error on the (X,Y).
  * AvgRelError       average relative error on the non-zero Y
  * MaxError          maximum error
  NON-WEIGHTED ERRORS ARE CALCULATED

  SEE ALSO
  LSFitLinear
  LSFitLinearC
  LSFitLinearWC
  ************************************************************************ *)
procedure LSFitLinearW(y: TLVec; w: TLVec; FMatrix: TLMatrix; n: TLInt; M: TLInt;
  var Info: TLInt; var c: TLVec; var Rep: TLSFitReport);
begin
  LSFitLinearInternal(y, w, FMatrix, n, M, Info, c, Rep);
end;

(* ************************************************************************
  Weighted constained linear least squares fitting.

  This is variation of LSFitLinearW(), which searchs for min|A*x=b| given that K additional constaints C*x=bc are satisfied.
  It reduces original task to modified one: min|B*y-d| WITHOUT constraints, then LSFitLinearW() is called.

  INPUT PARAMETERS:
  Y       -   array[0..N-1] Function values in  N  points.
  W       -   array[0..N-1]  Weights  corresponding to function  values.
  Each summand in square sum of approximation deviations from given values is multiplied by the square of corresponding weight.
  FMatrix -   a table of basis functions values, array[0..N-1, 0..M-1].
  FMatrix[I,J] - value of J-th basis function in I-th point.
  CMatrix -   a table of constaints, array[0..K-1,0..M].
  I-th row of CMatrix corresponds to I-th linear constraint:
  CMatrix[I,0]*C[0] + ... + CMatrix[I,M-1]*C[M-1] = CMatrix[I,M]
  N       -   number of points used. N>=1.
  M       -   number of basis functions, M>=1.
  K       -   number of constraints, 0 <= K < M
  K=0 corresponds to absence of constraints.

  OUTPUT PARAMETERS:
  Info    -   error code:
  * -4    internal SVD decomposition subroutine failed (very rare and for degenerate systems only)
  * -3    either too many constraints (M or more), degenerate constraints (some constraints are repetead twice) or inconsistent constraints were specified.
  * -1    incorrect N/M/K were specified
  *  1    task is solved
  C       -   decomposition coefficients, array[0..M-1]
  Rep     -   fitting report. Following fields are set:
  * RMSError          rms error on the (X,Y).
  * AvgError          average error on the (X,Y).
  * AvgRelError       average relative error on the non-zero Y
  * MaxError          maximum error
  NON-WEIGHTED ERRORS ARE CALCULATED

  IMPORTANT:
  this subroitine doesn't calculate task's condition number for K<>0.

  SEE ALSO
  LSFitLinear
  LSFitLinearC
  LSFitLinearWC
  ************************************************************************ *)
procedure LSFitLinearWC(y: TLVec; w: TLVec; FMatrix: TLMatrix; CMatrix: TLMatrix; n: TLInt; M: TLInt; k: TLInt;
  var Info: TLInt; var c: TLVec; var Rep: TLSFitReport);
var
  i: TLInt;
  j: TLInt;
  TAU: TLVec;
  q: TLMatrix;
  f2: TLMatrix;
  tmp: TLVec;
  C0: TLVec;
  v: TLFloat;
begin
  y := DynamicArrayCopy(y);
  CMatrix := DynamicArrayCopy(CMatrix);
  if (n < 1) or (M < 1) or (k < 0) then
    begin
      Info := -1;
      Exit;
    end;
  if k >= M then
    begin
      Info := -3;
      Exit;
    end;

  //
  // Solve
  //
  if k = 0 then
    begin

      //
      // no constraints
      //
      LSFitLinearInternal(y, w, FMatrix, n, M, Info, c, Rep);
    end
  else
    begin

      //
      // First, find general form solution of constraints system:
      // * factorize C = L*Q
      // * unpack Q
      // * fill upper part of C with zeros (for RCond)
      //
      // We got C=C0+Q2'*y where Q2 is lower M-K rows of Q.
      //
      RMatrixLQ(CMatrix, k, M, TAU);
      RMatrixLQUnpackQ(CMatrix, k, M, TAU, M, q);
      i := 0;
      while i <= k - 1 do
        begin
          j := i + 1;
          while j <= M - 1 do
            begin
              CMatrix[i, j] := 0.0;
              inc(j);
            end;
          inc(i);
        end;
      if AP_FP_Less(RMatrixLURCondInf(CMatrix, k), 1000 * MachineEpsilon) then
        begin
          Info := -3;
          Exit;
        end;
      SetLength(tmp, k);
      i := 0;
      while i <= k - 1 do
        begin
          if i > 0 then
            begin
              v := APVDotProduct(@CMatrix[i][0], 0, i - 1, @tmp[0], 0, i - 1);
            end
          else
            begin
              v := 0;
            end;
          tmp[i] := (CMatrix[i, M] - v) / CMatrix[i, i];
          inc(i);
        end;
      SetLength(C0, M);
      i := 0;
      while i <= M - 1 do
        begin
          C0[i] := 0;
          inc(i);
        end;
      i := 0;
      while i <= k - 1 do
        begin
          v := tmp[i];
          APVAdd(@C0[0], 0, M - 1, @q[i][0], 0, M - 1, v);
          inc(i);
        end;

      //
      // Second, prepare modified matrix F2 = F*Q2' and solve modified task
      //
      SetLength(tmp, Max(n, M) + 1);
      SetLength(f2, n, M - k);
      MatrixVectorMultiply(FMatrix, 0, n - 1, 0, M - 1, False, C0, 0, M - 1, -1.0, y, 0, n - 1, 1.0);
      MatrixMatrixMultiply(FMatrix, 0, n - 1, 0, M - 1, False, q, k, M - 1, 0, M - 1, True, 1.0, f2, 0, n - 1, 0, M - k - 1, 0.0, tmp);
      LSFitLinearInternal(y, w, f2, n, M - k, Info, tmp, Rep);
      Rep.TaskRCond := -1;
      if Info <= 0 then
          Exit;

      //
      // then, convert back to original answer: C = C0 + Q2'*Y0
      //
      SetLength(c, M);
      APVMove(@c[0], 0, M - 1, @C0[0], 0, M - 1);
      MatrixVectorMultiply(q, k, M - 1, 0, M - 1, True, tmp, 0, M - k - 1, 1.0, c, 0, M - 1, 1.0);
    end;
end;

(* ************************************************************************
  Linear least squares fitting, without weights.
  See LSFitLinearW for more information.
  ************************************************************************ *)
procedure LSFitLinear(y: TLVec; FMatrix: TLMatrix;
  n: TLInt; M: TLInt; var Info: TLInt;
  var c: TLVec; var Rep: TLSFitReport);
var
  w: TLVec;
  i: TLInt;
begin
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(w, n);
  i := 0;
  while i <= n - 1 do
    begin
      w[i] := 1;
      inc(i);
    end;
  LSFitLinearInternal(y, w, FMatrix, n, M, Info, c, Rep);
end;

(* ************************************************************************
  Constained linear least squares fitting, without weights.
  See LSFitLinearWC() for more information.
  ************************************************************************ *)
procedure LSFitLinearC(y: TLVec; FMatrix: TLMatrix;
  CMatrix: TLMatrix; n: TLInt; M: TLInt;
  k: TLInt; var Info: TLInt; var c: TLVec;
  var Rep: TLSFitReport);
var
  w: TLVec;
  i: TLInt;
begin
  y := DynamicArrayCopy(y);
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(w, n);
  i := 0;
  while i <= n - 1 do
    begin
      w[i] := 1;
      inc(i);
    end;
  LSFitLinearWC(y, w, FMatrix, CMatrix, n, M, k, Info, c, Rep);
end;

(* ************************************************************************
  Weighted nonlinear least squares fitting using gradient and Hessian.

  Nonlinear task min(F(c)) is solved, where

  F(c) = (w[0]*(f(x[0],c)-y[0]))^2 + ... + (w[n-1]*(f(x[n-1],c)-y[n-1]))^2,

  * N is a number of points,
  * M is a dimension of a space points belong to,
  * K is a dimension of a space of parameters being fitted,
  * w is an N-dimensional vector of weight coefficients,
  * x is a set of N points, each of them is an M-dimensional vector,
  * c is a K-dimensional vector of parameters being fitted

  This subroutine uses only f(x[i],c) and its gradient.

  INPUT PARAMETERS:
  X       -   array[0..N-1,0..M-1], points (one row = one point)
  Y       -   array[0..N-1], function values.
  W       -   weights, array[0..N-1]
  C       -   array[0..K-1], initial approximation to the solution,
  N       -   number of points, N>1
  M       -   dimension of space
  K       -   number of parameters being fitted
  CheapFG -   boolean flag, which is:
  * True if both function and gradient calculation complexity are less than O(M^2). An improved algorithm can be used which corresponds to FGJ scheme from MINLM unit.
  * False otherwise.
  Standard Jacibian-bases  Levenberg-Marquardt  algo
  will be used (FJ scheme).

  OUTPUT PARAMETERS:
  State   -   structure which stores algorithm state between subsequent
  calls  of   LSFitNonlinearIteration.   Used  for  reverse
  communication.  This  structure   should   be  passed  to
  LSFitNonlinearIteration subroutine.

  See also:
  LSFitNonlinearIteration
  LSFitNonlinearResults
  LSFitNonlinearFG (fitting without weights)
  LSFitNonlinearWFGH (fitting using Hessian)
  LSFitNonlinearFGH (fitting using Hessian, without weights)
  ************************************************************************ *)
procedure LSFitNonlinearWFG(x: TLMatrix; y: TLVec; w: TLVec; c: TLVec; n: TLInt; M: TLInt; k: TLInt; CheapFG: Boolean;
  var State: TLSFitState);
var
  i: TLInt;
begin
  State.n := n;
  State.M := M;
  State.k := k;
  LSFitNonlinearSetCond(State, 0.0, 0.0, 0);
  LSFitNonlinearSetStpMax(State, 0.0);
  State.CheapFG := CheapFG;
  State.HaveHess := False;
  if (n >= 1) and (M >= 1) and (k >= 1) then
    begin
      SetLength(State.TaskX, n, M);
      SetLength(State.TaskY, n);
      SetLength(State.w, n);
      SetLength(State.c, k);
      APVMove(@State.c[0], 0, k - 1, @c[0], 0, k - 1);
      APVMove(@State.w[0], 0, n - 1, @w[0], 0, n - 1);
      i := 0;
      while i <= n - 1 do
        begin
          APVMove(@State.TaskX[i][0], 0, M - 1, @x[i][0], 0, M - 1);
          State.TaskY[i] := y[i];
          inc(i);
        end;
    end;
  SetLength(State.RState.IA, 4 + 1);
  SetLength(State.RState.ResArry, 1 + 1);
  State.RState.Stage := -1;
end;

(* ************************************************************************
  Nonlinear least squares fitting, no individual weights.
  See LSFitNonlinearWFG for more information.
  ************************************************************************ *)
procedure LSFitNonlinearFG(x: TLMatrix; y: TLVec; c: TLVec; n: TLInt; M: TLInt; k: TLInt; CheapFG: Boolean;
  var State: TLSFitState);
var
  i: TLInt;
begin
  State.n := n;
  State.M := M;
  State.k := k;
  LSFitNonlinearSetCond(State, 0.0, 0.0, 0);
  LSFitNonlinearSetStpMax(State, 0.0);
  State.CheapFG := CheapFG;
  State.HaveHess := False;
  if (n >= 1) and (M >= 1) and (k >= 1) then
    begin
      SetLength(State.TaskX, n, M);
      SetLength(State.TaskY, n);
      SetLength(State.w, n);
      SetLength(State.c, k);
      APVMove(@State.c[0], 0, k - 1, @c[0], 0, k - 1);
      i := 0;
      while i <= n - 1 do
        begin
          APVMove(@State.TaskX[i][0], 0, M - 1, @x[i][0], 0, M - 1);
          State.TaskY[i] := y[i];
          State.w[i] := 1;
          inc(i);
        end;
    end;
  SetLength(State.RState.IA, 4 + 1);
  SetLength(State.RState.ResArry, 1 + 1);
  State.RState.Stage := -1;
end;

(* ************************************************************************
  Weighted nonlinear least squares fitting using gradient/Hessian.

  Nonlinear task min(F(c)) is solved, where

  F(c) = (w[0]*(f(x[0],c)-y[0]))^2 + ... + (w[n-1]*(f(x[n-1],c)-y[n-1]))^2,

  * N is a number of points,
  * M is a dimension of a space points belong to,
  * K is a dimension of a space of parameters being fitted,
  * w is an N-dimensional vector of weight coefficients,
  * x is a set of N points, each of them is an M-dimensional vector,
  * c is a K-dimensional vector of parameters being fitted

  This subroutine uses f(x[i],c), its gradient and its Hessian.

  See LSFitNonlinearWFG() subroutine for information about function  arameters.
  ************************************************************************ *)
procedure LSFitNonlinearWFGH(x: TLMatrix; y: TLVec; w: TLVec; c: TLVec; n: TLInt; M: TLInt; k: TLInt;
  var State: TLSFitState);
var
  i: TLInt;
begin
  State.n := n;
  State.M := M;
  State.k := k;
  LSFitNonlinearSetCond(State, 0.0, 0.0, 0);
  LSFitNonlinearSetStpMax(State, 0.0);
  State.CheapFG := True;
  State.HaveHess := True;
  if (n >= 1) and (M >= 1) and (k >= 1) then
    begin
      SetLength(State.TaskX, n, M);
      SetLength(State.TaskY, n);
      SetLength(State.w, n);
      SetLength(State.c, k);
      APVMove(@State.c[0], 0, k - 1, @c[0], 0, k - 1);
      APVMove(@State.w[0], 0, n - 1, @w[0], 0, n - 1);
      i := 0;
      while i <= n - 1 do
        begin
          APVMove(@State.TaskX[i][0], 0, M - 1, @x[i][0], 0, M - 1);
          State.TaskY[i] := y[i];
          inc(i);
        end;
    end;
  SetLength(State.RState.IA, 4 + 1);
  SetLength(State.RState.ResArry, 1 + 1);
  State.RState.Stage := -1;
end;

(* ************************************************************************
  Nonlinear least squares fitting using gradient/Hessian without  individual
  weights. See LSFitNonlinearWFGH() for more information.
  ************************************************************************ *)
procedure LSFitNonlinearFGH(x: TLMatrix; y: TLVec; c: TLVec; n: TLInt; M: TLInt; k: TLInt; var State: TLSFitState);
var
  i: TLInt;
begin
  State.n := n;
  State.M := M;
  State.k := k;
  LSFitNonlinearSetCond(State, 0.0, 0.0, 0);
  LSFitNonlinearSetStpMax(State, 0.0);
  State.CheapFG := True;
  State.HaveHess := True;
  if (n >= 1) and (M >= 1) and (k >= 1) then
    begin
      SetLength(State.TaskX, n, M);
      SetLength(State.TaskY, n);
      SetLength(State.w, n);
      SetLength(State.c, k);
      APVMove(@State.c[0], 0, k - 1, @c[0], 0, k - 1);
      i := 0;
      while i <= n - 1 do
        begin
          APVMove(@State.TaskX[i][0], 0, M - 1, @x[i][0], 0, M - 1);
          State.TaskY[i] := y[i];
          State.w[i] := 1;
          inc(i);
        end;
    end;
  SetLength(State.RState.IA, 4 + 1);
  SetLength(State.RState.ResArry, 1 + 1);
  State.RState.Stage := -1;
end;

(* ************************************************************************
  Stopping conditions for nonlinear least squares fitting.

  INPUT PARAMETERS:
  State   -   structure which stores algorithm state between calls and which is used for reverse communication. Must be initialized with LSFitNonLinearCreate???()
  EpsF    -   stopping criterion. Algorithm stops if |F(k+1)-F(k)| <= EpsF*max{|F(k)|, |F(k+1)|, 1}
  EpsX    -   stopping criterion. Algorithm stops if |X(k+1)-X(k)| <= EpsX*(1+|X(k)|)
  MaxIts  -   stopping criterion. Algorithm stops after MaxIts iterations. MaxIts=0 means no stopping criterion.

  NOTE
  Passing EpsF=0, EpsX=0 and MaxIts=0 (simultaneously) will lead to automatic
  stopping criterion selection (according to the scheme used by MINLM unit).
  ************************************************************************ *)
procedure LSFitNonlinearSetCond(var State: TLSFitState; EpsF: TLFloat; EpsX: TLFloat; MAXITS: TLInt);
begin
  Assert(AP_FP_Greater_Eq(EpsF, 0), 'LSFitNonlinearSetCond: negative EpsF!');
  Assert(AP_FP_Greater_Eq(EpsX, 0), 'LSFitNonlinearSetCond: negative EpsX!');
  Assert(MAXITS >= 0, 'LSFitNonlinearSetCond: negative MaxIts!');
  State.EpsF := EpsF;
  State.EpsX := EpsX;
  State.MAXITS := MAXITS;
end;

(* ************************************************************************
  This function sets maximum step length

  INPUT PARAMETERS:
  State   -   structure which stores algorithm state between calls and which is used for reverse communication. Must be initialized with LSFitNonLinearCreate???()
  StpMax  -   maximum step length, >=0. Set StpMax to 0.0,  if you don't want to limit step length.

  Use this subroutine when you optimize target function which contains exp() or other fast growing functions,
  and optimization algorithm makes too large steps which leads to overflow.
  This function allows us to reject steps that are too large (and therefore expose us to the possible overflow)
  without actually calculating function value at the x+stp*d.

  NOTE: non-zero StpMax leads to moderate performance degradation because intermediate step
  of preconditioned L-BFGS optimization is incompatible with limits on step size.
  ************************************************************************ *)
procedure LSFitNonlinearSetStpMax(var State: TLSFitState; StpMax: TLFloat);
begin
  Assert(AP_FP_Greater_Eq(StpMax, 0), 'LSFitNonlinearSetStpMax: StpMax<0!');
  State.StpMax := StpMax;
end;

(* ************************************************************************
  Nonlinear least squares fitting. Algorithm iteration.

  Called after inialization of the State structure with LSFitNonlinearXXX() subroutine. See HTML docs for examples.

  INPUT PARAMETERS:
  State   -   structure which stores algorithm state between subsequent calls and which is used for reverse communication.
  Must be initialized with LSFitNonlinearXXX() call first.

  RESULT
  1. If subroutine returned False, iterative algorithm has converged.
  2. If subroutine returned True, then if:
  * if State.NeedF=True,      function value F(X,C) is required
  * if State.NeedFG=True,     function value F(X,C) and gradient  dF/dC(X,C) are required
  * if State.NeedFGH=True     function value F(X,C), gradient dF/dC(X,C) and Hessian are required

  One and only one of this fields can be set at time.

  Function, its gradient and Hessian are calculated at  (X,C),  where  X  is
  stored in State.X[0..M-1] and C is stored in State.C[0..K-1].

  Results are stored:
  * function value            -   in State.F
  * gradient                  -   in State.G[0..K-1]
  * Hessian                   -   in State.H[0..K-1,0..K-1]
  ************************************************************************ *)
function LSFitNonlinearIteration(var State: TLSFitState): Boolean;
label
  lbl_5, lbl_9, lbl_0, lbl_11, lbl_7, lbl_14, lbl_1, lbl_16, lbl_12, lbl_19,
  lbl_2, lbl_21, lbl_17, lbl_24, lbl_3, lbl_26, lbl_22, lbl_6, lbl_29, lbl_4,
  lbl_31, lbl_27, lbl_rcomm;
var
  n: TLInt;
  M: TLInt;
  k: TLInt;
  i: TLInt;
  j: TLInt;
  v: TLFloat;
  RelCnt: TLFloat;
begin

  //
  // Reverse communication preparations
  // I know it looks ugly, but it works the same way
  // anywhere from C++ to Python.
  //
  // This code initializes locals by:
  // * random values determined during code
  // generation - on first subroutine call
  // * values from previous call - on subsequent calls
  //
  if State.RState.Stage >= 0 then
    begin
      n := State.RState.IA[0];
      M := State.RState.IA[1];
      k := State.RState.IA[2];
      i := State.RState.IA[3];
      j := State.RState.IA[4];
      v := State.RState.ResArry[0];
      RelCnt := State.RState.ResArry[1];
    end
  else
    begin
      n := -983;
      M := -989;
      k := -834;
      i := 900;
      j := -287;
      v := 364;
      RelCnt := 214;
    end;
  if State.RState.Stage = 0 then
      goto lbl_0;
  if State.RState.Stage = 1 then
      goto lbl_1;
  if State.RState.Stage = 2 then
      goto lbl_2;
  if State.RState.Stage = 3 then
      goto lbl_3;
  if State.RState.Stage = 4 then
      goto lbl_4;

  //
  // Routine body
  //

  //
  // check params
  //
  if (State.n < 1) or (State.M < 1) or (State.k < 1) or AP_FP_Less(State.EpsF, 0) or AP_FP_Less(State.EpsX, 0) or (State.MAXITS < 0) then
    begin
      State.RepTerminationType := -1;
      Result := False;
      Exit;
    end;

  //
  // init
  //
  n := State.n;
  M := State.M;
  k := State.k;
  SetLength(State.x, M);
  SetLength(State.g, k);
  if State.HaveHess then
      SetLength(State.h, k, k);

  //
  // initialize LM optimizer
  //
  if State.HaveHess then
    begin

      //
      // use Hessian.
      // transform stopping conditions.
      //
      MinLMCreateFGH(k, State.c, State.OptState);
    end
  else
    begin

      //
      // use one of gradient-based schemes (depending on gradient cost).
      // transform stopping conditions.
      //
      if State.CheapFG then
        begin
          MinLMCreateFGJ(k, n, State.c, State.OptState);
        end
      else
        begin
          MinLMCreateFJ(k, n, State.c, State.OptState);
        end;
    end;
  MinLMSetCond(State.OptState, 0.0, State.EpsF, State.EpsX, State.MAXITS);
  MinLMSetStpMax(State.OptState, State.StpMax);

  //
  // Optimize
  //
lbl_5:
  if not MinLMIteration(State.OptState) then
      goto lbl_6;
  if not State.OptState.NeedF then
      goto lbl_7;

  //
  // calculate F = sum (wi*(f(xi,c)-yi))^2
  //
  State.OptState.f := 0;
  i := 0;

lbl_9:
  if i > n - 1 then
      goto lbl_11;
  APVMove(@State.c[0], 0, k - 1, @State.OptState.x[0], 0, k - 1);
  APVMove(@State.x[0], 0, M - 1, @State.TaskX[i][0], 0, M - 1);
  State.PointIndex := i;
  LSFitClearRequestFields(State);
  State.NeedF := True;
  State.RState.Stage := 0;
  goto lbl_rcomm;

lbl_0:
  State.OptState.f := State.OptState.f + AP_Sqr(State.w[i] * (State.f - State.TaskY[i]));
  i := i + 1;
  goto lbl_9;

lbl_11:
  goto lbl_5;

lbl_7:
  if not State.OptState.NeedFG then
      goto lbl_12;

  //
  // calculate F/gradF
  //
  State.OptState.f := 0;
  i := 0;
  while i <= k - 1 do
    begin
      State.OptState.g[i] := 0;
      inc(i);
    end;
  i := 0;

lbl_14:
  if i > n - 1 then
      goto lbl_16;
  APVMove(@State.c[0], 0, k - 1, @State.OptState.x[0], 0, k - 1);
  APVMove(@State.x[0], 0, M - 1, @State.TaskX[i][0], 0, M - 1);
  State.PointIndex := i;
  LSFitClearRequestFields(State);
  State.NeedFG := True;
  State.RState.Stage := 1;
  goto lbl_rcomm;

lbl_1:
  State.OptState.f := State.OptState.f + AP_Sqr(State.w[i] * (State.f - State.TaskY[i]));
  v := AP_Sqr(State.w[i]) * 2 * (State.f - State.TaskY[i]);
  APVAdd(@State.OptState.g[0], 0, k - 1, @State.g[0], 0, k - 1, v);
  i := i + 1;
  goto lbl_14;

lbl_16:
  goto lbl_5;

lbl_12:
  if not State.OptState.NeedFiJ then
      goto lbl_17;

  //
  // calculate Fi/jac(Fi)
  //
  i := 0;

lbl_19:
  if i > n - 1 then
      goto lbl_21;
  APVMove(@State.c[0], 0, k - 1, @State.OptState.x[0], 0, k - 1);
  APVMove(@State.x[0], 0, M - 1, @State.TaskX[i][0], 0, M - 1);
  State.PointIndex := i;
  LSFitClearRequestFields(State);
  State.NeedFG := True;
  State.RState.Stage := 2;
  goto lbl_rcomm;

lbl_2:
  State.OptState.fi[i] := State.w[i] * (State.f - State.TaskY[i]);
  v := State.w[i];
  APVMove(@State.OptState.j[i][0], 0, k - 1, @State.g[0], 0, k - 1, v);
  i := i + 1;
  goto lbl_19;

lbl_21:
  goto lbl_5;

lbl_17:
  if not State.OptState.NeedFGH then
      goto lbl_22;

  //
  // calculate F/grad(F)/hess(F)
  //
  State.OptState.f := 0;
  i := 0;
  while i <= k - 1 do
    begin
      State.OptState.g[i] := 0;
      inc(i);
    end;
  i := 0;
  while i <= k - 1 do
    begin
      j := 0;
      while j <= k - 1 do
        begin
          State.OptState.h[i, j] := 0;
          inc(j);
        end;
      inc(i);
    end;
  i := 0;

lbl_24:
  if i > n - 1 then
      goto lbl_26;
  APVMove(@State.c[0], 0, k - 1, @State.OptState.x[0], 0, k - 1);
  APVMove(@State.x[0], 0, M - 1, @State.TaskX[i][0], 0, M - 1);
  State.PointIndex := i;
  LSFitClearRequestFields(State);
  State.NeedFGH := True;
  State.RState.Stage := 3;
  goto lbl_rcomm;

lbl_3:
  State.OptState.f := State.OptState.f + AP_Sqr(State.w[i] * (State.f - State.TaskY[i]));
  v := AP_Sqr(State.w[i]) * 2 * (State.f - State.TaskY[i]);
  APVAdd(@State.OptState.g[0], 0, k - 1, @State.g[0], 0, k - 1, v);
  j := 0;
  while j <= k - 1 do
    begin
      v := 2 * AP_Sqr(State.w[i]) * State.g[j];
      APVAdd(@State.OptState.h[j][0], 0, k - 1, @State.g[0], 0, k - 1, v);
      v := 2 * AP_Sqr(State.w[i]) * (State.f - State.TaskY[i]);
      APVAdd(@State.OptState.h[j][0], 0, k - 1, @State.h[j][0], 0, k - 1, v);
      inc(j);
    end;
  i := i + 1;
  goto lbl_24;

lbl_26:
  goto lbl_5;

lbl_22:
  goto lbl_5;

lbl_6:
  MinLMResults(State.OptState, State.c, State.OptRep);
  State.RepTerminationType := State.OptRep.TerminationType;

  //
  // calculate errors
  //
  if State.RepTerminationType <= 0 then
      goto lbl_27;
  State.RepRMSError := 0;
  State.RepAvgError := 0;
  State.RepAvgRelError := 0;
  State.RepMaxError := 0;
  RelCnt := 0;
  i := 0;

lbl_29:
  if i > n - 1 then
      goto lbl_31;
  APVMove(@State.c[0], 0, k - 1, @State.c[0], 0, k - 1);
  APVMove(@State.x[0], 0, M - 1, @State.TaskX[i][0], 0, M - 1);
  State.PointIndex := i;
  LSFitClearRequestFields(State);
  State.NeedF := True;
  State.RState.Stage := 4;
  goto lbl_rcomm;

lbl_4:
  v := State.f;
  State.RepRMSError := State.RepRMSError + AP_Sqr(v - State.TaskY[i]);
  State.RepAvgError := State.RepAvgError + AbsReal(v - State.TaskY[i]);
  if AP_FP_NEq(State.TaskY[i], 0) then
    begin
      State.RepAvgRelError := State.RepAvgRelError + AbsReal(v - State.TaskY[i]) /
        AbsReal(State.TaskY[i]);
      RelCnt := RelCnt + 1;
    end;
  State.RepMaxError := Max(State.RepMaxError, AbsReal(v - State.TaskY[i]));
  i := i + 1;
  goto lbl_29;

lbl_31:
  State.RepRMSError := Sqrt(State.RepRMSError / n);
  State.RepAvgError := State.RepAvgError / n;
  if AP_FP_NEq(RelCnt, 0) then
    begin
      State.RepAvgRelError := State.RepAvgRelError / RelCnt;
    end;

lbl_27:
  Result := False;
  Exit;

  //
  // Saving state
  //
lbl_rcomm:
  Result := True;
  State.RState.IA[0] := n;
  State.RState.IA[1] := M;
  State.RState.IA[2] := k;
  State.RState.IA[3] := i;
  State.RState.IA[4] := j;
  State.RState.ResArry[0] := v;
  State.RState.ResArry[1] := RelCnt;
end;

(* ************************************************************************
  Nonlinear least squares fitting results.

  Called after LSFitNonlinearIteration() returned False.

  INPUT PARAMETERS:
  State   -   algorithm state (used by LSFitNonlinearIteration).

  OUTPUT PARAMETERS:
  Info    -   completetion code:
  * -1    incorrect parameters were specified
  *  1    relative function improvement is no more than EpsF.
  *  2    relative step is no more than EpsX.
  *  4    gradient norm is no more than EpsG
  *  5    MaxIts steps was taken
  C       -   array[0..K-1], solution
  Rep     -   optimization report. Following fields are set:
  * Rep.TerminationType completetion code:
  * RMSError          rms error on the (X,Y).
  * AvgError          average error on the (X,Y).
  * AvgRelError       average relative error on the non-zero Y
  * MaxError          maximum error
  NON-WEIGHTED ERRORS ARE CALCULATED
  ************************************************************************ *)
procedure LSFitNonlinearResults(State: TLSFitState; var Info: TLInt; var c: TLVec; var Rep: TLSFitReport);
begin
  Info := State.RepTerminationType;
  if Info > 0 then
    begin
      SetLength(c, State.k);
      APVMove(@c[0], 0, State.k - 1, @State.c[0], 0, State.k - 1);
      Rep.RMSError := State.RepRMSError;
      Rep.AvgError := State.RepAvgError;
      Rep.AvgRelError := State.RepAvgRelError;
      Rep.MaxError := State.RepMaxError;
    end;
end;

procedure LSFitScaleXY(var x, y: TLVec; n: TLInt; var XC, YC: TLVec; DC: TLIVec; k: TLInt; var XA, XB, SA, SB: TLFloat; var XOriginal, YOriginal: TLVec);
var
  XMin: TLFloat;
  XMax: TLFloat;
  i: TLInt;
begin
  Assert(n >= 1, 'LSFitScaleXY: incorrect N');
  Assert(k >= 0, 'LSFitScaleXY: incorrect K');

  //
  // Calculate xmin/xmax.
  // Force xmin<>xmax.
  //
  XMin := x[0];
  XMax := x[0];
  i := 1;
  while i <= n - 1 do
    begin
      XMin := Min(XMin, x[i]);
      XMax := Max(XMax, x[i]);
      inc(i);
    end;
  i := 0;
  while i <= k - 1 do
    begin
      XMin := Min(XMin, XC[i]);
      XMax := Max(XMax, XC[i]);
      inc(i);
    end;
  if AP_FP_Eq(XMin, XMax) then
    begin
      if AP_FP_Eq(XMin, 0) then
        begin
          XMin := -1;
          XMax := +1;
        end
      else
        begin
          XMin := 0.5 * XMin;
        end;
    end;

  //
  // Transform abscissas: map [XA,XB] to [0,1]
  //
  // Store old X[] in XOriginal[] (it will be used
  // to calculate relative error).
  //
  SetLength(XOriginal, n);
  APVMove(@XOriginal[0], 0, n - 1, @x[0], 0, n - 1);
  XA := XMin;
  XB := XMax;
  i := 0;
  while i <= n - 1 do
    begin
      x[i] := 2 * (x[i] - 0.5 * (XA + XB)) / (XB - XA);
      inc(i);
    end;
  i := 0;
  while i <= k - 1 do
    begin
      Assert(DC[i] >= 0, 'LSFitScaleXY: internal error!');
      XC[i] := 2 * (XC[i] - 0.5 * (XA + XB)) / (XB - XA);
      YC[i] := YC[i] * Power(0.5 * (XB - XA), DC[i]);
      inc(i);
    end;

  //
  // Transform function values: map [SA,SB] to [0,1]
  // SA = mean(Y),
  // SB = SA+stddev(Y).
  //
  // Store old Y[] in YOriginal[] (it will be used
  // to calculate relative error).
  //
  SetLength(YOriginal, n);
  APVMove(@YOriginal[0], 0, n - 1, @y[0], 0, n - 1);
  SA := 0;
  i := 0;
  while i <= n - 1 do
    begin
      SA := SA + y[i];
      inc(i);
    end;
  SA := SA / n;
  SB := 0;
  i := 0;
  while i <= n - 1 do
    begin
      SB := SB + AP_Sqr(y[i] - SA);
      inc(i);
    end;
  SB := Sqrt(SB / n) + SA;
  if AP_FP_Eq(SB, SA) then
    begin
      SB := 2 * SA;
    end;
  if AP_FP_Eq(SB, SA) then
    begin
      SB := SA + 1;
    end;
  i := 0;
  while i <= n - 1 do
    begin
      y[i] := (y[i] - SA) / (SB - SA);
      inc(i);
    end;
  i := 0;
  while i <= k - 1 do
    begin
      if DC[i] = 0 then
        begin
          YC[i] := (YC[i] - SA) / (SB - SA);
        end
      else
        begin
          YC[i] := YC[i] / (SB - SA);
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Internal fitting subroutine
  ************************************************************************ *)
procedure LSFitLinearInternal(y: TLVec; w: TLVec; FMatrix: TLMatrix; n: TLInt; M: TLInt;
  var Info: TLInt; var c: TLVec; var Rep: TLSFitReport);
var
  Threshold: TLFloat;
  FT: TLMatrix;
  q: TLMatrix;
  L: TLMatrix;
  r: TLMatrix;
  b: TLVec;
  WMod: TLVec;
  TAU: TLVec;
  i: TLInt;
  j: TLInt;
  v: TLFloat;
  SV: TLVec;
  u: TLMatrix;
  VT: TLMatrix;
  tmp: TLVec;
  UTB: TLVec;
  SUTB: TLVec;
  RelCnt: TLInt;
begin
  if (n < 1) or (M < 1) then
    begin
      Info := -1;
      Exit;
    end;
  Info := 1;
  Threshold := Sqrt(MachineEpsilon);

  //
  // Degenerate case, needs special handling
  //
  if n < M then
    begin

      //
      // Create design matrix.
      //
      SetLength(FT, n, M);
      SetLength(b, n);
      SetLength(WMod, n);
      j := 0;
      while j <= n - 1 do
        begin
          v := w[j];
          APVMove(@FT[j][0], 0, M - 1, @FMatrix[j][0], 0, M - 1, v);
          b[j] := w[j] * y[j];
          WMod[j] := 1;
          inc(j);
        end;

      //
      // LQ decomposition and reduction to M=N
      //
      SetLength(c, M);
      i := 0;
      while i <= M - 1 do
        begin
          c[i] := 0;
          inc(i);
        end;
      Rep.TaskRCond := 0;
      RMatrixLQ(FT, n, M, TAU);
      RMatrixLQUnpackQ(FT, n, M, TAU, n, q);
      RMatrixLQUnpackL(FT, n, M, L);
      LSFitLinearInternal(b, WMod, L, n, n, Info, tmp, Rep);
      if Info <= 0 then
        begin
          Exit;
        end;
      i := 0;
      while i <= n - 1 do
        begin
          v := tmp[i];
          APVAdd(@c[0], 0, M - 1, @q[i][0], 0, M - 1, v);
          inc(i);
        end;
      Exit;
    end;

  //
  // N>=M. Generate design matrix and reduce to N=M using
  // QR decomposition.
  //
  SetLength(FT, n, M);
  SetLength(b, n);
  j := 0;
  while j <= n - 1 do
    begin
      v := w[j];
      APVMove(@FT[j][0], 0, M - 1, @FMatrix[j][0], 0, M - 1, v);
      b[j] := w[j] * y[j];
      inc(j);
    end;
  RMatrixQR(FT, n, M, TAU);
  RMatrixQRUnpackQ(FT, n, M, TAU, M, q);
  RMatrixQRUnpackR(FT, n, M, r);
  SetLength(tmp, M);
  i := 0;
  while i <= M - 1 do
    begin
      tmp[i] := 0;
      inc(i);
    end;
  i := 0;
  while i <= n - 1 do
    begin
      v := b[i];
      APVAdd(@tmp[0], 0, M - 1, @q[i][0], 0, M - 1, v);
      inc(i);
    end;
  SetLength(b, M);
  APVMove(@b[0], 0, M - 1, @tmp[0], 0, M - 1);

  //
  // R contains reduced MxM design upper triangular matrix,
  // B contains reduced Mx1 right part.
  //
  // Determine system condition number and decide
  // should we use triangular solver (faster) or
  // SVD-based solver (more stable).
  //
  // We can use LU-based RCond estimator for this task.
  //
  Rep.TaskRCond := RMatrixLURCondInf(r, M);
  if AP_FP_Greater(Rep.TaskRCond, Threshold) then
    begin

      //
      // use QR-based solver
      //
      SetLength(c, M);
      c[M - 1] := b[M - 1] / r[M - 1, M - 1];
      i := M - 2;
      while i >= 0 do
        begin
          v := APVDotProduct(@r[i][0], i + 1, M - 1, @c[0], i + 1, M - 1);
          c[i] := (b[i] - v) / r[i, i];
          dec(i);
        end;
    end
  else
    begin

      //
      // use SVD-based solver
      //
      if not RMatrixSVD(r, M, M, 1, 1, 2, SV, u, VT) then
        begin
          Info := -4;
          Exit;
        end;
      SetLength(UTB, M);
      SetLength(SUTB, M);
      i := 0;
      while i <= M - 1 do
        begin
          UTB[i] := 0;
          inc(i);
        end;
      i := 0;
      while i <= M - 1 do
        begin
          v := b[i];
          APVAdd(@UTB[0], 0, M - 1, @u[i][0], 0, M - 1, v);
          inc(i);
        end;
      if AP_FP_Greater(SV[0], 0) then
        begin
          Rep.TaskRCond := SV[M - 1] / SV[0];
          i := 0;
          while i <= M - 1 do
            begin
              if AP_FP_Greater(SV[i], Threshold * SV[0]) then
                begin
                  SUTB[i] := UTB[i] / SV[i];
                end
              else
                begin
                  SUTB[i] := 0;
                end;
              inc(i);
            end;
        end
      else
        begin
          Rep.TaskRCond := 0;
          i := 0;
          while i <= M - 1 do
            begin
              SUTB[i] := 0;
              inc(i);
            end;
        end;
      SetLength(c, M);
      i := 0;
      while i <= M - 1 do
        begin
          c[i] := 0;
          inc(i);
        end;
      i := 0;
      while i <= M - 1 do
        begin
          v := SUTB[i];
          APVAdd(@c[0], 0, M - 1, @VT[i][0], 0, M - 1, v);
          inc(i);
        end;
    end;

  //
  // calculate errors
  //
  Rep.RMSError := 0;
  Rep.AvgError := 0;
  Rep.AvgRelError := 0;
  Rep.MaxError := 0;
  RelCnt := 0;
  i := 0;
  while i <= n - 1 do
    begin
      v := APVDotProduct(@FMatrix[i][0], 0, M - 1, @c[0], 0, M - 1);
      Rep.RMSError := Rep.RMSError + AP_Sqr(v - y[i]);
      Rep.AvgError := Rep.AvgError + AbsReal(v - y[i]);
      if AP_FP_NEq(y[i], 0) then
        begin
          Rep.AvgRelError := Rep.AvgRelError + AbsReal(v - y[i]) / AbsReal(y[i]);
          RelCnt := RelCnt + 1;
        end;
      Rep.MaxError := Max(Rep.MaxError, AbsReal(v - y[i]));
      inc(i);
    end;
  Rep.RMSError := Sqrt(Rep.RMSError / n);
  Rep.AvgError := Rep.AvgError / n;
  if RelCnt <> 0 then
    begin
      Rep.AvgRelError := Rep.AvgRelError / RelCnt;
    end;
end;

(* ************************************************************************
  Internal subroutine
  ************************************************************************ *)
procedure LSFitClearRequestFields(var State: TLSFitState);
begin
  State.NeedF := False;
  State.NeedFG := False;
  State.NeedFGH := False;
end;

const
  BRCVNum = 10;

procedure BarycentricNormalize(var b: TBarycentricInterpolant); forward;
procedure BarycentricCalcBasis(b: TBarycentricInterpolant; t: TLFloat; var y: TLVec); forward;
procedure BarycentricFitWCFixedD(x, y: TLVec;
  w: TLVec; n: TLInt; XC: TLVec; YC: TLVec;
  DC: TLIVec; k: TLInt; M: TLInt;
  d: TLInt; var Info: TLInt; var b: TBarycentricInterpolant;
  var Rep: TBarycentricFitReport); forward;

(* ************************************************************************
  Rational interpolation using barycentric formula

  F(t) = SUM(i=0,n-1,w[i]*f[i]/(t-x[i])) / SUM(i=0,n-1,w[i]/(t-x[i]))

  Input parameters:
  B   -   barycentric interpolant built with one of model building subroutines.
  T   -   interpolation point

  Result:
  barycentric interpolant F(t)
  ************************************************************************ *)
function BarycentricCalc(b: TBarycentricInterpolant; t: TLFloat): TLFloat;
var
  s1: TLFloat;
  s2: TLFloat;
  s: TLFloat;
  v: TLFloat;
  i: TLInt;
begin

  //
  // special case: N=1
  //
  if b.n = 1 then
    begin
      Result := b.SY * b.y[0];
      Exit;
    end;

  //
  // Here we assume that task is normalized, i.e.:
  // 1. abs(Y[i])<=1
  // 2. abs(W[i])<=1
  // 3. X[] is ordered
  //
  s := AbsReal(t - b.x[0]);
  i := 0;
  while i <= b.n - 1 do
    begin
      v := b.x[i];
      if AP_FP_Eq(v, t) then
        begin
          Result := b.SY * b.y[i];
          Exit;
        end;
      v := AbsReal(t - v);
      if AP_FP_Less(v, s) then
        begin
          s := v;
        end;
      inc(i);
    end;
  s1 := 0;
  s2 := 0;
  i := 0;
  while i <= b.n - 1 do
    begin
      v := s / (t - b.x[i]);
      v := v * b.w[i];
      s1 := s1 + v * b.y[i];
      s2 := s2 + v;
      inc(i);
    end;
  Result := b.SY * s1 / s2;
end;

(* ************************************************************************
  Differentiation of barycentric interpolant: first derivative.

  Algorithm used in this subroutine is very robust and should not fail until
  provided with values too close to MaxRealNumber  (usually  MaxRealNumber/N or greater will overflow).

  INPUT PARAMETERS:
  B   -   barycentric interpolant built with one of model building subroutines.
  T   -   interpolation point

  OUTPUT PARAMETERS:
  F   -   barycentric interpolant at T
  DF  -   first derivative
  ************************************************************************ *)
procedure BarycentricDiff1(b: TBarycentricInterpolant; t: TLFloat; var f: TLFloat; var df: TLFloat);
var
  v: TLFloat;
  VV: TLFloat;
  i: TLInt;
  k: TLInt;
  N0: TLFloat;
  n1: TLFloat;
  D0: TLFloat;
  d1: TLFloat;
  S0: TLFloat;
  s1: TLFloat;
  xk: TLFloat;
  XI: TLFloat;
  XMin: TLFloat;
  XMax: TLFloat;
  XScale1: TLFloat;
  XOffs1: TLFloat;
  XScale2: TLFloat;
  XOffs2: TLFloat;
  XPrev: TLFloat;
begin

  //
  // special case: N=1
  //
  if b.n = 1 then
    begin
      f := b.SY * b.y[0];
      df := 0;
      Exit;
    end;
  if AP_FP_Eq(b.SY, 0) then
    begin
      f := 0;
      df := 0;
      Exit;
    end;
  Assert(AP_FP_Greater(b.SY, 0), 'BarycentricDiff1: internal error');

  //
  // We assume than N>1 and B.SY>0. Find:
  // 1. pivot point (X[i] closest to T)
  // 2. width of interval containing X[i]
  //
  v := AbsReal(b.x[0] - t);
  k := 0;
  XMin := b.x[0];
  XMax := b.x[0];
  i := 1;
  while i <= b.n - 1 do
    begin
      VV := b.x[i];
      if AP_FP_Less(AbsReal(VV - t), v) then
        begin
          v := AbsReal(VV - t);
          k := i;
        end;
      XMin := Min(XMin, VV);
      XMax := Max(XMax, VV);
      inc(i);
    end;

  //
  // pivot point found, calculate dNumerator and dDenominator
  //
  XScale1 := 1 / (XMax - XMin);
  XOffs1 := -XMin / (XMax - XMin) + 1;
  XScale2 := 2;
  XOffs2 := -3;
  t := t * XScale1 + XOffs1;
  t := t * XScale2 + XOffs2;
  xk := b.x[k];
  xk := xk * XScale1 + XOffs1;
  xk := xk * XScale2 + XOffs2;
  v := t - xk;
  N0 := 0;
  n1 := 0;
  D0 := 0;
  d1 := 0;
  XPrev := -2;
  i := 0;
  while i <= b.n - 1 do
    begin
      XI := b.x[i];
      XI := XI * XScale1 + XOffs1;
      XI := XI * XScale2 + XOffs2;
      Assert(AP_FP_Greater(XI, XPrev), 'BarycentricDiff1: points are too close!');
      XPrev := XI;
      if i <> k then
        begin
          VV := AP_Sqr(t - XI);
          S0 := (t - xk) / (t - XI);
          s1 := (xk - XI) / VV;
        end
      else
        begin
          S0 := 1;
          s1 := 0;
        end;
      VV := b.w[i] * b.y[i];
      N0 := N0 + S0 * VV;
      n1 := n1 + s1 * VV;
      VV := b.w[i];
      D0 := D0 + S0 * VV;
      d1 := d1 + s1 * VV;
      inc(i);
    end;
  f := b.SY * N0 / D0;
  df := (n1 * D0 - N0 * d1) / AP_Sqr(D0);
  if AP_FP_NEq(df, 0) then
    begin
      df := Sign(df) * Exp(ln(AbsReal(df)) + ln(b.SY) + ln(XScale1) +
        ln(XScale2));
    end;
end;

(* ************************************************************************
  Differentiation of barycentric interpolant: first/second derivatives.

  INPUT PARAMETERS:
  B   -   barycentric interpolant built with one of model building subroutines.
  T   -   interpolation point

  OUTPUT PARAMETERS:
  F   -   barycentric interpolant at T
  DF  -   first derivative
  D2F -   second derivative

  NOTE: this algorithm may fail due to overflow/underflor if used on data whose values are close to MaxRealNumber or MinRealNumber.
  Use more robust BarycentricDiff1() subroutine in such cases.
  ************************************************************************ *)
procedure BarycentricDiff2(b: TBarycentricInterpolant; t: TLFloat; var f: TLFloat; var df: TLFloat; var D2F: TLFloat);
var
  v: TLFloat;
  VV: TLFloat;
  i: TLInt;
  k: TLInt;
  N0: TLFloat;
  n1: TLFloat;
  n2: TLFloat;
  D0: TLFloat;
  d1: TLFloat;
  d2: TLFloat;
  S0: TLFloat;
  s1: TLFloat;
  s2: TLFloat;
  xk: TLFloat;
  XI: TLFloat;
begin
  f := 0;
  df := 0;
  D2F := 0;

  //
  // special case: N=1
  //
  if b.n = 1 then
    begin
      f := b.SY * b.y[0];
      df := 0;
      D2F := 0;
      Exit;
    end;
  if AP_FP_Eq(b.SY, 0) then
    begin
      f := 0;
      df := 0;
      D2F := 0;
      Exit;
    end;
  Assert(AP_FP_Greater(b.SY, 0), 'BarycentricDiff: internal error');

  //
  // We assume than N>1 and B.SY>0. Find:
  // 1. pivot point (X[i] closest to T)
  // 2. width of interval containing X[i]
  //
  v := AbsReal(b.x[0] - t);
  k := 0;
  i := 1;
  while i <= b.n - 1 do
    begin
      VV := b.x[i];
      if AP_FP_Less(AbsReal(VV - t), v) then
        begin
          v := AbsReal(VV - t);
          k := i;
        end;
      inc(i);
    end;

  //
  // pivot point found, calculate dNumerator and dDenominator
  //
  xk := b.x[k];
  v := t - xk;
  N0 := 0;
  n1 := 0;
  n2 := 0;
  D0 := 0;
  d1 := 0;
  d2 := 0;
  i := 0;
  while i <= b.n - 1 do
    begin
      if i <> k then
        begin
          XI := b.x[i];
          VV := AP_Sqr(t - XI);
          S0 := (t - xk) / (t - XI);
          s1 := (xk - XI) / VV;
          s2 := -2 * (xk - XI) / (VV * (t - XI));
        end
      else
        begin
          S0 := 1;
          s1 := 0;
          s2 := 0;
        end;
      VV := b.w[i] * b.y[i];
      N0 := N0 + S0 * VV;
      n1 := n1 + s1 * VV;
      n2 := n2 + s2 * VV;
      VV := b.w[i];
      D0 := D0 + S0 * VV;
      d1 := d1 + s1 * VV;
      d2 := d2 + s2 * VV;
      inc(i);
    end;
  f := b.SY * N0 / D0;
  df := b.SY * (n1 * D0 - N0 * d1) / AP_Sqr(D0);
  D2F := b.SY * ((n2 * D0 - N0 * d2) * AP_Sqr(D0) - (n1 * D0 - N0 * d1) * 2 * D0
    * d1) / AP_Sqr(AP_Sqr(D0));
end;

(* ************************************************************************
  This subroutine performs linear transformation of the argument.

  INPUT PARAMETERS:
  B       -   rational interpolant in barycentric form
  CA, CB  -   transformation coefficients: x = CA*t + CB

  OUTPUT PARAMETERS:
  B       -   transformed interpolant with X replaced by T
  ************************************************************************ *)
procedure BarycentricLinTransX(var b: TBarycentricInterpolant; ca: TLFloat; CB: TLFloat);
var
  i: TLInt;
  j: TLInt;
  v: TLFloat;
begin

  //
  // special case, replace by constant F(CB)
  //
  if AP_FP_Eq(ca, 0) then
    begin
      b.SY := BarycentricCalc(b, CB);
      v := 1;
      i := 0;
      while i <= b.n - 1 do
        begin
          b.y[i] := 1;
          b.w[i] := v;
          v := -v;
          inc(i);
        end;
      Exit;
    end;

  //
  // general case: CA<>0
  //
  i := 0;
  while i <= b.n - 1 do
    begin
      b.x[i] := (b.x[i] - CB) / ca;
      inc(i);
    end;
  if AP_FP_Less(ca, 0) then
    begin
      i := 0;
      while i <= b.n - 1 do
        begin
          if i < b.n - 1 - i then
            begin
              j := b.n - 1 - i;
              v := b.x[i];
              b.x[i] := b.x[j];
              b.x[j] := v;
              v := b.y[i];
              b.y[i] := b.y[j];
              b.y[j] := v;
              v := b.w[i];
              b.w[i] := b.w[j];
              b.w[j] := v;
            end
          else
            begin
              Break;
            end;
          inc(i);
        end;
    end;
end;

(* ************************************************************************
  This subroutine performs linear transformation of the barycentric interpolant.

  INPUT PARAMETERS:
  B       -   rational interpolant in barycentric form
  CA, CB  -   transformation coefficients: B2(x) = CA*B(x) + CB

  OUTPUT PARAMETERS:
  B       -   transformed interpolant
  ************************************************************************ *)
procedure BarycentricLinTransY(var b: TBarycentricInterpolant; ca: TLFloat; CB: TLFloat);
var
  i: TLInt;
  v: TLFloat;
begin
  i := 0;
  while i <= b.n - 1 do
    begin
      b.y[i] := ca * b.SY * b.y[i] + CB;
      inc(i);
    end;
  b.SY := 0;
  i := 0;
  while i <= b.n - 1 do
    begin
      b.SY := Max(b.SY, AbsReal(b.y[i]));
      inc(i);
    end;
  if AP_FP_Greater(b.SY, 0) then
    begin
      v := 1 / b.SY;
      APVMul(@b.y[0], 0, b.n - 1, v);
    end;
end;

(* ************************************************************************
  Extracts X/Y/W arrays from rational interpolant

  INPUT PARAMETERS:
  B   -   barycentric interpolant

  OUTPUT PARAMETERS:
  N   -   nodes count, N>0
  X   -   interpolation nodes, array[0..N-1]
  F   -   function values, array[0..N-1]
  W   -   barycentric weights, array[0..N-1]
  ************************************************************************ *)
procedure BarycentricUnpack(b: TBarycentricInterpolant; var n: TLInt; var x: TLVec; var y: TLVec; var w: TLVec);
var
  v: TLFloat;
begin
  n := b.n;
  SetLength(x, n);
  SetLength(y, n);
  SetLength(w, n);
  v := b.SY;
  APVMove(@x[0], 0, n - 1, @b.x[0], 0, n - 1);
  APVMove(@y[0], 0, n - 1, @b.y[0], 0, n - 1, v);
  APVMove(@w[0], 0, n - 1, @b.w[0], 0, n - 1);
end;

(* ************************************************************************
  Serialization of the barycentric interpolant

  INPUT PARAMETERS:
  B   -   barycentric interpolant

  OUTPUT PARAMETERS:
  ResArry -   array of real numbers which contains interpolant, array[0..RLen-1]
  RLen    -   ResArry lenght
  ************************************************************************ *)
procedure BarycentricSerialize(b: TBarycentricInterpolant; var ResArry: TLVec; var ResLen: TLInt);
begin
  ResLen := 2 + 2 + 3 * b.n;
  SetLength(ResArry, ResLen);
  ResArry[0] := ResLen;
  ResArry[1] := BRCVNum;
  ResArry[2] := b.n;
  ResArry[3] := b.SY;
  APVMove(@ResArry[0], 4, 4 + b.n - 1, @b.x[0], 0, b.n - 1);
  APVMove(@ResArry[0], 4 + b.n, 4 + 2 * b.n - 1, @b.y[0], 0, b.n - 1);
  APVMove(@ResArry[0], 4 + 2 * b.n, 4 + 3 * b.n - 1, @b.w[0], 0, b.n - 1);
end;

(* ************************************************************************
  Unserialization of the barycentric interpolant

  INPUT PARAMETERS:
  ResArry  -   array of real numbers which contains interpolant,

  OUTPUT PARAMETERS:
  B   -   barycentric interpolant
  ************************************************************************ *)
procedure BarycentricUnserialize(ResArry: TLVec; var b: TBarycentricInterpolant);
begin
  Assert(Round(ResArry[1]) = BRCVNum, 'BarycentricUnserialize: corrupted array!');
  b.n := Round(ResArry[2]);
  b.SY := ResArry[3];
  SetLength(b.x, b.n);
  SetLength(b.y, b.n);
  SetLength(b.w, b.n);
  APVMove(@b.x[0], 0, b.n - 1, @ResArry[0], 4, 4 + b.n - 1);
  APVMove(@b.y[0], 0, b.n - 1, @ResArry[0], 4 + b.n, 4 + 2 * b.n - 1);
  APVMove(@b.w[0], 0, b.n - 1, @ResArry[0], 4 + 2 * b.n, 4 + 3 * b.n - 1);
end;

(* ************************************************************************
  Copying of the barycentric interpolant

  INPUT PARAMETERS:
  B   -   barycentric interpolant

  OUTPUT PARAMETERS:
  B2  -   copy(B1)
  ************************************************************************ *)
procedure BarycentricCopy(b: TBarycentricInterpolant; var b2: TBarycentricInterpolant);
begin
  b2.n := b.n;
  b2.SY := b.SY;
  SetLength(b2.x, b2.n);
  SetLength(b2.y, b2.n);
  SetLength(b2.w, b2.n);
  APVMove(@b2.x[0], 0, b2.n - 1, @b.x[0], 0, b2.n - 1);
  APVMove(@b2.y[0], 0, b2.n - 1, @b.y[0], 0, b2.n - 1);
  APVMove(@b2.w[0], 0, b2.n - 1, @b.w[0], 0, b2.n - 1);
end;

(* ************************************************************************
  Rational interpolant from X/Y/W arrays

  F(t) = SUM(i=0,n-1,w[i]*f[i]/(t-x[i])) / SUM(i=0,n-1,w[i]/(t-x[i]))

  INPUT PARAMETERS:
  X   -   interpolation nodes, array[0..N-1]
  F   -   function values, array[0..N-1]
  W   -   barycentric weights, array[0..N-1]
  N   -   nodes count, N>0

  OUTPUT PARAMETERS:
  B   -   barycentric interpolant built from (X, Y, W)
  ************************************************************************ *)
procedure BarycentricBuildXYW(x, y, w: TLVec; n: TLInt; var b: TBarycentricInterpolant);
begin
  Assert(n > 0, 'BarycentricBuildXYW: incorrect N!');

  //
  // fill X/Y/W
  //
  SetLength(b.x, n);
  SetLength(b.y, n);
  SetLength(b.w, n);
  APVMove(@b.x[0], 0, n - 1, @x[0], 0, n - 1);
  APVMove(@b.y[0], 0, n - 1, @y[0], 0, n - 1);
  APVMove(@b.w[0], 0, n - 1, @w[0], 0, n - 1);
  b.n := n;

  //
  // Normalize
  //
  BarycentricNormalize(b);
end;

(* ************************************************************************
  Rational interpolant without poles

  The subroutine constructs the rational interpolating function without real poles
  (see 'Barycentric rational interpolation with no poles and high rates of approximation', Michael S. Floater. and Kai Hormann, for more information on this subject).

  Input parameters:
  X   -   interpolation nodes, array[0..N-1].
  Y   -   function values, array[0..N-1].
  N   -   number of nodes, N>0.
  D   -   order of the interpolation scheme, 0 <= D <= N-1.
  D<0  will cause an error.
  D>=N it will be replaced with D=N-1.
  if you don't know what D to choose, use small value about 3-5.

  Output parameters:
  B   -   barycentric interpolant.

  Note:
  this algorithm always succeeds and calculates the weights with close to machine precision.
  ************************************************************************ *)
procedure BarycentricBuildFloaterHormann(x, y: TLVec; n: TLInt; d: TLInt; var b: TBarycentricInterpolant);
var
  S0: TLFloat;
  s: TLFloat;
  v: TLFloat;
  i: TLInt;
  j: TLInt;
  k: TLInt;
  Perm: TLIVec;
  WTEMP: TLVec;
begin
  Assert(n > 0, 'BarycentricFloaterHormann: N<=0!');
  Assert(d >= 0, 'BarycentricFloaterHormann: incorrect D!');

  //
  // Prepare
  //
  if d > n - 1 then
    begin
      d := n - 1;
    end;
  b.n := n;

  //
  // special case: N=1
  //
  if n = 1 then
    begin
      SetLength(b.x, n);
      SetLength(b.y, n);
      SetLength(b.w, n);
      b.x[0] := x[0];
      b.y[0] := y[0];
      b.w[0] := 1;
      BarycentricNormalize(b);
      Exit;
    end;

  //
  // Fill X/Y
  //
  SetLength(b.x, n);
  SetLength(b.y, n);
  APVMove(@b.x[0], 0, n - 1, @x[0], 0, n - 1);
  APVMove(@b.y[0], 0, n - 1, @y[0], 0, n - 1);
  TagSortFastR(b.x, b.y, n);

  //
  // Calculate Wk
  //
  SetLength(b.w, n);
  S0 := 1;
  k := 1;
  while k <= d do
    begin
      S0 := -S0;
      inc(k);
    end;
  k := 0;
  while k <= n - 1 do
    begin

      //
      // Wk
      //
      s := 0;
      i := Max(k - d, 0);
      while i <= Min(k, n - 1 - d) do
        begin
          v := 1;
          j := i;
          while j <= i + d do
            begin
              if j <> k then
                begin
                  v := v / AbsReal(b.x[k] - b.x[j]);
                end;
              inc(j);
            end;
          s := s + v;
          inc(i);
        end;
      b.w[k] := S0 * s;

      //
      // Next S0
      //
      S0 := -S0;
      inc(k);
    end;

  //
  // Normalize
  //
  BarycentricNormalize(b);
end;

(* ************************************************************************
  Weghted rational least squares fitting using Floater-Hormann rational functions with optimal D chosen from [0,9],
  with constraints and individual weights.

  Equidistant grid with M node on [min(x),max(x)] is used to build basis functions.
  Different values of D are tried, optimal D (least WEIGHTED root mean square error) is chosen.
  Task is linear, so linear least squares solver is used.
  Complexity of this computational scheme is O(N*M^2) (mostly dominated by the least squares solver).

  SEE ALSO
  * BarycentricFitFloaterHormann(), "lightweight" fitting without invididual weights and constraints.

  INPUT PARAMETERS:
  X   -   points, array[0..N-1].
  Y   -   function values, array[0..N-1].

  W   -   weights, array[0..N-1]
  Each summand in square sum of approximation deviations from given values is multiplied by the square of corresponding weight.
  Fill it by 1's if you don't want to solve weighted task.

  N   -   number of points, N>0.
  XC  -   points where function values/derivatives are constrained, array[0..K-1].
  YC  -   values of constraints, array[0..K-1]

  DC  -   array[0..K-1], types of constraints:
  * DC[i]=0   means that S(XC[i])=YC[i]
  * DC[i]=1   means that S'(XC[i])=YC[i]
  SEE BELOW FOR IMPORTANT INFORMATION ON CONSTRAINTS

  K   -   number of constraints, 0<=K<M.
  K=0 means no constraints (XC/YC/DC are not used in such cases)

  M   -   number of basis functions ( = number_of_nodes), M>=2.

  OUTPUT PARAMETERS:
  Info-   same format as in LSFitLinearWC() subroutine.
  * Info>0    task is solved
  * Info<=0   an error occured:
  -4 means inconvergence of internal SVD
  -3 means inconsistent constraints
  -1 means another errors in parameters passed (N<=0, for example)
  B   -   barycentric interpolant.
  Rep -   report, same format as in LSFitLinearWC() subroutine.
  Following fields are set:
  * DBest         best value of the D parameter
  * RMSError      rms error on the (X,Y).
  * AvgError      average error on the (X,Y).
  * AvgRelError   average relative error on the non-zero Y
  * MaxError      maximum error
  NON-WEIGHTED ERRORS ARE CALCULATED

  IMPORTANT:
  this subroitine doesn't calculate task's condition number for K<>0.

  SETTING CONSTRAINTS - DANGERS AND OPPORTUNITIES:

  Setting constraints can lead to undesired results,
  like ill-conditioned behavior, or inconsistency being detected. From the other side,
  it allows us to improve quality of the fit. Here we summarize our experience with constrained barycentric interpolants:
  * excessive constraints can be inconsistent. Floater-Hormann basis functions aren't as flexible as splines (although they are very smooth).
  * the more evenly constraints are spread across [min(x),max(x)], the more chances that they will be consistent
  * the greater is M (given fixed constraints), the more chances that constraints will be consistent
  * in the general case, consistency of constraints IS NOT GUARANTEED.
  * in the several special cases, however, we CAN guarantee consistency.
  * one of this cases is constraints on the function VALUES at the interval boundaries.
  Note that consustency of the constraints on the function DERIVATIVES is NOT guaranteed (you can use in such cases cubic splines which are more flexible).
  * another special case is ONE constraint on the function value (OR, but not AND, derivative) anywhere in the interval

  Our final recommendation is to use constraints WHEN AND ONLY WHEN you can't solve your task without them.
  Anything beyond special cases given above is not guaranteed and may result in inconsistency.
  ************************************************************************ *)
procedure BarycentricFitFloaterHormannWC(x, y, w: TLVec; n: TLInt; XC, YC: TLVec; DC: TLIVec; k, M: TLInt; var Info: TLInt; var b: TBarycentricInterpolant; var Rep: TBarycentricFitReport);
var
  d: TLInt;
  i: TLInt;
  WRMSCur: TLFloat;
  WRMSBest: TLFloat;
  LocB: TBarycentricInterpolant;
  LocRep: TBarycentricFitReport;
  LocInfo: TLInt;
begin
  if (n < 1) or (M < 2) or (k < 0) or (k >= M) then
    begin
      Info := -1;
      Exit;
    end;

  //
  // Find optimal D
  //
  // Info is -3 by default (degenerate constraints).
  // If LocInfo will always be equal to -3, Info will remain equal to -3.
  // If at least once LocInfo will be -4, Info will be -4.
  //
  WRMSBest := MaxRealNumber;
  Rep.DBest := -1;
  Info := -3;
  d := 0;
  while d <= Min(9, n - 1) do
    begin
      BarycentricFitWCFixedD(x, y, w, n, XC, YC, DC, k, M, d, LocInfo,
        LocB, LocRep);
      Assert((LocInfo = -4) or (LocInfo = -3) or (LocInfo > 0),
        'BarycentricFitFloaterHormannWC: unexpected result from BarycentricFitWCFixedD!');
      if LocInfo > 0 then
        begin

          //
          // Calculate weghted RMS
          //
          WRMSCur := 0;
          i := 0;
          while i <= n - 1 do
            begin
              WRMSCur := WRMSCur +
                AP_Sqr(w[i] * (y[i] - BarycentricCalc(LocB, x[i])));
              inc(i);
            end;
          WRMSCur := Sqrt(WRMSCur / n);
          if AP_FP_Less(WRMSCur, WRMSBest) or (Rep.DBest < 0) then
            begin
              BarycentricCopy(LocB, b);
              Rep.DBest := d;
              Info := 1;
              Rep.RMSError := LocRep.RMSError;
              Rep.AvgError := LocRep.AvgError;
              Rep.AvgRelError := LocRep.AvgRelError;
              Rep.MaxError := LocRep.MaxError;
              Rep.TaskRCond := LocRep.TaskRCond;
              WRMSBest := WRMSCur;
            end;
        end
      else
        begin
          if (LocInfo <> -3) and (Info < 0) then
            begin
              Info := LocInfo;
            end;
        end;
      inc(d);
    end;
end;

(* ************************************************************************
  Rational least squares fitting, without weights and constraints.
  See BarycentricFitFloaterHormannWC() for more information.
  ************************************************************************ *)
procedure BarycentricFitFloaterHormann(x, y: TLVec; n: TLInt; M: TLInt; var Info: TLInt; var b: TBarycentricInterpolant; var Rep: TBarycentricFitReport);
var
  w: TLVec;
  XC: TLVec;
  YC: TLVec;
  DC: TLIVec;
  i: TLInt;
begin
  if n < 1 then
    begin
      Info := -1;
      Exit;
    end;
  SetLength(w, n);
  i := 0;
  while i <= n - 1 do
    begin
      w[i] := 1;
      inc(i);
    end;
  BarycentricFitFloaterHormannWC(x, y, w, n, XC, YC, DC, 0, M, Info, b, Rep);
end;

(* ************************************************************************
  Normalization of barycentric interpolant:
  * B.N, B.X, B.Y and B.W are initialized
  * B.SY is NOT initialized
  * Y[] is normalized, scaling coefficient is stored in B.SY
  * W[] is normalized, no scaling coefficient is stored
  * X[] is sorted

  Internal subroutine.
  ************************************************************************ *)
procedure BarycentricNormalize(var b: TBarycentricInterpolant);
var
  p1: TLIVec;
  p2: TLIVec;
  i: TLInt;
  j: TLInt;
  j2: TLInt;
  v: TLFloat;
begin

  //
  // Normalize task: |Y|<=1, |W|<=1, sort X[]
  //
  b.SY := 0;
  i := 0;
  while i <= b.n - 1 do
    begin
      b.SY := Max(b.SY, AbsReal(b.y[i]));
      inc(i);
    end;
  if AP_FP_Greater(b.SY, 0) and AP_FP_Greater(AbsReal(b.SY - 1),
    10 * MachineEpsilon) then
    begin
      v := 1 / b.SY;
      APVMul(@b.y[0], 0, b.n - 1, v);
    end;
  v := 0;
  i := 0;
  while i <= b.n - 1 do
    begin
      v := Max(v, AbsReal(b.w[i]));
      inc(i);
    end;
  if AP_FP_Greater(v, 0) and AP_FP_Greater(AbsReal(v - 1), 10 * MachineEpsilon)
  then
    begin
      v := 1 / v;
      APVMul(@b.w[0], 0, b.n - 1, v);
    end;
  i := 0;
  while i <= b.n - 2 do
    begin
      if AP_FP_Less(b.x[i + 1], b.x[i]) then
        begin
          TagSort(b.x, b.n, p1, p2);
          j := 0;
          while j <= b.n - 1 do
            begin
              j2 := p2[j];
              v := b.y[j];
              b.y[j] := b.y[j2];
              b.y[j2] := v;
              v := b.w[j];
              b.w[j] := b.w[j2];
              b.w[j2] := v;
              inc(j);
            end;
          Break;
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Internal subroutine, calculates barycentric basis functions.
  Used for efficient simultaneous calculation of N basis functions.
  ************************************************************************ *)
procedure BarycentricCalcBasis(b: TBarycentricInterpolant; t: TLFloat;
  var y: TLVec);
var
  s2: TLFloat;
  s: TLFloat;
  v: TLFloat;
  i: TLInt;
  j: TLInt;
begin

  //
  // special case: N=1
  //
  if b.n = 1 then
    begin
      y[0] := 1;
      Exit;
    end;

  //
  // Here we assume that task is normalized, i.e.:
  // 1. abs(Y[i])<=1
  // 2. abs(W[i])<=1
  // 3. X[] is ordered
  //
  // First, we decide: should we use "safe" formula (guarded
  // against overflow) or fast one?
  //
  s := AbsReal(t - b.x[0]);
  i := 0;
  while i <= b.n - 1 do
    begin
      v := b.x[i];
      if AP_FP_Eq(v, t) then
        begin
          j := 0;
          while j <= b.n - 1 do
            begin
              y[j] := 0;
              inc(j);
            end;
          y[i] := 1;
          Exit;
        end;
      v := AbsReal(t - v);
      if AP_FP_Less(v, s) then
        begin
          s := v;
        end;
      inc(i);
    end;
  s2 := 0;
  i := 0;
  while i <= b.n - 1 do
    begin
      v := s / (t - b.x[i]);
      v := v * b.w[i];
      y[i] := v;
      s2 := s2 + v;
      inc(i);
    end;
  v := 1 / s2;
  APVMul(@y[0], 0, b.n - 1, v);
end;

(* ************************************************************************
  Internal Floater-Hormann fitting subroutine for fixed D
  ************************************************************************ *)
procedure BarycentricFitWCFixedD(x, y: TLVec;
  w: TLVec; n: TLInt; XC: TLVec; YC: TLVec;
  DC: TLIVec; k: TLInt; M: TLInt;
  d: TLInt; var Info: TLInt; var b: TBarycentricInterpolant;
  var Rep: TBarycentricFitReport);
var
  FMatrix: TLMatrix;
  CMatrix: TLMatrix;
  y2: TLVec;
  W2: TLVec;
  SX: TLVec;
  SY: TLVec;
  SBF: TLVec;
  XOriginal: TLVec;
  YOriginal: TLVec;
  tmp: TLVec;
  LRep: TLSFitReport;
  v0: TLFloat;
  v1: TLFloat;
  mx: TLFloat;
  b2: TBarycentricInterpolant;
  i: TLInt;
  j: TLInt;
  RelCnt: TLInt;
  XA: TLFloat;
  XB: TLFloat;
  SA: TLFloat;
  SB: TLFloat;
  Decay: TLFloat;
begin
  x := DynamicArrayCopy(x);
  y := DynamicArrayCopy(y);
  XC := DynamicArrayCopy(XC);
  YC := DynamicArrayCopy(YC);
  if (n < 1) or (M < 2) or (k < 0) or (k >= M) then
    begin
      Info := -1;
      Exit;
    end;
  i := 0;
  while i <= k - 1 do
    begin
      Info := 0;
      if DC[i] < 0 then
        begin
          Info := -1;
        end;
      if DC[i] > 1 then
        begin
          Info := -1;
        end;
      if Info < 0 then
        begin
          Exit;
        end;
      inc(i);
    end;

  //
  // weight decay for correct handling of task which becomes
  // degenerate after constraints are applied
  //
  Decay := 10000 * MachineEpsilon;

  //
  // Scale X, Y, XC, YC
  //
  LSFitScaleXY(x, y, n, XC, YC, DC, k, XA, XB, SA, SB, XOriginal, YOriginal);

  //
  // allocate space, initialize:
  // * FMatrix-   values of basis functions at X[]
  // * CMatrix-   values (derivatives) of basis functions at XC[]
  //
  SetLength(y2, n + M);
  SetLength(W2, n + M);
  SetLength(FMatrix, n + M, M);
  if k > 0 then
    begin
      SetLength(CMatrix, k, M + 1);
    end;
  SetLength(y2, n + M);
  SetLength(W2, n + M);

  //
  // Prepare design and constraints matrices:
  // * fill constraints matrix
  // * fill first N rows of design matrix with values
  // * fill next M rows of design matrix with regularizing term
  // * append M zeros to Y
  // * append M elements, mean(abs(W)) each, to W
  //
  SetLength(SX, M);
  SetLength(SY, M);
  SetLength(SBF, M);
  j := 0;
  while j <= M - 1 do
    begin
      SX[j] := AP_Float(2 * j) / (M - 1) - 1;
      inc(j);
    end;
  i := 0;
  while i <= M - 1 do
    begin
      SY[i] := 1;
      inc(i);
    end;
  BarycentricBuildFloaterHormann(SX, SY, M, d, b2);
  mx := 0;
  i := 0;
  while i <= n - 1 do
    begin
      BarycentricCalcBasis(b2, x[i], SBF);
      APVMove(@FMatrix[i][0], 0, M - 1, @SBF[0], 0, M - 1);
      y2[i] := y[i];
      W2[i] := w[i];
      mx := mx + AbsReal(w[i]) / n;
      inc(i);
    end;
  i := 0;
  while i <= M - 1 do
    begin
      j := 0;
      while j <= M - 1 do
        begin
          if i = j then
            begin
              FMatrix[n + i, j] := Decay;
            end
          else
            begin
              FMatrix[n + i, j] := 0;
            end;
          inc(j);
        end;
      y2[n + i] := 0;
      W2[n + i] := mx;
      inc(i);
    end;
  if k > 0 then
    begin
      j := 0;
      while j <= M - 1 do
        begin
          i := 0;
          while i <= M - 1 do
            begin
              SY[i] := 0;
              inc(i);
            end;
          SY[j] := 1;
          BarycentricBuildFloaterHormann(SX, SY, M, d, b2);
          i := 0;
          while i <= k - 1 do
            begin
              Assert((DC[i] >= 0) and (DC[i] <= 1),
                'BarycentricFit: internal error!');
              BarycentricDiff1(b2, XC[i], v0, v1);
              if DC[i] = 0 then
                begin
                  CMatrix[i, j] := v0;
                end;
              if DC[i] = 1 then
                begin
                  CMatrix[i, j] := v1;
                end;
              inc(i);
            end;
          inc(j);
        end;
      i := 0;
      while i <= k - 1 do
        begin
          CMatrix[i, M] := YC[i];
          inc(i);
        end;
    end;

  //
  // Solve constrained task
  //
  if k > 0 then
    begin

      //
      // solve using regularization
      //
      LSFitLinearWC(y2, W2, FMatrix, CMatrix, n + M, M, k, Info, tmp, LRep);
    end
  else
    begin

      //
      // no constraints, no regularization needed
      //
      LSFitLinearWC(y, w, FMatrix, CMatrix, n, M, k, Info, tmp, LRep);
    end;
  if Info < 0 then
    begin
      Exit;
    end;

  //
  // Generate interpolant and scale it
  //
  APVMove(@SY[0], 0, M - 1, @tmp[0], 0, M - 1);
  BarycentricBuildFloaterHormann(SX, SY, M, d, b);
  BarycentricLinTransX(b, 2 / (XB - XA), -(XA + XB) / (XB - XA));
  BarycentricLinTransY(b, SB - SA, SA);

  //
  // Scale absolute errors obtained from LSFitLinearW.
  // Relative error should be calculated separately
  // (because of shifting/scaling of the task)
  //
  Rep.TaskRCond := LRep.TaskRCond;
  Rep.RMSError := LRep.RMSError * (SB - SA);
  Rep.AvgError := LRep.AvgError * (SB - SA);
  Rep.MaxError := LRep.MaxError * (SB - SA);
  Rep.AvgRelError := 0;
  RelCnt := 0;
  i := 0;
  while i <= n - 1 do
    begin
      if AP_FP_NEq(YOriginal[i], 0) then
        begin
          Rep.AvgRelError := Rep.AvgRelError +
            AbsReal(BarycentricCalc(b, XOriginal[i]) - YOriginal[i]) /
            AbsReal(YOriginal[i]);
          RelCnt := RelCnt + 1;
        end;
      inc(i);
    end;
  if RelCnt <> 0 then
    begin
      Rep.AvgRelError := Rep.AvgRelError / RelCnt;
    end;
end;

(* ************************************************************************
  Lagrange intepolant: generation of the model on the general grid.
  This function has O(N^2) complexity.

  INPUT PARAMETERS:
  X   -   abscissas, array[0..N-1]
  Y   -   function values, array[0..N-1]
  N   -   number of points, N>=1

  OIYTPUT PARAMETERS
  P   -   barycentric model which represents Lagrange interpolant
  (see ratint unit info and BarycentricCalc() description for more information).
  ************************************************************************ *)
procedure PolynomialBuild(x, y: TLVec; n: TLInt; var p: TBarycentricInterpolant);
var
  j: TLInt;
  k: TLInt;
  w: TLVec;
  b: TLFloat;
  a: TLFloat;
  v: TLFloat;
  mx: TLFloat;
begin
  Assert(n > 0, 'PolIntBuild: N<=0!');

  //
  // calculate W[j]
  // multi-pass algorithm is used to avoid overflow
  //
  SetLength(w, n);
  a := x[0];
  b := x[0];
  j := 0;
  while j <= n - 1 do
    begin
      w[j] := 1;
      a := Min(a, x[j]);
      b := Max(b, x[j]);
      inc(j);
    end;
  k := 0;
  while k <= n - 1 do
    begin

      //
      // W[K] is used instead of 0.0 because
      // cycle on J does not touch K-th element
      // and we MUST get maximum from ALL elements
      //
      mx := AbsReal(w[k]);
      j := 0;
      while j <= n - 1 do
        begin
          if j <> k then
            begin
              v := (b - a) / (x[j] - x[k]);
              w[j] := w[j] * v;
              mx := Max(mx, AbsReal(w[j]));
            end;
          inc(j);
        end;
      if k mod 5 = 0 then
        begin

          //
          // every 5-th run we renormalize W[]
          //
          v := 1 / mx;
          APVMul(@w[0], 0, n - 1, v);
        end;
      inc(k);
    end;
  BarycentricBuildXYW(x, y, w, n, p);
end;

(* ************************************************************************
  Lagrange intepolant: generation of the model on equidistant grid.
  This function has O(N) complexity.

  INPUT PARAMETERS:
  A   -   left boundary of [A,B]
  B   -   right boundary of [A,B]
  Y   -   function values at the nodes, array[0..N-1]
  N   -   number of points, N>=1 for N=1 a constant model is constructed.

  OIYTPUT PARAMETERS
  P   -   barycentric model which represents Lagrange interpolant
  (see ratint unit info and BarycentricCalc() description for more information).
  ************************************************************************ *)
procedure PolynomialBuildEqDist(a: TLFloat; b: TLFloat; y: TLVec; n: TLInt; var p: TBarycentricInterpolant);
var
  i: TLInt;
  w: TLVec;
  x: TLVec;
  v: TLFloat;
begin
  Assert(n > 0, 'PolIntBuildEqDist: N<=0!');

  //
  // Special case: N=1
  //
  if n = 1 then
    begin
      SetLength(x, 1);
      SetLength(w, 1);
      x[0] := 0.5 * (b + a);
      w[0] := 1;
      BarycentricBuildXYW(x, y, w, 1, p);
      Exit;
    end;

  //
  // general case
  //
  SetLength(x, n);
  SetLength(w, n);
  v := 1;
  i := 0;
  while i <= n - 1 do
    begin
      w[i] := v;
      x[i] := a + (b - a) * i / (n - 1);
      v := -v * (n - 1 - i);
      v := v / (i + 1);
      inc(i);
    end;
  BarycentricBuildXYW(x, y, w, n, p);
end;

(* ************************************************************************
  Lagrange intepolant on Chebyshev grid (first kind).
  This function has O(N) complexity.

  INPUT PARAMETERS:
  A   -   left boundary of [A,B]
  B   -   right boundary of [A,B]
  Y   -   function values at the nodes, array[0..N-1], Y[I] = Y(0.5*(B+A) + 0.5*(B-A)*Cos(PI*(2*i+1)/(2*n)))
  N   -   number of points, N>=1 for N=1 a constant model is constructed.

  OIYTPUT PARAMETERS
  P   -   barycentric model which represents Lagrange interpolant
  (see ratint unit info and BarycentricCalc() description for more information).
  ************************************************************************ *)
procedure PolynomialBuildCheb1(a: TLFloat; b: TLFloat; y: TLVec; n: TLInt; var p: TBarycentricInterpolant);
var
  i: TLInt;
  w: TLVec;
  x: TLVec;
  v: TLFloat;
  t: TLFloat;
begin
  Assert(n > 0, 'PolIntBuildCheb1: N<=0!');

  //
  // Special case: N=1
  //
  if n = 1 then
    begin
      SetLength(x, 1);
      SetLength(w, 1);
      x[0] := 0.5 * (b + a);
      w[0] := 1;
      BarycentricBuildXYW(x, y, w, 1, p);
      Exit;
    end;

  //
  // general case
  //
  SetLength(x, n);
  SetLength(w, n);
  v := 1;
  i := 0;
  while i <= n - 1 do
    begin
      t := Tan(0.5 * pi * (2 * i + 1) / (2 * n));
      w[i] := 2 * v * t / (1 + AP_Sqr(t));
      x[i] := 0.5 * (b + a) + 0.5 * (b - a) * (1 - AP_Sqr(t)) / (1 + AP_Sqr(t));
      v := -v;
      inc(i);
    end;
  BarycentricBuildXYW(x, y, w, n, p);
end;

(* ************************************************************************
  Lagrange intepolant on Chebyshev grid (second kind).
  This function has O(N) complexity.

  INPUT PARAMETERS:
  A   -   left boundary of [A,B]
  B   -   right boundary of [A,B]
  Y   -   function values at the nodes, array[0..N-1], Y[I] = Y(0.5*(B+A) + 0.5*(B-A)*Cos(PI*i/(n-1)))
  N   -   number of points, N>=1 for N=1 a constant model is constructed.

  OIYTPUT PARAMETERS
  P   -   barycentric model which represents Lagrange interpolant
  (see ratint unit info and BarycentricCalc() description for more information).
  ************************************************************************ *)
procedure PolynomialBuildCheb2(a: TLFloat; b: TLFloat; y: TLVec; n: TLInt; var p: TBarycentricInterpolant);
var
  i: TLInt;
  w: TLVec;
  x: TLVec;
  v: TLFloat;
begin
  Assert(n > 0, 'PolIntBuildCheb2: N<=0!');

  //
  // Special case: N=1
  //
  if n = 1 then
    begin
      SetLength(x, 1);
      SetLength(w, 1);
      x[0] := 0.5 * (b + a);
      w[0] := 1;
      BarycentricBuildXYW(x, y, w, 1, p);
      Exit;
    end;

  //
  // general case
  //
  SetLength(x, n);
  SetLength(w, n);
  v := 1;
  i := 0;
  while i <= n - 1 do
    begin
      if (i = 0) or (i = n - 1) then
        begin
          w[i] := v * 0.5;
        end
      else
        begin
          w[i] := v;
        end;
      x[i] := 0.5 * (b + a) + 0.5 * (b - a) * Cos(pi * i / (n - 1));
      v := -v;
      inc(i);
    end;
  BarycentricBuildXYW(x, y, w, n, p);
end;

(* ************************************************************************
  Fast equidistant polynomial interpolation function with O(N) complexity

  INPUT PARAMETERS:
  A   -   left boundary of [A,B]
  B   -   right boundary of [A,B]
  F   -   function values, array[0..N-1]
  N   -   number of points on equidistant grid, N>=1 for N=1 a constant model is constructed.
  T   -   position where P(x) is calculated

  RESULT
  value of the Lagrange interpolant at T

  IMPORTANT
  this function provides fast

  interface which is not overflow-safe nor it is very precise.
  the best option is to use PolynomialBuildEqDist()/BarycentricCalc() subroutines unless you are pretty sure that your data will not result in overflow.
  ************************************************************************ *)
function PolynomialCalcEqDist(a: TLFloat; b: TLFloat; f: TLVec; n: TLInt; t: TLFloat): TLFloat;
var
  s1: TLFloat;
  s2: TLFloat;
  v: TLFloat;
  Threshold: TLFloat;
  s: TLFloat;
  h: TLFloat;
  i: TLInt;
  j: TLInt;
  w: TLFloat;
  x: TLFloat;
begin
  Assert(n > 0, 'PolIntEqDist: N<=0!');
  Threshold := Sqrt(MinRealNumber);

  //
  // Special case: N=1
  //
  if n = 1 then
    begin
      Result := f[0];
      Exit;
    end;

  //
  // First, decide: should we use "safe" formula (guarded
  // against overflow) or fast one?
  //
  j := 0;
  s := t - a;
  i := 1;
  while i <= n - 1 do
    begin
      x := a + AP_Float(i) / (n - 1) * (b - a);
      if AP_FP_Less(AbsReal(t - x), AbsReal(s)) then
        begin
          s := t - x;
          j := i;
        end;
      inc(i);
    end;
  if AP_FP_Eq(s, 0) then
    begin
      Result := f[j];
      Exit;
    end;
  if AP_FP_Greater(AbsReal(s), Threshold) then
    begin

      //
      // use fast formula
      //
      j := -1;
      s := 1.0;
    end;

  //
  // Calculate using safe or fast barycentric formula
  //
  s1 := 0;
  s2 := 0;
  w := 1.0;
  h := (b - a) / (n - 1);
  i := 0;
  while i <= n - 1 do
    begin
      if i <> j then
        begin
          v := s * w / (t - (a + i * h));
          s1 := s1 + v * f[i];
          s2 := s2 + v;
        end
      else
        begin
          v := w;
          s1 := s1 + v * f[i];
          s2 := s2 + v;
        end;
      w := -w * (n - 1 - i);
      w := w / (i + 1);
      inc(i);
    end;
  Result := s1 / s2;
end;

(* ************************************************************************
  Fast polynomial interpolation function on Chebyshev points (first kind)
  with O(N) complexity.

  INPUT PARAMETERS:
  A   -   left boundary of [A,B]
  B   -   right boundary of [A,B]
  F   -   function values, array[0..N-1]
  N   -   number of points on Chebyshev grid (first kind), X[i] = 0.5*(B+A) + 0.5*(B-A)*Cos(PI*(2*i+1)/(2*n)) for N=1 a constant model is constructed.
  T   -   position where P(x) is calculated

  RESULT
  value of the Lagrange interpolant at T

  IMPORTANT
  this function provides fast

  interface which is not overflow-safe nor it is very precise.
  the best option is to use PolIntBuildCheb1()/BarycentricCalc() subroutines unless you are pretty sure that your data will not result in overflow.
  ************************************************************************ *)
function PolynomialCalcCheb1(a: TLFloat; b: TLFloat; f: TLVec; n: TLInt; t: TLFloat): TLFloat;
var
  s1: TLFloat;
  s2: TLFloat;
  v: TLFloat;
  Threshold: TLFloat;
  s: TLFloat;
  i: TLInt;
  j: TLInt;
  A0: TLFloat;
  Delta: TLFloat;
  alpha: TLFloat;
  beta: TLFloat;
  ca: TLFloat;
  SA: TLFloat;
  TempC: TLFloat;
  TempS: TLFloat;
  x: TLFloat;
  w: TLFloat;
  p1: TLFloat;
begin
  Assert(n > 0, 'PolIntCheb1: N<=0!');
  Threshold := Sqrt(MinRealNumber);
  t := (t - 0.5 * (a + b)) / (0.5 * (b - a));

  //
  // Fast exit
  //
  if n = 1 then
    begin
      Result := f[0];
      Exit;
    end;

  //
  // Prepare information for the recurrence formula
  // used to calculate sin(pi*(2j+1)/(2n+2)) and
  // cos(pi*(2j+1)/(2n+2)):
  //
  // A0    = pi/(2n+2)
  // Delta = pi/(n+1)
  // Alpha = 2 sin^2 (Delta/2)
  // Beta  = sin(Delta)
  //
  // so that sin(..) = sin(A0+j*delta) and cos(..) = cos(A0+j*delta).
  // Then we use
  //
  // sin(x+delta) = sin(x) - (alpha*sin(x) - beta*cos(x))
  // cos(x+delta) = cos(x) - (alpha*cos(x) - beta*sin(x))
  //
  // to repeatedly calculate sin(..) and cos(..).
  //
  A0 := pi / (2 * (n - 1) + 2);
  Delta := 2 * pi / (2 * (n - 1) + 2);
  alpha := 2 * AP_Sqr(Sin(Delta / 2));
  beta := Sin(Delta);

  //
  // First, decide: should we use "safe" formula (guarded
  // against overflow) or fast one?
  //
  ca := Cos(A0);
  SA := Sin(A0);
  j := 0;
  x := ca;
  s := t - x;
  i := 1;
  while i <= n - 1 do
    begin

      //
      // Next X[i]
      //
      TempS := SA - (alpha * SA - beta * ca);
      TempC := ca - (alpha * ca + beta * SA);
      SA := TempS;
      ca := TempC;
      x := ca;

      //
      // Use X[i]
      //
      if AP_FP_Less(AbsReal(t - x), AbsReal(s)) then
        begin
          s := t - x;
          j := i;
        end;
      inc(i);
    end;
  if AP_FP_Eq(s, 0) then
    begin
      Result := f[j];
      Exit;
    end;
  if AP_FP_Greater(AbsReal(s), Threshold) then
    begin

      //
      // use fast formula
      //
      j := -1;
      s := 1.0;
    end;

  //
  // Calculate using safe or fast barycentric formula
  //
  s1 := 0;
  s2 := 0;
  ca := Cos(A0);
  SA := Sin(A0);
  p1 := 1.0;
  i := 0;
  while i <= n - 1 do
    begin

      //
      // Calculate X[i], W[i]
      //
      x := ca;
      w := p1 * SA;

      //
      // Proceed
      //
      if i <> j then
        begin
          v := s * w / (t - x);
          s1 := s1 + v * f[i];
          s2 := s2 + v;
        end
      else
        begin
          v := w;
          s1 := s1 + v * f[i];
          s2 := s2 + v;
        end;

      //
      // Next CA, SA, P1
      //
      TempS := SA - (alpha * SA - beta * ca);
      TempC := ca - (alpha * ca + beta * SA);
      SA := TempS;
      ca := TempC;
      p1 := -p1;
      inc(i);
    end;
  Result := s1 / s2;
end;

(* ************************************************************************
  Fast polynomial interpolation function on Chebyshev points (second kind)
  with O(N) complexity.

  INPUT PARAMETERS:
  A   -   left boundary of [A,B]
  B   -   right boundary of [A,B]
  F   -   function values, array[0..N-1]
  N   -   number of points on Chebyshev grid (second kind),
  X[i] = 0.5*(B+A) + 0.5*(B-A)*Cos(PI*i/(n-1))
  for N=1 a constant model is constructed.
  T   -   position where P(x) is calculated

  RESULT
  value of the Lagrange interpolant at T

  IMPORTANT
  this function provides fast

  interface which is not overflow-safe nor it is very precise.
  the best option is to use PolIntBuildCheb2()/BarycentricCalc() subroutines unless you are pretty sure that your data will not result in overflow.
  ************************************************************************ *)
function PolynomialCalcCheb2(a: TLFloat; b: TLFloat; f: TLVec; n: TLInt; t: TLFloat): TLFloat;
var
  s1: TLFloat;
  s2: TLFloat;
  v: TLFloat;
  Threshold: TLFloat;
  s: TLFloat;
  i: TLInt;
  j: TLInt;
  A0: TLFloat;
  Delta: TLFloat;
  alpha: TLFloat;
  beta: TLFloat;
  ca: TLFloat;
  SA: TLFloat;
  TempC: TLFloat;
  TempS: TLFloat;
  x: TLFloat;
  w: TLFloat;
  p1: TLFloat;
begin
  Assert(n > 0, 'PolIntCheb2: N<=0!');
  Threshold := Sqrt(MinRealNumber);
  t := (t - 0.5 * (a + b)) / (0.5 * (b - a));

  //
  // Fast exit
  //
  if n = 1 then
    begin
      Result := f[0];
      Exit;
    end;

  //
  // Prepare information for the recurrence formula
  // used to calculate sin(pi*i/n) and
  // cos(pi*i/n):
  //
  // A0    = 0
  // Delta = pi/n
  // Alpha = 2 sin^2 (Delta/2)
  // Beta  = sin(Delta)
  //
  // so that sin(..) = sin(A0+j*delta) and cos(..) = cos(A0+j*delta).
  // Then we use
  //
  // sin(x+delta) = sin(x) - (alpha*sin(x) - beta*cos(x))
  // cos(x+delta) = cos(x) - (alpha*cos(x) - beta*sin(x))
  //
  // to repeatedly calculate sin(..) and cos(..).
  //
  A0 := 0.0;
  Delta := pi / (n - 1);
  alpha := 2 * AP_Sqr(Sin(Delta / 2));
  beta := Sin(Delta);

  //
  // First, decide: should we use "safe" formula (guarded
  // against overflow) or fast one?
  //
  ca := Cos(A0);
  SA := Sin(A0);
  j := 0;
  x := ca;
  s := t - x;
  i := 1;
  while i <= n - 1 do
    begin

      //
      // Next X[i]
      //
      TempS := SA - (alpha * SA - beta * ca);
      TempC := ca - (alpha * ca + beta * SA);
      SA := TempS;
      ca := TempC;
      x := ca;

      //
      // Use X[i]
      //
      if AP_FP_Less(AbsReal(t - x), AbsReal(s)) then
        begin
          s := t - x;
          j := i;
        end;
      inc(i);
    end;
  if AP_FP_Eq(s, 0) then
    begin
      Result := f[j];
      Exit;
    end;
  if AP_FP_Greater(AbsReal(s), Threshold) then
    begin

      //
      // use fast formula
      //
      j := -1;
      s := 1.0;
    end;

  //
  // Calculate using safe or fast barycentric formula
  //
  s1 := 0;
  s2 := 0;
  ca := Cos(A0);
  SA := Sin(A0);
  p1 := 1.0;
  i := 0;
  while i <= n - 1 do
    begin

      //
      // Calculate X[i], W[i]
      //
      x := ca;
      if (i = 0) or (i = n - 1) then
        begin
          w := 0.5 * p1;
        end
      else
        begin
          w := 1.0 * p1;
        end;

      //
      // Proceed
      //
      if i <> j then
        begin
          v := s * w / (t - x);
          s1 := s1 + v * f[i];
          s2 := s2 + v;
        end
      else
        begin
          v := w;
          s1 := s1 + v * f[i];
          s2 := s2 + v;
        end;

      //
      // Next CA, SA, P1
      //
      TempS := SA - (alpha * SA - beta * ca);
      TempC := ca - (alpha * ca + beta * SA);
      SA := TempS;
      ca := TempC;
      p1 := -p1;
      inc(i);
    end;
  Result := s1 / s2;
end;

(* ************************************************************************
  Least squares fitting by polynomial.

  This subroutine is "lightweight" alternative for more TLComplex and feature-rich PolynomialFitWC().
  See PolynomialFitWC() for more information about subroutine parameters (we don't duplicate it here because of length)
  ************************************************************************ *)
procedure PolynomialFit(x, y: TLVec; n, M: TLInt; var Info: TLInt; var p: TBarycentricInterpolant; var Rep: TPolynomialFitReport);
var
  i: TLInt;
  w: TLVec;
  XC: TLVec;
  YC: TLVec;
  DC: TLIVec;
begin
  if n > 0 then
    begin
      SetLength(w, n);
      i := 0;
      while i <= n - 1 do
        begin
          w[i] := 1;
          inc(i);
        end;
    end;
  PolynomialFitWC(x, y, w, n, XC, YC, DC, 0, M, Info, p, Rep);
end;

(* ************************************************************************
  Weighted fitting by Chebyshev polynomial in barycentric form, with constraints on function values or first derivatives.

  Small regularizing term is used when solving constrained tasks (to improve stability).

  Task is linear, so linear least squares solver is used. Complexity of this computational scheme is O(N*M^2), mostly dominated by least squares solver

  SEE ALSO:
  PolynomialFit()

  INPUT PARAMETERS:
  X   -   points, array[0..N-1].
  Y   -   function values, array[0..N-1].

  W   -   weights, array[0..N-1]
  Each summand in square sum of approximation deviations from given values is multiplied by the square of corresponding weight.
  Fill it by 1's if you don't want to solve weighted task.

  N   -   number of points, N>0.
  XC  -   points where polynomial values/derivatives are constrained, array[0..K-1].
  YC  -   values of constraints, array[0..K-1]

  DC  -   array[0..K-1], types of constraints:
  * DC[i]=0   means that P(XC[i])=YC[i]
  * DC[i]=1   means that P'(XC[i])=YC[i]
  SEE BELOW FOR IMPORTANT INFORMATION ON CONSTRAINTS

  K   -   number of constraints, 0<=K<M. K=0 means no constraints (XC/YC/DC are not used in such cases)
  M   -   number of basis functions (= polynomial_degree + 1), M>=1

  OUTPUT PARAMETERS:
  Info-   same format as in LSFitLinearW() subroutine:
  * Info>0    task is solved
  * Info<=0   an error occured:
  -4 means inconvergence of internal SVD
  -3 means inconsistent constraints
  -1 means another errors in parameters passed (N<=0, for example)

  P   -   interpolant in barycentric form.

  Rep -   report, same format as in LSFitLinearW() subroutine.
  Following fields are set:
  * RMSError      rms error on the (X,Y).
  * AvgError      average error on the (X,Y).
  * AvgRelError   average relative error on the non-zero Y
  * MaxError      maximum error
  NON-WEIGHTED ERRORS ARE CALCULATED

  IMPORTANT:
  this subroitine doesn't calculate task's condition number for K<>0.

  SETTING CONSTRAINTS - DANGERS AND OPPORTUNITIES:
  Setting constraints can lead to undesired results, like ill-conditioned behavior, or inconsistency being detected.
  From the other side, it allows us to improve quality of the fit. Here we summarize our experience with constrained regression splines:
  * even simple constraints can be inconsistent, see Wikipedia article on this subject: http://en.wikipedia.org/wiki/Birkhoff_interpolation
  * the greater is M (given fixed constraints), the more chances that constraints will be consistent
  * in the general case, consistency of constraints is NOT GUARANTEED.
  * in the one special cases, however, we can guarantee consistency. This case is: M>1 and constraints on the function values (NOT DERIVATIVES)

  Our final recommendation is to use constraints WHEN AND ONLY when you can't solve your task without them.
  Anything beyond special cases given above is not guaranteed and may result in inconsistency.
  ************************************************************************ *)
procedure PolynomialFitWC(x, y, w: TLVec; n: TLInt; XC, YC: TLVec; DC: TLIVec; k: TLInt; M: TLInt; var Info: TLInt; var p: TBarycentricInterpolant; var Rep: TPolynomialFitReport);
var
  XA: TLFloat;
  XB: TLFloat;
  SA: TLFloat;
  SB: TLFloat;
  XOriginal: TLVec;
  YOriginal: TLVec;
  y2: TLVec;
  W2: TLVec;
  tmp: TLVec;
  TMP2: TLVec;
  TmpDiff: TLVec;
  Bx: TLVec;
  By: TLVec;
  BW: TLVec;
  FMatrix: TLMatrix;
  CMatrix: TLMatrix;
  i: TLInt;
  j: TLInt;
  mx: TLFloat;
  Decay: TLFloat;
  u: TLFloat;
  v: TLFloat;
  s: TLFloat;
  RelCnt: TLInt;
  LRep: TLSFitReport;
begin
  x := DynamicArrayCopy(x);
  y := DynamicArrayCopy(y);
  XC := DynamicArrayCopy(XC);
  YC := DynamicArrayCopy(YC);
  if (M < 1) or (n < 1) or (k < 0) or (k >= M) then
    begin
      Info := -1;
      Exit;
    end;
  i := 0;
  while i <= k - 1 do
    begin
      Info := 0;
      if DC[i] < 0 then
        begin
          Info := -1;
        end;
      if DC[i] > 1 then
        begin
          Info := -1;
        end;
      if Info < 0 then
        begin
          Exit;
        end;
      inc(i);
    end;

  //
  // weight decay for correct handling of task which becomes
  // degenerate after constraints are applied
  //
  Decay := 10000 * MachineEpsilon;

  //
  // Scale X, Y, XC, YC
  //
  LSFitScaleXY(x, y, n, XC, YC, DC, k, XA, XB, SA, SB, XOriginal, YOriginal);

  //
  // allocate space, initialize/fill:
  // * FMatrix-   values of basis functions at X[]
  // * CMatrix-   values (derivatives) of basis functions at XC[]
  // * fill constraints matrix
  // * fill first N rows of design matrix with values
  // * fill next M rows of design matrix with regularizing term
  // * append M zeros to Y
  // * append M elements, mean(abs(W)) each, to W
  //
  SetLength(y2, n + M);
  SetLength(W2, n + M);
  SetLength(tmp, M);
  SetLength(TmpDiff, M);
  SetLength(FMatrix, n + M, M);
  if k > 0 then
    begin
      SetLength(CMatrix, k, M + 1);
    end;

  //
  // Fill design matrix, Y2, W2:
  // * first N rows with basis functions for original points
  // * next M rows with decay terms
  //
  i := 0;
  while i <= n - 1 do
    begin

      //
      // prepare Ith row
      // use Tmp for calculations to avoid multidimensional arrays overhead
      //
      j := 0;
      while j <= M - 1 do
        begin
          if j = 0 then
            begin
              tmp[j] := 1;
            end
          else
            begin
              if j = 1 then
                begin
                  tmp[j] := x[i];
                end
              else
                begin
                  tmp[j] := 2 * x[i] * tmp[j - 1] - tmp[j - 2];
                end;
            end;
          inc(j);
        end;
      APVMove(@FMatrix[i][0], 0, M - 1, @tmp[0], 0, M - 1);
      inc(i);
    end;
  i := 0;
  while i <= M - 1 do
    begin
      j := 0;
      while j <= M - 1 do
        begin
          if i = j then
            begin
              FMatrix[n + i, j] := Decay;
            end
          else
            begin
              FMatrix[n + i, j] := 0;
            end;
          inc(j);
        end;
      inc(i);
    end;
  APVMove(@y2[0], 0, n - 1, @y[0], 0, n - 1);
  APVMove(@W2[0], 0, n - 1, @w[0], 0, n - 1);
  mx := 0;
  i := 0;
  while i <= n - 1 do
    begin
      mx := mx + AbsReal(w[i]);
      inc(i);
    end;
  mx := mx / n;
  i := 0;
  while i <= M - 1 do
    begin
      y2[n + i] := 0;
      W2[n + i] := mx;
      inc(i);
    end;

  //
  // fill constraints matrix
  //
  i := 0;
  while i <= k - 1 do
    begin

      //
      // prepare Ith row
      // use Tmp for basis function values,
      // TmpDiff for basos function derivatives
      //
      j := 0;
      while j <= M - 1 do
        begin
          if j = 0 then
            begin
              tmp[j] := 1;
              TmpDiff[j] := 0;
            end
          else
            begin
              if j = 1 then
                begin
                  tmp[j] := XC[i];
                  TmpDiff[j] := 1;
                end
              else
                begin
                  tmp[j] := 2 * XC[i] * tmp[j - 1] - tmp[j - 2];
                  TmpDiff[j] := 2 * (tmp[j - 1] + XC[i] * TmpDiff[j - 1]) - TmpDiff[j - 2];
                end;
            end;
          inc(j);
        end;
      if DC[i] = 0 then
        begin
          APVMove(@CMatrix[i][0], 0, M - 1, @tmp[0], 0, M - 1);
        end;
      if DC[i] = 1 then
        begin
          APVMove(@CMatrix[i][0], 0, M - 1, @TmpDiff[0], 0, M - 1);
        end;
      CMatrix[i, M] := YC[i];
      inc(i);
    end;

  //
  // Solve constrained task
  //
  if k > 0 then
    begin

      //
      // solve using regularization
      //
      LSFitLinearWC(y2, W2, FMatrix, CMatrix, n + M, M, k, Info, tmp, LRep);
    end
  else
    begin

      //
      // no constraints, no regularization needed
      //
      LSFitLinearWC(y, w, FMatrix, CMatrix, n, M, 0, Info, tmp, LRep);
    end;
  if Info < 0 then
    begin
      Exit;
    end;

  //
  // Generate barycentric model and scale it
  // * BX, BY store barycentric model nodes
  // * FMatrix is reused (remember - it is at least MxM, what we need)
  //
  // Model intialization is done in O(M^2). In principle, it can be
  // done in O(M*log(M)), but before it we solved task with O(N*M^2)
  // complexity, so it is only a small amount of total time spent.
  //
  SetLength(Bx, M);
  SetLength(By, M);
  SetLength(BW, M);
  SetLength(TMP2, M);
  s := 1;
  i := 0;
  while i <= M - 1 do
    begin
      if M <> 1 then
        begin
          u := Cos(pi * i / (M - 1));
        end
      else
        begin
          u := 0;
        end;
      v := 0;
      j := 0;
      while j <= M - 1 do
        begin
          if j = 0 then
            begin
              TMP2[j] := 1;
            end
          else
            begin
              if j = 1 then
                begin
                  TMP2[j] := u;
                end
              else
                begin
                  TMP2[j] := 2 * u * TMP2[j - 1] - TMP2[j - 2];
                end;
            end;
          v := v + tmp[j] * TMP2[j];
          inc(j);
        end;
      Bx[i] := u;
      By[i] := v;
      BW[i] := s;
      if (i = 0) or (i = M - 1) then
        begin
          BW[i] := 0.5 * BW[i];
        end;
      s := -s;
      inc(i);
    end;
  BarycentricBuildXYW(Bx, By, BW, M, p);
  BarycentricLinTransX(p, 2 / (XB - XA), -(XA + XB) / (XB - XA));
  BarycentricLinTransY(p, SB - SA, SA);

  //
  // Scale absolute errors obtained from LSFitLinearW.
  // Relative error should be calculated separately
  // (because of shifting/scaling of the task)
  //
  Rep.TaskRCond := LRep.TaskRCond;
  Rep.RMSError := LRep.RMSError * (SB - SA);
  Rep.AvgError := LRep.AvgError * (SB - SA);
  Rep.MaxError := LRep.MaxError * (SB - SA);
  Rep.AvgRelError := 0;
  RelCnt := 0;
  i := 0;
  while i <= n - 1 do
    begin
      if AP_FP_NEq(YOriginal[i], 0) then
        begin
          Rep.AvgRelError := Rep.AvgRelError + AbsReal(BarycentricCalc(p, XOriginal[i]) - YOriginal[i]) / AbsReal(YOriginal[i]);
          RelCnt := RelCnt + 1;
        end;
      inc(i);
    end;
  if RelCnt <> 0 then
    begin
      Rep.AvgRelError := Rep.AvgRelError / RelCnt;
    end;
end;

const
  Spline1DVNum = 11;

procedure Spline1DFitInternal(st: TLInt; x: TLVec; y: TLVec; w: TLVec; n: TLInt; XC: TLVec; YC: TLVec;
  DC: TLIVec; k: TLInt; M: TLInt;
  var Info: TLInt; var s: TSpline1DInterpolant; var Rep: TSpline1DFitReport); forward;
procedure HeapSortPoints(var x: TLVec; var y: TLVec; n: TLInt); forward;
procedure HeapSortDPoints(var x: TLVec; var y: TLVec; var d: TLVec; n: TLInt); forward;
procedure SolveTridiagonal(a: TLVec; b: TLVec; c: TLVec; d: TLVec; n: TLInt; var x: TLVec); forward;
procedure SolveCyclicTridiagonal(a: TLVec; b: TLVec; c: TLVec; d: TLVec; n: TLInt; var x: TLVec); forward;
function DiffThreePoint(t: TLFloat; x0: TLFloat; F0: TLFloat; x1: TLFloat; f1: TLFloat; x2: TLFloat; f2: TLFloat): TLFloat; forward;

(* ************************************************************************
  This subroutine builds linear spline interpolant

  INPUT PARAMETERS:
  X   -   spline nodes, array[0..N-1]
  Y   -   function values, array[0..N-1]
  N   -   points count, N>=2

  OUTPUT PARAMETERS:
  C   -   spline interpolant

  ORDER OF POINTS
  Subroutine automatically sorts points, so caller may pass unsorted array.
  ************************************************************************ *)
procedure Spline1DBuildLinear(x, y: TLVec; n: TLInt; var c: TSpline1DInterpolant);
var
  i: TLInt;
begin
  x := DynamicArrayCopy(x);
  y := DynamicArrayCopy(y);
  Assert(n > 1, 'Spline1DBuildLinear: N<2!');

  //
  // Sort points
  //
  HeapSortPoints(x, y, n);

  //
  // Build
  //
  c.Periodic := False;
  c.n := n;
  c.k := 3;
  SetLength(c.x, n);
  SetLength(c.c, 4 * (n - 1));
  i := 0;
  while i <= n - 1 do
    begin
      c.x[i] := x[i];
      inc(i);
    end;
  i := 0;
  while i <= n - 2 do
    begin
      c.c[4 * i + 0] := y[i];
      c.c[4 * i + 1] := (y[i + 1] - y[i]) / (x[i + 1] - x[i]);
      c.c[4 * i + 2] := 0;
      c.c[4 * i + 3] := 0;
      inc(i);
    end;
end;

(* ************************************************************************
  This subroutine builds cubic spline interpolant.

  INPUT PARAMETERS:
  X           -   spline nodes, array[0..N-1].
  Y           -   function values, array[0..N-1].
  N           -   points count, N>=2
  BoundLType  -   boundary condition type for the left boundary
  BoundL      -   left boundary condition (first or second derivative, depending on the BoundLType)
  BoundRType  -   boundary condition type for the right boundary
  BoundR      -   right boundary condition (first or second derivative, depending on the BoundRType)

  OUTPUT PARAMETERS:
  C           -   spline interpolant

  ORDER OF POINTS

  Subroutine automatically sorts points, so caller may pass unsorted array.

  SETTING BOUNDARY VALUES:

  The BoundLType/BoundRType parameters can have the following values:
  * -1, which corresonds to the periodic (cyclic) boundary conditions.
  In this case:
  * both BoundLType and BoundRType must be equal to -1.
  * BoundL/BoundR are ignored
  * Y[last] is ignored (it is assumed to be equal to Y[first]).
  *  0, which corresponds to the parabolically terminated spline (BoundL and/or BoundR are ignored).
  *  1, which corresponds to the first derivative boundary condition
  *  2, which corresponds to the second derivative boundary condition

  PROBLEMS WITH PERIODIC BOUNDARY CONDITIONS:
  Problems with periodic boundary conditions have Y[first_point]=Y[last_point].
  However, this subroutine doesn't require you to specify equal  values  for
  the first and last points - it automatically forces them to be equal.
  ************************************************************************ *)
procedure Spline1DBuildCubic(x, y: TLVec; n: TLInt; BoundLType: TLInt; BoundL: TLFloat; BoundRType: TLInt; BoundR: TLFloat;
  var c: TSpline1DInterpolant);
var
  a1: TLVec;
  a2: TLVec;
  a3: TLVec;
  b: TLVec;
  d: TLVec;
  dt: TLVec;
  i: TLInt;
  v: TLFloat;
begin
  x := DynamicArrayCopy(x);
  y := DynamicArrayCopy(y);
  Assert(n >= 2, 'Spline1DBuildCubic: N<2!');
  Assert((BoundLType = -1) or (BoundLType = 0) or (BoundLType = 1) or
    (BoundLType = 2), 'Spline1DBuildCubic: incorrect BoundLType!');
  Assert((BoundRType = -1) or (BoundRType = 0) or (BoundRType = 1) or
    (BoundRType = 2), 'Spline1DBuildCubic: incorrect BoundRType!');
  Assert((BoundRType = -1) and (BoundLType = -1) or (BoundRType <> -1) and
    (BoundLType <> -1), 'Spline1DBuildCubic: incorrect BoundLType/BoundRType!');

  //
  // Special cases:
  // * N=2, parabolic terminated boundary condition on both ends
  // * N=2, periodic boundary condition
  //
  if (n = 2) and (BoundLType = 0) and (BoundRType = 0) then
    begin

      //
      // Change task type
      //
      BoundLType := 2;
      BoundL := 0;
      BoundRType := 2;
      BoundR := 0;
    end;
  if (n = 2) and (BoundLType = -1) and (BoundRType = -1) then
    begin

      //
      // Change task type
      //
      BoundLType := 1;
      BoundL := 0;
      BoundRType := 1;
      BoundR := 0;
      y[1] := y[0];
    end;

  //
  // Periodic and non-periodic boundary conditions are
  // two separate classes
  //
  if (BoundRType = -1) and (BoundLType = -1) then
    begin

      //
      // Periodic boundary conditions
      //
      SetLength(a1, n - 1);
      SetLength(a2, n - 1);
      SetLength(a3, n - 1);
      SetLength(b, n - 1);

      //
      // Sort points.
      //
      HeapSortPoints(x, y, n);
      y[n - 1] := y[0];

      //
      // Boundary conditions at N-1 points
      // (one point less because last point is the same as first point).
      //
      a1[0] := x[1] - x[0];
      a2[0] := 2 * (x[1] - x[0] + x[n - 1] - x[n - 2]);
      a3[0] := x[n - 1] - x[n - 2];
      b[0] := 3 * (y[n - 1] - y[n - 2]) / (x[n - 1] - x[n - 2]) * (x[1] - x[0]) +
        3 * (y[1] - y[0]) / (x[1] - x[0]) * (x[n - 1] - x[n - 2]);
      i := 1;
      while i <= n - 2 do
        begin

          //
          // Altough last point is [N-2], we use X[N-1] and Y[N-1]
          // (because of periodicity)
          //
          a1[i] := x[i + 1] - x[i];
          a2[i] := 2 * (x[i + 1] - x[i - 1]);
          a3[i] := x[i] - x[i - 1];
          b[i] := 3 * (y[i] - y[i - 1]) / (x[i] - x[i - 1]) * (x[i + 1] - x[i]) + 3
            * (y[i + 1] - y[i]) / (x[i + 1] - x[i]) * (x[i] - x[i - 1]);
          inc(i);
        end;

      //
      // Solve, add last point (with index N-1)
      //
      SolveCyclicTridiagonal(a1, a2, a3, b, n - 1, dt);
      SetLength(d, n);
      APVMove(@d[0], 0, n - 2, @dt[0], 0, n - 2);
      d[n - 1] := d[0];

      //
      // Now problem is reduced to the cubic Hermite spline
      //
      Spline1DBuildHermite(x, y, d, n, c);
      c.Periodic := True;
    end
  else
    begin

      //
      // Non-periodic boundary condition
      //
      SetLength(a1, n);
      SetLength(a2, n);
      SetLength(a3, n);
      SetLength(b, n);

      //
      // Sort points.
      //
      HeapSortPoints(x, y, n);

      //
      // Left boundary conditions
      //
      if BoundLType = 0 then
        begin
          a1[0] := 0;
          a2[0] := 1;
          a3[0] := 1;
          b[0] := 2 * (y[1] - y[0]) / (x[1] - x[0]);
        end;
      if BoundLType = 1 then
        begin
          a1[0] := 0;
          a2[0] := 1;
          a3[0] := 0;
          b[0] := BoundL;
        end;
      if BoundLType = 2 then
        begin
          a1[0] := 0;
          a2[0] := 2;
          a3[0] := 1;
          b[0] := 3 * (y[1] - y[0]) / (x[1] - x[0]) - 0.5 * BoundL * (x[1] - x[0]);
        end;

      //
      // Central conditions
      //
      i := 1;
      while i <= n - 2 do
        begin
          a1[i] := x[i + 1] - x[i];
          a2[i] := 2 * (x[i + 1] - x[i - 1]);
          a3[i] := x[i] - x[i - 1];
          b[i] := 3 * (y[i] - y[i - 1]) / (x[i] - x[i - 1]) * (x[i + 1] - x[i]) + 3
            * (y[i + 1] - y[i]) / (x[i + 1] - x[i]) * (x[i] - x[i - 1]);
          inc(i);
        end;

      //
      // Right boundary conditions
      //
      if BoundRType = 0 then
        begin
          a1[n - 1] := 1;
          a2[n - 1] := 1;
          a3[n - 1] := 0;
          b[n - 1] := 2 * (y[n - 1] - y[n - 2]) / (x[n - 1] - x[n - 2]);
        end;
      if BoundRType = 1 then
        begin
          a1[n - 1] := 0;
          a2[n - 1] := 1;
          a3[n - 1] := 0;
          b[n - 1] := BoundR;
        end;
      if BoundRType = 2 then
        begin
          a1[n - 1] := 1;
          a2[n - 1] := 2;
          a3[n - 1] := 0;
          b[n - 1] := 3 * (y[n - 1] - y[n - 2]) / (x[n - 1] - x[n - 2]) + 0.5 *
            BoundR * (x[n - 1] - x[n - 2]);
        end;

      //
      // Solve
      //
      SolveTridiagonal(a1, a2, a3, b, n, d);

      //
      // Now problem is reduced to the cubic Hermite spline
      //
      Spline1DBuildHermite(x, y, d, n, c);
    end;
end;

(* ************************************************************************
  This subroutine builds Catmull-Rom spline interpolant.

  INPUT PARAMETERS:
  X           -   spline nodes, array[0..N-1].
  Y           -   function values, array[0..N-1].
  N           -   points count, N>=2

  BoundType   -   boundary condition type:
  * -1 for periodic boundary condition
  *  0 for parabolically terminated spline

  Tension     -   tension parameter:
  * tension=0   corresponds to classic Catmull-Rom spline
  * 0<tension<1 corresponds to more general form - cardinal spline

  OUTPUT PARAMETERS:
  C           -   spline interpolant

  ORDER OF POINTS

  Subroutine automatically sorts points, so caller may pass unsorted array.

  PROBLEMS WITH PERIODIC BOUNDARY CONDITIONS:

  Problems with periodic boundary conditions have Y[first_point]=Y[last_point].
  However, this subroutine doesn't require you to specify equal values for
  the first and last points - it automatically forces them to be equal.
  ************************************************************************ *)
procedure Spline1DBuildCatmullRom(x, y: TLVec; n: TLInt; BoundType: TLInt; Tension: TLFloat;
  var c: TSpline1DInterpolant);
var
  a1: TLVec;
  a2: TLVec;
  a3: TLVec;
  b: TLVec;
  d: TLVec;
  dt: TLVec;
  i: TLInt;
  v: TLFloat;
begin
  x := DynamicArrayCopy(x);
  y := DynamicArrayCopy(y);
  Assert(n >= 2, 'Spline1DBuildCatmullRom: N<2!');
  Assert((BoundType = -1) or (BoundType = 0),
    'Spline1DBuildCatmullRom: incorrect BoundType!');

  //
  // Special cases:
  // * N=2, parabolic terminated boundary condition on both ends
  // * N=2, periodic boundary condition
  //
  if (n = 2) and (BoundType = 0) then
    begin

      //
      // Just linear spline
      //
      Spline1DBuildLinear(x, y, n, c);
      Exit;
    end;
  if (n = 2) and (BoundType = -1) then
    begin

      //
      // Same as cubic spline with periodic conditions
      //
      Spline1DBuildCubic(x, y, n, -1, 0.0, -1, 0.0, c);
      Exit;
    end;

  //
  // Periodic or non-periodic boundary conditions
  //
  if BoundType = -1 then
    begin

      //
      // Sort points.
      //
      HeapSortPoints(x, y, n);
      y[n - 1] := y[0];

      //
      // Periodic boundary conditions
      //
      SetLength(d, n);
      d[0] := (y[1] - y[n - 2]) / (2 * (x[1] - x[0] + x[n - 1] - x[n - 2]));
      i := 1;
      while i <= n - 2 do
        begin
          d[i] := (1 - Tension) * (y[i + 1] - y[i - 1]) / (x[i + 1] - x[i - 1]);
          inc(i);
        end;
      d[n - 1] := d[0];

      //
      // Now problem is reduced to the cubic Hermite spline
      //
      Spline1DBuildHermite(x, y, d, n, c);
      c.Periodic := True;
    end
  else
    begin

      //
      // Sort points.
      //
      HeapSortPoints(x, y, n);

      //
      // Non-periodic boundary conditions
      //
      SetLength(d, n);
      i := 1;
      while i <= n - 2 do
        begin
          d[i] := (1 - Tension) * (y[i + 1] - y[i - 1]) / (x[i + 1] - x[i - 1]);
          inc(i);
        end;
      d[0] := 2 * (y[1] - y[0]) / (x[1] - x[0]) - d[1];
      d[n - 1] := 2 * (y[n - 1] - y[n - 2]) / (x[n - 1] - x[n - 2]) - d[n - 2];

      //
      // Now problem is reduced to the cubic Hermite spline
      //
      Spline1DBuildHermite(x, y, d, n, c);
    end;
end;

(* ************************************************************************
  This subroutine builds Hermite spline interpolant.

  INPUT PARAMETERS:
  X           -   spline nodes, array[0..N-1]
  Y           -   function values, array[0..N-1]
  D           -   derivatives, array[0..N-1]
  N           -   points count, N>=2

  OUTPUT PARAMETERS:
  C           -   spline interpolant.


  ORDER OF POINTS

  Subroutine automatically sorts points, so caller may pass unsorted array.
  ************************************************************************ *)
procedure Spline1DBuildHermite(x, y: TLVec; d: TLVec; n: TLInt; var c: TSpline1DInterpolant);
var
  i: TLInt;
  Delta: TLFloat;
  Delta2: TLFloat;
  Delta3: TLFloat;
begin
  x := DynamicArrayCopy(x);
  y := DynamicArrayCopy(y);
  d := DynamicArrayCopy(d);
  Assert(n >= 2, 'BuildHermiteSpline: N<2!');

  //
  // Sort points
  //
  HeapSortDPoints(x, y, d, n);

  //
  // Build
  //
  SetLength(c.x, n);
  SetLength(c.c, 4 * (n - 1));
  c.Periodic := False;
  c.k := 3;
  c.n := n;
  i := 0;
  while i <= n - 1 do
    begin
      c.x[i] := x[i];
      inc(i);
    end;
  i := 0;
  while i <= n - 2 do
    begin
      Delta := x[i + 1] - x[i];
      Delta2 := AP_Sqr(Delta);
      Delta3 := Delta * Delta2;
      c.c[4 * i + 0] := y[i];
      c.c[4 * i + 1] := d[i];
      c.c[4 * i + 2] := (3 * (y[i + 1] - y[i]) - 2 * d[i] * Delta - d[i + 1] *
        Delta) / Delta2;
      c.c[4 * i + 3] := (2 * (y[i] - y[i + 1]) + d[i] * Delta + d[i + 1] *
        Delta) / Delta3;
      inc(i);
    end;
end;

(* ************************************************************************
  This subroutine builds Akima spline interpolant

  INPUT PARAMETERS:
  X           -   spline nodes, array[0..N-1]
  Y           -   function values, array[0..N-1]
  N           -   points count, N>=5

  OUTPUT PARAMETERS:
  C           -   spline interpolant


  ORDER OF POINTS

  Subroutine automatically sorts points, so caller may pass unsorted array.
  ************************************************************************ *)
procedure Spline1DBuildAkima(x, y: TLVec; n: TLInt; var c: TSpline1DInterpolant);
var
  i: TLInt;
  d: TLVec;
  w: TLVec;
  Diff: TLVec;
begin
  x := DynamicArrayCopy(x);
  y := DynamicArrayCopy(y);
  Assert(n >= 5, 'BuildAkimaSpline: N<5!');

  //
  // Sort points
  //
  HeapSortPoints(x, y, n);

  //
  // Prepare W (weights), Diff (divided differences)
  //
  SetLength(w, n - 1);
  SetLength(Diff, n - 1);
  i := 0;
  while i <= n - 2 do
    begin
      Diff[i] := (y[i + 1] - y[i]) / (x[i + 1] - x[i]);
      inc(i);
    end;
  i := 1;
  while i <= n - 2 do
    begin
      w[i] := AbsReal(Diff[i] - Diff[i - 1]);
      inc(i);
    end;

  //
  // Prepare Hermite interpolation scheme
  //
  SetLength(d, n);
  i := 2;
  while i <= n - 3 do
    begin
      if AP_FP_NEq(AbsReal(w[i - 1]) + AbsReal(w[i + 1]), 0) then
        begin
          d[i] := (w[i + 1] * Diff[i - 1] + w[i - 1] * Diff[i]) /
            (w[i + 1] + w[i - 1]);
        end
      else
        begin
          d[i] := ((x[i + 1] - x[i]) * Diff[i - 1] + (x[i] - x[i - 1]) * Diff[i]) /
            (x[i + 1] - x[i - 1]);
        end;
      inc(i);
    end;
  d[0] := DiffThreePoint(x[0], x[0], y[0], x[1], y[1], x[2], y[2]);
  d[1] := DiffThreePoint(x[1], x[0], y[0], x[1], y[1], x[2], y[2]);
  d[n - 2] := DiffThreePoint(x[n - 2], x[n - 3], y[n - 3], x[n - 2], y[n - 2],
    x[n - 1], y[n - 1]);
  d[n - 1] := DiffThreePoint(x[n - 1], x[n - 3], y[n - 3], x[n - 2], y[n - 2],
    x[n - 1], y[n - 1]);

  //
  // Build Akima spline using Hermite interpolation scheme
  //
  Spline1DBuildHermite(x, y, d, n, c);
end;

(* ************************************************************************
  Weighted fitting by cubic spline, with constraints on function values or derivatives.

  Equidistant grid with M-2 nodes on [min(x,xc),max(x,xc)] is used to build basis functions.
  Basis functions are cubic splines with continuous second derivatives and non-fixed first derivatives at interval ends.
  Small regularizing term is used when solving constrained tasks (to improve stability).

  Task is linear, so linear least squares solver is used. Complexity of this computational scheme is O(N*M^2),
  mostly dominated by least squares solver

  SEE ALSO
  Spline1DFitHermiteWC()  -   fitting by Hermite splines (more flexible, less smooth)
  Spline1DFitCubic()      -   "lightweight" fitting  by  cubic  splines, without invididual weights and constraints

  INPUT PARAMETERS:
  X   -   points, array[0..N-1].
  Y   -   function values, array[0..N-1].

  W   -   weights, array[0..N-1]
  Each summand in square sum of approximation deviations from given values is multiplied by the square of corresponding weight.
  Fill it by 1's if you don't want to solve weighted task.

  N   -   number of points, N>0.
  XC  -   points where spline values/derivatives are constrained, array[0..K-1].
  YC  -   values of constraints, array[0..K-1]

  DC  -   array[0..K-1], types of constraints:
  * DC[i]=0   means that S(XC[i])=YC[i]
  * DC[i]=1   means that S'(XC[i])=YC[i]

  SEE BELOW FOR IMPORTANT INFORMATION ON CONSTRAINTS
  K   -   number of constraints, 0<=K<M. K=0 means no constraints (XC/YC/DC are not used in such cases)
  M   -   number of basis functions ( = number_of_nodes+2), M>=4.

  OUTPUT PARAMETERS:
  Info-   same format as in LSFitLinearWC() subroutine.
  * Info>0    task is solved
  * Info<=0   an error occured:
  -4 means inconvergence of internal SVD
  -3 means inconsistent constraints
  -1 means another errors in parameters passed (N<=0, for example)
  S   -   spline interpolant.
  Rep -   report, same format as in LSFitLinearWC() subroutine.
  Following fields are set:
  * RMSError      rms error on the (X,Y).
  * AvgError      average error on the (X,Y).
  * AvgRelError   average relative error on the non-zero Y
  * MaxError      maximum error
  NON-WEIGHTED ERRORS ARE CALCULATED

  IMPORTANT:
  this subroitine doesn't calculate task's condition number for K<>0.

  ORDER OF POINTS

  Subroutine automatically sorts points, so caller may pass unsorted array.

  SETTING CONSTRAINTS - DANGERS AND OPPORTUNITIES:

  Setting constraints can lead to undesired results, like ill-conditioned
  behavior, or inconsistency being detected. From the other side, it allows
  us to improve quality of the fit. Here we summarize our experience with
  constrained regression splines:
  * excessive constraints can be inconsistent. Splines are piecewise cubic
  functions, and it is easy to create an example, where large number of
  constraints concentrated in small area will result in inconsistency.
  Just because spline is not flexible enough to satisfy all of them. And
  same constraints spread across the [min(x),max(x)] will be perfectly
  consistent.
  * the more evenly constraints are spread across [min(x),max(x)], the more chances that they will be consistent
  * the greater is M (given fixed constraints), the more chances that constraints will be consistent
  * in the general case, consistency of constraints IS NOT GUARANTEED.
  * in the several special cases, however, we CAN guarantee consistency.
  * one of this cases is constraints on the function values AND/OR its derivatives at the interval boundaries.
  * another special case is ONE constraint on the function value (OR, but not AND, derivative) anywhere in the interval

  Our final recommendation is to use constraints WHEN AND ONLY WHEN you can't solve your task without them.
  Anything beyond special cases given above is not guaranteed and may result in inconsistency.
  ************************************************************************ *)
procedure Spline1DFitCubicWC(x, y, w: TLVec; n: TLInt; XC: TLVec; YC: TLVec; DC: TLIVec; k: TLInt; M: TLInt;
  var Info: TLInt; var s: TSpline1DInterpolant; var Rep: TSpline1DFitReport);
begin
  Spline1DFitInternal(0, x, y, w, n, XC, YC, DC, k, M, Info, s, Rep);
end;

(* ************************************************************************
  Weighted fitting by Hermite spline, with constraints on function values or first derivatives.

  Equidistant grid with M nodes on [min(x,xc),max(x,xc)] is used to build basis functions.
  Basis functions are Hermite splines. Small regularizing term is used when solving constrained tasks (to improve stability).

  Task is linear, so linear least squares solver is used. Complexity of this computational scheme is O(N*M^2),
  mostly dominated by least squares solver

  SEE ALSO
  Spline1DFitCubicWC()    -   fitting by Cubic splines (less flexible, more smooth)
  Spline1DFitHermite()    -   "lightweight" Hermite fitting, without invididual weights and constraints

  INPUT PARAMETERS:
  X   -   points, array[0..N-1].
  Y   -   function values, array[0..N-1].

  W   -   weights, array[0..N-1]
  Each summand in square sum of approximation deviations from given values is multiplied by the square of corresponding weight.
  Fill it by 1's if you don't want to solve weighted task.

  N   -   number of points, N>0.
  XC  -   points where spline values/derivatives are constrained, array[0..K-1].
  YC  -   values of constraints, array[0..K-1]

  DC  -   array[0..K-1], types of constraints:
  * DC[i]=0   means that S(XC[i])=YC[i]
  * DC[i]=1   means that S'(XC[i])=YC[i]

  SEE BELOW FOR IMPORTANT INFORMATION ON CONSTRAINTS
  K   -   number of constraints, 0<=K<M. K=0 means no constraints (XC/YC/DC are not used in such cases)
  M   -   number of basis functions (= 2 * number of nodes), M>=4, M IS EVEN!

  OUTPUT PARAMETERS:
  Info-   same format as in LSFitLinearW() subroutine:
  * Info>0    task is solved
  * Info<=0   an error occured:
  -4 means inconvergence of internal SVD
  -3 means inconsistent constraints
  -2 means odd M was passed (which is not supported)
  -1 means another errors in parameters passed (N<=0, for example)
  S   -   spline interpolant.
  Rep -   report, same format as in LSFitLinearW() subroutine.
  Following fields are set:
  * RMSError      rms error on the (X,Y).
  * AvgError      average error on the (X,Y).
  * AvgRelError   average relative error on the non-zero Y
  * MaxError      maximum error
  NON-WEIGHTED ERRORS ARE CALCULATED

  IMPORTANT:
  this subroitine doesn't calculate task's condition number for K<>0.

  ORDER OF POINTS

  Subroutine automatically sorts points, so caller may pass unsorted array.

  SETTING CONSTRAINTS - DANGERS AND OPPORTUNITIES:

  Setting constraints can lead to undesired results, like ill-conditioned
  behavior, or inconsistency being detected. From the other side, it allows
  us to improve quality of the fit. Here we summarize our experience with
  constrained regression splines:
  * excessive constraints can be inconsistent. Splines are piecewise cubic
  functions, and it is easy to create an example, where large number of
  constraints concentrated in small area will result in inconsistency.
  Just because spline is not flexible enough to satisfy all of them. And
  same constraints spread across the [min(x),max(x)] will be perfectly
  consistent.
  * the more evenly constraints are spread across [min(x),max(x)], the more chances that they will be consistent
  * the greater is M (given fixed constraints), the more chances that constraints will be consistent
  * in the general case, consistency of constraints IS NOT GUARANTEED.
  * in the several special cases, however, we CAN guarantee consistency.
  * one of this cases is constraints on the function values AND/OR its derivatives at the interval boundaries.
  * another special case is ONE constraint on the function value (OR, but not AND, derivative) anywhere in the interval

  Our final recommendation is to use constraints WHEN AND ONLY WHEN you can't solve your task without them.
  Anything beyond special cases given above is not guaranteed and may result in inconsistency.
  ************************************************************************ *)
procedure Spline1DFitHermiteWC(x, y, w: TLVec; n: TLInt; XC: TLVec; YC: TLVec; DC: TLIVec; k: TLInt; M: TLInt;
  var Info: TLInt; var s: TSpline1DInterpolant; var Rep: TSpline1DFitReport);
begin
  Spline1DFitInternal(1, x, y, w, n, XC, YC, DC, k, M, Info, s, Rep);
end;

(* ************************************************************************
  Least squares fitting by cubic spline.

  This subroutine is "lightweight" alternative for more TLComplex and feature-rich Spline1DFitCubicWC().
  See Spline1DFitCubicWC() for more information about subroutine parameters (we don't duplicate it here because of length)
  ************************************************************************ *)
procedure Spline1DFitCubic(x, y: TLVec; n: TLInt; M: TLInt;
  var Info: TLInt; var s: TSpline1DInterpolant; var Rep: TSpline1DFitReport);
var
  i: TLInt;
  w: TLVec;
  XC: TLVec;
  YC: TLVec;
  DC: TLIVec;
begin
  if n > 0 then
    begin
      SetLength(w, n);
      i := 0;
      while i <= n - 1 do
        begin
          w[i] := 1;
          inc(i);
        end;
    end;
  Spline1DFitCubicWC(x, y, w, n, XC, YC, DC, 0, M, Info, s, Rep);
end;

(* ************************************************************************
  Least squares fitting by Hermite spline.

  This subroutine is "lightweight" alternative for more TLComplex and feature-rich Spline1DFitHermiteWC().
  See Spline1DFitHermiteWC() description for more information about subroutine parameters (we don't duplicate it here because of length).
  ************************************************************************ *)
procedure Spline1DFitHermite(x, y: TLVec; n: TLInt; M: TLInt; var Info: TLInt;
  var s: TSpline1DInterpolant; var Rep: TSpline1DFitReport);
var
  i: TLInt;
  w: TLVec;
  XC: TLVec;
  YC: TLVec;
  DC: TLIVec;
begin
  if n > 0 then
    begin
      SetLength(w, n);
      i := 0;
      while i <= n - 1 do
        begin
          w[i] := 1;
          inc(i);
        end;
    end;
  Spline1DFitHermiteWC(x, y, w, n, XC, YC, DC, 0, M, Info, s, Rep);
end;

(* ************************************************************************
  This subroutine calculates the value of the spline at the given point X.

  INPUT PARAMETERS:
  C   -   spline interpolant
  X   -   point

  Result:
  S(x)
  ************************************************************************ *)
function Spline1DCalc(c: TSpline1DInterpolant; x: TLFloat): TLFloat;
var
  L: TLInt;
  r: TLInt;
  M: TLInt;
  t: TLFloat;
begin
  Assert(c.k = 3, 'Spline1DCalc: internal error');

  //
  // correct if periodic
  //
  if c.Periodic then
    begin
      APPeriodicMap(x, c.x[0], c.x[c.n - 1], t);
    end;

  //
  // Binary search in the [ x[0], ..., x[n-2] ] (x[n-1] is not included)
  //
  L := 0;
  r := c.n - 2 + 1;
  while L <> r - 1 do
    begin
      M := (L + r) div 2;
      if AP_FP_Greater_Eq(c.x[M], x) then
        begin
          r := M;
        end
      else
        begin
          L := M;
        end;
    end;

  //
  // Interpolation
  //
  x := x - c.x[L];
  M := 4 * L;
  Result := c.c[M] + x * (c.c[M + 1] + x * (c.c[M + 2] + x * c.c[M + 3]));
end;

(* ************************************************************************
  This subroutine differentiates the spline.

  INPUT PARAMETERS:
  C   -   spline interpolant.
  X   -   point

  Result:
  S   -   S(x)
  DS  -   S'(x)
  D2S -   S''(x)
  ************************************************************************ *)
procedure Spline1DDiff(c: TSpline1DInterpolant; x: TLFloat; var s: TLFloat; var DS: TLFloat; var D2S: TLFloat);
var
  L: TLInt;
  r: TLInt;
  M: TLInt;
  t: TLFloat;
begin
  Assert(c.k = 3, 'Spline1DCalc: internal error');

  //
  // correct if periodic
  //
  if c.Periodic then
    begin
      APPeriodicMap(x, c.x[0], c.x[c.n - 1], t);
    end;

  //
  // Binary search
  //
  L := 0;
  r := c.n - 2 + 1;
  while L <> r - 1 do
    begin
      M := (L + r) div 2;
      if AP_FP_Greater_Eq(c.x[M], x) then
        begin
          r := M;
        end
      else
        begin
          L := M;
        end;
    end;

  //
  // Differentiation
  //
  x := x - c.x[L];
  M := 4 * L;
  s := c.c[M] + x * (c.c[M + 1] + x * (c.c[M + 2] + x * c.c[M + 3]));
  DS := c.c[M + 1] + 2 * x * c.c[M + 2] + 3 * AP_Sqr(x) * c.c[M + 3];
  D2S := 2 * c.c[M + 2] + 6 * x * c.c[M + 3];
end;

(* ************************************************************************
  This subroutine makes the copy of the spline.

  INPUT PARAMETERS:
  C   -   spline interpolant.

  Result:
  CC  -   spline copy
  ************************************************************************ *)
procedure Spline1DCopy(c: TSpline1DInterpolant; var CC: TSpline1DInterpolant);
begin
  CC.Periodic := c.Periodic;
  CC.n := c.n;
  CC.k := c.k;
  SetLength(CC.x, CC.n);
  APVMove(@CC.x[0], 0, CC.n - 1, @c.x[0], 0, CC.n - 1);
  SetLength(CC.c, (CC.k + 1) * (CC.n - 1));
  APVMove(@CC.c[0], 0, (CC.k + 1) * (CC.n - 1) - 1, @c.c[0], 0,
    (CC.k + 1) * (CC.n - 1) - 1);
end;

(* ************************************************************************
  This subroutine unpacks the spline into the coefficients table.

  INPUT PARAMETERS:
  C   -   spline interpolant.
  X   -   point

  Result:
  Tbl -   coefficients table, unpacked format, array[0..N-2, 0..5].
  For I = 0...N-2:
  Tbl[I,0] = X[i]
  Tbl[I,1] = X[i+1]
  Tbl[I,2] = C0
  Tbl[I,3] = C1
  Tbl[I,4] = C2
  Tbl[I,5] = C3
  On [x[i], x[i+1]] spline is equals to:
  S(x) = C0 + C1*t + C2*t^2 + C3*t^3
  t = x-x[i]
  ************************************************************************ *)
procedure Spline1DUnpack(c: TSpline1DInterpolant; var n: TLInt; var Tbl: TLMatrix);
var
  i: TLInt;
  j: TLInt;
begin
  SetLength(Tbl, c.n - 2 + 1, 2 + c.k + 1);
  n := c.n;

  //
  // Fill
  //
  i := 0;
  while i <= n - 2 do
    begin
      Tbl[i, 0] := c.x[i];
      Tbl[i, 1] := c.x[i + 1];
      j := 0;
      while j <= c.k do
        begin
          Tbl[i, 2 + j] := c.c[(c.k + 1) * i + j];
          inc(j);
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  This subroutine performs linear transformation of the spline argument.

  INPUT PARAMETERS:
  C   -   spline interpolant.
  A, B-   transformation coefficients: x = A*t + B

  Result:
  C   -   transformed spline
  ************************************************************************ *)
procedure Spline1DLinTransX(var c: TSpline1DInterpolant; a: TLFloat; b: TLFloat);
var
  i: TLInt;
  j: TLInt;
  n: TLInt;
  v: TLFloat;
  DV: TLFloat;
  D2V: TLFloat;
  x: TLVec;
  y: TLVec;
  d: TLVec;
begin
  n := c.n;

  //
  // Special case: A=0
  //
  if AP_FP_Eq(a, 0) then
    begin
      v := Spline1DCalc(c, b);
      i := 0;
      while i <= n - 2 do
        begin
          c.c[(c.k + 1) * i] := v;
          j := 1;
          while j <= c.k do
            begin
              c.c[(c.k + 1) * i + j] := 0;
              inc(j);
            end;
          inc(i);
        end;
      Exit;
    end;

  //
  // General case: A<>0.
  // Unpack, X, Y, dY/dX.
  // Scale and pack again.
  //
  Assert(c.k = 3, 'Spline1DLinTransX: internal error');
  SetLength(x, n);
  SetLength(y, n);
  SetLength(d, n);
  i := 0;
  while i <= n - 1 do
    begin
      x[i] := c.x[i];
      Spline1DDiff(c, x[i], v, DV, D2V);
      x[i] := (x[i] - b) / a;
      y[i] := v;
      d[i] := a * DV;
      inc(i);
    end;
  Spline1DBuildHermite(x, y, d, n, c);
end;

(* ************************************************************************
  This subroutine performs linear transformation of the spline.

  INPUT PARAMETERS:
  C   -   spline interpolant.
  A, B-   transformation coefficients: S2(x) = A*S(x) + B

  Result:
  C   -   transformed spline
  ************************************************************************ *)
procedure Spline1DLinTransY(var c: TSpline1DInterpolant; a: TLFloat; b: TLFloat);
var
  i: TLInt;
  j: TLInt;
  n: TLInt;
begin
  n := c.n;
  i := 0;
  while i <= n - 2 do
    begin
      c.c[(c.k + 1) * i] := a * c.c[(c.k + 1) * i] + b;
      j := 1;
      while j <= c.k do
        begin
          c.c[(c.k + 1) * i + j] := a * c.c[(c.k + 1) * i + j];
          inc(j);
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  This subroutine integrates the spline.

  INPUT PARAMETERS:
  C   -   spline interpolant.
  X   -   right bound of the integration interval [a, x],
  here 'a' denotes min(x[])

  Result:
  integral(S(t)dt,a,x)
  ************************************************************************ *)
function Spline1DIntegrate(c: TSpline1DInterpolant; x: TLFloat): TLFloat;
var
  n: TLInt;
  i: TLInt;
  j: TLInt;
  L: TLInt;
  r: TLInt;
  M: TLInt;
  w: TLFloat;
  v: TLFloat;
  t: TLFloat;
  IntAB: TLFloat;
  AdditionalTerm: TLFloat;
begin
  n := c.n;

  //
  // Periodic splines require special treatment. We make
  // following transformation:
  //
  // integral(S(t)dt,A,X) = integral(S(t)dt,A,Z)+AdditionalTerm
  //
  // here X may lie outside of [A,B], Z lies strictly in [A,B],
  // AdditionalTerm is equals to integral(S(t)dt,A,B) times some
  // integer number (may be zero).
  //
  if c.Periodic and (AP_FP_Less(x, c.x[0]) or AP_FP_Greater(x, c.x[c.n - 1]))
  then
    begin

      //
      // compute integral(S(x)dx,A,B)
      //
      IntAB := 0;
      i := 0;
      while i <= c.n - 2 do
        begin
          w := c.x[i + 1] - c.x[i];
          M := (c.k + 1) * i;
          IntAB := IntAB + c.c[M] * w;
          v := w;
          j := 1;
          while j <= c.k do
            begin
              v := v * w;
              IntAB := IntAB + c.c[M + j] * v / (j + 1);
              inc(j);
            end;
          inc(i);
        end;

      //
      // map X into [A,B]
      //
      APPeriodicMap(x, c.x[0], c.x[c.n - 1], t);
      AdditionalTerm := t * IntAB;
    end
  else
    begin
      AdditionalTerm := 0;
    end;

  //
  // Binary search in the [ x[0], ..., x[n-2] ] (x[n-1] is not included)
  //
  L := 0;
  r := n - 2 + 1;
  while L <> r - 1 do
    begin
      M := (L + r) div 2;
      if AP_FP_Greater_Eq(c.x[M], x) then
        begin
          r := M;
        end
      else
        begin
          L := M;
        end;
    end;

  //
  // Integration
  //
  Result := 0;
  i := 0;
  while i <= L - 1 do
    begin
      w := c.x[i + 1] - c.x[i];
      M := (c.k + 1) * i;
      Result := Result + c.c[M] * w;
      v := w;
      j := 1;
      while j <= c.k do
        begin
          v := v * w;
          Result := Result + c.c[M + j] * v / (j + 1);
          inc(j);
        end;
      inc(i);
    end;
  w := x - c.x[L];
  M := (c.k + 1) * L;
  v := w;
  Result := Result + c.c[M] * w;
  j := 1;
  while j <= c.k do
    begin
      v := v * w;
      Result := Result + c.c[M + j] * v / (j + 1);
      inc(j);
    end;
  Result := Result + AdditionalTerm;
end;

(* ************************************************************************
  Internal spline fitting subroutine
  ************************************************************************ *)
procedure Spline1DFitInternal(st: TLInt; x: TLVec; y: TLVec; w: TLVec; n: TLInt; XC: TLVec; YC: TLVec;
  DC: TLIVec; k: TLInt; M: TLInt;
  var Info: TLInt; var s: TSpline1DInterpolant; var Rep: TSpline1DFitReport);
var
  FMatrix: TLMatrix;
  CMatrix: TLMatrix;
  y2: TLVec;
  W2: TLVec;
  SX: TLVec;
  SY: TLVec;
  sD: TLVec;
  tmp: TLVec;
  XOriginal: TLVec;
  YOriginal: TLVec;
  LRep: TLSFitReport;
  v0: TLFloat;
  v1: TLFloat;
  v2: TLFloat;
  mx: TLFloat;
  s2: TSpline1DInterpolant;
  i: TLInt;
  j: TLInt;
  RelCnt: TLInt;
  XA: TLFloat;
  XB: TLFloat;
  SA: TLFloat;
  SB: TLFloat;
  BL: TLFloat;
  BR: TLFloat;
  Decay: TLFloat;
begin
  x := DynamicArrayCopy(x);
  y := DynamicArrayCopy(y);
  XC := DynamicArrayCopy(XC);
  YC := DynamicArrayCopy(YC);
  Assert((st = 0) or (st = 1), 'Spline1DFit: internal error!');
  if (st = 0) and (M < 4) then
    begin
      Info := -1;
      Exit;
    end;
  if (st = 1) and (M < 4) then
    begin
      Info := -1;
      Exit;
    end;
  if (n < 1) or (k < 0) or (k >= M) then
    begin
      Info := -1;
      Exit;
    end;
  i := 0;
  while i <= k - 1 do
    begin
      Info := 0;
      if DC[i] < 0 then
        begin
          Info := -1;
        end;
      if DC[i] > 1 then
        begin
          Info := -1;
        end;
      if Info < 0 then
        begin
          Exit;
        end;
      inc(i);
    end;
  if (st = 1) and (M mod 2 <> 0) then
    begin

      //
      // Hermite fitter must have even number of basis functions
      //
      Info := -2;
      Exit;
    end;

  //
  // weight decay for correct handling of task which becomes
  // degenerate after constraints are applied
  //
  Decay := 10000 * MachineEpsilon;

  //
  // Scale X, Y, XC, YC
  //
  LSFitScaleXY(x, y, n, XC, YC, DC, k, XA, XB, SA, SB, XOriginal, YOriginal);

  //
  // allocate space, initialize:
  // * SX     -   grid for basis functions
  // * SY     -   values of basis functions at grid points
  // * FMatrix-   values of basis functions at X[]
  // * CMatrix-   values (derivatives) of basis functions at XC[]
  //
  SetLength(y2, n + M);
  SetLength(W2, n + M);
  SetLength(FMatrix, n + M, M);
  if k > 0 then
    begin
      SetLength(CMatrix, k, M + 1);
    end;
  if st = 0 then
    begin

      //
      // allocate space for cubic spline
      //
      SetLength(SX, M - 2);
      SetLength(SY, M - 2);
      j := 0;
      while j <= M - 2 - 1 do
        begin
          SX[j] := AP_Float(2 * j) / (M - 2 - 1) - 1;
          inc(j);
        end;
    end;
  if st = 1 then
    begin

      //
      // allocate space for Hermite spline
      //
      SetLength(SX, M div 2);
      SetLength(SY, M div 2);
      SetLength(sD, M div 2);
      j := 0;
      while j <= M div 2 - 1 do
        begin
          SX[j] := AP_Float(2 * j) / (M div 2 - 1) - 1;
          inc(j);
        end;
    end;

  //
  // Prepare design and constraints matrices:
  // * fill constraints matrix
  // * fill first N rows of design matrix with values
  // * fill next M rows of design matrix with regularizing term
  // * append M zeros to Y
  // * append M elements, mean(abs(W)) each, to W
  //
  j := 0;
  while j <= M - 1 do
    begin

      //
      // prepare Jth basis function
      //
      if st = 0 then
        begin

          //
          // cubic spline basis
          //
          i := 0;
          while i <= M - 2 - 1 do
            begin
              SY[i] := 0;
              inc(i);
            end;
          BL := 0;
          BR := 0;
          if j < M - 2 then
            begin
              SY[j] := 1;
            end;
          if j = M - 2 then
            begin
              BL := 1;
            end;
          if j = M - 1 then
            begin
              BR := 1;
            end;
          Spline1DBuildCubic(SX, SY, M - 2, 1, BL, 1, BR, s2);
        end;
      if st = 1 then
        begin

          //
          // Hermite basis
          //
          i := 0;
          while i <= M div 2 - 1 do
            begin
              SY[i] := 0;
              sD[i] := 0;
              inc(i);
            end;
          if j mod 2 = 0 then
            begin
              SY[j div 2] := 1;
            end
          else
            begin
              sD[j div 2] := 1;
            end;
          Spline1DBuildHermite(SX, SY, sD, M div 2, s2);
        end;

      //
      // values at X[], XC[]
      //
      i := 0;
      while i <= n - 1 do
        begin
          FMatrix[i, j] := Spline1DCalc(s2, x[i]);
          inc(i);
        end;
      i := 0;
      while i <= k - 1 do
        begin
          Assert((DC[i] >= 0) and (DC[i] <= 2), 'Spline1DFit: internal error!');
          Spline1DDiff(s2, XC[i], v0, v1, v2);
          if DC[i] = 0 then
            begin
              CMatrix[i, j] := v0;
            end;
          if DC[i] = 1 then
            begin
              CMatrix[i, j] := v1;
            end;
          if DC[i] = 2 then
            begin
              CMatrix[i, j] := v2;
            end;
          inc(i);
        end;
      inc(j);
    end;
  i := 0;
  while i <= k - 1 do
    begin
      CMatrix[i, M] := YC[i];
      inc(i);
    end;
  i := 0;
  while i <= M - 1 do
    begin
      j := 0;
      while j <= M - 1 do
        begin
          if i = j then
            begin
              FMatrix[n + i, j] := Decay;
            end
          else
            begin
              FMatrix[n + i, j] := 0;
            end;
          inc(j);
        end;
      inc(i);
    end;
  SetLength(y2, n + M);
  SetLength(W2, n + M);
  APVMove(@y2[0], 0, n - 1, @y[0], 0, n - 1);
  APVMove(@W2[0], 0, n - 1, @w[0], 0, n - 1);
  mx := 0;
  i := 0;
  while i <= n - 1 do
    begin
      mx := mx + AbsReal(w[i]);
      inc(i);
    end;
  mx := mx / n;
  i := 0;
  while i <= M - 1 do
    begin
      y2[n + i] := 0;
      W2[n + i] := mx;
      inc(i);
    end;

  //
  // Solve constrained task
  //
  if k > 0 then
    begin

      //
      // solve using regularization
      //
      LSFitLinearWC(y2, W2, FMatrix, CMatrix, n + M, M, k, Info, tmp, LRep);
    end
  else
    begin

      //
      // no constraints, no regularization needed
      //
      LSFitLinearWC(y, w, FMatrix, CMatrix, n, M, k, Info, tmp, LRep);
    end;
  if Info < 0 then
    begin
      Exit;
    end;

  //
  // Generate spline and scale it
  //
  if st = 0 then
    begin

      //
      // cubic spline basis
      //
      APVMove(@SY[0], 0, M - 2 - 1, @tmp[0], 0, M - 2 - 1);
      Spline1DBuildCubic(SX, SY, M - 2, 1, tmp[M - 2], 1, tmp[M - 1], s);
    end;
  if st = 1 then
    begin

      //
      // Hermite basis
      //
      i := 0;
      while i <= M div 2 - 1 do
        begin
          SY[i] := tmp[2 * i];
          sD[i] := tmp[2 * i + 1];
          inc(i);
        end;
      Spline1DBuildHermite(SX, SY, sD, M div 2, s);
    end;
  Spline1DLinTransX(s, 2 / (XB - XA), -(XA + XB) / (XB - XA));
  Spline1DLinTransY(s, SB - SA, SA);

  //
  // Scale absolute errors obtained from LSFitLinearW.
  // Relative error should be calculated separately
  // (because of shifting/scaling of the task)
  //
  Rep.TaskRCond := LRep.TaskRCond;
  Rep.RMSError := LRep.RMSError * (SB - SA);
  Rep.AvgError := LRep.AvgError * (SB - SA);
  Rep.MaxError := LRep.MaxError * (SB - SA);
  Rep.AvgRelError := 0;
  RelCnt := 0;
  i := 0;
  while i <= n - 1 do
    begin
      if AP_FP_NEq(YOriginal[i], 0) then
        begin
          Rep.AvgRelError := Rep.AvgRelError + AbsReal(Spline1DCalc(s, XOriginal[i]) - YOriginal[i]) / AbsReal(YOriginal[i]);
          RelCnt := RelCnt + 1;
        end;
      inc(i);
    end;
  if RelCnt <> 0 then
    begin
      Rep.AvgRelError := Rep.AvgRelError / RelCnt;
    end;
end;

(* ************************************************************************
  Internal subroutine. Heap sort.
  ************************************************************************ *)
procedure HeapSortPoints(var x: TLVec; var y: TLVec; n: TLInt);
var
  i: TLInt;
  j: TLInt;
  k: TLInt;
  t: TLInt;
  tmp: TLFloat;
  IsAscending: Boolean;
  IsDescending: Boolean;
begin

  //
  // Test for already sorted set
  //
  IsAscending := True;
  IsDescending := True;
  i := 1;
  while i <= n - 1 do
    begin
      IsAscending := IsAscending and AP_FP_Greater(x[i], x[i - 1]);
      IsDescending := IsDescending and AP_FP_Less(x[i], x[i - 1]);
      inc(i);
    end;
  if IsAscending then
    begin
      Exit;
    end;
  if IsDescending then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          j := n - 1 - i;
          if j <= i then
            begin
              Break;
            end;
          tmp := x[i];
          x[i] := x[j];
          x[j] := tmp;
          tmp := y[i];
          y[i] := y[j];
          y[j] := tmp;
          inc(i);
        end;
      Exit;
    end;

  //
  // Special case: N=1
  //
  if n = 1 then
    begin
      Exit;
    end;

  //
  // General case
  //
  i := 2;
  repeat
    t := i;
    while t <> 1 do
      begin
        k := t div 2;
        if AP_FP_Greater_Eq(x[k - 1], x[t - 1]) then
          begin
            t := 1;
          end
        else
          begin
            tmp := x[k - 1];
            x[k - 1] := x[t - 1];
            x[t - 1] := tmp;
            tmp := y[k - 1];
            y[k - 1] := y[t - 1];
            y[t - 1] := tmp;
            t := k;
          end;
      end;
    i := i + 1;
  until not(i <= n);
  i := n - 1;
  repeat
    tmp := x[i];
    x[i] := x[0];
    x[0] := tmp;
    tmp := y[i];
    y[i] := y[0];
    y[0] := tmp;
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
                if AP_FP_Greater(x[k], x[k - 1]) then
                  begin
                    k := k + 1;
                  end;
              end;
            if AP_FP_Greater_Eq(x[t - 1], x[k - 1]) then
              begin
                t := 0;
              end
            else
              begin
                tmp := x[k - 1];
                x[k - 1] := x[t - 1];
                x[t - 1] := tmp;
                tmp := y[k - 1];
                y[k - 1] := y[t - 1];
                y[t - 1] := tmp;
                t := k;
              end;
          end;
      end;
    i := i - 1;
  until not(i >= 1);
end;

(* ************************************************************************
  Internal subroutine. Heap sort.
  ************************************************************************ *)
procedure HeapSortDPoints(var x: TLVec; var y: TLVec; var d: TLVec; n: TLInt);
var
  i: TLInt;
  j: TLInt;
  k: TLInt;
  t: TLInt;
  tmp: TLFloat;
  IsAscending: Boolean;
  IsDescending: Boolean;
begin

  //
  // Test for already sorted set
  //
  IsAscending := True;
  IsDescending := True;
  i := 1;
  while i <= n - 1 do
    begin
      IsAscending := IsAscending and AP_FP_Greater(x[i], x[i - 1]);
      IsDescending := IsDescending and AP_FP_Less(x[i], x[i - 1]);
      inc(i);
    end;
  if IsAscending then
    begin
      Exit;
    end;
  if IsDescending then
    begin
      i := 0;
      while i <= n - 1 do
        begin
          j := n - 1 - i;
          if j <= i then
            begin
              Break;
            end;
          tmp := x[i];
          x[i] := x[j];
          x[j] := tmp;
          tmp := y[i];
          y[i] := y[j];
          y[j] := tmp;
          tmp := d[i];
          d[i] := d[j];
          d[j] := tmp;
          inc(i);
        end;
      Exit;
    end;

  //
  // Special case: N=1
  //
  if n = 1 then
    begin
      Exit;
    end;

  //
  // General case
  //
  i := 2;
  repeat
    t := i;
    while t <> 1 do
      begin
        k := t div 2;
        if AP_FP_Greater_Eq(x[k - 1], x[t - 1]) then
          begin
            t := 1;
          end
        else
          begin
            tmp := x[k - 1];
            x[k - 1] := x[t - 1];
            x[t - 1] := tmp;
            tmp := y[k - 1];
            y[k - 1] := y[t - 1];
            y[t - 1] := tmp;
            tmp := d[k - 1];
            d[k - 1] := d[t - 1];
            d[t - 1] := tmp;
            t := k;
          end;
      end;
    i := i + 1;
  until not(i <= n);
  i := n - 1;
  repeat
    tmp := x[i];
    x[i] := x[0];
    x[0] := tmp;
    tmp := y[i];
    y[i] := y[0];
    y[0] := tmp;
    tmp := d[i];
    d[i] := d[0];
    d[0] := tmp;
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
                if AP_FP_Greater(x[k], x[k - 1]) then
                  begin
                    k := k + 1;
                  end;
              end;
            if AP_FP_Greater_Eq(x[t - 1], x[k - 1]) then
              begin
                t := 0;
              end
            else
              begin
                tmp := x[k - 1];
                x[k - 1] := x[t - 1];
                x[t - 1] := tmp;
                tmp := y[k - 1];
                y[k - 1] := y[t - 1];
                y[t - 1] := tmp;
                tmp := d[k - 1];
                d[k - 1] := d[t - 1];
                d[t - 1] := tmp;
                t := k;
              end;
          end;
      end;
    i := i - 1;
  until not(i >= 1);
end;

(* ************************************************************************
  Internal subroutine. Tridiagonal solver. Solves

  ( B[0] C[0]                      )
  ( A[1] B[1] C[1]                 )
  (      A[2] B[2] C[2]            )
  (            ..........          ) * X = D
  (            ..........          )
  (           A[N-2] B[N-2] C[N-2] )
  (                  A[N-1] B[N-1] )

  ************************************************************************ *)
procedure SolveTridiagonal(a: TLVec; b: TLVec; c: TLVec; d: TLVec; n: TLInt; var x: TLVec);
var
  k: TLInt;
  t: TLFloat;
begin
  a := DynamicArrayCopy(a);
  b := DynamicArrayCopy(b);
  c := DynamicArrayCopy(c);
  d := DynamicArrayCopy(d);
  SetLength(x, n);
  a[0] := 0;
  c[n - 1] := 0;
  k := 1;
  while k <= n - 1 do
    begin
      t := a[k] / b[k - 1];
      b[k] := b[k] - t * c[k - 1];
      d[k] := d[k] - t * d[k - 1];
      inc(k);
    end;
  x[n - 1] := d[n - 1] / b[n - 1];
  k := n - 2;
  while k >= 0 do
    begin
      x[k] := (d[k] - c[k] * x[k + 1]) / b[k];
      dec(k);
    end;
end;

(* ************************************************************************
  Internal subroutine. Cyclic tridiagonal solver. Solves

  ( B[0] C[0]                 A[0] )
  ( A[1] B[1] C[1]                 )
  (      A[2] B[2] C[2]            )
  (            ..........          ) * X = D
  (            ..........          )
  (           A[N-2] B[N-2] C[N-2] )
  ( C[N-1]           A[N-1] B[N-1] )
  ************************************************************************ *)
procedure SolveCyclicTridiagonal(a: TLVec; b: TLVec; c: TLVec; d: TLVec; n: TLInt; var x: TLVec);
var
  k: TLInt;
  t: TLFloat;
  alpha: TLFloat;
  beta: TLFloat;
  Gamma: TLFloat;
  y: TLVec;
  z: TLVec;
  u: TLVec;
begin
  b := DynamicArrayCopy(b);
  beta := a[0];
  alpha := c[n - 1];
  Gamma := -b[0];
  b[0] := 2 * b[0];
  b[n - 1] := b[n - 1] - alpha * beta / Gamma;
  SetLength(u, n);
  k := 0;
  while k <= n - 1 do
    begin
      u[k] := 0;
      inc(k);
    end;
  u[0] := Gamma;
  u[n - 1] := alpha;
  SolveTridiagonal(a, b, c, d, n, y);
  SolveTridiagonal(a, b, c, u, n, z);
  SetLength(x, n);
  k := 0;
  while k <= n - 1 do
    begin
      x[k] := y[k] - (y[0] + beta / Gamma * y[n - 1]) / (1 + z[0] + beta / Gamma * z[n - 1]) * z[k];
      inc(k);
    end;
end;

(* ************************************************************************
  Internal subroutine. Three-point differentiation
  ************************************************************************ *)
function DiffThreePoint(t: TLFloat; x0: TLFloat; F0: TLFloat; x1: TLFloat; f1: TLFloat; x2: TLFloat; f2: TLFloat): TLFloat;
var
  a: TLFloat;
  b: TLFloat;
begin
  t := t - x0;
  x1 := x1 - x0;
  x2 := x2 - x0;
  a := (f2 - F0 - x2 / x1 * (f1 - F0)) / (AP_Sqr(x2) - x1 * x2);
  b := (f1 - F0 - a * AP_Sqr(x1)) / x1;
  Result := 2 * a * t + b;
end;
