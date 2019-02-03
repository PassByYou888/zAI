{ ****************************************************************************** }
{ * Limited memory BFGS optimizer support, by QQ 600585@qq.com                 * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
type
  TMinLBFGSState = record
    n: TLInt;
    M: TLInt;
    EpsG: TLFloat;
    EpsF: TLFloat;
    EpsX: TLFloat;
    MAXITS: TLInt;
    Flags: TLInt;
    XRep: Boolean;
    StpMax: TLFloat;
    NFEV: TLInt;
    MCStage: TLInt;
    k: TLInt;
    q: TLInt;
    p: TLInt;
    Rho: TLVec;
    y: TLMatrix;
    s: TLMatrix;
    Theta: TLVec;
    d: TLVec;
    Stp: TLFloat;
    Work: TLVec;
    FOld: TLFloat;
    GammaK: TLFloat;
    x: TLVec;
    f: TLFloat;
    g: TLVec;
    NeedFG: Boolean;
    XUpdated: Boolean;
    RState: TLearnCommState;
    RepIterationsCount: TLInt;
    RepNFEV: TLInt;
    RepTerminationType: TLInt;
    LState: TLinMinState;
  end;

  TMinLBFGSReport = record
    IterationsCount: TLInt;
    NFEV: TLInt;
    TerminationType: TLInt;
  end;

procedure MinLBFGSCreate(n: TLInt; M: TLInt; const x: TLVec; var State: TMinLBFGSState); forward;
procedure MinLBFGSSetCond(var State: TMinLBFGSState; EpsG: TLFloat; EpsF: TLFloat; EpsX: TLFloat; MAXITS: TLInt); forward;
procedure MinLBFGSSetXRep(var State: TMinLBFGSState; NeedXRep: Boolean); forward;
procedure MinLBFGSSetStpMax(var State: TMinLBFGSState; StpMax: TLFloat); forward;
procedure MinLBFGSCreateX(n: TLInt; M: TLInt; const x: TLVec; Flags: TLInt; var State: TMinLBFGSState); forward;
function MinLBFGSIteration(var State: TMinLBFGSState): Boolean; forward;
procedure MinLBFGSResults(const State: TMinLBFGSState; var x: TLVec; var Rep: TMinLBFGSReport); forward;
procedure MinLBFGSFree(var x: TLVec; var State: TMinLBFGSState); forward;

procedure ClearRequestFields(var State: TMinLBFGSState); forward;


(* ************************************************************************
  LIMITED MEMORY BFGS METHOD FOR LARGE SCALE OPTIMIZATION

  The subroutine minimizes function F(x) of N arguments by  using  a  quasi-
  Newton method (LBFGS scheme) which is optimized to use  a  minimum  amount
  of memory.

  The subroutine generates the approximation of an inverse Hessian matrix by
  using information about the last M steps of the algorithm  (instead of N).
  It lessens a required amount of memory from a value  of  order  N^2  to  a
  value of order 2*N*M.

  INPUT PARAMETERS:
  N       -   problem dimension. N>0
  M       -   number of corrections in the BFGS scheme of Hessian
  approximation update. Recommended value:  3<=M<=7. The smaller
  value causes worse convergence, the bigger will  not  cause  a
  considerably better convergence, but will cause a fall in  the
  performance. M<=N.
  X       -   initial solution approximation, array[0..N-1].

  OUTPUT PARAMETERS:
  State   -   structure used for reverse communication.

  This function  initializes  State   structure  with  default  optimization
  parameters (stopping conditions, step size, etc.). Use MinLBFGSSet??????()
  functions to tune optimization parameters.

  After   all   optimization   parameters   are   tuned,   you   should  use
  MinLBFGSIteration() function to advance algorithm iterations.

  NOTES:

  1. you may tune stopping conditions with MinLBFGSSetCond() function
  2. if target function contains exp() or other fast growing functions,  and
  optimization algorithm makes too large steps which leads  to  overflow,
  use MinLBFGSSetStpMax() function to bound algorithm's  steps.  However,
  L-BFGS rarely needs such a tuning.
  ************************************************************************ *)
procedure MinLBFGSCreate(n: TLInt; M: TLInt;
  const x: TLVec; var State: TMinLBFGSState);
begin
  MinLBFGSCreateX(n, M, x, 0, State);
end;

(* ************************************************************************
  This function sets stopping conditions for L-BFGS optimization algorithm.

  INPUT PARAMETERS:
  State   -   structure which stores algorithm state between calls and
  which is used for reverse communication. Must be initialized
  with MinLBFGSCreate()
  EpsG    -   >=0
  The  subroutine  finishes  its  work   if   the  condition
  ||G||<EpsG is satisfied, where ||.|| means Euclidian norm,
  G - gradient.
  EpsF    -   >=0
  The  subroutine  finishes  its work if on k+1-th iteration
  the  condition  |F(k+1)-F(k)|<=EpsF*max{|F(k)|,|F(k+1)|,1}
  is satisfied.
  EpsX    -   >=0
  The subroutine finishes its work if  on  k+1-th  iteration
  the condition |X(k+1)-X(k)| <= EpsX is fulfilled.
  MaxIts  -   maximum number of iterations. If MaxIts=0, the  number  of
  iterations is unlimited.

  Passing EpsG=0, EpsF=0, EpsX=0 and MaxIts=0 (simultaneously) will lead to
  automatic stopping criterion selection (small EpsX).
  ************************************************************************ *)
procedure MinLBFGSSetCond(var State: TMinLBFGSState; EpsG: TLFloat;
  EpsF: TLFloat; EpsX: TLFloat; MAXITS: TLInt);
begin
  Assert(AP_FP_Greater_Eq(EpsG, 0), 'MinLBFGSSetCond: negative EpsG!');
  Assert(AP_FP_Greater_Eq(EpsF, 0), 'MinLBFGSSetCond: negative EpsF!');
  Assert(AP_FP_Greater_Eq(EpsX, 0), 'MinLBFGSSetCond: negative EpsX!');
  Assert(MAXITS >= 0, 'MinLBFGSSetCond: negative MaxIts!');
  if AP_FP_Eq(EpsG, 0) and AP_FP_Eq(EpsF, 0) and AP_FP_Eq(EpsX, 0) and (MAXITS = 0) then
      EpsX := 1.0E-6;
  State.EpsG := EpsG;
  State.EpsF := EpsF;
  State.EpsX := EpsX;
  State.MAXITS := MAXITS;
end;

(* ************************************************************************
  This function turns on/off reporting.

  INPUT PARAMETERS:
  State   -   structure which stores algorithm state between calls and
  which is used for reverse communication. Must be
  initialized with MinLBFGSCreate()
  NeedXRep-   whether iteration reports are needed or not

  Usually algorithm returns  from  MinLBFGSIteration()  only when  it  needs
  function/gradient/ (which is indicated by NeedFG field. However, with this
  function we can let it  stop  after  each  iteration  (one  iteration  may
  include more than one function evaluation), which is indicated by XUpdated
  field.
  ************************************************************************ *)
procedure MinLBFGSSetXRep(var State: TMinLBFGSState; NeedXRep: Boolean);
begin
  State.XRep := NeedXRep;
end;

(* ************************************************************************
  This function sets maximum step length

  INPUT PARAMETERS:
  State   -   structure which stores algorithm state between calls and
  which is used for reverse communication. Must be
  initialized with MinLBFGSCreate()
  StpMax  -   maximum step length, >=0. Set StpMax to 0.0,  if you don't
  want to limit step length.

  Use this subroutine when you optimize target function which contains exp()
  or  other  fast  growing  functions,  and optimization algorithm makes too
  large  steps  which  leads  to overflow. This function allows us to reject
  steps  that  are  too  large  (and  therefore  expose  us  to the possible
  overflow) without actually calculating function value at the x+stp*d.
  ************************************************************************ *)
procedure MinLBFGSSetStpMax(var State: TMinLBFGSState; StpMax: TLFloat);
begin
  Assert(AP_FP_Greater_Eq(StpMax, 0), 'MinLBFGSSetStpMax: StpMax<0!');
  State.StpMax := StpMax;
end;

(* ************************************************************************
  Extended subroutine for internal use only.

  Accepts additional parameters:

  Flags - additional settings:
  * Flags = 0     means no additional settings
  * Flags = 1     "do not allocate memory". used when solving
  a many subsequent tasks with  same N/M  values.
  First  call MUST  be without this flag bit set,
  subsequent  calls   of   MinLBFGS   with   same
  TMinLBFGSState structure can set Flags to 1.
  ************************************************************************ *)
procedure MinLBFGSCreateX(n: TLInt; M: TLInt;
  const x: TLVec; Flags: TLInt; var State: TMinLBFGSState);
var
  AllocateMem: Boolean;
begin
  Assert(n >= 1, 'MinLBFGS: N too small!');
  Assert(M >= 1, 'MinLBFGS: M too small!');
  Assert(M <= n, 'MinLBFGS: M too large!');

  //
  // Initialize
  //
  State.n := n;
  State.M := M;
  State.Flags := Flags;
  AllocateMem := Flags mod 2 = 0;
  Flags := Flags div 2;
  if AllocateMem then
    begin
      SetLength(State.Rho, M  );
      SetLength(State.Theta, M  );
      SetLength(State.y, M  , n  );
      SetLength(State.s, M  , n  );
      SetLength(State.d, n  );
      SetLength(State.x, n  );
      SetLength(State.g, n  );
      SetLength(State.Work, n  );
    end;
  MinLBFGSSetCond(State, 0, 0, 0, 0);
  MinLBFGSSetXRep(State, False);
  MinLBFGSSetStpMax(State, 0);

  //
  // Prepare first run
  //
  State.k := 0;
  APVMove(@State.x[0], 0, n - 1, @x[0], 0, n - 1);
  SetLength(State.RState.IA, 6 + 1);
  SetLength(State.RState.RA, 4 + 1);
  State.RState.Stage := -1;
end;

(* ************************************************************************
  L-BFGS iterations

  Called after initialization with MinLBFGSCreate() function.

  INPUT PARAMETERS:
  State   -   structure which stores algorithm state between calls and
  which is used for reverse communication. Must be initialized
  with MinLBFGSCreate()

  RESULT:
  * if function returned False, iterative proces has converged.
  Use MinLBFGSResults() to obtain optimization results.
  * if subroutine returned True, then, depending on structure fields, we
  have one of the following situations


  === FUNC/GRAD REQUEST ===
  State.NeedFG is True => function value/gradient are needed.
  Caller should calculate function value State.F and gradient
  State.G[0..N-1] at State.X[0..N-1] and call MinLBFGSIteration() again.

  === NEW INTERATION IS REPORTED ===
  State.XUpdated is True => one more iteration was made.
  State.X contains current position, State.F contains function value at X.
  You can read info from these fields, but never modify  them  because  they
  contain the only copy of optimization algorithm state.


  One and only one of these fields (NeedFG, XUpdated) is true on return. New
  iterations are reported only when reports  are  explicitly  turned  on  by
  MinLBFGSSetXRep() function, so if you never called it, you can expect that
  NeedFG is always True.
  ************************************************************************ *)
function MinLBFGSIteration(var State: TMinLBFGSState): Boolean;
var
  n     : TLInt;
  M     : TLInt;
  MAXITS: TLInt;
  EpsF  : TLFloat;
  EpsG  : TLFloat;
  EpsX  : TLFloat;
  i     : TLInt;
  j     : TLInt;
  IC    : TLInt;
  MCINFO: TLInt;
  v     : TLFloat;
  VV    : TLFloat;
label
  lbl_0, lbl_1, lbl_4, lbl_6, lbl_8, lbl_2, lbl_9, lbl_3, lbl_10, lbl_7,
  lbl_rcomm;
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
      MAXITS := State.RState.IA[2];
      i := State.RState.IA[3];
      j := State.RState.IA[4];
      IC := State.RState.IA[5];
      MCINFO := State.RState.IA[6];
      EpsF := State.RState.RA[0];
      EpsG := State.RState.RA[1];
      EpsX := State.RState.RA[2];
      v := State.RState.RA[3];
      VV := State.RState.RA[4];
    end
  else
    begin
      n := -983;
      M := -989;
      MAXITS := -834;
      i := 900;
      j := -287;
      IC := 364;
      MCINFO := 214;
      EpsF := -338;
      EpsG := -686;
      EpsX := 912;
      v := 585;
      VV := 497;
    end;
  if State.RState.Stage = 0 then
    begin
      goto lbl_0;
    end;
  if State.RState.Stage = 1 then
    begin
      goto lbl_1;
    end;
  if State.RState.Stage = 2 then
    begin
      goto lbl_2;
    end;
  if State.RState.Stage = 3 then
    begin
      goto lbl_3;
    end;

  //
  // Routine body
  //

  //
  // Unload frequently used variables from State structure
  // (just for typing convinience)
  //
  n := State.n;
  M := State.M;
  EpsG := State.EpsG;
  EpsF := State.EpsF;
  EpsX := State.EpsX;
  MAXITS := State.MAXITS;
  State.RepTerminationType := 0;
  State.RepIterationsCount := 0;
  State.RepNFEV := 0;

  //
  // Calculate F/G at the initial point
  //
  ClearRequestFields(State);
  State.NeedFG := True;
  State.RState.Stage := 0;
  goto lbl_rcomm;
lbl_0:
  if not State.XRep then
    begin
      goto lbl_4;
    end;
  ClearRequestFields(State);
  State.XUpdated := True;
  State.RState.Stage := 1;
  goto lbl_rcomm;
lbl_1:
lbl_4:
  State.RepNFEV := 1;
  State.FOld := State.f;
  v := APVDotProduct(@State.g[0], 0, n - 1, @State.g[0], 0, n - 1);
  v := Sqrt(v);
  if AP_FP_Less_Eq(v, EpsG) then
    begin
      State.RepTerminationType := 4;
      Result := False;
      Exit;
    end;

  //
  // Choose initial step
  //
  if AP_FP_Eq(State.StpMax, 0) then
    begin
      State.Stp := Min(1.0 / v, 1);
    end
  else
    begin
      State.Stp := Min(1.0 / v, State.StpMax);
    end;
  APVMoveNeg(@State.d[0], 0, n - 1, @State.g[0], 0, n - 1);

  //
  // Main cycle
  //
lbl_6:
  if False then
    begin
      goto lbl_7;
    end;

  //
  // Main cycle: prepare to 1-D line search
  //
  State.p := State.k mod M;
  State.q := Min(State.k, M - 1);

  //
  // Store X[k], G[k]
  //
  APVMoveNeg(@State.s[State.p][0], 0, n - 1, @State.x[0], 0, n - 1);
  APVMoveNeg(@State.y[State.p][0], 0, n - 1, @State.g[0], 0, n - 1);

  //
  // Minimize F(x+alpha*d)
  // Calculate S[k], Y[k]
  //
  State.MCStage := 0;
  if State.k <> 0 then
    begin
      State.Stp := 1.0;
    end;
  LinMinNormalizeD(State.d, State.Stp, n);
  MCSRCH(n, State.x, State.f, State.g, State.d, State.Stp, State.StpMax, MCINFO,
    State.NFEV, State.Work, State.LState, State.MCStage);
lbl_8:
  if State.MCStage = 0 then
    begin
      goto lbl_9;
    end;
  ClearRequestFields(State);
  State.NeedFG := True;
  State.RState.Stage := 2;
  goto lbl_rcomm;
lbl_2:
  MCSRCH(n, State.x, State.f, State.g, State.d, State.Stp, State.StpMax, MCINFO,
    State.NFEV, State.Work, State.LState, State.MCStage);
  goto lbl_8;
lbl_9:
  if not State.XRep then
    begin
      goto lbl_10;
    end;

  //
  // report
  //
  ClearRequestFields(State);
  State.XUpdated := True;
  State.RState.Stage := 3;
  goto lbl_rcomm;
lbl_3:
lbl_10:
  State.RepNFEV := State.RepNFEV + State.NFEV;
  State.RepIterationsCount := State.RepIterationsCount + 1;
  APVAdd(@State.s[State.p][0], 0, n - 1, @State.x[0], 0, n - 1);
  APVAdd(@State.y[State.p][0], 0, n - 1, @State.g[0], 0, n - 1);

  //
  // Stopping conditions
  //
  if (State.RepIterationsCount >= MAXITS) and (MAXITS > 0) then
    begin

      //
      // Too many iterations
      //
      State.RepTerminationType := 5;
      Result := False;
      Exit;
    end;
  v := APVDotProduct(@State.g[0], 0, n - 1, @State.g[0], 0, n - 1);
  if AP_FP_Less_Eq(Sqrt(v), EpsG) then
    begin

      //
      // Gradient is small enough
      //
      State.RepTerminationType := 4;
      Result := False;
      Exit;
    end;
  if AP_FP_Less_Eq(State.FOld - State.f, EpsF * Max(AbsReal(State.FOld),
    Max(AbsReal(State.f), 1.0))) then
    begin

      //
      // F(k+1)-F(k) is small enough
      //
      State.RepTerminationType := 1;
      Result := False;
      Exit;
    end;
  v := APVDotProduct(@State.s[State.p][0], 0, n - 1, @State.s[State.p][0],
    0, n - 1);
  if AP_FP_Less_Eq(Sqrt(v), EpsX) then
    begin

      //
      // X(k+1)-X(k) is small enough
      //
      State.RepTerminationType := 2;
      Result := False;
      Exit;
    end;

  //
  // If Wolfe conditions are satisfied, we can update
  // limited memory model.
  //
  // However, if conditions are not satisfied (NFEV limit is met,
  // function is too wild, ...), we'll skip L-BFGS update
  //
  if MCINFO <> 1 then
    begin

      //
      // Skip update.
      //
      // In such cases we'll initialize search direction by
      // antigradient vector, because it  leads to more
      // transparent code with less number of special cases
      //
      State.FOld := State.f;
      APVMoveNeg(@State.d[0], 0, n - 1, @State.g[0], 0, n - 1);
    end
  else
    begin

      //
      // Calculate Rho[k], GammaK
      //
      v := APVDotProduct(@State.y[State.p][0], 0, n - 1, @State.s[State.p][0],
        0, n - 1);
      VV := APVDotProduct(@State.y[State.p][0], 0, n - 1, @State.y[State.p][0],
        0, n - 1);
      if AP_FP_Eq(v, 0) or AP_FP_Eq(VV, 0) then
        begin

          //
          // Rounding errors make further iterations impossible.
          //
          State.RepTerminationType := -2;
          Result := False;
          Exit;
        end;
      State.Rho[State.p] := 1 / v;
      State.GammaK := v / VV;

      //
      // Calculate d(k+1) = -H(k+1)*g(k+1)
      //
      // for I:=K downto K-Q do
      // V = s(i)^T * work(iteration:I)
      // theta(i) = V
      // work(iteration:I+1) = work(iteration:I) - V*Rho(i)*y(i)
      // work(last iteration) = H0*work(last iteration)
      // for I:=K-Q to K do
      // V = y(i)^T*work(iteration:I)
      // work(iteration:I+1) = work(iteration:I) +(-V+theta(i))*Rho(i)*s(i)
      //
      // NOW WORK CONTAINS d(k+1)
      //
      APVMove(@State.Work[0], 0, n - 1, @State.g[0], 0, n - 1);
      i := State.k;
      while i >= State.k - State.q do
        begin
          IC := i mod M;
          v := APVDotProduct(@State.s[IC][0], 0, n - 1, @State.Work[0], 0, n - 1);
          State.Theta[IC] := v;
          VV := v * State.Rho[IC];
          APVSub(@State.Work[0], 0, n - 1, @State.y[IC][0], 0, n - 1, VV);
          dec(i);
        end;
      v := State.GammaK;
      APVMul(@State.Work[0], 0, n - 1, v);
      i := State.k - State.q;
      while i <= State.k do
        begin
          IC := i mod M;
          v := APVDotProduct(@State.y[IC][0], 0, n - 1, @State.Work[0], 0, n - 1);
          VV := State.Rho[IC] * (-v + State.Theta[IC]);
          APVAdd(@State.Work[0], 0, n - 1, @State.s[IC][0], 0, n - 1, VV);
          inc(i);
        end;
      APVMoveNeg(@State.d[0], 0, n - 1, @State.Work[0], 0, n - 1);

      //
      // Next step
      //
      State.FOld := State.f;
      State.k := State.k + 1;
    end;
  goto lbl_6;
lbl_7:
  Result := False;
  Exit;

  //
  // Saving state
  //
lbl_rcomm:
  Result := True;
  State.RState.IA[0] := n;
  State.RState.IA[1] := M;
  State.RState.IA[2] := MAXITS;
  State.RState.IA[3] := i;
  State.RState.IA[4] := j;
  State.RState.IA[5] := IC;
  State.RState.IA[6] := MCINFO;
  State.RState.RA[0] := EpsF;
  State.RState.RA[1] := EpsG;
  State.RState.RA[2] := EpsX;
  State.RState.RA[3] := v;
  State.RState.RA[4] := VV;
end;

(* ************************************************************************
  L-BFGS algorithm results

  Called after MinLBFGSIteration() returned False.

  INPUT PARAMETERS:
  State   -   algorithm state (used by MinLBFGSIteration).

  OUTPUT PARAMETERS:
  X       -   array[0..N-1], solution
  Rep     -   optimization report:
  * Rep.TerminationType completetion code:
  * -2    rounding errors prevent further improvement.
  X contains best point found.
  * -1    incorrect parameters were specified
  *  1    relative function improvement is no more than
  EpsF.
  *  2    relative step is no more than EpsX.
  *  4    gradient norm is no more than EpsG
  *  5    MaxIts steps was taken
  *  7    stopping conditions are too stringent,
  further improvement is impossible
  * Rep.IterationsCount contains iterations count
  * NFEV countains number of function calculations
  ************************************************************************ *)
procedure MinLBFGSResults(const State: TMinLBFGSState; var x: TLVec;
  var Rep: TMinLBFGSReport);
begin
  SetLength(x, State.n  );
  APVMove(@x[0], 0, State.n - 1, @State.x[0], 0, State.n - 1);
  Rep.IterationsCount := State.RepIterationsCount;
  Rep.NFEV := State.RepNFEV;
  Rep.TerminationType := State.RepTerminationType;
end;

procedure MinLBFGSFree(var x: TLVec; var State: TMinLBFGSState);
begin
  SetLength(x, 0);

  SetLength(State.Rho, 0);
  SetLength(State.Theta, 0);
  SetLength(State.y, 0);
  SetLength(State.s, 0);
  SetLength(State.d, 0);
  SetLength(State.x, 0);
  SetLength(State.g, 0);
  SetLength(State.Work, 0);
  SetLength(State.RState.IA, 0);
  SetLength(State.RState.RA, 0);
end;

(* ************************************************************************
  Clears request fileds (to be sure that we don't forgot to clear something)
  ************************************************************************ *)
procedure ClearRequestFields(var State: TMinLBFGSState);
begin
  State.NeedFG := False;
  State.XUpdated := False;
end; 
 
 
