{ ****************************************************************************** }
{ * Training support, by QQ 600585@qq.com                                      * }
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
type
  (* ************************************************************************
    Training report:
    * NGrad     - number of gradient calculations
    * NHess     - number of Hessian calculations
    * NCholesky - number of Cholesky decompositions
    ************************************************************************ *)
  TMLPReport = record
    NGrad: TLInt;
    NHess: TLInt;
    NCholesky: TLInt;
  end;

  (* ************************************************************************
    Cross-validation estimates of generalization error
    ************************************************************************ *)
  TMLPCVReport = record
    RelClsError: TLFloat;
    AvgCE: TLFloat;
    RMSError: TLFloat;
    AvgError: TLFloat;
    AvgRelError: TLFloat;
  end;

procedure MLPTrainLM(var Network: TMultiLayerPerceptron; const xy: TLMatrix;
  NPoints: TLInt; Decay: TLFloat; Restarts: TLInt;
  var Info: TLInt; var Rep: TMLPReport); forward;

procedure MLPTrainLM_MT(var Network: TMultiLayerPerceptron; const xy: TLMatrix;
  NPoints: TLInt; Decay: TLFloat; Restarts: TLInt;
  var Info: TLInt; var Rep: TMLPReport); forward;

procedure MLPTrainLBFGS(var Network: TMultiLayerPerceptron;
  const xy: TLMatrix; NPoints: TLInt; Decay: TLFloat;
  Restarts: TLInt; WStep: TLFloat; MAXITS: TLInt;
  var Info: TLInt; var Rep: TMLPReport; IsTerminated: PBoolean;
  out EBest: TLFloat); forward;

procedure MLPTrainLBFGS_MT(var Network: TMultiLayerPerceptron;
  const xy: TLMatrix; NPoints: TLInt; Decay: TLFloat;
  Restarts: TLInt; WStep: TLFloat; MAXITS: TLInt;
  var Info: TLInt; var Rep: TMLPReport); forward;

procedure MLPTrainLBFGS_MT_Mod(var Network: TMultiLayerPerceptron;
  const xy: TLMatrix; NPoints: TLInt; Restarts: TLInt;
  WStep, Diameter: TLFloat; MAXITS: TLInt;
  var Info: TLInt; var Rep: TMLPReport); forward;

procedure MLPTrainMonteCarlo(var Network: TMultiLayerPerceptron; const xy: TLMatrix; NPoints: TLInt;
  const MainRestarts, SubRestarts: TLInt; const MinError: TLFloat;
  Diameter: TLFloat; var Info: TLInt; var Rep: TMLPReport); forward;

procedure MLPKFoldCVLBFGS(const Network: TMultiLayerPerceptron;
  const xy: TLMatrix; NPoints: TLInt; Decay: TLFloat;
  Restarts: TLInt; WStep: TLFloat; MAXITS: TLInt;
  FoldsCount: TLInt; var Info: TLInt; var Rep: TMLPReport;
  var CVRep: TMLPCVReport); forward;

procedure MLPKFoldCVLM(const Network: TMultiLayerPerceptron;
  const xy: TLMatrix; NPoints: TLInt; Decay: TLFloat;
  Restarts: TLInt; FoldsCount: TLInt; var Info: TLInt;
  var Rep: TMLPReport; var CVRep: TMLPCVReport); forward;

type
  TMLPProcessData = record
    TotalRestarts: TLInt;
    RestartsFinished: TLInt;
    IsTerminated: Boolean;
    EBest: TLFloat;
  end;

  PMLPProcessData = ^TMLPProcessData;

const
  MinDecay = 0.001;

procedure MLPKFoldCVGeneral(const n: TMultiLayerPerceptron;
  const xy: TLMatrix; NPoints: TLInt; Decay: TLFloat;
  Restarts: TLInt; FoldsCount: TLInt; LMAlgorithm: Boolean;
  WStep: TLFloat; MAXITS: TLInt; var Info: TLInt;
  var Rep: TMLPReport; var CVRep: TMLPCVReport); forward;
procedure MLPKFoldSplit(const xy: TLMatrix; NPoints: TLInt;
  NClasses: TLInt; FoldsCount: TLInt; StratifiedSplits: Boolean;
  var Folds: TLIVec); forward;

(* ************************************************************************
  //  Neural network training using modified Levenberg-Marquardt with exact
  //  Hessian calculation and regularization. Subroutine trains neural network
  //  with restarts from random positions.Algorithm is well suited for small
  //  and medium scale problems (hundreds of weights).
  //
  //  INPUT PARAMETERS:
  //  Network     -   neural network with initialized geometry
  //  XY          -   training set
  //  NPoints     -   training set size
  //  Decay       -   weight decay constant, >=0.001 Decay term 'Decay*||Weights||^2' is added to error function.
  //                  If you don't know what Decay to choose, use 0.001.
  //  Restarts    -   number of restarts from random position, >0.
  //                  If you don't know what Restarts to choose, use 2.
  //
  //  OUTPUT PARAMETERS:
  //  Network     -   trained neural network.
  //  Info        -   return code:
  //  * -9, if internal matrix inverse subroutine failed
  //  * -2, if there is a point with class number outside of [0..NOut-1].
  //  * -1, if wrong parameters specified (NPoints<0, Restarts<1).
  //  *  2, if task has been solved.
  //  Rep         -   training report
  ************************************************************************ *)
procedure MLPTrainLM(var Network: TMultiLayerPerceptron; const xy: TLMatrix;
  NPoints: TLInt; Decay: TLFloat; Restarts: TLInt;
  var Info: TLInt; var Rep: TMLPReport);
var
  NIn: TLInt;
  NOut: TLInt;
  WCount: TLInt;
  LMFTol: TLFloat;
  LMStepTol: TLFloat;
  i: TLInt;
  k: TLInt;
  v: TLFloat;
  E: TLFloat;
  ENew: TLFloat;
  XNorm2: TLFloat;
  StepNorm: TLFloat;
  g: TLVec;
  d: TLVec;
  h: TLMatrix;
  HMod: TLMatrix;
  z: TLMatrix;
  SPD: Boolean;
  Nu: TLFloat;
  LAMBDA: TLFloat;
  LambdaUp: TLFloat;
  LambdaDown: TLFloat;
  InternalRep: TMinLBFGSReport;
  State: TMinLBFGSState;
  x: TLVec;
  y: TLVec;
  WBase: TLVec;
  WDir: TLVec;
  WT: TLVec;
  Wx: TLVec;
  pass: TLInt;
  WBest: TLVec;
  EBest: TLFloat;
  InvInfo: TLInt;
  InvRep: TMatInvReport;
  SolverInfo: TLInt;
  SolverRep: TDenseSolverReport;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  LambdaUp := 10;
  LambdaDown := 0.3;
  LMFTol := 0.001;
  LMStepTol := 0.001;

  //
  // Test for inputs
  //
  if (NPoints <= 0) or (Restarts < 1) then
    begin
      Info := -1;
      Exit;
    end;
  if MLPIsSoftmax(Network) then
    begin
      i := 0;
      while i <= NPoints - 1 do
        begin
          if (Round(xy[i, NIn]) < 0) or (Round(xy[i, NIn]) >= NOut) then
            begin
              Info := -2;
              Exit;
            end;
          inc(i);
        end;
    end;
  Decay := Max(Decay, MinDecay);
  Info := 2;

  //
  // Initialize data
  //
  Rep.NGrad := 0;
  Rep.NHess := 0;
  Rep.NCholesky := 0;

  //
  // General case.
  // Prepare task and network. Allocate space.
  //
  MLPInitPreprocessor(Network, xy, NPoints);
  SetLength(g, WCount - 1 + 1);
  SetLength(h, WCount - 1 + 1, WCount - 1 + 1);
  SetLength(HMod, WCount - 1 + 1, WCount - 1 + 1);
  SetLength(WBase, WCount - 1 + 1);
  SetLength(WDir, WCount - 1 + 1);
  SetLength(WBest, WCount - 1 + 1);
  SetLength(WT, WCount - 1 + 1);
  SetLength(Wx, WCount - 1 + 1);
  EBest := MaxRealNumber;

  //
  // Multiple passes
  //
  pass := 1;
  while pass <= Restarts do
    begin

      //
      // Initialize weights
      //
      MLPRandomize(Network);

      //
      // First stage of the hybrid algorithm: LBFGS
      //
      APVMove(@WBase[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
      MinLBFGSCreate(WCount, Min(WCount, 5), WBase, State);
      MinLBFGSSetCond(State, 0, 0, 0, Max(25, WCount));
      while MinLBFGSIteration(State) do
        begin

          //
          // gradient
          //
          APVMove(@Network.Weights[0], 0, WCount - 1, @State.x[0], 0, WCount - 1);
          MLPGradBatch(Network, xy, NPoints, State.f, State.g);

          //
          // weight decay
          //
          v := APVDotProduct(@Network.Weights[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
          State.f := State.f + 0.5 * Decay * v;
          APVAdd(@State.g[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1, Decay);

          //
          // next iteration
          //
          Rep.NGrad := Rep.NGrad + 1;
        end;
      MinLBFGSResults(State, WBase, InternalRep);
      APVMove(@Network.Weights[0], 0, WCount - 1, @WBase[0], 0, WCount - 1);

      //
      // Second stage of the hybrid algorithm: LM
      //
      // Initialize H with identity matrix,
      // G with gradient,
      // E with regularized error.
      //
      MLPHessianBatch(Network, xy, NPoints, E, g, h);
      v := APVDotProduct(@Network.Weights[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
      E := E + 0.5 * Decay * v;
      APVAdd(@g[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1, Decay);
      k := 0;
      while k <= WCount - 1 do
        begin
          h[k, k] := h[k, k] + Decay;
          inc(k);
        end;
      Rep.NHess := Rep.NHess + 1;
      LAMBDA := 0.001;
      Nu := 2;
      while True do
        begin

          //
          // 1. HMod = H+lambda*I
          // 2. Try to solve (H+Lambda*I)*dx = -g.
          // Increase lambda if left part is not positive definite.
          //
          i := 0;
          while i <= WCount - 1 do
            begin
              APVMove(@HMod[i][0], 0, WCount - 1, @h[i][0], 0, WCount - 1);
              HMod[i, i] := HMod[i, i] + LAMBDA;
              inc(i);
            end;
          SPD := SPDMatrixCholesky(HMod, WCount, True);
          Rep.NCholesky := Rep.NCholesky + 1;
          if not SPD then
            begin
              LAMBDA := LAMBDA * LambdaUp * Nu;
              Nu := Nu * 2;
              Continue;
            end;
          SPDMatrixCholeskySolve(HMod, WCount, True, g, SolverInfo, SolverRep, WDir);
          if SolverInfo < 0 then
            begin
              LAMBDA := LAMBDA * LambdaUp * Nu;
              Nu := Nu * 2;
              Continue;
            end;
          APVMul(@WDir[0], 0, WCount - 1, -1);

          //
          // Lambda found.
          // 1. Save old w in WBase
          // 1. Test some stopping criterions
          // 2. If error(w+wdir)>error(w), increase lambda
          //
          APVAdd(@Network.Weights[0], 0, WCount - 1, @WDir[0], 0, WCount - 1);
          XNorm2 := APVDotProduct(@Network.Weights[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
          StepNorm := APVDotProduct(@WDir[0], 0, WCount - 1, @WDir[0], 0, WCount - 1);
          StepNorm := Sqrt(StepNorm);
          ENew := MLPError(Network, xy, NPoints) + 0.5 * Decay * XNorm2;
          if AP_FP_Less(StepNorm, LMStepTol * (1 + Sqrt(XNorm2))) then
            begin
              Break;
            end;
          if AP_FP_Greater(ENew, E) then
            begin
              LAMBDA := LAMBDA * LambdaUp * Nu;
              Nu := Nu * 2;
              Continue;
            end;

          //
          // Optimize using inv(cholesky(H)) as preconditioner
          //
          RMatrixTRInverse(HMod, WCount, True, False, InvInfo, InvRep);
          if InvInfo <= 0 then
            begin

              //
              // if matrix can't be inverted then exit with errors
              // TODO: make WCount steps in direction suggested by HMod
              //
              Info := -9;
              Exit;
            end;
          APVMove(@WBase[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
          i := 0;
          while i <= WCount - 1 do
            begin
              WT[i] := 0;
              inc(i);
            end;
          MinLBFGSCreateX(WCount, WCount, WT, 1, State);
          MinLBFGSSetCond(State, 0, 0, 0, 5);
          while MinLBFGSIteration(State) do
            begin

              //
              // gradient
              //
              i := 0;
              while i <= WCount - 1 do
                begin
                  v := APVDotProduct(@State.x[0], i, WCount - 1, @HMod[i][0], i, WCount - 1);
                  Network.Weights[i] := WBase[i] + v;
                  inc(i);
                end;
              MLPGradBatch(Network, xy, NPoints, State.f, g);
              i := 0;
              while i <= WCount - 1 do
                begin
                  State.g[i] := 0;
                  inc(i);
                end;
              i := 0;
              while i <= WCount - 1 do
                begin
                  v := g[i];
                  APVAdd(@State.g[0], i, WCount - 1, @HMod[i][0], i, WCount - 1, v);
                  inc(i);
                end;

              //
              // weight decay
              // grad(x'*x) = A'*(x0+A*t)
              //
              v := APVDotProduct(@Network.Weights[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
              State.f := State.f + 0.5 * Decay * v;
              i := 0;
              while i <= WCount - 1 do
                begin
                  v := Decay * Network.Weights[i];
                  APVAdd(@State.g[0], i, WCount - 1, @HMod[i][0], i, WCount - 1, v);
                  inc(i);
                end;

              //
              // next iteration
              //
              Rep.NGrad := Rep.NGrad + 1;
            end;
          MinLBFGSResults(State, WT, InternalRep);

          //
          // Accept new position.
          // Calculate Hessian
          //
          i := 0;
          while i <= WCount - 1 do
            begin
              v := APVDotProduct(@WT[0], i, WCount - 1, @HMod[i][0], i, WCount - 1);
              Network.Weights[i] := WBase[i] + v;
              inc(i);
            end;
          MLPHessianBatch(Network, xy, NPoints, E, g, h);
          v := APVDotProduct(@Network.Weights[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
          E := E + 0.5 * Decay * v;
          APVAdd(@g[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1, Decay);
          k := 0;
          while k <= WCount - 1 do
            begin
              h[k, k] := h[k, k] + Decay;
              inc(k);
            end;
          Rep.NHess := Rep.NHess + 1;

          //
          // Update lambda
          //
          LAMBDA := LAMBDA * LambdaDown;
          Nu := 2;
        end;

      //
      // update WBest
      //
      v := APVDotProduct(@Network.Weights[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
      E := 0.5 * Decay * v + MLPError(Network, xy, NPoints);
      if AP_FP_Less(E, EBest) then
        begin
          EBest := E;
          APVMove(@WBest[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
        end;
      inc(pass);
    end;

  //
  // copy WBest to output
  //
  APVMove(@Network.Weights[0], 0, WCount - 1, @WBest[0], 0, WCount - 1);
end;

(* ************************************************************************
  Neural network training  using  modified  Levenberg-Marquardt  with  exact
  Hessian calculation and regularization. Subroutine trains  neural  network
  with restarts from random positions. Algorithm is well  suited  for  small
  and medium scale problems (hundreds of weights).

  INPUT PARAMETERS:
  Network     -   neural network with initialized geometry
  XY          -   training set
  NPoints     -   training set size
  Decay       -   weight decay constant, >=0.001 Decay term 'Decay*||Weights||^2' is added to error function. If you don't know what Decay to choose, use 0.001.
  Restarts    -   number of restarts from random position, >0. If you don't know what Restarts to choose, use 2.

  OUTPUT PARAMETERS:
  Network     -   trained neural network.
  Info        -   return code:
  * -9, if internal matrix inverse subroutine failed
  * -2, if there is a point with class number outside of [0..NOut-1].
  * -1, if wrong parameters specified (NPoints<0, Restarts<1).
  *  2, if task has been solved.
  Rep         -   training report
  ************************************************************************ *)
procedure MLPTrainLM_MT(var Network: TMultiLayerPerceptron; const xy: TLMatrix;
  NPoints: TLInt; Decay: TLFloat; Restarts: TLInt;
  var Info: TLInt; var Rep: TMLPReport);
var
  i: TLInt;
  NIn: TLInt;
  NOut: TLInt;
  WCount: TLInt;
  LMStepTol: TLFloat;

  LAMBDA: TLFloat;
  LambdaUp: TLFloat;
  LambdaDown: TLFloat;

  WBest: TLVec;
  EBest: TLFloat;

  zLock: TCriticalSection;
  RepPtr: ^TMLPReport;
  InfoPtr: ^TLInt;

  NetworkPtr: PMultiLayerPerceptron;

{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer;
    Item: TMultiThreadProcItem);
  var
    _Parallel_I: TLInt;
    NetworkCopy: TMultiLayerPerceptron;
    // X: TLVec;
    // Y: TLVec;
    // D: TLVec;
    // Z: TLMatrix;
    InvInfo: TLInt;
    InternalRep: TMinLBFGSReport;
    SolverRep: TDenseSolverReport;
    SolverInfo: TLInt;
    InvRep: TMatInvReport;
    WT: TLVec;
    WDir: TLVec;
    State: TMinLBFGSState;
    WBase: TLVec;
    Nu: TLFloat;
    SPD: Boolean;
    HMod: TLMatrix;
    h: TLMatrix;
    g: TLVec;
    StepNorm: TLFloat;
    XNorm2: TLFloat;
    ENew: TLFloat;
    E: TLFloat;
    v: TLFloat;
    k: TLInt;
  begin

    SetLength(WT, WCount);
    SetLength(WDir, WCount);
    SetLength(WBase, WCount);
    SetLength(HMod, WCount, WCount);
    SetLength(h, WCount, WCount);
    SetLength(g, WCount);

    MLPCopy(NetworkPtr^, NetworkCopy);
    //
    // Initialize weights
    //
    MLPRandomize(NetworkCopy);

    //
    // First stage of the hybrid algorithm: LBFGS
    //
    APVMove(@WBase[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);
    MinLBFGSCreate(WCount, Min(WCount, 5), WBase, State);
    MinLBFGSSetCond(State, 0, 0, 0, Max(25, WCount));
    while MinLBFGSIteration(State) do
      begin

        //
        // gradient
        //
        APVMove(@NetworkCopy.Weights[0], 0, WCount - 1, @State.x[0], 0,
          WCount - 1);
        MLPGradBatch(NetworkCopy, xy, NPoints, State.f, State.g);

        //
        // weight decay
        //
        v := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1,
          @NetworkCopy.Weights[0], 0, WCount - 1);
        State.f := State.f + 0.5 * Decay * v;
        APVAdd(@State.g[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0,
          WCount - 1, Decay);

        //
        // next iteration
        //
        RepPtr^.NGrad := RepPtr^.NGrad + 1;
      end;
    MinLBFGSResults(State, WBase, InternalRep);
    APVMove(@NetworkCopy.Weights[0], 0, WCount - 1, @WBase[0], 0, WCount - 1);

    //
    // Second stage of the hybrid algorithm: LM
    //
    // Initialize H with identity matrix,
    // G with gradient,
    // E with regularized error.
    //
    MLPHessianBatch(NetworkCopy, xy, NPoints, E, g, h);
    v := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1,
      @NetworkCopy.Weights[0], 0, WCount - 1);
    E := E + 0.5 * Decay * v;
    APVAdd(@g[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1, Decay);
    k := 0;
    while k <= WCount - 1 do
      begin
        h[k, k] := h[k, k] + Decay;
        inc(k);
      end;
    RepPtr^.NHess := RepPtr^.NHess + 1;
    LAMBDA := 0.001;
    Nu := 2;
    while True do
      begin

        //
        // 1. HMod = H+lambda*_Parallel_I
        // 2. Try to solve (H+Lambda*_Parallel_I)*dx = -g.
        // Increase lambda if left part is not positive definite.
        //
        _Parallel_I := 0;
        while _Parallel_I <= WCount - 1 do
          begin
            APVMove(@HMod[_Parallel_I][0], 0, WCount - 1, @h[_Parallel_I][0], 0,
              WCount - 1);
            HMod[_Parallel_I, _Parallel_I] :=
              HMod[_Parallel_I, _Parallel_I] + LAMBDA;
            inc(_Parallel_I);
          end;
        SPD := SPDMatrixCholesky(HMod, WCount, True);
        RepPtr^.NCholesky := RepPtr^.NCholesky + 1;
        if not SPD then
          begin
            LAMBDA := LAMBDA * LambdaUp * Nu;
            Nu := Nu * 2;
            Continue;
          end;
        SPDMatrixCholeskySolve(HMod, WCount, True, g, SolverInfo,
          SolverRep, WDir);
        if SolverInfo < 0 then
          begin
            LAMBDA := LAMBDA * LambdaUp * Nu;
            Nu := Nu * 2;
            Continue;
          end;
        APVMul(@WDir[0], 0, WCount - 1, -1);

        //
        // Lambda found.
        // 1. Save old w in WBase
        // 1. Test some stopping criterions
        // 2. If error(w+wdir)>error(w), increase lambda
        //
        APVAdd(@NetworkCopy.Weights[0], 0, WCount - 1, @WDir[0], 0, WCount - 1);
        XNorm2 := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1,
          @NetworkCopy.Weights[0], 0, WCount - 1);
        StepNorm := APVDotProduct(@WDir[0], 0, WCount - 1, @WDir[0], 0,
          WCount - 1);
        StepNorm := Sqrt(StepNorm);
        ENew := MLPError(NetworkCopy, xy, NPoints) + 0.5 * Decay * XNorm2;
        if AP_FP_Less(StepNorm, LMStepTol * (1 + Sqrt(XNorm2))) then
          begin
            Break;
          end;
        if AP_FP_Greater(ENew, E) then
          begin
            LAMBDA := LAMBDA * LambdaUp * Nu;
            Nu := Nu * 2;
            Continue;
          end;

        //
        // Optimize using inv(cholesky(H)) as preconditioner
        //
        RMatrixTRInverse(HMod, WCount, True, False, InvInfo, InvRep);
        if InvInfo <= 0 then
          begin

            //
            // if matrix can't be inverted then exit with errors
            // TODO: make WCount steps in direction suggested by HMod
            //
            InfoPtr^ := -9;
            Exit;
          end;
        APVMove(@WBase[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);
        _Parallel_I := 0;
        while _Parallel_I <= WCount - 1 do
          begin
            WT[_Parallel_I] := 0;
            inc(_Parallel_I);
          end;
        MinLBFGSCreateX(WCount, WCount, WT, 1, State);
        MinLBFGSSetCond(State, 0, 0, 0, 5);
        while MinLBFGSIteration(State) do
          begin

            //
            // gradient
            //
            _Parallel_I := 0;
            while _Parallel_I <= WCount - 1 do
              begin
                v := APVDotProduct(@State.x[0], _Parallel_I, WCount - 1,
                  @HMod[_Parallel_I][0], _Parallel_I, WCount - 1);
                NetworkCopy.Weights[_Parallel_I] := WBase[_Parallel_I] + v;
                inc(_Parallel_I);
              end;
            MLPGradBatch(NetworkCopy, xy, NPoints, State.f, g);
            _Parallel_I := 0;
            while _Parallel_I <= WCount - 1 do
              begin
                State.g[_Parallel_I] := 0;
                inc(_Parallel_I);
              end;
            _Parallel_I := 0;
            while _Parallel_I <= WCount - 1 do
              begin
                v := g[_Parallel_I];
                APVAdd(@State.g[0], _Parallel_I, WCount - 1, @HMod[_Parallel_I][0],
                  _Parallel_I, WCount - 1, v);
                inc(_Parallel_I);
              end;

            //
            // weight decay
            // grad(x'*x) = A'*(x0+A*t)
            //
            v := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1,
              @NetworkCopy.Weights[0], 0, WCount - 1);
            State.f := State.f + 0.5 * Decay * v;
            _Parallel_I := 0;
            while _Parallel_I <= WCount - 1 do
              begin
                v := Decay * NetworkCopy.Weights[_Parallel_I];
                APVAdd(@State.g[0], _Parallel_I, WCount - 1, @HMod[_Parallel_I][0],
                  _Parallel_I, WCount - 1, v);
                inc(_Parallel_I);
              end;

            //
            // next iteration
            //
            RepPtr^.NGrad := RepPtr^.NGrad + 1;
          end;
        MinLBFGSResults(State, WT, InternalRep);

        //
        // Accept new position.
        // Calculate Hessian
        //
        _Parallel_I := 0;
        while _Parallel_I <= WCount - 1 do
          begin
            v := APVDotProduct(@WT[0], _Parallel_I, WCount - 1,
              @HMod[_Parallel_I][0], _Parallel_I, WCount - 1);
            NetworkCopy.Weights[_Parallel_I] := WBase[_Parallel_I] + v;
            inc(_Parallel_I);
          end;
        MLPHessianBatch(NetworkCopy, xy, NPoints, E, g, h);
        v := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1,
          @NetworkCopy.Weights[0], 0, WCount - 1);
        E := E + 0.5 * Decay * v;
        APVAdd(@g[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0,
          WCount - 1, Decay);
        k := 0;
        while k <= WCount - 1 do
          begin
            h[k, k] := h[k, k] + Decay;
            inc(k);
          end;
        RepPtr^.NHess := RepPtr^.NHess + 1;

        //
        // Update lambda
        //
        LAMBDA := LAMBDA * LambdaDown;
        Nu := 2;
      end;

    //
    // update WBest
    //
    v := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1,
      @NetworkCopy.Weights[0], 0, WCount - 1);
    E := 0.5 * Decay * v + MLPError(NetworkCopy, xy, NPoints);
    if AP_FP_Less(E, EBest) then
      begin
        zLock.Enter;
        EBest := E;
        APVMove(@WBest[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);
        zLock.Leave;
      end;

    // Free
    SetLength(WT, 0);
    SetLength(WDir, 0);
    SetLength(WBase, 0);
    SetLength(HMod, 0, 0);
    SetLength(h, 0, 0);
    SetLength(g, 0);
    MLPFree(NetworkCopy);
  end;
{$ENDIF FPC}


begin
  MLPProperties(Network, NIn, NOut, WCount);
  LambdaUp := 10;
  LambdaDown := 0.3;
  // LMFTol := 0.001;
  LMStepTol := 0.001;

  //
  // Test for inputs
  //
  if (NPoints <= 0) or (Restarts < 1) then
    begin
      Info := -1;
      Exit;
    end;
  if MLPIsSoftmax(Network) then
    begin
      i := 0;
      while i <= NPoints - 1 do
        begin
          if (Round(xy[i, NIn]) < 0) or (Round(xy[i, NIn]) >= NOut) then
            begin
              Info := -2;
              Exit;
            end;
          inc(i);
        end;
    end;
  Decay := Max(Decay, MinDecay);
  Info := 2;

  //
  // Initialize data
  //
  Rep.NGrad := 0;
  Rep.NHess := 0;
  Rep.NCholesky := 0;

  //
  // General case.
  // Prepare task and network. Allocate space.
  //
  MLPInitPreprocessor(Network, xy, NPoints);
  SetLength(WBest, WCount);
  EBest := MaxRealNumber;

  //
  // Multiple passes
  //
  NetworkPtr := @Network;
  RepPtr := @Rep;
  InfoPtr := @Info;
  zLock := TCriticalSection.Create;

{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 1, PtrInt(Restarts));
{$ELSE}
  TParallel.for(1, Restarts, procedure(pass: TLInt)
    var
      _Parallel_I: TLInt;
      NetworkCopy: TMultiLayerPerceptron;
      // X: TLVec;
      // Y: TLVec;
      // D: TLVec;
      // Z: TLMatrix;
      InvInfo: TLInt;
      InternalRep: TMinLBFGSReport;
      SolverRep: TDenseSolverReport;
      SolverInfo: TLInt;
      InvRep: TMatInvReport;
      WT: TLVec;
      WDir: TLVec;
      State: TMinLBFGSState;
      WBase: TLVec;
      Nu: TLFloat;
      SPD: Boolean;
      HMod: TLMatrix;
      h: TLMatrix;
      g: TLVec;
      StepNorm: TLFloat;
      XNorm2: TLFloat;
      ENew: TLFloat;
      E: TLFloat;
      v: TLFloat;
      k: TLInt;
    begin

      SetLength(WT, WCount);
      SetLength(WDir, WCount);
      SetLength(WBase, WCount);
      SetLength(HMod, WCount, WCount);
      SetLength(h, WCount, WCount);
      SetLength(g, WCount);

      MLPCopy(NetworkPtr^, NetworkCopy);
      //
      // Initialize weights
      //
      MLPRandomize(NetworkCopy);

      //
      // First stage of the hybrid algorithm: LBFGS
      //
      APVMove(@WBase[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);
      MinLBFGSCreate(WCount, Min(WCount, 5), WBase, State);
      MinLBFGSSetCond(State, 0, 0, 0, Max(25, WCount));
      while MinLBFGSIteration(State) do
        begin

          //
          // gradient
          //
          APVMove(@NetworkCopy.Weights[0], 0, WCount - 1, @State.x[0], 0,
            WCount - 1);
          MLPGradBatch(NetworkCopy, xy, NPoints, State.f, State.g);

          //
          // weight decay
          //
          v := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1,
            @NetworkCopy.Weights[0], 0, WCount - 1);
          State.f := State.f + 0.5 * Decay * v;
          APVAdd(@State.g[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0,
            WCount - 1, Decay);

          //
          // next iteration
          //
          RepPtr^.NGrad := RepPtr^.NGrad + 1;
        end;
      MinLBFGSResults(State, WBase, InternalRep);
      APVMove(@NetworkCopy.Weights[0], 0, WCount - 1, @WBase[0], 0, WCount - 1);

      //
      // Second stage of the hybrid algorithm: LM
      //
      // Initialize H with identity matrix,
      // G with gradient,
      // E with regularized error.
      //
      MLPHessianBatch(NetworkCopy, xy, NPoints, E, g, h);
      v := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1,
        @NetworkCopy.Weights[0], 0, WCount - 1);
      E := E + 0.5 * Decay * v;
      APVAdd(@g[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0,
        WCount - 1, Decay);
      k := 0;
      while k <= WCount - 1 do
        begin
          h[k, k] := h[k, k] + Decay;
          inc(k);
        end;
      RepPtr^.NHess := RepPtr^.NHess + 1;
      LAMBDA := 0.001;
      Nu := 2;
      while True do
        begin

          //
          // 1. HMod = H+lambda*_Parallel_I
          // 2. Try to solve (H+Lambda*_Parallel_I)*dx = -g.
          // Increase lambda if left part is not positive definite.
          //
          _Parallel_I := 0;
          while _Parallel_I <= WCount - 1 do
            begin
              APVMove(@HMod[_Parallel_I][0], 0, WCount - 1, @h[_Parallel_I][0], 0,
                WCount - 1);
              HMod[_Parallel_I, _Parallel_I] := HMod[_Parallel_I,
                _Parallel_I] + LAMBDA;
              inc(_Parallel_I);
            end;
          SPD := SPDMatrixCholesky(HMod, WCount, True);
          RepPtr^.NCholesky := RepPtr^.NCholesky + 1;
          if not SPD then
            begin
              LAMBDA := LAMBDA * LambdaUp * Nu;
              Nu := Nu * 2;
              Continue;
            end;
          SPDMatrixCholeskySolve(HMod, WCount, True, g, SolverInfo,
            SolverRep, WDir);
          if SolverInfo < 0 then
            begin
              LAMBDA := LAMBDA * LambdaUp * Nu;
              Nu := Nu * 2;
              Continue;
            end;
          APVMul(@WDir[0], 0, WCount - 1, -1);

          //
          // Lambda found.
          // 1. Save old w in WBase
          // 1. Test some stopping criterions
          // 2. If error(w+wdir)>error(w), increase lambda
          //
          APVAdd(@NetworkCopy.Weights[0], 0, WCount - 1, @WDir[0], 0, WCount - 1);
          XNorm2 := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1,
            @NetworkCopy.Weights[0], 0, WCount - 1);
          StepNorm := APVDotProduct(@WDir[0], 0, WCount - 1, @WDir[0], 0,
            WCount - 1);
          StepNorm := Sqrt(StepNorm);
          ENew := MLPError(NetworkCopy, xy, NPoints) + 0.5 * Decay * XNorm2;
          if AP_FP_Less(StepNorm, LMStepTol * (1 + Sqrt(XNorm2))) then
            begin
              Break;
            end;
          if AP_FP_Greater(ENew, E) then
            begin
              LAMBDA := LAMBDA * LambdaUp * Nu;
              Nu := Nu * 2;
              Continue;
            end;

          //
          // Optimize using inv(cholesky(H)) as preconditioner
          //
          RMatrixTRInverse(HMod, WCount, True, False, InvInfo, InvRep);
          if InvInfo <= 0 then
            begin

              //
              // if matrix can't be inverted then exit with errors
              // TODO: make WCount steps in direction suggested by HMod
              //
              InfoPtr^ := -9;
              Exit;
            end;
          APVMove(@WBase[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0,
            WCount - 1);
          _Parallel_I := 0;
          while _Parallel_I <= WCount - 1 do
            begin
              WT[_Parallel_I] := 0;
              inc(_Parallel_I);
            end;
          MinLBFGSCreateX(WCount, WCount, WT, 1, State);
          MinLBFGSSetCond(State, 0, 0, 0, 5);
          while MinLBFGSIteration(State) do
            begin

              //
              // gradient
              //
              _Parallel_I := 0;
              while _Parallel_I <= WCount - 1 do
                begin
                  v := APVDotProduct(@State.x[0], _Parallel_I, WCount - 1,
                    @HMod[_Parallel_I][0], _Parallel_I, WCount - 1);
                  NetworkCopy.Weights[_Parallel_I] := WBase[_Parallel_I] + v;
                  inc(_Parallel_I);
                end;
              MLPGradBatch(NetworkCopy, xy, NPoints, State.f, g);
              _Parallel_I := 0;
              while _Parallel_I <= WCount - 1 do
                begin
                  State.g[_Parallel_I] := 0;
                  inc(_Parallel_I);
                end;
              _Parallel_I := 0;
              while _Parallel_I <= WCount - 1 do
                begin
                  v := g[_Parallel_I];
                  APVAdd(@State.g[0], _Parallel_I, WCount - 1, @HMod[_Parallel_I][0],
                    _Parallel_I, WCount - 1, v);
                  inc(_Parallel_I);
                end;

              //
              // weight decay
              // grad(x'*x) = A'*(x0+A*t)
              //
              v := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1,
                @NetworkCopy.Weights[0], 0, WCount - 1);
              State.f := State.f + 0.5 * Decay * v;
              _Parallel_I := 0;
              while _Parallel_I <= WCount - 1 do
                begin
                  v := Decay * NetworkCopy.Weights[_Parallel_I];
                  APVAdd(@State.g[0], _Parallel_I, WCount - 1, @HMod[_Parallel_I][0],
                    _Parallel_I, WCount - 1, v);
                  inc(_Parallel_I);
                end;

              //
              // next iteration
              //
              RepPtr^.NGrad := RepPtr^.NGrad + 1;
            end;
          MinLBFGSResults(State, WT, InternalRep);

          //
          // Accept new position.
          // Calculate Hessian
          //
          _Parallel_I := 0;
          while _Parallel_I <= WCount - 1 do
            begin
              v := APVDotProduct(@WT[0], _Parallel_I, WCount - 1,
                @HMod[_Parallel_I][0], _Parallel_I, WCount - 1);
              NetworkCopy.Weights[_Parallel_I] := WBase[_Parallel_I] + v;
              inc(_Parallel_I);
            end;
          MLPHessianBatch(NetworkCopy, xy, NPoints, E, g, h);
          v := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1,
            @NetworkCopy.Weights[0], 0, WCount - 1);
          E := E + 0.5 * Decay * v;
          APVAdd(@g[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0,
            WCount - 1, Decay);
          k := 0;
          while k <= WCount - 1 do
            begin
              h[k, k] := h[k, k] + Decay;
              inc(k);
            end;
          RepPtr^.NHess := RepPtr^.NHess + 1;

          //
          // Update lambda
          //
          LAMBDA := LAMBDA * LambdaDown;
          Nu := 2;
        end;

      //
      // update WBest
      //
      v := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1,
        @NetworkCopy.Weights[0], 0, WCount - 1);
      E := 0.5 * Decay * v + MLPError(NetworkCopy, xy, NPoints);
      if AP_FP_Less(E, EBest) then
        begin
          zLock.Enter;
          EBest := E;
          APVMove(@WBest[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0,
            WCount - 1);
          zLock.Leave;
        end;

      // Free
      SetLength(WT, 0);
      SetLength(WDir, 0);
      SetLength(WBase, 0);
      SetLength(HMod, 0, 0);
      SetLength(h, 0, 0);
      SetLength(g, 0);
      MLPFree(NetworkCopy);
    end);
{$ENDIF FPC}
  zLock.Free;

  //
  // copy WBest to output
  //
  Rep := RepPtr^;
  Info := InfoPtr^;
  APVMove(@Network.Weights[0], 0, WCount - 1, @WBest[0], 0, WCount - 1);
end;

(* ************************************************************************
  Neural  network  training  using  L-BFGS  algorithm  with  regularization.
  Subroutine  trains  neural  network  with  restarts from random positions.
  Algorithm  is  well  suited  for  problems  of  any dimensionality (memory
  requirements and step complexity are linear by weights number).

  INPUT PARAMETERS:
  Network     -   neural network with initialized geometry
  XY          -   training set
  NPoints     -   training set size
  Decay       -   weight decay constant, >=0.001 Decay term 'Decay*||Weights||^2' is added to error function. If you don't know what Decay to choose, use 0.001.
  Restarts    -   number of restarts from random position, >0. If you don't know what Restarts to choose, use 2.
  WStep       -   stopping criterion. Algorithm stops if  step  size  is less than WStep. Recommended value - 0.01. Zero  step size means stopping after MaxIts iterations.
  MaxIts      -   stopping   criterion. Algorithm stops after MaxIts iterations (NOT gradient calculations). Zero MaxIts means stopping when step is sufficiently small.

  OUTPUT PARAMETERS:
  Network     -   trained neural network.
  Info        -   return code:
  * -8, if both WStep=0 and MaxIts=0
  * -2, if there is a point with class number outside of [0..NOut-1].
  * -1, if wrong parameters specified (NPoints<0, Restarts<1).
  *  2, if task has been solved.
  Rep         -   training report
  ************************************************************************ *)
procedure MLPTrainLBFGS(var Network: TMultiLayerPerceptron;
const xy: TLMatrix; NPoints: TLInt; Decay: TLFloat;
Restarts: TLInt; WStep: TLFloat; MAXITS: TLInt;
var Info: TLInt; var Rep: TMLPReport; IsTerminated: PBoolean;
out EBest: TLFloat);
var
  i: TLInt;
  pass: TLInt;
  NIn: TLInt;
  NOut: TLInt;
  WCount: TLInt;
  w: TLVec;
  WBest: TLVec;
  E: TLFloat;
  v: TLFloat;
  InternalRep: TMinLBFGSReport;
  State: TMinLBFGSState;
begin

  EBest := MaxDouble;
  //
  // Test inputs, parse flags, read network geometry
  //
  if AP_FP_Eq(WStep, 0) and (MAXITS = 0) then
    begin
      Info := -8;
      Exit;
    end;
  if (NPoints <= 0) or (Restarts < 1) or AP_FP_Less(WStep, 0) or (MAXITS < 0)
  then
    begin
      Info := -1;
      Exit;
    end;
  MLPProperties(Network, NIn, NOut, WCount);
  if MLPIsSoftmax(Network) then
    begin
      i := 0;
      while i <= NPoints - 1 do
        begin
          if (Round(xy[i, NIn]) < 0) or (Round(xy[i, NIn]) >= NOut) then
            begin
              Info := -2;
              Exit;
            end;
          inc(i);
        end;
    end;
  Decay := Max(Decay, MinDecay);
  Info := 2;

  //
  // Prepare
  //
  MLPInitPreprocessor(Network, xy, NPoints);
  SetLength(w, WCount);
  SetLength(WBest, WCount);
  EBest := MaxRealNumber;

  //
  // Multiple starts
  //
  Rep.NCholesky := 0;
  Rep.NHess := 0;
  Rep.NGrad := 0;
  for pass := 1 to Restarts do
    begin

      //
      // Process
      //
      MLPRandomize(Network);
      APVMove(@w[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
      MinLBFGSCreate(WCount, Min(WCount, 10), w, State);
      MinLBFGSSetCond(State, 0.0, 0.0, WStep, MAXITS);
      while not IsTerminated^ and MinLBFGSIteration(State) do
        begin
          APVMove(@Network.Weights[0], 0, WCount - 1, @State.x[0], 0, WCount - 1);
          MLPGradNBatch(Network, xy, NPoints, State.f, State.g);
          v := APVDotProduct(@Network.Weights[0], 0, WCount - 1,
            @Network.Weights[0], 0, WCount - 1);
          State.f := State.f + 0.5 * Decay * v;
          APVAdd(@State.g[0], 0, WCount - 1, @Network.Weights[0], 0,
            WCount - 1, Decay);
          Rep.NGrad := Rep.NGrad + 1;
        end;
      MinLBFGSResults(State, w, InternalRep);
      APVMove(@Network.Weights[0], 0, WCount - 1, @w[0], 0, WCount - 1);

      //
      // Compare with best
      //
      v := APVDotProduct(@Network.Weights[0], 0, WCount - 1, @Network.Weights[0],
        0, WCount - 1);
      E := MLPErrorN(Network, xy, NPoints) + 0.5 * Decay * v;
      if AP_FP_Less(E, EBest) then
        begin
          APVMove(@WBest[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
          EBest := E;
        end;
    end;

  //
  // The best network
  //
  APVMove(@Network.Weights[0], 0, WCount - 1, @WBest[0], 0, WCount - 1);
end;

{ WStep - affects the accuracy of optimal weights. high precision starts from 0.01
  Restarts affect the likelihood of finding the best weights > 100
  MaxIts - the higher the accuracy of the required its iterations > = 500
  Diameter - an initial values within the vector weighting > = 2 }
procedure MLPTrainLBFGS_MT(var Network: TMultiLayerPerceptron;
const xy: TLMatrix; NPoints: TLInt; Decay: TLFloat;
Restarts: TLInt; WStep: TLFloat; MAXITS: TLInt;
var Info: TLInt; var Rep: TMLPReport);
var
  i: TLInt;
  NIn: TLInt;
  NOut: TLInt;
  WCount: TLInt;
  WBest: TLVec;
  EBest: TLFloat;

  zLock: TCriticalSection;
  NetworkPtr: PMultiLayerPerceptron;
  RepCopy: TMLPReport;
  zProcessData: PMLPProcessData;

{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer;
  Item: TMultiThreadProcItem);
  var
    E: TLFloat;
    v: TLFloat;
    w: TLVec;
    State: TMinLBFGSState;
    InternalRep: TMinLBFGSReport;
    NetworkCopy: TMultiLayerPerceptron;
  begin
    SetLength(w, WCount);
    MLPCopy(NetworkPtr^, NetworkCopy);

    //
    // Process
    //
    MLPRandomize(NetworkCopy);
    APVMove(@w[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);
    MinLBFGSCreate(WCount, WCount, w, State);
    MinLBFGSSetCond(State, 0.0, 0.0, WStep, MAXITS);
    while MinLBFGSIteration(State) do
      begin
        APVMove(@NetworkCopy.Weights[0], 0, WCount - 1, @State.x[0], 0, WCount - 1);
        MLPGradNBatch(NetworkCopy, xy, NPoints, State.f, State.g);
        v := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);
        State.f := State.f + 0.5 * Decay * v;
        APVAdd(@State.g[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1, Decay);
        RepCopy.NGrad := RepCopy.NGrad + 1;
        if zProcessData^.IsTerminated then
            Break;
      end;
    MinLBFGSResults(State, w, InternalRep);
    APVMove(@NetworkCopy.Weights[0], 0, WCount - 1, @w[0], 0, WCount - 1);

    //
    // Compare with best
    //

    E := MLPErrorN(NetworkCopy, xy, NPoints);
    if AP_FP_Less(E, EBest) then
      begin
        zLock.Enter;
        APVMove(@WBest[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);
        EBest := E;
        zProcessData^.EBest := EBest;
        zLock.Leave;
      end;
    MLPFree(NetworkCopy);
    MinLBFGSFree(w, State);
    inc(zProcessData^.RestartsFinished);

    if zProcessData^.IsTerminated then
        Exit;
  end;
{$ENDIF FPC}


begin
  //
  // Test inputs, parse flags, read network geometry
  //
  if AP_FP_Eq(WStep, 0) and (MAXITS = 0) then
    begin
      Info := -8;
      Exit;
    end;
  if (NPoints <= 0) or (Restarts < 1) or AP_FP_Less(WStep, 0) or (MAXITS < 0)
  then
    begin
      Info := -1;
      Exit;
    end;
  MLPProperties(Network, NIn, NOut, WCount);
  if MLPIsSoftmax(Network) then
    begin
      i := 0;
      while i <= NPoints - 1 do
        begin
          if (Round(xy[i, NIn]) < 0) or (Round(xy[i, NIn]) >= NOut) then
            begin
              Info := -2;
              Exit;
            end;
          inc(i);
        end;
    end;
  Decay := Max(Decay, MinDecay);
  Info := 2;

  //
  // Prepare
  //
  MLPInitPreprocessor(Network, xy, NPoints);
  SetLength(WBest, WCount);
  EBest := MaxRealNumber;
  new(zProcessData);
  zProcessData^.TotalRestarts := Restarts;
  zProcessData^.IsTerminated := False;
  zProcessData^.EBest := EBest;

  //
  // Multiple starts
  //
  Rep.NCholesky := 0;
  Rep.NHess := 0;
  Rep.NGrad := 0;

  NetworkPtr := @Network;
  RepCopy := Rep;
  zLock := TCriticalSection.Create;

  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 1, PtrInt(Restarts));
{$ELSE}
    TParallel.for(1, Restarts, procedure(pass: TLInt)
      var
        E: TLFloat;
        v: TLFloat;
        w: TLVec;
        State: TMinLBFGSState;
        InternalRep: TMinLBFGSReport;
        NetworkCopy: TMultiLayerPerceptron;
      begin
        SetLength(w, WCount);
        MLPCopy(NetworkPtr^, NetworkCopy);

        //
        // Process
        //
        MLPRandomize(NetworkCopy);
        APVMove(@w[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);
        MinLBFGSCreate(WCount, WCount, w, State);
        MinLBFGSSetCond(State, 0.0, 0.0, WStep, MAXITS);
        while MinLBFGSIteration(State) do
          begin
            APVMove(@NetworkCopy.Weights[0], 0, WCount - 1, @State.x[0], 0, WCount - 1);
            MLPGradNBatch(NetworkCopy, xy, NPoints, State.f, State.g);
            v := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);
            State.f := State.f + 0.5 * Decay * v;
            APVAdd(@State.g[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1, Decay);
            RepCopy.NGrad := RepCopy.NGrad + 1;
            if zProcessData.IsTerminated then
                Break;
          end;
        MinLBFGSResults(State, w, InternalRep);
        APVMove(@NetworkCopy.Weights[0], 0, WCount - 1, @w[0], 0, WCount - 1);

        //
        // Compare with best
        //

        E := MLPErrorN(NetworkCopy, xy, NPoints);
        if AP_FP_Less(E, EBest) then
          begin
            zLock.Enter;
            APVMove(@WBest[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);
            EBest := E;
            zProcessData.EBest := EBest;
            zLock.Leave;
          end;
        MLPFree(NetworkCopy);
        MinLBFGSFree(w, State);
        inc(zProcessData.RestartsFinished);

        if zProcessData.IsTerminated then
            Exit;
      end);
{$ENDIF FPC}
  finally
    zLock.Free;
    Dispose(zProcessData);

    //
    // The best network
    //
    Rep := RepCopy;

    APVMove(@Network.Weights[0], 0, WCount - 1, @WBest[0], 0, WCount - 1);
  end;
end;

{ WStep - affects the accuracy of optimal weights. high precision starts from 0.01
  Restarts affect the likelihood of finding the best weights > 100
  MaxIts - the higher the accuracy of the required its iterations > = 500
  Diameter - an initial values within the vector weighting > = 2 }
procedure MLPTrainLBFGS_MT_Mod(var Network: TMultiLayerPerceptron;
const xy: TLMatrix; NPoints: TLInt; Restarts: TLInt;
WStep, Diameter: TLFloat; MAXITS: TLInt; var Info: TLInt;
var Rep: TMLPReport);
var
  i: TLInt;
  NIn: TLInt;
  NOut: TLInt;
  WCount: TLInt;
  WBest: TLVec;
  EBest: TLFloat;

  zLock: TCriticalSection;
  NetworkPtr: PMultiLayerPerceptron;
  RepCopy: TMLPReport;
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer;
  Item: TMultiThreadProcItem);
  var
    Ebegin: TLFloat; { the initial error }
    E: TLFloat;
    v: TLFloat;
    w: TLVec;
    State: TMinLBFGSState;
    InternalRep: TMinLBFGSReport;
    NetworkCopy: TMultiLayerPerceptron;
    Decay: TLFloat;
    Iteration: TLInt;
  begin
    try
      Decay := 1;
      Iteration := 0;
      SetLength(w, WCount);
      MLPCopy(NetworkPtr^, NetworkCopy);

      //
      // Process
      //
      MLPRandomize(NetworkCopy, Diameter);

      APVMove(@w[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);
      MinLBFGSCreate(WCount, Min(WCount, 10), w, State);
      MinLBFGSSetCond(State, 0.0, 0.0, WStep, MAXITS);
      Ebegin := MLPErrorN(NetworkCopy, xy, NPoints);

      // Compare with best
      if AP_FP_Less(Ebegin, EBest) then
        begin
          zLock.Enter;
          APVMove(@WBest[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0,
            WCount - 1);
          EBest := Ebegin;
          zLock.Leave;
        end;

      while (EBest > 1E-50) and MinLBFGSIteration(State) do
        begin
          APVMove(@NetworkCopy.Weights[0], 0, WCount - 1, @State.x[0], 0, WCount - 1);
          MLPGradNBatch(NetworkCopy, xy, NPoints, State.f, State.g);
          v := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);

          State.f := State.f + 0.5 * Decay * v;
          APVAdd(@State.g[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1, Decay);
          RepCopy.NGrad := RepCopy.NGrad + 1;

          // checking
          MinLBFGSResults(State, w, InternalRep);
          APVMove(@NetworkCopy.Weights[0], 0, WCount - 1, @w[0], 0, WCount - 1);
          E := MLPErrorN(NetworkCopy, xy, NPoints);

          if (E <= Ebegin) then
            begin
              Ebegin := E;
              if (E < 1E-50) then
                  Break;
            end;

          { dynamic change of decay }
          inc(Iteration);
          // Decay := 1 / ((10 + Iteration) div 10);
          Decay := 1 / Iteration;
        end;

      // Compare with best
      if AP_FP_Less(Ebegin, EBest) then
        begin
          zLock.Enter;
          APVMove(@WBest[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0,
            WCount - 1);
          EBest := Ebegin;
          zLock.Leave;
        end;

    finally
      MLPFree(NetworkCopy);
      MinLBFGSFree(w, State);
    end;

  end;
{$ENDIF FPC}


begin

  //
  // Test inputs, parse flags, read network geometry
  //
  if AP_FP_Eq(WStep, 0) and (MAXITS = 0) then
    begin
      Info := -8;
      Exit;
    end;
  if (NPoints <= 0) or (Restarts < 1) or AP_FP_Less(WStep, 0) or (MAXITS < 0)
  then
    begin
      Info := -1;
      Exit;
    end;
  MLPProperties(Network, NIn, NOut, WCount);
  if MLPIsSoftmax(Network) then
    begin
      i := 0;
      while i <= NPoints - 1 do
        begin
          if (Round(xy[i, NIn]) < 0) or (Round(xy[i, NIn]) >= NOut) then
            begin
              Info := -2;
              Exit;
            end;
          inc(i);
        end;
    end;
  Info := 2;

  //
  // Prepare
  //
  MLPInitPreprocessor(Network, xy, NPoints);
  SetLength(WBest, WCount);
  EBest := MaxRealNumber;

  //
  // Multiple starts
  //
  Rep.NCholesky := 0;
  Rep.NHess := 0;
  Rep.NGrad := 0;

  NetworkPtr := @Network;
  RepCopy := Rep;
  zLock := TCriticalSection.Create;

{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 1, PtrInt(Restarts));
{$ELSE}
  TParallel.for(1, Restarts, procedure(pass: TLInt)
    var
      Ebegin: TLFloat; { the initial error }
      E: TLFloat;
      v: TLFloat;
      w: TLVec;
      State: TMinLBFGSState;
      InternalRep: TMinLBFGSReport;
      NetworkCopy: TMultiLayerPerceptron;
      Decay: TLFloat;
      Iteration: TLInt;
    begin
      try
        Decay := 1;
        Iteration := 0;
        SetLength(w, WCount);
        MLPCopy(NetworkPtr^, NetworkCopy);

        //
        // Process
        //
        MLPRandomize(NetworkCopy, Diameter);

        APVMove(@w[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);
        MinLBFGSCreate(WCount, Min(WCount, 10), w, State);
        MinLBFGSSetCond(State, 0.0, 0.0, WStep, MAXITS);
        Ebegin := MLPErrorN(NetworkCopy, xy, NPoints);

        // Compare with best
        if AP_FP_Less(Ebegin, EBest) then
          begin
            zLock.Enter;
            APVMove(@WBest[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);
            EBest := Ebegin;
            zLock.Leave;
          end;

        while (EBest > 1E-50) and MinLBFGSIteration(State) do
          begin
            APVMove(@NetworkCopy.Weights[0], 0, WCount - 1, @State.x[0], 0, WCount - 1);
            MLPGradNBatch(NetworkCopy, xy, NPoints, State.f, State.g);
            v := APVDotProduct(@NetworkCopy.Weights[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1);

            State.f := State.f + 0.5 * Decay * v;
            APVAdd(@State.g[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0, WCount - 1, Decay);
            RepCopy.NGrad := RepCopy.NGrad + 1;

            // checking
            MinLBFGSResults(State, w, InternalRep);
            APVMove(@NetworkCopy.Weights[0], 0, WCount - 1, @w[0], 0, WCount - 1);
            E := MLPErrorN(NetworkCopy, xy, NPoints);

            if (E <= Ebegin) then
              begin
                Ebegin := E;
                if (E < 1E-50) then
                    Break;
              end;

            { dynamic change of decay }
            inc(Iteration);
            // Decay := 1 / ((10 + Iteration) div 10);
            Decay := 1 / Iteration;
          end;

        // Compare with best
        if AP_FP_Less(Ebegin, EBest) then
          begin
            zLock.Enter;
            APVMove(@WBest[0], 0, WCount - 1, @NetworkCopy.Weights[0], 0,
              WCount - 1);
            EBest := Ebegin;
            zLock.Leave;
          end;

      finally
        MLPFree(NetworkCopy);
        MinLBFGSFree(w, State);
      end;

    end);
{$ENDIF FPC}
  zLock.Free;

  //
  // The best network
  //
  Rep := RepCopy;

  APVMove(@Network.Weights[0], 0, WCount - 1, @WBest[0], 0, WCount - 1);
end;

procedure MLPTrainMonteCarlo(var Network: TMultiLayerPerceptron; const xy: TLMatrix; NPoints: TLInt;
const MainRestarts, SubRestarts: TLInt; const MinError: TLFloat;
Diameter: TLFloat; var Info: TLInt; var Rep: TMLPReport);
var
  i: TLInt;
  MainPass, SubPass: TLInt;
  NIn: TLInt;
  NOut: TLInt;
  WCount: TLInt;
  w: TLVec;
  WBest: TLVec;
  E: TLFloat;
  EBest: TLFloat;
  EBestSub: TLFloat;
begin

  //
  // Test inputs, parse flags, read network geometry
  //
  if (NPoints <= 0) or (MainRestarts < 1) or (SubRestarts < 1) or
    (Diameter < 0.01) then
    begin
      Info := -1;
      Exit;
    end;
  MLPProperties(Network, NIn, NOut, WCount);
  if MLPIsSoftmax(Network) then
    begin
      i := 0;
      while i <= NPoints - 1 do
        begin
          if (Round(xy[i, NIn]) < 0) or (Round(xy[i, NIn]) >= NOut) then
            begin
              Info := -2;
              Exit;
            end;
          inc(i);
        end;
    end;
  Info := 2;

  //
  // Prepare
  //
  MLPInitPreprocessor(Network, xy, NPoints);
  SetLength(w, WCount);
  SetLength(WBest, WCount);
  APVFillValue(@WBest[0], 0, WCount - 1, 0);
  EBest := MaxRealNumber;

  //
  // Multiple starts
  //
  Rep.NCholesky := 0;
  Rep.NHess := 0;
  Rep.NGrad := 0;

  for MainPass := 1 to MainRestarts do
    begin
      EBestSub := MaxRealNumber;
      for SubPass := 1 to SubRestarts do
        begin

          // Process
          MLPRandomize(Network, WBest, Diameter);

          // Compare with best
          E := MLPErrorN(Network, xy, NPoints);
          if AP_FP_Less(E, EBestSub) then
            begin
              if AP_FP_Less(E, EBest) then
                  APVMove(@w[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
              EBestSub := E;
            end;

          if AP_FP_Less(EBestSub, MinError) then
              Break;

        end;

      // The best network
      if AP_FP_Less(EBestSub, EBest) then
        begin
          APVMove(@WBest[0], 0, WCount - 1, @w[0], 0, WCount - 1);
          EBest := EBestSub;
        end;

      if AP_FP_Less(EBest, MinError) then
          Break;

      Diameter := Diameter * 0.5;
    end;
  // The best network
  APVMove(@Network.Weights[0], 0, WCount - 1, @WBest[0], 0, WCount - 1);
end;

(* ************************************************************************
  Cross-validation estimate of generalization error.

  Base algorithm - L-BFGS.

  INPUT PARAMETERS:
  Network     -   neural network with initialized geometry.   Network is
  not changed during cross-validation -  it is used only
  as a representative of its architecture.
  XY          -   training set.
  SSize       -   training set size
  Decay       -   weight  decay, same as in MLPTrainLBFGS
  Restarts    -   number of restarts, >0.
  restarts are counted for each partition separately, so
  total number of restarts will be Restarts*FoldsCount.
  WStep       -   stopping criterion, same as in MLPTrainLBFGS
  MaxIts      -   stopping criterion, same as in MLPTrainLBFGS
  FoldsCount  -   number of folds in k-fold cross-validation, 2<=FoldsCount<=SSize. recommended value: 10.

  OUTPUT PARAMETERS:
  Info        -   return code, same as in MLPTrainLBFGS
  Rep         -   report, same as in MLPTrainLM/MLPTrainLBFGS
  CVRep       -   generalization error estimates
  ************************************************************************ *)
procedure MLPKFoldCVLBFGS(const Network: TMultiLayerPerceptron;
const xy: TLMatrix; NPoints: TLInt; Decay: TLFloat;
Restarts: TLInt; WStep: TLFloat; MAXITS: TLInt;
FoldsCount: TLInt; var Info: TLInt; var Rep: TMLPReport;
var CVRep: TMLPCVReport);
begin
  MLPKFoldCVGeneral(Network, xy, NPoints, Decay, Restarts, FoldsCount, False, WStep, MAXITS, Info, Rep, CVRep);
end;

(* ************************************************************************
  Cross-validation estimate of generalization error.

  Base algorithm - Levenberg-Marquardt.

  INPUT PARAMETERS:
  Network     -   neural network with initialized geometry.   Network is
  not changed during cross-validation -  it is used only
  as a representative of its architecture.
  XY          -   training set.
  SSize       -   training set size
  Decay       -   weight  decay, same as in MLPTrainLBFGS
  Restarts    -   number of restarts, >0.
  restarts are counted for each partition separately, so
  total number of restarts will be Restarts*FoldsCount.
  FoldsCount  -   number of folds in k-fold cross-validation, 2<=FoldsCount<=SSize. recommended value: 10.

  OUTPUT PARAMETERS:
  Info        -   return code, same as in MLPTrainLBFGS
  Rep         -   report, same as in MLPTrainLM/MLPTrainLBFGS
  CVRep       -   generalization error estimates
  ************************************************************************ *)
procedure MLPKFoldCVLM(const Network: TMultiLayerPerceptron;
const xy: TLMatrix; NPoints: TLInt; Decay: TLFloat;
Restarts: TLInt; FoldsCount: TLInt; var Info: TLInt;
var Rep: TMLPReport; var CVRep: TMLPCVReport);
begin
  MLPKFoldCVGeneral(Network, xy, NPoints, Decay, Restarts, FoldsCount, True, 0.0, 0, Info, Rep, CVRep);
end;

(* ************************************************************************
  Internal cross-validation subroutine
  ************************************************************************ *)
procedure MLPKFoldCVGeneral(const n: TMultiLayerPerceptron;
const xy: TLMatrix; NPoints: TLInt; Decay: TLFloat;
Restarts: TLInt; FoldsCount: TLInt; LMAlgorithm: Boolean;
WStep: TLFloat; MAXITS: TLInt; var Info: TLInt;
var Rep: TMLPReport; var CVRep: TMLPCVReport);
var
  i: TLInt;
  FOld: TLInt;
  j: TLInt;
  k: TLInt;
  Network: TMultiLayerPerceptron;
  NIn: TLInt;
  NOut: TLInt;
  RowLen: TLInt;
  WCount: TLInt;
  NClasses: TLInt;
  TSSize: TLInt;
  CVSSize: TLInt;
  CVSet: TLMatrix;
  TestSet: TLMatrix;
  Folds: TLIVec;
  RelCnt: TLInt;
  InternalRep: TMLPReport;
  x: TLVec;
  y: TLVec;
  IsTerminated: Boolean;
  EBest: TLFloat;
begin

  //
  // Read network geometry, test parameters
  //
  MLPProperties(n, NIn, NOut, WCount);
  if MLPIsSoftmax(n) then
    begin
      NClasses := NOut;
      RowLen := NIn + 1;
    end
  else
    begin
      NClasses := -NOut;
      RowLen := NIn + NOut;
    end;
  if (NPoints <= 0) or (FoldsCount < 2) or (FoldsCount > NPoints) then
    begin
      Info := -1;
      Exit;
    end;
  MLPCopy(n, Network);

  //
  // K-fold out cross-validation.
  // First, estimate generalization error
  //
  SetLength(TestSet, NPoints, RowLen);
  SetLength(CVSet, NPoints, RowLen);
  SetLength(x, NIn);
  SetLength(y, NOut);
  MLPKFoldSplit(xy, NPoints, NClasses, FoldsCount, False, Folds);
  CVRep.RelClsError := 0;
  CVRep.AvgCE := 0;
  CVRep.RMSError := 0;
  CVRep.AvgError := 0;
  CVRep.AvgRelError := 0;
  Rep.NGrad := 0;
  Rep.NHess := 0;
  Rep.NCholesky := 0;
  RelCnt := 0;
  FOld := 0;
  while FOld <= FoldsCount - 1 do
    begin

      //
      // Separate set
      //
      TSSize := 0;
      CVSSize := 0;
      i := 0;
      while i <= NPoints - 1 do
        begin
          if Folds[i] = FOld then
            begin
              APVMove(@TestSet[TSSize][0], 0, RowLen - 1, @xy[i][0], 0, RowLen - 1);
              TSSize := TSSize + 1;
            end
          else
            begin
              APVMove(@CVSet[CVSSize][0], 0, RowLen - 1, @xy[i][0], 0, RowLen - 1);
              CVSSize := CVSSize + 1;
            end;
          inc(i);
        end;

      //
      // Train on CV training set
      //
      if LMAlgorithm then
        begin
          MLPTrainLM(Network, CVSet, CVSSize, Decay, Restarts, Info, InternalRep);
        end
      else
        begin
          IsTerminated := False;
          MLPTrainLBFGS(Network, CVSet, CVSSize, Decay, Restarts, WStep, MAXITS, Info, InternalRep, @IsTerminated, EBest);
        end;
      if Info < 0 then
        begin
          CVRep.RelClsError := 0;
          CVRep.AvgCE := 0;
          CVRep.RMSError := 0;
          CVRep.AvgError := 0;
          CVRep.AvgRelError := 0;
          Exit;
        end;
      Rep.NGrad := Rep.NGrad + InternalRep.NGrad;
      Rep.NHess := Rep.NHess + InternalRep.NHess;
      Rep.NCholesky := Rep.NCholesky + InternalRep.NCholesky;

      //
      // Estimate error using CV test set
      //
      if MLPIsSoftmax(Network) then
        begin

          //
          // classification-only code
          //
          CVRep.RelClsError := CVRep.RelClsError + MLPClsError(Network, TestSet, TSSize);
          CVRep.AvgCE := CVRep.AvgCE + MLPErrorN(Network, TestSet, TSSize);
        end;
      i := 0;
      while i <= TSSize - 1 do
        begin
          APVMove(@x[0], 0, NIn - 1, @TestSet[i][0], 0, NIn - 1);
          MLPProcess(Network, x, y);
          if MLPIsSoftmax(Network) then
            begin

              //
              // Classification-specific code
              //
              k := Round(TestSet[i, NIn]);
              j := 0;
              while j <= NOut - 1 do
                begin
                  if j = k then
                    begin
                      CVRep.RMSError := CVRep.RMSError + AP_Sqr(y[j] - 1);
                      CVRep.AvgError := CVRep.AvgError + AbsReal(y[j] - 1);
                      CVRep.AvgRelError := CVRep.AvgRelError + AbsReal(y[j] - 1);
                      RelCnt := RelCnt + 1;
                    end
                  else
                    begin
                      CVRep.RMSError := CVRep.RMSError + AP_Sqr(y[j]);
                      CVRep.AvgError := CVRep.AvgError + AbsReal(y[j]);
                    end;
                  inc(j);
                end;
            end
          else
            begin

              //
              // Regression-specific code
              //
              j := 0;
              while j <= NOut - 1 do
                begin
                  CVRep.RMSError := CVRep.RMSError + AP_Sqr(y[j] - TestSet[i, NIn + j]);
                  CVRep.AvgError := CVRep.AvgError + AbsReal(y[j] - TestSet[i, NIn + j]);
                  if AP_FP_NEq(TestSet[i, NIn + j], 0) then
                    begin
                      CVRep.AvgRelError := CVRep.AvgRelError + AbsReal((y[j] - TestSet[i, NIn + j]) / TestSet[i, NIn + j]);
                      RelCnt := RelCnt + 1;
                    end;
                  inc(j);
                end;
            end;
          inc(i);
        end;
      inc(FOld);
    end;
  if MLPIsSoftmax(Network) then
    begin
      CVRep.RelClsError := CVRep.RelClsError / NPoints;
      CVRep.AvgCE := CVRep.AvgCE / (ln(2) * NPoints);
    end;
  CVRep.RMSError := Sqrt(CVRep.RMSError / (NPoints * NOut));
  CVRep.AvgError := CVRep.AvgError / (NPoints * NOut);
  CVRep.AvgRelError := CVRep.AvgRelError / RelCnt;
  Info := 1;
end;

(* ************************************************************************
  Subroutine prepares K-fold split of the training set.

  NOTES:
  "NClasses>0" means that we have classification task.
  "NClasses<0" means regression task with -NClasses real outputs.
  ************************************************************************ *)
procedure MLPKFoldSplit(const xy: TLMatrix; NPoints: TLInt;
NClasses: TLInt; FoldsCount: TLInt; StratifiedSplits: Boolean;
var Folds: TLIVec);
var
  i: TLInt;
  j: TLInt;
  k: TLInt;
begin

  //
  // test parameters
  //
  Assert(NPoints > 0, 'MLPKFoldSplit: wrong NPoints!');
  Assert((NClasses > 1) or (NClasses < 0), 'MLPKFoldSplit: wrong NClasses!');
  Assert((FoldsCount >= 2) and (FoldsCount <= NPoints),
    'MLPKFoldSplit: wrong FoldsCount!');
  Assert(not StratifiedSplits,
    'MLPKFoldSplit: stratified splits are not supported!');

  //
  // Folds
  //
  SetLength(Folds, NPoints);
  i := 0;
  while i <= NPoints - 1 do
    begin
      Folds[i] := i * FoldsCount div NPoints;
      inc(i);
    end;
  i := 0;
  while i <= NPoints - 2 do
    begin
      j := i + RandomInteger(NPoints - i);
      if j <> i then
        begin
          k := Folds[i];
          Folds[i] := Folds[j];
          Folds[j] := k;
        end;
      inc(i);
    end;
end;
