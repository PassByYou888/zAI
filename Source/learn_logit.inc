type
  TLogitModel = record
    w: TLVec;
  end;

  PLogitModel = ^TLogitModel;

  TLogitMCState = record
    BRACKT: Boolean;
    STAGE1: Boolean;
    INFOC: TLInt;
    DG: TLFloat;
    DGM: TLFloat;
    DGINIT: TLFloat;
    DGTEST: TLFloat;
    DGX: TLFloat;
    DGXM: TLFloat;
    DGY: TLFloat;
    DGYM: TLFloat;
    FINIT: TLFloat;
    FTEST1: TLFloat;
    FM: TLFloat;
    fx: TLFloat;
    FXM: TLFloat;
    fy: TLFloat;
    FYM: TLFloat;
    STX: TLFloat;
    STY: TLFloat;
    STMIN: TLFloat;
    STMAX: TLFloat;
    width: TLFloat;
    WIDTH1: TLFloat;
    XTRAPF: TLFloat;
  end;

  (* ************************************************************************
    MNLReport structure contains information about training process:
    * NGrad     -   number of gradient calculations
    * NHess     -   number of Hessian calculations
    ************************************************************************ *)
  TMNLReport = record
    NGrad: TLInt;
    NHess: TLInt;
  end;

procedure MNLTrainH(const xy: TLMatrix; NPoints: TLInt; NVars: TLInt; NClasses: TLInt;
  var Info: TLInt; var LM: TLogitModel; var Rep: TMNLReport); forward;

procedure MNLProcess(var LM: TLogitModel; const x: TLVec; var y: TLVec); forward;
procedure MNLUnpack(const LM: TLogitModel; var a: TLMatrix; var NVars: TLInt; var NClasses: TLInt); forward;
procedure MNLPack(const a: TLMatrix; NVars: TLInt; NClasses: TLInt; var LM: TLogitModel); forward;
procedure MNLCopy(const LM1: TLogitModel; var LM2: TLogitModel); forward;
procedure MNLSerialize(const LM: TLogitModel; var RA: TLVec; var RLen: TLInt); forward;
procedure MNLUnserialize(const RA: TLVec; var LM: TLogitModel); forward;
function MNLAvgCE(var LM: TLogitModel; const xy: TLMatrix; NPoints: TLInt): TLFloat; forward;
function MNLRelClsError(var LM: TLogitModel; const xy: TLMatrix; NPoints: TLInt): TLFloat; forward;
function MNLRMSError(var LM: TLogitModel; const xy: TLMatrix; NPoints: TLInt): TLFloat; forward;
function MNLAvgError(var LM: TLogitModel; const xy: TLMatrix; NPoints: TLInt): TLFloat; forward;
function MNLAvgRelError(var LM: TLogitModel; const xy: TLMatrix; SSize: TLInt): TLFloat; forward;
function MNLClsError(var LM: TLogitModel; const xy: TLMatrix; NPoints: TLInt): TLInt; forward;


const
  LogitXTOL   = 100 * MachineEpsilon;
  LogitFTOL   = 0.0001;
  LogitGTOL   = 0.3;
  LogitMAXFEV = 20;
  LogitSTPMIN = 1.0E-2;
  LogitSTPMAX = 1.0E5;
  LogitVNum   = 6;

procedure MNLIExp(var w: TLVec; const x: TLVec); forward;

procedure MNLAllErrors(var LM: TLogitModel; const xy: TLMatrix;
  NPoints: TLInt; var RelCls: TLFloat; var AvgCE: TLFloat;
  var RMS: TLFloat; var Avg: TLFloat; var AvgRel: TLFloat); forward;

procedure MNLMCSRCH(const n: TLInt; var x: TLVec;
  var f: TLFloat; var g: TLVec; const s: TLVec;
  var Stp: TLFloat; var Info: TLInt; var NFEV: TLInt;
  var WA: TLVec; var State: TLogitMCState;
  var Stage: TLInt); forward;

procedure MNLMCSTEP(var STX: TLFloat; var fx: TLFloat;
  var dx: TLFloat; var STY: TLFloat; var fy: TLFloat;
  var dy: TLFloat; var Stp: TLFloat; const FP: TLFloat;
  const DP: TLFloat; var BRACKT: Boolean; const STMIN: TLFloat;
  const STMAX: TLFloat; var Info: TLInt); forward;


(* ************************************************************************
  //  This subroutine trains logit model.
  //
  //  INPUT PARAMETERS:
  //  XY          -   training set, array[0..NPoints-1,0..NVars]
  //                  First NVars columns store values of independent
  //                  variables, next column stores number of class (from 0 to NClasses-1) which dataset element belongs to. Fractional
  //                  values are rounded to nearest integer.
  //  NPoints     -   training set size, NPoints>=1
  //  NVars       -   number of independent variables, NVars>=1
  //  NClasses    -   number of classes, NClasses>=2
  //
  //  OUTPUT PARAMETERS:
  //  Info        -   return code:
  //  * -2, if there is a point with class number outside of [0..NClasses-1].
  //  * -1, if incorrect parameters was passed (NPoints<NVars+2, NVars<1, NClasses<2).
  //  *  1, if task has been solved
  //  LM          -   model built
  //  Rep         -   training report
  ************************************************************************ *)
procedure MNLTrainH(const xy: TLMatrix; NPoints: TLInt;
  NVars: TLInt; NClasses: TLInt; var Info: TLInt;
  var LM: TLogitModel; var Rep: TMNLReport);
var
  i         : TLInt;
  j         : TLInt;
  k         : TLInt;
  SSize     : TLInt;
  AllSame   : Boolean;
  Offs      : TLInt;
  Threshold : TLFloat;
  WMinStep  : TLFloat;
  Decay     : TLFloat;
  WDim      : TLInt;
  ExpOffs   : TLInt;
  v         : TLFloat;
  s         : TLFloat;
  Network   : TMultiLayerPerceptron;
  NIn       : TLInt;
  NOut      : TLInt;
  WCount    : TLInt;
  E         : TLFloat;
  g         : TLVec;
  h         : TLMatrix;
  SPD       : Boolean;
  x         : TLVec;
  y         : TLVec;
  WBase     : TLVec;
  WStep     : TLFloat;
  WDir      : TLVec;
  Work      : TLVec;
  MCStage   : TLInt;
  MCState   : TLogitMCState;
  MCINFO    : TLInt;
  MCNFEV    : TLInt;
  SolverInfo: TLInt;
  SolverRep : TDenseSolverReport;
begin
  Threshold := 1000 * MachineEpsilon;
  WMinStep := 0.001;
  Decay := 0.001;

  //
  // Test for inputs
  //
  if (NPoints < NVars + 2) or (NVars < 1) or (NClasses < 2) then
    begin
      Info := -1;
      Exit;
    end;
  i := 0;
  while i <= NPoints - 1 do
    begin
      if (Round(xy[i, NVars]) < 0) or (Round(xy[i, NVars]) >= NClasses) then
        begin
          Info := -2;
          Exit;
        end;
      inc(i);
    end;
  Info := 1;

  //
  // Initialize data
  //
  Rep.NGrad := 0;
  Rep.NHess := 0;

  //
  // Allocate array
  //
  WDim := (NVars + 1) * (NClasses - 1);
  Offs := 5;
  ExpOffs := Offs + WDim;
  SSize := 5 + (NVars + 1) * (NClasses - 1) + NClasses;
  SetLength(LM.w, SSize  );
  LM.w[0] := SSize;
  LM.w[1] := LogitVNum;
  LM.w[2] := NVars;
  LM.w[3] := NClasses;
  LM.w[4] := Offs;

  //
  // Degenerate case: all outputs are equal
  //
  AllSame := True;
  i := 1;
  while i <= NPoints - 1 do
    begin
      if Round(xy[i, NVars]) <> Round(xy[i - 1, NVars]) then
        begin
          AllSame := False;
        end;
      inc(i);
    end;
  if AllSame then
    begin
      i := 0;
      while i <= (NVars + 1) * (NClasses - 1) - 1 do
        begin
          LM.w[Offs + i] := 0;
          inc(i);
        end;
      v := -2 * ln(MinRealNumber);
      k := Round(xy[0, NVars]);
      if k = NClasses - 1 then
        begin
          i := 0;
          while i <= NClasses - 2 do
            begin
              LM.w[Offs + i * (NVars + 1) + NVars] := -v;
              inc(i);
            end;
        end
      else
        begin
          i := 0;
          while i <= NClasses - 2 do
            begin
              if i = k then
                begin
                  LM.w[Offs + i * (NVars + 1) + NVars] := +v;
                end
              else
                begin
                  LM.w[Offs + i * (NVars + 1) + NVars] := 0;
                end;
              inc(i);
            end;
        end;
      Exit;
    end;

  //
  // General case.
  // Prepare task and network. Allocate space.
  //
  MLPCreateC0(NVars, NClasses, Network);
  MLPInitPreprocessor(Network, xy, NPoints);
  MLPProperties(Network, NIn, NOut, WCount);
  i := 0;
  while i <= WCount - 1 do
    begin
      Network.Weights[i] := (2 * RandomReal - 1) / NVars;
      inc(i);
    end;
  SetLength(g, WCount  );
  SetLength(h, WCount  , WCount  );
  SetLength(WBase, WCount  );
  SetLength(WDir, WCount  );
  SetLength(Work, WCount  );

  //
  // First stage: optimize in gradient direction.
  //
  k := 0;
  while k <= WCount div 3 + 10 do
    begin

      //
      // Calculate gradient in starting point
      //
      MLPGradNBatch(Network, xy, NPoints, E, g);
      v := APVDotProduct(@Network.Weights[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
      E := E + 0.5 * Decay * v;
      APVAdd(@g[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1, Decay);
      Rep.NGrad := Rep.NGrad + 1;

      //
      // Setup optimization scheme
      //
      APVMoveNeg(@WDir[0], 0, WCount - 1, @g[0], 0, WCount - 1);
      v := APVDotProduct(@WDir[0], 0, WCount - 1, @WDir[0], 0, WCount - 1);
      WStep := Sqrt(v);
      v := 1 / Sqrt(v);
      APVMul(@WDir[0], 0, WCount - 1, v);
      MCStage := 0;
      MNLMCSRCH(WCount, Network.Weights, E, g, WDir, WStep, MCINFO, MCNFEV, Work, MCState, MCStage);
      while MCStage <> 0 do
        begin
          MLPGradNBatch(Network, xy, NPoints, E, g);
          v := APVDotProduct(@Network.Weights[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1);
          E := E + 0.5 * Decay * v;
          APVAdd(@g[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1, Decay);
          Rep.NGrad := Rep.NGrad + 1;
          MNLMCSRCH(WCount, Network.Weights, E, g, WDir, WStep, MCINFO, MCNFEV, Work, MCState, MCStage);
        end;
      inc(k);
    end;

  //
  // Second stage: use Hessian when we are close to the minimum
  //
  while True do
    begin

      //
      // Calculate and update E/G/H
      //
      MLPHessianNBatch(Network, xy, NPoints, E, g, h);
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
      // Select step direction
      // NOTE: it is important to use lower-triangle Cholesky
      // factorization since it is much faster than higher-triangle version.
      //
      SPD := SPDMatrixCholesky(h, WCount, False);
      SPDMatrixCholeskySolve(h, WCount, False, g, SolverInfo, SolverRep, WDir);
      SPD := SolverInfo > 0;
      if SPD then
        begin

          //
          // H is positive definite.
          // Step in Newton direction.
          //
          APVMul(@WDir[0], 0, WCount - 1, -1);
          SPD := True;
        end
      else
        begin

          //
          // H is indefinite.
          // Step in gradient direction.
          //
          APVMoveNeg(@WDir[0], 0, WCount - 1, @g[0], 0, WCount - 1);
          SPD := False;
        end;

      //
      // Optimize in WDir direction
      //
      v := APVDotProduct(@WDir[0], 0, WCount - 1, @WDir[0], 0, WCount - 1);
      WStep := Sqrt(v);
      v := 1 / Sqrt(v);
      APVMul(@WDir[0], 0, WCount - 1, v);
      MCStage := 0;
      MNLMCSRCH(WCount, Network.Weights, E, g, WDir, WStep, MCINFO, MCNFEV, Work, MCState, MCStage);
      while MCStage <> 0 do
        begin
          MLPGradNBatch(Network, xy, NPoints, E, g);
          v := APVDotProduct(@Network.Weights[0], 0, WCount - 1,
            @Network.Weights[0], 0, WCount - 1);
          E := E + 0.5 * Decay * v;
          APVAdd(@g[0], 0, WCount - 1, @Network.Weights[0], 0, WCount - 1, Decay);
          Rep.NGrad := Rep.NGrad + 1;
          MNLMCSRCH(WCount, Network.Weights, E, g, WDir, WStep, MCINFO, MCNFEV, Work, MCState, MCStage);
        end;
      if SPD and ((MCINFO = 2) or (MCINFO = 4) or (MCINFO = 6)) then
        begin
          Break;
        end;
    end;

  //
  // Convert from NN format to MNL format
  //
  APVMove(@LM.w[0], Offs, Offs + WCount - 1, @Network.Weights[0], 0,
    WCount - 1);
  k := 0;
  while k <= NVars - 1 do
    begin
      i := 0;
      while i <= NClasses - 2 do
        begin
          s := Network.ColumnSigmas[k];
          if AP_FP_Eq(s, 0) then
            begin
              s := 1;
            end;
          j := Offs + (NVars + 1) * i;
          v := LM.w[j + k];
          LM.w[j + k] := v / s;
          LM.w[j + NVars] := LM.w[j + NVars] + v * Network.ColumnMeans[k] / s;
          inc(i);
        end;
      inc(k);
    end;
  k := 0;
  while k <= NClasses - 2 do
    begin
      LM.w[Offs + (NVars + 1) * k + NVars] :=
        -LM.w[Offs + (NVars + 1) * k + NVars];
      inc(k);
    end;
end;

(* ************************************************************************
  Procesing

  INPUT PARAMETERS:
  LM      -   logit model, passed by non-constant reference  (some fields of structure are used as temporaries  when calculating model output).
  X       -   input vector,  array[0..NVars-1].

  OUTPUT PARAMETERS:
  Y       -   result, array[0..NClasses-1]
  Vector of posterior probabilities for classification task.
  Subroutine does not allocate memory for this vector, it is
  responsibility of a caller to allocate it. Array  must  be
  at least [0..NClasses-1].
  ************************************************************************ *)
procedure MNLProcess(var LM: TLogitModel; const x: TLVec; var y: TLVec);
var
  NVars   : TLInt;
  NClasses: TLInt;
  Offs    : TLInt;
  i       : TLInt;
  i1      : TLInt;
  s       : TLFloat;
begin
  Assert(AP_FP_Eq(LM.w[1], LogitVNum), 'MNLProcess: unexpected model version');
  NVars := Round(LM.w[2]);
  NClasses := Round(LM.w[3]);
  Offs := Round(LM.w[4]);
  MNLIExp(LM.w, x);
  s := 0;
  i1 := Offs + (NVars + 1) * (NClasses - 1);
  i := i1;
  while i <= i1 + NClasses - 1 do
    begin
      s := s + LM.w[i];
      inc(i);
    end;
  i := 0;
  while i <= NClasses - 1 do
    begin
      y[i] := LM.w[i1 + i] / s;
      inc(i);
    end;
end;

(* ************************************************************************
  Unpacks coefficients of logit model. Logit model have form:

  P(class=i) = S(i) / (S(0) + S(1) + ... +S(M-1))
  S(i) = Exp(A[i,0]*X[0] + ... + A[i,N-1]*X[N-1] + A[i,N]), when i<M-1
  S(M-1) = 1

  INPUT PARAMETERS:
  LM          -   logit model in ALGLIB format

  OUTPUT PARAMETERS:
  V           -   coefficients, array[0..NClasses-2,0..NVars]
  NVars       -   number of independent variables
  NClasses    -   number of classes
  ************************************************************************ *)
procedure MNLUnpack(const LM: TLogitModel; var a: TLMatrix;
  var NVars: TLInt; var NClasses: TLInt);
var
  Offs: TLInt;
  i   : TLInt;
begin
  Assert(AP_FP_Eq(LM.w[1], LogitVNum), 'MNLUnpack: unexpected model version');
  NVars := Round(LM.w[2]);
  NClasses := Round(LM.w[3]);
  Offs := Round(LM.w[4]);
  SetLength(a, NClasses - 2 + 1, NVars + 1);
  i := 0;
  while i <= NClasses - 2 do
    begin
      APVMove(@a[i][0], 0, NVars, @LM.w[0], Offs + i * (NVars + 1),
        Offs + i * (NVars + 1) + NVars);
      inc(i);
    end;
end;

(* ************************************************************************
  "Packs" coefficients and creates logit model in ALGLIB format (MNLUnpack reversed).

  INPUT PARAMETERS:
  A           -   model (see MNLUnpack)
  NVars       -   number of independent variables
  NClasses    -   number of classes

  OUTPUT PARAMETERS:
  LM          -   logit model.
  ************************************************************************ *)
procedure MNLPack(const a: TLMatrix; NVars: TLInt;
  NClasses: TLInt; var LM: TLogitModel);
var
  Offs : TLInt;
  i    : TLInt;
  WDim : TLInt;
  SSize: TLInt;
begin
  WDim := (NVars + 1) * (NClasses - 1);
  Offs := 5;
  SSize := 5 + (NVars + 1) * (NClasses - 1) + NClasses;
  SetLength(LM.w, SSize  );
  LM.w[0] := SSize;
  LM.w[1] := LogitVNum;
  LM.w[2] := NVars;
  LM.w[3] := NClasses;
  LM.w[4] := Offs;
  i := 0;
  while i <= NClasses - 2 do
    begin
      APVMove(@LM.w[0], Offs + i * (NVars + 1), Offs + i * (NVars + 1) + NVars,
        @a[i][0], 0, NVars);
      inc(i);
    end;
end;

(* ************************************************************************
  Copying of TLogitModel strucure

  INPUT PARAMETERS:
  LM1 -   original

  OUTPUT PARAMETERS:
  LM2 -   copy
  ************************************************************************ *)
procedure MNLCopy(const LM1: TLogitModel; var LM2: TLogitModel);
var
  k: TLInt;
begin
  k := Round(LM1.w[0]);
  SetLength(LM2.w, k  );
  APVMove(@LM2.w[0], 0, k - 1, @LM1.w[0], 0, k - 1);
end;

(* ************************************************************************
  Serialization of TLogitModel strucure

  INPUT PARAMETERS:
  LM      -   original

  OUTPUT PARAMETERS:
  RA      -   array of real numbers which stores model,
  array[0..RLen-1]
  RLen    -   RA lenght
  ************************************************************************ *)
procedure MNLSerialize(const LM: TLogitModel; var RA: TLVec;
  var RLen: TLInt);
begin
  RLen := Round(LM.w[0]) + 1;
  SetLength(RA, RLen  );
  RA[0] := LogitVNum;
  APVMove(@RA[0], 1, RLen - 1, @LM.w[0], 0, RLen - 2);
end;

(* ************************************************************************
  Unserialization of TLogitModel strucure

  INPUT PARAMETERS:
  RA      -   real array which stores model

  OUTPUT PARAMETERS:
  LM      -   restored model
  ************************************************************************ *)
procedure MNLUnserialize(const RA: TLVec; var LM: TLogitModel);
begin
  Assert(Round(RA[0]) = LogitVNum, 'MNLUnserialize: incorrect array!');
  SetLength(LM.w, Round(RA[1])  );
  APVMove(@LM.w[0], 0, Round(RA[1]) - 1, @RA[0], 1, Round(RA[1]));
end;

(* ************************************************************************
  Average cross-entropy (in bits per element) on the test set

  INPUT PARAMETERS:
  LM      -   logit model
  XY      -   test set
  NPoints -   test set size

  RESULT:
  CrossEntropy/(NPoints*ln(2)).
  ************************************************************************ *)
function MNLAvgCE(var LM: TLogitModel; const xy: TLMatrix;
  NPoints: TLInt): TLFloat;
var
  NVars   : TLInt;
  NClasses: TLInt;
  i       : TLInt;
  WorkX   : TLVec;
  WorkY   : TLVec;
begin
  Assert(AP_FP_Eq(LM.w[1], LogitVNum), 'MNLClsError: unexpected model version');
  NVars := Round(LM.w[2]);
  NClasses := Round(LM.w[3]);
  SetLength(WorkX, NVars  );
  SetLength(WorkY, NClasses  );
  Result := 0;
  i := 0;
  while i <= NPoints - 1 do
    begin
      Assert((Round(xy[i, NVars]) >= 0) and (Round(xy[i, NVars]) < NClasses),
        'MNLAvgCE: incorrect class number!');

      //
      // Process
      //
      APVMove(@WorkX[0], 0, NVars - 1, @xy[i][0], 0, NVars - 1);
      MNLProcess(LM, WorkX, WorkY);
      if AP_FP_Greater(WorkY[Round(xy[i, NVars])], 0) then
        begin
          Result := Result - ln(WorkY[Round(xy[i, NVars])]);
        end
      else
        begin
          Result := Result - ln(MinRealNumber);
        end;
      inc(i);
    end;
  Result := Result / (NPoints * ln(2));
end;

(* ************************************************************************
  Relative classification error on the test set

  INPUT PARAMETERS:
  LM      -   logit model
  XY      -   test set
  NPoints -   test set size

  RESULT:
  percent of incorrectly classified cases.
  ************************************************************************ *)
function MNLRelClsError(var LM: TLogitModel; const xy: TLMatrix;
  NPoints: TLInt): TLFloat;
begin
  Result := AP_Float(MNLClsError(LM, xy, NPoints)) / NPoints;
end;

(* ************************************************************************
  RMS error on the test set

  INPUT PARAMETERS:
  LM      -   logit model
  XY      -   test set
  NPoints -   test set size

  RESULT:
  root mean square error (error when estimating posterior probabilities).
  ************************************************************************ *)
function MNLRMSError(var LM: TLogitModel; const xy: TLMatrix;
  NPoints: TLInt): TLFloat;
var
  RelCls: TLFloat;
  AvgCE : TLFloat;
  RMS   : TLFloat;
  Avg   : TLFloat;
  AvgRel: TLFloat;
begin
  Assert(Round(LM.w[1]) = LogitVNum, 'MNLRMSError: Incorrect MNL version!');
  MNLAllErrors(LM, xy, NPoints, RelCls, AvgCE, RMS, Avg, AvgRel);
  Result := RMS;
end;

(* ************************************************************************
  Average error on the test set

  INPUT PARAMETERS:
  LM      -   logit model
  XY      -   test set
  NPoints -   test set size

  RESULT:
  average error (error when estimating posterior probabilities).
  ************************************************************************ *)
function MNLAvgError(var LM: TLogitModel; const xy: TLMatrix;
  NPoints: TLInt): TLFloat;
var
  RelCls: TLFloat;
  AvgCE : TLFloat;
  RMS   : TLFloat;
  Avg   : TLFloat;
  AvgRel: TLFloat;
begin
  Assert(Round(LM.w[1]) = LogitVNum, 'MNLRMSError: Incorrect MNL version!');
  MNLAllErrors(LM, xy, NPoints, RelCls, AvgCE, RMS, Avg, AvgRel);
  Result := Avg;
end;

(* ************************************************************************
  Average relative error on the test set

  INPUT PARAMETERS:
  LM      -   logit model
  XY      -   test set
  NPoints -   test set size

  RESULT:
  average relative error (error when estimating posterior probabilities).
  ************************************************************************ *)
function MNLAvgRelError(var LM: TLogitModel; const xy: TLMatrix;
  SSize: TLInt): TLFloat;
var
  RelCls: TLFloat;
  AvgCE : TLFloat;
  RMS   : TLFloat;
  Avg   : TLFloat;
  AvgRel: TLFloat;
begin
  Assert(Round(LM.w[1]) = LogitVNum, 'MNLRMSError: Incorrect MNL version!');
  MNLAllErrors(LM, xy, SSize, RelCls, AvgCE, RMS, Avg, AvgRel);
  Result := AvgRel;
end;

(* ************************************************************************
  Classification error on test set = MNLRelClsError*NPoints
  ************************************************************************ *)
function MNLClsError(var LM: TLogitModel; const xy: TLMatrix;
  NPoints: TLInt): TLInt;
var
  NVars   : TLInt;
  NClasses: TLInt;
  i       : TLInt;
  j       : TLInt;
  WorkX   : TLVec;
  WorkY   : TLVec;
  NMAX    : TLInt;
begin
  Assert(AP_FP_Eq(LM.w[1], LogitVNum), 'MNLClsError: unexpected model version');
  NVars := Round(LM.w[2]);
  NClasses := Round(LM.w[3]);
  SetLength(WorkX, NVars  );
  SetLength(WorkY, NClasses  );
  Result := 0;
  i := 0;
  while i <= NPoints - 1 do
    begin

      //
      // Process
      //
      APVMove(@WorkX[0], 0, NVars - 1, @xy[i][0], 0, NVars - 1);
      MNLProcess(LM, WorkX, WorkY);

      //
      // Logit version of the answer
      //
      NMAX := 0;
      j := 0;
      while j <= NClasses - 1 do
        begin
          if AP_FP_Greater(WorkY[j], WorkY[NMAX]) then
            begin
              NMAX := j;
            end;
          inc(j);
        end;

      //
      // compare
      //
      if NMAX <> Round(xy[i, NVars]) then
        begin
          Result := Result + 1;
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Internal subroutine. Places exponents of the anti-overflow shifted
  internal linear outputs into the service part of the W array.
  ************************************************************************ *)
procedure MNLIExp(var w: TLVec; const x: TLVec);
var
  NVars   : TLInt;
  NClasses: TLInt;
  Offs    : TLInt;
  i       : TLInt;
  i1      : TLInt;
  v       : TLFloat;
  mx      : TLFloat;
begin
  Assert(AP_FP_Eq(w[1], LogitVNum), 'LOGIT: unexpected model version');
  NVars := Round(w[2]);
  NClasses := Round(w[3]);
  Offs := Round(w[4]);
  i1 := Offs + (NVars + 1) * (NClasses - 1);
  i := 0;
  while i <= NClasses - 2 do
    begin
      v := APVDotProduct(@w[0], Offs + i * (NVars + 1), Offs + i * (NVars + 1) +
        NVars - 1, @x[0], 0, NVars - 1);
      w[i1 + i] := v + w[Offs + i * (NVars + 1) + NVars];
      inc(i);
    end;
  w[i1 + NClasses - 1] := 0;
  mx := 0;
  i := i1;
  while i <= i1 + NClasses - 1 do
    begin
      mx := Max(mx, w[i]);
      inc(i);
    end;
  i := i1;
  while i <= i1 + NClasses - 1 do
    begin
      w[i] := Exp(w[i] - mx);
      inc(i);
    end;
end;

(* ************************************************************************
  Calculation of all types of errors
  ************************************************************************ *)
procedure MNLAllErrors(var LM: TLogitModel; const xy: TLMatrix;
  NPoints: TLInt; var RelCls: TLFloat; var AvgCE: TLFloat;
  var RMS: TLFloat; var Avg: TLFloat; var AvgRel: TLFloat);
var
  NVars   : TLInt;
  NClasses: TLInt;
  i       : TLInt;
  Buf     : TLVec;
  WorkX   : TLVec;
  y       : TLVec;
  dy      : TLVec;
begin
  Assert(Round(LM.w[1]) = LogitVNum, 'MNL unit: Incorrect MNL version!');
  NVars := Round(LM.w[2]);
  NClasses := Round(LM.w[3]);
  SetLength(WorkX, NVars  );
  SetLength(y, NClasses  );
  SetLength(dy, 0 + 1);
  DSErrAllocate(NClasses, Buf);
  i := 0;
  while i <= NPoints - 1 do
    begin
      APVMove(@WorkX[0], 0, NVars - 1, @xy[i][0], 0, NVars - 1);
      MNLProcess(LM, WorkX, y);
      dy[0] := xy[i, NVars];
      DSErrAccumulate(Buf, y, dy);
      inc(i);
    end;
  DSErrFinish(Buf);
  RelCls := Buf[0];
  AvgCE := Buf[1];
  RMS := Buf[2];
  Avg := Buf[3];
  AvgRel := Buf[4];
end;

(* ************************************************************************
  THE  PURPOSE  OF  MCSRCH  IS  TO  FIND A STEP WHICH SATISFIES A SUFFICIENT
  DECREASE CONDITION AND A CURVATURE CONDITION.

  AT EACH STAGE THE SUBROUTINE  UPDATES  AN  INTERVAL  OF  UNCERTAINTY  WITH
  ENDPOINTS  STX  AND  STY.  THE INTERVAL OF UNCERTAINTY IS INITIALLY CHOSEN
  SO THAT IT CONTAINS A MINIMIZER OF THE MODIFIED FUNCTION

  F(X+STP*S) - F(X) - LogitFTOL*STP*(GRADF(X)'S).

  IF  A STEP  IS OBTAINED FOR  WHICH THE MODIFIED FUNCTION HAS A NONPOSITIVE
  FUNCTION  VALUE  AND  NONNEGATIVE  DERIVATIVE,   THEN   THE   INTERVAL  OF
  UNCERTAINTY IS CHOSEN SO THAT IT CONTAINS A MINIMIZER OF F(X+STP*S).

  THE  ALGORITHM  IS  DESIGNED TO FIND A STEP WHICH SATISFIES THE SUFFICIENT
  DECREASE CONDITION

  F(X+STP*S) .LE. F(X) + LogitFTOL*STP*(GRADF(X)'S),

  AND THE CURVATURE CONDITION

  ABS(GRADF(X+STP*S)'S)) .LE. LogitGTOL*ABS(GRADF(X)'S).

  IF  LogitFTOL  IS  LESS  THAN LogitGTOL AND IF, FOR EXAMPLE, THE FUNCTION IS BOUNDED
  BELOW,  THEN  THERE  IS  ALWAYS  A  STEP  WHICH SATISFIES BOTH CONDITIONS.
  IF  NO  STEP  CAN BE FOUND  WHICH  SATISFIES  BOTH  CONDITIONS,  THEN  THE
  ALGORITHM  USUALLY STOPS  WHEN  ROUNDING ERRORS  PREVENT FURTHER PROGRESS.
  IN THIS CASE STP ONLY SATISFIES THE SUFFICIENT DECREASE CONDITION.

  PARAMETERS DESCRIPRION

  N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER OF VARIABLES.

  X IS  AN  ARRAY  OF  LENGTH N. ON INPUT IT MUST CONTAIN THE BASE POINT FOR
  THE LINE SEARCH. ON OUTPUT IT CONTAINS X+STP*S.

  F IS  A  VARIABLE. ON INPUT IT MUST CONTAIN THE VALUE OF F AT X. ON OUTPUT
  IT CONTAINS THE VALUE OF F AT X + STP*S.

  G IS AN array of LENGTH N. ON INPUT IT MUST CONTAIN THE GRADIENT OF F AT X.
  ON OUTPUT IT CONTAINS THE GRADIENT OF F AT X + STP*S.

  S IS AN INPUT array of LENGTH N WHICH SPECIFIES THE SEARCH DIRECTION.

  STP  IS  A NONNEGATIVE VARIABLE. ON INPUT STP CONTAINS AN INITIAL ESTIMATE
  OF A SATISFACTORY STEP. ON OUTPUT STP CONTAINS THE FINAL ESTIMATE.

  LogitFTOL AND LogitGTOL ARE NONNEGATIVE INPUT VARIABLES. TERMINATION OCCURS WHEN THE
  SUFFICIENT DECREASE CONDITION AND THE DIRECTIONAL DERIVATIVE CONDITION ARE
  SATISFIED.

  LogitXTOL IS A NONNEGATIVE INPUT VARIABLE. TERMINATION OCCURS WHEN THE RELATIVE
  WIDTH OF THE INTERVAL OF UNCERTAINTY IS AT MOST LogitXTOL.

  LogitSTPMIN AND LogitSTPMAX ARE NONNEGATIVE INPUT VARIABLES WHICH SPECIFY LOWER  AND
  UPPER BOUNDS FOR THE STEP.

  LogitMAXFEV IS A POSITIVE INTEGER INPUT VARIABLE. TERMINATION OCCURS WHEN THE
  NUMBER OF CALLS TO FCN IS AT LEAST LogitMAXFEV BY THE END OF AN ITERATION.

  INFO IS AN INTEGER OUTPUT VARIABLE SET AS FOLLOWS:
  INFO = 0  IMPROPER INPUT PARAMETERS.

  INFO = 1  THE SUFFICIENT DECREASE CONDITION AND THE
  DIRECTIONAL DERIVATIVE CONDITION HOLD.

  INFO = 2  RELATIVE WIDTH OF THE INTERVAL OF UNCERTAINTY
  IS AT MOST LogitXTOL.

  INFO = 3  NUMBER OF CALLS TO FCN HAS REACHED LogitMAXFEV.

  INFO = 4  THE STEP IS AT THE LOWER BOUND LogitSTPMIN.

  INFO = 5  THE STEP IS AT THE UPPER BOUND LogitSTPMAX.

  INFO = 6  ROUNDING ERRORS PREVENT FURTHER PROGRESS.
  THERE MAY NOT BE A STEP WHICH SATISFIES THE
  SUFFICIENT DECREASE AND CURVATURE CONDITIONS.
  TOLERANCES MAY BE TOO SMALL.

  NFEV IS AN INTEGER OUTPUT VARIABLE SET TO THE NUMBER OF CALLS TO FCN.

  WA IS A WORK array of LENGTH N.

  ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. JUNE 1983
  JORGE J. MORE', DAVID J. THUENTE
  ************************************************************************ *)
procedure MNLMCSRCH(const n: TLInt; var x: TLVec;
  var f: TLFloat; var g: TLVec; const s: TLVec;
  var Stp: TLFloat; var Info: TLInt; var NFEV: TLInt;
  var WA: TLVec; var State: TLogitMCState; var Stage: TLInt);
var
  v   : TLFloat;
  P5  : TLFloat;
  P66 : TLFloat;
  Zero: TLFloat;
begin

  //
  // init
  //
  P5 := 0.5;
  P66 := 0.66;
  State.XTRAPF := 4.0;
  Zero := 0;

  //
  // Main cycle
  //
  while True do
    begin
      if Stage = 0 then
        begin

          //
          // NEXT
          //
          Stage := 2;
          Continue;
        end;
      if Stage = 2 then
        begin
          State.INFOC := 1;
          Info := 0;

          //
          // CHECK THE INPUT PARAMETERS FOR ERRORS.
          //
          if (n <= 0) or AP_FP_Less_Eq(Stp, 0) or AP_FP_Less(LogitFTOL, 0) or
            AP_FP_Less(LogitGTOL, Zero) or AP_FP_Less(LogitXTOL, Zero) or
            AP_FP_Less(LogitSTPMIN, Zero) or AP_FP_Less(LogitSTPMAX, LogitSTPMIN) or (LogitMAXFEV <= 0)
          then
            begin
              Stage := 0;
              Exit;
            end;

          //
          // COMPUTE THE INITIAL GRADIENT IN THE SEARCH DIRECTION
          // AND CHECK THAT S IS A DESCENT DIRECTION.
          //
          v := APVDotProduct(@g[0], 0, n - 1, @s[0], 0, n - 1);
          State.DGINIT := v;
          if AP_FP_Greater_Eq(State.DGINIT, 0) then
            begin
              Stage := 0;
              Exit;
            end;

          //
          // INITIALIZE LOCAL VARIABLES.
          //
          State.BRACKT := False;
          State.STAGE1 := True;
          NFEV := 0;
          State.FINIT := f;
          State.DGTEST := LogitFTOL * State.DGINIT;
          State.width := LogitSTPMAX - LogitSTPMIN;
          State.WIDTH1 := State.width / P5;
          APVMove(@WA[0], 0, n - 1, @x[0], 0, n - 1);

          //
          // THE VARIABLES STX, FX, DGX CONTAIN THE VALUES OF THE STEP,
          // FUNCTION, AND DIRECTIONAL DERIVATIVE AT THE BEST STEP.
          // THE VARIABLES STY, FY, DGY CONTAIN THE VALUE OF THE STEP,
          // FUNCTION, AND DERIVATIVE AT THE OTHER ENDPOINT OF
          // THE INTERVAL OF UNCERTAINTY.
          // THE VARIABLES STP, F, DG CONTAIN THE VALUES OF THE STEP,
          // FUNCTION, AND DERIVATIVE AT THE CURRENT STEP.
          //
          State.STX := 0;
          State.fx := State.FINIT;
          State.DGX := State.DGINIT;
          State.STY := 0;
          State.fy := State.FINIT;
          State.DGY := State.DGINIT;

          //
          // NEXT
          //
          Stage := 3;
          Continue;
        end;
      if Stage = 3 then
        begin

          //
          // START OF ITERATION.
          //
          // SET THE MINIMUM AND MAXIMUM STEPS TO CORRESPOND
          // TO THE PRESENT INTERVAL OF UNCERTAINTY.
          //
          if State.BRACKT then
            begin
              if AP_FP_Less(State.STX, State.STY) then
                begin
                  State.STMIN := State.STX;
                  State.STMAX := State.STY;
                end
              else
                begin
                  State.STMIN := State.STY;
                  State.STMAX := State.STX;
                end;
            end
          else
            begin
              State.STMIN := State.STX;
              State.STMAX := Stp + State.XTRAPF * (Stp - State.STX);
            end;

          //
          // FORCE THE STEP TO BE WITHIN THE BOUNDS LogitSTPMAX AND LogitSTPMIN.
          //
          if AP_FP_Greater(Stp, LogitSTPMAX) then
            begin
              Stp := LogitSTPMAX;
            end;
          if AP_FP_Less(Stp, LogitSTPMIN) then
            begin
              Stp := LogitSTPMIN;
            end;

          //
          // IF AN UNUSUAL TERMINATION IS TO OCCUR THEN LET
          // STP BE THE LOWEST POINT OBTAINED SO FAR.
          //
          if State.BRACKT and (AP_FP_Less_Eq(Stp, State.STMIN) or
            AP_FP_Greater_Eq(Stp, State.STMAX)) or (NFEV >= LogitMAXFEV - 1) or
            (State.INFOC = 0) or State.BRACKT and
            AP_FP_Less_Eq(State.STMAX - State.STMIN, LogitXTOL * State.STMAX) then
            begin
              Stp := State.STX;
            end;

          //
          // EVALUATE THE FUNCTION AND GRADIENT AT STP
          // AND COMPUTE THE DIRECTIONAL DERIVATIVE.
          //
          APVMove(@x[0], 0, n - 1, @WA[0], 0, n - 1);
          APVAdd(@x[0], 0, n - 1, @s[0], 0, n - 1, Stp);

          //
          // NEXT
          //
          Stage := 4;
          Exit;
        end;
      if Stage = 4 then
        begin
          Info := 0;
          NFEV := NFEV + 1;
          v := APVDotProduct(@g[0], 0, n - 1, @s[0], 0, n - 1);
          State.DG := v;
          State.FTEST1 := State.FINIT + Stp * State.DGTEST;

          //
          // TEST FOR CONVERGENCE.
          //
          if State.BRACKT and (AP_FP_Less_Eq(Stp, State.STMIN) or
            AP_FP_Greater_Eq(Stp, State.STMAX)) or (State.INFOC = 0) then
            begin
              Info := 6;
            end;
          if AP_FP_Eq(Stp, LogitSTPMAX) and AP_FP_Less_Eq(f, State.FTEST1) and
            AP_FP_Less_Eq(State.DG, State.DGTEST) then
            begin
              Info := 5;
            end;
          if AP_FP_Eq(Stp, LogitSTPMIN) and (AP_FP_Greater(f, State.FTEST1) or
            AP_FP_Greater_Eq(State.DG, State.DGTEST)) then
            begin
              Info := 4;
            end;
          if NFEV >= LogitMAXFEV then
            begin
              Info := 3;
            end;
          if State.BRACKT and AP_FP_Less_Eq(State.STMAX - State.STMIN,
            LogitXTOL * State.STMAX) then
            begin
              Info := 2;
            end;
          if AP_FP_Less_Eq(f, State.FTEST1) and AP_FP_Less_Eq(AbsReal(State.DG),
            -LogitGTOL * State.DGINIT) then
            begin
              Info := 1;
            end;

          //
          // CHECK FOR TERMINATION.
          //
          if Info <> 0 then
            begin
              Stage := 0;
              Exit;
            end;

          //
          // IN THE FIRST STAGE WE SEEK A STEP FOR WHICH THE MODIFIED
          // FUNCTION HAS A NONPOSITIVE VALUE AND NONNEGATIVE DERIVATIVE.
          //
          if State.STAGE1 and AP_FP_Less_Eq(f, State.FTEST1) and
            AP_FP_Greater_Eq(State.DG, Min(LogitFTOL, LogitGTOL) * State.DGINIT) then
            begin
              State.STAGE1 := False;
            end;

          //
          // A MODIFIED FUNCTION IS USED TO PREDICT THE STEP ONLY IF
          // WE HAVE NOT OBTAINED A STEP FOR WHICH THE MODIFIED
          // FUNCTION HAS A NONPOSITIVE FUNCTION VALUE AND NONNEGATIVE
          // DERIVATIVE, AND IF A LOWER FUNCTION VALUE HAS BEEN
          // OBTAINED BUT THE DECREASE IS NOT SUFFICIENT.
          //
          if State.STAGE1 and AP_FP_Less_Eq(f, State.fx) and
            AP_FP_Greater(f, State.FTEST1) then
            begin

              //
              // DEFINE THE MODIFIED FUNCTION AND DERIVATIVE VALUES.
              //
              State.FM := f - Stp * State.DGTEST;
              State.FXM := State.fx - State.STX * State.DGTEST;
              State.FYM := State.fy - State.STY * State.DGTEST;
              State.DGM := State.DG - State.DGTEST;
              State.DGXM := State.DGX - State.DGTEST;
              State.DGYM := State.DGY - State.DGTEST;

              //
              // CALL CSTEP TO UPDATE THE INTERVAL OF UNCERTAINTY
              // AND TO COMPUTE THE NEW STEP.
              //
              MNLMCSTEP(State.STX, State.FXM, State.DGXM, State.STY, State.FYM,
                State.DGYM, Stp, State.FM, State.DGM, State.BRACKT, State.STMIN,
                State.STMAX, State.INFOC);

              //
              // RESET THE FUNCTION AND GRADIENT VALUES FOR F.
              //
              State.fx := State.FXM + State.STX * State.DGTEST;
              State.fy := State.FYM + State.STY * State.DGTEST;
              State.DGX := State.DGXM + State.DGTEST;
              State.DGY := State.DGYM + State.DGTEST;
            end
          else
            begin

              //
              // CALL MCSTEP TO UPDATE THE INTERVAL OF UNCERTAINTY
              // AND TO COMPUTE THE NEW STEP.
              //
              MNLMCSTEP(State.STX, State.fx, State.DGX, State.STY, State.fy,
                State.DGY, Stp, f, State.DG, State.BRACKT, State.STMIN, State.STMAX,
                State.INFOC);
            end;

          //
          // FORCE A SUFFICIENT DECREASE IN THE SIZE OF THE
          // INTERVAL OF UNCERTAINTY.
          //
          if State.BRACKT then
            begin
              if AP_FP_Greater_Eq(AbsReal(State.STY - State.STX), P66 * State.WIDTH1)
              then
                begin
                  Stp := State.STX + P5 * (State.STY - State.STX);
                end;
              State.WIDTH1 := State.width;
              State.width := AbsReal(State.STY - State.STX);
            end;

          //
          // NEXT.
          //
          Stage := 3;
          Continue;
        end;
    end;
end;

procedure MNLMCSTEP(var STX: TLFloat; var fx: TLFloat;
  var dx: TLFloat; var STY: TLFloat; var fy: TLFloat;
  var dy: TLFloat; var Stp: TLFloat; const FP: TLFloat;
  const DP: TLFloat; var BRACKT: Boolean; const STMIN: TLFloat;
  const STMAX: TLFloat; var Info: TLInt);
var
  Bound: Boolean;
  Gamma: TLFloat;
  p    : TLFloat;
  q    : TLFloat;
  r    : TLFloat;
  s    : TLFloat;
  SGND : TLFloat;
  STPC : TLFloat;
  STPF : TLFloat;
  STPQ : TLFloat;
  Theta: TLFloat;
begin
  Info := 0;

  //
  // CHECK THE INPUT PARAMETERS FOR ERRORS.
  //
  if BRACKT and (AP_FP_Less_Eq(Stp, Min(STX, STY)) or AP_FP_Greater_Eq(Stp,
    Max(STX, STY))) or AP_FP_Greater_Eq(dx * (Stp - STX), 0) or
    AP_FP_Less(STMAX, STMIN) then
    begin
      Exit;
    end;

  //
  // DETERMINE IF THE DERIVATIVES HAVE OPPOSITE SIGN.
  //
  SGND := DP * (dx / AbsReal(dx));

  //
  // FIRST CASE. A HIGHER FUNCTION VALUE.
  // THE MINIMUM IS BRACKETED. IF THE CUBIC STEP IS CLOSER
  // TO STX THAN THE QUADRATIC STEP, THE CUBIC STEP IS TAKEN,
  // ELSE THE AVERAGE OF THE CUBIC AND QUADRATIC STEPS IS TAKEN.
  //
  if AP_FP_Greater(FP, fx) then
    begin
      Info := 1;
      Bound := True;
      Theta := 3 * (fx - FP) / (Stp - STX) + dx + DP;
      s := Max(AbsReal(Theta), Max(AbsReal(dx), AbsReal(DP)));
      Gamma := s * Sqrt(AP_Sqr(Theta / s) - dx / s * (DP / s));
      if AP_FP_Less(Stp, STX) then
        begin
          Gamma := -Gamma;
        end;
      p := Gamma - dx + Theta;
      q := Gamma - dx + Gamma + DP;
      r := p / q;
      STPC := STX + r * (Stp - STX);
      STPQ := STX + dx / ((fx - FP) / (Stp - STX) + dx) / 2 * (Stp - STX);
      if AP_FP_Less(AbsReal(STPC - STX), AbsReal(STPQ - STX)) then
        begin
          STPF := STPC;
        end
      else
        begin
          STPF := STPC + (STPQ - STPC) / 2;
        end;
      BRACKT := True;
    end
  else
    begin
      if AP_FP_Less(SGND, 0) then
        begin

          //
          // SECOND CASE. A LOWER FUNCTION VALUE AND DERIVATIVES OF
          // OPPOSITE SIGN. THE MINIMUM IS BRACKETED. IF THE CUBIC
          // STEP IS CLOSER TO STX THAN THE QUADRATIC (SECANT) STEP,
          // THE CUBIC STEP IS TAKEN, ELSE THE QUADRATIC STEP IS TAKEN.
          //
          Info := 2;
          Bound := False;
          Theta := 3 * (fx - FP) / (Stp - STX) + dx + DP;
          s := Max(AbsReal(Theta), Max(AbsReal(dx), AbsReal(DP)));
          Gamma := s * Sqrt(AP_Sqr(Theta / s) - dx / s * (DP / s));
          if AP_FP_Greater(Stp, STX) then
            begin
              Gamma := -Gamma;
            end;
          p := Gamma - DP + Theta;
          q := Gamma - DP + Gamma + dx;
          r := p / q;
          STPC := Stp + r * (STX - Stp);
          STPQ := Stp + DP / (DP - dx) * (STX - Stp);
          if AP_FP_Greater(AbsReal(STPC - Stp), AbsReal(STPQ - Stp)) then
            begin
              STPF := STPC;
            end
          else
            begin
              STPF := STPQ;
            end;
          BRACKT := True;
        end
      else
        begin
          if AP_FP_Less(AbsReal(DP), AbsReal(dx)) then
            begin

              //
              // THIRD CASE. A LOWER FUNCTION VALUE, DERIVATIVES OF THE
              // SAME SIGN, AND THE MAGNITUDE OF THE DERIVATIVE DECREASES.
              // THE CUBIC STEP IS ONLY USED IF THE CUBIC TENDS TO INFINITY
              // IN THE DIRECTION OF THE STEP OR IF THE MINIMUM OF THE CUBIC
              // IS BEYOND STP. OTHERWISE THE CUBIC STEP IS DEFINED TO BE
              // EITHER LogitSTPMIN OR LogitSTPMAX. THE QUADRATIC (SECANT) STEP IS ALSO
              // COMPUTED AND IF THE MINIMUM IS BRACKETED THEN THE THE STEP
              // CLOSEST TO STX IS TAKEN, ELSE THE STEP FARTHEST AWAY IS TAKEN.
              //
              Info := 3;
              Bound := True;
              Theta := 3 * (fx - FP) / (Stp - STX) + dx + DP;
              s := Max(AbsReal(Theta), Max(AbsReal(dx), AbsReal(DP)));

              //
              // THE CASE GAMMA = 0 ONLY ARISES IF THE CUBIC DOES NOT TEND
              // TO INFINITY IN THE DIRECTION OF THE STEP.
              //
              Gamma := s * Sqrt(Max(0, AP_Sqr(Theta / s) - dx / s * (DP / s)));
              if AP_FP_Greater(Stp, STX) then
                begin
                  Gamma := -Gamma;
                end;
              p := Gamma - DP + Theta;
              q := Gamma + (dx - DP) + Gamma;
              r := p / q;
              if AP_FP_Less(r, 0) and AP_FP_NEq(Gamma, 0) then
                begin
                  STPC := Stp + r * (STX - Stp);
                end
              else
                begin
                  if AP_FP_Greater(Stp, STX) then
                    begin
                      STPC := STMAX;
                    end
                  else
                    begin
                      STPC := STMIN;
                    end;
                end;
              STPQ := Stp + DP / (DP - dx) * (STX - Stp);
              if BRACKT then
                begin
                  if AP_FP_Less(AbsReal(Stp - STPC), AbsReal(Stp - STPQ)) then
                    begin
                      STPF := STPC;
                    end
                  else
                    begin
                      STPF := STPQ;
                    end;
                end
              else
                begin
                  if AP_FP_Greater(AbsReal(Stp - STPC), AbsReal(Stp - STPQ)) then
                    begin
                      STPF := STPC;
                    end
                  else
                    begin
                      STPF := STPQ;
                    end;
                end;
            end
          else
            begin

              //
              // FOURTH CASE. A LOWER FUNCTION VALUE, DERIVATIVES OF THE
              // SAME SIGN, AND THE MAGNITUDE OF THE DERIVATIVE DOES
              // NOT DECREASE. IF THE MINIMUM IS NOT BRACKETED, THE STEP
              // IS EITHER LogitSTPMIN OR LogitSTPMAX, ELSE THE CUBIC STEP IS TAKEN.
              //
              Info := 4;
              Bound := False;
              if BRACKT then
                begin
                  Theta := 3 * (FP - fy) / (STY - Stp) + dy + DP;
                  s := Max(AbsReal(Theta), Max(AbsReal(dy), AbsReal(DP)));
                  Gamma := s * Sqrt(AP_Sqr(Theta / s) - dy / s * (DP / s));
                  if AP_FP_Greater(Stp, STY) then
                    begin
                      Gamma := -Gamma;
                    end;
                  p := Gamma - DP + Theta;
                  q := Gamma - DP + Gamma + dy;
                  r := p / q;
                  STPC := Stp + r * (STY - Stp);
                  STPF := STPC;
                end
              else
                begin
                  if AP_FP_Greater(Stp, STX) then
                    begin
                      STPF := STMAX;
                    end
                  else
                    begin
                      STPF := STMIN;
                    end;
                end;
            end;
        end;
    end;

  //
  // UPDATE THE INTERVAL OF UNCERTAINTY. THIS UPDATE DOES NOT
  // DEPEND ON THE NEW STEP OR THE CASE ANALYSIS ABOVE.
  //
  if AP_FP_Greater(FP, fx) then
    begin
      STY := Stp;
      fy := FP;
      dy := DP;
    end
  else
    begin
      if AP_FP_Less(SGND, 0.0) then
        begin
          STY := STX;
          fy := fx;
          dy := dx;
        end;
      STX := Stp;
      fx := FP;
      dx := DP;
    end;

  //
  // COMPUTE THE NEW STEP AND SAFEGUARD IT.
  //
  STPF := Min(STMAX, STPF);
  STPF := Max(STMIN, STPF);
  Stp := STPF;
  if BRACKT and Bound then
    begin
      if AP_FP_Greater(STY, STX) then
        begin
          Stp := Min(STX + 0.66 * (STY - STX), Stp);
        end
      else
        begin
          Stp := Max(STX + 0.66 * (STY - STX), Stp);
        end;
    end;
end; 
 
 
