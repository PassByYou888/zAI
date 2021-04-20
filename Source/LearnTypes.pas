{ ****************************************************************************** }
{ * machine Learn types    writen by QQ 600585@qq.com                          * }
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
unit LearnTypes;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, PascalStrings, UnicodeMixedLib, KDTree, KM, DoStatusIO;

type
  TLFloat = TKDTree_VecType;
  PLFloat = PKDTree_VecType;
  TLVec = TKDTree_Vec;
  PLVec = PKDTree_Vec;
  TLMatrix = TKDTree_DynamicVecBuffer;
  PLMatrix = PKDTree_DynamicVecBuffer;

  TLInt = TKMInt;
  PLInt = PKMInt;
  TLIVec = TKMIntegerArray;
  PLIVec = PKMIntegerArray;
  TLIMatrix = array of TLIVec;
  PLIMatrix = ^TLIMatrix;

  TLBVec = array of Boolean;
  PLBVec = ^TLBVec;
  TLBMatrix = array of TLBVec;
  PLBMatrix = ^TLBMatrix;

  TLComplex = record
    x, y: TLFloat;
  end;

  TLComplexVec = array of TLComplex;
  TLComplexMatrix = array of TLComplexVec;

  TLearnType = (
    ltKDT,              // KDTree, fast space operation, this not Neurons network
    ltKM,               // k-means++ clusterization, this not Neurons network
    ltForest,           // random decision forest
    ltLogit,            // Logistic regression
    ltLM,               // Levenberg-Marquardt
    ltLM_MT,            // Levenberg-Marquardt with Parallel
    ltLBFGS,            // L-BFGS
    ltLBFGS_MT,         // L-BFGS with Parallel
    ltLBFGS_MT_Mod,     // L-BFGS with Parallel and optimization
    ltMonteCarlo,       // fast Monte Carlo train
    ltLM_Ensemble,      // Levenberg-Marquardt Ensemble
    ltLM_Ensemble_MT,   // Levenberg-Marquardt Ensemble with Parallel
    ltLBFGS_Ensemble,   // L-BFGS Ensemble
    ltLBFGS_Ensemble_MT // L-BFGS Ensemble with Parallel
    );

  TLearnCommState = record
    Stage: TLInt;
    IA: TLIVec;
    BA: TLBVec;
    ResArry: TLVec;
    ca: TLComplexVec;
  end;

  TMatInvReport = record
    r1: TLFloat;
    RInf: TLFloat;
  end;

  (* ************************************************************************
    Portable high quality random number generator state.
    Initialized with HQRNDRandomize() or HQRNDSeed().

    Fields:
    S1, S2      -   seed values
    V           -   precomputed value
    MagicV      -   'magic' value used to determine whether State structure was correctly initialized.
    ************************************************************************ *)
  THQRNDState = record
    s1: TLInt;
    s2: TLInt;
    v: TLFloat;
    MagicV: TLInt;
  end;

  { * Normalizes direction/step pair * }
  TLinMinState = record
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

  { * Limited memory BFGS optimizer * }
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

  { * Dense linear system solver * }
  TDenseSolverReport = record
    r1: TLFloat;
    RInf: TLFloat;
  end;

  TDenseSolverLSReport = record
    r2: TLFloat;
    Cx: TLMatrix;
    n: TLInt;
    k: TLInt;
  end;

  { * Improved Levenberg-Marquardt optimizer * }
  TMinLMState = record
    WrongParams: Boolean;
    n: TLInt;
    M: TLInt;
    EpsG: TLFloat;
    EpsF: TLFloat;
    EpsX: TLFloat;
    MAXITS: TLInt;
    XRep: Boolean;
    StpMax: TLFloat;
    Flags: TLInt;
    UserMode: TLInt;
    x: TLVec;
    f: TLFloat;
    fi: TLVec;
    j: TLMatrix;
    h: TLMatrix;
    g: TLVec;
    NeedF: Boolean;
    NeedFG: Boolean;
    NeedFGH: Boolean;
    NeedFiJ: Boolean;
    XUpdated: Boolean;
    InternalState: TMinLBFGSState;
    InternalRep: TMinLBFGSReport;
    XPrec: TLVec;
    XBase: TLVec;
    XDir: TLVec;
    GBase: TLVec;
    XPrev: TLVec;
    FPrev: TLFloat;
    RawModel: TLMatrix;
    Model: TLMatrix;
    Work: TLVec;
    RState: TLearnCommState;
    RepIterationsCount: TLInt;
    RepTerminationType: TLInt;
    RepNFunc: TLInt;
    RepNJac: TLInt;
    RepNGrad: TLInt;
    RepNHess: TLInt;
    RepNCholesky: TLInt;
    SolverInfo: TLInt;
    SolverRep: TDenseSolverReport;
    InvInfo: TLInt;
    InvRep: TMatInvReport;
  end;

  TMinLMReport = record
    IterationsCount: TLInt;
    TerminationType: TLInt;
    NFunc: TLInt;
    NJac: TLInt;
    NGrad: TLInt;
    NHess: TLInt;
    NCholesky: TLInt;
  end;

  { * neural network * }
  TMultiLayerPerceptron = record
    StructInfo: TLIVec;
    Weights: TLVec;
    ColumnMeans: TLVec;
    ColumnSigmas: TLVec;
    Neurons: TLVec;
    DFDNET: TLVec;
    DError: TLVec;
    x: TLVec;
    y: TLVec;
    Chunks: TLMatrix;
    NWBuf: TLVec;
  end;

  PMultiLayerPerceptron = ^TMultiLayerPerceptron;

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

  (* ************************************************************************
    Neural networks ensemble
    ************************************************************************ *)
  TMLPEnsemble = record
    StructInfo: TLIVec;
    EnsembleSize: TLInt;
    NIn: TLInt;
    NOut: TLInt;
    WCount: TLInt;
    IsSoftmax: Boolean;
    PostProcessing: Boolean;
    Weights: TLVec;
    ColumnMeans: TLVec;
    ColumnSigmas: TLVec;
    SerializedLen: TLInt;
    SerializedMLP: TLVec;
    TmpWeights: TLVec;
    TmpMeans: TLVec;
    TmpSigmas: TLVec;
    Neurons: TLVec;
    DFDNET: TLVec;
    y: TLVec;
  end;

  PMLPEnsemble = ^TMLPEnsemble;

  { * Random Decision Forest * }
  TDecisionForest = record
    NVars: TLInt;
    NClasses: TLInt;
    NTrees: TLInt;
    BufSize: TLInt;
    Trees: TLVec;
  end;

  PDecisionForest = ^TDecisionForest;

  TDFReport = record
    RelClsError: TLFloat;
    AvgCE: TLFloat;
    RMSError: TLFloat;
    AvgError: TLFloat;
    AvgRelError: TLFloat;
    OOBRelClsError: TLFloat;
    OOBAvgCE: TLFloat;
    OOBRMSError: TLFloat;
    OOBAvgError: TLFloat;
    OOBAvgRelError: TLFloat;
  end;

  { * LogitModel * }
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

  (* ************************************************************************
    Least squares fitting report:
    TaskRCond       reciprocal of task's condition number
    RMSError        RMS error
    AvgError        average error
    AvgRelError     average relative error (for non-zero Y[I])
    MaxError        maximum error
    ************************************************************************ *)
  TLSFitReport = record
    TaskRCond: TLFloat;
    RMSError: TLFloat;
    AvgError: TLFloat;
    AvgRelError: TLFloat;
    MaxError: TLFloat;
  end;

  TLSFitState = record
    n: TLInt;
    M: TLInt;
    k: TLInt;
    EpsF: TLFloat;
    EpsX: TLFloat;
    MAXITS: TLInt;
    StpMax: TLFloat;
    TaskX: TLMatrix;
    TaskY: TLVec;
    w: TLVec;
    CheapFG: Boolean;
    HaveHess: Boolean;
    NeedF: Boolean;
    NeedFG: Boolean;
    NeedFGH: Boolean;
    PointIndex: TLInt;
    x: TLVec;
    c: TLVec;
    f: TLFloat;
    g: TLVec;
    h: TLMatrix;
    RepTerminationType: TLInt;
    RepRMSError: TLFloat;
    RepAvgError: TLFloat;
    RepAvgRelError: TLFloat;
    RepMaxError: TLFloat;
    OptState: TMinLMState;
    OptRep: TMinLMReport;
    RState: TLearnCommState;
  end;

  (* ************************************************************************
    Barycentric interpolant.
    ************************************************************************ *)
  TBarycentricInterpolant = record
    n: TLInt;
    SY: TLFloat;
    x: TLVec;
    y: TLVec;
    w: TLVec;
  end;

  (* ************************************************************************
    Barycentric fitting report:
    TaskRCond       reciprocal of task's condition number
    RMSError        RMS error
    AvgError        average error
    AvgRelError     average relative error (for non-zero Y[I])
    MaxError        maximum error
    ************************************************************************ *)
  TBarycentricFitReport = record
    TaskRCond: TLFloat;
    DBest: TLInt;
    RMSError: TLFloat;
    AvgError: TLFloat;
    AvgRelError: TLFloat;
    MaxError: TLFloat;
  end;

  (* ************************************************************************
    Polynomial fitting report:
    TaskRCond       reciprocal of task's condition number
    RMSError        RMS error
    AvgError        average error
    AvgRelError     average relative error (for non-zero Y[I])
    MaxError        maximum error
    ************************************************************************ *)
  TPolynomialFitReport = record
    TaskRCond: TLFloat;
    RMSError: TLFloat;
    AvgError: TLFloat;
    AvgRelError: TLFloat;
    MaxError: TLFloat;
  end;

  (* ************************************************************************
    1-dimensional spline inteprolant
    ************************************************************************ *)
  TSpline1DInterpolant = record
    Periodic: Boolean;
    n: TLInt;
    k: TLInt;
    x: TLVec;
    c: TLVec;
  end;

  (* ************************************************************************
    Spline fitting report:
    TaskRCond       reciprocal of task's condition number
    RMSError        RMS error
    AvgError        average error
    AvgRelError     average relative error (for non-zero Y[I])
    MaxError        maximum error
    ************************************************************************ *)
  TSpline1DFitReport = record
    TaskRCond: TLFloat;
    RMSError: TLFloat;
    AvgError: TLFloat;
    AvgRelError: TLFloat;
    MaxError: TLFloat;
  end;

  (*
    generates FFT plan
  *)
  TFTPlan = record
    Plan: TLIVec;
    Precomputed: TLVec;
    TmpBuf: TLVec;
    StackBuf: TLVec;
  end;

const
  // IEEE floating
  MachineEpsilon = 5.0E-16;
  MaxRealNumber = 1.0E300;
  MinRealNumber = 1.0E-300;

  // LearnType info
  CLearnString: array [TLearnType] of SystemString = (
    'k-dimensional tree',
    'k-means++ clusterization',
    'Random forest',
    'Logistic regression',
    'Levenberg-Marquardt',
    'Levenberg-Marquardt with Parallel',
    'L-BFGS',
    'L-BFGS with Parallel',
    'L-BFGS with Parallel and optimization',
    'fast Monte Carlo',
    'Levenberg-Marquardt Ensemble',
    'Levenberg-Marquardt Ensemble with Parallel',
    'L-BFGS Ensemble',
    'L-BFGS Ensemble with Parallel'
    );

procedure DoStatus(v: TLVec); overload;
procedure DoStatus(v: TLIVec); overload;
procedure DoStatus(v: TLBVec); overload;
procedure DoStatus(v: TLMatrix); overload;
procedure DoStatus(v: TLIMatrix); overload;
procedure DoStatus(v: TLBMatrix); overload;

implementation

procedure DoStatus(v: TLVec);
var
  i: NativeInt;
begin
  for i := 0 to length(v) - 1 do
      DoStatusNoLn(umlFloatToStr(v[i]) + ' ');
  DoStatusNoLn;
end;

procedure DoStatus(v: TLIVec);
var
  i: NativeInt;
begin
  for i := 0 to length(v) - 1 do
      DoStatusNoLn(umlIntToStr(v[i]) + ' ');
  DoStatusNoLn;
end;

procedure DoStatus(v: TLBVec);
var
  i: NativeInt;
begin
  for i := 0 to length(v) - 1 do
      DoStatusNoLn(umlBoolToStr(v[i]) + ' ');
  DoStatusNoLn;
end;

procedure DoStatus(v: TLMatrix);
var
  i: Integer;
begin
  for i := 0 to high(v) do
      DoStatus(v[i]);
end;

procedure DoStatus(v: TLIMatrix);
var
  i: Integer;
begin
  for i := 0 to high(v) do
      DoStatus(v[i]);
end;

procedure DoStatus(v: TLBMatrix);
var
  i: Integer;
begin
  for i := 0 to high(v) do
      DoStatus(v[i]);
end;

end.
