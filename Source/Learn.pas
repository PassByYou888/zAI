{ ****************************************************************************** }
{ * machine Learn, writen by QQ 600585@qq.com                                  * }
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
unit Learn;

{$INCLUDE zDefine.inc}

interface

uses Math, CoreClasses, UnicodeMixedLib, PascalStrings, KDTree, LearnTypes, MemoryStream64, DataFrameEngine, ListEngine,
  Geometry2DUnit, Geometry3DUnit;

{$REGION 'Class'}


type
  TLearn = class;

  TLearnState_Call = procedure(const LSender: TLearn; const State: Boolean);
  TLearnState_Method = procedure(const LSender: TLearn; const State: Boolean) of object;
{$IFDEF FPC}
  TLearnState_Proc = procedure(const LSender: TLearn; const State: Boolean) is nested;
{$ELSE FPC}
  TLearnState_Proc = reference to procedure(const LSender: TLearn; const State: Boolean);
{$ENDIF FPC}

  TLearnMemory = record
    m_in, m_out: TLVec;
    token: TPascalString;
  end;

  PLearnMemory = ^TLearnMemory;

  TLearn = class(TCoreClassInterfacedObject)
  public type
    TLearnKDT = record
      K: TKDTree;
    end;

    PLearnKDT = ^TLearnKDT;
    THideLayerDepth = (hld0, hld1, hld2);
  private
    FRandomNumber: Boolean;
    FInSize, FOutSize: TLInt;
    FMemorySource: TCoreClassList;
    FTokenCache: THashList;
    FKDToken: TKDTree;
    FLearnType: TLearnType;
    FLearnData: Pointer;
    FClassifier: Boolean;
    FHideLayerDepth: THideLayerDepth;
    FLastTrainMaxInValue, FLastTrainMaxOutValue: TLFloat;
    FInfo: TPascalString;
    FIsTraining: Boolean;
    FTrainingThreadRuning: Boolean;
    FUserData: Pointer;
    FUserObject: TCoreClassObject;

    procedure KDInput(const IndexFor: NativeInt; var Source: TKDTree_Source; const Data: Pointer);
    procedure TokenInput(const IndexFor: NativeInt; var Source: TKDTree_Source; const Data: Pointer);

    procedure FreeLearnData;
    procedure CreateLearnData(const isTrainingTime: Boolean);
  public
    { regression style }
    class function CreateRegression(const lt: TLearnType; const InDataLen, OutDataLen: TLInt): TLearn;
    { regression style of level 1 }
    class function CreateRegression1(const lt: TLearnType; const InDataLen, OutDataLen: TLInt): TLearn;
    { regression style of level 2 }
    class function CreateRegression2(const lt: TLearnType; const InDataLen, OutDataLen: TLInt): TLearn;

    { classifier style }
    class function CreateClassifier(const lt: TLearnType; const InDataLen: TLInt): TLearn;
    { classifier style of level 1 }
    class function CreateClassifier1(const lt: TLearnType; const InDataLen: TLInt): TLearn;
    { classifier style of level 2 }
    class function CreateClassifier2(const lt: TLearnType; const InDataLen: TLInt): TLearn;

    constructor Create; virtual;
    destructor Destroy; override;

    { * random number * }
    property RandomNumber: Boolean read FRandomNumber write FRandomNumber;

    { * clear * }
    procedure Clear;

    { * parameter * }
    function Count: TLInt;
    property InSize: TLInt read FInSize;
    property OutSize: TLInt read FOutSize;
    property LearnType: TLearnType read FLearnType;
    property Info: TPascalString read FInfo;
    property TrainingThreadRuning: Boolean read FTrainingThreadRuning;
    function GetMemorySource(const index: TLInt): PLearnMemory;
    property MemorySource[const index: TLInt]: PLearnMemory read GetMemorySource; default;
    property LastTrainMaxInValue: TLFloat read FLastTrainMaxInValue;
    property LastTrainMaxOutValue: TLFloat read FLastTrainMaxOutValue;

    { * user parameter * }
    property UserData: Pointer read FUserData write FUserData;
    property UserObject: TCoreClassObject read FUserObject write FUserObject;

    { * sampler * }
    function AddMemory(const f_In, f_Out: TLVec; f_token: TPascalString): PLearnMemory; overload;
    function AddMemory(const f_In: TLVec; f_token: TPascalString): PLearnMemory; overload;
    function AddMemory(const f_In, f_Out: TLVec): PLearnMemory; overload;
    function AddMemory(const s_In, s_Out: TPascalString): PLearnMemory; overload;
    function AddMemory(const s_In, s_Out, s_token: TPascalString): PLearnMemory; overload;
    function AddMemory(const s: TPascalString): PLearnMemory; overload;

    procedure AddSampler(const f_In, f_Out: TLVec); overload;
    procedure AddSampler(const s_In, s_Out: TPascalString); overload;
    procedure AddSampler(const s: TPascalString); overload;
    procedure AddMatrix(const m_in: TLMatrix; const f_Out: TLVec); overload;
    procedure AddMatrix(const m_in: TLMatrix; const f_Out: TLVec; const f_token: TPascalString); overload;

    { * kdtree * }
    procedure AddKDTree(kd: TKDTreeDataList);

    { * normal Training * }
    function Training(const TrainDepth: TLInt): Boolean; overload;
    function Training: Boolean; overload;
    { * Training with thread * }
    procedure Training_MT; overload;
    procedure Training_MT(const TrainDepth: TLInt); overload;
    procedure TrainingC(const TrainDepth: TLInt; const OnResult: TLearnState_Call);
    procedure TrainingM(const TrainDepth: TLInt; const OnResult: TLearnState_Method);
    procedure TrainingP(const TrainDepth: TLInt; const OnResult: TLearnState_Proc);

    { wait thread }
    procedure WaitTraining;

    { token }
    function SearchToken(const v: TLVec): TPascalString;
    function SearchOutVecToken(const v: TLVec): TPascalString;
    function FindTokenIndex(const token_: TPascalString): Integer;
    function FindTokenData(const token_: TPascalString): PLearnMemory;

    { data input/output }
    function Process(const p_in, p_out: PLVec): Boolean; overload;
    function Process(const ProcessIn: PLVec): TPascalString; overload;
    function Process(const ProcessIn: TLVec): TPascalString; overload;
    function Process(const ProcessIn: TPascalString): TPascalString; overload;
    function ProcessMatrix(const p_in: PLMatrix; const p_out: PLVec): Boolean; overload;
    function ProcessToken(const ProcessIn: PLVec): TPascalString; overload;
    function ProcessToken(const ProcessIn: TLVec): TPascalString; overload;

    { result max value }
    function ProcessMax(const ProcessIn: TLVec): TLFloat; overload;
    function ProcessMax(const ProcessIn: TLMatrix): TLFloat; overload;
    function ProcessMaxToken(const ProcessIn: TLVec): TPascalString; overload;
    function ProcessMaxToken(const ProcessIn: TLMatrix): TPascalString; overload;

    { result max index }
    function ProcessMaxIndex(const ProcessIn: TLVec): TLInt; overload;
    function ProcessMaxIndex(const ProcessIn: TLMatrix): TLInt; overload;
    function ProcessMaxIndexToken(const ProcessIn: TLVec): TPascalString; overload;
    function ProcessMaxIndexToken(const ProcessIn: TLMatrix): TPascalString; overload;
    function ProcessMaxIndexCandidate(const ProcessIn: TLVec): TLIVec; overload;
    function ProcessMaxIndexCandidate(const ProcessIn: TLMatrix): TLIVec; overload;

    { result min value }
    function ProcessMin(const ProcessIn: TLVec): TLFloat; overload;
    function ProcessMin(const ProcessIn: TLMatrix): TLFloat; overload;
    function ProcessMinToken(const ProcessIn: TLVec): TPascalString; overload;
    function ProcessMinToken(const ProcessIn: TLMatrix): TPascalString; overload;

    { result min index }
    function ProcessMinIndex(const ProcessIn: TLVec): TLInt; overload;
    function ProcessMinIndex(const ProcessIn: TLMatrix): TLInt; overload;
    function ProcessMinIndexToken(const ProcessIn: TLVec): TPascalString; overload;
    function ProcessMinIndexToken(const ProcessIn: TLMatrix): TPascalString; overload;
    function ProcessMinIndexCandidate(const ProcessIn: TLVec): TLIVec; overload;
    function ProcessMinIndexCandidate(const ProcessIn: TLMatrix): TLIVec; overload;

    { result first value }
    function ProcessFV(const ProcessIn: TLVec): TLFloat; overload;
    function ProcessFV(const ProcessIn: TLMatrix): TLFloat; overload;
    function ProcessFV(const ProcessIn: TPascalString): TLFloat; overload;

    { result last value }
    function ProcessLV(const ProcessIn: TLVec): TLFloat; overload;
    function ProcessLV(const ProcessIn: TLMatrix): TLFloat; overload;
    function ProcessLV(const ProcessIn: TPascalString): TLFloat; overload;

    { search with Pearson }
    function SearchMemoryPearson(const ProcessIn: TLVec): TLInt; overload;
    procedure SearchMemoryPearson(const ProcessIn: TLVec; out List: TLIVec); overload;

    { search with Spearman }
    function SearchMemorySpearman(const ProcessIn: TLVec): TLInt; overload;
    procedure SearchMemorySpearman(const ProcessIn: TLVec; out List: TLIVec); overload;

    { search with euclidean metric:K }
    function SearchMemoryDistance(const ProcessIn: TLVec): TLInt; overload;
    procedure SearchMemoryDistance(const ProcessIn: TLVec; out List: TLIVec); overload;

    { build input Vector to KDTree }
    function BuildKDTree: TKDTree;

    { * fast binary store * }
    procedure SaveToDF(df: TDataFrameEngine);
    procedure LoadFromDF(df: TDataFrameEngine);

    { store support }
    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: TPascalString);
    procedure LoadFromFile(FileName: TPascalString);

{$IFNDEF FPC}
    { * json store support * }
    procedure SaveToJsonStream(stream: TCoreClassStream);
    procedure LoadFromJsonStream(stream: TCoreClassStream);

    procedure SaveToJsonFile(FileName: TPascalString);
    procedure LoadFromJsonFile(FileName: TPascalString);
{$ENDIF FPC}
  end;

  TLearnRandom = class(TMT19937Random)
  public
    property RandReal: Double read RandD;
  end;

{$ENDREGION 'Class'}
{$REGION 'LearnAPI'}


procedure LAdd(var f: TLFloat; const Value: TLFloat);
procedure LSub(var f: TLFloat; const Value: TLFloat);
procedure LMul(var f: TLFloat; const Value: TLFloat);
procedure LDiv(var f: TLFloat; const Value: TLFloat);
function LSafeDivF(const s, d: TLFloat): TLFloat;
procedure LSetVec(var v: TLVec; const VDef: TLFloat); overload;
procedure LSetVec(var v: TLIVec; const VDef: TLInt); overload;
procedure LSetVec(var v: TLBVec; const VDef: Boolean); overload;
procedure LSetMatrix(var M: TLMatrix; const VDef: TLFloat); overload;
procedure LSetMatrix(var M: TLIMatrix; const VDef: TLInt); overload;
procedure LSetMatrix(var M: TLBMatrix; const VDef: Boolean); overload;
function LVecCopy(const v: TLVec): TLVec; overload;
function LVecCopy(const v: TLVec; const index, Count: TLInt): TLVec; overload;
function LVecCopy(const v: TLIVec): TLIVec; overload;
function LVecCopy(const v: TLIVec; const index, Count: TLInt): TLIVec; overload;
function LVecCopy(const v: TLBVec): TLBVec; overload;
function LVecCopy(const v: TLBVec; const index, Count: TLInt): TLBVec; overload;
function LMatrixCopy(const v: TLMatrix): TLMatrix; overload;
function LMatrixCopy(const v: TLIMatrix): TLIMatrix; overload;
function LMatrixCopy(const v: TLBMatrix): TLBMatrix; overload;
function LVec(): TLVec; overload;
function LVec(const veclen: TLInt; const VDef: TLFloat): TLVec; overload;
function LVec(const veclen: TLInt): TLVec; overload;
function LVec(const v: TLVec): TPascalString; overload;
function LVec(const M: TLMatrix; const veclen: TLInt): TLVec; overload;
function LVec(const M: TLMatrix): TLVec; overload;
function LVec(const s: TPascalString): TLVec; overload;
function LVec(const s: TPascalString; const veclen: TLInt): TLVec; overload;
function LVec(const v: TLVec; const ShortFloat: Boolean): TPascalString; overload;
function LVec(const M: TLBMatrix; const veclen: TLInt): TLBVec; overload;
function LVec(const M: TLBMatrix): TLBVec; overload;
function LVec(const M: TLIMatrix; const veclen: TLInt): TLIVec; overload;
function LVec(const M: TLIMatrix): TLIVec; overload;
function ExpressionToLVec(const s: TPascalString; const_vl: THashVariantList): TLVec; overload;
function ExpressionToLVec(const s: TPascalString): TLVec; overload;
function ExpLVec(const s: TPascalString; const_vl: THashVariantList): TLVec; overload;
function ExpLVec(const s: TPascalString): TLVec; overload;
function ExpressionToLIVec(const s: TPascalString; const_vl: THashVariantList): TLIVec; overload;
function ExpressionToLIVec(const s: TPascalString): TLIVec; overload;
function ExpLIVec(const s: TPascalString; const_vl: THashVariantList): TLIVec; overload;
function ExpLIVec(const s: TPascalString): TLIVec; overload;
function LSpearmanVec(const M: TLMatrix; const veclen: TLInt): TLVec;
function LAbsMaxVec(const v: TLVec): TLFloat;
function LMaxVec(const v: TLVec): TLFloat; overload;
function LMaxVec(const v: TLIVec): TLInt; overload;
function LMaxVec(const v: TLMatrix): TLFloat; overload;
function LMaxVec(const v: TLIMatrix): TLInt; overload;
function LMinVec(const v: TLVec): TLFloat; overload;
function LMinVec(const v: TLIVec): TLInt; overload;
function LMinVec(const v: TLMatrix): TLFloat; overload;
function LMinVec(const v: TLIMatrix): TLInt; overload;
function LMaxVecIndex(const v: TLVec): TLInt;
function LMinVecIndex(const v: TLVec): TLInt;
function LDistance(const v1, v2: TLVec): TLFloat;
function LHamming(const v1, v2: TLVec): TLInt; overload;
function LHamming(const v1, v2: TLIVec): TLInt; overload;
procedure LClampF(var v: TLFloat; const min_, max_: TLFloat); overload;
procedure LClampI(var v: TLInt; const min_, max_: TLInt); overload;
function LClamp(const v: TLFloat; const min_, max_: TLFloat): TLFloat; overload;
function LClamp(const v: TLInt; const min_, max_: TLInt): TLInt; overload;
function LComplex(X, Y: TLFloat): TLComplex; overload;
function LComplex(f: TLFloat): TLComplex; overload;

{ * sampler support * }
procedure LZoomMatrix(var Source, dest: TLMatrix; const DestWidth, DestHeight: TLInt); overload;
procedure LZoomMatrix(var Source, dest: TLIMatrix; const DestWidth, DestHeight: TLInt); overload;
procedure LZoomMatrix(var Source, dest: TLBMatrix; const DestWidth, DestHeight: TLInt); overload;

{ matrix as stream }
procedure LSaveMatrix(var Source: TLMatrix; dest: TCoreClassStream); overload;
procedure LLoadMatrix(Source: TCoreClassStream; var dest: TLMatrix); overload;
procedure LSaveMatrix(var Source: TLIMatrix; dest: TCoreClassStream); overload;
procedure LLoadMatrix(Source: TCoreClassStream; var dest: TLIMatrix); overload;
procedure LSaveMatrix(var Source: TLBMatrix; dest: TCoreClassStream); overload;
procedure LLoadMatrix(Source: TCoreClassStream; var dest: TLBMatrix); overload;

{ * linear discriminant analysis support * }
function LDA(const M: TLMatrix; const cv: TLVec; const Nclass: TLInt; var sInfo: TPascalString; var output: TLMatrix): Boolean; overload;
function LDA(const M: TLMatrix; const cv: TLVec; const Nclass: TLInt; var sInfo: TPascalString; var output: TLVec): Boolean; overload;
procedure FisherLDAN(const xy: TLMatrix; NPoints: TLInt; NVars: TLInt; NClasses: TLInt; var Info: TLInt; var w: TLMatrix);
procedure FisherLDA(const xy: TLMatrix; NPoints: TLInt; NVars: TLInt; NClasses: TLInt; var Info: TLInt; var w: TLVec);

{ * principal component analysis support * }

(*
  return code:
  * -4, if SVD subroutine haven't converged
  * -1, if wrong parameters has been passed (NPoints<0, NVars<1)
  *  1, if task is solved
*)
function PCA(const buff: TLMatrix; const NPoints, NVars: TLInt; var v: TLVec; var M: TLMatrix): TLInt; overload;
function PCA(const buff: TLMatrix; const NPoints, NVars: TLInt; var M: TLMatrix): TLInt; overload;
procedure PCABuildBasis(const X: TLMatrix; NPoints: TLInt; NVars: TLInt; var Info: TLInt; var s2: TLVec; var v: TLMatrix);

{ * k-means++ clusterization support * }
function KMeans(const Source: TLMatrix; const NVars, K: TLInt; var KArray: TLMatrix; var kIndex: TLIVec): Boolean;

{ * init Matrix * }
function LMatrix(const L1, l2: TLInt): TLMatrix; overload;
function LBMatrix(const L1, l2: TLInt): TLBMatrix; overload;
function LIMatrix(const L1, l2: TLInt): TLIMatrix; overload;
function ExpressionToLMatrix(w, h: TLInt; const s: TPascalString; const_vl: THashVariantList): TLMatrix; overload;
function ExpressionToLMatrix(w, h: TLInt; const s: TPascalString): TLMatrix; overload;

{$ENDREGION 'LearnAPI'}
{$REGION 'FloatAPI'}
function AbsReal(X: TLFloat): TLFloat;
function AbsInt(i: TLInt): TLInt;
function RandomReal(): TLFloat;
function RandomInteger(i: TLInt): TLInt;
function Sign(X: TLFloat): TLInt;
function AP_Sqr(X: TLFloat): TLFloat;

function DynamicArrayCopy(const a: TLIVec): TLIVec; overload;
function DynamicArrayCopy(const a: TLVec): TLVec; overload;
function DynamicArrayCopy(const a: TLComplexVec): TLComplexVec; overload;
function DynamicArrayCopy(const a: TLBVec): TLBVec; overload;

function DynamicArrayCopy(const a: TLIMatrix): TLIMatrix; overload;
function DynamicArrayCopy(const a: TLMatrix): TLMatrix; overload;
function DynamicArrayCopy(const a: TLComplexMatrix): TLComplexMatrix; overload;
function DynamicArrayCopy(const a: TLBMatrix): TLBMatrix; overload;

function AbsComplex(const z: TLComplex): TLFloat;
function Conj(const z: TLComplex): TLComplex;
function CSqr(const z: TLComplex): TLComplex;

function C_Complex(const X: TLFloat): TLComplex;
function C_Opposite(const z: TLComplex): TLComplex;
function C_Add(const z1: TLComplex; const z2: TLComplex): TLComplex;
function C_Mul(const z1: TLComplex; const z2: TLComplex): TLComplex;
function C_AddR(const z1: TLComplex; const r: TLFloat): TLComplex;
function C_MulR(const z1: TLComplex; const r: TLFloat): TLComplex;
function C_Sub(const z1: TLComplex; const z2: TLComplex): TLComplex;
function C_SubR(const z1: TLComplex; const r: TLFloat): TLComplex;
function C_RSub(const r: TLFloat; const z1: TLComplex): TLComplex;
function C_Div(const z1: TLComplex; const z2: TLComplex): TLComplex;
function C_DivR(const z1: TLComplex; const r: TLFloat): TLComplex;
function C_RDiv(const r: TLFloat; const z2: TLComplex): TLComplex;
function C_Equal(const z1: TLComplex; const z2: TLComplex): Boolean;
function C_NotEqual(const z1: TLComplex; const z2: TLComplex): Boolean;
function C_EqualR(const z1: TLComplex; const r: TLFloat): Boolean;
function C_NotEqualR(const z1: TLComplex; const r: TLFloat): Boolean;

function APVDotProduct(v1: PLFloat; i11, i12: TLInt; v2: PLFloat; i21, i22: TLInt): TLFloat;
procedure APVMove(VDst: PLFloat; i11, i12: TLInt; vSrc: PLFloat; i21, i22: TLInt); overload;
procedure APVMove(VDst: PLFloat; i11, i12: TLInt; vSrc: PLFloat; i21, i22: TLInt; s: TLFloat); overload;
procedure APVMoveNeg(VDst: PLFloat; i11, i12: TLInt; vSrc: PLFloat; i21, i22: TLInt);
procedure APVAdd(VDst: PLFloat; i11, i12: TLInt; vSrc: PLFloat; i21, i22: TLInt); overload;
procedure APVAdd(VDst: PLFloat; i11, i12: TLInt; vSrc: PLFloat; i21, i22: TLInt; s: TLFloat); overload;
procedure APVSub(VDst: PLFloat; i11, i12: TLInt; vSrc: PLFloat; i21, i22: TLInt); overload;
procedure APVSub(VDst: PLFloat; i11, i12: TLInt; vSrc: PLFloat; i21, i22: TLInt; s: TLFloat); overload;
procedure APVMul(VOp: PLFloat; i1, i2: TLInt; s: TLFloat);
procedure APVFillValue(VOp: PLFloat; i1, i2: TLInt; s: TLFloat);

function AP_Float(X: TLFloat): TLFloat;
function AP_FP_Eq(X: TLFloat; Y: TLFloat): Boolean;
function AP_FP_NEq(X: TLFloat; Y: TLFloat): Boolean;
function AP_FP_Less(X: TLFloat; Y: TLFloat): Boolean;
function AP_FP_Less_Eq(X: TLFloat; Y: TLFloat): Boolean;
function AP_FP_Greater(X: TLFloat; Y: TLFloat): Boolean;
function AP_FP_Greater_Eq(X: TLFloat; Y: TLFloat): Boolean;

procedure TagSort(var a: TLVec; const n: TLInt; var p1: TLIVec; var p2: TLIVec);
procedure TagSortFastI(var a: TLVec; var b: TLIVec; n: TLInt);
procedure TagSortFastR(var a: TLVec; var b: TLVec; n: TLInt);
procedure TagSortFast(var a: TLVec; const n: TLInt);
procedure TagHeapPushI(var a: TLVec; var b: TLIVec; var n: TLInt; const VA: TLFloat; const VB: TLInt);
procedure TagHeapReplaceTopI(var a: TLVec; var b: TLIVec; const n: TLInt; const VA: TLFloat; const VB: TLInt);
procedure TagHeapPopI(var a: TLVec; var b: TLIVec; var n: TLInt);

(* ************************************************************************
  More precise dot-product. Absolute error of  subroutine  result  is  about
  1 ulp of max(MX,V), where:
  MX = max( |a[i]*b[i]| )
  V  = |(a,b)|

  INPUT PARAMETERS
  A       -   array[0..N-1], vector 1
  B       -   array[0..N-1], vector 2
  N       -   vectors length, N<2^29.
  Temp    -   array[0..N-1], pre-allocated temporary storage

  OUTPUT PARAMETERS
  R       -   (A,B)
  RErr    -   estimate of error. This estimate accounts for both  errors
  during  calculation  of  (A,B)  and  errors  introduced by
  rounding of A and B to fit in TLFloat (about 1 ulp).
  ************************************************************************ *)
procedure XDot(const a: TLVec; const b: TLVec; n: TLInt; var Temp: TLVec; var r: TLFloat; var RErr: TLFloat);

(* ************************************************************************
  Internal subroutine for extra-precise calculation of SUM(w[i]).

  INPUT PARAMETERS:
  W   -   array[0..N-1], values to be added W is modified during calculations.
  MX  -   max(W[i])
  N   -   array size

  OUTPUT PARAMETERS:
  R   -   SUM(w[i])
  RErr-   error estimate for R
  ************************************************************************ *)
procedure XSum(var w: TLVec; mx: TLFloat; n: TLInt; var r: TLFloat; var RErr: TLFloat);

(* ************************************************************************
  Fast Pow
  ************************************************************************ *)
function XFastPow(r: TLFloat; n: TLInt): TLFloat;

{$ENDREGION 'FloatAPI'}
{$REGION 'LowLevelMatrix'}
{ matrix base }
function VectorNorm2(const X: TLVec; const i1, i2: TLInt): TLFloat;
function VectorIdxAbsMax(const X: TLVec; const i1, i2: TLInt): TLInt;
function ColumnIdxAbsMax(const X: TLMatrix; const i1, i2, j: TLInt): TLInt;
function RowIdxAbsMax(const X: TLMatrix; const j1, j2, i: TLInt): TLInt;
function UpperHessenberg1Norm(const a: TLMatrix; const i1, i2, j1, j2: TLInt; var Work: TLVec): TLFloat;

procedure CopyMatrix(const a: TLMatrix; const IS1, IS2, JS1, JS2: TLInt;
  var b: TLMatrix; const ID1, id2, JD1, JD2: TLInt);

procedure InplaceTranspose(var a: TLMatrix; const i1, i2, j1, j2: TLInt; var Work: TLVec);

procedure CopyAndTranspose(const a: TLMatrix; IS1, IS2, JS1, JS2: TLInt;
  var b: TLMatrix; ID1, id2, JD1, JD2: TLInt);

procedure MatrixVectorMultiply(const a: TLMatrix; const i1, i2, j1, j2: TLInt; const Trans: Boolean;
  const X: TLVec; const IX1, IX2: TLInt; const alpha: TLFloat;
  var Y: TLVec; const IY1, IY2: TLInt; const beta: TLFloat);

function Pythag2(X: TLFloat; Y: TLFloat): TLFloat;

procedure MatrixMatrixMultiply(const a: TLMatrix; const AI1, AI2, AJ1, AJ2: TLInt; const TransA: Boolean;
  const b: TLMatrix; const BI1, BI2, BJ1, BJ2: TLInt; const TransB: Boolean;
  const alpha: TLFloat;
  var c: TLMatrix; const CI1, CI2, CJ1, CJ2: TLInt;
  const beta: TLFloat;
  var Work: TLVec);

{ Level 2 and Level 3 BLAS operations }
procedure ABLASSplitLength(const a: TLMatrix; n: TLInt; var n1: TLInt; var n2: TLInt);
procedure ABLASComplexSplitLength(const a: TLComplexMatrix; n: TLInt; var n1: TLInt; var n2: TLInt);
function ABLASBlockSize(const a: TLMatrix): TLInt;
function ABLASComplexBlockSize(const a: TLComplexMatrix): TLInt;
function ABLASMicroBlockSize(): TLInt;
procedure CMatrixTranspose(M: TLInt; n: TLInt; const a: TLComplexMatrix; IA: TLInt; ja: TLInt; var b: TLComplexMatrix; IB: TLInt; JB: TLInt);
procedure RMatrixTranspose(M: TLInt; n: TLInt; const a: TLMatrix; IA: TLInt; ja: TLInt; var b: TLMatrix; IB: TLInt; JB: TLInt);
procedure CMatrixCopy(M: TLInt; n: TLInt; const a: TLComplexMatrix; IA: TLInt; ja: TLInt; var b: TLComplexMatrix; IB: TLInt; JB: TLInt);
procedure RMatrixCopy(M: TLInt; n: TLInt; const a: TLMatrix; IA: TLInt; ja: TLInt; var b: TLMatrix; IB: TLInt; JB: TLInt);
procedure CMatrixRank1(M: TLInt; n: TLInt; var a: TLComplexMatrix; IA: TLInt; ja: TLInt; var u: TLComplexVec; IU: TLInt; var v: TLComplexVec; IV: TLInt);
procedure RMatrixRank1(M: TLInt; n: TLInt; var a: TLMatrix; IA: TLInt; ja: TLInt; var u: TLVec; IU: TLInt; var v: TLVec; IV: TLInt);
procedure CMatrixMV(M: TLInt; n: TLInt; var a: TLComplexMatrix; IA: TLInt; ja: TLInt; OpA: TLInt; var X: TLComplexVec; ix: TLInt; var Y: TLComplexVec; iy: TLInt);
procedure RMatrixMV(M: TLInt; n: TLInt; var a: TLMatrix; IA: TLInt; ja: TLInt; OpA: TLInt; var X: TLVec; ix: TLInt; var Y: TLVec; iy: TLInt);

procedure CMatrixRightTRSM(M: TLInt; n: TLInt;
  const a: TLComplexMatrix; i1: TLInt; j1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var X: TLComplexMatrix; i2: TLInt; j2: TLInt);

procedure CMatrixLeftTRSM(M: TLInt; n: TLInt;
  const a: TLComplexMatrix; i1: TLInt; j1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var X: TLComplexMatrix; i2: TLInt; j2: TLInt);

procedure RMatrixRightTRSM(M: TLInt; n: TLInt;
  const a: TLMatrix; i1: TLInt; j1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var X: TLMatrix; i2: TLInt; j2: TLInt);

procedure RMatrixLeftTRSM(M: TLInt; n: TLInt;
  const a: TLMatrix; i1: TLInt; j1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var X: TLMatrix; i2: TLInt; j2: TLInt);

procedure CMatrixSYRK(n: TLInt; K: TLInt; alpha: TLFloat;
  const a: TLComplexMatrix; IA: TLInt; ja: TLInt; OpTypeA: TLInt;
  beta: TLFloat; var c: TLComplexMatrix; IC: TLInt; JC: TLInt; IsUpper: Boolean);

procedure RMatrixSYRK(n: TLInt; K: TLInt; alpha: TLFloat;
  const a: TLMatrix; IA: TLInt; ja: TLInt; OpTypeA: TLInt;
  beta: TLFloat; var c: TLMatrix; IC: TLInt; JC: TLInt; IsUpper: Boolean);

procedure CMatrixGEMM(M: TLInt; n: TLInt; K: TLInt; alpha: TLComplex;
  const a: TLComplexMatrix; IA: TLInt; ja: TLInt; OpTypeA: TLInt;
  const b: TLComplexMatrix; IB: TLInt; JB: TLInt; OpTypeB: TLInt;
  beta: TLComplex; var c: TLComplexMatrix; IC: TLInt; JC: TLInt);

procedure RMatrixGEMM(M: TLInt; n: TLInt; K: TLInt; alpha: TLFloat;
  const a: TLMatrix; IA: TLInt; ja: TLInt; OpTypeA: TLInt;
  const b: TLMatrix; IB: TLInt; JB: TLInt; OpTypeB: TLInt;
  beta: TLFloat; var c: TLMatrix; IC: TLInt; JC: TLInt);

{ LU and Cholesky decompositions }
procedure RMatrixLU(var a: TLMatrix; M: TLInt; n: TLInt; var Pivots: TLIVec);
procedure CMatrixLU(var a: TLComplexMatrix; M: TLInt; n: TLInt; var Pivots: TLIVec);
function HPDMatrixCholesky(var a: TLComplexMatrix; n: TLInt; IsUpper: Boolean): Boolean;
function SPDMatrixCholesky(var a: TLMatrix; n: TLInt; IsUpper: Boolean): Boolean;
procedure RMatrixLUP(var a: TLMatrix; M: TLInt; n: TLInt; var Pivots: TLIVec);
procedure CMatrixLUP(var a: TLComplexMatrix; M: TLInt; n: TLInt; var Pivots: TLIVec);
procedure RMatrixPLU(var a: TLMatrix; M: TLInt; n: TLInt; var Pivots: TLIVec);
procedure CMatrixPLU(var a: TLComplexMatrix; M: TLInt; n: TLInt; var Pivots: TLIVec);

{ matrix safe }
function RMatrixScaledTRSafeSolve(const a: TLMatrix; SA: TLFloat;
  n: TLInt; var X: TLVec; IsUpper: Boolean; Trans: TLInt;
  IsUnit: Boolean; MaxGrowth: TLFloat): Boolean;

function CMatrixScaledTRSafeSolve(const a: TLComplexMatrix; SA: TLFloat;
  n: TLInt; var X: TLComplexVec; IsUpper: Boolean;
  Trans: TLInt; IsUnit: Boolean; MaxGrowth: TLFloat): Boolean;

{ * Condition number estimate support * }
function RMatrixRCond1(a: TLMatrix; n: TLInt): TLFloat;
function RMatrixRCondInf(a: TLMatrix; n: TLInt): TLFloat;
function SPDMatrixRCond(a: TLMatrix; n: TLInt; IsUpper: Boolean): TLFloat;
function RMatrixTRRCond1(const a: TLMatrix; n: TLInt; IsUpper: Boolean; IsUnit: Boolean): TLFloat;
function RMatrixTRRCondInf(const a: TLMatrix; n: TLInt; IsUpper: Boolean; IsUnit: Boolean): TLFloat;
function HPDMatrixRCond(a: TLComplexMatrix; n: TLInt; IsUpper: Boolean): TLFloat;
function CMatrixRCond1(a: TLComplexMatrix; n: TLInt): TLFloat;
function CMatrixRCondInf(a: TLComplexMatrix; n: TLInt): TLFloat;
function RMatrixLURCond1(const LUA: TLMatrix; n: TLInt): TLFloat;
function RMatrixLURCondInf(const LUA: TLMatrix; n: TLInt): TLFloat;
function SPDMatrixCholeskyRCond(const a: TLMatrix; n: TLInt; IsUpper: Boolean): TLFloat;
function HPDMatrixCholeskyRCond(const a: TLComplexMatrix; n: TLInt; IsUpper: Boolean): TLFloat;
function CMatrixLURCond1(const LUA: TLComplexMatrix; n: TLInt): TLFloat;
function CMatrixLURCondInf(const LUA: TLComplexMatrix; n: TLInt): TLFloat;
function CMatrixTRRCond1(const a: TLComplexMatrix; n: TLInt; IsUpper: Boolean; IsUnit: Boolean): TLFloat;
function CMatrixTRRCondInf(const a: TLComplexMatrix; n: TLInt; IsUpper: Boolean; IsUnit: Boolean): TLFloat;
function RCondThreshold(): TLFloat;

{ Matrix inverse }
procedure RMatrixLUInverse(var a: TLMatrix; const Pivots: TLIVec; n: TLInt; var Info: TLInt; var Rep: TMatInvReport);
procedure RMatrixInverse(var a: TLMatrix; n: TLInt; var Info: TLInt; var Rep: TMatInvReport);
procedure CMatrixLUInverse(var a: TLComplexMatrix; const Pivots: TLIVec; n: TLInt; var Info: TLInt; var Rep: TMatInvReport);
procedure CMatrixInverse(var a: TLComplexMatrix; n: TLInt; var Info: TLInt; var Rep: TMatInvReport);
procedure SPDMatrixCholeskyInverse(var a: TLMatrix; n: TLInt; IsUpper: Boolean; var Info: TLInt; var Rep: TMatInvReport);
procedure SPDMatrixInverse(var a: TLMatrix; n: TLInt; IsUpper: Boolean; var Info: TLInt; var Rep: TMatInvReport);
procedure HPDMatrixCholeskyInverse(var a: TLComplexMatrix; n: TLInt; IsUpper: Boolean; var Info: TLInt; var Rep: TMatInvReport);
procedure HPDMatrixInverse(var a: TLComplexMatrix; n: TLInt; IsUpper: Boolean; var Info: TLInt; var Rep: TMatInvReport);
procedure RMatrixTRInverse(var a: TLMatrix; n: TLInt; IsUpper: Boolean; IsUnit: Boolean; var Info: TLInt; var Rep: TMatInvReport);
procedure CMatrixTRInverse(var a: TLComplexMatrix; n: TLInt; IsUpper: Boolean; IsUnit: Boolean; var Info: TLInt; var Rep: TMatInvReport);

{ matrix rotations }
procedure ApplyRotationsFromTheLeft(IsForward: Boolean; m1: TLInt; m2: TLInt; n1: TLInt; n2: TLInt;
  const c: TLVec; const s: TLVec; var a: TLMatrix; var Work: TLVec);
procedure ApplyRotationsFromTheRight(IsForward: Boolean; m1: TLInt; m2: TLInt; n1: TLInt; n2: TLInt;
  const c: TLVec; const s: TLVec; var a: TLMatrix; var Work: TLVec);
procedure GenerateRotation(f: TLFloat; g: TLFloat; var cs: TLFloat; var sn: TLFloat; var r: TLFloat);

{ Bidiagonal SVD }
function RMatrixBDSVD(var d: TLVec; E: TLVec; n: TLInt; IsUpper: Boolean; IsFractionalAccuracyRequired: Boolean;
  var u: TLMatrix; NRU: TLInt; var c: TLMatrix; NCC: TLInt; var VT: TLMatrix; NCVT: TLInt): Boolean;

function BidiagonalSVDDecomposition(var d: TLVec; E: TLVec; n: TLInt; IsUpper: Boolean; IsFractionalAccuracyRequired: Boolean;
  var u: TLMatrix; NRU: TLInt; var c: TLMatrix; NCC: TLInt; var VT: TLMatrix; NCVT: TLInt): Boolean;

(* ************************************************************************
  Singular value decomposition of a rectangular matrix.

  The algorithm calculates the singular value decomposition of a matrix of
  size MxN: A = U * S * V^T

  The algorithm finds the singular values and, optionally, matrices U and V^T.
  The algorithm can find both first min(M,N) columns of matrix U and rows of
  matrix V^T (singular vectors), and matrices U and V^T wholly (of sizes MxM
  and NxN respectively).

  Take into account that the subroutine does not return matrix V but V^T.

  Input parameters:
  A           -   matrix to be decomposed.
  Array whose indexes range within [0..M-1, 0..N-1].
  M           -   number of rows in matrix A.
  N           -   number of columns in matrix A.
  UNeeded     -   0, 1 or 2. See the description of the parameter U.
  VTNeeded    -   0, 1 or 2. See the description of the parameter VT.

  AdditionalMemory -
  If the parameter:
  * equals 0, the algorithm dont use additional memory (lower requirements, lower performance).
  * equals 1, the algorithm uses additional memory of size min(M,N)*min(M,N) of real numbers. It often speeds up the algorithm.
  * equals 2, the algorithm uses additional memory of size M*min(M,N) of real numbers.
  It allows to get a maximum performance. The recommended value of the parameter is 2.

  Output parameters:
  W           -   contains singular values in descending order.

  U           -   if UNeeded=0, U isn't changed, the left singular vectors are not calculated.
  if Uneeded=1, U contains left singular vectors (first min(M,N) columns of matrix U). Array whose indexes range within [0..M-1, 0..Min(M,N)-1].
  if UNeeded=2, U contains matrix U wholly. Array whose indexes range within [0..M-1, 0..M-1].

  VT          -   if VTNeeded=0, VT isnæŠ?changed, the right singular vectors are not calculated.
  if VTNeeded=1, VT contains right singular vectors (first min(M,N) rows of matrix V^T). Array whose indexes range within [0..min(M,N)-1, 0..N-1].
  if VTNeeded=2, VT contains matrix V^T wholly. Array whose indexes range within [0..N-1, 0..N-1].
  ************************************************************************ *)
function RMatrixSVD(a: TLMatrix; const M, n, UNeeded, VTNeeded, AdditionalMemory: TLInt; var w: TLVec; var u: TLMatrix; var VT: TLMatrix): Boolean;

{ Eigensolvers }
function SMatrixEVD(a: TLMatrix; n: TLInt; ZNeeded: TLInt; IsUpper: Boolean; var d: TLVec; var z: TLMatrix): Boolean;

function SMatrixEVDR(a: TLMatrix; n: TLInt; ZNeeded: TLInt;
  IsUpper: Boolean; b1: TLFloat; b2: TLFloat; var M: TLInt;
  var w: TLVec; var z: TLMatrix): Boolean;

function SMatrixEVDI(a: TLMatrix; n: TLInt; ZNeeded: TLInt;
  IsUpper: Boolean; i1: TLInt; i2: TLInt;
  var w: TLVec; var z: TLMatrix): Boolean;

function HMatrixEVD(a: TLComplexMatrix; n: TLInt; ZNeeded: TLInt; IsUpper: Boolean;
  var d: TLVec; var z: TLComplexMatrix): Boolean;

function HMatrixEVDR(a: TLComplexMatrix; n: TLInt;
  ZNeeded: TLInt; IsUpper: Boolean; b1: TLFloat; b2: TLFloat;
  var M: TLInt; var w: TLVec; var z: TLComplexMatrix): Boolean;

function HMatrixEVDI(a: TLComplexMatrix; n: TLInt;
  ZNeeded: TLInt; IsUpper: Boolean; i1: TLInt;
  i2: TLInt; var w: TLVec; var z: TLComplexMatrix): Boolean;

function SMatrixTDEVD(var d: TLVec; E: TLVec; n: TLInt; ZNeeded: TLInt; var z: TLMatrix): Boolean;

function SMatrixTDEVDR(var d: TLVec; const E: TLVec;
  n: TLInt; ZNeeded: TLInt; a: TLFloat; b: TLFloat;
  var M: TLInt; var z: TLMatrix): Boolean;

function SMatrixTDEVDI(var d: TLVec; const E: TLVec;
  n: TLInt; ZNeeded: TLInt; i1: TLInt;
  i2: TLInt; var z: TLMatrix): Boolean;

function RMatrixEVD(a: TLMatrix; n: TLInt; VNeeded: TLInt;
  var WR: TLVec; var WI: TLVec; var vl: TLMatrix;
  var vr: TLMatrix): Boolean;

function InternalBisectionEigenValues(d: TLVec; E: TLVec;
  n: TLInt; IRANGE: TLInt; IORDER: TLInt;
  vl: TLFloat; VU: TLFloat; IL: TLInt; IU: TLInt;
  ABSTOL: TLFloat; var w: TLVec; var M: TLInt;
  var NSPLIT: TLInt; var IBLOCK: TLIVec;
  var ISPLIT: TLIVec; var ErrorCode: TLInt): Boolean;

procedure InternalDSTEIN(const n: TLInt; const d: TLVec;
  E: TLVec; const M: TLInt; w: TLVec;
  const IBLOCK: TLIVec; const ISPLIT: TLIVec;
  var z: TLMatrix; var IFAIL: TLIVec; var Info: TLInt);

{ Schur decomposition }
function RMatrixSchur(var a: TLMatrix; n: TLInt; var s: TLMatrix): Boolean;
function UpperHessenbergSchurDecomposition(var h: TLMatrix; n: TLInt; var s: TLMatrix): Boolean;

{$ENDREGION 'LowLevelMatrix'}
{$REGION 'LowlevelDistribution'}

{ Normal distribution support }
function NormalDistribution(const X: TLFloat): TLFloat;
function InvNormalDistribution(const y0: TLFloat): TLFloat;

{ statistics base }
function Log1P(const X: TLFloat): TLFloat;
function ExpM1(const X: TLFloat): TLFloat;
function CosM1(const X: TLFloat): TLFloat;
{ Gamma support }
function Gamma(const X: TLFloat): TLFloat;
{ Natural logarithm of gamma function }
function LnGamma(const X: TLFloat; var SgnGam: TLFloat): TLFloat;
{ Incomplete gamma integral }
function IncompleteGamma(const a, X: TLFloat): TLFloat;
{ Complemented incomplete gamma integral }
function IncompleteGammaC(const a, X: TLFloat): TLFloat;
{ Inverse of complemented imcomplete gamma integral }
function InvIncompleteGammaC(const a, y0: TLFloat): TLFloat;

{ Poisson distribution }
function PoissonDistribution(K: TLInt; M: TLFloat): TLFloat;
{ Complemented Poisson distribution }
function PoissonCDistribution(K: TLInt; M: TLFloat): TLFloat;
{ Inverse Poisson distribution }
function InvPoissonDistribution(K: TLInt; Y: TLFloat): TLFloat;

{ Incomplete beta integral support }
function IncompleteBeta(a, b, X: TLFloat): TLFloat;
{ Inverse of imcomplete beta integral }
function InvIncompleteBeta(const a, b, Y: TLFloat): TLFloat;

{ F distribution support }
function FDistribution(const a: TLInt; const b: TLInt; const X: TLFloat): TLFloat;
{ Complemented F distribution }
function FCDistribution(const a: TLInt; const b: TLInt; const X: TLFloat): TLFloat;
{ Inverse of complemented F distribution }
function InvFDistribution(const a: TLInt; const b: TLInt; const Y: TLFloat): TLFloat;
{ Two-sample F-test }
procedure FTest(const X: TLVec; n: TLInt; const Y: TLVec; M: TLInt; var BothTails, LeftTail, RightTail: TLFloat);

{ Binomial distribution support }
function BinomialDistribution(const K, n: TLInt; const p: TLFloat): TLFloat;
{ Complemented binomial distribution }
function BinomialCDistribution(const K, n: TLInt; const p: TLFloat): TLFloat;
{ Inverse binomial distribution }
function InvBinomialDistribution(const K, n: TLInt; const Y: TLFloat): TLFloat;
{ Sign test }
procedure OneSampleSignTest(const X: TLVec; n: TLInt; Median: TLFloat; var BothTails, LeftTail, RightTail: TLFloat);

{ Chi-square distribution support }
function ChiSquareDistribution(const v, X: TLFloat): TLFloat;
{ Complemented Chi-square distribution }
function ChiSquareCDistribution(const v, X: TLFloat): TLFloat;
{ Inverse of complemented Chi-square distribution }
function InvChiSquareDistribution(const v, Y: TLFloat): TLFloat;
{ One-sample chi-square test }
procedure OneSampleVarianceTest(const X: TLVec; n: TLInt; Variance: TLFloat; var BothTails, LeftTail, RightTail: TLFloat);

{ Student's t distribution support }
function StudentTDistribution(const K: TLInt; const t: TLFloat): TLFloat;
{ Functional inverse of Student's t distribution }
function InvStudentTDistribution(const K: TLInt; p: TLFloat): TLFloat;
{ One-sample t-test }
procedure StudentTTest1(const X: TLVec; n: TLInt; Mean: TLFloat; var BothTails, LeftTail, RightTail: TLFloat);
{ Two-sample pooled test }
procedure StudentTTest2(const X: TLVec; n: TLInt; const Y: TLVec; M: TLInt; var BothTails, LeftTail, RightTail: TLFloat);
{ Two-sample unpooled test }
procedure UnequalVarianceTTest(const X: TLVec; n: TLInt; const Y: TLVec; M: TLInt; var BothTails, LeftTail, RightTail: TLFloat);

{ Pearson and Spearman distribution support }
{ Pearson product-moment correlation coefficient }
function PearsonCorrelation(const X, Y: TLVec; const n: TLInt): TLFloat;
{ Spearman's rank correlation coefficient }
function SpearmanRankCorrelation(const X, Y: TLVec; const n: TLInt): TLFloat;
procedure SpearmanRank(var X: TLVec; n: TLInt);
{ Pearson's correlation coefficient significance test }
procedure PearsonCorrelationSignificance(const r: TLFloat; const n: TLInt; var BothTails, LeftTail, RightTail: TLFloat);
{ Spearman's rank correlation coefficient significance test }
procedure SpearmanRankCorrelationSignificance(const r: TLFloat; const n: TLInt; var BothTails, LeftTail, RightTail: TLFloat);

{ Jarque-Bera test }
procedure JarqueBeraTest(const X: TLVec; const n: TLInt; var p: TLFloat);

{ Mann-Whitney U-test }
procedure MannWhitneyUTest(const X: TLVec; n: TLInt; const Y: TLVec; M: TLInt; var BothTails, LeftTail, RightTail: TLFloat);

{ Wilcoxon signed-rank test }
procedure WilcoxonSignedRankTest(const X: TLVec; n: TLInt; E: TLFloat; var BothTails, LeftTail, RightTail: TLFloat);
{$ENDREGION 'LowlevelDistribution'}
{$REGION 'LowLevelGauss'}
{
  Computation of nodes and weights for a Gauss quadrature formula

  The algorithm generates the N-point Gauss quadrature formula with weight
  function given by coefficients alpha and beta of a recurrence relation
  which generates a system of orthogonal polynomials:

  P-1(x)   =  0
  P0(x)    =  1
  Pn+1(x)  =  (x-alpha(n))*Pn(x)  -  beta(n)*Pn-1(x)

  and zeroth moment Mu0

  Mu0 = integral(W(x)dx,a,b)
}
procedure GaussQuadratureGenerateRec(const alpha, beta: TLVec; const Mu0: TLFloat; n: TLInt; var Info: TLInt; var X: TLVec; var w: TLVec);
{
  Computation of nodes and weights for a Gauss-Lobatto quadrature formula

  The algorithm generates the N-point Gauss-Lobatto quadrature formula with
  weight function given by coefficients alpha and beta of a recurrence which
  generates a system of orthogonal polynomials.

  P-1(x)   =  0
  P0(x)    =  1
  Pn+1(x)  =  (x-alpha(n))*Pn(x)  -  beta(n)*Pn-1(x)

  and zeroth moment Mu0

  Mu0 = integral(W(x)dx,a,b)
}
procedure GaussQuadratureGenerateGaussLobattoRec(const alpha, beta: TLVec; const Mu0, a, b: TLFloat; n: TLInt; var Info: TLInt; var X: TLVec; var w: TLVec);
{
  Computation of nodes and weights for a Gauss-Radau quadrature formula

  The algorithm generates the N-point Gauss-Radau quadrature formula with
  weight function given by the coefficients alpha and beta of a recurrence
  which generates a system of orthogonal polynomials.

  P-1(x)   =  0
  P0(x)    =  1
  Pn+1(x)  =  (x-alpha(n))*Pn(x)  -  beta(n)*Pn-1(x)

  and zeroth moment Mu0

  Mu0 = integral(W(x)dx,a,b)
}
procedure GaussQuadratureGenerateGaussRadauRec(const alpha, beta: TLVec; const Mu0, a: TLFloat; n: TLInt; var Info: TLInt; var X: TLVec; var w: TLVec);

{ Returns nodes/weights for Gauss-Legendre quadrature on [-1,1] with N nodes }
procedure GaussQuadratureGenerateGaussLegendre(const n: TLInt; var Info: TLInt; var X: TLVec; var w: TLVec);

{ Returns nodes/weights for Gauss-Jacobi quadrature on [-1,1] with weight function W(x)=Power(1-x,Alpha)*Power(1+x,Beta) }
procedure GaussQuadratureGenerateGaussJacobi(const n: TLInt; const alpha, beta: TLFloat; var Info: TLInt; var X: TLVec; var w: TLVec);

{ Returns nodes/weights for Gauss-Laguerre quadrature on (0,+inf) with weight function W(x)=Power(x,Alpha)*Exp(-x) }
procedure GaussQuadratureGenerateGaussLaguerre(const n: TLInt; const alpha: TLFloat; var Info: TLInt; var X: TLVec; var w: TLVec);

{ Returns nodes/weights for Gauss-Hermite quadrature on (-inf,+inf) with weight function W(x)=Exp(-x*x) }
procedure GaussQuadratureGenerateGaussHermite(const n: TLInt; var Info: TLInt; var X: TLVec; var w: TLVec);

{
  Computation of nodes and weights of a Gauss-Kronrod quadrature formula

  The algorithm generates the N-point Gauss-Kronrod quadrature formula  with
  weight function given by coefficients alpha and beta of a recurrence
  relation which generates a system of orthogonal polynomials:

  P-1(x)   =  0
  P0(x)    =  1
  Pn+1(x)  =  (x-alpha(n))*Pn(x)  -  beta(n)*Pn-1(x)

  and zero moment Mu0

  Mu0 = integral(W(x)dx,a,b)
}
procedure GaussKronrodQuadratureGenerateRec(const alpha, beta: TLVec; const Mu0: TLFloat; n: TLInt; var Info: TLInt; var X, WKronrod, WGauss: TLVec);

{
  Returns Gauss and Gauss-Kronrod nodes/weights for Gauss-Legendre quadrature with N points.
  GKQLegendreCalc (calculation) or GKQLegendreTbl (precomputed table) is used depending on machine precision and number of nodes.
}
procedure GaussKronrodQuadratureGenerateGaussLegendre(const n: TLInt; var Info: TLInt; var X, WKronrod, WGauss: TLVec);

{
  Returns Gauss and Gauss-Kronrod nodes/weights for Gauss-Jacobi quadrature on [-1,1] with weight function
  W(x)=Power(1-x,Alpha)*Power(1+x,Beta).
}
procedure GaussKronrodQuadratureGenerateGaussJacobi(const n: TLInt; const alpha, beta: TLFloat; var Info: TLInt; var X, WKronrod, WGauss: TLVec);

{
  Returns Gauss and Gauss-Kronrod nodes for quadrature with N points.
  Reduction to tridiagonal eigenproblem is used.
}
procedure GaussKronrodQuadratureLegendreCalc(const n: TLInt; var Info: TLInt; var X, WKronrod, WGauss: TLVec);

{
  Returns Gauss and Gauss-Kronrod nodes for quadrature with N  points  using pre-calculated table. Nodes/weights were computed with accuracy up to 1.0E-32.
  In standard TLFloat  precision accuracy reduces to something about 2.0E-16 (depending  on your compiler's handling of long floating point constants).
}
procedure GaussKronrodQuadratureLegendreTbl(const n: TLInt; var X, WKronrod, WGauss: TLVec; var Eps: TLFloat);
{$ENDREGION 'LowLevelGauss'}
{$REGION 'Limited memory BFGS optimizer'}
procedure MinLBFGSCreate(n: TLInt; M: TLInt; const X: TLVec; var State: TMinLBFGSState);
procedure MinLBFGSSetCond(var State: TMinLBFGSState; EpsG: TLFloat; EpsF: TLFloat; EpsX: TLFloat; MAXITS: TLInt);
procedure MinLBFGSSetXRep(var State: TMinLBFGSState; NeedXRep: Boolean);
procedure MinLBFGSSetStpMax(var State: TMinLBFGSState; StpMax: TLFloat);
procedure MinLBFGSCreateX(n: TLInt; M: TLInt; const X: TLVec; Flags: TLInt; var State: TMinLBFGSState);
function MinLBFGSIteration(var State: TMinLBFGSState): Boolean;
procedure MinLBFGSResults(const State: TMinLBFGSState; var X: TLVec; var Rep: TMinLBFGSReport);
procedure MinLBFGSFree(var X: TLVec; var State: TMinLBFGSState);
{$ENDREGION 'Limited memory BFGS optimizer'}
{$REGION 'Improved Levenberg-Marquardt optimizer'}
procedure MinLMCreateFGH(const n: TLInt; const X: TLVec; var State: TMinLMState);
procedure MinLMCreateFGJ(const n: TLInt; const M: TLInt; const X: TLVec; var State: TMinLMState);
procedure MinLMCreateFJ(const n: TLInt; const M: TLInt; const X: TLVec; var State: TMinLMState);
procedure MinLMSetCond(var State: TMinLMState; EpsG: TLFloat; EpsF: TLFloat; EpsX: TLFloat; MAXITS: TLInt);
procedure MinLMSetXRep(var State: TMinLMState; NeedXRep: Boolean);
procedure MinLMSetStpMax(var State: TMinLMState; StpMax: TLFloat);
function MinLMIteration(var State: TMinLMState): Boolean;
procedure MinLMResults(const State: TMinLMState; var X: TLVec; var Rep: TMinLMReport);
{$ENDREGION 'Improved Levenberg-Marquardt optimizer'}
{$REGION 'neural network'}
procedure MLPCreate0(NIn, NOut: TLInt; var Network: TMultiLayerPerceptron);
procedure MLPCreate1(NIn, NHid, NOut: TLInt; var Network: TMultiLayerPerceptron);
procedure MLPCreate2(NIn, NHid1, NHid2, NOut: TLInt; var Network: TMultiLayerPerceptron);

procedure MLPCreateB0(NIn, NOut: TLInt; b, d: TLFloat; var Network: TMultiLayerPerceptron);
procedure MLPCreateB1(NIn, NHid, NOut: TLInt; b, d: TLFloat; var Network: TMultiLayerPerceptron);
procedure MLPCreateB2(NIn, NHid1, NHid2, NOut: TLInt; b, d: TLFloat; var Network: TMultiLayerPerceptron);

procedure MLPCreateR0(NIn, NOut: TLInt; a, b: TLFloat; var Network: TMultiLayerPerceptron);
procedure MLPCreateR1(NIn, NHid, NOut: TLInt; a, b: TLFloat; var Network: TMultiLayerPerceptron);
procedure MLPCreateR2(NIn, NHid1, NHid2, NOut: TLInt; a, b: TLFloat; var Network: TMultiLayerPerceptron);

procedure MLPCreateC0(NIn, NOut: TLInt; var Network: TMultiLayerPerceptron);
procedure MLPCreateC1(NIn, NHid, NOut: TLInt; var Network: TMultiLayerPerceptron);
procedure MLPCreateC2(NIn, NHid1, NHid2, NOut: TLInt; var Network: TMultiLayerPerceptron);

procedure MLPFree(var Network: TMultiLayerPerceptron);
procedure MLPCopy(const Network1: TMultiLayerPerceptron; var Network2: TMultiLayerPerceptron);

procedure MLPSerialize(const Network: TMultiLayerPerceptron; var ResArry: TLVec; var RLen: TLInt);
procedure MLPUNSerialize(const ResArry: TLVec; var Network: TMultiLayerPerceptron);

procedure MLPRandomize(var Network: TMultiLayerPerceptron); overload;
procedure MLPRandomize(var Network: TMultiLayerPerceptron; const Diameter: TLFloat); overload;
procedure MLPRandomize(var Network: TMultiLayerPerceptron; const WBest: TLVec; const Diameter: TLFloat); overload;
procedure MLPRandomizeFull(var Network: TMultiLayerPerceptron);

procedure MLPInitPreprocessor(var Network: TMultiLayerPerceptron; const xy: TLMatrix; SSize: TLInt);
procedure MLPProperties(const Network: TMultiLayerPerceptron; var NIn: TLInt; var NOut: TLInt; var WCount: TLInt);
function MLPIsSoftmax(const Network: TMultiLayerPerceptron): Boolean;

procedure MLPProcess(var Network: TMultiLayerPerceptron; const X: TLVec; var Y: TLVec);

function MLPError(var Network: TMultiLayerPerceptron; const xy: TLMatrix; SSize: TLInt): TLFloat;
function MLPErrorN(var Network: TMultiLayerPerceptron; const xy: TLMatrix; SSize: TLInt): TLFloat;
function MLPClsError(var Network: TMultiLayerPerceptron; const xy: TLMatrix; SSize: TLInt): TLInt;
function MLPRelClsError(var Network: TMultiLayerPerceptron; const xy: TLMatrix; NPoints: TLInt): TLFloat;
function MLPAvgCE(var Network: TMultiLayerPerceptron; const xy: TLMatrix; NPoints: TLInt): TLFloat;
function MLPRMSError(var Network: TMultiLayerPerceptron; const xy: TLMatrix; NPoints: TLInt): TLFloat;
function MLPAvgError(var Network: TMultiLayerPerceptron; const xy: TLMatrix; NPoints: TLInt): TLFloat;
function MLPAvgRelError(var Network: TMultiLayerPerceptron; const xy: TLMatrix; NPoints: TLInt): TLFloat;

procedure MLPGrad(var Network: TMultiLayerPerceptron; const X: TLVec; const DesiredY: TLVec; var E: TLFloat; var Grad: TLVec);
procedure MLPGradN(var Network: TMultiLayerPerceptron; const X: TLVec; const DesiredY: TLVec; var E: TLFloat; var Grad: TLVec);
procedure MLPGradBatch(var Network: TMultiLayerPerceptron; const xy: TLMatrix; SSize: TLInt; var E: TLFloat; var Grad: TLVec);
procedure MLPGradNBatch(var Network: TMultiLayerPerceptron; const xy: TLMatrix; SSize: TLInt; var E: TLFloat; var Grad: TLVec);

procedure MLPHessianNBatch(var Network: TMultiLayerPerceptron; const xy: TLMatrix; SSize: TLInt; var E: TLFloat; var Grad: TLVec; var h: TLMatrix);
procedure MLPHessianBatch(var Network: TMultiLayerPerceptron; const xy: TLMatrix; SSize: TLInt; var E: TLFloat; var Grad: TLVec; var h: TLMatrix);

procedure MLPInternalProcessVector(const StructInfo: TLIVec;
  const Weights: TLVec; const ColumnMeans: TLVec;
  const ColumnSigmas: TLVec; var Neurons: TLVec;
  var DFDNET: TLVec; const X: TLVec; var Y: TLVec);

procedure MLPTrainLM(var Network: TMultiLayerPerceptron; const xy: TLMatrix;
  NPoints: TLInt; Decay: TLFloat; Restarts: TLInt;
  var Info: TLInt; var Rep: TMLPReport);

procedure MLPTrainLM_MT(var Network: TMultiLayerPerceptron; const xy: TLMatrix;
  NPoints: TLInt; Decay: TLFloat; Restarts: TLInt;
  var Info: TLInt; var Rep: TMLPReport);

procedure MLPTrainLBFGS(var Network: TMultiLayerPerceptron;
  const xy: TLMatrix; NPoints: TLInt; Decay: TLFloat;
  Restarts: TLInt; WStep: TLFloat; MAXITS: TLInt;
  var Info: TLInt; var Rep: TMLPReport; IsTerminated: PBoolean;
  out EBest: TLFloat);

procedure MLPTrainLBFGS_MT(var Network: TMultiLayerPerceptron;
  const xy: TLMatrix; NPoints: TLInt; Decay: TLFloat;
  Restarts: TLInt; WStep: TLFloat; MAXITS: TLInt;
  var Info: TLInt; var Rep: TMLPReport);

procedure MLPTrainLBFGS_MT_Mod(var Network: TMultiLayerPerceptron;
  const xy: TLMatrix; NPoints: TLInt; Restarts: TLInt;
  WStep, Diameter: TLFloat; MAXITS: TLInt;
  var Info: TLInt; var Rep: TMLPReport);

procedure MLPTrainMonteCarlo(var Network: TMultiLayerPerceptron; const xy: TLMatrix; NPoints: TLInt;
  const MainRestarts, SubRestarts: TLInt; const MinError: TLFloat;
  Diameter: TLFloat; var Info: TLInt; var Rep: TMLPReport);

procedure MLPKFoldCVLBFGS(const Network: TMultiLayerPerceptron;
  const xy: TLMatrix; NPoints: TLInt; Decay: TLFloat;
  Restarts: TLInt; WStep: TLFloat; MAXITS: TLInt;
  FoldsCount: TLInt; var Info: TLInt; var Rep: TMLPReport;
  var CVRep: TMLPCVReport);

procedure MLPKFoldCVLM(const Network: TMultiLayerPerceptron;
  const xy: TLMatrix; NPoints: TLInt; Decay: TLFloat;
  Restarts: TLInt; FoldsCount: TLInt; var Info: TLInt;
  var Rep: TMLPReport; var CVRep: TMLPCVReport);
{$ENDREGION 'neural network'}
{$REGION 'Neural networks ensemble'}
procedure MLPECreate0(NIn, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
procedure MLPECreate1(NIn, NHid, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
procedure MLPECreate2(NIn, NHid1, NHid2, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);

procedure MLPECreateB0(NIn, NOut: TLInt; b, d: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
procedure MLPECreateB1(NIn, NHid, NOut: TLInt; b, d: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
procedure MLPECreateB2(NIn, NHid1, NHid2, NOut: TLInt; b, d: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);

procedure MLPECreateR0(NIn, NOut: TLInt; a, b: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
procedure MLPECreateR1(NIn, NHid, NOut: TLInt; a, b: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
procedure MLPECreateR2(NIn, NHid1, NHid2, NOut: TLInt; a, b: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);

procedure MLPECreateC0(NIn, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
procedure MLPECreateC1(NIn, NHid, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
procedure MLPECreateC2(NIn, NHid1, NHid2, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);

procedure MLPECreateFromNetwork(const Network: TMultiLayerPerceptron; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);

procedure MLPECopy(const Ensemble1: TMLPEnsemble; var Ensemble2: TMLPEnsemble);
procedure MLPESerialize(var Ensemble: TMLPEnsemble; var ResArry: TLVec; var RLen: TLInt);
procedure MLPEUNSerialize(const ResArry: TLVec; var Ensemble: TMLPEnsemble);

procedure MLPERandomize(var Ensemble: TMLPEnsemble);

procedure MLPEProperties(const Ensemble: TMLPEnsemble; var NIn: TLInt; var NOut: TLInt);

function MLPEIsSoftmax(const Ensemble: TMLPEnsemble): Boolean;

procedure MLPEProcess(var Ensemble: TMLPEnsemble; const X: TLVec; var Y: TLVec);

function MLPERelClsError(var Ensemble: TMLPEnsemble; const xy: TLMatrix; NPoints: TLInt): TLFloat;
function MLPEAvgCE(var Ensemble: TMLPEnsemble; const xy: TLMatrix; NPoints: TLInt): TLFloat;
function MLPERMSError(var Ensemble: TMLPEnsemble; const xy: TLMatrix; NPoints: TLInt): TLFloat;
function MLPEAvgError(var Ensemble: TMLPEnsemble; const xy: TLMatrix; NPoints: TLInt): TLFloat;
function MLPEAvgRelError(var Ensemble: TMLPEnsemble; const xy: TLMatrix; NPoints: TLInt): TLFloat;

procedure MLPEBaggingLM(const MultiThread: Boolean; var Ensemble: TMLPEnsemble; const xy: TLMatrix;
  NPoints: TLInt; Decay: TLFloat; Restarts: TLInt;
  var Info: TLInt; var Rep: TMLPReport; var OOBErrors: TMLPCVReport);

procedure MLPEBaggingLBFGS(const MultiThread: Boolean; var Ensemble: TMLPEnsemble; const xy: TLMatrix;
  NPoints: TLInt; Decay: TLFloat; Restarts: TLInt;
  WStep: TLFloat; MAXITS: TLInt; var Info: TLInt;
  var Rep: TMLPReport; var OOBErrors: TMLPCVReport);
{$ENDREGION 'Neural networks ensemble'}
{$REGION 'Random Decision Forest'}
procedure DFBuildRandomDecisionForest(const xy: TLMatrix; NPoints, NVars, NClasses, NTrees: TLInt; r: TLFloat; var Info: TLInt; var df: TDecisionForest; var Rep: TDFReport);
procedure DFProcess(const df: TDecisionForest; const X: TLVec; var Y: TLVec);
function DFRelClsError(const df: TDecisionForest; const xy: TLMatrix; NPoints: TLInt): TLFloat;
function DFAvgCE(const df: TDecisionForest; const xy: TLMatrix; NPoints: TLInt): TLFloat;
function DFRMSError(const df: TDecisionForest; const xy: TLMatrix; NPoints: TLInt): TLFloat;
function DFAvgError(const df: TDecisionForest; const xy: TLMatrix; NPoints: TLInt): TLFloat;
function DFAvgRelError(const df: TDecisionForest; const xy: TLMatrix; NPoints: TLInt): TLFloat;
procedure DFCopy(const DF1: TDecisionForest; var DF2: TDecisionForest);
procedure DFSerialize(const df: TDecisionForest; var ResArry: TLVec; var RLen: TLInt);
procedure DFUnserialize(const ResArry: TLVec; var df: TDecisionForest);
{$ENDREGION 'Random Decision Forest'}
{$REGION 'LogitModel'}

procedure MNLTrainH(const xy: TLMatrix; NPoints: TLInt; NVars: TLInt; NClasses: TLInt; var Info: TLInt; var LM: TLogitModel; var Rep: TMNLReport);
procedure MNLProcess(var LM: TLogitModel; const X: TLVec; var Y: TLVec);
procedure MNLUnpack(const LM: TLogitModel; var a: TLMatrix; var NVars: TLInt; var NClasses: TLInt);
procedure MNLPack(const a: TLMatrix; NVars: TLInt; NClasses: TLInt; var LM: TLogitModel);
procedure MNLCopy(const LM1: TLogitModel; var LM2: TLogitModel);
procedure MNLSerialize(const LM: TLogitModel; var ResArry: TLVec; var RLen: TLInt);
procedure MNLUnserialize(const ResArry: TLVec; var LM: TLogitModel);
function MNLAvgCE(var LM: TLogitModel; const xy: TLMatrix; NPoints: TLInt): TLFloat;
function MNLRelClsError(var LM: TLogitModel; const xy: TLMatrix; NPoints: TLInt): TLFloat;
function MNLRMSError(var LM: TLogitModel; const xy: TLMatrix; NPoints: TLInt): TLFloat;
function MNLAvgError(var LM: TLogitModel; const xy: TLMatrix; NPoints: TLInt): TLFloat;
function MNLAvgRelError(var LM: TLogitModel; const xy: TLMatrix; SSize: TLInt): TLFloat;
function MNLClsError(var LM: TLogitModel; const xy: TLMatrix; NPoints: TLInt): TLInt;
{$ENDREGION 'LogitModel'}
{$REGION 'fitting'}

{ Least squares fitting }
procedure LSFitLinearW(Y: TLVec; w: TLVec; FMatrix: TLMatrix; n: TLInt; M: TLInt; var Info: TLInt; var c: TLVec; var Rep: TLSFitReport);
procedure LSFitLinearWC(Y: TLVec; w: TLVec; FMatrix: TLMatrix; CMatrix: TLMatrix; n: TLInt; M: TLInt; K: TLInt; var Info: TLInt; var c: TLVec; var Rep: TLSFitReport);
procedure LSFitLinear(Y: TLVec; FMatrix: TLMatrix; n: TLInt; M: TLInt; var Info: TLInt; var c: TLVec; var Rep: TLSFitReport);
procedure LSFitLinearC(Y: TLVec; FMatrix: TLMatrix; CMatrix: TLMatrix; n: TLInt; M: TLInt; K: TLInt; var Info: TLInt; var c: TLVec; var Rep: TLSFitReport);
procedure LSFitNonlinearWFG(X: TLMatrix; Y: TLVec; w: TLVec; c: TLVec; n: TLInt; M: TLInt; K: TLInt; CheapFG: Boolean; var State: TLSFitState);
procedure LSFitNonlinearFG(X: TLMatrix; Y: TLVec; c: TLVec; n: TLInt; M: TLInt; K: TLInt; CheapFG: Boolean; var State: TLSFitState);
procedure LSFitNonlinearWFGH(X: TLMatrix; Y: TLVec; w: TLVec; c: TLVec; n: TLInt; M: TLInt; K: TLInt; var State: TLSFitState);
procedure LSFitNonlinearFGH(X: TLMatrix; Y: TLVec; c: TLVec; n: TLInt; M: TLInt; K: TLInt; var State: TLSFitState);
procedure LSFitNonlinearSetCond(var State: TLSFitState; EpsF: TLFloat; EpsX: TLFloat; MAXITS: TLInt);
procedure LSFitNonlinearSetStpMax(var State: TLSFitState; StpMax: TLFloat);
function LSFitNonlinearIteration(var State: TLSFitState): Boolean;
procedure LSFitNonlinearResults(State: TLSFitState; var Info: TLInt; var c: TLVec; var Rep: TLSFitReport);
procedure LSFitScaleXY(var X, Y: TLVec; n: TLInt; var XC, YC: TLVec; DC: TLIVec; K: TLInt; var XA, XB, SA, SB: TLFloat; var XOriginal, YOriginal: TLVec);

{ Barycentric fitting }
function BarycentricCalc(b: TBarycentricInterpolant; t: TLFloat): TLFloat;
procedure BarycentricDiff1(b: TBarycentricInterpolant; t: TLFloat; var f: TLFloat; var df: TLFloat);
procedure BarycentricDiff2(b: TBarycentricInterpolant; t: TLFloat; var f: TLFloat; var df: TLFloat; var D2F: TLFloat);
procedure BarycentricLinTransX(var b: TBarycentricInterpolant; ca: TLFloat; CB: TLFloat);
procedure BarycentricLinTransY(var b: TBarycentricInterpolant; ca: TLFloat; CB: TLFloat);
procedure BarycentricUnpack(b: TBarycentricInterpolant; var n: TLInt; var X: TLVec; var Y: TLVec; var w: TLVec);
procedure BarycentricSerialize(b: TBarycentricInterpolant; var ResArry: TLVec; var ResLen: TLInt);
procedure BarycentricUnserialize(ResArry: TLVec; var b: TBarycentricInterpolant);
procedure BarycentricCopy(b: TBarycentricInterpolant; var b2: TBarycentricInterpolant);
procedure BarycentricBuildXYW(X, Y, w: TLVec; n: TLInt; var b: TBarycentricInterpolant);
procedure BarycentricBuildFloaterHormann(X, Y: TLVec; n: TLInt; d: TLInt; var b: TBarycentricInterpolant);
procedure BarycentricFitFloaterHormannWC(X, Y, w: TLVec; n: TLInt; XC, YC: TLVec; DC: TLIVec; K, M: TLInt; var Info: TLInt; var b: TBarycentricInterpolant; var Rep: TBarycentricFitReport);
procedure BarycentricFitFloaterHormann(X, Y: TLVec; n: TLInt; M: TLInt; var Info: TLInt; var b: TBarycentricInterpolant; var Rep: TBarycentricFitReport);

{ Polynomial fitting }
procedure PolynomialBuild(X, Y: TLVec; n: TLInt; var p: TBarycentricInterpolant);
procedure PolynomialBuildEqDist(a: TLFloat; b: TLFloat; Y: TLVec; n: TLInt; var p: TBarycentricInterpolant);
procedure PolynomialBuildCheb1(a: TLFloat; b: TLFloat; Y: TLVec; n: TLInt; var p: TBarycentricInterpolant);
procedure PolynomialBuildCheb2(a: TLFloat; b: TLFloat; Y: TLVec; n: TLInt; var p: TBarycentricInterpolant);
function PolynomialCalcEqDist(a: TLFloat; b: TLFloat; f: TLVec; n: TLInt; t: TLFloat): TLFloat;
function PolynomialCalcCheb1(a: TLFloat; b: TLFloat; f: TLVec; n: TLInt; t: TLFloat): TLFloat;
function PolynomialCalcCheb2(a: TLFloat; b: TLFloat; f: TLVec; n: TLInt; t: TLFloat): TLFloat;
procedure PolynomialFit(X, Y: TLVec; n, M: TLInt; var Info: TLInt; var p: TBarycentricInterpolant; var Rep: TPolynomialFitReport);
procedure PolynomialFitWC(X, Y, w: TLVec; n: TLInt; XC, YC: TLVec; DC: TLIVec; K: TLInt; M: TLInt; var Info: TLInt; var p: TBarycentricInterpolant; var Rep: TPolynomialFitReport);

{ Spline1D fitting }
procedure Spline1DBuildLinear(X, Y: TLVec; n: TLInt; var c: TSpline1DInterpolant);
procedure Spline1DBuildCubic(X, Y: TLVec; n: TLInt; BoundLType: TLInt; BoundL: TLFloat; BoundRType: TLInt; BoundR: TLFloat; var c: TSpline1DInterpolant);
procedure Spline1DBuildCatmullRom(X, Y: TLVec; n: TLInt; BoundType: TLInt; Tension: TLFloat; var c: TSpline1DInterpolant);
procedure Spline1DBuildHermite(X, Y: TLVec; d: TLVec; n: TLInt; var c: TSpline1DInterpolant);
procedure Spline1DBuildAkima(X, Y: TLVec; n: TLInt; var c: TSpline1DInterpolant);
procedure Spline1DFitCubicWC(X, Y, w: TLVec; n: TLInt; XC: TLVec; YC: TLVec; DC: TLIVec; K: TLInt; M: TLInt; var Info: TLInt; var s: TSpline1DInterpolant; var Rep: TSpline1DFitReport);
procedure Spline1DFitHermiteWC(X, Y, w: TLVec; n: TLInt; XC: TLVec; YC: TLVec; DC: TLIVec; K: TLInt; M: TLInt; var Info: TLInt; var s: TSpline1DInterpolant; var Rep: TSpline1DFitReport);
procedure Spline1DFitCubic(X, Y: TLVec; n: TLInt; M: TLInt; var Info: TLInt; var s: TSpline1DInterpolant; var Rep: TSpline1DFitReport);
procedure Spline1DFitHermite(X, Y: TLVec; n: TLInt; M: TLInt; var Info: TLInt; var s: TSpline1DInterpolant; var Rep: TSpline1DFitReport);
function Spline1DCalc(c: TSpline1DInterpolant; X: TLFloat): TLFloat;
procedure Spline1DDiff(c: TSpline1DInterpolant; X: TLFloat; var s: TLFloat; var DS: TLFloat; var D2S: TLFloat);
procedure Spline1DCopy(c: TSpline1DInterpolant; var CC: TSpline1DInterpolant);
procedure Spline1DUnpack(c: TSpline1DInterpolant; var n: TLInt; var Tbl: TLMatrix);
procedure Spline1DLinTransX(var c: TSpline1DInterpolant; a: TLFloat; b: TLFloat);
procedure Spline1DLinTransY(var c: TSpline1DInterpolant; a: TLFloat; b: TLFloat);
function Spline1DIntegrate(c: TSpline1DInterpolant; X: TLFloat): TLFloat;

{$ENDREGION 'fitting'}
{$REGION 'Portable high quality random number'}
procedure HQRNDRandomize(var State: THQRNDState);
procedure HQRNDSeed(const s1, s2: TLInt; var State: THQRNDState);
function HQRNDUniformR(var State: THQRNDState): TLFloat;
function HQRNDUniformI(const n: TLInt; var State: THQRNDState): TLInt;
function HQRNDNormal(var State: THQRNDState): TLFloat;
procedure HQRNDUnit2(var State: THQRNDState; var X: TLFloat; var Y: TLFloat);
procedure HQRNDNormal2(var State: THQRNDState; var x1: TLFloat; var x2: TLFloat);
function HQRNDExponential(const LAMBDA: TLFloat; var State: THQRNDState): TLFloat;
{$ENDREGION 'Portable high quality random number'}
{$REGION 'Generation of random matrix'}
procedure RMatrixRndOrthogonal(n: TLInt; var a: TLMatrix);
procedure RMatrixRndCond(n: TLInt; c: TLFloat; var a: TLMatrix);
procedure CMatrixRndOrthogonal(n: TLInt; var a: TLComplexMatrix);
procedure CMatrixRndCond(n: TLInt; c: TLFloat; var a: TLComplexMatrix);
procedure SMatrixRndCond(n: TLInt; c: TLFloat; var a: TLMatrix);
procedure SPDMatrixRndCond(n: TLInt; c: TLFloat; var a: TLMatrix);
procedure HMatrixRndCond(n: TLInt; c: TLFloat; var a: TLComplexMatrix);
procedure HPDMatrixRndCond(n: TLInt; c: TLFloat; var a: TLComplexMatrix);
procedure RMatrixRndOrthogonalFromTheRight(var a: TLMatrix; M: TLInt; n: TLInt);
procedure RMatrixRndOrthogonalFromTheLeft(var a: TLMatrix; M: TLInt; n: TLInt);
procedure CMatrixRndOrthogonalFromTheRight(var a: TLComplexMatrix; M: TLInt; n: TLInt);
procedure CMatrixRndOrthogonalFromTheLeft(var a: TLComplexMatrix; M: TLInt; n: TLInt);
procedure SMatrixRndMultiply(var a: TLMatrix; n: TLInt);
procedure HMatrixRndMultiply(var a: TLComplexMatrix; n: TLInt);
{$ENDREGION 'Generation of random matrix'}
{$REGION 'fft'}
{ generates FFT plan }
procedure FTBaseGenerateComplexFFTPlan(n: TLInt; var Plan: TFTPlan);
procedure FTBaseGenerateRealFFTPlan(n: TLInt; var Plan: TFTPlan);
procedure FTBaseGenerateRealFHTPlan(n: TLInt; var Plan: TFTPlan);
procedure FTBaseExecutePlan(var a: TLVec; AOffset: TLInt; n: TLInt; var Plan: TFTPlan);
procedure FTBaseExecutePlanRec(var a: TLVec; AOffset: TLInt; var Plan: TFTPlan; EntryOffset: TLInt; StackPtr: TLInt);
procedure FTBaseFactorize(n: TLInt; TaskType: TLInt; var n1: TLInt; var n2: TLInt);
function FTBaseIsSmooth(n: TLInt): Boolean;
function FTBaseFindSmooth(n: TLInt): TLInt;
function FTBaseFindSmoothEven(n: TLInt): TLInt;
function FTBaseGetFLOPEstimate(n: TLInt): TLFloat;
{ 1-dimensional TLComplex FFT. }
procedure FFTC1D(var a: TLComplexVec; n: TLInt);
{ 1-dimensional TLComplex inverse FFT. }
procedure FFTC1DInv(var a: TLComplexVec; n: TLInt);
{ 1-dimensional real FFT. }
procedure FFTR1D(const a: TLVec; n: TLInt; var f: TLComplexVec);
{ 1-dimensional real inverse FFT. }
procedure FFTR1DInv(const f: TLComplexVec; n: TLInt; var a: TLVec);
{$ENDREGION 'fft'}
{$REGION 'test'}
procedure LearnTest;
{$ENDREGION 'test'}

implementation

uses KM, DoStatusIO, TextParsing, zExpression, OpCode;

{$REGION 'Include'}
{$INCLUDE learn_base.inc}
{$INCLUDE learn_blas.inc}
{$INCLUDE learn_ablas.inc}
{$INCLUDE learn_trfac.inc}
{$INCLUDE learn_safesolve.inc}
{$INCLUDE learn_rcond.inc}
{$INCLUDE learn_matinv.inc}
{$INCLUDE learn_linmin.inc}
{$INCLUDE learn_lbfgs.inc}
{$INCLUDE learn_rotations.inc}
{$INCLUDE learn_ortfac.inc}
{$INCLUDE learn_bdsvd.inc}
{$INCLUDE learn_svd.inc}
{$INCLUDE learn_densesolver.inc}
{$INCLUDE learn_minlm.inc}
{$INCLUDE learn_trainbase.inc}
{$INCLUDE learn_train.inc}
{$INCLUDE learn_trainEnsemble.inc}
{$INCLUDE learn_schur.inc}
{$INCLUDE learn_evd.inc}
{$INCLUDE learn_PCA.inc}
{$INCLUDE learn_LDA.inc}
{$INCLUDE learn_forest.inc}
{$INCLUDE learn_logit.inc}
{$INCLUDE learn_statistics_normal_distribution.inc}
{$INCLUDE learn_statistics_base.inc}
{$INCLUDE learn_statistics_IncompleteBeta.inc}
{$INCLUDE learn_statistics_fdistribution.inc}
{$INCLUDE learn_statistics_binomial_distribution.inc}
{$INCLUDE learn_statistics_chisquare_distribution.inc}
{$INCLUDE learn_statistics_StudentsT_distribution.inc}
{$INCLUDE learn_statistics_Pearson_Spearman.inc}
{$INCLUDE learn_statistics_JarqueBeraTest.inc}
{$INCLUDE learn_statistics_MannWhitneyUTest.inc}
{$INCLUDE learn_statistics_Wilcoxon.inc}
{$INCLUDE learn_gaussintegral.inc}
{$INCLUDE learn_fitting.inc}
{$INCLUDE learn_quality_random.inc}
{$INCLUDE learn_matgen.inc}
{$INCLUDE learn_fft.inc}
{$INCLUDE learn_extAPI.inc}
{$INCLUDE learn_th.inc}
{$INCLUDE learn_class.inc}
{$INCLUDE learn_test.inc}
{$ENDREGION 'Include'}


end.
