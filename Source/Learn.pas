{ ****************************************************************************** }
{ * machine Learn          writen by QQ 600585@qq.com                          * }
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

uses Math, CoreClasses, UnicodeMixedLib, PascalStrings, KDTree, LearnTypes, MemoryStream64, DataFrameEngine;

{$REGION 'Class'}


type
  TLearn = class;

  TLearnState_Call = procedure(const LSender: TLearn; const State: Boolean);
  TLearnState_Method = procedure(const LSender: TLearn; const State: Boolean) of object;
{$IFNDEF FPC} TLearnState_Proc = reference to procedure(const LSender: TLearn; const State: Boolean); {$ENDIF}

  TLearn = class(TCoreClassInterfacedObject)
  public type
    TLearnMemory = record
      m_in, m_out: TLVec;
      token: SystemString;
    end;

    PLearnMemory = ^TLearnMemory;
  private type
    TLearnKDT = record
      K: TKDTree;
    end;

    PLearnKDT = ^TLearnKDT;

    THideLayerDepth = (hld0, hld1, hld2);
  private
    FEnabledRandomNumber: Boolean;
    FInLen, FOutLen: TLInt;
    FMemorySource: TCoreClassList;
    FKDToken: TKDTree;
    FLearnType: TLearnType;
    FLearnData: Pointer;
    FClassifier: Boolean;
    FHideLayerDepth: THideLayerDepth;
    FLastTrainMaxInValue, FLastTrainMaxOutValue: TLFloat;
    FInfo: SystemString;
    FIsTraining: Boolean;
    FTrainThreadRuning: Boolean;
    FUserData: Pointer;
    FUserObject: TCoreClassObject;

    procedure KDInput(const IndexFor: NativeInt; var Source: TKDTree_Source; const Data: Pointer);
    procedure TokenInput(const IndexFor: NativeInt; var Source: TKDTree_Source; const Data: Pointer);

    procedure FreeLearnData;
    procedure CreateLearnData(const isTrainTime: Boolean);
  public
    // regression style
    class function CreateRegression(const lt: TLearnType; const InDataLen, OutDataLen: TLInt): TLearn;
    // regression style of level 1
    class function CreateRegression1(const lt: TLearnType; const InDataLen, OutDataLen: TLInt): TLearn;
    // regression style of level 2
    class function CreateRegression2(const lt: TLearnType; const InDataLen, OutDataLen: TLInt): TLearn;

    // classifier style
    class function CreateClassifier(const lt: TLearnType; const InDataLen: TLInt): TLearn;
    // classifier style of level 1
    class function CreateClassifier1(const lt: TLearnType; const InDataLen: TLInt): TLearn;
    // classifier style of level 2
    class function CreateClassifier2(const lt: TLearnType; const InDataLen: TLInt): TLearn;

    constructor Create; virtual;
    destructor Destroy; override;

    { * random number * }
    property EnabledRandomNumber: Boolean read FEnabledRandomNumber write FEnabledRandomNumber;

    { * clear * }
    procedure Clear;

    { * parameter * }
    function Count: TLInt;
    property InLen: TLInt read FInLen;
    property OutLen: TLInt read FOutLen;
    property LearnType: TLearnType read FLearnType;
    property Info: SystemString read FInfo;
    property TrainThreadRuning: Boolean read FTrainThreadRuning;
    function GetMemorySource(const index: TLInt): PLearnMemory;
    property MemorySource[const index: TLInt]: PLearnMemory read GetMemorySource; default;
    property LastTrainMaxInValue: TLFloat read FLastTrainMaxInValue;
    property LastTrainMaxOutValue: TLFloat read FLastTrainMaxOutValue;

    { * user parameter * }
    property UserData: Pointer read FUserData write FUserData;
    property UserObject: TCoreClassObject read FUserObject write FUserObject;

    { * sampler * }
    procedure AddMemory(const f_In, f_Out: TLVec; f_token: SystemString); overload;
    procedure AddMemory(const f_In: TLVec; f_token: SystemString); overload;
    procedure AddMemory(const f_In, f_Out: TLVec); overload;
    procedure AddMemory(const s_In, s_Out: SystemString); overload;
    procedure AddMemory(const s_In, s_Out, s_token: SystemString); overload;
    procedure AddMemory(const s: TPascalString); overload;
    procedure AddSampler(const f_In, f_Out: TLVec); overload;
    procedure AddSampler(const s_In, s_Out: SystemString); overload;
    procedure AddSampler(const s: TPascalString); overload;
    procedure AddMatrix(const m_in: TLMatrix; const f_Out: TLVec); overload;
    procedure AddMatrix(const m_in: TLMatrix; const f_Out: TLVec; const f_token: SystemString); overload;

    { * kdtree * }
    procedure AddKDTree(kd: TKDTreeDataList);

    { * normal train * }
    function Train(const TrainDepth: TLInt): Boolean; overload;
    function Train: Boolean; overload;
    { * train with thread * }
    procedure Train_MT; overload;
    procedure Train_MT(const TrainDepth: TLInt); overload;
    procedure TrainC(const TrainDepth: TLInt; const OnResult: TLearnState_Call);
    procedure TrainM(const TrainDepth: TLInt; const OnResult: TLearnState_Method);
{$IFNDEF FPC} procedure TrainP(const TrainDepth: TLInt; const OnResult: TLearnState_Proc); {$ENDIF FPC}
    //
    // wait thread
    procedure WaitTrain;

    // token
    function SearchToken(const v: TLVec): SystemString;
    function SearchOutVecToken(const v: TLVec): SystemString;

    // data input/output
    function Process(const p_in, p_out: PLVec): Boolean; overload;
    function Process(const ProcessIn: PLVec): SystemString; overload;
    function Process(const ProcessIn: TLVec): SystemString; overload;
    function Process(const ProcessIn: TPascalString): SystemString; overload;
    function ProcessMatrix(const p_in: PLMatrix; const p_out: PLVec): Boolean; overload;
    function ProcessToken(const ProcessIn: PLVec): SystemString; overload;

    // result max value
    function ProcessMax(const ProcessIn: TLVec): TLFloat; overload;
    function ProcessMax(const ProcessIn: TLMatrix): TLFloat; overload;
    function ProcessMaxToken(const ProcessIn: TLVec): SystemString; overload;
    function ProcessMaxToken(const ProcessIn: TLMatrix): SystemString; overload;

    // result max index
    function ProcessMaxIndex(const ProcessIn: TLVec): TLInt; overload;
    function ProcessMaxIndex(const ProcessIn: TLMatrix): TLInt; overload;
    function ProcessMaxIndexToken(const ProcessIn: TLVec): SystemString; overload;
    function ProcessMaxIndexToken(const ProcessIn: TLMatrix): SystemString; overload;

    // result min value
    function ProcessMin(const ProcessIn: TLVec): TLFloat; overload;
    function ProcessMin(const ProcessIn: TLMatrix): TLFloat; overload;
    function ProcessMinToken(const ProcessIn: TLVec): SystemString; overload;
    function ProcessMinToken(const ProcessIn: TLMatrix): SystemString; overload;

    // result min index
    function ProcessMinIndex(const ProcessIn: TLVec): TLInt; overload;
    function ProcessMinIndex(const ProcessIn: TLMatrix): TLInt; overload;
    function ProcessMinIndexToken(const ProcessIn: TLVec): SystemString; overload;
    function ProcessMinIndexToken(const ProcessIn: TLMatrix): SystemString; overload;

    // result first value
    function ProcessFV(const ProcessIn: TLVec): TLFloat; overload;
    function ProcessFV(const ProcessIn: TLMatrix): TLFloat; overload;
    function ProcessFV(const ProcessIn: TPascalString): TLFloat; overload;

    // result last value
    function ProcessLV(const ProcessIn: TLVec): TLFloat; overload;
    function ProcessLV(const ProcessIn: TLMatrix): TLFloat; overload;
    function ProcessLV(const ProcessIn: TPascalString): TLFloat; overload;

    // search with Pearson
    function SearchMemoryPearson(const ProcessIn: TLVec): TLInt; overload;
    procedure SearchMemoryPearson(const ProcessIn: TLVec; out List: TLIVec); overload;

    // search with Spearman
    function SearchMemorySpearman(const ProcessIn: TLVec): TLInt; overload;
    procedure SearchMemorySpearman(const ProcessIn: TLVec; out List: TLIVec); overload;

    // search with euclidean metric:K
    function SearchMemoryDistance(const ProcessIn: TLVec): TLInt; overload;
    procedure SearchMemoryDistance(const ProcessIn: TLVec; out List: TLIVec); overload;

    { * fast binary store * }
    procedure SaveToDF(df: TDataFrameEngine);
    procedure LoadFromDF(df: TDataFrameEngine);

    { store support }
    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

{$IFNDEF FPC}
    { * json store support * }
    procedure SaveToJsonStream(stream: TCoreClassStream);
    procedure LoadFromJsonStream(stream: TCoreClassStream);

    procedure SaveToJsonFile(FileName: SystemString);
    procedure LoadFromJsonFile(FileName: SystemString);
{$ENDIF FPC}
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
function LVec(const veclen: TLInt; const VDef: TLFloat): TLVec; overload;
function LVec(const veclen: TLInt): TLVec; overload;
function LVec(const v: TLVec): TPascalString; overload;
function LVec(const M: TLMatrix; const veclen: TLInt): TLVec; overload;
function LVec(const M: TLMatrix): TLVec; overload;
function LVec(const s: TPascalString; const veclen: TLInt): TLVec; overload;
function LVec(const v: TLVec; const ShortFloat: Boolean): TPascalString; overload;
function LVec(const M: TLBMatrix; const veclen: TLInt): TLBVec; overload;
function LVec(const M: TLBMatrix): TLBVec; overload;
function LVec(const M: TLIMatrix; const veclen: TLInt): TLIVec; overload;
function LVec(const M: TLIMatrix): TLIVec; overload;
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
procedure LClampF(var AValue: TLFloat; const aMin, aMax: TLFloat); overload;
procedure LClampI(var AValue: TLInt; const aMin, aMax: TLInt); overload;
function LClamp(const AValue: TLFloat; const aMin, aMax: TLFloat): TLFloat; overload;
function LClamp(const AValue: TLInt; const aMin, aMax: TLInt): TLInt; overload;

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
function LDA(const M: TLMatrix; const cv: TLVec; const Nclass: TLInt; var sInfo: SystemString; var output: TLMatrix): Boolean; overload;
function LDA(const M: TLMatrix; const cv: TLVec; const Nclass: TLInt; var sInfo: SystemString; var output: TLVec): Boolean; overload;

{ * principal component analysis support * }
function PCA(const buff: TLMatrix; const NPoints, NVars: TLInt; var v: TLMatrix): TLInt;

{ * k-means++ clusterization support * }
function KMeans(const Source: TLMatrix; const NVars, K: TLInt; var KArray: TLMatrix; var kIndex: TLIVec): Boolean;

{ * init Matrix * }
function LMatrix(const L1, l2: TLInt): TLMatrix; overload;
function LBMatrix(const L1, l2: TLInt): TLBMatrix; overload;
function LIMatrix(const L1, l2: TLInt): TLIMatrix; overload;

{$ENDREGION 'LearnAPI'}

{$REGION 'FloatAPI'}
function AbsReal(x: TLFloat): TLFloat;
function AbsInt(i: TLInt): TLInt;
function RandomReal(): TLFloat;
function RandomInteger(i: TLInt): TLInt;
function Sign(x: TLFloat): TLInt;
function AP_Sqr(x: TLFloat): TLFloat;

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

function C_Complex(const x: TLFloat): TLComplex;
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

function AP_Float(x: TLFloat): TLFloat;
function AP_FP_Eq(x: TLFloat; y: TLFloat): Boolean;
function AP_FP_NEq(x: TLFloat; y: TLFloat): Boolean;
function AP_FP_Less(x: TLFloat; y: TLFloat): Boolean;
function AP_FP_Less_Eq(x: TLFloat; y: TLFloat): Boolean;
function AP_FP_Greater(x: TLFloat; y: TLFloat): Boolean;
function AP_FP_Greater_Eq(x: TLFloat; y: TLFloat): Boolean;

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
function VectorNorm2(const x: TLVec; const i1, i2: TLInt): TLFloat;
function VectorIdxAbsMax(const x: TLVec; const i1, i2: TLInt): TLInt;
function ColumnIdxAbsMax(const x: TLMatrix; const i1, i2, j: TLInt): TLInt;
function RowIdxAbsMax(const x: TLMatrix; const j1, j2, i: TLInt): TLInt;
function UpperHessenberg1Norm(const a: TLMatrix; const i1, i2, j1, j2: TLInt; var Work: TLVec): TLFloat;

procedure CopyMatrix(const a: TLMatrix; const IS1, IS2, JS1, JS2: TLInt;
  var b: TLMatrix; const ID1, id2, JD1, JD2: TLInt);

procedure InplaceTranspose(var a: TLMatrix; const i1, i2, j1, j2: TLInt; var Work: TLVec);

procedure CopyAndTranspose(const a: TLMatrix; IS1, IS2, JS1, JS2: TLInt;
  var b: TLMatrix; ID1, id2, JD1, JD2: TLInt);

procedure MatrixVectorMultiply(const a: TLMatrix; const i1, i2, j1, j2: TLInt; const Trans: Boolean;
  const x: TLVec; const IX1, IX2: TLInt; const alpha: TLFloat;
  var y: TLVec; const IY1, IY2: TLInt; const beta: TLFloat);

function Pythag2(x: TLFloat; y: TLFloat): TLFloat;

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
procedure CMatrixMV(M: TLInt; n: TLInt; var a: TLComplexMatrix; IA: TLInt; ja: TLInt; OpA: TLInt; var x: TLComplexVec; ix: TLInt; var y: TLComplexVec; iy: TLInt);
procedure RMatrixMV(M: TLInt; n: TLInt; var a: TLMatrix; IA: TLInt; ja: TLInt; OpA: TLInt; var x: TLVec; ix: TLInt; var y: TLVec; iy: TLInt);

procedure CMatrixRightTRSM(M: TLInt; n: TLInt;
  const a: TLComplexMatrix; i1: TLInt; j1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var x: TLComplexMatrix; i2: TLInt; j2: TLInt);

procedure CMatrixLeftTRSM(M: TLInt; n: TLInt;
  const a: TLComplexMatrix; i1: TLInt; j1: TLInt;
  IsUpper: Boolean; IsUnit: Boolean; OpType: TLInt;
  var x: TLComplexMatrix; i2: TLInt; j2: TLInt);

procedure RMatrixRightTRSM(M: TLInt; n: TLInt;
  const a: TLMatrix; i1: TLInt; j1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var x: TLMatrix; i2: TLInt; j2: TLInt);

procedure RMatrixLeftTRSM(M: TLInt; n: TLInt;
  const a: TLMatrix; i1: TLInt; j1: TLInt; IsUpper: Boolean;
  IsUnit: Boolean; OpType: TLInt; var x: TLMatrix; i2: TLInt; j2: TLInt);

procedure CMatrixSYRK(n: TLInt; K: TLInt; alpha: TLFloat;
  const a: TLComplexMatrix; IA: TLInt; ja: TLInt;
  OpTypeA: TLInt; beta: TLFloat; var c: TLComplexMatrix; IC: TLInt; JC: TLInt; IsUpper: Boolean);

procedure RMatrixSYRK(n: TLInt; K: TLInt; alpha: TLFloat;
  const a: TLMatrix; IA: TLInt; ja: TLInt;
  OpTypeA: TLInt; beta: TLFloat; var c: TLMatrix; IC: TLInt; JC: TLInt; IsUpper: Boolean);

procedure CMatrixGEMM(M: TLInt; n: TLInt; K: TLInt;
  alpha: TLComplex; const a: TLComplexMatrix; IA: TLInt;
  ja: TLInt; OpTypeA: TLInt; const b: TLComplexMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt; beta: TLComplex;
  var c: TLComplexMatrix; IC: TLInt; JC: TLInt);

procedure RMatrixGEMM(M: TLInt; n: TLInt; K: TLInt;
  alpha: TLFloat; const a: TLMatrix; IA: TLInt;
  ja: TLInt; OpTypeA: TLInt; const b: TLMatrix;
  IB: TLInt; JB: TLInt; OpTypeB: TLInt; beta: TLFloat;
  var c: TLMatrix; IC: TLInt; JC: TLInt);

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
  n: TLInt; var x: TLVec; IsUpper: Boolean; Trans: TLInt;
  IsUnit: Boolean; MaxGrowth: TLFloat): Boolean;

function CMatrixScaledTRSafeSolve(const a: TLComplexMatrix; SA: TLFloat;
  n: TLInt; var x: TLComplexVec; IsUpper: Boolean;
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
type
  TMatInvReport = record
    r1: TLFloat;
    RInf: TLFloat;
  end;

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
function NormalDistribution(const x: TLFloat): TLFloat;
function InvNormalDistribution(const y0: TLFloat): TLFloat;

{ statistics base }
function Log1P(const x: TLFloat): TLFloat;
function ExpM1(const x: TLFloat): TLFloat;
function CosM1(const x: TLFloat): TLFloat;
{ Gamma support }
function Gamma(const x: TLFloat): TLFloat;
{ Natural logarithm of gamma function }
function LnGamma(const x: TLFloat; var SgnGam: TLFloat): TLFloat;
{ Incomplete gamma integral }
function IncompleteGamma(const a, x: TLFloat): TLFloat;
{ Complemented incomplete gamma integral }
function IncompleteGammaC(const a, x: TLFloat): TLFloat;
{ Inverse of complemented imcomplete gamma integral }
function InvIncompleteGammaC(const a, y0: TLFloat): TLFloat;

{ Poisson distribution }
function PoissonDistribution(K: TLInt; M: TLFloat): TLFloat;
{ Complemented Poisson distribution }
function PoissonCDistribution(K: TLInt; M: TLFloat): TLFloat;
{ Inverse Poisson distribution }
function InvPoissonDistribution(K: TLInt; y: TLFloat): TLFloat;

{ Incomplete beta integral support }
function IncompleteBeta(a, b, x: TLFloat): TLFloat;
{ Inverse of imcomplete beta integral }
function InvIncompleteBeta(const a, b, y: TLFloat): TLFloat;

{ F distribution support }
function FDistribution(const a: TLInt; const b: TLInt; const x: TLFloat): TLFloat;
{ Complemented F distribution }
function FCDistribution(const a: TLInt; const b: TLInt; const x: TLFloat): TLFloat;
{ Inverse of complemented F distribution }
function InvFDistribution(const a: TLInt; const b: TLInt; const y: TLFloat): TLFloat;
{ Two-sample F-test }
procedure FTest(const x: TLVec; n: TLInt; const y: TLVec; M: TLInt; var BothTails, LeftTail, RightTail: TLFloat);

{ Binomial distribution support }
function BinomialDistribution(const K, n: TLInt; const p: TLFloat): TLFloat;
{ Complemented binomial distribution }
function BinomialCDistribution(const K, n: TLInt; const p: TLFloat): TLFloat;
{ Inverse binomial distribution }
function InvBinomialDistribution(const K, n: TLInt; const y: TLFloat): TLFloat;
{ Sign test }
procedure OneSampleSignTest(const x: TLVec; n: TLInt; Median: TLFloat; var BothTails, LeftTail, RightTail: TLFloat);

{ Chi-square distribution support }
function ChiSquareDistribution(const v, x: TLFloat): TLFloat;
{ Complemented Chi-square distribution }
function ChiSquareCDistribution(const v, x: TLFloat): TLFloat;
{ Inverse of complemented Chi-square distribution }
function InvChiSquareDistribution(const v, y: TLFloat): TLFloat;
{ One-sample chi-square test }
procedure OneSampleVarianceTest(const x: TLVec; n: TLInt; Variance: TLFloat; var BothTails, LeftTail, RightTail: TLFloat);

{ Student's t distribution support }
function StudentTDistribution(const K: TLInt; const t: TLFloat): TLFloat;
{ Functional inverse of Student's t distribution }
function InvStudentTDistribution(const K: TLInt; p: TLFloat): TLFloat;
{ One-sample t-test }
procedure StudentTTest1(const x: TLVec; n: TLInt; Mean: TLFloat; var BothTails, LeftTail, RightTail: TLFloat);
{ Two-sample pooled test }
procedure StudentTTest2(const x: TLVec; n: TLInt; const y: TLVec; M: TLInt; var BothTails, LeftTail, RightTail: TLFloat);
{ Two-sample unpooled test }
procedure UnequalVarianceTTest(const x: TLVec; n: TLInt; const y: TLVec; M: TLInt; var BothTails, LeftTail, RightTail: TLFloat);

{ Pearson and Spearman distribution support }
{ Pearson product-moment correlation coefficient }
function PearsonCorrelation(const x, y: TLVec; const n: TLInt): TLFloat;
{ Spearman's rank correlation coefficient }
function SpearmanRankCorrelation(const x, y: TLVec; const n: TLInt): TLFloat;
procedure SpearmanRank(var x: TLVec; n: TLInt);
{ Pearson's correlation coefficient significance test }
procedure PearsonCorrelationSignificance(const r: TLFloat; const n: TLInt; var BothTails, LeftTail, RightTail: TLFloat);
{ Spearman's rank correlation coefficient significance test }
procedure SpearmanRankCorrelationSignificance(const r: TLFloat; const n: TLInt; var BothTails, LeftTail, RightTail: TLFloat);

{ Jarque-Bera test }
procedure JarqueBeraTest(const x: TLVec; const n: TLInt; var p: TLFloat);

{ Mann-Whitney U-test }
procedure MannWhitneyUTest(const x: TLVec; n: TLInt; const y: TLVec; M: TLInt; var BothTails, LeftTail, RightTail: TLFloat);

{ Wilcoxon signed-rank test }
procedure WilcoxonSignedRankTest(const x: TLVec; n: TLInt; E: TLFloat; var BothTails, LeftTail, RightTail: TLFloat);
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
procedure GaussQuadratureGenerateRec(const alpha, beta: TLVec; const Mu0: TLFloat; n: TLInt; var Info: TLInt; var x: TLVec; var w: TLVec);
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
procedure GaussQuadratureGenerateGaussLobattoRec(const alpha, beta: TLVec; const Mu0, a, b: TLFloat; n: TLInt; var Info: TLInt; var x: TLVec; var w: TLVec);
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
procedure GaussQuadratureGenerateGaussRadauRec(const alpha, beta: TLVec; const Mu0, a: TLFloat; n: TLInt; var Info: TLInt; var x: TLVec; var w: TLVec);

{ Returns nodes/weights for Gauss-Legendre quadrature on [-1,1] with N nodes }
procedure GaussQuadratureGenerateGaussLegendre(const n: TLInt; var Info: TLInt; var x: TLVec; var w: TLVec);
{ Returns nodes/weights for Gauss-Jacobi quadrature on [-1,1] with weight function W(x)=Power(1-x,Alpha)*Power(1+x,Beta) }
procedure GaussQuadratureGenerateGaussJacobi(const n: TLInt; const alpha, beta: TLFloat; var Info: TLInt; var x: TLVec; var w: TLVec);
{ Returns nodes/weights for Gauss-Laguerre quadrature on (0,+inf) with weight function W(x)=Power(x,Alpha)*Exp(-x) }
procedure GaussQuadratureGenerateGaussLaguerre(const n: TLInt; const alpha: TLFloat; var Info: TLInt; var x: TLVec; var w: TLVec);
{ Returns nodes/weights for Gauss-Hermite quadrature on (-inf,+inf) with weight function W(x)=Exp(-x*x) }
procedure GaussQuadratureGenerateGaussHermite(const n: TLInt; var Info: TLInt; var x: TLVec; var w: TLVec);

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
procedure GaussKronrodQuadratureGenerateRec(const alpha, beta: TLVec; const Mu0: TLFloat; n: TLInt; var Info: TLInt; var x, WKronrod, WGauss: TLVec);
{
  Returns Gauss and Gauss-Kronrod nodes/weights for Gauss-Legendre quadrature with N points.
  GKQLegendreCalc (calculation) or GKQLegendreTbl (precomputed table) is used depending on machine precision and number of nodes.
}
procedure GaussKronrodQuadratureGenerateGaussLegendre(const n: TLInt; var Info: TLInt; var x, WKronrod, WGauss: TLVec);
{
  Returns Gauss and Gauss-Kronrod nodes/weights for Gauss-Jacobi quadrature on [-1,1] with weight function
  W(x)=Power(1-x,Alpha)*Power(1+x,Beta).
}
procedure GaussKronrodQuadratureGenerateGaussJacobi(const n: TLInt; const alpha, beta: TLFloat; var Info: TLInt; var x, WKronrod, WGauss: TLVec);
{
  Returns Gauss and Gauss-Kronrod nodes for quadrature with N points.
  Reduction to tridiagonal eigenproblem is used.
}
procedure GaussKronrodQuadratureLegendreCalc(const n: TLInt; var Info: TLInt; var x, WKronrod, WGauss: TLVec);
{
  Returns Gauss and Gauss-Kronrod nodes for quadrature with N  points  using pre-calculated table. Nodes/weights were computed with accuracy up to 1.0E-32.
  In standard TLFloat  precision accuracy reduces to something about 2.0E-16 (depending  on your compiler's handling of long floating point constants).
}
procedure GaussKronrodQuadratureLegendreTbl(const n: TLInt; var x, WKronrod, WGauss: TLVec; var Eps: TLFloat);
{$ENDREGION 'LowLevelGauss'}

procedure LearnTest;

const
  // IEEE floating
  MachineEpsilon = 5.0E-16;
  MaxRealNumber = 1.0E300;
  MinRealNumber = 1.0E-300;

implementation

uses KM,
{$IFDEF FPC}
  mtprocs,
{$ELSE}
  Threading,
{$ENDIF FPC}
  SyncObjs, DoStatusIO;

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
{$INCLUDE learn_extAPI.inc}
{$INCLUDE learn_th.inc}
{$INCLUDE learn_class.inc}
{$INCLUDE learn_test.inc}
{$ENDREGION 'Include'}


initialization

finalization

end.
