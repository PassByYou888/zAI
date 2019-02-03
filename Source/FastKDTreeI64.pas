{ Fast KDTree Int64 type support                                                 }
{ ****************************************************************************** }
{ * fast KDTree Support,writen by QQ 600585@qq.com                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }

unit FastKDTreeI64;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, PascalStrings, UnicodeMixedLib, KM;

const

  // Int64 KDTree
  KDT1DI64_Axis = 1;
  KDT2DI64_Axis = 2;
  KDT3DI64_Axis = 3;
  KDT4DI64_Axis = 4;
  KDT5DI64_Axis = 5;
  KDT6DI64_Axis = 6;
  KDT7DI64_Axis = 7;
  KDT8DI64_Axis = 8;
  KDT9DI64_Axis = 9;
  KDT10DI64_Axis = 10;
  KDT11DI64_Axis = 11;
  KDT12DI64_Axis = 12;
  KDT13DI64_Axis = 13;
  KDT14DI64_Axis = 14;
  KDT15DI64_Axis = 15;
  KDT16DI64_Axis = 16;
  KDT17DI64_Axis = 17;
  KDT18DI64_Axis = 18;
  KDT19DI64_Axis = 19;
  KDT20DI64_Axis = 20;
  KDT21DI64_Axis = 21;
  KDT22DI64_Axis = 22;
  KDT23DI64_Axis = 23;
  KDT24DI64_Axis = 24;

type

  // Int64: KDTree
  TKDT1DI64 = class;  TKDT1DI64_VecType = KM.TKMFloat; // 1D
  TKDT2DI64 = class;  TKDT2DI64_VecType = KM.TKMFloat; // 2D
  TKDT3DI64 = class;  TKDT3DI64_VecType = KM.TKMFloat; // 3D
  TKDT4DI64 = class;  TKDT4DI64_VecType = KM.TKMFloat; // 4D
  TKDT5DI64 = class;  TKDT5DI64_VecType = KM.TKMFloat; // 5D
  TKDT6DI64 = class;  TKDT6DI64_VecType = KM.TKMFloat; // 6D
  TKDT7DI64 = class;  TKDT7DI64_VecType = KM.TKMFloat; // 7D
  TKDT8DI64 = class;  TKDT8DI64_VecType = KM.TKMFloat; // 8D
  TKDT9DI64 = class;  TKDT9DI64_VecType = KM.TKMFloat; // 9D
  TKDT10DI64 = class;  TKDT10DI64_VecType = KM.TKMFloat; // 10D
  TKDT11DI64 = class;  TKDT11DI64_VecType = KM.TKMFloat; // 11D
  TKDT12DI64 = class;  TKDT12DI64_VecType = KM.TKMFloat; // 12D
  TKDT13DI64 = class;  TKDT13DI64_VecType = KM.TKMFloat; // 13D
  TKDT14DI64 = class;  TKDT14DI64_VecType = KM.TKMFloat; // 14D
  TKDT15DI64 = class;  TKDT15DI64_VecType = KM.TKMFloat; // 15D
  TKDT16DI64 = class;  TKDT16DI64_VecType = KM.TKMFloat; // 16D
  TKDT17DI64 = class;  TKDT17DI64_VecType = KM.TKMFloat; // 17D
  TKDT18DI64 = class;  TKDT18DI64_VecType = KM.TKMFloat; // 18D
  TKDT19DI64 = class;  TKDT19DI64_VecType = KM.TKMFloat; // 19D
  TKDT20DI64 = class;  TKDT20DI64_VecType = KM.TKMFloat; // 20D
  TKDT21DI64 = class;  TKDT21DI64_VecType = KM.TKMFloat; // 21D
  TKDT22DI64 = class;  TKDT22DI64_VecType = KM.TKMFloat; // 22D
  TKDT23DI64 = class;  TKDT23DI64_VecType = KM.TKMFloat; // 23D
  TKDT24DI64 = class;  TKDT24DI64_VecType = KM.TKMFloat; // 24D










  // Int64 KDTree


  TKDT1DI64 = class(TCoreClassObject)
  public type
    // code split
    TKDT1DI64_Vec = array [0 .. KDT1DI64_Axis - 1] of TKDT1DI64_VecType;
    PKDT1DI64_Vec = ^TKDT1DI64_Vec;

    TKDT1DI64_DynamicVecBuffer = array of TKDT1DI64_Vec;
    PKDT1DI64_DynamicVecBuffer = ^TKDT1DI64_DynamicVecBuffer;

    TKDT1DI64_Source = record
      buff: TKDT1DI64_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT1DI64_Source = ^TKDT1DI64_Source;
    TKDT1DI64_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT1DI64_Source) - 1] of PKDT1DI64_Source;
    PKDT1DI64_SourceBuffer = ^TKDT1DI64_SourceBuffer;

    TKDT1DI64_DyanmicSourceBuffer = array of PKDT1DI64_Source;
    PKDT1DI64_DyanmicSourceBuffer = ^TKDT1DI64_DyanmicSourceBuffer;

    TKDT1DI64_DyanmicStoreBuffer = array of TKDT1DI64_Source;
    PKDT1DI64_DyanmicStoreBuffer = ^TKDT1DI64_DyanmicStoreBuffer;

    PKDT1DI64_Node = ^TKDT1DI64_Node;

    TKDT1DI64_Node = record
      Parent, Right, Left: PKDT1DI64_Node;
      vec: PKDT1DI64_Source;
    end;

    TKDT1DI64_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT1DI64_Source; const Data: Pointer);
    TKDT1DI64_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT1DI64_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT1DI64_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT1DI64_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT1DI64_DyanmicStoreBuffer;
    KDBuff: TKDT1DI64_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT1DI64_Node;
    TestBuff: TKDT1DI64_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT1DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT1DI64_Node;
    function GetData(const Index: NativeInt): PKDT1DI64_Source;
  public
    RootNode: PKDT1DI64_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT1DI64_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT1DI64_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DI64_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DI64_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DI64_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT1DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT1DI64_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DI64_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DI64_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DI64_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT1DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT1DI64_Node; overload;
    function Search(const buff: TKDT1DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT1DI64_Node; overload;
    function Search(const buff: TKDT1DI64_Vec; var SearchedDistanceMin: Double): PKDT1DI64_Node; overload;
    function Search(const buff: TKDT1DI64_Vec): PKDT1DI64_Node; overload;
    function SearchToken(const buff: TKDT1DI64_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT1DI64_DynamicVecBuffer; var OutBuff: TKDT1DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT1DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT1DI64_Node);
    procedure PrintBuffer;

    class function KDT1DI64Vec(const s: SystemString): TKDT1DI64_Vec; overload;
    class function KDT1DI64Vec(const v: TKDT1DI64_Vec): SystemString; overload;
    class function KDT1DI64Pow(const v: TKDT1DI64_VecType): Double;
    class function KDT1DI64Distance(const v1, v2: TKDT1DI64_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT1DI64_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT2DI64 = class(TCoreClassObject)
  public type
    // code split
    TKDT2DI64_Vec = array [0 .. KDT2DI64_Axis - 1] of TKDT2DI64_VecType;
    PKDT2DI64_Vec = ^TKDT2DI64_Vec;

    TKDT2DI64_DynamicVecBuffer = array of TKDT2DI64_Vec;
    PKDT2DI64_DynamicVecBuffer = ^TKDT2DI64_DynamicVecBuffer;

    TKDT2DI64_Source = record
      buff: TKDT2DI64_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT2DI64_Source = ^TKDT2DI64_Source;
    TKDT2DI64_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT2DI64_Source) - 1] of PKDT2DI64_Source;
    PKDT2DI64_SourceBuffer = ^TKDT2DI64_SourceBuffer;

    TKDT2DI64_DyanmicSourceBuffer = array of PKDT2DI64_Source;
    PKDT2DI64_DyanmicSourceBuffer = ^TKDT2DI64_DyanmicSourceBuffer;

    TKDT2DI64_DyanmicStoreBuffer = array of TKDT2DI64_Source;
    PKDT2DI64_DyanmicStoreBuffer = ^TKDT2DI64_DyanmicStoreBuffer;

    PKDT2DI64_Node = ^TKDT2DI64_Node;

    TKDT2DI64_Node = record
      Parent, Right, Left: PKDT2DI64_Node;
      vec: PKDT2DI64_Source;
    end;

    TKDT2DI64_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT2DI64_Source; const Data: Pointer);
    TKDT2DI64_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT2DI64_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT2DI64_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT2DI64_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT2DI64_DyanmicStoreBuffer;
    KDBuff: TKDT2DI64_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT2DI64_Node;
    TestBuff: TKDT2DI64_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT2DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT2DI64_Node;
    function GetData(const Index: NativeInt): PKDT2DI64_Source;
  public
    RootNode: PKDT2DI64_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT2DI64_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT2DI64_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DI64_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DI64_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DI64_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT2DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT2DI64_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DI64_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DI64_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DI64_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT2DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT2DI64_Node; overload;
    function Search(const buff: TKDT2DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT2DI64_Node; overload;
    function Search(const buff: TKDT2DI64_Vec; var SearchedDistanceMin: Double): PKDT2DI64_Node; overload;
    function Search(const buff: TKDT2DI64_Vec): PKDT2DI64_Node; overload;
    function SearchToken(const buff: TKDT2DI64_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT2DI64_DynamicVecBuffer; var OutBuff: TKDT2DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT2DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT2DI64_Node);
    procedure PrintBuffer;

    class function KDT2DI64Vec(const s: SystemString): TKDT2DI64_Vec; overload;
    class function KDT2DI64Vec(const v: TKDT2DI64_Vec): SystemString; overload;
    class function KDT2DI64Pow(const v: TKDT2DI64_VecType): Double;
    class function KDT2DI64Distance(const v1, v2: TKDT2DI64_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT2DI64_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT3DI64 = class(TCoreClassObject)
  public type
    // code split
    TKDT3DI64_Vec = array [0 .. KDT3DI64_Axis - 1] of TKDT3DI64_VecType;
    PKDT3DI64_Vec = ^TKDT3DI64_Vec;

    TKDT3DI64_DynamicVecBuffer = array of TKDT3DI64_Vec;
    PKDT3DI64_DynamicVecBuffer = ^TKDT3DI64_DynamicVecBuffer;

    TKDT3DI64_Source = record
      buff: TKDT3DI64_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT3DI64_Source = ^TKDT3DI64_Source;
    TKDT3DI64_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT3DI64_Source) - 1] of PKDT3DI64_Source;
    PKDT3DI64_SourceBuffer = ^TKDT3DI64_SourceBuffer;

    TKDT3DI64_DyanmicSourceBuffer = array of PKDT3DI64_Source;
    PKDT3DI64_DyanmicSourceBuffer = ^TKDT3DI64_DyanmicSourceBuffer;

    TKDT3DI64_DyanmicStoreBuffer = array of TKDT3DI64_Source;
    PKDT3DI64_DyanmicStoreBuffer = ^TKDT3DI64_DyanmicStoreBuffer;

    PKDT3DI64_Node = ^TKDT3DI64_Node;

    TKDT3DI64_Node = record
      Parent, Right, Left: PKDT3DI64_Node;
      vec: PKDT3DI64_Source;
    end;

    TKDT3DI64_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT3DI64_Source; const Data: Pointer);
    TKDT3DI64_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT3DI64_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT3DI64_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT3DI64_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT3DI64_DyanmicStoreBuffer;
    KDBuff: TKDT3DI64_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT3DI64_Node;
    TestBuff: TKDT3DI64_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT3DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT3DI64_Node;
    function GetData(const Index: NativeInt): PKDT3DI64_Source;
  public
    RootNode: PKDT3DI64_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT3DI64_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT3DI64_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DI64_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DI64_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DI64_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT3DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT3DI64_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DI64_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DI64_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DI64_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT3DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT3DI64_Node; overload;
    function Search(const buff: TKDT3DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT3DI64_Node; overload;
    function Search(const buff: TKDT3DI64_Vec; var SearchedDistanceMin: Double): PKDT3DI64_Node; overload;
    function Search(const buff: TKDT3DI64_Vec): PKDT3DI64_Node; overload;
    function SearchToken(const buff: TKDT3DI64_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT3DI64_DynamicVecBuffer; var OutBuff: TKDT3DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT3DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT3DI64_Node);
    procedure PrintBuffer;

    class function KDT3DI64Vec(const s: SystemString): TKDT3DI64_Vec; overload;
    class function KDT3DI64Vec(const v: TKDT3DI64_Vec): SystemString; overload;
    class function KDT3DI64Pow(const v: TKDT3DI64_VecType): Double;
    class function KDT3DI64Distance(const v1, v2: TKDT3DI64_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT3DI64_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT4DI64 = class(TCoreClassObject)
  public type
    // code split
    TKDT4DI64_Vec = array [0 .. KDT4DI64_Axis - 1] of TKDT4DI64_VecType;
    PKDT4DI64_Vec = ^TKDT4DI64_Vec;

    TKDT4DI64_DynamicVecBuffer = array of TKDT4DI64_Vec;
    PKDT4DI64_DynamicVecBuffer = ^TKDT4DI64_DynamicVecBuffer;

    TKDT4DI64_Source = record
      buff: TKDT4DI64_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT4DI64_Source = ^TKDT4DI64_Source;
    TKDT4DI64_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT4DI64_Source) - 1] of PKDT4DI64_Source;
    PKDT4DI64_SourceBuffer = ^TKDT4DI64_SourceBuffer;

    TKDT4DI64_DyanmicSourceBuffer = array of PKDT4DI64_Source;
    PKDT4DI64_DyanmicSourceBuffer = ^TKDT4DI64_DyanmicSourceBuffer;

    TKDT4DI64_DyanmicStoreBuffer = array of TKDT4DI64_Source;
    PKDT4DI64_DyanmicStoreBuffer = ^TKDT4DI64_DyanmicStoreBuffer;

    PKDT4DI64_Node = ^TKDT4DI64_Node;

    TKDT4DI64_Node = record
      Parent, Right, Left: PKDT4DI64_Node;
      vec: PKDT4DI64_Source;
    end;

    TKDT4DI64_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT4DI64_Source; const Data: Pointer);
    TKDT4DI64_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT4DI64_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT4DI64_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT4DI64_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT4DI64_DyanmicStoreBuffer;
    KDBuff: TKDT4DI64_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT4DI64_Node;
    TestBuff: TKDT4DI64_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT4DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT4DI64_Node;
    function GetData(const Index: NativeInt): PKDT4DI64_Source;
  public
    RootNode: PKDT4DI64_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT4DI64_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT4DI64_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DI64_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DI64_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DI64_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT4DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT4DI64_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DI64_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DI64_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DI64_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT4DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT4DI64_Node; overload;
    function Search(const buff: TKDT4DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT4DI64_Node; overload;
    function Search(const buff: TKDT4DI64_Vec; var SearchedDistanceMin: Double): PKDT4DI64_Node; overload;
    function Search(const buff: TKDT4DI64_Vec): PKDT4DI64_Node; overload;
    function SearchToken(const buff: TKDT4DI64_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT4DI64_DynamicVecBuffer; var OutBuff: TKDT4DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT4DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT4DI64_Node);
    procedure PrintBuffer;

    class function KDT4DI64Vec(const s: SystemString): TKDT4DI64_Vec; overload;
    class function KDT4DI64Vec(const v: TKDT4DI64_Vec): SystemString; overload;
    class function KDT4DI64Pow(const v: TKDT4DI64_VecType): Double;
    class function KDT4DI64Distance(const v1, v2: TKDT4DI64_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT4DI64_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT5DI64 = class(TCoreClassObject)
  public type
    // code split
    TKDT5DI64_Vec = array [0 .. KDT5DI64_Axis - 1] of TKDT5DI64_VecType;
    PKDT5DI64_Vec = ^TKDT5DI64_Vec;

    TKDT5DI64_DynamicVecBuffer = array of TKDT5DI64_Vec;
    PKDT5DI64_DynamicVecBuffer = ^TKDT5DI64_DynamicVecBuffer;

    TKDT5DI64_Source = record
      buff: TKDT5DI64_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT5DI64_Source = ^TKDT5DI64_Source;
    TKDT5DI64_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT5DI64_Source) - 1] of PKDT5DI64_Source;
    PKDT5DI64_SourceBuffer = ^TKDT5DI64_SourceBuffer;

    TKDT5DI64_DyanmicSourceBuffer = array of PKDT5DI64_Source;
    PKDT5DI64_DyanmicSourceBuffer = ^TKDT5DI64_DyanmicSourceBuffer;

    TKDT5DI64_DyanmicStoreBuffer = array of TKDT5DI64_Source;
    PKDT5DI64_DyanmicStoreBuffer = ^TKDT5DI64_DyanmicStoreBuffer;

    PKDT5DI64_Node = ^TKDT5DI64_Node;

    TKDT5DI64_Node = record
      Parent, Right, Left: PKDT5DI64_Node;
      vec: PKDT5DI64_Source;
    end;

    TKDT5DI64_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT5DI64_Source; const Data: Pointer);
    TKDT5DI64_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT5DI64_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT5DI64_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT5DI64_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT5DI64_DyanmicStoreBuffer;
    KDBuff: TKDT5DI64_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT5DI64_Node;
    TestBuff: TKDT5DI64_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT5DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT5DI64_Node;
    function GetData(const Index: NativeInt): PKDT5DI64_Source;
  public
    RootNode: PKDT5DI64_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT5DI64_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT5DI64_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DI64_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DI64_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DI64_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT5DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT5DI64_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DI64_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DI64_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DI64_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT5DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT5DI64_Node; overload;
    function Search(const buff: TKDT5DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT5DI64_Node; overload;
    function Search(const buff: TKDT5DI64_Vec; var SearchedDistanceMin: Double): PKDT5DI64_Node; overload;
    function Search(const buff: TKDT5DI64_Vec): PKDT5DI64_Node; overload;
    function SearchToken(const buff: TKDT5DI64_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT5DI64_DynamicVecBuffer; var OutBuff: TKDT5DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT5DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT5DI64_Node);
    procedure PrintBuffer;

    class function KDT5DI64Vec(const s: SystemString): TKDT5DI64_Vec; overload;
    class function KDT5DI64Vec(const v: TKDT5DI64_Vec): SystemString; overload;
    class function KDT5DI64Pow(const v: TKDT5DI64_VecType): Double;
    class function KDT5DI64Distance(const v1, v2: TKDT5DI64_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT5DI64_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT6DI64 = class(TCoreClassObject)
  public type
    // code split
    TKDT6DI64_Vec = array [0 .. KDT6DI64_Axis - 1] of TKDT6DI64_VecType;
    PKDT6DI64_Vec = ^TKDT6DI64_Vec;

    TKDT6DI64_DynamicVecBuffer = array of TKDT6DI64_Vec;
    PKDT6DI64_DynamicVecBuffer = ^TKDT6DI64_DynamicVecBuffer;

    TKDT6DI64_Source = record
      buff: TKDT6DI64_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT6DI64_Source = ^TKDT6DI64_Source;
    TKDT6DI64_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT6DI64_Source) - 1] of PKDT6DI64_Source;
    PKDT6DI64_SourceBuffer = ^TKDT6DI64_SourceBuffer;

    TKDT6DI64_DyanmicSourceBuffer = array of PKDT6DI64_Source;
    PKDT6DI64_DyanmicSourceBuffer = ^TKDT6DI64_DyanmicSourceBuffer;

    TKDT6DI64_DyanmicStoreBuffer = array of TKDT6DI64_Source;
    PKDT6DI64_DyanmicStoreBuffer = ^TKDT6DI64_DyanmicStoreBuffer;

    PKDT6DI64_Node = ^TKDT6DI64_Node;

    TKDT6DI64_Node = record
      Parent, Right, Left: PKDT6DI64_Node;
      vec: PKDT6DI64_Source;
    end;

    TKDT6DI64_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT6DI64_Source; const Data: Pointer);
    TKDT6DI64_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT6DI64_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT6DI64_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT6DI64_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT6DI64_DyanmicStoreBuffer;
    KDBuff: TKDT6DI64_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT6DI64_Node;
    TestBuff: TKDT6DI64_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT6DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT6DI64_Node;
    function GetData(const Index: NativeInt): PKDT6DI64_Source;
  public
    RootNode: PKDT6DI64_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT6DI64_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT6DI64_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DI64_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DI64_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DI64_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT6DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT6DI64_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DI64_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DI64_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DI64_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT6DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT6DI64_Node; overload;
    function Search(const buff: TKDT6DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT6DI64_Node; overload;
    function Search(const buff: TKDT6DI64_Vec; var SearchedDistanceMin: Double): PKDT6DI64_Node; overload;
    function Search(const buff: TKDT6DI64_Vec): PKDT6DI64_Node; overload;
    function SearchToken(const buff: TKDT6DI64_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT6DI64_DynamicVecBuffer; var OutBuff: TKDT6DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT6DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT6DI64_Node);
    procedure PrintBuffer;

    class function KDT6DI64Vec(const s: SystemString): TKDT6DI64_Vec; overload;
    class function KDT6DI64Vec(const v: TKDT6DI64_Vec): SystemString; overload;
    class function KDT6DI64Pow(const v: TKDT6DI64_VecType): Double;
    class function KDT6DI64Distance(const v1, v2: TKDT6DI64_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT6DI64_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT7DI64 = class(TCoreClassObject)
  public type
    // code split
    TKDT7DI64_Vec = array [0 .. KDT7DI64_Axis - 1] of TKDT7DI64_VecType;
    PKDT7DI64_Vec = ^TKDT7DI64_Vec;

    TKDT7DI64_DynamicVecBuffer = array of TKDT7DI64_Vec;
    PKDT7DI64_DynamicVecBuffer = ^TKDT7DI64_DynamicVecBuffer;

    TKDT7DI64_Source = record
      buff: TKDT7DI64_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT7DI64_Source = ^TKDT7DI64_Source;
    TKDT7DI64_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT7DI64_Source) - 1] of PKDT7DI64_Source;
    PKDT7DI64_SourceBuffer = ^TKDT7DI64_SourceBuffer;

    TKDT7DI64_DyanmicSourceBuffer = array of PKDT7DI64_Source;
    PKDT7DI64_DyanmicSourceBuffer = ^TKDT7DI64_DyanmicSourceBuffer;

    TKDT7DI64_DyanmicStoreBuffer = array of TKDT7DI64_Source;
    PKDT7DI64_DyanmicStoreBuffer = ^TKDT7DI64_DyanmicStoreBuffer;

    PKDT7DI64_Node = ^TKDT7DI64_Node;

    TKDT7DI64_Node = record
      Parent, Right, Left: PKDT7DI64_Node;
      vec: PKDT7DI64_Source;
    end;

    TKDT7DI64_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT7DI64_Source; const Data: Pointer);
    TKDT7DI64_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT7DI64_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT7DI64_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT7DI64_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT7DI64_DyanmicStoreBuffer;
    KDBuff: TKDT7DI64_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT7DI64_Node;
    TestBuff: TKDT7DI64_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT7DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT7DI64_Node;
    function GetData(const Index: NativeInt): PKDT7DI64_Source;
  public
    RootNode: PKDT7DI64_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT7DI64_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT7DI64_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DI64_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DI64_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DI64_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT7DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT7DI64_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DI64_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DI64_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DI64_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT7DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT7DI64_Node; overload;
    function Search(const buff: TKDT7DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT7DI64_Node; overload;
    function Search(const buff: TKDT7DI64_Vec; var SearchedDistanceMin: Double): PKDT7DI64_Node; overload;
    function Search(const buff: TKDT7DI64_Vec): PKDT7DI64_Node; overload;
    function SearchToken(const buff: TKDT7DI64_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT7DI64_DynamicVecBuffer; var OutBuff: TKDT7DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT7DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT7DI64_Node);
    procedure PrintBuffer;

    class function KDT7DI64Vec(const s: SystemString): TKDT7DI64_Vec; overload;
    class function KDT7DI64Vec(const v: TKDT7DI64_Vec): SystemString; overload;
    class function KDT7DI64Pow(const v: TKDT7DI64_VecType): Double;
    class function KDT7DI64Distance(const v1, v2: TKDT7DI64_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT7DI64_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT8DI64 = class(TCoreClassObject)
  public type
    // code split
    TKDT8DI64_Vec = array [0 .. KDT8DI64_Axis - 1] of TKDT8DI64_VecType;
    PKDT8DI64_Vec = ^TKDT8DI64_Vec;

    TKDT8DI64_DynamicVecBuffer = array of TKDT8DI64_Vec;
    PKDT8DI64_DynamicVecBuffer = ^TKDT8DI64_DynamicVecBuffer;

    TKDT8DI64_Source = record
      buff: TKDT8DI64_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT8DI64_Source = ^TKDT8DI64_Source;
    TKDT8DI64_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT8DI64_Source) - 1] of PKDT8DI64_Source;
    PKDT8DI64_SourceBuffer = ^TKDT8DI64_SourceBuffer;

    TKDT8DI64_DyanmicSourceBuffer = array of PKDT8DI64_Source;
    PKDT8DI64_DyanmicSourceBuffer = ^TKDT8DI64_DyanmicSourceBuffer;

    TKDT8DI64_DyanmicStoreBuffer = array of TKDT8DI64_Source;
    PKDT8DI64_DyanmicStoreBuffer = ^TKDT8DI64_DyanmicStoreBuffer;

    PKDT8DI64_Node = ^TKDT8DI64_Node;

    TKDT8DI64_Node = record
      Parent, Right, Left: PKDT8DI64_Node;
      vec: PKDT8DI64_Source;
    end;

    TKDT8DI64_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT8DI64_Source; const Data: Pointer);
    TKDT8DI64_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT8DI64_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT8DI64_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT8DI64_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT8DI64_DyanmicStoreBuffer;
    KDBuff: TKDT8DI64_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT8DI64_Node;
    TestBuff: TKDT8DI64_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT8DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT8DI64_Node;
    function GetData(const Index: NativeInt): PKDT8DI64_Source;
  public
    RootNode: PKDT8DI64_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT8DI64_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT8DI64_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DI64_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DI64_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DI64_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT8DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT8DI64_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DI64_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DI64_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DI64_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT8DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT8DI64_Node; overload;
    function Search(const buff: TKDT8DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT8DI64_Node; overload;
    function Search(const buff: TKDT8DI64_Vec; var SearchedDistanceMin: Double): PKDT8DI64_Node; overload;
    function Search(const buff: TKDT8DI64_Vec): PKDT8DI64_Node; overload;
    function SearchToken(const buff: TKDT8DI64_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT8DI64_DynamicVecBuffer; var OutBuff: TKDT8DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT8DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT8DI64_Node);
    procedure PrintBuffer;

    class function KDT8DI64Vec(const s: SystemString): TKDT8DI64_Vec; overload;
    class function KDT8DI64Vec(const v: TKDT8DI64_Vec): SystemString; overload;
    class function KDT8DI64Pow(const v: TKDT8DI64_VecType): Double;
    class function KDT8DI64Distance(const v1, v2: TKDT8DI64_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT8DI64_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT9DI64 = class(TCoreClassObject)
  public type
    // code split
    TKDT9DI64_Vec = array [0 .. KDT9DI64_Axis - 1] of TKDT9DI64_VecType;
    PKDT9DI64_Vec = ^TKDT9DI64_Vec;

    TKDT9DI64_DynamicVecBuffer = array of TKDT9DI64_Vec;
    PKDT9DI64_DynamicVecBuffer = ^TKDT9DI64_DynamicVecBuffer;

    TKDT9DI64_Source = record
      buff: TKDT9DI64_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT9DI64_Source = ^TKDT9DI64_Source;
    TKDT9DI64_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT9DI64_Source) - 1] of PKDT9DI64_Source;
    PKDT9DI64_SourceBuffer = ^TKDT9DI64_SourceBuffer;

    TKDT9DI64_DyanmicSourceBuffer = array of PKDT9DI64_Source;
    PKDT9DI64_DyanmicSourceBuffer = ^TKDT9DI64_DyanmicSourceBuffer;

    TKDT9DI64_DyanmicStoreBuffer = array of TKDT9DI64_Source;
    PKDT9DI64_DyanmicStoreBuffer = ^TKDT9DI64_DyanmicStoreBuffer;

    PKDT9DI64_Node = ^TKDT9DI64_Node;

    TKDT9DI64_Node = record
      Parent, Right, Left: PKDT9DI64_Node;
      vec: PKDT9DI64_Source;
    end;

    TKDT9DI64_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT9DI64_Source; const Data: Pointer);
    TKDT9DI64_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT9DI64_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT9DI64_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT9DI64_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT9DI64_DyanmicStoreBuffer;
    KDBuff: TKDT9DI64_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT9DI64_Node;
    TestBuff: TKDT9DI64_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT9DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT9DI64_Node;
    function GetData(const Index: NativeInt): PKDT9DI64_Source;
  public
    RootNode: PKDT9DI64_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT9DI64_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT9DI64_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DI64_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DI64_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DI64_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT9DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT9DI64_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DI64_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DI64_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DI64_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT9DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT9DI64_Node; overload;
    function Search(const buff: TKDT9DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT9DI64_Node; overload;
    function Search(const buff: TKDT9DI64_Vec; var SearchedDistanceMin: Double): PKDT9DI64_Node; overload;
    function Search(const buff: TKDT9DI64_Vec): PKDT9DI64_Node; overload;
    function SearchToken(const buff: TKDT9DI64_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT9DI64_DynamicVecBuffer; var OutBuff: TKDT9DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT9DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT9DI64_Node);
    procedure PrintBuffer;

    class function KDT9DI64Vec(const s: SystemString): TKDT9DI64_Vec; overload;
    class function KDT9DI64Vec(const v: TKDT9DI64_Vec): SystemString; overload;
    class function KDT9DI64Pow(const v: TKDT9DI64_VecType): Double;
    class function KDT9DI64Distance(const v1, v2: TKDT9DI64_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT9DI64_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT10DI64 = class(TCoreClassObject)
  public type
    // code split
    TKDT10DI64_Vec = array [0 .. KDT10DI64_Axis - 1] of TKDT10DI64_VecType;
    PKDT10DI64_Vec = ^TKDT10DI64_Vec;

    TKDT10DI64_DynamicVecBuffer = array of TKDT10DI64_Vec;
    PKDT10DI64_DynamicVecBuffer = ^TKDT10DI64_DynamicVecBuffer;

    TKDT10DI64_Source = record
      buff: TKDT10DI64_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT10DI64_Source = ^TKDT10DI64_Source;
    TKDT10DI64_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT10DI64_Source) - 1] of PKDT10DI64_Source;
    PKDT10DI64_SourceBuffer = ^TKDT10DI64_SourceBuffer;

    TKDT10DI64_DyanmicSourceBuffer = array of PKDT10DI64_Source;
    PKDT10DI64_DyanmicSourceBuffer = ^TKDT10DI64_DyanmicSourceBuffer;

    TKDT10DI64_DyanmicStoreBuffer = array of TKDT10DI64_Source;
    PKDT10DI64_DyanmicStoreBuffer = ^TKDT10DI64_DyanmicStoreBuffer;

    PKDT10DI64_Node = ^TKDT10DI64_Node;

    TKDT10DI64_Node = record
      Parent, Right, Left: PKDT10DI64_Node;
      vec: PKDT10DI64_Source;
    end;

    TKDT10DI64_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT10DI64_Source; const Data: Pointer);
    TKDT10DI64_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT10DI64_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT10DI64_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT10DI64_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT10DI64_DyanmicStoreBuffer;
    KDBuff: TKDT10DI64_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT10DI64_Node;
    TestBuff: TKDT10DI64_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT10DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT10DI64_Node;
    function GetData(const Index: NativeInt): PKDT10DI64_Source;
  public
    RootNode: PKDT10DI64_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT10DI64_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT10DI64_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DI64_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DI64_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DI64_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT10DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT10DI64_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DI64_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DI64_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DI64_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT10DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT10DI64_Node; overload;
    function Search(const buff: TKDT10DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT10DI64_Node; overload;
    function Search(const buff: TKDT10DI64_Vec; var SearchedDistanceMin: Double): PKDT10DI64_Node; overload;
    function Search(const buff: TKDT10DI64_Vec): PKDT10DI64_Node; overload;
    function SearchToken(const buff: TKDT10DI64_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT10DI64_DynamicVecBuffer; var OutBuff: TKDT10DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT10DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT10DI64_Node);
    procedure PrintBuffer;

    class function KDT10DI64Vec(const s: SystemString): TKDT10DI64_Vec; overload;
    class function KDT10DI64Vec(const v: TKDT10DI64_Vec): SystemString; overload;
    class function KDT10DI64Pow(const v: TKDT10DI64_VecType): Double;
    class function KDT10DI64Distance(const v1, v2: TKDT10DI64_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT10DI64_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT11DI64 = class(TCoreClassObject)
  public type
    // code split
    TKDT11DI64_Vec = array [0 .. KDT11DI64_Axis - 1] of TKDT11DI64_VecType;
    PKDT11DI64_Vec = ^TKDT11DI64_Vec;

    TKDT11DI64_DynamicVecBuffer = array of TKDT11DI64_Vec;
    PKDT11DI64_DynamicVecBuffer = ^TKDT11DI64_DynamicVecBuffer;

    TKDT11DI64_Source = record
      buff: TKDT11DI64_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT11DI64_Source = ^TKDT11DI64_Source;
    TKDT11DI64_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT11DI64_Source) - 1] of PKDT11DI64_Source;
    PKDT11DI64_SourceBuffer = ^TKDT11DI64_SourceBuffer;

    TKDT11DI64_DyanmicSourceBuffer = array of PKDT11DI64_Source;
    PKDT11DI64_DyanmicSourceBuffer = ^TKDT11DI64_DyanmicSourceBuffer;

    TKDT11DI64_DyanmicStoreBuffer = array of TKDT11DI64_Source;
    PKDT11DI64_DyanmicStoreBuffer = ^TKDT11DI64_DyanmicStoreBuffer;

    PKDT11DI64_Node = ^TKDT11DI64_Node;

    TKDT11DI64_Node = record
      Parent, Right, Left: PKDT11DI64_Node;
      vec: PKDT11DI64_Source;
    end;

    TKDT11DI64_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT11DI64_Source; const Data: Pointer);
    TKDT11DI64_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT11DI64_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT11DI64_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT11DI64_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT11DI64_DyanmicStoreBuffer;
    KDBuff: TKDT11DI64_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT11DI64_Node;
    TestBuff: TKDT11DI64_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT11DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT11DI64_Node;
    function GetData(const Index: NativeInt): PKDT11DI64_Source;
  public
    RootNode: PKDT11DI64_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT11DI64_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT11DI64_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DI64_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DI64_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DI64_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT11DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT11DI64_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DI64_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DI64_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DI64_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT11DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT11DI64_Node; overload;
    function Search(const buff: TKDT11DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT11DI64_Node; overload;
    function Search(const buff: TKDT11DI64_Vec; var SearchedDistanceMin: Double): PKDT11DI64_Node; overload;
    function Search(const buff: TKDT11DI64_Vec): PKDT11DI64_Node; overload;
    function SearchToken(const buff: TKDT11DI64_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT11DI64_DynamicVecBuffer; var OutBuff: TKDT11DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT11DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT11DI64_Node);
    procedure PrintBuffer;

    class function KDT11DI64Vec(const s: SystemString): TKDT11DI64_Vec; overload;
    class function KDT11DI64Vec(const v: TKDT11DI64_Vec): SystemString; overload;
    class function KDT11DI64Pow(const v: TKDT11DI64_VecType): Double;
    class function KDT11DI64Distance(const v1, v2: TKDT11DI64_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT11DI64_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT12DI64 = class(TCoreClassObject)
  public type
    // code split
    TKDT12DI64_Vec = array [0 .. KDT12DI64_Axis - 1] of TKDT12DI64_VecType;
    PKDT12DI64_Vec = ^TKDT12DI64_Vec;

    TKDT12DI64_DynamicVecBuffer = array of TKDT12DI64_Vec;
    PKDT12DI64_DynamicVecBuffer = ^TKDT12DI64_DynamicVecBuffer;

    TKDT12DI64_Source = record
      buff: TKDT12DI64_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT12DI64_Source = ^TKDT12DI64_Source;
    TKDT12DI64_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT12DI64_Source) - 1] of PKDT12DI64_Source;
    PKDT12DI64_SourceBuffer = ^TKDT12DI64_SourceBuffer;

    TKDT12DI64_DyanmicSourceBuffer = array of PKDT12DI64_Source;
    PKDT12DI64_DyanmicSourceBuffer = ^TKDT12DI64_DyanmicSourceBuffer;

    TKDT12DI64_DyanmicStoreBuffer = array of TKDT12DI64_Source;
    PKDT12DI64_DyanmicStoreBuffer = ^TKDT12DI64_DyanmicStoreBuffer;

    PKDT12DI64_Node = ^TKDT12DI64_Node;

    TKDT12DI64_Node = record
      Parent, Right, Left: PKDT12DI64_Node;
      vec: PKDT12DI64_Source;
    end;

    TKDT12DI64_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT12DI64_Source; const Data: Pointer);
    TKDT12DI64_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT12DI64_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT12DI64_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT12DI64_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT12DI64_DyanmicStoreBuffer;
    KDBuff: TKDT12DI64_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT12DI64_Node;
    TestBuff: TKDT12DI64_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT12DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT12DI64_Node;
    function GetData(const Index: NativeInt): PKDT12DI64_Source;
  public
    RootNode: PKDT12DI64_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT12DI64_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT12DI64_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DI64_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DI64_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DI64_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT12DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT12DI64_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DI64_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DI64_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DI64_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT12DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT12DI64_Node; overload;
    function Search(const buff: TKDT12DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT12DI64_Node; overload;
    function Search(const buff: TKDT12DI64_Vec; var SearchedDistanceMin: Double): PKDT12DI64_Node; overload;
    function Search(const buff: TKDT12DI64_Vec): PKDT12DI64_Node; overload;
    function SearchToken(const buff: TKDT12DI64_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT12DI64_DynamicVecBuffer; var OutBuff: TKDT12DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT12DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT12DI64_Node);
    procedure PrintBuffer;

    class function KDT12DI64Vec(const s: SystemString): TKDT12DI64_Vec; overload;
    class function KDT12DI64Vec(const v: TKDT12DI64_Vec): SystemString; overload;
    class function KDT12DI64Pow(const v: TKDT12DI64_VecType): Double;
    class function KDT12DI64Distance(const v1, v2: TKDT12DI64_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT12DI64_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT13DI64 = class(TCoreClassObject)
  public type
    // code split
    TKDT13DI64_Vec = array [0 .. KDT13DI64_Axis - 1] of TKDT13DI64_VecType;
    PKDT13DI64_Vec = ^TKDT13DI64_Vec;

    TKDT13DI64_DynamicVecBuffer = array of TKDT13DI64_Vec;
    PKDT13DI64_DynamicVecBuffer = ^TKDT13DI64_DynamicVecBuffer;

    TKDT13DI64_Source = record
      buff: TKDT13DI64_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT13DI64_Source = ^TKDT13DI64_Source;
    TKDT13DI64_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT13DI64_Source) - 1] of PKDT13DI64_Source;
    PKDT13DI64_SourceBuffer = ^TKDT13DI64_SourceBuffer;

    TKDT13DI64_DyanmicSourceBuffer = array of PKDT13DI64_Source;
    PKDT13DI64_DyanmicSourceBuffer = ^TKDT13DI64_DyanmicSourceBuffer;

    TKDT13DI64_DyanmicStoreBuffer = array of TKDT13DI64_Source;
    PKDT13DI64_DyanmicStoreBuffer = ^TKDT13DI64_DyanmicStoreBuffer;

    PKDT13DI64_Node = ^TKDT13DI64_Node;

    TKDT13DI64_Node = record
      Parent, Right, Left: PKDT13DI64_Node;
      vec: PKDT13DI64_Source;
    end;

    TKDT13DI64_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT13DI64_Source; const Data: Pointer);
    TKDT13DI64_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT13DI64_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT13DI64_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT13DI64_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT13DI64_DyanmicStoreBuffer;
    KDBuff: TKDT13DI64_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT13DI64_Node;
    TestBuff: TKDT13DI64_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT13DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT13DI64_Node;
    function GetData(const Index: NativeInt): PKDT13DI64_Source;
  public
    RootNode: PKDT13DI64_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT13DI64_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT13DI64_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DI64_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DI64_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DI64_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT13DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT13DI64_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DI64_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DI64_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DI64_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT13DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT13DI64_Node; overload;
    function Search(const buff: TKDT13DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT13DI64_Node; overload;
    function Search(const buff: TKDT13DI64_Vec; var SearchedDistanceMin: Double): PKDT13DI64_Node; overload;
    function Search(const buff: TKDT13DI64_Vec): PKDT13DI64_Node; overload;
    function SearchToken(const buff: TKDT13DI64_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT13DI64_DynamicVecBuffer; var OutBuff: TKDT13DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT13DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT13DI64_Node);
    procedure PrintBuffer;

    class function KDT13DI64Vec(const s: SystemString): TKDT13DI64_Vec; overload;
    class function KDT13DI64Vec(const v: TKDT13DI64_Vec): SystemString; overload;
    class function KDT13DI64Pow(const v: TKDT13DI64_VecType): Double;
    class function KDT13DI64Distance(const v1, v2: TKDT13DI64_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT13DI64_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT14DI64 = class(TCoreClassObject)
  public type
    // code split
    TKDT14DI64_Vec = array [0 .. KDT14DI64_Axis - 1] of TKDT14DI64_VecType;
    PKDT14DI64_Vec = ^TKDT14DI64_Vec;

    TKDT14DI64_DynamicVecBuffer = array of TKDT14DI64_Vec;
    PKDT14DI64_DynamicVecBuffer = ^TKDT14DI64_DynamicVecBuffer;

    TKDT14DI64_Source = record
      buff: TKDT14DI64_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT14DI64_Source = ^TKDT14DI64_Source;
    TKDT14DI64_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT14DI64_Source) - 1] of PKDT14DI64_Source;
    PKDT14DI64_SourceBuffer = ^TKDT14DI64_SourceBuffer;

    TKDT14DI64_DyanmicSourceBuffer = array of PKDT14DI64_Source;
    PKDT14DI64_DyanmicSourceBuffer = ^TKDT14DI64_DyanmicSourceBuffer;

    TKDT14DI64_DyanmicStoreBuffer = array of TKDT14DI64_Source;
    PKDT14DI64_DyanmicStoreBuffer = ^TKDT14DI64_DyanmicStoreBuffer;

    PKDT14DI64_Node = ^TKDT14DI64_Node;

    TKDT14DI64_Node = record
      Parent, Right, Left: PKDT14DI64_Node;
      vec: PKDT14DI64_Source;
    end;

    TKDT14DI64_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT14DI64_Source; const Data: Pointer);
    TKDT14DI64_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT14DI64_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT14DI64_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT14DI64_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT14DI64_DyanmicStoreBuffer;
    KDBuff: TKDT14DI64_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT14DI64_Node;
    TestBuff: TKDT14DI64_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT14DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT14DI64_Node;
    function GetData(const Index: NativeInt): PKDT14DI64_Source;
  public
    RootNode: PKDT14DI64_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT14DI64_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT14DI64_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DI64_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DI64_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DI64_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT14DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT14DI64_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DI64_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DI64_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DI64_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT14DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT14DI64_Node; overload;
    function Search(const buff: TKDT14DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT14DI64_Node; overload;
    function Search(const buff: TKDT14DI64_Vec; var SearchedDistanceMin: Double): PKDT14DI64_Node; overload;
    function Search(const buff: TKDT14DI64_Vec): PKDT14DI64_Node; overload;
    function SearchToken(const buff: TKDT14DI64_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT14DI64_DynamicVecBuffer; var OutBuff: TKDT14DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT14DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT14DI64_Node);
    procedure PrintBuffer;

    class function KDT14DI64Vec(const s: SystemString): TKDT14DI64_Vec; overload;
    class function KDT14DI64Vec(const v: TKDT14DI64_Vec): SystemString; overload;
    class function KDT14DI64Pow(const v: TKDT14DI64_VecType): Double;
    class function KDT14DI64Distance(const v1, v2: TKDT14DI64_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT14DI64_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT15DI64 = class(TCoreClassObject)
  public type
    // code split
    TKDT15DI64_Vec = array [0 .. KDT15DI64_Axis - 1] of TKDT15DI64_VecType;
    PKDT15DI64_Vec = ^TKDT15DI64_Vec;

    TKDT15DI64_DynamicVecBuffer = array of TKDT15DI64_Vec;
    PKDT15DI64_DynamicVecBuffer = ^TKDT15DI64_DynamicVecBuffer;

    TKDT15DI64_Source = record
      buff: TKDT15DI64_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT15DI64_Source = ^TKDT15DI64_Source;
    TKDT15DI64_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT15DI64_Source) - 1] of PKDT15DI64_Source;
    PKDT15DI64_SourceBuffer = ^TKDT15DI64_SourceBuffer;

    TKDT15DI64_DyanmicSourceBuffer = array of PKDT15DI64_Source;
    PKDT15DI64_DyanmicSourceBuffer = ^TKDT15DI64_DyanmicSourceBuffer;

    TKDT15DI64_DyanmicStoreBuffer = array of TKDT15DI64_Source;
    PKDT15DI64_DyanmicStoreBuffer = ^TKDT15DI64_DyanmicStoreBuffer;

    PKDT15DI64_Node = ^TKDT15DI64_Node;

    TKDT15DI64_Node = record
      Parent, Right, Left: PKDT15DI64_Node;
      vec: PKDT15DI64_Source;
    end;

    TKDT15DI64_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT15DI64_Source; const Data: Pointer);
    TKDT15DI64_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT15DI64_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT15DI64_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT15DI64_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT15DI64_DyanmicStoreBuffer;
    KDBuff: TKDT15DI64_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT15DI64_Node;
    TestBuff: TKDT15DI64_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT15DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT15DI64_Node;
    function GetData(const Index: NativeInt): PKDT15DI64_Source;
  public
    RootNode: PKDT15DI64_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT15DI64_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT15DI64_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DI64_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DI64_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DI64_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT15DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT15DI64_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DI64_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DI64_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DI64_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT15DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT15DI64_Node; overload;
    function Search(const buff: TKDT15DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT15DI64_Node; overload;
    function Search(const buff: TKDT15DI64_Vec; var SearchedDistanceMin: Double): PKDT15DI64_Node; overload;
    function Search(const buff: TKDT15DI64_Vec): PKDT15DI64_Node; overload;
    function SearchToken(const buff: TKDT15DI64_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT15DI64_DynamicVecBuffer; var OutBuff: TKDT15DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT15DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT15DI64_Node);
    procedure PrintBuffer;

    class function KDT15DI64Vec(const s: SystemString): TKDT15DI64_Vec; overload;
    class function KDT15DI64Vec(const v: TKDT15DI64_Vec): SystemString; overload;
    class function KDT15DI64Pow(const v: TKDT15DI64_VecType): Double;
    class function KDT15DI64Distance(const v1, v2: TKDT15DI64_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT15DI64_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT16DI64 = class(TCoreClassObject)
  public type
    // code split
    TKDT16DI64_Vec = array [0 .. KDT16DI64_Axis - 1] of TKDT16DI64_VecType;
    PKDT16DI64_Vec = ^TKDT16DI64_Vec;

    TKDT16DI64_DynamicVecBuffer = array of TKDT16DI64_Vec;
    PKDT16DI64_DynamicVecBuffer = ^TKDT16DI64_DynamicVecBuffer;

    TKDT16DI64_Source = record
      buff: TKDT16DI64_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT16DI64_Source = ^TKDT16DI64_Source;
    TKDT16DI64_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT16DI64_Source) - 1] of PKDT16DI64_Source;
    PKDT16DI64_SourceBuffer = ^TKDT16DI64_SourceBuffer;

    TKDT16DI64_DyanmicSourceBuffer = array of PKDT16DI64_Source;
    PKDT16DI64_DyanmicSourceBuffer = ^TKDT16DI64_DyanmicSourceBuffer;

    TKDT16DI64_DyanmicStoreBuffer = array of TKDT16DI64_Source;
    PKDT16DI64_DyanmicStoreBuffer = ^TKDT16DI64_DyanmicStoreBuffer;

    PKDT16DI64_Node = ^TKDT16DI64_Node;

    TKDT16DI64_Node = record
      Parent, Right, Left: PKDT16DI64_Node;
      vec: PKDT16DI64_Source;
    end;

    TKDT16DI64_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT16DI64_Source; const Data: Pointer);
    TKDT16DI64_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT16DI64_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT16DI64_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT16DI64_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT16DI64_DyanmicStoreBuffer;
    KDBuff: TKDT16DI64_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT16DI64_Node;
    TestBuff: TKDT16DI64_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT16DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT16DI64_Node;
    function GetData(const Index: NativeInt): PKDT16DI64_Source;
  public
    RootNode: PKDT16DI64_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT16DI64_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT16DI64_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DI64_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DI64_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DI64_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT16DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT16DI64_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DI64_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DI64_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DI64_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT16DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT16DI64_Node; overload;
    function Search(const buff: TKDT16DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT16DI64_Node; overload;
    function Search(const buff: TKDT16DI64_Vec; var SearchedDistanceMin: Double): PKDT16DI64_Node; overload;
    function Search(const buff: TKDT16DI64_Vec): PKDT16DI64_Node; overload;
    function SearchToken(const buff: TKDT16DI64_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT16DI64_DynamicVecBuffer; var OutBuff: TKDT16DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT16DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT16DI64_Node);
    procedure PrintBuffer;

    class function KDT16DI64Vec(const s: SystemString): TKDT16DI64_Vec; overload;
    class function KDT16DI64Vec(const v: TKDT16DI64_Vec): SystemString; overload;
    class function KDT16DI64Pow(const v: TKDT16DI64_VecType): Double;
    class function KDT16DI64Distance(const v1, v2: TKDT16DI64_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT16DI64_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT17DI64 = class(TCoreClassObject)
  public type
    // code split
    TKDT17DI64_Vec = array [0 .. KDT17DI64_Axis - 1] of TKDT17DI64_VecType;
    PKDT17DI64_Vec = ^TKDT17DI64_Vec;

    TKDT17DI64_DynamicVecBuffer = array of TKDT17DI64_Vec;
    PKDT17DI64_DynamicVecBuffer = ^TKDT17DI64_DynamicVecBuffer;

    TKDT17DI64_Source = record
      buff: TKDT17DI64_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT17DI64_Source = ^TKDT17DI64_Source;
    TKDT17DI64_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT17DI64_Source) - 1] of PKDT17DI64_Source;
    PKDT17DI64_SourceBuffer = ^TKDT17DI64_SourceBuffer;

    TKDT17DI64_DyanmicSourceBuffer = array of PKDT17DI64_Source;
    PKDT17DI64_DyanmicSourceBuffer = ^TKDT17DI64_DyanmicSourceBuffer;

    TKDT17DI64_DyanmicStoreBuffer = array of TKDT17DI64_Source;
    PKDT17DI64_DyanmicStoreBuffer = ^TKDT17DI64_DyanmicStoreBuffer;

    PKDT17DI64_Node = ^TKDT17DI64_Node;

    TKDT17DI64_Node = record
      Parent, Right, Left: PKDT17DI64_Node;
      vec: PKDT17DI64_Source;
    end;

    TKDT17DI64_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT17DI64_Source; const Data: Pointer);
    TKDT17DI64_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT17DI64_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT17DI64_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT17DI64_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT17DI64_DyanmicStoreBuffer;
    KDBuff: TKDT17DI64_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT17DI64_Node;
    TestBuff: TKDT17DI64_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT17DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT17DI64_Node;
    function GetData(const Index: NativeInt): PKDT17DI64_Source;
  public
    RootNode: PKDT17DI64_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT17DI64_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT17DI64_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DI64_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DI64_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DI64_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT17DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT17DI64_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DI64_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DI64_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DI64_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT17DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT17DI64_Node; overload;
    function Search(const buff: TKDT17DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT17DI64_Node; overload;
    function Search(const buff: TKDT17DI64_Vec; var SearchedDistanceMin: Double): PKDT17DI64_Node; overload;
    function Search(const buff: TKDT17DI64_Vec): PKDT17DI64_Node; overload;
    function SearchToken(const buff: TKDT17DI64_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT17DI64_DynamicVecBuffer; var OutBuff: TKDT17DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT17DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT17DI64_Node);
    procedure PrintBuffer;

    class function KDT17DI64Vec(const s: SystemString): TKDT17DI64_Vec; overload;
    class function KDT17DI64Vec(const v: TKDT17DI64_Vec): SystemString; overload;
    class function KDT17DI64Pow(const v: TKDT17DI64_VecType): Double;
    class function KDT17DI64Distance(const v1, v2: TKDT17DI64_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT17DI64_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT18DI64 = class(TCoreClassObject)
  public type
    // code split
    TKDT18DI64_Vec = array [0 .. KDT18DI64_Axis - 1] of TKDT18DI64_VecType;
    PKDT18DI64_Vec = ^TKDT18DI64_Vec;

    TKDT18DI64_DynamicVecBuffer = array of TKDT18DI64_Vec;
    PKDT18DI64_DynamicVecBuffer = ^TKDT18DI64_DynamicVecBuffer;

    TKDT18DI64_Source = record
      buff: TKDT18DI64_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT18DI64_Source = ^TKDT18DI64_Source;
    TKDT18DI64_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT18DI64_Source) - 1] of PKDT18DI64_Source;
    PKDT18DI64_SourceBuffer = ^TKDT18DI64_SourceBuffer;

    TKDT18DI64_DyanmicSourceBuffer = array of PKDT18DI64_Source;
    PKDT18DI64_DyanmicSourceBuffer = ^TKDT18DI64_DyanmicSourceBuffer;

    TKDT18DI64_DyanmicStoreBuffer = array of TKDT18DI64_Source;
    PKDT18DI64_DyanmicStoreBuffer = ^TKDT18DI64_DyanmicStoreBuffer;

    PKDT18DI64_Node = ^TKDT18DI64_Node;

    TKDT18DI64_Node = record
      Parent, Right, Left: PKDT18DI64_Node;
      vec: PKDT18DI64_Source;
    end;

    TKDT18DI64_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT18DI64_Source; const Data: Pointer);
    TKDT18DI64_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT18DI64_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT18DI64_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT18DI64_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT18DI64_DyanmicStoreBuffer;
    KDBuff: TKDT18DI64_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT18DI64_Node;
    TestBuff: TKDT18DI64_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT18DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT18DI64_Node;
    function GetData(const Index: NativeInt): PKDT18DI64_Source;
  public
    RootNode: PKDT18DI64_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT18DI64_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT18DI64_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DI64_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DI64_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DI64_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT18DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT18DI64_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DI64_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DI64_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DI64_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT18DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT18DI64_Node; overload;
    function Search(const buff: TKDT18DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT18DI64_Node; overload;
    function Search(const buff: TKDT18DI64_Vec; var SearchedDistanceMin: Double): PKDT18DI64_Node; overload;
    function Search(const buff: TKDT18DI64_Vec): PKDT18DI64_Node; overload;
    function SearchToken(const buff: TKDT18DI64_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT18DI64_DynamicVecBuffer; var OutBuff: TKDT18DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT18DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT18DI64_Node);
    procedure PrintBuffer;

    class function KDT18DI64Vec(const s: SystemString): TKDT18DI64_Vec; overload;
    class function KDT18DI64Vec(const v: TKDT18DI64_Vec): SystemString; overload;
    class function KDT18DI64Pow(const v: TKDT18DI64_VecType): Double;
    class function KDT18DI64Distance(const v1, v2: TKDT18DI64_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT18DI64_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT19DI64 = class(TCoreClassObject)
  public type
    // code split
    TKDT19DI64_Vec = array [0 .. KDT19DI64_Axis - 1] of TKDT19DI64_VecType;
    PKDT19DI64_Vec = ^TKDT19DI64_Vec;

    TKDT19DI64_DynamicVecBuffer = array of TKDT19DI64_Vec;
    PKDT19DI64_DynamicVecBuffer = ^TKDT19DI64_DynamicVecBuffer;

    TKDT19DI64_Source = record
      buff: TKDT19DI64_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT19DI64_Source = ^TKDT19DI64_Source;
    TKDT19DI64_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT19DI64_Source) - 1] of PKDT19DI64_Source;
    PKDT19DI64_SourceBuffer = ^TKDT19DI64_SourceBuffer;

    TKDT19DI64_DyanmicSourceBuffer = array of PKDT19DI64_Source;
    PKDT19DI64_DyanmicSourceBuffer = ^TKDT19DI64_DyanmicSourceBuffer;

    TKDT19DI64_DyanmicStoreBuffer = array of TKDT19DI64_Source;
    PKDT19DI64_DyanmicStoreBuffer = ^TKDT19DI64_DyanmicStoreBuffer;

    PKDT19DI64_Node = ^TKDT19DI64_Node;

    TKDT19DI64_Node = record
      Parent, Right, Left: PKDT19DI64_Node;
      vec: PKDT19DI64_Source;
    end;

    TKDT19DI64_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT19DI64_Source; const Data: Pointer);
    TKDT19DI64_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT19DI64_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT19DI64_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT19DI64_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT19DI64_DyanmicStoreBuffer;
    KDBuff: TKDT19DI64_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT19DI64_Node;
    TestBuff: TKDT19DI64_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT19DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT19DI64_Node;
    function GetData(const Index: NativeInt): PKDT19DI64_Source;
  public
    RootNode: PKDT19DI64_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT19DI64_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT19DI64_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DI64_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DI64_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DI64_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT19DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT19DI64_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DI64_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DI64_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DI64_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT19DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT19DI64_Node; overload;
    function Search(const buff: TKDT19DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT19DI64_Node; overload;
    function Search(const buff: TKDT19DI64_Vec; var SearchedDistanceMin: Double): PKDT19DI64_Node; overload;
    function Search(const buff: TKDT19DI64_Vec): PKDT19DI64_Node; overload;
    function SearchToken(const buff: TKDT19DI64_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT19DI64_DynamicVecBuffer; var OutBuff: TKDT19DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT19DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT19DI64_Node);
    procedure PrintBuffer;

    class function KDT19DI64Vec(const s: SystemString): TKDT19DI64_Vec; overload;
    class function KDT19DI64Vec(const v: TKDT19DI64_Vec): SystemString; overload;
    class function KDT19DI64Pow(const v: TKDT19DI64_VecType): Double;
    class function KDT19DI64Distance(const v1, v2: TKDT19DI64_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT19DI64_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT20DI64 = class(TCoreClassObject)
  public type
    // code split
    TKDT20DI64_Vec = array [0 .. KDT20DI64_Axis - 1] of TKDT20DI64_VecType;
    PKDT20DI64_Vec = ^TKDT20DI64_Vec;

    TKDT20DI64_DynamicVecBuffer = array of TKDT20DI64_Vec;
    PKDT20DI64_DynamicVecBuffer = ^TKDT20DI64_DynamicVecBuffer;

    TKDT20DI64_Source = record
      buff: TKDT20DI64_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT20DI64_Source = ^TKDT20DI64_Source;
    TKDT20DI64_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT20DI64_Source) - 1] of PKDT20DI64_Source;
    PKDT20DI64_SourceBuffer = ^TKDT20DI64_SourceBuffer;

    TKDT20DI64_DyanmicSourceBuffer = array of PKDT20DI64_Source;
    PKDT20DI64_DyanmicSourceBuffer = ^TKDT20DI64_DyanmicSourceBuffer;

    TKDT20DI64_DyanmicStoreBuffer = array of TKDT20DI64_Source;
    PKDT20DI64_DyanmicStoreBuffer = ^TKDT20DI64_DyanmicStoreBuffer;

    PKDT20DI64_Node = ^TKDT20DI64_Node;

    TKDT20DI64_Node = record
      Parent, Right, Left: PKDT20DI64_Node;
      vec: PKDT20DI64_Source;
    end;

    TKDT20DI64_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT20DI64_Source; const Data: Pointer);
    TKDT20DI64_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT20DI64_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT20DI64_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT20DI64_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT20DI64_DyanmicStoreBuffer;
    KDBuff: TKDT20DI64_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT20DI64_Node;
    TestBuff: TKDT20DI64_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT20DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT20DI64_Node;
    function GetData(const Index: NativeInt): PKDT20DI64_Source;
  public
    RootNode: PKDT20DI64_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT20DI64_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT20DI64_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DI64_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DI64_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DI64_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT20DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT20DI64_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DI64_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DI64_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DI64_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT20DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT20DI64_Node; overload;
    function Search(const buff: TKDT20DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT20DI64_Node; overload;
    function Search(const buff: TKDT20DI64_Vec; var SearchedDistanceMin: Double): PKDT20DI64_Node; overload;
    function Search(const buff: TKDT20DI64_Vec): PKDT20DI64_Node; overload;
    function SearchToken(const buff: TKDT20DI64_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT20DI64_DynamicVecBuffer; var OutBuff: TKDT20DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT20DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT20DI64_Node);
    procedure PrintBuffer;

    class function KDT20DI64Vec(const s: SystemString): TKDT20DI64_Vec; overload;
    class function KDT20DI64Vec(const v: TKDT20DI64_Vec): SystemString; overload;
    class function KDT20DI64Pow(const v: TKDT20DI64_VecType): Double;
    class function KDT20DI64Distance(const v1, v2: TKDT20DI64_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT20DI64_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT21DI64 = class(TCoreClassObject)
  public type
    // code split
    TKDT21DI64_Vec = array [0 .. KDT21DI64_Axis - 1] of TKDT21DI64_VecType;
    PKDT21DI64_Vec = ^TKDT21DI64_Vec;

    TKDT21DI64_DynamicVecBuffer = array of TKDT21DI64_Vec;
    PKDT21DI64_DynamicVecBuffer = ^TKDT21DI64_DynamicVecBuffer;

    TKDT21DI64_Source = record
      buff: TKDT21DI64_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT21DI64_Source = ^TKDT21DI64_Source;
    TKDT21DI64_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT21DI64_Source) - 1] of PKDT21DI64_Source;
    PKDT21DI64_SourceBuffer = ^TKDT21DI64_SourceBuffer;

    TKDT21DI64_DyanmicSourceBuffer = array of PKDT21DI64_Source;
    PKDT21DI64_DyanmicSourceBuffer = ^TKDT21DI64_DyanmicSourceBuffer;

    TKDT21DI64_DyanmicStoreBuffer = array of TKDT21DI64_Source;
    PKDT21DI64_DyanmicStoreBuffer = ^TKDT21DI64_DyanmicStoreBuffer;

    PKDT21DI64_Node = ^TKDT21DI64_Node;

    TKDT21DI64_Node = record
      Parent, Right, Left: PKDT21DI64_Node;
      vec: PKDT21DI64_Source;
    end;

    TKDT21DI64_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT21DI64_Source; const Data: Pointer);
    TKDT21DI64_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT21DI64_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT21DI64_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT21DI64_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT21DI64_DyanmicStoreBuffer;
    KDBuff: TKDT21DI64_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT21DI64_Node;
    TestBuff: TKDT21DI64_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT21DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT21DI64_Node;
    function GetData(const Index: NativeInt): PKDT21DI64_Source;
  public
    RootNode: PKDT21DI64_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT21DI64_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT21DI64_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DI64_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DI64_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DI64_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT21DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT21DI64_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DI64_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DI64_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DI64_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT21DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT21DI64_Node; overload;
    function Search(const buff: TKDT21DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT21DI64_Node; overload;
    function Search(const buff: TKDT21DI64_Vec; var SearchedDistanceMin: Double): PKDT21DI64_Node; overload;
    function Search(const buff: TKDT21DI64_Vec): PKDT21DI64_Node; overload;
    function SearchToken(const buff: TKDT21DI64_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT21DI64_DynamicVecBuffer; var OutBuff: TKDT21DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT21DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT21DI64_Node);
    procedure PrintBuffer;

    class function KDT21DI64Vec(const s: SystemString): TKDT21DI64_Vec; overload;
    class function KDT21DI64Vec(const v: TKDT21DI64_Vec): SystemString; overload;
    class function KDT21DI64Pow(const v: TKDT21DI64_VecType): Double;
    class function KDT21DI64Distance(const v1, v2: TKDT21DI64_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT21DI64_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT22DI64 = class(TCoreClassObject)
  public type
    // code split
    TKDT22DI64_Vec = array [0 .. KDT22DI64_Axis - 1] of TKDT22DI64_VecType;
    PKDT22DI64_Vec = ^TKDT22DI64_Vec;

    TKDT22DI64_DynamicVecBuffer = array of TKDT22DI64_Vec;
    PKDT22DI64_DynamicVecBuffer = ^TKDT22DI64_DynamicVecBuffer;

    TKDT22DI64_Source = record
      buff: TKDT22DI64_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT22DI64_Source = ^TKDT22DI64_Source;
    TKDT22DI64_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT22DI64_Source) - 1] of PKDT22DI64_Source;
    PKDT22DI64_SourceBuffer = ^TKDT22DI64_SourceBuffer;

    TKDT22DI64_DyanmicSourceBuffer = array of PKDT22DI64_Source;
    PKDT22DI64_DyanmicSourceBuffer = ^TKDT22DI64_DyanmicSourceBuffer;

    TKDT22DI64_DyanmicStoreBuffer = array of TKDT22DI64_Source;
    PKDT22DI64_DyanmicStoreBuffer = ^TKDT22DI64_DyanmicStoreBuffer;

    PKDT22DI64_Node = ^TKDT22DI64_Node;

    TKDT22DI64_Node = record
      Parent, Right, Left: PKDT22DI64_Node;
      vec: PKDT22DI64_Source;
    end;

    TKDT22DI64_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT22DI64_Source; const Data: Pointer);
    TKDT22DI64_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT22DI64_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT22DI64_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT22DI64_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT22DI64_DyanmicStoreBuffer;
    KDBuff: TKDT22DI64_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT22DI64_Node;
    TestBuff: TKDT22DI64_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT22DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT22DI64_Node;
    function GetData(const Index: NativeInt): PKDT22DI64_Source;
  public
    RootNode: PKDT22DI64_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT22DI64_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT22DI64_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DI64_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DI64_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DI64_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT22DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT22DI64_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DI64_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DI64_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DI64_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT22DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT22DI64_Node; overload;
    function Search(const buff: TKDT22DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT22DI64_Node; overload;
    function Search(const buff: TKDT22DI64_Vec; var SearchedDistanceMin: Double): PKDT22DI64_Node; overload;
    function Search(const buff: TKDT22DI64_Vec): PKDT22DI64_Node; overload;
    function SearchToken(const buff: TKDT22DI64_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT22DI64_DynamicVecBuffer; var OutBuff: TKDT22DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT22DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT22DI64_Node);
    procedure PrintBuffer;

    class function KDT22DI64Vec(const s: SystemString): TKDT22DI64_Vec; overload;
    class function KDT22DI64Vec(const v: TKDT22DI64_Vec): SystemString; overload;
    class function KDT22DI64Pow(const v: TKDT22DI64_VecType): Double;
    class function KDT22DI64Distance(const v1, v2: TKDT22DI64_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT22DI64_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT23DI64 = class(TCoreClassObject)
  public type
    // code split
    TKDT23DI64_Vec = array [0 .. KDT23DI64_Axis - 1] of TKDT23DI64_VecType;
    PKDT23DI64_Vec = ^TKDT23DI64_Vec;

    TKDT23DI64_DynamicVecBuffer = array of TKDT23DI64_Vec;
    PKDT23DI64_DynamicVecBuffer = ^TKDT23DI64_DynamicVecBuffer;

    TKDT23DI64_Source = record
      buff: TKDT23DI64_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT23DI64_Source = ^TKDT23DI64_Source;
    TKDT23DI64_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT23DI64_Source) - 1] of PKDT23DI64_Source;
    PKDT23DI64_SourceBuffer = ^TKDT23DI64_SourceBuffer;

    TKDT23DI64_DyanmicSourceBuffer = array of PKDT23DI64_Source;
    PKDT23DI64_DyanmicSourceBuffer = ^TKDT23DI64_DyanmicSourceBuffer;

    TKDT23DI64_DyanmicStoreBuffer = array of TKDT23DI64_Source;
    PKDT23DI64_DyanmicStoreBuffer = ^TKDT23DI64_DyanmicStoreBuffer;

    PKDT23DI64_Node = ^TKDT23DI64_Node;

    TKDT23DI64_Node = record
      Parent, Right, Left: PKDT23DI64_Node;
      vec: PKDT23DI64_Source;
    end;

    TKDT23DI64_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT23DI64_Source; const Data: Pointer);
    TKDT23DI64_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT23DI64_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT23DI64_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT23DI64_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT23DI64_DyanmicStoreBuffer;
    KDBuff: TKDT23DI64_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT23DI64_Node;
    TestBuff: TKDT23DI64_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT23DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT23DI64_Node;
    function GetData(const Index: NativeInt): PKDT23DI64_Source;
  public
    RootNode: PKDT23DI64_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT23DI64_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT23DI64_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DI64_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DI64_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DI64_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT23DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT23DI64_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DI64_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DI64_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DI64_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT23DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT23DI64_Node; overload;
    function Search(const buff: TKDT23DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT23DI64_Node; overload;
    function Search(const buff: TKDT23DI64_Vec; var SearchedDistanceMin: Double): PKDT23DI64_Node; overload;
    function Search(const buff: TKDT23DI64_Vec): PKDT23DI64_Node; overload;
    function SearchToken(const buff: TKDT23DI64_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT23DI64_DynamicVecBuffer; var OutBuff: TKDT23DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT23DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT23DI64_Node);
    procedure PrintBuffer;

    class function KDT23DI64Vec(const s: SystemString): TKDT23DI64_Vec; overload;
    class function KDT23DI64Vec(const v: TKDT23DI64_Vec): SystemString; overload;
    class function KDT23DI64Pow(const v: TKDT23DI64_VecType): Double;
    class function KDT23DI64Distance(const v1, v2: TKDT23DI64_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT23DI64_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT24DI64 = class(TCoreClassObject)
  public type
    // code split
    TKDT24DI64_Vec = array [0 .. KDT24DI64_Axis - 1] of TKDT24DI64_VecType;
    PKDT24DI64_Vec = ^TKDT24DI64_Vec;

    TKDT24DI64_DynamicVecBuffer = array of TKDT24DI64_Vec;
    PKDT24DI64_DynamicVecBuffer = ^TKDT24DI64_DynamicVecBuffer;

    TKDT24DI64_Source = record
      buff: TKDT24DI64_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT24DI64_Source = ^TKDT24DI64_Source;
    TKDT24DI64_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT24DI64_Source) - 1] of PKDT24DI64_Source;
    PKDT24DI64_SourceBuffer = ^TKDT24DI64_SourceBuffer;

    TKDT24DI64_DyanmicSourceBuffer = array of PKDT24DI64_Source;
    PKDT24DI64_DyanmicSourceBuffer = ^TKDT24DI64_DyanmicSourceBuffer;

    TKDT24DI64_DyanmicStoreBuffer = array of TKDT24DI64_Source;
    PKDT24DI64_DyanmicStoreBuffer = ^TKDT24DI64_DyanmicStoreBuffer;

    PKDT24DI64_Node = ^TKDT24DI64_Node;

    TKDT24DI64_Node = record
      Parent, Right, Left: PKDT24DI64_Node;
      vec: PKDT24DI64_Source;
    end;

    TKDT24DI64_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT24DI64_Source; const Data: Pointer);
    TKDT24DI64_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT24DI64_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT24DI64_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT24DI64_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT24DI64_DyanmicStoreBuffer;
    KDBuff: TKDT24DI64_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT24DI64_Node;
    TestBuff: TKDT24DI64_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT24DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT24DI64_Node;
    function GetData(const Index: NativeInt): PKDT24DI64_Source;
  public
    RootNode: PKDT24DI64_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT24DI64_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT24DI64_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DI64_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DI64_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DI64_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT24DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT24DI64_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DI64_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DI64_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DI64_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT24DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT24DI64_Node; overload;
    function Search(const buff: TKDT24DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT24DI64_Node; overload;
    function Search(const buff: TKDT24DI64_Vec; var SearchedDistanceMin: Double): PKDT24DI64_Node; overload;
    function Search(const buff: TKDT24DI64_Vec): PKDT24DI64_Node; overload;
    function SearchToken(const buff: TKDT24DI64_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT24DI64_DynamicVecBuffer; var OutBuff: TKDT24DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT24DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT24DI64_Node);
    procedure PrintBuffer;

    class function KDT24DI64Vec(const s: SystemString): TKDT24DI64_Vec; overload;
    class function KDT24DI64Vec(const v: TKDT24DI64_Vec): SystemString; overload;
    class function KDT24DI64Pow(const v: TKDT24DI64_VecType): Double;
    class function KDT24DI64Distance(const v1, v2: TKDT24DI64_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT24DI64_Source; const Data: Pointer);
    class procedure Test;
  end;






procedure Test_All;



implementation

uses
  {$IFDEF FPC}
  mtprocs,
  {$ELSE FPC}
  Threading,
  {$ENDIF FPC}
  TextParsing, MemoryStream64, DoStatusIO;





const
  SaveToken = $66;



function TKDT1DI64.InternalBuildKdTree(const KDSourceBufferPtr: PKDT1DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT1DI64_Node;
  function SortCompare(const p1, p2: PKDT1DI64_Source; const axis: NativeInt): ShortInt;
  begin
    if p1^.buff[axis] = p2^.buff[axis] then
      begin
        if p1^.Index = p2^.Index then
            Result := 0
        else if p1^.Index < p2^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.buff[axis] < p2^.buff[axis] then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(const SortBuffer: PKDT1DI64_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT1DI64_Source;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer^[(L + R) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, axis) < 0 do
            Inc(i);
        while SortCompare(SortBuffer^[j], p, axis) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, axis);
      L := i;
    until i >= R;
  end;

var
  M: NativeInt;
  axis: NativeInt;
  kdBuffPtr: PKDT1DI64_SourceBuffer;
begin
  Result := nil;
  if PlanCount = 0 then
      Exit;

  if PlanCount = 1 then
    begin
      new(Result);
      Result^.Parent := nil;
      Result^.Right := nil;
      Result^.Left := nil;
      Result^.vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);
    end
  else
    begin
      axis := Depth mod KDT1DI64_Axis;
      M := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr^[0], 0, PlanCount - 1, axis);

      new(Result);
      Result^.Parent := nil;
      Result^.vec := kdBuffPtr^[M];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], M, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[M + 1], PlanCount - (M + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDT1DI64.GetData(const Index: NativeInt): PKDT1DI64_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT1DI64.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT1DI64.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT1DI64.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT1DI64_Node(KDNodes[i]));
      Inc(i);
    end;

  for i := 0 to length(KDStoreBuff) - 1 do
      KDStoreBuff[i].Token := '';

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDT1DI64.StoreBuffPtr: PKDT1DI64_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT1DI64.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DI64_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT1DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT1DI64.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DI64_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT1DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT1DI64.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DI64_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT1DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT1DI64.BuildKDTreeWithCluster(const inBuff: TKDT1DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT1DI64_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT1DI64_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT1DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT1DI64.BuildKDTreeWithCluster(const inBuff: TKDT1DI64_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT1DI64.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DI64_BuildCall);
var
  TempStoreBuff: TKDT1DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT1DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT1DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT1DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT1DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT1DI64.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DI64_BuildMethod);
var
  TempStoreBuff: TKDT1DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT1DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT1DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT1DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT1DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$IFNDEF FPC}


procedure TKDT1DI64.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DI64_BuildProc);
var
  TempStoreBuff: TKDT1DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT1DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT1DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT1DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT1DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$ENDIF FPC}


function TKDT1DI64.Search(const buff: TKDT1DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT1DI64_Node;

var
  NearestNeighbour: PKDT1DI64_Node;

  function FindParentNode(const buffPtr: PKDT1DI64_Vec; NodePtr: PKDT1DI64_Node): PKDT1DI64_Node;
  var
    Next: PKDT1DI64_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT1DI64_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT1DI64_Node; const buffPtr: PKDT1DI64_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT1DI64Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT1DI64_Axis;
    Dist := NodePtr^.vec^.buff[axis] - buffPtr^[axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.vec^.buff[axis] > buffPtr^[axis] then
            ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes)
        else
            ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end
    else
      begin
        ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes);
        ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end;
  end;

  function SortCompare(const buffPtr: PKDT1DI64_Vec; const p1, p2: PKDT1DI64_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT1DI64Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT1DI64Distance(buffPtr^, p2^.vec^.buff);
    if d1 = d2 then
      begin
        if p1^.vec^.Index = p2^.vec^.Index then
            Result := 0
        else if p1^.vec^.Index < p2^.vec^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT1DI64_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT1DI64_Node;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer[(L + R) shr 1];
      repeat
        while SortCompare(buffPtr, SortBuffer[i], p) < 0 do
            Inc(i);
        while SortCompare(buffPtr, SortBuffer[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, buffPtr);
      L := i;
    until i >= R;
  end;

var
  Parent: PKDT1DI64_Node;
begin
  Result := nil;
  SearchedDistanceMin := 0;
  SearchedCounter := 0;
  NearestNeighbour := nil;
  if NearestNodes <> nil then
      NearestNodes.Clear;
  if RootNode = nil then
      Exit;
  if Count = 0 then
      Exit;

  Parent := FindParentNode(@buff[0], RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := KDT1DI64Distance(buff, Parent^.vec^.buff);

  ScanSubtree(RootNode, @buff[0], 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @buff[0]);

      if NearestNodes.Count > 0 then
          Result := PKDT1DI64_Node(NearestNodes[0]);
    end;
end;

function TKDT1DI64.Search(const buff: TKDT1DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT1DI64_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT1DI64.Search(const buff: TKDT1DI64_Vec; var SearchedDistanceMin: Double): PKDT1DI64_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT1DI64.Search(const buff: TKDT1DI64_Vec): PKDT1DI64_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT1DI64.SearchToken(const buff: TKDT1DI64_Vec): TPascalString;
var
  p: PKDT1DI64_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT1DI64.Search(const inBuff: TKDT1DI64_DynamicVecBuffer; var OutBuff: TKDT1DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT1DI64_DynamicVecBuffer;
  outBuffPtr: PKDT1DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT1DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outBuffPtr^[pass] := p^.vec^.buff;
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outBuffPtr := @OutBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT1DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outBuffPtr^[pass] := p^.vec^.buff;
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT1DI64_Node;
begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutBuff[i] := p^.vec^.buff;
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT1DI64.Search(const inBuff: TKDT1DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT1DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT1DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT1DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT1DI64_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT1DI64.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT1DI64_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT1DI64_Vec));
      stream.write(KDStoreBuff[i].Index, 8);
      token_B := KDStoreBuff[i].Token.Bytes;
      token_L := length(token_B);
      stream.write(token_L, 4);
      if token_L > 0 then
        begin
          stream.write(token_B[0], token_L);
          SetLength(token_B, 0);
        end;
      Inc(i);
    end;
end;

procedure TKDT1DI64.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  Clear;

  stream.read(st, 4);
  stream.read(ID, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if ID <> KDT1DI64_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT1DI64_Vec)) <> SizeOf(TKDT1DI64_Vec) then
          begin
            Clear;
            Exit;
          end;
        if stream.read(KDStoreBuff[i].Index, 8) <> 8 then
          begin
            Clear;
            Exit;
          end;
        if stream.read(token_L, 4) <> 4 then
          begin
            Clear;
            Exit;
          end;
        if token_L > 0 then
          begin
            SetLength(token_B, token_L);
            if stream.read(token_B[0], token_L) <> token_L then
              begin
                Clear;
                Exit;
              end;
            KDStoreBuff[i].Token.Bytes := token_B;
            SetLength(token_B, 0);
          end
        else
            KDStoreBuff[i].Token := '';
        Inc(i);
      end;
  except
    Clear;
    Exit;
  end;

  SetLength(KDBuff, cnt);
  SetLength(KDNodes, cnt);

  i := 0;
  while i < cnt do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      Inc(i);
    end;

  if cnt > 0 then
    RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDT1DI64.SaveToFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT1DI64.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT1DI64.PrintNodeTree(const NodePtr: PKDT1DI64_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT1DI64_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT1DI64Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT1DI64.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT1DI64Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT1DI64.KDT1DI64Vec(const s: SystemString): TKDT1DI64_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT1DI64_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToInt(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT1DI64_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT1DI64.KDT1DI64Vec(const v: TKDT1DI64_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT1DI64_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT1DI64.KDT1DI64Pow(const v: TKDT1DI64_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT1DI64.KDT1DI64Distance(const v1, v2: TKDT1DI64_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT1DI64_Axis - 1 do
      Result := Result + KDT1DI64Pow(v2[i] - v1[i]);
end;

procedure TKDT1DI64.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT1DI64_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT1DI64.Test;
var
  TKDT1DI64_Test: TKDT1DI64;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT1DI64_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT1DI64_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT1DI64_Test := TKDT1DI64.Create;

  DoStatusNoLn('...');
  SetLength(TKDT1DI64_Test.TestBuff, 1000);
  for i := 0 to length(TKDT1DI64_Test.TestBuff) - 1 do
    for j := 0 to KDT1DI64_Axis - 1 do
        TKDT1DI64_Test.TestBuff[i][j] := i * KDT1DI64_Axis + j;

{$IFDEF FPC}
  TKDT1DI64_Test.BuildKDTreeM(length(TKDT1DI64_Test.TestBuff), nil, @TKDT1DI64_Test.Test_BuildM);
{$ELSE FPC}
  TKDT1DI64_Test.BuildKDTreeM(length(TKDT1DI64_Test.TestBuff), nil, TKDT1DI64_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT1DI64_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT1DI64_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT1DI64_Test.TestBuff) - 1 do
    begin
      p := TKDT1DI64_Test.Search(TKDT1DI64_Test.TestBuff[i]);
      if p^.vec^.Index <> i then
          errored := True;
      if not p^.vec^.Token.Same(umlIntToStr(i)) then
          errored := True;
      if errored then
          Break;
    end;
  DisposeObject(m64);

  if not errored then
    begin
      { parallel search test }
      DoStatusNoLn('...parallel');
      SetLength(TestResultBuff, length(TKDT1DI64_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT1DI64_Test.TestBuff));
      TKDT1DI64_Test.Search(TKDT1DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT1DI64Distance(TKDT1DI64_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT1DI64_Test.Clear;
      { kMean test }
      TKDT1DI64_Test.BuildKDTreeWithCluster(TKDT1DI64_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT1DI64_Test.Search(TKDT1DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT1DI64_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT1DI64_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT1DI64_Test);
end;


function TKDT2DI64.InternalBuildKdTree(const KDSourceBufferPtr: PKDT2DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT2DI64_Node;
  function SortCompare(const p1, p2: PKDT2DI64_Source; const axis: NativeInt): ShortInt;
  begin
    if p1^.buff[axis] = p2^.buff[axis] then
      begin
        if p1^.Index = p2^.Index then
            Result := 0
        else if p1^.Index < p2^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.buff[axis] < p2^.buff[axis] then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(const SortBuffer: PKDT2DI64_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT2DI64_Source;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer^[(L + R) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, axis) < 0 do
            Inc(i);
        while SortCompare(SortBuffer^[j], p, axis) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, axis);
      L := i;
    until i >= R;
  end;

var
  M: NativeInt;
  axis: NativeInt;
  kdBuffPtr: PKDT2DI64_SourceBuffer;
begin
  Result := nil;
  if PlanCount = 0 then
      Exit;

  if PlanCount = 1 then
    begin
      new(Result);
      Result^.Parent := nil;
      Result^.Right := nil;
      Result^.Left := nil;
      Result^.vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);
    end
  else
    begin
      axis := Depth mod KDT2DI64_Axis;
      M := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr^[0], 0, PlanCount - 1, axis);

      new(Result);
      Result^.Parent := nil;
      Result^.vec := kdBuffPtr^[M];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], M, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[M + 1], PlanCount - (M + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDT2DI64.GetData(const Index: NativeInt): PKDT2DI64_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT2DI64.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT2DI64.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT2DI64.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT2DI64_Node(KDNodes[i]));
      Inc(i);
    end;

  for i := 0 to length(KDStoreBuff) - 1 do
      KDStoreBuff[i].Token := '';

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDT2DI64.StoreBuffPtr: PKDT2DI64_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT2DI64.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DI64_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT2DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT2DI64.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DI64_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT2DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT2DI64.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DI64_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT2DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT2DI64.BuildKDTreeWithCluster(const inBuff: TKDT2DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT2DI64_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT2DI64_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT2DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT2DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT2DI64.BuildKDTreeWithCluster(const inBuff: TKDT2DI64_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT2DI64.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DI64_BuildCall);
var
  TempStoreBuff: TKDT2DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT2DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT2DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT2DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT2DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT2DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT2DI64.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DI64_BuildMethod);
var
  TempStoreBuff: TKDT2DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT2DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT2DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT2DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT2DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT2DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$IFNDEF FPC}


procedure TKDT2DI64.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DI64_BuildProc);
var
  TempStoreBuff: TKDT2DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT2DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT2DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT2DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT2DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT2DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$ENDIF FPC}


function TKDT2DI64.Search(const buff: TKDT2DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT2DI64_Node;

var
  NearestNeighbour: PKDT2DI64_Node;

  function FindParentNode(const buffPtr: PKDT2DI64_Vec; NodePtr: PKDT2DI64_Node): PKDT2DI64_Node;
  var
    Next: PKDT2DI64_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT2DI64_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT2DI64_Node; const buffPtr: PKDT2DI64_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT2DI64Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT2DI64_Axis;
    Dist := NodePtr^.vec^.buff[axis] - buffPtr^[axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.vec^.buff[axis] > buffPtr^[axis] then
            ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes)
        else
            ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end
    else
      begin
        ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes);
        ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end;
  end;

  function SortCompare(const buffPtr: PKDT2DI64_Vec; const p1, p2: PKDT2DI64_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT2DI64Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT2DI64Distance(buffPtr^, p2^.vec^.buff);
    if d1 = d2 then
      begin
        if p1^.vec^.Index = p2^.vec^.Index then
            Result := 0
        else if p1^.vec^.Index < p2^.vec^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT2DI64_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT2DI64_Node;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer[(L + R) shr 1];
      repeat
        while SortCompare(buffPtr, SortBuffer[i], p) < 0 do
            Inc(i);
        while SortCompare(buffPtr, SortBuffer[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, buffPtr);
      L := i;
    until i >= R;
  end;

var
  Parent: PKDT2DI64_Node;
begin
  Result := nil;
  SearchedDistanceMin := 0;
  SearchedCounter := 0;
  NearestNeighbour := nil;
  if NearestNodes <> nil then
      NearestNodes.Clear;
  if RootNode = nil then
      Exit;
  if Count = 0 then
      Exit;

  Parent := FindParentNode(@buff[0], RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := KDT2DI64Distance(buff, Parent^.vec^.buff);

  ScanSubtree(RootNode, @buff[0], 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @buff[0]);

      if NearestNodes.Count > 0 then
          Result := PKDT2DI64_Node(NearestNodes[0]);
    end;
end;

function TKDT2DI64.Search(const buff: TKDT2DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT2DI64_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT2DI64.Search(const buff: TKDT2DI64_Vec; var SearchedDistanceMin: Double): PKDT2DI64_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT2DI64.Search(const buff: TKDT2DI64_Vec): PKDT2DI64_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT2DI64.SearchToken(const buff: TKDT2DI64_Vec): TPascalString;
var
  p: PKDT2DI64_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT2DI64.Search(const inBuff: TKDT2DI64_DynamicVecBuffer; var OutBuff: TKDT2DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT2DI64_DynamicVecBuffer;
  outBuffPtr: PKDT2DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT2DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outBuffPtr^[pass] := p^.vec^.buff;
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outBuffPtr := @OutBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT2DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outBuffPtr^[pass] := p^.vec^.buff;
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT2DI64_Node;
begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutBuff[i] := p^.vec^.buff;
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT2DI64.Search(const inBuff: TKDT2DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT2DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT2DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT2DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT2DI64_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT2DI64.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT2DI64_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT2DI64_Vec));
      stream.write(KDStoreBuff[i].Index, 8);
      token_B := KDStoreBuff[i].Token.Bytes;
      token_L := length(token_B);
      stream.write(token_L, 4);
      if token_L > 0 then
        begin
          stream.write(token_B[0], token_L);
          SetLength(token_B, 0);
        end;
      Inc(i);
    end;
end;

procedure TKDT2DI64.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  Clear;

  stream.read(st, 4);
  stream.read(ID, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if ID <> KDT2DI64_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT2DI64_Vec)) <> SizeOf(TKDT2DI64_Vec) then
          begin
            Clear;
            Exit;
          end;
        if stream.read(KDStoreBuff[i].Index, 8) <> 8 then
          begin
            Clear;
            Exit;
          end;
        if stream.read(token_L, 4) <> 4 then
          begin
            Clear;
            Exit;
          end;
        if token_L > 0 then
          begin
            SetLength(token_B, token_L);
            if stream.read(token_B[0], token_L) <> token_L then
              begin
                Clear;
                Exit;
              end;
            KDStoreBuff[i].Token.Bytes := token_B;
            SetLength(token_B, 0);
          end
        else
            KDStoreBuff[i].Token := '';
        Inc(i);
      end;
  except
    Clear;
    Exit;
  end;

  SetLength(KDBuff, cnt);
  SetLength(KDNodes, cnt);

  i := 0;
  while i < cnt do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      Inc(i);
    end;

  if cnt > 0 then
    RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDT2DI64.SaveToFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT2DI64.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT2DI64.PrintNodeTree(const NodePtr: PKDT2DI64_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT2DI64_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT2DI64Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT2DI64.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT2DI64Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT2DI64.KDT2DI64Vec(const s: SystemString): TKDT2DI64_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT2DI64_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToInt(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT2DI64_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT2DI64.KDT2DI64Vec(const v: TKDT2DI64_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT2DI64_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT2DI64.KDT2DI64Pow(const v: TKDT2DI64_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT2DI64.KDT2DI64Distance(const v1, v2: TKDT2DI64_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT2DI64_Axis - 1 do
      Result := Result + KDT2DI64Pow(v2[i] - v1[i]);
end;

procedure TKDT2DI64.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT2DI64_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT2DI64.Test;
var
  TKDT2DI64_Test: TKDT2DI64;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT2DI64_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT2DI64_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT2DI64_Test := TKDT2DI64.Create;

  DoStatusNoLn('...');
  SetLength(TKDT2DI64_Test.TestBuff, 1000);
  for i := 0 to length(TKDT2DI64_Test.TestBuff) - 1 do
    for j := 0 to KDT2DI64_Axis - 1 do
        TKDT2DI64_Test.TestBuff[i][j] := i * KDT2DI64_Axis + j;

{$IFDEF FPC}
  TKDT2DI64_Test.BuildKDTreeM(length(TKDT2DI64_Test.TestBuff), nil, @TKDT2DI64_Test.Test_BuildM);
{$ELSE FPC}
  TKDT2DI64_Test.BuildKDTreeM(length(TKDT2DI64_Test.TestBuff), nil, TKDT2DI64_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT2DI64_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT2DI64_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT2DI64_Test.TestBuff) - 1 do
    begin
      p := TKDT2DI64_Test.Search(TKDT2DI64_Test.TestBuff[i]);
      if p^.vec^.Index <> i then
          errored := True;
      if not p^.vec^.Token.Same(umlIntToStr(i)) then
          errored := True;
      if errored then
          Break;
    end;
  DisposeObject(m64);

  if not errored then
    begin
      { parallel search test }
      DoStatusNoLn('...parallel');
      SetLength(TestResultBuff, length(TKDT2DI64_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT2DI64_Test.TestBuff));
      TKDT2DI64_Test.Search(TKDT2DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT2DI64Distance(TKDT2DI64_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT2DI64_Test.Clear;
      { kMean test }
      TKDT2DI64_Test.BuildKDTreeWithCluster(TKDT2DI64_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT2DI64_Test.Search(TKDT2DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT2DI64_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT2DI64_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT2DI64_Test);
end;


function TKDT3DI64.InternalBuildKdTree(const KDSourceBufferPtr: PKDT3DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT3DI64_Node;
  function SortCompare(const p1, p2: PKDT3DI64_Source; const axis: NativeInt): ShortInt;
  begin
    if p1^.buff[axis] = p2^.buff[axis] then
      begin
        if p1^.Index = p2^.Index then
            Result := 0
        else if p1^.Index < p2^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.buff[axis] < p2^.buff[axis] then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(const SortBuffer: PKDT3DI64_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT3DI64_Source;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer^[(L + R) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, axis) < 0 do
            Inc(i);
        while SortCompare(SortBuffer^[j], p, axis) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, axis);
      L := i;
    until i >= R;
  end;

var
  M: NativeInt;
  axis: NativeInt;
  kdBuffPtr: PKDT3DI64_SourceBuffer;
begin
  Result := nil;
  if PlanCount = 0 then
      Exit;

  if PlanCount = 1 then
    begin
      new(Result);
      Result^.Parent := nil;
      Result^.Right := nil;
      Result^.Left := nil;
      Result^.vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);
    end
  else
    begin
      axis := Depth mod KDT3DI64_Axis;
      M := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr^[0], 0, PlanCount - 1, axis);

      new(Result);
      Result^.Parent := nil;
      Result^.vec := kdBuffPtr^[M];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], M, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[M + 1], PlanCount - (M + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDT3DI64.GetData(const Index: NativeInt): PKDT3DI64_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT3DI64.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT3DI64.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT3DI64.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT3DI64_Node(KDNodes[i]));
      Inc(i);
    end;

  for i := 0 to length(KDStoreBuff) - 1 do
      KDStoreBuff[i].Token := '';

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDT3DI64.StoreBuffPtr: PKDT3DI64_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT3DI64.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DI64_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT3DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT3DI64.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DI64_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT3DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT3DI64.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DI64_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT3DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT3DI64.BuildKDTreeWithCluster(const inBuff: TKDT3DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT3DI64_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT3DI64_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT3DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT3DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT3DI64.BuildKDTreeWithCluster(const inBuff: TKDT3DI64_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT3DI64.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DI64_BuildCall);
var
  TempStoreBuff: TKDT3DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT3DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT3DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT3DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT3DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT3DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT3DI64.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DI64_BuildMethod);
var
  TempStoreBuff: TKDT3DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT3DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT3DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT3DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT3DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT3DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$IFNDEF FPC}


procedure TKDT3DI64.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DI64_BuildProc);
var
  TempStoreBuff: TKDT3DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT3DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT3DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT3DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT3DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT3DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$ENDIF FPC}


function TKDT3DI64.Search(const buff: TKDT3DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT3DI64_Node;

var
  NearestNeighbour: PKDT3DI64_Node;

  function FindParentNode(const buffPtr: PKDT3DI64_Vec; NodePtr: PKDT3DI64_Node): PKDT3DI64_Node;
  var
    Next: PKDT3DI64_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT3DI64_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT3DI64_Node; const buffPtr: PKDT3DI64_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT3DI64Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT3DI64_Axis;
    Dist := NodePtr^.vec^.buff[axis] - buffPtr^[axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.vec^.buff[axis] > buffPtr^[axis] then
            ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes)
        else
            ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end
    else
      begin
        ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes);
        ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end;
  end;

  function SortCompare(const buffPtr: PKDT3DI64_Vec; const p1, p2: PKDT3DI64_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT3DI64Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT3DI64Distance(buffPtr^, p2^.vec^.buff);
    if d1 = d2 then
      begin
        if p1^.vec^.Index = p2^.vec^.Index then
            Result := 0
        else if p1^.vec^.Index < p2^.vec^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT3DI64_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT3DI64_Node;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer[(L + R) shr 1];
      repeat
        while SortCompare(buffPtr, SortBuffer[i], p) < 0 do
            Inc(i);
        while SortCompare(buffPtr, SortBuffer[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, buffPtr);
      L := i;
    until i >= R;
  end;

var
  Parent: PKDT3DI64_Node;
begin
  Result := nil;
  SearchedDistanceMin := 0;
  SearchedCounter := 0;
  NearestNeighbour := nil;
  if NearestNodes <> nil then
      NearestNodes.Clear;
  if RootNode = nil then
      Exit;
  if Count = 0 then
      Exit;

  Parent := FindParentNode(@buff[0], RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := KDT3DI64Distance(buff, Parent^.vec^.buff);

  ScanSubtree(RootNode, @buff[0], 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @buff[0]);

      if NearestNodes.Count > 0 then
          Result := PKDT3DI64_Node(NearestNodes[0]);
    end;
end;

function TKDT3DI64.Search(const buff: TKDT3DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT3DI64_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT3DI64.Search(const buff: TKDT3DI64_Vec; var SearchedDistanceMin: Double): PKDT3DI64_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT3DI64.Search(const buff: TKDT3DI64_Vec): PKDT3DI64_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT3DI64.SearchToken(const buff: TKDT3DI64_Vec): TPascalString;
var
  p: PKDT3DI64_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT3DI64.Search(const inBuff: TKDT3DI64_DynamicVecBuffer; var OutBuff: TKDT3DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT3DI64_DynamicVecBuffer;
  outBuffPtr: PKDT3DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT3DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outBuffPtr^[pass] := p^.vec^.buff;
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outBuffPtr := @OutBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT3DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outBuffPtr^[pass] := p^.vec^.buff;
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT3DI64_Node;
begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutBuff[i] := p^.vec^.buff;
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT3DI64.Search(const inBuff: TKDT3DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT3DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT3DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT3DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT3DI64_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT3DI64.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT3DI64_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT3DI64_Vec));
      stream.write(KDStoreBuff[i].Index, 8);
      token_B := KDStoreBuff[i].Token.Bytes;
      token_L := length(token_B);
      stream.write(token_L, 4);
      if token_L > 0 then
        begin
          stream.write(token_B[0], token_L);
          SetLength(token_B, 0);
        end;
      Inc(i);
    end;
end;

procedure TKDT3DI64.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  Clear;

  stream.read(st, 4);
  stream.read(ID, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if ID <> KDT3DI64_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT3DI64_Vec)) <> SizeOf(TKDT3DI64_Vec) then
          begin
            Clear;
            Exit;
          end;
        if stream.read(KDStoreBuff[i].Index, 8) <> 8 then
          begin
            Clear;
            Exit;
          end;
        if stream.read(token_L, 4) <> 4 then
          begin
            Clear;
            Exit;
          end;
        if token_L > 0 then
          begin
            SetLength(token_B, token_L);
            if stream.read(token_B[0], token_L) <> token_L then
              begin
                Clear;
                Exit;
              end;
            KDStoreBuff[i].Token.Bytes := token_B;
            SetLength(token_B, 0);
          end
        else
            KDStoreBuff[i].Token := '';
        Inc(i);
      end;
  except
    Clear;
    Exit;
  end;

  SetLength(KDBuff, cnt);
  SetLength(KDNodes, cnt);

  i := 0;
  while i < cnt do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      Inc(i);
    end;

  if cnt > 0 then
    RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDT3DI64.SaveToFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT3DI64.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT3DI64.PrintNodeTree(const NodePtr: PKDT3DI64_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT3DI64_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT3DI64Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT3DI64.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT3DI64Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT3DI64.KDT3DI64Vec(const s: SystemString): TKDT3DI64_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT3DI64_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToInt(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT3DI64_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT3DI64.KDT3DI64Vec(const v: TKDT3DI64_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT3DI64_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT3DI64.KDT3DI64Pow(const v: TKDT3DI64_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT3DI64.KDT3DI64Distance(const v1, v2: TKDT3DI64_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT3DI64_Axis - 1 do
      Result := Result + KDT3DI64Pow(v2[i] - v1[i]);
end;

procedure TKDT3DI64.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT3DI64_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT3DI64.Test;
var
  TKDT3DI64_Test: TKDT3DI64;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT3DI64_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT3DI64_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT3DI64_Test := TKDT3DI64.Create;

  DoStatusNoLn('...');
  SetLength(TKDT3DI64_Test.TestBuff, 1000);
  for i := 0 to length(TKDT3DI64_Test.TestBuff) - 1 do
    for j := 0 to KDT3DI64_Axis - 1 do
        TKDT3DI64_Test.TestBuff[i][j] := i * KDT3DI64_Axis + j;

{$IFDEF FPC}
  TKDT3DI64_Test.BuildKDTreeM(length(TKDT3DI64_Test.TestBuff), nil, @TKDT3DI64_Test.Test_BuildM);
{$ELSE FPC}
  TKDT3DI64_Test.BuildKDTreeM(length(TKDT3DI64_Test.TestBuff), nil, TKDT3DI64_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT3DI64_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT3DI64_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT3DI64_Test.TestBuff) - 1 do
    begin
      p := TKDT3DI64_Test.Search(TKDT3DI64_Test.TestBuff[i]);
      if p^.vec^.Index <> i then
          errored := True;
      if not p^.vec^.Token.Same(umlIntToStr(i)) then
          errored := True;
      if errored then
          Break;
    end;
  DisposeObject(m64);

  if not errored then
    begin
      { parallel search test }
      DoStatusNoLn('...parallel');
      SetLength(TestResultBuff, length(TKDT3DI64_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT3DI64_Test.TestBuff));
      TKDT3DI64_Test.Search(TKDT3DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT3DI64Distance(TKDT3DI64_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT3DI64_Test.Clear;
      { kMean test }
      TKDT3DI64_Test.BuildKDTreeWithCluster(TKDT3DI64_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT3DI64_Test.Search(TKDT3DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT3DI64_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT3DI64_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT3DI64_Test);
end;


function TKDT4DI64.InternalBuildKdTree(const KDSourceBufferPtr: PKDT4DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT4DI64_Node;
  function SortCompare(const p1, p2: PKDT4DI64_Source; const axis: NativeInt): ShortInt;
  begin
    if p1^.buff[axis] = p2^.buff[axis] then
      begin
        if p1^.Index = p2^.Index then
            Result := 0
        else if p1^.Index < p2^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.buff[axis] < p2^.buff[axis] then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(const SortBuffer: PKDT4DI64_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT4DI64_Source;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer^[(L + R) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, axis) < 0 do
            Inc(i);
        while SortCompare(SortBuffer^[j], p, axis) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, axis);
      L := i;
    until i >= R;
  end;

var
  M: NativeInt;
  axis: NativeInt;
  kdBuffPtr: PKDT4DI64_SourceBuffer;
begin
  Result := nil;
  if PlanCount = 0 then
      Exit;

  if PlanCount = 1 then
    begin
      new(Result);
      Result^.Parent := nil;
      Result^.Right := nil;
      Result^.Left := nil;
      Result^.vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);
    end
  else
    begin
      axis := Depth mod KDT4DI64_Axis;
      M := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr^[0], 0, PlanCount - 1, axis);

      new(Result);
      Result^.Parent := nil;
      Result^.vec := kdBuffPtr^[M];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], M, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[M + 1], PlanCount - (M + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDT4DI64.GetData(const Index: NativeInt): PKDT4DI64_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT4DI64.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT4DI64.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT4DI64.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT4DI64_Node(KDNodes[i]));
      Inc(i);
    end;

  for i := 0 to length(KDStoreBuff) - 1 do
      KDStoreBuff[i].Token := '';

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDT4DI64.StoreBuffPtr: PKDT4DI64_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT4DI64.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DI64_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT4DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT4DI64.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DI64_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT4DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT4DI64.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DI64_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT4DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT4DI64.BuildKDTreeWithCluster(const inBuff: TKDT4DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT4DI64_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT4DI64_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT4DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT4DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT4DI64.BuildKDTreeWithCluster(const inBuff: TKDT4DI64_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT4DI64.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DI64_BuildCall);
var
  TempStoreBuff: TKDT4DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT4DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT4DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT4DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT4DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT4DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT4DI64.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DI64_BuildMethod);
var
  TempStoreBuff: TKDT4DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT4DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT4DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT4DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT4DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT4DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$IFNDEF FPC}


procedure TKDT4DI64.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DI64_BuildProc);
var
  TempStoreBuff: TKDT4DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT4DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT4DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT4DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT4DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT4DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$ENDIF FPC}


function TKDT4DI64.Search(const buff: TKDT4DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT4DI64_Node;

var
  NearestNeighbour: PKDT4DI64_Node;

  function FindParentNode(const buffPtr: PKDT4DI64_Vec; NodePtr: PKDT4DI64_Node): PKDT4DI64_Node;
  var
    Next: PKDT4DI64_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT4DI64_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT4DI64_Node; const buffPtr: PKDT4DI64_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT4DI64Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT4DI64_Axis;
    Dist := NodePtr^.vec^.buff[axis] - buffPtr^[axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.vec^.buff[axis] > buffPtr^[axis] then
            ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes)
        else
            ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end
    else
      begin
        ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes);
        ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end;
  end;

  function SortCompare(const buffPtr: PKDT4DI64_Vec; const p1, p2: PKDT4DI64_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT4DI64Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT4DI64Distance(buffPtr^, p2^.vec^.buff);
    if d1 = d2 then
      begin
        if p1^.vec^.Index = p2^.vec^.Index then
            Result := 0
        else if p1^.vec^.Index < p2^.vec^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT4DI64_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT4DI64_Node;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer[(L + R) shr 1];
      repeat
        while SortCompare(buffPtr, SortBuffer[i], p) < 0 do
            Inc(i);
        while SortCompare(buffPtr, SortBuffer[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, buffPtr);
      L := i;
    until i >= R;
  end;

var
  Parent: PKDT4DI64_Node;
begin
  Result := nil;
  SearchedDistanceMin := 0;
  SearchedCounter := 0;
  NearestNeighbour := nil;
  if NearestNodes <> nil then
      NearestNodes.Clear;
  if RootNode = nil then
      Exit;
  if Count = 0 then
      Exit;

  Parent := FindParentNode(@buff[0], RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := KDT4DI64Distance(buff, Parent^.vec^.buff);

  ScanSubtree(RootNode, @buff[0], 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @buff[0]);

      if NearestNodes.Count > 0 then
          Result := PKDT4DI64_Node(NearestNodes[0]);
    end;
end;

function TKDT4DI64.Search(const buff: TKDT4DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT4DI64_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT4DI64.Search(const buff: TKDT4DI64_Vec; var SearchedDistanceMin: Double): PKDT4DI64_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT4DI64.Search(const buff: TKDT4DI64_Vec): PKDT4DI64_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT4DI64.SearchToken(const buff: TKDT4DI64_Vec): TPascalString;
var
  p: PKDT4DI64_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT4DI64.Search(const inBuff: TKDT4DI64_DynamicVecBuffer; var OutBuff: TKDT4DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT4DI64_DynamicVecBuffer;
  outBuffPtr: PKDT4DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT4DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outBuffPtr^[pass] := p^.vec^.buff;
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outBuffPtr := @OutBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT4DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outBuffPtr^[pass] := p^.vec^.buff;
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT4DI64_Node;
begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutBuff[i] := p^.vec^.buff;
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT4DI64.Search(const inBuff: TKDT4DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT4DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT4DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT4DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT4DI64_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT4DI64.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT4DI64_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT4DI64_Vec));
      stream.write(KDStoreBuff[i].Index, 8);
      token_B := KDStoreBuff[i].Token.Bytes;
      token_L := length(token_B);
      stream.write(token_L, 4);
      if token_L > 0 then
        begin
          stream.write(token_B[0], token_L);
          SetLength(token_B, 0);
        end;
      Inc(i);
    end;
end;

procedure TKDT4DI64.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  Clear;

  stream.read(st, 4);
  stream.read(ID, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if ID <> KDT4DI64_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT4DI64_Vec)) <> SizeOf(TKDT4DI64_Vec) then
          begin
            Clear;
            Exit;
          end;
        if stream.read(KDStoreBuff[i].Index, 8) <> 8 then
          begin
            Clear;
            Exit;
          end;
        if stream.read(token_L, 4) <> 4 then
          begin
            Clear;
            Exit;
          end;
        if token_L > 0 then
          begin
            SetLength(token_B, token_L);
            if stream.read(token_B[0], token_L) <> token_L then
              begin
                Clear;
                Exit;
              end;
            KDStoreBuff[i].Token.Bytes := token_B;
            SetLength(token_B, 0);
          end
        else
            KDStoreBuff[i].Token := '';
        Inc(i);
      end;
  except
    Clear;
    Exit;
  end;

  SetLength(KDBuff, cnt);
  SetLength(KDNodes, cnt);

  i := 0;
  while i < cnt do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      Inc(i);
    end;

  if cnt > 0 then
    RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDT4DI64.SaveToFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT4DI64.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT4DI64.PrintNodeTree(const NodePtr: PKDT4DI64_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT4DI64_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT4DI64Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT4DI64.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT4DI64Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT4DI64.KDT4DI64Vec(const s: SystemString): TKDT4DI64_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT4DI64_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToInt(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT4DI64_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT4DI64.KDT4DI64Vec(const v: TKDT4DI64_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT4DI64_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT4DI64.KDT4DI64Pow(const v: TKDT4DI64_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT4DI64.KDT4DI64Distance(const v1, v2: TKDT4DI64_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT4DI64_Axis - 1 do
      Result := Result + KDT4DI64Pow(v2[i] - v1[i]);
end;

procedure TKDT4DI64.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT4DI64_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT4DI64.Test;
var
  TKDT4DI64_Test: TKDT4DI64;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT4DI64_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT4DI64_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT4DI64_Test := TKDT4DI64.Create;

  DoStatusNoLn('...');
  SetLength(TKDT4DI64_Test.TestBuff, 1000);
  for i := 0 to length(TKDT4DI64_Test.TestBuff) - 1 do
    for j := 0 to KDT4DI64_Axis - 1 do
        TKDT4DI64_Test.TestBuff[i][j] := i * KDT4DI64_Axis + j;

{$IFDEF FPC}
  TKDT4DI64_Test.BuildKDTreeM(length(TKDT4DI64_Test.TestBuff), nil, @TKDT4DI64_Test.Test_BuildM);
{$ELSE FPC}
  TKDT4DI64_Test.BuildKDTreeM(length(TKDT4DI64_Test.TestBuff), nil, TKDT4DI64_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT4DI64_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT4DI64_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT4DI64_Test.TestBuff) - 1 do
    begin
      p := TKDT4DI64_Test.Search(TKDT4DI64_Test.TestBuff[i]);
      if p^.vec^.Index <> i then
          errored := True;
      if not p^.vec^.Token.Same(umlIntToStr(i)) then
          errored := True;
      if errored then
          Break;
    end;
  DisposeObject(m64);

  if not errored then
    begin
      { parallel search test }
      DoStatusNoLn('...parallel');
      SetLength(TestResultBuff, length(TKDT4DI64_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT4DI64_Test.TestBuff));
      TKDT4DI64_Test.Search(TKDT4DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT4DI64Distance(TKDT4DI64_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT4DI64_Test.Clear;
      { kMean test }
      TKDT4DI64_Test.BuildKDTreeWithCluster(TKDT4DI64_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT4DI64_Test.Search(TKDT4DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT4DI64_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT4DI64_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT4DI64_Test);
end;


function TKDT5DI64.InternalBuildKdTree(const KDSourceBufferPtr: PKDT5DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT5DI64_Node;
  function SortCompare(const p1, p2: PKDT5DI64_Source; const axis: NativeInt): ShortInt;
  begin
    if p1^.buff[axis] = p2^.buff[axis] then
      begin
        if p1^.Index = p2^.Index then
            Result := 0
        else if p1^.Index < p2^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.buff[axis] < p2^.buff[axis] then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(const SortBuffer: PKDT5DI64_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT5DI64_Source;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer^[(L + R) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, axis) < 0 do
            Inc(i);
        while SortCompare(SortBuffer^[j], p, axis) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, axis);
      L := i;
    until i >= R;
  end;

var
  M: NativeInt;
  axis: NativeInt;
  kdBuffPtr: PKDT5DI64_SourceBuffer;
begin
  Result := nil;
  if PlanCount = 0 then
      Exit;

  if PlanCount = 1 then
    begin
      new(Result);
      Result^.Parent := nil;
      Result^.Right := nil;
      Result^.Left := nil;
      Result^.vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);
    end
  else
    begin
      axis := Depth mod KDT5DI64_Axis;
      M := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr^[0], 0, PlanCount - 1, axis);

      new(Result);
      Result^.Parent := nil;
      Result^.vec := kdBuffPtr^[M];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], M, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[M + 1], PlanCount - (M + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDT5DI64.GetData(const Index: NativeInt): PKDT5DI64_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT5DI64.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT5DI64.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT5DI64.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT5DI64_Node(KDNodes[i]));
      Inc(i);
    end;

  for i := 0 to length(KDStoreBuff) - 1 do
      KDStoreBuff[i].Token := '';

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDT5DI64.StoreBuffPtr: PKDT5DI64_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT5DI64.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DI64_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT5DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT5DI64.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DI64_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT5DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT5DI64.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DI64_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT5DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT5DI64.BuildKDTreeWithCluster(const inBuff: TKDT5DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT5DI64_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT5DI64_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT5DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT5DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT5DI64.BuildKDTreeWithCluster(const inBuff: TKDT5DI64_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT5DI64.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DI64_BuildCall);
var
  TempStoreBuff: TKDT5DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT5DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT5DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT5DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT5DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT5DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT5DI64.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DI64_BuildMethod);
var
  TempStoreBuff: TKDT5DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT5DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT5DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT5DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT5DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT5DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$IFNDEF FPC}


procedure TKDT5DI64.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DI64_BuildProc);
var
  TempStoreBuff: TKDT5DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT5DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT5DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT5DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT5DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT5DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$ENDIF FPC}


function TKDT5DI64.Search(const buff: TKDT5DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT5DI64_Node;

var
  NearestNeighbour: PKDT5DI64_Node;

  function FindParentNode(const buffPtr: PKDT5DI64_Vec; NodePtr: PKDT5DI64_Node): PKDT5DI64_Node;
  var
    Next: PKDT5DI64_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT5DI64_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT5DI64_Node; const buffPtr: PKDT5DI64_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT5DI64Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT5DI64_Axis;
    Dist := NodePtr^.vec^.buff[axis] - buffPtr^[axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.vec^.buff[axis] > buffPtr^[axis] then
            ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes)
        else
            ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end
    else
      begin
        ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes);
        ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end;
  end;

  function SortCompare(const buffPtr: PKDT5DI64_Vec; const p1, p2: PKDT5DI64_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT5DI64Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT5DI64Distance(buffPtr^, p2^.vec^.buff);
    if d1 = d2 then
      begin
        if p1^.vec^.Index = p2^.vec^.Index then
            Result := 0
        else if p1^.vec^.Index < p2^.vec^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT5DI64_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT5DI64_Node;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer[(L + R) shr 1];
      repeat
        while SortCompare(buffPtr, SortBuffer[i], p) < 0 do
            Inc(i);
        while SortCompare(buffPtr, SortBuffer[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, buffPtr);
      L := i;
    until i >= R;
  end;

var
  Parent: PKDT5DI64_Node;
begin
  Result := nil;
  SearchedDistanceMin := 0;
  SearchedCounter := 0;
  NearestNeighbour := nil;
  if NearestNodes <> nil then
      NearestNodes.Clear;
  if RootNode = nil then
      Exit;
  if Count = 0 then
      Exit;

  Parent := FindParentNode(@buff[0], RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := KDT5DI64Distance(buff, Parent^.vec^.buff);

  ScanSubtree(RootNode, @buff[0], 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @buff[0]);

      if NearestNodes.Count > 0 then
          Result := PKDT5DI64_Node(NearestNodes[0]);
    end;
end;

function TKDT5DI64.Search(const buff: TKDT5DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT5DI64_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT5DI64.Search(const buff: TKDT5DI64_Vec; var SearchedDistanceMin: Double): PKDT5DI64_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT5DI64.Search(const buff: TKDT5DI64_Vec): PKDT5DI64_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT5DI64.SearchToken(const buff: TKDT5DI64_Vec): TPascalString;
var
  p: PKDT5DI64_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT5DI64.Search(const inBuff: TKDT5DI64_DynamicVecBuffer; var OutBuff: TKDT5DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT5DI64_DynamicVecBuffer;
  outBuffPtr: PKDT5DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT5DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outBuffPtr^[pass] := p^.vec^.buff;
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outBuffPtr := @OutBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT5DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outBuffPtr^[pass] := p^.vec^.buff;
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT5DI64_Node;
begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutBuff[i] := p^.vec^.buff;
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT5DI64.Search(const inBuff: TKDT5DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT5DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT5DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT5DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT5DI64_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT5DI64.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT5DI64_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT5DI64_Vec));
      stream.write(KDStoreBuff[i].Index, 8);
      token_B := KDStoreBuff[i].Token.Bytes;
      token_L := length(token_B);
      stream.write(token_L, 4);
      if token_L > 0 then
        begin
          stream.write(token_B[0], token_L);
          SetLength(token_B, 0);
        end;
      Inc(i);
    end;
end;

procedure TKDT5DI64.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  Clear;

  stream.read(st, 4);
  stream.read(ID, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if ID <> KDT5DI64_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT5DI64_Vec)) <> SizeOf(TKDT5DI64_Vec) then
          begin
            Clear;
            Exit;
          end;
        if stream.read(KDStoreBuff[i].Index, 8) <> 8 then
          begin
            Clear;
            Exit;
          end;
        if stream.read(token_L, 4) <> 4 then
          begin
            Clear;
            Exit;
          end;
        if token_L > 0 then
          begin
            SetLength(token_B, token_L);
            if stream.read(token_B[0], token_L) <> token_L then
              begin
                Clear;
                Exit;
              end;
            KDStoreBuff[i].Token.Bytes := token_B;
            SetLength(token_B, 0);
          end
        else
            KDStoreBuff[i].Token := '';
        Inc(i);
      end;
  except
    Clear;
    Exit;
  end;

  SetLength(KDBuff, cnt);
  SetLength(KDNodes, cnt);

  i := 0;
  while i < cnt do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      Inc(i);
    end;

  if cnt > 0 then
    RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDT5DI64.SaveToFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT5DI64.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT5DI64.PrintNodeTree(const NodePtr: PKDT5DI64_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT5DI64_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT5DI64Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT5DI64.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT5DI64Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT5DI64.KDT5DI64Vec(const s: SystemString): TKDT5DI64_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT5DI64_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToInt(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT5DI64_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT5DI64.KDT5DI64Vec(const v: TKDT5DI64_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT5DI64_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT5DI64.KDT5DI64Pow(const v: TKDT5DI64_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT5DI64.KDT5DI64Distance(const v1, v2: TKDT5DI64_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT5DI64_Axis - 1 do
      Result := Result + KDT5DI64Pow(v2[i] - v1[i]);
end;

procedure TKDT5DI64.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT5DI64_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT5DI64.Test;
var
  TKDT5DI64_Test: TKDT5DI64;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT5DI64_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT5DI64_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT5DI64_Test := TKDT5DI64.Create;

  DoStatusNoLn('...');
  SetLength(TKDT5DI64_Test.TestBuff, 1000);
  for i := 0 to length(TKDT5DI64_Test.TestBuff) - 1 do
    for j := 0 to KDT5DI64_Axis - 1 do
        TKDT5DI64_Test.TestBuff[i][j] := i * KDT5DI64_Axis + j;

{$IFDEF FPC}
  TKDT5DI64_Test.BuildKDTreeM(length(TKDT5DI64_Test.TestBuff), nil, @TKDT5DI64_Test.Test_BuildM);
{$ELSE FPC}
  TKDT5DI64_Test.BuildKDTreeM(length(TKDT5DI64_Test.TestBuff), nil, TKDT5DI64_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT5DI64_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT5DI64_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT5DI64_Test.TestBuff) - 1 do
    begin
      p := TKDT5DI64_Test.Search(TKDT5DI64_Test.TestBuff[i]);
      if p^.vec^.Index <> i then
          errored := True;
      if not p^.vec^.Token.Same(umlIntToStr(i)) then
          errored := True;
      if errored then
          Break;
    end;
  DisposeObject(m64);

  if not errored then
    begin
      { parallel search test }
      DoStatusNoLn('...parallel');
      SetLength(TestResultBuff, length(TKDT5DI64_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT5DI64_Test.TestBuff));
      TKDT5DI64_Test.Search(TKDT5DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT5DI64Distance(TKDT5DI64_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT5DI64_Test.Clear;
      { kMean test }
      TKDT5DI64_Test.BuildKDTreeWithCluster(TKDT5DI64_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT5DI64_Test.Search(TKDT5DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT5DI64_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT5DI64_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT5DI64_Test);
end;


function TKDT6DI64.InternalBuildKdTree(const KDSourceBufferPtr: PKDT6DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT6DI64_Node;
  function SortCompare(const p1, p2: PKDT6DI64_Source; const axis: NativeInt): ShortInt;
  begin
    if p1^.buff[axis] = p2^.buff[axis] then
      begin
        if p1^.Index = p2^.Index then
            Result := 0
        else if p1^.Index < p2^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.buff[axis] < p2^.buff[axis] then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(const SortBuffer: PKDT6DI64_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT6DI64_Source;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer^[(L + R) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, axis) < 0 do
            Inc(i);
        while SortCompare(SortBuffer^[j], p, axis) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, axis);
      L := i;
    until i >= R;
  end;

var
  M: NativeInt;
  axis: NativeInt;
  kdBuffPtr: PKDT6DI64_SourceBuffer;
begin
  Result := nil;
  if PlanCount = 0 then
      Exit;

  if PlanCount = 1 then
    begin
      new(Result);
      Result^.Parent := nil;
      Result^.Right := nil;
      Result^.Left := nil;
      Result^.vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);
    end
  else
    begin
      axis := Depth mod KDT6DI64_Axis;
      M := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr^[0], 0, PlanCount - 1, axis);

      new(Result);
      Result^.Parent := nil;
      Result^.vec := kdBuffPtr^[M];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], M, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[M + 1], PlanCount - (M + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDT6DI64.GetData(const Index: NativeInt): PKDT6DI64_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT6DI64.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT6DI64.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT6DI64.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT6DI64_Node(KDNodes[i]));
      Inc(i);
    end;

  for i := 0 to length(KDStoreBuff) - 1 do
      KDStoreBuff[i].Token := '';

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDT6DI64.StoreBuffPtr: PKDT6DI64_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT6DI64.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DI64_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT6DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT6DI64.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DI64_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT6DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT6DI64.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DI64_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT6DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT6DI64.BuildKDTreeWithCluster(const inBuff: TKDT6DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT6DI64_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT6DI64_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT6DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT6DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT6DI64.BuildKDTreeWithCluster(const inBuff: TKDT6DI64_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT6DI64.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DI64_BuildCall);
var
  TempStoreBuff: TKDT6DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT6DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT6DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT6DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT6DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT6DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT6DI64.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DI64_BuildMethod);
var
  TempStoreBuff: TKDT6DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT6DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT6DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT6DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT6DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT6DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$IFNDEF FPC}


procedure TKDT6DI64.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DI64_BuildProc);
var
  TempStoreBuff: TKDT6DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT6DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT6DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT6DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT6DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT6DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$ENDIF FPC}


function TKDT6DI64.Search(const buff: TKDT6DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT6DI64_Node;

var
  NearestNeighbour: PKDT6DI64_Node;

  function FindParentNode(const buffPtr: PKDT6DI64_Vec; NodePtr: PKDT6DI64_Node): PKDT6DI64_Node;
  var
    Next: PKDT6DI64_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT6DI64_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT6DI64_Node; const buffPtr: PKDT6DI64_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT6DI64Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT6DI64_Axis;
    Dist := NodePtr^.vec^.buff[axis] - buffPtr^[axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.vec^.buff[axis] > buffPtr^[axis] then
            ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes)
        else
            ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end
    else
      begin
        ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes);
        ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end;
  end;

  function SortCompare(const buffPtr: PKDT6DI64_Vec; const p1, p2: PKDT6DI64_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT6DI64Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT6DI64Distance(buffPtr^, p2^.vec^.buff);
    if d1 = d2 then
      begin
        if p1^.vec^.Index = p2^.vec^.Index then
            Result := 0
        else if p1^.vec^.Index < p2^.vec^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT6DI64_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT6DI64_Node;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer[(L + R) shr 1];
      repeat
        while SortCompare(buffPtr, SortBuffer[i], p) < 0 do
            Inc(i);
        while SortCompare(buffPtr, SortBuffer[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, buffPtr);
      L := i;
    until i >= R;
  end;

var
  Parent: PKDT6DI64_Node;
begin
  Result := nil;
  SearchedDistanceMin := 0;
  SearchedCounter := 0;
  NearestNeighbour := nil;
  if NearestNodes <> nil then
      NearestNodes.Clear;
  if RootNode = nil then
      Exit;
  if Count = 0 then
      Exit;

  Parent := FindParentNode(@buff[0], RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := KDT6DI64Distance(buff, Parent^.vec^.buff);

  ScanSubtree(RootNode, @buff[0], 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @buff[0]);

      if NearestNodes.Count > 0 then
          Result := PKDT6DI64_Node(NearestNodes[0]);
    end;
end;

function TKDT6DI64.Search(const buff: TKDT6DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT6DI64_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT6DI64.Search(const buff: TKDT6DI64_Vec; var SearchedDistanceMin: Double): PKDT6DI64_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT6DI64.Search(const buff: TKDT6DI64_Vec): PKDT6DI64_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT6DI64.SearchToken(const buff: TKDT6DI64_Vec): TPascalString;
var
  p: PKDT6DI64_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT6DI64.Search(const inBuff: TKDT6DI64_DynamicVecBuffer; var OutBuff: TKDT6DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT6DI64_DynamicVecBuffer;
  outBuffPtr: PKDT6DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT6DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outBuffPtr^[pass] := p^.vec^.buff;
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outBuffPtr := @OutBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT6DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outBuffPtr^[pass] := p^.vec^.buff;
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT6DI64_Node;
begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutBuff[i] := p^.vec^.buff;
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT6DI64.Search(const inBuff: TKDT6DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT6DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT6DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT6DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT6DI64_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT6DI64.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT6DI64_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT6DI64_Vec));
      stream.write(KDStoreBuff[i].Index, 8);
      token_B := KDStoreBuff[i].Token.Bytes;
      token_L := length(token_B);
      stream.write(token_L, 4);
      if token_L > 0 then
        begin
          stream.write(token_B[0], token_L);
          SetLength(token_B, 0);
        end;
      Inc(i);
    end;
end;

procedure TKDT6DI64.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  Clear;

  stream.read(st, 4);
  stream.read(ID, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if ID <> KDT6DI64_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT6DI64_Vec)) <> SizeOf(TKDT6DI64_Vec) then
          begin
            Clear;
            Exit;
          end;
        if stream.read(KDStoreBuff[i].Index, 8) <> 8 then
          begin
            Clear;
            Exit;
          end;
        if stream.read(token_L, 4) <> 4 then
          begin
            Clear;
            Exit;
          end;
        if token_L > 0 then
          begin
            SetLength(token_B, token_L);
            if stream.read(token_B[0], token_L) <> token_L then
              begin
                Clear;
                Exit;
              end;
            KDStoreBuff[i].Token.Bytes := token_B;
            SetLength(token_B, 0);
          end
        else
            KDStoreBuff[i].Token := '';
        Inc(i);
      end;
  except
    Clear;
    Exit;
  end;

  SetLength(KDBuff, cnt);
  SetLength(KDNodes, cnt);

  i := 0;
  while i < cnt do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      Inc(i);
    end;

  if cnt > 0 then
    RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDT6DI64.SaveToFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT6DI64.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT6DI64.PrintNodeTree(const NodePtr: PKDT6DI64_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT6DI64_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT6DI64Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT6DI64.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT6DI64Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT6DI64.KDT6DI64Vec(const s: SystemString): TKDT6DI64_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT6DI64_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToInt(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT6DI64_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT6DI64.KDT6DI64Vec(const v: TKDT6DI64_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT6DI64_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT6DI64.KDT6DI64Pow(const v: TKDT6DI64_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT6DI64.KDT6DI64Distance(const v1, v2: TKDT6DI64_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT6DI64_Axis - 1 do
      Result := Result + KDT6DI64Pow(v2[i] - v1[i]);
end;

procedure TKDT6DI64.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT6DI64_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT6DI64.Test;
var
  TKDT6DI64_Test: TKDT6DI64;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT6DI64_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT6DI64_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT6DI64_Test := TKDT6DI64.Create;

  DoStatusNoLn('...');
  SetLength(TKDT6DI64_Test.TestBuff, 1000);
  for i := 0 to length(TKDT6DI64_Test.TestBuff) - 1 do
    for j := 0 to KDT6DI64_Axis - 1 do
        TKDT6DI64_Test.TestBuff[i][j] := i * KDT6DI64_Axis + j;

{$IFDEF FPC}
  TKDT6DI64_Test.BuildKDTreeM(length(TKDT6DI64_Test.TestBuff), nil, @TKDT6DI64_Test.Test_BuildM);
{$ELSE FPC}
  TKDT6DI64_Test.BuildKDTreeM(length(TKDT6DI64_Test.TestBuff), nil, TKDT6DI64_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT6DI64_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT6DI64_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT6DI64_Test.TestBuff) - 1 do
    begin
      p := TKDT6DI64_Test.Search(TKDT6DI64_Test.TestBuff[i]);
      if p^.vec^.Index <> i then
          errored := True;
      if not p^.vec^.Token.Same(umlIntToStr(i)) then
          errored := True;
      if errored then
          Break;
    end;
  DisposeObject(m64);

  if not errored then
    begin
      { parallel search test }
      DoStatusNoLn('...parallel');
      SetLength(TestResultBuff, length(TKDT6DI64_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT6DI64_Test.TestBuff));
      TKDT6DI64_Test.Search(TKDT6DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT6DI64Distance(TKDT6DI64_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT6DI64_Test.Clear;
      { kMean test }
      TKDT6DI64_Test.BuildKDTreeWithCluster(TKDT6DI64_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT6DI64_Test.Search(TKDT6DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT6DI64_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT6DI64_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT6DI64_Test);
end;


function TKDT7DI64.InternalBuildKdTree(const KDSourceBufferPtr: PKDT7DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT7DI64_Node;
  function SortCompare(const p1, p2: PKDT7DI64_Source; const axis: NativeInt): ShortInt;
  begin
    if p1^.buff[axis] = p2^.buff[axis] then
      begin
        if p1^.Index = p2^.Index then
            Result := 0
        else if p1^.Index < p2^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.buff[axis] < p2^.buff[axis] then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(const SortBuffer: PKDT7DI64_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT7DI64_Source;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer^[(L + R) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, axis) < 0 do
            Inc(i);
        while SortCompare(SortBuffer^[j], p, axis) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, axis);
      L := i;
    until i >= R;
  end;

var
  M: NativeInt;
  axis: NativeInt;
  kdBuffPtr: PKDT7DI64_SourceBuffer;
begin
  Result := nil;
  if PlanCount = 0 then
      Exit;

  if PlanCount = 1 then
    begin
      new(Result);
      Result^.Parent := nil;
      Result^.Right := nil;
      Result^.Left := nil;
      Result^.vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);
    end
  else
    begin
      axis := Depth mod KDT7DI64_Axis;
      M := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr^[0], 0, PlanCount - 1, axis);

      new(Result);
      Result^.Parent := nil;
      Result^.vec := kdBuffPtr^[M];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], M, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[M + 1], PlanCount - (M + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDT7DI64.GetData(const Index: NativeInt): PKDT7DI64_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT7DI64.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT7DI64.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT7DI64.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT7DI64_Node(KDNodes[i]));
      Inc(i);
    end;

  for i := 0 to length(KDStoreBuff) - 1 do
      KDStoreBuff[i].Token := '';

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDT7DI64.StoreBuffPtr: PKDT7DI64_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT7DI64.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DI64_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT7DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT7DI64.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DI64_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT7DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT7DI64.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DI64_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT7DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT7DI64.BuildKDTreeWithCluster(const inBuff: TKDT7DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT7DI64_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT7DI64_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT7DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT7DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT7DI64.BuildKDTreeWithCluster(const inBuff: TKDT7DI64_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT7DI64.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DI64_BuildCall);
var
  TempStoreBuff: TKDT7DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT7DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT7DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT7DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT7DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT7DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT7DI64.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DI64_BuildMethod);
var
  TempStoreBuff: TKDT7DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT7DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT7DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT7DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT7DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT7DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$IFNDEF FPC}


procedure TKDT7DI64.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DI64_BuildProc);
var
  TempStoreBuff: TKDT7DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT7DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT7DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT7DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT7DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT7DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$ENDIF FPC}


function TKDT7DI64.Search(const buff: TKDT7DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT7DI64_Node;

var
  NearestNeighbour: PKDT7DI64_Node;

  function FindParentNode(const buffPtr: PKDT7DI64_Vec; NodePtr: PKDT7DI64_Node): PKDT7DI64_Node;
  var
    Next: PKDT7DI64_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT7DI64_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT7DI64_Node; const buffPtr: PKDT7DI64_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT7DI64Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT7DI64_Axis;
    Dist := NodePtr^.vec^.buff[axis] - buffPtr^[axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.vec^.buff[axis] > buffPtr^[axis] then
            ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes)
        else
            ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end
    else
      begin
        ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes);
        ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end;
  end;

  function SortCompare(const buffPtr: PKDT7DI64_Vec; const p1, p2: PKDT7DI64_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT7DI64Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT7DI64Distance(buffPtr^, p2^.vec^.buff);
    if d1 = d2 then
      begin
        if p1^.vec^.Index = p2^.vec^.Index then
            Result := 0
        else if p1^.vec^.Index < p2^.vec^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT7DI64_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT7DI64_Node;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer[(L + R) shr 1];
      repeat
        while SortCompare(buffPtr, SortBuffer[i], p) < 0 do
            Inc(i);
        while SortCompare(buffPtr, SortBuffer[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, buffPtr);
      L := i;
    until i >= R;
  end;

var
  Parent: PKDT7DI64_Node;
begin
  Result := nil;
  SearchedDistanceMin := 0;
  SearchedCounter := 0;
  NearestNeighbour := nil;
  if NearestNodes <> nil then
      NearestNodes.Clear;
  if RootNode = nil then
      Exit;
  if Count = 0 then
      Exit;

  Parent := FindParentNode(@buff[0], RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := KDT7DI64Distance(buff, Parent^.vec^.buff);

  ScanSubtree(RootNode, @buff[0], 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @buff[0]);

      if NearestNodes.Count > 0 then
          Result := PKDT7DI64_Node(NearestNodes[0]);
    end;
end;

function TKDT7DI64.Search(const buff: TKDT7DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT7DI64_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT7DI64.Search(const buff: TKDT7DI64_Vec; var SearchedDistanceMin: Double): PKDT7DI64_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT7DI64.Search(const buff: TKDT7DI64_Vec): PKDT7DI64_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT7DI64.SearchToken(const buff: TKDT7DI64_Vec): TPascalString;
var
  p: PKDT7DI64_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT7DI64.Search(const inBuff: TKDT7DI64_DynamicVecBuffer; var OutBuff: TKDT7DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT7DI64_DynamicVecBuffer;
  outBuffPtr: PKDT7DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT7DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outBuffPtr^[pass] := p^.vec^.buff;
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outBuffPtr := @OutBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT7DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outBuffPtr^[pass] := p^.vec^.buff;
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT7DI64_Node;
begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutBuff[i] := p^.vec^.buff;
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT7DI64.Search(const inBuff: TKDT7DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT7DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT7DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT7DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT7DI64_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT7DI64.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT7DI64_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT7DI64_Vec));
      stream.write(KDStoreBuff[i].Index, 8);
      token_B := KDStoreBuff[i].Token.Bytes;
      token_L := length(token_B);
      stream.write(token_L, 4);
      if token_L > 0 then
        begin
          stream.write(token_B[0], token_L);
          SetLength(token_B, 0);
        end;
      Inc(i);
    end;
end;

procedure TKDT7DI64.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  Clear;

  stream.read(st, 4);
  stream.read(ID, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if ID <> KDT7DI64_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT7DI64_Vec)) <> SizeOf(TKDT7DI64_Vec) then
          begin
            Clear;
            Exit;
          end;
        if stream.read(KDStoreBuff[i].Index, 8) <> 8 then
          begin
            Clear;
            Exit;
          end;
        if stream.read(token_L, 4) <> 4 then
          begin
            Clear;
            Exit;
          end;
        if token_L > 0 then
          begin
            SetLength(token_B, token_L);
            if stream.read(token_B[0], token_L) <> token_L then
              begin
                Clear;
                Exit;
              end;
            KDStoreBuff[i].Token.Bytes := token_B;
            SetLength(token_B, 0);
          end
        else
            KDStoreBuff[i].Token := '';
        Inc(i);
      end;
  except
    Clear;
    Exit;
  end;

  SetLength(KDBuff, cnt);
  SetLength(KDNodes, cnt);

  i := 0;
  while i < cnt do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      Inc(i);
    end;

  if cnt > 0 then
    RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDT7DI64.SaveToFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT7DI64.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT7DI64.PrintNodeTree(const NodePtr: PKDT7DI64_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT7DI64_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT7DI64Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT7DI64.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT7DI64Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT7DI64.KDT7DI64Vec(const s: SystemString): TKDT7DI64_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT7DI64_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToInt(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT7DI64_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT7DI64.KDT7DI64Vec(const v: TKDT7DI64_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT7DI64_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT7DI64.KDT7DI64Pow(const v: TKDT7DI64_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT7DI64.KDT7DI64Distance(const v1, v2: TKDT7DI64_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT7DI64_Axis - 1 do
      Result := Result + KDT7DI64Pow(v2[i] - v1[i]);
end;

procedure TKDT7DI64.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT7DI64_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT7DI64.Test;
var
  TKDT7DI64_Test: TKDT7DI64;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT7DI64_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT7DI64_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT7DI64_Test := TKDT7DI64.Create;

  DoStatusNoLn('...');
  SetLength(TKDT7DI64_Test.TestBuff, 1000);
  for i := 0 to length(TKDT7DI64_Test.TestBuff) - 1 do
    for j := 0 to KDT7DI64_Axis - 1 do
        TKDT7DI64_Test.TestBuff[i][j] := i * KDT7DI64_Axis + j;

{$IFDEF FPC}
  TKDT7DI64_Test.BuildKDTreeM(length(TKDT7DI64_Test.TestBuff), nil, @TKDT7DI64_Test.Test_BuildM);
{$ELSE FPC}
  TKDT7DI64_Test.BuildKDTreeM(length(TKDT7DI64_Test.TestBuff), nil, TKDT7DI64_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT7DI64_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT7DI64_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT7DI64_Test.TestBuff) - 1 do
    begin
      p := TKDT7DI64_Test.Search(TKDT7DI64_Test.TestBuff[i]);
      if p^.vec^.Index <> i then
          errored := True;
      if not p^.vec^.Token.Same(umlIntToStr(i)) then
          errored := True;
      if errored then
          Break;
    end;
  DisposeObject(m64);

  if not errored then
    begin
      { parallel search test }
      DoStatusNoLn('...parallel');
      SetLength(TestResultBuff, length(TKDT7DI64_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT7DI64_Test.TestBuff));
      TKDT7DI64_Test.Search(TKDT7DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT7DI64Distance(TKDT7DI64_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT7DI64_Test.Clear;
      { kMean test }
      TKDT7DI64_Test.BuildKDTreeWithCluster(TKDT7DI64_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT7DI64_Test.Search(TKDT7DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT7DI64_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT7DI64_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT7DI64_Test);
end;


function TKDT8DI64.InternalBuildKdTree(const KDSourceBufferPtr: PKDT8DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT8DI64_Node;
  function SortCompare(const p1, p2: PKDT8DI64_Source; const axis: NativeInt): ShortInt;
  begin
    if p1^.buff[axis] = p2^.buff[axis] then
      begin
        if p1^.Index = p2^.Index then
            Result := 0
        else if p1^.Index < p2^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.buff[axis] < p2^.buff[axis] then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(const SortBuffer: PKDT8DI64_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT8DI64_Source;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer^[(L + R) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, axis) < 0 do
            Inc(i);
        while SortCompare(SortBuffer^[j], p, axis) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, axis);
      L := i;
    until i >= R;
  end;

var
  M: NativeInt;
  axis: NativeInt;
  kdBuffPtr: PKDT8DI64_SourceBuffer;
begin
  Result := nil;
  if PlanCount = 0 then
      Exit;

  if PlanCount = 1 then
    begin
      new(Result);
      Result^.Parent := nil;
      Result^.Right := nil;
      Result^.Left := nil;
      Result^.vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);
    end
  else
    begin
      axis := Depth mod KDT8DI64_Axis;
      M := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr^[0], 0, PlanCount - 1, axis);

      new(Result);
      Result^.Parent := nil;
      Result^.vec := kdBuffPtr^[M];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], M, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[M + 1], PlanCount - (M + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDT8DI64.GetData(const Index: NativeInt): PKDT8DI64_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT8DI64.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT8DI64.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT8DI64.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT8DI64_Node(KDNodes[i]));
      Inc(i);
    end;

  for i := 0 to length(KDStoreBuff) - 1 do
      KDStoreBuff[i].Token := '';

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDT8DI64.StoreBuffPtr: PKDT8DI64_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT8DI64.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DI64_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT8DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT8DI64.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DI64_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT8DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT8DI64.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DI64_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT8DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT8DI64.BuildKDTreeWithCluster(const inBuff: TKDT8DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT8DI64_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT8DI64_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT8DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT8DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT8DI64.BuildKDTreeWithCluster(const inBuff: TKDT8DI64_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT8DI64.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DI64_BuildCall);
var
  TempStoreBuff: TKDT8DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT8DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT8DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT8DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT8DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT8DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT8DI64.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DI64_BuildMethod);
var
  TempStoreBuff: TKDT8DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT8DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT8DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT8DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT8DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT8DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$IFNDEF FPC}


procedure TKDT8DI64.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DI64_BuildProc);
var
  TempStoreBuff: TKDT8DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT8DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT8DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT8DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT8DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT8DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$ENDIF FPC}


function TKDT8DI64.Search(const buff: TKDT8DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT8DI64_Node;

var
  NearestNeighbour: PKDT8DI64_Node;

  function FindParentNode(const buffPtr: PKDT8DI64_Vec; NodePtr: PKDT8DI64_Node): PKDT8DI64_Node;
  var
    Next: PKDT8DI64_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT8DI64_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT8DI64_Node; const buffPtr: PKDT8DI64_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT8DI64Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT8DI64_Axis;
    Dist := NodePtr^.vec^.buff[axis] - buffPtr^[axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.vec^.buff[axis] > buffPtr^[axis] then
            ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes)
        else
            ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end
    else
      begin
        ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes);
        ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end;
  end;

  function SortCompare(const buffPtr: PKDT8DI64_Vec; const p1, p2: PKDT8DI64_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT8DI64Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT8DI64Distance(buffPtr^, p2^.vec^.buff);
    if d1 = d2 then
      begin
        if p1^.vec^.Index = p2^.vec^.Index then
            Result := 0
        else if p1^.vec^.Index < p2^.vec^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT8DI64_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT8DI64_Node;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer[(L + R) shr 1];
      repeat
        while SortCompare(buffPtr, SortBuffer[i], p) < 0 do
            Inc(i);
        while SortCompare(buffPtr, SortBuffer[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, buffPtr);
      L := i;
    until i >= R;
  end;

var
  Parent: PKDT8DI64_Node;
begin
  Result := nil;
  SearchedDistanceMin := 0;
  SearchedCounter := 0;
  NearestNeighbour := nil;
  if NearestNodes <> nil then
      NearestNodes.Clear;
  if RootNode = nil then
      Exit;
  if Count = 0 then
      Exit;

  Parent := FindParentNode(@buff[0], RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := KDT8DI64Distance(buff, Parent^.vec^.buff);

  ScanSubtree(RootNode, @buff[0], 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @buff[0]);

      if NearestNodes.Count > 0 then
          Result := PKDT8DI64_Node(NearestNodes[0]);
    end;
end;

function TKDT8DI64.Search(const buff: TKDT8DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT8DI64_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT8DI64.Search(const buff: TKDT8DI64_Vec; var SearchedDistanceMin: Double): PKDT8DI64_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT8DI64.Search(const buff: TKDT8DI64_Vec): PKDT8DI64_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT8DI64.SearchToken(const buff: TKDT8DI64_Vec): TPascalString;
var
  p: PKDT8DI64_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT8DI64.Search(const inBuff: TKDT8DI64_DynamicVecBuffer; var OutBuff: TKDT8DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT8DI64_DynamicVecBuffer;
  outBuffPtr: PKDT8DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT8DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outBuffPtr^[pass] := p^.vec^.buff;
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outBuffPtr := @OutBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT8DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outBuffPtr^[pass] := p^.vec^.buff;
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT8DI64_Node;
begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutBuff[i] := p^.vec^.buff;
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT8DI64.Search(const inBuff: TKDT8DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT8DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT8DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT8DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT8DI64_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT8DI64.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT8DI64_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT8DI64_Vec));
      stream.write(KDStoreBuff[i].Index, 8);
      token_B := KDStoreBuff[i].Token.Bytes;
      token_L := length(token_B);
      stream.write(token_L, 4);
      if token_L > 0 then
        begin
          stream.write(token_B[0], token_L);
          SetLength(token_B, 0);
        end;
      Inc(i);
    end;
end;

procedure TKDT8DI64.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  Clear;

  stream.read(st, 4);
  stream.read(ID, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if ID <> KDT8DI64_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT8DI64_Vec)) <> SizeOf(TKDT8DI64_Vec) then
          begin
            Clear;
            Exit;
          end;
        if stream.read(KDStoreBuff[i].Index, 8) <> 8 then
          begin
            Clear;
            Exit;
          end;
        if stream.read(token_L, 4) <> 4 then
          begin
            Clear;
            Exit;
          end;
        if token_L > 0 then
          begin
            SetLength(token_B, token_L);
            if stream.read(token_B[0], token_L) <> token_L then
              begin
                Clear;
                Exit;
              end;
            KDStoreBuff[i].Token.Bytes := token_B;
            SetLength(token_B, 0);
          end
        else
            KDStoreBuff[i].Token := '';
        Inc(i);
      end;
  except
    Clear;
    Exit;
  end;

  SetLength(KDBuff, cnt);
  SetLength(KDNodes, cnt);

  i := 0;
  while i < cnt do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      Inc(i);
    end;

  if cnt > 0 then
    RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDT8DI64.SaveToFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT8DI64.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT8DI64.PrintNodeTree(const NodePtr: PKDT8DI64_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT8DI64_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT8DI64Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT8DI64.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT8DI64Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT8DI64.KDT8DI64Vec(const s: SystemString): TKDT8DI64_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT8DI64_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToInt(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT8DI64_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT8DI64.KDT8DI64Vec(const v: TKDT8DI64_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT8DI64_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT8DI64.KDT8DI64Pow(const v: TKDT8DI64_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT8DI64.KDT8DI64Distance(const v1, v2: TKDT8DI64_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT8DI64_Axis - 1 do
      Result := Result + KDT8DI64Pow(v2[i] - v1[i]);
end;

procedure TKDT8DI64.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT8DI64_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT8DI64.Test;
var
  TKDT8DI64_Test: TKDT8DI64;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT8DI64_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT8DI64_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT8DI64_Test := TKDT8DI64.Create;

  DoStatusNoLn('...');
  SetLength(TKDT8DI64_Test.TestBuff, 1000);
  for i := 0 to length(TKDT8DI64_Test.TestBuff) - 1 do
    for j := 0 to KDT8DI64_Axis - 1 do
        TKDT8DI64_Test.TestBuff[i][j] := i * KDT8DI64_Axis + j;

{$IFDEF FPC}
  TKDT8DI64_Test.BuildKDTreeM(length(TKDT8DI64_Test.TestBuff), nil, @TKDT8DI64_Test.Test_BuildM);
{$ELSE FPC}
  TKDT8DI64_Test.BuildKDTreeM(length(TKDT8DI64_Test.TestBuff), nil, TKDT8DI64_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT8DI64_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT8DI64_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT8DI64_Test.TestBuff) - 1 do
    begin
      p := TKDT8DI64_Test.Search(TKDT8DI64_Test.TestBuff[i]);
      if p^.vec^.Index <> i then
          errored := True;
      if not p^.vec^.Token.Same(umlIntToStr(i)) then
          errored := True;
      if errored then
          Break;
    end;
  DisposeObject(m64);

  if not errored then
    begin
      { parallel search test }
      DoStatusNoLn('...parallel');
      SetLength(TestResultBuff, length(TKDT8DI64_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT8DI64_Test.TestBuff));
      TKDT8DI64_Test.Search(TKDT8DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT8DI64Distance(TKDT8DI64_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT8DI64_Test.Clear;
      { kMean test }
      TKDT8DI64_Test.BuildKDTreeWithCluster(TKDT8DI64_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT8DI64_Test.Search(TKDT8DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT8DI64_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT8DI64_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT8DI64_Test);
end;


function TKDT9DI64.InternalBuildKdTree(const KDSourceBufferPtr: PKDT9DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT9DI64_Node;
  function SortCompare(const p1, p2: PKDT9DI64_Source; const axis: NativeInt): ShortInt;
  begin
    if p1^.buff[axis] = p2^.buff[axis] then
      begin
        if p1^.Index = p2^.Index then
            Result := 0
        else if p1^.Index < p2^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.buff[axis] < p2^.buff[axis] then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(const SortBuffer: PKDT9DI64_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT9DI64_Source;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer^[(L + R) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, axis) < 0 do
            Inc(i);
        while SortCompare(SortBuffer^[j], p, axis) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, axis);
      L := i;
    until i >= R;
  end;

var
  M: NativeInt;
  axis: NativeInt;
  kdBuffPtr: PKDT9DI64_SourceBuffer;
begin
  Result := nil;
  if PlanCount = 0 then
      Exit;

  if PlanCount = 1 then
    begin
      new(Result);
      Result^.Parent := nil;
      Result^.Right := nil;
      Result^.Left := nil;
      Result^.vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);
    end
  else
    begin
      axis := Depth mod KDT9DI64_Axis;
      M := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr^[0], 0, PlanCount - 1, axis);

      new(Result);
      Result^.Parent := nil;
      Result^.vec := kdBuffPtr^[M];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], M, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[M + 1], PlanCount - (M + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDT9DI64.GetData(const Index: NativeInt): PKDT9DI64_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT9DI64.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT9DI64.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT9DI64.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT9DI64_Node(KDNodes[i]));
      Inc(i);
    end;

  for i := 0 to length(KDStoreBuff) - 1 do
      KDStoreBuff[i].Token := '';

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDT9DI64.StoreBuffPtr: PKDT9DI64_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT9DI64.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DI64_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT9DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT9DI64.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DI64_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT9DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT9DI64.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DI64_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT9DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT9DI64.BuildKDTreeWithCluster(const inBuff: TKDT9DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT9DI64_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT9DI64_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT9DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT9DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT9DI64.BuildKDTreeWithCluster(const inBuff: TKDT9DI64_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT9DI64.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DI64_BuildCall);
var
  TempStoreBuff: TKDT9DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT9DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT9DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT9DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT9DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT9DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT9DI64.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DI64_BuildMethod);
var
  TempStoreBuff: TKDT9DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT9DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT9DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT9DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT9DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT9DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$IFNDEF FPC}


procedure TKDT9DI64.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DI64_BuildProc);
var
  TempStoreBuff: TKDT9DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT9DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT9DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT9DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT9DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT9DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$ENDIF FPC}


function TKDT9DI64.Search(const buff: TKDT9DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT9DI64_Node;

var
  NearestNeighbour: PKDT9DI64_Node;

  function FindParentNode(const buffPtr: PKDT9DI64_Vec; NodePtr: PKDT9DI64_Node): PKDT9DI64_Node;
  var
    Next: PKDT9DI64_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT9DI64_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT9DI64_Node; const buffPtr: PKDT9DI64_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT9DI64Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT9DI64_Axis;
    Dist := NodePtr^.vec^.buff[axis] - buffPtr^[axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.vec^.buff[axis] > buffPtr^[axis] then
            ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes)
        else
            ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end
    else
      begin
        ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes);
        ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end;
  end;

  function SortCompare(const buffPtr: PKDT9DI64_Vec; const p1, p2: PKDT9DI64_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT9DI64Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT9DI64Distance(buffPtr^, p2^.vec^.buff);
    if d1 = d2 then
      begin
        if p1^.vec^.Index = p2^.vec^.Index then
            Result := 0
        else if p1^.vec^.Index < p2^.vec^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT9DI64_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT9DI64_Node;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer[(L + R) shr 1];
      repeat
        while SortCompare(buffPtr, SortBuffer[i], p) < 0 do
            Inc(i);
        while SortCompare(buffPtr, SortBuffer[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, buffPtr);
      L := i;
    until i >= R;
  end;

var
  Parent: PKDT9DI64_Node;
begin
  Result := nil;
  SearchedDistanceMin := 0;
  SearchedCounter := 0;
  NearestNeighbour := nil;
  if NearestNodes <> nil then
      NearestNodes.Clear;
  if RootNode = nil then
      Exit;
  if Count = 0 then
      Exit;

  Parent := FindParentNode(@buff[0], RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := KDT9DI64Distance(buff, Parent^.vec^.buff);

  ScanSubtree(RootNode, @buff[0], 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @buff[0]);

      if NearestNodes.Count > 0 then
          Result := PKDT9DI64_Node(NearestNodes[0]);
    end;
end;

function TKDT9DI64.Search(const buff: TKDT9DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT9DI64_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT9DI64.Search(const buff: TKDT9DI64_Vec; var SearchedDistanceMin: Double): PKDT9DI64_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT9DI64.Search(const buff: TKDT9DI64_Vec): PKDT9DI64_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT9DI64.SearchToken(const buff: TKDT9DI64_Vec): TPascalString;
var
  p: PKDT9DI64_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT9DI64.Search(const inBuff: TKDT9DI64_DynamicVecBuffer; var OutBuff: TKDT9DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT9DI64_DynamicVecBuffer;
  outBuffPtr: PKDT9DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT9DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outBuffPtr^[pass] := p^.vec^.buff;
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outBuffPtr := @OutBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT9DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outBuffPtr^[pass] := p^.vec^.buff;
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT9DI64_Node;
begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutBuff[i] := p^.vec^.buff;
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT9DI64.Search(const inBuff: TKDT9DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT9DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT9DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT9DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT9DI64_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT9DI64.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT9DI64_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT9DI64_Vec));
      stream.write(KDStoreBuff[i].Index, 8);
      token_B := KDStoreBuff[i].Token.Bytes;
      token_L := length(token_B);
      stream.write(token_L, 4);
      if token_L > 0 then
        begin
          stream.write(token_B[0], token_L);
          SetLength(token_B, 0);
        end;
      Inc(i);
    end;
end;

procedure TKDT9DI64.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  Clear;

  stream.read(st, 4);
  stream.read(ID, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if ID <> KDT9DI64_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT9DI64_Vec)) <> SizeOf(TKDT9DI64_Vec) then
          begin
            Clear;
            Exit;
          end;
        if stream.read(KDStoreBuff[i].Index, 8) <> 8 then
          begin
            Clear;
            Exit;
          end;
        if stream.read(token_L, 4) <> 4 then
          begin
            Clear;
            Exit;
          end;
        if token_L > 0 then
          begin
            SetLength(token_B, token_L);
            if stream.read(token_B[0], token_L) <> token_L then
              begin
                Clear;
                Exit;
              end;
            KDStoreBuff[i].Token.Bytes := token_B;
            SetLength(token_B, 0);
          end
        else
            KDStoreBuff[i].Token := '';
        Inc(i);
      end;
  except
    Clear;
    Exit;
  end;

  SetLength(KDBuff, cnt);
  SetLength(KDNodes, cnt);

  i := 0;
  while i < cnt do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      Inc(i);
    end;

  if cnt > 0 then
    RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDT9DI64.SaveToFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT9DI64.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT9DI64.PrintNodeTree(const NodePtr: PKDT9DI64_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT9DI64_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT9DI64Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT9DI64.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT9DI64Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT9DI64.KDT9DI64Vec(const s: SystemString): TKDT9DI64_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT9DI64_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToInt(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT9DI64_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT9DI64.KDT9DI64Vec(const v: TKDT9DI64_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT9DI64_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT9DI64.KDT9DI64Pow(const v: TKDT9DI64_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT9DI64.KDT9DI64Distance(const v1, v2: TKDT9DI64_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT9DI64_Axis - 1 do
      Result := Result + KDT9DI64Pow(v2[i] - v1[i]);
end;

procedure TKDT9DI64.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT9DI64_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT9DI64.Test;
var
  TKDT9DI64_Test: TKDT9DI64;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT9DI64_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT9DI64_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT9DI64_Test := TKDT9DI64.Create;

  DoStatusNoLn('...');
  SetLength(TKDT9DI64_Test.TestBuff, 1000);
  for i := 0 to length(TKDT9DI64_Test.TestBuff) - 1 do
    for j := 0 to KDT9DI64_Axis - 1 do
        TKDT9DI64_Test.TestBuff[i][j] := i * KDT9DI64_Axis + j;

{$IFDEF FPC}
  TKDT9DI64_Test.BuildKDTreeM(length(TKDT9DI64_Test.TestBuff), nil, @TKDT9DI64_Test.Test_BuildM);
{$ELSE FPC}
  TKDT9DI64_Test.BuildKDTreeM(length(TKDT9DI64_Test.TestBuff), nil, TKDT9DI64_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT9DI64_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT9DI64_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT9DI64_Test.TestBuff) - 1 do
    begin
      p := TKDT9DI64_Test.Search(TKDT9DI64_Test.TestBuff[i]);
      if p^.vec^.Index <> i then
          errored := True;
      if not p^.vec^.Token.Same(umlIntToStr(i)) then
          errored := True;
      if errored then
          Break;
    end;
  DisposeObject(m64);

  if not errored then
    begin
      { parallel search test }
      DoStatusNoLn('...parallel');
      SetLength(TestResultBuff, length(TKDT9DI64_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT9DI64_Test.TestBuff));
      TKDT9DI64_Test.Search(TKDT9DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT9DI64Distance(TKDT9DI64_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT9DI64_Test.Clear;
      { kMean test }
      TKDT9DI64_Test.BuildKDTreeWithCluster(TKDT9DI64_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT9DI64_Test.Search(TKDT9DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT9DI64_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT9DI64_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT9DI64_Test);
end;


function TKDT10DI64.InternalBuildKdTree(const KDSourceBufferPtr: PKDT10DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT10DI64_Node;
  function SortCompare(const p1, p2: PKDT10DI64_Source; const axis: NativeInt): ShortInt;
  begin
    if p1^.buff[axis] = p2^.buff[axis] then
      begin
        if p1^.Index = p2^.Index then
            Result := 0
        else if p1^.Index < p2^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.buff[axis] < p2^.buff[axis] then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(const SortBuffer: PKDT10DI64_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT10DI64_Source;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer^[(L + R) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, axis) < 0 do
            Inc(i);
        while SortCompare(SortBuffer^[j], p, axis) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, axis);
      L := i;
    until i >= R;
  end;

var
  M: NativeInt;
  axis: NativeInt;
  kdBuffPtr: PKDT10DI64_SourceBuffer;
begin
  Result := nil;
  if PlanCount = 0 then
      Exit;

  if PlanCount = 1 then
    begin
      new(Result);
      Result^.Parent := nil;
      Result^.Right := nil;
      Result^.Left := nil;
      Result^.vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);
    end
  else
    begin
      axis := Depth mod KDT10DI64_Axis;
      M := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr^[0], 0, PlanCount - 1, axis);

      new(Result);
      Result^.Parent := nil;
      Result^.vec := kdBuffPtr^[M];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], M, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[M + 1], PlanCount - (M + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDT10DI64.GetData(const Index: NativeInt): PKDT10DI64_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT10DI64.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT10DI64.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT10DI64.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT10DI64_Node(KDNodes[i]));
      Inc(i);
    end;

  for i := 0 to length(KDStoreBuff) - 1 do
      KDStoreBuff[i].Token := '';

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDT10DI64.StoreBuffPtr: PKDT10DI64_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT10DI64.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DI64_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT10DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT10DI64.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DI64_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT10DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT10DI64.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DI64_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT10DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT10DI64.BuildKDTreeWithCluster(const inBuff: TKDT10DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT10DI64_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT10DI64_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT10DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT10DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT10DI64.BuildKDTreeWithCluster(const inBuff: TKDT10DI64_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT10DI64.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DI64_BuildCall);
var
  TempStoreBuff: TKDT10DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT10DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT10DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT10DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT10DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT10DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT10DI64.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DI64_BuildMethod);
var
  TempStoreBuff: TKDT10DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT10DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT10DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT10DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT10DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT10DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$IFNDEF FPC}


procedure TKDT10DI64.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DI64_BuildProc);
var
  TempStoreBuff: TKDT10DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT10DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT10DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT10DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT10DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT10DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$ENDIF FPC}


function TKDT10DI64.Search(const buff: TKDT10DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT10DI64_Node;

var
  NearestNeighbour: PKDT10DI64_Node;

  function FindParentNode(const buffPtr: PKDT10DI64_Vec; NodePtr: PKDT10DI64_Node): PKDT10DI64_Node;
  var
    Next: PKDT10DI64_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT10DI64_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT10DI64_Node; const buffPtr: PKDT10DI64_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT10DI64Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT10DI64_Axis;
    Dist := NodePtr^.vec^.buff[axis] - buffPtr^[axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.vec^.buff[axis] > buffPtr^[axis] then
            ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes)
        else
            ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end
    else
      begin
        ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes);
        ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end;
  end;

  function SortCompare(const buffPtr: PKDT10DI64_Vec; const p1, p2: PKDT10DI64_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT10DI64Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT10DI64Distance(buffPtr^, p2^.vec^.buff);
    if d1 = d2 then
      begin
        if p1^.vec^.Index = p2^.vec^.Index then
            Result := 0
        else if p1^.vec^.Index < p2^.vec^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT10DI64_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT10DI64_Node;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer[(L + R) shr 1];
      repeat
        while SortCompare(buffPtr, SortBuffer[i], p) < 0 do
            Inc(i);
        while SortCompare(buffPtr, SortBuffer[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, buffPtr);
      L := i;
    until i >= R;
  end;

var
  Parent: PKDT10DI64_Node;
begin
  Result := nil;
  SearchedDistanceMin := 0;
  SearchedCounter := 0;
  NearestNeighbour := nil;
  if NearestNodes <> nil then
      NearestNodes.Clear;
  if RootNode = nil then
      Exit;
  if Count = 0 then
      Exit;

  Parent := FindParentNode(@buff[0], RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := KDT10DI64Distance(buff, Parent^.vec^.buff);

  ScanSubtree(RootNode, @buff[0], 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @buff[0]);

      if NearestNodes.Count > 0 then
          Result := PKDT10DI64_Node(NearestNodes[0]);
    end;
end;

function TKDT10DI64.Search(const buff: TKDT10DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT10DI64_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT10DI64.Search(const buff: TKDT10DI64_Vec; var SearchedDistanceMin: Double): PKDT10DI64_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT10DI64.Search(const buff: TKDT10DI64_Vec): PKDT10DI64_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT10DI64.SearchToken(const buff: TKDT10DI64_Vec): TPascalString;
var
  p: PKDT10DI64_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT10DI64.Search(const inBuff: TKDT10DI64_DynamicVecBuffer; var OutBuff: TKDT10DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT10DI64_DynamicVecBuffer;
  outBuffPtr: PKDT10DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT10DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outBuffPtr^[pass] := p^.vec^.buff;
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outBuffPtr := @OutBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT10DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outBuffPtr^[pass] := p^.vec^.buff;
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT10DI64_Node;
begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutBuff[i] := p^.vec^.buff;
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT10DI64.Search(const inBuff: TKDT10DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT10DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT10DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT10DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT10DI64_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT10DI64.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT10DI64_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT10DI64_Vec));
      stream.write(KDStoreBuff[i].Index, 8);
      token_B := KDStoreBuff[i].Token.Bytes;
      token_L := length(token_B);
      stream.write(token_L, 4);
      if token_L > 0 then
        begin
          stream.write(token_B[0], token_L);
          SetLength(token_B, 0);
        end;
      Inc(i);
    end;
end;

procedure TKDT10DI64.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  Clear;

  stream.read(st, 4);
  stream.read(ID, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if ID <> KDT10DI64_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT10DI64_Vec)) <> SizeOf(TKDT10DI64_Vec) then
          begin
            Clear;
            Exit;
          end;
        if stream.read(KDStoreBuff[i].Index, 8) <> 8 then
          begin
            Clear;
            Exit;
          end;
        if stream.read(token_L, 4) <> 4 then
          begin
            Clear;
            Exit;
          end;
        if token_L > 0 then
          begin
            SetLength(token_B, token_L);
            if stream.read(token_B[0], token_L) <> token_L then
              begin
                Clear;
                Exit;
              end;
            KDStoreBuff[i].Token.Bytes := token_B;
            SetLength(token_B, 0);
          end
        else
            KDStoreBuff[i].Token := '';
        Inc(i);
      end;
  except
    Clear;
    Exit;
  end;

  SetLength(KDBuff, cnt);
  SetLength(KDNodes, cnt);

  i := 0;
  while i < cnt do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      Inc(i);
    end;

  if cnt > 0 then
    RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDT10DI64.SaveToFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT10DI64.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT10DI64.PrintNodeTree(const NodePtr: PKDT10DI64_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT10DI64_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT10DI64Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT10DI64.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT10DI64Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT10DI64.KDT10DI64Vec(const s: SystemString): TKDT10DI64_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT10DI64_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToInt(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT10DI64_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT10DI64.KDT10DI64Vec(const v: TKDT10DI64_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT10DI64_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT10DI64.KDT10DI64Pow(const v: TKDT10DI64_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT10DI64.KDT10DI64Distance(const v1, v2: TKDT10DI64_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT10DI64_Axis - 1 do
      Result := Result + KDT10DI64Pow(v2[i] - v1[i]);
end;

procedure TKDT10DI64.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT10DI64_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT10DI64.Test;
var
  TKDT10DI64_Test: TKDT10DI64;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT10DI64_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT10DI64_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT10DI64_Test := TKDT10DI64.Create;

  DoStatusNoLn('...');
  SetLength(TKDT10DI64_Test.TestBuff, 1000);
  for i := 0 to length(TKDT10DI64_Test.TestBuff) - 1 do
    for j := 0 to KDT10DI64_Axis - 1 do
        TKDT10DI64_Test.TestBuff[i][j] := i * KDT10DI64_Axis + j;

{$IFDEF FPC}
  TKDT10DI64_Test.BuildKDTreeM(length(TKDT10DI64_Test.TestBuff), nil, @TKDT10DI64_Test.Test_BuildM);
{$ELSE FPC}
  TKDT10DI64_Test.BuildKDTreeM(length(TKDT10DI64_Test.TestBuff), nil, TKDT10DI64_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT10DI64_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT10DI64_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT10DI64_Test.TestBuff) - 1 do
    begin
      p := TKDT10DI64_Test.Search(TKDT10DI64_Test.TestBuff[i]);
      if p^.vec^.Index <> i then
          errored := True;
      if not p^.vec^.Token.Same(umlIntToStr(i)) then
          errored := True;
      if errored then
          Break;
    end;
  DisposeObject(m64);

  if not errored then
    begin
      { parallel search test }
      DoStatusNoLn('...parallel');
      SetLength(TestResultBuff, length(TKDT10DI64_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT10DI64_Test.TestBuff));
      TKDT10DI64_Test.Search(TKDT10DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT10DI64Distance(TKDT10DI64_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT10DI64_Test.Clear;
      { kMean test }
      TKDT10DI64_Test.BuildKDTreeWithCluster(TKDT10DI64_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT10DI64_Test.Search(TKDT10DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT10DI64_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT10DI64_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT10DI64_Test);
end;


function TKDT11DI64.InternalBuildKdTree(const KDSourceBufferPtr: PKDT11DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT11DI64_Node;
  function SortCompare(const p1, p2: PKDT11DI64_Source; const axis: NativeInt): ShortInt;
  begin
    if p1^.buff[axis] = p2^.buff[axis] then
      begin
        if p1^.Index = p2^.Index then
            Result := 0
        else if p1^.Index < p2^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.buff[axis] < p2^.buff[axis] then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(const SortBuffer: PKDT11DI64_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT11DI64_Source;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer^[(L + R) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, axis) < 0 do
            Inc(i);
        while SortCompare(SortBuffer^[j], p, axis) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, axis);
      L := i;
    until i >= R;
  end;

var
  M: NativeInt;
  axis: NativeInt;
  kdBuffPtr: PKDT11DI64_SourceBuffer;
begin
  Result := nil;
  if PlanCount = 0 then
      Exit;

  if PlanCount = 1 then
    begin
      new(Result);
      Result^.Parent := nil;
      Result^.Right := nil;
      Result^.Left := nil;
      Result^.vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);
    end
  else
    begin
      axis := Depth mod KDT11DI64_Axis;
      M := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr^[0], 0, PlanCount - 1, axis);

      new(Result);
      Result^.Parent := nil;
      Result^.vec := kdBuffPtr^[M];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], M, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[M + 1], PlanCount - (M + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDT11DI64.GetData(const Index: NativeInt): PKDT11DI64_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT11DI64.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT11DI64.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT11DI64.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT11DI64_Node(KDNodes[i]));
      Inc(i);
    end;

  for i := 0 to length(KDStoreBuff) - 1 do
      KDStoreBuff[i].Token := '';

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDT11DI64.StoreBuffPtr: PKDT11DI64_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT11DI64.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DI64_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT11DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT11DI64.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DI64_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT11DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT11DI64.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DI64_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT11DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT11DI64.BuildKDTreeWithCluster(const inBuff: TKDT11DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT11DI64_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT11DI64_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT11DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT11DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT11DI64.BuildKDTreeWithCluster(const inBuff: TKDT11DI64_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT11DI64.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DI64_BuildCall);
var
  TempStoreBuff: TKDT11DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT11DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT11DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT11DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT11DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT11DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT11DI64.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DI64_BuildMethod);
var
  TempStoreBuff: TKDT11DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT11DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT11DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT11DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT11DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT11DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$IFNDEF FPC}


procedure TKDT11DI64.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DI64_BuildProc);
var
  TempStoreBuff: TKDT11DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT11DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT11DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT11DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT11DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT11DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$ENDIF FPC}


function TKDT11DI64.Search(const buff: TKDT11DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT11DI64_Node;

var
  NearestNeighbour: PKDT11DI64_Node;

  function FindParentNode(const buffPtr: PKDT11DI64_Vec; NodePtr: PKDT11DI64_Node): PKDT11DI64_Node;
  var
    Next: PKDT11DI64_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT11DI64_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT11DI64_Node; const buffPtr: PKDT11DI64_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT11DI64Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT11DI64_Axis;
    Dist := NodePtr^.vec^.buff[axis] - buffPtr^[axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.vec^.buff[axis] > buffPtr^[axis] then
            ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes)
        else
            ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end
    else
      begin
        ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes);
        ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end;
  end;

  function SortCompare(const buffPtr: PKDT11DI64_Vec; const p1, p2: PKDT11DI64_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT11DI64Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT11DI64Distance(buffPtr^, p2^.vec^.buff);
    if d1 = d2 then
      begin
        if p1^.vec^.Index = p2^.vec^.Index then
            Result := 0
        else if p1^.vec^.Index < p2^.vec^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT11DI64_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT11DI64_Node;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer[(L + R) shr 1];
      repeat
        while SortCompare(buffPtr, SortBuffer[i], p) < 0 do
            Inc(i);
        while SortCompare(buffPtr, SortBuffer[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, buffPtr);
      L := i;
    until i >= R;
  end;

var
  Parent: PKDT11DI64_Node;
begin
  Result := nil;
  SearchedDistanceMin := 0;
  SearchedCounter := 0;
  NearestNeighbour := nil;
  if NearestNodes <> nil then
      NearestNodes.Clear;
  if RootNode = nil then
      Exit;
  if Count = 0 then
      Exit;

  Parent := FindParentNode(@buff[0], RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := KDT11DI64Distance(buff, Parent^.vec^.buff);

  ScanSubtree(RootNode, @buff[0], 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @buff[0]);

      if NearestNodes.Count > 0 then
          Result := PKDT11DI64_Node(NearestNodes[0]);
    end;
end;

function TKDT11DI64.Search(const buff: TKDT11DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT11DI64_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT11DI64.Search(const buff: TKDT11DI64_Vec; var SearchedDistanceMin: Double): PKDT11DI64_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT11DI64.Search(const buff: TKDT11DI64_Vec): PKDT11DI64_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT11DI64.SearchToken(const buff: TKDT11DI64_Vec): TPascalString;
var
  p: PKDT11DI64_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT11DI64.Search(const inBuff: TKDT11DI64_DynamicVecBuffer; var OutBuff: TKDT11DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT11DI64_DynamicVecBuffer;
  outBuffPtr: PKDT11DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT11DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outBuffPtr^[pass] := p^.vec^.buff;
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outBuffPtr := @OutBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT11DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outBuffPtr^[pass] := p^.vec^.buff;
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT11DI64_Node;
begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutBuff[i] := p^.vec^.buff;
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT11DI64.Search(const inBuff: TKDT11DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT11DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT11DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT11DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT11DI64_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT11DI64.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT11DI64_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT11DI64_Vec));
      stream.write(KDStoreBuff[i].Index, 8);
      token_B := KDStoreBuff[i].Token.Bytes;
      token_L := length(token_B);
      stream.write(token_L, 4);
      if token_L > 0 then
        begin
          stream.write(token_B[0], token_L);
          SetLength(token_B, 0);
        end;
      Inc(i);
    end;
end;

procedure TKDT11DI64.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  Clear;

  stream.read(st, 4);
  stream.read(ID, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if ID <> KDT11DI64_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT11DI64_Vec)) <> SizeOf(TKDT11DI64_Vec) then
          begin
            Clear;
            Exit;
          end;
        if stream.read(KDStoreBuff[i].Index, 8) <> 8 then
          begin
            Clear;
            Exit;
          end;
        if stream.read(token_L, 4) <> 4 then
          begin
            Clear;
            Exit;
          end;
        if token_L > 0 then
          begin
            SetLength(token_B, token_L);
            if stream.read(token_B[0], token_L) <> token_L then
              begin
                Clear;
                Exit;
              end;
            KDStoreBuff[i].Token.Bytes := token_B;
            SetLength(token_B, 0);
          end
        else
            KDStoreBuff[i].Token := '';
        Inc(i);
      end;
  except
    Clear;
    Exit;
  end;

  SetLength(KDBuff, cnt);
  SetLength(KDNodes, cnt);

  i := 0;
  while i < cnt do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      Inc(i);
    end;

  if cnt > 0 then
    RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDT11DI64.SaveToFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT11DI64.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT11DI64.PrintNodeTree(const NodePtr: PKDT11DI64_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT11DI64_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT11DI64Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT11DI64.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT11DI64Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT11DI64.KDT11DI64Vec(const s: SystemString): TKDT11DI64_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT11DI64_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToInt(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT11DI64_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT11DI64.KDT11DI64Vec(const v: TKDT11DI64_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT11DI64_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT11DI64.KDT11DI64Pow(const v: TKDT11DI64_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT11DI64.KDT11DI64Distance(const v1, v2: TKDT11DI64_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT11DI64_Axis - 1 do
      Result := Result + KDT11DI64Pow(v2[i] - v1[i]);
end;

procedure TKDT11DI64.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT11DI64_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT11DI64.Test;
var
  TKDT11DI64_Test: TKDT11DI64;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT11DI64_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT11DI64_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT11DI64_Test := TKDT11DI64.Create;

  DoStatusNoLn('...');
  SetLength(TKDT11DI64_Test.TestBuff, 1000);
  for i := 0 to length(TKDT11DI64_Test.TestBuff) - 1 do
    for j := 0 to KDT11DI64_Axis - 1 do
        TKDT11DI64_Test.TestBuff[i][j] := i * KDT11DI64_Axis + j;

{$IFDEF FPC}
  TKDT11DI64_Test.BuildKDTreeM(length(TKDT11DI64_Test.TestBuff), nil, @TKDT11DI64_Test.Test_BuildM);
{$ELSE FPC}
  TKDT11DI64_Test.BuildKDTreeM(length(TKDT11DI64_Test.TestBuff), nil, TKDT11DI64_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT11DI64_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT11DI64_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT11DI64_Test.TestBuff) - 1 do
    begin
      p := TKDT11DI64_Test.Search(TKDT11DI64_Test.TestBuff[i]);
      if p^.vec^.Index <> i then
          errored := True;
      if not p^.vec^.Token.Same(umlIntToStr(i)) then
          errored := True;
      if errored then
          Break;
    end;
  DisposeObject(m64);

  if not errored then
    begin
      { parallel search test }
      DoStatusNoLn('...parallel');
      SetLength(TestResultBuff, length(TKDT11DI64_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT11DI64_Test.TestBuff));
      TKDT11DI64_Test.Search(TKDT11DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT11DI64Distance(TKDT11DI64_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT11DI64_Test.Clear;
      { kMean test }
      TKDT11DI64_Test.BuildKDTreeWithCluster(TKDT11DI64_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT11DI64_Test.Search(TKDT11DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT11DI64_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT11DI64_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT11DI64_Test);
end;


function TKDT12DI64.InternalBuildKdTree(const KDSourceBufferPtr: PKDT12DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT12DI64_Node;
  function SortCompare(const p1, p2: PKDT12DI64_Source; const axis: NativeInt): ShortInt;
  begin
    if p1^.buff[axis] = p2^.buff[axis] then
      begin
        if p1^.Index = p2^.Index then
            Result := 0
        else if p1^.Index < p2^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.buff[axis] < p2^.buff[axis] then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(const SortBuffer: PKDT12DI64_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT12DI64_Source;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer^[(L + R) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, axis) < 0 do
            Inc(i);
        while SortCompare(SortBuffer^[j], p, axis) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, axis);
      L := i;
    until i >= R;
  end;

var
  M: NativeInt;
  axis: NativeInt;
  kdBuffPtr: PKDT12DI64_SourceBuffer;
begin
  Result := nil;
  if PlanCount = 0 then
      Exit;

  if PlanCount = 1 then
    begin
      new(Result);
      Result^.Parent := nil;
      Result^.Right := nil;
      Result^.Left := nil;
      Result^.vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);
    end
  else
    begin
      axis := Depth mod KDT12DI64_Axis;
      M := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr^[0], 0, PlanCount - 1, axis);

      new(Result);
      Result^.Parent := nil;
      Result^.vec := kdBuffPtr^[M];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], M, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[M + 1], PlanCount - (M + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDT12DI64.GetData(const Index: NativeInt): PKDT12DI64_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT12DI64.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT12DI64.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT12DI64.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT12DI64_Node(KDNodes[i]));
      Inc(i);
    end;

  for i := 0 to length(KDStoreBuff) - 1 do
      KDStoreBuff[i].Token := '';

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDT12DI64.StoreBuffPtr: PKDT12DI64_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT12DI64.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DI64_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT12DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT12DI64.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DI64_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT12DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT12DI64.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DI64_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT12DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT12DI64.BuildKDTreeWithCluster(const inBuff: TKDT12DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT12DI64_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT12DI64_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT12DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT12DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT12DI64.BuildKDTreeWithCluster(const inBuff: TKDT12DI64_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT12DI64.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DI64_BuildCall);
var
  TempStoreBuff: TKDT12DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT12DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT12DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT12DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT12DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT12DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT12DI64.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DI64_BuildMethod);
var
  TempStoreBuff: TKDT12DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT12DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT12DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT12DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT12DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT12DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$IFNDEF FPC}


procedure TKDT12DI64.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DI64_BuildProc);
var
  TempStoreBuff: TKDT12DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT12DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT12DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT12DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT12DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT12DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$ENDIF FPC}


function TKDT12DI64.Search(const buff: TKDT12DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT12DI64_Node;

var
  NearestNeighbour: PKDT12DI64_Node;

  function FindParentNode(const buffPtr: PKDT12DI64_Vec; NodePtr: PKDT12DI64_Node): PKDT12DI64_Node;
  var
    Next: PKDT12DI64_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT12DI64_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT12DI64_Node; const buffPtr: PKDT12DI64_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT12DI64Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT12DI64_Axis;
    Dist := NodePtr^.vec^.buff[axis] - buffPtr^[axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.vec^.buff[axis] > buffPtr^[axis] then
            ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes)
        else
            ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end
    else
      begin
        ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes);
        ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end;
  end;

  function SortCompare(const buffPtr: PKDT12DI64_Vec; const p1, p2: PKDT12DI64_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT12DI64Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT12DI64Distance(buffPtr^, p2^.vec^.buff);
    if d1 = d2 then
      begin
        if p1^.vec^.Index = p2^.vec^.Index then
            Result := 0
        else if p1^.vec^.Index < p2^.vec^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT12DI64_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT12DI64_Node;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer[(L + R) shr 1];
      repeat
        while SortCompare(buffPtr, SortBuffer[i], p) < 0 do
            Inc(i);
        while SortCompare(buffPtr, SortBuffer[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, buffPtr);
      L := i;
    until i >= R;
  end;

var
  Parent: PKDT12DI64_Node;
begin
  Result := nil;
  SearchedDistanceMin := 0;
  SearchedCounter := 0;
  NearestNeighbour := nil;
  if NearestNodes <> nil then
      NearestNodes.Clear;
  if RootNode = nil then
      Exit;
  if Count = 0 then
      Exit;

  Parent := FindParentNode(@buff[0], RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := KDT12DI64Distance(buff, Parent^.vec^.buff);

  ScanSubtree(RootNode, @buff[0], 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @buff[0]);

      if NearestNodes.Count > 0 then
          Result := PKDT12DI64_Node(NearestNodes[0]);
    end;
end;

function TKDT12DI64.Search(const buff: TKDT12DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT12DI64_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT12DI64.Search(const buff: TKDT12DI64_Vec; var SearchedDistanceMin: Double): PKDT12DI64_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT12DI64.Search(const buff: TKDT12DI64_Vec): PKDT12DI64_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT12DI64.SearchToken(const buff: TKDT12DI64_Vec): TPascalString;
var
  p: PKDT12DI64_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT12DI64.Search(const inBuff: TKDT12DI64_DynamicVecBuffer; var OutBuff: TKDT12DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT12DI64_DynamicVecBuffer;
  outBuffPtr: PKDT12DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT12DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outBuffPtr^[pass] := p^.vec^.buff;
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outBuffPtr := @OutBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT12DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outBuffPtr^[pass] := p^.vec^.buff;
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT12DI64_Node;
begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutBuff[i] := p^.vec^.buff;
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT12DI64.Search(const inBuff: TKDT12DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT12DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT12DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT12DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT12DI64_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT12DI64.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT12DI64_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT12DI64_Vec));
      stream.write(KDStoreBuff[i].Index, 8);
      token_B := KDStoreBuff[i].Token.Bytes;
      token_L := length(token_B);
      stream.write(token_L, 4);
      if token_L > 0 then
        begin
          stream.write(token_B[0], token_L);
          SetLength(token_B, 0);
        end;
      Inc(i);
    end;
end;

procedure TKDT12DI64.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  Clear;

  stream.read(st, 4);
  stream.read(ID, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if ID <> KDT12DI64_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT12DI64_Vec)) <> SizeOf(TKDT12DI64_Vec) then
          begin
            Clear;
            Exit;
          end;
        if stream.read(KDStoreBuff[i].Index, 8) <> 8 then
          begin
            Clear;
            Exit;
          end;
        if stream.read(token_L, 4) <> 4 then
          begin
            Clear;
            Exit;
          end;
        if token_L > 0 then
          begin
            SetLength(token_B, token_L);
            if stream.read(token_B[0], token_L) <> token_L then
              begin
                Clear;
                Exit;
              end;
            KDStoreBuff[i].Token.Bytes := token_B;
            SetLength(token_B, 0);
          end
        else
            KDStoreBuff[i].Token := '';
        Inc(i);
      end;
  except
    Clear;
    Exit;
  end;

  SetLength(KDBuff, cnt);
  SetLength(KDNodes, cnt);

  i := 0;
  while i < cnt do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      Inc(i);
    end;

  if cnt > 0 then
    RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDT12DI64.SaveToFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT12DI64.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT12DI64.PrintNodeTree(const NodePtr: PKDT12DI64_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT12DI64_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT12DI64Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT12DI64.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT12DI64Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT12DI64.KDT12DI64Vec(const s: SystemString): TKDT12DI64_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT12DI64_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToInt(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT12DI64_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT12DI64.KDT12DI64Vec(const v: TKDT12DI64_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT12DI64_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT12DI64.KDT12DI64Pow(const v: TKDT12DI64_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT12DI64.KDT12DI64Distance(const v1, v2: TKDT12DI64_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT12DI64_Axis - 1 do
      Result := Result + KDT12DI64Pow(v2[i] - v1[i]);
end;

procedure TKDT12DI64.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT12DI64_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT12DI64.Test;
var
  TKDT12DI64_Test: TKDT12DI64;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT12DI64_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT12DI64_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT12DI64_Test := TKDT12DI64.Create;

  DoStatusNoLn('...');
  SetLength(TKDT12DI64_Test.TestBuff, 1000);
  for i := 0 to length(TKDT12DI64_Test.TestBuff) - 1 do
    for j := 0 to KDT12DI64_Axis - 1 do
        TKDT12DI64_Test.TestBuff[i][j] := i * KDT12DI64_Axis + j;

{$IFDEF FPC}
  TKDT12DI64_Test.BuildKDTreeM(length(TKDT12DI64_Test.TestBuff), nil, @TKDT12DI64_Test.Test_BuildM);
{$ELSE FPC}
  TKDT12DI64_Test.BuildKDTreeM(length(TKDT12DI64_Test.TestBuff), nil, TKDT12DI64_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT12DI64_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT12DI64_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT12DI64_Test.TestBuff) - 1 do
    begin
      p := TKDT12DI64_Test.Search(TKDT12DI64_Test.TestBuff[i]);
      if p^.vec^.Index <> i then
          errored := True;
      if not p^.vec^.Token.Same(umlIntToStr(i)) then
          errored := True;
      if errored then
          Break;
    end;
  DisposeObject(m64);

  if not errored then
    begin
      { parallel search test }
      DoStatusNoLn('...parallel');
      SetLength(TestResultBuff, length(TKDT12DI64_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT12DI64_Test.TestBuff));
      TKDT12DI64_Test.Search(TKDT12DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT12DI64Distance(TKDT12DI64_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT12DI64_Test.Clear;
      { kMean test }
      TKDT12DI64_Test.BuildKDTreeWithCluster(TKDT12DI64_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT12DI64_Test.Search(TKDT12DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT12DI64_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT12DI64_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT12DI64_Test);
end;


function TKDT13DI64.InternalBuildKdTree(const KDSourceBufferPtr: PKDT13DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT13DI64_Node;
  function SortCompare(const p1, p2: PKDT13DI64_Source; const axis: NativeInt): ShortInt;
  begin
    if p1^.buff[axis] = p2^.buff[axis] then
      begin
        if p1^.Index = p2^.Index then
            Result := 0
        else if p1^.Index < p2^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.buff[axis] < p2^.buff[axis] then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(const SortBuffer: PKDT13DI64_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT13DI64_Source;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer^[(L + R) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, axis) < 0 do
            Inc(i);
        while SortCompare(SortBuffer^[j], p, axis) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, axis);
      L := i;
    until i >= R;
  end;

var
  M: NativeInt;
  axis: NativeInt;
  kdBuffPtr: PKDT13DI64_SourceBuffer;
begin
  Result := nil;
  if PlanCount = 0 then
      Exit;

  if PlanCount = 1 then
    begin
      new(Result);
      Result^.Parent := nil;
      Result^.Right := nil;
      Result^.Left := nil;
      Result^.vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);
    end
  else
    begin
      axis := Depth mod KDT13DI64_Axis;
      M := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr^[0], 0, PlanCount - 1, axis);

      new(Result);
      Result^.Parent := nil;
      Result^.vec := kdBuffPtr^[M];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], M, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[M + 1], PlanCount - (M + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDT13DI64.GetData(const Index: NativeInt): PKDT13DI64_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT13DI64.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT13DI64.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT13DI64.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT13DI64_Node(KDNodes[i]));
      Inc(i);
    end;

  for i := 0 to length(KDStoreBuff) - 1 do
      KDStoreBuff[i].Token := '';

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDT13DI64.StoreBuffPtr: PKDT13DI64_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT13DI64.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DI64_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT13DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT13DI64.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DI64_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT13DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT13DI64.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DI64_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT13DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT13DI64.BuildKDTreeWithCluster(const inBuff: TKDT13DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT13DI64_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT13DI64_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT13DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT13DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT13DI64.BuildKDTreeWithCluster(const inBuff: TKDT13DI64_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT13DI64.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DI64_BuildCall);
var
  TempStoreBuff: TKDT13DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT13DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT13DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT13DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT13DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT13DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT13DI64.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DI64_BuildMethod);
var
  TempStoreBuff: TKDT13DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT13DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT13DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT13DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT13DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT13DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$IFNDEF FPC}


procedure TKDT13DI64.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DI64_BuildProc);
var
  TempStoreBuff: TKDT13DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT13DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT13DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT13DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT13DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT13DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$ENDIF FPC}


function TKDT13DI64.Search(const buff: TKDT13DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT13DI64_Node;

var
  NearestNeighbour: PKDT13DI64_Node;

  function FindParentNode(const buffPtr: PKDT13DI64_Vec; NodePtr: PKDT13DI64_Node): PKDT13DI64_Node;
  var
    Next: PKDT13DI64_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT13DI64_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT13DI64_Node; const buffPtr: PKDT13DI64_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT13DI64Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT13DI64_Axis;
    Dist := NodePtr^.vec^.buff[axis] - buffPtr^[axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.vec^.buff[axis] > buffPtr^[axis] then
            ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes)
        else
            ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end
    else
      begin
        ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes);
        ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end;
  end;

  function SortCompare(const buffPtr: PKDT13DI64_Vec; const p1, p2: PKDT13DI64_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT13DI64Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT13DI64Distance(buffPtr^, p2^.vec^.buff);
    if d1 = d2 then
      begin
        if p1^.vec^.Index = p2^.vec^.Index then
            Result := 0
        else if p1^.vec^.Index < p2^.vec^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT13DI64_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT13DI64_Node;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer[(L + R) shr 1];
      repeat
        while SortCompare(buffPtr, SortBuffer[i], p) < 0 do
            Inc(i);
        while SortCompare(buffPtr, SortBuffer[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, buffPtr);
      L := i;
    until i >= R;
  end;

var
  Parent: PKDT13DI64_Node;
begin
  Result := nil;
  SearchedDistanceMin := 0;
  SearchedCounter := 0;
  NearestNeighbour := nil;
  if NearestNodes <> nil then
      NearestNodes.Clear;
  if RootNode = nil then
      Exit;
  if Count = 0 then
      Exit;

  Parent := FindParentNode(@buff[0], RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := KDT13DI64Distance(buff, Parent^.vec^.buff);

  ScanSubtree(RootNode, @buff[0], 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @buff[0]);

      if NearestNodes.Count > 0 then
          Result := PKDT13DI64_Node(NearestNodes[0]);
    end;
end;

function TKDT13DI64.Search(const buff: TKDT13DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT13DI64_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT13DI64.Search(const buff: TKDT13DI64_Vec; var SearchedDistanceMin: Double): PKDT13DI64_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT13DI64.Search(const buff: TKDT13DI64_Vec): PKDT13DI64_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT13DI64.SearchToken(const buff: TKDT13DI64_Vec): TPascalString;
var
  p: PKDT13DI64_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT13DI64.Search(const inBuff: TKDT13DI64_DynamicVecBuffer; var OutBuff: TKDT13DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT13DI64_DynamicVecBuffer;
  outBuffPtr: PKDT13DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT13DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outBuffPtr^[pass] := p^.vec^.buff;
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outBuffPtr := @OutBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT13DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outBuffPtr^[pass] := p^.vec^.buff;
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT13DI64_Node;
begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutBuff[i] := p^.vec^.buff;
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT13DI64.Search(const inBuff: TKDT13DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT13DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT13DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT13DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT13DI64_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT13DI64.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT13DI64_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT13DI64_Vec));
      stream.write(KDStoreBuff[i].Index, 8);
      token_B := KDStoreBuff[i].Token.Bytes;
      token_L := length(token_B);
      stream.write(token_L, 4);
      if token_L > 0 then
        begin
          stream.write(token_B[0], token_L);
          SetLength(token_B, 0);
        end;
      Inc(i);
    end;
end;

procedure TKDT13DI64.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  Clear;

  stream.read(st, 4);
  stream.read(ID, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if ID <> KDT13DI64_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT13DI64_Vec)) <> SizeOf(TKDT13DI64_Vec) then
          begin
            Clear;
            Exit;
          end;
        if stream.read(KDStoreBuff[i].Index, 8) <> 8 then
          begin
            Clear;
            Exit;
          end;
        if stream.read(token_L, 4) <> 4 then
          begin
            Clear;
            Exit;
          end;
        if token_L > 0 then
          begin
            SetLength(token_B, token_L);
            if stream.read(token_B[0], token_L) <> token_L then
              begin
                Clear;
                Exit;
              end;
            KDStoreBuff[i].Token.Bytes := token_B;
            SetLength(token_B, 0);
          end
        else
            KDStoreBuff[i].Token := '';
        Inc(i);
      end;
  except
    Clear;
    Exit;
  end;

  SetLength(KDBuff, cnt);
  SetLength(KDNodes, cnt);

  i := 0;
  while i < cnt do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      Inc(i);
    end;

  if cnt > 0 then
    RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDT13DI64.SaveToFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT13DI64.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT13DI64.PrintNodeTree(const NodePtr: PKDT13DI64_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT13DI64_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT13DI64Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT13DI64.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT13DI64Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT13DI64.KDT13DI64Vec(const s: SystemString): TKDT13DI64_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT13DI64_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToInt(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT13DI64_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT13DI64.KDT13DI64Vec(const v: TKDT13DI64_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT13DI64_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT13DI64.KDT13DI64Pow(const v: TKDT13DI64_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT13DI64.KDT13DI64Distance(const v1, v2: TKDT13DI64_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT13DI64_Axis - 1 do
      Result := Result + KDT13DI64Pow(v2[i] - v1[i]);
end;

procedure TKDT13DI64.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT13DI64_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT13DI64.Test;
var
  TKDT13DI64_Test: TKDT13DI64;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT13DI64_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT13DI64_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT13DI64_Test := TKDT13DI64.Create;

  DoStatusNoLn('...');
  SetLength(TKDT13DI64_Test.TestBuff, 1000);
  for i := 0 to length(TKDT13DI64_Test.TestBuff) - 1 do
    for j := 0 to KDT13DI64_Axis - 1 do
        TKDT13DI64_Test.TestBuff[i][j] := i * KDT13DI64_Axis + j;

{$IFDEF FPC}
  TKDT13DI64_Test.BuildKDTreeM(length(TKDT13DI64_Test.TestBuff), nil, @TKDT13DI64_Test.Test_BuildM);
{$ELSE FPC}
  TKDT13DI64_Test.BuildKDTreeM(length(TKDT13DI64_Test.TestBuff), nil, TKDT13DI64_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT13DI64_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT13DI64_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT13DI64_Test.TestBuff) - 1 do
    begin
      p := TKDT13DI64_Test.Search(TKDT13DI64_Test.TestBuff[i]);
      if p^.vec^.Index <> i then
          errored := True;
      if not p^.vec^.Token.Same(umlIntToStr(i)) then
          errored := True;
      if errored then
          Break;
    end;
  DisposeObject(m64);

  if not errored then
    begin
      { parallel search test }
      DoStatusNoLn('...parallel');
      SetLength(TestResultBuff, length(TKDT13DI64_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT13DI64_Test.TestBuff));
      TKDT13DI64_Test.Search(TKDT13DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT13DI64Distance(TKDT13DI64_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT13DI64_Test.Clear;
      { kMean test }
      TKDT13DI64_Test.BuildKDTreeWithCluster(TKDT13DI64_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT13DI64_Test.Search(TKDT13DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT13DI64_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT13DI64_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT13DI64_Test);
end;


function TKDT14DI64.InternalBuildKdTree(const KDSourceBufferPtr: PKDT14DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT14DI64_Node;
  function SortCompare(const p1, p2: PKDT14DI64_Source; const axis: NativeInt): ShortInt;
  begin
    if p1^.buff[axis] = p2^.buff[axis] then
      begin
        if p1^.Index = p2^.Index then
            Result := 0
        else if p1^.Index < p2^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.buff[axis] < p2^.buff[axis] then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(const SortBuffer: PKDT14DI64_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT14DI64_Source;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer^[(L + R) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, axis) < 0 do
            Inc(i);
        while SortCompare(SortBuffer^[j], p, axis) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, axis);
      L := i;
    until i >= R;
  end;

var
  M: NativeInt;
  axis: NativeInt;
  kdBuffPtr: PKDT14DI64_SourceBuffer;
begin
  Result := nil;
  if PlanCount = 0 then
      Exit;

  if PlanCount = 1 then
    begin
      new(Result);
      Result^.Parent := nil;
      Result^.Right := nil;
      Result^.Left := nil;
      Result^.vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);
    end
  else
    begin
      axis := Depth mod KDT14DI64_Axis;
      M := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr^[0], 0, PlanCount - 1, axis);

      new(Result);
      Result^.Parent := nil;
      Result^.vec := kdBuffPtr^[M];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], M, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[M + 1], PlanCount - (M + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDT14DI64.GetData(const Index: NativeInt): PKDT14DI64_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT14DI64.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT14DI64.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT14DI64.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT14DI64_Node(KDNodes[i]));
      Inc(i);
    end;

  for i := 0 to length(KDStoreBuff) - 1 do
      KDStoreBuff[i].Token := '';

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDT14DI64.StoreBuffPtr: PKDT14DI64_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT14DI64.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DI64_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT14DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT14DI64.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DI64_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT14DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT14DI64.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DI64_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT14DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT14DI64.BuildKDTreeWithCluster(const inBuff: TKDT14DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT14DI64_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT14DI64_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT14DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT14DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT14DI64.BuildKDTreeWithCluster(const inBuff: TKDT14DI64_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT14DI64.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DI64_BuildCall);
var
  TempStoreBuff: TKDT14DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT14DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT14DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT14DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT14DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT14DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT14DI64.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DI64_BuildMethod);
var
  TempStoreBuff: TKDT14DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT14DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT14DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT14DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT14DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT14DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$IFNDEF FPC}


procedure TKDT14DI64.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DI64_BuildProc);
var
  TempStoreBuff: TKDT14DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT14DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT14DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT14DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT14DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT14DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$ENDIF FPC}


function TKDT14DI64.Search(const buff: TKDT14DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT14DI64_Node;

var
  NearestNeighbour: PKDT14DI64_Node;

  function FindParentNode(const buffPtr: PKDT14DI64_Vec; NodePtr: PKDT14DI64_Node): PKDT14DI64_Node;
  var
    Next: PKDT14DI64_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT14DI64_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT14DI64_Node; const buffPtr: PKDT14DI64_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT14DI64Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT14DI64_Axis;
    Dist := NodePtr^.vec^.buff[axis] - buffPtr^[axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.vec^.buff[axis] > buffPtr^[axis] then
            ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes)
        else
            ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end
    else
      begin
        ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes);
        ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end;
  end;

  function SortCompare(const buffPtr: PKDT14DI64_Vec; const p1, p2: PKDT14DI64_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT14DI64Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT14DI64Distance(buffPtr^, p2^.vec^.buff);
    if d1 = d2 then
      begin
        if p1^.vec^.Index = p2^.vec^.Index then
            Result := 0
        else if p1^.vec^.Index < p2^.vec^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT14DI64_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT14DI64_Node;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer[(L + R) shr 1];
      repeat
        while SortCompare(buffPtr, SortBuffer[i], p) < 0 do
            Inc(i);
        while SortCompare(buffPtr, SortBuffer[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, buffPtr);
      L := i;
    until i >= R;
  end;

var
  Parent: PKDT14DI64_Node;
begin
  Result := nil;
  SearchedDistanceMin := 0;
  SearchedCounter := 0;
  NearestNeighbour := nil;
  if NearestNodes <> nil then
      NearestNodes.Clear;
  if RootNode = nil then
      Exit;
  if Count = 0 then
      Exit;

  Parent := FindParentNode(@buff[0], RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := KDT14DI64Distance(buff, Parent^.vec^.buff);

  ScanSubtree(RootNode, @buff[0], 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @buff[0]);

      if NearestNodes.Count > 0 then
          Result := PKDT14DI64_Node(NearestNodes[0]);
    end;
end;

function TKDT14DI64.Search(const buff: TKDT14DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT14DI64_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT14DI64.Search(const buff: TKDT14DI64_Vec; var SearchedDistanceMin: Double): PKDT14DI64_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT14DI64.Search(const buff: TKDT14DI64_Vec): PKDT14DI64_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT14DI64.SearchToken(const buff: TKDT14DI64_Vec): TPascalString;
var
  p: PKDT14DI64_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT14DI64.Search(const inBuff: TKDT14DI64_DynamicVecBuffer; var OutBuff: TKDT14DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT14DI64_DynamicVecBuffer;
  outBuffPtr: PKDT14DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT14DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outBuffPtr^[pass] := p^.vec^.buff;
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outBuffPtr := @OutBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT14DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outBuffPtr^[pass] := p^.vec^.buff;
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT14DI64_Node;
begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutBuff[i] := p^.vec^.buff;
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT14DI64.Search(const inBuff: TKDT14DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT14DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT14DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT14DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT14DI64_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT14DI64.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT14DI64_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT14DI64_Vec));
      stream.write(KDStoreBuff[i].Index, 8);
      token_B := KDStoreBuff[i].Token.Bytes;
      token_L := length(token_B);
      stream.write(token_L, 4);
      if token_L > 0 then
        begin
          stream.write(token_B[0], token_L);
          SetLength(token_B, 0);
        end;
      Inc(i);
    end;
end;

procedure TKDT14DI64.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  Clear;

  stream.read(st, 4);
  stream.read(ID, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if ID <> KDT14DI64_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT14DI64_Vec)) <> SizeOf(TKDT14DI64_Vec) then
          begin
            Clear;
            Exit;
          end;
        if stream.read(KDStoreBuff[i].Index, 8) <> 8 then
          begin
            Clear;
            Exit;
          end;
        if stream.read(token_L, 4) <> 4 then
          begin
            Clear;
            Exit;
          end;
        if token_L > 0 then
          begin
            SetLength(token_B, token_L);
            if stream.read(token_B[0], token_L) <> token_L then
              begin
                Clear;
                Exit;
              end;
            KDStoreBuff[i].Token.Bytes := token_B;
            SetLength(token_B, 0);
          end
        else
            KDStoreBuff[i].Token := '';
        Inc(i);
      end;
  except
    Clear;
    Exit;
  end;

  SetLength(KDBuff, cnt);
  SetLength(KDNodes, cnt);

  i := 0;
  while i < cnt do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      Inc(i);
    end;

  if cnt > 0 then
    RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDT14DI64.SaveToFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT14DI64.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT14DI64.PrintNodeTree(const NodePtr: PKDT14DI64_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT14DI64_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT14DI64Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT14DI64.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT14DI64Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT14DI64.KDT14DI64Vec(const s: SystemString): TKDT14DI64_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT14DI64_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToInt(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT14DI64_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT14DI64.KDT14DI64Vec(const v: TKDT14DI64_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT14DI64_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT14DI64.KDT14DI64Pow(const v: TKDT14DI64_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT14DI64.KDT14DI64Distance(const v1, v2: TKDT14DI64_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT14DI64_Axis - 1 do
      Result := Result + KDT14DI64Pow(v2[i] - v1[i]);
end;

procedure TKDT14DI64.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT14DI64_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT14DI64.Test;
var
  TKDT14DI64_Test: TKDT14DI64;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT14DI64_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT14DI64_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT14DI64_Test := TKDT14DI64.Create;

  DoStatusNoLn('...');
  SetLength(TKDT14DI64_Test.TestBuff, 1000);
  for i := 0 to length(TKDT14DI64_Test.TestBuff) - 1 do
    for j := 0 to KDT14DI64_Axis - 1 do
        TKDT14DI64_Test.TestBuff[i][j] := i * KDT14DI64_Axis + j;

{$IFDEF FPC}
  TKDT14DI64_Test.BuildKDTreeM(length(TKDT14DI64_Test.TestBuff), nil, @TKDT14DI64_Test.Test_BuildM);
{$ELSE FPC}
  TKDT14DI64_Test.BuildKDTreeM(length(TKDT14DI64_Test.TestBuff), nil, TKDT14DI64_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT14DI64_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT14DI64_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT14DI64_Test.TestBuff) - 1 do
    begin
      p := TKDT14DI64_Test.Search(TKDT14DI64_Test.TestBuff[i]);
      if p^.vec^.Index <> i then
          errored := True;
      if not p^.vec^.Token.Same(umlIntToStr(i)) then
          errored := True;
      if errored then
          Break;
    end;
  DisposeObject(m64);

  if not errored then
    begin
      { parallel search test }
      DoStatusNoLn('...parallel');
      SetLength(TestResultBuff, length(TKDT14DI64_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT14DI64_Test.TestBuff));
      TKDT14DI64_Test.Search(TKDT14DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT14DI64Distance(TKDT14DI64_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT14DI64_Test.Clear;
      { kMean test }
      TKDT14DI64_Test.BuildKDTreeWithCluster(TKDT14DI64_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT14DI64_Test.Search(TKDT14DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT14DI64_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT14DI64_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT14DI64_Test);
end;


function TKDT15DI64.InternalBuildKdTree(const KDSourceBufferPtr: PKDT15DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT15DI64_Node;
  function SortCompare(const p1, p2: PKDT15DI64_Source; const axis: NativeInt): ShortInt;
  begin
    if p1^.buff[axis] = p2^.buff[axis] then
      begin
        if p1^.Index = p2^.Index then
            Result := 0
        else if p1^.Index < p2^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.buff[axis] < p2^.buff[axis] then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(const SortBuffer: PKDT15DI64_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT15DI64_Source;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer^[(L + R) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, axis) < 0 do
            Inc(i);
        while SortCompare(SortBuffer^[j], p, axis) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, axis);
      L := i;
    until i >= R;
  end;

var
  M: NativeInt;
  axis: NativeInt;
  kdBuffPtr: PKDT15DI64_SourceBuffer;
begin
  Result := nil;
  if PlanCount = 0 then
      Exit;

  if PlanCount = 1 then
    begin
      new(Result);
      Result^.Parent := nil;
      Result^.Right := nil;
      Result^.Left := nil;
      Result^.vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);
    end
  else
    begin
      axis := Depth mod KDT15DI64_Axis;
      M := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr^[0], 0, PlanCount - 1, axis);

      new(Result);
      Result^.Parent := nil;
      Result^.vec := kdBuffPtr^[M];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], M, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[M + 1], PlanCount - (M + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDT15DI64.GetData(const Index: NativeInt): PKDT15DI64_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT15DI64.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT15DI64.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT15DI64.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT15DI64_Node(KDNodes[i]));
      Inc(i);
    end;

  for i := 0 to length(KDStoreBuff) - 1 do
      KDStoreBuff[i].Token := '';

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDT15DI64.StoreBuffPtr: PKDT15DI64_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT15DI64.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DI64_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT15DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT15DI64.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DI64_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT15DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT15DI64.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DI64_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT15DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT15DI64.BuildKDTreeWithCluster(const inBuff: TKDT15DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT15DI64_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT15DI64_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT15DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT15DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT15DI64.BuildKDTreeWithCluster(const inBuff: TKDT15DI64_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT15DI64.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DI64_BuildCall);
var
  TempStoreBuff: TKDT15DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT15DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT15DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT15DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT15DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT15DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT15DI64.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DI64_BuildMethod);
var
  TempStoreBuff: TKDT15DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT15DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT15DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT15DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT15DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT15DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$IFNDEF FPC}


procedure TKDT15DI64.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DI64_BuildProc);
var
  TempStoreBuff: TKDT15DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT15DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT15DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT15DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT15DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT15DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$ENDIF FPC}


function TKDT15DI64.Search(const buff: TKDT15DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT15DI64_Node;

var
  NearestNeighbour: PKDT15DI64_Node;

  function FindParentNode(const buffPtr: PKDT15DI64_Vec; NodePtr: PKDT15DI64_Node): PKDT15DI64_Node;
  var
    Next: PKDT15DI64_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT15DI64_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT15DI64_Node; const buffPtr: PKDT15DI64_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT15DI64Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT15DI64_Axis;
    Dist := NodePtr^.vec^.buff[axis] - buffPtr^[axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.vec^.buff[axis] > buffPtr^[axis] then
            ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes)
        else
            ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end
    else
      begin
        ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes);
        ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end;
  end;

  function SortCompare(const buffPtr: PKDT15DI64_Vec; const p1, p2: PKDT15DI64_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT15DI64Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT15DI64Distance(buffPtr^, p2^.vec^.buff);
    if d1 = d2 then
      begin
        if p1^.vec^.Index = p2^.vec^.Index then
            Result := 0
        else if p1^.vec^.Index < p2^.vec^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT15DI64_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT15DI64_Node;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer[(L + R) shr 1];
      repeat
        while SortCompare(buffPtr, SortBuffer[i], p) < 0 do
            Inc(i);
        while SortCompare(buffPtr, SortBuffer[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, buffPtr);
      L := i;
    until i >= R;
  end;

var
  Parent: PKDT15DI64_Node;
begin
  Result := nil;
  SearchedDistanceMin := 0;
  SearchedCounter := 0;
  NearestNeighbour := nil;
  if NearestNodes <> nil then
      NearestNodes.Clear;
  if RootNode = nil then
      Exit;
  if Count = 0 then
      Exit;

  Parent := FindParentNode(@buff[0], RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := KDT15DI64Distance(buff, Parent^.vec^.buff);

  ScanSubtree(RootNode, @buff[0], 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @buff[0]);

      if NearestNodes.Count > 0 then
          Result := PKDT15DI64_Node(NearestNodes[0]);
    end;
end;

function TKDT15DI64.Search(const buff: TKDT15DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT15DI64_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT15DI64.Search(const buff: TKDT15DI64_Vec; var SearchedDistanceMin: Double): PKDT15DI64_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT15DI64.Search(const buff: TKDT15DI64_Vec): PKDT15DI64_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT15DI64.SearchToken(const buff: TKDT15DI64_Vec): TPascalString;
var
  p: PKDT15DI64_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT15DI64.Search(const inBuff: TKDT15DI64_DynamicVecBuffer; var OutBuff: TKDT15DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT15DI64_DynamicVecBuffer;
  outBuffPtr: PKDT15DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT15DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outBuffPtr^[pass] := p^.vec^.buff;
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outBuffPtr := @OutBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT15DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outBuffPtr^[pass] := p^.vec^.buff;
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT15DI64_Node;
begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutBuff[i] := p^.vec^.buff;
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT15DI64.Search(const inBuff: TKDT15DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT15DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT15DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT15DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT15DI64_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT15DI64.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT15DI64_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT15DI64_Vec));
      stream.write(KDStoreBuff[i].Index, 8);
      token_B := KDStoreBuff[i].Token.Bytes;
      token_L := length(token_B);
      stream.write(token_L, 4);
      if token_L > 0 then
        begin
          stream.write(token_B[0], token_L);
          SetLength(token_B, 0);
        end;
      Inc(i);
    end;
end;

procedure TKDT15DI64.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  Clear;

  stream.read(st, 4);
  stream.read(ID, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if ID <> KDT15DI64_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT15DI64_Vec)) <> SizeOf(TKDT15DI64_Vec) then
          begin
            Clear;
            Exit;
          end;
        if stream.read(KDStoreBuff[i].Index, 8) <> 8 then
          begin
            Clear;
            Exit;
          end;
        if stream.read(token_L, 4) <> 4 then
          begin
            Clear;
            Exit;
          end;
        if token_L > 0 then
          begin
            SetLength(token_B, token_L);
            if stream.read(token_B[0], token_L) <> token_L then
              begin
                Clear;
                Exit;
              end;
            KDStoreBuff[i].Token.Bytes := token_B;
            SetLength(token_B, 0);
          end
        else
            KDStoreBuff[i].Token := '';
        Inc(i);
      end;
  except
    Clear;
    Exit;
  end;

  SetLength(KDBuff, cnt);
  SetLength(KDNodes, cnt);

  i := 0;
  while i < cnt do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      Inc(i);
    end;

  if cnt > 0 then
    RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDT15DI64.SaveToFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT15DI64.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT15DI64.PrintNodeTree(const NodePtr: PKDT15DI64_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT15DI64_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT15DI64Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT15DI64.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT15DI64Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT15DI64.KDT15DI64Vec(const s: SystemString): TKDT15DI64_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT15DI64_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToInt(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT15DI64_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT15DI64.KDT15DI64Vec(const v: TKDT15DI64_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT15DI64_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT15DI64.KDT15DI64Pow(const v: TKDT15DI64_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT15DI64.KDT15DI64Distance(const v1, v2: TKDT15DI64_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT15DI64_Axis - 1 do
      Result := Result + KDT15DI64Pow(v2[i] - v1[i]);
end;

procedure TKDT15DI64.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT15DI64_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT15DI64.Test;
var
  TKDT15DI64_Test: TKDT15DI64;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT15DI64_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT15DI64_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT15DI64_Test := TKDT15DI64.Create;

  DoStatusNoLn('...');
  SetLength(TKDT15DI64_Test.TestBuff, 1000);
  for i := 0 to length(TKDT15DI64_Test.TestBuff) - 1 do
    for j := 0 to KDT15DI64_Axis - 1 do
        TKDT15DI64_Test.TestBuff[i][j] := i * KDT15DI64_Axis + j;

{$IFDEF FPC}
  TKDT15DI64_Test.BuildKDTreeM(length(TKDT15DI64_Test.TestBuff), nil, @TKDT15DI64_Test.Test_BuildM);
{$ELSE FPC}
  TKDT15DI64_Test.BuildKDTreeM(length(TKDT15DI64_Test.TestBuff), nil, TKDT15DI64_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT15DI64_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT15DI64_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT15DI64_Test.TestBuff) - 1 do
    begin
      p := TKDT15DI64_Test.Search(TKDT15DI64_Test.TestBuff[i]);
      if p^.vec^.Index <> i then
          errored := True;
      if not p^.vec^.Token.Same(umlIntToStr(i)) then
          errored := True;
      if errored then
          Break;
    end;
  DisposeObject(m64);

  if not errored then
    begin
      { parallel search test }
      DoStatusNoLn('...parallel');
      SetLength(TestResultBuff, length(TKDT15DI64_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT15DI64_Test.TestBuff));
      TKDT15DI64_Test.Search(TKDT15DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT15DI64Distance(TKDT15DI64_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT15DI64_Test.Clear;
      { kMean test }
      TKDT15DI64_Test.BuildKDTreeWithCluster(TKDT15DI64_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT15DI64_Test.Search(TKDT15DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT15DI64_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT15DI64_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT15DI64_Test);
end;


function TKDT16DI64.InternalBuildKdTree(const KDSourceBufferPtr: PKDT16DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT16DI64_Node;
  function SortCompare(const p1, p2: PKDT16DI64_Source; const axis: NativeInt): ShortInt;
  begin
    if p1^.buff[axis] = p2^.buff[axis] then
      begin
        if p1^.Index = p2^.Index then
            Result := 0
        else if p1^.Index < p2^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.buff[axis] < p2^.buff[axis] then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(const SortBuffer: PKDT16DI64_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT16DI64_Source;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer^[(L + R) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, axis) < 0 do
            Inc(i);
        while SortCompare(SortBuffer^[j], p, axis) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, axis);
      L := i;
    until i >= R;
  end;

var
  M: NativeInt;
  axis: NativeInt;
  kdBuffPtr: PKDT16DI64_SourceBuffer;
begin
  Result := nil;
  if PlanCount = 0 then
      Exit;

  if PlanCount = 1 then
    begin
      new(Result);
      Result^.Parent := nil;
      Result^.Right := nil;
      Result^.Left := nil;
      Result^.vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);
    end
  else
    begin
      axis := Depth mod KDT16DI64_Axis;
      M := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr^[0], 0, PlanCount - 1, axis);

      new(Result);
      Result^.Parent := nil;
      Result^.vec := kdBuffPtr^[M];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], M, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[M + 1], PlanCount - (M + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDT16DI64.GetData(const Index: NativeInt): PKDT16DI64_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT16DI64.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT16DI64.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT16DI64.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT16DI64_Node(KDNodes[i]));
      Inc(i);
    end;

  for i := 0 to length(KDStoreBuff) - 1 do
      KDStoreBuff[i].Token := '';

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDT16DI64.StoreBuffPtr: PKDT16DI64_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT16DI64.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DI64_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT16DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT16DI64.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DI64_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT16DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT16DI64.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DI64_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT16DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT16DI64.BuildKDTreeWithCluster(const inBuff: TKDT16DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT16DI64_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT16DI64_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT16DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT16DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT16DI64.BuildKDTreeWithCluster(const inBuff: TKDT16DI64_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT16DI64.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DI64_BuildCall);
var
  TempStoreBuff: TKDT16DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT16DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT16DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT16DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT16DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT16DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT16DI64.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DI64_BuildMethod);
var
  TempStoreBuff: TKDT16DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT16DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT16DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT16DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT16DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT16DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$IFNDEF FPC}


procedure TKDT16DI64.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DI64_BuildProc);
var
  TempStoreBuff: TKDT16DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT16DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT16DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT16DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT16DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT16DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$ENDIF FPC}


function TKDT16DI64.Search(const buff: TKDT16DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT16DI64_Node;

var
  NearestNeighbour: PKDT16DI64_Node;

  function FindParentNode(const buffPtr: PKDT16DI64_Vec; NodePtr: PKDT16DI64_Node): PKDT16DI64_Node;
  var
    Next: PKDT16DI64_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT16DI64_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT16DI64_Node; const buffPtr: PKDT16DI64_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT16DI64Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT16DI64_Axis;
    Dist := NodePtr^.vec^.buff[axis] - buffPtr^[axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.vec^.buff[axis] > buffPtr^[axis] then
            ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes)
        else
            ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end
    else
      begin
        ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes);
        ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end;
  end;

  function SortCompare(const buffPtr: PKDT16DI64_Vec; const p1, p2: PKDT16DI64_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT16DI64Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT16DI64Distance(buffPtr^, p2^.vec^.buff);
    if d1 = d2 then
      begin
        if p1^.vec^.Index = p2^.vec^.Index then
            Result := 0
        else if p1^.vec^.Index < p2^.vec^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT16DI64_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT16DI64_Node;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer[(L + R) shr 1];
      repeat
        while SortCompare(buffPtr, SortBuffer[i], p) < 0 do
            Inc(i);
        while SortCompare(buffPtr, SortBuffer[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, buffPtr);
      L := i;
    until i >= R;
  end;

var
  Parent: PKDT16DI64_Node;
begin
  Result := nil;
  SearchedDistanceMin := 0;
  SearchedCounter := 0;
  NearestNeighbour := nil;
  if NearestNodes <> nil then
      NearestNodes.Clear;
  if RootNode = nil then
      Exit;
  if Count = 0 then
      Exit;

  Parent := FindParentNode(@buff[0], RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := KDT16DI64Distance(buff, Parent^.vec^.buff);

  ScanSubtree(RootNode, @buff[0], 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @buff[0]);

      if NearestNodes.Count > 0 then
          Result := PKDT16DI64_Node(NearestNodes[0]);
    end;
end;

function TKDT16DI64.Search(const buff: TKDT16DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT16DI64_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT16DI64.Search(const buff: TKDT16DI64_Vec; var SearchedDistanceMin: Double): PKDT16DI64_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT16DI64.Search(const buff: TKDT16DI64_Vec): PKDT16DI64_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT16DI64.SearchToken(const buff: TKDT16DI64_Vec): TPascalString;
var
  p: PKDT16DI64_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT16DI64.Search(const inBuff: TKDT16DI64_DynamicVecBuffer; var OutBuff: TKDT16DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT16DI64_DynamicVecBuffer;
  outBuffPtr: PKDT16DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT16DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outBuffPtr^[pass] := p^.vec^.buff;
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outBuffPtr := @OutBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT16DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outBuffPtr^[pass] := p^.vec^.buff;
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT16DI64_Node;
begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutBuff[i] := p^.vec^.buff;
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT16DI64.Search(const inBuff: TKDT16DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT16DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT16DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT16DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT16DI64_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT16DI64.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT16DI64_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT16DI64_Vec));
      stream.write(KDStoreBuff[i].Index, 8);
      token_B := KDStoreBuff[i].Token.Bytes;
      token_L := length(token_B);
      stream.write(token_L, 4);
      if token_L > 0 then
        begin
          stream.write(token_B[0], token_L);
          SetLength(token_B, 0);
        end;
      Inc(i);
    end;
end;

procedure TKDT16DI64.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  Clear;

  stream.read(st, 4);
  stream.read(ID, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if ID <> KDT16DI64_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT16DI64_Vec)) <> SizeOf(TKDT16DI64_Vec) then
          begin
            Clear;
            Exit;
          end;
        if stream.read(KDStoreBuff[i].Index, 8) <> 8 then
          begin
            Clear;
            Exit;
          end;
        if stream.read(token_L, 4) <> 4 then
          begin
            Clear;
            Exit;
          end;
        if token_L > 0 then
          begin
            SetLength(token_B, token_L);
            if stream.read(token_B[0], token_L) <> token_L then
              begin
                Clear;
                Exit;
              end;
            KDStoreBuff[i].Token.Bytes := token_B;
            SetLength(token_B, 0);
          end
        else
            KDStoreBuff[i].Token := '';
        Inc(i);
      end;
  except
    Clear;
    Exit;
  end;

  SetLength(KDBuff, cnt);
  SetLength(KDNodes, cnt);

  i := 0;
  while i < cnt do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      Inc(i);
    end;

  if cnt > 0 then
    RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDT16DI64.SaveToFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT16DI64.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT16DI64.PrintNodeTree(const NodePtr: PKDT16DI64_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT16DI64_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT16DI64Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT16DI64.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT16DI64Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT16DI64.KDT16DI64Vec(const s: SystemString): TKDT16DI64_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT16DI64_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToInt(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT16DI64_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT16DI64.KDT16DI64Vec(const v: TKDT16DI64_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT16DI64_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT16DI64.KDT16DI64Pow(const v: TKDT16DI64_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT16DI64.KDT16DI64Distance(const v1, v2: TKDT16DI64_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT16DI64_Axis - 1 do
      Result := Result + KDT16DI64Pow(v2[i] - v1[i]);
end;

procedure TKDT16DI64.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT16DI64_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT16DI64.Test;
var
  TKDT16DI64_Test: TKDT16DI64;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT16DI64_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT16DI64_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT16DI64_Test := TKDT16DI64.Create;

  DoStatusNoLn('...');
  SetLength(TKDT16DI64_Test.TestBuff, 1000);
  for i := 0 to length(TKDT16DI64_Test.TestBuff) - 1 do
    for j := 0 to KDT16DI64_Axis - 1 do
        TKDT16DI64_Test.TestBuff[i][j] := i * KDT16DI64_Axis + j;

{$IFDEF FPC}
  TKDT16DI64_Test.BuildKDTreeM(length(TKDT16DI64_Test.TestBuff), nil, @TKDT16DI64_Test.Test_BuildM);
{$ELSE FPC}
  TKDT16DI64_Test.BuildKDTreeM(length(TKDT16DI64_Test.TestBuff), nil, TKDT16DI64_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT16DI64_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT16DI64_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT16DI64_Test.TestBuff) - 1 do
    begin
      p := TKDT16DI64_Test.Search(TKDT16DI64_Test.TestBuff[i]);
      if p^.vec^.Index <> i then
          errored := True;
      if not p^.vec^.Token.Same(umlIntToStr(i)) then
          errored := True;
      if errored then
          Break;
    end;
  DisposeObject(m64);

  if not errored then
    begin
      { parallel search test }
      DoStatusNoLn('...parallel');
      SetLength(TestResultBuff, length(TKDT16DI64_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT16DI64_Test.TestBuff));
      TKDT16DI64_Test.Search(TKDT16DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT16DI64Distance(TKDT16DI64_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT16DI64_Test.Clear;
      { kMean test }
      TKDT16DI64_Test.BuildKDTreeWithCluster(TKDT16DI64_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT16DI64_Test.Search(TKDT16DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT16DI64_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT16DI64_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT16DI64_Test);
end;


function TKDT17DI64.InternalBuildKdTree(const KDSourceBufferPtr: PKDT17DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT17DI64_Node;
  function SortCompare(const p1, p2: PKDT17DI64_Source; const axis: NativeInt): ShortInt;
  begin
    if p1^.buff[axis] = p2^.buff[axis] then
      begin
        if p1^.Index = p2^.Index then
            Result := 0
        else if p1^.Index < p2^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.buff[axis] < p2^.buff[axis] then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(const SortBuffer: PKDT17DI64_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT17DI64_Source;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer^[(L + R) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, axis) < 0 do
            Inc(i);
        while SortCompare(SortBuffer^[j], p, axis) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, axis);
      L := i;
    until i >= R;
  end;

var
  M: NativeInt;
  axis: NativeInt;
  kdBuffPtr: PKDT17DI64_SourceBuffer;
begin
  Result := nil;
  if PlanCount = 0 then
      Exit;

  if PlanCount = 1 then
    begin
      new(Result);
      Result^.Parent := nil;
      Result^.Right := nil;
      Result^.Left := nil;
      Result^.vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);
    end
  else
    begin
      axis := Depth mod KDT17DI64_Axis;
      M := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr^[0], 0, PlanCount - 1, axis);

      new(Result);
      Result^.Parent := nil;
      Result^.vec := kdBuffPtr^[M];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], M, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[M + 1], PlanCount - (M + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDT17DI64.GetData(const Index: NativeInt): PKDT17DI64_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT17DI64.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT17DI64.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT17DI64.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT17DI64_Node(KDNodes[i]));
      Inc(i);
    end;

  for i := 0 to length(KDStoreBuff) - 1 do
      KDStoreBuff[i].Token := '';

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDT17DI64.StoreBuffPtr: PKDT17DI64_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT17DI64.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DI64_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT17DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT17DI64.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DI64_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT17DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT17DI64.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DI64_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT17DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT17DI64.BuildKDTreeWithCluster(const inBuff: TKDT17DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT17DI64_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT17DI64_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT17DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT17DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT17DI64.BuildKDTreeWithCluster(const inBuff: TKDT17DI64_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT17DI64.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DI64_BuildCall);
var
  TempStoreBuff: TKDT17DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT17DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT17DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT17DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT17DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT17DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT17DI64.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DI64_BuildMethod);
var
  TempStoreBuff: TKDT17DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT17DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT17DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT17DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT17DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT17DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$IFNDEF FPC}


procedure TKDT17DI64.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DI64_BuildProc);
var
  TempStoreBuff: TKDT17DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT17DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT17DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT17DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT17DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT17DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$ENDIF FPC}


function TKDT17DI64.Search(const buff: TKDT17DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT17DI64_Node;

var
  NearestNeighbour: PKDT17DI64_Node;

  function FindParentNode(const buffPtr: PKDT17DI64_Vec; NodePtr: PKDT17DI64_Node): PKDT17DI64_Node;
  var
    Next: PKDT17DI64_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT17DI64_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT17DI64_Node; const buffPtr: PKDT17DI64_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT17DI64Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT17DI64_Axis;
    Dist := NodePtr^.vec^.buff[axis] - buffPtr^[axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.vec^.buff[axis] > buffPtr^[axis] then
            ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes)
        else
            ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end
    else
      begin
        ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes);
        ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end;
  end;

  function SortCompare(const buffPtr: PKDT17DI64_Vec; const p1, p2: PKDT17DI64_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT17DI64Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT17DI64Distance(buffPtr^, p2^.vec^.buff);
    if d1 = d2 then
      begin
        if p1^.vec^.Index = p2^.vec^.Index then
            Result := 0
        else if p1^.vec^.Index < p2^.vec^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT17DI64_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT17DI64_Node;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer[(L + R) shr 1];
      repeat
        while SortCompare(buffPtr, SortBuffer[i], p) < 0 do
            Inc(i);
        while SortCompare(buffPtr, SortBuffer[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, buffPtr);
      L := i;
    until i >= R;
  end;

var
  Parent: PKDT17DI64_Node;
begin
  Result := nil;
  SearchedDistanceMin := 0;
  SearchedCounter := 0;
  NearestNeighbour := nil;
  if NearestNodes <> nil then
      NearestNodes.Clear;
  if RootNode = nil then
      Exit;
  if Count = 0 then
      Exit;

  Parent := FindParentNode(@buff[0], RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := KDT17DI64Distance(buff, Parent^.vec^.buff);

  ScanSubtree(RootNode, @buff[0], 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @buff[0]);

      if NearestNodes.Count > 0 then
          Result := PKDT17DI64_Node(NearestNodes[0]);
    end;
end;

function TKDT17DI64.Search(const buff: TKDT17DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT17DI64_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT17DI64.Search(const buff: TKDT17DI64_Vec; var SearchedDistanceMin: Double): PKDT17DI64_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT17DI64.Search(const buff: TKDT17DI64_Vec): PKDT17DI64_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT17DI64.SearchToken(const buff: TKDT17DI64_Vec): TPascalString;
var
  p: PKDT17DI64_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT17DI64.Search(const inBuff: TKDT17DI64_DynamicVecBuffer; var OutBuff: TKDT17DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT17DI64_DynamicVecBuffer;
  outBuffPtr: PKDT17DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT17DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outBuffPtr^[pass] := p^.vec^.buff;
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outBuffPtr := @OutBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT17DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outBuffPtr^[pass] := p^.vec^.buff;
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT17DI64_Node;
begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutBuff[i] := p^.vec^.buff;
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT17DI64.Search(const inBuff: TKDT17DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT17DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT17DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT17DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT17DI64_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT17DI64.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT17DI64_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT17DI64_Vec));
      stream.write(KDStoreBuff[i].Index, 8);
      token_B := KDStoreBuff[i].Token.Bytes;
      token_L := length(token_B);
      stream.write(token_L, 4);
      if token_L > 0 then
        begin
          stream.write(token_B[0], token_L);
          SetLength(token_B, 0);
        end;
      Inc(i);
    end;
end;

procedure TKDT17DI64.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  Clear;

  stream.read(st, 4);
  stream.read(ID, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if ID <> KDT17DI64_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT17DI64_Vec)) <> SizeOf(TKDT17DI64_Vec) then
          begin
            Clear;
            Exit;
          end;
        if stream.read(KDStoreBuff[i].Index, 8) <> 8 then
          begin
            Clear;
            Exit;
          end;
        if stream.read(token_L, 4) <> 4 then
          begin
            Clear;
            Exit;
          end;
        if token_L > 0 then
          begin
            SetLength(token_B, token_L);
            if stream.read(token_B[0], token_L) <> token_L then
              begin
                Clear;
                Exit;
              end;
            KDStoreBuff[i].Token.Bytes := token_B;
            SetLength(token_B, 0);
          end
        else
            KDStoreBuff[i].Token := '';
        Inc(i);
      end;
  except
    Clear;
    Exit;
  end;

  SetLength(KDBuff, cnt);
  SetLength(KDNodes, cnt);

  i := 0;
  while i < cnt do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      Inc(i);
    end;

  if cnt > 0 then
    RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDT17DI64.SaveToFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT17DI64.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT17DI64.PrintNodeTree(const NodePtr: PKDT17DI64_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT17DI64_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT17DI64Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT17DI64.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT17DI64Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT17DI64.KDT17DI64Vec(const s: SystemString): TKDT17DI64_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT17DI64_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToInt(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT17DI64_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT17DI64.KDT17DI64Vec(const v: TKDT17DI64_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT17DI64_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT17DI64.KDT17DI64Pow(const v: TKDT17DI64_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT17DI64.KDT17DI64Distance(const v1, v2: TKDT17DI64_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT17DI64_Axis - 1 do
      Result := Result + KDT17DI64Pow(v2[i] - v1[i]);
end;

procedure TKDT17DI64.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT17DI64_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT17DI64.Test;
var
  TKDT17DI64_Test: TKDT17DI64;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT17DI64_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT17DI64_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT17DI64_Test := TKDT17DI64.Create;

  DoStatusNoLn('...');
  SetLength(TKDT17DI64_Test.TestBuff, 1000);
  for i := 0 to length(TKDT17DI64_Test.TestBuff) - 1 do
    for j := 0 to KDT17DI64_Axis - 1 do
        TKDT17DI64_Test.TestBuff[i][j] := i * KDT17DI64_Axis + j;

{$IFDEF FPC}
  TKDT17DI64_Test.BuildKDTreeM(length(TKDT17DI64_Test.TestBuff), nil, @TKDT17DI64_Test.Test_BuildM);
{$ELSE FPC}
  TKDT17DI64_Test.BuildKDTreeM(length(TKDT17DI64_Test.TestBuff), nil, TKDT17DI64_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT17DI64_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT17DI64_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT17DI64_Test.TestBuff) - 1 do
    begin
      p := TKDT17DI64_Test.Search(TKDT17DI64_Test.TestBuff[i]);
      if p^.vec^.Index <> i then
          errored := True;
      if not p^.vec^.Token.Same(umlIntToStr(i)) then
          errored := True;
      if errored then
          Break;
    end;
  DisposeObject(m64);

  if not errored then
    begin
      { parallel search test }
      DoStatusNoLn('...parallel');
      SetLength(TestResultBuff, length(TKDT17DI64_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT17DI64_Test.TestBuff));
      TKDT17DI64_Test.Search(TKDT17DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT17DI64Distance(TKDT17DI64_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT17DI64_Test.Clear;
      { kMean test }
      TKDT17DI64_Test.BuildKDTreeWithCluster(TKDT17DI64_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT17DI64_Test.Search(TKDT17DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT17DI64_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT17DI64_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT17DI64_Test);
end;


function TKDT18DI64.InternalBuildKdTree(const KDSourceBufferPtr: PKDT18DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT18DI64_Node;
  function SortCompare(const p1, p2: PKDT18DI64_Source; const axis: NativeInt): ShortInt;
  begin
    if p1^.buff[axis] = p2^.buff[axis] then
      begin
        if p1^.Index = p2^.Index then
            Result := 0
        else if p1^.Index < p2^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.buff[axis] < p2^.buff[axis] then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(const SortBuffer: PKDT18DI64_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT18DI64_Source;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer^[(L + R) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, axis) < 0 do
            Inc(i);
        while SortCompare(SortBuffer^[j], p, axis) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, axis);
      L := i;
    until i >= R;
  end;

var
  M: NativeInt;
  axis: NativeInt;
  kdBuffPtr: PKDT18DI64_SourceBuffer;
begin
  Result := nil;
  if PlanCount = 0 then
      Exit;

  if PlanCount = 1 then
    begin
      new(Result);
      Result^.Parent := nil;
      Result^.Right := nil;
      Result^.Left := nil;
      Result^.vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);
    end
  else
    begin
      axis := Depth mod KDT18DI64_Axis;
      M := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr^[0], 0, PlanCount - 1, axis);

      new(Result);
      Result^.Parent := nil;
      Result^.vec := kdBuffPtr^[M];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], M, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[M + 1], PlanCount - (M + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDT18DI64.GetData(const Index: NativeInt): PKDT18DI64_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT18DI64.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT18DI64.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT18DI64.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT18DI64_Node(KDNodes[i]));
      Inc(i);
    end;

  for i := 0 to length(KDStoreBuff) - 1 do
      KDStoreBuff[i].Token := '';

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDT18DI64.StoreBuffPtr: PKDT18DI64_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT18DI64.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DI64_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT18DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT18DI64.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DI64_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT18DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT18DI64.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DI64_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT18DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT18DI64.BuildKDTreeWithCluster(const inBuff: TKDT18DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT18DI64_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT18DI64_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT18DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT18DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT18DI64.BuildKDTreeWithCluster(const inBuff: TKDT18DI64_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT18DI64.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DI64_BuildCall);
var
  TempStoreBuff: TKDT18DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT18DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT18DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT18DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT18DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT18DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT18DI64.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DI64_BuildMethod);
var
  TempStoreBuff: TKDT18DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT18DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT18DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT18DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT18DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT18DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$IFNDEF FPC}


procedure TKDT18DI64.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DI64_BuildProc);
var
  TempStoreBuff: TKDT18DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT18DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT18DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT18DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT18DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT18DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$ENDIF FPC}


function TKDT18DI64.Search(const buff: TKDT18DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT18DI64_Node;

var
  NearestNeighbour: PKDT18DI64_Node;

  function FindParentNode(const buffPtr: PKDT18DI64_Vec; NodePtr: PKDT18DI64_Node): PKDT18DI64_Node;
  var
    Next: PKDT18DI64_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT18DI64_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT18DI64_Node; const buffPtr: PKDT18DI64_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT18DI64Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT18DI64_Axis;
    Dist := NodePtr^.vec^.buff[axis] - buffPtr^[axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.vec^.buff[axis] > buffPtr^[axis] then
            ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes)
        else
            ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end
    else
      begin
        ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes);
        ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end;
  end;

  function SortCompare(const buffPtr: PKDT18DI64_Vec; const p1, p2: PKDT18DI64_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT18DI64Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT18DI64Distance(buffPtr^, p2^.vec^.buff);
    if d1 = d2 then
      begin
        if p1^.vec^.Index = p2^.vec^.Index then
            Result := 0
        else if p1^.vec^.Index < p2^.vec^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT18DI64_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT18DI64_Node;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer[(L + R) shr 1];
      repeat
        while SortCompare(buffPtr, SortBuffer[i], p) < 0 do
            Inc(i);
        while SortCompare(buffPtr, SortBuffer[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, buffPtr);
      L := i;
    until i >= R;
  end;

var
  Parent: PKDT18DI64_Node;
begin
  Result := nil;
  SearchedDistanceMin := 0;
  SearchedCounter := 0;
  NearestNeighbour := nil;
  if NearestNodes <> nil then
      NearestNodes.Clear;
  if RootNode = nil then
      Exit;
  if Count = 0 then
      Exit;

  Parent := FindParentNode(@buff[0], RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := KDT18DI64Distance(buff, Parent^.vec^.buff);

  ScanSubtree(RootNode, @buff[0], 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @buff[0]);

      if NearestNodes.Count > 0 then
          Result := PKDT18DI64_Node(NearestNodes[0]);
    end;
end;

function TKDT18DI64.Search(const buff: TKDT18DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT18DI64_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT18DI64.Search(const buff: TKDT18DI64_Vec; var SearchedDistanceMin: Double): PKDT18DI64_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT18DI64.Search(const buff: TKDT18DI64_Vec): PKDT18DI64_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT18DI64.SearchToken(const buff: TKDT18DI64_Vec): TPascalString;
var
  p: PKDT18DI64_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT18DI64.Search(const inBuff: TKDT18DI64_DynamicVecBuffer; var OutBuff: TKDT18DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT18DI64_DynamicVecBuffer;
  outBuffPtr: PKDT18DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT18DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outBuffPtr^[pass] := p^.vec^.buff;
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outBuffPtr := @OutBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT18DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outBuffPtr^[pass] := p^.vec^.buff;
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT18DI64_Node;
begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutBuff[i] := p^.vec^.buff;
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT18DI64.Search(const inBuff: TKDT18DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT18DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT18DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT18DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT18DI64_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT18DI64.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT18DI64_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT18DI64_Vec));
      stream.write(KDStoreBuff[i].Index, 8);
      token_B := KDStoreBuff[i].Token.Bytes;
      token_L := length(token_B);
      stream.write(token_L, 4);
      if token_L > 0 then
        begin
          stream.write(token_B[0], token_L);
          SetLength(token_B, 0);
        end;
      Inc(i);
    end;
end;

procedure TKDT18DI64.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  Clear;

  stream.read(st, 4);
  stream.read(ID, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if ID <> KDT18DI64_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT18DI64_Vec)) <> SizeOf(TKDT18DI64_Vec) then
          begin
            Clear;
            Exit;
          end;
        if stream.read(KDStoreBuff[i].Index, 8) <> 8 then
          begin
            Clear;
            Exit;
          end;
        if stream.read(token_L, 4) <> 4 then
          begin
            Clear;
            Exit;
          end;
        if token_L > 0 then
          begin
            SetLength(token_B, token_L);
            if stream.read(token_B[0], token_L) <> token_L then
              begin
                Clear;
                Exit;
              end;
            KDStoreBuff[i].Token.Bytes := token_B;
            SetLength(token_B, 0);
          end
        else
            KDStoreBuff[i].Token := '';
        Inc(i);
      end;
  except
    Clear;
    Exit;
  end;

  SetLength(KDBuff, cnt);
  SetLength(KDNodes, cnt);

  i := 0;
  while i < cnt do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      Inc(i);
    end;

  if cnt > 0 then
    RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDT18DI64.SaveToFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT18DI64.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT18DI64.PrintNodeTree(const NodePtr: PKDT18DI64_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT18DI64_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT18DI64Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT18DI64.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT18DI64Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT18DI64.KDT18DI64Vec(const s: SystemString): TKDT18DI64_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT18DI64_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToInt(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT18DI64_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT18DI64.KDT18DI64Vec(const v: TKDT18DI64_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT18DI64_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT18DI64.KDT18DI64Pow(const v: TKDT18DI64_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT18DI64.KDT18DI64Distance(const v1, v2: TKDT18DI64_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT18DI64_Axis - 1 do
      Result := Result + KDT18DI64Pow(v2[i] - v1[i]);
end;

procedure TKDT18DI64.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT18DI64_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT18DI64.Test;
var
  TKDT18DI64_Test: TKDT18DI64;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT18DI64_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT18DI64_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT18DI64_Test := TKDT18DI64.Create;

  DoStatusNoLn('...');
  SetLength(TKDT18DI64_Test.TestBuff, 1000);
  for i := 0 to length(TKDT18DI64_Test.TestBuff) - 1 do
    for j := 0 to KDT18DI64_Axis - 1 do
        TKDT18DI64_Test.TestBuff[i][j] := i * KDT18DI64_Axis + j;

{$IFDEF FPC}
  TKDT18DI64_Test.BuildKDTreeM(length(TKDT18DI64_Test.TestBuff), nil, @TKDT18DI64_Test.Test_BuildM);
{$ELSE FPC}
  TKDT18DI64_Test.BuildKDTreeM(length(TKDT18DI64_Test.TestBuff), nil, TKDT18DI64_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT18DI64_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT18DI64_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT18DI64_Test.TestBuff) - 1 do
    begin
      p := TKDT18DI64_Test.Search(TKDT18DI64_Test.TestBuff[i]);
      if p^.vec^.Index <> i then
          errored := True;
      if not p^.vec^.Token.Same(umlIntToStr(i)) then
          errored := True;
      if errored then
          Break;
    end;
  DisposeObject(m64);

  if not errored then
    begin
      { parallel search test }
      DoStatusNoLn('...parallel');
      SetLength(TestResultBuff, length(TKDT18DI64_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT18DI64_Test.TestBuff));
      TKDT18DI64_Test.Search(TKDT18DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT18DI64Distance(TKDT18DI64_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT18DI64_Test.Clear;
      { kMean test }
      TKDT18DI64_Test.BuildKDTreeWithCluster(TKDT18DI64_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT18DI64_Test.Search(TKDT18DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT18DI64_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT18DI64_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT18DI64_Test);
end;


function TKDT19DI64.InternalBuildKdTree(const KDSourceBufferPtr: PKDT19DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT19DI64_Node;
  function SortCompare(const p1, p2: PKDT19DI64_Source; const axis: NativeInt): ShortInt;
  begin
    if p1^.buff[axis] = p2^.buff[axis] then
      begin
        if p1^.Index = p2^.Index then
            Result := 0
        else if p1^.Index < p2^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.buff[axis] < p2^.buff[axis] then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(const SortBuffer: PKDT19DI64_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT19DI64_Source;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer^[(L + R) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, axis) < 0 do
            Inc(i);
        while SortCompare(SortBuffer^[j], p, axis) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, axis);
      L := i;
    until i >= R;
  end;

var
  M: NativeInt;
  axis: NativeInt;
  kdBuffPtr: PKDT19DI64_SourceBuffer;
begin
  Result := nil;
  if PlanCount = 0 then
      Exit;

  if PlanCount = 1 then
    begin
      new(Result);
      Result^.Parent := nil;
      Result^.Right := nil;
      Result^.Left := nil;
      Result^.vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);
    end
  else
    begin
      axis := Depth mod KDT19DI64_Axis;
      M := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr^[0], 0, PlanCount - 1, axis);

      new(Result);
      Result^.Parent := nil;
      Result^.vec := kdBuffPtr^[M];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], M, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[M + 1], PlanCount - (M + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDT19DI64.GetData(const Index: NativeInt): PKDT19DI64_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT19DI64.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT19DI64.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT19DI64.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT19DI64_Node(KDNodes[i]));
      Inc(i);
    end;

  for i := 0 to length(KDStoreBuff) - 1 do
      KDStoreBuff[i].Token := '';

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDT19DI64.StoreBuffPtr: PKDT19DI64_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT19DI64.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DI64_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT19DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT19DI64.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DI64_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT19DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT19DI64.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DI64_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT19DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT19DI64.BuildKDTreeWithCluster(const inBuff: TKDT19DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT19DI64_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT19DI64_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT19DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT19DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT19DI64.BuildKDTreeWithCluster(const inBuff: TKDT19DI64_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT19DI64.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DI64_BuildCall);
var
  TempStoreBuff: TKDT19DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT19DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT19DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT19DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT19DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT19DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT19DI64.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DI64_BuildMethod);
var
  TempStoreBuff: TKDT19DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT19DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT19DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT19DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT19DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT19DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$IFNDEF FPC}


procedure TKDT19DI64.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DI64_BuildProc);
var
  TempStoreBuff: TKDT19DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT19DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT19DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT19DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT19DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT19DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$ENDIF FPC}


function TKDT19DI64.Search(const buff: TKDT19DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT19DI64_Node;

var
  NearestNeighbour: PKDT19DI64_Node;

  function FindParentNode(const buffPtr: PKDT19DI64_Vec; NodePtr: PKDT19DI64_Node): PKDT19DI64_Node;
  var
    Next: PKDT19DI64_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT19DI64_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT19DI64_Node; const buffPtr: PKDT19DI64_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT19DI64Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT19DI64_Axis;
    Dist := NodePtr^.vec^.buff[axis] - buffPtr^[axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.vec^.buff[axis] > buffPtr^[axis] then
            ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes)
        else
            ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end
    else
      begin
        ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes);
        ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end;
  end;

  function SortCompare(const buffPtr: PKDT19DI64_Vec; const p1, p2: PKDT19DI64_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT19DI64Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT19DI64Distance(buffPtr^, p2^.vec^.buff);
    if d1 = d2 then
      begin
        if p1^.vec^.Index = p2^.vec^.Index then
            Result := 0
        else if p1^.vec^.Index < p2^.vec^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT19DI64_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT19DI64_Node;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer[(L + R) shr 1];
      repeat
        while SortCompare(buffPtr, SortBuffer[i], p) < 0 do
            Inc(i);
        while SortCompare(buffPtr, SortBuffer[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, buffPtr);
      L := i;
    until i >= R;
  end;

var
  Parent: PKDT19DI64_Node;
begin
  Result := nil;
  SearchedDistanceMin := 0;
  SearchedCounter := 0;
  NearestNeighbour := nil;
  if NearestNodes <> nil then
      NearestNodes.Clear;
  if RootNode = nil then
      Exit;
  if Count = 0 then
      Exit;

  Parent := FindParentNode(@buff[0], RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := KDT19DI64Distance(buff, Parent^.vec^.buff);

  ScanSubtree(RootNode, @buff[0], 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @buff[0]);

      if NearestNodes.Count > 0 then
          Result := PKDT19DI64_Node(NearestNodes[0]);
    end;
end;

function TKDT19DI64.Search(const buff: TKDT19DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT19DI64_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT19DI64.Search(const buff: TKDT19DI64_Vec; var SearchedDistanceMin: Double): PKDT19DI64_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT19DI64.Search(const buff: TKDT19DI64_Vec): PKDT19DI64_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT19DI64.SearchToken(const buff: TKDT19DI64_Vec): TPascalString;
var
  p: PKDT19DI64_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT19DI64.Search(const inBuff: TKDT19DI64_DynamicVecBuffer; var OutBuff: TKDT19DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT19DI64_DynamicVecBuffer;
  outBuffPtr: PKDT19DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT19DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outBuffPtr^[pass] := p^.vec^.buff;
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outBuffPtr := @OutBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT19DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outBuffPtr^[pass] := p^.vec^.buff;
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT19DI64_Node;
begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutBuff[i] := p^.vec^.buff;
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT19DI64.Search(const inBuff: TKDT19DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT19DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT19DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT19DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT19DI64_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT19DI64.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT19DI64_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT19DI64_Vec));
      stream.write(KDStoreBuff[i].Index, 8);
      token_B := KDStoreBuff[i].Token.Bytes;
      token_L := length(token_B);
      stream.write(token_L, 4);
      if token_L > 0 then
        begin
          stream.write(token_B[0], token_L);
          SetLength(token_B, 0);
        end;
      Inc(i);
    end;
end;

procedure TKDT19DI64.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  Clear;

  stream.read(st, 4);
  stream.read(ID, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if ID <> KDT19DI64_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT19DI64_Vec)) <> SizeOf(TKDT19DI64_Vec) then
          begin
            Clear;
            Exit;
          end;
        if stream.read(KDStoreBuff[i].Index, 8) <> 8 then
          begin
            Clear;
            Exit;
          end;
        if stream.read(token_L, 4) <> 4 then
          begin
            Clear;
            Exit;
          end;
        if token_L > 0 then
          begin
            SetLength(token_B, token_L);
            if stream.read(token_B[0], token_L) <> token_L then
              begin
                Clear;
                Exit;
              end;
            KDStoreBuff[i].Token.Bytes := token_B;
            SetLength(token_B, 0);
          end
        else
            KDStoreBuff[i].Token := '';
        Inc(i);
      end;
  except
    Clear;
    Exit;
  end;

  SetLength(KDBuff, cnt);
  SetLength(KDNodes, cnt);

  i := 0;
  while i < cnt do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      Inc(i);
    end;

  if cnt > 0 then
    RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDT19DI64.SaveToFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT19DI64.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT19DI64.PrintNodeTree(const NodePtr: PKDT19DI64_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT19DI64_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT19DI64Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT19DI64.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT19DI64Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT19DI64.KDT19DI64Vec(const s: SystemString): TKDT19DI64_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT19DI64_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToInt(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT19DI64_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT19DI64.KDT19DI64Vec(const v: TKDT19DI64_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT19DI64_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT19DI64.KDT19DI64Pow(const v: TKDT19DI64_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT19DI64.KDT19DI64Distance(const v1, v2: TKDT19DI64_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT19DI64_Axis - 1 do
      Result := Result + KDT19DI64Pow(v2[i] - v1[i]);
end;

procedure TKDT19DI64.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT19DI64_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT19DI64.Test;
var
  TKDT19DI64_Test: TKDT19DI64;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT19DI64_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT19DI64_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT19DI64_Test := TKDT19DI64.Create;

  DoStatusNoLn('...');
  SetLength(TKDT19DI64_Test.TestBuff, 1000);
  for i := 0 to length(TKDT19DI64_Test.TestBuff) - 1 do
    for j := 0 to KDT19DI64_Axis - 1 do
        TKDT19DI64_Test.TestBuff[i][j] := i * KDT19DI64_Axis + j;

{$IFDEF FPC}
  TKDT19DI64_Test.BuildKDTreeM(length(TKDT19DI64_Test.TestBuff), nil, @TKDT19DI64_Test.Test_BuildM);
{$ELSE FPC}
  TKDT19DI64_Test.BuildKDTreeM(length(TKDT19DI64_Test.TestBuff), nil, TKDT19DI64_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT19DI64_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT19DI64_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT19DI64_Test.TestBuff) - 1 do
    begin
      p := TKDT19DI64_Test.Search(TKDT19DI64_Test.TestBuff[i]);
      if p^.vec^.Index <> i then
          errored := True;
      if not p^.vec^.Token.Same(umlIntToStr(i)) then
          errored := True;
      if errored then
          Break;
    end;
  DisposeObject(m64);

  if not errored then
    begin
      { parallel search test }
      DoStatusNoLn('...parallel');
      SetLength(TestResultBuff, length(TKDT19DI64_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT19DI64_Test.TestBuff));
      TKDT19DI64_Test.Search(TKDT19DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT19DI64Distance(TKDT19DI64_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT19DI64_Test.Clear;
      { kMean test }
      TKDT19DI64_Test.BuildKDTreeWithCluster(TKDT19DI64_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT19DI64_Test.Search(TKDT19DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT19DI64_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT19DI64_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT19DI64_Test);
end;


function TKDT20DI64.InternalBuildKdTree(const KDSourceBufferPtr: PKDT20DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT20DI64_Node;
  function SortCompare(const p1, p2: PKDT20DI64_Source; const axis: NativeInt): ShortInt;
  begin
    if p1^.buff[axis] = p2^.buff[axis] then
      begin
        if p1^.Index = p2^.Index then
            Result := 0
        else if p1^.Index < p2^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.buff[axis] < p2^.buff[axis] then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(const SortBuffer: PKDT20DI64_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT20DI64_Source;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer^[(L + R) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, axis) < 0 do
            Inc(i);
        while SortCompare(SortBuffer^[j], p, axis) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, axis);
      L := i;
    until i >= R;
  end;

var
  M: NativeInt;
  axis: NativeInt;
  kdBuffPtr: PKDT20DI64_SourceBuffer;
begin
  Result := nil;
  if PlanCount = 0 then
      Exit;

  if PlanCount = 1 then
    begin
      new(Result);
      Result^.Parent := nil;
      Result^.Right := nil;
      Result^.Left := nil;
      Result^.vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);
    end
  else
    begin
      axis := Depth mod KDT20DI64_Axis;
      M := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr^[0], 0, PlanCount - 1, axis);

      new(Result);
      Result^.Parent := nil;
      Result^.vec := kdBuffPtr^[M];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], M, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[M + 1], PlanCount - (M + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDT20DI64.GetData(const Index: NativeInt): PKDT20DI64_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT20DI64.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT20DI64.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT20DI64.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT20DI64_Node(KDNodes[i]));
      Inc(i);
    end;

  for i := 0 to length(KDStoreBuff) - 1 do
      KDStoreBuff[i].Token := '';

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDT20DI64.StoreBuffPtr: PKDT20DI64_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT20DI64.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DI64_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT20DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT20DI64.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DI64_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT20DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT20DI64.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DI64_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT20DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT20DI64.BuildKDTreeWithCluster(const inBuff: TKDT20DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT20DI64_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT20DI64_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT20DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT20DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT20DI64.BuildKDTreeWithCluster(const inBuff: TKDT20DI64_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT20DI64.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DI64_BuildCall);
var
  TempStoreBuff: TKDT20DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT20DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT20DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT20DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT20DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT20DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT20DI64.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DI64_BuildMethod);
var
  TempStoreBuff: TKDT20DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT20DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT20DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT20DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT20DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT20DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$IFNDEF FPC}


procedure TKDT20DI64.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DI64_BuildProc);
var
  TempStoreBuff: TKDT20DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT20DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT20DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT20DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT20DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT20DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$ENDIF FPC}


function TKDT20DI64.Search(const buff: TKDT20DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT20DI64_Node;

var
  NearestNeighbour: PKDT20DI64_Node;

  function FindParentNode(const buffPtr: PKDT20DI64_Vec; NodePtr: PKDT20DI64_Node): PKDT20DI64_Node;
  var
    Next: PKDT20DI64_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT20DI64_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT20DI64_Node; const buffPtr: PKDT20DI64_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT20DI64Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT20DI64_Axis;
    Dist := NodePtr^.vec^.buff[axis] - buffPtr^[axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.vec^.buff[axis] > buffPtr^[axis] then
            ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes)
        else
            ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end
    else
      begin
        ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes);
        ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end;
  end;

  function SortCompare(const buffPtr: PKDT20DI64_Vec; const p1, p2: PKDT20DI64_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT20DI64Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT20DI64Distance(buffPtr^, p2^.vec^.buff);
    if d1 = d2 then
      begin
        if p1^.vec^.Index = p2^.vec^.Index then
            Result := 0
        else if p1^.vec^.Index < p2^.vec^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT20DI64_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT20DI64_Node;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer[(L + R) shr 1];
      repeat
        while SortCompare(buffPtr, SortBuffer[i], p) < 0 do
            Inc(i);
        while SortCompare(buffPtr, SortBuffer[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, buffPtr);
      L := i;
    until i >= R;
  end;

var
  Parent: PKDT20DI64_Node;
begin
  Result := nil;
  SearchedDistanceMin := 0;
  SearchedCounter := 0;
  NearestNeighbour := nil;
  if NearestNodes <> nil then
      NearestNodes.Clear;
  if RootNode = nil then
      Exit;
  if Count = 0 then
      Exit;

  Parent := FindParentNode(@buff[0], RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := KDT20DI64Distance(buff, Parent^.vec^.buff);

  ScanSubtree(RootNode, @buff[0], 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @buff[0]);

      if NearestNodes.Count > 0 then
          Result := PKDT20DI64_Node(NearestNodes[0]);
    end;
end;

function TKDT20DI64.Search(const buff: TKDT20DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT20DI64_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT20DI64.Search(const buff: TKDT20DI64_Vec; var SearchedDistanceMin: Double): PKDT20DI64_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT20DI64.Search(const buff: TKDT20DI64_Vec): PKDT20DI64_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT20DI64.SearchToken(const buff: TKDT20DI64_Vec): TPascalString;
var
  p: PKDT20DI64_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT20DI64.Search(const inBuff: TKDT20DI64_DynamicVecBuffer; var OutBuff: TKDT20DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT20DI64_DynamicVecBuffer;
  outBuffPtr: PKDT20DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT20DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outBuffPtr^[pass] := p^.vec^.buff;
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outBuffPtr := @OutBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT20DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outBuffPtr^[pass] := p^.vec^.buff;
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT20DI64_Node;
begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutBuff[i] := p^.vec^.buff;
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT20DI64.Search(const inBuff: TKDT20DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT20DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT20DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT20DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT20DI64_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT20DI64.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT20DI64_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT20DI64_Vec));
      stream.write(KDStoreBuff[i].Index, 8);
      token_B := KDStoreBuff[i].Token.Bytes;
      token_L := length(token_B);
      stream.write(token_L, 4);
      if token_L > 0 then
        begin
          stream.write(token_B[0], token_L);
          SetLength(token_B, 0);
        end;
      Inc(i);
    end;
end;

procedure TKDT20DI64.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  Clear;

  stream.read(st, 4);
  stream.read(ID, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if ID <> KDT20DI64_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT20DI64_Vec)) <> SizeOf(TKDT20DI64_Vec) then
          begin
            Clear;
            Exit;
          end;
        if stream.read(KDStoreBuff[i].Index, 8) <> 8 then
          begin
            Clear;
            Exit;
          end;
        if stream.read(token_L, 4) <> 4 then
          begin
            Clear;
            Exit;
          end;
        if token_L > 0 then
          begin
            SetLength(token_B, token_L);
            if stream.read(token_B[0], token_L) <> token_L then
              begin
                Clear;
                Exit;
              end;
            KDStoreBuff[i].Token.Bytes := token_B;
            SetLength(token_B, 0);
          end
        else
            KDStoreBuff[i].Token := '';
        Inc(i);
      end;
  except
    Clear;
    Exit;
  end;

  SetLength(KDBuff, cnt);
  SetLength(KDNodes, cnt);

  i := 0;
  while i < cnt do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      Inc(i);
    end;

  if cnt > 0 then
    RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDT20DI64.SaveToFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT20DI64.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT20DI64.PrintNodeTree(const NodePtr: PKDT20DI64_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT20DI64_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT20DI64Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT20DI64.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT20DI64Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT20DI64.KDT20DI64Vec(const s: SystemString): TKDT20DI64_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT20DI64_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToInt(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT20DI64_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT20DI64.KDT20DI64Vec(const v: TKDT20DI64_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT20DI64_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT20DI64.KDT20DI64Pow(const v: TKDT20DI64_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT20DI64.KDT20DI64Distance(const v1, v2: TKDT20DI64_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT20DI64_Axis - 1 do
      Result := Result + KDT20DI64Pow(v2[i] - v1[i]);
end;

procedure TKDT20DI64.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT20DI64_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT20DI64.Test;
var
  TKDT20DI64_Test: TKDT20DI64;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT20DI64_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT20DI64_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT20DI64_Test := TKDT20DI64.Create;

  DoStatusNoLn('...');
  SetLength(TKDT20DI64_Test.TestBuff, 1000);
  for i := 0 to length(TKDT20DI64_Test.TestBuff) - 1 do
    for j := 0 to KDT20DI64_Axis - 1 do
        TKDT20DI64_Test.TestBuff[i][j] := i * KDT20DI64_Axis + j;

{$IFDEF FPC}
  TKDT20DI64_Test.BuildKDTreeM(length(TKDT20DI64_Test.TestBuff), nil, @TKDT20DI64_Test.Test_BuildM);
{$ELSE FPC}
  TKDT20DI64_Test.BuildKDTreeM(length(TKDT20DI64_Test.TestBuff), nil, TKDT20DI64_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT20DI64_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT20DI64_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT20DI64_Test.TestBuff) - 1 do
    begin
      p := TKDT20DI64_Test.Search(TKDT20DI64_Test.TestBuff[i]);
      if p^.vec^.Index <> i then
          errored := True;
      if not p^.vec^.Token.Same(umlIntToStr(i)) then
          errored := True;
      if errored then
          Break;
    end;
  DisposeObject(m64);

  if not errored then
    begin
      { parallel search test }
      DoStatusNoLn('...parallel');
      SetLength(TestResultBuff, length(TKDT20DI64_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT20DI64_Test.TestBuff));
      TKDT20DI64_Test.Search(TKDT20DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT20DI64Distance(TKDT20DI64_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT20DI64_Test.Clear;
      { kMean test }
      TKDT20DI64_Test.BuildKDTreeWithCluster(TKDT20DI64_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT20DI64_Test.Search(TKDT20DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT20DI64_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT20DI64_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT20DI64_Test);
end;


function TKDT21DI64.InternalBuildKdTree(const KDSourceBufferPtr: PKDT21DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT21DI64_Node;
  function SortCompare(const p1, p2: PKDT21DI64_Source; const axis: NativeInt): ShortInt;
  begin
    if p1^.buff[axis] = p2^.buff[axis] then
      begin
        if p1^.Index = p2^.Index then
            Result := 0
        else if p1^.Index < p2^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.buff[axis] < p2^.buff[axis] then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(const SortBuffer: PKDT21DI64_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT21DI64_Source;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer^[(L + R) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, axis) < 0 do
            Inc(i);
        while SortCompare(SortBuffer^[j], p, axis) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, axis);
      L := i;
    until i >= R;
  end;

var
  M: NativeInt;
  axis: NativeInt;
  kdBuffPtr: PKDT21DI64_SourceBuffer;
begin
  Result := nil;
  if PlanCount = 0 then
      Exit;

  if PlanCount = 1 then
    begin
      new(Result);
      Result^.Parent := nil;
      Result^.Right := nil;
      Result^.Left := nil;
      Result^.vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);
    end
  else
    begin
      axis := Depth mod KDT21DI64_Axis;
      M := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr^[0], 0, PlanCount - 1, axis);

      new(Result);
      Result^.Parent := nil;
      Result^.vec := kdBuffPtr^[M];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], M, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[M + 1], PlanCount - (M + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDT21DI64.GetData(const Index: NativeInt): PKDT21DI64_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT21DI64.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT21DI64.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT21DI64.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT21DI64_Node(KDNodes[i]));
      Inc(i);
    end;

  for i := 0 to length(KDStoreBuff) - 1 do
      KDStoreBuff[i].Token := '';

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDT21DI64.StoreBuffPtr: PKDT21DI64_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT21DI64.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DI64_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT21DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT21DI64.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DI64_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT21DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT21DI64.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DI64_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT21DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT21DI64.BuildKDTreeWithCluster(const inBuff: TKDT21DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT21DI64_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT21DI64_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT21DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT21DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT21DI64.BuildKDTreeWithCluster(const inBuff: TKDT21DI64_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT21DI64.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DI64_BuildCall);
var
  TempStoreBuff: TKDT21DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT21DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT21DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT21DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT21DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT21DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT21DI64.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DI64_BuildMethod);
var
  TempStoreBuff: TKDT21DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT21DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT21DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT21DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT21DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT21DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$IFNDEF FPC}


procedure TKDT21DI64.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DI64_BuildProc);
var
  TempStoreBuff: TKDT21DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT21DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT21DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT21DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT21DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT21DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$ENDIF FPC}


function TKDT21DI64.Search(const buff: TKDT21DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT21DI64_Node;

var
  NearestNeighbour: PKDT21DI64_Node;

  function FindParentNode(const buffPtr: PKDT21DI64_Vec; NodePtr: PKDT21DI64_Node): PKDT21DI64_Node;
  var
    Next: PKDT21DI64_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT21DI64_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT21DI64_Node; const buffPtr: PKDT21DI64_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT21DI64Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT21DI64_Axis;
    Dist := NodePtr^.vec^.buff[axis] - buffPtr^[axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.vec^.buff[axis] > buffPtr^[axis] then
            ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes)
        else
            ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end
    else
      begin
        ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes);
        ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end;
  end;

  function SortCompare(const buffPtr: PKDT21DI64_Vec; const p1, p2: PKDT21DI64_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT21DI64Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT21DI64Distance(buffPtr^, p2^.vec^.buff);
    if d1 = d2 then
      begin
        if p1^.vec^.Index = p2^.vec^.Index then
            Result := 0
        else if p1^.vec^.Index < p2^.vec^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT21DI64_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT21DI64_Node;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer[(L + R) shr 1];
      repeat
        while SortCompare(buffPtr, SortBuffer[i], p) < 0 do
            Inc(i);
        while SortCompare(buffPtr, SortBuffer[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, buffPtr);
      L := i;
    until i >= R;
  end;

var
  Parent: PKDT21DI64_Node;
begin
  Result := nil;
  SearchedDistanceMin := 0;
  SearchedCounter := 0;
  NearestNeighbour := nil;
  if NearestNodes <> nil then
      NearestNodes.Clear;
  if RootNode = nil then
      Exit;
  if Count = 0 then
      Exit;

  Parent := FindParentNode(@buff[0], RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := KDT21DI64Distance(buff, Parent^.vec^.buff);

  ScanSubtree(RootNode, @buff[0], 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @buff[0]);

      if NearestNodes.Count > 0 then
          Result := PKDT21DI64_Node(NearestNodes[0]);
    end;
end;

function TKDT21DI64.Search(const buff: TKDT21DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT21DI64_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT21DI64.Search(const buff: TKDT21DI64_Vec; var SearchedDistanceMin: Double): PKDT21DI64_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT21DI64.Search(const buff: TKDT21DI64_Vec): PKDT21DI64_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT21DI64.SearchToken(const buff: TKDT21DI64_Vec): TPascalString;
var
  p: PKDT21DI64_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT21DI64.Search(const inBuff: TKDT21DI64_DynamicVecBuffer; var OutBuff: TKDT21DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT21DI64_DynamicVecBuffer;
  outBuffPtr: PKDT21DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT21DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outBuffPtr^[pass] := p^.vec^.buff;
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outBuffPtr := @OutBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT21DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outBuffPtr^[pass] := p^.vec^.buff;
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT21DI64_Node;
begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutBuff[i] := p^.vec^.buff;
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT21DI64.Search(const inBuff: TKDT21DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT21DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT21DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT21DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT21DI64_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT21DI64.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT21DI64_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT21DI64_Vec));
      stream.write(KDStoreBuff[i].Index, 8);
      token_B := KDStoreBuff[i].Token.Bytes;
      token_L := length(token_B);
      stream.write(token_L, 4);
      if token_L > 0 then
        begin
          stream.write(token_B[0], token_L);
          SetLength(token_B, 0);
        end;
      Inc(i);
    end;
end;

procedure TKDT21DI64.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  Clear;

  stream.read(st, 4);
  stream.read(ID, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if ID <> KDT21DI64_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT21DI64_Vec)) <> SizeOf(TKDT21DI64_Vec) then
          begin
            Clear;
            Exit;
          end;
        if stream.read(KDStoreBuff[i].Index, 8) <> 8 then
          begin
            Clear;
            Exit;
          end;
        if stream.read(token_L, 4) <> 4 then
          begin
            Clear;
            Exit;
          end;
        if token_L > 0 then
          begin
            SetLength(token_B, token_L);
            if stream.read(token_B[0], token_L) <> token_L then
              begin
                Clear;
                Exit;
              end;
            KDStoreBuff[i].Token.Bytes := token_B;
            SetLength(token_B, 0);
          end
        else
            KDStoreBuff[i].Token := '';
        Inc(i);
      end;
  except
    Clear;
    Exit;
  end;

  SetLength(KDBuff, cnt);
  SetLength(KDNodes, cnt);

  i := 0;
  while i < cnt do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      Inc(i);
    end;

  if cnt > 0 then
    RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDT21DI64.SaveToFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT21DI64.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT21DI64.PrintNodeTree(const NodePtr: PKDT21DI64_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT21DI64_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT21DI64Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT21DI64.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT21DI64Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT21DI64.KDT21DI64Vec(const s: SystemString): TKDT21DI64_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT21DI64_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToInt(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT21DI64_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT21DI64.KDT21DI64Vec(const v: TKDT21DI64_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT21DI64_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT21DI64.KDT21DI64Pow(const v: TKDT21DI64_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT21DI64.KDT21DI64Distance(const v1, v2: TKDT21DI64_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT21DI64_Axis - 1 do
      Result := Result + KDT21DI64Pow(v2[i] - v1[i]);
end;

procedure TKDT21DI64.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT21DI64_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT21DI64.Test;
var
  TKDT21DI64_Test: TKDT21DI64;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT21DI64_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT21DI64_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT21DI64_Test := TKDT21DI64.Create;

  DoStatusNoLn('...');
  SetLength(TKDT21DI64_Test.TestBuff, 1000);
  for i := 0 to length(TKDT21DI64_Test.TestBuff) - 1 do
    for j := 0 to KDT21DI64_Axis - 1 do
        TKDT21DI64_Test.TestBuff[i][j] := i * KDT21DI64_Axis + j;

{$IFDEF FPC}
  TKDT21DI64_Test.BuildKDTreeM(length(TKDT21DI64_Test.TestBuff), nil, @TKDT21DI64_Test.Test_BuildM);
{$ELSE FPC}
  TKDT21DI64_Test.BuildKDTreeM(length(TKDT21DI64_Test.TestBuff), nil, TKDT21DI64_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT21DI64_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT21DI64_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT21DI64_Test.TestBuff) - 1 do
    begin
      p := TKDT21DI64_Test.Search(TKDT21DI64_Test.TestBuff[i]);
      if p^.vec^.Index <> i then
          errored := True;
      if not p^.vec^.Token.Same(umlIntToStr(i)) then
          errored := True;
      if errored then
          Break;
    end;
  DisposeObject(m64);

  if not errored then
    begin
      { parallel search test }
      DoStatusNoLn('...parallel');
      SetLength(TestResultBuff, length(TKDT21DI64_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT21DI64_Test.TestBuff));
      TKDT21DI64_Test.Search(TKDT21DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT21DI64Distance(TKDT21DI64_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT21DI64_Test.Clear;
      { kMean test }
      TKDT21DI64_Test.BuildKDTreeWithCluster(TKDT21DI64_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT21DI64_Test.Search(TKDT21DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT21DI64_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT21DI64_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT21DI64_Test);
end;


function TKDT22DI64.InternalBuildKdTree(const KDSourceBufferPtr: PKDT22DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT22DI64_Node;
  function SortCompare(const p1, p2: PKDT22DI64_Source; const axis: NativeInt): ShortInt;
  begin
    if p1^.buff[axis] = p2^.buff[axis] then
      begin
        if p1^.Index = p2^.Index then
            Result := 0
        else if p1^.Index < p2^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.buff[axis] < p2^.buff[axis] then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(const SortBuffer: PKDT22DI64_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT22DI64_Source;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer^[(L + R) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, axis) < 0 do
            Inc(i);
        while SortCompare(SortBuffer^[j], p, axis) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, axis);
      L := i;
    until i >= R;
  end;

var
  M: NativeInt;
  axis: NativeInt;
  kdBuffPtr: PKDT22DI64_SourceBuffer;
begin
  Result := nil;
  if PlanCount = 0 then
      Exit;

  if PlanCount = 1 then
    begin
      new(Result);
      Result^.Parent := nil;
      Result^.Right := nil;
      Result^.Left := nil;
      Result^.vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);
    end
  else
    begin
      axis := Depth mod KDT22DI64_Axis;
      M := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr^[0], 0, PlanCount - 1, axis);

      new(Result);
      Result^.Parent := nil;
      Result^.vec := kdBuffPtr^[M];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], M, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[M + 1], PlanCount - (M + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDT22DI64.GetData(const Index: NativeInt): PKDT22DI64_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT22DI64.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT22DI64.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT22DI64.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT22DI64_Node(KDNodes[i]));
      Inc(i);
    end;

  for i := 0 to length(KDStoreBuff) - 1 do
      KDStoreBuff[i].Token := '';

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDT22DI64.StoreBuffPtr: PKDT22DI64_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT22DI64.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DI64_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT22DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT22DI64.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DI64_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT22DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT22DI64.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DI64_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT22DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT22DI64.BuildKDTreeWithCluster(const inBuff: TKDT22DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT22DI64_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT22DI64_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT22DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT22DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT22DI64.BuildKDTreeWithCluster(const inBuff: TKDT22DI64_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT22DI64.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DI64_BuildCall);
var
  TempStoreBuff: TKDT22DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT22DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT22DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT22DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT22DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT22DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT22DI64.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DI64_BuildMethod);
var
  TempStoreBuff: TKDT22DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT22DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT22DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT22DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT22DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT22DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$IFNDEF FPC}


procedure TKDT22DI64.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DI64_BuildProc);
var
  TempStoreBuff: TKDT22DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT22DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT22DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT22DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT22DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT22DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$ENDIF FPC}


function TKDT22DI64.Search(const buff: TKDT22DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT22DI64_Node;

var
  NearestNeighbour: PKDT22DI64_Node;

  function FindParentNode(const buffPtr: PKDT22DI64_Vec; NodePtr: PKDT22DI64_Node): PKDT22DI64_Node;
  var
    Next: PKDT22DI64_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT22DI64_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT22DI64_Node; const buffPtr: PKDT22DI64_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT22DI64Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT22DI64_Axis;
    Dist := NodePtr^.vec^.buff[axis] - buffPtr^[axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.vec^.buff[axis] > buffPtr^[axis] then
            ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes)
        else
            ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end
    else
      begin
        ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes);
        ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end;
  end;

  function SortCompare(const buffPtr: PKDT22DI64_Vec; const p1, p2: PKDT22DI64_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT22DI64Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT22DI64Distance(buffPtr^, p2^.vec^.buff);
    if d1 = d2 then
      begin
        if p1^.vec^.Index = p2^.vec^.Index then
            Result := 0
        else if p1^.vec^.Index < p2^.vec^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT22DI64_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT22DI64_Node;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer[(L + R) shr 1];
      repeat
        while SortCompare(buffPtr, SortBuffer[i], p) < 0 do
            Inc(i);
        while SortCompare(buffPtr, SortBuffer[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, buffPtr);
      L := i;
    until i >= R;
  end;

var
  Parent: PKDT22DI64_Node;
begin
  Result := nil;
  SearchedDistanceMin := 0;
  SearchedCounter := 0;
  NearestNeighbour := nil;
  if NearestNodes <> nil then
      NearestNodes.Clear;
  if RootNode = nil then
      Exit;
  if Count = 0 then
      Exit;

  Parent := FindParentNode(@buff[0], RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := KDT22DI64Distance(buff, Parent^.vec^.buff);

  ScanSubtree(RootNode, @buff[0], 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @buff[0]);

      if NearestNodes.Count > 0 then
          Result := PKDT22DI64_Node(NearestNodes[0]);
    end;
end;

function TKDT22DI64.Search(const buff: TKDT22DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT22DI64_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT22DI64.Search(const buff: TKDT22DI64_Vec; var SearchedDistanceMin: Double): PKDT22DI64_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT22DI64.Search(const buff: TKDT22DI64_Vec): PKDT22DI64_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT22DI64.SearchToken(const buff: TKDT22DI64_Vec): TPascalString;
var
  p: PKDT22DI64_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT22DI64.Search(const inBuff: TKDT22DI64_DynamicVecBuffer; var OutBuff: TKDT22DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT22DI64_DynamicVecBuffer;
  outBuffPtr: PKDT22DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT22DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outBuffPtr^[pass] := p^.vec^.buff;
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outBuffPtr := @OutBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT22DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outBuffPtr^[pass] := p^.vec^.buff;
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT22DI64_Node;
begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutBuff[i] := p^.vec^.buff;
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT22DI64.Search(const inBuff: TKDT22DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT22DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT22DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT22DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT22DI64_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT22DI64.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT22DI64_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT22DI64_Vec));
      stream.write(KDStoreBuff[i].Index, 8);
      token_B := KDStoreBuff[i].Token.Bytes;
      token_L := length(token_B);
      stream.write(token_L, 4);
      if token_L > 0 then
        begin
          stream.write(token_B[0], token_L);
          SetLength(token_B, 0);
        end;
      Inc(i);
    end;
end;

procedure TKDT22DI64.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  Clear;

  stream.read(st, 4);
  stream.read(ID, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if ID <> KDT22DI64_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT22DI64_Vec)) <> SizeOf(TKDT22DI64_Vec) then
          begin
            Clear;
            Exit;
          end;
        if stream.read(KDStoreBuff[i].Index, 8) <> 8 then
          begin
            Clear;
            Exit;
          end;
        if stream.read(token_L, 4) <> 4 then
          begin
            Clear;
            Exit;
          end;
        if token_L > 0 then
          begin
            SetLength(token_B, token_L);
            if stream.read(token_B[0], token_L) <> token_L then
              begin
                Clear;
                Exit;
              end;
            KDStoreBuff[i].Token.Bytes := token_B;
            SetLength(token_B, 0);
          end
        else
            KDStoreBuff[i].Token := '';
        Inc(i);
      end;
  except
    Clear;
    Exit;
  end;

  SetLength(KDBuff, cnt);
  SetLength(KDNodes, cnt);

  i := 0;
  while i < cnt do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      Inc(i);
    end;

  if cnt > 0 then
    RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDT22DI64.SaveToFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT22DI64.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT22DI64.PrintNodeTree(const NodePtr: PKDT22DI64_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT22DI64_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT22DI64Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT22DI64.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT22DI64Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT22DI64.KDT22DI64Vec(const s: SystemString): TKDT22DI64_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT22DI64_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToInt(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT22DI64_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT22DI64.KDT22DI64Vec(const v: TKDT22DI64_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT22DI64_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT22DI64.KDT22DI64Pow(const v: TKDT22DI64_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT22DI64.KDT22DI64Distance(const v1, v2: TKDT22DI64_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT22DI64_Axis - 1 do
      Result := Result + KDT22DI64Pow(v2[i] - v1[i]);
end;

procedure TKDT22DI64.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT22DI64_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT22DI64.Test;
var
  TKDT22DI64_Test: TKDT22DI64;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT22DI64_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT22DI64_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT22DI64_Test := TKDT22DI64.Create;

  DoStatusNoLn('...');
  SetLength(TKDT22DI64_Test.TestBuff, 1000);
  for i := 0 to length(TKDT22DI64_Test.TestBuff) - 1 do
    for j := 0 to KDT22DI64_Axis - 1 do
        TKDT22DI64_Test.TestBuff[i][j] := i * KDT22DI64_Axis + j;

{$IFDEF FPC}
  TKDT22DI64_Test.BuildKDTreeM(length(TKDT22DI64_Test.TestBuff), nil, @TKDT22DI64_Test.Test_BuildM);
{$ELSE FPC}
  TKDT22DI64_Test.BuildKDTreeM(length(TKDT22DI64_Test.TestBuff), nil, TKDT22DI64_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT22DI64_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT22DI64_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT22DI64_Test.TestBuff) - 1 do
    begin
      p := TKDT22DI64_Test.Search(TKDT22DI64_Test.TestBuff[i]);
      if p^.vec^.Index <> i then
          errored := True;
      if not p^.vec^.Token.Same(umlIntToStr(i)) then
          errored := True;
      if errored then
          Break;
    end;
  DisposeObject(m64);

  if not errored then
    begin
      { parallel search test }
      DoStatusNoLn('...parallel');
      SetLength(TestResultBuff, length(TKDT22DI64_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT22DI64_Test.TestBuff));
      TKDT22DI64_Test.Search(TKDT22DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT22DI64Distance(TKDT22DI64_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT22DI64_Test.Clear;
      { kMean test }
      TKDT22DI64_Test.BuildKDTreeWithCluster(TKDT22DI64_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT22DI64_Test.Search(TKDT22DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT22DI64_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT22DI64_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT22DI64_Test);
end;


function TKDT23DI64.InternalBuildKdTree(const KDSourceBufferPtr: PKDT23DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT23DI64_Node;
  function SortCompare(const p1, p2: PKDT23DI64_Source; const axis: NativeInt): ShortInt;
  begin
    if p1^.buff[axis] = p2^.buff[axis] then
      begin
        if p1^.Index = p2^.Index then
            Result := 0
        else if p1^.Index < p2^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.buff[axis] < p2^.buff[axis] then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(const SortBuffer: PKDT23DI64_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT23DI64_Source;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer^[(L + R) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, axis) < 0 do
            Inc(i);
        while SortCompare(SortBuffer^[j], p, axis) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, axis);
      L := i;
    until i >= R;
  end;

var
  M: NativeInt;
  axis: NativeInt;
  kdBuffPtr: PKDT23DI64_SourceBuffer;
begin
  Result := nil;
  if PlanCount = 0 then
      Exit;

  if PlanCount = 1 then
    begin
      new(Result);
      Result^.Parent := nil;
      Result^.Right := nil;
      Result^.Left := nil;
      Result^.vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);
    end
  else
    begin
      axis := Depth mod KDT23DI64_Axis;
      M := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr^[0], 0, PlanCount - 1, axis);

      new(Result);
      Result^.Parent := nil;
      Result^.vec := kdBuffPtr^[M];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], M, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[M + 1], PlanCount - (M + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDT23DI64.GetData(const Index: NativeInt): PKDT23DI64_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT23DI64.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT23DI64.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT23DI64.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT23DI64_Node(KDNodes[i]));
      Inc(i);
    end;

  for i := 0 to length(KDStoreBuff) - 1 do
      KDStoreBuff[i].Token := '';

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDT23DI64.StoreBuffPtr: PKDT23DI64_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT23DI64.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DI64_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT23DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT23DI64.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DI64_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT23DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT23DI64.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DI64_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT23DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT23DI64.BuildKDTreeWithCluster(const inBuff: TKDT23DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT23DI64_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT23DI64_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT23DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT23DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT23DI64.BuildKDTreeWithCluster(const inBuff: TKDT23DI64_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT23DI64.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DI64_BuildCall);
var
  TempStoreBuff: TKDT23DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT23DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT23DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT23DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT23DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT23DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT23DI64.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DI64_BuildMethod);
var
  TempStoreBuff: TKDT23DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT23DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT23DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT23DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT23DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT23DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$IFNDEF FPC}


procedure TKDT23DI64.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DI64_BuildProc);
var
  TempStoreBuff: TKDT23DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT23DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT23DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT23DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT23DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT23DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$ENDIF FPC}


function TKDT23DI64.Search(const buff: TKDT23DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT23DI64_Node;

var
  NearestNeighbour: PKDT23DI64_Node;

  function FindParentNode(const buffPtr: PKDT23DI64_Vec; NodePtr: PKDT23DI64_Node): PKDT23DI64_Node;
  var
    Next: PKDT23DI64_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT23DI64_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT23DI64_Node; const buffPtr: PKDT23DI64_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT23DI64Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT23DI64_Axis;
    Dist := NodePtr^.vec^.buff[axis] - buffPtr^[axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.vec^.buff[axis] > buffPtr^[axis] then
            ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes)
        else
            ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end
    else
      begin
        ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes);
        ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end;
  end;

  function SortCompare(const buffPtr: PKDT23DI64_Vec; const p1, p2: PKDT23DI64_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT23DI64Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT23DI64Distance(buffPtr^, p2^.vec^.buff);
    if d1 = d2 then
      begin
        if p1^.vec^.Index = p2^.vec^.Index then
            Result := 0
        else if p1^.vec^.Index < p2^.vec^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT23DI64_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT23DI64_Node;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer[(L + R) shr 1];
      repeat
        while SortCompare(buffPtr, SortBuffer[i], p) < 0 do
            Inc(i);
        while SortCompare(buffPtr, SortBuffer[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, buffPtr);
      L := i;
    until i >= R;
  end;

var
  Parent: PKDT23DI64_Node;
begin
  Result := nil;
  SearchedDistanceMin := 0;
  SearchedCounter := 0;
  NearestNeighbour := nil;
  if NearestNodes <> nil then
      NearestNodes.Clear;
  if RootNode = nil then
      Exit;
  if Count = 0 then
      Exit;

  Parent := FindParentNode(@buff[0], RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := KDT23DI64Distance(buff, Parent^.vec^.buff);

  ScanSubtree(RootNode, @buff[0], 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @buff[0]);

      if NearestNodes.Count > 0 then
          Result := PKDT23DI64_Node(NearestNodes[0]);
    end;
end;

function TKDT23DI64.Search(const buff: TKDT23DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT23DI64_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT23DI64.Search(const buff: TKDT23DI64_Vec; var SearchedDistanceMin: Double): PKDT23DI64_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT23DI64.Search(const buff: TKDT23DI64_Vec): PKDT23DI64_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT23DI64.SearchToken(const buff: TKDT23DI64_Vec): TPascalString;
var
  p: PKDT23DI64_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT23DI64.Search(const inBuff: TKDT23DI64_DynamicVecBuffer; var OutBuff: TKDT23DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT23DI64_DynamicVecBuffer;
  outBuffPtr: PKDT23DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT23DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outBuffPtr^[pass] := p^.vec^.buff;
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outBuffPtr := @OutBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT23DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outBuffPtr^[pass] := p^.vec^.buff;
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT23DI64_Node;
begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutBuff[i] := p^.vec^.buff;
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT23DI64.Search(const inBuff: TKDT23DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT23DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT23DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT23DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT23DI64_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT23DI64.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT23DI64_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT23DI64_Vec));
      stream.write(KDStoreBuff[i].Index, 8);
      token_B := KDStoreBuff[i].Token.Bytes;
      token_L := length(token_B);
      stream.write(token_L, 4);
      if token_L > 0 then
        begin
          stream.write(token_B[0], token_L);
          SetLength(token_B, 0);
        end;
      Inc(i);
    end;
end;

procedure TKDT23DI64.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  Clear;

  stream.read(st, 4);
  stream.read(ID, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if ID <> KDT23DI64_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT23DI64_Vec)) <> SizeOf(TKDT23DI64_Vec) then
          begin
            Clear;
            Exit;
          end;
        if stream.read(KDStoreBuff[i].Index, 8) <> 8 then
          begin
            Clear;
            Exit;
          end;
        if stream.read(token_L, 4) <> 4 then
          begin
            Clear;
            Exit;
          end;
        if token_L > 0 then
          begin
            SetLength(token_B, token_L);
            if stream.read(token_B[0], token_L) <> token_L then
              begin
                Clear;
                Exit;
              end;
            KDStoreBuff[i].Token.Bytes := token_B;
            SetLength(token_B, 0);
          end
        else
            KDStoreBuff[i].Token := '';
        Inc(i);
      end;
  except
    Clear;
    Exit;
  end;

  SetLength(KDBuff, cnt);
  SetLength(KDNodes, cnt);

  i := 0;
  while i < cnt do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      Inc(i);
    end;

  if cnt > 0 then
    RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDT23DI64.SaveToFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT23DI64.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT23DI64.PrintNodeTree(const NodePtr: PKDT23DI64_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT23DI64_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT23DI64Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT23DI64.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT23DI64Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT23DI64.KDT23DI64Vec(const s: SystemString): TKDT23DI64_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT23DI64_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToInt(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT23DI64_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT23DI64.KDT23DI64Vec(const v: TKDT23DI64_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT23DI64_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT23DI64.KDT23DI64Pow(const v: TKDT23DI64_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT23DI64.KDT23DI64Distance(const v1, v2: TKDT23DI64_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT23DI64_Axis - 1 do
      Result := Result + KDT23DI64Pow(v2[i] - v1[i]);
end;

procedure TKDT23DI64.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT23DI64_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT23DI64.Test;
var
  TKDT23DI64_Test: TKDT23DI64;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT23DI64_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT23DI64_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT23DI64_Test := TKDT23DI64.Create;

  DoStatusNoLn('...');
  SetLength(TKDT23DI64_Test.TestBuff, 1000);
  for i := 0 to length(TKDT23DI64_Test.TestBuff) - 1 do
    for j := 0 to KDT23DI64_Axis - 1 do
        TKDT23DI64_Test.TestBuff[i][j] := i * KDT23DI64_Axis + j;

{$IFDEF FPC}
  TKDT23DI64_Test.BuildKDTreeM(length(TKDT23DI64_Test.TestBuff), nil, @TKDT23DI64_Test.Test_BuildM);
{$ELSE FPC}
  TKDT23DI64_Test.BuildKDTreeM(length(TKDT23DI64_Test.TestBuff), nil, TKDT23DI64_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT23DI64_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT23DI64_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT23DI64_Test.TestBuff) - 1 do
    begin
      p := TKDT23DI64_Test.Search(TKDT23DI64_Test.TestBuff[i]);
      if p^.vec^.Index <> i then
          errored := True;
      if not p^.vec^.Token.Same(umlIntToStr(i)) then
          errored := True;
      if errored then
          Break;
    end;
  DisposeObject(m64);

  if not errored then
    begin
      { parallel search test }
      DoStatusNoLn('...parallel');
      SetLength(TestResultBuff, length(TKDT23DI64_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT23DI64_Test.TestBuff));
      TKDT23DI64_Test.Search(TKDT23DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT23DI64Distance(TKDT23DI64_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT23DI64_Test.Clear;
      { kMean test }
      TKDT23DI64_Test.BuildKDTreeWithCluster(TKDT23DI64_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT23DI64_Test.Search(TKDT23DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT23DI64_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT23DI64_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT23DI64_Test);
end;


function TKDT24DI64.InternalBuildKdTree(const KDSourceBufferPtr: PKDT24DI64_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT24DI64_Node;
  function SortCompare(const p1, p2: PKDT24DI64_Source; const axis: NativeInt): ShortInt;
  begin
    if p1^.buff[axis] = p2^.buff[axis] then
      begin
        if p1^.Index = p2^.Index then
            Result := 0
        else if p1^.Index < p2^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.buff[axis] < p2^.buff[axis] then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(const SortBuffer: PKDT24DI64_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT24DI64_Source;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer^[(L + R) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, axis) < 0 do
            Inc(i);
        while SortCompare(SortBuffer^[j], p, axis) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, axis);
      L := i;
    until i >= R;
  end;

var
  M: NativeInt;
  axis: NativeInt;
  kdBuffPtr: PKDT24DI64_SourceBuffer;
begin
  Result := nil;
  if PlanCount = 0 then
      Exit;

  if PlanCount = 1 then
    begin
      new(Result);
      Result^.Parent := nil;
      Result^.Right := nil;
      Result^.Left := nil;
      Result^.vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);
    end
  else
    begin
      axis := Depth mod KDT24DI64_Axis;
      M := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr^[0], 0, PlanCount - 1, axis);

      new(Result);
      Result^.Parent := nil;
      Result^.vec := kdBuffPtr^[M];

      KDNodes[NodeCounter] := Result;
      Inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], M, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[M + 1], PlanCount - (M + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDT24DI64.GetData(const Index: NativeInt): PKDT24DI64_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT24DI64.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT24DI64.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT24DI64.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT24DI64_Node(KDNodes[i]));
      Inc(i);
    end;

  for i := 0 to length(KDStoreBuff) - 1 do
      KDStoreBuff[i].Token := '';

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDT24DI64.StoreBuffPtr: PKDT24DI64_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT24DI64.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DI64_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT24DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT24DI64.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DI64_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT24DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT24DI64.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DI64_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT24DI64_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT24DI64.BuildKDTreeWithCluster(const inBuff: TKDT24DI64_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT24DI64_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT24DI64_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT24DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT24DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT24DI64.BuildKDTreeWithCluster(const inBuff: TKDT24DI64_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT24DI64.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DI64_BuildCall);
var
  TempStoreBuff: TKDT24DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT24DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT24DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT24DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT24DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT24DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT24DI64.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DI64_BuildMethod);
var
  TempStoreBuff: TKDT24DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT24DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT24DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT24DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT24DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT24DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$IFNDEF FPC}


procedure TKDT24DI64.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DI64_BuildProc);
var
  TempStoreBuff: TKDT24DI64_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT24DI64_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT24DI64_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT24DI64_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT24DI64_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT24DI64_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

{$ENDIF FPC}


function TKDT24DI64.Search(const buff: TKDT24DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT24DI64_Node;

var
  NearestNeighbour: PKDT24DI64_Node;

  function FindParentNode(const buffPtr: PKDT24DI64_Vec; NodePtr: PKDT24DI64_Node): PKDT24DI64_Node;
  var
    Next: PKDT24DI64_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT24DI64_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT24DI64_Node; const buffPtr: PKDT24DI64_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT24DI64Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT24DI64_Axis;
    Dist := NodePtr^.vec^.buff[axis] - buffPtr^[axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.vec^.buff[axis] > buffPtr^[axis] then
            ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes)
        else
            ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end
    else
      begin
        ScanSubtree(NodePtr^.Left, buffPtr, Depth + 1, NearestNodes);
        ScanSubtree(NodePtr^.Right, buffPtr, Depth + 1, NearestNodes);
      end;
  end;

  function SortCompare(const buffPtr: PKDT24DI64_Vec; const p1, p2: PKDT24DI64_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT24DI64Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT24DI64Distance(buffPtr^, p2^.vec^.buff);
    if d1 = d2 then
      begin
        if p1^.vec^.Index = p2^.vec^.Index then
            Result := 0
        else if p1^.vec^.Index < p2^.vec^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT24DI64_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT24DI64_Node;
  begin
    repeat
      i := L;
      j := R;
      p := SortBuffer[(L + R) shr 1];
      repeat
        while SortCompare(buffPtr, SortBuffer[i], p) < 0 do
            Inc(i);
        while SortCompare(buffPtr, SortBuffer[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, buffPtr);
      L := i;
    until i >= R;
  end;

var
  Parent: PKDT24DI64_Node;
begin
  Result := nil;
  SearchedDistanceMin := 0;
  SearchedCounter := 0;
  NearestNeighbour := nil;
  if NearestNodes <> nil then
      NearestNodes.Clear;
  if RootNode = nil then
      Exit;
  if Count = 0 then
      Exit;

  Parent := FindParentNode(@buff[0], RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := KDT24DI64Distance(buff, Parent^.vec^.buff);

  ScanSubtree(RootNode, @buff[0], 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @buff[0]);

      if NearestNodes.Count > 0 then
          Result := PKDT24DI64_Node(NearestNodes[0]);
    end;
end;

function TKDT24DI64.Search(const buff: TKDT24DI64_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT24DI64_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT24DI64.Search(const buff: TKDT24DI64_Vec; var SearchedDistanceMin: Double): PKDT24DI64_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT24DI64.Search(const buff: TKDT24DI64_Vec): PKDT24DI64_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT24DI64.SearchToken(const buff: TKDT24DI64_Vec): TPascalString;
var
  p: PKDT24DI64_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT24DI64.Search(const inBuff: TKDT24DI64_DynamicVecBuffer; var OutBuff: TKDT24DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT24DI64_DynamicVecBuffer;
  outBuffPtr: PKDT24DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT24DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outBuffPtr^[pass] := p^.vec^.buff;
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outBuffPtr := @OutBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT24DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outBuffPtr^[pass] := p^.vec^.buff;
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT24DI64_Node;
begin
  if length(OutBuff) <> length(OutIndex) then
      Exit;
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutBuff[i] := p^.vec^.buff;
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT24DI64.Search(const inBuff: TKDT24DI64_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT24DI64_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT24DI64_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outIndexPtr^[pass] := p^.vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook := False;
  try
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, PtrInt(length(inBuff) - 1));
{$ELSE FPC}
    TParallel.for(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDT24DI64_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outIndexPtr^[pass] := p^.vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook := True;
  end;
end;

{$ELSE parallel}


var
  i: NativeInt;
  p: PKDT24DI64_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.vec^.Index;
    end;
end;
{$ENDIF parallel}


procedure TKDT24DI64.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT24DI64_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT24DI64_Vec));
      stream.write(KDStoreBuff[i].Index, 8);
      token_B := KDStoreBuff[i].Token.Bytes;
      token_L := length(token_B);
      stream.write(token_L, 4);
      if token_L > 0 then
        begin
          stream.write(token_B[0], token_L);
          SetLength(token_B, 0);
        end;
      Inc(i);
    end;
end;

procedure TKDT24DI64.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  Clear;

  stream.read(st, 4);
  stream.read(ID, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if ID <> KDT24DI64_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT24DI64_Vec)) <> SizeOf(TKDT24DI64_Vec) then
          begin
            Clear;
            Exit;
          end;
        if stream.read(KDStoreBuff[i].Index, 8) <> 8 then
          begin
            Clear;
            Exit;
          end;
        if stream.read(token_L, 4) <> 4 then
          begin
            Clear;
            Exit;
          end;
        if token_L > 0 then
          begin
            SetLength(token_B, token_L);
            if stream.read(token_B[0], token_L) <> token_L then
              begin
                Clear;
                Exit;
              end;
            KDStoreBuff[i].Token.Bytes := token_B;
            SetLength(token_B, 0);
          end
        else
            KDStoreBuff[i].Token := '';
        Inc(i);
      end;
  except
    Clear;
    Exit;
  end;

  SetLength(KDBuff, cnt);
  SetLength(KDNodes, cnt);

  i := 0;
  while i < cnt do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      Inc(i);
    end;

  if cnt > 0 then
    RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDT24DI64.SaveToFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT24DI64.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDT24DI64.PrintNodeTree(const NodePtr: PKDT24DI64_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT24DI64_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT24DI64Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT24DI64.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT24DI64Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT24DI64.KDT24DI64Vec(const s: SystemString): TKDT24DI64_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT24DI64_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToInt(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT24DI64_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT24DI64.KDT24DI64Vec(const v: TKDT24DI64_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT24DI64_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT24DI64.KDT24DI64Pow(const v: TKDT24DI64_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT24DI64.KDT24DI64Distance(const v1, v2: TKDT24DI64_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT24DI64_Axis - 1 do
      Result := Result + KDT24DI64Pow(v2[i] - v1[i]);
end;

procedure TKDT24DI64.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT24DI64_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT24DI64.Test;
var
  TKDT24DI64_Test: TKDT24DI64;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT24DI64_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT24DI64_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT24DI64_Test := TKDT24DI64.Create;

  DoStatusNoLn('...');
  SetLength(TKDT24DI64_Test.TestBuff, 1000);
  for i := 0 to length(TKDT24DI64_Test.TestBuff) - 1 do
    for j := 0 to KDT24DI64_Axis - 1 do
        TKDT24DI64_Test.TestBuff[i][j] := i * KDT24DI64_Axis + j;

{$IFDEF FPC}
  TKDT24DI64_Test.BuildKDTreeM(length(TKDT24DI64_Test.TestBuff), nil, @TKDT24DI64_Test.Test_BuildM);
{$ELSE FPC}
  TKDT24DI64_Test.BuildKDTreeM(length(TKDT24DI64_Test.TestBuff), nil, TKDT24DI64_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT24DI64_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT24DI64_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT24DI64_Test.TestBuff) - 1 do
    begin
      p := TKDT24DI64_Test.Search(TKDT24DI64_Test.TestBuff[i]);
      if p^.vec^.Index <> i then
          errored := True;
      if not p^.vec^.Token.Same(umlIntToStr(i)) then
          errored := True;
      if errored then
          Break;
    end;
  DisposeObject(m64);

  if not errored then
    begin
      { parallel search test }
      DoStatusNoLn('...parallel');
      SetLength(TestResultBuff, length(TKDT24DI64_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT24DI64_Test.TestBuff));
      TKDT24DI64_Test.Search(TKDT24DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT24DI64Distance(TKDT24DI64_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT24DI64_Test.Clear;
      { kMean test }
      TKDT24DI64_Test.BuildKDTreeWithCluster(TKDT24DI64_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT24DI64_Test.Search(TKDT24DI64_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT24DI64_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT24DI64_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT24DI64_Test);
end;


procedure Test_All;
begin
  TKDT1DI64.Test();
  TKDT2DI64.Test();
  TKDT3DI64.Test();
  TKDT4DI64.Test();
  TKDT5DI64.Test();
  TKDT6DI64.Test();
  TKDT7DI64.Test();
  TKDT8DI64.Test();
  TKDT9DI64.Test();
  TKDT10DI64.Test();
  TKDT11DI64.Test();
  TKDT12DI64.Test();
  TKDT13DI64.Test();
  TKDT14DI64.Test();
  TKDT15DI64.Test();
  TKDT16DI64.Test();
  TKDT17DI64.Test();
  TKDT18DI64.Test();
  TKDT19DI64.Test();
  TKDT20DI64.Test();
  TKDT21DI64.Test();
  TKDT22DI64.Test();
  TKDT23DI64.Test();
  TKDT24DI64.Test();
end;





initialization

finalization

end.

