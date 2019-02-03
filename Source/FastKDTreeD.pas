{ Fast KDTree Double Type support                                                }
{ ****************************************************************************** }
{ * fast KDTree Support,writen by QQ 600585@qq.com                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }

unit FastKDTreeD;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, PascalStrings, UnicodeMixedLib, KM;

const

  // Double float: KDTree
  KDT1DD_Axis = 1;
  KDT2DD_Axis = 2;
  KDT3DD_Axis = 3;
  KDT4DD_Axis = 4;
  KDT5DD_Axis = 5;
  KDT6DD_Axis = 6;
  KDT7DD_Axis = 7;
  KDT8DD_Axis = 8;
  KDT9DD_Axis = 9;
  KDT10DD_Axis = 10;
  KDT11DD_Axis = 11;
  KDT12DD_Axis = 12;
  KDT13DD_Axis = 13;
  KDT14DD_Axis = 14;
  KDT15DD_Axis = 15;
  KDT16DD_Axis = 16;
  KDT17DD_Axis = 17;
  KDT18DD_Axis = 18;
  KDT19DD_Axis = 19;
  KDT20DD_Axis = 20;
  KDT21DD_Axis = 21;
  KDT22DD_Axis = 22;
  KDT23DD_Axis = 23;
  KDT24DD_Axis = 24;

type

  // Double float: KDTree
  TKDT1DD = class;  TKDT1DD_VecType = KM.TKMFloat; // 1D
  TKDT2DD = class;  TKDT2DD_VecType = KM.TKMFloat; // 2D
  TKDT3DD = class;  TKDT3DD_VecType = KM.TKMFloat; // 3D
  TKDT4DD = class;  TKDT4DD_VecType = KM.TKMFloat; // 4D
  TKDT5DD = class;  TKDT5DD_VecType = KM.TKMFloat; // 5D
  TKDT6DD = class;  TKDT6DD_VecType = KM.TKMFloat; // 6D
  TKDT7DD = class;  TKDT7DD_VecType = KM.TKMFloat; // 7D
  TKDT8DD = class;  TKDT8DD_VecType = KM.TKMFloat; // 8D
  TKDT9DD = class;  TKDT9DD_VecType = KM.TKMFloat; // 9D
  TKDT10DD = class;  TKDT10DD_VecType = KM.TKMFloat; // 10D
  TKDT11DD = class;  TKDT11DD_VecType = KM.TKMFloat; // 11D
  TKDT12DD = class;  TKDT12DD_VecType = KM.TKMFloat; // 12D
  TKDT13DD = class;  TKDT13DD_VecType = KM.TKMFloat; // 13D
  TKDT14DD = class;  TKDT14DD_VecType = KM.TKMFloat; // 14D
  TKDT15DD = class;  TKDT15DD_VecType = KM.TKMFloat; // 15D
  TKDT16DD = class;  TKDT16DD_VecType = KM.TKMFloat; // 16D
  TKDT17DD = class;  TKDT17DD_VecType = KM.TKMFloat; // 17D
  TKDT18DD = class;  TKDT18DD_VecType = KM.TKMFloat; // 18D
  TKDT19DD = class;  TKDT19DD_VecType = KM.TKMFloat; // 19D
  TKDT20DD = class;  TKDT20DD_VecType = KM.TKMFloat; // 20D
  TKDT21DD = class;  TKDT21DD_VecType = KM.TKMFloat; // 21D
  TKDT22DD = class;  TKDT22DD_VecType = KM.TKMFloat; // 22D
  TKDT23DD = class;  TKDT23DD_VecType = KM.TKMFloat; // 23D
  TKDT24DD = class;  TKDT24DD_VecType = KM.TKMFloat; // 24D





  // Double float: KDTree


  TKDT1DD = class(TCoreClassObject)
  public type
    // code split
    TKDT1DD_Vec = array [0 .. KDT1DD_Axis - 1] of TKDT1DD_VecType;
    PKDT1DD_Vec = ^TKDT1DD_Vec;

    TKDT1DD_DynamicVecBuffer = array of TKDT1DD_Vec;
    PKDT1DD_DynamicVecBuffer = ^TKDT1DD_DynamicVecBuffer;

    TKDT1DD_Source = record
      buff: TKDT1DD_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT1DD_Source = ^TKDT1DD_Source;
    TKDT1DD_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT1DD_Source) - 1] of PKDT1DD_Source;
    PKDT1DD_SourceBuffer = ^TKDT1DD_SourceBuffer;

    TKDT1DD_DyanmicSourceBuffer = array of PKDT1DD_Source;
    PKDT1DD_DyanmicSourceBuffer = ^TKDT1DD_DyanmicSourceBuffer;

    TKDT1DD_DyanmicStoreBuffer = array of TKDT1DD_Source;
    PKDT1DD_DyanmicStoreBuffer = ^TKDT1DD_DyanmicStoreBuffer;

    PKDT1DD_Node = ^TKDT1DD_Node;

    TKDT1DD_Node = record
      Parent, Right, Left: PKDT1DD_Node;
      vec: PKDT1DD_Source;
    end;

    TKDT1DD_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT1DD_Source; const Data: Pointer);
    TKDT1DD_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT1DD_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT1DD_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT1DD_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT1DD_DyanmicStoreBuffer;
    KDBuff: TKDT1DD_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT1DD_Node;
    TestBuff: TKDT1DD_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT1DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT1DD_Node;
    function GetData(const Index: NativeInt): PKDT1DD_Source;
  public
    RootNode: PKDT1DD_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT1DD_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT1DD_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DD_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DD_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DD_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT1DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT1DD_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DD_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DD_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DD_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT1DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT1DD_Node; overload;
    function Search(const buff: TKDT1DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT1DD_Node; overload;
    function Search(const buff: TKDT1DD_Vec; var SearchedDistanceMin: Double): PKDT1DD_Node; overload;
    function Search(const buff: TKDT1DD_Vec): PKDT1DD_Node; overload;
    function SearchToken(const buff: TKDT1DD_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT1DD_DynamicVecBuffer; var OutBuff: TKDT1DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT1DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT1DD_Node);
    procedure PrintBuffer;

    class function KDT1DDVec(const s: SystemString): TKDT1DD_Vec; overload;
    class function KDT1DDVec(const v: TKDT1DD_Vec): SystemString; overload;
    class function KDT1DDPow(const v: TKDT1DD_VecType): Double;
    class function KDT1DDDistance(const v1, v2: TKDT1DD_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT1DD_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT2DD = class(TCoreClassObject)
  public type
    // code split
    TKDT2DD_Vec = array [0 .. KDT2DD_Axis - 1] of TKDT2DD_VecType;
    PKDT2DD_Vec = ^TKDT2DD_Vec;

    TKDT2DD_DynamicVecBuffer = array of TKDT2DD_Vec;
    PKDT2DD_DynamicVecBuffer = ^TKDT2DD_DynamicVecBuffer;

    TKDT2DD_Source = record
      buff: TKDT2DD_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT2DD_Source = ^TKDT2DD_Source;
    TKDT2DD_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT2DD_Source) - 1] of PKDT2DD_Source;
    PKDT2DD_SourceBuffer = ^TKDT2DD_SourceBuffer;

    TKDT2DD_DyanmicSourceBuffer = array of PKDT2DD_Source;
    PKDT2DD_DyanmicSourceBuffer = ^TKDT2DD_DyanmicSourceBuffer;

    TKDT2DD_DyanmicStoreBuffer = array of TKDT2DD_Source;
    PKDT2DD_DyanmicStoreBuffer = ^TKDT2DD_DyanmicStoreBuffer;

    PKDT2DD_Node = ^TKDT2DD_Node;

    TKDT2DD_Node = record
      Parent, Right, Left: PKDT2DD_Node;
      vec: PKDT2DD_Source;
    end;

    TKDT2DD_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT2DD_Source; const Data: Pointer);
    TKDT2DD_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT2DD_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT2DD_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT2DD_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT2DD_DyanmicStoreBuffer;
    KDBuff: TKDT2DD_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT2DD_Node;
    TestBuff: TKDT2DD_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT2DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT2DD_Node;
    function GetData(const Index: NativeInt): PKDT2DD_Source;
  public
    RootNode: PKDT2DD_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT2DD_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT2DD_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DD_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DD_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DD_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT2DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT2DD_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DD_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DD_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DD_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT2DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT2DD_Node; overload;
    function Search(const buff: TKDT2DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT2DD_Node; overload;
    function Search(const buff: TKDT2DD_Vec; var SearchedDistanceMin: Double): PKDT2DD_Node; overload;
    function Search(const buff: TKDT2DD_Vec): PKDT2DD_Node; overload;
    function SearchToken(const buff: TKDT2DD_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT2DD_DynamicVecBuffer; var OutBuff: TKDT2DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT2DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT2DD_Node);
    procedure PrintBuffer;

    class function KDT2DDVec(const s: SystemString): TKDT2DD_Vec; overload;
    class function KDT2DDVec(const v: TKDT2DD_Vec): SystemString; overload;
    class function KDT2DDPow(const v: TKDT2DD_VecType): Double;
    class function KDT2DDDistance(const v1, v2: TKDT2DD_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT2DD_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT3DD = class(TCoreClassObject)
  public type
    // code split
    TKDT3DD_Vec = array [0 .. KDT3DD_Axis - 1] of TKDT3DD_VecType;
    PKDT3DD_Vec = ^TKDT3DD_Vec;

    TKDT3DD_DynamicVecBuffer = array of TKDT3DD_Vec;
    PKDT3DD_DynamicVecBuffer = ^TKDT3DD_DynamicVecBuffer;

    TKDT3DD_Source = record
      buff: TKDT3DD_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT3DD_Source = ^TKDT3DD_Source;
    TKDT3DD_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT3DD_Source) - 1] of PKDT3DD_Source;
    PKDT3DD_SourceBuffer = ^TKDT3DD_SourceBuffer;

    TKDT3DD_DyanmicSourceBuffer = array of PKDT3DD_Source;
    PKDT3DD_DyanmicSourceBuffer = ^TKDT3DD_DyanmicSourceBuffer;

    TKDT3DD_DyanmicStoreBuffer = array of TKDT3DD_Source;
    PKDT3DD_DyanmicStoreBuffer = ^TKDT3DD_DyanmicStoreBuffer;

    PKDT3DD_Node = ^TKDT3DD_Node;

    TKDT3DD_Node = record
      Parent, Right, Left: PKDT3DD_Node;
      vec: PKDT3DD_Source;
    end;

    TKDT3DD_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT3DD_Source; const Data: Pointer);
    TKDT3DD_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT3DD_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT3DD_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT3DD_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT3DD_DyanmicStoreBuffer;
    KDBuff: TKDT3DD_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT3DD_Node;
    TestBuff: TKDT3DD_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT3DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT3DD_Node;
    function GetData(const Index: NativeInt): PKDT3DD_Source;
  public
    RootNode: PKDT3DD_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT3DD_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT3DD_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DD_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DD_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DD_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT3DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT3DD_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DD_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DD_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DD_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT3DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT3DD_Node; overload;
    function Search(const buff: TKDT3DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT3DD_Node; overload;
    function Search(const buff: TKDT3DD_Vec; var SearchedDistanceMin: Double): PKDT3DD_Node; overload;
    function Search(const buff: TKDT3DD_Vec): PKDT3DD_Node; overload;
    function SearchToken(const buff: TKDT3DD_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT3DD_DynamicVecBuffer; var OutBuff: TKDT3DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT3DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT3DD_Node);
    procedure PrintBuffer;

    class function KDT3DDVec(const s: SystemString): TKDT3DD_Vec; overload;
    class function KDT3DDVec(const v: TKDT3DD_Vec): SystemString; overload;
    class function KDT3DDPow(const v: TKDT3DD_VecType): Double;
    class function KDT3DDDistance(const v1, v2: TKDT3DD_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT3DD_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT4DD = class(TCoreClassObject)
  public type
    // code split
    TKDT4DD_Vec = array [0 .. KDT4DD_Axis - 1] of TKDT4DD_VecType;
    PKDT4DD_Vec = ^TKDT4DD_Vec;

    TKDT4DD_DynamicVecBuffer = array of TKDT4DD_Vec;
    PKDT4DD_DynamicVecBuffer = ^TKDT4DD_DynamicVecBuffer;

    TKDT4DD_Source = record
      buff: TKDT4DD_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT4DD_Source = ^TKDT4DD_Source;
    TKDT4DD_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT4DD_Source) - 1] of PKDT4DD_Source;
    PKDT4DD_SourceBuffer = ^TKDT4DD_SourceBuffer;

    TKDT4DD_DyanmicSourceBuffer = array of PKDT4DD_Source;
    PKDT4DD_DyanmicSourceBuffer = ^TKDT4DD_DyanmicSourceBuffer;

    TKDT4DD_DyanmicStoreBuffer = array of TKDT4DD_Source;
    PKDT4DD_DyanmicStoreBuffer = ^TKDT4DD_DyanmicStoreBuffer;

    PKDT4DD_Node = ^TKDT4DD_Node;

    TKDT4DD_Node = record
      Parent, Right, Left: PKDT4DD_Node;
      vec: PKDT4DD_Source;
    end;

    TKDT4DD_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT4DD_Source; const Data: Pointer);
    TKDT4DD_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT4DD_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT4DD_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT4DD_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT4DD_DyanmicStoreBuffer;
    KDBuff: TKDT4DD_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT4DD_Node;
    TestBuff: TKDT4DD_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT4DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT4DD_Node;
    function GetData(const Index: NativeInt): PKDT4DD_Source;
  public
    RootNode: PKDT4DD_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT4DD_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT4DD_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DD_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DD_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DD_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT4DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT4DD_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DD_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DD_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DD_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT4DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT4DD_Node; overload;
    function Search(const buff: TKDT4DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT4DD_Node; overload;
    function Search(const buff: TKDT4DD_Vec; var SearchedDistanceMin: Double): PKDT4DD_Node; overload;
    function Search(const buff: TKDT4DD_Vec): PKDT4DD_Node; overload;
    function SearchToken(const buff: TKDT4DD_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT4DD_DynamicVecBuffer; var OutBuff: TKDT4DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT4DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT4DD_Node);
    procedure PrintBuffer;

    class function KDT4DDVec(const s: SystemString): TKDT4DD_Vec; overload;
    class function KDT4DDVec(const v: TKDT4DD_Vec): SystemString; overload;
    class function KDT4DDPow(const v: TKDT4DD_VecType): Double;
    class function KDT4DDDistance(const v1, v2: TKDT4DD_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT4DD_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT5DD = class(TCoreClassObject)
  public type
    // code split
    TKDT5DD_Vec = array [0 .. KDT5DD_Axis - 1] of TKDT5DD_VecType;
    PKDT5DD_Vec = ^TKDT5DD_Vec;

    TKDT5DD_DynamicVecBuffer = array of TKDT5DD_Vec;
    PKDT5DD_DynamicVecBuffer = ^TKDT5DD_DynamicVecBuffer;

    TKDT5DD_Source = record
      buff: TKDT5DD_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT5DD_Source = ^TKDT5DD_Source;
    TKDT5DD_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT5DD_Source) - 1] of PKDT5DD_Source;
    PKDT5DD_SourceBuffer = ^TKDT5DD_SourceBuffer;

    TKDT5DD_DyanmicSourceBuffer = array of PKDT5DD_Source;
    PKDT5DD_DyanmicSourceBuffer = ^TKDT5DD_DyanmicSourceBuffer;

    TKDT5DD_DyanmicStoreBuffer = array of TKDT5DD_Source;
    PKDT5DD_DyanmicStoreBuffer = ^TKDT5DD_DyanmicStoreBuffer;

    PKDT5DD_Node = ^TKDT5DD_Node;

    TKDT5DD_Node = record
      Parent, Right, Left: PKDT5DD_Node;
      vec: PKDT5DD_Source;
    end;

    TKDT5DD_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT5DD_Source; const Data: Pointer);
    TKDT5DD_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT5DD_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT5DD_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT5DD_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT5DD_DyanmicStoreBuffer;
    KDBuff: TKDT5DD_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT5DD_Node;
    TestBuff: TKDT5DD_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT5DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT5DD_Node;
    function GetData(const Index: NativeInt): PKDT5DD_Source;
  public
    RootNode: PKDT5DD_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT5DD_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT5DD_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DD_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DD_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DD_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT5DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT5DD_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DD_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DD_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DD_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT5DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT5DD_Node; overload;
    function Search(const buff: TKDT5DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT5DD_Node; overload;
    function Search(const buff: TKDT5DD_Vec; var SearchedDistanceMin: Double): PKDT5DD_Node; overload;
    function Search(const buff: TKDT5DD_Vec): PKDT5DD_Node; overload;
    function SearchToken(const buff: TKDT5DD_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT5DD_DynamicVecBuffer; var OutBuff: TKDT5DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT5DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT5DD_Node);
    procedure PrintBuffer;

    class function KDT5DDVec(const s: SystemString): TKDT5DD_Vec; overload;
    class function KDT5DDVec(const v: TKDT5DD_Vec): SystemString; overload;
    class function KDT5DDPow(const v: TKDT5DD_VecType): Double;
    class function KDT5DDDistance(const v1, v2: TKDT5DD_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT5DD_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT6DD = class(TCoreClassObject)
  public type
    // code split
    TKDT6DD_Vec = array [0 .. KDT6DD_Axis - 1] of TKDT6DD_VecType;
    PKDT6DD_Vec = ^TKDT6DD_Vec;

    TKDT6DD_DynamicVecBuffer = array of TKDT6DD_Vec;
    PKDT6DD_DynamicVecBuffer = ^TKDT6DD_DynamicVecBuffer;

    TKDT6DD_Source = record
      buff: TKDT6DD_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT6DD_Source = ^TKDT6DD_Source;
    TKDT6DD_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT6DD_Source) - 1] of PKDT6DD_Source;
    PKDT6DD_SourceBuffer = ^TKDT6DD_SourceBuffer;

    TKDT6DD_DyanmicSourceBuffer = array of PKDT6DD_Source;
    PKDT6DD_DyanmicSourceBuffer = ^TKDT6DD_DyanmicSourceBuffer;

    TKDT6DD_DyanmicStoreBuffer = array of TKDT6DD_Source;
    PKDT6DD_DyanmicStoreBuffer = ^TKDT6DD_DyanmicStoreBuffer;

    PKDT6DD_Node = ^TKDT6DD_Node;

    TKDT6DD_Node = record
      Parent, Right, Left: PKDT6DD_Node;
      vec: PKDT6DD_Source;
    end;

    TKDT6DD_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT6DD_Source; const Data: Pointer);
    TKDT6DD_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT6DD_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT6DD_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT6DD_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT6DD_DyanmicStoreBuffer;
    KDBuff: TKDT6DD_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT6DD_Node;
    TestBuff: TKDT6DD_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT6DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT6DD_Node;
    function GetData(const Index: NativeInt): PKDT6DD_Source;
  public
    RootNode: PKDT6DD_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT6DD_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT6DD_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DD_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DD_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DD_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT6DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT6DD_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DD_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DD_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DD_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT6DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT6DD_Node; overload;
    function Search(const buff: TKDT6DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT6DD_Node; overload;
    function Search(const buff: TKDT6DD_Vec; var SearchedDistanceMin: Double): PKDT6DD_Node; overload;
    function Search(const buff: TKDT6DD_Vec): PKDT6DD_Node; overload;
    function SearchToken(const buff: TKDT6DD_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT6DD_DynamicVecBuffer; var OutBuff: TKDT6DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT6DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT6DD_Node);
    procedure PrintBuffer;

    class function KDT6DDVec(const s: SystemString): TKDT6DD_Vec; overload;
    class function KDT6DDVec(const v: TKDT6DD_Vec): SystemString; overload;
    class function KDT6DDPow(const v: TKDT6DD_VecType): Double;
    class function KDT6DDDistance(const v1, v2: TKDT6DD_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT6DD_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT7DD = class(TCoreClassObject)
  public type
    // code split
    TKDT7DD_Vec = array [0 .. KDT7DD_Axis - 1] of TKDT7DD_VecType;
    PKDT7DD_Vec = ^TKDT7DD_Vec;

    TKDT7DD_DynamicVecBuffer = array of TKDT7DD_Vec;
    PKDT7DD_DynamicVecBuffer = ^TKDT7DD_DynamicVecBuffer;

    TKDT7DD_Source = record
      buff: TKDT7DD_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT7DD_Source = ^TKDT7DD_Source;
    TKDT7DD_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT7DD_Source) - 1] of PKDT7DD_Source;
    PKDT7DD_SourceBuffer = ^TKDT7DD_SourceBuffer;

    TKDT7DD_DyanmicSourceBuffer = array of PKDT7DD_Source;
    PKDT7DD_DyanmicSourceBuffer = ^TKDT7DD_DyanmicSourceBuffer;

    TKDT7DD_DyanmicStoreBuffer = array of TKDT7DD_Source;
    PKDT7DD_DyanmicStoreBuffer = ^TKDT7DD_DyanmicStoreBuffer;

    PKDT7DD_Node = ^TKDT7DD_Node;

    TKDT7DD_Node = record
      Parent, Right, Left: PKDT7DD_Node;
      vec: PKDT7DD_Source;
    end;

    TKDT7DD_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT7DD_Source; const Data: Pointer);
    TKDT7DD_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT7DD_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT7DD_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT7DD_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT7DD_DyanmicStoreBuffer;
    KDBuff: TKDT7DD_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT7DD_Node;
    TestBuff: TKDT7DD_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT7DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT7DD_Node;
    function GetData(const Index: NativeInt): PKDT7DD_Source;
  public
    RootNode: PKDT7DD_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT7DD_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT7DD_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DD_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DD_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DD_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT7DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT7DD_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DD_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DD_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DD_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT7DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT7DD_Node; overload;
    function Search(const buff: TKDT7DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT7DD_Node; overload;
    function Search(const buff: TKDT7DD_Vec; var SearchedDistanceMin: Double): PKDT7DD_Node; overload;
    function Search(const buff: TKDT7DD_Vec): PKDT7DD_Node; overload;
    function SearchToken(const buff: TKDT7DD_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT7DD_DynamicVecBuffer; var OutBuff: TKDT7DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT7DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT7DD_Node);
    procedure PrintBuffer;

    class function KDT7DDVec(const s: SystemString): TKDT7DD_Vec; overload;
    class function KDT7DDVec(const v: TKDT7DD_Vec): SystemString; overload;
    class function KDT7DDPow(const v: TKDT7DD_VecType): Double;
    class function KDT7DDDistance(const v1, v2: TKDT7DD_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT7DD_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT8DD = class(TCoreClassObject)
  public type
    // code split
    TKDT8DD_Vec = array [0 .. KDT8DD_Axis - 1] of TKDT8DD_VecType;
    PKDT8DD_Vec = ^TKDT8DD_Vec;

    TKDT8DD_DynamicVecBuffer = array of TKDT8DD_Vec;
    PKDT8DD_DynamicVecBuffer = ^TKDT8DD_DynamicVecBuffer;

    TKDT8DD_Source = record
      buff: TKDT8DD_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT8DD_Source = ^TKDT8DD_Source;
    TKDT8DD_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT8DD_Source) - 1] of PKDT8DD_Source;
    PKDT8DD_SourceBuffer = ^TKDT8DD_SourceBuffer;

    TKDT8DD_DyanmicSourceBuffer = array of PKDT8DD_Source;
    PKDT8DD_DyanmicSourceBuffer = ^TKDT8DD_DyanmicSourceBuffer;

    TKDT8DD_DyanmicStoreBuffer = array of TKDT8DD_Source;
    PKDT8DD_DyanmicStoreBuffer = ^TKDT8DD_DyanmicStoreBuffer;

    PKDT8DD_Node = ^TKDT8DD_Node;

    TKDT8DD_Node = record
      Parent, Right, Left: PKDT8DD_Node;
      vec: PKDT8DD_Source;
    end;

    TKDT8DD_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT8DD_Source; const Data: Pointer);
    TKDT8DD_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT8DD_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT8DD_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT8DD_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT8DD_DyanmicStoreBuffer;
    KDBuff: TKDT8DD_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT8DD_Node;
    TestBuff: TKDT8DD_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT8DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT8DD_Node;
    function GetData(const Index: NativeInt): PKDT8DD_Source;
  public
    RootNode: PKDT8DD_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT8DD_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT8DD_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DD_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DD_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DD_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT8DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT8DD_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DD_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DD_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DD_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT8DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT8DD_Node; overload;
    function Search(const buff: TKDT8DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT8DD_Node; overload;
    function Search(const buff: TKDT8DD_Vec; var SearchedDistanceMin: Double): PKDT8DD_Node; overload;
    function Search(const buff: TKDT8DD_Vec): PKDT8DD_Node; overload;
    function SearchToken(const buff: TKDT8DD_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT8DD_DynamicVecBuffer; var OutBuff: TKDT8DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT8DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT8DD_Node);
    procedure PrintBuffer;

    class function KDT8DDVec(const s: SystemString): TKDT8DD_Vec; overload;
    class function KDT8DDVec(const v: TKDT8DD_Vec): SystemString; overload;
    class function KDT8DDPow(const v: TKDT8DD_VecType): Double;
    class function KDT8DDDistance(const v1, v2: TKDT8DD_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT8DD_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT9DD = class(TCoreClassObject)
  public type
    // code split
    TKDT9DD_Vec = array [0 .. KDT9DD_Axis - 1] of TKDT9DD_VecType;
    PKDT9DD_Vec = ^TKDT9DD_Vec;

    TKDT9DD_DynamicVecBuffer = array of TKDT9DD_Vec;
    PKDT9DD_DynamicVecBuffer = ^TKDT9DD_DynamicVecBuffer;

    TKDT9DD_Source = record
      buff: TKDT9DD_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT9DD_Source = ^TKDT9DD_Source;
    TKDT9DD_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT9DD_Source) - 1] of PKDT9DD_Source;
    PKDT9DD_SourceBuffer = ^TKDT9DD_SourceBuffer;

    TKDT9DD_DyanmicSourceBuffer = array of PKDT9DD_Source;
    PKDT9DD_DyanmicSourceBuffer = ^TKDT9DD_DyanmicSourceBuffer;

    TKDT9DD_DyanmicStoreBuffer = array of TKDT9DD_Source;
    PKDT9DD_DyanmicStoreBuffer = ^TKDT9DD_DyanmicStoreBuffer;

    PKDT9DD_Node = ^TKDT9DD_Node;

    TKDT9DD_Node = record
      Parent, Right, Left: PKDT9DD_Node;
      vec: PKDT9DD_Source;
    end;

    TKDT9DD_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT9DD_Source; const Data: Pointer);
    TKDT9DD_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT9DD_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT9DD_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT9DD_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT9DD_DyanmicStoreBuffer;
    KDBuff: TKDT9DD_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT9DD_Node;
    TestBuff: TKDT9DD_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT9DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT9DD_Node;
    function GetData(const Index: NativeInt): PKDT9DD_Source;
  public
    RootNode: PKDT9DD_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT9DD_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT9DD_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DD_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DD_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DD_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT9DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT9DD_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DD_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DD_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DD_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT9DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT9DD_Node; overload;
    function Search(const buff: TKDT9DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT9DD_Node; overload;
    function Search(const buff: TKDT9DD_Vec; var SearchedDistanceMin: Double): PKDT9DD_Node; overload;
    function Search(const buff: TKDT9DD_Vec): PKDT9DD_Node; overload;
    function SearchToken(const buff: TKDT9DD_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT9DD_DynamicVecBuffer; var OutBuff: TKDT9DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT9DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT9DD_Node);
    procedure PrintBuffer;

    class function KDT9DDVec(const s: SystemString): TKDT9DD_Vec; overload;
    class function KDT9DDVec(const v: TKDT9DD_Vec): SystemString; overload;
    class function KDT9DDPow(const v: TKDT9DD_VecType): Double;
    class function KDT9DDDistance(const v1, v2: TKDT9DD_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT9DD_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT10DD = class(TCoreClassObject)
  public type
    // code split
    TKDT10DD_Vec = array [0 .. KDT10DD_Axis - 1] of TKDT10DD_VecType;
    PKDT10DD_Vec = ^TKDT10DD_Vec;

    TKDT10DD_DynamicVecBuffer = array of TKDT10DD_Vec;
    PKDT10DD_DynamicVecBuffer = ^TKDT10DD_DynamicVecBuffer;

    TKDT10DD_Source = record
      buff: TKDT10DD_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT10DD_Source = ^TKDT10DD_Source;
    TKDT10DD_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT10DD_Source) - 1] of PKDT10DD_Source;
    PKDT10DD_SourceBuffer = ^TKDT10DD_SourceBuffer;

    TKDT10DD_DyanmicSourceBuffer = array of PKDT10DD_Source;
    PKDT10DD_DyanmicSourceBuffer = ^TKDT10DD_DyanmicSourceBuffer;

    TKDT10DD_DyanmicStoreBuffer = array of TKDT10DD_Source;
    PKDT10DD_DyanmicStoreBuffer = ^TKDT10DD_DyanmicStoreBuffer;

    PKDT10DD_Node = ^TKDT10DD_Node;

    TKDT10DD_Node = record
      Parent, Right, Left: PKDT10DD_Node;
      vec: PKDT10DD_Source;
    end;

    TKDT10DD_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT10DD_Source; const Data: Pointer);
    TKDT10DD_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT10DD_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT10DD_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT10DD_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT10DD_DyanmicStoreBuffer;
    KDBuff: TKDT10DD_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT10DD_Node;
    TestBuff: TKDT10DD_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT10DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT10DD_Node;
    function GetData(const Index: NativeInt): PKDT10DD_Source;
  public
    RootNode: PKDT10DD_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT10DD_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT10DD_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DD_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DD_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DD_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT10DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT10DD_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DD_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DD_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DD_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT10DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT10DD_Node; overload;
    function Search(const buff: TKDT10DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT10DD_Node; overload;
    function Search(const buff: TKDT10DD_Vec; var SearchedDistanceMin: Double): PKDT10DD_Node; overload;
    function Search(const buff: TKDT10DD_Vec): PKDT10DD_Node; overload;
    function SearchToken(const buff: TKDT10DD_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT10DD_DynamicVecBuffer; var OutBuff: TKDT10DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT10DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT10DD_Node);
    procedure PrintBuffer;

    class function KDT10DDVec(const s: SystemString): TKDT10DD_Vec; overload;
    class function KDT10DDVec(const v: TKDT10DD_Vec): SystemString; overload;
    class function KDT10DDPow(const v: TKDT10DD_VecType): Double;
    class function KDT10DDDistance(const v1, v2: TKDT10DD_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT10DD_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT11DD = class(TCoreClassObject)
  public type
    // code split
    TKDT11DD_Vec = array [0 .. KDT11DD_Axis - 1] of TKDT11DD_VecType;
    PKDT11DD_Vec = ^TKDT11DD_Vec;

    TKDT11DD_DynamicVecBuffer = array of TKDT11DD_Vec;
    PKDT11DD_DynamicVecBuffer = ^TKDT11DD_DynamicVecBuffer;

    TKDT11DD_Source = record
      buff: TKDT11DD_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT11DD_Source = ^TKDT11DD_Source;
    TKDT11DD_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT11DD_Source) - 1] of PKDT11DD_Source;
    PKDT11DD_SourceBuffer = ^TKDT11DD_SourceBuffer;

    TKDT11DD_DyanmicSourceBuffer = array of PKDT11DD_Source;
    PKDT11DD_DyanmicSourceBuffer = ^TKDT11DD_DyanmicSourceBuffer;

    TKDT11DD_DyanmicStoreBuffer = array of TKDT11DD_Source;
    PKDT11DD_DyanmicStoreBuffer = ^TKDT11DD_DyanmicStoreBuffer;

    PKDT11DD_Node = ^TKDT11DD_Node;

    TKDT11DD_Node = record
      Parent, Right, Left: PKDT11DD_Node;
      vec: PKDT11DD_Source;
    end;

    TKDT11DD_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT11DD_Source; const Data: Pointer);
    TKDT11DD_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT11DD_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT11DD_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT11DD_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT11DD_DyanmicStoreBuffer;
    KDBuff: TKDT11DD_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT11DD_Node;
    TestBuff: TKDT11DD_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT11DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT11DD_Node;
    function GetData(const Index: NativeInt): PKDT11DD_Source;
  public
    RootNode: PKDT11DD_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT11DD_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT11DD_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DD_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DD_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DD_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT11DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT11DD_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DD_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DD_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DD_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT11DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT11DD_Node; overload;
    function Search(const buff: TKDT11DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT11DD_Node; overload;
    function Search(const buff: TKDT11DD_Vec; var SearchedDistanceMin: Double): PKDT11DD_Node; overload;
    function Search(const buff: TKDT11DD_Vec): PKDT11DD_Node; overload;
    function SearchToken(const buff: TKDT11DD_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT11DD_DynamicVecBuffer; var OutBuff: TKDT11DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT11DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT11DD_Node);
    procedure PrintBuffer;

    class function KDT11DDVec(const s: SystemString): TKDT11DD_Vec; overload;
    class function KDT11DDVec(const v: TKDT11DD_Vec): SystemString; overload;
    class function KDT11DDPow(const v: TKDT11DD_VecType): Double;
    class function KDT11DDDistance(const v1, v2: TKDT11DD_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT11DD_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT12DD = class(TCoreClassObject)
  public type
    // code split
    TKDT12DD_Vec = array [0 .. KDT12DD_Axis - 1] of TKDT12DD_VecType;
    PKDT12DD_Vec = ^TKDT12DD_Vec;

    TKDT12DD_DynamicVecBuffer = array of TKDT12DD_Vec;
    PKDT12DD_DynamicVecBuffer = ^TKDT12DD_DynamicVecBuffer;

    TKDT12DD_Source = record
      buff: TKDT12DD_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT12DD_Source = ^TKDT12DD_Source;
    TKDT12DD_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT12DD_Source) - 1] of PKDT12DD_Source;
    PKDT12DD_SourceBuffer = ^TKDT12DD_SourceBuffer;

    TKDT12DD_DyanmicSourceBuffer = array of PKDT12DD_Source;
    PKDT12DD_DyanmicSourceBuffer = ^TKDT12DD_DyanmicSourceBuffer;

    TKDT12DD_DyanmicStoreBuffer = array of TKDT12DD_Source;
    PKDT12DD_DyanmicStoreBuffer = ^TKDT12DD_DyanmicStoreBuffer;

    PKDT12DD_Node = ^TKDT12DD_Node;

    TKDT12DD_Node = record
      Parent, Right, Left: PKDT12DD_Node;
      vec: PKDT12DD_Source;
    end;

    TKDT12DD_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT12DD_Source; const Data: Pointer);
    TKDT12DD_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT12DD_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT12DD_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT12DD_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT12DD_DyanmicStoreBuffer;
    KDBuff: TKDT12DD_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT12DD_Node;
    TestBuff: TKDT12DD_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT12DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT12DD_Node;
    function GetData(const Index: NativeInt): PKDT12DD_Source;
  public
    RootNode: PKDT12DD_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT12DD_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT12DD_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DD_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DD_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DD_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT12DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT12DD_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DD_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DD_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DD_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT12DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT12DD_Node; overload;
    function Search(const buff: TKDT12DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT12DD_Node; overload;
    function Search(const buff: TKDT12DD_Vec; var SearchedDistanceMin: Double): PKDT12DD_Node; overload;
    function Search(const buff: TKDT12DD_Vec): PKDT12DD_Node; overload;
    function SearchToken(const buff: TKDT12DD_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT12DD_DynamicVecBuffer; var OutBuff: TKDT12DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT12DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT12DD_Node);
    procedure PrintBuffer;

    class function KDT12DDVec(const s: SystemString): TKDT12DD_Vec; overload;
    class function KDT12DDVec(const v: TKDT12DD_Vec): SystemString; overload;
    class function KDT12DDPow(const v: TKDT12DD_VecType): Double;
    class function KDT12DDDistance(const v1, v2: TKDT12DD_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT12DD_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT13DD = class(TCoreClassObject)
  public type
    // code split
    TKDT13DD_Vec = array [0 .. KDT13DD_Axis - 1] of TKDT13DD_VecType;
    PKDT13DD_Vec = ^TKDT13DD_Vec;

    TKDT13DD_DynamicVecBuffer = array of TKDT13DD_Vec;
    PKDT13DD_DynamicVecBuffer = ^TKDT13DD_DynamicVecBuffer;

    TKDT13DD_Source = record
      buff: TKDT13DD_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT13DD_Source = ^TKDT13DD_Source;
    TKDT13DD_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT13DD_Source) - 1] of PKDT13DD_Source;
    PKDT13DD_SourceBuffer = ^TKDT13DD_SourceBuffer;

    TKDT13DD_DyanmicSourceBuffer = array of PKDT13DD_Source;
    PKDT13DD_DyanmicSourceBuffer = ^TKDT13DD_DyanmicSourceBuffer;

    TKDT13DD_DyanmicStoreBuffer = array of TKDT13DD_Source;
    PKDT13DD_DyanmicStoreBuffer = ^TKDT13DD_DyanmicStoreBuffer;

    PKDT13DD_Node = ^TKDT13DD_Node;

    TKDT13DD_Node = record
      Parent, Right, Left: PKDT13DD_Node;
      vec: PKDT13DD_Source;
    end;

    TKDT13DD_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT13DD_Source; const Data: Pointer);
    TKDT13DD_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT13DD_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT13DD_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT13DD_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT13DD_DyanmicStoreBuffer;
    KDBuff: TKDT13DD_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT13DD_Node;
    TestBuff: TKDT13DD_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT13DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT13DD_Node;
    function GetData(const Index: NativeInt): PKDT13DD_Source;
  public
    RootNode: PKDT13DD_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT13DD_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT13DD_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DD_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DD_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DD_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT13DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT13DD_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DD_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DD_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DD_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT13DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT13DD_Node; overload;
    function Search(const buff: TKDT13DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT13DD_Node; overload;
    function Search(const buff: TKDT13DD_Vec; var SearchedDistanceMin: Double): PKDT13DD_Node; overload;
    function Search(const buff: TKDT13DD_Vec): PKDT13DD_Node; overload;
    function SearchToken(const buff: TKDT13DD_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT13DD_DynamicVecBuffer; var OutBuff: TKDT13DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT13DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT13DD_Node);
    procedure PrintBuffer;

    class function KDT13DDVec(const s: SystemString): TKDT13DD_Vec; overload;
    class function KDT13DDVec(const v: TKDT13DD_Vec): SystemString; overload;
    class function KDT13DDPow(const v: TKDT13DD_VecType): Double;
    class function KDT13DDDistance(const v1, v2: TKDT13DD_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT13DD_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT14DD = class(TCoreClassObject)
  public type
    // code split
    TKDT14DD_Vec = array [0 .. KDT14DD_Axis - 1] of TKDT14DD_VecType;
    PKDT14DD_Vec = ^TKDT14DD_Vec;

    TKDT14DD_DynamicVecBuffer = array of TKDT14DD_Vec;
    PKDT14DD_DynamicVecBuffer = ^TKDT14DD_DynamicVecBuffer;

    TKDT14DD_Source = record
      buff: TKDT14DD_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT14DD_Source = ^TKDT14DD_Source;
    TKDT14DD_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT14DD_Source) - 1] of PKDT14DD_Source;
    PKDT14DD_SourceBuffer = ^TKDT14DD_SourceBuffer;

    TKDT14DD_DyanmicSourceBuffer = array of PKDT14DD_Source;
    PKDT14DD_DyanmicSourceBuffer = ^TKDT14DD_DyanmicSourceBuffer;

    TKDT14DD_DyanmicStoreBuffer = array of TKDT14DD_Source;
    PKDT14DD_DyanmicStoreBuffer = ^TKDT14DD_DyanmicStoreBuffer;

    PKDT14DD_Node = ^TKDT14DD_Node;

    TKDT14DD_Node = record
      Parent, Right, Left: PKDT14DD_Node;
      vec: PKDT14DD_Source;
    end;

    TKDT14DD_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT14DD_Source; const Data: Pointer);
    TKDT14DD_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT14DD_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT14DD_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT14DD_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT14DD_DyanmicStoreBuffer;
    KDBuff: TKDT14DD_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT14DD_Node;
    TestBuff: TKDT14DD_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT14DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT14DD_Node;
    function GetData(const Index: NativeInt): PKDT14DD_Source;
  public
    RootNode: PKDT14DD_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT14DD_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT14DD_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DD_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DD_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DD_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT14DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT14DD_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DD_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DD_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DD_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT14DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT14DD_Node; overload;
    function Search(const buff: TKDT14DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT14DD_Node; overload;
    function Search(const buff: TKDT14DD_Vec; var SearchedDistanceMin: Double): PKDT14DD_Node; overload;
    function Search(const buff: TKDT14DD_Vec): PKDT14DD_Node; overload;
    function SearchToken(const buff: TKDT14DD_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT14DD_DynamicVecBuffer; var OutBuff: TKDT14DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT14DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT14DD_Node);
    procedure PrintBuffer;

    class function KDT14DDVec(const s: SystemString): TKDT14DD_Vec; overload;
    class function KDT14DDVec(const v: TKDT14DD_Vec): SystemString; overload;
    class function KDT14DDPow(const v: TKDT14DD_VecType): Double;
    class function KDT14DDDistance(const v1, v2: TKDT14DD_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT14DD_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT15DD = class(TCoreClassObject)
  public type
    // code split
    TKDT15DD_Vec = array [0 .. KDT15DD_Axis - 1] of TKDT15DD_VecType;
    PKDT15DD_Vec = ^TKDT15DD_Vec;

    TKDT15DD_DynamicVecBuffer = array of TKDT15DD_Vec;
    PKDT15DD_DynamicVecBuffer = ^TKDT15DD_DynamicVecBuffer;

    TKDT15DD_Source = record
      buff: TKDT15DD_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT15DD_Source = ^TKDT15DD_Source;
    TKDT15DD_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT15DD_Source) - 1] of PKDT15DD_Source;
    PKDT15DD_SourceBuffer = ^TKDT15DD_SourceBuffer;

    TKDT15DD_DyanmicSourceBuffer = array of PKDT15DD_Source;
    PKDT15DD_DyanmicSourceBuffer = ^TKDT15DD_DyanmicSourceBuffer;

    TKDT15DD_DyanmicStoreBuffer = array of TKDT15DD_Source;
    PKDT15DD_DyanmicStoreBuffer = ^TKDT15DD_DyanmicStoreBuffer;

    PKDT15DD_Node = ^TKDT15DD_Node;

    TKDT15DD_Node = record
      Parent, Right, Left: PKDT15DD_Node;
      vec: PKDT15DD_Source;
    end;

    TKDT15DD_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT15DD_Source; const Data: Pointer);
    TKDT15DD_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT15DD_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT15DD_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT15DD_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT15DD_DyanmicStoreBuffer;
    KDBuff: TKDT15DD_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT15DD_Node;
    TestBuff: TKDT15DD_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT15DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT15DD_Node;
    function GetData(const Index: NativeInt): PKDT15DD_Source;
  public
    RootNode: PKDT15DD_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT15DD_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT15DD_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DD_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DD_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DD_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT15DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT15DD_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DD_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DD_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DD_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT15DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT15DD_Node; overload;
    function Search(const buff: TKDT15DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT15DD_Node; overload;
    function Search(const buff: TKDT15DD_Vec; var SearchedDistanceMin: Double): PKDT15DD_Node; overload;
    function Search(const buff: TKDT15DD_Vec): PKDT15DD_Node; overload;
    function SearchToken(const buff: TKDT15DD_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT15DD_DynamicVecBuffer; var OutBuff: TKDT15DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT15DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT15DD_Node);
    procedure PrintBuffer;

    class function KDT15DDVec(const s: SystemString): TKDT15DD_Vec; overload;
    class function KDT15DDVec(const v: TKDT15DD_Vec): SystemString; overload;
    class function KDT15DDPow(const v: TKDT15DD_VecType): Double;
    class function KDT15DDDistance(const v1, v2: TKDT15DD_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT15DD_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT16DD = class(TCoreClassObject)
  public type
    // code split
    TKDT16DD_Vec = array [0 .. KDT16DD_Axis - 1] of TKDT16DD_VecType;
    PKDT16DD_Vec = ^TKDT16DD_Vec;

    TKDT16DD_DynamicVecBuffer = array of TKDT16DD_Vec;
    PKDT16DD_DynamicVecBuffer = ^TKDT16DD_DynamicVecBuffer;

    TKDT16DD_Source = record
      buff: TKDT16DD_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT16DD_Source = ^TKDT16DD_Source;
    TKDT16DD_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT16DD_Source) - 1] of PKDT16DD_Source;
    PKDT16DD_SourceBuffer = ^TKDT16DD_SourceBuffer;

    TKDT16DD_DyanmicSourceBuffer = array of PKDT16DD_Source;
    PKDT16DD_DyanmicSourceBuffer = ^TKDT16DD_DyanmicSourceBuffer;

    TKDT16DD_DyanmicStoreBuffer = array of TKDT16DD_Source;
    PKDT16DD_DyanmicStoreBuffer = ^TKDT16DD_DyanmicStoreBuffer;

    PKDT16DD_Node = ^TKDT16DD_Node;

    TKDT16DD_Node = record
      Parent, Right, Left: PKDT16DD_Node;
      vec: PKDT16DD_Source;
    end;

    TKDT16DD_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT16DD_Source; const Data: Pointer);
    TKDT16DD_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT16DD_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT16DD_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT16DD_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT16DD_DyanmicStoreBuffer;
    KDBuff: TKDT16DD_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT16DD_Node;
    TestBuff: TKDT16DD_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT16DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT16DD_Node;
    function GetData(const Index: NativeInt): PKDT16DD_Source;
  public
    RootNode: PKDT16DD_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT16DD_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT16DD_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DD_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DD_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DD_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT16DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT16DD_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DD_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DD_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DD_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT16DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT16DD_Node; overload;
    function Search(const buff: TKDT16DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT16DD_Node; overload;
    function Search(const buff: TKDT16DD_Vec; var SearchedDistanceMin: Double): PKDT16DD_Node; overload;
    function Search(const buff: TKDT16DD_Vec): PKDT16DD_Node; overload;
    function SearchToken(const buff: TKDT16DD_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT16DD_DynamicVecBuffer; var OutBuff: TKDT16DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT16DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT16DD_Node);
    procedure PrintBuffer;

    class function KDT16DDVec(const s: SystemString): TKDT16DD_Vec; overload;
    class function KDT16DDVec(const v: TKDT16DD_Vec): SystemString; overload;
    class function KDT16DDPow(const v: TKDT16DD_VecType): Double;
    class function KDT16DDDistance(const v1, v2: TKDT16DD_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT16DD_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT17DD = class(TCoreClassObject)
  public type
    // code split
    TKDT17DD_Vec = array [0 .. KDT17DD_Axis - 1] of TKDT17DD_VecType;
    PKDT17DD_Vec = ^TKDT17DD_Vec;

    TKDT17DD_DynamicVecBuffer = array of TKDT17DD_Vec;
    PKDT17DD_DynamicVecBuffer = ^TKDT17DD_DynamicVecBuffer;

    TKDT17DD_Source = record
      buff: TKDT17DD_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT17DD_Source = ^TKDT17DD_Source;
    TKDT17DD_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT17DD_Source) - 1] of PKDT17DD_Source;
    PKDT17DD_SourceBuffer = ^TKDT17DD_SourceBuffer;

    TKDT17DD_DyanmicSourceBuffer = array of PKDT17DD_Source;
    PKDT17DD_DyanmicSourceBuffer = ^TKDT17DD_DyanmicSourceBuffer;

    TKDT17DD_DyanmicStoreBuffer = array of TKDT17DD_Source;
    PKDT17DD_DyanmicStoreBuffer = ^TKDT17DD_DyanmicStoreBuffer;

    PKDT17DD_Node = ^TKDT17DD_Node;

    TKDT17DD_Node = record
      Parent, Right, Left: PKDT17DD_Node;
      vec: PKDT17DD_Source;
    end;

    TKDT17DD_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT17DD_Source; const Data: Pointer);
    TKDT17DD_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT17DD_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT17DD_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT17DD_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT17DD_DyanmicStoreBuffer;
    KDBuff: TKDT17DD_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT17DD_Node;
    TestBuff: TKDT17DD_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT17DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT17DD_Node;
    function GetData(const Index: NativeInt): PKDT17DD_Source;
  public
    RootNode: PKDT17DD_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT17DD_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT17DD_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DD_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DD_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DD_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT17DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT17DD_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DD_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DD_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DD_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT17DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT17DD_Node; overload;
    function Search(const buff: TKDT17DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT17DD_Node; overload;
    function Search(const buff: TKDT17DD_Vec; var SearchedDistanceMin: Double): PKDT17DD_Node; overload;
    function Search(const buff: TKDT17DD_Vec): PKDT17DD_Node; overload;
    function SearchToken(const buff: TKDT17DD_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT17DD_DynamicVecBuffer; var OutBuff: TKDT17DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT17DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT17DD_Node);
    procedure PrintBuffer;

    class function KDT17DDVec(const s: SystemString): TKDT17DD_Vec; overload;
    class function KDT17DDVec(const v: TKDT17DD_Vec): SystemString; overload;
    class function KDT17DDPow(const v: TKDT17DD_VecType): Double;
    class function KDT17DDDistance(const v1, v2: TKDT17DD_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT17DD_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT18DD = class(TCoreClassObject)
  public type
    // code split
    TKDT18DD_Vec = array [0 .. KDT18DD_Axis - 1] of TKDT18DD_VecType;
    PKDT18DD_Vec = ^TKDT18DD_Vec;

    TKDT18DD_DynamicVecBuffer = array of TKDT18DD_Vec;
    PKDT18DD_DynamicVecBuffer = ^TKDT18DD_DynamicVecBuffer;

    TKDT18DD_Source = record
      buff: TKDT18DD_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT18DD_Source = ^TKDT18DD_Source;
    TKDT18DD_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT18DD_Source) - 1] of PKDT18DD_Source;
    PKDT18DD_SourceBuffer = ^TKDT18DD_SourceBuffer;

    TKDT18DD_DyanmicSourceBuffer = array of PKDT18DD_Source;
    PKDT18DD_DyanmicSourceBuffer = ^TKDT18DD_DyanmicSourceBuffer;

    TKDT18DD_DyanmicStoreBuffer = array of TKDT18DD_Source;
    PKDT18DD_DyanmicStoreBuffer = ^TKDT18DD_DyanmicStoreBuffer;

    PKDT18DD_Node = ^TKDT18DD_Node;

    TKDT18DD_Node = record
      Parent, Right, Left: PKDT18DD_Node;
      vec: PKDT18DD_Source;
    end;

    TKDT18DD_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT18DD_Source; const Data: Pointer);
    TKDT18DD_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT18DD_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT18DD_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT18DD_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT18DD_DyanmicStoreBuffer;
    KDBuff: TKDT18DD_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT18DD_Node;
    TestBuff: TKDT18DD_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT18DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT18DD_Node;
    function GetData(const Index: NativeInt): PKDT18DD_Source;
  public
    RootNode: PKDT18DD_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT18DD_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT18DD_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DD_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DD_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DD_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT18DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT18DD_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DD_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DD_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DD_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT18DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT18DD_Node; overload;
    function Search(const buff: TKDT18DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT18DD_Node; overload;
    function Search(const buff: TKDT18DD_Vec; var SearchedDistanceMin: Double): PKDT18DD_Node; overload;
    function Search(const buff: TKDT18DD_Vec): PKDT18DD_Node; overload;
    function SearchToken(const buff: TKDT18DD_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT18DD_DynamicVecBuffer; var OutBuff: TKDT18DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT18DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT18DD_Node);
    procedure PrintBuffer;

    class function KDT18DDVec(const s: SystemString): TKDT18DD_Vec; overload;
    class function KDT18DDVec(const v: TKDT18DD_Vec): SystemString; overload;
    class function KDT18DDPow(const v: TKDT18DD_VecType): Double;
    class function KDT18DDDistance(const v1, v2: TKDT18DD_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT18DD_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT19DD = class(TCoreClassObject)
  public type
    // code split
    TKDT19DD_Vec = array [0 .. KDT19DD_Axis - 1] of TKDT19DD_VecType;
    PKDT19DD_Vec = ^TKDT19DD_Vec;

    TKDT19DD_DynamicVecBuffer = array of TKDT19DD_Vec;
    PKDT19DD_DynamicVecBuffer = ^TKDT19DD_DynamicVecBuffer;

    TKDT19DD_Source = record
      buff: TKDT19DD_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT19DD_Source = ^TKDT19DD_Source;
    TKDT19DD_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT19DD_Source) - 1] of PKDT19DD_Source;
    PKDT19DD_SourceBuffer = ^TKDT19DD_SourceBuffer;

    TKDT19DD_DyanmicSourceBuffer = array of PKDT19DD_Source;
    PKDT19DD_DyanmicSourceBuffer = ^TKDT19DD_DyanmicSourceBuffer;

    TKDT19DD_DyanmicStoreBuffer = array of TKDT19DD_Source;
    PKDT19DD_DyanmicStoreBuffer = ^TKDT19DD_DyanmicStoreBuffer;

    PKDT19DD_Node = ^TKDT19DD_Node;

    TKDT19DD_Node = record
      Parent, Right, Left: PKDT19DD_Node;
      vec: PKDT19DD_Source;
    end;

    TKDT19DD_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT19DD_Source; const Data: Pointer);
    TKDT19DD_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT19DD_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT19DD_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT19DD_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT19DD_DyanmicStoreBuffer;
    KDBuff: TKDT19DD_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT19DD_Node;
    TestBuff: TKDT19DD_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT19DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT19DD_Node;
    function GetData(const Index: NativeInt): PKDT19DD_Source;
  public
    RootNode: PKDT19DD_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT19DD_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT19DD_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DD_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DD_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DD_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT19DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT19DD_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DD_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DD_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DD_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT19DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT19DD_Node; overload;
    function Search(const buff: TKDT19DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT19DD_Node; overload;
    function Search(const buff: TKDT19DD_Vec; var SearchedDistanceMin: Double): PKDT19DD_Node; overload;
    function Search(const buff: TKDT19DD_Vec): PKDT19DD_Node; overload;
    function SearchToken(const buff: TKDT19DD_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT19DD_DynamicVecBuffer; var OutBuff: TKDT19DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT19DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT19DD_Node);
    procedure PrintBuffer;

    class function KDT19DDVec(const s: SystemString): TKDT19DD_Vec; overload;
    class function KDT19DDVec(const v: TKDT19DD_Vec): SystemString; overload;
    class function KDT19DDPow(const v: TKDT19DD_VecType): Double;
    class function KDT19DDDistance(const v1, v2: TKDT19DD_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT19DD_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT20DD = class(TCoreClassObject)
  public type
    // code split
    TKDT20DD_Vec = array [0 .. KDT20DD_Axis - 1] of TKDT20DD_VecType;
    PKDT20DD_Vec = ^TKDT20DD_Vec;

    TKDT20DD_DynamicVecBuffer = array of TKDT20DD_Vec;
    PKDT20DD_DynamicVecBuffer = ^TKDT20DD_DynamicVecBuffer;

    TKDT20DD_Source = record
      buff: TKDT20DD_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT20DD_Source = ^TKDT20DD_Source;
    TKDT20DD_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT20DD_Source) - 1] of PKDT20DD_Source;
    PKDT20DD_SourceBuffer = ^TKDT20DD_SourceBuffer;

    TKDT20DD_DyanmicSourceBuffer = array of PKDT20DD_Source;
    PKDT20DD_DyanmicSourceBuffer = ^TKDT20DD_DyanmicSourceBuffer;

    TKDT20DD_DyanmicStoreBuffer = array of TKDT20DD_Source;
    PKDT20DD_DyanmicStoreBuffer = ^TKDT20DD_DyanmicStoreBuffer;

    PKDT20DD_Node = ^TKDT20DD_Node;

    TKDT20DD_Node = record
      Parent, Right, Left: PKDT20DD_Node;
      vec: PKDT20DD_Source;
    end;

    TKDT20DD_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT20DD_Source; const Data: Pointer);
    TKDT20DD_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT20DD_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT20DD_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT20DD_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT20DD_DyanmicStoreBuffer;
    KDBuff: TKDT20DD_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT20DD_Node;
    TestBuff: TKDT20DD_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT20DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT20DD_Node;
    function GetData(const Index: NativeInt): PKDT20DD_Source;
  public
    RootNode: PKDT20DD_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT20DD_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT20DD_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DD_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DD_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DD_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT20DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT20DD_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DD_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DD_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DD_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT20DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT20DD_Node; overload;
    function Search(const buff: TKDT20DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT20DD_Node; overload;
    function Search(const buff: TKDT20DD_Vec; var SearchedDistanceMin: Double): PKDT20DD_Node; overload;
    function Search(const buff: TKDT20DD_Vec): PKDT20DD_Node; overload;
    function SearchToken(const buff: TKDT20DD_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT20DD_DynamicVecBuffer; var OutBuff: TKDT20DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT20DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT20DD_Node);
    procedure PrintBuffer;

    class function KDT20DDVec(const s: SystemString): TKDT20DD_Vec; overload;
    class function KDT20DDVec(const v: TKDT20DD_Vec): SystemString; overload;
    class function KDT20DDPow(const v: TKDT20DD_VecType): Double;
    class function KDT20DDDistance(const v1, v2: TKDT20DD_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT20DD_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT21DD = class(TCoreClassObject)
  public type
    // code split
    TKDT21DD_Vec = array [0 .. KDT21DD_Axis - 1] of TKDT21DD_VecType;
    PKDT21DD_Vec = ^TKDT21DD_Vec;

    TKDT21DD_DynamicVecBuffer = array of TKDT21DD_Vec;
    PKDT21DD_DynamicVecBuffer = ^TKDT21DD_DynamicVecBuffer;

    TKDT21DD_Source = record
      buff: TKDT21DD_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT21DD_Source = ^TKDT21DD_Source;
    TKDT21DD_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT21DD_Source) - 1] of PKDT21DD_Source;
    PKDT21DD_SourceBuffer = ^TKDT21DD_SourceBuffer;

    TKDT21DD_DyanmicSourceBuffer = array of PKDT21DD_Source;
    PKDT21DD_DyanmicSourceBuffer = ^TKDT21DD_DyanmicSourceBuffer;

    TKDT21DD_DyanmicStoreBuffer = array of TKDT21DD_Source;
    PKDT21DD_DyanmicStoreBuffer = ^TKDT21DD_DyanmicStoreBuffer;

    PKDT21DD_Node = ^TKDT21DD_Node;

    TKDT21DD_Node = record
      Parent, Right, Left: PKDT21DD_Node;
      vec: PKDT21DD_Source;
    end;

    TKDT21DD_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT21DD_Source; const Data: Pointer);
    TKDT21DD_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT21DD_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT21DD_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT21DD_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT21DD_DyanmicStoreBuffer;
    KDBuff: TKDT21DD_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT21DD_Node;
    TestBuff: TKDT21DD_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT21DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT21DD_Node;
    function GetData(const Index: NativeInt): PKDT21DD_Source;
  public
    RootNode: PKDT21DD_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT21DD_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT21DD_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DD_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DD_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DD_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT21DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT21DD_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DD_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DD_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DD_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT21DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT21DD_Node; overload;
    function Search(const buff: TKDT21DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT21DD_Node; overload;
    function Search(const buff: TKDT21DD_Vec; var SearchedDistanceMin: Double): PKDT21DD_Node; overload;
    function Search(const buff: TKDT21DD_Vec): PKDT21DD_Node; overload;
    function SearchToken(const buff: TKDT21DD_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT21DD_DynamicVecBuffer; var OutBuff: TKDT21DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT21DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT21DD_Node);
    procedure PrintBuffer;

    class function KDT21DDVec(const s: SystemString): TKDT21DD_Vec; overload;
    class function KDT21DDVec(const v: TKDT21DD_Vec): SystemString; overload;
    class function KDT21DDPow(const v: TKDT21DD_VecType): Double;
    class function KDT21DDDistance(const v1, v2: TKDT21DD_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT21DD_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT22DD = class(TCoreClassObject)
  public type
    // code split
    TKDT22DD_Vec = array [0 .. KDT22DD_Axis - 1] of TKDT22DD_VecType;
    PKDT22DD_Vec = ^TKDT22DD_Vec;

    TKDT22DD_DynamicVecBuffer = array of TKDT22DD_Vec;
    PKDT22DD_DynamicVecBuffer = ^TKDT22DD_DynamicVecBuffer;

    TKDT22DD_Source = record
      buff: TKDT22DD_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT22DD_Source = ^TKDT22DD_Source;
    TKDT22DD_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT22DD_Source) - 1] of PKDT22DD_Source;
    PKDT22DD_SourceBuffer = ^TKDT22DD_SourceBuffer;

    TKDT22DD_DyanmicSourceBuffer = array of PKDT22DD_Source;
    PKDT22DD_DyanmicSourceBuffer = ^TKDT22DD_DyanmicSourceBuffer;

    TKDT22DD_DyanmicStoreBuffer = array of TKDT22DD_Source;
    PKDT22DD_DyanmicStoreBuffer = ^TKDT22DD_DyanmicStoreBuffer;

    PKDT22DD_Node = ^TKDT22DD_Node;

    TKDT22DD_Node = record
      Parent, Right, Left: PKDT22DD_Node;
      vec: PKDT22DD_Source;
    end;

    TKDT22DD_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT22DD_Source; const Data: Pointer);
    TKDT22DD_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT22DD_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT22DD_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT22DD_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT22DD_DyanmicStoreBuffer;
    KDBuff: TKDT22DD_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT22DD_Node;
    TestBuff: TKDT22DD_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT22DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT22DD_Node;
    function GetData(const Index: NativeInt): PKDT22DD_Source;
  public
    RootNode: PKDT22DD_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT22DD_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT22DD_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DD_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DD_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DD_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT22DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT22DD_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DD_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DD_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DD_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT22DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT22DD_Node; overload;
    function Search(const buff: TKDT22DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT22DD_Node; overload;
    function Search(const buff: TKDT22DD_Vec; var SearchedDistanceMin: Double): PKDT22DD_Node; overload;
    function Search(const buff: TKDT22DD_Vec): PKDT22DD_Node; overload;
    function SearchToken(const buff: TKDT22DD_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT22DD_DynamicVecBuffer; var OutBuff: TKDT22DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT22DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT22DD_Node);
    procedure PrintBuffer;

    class function KDT22DDVec(const s: SystemString): TKDT22DD_Vec; overload;
    class function KDT22DDVec(const v: TKDT22DD_Vec): SystemString; overload;
    class function KDT22DDPow(const v: TKDT22DD_VecType): Double;
    class function KDT22DDDistance(const v1, v2: TKDT22DD_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT22DD_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT23DD = class(TCoreClassObject)
  public type
    // code split
    TKDT23DD_Vec = array [0 .. KDT23DD_Axis - 1] of TKDT23DD_VecType;
    PKDT23DD_Vec = ^TKDT23DD_Vec;

    TKDT23DD_DynamicVecBuffer = array of TKDT23DD_Vec;
    PKDT23DD_DynamicVecBuffer = ^TKDT23DD_DynamicVecBuffer;

    TKDT23DD_Source = record
      buff: TKDT23DD_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT23DD_Source = ^TKDT23DD_Source;
    TKDT23DD_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT23DD_Source) - 1] of PKDT23DD_Source;
    PKDT23DD_SourceBuffer = ^TKDT23DD_SourceBuffer;

    TKDT23DD_DyanmicSourceBuffer = array of PKDT23DD_Source;
    PKDT23DD_DyanmicSourceBuffer = ^TKDT23DD_DyanmicSourceBuffer;

    TKDT23DD_DyanmicStoreBuffer = array of TKDT23DD_Source;
    PKDT23DD_DyanmicStoreBuffer = ^TKDT23DD_DyanmicStoreBuffer;

    PKDT23DD_Node = ^TKDT23DD_Node;

    TKDT23DD_Node = record
      Parent, Right, Left: PKDT23DD_Node;
      vec: PKDT23DD_Source;
    end;

    TKDT23DD_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT23DD_Source; const Data: Pointer);
    TKDT23DD_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT23DD_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT23DD_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT23DD_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT23DD_DyanmicStoreBuffer;
    KDBuff: TKDT23DD_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT23DD_Node;
    TestBuff: TKDT23DD_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT23DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT23DD_Node;
    function GetData(const Index: NativeInt): PKDT23DD_Source;
  public
    RootNode: PKDT23DD_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT23DD_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT23DD_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DD_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DD_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DD_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT23DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT23DD_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DD_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DD_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DD_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT23DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT23DD_Node; overload;
    function Search(const buff: TKDT23DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT23DD_Node; overload;
    function Search(const buff: TKDT23DD_Vec; var SearchedDistanceMin: Double): PKDT23DD_Node; overload;
    function Search(const buff: TKDT23DD_Vec): PKDT23DD_Node; overload;
    function SearchToken(const buff: TKDT23DD_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT23DD_DynamicVecBuffer; var OutBuff: TKDT23DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT23DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT23DD_Node);
    procedure PrintBuffer;

    class function KDT23DDVec(const s: SystemString): TKDT23DD_Vec; overload;
    class function KDT23DDVec(const v: TKDT23DD_Vec): SystemString; overload;
    class function KDT23DDPow(const v: TKDT23DD_VecType): Double;
    class function KDT23DDDistance(const v1, v2: TKDT23DD_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT23DD_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT24DD = class(TCoreClassObject)
  public type
    // code split
    TKDT24DD_Vec = array [0 .. KDT24DD_Axis - 1] of TKDT24DD_VecType;
    PKDT24DD_Vec = ^TKDT24DD_Vec;

    TKDT24DD_DynamicVecBuffer = array of TKDT24DD_Vec;
    PKDT24DD_DynamicVecBuffer = ^TKDT24DD_DynamicVecBuffer;

    TKDT24DD_Source = record
      buff: TKDT24DD_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT24DD_Source = ^TKDT24DD_Source;
    TKDT24DD_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT24DD_Source) - 1] of PKDT24DD_Source;
    PKDT24DD_SourceBuffer = ^TKDT24DD_SourceBuffer;

    TKDT24DD_DyanmicSourceBuffer = array of PKDT24DD_Source;
    PKDT24DD_DyanmicSourceBuffer = ^TKDT24DD_DyanmicSourceBuffer;

    TKDT24DD_DyanmicStoreBuffer = array of TKDT24DD_Source;
    PKDT24DD_DyanmicStoreBuffer = ^TKDT24DD_DyanmicStoreBuffer;

    PKDT24DD_Node = ^TKDT24DD_Node;

    TKDT24DD_Node = record
      Parent, Right, Left: PKDT24DD_Node;
      vec: PKDT24DD_Source;
    end;

    TKDT24DD_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT24DD_Source; const Data: Pointer);
    TKDT24DD_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT24DD_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT24DD_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT24DD_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT24DD_DyanmicStoreBuffer;
    KDBuff: TKDT24DD_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT24DD_Node;
    TestBuff: TKDT24DD_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT24DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT24DD_Node;
    function GetData(const Index: NativeInt): PKDT24DD_Source;
  public
    RootNode: PKDT24DD_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT24DD_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT24DD_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DD_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DD_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DD_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT24DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT24DD_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DD_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DD_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DD_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT24DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT24DD_Node; overload;
    function Search(const buff: TKDT24DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT24DD_Node; overload;
    function Search(const buff: TKDT24DD_Vec; var SearchedDistanceMin: Double): PKDT24DD_Node; overload;
    function Search(const buff: TKDT24DD_Vec): PKDT24DD_Node; overload;
    function SearchToken(const buff: TKDT24DD_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT24DD_DynamicVecBuffer; var OutBuff: TKDT24DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT24DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT24DD_Node);
    procedure PrintBuffer;

    class function KDT24DDVec(const s: SystemString): TKDT24DD_Vec; overload;
    class function KDT24DDVec(const v: TKDT24DD_Vec): SystemString; overload;
    class function KDT24DDPow(const v: TKDT24DD_VecType): Double;
    class function KDT24DDDistance(const v1, v2: TKDT24DD_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT24DD_Source; const Data: Pointer);
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
  SaveToken = $11;



function TKDT1DD.InternalBuildKdTree(const KDSourceBufferPtr: PKDT1DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT1DD_Node;
  function SortCompare(const p1, p2: PKDT1DD_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT1DD_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT1DD_Source;
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
  kdBuffPtr: PKDT1DD_SourceBuffer;
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
      axis := Depth mod KDT1DD_Axis;
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

function TKDT1DD.GetData(const Index: NativeInt): PKDT1DD_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT1DD.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT1DD.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT1DD.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT1DD_Node(KDNodes[i]));
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

function TKDT1DD.StoreBuffPtr: PKDT1DD_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT1DD.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DD_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT1DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT1DD.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DD_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT1DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT1DD.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DD_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT1DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT1DD.BuildKDTreeWithCluster(const inBuff: TKDT1DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT1DD_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT1DD_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT1DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1DD_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT1DD.BuildKDTreeWithCluster(const inBuff: TKDT1DD_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT1DD.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DD_BuildCall);
var
  TempStoreBuff: TKDT1DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT1DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT1DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT1DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT1DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1DD_Axis - 1 do
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

procedure TKDT1DD.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DD_BuildMethod);
var
  TempStoreBuff: TKDT1DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT1DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT1DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT1DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT1DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1DD_Axis - 1 do
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


procedure TKDT1DD.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DD_BuildProc);
var
  TempStoreBuff: TKDT1DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT1DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT1DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT1DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT1DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1DD_Axis - 1 do
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


function TKDT1DD.Search(const buff: TKDT1DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT1DD_Node;

var
  NearestNeighbour: PKDT1DD_Node;

  function FindParentNode(const buffPtr: PKDT1DD_Vec; NodePtr: PKDT1DD_Node): PKDT1DD_Node;
  var
    Next: PKDT1DD_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT1DD_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT1DD_Node; const buffPtr: PKDT1DD_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT1DDDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT1DD_Axis;
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

  function SortCompare(const buffPtr: PKDT1DD_Vec; const p1, p2: PKDT1DD_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT1DDDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT1DDDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT1DD_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT1DD_Node;
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
  Parent: PKDT1DD_Node;
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

  SearchedDistanceMin := KDT1DDDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT1DD_Node(NearestNodes[0]);
    end;
end;

function TKDT1DD.Search(const buff: TKDT1DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT1DD_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT1DD.Search(const buff: TKDT1DD_Vec; var SearchedDistanceMin: Double): PKDT1DD_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT1DD.Search(const buff: TKDT1DD_Vec): PKDT1DD_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT1DD.SearchToken(const buff: TKDT1DD_Vec): TPascalString;
var
  p: PKDT1DD_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT1DD.Search(const inBuff: TKDT1DD_DynamicVecBuffer; var OutBuff: TKDT1DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT1DD_DynamicVecBuffer;
  outBuffPtr: PKDT1DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT1DD_Node;
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
        p: PKDT1DD_Node;
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
  p: PKDT1DD_Node;
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


procedure TKDT1DD.Search(const inBuff: TKDT1DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT1DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT1DD_Node;
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
        p: PKDT1DD_Node;
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
  p: PKDT1DD_Node;
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


procedure TKDT1DD.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT1DD_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT1DD_Vec));
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

procedure TKDT1DD.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT1DD_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT1DD_Vec)) <> SizeOf(TKDT1DD_Vec) then
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

procedure TKDT1DD.SaveToFile(FileName: SystemString);
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

procedure TKDT1DD.LoadFromFile(FileName: SystemString);
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

procedure TKDT1DD.PrintNodeTree(const NodePtr: PKDT1DD_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT1DD_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT1DDVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT1DD.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT1DDVec(KDStoreBuff[i].buff)]);
end;

class function TKDT1DD.KDT1DDVec(const s: SystemString): TKDT1DD_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT1DD_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT1DD_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT1DD.KDT1DDVec(const v: TKDT1DD_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT1DD_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT1DD.KDT1DDPow(const v: TKDT1DD_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT1DD.KDT1DDDistance(const v1, v2: TKDT1DD_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT1DD_Axis - 1 do
      Result := Result + KDT1DDPow(v2[i] - v1[i]);
end;

procedure TKDT1DD.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT1DD_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT1DD.Test;
var
  TKDT1DD_Test: TKDT1DD;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT1DD_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT1DD_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT1DD_Test := TKDT1DD.Create;

  DoStatusNoLn('...');
  SetLength(TKDT1DD_Test.TestBuff, 1000);
  for i := 0 to length(TKDT1DD_Test.TestBuff) - 1 do
    for j := 0 to KDT1DD_Axis - 1 do
        TKDT1DD_Test.TestBuff[i][j] := i * KDT1DD_Axis + j;

{$IFDEF FPC}
  TKDT1DD_Test.BuildKDTreeM(length(TKDT1DD_Test.TestBuff), nil, @TKDT1DD_Test.Test_BuildM);
{$ELSE FPC}
  TKDT1DD_Test.BuildKDTreeM(length(TKDT1DD_Test.TestBuff), nil, TKDT1DD_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT1DD_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT1DD_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT1DD_Test.TestBuff) - 1 do
    begin
      p := TKDT1DD_Test.Search(TKDT1DD_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT1DD_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT1DD_Test.TestBuff));
      TKDT1DD_Test.Search(TKDT1DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT1DDDistance(TKDT1DD_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT1DD_Test.Clear;
      { kMean test }
      TKDT1DD_Test.BuildKDTreeWithCluster(TKDT1DD_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT1DD_Test.Search(TKDT1DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT1DD_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT1DD_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT1DD_Test);
end;


function TKDT2DD.InternalBuildKdTree(const KDSourceBufferPtr: PKDT2DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT2DD_Node;
  function SortCompare(const p1, p2: PKDT2DD_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT2DD_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT2DD_Source;
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
  kdBuffPtr: PKDT2DD_SourceBuffer;
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
      axis := Depth mod KDT2DD_Axis;
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

function TKDT2DD.GetData(const Index: NativeInt): PKDT2DD_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT2DD.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT2DD.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT2DD.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT2DD_Node(KDNodes[i]));
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

function TKDT2DD.StoreBuffPtr: PKDT2DD_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT2DD.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DD_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT2DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT2DD.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DD_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT2DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT2DD.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DD_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT2DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT2DD.BuildKDTreeWithCluster(const inBuff: TKDT2DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT2DD_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT2DD_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT2DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT2DD_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT2DD.BuildKDTreeWithCluster(const inBuff: TKDT2DD_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT2DD.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DD_BuildCall);
var
  TempStoreBuff: TKDT2DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT2DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT2DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT2DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT2DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT2DD_Axis - 1 do
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

procedure TKDT2DD.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DD_BuildMethod);
var
  TempStoreBuff: TKDT2DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT2DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT2DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT2DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT2DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT2DD_Axis - 1 do
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


procedure TKDT2DD.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DD_BuildProc);
var
  TempStoreBuff: TKDT2DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT2DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT2DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT2DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT2DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT2DD_Axis - 1 do
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


function TKDT2DD.Search(const buff: TKDT2DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT2DD_Node;

var
  NearestNeighbour: PKDT2DD_Node;

  function FindParentNode(const buffPtr: PKDT2DD_Vec; NodePtr: PKDT2DD_Node): PKDT2DD_Node;
  var
    Next: PKDT2DD_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT2DD_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT2DD_Node; const buffPtr: PKDT2DD_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT2DDDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT2DD_Axis;
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

  function SortCompare(const buffPtr: PKDT2DD_Vec; const p1, p2: PKDT2DD_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT2DDDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT2DDDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT2DD_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT2DD_Node;
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
  Parent: PKDT2DD_Node;
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

  SearchedDistanceMin := KDT2DDDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT2DD_Node(NearestNodes[0]);
    end;
end;

function TKDT2DD.Search(const buff: TKDT2DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT2DD_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT2DD.Search(const buff: TKDT2DD_Vec; var SearchedDistanceMin: Double): PKDT2DD_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT2DD.Search(const buff: TKDT2DD_Vec): PKDT2DD_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT2DD.SearchToken(const buff: TKDT2DD_Vec): TPascalString;
var
  p: PKDT2DD_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT2DD.Search(const inBuff: TKDT2DD_DynamicVecBuffer; var OutBuff: TKDT2DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT2DD_DynamicVecBuffer;
  outBuffPtr: PKDT2DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT2DD_Node;
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
        p: PKDT2DD_Node;
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
  p: PKDT2DD_Node;
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


procedure TKDT2DD.Search(const inBuff: TKDT2DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT2DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT2DD_Node;
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
        p: PKDT2DD_Node;
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
  p: PKDT2DD_Node;
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


procedure TKDT2DD.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT2DD_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT2DD_Vec));
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

procedure TKDT2DD.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT2DD_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT2DD_Vec)) <> SizeOf(TKDT2DD_Vec) then
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

procedure TKDT2DD.SaveToFile(FileName: SystemString);
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

procedure TKDT2DD.LoadFromFile(FileName: SystemString);
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

procedure TKDT2DD.PrintNodeTree(const NodePtr: PKDT2DD_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT2DD_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT2DDVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT2DD.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT2DDVec(KDStoreBuff[i].buff)]);
end;

class function TKDT2DD.KDT2DDVec(const s: SystemString): TKDT2DD_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT2DD_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT2DD_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT2DD.KDT2DDVec(const v: TKDT2DD_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT2DD_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT2DD.KDT2DDPow(const v: TKDT2DD_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT2DD.KDT2DDDistance(const v1, v2: TKDT2DD_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT2DD_Axis - 1 do
      Result := Result + KDT2DDPow(v2[i] - v1[i]);
end;

procedure TKDT2DD.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT2DD_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT2DD.Test;
var
  TKDT2DD_Test: TKDT2DD;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT2DD_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT2DD_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT2DD_Test := TKDT2DD.Create;

  DoStatusNoLn('...');
  SetLength(TKDT2DD_Test.TestBuff, 1000);
  for i := 0 to length(TKDT2DD_Test.TestBuff) - 1 do
    for j := 0 to KDT2DD_Axis - 1 do
        TKDT2DD_Test.TestBuff[i][j] := i * KDT2DD_Axis + j;

{$IFDEF FPC}
  TKDT2DD_Test.BuildKDTreeM(length(TKDT2DD_Test.TestBuff), nil, @TKDT2DD_Test.Test_BuildM);
{$ELSE FPC}
  TKDT2DD_Test.BuildKDTreeM(length(TKDT2DD_Test.TestBuff), nil, TKDT2DD_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT2DD_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT2DD_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT2DD_Test.TestBuff) - 1 do
    begin
      p := TKDT2DD_Test.Search(TKDT2DD_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT2DD_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT2DD_Test.TestBuff));
      TKDT2DD_Test.Search(TKDT2DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT2DDDistance(TKDT2DD_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT2DD_Test.Clear;
      { kMean test }
      TKDT2DD_Test.BuildKDTreeWithCluster(TKDT2DD_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT2DD_Test.Search(TKDT2DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT2DD_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT2DD_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT2DD_Test);
end;


function TKDT3DD.InternalBuildKdTree(const KDSourceBufferPtr: PKDT3DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT3DD_Node;
  function SortCompare(const p1, p2: PKDT3DD_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT3DD_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT3DD_Source;
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
  kdBuffPtr: PKDT3DD_SourceBuffer;
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
      axis := Depth mod KDT3DD_Axis;
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

function TKDT3DD.GetData(const Index: NativeInt): PKDT3DD_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT3DD.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT3DD.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT3DD.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT3DD_Node(KDNodes[i]));
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

function TKDT3DD.StoreBuffPtr: PKDT3DD_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT3DD.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DD_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT3DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT3DD.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DD_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT3DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT3DD.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DD_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT3DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT3DD.BuildKDTreeWithCluster(const inBuff: TKDT3DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT3DD_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT3DD_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT3DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT3DD_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT3DD.BuildKDTreeWithCluster(const inBuff: TKDT3DD_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT3DD.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DD_BuildCall);
var
  TempStoreBuff: TKDT3DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT3DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT3DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT3DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT3DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT3DD_Axis - 1 do
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

procedure TKDT3DD.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DD_BuildMethod);
var
  TempStoreBuff: TKDT3DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT3DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT3DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT3DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT3DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT3DD_Axis - 1 do
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


procedure TKDT3DD.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DD_BuildProc);
var
  TempStoreBuff: TKDT3DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT3DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT3DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT3DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT3DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT3DD_Axis - 1 do
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


function TKDT3DD.Search(const buff: TKDT3DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT3DD_Node;

var
  NearestNeighbour: PKDT3DD_Node;

  function FindParentNode(const buffPtr: PKDT3DD_Vec; NodePtr: PKDT3DD_Node): PKDT3DD_Node;
  var
    Next: PKDT3DD_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT3DD_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT3DD_Node; const buffPtr: PKDT3DD_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT3DDDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT3DD_Axis;
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

  function SortCompare(const buffPtr: PKDT3DD_Vec; const p1, p2: PKDT3DD_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT3DDDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT3DDDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT3DD_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT3DD_Node;
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
  Parent: PKDT3DD_Node;
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

  SearchedDistanceMin := KDT3DDDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT3DD_Node(NearestNodes[0]);
    end;
end;

function TKDT3DD.Search(const buff: TKDT3DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT3DD_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT3DD.Search(const buff: TKDT3DD_Vec; var SearchedDistanceMin: Double): PKDT3DD_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT3DD.Search(const buff: TKDT3DD_Vec): PKDT3DD_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT3DD.SearchToken(const buff: TKDT3DD_Vec): TPascalString;
var
  p: PKDT3DD_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT3DD.Search(const inBuff: TKDT3DD_DynamicVecBuffer; var OutBuff: TKDT3DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT3DD_DynamicVecBuffer;
  outBuffPtr: PKDT3DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT3DD_Node;
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
        p: PKDT3DD_Node;
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
  p: PKDT3DD_Node;
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


procedure TKDT3DD.Search(const inBuff: TKDT3DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT3DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT3DD_Node;
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
        p: PKDT3DD_Node;
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
  p: PKDT3DD_Node;
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


procedure TKDT3DD.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT3DD_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT3DD_Vec));
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

procedure TKDT3DD.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT3DD_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT3DD_Vec)) <> SizeOf(TKDT3DD_Vec) then
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

procedure TKDT3DD.SaveToFile(FileName: SystemString);
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

procedure TKDT3DD.LoadFromFile(FileName: SystemString);
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

procedure TKDT3DD.PrintNodeTree(const NodePtr: PKDT3DD_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT3DD_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT3DDVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT3DD.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT3DDVec(KDStoreBuff[i].buff)]);
end;

class function TKDT3DD.KDT3DDVec(const s: SystemString): TKDT3DD_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT3DD_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT3DD_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT3DD.KDT3DDVec(const v: TKDT3DD_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT3DD_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT3DD.KDT3DDPow(const v: TKDT3DD_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT3DD.KDT3DDDistance(const v1, v2: TKDT3DD_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT3DD_Axis - 1 do
      Result := Result + KDT3DDPow(v2[i] - v1[i]);
end;

procedure TKDT3DD.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT3DD_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT3DD.Test;
var
  TKDT3DD_Test: TKDT3DD;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT3DD_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT3DD_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT3DD_Test := TKDT3DD.Create;

  DoStatusNoLn('...');
  SetLength(TKDT3DD_Test.TestBuff, 1000);
  for i := 0 to length(TKDT3DD_Test.TestBuff) - 1 do
    for j := 0 to KDT3DD_Axis - 1 do
        TKDT3DD_Test.TestBuff[i][j] := i * KDT3DD_Axis + j;

{$IFDEF FPC}
  TKDT3DD_Test.BuildKDTreeM(length(TKDT3DD_Test.TestBuff), nil, @TKDT3DD_Test.Test_BuildM);
{$ELSE FPC}
  TKDT3DD_Test.BuildKDTreeM(length(TKDT3DD_Test.TestBuff), nil, TKDT3DD_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT3DD_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT3DD_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT3DD_Test.TestBuff) - 1 do
    begin
      p := TKDT3DD_Test.Search(TKDT3DD_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT3DD_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT3DD_Test.TestBuff));
      TKDT3DD_Test.Search(TKDT3DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT3DDDistance(TKDT3DD_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT3DD_Test.Clear;
      { kMean test }
      TKDT3DD_Test.BuildKDTreeWithCluster(TKDT3DD_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT3DD_Test.Search(TKDT3DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT3DD_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT3DD_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT3DD_Test);
end;


function TKDT4DD.InternalBuildKdTree(const KDSourceBufferPtr: PKDT4DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT4DD_Node;
  function SortCompare(const p1, p2: PKDT4DD_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT4DD_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT4DD_Source;
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
  kdBuffPtr: PKDT4DD_SourceBuffer;
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
      axis := Depth mod KDT4DD_Axis;
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

function TKDT4DD.GetData(const Index: NativeInt): PKDT4DD_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT4DD.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT4DD.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT4DD.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT4DD_Node(KDNodes[i]));
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

function TKDT4DD.StoreBuffPtr: PKDT4DD_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT4DD.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DD_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT4DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT4DD.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DD_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT4DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT4DD.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DD_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT4DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT4DD.BuildKDTreeWithCluster(const inBuff: TKDT4DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT4DD_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT4DD_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT4DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT4DD_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT4DD.BuildKDTreeWithCluster(const inBuff: TKDT4DD_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT4DD.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DD_BuildCall);
var
  TempStoreBuff: TKDT4DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT4DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT4DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT4DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT4DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT4DD_Axis - 1 do
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

procedure TKDT4DD.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DD_BuildMethod);
var
  TempStoreBuff: TKDT4DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT4DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT4DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT4DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT4DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT4DD_Axis - 1 do
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


procedure TKDT4DD.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DD_BuildProc);
var
  TempStoreBuff: TKDT4DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT4DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT4DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT4DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT4DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT4DD_Axis - 1 do
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


function TKDT4DD.Search(const buff: TKDT4DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT4DD_Node;

var
  NearestNeighbour: PKDT4DD_Node;

  function FindParentNode(const buffPtr: PKDT4DD_Vec; NodePtr: PKDT4DD_Node): PKDT4DD_Node;
  var
    Next: PKDT4DD_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT4DD_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT4DD_Node; const buffPtr: PKDT4DD_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT4DDDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT4DD_Axis;
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

  function SortCompare(const buffPtr: PKDT4DD_Vec; const p1, p2: PKDT4DD_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT4DDDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT4DDDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT4DD_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT4DD_Node;
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
  Parent: PKDT4DD_Node;
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

  SearchedDistanceMin := KDT4DDDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT4DD_Node(NearestNodes[0]);
    end;
end;

function TKDT4DD.Search(const buff: TKDT4DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT4DD_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT4DD.Search(const buff: TKDT4DD_Vec; var SearchedDistanceMin: Double): PKDT4DD_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT4DD.Search(const buff: TKDT4DD_Vec): PKDT4DD_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT4DD.SearchToken(const buff: TKDT4DD_Vec): TPascalString;
var
  p: PKDT4DD_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT4DD.Search(const inBuff: TKDT4DD_DynamicVecBuffer; var OutBuff: TKDT4DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT4DD_DynamicVecBuffer;
  outBuffPtr: PKDT4DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT4DD_Node;
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
        p: PKDT4DD_Node;
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
  p: PKDT4DD_Node;
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


procedure TKDT4DD.Search(const inBuff: TKDT4DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT4DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT4DD_Node;
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
        p: PKDT4DD_Node;
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
  p: PKDT4DD_Node;
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


procedure TKDT4DD.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT4DD_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT4DD_Vec));
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

procedure TKDT4DD.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT4DD_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT4DD_Vec)) <> SizeOf(TKDT4DD_Vec) then
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

procedure TKDT4DD.SaveToFile(FileName: SystemString);
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

procedure TKDT4DD.LoadFromFile(FileName: SystemString);
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

procedure TKDT4DD.PrintNodeTree(const NodePtr: PKDT4DD_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT4DD_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT4DDVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT4DD.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT4DDVec(KDStoreBuff[i].buff)]);
end;

class function TKDT4DD.KDT4DDVec(const s: SystemString): TKDT4DD_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT4DD_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT4DD_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT4DD.KDT4DDVec(const v: TKDT4DD_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT4DD_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT4DD.KDT4DDPow(const v: TKDT4DD_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT4DD.KDT4DDDistance(const v1, v2: TKDT4DD_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT4DD_Axis - 1 do
      Result := Result + KDT4DDPow(v2[i] - v1[i]);
end;

procedure TKDT4DD.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT4DD_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT4DD.Test;
var
  TKDT4DD_Test: TKDT4DD;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT4DD_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT4DD_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT4DD_Test := TKDT4DD.Create;

  DoStatusNoLn('...');
  SetLength(TKDT4DD_Test.TestBuff, 1000);
  for i := 0 to length(TKDT4DD_Test.TestBuff) - 1 do
    for j := 0 to KDT4DD_Axis - 1 do
        TKDT4DD_Test.TestBuff[i][j] := i * KDT4DD_Axis + j;

{$IFDEF FPC}
  TKDT4DD_Test.BuildKDTreeM(length(TKDT4DD_Test.TestBuff), nil, @TKDT4DD_Test.Test_BuildM);
{$ELSE FPC}
  TKDT4DD_Test.BuildKDTreeM(length(TKDT4DD_Test.TestBuff), nil, TKDT4DD_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT4DD_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT4DD_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT4DD_Test.TestBuff) - 1 do
    begin
      p := TKDT4DD_Test.Search(TKDT4DD_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT4DD_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT4DD_Test.TestBuff));
      TKDT4DD_Test.Search(TKDT4DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT4DDDistance(TKDT4DD_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT4DD_Test.Clear;
      { kMean test }
      TKDT4DD_Test.BuildKDTreeWithCluster(TKDT4DD_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT4DD_Test.Search(TKDT4DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT4DD_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT4DD_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT4DD_Test);
end;


function TKDT5DD.InternalBuildKdTree(const KDSourceBufferPtr: PKDT5DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT5DD_Node;
  function SortCompare(const p1, p2: PKDT5DD_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT5DD_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT5DD_Source;
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
  kdBuffPtr: PKDT5DD_SourceBuffer;
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
      axis := Depth mod KDT5DD_Axis;
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

function TKDT5DD.GetData(const Index: NativeInt): PKDT5DD_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT5DD.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT5DD.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT5DD.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT5DD_Node(KDNodes[i]));
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

function TKDT5DD.StoreBuffPtr: PKDT5DD_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT5DD.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DD_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT5DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT5DD.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DD_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT5DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT5DD.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DD_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT5DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT5DD.BuildKDTreeWithCluster(const inBuff: TKDT5DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT5DD_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT5DD_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT5DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT5DD_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT5DD.BuildKDTreeWithCluster(const inBuff: TKDT5DD_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT5DD.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DD_BuildCall);
var
  TempStoreBuff: TKDT5DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT5DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT5DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT5DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT5DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT5DD_Axis - 1 do
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

procedure TKDT5DD.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DD_BuildMethod);
var
  TempStoreBuff: TKDT5DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT5DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT5DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT5DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT5DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT5DD_Axis - 1 do
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


procedure TKDT5DD.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DD_BuildProc);
var
  TempStoreBuff: TKDT5DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT5DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT5DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT5DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT5DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT5DD_Axis - 1 do
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


function TKDT5DD.Search(const buff: TKDT5DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT5DD_Node;

var
  NearestNeighbour: PKDT5DD_Node;

  function FindParentNode(const buffPtr: PKDT5DD_Vec; NodePtr: PKDT5DD_Node): PKDT5DD_Node;
  var
    Next: PKDT5DD_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT5DD_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT5DD_Node; const buffPtr: PKDT5DD_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT5DDDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT5DD_Axis;
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

  function SortCompare(const buffPtr: PKDT5DD_Vec; const p1, p2: PKDT5DD_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT5DDDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT5DDDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT5DD_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT5DD_Node;
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
  Parent: PKDT5DD_Node;
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

  SearchedDistanceMin := KDT5DDDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT5DD_Node(NearestNodes[0]);
    end;
end;

function TKDT5DD.Search(const buff: TKDT5DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT5DD_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT5DD.Search(const buff: TKDT5DD_Vec; var SearchedDistanceMin: Double): PKDT5DD_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT5DD.Search(const buff: TKDT5DD_Vec): PKDT5DD_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT5DD.SearchToken(const buff: TKDT5DD_Vec): TPascalString;
var
  p: PKDT5DD_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT5DD.Search(const inBuff: TKDT5DD_DynamicVecBuffer; var OutBuff: TKDT5DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT5DD_DynamicVecBuffer;
  outBuffPtr: PKDT5DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT5DD_Node;
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
        p: PKDT5DD_Node;
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
  p: PKDT5DD_Node;
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


procedure TKDT5DD.Search(const inBuff: TKDT5DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT5DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT5DD_Node;
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
        p: PKDT5DD_Node;
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
  p: PKDT5DD_Node;
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


procedure TKDT5DD.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT5DD_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT5DD_Vec));
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

procedure TKDT5DD.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT5DD_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT5DD_Vec)) <> SizeOf(TKDT5DD_Vec) then
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

procedure TKDT5DD.SaveToFile(FileName: SystemString);
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

procedure TKDT5DD.LoadFromFile(FileName: SystemString);
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

procedure TKDT5DD.PrintNodeTree(const NodePtr: PKDT5DD_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT5DD_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT5DDVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT5DD.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT5DDVec(KDStoreBuff[i].buff)]);
end;

class function TKDT5DD.KDT5DDVec(const s: SystemString): TKDT5DD_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT5DD_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT5DD_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT5DD.KDT5DDVec(const v: TKDT5DD_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT5DD_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT5DD.KDT5DDPow(const v: TKDT5DD_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT5DD.KDT5DDDistance(const v1, v2: TKDT5DD_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT5DD_Axis - 1 do
      Result := Result + KDT5DDPow(v2[i] - v1[i]);
end;

procedure TKDT5DD.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT5DD_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT5DD.Test;
var
  TKDT5DD_Test: TKDT5DD;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT5DD_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT5DD_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT5DD_Test := TKDT5DD.Create;

  DoStatusNoLn('...');
  SetLength(TKDT5DD_Test.TestBuff, 1000);
  for i := 0 to length(TKDT5DD_Test.TestBuff) - 1 do
    for j := 0 to KDT5DD_Axis - 1 do
        TKDT5DD_Test.TestBuff[i][j] := i * KDT5DD_Axis + j;

{$IFDEF FPC}
  TKDT5DD_Test.BuildKDTreeM(length(TKDT5DD_Test.TestBuff), nil, @TKDT5DD_Test.Test_BuildM);
{$ELSE FPC}
  TKDT5DD_Test.BuildKDTreeM(length(TKDT5DD_Test.TestBuff), nil, TKDT5DD_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT5DD_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT5DD_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT5DD_Test.TestBuff) - 1 do
    begin
      p := TKDT5DD_Test.Search(TKDT5DD_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT5DD_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT5DD_Test.TestBuff));
      TKDT5DD_Test.Search(TKDT5DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT5DDDistance(TKDT5DD_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT5DD_Test.Clear;
      { kMean test }
      TKDT5DD_Test.BuildKDTreeWithCluster(TKDT5DD_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT5DD_Test.Search(TKDT5DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT5DD_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT5DD_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT5DD_Test);
end;


function TKDT6DD.InternalBuildKdTree(const KDSourceBufferPtr: PKDT6DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT6DD_Node;
  function SortCompare(const p1, p2: PKDT6DD_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT6DD_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT6DD_Source;
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
  kdBuffPtr: PKDT6DD_SourceBuffer;
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
      axis := Depth mod KDT6DD_Axis;
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

function TKDT6DD.GetData(const Index: NativeInt): PKDT6DD_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT6DD.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT6DD.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT6DD.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT6DD_Node(KDNodes[i]));
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

function TKDT6DD.StoreBuffPtr: PKDT6DD_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT6DD.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DD_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT6DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT6DD.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DD_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT6DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT6DD.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DD_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT6DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT6DD.BuildKDTreeWithCluster(const inBuff: TKDT6DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT6DD_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT6DD_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT6DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT6DD_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT6DD.BuildKDTreeWithCluster(const inBuff: TKDT6DD_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT6DD.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DD_BuildCall);
var
  TempStoreBuff: TKDT6DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT6DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT6DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT6DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT6DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT6DD_Axis - 1 do
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

procedure TKDT6DD.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DD_BuildMethod);
var
  TempStoreBuff: TKDT6DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT6DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT6DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT6DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT6DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT6DD_Axis - 1 do
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


procedure TKDT6DD.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DD_BuildProc);
var
  TempStoreBuff: TKDT6DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT6DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT6DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT6DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT6DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT6DD_Axis - 1 do
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


function TKDT6DD.Search(const buff: TKDT6DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT6DD_Node;

var
  NearestNeighbour: PKDT6DD_Node;

  function FindParentNode(const buffPtr: PKDT6DD_Vec; NodePtr: PKDT6DD_Node): PKDT6DD_Node;
  var
    Next: PKDT6DD_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT6DD_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT6DD_Node; const buffPtr: PKDT6DD_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT6DDDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT6DD_Axis;
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

  function SortCompare(const buffPtr: PKDT6DD_Vec; const p1, p2: PKDT6DD_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT6DDDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT6DDDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT6DD_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT6DD_Node;
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
  Parent: PKDT6DD_Node;
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

  SearchedDistanceMin := KDT6DDDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT6DD_Node(NearestNodes[0]);
    end;
end;

function TKDT6DD.Search(const buff: TKDT6DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT6DD_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT6DD.Search(const buff: TKDT6DD_Vec; var SearchedDistanceMin: Double): PKDT6DD_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT6DD.Search(const buff: TKDT6DD_Vec): PKDT6DD_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT6DD.SearchToken(const buff: TKDT6DD_Vec): TPascalString;
var
  p: PKDT6DD_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT6DD.Search(const inBuff: TKDT6DD_DynamicVecBuffer; var OutBuff: TKDT6DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT6DD_DynamicVecBuffer;
  outBuffPtr: PKDT6DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT6DD_Node;
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
        p: PKDT6DD_Node;
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
  p: PKDT6DD_Node;
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


procedure TKDT6DD.Search(const inBuff: TKDT6DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT6DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT6DD_Node;
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
        p: PKDT6DD_Node;
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
  p: PKDT6DD_Node;
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


procedure TKDT6DD.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT6DD_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT6DD_Vec));
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

procedure TKDT6DD.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT6DD_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT6DD_Vec)) <> SizeOf(TKDT6DD_Vec) then
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

procedure TKDT6DD.SaveToFile(FileName: SystemString);
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

procedure TKDT6DD.LoadFromFile(FileName: SystemString);
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

procedure TKDT6DD.PrintNodeTree(const NodePtr: PKDT6DD_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT6DD_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT6DDVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT6DD.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT6DDVec(KDStoreBuff[i].buff)]);
end;

class function TKDT6DD.KDT6DDVec(const s: SystemString): TKDT6DD_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT6DD_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT6DD_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT6DD.KDT6DDVec(const v: TKDT6DD_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT6DD_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT6DD.KDT6DDPow(const v: TKDT6DD_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT6DD.KDT6DDDistance(const v1, v2: TKDT6DD_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT6DD_Axis - 1 do
      Result := Result + KDT6DDPow(v2[i] - v1[i]);
end;

procedure TKDT6DD.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT6DD_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT6DD.Test;
var
  TKDT6DD_Test: TKDT6DD;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT6DD_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT6DD_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT6DD_Test := TKDT6DD.Create;

  DoStatusNoLn('...');
  SetLength(TKDT6DD_Test.TestBuff, 1000);
  for i := 0 to length(TKDT6DD_Test.TestBuff) - 1 do
    for j := 0 to KDT6DD_Axis - 1 do
        TKDT6DD_Test.TestBuff[i][j] := i * KDT6DD_Axis + j;

{$IFDEF FPC}
  TKDT6DD_Test.BuildKDTreeM(length(TKDT6DD_Test.TestBuff), nil, @TKDT6DD_Test.Test_BuildM);
{$ELSE FPC}
  TKDT6DD_Test.BuildKDTreeM(length(TKDT6DD_Test.TestBuff), nil, TKDT6DD_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT6DD_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT6DD_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT6DD_Test.TestBuff) - 1 do
    begin
      p := TKDT6DD_Test.Search(TKDT6DD_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT6DD_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT6DD_Test.TestBuff));
      TKDT6DD_Test.Search(TKDT6DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT6DDDistance(TKDT6DD_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT6DD_Test.Clear;
      { kMean test }
      TKDT6DD_Test.BuildKDTreeWithCluster(TKDT6DD_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT6DD_Test.Search(TKDT6DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT6DD_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT6DD_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT6DD_Test);
end;


function TKDT7DD.InternalBuildKdTree(const KDSourceBufferPtr: PKDT7DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT7DD_Node;
  function SortCompare(const p1, p2: PKDT7DD_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT7DD_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT7DD_Source;
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
  kdBuffPtr: PKDT7DD_SourceBuffer;
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
      axis := Depth mod KDT7DD_Axis;
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

function TKDT7DD.GetData(const Index: NativeInt): PKDT7DD_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT7DD.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT7DD.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT7DD.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT7DD_Node(KDNodes[i]));
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

function TKDT7DD.StoreBuffPtr: PKDT7DD_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT7DD.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DD_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT7DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT7DD.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DD_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT7DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT7DD.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DD_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT7DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT7DD.BuildKDTreeWithCluster(const inBuff: TKDT7DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT7DD_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT7DD_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT7DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT7DD_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT7DD.BuildKDTreeWithCluster(const inBuff: TKDT7DD_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT7DD.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DD_BuildCall);
var
  TempStoreBuff: TKDT7DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT7DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT7DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT7DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT7DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT7DD_Axis - 1 do
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

procedure TKDT7DD.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DD_BuildMethod);
var
  TempStoreBuff: TKDT7DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT7DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT7DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT7DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT7DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT7DD_Axis - 1 do
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


procedure TKDT7DD.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DD_BuildProc);
var
  TempStoreBuff: TKDT7DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT7DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT7DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT7DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT7DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT7DD_Axis - 1 do
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


function TKDT7DD.Search(const buff: TKDT7DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT7DD_Node;

var
  NearestNeighbour: PKDT7DD_Node;

  function FindParentNode(const buffPtr: PKDT7DD_Vec; NodePtr: PKDT7DD_Node): PKDT7DD_Node;
  var
    Next: PKDT7DD_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT7DD_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT7DD_Node; const buffPtr: PKDT7DD_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT7DDDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT7DD_Axis;
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

  function SortCompare(const buffPtr: PKDT7DD_Vec; const p1, p2: PKDT7DD_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT7DDDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT7DDDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT7DD_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT7DD_Node;
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
  Parent: PKDT7DD_Node;
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

  SearchedDistanceMin := KDT7DDDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT7DD_Node(NearestNodes[0]);
    end;
end;

function TKDT7DD.Search(const buff: TKDT7DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT7DD_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT7DD.Search(const buff: TKDT7DD_Vec; var SearchedDistanceMin: Double): PKDT7DD_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT7DD.Search(const buff: TKDT7DD_Vec): PKDT7DD_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT7DD.SearchToken(const buff: TKDT7DD_Vec): TPascalString;
var
  p: PKDT7DD_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT7DD.Search(const inBuff: TKDT7DD_DynamicVecBuffer; var OutBuff: TKDT7DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT7DD_DynamicVecBuffer;
  outBuffPtr: PKDT7DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT7DD_Node;
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
        p: PKDT7DD_Node;
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
  p: PKDT7DD_Node;
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


procedure TKDT7DD.Search(const inBuff: TKDT7DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT7DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT7DD_Node;
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
        p: PKDT7DD_Node;
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
  p: PKDT7DD_Node;
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


procedure TKDT7DD.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT7DD_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT7DD_Vec));
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

procedure TKDT7DD.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT7DD_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT7DD_Vec)) <> SizeOf(TKDT7DD_Vec) then
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

procedure TKDT7DD.SaveToFile(FileName: SystemString);
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

procedure TKDT7DD.LoadFromFile(FileName: SystemString);
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

procedure TKDT7DD.PrintNodeTree(const NodePtr: PKDT7DD_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT7DD_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT7DDVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT7DD.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT7DDVec(KDStoreBuff[i].buff)]);
end;

class function TKDT7DD.KDT7DDVec(const s: SystemString): TKDT7DD_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT7DD_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT7DD_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT7DD.KDT7DDVec(const v: TKDT7DD_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT7DD_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT7DD.KDT7DDPow(const v: TKDT7DD_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT7DD.KDT7DDDistance(const v1, v2: TKDT7DD_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT7DD_Axis - 1 do
      Result := Result + KDT7DDPow(v2[i] - v1[i]);
end;

procedure TKDT7DD.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT7DD_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT7DD.Test;
var
  TKDT7DD_Test: TKDT7DD;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT7DD_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT7DD_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT7DD_Test := TKDT7DD.Create;

  DoStatusNoLn('...');
  SetLength(TKDT7DD_Test.TestBuff, 1000);
  for i := 0 to length(TKDT7DD_Test.TestBuff) - 1 do
    for j := 0 to KDT7DD_Axis - 1 do
        TKDT7DD_Test.TestBuff[i][j] := i * KDT7DD_Axis + j;

{$IFDEF FPC}
  TKDT7DD_Test.BuildKDTreeM(length(TKDT7DD_Test.TestBuff), nil, @TKDT7DD_Test.Test_BuildM);
{$ELSE FPC}
  TKDT7DD_Test.BuildKDTreeM(length(TKDT7DD_Test.TestBuff), nil, TKDT7DD_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT7DD_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT7DD_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT7DD_Test.TestBuff) - 1 do
    begin
      p := TKDT7DD_Test.Search(TKDT7DD_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT7DD_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT7DD_Test.TestBuff));
      TKDT7DD_Test.Search(TKDT7DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT7DDDistance(TKDT7DD_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT7DD_Test.Clear;
      { kMean test }
      TKDT7DD_Test.BuildKDTreeWithCluster(TKDT7DD_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT7DD_Test.Search(TKDT7DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT7DD_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT7DD_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT7DD_Test);
end;


function TKDT8DD.InternalBuildKdTree(const KDSourceBufferPtr: PKDT8DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT8DD_Node;
  function SortCompare(const p1, p2: PKDT8DD_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT8DD_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT8DD_Source;
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
  kdBuffPtr: PKDT8DD_SourceBuffer;
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
      axis := Depth mod KDT8DD_Axis;
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

function TKDT8DD.GetData(const Index: NativeInt): PKDT8DD_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT8DD.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT8DD.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT8DD.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT8DD_Node(KDNodes[i]));
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

function TKDT8DD.StoreBuffPtr: PKDT8DD_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT8DD.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DD_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT8DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT8DD.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DD_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT8DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT8DD.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DD_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT8DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT8DD.BuildKDTreeWithCluster(const inBuff: TKDT8DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT8DD_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT8DD_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT8DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT8DD_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT8DD.BuildKDTreeWithCluster(const inBuff: TKDT8DD_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT8DD.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DD_BuildCall);
var
  TempStoreBuff: TKDT8DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT8DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT8DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT8DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT8DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT8DD_Axis - 1 do
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

procedure TKDT8DD.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DD_BuildMethod);
var
  TempStoreBuff: TKDT8DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT8DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT8DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT8DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT8DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT8DD_Axis - 1 do
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


procedure TKDT8DD.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DD_BuildProc);
var
  TempStoreBuff: TKDT8DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT8DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT8DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT8DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT8DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT8DD_Axis - 1 do
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


function TKDT8DD.Search(const buff: TKDT8DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT8DD_Node;

var
  NearestNeighbour: PKDT8DD_Node;

  function FindParentNode(const buffPtr: PKDT8DD_Vec; NodePtr: PKDT8DD_Node): PKDT8DD_Node;
  var
    Next: PKDT8DD_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT8DD_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT8DD_Node; const buffPtr: PKDT8DD_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT8DDDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT8DD_Axis;
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

  function SortCompare(const buffPtr: PKDT8DD_Vec; const p1, p2: PKDT8DD_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT8DDDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT8DDDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT8DD_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT8DD_Node;
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
  Parent: PKDT8DD_Node;
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

  SearchedDistanceMin := KDT8DDDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT8DD_Node(NearestNodes[0]);
    end;
end;

function TKDT8DD.Search(const buff: TKDT8DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT8DD_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT8DD.Search(const buff: TKDT8DD_Vec; var SearchedDistanceMin: Double): PKDT8DD_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT8DD.Search(const buff: TKDT8DD_Vec): PKDT8DD_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT8DD.SearchToken(const buff: TKDT8DD_Vec): TPascalString;
var
  p: PKDT8DD_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT8DD.Search(const inBuff: TKDT8DD_DynamicVecBuffer; var OutBuff: TKDT8DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT8DD_DynamicVecBuffer;
  outBuffPtr: PKDT8DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT8DD_Node;
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
        p: PKDT8DD_Node;
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
  p: PKDT8DD_Node;
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


procedure TKDT8DD.Search(const inBuff: TKDT8DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT8DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT8DD_Node;
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
        p: PKDT8DD_Node;
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
  p: PKDT8DD_Node;
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


procedure TKDT8DD.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT8DD_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT8DD_Vec));
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

procedure TKDT8DD.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT8DD_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT8DD_Vec)) <> SizeOf(TKDT8DD_Vec) then
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

procedure TKDT8DD.SaveToFile(FileName: SystemString);
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

procedure TKDT8DD.LoadFromFile(FileName: SystemString);
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

procedure TKDT8DD.PrintNodeTree(const NodePtr: PKDT8DD_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT8DD_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT8DDVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT8DD.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT8DDVec(KDStoreBuff[i].buff)]);
end;

class function TKDT8DD.KDT8DDVec(const s: SystemString): TKDT8DD_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT8DD_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT8DD_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT8DD.KDT8DDVec(const v: TKDT8DD_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT8DD_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT8DD.KDT8DDPow(const v: TKDT8DD_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT8DD.KDT8DDDistance(const v1, v2: TKDT8DD_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT8DD_Axis - 1 do
      Result := Result + KDT8DDPow(v2[i] - v1[i]);
end;

procedure TKDT8DD.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT8DD_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT8DD.Test;
var
  TKDT8DD_Test: TKDT8DD;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT8DD_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT8DD_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT8DD_Test := TKDT8DD.Create;

  DoStatusNoLn('...');
  SetLength(TKDT8DD_Test.TestBuff, 1000);
  for i := 0 to length(TKDT8DD_Test.TestBuff) - 1 do
    for j := 0 to KDT8DD_Axis - 1 do
        TKDT8DD_Test.TestBuff[i][j] := i * KDT8DD_Axis + j;

{$IFDEF FPC}
  TKDT8DD_Test.BuildKDTreeM(length(TKDT8DD_Test.TestBuff), nil, @TKDT8DD_Test.Test_BuildM);
{$ELSE FPC}
  TKDT8DD_Test.BuildKDTreeM(length(TKDT8DD_Test.TestBuff), nil, TKDT8DD_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT8DD_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT8DD_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT8DD_Test.TestBuff) - 1 do
    begin
      p := TKDT8DD_Test.Search(TKDT8DD_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT8DD_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT8DD_Test.TestBuff));
      TKDT8DD_Test.Search(TKDT8DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT8DDDistance(TKDT8DD_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT8DD_Test.Clear;
      { kMean test }
      TKDT8DD_Test.BuildKDTreeWithCluster(TKDT8DD_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT8DD_Test.Search(TKDT8DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT8DD_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT8DD_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT8DD_Test);
end;


function TKDT9DD.InternalBuildKdTree(const KDSourceBufferPtr: PKDT9DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT9DD_Node;
  function SortCompare(const p1, p2: PKDT9DD_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT9DD_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT9DD_Source;
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
  kdBuffPtr: PKDT9DD_SourceBuffer;
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
      axis := Depth mod KDT9DD_Axis;
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

function TKDT9DD.GetData(const Index: NativeInt): PKDT9DD_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT9DD.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT9DD.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT9DD.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT9DD_Node(KDNodes[i]));
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

function TKDT9DD.StoreBuffPtr: PKDT9DD_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT9DD.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DD_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT9DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT9DD.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DD_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT9DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT9DD.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DD_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT9DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT9DD.BuildKDTreeWithCluster(const inBuff: TKDT9DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT9DD_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT9DD_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT9DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT9DD_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT9DD.BuildKDTreeWithCluster(const inBuff: TKDT9DD_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT9DD.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DD_BuildCall);
var
  TempStoreBuff: TKDT9DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT9DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT9DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT9DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT9DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT9DD_Axis - 1 do
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

procedure TKDT9DD.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DD_BuildMethod);
var
  TempStoreBuff: TKDT9DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT9DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT9DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT9DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT9DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT9DD_Axis - 1 do
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


procedure TKDT9DD.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DD_BuildProc);
var
  TempStoreBuff: TKDT9DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT9DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT9DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT9DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT9DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT9DD_Axis - 1 do
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


function TKDT9DD.Search(const buff: TKDT9DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT9DD_Node;

var
  NearestNeighbour: PKDT9DD_Node;

  function FindParentNode(const buffPtr: PKDT9DD_Vec; NodePtr: PKDT9DD_Node): PKDT9DD_Node;
  var
    Next: PKDT9DD_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT9DD_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT9DD_Node; const buffPtr: PKDT9DD_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT9DDDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT9DD_Axis;
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

  function SortCompare(const buffPtr: PKDT9DD_Vec; const p1, p2: PKDT9DD_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT9DDDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT9DDDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT9DD_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT9DD_Node;
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
  Parent: PKDT9DD_Node;
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

  SearchedDistanceMin := KDT9DDDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT9DD_Node(NearestNodes[0]);
    end;
end;

function TKDT9DD.Search(const buff: TKDT9DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT9DD_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT9DD.Search(const buff: TKDT9DD_Vec; var SearchedDistanceMin: Double): PKDT9DD_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT9DD.Search(const buff: TKDT9DD_Vec): PKDT9DD_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT9DD.SearchToken(const buff: TKDT9DD_Vec): TPascalString;
var
  p: PKDT9DD_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT9DD.Search(const inBuff: TKDT9DD_DynamicVecBuffer; var OutBuff: TKDT9DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT9DD_DynamicVecBuffer;
  outBuffPtr: PKDT9DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT9DD_Node;
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
        p: PKDT9DD_Node;
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
  p: PKDT9DD_Node;
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


procedure TKDT9DD.Search(const inBuff: TKDT9DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT9DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT9DD_Node;
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
        p: PKDT9DD_Node;
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
  p: PKDT9DD_Node;
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


procedure TKDT9DD.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT9DD_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT9DD_Vec));
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

procedure TKDT9DD.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT9DD_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT9DD_Vec)) <> SizeOf(TKDT9DD_Vec) then
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

procedure TKDT9DD.SaveToFile(FileName: SystemString);
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

procedure TKDT9DD.LoadFromFile(FileName: SystemString);
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

procedure TKDT9DD.PrintNodeTree(const NodePtr: PKDT9DD_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT9DD_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT9DDVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT9DD.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT9DDVec(KDStoreBuff[i].buff)]);
end;

class function TKDT9DD.KDT9DDVec(const s: SystemString): TKDT9DD_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT9DD_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT9DD_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT9DD.KDT9DDVec(const v: TKDT9DD_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT9DD_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT9DD.KDT9DDPow(const v: TKDT9DD_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT9DD.KDT9DDDistance(const v1, v2: TKDT9DD_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT9DD_Axis - 1 do
      Result := Result + KDT9DDPow(v2[i] - v1[i]);
end;

procedure TKDT9DD.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT9DD_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT9DD.Test;
var
  TKDT9DD_Test: TKDT9DD;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT9DD_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT9DD_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT9DD_Test := TKDT9DD.Create;

  DoStatusNoLn('...');
  SetLength(TKDT9DD_Test.TestBuff, 1000);
  for i := 0 to length(TKDT9DD_Test.TestBuff) - 1 do
    for j := 0 to KDT9DD_Axis - 1 do
        TKDT9DD_Test.TestBuff[i][j] := i * KDT9DD_Axis + j;

{$IFDEF FPC}
  TKDT9DD_Test.BuildKDTreeM(length(TKDT9DD_Test.TestBuff), nil, @TKDT9DD_Test.Test_BuildM);
{$ELSE FPC}
  TKDT9DD_Test.BuildKDTreeM(length(TKDT9DD_Test.TestBuff), nil, TKDT9DD_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT9DD_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT9DD_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT9DD_Test.TestBuff) - 1 do
    begin
      p := TKDT9DD_Test.Search(TKDT9DD_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT9DD_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT9DD_Test.TestBuff));
      TKDT9DD_Test.Search(TKDT9DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT9DDDistance(TKDT9DD_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT9DD_Test.Clear;
      { kMean test }
      TKDT9DD_Test.BuildKDTreeWithCluster(TKDT9DD_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT9DD_Test.Search(TKDT9DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT9DD_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT9DD_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT9DD_Test);
end;


function TKDT10DD.InternalBuildKdTree(const KDSourceBufferPtr: PKDT10DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT10DD_Node;
  function SortCompare(const p1, p2: PKDT10DD_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT10DD_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT10DD_Source;
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
  kdBuffPtr: PKDT10DD_SourceBuffer;
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
      axis := Depth mod KDT10DD_Axis;
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

function TKDT10DD.GetData(const Index: NativeInt): PKDT10DD_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT10DD.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT10DD.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT10DD.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT10DD_Node(KDNodes[i]));
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

function TKDT10DD.StoreBuffPtr: PKDT10DD_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT10DD.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DD_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT10DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT10DD.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DD_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT10DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT10DD.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DD_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT10DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT10DD.BuildKDTreeWithCluster(const inBuff: TKDT10DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT10DD_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT10DD_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT10DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT10DD_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT10DD.BuildKDTreeWithCluster(const inBuff: TKDT10DD_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT10DD.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DD_BuildCall);
var
  TempStoreBuff: TKDT10DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT10DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT10DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT10DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT10DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT10DD_Axis - 1 do
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

procedure TKDT10DD.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DD_BuildMethod);
var
  TempStoreBuff: TKDT10DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT10DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT10DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT10DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT10DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT10DD_Axis - 1 do
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


procedure TKDT10DD.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DD_BuildProc);
var
  TempStoreBuff: TKDT10DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT10DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT10DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT10DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT10DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT10DD_Axis - 1 do
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


function TKDT10DD.Search(const buff: TKDT10DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT10DD_Node;

var
  NearestNeighbour: PKDT10DD_Node;

  function FindParentNode(const buffPtr: PKDT10DD_Vec; NodePtr: PKDT10DD_Node): PKDT10DD_Node;
  var
    Next: PKDT10DD_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT10DD_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT10DD_Node; const buffPtr: PKDT10DD_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT10DDDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT10DD_Axis;
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

  function SortCompare(const buffPtr: PKDT10DD_Vec; const p1, p2: PKDT10DD_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT10DDDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT10DDDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT10DD_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT10DD_Node;
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
  Parent: PKDT10DD_Node;
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

  SearchedDistanceMin := KDT10DDDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT10DD_Node(NearestNodes[0]);
    end;
end;

function TKDT10DD.Search(const buff: TKDT10DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT10DD_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT10DD.Search(const buff: TKDT10DD_Vec; var SearchedDistanceMin: Double): PKDT10DD_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT10DD.Search(const buff: TKDT10DD_Vec): PKDT10DD_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT10DD.SearchToken(const buff: TKDT10DD_Vec): TPascalString;
var
  p: PKDT10DD_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT10DD.Search(const inBuff: TKDT10DD_DynamicVecBuffer; var OutBuff: TKDT10DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT10DD_DynamicVecBuffer;
  outBuffPtr: PKDT10DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT10DD_Node;
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
        p: PKDT10DD_Node;
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
  p: PKDT10DD_Node;
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


procedure TKDT10DD.Search(const inBuff: TKDT10DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT10DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT10DD_Node;
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
        p: PKDT10DD_Node;
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
  p: PKDT10DD_Node;
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


procedure TKDT10DD.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT10DD_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT10DD_Vec));
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

procedure TKDT10DD.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT10DD_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT10DD_Vec)) <> SizeOf(TKDT10DD_Vec) then
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

procedure TKDT10DD.SaveToFile(FileName: SystemString);
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

procedure TKDT10DD.LoadFromFile(FileName: SystemString);
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

procedure TKDT10DD.PrintNodeTree(const NodePtr: PKDT10DD_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT10DD_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT10DDVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT10DD.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT10DDVec(KDStoreBuff[i].buff)]);
end;

class function TKDT10DD.KDT10DDVec(const s: SystemString): TKDT10DD_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT10DD_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT10DD_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT10DD.KDT10DDVec(const v: TKDT10DD_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT10DD_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT10DD.KDT10DDPow(const v: TKDT10DD_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT10DD.KDT10DDDistance(const v1, v2: TKDT10DD_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT10DD_Axis - 1 do
      Result := Result + KDT10DDPow(v2[i] - v1[i]);
end;

procedure TKDT10DD.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT10DD_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT10DD.Test;
var
  TKDT10DD_Test: TKDT10DD;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT10DD_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT10DD_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT10DD_Test := TKDT10DD.Create;

  DoStatusNoLn('...');
  SetLength(TKDT10DD_Test.TestBuff, 1000);
  for i := 0 to length(TKDT10DD_Test.TestBuff) - 1 do
    for j := 0 to KDT10DD_Axis - 1 do
        TKDT10DD_Test.TestBuff[i][j] := i * KDT10DD_Axis + j;

{$IFDEF FPC}
  TKDT10DD_Test.BuildKDTreeM(length(TKDT10DD_Test.TestBuff), nil, @TKDT10DD_Test.Test_BuildM);
{$ELSE FPC}
  TKDT10DD_Test.BuildKDTreeM(length(TKDT10DD_Test.TestBuff), nil, TKDT10DD_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT10DD_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT10DD_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT10DD_Test.TestBuff) - 1 do
    begin
      p := TKDT10DD_Test.Search(TKDT10DD_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT10DD_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT10DD_Test.TestBuff));
      TKDT10DD_Test.Search(TKDT10DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT10DDDistance(TKDT10DD_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT10DD_Test.Clear;
      { kMean test }
      TKDT10DD_Test.BuildKDTreeWithCluster(TKDT10DD_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT10DD_Test.Search(TKDT10DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT10DD_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT10DD_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT10DD_Test);
end;


function TKDT11DD.InternalBuildKdTree(const KDSourceBufferPtr: PKDT11DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT11DD_Node;
  function SortCompare(const p1, p2: PKDT11DD_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT11DD_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT11DD_Source;
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
  kdBuffPtr: PKDT11DD_SourceBuffer;
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
      axis := Depth mod KDT11DD_Axis;
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

function TKDT11DD.GetData(const Index: NativeInt): PKDT11DD_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT11DD.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT11DD.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT11DD.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT11DD_Node(KDNodes[i]));
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

function TKDT11DD.StoreBuffPtr: PKDT11DD_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT11DD.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DD_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT11DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT11DD.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DD_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT11DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT11DD.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DD_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT11DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT11DD.BuildKDTreeWithCluster(const inBuff: TKDT11DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT11DD_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT11DD_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT11DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT11DD_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT11DD.BuildKDTreeWithCluster(const inBuff: TKDT11DD_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT11DD.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DD_BuildCall);
var
  TempStoreBuff: TKDT11DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT11DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT11DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT11DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT11DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT11DD_Axis - 1 do
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

procedure TKDT11DD.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DD_BuildMethod);
var
  TempStoreBuff: TKDT11DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT11DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT11DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT11DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT11DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT11DD_Axis - 1 do
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


procedure TKDT11DD.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DD_BuildProc);
var
  TempStoreBuff: TKDT11DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT11DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT11DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT11DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT11DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT11DD_Axis - 1 do
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


function TKDT11DD.Search(const buff: TKDT11DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT11DD_Node;

var
  NearestNeighbour: PKDT11DD_Node;

  function FindParentNode(const buffPtr: PKDT11DD_Vec; NodePtr: PKDT11DD_Node): PKDT11DD_Node;
  var
    Next: PKDT11DD_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT11DD_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT11DD_Node; const buffPtr: PKDT11DD_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT11DDDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT11DD_Axis;
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

  function SortCompare(const buffPtr: PKDT11DD_Vec; const p1, p2: PKDT11DD_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT11DDDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT11DDDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT11DD_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT11DD_Node;
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
  Parent: PKDT11DD_Node;
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

  SearchedDistanceMin := KDT11DDDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT11DD_Node(NearestNodes[0]);
    end;
end;

function TKDT11DD.Search(const buff: TKDT11DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT11DD_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT11DD.Search(const buff: TKDT11DD_Vec; var SearchedDistanceMin: Double): PKDT11DD_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT11DD.Search(const buff: TKDT11DD_Vec): PKDT11DD_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT11DD.SearchToken(const buff: TKDT11DD_Vec): TPascalString;
var
  p: PKDT11DD_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT11DD.Search(const inBuff: TKDT11DD_DynamicVecBuffer; var OutBuff: TKDT11DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT11DD_DynamicVecBuffer;
  outBuffPtr: PKDT11DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT11DD_Node;
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
        p: PKDT11DD_Node;
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
  p: PKDT11DD_Node;
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


procedure TKDT11DD.Search(const inBuff: TKDT11DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT11DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT11DD_Node;
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
        p: PKDT11DD_Node;
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
  p: PKDT11DD_Node;
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


procedure TKDT11DD.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT11DD_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT11DD_Vec));
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

procedure TKDT11DD.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT11DD_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT11DD_Vec)) <> SizeOf(TKDT11DD_Vec) then
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

procedure TKDT11DD.SaveToFile(FileName: SystemString);
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

procedure TKDT11DD.LoadFromFile(FileName: SystemString);
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

procedure TKDT11DD.PrintNodeTree(const NodePtr: PKDT11DD_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT11DD_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT11DDVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT11DD.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT11DDVec(KDStoreBuff[i].buff)]);
end;

class function TKDT11DD.KDT11DDVec(const s: SystemString): TKDT11DD_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT11DD_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT11DD_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT11DD.KDT11DDVec(const v: TKDT11DD_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT11DD_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT11DD.KDT11DDPow(const v: TKDT11DD_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT11DD.KDT11DDDistance(const v1, v2: TKDT11DD_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT11DD_Axis - 1 do
      Result := Result + KDT11DDPow(v2[i] - v1[i]);
end;

procedure TKDT11DD.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT11DD_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT11DD.Test;
var
  TKDT11DD_Test: TKDT11DD;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT11DD_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT11DD_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT11DD_Test := TKDT11DD.Create;

  DoStatusNoLn('...');
  SetLength(TKDT11DD_Test.TestBuff, 1000);
  for i := 0 to length(TKDT11DD_Test.TestBuff) - 1 do
    for j := 0 to KDT11DD_Axis - 1 do
        TKDT11DD_Test.TestBuff[i][j] := i * KDT11DD_Axis + j;

{$IFDEF FPC}
  TKDT11DD_Test.BuildKDTreeM(length(TKDT11DD_Test.TestBuff), nil, @TKDT11DD_Test.Test_BuildM);
{$ELSE FPC}
  TKDT11DD_Test.BuildKDTreeM(length(TKDT11DD_Test.TestBuff), nil, TKDT11DD_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT11DD_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT11DD_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT11DD_Test.TestBuff) - 1 do
    begin
      p := TKDT11DD_Test.Search(TKDT11DD_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT11DD_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT11DD_Test.TestBuff));
      TKDT11DD_Test.Search(TKDT11DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT11DDDistance(TKDT11DD_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT11DD_Test.Clear;
      { kMean test }
      TKDT11DD_Test.BuildKDTreeWithCluster(TKDT11DD_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT11DD_Test.Search(TKDT11DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT11DD_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT11DD_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT11DD_Test);
end;


function TKDT12DD.InternalBuildKdTree(const KDSourceBufferPtr: PKDT12DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT12DD_Node;
  function SortCompare(const p1, p2: PKDT12DD_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT12DD_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT12DD_Source;
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
  kdBuffPtr: PKDT12DD_SourceBuffer;
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
      axis := Depth mod KDT12DD_Axis;
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

function TKDT12DD.GetData(const Index: NativeInt): PKDT12DD_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT12DD.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT12DD.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT12DD.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT12DD_Node(KDNodes[i]));
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

function TKDT12DD.StoreBuffPtr: PKDT12DD_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT12DD.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DD_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT12DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT12DD.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DD_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT12DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT12DD.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DD_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT12DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT12DD.BuildKDTreeWithCluster(const inBuff: TKDT12DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT12DD_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT12DD_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT12DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT12DD_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT12DD.BuildKDTreeWithCluster(const inBuff: TKDT12DD_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT12DD.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DD_BuildCall);
var
  TempStoreBuff: TKDT12DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT12DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT12DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT12DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT12DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT12DD_Axis - 1 do
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

procedure TKDT12DD.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DD_BuildMethod);
var
  TempStoreBuff: TKDT12DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT12DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT12DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT12DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT12DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT12DD_Axis - 1 do
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


procedure TKDT12DD.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DD_BuildProc);
var
  TempStoreBuff: TKDT12DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT12DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT12DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT12DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT12DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT12DD_Axis - 1 do
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


function TKDT12DD.Search(const buff: TKDT12DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT12DD_Node;

var
  NearestNeighbour: PKDT12DD_Node;

  function FindParentNode(const buffPtr: PKDT12DD_Vec; NodePtr: PKDT12DD_Node): PKDT12DD_Node;
  var
    Next: PKDT12DD_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT12DD_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT12DD_Node; const buffPtr: PKDT12DD_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT12DDDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT12DD_Axis;
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

  function SortCompare(const buffPtr: PKDT12DD_Vec; const p1, p2: PKDT12DD_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT12DDDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT12DDDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT12DD_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT12DD_Node;
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
  Parent: PKDT12DD_Node;
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

  SearchedDistanceMin := KDT12DDDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT12DD_Node(NearestNodes[0]);
    end;
end;

function TKDT12DD.Search(const buff: TKDT12DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT12DD_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT12DD.Search(const buff: TKDT12DD_Vec; var SearchedDistanceMin: Double): PKDT12DD_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT12DD.Search(const buff: TKDT12DD_Vec): PKDT12DD_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT12DD.SearchToken(const buff: TKDT12DD_Vec): TPascalString;
var
  p: PKDT12DD_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT12DD.Search(const inBuff: TKDT12DD_DynamicVecBuffer; var OutBuff: TKDT12DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT12DD_DynamicVecBuffer;
  outBuffPtr: PKDT12DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT12DD_Node;
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
        p: PKDT12DD_Node;
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
  p: PKDT12DD_Node;
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


procedure TKDT12DD.Search(const inBuff: TKDT12DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT12DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT12DD_Node;
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
        p: PKDT12DD_Node;
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
  p: PKDT12DD_Node;
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


procedure TKDT12DD.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT12DD_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT12DD_Vec));
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

procedure TKDT12DD.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT12DD_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT12DD_Vec)) <> SizeOf(TKDT12DD_Vec) then
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

procedure TKDT12DD.SaveToFile(FileName: SystemString);
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

procedure TKDT12DD.LoadFromFile(FileName: SystemString);
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

procedure TKDT12DD.PrintNodeTree(const NodePtr: PKDT12DD_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT12DD_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT12DDVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT12DD.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT12DDVec(KDStoreBuff[i].buff)]);
end;

class function TKDT12DD.KDT12DDVec(const s: SystemString): TKDT12DD_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT12DD_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT12DD_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT12DD.KDT12DDVec(const v: TKDT12DD_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT12DD_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT12DD.KDT12DDPow(const v: TKDT12DD_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT12DD.KDT12DDDistance(const v1, v2: TKDT12DD_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT12DD_Axis - 1 do
      Result := Result + KDT12DDPow(v2[i] - v1[i]);
end;

procedure TKDT12DD.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT12DD_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT12DD.Test;
var
  TKDT12DD_Test: TKDT12DD;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT12DD_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT12DD_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT12DD_Test := TKDT12DD.Create;

  DoStatusNoLn('...');
  SetLength(TKDT12DD_Test.TestBuff, 1000);
  for i := 0 to length(TKDT12DD_Test.TestBuff) - 1 do
    for j := 0 to KDT12DD_Axis - 1 do
        TKDT12DD_Test.TestBuff[i][j] := i * KDT12DD_Axis + j;

{$IFDEF FPC}
  TKDT12DD_Test.BuildKDTreeM(length(TKDT12DD_Test.TestBuff), nil, @TKDT12DD_Test.Test_BuildM);
{$ELSE FPC}
  TKDT12DD_Test.BuildKDTreeM(length(TKDT12DD_Test.TestBuff), nil, TKDT12DD_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT12DD_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT12DD_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT12DD_Test.TestBuff) - 1 do
    begin
      p := TKDT12DD_Test.Search(TKDT12DD_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT12DD_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT12DD_Test.TestBuff));
      TKDT12DD_Test.Search(TKDT12DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT12DDDistance(TKDT12DD_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT12DD_Test.Clear;
      { kMean test }
      TKDT12DD_Test.BuildKDTreeWithCluster(TKDT12DD_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT12DD_Test.Search(TKDT12DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT12DD_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT12DD_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT12DD_Test);
end;


function TKDT13DD.InternalBuildKdTree(const KDSourceBufferPtr: PKDT13DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT13DD_Node;
  function SortCompare(const p1, p2: PKDT13DD_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT13DD_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT13DD_Source;
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
  kdBuffPtr: PKDT13DD_SourceBuffer;
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
      axis := Depth mod KDT13DD_Axis;
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

function TKDT13DD.GetData(const Index: NativeInt): PKDT13DD_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT13DD.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT13DD.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT13DD.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT13DD_Node(KDNodes[i]));
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

function TKDT13DD.StoreBuffPtr: PKDT13DD_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT13DD.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DD_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT13DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT13DD.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DD_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT13DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT13DD.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DD_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT13DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT13DD.BuildKDTreeWithCluster(const inBuff: TKDT13DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT13DD_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT13DD_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT13DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT13DD_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT13DD.BuildKDTreeWithCluster(const inBuff: TKDT13DD_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT13DD.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DD_BuildCall);
var
  TempStoreBuff: TKDT13DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT13DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT13DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT13DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT13DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT13DD_Axis - 1 do
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

procedure TKDT13DD.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DD_BuildMethod);
var
  TempStoreBuff: TKDT13DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT13DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT13DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT13DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT13DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT13DD_Axis - 1 do
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


procedure TKDT13DD.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DD_BuildProc);
var
  TempStoreBuff: TKDT13DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT13DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT13DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT13DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT13DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT13DD_Axis - 1 do
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


function TKDT13DD.Search(const buff: TKDT13DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT13DD_Node;

var
  NearestNeighbour: PKDT13DD_Node;

  function FindParentNode(const buffPtr: PKDT13DD_Vec; NodePtr: PKDT13DD_Node): PKDT13DD_Node;
  var
    Next: PKDT13DD_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT13DD_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT13DD_Node; const buffPtr: PKDT13DD_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT13DDDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT13DD_Axis;
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

  function SortCompare(const buffPtr: PKDT13DD_Vec; const p1, p2: PKDT13DD_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT13DDDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT13DDDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT13DD_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT13DD_Node;
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
  Parent: PKDT13DD_Node;
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

  SearchedDistanceMin := KDT13DDDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT13DD_Node(NearestNodes[0]);
    end;
end;

function TKDT13DD.Search(const buff: TKDT13DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT13DD_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT13DD.Search(const buff: TKDT13DD_Vec; var SearchedDistanceMin: Double): PKDT13DD_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT13DD.Search(const buff: TKDT13DD_Vec): PKDT13DD_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT13DD.SearchToken(const buff: TKDT13DD_Vec): TPascalString;
var
  p: PKDT13DD_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT13DD.Search(const inBuff: TKDT13DD_DynamicVecBuffer; var OutBuff: TKDT13DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT13DD_DynamicVecBuffer;
  outBuffPtr: PKDT13DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT13DD_Node;
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
        p: PKDT13DD_Node;
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
  p: PKDT13DD_Node;
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


procedure TKDT13DD.Search(const inBuff: TKDT13DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT13DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT13DD_Node;
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
        p: PKDT13DD_Node;
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
  p: PKDT13DD_Node;
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


procedure TKDT13DD.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT13DD_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT13DD_Vec));
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

procedure TKDT13DD.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT13DD_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT13DD_Vec)) <> SizeOf(TKDT13DD_Vec) then
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

procedure TKDT13DD.SaveToFile(FileName: SystemString);
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

procedure TKDT13DD.LoadFromFile(FileName: SystemString);
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

procedure TKDT13DD.PrintNodeTree(const NodePtr: PKDT13DD_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT13DD_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT13DDVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT13DD.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT13DDVec(KDStoreBuff[i].buff)]);
end;

class function TKDT13DD.KDT13DDVec(const s: SystemString): TKDT13DD_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT13DD_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT13DD_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT13DD.KDT13DDVec(const v: TKDT13DD_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT13DD_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT13DD.KDT13DDPow(const v: TKDT13DD_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT13DD.KDT13DDDistance(const v1, v2: TKDT13DD_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT13DD_Axis - 1 do
      Result := Result + KDT13DDPow(v2[i] - v1[i]);
end;

procedure TKDT13DD.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT13DD_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT13DD.Test;
var
  TKDT13DD_Test: TKDT13DD;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT13DD_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT13DD_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT13DD_Test := TKDT13DD.Create;

  DoStatusNoLn('...');
  SetLength(TKDT13DD_Test.TestBuff, 1000);
  for i := 0 to length(TKDT13DD_Test.TestBuff) - 1 do
    for j := 0 to KDT13DD_Axis - 1 do
        TKDT13DD_Test.TestBuff[i][j] := i * KDT13DD_Axis + j;

{$IFDEF FPC}
  TKDT13DD_Test.BuildKDTreeM(length(TKDT13DD_Test.TestBuff), nil, @TKDT13DD_Test.Test_BuildM);
{$ELSE FPC}
  TKDT13DD_Test.BuildKDTreeM(length(TKDT13DD_Test.TestBuff), nil, TKDT13DD_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT13DD_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT13DD_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT13DD_Test.TestBuff) - 1 do
    begin
      p := TKDT13DD_Test.Search(TKDT13DD_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT13DD_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT13DD_Test.TestBuff));
      TKDT13DD_Test.Search(TKDT13DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT13DDDistance(TKDT13DD_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT13DD_Test.Clear;
      { kMean test }
      TKDT13DD_Test.BuildKDTreeWithCluster(TKDT13DD_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT13DD_Test.Search(TKDT13DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT13DD_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT13DD_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT13DD_Test);
end;


function TKDT14DD.InternalBuildKdTree(const KDSourceBufferPtr: PKDT14DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT14DD_Node;
  function SortCompare(const p1, p2: PKDT14DD_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT14DD_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT14DD_Source;
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
  kdBuffPtr: PKDT14DD_SourceBuffer;
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
      axis := Depth mod KDT14DD_Axis;
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

function TKDT14DD.GetData(const Index: NativeInt): PKDT14DD_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT14DD.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT14DD.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT14DD.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT14DD_Node(KDNodes[i]));
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

function TKDT14DD.StoreBuffPtr: PKDT14DD_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT14DD.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DD_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT14DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT14DD.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DD_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT14DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT14DD.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DD_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT14DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT14DD.BuildKDTreeWithCluster(const inBuff: TKDT14DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT14DD_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT14DD_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT14DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT14DD_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT14DD.BuildKDTreeWithCluster(const inBuff: TKDT14DD_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT14DD.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DD_BuildCall);
var
  TempStoreBuff: TKDT14DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT14DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT14DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT14DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT14DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT14DD_Axis - 1 do
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

procedure TKDT14DD.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DD_BuildMethod);
var
  TempStoreBuff: TKDT14DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT14DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT14DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT14DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT14DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT14DD_Axis - 1 do
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


procedure TKDT14DD.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DD_BuildProc);
var
  TempStoreBuff: TKDT14DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT14DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT14DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT14DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT14DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT14DD_Axis - 1 do
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


function TKDT14DD.Search(const buff: TKDT14DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT14DD_Node;

var
  NearestNeighbour: PKDT14DD_Node;

  function FindParentNode(const buffPtr: PKDT14DD_Vec; NodePtr: PKDT14DD_Node): PKDT14DD_Node;
  var
    Next: PKDT14DD_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT14DD_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT14DD_Node; const buffPtr: PKDT14DD_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT14DDDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT14DD_Axis;
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

  function SortCompare(const buffPtr: PKDT14DD_Vec; const p1, p2: PKDT14DD_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT14DDDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT14DDDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT14DD_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT14DD_Node;
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
  Parent: PKDT14DD_Node;
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

  SearchedDistanceMin := KDT14DDDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT14DD_Node(NearestNodes[0]);
    end;
end;

function TKDT14DD.Search(const buff: TKDT14DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT14DD_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT14DD.Search(const buff: TKDT14DD_Vec; var SearchedDistanceMin: Double): PKDT14DD_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT14DD.Search(const buff: TKDT14DD_Vec): PKDT14DD_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT14DD.SearchToken(const buff: TKDT14DD_Vec): TPascalString;
var
  p: PKDT14DD_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT14DD.Search(const inBuff: TKDT14DD_DynamicVecBuffer; var OutBuff: TKDT14DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT14DD_DynamicVecBuffer;
  outBuffPtr: PKDT14DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT14DD_Node;
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
        p: PKDT14DD_Node;
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
  p: PKDT14DD_Node;
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


procedure TKDT14DD.Search(const inBuff: TKDT14DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT14DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT14DD_Node;
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
        p: PKDT14DD_Node;
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
  p: PKDT14DD_Node;
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


procedure TKDT14DD.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT14DD_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT14DD_Vec));
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

procedure TKDT14DD.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT14DD_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT14DD_Vec)) <> SizeOf(TKDT14DD_Vec) then
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

procedure TKDT14DD.SaveToFile(FileName: SystemString);
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

procedure TKDT14DD.LoadFromFile(FileName: SystemString);
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

procedure TKDT14DD.PrintNodeTree(const NodePtr: PKDT14DD_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT14DD_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT14DDVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT14DD.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT14DDVec(KDStoreBuff[i].buff)]);
end;

class function TKDT14DD.KDT14DDVec(const s: SystemString): TKDT14DD_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT14DD_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT14DD_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT14DD.KDT14DDVec(const v: TKDT14DD_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT14DD_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT14DD.KDT14DDPow(const v: TKDT14DD_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT14DD.KDT14DDDistance(const v1, v2: TKDT14DD_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT14DD_Axis - 1 do
      Result := Result + KDT14DDPow(v2[i] - v1[i]);
end;

procedure TKDT14DD.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT14DD_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT14DD.Test;
var
  TKDT14DD_Test: TKDT14DD;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT14DD_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT14DD_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT14DD_Test := TKDT14DD.Create;

  DoStatusNoLn('...');
  SetLength(TKDT14DD_Test.TestBuff, 1000);
  for i := 0 to length(TKDT14DD_Test.TestBuff) - 1 do
    for j := 0 to KDT14DD_Axis - 1 do
        TKDT14DD_Test.TestBuff[i][j] := i * KDT14DD_Axis + j;

{$IFDEF FPC}
  TKDT14DD_Test.BuildKDTreeM(length(TKDT14DD_Test.TestBuff), nil, @TKDT14DD_Test.Test_BuildM);
{$ELSE FPC}
  TKDT14DD_Test.BuildKDTreeM(length(TKDT14DD_Test.TestBuff), nil, TKDT14DD_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT14DD_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT14DD_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT14DD_Test.TestBuff) - 1 do
    begin
      p := TKDT14DD_Test.Search(TKDT14DD_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT14DD_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT14DD_Test.TestBuff));
      TKDT14DD_Test.Search(TKDT14DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT14DDDistance(TKDT14DD_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT14DD_Test.Clear;
      { kMean test }
      TKDT14DD_Test.BuildKDTreeWithCluster(TKDT14DD_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT14DD_Test.Search(TKDT14DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT14DD_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT14DD_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT14DD_Test);
end;


function TKDT15DD.InternalBuildKdTree(const KDSourceBufferPtr: PKDT15DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT15DD_Node;
  function SortCompare(const p1, p2: PKDT15DD_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT15DD_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT15DD_Source;
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
  kdBuffPtr: PKDT15DD_SourceBuffer;
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
      axis := Depth mod KDT15DD_Axis;
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

function TKDT15DD.GetData(const Index: NativeInt): PKDT15DD_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT15DD.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT15DD.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT15DD.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT15DD_Node(KDNodes[i]));
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

function TKDT15DD.StoreBuffPtr: PKDT15DD_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT15DD.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DD_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT15DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT15DD.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DD_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT15DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT15DD.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DD_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT15DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT15DD.BuildKDTreeWithCluster(const inBuff: TKDT15DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT15DD_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT15DD_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT15DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT15DD_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT15DD.BuildKDTreeWithCluster(const inBuff: TKDT15DD_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT15DD.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DD_BuildCall);
var
  TempStoreBuff: TKDT15DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT15DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT15DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT15DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT15DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT15DD_Axis - 1 do
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

procedure TKDT15DD.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DD_BuildMethod);
var
  TempStoreBuff: TKDT15DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT15DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT15DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT15DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT15DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT15DD_Axis - 1 do
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


procedure TKDT15DD.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DD_BuildProc);
var
  TempStoreBuff: TKDT15DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT15DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT15DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT15DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT15DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT15DD_Axis - 1 do
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


function TKDT15DD.Search(const buff: TKDT15DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT15DD_Node;

var
  NearestNeighbour: PKDT15DD_Node;

  function FindParentNode(const buffPtr: PKDT15DD_Vec; NodePtr: PKDT15DD_Node): PKDT15DD_Node;
  var
    Next: PKDT15DD_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT15DD_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT15DD_Node; const buffPtr: PKDT15DD_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT15DDDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT15DD_Axis;
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

  function SortCompare(const buffPtr: PKDT15DD_Vec; const p1, p2: PKDT15DD_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT15DDDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT15DDDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT15DD_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT15DD_Node;
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
  Parent: PKDT15DD_Node;
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

  SearchedDistanceMin := KDT15DDDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT15DD_Node(NearestNodes[0]);
    end;
end;

function TKDT15DD.Search(const buff: TKDT15DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT15DD_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT15DD.Search(const buff: TKDT15DD_Vec; var SearchedDistanceMin: Double): PKDT15DD_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT15DD.Search(const buff: TKDT15DD_Vec): PKDT15DD_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT15DD.SearchToken(const buff: TKDT15DD_Vec): TPascalString;
var
  p: PKDT15DD_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT15DD.Search(const inBuff: TKDT15DD_DynamicVecBuffer; var OutBuff: TKDT15DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT15DD_DynamicVecBuffer;
  outBuffPtr: PKDT15DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT15DD_Node;
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
        p: PKDT15DD_Node;
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
  p: PKDT15DD_Node;
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


procedure TKDT15DD.Search(const inBuff: TKDT15DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT15DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT15DD_Node;
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
        p: PKDT15DD_Node;
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
  p: PKDT15DD_Node;
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


procedure TKDT15DD.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT15DD_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT15DD_Vec));
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

procedure TKDT15DD.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT15DD_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT15DD_Vec)) <> SizeOf(TKDT15DD_Vec) then
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

procedure TKDT15DD.SaveToFile(FileName: SystemString);
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

procedure TKDT15DD.LoadFromFile(FileName: SystemString);
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

procedure TKDT15DD.PrintNodeTree(const NodePtr: PKDT15DD_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT15DD_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT15DDVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT15DD.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT15DDVec(KDStoreBuff[i].buff)]);
end;

class function TKDT15DD.KDT15DDVec(const s: SystemString): TKDT15DD_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT15DD_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT15DD_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT15DD.KDT15DDVec(const v: TKDT15DD_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT15DD_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT15DD.KDT15DDPow(const v: TKDT15DD_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT15DD.KDT15DDDistance(const v1, v2: TKDT15DD_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT15DD_Axis - 1 do
      Result := Result + KDT15DDPow(v2[i] - v1[i]);
end;

procedure TKDT15DD.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT15DD_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT15DD.Test;
var
  TKDT15DD_Test: TKDT15DD;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT15DD_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT15DD_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT15DD_Test := TKDT15DD.Create;

  DoStatusNoLn('...');
  SetLength(TKDT15DD_Test.TestBuff, 1000);
  for i := 0 to length(TKDT15DD_Test.TestBuff) - 1 do
    for j := 0 to KDT15DD_Axis - 1 do
        TKDT15DD_Test.TestBuff[i][j] := i * KDT15DD_Axis + j;

{$IFDEF FPC}
  TKDT15DD_Test.BuildKDTreeM(length(TKDT15DD_Test.TestBuff), nil, @TKDT15DD_Test.Test_BuildM);
{$ELSE FPC}
  TKDT15DD_Test.BuildKDTreeM(length(TKDT15DD_Test.TestBuff), nil, TKDT15DD_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT15DD_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT15DD_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT15DD_Test.TestBuff) - 1 do
    begin
      p := TKDT15DD_Test.Search(TKDT15DD_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT15DD_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT15DD_Test.TestBuff));
      TKDT15DD_Test.Search(TKDT15DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT15DDDistance(TKDT15DD_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT15DD_Test.Clear;
      { kMean test }
      TKDT15DD_Test.BuildKDTreeWithCluster(TKDT15DD_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT15DD_Test.Search(TKDT15DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT15DD_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT15DD_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT15DD_Test);
end;


function TKDT16DD.InternalBuildKdTree(const KDSourceBufferPtr: PKDT16DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT16DD_Node;
  function SortCompare(const p1, p2: PKDT16DD_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT16DD_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT16DD_Source;
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
  kdBuffPtr: PKDT16DD_SourceBuffer;
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
      axis := Depth mod KDT16DD_Axis;
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

function TKDT16DD.GetData(const Index: NativeInt): PKDT16DD_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT16DD.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT16DD.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT16DD.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT16DD_Node(KDNodes[i]));
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

function TKDT16DD.StoreBuffPtr: PKDT16DD_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT16DD.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DD_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT16DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT16DD.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DD_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT16DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT16DD.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DD_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT16DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT16DD.BuildKDTreeWithCluster(const inBuff: TKDT16DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT16DD_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT16DD_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT16DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT16DD_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT16DD.BuildKDTreeWithCluster(const inBuff: TKDT16DD_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT16DD.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DD_BuildCall);
var
  TempStoreBuff: TKDT16DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT16DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT16DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT16DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT16DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT16DD_Axis - 1 do
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

procedure TKDT16DD.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DD_BuildMethod);
var
  TempStoreBuff: TKDT16DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT16DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT16DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT16DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT16DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT16DD_Axis - 1 do
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


procedure TKDT16DD.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DD_BuildProc);
var
  TempStoreBuff: TKDT16DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT16DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT16DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT16DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT16DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT16DD_Axis - 1 do
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


function TKDT16DD.Search(const buff: TKDT16DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT16DD_Node;

var
  NearestNeighbour: PKDT16DD_Node;

  function FindParentNode(const buffPtr: PKDT16DD_Vec; NodePtr: PKDT16DD_Node): PKDT16DD_Node;
  var
    Next: PKDT16DD_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT16DD_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT16DD_Node; const buffPtr: PKDT16DD_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT16DDDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT16DD_Axis;
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

  function SortCompare(const buffPtr: PKDT16DD_Vec; const p1, p2: PKDT16DD_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT16DDDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT16DDDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT16DD_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT16DD_Node;
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
  Parent: PKDT16DD_Node;
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

  SearchedDistanceMin := KDT16DDDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT16DD_Node(NearestNodes[0]);
    end;
end;

function TKDT16DD.Search(const buff: TKDT16DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT16DD_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT16DD.Search(const buff: TKDT16DD_Vec; var SearchedDistanceMin: Double): PKDT16DD_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT16DD.Search(const buff: TKDT16DD_Vec): PKDT16DD_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT16DD.SearchToken(const buff: TKDT16DD_Vec): TPascalString;
var
  p: PKDT16DD_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT16DD.Search(const inBuff: TKDT16DD_DynamicVecBuffer; var OutBuff: TKDT16DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT16DD_DynamicVecBuffer;
  outBuffPtr: PKDT16DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT16DD_Node;
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
        p: PKDT16DD_Node;
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
  p: PKDT16DD_Node;
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


procedure TKDT16DD.Search(const inBuff: TKDT16DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT16DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT16DD_Node;
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
        p: PKDT16DD_Node;
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
  p: PKDT16DD_Node;
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


procedure TKDT16DD.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT16DD_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT16DD_Vec));
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

procedure TKDT16DD.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT16DD_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT16DD_Vec)) <> SizeOf(TKDT16DD_Vec) then
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

procedure TKDT16DD.SaveToFile(FileName: SystemString);
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

procedure TKDT16DD.LoadFromFile(FileName: SystemString);
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

procedure TKDT16DD.PrintNodeTree(const NodePtr: PKDT16DD_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT16DD_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT16DDVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT16DD.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT16DDVec(KDStoreBuff[i].buff)]);
end;

class function TKDT16DD.KDT16DDVec(const s: SystemString): TKDT16DD_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT16DD_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT16DD_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT16DD.KDT16DDVec(const v: TKDT16DD_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT16DD_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT16DD.KDT16DDPow(const v: TKDT16DD_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT16DD.KDT16DDDistance(const v1, v2: TKDT16DD_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT16DD_Axis - 1 do
      Result := Result + KDT16DDPow(v2[i] - v1[i]);
end;

procedure TKDT16DD.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT16DD_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT16DD.Test;
var
  TKDT16DD_Test: TKDT16DD;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT16DD_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT16DD_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT16DD_Test := TKDT16DD.Create;

  DoStatusNoLn('...');
  SetLength(TKDT16DD_Test.TestBuff, 1000);
  for i := 0 to length(TKDT16DD_Test.TestBuff) - 1 do
    for j := 0 to KDT16DD_Axis - 1 do
        TKDT16DD_Test.TestBuff[i][j] := i * KDT16DD_Axis + j;

{$IFDEF FPC}
  TKDT16DD_Test.BuildKDTreeM(length(TKDT16DD_Test.TestBuff), nil, @TKDT16DD_Test.Test_BuildM);
{$ELSE FPC}
  TKDT16DD_Test.BuildKDTreeM(length(TKDT16DD_Test.TestBuff), nil, TKDT16DD_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT16DD_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT16DD_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT16DD_Test.TestBuff) - 1 do
    begin
      p := TKDT16DD_Test.Search(TKDT16DD_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT16DD_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT16DD_Test.TestBuff));
      TKDT16DD_Test.Search(TKDT16DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT16DDDistance(TKDT16DD_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT16DD_Test.Clear;
      { kMean test }
      TKDT16DD_Test.BuildKDTreeWithCluster(TKDT16DD_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT16DD_Test.Search(TKDT16DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT16DD_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT16DD_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT16DD_Test);
end;


function TKDT17DD.InternalBuildKdTree(const KDSourceBufferPtr: PKDT17DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT17DD_Node;
  function SortCompare(const p1, p2: PKDT17DD_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT17DD_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT17DD_Source;
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
  kdBuffPtr: PKDT17DD_SourceBuffer;
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
      axis := Depth mod KDT17DD_Axis;
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

function TKDT17DD.GetData(const Index: NativeInt): PKDT17DD_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT17DD.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT17DD.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT17DD.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT17DD_Node(KDNodes[i]));
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

function TKDT17DD.StoreBuffPtr: PKDT17DD_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT17DD.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DD_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT17DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT17DD.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DD_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT17DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT17DD.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DD_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT17DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT17DD.BuildKDTreeWithCluster(const inBuff: TKDT17DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT17DD_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT17DD_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT17DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT17DD_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT17DD.BuildKDTreeWithCluster(const inBuff: TKDT17DD_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT17DD.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DD_BuildCall);
var
  TempStoreBuff: TKDT17DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT17DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT17DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT17DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT17DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT17DD_Axis - 1 do
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

procedure TKDT17DD.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DD_BuildMethod);
var
  TempStoreBuff: TKDT17DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT17DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT17DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT17DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT17DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT17DD_Axis - 1 do
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


procedure TKDT17DD.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DD_BuildProc);
var
  TempStoreBuff: TKDT17DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT17DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT17DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT17DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT17DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT17DD_Axis - 1 do
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


function TKDT17DD.Search(const buff: TKDT17DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT17DD_Node;

var
  NearestNeighbour: PKDT17DD_Node;

  function FindParentNode(const buffPtr: PKDT17DD_Vec; NodePtr: PKDT17DD_Node): PKDT17DD_Node;
  var
    Next: PKDT17DD_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT17DD_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT17DD_Node; const buffPtr: PKDT17DD_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT17DDDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT17DD_Axis;
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

  function SortCompare(const buffPtr: PKDT17DD_Vec; const p1, p2: PKDT17DD_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT17DDDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT17DDDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT17DD_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT17DD_Node;
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
  Parent: PKDT17DD_Node;
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

  SearchedDistanceMin := KDT17DDDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT17DD_Node(NearestNodes[0]);
    end;
end;

function TKDT17DD.Search(const buff: TKDT17DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT17DD_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT17DD.Search(const buff: TKDT17DD_Vec; var SearchedDistanceMin: Double): PKDT17DD_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT17DD.Search(const buff: TKDT17DD_Vec): PKDT17DD_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT17DD.SearchToken(const buff: TKDT17DD_Vec): TPascalString;
var
  p: PKDT17DD_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT17DD.Search(const inBuff: TKDT17DD_DynamicVecBuffer; var OutBuff: TKDT17DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT17DD_DynamicVecBuffer;
  outBuffPtr: PKDT17DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT17DD_Node;
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
        p: PKDT17DD_Node;
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
  p: PKDT17DD_Node;
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


procedure TKDT17DD.Search(const inBuff: TKDT17DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT17DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT17DD_Node;
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
        p: PKDT17DD_Node;
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
  p: PKDT17DD_Node;
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


procedure TKDT17DD.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT17DD_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT17DD_Vec));
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

procedure TKDT17DD.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT17DD_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT17DD_Vec)) <> SizeOf(TKDT17DD_Vec) then
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

procedure TKDT17DD.SaveToFile(FileName: SystemString);
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

procedure TKDT17DD.LoadFromFile(FileName: SystemString);
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

procedure TKDT17DD.PrintNodeTree(const NodePtr: PKDT17DD_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT17DD_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT17DDVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT17DD.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT17DDVec(KDStoreBuff[i].buff)]);
end;

class function TKDT17DD.KDT17DDVec(const s: SystemString): TKDT17DD_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT17DD_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT17DD_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT17DD.KDT17DDVec(const v: TKDT17DD_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT17DD_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT17DD.KDT17DDPow(const v: TKDT17DD_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT17DD.KDT17DDDistance(const v1, v2: TKDT17DD_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT17DD_Axis - 1 do
      Result := Result + KDT17DDPow(v2[i] - v1[i]);
end;

procedure TKDT17DD.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT17DD_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT17DD.Test;
var
  TKDT17DD_Test: TKDT17DD;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT17DD_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT17DD_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT17DD_Test := TKDT17DD.Create;

  DoStatusNoLn('...');
  SetLength(TKDT17DD_Test.TestBuff, 1000);
  for i := 0 to length(TKDT17DD_Test.TestBuff) - 1 do
    for j := 0 to KDT17DD_Axis - 1 do
        TKDT17DD_Test.TestBuff[i][j] := i * KDT17DD_Axis + j;

{$IFDEF FPC}
  TKDT17DD_Test.BuildKDTreeM(length(TKDT17DD_Test.TestBuff), nil, @TKDT17DD_Test.Test_BuildM);
{$ELSE FPC}
  TKDT17DD_Test.BuildKDTreeM(length(TKDT17DD_Test.TestBuff), nil, TKDT17DD_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT17DD_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT17DD_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT17DD_Test.TestBuff) - 1 do
    begin
      p := TKDT17DD_Test.Search(TKDT17DD_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT17DD_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT17DD_Test.TestBuff));
      TKDT17DD_Test.Search(TKDT17DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT17DDDistance(TKDT17DD_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT17DD_Test.Clear;
      { kMean test }
      TKDT17DD_Test.BuildKDTreeWithCluster(TKDT17DD_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT17DD_Test.Search(TKDT17DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT17DD_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT17DD_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT17DD_Test);
end;


function TKDT18DD.InternalBuildKdTree(const KDSourceBufferPtr: PKDT18DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT18DD_Node;
  function SortCompare(const p1, p2: PKDT18DD_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT18DD_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT18DD_Source;
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
  kdBuffPtr: PKDT18DD_SourceBuffer;
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
      axis := Depth mod KDT18DD_Axis;
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

function TKDT18DD.GetData(const Index: NativeInt): PKDT18DD_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT18DD.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT18DD.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT18DD.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT18DD_Node(KDNodes[i]));
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

function TKDT18DD.StoreBuffPtr: PKDT18DD_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT18DD.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DD_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT18DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT18DD.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DD_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT18DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT18DD.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DD_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT18DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT18DD.BuildKDTreeWithCluster(const inBuff: TKDT18DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT18DD_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT18DD_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT18DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT18DD_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT18DD.BuildKDTreeWithCluster(const inBuff: TKDT18DD_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT18DD.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DD_BuildCall);
var
  TempStoreBuff: TKDT18DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT18DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT18DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT18DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT18DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT18DD_Axis - 1 do
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

procedure TKDT18DD.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DD_BuildMethod);
var
  TempStoreBuff: TKDT18DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT18DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT18DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT18DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT18DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT18DD_Axis - 1 do
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


procedure TKDT18DD.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DD_BuildProc);
var
  TempStoreBuff: TKDT18DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT18DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT18DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT18DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT18DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT18DD_Axis - 1 do
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


function TKDT18DD.Search(const buff: TKDT18DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT18DD_Node;

var
  NearestNeighbour: PKDT18DD_Node;

  function FindParentNode(const buffPtr: PKDT18DD_Vec; NodePtr: PKDT18DD_Node): PKDT18DD_Node;
  var
    Next: PKDT18DD_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT18DD_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT18DD_Node; const buffPtr: PKDT18DD_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT18DDDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT18DD_Axis;
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

  function SortCompare(const buffPtr: PKDT18DD_Vec; const p1, p2: PKDT18DD_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT18DDDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT18DDDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT18DD_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT18DD_Node;
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
  Parent: PKDT18DD_Node;
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

  SearchedDistanceMin := KDT18DDDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT18DD_Node(NearestNodes[0]);
    end;
end;

function TKDT18DD.Search(const buff: TKDT18DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT18DD_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT18DD.Search(const buff: TKDT18DD_Vec; var SearchedDistanceMin: Double): PKDT18DD_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT18DD.Search(const buff: TKDT18DD_Vec): PKDT18DD_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT18DD.SearchToken(const buff: TKDT18DD_Vec): TPascalString;
var
  p: PKDT18DD_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT18DD.Search(const inBuff: TKDT18DD_DynamicVecBuffer; var OutBuff: TKDT18DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT18DD_DynamicVecBuffer;
  outBuffPtr: PKDT18DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT18DD_Node;
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
        p: PKDT18DD_Node;
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
  p: PKDT18DD_Node;
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


procedure TKDT18DD.Search(const inBuff: TKDT18DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT18DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT18DD_Node;
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
        p: PKDT18DD_Node;
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
  p: PKDT18DD_Node;
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


procedure TKDT18DD.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT18DD_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT18DD_Vec));
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

procedure TKDT18DD.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT18DD_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT18DD_Vec)) <> SizeOf(TKDT18DD_Vec) then
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

procedure TKDT18DD.SaveToFile(FileName: SystemString);
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

procedure TKDT18DD.LoadFromFile(FileName: SystemString);
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

procedure TKDT18DD.PrintNodeTree(const NodePtr: PKDT18DD_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT18DD_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT18DDVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT18DD.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT18DDVec(KDStoreBuff[i].buff)]);
end;

class function TKDT18DD.KDT18DDVec(const s: SystemString): TKDT18DD_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT18DD_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT18DD_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT18DD.KDT18DDVec(const v: TKDT18DD_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT18DD_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT18DD.KDT18DDPow(const v: TKDT18DD_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT18DD.KDT18DDDistance(const v1, v2: TKDT18DD_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT18DD_Axis - 1 do
      Result := Result + KDT18DDPow(v2[i] - v1[i]);
end;

procedure TKDT18DD.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT18DD_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT18DD.Test;
var
  TKDT18DD_Test: TKDT18DD;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT18DD_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT18DD_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT18DD_Test := TKDT18DD.Create;

  DoStatusNoLn('...');
  SetLength(TKDT18DD_Test.TestBuff, 1000);
  for i := 0 to length(TKDT18DD_Test.TestBuff) - 1 do
    for j := 0 to KDT18DD_Axis - 1 do
        TKDT18DD_Test.TestBuff[i][j] := i * KDT18DD_Axis + j;

{$IFDEF FPC}
  TKDT18DD_Test.BuildKDTreeM(length(TKDT18DD_Test.TestBuff), nil, @TKDT18DD_Test.Test_BuildM);
{$ELSE FPC}
  TKDT18DD_Test.BuildKDTreeM(length(TKDT18DD_Test.TestBuff), nil, TKDT18DD_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT18DD_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT18DD_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT18DD_Test.TestBuff) - 1 do
    begin
      p := TKDT18DD_Test.Search(TKDT18DD_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT18DD_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT18DD_Test.TestBuff));
      TKDT18DD_Test.Search(TKDT18DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT18DDDistance(TKDT18DD_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT18DD_Test.Clear;
      { kMean test }
      TKDT18DD_Test.BuildKDTreeWithCluster(TKDT18DD_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT18DD_Test.Search(TKDT18DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT18DD_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT18DD_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT18DD_Test);
end;


function TKDT19DD.InternalBuildKdTree(const KDSourceBufferPtr: PKDT19DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT19DD_Node;
  function SortCompare(const p1, p2: PKDT19DD_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT19DD_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT19DD_Source;
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
  kdBuffPtr: PKDT19DD_SourceBuffer;
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
      axis := Depth mod KDT19DD_Axis;
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

function TKDT19DD.GetData(const Index: NativeInt): PKDT19DD_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT19DD.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT19DD.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT19DD.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT19DD_Node(KDNodes[i]));
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

function TKDT19DD.StoreBuffPtr: PKDT19DD_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT19DD.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DD_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT19DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT19DD.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DD_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT19DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT19DD.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DD_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT19DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT19DD.BuildKDTreeWithCluster(const inBuff: TKDT19DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT19DD_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT19DD_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT19DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT19DD_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT19DD.BuildKDTreeWithCluster(const inBuff: TKDT19DD_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT19DD.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DD_BuildCall);
var
  TempStoreBuff: TKDT19DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT19DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT19DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT19DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT19DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT19DD_Axis - 1 do
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

procedure TKDT19DD.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DD_BuildMethod);
var
  TempStoreBuff: TKDT19DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT19DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT19DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT19DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT19DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT19DD_Axis - 1 do
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


procedure TKDT19DD.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DD_BuildProc);
var
  TempStoreBuff: TKDT19DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT19DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT19DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT19DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT19DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT19DD_Axis - 1 do
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


function TKDT19DD.Search(const buff: TKDT19DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT19DD_Node;

var
  NearestNeighbour: PKDT19DD_Node;

  function FindParentNode(const buffPtr: PKDT19DD_Vec; NodePtr: PKDT19DD_Node): PKDT19DD_Node;
  var
    Next: PKDT19DD_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT19DD_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT19DD_Node; const buffPtr: PKDT19DD_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT19DDDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT19DD_Axis;
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

  function SortCompare(const buffPtr: PKDT19DD_Vec; const p1, p2: PKDT19DD_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT19DDDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT19DDDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT19DD_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT19DD_Node;
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
  Parent: PKDT19DD_Node;
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

  SearchedDistanceMin := KDT19DDDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT19DD_Node(NearestNodes[0]);
    end;
end;

function TKDT19DD.Search(const buff: TKDT19DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT19DD_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT19DD.Search(const buff: TKDT19DD_Vec; var SearchedDistanceMin: Double): PKDT19DD_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT19DD.Search(const buff: TKDT19DD_Vec): PKDT19DD_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT19DD.SearchToken(const buff: TKDT19DD_Vec): TPascalString;
var
  p: PKDT19DD_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT19DD.Search(const inBuff: TKDT19DD_DynamicVecBuffer; var OutBuff: TKDT19DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT19DD_DynamicVecBuffer;
  outBuffPtr: PKDT19DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT19DD_Node;
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
        p: PKDT19DD_Node;
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
  p: PKDT19DD_Node;
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


procedure TKDT19DD.Search(const inBuff: TKDT19DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT19DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT19DD_Node;
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
        p: PKDT19DD_Node;
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
  p: PKDT19DD_Node;
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


procedure TKDT19DD.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT19DD_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT19DD_Vec));
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

procedure TKDT19DD.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT19DD_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT19DD_Vec)) <> SizeOf(TKDT19DD_Vec) then
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

procedure TKDT19DD.SaveToFile(FileName: SystemString);
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

procedure TKDT19DD.LoadFromFile(FileName: SystemString);
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

procedure TKDT19DD.PrintNodeTree(const NodePtr: PKDT19DD_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT19DD_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT19DDVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT19DD.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT19DDVec(KDStoreBuff[i].buff)]);
end;

class function TKDT19DD.KDT19DDVec(const s: SystemString): TKDT19DD_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT19DD_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT19DD_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT19DD.KDT19DDVec(const v: TKDT19DD_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT19DD_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT19DD.KDT19DDPow(const v: TKDT19DD_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT19DD.KDT19DDDistance(const v1, v2: TKDT19DD_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT19DD_Axis - 1 do
      Result := Result + KDT19DDPow(v2[i] - v1[i]);
end;

procedure TKDT19DD.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT19DD_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT19DD.Test;
var
  TKDT19DD_Test: TKDT19DD;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT19DD_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT19DD_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT19DD_Test := TKDT19DD.Create;

  DoStatusNoLn('...');
  SetLength(TKDT19DD_Test.TestBuff, 1000);
  for i := 0 to length(TKDT19DD_Test.TestBuff) - 1 do
    for j := 0 to KDT19DD_Axis - 1 do
        TKDT19DD_Test.TestBuff[i][j] := i * KDT19DD_Axis + j;

{$IFDEF FPC}
  TKDT19DD_Test.BuildKDTreeM(length(TKDT19DD_Test.TestBuff), nil, @TKDT19DD_Test.Test_BuildM);
{$ELSE FPC}
  TKDT19DD_Test.BuildKDTreeM(length(TKDT19DD_Test.TestBuff), nil, TKDT19DD_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT19DD_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT19DD_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT19DD_Test.TestBuff) - 1 do
    begin
      p := TKDT19DD_Test.Search(TKDT19DD_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT19DD_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT19DD_Test.TestBuff));
      TKDT19DD_Test.Search(TKDT19DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT19DDDistance(TKDT19DD_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT19DD_Test.Clear;
      { kMean test }
      TKDT19DD_Test.BuildKDTreeWithCluster(TKDT19DD_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT19DD_Test.Search(TKDT19DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT19DD_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT19DD_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT19DD_Test);
end;


function TKDT20DD.InternalBuildKdTree(const KDSourceBufferPtr: PKDT20DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT20DD_Node;
  function SortCompare(const p1, p2: PKDT20DD_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT20DD_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT20DD_Source;
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
  kdBuffPtr: PKDT20DD_SourceBuffer;
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
      axis := Depth mod KDT20DD_Axis;
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

function TKDT20DD.GetData(const Index: NativeInt): PKDT20DD_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT20DD.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT20DD.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT20DD.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT20DD_Node(KDNodes[i]));
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

function TKDT20DD.StoreBuffPtr: PKDT20DD_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT20DD.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DD_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT20DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT20DD.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DD_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT20DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT20DD.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DD_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT20DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT20DD.BuildKDTreeWithCluster(const inBuff: TKDT20DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT20DD_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT20DD_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT20DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT20DD_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT20DD.BuildKDTreeWithCluster(const inBuff: TKDT20DD_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT20DD.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DD_BuildCall);
var
  TempStoreBuff: TKDT20DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT20DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT20DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT20DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT20DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT20DD_Axis - 1 do
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

procedure TKDT20DD.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DD_BuildMethod);
var
  TempStoreBuff: TKDT20DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT20DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT20DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT20DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT20DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT20DD_Axis - 1 do
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


procedure TKDT20DD.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DD_BuildProc);
var
  TempStoreBuff: TKDT20DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT20DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT20DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT20DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT20DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT20DD_Axis - 1 do
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


function TKDT20DD.Search(const buff: TKDT20DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT20DD_Node;

var
  NearestNeighbour: PKDT20DD_Node;

  function FindParentNode(const buffPtr: PKDT20DD_Vec; NodePtr: PKDT20DD_Node): PKDT20DD_Node;
  var
    Next: PKDT20DD_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT20DD_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT20DD_Node; const buffPtr: PKDT20DD_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT20DDDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT20DD_Axis;
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

  function SortCompare(const buffPtr: PKDT20DD_Vec; const p1, p2: PKDT20DD_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT20DDDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT20DDDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT20DD_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT20DD_Node;
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
  Parent: PKDT20DD_Node;
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

  SearchedDistanceMin := KDT20DDDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT20DD_Node(NearestNodes[0]);
    end;
end;

function TKDT20DD.Search(const buff: TKDT20DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT20DD_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT20DD.Search(const buff: TKDT20DD_Vec; var SearchedDistanceMin: Double): PKDT20DD_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT20DD.Search(const buff: TKDT20DD_Vec): PKDT20DD_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT20DD.SearchToken(const buff: TKDT20DD_Vec): TPascalString;
var
  p: PKDT20DD_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT20DD.Search(const inBuff: TKDT20DD_DynamicVecBuffer; var OutBuff: TKDT20DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT20DD_DynamicVecBuffer;
  outBuffPtr: PKDT20DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT20DD_Node;
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
        p: PKDT20DD_Node;
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
  p: PKDT20DD_Node;
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


procedure TKDT20DD.Search(const inBuff: TKDT20DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT20DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT20DD_Node;
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
        p: PKDT20DD_Node;
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
  p: PKDT20DD_Node;
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


procedure TKDT20DD.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT20DD_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT20DD_Vec));
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

procedure TKDT20DD.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT20DD_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT20DD_Vec)) <> SizeOf(TKDT20DD_Vec) then
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

procedure TKDT20DD.SaveToFile(FileName: SystemString);
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

procedure TKDT20DD.LoadFromFile(FileName: SystemString);
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

procedure TKDT20DD.PrintNodeTree(const NodePtr: PKDT20DD_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT20DD_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT20DDVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT20DD.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT20DDVec(KDStoreBuff[i].buff)]);
end;

class function TKDT20DD.KDT20DDVec(const s: SystemString): TKDT20DD_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT20DD_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT20DD_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT20DD.KDT20DDVec(const v: TKDT20DD_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT20DD_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT20DD.KDT20DDPow(const v: TKDT20DD_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT20DD.KDT20DDDistance(const v1, v2: TKDT20DD_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT20DD_Axis - 1 do
      Result := Result + KDT20DDPow(v2[i] - v1[i]);
end;

procedure TKDT20DD.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT20DD_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT20DD.Test;
var
  TKDT20DD_Test: TKDT20DD;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT20DD_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT20DD_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT20DD_Test := TKDT20DD.Create;

  DoStatusNoLn('...');
  SetLength(TKDT20DD_Test.TestBuff, 1000);
  for i := 0 to length(TKDT20DD_Test.TestBuff) - 1 do
    for j := 0 to KDT20DD_Axis - 1 do
        TKDT20DD_Test.TestBuff[i][j] := i * KDT20DD_Axis + j;

{$IFDEF FPC}
  TKDT20DD_Test.BuildKDTreeM(length(TKDT20DD_Test.TestBuff), nil, @TKDT20DD_Test.Test_BuildM);
{$ELSE FPC}
  TKDT20DD_Test.BuildKDTreeM(length(TKDT20DD_Test.TestBuff), nil, TKDT20DD_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT20DD_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT20DD_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT20DD_Test.TestBuff) - 1 do
    begin
      p := TKDT20DD_Test.Search(TKDT20DD_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT20DD_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT20DD_Test.TestBuff));
      TKDT20DD_Test.Search(TKDT20DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT20DDDistance(TKDT20DD_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT20DD_Test.Clear;
      { kMean test }
      TKDT20DD_Test.BuildKDTreeWithCluster(TKDT20DD_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT20DD_Test.Search(TKDT20DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT20DD_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT20DD_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT20DD_Test);
end;


function TKDT21DD.InternalBuildKdTree(const KDSourceBufferPtr: PKDT21DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT21DD_Node;
  function SortCompare(const p1, p2: PKDT21DD_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT21DD_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT21DD_Source;
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
  kdBuffPtr: PKDT21DD_SourceBuffer;
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
      axis := Depth mod KDT21DD_Axis;
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

function TKDT21DD.GetData(const Index: NativeInt): PKDT21DD_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT21DD.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT21DD.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT21DD.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT21DD_Node(KDNodes[i]));
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

function TKDT21DD.StoreBuffPtr: PKDT21DD_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT21DD.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DD_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT21DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT21DD.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DD_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT21DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT21DD.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DD_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT21DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT21DD.BuildKDTreeWithCluster(const inBuff: TKDT21DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT21DD_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT21DD_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT21DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT21DD_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT21DD.BuildKDTreeWithCluster(const inBuff: TKDT21DD_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT21DD.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DD_BuildCall);
var
  TempStoreBuff: TKDT21DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT21DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT21DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT21DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT21DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT21DD_Axis - 1 do
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

procedure TKDT21DD.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DD_BuildMethod);
var
  TempStoreBuff: TKDT21DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT21DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT21DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT21DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT21DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT21DD_Axis - 1 do
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


procedure TKDT21DD.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DD_BuildProc);
var
  TempStoreBuff: TKDT21DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT21DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT21DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT21DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT21DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT21DD_Axis - 1 do
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


function TKDT21DD.Search(const buff: TKDT21DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT21DD_Node;

var
  NearestNeighbour: PKDT21DD_Node;

  function FindParentNode(const buffPtr: PKDT21DD_Vec; NodePtr: PKDT21DD_Node): PKDT21DD_Node;
  var
    Next: PKDT21DD_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT21DD_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT21DD_Node; const buffPtr: PKDT21DD_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT21DDDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT21DD_Axis;
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

  function SortCompare(const buffPtr: PKDT21DD_Vec; const p1, p2: PKDT21DD_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT21DDDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT21DDDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT21DD_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT21DD_Node;
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
  Parent: PKDT21DD_Node;
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

  SearchedDistanceMin := KDT21DDDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT21DD_Node(NearestNodes[0]);
    end;
end;

function TKDT21DD.Search(const buff: TKDT21DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT21DD_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT21DD.Search(const buff: TKDT21DD_Vec; var SearchedDistanceMin: Double): PKDT21DD_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT21DD.Search(const buff: TKDT21DD_Vec): PKDT21DD_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT21DD.SearchToken(const buff: TKDT21DD_Vec): TPascalString;
var
  p: PKDT21DD_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT21DD.Search(const inBuff: TKDT21DD_DynamicVecBuffer; var OutBuff: TKDT21DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT21DD_DynamicVecBuffer;
  outBuffPtr: PKDT21DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT21DD_Node;
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
        p: PKDT21DD_Node;
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
  p: PKDT21DD_Node;
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


procedure TKDT21DD.Search(const inBuff: TKDT21DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT21DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT21DD_Node;
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
        p: PKDT21DD_Node;
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
  p: PKDT21DD_Node;
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


procedure TKDT21DD.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT21DD_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT21DD_Vec));
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

procedure TKDT21DD.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT21DD_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT21DD_Vec)) <> SizeOf(TKDT21DD_Vec) then
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

procedure TKDT21DD.SaveToFile(FileName: SystemString);
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

procedure TKDT21DD.LoadFromFile(FileName: SystemString);
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

procedure TKDT21DD.PrintNodeTree(const NodePtr: PKDT21DD_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT21DD_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT21DDVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT21DD.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT21DDVec(KDStoreBuff[i].buff)]);
end;

class function TKDT21DD.KDT21DDVec(const s: SystemString): TKDT21DD_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT21DD_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT21DD_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT21DD.KDT21DDVec(const v: TKDT21DD_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT21DD_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT21DD.KDT21DDPow(const v: TKDT21DD_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT21DD.KDT21DDDistance(const v1, v2: TKDT21DD_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT21DD_Axis - 1 do
      Result := Result + KDT21DDPow(v2[i] - v1[i]);
end;

procedure TKDT21DD.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT21DD_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT21DD.Test;
var
  TKDT21DD_Test: TKDT21DD;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT21DD_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT21DD_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT21DD_Test := TKDT21DD.Create;

  DoStatusNoLn('...');
  SetLength(TKDT21DD_Test.TestBuff, 1000);
  for i := 0 to length(TKDT21DD_Test.TestBuff) - 1 do
    for j := 0 to KDT21DD_Axis - 1 do
        TKDT21DD_Test.TestBuff[i][j] := i * KDT21DD_Axis + j;

{$IFDEF FPC}
  TKDT21DD_Test.BuildKDTreeM(length(TKDT21DD_Test.TestBuff), nil, @TKDT21DD_Test.Test_BuildM);
{$ELSE FPC}
  TKDT21DD_Test.BuildKDTreeM(length(TKDT21DD_Test.TestBuff), nil, TKDT21DD_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT21DD_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT21DD_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT21DD_Test.TestBuff) - 1 do
    begin
      p := TKDT21DD_Test.Search(TKDT21DD_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT21DD_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT21DD_Test.TestBuff));
      TKDT21DD_Test.Search(TKDT21DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT21DDDistance(TKDT21DD_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT21DD_Test.Clear;
      { kMean test }
      TKDT21DD_Test.BuildKDTreeWithCluster(TKDT21DD_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT21DD_Test.Search(TKDT21DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT21DD_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT21DD_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT21DD_Test);
end;


function TKDT22DD.InternalBuildKdTree(const KDSourceBufferPtr: PKDT22DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT22DD_Node;
  function SortCompare(const p1, p2: PKDT22DD_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT22DD_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT22DD_Source;
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
  kdBuffPtr: PKDT22DD_SourceBuffer;
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
      axis := Depth mod KDT22DD_Axis;
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

function TKDT22DD.GetData(const Index: NativeInt): PKDT22DD_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT22DD.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT22DD.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT22DD.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT22DD_Node(KDNodes[i]));
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

function TKDT22DD.StoreBuffPtr: PKDT22DD_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT22DD.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DD_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT22DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT22DD.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DD_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT22DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT22DD.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DD_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT22DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT22DD.BuildKDTreeWithCluster(const inBuff: TKDT22DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT22DD_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT22DD_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT22DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT22DD_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT22DD.BuildKDTreeWithCluster(const inBuff: TKDT22DD_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT22DD.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DD_BuildCall);
var
  TempStoreBuff: TKDT22DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT22DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT22DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT22DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT22DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT22DD_Axis - 1 do
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

procedure TKDT22DD.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DD_BuildMethod);
var
  TempStoreBuff: TKDT22DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT22DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT22DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT22DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT22DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT22DD_Axis - 1 do
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


procedure TKDT22DD.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DD_BuildProc);
var
  TempStoreBuff: TKDT22DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT22DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT22DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT22DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT22DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT22DD_Axis - 1 do
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


function TKDT22DD.Search(const buff: TKDT22DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT22DD_Node;

var
  NearestNeighbour: PKDT22DD_Node;

  function FindParentNode(const buffPtr: PKDT22DD_Vec; NodePtr: PKDT22DD_Node): PKDT22DD_Node;
  var
    Next: PKDT22DD_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT22DD_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT22DD_Node; const buffPtr: PKDT22DD_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT22DDDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT22DD_Axis;
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

  function SortCompare(const buffPtr: PKDT22DD_Vec; const p1, p2: PKDT22DD_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT22DDDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT22DDDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT22DD_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT22DD_Node;
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
  Parent: PKDT22DD_Node;
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

  SearchedDistanceMin := KDT22DDDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT22DD_Node(NearestNodes[0]);
    end;
end;

function TKDT22DD.Search(const buff: TKDT22DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT22DD_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT22DD.Search(const buff: TKDT22DD_Vec; var SearchedDistanceMin: Double): PKDT22DD_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT22DD.Search(const buff: TKDT22DD_Vec): PKDT22DD_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT22DD.SearchToken(const buff: TKDT22DD_Vec): TPascalString;
var
  p: PKDT22DD_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT22DD.Search(const inBuff: TKDT22DD_DynamicVecBuffer; var OutBuff: TKDT22DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT22DD_DynamicVecBuffer;
  outBuffPtr: PKDT22DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT22DD_Node;
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
        p: PKDT22DD_Node;
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
  p: PKDT22DD_Node;
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


procedure TKDT22DD.Search(const inBuff: TKDT22DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT22DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT22DD_Node;
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
        p: PKDT22DD_Node;
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
  p: PKDT22DD_Node;
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


procedure TKDT22DD.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT22DD_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT22DD_Vec));
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

procedure TKDT22DD.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT22DD_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT22DD_Vec)) <> SizeOf(TKDT22DD_Vec) then
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

procedure TKDT22DD.SaveToFile(FileName: SystemString);
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

procedure TKDT22DD.LoadFromFile(FileName: SystemString);
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

procedure TKDT22DD.PrintNodeTree(const NodePtr: PKDT22DD_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT22DD_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT22DDVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT22DD.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT22DDVec(KDStoreBuff[i].buff)]);
end;

class function TKDT22DD.KDT22DDVec(const s: SystemString): TKDT22DD_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT22DD_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT22DD_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT22DD.KDT22DDVec(const v: TKDT22DD_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT22DD_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT22DD.KDT22DDPow(const v: TKDT22DD_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT22DD.KDT22DDDistance(const v1, v2: TKDT22DD_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT22DD_Axis - 1 do
      Result := Result + KDT22DDPow(v2[i] - v1[i]);
end;

procedure TKDT22DD.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT22DD_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT22DD.Test;
var
  TKDT22DD_Test: TKDT22DD;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT22DD_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT22DD_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT22DD_Test := TKDT22DD.Create;

  DoStatusNoLn('...');
  SetLength(TKDT22DD_Test.TestBuff, 1000);
  for i := 0 to length(TKDT22DD_Test.TestBuff) - 1 do
    for j := 0 to KDT22DD_Axis - 1 do
        TKDT22DD_Test.TestBuff[i][j] := i * KDT22DD_Axis + j;

{$IFDEF FPC}
  TKDT22DD_Test.BuildKDTreeM(length(TKDT22DD_Test.TestBuff), nil, @TKDT22DD_Test.Test_BuildM);
{$ELSE FPC}
  TKDT22DD_Test.BuildKDTreeM(length(TKDT22DD_Test.TestBuff), nil, TKDT22DD_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT22DD_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT22DD_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT22DD_Test.TestBuff) - 1 do
    begin
      p := TKDT22DD_Test.Search(TKDT22DD_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT22DD_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT22DD_Test.TestBuff));
      TKDT22DD_Test.Search(TKDT22DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT22DDDistance(TKDT22DD_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT22DD_Test.Clear;
      { kMean test }
      TKDT22DD_Test.BuildKDTreeWithCluster(TKDT22DD_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT22DD_Test.Search(TKDT22DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT22DD_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT22DD_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT22DD_Test);
end;


function TKDT23DD.InternalBuildKdTree(const KDSourceBufferPtr: PKDT23DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT23DD_Node;
  function SortCompare(const p1, p2: PKDT23DD_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT23DD_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT23DD_Source;
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
  kdBuffPtr: PKDT23DD_SourceBuffer;
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
      axis := Depth mod KDT23DD_Axis;
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

function TKDT23DD.GetData(const Index: NativeInt): PKDT23DD_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT23DD.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT23DD.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT23DD.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT23DD_Node(KDNodes[i]));
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

function TKDT23DD.StoreBuffPtr: PKDT23DD_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT23DD.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DD_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT23DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT23DD.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DD_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT23DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT23DD.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DD_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT23DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT23DD.BuildKDTreeWithCluster(const inBuff: TKDT23DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT23DD_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT23DD_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT23DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT23DD_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT23DD.BuildKDTreeWithCluster(const inBuff: TKDT23DD_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT23DD.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DD_BuildCall);
var
  TempStoreBuff: TKDT23DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT23DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT23DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT23DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT23DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT23DD_Axis - 1 do
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

procedure TKDT23DD.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DD_BuildMethod);
var
  TempStoreBuff: TKDT23DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT23DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT23DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT23DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT23DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT23DD_Axis - 1 do
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


procedure TKDT23DD.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DD_BuildProc);
var
  TempStoreBuff: TKDT23DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT23DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT23DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT23DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT23DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT23DD_Axis - 1 do
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


function TKDT23DD.Search(const buff: TKDT23DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT23DD_Node;

var
  NearestNeighbour: PKDT23DD_Node;

  function FindParentNode(const buffPtr: PKDT23DD_Vec; NodePtr: PKDT23DD_Node): PKDT23DD_Node;
  var
    Next: PKDT23DD_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT23DD_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT23DD_Node; const buffPtr: PKDT23DD_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT23DDDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT23DD_Axis;
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

  function SortCompare(const buffPtr: PKDT23DD_Vec; const p1, p2: PKDT23DD_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT23DDDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT23DDDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT23DD_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT23DD_Node;
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
  Parent: PKDT23DD_Node;
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

  SearchedDistanceMin := KDT23DDDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT23DD_Node(NearestNodes[0]);
    end;
end;

function TKDT23DD.Search(const buff: TKDT23DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT23DD_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT23DD.Search(const buff: TKDT23DD_Vec; var SearchedDistanceMin: Double): PKDT23DD_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT23DD.Search(const buff: TKDT23DD_Vec): PKDT23DD_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT23DD.SearchToken(const buff: TKDT23DD_Vec): TPascalString;
var
  p: PKDT23DD_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT23DD.Search(const inBuff: TKDT23DD_DynamicVecBuffer; var OutBuff: TKDT23DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT23DD_DynamicVecBuffer;
  outBuffPtr: PKDT23DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT23DD_Node;
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
        p: PKDT23DD_Node;
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
  p: PKDT23DD_Node;
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


procedure TKDT23DD.Search(const inBuff: TKDT23DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT23DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT23DD_Node;
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
        p: PKDT23DD_Node;
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
  p: PKDT23DD_Node;
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


procedure TKDT23DD.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT23DD_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT23DD_Vec));
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

procedure TKDT23DD.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT23DD_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT23DD_Vec)) <> SizeOf(TKDT23DD_Vec) then
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

procedure TKDT23DD.SaveToFile(FileName: SystemString);
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

procedure TKDT23DD.LoadFromFile(FileName: SystemString);
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

procedure TKDT23DD.PrintNodeTree(const NodePtr: PKDT23DD_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT23DD_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT23DDVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT23DD.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT23DDVec(KDStoreBuff[i].buff)]);
end;

class function TKDT23DD.KDT23DDVec(const s: SystemString): TKDT23DD_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT23DD_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT23DD_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT23DD.KDT23DDVec(const v: TKDT23DD_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT23DD_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT23DD.KDT23DDPow(const v: TKDT23DD_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT23DD.KDT23DDDistance(const v1, v2: TKDT23DD_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT23DD_Axis - 1 do
      Result := Result + KDT23DDPow(v2[i] - v1[i]);
end;

procedure TKDT23DD.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT23DD_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT23DD.Test;
var
  TKDT23DD_Test: TKDT23DD;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT23DD_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT23DD_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT23DD_Test := TKDT23DD.Create;

  DoStatusNoLn('...');
  SetLength(TKDT23DD_Test.TestBuff, 1000);
  for i := 0 to length(TKDT23DD_Test.TestBuff) - 1 do
    for j := 0 to KDT23DD_Axis - 1 do
        TKDT23DD_Test.TestBuff[i][j] := i * KDT23DD_Axis + j;

{$IFDEF FPC}
  TKDT23DD_Test.BuildKDTreeM(length(TKDT23DD_Test.TestBuff), nil, @TKDT23DD_Test.Test_BuildM);
{$ELSE FPC}
  TKDT23DD_Test.BuildKDTreeM(length(TKDT23DD_Test.TestBuff), nil, TKDT23DD_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT23DD_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT23DD_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT23DD_Test.TestBuff) - 1 do
    begin
      p := TKDT23DD_Test.Search(TKDT23DD_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT23DD_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT23DD_Test.TestBuff));
      TKDT23DD_Test.Search(TKDT23DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT23DDDistance(TKDT23DD_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT23DD_Test.Clear;
      { kMean test }
      TKDT23DD_Test.BuildKDTreeWithCluster(TKDT23DD_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT23DD_Test.Search(TKDT23DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT23DD_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT23DD_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT23DD_Test);
end;


function TKDT24DD.InternalBuildKdTree(const KDSourceBufferPtr: PKDT24DD_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT24DD_Node;
  function SortCompare(const p1, p2: PKDT24DD_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT24DD_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT24DD_Source;
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
  kdBuffPtr: PKDT24DD_SourceBuffer;
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
      axis := Depth mod KDT24DD_Axis;
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

function TKDT24DD.GetData(const Index: NativeInt): PKDT24DD_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT24DD.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT24DD.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT24DD.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT24DD_Node(KDNodes[i]));
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

function TKDT24DD.StoreBuffPtr: PKDT24DD_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT24DD.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DD_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT24DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT24DD.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DD_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT24DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT24DD.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DD_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT24DD_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT24DD.BuildKDTreeWithCluster(const inBuff: TKDT24DD_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT24DD_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT24DD_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT24DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT24DD_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT24DD.BuildKDTreeWithCluster(const inBuff: TKDT24DD_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT24DD.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DD_BuildCall);
var
  TempStoreBuff: TKDT24DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT24DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT24DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT24DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT24DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT24DD_Axis - 1 do
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

procedure TKDT24DD.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DD_BuildMethod);
var
  TempStoreBuff: TKDT24DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT24DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT24DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT24DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT24DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT24DD_Axis - 1 do
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


procedure TKDT24DD.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DD_BuildProc);
var
  TempStoreBuff: TKDT24DD_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT24DD_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT24DD_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT24DD_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT24DD_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT24DD_Axis - 1 do
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


function TKDT24DD.Search(const buff: TKDT24DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT24DD_Node;

var
  NearestNeighbour: PKDT24DD_Node;

  function FindParentNode(const buffPtr: PKDT24DD_Vec; NodePtr: PKDT24DD_Node): PKDT24DD_Node;
  var
    Next: PKDT24DD_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT24DD_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT24DD_Node; const buffPtr: PKDT24DD_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT24DDDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT24DD_Axis;
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

  function SortCompare(const buffPtr: PKDT24DD_Vec; const p1, p2: PKDT24DD_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT24DDDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT24DDDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT24DD_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT24DD_Node;
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
  Parent: PKDT24DD_Node;
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

  SearchedDistanceMin := KDT24DDDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT24DD_Node(NearestNodes[0]);
    end;
end;

function TKDT24DD.Search(const buff: TKDT24DD_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT24DD_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT24DD.Search(const buff: TKDT24DD_Vec; var SearchedDistanceMin: Double): PKDT24DD_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT24DD.Search(const buff: TKDT24DD_Vec): PKDT24DD_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT24DD.SearchToken(const buff: TKDT24DD_Vec): TPascalString;
var
  p: PKDT24DD_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT24DD.Search(const inBuff: TKDT24DD_DynamicVecBuffer; var OutBuff: TKDT24DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT24DD_DynamicVecBuffer;
  outBuffPtr: PKDT24DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT24DD_Node;
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
        p: PKDT24DD_Node;
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
  p: PKDT24DD_Node;
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


procedure TKDT24DD.Search(const inBuff: TKDT24DD_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT24DD_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT24DD_Node;
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
        p: PKDT24DD_Node;
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
  p: PKDT24DD_Node;
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


procedure TKDT24DD.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT24DD_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT24DD_Vec));
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

procedure TKDT24DD.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT24DD_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT24DD_Vec)) <> SizeOf(TKDT24DD_Vec) then
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

procedure TKDT24DD.SaveToFile(FileName: SystemString);
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

procedure TKDT24DD.LoadFromFile(FileName: SystemString);
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

procedure TKDT24DD.PrintNodeTree(const NodePtr: PKDT24DD_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT24DD_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT24DDVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT24DD.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT24DDVec(KDStoreBuff[i].buff)]);
end;

class function TKDT24DD.KDT24DDVec(const s: SystemString): TKDT24DD_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT24DD_Axis - 1 do
      Result[i] := 0;

  t := TTextParsing.Create(s, tsText, nil);
  if t.SplitChar(1, ', ', '', SplitOutput) > 0 then
    begin
      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            Inc(j);
            if j >= KDT24DD_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT24DD.KDT24DDVec(const v: TKDT24DD_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT24DD_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT24DD.KDT24DDPow(const v: TKDT24DD_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT24DD.KDT24DDDistance(const v1, v2: TKDT24DD_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT24DD_Axis - 1 do
      Result := Result + KDT24DDPow(v2[i] - v1[i]);
end;

procedure TKDT24DD.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT24DD_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT24DD.Test;
var
  TKDT24DD_Test: TKDT24DD;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT24DD_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT24DD_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT24DD_Test := TKDT24DD.Create;

  DoStatusNoLn('...');
  SetLength(TKDT24DD_Test.TestBuff, 1000);
  for i := 0 to length(TKDT24DD_Test.TestBuff) - 1 do
    for j := 0 to KDT24DD_Axis - 1 do
        TKDT24DD_Test.TestBuff[i][j] := i * KDT24DD_Axis + j;

{$IFDEF FPC}
  TKDT24DD_Test.BuildKDTreeM(length(TKDT24DD_Test.TestBuff), nil, @TKDT24DD_Test.Test_BuildM);
{$ELSE FPC}
  TKDT24DD_Test.BuildKDTreeM(length(TKDT24DD_Test.TestBuff), nil, TKDT24DD_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT24DD_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT24DD_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT24DD_Test.TestBuff) - 1 do
    begin
      p := TKDT24DD_Test.Search(TKDT24DD_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT24DD_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT24DD_Test.TestBuff));
      TKDT24DD_Test.Search(TKDT24DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT24DDDistance(TKDT24DD_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT24DD_Test.Clear;
      { kMean test }
      TKDT24DD_Test.BuildKDTreeWithCluster(TKDT24DD_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT24DD_Test.Search(TKDT24DD_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT24DD_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT24DD_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT24DD_Test);
end;


procedure Test_All;
begin
  TKDT1DD.Test();
  TKDT2DD.Test();
  TKDT3DD.Test();
  TKDT4DD.Test();
  TKDT5DD.Test();
  TKDT6DD.Test();
  TKDT7DD.Test();
  TKDT8DD.Test();
  TKDT9DD.Test();
  TKDT10DD.Test();
  TKDT11DD.Test();
  TKDT12DD.Test();
  TKDT13DD.Test();
  TKDT14DD.Test();
  TKDT15DD.Test();
  TKDT16DD.Test();
  TKDT17DD.Test();
  TKDT18DD.Test();
  TKDT19DD.Test();
  TKDT20DD.Test();
  TKDT21DD.Test();
  TKDT22DD.Test();
  TKDT23DD.Test();
  TKDT24DD.Test();
end;





initialization

finalization

end.

