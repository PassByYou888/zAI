{ ****************************************************************************** }
{ Fast KDTree extended Type support                                              }
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

unit FastKDTreeE;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, PascalStrings, UnicodeMixedLib, KM;

const

  // extended float: KDTree
  KDT1DE_Axis = 1;
  KDT2DE_Axis = 2;
  KDT3DE_Axis = 3;
  KDT4DE_Axis = 4;
  KDT5DE_Axis = 5;
  KDT6DE_Axis = 6;
  KDT7DE_Axis = 7;
  KDT8DE_Axis = 8;
  KDT9DE_Axis = 9;
  KDT10DE_Axis = 10;
  KDT11DE_Axis = 11;
  KDT12DE_Axis = 12;
  KDT13DE_Axis = 13;
  KDT14DE_Axis = 14;
  KDT15DE_Axis = 15;
  KDT16DE_Axis = 16;
  KDT17DE_Axis = 17;
  KDT18DE_Axis = 18;
  KDT19DE_Axis = 19;
  KDT20DE_Axis = 20;
  KDT21DE_Axis = 21;
  KDT22DE_Axis = 22;
  KDT23DE_Axis = 23;
  KDT24DE_Axis = 24;
  KDT256DE_Axis = 256;
  KDT512DE_Axis = 512;
  KDT1024DE_Axis = 1024;

type

  // extended float: KDTree
  TKDT1DE = class; TKDT1DE_VecType = KM.TKMFloat; // 1D
  TKDT2DE = class; TKDT2DE_VecType = KM.TKMFloat; // 2D
  TKDT3DE = class; TKDT3DE_VecType = KM.TKMFloat; // 3D
  TKDT4DE = class; TKDT4DE_VecType = KM.TKMFloat; // 4D
  TKDT5DE = class; TKDT5DE_VecType = KM.TKMFloat; // 5D
  TKDT6DE = class; TKDT6DE_VecType = KM.TKMFloat; // 6D
  TKDT7DE = class; TKDT7DE_VecType = KM.TKMFloat; // 7D
  TKDT8DE = class; TKDT8DE_VecType = KM.TKMFloat; // 8D
  TKDT9DE = class; TKDT9DE_VecType = KM.TKMFloat; // 9D
  TKDT10DE = class; TKDT10DE_VecType = KM.TKMFloat; // 10D
  TKDT11DE = class; TKDT11DE_VecType = KM.TKMFloat; // 11D
  TKDT12DE = class; TKDT12DE_VecType = KM.TKMFloat; // 12D
  TKDT13DE = class; TKDT13DE_VecType = KM.TKMFloat; // 13D
  TKDT14DE = class; TKDT14DE_VecType = KM.TKMFloat; // 14D
  TKDT15DE = class; TKDT15DE_VecType = KM.TKMFloat; // 15D
  TKDT16DE = class; TKDT16DE_VecType = KM.TKMFloat; // 16D
  TKDT17DE = class; TKDT17DE_VecType = KM.TKMFloat; // 17D
  TKDT18DE = class; TKDT18DE_VecType = KM.TKMFloat; // 18D
  TKDT19DE = class; TKDT19DE_VecType = KM.TKMFloat; // 19D
  TKDT20DE = class; TKDT20DE_VecType = KM.TKMFloat; // 20D
  TKDT21DE = class; TKDT21DE_VecType = KM.TKMFloat; // 21D
  TKDT22DE = class; TKDT22DE_VecType = KM.TKMFloat; // 22D
  TKDT23DE = class; TKDT23DE_VecType = KM.TKMFloat; // 23D
  TKDT24DE = class; TKDT24DE_VecType = KM.TKMFloat; // 24D
  TKDT256DE = class; TKDT256DE_VecType = KM.TKMFloat; // 256D
  TKDT512DE = class; TKDT512DE_VecType = KM.TKMFloat; // 512D
  TKDT1024DE = class; TKDT1024DE_VecType = KM.TKMFloat; // 1024D





  // extended float: KDTree


  TKDT1DE = class(TCoreClassObject)
  public type
    // code split
    TKDT1DE_Vec = array [0 .. KDT1DE_Axis - 1] of TKDT1DE_VecType;
    PKDT1DE_Vec = ^TKDT1DE_Vec;

    TKDT1DE_DynamicVecBuffer = array of TKDT1DE_Vec;
    PKDT1DE_DynamicVecBuffer = ^TKDT1DE_DynamicVecBuffer;

    TKDT1DE_Source = record
      buff: TKDT1DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT1DE_Source = ^TKDT1DE_Source;
    TKDT1DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT1DE_Source) - 1] of PKDT1DE_Source;
    PKDT1DE_SourceBuffer = ^TKDT1DE_SourceBuffer;

    TKDT1DE_DyanmicSourceBuffer = array of PKDT1DE_Source;
    PKDT1DE_DyanmicSourceBuffer = ^TKDT1DE_DyanmicSourceBuffer;

    TKDT1DE_DyanmicStoreBuffer = array of TKDT1DE_Source;
    PKDT1DE_DyanmicStoreBuffer = ^TKDT1DE_DyanmicStoreBuffer;

    PKDT1DE_Node = ^TKDT1DE_Node;

    TKDT1DE_Node = record
      Parent, Right, Left: PKDT1DE_Node;
      vec: PKDT1DE_Source;
    end;

    TKDT1DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT1DE_Source; const Data: Pointer);
    TKDT1DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT1DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT1DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT1DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT1DE_DyanmicStoreBuffer;
    KDBuff: TKDT1DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT1DE_Node;
    TestBuff: TKDT1DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT1DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT1DE_Node;
    function GetData(const Index: NativeInt): PKDT1DE_Source;
  public
    RootNode: PKDT1DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT1DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT1DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT1DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT1DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT1DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT1DE_Node; overload;
    function Search(const buff: TKDT1DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT1DE_Node; overload;
    function Search(const buff: TKDT1DE_Vec; var SearchedDistanceMin: Double): PKDT1DE_Node; overload;
    function Search(const buff: TKDT1DE_Vec): PKDT1DE_Node; overload;
    function SearchToken(const buff: TKDT1DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT1DE_DynamicVecBuffer; var OutBuff: TKDT1DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT1DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT1DE_Node);
    procedure PrintBuffer;

    class function KDT1DEVec(const s: SystemString): TKDT1DE_Vec; overload;
    class function KDT1DEVec(const v: TKDT1DE_Vec): SystemString; overload;
    class function KDT1DEPow(const v: TKDT1DE_VecType): Double;
    class function KDT1DEDistance(const v1, v2: TKDT1DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT1DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT2DE = class(TCoreClassObject)
  public type
    // code split
    TKDT2DE_Vec = array [0 .. KDT2DE_Axis - 1] of TKDT2DE_VecType;
    PKDT2DE_Vec = ^TKDT2DE_Vec;

    TKDT2DE_DynamicVecBuffer = array of TKDT2DE_Vec;
    PKDT2DE_DynamicVecBuffer = ^TKDT2DE_DynamicVecBuffer;

    TKDT2DE_Source = record
      buff: TKDT2DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT2DE_Source = ^TKDT2DE_Source;
    TKDT2DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT2DE_Source) - 1] of PKDT2DE_Source;
    PKDT2DE_SourceBuffer = ^TKDT2DE_SourceBuffer;

    TKDT2DE_DyanmicSourceBuffer = array of PKDT2DE_Source;
    PKDT2DE_DyanmicSourceBuffer = ^TKDT2DE_DyanmicSourceBuffer;

    TKDT2DE_DyanmicStoreBuffer = array of TKDT2DE_Source;
    PKDT2DE_DyanmicStoreBuffer = ^TKDT2DE_DyanmicStoreBuffer;

    PKDT2DE_Node = ^TKDT2DE_Node;

    TKDT2DE_Node = record
      Parent, Right, Left: PKDT2DE_Node;
      vec: PKDT2DE_Source;
    end;

    TKDT2DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT2DE_Source; const Data: Pointer);
    TKDT2DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT2DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT2DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT2DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT2DE_DyanmicStoreBuffer;
    KDBuff: TKDT2DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT2DE_Node;
    TestBuff: TKDT2DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT2DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT2DE_Node;
    function GetData(const Index: NativeInt): PKDT2DE_Source;
  public
    RootNode: PKDT2DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT2DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT2DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT2DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT2DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT2DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT2DE_Node; overload;
    function Search(const buff: TKDT2DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT2DE_Node; overload;
    function Search(const buff: TKDT2DE_Vec; var SearchedDistanceMin: Double): PKDT2DE_Node; overload;
    function Search(const buff: TKDT2DE_Vec): PKDT2DE_Node; overload;
    function SearchToken(const buff: TKDT2DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT2DE_DynamicVecBuffer; var OutBuff: TKDT2DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT2DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT2DE_Node);
    procedure PrintBuffer;

    class function KDT2DEVec(const s: SystemString): TKDT2DE_Vec; overload;
    class function KDT2DEVec(const v: TKDT2DE_Vec): SystemString; overload;
    class function KDT2DEPow(const v: TKDT2DE_VecType): Double;
    class function KDT2DEDistance(const v1, v2: TKDT2DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT2DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT3DE = class(TCoreClassObject)
  public type
    // code split
    TKDT3DE_Vec = array [0 .. KDT3DE_Axis - 1] of TKDT3DE_VecType;
    PKDT3DE_Vec = ^TKDT3DE_Vec;

    TKDT3DE_DynamicVecBuffer = array of TKDT3DE_Vec;
    PKDT3DE_DynamicVecBuffer = ^TKDT3DE_DynamicVecBuffer;

    TKDT3DE_Source = record
      buff: TKDT3DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT3DE_Source = ^TKDT3DE_Source;
    TKDT3DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT3DE_Source) - 1] of PKDT3DE_Source;
    PKDT3DE_SourceBuffer = ^TKDT3DE_SourceBuffer;

    TKDT3DE_DyanmicSourceBuffer = array of PKDT3DE_Source;
    PKDT3DE_DyanmicSourceBuffer = ^TKDT3DE_DyanmicSourceBuffer;

    TKDT3DE_DyanmicStoreBuffer = array of TKDT3DE_Source;
    PKDT3DE_DyanmicStoreBuffer = ^TKDT3DE_DyanmicStoreBuffer;

    PKDT3DE_Node = ^TKDT3DE_Node;

    TKDT3DE_Node = record
      Parent, Right, Left: PKDT3DE_Node;
      vec: PKDT3DE_Source;
    end;

    TKDT3DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT3DE_Source; const Data: Pointer);
    TKDT3DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT3DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT3DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT3DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT3DE_DyanmicStoreBuffer;
    KDBuff: TKDT3DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT3DE_Node;
    TestBuff: TKDT3DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT3DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT3DE_Node;
    function GetData(const Index: NativeInt): PKDT3DE_Source;
  public
    RootNode: PKDT3DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT3DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT3DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT3DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT3DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT3DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT3DE_Node; overload;
    function Search(const buff: TKDT3DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT3DE_Node; overload;
    function Search(const buff: TKDT3DE_Vec; var SearchedDistanceMin: Double): PKDT3DE_Node; overload;
    function Search(const buff: TKDT3DE_Vec): PKDT3DE_Node; overload;
    function SearchToken(const buff: TKDT3DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT3DE_DynamicVecBuffer; var OutBuff: TKDT3DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT3DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT3DE_Node);
    procedure PrintBuffer;

    class function KDT3DEVec(const s: SystemString): TKDT3DE_Vec; overload;
    class function KDT3DEVec(const v: TKDT3DE_Vec): SystemString; overload;
    class function KDT3DEPow(const v: TKDT3DE_VecType): Double;
    class function KDT3DEDistance(const v1, v2: TKDT3DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT3DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT4DE = class(TCoreClassObject)
  public type
    // code split
    TKDT4DE_Vec = array [0 .. KDT4DE_Axis - 1] of TKDT4DE_VecType;
    PKDT4DE_Vec = ^TKDT4DE_Vec;

    TKDT4DE_DynamicVecBuffer = array of TKDT4DE_Vec;
    PKDT4DE_DynamicVecBuffer = ^TKDT4DE_DynamicVecBuffer;

    TKDT4DE_Source = record
      buff: TKDT4DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT4DE_Source = ^TKDT4DE_Source;
    TKDT4DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT4DE_Source) - 1] of PKDT4DE_Source;
    PKDT4DE_SourceBuffer = ^TKDT4DE_SourceBuffer;

    TKDT4DE_DyanmicSourceBuffer = array of PKDT4DE_Source;
    PKDT4DE_DyanmicSourceBuffer = ^TKDT4DE_DyanmicSourceBuffer;

    TKDT4DE_DyanmicStoreBuffer = array of TKDT4DE_Source;
    PKDT4DE_DyanmicStoreBuffer = ^TKDT4DE_DyanmicStoreBuffer;

    PKDT4DE_Node = ^TKDT4DE_Node;

    TKDT4DE_Node = record
      Parent, Right, Left: PKDT4DE_Node;
      vec: PKDT4DE_Source;
    end;

    TKDT4DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT4DE_Source; const Data: Pointer);
    TKDT4DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT4DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT4DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT4DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT4DE_DyanmicStoreBuffer;
    KDBuff: TKDT4DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT4DE_Node;
    TestBuff: TKDT4DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT4DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT4DE_Node;
    function GetData(const Index: NativeInt): PKDT4DE_Source;
  public
    RootNode: PKDT4DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT4DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT4DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT4DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT4DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT4DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT4DE_Node; overload;
    function Search(const buff: TKDT4DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT4DE_Node; overload;
    function Search(const buff: TKDT4DE_Vec; var SearchedDistanceMin: Double): PKDT4DE_Node; overload;
    function Search(const buff: TKDT4DE_Vec): PKDT4DE_Node; overload;
    function SearchToken(const buff: TKDT4DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT4DE_DynamicVecBuffer; var OutBuff: TKDT4DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT4DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT4DE_Node);
    procedure PrintBuffer;

    class function KDT4DEVec(const s: SystemString): TKDT4DE_Vec; overload;
    class function KDT4DEVec(const v: TKDT4DE_Vec): SystemString; overload;
    class function KDT4DEPow(const v: TKDT4DE_VecType): Double;
    class function KDT4DEDistance(const v1, v2: TKDT4DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT4DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT5DE = class(TCoreClassObject)
  public type
    // code split
    TKDT5DE_Vec = array [0 .. KDT5DE_Axis - 1] of TKDT5DE_VecType;
    PKDT5DE_Vec = ^TKDT5DE_Vec;

    TKDT5DE_DynamicVecBuffer = array of TKDT5DE_Vec;
    PKDT5DE_DynamicVecBuffer = ^TKDT5DE_DynamicVecBuffer;

    TKDT5DE_Source = record
      buff: TKDT5DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT5DE_Source = ^TKDT5DE_Source;
    TKDT5DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT5DE_Source) - 1] of PKDT5DE_Source;
    PKDT5DE_SourceBuffer = ^TKDT5DE_SourceBuffer;

    TKDT5DE_DyanmicSourceBuffer = array of PKDT5DE_Source;
    PKDT5DE_DyanmicSourceBuffer = ^TKDT5DE_DyanmicSourceBuffer;

    TKDT5DE_DyanmicStoreBuffer = array of TKDT5DE_Source;
    PKDT5DE_DyanmicStoreBuffer = ^TKDT5DE_DyanmicStoreBuffer;

    PKDT5DE_Node = ^TKDT5DE_Node;

    TKDT5DE_Node = record
      Parent, Right, Left: PKDT5DE_Node;
      vec: PKDT5DE_Source;
    end;

    TKDT5DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT5DE_Source; const Data: Pointer);
    TKDT5DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT5DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT5DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT5DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT5DE_DyanmicStoreBuffer;
    KDBuff: TKDT5DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT5DE_Node;
    TestBuff: TKDT5DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT5DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT5DE_Node;
    function GetData(const Index: NativeInt): PKDT5DE_Source;
  public
    RootNode: PKDT5DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT5DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT5DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT5DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT5DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT5DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT5DE_Node; overload;
    function Search(const buff: TKDT5DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT5DE_Node; overload;
    function Search(const buff: TKDT5DE_Vec; var SearchedDistanceMin: Double): PKDT5DE_Node; overload;
    function Search(const buff: TKDT5DE_Vec): PKDT5DE_Node; overload;
    function SearchToken(const buff: TKDT5DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT5DE_DynamicVecBuffer; var OutBuff: TKDT5DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT5DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT5DE_Node);
    procedure PrintBuffer;

    class function KDT5DEVec(const s: SystemString): TKDT5DE_Vec; overload;
    class function KDT5DEVec(const v: TKDT5DE_Vec): SystemString; overload;
    class function KDT5DEPow(const v: TKDT5DE_VecType): Double;
    class function KDT5DEDistance(const v1, v2: TKDT5DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT5DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT6DE = class(TCoreClassObject)
  public type
    // code split
    TKDT6DE_Vec = array [0 .. KDT6DE_Axis - 1] of TKDT6DE_VecType;
    PKDT6DE_Vec = ^TKDT6DE_Vec;

    TKDT6DE_DynamicVecBuffer = array of TKDT6DE_Vec;
    PKDT6DE_DynamicVecBuffer = ^TKDT6DE_DynamicVecBuffer;

    TKDT6DE_Source = record
      buff: TKDT6DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT6DE_Source = ^TKDT6DE_Source;
    TKDT6DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT6DE_Source) - 1] of PKDT6DE_Source;
    PKDT6DE_SourceBuffer = ^TKDT6DE_SourceBuffer;

    TKDT6DE_DyanmicSourceBuffer = array of PKDT6DE_Source;
    PKDT6DE_DyanmicSourceBuffer = ^TKDT6DE_DyanmicSourceBuffer;

    TKDT6DE_DyanmicStoreBuffer = array of TKDT6DE_Source;
    PKDT6DE_DyanmicStoreBuffer = ^TKDT6DE_DyanmicStoreBuffer;

    PKDT6DE_Node = ^TKDT6DE_Node;

    TKDT6DE_Node = record
      Parent, Right, Left: PKDT6DE_Node;
      vec: PKDT6DE_Source;
    end;

    TKDT6DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT6DE_Source; const Data: Pointer);
    TKDT6DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT6DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT6DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT6DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT6DE_DyanmicStoreBuffer;
    KDBuff: TKDT6DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT6DE_Node;
    TestBuff: TKDT6DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT6DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT6DE_Node;
    function GetData(const Index: NativeInt): PKDT6DE_Source;
  public
    RootNode: PKDT6DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT6DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT6DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT6DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT6DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT6DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT6DE_Node; overload;
    function Search(const buff: TKDT6DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT6DE_Node; overload;
    function Search(const buff: TKDT6DE_Vec; var SearchedDistanceMin: Double): PKDT6DE_Node; overload;
    function Search(const buff: TKDT6DE_Vec): PKDT6DE_Node; overload;
    function SearchToken(const buff: TKDT6DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT6DE_DynamicVecBuffer; var OutBuff: TKDT6DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT6DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT6DE_Node);
    procedure PrintBuffer;

    class function KDT6DEVec(const s: SystemString): TKDT6DE_Vec; overload;
    class function KDT6DEVec(const v: TKDT6DE_Vec): SystemString; overload;
    class function KDT6DEPow(const v: TKDT6DE_VecType): Double;
    class function KDT6DEDistance(const v1, v2: TKDT6DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT6DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT7DE = class(TCoreClassObject)
  public type
    // code split
    TKDT7DE_Vec = array [0 .. KDT7DE_Axis - 1] of TKDT7DE_VecType;
    PKDT7DE_Vec = ^TKDT7DE_Vec;

    TKDT7DE_DynamicVecBuffer = array of TKDT7DE_Vec;
    PKDT7DE_DynamicVecBuffer = ^TKDT7DE_DynamicVecBuffer;

    TKDT7DE_Source = record
      buff: TKDT7DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT7DE_Source = ^TKDT7DE_Source;
    TKDT7DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT7DE_Source) - 1] of PKDT7DE_Source;
    PKDT7DE_SourceBuffer = ^TKDT7DE_SourceBuffer;

    TKDT7DE_DyanmicSourceBuffer = array of PKDT7DE_Source;
    PKDT7DE_DyanmicSourceBuffer = ^TKDT7DE_DyanmicSourceBuffer;

    TKDT7DE_DyanmicStoreBuffer = array of TKDT7DE_Source;
    PKDT7DE_DyanmicStoreBuffer = ^TKDT7DE_DyanmicStoreBuffer;

    PKDT7DE_Node = ^TKDT7DE_Node;

    TKDT7DE_Node = record
      Parent, Right, Left: PKDT7DE_Node;
      vec: PKDT7DE_Source;
    end;

    TKDT7DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT7DE_Source; const Data: Pointer);
    TKDT7DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT7DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT7DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT7DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT7DE_DyanmicStoreBuffer;
    KDBuff: TKDT7DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT7DE_Node;
    TestBuff: TKDT7DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT7DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT7DE_Node;
    function GetData(const Index: NativeInt): PKDT7DE_Source;
  public
    RootNode: PKDT7DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT7DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT7DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT7DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT7DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT7DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT7DE_Node; overload;
    function Search(const buff: TKDT7DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT7DE_Node; overload;
    function Search(const buff: TKDT7DE_Vec; var SearchedDistanceMin: Double): PKDT7DE_Node; overload;
    function Search(const buff: TKDT7DE_Vec): PKDT7DE_Node; overload;
    function SearchToken(const buff: TKDT7DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT7DE_DynamicVecBuffer; var OutBuff: TKDT7DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT7DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT7DE_Node);
    procedure PrintBuffer;

    class function KDT7DEVec(const s: SystemString): TKDT7DE_Vec; overload;
    class function KDT7DEVec(const v: TKDT7DE_Vec): SystemString; overload;
    class function KDT7DEPow(const v: TKDT7DE_VecType): Double;
    class function KDT7DEDistance(const v1, v2: TKDT7DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT7DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT8DE = class(TCoreClassObject)
  public type
    // code split
    TKDT8DE_Vec = array [0 .. KDT8DE_Axis - 1] of TKDT8DE_VecType;
    PKDT8DE_Vec = ^TKDT8DE_Vec;

    TKDT8DE_DynamicVecBuffer = array of TKDT8DE_Vec;
    PKDT8DE_DynamicVecBuffer = ^TKDT8DE_DynamicVecBuffer;

    TKDT8DE_Source = record
      buff: TKDT8DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT8DE_Source = ^TKDT8DE_Source;
    TKDT8DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT8DE_Source) - 1] of PKDT8DE_Source;
    PKDT8DE_SourceBuffer = ^TKDT8DE_SourceBuffer;

    TKDT8DE_DyanmicSourceBuffer = array of PKDT8DE_Source;
    PKDT8DE_DyanmicSourceBuffer = ^TKDT8DE_DyanmicSourceBuffer;

    TKDT8DE_DyanmicStoreBuffer = array of TKDT8DE_Source;
    PKDT8DE_DyanmicStoreBuffer = ^TKDT8DE_DyanmicStoreBuffer;

    PKDT8DE_Node = ^TKDT8DE_Node;

    TKDT8DE_Node = record
      Parent, Right, Left: PKDT8DE_Node;
      vec: PKDT8DE_Source;
    end;

    TKDT8DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT8DE_Source; const Data: Pointer);
    TKDT8DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT8DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT8DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT8DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT8DE_DyanmicStoreBuffer;
    KDBuff: TKDT8DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT8DE_Node;
    TestBuff: TKDT8DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT8DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT8DE_Node;
    function GetData(const Index: NativeInt): PKDT8DE_Source;
  public
    RootNode: PKDT8DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT8DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT8DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT8DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT8DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT8DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT8DE_Node; overload;
    function Search(const buff: TKDT8DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT8DE_Node; overload;
    function Search(const buff: TKDT8DE_Vec; var SearchedDistanceMin: Double): PKDT8DE_Node; overload;
    function Search(const buff: TKDT8DE_Vec): PKDT8DE_Node; overload;
    function SearchToken(const buff: TKDT8DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT8DE_DynamicVecBuffer; var OutBuff: TKDT8DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT8DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT8DE_Node);
    procedure PrintBuffer;

    class function KDT8DEVec(const s: SystemString): TKDT8DE_Vec; overload;
    class function KDT8DEVec(const v: TKDT8DE_Vec): SystemString; overload;
    class function KDT8DEPow(const v: TKDT8DE_VecType): Double;
    class function KDT8DEDistance(const v1, v2: TKDT8DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT8DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT9DE = class(TCoreClassObject)
  public type
    // code split
    TKDT9DE_Vec = array [0 .. KDT9DE_Axis - 1] of TKDT9DE_VecType;
    PKDT9DE_Vec = ^TKDT9DE_Vec;

    TKDT9DE_DynamicVecBuffer = array of TKDT9DE_Vec;
    PKDT9DE_DynamicVecBuffer = ^TKDT9DE_DynamicVecBuffer;

    TKDT9DE_Source = record
      buff: TKDT9DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT9DE_Source = ^TKDT9DE_Source;
    TKDT9DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT9DE_Source) - 1] of PKDT9DE_Source;
    PKDT9DE_SourceBuffer = ^TKDT9DE_SourceBuffer;

    TKDT9DE_DyanmicSourceBuffer = array of PKDT9DE_Source;
    PKDT9DE_DyanmicSourceBuffer = ^TKDT9DE_DyanmicSourceBuffer;

    TKDT9DE_DyanmicStoreBuffer = array of TKDT9DE_Source;
    PKDT9DE_DyanmicStoreBuffer = ^TKDT9DE_DyanmicStoreBuffer;

    PKDT9DE_Node = ^TKDT9DE_Node;

    TKDT9DE_Node = record
      Parent, Right, Left: PKDT9DE_Node;
      vec: PKDT9DE_Source;
    end;

    TKDT9DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT9DE_Source; const Data: Pointer);
    TKDT9DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT9DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT9DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT9DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT9DE_DyanmicStoreBuffer;
    KDBuff: TKDT9DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT9DE_Node;
    TestBuff: TKDT9DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT9DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT9DE_Node;
    function GetData(const Index: NativeInt): PKDT9DE_Source;
  public
    RootNode: PKDT9DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT9DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT9DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT9DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT9DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT9DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT9DE_Node; overload;
    function Search(const buff: TKDT9DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT9DE_Node; overload;
    function Search(const buff: TKDT9DE_Vec; var SearchedDistanceMin: Double): PKDT9DE_Node; overload;
    function Search(const buff: TKDT9DE_Vec): PKDT9DE_Node; overload;
    function SearchToken(const buff: TKDT9DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT9DE_DynamicVecBuffer; var OutBuff: TKDT9DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT9DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT9DE_Node);
    procedure PrintBuffer;

    class function KDT9DEVec(const s: SystemString): TKDT9DE_Vec; overload;
    class function KDT9DEVec(const v: TKDT9DE_Vec): SystemString; overload;
    class function KDT9DEPow(const v: TKDT9DE_VecType): Double;
    class function KDT9DEDistance(const v1, v2: TKDT9DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT9DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT10DE = class(TCoreClassObject)
  public type
    // code split
    TKDT10DE_Vec = array [0 .. KDT10DE_Axis - 1] of TKDT10DE_VecType;
    PKDT10DE_Vec = ^TKDT10DE_Vec;

    TKDT10DE_DynamicVecBuffer = array of TKDT10DE_Vec;
    PKDT10DE_DynamicVecBuffer = ^TKDT10DE_DynamicVecBuffer;

    TKDT10DE_Source = record
      buff: TKDT10DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT10DE_Source = ^TKDT10DE_Source;
    TKDT10DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT10DE_Source) - 1] of PKDT10DE_Source;
    PKDT10DE_SourceBuffer = ^TKDT10DE_SourceBuffer;

    TKDT10DE_DyanmicSourceBuffer = array of PKDT10DE_Source;
    PKDT10DE_DyanmicSourceBuffer = ^TKDT10DE_DyanmicSourceBuffer;

    TKDT10DE_DyanmicStoreBuffer = array of TKDT10DE_Source;
    PKDT10DE_DyanmicStoreBuffer = ^TKDT10DE_DyanmicStoreBuffer;

    PKDT10DE_Node = ^TKDT10DE_Node;

    TKDT10DE_Node = record
      Parent, Right, Left: PKDT10DE_Node;
      vec: PKDT10DE_Source;
    end;

    TKDT10DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT10DE_Source; const Data: Pointer);
    TKDT10DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT10DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT10DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT10DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT10DE_DyanmicStoreBuffer;
    KDBuff: TKDT10DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT10DE_Node;
    TestBuff: TKDT10DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT10DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT10DE_Node;
    function GetData(const Index: NativeInt): PKDT10DE_Source;
  public
    RootNode: PKDT10DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT10DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT10DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT10DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT10DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT10DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT10DE_Node; overload;
    function Search(const buff: TKDT10DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT10DE_Node; overload;
    function Search(const buff: TKDT10DE_Vec; var SearchedDistanceMin: Double): PKDT10DE_Node; overload;
    function Search(const buff: TKDT10DE_Vec): PKDT10DE_Node; overload;
    function SearchToken(const buff: TKDT10DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT10DE_DynamicVecBuffer; var OutBuff: TKDT10DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT10DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT10DE_Node);
    procedure PrintBuffer;

    class function KDT10DEVec(const s: SystemString): TKDT10DE_Vec; overload;
    class function KDT10DEVec(const v: TKDT10DE_Vec): SystemString; overload;
    class function KDT10DEPow(const v: TKDT10DE_VecType): Double;
    class function KDT10DEDistance(const v1, v2: TKDT10DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT10DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT11DE = class(TCoreClassObject)
  public type
    // code split
    TKDT11DE_Vec = array [0 .. KDT11DE_Axis - 1] of TKDT11DE_VecType;
    PKDT11DE_Vec = ^TKDT11DE_Vec;

    TKDT11DE_DynamicVecBuffer = array of TKDT11DE_Vec;
    PKDT11DE_DynamicVecBuffer = ^TKDT11DE_DynamicVecBuffer;

    TKDT11DE_Source = record
      buff: TKDT11DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT11DE_Source = ^TKDT11DE_Source;
    TKDT11DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT11DE_Source) - 1] of PKDT11DE_Source;
    PKDT11DE_SourceBuffer = ^TKDT11DE_SourceBuffer;

    TKDT11DE_DyanmicSourceBuffer = array of PKDT11DE_Source;
    PKDT11DE_DyanmicSourceBuffer = ^TKDT11DE_DyanmicSourceBuffer;

    TKDT11DE_DyanmicStoreBuffer = array of TKDT11DE_Source;
    PKDT11DE_DyanmicStoreBuffer = ^TKDT11DE_DyanmicStoreBuffer;

    PKDT11DE_Node = ^TKDT11DE_Node;

    TKDT11DE_Node = record
      Parent, Right, Left: PKDT11DE_Node;
      vec: PKDT11DE_Source;
    end;

    TKDT11DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT11DE_Source; const Data: Pointer);
    TKDT11DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT11DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT11DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT11DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT11DE_DyanmicStoreBuffer;
    KDBuff: TKDT11DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT11DE_Node;
    TestBuff: TKDT11DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT11DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT11DE_Node;
    function GetData(const Index: NativeInt): PKDT11DE_Source;
  public
    RootNode: PKDT11DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT11DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT11DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT11DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT11DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT11DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT11DE_Node; overload;
    function Search(const buff: TKDT11DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT11DE_Node; overload;
    function Search(const buff: TKDT11DE_Vec; var SearchedDistanceMin: Double): PKDT11DE_Node; overload;
    function Search(const buff: TKDT11DE_Vec): PKDT11DE_Node; overload;
    function SearchToken(const buff: TKDT11DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT11DE_DynamicVecBuffer; var OutBuff: TKDT11DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT11DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT11DE_Node);
    procedure PrintBuffer;

    class function KDT11DEVec(const s: SystemString): TKDT11DE_Vec; overload;
    class function KDT11DEVec(const v: TKDT11DE_Vec): SystemString; overload;
    class function KDT11DEPow(const v: TKDT11DE_VecType): Double;
    class function KDT11DEDistance(const v1, v2: TKDT11DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT11DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT12DE = class(TCoreClassObject)
  public type
    // code split
    TKDT12DE_Vec = array [0 .. KDT12DE_Axis - 1] of TKDT12DE_VecType;
    PKDT12DE_Vec = ^TKDT12DE_Vec;

    TKDT12DE_DynamicVecBuffer = array of TKDT12DE_Vec;
    PKDT12DE_DynamicVecBuffer = ^TKDT12DE_DynamicVecBuffer;

    TKDT12DE_Source = record
      buff: TKDT12DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT12DE_Source = ^TKDT12DE_Source;
    TKDT12DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT12DE_Source) - 1] of PKDT12DE_Source;
    PKDT12DE_SourceBuffer = ^TKDT12DE_SourceBuffer;

    TKDT12DE_DyanmicSourceBuffer = array of PKDT12DE_Source;
    PKDT12DE_DyanmicSourceBuffer = ^TKDT12DE_DyanmicSourceBuffer;

    TKDT12DE_DyanmicStoreBuffer = array of TKDT12DE_Source;
    PKDT12DE_DyanmicStoreBuffer = ^TKDT12DE_DyanmicStoreBuffer;

    PKDT12DE_Node = ^TKDT12DE_Node;

    TKDT12DE_Node = record
      Parent, Right, Left: PKDT12DE_Node;
      vec: PKDT12DE_Source;
    end;

    TKDT12DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT12DE_Source; const Data: Pointer);
    TKDT12DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT12DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT12DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT12DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT12DE_DyanmicStoreBuffer;
    KDBuff: TKDT12DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT12DE_Node;
    TestBuff: TKDT12DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT12DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT12DE_Node;
    function GetData(const Index: NativeInt): PKDT12DE_Source;
  public
    RootNode: PKDT12DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT12DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT12DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT12DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT12DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT12DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT12DE_Node; overload;
    function Search(const buff: TKDT12DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT12DE_Node; overload;
    function Search(const buff: TKDT12DE_Vec; var SearchedDistanceMin: Double): PKDT12DE_Node; overload;
    function Search(const buff: TKDT12DE_Vec): PKDT12DE_Node; overload;
    function SearchToken(const buff: TKDT12DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT12DE_DynamicVecBuffer; var OutBuff: TKDT12DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT12DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT12DE_Node);
    procedure PrintBuffer;

    class function KDT12DEVec(const s: SystemString): TKDT12DE_Vec; overload;
    class function KDT12DEVec(const v: TKDT12DE_Vec): SystemString; overload;
    class function KDT12DEPow(const v: TKDT12DE_VecType): Double;
    class function KDT12DEDistance(const v1, v2: TKDT12DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT12DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT13DE = class(TCoreClassObject)
  public type
    // code split
    TKDT13DE_Vec = array [0 .. KDT13DE_Axis - 1] of TKDT13DE_VecType;
    PKDT13DE_Vec = ^TKDT13DE_Vec;

    TKDT13DE_DynamicVecBuffer = array of TKDT13DE_Vec;
    PKDT13DE_DynamicVecBuffer = ^TKDT13DE_DynamicVecBuffer;

    TKDT13DE_Source = record
      buff: TKDT13DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT13DE_Source = ^TKDT13DE_Source;
    TKDT13DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT13DE_Source) - 1] of PKDT13DE_Source;
    PKDT13DE_SourceBuffer = ^TKDT13DE_SourceBuffer;

    TKDT13DE_DyanmicSourceBuffer = array of PKDT13DE_Source;
    PKDT13DE_DyanmicSourceBuffer = ^TKDT13DE_DyanmicSourceBuffer;

    TKDT13DE_DyanmicStoreBuffer = array of TKDT13DE_Source;
    PKDT13DE_DyanmicStoreBuffer = ^TKDT13DE_DyanmicStoreBuffer;

    PKDT13DE_Node = ^TKDT13DE_Node;

    TKDT13DE_Node = record
      Parent, Right, Left: PKDT13DE_Node;
      vec: PKDT13DE_Source;
    end;

    TKDT13DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT13DE_Source; const Data: Pointer);
    TKDT13DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT13DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT13DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT13DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT13DE_DyanmicStoreBuffer;
    KDBuff: TKDT13DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT13DE_Node;
    TestBuff: TKDT13DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT13DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT13DE_Node;
    function GetData(const Index: NativeInt): PKDT13DE_Source;
  public
    RootNode: PKDT13DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT13DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT13DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT13DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT13DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT13DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT13DE_Node; overload;
    function Search(const buff: TKDT13DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT13DE_Node; overload;
    function Search(const buff: TKDT13DE_Vec; var SearchedDistanceMin: Double): PKDT13DE_Node; overload;
    function Search(const buff: TKDT13DE_Vec): PKDT13DE_Node; overload;
    function SearchToken(const buff: TKDT13DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT13DE_DynamicVecBuffer; var OutBuff: TKDT13DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT13DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT13DE_Node);
    procedure PrintBuffer;

    class function KDT13DEVec(const s: SystemString): TKDT13DE_Vec; overload;
    class function KDT13DEVec(const v: TKDT13DE_Vec): SystemString; overload;
    class function KDT13DEPow(const v: TKDT13DE_VecType): Double;
    class function KDT13DEDistance(const v1, v2: TKDT13DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT13DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT14DE = class(TCoreClassObject)
  public type
    // code split
    TKDT14DE_Vec = array [0 .. KDT14DE_Axis - 1] of TKDT14DE_VecType;
    PKDT14DE_Vec = ^TKDT14DE_Vec;

    TKDT14DE_DynamicVecBuffer = array of TKDT14DE_Vec;
    PKDT14DE_DynamicVecBuffer = ^TKDT14DE_DynamicVecBuffer;

    TKDT14DE_Source = record
      buff: TKDT14DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT14DE_Source = ^TKDT14DE_Source;
    TKDT14DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT14DE_Source) - 1] of PKDT14DE_Source;
    PKDT14DE_SourceBuffer = ^TKDT14DE_SourceBuffer;

    TKDT14DE_DyanmicSourceBuffer = array of PKDT14DE_Source;
    PKDT14DE_DyanmicSourceBuffer = ^TKDT14DE_DyanmicSourceBuffer;

    TKDT14DE_DyanmicStoreBuffer = array of TKDT14DE_Source;
    PKDT14DE_DyanmicStoreBuffer = ^TKDT14DE_DyanmicStoreBuffer;

    PKDT14DE_Node = ^TKDT14DE_Node;

    TKDT14DE_Node = record
      Parent, Right, Left: PKDT14DE_Node;
      vec: PKDT14DE_Source;
    end;

    TKDT14DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT14DE_Source; const Data: Pointer);
    TKDT14DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT14DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT14DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT14DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT14DE_DyanmicStoreBuffer;
    KDBuff: TKDT14DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT14DE_Node;
    TestBuff: TKDT14DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT14DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT14DE_Node;
    function GetData(const Index: NativeInt): PKDT14DE_Source;
  public
    RootNode: PKDT14DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT14DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT14DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT14DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT14DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT14DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT14DE_Node; overload;
    function Search(const buff: TKDT14DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT14DE_Node; overload;
    function Search(const buff: TKDT14DE_Vec; var SearchedDistanceMin: Double): PKDT14DE_Node; overload;
    function Search(const buff: TKDT14DE_Vec): PKDT14DE_Node; overload;
    function SearchToken(const buff: TKDT14DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT14DE_DynamicVecBuffer; var OutBuff: TKDT14DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT14DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT14DE_Node);
    procedure PrintBuffer;

    class function KDT14DEVec(const s: SystemString): TKDT14DE_Vec; overload;
    class function KDT14DEVec(const v: TKDT14DE_Vec): SystemString; overload;
    class function KDT14DEPow(const v: TKDT14DE_VecType): Double;
    class function KDT14DEDistance(const v1, v2: TKDT14DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT14DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT15DE = class(TCoreClassObject)
  public type
    // code split
    TKDT15DE_Vec = array [0 .. KDT15DE_Axis - 1] of TKDT15DE_VecType;
    PKDT15DE_Vec = ^TKDT15DE_Vec;

    TKDT15DE_DynamicVecBuffer = array of TKDT15DE_Vec;
    PKDT15DE_DynamicVecBuffer = ^TKDT15DE_DynamicVecBuffer;

    TKDT15DE_Source = record
      buff: TKDT15DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT15DE_Source = ^TKDT15DE_Source;
    TKDT15DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT15DE_Source) - 1] of PKDT15DE_Source;
    PKDT15DE_SourceBuffer = ^TKDT15DE_SourceBuffer;

    TKDT15DE_DyanmicSourceBuffer = array of PKDT15DE_Source;
    PKDT15DE_DyanmicSourceBuffer = ^TKDT15DE_DyanmicSourceBuffer;

    TKDT15DE_DyanmicStoreBuffer = array of TKDT15DE_Source;
    PKDT15DE_DyanmicStoreBuffer = ^TKDT15DE_DyanmicStoreBuffer;

    PKDT15DE_Node = ^TKDT15DE_Node;

    TKDT15DE_Node = record
      Parent, Right, Left: PKDT15DE_Node;
      vec: PKDT15DE_Source;
    end;

    TKDT15DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT15DE_Source; const Data: Pointer);
    TKDT15DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT15DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT15DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT15DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT15DE_DyanmicStoreBuffer;
    KDBuff: TKDT15DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT15DE_Node;
    TestBuff: TKDT15DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT15DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT15DE_Node;
    function GetData(const Index: NativeInt): PKDT15DE_Source;
  public
    RootNode: PKDT15DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT15DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT15DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT15DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT15DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT15DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT15DE_Node; overload;
    function Search(const buff: TKDT15DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT15DE_Node; overload;
    function Search(const buff: TKDT15DE_Vec; var SearchedDistanceMin: Double): PKDT15DE_Node; overload;
    function Search(const buff: TKDT15DE_Vec): PKDT15DE_Node; overload;
    function SearchToken(const buff: TKDT15DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT15DE_DynamicVecBuffer; var OutBuff: TKDT15DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT15DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT15DE_Node);
    procedure PrintBuffer;

    class function KDT15DEVec(const s: SystemString): TKDT15DE_Vec; overload;
    class function KDT15DEVec(const v: TKDT15DE_Vec): SystemString; overload;
    class function KDT15DEPow(const v: TKDT15DE_VecType): Double;
    class function KDT15DEDistance(const v1, v2: TKDT15DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT15DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT16DE = class(TCoreClassObject)
  public type
    // code split
    TKDT16DE_Vec = array [0 .. KDT16DE_Axis - 1] of TKDT16DE_VecType;
    PKDT16DE_Vec = ^TKDT16DE_Vec;

    TKDT16DE_DynamicVecBuffer = array of TKDT16DE_Vec;
    PKDT16DE_DynamicVecBuffer = ^TKDT16DE_DynamicVecBuffer;

    TKDT16DE_Source = record
      buff: TKDT16DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT16DE_Source = ^TKDT16DE_Source;
    TKDT16DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT16DE_Source) - 1] of PKDT16DE_Source;
    PKDT16DE_SourceBuffer = ^TKDT16DE_SourceBuffer;

    TKDT16DE_DyanmicSourceBuffer = array of PKDT16DE_Source;
    PKDT16DE_DyanmicSourceBuffer = ^TKDT16DE_DyanmicSourceBuffer;

    TKDT16DE_DyanmicStoreBuffer = array of TKDT16DE_Source;
    PKDT16DE_DyanmicStoreBuffer = ^TKDT16DE_DyanmicStoreBuffer;

    PKDT16DE_Node = ^TKDT16DE_Node;

    TKDT16DE_Node = record
      Parent, Right, Left: PKDT16DE_Node;
      vec: PKDT16DE_Source;
    end;

    TKDT16DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT16DE_Source; const Data: Pointer);
    TKDT16DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT16DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT16DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT16DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT16DE_DyanmicStoreBuffer;
    KDBuff: TKDT16DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT16DE_Node;
    TestBuff: TKDT16DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT16DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT16DE_Node;
    function GetData(const Index: NativeInt): PKDT16DE_Source;
  public
    RootNode: PKDT16DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT16DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT16DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT16DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT16DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT16DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT16DE_Node; overload;
    function Search(const buff: TKDT16DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT16DE_Node; overload;
    function Search(const buff: TKDT16DE_Vec; var SearchedDistanceMin: Double): PKDT16DE_Node; overload;
    function Search(const buff: TKDT16DE_Vec): PKDT16DE_Node; overload;
    function SearchToken(const buff: TKDT16DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT16DE_DynamicVecBuffer; var OutBuff: TKDT16DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT16DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT16DE_Node);
    procedure PrintBuffer;

    class function KDT16DEVec(const s: SystemString): TKDT16DE_Vec; overload;
    class function KDT16DEVec(const v: TKDT16DE_Vec): SystemString; overload;
    class function KDT16DEPow(const v: TKDT16DE_VecType): Double;
    class function KDT16DEDistance(const v1, v2: TKDT16DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT16DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT17DE = class(TCoreClassObject)
  public type
    // code split
    TKDT17DE_Vec = array [0 .. KDT17DE_Axis - 1] of TKDT17DE_VecType;
    PKDT17DE_Vec = ^TKDT17DE_Vec;

    TKDT17DE_DynamicVecBuffer = array of TKDT17DE_Vec;
    PKDT17DE_DynamicVecBuffer = ^TKDT17DE_DynamicVecBuffer;

    TKDT17DE_Source = record
      buff: TKDT17DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT17DE_Source = ^TKDT17DE_Source;
    TKDT17DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT17DE_Source) - 1] of PKDT17DE_Source;
    PKDT17DE_SourceBuffer = ^TKDT17DE_SourceBuffer;

    TKDT17DE_DyanmicSourceBuffer = array of PKDT17DE_Source;
    PKDT17DE_DyanmicSourceBuffer = ^TKDT17DE_DyanmicSourceBuffer;

    TKDT17DE_DyanmicStoreBuffer = array of TKDT17DE_Source;
    PKDT17DE_DyanmicStoreBuffer = ^TKDT17DE_DyanmicStoreBuffer;

    PKDT17DE_Node = ^TKDT17DE_Node;

    TKDT17DE_Node = record
      Parent, Right, Left: PKDT17DE_Node;
      vec: PKDT17DE_Source;
    end;

    TKDT17DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT17DE_Source; const Data: Pointer);
    TKDT17DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT17DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT17DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT17DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT17DE_DyanmicStoreBuffer;
    KDBuff: TKDT17DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT17DE_Node;
    TestBuff: TKDT17DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT17DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT17DE_Node;
    function GetData(const Index: NativeInt): PKDT17DE_Source;
  public
    RootNode: PKDT17DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT17DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT17DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT17DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT17DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT17DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT17DE_Node; overload;
    function Search(const buff: TKDT17DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT17DE_Node; overload;
    function Search(const buff: TKDT17DE_Vec; var SearchedDistanceMin: Double): PKDT17DE_Node; overload;
    function Search(const buff: TKDT17DE_Vec): PKDT17DE_Node; overload;
    function SearchToken(const buff: TKDT17DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT17DE_DynamicVecBuffer; var OutBuff: TKDT17DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT17DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT17DE_Node);
    procedure PrintBuffer;

    class function KDT17DEVec(const s: SystemString): TKDT17DE_Vec; overload;
    class function KDT17DEVec(const v: TKDT17DE_Vec): SystemString; overload;
    class function KDT17DEPow(const v: TKDT17DE_VecType): Double;
    class function KDT17DEDistance(const v1, v2: TKDT17DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT17DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT18DE = class(TCoreClassObject)
  public type
    // code split
    TKDT18DE_Vec = array [0 .. KDT18DE_Axis - 1] of TKDT18DE_VecType;
    PKDT18DE_Vec = ^TKDT18DE_Vec;

    TKDT18DE_DynamicVecBuffer = array of TKDT18DE_Vec;
    PKDT18DE_DynamicVecBuffer = ^TKDT18DE_DynamicVecBuffer;

    TKDT18DE_Source = record
      buff: TKDT18DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT18DE_Source = ^TKDT18DE_Source;
    TKDT18DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT18DE_Source) - 1] of PKDT18DE_Source;
    PKDT18DE_SourceBuffer = ^TKDT18DE_SourceBuffer;

    TKDT18DE_DyanmicSourceBuffer = array of PKDT18DE_Source;
    PKDT18DE_DyanmicSourceBuffer = ^TKDT18DE_DyanmicSourceBuffer;

    TKDT18DE_DyanmicStoreBuffer = array of TKDT18DE_Source;
    PKDT18DE_DyanmicStoreBuffer = ^TKDT18DE_DyanmicStoreBuffer;

    PKDT18DE_Node = ^TKDT18DE_Node;

    TKDT18DE_Node = record
      Parent, Right, Left: PKDT18DE_Node;
      vec: PKDT18DE_Source;
    end;

    TKDT18DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT18DE_Source; const Data: Pointer);
    TKDT18DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT18DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT18DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT18DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT18DE_DyanmicStoreBuffer;
    KDBuff: TKDT18DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT18DE_Node;
    TestBuff: TKDT18DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT18DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT18DE_Node;
    function GetData(const Index: NativeInt): PKDT18DE_Source;
  public
    RootNode: PKDT18DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT18DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT18DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT18DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT18DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT18DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT18DE_Node; overload;
    function Search(const buff: TKDT18DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT18DE_Node; overload;
    function Search(const buff: TKDT18DE_Vec; var SearchedDistanceMin: Double): PKDT18DE_Node; overload;
    function Search(const buff: TKDT18DE_Vec): PKDT18DE_Node; overload;
    function SearchToken(const buff: TKDT18DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT18DE_DynamicVecBuffer; var OutBuff: TKDT18DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT18DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT18DE_Node);
    procedure PrintBuffer;

    class function KDT18DEVec(const s: SystemString): TKDT18DE_Vec; overload;
    class function KDT18DEVec(const v: TKDT18DE_Vec): SystemString; overload;
    class function KDT18DEPow(const v: TKDT18DE_VecType): Double;
    class function KDT18DEDistance(const v1, v2: TKDT18DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT18DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT19DE = class(TCoreClassObject)
  public type
    // code split
    TKDT19DE_Vec = array [0 .. KDT19DE_Axis - 1] of TKDT19DE_VecType;
    PKDT19DE_Vec = ^TKDT19DE_Vec;

    TKDT19DE_DynamicVecBuffer = array of TKDT19DE_Vec;
    PKDT19DE_DynamicVecBuffer = ^TKDT19DE_DynamicVecBuffer;

    TKDT19DE_Source = record
      buff: TKDT19DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT19DE_Source = ^TKDT19DE_Source;
    TKDT19DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT19DE_Source) - 1] of PKDT19DE_Source;
    PKDT19DE_SourceBuffer = ^TKDT19DE_SourceBuffer;

    TKDT19DE_DyanmicSourceBuffer = array of PKDT19DE_Source;
    PKDT19DE_DyanmicSourceBuffer = ^TKDT19DE_DyanmicSourceBuffer;

    TKDT19DE_DyanmicStoreBuffer = array of TKDT19DE_Source;
    PKDT19DE_DyanmicStoreBuffer = ^TKDT19DE_DyanmicStoreBuffer;

    PKDT19DE_Node = ^TKDT19DE_Node;

    TKDT19DE_Node = record
      Parent, Right, Left: PKDT19DE_Node;
      vec: PKDT19DE_Source;
    end;

    TKDT19DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT19DE_Source; const Data: Pointer);
    TKDT19DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT19DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT19DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT19DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT19DE_DyanmicStoreBuffer;
    KDBuff: TKDT19DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT19DE_Node;
    TestBuff: TKDT19DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT19DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT19DE_Node;
    function GetData(const Index: NativeInt): PKDT19DE_Source;
  public
    RootNode: PKDT19DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT19DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT19DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT19DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT19DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT19DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT19DE_Node; overload;
    function Search(const buff: TKDT19DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT19DE_Node; overload;
    function Search(const buff: TKDT19DE_Vec; var SearchedDistanceMin: Double): PKDT19DE_Node; overload;
    function Search(const buff: TKDT19DE_Vec): PKDT19DE_Node; overload;
    function SearchToken(const buff: TKDT19DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT19DE_DynamicVecBuffer; var OutBuff: TKDT19DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT19DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT19DE_Node);
    procedure PrintBuffer;

    class function KDT19DEVec(const s: SystemString): TKDT19DE_Vec; overload;
    class function KDT19DEVec(const v: TKDT19DE_Vec): SystemString; overload;
    class function KDT19DEPow(const v: TKDT19DE_VecType): Double;
    class function KDT19DEDistance(const v1, v2: TKDT19DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT19DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT20DE = class(TCoreClassObject)
  public type
    // code split
    TKDT20DE_Vec = array [0 .. KDT20DE_Axis - 1] of TKDT20DE_VecType;
    PKDT20DE_Vec = ^TKDT20DE_Vec;

    TKDT20DE_DynamicVecBuffer = array of TKDT20DE_Vec;
    PKDT20DE_DynamicVecBuffer = ^TKDT20DE_DynamicVecBuffer;

    TKDT20DE_Source = record
      buff: TKDT20DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT20DE_Source = ^TKDT20DE_Source;
    TKDT20DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT20DE_Source) - 1] of PKDT20DE_Source;
    PKDT20DE_SourceBuffer = ^TKDT20DE_SourceBuffer;

    TKDT20DE_DyanmicSourceBuffer = array of PKDT20DE_Source;
    PKDT20DE_DyanmicSourceBuffer = ^TKDT20DE_DyanmicSourceBuffer;

    TKDT20DE_DyanmicStoreBuffer = array of TKDT20DE_Source;
    PKDT20DE_DyanmicStoreBuffer = ^TKDT20DE_DyanmicStoreBuffer;

    PKDT20DE_Node = ^TKDT20DE_Node;

    TKDT20DE_Node = record
      Parent, Right, Left: PKDT20DE_Node;
      vec: PKDT20DE_Source;
    end;

    TKDT20DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT20DE_Source; const Data: Pointer);
    TKDT20DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT20DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT20DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT20DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT20DE_DyanmicStoreBuffer;
    KDBuff: TKDT20DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT20DE_Node;
    TestBuff: TKDT20DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT20DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT20DE_Node;
    function GetData(const Index: NativeInt): PKDT20DE_Source;
  public
    RootNode: PKDT20DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT20DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT20DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT20DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT20DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT20DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT20DE_Node; overload;
    function Search(const buff: TKDT20DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT20DE_Node; overload;
    function Search(const buff: TKDT20DE_Vec; var SearchedDistanceMin: Double): PKDT20DE_Node; overload;
    function Search(const buff: TKDT20DE_Vec): PKDT20DE_Node; overload;
    function SearchToken(const buff: TKDT20DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT20DE_DynamicVecBuffer; var OutBuff: TKDT20DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT20DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT20DE_Node);
    procedure PrintBuffer;

    class function KDT20DEVec(const s: SystemString): TKDT20DE_Vec; overload;
    class function KDT20DEVec(const v: TKDT20DE_Vec): SystemString; overload;
    class function KDT20DEPow(const v: TKDT20DE_VecType): Double;
    class function KDT20DEDistance(const v1, v2: TKDT20DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT20DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT21DE = class(TCoreClassObject)
  public type
    // code split
    TKDT21DE_Vec = array [0 .. KDT21DE_Axis - 1] of TKDT21DE_VecType;
    PKDT21DE_Vec = ^TKDT21DE_Vec;

    TKDT21DE_DynamicVecBuffer = array of TKDT21DE_Vec;
    PKDT21DE_DynamicVecBuffer = ^TKDT21DE_DynamicVecBuffer;

    TKDT21DE_Source = record
      buff: TKDT21DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT21DE_Source = ^TKDT21DE_Source;
    TKDT21DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT21DE_Source) - 1] of PKDT21DE_Source;
    PKDT21DE_SourceBuffer = ^TKDT21DE_SourceBuffer;

    TKDT21DE_DyanmicSourceBuffer = array of PKDT21DE_Source;
    PKDT21DE_DyanmicSourceBuffer = ^TKDT21DE_DyanmicSourceBuffer;

    TKDT21DE_DyanmicStoreBuffer = array of TKDT21DE_Source;
    PKDT21DE_DyanmicStoreBuffer = ^TKDT21DE_DyanmicStoreBuffer;

    PKDT21DE_Node = ^TKDT21DE_Node;

    TKDT21DE_Node = record
      Parent, Right, Left: PKDT21DE_Node;
      vec: PKDT21DE_Source;
    end;

    TKDT21DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT21DE_Source; const Data: Pointer);
    TKDT21DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT21DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT21DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT21DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT21DE_DyanmicStoreBuffer;
    KDBuff: TKDT21DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT21DE_Node;
    TestBuff: TKDT21DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT21DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT21DE_Node;
    function GetData(const Index: NativeInt): PKDT21DE_Source;
  public
    RootNode: PKDT21DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT21DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT21DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT21DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT21DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT21DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT21DE_Node; overload;
    function Search(const buff: TKDT21DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT21DE_Node; overload;
    function Search(const buff: TKDT21DE_Vec; var SearchedDistanceMin: Double): PKDT21DE_Node; overload;
    function Search(const buff: TKDT21DE_Vec): PKDT21DE_Node; overload;
    function SearchToken(const buff: TKDT21DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT21DE_DynamicVecBuffer; var OutBuff: TKDT21DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT21DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT21DE_Node);
    procedure PrintBuffer;

    class function KDT21DEVec(const s: SystemString): TKDT21DE_Vec; overload;
    class function KDT21DEVec(const v: TKDT21DE_Vec): SystemString; overload;
    class function KDT21DEPow(const v: TKDT21DE_VecType): Double;
    class function KDT21DEDistance(const v1, v2: TKDT21DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT21DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT22DE = class(TCoreClassObject)
  public type
    // code split
    TKDT22DE_Vec = array [0 .. KDT22DE_Axis - 1] of TKDT22DE_VecType;
    PKDT22DE_Vec = ^TKDT22DE_Vec;

    TKDT22DE_DynamicVecBuffer = array of TKDT22DE_Vec;
    PKDT22DE_DynamicVecBuffer = ^TKDT22DE_DynamicVecBuffer;

    TKDT22DE_Source = record
      buff: TKDT22DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT22DE_Source = ^TKDT22DE_Source;
    TKDT22DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT22DE_Source) - 1] of PKDT22DE_Source;
    PKDT22DE_SourceBuffer = ^TKDT22DE_SourceBuffer;

    TKDT22DE_DyanmicSourceBuffer = array of PKDT22DE_Source;
    PKDT22DE_DyanmicSourceBuffer = ^TKDT22DE_DyanmicSourceBuffer;

    TKDT22DE_DyanmicStoreBuffer = array of TKDT22DE_Source;
    PKDT22DE_DyanmicStoreBuffer = ^TKDT22DE_DyanmicStoreBuffer;

    PKDT22DE_Node = ^TKDT22DE_Node;

    TKDT22DE_Node = record
      Parent, Right, Left: PKDT22DE_Node;
      vec: PKDT22DE_Source;
    end;

    TKDT22DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT22DE_Source; const Data: Pointer);
    TKDT22DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT22DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT22DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT22DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT22DE_DyanmicStoreBuffer;
    KDBuff: TKDT22DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT22DE_Node;
    TestBuff: TKDT22DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT22DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT22DE_Node;
    function GetData(const Index: NativeInt): PKDT22DE_Source;
  public
    RootNode: PKDT22DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT22DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT22DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT22DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT22DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT22DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT22DE_Node; overload;
    function Search(const buff: TKDT22DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT22DE_Node; overload;
    function Search(const buff: TKDT22DE_Vec; var SearchedDistanceMin: Double): PKDT22DE_Node; overload;
    function Search(const buff: TKDT22DE_Vec): PKDT22DE_Node; overload;
    function SearchToken(const buff: TKDT22DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT22DE_DynamicVecBuffer; var OutBuff: TKDT22DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT22DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT22DE_Node);
    procedure PrintBuffer;

    class function KDT22DEVec(const s: SystemString): TKDT22DE_Vec; overload;
    class function KDT22DEVec(const v: TKDT22DE_Vec): SystemString; overload;
    class function KDT22DEPow(const v: TKDT22DE_VecType): Double;
    class function KDT22DEDistance(const v1, v2: TKDT22DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT22DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT23DE = class(TCoreClassObject)
  public type
    // code split
    TKDT23DE_Vec = array [0 .. KDT23DE_Axis - 1] of TKDT23DE_VecType;
    PKDT23DE_Vec = ^TKDT23DE_Vec;

    TKDT23DE_DynamicVecBuffer = array of TKDT23DE_Vec;
    PKDT23DE_DynamicVecBuffer = ^TKDT23DE_DynamicVecBuffer;

    TKDT23DE_Source = record
      buff: TKDT23DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT23DE_Source = ^TKDT23DE_Source;
    TKDT23DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT23DE_Source) - 1] of PKDT23DE_Source;
    PKDT23DE_SourceBuffer = ^TKDT23DE_SourceBuffer;

    TKDT23DE_DyanmicSourceBuffer = array of PKDT23DE_Source;
    PKDT23DE_DyanmicSourceBuffer = ^TKDT23DE_DyanmicSourceBuffer;

    TKDT23DE_DyanmicStoreBuffer = array of TKDT23DE_Source;
    PKDT23DE_DyanmicStoreBuffer = ^TKDT23DE_DyanmicStoreBuffer;

    PKDT23DE_Node = ^TKDT23DE_Node;

    TKDT23DE_Node = record
      Parent, Right, Left: PKDT23DE_Node;
      vec: PKDT23DE_Source;
    end;

    TKDT23DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT23DE_Source; const Data: Pointer);
    TKDT23DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT23DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT23DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT23DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT23DE_DyanmicStoreBuffer;
    KDBuff: TKDT23DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT23DE_Node;
    TestBuff: TKDT23DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT23DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT23DE_Node;
    function GetData(const Index: NativeInt): PKDT23DE_Source;
  public
    RootNode: PKDT23DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT23DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT23DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT23DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT23DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT23DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT23DE_Node; overload;
    function Search(const buff: TKDT23DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT23DE_Node; overload;
    function Search(const buff: TKDT23DE_Vec; var SearchedDistanceMin: Double): PKDT23DE_Node; overload;
    function Search(const buff: TKDT23DE_Vec): PKDT23DE_Node; overload;
    function SearchToken(const buff: TKDT23DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT23DE_DynamicVecBuffer; var OutBuff: TKDT23DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT23DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT23DE_Node);
    procedure PrintBuffer;

    class function KDT23DEVec(const s: SystemString): TKDT23DE_Vec; overload;
    class function KDT23DEVec(const v: TKDT23DE_Vec): SystemString; overload;
    class function KDT23DEPow(const v: TKDT23DE_VecType): Double;
    class function KDT23DEDistance(const v1, v2: TKDT23DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT23DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT24DE = class(TCoreClassObject)
  public type
    // code split
    TKDT24DE_Vec = array [0 .. KDT24DE_Axis - 1] of TKDT24DE_VecType;
    PKDT24DE_Vec = ^TKDT24DE_Vec;

    TKDT24DE_DynamicVecBuffer = array of TKDT24DE_Vec;
    PKDT24DE_DynamicVecBuffer = ^TKDT24DE_DynamicVecBuffer;

    TKDT24DE_Source = record
      buff: TKDT24DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT24DE_Source = ^TKDT24DE_Source;
    TKDT24DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT24DE_Source) - 1] of PKDT24DE_Source;
    PKDT24DE_SourceBuffer = ^TKDT24DE_SourceBuffer;

    TKDT24DE_DyanmicSourceBuffer = array of PKDT24DE_Source;
    PKDT24DE_DyanmicSourceBuffer = ^TKDT24DE_DyanmicSourceBuffer;

    TKDT24DE_DyanmicStoreBuffer = array of TKDT24DE_Source;
    PKDT24DE_DyanmicStoreBuffer = ^TKDT24DE_DyanmicStoreBuffer;

    PKDT24DE_Node = ^TKDT24DE_Node;

    TKDT24DE_Node = record
      Parent, Right, Left: PKDT24DE_Node;
      vec: PKDT24DE_Source;
    end;

    TKDT24DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT24DE_Source; const Data: Pointer);
    TKDT24DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT24DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT24DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT24DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT24DE_DyanmicStoreBuffer;
    KDBuff: TKDT24DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT24DE_Node;
    TestBuff: TKDT24DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT24DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT24DE_Node;
    function GetData(const Index: NativeInt): PKDT24DE_Source;
  public
    RootNode: PKDT24DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT24DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT24DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT24DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT24DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT24DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT24DE_Node; overload;
    function Search(const buff: TKDT24DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT24DE_Node; overload;
    function Search(const buff: TKDT24DE_Vec; var SearchedDistanceMin: Double): PKDT24DE_Node; overload;
    function Search(const buff: TKDT24DE_Vec): PKDT24DE_Node; overload;
    function SearchToken(const buff: TKDT24DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT24DE_DynamicVecBuffer; var OutBuff: TKDT24DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT24DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT24DE_Node);
    procedure PrintBuffer;

    class function KDT24DEVec(const s: SystemString): TKDT24DE_Vec; overload;
    class function KDT24DEVec(const v: TKDT24DE_Vec): SystemString; overload;
    class function KDT24DEPow(const v: TKDT24DE_VecType): Double;
    class function KDT24DEDistance(const v1, v2: TKDT24DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT24DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT256DE = class(TCoreClassObject)
  public type
    // code split
    TKDT256DE_Vec = array [0 .. KDT256DE_Axis - 1] of TKDT256DE_VecType;
    PKDT256DE_Vec = ^TKDT256DE_Vec;

    TKDT256DE_DynamicVecBuffer = array of TKDT256DE_Vec;
    PKDT256DE_DynamicVecBuffer = ^TKDT256DE_DynamicVecBuffer;

    TKDT256DE_Source = record
      buff: TKDT256DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT256DE_Source = ^TKDT256DE_Source;
    TKDT256DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT256DE_Source) - 1] of PKDT256DE_Source;
    PKDT256DE_SourceBuffer = ^TKDT256DE_SourceBuffer;

    TKDT256DE_DyanmicSourceBuffer = array of PKDT256DE_Source;
    PKDT256DE_DyanmicSourceBuffer = ^TKDT256DE_DyanmicSourceBuffer;

    TKDT256DE_DyanmicStoreBuffer = array of TKDT256DE_Source;
    PKDT256DE_DyanmicStoreBuffer = ^TKDT256DE_DyanmicStoreBuffer;

    PKDT256DE_Node = ^TKDT256DE_Node;

    TKDT256DE_Node = record
      Parent, Right, Left: PKDT256DE_Node;
      vec: PKDT256DE_Source;
    end;

    TKDT256DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT256DE_Source; const Data: Pointer);
    TKDT256DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT256DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT256DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT256DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT256DE_DyanmicStoreBuffer;
    KDBuff: TKDT256DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT256DE_Node;
    TestBuff: TKDT256DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT256DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT256DE_Node;
    function GetData(const Index: NativeInt): PKDT256DE_Source;
  public
    RootNode: PKDT256DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT256DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT256DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT256DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT256DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT256DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT256DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT256DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT256DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT256DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT256DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT256DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT256DE_Node; overload;
    function Search(const buff: TKDT256DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT256DE_Node; overload;
    function Search(const buff: TKDT256DE_Vec; var SearchedDistanceMin: Double): PKDT256DE_Node; overload;
    function Search(const buff: TKDT256DE_Vec): PKDT256DE_Node; overload;
    function SearchToken(const buff: TKDT256DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT256DE_DynamicVecBuffer; var OutBuff: TKDT256DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT256DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT256DE_Node);
    procedure PrintBuffer;

    class function KDT256DEVec(const s: SystemString): TKDT256DE_Vec; overload;
    class function KDT256DEVec(const v: TKDT256DE_Vec): SystemString; overload;
    class function KDT256DEPow(const v: TKDT256DE_VecType): Double;
    class function KDT256DEDistance(const v1, v2: TKDT256DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT256DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT512DE = class(TCoreClassObject)
  public type
    // code split
    TKDT512DE_Vec = array [0 .. KDT512DE_Axis - 1] of TKDT512DE_VecType;
    PKDT512DE_Vec = ^TKDT512DE_Vec;

    TKDT512DE_DynamicVecBuffer = array of TKDT512DE_Vec;
    PKDT512DE_DynamicVecBuffer = ^TKDT512DE_DynamicVecBuffer;

    TKDT512DE_Source = record
      buff: TKDT512DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT512DE_Source = ^TKDT512DE_Source;
    TKDT512DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT512DE_Source) - 1] of PKDT512DE_Source;
    PKDT512DE_SourceBuffer = ^TKDT512DE_SourceBuffer;

    TKDT512DE_DyanmicSourceBuffer = array of PKDT512DE_Source;
    PKDT512DE_DyanmicSourceBuffer = ^TKDT512DE_DyanmicSourceBuffer;

    TKDT512DE_DyanmicStoreBuffer = array of TKDT512DE_Source;
    PKDT512DE_DyanmicStoreBuffer = ^TKDT512DE_DyanmicStoreBuffer;

    PKDT512DE_Node = ^TKDT512DE_Node;

    TKDT512DE_Node = record
      Parent, Right, Left: PKDT512DE_Node;
      vec: PKDT512DE_Source;
    end;

    TKDT512DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT512DE_Source; const Data: Pointer);
    TKDT512DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT512DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT512DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT512DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT512DE_DyanmicStoreBuffer;
    KDBuff: TKDT512DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT512DE_Node;
    TestBuff: TKDT512DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT512DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT512DE_Node;
    function GetData(const Index: NativeInt): PKDT512DE_Source;
  public
    RootNode: PKDT512DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT512DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT512DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT512DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT512DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT512DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT512DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT512DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT512DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT512DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT512DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT512DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT512DE_Node; overload;
    function Search(const buff: TKDT512DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT512DE_Node; overload;
    function Search(const buff: TKDT512DE_Vec; var SearchedDistanceMin: Double): PKDT512DE_Node; overload;
    function Search(const buff: TKDT512DE_Vec): PKDT512DE_Node; overload;
    function SearchToken(const buff: TKDT512DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT512DE_DynamicVecBuffer; var OutBuff: TKDT512DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT512DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT512DE_Node);
    procedure PrintBuffer;

    class function KDT512DEVec(const s: SystemString): TKDT512DE_Vec; overload;
    class function KDT512DEVec(const v: TKDT512DE_Vec): SystemString; overload;
    class function KDT512DEPow(const v: TKDT512DE_VecType): Double;
    class function KDT512DEDistance(const v1, v2: TKDT512DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT512DE_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT1024DE = class(TCoreClassObject)
  public type
    // code split
    TKDT1024DE_Vec = array [0 .. KDT1024DE_Axis - 1] of TKDT1024DE_VecType;
    PKDT1024DE_Vec = ^TKDT1024DE_Vec;

    TKDT1024DE_DynamicVecBuffer = array of TKDT1024DE_Vec;
    PKDT1024DE_DynamicVecBuffer = ^TKDT1024DE_DynamicVecBuffer;

    TKDT1024DE_Source = record
      buff: TKDT1024DE_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT1024DE_Source = ^TKDT1024DE_Source;
    TKDT1024DE_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT1024DE_Source) - 1] of PKDT1024DE_Source;
    PKDT1024DE_SourceBuffer = ^TKDT1024DE_SourceBuffer;

    TKDT1024DE_DyanmicSourceBuffer = array of PKDT1024DE_Source;
    PKDT1024DE_DyanmicSourceBuffer = ^TKDT1024DE_DyanmicSourceBuffer;

    TKDT1024DE_DyanmicStoreBuffer = array of TKDT1024DE_Source;
    PKDT1024DE_DyanmicStoreBuffer = ^TKDT1024DE_DyanmicStoreBuffer;

    PKDT1024DE_Node = ^TKDT1024DE_Node;

    TKDT1024DE_Node = record
      Parent, Right, Left: PKDT1024DE_Node;
      vec: PKDT1024DE_Source;
    end;

    TKDT1024DE_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT1024DE_Source; const Data: Pointer);
    TKDT1024DE_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT1024DE_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT1024DE_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT1024DE_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT1024DE_DyanmicStoreBuffer;
    KDBuff: TKDT1024DE_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT1024DE_Node;
    TestBuff: TKDT1024DE_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT1024DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT1024DE_Node;
    function GetData(const Index: NativeInt): PKDT1024DE_Source;
  public
    RootNode: PKDT1024DE_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT1024DE_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT1024DE_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1024DE_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1024DE_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1024DE_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT1024DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT1024DE_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1024DE_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1024DE_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1024DE_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT1024DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT1024DE_Node; overload;
    function Search(const buff: TKDT1024DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT1024DE_Node; overload;
    function Search(const buff: TKDT1024DE_Vec; var SearchedDistanceMin: Double): PKDT1024DE_Node; overload;
    function Search(const buff: TKDT1024DE_Vec): PKDT1024DE_Node; overload;
    function SearchToken(const buff: TKDT1024DE_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT1024DE_DynamicVecBuffer; var OutBuff: TKDT1024DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT1024DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT1024DE_Node);
    procedure PrintBuffer;

    class function KDT1024DEVec(const s: SystemString): TKDT1024DE_Vec; overload;
    class function KDT1024DEVec(const v: TKDT1024DE_Vec): SystemString; overload;
    class function KDT1024DEPow(const v: TKDT1024DE_VecType): Double;
    class function KDT1024DEDistance(const v1, v2: TKDT1024DE_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT1024DE_Source; const Data: Pointer);
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
  SaveToken = $22;



function TKDT1DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT1DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT1DE_Node;
  function SortCompare(const p1, p2: PKDT1DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT1DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT1DE_Source;
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
  kdBuffPtr: PKDT1DE_SourceBuffer;
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
      axis := Depth mod KDT1DE_Axis;
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

function TKDT1DE.GetData(const Index: NativeInt): PKDT1DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT1DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT1DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT1DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT1DE_Node(KDNodes[i]));
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

function TKDT1DE.StoreBuffPtr: PKDT1DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT1DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT1DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT1DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT1DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT1DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT1DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT1DE.BuildKDTreeWithCluster(const inBuff: TKDT1DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT1DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT1DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT1DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT1DE.BuildKDTreeWithCluster(const inBuff: TKDT1DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT1DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DE_BuildCall);
var
  TempStoreBuff: TKDT1DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT1DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT1DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT1DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT1DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1DE_Axis - 1 do
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

procedure TKDT1DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DE_BuildMethod);
var
  TempStoreBuff: TKDT1DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT1DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT1DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT1DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT1DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1DE_Axis - 1 do
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


procedure TKDT1DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DE_BuildProc);
var
  TempStoreBuff: TKDT1DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT1DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT1DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT1DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT1DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1DE_Axis - 1 do
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


function TKDT1DE.Search(const buff: TKDT1DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT1DE_Node;

var
  NearestNeighbour: PKDT1DE_Node;

  function FindParentNode(const buffPtr: PKDT1DE_Vec; NodePtr: PKDT1DE_Node): PKDT1DE_Node;
  var
    Next: PKDT1DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT1DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT1DE_Node; const buffPtr: PKDT1DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT1DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT1DE_Axis;
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

  function SortCompare(const buffPtr: PKDT1DE_Vec; const p1, p2: PKDT1DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT1DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT1DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT1DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT1DE_Node;
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
  Parent: PKDT1DE_Node;
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

  SearchedDistanceMin := KDT1DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT1DE_Node(NearestNodes[0]);
    end;
end;

function TKDT1DE.Search(const buff: TKDT1DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT1DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT1DE.Search(const buff: TKDT1DE_Vec; var SearchedDistanceMin: Double): PKDT1DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT1DE.Search(const buff: TKDT1DE_Vec): PKDT1DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT1DE.SearchToken(const buff: TKDT1DE_Vec): TPascalString;
var
  p: PKDT1DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT1DE.Search(const inBuff: TKDT1DE_DynamicVecBuffer; var OutBuff: TKDT1DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT1DE_DynamicVecBuffer;
  outBuffPtr: PKDT1DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT1DE_Node;
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
        p: PKDT1DE_Node;
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
  p: PKDT1DE_Node;
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


procedure TKDT1DE.Search(const inBuff: TKDT1DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT1DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT1DE_Node;
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
        p: PKDT1DE_Node;
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
  p: PKDT1DE_Node;
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


procedure TKDT1DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT1DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT1DE_Vec));
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

procedure TKDT1DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT1DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT1DE_Vec)) <> SizeOf(TKDT1DE_Vec) then
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

procedure TKDT1DE.SaveToFile(FileName: SystemString);
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

procedure TKDT1DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT1DE.PrintNodeTree(const NodePtr: PKDT1DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT1DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT1DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT1DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT1DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT1DE.KDT1DEVec(const s: SystemString): TKDT1DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT1DE_Axis - 1 do
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
            if j >= KDT1DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT1DE.KDT1DEVec(const v: TKDT1DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT1DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT1DE.KDT1DEPow(const v: TKDT1DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT1DE.KDT1DEDistance(const v1, v2: TKDT1DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT1DE_Axis - 1 do
      Result := Result + KDT1DEPow(v2[i] - v1[i]);
end;

procedure TKDT1DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT1DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT1DE.Test;
var
  TKDT1DE_Test: TKDT1DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT1DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT1DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT1DE_Test := TKDT1DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT1DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT1DE_Test.TestBuff) - 1 do
    for j := 0 to KDT1DE_Axis - 1 do
        TKDT1DE_Test.TestBuff[i][j] := i * KDT1DE_Axis + j;

{$IFDEF FPC}
  TKDT1DE_Test.BuildKDTreeM(length(TKDT1DE_Test.TestBuff), nil, @TKDT1DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT1DE_Test.BuildKDTreeM(length(TKDT1DE_Test.TestBuff), nil, TKDT1DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT1DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT1DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT1DE_Test.TestBuff) - 1 do
    begin
      p := TKDT1DE_Test.Search(TKDT1DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT1DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT1DE_Test.TestBuff));
      TKDT1DE_Test.Search(TKDT1DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT1DEDistance(TKDT1DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT1DE_Test.Clear;
      { kMean test }
      TKDT1DE_Test.BuildKDTreeWithCluster(TKDT1DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT1DE_Test.Search(TKDT1DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT1DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT1DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT1DE_Test);
end;


function TKDT2DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT2DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT2DE_Node;
  function SortCompare(const p1, p2: PKDT2DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT2DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT2DE_Source;
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
  kdBuffPtr: PKDT2DE_SourceBuffer;
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
      axis := Depth mod KDT2DE_Axis;
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

function TKDT2DE.GetData(const Index: NativeInt): PKDT2DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT2DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT2DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT2DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT2DE_Node(KDNodes[i]));
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

function TKDT2DE.StoreBuffPtr: PKDT2DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT2DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT2DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT2DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT2DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT2DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT2DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT2DE.BuildKDTreeWithCluster(const inBuff: TKDT2DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT2DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT2DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT2DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT2DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT2DE.BuildKDTreeWithCluster(const inBuff: TKDT2DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT2DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DE_BuildCall);
var
  TempStoreBuff: TKDT2DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT2DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT2DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT2DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT2DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT2DE_Axis - 1 do
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

procedure TKDT2DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DE_BuildMethod);
var
  TempStoreBuff: TKDT2DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT2DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT2DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT2DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT2DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT2DE_Axis - 1 do
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


procedure TKDT2DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DE_BuildProc);
var
  TempStoreBuff: TKDT2DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT2DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT2DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT2DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT2DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT2DE_Axis - 1 do
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


function TKDT2DE.Search(const buff: TKDT2DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT2DE_Node;

var
  NearestNeighbour: PKDT2DE_Node;

  function FindParentNode(const buffPtr: PKDT2DE_Vec; NodePtr: PKDT2DE_Node): PKDT2DE_Node;
  var
    Next: PKDT2DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT2DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT2DE_Node; const buffPtr: PKDT2DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT2DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT2DE_Axis;
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

  function SortCompare(const buffPtr: PKDT2DE_Vec; const p1, p2: PKDT2DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT2DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT2DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT2DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT2DE_Node;
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
  Parent: PKDT2DE_Node;
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

  SearchedDistanceMin := KDT2DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT2DE_Node(NearestNodes[0]);
    end;
end;

function TKDT2DE.Search(const buff: TKDT2DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT2DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT2DE.Search(const buff: TKDT2DE_Vec; var SearchedDistanceMin: Double): PKDT2DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT2DE.Search(const buff: TKDT2DE_Vec): PKDT2DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT2DE.SearchToken(const buff: TKDT2DE_Vec): TPascalString;
var
  p: PKDT2DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT2DE.Search(const inBuff: TKDT2DE_DynamicVecBuffer; var OutBuff: TKDT2DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT2DE_DynamicVecBuffer;
  outBuffPtr: PKDT2DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT2DE_Node;
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
        p: PKDT2DE_Node;
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
  p: PKDT2DE_Node;
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


procedure TKDT2DE.Search(const inBuff: TKDT2DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT2DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT2DE_Node;
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
        p: PKDT2DE_Node;
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
  p: PKDT2DE_Node;
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


procedure TKDT2DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT2DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT2DE_Vec));
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

procedure TKDT2DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT2DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT2DE_Vec)) <> SizeOf(TKDT2DE_Vec) then
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

procedure TKDT2DE.SaveToFile(FileName: SystemString);
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

procedure TKDT2DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT2DE.PrintNodeTree(const NodePtr: PKDT2DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT2DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT2DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT2DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT2DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT2DE.KDT2DEVec(const s: SystemString): TKDT2DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT2DE_Axis - 1 do
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
            if j >= KDT2DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT2DE.KDT2DEVec(const v: TKDT2DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT2DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT2DE.KDT2DEPow(const v: TKDT2DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT2DE.KDT2DEDistance(const v1, v2: TKDT2DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT2DE_Axis - 1 do
      Result := Result + KDT2DEPow(v2[i] - v1[i]);
end;

procedure TKDT2DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT2DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT2DE.Test;
var
  TKDT2DE_Test: TKDT2DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT2DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT2DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT2DE_Test := TKDT2DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT2DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT2DE_Test.TestBuff) - 1 do
    for j := 0 to KDT2DE_Axis - 1 do
        TKDT2DE_Test.TestBuff[i][j] := i * KDT2DE_Axis + j;

{$IFDEF FPC}
  TKDT2DE_Test.BuildKDTreeM(length(TKDT2DE_Test.TestBuff), nil, @TKDT2DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT2DE_Test.BuildKDTreeM(length(TKDT2DE_Test.TestBuff), nil, TKDT2DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT2DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT2DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT2DE_Test.TestBuff) - 1 do
    begin
      p := TKDT2DE_Test.Search(TKDT2DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT2DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT2DE_Test.TestBuff));
      TKDT2DE_Test.Search(TKDT2DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT2DEDistance(TKDT2DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT2DE_Test.Clear;
      { kMean test }
      TKDT2DE_Test.BuildKDTreeWithCluster(TKDT2DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT2DE_Test.Search(TKDT2DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT2DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT2DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT2DE_Test);
end;


function TKDT3DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT3DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT3DE_Node;
  function SortCompare(const p1, p2: PKDT3DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT3DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT3DE_Source;
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
  kdBuffPtr: PKDT3DE_SourceBuffer;
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
      axis := Depth mod KDT3DE_Axis;
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

function TKDT3DE.GetData(const Index: NativeInt): PKDT3DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT3DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT3DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT3DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT3DE_Node(KDNodes[i]));
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

function TKDT3DE.StoreBuffPtr: PKDT3DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT3DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT3DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT3DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT3DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT3DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT3DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT3DE.BuildKDTreeWithCluster(const inBuff: TKDT3DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT3DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT3DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT3DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT3DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT3DE.BuildKDTreeWithCluster(const inBuff: TKDT3DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT3DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DE_BuildCall);
var
  TempStoreBuff: TKDT3DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT3DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT3DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT3DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT3DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT3DE_Axis - 1 do
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

procedure TKDT3DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DE_BuildMethod);
var
  TempStoreBuff: TKDT3DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT3DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT3DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT3DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT3DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT3DE_Axis - 1 do
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


procedure TKDT3DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DE_BuildProc);
var
  TempStoreBuff: TKDT3DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT3DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT3DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT3DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT3DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT3DE_Axis - 1 do
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


function TKDT3DE.Search(const buff: TKDT3DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT3DE_Node;

var
  NearestNeighbour: PKDT3DE_Node;

  function FindParentNode(const buffPtr: PKDT3DE_Vec; NodePtr: PKDT3DE_Node): PKDT3DE_Node;
  var
    Next: PKDT3DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT3DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT3DE_Node; const buffPtr: PKDT3DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT3DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT3DE_Axis;
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

  function SortCompare(const buffPtr: PKDT3DE_Vec; const p1, p2: PKDT3DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT3DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT3DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT3DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT3DE_Node;
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
  Parent: PKDT3DE_Node;
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

  SearchedDistanceMin := KDT3DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT3DE_Node(NearestNodes[0]);
    end;
end;

function TKDT3DE.Search(const buff: TKDT3DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT3DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT3DE.Search(const buff: TKDT3DE_Vec; var SearchedDistanceMin: Double): PKDT3DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT3DE.Search(const buff: TKDT3DE_Vec): PKDT3DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT3DE.SearchToken(const buff: TKDT3DE_Vec): TPascalString;
var
  p: PKDT3DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT3DE.Search(const inBuff: TKDT3DE_DynamicVecBuffer; var OutBuff: TKDT3DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT3DE_DynamicVecBuffer;
  outBuffPtr: PKDT3DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT3DE_Node;
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
        p: PKDT3DE_Node;
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
  p: PKDT3DE_Node;
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


procedure TKDT3DE.Search(const inBuff: TKDT3DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT3DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT3DE_Node;
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
        p: PKDT3DE_Node;
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
  p: PKDT3DE_Node;
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


procedure TKDT3DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT3DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT3DE_Vec));
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

procedure TKDT3DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT3DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT3DE_Vec)) <> SizeOf(TKDT3DE_Vec) then
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

procedure TKDT3DE.SaveToFile(FileName: SystemString);
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

procedure TKDT3DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT3DE.PrintNodeTree(const NodePtr: PKDT3DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT3DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT3DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT3DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT3DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT3DE.KDT3DEVec(const s: SystemString): TKDT3DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT3DE_Axis - 1 do
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
            if j >= KDT3DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT3DE.KDT3DEVec(const v: TKDT3DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT3DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT3DE.KDT3DEPow(const v: TKDT3DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT3DE.KDT3DEDistance(const v1, v2: TKDT3DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT3DE_Axis - 1 do
      Result := Result + KDT3DEPow(v2[i] - v1[i]);
end;

procedure TKDT3DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT3DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT3DE.Test;
var
  TKDT3DE_Test: TKDT3DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT3DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT3DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT3DE_Test := TKDT3DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT3DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT3DE_Test.TestBuff) - 1 do
    for j := 0 to KDT3DE_Axis - 1 do
        TKDT3DE_Test.TestBuff[i][j] := i * KDT3DE_Axis + j;

{$IFDEF FPC}
  TKDT3DE_Test.BuildKDTreeM(length(TKDT3DE_Test.TestBuff), nil, @TKDT3DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT3DE_Test.BuildKDTreeM(length(TKDT3DE_Test.TestBuff), nil, TKDT3DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT3DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT3DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT3DE_Test.TestBuff) - 1 do
    begin
      p := TKDT3DE_Test.Search(TKDT3DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT3DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT3DE_Test.TestBuff));
      TKDT3DE_Test.Search(TKDT3DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT3DEDistance(TKDT3DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT3DE_Test.Clear;
      { kMean test }
      TKDT3DE_Test.BuildKDTreeWithCluster(TKDT3DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT3DE_Test.Search(TKDT3DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT3DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT3DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT3DE_Test);
end;


function TKDT4DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT4DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT4DE_Node;
  function SortCompare(const p1, p2: PKDT4DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT4DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT4DE_Source;
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
  kdBuffPtr: PKDT4DE_SourceBuffer;
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
      axis := Depth mod KDT4DE_Axis;
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

function TKDT4DE.GetData(const Index: NativeInt): PKDT4DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT4DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT4DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT4DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT4DE_Node(KDNodes[i]));
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

function TKDT4DE.StoreBuffPtr: PKDT4DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT4DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT4DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT4DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT4DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT4DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT4DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT4DE.BuildKDTreeWithCluster(const inBuff: TKDT4DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT4DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT4DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT4DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT4DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT4DE.BuildKDTreeWithCluster(const inBuff: TKDT4DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT4DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DE_BuildCall);
var
  TempStoreBuff: TKDT4DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT4DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT4DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT4DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT4DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT4DE_Axis - 1 do
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

procedure TKDT4DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DE_BuildMethod);
var
  TempStoreBuff: TKDT4DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT4DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT4DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT4DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT4DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT4DE_Axis - 1 do
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


procedure TKDT4DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DE_BuildProc);
var
  TempStoreBuff: TKDT4DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT4DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT4DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT4DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT4DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT4DE_Axis - 1 do
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


function TKDT4DE.Search(const buff: TKDT4DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT4DE_Node;

var
  NearestNeighbour: PKDT4DE_Node;

  function FindParentNode(const buffPtr: PKDT4DE_Vec; NodePtr: PKDT4DE_Node): PKDT4DE_Node;
  var
    Next: PKDT4DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT4DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT4DE_Node; const buffPtr: PKDT4DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT4DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT4DE_Axis;
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

  function SortCompare(const buffPtr: PKDT4DE_Vec; const p1, p2: PKDT4DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT4DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT4DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT4DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT4DE_Node;
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
  Parent: PKDT4DE_Node;
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

  SearchedDistanceMin := KDT4DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT4DE_Node(NearestNodes[0]);
    end;
end;

function TKDT4DE.Search(const buff: TKDT4DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT4DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT4DE.Search(const buff: TKDT4DE_Vec; var SearchedDistanceMin: Double): PKDT4DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT4DE.Search(const buff: TKDT4DE_Vec): PKDT4DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT4DE.SearchToken(const buff: TKDT4DE_Vec): TPascalString;
var
  p: PKDT4DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT4DE.Search(const inBuff: TKDT4DE_DynamicVecBuffer; var OutBuff: TKDT4DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT4DE_DynamicVecBuffer;
  outBuffPtr: PKDT4DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT4DE_Node;
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
        p: PKDT4DE_Node;
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
  p: PKDT4DE_Node;
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


procedure TKDT4DE.Search(const inBuff: TKDT4DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT4DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT4DE_Node;
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
        p: PKDT4DE_Node;
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
  p: PKDT4DE_Node;
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


procedure TKDT4DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT4DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT4DE_Vec));
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

procedure TKDT4DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT4DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT4DE_Vec)) <> SizeOf(TKDT4DE_Vec) then
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

procedure TKDT4DE.SaveToFile(FileName: SystemString);
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

procedure TKDT4DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT4DE.PrintNodeTree(const NodePtr: PKDT4DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT4DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT4DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT4DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT4DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT4DE.KDT4DEVec(const s: SystemString): TKDT4DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT4DE_Axis - 1 do
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
            if j >= KDT4DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT4DE.KDT4DEVec(const v: TKDT4DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT4DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT4DE.KDT4DEPow(const v: TKDT4DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT4DE.KDT4DEDistance(const v1, v2: TKDT4DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT4DE_Axis - 1 do
      Result := Result + KDT4DEPow(v2[i] - v1[i]);
end;

procedure TKDT4DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT4DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT4DE.Test;
var
  TKDT4DE_Test: TKDT4DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT4DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT4DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT4DE_Test := TKDT4DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT4DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT4DE_Test.TestBuff) - 1 do
    for j := 0 to KDT4DE_Axis - 1 do
        TKDT4DE_Test.TestBuff[i][j] := i * KDT4DE_Axis + j;

{$IFDEF FPC}
  TKDT4DE_Test.BuildKDTreeM(length(TKDT4DE_Test.TestBuff), nil, @TKDT4DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT4DE_Test.BuildKDTreeM(length(TKDT4DE_Test.TestBuff), nil, TKDT4DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT4DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT4DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT4DE_Test.TestBuff) - 1 do
    begin
      p := TKDT4DE_Test.Search(TKDT4DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT4DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT4DE_Test.TestBuff));
      TKDT4DE_Test.Search(TKDT4DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT4DEDistance(TKDT4DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT4DE_Test.Clear;
      { kMean test }
      TKDT4DE_Test.BuildKDTreeWithCluster(TKDT4DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT4DE_Test.Search(TKDT4DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT4DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT4DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT4DE_Test);
end;


function TKDT5DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT5DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT5DE_Node;
  function SortCompare(const p1, p2: PKDT5DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT5DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT5DE_Source;
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
  kdBuffPtr: PKDT5DE_SourceBuffer;
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
      axis := Depth mod KDT5DE_Axis;
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

function TKDT5DE.GetData(const Index: NativeInt): PKDT5DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT5DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT5DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT5DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT5DE_Node(KDNodes[i]));
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

function TKDT5DE.StoreBuffPtr: PKDT5DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT5DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT5DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT5DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT5DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT5DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT5DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT5DE.BuildKDTreeWithCluster(const inBuff: TKDT5DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT5DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT5DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT5DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT5DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT5DE.BuildKDTreeWithCluster(const inBuff: TKDT5DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT5DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DE_BuildCall);
var
  TempStoreBuff: TKDT5DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT5DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT5DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT5DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT5DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT5DE_Axis - 1 do
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

procedure TKDT5DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DE_BuildMethod);
var
  TempStoreBuff: TKDT5DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT5DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT5DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT5DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT5DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT5DE_Axis - 1 do
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


procedure TKDT5DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DE_BuildProc);
var
  TempStoreBuff: TKDT5DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT5DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT5DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT5DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT5DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT5DE_Axis - 1 do
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


function TKDT5DE.Search(const buff: TKDT5DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT5DE_Node;

var
  NearestNeighbour: PKDT5DE_Node;

  function FindParentNode(const buffPtr: PKDT5DE_Vec; NodePtr: PKDT5DE_Node): PKDT5DE_Node;
  var
    Next: PKDT5DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT5DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT5DE_Node; const buffPtr: PKDT5DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT5DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT5DE_Axis;
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

  function SortCompare(const buffPtr: PKDT5DE_Vec; const p1, p2: PKDT5DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT5DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT5DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT5DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT5DE_Node;
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
  Parent: PKDT5DE_Node;
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

  SearchedDistanceMin := KDT5DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT5DE_Node(NearestNodes[0]);
    end;
end;

function TKDT5DE.Search(const buff: TKDT5DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT5DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT5DE.Search(const buff: TKDT5DE_Vec; var SearchedDistanceMin: Double): PKDT5DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT5DE.Search(const buff: TKDT5DE_Vec): PKDT5DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT5DE.SearchToken(const buff: TKDT5DE_Vec): TPascalString;
var
  p: PKDT5DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT5DE.Search(const inBuff: TKDT5DE_DynamicVecBuffer; var OutBuff: TKDT5DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT5DE_DynamicVecBuffer;
  outBuffPtr: PKDT5DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT5DE_Node;
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
        p: PKDT5DE_Node;
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
  p: PKDT5DE_Node;
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


procedure TKDT5DE.Search(const inBuff: TKDT5DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT5DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT5DE_Node;
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
        p: PKDT5DE_Node;
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
  p: PKDT5DE_Node;
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


procedure TKDT5DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT5DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT5DE_Vec));
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

procedure TKDT5DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT5DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT5DE_Vec)) <> SizeOf(TKDT5DE_Vec) then
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

procedure TKDT5DE.SaveToFile(FileName: SystemString);
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

procedure TKDT5DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT5DE.PrintNodeTree(const NodePtr: PKDT5DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT5DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT5DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT5DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT5DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT5DE.KDT5DEVec(const s: SystemString): TKDT5DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT5DE_Axis - 1 do
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
            if j >= KDT5DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT5DE.KDT5DEVec(const v: TKDT5DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT5DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT5DE.KDT5DEPow(const v: TKDT5DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT5DE.KDT5DEDistance(const v1, v2: TKDT5DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT5DE_Axis - 1 do
      Result := Result + KDT5DEPow(v2[i] - v1[i]);
end;

procedure TKDT5DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT5DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT5DE.Test;
var
  TKDT5DE_Test: TKDT5DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT5DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT5DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT5DE_Test := TKDT5DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT5DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT5DE_Test.TestBuff) - 1 do
    for j := 0 to KDT5DE_Axis - 1 do
        TKDT5DE_Test.TestBuff[i][j] := i * KDT5DE_Axis + j;

{$IFDEF FPC}
  TKDT5DE_Test.BuildKDTreeM(length(TKDT5DE_Test.TestBuff), nil, @TKDT5DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT5DE_Test.BuildKDTreeM(length(TKDT5DE_Test.TestBuff), nil, TKDT5DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT5DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT5DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT5DE_Test.TestBuff) - 1 do
    begin
      p := TKDT5DE_Test.Search(TKDT5DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT5DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT5DE_Test.TestBuff));
      TKDT5DE_Test.Search(TKDT5DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT5DEDistance(TKDT5DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT5DE_Test.Clear;
      { kMean test }
      TKDT5DE_Test.BuildKDTreeWithCluster(TKDT5DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT5DE_Test.Search(TKDT5DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT5DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT5DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT5DE_Test);
end;


function TKDT6DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT6DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT6DE_Node;
  function SortCompare(const p1, p2: PKDT6DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT6DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT6DE_Source;
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
  kdBuffPtr: PKDT6DE_SourceBuffer;
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
      axis := Depth mod KDT6DE_Axis;
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

function TKDT6DE.GetData(const Index: NativeInt): PKDT6DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT6DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT6DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT6DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT6DE_Node(KDNodes[i]));
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

function TKDT6DE.StoreBuffPtr: PKDT6DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT6DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT6DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT6DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT6DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT6DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT6DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT6DE.BuildKDTreeWithCluster(const inBuff: TKDT6DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT6DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT6DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT6DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT6DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT6DE.BuildKDTreeWithCluster(const inBuff: TKDT6DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT6DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DE_BuildCall);
var
  TempStoreBuff: TKDT6DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT6DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT6DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT6DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT6DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT6DE_Axis - 1 do
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

procedure TKDT6DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DE_BuildMethod);
var
  TempStoreBuff: TKDT6DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT6DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT6DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT6DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT6DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT6DE_Axis - 1 do
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


procedure TKDT6DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DE_BuildProc);
var
  TempStoreBuff: TKDT6DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT6DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT6DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT6DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT6DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT6DE_Axis - 1 do
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


function TKDT6DE.Search(const buff: TKDT6DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT6DE_Node;

var
  NearestNeighbour: PKDT6DE_Node;

  function FindParentNode(const buffPtr: PKDT6DE_Vec; NodePtr: PKDT6DE_Node): PKDT6DE_Node;
  var
    Next: PKDT6DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT6DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT6DE_Node; const buffPtr: PKDT6DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT6DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT6DE_Axis;
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

  function SortCompare(const buffPtr: PKDT6DE_Vec; const p1, p2: PKDT6DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT6DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT6DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT6DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT6DE_Node;
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
  Parent: PKDT6DE_Node;
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

  SearchedDistanceMin := KDT6DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT6DE_Node(NearestNodes[0]);
    end;
end;

function TKDT6DE.Search(const buff: TKDT6DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT6DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT6DE.Search(const buff: TKDT6DE_Vec; var SearchedDistanceMin: Double): PKDT6DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT6DE.Search(const buff: TKDT6DE_Vec): PKDT6DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT6DE.SearchToken(const buff: TKDT6DE_Vec): TPascalString;
var
  p: PKDT6DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT6DE.Search(const inBuff: TKDT6DE_DynamicVecBuffer; var OutBuff: TKDT6DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT6DE_DynamicVecBuffer;
  outBuffPtr: PKDT6DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT6DE_Node;
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
        p: PKDT6DE_Node;
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
  p: PKDT6DE_Node;
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


procedure TKDT6DE.Search(const inBuff: TKDT6DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT6DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT6DE_Node;
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
        p: PKDT6DE_Node;
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
  p: PKDT6DE_Node;
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


procedure TKDT6DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT6DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT6DE_Vec));
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

procedure TKDT6DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT6DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT6DE_Vec)) <> SizeOf(TKDT6DE_Vec) then
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

procedure TKDT6DE.SaveToFile(FileName: SystemString);
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

procedure TKDT6DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT6DE.PrintNodeTree(const NodePtr: PKDT6DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT6DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT6DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT6DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT6DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT6DE.KDT6DEVec(const s: SystemString): TKDT6DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT6DE_Axis - 1 do
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
            if j >= KDT6DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT6DE.KDT6DEVec(const v: TKDT6DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT6DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT6DE.KDT6DEPow(const v: TKDT6DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT6DE.KDT6DEDistance(const v1, v2: TKDT6DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT6DE_Axis - 1 do
      Result := Result + KDT6DEPow(v2[i] - v1[i]);
end;

procedure TKDT6DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT6DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT6DE.Test;
var
  TKDT6DE_Test: TKDT6DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT6DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT6DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT6DE_Test := TKDT6DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT6DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT6DE_Test.TestBuff) - 1 do
    for j := 0 to KDT6DE_Axis - 1 do
        TKDT6DE_Test.TestBuff[i][j] := i * KDT6DE_Axis + j;

{$IFDEF FPC}
  TKDT6DE_Test.BuildKDTreeM(length(TKDT6DE_Test.TestBuff), nil, @TKDT6DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT6DE_Test.BuildKDTreeM(length(TKDT6DE_Test.TestBuff), nil, TKDT6DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT6DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT6DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT6DE_Test.TestBuff) - 1 do
    begin
      p := TKDT6DE_Test.Search(TKDT6DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT6DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT6DE_Test.TestBuff));
      TKDT6DE_Test.Search(TKDT6DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT6DEDistance(TKDT6DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT6DE_Test.Clear;
      { kMean test }
      TKDT6DE_Test.BuildKDTreeWithCluster(TKDT6DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT6DE_Test.Search(TKDT6DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT6DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT6DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT6DE_Test);
end;


function TKDT7DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT7DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT7DE_Node;
  function SortCompare(const p1, p2: PKDT7DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT7DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT7DE_Source;
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
  kdBuffPtr: PKDT7DE_SourceBuffer;
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
      axis := Depth mod KDT7DE_Axis;
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

function TKDT7DE.GetData(const Index: NativeInt): PKDT7DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT7DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT7DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT7DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT7DE_Node(KDNodes[i]));
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

function TKDT7DE.StoreBuffPtr: PKDT7DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT7DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT7DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT7DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT7DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT7DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT7DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT7DE.BuildKDTreeWithCluster(const inBuff: TKDT7DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT7DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT7DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT7DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT7DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT7DE.BuildKDTreeWithCluster(const inBuff: TKDT7DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT7DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DE_BuildCall);
var
  TempStoreBuff: TKDT7DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT7DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT7DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT7DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT7DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT7DE_Axis - 1 do
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

procedure TKDT7DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DE_BuildMethod);
var
  TempStoreBuff: TKDT7DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT7DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT7DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT7DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT7DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT7DE_Axis - 1 do
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


procedure TKDT7DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DE_BuildProc);
var
  TempStoreBuff: TKDT7DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT7DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT7DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT7DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT7DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT7DE_Axis - 1 do
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


function TKDT7DE.Search(const buff: TKDT7DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT7DE_Node;

var
  NearestNeighbour: PKDT7DE_Node;

  function FindParentNode(const buffPtr: PKDT7DE_Vec; NodePtr: PKDT7DE_Node): PKDT7DE_Node;
  var
    Next: PKDT7DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT7DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT7DE_Node; const buffPtr: PKDT7DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT7DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT7DE_Axis;
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

  function SortCompare(const buffPtr: PKDT7DE_Vec; const p1, p2: PKDT7DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT7DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT7DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT7DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT7DE_Node;
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
  Parent: PKDT7DE_Node;
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

  SearchedDistanceMin := KDT7DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT7DE_Node(NearestNodes[0]);
    end;
end;

function TKDT7DE.Search(const buff: TKDT7DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT7DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT7DE.Search(const buff: TKDT7DE_Vec; var SearchedDistanceMin: Double): PKDT7DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT7DE.Search(const buff: TKDT7DE_Vec): PKDT7DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT7DE.SearchToken(const buff: TKDT7DE_Vec): TPascalString;
var
  p: PKDT7DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT7DE.Search(const inBuff: TKDT7DE_DynamicVecBuffer; var OutBuff: TKDT7DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT7DE_DynamicVecBuffer;
  outBuffPtr: PKDT7DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT7DE_Node;
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
        p: PKDT7DE_Node;
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
  p: PKDT7DE_Node;
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


procedure TKDT7DE.Search(const inBuff: TKDT7DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT7DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT7DE_Node;
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
        p: PKDT7DE_Node;
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
  p: PKDT7DE_Node;
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


procedure TKDT7DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT7DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT7DE_Vec));
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

procedure TKDT7DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT7DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT7DE_Vec)) <> SizeOf(TKDT7DE_Vec) then
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

procedure TKDT7DE.SaveToFile(FileName: SystemString);
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

procedure TKDT7DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT7DE.PrintNodeTree(const NodePtr: PKDT7DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT7DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT7DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT7DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT7DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT7DE.KDT7DEVec(const s: SystemString): TKDT7DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT7DE_Axis - 1 do
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
            if j >= KDT7DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT7DE.KDT7DEVec(const v: TKDT7DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT7DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT7DE.KDT7DEPow(const v: TKDT7DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT7DE.KDT7DEDistance(const v1, v2: TKDT7DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT7DE_Axis - 1 do
      Result := Result + KDT7DEPow(v2[i] - v1[i]);
end;

procedure TKDT7DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT7DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT7DE.Test;
var
  TKDT7DE_Test: TKDT7DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT7DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT7DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT7DE_Test := TKDT7DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT7DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT7DE_Test.TestBuff) - 1 do
    for j := 0 to KDT7DE_Axis - 1 do
        TKDT7DE_Test.TestBuff[i][j] := i * KDT7DE_Axis + j;

{$IFDEF FPC}
  TKDT7DE_Test.BuildKDTreeM(length(TKDT7DE_Test.TestBuff), nil, @TKDT7DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT7DE_Test.BuildKDTreeM(length(TKDT7DE_Test.TestBuff), nil, TKDT7DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT7DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT7DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT7DE_Test.TestBuff) - 1 do
    begin
      p := TKDT7DE_Test.Search(TKDT7DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT7DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT7DE_Test.TestBuff));
      TKDT7DE_Test.Search(TKDT7DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT7DEDistance(TKDT7DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT7DE_Test.Clear;
      { kMean test }
      TKDT7DE_Test.BuildKDTreeWithCluster(TKDT7DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT7DE_Test.Search(TKDT7DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT7DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT7DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT7DE_Test);
end;


function TKDT8DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT8DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT8DE_Node;
  function SortCompare(const p1, p2: PKDT8DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT8DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT8DE_Source;
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
  kdBuffPtr: PKDT8DE_SourceBuffer;
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
      axis := Depth mod KDT8DE_Axis;
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

function TKDT8DE.GetData(const Index: NativeInt): PKDT8DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT8DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT8DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT8DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT8DE_Node(KDNodes[i]));
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

function TKDT8DE.StoreBuffPtr: PKDT8DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT8DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT8DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT8DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT8DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT8DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT8DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT8DE.BuildKDTreeWithCluster(const inBuff: TKDT8DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT8DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT8DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT8DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT8DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT8DE.BuildKDTreeWithCluster(const inBuff: TKDT8DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT8DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DE_BuildCall);
var
  TempStoreBuff: TKDT8DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT8DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT8DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT8DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT8DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT8DE_Axis - 1 do
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

procedure TKDT8DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DE_BuildMethod);
var
  TempStoreBuff: TKDT8DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT8DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT8DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT8DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT8DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT8DE_Axis - 1 do
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


procedure TKDT8DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DE_BuildProc);
var
  TempStoreBuff: TKDT8DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT8DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT8DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT8DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT8DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT8DE_Axis - 1 do
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


function TKDT8DE.Search(const buff: TKDT8DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT8DE_Node;

var
  NearestNeighbour: PKDT8DE_Node;

  function FindParentNode(const buffPtr: PKDT8DE_Vec; NodePtr: PKDT8DE_Node): PKDT8DE_Node;
  var
    Next: PKDT8DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT8DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT8DE_Node; const buffPtr: PKDT8DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT8DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT8DE_Axis;
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

  function SortCompare(const buffPtr: PKDT8DE_Vec; const p1, p2: PKDT8DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT8DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT8DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT8DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT8DE_Node;
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
  Parent: PKDT8DE_Node;
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

  SearchedDistanceMin := KDT8DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT8DE_Node(NearestNodes[0]);
    end;
end;

function TKDT8DE.Search(const buff: TKDT8DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT8DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT8DE.Search(const buff: TKDT8DE_Vec; var SearchedDistanceMin: Double): PKDT8DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT8DE.Search(const buff: TKDT8DE_Vec): PKDT8DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT8DE.SearchToken(const buff: TKDT8DE_Vec): TPascalString;
var
  p: PKDT8DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT8DE.Search(const inBuff: TKDT8DE_DynamicVecBuffer; var OutBuff: TKDT8DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT8DE_DynamicVecBuffer;
  outBuffPtr: PKDT8DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT8DE_Node;
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
        p: PKDT8DE_Node;
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
  p: PKDT8DE_Node;
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


procedure TKDT8DE.Search(const inBuff: TKDT8DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT8DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT8DE_Node;
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
        p: PKDT8DE_Node;
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
  p: PKDT8DE_Node;
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


procedure TKDT8DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT8DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT8DE_Vec));
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

procedure TKDT8DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT8DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT8DE_Vec)) <> SizeOf(TKDT8DE_Vec) then
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

procedure TKDT8DE.SaveToFile(FileName: SystemString);
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

procedure TKDT8DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT8DE.PrintNodeTree(const NodePtr: PKDT8DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT8DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT8DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT8DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT8DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT8DE.KDT8DEVec(const s: SystemString): TKDT8DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT8DE_Axis - 1 do
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
            if j >= KDT8DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT8DE.KDT8DEVec(const v: TKDT8DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT8DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT8DE.KDT8DEPow(const v: TKDT8DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT8DE.KDT8DEDistance(const v1, v2: TKDT8DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT8DE_Axis - 1 do
      Result := Result + KDT8DEPow(v2[i] - v1[i]);
end;

procedure TKDT8DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT8DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT8DE.Test;
var
  TKDT8DE_Test: TKDT8DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT8DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT8DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT8DE_Test := TKDT8DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT8DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT8DE_Test.TestBuff) - 1 do
    for j := 0 to KDT8DE_Axis - 1 do
        TKDT8DE_Test.TestBuff[i][j] := i * KDT8DE_Axis + j;

{$IFDEF FPC}
  TKDT8DE_Test.BuildKDTreeM(length(TKDT8DE_Test.TestBuff), nil, @TKDT8DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT8DE_Test.BuildKDTreeM(length(TKDT8DE_Test.TestBuff), nil, TKDT8DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT8DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT8DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT8DE_Test.TestBuff) - 1 do
    begin
      p := TKDT8DE_Test.Search(TKDT8DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT8DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT8DE_Test.TestBuff));
      TKDT8DE_Test.Search(TKDT8DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT8DEDistance(TKDT8DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT8DE_Test.Clear;
      { kMean test }
      TKDT8DE_Test.BuildKDTreeWithCluster(TKDT8DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT8DE_Test.Search(TKDT8DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT8DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT8DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT8DE_Test);
end;


function TKDT9DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT9DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT9DE_Node;
  function SortCompare(const p1, p2: PKDT9DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT9DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT9DE_Source;
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
  kdBuffPtr: PKDT9DE_SourceBuffer;
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
      axis := Depth mod KDT9DE_Axis;
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

function TKDT9DE.GetData(const Index: NativeInt): PKDT9DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT9DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT9DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT9DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT9DE_Node(KDNodes[i]));
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

function TKDT9DE.StoreBuffPtr: PKDT9DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT9DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT9DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT9DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT9DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT9DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT9DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT9DE.BuildKDTreeWithCluster(const inBuff: TKDT9DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT9DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT9DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT9DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT9DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT9DE.BuildKDTreeWithCluster(const inBuff: TKDT9DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT9DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DE_BuildCall);
var
  TempStoreBuff: TKDT9DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT9DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT9DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT9DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT9DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT9DE_Axis - 1 do
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

procedure TKDT9DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DE_BuildMethod);
var
  TempStoreBuff: TKDT9DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT9DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT9DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT9DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT9DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT9DE_Axis - 1 do
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


procedure TKDT9DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DE_BuildProc);
var
  TempStoreBuff: TKDT9DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT9DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT9DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT9DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT9DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT9DE_Axis - 1 do
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


function TKDT9DE.Search(const buff: TKDT9DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT9DE_Node;

var
  NearestNeighbour: PKDT9DE_Node;

  function FindParentNode(const buffPtr: PKDT9DE_Vec; NodePtr: PKDT9DE_Node): PKDT9DE_Node;
  var
    Next: PKDT9DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT9DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT9DE_Node; const buffPtr: PKDT9DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT9DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT9DE_Axis;
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

  function SortCompare(const buffPtr: PKDT9DE_Vec; const p1, p2: PKDT9DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT9DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT9DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT9DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT9DE_Node;
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
  Parent: PKDT9DE_Node;
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

  SearchedDistanceMin := KDT9DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT9DE_Node(NearestNodes[0]);
    end;
end;

function TKDT9DE.Search(const buff: TKDT9DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT9DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT9DE.Search(const buff: TKDT9DE_Vec; var SearchedDistanceMin: Double): PKDT9DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT9DE.Search(const buff: TKDT9DE_Vec): PKDT9DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT9DE.SearchToken(const buff: TKDT9DE_Vec): TPascalString;
var
  p: PKDT9DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT9DE.Search(const inBuff: TKDT9DE_DynamicVecBuffer; var OutBuff: TKDT9DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT9DE_DynamicVecBuffer;
  outBuffPtr: PKDT9DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT9DE_Node;
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
        p: PKDT9DE_Node;
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
  p: PKDT9DE_Node;
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


procedure TKDT9DE.Search(const inBuff: TKDT9DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT9DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT9DE_Node;
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
        p: PKDT9DE_Node;
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
  p: PKDT9DE_Node;
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


procedure TKDT9DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT9DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT9DE_Vec));
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

procedure TKDT9DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT9DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT9DE_Vec)) <> SizeOf(TKDT9DE_Vec) then
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

procedure TKDT9DE.SaveToFile(FileName: SystemString);
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

procedure TKDT9DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT9DE.PrintNodeTree(const NodePtr: PKDT9DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT9DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT9DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT9DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT9DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT9DE.KDT9DEVec(const s: SystemString): TKDT9DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT9DE_Axis - 1 do
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
            if j >= KDT9DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT9DE.KDT9DEVec(const v: TKDT9DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT9DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT9DE.KDT9DEPow(const v: TKDT9DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT9DE.KDT9DEDistance(const v1, v2: TKDT9DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT9DE_Axis - 1 do
      Result := Result + KDT9DEPow(v2[i] - v1[i]);
end;

procedure TKDT9DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT9DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT9DE.Test;
var
  TKDT9DE_Test: TKDT9DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT9DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT9DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT9DE_Test := TKDT9DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT9DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT9DE_Test.TestBuff) - 1 do
    for j := 0 to KDT9DE_Axis - 1 do
        TKDT9DE_Test.TestBuff[i][j] := i * KDT9DE_Axis + j;

{$IFDEF FPC}
  TKDT9DE_Test.BuildKDTreeM(length(TKDT9DE_Test.TestBuff), nil, @TKDT9DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT9DE_Test.BuildKDTreeM(length(TKDT9DE_Test.TestBuff), nil, TKDT9DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT9DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT9DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT9DE_Test.TestBuff) - 1 do
    begin
      p := TKDT9DE_Test.Search(TKDT9DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT9DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT9DE_Test.TestBuff));
      TKDT9DE_Test.Search(TKDT9DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT9DEDistance(TKDT9DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT9DE_Test.Clear;
      { kMean test }
      TKDT9DE_Test.BuildKDTreeWithCluster(TKDT9DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT9DE_Test.Search(TKDT9DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT9DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT9DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT9DE_Test);
end;


function TKDT10DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT10DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT10DE_Node;
  function SortCompare(const p1, p2: PKDT10DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT10DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT10DE_Source;
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
  kdBuffPtr: PKDT10DE_SourceBuffer;
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
      axis := Depth mod KDT10DE_Axis;
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

function TKDT10DE.GetData(const Index: NativeInt): PKDT10DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT10DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT10DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT10DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT10DE_Node(KDNodes[i]));
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

function TKDT10DE.StoreBuffPtr: PKDT10DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT10DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT10DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT10DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT10DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT10DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT10DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT10DE.BuildKDTreeWithCluster(const inBuff: TKDT10DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT10DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT10DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT10DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT10DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT10DE.BuildKDTreeWithCluster(const inBuff: TKDT10DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT10DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DE_BuildCall);
var
  TempStoreBuff: TKDT10DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT10DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT10DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT10DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT10DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT10DE_Axis - 1 do
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

procedure TKDT10DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DE_BuildMethod);
var
  TempStoreBuff: TKDT10DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT10DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT10DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT10DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT10DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT10DE_Axis - 1 do
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


procedure TKDT10DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DE_BuildProc);
var
  TempStoreBuff: TKDT10DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT10DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT10DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT10DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT10DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT10DE_Axis - 1 do
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


function TKDT10DE.Search(const buff: TKDT10DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT10DE_Node;

var
  NearestNeighbour: PKDT10DE_Node;

  function FindParentNode(const buffPtr: PKDT10DE_Vec; NodePtr: PKDT10DE_Node): PKDT10DE_Node;
  var
    Next: PKDT10DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT10DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT10DE_Node; const buffPtr: PKDT10DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT10DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT10DE_Axis;
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

  function SortCompare(const buffPtr: PKDT10DE_Vec; const p1, p2: PKDT10DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT10DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT10DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT10DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT10DE_Node;
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
  Parent: PKDT10DE_Node;
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

  SearchedDistanceMin := KDT10DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT10DE_Node(NearestNodes[0]);
    end;
end;

function TKDT10DE.Search(const buff: TKDT10DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT10DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT10DE.Search(const buff: TKDT10DE_Vec; var SearchedDistanceMin: Double): PKDT10DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT10DE.Search(const buff: TKDT10DE_Vec): PKDT10DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT10DE.SearchToken(const buff: TKDT10DE_Vec): TPascalString;
var
  p: PKDT10DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT10DE.Search(const inBuff: TKDT10DE_DynamicVecBuffer; var OutBuff: TKDT10DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT10DE_DynamicVecBuffer;
  outBuffPtr: PKDT10DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT10DE_Node;
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
        p: PKDT10DE_Node;
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
  p: PKDT10DE_Node;
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


procedure TKDT10DE.Search(const inBuff: TKDT10DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT10DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT10DE_Node;
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
        p: PKDT10DE_Node;
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
  p: PKDT10DE_Node;
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


procedure TKDT10DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT10DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT10DE_Vec));
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

procedure TKDT10DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT10DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT10DE_Vec)) <> SizeOf(TKDT10DE_Vec) then
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

procedure TKDT10DE.SaveToFile(FileName: SystemString);
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

procedure TKDT10DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT10DE.PrintNodeTree(const NodePtr: PKDT10DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT10DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT10DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT10DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT10DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT10DE.KDT10DEVec(const s: SystemString): TKDT10DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT10DE_Axis - 1 do
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
            if j >= KDT10DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT10DE.KDT10DEVec(const v: TKDT10DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT10DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT10DE.KDT10DEPow(const v: TKDT10DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT10DE.KDT10DEDistance(const v1, v2: TKDT10DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT10DE_Axis - 1 do
      Result := Result + KDT10DEPow(v2[i] - v1[i]);
end;

procedure TKDT10DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT10DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT10DE.Test;
var
  TKDT10DE_Test: TKDT10DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT10DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT10DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT10DE_Test := TKDT10DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT10DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT10DE_Test.TestBuff) - 1 do
    for j := 0 to KDT10DE_Axis - 1 do
        TKDT10DE_Test.TestBuff[i][j] := i * KDT10DE_Axis + j;

{$IFDEF FPC}
  TKDT10DE_Test.BuildKDTreeM(length(TKDT10DE_Test.TestBuff), nil, @TKDT10DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT10DE_Test.BuildKDTreeM(length(TKDT10DE_Test.TestBuff), nil, TKDT10DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT10DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT10DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT10DE_Test.TestBuff) - 1 do
    begin
      p := TKDT10DE_Test.Search(TKDT10DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT10DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT10DE_Test.TestBuff));
      TKDT10DE_Test.Search(TKDT10DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT10DEDistance(TKDT10DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT10DE_Test.Clear;
      { kMean test }
      TKDT10DE_Test.BuildKDTreeWithCluster(TKDT10DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT10DE_Test.Search(TKDT10DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT10DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT10DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT10DE_Test);
end;


function TKDT11DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT11DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT11DE_Node;
  function SortCompare(const p1, p2: PKDT11DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT11DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT11DE_Source;
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
  kdBuffPtr: PKDT11DE_SourceBuffer;
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
      axis := Depth mod KDT11DE_Axis;
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

function TKDT11DE.GetData(const Index: NativeInt): PKDT11DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT11DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT11DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT11DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT11DE_Node(KDNodes[i]));
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

function TKDT11DE.StoreBuffPtr: PKDT11DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT11DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT11DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT11DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT11DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT11DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT11DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT11DE.BuildKDTreeWithCluster(const inBuff: TKDT11DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT11DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT11DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT11DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT11DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT11DE.BuildKDTreeWithCluster(const inBuff: TKDT11DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT11DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DE_BuildCall);
var
  TempStoreBuff: TKDT11DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT11DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT11DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT11DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT11DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT11DE_Axis - 1 do
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

procedure TKDT11DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DE_BuildMethod);
var
  TempStoreBuff: TKDT11DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT11DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT11DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT11DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT11DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT11DE_Axis - 1 do
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


procedure TKDT11DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DE_BuildProc);
var
  TempStoreBuff: TKDT11DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT11DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT11DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT11DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT11DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT11DE_Axis - 1 do
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


function TKDT11DE.Search(const buff: TKDT11DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT11DE_Node;

var
  NearestNeighbour: PKDT11DE_Node;

  function FindParentNode(const buffPtr: PKDT11DE_Vec; NodePtr: PKDT11DE_Node): PKDT11DE_Node;
  var
    Next: PKDT11DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT11DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT11DE_Node; const buffPtr: PKDT11DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT11DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT11DE_Axis;
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

  function SortCompare(const buffPtr: PKDT11DE_Vec; const p1, p2: PKDT11DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT11DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT11DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT11DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT11DE_Node;
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
  Parent: PKDT11DE_Node;
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

  SearchedDistanceMin := KDT11DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT11DE_Node(NearestNodes[0]);
    end;
end;

function TKDT11DE.Search(const buff: TKDT11DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT11DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT11DE.Search(const buff: TKDT11DE_Vec; var SearchedDistanceMin: Double): PKDT11DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT11DE.Search(const buff: TKDT11DE_Vec): PKDT11DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT11DE.SearchToken(const buff: TKDT11DE_Vec): TPascalString;
var
  p: PKDT11DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT11DE.Search(const inBuff: TKDT11DE_DynamicVecBuffer; var OutBuff: TKDT11DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT11DE_DynamicVecBuffer;
  outBuffPtr: PKDT11DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT11DE_Node;
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
        p: PKDT11DE_Node;
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
  p: PKDT11DE_Node;
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


procedure TKDT11DE.Search(const inBuff: TKDT11DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT11DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT11DE_Node;
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
        p: PKDT11DE_Node;
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
  p: PKDT11DE_Node;
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


procedure TKDT11DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT11DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT11DE_Vec));
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

procedure TKDT11DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT11DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT11DE_Vec)) <> SizeOf(TKDT11DE_Vec) then
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

procedure TKDT11DE.SaveToFile(FileName: SystemString);
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

procedure TKDT11DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT11DE.PrintNodeTree(const NodePtr: PKDT11DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT11DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT11DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT11DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT11DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT11DE.KDT11DEVec(const s: SystemString): TKDT11DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT11DE_Axis - 1 do
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
            if j >= KDT11DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT11DE.KDT11DEVec(const v: TKDT11DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT11DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT11DE.KDT11DEPow(const v: TKDT11DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT11DE.KDT11DEDistance(const v1, v2: TKDT11DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT11DE_Axis - 1 do
      Result := Result + KDT11DEPow(v2[i] - v1[i]);
end;

procedure TKDT11DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT11DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT11DE.Test;
var
  TKDT11DE_Test: TKDT11DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT11DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT11DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT11DE_Test := TKDT11DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT11DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT11DE_Test.TestBuff) - 1 do
    for j := 0 to KDT11DE_Axis - 1 do
        TKDT11DE_Test.TestBuff[i][j] := i * KDT11DE_Axis + j;

{$IFDEF FPC}
  TKDT11DE_Test.BuildKDTreeM(length(TKDT11DE_Test.TestBuff), nil, @TKDT11DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT11DE_Test.BuildKDTreeM(length(TKDT11DE_Test.TestBuff), nil, TKDT11DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT11DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT11DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT11DE_Test.TestBuff) - 1 do
    begin
      p := TKDT11DE_Test.Search(TKDT11DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT11DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT11DE_Test.TestBuff));
      TKDT11DE_Test.Search(TKDT11DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT11DEDistance(TKDT11DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT11DE_Test.Clear;
      { kMean test }
      TKDT11DE_Test.BuildKDTreeWithCluster(TKDT11DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT11DE_Test.Search(TKDT11DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT11DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT11DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT11DE_Test);
end;


function TKDT12DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT12DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT12DE_Node;
  function SortCompare(const p1, p2: PKDT12DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT12DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT12DE_Source;
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
  kdBuffPtr: PKDT12DE_SourceBuffer;
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
      axis := Depth mod KDT12DE_Axis;
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

function TKDT12DE.GetData(const Index: NativeInt): PKDT12DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT12DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT12DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT12DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT12DE_Node(KDNodes[i]));
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

function TKDT12DE.StoreBuffPtr: PKDT12DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT12DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT12DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT12DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT12DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT12DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT12DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT12DE.BuildKDTreeWithCluster(const inBuff: TKDT12DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT12DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT12DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT12DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT12DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT12DE.BuildKDTreeWithCluster(const inBuff: TKDT12DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT12DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DE_BuildCall);
var
  TempStoreBuff: TKDT12DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT12DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT12DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT12DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT12DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT12DE_Axis - 1 do
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

procedure TKDT12DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DE_BuildMethod);
var
  TempStoreBuff: TKDT12DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT12DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT12DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT12DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT12DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT12DE_Axis - 1 do
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


procedure TKDT12DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DE_BuildProc);
var
  TempStoreBuff: TKDT12DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT12DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT12DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT12DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT12DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT12DE_Axis - 1 do
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


function TKDT12DE.Search(const buff: TKDT12DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT12DE_Node;

var
  NearestNeighbour: PKDT12DE_Node;

  function FindParentNode(const buffPtr: PKDT12DE_Vec; NodePtr: PKDT12DE_Node): PKDT12DE_Node;
  var
    Next: PKDT12DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT12DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT12DE_Node; const buffPtr: PKDT12DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT12DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT12DE_Axis;
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

  function SortCompare(const buffPtr: PKDT12DE_Vec; const p1, p2: PKDT12DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT12DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT12DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT12DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT12DE_Node;
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
  Parent: PKDT12DE_Node;
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

  SearchedDistanceMin := KDT12DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT12DE_Node(NearestNodes[0]);
    end;
end;

function TKDT12DE.Search(const buff: TKDT12DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT12DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT12DE.Search(const buff: TKDT12DE_Vec; var SearchedDistanceMin: Double): PKDT12DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT12DE.Search(const buff: TKDT12DE_Vec): PKDT12DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT12DE.SearchToken(const buff: TKDT12DE_Vec): TPascalString;
var
  p: PKDT12DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT12DE.Search(const inBuff: TKDT12DE_DynamicVecBuffer; var OutBuff: TKDT12DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT12DE_DynamicVecBuffer;
  outBuffPtr: PKDT12DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT12DE_Node;
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
        p: PKDT12DE_Node;
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
  p: PKDT12DE_Node;
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


procedure TKDT12DE.Search(const inBuff: TKDT12DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT12DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT12DE_Node;
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
        p: PKDT12DE_Node;
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
  p: PKDT12DE_Node;
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


procedure TKDT12DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT12DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT12DE_Vec));
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

procedure TKDT12DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT12DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT12DE_Vec)) <> SizeOf(TKDT12DE_Vec) then
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

procedure TKDT12DE.SaveToFile(FileName: SystemString);
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

procedure TKDT12DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT12DE.PrintNodeTree(const NodePtr: PKDT12DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT12DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT12DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT12DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT12DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT12DE.KDT12DEVec(const s: SystemString): TKDT12DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT12DE_Axis - 1 do
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
            if j >= KDT12DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT12DE.KDT12DEVec(const v: TKDT12DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT12DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT12DE.KDT12DEPow(const v: TKDT12DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT12DE.KDT12DEDistance(const v1, v2: TKDT12DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT12DE_Axis - 1 do
      Result := Result + KDT12DEPow(v2[i] - v1[i]);
end;

procedure TKDT12DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT12DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT12DE.Test;
var
  TKDT12DE_Test: TKDT12DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT12DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT12DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT12DE_Test := TKDT12DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT12DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT12DE_Test.TestBuff) - 1 do
    for j := 0 to KDT12DE_Axis - 1 do
        TKDT12DE_Test.TestBuff[i][j] := i * KDT12DE_Axis + j;

{$IFDEF FPC}
  TKDT12DE_Test.BuildKDTreeM(length(TKDT12DE_Test.TestBuff), nil, @TKDT12DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT12DE_Test.BuildKDTreeM(length(TKDT12DE_Test.TestBuff), nil, TKDT12DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT12DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT12DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT12DE_Test.TestBuff) - 1 do
    begin
      p := TKDT12DE_Test.Search(TKDT12DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT12DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT12DE_Test.TestBuff));
      TKDT12DE_Test.Search(TKDT12DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT12DEDistance(TKDT12DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT12DE_Test.Clear;
      { kMean test }
      TKDT12DE_Test.BuildKDTreeWithCluster(TKDT12DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT12DE_Test.Search(TKDT12DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT12DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT12DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT12DE_Test);
end;


function TKDT13DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT13DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT13DE_Node;
  function SortCompare(const p1, p2: PKDT13DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT13DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT13DE_Source;
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
  kdBuffPtr: PKDT13DE_SourceBuffer;
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
      axis := Depth mod KDT13DE_Axis;
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

function TKDT13DE.GetData(const Index: NativeInt): PKDT13DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT13DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT13DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT13DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT13DE_Node(KDNodes[i]));
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

function TKDT13DE.StoreBuffPtr: PKDT13DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT13DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT13DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT13DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT13DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT13DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT13DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT13DE.BuildKDTreeWithCluster(const inBuff: TKDT13DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT13DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT13DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT13DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT13DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT13DE.BuildKDTreeWithCluster(const inBuff: TKDT13DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT13DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DE_BuildCall);
var
  TempStoreBuff: TKDT13DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT13DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT13DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT13DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT13DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT13DE_Axis - 1 do
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

procedure TKDT13DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DE_BuildMethod);
var
  TempStoreBuff: TKDT13DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT13DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT13DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT13DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT13DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT13DE_Axis - 1 do
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


procedure TKDT13DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DE_BuildProc);
var
  TempStoreBuff: TKDT13DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT13DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT13DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT13DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT13DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT13DE_Axis - 1 do
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


function TKDT13DE.Search(const buff: TKDT13DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT13DE_Node;

var
  NearestNeighbour: PKDT13DE_Node;

  function FindParentNode(const buffPtr: PKDT13DE_Vec; NodePtr: PKDT13DE_Node): PKDT13DE_Node;
  var
    Next: PKDT13DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT13DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT13DE_Node; const buffPtr: PKDT13DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT13DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT13DE_Axis;
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

  function SortCompare(const buffPtr: PKDT13DE_Vec; const p1, p2: PKDT13DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT13DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT13DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT13DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT13DE_Node;
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
  Parent: PKDT13DE_Node;
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

  SearchedDistanceMin := KDT13DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT13DE_Node(NearestNodes[0]);
    end;
end;

function TKDT13DE.Search(const buff: TKDT13DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT13DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT13DE.Search(const buff: TKDT13DE_Vec; var SearchedDistanceMin: Double): PKDT13DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT13DE.Search(const buff: TKDT13DE_Vec): PKDT13DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT13DE.SearchToken(const buff: TKDT13DE_Vec): TPascalString;
var
  p: PKDT13DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT13DE.Search(const inBuff: TKDT13DE_DynamicVecBuffer; var OutBuff: TKDT13DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT13DE_DynamicVecBuffer;
  outBuffPtr: PKDT13DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT13DE_Node;
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
        p: PKDT13DE_Node;
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
  p: PKDT13DE_Node;
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


procedure TKDT13DE.Search(const inBuff: TKDT13DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT13DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT13DE_Node;
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
        p: PKDT13DE_Node;
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
  p: PKDT13DE_Node;
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


procedure TKDT13DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT13DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT13DE_Vec));
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

procedure TKDT13DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT13DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT13DE_Vec)) <> SizeOf(TKDT13DE_Vec) then
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

procedure TKDT13DE.SaveToFile(FileName: SystemString);
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

procedure TKDT13DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT13DE.PrintNodeTree(const NodePtr: PKDT13DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT13DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT13DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT13DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT13DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT13DE.KDT13DEVec(const s: SystemString): TKDT13DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT13DE_Axis - 1 do
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
            if j >= KDT13DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT13DE.KDT13DEVec(const v: TKDT13DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT13DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT13DE.KDT13DEPow(const v: TKDT13DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT13DE.KDT13DEDistance(const v1, v2: TKDT13DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT13DE_Axis - 1 do
      Result := Result + KDT13DEPow(v2[i] - v1[i]);
end;

procedure TKDT13DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT13DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT13DE.Test;
var
  TKDT13DE_Test: TKDT13DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT13DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT13DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT13DE_Test := TKDT13DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT13DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT13DE_Test.TestBuff) - 1 do
    for j := 0 to KDT13DE_Axis - 1 do
        TKDT13DE_Test.TestBuff[i][j] := i * KDT13DE_Axis + j;

{$IFDEF FPC}
  TKDT13DE_Test.BuildKDTreeM(length(TKDT13DE_Test.TestBuff), nil, @TKDT13DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT13DE_Test.BuildKDTreeM(length(TKDT13DE_Test.TestBuff), nil, TKDT13DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT13DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT13DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT13DE_Test.TestBuff) - 1 do
    begin
      p := TKDT13DE_Test.Search(TKDT13DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT13DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT13DE_Test.TestBuff));
      TKDT13DE_Test.Search(TKDT13DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT13DEDistance(TKDT13DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT13DE_Test.Clear;
      { kMean test }
      TKDT13DE_Test.BuildKDTreeWithCluster(TKDT13DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT13DE_Test.Search(TKDT13DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT13DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT13DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT13DE_Test);
end;


function TKDT14DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT14DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT14DE_Node;
  function SortCompare(const p1, p2: PKDT14DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT14DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT14DE_Source;
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
  kdBuffPtr: PKDT14DE_SourceBuffer;
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
      axis := Depth mod KDT14DE_Axis;
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

function TKDT14DE.GetData(const Index: NativeInt): PKDT14DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT14DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT14DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT14DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT14DE_Node(KDNodes[i]));
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

function TKDT14DE.StoreBuffPtr: PKDT14DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT14DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT14DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT14DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT14DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT14DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT14DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT14DE.BuildKDTreeWithCluster(const inBuff: TKDT14DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT14DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT14DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT14DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT14DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT14DE.BuildKDTreeWithCluster(const inBuff: TKDT14DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT14DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DE_BuildCall);
var
  TempStoreBuff: TKDT14DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT14DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT14DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT14DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT14DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT14DE_Axis - 1 do
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

procedure TKDT14DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DE_BuildMethod);
var
  TempStoreBuff: TKDT14DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT14DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT14DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT14DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT14DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT14DE_Axis - 1 do
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


procedure TKDT14DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DE_BuildProc);
var
  TempStoreBuff: TKDT14DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT14DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT14DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT14DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT14DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT14DE_Axis - 1 do
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


function TKDT14DE.Search(const buff: TKDT14DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT14DE_Node;

var
  NearestNeighbour: PKDT14DE_Node;

  function FindParentNode(const buffPtr: PKDT14DE_Vec; NodePtr: PKDT14DE_Node): PKDT14DE_Node;
  var
    Next: PKDT14DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT14DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT14DE_Node; const buffPtr: PKDT14DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT14DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT14DE_Axis;
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

  function SortCompare(const buffPtr: PKDT14DE_Vec; const p1, p2: PKDT14DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT14DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT14DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT14DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT14DE_Node;
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
  Parent: PKDT14DE_Node;
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

  SearchedDistanceMin := KDT14DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT14DE_Node(NearestNodes[0]);
    end;
end;

function TKDT14DE.Search(const buff: TKDT14DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT14DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT14DE.Search(const buff: TKDT14DE_Vec; var SearchedDistanceMin: Double): PKDT14DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT14DE.Search(const buff: TKDT14DE_Vec): PKDT14DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT14DE.SearchToken(const buff: TKDT14DE_Vec): TPascalString;
var
  p: PKDT14DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT14DE.Search(const inBuff: TKDT14DE_DynamicVecBuffer; var OutBuff: TKDT14DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT14DE_DynamicVecBuffer;
  outBuffPtr: PKDT14DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT14DE_Node;
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
        p: PKDT14DE_Node;
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
  p: PKDT14DE_Node;
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


procedure TKDT14DE.Search(const inBuff: TKDT14DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT14DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT14DE_Node;
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
        p: PKDT14DE_Node;
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
  p: PKDT14DE_Node;
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


procedure TKDT14DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT14DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT14DE_Vec));
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

procedure TKDT14DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT14DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT14DE_Vec)) <> SizeOf(TKDT14DE_Vec) then
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

procedure TKDT14DE.SaveToFile(FileName: SystemString);
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

procedure TKDT14DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT14DE.PrintNodeTree(const NodePtr: PKDT14DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT14DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT14DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT14DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT14DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT14DE.KDT14DEVec(const s: SystemString): TKDT14DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT14DE_Axis - 1 do
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
            if j >= KDT14DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT14DE.KDT14DEVec(const v: TKDT14DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT14DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT14DE.KDT14DEPow(const v: TKDT14DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT14DE.KDT14DEDistance(const v1, v2: TKDT14DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT14DE_Axis - 1 do
      Result := Result + KDT14DEPow(v2[i] - v1[i]);
end;

procedure TKDT14DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT14DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT14DE.Test;
var
  TKDT14DE_Test: TKDT14DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT14DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT14DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT14DE_Test := TKDT14DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT14DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT14DE_Test.TestBuff) - 1 do
    for j := 0 to KDT14DE_Axis - 1 do
        TKDT14DE_Test.TestBuff[i][j] := i * KDT14DE_Axis + j;

{$IFDEF FPC}
  TKDT14DE_Test.BuildKDTreeM(length(TKDT14DE_Test.TestBuff), nil, @TKDT14DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT14DE_Test.BuildKDTreeM(length(TKDT14DE_Test.TestBuff), nil, TKDT14DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT14DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT14DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT14DE_Test.TestBuff) - 1 do
    begin
      p := TKDT14DE_Test.Search(TKDT14DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT14DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT14DE_Test.TestBuff));
      TKDT14DE_Test.Search(TKDT14DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT14DEDistance(TKDT14DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT14DE_Test.Clear;
      { kMean test }
      TKDT14DE_Test.BuildKDTreeWithCluster(TKDT14DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT14DE_Test.Search(TKDT14DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT14DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT14DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT14DE_Test);
end;


function TKDT15DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT15DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT15DE_Node;
  function SortCompare(const p1, p2: PKDT15DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT15DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT15DE_Source;
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
  kdBuffPtr: PKDT15DE_SourceBuffer;
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
      axis := Depth mod KDT15DE_Axis;
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

function TKDT15DE.GetData(const Index: NativeInt): PKDT15DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT15DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT15DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT15DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT15DE_Node(KDNodes[i]));
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

function TKDT15DE.StoreBuffPtr: PKDT15DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT15DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT15DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT15DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT15DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT15DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT15DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT15DE.BuildKDTreeWithCluster(const inBuff: TKDT15DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT15DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT15DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT15DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT15DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT15DE.BuildKDTreeWithCluster(const inBuff: TKDT15DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT15DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DE_BuildCall);
var
  TempStoreBuff: TKDT15DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT15DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT15DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT15DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT15DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT15DE_Axis - 1 do
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

procedure TKDT15DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DE_BuildMethod);
var
  TempStoreBuff: TKDT15DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT15DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT15DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT15DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT15DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT15DE_Axis - 1 do
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


procedure TKDT15DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DE_BuildProc);
var
  TempStoreBuff: TKDT15DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT15DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT15DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT15DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT15DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT15DE_Axis - 1 do
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


function TKDT15DE.Search(const buff: TKDT15DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT15DE_Node;

var
  NearestNeighbour: PKDT15DE_Node;

  function FindParentNode(const buffPtr: PKDT15DE_Vec; NodePtr: PKDT15DE_Node): PKDT15DE_Node;
  var
    Next: PKDT15DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT15DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT15DE_Node; const buffPtr: PKDT15DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT15DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT15DE_Axis;
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

  function SortCompare(const buffPtr: PKDT15DE_Vec; const p1, p2: PKDT15DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT15DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT15DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT15DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT15DE_Node;
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
  Parent: PKDT15DE_Node;
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

  SearchedDistanceMin := KDT15DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT15DE_Node(NearestNodes[0]);
    end;
end;

function TKDT15DE.Search(const buff: TKDT15DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT15DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT15DE.Search(const buff: TKDT15DE_Vec; var SearchedDistanceMin: Double): PKDT15DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT15DE.Search(const buff: TKDT15DE_Vec): PKDT15DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT15DE.SearchToken(const buff: TKDT15DE_Vec): TPascalString;
var
  p: PKDT15DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT15DE.Search(const inBuff: TKDT15DE_DynamicVecBuffer; var OutBuff: TKDT15DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT15DE_DynamicVecBuffer;
  outBuffPtr: PKDT15DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT15DE_Node;
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
        p: PKDT15DE_Node;
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
  p: PKDT15DE_Node;
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


procedure TKDT15DE.Search(const inBuff: TKDT15DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT15DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT15DE_Node;
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
        p: PKDT15DE_Node;
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
  p: PKDT15DE_Node;
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


procedure TKDT15DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT15DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT15DE_Vec));
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

procedure TKDT15DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT15DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT15DE_Vec)) <> SizeOf(TKDT15DE_Vec) then
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

procedure TKDT15DE.SaveToFile(FileName: SystemString);
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

procedure TKDT15DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT15DE.PrintNodeTree(const NodePtr: PKDT15DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT15DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT15DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT15DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT15DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT15DE.KDT15DEVec(const s: SystemString): TKDT15DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT15DE_Axis - 1 do
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
            if j >= KDT15DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT15DE.KDT15DEVec(const v: TKDT15DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT15DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT15DE.KDT15DEPow(const v: TKDT15DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT15DE.KDT15DEDistance(const v1, v2: TKDT15DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT15DE_Axis - 1 do
      Result := Result + KDT15DEPow(v2[i] - v1[i]);
end;

procedure TKDT15DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT15DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT15DE.Test;
var
  TKDT15DE_Test: TKDT15DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT15DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT15DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT15DE_Test := TKDT15DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT15DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT15DE_Test.TestBuff) - 1 do
    for j := 0 to KDT15DE_Axis - 1 do
        TKDT15DE_Test.TestBuff[i][j] := i * KDT15DE_Axis + j;

{$IFDEF FPC}
  TKDT15DE_Test.BuildKDTreeM(length(TKDT15DE_Test.TestBuff), nil, @TKDT15DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT15DE_Test.BuildKDTreeM(length(TKDT15DE_Test.TestBuff), nil, TKDT15DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT15DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT15DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT15DE_Test.TestBuff) - 1 do
    begin
      p := TKDT15DE_Test.Search(TKDT15DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT15DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT15DE_Test.TestBuff));
      TKDT15DE_Test.Search(TKDT15DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT15DEDistance(TKDT15DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT15DE_Test.Clear;
      { kMean test }
      TKDT15DE_Test.BuildKDTreeWithCluster(TKDT15DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT15DE_Test.Search(TKDT15DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT15DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT15DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT15DE_Test);
end;


function TKDT16DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT16DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT16DE_Node;
  function SortCompare(const p1, p2: PKDT16DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT16DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT16DE_Source;
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
  kdBuffPtr: PKDT16DE_SourceBuffer;
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
      axis := Depth mod KDT16DE_Axis;
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

function TKDT16DE.GetData(const Index: NativeInt): PKDT16DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT16DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT16DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT16DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT16DE_Node(KDNodes[i]));
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

function TKDT16DE.StoreBuffPtr: PKDT16DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT16DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT16DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT16DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT16DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT16DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT16DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT16DE.BuildKDTreeWithCluster(const inBuff: TKDT16DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT16DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT16DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT16DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT16DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT16DE.BuildKDTreeWithCluster(const inBuff: TKDT16DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT16DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DE_BuildCall);
var
  TempStoreBuff: TKDT16DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT16DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT16DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT16DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT16DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT16DE_Axis - 1 do
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

procedure TKDT16DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DE_BuildMethod);
var
  TempStoreBuff: TKDT16DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT16DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT16DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT16DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT16DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT16DE_Axis - 1 do
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


procedure TKDT16DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DE_BuildProc);
var
  TempStoreBuff: TKDT16DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT16DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT16DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT16DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT16DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT16DE_Axis - 1 do
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


function TKDT16DE.Search(const buff: TKDT16DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT16DE_Node;

var
  NearestNeighbour: PKDT16DE_Node;

  function FindParentNode(const buffPtr: PKDT16DE_Vec; NodePtr: PKDT16DE_Node): PKDT16DE_Node;
  var
    Next: PKDT16DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT16DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT16DE_Node; const buffPtr: PKDT16DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT16DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT16DE_Axis;
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

  function SortCompare(const buffPtr: PKDT16DE_Vec; const p1, p2: PKDT16DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT16DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT16DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT16DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT16DE_Node;
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
  Parent: PKDT16DE_Node;
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

  SearchedDistanceMin := KDT16DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT16DE_Node(NearestNodes[0]);
    end;
end;

function TKDT16DE.Search(const buff: TKDT16DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT16DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT16DE.Search(const buff: TKDT16DE_Vec; var SearchedDistanceMin: Double): PKDT16DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT16DE.Search(const buff: TKDT16DE_Vec): PKDT16DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT16DE.SearchToken(const buff: TKDT16DE_Vec): TPascalString;
var
  p: PKDT16DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT16DE.Search(const inBuff: TKDT16DE_DynamicVecBuffer; var OutBuff: TKDT16DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT16DE_DynamicVecBuffer;
  outBuffPtr: PKDT16DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT16DE_Node;
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
        p: PKDT16DE_Node;
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
  p: PKDT16DE_Node;
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


procedure TKDT16DE.Search(const inBuff: TKDT16DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT16DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT16DE_Node;
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
        p: PKDT16DE_Node;
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
  p: PKDT16DE_Node;
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


procedure TKDT16DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT16DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT16DE_Vec));
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

procedure TKDT16DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT16DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT16DE_Vec)) <> SizeOf(TKDT16DE_Vec) then
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

procedure TKDT16DE.SaveToFile(FileName: SystemString);
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

procedure TKDT16DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT16DE.PrintNodeTree(const NodePtr: PKDT16DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT16DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT16DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT16DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT16DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT16DE.KDT16DEVec(const s: SystemString): TKDT16DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT16DE_Axis - 1 do
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
            if j >= KDT16DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT16DE.KDT16DEVec(const v: TKDT16DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT16DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT16DE.KDT16DEPow(const v: TKDT16DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT16DE.KDT16DEDistance(const v1, v2: TKDT16DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT16DE_Axis - 1 do
      Result := Result + KDT16DEPow(v2[i] - v1[i]);
end;

procedure TKDT16DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT16DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT16DE.Test;
var
  TKDT16DE_Test: TKDT16DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT16DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT16DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT16DE_Test := TKDT16DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT16DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT16DE_Test.TestBuff) - 1 do
    for j := 0 to KDT16DE_Axis - 1 do
        TKDT16DE_Test.TestBuff[i][j] := i * KDT16DE_Axis + j;

{$IFDEF FPC}
  TKDT16DE_Test.BuildKDTreeM(length(TKDT16DE_Test.TestBuff), nil, @TKDT16DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT16DE_Test.BuildKDTreeM(length(TKDT16DE_Test.TestBuff), nil, TKDT16DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT16DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT16DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT16DE_Test.TestBuff) - 1 do
    begin
      p := TKDT16DE_Test.Search(TKDT16DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT16DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT16DE_Test.TestBuff));
      TKDT16DE_Test.Search(TKDT16DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT16DEDistance(TKDT16DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT16DE_Test.Clear;
      { kMean test }
      TKDT16DE_Test.BuildKDTreeWithCluster(TKDT16DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT16DE_Test.Search(TKDT16DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT16DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT16DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT16DE_Test);
end;


function TKDT17DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT17DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT17DE_Node;
  function SortCompare(const p1, p2: PKDT17DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT17DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT17DE_Source;
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
  kdBuffPtr: PKDT17DE_SourceBuffer;
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
      axis := Depth mod KDT17DE_Axis;
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

function TKDT17DE.GetData(const Index: NativeInt): PKDT17DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT17DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT17DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT17DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT17DE_Node(KDNodes[i]));
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

function TKDT17DE.StoreBuffPtr: PKDT17DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT17DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT17DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT17DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT17DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT17DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT17DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT17DE.BuildKDTreeWithCluster(const inBuff: TKDT17DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT17DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT17DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT17DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT17DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT17DE.BuildKDTreeWithCluster(const inBuff: TKDT17DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT17DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DE_BuildCall);
var
  TempStoreBuff: TKDT17DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT17DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT17DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT17DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT17DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT17DE_Axis - 1 do
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

procedure TKDT17DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DE_BuildMethod);
var
  TempStoreBuff: TKDT17DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT17DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT17DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT17DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT17DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT17DE_Axis - 1 do
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


procedure TKDT17DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DE_BuildProc);
var
  TempStoreBuff: TKDT17DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT17DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT17DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT17DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT17DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT17DE_Axis - 1 do
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


function TKDT17DE.Search(const buff: TKDT17DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT17DE_Node;

var
  NearestNeighbour: PKDT17DE_Node;

  function FindParentNode(const buffPtr: PKDT17DE_Vec; NodePtr: PKDT17DE_Node): PKDT17DE_Node;
  var
    Next: PKDT17DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT17DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT17DE_Node; const buffPtr: PKDT17DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT17DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT17DE_Axis;
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

  function SortCompare(const buffPtr: PKDT17DE_Vec; const p1, p2: PKDT17DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT17DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT17DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT17DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT17DE_Node;
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
  Parent: PKDT17DE_Node;
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

  SearchedDistanceMin := KDT17DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT17DE_Node(NearestNodes[0]);
    end;
end;

function TKDT17DE.Search(const buff: TKDT17DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT17DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT17DE.Search(const buff: TKDT17DE_Vec; var SearchedDistanceMin: Double): PKDT17DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT17DE.Search(const buff: TKDT17DE_Vec): PKDT17DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT17DE.SearchToken(const buff: TKDT17DE_Vec): TPascalString;
var
  p: PKDT17DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT17DE.Search(const inBuff: TKDT17DE_DynamicVecBuffer; var OutBuff: TKDT17DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT17DE_DynamicVecBuffer;
  outBuffPtr: PKDT17DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT17DE_Node;
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
        p: PKDT17DE_Node;
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
  p: PKDT17DE_Node;
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


procedure TKDT17DE.Search(const inBuff: TKDT17DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT17DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT17DE_Node;
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
        p: PKDT17DE_Node;
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
  p: PKDT17DE_Node;
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


procedure TKDT17DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT17DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT17DE_Vec));
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

procedure TKDT17DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT17DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT17DE_Vec)) <> SizeOf(TKDT17DE_Vec) then
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

procedure TKDT17DE.SaveToFile(FileName: SystemString);
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

procedure TKDT17DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT17DE.PrintNodeTree(const NodePtr: PKDT17DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT17DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT17DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT17DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT17DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT17DE.KDT17DEVec(const s: SystemString): TKDT17DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT17DE_Axis - 1 do
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
            if j >= KDT17DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT17DE.KDT17DEVec(const v: TKDT17DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT17DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT17DE.KDT17DEPow(const v: TKDT17DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT17DE.KDT17DEDistance(const v1, v2: TKDT17DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT17DE_Axis - 1 do
      Result := Result + KDT17DEPow(v2[i] - v1[i]);
end;

procedure TKDT17DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT17DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT17DE.Test;
var
  TKDT17DE_Test: TKDT17DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT17DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT17DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT17DE_Test := TKDT17DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT17DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT17DE_Test.TestBuff) - 1 do
    for j := 0 to KDT17DE_Axis - 1 do
        TKDT17DE_Test.TestBuff[i][j] := i * KDT17DE_Axis + j;

{$IFDEF FPC}
  TKDT17DE_Test.BuildKDTreeM(length(TKDT17DE_Test.TestBuff), nil, @TKDT17DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT17DE_Test.BuildKDTreeM(length(TKDT17DE_Test.TestBuff), nil, TKDT17DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT17DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT17DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT17DE_Test.TestBuff) - 1 do
    begin
      p := TKDT17DE_Test.Search(TKDT17DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT17DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT17DE_Test.TestBuff));
      TKDT17DE_Test.Search(TKDT17DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT17DEDistance(TKDT17DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT17DE_Test.Clear;
      { kMean test }
      TKDT17DE_Test.BuildKDTreeWithCluster(TKDT17DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT17DE_Test.Search(TKDT17DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT17DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT17DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT17DE_Test);
end;


function TKDT18DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT18DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT18DE_Node;
  function SortCompare(const p1, p2: PKDT18DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT18DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT18DE_Source;
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
  kdBuffPtr: PKDT18DE_SourceBuffer;
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
      axis := Depth mod KDT18DE_Axis;
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

function TKDT18DE.GetData(const Index: NativeInt): PKDT18DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT18DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT18DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT18DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT18DE_Node(KDNodes[i]));
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

function TKDT18DE.StoreBuffPtr: PKDT18DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT18DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT18DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT18DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT18DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT18DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT18DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT18DE.BuildKDTreeWithCluster(const inBuff: TKDT18DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT18DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT18DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT18DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT18DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT18DE.BuildKDTreeWithCluster(const inBuff: TKDT18DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT18DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DE_BuildCall);
var
  TempStoreBuff: TKDT18DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT18DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT18DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT18DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT18DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT18DE_Axis - 1 do
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

procedure TKDT18DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DE_BuildMethod);
var
  TempStoreBuff: TKDT18DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT18DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT18DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT18DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT18DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT18DE_Axis - 1 do
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


procedure TKDT18DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DE_BuildProc);
var
  TempStoreBuff: TKDT18DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT18DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT18DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT18DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT18DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT18DE_Axis - 1 do
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


function TKDT18DE.Search(const buff: TKDT18DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT18DE_Node;

var
  NearestNeighbour: PKDT18DE_Node;

  function FindParentNode(const buffPtr: PKDT18DE_Vec; NodePtr: PKDT18DE_Node): PKDT18DE_Node;
  var
    Next: PKDT18DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT18DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT18DE_Node; const buffPtr: PKDT18DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT18DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT18DE_Axis;
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

  function SortCompare(const buffPtr: PKDT18DE_Vec; const p1, p2: PKDT18DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT18DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT18DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT18DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT18DE_Node;
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
  Parent: PKDT18DE_Node;
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

  SearchedDistanceMin := KDT18DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT18DE_Node(NearestNodes[0]);
    end;
end;

function TKDT18DE.Search(const buff: TKDT18DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT18DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT18DE.Search(const buff: TKDT18DE_Vec; var SearchedDistanceMin: Double): PKDT18DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT18DE.Search(const buff: TKDT18DE_Vec): PKDT18DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT18DE.SearchToken(const buff: TKDT18DE_Vec): TPascalString;
var
  p: PKDT18DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT18DE.Search(const inBuff: TKDT18DE_DynamicVecBuffer; var OutBuff: TKDT18DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT18DE_DynamicVecBuffer;
  outBuffPtr: PKDT18DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT18DE_Node;
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
        p: PKDT18DE_Node;
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
  p: PKDT18DE_Node;
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


procedure TKDT18DE.Search(const inBuff: TKDT18DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT18DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT18DE_Node;
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
        p: PKDT18DE_Node;
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
  p: PKDT18DE_Node;
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


procedure TKDT18DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT18DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT18DE_Vec));
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

procedure TKDT18DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT18DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT18DE_Vec)) <> SizeOf(TKDT18DE_Vec) then
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

procedure TKDT18DE.SaveToFile(FileName: SystemString);
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

procedure TKDT18DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT18DE.PrintNodeTree(const NodePtr: PKDT18DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT18DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT18DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT18DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT18DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT18DE.KDT18DEVec(const s: SystemString): TKDT18DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT18DE_Axis - 1 do
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
            if j >= KDT18DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT18DE.KDT18DEVec(const v: TKDT18DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT18DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT18DE.KDT18DEPow(const v: TKDT18DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT18DE.KDT18DEDistance(const v1, v2: TKDT18DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT18DE_Axis - 1 do
      Result := Result + KDT18DEPow(v2[i] - v1[i]);
end;

procedure TKDT18DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT18DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT18DE.Test;
var
  TKDT18DE_Test: TKDT18DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT18DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT18DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT18DE_Test := TKDT18DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT18DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT18DE_Test.TestBuff) - 1 do
    for j := 0 to KDT18DE_Axis - 1 do
        TKDT18DE_Test.TestBuff[i][j] := i * KDT18DE_Axis + j;

{$IFDEF FPC}
  TKDT18DE_Test.BuildKDTreeM(length(TKDT18DE_Test.TestBuff), nil, @TKDT18DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT18DE_Test.BuildKDTreeM(length(TKDT18DE_Test.TestBuff), nil, TKDT18DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT18DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT18DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT18DE_Test.TestBuff) - 1 do
    begin
      p := TKDT18DE_Test.Search(TKDT18DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT18DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT18DE_Test.TestBuff));
      TKDT18DE_Test.Search(TKDT18DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT18DEDistance(TKDT18DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT18DE_Test.Clear;
      { kMean test }
      TKDT18DE_Test.BuildKDTreeWithCluster(TKDT18DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT18DE_Test.Search(TKDT18DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT18DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT18DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT18DE_Test);
end;


function TKDT19DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT19DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT19DE_Node;
  function SortCompare(const p1, p2: PKDT19DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT19DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT19DE_Source;
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
  kdBuffPtr: PKDT19DE_SourceBuffer;
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
      axis := Depth mod KDT19DE_Axis;
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

function TKDT19DE.GetData(const Index: NativeInt): PKDT19DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT19DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT19DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT19DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT19DE_Node(KDNodes[i]));
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

function TKDT19DE.StoreBuffPtr: PKDT19DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT19DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT19DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT19DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT19DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT19DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT19DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT19DE.BuildKDTreeWithCluster(const inBuff: TKDT19DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT19DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT19DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT19DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT19DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT19DE.BuildKDTreeWithCluster(const inBuff: TKDT19DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT19DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DE_BuildCall);
var
  TempStoreBuff: TKDT19DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT19DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT19DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT19DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT19DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT19DE_Axis - 1 do
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

procedure TKDT19DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DE_BuildMethod);
var
  TempStoreBuff: TKDT19DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT19DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT19DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT19DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT19DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT19DE_Axis - 1 do
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


procedure TKDT19DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DE_BuildProc);
var
  TempStoreBuff: TKDT19DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT19DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT19DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT19DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT19DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT19DE_Axis - 1 do
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


function TKDT19DE.Search(const buff: TKDT19DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT19DE_Node;

var
  NearestNeighbour: PKDT19DE_Node;

  function FindParentNode(const buffPtr: PKDT19DE_Vec; NodePtr: PKDT19DE_Node): PKDT19DE_Node;
  var
    Next: PKDT19DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT19DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT19DE_Node; const buffPtr: PKDT19DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT19DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT19DE_Axis;
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

  function SortCompare(const buffPtr: PKDT19DE_Vec; const p1, p2: PKDT19DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT19DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT19DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT19DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT19DE_Node;
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
  Parent: PKDT19DE_Node;
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

  SearchedDistanceMin := KDT19DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT19DE_Node(NearestNodes[0]);
    end;
end;

function TKDT19DE.Search(const buff: TKDT19DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT19DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT19DE.Search(const buff: TKDT19DE_Vec; var SearchedDistanceMin: Double): PKDT19DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT19DE.Search(const buff: TKDT19DE_Vec): PKDT19DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT19DE.SearchToken(const buff: TKDT19DE_Vec): TPascalString;
var
  p: PKDT19DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT19DE.Search(const inBuff: TKDT19DE_DynamicVecBuffer; var OutBuff: TKDT19DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT19DE_DynamicVecBuffer;
  outBuffPtr: PKDT19DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT19DE_Node;
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
        p: PKDT19DE_Node;
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
  p: PKDT19DE_Node;
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


procedure TKDT19DE.Search(const inBuff: TKDT19DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT19DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT19DE_Node;
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
        p: PKDT19DE_Node;
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
  p: PKDT19DE_Node;
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


procedure TKDT19DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT19DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT19DE_Vec));
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

procedure TKDT19DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT19DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT19DE_Vec)) <> SizeOf(TKDT19DE_Vec) then
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

procedure TKDT19DE.SaveToFile(FileName: SystemString);
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

procedure TKDT19DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT19DE.PrintNodeTree(const NodePtr: PKDT19DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT19DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT19DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT19DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT19DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT19DE.KDT19DEVec(const s: SystemString): TKDT19DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT19DE_Axis - 1 do
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
            if j >= KDT19DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT19DE.KDT19DEVec(const v: TKDT19DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT19DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT19DE.KDT19DEPow(const v: TKDT19DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT19DE.KDT19DEDistance(const v1, v2: TKDT19DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT19DE_Axis - 1 do
      Result := Result + KDT19DEPow(v2[i] - v1[i]);
end;

procedure TKDT19DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT19DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT19DE.Test;
var
  TKDT19DE_Test: TKDT19DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT19DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT19DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT19DE_Test := TKDT19DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT19DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT19DE_Test.TestBuff) - 1 do
    for j := 0 to KDT19DE_Axis - 1 do
        TKDT19DE_Test.TestBuff[i][j] := i * KDT19DE_Axis + j;

{$IFDEF FPC}
  TKDT19DE_Test.BuildKDTreeM(length(TKDT19DE_Test.TestBuff), nil, @TKDT19DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT19DE_Test.BuildKDTreeM(length(TKDT19DE_Test.TestBuff), nil, TKDT19DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT19DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT19DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT19DE_Test.TestBuff) - 1 do
    begin
      p := TKDT19DE_Test.Search(TKDT19DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT19DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT19DE_Test.TestBuff));
      TKDT19DE_Test.Search(TKDT19DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT19DEDistance(TKDT19DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT19DE_Test.Clear;
      { kMean test }
      TKDT19DE_Test.BuildKDTreeWithCluster(TKDT19DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT19DE_Test.Search(TKDT19DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT19DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT19DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT19DE_Test);
end;


function TKDT20DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT20DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT20DE_Node;
  function SortCompare(const p1, p2: PKDT20DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT20DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT20DE_Source;
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
  kdBuffPtr: PKDT20DE_SourceBuffer;
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
      axis := Depth mod KDT20DE_Axis;
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

function TKDT20DE.GetData(const Index: NativeInt): PKDT20DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT20DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT20DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT20DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT20DE_Node(KDNodes[i]));
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

function TKDT20DE.StoreBuffPtr: PKDT20DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT20DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT20DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT20DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT20DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT20DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT20DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT20DE.BuildKDTreeWithCluster(const inBuff: TKDT20DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT20DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT20DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT20DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT20DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT20DE.BuildKDTreeWithCluster(const inBuff: TKDT20DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT20DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DE_BuildCall);
var
  TempStoreBuff: TKDT20DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT20DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT20DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT20DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT20DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT20DE_Axis - 1 do
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

procedure TKDT20DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DE_BuildMethod);
var
  TempStoreBuff: TKDT20DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT20DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT20DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT20DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT20DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT20DE_Axis - 1 do
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


procedure TKDT20DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DE_BuildProc);
var
  TempStoreBuff: TKDT20DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT20DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT20DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT20DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT20DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT20DE_Axis - 1 do
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


function TKDT20DE.Search(const buff: TKDT20DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT20DE_Node;

var
  NearestNeighbour: PKDT20DE_Node;

  function FindParentNode(const buffPtr: PKDT20DE_Vec; NodePtr: PKDT20DE_Node): PKDT20DE_Node;
  var
    Next: PKDT20DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT20DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT20DE_Node; const buffPtr: PKDT20DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT20DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT20DE_Axis;
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

  function SortCompare(const buffPtr: PKDT20DE_Vec; const p1, p2: PKDT20DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT20DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT20DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT20DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT20DE_Node;
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
  Parent: PKDT20DE_Node;
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

  SearchedDistanceMin := KDT20DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT20DE_Node(NearestNodes[0]);
    end;
end;

function TKDT20DE.Search(const buff: TKDT20DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT20DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT20DE.Search(const buff: TKDT20DE_Vec; var SearchedDistanceMin: Double): PKDT20DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT20DE.Search(const buff: TKDT20DE_Vec): PKDT20DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT20DE.SearchToken(const buff: TKDT20DE_Vec): TPascalString;
var
  p: PKDT20DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT20DE.Search(const inBuff: TKDT20DE_DynamicVecBuffer; var OutBuff: TKDT20DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT20DE_DynamicVecBuffer;
  outBuffPtr: PKDT20DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT20DE_Node;
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
        p: PKDT20DE_Node;
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
  p: PKDT20DE_Node;
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


procedure TKDT20DE.Search(const inBuff: TKDT20DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT20DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT20DE_Node;
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
        p: PKDT20DE_Node;
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
  p: PKDT20DE_Node;
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


procedure TKDT20DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT20DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT20DE_Vec));
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

procedure TKDT20DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT20DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT20DE_Vec)) <> SizeOf(TKDT20DE_Vec) then
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

procedure TKDT20DE.SaveToFile(FileName: SystemString);
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

procedure TKDT20DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT20DE.PrintNodeTree(const NodePtr: PKDT20DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT20DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT20DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT20DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT20DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT20DE.KDT20DEVec(const s: SystemString): TKDT20DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT20DE_Axis - 1 do
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
            if j >= KDT20DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT20DE.KDT20DEVec(const v: TKDT20DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT20DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT20DE.KDT20DEPow(const v: TKDT20DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT20DE.KDT20DEDistance(const v1, v2: TKDT20DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT20DE_Axis - 1 do
      Result := Result + KDT20DEPow(v2[i] - v1[i]);
end;

procedure TKDT20DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT20DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT20DE.Test;
var
  TKDT20DE_Test: TKDT20DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT20DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT20DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT20DE_Test := TKDT20DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT20DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT20DE_Test.TestBuff) - 1 do
    for j := 0 to KDT20DE_Axis - 1 do
        TKDT20DE_Test.TestBuff[i][j] := i * KDT20DE_Axis + j;

{$IFDEF FPC}
  TKDT20DE_Test.BuildKDTreeM(length(TKDT20DE_Test.TestBuff), nil, @TKDT20DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT20DE_Test.BuildKDTreeM(length(TKDT20DE_Test.TestBuff), nil, TKDT20DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT20DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT20DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT20DE_Test.TestBuff) - 1 do
    begin
      p := TKDT20DE_Test.Search(TKDT20DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT20DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT20DE_Test.TestBuff));
      TKDT20DE_Test.Search(TKDT20DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT20DEDistance(TKDT20DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT20DE_Test.Clear;
      { kMean test }
      TKDT20DE_Test.BuildKDTreeWithCluster(TKDT20DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT20DE_Test.Search(TKDT20DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT20DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT20DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT20DE_Test);
end;


function TKDT21DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT21DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT21DE_Node;
  function SortCompare(const p1, p2: PKDT21DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT21DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT21DE_Source;
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
  kdBuffPtr: PKDT21DE_SourceBuffer;
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
      axis := Depth mod KDT21DE_Axis;
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

function TKDT21DE.GetData(const Index: NativeInt): PKDT21DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT21DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT21DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT21DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT21DE_Node(KDNodes[i]));
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

function TKDT21DE.StoreBuffPtr: PKDT21DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT21DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT21DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT21DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT21DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT21DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT21DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT21DE.BuildKDTreeWithCluster(const inBuff: TKDT21DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT21DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT21DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT21DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT21DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT21DE.BuildKDTreeWithCluster(const inBuff: TKDT21DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT21DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DE_BuildCall);
var
  TempStoreBuff: TKDT21DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT21DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT21DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT21DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT21DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT21DE_Axis - 1 do
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

procedure TKDT21DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DE_BuildMethod);
var
  TempStoreBuff: TKDT21DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT21DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT21DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT21DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT21DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT21DE_Axis - 1 do
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


procedure TKDT21DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DE_BuildProc);
var
  TempStoreBuff: TKDT21DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT21DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT21DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT21DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT21DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT21DE_Axis - 1 do
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


function TKDT21DE.Search(const buff: TKDT21DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT21DE_Node;

var
  NearestNeighbour: PKDT21DE_Node;

  function FindParentNode(const buffPtr: PKDT21DE_Vec; NodePtr: PKDT21DE_Node): PKDT21DE_Node;
  var
    Next: PKDT21DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT21DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT21DE_Node; const buffPtr: PKDT21DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT21DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT21DE_Axis;
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

  function SortCompare(const buffPtr: PKDT21DE_Vec; const p1, p2: PKDT21DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT21DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT21DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT21DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT21DE_Node;
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
  Parent: PKDT21DE_Node;
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

  SearchedDistanceMin := KDT21DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT21DE_Node(NearestNodes[0]);
    end;
end;

function TKDT21DE.Search(const buff: TKDT21DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT21DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT21DE.Search(const buff: TKDT21DE_Vec; var SearchedDistanceMin: Double): PKDT21DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT21DE.Search(const buff: TKDT21DE_Vec): PKDT21DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT21DE.SearchToken(const buff: TKDT21DE_Vec): TPascalString;
var
  p: PKDT21DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT21DE.Search(const inBuff: TKDT21DE_DynamicVecBuffer; var OutBuff: TKDT21DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT21DE_DynamicVecBuffer;
  outBuffPtr: PKDT21DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT21DE_Node;
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
        p: PKDT21DE_Node;
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
  p: PKDT21DE_Node;
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


procedure TKDT21DE.Search(const inBuff: TKDT21DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT21DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT21DE_Node;
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
        p: PKDT21DE_Node;
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
  p: PKDT21DE_Node;
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


procedure TKDT21DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT21DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT21DE_Vec));
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

procedure TKDT21DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT21DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT21DE_Vec)) <> SizeOf(TKDT21DE_Vec) then
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

procedure TKDT21DE.SaveToFile(FileName: SystemString);
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

procedure TKDT21DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT21DE.PrintNodeTree(const NodePtr: PKDT21DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT21DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT21DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT21DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT21DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT21DE.KDT21DEVec(const s: SystemString): TKDT21DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT21DE_Axis - 1 do
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
            if j >= KDT21DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT21DE.KDT21DEVec(const v: TKDT21DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT21DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT21DE.KDT21DEPow(const v: TKDT21DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT21DE.KDT21DEDistance(const v1, v2: TKDT21DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT21DE_Axis - 1 do
      Result := Result + KDT21DEPow(v2[i] - v1[i]);
end;

procedure TKDT21DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT21DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT21DE.Test;
var
  TKDT21DE_Test: TKDT21DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT21DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT21DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT21DE_Test := TKDT21DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT21DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT21DE_Test.TestBuff) - 1 do
    for j := 0 to KDT21DE_Axis - 1 do
        TKDT21DE_Test.TestBuff[i][j] := i * KDT21DE_Axis + j;

{$IFDEF FPC}
  TKDT21DE_Test.BuildKDTreeM(length(TKDT21DE_Test.TestBuff), nil, @TKDT21DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT21DE_Test.BuildKDTreeM(length(TKDT21DE_Test.TestBuff), nil, TKDT21DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT21DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT21DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT21DE_Test.TestBuff) - 1 do
    begin
      p := TKDT21DE_Test.Search(TKDT21DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT21DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT21DE_Test.TestBuff));
      TKDT21DE_Test.Search(TKDT21DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT21DEDistance(TKDT21DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT21DE_Test.Clear;
      { kMean test }
      TKDT21DE_Test.BuildKDTreeWithCluster(TKDT21DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT21DE_Test.Search(TKDT21DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT21DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT21DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT21DE_Test);
end;


function TKDT22DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT22DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT22DE_Node;
  function SortCompare(const p1, p2: PKDT22DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT22DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT22DE_Source;
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
  kdBuffPtr: PKDT22DE_SourceBuffer;
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
      axis := Depth mod KDT22DE_Axis;
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

function TKDT22DE.GetData(const Index: NativeInt): PKDT22DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT22DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT22DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT22DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT22DE_Node(KDNodes[i]));
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

function TKDT22DE.StoreBuffPtr: PKDT22DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT22DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT22DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT22DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT22DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT22DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT22DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT22DE.BuildKDTreeWithCluster(const inBuff: TKDT22DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT22DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT22DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT22DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT22DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT22DE.BuildKDTreeWithCluster(const inBuff: TKDT22DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT22DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DE_BuildCall);
var
  TempStoreBuff: TKDT22DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT22DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT22DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT22DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT22DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT22DE_Axis - 1 do
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

procedure TKDT22DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DE_BuildMethod);
var
  TempStoreBuff: TKDT22DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT22DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT22DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT22DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT22DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT22DE_Axis - 1 do
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


procedure TKDT22DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DE_BuildProc);
var
  TempStoreBuff: TKDT22DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT22DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT22DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT22DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT22DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT22DE_Axis - 1 do
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


function TKDT22DE.Search(const buff: TKDT22DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT22DE_Node;

var
  NearestNeighbour: PKDT22DE_Node;

  function FindParentNode(const buffPtr: PKDT22DE_Vec; NodePtr: PKDT22DE_Node): PKDT22DE_Node;
  var
    Next: PKDT22DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT22DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT22DE_Node; const buffPtr: PKDT22DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT22DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT22DE_Axis;
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

  function SortCompare(const buffPtr: PKDT22DE_Vec; const p1, p2: PKDT22DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT22DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT22DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT22DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT22DE_Node;
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
  Parent: PKDT22DE_Node;
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

  SearchedDistanceMin := KDT22DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT22DE_Node(NearestNodes[0]);
    end;
end;

function TKDT22DE.Search(const buff: TKDT22DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT22DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT22DE.Search(const buff: TKDT22DE_Vec; var SearchedDistanceMin: Double): PKDT22DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT22DE.Search(const buff: TKDT22DE_Vec): PKDT22DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT22DE.SearchToken(const buff: TKDT22DE_Vec): TPascalString;
var
  p: PKDT22DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT22DE.Search(const inBuff: TKDT22DE_DynamicVecBuffer; var OutBuff: TKDT22DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT22DE_DynamicVecBuffer;
  outBuffPtr: PKDT22DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT22DE_Node;
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
        p: PKDT22DE_Node;
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
  p: PKDT22DE_Node;
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


procedure TKDT22DE.Search(const inBuff: TKDT22DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT22DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT22DE_Node;
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
        p: PKDT22DE_Node;
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
  p: PKDT22DE_Node;
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


procedure TKDT22DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT22DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT22DE_Vec));
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

procedure TKDT22DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT22DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT22DE_Vec)) <> SizeOf(TKDT22DE_Vec) then
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

procedure TKDT22DE.SaveToFile(FileName: SystemString);
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

procedure TKDT22DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT22DE.PrintNodeTree(const NodePtr: PKDT22DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT22DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT22DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT22DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT22DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT22DE.KDT22DEVec(const s: SystemString): TKDT22DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT22DE_Axis - 1 do
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
            if j >= KDT22DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT22DE.KDT22DEVec(const v: TKDT22DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT22DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT22DE.KDT22DEPow(const v: TKDT22DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT22DE.KDT22DEDistance(const v1, v2: TKDT22DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT22DE_Axis - 1 do
      Result := Result + KDT22DEPow(v2[i] - v1[i]);
end;

procedure TKDT22DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT22DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT22DE.Test;
var
  TKDT22DE_Test: TKDT22DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT22DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT22DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT22DE_Test := TKDT22DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT22DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT22DE_Test.TestBuff) - 1 do
    for j := 0 to KDT22DE_Axis - 1 do
        TKDT22DE_Test.TestBuff[i][j] := i * KDT22DE_Axis + j;

{$IFDEF FPC}
  TKDT22DE_Test.BuildKDTreeM(length(TKDT22DE_Test.TestBuff), nil, @TKDT22DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT22DE_Test.BuildKDTreeM(length(TKDT22DE_Test.TestBuff), nil, TKDT22DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT22DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT22DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT22DE_Test.TestBuff) - 1 do
    begin
      p := TKDT22DE_Test.Search(TKDT22DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT22DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT22DE_Test.TestBuff));
      TKDT22DE_Test.Search(TKDT22DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT22DEDistance(TKDT22DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT22DE_Test.Clear;
      { kMean test }
      TKDT22DE_Test.BuildKDTreeWithCluster(TKDT22DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT22DE_Test.Search(TKDT22DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT22DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT22DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT22DE_Test);
end;


function TKDT23DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT23DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT23DE_Node;
  function SortCompare(const p1, p2: PKDT23DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT23DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT23DE_Source;
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
  kdBuffPtr: PKDT23DE_SourceBuffer;
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
      axis := Depth mod KDT23DE_Axis;
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

function TKDT23DE.GetData(const Index: NativeInt): PKDT23DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT23DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT23DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT23DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT23DE_Node(KDNodes[i]));
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

function TKDT23DE.StoreBuffPtr: PKDT23DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT23DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT23DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT23DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT23DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT23DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT23DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT23DE.BuildKDTreeWithCluster(const inBuff: TKDT23DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT23DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT23DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT23DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT23DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT23DE.BuildKDTreeWithCluster(const inBuff: TKDT23DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT23DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DE_BuildCall);
var
  TempStoreBuff: TKDT23DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT23DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT23DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT23DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT23DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT23DE_Axis - 1 do
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

procedure TKDT23DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DE_BuildMethod);
var
  TempStoreBuff: TKDT23DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT23DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT23DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT23DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT23DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT23DE_Axis - 1 do
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


procedure TKDT23DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DE_BuildProc);
var
  TempStoreBuff: TKDT23DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT23DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT23DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT23DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT23DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT23DE_Axis - 1 do
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


function TKDT23DE.Search(const buff: TKDT23DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT23DE_Node;

var
  NearestNeighbour: PKDT23DE_Node;

  function FindParentNode(const buffPtr: PKDT23DE_Vec; NodePtr: PKDT23DE_Node): PKDT23DE_Node;
  var
    Next: PKDT23DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT23DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT23DE_Node; const buffPtr: PKDT23DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT23DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT23DE_Axis;
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

  function SortCompare(const buffPtr: PKDT23DE_Vec; const p1, p2: PKDT23DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT23DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT23DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT23DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT23DE_Node;
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
  Parent: PKDT23DE_Node;
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

  SearchedDistanceMin := KDT23DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT23DE_Node(NearestNodes[0]);
    end;
end;

function TKDT23DE.Search(const buff: TKDT23DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT23DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT23DE.Search(const buff: TKDT23DE_Vec; var SearchedDistanceMin: Double): PKDT23DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT23DE.Search(const buff: TKDT23DE_Vec): PKDT23DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT23DE.SearchToken(const buff: TKDT23DE_Vec): TPascalString;
var
  p: PKDT23DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT23DE.Search(const inBuff: TKDT23DE_DynamicVecBuffer; var OutBuff: TKDT23DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT23DE_DynamicVecBuffer;
  outBuffPtr: PKDT23DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT23DE_Node;
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
        p: PKDT23DE_Node;
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
  p: PKDT23DE_Node;
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


procedure TKDT23DE.Search(const inBuff: TKDT23DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT23DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT23DE_Node;
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
        p: PKDT23DE_Node;
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
  p: PKDT23DE_Node;
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


procedure TKDT23DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT23DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT23DE_Vec));
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

procedure TKDT23DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT23DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT23DE_Vec)) <> SizeOf(TKDT23DE_Vec) then
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

procedure TKDT23DE.SaveToFile(FileName: SystemString);
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

procedure TKDT23DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT23DE.PrintNodeTree(const NodePtr: PKDT23DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT23DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT23DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT23DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT23DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT23DE.KDT23DEVec(const s: SystemString): TKDT23DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT23DE_Axis - 1 do
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
            if j >= KDT23DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT23DE.KDT23DEVec(const v: TKDT23DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT23DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT23DE.KDT23DEPow(const v: TKDT23DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT23DE.KDT23DEDistance(const v1, v2: TKDT23DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT23DE_Axis - 1 do
      Result := Result + KDT23DEPow(v2[i] - v1[i]);
end;

procedure TKDT23DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT23DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT23DE.Test;
var
  TKDT23DE_Test: TKDT23DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT23DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT23DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT23DE_Test := TKDT23DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT23DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT23DE_Test.TestBuff) - 1 do
    for j := 0 to KDT23DE_Axis - 1 do
        TKDT23DE_Test.TestBuff[i][j] := i * KDT23DE_Axis + j;

{$IFDEF FPC}
  TKDT23DE_Test.BuildKDTreeM(length(TKDT23DE_Test.TestBuff), nil, @TKDT23DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT23DE_Test.BuildKDTreeM(length(TKDT23DE_Test.TestBuff), nil, TKDT23DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT23DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT23DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT23DE_Test.TestBuff) - 1 do
    begin
      p := TKDT23DE_Test.Search(TKDT23DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT23DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT23DE_Test.TestBuff));
      TKDT23DE_Test.Search(TKDT23DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT23DEDistance(TKDT23DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT23DE_Test.Clear;
      { kMean test }
      TKDT23DE_Test.BuildKDTreeWithCluster(TKDT23DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT23DE_Test.Search(TKDT23DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT23DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT23DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT23DE_Test);
end;


function TKDT24DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT24DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT24DE_Node;
  function SortCompare(const p1, p2: PKDT24DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT24DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT24DE_Source;
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
  kdBuffPtr: PKDT24DE_SourceBuffer;
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
      axis := Depth mod KDT24DE_Axis;
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

function TKDT24DE.GetData(const Index: NativeInt): PKDT24DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT24DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT24DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT24DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT24DE_Node(KDNodes[i]));
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

function TKDT24DE.StoreBuffPtr: PKDT24DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT24DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT24DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT24DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT24DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT24DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT24DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT24DE.BuildKDTreeWithCluster(const inBuff: TKDT24DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT24DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT24DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT24DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT24DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT24DE.BuildKDTreeWithCluster(const inBuff: TKDT24DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT24DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DE_BuildCall);
var
  TempStoreBuff: TKDT24DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT24DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT24DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT24DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT24DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT24DE_Axis - 1 do
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

procedure TKDT24DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DE_BuildMethod);
var
  TempStoreBuff: TKDT24DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT24DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT24DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT24DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT24DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT24DE_Axis - 1 do
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


procedure TKDT24DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DE_BuildProc);
var
  TempStoreBuff: TKDT24DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT24DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT24DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT24DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT24DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT24DE_Axis - 1 do
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


function TKDT24DE.Search(const buff: TKDT24DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT24DE_Node;

var
  NearestNeighbour: PKDT24DE_Node;

  function FindParentNode(const buffPtr: PKDT24DE_Vec; NodePtr: PKDT24DE_Node): PKDT24DE_Node;
  var
    Next: PKDT24DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT24DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT24DE_Node; const buffPtr: PKDT24DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT24DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT24DE_Axis;
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

  function SortCompare(const buffPtr: PKDT24DE_Vec; const p1, p2: PKDT24DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT24DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT24DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT24DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT24DE_Node;
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
  Parent: PKDT24DE_Node;
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

  SearchedDistanceMin := KDT24DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT24DE_Node(NearestNodes[0]);
    end;
end;

function TKDT24DE.Search(const buff: TKDT24DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT24DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT24DE.Search(const buff: TKDT24DE_Vec; var SearchedDistanceMin: Double): PKDT24DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT24DE.Search(const buff: TKDT24DE_Vec): PKDT24DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT24DE.SearchToken(const buff: TKDT24DE_Vec): TPascalString;
var
  p: PKDT24DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT24DE.Search(const inBuff: TKDT24DE_DynamicVecBuffer; var OutBuff: TKDT24DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT24DE_DynamicVecBuffer;
  outBuffPtr: PKDT24DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT24DE_Node;
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
        p: PKDT24DE_Node;
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
  p: PKDT24DE_Node;
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


procedure TKDT24DE.Search(const inBuff: TKDT24DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT24DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT24DE_Node;
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
        p: PKDT24DE_Node;
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
  p: PKDT24DE_Node;
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


procedure TKDT24DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT24DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT24DE_Vec));
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

procedure TKDT24DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT24DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT24DE_Vec)) <> SizeOf(TKDT24DE_Vec) then
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

procedure TKDT24DE.SaveToFile(FileName: SystemString);
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

procedure TKDT24DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT24DE.PrintNodeTree(const NodePtr: PKDT24DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT24DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT24DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT24DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT24DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT24DE.KDT24DEVec(const s: SystemString): TKDT24DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT24DE_Axis - 1 do
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
            if j >= KDT24DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT24DE.KDT24DEVec(const v: TKDT24DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT24DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT24DE.KDT24DEPow(const v: TKDT24DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT24DE.KDT24DEDistance(const v1, v2: TKDT24DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT24DE_Axis - 1 do
      Result := Result + KDT24DEPow(v2[i] - v1[i]);
end;

procedure TKDT24DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT24DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT24DE.Test;
var
  TKDT24DE_Test: TKDT24DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT24DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT24DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT24DE_Test := TKDT24DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT24DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT24DE_Test.TestBuff) - 1 do
    for j := 0 to KDT24DE_Axis - 1 do
        TKDT24DE_Test.TestBuff[i][j] := i * KDT24DE_Axis + j;

{$IFDEF FPC}
  TKDT24DE_Test.BuildKDTreeM(length(TKDT24DE_Test.TestBuff), nil, @TKDT24DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT24DE_Test.BuildKDTreeM(length(TKDT24DE_Test.TestBuff), nil, TKDT24DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT24DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT24DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT24DE_Test.TestBuff) - 1 do
    begin
      p := TKDT24DE_Test.Search(TKDT24DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT24DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT24DE_Test.TestBuff));
      TKDT24DE_Test.Search(TKDT24DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT24DEDistance(TKDT24DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT24DE_Test.Clear;
      { kMean test }
      TKDT24DE_Test.BuildKDTreeWithCluster(TKDT24DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT24DE_Test.Search(TKDT24DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT24DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT24DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT24DE_Test);
end;


function TKDT256DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT256DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT256DE_Node;
  function SortCompare(const p1, p2: PKDT256DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT256DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT256DE_Source;
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
  kdBuffPtr: PKDT256DE_SourceBuffer;
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
      axis := Depth mod KDT256DE_Axis;
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

function TKDT256DE.GetData(const Index: NativeInt): PKDT256DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT256DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT256DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT256DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT256DE_Node(KDNodes[i]));
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

function TKDT256DE.StoreBuffPtr: PKDT256DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT256DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT256DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT256DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT256DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT256DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT256DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT256DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT256DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT256DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT256DE.BuildKDTreeWithCluster(const inBuff: TKDT256DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT256DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT256DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT256DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT256DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT256DE.BuildKDTreeWithCluster(const inBuff: TKDT256DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT256DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT256DE_BuildCall);
var
  TempStoreBuff: TKDT256DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT256DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT256DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT256DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT256DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT256DE_Axis - 1 do
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

procedure TKDT256DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT256DE_BuildMethod);
var
  TempStoreBuff: TKDT256DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT256DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT256DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT256DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT256DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT256DE_Axis - 1 do
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


procedure TKDT256DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT256DE_BuildProc);
var
  TempStoreBuff: TKDT256DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT256DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT256DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT256DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT256DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT256DE_Axis - 1 do
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


function TKDT256DE.Search(const buff: TKDT256DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT256DE_Node;

var
  NearestNeighbour: PKDT256DE_Node;

  function FindParentNode(const buffPtr: PKDT256DE_Vec; NodePtr: PKDT256DE_Node): PKDT256DE_Node;
  var
    Next: PKDT256DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT256DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT256DE_Node; const buffPtr: PKDT256DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT256DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT256DE_Axis;
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

  function SortCompare(const buffPtr: PKDT256DE_Vec; const p1, p2: PKDT256DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT256DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT256DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT256DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT256DE_Node;
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
  Parent: PKDT256DE_Node;
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

  SearchedDistanceMin := KDT256DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT256DE_Node(NearestNodes[0]);
    end;
end;

function TKDT256DE.Search(const buff: TKDT256DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT256DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT256DE.Search(const buff: TKDT256DE_Vec; var SearchedDistanceMin: Double): PKDT256DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT256DE.Search(const buff: TKDT256DE_Vec): PKDT256DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT256DE.SearchToken(const buff: TKDT256DE_Vec): TPascalString;
var
  p: PKDT256DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT256DE.Search(const inBuff: TKDT256DE_DynamicVecBuffer; var OutBuff: TKDT256DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT256DE_DynamicVecBuffer;
  outBuffPtr: PKDT256DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT256DE_Node;
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
        p: PKDT256DE_Node;
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
  p: PKDT256DE_Node;
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


procedure TKDT256DE.Search(const inBuff: TKDT256DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT256DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT256DE_Node;
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
        p: PKDT256DE_Node;
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
  p: PKDT256DE_Node;
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


procedure TKDT256DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT256DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT256DE_Vec));
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

procedure TKDT256DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT256DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT256DE_Vec)) <> SizeOf(TKDT256DE_Vec) then
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

procedure TKDT256DE.SaveToFile(FileName: SystemString);
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

procedure TKDT256DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT256DE.PrintNodeTree(const NodePtr: PKDT256DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT256DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT256DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT256DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT256DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT256DE.KDT256DEVec(const s: SystemString): TKDT256DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT256DE_Axis - 1 do
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
            if j >= KDT256DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT256DE.KDT256DEVec(const v: TKDT256DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT256DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT256DE.KDT256DEPow(const v: TKDT256DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT256DE.KDT256DEDistance(const v1, v2: TKDT256DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT256DE_Axis - 1 do
      Result := Result + KDT256DEPow(v2[i] - v1[i]);
end;

procedure TKDT256DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT256DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT256DE.Test;
var
  TKDT256DE_Test: TKDT256DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT256DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT256DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT256DE_Test := TKDT256DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT256DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT256DE_Test.TestBuff) - 1 do
    for j := 0 to KDT256DE_Axis - 1 do
        TKDT256DE_Test.TestBuff[i][j] := i * KDT256DE_Axis + j;

{$IFDEF FPC}
  TKDT256DE_Test.BuildKDTreeM(length(TKDT256DE_Test.TestBuff), nil, @TKDT256DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT256DE_Test.BuildKDTreeM(length(TKDT256DE_Test.TestBuff), nil, TKDT256DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT256DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT256DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT256DE_Test.TestBuff) - 1 do
    begin
      p := TKDT256DE_Test.Search(TKDT256DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT256DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT256DE_Test.TestBuff));
      TKDT256DE_Test.Search(TKDT256DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT256DEDistance(TKDT256DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT256DE_Test.Clear;
      { kMean test }
      TKDT256DE_Test.BuildKDTreeWithCluster(TKDT256DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT256DE_Test.Search(TKDT256DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT256DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT256DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT256DE_Test);
end;


function TKDT512DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT512DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT512DE_Node;
  function SortCompare(const p1, p2: PKDT512DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT512DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT512DE_Source;
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
  kdBuffPtr: PKDT512DE_SourceBuffer;
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
      axis := Depth mod KDT512DE_Axis;
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

function TKDT512DE.GetData(const Index: NativeInt): PKDT512DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT512DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT512DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT512DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT512DE_Node(KDNodes[i]));
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

function TKDT512DE.StoreBuffPtr: PKDT512DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT512DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT512DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT512DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT512DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT512DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT512DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT512DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT512DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT512DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT512DE.BuildKDTreeWithCluster(const inBuff: TKDT512DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT512DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT512DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT512DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT512DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT512DE.BuildKDTreeWithCluster(const inBuff: TKDT512DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT512DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT512DE_BuildCall);
var
  TempStoreBuff: TKDT512DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT512DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT512DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT512DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT512DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT512DE_Axis - 1 do
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

procedure TKDT512DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT512DE_BuildMethod);
var
  TempStoreBuff: TKDT512DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT512DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT512DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT512DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT512DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT512DE_Axis - 1 do
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


procedure TKDT512DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT512DE_BuildProc);
var
  TempStoreBuff: TKDT512DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT512DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT512DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT512DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT512DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT512DE_Axis - 1 do
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


function TKDT512DE.Search(const buff: TKDT512DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT512DE_Node;

var
  NearestNeighbour: PKDT512DE_Node;

  function FindParentNode(const buffPtr: PKDT512DE_Vec; NodePtr: PKDT512DE_Node): PKDT512DE_Node;
  var
    Next: PKDT512DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT512DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT512DE_Node; const buffPtr: PKDT512DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT512DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT512DE_Axis;
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

  function SortCompare(const buffPtr: PKDT512DE_Vec; const p1, p2: PKDT512DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT512DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT512DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT512DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT512DE_Node;
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
  Parent: PKDT512DE_Node;
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

  SearchedDistanceMin := KDT512DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT512DE_Node(NearestNodes[0]);
    end;
end;

function TKDT512DE.Search(const buff: TKDT512DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT512DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT512DE.Search(const buff: TKDT512DE_Vec; var SearchedDistanceMin: Double): PKDT512DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT512DE.Search(const buff: TKDT512DE_Vec): PKDT512DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT512DE.SearchToken(const buff: TKDT512DE_Vec): TPascalString;
var
  p: PKDT512DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT512DE.Search(const inBuff: TKDT512DE_DynamicVecBuffer; var OutBuff: TKDT512DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT512DE_DynamicVecBuffer;
  outBuffPtr: PKDT512DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT512DE_Node;
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
        p: PKDT512DE_Node;
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
  p: PKDT512DE_Node;
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


procedure TKDT512DE.Search(const inBuff: TKDT512DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT512DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT512DE_Node;
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
        p: PKDT512DE_Node;
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
  p: PKDT512DE_Node;
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


procedure TKDT512DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT512DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT512DE_Vec));
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

procedure TKDT512DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT512DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT512DE_Vec)) <> SizeOf(TKDT512DE_Vec) then
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

procedure TKDT512DE.SaveToFile(FileName: SystemString);
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

procedure TKDT512DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT512DE.PrintNodeTree(const NodePtr: PKDT512DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT512DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT512DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT512DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT512DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT512DE.KDT512DEVec(const s: SystemString): TKDT512DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT512DE_Axis - 1 do
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
            if j >= KDT512DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT512DE.KDT512DEVec(const v: TKDT512DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT512DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT512DE.KDT512DEPow(const v: TKDT512DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT512DE.KDT512DEDistance(const v1, v2: TKDT512DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT512DE_Axis - 1 do
      Result := Result + KDT512DEPow(v2[i] - v1[i]);
end;

procedure TKDT512DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT512DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT512DE.Test;
var
  TKDT512DE_Test: TKDT512DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT512DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT512DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT512DE_Test := TKDT512DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT512DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT512DE_Test.TestBuff) - 1 do
    for j := 0 to KDT512DE_Axis - 1 do
        TKDT512DE_Test.TestBuff[i][j] := i * KDT512DE_Axis + j;

{$IFDEF FPC}
  TKDT512DE_Test.BuildKDTreeM(length(TKDT512DE_Test.TestBuff), nil, @TKDT512DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT512DE_Test.BuildKDTreeM(length(TKDT512DE_Test.TestBuff), nil, TKDT512DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT512DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT512DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT512DE_Test.TestBuff) - 1 do
    begin
      p := TKDT512DE_Test.Search(TKDT512DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT512DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT512DE_Test.TestBuff));
      TKDT512DE_Test.Search(TKDT512DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT512DEDistance(TKDT512DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT512DE_Test.Clear;
      { kMean test }
      TKDT512DE_Test.BuildKDTreeWithCluster(TKDT512DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT512DE_Test.Search(TKDT512DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT512DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT512DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT512DE_Test);
end;


function TKDT1024DE.InternalBuildKdTree(const KDSourceBufferPtr: PKDT1024DE_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT1024DE_Node;
  function SortCompare(const p1, p2: PKDT1024DE_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT1024DE_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT1024DE_Source;
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
  kdBuffPtr: PKDT1024DE_SourceBuffer;
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
      axis := Depth mod KDT1024DE_Axis;
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

function TKDT1024DE.GetData(const Index: NativeInt): PKDT1024DE_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT1024DE.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT1024DE.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT1024DE.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT1024DE_Node(KDNodes[i]));
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

function TKDT1024DE.StoreBuffPtr: PKDT1024DE_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT1024DE.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1024DE_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT1024DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT1024DE.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1024DE_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT1024DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT1024DE.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1024DE_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT1024DE_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT1024DE.BuildKDTreeWithCluster(const inBuff: TKDT1024DE_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT1024DE_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT1024DE_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT1024DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1024DE_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT1024DE.BuildKDTreeWithCluster(const inBuff: TKDT1024DE_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT1024DE.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1024DE_BuildCall);
var
  TempStoreBuff: TKDT1024DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT1024DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT1024DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT1024DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT1024DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1024DE_Axis - 1 do
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

procedure TKDT1024DE.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1024DE_BuildMethod);
var
  TempStoreBuff: TKDT1024DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT1024DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT1024DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT1024DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT1024DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1024DE_Axis - 1 do
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


procedure TKDT1024DE.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1024DE_BuildProc);
var
  TempStoreBuff: TKDT1024DE_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT1024DE_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT1024DE_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT1024DE_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT1024DE_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1024DE_Axis - 1 do
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


function TKDT1024DE.Search(const buff: TKDT1024DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT1024DE_Node;

var
  NearestNeighbour: PKDT1024DE_Node;

  function FindParentNode(const buffPtr: PKDT1024DE_Vec; NodePtr: PKDT1024DE_Node): PKDT1024DE_Node;
  var
    Next: PKDT1024DE_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT1024DE_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT1024DE_Node; const buffPtr: PKDT1024DE_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT1024DEDistance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT1024DE_Axis;
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

  function SortCompare(const buffPtr: PKDT1024DE_Vec; const p1, p2: PKDT1024DE_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT1024DEDistance(buffPtr^, p1^.vec^.buff);
    d2 := KDT1024DEDistance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT1024DE_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT1024DE_Node;
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
  Parent: PKDT1024DE_Node;
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

  SearchedDistanceMin := KDT1024DEDistance(buff, Parent^.vec^.buff);

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
          Result := PKDT1024DE_Node(NearestNodes[0]);
    end;
end;

function TKDT1024DE.Search(const buff: TKDT1024DE_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT1024DE_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT1024DE.Search(const buff: TKDT1024DE_Vec; var SearchedDistanceMin: Double): PKDT1024DE_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT1024DE.Search(const buff: TKDT1024DE_Vec): PKDT1024DE_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT1024DE.SearchToken(const buff: TKDT1024DE_Vec): TPascalString;
var
  p: PKDT1024DE_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT1024DE.Search(const inBuff: TKDT1024DE_DynamicVecBuffer; var OutBuff: TKDT1024DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT1024DE_DynamicVecBuffer;
  outBuffPtr: PKDT1024DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT1024DE_Node;
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
        p: PKDT1024DE_Node;
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
  p: PKDT1024DE_Node;
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


procedure TKDT1024DE.Search(const inBuff: TKDT1024DE_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT1024DE_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT1024DE_Node;
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
        p: PKDT1024DE_Node;
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
  p: PKDT1024DE_Node;
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


procedure TKDT1024DE.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT1024DE_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT1024DE_Vec));
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

procedure TKDT1024DE.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT1024DE_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT1024DE_Vec)) <> SizeOf(TKDT1024DE_Vec) then
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

procedure TKDT1024DE.SaveToFile(FileName: SystemString);
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

procedure TKDT1024DE.LoadFromFile(FileName: SystemString);
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

procedure TKDT1024DE.PrintNodeTree(const NodePtr: PKDT1024DE_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT1024DE_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT1024DEVec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT1024DE.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT1024DEVec(KDStoreBuff[i].buff)]);
end;

class function TKDT1024DE.KDT1024DEVec(const s: SystemString): TKDT1024DE_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT1024DE_Axis - 1 do
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
            if j >= KDT1024DE_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT1024DE.KDT1024DEVec(const v: TKDT1024DE_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT1024DE_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDT1024DE.KDT1024DEPow(const v: TKDT1024DE_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT1024DE.KDT1024DEDistance(const v1, v2: TKDT1024DE_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT1024DE_Axis - 1 do
      Result := Result + KDT1024DEPow(v2[i] - v1[i]);
end;

procedure TKDT1024DE.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT1024DE_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT1024DE.Test;
var
  TKDT1024DE_Test: TKDT1024DE;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT1024DE_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT1024DE_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT1024DE_Test := TKDT1024DE.Create;

  DoStatusNoLn('...');
  SetLength(TKDT1024DE_Test.TestBuff, 1000);
  for i := 0 to length(TKDT1024DE_Test.TestBuff) - 1 do
    for j := 0 to KDT1024DE_Axis - 1 do
        TKDT1024DE_Test.TestBuff[i][j] := i * KDT1024DE_Axis + j;

{$IFDEF FPC}
  TKDT1024DE_Test.BuildKDTreeM(length(TKDT1024DE_Test.TestBuff), nil, @TKDT1024DE_Test.Test_BuildM);
{$ELSE FPC}
  TKDT1024DE_Test.BuildKDTreeM(length(TKDT1024DE_Test.TestBuff), nil, TKDT1024DE_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT1024DE_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT1024DE_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT1024DE_Test.TestBuff) - 1 do
    begin
      p := TKDT1024DE_Test.Search(TKDT1024DE_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT1024DE_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT1024DE_Test.TestBuff));
      TKDT1024DE_Test.Search(TKDT1024DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT1024DEDistance(TKDT1024DE_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT1024DE_Test.Clear;
      { kMean test }
      TKDT1024DE_Test.BuildKDTreeWithCluster(TKDT1024DE_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT1024DE_Test.Search(TKDT1024DE_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT1024DE_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT1024DE_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT1024DE_Test);
end;


procedure Test_All;
begin
  TKDT1DE.Test();
  TKDT2DE.Test();
  TKDT3DE.Test();
  TKDT4DE.Test();
  TKDT5DE.Test();
  TKDT6DE.Test();
  TKDT7DE.Test();
  TKDT8DE.Test();
  TKDT9DE.Test();
  TKDT10DE.Test();
  TKDT11DE.Test();
  TKDT12DE.Test();
  TKDT13DE.Test();
  TKDT14DE.Test();
  TKDT15DE.Test();
  TKDT16DE.Test();
  TKDT17DE.Test();
  TKDT18DE.Test();
  TKDT19DE.Test();
  TKDT20DE.Test();
  TKDT21DE.Test();
  TKDT22DE.Test();
  TKDT23DE.Test();
  TKDT24DE.Test();
  TKDT256DE.Test();
  TKDT512DE.Test();
  TKDT1024DE.Test();
end;





initialization

finalization

end.

