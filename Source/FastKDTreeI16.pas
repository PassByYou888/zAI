{ ****************************************************************************** }
{ Fast KDTree SmallInt type support                                              }
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

unit FastKDTreeI16;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, PascalStrings, UnicodeMixedLib, KM;

const

  // SmallInt KDTree
  KDT1DI16_Axis = 1;
  KDT2DI16_Axis = 2;
  KDT3DI16_Axis = 3;
  KDT4DI16_Axis = 4;
  KDT5DI16_Axis = 5;
  KDT6DI16_Axis = 6;
  KDT7DI16_Axis = 7;
  KDT8DI16_Axis = 8;
  KDT9DI16_Axis = 9;
  KDT10DI16_Axis = 10;
  KDT11DI16_Axis = 11;
  KDT12DI16_Axis = 12;
  KDT13DI16_Axis = 13;
  KDT14DI16_Axis = 14;
  KDT15DI16_Axis = 15;
  KDT16DI16_Axis = 16;
  KDT17DI16_Axis = 17;
  KDT18DI16_Axis = 18;
  KDT19DI16_Axis = 19;
  KDT20DI16_Axis = 20;
  KDT21DI16_Axis = 21;
  KDT22DI16_Axis = 22;
  KDT23DI16_Axis = 23;
  KDT24DI16_Axis = 24;
  KDT256DI16_Axis = 256;
  KDT512DI16_Axis = 512;
  KDT1024DI16_Axis = 1024;

type

  // SmallInt: KDTree
  TKDT1DI16 = class;  TKDT1DI16_VecType = KM.TKMFloat; // 1D
  TKDT2DI16 = class;  TKDT2DI16_VecType = KM.TKMFloat; // 2D
  TKDT3DI16 = class;  TKDT3DI16_VecType = KM.TKMFloat; // 3D
  TKDT4DI16 = class;  TKDT4DI16_VecType = KM.TKMFloat; // 4D
  TKDT5DI16 = class;  TKDT5DI16_VecType = KM.TKMFloat; // 5D
  TKDT6DI16 = class;  TKDT6DI16_VecType = KM.TKMFloat; // 6D
  TKDT7DI16 = class;  TKDT7DI16_VecType = KM.TKMFloat; // 7D
  TKDT8DI16 = class;  TKDT8DI16_VecType = KM.TKMFloat; // 8D
  TKDT9DI16 = class;  TKDT9DI16_VecType = KM.TKMFloat; // 9D
  TKDT10DI16 = class;  TKDT10DI16_VecType = KM.TKMFloat; // 10D
  TKDT11DI16 = class;  TKDT11DI16_VecType = KM.TKMFloat; // 11D
  TKDT12DI16 = class;  TKDT12DI16_VecType = KM.TKMFloat; // 12D
  TKDT13DI16 = class;  TKDT13DI16_VecType = KM.TKMFloat; // 13D
  TKDT14DI16 = class;  TKDT14DI16_VecType = KM.TKMFloat; // 14D
  TKDT15DI16 = class;  TKDT15DI16_VecType = KM.TKMFloat; // 15D
  TKDT16DI16 = class;  TKDT16DI16_VecType = KM.TKMFloat; // 16D
  TKDT17DI16 = class;  TKDT17DI16_VecType = KM.TKMFloat; // 17D
  TKDT18DI16 = class;  TKDT18DI16_VecType = KM.TKMFloat; // 18D
  TKDT19DI16 = class;  TKDT19DI16_VecType = KM.TKMFloat; // 19D
  TKDT20DI16 = class;  TKDT20DI16_VecType = KM.TKMFloat; // 20D
  TKDT21DI16 = class;  TKDT21DI16_VecType = KM.TKMFloat; // 21D
  TKDT22DI16 = class;  TKDT22DI16_VecType = KM.TKMFloat; // 22D
  TKDT23DI16 = class;  TKDT23DI16_VecType = KM.TKMFloat; // 23D
  TKDT24DI16 = class;  TKDT24DI16_VecType = KM.TKMFloat; // 24D
  TKDT256DI16 = class;  TKDT256DI16_VecType = KM.TKMFloat; // 256D
  TKDT512DI16 = class;  TKDT512DI16_VecType = KM.TKMFloat; // 512D
  TKDT1024DI16 = class;  TKDT1024DI16_VecType = KM.TKMFloat; // 1024D










  // SmallInt KDTree


  TKDT1DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT1DI16_Vec = array [0 .. KDT1DI16_Axis - 1] of TKDT1DI16_VecType;
    PKDT1DI16_Vec = ^TKDT1DI16_Vec;

    TKDT1DI16_DynamicVecBuffer = array of TKDT1DI16_Vec;
    PKDT1DI16_DynamicVecBuffer = ^TKDT1DI16_DynamicVecBuffer;

    TKDT1DI16_Source = record
      buff: TKDT1DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT1DI16_Source = ^TKDT1DI16_Source;
    TKDT1DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT1DI16_Source) - 1] of PKDT1DI16_Source;
    PKDT1DI16_SourceBuffer = ^TKDT1DI16_SourceBuffer;

    TKDT1DI16_DyanmicSourceBuffer = array of PKDT1DI16_Source;
    PKDT1DI16_DyanmicSourceBuffer = ^TKDT1DI16_DyanmicSourceBuffer;

    TKDT1DI16_DyanmicStoreBuffer = array of TKDT1DI16_Source;
    PKDT1DI16_DyanmicStoreBuffer = ^TKDT1DI16_DyanmicStoreBuffer;

    PKDT1DI16_Node = ^TKDT1DI16_Node;

    TKDT1DI16_Node = record
      Parent, Right, Left: PKDT1DI16_Node;
      vec: PKDT1DI16_Source;
    end;

    TKDT1DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT1DI16_Source; const Data: Pointer);
    TKDT1DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT1DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT1DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT1DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT1DI16_DyanmicStoreBuffer;
    KDBuff: TKDT1DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT1DI16_Node;
    TestBuff: TKDT1DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT1DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT1DI16_Node;
    function GetData(const Index: NativeInt): PKDT1DI16_Source;
  public
    RootNode: PKDT1DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT1DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT1DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT1DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT1DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT1DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT1DI16_Node; overload;
    function Search(const buff: TKDT1DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT1DI16_Node; overload;
    function Search(const buff: TKDT1DI16_Vec; var SearchedDistanceMin: Double): PKDT1DI16_Node; overload;
    function Search(const buff: TKDT1DI16_Vec): PKDT1DI16_Node; overload;
    function SearchToken(const buff: TKDT1DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT1DI16_DynamicVecBuffer; var OutBuff: TKDT1DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT1DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT1DI16_Node);
    procedure PrintBuffer;

    class function KDT1DI16Vec(const s: SystemString): TKDT1DI16_Vec; overload;
    class function KDT1DI16Vec(const v: TKDT1DI16_Vec): SystemString; overload;
    class function KDT1DI16Pow(const v: TKDT1DI16_VecType): Double;
    class function KDT1DI16Distance(const v1, v2: TKDT1DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT1DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT2DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT2DI16_Vec = array [0 .. KDT2DI16_Axis - 1] of TKDT2DI16_VecType;
    PKDT2DI16_Vec = ^TKDT2DI16_Vec;

    TKDT2DI16_DynamicVecBuffer = array of TKDT2DI16_Vec;
    PKDT2DI16_DynamicVecBuffer = ^TKDT2DI16_DynamicVecBuffer;

    TKDT2DI16_Source = record
      buff: TKDT2DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT2DI16_Source = ^TKDT2DI16_Source;
    TKDT2DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT2DI16_Source) - 1] of PKDT2DI16_Source;
    PKDT2DI16_SourceBuffer = ^TKDT2DI16_SourceBuffer;

    TKDT2DI16_DyanmicSourceBuffer = array of PKDT2DI16_Source;
    PKDT2DI16_DyanmicSourceBuffer = ^TKDT2DI16_DyanmicSourceBuffer;

    TKDT2DI16_DyanmicStoreBuffer = array of TKDT2DI16_Source;
    PKDT2DI16_DyanmicStoreBuffer = ^TKDT2DI16_DyanmicStoreBuffer;

    PKDT2DI16_Node = ^TKDT2DI16_Node;

    TKDT2DI16_Node = record
      Parent, Right, Left: PKDT2DI16_Node;
      vec: PKDT2DI16_Source;
    end;

    TKDT2DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT2DI16_Source; const Data: Pointer);
    TKDT2DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT2DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT2DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT2DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT2DI16_DyanmicStoreBuffer;
    KDBuff: TKDT2DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT2DI16_Node;
    TestBuff: TKDT2DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT2DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT2DI16_Node;
    function GetData(const Index: NativeInt): PKDT2DI16_Source;
  public
    RootNode: PKDT2DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT2DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT2DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT2DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT2DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT2DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT2DI16_Node; overload;
    function Search(const buff: TKDT2DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT2DI16_Node; overload;
    function Search(const buff: TKDT2DI16_Vec; var SearchedDistanceMin: Double): PKDT2DI16_Node; overload;
    function Search(const buff: TKDT2DI16_Vec): PKDT2DI16_Node; overload;
    function SearchToken(const buff: TKDT2DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT2DI16_DynamicVecBuffer; var OutBuff: TKDT2DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT2DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT2DI16_Node);
    procedure PrintBuffer;

    class function KDT2DI16Vec(const s: SystemString): TKDT2DI16_Vec; overload;
    class function KDT2DI16Vec(const v: TKDT2DI16_Vec): SystemString; overload;
    class function KDT2DI16Pow(const v: TKDT2DI16_VecType): Double;
    class function KDT2DI16Distance(const v1, v2: TKDT2DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT2DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT3DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT3DI16_Vec = array [0 .. KDT3DI16_Axis - 1] of TKDT3DI16_VecType;
    PKDT3DI16_Vec = ^TKDT3DI16_Vec;

    TKDT3DI16_DynamicVecBuffer = array of TKDT3DI16_Vec;
    PKDT3DI16_DynamicVecBuffer = ^TKDT3DI16_DynamicVecBuffer;

    TKDT3DI16_Source = record
      buff: TKDT3DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT3DI16_Source = ^TKDT3DI16_Source;
    TKDT3DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT3DI16_Source) - 1] of PKDT3DI16_Source;
    PKDT3DI16_SourceBuffer = ^TKDT3DI16_SourceBuffer;

    TKDT3DI16_DyanmicSourceBuffer = array of PKDT3DI16_Source;
    PKDT3DI16_DyanmicSourceBuffer = ^TKDT3DI16_DyanmicSourceBuffer;

    TKDT3DI16_DyanmicStoreBuffer = array of TKDT3DI16_Source;
    PKDT3DI16_DyanmicStoreBuffer = ^TKDT3DI16_DyanmicStoreBuffer;

    PKDT3DI16_Node = ^TKDT3DI16_Node;

    TKDT3DI16_Node = record
      Parent, Right, Left: PKDT3DI16_Node;
      vec: PKDT3DI16_Source;
    end;

    TKDT3DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT3DI16_Source; const Data: Pointer);
    TKDT3DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT3DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT3DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT3DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT3DI16_DyanmicStoreBuffer;
    KDBuff: TKDT3DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT3DI16_Node;
    TestBuff: TKDT3DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT3DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT3DI16_Node;
    function GetData(const Index: NativeInt): PKDT3DI16_Source;
  public
    RootNode: PKDT3DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT3DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT3DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT3DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT3DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT3DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT3DI16_Node; overload;
    function Search(const buff: TKDT3DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT3DI16_Node; overload;
    function Search(const buff: TKDT3DI16_Vec; var SearchedDistanceMin: Double): PKDT3DI16_Node; overload;
    function Search(const buff: TKDT3DI16_Vec): PKDT3DI16_Node; overload;
    function SearchToken(const buff: TKDT3DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT3DI16_DynamicVecBuffer; var OutBuff: TKDT3DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT3DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT3DI16_Node);
    procedure PrintBuffer;

    class function KDT3DI16Vec(const s: SystemString): TKDT3DI16_Vec; overload;
    class function KDT3DI16Vec(const v: TKDT3DI16_Vec): SystemString; overload;
    class function KDT3DI16Pow(const v: TKDT3DI16_VecType): Double;
    class function KDT3DI16Distance(const v1, v2: TKDT3DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT3DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT4DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT4DI16_Vec = array [0 .. KDT4DI16_Axis - 1] of TKDT4DI16_VecType;
    PKDT4DI16_Vec = ^TKDT4DI16_Vec;

    TKDT4DI16_DynamicVecBuffer = array of TKDT4DI16_Vec;
    PKDT4DI16_DynamicVecBuffer = ^TKDT4DI16_DynamicVecBuffer;

    TKDT4DI16_Source = record
      buff: TKDT4DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT4DI16_Source = ^TKDT4DI16_Source;
    TKDT4DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT4DI16_Source) - 1] of PKDT4DI16_Source;
    PKDT4DI16_SourceBuffer = ^TKDT4DI16_SourceBuffer;

    TKDT4DI16_DyanmicSourceBuffer = array of PKDT4DI16_Source;
    PKDT4DI16_DyanmicSourceBuffer = ^TKDT4DI16_DyanmicSourceBuffer;

    TKDT4DI16_DyanmicStoreBuffer = array of TKDT4DI16_Source;
    PKDT4DI16_DyanmicStoreBuffer = ^TKDT4DI16_DyanmicStoreBuffer;

    PKDT4DI16_Node = ^TKDT4DI16_Node;

    TKDT4DI16_Node = record
      Parent, Right, Left: PKDT4DI16_Node;
      vec: PKDT4DI16_Source;
    end;

    TKDT4DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT4DI16_Source; const Data: Pointer);
    TKDT4DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT4DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT4DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT4DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT4DI16_DyanmicStoreBuffer;
    KDBuff: TKDT4DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT4DI16_Node;
    TestBuff: TKDT4DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT4DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT4DI16_Node;
    function GetData(const Index: NativeInt): PKDT4DI16_Source;
  public
    RootNode: PKDT4DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT4DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT4DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT4DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT4DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT4DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT4DI16_Node; overload;
    function Search(const buff: TKDT4DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT4DI16_Node; overload;
    function Search(const buff: TKDT4DI16_Vec; var SearchedDistanceMin: Double): PKDT4DI16_Node; overload;
    function Search(const buff: TKDT4DI16_Vec): PKDT4DI16_Node; overload;
    function SearchToken(const buff: TKDT4DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT4DI16_DynamicVecBuffer; var OutBuff: TKDT4DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT4DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT4DI16_Node);
    procedure PrintBuffer;

    class function KDT4DI16Vec(const s: SystemString): TKDT4DI16_Vec; overload;
    class function KDT4DI16Vec(const v: TKDT4DI16_Vec): SystemString; overload;
    class function KDT4DI16Pow(const v: TKDT4DI16_VecType): Double;
    class function KDT4DI16Distance(const v1, v2: TKDT4DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT4DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT5DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT5DI16_Vec = array [0 .. KDT5DI16_Axis - 1] of TKDT5DI16_VecType;
    PKDT5DI16_Vec = ^TKDT5DI16_Vec;

    TKDT5DI16_DynamicVecBuffer = array of TKDT5DI16_Vec;
    PKDT5DI16_DynamicVecBuffer = ^TKDT5DI16_DynamicVecBuffer;

    TKDT5DI16_Source = record
      buff: TKDT5DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT5DI16_Source = ^TKDT5DI16_Source;
    TKDT5DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT5DI16_Source) - 1] of PKDT5DI16_Source;
    PKDT5DI16_SourceBuffer = ^TKDT5DI16_SourceBuffer;

    TKDT5DI16_DyanmicSourceBuffer = array of PKDT5DI16_Source;
    PKDT5DI16_DyanmicSourceBuffer = ^TKDT5DI16_DyanmicSourceBuffer;

    TKDT5DI16_DyanmicStoreBuffer = array of TKDT5DI16_Source;
    PKDT5DI16_DyanmicStoreBuffer = ^TKDT5DI16_DyanmicStoreBuffer;

    PKDT5DI16_Node = ^TKDT5DI16_Node;

    TKDT5DI16_Node = record
      Parent, Right, Left: PKDT5DI16_Node;
      vec: PKDT5DI16_Source;
    end;

    TKDT5DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT5DI16_Source; const Data: Pointer);
    TKDT5DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT5DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT5DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT5DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT5DI16_DyanmicStoreBuffer;
    KDBuff: TKDT5DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT5DI16_Node;
    TestBuff: TKDT5DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT5DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT5DI16_Node;
    function GetData(const Index: NativeInt): PKDT5DI16_Source;
  public
    RootNode: PKDT5DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT5DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT5DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT5DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT5DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT5DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT5DI16_Node; overload;
    function Search(const buff: TKDT5DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT5DI16_Node; overload;
    function Search(const buff: TKDT5DI16_Vec; var SearchedDistanceMin: Double): PKDT5DI16_Node; overload;
    function Search(const buff: TKDT5DI16_Vec): PKDT5DI16_Node; overload;
    function SearchToken(const buff: TKDT5DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT5DI16_DynamicVecBuffer; var OutBuff: TKDT5DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT5DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT5DI16_Node);
    procedure PrintBuffer;

    class function KDT5DI16Vec(const s: SystemString): TKDT5DI16_Vec; overload;
    class function KDT5DI16Vec(const v: TKDT5DI16_Vec): SystemString; overload;
    class function KDT5DI16Pow(const v: TKDT5DI16_VecType): Double;
    class function KDT5DI16Distance(const v1, v2: TKDT5DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT5DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT6DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT6DI16_Vec = array [0 .. KDT6DI16_Axis - 1] of TKDT6DI16_VecType;
    PKDT6DI16_Vec = ^TKDT6DI16_Vec;

    TKDT6DI16_DynamicVecBuffer = array of TKDT6DI16_Vec;
    PKDT6DI16_DynamicVecBuffer = ^TKDT6DI16_DynamicVecBuffer;

    TKDT6DI16_Source = record
      buff: TKDT6DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT6DI16_Source = ^TKDT6DI16_Source;
    TKDT6DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT6DI16_Source) - 1] of PKDT6DI16_Source;
    PKDT6DI16_SourceBuffer = ^TKDT6DI16_SourceBuffer;

    TKDT6DI16_DyanmicSourceBuffer = array of PKDT6DI16_Source;
    PKDT6DI16_DyanmicSourceBuffer = ^TKDT6DI16_DyanmicSourceBuffer;

    TKDT6DI16_DyanmicStoreBuffer = array of TKDT6DI16_Source;
    PKDT6DI16_DyanmicStoreBuffer = ^TKDT6DI16_DyanmicStoreBuffer;

    PKDT6DI16_Node = ^TKDT6DI16_Node;

    TKDT6DI16_Node = record
      Parent, Right, Left: PKDT6DI16_Node;
      vec: PKDT6DI16_Source;
    end;

    TKDT6DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT6DI16_Source; const Data: Pointer);
    TKDT6DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT6DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT6DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT6DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT6DI16_DyanmicStoreBuffer;
    KDBuff: TKDT6DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT6DI16_Node;
    TestBuff: TKDT6DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT6DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT6DI16_Node;
    function GetData(const Index: NativeInt): PKDT6DI16_Source;
  public
    RootNode: PKDT6DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT6DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT6DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT6DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT6DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT6DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT6DI16_Node; overload;
    function Search(const buff: TKDT6DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT6DI16_Node; overload;
    function Search(const buff: TKDT6DI16_Vec; var SearchedDistanceMin: Double): PKDT6DI16_Node; overload;
    function Search(const buff: TKDT6DI16_Vec): PKDT6DI16_Node; overload;
    function SearchToken(const buff: TKDT6DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT6DI16_DynamicVecBuffer; var OutBuff: TKDT6DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT6DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT6DI16_Node);
    procedure PrintBuffer;

    class function KDT6DI16Vec(const s: SystemString): TKDT6DI16_Vec; overload;
    class function KDT6DI16Vec(const v: TKDT6DI16_Vec): SystemString; overload;
    class function KDT6DI16Pow(const v: TKDT6DI16_VecType): Double;
    class function KDT6DI16Distance(const v1, v2: TKDT6DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT6DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT7DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT7DI16_Vec = array [0 .. KDT7DI16_Axis - 1] of TKDT7DI16_VecType;
    PKDT7DI16_Vec = ^TKDT7DI16_Vec;

    TKDT7DI16_DynamicVecBuffer = array of TKDT7DI16_Vec;
    PKDT7DI16_DynamicVecBuffer = ^TKDT7DI16_DynamicVecBuffer;

    TKDT7DI16_Source = record
      buff: TKDT7DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT7DI16_Source = ^TKDT7DI16_Source;
    TKDT7DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT7DI16_Source) - 1] of PKDT7DI16_Source;
    PKDT7DI16_SourceBuffer = ^TKDT7DI16_SourceBuffer;

    TKDT7DI16_DyanmicSourceBuffer = array of PKDT7DI16_Source;
    PKDT7DI16_DyanmicSourceBuffer = ^TKDT7DI16_DyanmicSourceBuffer;

    TKDT7DI16_DyanmicStoreBuffer = array of TKDT7DI16_Source;
    PKDT7DI16_DyanmicStoreBuffer = ^TKDT7DI16_DyanmicStoreBuffer;

    PKDT7DI16_Node = ^TKDT7DI16_Node;

    TKDT7DI16_Node = record
      Parent, Right, Left: PKDT7DI16_Node;
      vec: PKDT7DI16_Source;
    end;

    TKDT7DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT7DI16_Source; const Data: Pointer);
    TKDT7DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT7DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT7DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT7DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT7DI16_DyanmicStoreBuffer;
    KDBuff: TKDT7DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT7DI16_Node;
    TestBuff: TKDT7DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT7DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT7DI16_Node;
    function GetData(const Index: NativeInt): PKDT7DI16_Source;
  public
    RootNode: PKDT7DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT7DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT7DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT7DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT7DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT7DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT7DI16_Node; overload;
    function Search(const buff: TKDT7DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT7DI16_Node; overload;
    function Search(const buff: TKDT7DI16_Vec; var SearchedDistanceMin: Double): PKDT7DI16_Node; overload;
    function Search(const buff: TKDT7DI16_Vec): PKDT7DI16_Node; overload;
    function SearchToken(const buff: TKDT7DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT7DI16_DynamicVecBuffer; var OutBuff: TKDT7DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT7DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT7DI16_Node);
    procedure PrintBuffer;

    class function KDT7DI16Vec(const s: SystemString): TKDT7DI16_Vec; overload;
    class function KDT7DI16Vec(const v: TKDT7DI16_Vec): SystemString; overload;
    class function KDT7DI16Pow(const v: TKDT7DI16_VecType): Double;
    class function KDT7DI16Distance(const v1, v2: TKDT7DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT7DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT8DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT8DI16_Vec = array [0 .. KDT8DI16_Axis - 1] of TKDT8DI16_VecType;
    PKDT8DI16_Vec = ^TKDT8DI16_Vec;

    TKDT8DI16_DynamicVecBuffer = array of TKDT8DI16_Vec;
    PKDT8DI16_DynamicVecBuffer = ^TKDT8DI16_DynamicVecBuffer;

    TKDT8DI16_Source = record
      buff: TKDT8DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT8DI16_Source = ^TKDT8DI16_Source;
    TKDT8DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT8DI16_Source) - 1] of PKDT8DI16_Source;
    PKDT8DI16_SourceBuffer = ^TKDT8DI16_SourceBuffer;

    TKDT8DI16_DyanmicSourceBuffer = array of PKDT8DI16_Source;
    PKDT8DI16_DyanmicSourceBuffer = ^TKDT8DI16_DyanmicSourceBuffer;

    TKDT8DI16_DyanmicStoreBuffer = array of TKDT8DI16_Source;
    PKDT8DI16_DyanmicStoreBuffer = ^TKDT8DI16_DyanmicStoreBuffer;

    PKDT8DI16_Node = ^TKDT8DI16_Node;

    TKDT8DI16_Node = record
      Parent, Right, Left: PKDT8DI16_Node;
      vec: PKDT8DI16_Source;
    end;

    TKDT8DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT8DI16_Source; const Data: Pointer);
    TKDT8DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT8DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT8DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT8DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT8DI16_DyanmicStoreBuffer;
    KDBuff: TKDT8DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT8DI16_Node;
    TestBuff: TKDT8DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT8DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT8DI16_Node;
    function GetData(const Index: NativeInt): PKDT8DI16_Source;
  public
    RootNode: PKDT8DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT8DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT8DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT8DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT8DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT8DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT8DI16_Node; overload;
    function Search(const buff: TKDT8DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT8DI16_Node; overload;
    function Search(const buff: TKDT8DI16_Vec; var SearchedDistanceMin: Double): PKDT8DI16_Node; overload;
    function Search(const buff: TKDT8DI16_Vec): PKDT8DI16_Node; overload;
    function SearchToken(const buff: TKDT8DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT8DI16_DynamicVecBuffer; var OutBuff: TKDT8DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT8DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT8DI16_Node);
    procedure PrintBuffer;

    class function KDT8DI16Vec(const s: SystemString): TKDT8DI16_Vec; overload;
    class function KDT8DI16Vec(const v: TKDT8DI16_Vec): SystemString; overload;
    class function KDT8DI16Pow(const v: TKDT8DI16_VecType): Double;
    class function KDT8DI16Distance(const v1, v2: TKDT8DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT8DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT9DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT9DI16_Vec = array [0 .. KDT9DI16_Axis - 1] of TKDT9DI16_VecType;
    PKDT9DI16_Vec = ^TKDT9DI16_Vec;

    TKDT9DI16_DynamicVecBuffer = array of TKDT9DI16_Vec;
    PKDT9DI16_DynamicVecBuffer = ^TKDT9DI16_DynamicVecBuffer;

    TKDT9DI16_Source = record
      buff: TKDT9DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT9DI16_Source = ^TKDT9DI16_Source;
    TKDT9DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT9DI16_Source) - 1] of PKDT9DI16_Source;
    PKDT9DI16_SourceBuffer = ^TKDT9DI16_SourceBuffer;

    TKDT9DI16_DyanmicSourceBuffer = array of PKDT9DI16_Source;
    PKDT9DI16_DyanmicSourceBuffer = ^TKDT9DI16_DyanmicSourceBuffer;

    TKDT9DI16_DyanmicStoreBuffer = array of TKDT9DI16_Source;
    PKDT9DI16_DyanmicStoreBuffer = ^TKDT9DI16_DyanmicStoreBuffer;

    PKDT9DI16_Node = ^TKDT9DI16_Node;

    TKDT9DI16_Node = record
      Parent, Right, Left: PKDT9DI16_Node;
      vec: PKDT9DI16_Source;
    end;

    TKDT9DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT9DI16_Source; const Data: Pointer);
    TKDT9DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT9DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT9DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT9DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT9DI16_DyanmicStoreBuffer;
    KDBuff: TKDT9DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT9DI16_Node;
    TestBuff: TKDT9DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT9DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT9DI16_Node;
    function GetData(const Index: NativeInt): PKDT9DI16_Source;
  public
    RootNode: PKDT9DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT9DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT9DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT9DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT9DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT9DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT9DI16_Node; overload;
    function Search(const buff: TKDT9DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT9DI16_Node; overload;
    function Search(const buff: TKDT9DI16_Vec; var SearchedDistanceMin: Double): PKDT9DI16_Node; overload;
    function Search(const buff: TKDT9DI16_Vec): PKDT9DI16_Node; overload;
    function SearchToken(const buff: TKDT9DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT9DI16_DynamicVecBuffer; var OutBuff: TKDT9DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT9DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT9DI16_Node);
    procedure PrintBuffer;

    class function KDT9DI16Vec(const s: SystemString): TKDT9DI16_Vec; overload;
    class function KDT9DI16Vec(const v: TKDT9DI16_Vec): SystemString; overload;
    class function KDT9DI16Pow(const v: TKDT9DI16_VecType): Double;
    class function KDT9DI16Distance(const v1, v2: TKDT9DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT9DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT10DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT10DI16_Vec = array [0 .. KDT10DI16_Axis - 1] of TKDT10DI16_VecType;
    PKDT10DI16_Vec = ^TKDT10DI16_Vec;

    TKDT10DI16_DynamicVecBuffer = array of TKDT10DI16_Vec;
    PKDT10DI16_DynamicVecBuffer = ^TKDT10DI16_DynamicVecBuffer;

    TKDT10DI16_Source = record
      buff: TKDT10DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT10DI16_Source = ^TKDT10DI16_Source;
    TKDT10DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT10DI16_Source) - 1] of PKDT10DI16_Source;
    PKDT10DI16_SourceBuffer = ^TKDT10DI16_SourceBuffer;

    TKDT10DI16_DyanmicSourceBuffer = array of PKDT10DI16_Source;
    PKDT10DI16_DyanmicSourceBuffer = ^TKDT10DI16_DyanmicSourceBuffer;

    TKDT10DI16_DyanmicStoreBuffer = array of TKDT10DI16_Source;
    PKDT10DI16_DyanmicStoreBuffer = ^TKDT10DI16_DyanmicStoreBuffer;

    PKDT10DI16_Node = ^TKDT10DI16_Node;

    TKDT10DI16_Node = record
      Parent, Right, Left: PKDT10DI16_Node;
      vec: PKDT10DI16_Source;
    end;

    TKDT10DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT10DI16_Source; const Data: Pointer);
    TKDT10DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT10DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT10DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT10DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT10DI16_DyanmicStoreBuffer;
    KDBuff: TKDT10DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT10DI16_Node;
    TestBuff: TKDT10DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT10DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT10DI16_Node;
    function GetData(const Index: NativeInt): PKDT10DI16_Source;
  public
    RootNode: PKDT10DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT10DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT10DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT10DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT10DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT10DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT10DI16_Node; overload;
    function Search(const buff: TKDT10DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT10DI16_Node; overload;
    function Search(const buff: TKDT10DI16_Vec; var SearchedDistanceMin: Double): PKDT10DI16_Node; overload;
    function Search(const buff: TKDT10DI16_Vec): PKDT10DI16_Node; overload;
    function SearchToken(const buff: TKDT10DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT10DI16_DynamicVecBuffer; var OutBuff: TKDT10DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT10DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT10DI16_Node);
    procedure PrintBuffer;

    class function KDT10DI16Vec(const s: SystemString): TKDT10DI16_Vec; overload;
    class function KDT10DI16Vec(const v: TKDT10DI16_Vec): SystemString; overload;
    class function KDT10DI16Pow(const v: TKDT10DI16_VecType): Double;
    class function KDT10DI16Distance(const v1, v2: TKDT10DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT10DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT11DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT11DI16_Vec = array [0 .. KDT11DI16_Axis - 1] of TKDT11DI16_VecType;
    PKDT11DI16_Vec = ^TKDT11DI16_Vec;

    TKDT11DI16_DynamicVecBuffer = array of TKDT11DI16_Vec;
    PKDT11DI16_DynamicVecBuffer = ^TKDT11DI16_DynamicVecBuffer;

    TKDT11DI16_Source = record
      buff: TKDT11DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT11DI16_Source = ^TKDT11DI16_Source;
    TKDT11DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT11DI16_Source) - 1] of PKDT11DI16_Source;
    PKDT11DI16_SourceBuffer = ^TKDT11DI16_SourceBuffer;

    TKDT11DI16_DyanmicSourceBuffer = array of PKDT11DI16_Source;
    PKDT11DI16_DyanmicSourceBuffer = ^TKDT11DI16_DyanmicSourceBuffer;

    TKDT11DI16_DyanmicStoreBuffer = array of TKDT11DI16_Source;
    PKDT11DI16_DyanmicStoreBuffer = ^TKDT11DI16_DyanmicStoreBuffer;

    PKDT11DI16_Node = ^TKDT11DI16_Node;

    TKDT11DI16_Node = record
      Parent, Right, Left: PKDT11DI16_Node;
      vec: PKDT11DI16_Source;
    end;

    TKDT11DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT11DI16_Source; const Data: Pointer);
    TKDT11DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT11DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT11DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT11DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT11DI16_DyanmicStoreBuffer;
    KDBuff: TKDT11DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT11DI16_Node;
    TestBuff: TKDT11DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT11DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT11DI16_Node;
    function GetData(const Index: NativeInt): PKDT11DI16_Source;
  public
    RootNode: PKDT11DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT11DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT11DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT11DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT11DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT11DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT11DI16_Node; overload;
    function Search(const buff: TKDT11DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT11DI16_Node; overload;
    function Search(const buff: TKDT11DI16_Vec; var SearchedDistanceMin: Double): PKDT11DI16_Node; overload;
    function Search(const buff: TKDT11DI16_Vec): PKDT11DI16_Node; overload;
    function SearchToken(const buff: TKDT11DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT11DI16_DynamicVecBuffer; var OutBuff: TKDT11DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT11DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT11DI16_Node);
    procedure PrintBuffer;

    class function KDT11DI16Vec(const s: SystemString): TKDT11DI16_Vec; overload;
    class function KDT11DI16Vec(const v: TKDT11DI16_Vec): SystemString; overload;
    class function KDT11DI16Pow(const v: TKDT11DI16_VecType): Double;
    class function KDT11DI16Distance(const v1, v2: TKDT11DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT11DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT12DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT12DI16_Vec = array [0 .. KDT12DI16_Axis - 1] of TKDT12DI16_VecType;
    PKDT12DI16_Vec = ^TKDT12DI16_Vec;

    TKDT12DI16_DynamicVecBuffer = array of TKDT12DI16_Vec;
    PKDT12DI16_DynamicVecBuffer = ^TKDT12DI16_DynamicVecBuffer;

    TKDT12DI16_Source = record
      buff: TKDT12DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT12DI16_Source = ^TKDT12DI16_Source;
    TKDT12DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT12DI16_Source) - 1] of PKDT12DI16_Source;
    PKDT12DI16_SourceBuffer = ^TKDT12DI16_SourceBuffer;

    TKDT12DI16_DyanmicSourceBuffer = array of PKDT12DI16_Source;
    PKDT12DI16_DyanmicSourceBuffer = ^TKDT12DI16_DyanmicSourceBuffer;

    TKDT12DI16_DyanmicStoreBuffer = array of TKDT12DI16_Source;
    PKDT12DI16_DyanmicStoreBuffer = ^TKDT12DI16_DyanmicStoreBuffer;

    PKDT12DI16_Node = ^TKDT12DI16_Node;

    TKDT12DI16_Node = record
      Parent, Right, Left: PKDT12DI16_Node;
      vec: PKDT12DI16_Source;
    end;

    TKDT12DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT12DI16_Source; const Data: Pointer);
    TKDT12DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT12DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT12DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT12DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT12DI16_DyanmicStoreBuffer;
    KDBuff: TKDT12DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT12DI16_Node;
    TestBuff: TKDT12DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT12DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT12DI16_Node;
    function GetData(const Index: NativeInt): PKDT12DI16_Source;
  public
    RootNode: PKDT12DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT12DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT12DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT12DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT12DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT12DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT12DI16_Node; overload;
    function Search(const buff: TKDT12DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT12DI16_Node; overload;
    function Search(const buff: TKDT12DI16_Vec; var SearchedDistanceMin: Double): PKDT12DI16_Node; overload;
    function Search(const buff: TKDT12DI16_Vec): PKDT12DI16_Node; overload;
    function SearchToken(const buff: TKDT12DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT12DI16_DynamicVecBuffer; var OutBuff: TKDT12DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT12DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT12DI16_Node);
    procedure PrintBuffer;

    class function KDT12DI16Vec(const s: SystemString): TKDT12DI16_Vec; overload;
    class function KDT12DI16Vec(const v: TKDT12DI16_Vec): SystemString; overload;
    class function KDT12DI16Pow(const v: TKDT12DI16_VecType): Double;
    class function KDT12DI16Distance(const v1, v2: TKDT12DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT12DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT13DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT13DI16_Vec = array [0 .. KDT13DI16_Axis - 1] of TKDT13DI16_VecType;
    PKDT13DI16_Vec = ^TKDT13DI16_Vec;

    TKDT13DI16_DynamicVecBuffer = array of TKDT13DI16_Vec;
    PKDT13DI16_DynamicVecBuffer = ^TKDT13DI16_DynamicVecBuffer;

    TKDT13DI16_Source = record
      buff: TKDT13DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT13DI16_Source = ^TKDT13DI16_Source;
    TKDT13DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT13DI16_Source) - 1] of PKDT13DI16_Source;
    PKDT13DI16_SourceBuffer = ^TKDT13DI16_SourceBuffer;

    TKDT13DI16_DyanmicSourceBuffer = array of PKDT13DI16_Source;
    PKDT13DI16_DyanmicSourceBuffer = ^TKDT13DI16_DyanmicSourceBuffer;

    TKDT13DI16_DyanmicStoreBuffer = array of TKDT13DI16_Source;
    PKDT13DI16_DyanmicStoreBuffer = ^TKDT13DI16_DyanmicStoreBuffer;

    PKDT13DI16_Node = ^TKDT13DI16_Node;

    TKDT13DI16_Node = record
      Parent, Right, Left: PKDT13DI16_Node;
      vec: PKDT13DI16_Source;
    end;

    TKDT13DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT13DI16_Source; const Data: Pointer);
    TKDT13DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT13DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT13DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT13DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT13DI16_DyanmicStoreBuffer;
    KDBuff: TKDT13DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT13DI16_Node;
    TestBuff: TKDT13DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT13DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT13DI16_Node;
    function GetData(const Index: NativeInt): PKDT13DI16_Source;
  public
    RootNode: PKDT13DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT13DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT13DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT13DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT13DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT13DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT13DI16_Node; overload;
    function Search(const buff: TKDT13DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT13DI16_Node; overload;
    function Search(const buff: TKDT13DI16_Vec; var SearchedDistanceMin: Double): PKDT13DI16_Node; overload;
    function Search(const buff: TKDT13DI16_Vec): PKDT13DI16_Node; overload;
    function SearchToken(const buff: TKDT13DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT13DI16_DynamicVecBuffer; var OutBuff: TKDT13DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT13DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT13DI16_Node);
    procedure PrintBuffer;

    class function KDT13DI16Vec(const s: SystemString): TKDT13DI16_Vec; overload;
    class function KDT13DI16Vec(const v: TKDT13DI16_Vec): SystemString; overload;
    class function KDT13DI16Pow(const v: TKDT13DI16_VecType): Double;
    class function KDT13DI16Distance(const v1, v2: TKDT13DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT13DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT14DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT14DI16_Vec = array [0 .. KDT14DI16_Axis - 1] of TKDT14DI16_VecType;
    PKDT14DI16_Vec = ^TKDT14DI16_Vec;

    TKDT14DI16_DynamicVecBuffer = array of TKDT14DI16_Vec;
    PKDT14DI16_DynamicVecBuffer = ^TKDT14DI16_DynamicVecBuffer;

    TKDT14DI16_Source = record
      buff: TKDT14DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT14DI16_Source = ^TKDT14DI16_Source;
    TKDT14DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT14DI16_Source) - 1] of PKDT14DI16_Source;
    PKDT14DI16_SourceBuffer = ^TKDT14DI16_SourceBuffer;

    TKDT14DI16_DyanmicSourceBuffer = array of PKDT14DI16_Source;
    PKDT14DI16_DyanmicSourceBuffer = ^TKDT14DI16_DyanmicSourceBuffer;

    TKDT14DI16_DyanmicStoreBuffer = array of TKDT14DI16_Source;
    PKDT14DI16_DyanmicStoreBuffer = ^TKDT14DI16_DyanmicStoreBuffer;

    PKDT14DI16_Node = ^TKDT14DI16_Node;

    TKDT14DI16_Node = record
      Parent, Right, Left: PKDT14DI16_Node;
      vec: PKDT14DI16_Source;
    end;

    TKDT14DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT14DI16_Source; const Data: Pointer);
    TKDT14DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT14DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT14DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT14DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT14DI16_DyanmicStoreBuffer;
    KDBuff: TKDT14DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT14DI16_Node;
    TestBuff: TKDT14DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT14DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT14DI16_Node;
    function GetData(const Index: NativeInt): PKDT14DI16_Source;
  public
    RootNode: PKDT14DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT14DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT14DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT14DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT14DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT14DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT14DI16_Node; overload;
    function Search(const buff: TKDT14DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT14DI16_Node; overload;
    function Search(const buff: TKDT14DI16_Vec; var SearchedDistanceMin: Double): PKDT14DI16_Node; overload;
    function Search(const buff: TKDT14DI16_Vec): PKDT14DI16_Node; overload;
    function SearchToken(const buff: TKDT14DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT14DI16_DynamicVecBuffer; var OutBuff: TKDT14DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT14DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT14DI16_Node);
    procedure PrintBuffer;

    class function KDT14DI16Vec(const s: SystemString): TKDT14DI16_Vec; overload;
    class function KDT14DI16Vec(const v: TKDT14DI16_Vec): SystemString; overload;
    class function KDT14DI16Pow(const v: TKDT14DI16_VecType): Double;
    class function KDT14DI16Distance(const v1, v2: TKDT14DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT14DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT15DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT15DI16_Vec = array [0 .. KDT15DI16_Axis - 1] of TKDT15DI16_VecType;
    PKDT15DI16_Vec = ^TKDT15DI16_Vec;

    TKDT15DI16_DynamicVecBuffer = array of TKDT15DI16_Vec;
    PKDT15DI16_DynamicVecBuffer = ^TKDT15DI16_DynamicVecBuffer;

    TKDT15DI16_Source = record
      buff: TKDT15DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT15DI16_Source = ^TKDT15DI16_Source;
    TKDT15DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT15DI16_Source) - 1] of PKDT15DI16_Source;
    PKDT15DI16_SourceBuffer = ^TKDT15DI16_SourceBuffer;

    TKDT15DI16_DyanmicSourceBuffer = array of PKDT15DI16_Source;
    PKDT15DI16_DyanmicSourceBuffer = ^TKDT15DI16_DyanmicSourceBuffer;

    TKDT15DI16_DyanmicStoreBuffer = array of TKDT15DI16_Source;
    PKDT15DI16_DyanmicStoreBuffer = ^TKDT15DI16_DyanmicStoreBuffer;

    PKDT15DI16_Node = ^TKDT15DI16_Node;

    TKDT15DI16_Node = record
      Parent, Right, Left: PKDT15DI16_Node;
      vec: PKDT15DI16_Source;
    end;

    TKDT15DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT15DI16_Source; const Data: Pointer);
    TKDT15DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT15DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT15DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT15DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT15DI16_DyanmicStoreBuffer;
    KDBuff: TKDT15DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT15DI16_Node;
    TestBuff: TKDT15DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT15DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT15DI16_Node;
    function GetData(const Index: NativeInt): PKDT15DI16_Source;
  public
    RootNode: PKDT15DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT15DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT15DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT15DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT15DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT15DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT15DI16_Node; overload;
    function Search(const buff: TKDT15DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT15DI16_Node; overload;
    function Search(const buff: TKDT15DI16_Vec; var SearchedDistanceMin: Double): PKDT15DI16_Node; overload;
    function Search(const buff: TKDT15DI16_Vec): PKDT15DI16_Node; overload;
    function SearchToken(const buff: TKDT15DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT15DI16_DynamicVecBuffer; var OutBuff: TKDT15DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT15DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT15DI16_Node);
    procedure PrintBuffer;

    class function KDT15DI16Vec(const s: SystemString): TKDT15DI16_Vec; overload;
    class function KDT15DI16Vec(const v: TKDT15DI16_Vec): SystemString; overload;
    class function KDT15DI16Pow(const v: TKDT15DI16_VecType): Double;
    class function KDT15DI16Distance(const v1, v2: TKDT15DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT15DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT16DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT16DI16_Vec = array [0 .. KDT16DI16_Axis - 1] of TKDT16DI16_VecType;
    PKDT16DI16_Vec = ^TKDT16DI16_Vec;

    TKDT16DI16_DynamicVecBuffer = array of TKDT16DI16_Vec;
    PKDT16DI16_DynamicVecBuffer = ^TKDT16DI16_DynamicVecBuffer;

    TKDT16DI16_Source = record
      buff: TKDT16DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT16DI16_Source = ^TKDT16DI16_Source;
    TKDT16DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT16DI16_Source) - 1] of PKDT16DI16_Source;
    PKDT16DI16_SourceBuffer = ^TKDT16DI16_SourceBuffer;

    TKDT16DI16_DyanmicSourceBuffer = array of PKDT16DI16_Source;
    PKDT16DI16_DyanmicSourceBuffer = ^TKDT16DI16_DyanmicSourceBuffer;

    TKDT16DI16_DyanmicStoreBuffer = array of TKDT16DI16_Source;
    PKDT16DI16_DyanmicStoreBuffer = ^TKDT16DI16_DyanmicStoreBuffer;

    PKDT16DI16_Node = ^TKDT16DI16_Node;

    TKDT16DI16_Node = record
      Parent, Right, Left: PKDT16DI16_Node;
      vec: PKDT16DI16_Source;
    end;

    TKDT16DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT16DI16_Source; const Data: Pointer);
    TKDT16DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT16DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT16DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT16DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT16DI16_DyanmicStoreBuffer;
    KDBuff: TKDT16DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT16DI16_Node;
    TestBuff: TKDT16DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT16DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT16DI16_Node;
    function GetData(const Index: NativeInt): PKDT16DI16_Source;
  public
    RootNode: PKDT16DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT16DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT16DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT16DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT16DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT16DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT16DI16_Node; overload;
    function Search(const buff: TKDT16DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT16DI16_Node; overload;
    function Search(const buff: TKDT16DI16_Vec; var SearchedDistanceMin: Double): PKDT16DI16_Node; overload;
    function Search(const buff: TKDT16DI16_Vec): PKDT16DI16_Node; overload;
    function SearchToken(const buff: TKDT16DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT16DI16_DynamicVecBuffer; var OutBuff: TKDT16DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT16DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT16DI16_Node);
    procedure PrintBuffer;

    class function KDT16DI16Vec(const s: SystemString): TKDT16DI16_Vec; overload;
    class function KDT16DI16Vec(const v: TKDT16DI16_Vec): SystemString; overload;
    class function KDT16DI16Pow(const v: TKDT16DI16_VecType): Double;
    class function KDT16DI16Distance(const v1, v2: TKDT16DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT16DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT17DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT17DI16_Vec = array [0 .. KDT17DI16_Axis - 1] of TKDT17DI16_VecType;
    PKDT17DI16_Vec = ^TKDT17DI16_Vec;

    TKDT17DI16_DynamicVecBuffer = array of TKDT17DI16_Vec;
    PKDT17DI16_DynamicVecBuffer = ^TKDT17DI16_DynamicVecBuffer;

    TKDT17DI16_Source = record
      buff: TKDT17DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT17DI16_Source = ^TKDT17DI16_Source;
    TKDT17DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT17DI16_Source) - 1] of PKDT17DI16_Source;
    PKDT17DI16_SourceBuffer = ^TKDT17DI16_SourceBuffer;

    TKDT17DI16_DyanmicSourceBuffer = array of PKDT17DI16_Source;
    PKDT17DI16_DyanmicSourceBuffer = ^TKDT17DI16_DyanmicSourceBuffer;

    TKDT17DI16_DyanmicStoreBuffer = array of TKDT17DI16_Source;
    PKDT17DI16_DyanmicStoreBuffer = ^TKDT17DI16_DyanmicStoreBuffer;

    PKDT17DI16_Node = ^TKDT17DI16_Node;

    TKDT17DI16_Node = record
      Parent, Right, Left: PKDT17DI16_Node;
      vec: PKDT17DI16_Source;
    end;

    TKDT17DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT17DI16_Source; const Data: Pointer);
    TKDT17DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT17DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT17DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT17DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT17DI16_DyanmicStoreBuffer;
    KDBuff: TKDT17DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT17DI16_Node;
    TestBuff: TKDT17DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT17DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT17DI16_Node;
    function GetData(const Index: NativeInt): PKDT17DI16_Source;
  public
    RootNode: PKDT17DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT17DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT17DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT17DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT17DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT17DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT17DI16_Node; overload;
    function Search(const buff: TKDT17DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT17DI16_Node; overload;
    function Search(const buff: TKDT17DI16_Vec; var SearchedDistanceMin: Double): PKDT17DI16_Node; overload;
    function Search(const buff: TKDT17DI16_Vec): PKDT17DI16_Node; overload;
    function SearchToken(const buff: TKDT17DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT17DI16_DynamicVecBuffer; var OutBuff: TKDT17DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT17DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT17DI16_Node);
    procedure PrintBuffer;

    class function KDT17DI16Vec(const s: SystemString): TKDT17DI16_Vec; overload;
    class function KDT17DI16Vec(const v: TKDT17DI16_Vec): SystemString; overload;
    class function KDT17DI16Pow(const v: TKDT17DI16_VecType): Double;
    class function KDT17DI16Distance(const v1, v2: TKDT17DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT17DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT18DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT18DI16_Vec = array [0 .. KDT18DI16_Axis - 1] of TKDT18DI16_VecType;
    PKDT18DI16_Vec = ^TKDT18DI16_Vec;

    TKDT18DI16_DynamicVecBuffer = array of TKDT18DI16_Vec;
    PKDT18DI16_DynamicVecBuffer = ^TKDT18DI16_DynamicVecBuffer;

    TKDT18DI16_Source = record
      buff: TKDT18DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT18DI16_Source = ^TKDT18DI16_Source;
    TKDT18DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT18DI16_Source) - 1] of PKDT18DI16_Source;
    PKDT18DI16_SourceBuffer = ^TKDT18DI16_SourceBuffer;

    TKDT18DI16_DyanmicSourceBuffer = array of PKDT18DI16_Source;
    PKDT18DI16_DyanmicSourceBuffer = ^TKDT18DI16_DyanmicSourceBuffer;

    TKDT18DI16_DyanmicStoreBuffer = array of TKDT18DI16_Source;
    PKDT18DI16_DyanmicStoreBuffer = ^TKDT18DI16_DyanmicStoreBuffer;

    PKDT18DI16_Node = ^TKDT18DI16_Node;

    TKDT18DI16_Node = record
      Parent, Right, Left: PKDT18DI16_Node;
      vec: PKDT18DI16_Source;
    end;

    TKDT18DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT18DI16_Source; const Data: Pointer);
    TKDT18DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT18DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT18DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT18DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT18DI16_DyanmicStoreBuffer;
    KDBuff: TKDT18DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT18DI16_Node;
    TestBuff: TKDT18DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT18DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT18DI16_Node;
    function GetData(const Index: NativeInt): PKDT18DI16_Source;
  public
    RootNode: PKDT18DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT18DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT18DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT18DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT18DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT18DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT18DI16_Node; overload;
    function Search(const buff: TKDT18DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT18DI16_Node; overload;
    function Search(const buff: TKDT18DI16_Vec; var SearchedDistanceMin: Double): PKDT18DI16_Node; overload;
    function Search(const buff: TKDT18DI16_Vec): PKDT18DI16_Node; overload;
    function SearchToken(const buff: TKDT18DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT18DI16_DynamicVecBuffer; var OutBuff: TKDT18DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT18DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT18DI16_Node);
    procedure PrintBuffer;

    class function KDT18DI16Vec(const s: SystemString): TKDT18DI16_Vec; overload;
    class function KDT18DI16Vec(const v: TKDT18DI16_Vec): SystemString; overload;
    class function KDT18DI16Pow(const v: TKDT18DI16_VecType): Double;
    class function KDT18DI16Distance(const v1, v2: TKDT18DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT18DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT19DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT19DI16_Vec = array [0 .. KDT19DI16_Axis - 1] of TKDT19DI16_VecType;
    PKDT19DI16_Vec = ^TKDT19DI16_Vec;

    TKDT19DI16_DynamicVecBuffer = array of TKDT19DI16_Vec;
    PKDT19DI16_DynamicVecBuffer = ^TKDT19DI16_DynamicVecBuffer;

    TKDT19DI16_Source = record
      buff: TKDT19DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT19DI16_Source = ^TKDT19DI16_Source;
    TKDT19DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT19DI16_Source) - 1] of PKDT19DI16_Source;
    PKDT19DI16_SourceBuffer = ^TKDT19DI16_SourceBuffer;

    TKDT19DI16_DyanmicSourceBuffer = array of PKDT19DI16_Source;
    PKDT19DI16_DyanmicSourceBuffer = ^TKDT19DI16_DyanmicSourceBuffer;

    TKDT19DI16_DyanmicStoreBuffer = array of TKDT19DI16_Source;
    PKDT19DI16_DyanmicStoreBuffer = ^TKDT19DI16_DyanmicStoreBuffer;

    PKDT19DI16_Node = ^TKDT19DI16_Node;

    TKDT19DI16_Node = record
      Parent, Right, Left: PKDT19DI16_Node;
      vec: PKDT19DI16_Source;
    end;

    TKDT19DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT19DI16_Source; const Data: Pointer);
    TKDT19DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT19DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT19DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT19DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT19DI16_DyanmicStoreBuffer;
    KDBuff: TKDT19DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT19DI16_Node;
    TestBuff: TKDT19DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT19DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT19DI16_Node;
    function GetData(const Index: NativeInt): PKDT19DI16_Source;
  public
    RootNode: PKDT19DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT19DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT19DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT19DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT19DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT19DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT19DI16_Node; overload;
    function Search(const buff: TKDT19DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT19DI16_Node; overload;
    function Search(const buff: TKDT19DI16_Vec; var SearchedDistanceMin: Double): PKDT19DI16_Node; overload;
    function Search(const buff: TKDT19DI16_Vec): PKDT19DI16_Node; overload;
    function SearchToken(const buff: TKDT19DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT19DI16_DynamicVecBuffer; var OutBuff: TKDT19DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT19DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT19DI16_Node);
    procedure PrintBuffer;

    class function KDT19DI16Vec(const s: SystemString): TKDT19DI16_Vec; overload;
    class function KDT19DI16Vec(const v: TKDT19DI16_Vec): SystemString; overload;
    class function KDT19DI16Pow(const v: TKDT19DI16_VecType): Double;
    class function KDT19DI16Distance(const v1, v2: TKDT19DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT19DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT20DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT20DI16_Vec = array [0 .. KDT20DI16_Axis - 1] of TKDT20DI16_VecType;
    PKDT20DI16_Vec = ^TKDT20DI16_Vec;

    TKDT20DI16_DynamicVecBuffer = array of TKDT20DI16_Vec;
    PKDT20DI16_DynamicVecBuffer = ^TKDT20DI16_DynamicVecBuffer;

    TKDT20DI16_Source = record
      buff: TKDT20DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT20DI16_Source = ^TKDT20DI16_Source;
    TKDT20DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT20DI16_Source) - 1] of PKDT20DI16_Source;
    PKDT20DI16_SourceBuffer = ^TKDT20DI16_SourceBuffer;

    TKDT20DI16_DyanmicSourceBuffer = array of PKDT20DI16_Source;
    PKDT20DI16_DyanmicSourceBuffer = ^TKDT20DI16_DyanmicSourceBuffer;

    TKDT20DI16_DyanmicStoreBuffer = array of TKDT20DI16_Source;
    PKDT20DI16_DyanmicStoreBuffer = ^TKDT20DI16_DyanmicStoreBuffer;

    PKDT20DI16_Node = ^TKDT20DI16_Node;

    TKDT20DI16_Node = record
      Parent, Right, Left: PKDT20DI16_Node;
      vec: PKDT20DI16_Source;
    end;

    TKDT20DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT20DI16_Source; const Data: Pointer);
    TKDT20DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT20DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT20DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT20DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT20DI16_DyanmicStoreBuffer;
    KDBuff: TKDT20DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT20DI16_Node;
    TestBuff: TKDT20DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT20DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT20DI16_Node;
    function GetData(const Index: NativeInt): PKDT20DI16_Source;
  public
    RootNode: PKDT20DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT20DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT20DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT20DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT20DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT20DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT20DI16_Node; overload;
    function Search(const buff: TKDT20DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT20DI16_Node; overload;
    function Search(const buff: TKDT20DI16_Vec; var SearchedDistanceMin: Double): PKDT20DI16_Node; overload;
    function Search(const buff: TKDT20DI16_Vec): PKDT20DI16_Node; overload;
    function SearchToken(const buff: TKDT20DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT20DI16_DynamicVecBuffer; var OutBuff: TKDT20DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT20DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT20DI16_Node);
    procedure PrintBuffer;

    class function KDT20DI16Vec(const s: SystemString): TKDT20DI16_Vec; overload;
    class function KDT20DI16Vec(const v: TKDT20DI16_Vec): SystemString; overload;
    class function KDT20DI16Pow(const v: TKDT20DI16_VecType): Double;
    class function KDT20DI16Distance(const v1, v2: TKDT20DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT20DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT21DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT21DI16_Vec = array [0 .. KDT21DI16_Axis - 1] of TKDT21DI16_VecType;
    PKDT21DI16_Vec = ^TKDT21DI16_Vec;

    TKDT21DI16_DynamicVecBuffer = array of TKDT21DI16_Vec;
    PKDT21DI16_DynamicVecBuffer = ^TKDT21DI16_DynamicVecBuffer;

    TKDT21DI16_Source = record
      buff: TKDT21DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT21DI16_Source = ^TKDT21DI16_Source;
    TKDT21DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT21DI16_Source) - 1] of PKDT21DI16_Source;
    PKDT21DI16_SourceBuffer = ^TKDT21DI16_SourceBuffer;

    TKDT21DI16_DyanmicSourceBuffer = array of PKDT21DI16_Source;
    PKDT21DI16_DyanmicSourceBuffer = ^TKDT21DI16_DyanmicSourceBuffer;

    TKDT21DI16_DyanmicStoreBuffer = array of TKDT21DI16_Source;
    PKDT21DI16_DyanmicStoreBuffer = ^TKDT21DI16_DyanmicStoreBuffer;

    PKDT21DI16_Node = ^TKDT21DI16_Node;

    TKDT21DI16_Node = record
      Parent, Right, Left: PKDT21DI16_Node;
      vec: PKDT21DI16_Source;
    end;

    TKDT21DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT21DI16_Source; const Data: Pointer);
    TKDT21DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT21DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT21DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT21DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT21DI16_DyanmicStoreBuffer;
    KDBuff: TKDT21DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT21DI16_Node;
    TestBuff: TKDT21DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT21DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT21DI16_Node;
    function GetData(const Index: NativeInt): PKDT21DI16_Source;
  public
    RootNode: PKDT21DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT21DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT21DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT21DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT21DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT21DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT21DI16_Node; overload;
    function Search(const buff: TKDT21DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT21DI16_Node; overload;
    function Search(const buff: TKDT21DI16_Vec; var SearchedDistanceMin: Double): PKDT21DI16_Node; overload;
    function Search(const buff: TKDT21DI16_Vec): PKDT21DI16_Node; overload;
    function SearchToken(const buff: TKDT21DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT21DI16_DynamicVecBuffer; var OutBuff: TKDT21DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT21DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT21DI16_Node);
    procedure PrintBuffer;

    class function KDT21DI16Vec(const s: SystemString): TKDT21DI16_Vec; overload;
    class function KDT21DI16Vec(const v: TKDT21DI16_Vec): SystemString; overload;
    class function KDT21DI16Pow(const v: TKDT21DI16_VecType): Double;
    class function KDT21DI16Distance(const v1, v2: TKDT21DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT21DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT22DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT22DI16_Vec = array [0 .. KDT22DI16_Axis - 1] of TKDT22DI16_VecType;
    PKDT22DI16_Vec = ^TKDT22DI16_Vec;

    TKDT22DI16_DynamicVecBuffer = array of TKDT22DI16_Vec;
    PKDT22DI16_DynamicVecBuffer = ^TKDT22DI16_DynamicVecBuffer;

    TKDT22DI16_Source = record
      buff: TKDT22DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT22DI16_Source = ^TKDT22DI16_Source;
    TKDT22DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT22DI16_Source) - 1] of PKDT22DI16_Source;
    PKDT22DI16_SourceBuffer = ^TKDT22DI16_SourceBuffer;

    TKDT22DI16_DyanmicSourceBuffer = array of PKDT22DI16_Source;
    PKDT22DI16_DyanmicSourceBuffer = ^TKDT22DI16_DyanmicSourceBuffer;

    TKDT22DI16_DyanmicStoreBuffer = array of TKDT22DI16_Source;
    PKDT22DI16_DyanmicStoreBuffer = ^TKDT22DI16_DyanmicStoreBuffer;

    PKDT22DI16_Node = ^TKDT22DI16_Node;

    TKDT22DI16_Node = record
      Parent, Right, Left: PKDT22DI16_Node;
      vec: PKDT22DI16_Source;
    end;

    TKDT22DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT22DI16_Source; const Data: Pointer);
    TKDT22DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT22DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT22DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT22DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT22DI16_DyanmicStoreBuffer;
    KDBuff: TKDT22DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT22DI16_Node;
    TestBuff: TKDT22DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT22DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT22DI16_Node;
    function GetData(const Index: NativeInt): PKDT22DI16_Source;
  public
    RootNode: PKDT22DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT22DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT22DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT22DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT22DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT22DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT22DI16_Node; overload;
    function Search(const buff: TKDT22DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT22DI16_Node; overload;
    function Search(const buff: TKDT22DI16_Vec; var SearchedDistanceMin: Double): PKDT22DI16_Node; overload;
    function Search(const buff: TKDT22DI16_Vec): PKDT22DI16_Node; overload;
    function SearchToken(const buff: TKDT22DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT22DI16_DynamicVecBuffer; var OutBuff: TKDT22DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT22DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT22DI16_Node);
    procedure PrintBuffer;

    class function KDT22DI16Vec(const s: SystemString): TKDT22DI16_Vec; overload;
    class function KDT22DI16Vec(const v: TKDT22DI16_Vec): SystemString; overload;
    class function KDT22DI16Pow(const v: TKDT22DI16_VecType): Double;
    class function KDT22DI16Distance(const v1, v2: TKDT22DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT22DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT23DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT23DI16_Vec = array [0 .. KDT23DI16_Axis - 1] of TKDT23DI16_VecType;
    PKDT23DI16_Vec = ^TKDT23DI16_Vec;

    TKDT23DI16_DynamicVecBuffer = array of TKDT23DI16_Vec;
    PKDT23DI16_DynamicVecBuffer = ^TKDT23DI16_DynamicVecBuffer;

    TKDT23DI16_Source = record
      buff: TKDT23DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT23DI16_Source = ^TKDT23DI16_Source;
    TKDT23DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT23DI16_Source) - 1] of PKDT23DI16_Source;
    PKDT23DI16_SourceBuffer = ^TKDT23DI16_SourceBuffer;

    TKDT23DI16_DyanmicSourceBuffer = array of PKDT23DI16_Source;
    PKDT23DI16_DyanmicSourceBuffer = ^TKDT23DI16_DyanmicSourceBuffer;

    TKDT23DI16_DyanmicStoreBuffer = array of TKDT23DI16_Source;
    PKDT23DI16_DyanmicStoreBuffer = ^TKDT23DI16_DyanmicStoreBuffer;

    PKDT23DI16_Node = ^TKDT23DI16_Node;

    TKDT23DI16_Node = record
      Parent, Right, Left: PKDT23DI16_Node;
      vec: PKDT23DI16_Source;
    end;

    TKDT23DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT23DI16_Source; const Data: Pointer);
    TKDT23DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT23DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT23DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT23DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT23DI16_DyanmicStoreBuffer;
    KDBuff: TKDT23DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT23DI16_Node;
    TestBuff: TKDT23DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT23DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT23DI16_Node;
    function GetData(const Index: NativeInt): PKDT23DI16_Source;
  public
    RootNode: PKDT23DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT23DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT23DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT23DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT23DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT23DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT23DI16_Node; overload;
    function Search(const buff: TKDT23DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT23DI16_Node; overload;
    function Search(const buff: TKDT23DI16_Vec; var SearchedDistanceMin: Double): PKDT23DI16_Node; overload;
    function Search(const buff: TKDT23DI16_Vec): PKDT23DI16_Node; overload;
    function SearchToken(const buff: TKDT23DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT23DI16_DynamicVecBuffer; var OutBuff: TKDT23DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT23DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT23DI16_Node);
    procedure PrintBuffer;

    class function KDT23DI16Vec(const s: SystemString): TKDT23DI16_Vec; overload;
    class function KDT23DI16Vec(const v: TKDT23DI16_Vec): SystemString; overload;
    class function KDT23DI16Pow(const v: TKDT23DI16_VecType): Double;
    class function KDT23DI16Distance(const v1, v2: TKDT23DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT23DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT24DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT24DI16_Vec = array [0 .. KDT24DI16_Axis - 1] of TKDT24DI16_VecType;
    PKDT24DI16_Vec = ^TKDT24DI16_Vec;

    TKDT24DI16_DynamicVecBuffer = array of TKDT24DI16_Vec;
    PKDT24DI16_DynamicVecBuffer = ^TKDT24DI16_DynamicVecBuffer;

    TKDT24DI16_Source = record
      buff: TKDT24DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT24DI16_Source = ^TKDT24DI16_Source;
    TKDT24DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT24DI16_Source) - 1] of PKDT24DI16_Source;
    PKDT24DI16_SourceBuffer = ^TKDT24DI16_SourceBuffer;

    TKDT24DI16_DyanmicSourceBuffer = array of PKDT24DI16_Source;
    PKDT24DI16_DyanmicSourceBuffer = ^TKDT24DI16_DyanmicSourceBuffer;

    TKDT24DI16_DyanmicStoreBuffer = array of TKDT24DI16_Source;
    PKDT24DI16_DyanmicStoreBuffer = ^TKDT24DI16_DyanmicStoreBuffer;

    PKDT24DI16_Node = ^TKDT24DI16_Node;

    TKDT24DI16_Node = record
      Parent, Right, Left: PKDT24DI16_Node;
      vec: PKDT24DI16_Source;
    end;

    TKDT24DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT24DI16_Source; const Data: Pointer);
    TKDT24DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT24DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT24DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT24DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT24DI16_DyanmicStoreBuffer;
    KDBuff: TKDT24DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT24DI16_Node;
    TestBuff: TKDT24DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT24DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT24DI16_Node;
    function GetData(const Index: NativeInt): PKDT24DI16_Source;
  public
    RootNode: PKDT24DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT24DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT24DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT24DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT24DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT24DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT24DI16_Node; overload;
    function Search(const buff: TKDT24DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT24DI16_Node; overload;
    function Search(const buff: TKDT24DI16_Vec; var SearchedDistanceMin: Double): PKDT24DI16_Node; overload;
    function Search(const buff: TKDT24DI16_Vec): PKDT24DI16_Node; overload;
    function SearchToken(const buff: TKDT24DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT24DI16_DynamicVecBuffer; var OutBuff: TKDT24DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT24DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT24DI16_Node);
    procedure PrintBuffer;

    class function KDT24DI16Vec(const s: SystemString): TKDT24DI16_Vec; overload;
    class function KDT24DI16Vec(const v: TKDT24DI16_Vec): SystemString; overload;
    class function KDT24DI16Pow(const v: TKDT24DI16_VecType): Double;
    class function KDT24DI16Distance(const v1, v2: TKDT24DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT24DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT256DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT256DI16_Vec = array [0 .. KDT256DI16_Axis - 1] of TKDT256DI16_VecType;
    PKDT256DI16_Vec = ^TKDT256DI16_Vec;

    TKDT256DI16_DynamicVecBuffer = array of TKDT256DI16_Vec;
    PKDT256DI16_DynamicVecBuffer = ^TKDT256DI16_DynamicVecBuffer;

    TKDT256DI16_Source = record
      buff: TKDT256DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT256DI16_Source = ^TKDT256DI16_Source;
    TKDT256DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT256DI16_Source) - 1] of PKDT256DI16_Source;
    PKDT256DI16_SourceBuffer = ^TKDT256DI16_SourceBuffer;

    TKDT256DI16_DyanmicSourceBuffer = array of PKDT256DI16_Source;
    PKDT256DI16_DyanmicSourceBuffer = ^TKDT256DI16_DyanmicSourceBuffer;

    TKDT256DI16_DyanmicStoreBuffer = array of TKDT256DI16_Source;
    PKDT256DI16_DyanmicStoreBuffer = ^TKDT256DI16_DyanmicStoreBuffer;

    PKDT256DI16_Node = ^TKDT256DI16_Node;

    TKDT256DI16_Node = record
      Parent, Right, Left: PKDT256DI16_Node;
      vec: PKDT256DI16_Source;
    end;

    TKDT256DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT256DI16_Source; const Data: Pointer);
    TKDT256DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT256DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT256DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT256DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT256DI16_DyanmicStoreBuffer;
    KDBuff: TKDT256DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT256DI16_Node;
    TestBuff: TKDT256DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT256DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT256DI16_Node;
    function GetData(const Index: NativeInt): PKDT256DI16_Source;
  public
    RootNode: PKDT256DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT256DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT256DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT256DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT256DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT256DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT256DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT256DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT256DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT256DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT256DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT256DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT256DI16_Node; overload;
    function Search(const buff: TKDT256DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT256DI16_Node; overload;
    function Search(const buff: TKDT256DI16_Vec; var SearchedDistanceMin: Double): PKDT256DI16_Node; overload;
    function Search(const buff: TKDT256DI16_Vec): PKDT256DI16_Node; overload;
    function SearchToken(const buff: TKDT256DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT256DI16_DynamicVecBuffer; var OutBuff: TKDT256DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT256DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT256DI16_Node);
    procedure PrintBuffer;

    class function KDT256DI16Vec(const s: SystemString): TKDT256DI16_Vec; overload;
    class function KDT256DI16Vec(const v: TKDT256DI16_Vec): SystemString; overload;
    class function KDT256DI16Pow(const v: TKDT256DI16_VecType): Double;
    class function KDT256DI16Distance(const v1, v2: TKDT256DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT256DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT512DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT512DI16_Vec = array [0 .. KDT512DI16_Axis - 1] of TKDT512DI16_VecType;
    PKDT512DI16_Vec = ^TKDT512DI16_Vec;

    TKDT512DI16_DynamicVecBuffer = array of TKDT512DI16_Vec;
    PKDT512DI16_DynamicVecBuffer = ^TKDT512DI16_DynamicVecBuffer;

    TKDT512DI16_Source = record
      buff: TKDT512DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT512DI16_Source = ^TKDT512DI16_Source;
    TKDT512DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT512DI16_Source) - 1] of PKDT512DI16_Source;
    PKDT512DI16_SourceBuffer = ^TKDT512DI16_SourceBuffer;

    TKDT512DI16_DyanmicSourceBuffer = array of PKDT512DI16_Source;
    PKDT512DI16_DyanmicSourceBuffer = ^TKDT512DI16_DyanmicSourceBuffer;

    TKDT512DI16_DyanmicStoreBuffer = array of TKDT512DI16_Source;
    PKDT512DI16_DyanmicStoreBuffer = ^TKDT512DI16_DyanmicStoreBuffer;

    PKDT512DI16_Node = ^TKDT512DI16_Node;

    TKDT512DI16_Node = record
      Parent, Right, Left: PKDT512DI16_Node;
      vec: PKDT512DI16_Source;
    end;

    TKDT512DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT512DI16_Source; const Data: Pointer);
    TKDT512DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT512DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT512DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT512DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT512DI16_DyanmicStoreBuffer;
    KDBuff: TKDT512DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT512DI16_Node;
    TestBuff: TKDT512DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT512DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT512DI16_Node;
    function GetData(const Index: NativeInt): PKDT512DI16_Source;
  public
    RootNode: PKDT512DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT512DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT512DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT512DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT512DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT512DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT512DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT512DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT512DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT512DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT512DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT512DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT512DI16_Node; overload;
    function Search(const buff: TKDT512DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT512DI16_Node; overload;
    function Search(const buff: TKDT512DI16_Vec; var SearchedDistanceMin: Double): PKDT512DI16_Node; overload;
    function Search(const buff: TKDT512DI16_Vec): PKDT512DI16_Node; overload;
    function SearchToken(const buff: TKDT512DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT512DI16_DynamicVecBuffer; var OutBuff: TKDT512DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT512DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT512DI16_Node);
    procedure PrintBuffer;

    class function KDT512DI16Vec(const s: SystemString): TKDT512DI16_Vec; overload;
    class function KDT512DI16Vec(const v: TKDT512DI16_Vec): SystemString; overload;
    class function KDT512DI16Pow(const v: TKDT512DI16_VecType): Double;
    class function KDT512DI16Distance(const v1, v2: TKDT512DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT512DI16_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT1024DI16 = class(TCoreClassObject)
  public type
    // code split
    TKDT1024DI16_Vec = array [0 .. KDT1024DI16_Axis - 1] of TKDT1024DI16_VecType;
    PKDT1024DI16_Vec = ^TKDT1024DI16_Vec;

    TKDT1024DI16_DynamicVecBuffer = array of TKDT1024DI16_Vec;
    PKDT1024DI16_DynamicVecBuffer = ^TKDT1024DI16_DynamicVecBuffer;

    TKDT1024DI16_Source = record
      buff: TKDT1024DI16_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT1024DI16_Source = ^TKDT1024DI16_Source;
    TKDT1024DI16_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT1024DI16_Source) - 1] of PKDT1024DI16_Source;
    PKDT1024DI16_SourceBuffer = ^TKDT1024DI16_SourceBuffer;

    TKDT1024DI16_DyanmicSourceBuffer = array of PKDT1024DI16_Source;
    PKDT1024DI16_DyanmicSourceBuffer = ^TKDT1024DI16_DyanmicSourceBuffer;

    TKDT1024DI16_DyanmicStoreBuffer = array of TKDT1024DI16_Source;
    PKDT1024DI16_DyanmicStoreBuffer = ^TKDT1024DI16_DyanmicStoreBuffer;

    PKDT1024DI16_Node = ^TKDT1024DI16_Node;

    TKDT1024DI16_Node = record
      Parent, Right, Left: PKDT1024DI16_Node;
      vec: PKDT1024DI16_Source;
    end;

    TKDT1024DI16_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT1024DI16_Source; const Data: Pointer);
    TKDT1024DI16_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT1024DI16_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT1024DI16_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT1024DI16_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT1024DI16_DyanmicStoreBuffer;
    KDBuff: TKDT1024DI16_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT1024DI16_Node;
    TestBuff: TKDT1024DI16_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT1024DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT1024DI16_Node;
    function GetData(const Index: NativeInt): PKDT1024DI16_Source;
  public
    RootNode: PKDT1024DI16_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT1024DI16_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT1024DI16_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1024DI16_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1024DI16_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1024DI16_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT1024DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT1024DI16_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1024DI16_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1024DI16_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1024DI16_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT1024DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT1024DI16_Node; overload;
    function Search(const buff: TKDT1024DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT1024DI16_Node; overload;
    function Search(const buff: TKDT1024DI16_Vec; var SearchedDistanceMin: Double): PKDT1024DI16_Node; overload;
    function Search(const buff: TKDT1024DI16_Vec): PKDT1024DI16_Node; overload;
    function SearchToken(const buff: TKDT1024DI16_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT1024DI16_DynamicVecBuffer; var OutBuff: TKDT1024DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT1024DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT1024DI16_Node);
    procedure PrintBuffer;

    class function KDT1024DI16Vec(const s: SystemString): TKDT1024DI16_Vec; overload;
    class function KDT1024DI16Vec(const v: TKDT1024DI16_Vec): SystemString; overload;
    class function KDT1024DI16Pow(const v: TKDT1024DI16_VecType): Double;
    class function KDT1024DI16Distance(const v1, v2: TKDT1024DI16_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT1024DI16_Source; const Data: Pointer);
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
  SaveToken = $77;



function TKDT1DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT1DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT1DI16_Node;
  function SortCompare(const p1, p2: PKDT1DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT1DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT1DI16_Source;
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
  kdBuffPtr: PKDT1DI16_SourceBuffer;
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
      axis := Depth mod KDT1DI16_Axis;
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

function TKDT1DI16.GetData(const Index: NativeInt): PKDT1DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT1DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT1DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT1DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT1DI16_Node(KDNodes[i]));
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

function TKDT1DI16.StoreBuffPtr: PKDT1DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT1DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT1DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT1DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT1DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT1DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT1DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT1DI16.BuildKDTreeWithCluster(const inBuff: TKDT1DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT1DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT1DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT1DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT1DI16.BuildKDTreeWithCluster(const inBuff: TKDT1DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT1DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DI16_BuildCall);
var
  TempStoreBuff: TKDT1DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT1DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT1DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT1DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT1DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1DI16_Axis - 1 do
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

procedure TKDT1DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DI16_BuildMethod);
var
  TempStoreBuff: TKDT1DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT1DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT1DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT1DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT1DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1DI16_Axis - 1 do
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


procedure TKDT1DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DI16_BuildProc);
var
  TempStoreBuff: TKDT1DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT1DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT1DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT1DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT1DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1DI16_Axis - 1 do
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


function TKDT1DI16.Search(const buff: TKDT1DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT1DI16_Node;

var
  NearestNeighbour: PKDT1DI16_Node;

  function FindParentNode(const buffPtr: PKDT1DI16_Vec; NodePtr: PKDT1DI16_Node): PKDT1DI16_Node;
  var
    Next: PKDT1DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT1DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT1DI16_Node; const buffPtr: PKDT1DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT1DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT1DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT1DI16_Vec; const p1, p2: PKDT1DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT1DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT1DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT1DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT1DI16_Node;
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
  Parent: PKDT1DI16_Node;
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

  SearchedDistanceMin := KDT1DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT1DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT1DI16.Search(const buff: TKDT1DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT1DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT1DI16.Search(const buff: TKDT1DI16_Vec; var SearchedDistanceMin: Double): PKDT1DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT1DI16.Search(const buff: TKDT1DI16_Vec): PKDT1DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT1DI16.SearchToken(const buff: TKDT1DI16_Vec): TPascalString;
var
  p: PKDT1DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT1DI16.Search(const inBuff: TKDT1DI16_DynamicVecBuffer; var OutBuff: TKDT1DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT1DI16_DynamicVecBuffer;
  outBuffPtr: PKDT1DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT1DI16_Node;
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
        p: PKDT1DI16_Node;
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
  p: PKDT1DI16_Node;
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


procedure TKDT1DI16.Search(const inBuff: TKDT1DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT1DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT1DI16_Node;
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
        p: PKDT1DI16_Node;
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
  p: PKDT1DI16_Node;
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


procedure TKDT1DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT1DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT1DI16_Vec));
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

procedure TKDT1DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT1DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT1DI16_Vec)) <> SizeOf(TKDT1DI16_Vec) then
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

procedure TKDT1DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT1DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT1DI16.PrintNodeTree(const NodePtr: PKDT1DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT1DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT1DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT1DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT1DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT1DI16.KDT1DI16Vec(const s: SystemString): TKDT1DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT1DI16_Axis - 1 do
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
            if j >= KDT1DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT1DI16.KDT1DI16Vec(const v: TKDT1DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT1DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT1DI16.KDT1DI16Pow(const v: TKDT1DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT1DI16.KDT1DI16Distance(const v1, v2: TKDT1DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT1DI16_Axis - 1 do
      Result := Result + KDT1DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT1DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT1DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT1DI16.Test;
var
  TKDT1DI16_Test: TKDT1DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT1DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT1DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT1DI16_Test := TKDT1DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT1DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT1DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT1DI16_Axis - 1 do
        TKDT1DI16_Test.TestBuff[i][j] := i * KDT1DI16_Axis + j;

{$IFDEF FPC}
  TKDT1DI16_Test.BuildKDTreeM(length(TKDT1DI16_Test.TestBuff), nil, @TKDT1DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT1DI16_Test.BuildKDTreeM(length(TKDT1DI16_Test.TestBuff), nil, TKDT1DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT1DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT1DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT1DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT1DI16_Test.Search(TKDT1DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT1DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT1DI16_Test.TestBuff));
      TKDT1DI16_Test.Search(TKDT1DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT1DI16Distance(TKDT1DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT1DI16_Test.Clear;
      { kMean test }
      TKDT1DI16_Test.BuildKDTreeWithCluster(TKDT1DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT1DI16_Test.Search(TKDT1DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT1DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT1DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT1DI16_Test);
end;


function TKDT2DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT2DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT2DI16_Node;
  function SortCompare(const p1, p2: PKDT2DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT2DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT2DI16_Source;
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
  kdBuffPtr: PKDT2DI16_SourceBuffer;
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
      axis := Depth mod KDT2DI16_Axis;
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

function TKDT2DI16.GetData(const Index: NativeInt): PKDT2DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT2DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT2DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT2DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT2DI16_Node(KDNodes[i]));
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

function TKDT2DI16.StoreBuffPtr: PKDT2DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT2DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT2DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT2DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT2DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT2DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT2DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT2DI16.BuildKDTreeWithCluster(const inBuff: TKDT2DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT2DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT2DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT2DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT2DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT2DI16.BuildKDTreeWithCluster(const inBuff: TKDT2DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT2DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DI16_BuildCall);
var
  TempStoreBuff: TKDT2DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT2DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT2DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT2DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT2DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT2DI16_Axis - 1 do
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

procedure TKDT2DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DI16_BuildMethod);
var
  TempStoreBuff: TKDT2DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT2DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT2DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT2DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT2DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT2DI16_Axis - 1 do
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


procedure TKDT2DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DI16_BuildProc);
var
  TempStoreBuff: TKDT2DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT2DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT2DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT2DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT2DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT2DI16_Axis - 1 do
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


function TKDT2DI16.Search(const buff: TKDT2DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT2DI16_Node;

var
  NearestNeighbour: PKDT2DI16_Node;

  function FindParentNode(const buffPtr: PKDT2DI16_Vec; NodePtr: PKDT2DI16_Node): PKDT2DI16_Node;
  var
    Next: PKDT2DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT2DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT2DI16_Node; const buffPtr: PKDT2DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT2DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT2DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT2DI16_Vec; const p1, p2: PKDT2DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT2DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT2DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT2DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT2DI16_Node;
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
  Parent: PKDT2DI16_Node;
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

  SearchedDistanceMin := KDT2DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT2DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT2DI16.Search(const buff: TKDT2DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT2DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT2DI16.Search(const buff: TKDT2DI16_Vec; var SearchedDistanceMin: Double): PKDT2DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT2DI16.Search(const buff: TKDT2DI16_Vec): PKDT2DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT2DI16.SearchToken(const buff: TKDT2DI16_Vec): TPascalString;
var
  p: PKDT2DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT2DI16.Search(const inBuff: TKDT2DI16_DynamicVecBuffer; var OutBuff: TKDT2DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT2DI16_DynamicVecBuffer;
  outBuffPtr: PKDT2DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT2DI16_Node;
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
        p: PKDT2DI16_Node;
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
  p: PKDT2DI16_Node;
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


procedure TKDT2DI16.Search(const inBuff: TKDT2DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT2DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT2DI16_Node;
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
        p: PKDT2DI16_Node;
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
  p: PKDT2DI16_Node;
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


procedure TKDT2DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT2DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT2DI16_Vec));
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

procedure TKDT2DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT2DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT2DI16_Vec)) <> SizeOf(TKDT2DI16_Vec) then
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

procedure TKDT2DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT2DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT2DI16.PrintNodeTree(const NodePtr: PKDT2DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT2DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT2DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT2DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT2DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT2DI16.KDT2DI16Vec(const s: SystemString): TKDT2DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT2DI16_Axis - 1 do
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
            if j >= KDT2DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT2DI16.KDT2DI16Vec(const v: TKDT2DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT2DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT2DI16.KDT2DI16Pow(const v: TKDT2DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT2DI16.KDT2DI16Distance(const v1, v2: TKDT2DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT2DI16_Axis - 1 do
      Result := Result + KDT2DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT2DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT2DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT2DI16.Test;
var
  TKDT2DI16_Test: TKDT2DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT2DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT2DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT2DI16_Test := TKDT2DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT2DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT2DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT2DI16_Axis - 1 do
        TKDT2DI16_Test.TestBuff[i][j] := i * KDT2DI16_Axis + j;

{$IFDEF FPC}
  TKDT2DI16_Test.BuildKDTreeM(length(TKDT2DI16_Test.TestBuff), nil, @TKDT2DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT2DI16_Test.BuildKDTreeM(length(TKDT2DI16_Test.TestBuff), nil, TKDT2DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT2DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT2DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT2DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT2DI16_Test.Search(TKDT2DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT2DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT2DI16_Test.TestBuff));
      TKDT2DI16_Test.Search(TKDT2DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT2DI16Distance(TKDT2DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT2DI16_Test.Clear;
      { kMean test }
      TKDT2DI16_Test.BuildKDTreeWithCluster(TKDT2DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT2DI16_Test.Search(TKDT2DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT2DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT2DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT2DI16_Test);
end;


function TKDT3DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT3DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT3DI16_Node;
  function SortCompare(const p1, p2: PKDT3DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT3DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT3DI16_Source;
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
  kdBuffPtr: PKDT3DI16_SourceBuffer;
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
      axis := Depth mod KDT3DI16_Axis;
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

function TKDT3DI16.GetData(const Index: NativeInt): PKDT3DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT3DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT3DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT3DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT3DI16_Node(KDNodes[i]));
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

function TKDT3DI16.StoreBuffPtr: PKDT3DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT3DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT3DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT3DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT3DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT3DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT3DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT3DI16.BuildKDTreeWithCluster(const inBuff: TKDT3DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT3DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT3DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT3DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT3DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT3DI16.BuildKDTreeWithCluster(const inBuff: TKDT3DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT3DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DI16_BuildCall);
var
  TempStoreBuff: TKDT3DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT3DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT3DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT3DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT3DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT3DI16_Axis - 1 do
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

procedure TKDT3DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DI16_BuildMethod);
var
  TempStoreBuff: TKDT3DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT3DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT3DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT3DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT3DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT3DI16_Axis - 1 do
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


procedure TKDT3DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DI16_BuildProc);
var
  TempStoreBuff: TKDT3DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT3DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT3DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT3DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT3DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT3DI16_Axis - 1 do
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


function TKDT3DI16.Search(const buff: TKDT3DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT3DI16_Node;

var
  NearestNeighbour: PKDT3DI16_Node;

  function FindParentNode(const buffPtr: PKDT3DI16_Vec; NodePtr: PKDT3DI16_Node): PKDT3DI16_Node;
  var
    Next: PKDT3DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT3DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT3DI16_Node; const buffPtr: PKDT3DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT3DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT3DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT3DI16_Vec; const p1, p2: PKDT3DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT3DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT3DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT3DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT3DI16_Node;
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
  Parent: PKDT3DI16_Node;
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

  SearchedDistanceMin := KDT3DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT3DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT3DI16.Search(const buff: TKDT3DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT3DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT3DI16.Search(const buff: TKDT3DI16_Vec; var SearchedDistanceMin: Double): PKDT3DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT3DI16.Search(const buff: TKDT3DI16_Vec): PKDT3DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT3DI16.SearchToken(const buff: TKDT3DI16_Vec): TPascalString;
var
  p: PKDT3DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT3DI16.Search(const inBuff: TKDT3DI16_DynamicVecBuffer; var OutBuff: TKDT3DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT3DI16_DynamicVecBuffer;
  outBuffPtr: PKDT3DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT3DI16_Node;
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
        p: PKDT3DI16_Node;
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
  p: PKDT3DI16_Node;
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


procedure TKDT3DI16.Search(const inBuff: TKDT3DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT3DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT3DI16_Node;
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
        p: PKDT3DI16_Node;
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
  p: PKDT3DI16_Node;
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


procedure TKDT3DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT3DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT3DI16_Vec));
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

procedure TKDT3DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT3DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT3DI16_Vec)) <> SizeOf(TKDT3DI16_Vec) then
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

procedure TKDT3DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT3DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT3DI16.PrintNodeTree(const NodePtr: PKDT3DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT3DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT3DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT3DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT3DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT3DI16.KDT3DI16Vec(const s: SystemString): TKDT3DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT3DI16_Axis - 1 do
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
            if j >= KDT3DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT3DI16.KDT3DI16Vec(const v: TKDT3DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT3DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT3DI16.KDT3DI16Pow(const v: TKDT3DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT3DI16.KDT3DI16Distance(const v1, v2: TKDT3DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT3DI16_Axis - 1 do
      Result := Result + KDT3DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT3DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT3DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT3DI16.Test;
var
  TKDT3DI16_Test: TKDT3DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT3DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT3DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT3DI16_Test := TKDT3DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT3DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT3DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT3DI16_Axis - 1 do
        TKDT3DI16_Test.TestBuff[i][j] := i * KDT3DI16_Axis + j;

{$IFDEF FPC}
  TKDT3DI16_Test.BuildKDTreeM(length(TKDT3DI16_Test.TestBuff), nil, @TKDT3DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT3DI16_Test.BuildKDTreeM(length(TKDT3DI16_Test.TestBuff), nil, TKDT3DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT3DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT3DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT3DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT3DI16_Test.Search(TKDT3DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT3DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT3DI16_Test.TestBuff));
      TKDT3DI16_Test.Search(TKDT3DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT3DI16Distance(TKDT3DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT3DI16_Test.Clear;
      { kMean test }
      TKDT3DI16_Test.BuildKDTreeWithCluster(TKDT3DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT3DI16_Test.Search(TKDT3DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT3DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT3DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT3DI16_Test);
end;


function TKDT4DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT4DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT4DI16_Node;
  function SortCompare(const p1, p2: PKDT4DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT4DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT4DI16_Source;
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
  kdBuffPtr: PKDT4DI16_SourceBuffer;
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
      axis := Depth mod KDT4DI16_Axis;
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

function TKDT4DI16.GetData(const Index: NativeInt): PKDT4DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT4DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT4DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT4DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT4DI16_Node(KDNodes[i]));
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

function TKDT4DI16.StoreBuffPtr: PKDT4DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT4DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT4DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT4DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT4DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT4DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT4DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT4DI16.BuildKDTreeWithCluster(const inBuff: TKDT4DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT4DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT4DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT4DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT4DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT4DI16.BuildKDTreeWithCluster(const inBuff: TKDT4DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT4DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DI16_BuildCall);
var
  TempStoreBuff: TKDT4DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT4DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT4DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT4DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT4DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT4DI16_Axis - 1 do
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

procedure TKDT4DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DI16_BuildMethod);
var
  TempStoreBuff: TKDT4DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT4DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT4DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT4DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT4DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT4DI16_Axis - 1 do
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


procedure TKDT4DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DI16_BuildProc);
var
  TempStoreBuff: TKDT4DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT4DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT4DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT4DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT4DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT4DI16_Axis - 1 do
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


function TKDT4DI16.Search(const buff: TKDT4DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT4DI16_Node;

var
  NearestNeighbour: PKDT4DI16_Node;

  function FindParentNode(const buffPtr: PKDT4DI16_Vec; NodePtr: PKDT4DI16_Node): PKDT4DI16_Node;
  var
    Next: PKDT4DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT4DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT4DI16_Node; const buffPtr: PKDT4DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT4DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT4DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT4DI16_Vec; const p1, p2: PKDT4DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT4DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT4DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT4DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT4DI16_Node;
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
  Parent: PKDT4DI16_Node;
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

  SearchedDistanceMin := KDT4DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT4DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT4DI16.Search(const buff: TKDT4DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT4DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT4DI16.Search(const buff: TKDT4DI16_Vec; var SearchedDistanceMin: Double): PKDT4DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT4DI16.Search(const buff: TKDT4DI16_Vec): PKDT4DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT4DI16.SearchToken(const buff: TKDT4DI16_Vec): TPascalString;
var
  p: PKDT4DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT4DI16.Search(const inBuff: TKDT4DI16_DynamicVecBuffer; var OutBuff: TKDT4DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT4DI16_DynamicVecBuffer;
  outBuffPtr: PKDT4DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT4DI16_Node;
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
        p: PKDT4DI16_Node;
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
  p: PKDT4DI16_Node;
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


procedure TKDT4DI16.Search(const inBuff: TKDT4DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT4DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT4DI16_Node;
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
        p: PKDT4DI16_Node;
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
  p: PKDT4DI16_Node;
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


procedure TKDT4DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT4DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT4DI16_Vec));
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

procedure TKDT4DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT4DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT4DI16_Vec)) <> SizeOf(TKDT4DI16_Vec) then
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

procedure TKDT4DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT4DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT4DI16.PrintNodeTree(const NodePtr: PKDT4DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT4DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT4DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT4DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT4DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT4DI16.KDT4DI16Vec(const s: SystemString): TKDT4DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT4DI16_Axis - 1 do
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
            if j >= KDT4DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT4DI16.KDT4DI16Vec(const v: TKDT4DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT4DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT4DI16.KDT4DI16Pow(const v: TKDT4DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT4DI16.KDT4DI16Distance(const v1, v2: TKDT4DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT4DI16_Axis - 1 do
      Result := Result + KDT4DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT4DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT4DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT4DI16.Test;
var
  TKDT4DI16_Test: TKDT4DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT4DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT4DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT4DI16_Test := TKDT4DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT4DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT4DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT4DI16_Axis - 1 do
        TKDT4DI16_Test.TestBuff[i][j] := i * KDT4DI16_Axis + j;

{$IFDEF FPC}
  TKDT4DI16_Test.BuildKDTreeM(length(TKDT4DI16_Test.TestBuff), nil, @TKDT4DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT4DI16_Test.BuildKDTreeM(length(TKDT4DI16_Test.TestBuff), nil, TKDT4DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT4DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT4DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT4DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT4DI16_Test.Search(TKDT4DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT4DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT4DI16_Test.TestBuff));
      TKDT4DI16_Test.Search(TKDT4DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT4DI16Distance(TKDT4DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT4DI16_Test.Clear;
      { kMean test }
      TKDT4DI16_Test.BuildKDTreeWithCluster(TKDT4DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT4DI16_Test.Search(TKDT4DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT4DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT4DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT4DI16_Test);
end;


function TKDT5DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT5DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT5DI16_Node;
  function SortCompare(const p1, p2: PKDT5DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT5DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT5DI16_Source;
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
  kdBuffPtr: PKDT5DI16_SourceBuffer;
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
      axis := Depth mod KDT5DI16_Axis;
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

function TKDT5DI16.GetData(const Index: NativeInt): PKDT5DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT5DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT5DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT5DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT5DI16_Node(KDNodes[i]));
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

function TKDT5DI16.StoreBuffPtr: PKDT5DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT5DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT5DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT5DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT5DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT5DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT5DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT5DI16.BuildKDTreeWithCluster(const inBuff: TKDT5DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT5DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT5DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT5DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT5DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT5DI16.BuildKDTreeWithCluster(const inBuff: TKDT5DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT5DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DI16_BuildCall);
var
  TempStoreBuff: TKDT5DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT5DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT5DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT5DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT5DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT5DI16_Axis - 1 do
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

procedure TKDT5DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DI16_BuildMethod);
var
  TempStoreBuff: TKDT5DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT5DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT5DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT5DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT5DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT5DI16_Axis - 1 do
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


procedure TKDT5DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DI16_BuildProc);
var
  TempStoreBuff: TKDT5DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT5DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT5DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT5DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT5DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT5DI16_Axis - 1 do
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


function TKDT5DI16.Search(const buff: TKDT5DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT5DI16_Node;

var
  NearestNeighbour: PKDT5DI16_Node;

  function FindParentNode(const buffPtr: PKDT5DI16_Vec; NodePtr: PKDT5DI16_Node): PKDT5DI16_Node;
  var
    Next: PKDT5DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT5DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT5DI16_Node; const buffPtr: PKDT5DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT5DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT5DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT5DI16_Vec; const p1, p2: PKDT5DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT5DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT5DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT5DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT5DI16_Node;
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
  Parent: PKDT5DI16_Node;
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

  SearchedDistanceMin := KDT5DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT5DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT5DI16.Search(const buff: TKDT5DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT5DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT5DI16.Search(const buff: TKDT5DI16_Vec; var SearchedDistanceMin: Double): PKDT5DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT5DI16.Search(const buff: TKDT5DI16_Vec): PKDT5DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT5DI16.SearchToken(const buff: TKDT5DI16_Vec): TPascalString;
var
  p: PKDT5DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT5DI16.Search(const inBuff: TKDT5DI16_DynamicVecBuffer; var OutBuff: TKDT5DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT5DI16_DynamicVecBuffer;
  outBuffPtr: PKDT5DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT5DI16_Node;
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
        p: PKDT5DI16_Node;
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
  p: PKDT5DI16_Node;
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


procedure TKDT5DI16.Search(const inBuff: TKDT5DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT5DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT5DI16_Node;
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
        p: PKDT5DI16_Node;
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
  p: PKDT5DI16_Node;
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


procedure TKDT5DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT5DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT5DI16_Vec));
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

procedure TKDT5DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT5DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT5DI16_Vec)) <> SizeOf(TKDT5DI16_Vec) then
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

procedure TKDT5DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT5DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT5DI16.PrintNodeTree(const NodePtr: PKDT5DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT5DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT5DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT5DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT5DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT5DI16.KDT5DI16Vec(const s: SystemString): TKDT5DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT5DI16_Axis - 1 do
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
            if j >= KDT5DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT5DI16.KDT5DI16Vec(const v: TKDT5DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT5DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT5DI16.KDT5DI16Pow(const v: TKDT5DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT5DI16.KDT5DI16Distance(const v1, v2: TKDT5DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT5DI16_Axis - 1 do
      Result := Result + KDT5DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT5DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT5DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT5DI16.Test;
var
  TKDT5DI16_Test: TKDT5DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT5DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT5DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT5DI16_Test := TKDT5DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT5DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT5DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT5DI16_Axis - 1 do
        TKDT5DI16_Test.TestBuff[i][j] := i * KDT5DI16_Axis + j;

{$IFDEF FPC}
  TKDT5DI16_Test.BuildKDTreeM(length(TKDT5DI16_Test.TestBuff), nil, @TKDT5DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT5DI16_Test.BuildKDTreeM(length(TKDT5DI16_Test.TestBuff), nil, TKDT5DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT5DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT5DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT5DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT5DI16_Test.Search(TKDT5DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT5DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT5DI16_Test.TestBuff));
      TKDT5DI16_Test.Search(TKDT5DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT5DI16Distance(TKDT5DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT5DI16_Test.Clear;
      { kMean test }
      TKDT5DI16_Test.BuildKDTreeWithCluster(TKDT5DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT5DI16_Test.Search(TKDT5DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT5DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT5DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT5DI16_Test);
end;


function TKDT6DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT6DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT6DI16_Node;
  function SortCompare(const p1, p2: PKDT6DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT6DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT6DI16_Source;
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
  kdBuffPtr: PKDT6DI16_SourceBuffer;
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
      axis := Depth mod KDT6DI16_Axis;
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

function TKDT6DI16.GetData(const Index: NativeInt): PKDT6DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT6DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT6DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT6DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT6DI16_Node(KDNodes[i]));
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

function TKDT6DI16.StoreBuffPtr: PKDT6DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT6DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT6DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT6DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT6DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT6DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT6DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT6DI16.BuildKDTreeWithCluster(const inBuff: TKDT6DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT6DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT6DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT6DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT6DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT6DI16.BuildKDTreeWithCluster(const inBuff: TKDT6DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT6DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DI16_BuildCall);
var
  TempStoreBuff: TKDT6DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT6DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT6DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT6DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT6DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT6DI16_Axis - 1 do
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

procedure TKDT6DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DI16_BuildMethod);
var
  TempStoreBuff: TKDT6DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT6DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT6DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT6DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT6DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT6DI16_Axis - 1 do
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


procedure TKDT6DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DI16_BuildProc);
var
  TempStoreBuff: TKDT6DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT6DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT6DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT6DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT6DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT6DI16_Axis - 1 do
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


function TKDT6DI16.Search(const buff: TKDT6DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT6DI16_Node;

var
  NearestNeighbour: PKDT6DI16_Node;

  function FindParentNode(const buffPtr: PKDT6DI16_Vec; NodePtr: PKDT6DI16_Node): PKDT6DI16_Node;
  var
    Next: PKDT6DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT6DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT6DI16_Node; const buffPtr: PKDT6DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT6DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT6DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT6DI16_Vec; const p1, p2: PKDT6DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT6DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT6DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT6DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT6DI16_Node;
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
  Parent: PKDT6DI16_Node;
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

  SearchedDistanceMin := KDT6DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT6DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT6DI16.Search(const buff: TKDT6DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT6DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT6DI16.Search(const buff: TKDT6DI16_Vec; var SearchedDistanceMin: Double): PKDT6DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT6DI16.Search(const buff: TKDT6DI16_Vec): PKDT6DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT6DI16.SearchToken(const buff: TKDT6DI16_Vec): TPascalString;
var
  p: PKDT6DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT6DI16.Search(const inBuff: TKDT6DI16_DynamicVecBuffer; var OutBuff: TKDT6DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT6DI16_DynamicVecBuffer;
  outBuffPtr: PKDT6DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT6DI16_Node;
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
        p: PKDT6DI16_Node;
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
  p: PKDT6DI16_Node;
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


procedure TKDT6DI16.Search(const inBuff: TKDT6DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT6DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT6DI16_Node;
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
        p: PKDT6DI16_Node;
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
  p: PKDT6DI16_Node;
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


procedure TKDT6DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT6DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT6DI16_Vec));
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

procedure TKDT6DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT6DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT6DI16_Vec)) <> SizeOf(TKDT6DI16_Vec) then
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

procedure TKDT6DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT6DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT6DI16.PrintNodeTree(const NodePtr: PKDT6DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT6DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT6DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT6DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT6DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT6DI16.KDT6DI16Vec(const s: SystemString): TKDT6DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT6DI16_Axis - 1 do
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
            if j >= KDT6DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT6DI16.KDT6DI16Vec(const v: TKDT6DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT6DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT6DI16.KDT6DI16Pow(const v: TKDT6DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT6DI16.KDT6DI16Distance(const v1, v2: TKDT6DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT6DI16_Axis - 1 do
      Result := Result + KDT6DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT6DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT6DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT6DI16.Test;
var
  TKDT6DI16_Test: TKDT6DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT6DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT6DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT6DI16_Test := TKDT6DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT6DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT6DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT6DI16_Axis - 1 do
        TKDT6DI16_Test.TestBuff[i][j] := i * KDT6DI16_Axis + j;

{$IFDEF FPC}
  TKDT6DI16_Test.BuildKDTreeM(length(TKDT6DI16_Test.TestBuff), nil, @TKDT6DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT6DI16_Test.BuildKDTreeM(length(TKDT6DI16_Test.TestBuff), nil, TKDT6DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT6DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT6DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT6DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT6DI16_Test.Search(TKDT6DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT6DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT6DI16_Test.TestBuff));
      TKDT6DI16_Test.Search(TKDT6DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT6DI16Distance(TKDT6DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT6DI16_Test.Clear;
      { kMean test }
      TKDT6DI16_Test.BuildKDTreeWithCluster(TKDT6DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT6DI16_Test.Search(TKDT6DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT6DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT6DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT6DI16_Test);
end;


function TKDT7DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT7DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT7DI16_Node;
  function SortCompare(const p1, p2: PKDT7DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT7DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT7DI16_Source;
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
  kdBuffPtr: PKDT7DI16_SourceBuffer;
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
      axis := Depth mod KDT7DI16_Axis;
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

function TKDT7DI16.GetData(const Index: NativeInt): PKDT7DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT7DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT7DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT7DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT7DI16_Node(KDNodes[i]));
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

function TKDT7DI16.StoreBuffPtr: PKDT7DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT7DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT7DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT7DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT7DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT7DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT7DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT7DI16.BuildKDTreeWithCluster(const inBuff: TKDT7DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT7DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT7DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT7DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT7DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT7DI16.BuildKDTreeWithCluster(const inBuff: TKDT7DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT7DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DI16_BuildCall);
var
  TempStoreBuff: TKDT7DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT7DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT7DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT7DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT7DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT7DI16_Axis - 1 do
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

procedure TKDT7DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DI16_BuildMethod);
var
  TempStoreBuff: TKDT7DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT7DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT7DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT7DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT7DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT7DI16_Axis - 1 do
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


procedure TKDT7DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DI16_BuildProc);
var
  TempStoreBuff: TKDT7DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT7DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT7DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT7DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT7DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT7DI16_Axis - 1 do
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


function TKDT7DI16.Search(const buff: TKDT7DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT7DI16_Node;

var
  NearestNeighbour: PKDT7DI16_Node;

  function FindParentNode(const buffPtr: PKDT7DI16_Vec; NodePtr: PKDT7DI16_Node): PKDT7DI16_Node;
  var
    Next: PKDT7DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT7DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT7DI16_Node; const buffPtr: PKDT7DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT7DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT7DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT7DI16_Vec; const p1, p2: PKDT7DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT7DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT7DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT7DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT7DI16_Node;
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
  Parent: PKDT7DI16_Node;
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

  SearchedDistanceMin := KDT7DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT7DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT7DI16.Search(const buff: TKDT7DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT7DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT7DI16.Search(const buff: TKDT7DI16_Vec; var SearchedDistanceMin: Double): PKDT7DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT7DI16.Search(const buff: TKDT7DI16_Vec): PKDT7DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT7DI16.SearchToken(const buff: TKDT7DI16_Vec): TPascalString;
var
  p: PKDT7DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT7DI16.Search(const inBuff: TKDT7DI16_DynamicVecBuffer; var OutBuff: TKDT7DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT7DI16_DynamicVecBuffer;
  outBuffPtr: PKDT7DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT7DI16_Node;
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
        p: PKDT7DI16_Node;
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
  p: PKDT7DI16_Node;
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


procedure TKDT7DI16.Search(const inBuff: TKDT7DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT7DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT7DI16_Node;
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
        p: PKDT7DI16_Node;
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
  p: PKDT7DI16_Node;
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


procedure TKDT7DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT7DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT7DI16_Vec));
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

procedure TKDT7DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT7DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT7DI16_Vec)) <> SizeOf(TKDT7DI16_Vec) then
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

procedure TKDT7DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT7DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT7DI16.PrintNodeTree(const NodePtr: PKDT7DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT7DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT7DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT7DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT7DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT7DI16.KDT7DI16Vec(const s: SystemString): TKDT7DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT7DI16_Axis - 1 do
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
            if j >= KDT7DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT7DI16.KDT7DI16Vec(const v: TKDT7DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT7DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT7DI16.KDT7DI16Pow(const v: TKDT7DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT7DI16.KDT7DI16Distance(const v1, v2: TKDT7DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT7DI16_Axis - 1 do
      Result := Result + KDT7DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT7DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT7DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT7DI16.Test;
var
  TKDT7DI16_Test: TKDT7DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT7DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT7DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT7DI16_Test := TKDT7DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT7DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT7DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT7DI16_Axis - 1 do
        TKDT7DI16_Test.TestBuff[i][j] := i * KDT7DI16_Axis + j;

{$IFDEF FPC}
  TKDT7DI16_Test.BuildKDTreeM(length(TKDT7DI16_Test.TestBuff), nil, @TKDT7DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT7DI16_Test.BuildKDTreeM(length(TKDT7DI16_Test.TestBuff), nil, TKDT7DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT7DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT7DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT7DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT7DI16_Test.Search(TKDT7DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT7DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT7DI16_Test.TestBuff));
      TKDT7DI16_Test.Search(TKDT7DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT7DI16Distance(TKDT7DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT7DI16_Test.Clear;
      { kMean test }
      TKDT7DI16_Test.BuildKDTreeWithCluster(TKDT7DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT7DI16_Test.Search(TKDT7DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT7DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT7DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT7DI16_Test);
end;


function TKDT8DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT8DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT8DI16_Node;
  function SortCompare(const p1, p2: PKDT8DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT8DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT8DI16_Source;
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
  kdBuffPtr: PKDT8DI16_SourceBuffer;
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
      axis := Depth mod KDT8DI16_Axis;
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

function TKDT8DI16.GetData(const Index: NativeInt): PKDT8DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT8DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT8DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT8DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT8DI16_Node(KDNodes[i]));
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

function TKDT8DI16.StoreBuffPtr: PKDT8DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT8DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT8DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT8DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT8DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT8DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT8DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT8DI16.BuildKDTreeWithCluster(const inBuff: TKDT8DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT8DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT8DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT8DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT8DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT8DI16.BuildKDTreeWithCluster(const inBuff: TKDT8DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT8DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DI16_BuildCall);
var
  TempStoreBuff: TKDT8DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT8DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT8DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT8DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT8DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT8DI16_Axis - 1 do
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

procedure TKDT8DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DI16_BuildMethod);
var
  TempStoreBuff: TKDT8DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT8DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT8DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT8DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT8DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT8DI16_Axis - 1 do
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


procedure TKDT8DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DI16_BuildProc);
var
  TempStoreBuff: TKDT8DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT8DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT8DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT8DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT8DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT8DI16_Axis - 1 do
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


function TKDT8DI16.Search(const buff: TKDT8DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT8DI16_Node;

var
  NearestNeighbour: PKDT8DI16_Node;

  function FindParentNode(const buffPtr: PKDT8DI16_Vec; NodePtr: PKDT8DI16_Node): PKDT8DI16_Node;
  var
    Next: PKDT8DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT8DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT8DI16_Node; const buffPtr: PKDT8DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT8DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT8DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT8DI16_Vec; const p1, p2: PKDT8DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT8DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT8DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT8DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT8DI16_Node;
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
  Parent: PKDT8DI16_Node;
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

  SearchedDistanceMin := KDT8DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT8DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT8DI16.Search(const buff: TKDT8DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT8DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT8DI16.Search(const buff: TKDT8DI16_Vec; var SearchedDistanceMin: Double): PKDT8DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT8DI16.Search(const buff: TKDT8DI16_Vec): PKDT8DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT8DI16.SearchToken(const buff: TKDT8DI16_Vec): TPascalString;
var
  p: PKDT8DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT8DI16.Search(const inBuff: TKDT8DI16_DynamicVecBuffer; var OutBuff: TKDT8DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT8DI16_DynamicVecBuffer;
  outBuffPtr: PKDT8DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT8DI16_Node;
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
        p: PKDT8DI16_Node;
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
  p: PKDT8DI16_Node;
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


procedure TKDT8DI16.Search(const inBuff: TKDT8DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT8DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT8DI16_Node;
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
        p: PKDT8DI16_Node;
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
  p: PKDT8DI16_Node;
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


procedure TKDT8DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT8DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT8DI16_Vec));
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

procedure TKDT8DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT8DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT8DI16_Vec)) <> SizeOf(TKDT8DI16_Vec) then
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

procedure TKDT8DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT8DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT8DI16.PrintNodeTree(const NodePtr: PKDT8DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT8DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT8DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT8DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT8DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT8DI16.KDT8DI16Vec(const s: SystemString): TKDT8DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT8DI16_Axis - 1 do
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
            if j >= KDT8DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT8DI16.KDT8DI16Vec(const v: TKDT8DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT8DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT8DI16.KDT8DI16Pow(const v: TKDT8DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT8DI16.KDT8DI16Distance(const v1, v2: TKDT8DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT8DI16_Axis - 1 do
      Result := Result + KDT8DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT8DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT8DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT8DI16.Test;
var
  TKDT8DI16_Test: TKDT8DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT8DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT8DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT8DI16_Test := TKDT8DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT8DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT8DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT8DI16_Axis - 1 do
        TKDT8DI16_Test.TestBuff[i][j] := i * KDT8DI16_Axis + j;

{$IFDEF FPC}
  TKDT8DI16_Test.BuildKDTreeM(length(TKDT8DI16_Test.TestBuff), nil, @TKDT8DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT8DI16_Test.BuildKDTreeM(length(TKDT8DI16_Test.TestBuff), nil, TKDT8DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT8DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT8DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT8DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT8DI16_Test.Search(TKDT8DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT8DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT8DI16_Test.TestBuff));
      TKDT8DI16_Test.Search(TKDT8DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT8DI16Distance(TKDT8DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT8DI16_Test.Clear;
      { kMean test }
      TKDT8DI16_Test.BuildKDTreeWithCluster(TKDT8DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT8DI16_Test.Search(TKDT8DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT8DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT8DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT8DI16_Test);
end;


function TKDT9DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT9DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT9DI16_Node;
  function SortCompare(const p1, p2: PKDT9DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT9DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT9DI16_Source;
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
  kdBuffPtr: PKDT9DI16_SourceBuffer;
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
      axis := Depth mod KDT9DI16_Axis;
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

function TKDT9DI16.GetData(const Index: NativeInt): PKDT9DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT9DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT9DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT9DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT9DI16_Node(KDNodes[i]));
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

function TKDT9DI16.StoreBuffPtr: PKDT9DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT9DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT9DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT9DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT9DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT9DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT9DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT9DI16.BuildKDTreeWithCluster(const inBuff: TKDT9DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT9DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT9DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT9DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT9DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT9DI16.BuildKDTreeWithCluster(const inBuff: TKDT9DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT9DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DI16_BuildCall);
var
  TempStoreBuff: TKDT9DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT9DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT9DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT9DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT9DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT9DI16_Axis - 1 do
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

procedure TKDT9DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DI16_BuildMethod);
var
  TempStoreBuff: TKDT9DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT9DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT9DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT9DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT9DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT9DI16_Axis - 1 do
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


procedure TKDT9DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DI16_BuildProc);
var
  TempStoreBuff: TKDT9DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT9DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT9DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT9DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT9DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT9DI16_Axis - 1 do
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


function TKDT9DI16.Search(const buff: TKDT9DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT9DI16_Node;

var
  NearestNeighbour: PKDT9DI16_Node;

  function FindParentNode(const buffPtr: PKDT9DI16_Vec; NodePtr: PKDT9DI16_Node): PKDT9DI16_Node;
  var
    Next: PKDT9DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT9DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT9DI16_Node; const buffPtr: PKDT9DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT9DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT9DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT9DI16_Vec; const p1, p2: PKDT9DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT9DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT9DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT9DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT9DI16_Node;
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
  Parent: PKDT9DI16_Node;
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

  SearchedDistanceMin := KDT9DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT9DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT9DI16.Search(const buff: TKDT9DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT9DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT9DI16.Search(const buff: TKDT9DI16_Vec; var SearchedDistanceMin: Double): PKDT9DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT9DI16.Search(const buff: TKDT9DI16_Vec): PKDT9DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT9DI16.SearchToken(const buff: TKDT9DI16_Vec): TPascalString;
var
  p: PKDT9DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT9DI16.Search(const inBuff: TKDT9DI16_DynamicVecBuffer; var OutBuff: TKDT9DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT9DI16_DynamicVecBuffer;
  outBuffPtr: PKDT9DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT9DI16_Node;
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
        p: PKDT9DI16_Node;
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
  p: PKDT9DI16_Node;
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


procedure TKDT9DI16.Search(const inBuff: TKDT9DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT9DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT9DI16_Node;
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
        p: PKDT9DI16_Node;
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
  p: PKDT9DI16_Node;
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


procedure TKDT9DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT9DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT9DI16_Vec));
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

procedure TKDT9DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT9DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT9DI16_Vec)) <> SizeOf(TKDT9DI16_Vec) then
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

procedure TKDT9DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT9DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT9DI16.PrintNodeTree(const NodePtr: PKDT9DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT9DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT9DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT9DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT9DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT9DI16.KDT9DI16Vec(const s: SystemString): TKDT9DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT9DI16_Axis - 1 do
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
            if j >= KDT9DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT9DI16.KDT9DI16Vec(const v: TKDT9DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT9DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT9DI16.KDT9DI16Pow(const v: TKDT9DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT9DI16.KDT9DI16Distance(const v1, v2: TKDT9DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT9DI16_Axis - 1 do
      Result := Result + KDT9DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT9DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT9DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT9DI16.Test;
var
  TKDT9DI16_Test: TKDT9DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT9DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT9DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT9DI16_Test := TKDT9DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT9DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT9DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT9DI16_Axis - 1 do
        TKDT9DI16_Test.TestBuff[i][j] := i * KDT9DI16_Axis + j;

{$IFDEF FPC}
  TKDT9DI16_Test.BuildKDTreeM(length(TKDT9DI16_Test.TestBuff), nil, @TKDT9DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT9DI16_Test.BuildKDTreeM(length(TKDT9DI16_Test.TestBuff), nil, TKDT9DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT9DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT9DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT9DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT9DI16_Test.Search(TKDT9DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT9DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT9DI16_Test.TestBuff));
      TKDT9DI16_Test.Search(TKDT9DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT9DI16Distance(TKDT9DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT9DI16_Test.Clear;
      { kMean test }
      TKDT9DI16_Test.BuildKDTreeWithCluster(TKDT9DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT9DI16_Test.Search(TKDT9DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT9DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT9DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT9DI16_Test);
end;


function TKDT10DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT10DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT10DI16_Node;
  function SortCompare(const p1, p2: PKDT10DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT10DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT10DI16_Source;
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
  kdBuffPtr: PKDT10DI16_SourceBuffer;
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
      axis := Depth mod KDT10DI16_Axis;
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

function TKDT10DI16.GetData(const Index: NativeInt): PKDT10DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT10DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT10DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT10DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT10DI16_Node(KDNodes[i]));
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

function TKDT10DI16.StoreBuffPtr: PKDT10DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT10DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT10DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT10DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT10DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT10DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT10DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT10DI16.BuildKDTreeWithCluster(const inBuff: TKDT10DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT10DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT10DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT10DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT10DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT10DI16.BuildKDTreeWithCluster(const inBuff: TKDT10DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT10DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DI16_BuildCall);
var
  TempStoreBuff: TKDT10DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT10DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT10DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT10DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT10DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT10DI16_Axis - 1 do
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

procedure TKDT10DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DI16_BuildMethod);
var
  TempStoreBuff: TKDT10DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT10DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT10DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT10DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT10DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT10DI16_Axis - 1 do
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


procedure TKDT10DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DI16_BuildProc);
var
  TempStoreBuff: TKDT10DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT10DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT10DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT10DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT10DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT10DI16_Axis - 1 do
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


function TKDT10DI16.Search(const buff: TKDT10DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT10DI16_Node;

var
  NearestNeighbour: PKDT10DI16_Node;

  function FindParentNode(const buffPtr: PKDT10DI16_Vec; NodePtr: PKDT10DI16_Node): PKDT10DI16_Node;
  var
    Next: PKDT10DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT10DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT10DI16_Node; const buffPtr: PKDT10DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT10DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT10DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT10DI16_Vec; const p1, p2: PKDT10DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT10DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT10DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT10DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT10DI16_Node;
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
  Parent: PKDT10DI16_Node;
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

  SearchedDistanceMin := KDT10DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT10DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT10DI16.Search(const buff: TKDT10DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT10DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT10DI16.Search(const buff: TKDT10DI16_Vec; var SearchedDistanceMin: Double): PKDT10DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT10DI16.Search(const buff: TKDT10DI16_Vec): PKDT10DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT10DI16.SearchToken(const buff: TKDT10DI16_Vec): TPascalString;
var
  p: PKDT10DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT10DI16.Search(const inBuff: TKDT10DI16_DynamicVecBuffer; var OutBuff: TKDT10DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT10DI16_DynamicVecBuffer;
  outBuffPtr: PKDT10DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT10DI16_Node;
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
        p: PKDT10DI16_Node;
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
  p: PKDT10DI16_Node;
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


procedure TKDT10DI16.Search(const inBuff: TKDT10DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT10DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT10DI16_Node;
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
        p: PKDT10DI16_Node;
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
  p: PKDT10DI16_Node;
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


procedure TKDT10DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT10DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT10DI16_Vec));
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

procedure TKDT10DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT10DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT10DI16_Vec)) <> SizeOf(TKDT10DI16_Vec) then
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

procedure TKDT10DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT10DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT10DI16.PrintNodeTree(const NodePtr: PKDT10DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT10DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT10DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT10DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT10DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT10DI16.KDT10DI16Vec(const s: SystemString): TKDT10DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT10DI16_Axis - 1 do
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
            if j >= KDT10DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT10DI16.KDT10DI16Vec(const v: TKDT10DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT10DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT10DI16.KDT10DI16Pow(const v: TKDT10DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT10DI16.KDT10DI16Distance(const v1, v2: TKDT10DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT10DI16_Axis - 1 do
      Result := Result + KDT10DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT10DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT10DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT10DI16.Test;
var
  TKDT10DI16_Test: TKDT10DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT10DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT10DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT10DI16_Test := TKDT10DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT10DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT10DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT10DI16_Axis - 1 do
        TKDT10DI16_Test.TestBuff[i][j] := i * KDT10DI16_Axis + j;

{$IFDEF FPC}
  TKDT10DI16_Test.BuildKDTreeM(length(TKDT10DI16_Test.TestBuff), nil, @TKDT10DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT10DI16_Test.BuildKDTreeM(length(TKDT10DI16_Test.TestBuff), nil, TKDT10DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT10DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT10DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT10DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT10DI16_Test.Search(TKDT10DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT10DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT10DI16_Test.TestBuff));
      TKDT10DI16_Test.Search(TKDT10DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT10DI16Distance(TKDT10DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT10DI16_Test.Clear;
      { kMean test }
      TKDT10DI16_Test.BuildKDTreeWithCluster(TKDT10DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT10DI16_Test.Search(TKDT10DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT10DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT10DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT10DI16_Test);
end;


function TKDT11DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT11DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT11DI16_Node;
  function SortCompare(const p1, p2: PKDT11DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT11DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT11DI16_Source;
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
  kdBuffPtr: PKDT11DI16_SourceBuffer;
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
      axis := Depth mod KDT11DI16_Axis;
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

function TKDT11DI16.GetData(const Index: NativeInt): PKDT11DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT11DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT11DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT11DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT11DI16_Node(KDNodes[i]));
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

function TKDT11DI16.StoreBuffPtr: PKDT11DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT11DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT11DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT11DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT11DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT11DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT11DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT11DI16.BuildKDTreeWithCluster(const inBuff: TKDT11DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT11DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT11DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT11DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT11DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT11DI16.BuildKDTreeWithCluster(const inBuff: TKDT11DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT11DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DI16_BuildCall);
var
  TempStoreBuff: TKDT11DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT11DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT11DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT11DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT11DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT11DI16_Axis - 1 do
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

procedure TKDT11DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DI16_BuildMethod);
var
  TempStoreBuff: TKDT11DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT11DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT11DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT11DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT11DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT11DI16_Axis - 1 do
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


procedure TKDT11DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DI16_BuildProc);
var
  TempStoreBuff: TKDT11DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT11DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT11DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT11DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT11DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT11DI16_Axis - 1 do
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


function TKDT11DI16.Search(const buff: TKDT11DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT11DI16_Node;

var
  NearestNeighbour: PKDT11DI16_Node;

  function FindParentNode(const buffPtr: PKDT11DI16_Vec; NodePtr: PKDT11DI16_Node): PKDT11DI16_Node;
  var
    Next: PKDT11DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT11DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT11DI16_Node; const buffPtr: PKDT11DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT11DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT11DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT11DI16_Vec; const p1, p2: PKDT11DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT11DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT11DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT11DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT11DI16_Node;
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
  Parent: PKDT11DI16_Node;
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

  SearchedDistanceMin := KDT11DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT11DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT11DI16.Search(const buff: TKDT11DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT11DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT11DI16.Search(const buff: TKDT11DI16_Vec; var SearchedDistanceMin: Double): PKDT11DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT11DI16.Search(const buff: TKDT11DI16_Vec): PKDT11DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT11DI16.SearchToken(const buff: TKDT11DI16_Vec): TPascalString;
var
  p: PKDT11DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT11DI16.Search(const inBuff: TKDT11DI16_DynamicVecBuffer; var OutBuff: TKDT11DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT11DI16_DynamicVecBuffer;
  outBuffPtr: PKDT11DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT11DI16_Node;
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
        p: PKDT11DI16_Node;
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
  p: PKDT11DI16_Node;
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


procedure TKDT11DI16.Search(const inBuff: TKDT11DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT11DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT11DI16_Node;
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
        p: PKDT11DI16_Node;
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
  p: PKDT11DI16_Node;
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


procedure TKDT11DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT11DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT11DI16_Vec));
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

procedure TKDT11DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT11DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT11DI16_Vec)) <> SizeOf(TKDT11DI16_Vec) then
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

procedure TKDT11DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT11DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT11DI16.PrintNodeTree(const NodePtr: PKDT11DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT11DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT11DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT11DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT11DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT11DI16.KDT11DI16Vec(const s: SystemString): TKDT11DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT11DI16_Axis - 1 do
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
            if j >= KDT11DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT11DI16.KDT11DI16Vec(const v: TKDT11DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT11DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT11DI16.KDT11DI16Pow(const v: TKDT11DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT11DI16.KDT11DI16Distance(const v1, v2: TKDT11DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT11DI16_Axis - 1 do
      Result := Result + KDT11DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT11DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT11DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT11DI16.Test;
var
  TKDT11DI16_Test: TKDT11DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT11DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT11DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT11DI16_Test := TKDT11DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT11DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT11DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT11DI16_Axis - 1 do
        TKDT11DI16_Test.TestBuff[i][j] := i * KDT11DI16_Axis + j;

{$IFDEF FPC}
  TKDT11DI16_Test.BuildKDTreeM(length(TKDT11DI16_Test.TestBuff), nil, @TKDT11DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT11DI16_Test.BuildKDTreeM(length(TKDT11DI16_Test.TestBuff), nil, TKDT11DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT11DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT11DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT11DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT11DI16_Test.Search(TKDT11DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT11DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT11DI16_Test.TestBuff));
      TKDT11DI16_Test.Search(TKDT11DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT11DI16Distance(TKDT11DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT11DI16_Test.Clear;
      { kMean test }
      TKDT11DI16_Test.BuildKDTreeWithCluster(TKDT11DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT11DI16_Test.Search(TKDT11DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT11DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT11DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT11DI16_Test);
end;


function TKDT12DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT12DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT12DI16_Node;
  function SortCompare(const p1, p2: PKDT12DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT12DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT12DI16_Source;
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
  kdBuffPtr: PKDT12DI16_SourceBuffer;
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
      axis := Depth mod KDT12DI16_Axis;
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

function TKDT12DI16.GetData(const Index: NativeInt): PKDT12DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT12DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT12DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT12DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT12DI16_Node(KDNodes[i]));
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

function TKDT12DI16.StoreBuffPtr: PKDT12DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT12DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT12DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT12DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT12DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT12DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT12DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT12DI16.BuildKDTreeWithCluster(const inBuff: TKDT12DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT12DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT12DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT12DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT12DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT12DI16.BuildKDTreeWithCluster(const inBuff: TKDT12DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT12DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DI16_BuildCall);
var
  TempStoreBuff: TKDT12DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT12DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT12DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT12DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT12DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT12DI16_Axis - 1 do
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

procedure TKDT12DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DI16_BuildMethod);
var
  TempStoreBuff: TKDT12DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT12DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT12DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT12DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT12DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT12DI16_Axis - 1 do
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


procedure TKDT12DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DI16_BuildProc);
var
  TempStoreBuff: TKDT12DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT12DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT12DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT12DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT12DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT12DI16_Axis - 1 do
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


function TKDT12DI16.Search(const buff: TKDT12DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT12DI16_Node;

var
  NearestNeighbour: PKDT12DI16_Node;

  function FindParentNode(const buffPtr: PKDT12DI16_Vec; NodePtr: PKDT12DI16_Node): PKDT12DI16_Node;
  var
    Next: PKDT12DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT12DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT12DI16_Node; const buffPtr: PKDT12DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT12DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT12DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT12DI16_Vec; const p1, p2: PKDT12DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT12DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT12DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT12DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT12DI16_Node;
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
  Parent: PKDT12DI16_Node;
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

  SearchedDistanceMin := KDT12DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT12DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT12DI16.Search(const buff: TKDT12DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT12DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT12DI16.Search(const buff: TKDT12DI16_Vec; var SearchedDistanceMin: Double): PKDT12DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT12DI16.Search(const buff: TKDT12DI16_Vec): PKDT12DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT12DI16.SearchToken(const buff: TKDT12DI16_Vec): TPascalString;
var
  p: PKDT12DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT12DI16.Search(const inBuff: TKDT12DI16_DynamicVecBuffer; var OutBuff: TKDT12DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT12DI16_DynamicVecBuffer;
  outBuffPtr: PKDT12DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT12DI16_Node;
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
        p: PKDT12DI16_Node;
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
  p: PKDT12DI16_Node;
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


procedure TKDT12DI16.Search(const inBuff: TKDT12DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT12DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT12DI16_Node;
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
        p: PKDT12DI16_Node;
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
  p: PKDT12DI16_Node;
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


procedure TKDT12DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT12DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT12DI16_Vec));
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

procedure TKDT12DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT12DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT12DI16_Vec)) <> SizeOf(TKDT12DI16_Vec) then
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

procedure TKDT12DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT12DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT12DI16.PrintNodeTree(const NodePtr: PKDT12DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT12DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT12DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT12DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT12DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT12DI16.KDT12DI16Vec(const s: SystemString): TKDT12DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT12DI16_Axis - 1 do
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
            if j >= KDT12DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT12DI16.KDT12DI16Vec(const v: TKDT12DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT12DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT12DI16.KDT12DI16Pow(const v: TKDT12DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT12DI16.KDT12DI16Distance(const v1, v2: TKDT12DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT12DI16_Axis - 1 do
      Result := Result + KDT12DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT12DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT12DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT12DI16.Test;
var
  TKDT12DI16_Test: TKDT12DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT12DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT12DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT12DI16_Test := TKDT12DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT12DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT12DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT12DI16_Axis - 1 do
        TKDT12DI16_Test.TestBuff[i][j] := i * KDT12DI16_Axis + j;

{$IFDEF FPC}
  TKDT12DI16_Test.BuildKDTreeM(length(TKDT12DI16_Test.TestBuff), nil, @TKDT12DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT12DI16_Test.BuildKDTreeM(length(TKDT12DI16_Test.TestBuff), nil, TKDT12DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT12DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT12DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT12DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT12DI16_Test.Search(TKDT12DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT12DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT12DI16_Test.TestBuff));
      TKDT12DI16_Test.Search(TKDT12DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT12DI16Distance(TKDT12DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT12DI16_Test.Clear;
      { kMean test }
      TKDT12DI16_Test.BuildKDTreeWithCluster(TKDT12DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT12DI16_Test.Search(TKDT12DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT12DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT12DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT12DI16_Test);
end;


function TKDT13DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT13DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT13DI16_Node;
  function SortCompare(const p1, p2: PKDT13DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT13DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT13DI16_Source;
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
  kdBuffPtr: PKDT13DI16_SourceBuffer;
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
      axis := Depth mod KDT13DI16_Axis;
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

function TKDT13DI16.GetData(const Index: NativeInt): PKDT13DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT13DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT13DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT13DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT13DI16_Node(KDNodes[i]));
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

function TKDT13DI16.StoreBuffPtr: PKDT13DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT13DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT13DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT13DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT13DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT13DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT13DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT13DI16.BuildKDTreeWithCluster(const inBuff: TKDT13DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT13DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT13DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT13DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT13DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT13DI16.BuildKDTreeWithCluster(const inBuff: TKDT13DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT13DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DI16_BuildCall);
var
  TempStoreBuff: TKDT13DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT13DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT13DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT13DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT13DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT13DI16_Axis - 1 do
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

procedure TKDT13DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DI16_BuildMethod);
var
  TempStoreBuff: TKDT13DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT13DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT13DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT13DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT13DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT13DI16_Axis - 1 do
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


procedure TKDT13DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DI16_BuildProc);
var
  TempStoreBuff: TKDT13DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT13DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT13DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT13DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT13DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT13DI16_Axis - 1 do
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


function TKDT13DI16.Search(const buff: TKDT13DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT13DI16_Node;

var
  NearestNeighbour: PKDT13DI16_Node;

  function FindParentNode(const buffPtr: PKDT13DI16_Vec; NodePtr: PKDT13DI16_Node): PKDT13DI16_Node;
  var
    Next: PKDT13DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT13DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT13DI16_Node; const buffPtr: PKDT13DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT13DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT13DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT13DI16_Vec; const p1, p2: PKDT13DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT13DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT13DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT13DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT13DI16_Node;
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
  Parent: PKDT13DI16_Node;
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

  SearchedDistanceMin := KDT13DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT13DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT13DI16.Search(const buff: TKDT13DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT13DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT13DI16.Search(const buff: TKDT13DI16_Vec; var SearchedDistanceMin: Double): PKDT13DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT13DI16.Search(const buff: TKDT13DI16_Vec): PKDT13DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT13DI16.SearchToken(const buff: TKDT13DI16_Vec): TPascalString;
var
  p: PKDT13DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT13DI16.Search(const inBuff: TKDT13DI16_DynamicVecBuffer; var OutBuff: TKDT13DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT13DI16_DynamicVecBuffer;
  outBuffPtr: PKDT13DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT13DI16_Node;
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
        p: PKDT13DI16_Node;
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
  p: PKDT13DI16_Node;
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


procedure TKDT13DI16.Search(const inBuff: TKDT13DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT13DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT13DI16_Node;
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
        p: PKDT13DI16_Node;
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
  p: PKDT13DI16_Node;
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


procedure TKDT13DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT13DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT13DI16_Vec));
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

procedure TKDT13DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT13DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT13DI16_Vec)) <> SizeOf(TKDT13DI16_Vec) then
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

procedure TKDT13DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT13DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT13DI16.PrintNodeTree(const NodePtr: PKDT13DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT13DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT13DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT13DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT13DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT13DI16.KDT13DI16Vec(const s: SystemString): TKDT13DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT13DI16_Axis - 1 do
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
            if j >= KDT13DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT13DI16.KDT13DI16Vec(const v: TKDT13DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT13DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT13DI16.KDT13DI16Pow(const v: TKDT13DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT13DI16.KDT13DI16Distance(const v1, v2: TKDT13DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT13DI16_Axis - 1 do
      Result := Result + KDT13DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT13DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT13DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT13DI16.Test;
var
  TKDT13DI16_Test: TKDT13DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT13DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT13DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT13DI16_Test := TKDT13DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT13DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT13DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT13DI16_Axis - 1 do
        TKDT13DI16_Test.TestBuff[i][j] := i * KDT13DI16_Axis + j;

{$IFDEF FPC}
  TKDT13DI16_Test.BuildKDTreeM(length(TKDT13DI16_Test.TestBuff), nil, @TKDT13DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT13DI16_Test.BuildKDTreeM(length(TKDT13DI16_Test.TestBuff), nil, TKDT13DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT13DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT13DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT13DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT13DI16_Test.Search(TKDT13DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT13DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT13DI16_Test.TestBuff));
      TKDT13DI16_Test.Search(TKDT13DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT13DI16Distance(TKDT13DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT13DI16_Test.Clear;
      { kMean test }
      TKDT13DI16_Test.BuildKDTreeWithCluster(TKDT13DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT13DI16_Test.Search(TKDT13DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT13DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT13DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT13DI16_Test);
end;


function TKDT14DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT14DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT14DI16_Node;
  function SortCompare(const p1, p2: PKDT14DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT14DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT14DI16_Source;
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
  kdBuffPtr: PKDT14DI16_SourceBuffer;
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
      axis := Depth mod KDT14DI16_Axis;
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

function TKDT14DI16.GetData(const Index: NativeInt): PKDT14DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT14DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT14DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT14DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT14DI16_Node(KDNodes[i]));
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

function TKDT14DI16.StoreBuffPtr: PKDT14DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT14DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT14DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT14DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT14DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT14DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT14DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT14DI16.BuildKDTreeWithCluster(const inBuff: TKDT14DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT14DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT14DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT14DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT14DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT14DI16.BuildKDTreeWithCluster(const inBuff: TKDT14DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT14DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DI16_BuildCall);
var
  TempStoreBuff: TKDT14DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT14DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT14DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT14DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT14DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT14DI16_Axis - 1 do
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

procedure TKDT14DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DI16_BuildMethod);
var
  TempStoreBuff: TKDT14DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT14DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT14DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT14DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT14DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT14DI16_Axis - 1 do
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


procedure TKDT14DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DI16_BuildProc);
var
  TempStoreBuff: TKDT14DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT14DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT14DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT14DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT14DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT14DI16_Axis - 1 do
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


function TKDT14DI16.Search(const buff: TKDT14DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT14DI16_Node;

var
  NearestNeighbour: PKDT14DI16_Node;

  function FindParentNode(const buffPtr: PKDT14DI16_Vec; NodePtr: PKDT14DI16_Node): PKDT14DI16_Node;
  var
    Next: PKDT14DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT14DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT14DI16_Node; const buffPtr: PKDT14DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT14DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT14DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT14DI16_Vec; const p1, p2: PKDT14DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT14DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT14DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT14DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT14DI16_Node;
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
  Parent: PKDT14DI16_Node;
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

  SearchedDistanceMin := KDT14DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT14DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT14DI16.Search(const buff: TKDT14DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT14DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT14DI16.Search(const buff: TKDT14DI16_Vec; var SearchedDistanceMin: Double): PKDT14DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT14DI16.Search(const buff: TKDT14DI16_Vec): PKDT14DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT14DI16.SearchToken(const buff: TKDT14DI16_Vec): TPascalString;
var
  p: PKDT14DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT14DI16.Search(const inBuff: TKDT14DI16_DynamicVecBuffer; var OutBuff: TKDT14DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT14DI16_DynamicVecBuffer;
  outBuffPtr: PKDT14DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT14DI16_Node;
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
        p: PKDT14DI16_Node;
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
  p: PKDT14DI16_Node;
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


procedure TKDT14DI16.Search(const inBuff: TKDT14DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT14DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT14DI16_Node;
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
        p: PKDT14DI16_Node;
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
  p: PKDT14DI16_Node;
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


procedure TKDT14DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT14DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT14DI16_Vec));
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

procedure TKDT14DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT14DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT14DI16_Vec)) <> SizeOf(TKDT14DI16_Vec) then
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

procedure TKDT14DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT14DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT14DI16.PrintNodeTree(const NodePtr: PKDT14DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT14DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT14DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT14DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT14DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT14DI16.KDT14DI16Vec(const s: SystemString): TKDT14DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT14DI16_Axis - 1 do
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
            if j >= KDT14DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT14DI16.KDT14DI16Vec(const v: TKDT14DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT14DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT14DI16.KDT14DI16Pow(const v: TKDT14DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT14DI16.KDT14DI16Distance(const v1, v2: TKDT14DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT14DI16_Axis - 1 do
      Result := Result + KDT14DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT14DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT14DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT14DI16.Test;
var
  TKDT14DI16_Test: TKDT14DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT14DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT14DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT14DI16_Test := TKDT14DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT14DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT14DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT14DI16_Axis - 1 do
        TKDT14DI16_Test.TestBuff[i][j] := i * KDT14DI16_Axis + j;

{$IFDEF FPC}
  TKDT14DI16_Test.BuildKDTreeM(length(TKDT14DI16_Test.TestBuff), nil, @TKDT14DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT14DI16_Test.BuildKDTreeM(length(TKDT14DI16_Test.TestBuff), nil, TKDT14DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT14DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT14DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT14DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT14DI16_Test.Search(TKDT14DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT14DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT14DI16_Test.TestBuff));
      TKDT14DI16_Test.Search(TKDT14DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT14DI16Distance(TKDT14DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT14DI16_Test.Clear;
      { kMean test }
      TKDT14DI16_Test.BuildKDTreeWithCluster(TKDT14DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT14DI16_Test.Search(TKDT14DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT14DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT14DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT14DI16_Test);
end;


function TKDT15DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT15DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT15DI16_Node;
  function SortCompare(const p1, p2: PKDT15DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT15DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT15DI16_Source;
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
  kdBuffPtr: PKDT15DI16_SourceBuffer;
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
      axis := Depth mod KDT15DI16_Axis;
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

function TKDT15DI16.GetData(const Index: NativeInt): PKDT15DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT15DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT15DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT15DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT15DI16_Node(KDNodes[i]));
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

function TKDT15DI16.StoreBuffPtr: PKDT15DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT15DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT15DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT15DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT15DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT15DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT15DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT15DI16.BuildKDTreeWithCluster(const inBuff: TKDT15DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT15DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT15DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT15DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT15DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT15DI16.BuildKDTreeWithCluster(const inBuff: TKDT15DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT15DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DI16_BuildCall);
var
  TempStoreBuff: TKDT15DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT15DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT15DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT15DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT15DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT15DI16_Axis - 1 do
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

procedure TKDT15DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DI16_BuildMethod);
var
  TempStoreBuff: TKDT15DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT15DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT15DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT15DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT15DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT15DI16_Axis - 1 do
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


procedure TKDT15DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DI16_BuildProc);
var
  TempStoreBuff: TKDT15DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT15DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT15DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT15DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT15DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT15DI16_Axis - 1 do
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


function TKDT15DI16.Search(const buff: TKDT15DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT15DI16_Node;

var
  NearestNeighbour: PKDT15DI16_Node;

  function FindParentNode(const buffPtr: PKDT15DI16_Vec; NodePtr: PKDT15DI16_Node): PKDT15DI16_Node;
  var
    Next: PKDT15DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT15DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT15DI16_Node; const buffPtr: PKDT15DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT15DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT15DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT15DI16_Vec; const p1, p2: PKDT15DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT15DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT15DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT15DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT15DI16_Node;
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
  Parent: PKDT15DI16_Node;
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

  SearchedDistanceMin := KDT15DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT15DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT15DI16.Search(const buff: TKDT15DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT15DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT15DI16.Search(const buff: TKDT15DI16_Vec; var SearchedDistanceMin: Double): PKDT15DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT15DI16.Search(const buff: TKDT15DI16_Vec): PKDT15DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT15DI16.SearchToken(const buff: TKDT15DI16_Vec): TPascalString;
var
  p: PKDT15DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT15DI16.Search(const inBuff: TKDT15DI16_DynamicVecBuffer; var OutBuff: TKDT15DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT15DI16_DynamicVecBuffer;
  outBuffPtr: PKDT15DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT15DI16_Node;
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
        p: PKDT15DI16_Node;
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
  p: PKDT15DI16_Node;
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


procedure TKDT15DI16.Search(const inBuff: TKDT15DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT15DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT15DI16_Node;
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
        p: PKDT15DI16_Node;
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
  p: PKDT15DI16_Node;
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


procedure TKDT15DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT15DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT15DI16_Vec));
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

procedure TKDT15DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT15DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT15DI16_Vec)) <> SizeOf(TKDT15DI16_Vec) then
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

procedure TKDT15DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT15DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT15DI16.PrintNodeTree(const NodePtr: PKDT15DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT15DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT15DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT15DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT15DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT15DI16.KDT15DI16Vec(const s: SystemString): TKDT15DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT15DI16_Axis - 1 do
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
            if j >= KDT15DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT15DI16.KDT15DI16Vec(const v: TKDT15DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT15DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT15DI16.KDT15DI16Pow(const v: TKDT15DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT15DI16.KDT15DI16Distance(const v1, v2: TKDT15DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT15DI16_Axis - 1 do
      Result := Result + KDT15DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT15DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT15DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT15DI16.Test;
var
  TKDT15DI16_Test: TKDT15DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT15DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT15DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT15DI16_Test := TKDT15DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT15DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT15DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT15DI16_Axis - 1 do
        TKDT15DI16_Test.TestBuff[i][j] := i * KDT15DI16_Axis + j;

{$IFDEF FPC}
  TKDT15DI16_Test.BuildKDTreeM(length(TKDT15DI16_Test.TestBuff), nil, @TKDT15DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT15DI16_Test.BuildKDTreeM(length(TKDT15DI16_Test.TestBuff), nil, TKDT15DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT15DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT15DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT15DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT15DI16_Test.Search(TKDT15DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT15DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT15DI16_Test.TestBuff));
      TKDT15DI16_Test.Search(TKDT15DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT15DI16Distance(TKDT15DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT15DI16_Test.Clear;
      { kMean test }
      TKDT15DI16_Test.BuildKDTreeWithCluster(TKDT15DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT15DI16_Test.Search(TKDT15DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT15DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT15DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT15DI16_Test);
end;


function TKDT16DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT16DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT16DI16_Node;
  function SortCompare(const p1, p2: PKDT16DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT16DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT16DI16_Source;
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
  kdBuffPtr: PKDT16DI16_SourceBuffer;
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
      axis := Depth mod KDT16DI16_Axis;
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

function TKDT16DI16.GetData(const Index: NativeInt): PKDT16DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT16DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT16DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT16DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT16DI16_Node(KDNodes[i]));
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

function TKDT16DI16.StoreBuffPtr: PKDT16DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT16DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT16DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT16DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT16DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT16DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT16DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT16DI16.BuildKDTreeWithCluster(const inBuff: TKDT16DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT16DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT16DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT16DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT16DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT16DI16.BuildKDTreeWithCluster(const inBuff: TKDT16DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT16DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DI16_BuildCall);
var
  TempStoreBuff: TKDT16DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT16DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT16DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT16DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT16DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT16DI16_Axis - 1 do
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

procedure TKDT16DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DI16_BuildMethod);
var
  TempStoreBuff: TKDT16DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT16DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT16DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT16DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT16DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT16DI16_Axis - 1 do
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


procedure TKDT16DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DI16_BuildProc);
var
  TempStoreBuff: TKDT16DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT16DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT16DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT16DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT16DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT16DI16_Axis - 1 do
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


function TKDT16DI16.Search(const buff: TKDT16DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT16DI16_Node;

var
  NearestNeighbour: PKDT16DI16_Node;

  function FindParentNode(const buffPtr: PKDT16DI16_Vec; NodePtr: PKDT16DI16_Node): PKDT16DI16_Node;
  var
    Next: PKDT16DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT16DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT16DI16_Node; const buffPtr: PKDT16DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT16DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT16DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT16DI16_Vec; const p1, p2: PKDT16DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT16DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT16DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT16DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT16DI16_Node;
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
  Parent: PKDT16DI16_Node;
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

  SearchedDistanceMin := KDT16DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT16DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT16DI16.Search(const buff: TKDT16DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT16DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT16DI16.Search(const buff: TKDT16DI16_Vec; var SearchedDistanceMin: Double): PKDT16DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT16DI16.Search(const buff: TKDT16DI16_Vec): PKDT16DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT16DI16.SearchToken(const buff: TKDT16DI16_Vec): TPascalString;
var
  p: PKDT16DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT16DI16.Search(const inBuff: TKDT16DI16_DynamicVecBuffer; var OutBuff: TKDT16DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT16DI16_DynamicVecBuffer;
  outBuffPtr: PKDT16DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT16DI16_Node;
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
        p: PKDT16DI16_Node;
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
  p: PKDT16DI16_Node;
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


procedure TKDT16DI16.Search(const inBuff: TKDT16DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT16DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT16DI16_Node;
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
        p: PKDT16DI16_Node;
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
  p: PKDT16DI16_Node;
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


procedure TKDT16DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT16DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT16DI16_Vec));
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

procedure TKDT16DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT16DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT16DI16_Vec)) <> SizeOf(TKDT16DI16_Vec) then
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

procedure TKDT16DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT16DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT16DI16.PrintNodeTree(const NodePtr: PKDT16DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT16DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT16DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT16DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT16DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT16DI16.KDT16DI16Vec(const s: SystemString): TKDT16DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT16DI16_Axis - 1 do
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
            if j >= KDT16DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT16DI16.KDT16DI16Vec(const v: TKDT16DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT16DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT16DI16.KDT16DI16Pow(const v: TKDT16DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT16DI16.KDT16DI16Distance(const v1, v2: TKDT16DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT16DI16_Axis - 1 do
      Result := Result + KDT16DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT16DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT16DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT16DI16.Test;
var
  TKDT16DI16_Test: TKDT16DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT16DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT16DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT16DI16_Test := TKDT16DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT16DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT16DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT16DI16_Axis - 1 do
        TKDT16DI16_Test.TestBuff[i][j] := i * KDT16DI16_Axis + j;

{$IFDEF FPC}
  TKDT16DI16_Test.BuildKDTreeM(length(TKDT16DI16_Test.TestBuff), nil, @TKDT16DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT16DI16_Test.BuildKDTreeM(length(TKDT16DI16_Test.TestBuff), nil, TKDT16DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT16DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT16DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT16DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT16DI16_Test.Search(TKDT16DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT16DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT16DI16_Test.TestBuff));
      TKDT16DI16_Test.Search(TKDT16DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT16DI16Distance(TKDT16DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT16DI16_Test.Clear;
      { kMean test }
      TKDT16DI16_Test.BuildKDTreeWithCluster(TKDT16DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT16DI16_Test.Search(TKDT16DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT16DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT16DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT16DI16_Test);
end;


function TKDT17DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT17DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT17DI16_Node;
  function SortCompare(const p1, p2: PKDT17DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT17DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT17DI16_Source;
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
  kdBuffPtr: PKDT17DI16_SourceBuffer;
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
      axis := Depth mod KDT17DI16_Axis;
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

function TKDT17DI16.GetData(const Index: NativeInt): PKDT17DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT17DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT17DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT17DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT17DI16_Node(KDNodes[i]));
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

function TKDT17DI16.StoreBuffPtr: PKDT17DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT17DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT17DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT17DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT17DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT17DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT17DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT17DI16.BuildKDTreeWithCluster(const inBuff: TKDT17DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT17DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT17DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT17DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT17DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT17DI16.BuildKDTreeWithCluster(const inBuff: TKDT17DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT17DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DI16_BuildCall);
var
  TempStoreBuff: TKDT17DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT17DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT17DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT17DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT17DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT17DI16_Axis - 1 do
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

procedure TKDT17DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DI16_BuildMethod);
var
  TempStoreBuff: TKDT17DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT17DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT17DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT17DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT17DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT17DI16_Axis - 1 do
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


procedure TKDT17DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DI16_BuildProc);
var
  TempStoreBuff: TKDT17DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT17DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT17DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT17DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT17DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT17DI16_Axis - 1 do
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


function TKDT17DI16.Search(const buff: TKDT17DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT17DI16_Node;

var
  NearestNeighbour: PKDT17DI16_Node;

  function FindParentNode(const buffPtr: PKDT17DI16_Vec; NodePtr: PKDT17DI16_Node): PKDT17DI16_Node;
  var
    Next: PKDT17DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT17DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT17DI16_Node; const buffPtr: PKDT17DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT17DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT17DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT17DI16_Vec; const p1, p2: PKDT17DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT17DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT17DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT17DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT17DI16_Node;
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
  Parent: PKDT17DI16_Node;
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

  SearchedDistanceMin := KDT17DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT17DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT17DI16.Search(const buff: TKDT17DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT17DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT17DI16.Search(const buff: TKDT17DI16_Vec; var SearchedDistanceMin: Double): PKDT17DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT17DI16.Search(const buff: TKDT17DI16_Vec): PKDT17DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT17DI16.SearchToken(const buff: TKDT17DI16_Vec): TPascalString;
var
  p: PKDT17DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT17DI16.Search(const inBuff: TKDT17DI16_DynamicVecBuffer; var OutBuff: TKDT17DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT17DI16_DynamicVecBuffer;
  outBuffPtr: PKDT17DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT17DI16_Node;
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
        p: PKDT17DI16_Node;
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
  p: PKDT17DI16_Node;
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


procedure TKDT17DI16.Search(const inBuff: TKDT17DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT17DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT17DI16_Node;
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
        p: PKDT17DI16_Node;
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
  p: PKDT17DI16_Node;
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


procedure TKDT17DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT17DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT17DI16_Vec));
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

procedure TKDT17DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT17DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT17DI16_Vec)) <> SizeOf(TKDT17DI16_Vec) then
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

procedure TKDT17DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT17DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT17DI16.PrintNodeTree(const NodePtr: PKDT17DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT17DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT17DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT17DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT17DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT17DI16.KDT17DI16Vec(const s: SystemString): TKDT17DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT17DI16_Axis - 1 do
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
            if j >= KDT17DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT17DI16.KDT17DI16Vec(const v: TKDT17DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT17DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT17DI16.KDT17DI16Pow(const v: TKDT17DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT17DI16.KDT17DI16Distance(const v1, v2: TKDT17DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT17DI16_Axis - 1 do
      Result := Result + KDT17DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT17DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT17DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT17DI16.Test;
var
  TKDT17DI16_Test: TKDT17DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT17DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT17DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT17DI16_Test := TKDT17DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT17DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT17DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT17DI16_Axis - 1 do
        TKDT17DI16_Test.TestBuff[i][j] := i * KDT17DI16_Axis + j;

{$IFDEF FPC}
  TKDT17DI16_Test.BuildKDTreeM(length(TKDT17DI16_Test.TestBuff), nil, @TKDT17DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT17DI16_Test.BuildKDTreeM(length(TKDT17DI16_Test.TestBuff), nil, TKDT17DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT17DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT17DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT17DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT17DI16_Test.Search(TKDT17DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT17DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT17DI16_Test.TestBuff));
      TKDT17DI16_Test.Search(TKDT17DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT17DI16Distance(TKDT17DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT17DI16_Test.Clear;
      { kMean test }
      TKDT17DI16_Test.BuildKDTreeWithCluster(TKDT17DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT17DI16_Test.Search(TKDT17DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT17DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT17DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT17DI16_Test);
end;


function TKDT18DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT18DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT18DI16_Node;
  function SortCompare(const p1, p2: PKDT18DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT18DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT18DI16_Source;
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
  kdBuffPtr: PKDT18DI16_SourceBuffer;
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
      axis := Depth mod KDT18DI16_Axis;
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

function TKDT18DI16.GetData(const Index: NativeInt): PKDT18DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT18DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT18DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT18DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT18DI16_Node(KDNodes[i]));
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

function TKDT18DI16.StoreBuffPtr: PKDT18DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT18DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT18DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT18DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT18DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT18DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT18DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT18DI16.BuildKDTreeWithCluster(const inBuff: TKDT18DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT18DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT18DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT18DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT18DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT18DI16.BuildKDTreeWithCluster(const inBuff: TKDT18DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT18DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DI16_BuildCall);
var
  TempStoreBuff: TKDT18DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT18DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT18DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT18DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT18DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT18DI16_Axis - 1 do
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

procedure TKDT18DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DI16_BuildMethod);
var
  TempStoreBuff: TKDT18DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT18DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT18DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT18DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT18DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT18DI16_Axis - 1 do
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


procedure TKDT18DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DI16_BuildProc);
var
  TempStoreBuff: TKDT18DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT18DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT18DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT18DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT18DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT18DI16_Axis - 1 do
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


function TKDT18DI16.Search(const buff: TKDT18DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT18DI16_Node;

var
  NearestNeighbour: PKDT18DI16_Node;

  function FindParentNode(const buffPtr: PKDT18DI16_Vec; NodePtr: PKDT18DI16_Node): PKDT18DI16_Node;
  var
    Next: PKDT18DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT18DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT18DI16_Node; const buffPtr: PKDT18DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT18DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT18DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT18DI16_Vec; const p1, p2: PKDT18DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT18DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT18DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT18DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT18DI16_Node;
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
  Parent: PKDT18DI16_Node;
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

  SearchedDistanceMin := KDT18DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT18DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT18DI16.Search(const buff: TKDT18DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT18DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT18DI16.Search(const buff: TKDT18DI16_Vec; var SearchedDistanceMin: Double): PKDT18DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT18DI16.Search(const buff: TKDT18DI16_Vec): PKDT18DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT18DI16.SearchToken(const buff: TKDT18DI16_Vec): TPascalString;
var
  p: PKDT18DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT18DI16.Search(const inBuff: TKDT18DI16_DynamicVecBuffer; var OutBuff: TKDT18DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT18DI16_DynamicVecBuffer;
  outBuffPtr: PKDT18DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT18DI16_Node;
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
        p: PKDT18DI16_Node;
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
  p: PKDT18DI16_Node;
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


procedure TKDT18DI16.Search(const inBuff: TKDT18DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT18DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT18DI16_Node;
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
        p: PKDT18DI16_Node;
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
  p: PKDT18DI16_Node;
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


procedure TKDT18DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT18DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT18DI16_Vec));
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

procedure TKDT18DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT18DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT18DI16_Vec)) <> SizeOf(TKDT18DI16_Vec) then
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

procedure TKDT18DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT18DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT18DI16.PrintNodeTree(const NodePtr: PKDT18DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT18DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT18DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT18DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT18DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT18DI16.KDT18DI16Vec(const s: SystemString): TKDT18DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT18DI16_Axis - 1 do
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
            if j >= KDT18DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT18DI16.KDT18DI16Vec(const v: TKDT18DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT18DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT18DI16.KDT18DI16Pow(const v: TKDT18DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT18DI16.KDT18DI16Distance(const v1, v2: TKDT18DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT18DI16_Axis - 1 do
      Result := Result + KDT18DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT18DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT18DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT18DI16.Test;
var
  TKDT18DI16_Test: TKDT18DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT18DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT18DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT18DI16_Test := TKDT18DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT18DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT18DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT18DI16_Axis - 1 do
        TKDT18DI16_Test.TestBuff[i][j] := i * KDT18DI16_Axis + j;

{$IFDEF FPC}
  TKDT18DI16_Test.BuildKDTreeM(length(TKDT18DI16_Test.TestBuff), nil, @TKDT18DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT18DI16_Test.BuildKDTreeM(length(TKDT18DI16_Test.TestBuff), nil, TKDT18DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT18DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT18DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT18DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT18DI16_Test.Search(TKDT18DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT18DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT18DI16_Test.TestBuff));
      TKDT18DI16_Test.Search(TKDT18DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT18DI16Distance(TKDT18DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT18DI16_Test.Clear;
      { kMean test }
      TKDT18DI16_Test.BuildKDTreeWithCluster(TKDT18DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT18DI16_Test.Search(TKDT18DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT18DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT18DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT18DI16_Test);
end;


function TKDT19DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT19DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT19DI16_Node;
  function SortCompare(const p1, p2: PKDT19DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT19DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT19DI16_Source;
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
  kdBuffPtr: PKDT19DI16_SourceBuffer;
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
      axis := Depth mod KDT19DI16_Axis;
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

function TKDT19DI16.GetData(const Index: NativeInt): PKDT19DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT19DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT19DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT19DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT19DI16_Node(KDNodes[i]));
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

function TKDT19DI16.StoreBuffPtr: PKDT19DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT19DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT19DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT19DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT19DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT19DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT19DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT19DI16.BuildKDTreeWithCluster(const inBuff: TKDT19DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT19DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT19DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT19DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT19DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT19DI16.BuildKDTreeWithCluster(const inBuff: TKDT19DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT19DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DI16_BuildCall);
var
  TempStoreBuff: TKDT19DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT19DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT19DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT19DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT19DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT19DI16_Axis - 1 do
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

procedure TKDT19DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DI16_BuildMethod);
var
  TempStoreBuff: TKDT19DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT19DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT19DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT19DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT19DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT19DI16_Axis - 1 do
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


procedure TKDT19DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DI16_BuildProc);
var
  TempStoreBuff: TKDT19DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT19DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT19DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT19DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT19DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT19DI16_Axis - 1 do
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


function TKDT19DI16.Search(const buff: TKDT19DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT19DI16_Node;

var
  NearestNeighbour: PKDT19DI16_Node;

  function FindParentNode(const buffPtr: PKDT19DI16_Vec; NodePtr: PKDT19DI16_Node): PKDT19DI16_Node;
  var
    Next: PKDT19DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT19DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT19DI16_Node; const buffPtr: PKDT19DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT19DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT19DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT19DI16_Vec; const p1, p2: PKDT19DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT19DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT19DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT19DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT19DI16_Node;
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
  Parent: PKDT19DI16_Node;
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

  SearchedDistanceMin := KDT19DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT19DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT19DI16.Search(const buff: TKDT19DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT19DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT19DI16.Search(const buff: TKDT19DI16_Vec; var SearchedDistanceMin: Double): PKDT19DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT19DI16.Search(const buff: TKDT19DI16_Vec): PKDT19DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT19DI16.SearchToken(const buff: TKDT19DI16_Vec): TPascalString;
var
  p: PKDT19DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT19DI16.Search(const inBuff: TKDT19DI16_DynamicVecBuffer; var OutBuff: TKDT19DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT19DI16_DynamicVecBuffer;
  outBuffPtr: PKDT19DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT19DI16_Node;
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
        p: PKDT19DI16_Node;
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
  p: PKDT19DI16_Node;
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


procedure TKDT19DI16.Search(const inBuff: TKDT19DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT19DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT19DI16_Node;
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
        p: PKDT19DI16_Node;
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
  p: PKDT19DI16_Node;
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


procedure TKDT19DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT19DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT19DI16_Vec));
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

procedure TKDT19DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT19DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT19DI16_Vec)) <> SizeOf(TKDT19DI16_Vec) then
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

procedure TKDT19DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT19DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT19DI16.PrintNodeTree(const NodePtr: PKDT19DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT19DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT19DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT19DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT19DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT19DI16.KDT19DI16Vec(const s: SystemString): TKDT19DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT19DI16_Axis - 1 do
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
            if j >= KDT19DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT19DI16.KDT19DI16Vec(const v: TKDT19DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT19DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT19DI16.KDT19DI16Pow(const v: TKDT19DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT19DI16.KDT19DI16Distance(const v1, v2: TKDT19DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT19DI16_Axis - 1 do
      Result := Result + KDT19DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT19DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT19DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT19DI16.Test;
var
  TKDT19DI16_Test: TKDT19DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT19DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT19DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT19DI16_Test := TKDT19DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT19DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT19DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT19DI16_Axis - 1 do
        TKDT19DI16_Test.TestBuff[i][j] := i * KDT19DI16_Axis + j;

{$IFDEF FPC}
  TKDT19DI16_Test.BuildKDTreeM(length(TKDT19DI16_Test.TestBuff), nil, @TKDT19DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT19DI16_Test.BuildKDTreeM(length(TKDT19DI16_Test.TestBuff), nil, TKDT19DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT19DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT19DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT19DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT19DI16_Test.Search(TKDT19DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT19DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT19DI16_Test.TestBuff));
      TKDT19DI16_Test.Search(TKDT19DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT19DI16Distance(TKDT19DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT19DI16_Test.Clear;
      { kMean test }
      TKDT19DI16_Test.BuildKDTreeWithCluster(TKDT19DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT19DI16_Test.Search(TKDT19DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT19DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT19DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT19DI16_Test);
end;


function TKDT20DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT20DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT20DI16_Node;
  function SortCompare(const p1, p2: PKDT20DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT20DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT20DI16_Source;
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
  kdBuffPtr: PKDT20DI16_SourceBuffer;
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
      axis := Depth mod KDT20DI16_Axis;
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

function TKDT20DI16.GetData(const Index: NativeInt): PKDT20DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT20DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT20DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT20DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT20DI16_Node(KDNodes[i]));
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

function TKDT20DI16.StoreBuffPtr: PKDT20DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT20DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT20DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT20DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT20DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT20DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT20DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT20DI16.BuildKDTreeWithCluster(const inBuff: TKDT20DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT20DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT20DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT20DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT20DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT20DI16.BuildKDTreeWithCluster(const inBuff: TKDT20DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT20DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DI16_BuildCall);
var
  TempStoreBuff: TKDT20DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT20DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT20DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT20DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT20DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT20DI16_Axis - 1 do
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

procedure TKDT20DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DI16_BuildMethod);
var
  TempStoreBuff: TKDT20DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT20DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT20DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT20DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT20DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT20DI16_Axis - 1 do
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


procedure TKDT20DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DI16_BuildProc);
var
  TempStoreBuff: TKDT20DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT20DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT20DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT20DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT20DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT20DI16_Axis - 1 do
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


function TKDT20DI16.Search(const buff: TKDT20DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT20DI16_Node;

var
  NearestNeighbour: PKDT20DI16_Node;

  function FindParentNode(const buffPtr: PKDT20DI16_Vec; NodePtr: PKDT20DI16_Node): PKDT20DI16_Node;
  var
    Next: PKDT20DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT20DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT20DI16_Node; const buffPtr: PKDT20DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT20DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT20DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT20DI16_Vec; const p1, p2: PKDT20DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT20DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT20DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT20DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT20DI16_Node;
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
  Parent: PKDT20DI16_Node;
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

  SearchedDistanceMin := KDT20DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT20DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT20DI16.Search(const buff: TKDT20DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT20DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT20DI16.Search(const buff: TKDT20DI16_Vec; var SearchedDistanceMin: Double): PKDT20DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT20DI16.Search(const buff: TKDT20DI16_Vec): PKDT20DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT20DI16.SearchToken(const buff: TKDT20DI16_Vec): TPascalString;
var
  p: PKDT20DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT20DI16.Search(const inBuff: TKDT20DI16_DynamicVecBuffer; var OutBuff: TKDT20DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT20DI16_DynamicVecBuffer;
  outBuffPtr: PKDT20DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT20DI16_Node;
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
        p: PKDT20DI16_Node;
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
  p: PKDT20DI16_Node;
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


procedure TKDT20DI16.Search(const inBuff: TKDT20DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT20DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT20DI16_Node;
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
        p: PKDT20DI16_Node;
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
  p: PKDT20DI16_Node;
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


procedure TKDT20DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT20DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT20DI16_Vec));
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

procedure TKDT20DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT20DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT20DI16_Vec)) <> SizeOf(TKDT20DI16_Vec) then
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

procedure TKDT20DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT20DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT20DI16.PrintNodeTree(const NodePtr: PKDT20DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT20DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT20DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT20DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT20DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT20DI16.KDT20DI16Vec(const s: SystemString): TKDT20DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT20DI16_Axis - 1 do
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
            if j >= KDT20DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT20DI16.KDT20DI16Vec(const v: TKDT20DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT20DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT20DI16.KDT20DI16Pow(const v: TKDT20DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT20DI16.KDT20DI16Distance(const v1, v2: TKDT20DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT20DI16_Axis - 1 do
      Result := Result + KDT20DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT20DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT20DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT20DI16.Test;
var
  TKDT20DI16_Test: TKDT20DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT20DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT20DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT20DI16_Test := TKDT20DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT20DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT20DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT20DI16_Axis - 1 do
        TKDT20DI16_Test.TestBuff[i][j] := i * KDT20DI16_Axis + j;

{$IFDEF FPC}
  TKDT20DI16_Test.BuildKDTreeM(length(TKDT20DI16_Test.TestBuff), nil, @TKDT20DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT20DI16_Test.BuildKDTreeM(length(TKDT20DI16_Test.TestBuff), nil, TKDT20DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT20DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT20DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT20DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT20DI16_Test.Search(TKDT20DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT20DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT20DI16_Test.TestBuff));
      TKDT20DI16_Test.Search(TKDT20DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT20DI16Distance(TKDT20DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT20DI16_Test.Clear;
      { kMean test }
      TKDT20DI16_Test.BuildKDTreeWithCluster(TKDT20DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT20DI16_Test.Search(TKDT20DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT20DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT20DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT20DI16_Test);
end;


function TKDT21DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT21DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT21DI16_Node;
  function SortCompare(const p1, p2: PKDT21DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT21DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT21DI16_Source;
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
  kdBuffPtr: PKDT21DI16_SourceBuffer;
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
      axis := Depth mod KDT21DI16_Axis;
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

function TKDT21DI16.GetData(const Index: NativeInt): PKDT21DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT21DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT21DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT21DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT21DI16_Node(KDNodes[i]));
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

function TKDT21DI16.StoreBuffPtr: PKDT21DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT21DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT21DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT21DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT21DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT21DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT21DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT21DI16.BuildKDTreeWithCluster(const inBuff: TKDT21DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT21DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT21DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT21DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT21DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT21DI16.BuildKDTreeWithCluster(const inBuff: TKDT21DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT21DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DI16_BuildCall);
var
  TempStoreBuff: TKDT21DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT21DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT21DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT21DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT21DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT21DI16_Axis - 1 do
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

procedure TKDT21DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DI16_BuildMethod);
var
  TempStoreBuff: TKDT21DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT21DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT21DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT21DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT21DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT21DI16_Axis - 1 do
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


procedure TKDT21DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DI16_BuildProc);
var
  TempStoreBuff: TKDT21DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT21DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT21DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT21DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT21DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT21DI16_Axis - 1 do
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


function TKDT21DI16.Search(const buff: TKDT21DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT21DI16_Node;

var
  NearestNeighbour: PKDT21DI16_Node;

  function FindParentNode(const buffPtr: PKDT21DI16_Vec; NodePtr: PKDT21DI16_Node): PKDT21DI16_Node;
  var
    Next: PKDT21DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT21DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT21DI16_Node; const buffPtr: PKDT21DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT21DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT21DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT21DI16_Vec; const p1, p2: PKDT21DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT21DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT21DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT21DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT21DI16_Node;
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
  Parent: PKDT21DI16_Node;
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

  SearchedDistanceMin := KDT21DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT21DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT21DI16.Search(const buff: TKDT21DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT21DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT21DI16.Search(const buff: TKDT21DI16_Vec; var SearchedDistanceMin: Double): PKDT21DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT21DI16.Search(const buff: TKDT21DI16_Vec): PKDT21DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT21DI16.SearchToken(const buff: TKDT21DI16_Vec): TPascalString;
var
  p: PKDT21DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT21DI16.Search(const inBuff: TKDT21DI16_DynamicVecBuffer; var OutBuff: TKDT21DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT21DI16_DynamicVecBuffer;
  outBuffPtr: PKDT21DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT21DI16_Node;
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
        p: PKDT21DI16_Node;
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
  p: PKDT21DI16_Node;
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


procedure TKDT21DI16.Search(const inBuff: TKDT21DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT21DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT21DI16_Node;
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
        p: PKDT21DI16_Node;
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
  p: PKDT21DI16_Node;
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


procedure TKDT21DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT21DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT21DI16_Vec));
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

procedure TKDT21DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT21DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT21DI16_Vec)) <> SizeOf(TKDT21DI16_Vec) then
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

procedure TKDT21DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT21DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT21DI16.PrintNodeTree(const NodePtr: PKDT21DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT21DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT21DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT21DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT21DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT21DI16.KDT21DI16Vec(const s: SystemString): TKDT21DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT21DI16_Axis - 1 do
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
            if j >= KDT21DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT21DI16.KDT21DI16Vec(const v: TKDT21DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT21DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT21DI16.KDT21DI16Pow(const v: TKDT21DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT21DI16.KDT21DI16Distance(const v1, v2: TKDT21DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT21DI16_Axis - 1 do
      Result := Result + KDT21DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT21DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT21DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT21DI16.Test;
var
  TKDT21DI16_Test: TKDT21DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT21DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT21DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT21DI16_Test := TKDT21DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT21DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT21DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT21DI16_Axis - 1 do
        TKDT21DI16_Test.TestBuff[i][j] := i * KDT21DI16_Axis + j;

{$IFDEF FPC}
  TKDT21DI16_Test.BuildKDTreeM(length(TKDT21DI16_Test.TestBuff), nil, @TKDT21DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT21DI16_Test.BuildKDTreeM(length(TKDT21DI16_Test.TestBuff), nil, TKDT21DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT21DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT21DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT21DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT21DI16_Test.Search(TKDT21DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT21DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT21DI16_Test.TestBuff));
      TKDT21DI16_Test.Search(TKDT21DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT21DI16Distance(TKDT21DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT21DI16_Test.Clear;
      { kMean test }
      TKDT21DI16_Test.BuildKDTreeWithCluster(TKDT21DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT21DI16_Test.Search(TKDT21DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT21DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT21DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT21DI16_Test);
end;


function TKDT22DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT22DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT22DI16_Node;
  function SortCompare(const p1, p2: PKDT22DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT22DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT22DI16_Source;
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
  kdBuffPtr: PKDT22DI16_SourceBuffer;
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
      axis := Depth mod KDT22DI16_Axis;
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

function TKDT22DI16.GetData(const Index: NativeInt): PKDT22DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT22DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT22DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT22DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT22DI16_Node(KDNodes[i]));
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

function TKDT22DI16.StoreBuffPtr: PKDT22DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT22DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT22DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT22DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT22DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT22DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT22DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT22DI16.BuildKDTreeWithCluster(const inBuff: TKDT22DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT22DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT22DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT22DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT22DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT22DI16.BuildKDTreeWithCluster(const inBuff: TKDT22DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT22DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DI16_BuildCall);
var
  TempStoreBuff: TKDT22DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT22DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT22DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT22DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT22DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT22DI16_Axis - 1 do
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

procedure TKDT22DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DI16_BuildMethod);
var
  TempStoreBuff: TKDT22DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT22DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT22DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT22DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT22DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT22DI16_Axis - 1 do
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


procedure TKDT22DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DI16_BuildProc);
var
  TempStoreBuff: TKDT22DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT22DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT22DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT22DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT22DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT22DI16_Axis - 1 do
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


function TKDT22DI16.Search(const buff: TKDT22DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT22DI16_Node;

var
  NearestNeighbour: PKDT22DI16_Node;

  function FindParentNode(const buffPtr: PKDT22DI16_Vec; NodePtr: PKDT22DI16_Node): PKDT22DI16_Node;
  var
    Next: PKDT22DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT22DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT22DI16_Node; const buffPtr: PKDT22DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT22DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT22DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT22DI16_Vec; const p1, p2: PKDT22DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT22DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT22DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT22DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT22DI16_Node;
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
  Parent: PKDT22DI16_Node;
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

  SearchedDistanceMin := KDT22DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT22DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT22DI16.Search(const buff: TKDT22DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT22DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT22DI16.Search(const buff: TKDT22DI16_Vec; var SearchedDistanceMin: Double): PKDT22DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT22DI16.Search(const buff: TKDT22DI16_Vec): PKDT22DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT22DI16.SearchToken(const buff: TKDT22DI16_Vec): TPascalString;
var
  p: PKDT22DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT22DI16.Search(const inBuff: TKDT22DI16_DynamicVecBuffer; var OutBuff: TKDT22DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT22DI16_DynamicVecBuffer;
  outBuffPtr: PKDT22DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT22DI16_Node;
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
        p: PKDT22DI16_Node;
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
  p: PKDT22DI16_Node;
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


procedure TKDT22DI16.Search(const inBuff: TKDT22DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT22DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT22DI16_Node;
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
        p: PKDT22DI16_Node;
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
  p: PKDT22DI16_Node;
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


procedure TKDT22DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT22DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT22DI16_Vec));
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

procedure TKDT22DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT22DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT22DI16_Vec)) <> SizeOf(TKDT22DI16_Vec) then
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

procedure TKDT22DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT22DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT22DI16.PrintNodeTree(const NodePtr: PKDT22DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT22DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT22DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT22DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT22DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT22DI16.KDT22DI16Vec(const s: SystemString): TKDT22DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT22DI16_Axis - 1 do
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
            if j >= KDT22DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT22DI16.KDT22DI16Vec(const v: TKDT22DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT22DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT22DI16.KDT22DI16Pow(const v: TKDT22DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT22DI16.KDT22DI16Distance(const v1, v2: TKDT22DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT22DI16_Axis - 1 do
      Result := Result + KDT22DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT22DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT22DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT22DI16.Test;
var
  TKDT22DI16_Test: TKDT22DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT22DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT22DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT22DI16_Test := TKDT22DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT22DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT22DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT22DI16_Axis - 1 do
        TKDT22DI16_Test.TestBuff[i][j] := i * KDT22DI16_Axis + j;

{$IFDEF FPC}
  TKDT22DI16_Test.BuildKDTreeM(length(TKDT22DI16_Test.TestBuff), nil, @TKDT22DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT22DI16_Test.BuildKDTreeM(length(TKDT22DI16_Test.TestBuff), nil, TKDT22DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT22DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT22DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT22DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT22DI16_Test.Search(TKDT22DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT22DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT22DI16_Test.TestBuff));
      TKDT22DI16_Test.Search(TKDT22DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT22DI16Distance(TKDT22DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT22DI16_Test.Clear;
      { kMean test }
      TKDT22DI16_Test.BuildKDTreeWithCluster(TKDT22DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT22DI16_Test.Search(TKDT22DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT22DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT22DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT22DI16_Test);
end;


function TKDT23DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT23DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT23DI16_Node;
  function SortCompare(const p1, p2: PKDT23DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT23DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT23DI16_Source;
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
  kdBuffPtr: PKDT23DI16_SourceBuffer;
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
      axis := Depth mod KDT23DI16_Axis;
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

function TKDT23DI16.GetData(const Index: NativeInt): PKDT23DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT23DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT23DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT23DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT23DI16_Node(KDNodes[i]));
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

function TKDT23DI16.StoreBuffPtr: PKDT23DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT23DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT23DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT23DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT23DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT23DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT23DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT23DI16.BuildKDTreeWithCluster(const inBuff: TKDT23DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT23DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT23DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT23DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT23DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT23DI16.BuildKDTreeWithCluster(const inBuff: TKDT23DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT23DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DI16_BuildCall);
var
  TempStoreBuff: TKDT23DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT23DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT23DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT23DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT23DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT23DI16_Axis - 1 do
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

procedure TKDT23DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DI16_BuildMethod);
var
  TempStoreBuff: TKDT23DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT23DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT23DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT23DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT23DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT23DI16_Axis - 1 do
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


procedure TKDT23DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DI16_BuildProc);
var
  TempStoreBuff: TKDT23DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT23DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT23DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT23DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT23DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT23DI16_Axis - 1 do
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


function TKDT23DI16.Search(const buff: TKDT23DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT23DI16_Node;

var
  NearestNeighbour: PKDT23DI16_Node;

  function FindParentNode(const buffPtr: PKDT23DI16_Vec; NodePtr: PKDT23DI16_Node): PKDT23DI16_Node;
  var
    Next: PKDT23DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT23DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT23DI16_Node; const buffPtr: PKDT23DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT23DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT23DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT23DI16_Vec; const p1, p2: PKDT23DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT23DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT23DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT23DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT23DI16_Node;
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
  Parent: PKDT23DI16_Node;
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

  SearchedDistanceMin := KDT23DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT23DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT23DI16.Search(const buff: TKDT23DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT23DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT23DI16.Search(const buff: TKDT23DI16_Vec; var SearchedDistanceMin: Double): PKDT23DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT23DI16.Search(const buff: TKDT23DI16_Vec): PKDT23DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT23DI16.SearchToken(const buff: TKDT23DI16_Vec): TPascalString;
var
  p: PKDT23DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT23DI16.Search(const inBuff: TKDT23DI16_DynamicVecBuffer; var OutBuff: TKDT23DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT23DI16_DynamicVecBuffer;
  outBuffPtr: PKDT23DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT23DI16_Node;
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
        p: PKDT23DI16_Node;
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
  p: PKDT23DI16_Node;
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


procedure TKDT23DI16.Search(const inBuff: TKDT23DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT23DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT23DI16_Node;
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
        p: PKDT23DI16_Node;
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
  p: PKDT23DI16_Node;
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


procedure TKDT23DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT23DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT23DI16_Vec));
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

procedure TKDT23DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT23DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT23DI16_Vec)) <> SizeOf(TKDT23DI16_Vec) then
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

procedure TKDT23DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT23DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT23DI16.PrintNodeTree(const NodePtr: PKDT23DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT23DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT23DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT23DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT23DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT23DI16.KDT23DI16Vec(const s: SystemString): TKDT23DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT23DI16_Axis - 1 do
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
            if j >= KDT23DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT23DI16.KDT23DI16Vec(const v: TKDT23DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT23DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT23DI16.KDT23DI16Pow(const v: TKDT23DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT23DI16.KDT23DI16Distance(const v1, v2: TKDT23DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT23DI16_Axis - 1 do
      Result := Result + KDT23DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT23DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT23DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT23DI16.Test;
var
  TKDT23DI16_Test: TKDT23DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT23DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT23DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT23DI16_Test := TKDT23DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT23DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT23DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT23DI16_Axis - 1 do
        TKDT23DI16_Test.TestBuff[i][j] := i * KDT23DI16_Axis + j;

{$IFDEF FPC}
  TKDT23DI16_Test.BuildKDTreeM(length(TKDT23DI16_Test.TestBuff), nil, @TKDT23DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT23DI16_Test.BuildKDTreeM(length(TKDT23DI16_Test.TestBuff), nil, TKDT23DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT23DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT23DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT23DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT23DI16_Test.Search(TKDT23DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT23DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT23DI16_Test.TestBuff));
      TKDT23DI16_Test.Search(TKDT23DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT23DI16Distance(TKDT23DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT23DI16_Test.Clear;
      { kMean test }
      TKDT23DI16_Test.BuildKDTreeWithCluster(TKDT23DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT23DI16_Test.Search(TKDT23DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT23DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT23DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT23DI16_Test);
end;


function TKDT24DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT24DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT24DI16_Node;
  function SortCompare(const p1, p2: PKDT24DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT24DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT24DI16_Source;
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
  kdBuffPtr: PKDT24DI16_SourceBuffer;
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
      axis := Depth mod KDT24DI16_Axis;
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

function TKDT24DI16.GetData(const Index: NativeInt): PKDT24DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT24DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT24DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT24DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT24DI16_Node(KDNodes[i]));
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

function TKDT24DI16.StoreBuffPtr: PKDT24DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT24DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT24DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT24DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT24DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT24DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT24DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT24DI16.BuildKDTreeWithCluster(const inBuff: TKDT24DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT24DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT24DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT24DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT24DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT24DI16.BuildKDTreeWithCluster(const inBuff: TKDT24DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT24DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DI16_BuildCall);
var
  TempStoreBuff: TKDT24DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT24DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT24DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT24DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT24DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT24DI16_Axis - 1 do
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

procedure TKDT24DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DI16_BuildMethod);
var
  TempStoreBuff: TKDT24DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT24DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT24DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT24DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT24DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT24DI16_Axis - 1 do
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


procedure TKDT24DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DI16_BuildProc);
var
  TempStoreBuff: TKDT24DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT24DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT24DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT24DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT24DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT24DI16_Axis - 1 do
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


function TKDT24DI16.Search(const buff: TKDT24DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT24DI16_Node;

var
  NearestNeighbour: PKDT24DI16_Node;

  function FindParentNode(const buffPtr: PKDT24DI16_Vec; NodePtr: PKDT24DI16_Node): PKDT24DI16_Node;
  var
    Next: PKDT24DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT24DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT24DI16_Node; const buffPtr: PKDT24DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT24DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT24DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT24DI16_Vec; const p1, p2: PKDT24DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT24DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT24DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT24DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT24DI16_Node;
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
  Parent: PKDT24DI16_Node;
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

  SearchedDistanceMin := KDT24DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT24DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT24DI16.Search(const buff: TKDT24DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT24DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT24DI16.Search(const buff: TKDT24DI16_Vec; var SearchedDistanceMin: Double): PKDT24DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT24DI16.Search(const buff: TKDT24DI16_Vec): PKDT24DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT24DI16.SearchToken(const buff: TKDT24DI16_Vec): TPascalString;
var
  p: PKDT24DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT24DI16.Search(const inBuff: TKDT24DI16_DynamicVecBuffer; var OutBuff: TKDT24DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT24DI16_DynamicVecBuffer;
  outBuffPtr: PKDT24DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT24DI16_Node;
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
        p: PKDT24DI16_Node;
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
  p: PKDT24DI16_Node;
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


procedure TKDT24DI16.Search(const inBuff: TKDT24DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT24DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT24DI16_Node;
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
        p: PKDT24DI16_Node;
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
  p: PKDT24DI16_Node;
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


procedure TKDT24DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT24DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT24DI16_Vec));
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

procedure TKDT24DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT24DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT24DI16_Vec)) <> SizeOf(TKDT24DI16_Vec) then
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

procedure TKDT24DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT24DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT24DI16.PrintNodeTree(const NodePtr: PKDT24DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT24DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT24DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT24DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT24DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT24DI16.KDT24DI16Vec(const s: SystemString): TKDT24DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT24DI16_Axis - 1 do
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
            if j >= KDT24DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT24DI16.KDT24DI16Vec(const v: TKDT24DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT24DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT24DI16.KDT24DI16Pow(const v: TKDT24DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT24DI16.KDT24DI16Distance(const v1, v2: TKDT24DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT24DI16_Axis - 1 do
      Result := Result + KDT24DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT24DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT24DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT24DI16.Test;
var
  TKDT24DI16_Test: TKDT24DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT24DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT24DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT24DI16_Test := TKDT24DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT24DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT24DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT24DI16_Axis - 1 do
        TKDT24DI16_Test.TestBuff[i][j] := i * KDT24DI16_Axis + j;

{$IFDEF FPC}
  TKDT24DI16_Test.BuildKDTreeM(length(TKDT24DI16_Test.TestBuff), nil, @TKDT24DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT24DI16_Test.BuildKDTreeM(length(TKDT24DI16_Test.TestBuff), nil, TKDT24DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT24DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT24DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT24DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT24DI16_Test.Search(TKDT24DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT24DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT24DI16_Test.TestBuff));
      TKDT24DI16_Test.Search(TKDT24DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT24DI16Distance(TKDT24DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT24DI16_Test.Clear;
      { kMean test }
      TKDT24DI16_Test.BuildKDTreeWithCluster(TKDT24DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT24DI16_Test.Search(TKDT24DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT24DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT24DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT24DI16_Test);
end;


function TKDT256DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT256DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT256DI16_Node;
  function SortCompare(const p1, p2: PKDT256DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT256DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT256DI16_Source;
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
  kdBuffPtr: PKDT256DI16_SourceBuffer;
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
      axis := Depth mod KDT256DI16_Axis;
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

function TKDT256DI16.GetData(const Index: NativeInt): PKDT256DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT256DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT256DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT256DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT256DI16_Node(KDNodes[i]));
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

function TKDT256DI16.StoreBuffPtr: PKDT256DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT256DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT256DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT256DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT256DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT256DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT256DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT256DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT256DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT256DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT256DI16.BuildKDTreeWithCluster(const inBuff: TKDT256DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT256DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT256DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT256DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT256DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT256DI16.BuildKDTreeWithCluster(const inBuff: TKDT256DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT256DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT256DI16_BuildCall);
var
  TempStoreBuff: TKDT256DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT256DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT256DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT256DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT256DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT256DI16_Axis - 1 do
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

procedure TKDT256DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT256DI16_BuildMethod);
var
  TempStoreBuff: TKDT256DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT256DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT256DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT256DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT256DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT256DI16_Axis - 1 do
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


procedure TKDT256DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT256DI16_BuildProc);
var
  TempStoreBuff: TKDT256DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT256DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT256DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT256DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT256DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT256DI16_Axis - 1 do
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


function TKDT256DI16.Search(const buff: TKDT256DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT256DI16_Node;

var
  NearestNeighbour: PKDT256DI16_Node;

  function FindParentNode(const buffPtr: PKDT256DI16_Vec; NodePtr: PKDT256DI16_Node): PKDT256DI16_Node;
  var
    Next: PKDT256DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT256DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT256DI16_Node; const buffPtr: PKDT256DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT256DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT256DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT256DI16_Vec; const p1, p2: PKDT256DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT256DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT256DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT256DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT256DI16_Node;
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
  Parent: PKDT256DI16_Node;
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

  SearchedDistanceMin := KDT256DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT256DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT256DI16.Search(const buff: TKDT256DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT256DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT256DI16.Search(const buff: TKDT256DI16_Vec; var SearchedDistanceMin: Double): PKDT256DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT256DI16.Search(const buff: TKDT256DI16_Vec): PKDT256DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT256DI16.SearchToken(const buff: TKDT256DI16_Vec): TPascalString;
var
  p: PKDT256DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT256DI16.Search(const inBuff: TKDT256DI16_DynamicVecBuffer; var OutBuff: TKDT256DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT256DI16_DynamicVecBuffer;
  outBuffPtr: PKDT256DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT256DI16_Node;
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
        p: PKDT256DI16_Node;
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
  p: PKDT256DI16_Node;
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


procedure TKDT256DI16.Search(const inBuff: TKDT256DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT256DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT256DI16_Node;
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
        p: PKDT256DI16_Node;
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
  p: PKDT256DI16_Node;
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


procedure TKDT256DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT256DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT256DI16_Vec));
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

procedure TKDT256DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT256DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT256DI16_Vec)) <> SizeOf(TKDT256DI16_Vec) then
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

procedure TKDT256DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT256DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT256DI16.PrintNodeTree(const NodePtr: PKDT256DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT256DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT256DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT256DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT256DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT256DI16.KDT256DI16Vec(const s: SystemString): TKDT256DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT256DI16_Axis - 1 do
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
            if j >= KDT256DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT256DI16.KDT256DI16Vec(const v: TKDT256DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT256DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT256DI16.KDT256DI16Pow(const v: TKDT256DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT256DI16.KDT256DI16Distance(const v1, v2: TKDT256DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT256DI16_Axis - 1 do
      Result := Result + KDT256DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT256DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT256DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT256DI16.Test;
var
  TKDT256DI16_Test: TKDT256DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT256DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT256DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT256DI16_Test := TKDT256DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT256DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT256DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT256DI16_Axis - 1 do
        TKDT256DI16_Test.TestBuff[i][j] := i * KDT256DI16_Axis + j;

{$IFDEF FPC}
  TKDT256DI16_Test.BuildKDTreeM(length(TKDT256DI16_Test.TestBuff), nil, @TKDT256DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT256DI16_Test.BuildKDTreeM(length(TKDT256DI16_Test.TestBuff), nil, TKDT256DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT256DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT256DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT256DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT256DI16_Test.Search(TKDT256DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT256DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT256DI16_Test.TestBuff));
      TKDT256DI16_Test.Search(TKDT256DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT256DI16Distance(TKDT256DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT256DI16_Test.Clear;
      { kMean test }
      TKDT256DI16_Test.BuildKDTreeWithCluster(TKDT256DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT256DI16_Test.Search(TKDT256DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT256DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT256DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT256DI16_Test);
end;


function TKDT512DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT512DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT512DI16_Node;
  function SortCompare(const p1, p2: PKDT512DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT512DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT512DI16_Source;
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
  kdBuffPtr: PKDT512DI16_SourceBuffer;
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
      axis := Depth mod KDT512DI16_Axis;
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

function TKDT512DI16.GetData(const Index: NativeInt): PKDT512DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT512DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT512DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT512DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT512DI16_Node(KDNodes[i]));
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

function TKDT512DI16.StoreBuffPtr: PKDT512DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT512DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT512DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT512DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT512DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT512DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT512DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT512DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT512DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT512DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT512DI16.BuildKDTreeWithCluster(const inBuff: TKDT512DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT512DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT512DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT512DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT512DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT512DI16.BuildKDTreeWithCluster(const inBuff: TKDT512DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT512DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT512DI16_BuildCall);
var
  TempStoreBuff: TKDT512DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT512DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT512DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT512DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT512DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT512DI16_Axis - 1 do
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

procedure TKDT512DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT512DI16_BuildMethod);
var
  TempStoreBuff: TKDT512DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT512DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT512DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT512DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT512DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT512DI16_Axis - 1 do
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


procedure TKDT512DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT512DI16_BuildProc);
var
  TempStoreBuff: TKDT512DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT512DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT512DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT512DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT512DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT512DI16_Axis - 1 do
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


function TKDT512DI16.Search(const buff: TKDT512DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT512DI16_Node;

var
  NearestNeighbour: PKDT512DI16_Node;

  function FindParentNode(const buffPtr: PKDT512DI16_Vec; NodePtr: PKDT512DI16_Node): PKDT512DI16_Node;
  var
    Next: PKDT512DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT512DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT512DI16_Node; const buffPtr: PKDT512DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT512DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT512DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT512DI16_Vec; const p1, p2: PKDT512DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT512DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT512DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT512DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT512DI16_Node;
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
  Parent: PKDT512DI16_Node;
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

  SearchedDistanceMin := KDT512DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT512DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT512DI16.Search(const buff: TKDT512DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT512DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT512DI16.Search(const buff: TKDT512DI16_Vec; var SearchedDistanceMin: Double): PKDT512DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT512DI16.Search(const buff: TKDT512DI16_Vec): PKDT512DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT512DI16.SearchToken(const buff: TKDT512DI16_Vec): TPascalString;
var
  p: PKDT512DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT512DI16.Search(const inBuff: TKDT512DI16_DynamicVecBuffer; var OutBuff: TKDT512DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT512DI16_DynamicVecBuffer;
  outBuffPtr: PKDT512DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT512DI16_Node;
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
        p: PKDT512DI16_Node;
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
  p: PKDT512DI16_Node;
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


procedure TKDT512DI16.Search(const inBuff: TKDT512DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT512DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT512DI16_Node;
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
        p: PKDT512DI16_Node;
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
  p: PKDT512DI16_Node;
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


procedure TKDT512DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT512DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT512DI16_Vec));
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

procedure TKDT512DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT512DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT512DI16_Vec)) <> SizeOf(TKDT512DI16_Vec) then
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

procedure TKDT512DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT512DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT512DI16.PrintNodeTree(const NodePtr: PKDT512DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT512DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT512DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT512DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT512DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT512DI16.KDT512DI16Vec(const s: SystemString): TKDT512DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT512DI16_Axis - 1 do
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
            if j >= KDT512DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT512DI16.KDT512DI16Vec(const v: TKDT512DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT512DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT512DI16.KDT512DI16Pow(const v: TKDT512DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT512DI16.KDT512DI16Distance(const v1, v2: TKDT512DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT512DI16_Axis - 1 do
      Result := Result + KDT512DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT512DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT512DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT512DI16.Test;
var
  TKDT512DI16_Test: TKDT512DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT512DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT512DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT512DI16_Test := TKDT512DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT512DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT512DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT512DI16_Axis - 1 do
        TKDT512DI16_Test.TestBuff[i][j] := i * KDT512DI16_Axis + j;

{$IFDEF FPC}
  TKDT512DI16_Test.BuildKDTreeM(length(TKDT512DI16_Test.TestBuff), nil, @TKDT512DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT512DI16_Test.BuildKDTreeM(length(TKDT512DI16_Test.TestBuff), nil, TKDT512DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT512DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT512DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT512DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT512DI16_Test.Search(TKDT512DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT512DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT512DI16_Test.TestBuff));
      TKDT512DI16_Test.Search(TKDT512DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT512DI16Distance(TKDT512DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT512DI16_Test.Clear;
      { kMean test }
      TKDT512DI16_Test.BuildKDTreeWithCluster(TKDT512DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT512DI16_Test.Search(TKDT512DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT512DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT512DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT512DI16_Test);
end;


function TKDT1024DI16.InternalBuildKdTree(const KDSourceBufferPtr: PKDT1024DI16_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT1024DI16_Node;
  function SortCompare(const p1, p2: PKDT1024DI16_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT1024DI16_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT1024DI16_Source;
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
  kdBuffPtr: PKDT1024DI16_SourceBuffer;
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
      axis := Depth mod KDT1024DI16_Axis;
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

function TKDT1024DI16.GetData(const Index: NativeInt): PKDT1024DI16_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT1024DI16.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT1024DI16.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT1024DI16.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT1024DI16_Node(KDNodes[i]));
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

function TKDT1024DI16.StoreBuffPtr: PKDT1024DI16_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT1024DI16.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1024DI16_BuildCall);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT1024DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT1024DI16.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1024DI16_BuildMethod);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT1024DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT1024DI16.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1024DI16_BuildProc);
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
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT1024DI16_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT1024DI16.BuildKDTreeWithCluster(const inBuff: TKDT1024DI16_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT1024DI16_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT1024DI16_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT1024DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1024DI16_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT1024DI16.BuildKDTreeWithCluster(const inBuff: TKDT1024DI16_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT1024DI16.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1024DI16_BuildCall);
var
  TempStoreBuff: TKDT1024DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT1024DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT1024DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT1024DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT1024DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1024DI16_Axis - 1 do
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

procedure TKDT1024DI16.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1024DI16_BuildMethod);
var
  TempStoreBuff: TKDT1024DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT1024DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT1024DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT1024DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT1024DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1024DI16_Axis - 1 do
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


procedure TKDT1024DI16.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1024DI16_BuildProc);
var
  TempStoreBuff: TKDT1024DI16_DyanmicStoreBuffer;
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
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT1024DI16_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT1024DI16_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT1024DI16_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT1024DI16_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1024DI16_Axis - 1 do
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


function TKDT1024DI16.Search(const buff: TKDT1024DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT1024DI16_Node;

var
  NearestNeighbour: PKDT1024DI16_Node;

  function FindParentNode(const buffPtr: PKDT1024DI16_Vec; NodePtr: PKDT1024DI16_Node): PKDT1024DI16_Node;
  var
    Next: PKDT1024DI16_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT1024DI16_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT1024DI16_Node; const buffPtr: PKDT1024DI16_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT1024DI16Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT1024DI16_Axis;
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

  function SortCompare(const buffPtr: PKDT1024DI16_Vec; const p1, p2: PKDT1024DI16_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT1024DI16Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT1024DI16Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT1024DI16_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT1024DI16_Node;
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
  Parent: PKDT1024DI16_Node;
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

  SearchedDistanceMin := KDT1024DI16Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT1024DI16_Node(NearestNodes[0]);
    end;
end;

function TKDT1024DI16.Search(const buff: TKDT1024DI16_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT1024DI16_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT1024DI16.Search(const buff: TKDT1024DI16_Vec; var SearchedDistanceMin: Double): PKDT1024DI16_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT1024DI16.Search(const buff: TKDT1024DI16_Vec): PKDT1024DI16_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT1024DI16.SearchToken(const buff: TKDT1024DI16_Vec): TPascalString;
var
  p: PKDT1024DI16_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT1024DI16.Search(const inBuff: TKDT1024DI16_DynamicVecBuffer; var OutBuff: TKDT1024DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT1024DI16_DynamicVecBuffer;
  outBuffPtr: PKDT1024DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT1024DI16_Node;
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
        p: PKDT1024DI16_Node;
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
  p: PKDT1024DI16_Node;
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


procedure TKDT1024DI16.Search(const inBuff: TKDT1024DI16_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT1024DI16_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT1024DI16_Node;
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
        p: PKDT1024DI16_Node;
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
  p: PKDT1024DI16_Node;
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


procedure TKDT1024DI16.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT1024DI16_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT1024DI16_Vec));
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

procedure TKDT1024DI16.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT1024DI16_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT1024DI16_Vec)) <> SizeOf(TKDT1024DI16_Vec) then
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

procedure TKDT1024DI16.SaveToFile(FileName: SystemString);
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

procedure TKDT1024DI16.LoadFromFile(FileName: SystemString);
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

procedure TKDT1024DI16.PrintNodeTree(const NodePtr: PKDT1024DI16_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT1024DI16_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT1024DI16Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT1024DI16.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT1024DI16Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT1024DI16.KDT1024DI16Vec(const s: SystemString): TKDT1024DI16_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT1024DI16_Axis - 1 do
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
            if j >= KDT1024DI16_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT1024DI16.KDT1024DI16Vec(const v: TKDT1024DI16_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT1024DI16_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT1024DI16.KDT1024DI16Pow(const v: TKDT1024DI16_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT1024DI16.KDT1024DI16Distance(const v1, v2: TKDT1024DI16_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT1024DI16_Axis - 1 do
      Result := Result + KDT1024DI16Pow(v2[i] - v1[i]);
end;

procedure TKDT1024DI16.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT1024DI16_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT1024DI16.Test;
var
  TKDT1024DI16_Test: TKDT1024DI16;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT1024DI16_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT1024DI16_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT1024DI16_Test := TKDT1024DI16.Create;

  DoStatusNoLn('...');
  SetLength(TKDT1024DI16_Test.TestBuff, 1000);
  for i := 0 to length(TKDT1024DI16_Test.TestBuff) - 1 do
    for j := 0 to KDT1024DI16_Axis - 1 do
        TKDT1024DI16_Test.TestBuff[i][j] := i * KDT1024DI16_Axis + j;

{$IFDEF FPC}
  TKDT1024DI16_Test.BuildKDTreeM(length(TKDT1024DI16_Test.TestBuff), nil, @TKDT1024DI16_Test.Test_BuildM);
{$ELSE FPC}
  TKDT1024DI16_Test.BuildKDTreeM(length(TKDT1024DI16_Test.TestBuff), nil, TKDT1024DI16_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT1024DI16_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT1024DI16_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT1024DI16_Test.TestBuff) - 1 do
    begin
      p := TKDT1024DI16_Test.Search(TKDT1024DI16_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT1024DI16_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT1024DI16_Test.TestBuff));
      TKDT1024DI16_Test.Search(TKDT1024DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT1024DI16Distance(TKDT1024DI16_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT1024DI16_Test.Clear;
      { kMean test }
      TKDT1024DI16_Test.BuildKDTreeWithCluster(TKDT1024DI16_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT1024DI16_Test.Search(TKDT1024DI16_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT1024DI16_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT1024DI16_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT1024DI16_Test);
end;


procedure Test_All;
begin
  TKDT1DI16.Test();
  TKDT2DI16.Test();
  TKDT3DI16.Test();
  TKDT4DI16.Test();
  TKDT5DI16.Test();
  TKDT6DI16.Test();
  TKDT7DI16.Test();
  TKDT8DI16.Test();
  TKDT9DI16.Test();
  TKDT10DI16.Test();
  TKDT11DI16.Test();
  TKDT12DI16.Test();
  TKDT13DI16.Test();
  TKDT14DI16.Test();
  TKDT15DI16.Test();
  TKDT16DI16.Test();
  TKDT17DI16.Test();
  TKDT18DI16.Test();
  TKDT19DI16.Test();
  TKDT20DI16.Test();
  TKDT21DI16.Test();
  TKDT22DI16.Test();
  TKDT23DI16.Test();
  TKDT24DI16.Test();
  TKDT256DI16.Test();
  TKDT512DI16.Test();
  TKDT1024DI16.Test();
end;





initialization

finalization

end.

