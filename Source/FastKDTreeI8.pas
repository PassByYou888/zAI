{ ****************************************************************************** }
{ Fast KDTree ShortInt type support                                              }
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

unit FastKDTreeI8;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, PascalStrings, UnicodeMixedLib, KM;

const

  // ShortInt KDTree
  KDT1DI8_Axis = 1;
  KDT2DI8_Axis = 2;
  KDT3DI8_Axis = 3;
  KDT4DI8_Axis = 4;
  KDT5DI8_Axis = 5;
  KDT6DI8_Axis = 6;
  KDT7DI8_Axis = 7;
  KDT8DI8_Axis = 8;
  KDT9DI8_Axis = 9;
  KDT10DI8_Axis = 10;
  KDT11DI8_Axis = 11;
  KDT12DI8_Axis = 12;
  KDT13DI8_Axis = 13;
  KDT14DI8_Axis = 14;
  KDT15DI8_Axis = 15;
  KDT16DI8_Axis = 16;
  KDT17DI8_Axis = 17;
  KDT18DI8_Axis = 18;
  KDT19DI8_Axis = 19;
  KDT20DI8_Axis = 20;
  KDT21DI8_Axis = 21;
  KDT22DI8_Axis = 22;
  KDT23DI8_Axis = 23;
  KDT24DI8_Axis = 24;
  KDT256DI8_Axis = 256;
  KDT512DI8_Axis = 512;
  KDT1024DI8_Axis = 1024;

type

  // ShortInt: KDTree
  TKDT1DI8 = class;  TKDT1DI8_VecType = KM.TKMFloat; // 1D
  TKDT2DI8 = class;  TKDT2DI8_VecType = KM.TKMFloat; // 2D
  TKDT3DI8 = class;  TKDT3DI8_VecType = KM.TKMFloat; // 3D
  TKDT4DI8 = class;  TKDT4DI8_VecType = KM.TKMFloat; // 4D
  TKDT5DI8 = class;  TKDT5DI8_VecType = KM.TKMFloat; // 5D
  TKDT6DI8 = class;  TKDT6DI8_VecType = KM.TKMFloat; // 6D
  TKDT7DI8 = class;  TKDT7DI8_VecType = KM.TKMFloat; // 7D
  TKDT8DI8 = class;  TKDT8DI8_VecType = KM.TKMFloat; // 8D
  TKDT9DI8 = class;  TKDT9DI8_VecType = KM.TKMFloat; // 9D
  TKDT10DI8 = class;  TKDT10DI8_VecType = KM.TKMFloat; // 10D
  TKDT11DI8 = class;  TKDT11DI8_VecType = KM.TKMFloat; // 11D
  TKDT12DI8 = class;  TKDT12DI8_VecType = KM.TKMFloat; // 12D
  TKDT13DI8 = class;  TKDT13DI8_VecType = KM.TKMFloat; // 13D
  TKDT14DI8 = class;  TKDT14DI8_VecType = KM.TKMFloat; // 14D
  TKDT15DI8 = class;  TKDT15DI8_VecType = KM.TKMFloat; // 15D
  TKDT16DI8 = class;  TKDT16DI8_VecType = KM.TKMFloat; // 16D
  TKDT17DI8 = class;  TKDT17DI8_VecType = KM.TKMFloat; // 17D
  TKDT18DI8 = class;  TKDT18DI8_VecType = KM.TKMFloat; // 18D
  TKDT19DI8 = class;  TKDT19DI8_VecType = KM.TKMFloat; // 19D
  TKDT20DI8 = class;  TKDT20DI8_VecType = KM.TKMFloat; // 20D
  TKDT21DI8 = class;  TKDT21DI8_VecType = KM.TKMFloat; // 21D
  TKDT22DI8 = class;  TKDT22DI8_VecType = KM.TKMFloat; // 22D
  TKDT23DI8 = class;  TKDT23DI8_VecType = KM.TKMFloat; // 23D
  TKDT24DI8 = class;  TKDT24DI8_VecType = KM.TKMFloat; // 24D
  TKDT256DI8 = class;  TKDT256DI8_VecType = KM.TKMFloat; // 256D
  TKDT512DI8 = class;  TKDT512DI8_VecType = KM.TKMFloat; // 512D
  TKDT1024DI8 = class;  TKDT1024DI8_VecType = KM.TKMFloat; // 1024D










  // ShortInt KDTree


  TKDT1DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT1DI8_Vec = array [0 .. KDT1DI8_Axis - 1] of TKDT1DI8_VecType;
    PKDT1DI8_Vec = ^TKDT1DI8_Vec;

    TKDT1DI8_DynamicVecBuffer = array of TKDT1DI8_Vec;
    PKDT1DI8_DynamicVecBuffer = ^TKDT1DI8_DynamicVecBuffer;

    TKDT1DI8_Source = record
      buff: TKDT1DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT1DI8_Source = ^TKDT1DI8_Source;
    TKDT1DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT1DI8_Source) - 1] of PKDT1DI8_Source;
    PKDT1DI8_SourceBuffer = ^TKDT1DI8_SourceBuffer;

    TKDT1DI8_DyanmicSourceBuffer = array of PKDT1DI8_Source;
    PKDT1DI8_DyanmicSourceBuffer = ^TKDT1DI8_DyanmicSourceBuffer;

    TKDT1DI8_DyanmicStoreBuffer = array of TKDT1DI8_Source;
    PKDT1DI8_DyanmicStoreBuffer = ^TKDT1DI8_DyanmicStoreBuffer;

    PKDT1DI8_Node = ^TKDT1DI8_Node;

    TKDT1DI8_Node = record
      Parent, Right, Left: PKDT1DI8_Node;
      vec: PKDT1DI8_Source;
    end;

    TKDT1DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT1DI8_Source; const Data: Pointer);
    TKDT1DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT1DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT1DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT1DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT1DI8_DyanmicStoreBuffer;
    KDBuff: TKDT1DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT1DI8_Node;
    TestBuff: TKDT1DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT1DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT1DI8_Node;
    function GetData(const Index: NativeInt): PKDT1DI8_Source;
  public
    RootNode: PKDT1DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT1DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT1DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT1DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT1DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT1DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT1DI8_Node; overload;
    function Search(const buff: TKDT1DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT1DI8_Node; overload;
    function Search(const buff: TKDT1DI8_Vec; var SearchedDistanceMin: Double): PKDT1DI8_Node; overload;
    function Search(const buff: TKDT1DI8_Vec): PKDT1DI8_Node; overload;
    function SearchToken(const buff: TKDT1DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT1DI8_DynamicVecBuffer; var OutBuff: TKDT1DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT1DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT1DI8_Node);
    procedure PrintBuffer;

    class function KDT1DI8Vec(const s: SystemString): TKDT1DI8_Vec; overload;
    class function KDT1DI8Vec(const v: TKDT1DI8_Vec): SystemString; overload;
    class function KDT1DI8Pow(const v: TKDT1DI8_VecType): Double;
    class function KDT1DI8Distance(const v1, v2: TKDT1DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT1DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT2DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT2DI8_Vec = array [0 .. KDT2DI8_Axis - 1] of TKDT2DI8_VecType;
    PKDT2DI8_Vec = ^TKDT2DI8_Vec;

    TKDT2DI8_DynamicVecBuffer = array of TKDT2DI8_Vec;
    PKDT2DI8_DynamicVecBuffer = ^TKDT2DI8_DynamicVecBuffer;

    TKDT2DI8_Source = record
      buff: TKDT2DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT2DI8_Source = ^TKDT2DI8_Source;
    TKDT2DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT2DI8_Source) - 1] of PKDT2DI8_Source;
    PKDT2DI8_SourceBuffer = ^TKDT2DI8_SourceBuffer;

    TKDT2DI8_DyanmicSourceBuffer = array of PKDT2DI8_Source;
    PKDT2DI8_DyanmicSourceBuffer = ^TKDT2DI8_DyanmicSourceBuffer;

    TKDT2DI8_DyanmicStoreBuffer = array of TKDT2DI8_Source;
    PKDT2DI8_DyanmicStoreBuffer = ^TKDT2DI8_DyanmicStoreBuffer;

    PKDT2DI8_Node = ^TKDT2DI8_Node;

    TKDT2DI8_Node = record
      Parent, Right, Left: PKDT2DI8_Node;
      vec: PKDT2DI8_Source;
    end;

    TKDT2DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT2DI8_Source; const Data: Pointer);
    TKDT2DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT2DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT2DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT2DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT2DI8_DyanmicStoreBuffer;
    KDBuff: TKDT2DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT2DI8_Node;
    TestBuff: TKDT2DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT2DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT2DI8_Node;
    function GetData(const Index: NativeInt): PKDT2DI8_Source;
  public
    RootNode: PKDT2DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT2DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT2DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT2DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT2DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT2DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT2DI8_Node; overload;
    function Search(const buff: TKDT2DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT2DI8_Node; overload;
    function Search(const buff: TKDT2DI8_Vec; var SearchedDistanceMin: Double): PKDT2DI8_Node; overload;
    function Search(const buff: TKDT2DI8_Vec): PKDT2DI8_Node; overload;
    function SearchToken(const buff: TKDT2DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT2DI8_DynamicVecBuffer; var OutBuff: TKDT2DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT2DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT2DI8_Node);
    procedure PrintBuffer;

    class function KDT2DI8Vec(const s: SystemString): TKDT2DI8_Vec; overload;
    class function KDT2DI8Vec(const v: TKDT2DI8_Vec): SystemString; overload;
    class function KDT2DI8Pow(const v: TKDT2DI8_VecType): Double;
    class function KDT2DI8Distance(const v1, v2: TKDT2DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT2DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT3DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT3DI8_Vec = array [0 .. KDT3DI8_Axis - 1] of TKDT3DI8_VecType;
    PKDT3DI8_Vec = ^TKDT3DI8_Vec;

    TKDT3DI8_DynamicVecBuffer = array of TKDT3DI8_Vec;
    PKDT3DI8_DynamicVecBuffer = ^TKDT3DI8_DynamicVecBuffer;

    TKDT3DI8_Source = record
      buff: TKDT3DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT3DI8_Source = ^TKDT3DI8_Source;
    TKDT3DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT3DI8_Source) - 1] of PKDT3DI8_Source;
    PKDT3DI8_SourceBuffer = ^TKDT3DI8_SourceBuffer;

    TKDT3DI8_DyanmicSourceBuffer = array of PKDT3DI8_Source;
    PKDT3DI8_DyanmicSourceBuffer = ^TKDT3DI8_DyanmicSourceBuffer;

    TKDT3DI8_DyanmicStoreBuffer = array of TKDT3DI8_Source;
    PKDT3DI8_DyanmicStoreBuffer = ^TKDT3DI8_DyanmicStoreBuffer;

    PKDT3DI8_Node = ^TKDT3DI8_Node;

    TKDT3DI8_Node = record
      Parent, Right, Left: PKDT3DI8_Node;
      vec: PKDT3DI8_Source;
    end;

    TKDT3DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT3DI8_Source; const Data: Pointer);
    TKDT3DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT3DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT3DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT3DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT3DI8_DyanmicStoreBuffer;
    KDBuff: TKDT3DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT3DI8_Node;
    TestBuff: TKDT3DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT3DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT3DI8_Node;
    function GetData(const Index: NativeInt): PKDT3DI8_Source;
  public
    RootNode: PKDT3DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT3DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT3DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT3DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT3DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT3DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT3DI8_Node; overload;
    function Search(const buff: TKDT3DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT3DI8_Node; overload;
    function Search(const buff: TKDT3DI8_Vec; var SearchedDistanceMin: Double): PKDT3DI8_Node; overload;
    function Search(const buff: TKDT3DI8_Vec): PKDT3DI8_Node; overload;
    function SearchToken(const buff: TKDT3DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT3DI8_DynamicVecBuffer; var OutBuff: TKDT3DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT3DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT3DI8_Node);
    procedure PrintBuffer;

    class function KDT3DI8Vec(const s: SystemString): TKDT3DI8_Vec; overload;
    class function KDT3DI8Vec(const v: TKDT3DI8_Vec): SystemString; overload;
    class function KDT3DI8Pow(const v: TKDT3DI8_VecType): Double;
    class function KDT3DI8Distance(const v1, v2: TKDT3DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT3DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT4DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT4DI8_Vec = array [0 .. KDT4DI8_Axis - 1] of TKDT4DI8_VecType;
    PKDT4DI8_Vec = ^TKDT4DI8_Vec;

    TKDT4DI8_DynamicVecBuffer = array of TKDT4DI8_Vec;
    PKDT4DI8_DynamicVecBuffer = ^TKDT4DI8_DynamicVecBuffer;

    TKDT4DI8_Source = record
      buff: TKDT4DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT4DI8_Source = ^TKDT4DI8_Source;
    TKDT4DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT4DI8_Source) - 1] of PKDT4DI8_Source;
    PKDT4DI8_SourceBuffer = ^TKDT4DI8_SourceBuffer;

    TKDT4DI8_DyanmicSourceBuffer = array of PKDT4DI8_Source;
    PKDT4DI8_DyanmicSourceBuffer = ^TKDT4DI8_DyanmicSourceBuffer;

    TKDT4DI8_DyanmicStoreBuffer = array of TKDT4DI8_Source;
    PKDT4DI8_DyanmicStoreBuffer = ^TKDT4DI8_DyanmicStoreBuffer;

    PKDT4DI8_Node = ^TKDT4DI8_Node;

    TKDT4DI8_Node = record
      Parent, Right, Left: PKDT4DI8_Node;
      vec: PKDT4DI8_Source;
    end;

    TKDT4DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT4DI8_Source; const Data: Pointer);
    TKDT4DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT4DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT4DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT4DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT4DI8_DyanmicStoreBuffer;
    KDBuff: TKDT4DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT4DI8_Node;
    TestBuff: TKDT4DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT4DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT4DI8_Node;
    function GetData(const Index: NativeInt): PKDT4DI8_Source;
  public
    RootNode: PKDT4DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT4DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT4DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT4DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT4DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT4DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT4DI8_Node; overload;
    function Search(const buff: TKDT4DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT4DI8_Node; overload;
    function Search(const buff: TKDT4DI8_Vec; var SearchedDistanceMin: Double): PKDT4DI8_Node; overload;
    function Search(const buff: TKDT4DI8_Vec): PKDT4DI8_Node; overload;
    function SearchToken(const buff: TKDT4DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT4DI8_DynamicVecBuffer; var OutBuff: TKDT4DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT4DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT4DI8_Node);
    procedure PrintBuffer;

    class function KDT4DI8Vec(const s: SystemString): TKDT4DI8_Vec; overload;
    class function KDT4DI8Vec(const v: TKDT4DI8_Vec): SystemString; overload;
    class function KDT4DI8Pow(const v: TKDT4DI8_VecType): Double;
    class function KDT4DI8Distance(const v1, v2: TKDT4DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT4DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT5DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT5DI8_Vec = array [0 .. KDT5DI8_Axis - 1] of TKDT5DI8_VecType;
    PKDT5DI8_Vec = ^TKDT5DI8_Vec;

    TKDT5DI8_DynamicVecBuffer = array of TKDT5DI8_Vec;
    PKDT5DI8_DynamicVecBuffer = ^TKDT5DI8_DynamicVecBuffer;

    TKDT5DI8_Source = record
      buff: TKDT5DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT5DI8_Source = ^TKDT5DI8_Source;
    TKDT5DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT5DI8_Source) - 1] of PKDT5DI8_Source;
    PKDT5DI8_SourceBuffer = ^TKDT5DI8_SourceBuffer;

    TKDT5DI8_DyanmicSourceBuffer = array of PKDT5DI8_Source;
    PKDT5DI8_DyanmicSourceBuffer = ^TKDT5DI8_DyanmicSourceBuffer;

    TKDT5DI8_DyanmicStoreBuffer = array of TKDT5DI8_Source;
    PKDT5DI8_DyanmicStoreBuffer = ^TKDT5DI8_DyanmicStoreBuffer;

    PKDT5DI8_Node = ^TKDT5DI8_Node;

    TKDT5DI8_Node = record
      Parent, Right, Left: PKDT5DI8_Node;
      vec: PKDT5DI8_Source;
    end;

    TKDT5DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT5DI8_Source; const Data: Pointer);
    TKDT5DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT5DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT5DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT5DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT5DI8_DyanmicStoreBuffer;
    KDBuff: TKDT5DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT5DI8_Node;
    TestBuff: TKDT5DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT5DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT5DI8_Node;
    function GetData(const Index: NativeInt): PKDT5DI8_Source;
  public
    RootNode: PKDT5DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT5DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT5DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT5DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT5DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT5DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT5DI8_Node; overload;
    function Search(const buff: TKDT5DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT5DI8_Node; overload;
    function Search(const buff: TKDT5DI8_Vec; var SearchedDistanceMin: Double): PKDT5DI8_Node; overload;
    function Search(const buff: TKDT5DI8_Vec): PKDT5DI8_Node; overload;
    function SearchToken(const buff: TKDT5DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT5DI8_DynamicVecBuffer; var OutBuff: TKDT5DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT5DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT5DI8_Node);
    procedure PrintBuffer;

    class function KDT5DI8Vec(const s: SystemString): TKDT5DI8_Vec; overload;
    class function KDT5DI8Vec(const v: TKDT5DI8_Vec): SystemString; overload;
    class function KDT5DI8Pow(const v: TKDT5DI8_VecType): Double;
    class function KDT5DI8Distance(const v1, v2: TKDT5DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT5DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT6DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT6DI8_Vec = array [0 .. KDT6DI8_Axis - 1] of TKDT6DI8_VecType;
    PKDT6DI8_Vec = ^TKDT6DI8_Vec;

    TKDT6DI8_DynamicVecBuffer = array of TKDT6DI8_Vec;
    PKDT6DI8_DynamicVecBuffer = ^TKDT6DI8_DynamicVecBuffer;

    TKDT6DI8_Source = record
      buff: TKDT6DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT6DI8_Source = ^TKDT6DI8_Source;
    TKDT6DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT6DI8_Source) - 1] of PKDT6DI8_Source;
    PKDT6DI8_SourceBuffer = ^TKDT6DI8_SourceBuffer;

    TKDT6DI8_DyanmicSourceBuffer = array of PKDT6DI8_Source;
    PKDT6DI8_DyanmicSourceBuffer = ^TKDT6DI8_DyanmicSourceBuffer;

    TKDT6DI8_DyanmicStoreBuffer = array of TKDT6DI8_Source;
    PKDT6DI8_DyanmicStoreBuffer = ^TKDT6DI8_DyanmicStoreBuffer;

    PKDT6DI8_Node = ^TKDT6DI8_Node;

    TKDT6DI8_Node = record
      Parent, Right, Left: PKDT6DI8_Node;
      vec: PKDT6DI8_Source;
    end;

    TKDT6DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT6DI8_Source; const Data: Pointer);
    TKDT6DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT6DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT6DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT6DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT6DI8_DyanmicStoreBuffer;
    KDBuff: TKDT6DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT6DI8_Node;
    TestBuff: TKDT6DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT6DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT6DI8_Node;
    function GetData(const Index: NativeInt): PKDT6DI8_Source;
  public
    RootNode: PKDT6DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT6DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT6DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT6DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT6DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT6DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT6DI8_Node; overload;
    function Search(const buff: TKDT6DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT6DI8_Node; overload;
    function Search(const buff: TKDT6DI8_Vec; var SearchedDistanceMin: Double): PKDT6DI8_Node; overload;
    function Search(const buff: TKDT6DI8_Vec): PKDT6DI8_Node; overload;
    function SearchToken(const buff: TKDT6DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT6DI8_DynamicVecBuffer; var OutBuff: TKDT6DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT6DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT6DI8_Node);
    procedure PrintBuffer;

    class function KDT6DI8Vec(const s: SystemString): TKDT6DI8_Vec; overload;
    class function KDT6DI8Vec(const v: TKDT6DI8_Vec): SystemString; overload;
    class function KDT6DI8Pow(const v: TKDT6DI8_VecType): Double;
    class function KDT6DI8Distance(const v1, v2: TKDT6DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT6DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT7DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT7DI8_Vec = array [0 .. KDT7DI8_Axis - 1] of TKDT7DI8_VecType;
    PKDT7DI8_Vec = ^TKDT7DI8_Vec;

    TKDT7DI8_DynamicVecBuffer = array of TKDT7DI8_Vec;
    PKDT7DI8_DynamicVecBuffer = ^TKDT7DI8_DynamicVecBuffer;

    TKDT7DI8_Source = record
      buff: TKDT7DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT7DI8_Source = ^TKDT7DI8_Source;
    TKDT7DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT7DI8_Source) - 1] of PKDT7DI8_Source;
    PKDT7DI8_SourceBuffer = ^TKDT7DI8_SourceBuffer;

    TKDT7DI8_DyanmicSourceBuffer = array of PKDT7DI8_Source;
    PKDT7DI8_DyanmicSourceBuffer = ^TKDT7DI8_DyanmicSourceBuffer;

    TKDT7DI8_DyanmicStoreBuffer = array of TKDT7DI8_Source;
    PKDT7DI8_DyanmicStoreBuffer = ^TKDT7DI8_DyanmicStoreBuffer;

    PKDT7DI8_Node = ^TKDT7DI8_Node;

    TKDT7DI8_Node = record
      Parent, Right, Left: PKDT7DI8_Node;
      vec: PKDT7DI8_Source;
    end;

    TKDT7DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT7DI8_Source; const Data: Pointer);
    TKDT7DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT7DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT7DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT7DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT7DI8_DyanmicStoreBuffer;
    KDBuff: TKDT7DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT7DI8_Node;
    TestBuff: TKDT7DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT7DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT7DI8_Node;
    function GetData(const Index: NativeInt): PKDT7DI8_Source;
  public
    RootNode: PKDT7DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT7DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT7DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT7DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT7DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT7DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT7DI8_Node; overload;
    function Search(const buff: TKDT7DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT7DI8_Node; overload;
    function Search(const buff: TKDT7DI8_Vec; var SearchedDistanceMin: Double): PKDT7DI8_Node; overload;
    function Search(const buff: TKDT7DI8_Vec): PKDT7DI8_Node; overload;
    function SearchToken(const buff: TKDT7DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT7DI8_DynamicVecBuffer; var OutBuff: TKDT7DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT7DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT7DI8_Node);
    procedure PrintBuffer;

    class function KDT7DI8Vec(const s: SystemString): TKDT7DI8_Vec; overload;
    class function KDT7DI8Vec(const v: TKDT7DI8_Vec): SystemString; overload;
    class function KDT7DI8Pow(const v: TKDT7DI8_VecType): Double;
    class function KDT7DI8Distance(const v1, v2: TKDT7DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT7DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT8DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT8DI8_Vec = array [0 .. KDT8DI8_Axis - 1] of TKDT8DI8_VecType;
    PKDT8DI8_Vec = ^TKDT8DI8_Vec;

    TKDT8DI8_DynamicVecBuffer = array of TKDT8DI8_Vec;
    PKDT8DI8_DynamicVecBuffer = ^TKDT8DI8_DynamicVecBuffer;

    TKDT8DI8_Source = record
      buff: TKDT8DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT8DI8_Source = ^TKDT8DI8_Source;
    TKDT8DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT8DI8_Source) - 1] of PKDT8DI8_Source;
    PKDT8DI8_SourceBuffer = ^TKDT8DI8_SourceBuffer;

    TKDT8DI8_DyanmicSourceBuffer = array of PKDT8DI8_Source;
    PKDT8DI8_DyanmicSourceBuffer = ^TKDT8DI8_DyanmicSourceBuffer;

    TKDT8DI8_DyanmicStoreBuffer = array of TKDT8DI8_Source;
    PKDT8DI8_DyanmicStoreBuffer = ^TKDT8DI8_DyanmicStoreBuffer;

    PKDT8DI8_Node = ^TKDT8DI8_Node;

    TKDT8DI8_Node = record
      Parent, Right, Left: PKDT8DI8_Node;
      vec: PKDT8DI8_Source;
    end;

    TKDT8DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT8DI8_Source; const Data: Pointer);
    TKDT8DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT8DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT8DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT8DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT8DI8_DyanmicStoreBuffer;
    KDBuff: TKDT8DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT8DI8_Node;
    TestBuff: TKDT8DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT8DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT8DI8_Node;
    function GetData(const Index: NativeInt): PKDT8DI8_Source;
  public
    RootNode: PKDT8DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT8DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT8DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT8DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT8DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT8DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT8DI8_Node; overload;
    function Search(const buff: TKDT8DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT8DI8_Node; overload;
    function Search(const buff: TKDT8DI8_Vec; var SearchedDistanceMin: Double): PKDT8DI8_Node; overload;
    function Search(const buff: TKDT8DI8_Vec): PKDT8DI8_Node; overload;
    function SearchToken(const buff: TKDT8DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT8DI8_DynamicVecBuffer; var OutBuff: TKDT8DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT8DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT8DI8_Node);
    procedure PrintBuffer;

    class function KDT8DI8Vec(const s: SystemString): TKDT8DI8_Vec; overload;
    class function KDT8DI8Vec(const v: TKDT8DI8_Vec): SystemString; overload;
    class function KDT8DI8Pow(const v: TKDT8DI8_VecType): Double;
    class function KDT8DI8Distance(const v1, v2: TKDT8DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT8DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT9DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT9DI8_Vec = array [0 .. KDT9DI8_Axis - 1] of TKDT9DI8_VecType;
    PKDT9DI8_Vec = ^TKDT9DI8_Vec;

    TKDT9DI8_DynamicVecBuffer = array of TKDT9DI8_Vec;
    PKDT9DI8_DynamicVecBuffer = ^TKDT9DI8_DynamicVecBuffer;

    TKDT9DI8_Source = record
      buff: TKDT9DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT9DI8_Source = ^TKDT9DI8_Source;
    TKDT9DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT9DI8_Source) - 1] of PKDT9DI8_Source;
    PKDT9DI8_SourceBuffer = ^TKDT9DI8_SourceBuffer;

    TKDT9DI8_DyanmicSourceBuffer = array of PKDT9DI8_Source;
    PKDT9DI8_DyanmicSourceBuffer = ^TKDT9DI8_DyanmicSourceBuffer;

    TKDT9DI8_DyanmicStoreBuffer = array of TKDT9DI8_Source;
    PKDT9DI8_DyanmicStoreBuffer = ^TKDT9DI8_DyanmicStoreBuffer;

    PKDT9DI8_Node = ^TKDT9DI8_Node;

    TKDT9DI8_Node = record
      Parent, Right, Left: PKDT9DI8_Node;
      vec: PKDT9DI8_Source;
    end;

    TKDT9DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT9DI8_Source; const Data: Pointer);
    TKDT9DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT9DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT9DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT9DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT9DI8_DyanmicStoreBuffer;
    KDBuff: TKDT9DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT9DI8_Node;
    TestBuff: TKDT9DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT9DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT9DI8_Node;
    function GetData(const Index: NativeInt): PKDT9DI8_Source;
  public
    RootNode: PKDT9DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT9DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT9DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT9DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT9DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT9DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT9DI8_Node; overload;
    function Search(const buff: TKDT9DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT9DI8_Node; overload;
    function Search(const buff: TKDT9DI8_Vec; var SearchedDistanceMin: Double): PKDT9DI8_Node; overload;
    function Search(const buff: TKDT9DI8_Vec): PKDT9DI8_Node; overload;
    function SearchToken(const buff: TKDT9DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT9DI8_DynamicVecBuffer; var OutBuff: TKDT9DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT9DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT9DI8_Node);
    procedure PrintBuffer;

    class function KDT9DI8Vec(const s: SystemString): TKDT9DI8_Vec; overload;
    class function KDT9DI8Vec(const v: TKDT9DI8_Vec): SystemString; overload;
    class function KDT9DI8Pow(const v: TKDT9DI8_VecType): Double;
    class function KDT9DI8Distance(const v1, v2: TKDT9DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT9DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT10DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT10DI8_Vec = array [0 .. KDT10DI8_Axis - 1] of TKDT10DI8_VecType;
    PKDT10DI8_Vec = ^TKDT10DI8_Vec;

    TKDT10DI8_DynamicVecBuffer = array of TKDT10DI8_Vec;
    PKDT10DI8_DynamicVecBuffer = ^TKDT10DI8_DynamicVecBuffer;

    TKDT10DI8_Source = record
      buff: TKDT10DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT10DI8_Source = ^TKDT10DI8_Source;
    TKDT10DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT10DI8_Source) - 1] of PKDT10DI8_Source;
    PKDT10DI8_SourceBuffer = ^TKDT10DI8_SourceBuffer;

    TKDT10DI8_DyanmicSourceBuffer = array of PKDT10DI8_Source;
    PKDT10DI8_DyanmicSourceBuffer = ^TKDT10DI8_DyanmicSourceBuffer;

    TKDT10DI8_DyanmicStoreBuffer = array of TKDT10DI8_Source;
    PKDT10DI8_DyanmicStoreBuffer = ^TKDT10DI8_DyanmicStoreBuffer;

    PKDT10DI8_Node = ^TKDT10DI8_Node;

    TKDT10DI8_Node = record
      Parent, Right, Left: PKDT10DI8_Node;
      vec: PKDT10DI8_Source;
    end;

    TKDT10DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT10DI8_Source; const Data: Pointer);
    TKDT10DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT10DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT10DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT10DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT10DI8_DyanmicStoreBuffer;
    KDBuff: TKDT10DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT10DI8_Node;
    TestBuff: TKDT10DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT10DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT10DI8_Node;
    function GetData(const Index: NativeInt): PKDT10DI8_Source;
  public
    RootNode: PKDT10DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT10DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT10DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT10DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT10DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT10DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT10DI8_Node; overload;
    function Search(const buff: TKDT10DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT10DI8_Node; overload;
    function Search(const buff: TKDT10DI8_Vec; var SearchedDistanceMin: Double): PKDT10DI8_Node; overload;
    function Search(const buff: TKDT10DI8_Vec): PKDT10DI8_Node; overload;
    function SearchToken(const buff: TKDT10DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT10DI8_DynamicVecBuffer; var OutBuff: TKDT10DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT10DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT10DI8_Node);
    procedure PrintBuffer;

    class function KDT10DI8Vec(const s: SystemString): TKDT10DI8_Vec; overload;
    class function KDT10DI8Vec(const v: TKDT10DI8_Vec): SystemString; overload;
    class function KDT10DI8Pow(const v: TKDT10DI8_VecType): Double;
    class function KDT10DI8Distance(const v1, v2: TKDT10DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT10DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT11DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT11DI8_Vec = array [0 .. KDT11DI8_Axis - 1] of TKDT11DI8_VecType;
    PKDT11DI8_Vec = ^TKDT11DI8_Vec;

    TKDT11DI8_DynamicVecBuffer = array of TKDT11DI8_Vec;
    PKDT11DI8_DynamicVecBuffer = ^TKDT11DI8_DynamicVecBuffer;

    TKDT11DI8_Source = record
      buff: TKDT11DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT11DI8_Source = ^TKDT11DI8_Source;
    TKDT11DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT11DI8_Source) - 1] of PKDT11DI8_Source;
    PKDT11DI8_SourceBuffer = ^TKDT11DI8_SourceBuffer;

    TKDT11DI8_DyanmicSourceBuffer = array of PKDT11DI8_Source;
    PKDT11DI8_DyanmicSourceBuffer = ^TKDT11DI8_DyanmicSourceBuffer;

    TKDT11DI8_DyanmicStoreBuffer = array of TKDT11DI8_Source;
    PKDT11DI8_DyanmicStoreBuffer = ^TKDT11DI8_DyanmicStoreBuffer;

    PKDT11DI8_Node = ^TKDT11DI8_Node;

    TKDT11DI8_Node = record
      Parent, Right, Left: PKDT11DI8_Node;
      vec: PKDT11DI8_Source;
    end;

    TKDT11DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT11DI8_Source; const Data: Pointer);
    TKDT11DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT11DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT11DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT11DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT11DI8_DyanmicStoreBuffer;
    KDBuff: TKDT11DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT11DI8_Node;
    TestBuff: TKDT11DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT11DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT11DI8_Node;
    function GetData(const Index: NativeInt): PKDT11DI8_Source;
  public
    RootNode: PKDT11DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT11DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT11DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT11DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT11DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT11DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT11DI8_Node; overload;
    function Search(const buff: TKDT11DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT11DI8_Node; overload;
    function Search(const buff: TKDT11DI8_Vec; var SearchedDistanceMin: Double): PKDT11DI8_Node; overload;
    function Search(const buff: TKDT11DI8_Vec): PKDT11DI8_Node; overload;
    function SearchToken(const buff: TKDT11DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT11DI8_DynamicVecBuffer; var OutBuff: TKDT11DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT11DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT11DI8_Node);
    procedure PrintBuffer;

    class function KDT11DI8Vec(const s: SystemString): TKDT11DI8_Vec; overload;
    class function KDT11DI8Vec(const v: TKDT11DI8_Vec): SystemString; overload;
    class function KDT11DI8Pow(const v: TKDT11DI8_VecType): Double;
    class function KDT11DI8Distance(const v1, v2: TKDT11DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT11DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT12DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT12DI8_Vec = array [0 .. KDT12DI8_Axis - 1] of TKDT12DI8_VecType;
    PKDT12DI8_Vec = ^TKDT12DI8_Vec;

    TKDT12DI8_DynamicVecBuffer = array of TKDT12DI8_Vec;
    PKDT12DI8_DynamicVecBuffer = ^TKDT12DI8_DynamicVecBuffer;

    TKDT12DI8_Source = record
      buff: TKDT12DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT12DI8_Source = ^TKDT12DI8_Source;
    TKDT12DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT12DI8_Source) - 1] of PKDT12DI8_Source;
    PKDT12DI8_SourceBuffer = ^TKDT12DI8_SourceBuffer;

    TKDT12DI8_DyanmicSourceBuffer = array of PKDT12DI8_Source;
    PKDT12DI8_DyanmicSourceBuffer = ^TKDT12DI8_DyanmicSourceBuffer;

    TKDT12DI8_DyanmicStoreBuffer = array of TKDT12DI8_Source;
    PKDT12DI8_DyanmicStoreBuffer = ^TKDT12DI8_DyanmicStoreBuffer;

    PKDT12DI8_Node = ^TKDT12DI8_Node;

    TKDT12DI8_Node = record
      Parent, Right, Left: PKDT12DI8_Node;
      vec: PKDT12DI8_Source;
    end;

    TKDT12DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT12DI8_Source; const Data: Pointer);
    TKDT12DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT12DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT12DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT12DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT12DI8_DyanmicStoreBuffer;
    KDBuff: TKDT12DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT12DI8_Node;
    TestBuff: TKDT12DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT12DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT12DI8_Node;
    function GetData(const Index: NativeInt): PKDT12DI8_Source;
  public
    RootNode: PKDT12DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT12DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT12DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT12DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT12DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT12DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT12DI8_Node; overload;
    function Search(const buff: TKDT12DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT12DI8_Node; overload;
    function Search(const buff: TKDT12DI8_Vec; var SearchedDistanceMin: Double): PKDT12DI8_Node; overload;
    function Search(const buff: TKDT12DI8_Vec): PKDT12DI8_Node; overload;
    function SearchToken(const buff: TKDT12DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT12DI8_DynamicVecBuffer; var OutBuff: TKDT12DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT12DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT12DI8_Node);
    procedure PrintBuffer;

    class function KDT12DI8Vec(const s: SystemString): TKDT12DI8_Vec; overload;
    class function KDT12DI8Vec(const v: TKDT12DI8_Vec): SystemString; overload;
    class function KDT12DI8Pow(const v: TKDT12DI8_VecType): Double;
    class function KDT12DI8Distance(const v1, v2: TKDT12DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT12DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT13DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT13DI8_Vec = array [0 .. KDT13DI8_Axis - 1] of TKDT13DI8_VecType;
    PKDT13DI8_Vec = ^TKDT13DI8_Vec;

    TKDT13DI8_DynamicVecBuffer = array of TKDT13DI8_Vec;
    PKDT13DI8_DynamicVecBuffer = ^TKDT13DI8_DynamicVecBuffer;

    TKDT13DI8_Source = record
      buff: TKDT13DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT13DI8_Source = ^TKDT13DI8_Source;
    TKDT13DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT13DI8_Source) - 1] of PKDT13DI8_Source;
    PKDT13DI8_SourceBuffer = ^TKDT13DI8_SourceBuffer;

    TKDT13DI8_DyanmicSourceBuffer = array of PKDT13DI8_Source;
    PKDT13DI8_DyanmicSourceBuffer = ^TKDT13DI8_DyanmicSourceBuffer;

    TKDT13DI8_DyanmicStoreBuffer = array of TKDT13DI8_Source;
    PKDT13DI8_DyanmicStoreBuffer = ^TKDT13DI8_DyanmicStoreBuffer;

    PKDT13DI8_Node = ^TKDT13DI8_Node;

    TKDT13DI8_Node = record
      Parent, Right, Left: PKDT13DI8_Node;
      vec: PKDT13DI8_Source;
    end;

    TKDT13DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT13DI8_Source; const Data: Pointer);
    TKDT13DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT13DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT13DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT13DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT13DI8_DyanmicStoreBuffer;
    KDBuff: TKDT13DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT13DI8_Node;
    TestBuff: TKDT13DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT13DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT13DI8_Node;
    function GetData(const Index: NativeInt): PKDT13DI8_Source;
  public
    RootNode: PKDT13DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT13DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT13DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT13DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT13DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT13DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT13DI8_Node; overload;
    function Search(const buff: TKDT13DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT13DI8_Node; overload;
    function Search(const buff: TKDT13DI8_Vec; var SearchedDistanceMin: Double): PKDT13DI8_Node; overload;
    function Search(const buff: TKDT13DI8_Vec): PKDT13DI8_Node; overload;
    function SearchToken(const buff: TKDT13DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT13DI8_DynamicVecBuffer; var OutBuff: TKDT13DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT13DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT13DI8_Node);
    procedure PrintBuffer;

    class function KDT13DI8Vec(const s: SystemString): TKDT13DI8_Vec; overload;
    class function KDT13DI8Vec(const v: TKDT13DI8_Vec): SystemString; overload;
    class function KDT13DI8Pow(const v: TKDT13DI8_VecType): Double;
    class function KDT13DI8Distance(const v1, v2: TKDT13DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT13DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT14DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT14DI8_Vec = array [0 .. KDT14DI8_Axis - 1] of TKDT14DI8_VecType;
    PKDT14DI8_Vec = ^TKDT14DI8_Vec;

    TKDT14DI8_DynamicVecBuffer = array of TKDT14DI8_Vec;
    PKDT14DI8_DynamicVecBuffer = ^TKDT14DI8_DynamicVecBuffer;

    TKDT14DI8_Source = record
      buff: TKDT14DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT14DI8_Source = ^TKDT14DI8_Source;
    TKDT14DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT14DI8_Source) - 1] of PKDT14DI8_Source;
    PKDT14DI8_SourceBuffer = ^TKDT14DI8_SourceBuffer;

    TKDT14DI8_DyanmicSourceBuffer = array of PKDT14DI8_Source;
    PKDT14DI8_DyanmicSourceBuffer = ^TKDT14DI8_DyanmicSourceBuffer;

    TKDT14DI8_DyanmicStoreBuffer = array of TKDT14DI8_Source;
    PKDT14DI8_DyanmicStoreBuffer = ^TKDT14DI8_DyanmicStoreBuffer;

    PKDT14DI8_Node = ^TKDT14DI8_Node;

    TKDT14DI8_Node = record
      Parent, Right, Left: PKDT14DI8_Node;
      vec: PKDT14DI8_Source;
    end;

    TKDT14DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT14DI8_Source; const Data: Pointer);
    TKDT14DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT14DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT14DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT14DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT14DI8_DyanmicStoreBuffer;
    KDBuff: TKDT14DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT14DI8_Node;
    TestBuff: TKDT14DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT14DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT14DI8_Node;
    function GetData(const Index: NativeInt): PKDT14DI8_Source;
  public
    RootNode: PKDT14DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT14DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT14DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT14DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT14DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT14DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT14DI8_Node; overload;
    function Search(const buff: TKDT14DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT14DI8_Node; overload;
    function Search(const buff: TKDT14DI8_Vec; var SearchedDistanceMin: Double): PKDT14DI8_Node; overload;
    function Search(const buff: TKDT14DI8_Vec): PKDT14DI8_Node; overload;
    function SearchToken(const buff: TKDT14DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT14DI8_DynamicVecBuffer; var OutBuff: TKDT14DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT14DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT14DI8_Node);
    procedure PrintBuffer;

    class function KDT14DI8Vec(const s: SystemString): TKDT14DI8_Vec; overload;
    class function KDT14DI8Vec(const v: TKDT14DI8_Vec): SystemString; overload;
    class function KDT14DI8Pow(const v: TKDT14DI8_VecType): Double;
    class function KDT14DI8Distance(const v1, v2: TKDT14DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT14DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT15DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT15DI8_Vec = array [0 .. KDT15DI8_Axis - 1] of TKDT15DI8_VecType;
    PKDT15DI8_Vec = ^TKDT15DI8_Vec;

    TKDT15DI8_DynamicVecBuffer = array of TKDT15DI8_Vec;
    PKDT15DI8_DynamicVecBuffer = ^TKDT15DI8_DynamicVecBuffer;

    TKDT15DI8_Source = record
      buff: TKDT15DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT15DI8_Source = ^TKDT15DI8_Source;
    TKDT15DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT15DI8_Source) - 1] of PKDT15DI8_Source;
    PKDT15DI8_SourceBuffer = ^TKDT15DI8_SourceBuffer;

    TKDT15DI8_DyanmicSourceBuffer = array of PKDT15DI8_Source;
    PKDT15DI8_DyanmicSourceBuffer = ^TKDT15DI8_DyanmicSourceBuffer;

    TKDT15DI8_DyanmicStoreBuffer = array of TKDT15DI8_Source;
    PKDT15DI8_DyanmicStoreBuffer = ^TKDT15DI8_DyanmicStoreBuffer;

    PKDT15DI8_Node = ^TKDT15DI8_Node;

    TKDT15DI8_Node = record
      Parent, Right, Left: PKDT15DI8_Node;
      vec: PKDT15DI8_Source;
    end;

    TKDT15DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT15DI8_Source; const Data: Pointer);
    TKDT15DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT15DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT15DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT15DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT15DI8_DyanmicStoreBuffer;
    KDBuff: TKDT15DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT15DI8_Node;
    TestBuff: TKDT15DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT15DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT15DI8_Node;
    function GetData(const Index: NativeInt): PKDT15DI8_Source;
  public
    RootNode: PKDT15DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT15DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT15DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT15DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT15DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT15DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT15DI8_Node; overload;
    function Search(const buff: TKDT15DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT15DI8_Node; overload;
    function Search(const buff: TKDT15DI8_Vec; var SearchedDistanceMin: Double): PKDT15DI8_Node; overload;
    function Search(const buff: TKDT15DI8_Vec): PKDT15DI8_Node; overload;
    function SearchToken(const buff: TKDT15DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT15DI8_DynamicVecBuffer; var OutBuff: TKDT15DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT15DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT15DI8_Node);
    procedure PrintBuffer;

    class function KDT15DI8Vec(const s: SystemString): TKDT15DI8_Vec; overload;
    class function KDT15DI8Vec(const v: TKDT15DI8_Vec): SystemString; overload;
    class function KDT15DI8Pow(const v: TKDT15DI8_VecType): Double;
    class function KDT15DI8Distance(const v1, v2: TKDT15DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT15DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT16DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT16DI8_Vec = array [0 .. KDT16DI8_Axis - 1] of TKDT16DI8_VecType;
    PKDT16DI8_Vec = ^TKDT16DI8_Vec;

    TKDT16DI8_DynamicVecBuffer = array of TKDT16DI8_Vec;
    PKDT16DI8_DynamicVecBuffer = ^TKDT16DI8_DynamicVecBuffer;

    TKDT16DI8_Source = record
      buff: TKDT16DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT16DI8_Source = ^TKDT16DI8_Source;
    TKDT16DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT16DI8_Source) - 1] of PKDT16DI8_Source;
    PKDT16DI8_SourceBuffer = ^TKDT16DI8_SourceBuffer;

    TKDT16DI8_DyanmicSourceBuffer = array of PKDT16DI8_Source;
    PKDT16DI8_DyanmicSourceBuffer = ^TKDT16DI8_DyanmicSourceBuffer;

    TKDT16DI8_DyanmicStoreBuffer = array of TKDT16DI8_Source;
    PKDT16DI8_DyanmicStoreBuffer = ^TKDT16DI8_DyanmicStoreBuffer;

    PKDT16DI8_Node = ^TKDT16DI8_Node;

    TKDT16DI8_Node = record
      Parent, Right, Left: PKDT16DI8_Node;
      vec: PKDT16DI8_Source;
    end;

    TKDT16DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT16DI8_Source; const Data: Pointer);
    TKDT16DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT16DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT16DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT16DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT16DI8_DyanmicStoreBuffer;
    KDBuff: TKDT16DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT16DI8_Node;
    TestBuff: TKDT16DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT16DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT16DI8_Node;
    function GetData(const Index: NativeInt): PKDT16DI8_Source;
  public
    RootNode: PKDT16DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT16DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT16DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT16DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT16DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT16DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT16DI8_Node; overload;
    function Search(const buff: TKDT16DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT16DI8_Node; overload;
    function Search(const buff: TKDT16DI8_Vec; var SearchedDistanceMin: Double): PKDT16DI8_Node; overload;
    function Search(const buff: TKDT16DI8_Vec): PKDT16DI8_Node; overload;
    function SearchToken(const buff: TKDT16DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT16DI8_DynamicVecBuffer; var OutBuff: TKDT16DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT16DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT16DI8_Node);
    procedure PrintBuffer;

    class function KDT16DI8Vec(const s: SystemString): TKDT16DI8_Vec; overload;
    class function KDT16DI8Vec(const v: TKDT16DI8_Vec): SystemString; overload;
    class function KDT16DI8Pow(const v: TKDT16DI8_VecType): Double;
    class function KDT16DI8Distance(const v1, v2: TKDT16DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT16DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT17DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT17DI8_Vec = array [0 .. KDT17DI8_Axis - 1] of TKDT17DI8_VecType;
    PKDT17DI8_Vec = ^TKDT17DI8_Vec;

    TKDT17DI8_DynamicVecBuffer = array of TKDT17DI8_Vec;
    PKDT17DI8_DynamicVecBuffer = ^TKDT17DI8_DynamicVecBuffer;

    TKDT17DI8_Source = record
      buff: TKDT17DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT17DI8_Source = ^TKDT17DI8_Source;
    TKDT17DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT17DI8_Source) - 1] of PKDT17DI8_Source;
    PKDT17DI8_SourceBuffer = ^TKDT17DI8_SourceBuffer;

    TKDT17DI8_DyanmicSourceBuffer = array of PKDT17DI8_Source;
    PKDT17DI8_DyanmicSourceBuffer = ^TKDT17DI8_DyanmicSourceBuffer;

    TKDT17DI8_DyanmicStoreBuffer = array of TKDT17DI8_Source;
    PKDT17DI8_DyanmicStoreBuffer = ^TKDT17DI8_DyanmicStoreBuffer;

    PKDT17DI8_Node = ^TKDT17DI8_Node;

    TKDT17DI8_Node = record
      Parent, Right, Left: PKDT17DI8_Node;
      vec: PKDT17DI8_Source;
    end;

    TKDT17DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT17DI8_Source; const Data: Pointer);
    TKDT17DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT17DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT17DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT17DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT17DI8_DyanmicStoreBuffer;
    KDBuff: TKDT17DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT17DI8_Node;
    TestBuff: TKDT17DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT17DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT17DI8_Node;
    function GetData(const Index: NativeInt): PKDT17DI8_Source;
  public
    RootNode: PKDT17DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT17DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT17DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT17DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT17DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT17DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT17DI8_Node; overload;
    function Search(const buff: TKDT17DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT17DI8_Node; overload;
    function Search(const buff: TKDT17DI8_Vec; var SearchedDistanceMin: Double): PKDT17DI8_Node; overload;
    function Search(const buff: TKDT17DI8_Vec): PKDT17DI8_Node; overload;
    function SearchToken(const buff: TKDT17DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT17DI8_DynamicVecBuffer; var OutBuff: TKDT17DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT17DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT17DI8_Node);
    procedure PrintBuffer;

    class function KDT17DI8Vec(const s: SystemString): TKDT17DI8_Vec; overload;
    class function KDT17DI8Vec(const v: TKDT17DI8_Vec): SystemString; overload;
    class function KDT17DI8Pow(const v: TKDT17DI8_VecType): Double;
    class function KDT17DI8Distance(const v1, v2: TKDT17DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT17DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT18DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT18DI8_Vec = array [0 .. KDT18DI8_Axis - 1] of TKDT18DI8_VecType;
    PKDT18DI8_Vec = ^TKDT18DI8_Vec;

    TKDT18DI8_DynamicVecBuffer = array of TKDT18DI8_Vec;
    PKDT18DI8_DynamicVecBuffer = ^TKDT18DI8_DynamicVecBuffer;

    TKDT18DI8_Source = record
      buff: TKDT18DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT18DI8_Source = ^TKDT18DI8_Source;
    TKDT18DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT18DI8_Source) - 1] of PKDT18DI8_Source;
    PKDT18DI8_SourceBuffer = ^TKDT18DI8_SourceBuffer;

    TKDT18DI8_DyanmicSourceBuffer = array of PKDT18DI8_Source;
    PKDT18DI8_DyanmicSourceBuffer = ^TKDT18DI8_DyanmicSourceBuffer;

    TKDT18DI8_DyanmicStoreBuffer = array of TKDT18DI8_Source;
    PKDT18DI8_DyanmicStoreBuffer = ^TKDT18DI8_DyanmicStoreBuffer;

    PKDT18DI8_Node = ^TKDT18DI8_Node;

    TKDT18DI8_Node = record
      Parent, Right, Left: PKDT18DI8_Node;
      vec: PKDT18DI8_Source;
    end;

    TKDT18DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT18DI8_Source; const Data: Pointer);
    TKDT18DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT18DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT18DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT18DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT18DI8_DyanmicStoreBuffer;
    KDBuff: TKDT18DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT18DI8_Node;
    TestBuff: TKDT18DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT18DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT18DI8_Node;
    function GetData(const Index: NativeInt): PKDT18DI8_Source;
  public
    RootNode: PKDT18DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT18DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT18DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT18DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT18DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT18DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT18DI8_Node; overload;
    function Search(const buff: TKDT18DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT18DI8_Node; overload;
    function Search(const buff: TKDT18DI8_Vec; var SearchedDistanceMin: Double): PKDT18DI8_Node; overload;
    function Search(const buff: TKDT18DI8_Vec): PKDT18DI8_Node; overload;
    function SearchToken(const buff: TKDT18DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT18DI8_DynamicVecBuffer; var OutBuff: TKDT18DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT18DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT18DI8_Node);
    procedure PrintBuffer;

    class function KDT18DI8Vec(const s: SystemString): TKDT18DI8_Vec; overload;
    class function KDT18DI8Vec(const v: TKDT18DI8_Vec): SystemString; overload;
    class function KDT18DI8Pow(const v: TKDT18DI8_VecType): Double;
    class function KDT18DI8Distance(const v1, v2: TKDT18DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT18DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT19DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT19DI8_Vec = array [0 .. KDT19DI8_Axis - 1] of TKDT19DI8_VecType;
    PKDT19DI8_Vec = ^TKDT19DI8_Vec;

    TKDT19DI8_DynamicVecBuffer = array of TKDT19DI8_Vec;
    PKDT19DI8_DynamicVecBuffer = ^TKDT19DI8_DynamicVecBuffer;

    TKDT19DI8_Source = record
      buff: TKDT19DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT19DI8_Source = ^TKDT19DI8_Source;
    TKDT19DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT19DI8_Source) - 1] of PKDT19DI8_Source;
    PKDT19DI8_SourceBuffer = ^TKDT19DI8_SourceBuffer;

    TKDT19DI8_DyanmicSourceBuffer = array of PKDT19DI8_Source;
    PKDT19DI8_DyanmicSourceBuffer = ^TKDT19DI8_DyanmicSourceBuffer;

    TKDT19DI8_DyanmicStoreBuffer = array of TKDT19DI8_Source;
    PKDT19DI8_DyanmicStoreBuffer = ^TKDT19DI8_DyanmicStoreBuffer;

    PKDT19DI8_Node = ^TKDT19DI8_Node;

    TKDT19DI8_Node = record
      Parent, Right, Left: PKDT19DI8_Node;
      vec: PKDT19DI8_Source;
    end;

    TKDT19DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT19DI8_Source; const Data: Pointer);
    TKDT19DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT19DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT19DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT19DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT19DI8_DyanmicStoreBuffer;
    KDBuff: TKDT19DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT19DI8_Node;
    TestBuff: TKDT19DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT19DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT19DI8_Node;
    function GetData(const Index: NativeInt): PKDT19DI8_Source;
  public
    RootNode: PKDT19DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT19DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT19DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT19DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT19DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT19DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT19DI8_Node; overload;
    function Search(const buff: TKDT19DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT19DI8_Node; overload;
    function Search(const buff: TKDT19DI8_Vec; var SearchedDistanceMin: Double): PKDT19DI8_Node; overload;
    function Search(const buff: TKDT19DI8_Vec): PKDT19DI8_Node; overload;
    function SearchToken(const buff: TKDT19DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT19DI8_DynamicVecBuffer; var OutBuff: TKDT19DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT19DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT19DI8_Node);
    procedure PrintBuffer;

    class function KDT19DI8Vec(const s: SystemString): TKDT19DI8_Vec; overload;
    class function KDT19DI8Vec(const v: TKDT19DI8_Vec): SystemString; overload;
    class function KDT19DI8Pow(const v: TKDT19DI8_VecType): Double;
    class function KDT19DI8Distance(const v1, v2: TKDT19DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT19DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT20DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT20DI8_Vec = array [0 .. KDT20DI8_Axis - 1] of TKDT20DI8_VecType;
    PKDT20DI8_Vec = ^TKDT20DI8_Vec;

    TKDT20DI8_DynamicVecBuffer = array of TKDT20DI8_Vec;
    PKDT20DI8_DynamicVecBuffer = ^TKDT20DI8_DynamicVecBuffer;

    TKDT20DI8_Source = record
      buff: TKDT20DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT20DI8_Source = ^TKDT20DI8_Source;
    TKDT20DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT20DI8_Source) - 1] of PKDT20DI8_Source;
    PKDT20DI8_SourceBuffer = ^TKDT20DI8_SourceBuffer;

    TKDT20DI8_DyanmicSourceBuffer = array of PKDT20DI8_Source;
    PKDT20DI8_DyanmicSourceBuffer = ^TKDT20DI8_DyanmicSourceBuffer;

    TKDT20DI8_DyanmicStoreBuffer = array of TKDT20DI8_Source;
    PKDT20DI8_DyanmicStoreBuffer = ^TKDT20DI8_DyanmicStoreBuffer;

    PKDT20DI8_Node = ^TKDT20DI8_Node;

    TKDT20DI8_Node = record
      Parent, Right, Left: PKDT20DI8_Node;
      vec: PKDT20DI8_Source;
    end;

    TKDT20DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT20DI8_Source; const Data: Pointer);
    TKDT20DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT20DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT20DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT20DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT20DI8_DyanmicStoreBuffer;
    KDBuff: TKDT20DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT20DI8_Node;
    TestBuff: TKDT20DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT20DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT20DI8_Node;
    function GetData(const Index: NativeInt): PKDT20DI8_Source;
  public
    RootNode: PKDT20DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT20DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT20DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT20DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT20DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT20DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT20DI8_Node; overload;
    function Search(const buff: TKDT20DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT20DI8_Node; overload;
    function Search(const buff: TKDT20DI8_Vec; var SearchedDistanceMin: Double): PKDT20DI8_Node; overload;
    function Search(const buff: TKDT20DI8_Vec): PKDT20DI8_Node; overload;
    function SearchToken(const buff: TKDT20DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT20DI8_DynamicVecBuffer; var OutBuff: TKDT20DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT20DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT20DI8_Node);
    procedure PrintBuffer;

    class function KDT20DI8Vec(const s: SystemString): TKDT20DI8_Vec; overload;
    class function KDT20DI8Vec(const v: TKDT20DI8_Vec): SystemString; overload;
    class function KDT20DI8Pow(const v: TKDT20DI8_VecType): Double;
    class function KDT20DI8Distance(const v1, v2: TKDT20DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT20DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT21DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT21DI8_Vec = array [0 .. KDT21DI8_Axis - 1] of TKDT21DI8_VecType;
    PKDT21DI8_Vec = ^TKDT21DI8_Vec;

    TKDT21DI8_DynamicVecBuffer = array of TKDT21DI8_Vec;
    PKDT21DI8_DynamicVecBuffer = ^TKDT21DI8_DynamicVecBuffer;

    TKDT21DI8_Source = record
      buff: TKDT21DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT21DI8_Source = ^TKDT21DI8_Source;
    TKDT21DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT21DI8_Source) - 1] of PKDT21DI8_Source;
    PKDT21DI8_SourceBuffer = ^TKDT21DI8_SourceBuffer;

    TKDT21DI8_DyanmicSourceBuffer = array of PKDT21DI8_Source;
    PKDT21DI8_DyanmicSourceBuffer = ^TKDT21DI8_DyanmicSourceBuffer;

    TKDT21DI8_DyanmicStoreBuffer = array of TKDT21DI8_Source;
    PKDT21DI8_DyanmicStoreBuffer = ^TKDT21DI8_DyanmicStoreBuffer;

    PKDT21DI8_Node = ^TKDT21DI8_Node;

    TKDT21DI8_Node = record
      Parent, Right, Left: PKDT21DI8_Node;
      vec: PKDT21DI8_Source;
    end;

    TKDT21DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT21DI8_Source; const Data: Pointer);
    TKDT21DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT21DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT21DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT21DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT21DI8_DyanmicStoreBuffer;
    KDBuff: TKDT21DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT21DI8_Node;
    TestBuff: TKDT21DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT21DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT21DI8_Node;
    function GetData(const Index: NativeInt): PKDT21DI8_Source;
  public
    RootNode: PKDT21DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT21DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT21DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT21DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT21DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT21DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT21DI8_Node; overload;
    function Search(const buff: TKDT21DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT21DI8_Node; overload;
    function Search(const buff: TKDT21DI8_Vec; var SearchedDistanceMin: Double): PKDT21DI8_Node; overload;
    function Search(const buff: TKDT21DI8_Vec): PKDT21DI8_Node; overload;
    function SearchToken(const buff: TKDT21DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT21DI8_DynamicVecBuffer; var OutBuff: TKDT21DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT21DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT21DI8_Node);
    procedure PrintBuffer;

    class function KDT21DI8Vec(const s: SystemString): TKDT21DI8_Vec; overload;
    class function KDT21DI8Vec(const v: TKDT21DI8_Vec): SystemString; overload;
    class function KDT21DI8Pow(const v: TKDT21DI8_VecType): Double;
    class function KDT21DI8Distance(const v1, v2: TKDT21DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT21DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT22DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT22DI8_Vec = array [0 .. KDT22DI8_Axis - 1] of TKDT22DI8_VecType;
    PKDT22DI8_Vec = ^TKDT22DI8_Vec;

    TKDT22DI8_DynamicVecBuffer = array of TKDT22DI8_Vec;
    PKDT22DI8_DynamicVecBuffer = ^TKDT22DI8_DynamicVecBuffer;

    TKDT22DI8_Source = record
      buff: TKDT22DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT22DI8_Source = ^TKDT22DI8_Source;
    TKDT22DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT22DI8_Source) - 1] of PKDT22DI8_Source;
    PKDT22DI8_SourceBuffer = ^TKDT22DI8_SourceBuffer;

    TKDT22DI8_DyanmicSourceBuffer = array of PKDT22DI8_Source;
    PKDT22DI8_DyanmicSourceBuffer = ^TKDT22DI8_DyanmicSourceBuffer;

    TKDT22DI8_DyanmicStoreBuffer = array of TKDT22DI8_Source;
    PKDT22DI8_DyanmicStoreBuffer = ^TKDT22DI8_DyanmicStoreBuffer;

    PKDT22DI8_Node = ^TKDT22DI8_Node;

    TKDT22DI8_Node = record
      Parent, Right, Left: PKDT22DI8_Node;
      vec: PKDT22DI8_Source;
    end;

    TKDT22DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT22DI8_Source; const Data: Pointer);
    TKDT22DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT22DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT22DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT22DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT22DI8_DyanmicStoreBuffer;
    KDBuff: TKDT22DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT22DI8_Node;
    TestBuff: TKDT22DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT22DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT22DI8_Node;
    function GetData(const Index: NativeInt): PKDT22DI8_Source;
  public
    RootNode: PKDT22DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT22DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT22DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT22DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT22DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT22DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT22DI8_Node; overload;
    function Search(const buff: TKDT22DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT22DI8_Node; overload;
    function Search(const buff: TKDT22DI8_Vec; var SearchedDistanceMin: Double): PKDT22DI8_Node; overload;
    function Search(const buff: TKDT22DI8_Vec): PKDT22DI8_Node; overload;
    function SearchToken(const buff: TKDT22DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT22DI8_DynamicVecBuffer; var OutBuff: TKDT22DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT22DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT22DI8_Node);
    procedure PrintBuffer;

    class function KDT22DI8Vec(const s: SystemString): TKDT22DI8_Vec; overload;
    class function KDT22DI8Vec(const v: TKDT22DI8_Vec): SystemString; overload;
    class function KDT22DI8Pow(const v: TKDT22DI8_VecType): Double;
    class function KDT22DI8Distance(const v1, v2: TKDT22DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT22DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT23DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT23DI8_Vec = array [0 .. KDT23DI8_Axis - 1] of TKDT23DI8_VecType;
    PKDT23DI8_Vec = ^TKDT23DI8_Vec;

    TKDT23DI8_DynamicVecBuffer = array of TKDT23DI8_Vec;
    PKDT23DI8_DynamicVecBuffer = ^TKDT23DI8_DynamicVecBuffer;

    TKDT23DI8_Source = record
      buff: TKDT23DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT23DI8_Source = ^TKDT23DI8_Source;
    TKDT23DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT23DI8_Source) - 1] of PKDT23DI8_Source;
    PKDT23DI8_SourceBuffer = ^TKDT23DI8_SourceBuffer;

    TKDT23DI8_DyanmicSourceBuffer = array of PKDT23DI8_Source;
    PKDT23DI8_DyanmicSourceBuffer = ^TKDT23DI8_DyanmicSourceBuffer;

    TKDT23DI8_DyanmicStoreBuffer = array of TKDT23DI8_Source;
    PKDT23DI8_DyanmicStoreBuffer = ^TKDT23DI8_DyanmicStoreBuffer;

    PKDT23DI8_Node = ^TKDT23DI8_Node;

    TKDT23DI8_Node = record
      Parent, Right, Left: PKDT23DI8_Node;
      vec: PKDT23DI8_Source;
    end;

    TKDT23DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT23DI8_Source; const Data: Pointer);
    TKDT23DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT23DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT23DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT23DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT23DI8_DyanmicStoreBuffer;
    KDBuff: TKDT23DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT23DI8_Node;
    TestBuff: TKDT23DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT23DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT23DI8_Node;
    function GetData(const Index: NativeInt): PKDT23DI8_Source;
  public
    RootNode: PKDT23DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT23DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT23DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT23DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT23DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT23DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT23DI8_Node; overload;
    function Search(const buff: TKDT23DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT23DI8_Node; overload;
    function Search(const buff: TKDT23DI8_Vec; var SearchedDistanceMin: Double): PKDT23DI8_Node; overload;
    function Search(const buff: TKDT23DI8_Vec): PKDT23DI8_Node; overload;
    function SearchToken(const buff: TKDT23DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT23DI8_DynamicVecBuffer; var OutBuff: TKDT23DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT23DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT23DI8_Node);
    procedure PrintBuffer;

    class function KDT23DI8Vec(const s: SystemString): TKDT23DI8_Vec; overload;
    class function KDT23DI8Vec(const v: TKDT23DI8_Vec): SystemString; overload;
    class function KDT23DI8Pow(const v: TKDT23DI8_VecType): Double;
    class function KDT23DI8Distance(const v1, v2: TKDT23DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT23DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT24DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT24DI8_Vec = array [0 .. KDT24DI8_Axis - 1] of TKDT24DI8_VecType;
    PKDT24DI8_Vec = ^TKDT24DI8_Vec;

    TKDT24DI8_DynamicVecBuffer = array of TKDT24DI8_Vec;
    PKDT24DI8_DynamicVecBuffer = ^TKDT24DI8_DynamicVecBuffer;

    TKDT24DI8_Source = record
      buff: TKDT24DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT24DI8_Source = ^TKDT24DI8_Source;
    TKDT24DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT24DI8_Source) - 1] of PKDT24DI8_Source;
    PKDT24DI8_SourceBuffer = ^TKDT24DI8_SourceBuffer;

    TKDT24DI8_DyanmicSourceBuffer = array of PKDT24DI8_Source;
    PKDT24DI8_DyanmicSourceBuffer = ^TKDT24DI8_DyanmicSourceBuffer;

    TKDT24DI8_DyanmicStoreBuffer = array of TKDT24DI8_Source;
    PKDT24DI8_DyanmicStoreBuffer = ^TKDT24DI8_DyanmicStoreBuffer;

    PKDT24DI8_Node = ^TKDT24DI8_Node;

    TKDT24DI8_Node = record
      Parent, Right, Left: PKDT24DI8_Node;
      vec: PKDT24DI8_Source;
    end;

    TKDT24DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT24DI8_Source; const Data: Pointer);
    TKDT24DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT24DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT24DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT24DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT24DI8_DyanmicStoreBuffer;
    KDBuff: TKDT24DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT24DI8_Node;
    TestBuff: TKDT24DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT24DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT24DI8_Node;
    function GetData(const Index: NativeInt): PKDT24DI8_Source;
  public
    RootNode: PKDT24DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT24DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT24DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT24DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT24DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT24DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT24DI8_Node; overload;
    function Search(const buff: TKDT24DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT24DI8_Node; overload;
    function Search(const buff: TKDT24DI8_Vec; var SearchedDistanceMin: Double): PKDT24DI8_Node; overload;
    function Search(const buff: TKDT24DI8_Vec): PKDT24DI8_Node; overload;
    function SearchToken(const buff: TKDT24DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT24DI8_DynamicVecBuffer; var OutBuff: TKDT24DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT24DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT24DI8_Node);
    procedure PrintBuffer;

    class function KDT24DI8Vec(const s: SystemString): TKDT24DI8_Vec; overload;
    class function KDT24DI8Vec(const v: TKDT24DI8_Vec): SystemString; overload;
    class function KDT24DI8Pow(const v: TKDT24DI8_VecType): Double;
    class function KDT24DI8Distance(const v1, v2: TKDT24DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT24DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT256DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT256DI8_Vec = array [0 .. KDT256DI8_Axis - 1] of TKDT256DI8_VecType;
    PKDT256DI8_Vec = ^TKDT256DI8_Vec;

    TKDT256DI8_DynamicVecBuffer = array of TKDT256DI8_Vec;
    PKDT256DI8_DynamicVecBuffer = ^TKDT256DI8_DynamicVecBuffer;

    TKDT256DI8_Source = record
      buff: TKDT256DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT256DI8_Source = ^TKDT256DI8_Source;
    TKDT256DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT256DI8_Source) - 1] of PKDT256DI8_Source;
    PKDT256DI8_SourceBuffer = ^TKDT256DI8_SourceBuffer;

    TKDT256DI8_DyanmicSourceBuffer = array of PKDT256DI8_Source;
    PKDT256DI8_DyanmicSourceBuffer = ^TKDT256DI8_DyanmicSourceBuffer;

    TKDT256DI8_DyanmicStoreBuffer = array of TKDT256DI8_Source;
    PKDT256DI8_DyanmicStoreBuffer = ^TKDT256DI8_DyanmicStoreBuffer;

    PKDT256DI8_Node = ^TKDT256DI8_Node;

    TKDT256DI8_Node = record
      Parent, Right, Left: PKDT256DI8_Node;
      vec: PKDT256DI8_Source;
    end;

    TKDT256DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT256DI8_Source; const Data: Pointer);
    TKDT256DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT256DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT256DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT256DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT256DI8_DyanmicStoreBuffer;
    KDBuff: TKDT256DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT256DI8_Node;
    TestBuff: TKDT256DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT256DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT256DI8_Node;
    function GetData(const Index: NativeInt): PKDT256DI8_Source;
  public
    RootNode: PKDT256DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT256DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT256DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT256DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT256DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT256DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT256DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT256DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT256DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT256DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT256DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT256DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT256DI8_Node; overload;
    function Search(const buff: TKDT256DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT256DI8_Node; overload;
    function Search(const buff: TKDT256DI8_Vec; var SearchedDistanceMin: Double): PKDT256DI8_Node; overload;
    function Search(const buff: TKDT256DI8_Vec): PKDT256DI8_Node; overload;
    function SearchToken(const buff: TKDT256DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT256DI8_DynamicVecBuffer; var OutBuff: TKDT256DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT256DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT256DI8_Node);
    procedure PrintBuffer;

    class function KDT256DI8Vec(const s: SystemString): TKDT256DI8_Vec; overload;
    class function KDT256DI8Vec(const v: TKDT256DI8_Vec): SystemString; overload;
    class function KDT256DI8Pow(const v: TKDT256DI8_VecType): Double;
    class function KDT256DI8Distance(const v1, v2: TKDT256DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT256DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT512DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT512DI8_Vec = array [0 .. KDT512DI8_Axis - 1] of TKDT512DI8_VecType;
    PKDT512DI8_Vec = ^TKDT512DI8_Vec;

    TKDT512DI8_DynamicVecBuffer = array of TKDT512DI8_Vec;
    PKDT512DI8_DynamicVecBuffer = ^TKDT512DI8_DynamicVecBuffer;

    TKDT512DI8_Source = record
      buff: TKDT512DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT512DI8_Source = ^TKDT512DI8_Source;
    TKDT512DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT512DI8_Source) - 1] of PKDT512DI8_Source;
    PKDT512DI8_SourceBuffer = ^TKDT512DI8_SourceBuffer;

    TKDT512DI8_DyanmicSourceBuffer = array of PKDT512DI8_Source;
    PKDT512DI8_DyanmicSourceBuffer = ^TKDT512DI8_DyanmicSourceBuffer;

    TKDT512DI8_DyanmicStoreBuffer = array of TKDT512DI8_Source;
    PKDT512DI8_DyanmicStoreBuffer = ^TKDT512DI8_DyanmicStoreBuffer;

    PKDT512DI8_Node = ^TKDT512DI8_Node;

    TKDT512DI8_Node = record
      Parent, Right, Left: PKDT512DI8_Node;
      vec: PKDT512DI8_Source;
    end;

    TKDT512DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT512DI8_Source; const Data: Pointer);
    TKDT512DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT512DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT512DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT512DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT512DI8_DyanmicStoreBuffer;
    KDBuff: TKDT512DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT512DI8_Node;
    TestBuff: TKDT512DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT512DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT512DI8_Node;
    function GetData(const Index: NativeInt): PKDT512DI8_Source;
  public
    RootNode: PKDT512DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT512DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT512DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT512DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT512DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT512DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT512DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT512DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT512DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT512DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT512DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT512DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT512DI8_Node; overload;
    function Search(const buff: TKDT512DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT512DI8_Node; overload;
    function Search(const buff: TKDT512DI8_Vec; var SearchedDistanceMin: Double): PKDT512DI8_Node; overload;
    function Search(const buff: TKDT512DI8_Vec): PKDT512DI8_Node; overload;
    function SearchToken(const buff: TKDT512DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT512DI8_DynamicVecBuffer; var OutBuff: TKDT512DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT512DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT512DI8_Node);
    procedure PrintBuffer;

    class function KDT512DI8Vec(const s: SystemString): TKDT512DI8_Vec; overload;
    class function KDT512DI8Vec(const v: TKDT512DI8_Vec): SystemString; overload;
    class function KDT512DI8Pow(const v: TKDT512DI8_VecType): Double;
    class function KDT512DI8Distance(const v1, v2: TKDT512DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT512DI8_Source; const Data: Pointer);
    class procedure Test;
  end;



  TKDT1024DI8 = class(TCoreClassObject)
  public type
    // code split
    TKDT1024DI8_Vec = array [0 .. KDT1024DI8_Axis - 1] of TKDT1024DI8_VecType;
    PKDT1024DI8_Vec = ^TKDT1024DI8_Vec;

    TKDT1024DI8_DynamicVecBuffer = array of TKDT1024DI8_Vec;
    PKDT1024DI8_DynamicVecBuffer = ^TKDT1024DI8_DynamicVecBuffer;

    TKDT1024DI8_Source = record
      buff: TKDT1024DI8_Vec;
      Index: Int64;
      Token: TPascalString;
    end;

    PKDT1024DI8_Source = ^TKDT1024DI8_Source;
    TKDT1024DI8_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDT1024DI8_Source) - 1] of PKDT1024DI8_Source;
    PKDT1024DI8_SourceBuffer = ^TKDT1024DI8_SourceBuffer;

    TKDT1024DI8_DyanmicSourceBuffer = array of PKDT1024DI8_Source;
    PKDT1024DI8_DyanmicSourceBuffer = ^TKDT1024DI8_DyanmicSourceBuffer;

    TKDT1024DI8_DyanmicStoreBuffer = array of TKDT1024DI8_Source;
    PKDT1024DI8_DyanmicStoreBuffer = ^TKDT1024DI8_DyanmicStoreBuffer;

    PKDT1024DI8_Node = ^TKDT1024DI8_Node;

    TKDT1024DI8_Node = record
      Parent, Right, Left: PKDT1024DI8_Node;
      vec: PKDT1024DI8_Source;
    end;

    TKDT1024DI8_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDT1024DI8_Source; const Data: Pointer);
    TKDT1024DI8_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDT1024DI8_Source; const Data: Pointer) of object;
{$IFNDEF FPC} TKDT1024DI8_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDT1024DI8_Source; const Data: Pointer); {$ENDIF}
  private
    KDStoreBuff: TKDT1024DI8_DyanmicStoreBuffer;
    KDBuff: TKDT1024DI8_DyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDT1024DI8_Node;
    TestBuff: TKDT1024DI8_DynamicVecBuffer;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDT1024DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT1024DI8_Node;
    function GetData(const Index: NativeInt): PKDT1024DI8_Source;
  public
    RootNode: PKDT1024DI8_Node;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDT1024DI8_DyanmicStoreBuffer;
    property SourceP[const Index: NativeInt]: PKDT1024DI8_Source read GetData; default;

    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1024DI8_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1024DI8_BuildMethod);
{$IFNDEF FPC} procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1024DI8_BuildProc); {$ENDIF}
    { direct k-means++ clusterization }
    procedure BuildKDTreeWithCluster(const inBuff: TKDT1024DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray); overload;
    procedure BuildKDTreeWithCluster(const inBuff: TKDT1024DI8_DynamicVecBuffer; const k, Restarts: NativeInt); overload;
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1024DI8_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1024DI8_BuildMethod); overload;
{$IFNDEF FPC}
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1024DI8_BuildProc); overload;
{$ENDIF FPC}
    { search }
    function Search(const buff: TKDT1024DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT1024DI8_Node; overload;
    function Search(const buff: TKDT1024DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT1024DI8_Node; overload;
    function Search(const buff: TKDT1024DI8_Vec; var SearchedDistanceMin: Double): PKDT1024DI8_Node; overload;
    function Search(const buff: TKDT1024DI8_Vec): PKDT1024DI8_Node; overload;
    function SearchToken(const buff: TKDT1024DI8_Vec): TPascalString;
    { parallel support }
    procedure Search(const inBuff: TKDT1024DI8_DynamicVecBuffer; var OutBuff: TKDT1024DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;
    procedure Search(const inBuff: TKDT1024DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);

    procedure PrintNodeTree(const NodePtr: PKDT1024DI8_Node);
    procedure PrintBuffer;

    class function KDT1024DI8Vec(const s: SystemString): TKDT1024DI8_Vec; overload;
    class function KDT1024DI8Vec(const v: TKDT1024DI8_Vec): SystemString; overload;
    class function KDT1024DI8Pow(const v: TKDT1024DI8_VecType): Double;
    class function KDT1024DI8Distance(const v1, v2: TKDT1024DI8_Vec): Double;
    // debug time
    procedure Test_BuildM(const IndexFor: NativeInt; var Source: TKDT1024DI8_Source; const Data: Pointer);
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
  SaveToken = $88;



function TKDT1DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT1DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT1DI8_Node;
  function SortCompare(const p1, p2: PKDT1DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT1DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT1DI8_Source;
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
  kdBuffPtr: PKDT1DI8_SourceBuffer;
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
      axis := Depth mod KDT1DI8_Axis;
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

function TKDT1DI8.GetData(const Index: NativeInt): PKDT1DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT1DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT1DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT1DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT1DI8_Node(KDNodes[i]));
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

function TKDT1DI8.StoreBuffPtr: PKDT1DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT1DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT1DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT1DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT1DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT1DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT1DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT1DI8.BuildKDTreeWithCluster(const inBuff: TKDT1DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT1DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT1DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT1DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT1DI8.BuildKDTreeWithCluster(const inBuff: TKDT1DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT1DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DI8_BuildCall);
var
  TempStoreBuff: TKDT1DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT1DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT1DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT1DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT1DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT1DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DI8_BuildMethod);
var
  TempStoreBuff: TKDT1DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT1DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT1DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT1DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT1DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT1DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1DI8_BuildProc);
var
  TempStoreBuff: TKDT1DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT1DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT1DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT1DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT1DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT1DI8.Search(const buff: TKDT1DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT1DI8_Node;

var
  NearestNeighbour: PKDT1DI8_Node;

  function FindParentNode(const buffPtr: PKDT1DI8_Vec; NodePtr: PKDT1DI8_Node): PKDT1DI8_Node;
  var
    Next: PKDT1DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT1DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT1DI8_Node; const buffPtr: PKDT1DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT1DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT1DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT1DI8_Vec; const p1, p2: PKDT1DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT1DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT1DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT1DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT1DI8_Node;
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
  Parent: PKDT1DI8_Node;
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

  SearchedDistanceMin := KDT1DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT1DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT1DI8.Search(const buff: TKDT1DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT1DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT1DI8.Search(const buff: TKDT1DI8_Vec; var SearchedDistanceMin: Double): PKDT1DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT1DI8.Search(const buff: TKDT1DI8_Vec): PKDT1DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT1DI8.SearchToken(const buff: TKDT1DI8_Vec): TPascalString;
var
  p: PKDT1DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT1DI8.Search(const inBuff: TKDT1DI8_DynamicVecBuffer; var OutBuff: TKDT1DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT1DI8_DynamicVecBuffer;
  outBuffPtr: PKDT1DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT1DI8_Node;
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
        p: PKDT1DI8_Node;
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
  p: PKDT1DI8_Node;
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


procedure TKDT1DI8.Search(const inBuff: TKDT1DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT1DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT1DI8_Node;
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
        p: PKDT1DI8_Node;
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
  p: PKDT1DI8_Node;
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


procedure TKDT1DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT1DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT1DI8_Vec));
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

procedure TKDT1DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT1DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT1DI8_Vec)) <> SizeOf(TKDT1DI8_Vec) then
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

procedure TKDT1DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT1DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT1DI8.PrintNodeTree(const NodePtr: PKDT1DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT1DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT1DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT1DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT1DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT1DI8.KDT1DI8Vec(const s: SystemString): TKDT1DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT1DI8_Axis - 1 do
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
            if j >= KDT1DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT1DI8.KDT1DI8Vec(const v: TKDT1DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT1DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT1DI8.KDT1DI8Pow(const v: TKDT1DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT1DI8.KDT1DI8Distance(const v1, v2: TKDT1DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT1DI8_Axis - 1 do
      Result := Result + KDT1DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT1DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT1DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT1DI8.Test;
var
  TKDT1DI8_Test: TKDT1DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT1DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT1DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT1DI8_Test := TKDT1DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT1DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT1DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT1DI8_Axis - 1 do
        TKDT1DI8_Test.TestBuff[i][j] := i * KDT1DI8_Axis + j;

{$IFDEF FPC}
  TKDT1DI8_Test.BuildKDTreeM(length(TKDT1DI8_Test.TestBuff), nil, @TKDT1DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT1DI8_Test.BuildKDTreeM(length(TKDT1DI8_Test.TestBuff), nil, TKDT1DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT1DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT1DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT1DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT1DI8_Test.Search(TKDT1DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT1DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT1DI8_Test.TestBuff));
      TKDT1DI8_Test.Search(TKDT1DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT1DI8Distance(TKDT1DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT1DI8_Test.Clear;
      { kMean test }
      TKDT1DI8_Test.BuildKDTreeWithCluster(TKDT1DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT1DI8_Test.Search(TKDT1DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT1DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT1DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT1DI8_Test);
end;


function TKDT2DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT2DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT2DI8_Node;
  function SortCompare(const p1, p2: PKDT2DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT2DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT2DI8_Source;
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
  kdBuffPtr: PKDT2DI8_SourceBuffer;
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
      axis := Depth mod KDT2DI8_Axis;
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

function TKDT2DI8.GetData(const Index: NativeInt): PKDT2DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT2DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT2DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT2DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT2DI8_Node(KDNodes[i]));
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

function TKDT2DI8.StoreBuffPtr: PKDT2DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT2DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT2DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT2DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT2DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT2DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT2DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT2DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT2DI8.BuildKDTreeWithCluster(const inBuff: TKDT2DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT2DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT2DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT2DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT2DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT2DI8.BuildKDTreeWithCluster(const inBuff: TKDT2DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT2DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DI8_BuildCall);
var
  TempStoreBuff: TKDT2DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT2DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT2DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT2DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT2DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT2DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT2DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DI8_BuildMethod);
var
  TempStoreBuff: TKDT2DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT2DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT2DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT2DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT2DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT2DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT2DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT2DI8_BuildProc);
var
  TempStoreBuff: TKDT2DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT2DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT2DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT2DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT2DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT2DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT2DI8.Search(const buff: TKDT2DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT2DI8_Node;

var
  NearestNeighbour: PKDT2DI8_Node;

  function FindParentNode(const buffPtr: PKDT2DI8_Vec; NodePtr: PKDT2DI8_Node): PKDT2DI8_Node;
  var
    Next: PKDT2DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT2DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT2DI8_Node; const buffPtr: PKDT2DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT2DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT2DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT2DI8_Vec; const p1, p2: PKDT2DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT2DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT2DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT2DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT2DI8_Node;
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
  Parent: PKDT2DI8_Node;
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

  SearchedDistanceMin := KDT2DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT2DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT2DI8.Search(const buff: TKDT2DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT2DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT2DI8.Search(const buff: TKDT2DI8_Vec; var SearchedDistanceMin: Double): PKDT2DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT2DI8.Search(const buff: TKDT2DI8_Vec): PKDT2DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT2DI8.SearchToken(const buff: TKDT2DI8_Vec): TPascalString;
var
  p: PKDT2DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT2DI8.Search(const inBuff: TKDT2DI8_DynamicVecBuffer; var OutBuff: TKDT2DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT2DI8_DynamicVecBuffer;
  outBuffPtr: PKDT2DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT2DI8_Node;
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
        p: PKDT2DI8_Node;
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
  p: PKDT2DI8_Node;
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


procedure TKDT2DI8.Search(const inBuff: TKDT2DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT2DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT2DI8_Node;
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
        p: PKDT2DI8_Node;
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
  p: PKDT2DI8_Node;
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


procedure TKDT2DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT2DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT2DI8_Vec));
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

procedure TKDT2DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT2DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT2DI8_Vec)) <> SizeOf(TKDT2DI8_Vec) then
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

procedure TKDT2DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT2DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT2DI8.PrintNodeTree(const NodePtr: PKDT2DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT2DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT2DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT2DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT2DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT2DI8.KDT2DI8Vec(const s: SystemString): TKDT2DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT2DI8_Axis - 1 do
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
            if j >= KDT2DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT2DI8.KDT2DI8Vec(const v: TKDT2DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT2DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT2DI8.KDT2DI8Pow(const v: TKDT2DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT2DI8.KDT2DI8Distance(const v1, v2: TKDT2DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT2DI8_Axis - 1 do
      Result := Result + KDT2DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT2DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT2DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT2DI8.Test;
var
  TKDT2DI8_Test: TKDT2DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT2DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT2DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT2DI8_Test := TKDT2DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT2DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT2DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT2DI8_Axis - 1 do
        TKDT2DI8_Test.TestBuff[i][j] := i * KDT2DI8_Axis + j;

{$IFDEF FPC}
  TKDT2DI8_Test.BuildKDTreeM(length(TKDT2DI8_Test.TestBuff), nil, @TKDT2DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT2DI8_Test.BuildKDTreeM(length(TKDT2DI8_Test.TestBuff), nil, TKDT2DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT2DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT2DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT2DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT2DI8_Test.Search(TKDT2DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT2DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT2DI8_Test.TestBuff));
      TKDT2DI8_Test.Search(TKDT2DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT2DI8Distance(TKDT2DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT2DI8_Test.Clear;
      { kMean test }
      TKDT2DI8_Test.BuildKDTreeWithCluster(TKDT2DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT2DI8_Test.Search(TKDT2DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT2DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT2DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT2DI8_Test);
end;


function TKDT3DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT3DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT3DI8_Node;
  function SortCompare(const p1, p2: PKDT3DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT3DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT3DI8_Source;
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
  kdBuffPtr: PKDT3DI8_SourceBuffer;
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
      axis := Depth mod KDT3DI8_Axis;
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

function TKDT3DI8.GetData(const Index: NativeInt): PKDT3DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT3DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT3DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT3DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT3DI8_Node(KDNodes[i]));
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

function TKDT3DI8.StoreBuffPtr: PKDT3DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT3DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT3DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT3DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT3DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT3DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT3DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT3DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT3DI8.BuildKDTreeWithCluster(const inBuff: TKDT3DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT3DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT3DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT3DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT3DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT3DI8.BuildKDTreeWithCluster(const inBuff: TKDT3DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT3DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DI8_BuildCall);
var
  TempStoreBuff: TKDT3DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT3DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT3DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT3DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT3DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT3DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT3DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DI8_BuildMethod);
var
  TempStoreBuff: TKDT3DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT3DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT3DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT3DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT3DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT3DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT3DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT3DI8_BuildProc);
var
  TempStoreBuff: TKDT3DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT3DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT3DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT3DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT3DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT3DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT3DI8.Search(const buff: TKDT3DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT3DI8_Node;

var
  NearestNeighbour: PKDT3DI8_Node;

  function FindParentNode(const buffPtr: PKDT3DI8_Vec; NodePtr: PKDT3DI8_Node): PKDT3DI8_Node;
  var
    Next: PKDT3DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT3DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT3DI8_Node; const buffPtr: PKDT3DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT3DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT3DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT3DI8_Vec; const p1, p2: PKDT3DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT3DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT3DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT3DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT3DI8_Node;
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
  Parent: PKDT3DI8_Node;
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

  SearchedDistanceMin := KDT3DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT3DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT3DI8.Search(const buff: TKDT3DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT3DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT3DI8.Search(const buff: TKDT3DI8_Vec; var SearchedDistanceMin: Double): PKDT3DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT3DI8.Search(const buff: TKDT3DI8_Vec): PKDT3DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT3DI8.SearchToken(const buff: TKDT3DI8_Vec): TPascalString;
var
  p: PKDT3DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT3DI8.Search(const inBuff: TKDT3DI8_DynamicVecBuffer; var OutBuff: TKDT3DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT3DI8_DynamicVecBuffer;
  outBuffPtr: PKDT3DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT3DI8_Node;
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
        p: PKDT3DI8_Node;
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
  p: PKDT3DI8_Node;
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


procedure TKDT3DI8.Search(const inBuff: TKDT3DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT3DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT3DI8_Node;
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
        p: PKDT3DI8_Node;
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
  p: PKDT3DI8_Node;
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


procedure TKDT3DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT3DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT3DI8_Vec));
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

procedure TKDT3DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT3DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT3DI8_Vec)) <> SizeOf(TKDT3DI8_Vec) then
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

procedure TKDT3DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT3DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT3DI8.PrintNodeTree(const NodePtr: PKDT3DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT3DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT3DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT3DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT3DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT3DI8.KDT3DI8Vec(const s: SystemString): TKDT3DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT3DI8_Axis - 1 do
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
            if j >= KDT3DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT3DI8.KDT3DI8Vec(const v: TKDT3DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT3DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT3DI8.KDT3DI8Pow(const v: TKDT3DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT3DI8.KDT3DI8Distance(const v1, v2: TKDT3DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT3DI8_Axis - 1 do
      Result := Result + KDT3DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT3DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT3DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT3DI8.Test;
var
  TKDT3DI8_Test: TKDT3DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT3DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT3DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT3DI8_Test := TKDT3DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT3DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT3DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT3DI8_Axis - 1 do
        TKDT3DI8_Test.TestBuff[i][j] := i * KDT3DI8_Axis + j;

{$IFDEF FPC}
  TKDT3DI8_Test.BuildKDTreeM(length(TKDT3DI8_Test.TestBuff), nil, @TKDT3DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT3DI8_Test.BuildKDTreeM(length(TKDT3DI8_Test.TestBuff), nil, TKDT3DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT3DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT3DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT3DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT3DI8_Test.Search(TKDT3DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT3DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT3DI8_Test.TestBuff));
      TKDT3DI8_Test.Search(TKDT3DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT3DI8Distance(TKDT3DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT3DI8_Test.Clear;
      { kMean test }
      TKDT3DI8_Test.BuildKDTreeWithCluster(TKDT3DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT3DI8_Test.Search(TKDT3DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT3DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT3DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT3DI8_Test);
end;


function TKDT4DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT4DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT4DI8_Node;
  function SortCompare(const p1, p2: PKDT4DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT4DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT4DI8_Source;
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
  kdBuffPtr: PKDT4DI8_SourceBuffer;
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
      axis := Depth mod KDT4DI8_Axis;
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

function TKDT4DI8.GetData(const Index: NativeInt): PKDT4DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT4DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT4DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT4DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT4DI8_Node(KDNodes[i]));
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

function TKDT4DI8.StoreBuffPtr: PKDT4DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT4DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT4DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT4DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT4DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT4DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT4DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT4DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT4DI8.BuildKDTreeWithCluster(const inBuff: TKDT4DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT4DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT4DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT4DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT4DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT4DI8.BuildKDTreeWithCluster(const inBuff: TKDT4DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT4DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DI8_BuildCall);
var
  TempStoreBuff: TKDT4DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT4DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT4DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT4DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT4DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT4DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT4DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DI8_BuildMethod);
var
  TempStoreBuff: TKDT4DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT4DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT4DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT4DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT4DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT4DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT4DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT4DI8_BuildProc);
var
  TempStoreBuff: TKDT4DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT4DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT4DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT4DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT4DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT4DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT4DI8.Search(const buff: TKDT4DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT4DI8_Node;

var
  NearestNeighbour: PKDT4DI8_Node;

  function FindParentNode(const buffPtr: PKDT4DI8_Vec; NodePtr: PKDT4DI8_Node): PKDT4DI8_Node;
  var
    Next: PKDT4DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT4DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT4DI8_Node; const buffPtr: PKDT4DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT4DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT4DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT4DI8_Vec; const p1, p2: PKDT4DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT4DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT4DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT4DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT4DI8_Node;
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
  Parent: PKDT4DI8_Node;
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

  SearchedDistanceMin := KDT4DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT4DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT4DI8.Search(const buff: TKDT4DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT4DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT4DI8.Search(const buff: TKDT4DI8_Vec; var SearchedDistanceMin: Double): PKDT4DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT4DI8.Search(const buff: TKDT4DI8_Vec): PKDT4DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT4DI8.SearchToken(const buff: TKDT4DI8_Vec): TPascalString;
var
  p: PKDT4DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT4DI8.Search(const inBuff: TKDT4DI8_DynamicVecBuffer; var OutBuff: TKDT4DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT4DI8_DynamicVecBuffer;
  outBuffPtr: PKDT4DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT4DI8_Node;
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
        p: PKDT4DI8_Node;
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
  p: PKDT4DI8_Node;
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


procedure TKDT4DI8.Search(const inBuff: TKDT4DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT4DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT4DI8_Node;
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
        p: PKDT4DI8_Node;
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
  p: PKDT4DI8_Node;
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


procedure TKDT4DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT4DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT4DI8_Vec));
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

procedure TKDT4DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT4DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT4DI8_Vec)) <> SizeOf(TKDT4DI8_Vec) then
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

procedure TKDT4DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT4DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT4DI8.PrintNodeTree(const NodePtr: PKDT4DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT4DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT4DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT4DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT4DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT4DI8.KDT4DI8Vec(const s: SystemString): TKDT4DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT4DI8_Axis - 1 do
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
            if j >= KDT4DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT4DI8.KDT4DI8Vec(const v: TKDT4DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT4DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT4DI8.KDT4DI8Pow(const v: TKDT4DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT4DI8.KDT4DI8Distance(const v1, v2: TKDT4DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT4DI8_Axis - 1 do
      Result := Result + KDT4DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT4DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT4DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT4DI8.Test;
var
  TKDT4DI8_Test: TKDT4DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT4DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT4DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT4DI8_Test := TKDT4DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT4DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT4DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT4DI8_Axis - 1 do
        TKDT4DI8_Test.TestBuff[i][j] := i * KDT4DI8_Axis + j;

{$IFDEF FPC}
  TKDT4DI8_Test.BuildKDTreeM(length(TKDT4DI8_Test.TestBuff), nil, @TKDT4DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT4DI8_Test.BuildKDTreeM(length(TKDT4DI8_Test.TestBuff), nil, TKDT4DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT4DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT4DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT4DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT4DI8_Test.Search(TKDT4DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT4DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT4DI8_Test.TestBuff));
      TKDT4DI8_Test.Search(TKDT4DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT4DI8Distance(TKDT4DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT4DI8_Test.Clear;
      { kMean test }
      TKDT4DI8_Test.BuildKDTreeWithCluster(TKDT4DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT4DI8_Test.Search(TKDT4DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT4DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT4DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT4DI8_Test);
end;


function TKDT5DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT5DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT5DI8_Node;
  function SortCompare(const p1, p2: PKDT5DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT5DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT5DI8_Source;
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
  kdBuffPtr: PKDT5DI8_SourceBuffer;
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
      axis := Depth mod KDT5DI8_Axis;
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

function TKDT5DI8.GetData(const Index: NativeInt): PKDT5DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT5DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT5DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT5DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT5DI8_Node(KDNodes[i]));
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

function TKDT5DI8.StoreBuffPtr: PKDT5DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT5DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT5DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT5DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT5DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT5DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT5DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT5DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT5DI8.BuildKDTreeWithCluster(const inBuff: TKDT5DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT5DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT5DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT5DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT5DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT5DI8.BuildKDTreeWithCluster(const inBuff: TKDT5DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT5DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DI8_BuildCall);
var
  TempStoreBuff: TKDT5DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT5DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT5DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT5DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT5DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT5DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT5DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DI8_BuildMethod);
var
  TempStoreBuff: TKDT5DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT5DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT5DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT5DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT5DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT5DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT5DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT5DI8_BuildProc);
var
  TempStoreBuff: TKDT5DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT5DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT5DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT5DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT5DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT5DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT5DI8.Search(const buff: TKDT5DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT5DI8_Node;

var
  NearestNeighbour: PKDT5DI8_Node;

  function FindParentNode(const buffPtr: PKDT5DI8_Vec; NodePtr: PKDT5DI8_Node): PKDT5DI8_Node;
  var
    Next: PKDT5DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT5DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT5DI8_Node; const buffPtr: PKDT5DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT5DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT5DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT5DI8_Vec; const p1, p2: PKDT5DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT5DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT5DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT5DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT5DI8_Node;
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
  Parent: PKDT5DI8_Node;
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

  SearchedDistanceMin := KDT5DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT5DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT5DI8.Search(const buff: TKDT5DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT5DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT5DI8.Search(const buff: TKDT5DI8_Vec; var SearchedDistanceMin: Double): PKDT5DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT5DI8.Search(const buff: TKDT5DI8_Vec): PKDT5DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT5DI8.SearchToken(const buff: TKDT5DI8_Vec): TPascalString;
var
  p: PKDT5DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT5DI8.Search(const inBuff: TKDT5DI8_DynamicVecBuffer; var OutBuff: TKDT5DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT5DI8_DynamicVecBuffer;
  outBuffPtr: PKDT5DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT5DI8_Node;
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
        p: PKDT5DI8_Node;
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
  p: PKDT5DI8_Node;
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


procedure TKDT5DI8.Search(const inBuff: TKDT5DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT5DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT5DI8_Node;
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
        p: PKDT5DI8_Node;
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
  p: PKDT5DI8_Node;
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


procedure TKDT5DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT5DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT5DI8_Vec));
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

procedure TKDT5DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT5DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT5DI8_Vec)) <> SizeOf(TKDT5DI8_Vec) then
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

procedure TKDT5DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT5DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT5DI8.PrintNodeTree(const NodePtr: PKDT5DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT5DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT5DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT5DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT5DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT5DI8.KDT5DI8Vec(const s: SystemString): TKDT5DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT5DI8_Axis - 1 do
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
            if j >= KDT5DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT5DI8.KDT5DI8Vec(const v: TKDT5DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT5DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT5DI8.KDT5DI8Pow(const v: TKDT5DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT5DI8.KDT5DI8Distance(const v1, v2: TKDT5DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT5DI8_Axis - 1 do
      Result := Result + KDT5DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT5DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT5DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT5DI8.Test;
var
  TKDT5DI8_Test: TKDT5DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT5DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT5DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT5DI8_Test := TKDT5DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT5DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT5DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT5DI8_Axis - 1 do
        TKDT5DI8_Test.TestBuff[i][j] := i * KDT5DI8_Axis + j;

{$IFDEF FPC}
  TKDT5DI8_Test.BuildKDTreeM(length(TKDT5DI8_Test.TestBuff), nil, @TKDT5DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT5DI8_Test.BuildKDTreeM(length(TKDT5DI8_Test.TestBuff), nil, TKDT5DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT5DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT5DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT5DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT5DI8_Test.Search(TKDT5DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT5DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT5DI8_Test.TestBuff));
      TKDT5DI8_Test.Search(TKDT5DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT5DI8Distance(TKDT5DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT5DI8_Test.Clear;
      { kMean test }
      TKDT5DI8_Test.BuildKDTreeWithCluster(TKDT5DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT5DI8_Test.Search(TKDT5DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT5DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT5DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT5DI8_Test);
end;


function TKDT6DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT6DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT6DI8_Node;
  function SortCompare(const p1, p2: PKDT6DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT6DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT6DI8_Source;
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
  kdBuffPtr: PKDT6DI8_SourceBuffer;
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
      axis := Depth mod KDT6DI8_Axis;
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

function TKDT6DI8.GetData(const Index: NativeInt): PKDT6DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT6DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT6DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT6DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT6DI8_Node(KDNodes[i]));
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

function TKDT6DI8.StoreBuffPtr: PKDT6DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT6DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT6DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT6DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT6DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT6DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT6DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT6DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT6DI8.BuildKDTreeWithCluster(const inBuff: TKDT6DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT6DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT6DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT6DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT6DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT6DI8.BuildKDTreeWithCluster(const inBuff: TKDT6DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT6DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DI8_BuildCall);
var
  TempStoreBuff: TKDT6DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT6DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT6DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT6DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT6DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT6DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT6DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DI8_BuildMethod);
var
  TempStoreBuff: TKDT6DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT6DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT6DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT6DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT6DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT6DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT6DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT6DI8_BuildProc);
var
  TempStoreBuff: TKDT6DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT6DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT6DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT6DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT6DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT6DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT6DI8.Search(const buff: TKDT6DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT6DI8_Node;

var
  NearestNeighbour: PKDT6DI8_Node;

  function FindParentNode(const buffPtr: PKDT6DI8_Vec; NodePtr: PKDT6DI8_Node): PKDT6DI8_Node;
  var
    Next: PKDT6DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT6DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT6DI8_Node; const buffPtr: PKDT6DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT6DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT6DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT6DI8_Vec; const p1, p2: PKDT6DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT6DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT6DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT6DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT6DI8_Node;
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
  Parent: PKDT6DI8_Node;
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

  SearchedDistanceMin := KDT6DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT6DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT6DI8.Search(const buff: TKDT6DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT6DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT6DI8.Search(const buff: TKDT6DI8_Vec; var SearchedDistanceMin: Double): PKDT6DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT6DI8.Search(const buff: TKDT6DI8_Vec): PKDT6DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT6DI8.SearchToken(const buff: TKDT6DI8_Vec): TPascalString;
var
  p: PKDT6DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT6DI8.Search(const inBuff: TKDT6DI8_DynamicVecBuffer; var OutBuff: TKDT6DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT6DI8_DynamicVecBuffer;
  outBuffPtr: PKDT6DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT6DI8_Node;
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
        p: PKDT6DI8_Node;
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
  p: PKDT6DI8_Node;
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


procedure TKDT6DI8.Search(const inBuff: TKDT6DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT6DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT6DI8_Node;
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
        p: PKDT6DI8_Node;
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
  p: PKDT6DI8_Node;
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


procedure TKDT6DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT6DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT6DI8_Vec));
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

procedure TKDT6DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT6DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT6DI8_Vec)) <> SizeOf(TKDT6DI8_Vec) then
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

procedure TKDT6DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT6DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT6DI8.PrintNodeTree(const NodePtr: PKDT6DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT6DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT6DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT6DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT6DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT6DI8.KDT6DI8Vec(const s: SystemString): TKDT6DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT6DI8_Axis - 1 do
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
            if j >= KDT6DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT6DI8.KDT6DI8Vec(const v: TKDT6DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT6DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT6DI8.KDT6DI8Pow(const v: TKDT6DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT6DI8.KDT6DI8Distance(const v1, v2: TKDT6DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT6DI8_Axis - 1 do
      Result := Result + KDT6DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT6DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT6DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT6DI8.Test;
var
  TKDT6DI8_Test: TKDT6DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT6DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT6DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT6DI8_Test := TKDT6DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT6DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT6DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT6DI8_Axis - 1 do
        TKDT6DI8_Test.TestBuff[i][j] := i * KDT6DI8_Axis + j;

{$IFDEF FPC}
  TKDT6DI8_Test.BuildKDTreeM(length(TKDT6DI8_Test.TestBuff), nil, @TKDT6DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT6DI8_Test.BuildKDTreeM(length(TKDT6DI8_Test.TestBuff), nil, TKDT6DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT6DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT6DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT6DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT6DI8_Test.Search(TKDT6DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT6DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT6DI8_Test.TestBuff));
      TKDT6DI8_Test.Search(TKDT6DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT6DI8Distance(TKDT6DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT6DI8_Test.Clear;
      { kMean test }
      TKDT6DI8_Test.BuildKDTreeWithCluster(TKDT6DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT6DI8_Test.Search(TKDT6DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT6DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT6DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT6DI8_Test);
end;


function TKDT7DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT7DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT7DI8_Node;
  function SortCompare(const p1, p2: PKDT7DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT7DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT7DI8_Source;
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
  kdBuffPtr: PKDT7DI8_SourceBuffer;
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
      axis := Depth mod KDT7DI8_Axis;
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

function TKDT7DI8.GetData(const Index: NativeInt): PKDT7DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT7DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT7DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT7DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT7DI8_Node(KDNodes[i]));
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

function TKDT7DI8.StoreBuffPtr: PKDT7DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT7DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT7DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT7DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT7DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT7DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT7DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT7DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT7DI8.BuildKDTreeWithCluster(const inBuff: TKDT7DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT7DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT7DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT7DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT7DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT7DI8.BuildKDTreeWithCluster(const inBuff: TKDT7DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT7DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DI8_BuildCall);
var
  TempStoreBuff: TKDT7DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT7DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT7DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT7DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT7DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT7DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT7DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DI8_BuildMethod);
var
  TempStoreBuff: TKDT7DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT7DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT7DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT7DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT7DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT7DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT7DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT7DI8_BuildProc);
var
  TempStoreBuff: TKDT7DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT7DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT7DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT7DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT7DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT7DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT7DI8.Search(const buff: TKDT7DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT7DI8_Node;

var
  NearestNeighbour: PKDT7DI8_Node;

  function FindParentNode(const buffPtr: PKDT7DI8_Vec; NodePtr: PKDT7DI8_Node): PKDT7DI8_Node;
  var
    Next: PKDT7DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT7DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT7DI8_Node; const buffPtr: PKDT7DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT7DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT7DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT7DI8_Vec; const p1, p2: PKDT7DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT7DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT7DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT7DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT7DI8_Node;
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
  Parent: PKDT7DI8_Node;
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

  SearchedDistanceMin := KDT7DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT7DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT7DI8.Search(const buff: TKDT7DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT7DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT7DI8.Search(const buff: TKDT7DI8_Vec; var SearchedDistanceMin: Double): PKDT7DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT7DI8.Search(const buff: TKDT7DI8_Vec): PKDT7DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT7DI8.SearchToken(const buff: TKDT7DI8_Vec): TPascalString;
var
  p: PKDT7DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT7DI8.Search(const inBuff: TKDT7DI8_DynamicVecBuffer; var OutBuff: TKDT7DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT7DI8_DynamicVecBuffer;
  outBuffPtr: PKDT7DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT7DI8_Node;
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
        p: PKDT7DI8_Node;
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
  p: PKDT7DI8_Node;
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


procedure TKDT7DI8.Search(const inBuff: TKDT7DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT7DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT7DI8_Node;
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
        p: PKDT7DI8_Node;
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
  p: PKDT7DI8_Node;
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


procedure TKDT7DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT7DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT7DI8_Vec));
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

procedure TKDT7DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT7DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT7DI8_Vec)) <> SizeOf(TKDT7DI8_Vec) then
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

procedure TKDT7DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT7DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT7DI8.PrintNodeTree(const NodePtr: PKDT7DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT7DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT7DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT7DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT7DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT7DI8.KDT7DI8Vec(const s: SystemString): TKDT7DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT7DI8_Axis - 1 do
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
            if j >= KDT7DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT7DI8.KDT7DI8Vec(const v: TKDT7DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT7DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT7DI8.KDT7DI8Pow(const v: TKDT7DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT7DI8.KDT7DI8Distance(const v1, v2: TKDT7DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT7DI8_Axis - 1 do
      Result := Result + KDT7DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT7DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT7DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT7DI8.Test;
var
  TKDT7DI8_Test: TKDT7DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT7DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT7DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT7DI8_Test := TKDT7DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT7DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT7DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT7DI8_Axis - 1 do
        TKDT7DI8_Test.TestBuff[i][j] := i * KDT7DI8_Axis + j;

{$IFDEF FPC}
  TKDT7DI8_Test.BuildKDTreeM(length(TKDT7DI8_Test.TestBuff), nil, @TKDT7DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT7DI8_Test.BuildKDTreeM(length(TKDT7DI8_Test.TestBuff), nil, TKDT7DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT7DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT7DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT7DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT7DI8_Test.Search(TKDT7DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT7DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT7DI8_Test.TestBuff));
      TKDT7DI8_Test.Search(TKDT7DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT7DI8Distance(TKDT7DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT7DI8_Test.Clear;
      { kMean test }
      TKDT7DI8_Test.BuildKDTreeWithCluster(TKDT7DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT7DI8_Test.Search(TKDT7DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT7DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT7DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT7DI8_Test);
end;


function TKDT8DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT8DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT8DI8_Node;
  function SortCompare(const p1, p2: PKDT8DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT8DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT8DI8_Source;
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
  kdBuffPtr: PKDT8DI8_SourceBuffer;
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
      axis := Depth mod KDT8DI8_Axis;
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

function TKDT8DI8.GetData(const Index: NativeInt): PKDT8DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT8DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT8DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT8DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT8DI8_Node(KDNodes[i]));
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

function TKDT8DI8.StoreBuffPtr: PKDT8DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT8DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT8DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT8DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT8DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT8DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT8DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT8DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT8DI8.BuildKDTreeWithCluster(const inBuff: TKDT8DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT8DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT8DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT8DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT8DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT8DI8.BuildKDTreeWithCluster(const inBuff: TKDT8DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT8DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DI8_BuildCall);
var
  TempStoreBuff: TKDT8DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT8DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT8DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT8DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT8DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT8DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT8DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DI8_BuildMethod);
var
  TempStoreBuff: TKDT8DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT8DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT8DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT8DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT8DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT8DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT8DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT8DI8_BuildProc);
var
  TempStoreBuff: TKDT8DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT8DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT8DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT8DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT8DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT8DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT8DI8.Search(const buff: TKDT8DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT8DI8_Node;

var
  NearestNeighbour: PKDT8DI8_Node;

  function FindParentNode(const buffPtr: PKDT8DI8_Vec; NodePtr: PKDT8DI8_Node): PKDT8DI8_Node;
  var
    Next: PKDT8DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT8DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT8DI8_Node; const buffPtr: PKDT8DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT8DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT8DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT8DI8_Vec; const p1, p2: PKDT8DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT8DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT8DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT8DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT8DI8_Node;
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
  Parent: PKDT8DI8_Node;
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

  SearchedDistanceMin := KDT8DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT8DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT8DI8.Search(const buff: TKDT8DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT8DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT8DI8.Search(const buff: TKDT8DI8_Vec; var SearchedDistanceMin: Double): PKDT8DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT8DI8.Search(const buff: TKDT8DI8_Vec): PKDT8DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT8DI8.SearchToken(const buff: TKDT8DI8_Vec): TPascalString;
var
  p: PKDT8DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT8DI8.Search(const inBuff: TKDT8DI8_DynamicVecBuffer; var OutBuff: TKDT8DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT8DI8_DynamicVecBuffer;
  outBuffPtr: PKDT8DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT8DI8_Node;
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
        p: PKDT8DI8_Node;
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
  p: PKDT8DI8_Node;
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


procedure TKDT8DI8.Search(const inBuff: TKDT8DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT8DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT8DI8_Node;
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
        p: PKDT8DI8_Node;
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
  p: PKDT8DI8_Node;
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


procedure TKDT8DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT8DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT8DI8_Vec));
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

procedure TKDT8DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT8DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT8DI8_Vec)) <> SizeOf(TKDT8DI8_Vec) then
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

procedure TKDT8DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT8DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT8DI8.PrintNodeTree(const NodePtr: PKDT8DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT8DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT8DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT8DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT8DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT8DI8.KDT8DI8Vec(const s: SystemString): TKDT8DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT8DI8_Axis - 1 do
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
            if j >= KDT8DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT8DI8.KDT8DI8Vec(const v: TKDT8DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT8DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT8DI8.KDT8DI8Pow(const v: TKDT8DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT8DI8.KDT8DI8Distance(const v1, v2: TKDT8DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT8DI8_Axis - 1 do
      Result := Result + KDT8DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT8DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT8DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT8DI8.Test;
var
  TKDT8DI8_Test: TKDT8DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT8DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT8DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT8DI8_Test := TKDT8DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT8DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT8DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT8DI8_Axis - 1 do
        TKDT8DI8_Test.TestBuff[i][j] := i * KDT8DI8_Axis + j;

{$IFDEF FPC}
  TKDT8DI8_Test.BuildKDTreeM(length(TKDT8DI8_Test.TestBuff), nil, @TKDT8DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT8DI8_Test.BuildKDTreeM(length(TKDT8DI8_Test.TestBuff), nil, TKDT8DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT8DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT8DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT8DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT8DI8_Test.Search(TKDT8DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT8DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT8DI8_Test.TestBuff));
      TKDT8DI8_Test.Search(TKDT8DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT8DI8Distance(TKDT8DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT8DI8_Test.Clear;
      { kMean test }
      TKDT8DI8_Test.BuildKDTreeWithCluster(TKDT8DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT8DI8_Test.Search(TKDT8DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT8DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT8DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT8DI8_Test);
end;


function TKDT9DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT9DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT9DI8_Node;
  function SortCompare(const p1, p2: PKDT9DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT9DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT9DI8_Source;
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
  kdBuffPtr: PKDT9DI8_SourceBuffer;
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
      axis := Depth mod KDT9DI8_Axis;
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

function TKDT9DI8.GetData(const Index: NativeInt): PKDT9DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT9DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT9DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT9DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT9DI8_Node(KDNodes[i]));
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

function TKDT9DI8.StoreBuffPtr: PKDT9DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT9DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT9DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT9DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT9DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT9DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT9DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT9DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT9DI8.BuildKDTreeWithCluster(const inBuff: TKDT9DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT9DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT9DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT9DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT9DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT9DI8.BuildKDTreeWithCluster(const inBuff: TKDT9DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT9DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DI8_BuildCall);
var
  TempStoreBuff: TKDT9DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT9DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT9DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT9DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT9DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT9DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT9DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DI8_BuildMethod);
var
  TempStoreBuff: TKDT9DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT9DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT9DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT9DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT9DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT9DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT9DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT9DI8_BuildProc);
var
  TempStoreBuff: TKDT9DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT9DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT9DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT9DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT9DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT9DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT9DI8.Search(const buff: TKDT9DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT9DI8_Node;

var
  NearestNeighbour: PKDT9DI8_Node;

  function FindParentNode(const buffPtr: PKDT9DI8_Vec; NodePtr: PKDT9DI8_Node): PKDT9DI8_Node;
  var
    Next: PKDT9DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT9DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT9DI8_Node; const buffPtr: PKDT9DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT9DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT9DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT9DI8_Vec; const p1, p2: PKDT9DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT9DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT9DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT9DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT9DI8_Node;
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
  Parent: PKDT9DI8_Node;
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

  SearchedDistanceMin := KDT9DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT9DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT9DI8.Search(const buff: TKDT9DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT9DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT9DI8.Search(const buff: TKDT9DI8_Vec; var SearchedDistanceMin: Double): PKDT9DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT9DI8.Search(const buff: TKDT9DI8_Vec): PKDT9DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT9DI8.SearchToken(const buff: TKDT9DI8_Vec): TPascalString;
var
  p: PKDT9DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT9DI8.Search(const inBuff: TKDT9DI8_DynamicVecBuffer; var OutBuff: TKDT9DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT9DI8_DynamicVecBuffer;
  outBuffPtr: PKDT9DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT9DI8_Node;
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
        p: PKDT9DI8_Node;
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
  p: PKDT9DI8_Node;
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


procedure TKDT9DI8.Search(const inBuff: TKDT9DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT9DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT9DI8_Node;
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
        p: PKDT9DI8_Node;
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
  p: PKDT9DI8_Node;
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


procedure TKDT9DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT9DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT9DI8_Vec));
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

procedure TKDT9DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT9DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT9DI8_Vec)) <> SizeOf(TKDT9DI8_Vec) then
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

procedure TKDT9DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT9DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT9DI8.PrintNodeTree(const NodePtr: PKDT9DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT9DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT9DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT9DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT9DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT9DI8.KDT9DI8Vec(const s: SystemString): TKDT9DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT9DI8_Axis - 1 do
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
            if j >= KDT9DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT9DI8.KDT9DI8Vec(const v: TKDT9DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT9DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT9DI8.KDT9DI8Pow(const v: TKDT9DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT9DI8.KDT9DI8Distance(const v1, v2: TKDT9DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT9DI8_Axis - 1 do
      Result := Result + KDT9DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT9DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT9DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT9DI8.Test;
var
  TKDT9DI8_Test: TKDT9DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT9DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT9DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT9DI8_Test := TKDT9DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT9DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT9DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT9DI8_Axis - 1 do
        TKDT9DI8_Test.TestBuff[i][j] := i * KDT9DI8_Axis + j;

{$IFDEF FPC}
  TKDT9DI8_Test.BuildKDTreeM(length(TKDT9DI8_Test.TestBuff), nil, @TKDT9DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT9DI8_Test.BuildKDTreeM(length(TKDT9DI8_Test.TestBuff), nil, TKDT9DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT9DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT9DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT9DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT9DI8_Test.Search(TKDT9DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT9DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT9DI8_Test.TestBuff));
      TKDT9DI8_Test.Search(TKDT9DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT9DI8Distance(TKDT9DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT9DI8_Test.Clear;
      { kMean test }
      TKDT9DI8_Test.BuildKDTreeWithCluster(TKDT9DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT9DI8_Test.Search(TKDT9DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT9DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT9DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT9DI8_Test);
end;


function TKDT10DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT10DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT10DI8_Node;
  function SortCompare(const p1, p2: PKDT10DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT10DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT10DI8_Source;
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
  kdBuffPtr: PKDT10DI8_SourceBuffer;
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
      axis := Depth mod KDT10DI8_Axis;
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

function TKDT10DI8.GetData(const Index: NativeInt): PKDT10DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT10DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT10DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT10DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT10DI8_Node(KDNodes[i]));
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

function TKDT10DI8.StoreBuffPtr: PKDT10DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT10DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT10DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT10DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT10DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT10DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT10DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT10DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT10DI8.BuildKDTreeWithCluster(const inBuff: TKDT10DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT10DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT10DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT10DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT10DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT10DI8.BuildKDTreeWithCluster(const inBuff: TKDT10DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT10DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DI8_BuildCall);
var
  TempStoreBuff: TKDT10DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT10DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT10DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT10DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT10DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT10DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT10DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DI8_BuildMethod);
var
  TempStoreBuff: TKDT10DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT10DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT10DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT10DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT10DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT10DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT10DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT10DI8_BuildProc);
var
  TempStoreBuff: TKDT10DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT10DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT10DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT10DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT10DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT10DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT10DI8.Search(const buff: TKDT10DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT10DI8_Node;

var
  NearestNeighbour: PKDT10DI8_Node;

  function FindParentNode(const buffPtr: PKDT10DI8_Vec; NodePtr: PKDT10DI8_Node): PKDT10DI8_Node;
  var
    Next: PKDT10DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT10DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT10DI8_Node; const buffPtr: PKDT10DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT10DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT10DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT10DI8_Vec; const p1, p2: PKDT10DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT10DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT10DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT10DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT10DI8_Node;
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
  Parent: PKDT10DI8_Node;
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

  SearchedDistanceMin := KDT10DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT10DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT10DI8.Search(const buff: TKDT10DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT10DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT10DI8.Search(const buff: TKDT10DI8_Vec; var SearchedDistanceMin: Double): PKDT10DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT10DI8.Search(const buff: TKDT10DI8_Vec): PKDT10DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT10DI8.SearchToken(const buff: TKDT10DI8_Vec): TPascalString;
var
  p: PKDT10DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT10DI8.Search(const inBuff: TKDT10DI8_DynamicVecBuffer; var OutBuff: TKDT10DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT10DI8_DynamicVecBuffer;
  outBuffPtr: PKDT10DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT10DI8_Node;
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
        p: PKDT10DI8_Node;
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
  p: PKDT10DI8_Node;
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


procedure TKDT10DI8.Search(const inBuff: TKDT10DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT10DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT10DI8_Node;
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
        p: PKDT10DI8_Node;
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
  p: PKDT10DI8_Node;
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


procedure TKDT10DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT10DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT10DI8_Vec));
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

procedure TKDT10DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT10DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT10DI8_Vec)) <> SizeOf(TKDT10DI8_Vec) then
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

procedure TKDT10DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT10DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT10DI8.PrintNodeTree(const NodePtr: PKDT10DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT10DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT10DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT10DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT10DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT10DI8.KDT10DI8Vec(const s: SystemString): TKDT10DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT10DI8_Axis - 1 do
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
            if j >= KDT10DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT10DI8.KDT10DI8Vec(const v: TKDT10DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT10DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT10DI8.KDT10DI8Pow(const v: TKDT10DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT10DI8.KDT10DI8Distance(const v1, v2: TKDT10DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT10DI8_Axis - 1 do
      Result := Result + KDT10DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT10DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT10DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT10DI8.Test;
var
  TKDT10DI8_Test: TKDT10DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT10DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT10DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT10DI8_Test := TKDT10DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT10DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT10DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT10DI8_Axis - 1 do
        TKDT10DI8_Test.TestBuff[i][j] := i * KDT10DI8_Axis + j;

{$IFDEF FPC}
  TKDT10DI8_Test.BuildKDTreeM(length(TKDT10DI8_Test.TestBuff), nil, @TKDT10DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT10DI8_Test.BuildKDTreeM(length(TKDT10DI8_Test.TestBuff), nil, TKDT10DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT10DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT10DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT10DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT10DI8_Test.Search(TKDT10DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT10DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT10DI8_Test.TestBuff));
      TKDT10DI8_Test.Search(TKDT10DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT10DI8Distance(TKDT10DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT10DI8_Test.Clear;
      { kMean test }
      TKDT10DI8_Test.BuildKDTreeWithCluster(TKDT10DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT10DI8_Test.Search(TKDT10DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT10DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT10DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT10DI8_Test);
end;


function TKDT11DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT11DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT11DI8_Node;
  function SortCompare(const p1, p2: PKDT11DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT11DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT11DI8_Source;
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
  kdBuffPtr: PKDT11DI8_SourceBuffer;
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
      axis := Depth mod KDT11DI8_Axis;
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

function TKDT11DI8.GetData(const Index: NativeInt): PKDT11DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT11DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT11DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT11DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT11DI8_Node(KDNodes[i]));
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

function TKDT11DI8.StoreBuffPtr: PKDT11DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT11DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT11DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT11DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT11DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT11DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT11DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT11DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT11DI8.BuildKDTreeWithCluster(const inBuff: TKDT11DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT11DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT11DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT11DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT11DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT11DI8.BuildKDTreeWithCluster(const inBuff: TKDT11DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT11DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DI8_BuildCall);
var
  TempStoreBuff: TKDT11DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT11DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT11DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT11DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT11DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT11DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT11DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DI8_BuildMethod);
var
  TempStoreBuff: TKDT11DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT11DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT11DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT11DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT11DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT11DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT11DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT11DI8_BuildProc);
var
  TempStoreBuff: TKDT11DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT11DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT11DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT11DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT11DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT11DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT11DI8.Search(const buff: TKDT11DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT11DI8_Node;

var
  NearestNeighbour: PKDT11DI8_Node;

  function FindParentNode(const buffPtr: PKDT11DI8_Vec; NodePtr: PKDT11DI8_Node): PKDT11DI8_Node;
  var
    Next: PKDT11DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT11DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT11DI8_Node; const buffPtr: PKDT11DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT11DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT11DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT11DI8_Vec; const p1, p2: PKDT11DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT11DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT11DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT11DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT11DI8_Node;
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
  Parent: PKDT11DI8_Node;
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

  SearchedDistanceMin := KDT11DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT11DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT11DI8.Search(const buff: TKDT11DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT11DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT11DI8.Search(const buff: TKDT11DI8_Vec; var SearchedDistanceMin: Double): PKDT11DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT11DI8.Search(const buff: TKDT11DI8_Vec): PKDT11DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT11DI8.SearchToken(const buff: TKDT11DI8_Vec): TPascalString;
var
  p: PKDT11DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT11DI8.Search(const inBuff: TKDT11DI8_DynamicVecBuffer; var OutBuff: TKDT11DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT11DI8_DynamicVecBuffer;
  outBuffPtr: PKDT11DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT11DI8_Node;
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
        p: PKDT11DI8_Node;
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
  p: PKDT11DI8_Node;
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


procedure TKDT11DI8.Search(const inBuff: TKDT11DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT11DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT11DI8_Node;
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
        p: PKDT11DI8_Node;
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
  p: PKDT11DI8_Node;
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


procedure TKDT11DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT11DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT11DI8_Vec));
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

procedure TKDT11DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT11DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT11DI8_Vec)) <> SizeOf(TKDT11DI8_Vec) then
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

procedure TKDT11DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT11DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT11DI8.PrintNodeTree(const NodePtr: PKDT11DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT11DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT11DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT11DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT11DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT11DI8.KDT11DI8Vec(const s: SystemString): TKDT11DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT11DI8_Axis - 1 do
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
            if j >= KDT11DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT11DI8.KDT11DI8Vec(const v: TKDT11DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT11DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT11DI8.KDT11DI8Pow(const v: TKDT11DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT11DI8.KDT11DI8Distance(const v1, v2: TKDT11DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT11DI8_Axis - 1 do
      Result := Result + KDT11DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT11DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT11DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT11DI8.Test;
var
  TKDT11DI8_Test: TKDT11DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT11DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT11DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT11DI8_Test := TKDT11DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT11DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT11DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT11DI8_Axis - 1 do
        TKDT11DI8_Test.TestBuff[i][j] := i * KDT11DI8_Axis + j;

{$IFDEF FPC}
  TKDT11DI8_Test.BuildKDTreeM(length(TKDT11DI8_Test.TestBuff), nil, @TKDT11DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT11DI8_Test.BuildKDTreeM(length(TKDT11DI8_Test.TestBuff), nil, TKDT11DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT11DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT11DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT11DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT11DI8_Test.Search(TKDT11DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT11DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT11DI8_Test.TestBuff));
      TKDT11DI8_Test.Search(TKDT11DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT11DI8Distance(TKDT11DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT11DI8_Test.Clear;
      { kMean test }
      TKDT11DI8_Test.BuildKDTreeWithCluster(TKDT11DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT11DI8_Test.Search(TKDT11DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT11DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT11DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT11DI8_Test);
end;


function TKDT12DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT12DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT12DI8_Node;
  function SortCompare(const p1, p2: PKDT12DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT12DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT12DI8_Source;
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
  kdBuffPtr: PKDT12DI8_SourceBuffer;
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
      axis := Depth mod KDT12DI8_Axis;
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

function TKDT12DI8.GetData(const Index: NativeInt): PKDT12DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT12DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT12DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT12DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT12DI8_Node(KDNodes[i]));
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

function TKDT12DI8.StoreBuffPtr: PKDT12DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT12DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT12DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT12DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT12DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT12DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT12DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT12DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT12DI8.BuildKDTreeWithCluster(const inBuff: TKDT12DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT12DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT12DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT12DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT12DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT12DI8.BuildKDTreeWithCluster(const inBuff: TKDT12DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT12DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DI8_BuildCall);
var
  TempStoreBuff: TKDT12DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT12DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT12DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT12DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT12DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT12DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT12DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DI8_BuildMethod);
var
  TempStoreBuff: TKDT12DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT12DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT12DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT12DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT12DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT12DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT12DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT12DI8_BuildProc);
var
  TempStoreBuff: TKDT12DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT12DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT12DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT12DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT12DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT12DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT12DI8.Search(const buff: TKDT12DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT12DI8_Node;

var
  NearestNeighbour: PKDT12DI8_Node;

  function FindParentNode(const buffPtr: PKDT12DI8_Vec; NodePtr: PKDT12DI8_Node): PKDT12DI8_Node;
  var
    Next: PKDT12DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT12DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT12DI8_Node; const buffPtr: PKDT12DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT12DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT12DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT12DI8_Vec; const p1, p2: PKDT12DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT12DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT12DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT12DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT12DI8_Node;
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
  Parent: PKDT12DI8_Node;
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

  SearchedDistanceMin := KDT12DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT12DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT12DI8.Search(const buff: TKDT12DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT12DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT12DI8.Search(const buff: TKDT12DI8_Vec; var SearchedDistanceMin: Double): PKDT12DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT12DI8.Search(const buff: TKDT12DI8_Vec): PKDT12DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT12DI8.SearchToken(const buff: TKDT12DI8_Vec): TPascalString;
var
  p: PKDT12DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT12DI8.Search(const inBuff: TKDT12DI8_DynamicVecBuffer; var OutBuff: TKDT12DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT12DI8_DynamicVecBuffer;
  outBuffPtr: PKDT12DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT12DI8_Node;
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
        p: PKDT12DI8_Node;
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
  p: PKDT12DI8_Node;
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


procedure TKDT12DI8.Search(const inBuff: TKDT12DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT12DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT12DI8_Node;
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
        p: PKDT12DI8_Node;
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
  p: PKDT12DI8_Node;
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


procedure TKDT12DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT12DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT12DI8_Vec));
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

procedure TKDT12DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT12DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT12DI8_Vec)) <> SizeOf(TKDT12DI8_Vec) then
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

procedure TKDT12DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT12DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT12DI8.PrintNodeTree(const NodePtr: PKDT12DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT12DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT12DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT12DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT12DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT12DI8.KDT12DI8Vec(const s: SystemString): TKDT12DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT12DI8_Axis - 1 do
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
            if j >= KDT12DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT12DI8.KDT12DI8Vec(const v: TKDT12DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT12DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT12DI8.KDT12DI8Pow(const v: TKDT12DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT12DI8.KDT12DI8Distance(const v1, v2: TKDT12DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT12DI8_Axis - 1 do
      Result := Result + KDT12DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT12DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT12DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT12DI8.Test;
var
  TKDT12DI8_Test: TKDT12DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT12DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT12DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT12DI8_Test := TKDT12DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT12DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT12DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT12DI8_Axis - 1 do
        TKDT12DI8_Test.TestBuff[i][j] := i * KDT12DI8_Axis + j;

{$IFDEF FPC}
  TKDT12DI8_Test.BuildKDTreeM(length(TKDT12DI8_Test.TestBuff), nil, @TKDT12DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT12DI8_Test.BuildKDTreeM(length(TKDT12DI8_Test.TestBuff), nil, TKDT12DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT12DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT12DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT12DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT12DI8_Test.Search(TKDT12DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT12DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT12DI8_Test.TestBuff));
      TKDT12DI8_Test.Search(TKDT12DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT12DI8Distance(TKDT12DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT12DI8_Test.Clear;
      { kMean test }
      TKDT12DI8_Test.BuildKDTreeWithCluster(TKDT12DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT12DI8_Test.Search(TKDT12DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT12DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT12DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT12DI8_Test);
end;


function TKDT13DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT13DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT13DI8_Node;
  function SortCompare(const p1, p2: PKDT13DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT13DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT13DI8_Source;
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
  kdBuffPtr: PKDT13DI8_SourceBuffer;
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
      axis := Depth mod KDT13DI8_Axis;
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

function TKDT13DI8.GetData(const Index: NativeInt): PKDT13DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT13DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT13DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT13DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT13DI8_Node(KDNodes[i]));
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

function TKDT13DI8.StoreBuffPtr: PKDT13DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT13DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT13DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT13DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT13DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT13DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT13DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT13DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT13DI8.BuildKDTreeWithCluster(const inBuff: TKDT13DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT13DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT13DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT13DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT13DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT13DI8.BuildKDTreeWithCluster(const inBuff: TKDT13DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT13DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DI8_BuildCall);
var
  TempStoreBuff: TKDT13DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT13DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT13DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT13DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT13DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT13DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT13DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DI8_BuildMethod);
var
  TempStoreBuff: TKDT13DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT13DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT13DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT13DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT13DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT13DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT13DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT13DI8_BuildProc);
var
  TempStoreBuff: TKDT13DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT13DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT13DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT13DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT13DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT13DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT13DI8.Search(const buff: TKDT13DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT13DI8_Node;

var
  NearestNeighbour: PKDT13DI8_Node;

  function FindParentNode(const buffPtr: PKDT13DI8_Vec; NodePtr: PKDT13DI8_Node): PKDT13DI8_Node;
  var
    Next: PKDT13DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT13DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT13DI8_Node; const buffPtr: PKDT13DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT13DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT13DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT13DI8_Vec; const p1, p2: PKDT13DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT13DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT13DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT13DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT13DI8_Node;
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
  Parent: PKDT13DI8_Node;
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

  SearchedDistanceMin := KDT13DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT13DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT13DI8.Search(const buff: TKDT13DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT13DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT13DI8.Search(const buff: TKDT13DI8_Vec; var SearchedDistanceMin: Double): PKDT13DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT13DI8.Search(const buff: TKDT13DI8_Vec): PKDT13DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT13DI8.SearchToken(const buff: TKDT13DI8_Vec): TPascalString;
var
  p: PKDT13DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT13DI8.Search(const inBuff: TKDT13DI8_DynamicVecBuffer; var OutBuff: TKDT13DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT13DI8_DynamicVecBuffer;
  outBuffPtr: PKDT13DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT13DI8_Node;
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
        p: PKDT13DI8_Node;
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
  p: PKDT13DI8_Node;
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


procedure TKDT13DI8.Search(const inBuff: TKDT13DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT13DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT13DI8_Node;
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
        p: PKDT13DI8_Node;
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
  p: PKDT13DI8_Node;
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


procedure TKDT13DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT13DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT13DI8_Vec));
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

procedure TKDT13DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT13DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT13DI8_Vec)) <> SizeOf(TKDT13DI8_Vec) then
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

procedure TKDT13DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT13DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT13DI8.PrintNodeTree(const NodePtr: PKDT13DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT13DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT13DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT13DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT13DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT13DI8.KDT13DI8Vec(const s: SystemString): TKDT13DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT13DI8_Axis - 1 do
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
            if j >= KDT13DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT13DI8.KDT13DI8Vec(const v: TKDT13DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT13DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT13DI8.KDT13DI8Pow(const v: TKDT13DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT13DI8.KDT13DI8Distance(const v1, v2: TKDT13DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT13DI8_Axis - 1 do
      Result := Result + KDT13DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT13DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT13DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT13DI8.Test;
var
  TKDT13DI8_Test: TKDT13DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT13DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT13DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT13DI8_Test := TKDT13DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT13DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT13DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT13DI8_Axis - 1 do
        TKDT13DI8_Test.TestBuff[i][j] := i * KDT13DI8_Axis + j;

{$IFDEF FPC}
  TKDT13DI8_Test.BuildKDTreeM(length(TKDT13DI8_Test.TestBuff), nil, @TKDT13DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT13DI8_Test.BuildKDTreeM(length(TKDT13DI8_Test.TestBuff), nil, TKDT13DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT13DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT13DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT13DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT13DI8_Test.Search(TKDT13DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT13DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT13DI8_Test.TestBuff));
      TKDT13DI8_Test.Search(TKDT13DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT13DI8Distance(TKDT13DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT13DI8_Test.Clear;
      { kMean test }
      TKDT13DI8_Test.BuildKDTreeWithCluster(TKDT13DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT13DI8_Test.Search(TKDT13DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT13DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT13DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT13DI8_Test);
end;


function TKDT14DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT14DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT14DI8_Node;
  function SortCompare(const p1, p2: PKDT14DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT14DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT14DI8_Source;
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
  kdBuffPtr: PKDT14DI8_SourceBuffer;
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
      axis := Depth mod KDT14DI8_Axis;
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

function TKDT14DI8.GetData(const Index: NativeInt): PKDT14DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT14DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT14DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT14DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT14DI8_Node(KDNodes[i]));
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

function TKDT14DI8.StoreBuffPtr: PKDT14DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT14DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT14DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT14DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT14DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT14DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT14DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT14DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT14DI8.BuildKDTreeWithCluster(const inBuff: TKDT14DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT14DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT14DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT14DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT14DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT14DI8.BuildKDTreeWithCluster(const inBuff: TKDT14DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT14DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DI8_BuildCall);
var
  TempStoreBuff: TKDT14DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT14DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT14DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT14DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT14DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT14DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT14DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DI8_BuildMethod);
var
  TempStoreBuff: TKDT14DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT14DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT14DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT14DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT14DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT14DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT14DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT14DI8_BuildProc);
var
  TempStoreBuff: TKDT14DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT14DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT14DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT14DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT14DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT14DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT14DI8.Search(const buff: TKDT14DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT14DI8_Node;

var
  NearestNeighbour: PKDT14DI8_Node;

  function FindParentNode(const buffPtr: PKDT14DI8_Vec; NodePtr: PKDT14DI8_Node): PKDT14DI8_Node;
  var
    Next: PKDT14DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT14DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT14DI8_Node; const buffPtr: PKDT14DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT14DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT14DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT14DI8_Vec; const p1, p2: PKDT14DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT14DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT14DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT14DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT14DI8_Node;
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
  Parent: PKDT14DI8_Node;
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

  SearchedDistanceMin := KDT14DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT14DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT14DI8.Search(const buff: TKDT14DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT14DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT14DI8.Search(const buff: TKDT14DI8_Vec; var SearchedDistanceMin: Double): PKDT14DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT14DI8.Search(const buff: TKDT14DI8_Vec): PKDT14DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT14DI8.SearchToken(const buff: TKDT14DI8_Vec): TPascalString;
var
  p: PKDT14DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT14DI8.Search(const inBuff: TKDT14DI8_DynamicVecBuffer; var OutBuff: TKDT14DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT14DI8_DynamicVecBuffer;
  outBuffPtr: PKDT14DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT14DI8_Node;
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
        p: PKDT14DI8_Node;
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
  p: PKDT14DI8_Node;
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


procedure TKDT14DI8.Search(const inBuff: TKDT14DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT14DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT14DI8_Node;
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
        p: PKDT14DI8_Node;
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
  p: PKDT14DI8_Node;
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


procedure TKDT14DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT14DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT14DI8_Vec));
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

procedure TKDT14DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT14DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT14DI8_Vec)) <> SizeOf(TKDT14DI8_Vec) then
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

procedure TKDT14DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT14DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT14DI8.PrintNodeTree(const NodePtr: PKDT14DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT14DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT14DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT14DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT14DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT14DI8.KDT14DI8Vec(const s: SystemString): TKDT14DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT14DI8_Axis - 1 do
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
            if j >= KDT14DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT14DI8.KDT14DI8Vec(const v: TKDT14DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT14DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT14DI8.KDT14DI8Pow(const v: TKDT14DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT14DI8.KDT14DI8Distance(const v1, v2: TKDT14DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT14DI8_Axis - 1 do
      Result := Result + KDT14DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT14DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT14DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT14DI8.Test;
var
  TKDT14DI8_Test: TKDT14DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT14DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT14DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT14DI8_Test := TKDT14DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT14DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT14DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT14DI8_Axis - 1 do
        TKDT14DI8_Test.TestBuff[i][j] := i * KDT14DI8_Axis + j;

{$IFDEF FPC}
  TKDT14DI8_Test.BuildKDTreeM(length(TKDT14DI8_Test.TestBuff), nil, @TKDT14DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT14DI8_Test.BuildKDTreeM(length(TKDT14DI8_Test.TestBuff), nil, TKDT14DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT14DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT14DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT14DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT14DI8_Test.Search(TKDT14DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT14DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT14DI8_Test.TestBuff));
      TKDT14DI8_Test.Search(TKDT14DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT14DI8Distance(TKDT14DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT14DI8_Test.Clear;
      { kMean test }
      TKDT14DI8_Test.BuildKDTreeWithCluster(TKDT14DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT14DI8_Test.Search(TKDT14DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT14DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT14DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT14DI8_Test);
end;


function TKDT15DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT15DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT15DI8_Node;
  function SortCompare(const p1, p2: PKDT15DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT15DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT15DI8_Source;
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
  kdBuffPtr: PKDT15DI8_SourceBuffer;
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
      axis := Depth mod KDT15DI8_Axis;
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

function TKDT15DI8.GetData(const Index: NativeInt): PKDT15DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT15DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT15DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT15DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT15DI8_Node(KDNodes[i]));
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

function TKDT15DI8.StoreBuffPtr: PKDT15DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT15DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT15DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT15DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT15DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT15DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT15DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT15DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT15DI8.BuildKDTreeWithCluster(const inBuff: TKDT15DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT15DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT15DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT15DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT15DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT15DI8.BuildKDTreeWithCluster(const inBuff: TKDT15DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT15DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DI8_BuildCall);
var
  TempStoreBuff: TKDT15DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT15DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT15DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT15DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT15DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT15DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT15DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DI8_BuildMethod);
var
  TempStoreBuff: TKDT15DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT15DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT15DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT15DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT15DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT15DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT15DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT15DI8_BuildProc);
var
  TempStoreBuff: TKDT15DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT15DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT15DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT15DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT15DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT15DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT15DI8.Search(const buff: TKDT15DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT15DI8_Node;

var
  NearestNeighbour: PKDT15DI8_Node;

  function FindParentNode(const buffPtr: PKDT15DI8_Vec; NodePtr: PKDT15DI8_Node): PKDT15DI8_Node;
  var
    Next: PKDT15DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT15DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT15DI8_Node; const buffPtr: PKDT15DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT15DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT15DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT15DI8_Vec; const p1, p2: PKDT15DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT15DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT15DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT15DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT15DI8_Node;
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
  Parent: PKDT15DI8_Node;
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

  SearchedDistanceMin := KDT15DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT15DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT15DI8.Search(const buff: TKDT15DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT15DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT15DI8.Search(const buff: TKDT15DI8_Vec; var SearchedDistanceMin: Double): PKDT15DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT15DI8.Search(const buff: TKDT15DI8_Vec): PKDT15DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT15DI8.SearchToken(const buff: TKDT15DI8_Vec): TPascalString;
var
  p: PKDT15DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT15DI8.Search(const inBuff: TKDT15DI8_DynamicVecBuffer; var OutBuff: TKDT15DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT15DI8_DynamicVecBuffer;
  outBuffPtr: PKDT15DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT15DI8_Node;
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
        p: PKDT15DI8_Node;
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
  p: PKDT15DI8_Node;
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


procedure TKDT15DI8.Search(const inBuff: TKDT15DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT15DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT15DI8_Node;
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
        p: PKDT15DI8_Node;
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
  p: PKDT15DI8_Node;
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


procedure TKDT15DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT15DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT15DI8_Vec));
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

procedure TKDT15DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT15DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT15DI8_Vec)) <> SizeOf(TKDT15DI8_Vec) then
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

procedure TKDT15DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT15DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT15DI8.PrintNodeTree(const NodePtr: PKDT15DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT15DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT15DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT15DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT15DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT15DI8.KDT15DI8Vec(const s: SystemString): TKDT15DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT15DI8_Axis - 1 do
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
            if j >= KDT15DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT15DI8.KDT15DI8Vec(const v: TKDT15DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT15DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT15DI8.KDT15DI8Pow(const v: TKDT15DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT15DI8.KDT15DI8Distance(const v1, v2: TKDT15DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT15DI8_Axis - 1 do
      Result := Result + KDT15DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT15DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT15DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT15DI8.Test;
var
  TKDT15DI8_Test: TKDT15DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT15DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT15DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT15DI8_Test := TKDT15DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT15DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT15DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT15DI8_Axis - 1 do
        TKDT15DI8_Test.TestBuff[i][j] := i * KDT15DI8_Axis + j;

{$IFDEF FPC}
  TKDT15DI8_Test.BuildKDTreeM(length(TKDT15DI8_Test.TestBuff), nil, @TKDT15DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT15DI8_Test.BuildKDTreeM(length(TKDT15DI8_Test.TestBuff), nil, TKDT15DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT15DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT15DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT15DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT15DI8_Test.Search(TKDT15DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT15DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT15DI8_Test.TestBuff));
      TKDT15DI8_Test.Search(TKDT15DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT15DI8Distance(TKDT15DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT15DI8_Test.Clear;
      { kMean test }
      TKDT15DI8_Test.BuildKDTreeWithCluster(TKDT15DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT15DI8_Test.Search(TKDT15DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT15DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT15DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT15DI8_Test);
end;


function TKDT16DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT16DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT16DI8_Node;
  function SortCompare(const p1, p2: PKDT16DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT16DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT16DI8_Source;
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
  kdBuffPtr: PKDT16DI8_SourceBuffer;
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
      axis := Depth mod KDT16DI8_Axis;
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

function TKDT16DI8.GetData(const Index: NativeInt): PKDT16DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT16DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT16DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT16DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT16DI8_Node(KDNodes[i]));
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

function TKDT16DI8.StoreBuffPtr: PKDT16DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT16DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT16DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT16DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT16DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT16DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT16DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT16DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT16DI8.BuildKDTreeWithCluster(const inBuff: TKDT16DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT16DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT16DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT16DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT16DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT16DI8.BuildKDTreeWithCluster(const inBuff: TKDT16DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT16DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DI8_BuildCall);
var
  TempStoreBuff: TKDT16DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT16DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT16DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT16DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT16DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT16DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT16DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DI8_BuildMethod);
var
  TempStoreBuff: TKDT16DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT16DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT16DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT16DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT16DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT16DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT16DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT16DI8_BuildProc);
var
  TempStoreBuff: TKDT16DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT16DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT16DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT16DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT16DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT16DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT16DI8.Search(const buff: TKDT16DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT16DI8_Node;

var
  NearestNeighbour: PKDT16DI8_Node;

  function FindParentNode(const buffPtr: PKDT16DI8_Vec; NodePtr: PKDT16DI8_Node): PKDT16DI8_Node;
  var
    Next: PKDT16DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT16DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT16DI8_Node; const buffPtr: PKDT16DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT16DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT16DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT16DI8_Vec; const p1, p2: PKDT16DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT16DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT16DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT16DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT16DI8_Node;
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
  Parent: PKDT16DI8_Node;
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

  SearchedDistanceMin := KDT16DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT16DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT16DI8.Search(const buff: TKDT16DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT16DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT16DI8.Search(const buff: TKDT16DI8_Vec; var SearchedDistanceMin: Double): PKDT16DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT16DI8.Search(const buff: TKDT16DI8_Vec): PKDT16DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT16DI8.SearchToken(const buff: TKDT16DI8_Vec): TPascalString;
var
  p: PKDT16DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT16DI8.Search(const inBuff: TKDT16DI8_DynamicVecBuffer; var OutBuff: TKDT16DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT16DI8_DynamicVecBuffer;
  outBuffPtr: PKDT16DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT16DI8_Node;
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
        p: PKDT16DI8_Node;
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
  p: PKDT16DI8_Node;
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


procedure TKDT16DI8.Search(const inBuff: TKDT16DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT16DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT16DI8_Node;
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
        p: PKDT16DI8_Node;
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
  p: PKDT16DI8_Node;
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


procedure TKDT16DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT16DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT16DI8_Vec));
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

procedure TKDT16DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT16DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT16DI8_Vec)) <> SizeOf(TKDT16DI8_Vec) then
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

procedure TKDT16DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT16DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT16DI8.PrintNodeTree(const NodePtr: PKDT16DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT16DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT16DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT16DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT16DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT16DI8.KDT16DI8Vec(const s: SystemString): TKDT16DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT16DI8_Axis - 1 do
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
            if j >= KDT16DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT16DI8.KDT16DI8Vec(const v: TKDT16DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT16DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT16DI8.KDT16DI8Pow(const v: TKDT16DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT16DI8.KDT16DI8Distance(const v1, v2: TKDT16DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT16DI8_Axis - 1 do
      Result := Result + KDT16DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT16DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT16DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT16DI8.Test;
var
  TKDT16DI8_Test: TKDT16DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT16DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT16DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT16DI8_Test := TKDT16DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT16DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT16DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT16DI8_Axis - 1 do
        TKDT16DI8_Test.TestBuff[i][j] := i * KDT16DI8_Axis + j;

{$IFDEF FPC}
  TKDT16DI8_Test.BuildKDTreeM(length(TKDT16DI8_Test.TestBuff), nil, @TKDT16DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT16DI8_Test.BuildKDTreeM(length(TKDT16DI8_Test.TestBuff), nil, TKDT16DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT16DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT16DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT16DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT16DI8_Test.Search(TKDT16DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT16DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT16DI8_Test.TestBuff));
      TKDT16DI8_Test.Search(TKDT16DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT16DI8Distance(TKDT16DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT16DI8_Test.Clear;
      { kMean test }
      TKDT16DI8_Test.BuildKDTreeWithCluster(TKDT16DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT16DI8_Test.Search(TKDT16DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT16DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT16DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT16DI8_Test);
end;


function TKDT17DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT17DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT17DI8_Node;
  function SortCompare(const p1, p2: PKDT17DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT17DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT17DI8_Source;
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
  kdBuffPtr: PKDT17DI8_SourceBuffer;
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
      axis := Depth mod KDT17DI8_Axis;
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

function TKDT17DI8.GetData(const Index: NativeInt): PKDT17DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT17DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT17DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT17DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT17DI8_Node(KDNodes[i]));
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

function TKDT17DI8.StoreBuffPtr: PKDT17DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT17DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT17DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT17DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT17DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT17DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT17DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT17DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT17DI8.BuildKDTreeWithCluster(const inBuff: TKDT17DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT17DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT17DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT17DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT17DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT17DI8.BuildKDTreeWithCluster(const inBuff: TKDT17DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT17DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DI8_BuildCall);
var
  TempStoreBuff: TKDT17DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT17DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT17DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT17DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT17DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT17DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT17DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DI8_BuildMethod);
var
  TempStoreBuff: TKDT17DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT17DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT17DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT17DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT17DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT17DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT17DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT17DI8_BuildProc);
var
  TempStoreBuff: TKDT17DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT17DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT17DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT17DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT17DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT17DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT17DI8.Search(const buff: TKDT17DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT17DI8_Node;

var
  NearestNeighbour: PKDT17DI8_Node;

  function FindParentNode(const buffPtr: PKDT17DI8_Vec; NodePtr: PKDT17DI8_Node): PKDT17DI8_Node;
  var
    Next: PKDT17DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT17DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT17DI8_Node; const buffPtr: PKDT17DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT17DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT17DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT17DI8_Vec; const p1, p2: PKDT17DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT17DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT17DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT17DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT17DI8_Node;
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
  Parent: PKDT17DI8_Node;
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

  SearchedDistanceMin := KDT17DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT17DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT17DI8.Search(const buff: TKDT17DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT17DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT17DI8.Search(const buff: TKDT17DI8_Vec; var SearchedDistanceMin: Double): PKDT17DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT17DI8.Search(const buff: TKDT17DI8_Vec): PKDT17DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT17DI8.SearchToken(const buff: TKDT17DI8_Vec): TPascalString;
var
  p: PKDT17DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT17DI8.Search(const inBuff: TKDT17DI8_DynamicVecBuffer; var OutBuff: TKDT17DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT17DI8_DynamicVecBuffer;
  outBuffPtr: PKDT17DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT17DI8_Node;
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
        p: PKDT17DI8_Node;
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
  p: PKDT17DI8_Node;
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


procedure TKDT17DI8.Search(const inBuff: TKDT17DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT17DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT17DI8_Node;
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
        p: PKDT17DI8_Node;
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
  p: PKDT17DI8_Node;
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


procedure TKDT17DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT17DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT17DI8_Vec));
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

procedure TKDT17DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT17DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT17DI8_Vec)) <> SizeOf(TKDT17DI8_Vec) then
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

procedure TKDT17DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT17DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT17DI8.PrintNodeTree(const NodePtr: PKDT17DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT17DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT17DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT17DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT17DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT17DI8.KDT17DI8Vec(const s: SystemString): TKDT17DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT17DI8_Axis - 1 do
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
            if j >= KDT17DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT17DI8.KDT17DI8Vec(const v: TKDT17DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT17DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT17DI8.KDT17DI8Pow(const v: TKDT17DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT17DI8.KDT17DI8Distance(const v1, v2: TKDT17DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT17DI8_Axis - 1 do
      Result := Result + KDT17DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT17DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT17DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT17DI8.Test;
var
  TKDT17DI8_Test: TKDT17DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT17DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT17DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT17DI8_Test := TKDT17DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT17DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT17DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT17DI8_Axis - 1 do
        TKDT17DI8_Test.TestBuff[i][j] := i * KDT17DI8_Axis + j;

{$IFDEF FPC}
  TKDT17DI8_Test.BuildKDTreeM(length(TKDT17DI8_Test.TestBuff), nil, @TKDT17DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT17DI8_Test.BuildKDTreeM(length(TKDT17DI8_Test.TestBuff), nil, TKDT17DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT17DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT17DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT17DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT17DI8_Test.Search(TKDT17DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT17DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT17DI8_Test.TestBuff));
      TKDT17DI8_Test.Search(TKDT17DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT17DI8Distance(TKDT17DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT17DI8_Test.Clear;
      { kMean test }
      TKDT17DI8_Test.BuildKDTreeWithCluster(TKDT17DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT17DI8_Test.Search(TKDT17DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT17DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT17DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT17DI8_Test);
end;


function TKDT18DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT18DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT18DI8_Node;
  function SortCompare(const p1, p2: PKDT18DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT18DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT18DI8_Source;
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
  kdBuffPtr: PKDT18DI8_SourceBuffer;
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
      axis := Depth mod KDT18DI8_Axis;
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

function TKDT18DI8.GetData(const Index: NativeInt): PKDT18DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT18DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT18DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT18DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT18DI8_Node(KDNodes[i]));
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

function TKDT18DI8.StoreBuffPtr: PKDT18DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT18DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT18DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT18DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT18DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT18DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT18DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT18DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT18DI8.BuildKDTreeWithCluster(const inBuff: TKDT18DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT18DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT18DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT18DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT18DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT18DI8.BuildKDTreeWithCluster(const inBuff: TKDT18DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT18DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DI8_BuildCall);
var
  TempStoreBuff: TKDT18DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT18DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT18DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT18DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT18DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT18DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT18DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DI8_BuildMethod);
var
  TempStoreBuff: TKDT18DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT18DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT18DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT18DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT18DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT18DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT18DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT18DI8_BuildProc);
var
  TempStoreBuff: TKDT18DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT18DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT18DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT18DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT18DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT18DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT18DI8.Search(const buff: TKDT18DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT18DI8_Node;

var
  NearestNeighbour: PKDT18DI8_Node;

  function FindParentNode(const buffPtr: PKDT18DI8_Vec; NodePtr: PKDT18DI8_Node): PKDT18DI8_Node;
  var
    Next: PKDT18DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT18DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT18DI8_Node; const buffPtr: PKDT18DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT18DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT18DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT18DI8_Vec; const p1, p2: PKDT18DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT18DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT18DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT18DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT18DI8_Node;
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
  Parent: PKDT18DI8_Node;
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

  SearchedDistanceMin := KDT18DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT18DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT18DI8.Search(const buff: TKDT18DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT18DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT18DI8.Search(const buff: TKDT18DI8_Vec; var SearchedDistanceMin: Double): PKDT18DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT18DI8.Search(const buff: TKDT18DI8_Vec): PKDT18DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT18DI8.SearchToken(const buff: TKDT18DI8_Vec): TPascalString;
var
  p: PKDT18DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT18DI8.Search(const inBuff: TKDT18DI8_DynamicVecBuffer; var OutBuff: TKDT18DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT18DI8_DynamicVecBuffer;
  outBuffPtr: PKDT18DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT18DI8_Node;
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
        p: PKDT18DI8_Node;
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
  p: PKDT18DI8_Node;
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


procedure TKDT18DI8.Search(const inBuff: TKDT18DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT18DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT18DI8_Node;
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
        p: PKDT18DI8_Node;
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
  p: PKDT18DI8_Node;
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


procedure TKDT18DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT18DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT18DI8_Vec));
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

procedure TKDT18DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT18DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT18DI8_Vec)) <> SizeOf(TKDT18DI8_Vec) then
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

procedure TKDT18DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT18DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT18DI8.PrintNodeTree(const NodePtr: PKDT18DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT18DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT18DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT18DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT18DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT18DI8.KDT18DI8Vec(const s: SystemString): TKDT18DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT18DI8_Axis - 1 do
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
            if j >= KDT18DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT18DI8.KDT18DI8Vec(const v: TKDT18DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT18DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT18DI8.KDT18DI8Pow(const v: TKDT18DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT18DI8.KDT18DI8Distance(const v1, v2: TKDT18DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT18DI8_Axis - 1 do
      Result := Result + KDT18DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT18DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT18DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT18DI8.Test;
var
  TKDT18DI8_Test: TKDT18DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT18DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT18DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT18DI8_Test := TKDT18DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT18DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT18DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT18DI8_Axis - 1 do
        TKDT18DI8_Test.TestBuff[i][j] := i * KDT18DI8_Axis + j;

{$IFDEF FPC}
  TKDT18DI8_Test.BuildKDTreeM(length(TKDT18DI8_Test.TestBuff), nil, @TKDT18DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT18DI8_Test.BuildKDTreeM(length(TKDT18DI8_Test.TestBuff), nil, TKDT18DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT18DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT18DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT18DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT18DI8_Test.Search(TKDT18DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT18DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT18DI8_Test.TestBuff));
      TKDT18DI8_Test.Search(TKDT18DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT18DI8Distance(TKDT18DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT18DI8_Test.Clear;
      { kMean test }
      TKDT18DI8_Test.BuildKDTreeWithCluster(TKDT18DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT18DI8_Test.Search(TKDT18DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT18DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT18DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT18DI8_Test);
end;


function TKDT19DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT19DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT19DI8_Node;
  function SortCompare(const p1, p2: PKDT19DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT19DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT19DI8_Source;
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
  kdBuffPtr: PKDT19DI8_SourceBuffer;
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
      axis := Depth mod KDT19DI8_Axis;
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

function TKDT19DI8.GetData(const Index: NativeInt): PKDT19DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT19DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT19DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT19DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT19DI8_Node(KDNodes[i]));
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

function TKDT19DI8.StoreBuffPtr: PKDT19DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT19DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT19DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT19DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT19DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT19DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT19DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT19DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT19DI8.BuildKDTreeWithCluster(const inBuff: TKDT19DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT19DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT19DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT19DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT19DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT19DI8.BuildKDTreeWithCluster(const inBuff: TKDT19DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT19DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DI8_BuildCall);
var
  TempStoreBuff: TKDT19DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT19DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT19DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT19DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT19DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT19DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT19DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DI8_BuildMethod);
var
  TempStoreBuff: TKDT19DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT19DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT19DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT19DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT19DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT19DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT19DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT19DI8_BuildProc);
var
  TempStoreBuff: TKDT19DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT19DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT19DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT19DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT19DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT19DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT19DI8.Search(const buff: TKDT19DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT19DI8_Node;

var
  NearestNeighbour: PKDT19DI8_Node;

  function FindParentNode(const buffPtr: PKDT19DI8_Vec; NodePtr: PKDT19DI8_Node): PKDT19DI8_Node;
  var
    Next: PKDT19DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT19DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT19DI8_Node; const buffPtr: PKDT19DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT19DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT19DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT19DI8_Vec; const p1, p2: PKDT19DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT19DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT19DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT19DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT19DI8_Node;
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
  Parent: PKDT19DI8_Node;
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

  SearchedDistanceMin := KDT19DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT19DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT19DI8.Search(const buff: TKDT19DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT19DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT19DI8.Search(const buff: TKDT19DI8_Vec; var SearchedDistanceMin: Double): PKDT19DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT19DI8.Search(const buff: TKDT19DI8_Vec): PKDT19DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT19DI8.SearchToken(const buff: TKDT19DI8_Vec): TPascalString;
var
  p: PKDT19DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT19DI8.Search(const inBuff: TKDT19DI8_DynamicVecBuffer; var OutBuff: TKDT19DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT19DI8_DynamicVecBuffer;
  outBuffPtr: PKDT19DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT19DI8_Node;
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
        p: PKDT19DI8_Node;
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
  p: PKDT19DI8_Node;
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


procedure TKDT19DI8.Search(const inBuff: TKDT19DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT19DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT19DI8_Node;
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
        p: PKDT19DI8_Node;
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
  p: PKDT19DI8_Node;
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


procedure TKDT19DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT19DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT19DI8_Vec));
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

procedure TKDT19DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT19DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT19DI8_Vec)) <> SizeOf(TKDT19DI8_Vec) then
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

procedure TKDT19DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT19DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT19DI8.PrintNodeTree(const NodePtr: PKDT19DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT19DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT19DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT19DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT19DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT19DI8.KDT19DI8Vec(const s: SystemString): TKDT19DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT19DI8_Axis - 1 do
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
            if j >= KDT19DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT19DI8.KDT19DI8Vec(const v: TKDT19DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT19DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT19DI8.KDT19DI8Pow(const v: TKDT19DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT19DI8.KDT19DI8Distance(const v1, v2: TKDT19DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT19DI8_Axis - 1 do
      Result := Result + KDT19DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT19DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT19DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT19DI8.Test;
var
  TKDT19DI8_Test: TKDT19DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT19DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT19DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT19DI8_Test := TKDT19DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT19DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT19DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT19DI8_Axis - 1 do
        TKDT19DI8_Test.TestBuff[i][j] := i * KDT19DI8_Axis + j;

{$IFDEF FPC}
  TKDT19DI8_Test.BuildKDTreeM(length(TKDT19DI8_Test.TestBuff), nil, @TKDT19DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT19DI8_Test.BuildKDTreeM(length(TKDT19DI8_Test.TestBuff), nil, TKDT19DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT19DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT19DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT19DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT19DI8_Test.Search(TKDT19DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT19DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT19DI8_Test.TestBuff));
      TKDT19DI8_Test.Search(TKDT19DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT19DI8Distance(TKDT19DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT19DI8_Test.Clear;
      { kMean test }
      TKDT19DI8_Test.BuildKDTreeWithCluster(TKDT19DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT19DI8_Test.Search(TKDT19DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT19DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT19DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT19DI8_Test);
end;


function TKDT20DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT20DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT20DI8_Node;
  function SortCompare(const p1, p2: PKDT20DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT20DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT20DI8_Source;
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
  kdBuffPtr: PKDT20DI8_SourceBuffer;
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
      axis := Depth mod KDT20DI8_Axis;
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

function TKDT20DI8.GetData(const Index: NativeInt): PKDT20DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT20DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT20DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT20DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT20DI8_Node(KDNodes[i]));
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

function TKDT20DI8.StoreBuffPtr: PKDT20DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT20DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT20DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT20DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT20DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT20DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT20DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT20DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT20DI8.BuildKDTreeWithCluster(const inBuff: TKDT20DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT20DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT20DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT20DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT20DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT20DI8.BuildKDTreeWithCluster(const inBuff: TKDT20DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT20DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DI8_BuildCall);
var
  TempStoreBuff: TKDT20DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT20DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT20DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT20DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT20DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT20DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT20DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DI8_BuildMethod);
var
  TempStoreBuff: TKDT20DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT20DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT20DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT20DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT20DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT20DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT20DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT20DI8_BuildProc);
var
  TempStoreBuff: TKDT20DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT20DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT20DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT20DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT20DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT20DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT20DI8.Search(const buff: TKDT20DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT20DI8_Node;

var
  NearestNeighbour: PKDT20DI8_Node;

  function FindParentNode(const buffPtr: PKDT20DI8_Vec; NodePtr: PKDT20DI8_Node): PKDT20DI8_Node;
  var
    Next: PKDT20DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT20DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT20DI8_Node; const buffPtr: PKDT20DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT20DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT20DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT20DI8_Vec; const p1, p2: PKDT20DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT20DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT20DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT20DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT20DI8_Node;
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
  Parent: PKDT20DI8_Node;
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

  SearchedDistanceMin := KDT20DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT20DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT20DI8.Search(const buff: TKDT20DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT20DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT20DI8.Search(const buff: TKDT20DI8_Vec; var SearchedDistanceMin: Double): PKDT20DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT20DI8.Search(const buff: TKDT20DI8_Vec): PKDT20DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT20DI8.SearchToken(const buff: TKDT20DI8_Vec): TPascalString;
var
  p: PKDT20DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT20DI8.Search(const inBuff: TKDT20DI8_DynamicVecBuffer; var OutBuff: TKDT20DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT20DI8_DynamicVecBuffer;
  outBuffPtr: PKDT20DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT20DI8_Node;
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
        p: PKDT20DI8_Node;
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
  p: PKDT20DI8_Node;
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


procedure TKDT20DI8.Search(const inBuff: TKDT20DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT20DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT20DI8_Node;
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
        p: PKDT20DI8_Node;
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
  p: PKDT20DI8_Node;
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


procedure TKDT20DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT20DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT20DI8_Vec));
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

procedure TKDT20DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT20DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT20DI8_Vec)) <> SizeOf(TKDT20DI8_Vec) then
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

procedure TKDT20DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT20DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT20DI8.PrintNodeTree(const NodePtr: PKDT20DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT20DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT20DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT20DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT20DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT20DI8.KDT20DI8Vec(const s: SystemString): TKDT20DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT20DI8_Axis - 1 do
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
            if j >= KDT20DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT20DI8.KDT20DI8Vec(const v: TKDT20DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT20DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT20DI8.KDT20DI8Pow(const v: TKDT20DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT20DI8.KDT20DI8Distance(const v1, v2: TKDT20DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT20DI8_Axis - 1 do
      Result := Result + KDT20DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT20DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT20DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT20DI8.Test;
var
  TKDT20DI8_Test: TKDT20DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT20DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT20DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT20DI8_Test := TKDT20DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT20DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT20DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT20DI8_Axis - 1 do
        TKDT20DI8_Test.TestBuff[i][j] := i * KDT20DI8_Axis + j;

{$IFDEF FPC}
  TKDT20DI8_Test.BuildKDTreeM(length(TKDT20DI8_Test.TestBuff), nil, @TKDT20DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT20DI8_Test.BuildKDTreeM(length(TKDT20DI8_Test.TestBuff), nil, TKDT20DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT20DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT20DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT20DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT20DI8_Test.Search(TKDT20DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT20DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT20DI8_Test.TestBuff));
      TKDT20DI8_Test.Search(TKDT20DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT20DI8Distance(TKDT20DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT20DI8_Test.Clear;
      { kMean test }
      TKDT20DI8_Test.BuildKDTreeWithCluster(TKDT20DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT20DI8_Test.Search(TKDT20DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT20DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT20DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT20DI8_Test);
end;


function TKDT21DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT21DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT21DI8_Node;
  function SortCompare(const p1, p2: PKDT21DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT21DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT21DI8_Source;
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
  kdBuffPtr: PKDT21DI8_SourceBuffer;
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
      axis := Depth mod KDT21DI8_Axis;
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

function TKDT21DI8.GetData(const Index: NativeInt): PKDT21DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT21DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT21DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT21DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT21DI8_Node(KDNodes[i]));
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

function TKDT21DI8.StoreBuffPtr: PKDT21DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT21DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT21DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT21DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT21DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT21DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT21DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT21DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT21DI8.BuildKDTreeWithCluster(const inBuff: TKDT21DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT21DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT21DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT21DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT21DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT21DI8.BuildKDTreeWithCluster(const inBuff: TKDT21DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT21DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DI8_BuildCall);
var
  TempStoreBuff: TKDT21DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT21DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT21DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT21DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT21DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT21DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT21DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DI8_BuildMethod);
var
  TempStoreBuff: TKDT21DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT21DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT21DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT21DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT21DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT21DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT21DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT21DI8_BuildProc);
var
  TempStoreBuff: TKDT21DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT21DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT21DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT21DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT21DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT21DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT21DI8.Search(const buff: TKDT21DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT21DI8_Node;

var
  NearestNeighbour: PKDT21DI8_Node;

  function FindParentNode(const buffPtr: PKDT21DI8_Vec; NodePtr: PKDT21DI8_Node): PKDT21DI8_Node;
  var
    Next: PKDT21DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT21DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT21DI8_Node; const buffPtr: PKDT21DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT21DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT21DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT21DI8_Vec; const p1, p2: PKDT21DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT21DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT21DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT21DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT21DI8_Node;
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
  Parent: PKDT21DI8_Node;
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

  SearchedDistanceMin := KDT21DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT21DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT21DI8.Search(const buff: TKDT21DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT21DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT21DI8.Search(const buff: TKDT21DI8_Vec; var SearchedDistanceMin: Double): PKDT21DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT21DI8.Search(const buff: TKDT21DI8_Vec): PKDT21DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT21DI8.SearchToken(const buff: TKDT21DI8_Vec): TPascalString;
var
  p: PKDT21DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT21DI8.Search(const inBuff: TKDT21DI8_DynamicVecBuffer; var OutBuff: TKDT21DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT21DI8_DynamicVecBuffer;
  outBuffPtr: PKDT21DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT21DI8_Node;
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
        p: PKDT21DI8_Node;
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
  p: PKDT21DI8_Node;
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


procedure TKDT21DI8.Search(const inBuff: TKDT21DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT21DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT21DI8_Node;
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
        p: PKDT21DI8_Node;
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
  p: PKDT21DI8_Node;
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


procedure TKDT21DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT21DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT21DI8_Vec));
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

procedure TKDT21DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT21DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT21DI8_Vec)) <> SizeOf(TKDT21DI8_Vec) then
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

procedure TKDT21DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT21DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT21DI8.PrintNodeTree(const NodePtr: PKDT21DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT21DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT21DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT21DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT21DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT21DI8.KDT21DI8Vec(const s: SystemString): TKDT21DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT21DI8_Axis - 1 do
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
            if j >= KDT21DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT21DI8.KDT21DI8Vec(const v: TKDT21DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT21DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT21DI8.KDT21DI8Pow(const v: TKDT21DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT21DI8.KDT21DI8Distance(const v1, v2: TKDT21DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT21DI8_Axis - 1 do
      Result := Result + KDT21DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT21DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT21DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT21DI8.Test;
var
  TKDT21DI8_Test: TKDT21DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT21DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT21DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT21DI8_Test := TKDT21DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT21DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT21DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT21DI8_Axis - 1 do
        TKDT21DI8_Test.TestBuff[i][j] := i * KDT21DI8_Axis + j;

{$IFDEF FPC}
  TKDT21DI8_Test.BuildKDTreeM(length(TKDT21DI8_Test.TestBuff), nil, @TKDT21DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT21DI8_Test.BuildKDTreeM(length(TKDT21DI8_Test.TestBuff), nil, TKDT21DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT21DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT21DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT21DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT21DI8_Test.Search(TKDT21DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT21DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT21DI8_Test.TestBuff));
      TKDT21DI8_Test.Search(TKDT21DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT21DI8Distance(TKDT21DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT21DI8_Test.Clear;
      { kMean test }
      TKDT21DI8_Test.BuildKDTreeWithCluster(TKDT21DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT21DI8_Test.Search(TKDT21DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT21DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT21DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT21DI8_Test);
end;


function TKDT22DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT22DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT22DI8_Node;
  function SortCompare(const p1, p2: PKDT22DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT22DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT22DI8_Source;
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
  kdBuffPtr: PKDT22DI8_SourceBuffer;
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
      axis := Depth mod KDT22DI8_Axis;
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

function TKDT22DI8.GetData(const Index: NativeInt): PKDT22DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT22DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT22DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT22DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT22DI8_Node(KDNodes[i]));
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

function TKDT22DI8.StoreBuffPtr: PKDT22DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT22DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT22DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT22DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT22DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT22DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT22DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT22DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT22DI8.BuildKDTreeWithCluster(const inBuff: TKDT22DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT22DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT22DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT22DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT22DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT22DI8.BuildKDTreeWithCluster(const inBuff: TKDT22DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT22DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DI8_BuildCall);
var
  TempStoreBuff: TKDT22DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT22DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT22DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT22DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT22DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT22DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT22DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DI8_BuildMethod);
var
  TempStoreBuff: TKDT22DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT22DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT22DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT22DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT22DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT22DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT22DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT22DI8_BuildProc);
var
  TempStoreBuff: TKDT22DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT22DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT22DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT22DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT22DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT22DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT22DI8.Search(const buff: TKDT22DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT22DI8_Node;

var
  NearestNeighbour: PKDT22DI8_Node;

  function FindParentNode(const buffPtr: PKDT22DI8_Vec; NodePtr: PKDT22DI8_Node): PKDT22DI8_Node;
  var
    Next: PKDT22DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT22DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT22DI8_Node; const buffPtr: PKDT22DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT22DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT22DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT22DI8_Vec; const p1, p2: PKDT22DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT22DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT22DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT22DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT22DI8_Node;
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
  Parent: PKDT22DI8_Node;
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

  SearchedDistanceMin := KDT22DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT22DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT22DI8.Search(const buff: TKDT22DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT22DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT22DI8.Search(const buff: TKDT22DI8_Vec; var SearchedDistanceMin: Double): PKDT22DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT22DI8.Search(const buff: TKDT22DI8_Vec): PKDT22DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT22DI8.SearchToken(const buff: TKDT22DI8_Vec): TPascalString;
var
  p: PKDT22DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT22DI8.Search(const inBuff: TKDT22DI8_DynamicVecBuffer; var OutBuff: TKDT22DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT22DI8_DynamicVecBuffer;
  outBuffPtr: PKDT22DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT22DI8_Node;
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
        p: PKDT22DI8_Node;
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
  p: PKDT22DI8_Node;
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


procedure TKDT22DI8.Search(const inBuff: TKDT22DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT22DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT22DI8_Node;
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
        p: PKDT22DI8_Node;
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
  p: PKDT22DI8_Node;
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


procedure TKDT22DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT22DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT22DI8_Vec));
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

procedure TKDT22DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT22DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT22DI8_Vec)) <> SizeOf(TKDT22DI8_Vec) then
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

procedure TKDT22DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT22DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT22DI8.PrintNodeTree(const NodePtr: PKDT22DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT22DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT22DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT22DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT22DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT22DI8.KDT22DI8Vec(const s: SystemString): TKDT22DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT22DI8_Axis - 1 do
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
            if j >= KDT22DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT22DI8.KDT22DI8Vec(const v: TKDT22DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT22DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT22DI8.KDT22DI8Pow(const v: TKDT22DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT22DI8.KDT22DI8Distance(const v1, v2: TKDT22DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT22DI8_Axis - 1 do
      Result := Result + KDT22DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT22DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT22DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT22DI8.Test;
var
  TKDT22DI8_Test: TKDT22DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT22DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT22DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT22DI8_Test := TKDT22DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT22DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT22DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT22DI8_Axis - 1 do
        TKDT22DI8_Test.TestBuff[i][j] := i * KDT22DI8_Axis + j;

{$IFDEF FPC}
  TKDT22DI8_Test.BuildKDTreeM(length(TKDT22DI8_Test.TestBuff), nil, @TKDT22DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT22DI8_Test.BuildKDTreeM(length(TKDT22DI8_Test.TestBuff), nil, TKDT22DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT22DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT22DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT22DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT22DI8_Test.Search(TKDT22DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT22DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT22DI8_Test.TestBuff));
      TKDT22DI8_Test.Search(TKDT22DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT22DI8Distance(TKDT22DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT22DI8_Test.Clear;
      { kMean test }
      TKDT22DI8_Test.BuildKDTreeWithCluster(TKDT22DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT22DI8_Test.Search(TKDT22DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT22DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT22DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT22DI8_Test);
end;


function TKDT23DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT23DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT23DI8_Node;
  function SortCompare(const p1, p2: PKDT23DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT23DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT23DI8_Source;
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
  kdBuffPtr: PKDT23DI8_SourceBuffer;
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
      axis := Depth mod KDT23DI8_Axis;
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

function TKDT23DI8.GetData(const Index: NativeInt): PKDT23DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT23DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT23DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT23DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT23DI8_Node(KDNodes[i]));
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

function TKDT23DI8.StoreBuffPtr: PKDT23DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT23DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT23DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT23DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT23DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT23DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT23DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT23DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT23DI8.BuildKDTreeWithCluster(const inBuff: TKDT23DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT23DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT23DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT23DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT23DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT23DI8.BuildKDTreeWithCluster(const inBuff: TKDT23DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT23DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DI8_BuildCall);
var
  TempStoreBuff: TKDT23DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT23DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT23DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT23DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT23DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT23DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT23DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DI8_BuildMethod);
var
  TempStoreBuff: TKDT23DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT23DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT23DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT23DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT23DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT23DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT23DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT23DI8_BuildProc);
var
  TempStoreBuff: TKDT23DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT23DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT23DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT23DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT23DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT23DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT23DI8.Search(const buff: TKDT23DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT23DI8_Node;

var
  NearestNeighbour: PKDT23DI8_Node;

  function FindParentNode(const buffPtr: PKDT23DI8_Vec; NodePtr: PKDT23DI8_Node): PKDT23DI8_Node;
  var
    Next: PKDT23DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT23DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT23DI8_Node; const buffPtr: PKDT23DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT23DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT23DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT23DI8_Vec; const p1, p2: PKDT23DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT23DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT23DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT23DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT23DI8_Node;
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
  Parent: PKDT23DI8_Node;
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

  SearchedDistanceMin := KDT23DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT23DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT23DI8.Search(const buff: TKDT23DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT23DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT23DI8.Search(const buff: TKDT23DI8_Vec; var SearchedDistanceMin: Double): PKDT23DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT23DI8.Search(const buff: TKDT23DI8_Vec): PKDT23DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT23DI8.SearchToken(const buff: TKDT23DI8_Vec): TPascalString;
var
  p: PKDT23DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT23DI8.Search(const inBuff: TKDT23DI8_DynamicVecBuffer; var OutBuff: TKDT23DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT23DI8_DynamicVecBuffer;
  outBuffPtr: PKDT23DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT23DI8_Node;
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
        p: PKDT23DI8_Node;
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
  p: PKDT23DI8_Node;
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


procedure TKDT23DI8.Search(const inBuff: TKDT23DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT23DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT23DI8_Node;
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
        p: PKDT23DI8_Node;
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
  p: PKDT23DI8_Node;
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


procedure TKDT23DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT23DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT23DI8_Vec));
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

procedure TKDT23DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT23DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT23DI8_Vec)) <> SizeOf(TKDT23DI8_Vec) then
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

procedure TKDT23DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT23DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT23DI8.PrintNodeTree(const NodePtr: PKDT23DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT23DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT23DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT23DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT23DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT23DI8.KDT23DI8Vec(const s: SystemString): TKDT23DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT23DI8_Axis - 1 do
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
            if j >= KDT23DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT23DI8.KDT23DI8Vec(const v: TKDT23DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT23DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT23DI8.KDT23DI8Pow(const v: TKDT23DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT23DI8.KDT23DI8Distance(const v1, v2: TKDT23DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT23DI8_Axis - 1 do
      Result := Result + KDT23DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT23DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT23DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT23DI8.Test;
var
  TKDT23DI8_Test: TKDT23DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT23DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT23DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT23DI8_Test := TKDT23DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT23DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT23DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT23DI8_Axis - 1 do
        TKDT23DI8_Test.TestBuff[i][j] := i * KDT23DI8_Axis + j;

{$IFDEF FPC}
  TKDT23DI8_Test.BuildKDTreeM(length(TKDT23DI8_Test.TestBuff), nil, @TKDT23DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT23DI8_Test.BuildKDTreeM(length(TKDT23DI8_Test.TestBuff), nil, TKDT23DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT23DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT23DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT23DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT23DI8_Test.Search(TKDT23DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT23DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT23DI8_Test.TestBuff));
      TKDT23DI8_Test.Search(TKDT23DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT23DI8Distance(TKDT23DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT23DI8_Test.Clear;
      { kMean test }
      TKDT23DI8_Test.BuildKDTreeWithCluster(TKDT23DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT23DI8_Test.Search(TKDT23DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT23DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT23DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT23DI8_Test);
end;


function TKDT24DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT24DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT24DI8_Node;
  function SortCompare(const p1, p2: PKDT24DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT24DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT24DI8_Source;
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
  kdBuffPtr: PKDT24DI8_SourceBuffer;
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
      axis := Depth mod KDT24DI8_Axis;
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

function TKDT24DI8.GetData(const Index: NativeInt): PKDT24DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT24DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT24DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT24DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT24DI8_Node(KDNodes[i]));
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

function TKDT24DI8.StoreBuffPtr: PKDT24DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT24DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT24DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT24DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT24DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT24DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT24DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT24DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT24DI8.BuildKDTreeWithCluster(const inBuff: TKDT24DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT24DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT24DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT24DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT24DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT24DI8.BuildKDTreeWithCluster(const inBuff: TKDT24DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT24DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DI8_BuildCall);
var
  TempStoreBuff: TKDT24DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT24DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT24DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT24DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT24DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT24DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT24DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DI8_BuildMethod);
var
  TempStoreBuff: TKDT24DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT24DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT24DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT24DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT24DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT24DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT24DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT24DI8_BuildProc);
var
  TempStoreBuff: TKDT24DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT24DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT24DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT24DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT24DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT24DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT24DI8.Search(const buff: TKDT24DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT24DI8_Node;

var
  NearestNeighbour: PKDT24DI8_Node;

  function FindParentNode(const buffPtr: PKDT24DI8_Vec; NodePtr: PKDT24DI8_Node): PKDT24DI8_Node;
  var
    Next: PKDT24DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT24DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT24DI8_Node; const buffPtr: PKDT24DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT24DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT24DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT24DI8_Vec; const p1, p2: PKDT24DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT24DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT24DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT24DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT24DI8_Node;
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
  Parent: PKDT24DI8_Node;
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

  SearchedDistanceMin := KDT24DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT24DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT24DI8.Search(const buff: TKDT24DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT24DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT24DI8.Search(const buff: TKDT24DI8_Vec; var SearchedDistanceMin: Double): PKDT24DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT24DI8.Search(const buff: TKDT24DI8_Vec): PKDT24DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT24DI8.SearchToken(const buff: TKDT24DI8_Vec): TPascalString;
var
  p: PKDT24DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT24DI8.Search(const inBuff: TKDT24DI8_DynamicVecBuffer; var OutBuff: TKDT24DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT24DI8_DynamicVecBuffer;
  outBuffPtr: PKDT24DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT24DI8_Node;
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
        p: PKDT24DI8_Node;
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
  p: PKDT24DI8_Node;
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


procedure TKDT24DI8.Search(const inBuff: TKDT24DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT24DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT24DI8_Node;
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
        p: PKDT24DI8_Node;
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
  p: PKDT24DI8_Node;
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


procedure TKDT24DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT24DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT24DI8_Vec));
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

procedure TKDT24DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT24DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT24DI8_Vec)) <> SizeOf(TKDT24DI8_Vec) then
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

procedure TKDT24DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT24DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT24DI8.PrintNodeTree(const NodePtr: PKDT24DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT24DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT24DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT24DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT24DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT24DI8.KDT24DI8Vec(const s: SystemString): TKDT24DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT24DI8_Axis - 1 do
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
            if j >= KDT24DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT24DI8.KDT24DI8Vec(const v: TKDT24DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT24DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT24DI8.KDT24DI8Pow(const v: TKDT24DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT24DI8.KDT24DI8Distance(const v1, v2: TKDT24DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT24DI8_Axis - 1 do
      Result := Result + KDT24DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT24DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT24DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT24DI8.Test;
var
  TKDT24DI8_Test: TKDT24DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT24DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT24DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT24DI8_Test := TKDT24DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT24DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT24DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT24DI8_Axis - 1 do
        TKDT24DI8_Test.TestBuff[i][j] := i * KDT24DI8_Axis + j;

{$IFDEF FPC}
  TKDT24DI8_Test.BuildKDTreeM(length(TKDT24DI8_Test.TestBuff), nil, @TKDT24DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT24DI8_Test.BuildKDTreeM(length(TKDT24DI8_Test.TestBuff), nil, TKDT24DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT24DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT24DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT24DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT24DI8_Test.Search(TKDT24DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT24DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT24DI8_Test.TestBuff));
      TKDT24DI8_Test.Search(TKDT24DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT24DI8Distance(TKDT24DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT24DI8_Test.Clear;
      { kMean test }
      TKDT24DI8_Test.BuildKDTreeWithCluster(TKDT24DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT24DI8_Test.Search(TKDT24DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT24DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT24DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT24DI8_Test);
end;


function TKDT256DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT256DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT256DI8_Node;
  function SortCompare(const p1, p2: PKDT256DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT256DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT256DI8_Source;
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
  kdBuffPtr: PKDT256DI8_SourceBuffer;
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
      axis := Depth mod KDT256DI8_Axis;
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

function TKDT256DI8.GetData(const Index: NativeInt): PKDT256DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT256DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT256DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT256DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT256DI8_Node(KDNodes[i]));
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

function TKDT256DI8.StoreBuffPtr: PKDT256DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT256DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT256DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT256DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT256DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT256DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT256DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT256DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT256DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT256DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT256DI8.BuildKDTreeWithCluster(const inBuff: TKDT256DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT256DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT256DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT256DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT256DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT256DI8.BuildKDTreeWithCluster(const inBuff: TKDT256DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT256DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT256DI8_BuildCall);
var
  TempStoreBuff: TKDT256DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT256DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT256DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT256DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT256DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT256DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT256DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT256DI8_BuildMethod);
var
  TempStoreBuff: TKDT256DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT256DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT256DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT256DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT256DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT256DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT256DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT256DI8_BuildProc);
var
  TempStoreBuff: TKDT256DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT256DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT256DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT256DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT256DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT256DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT256DI8.Search(const buff: TKDT256DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT256DI8_Node;

var
  NearestNeighbour: PKDT256DI8_Node;

  function FindParentNode(const buffPtr: PKDT256DI8_Vec; NodePtr: PKDT256DI8_Node): PKDT256DI8_Node;
  var
    Next: PKDT256DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT256DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT256DI8_Node; const buffPtr: PKDT256DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT256DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT256DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT256DI8_Vec; const p1, p2: PKDT256DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT256DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT256DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT256DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT256DI8_Node;
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
  Parent: PKDT256DI8_Node;
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

  SearchedDistanceMin := KDT256DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT256DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT256DI8.Search(const buff: TKDT256DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT256DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT256DI8.Search(const buff: TKDT256DI8_Vec; var SearchedDistanceMin: Double): PKDT256DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT256DI8.Search(const buff: TKDT256DI8_Vec): PKDT256DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT256DI8.SearchToken(const buff: TKDT256DI8_Vec): TPascalString;
var
  p: PKDT256DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT256DI8.Search(const inBuff: TKDT256DI8_DynamicVecBuffer; var OutBuff: TKDT256DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT256DI8_DynamicVecBuffer;
  outBuffPtr: PKDT256DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT256DI8_Node;
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
        p: PKDT256DI8_Node;
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
  p: PKDT256DI8_Node;
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


procedure TKDT256DI8.Search(const inBuff: TKDT256DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT256DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT256DI8_Node;
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
        p: PKDT256DI8_Node;
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
  p: PKDT256DI8_Node;
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


procedure TKDT256DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT256DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT256DI8_Vec));
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

procedure TKDT256DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT256DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT256DI8_Vec)) <> SizeOf(TKDT256DI8_Vec) then
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

procedure TKDT256DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT256DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT256DI8.PrintNodeTree(const NodePtr: PKDT256DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT256DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT256DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT256DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT256DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT256DI8.KDT256DI8Vec(const s: SystemString): TKDT256DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT256DI8_Axis - 1 do
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
            if j >= KDT256DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT256DI8.KDT256DI8Vec(const v: TKDT256DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT256DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT256DI8.KDT256DI8Pow(const v: TKDT256DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT256DI8.KDT256DI8Distance(const v1, v2: TKDT256DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT256DI8_Axis - 1 do
      Result := Result + KDT256DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT256DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT256DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT256DI8.Test;
var
  TKDT256DI8_Test: TKDT256DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT256DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT256DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT256DI8_Test := TKDT256DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT256DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT256DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT256DI8_Axis - 1 do
        TKDT256DI8_Test.TestBuff[i][j] := i * KDT256DI8_Axis + j;

{$IFDEF FPC}
  TKDT256DI8_Test.BuildKDTreeM(length(TKDT256DI8_Test.TestBuff), nil, @TKDT256DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT256DI8_Test.BuildKDTreeM(length(TKDT256DI8_Test.TestBuff), nil, TKDT256DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT256DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT256DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT256DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT256DI8_Test.Search(TKDT256DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT256DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT256DI8_Test.TestBuff));
      TKDT256DI8_Test.Search(TKDT256DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT256DI8Distance(TKDT256DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT256DI8_Test.Clear;
      { kMean test }
      TKDT256DI8_Test.BuildKDTreeWithCluster(TKDT256DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT256DI8_Test.Search(TKDT256DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT256DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT256DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT256DI8_Test);
end;


function TKDT512DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT512DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT512DI8_Node;
  function SortCompare(const p1, p2: PKDT512DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT512DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT512DI8_Source;
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
  kdBuffPtr: PKDT512DI8_SourceBuffer;
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
      axis := Depth mod KDT512DI8_Axis;
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

function TKDT512DI8.GetData(const Index: NativeInt): PKDT512DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT512DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT512DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT512DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT512DI8_Node(KDNodes[i]));
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

function TKDT512DI8.StoreBuffPtr: PKDT512DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT512DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT512DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT512DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT512DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT512DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT512DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT512DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT512DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT512DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT512DI8.BuildKDTreeWithCluster(const inBuff: TKDT512DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT512DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT512DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT512DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT512DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT512DI8.BuildKDTreeWithCluster(const inBuff: TKDT512DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT512DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT512DI8_BuildCall);
var
  TempStoreBuff: TKDT512DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT512DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT512DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT512DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT512DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT512DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT512DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT512DI8_BuildMethod);
var
  TempStoreBuff: TKDT512DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT512DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT512DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT512DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT512DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT512DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT512DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT512DI8_BuildProc);
var
  TempStoreBuff: TKDT512DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT512DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT512DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT512DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT512DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT512DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT512DI8.Search(const buff: TKDT512DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT512DI8_Node;

var
  NearestNeighbour: PKDT512DI8_Node;

  function FindParentNode(const buffPtr: PKDT512DI8_Vec; NodePtr: PKDT512DI8_Node): PKDT512DI8_Node;
  var
    Next: PKDT512DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT512DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT512DI8_Node; const buffPtr: PKDT512DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT512DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT512DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT512DI8_Vec; const p1, p2: PKDT512DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT512DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT512DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT512DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT512DI8_Node;
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
  Parent: PKDT512DI8_Node;
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

  SearchedDistanceMin := KDT512DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT512DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT512DI8.Search(const buff: TKDT512DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT512DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT512DI8.Search(const buff: TKDT512DI8_Vec; var SearchedDistanceMin: Double): PKDT512DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT512DI8.Search(const buff: TKDT512DI8_Vec): PKDT512DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT512DI8.SearchToken(const buff: TKDT512DI8_Vec): TPascalString;
var
  p: PKDT512DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT512DI8.Search(const inBuff: TKDT512DI8_DynamicVecBuffer; var OutBuff: TKDT512DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT512DI8_DynamicVecBuffer;
  outBuffPtr: PKDT512DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT512DI8_Node;
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
        p: PKDT512DI8_Node;
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
  p: PKDT512DI8_Node;
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


procedure TKDT512DI8.Search(const inBuff: TKDT512DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT512DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT512DI8_Node;
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
        p: PKDT512DI8_Node;
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
  p: PKDT512DI8_Node;
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


procedure TKDT512DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT512DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT512DI8_Vec));
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

procedure TKDT512DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT512DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT512DI8_Vec)) <> SizeOf(TKDT512DI8_Vec) then
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

procedure TKDT512DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT512DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT512DI8.PrintNodeTree(const NodePtr: PKDT512DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT512DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT512DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT512DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT512DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT512DI8.KDT512DI8Vec(const s: SystemString): TKDT512DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT512DI8_Axis - 1 do
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
            if j >= KDT512DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT512DI8.KDT512DI8Vec(const v: TKDT512DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT512DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT512DI8.KDT512DI8Pow(const v: TKDT512DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT512DI8.KDT512DI8Distance(const v1, v2: TKDT512DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT512DI8_Axis - 1 do
      Result := Result + KDT512DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT512DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT512DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT512DI8.Test;
var
  TKDT512DI8_Test: TKDT512DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT512DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT512DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT512DI8_Test := TKDT512DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT512DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT512DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT512DI8_Axis - 1 do
        TKDT512DI8_Test.TestBuff[i][j] := i * KDT512DI8_Axis + j;

{$IFDEF FPC}
  TKDT512DI8_Test.BuildKDTreeM(length(TKDT512DI8_Test.TestBuff), nil, @TKDT512DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT512DI8_Test.BuildKDTreeM(length(TKDT512DI8_Test.TestBuff), nil, TKDT512DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT512DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT512DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT512DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT512DI8_Test.Search(TKDT512DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT512DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT512DI8_Test.TestBuff));
      TKDT512DI8_Test.Search(TKDT512DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT512DI8Distance(TKDT512DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT512DI8_Test.Clear;
      { kMean test }
      TKDT512DI8_Test.BuildKDTreeWithCluster(TKDT512DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT512DI8_Test.Search(TKDT512DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT512DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT512DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT512DI8_Test);
end;


function TKDT1024DI8.InternalBuildKdTree(const KDSourceBufferPtr: PKDT1024DI8_SourceBuffer; const PlanCount, Depth: NativeInt): PKDT1024DI8_Node;
  function SortCompare(const p1, p2: PKDT1024DI8_Source; const axis: NativeInt): ShortInt;
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

  procedure InternalSort(const SortBuffer: PKDT1024DI8_SourceBuffer; L, R: NativeInt; const axis: NativeInt);
  var
    i, j: NativeInt;
    p, t: PKDT1024DI8_Source;
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
  kdBuffPtr: PKDT1024DI8_SourceBuffer;
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
      axis := Depth mod KDT1024DI8_Axis;
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

function TKDT1024DI8.GetData(const Index: NativeInt): PKDT1024DI8_Source;
begin
  Result := @KDStoreBuff[Index];
end;

constructor TKDT1024DI8.Create;
begin
  inherited Create;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  Clear;
end;

destructor TKDT1024DI8.Destroy;
begin
  Clear;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
  inherited Destroy;
end;

procedure TKDT1024DI8.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDT1024DI8_Node(KDNodes[i]));
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

function TKDT1024DI8.StoreBuffPtr: PKDT1024DI8_DyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDT1024DI8.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1024DI8_BuildCall);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT1024DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDT1024DI8.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1024DI8_BuildMethod);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT1024DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

{$IFNDEF FPC}


procedure TKDT1024DI8.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDT1024DI8_BuildProc);
var
  i, j: NativeInt;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      FillPtrByte(@KDStoreBuff[i].buff[0], SizeOf(TKDT1024DI8_Vec), 0);
      OnTrigger(i, KDStoreBuff[i], Data);
      Inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;
{$ENDIF}


{ k-means++ clusterization }
procedure TKDT1024DI8.BuildKDTreeWithCluster(const inBuff: TKDT1024DI8_DynamicVecBuffer; const k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray);
var
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  SetLength(Source, length(inBuff), KDT1024DI8_Axis);
  for i := 0 to length(inBuff) - 1 do
    for j := 0 to KDT1024DI8_Axis - 1 do
        Source[i, j] := inBuff[i, j];

  if KMeansCluster(Source, KDT1024DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1024DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      SetLength(KArray, 0);
    end;

  SetLength(Source, 0);
end;

procedure TKDT1024DI8.BuildKDTreeWithCluster(const inBuff: TKDT1024DI8_DynamicVecBuffer; const k, Restarts: NativeInt);
var
  OutIndex: TDynamicIndexArray;
begin
  BuildKDTreeWithCluster(inBuff, k, Restarts, OutIndex);
  SetLength(OutIndex, 0);
end;

procedure TKDT1024DI8.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1024DI8_BuildCall);
var
  TempStoreBuff: TKDT1024DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT1024DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT1024DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT1024DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT1024DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1024DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDT1024DI8.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1024DI8_BuildMethod);
var
  TempStoreBuff: TKDT1024DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT1024DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT1024DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT1024DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT1024DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1024DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


procedure TKDT1024DI8.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TDynamicIndexArray; const Data: Pointer; const OnTrigger: TKDT1024DI8_BuildProc);
var
  TempStoreBuff: TKDT1024DI8_DyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      FillPtrByte(@TempStoreBuff[i].buff[0], SizeOf(TKDT1024DI8_Vec), 0);
      OnTrigger(i, TempStoreBuff[i], Data);
      Inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), KDT1024DI8_Axis);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to KDT1024DI8_Axis - 1 do
        Source[i, j] := TempStoreBuff[i].buff[j];

  if KMeansCluster(Source, KDT1024DI8_Axis, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];

          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          for j := 0 to KDT1024DI8_Axis - 1 do
              KDStoreBuff[i].buff[j] := KArray[j, i];
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


function TKDT1024DI8.Search(const buff: TKDT1024DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDT1024DI8_Node;

var
  NearestNeighbour: PKDT1024DI8_Node;

  function FindParentNode(const buffPtr: PKDT1024DI8_Vec; NodePtr: PKDT1024DI8_Node): PKDT1024DI8_Node;
  var
    Next: PKDT1024DI8_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod KDT1024DI8_Axis;
        if buffPtr^[axis] > Next^.vec^.buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDT1024DI8_Node; const buffPtr: PKDT1024DI8_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    Inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := KDT1024DI8Distance(buffPtr^, NodePtr^.vec^.buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.vec^.Index < NearestNeighbour^.vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod KDT1024DI8_Axis;
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

  function SortCompare(const buffPtr: PKDT1024DI8_Vec; const p1, p2: PKDT1024DI8_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := KDT1024DI8Distance(buffPtr^, p1^.vec^.buff);
    d2 := KDT1024DI8Distance(buffPtr^, p2^.vec^.buff);
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

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, R: NativeInt; const buffPtr: PKDT1024DI8_Vec);
  var
    i, j: NativeInt;
    p, t: PKDT1024DI8_Node;
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
  Parent: PKDT1024DI8_Node;
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

  SearchedDistanceMin := KDT1024DI8Distance(buff, Parent^.vec^.buff);

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
          Result := PKDT1024DI8_Node(NearestNodes[0]);
    end;
end;

function TKDT1024DI8.Search(const buff: TKDT1024DI8_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDT1024DI8_Node;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDT1024DI8.Search(const buff: TKDT1024DI8_Vec; var SearchedDistanceMin: Double): PKDT1024DI8_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT1024DI8.Search(const buff: TKDT1024DI8_Vec): PKDT1024DI8_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDT1024DI8.SearchToken(const buff: TKDT1024DI8_Vec): TPascalString;
var
  p: PKDT1024DI8_Node;
begin
  p := Search(buff);
  if p <> nil then
      Result := p^.vec^.Token
  else
      Result := '';
end;

procedure TKDT1024DI8.Search(const inBuff: TKDT1024DI8_DynamicVecBuffer; var OutBuff: TKDT1024DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT1024DI8_DynamicVecBuffer;
  outBuffPtr: PKDT1024DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT1024DI8_Node;
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
        p: PKDT1024DI8_Node;
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
  p: PKDT1024DI8_Node;
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


procedure TKDT1024DI8.Search(const inBuff: TKDT1024DI8_DynamicVecBuffer; var OutIndex: TDynamicIndexArray);

{$IFDEF parallel}
var
  inBuffPtr: PKDT1024DI8_DynamicVecBuffer;
  outIndexPtr: PDynamicIndexArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PKDT1024DI8_Node;
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
        p: PKDT1024DI8_Node;
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
  p: PKDT1024DI8_Node;
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


procedure TKDT1024DI8.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := KDT1024DI8_Axis;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);

  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].buff[0], SizeOf(TKDT1024DI8_Vec));
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

procedure TKDT1024DI8.LoadFromStream(stream: TCoreClassStream);
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
  if ID <> KDT1024DI8_Axis then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  i := 0;
  try
    while i < cnt do
      begin
        if stream.read(KDStoreBuff[i].buff[0], SizeOf(TKDT1024DI8_Vec)) <> SizeOf(TKDT1024DI8_Vec) then
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

procedure TKDT1024DI8.SaveToFile(FileName: SystemString);
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

procedure TKDT1024DI8.LoadFromFile(FileName: SystemString);
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

procedure TKDT1024DI8.PrintNodeTree(const NodePtr: PKDT1024DI8_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDT1024DI8_Node);
  begin
    DoStatus('%s +%d (%s) ', [prefix, p^.vec^.Index, KDT1024DI8Vec(p^.vec^.buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDT1024DI8.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d : %s ', [i, KDStoreBuff[i].Index, KDT1024DI8Vec(KDStoreBuff[i].buff)]);
end;

class function TKDT1024DI8.KDT1024DI8Vec(const s: SystemString): TKDT1024DI8_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  i, j: NativeInt;
begin
  for i := 0 to KDT1024DI8_Axis - 1 do
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
            if j >= KDT1024DI8_Axis then
                Break;
          end;
    end;
  DisposeObject(t);
end;

class function TKDT1024DI8.KDT1024DI8Vec(const v: TKDT1024DI8_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to KDT1024DI8_Axis - 1 do
    begin
      if i > 0 then
          Result := Result + ',';
      Result := Result + umlIntToStr(v[i]);
    end;
end;

class function TKDT1024DI8.KDT1024DI8Pow(const v: TKDT1024DI8_VecType): Double;
begin
  Result := v * v;
end;

class function TKDT1024DI8.KDT1024DI8Distance(const v1, v2: TKDT1024DI8_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to KDT1024DI8_Axis - 1 do
      Result := Result + KDT1024DI8Pow(v2[i] - v1[i]);
end;

procedure TKDT1024DI8.Test_BuildM(const IndexFor: NativeInt; var Source: TKDT1024DI8_Source; const Data: Pointer);
begin
  Source.buff := TestBuff[IndexFor];
  Source.Token := umlIntToStr(IndexFor);
end;

class procedure TKDT1024DI8.Test;
var
  TKDT1024DI8_Test: TKDT1024DI8;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultBuff: TKDT1024DI8_DynamicVecBuffer;
  TestResultIndex: TDynamicIndexArray;
  KMeanOutIndex: TDynamicIndexArray;
  errored: Boolean;
  m64: TMemoryStream64;
  p: PKDT1024DI8_Node;
begin
  errored := False;
  DoStatusNoLn('test %s...', [ClassName]);
  t := GetTimeTick;

  DoStatusNoLn('...build');
  TKDT1024DI8_Test := TKDT1024DI8.Create;

  DoStatusNoLn('...');
  SetLength(TKDT1024DI8_Test.TestBuff, 1000);
  for i := 0 to length(TKDT1024DI8_Test.TestBuff) - 1 do
    for j := 0 to KDT1024DI8_Axis - 1 do
        TKDT1024DI8_Test.TestBuff[i][j] := i * KDT1024DI8_Axis + j;

{$IFDEF FPC}
  TKDT1024DI8_Test.BuildKDTreeM(length(TKDT1024DI8_Test.TestBuff), nil, @TKDT1024DI8_Test.Test_BuildM);
{$ELSE FPC}
  TKDT1024DI8_Test.BuildKDTreeM(length(TKDT1024DI8_Test.TestBuff), nil, TKDT1024DI8_Test.Test_BuildM);
{$ENDIF FPC}
  { save/load test }
  DoStatusNoLn('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDT1024DI8_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDT1024DI8_Test.LoadFromStream(m64);
  for i := 0 to length(TKDT1024DI8_Test.TestBuff) - 1 do
    begin
      p := TKDT1024DI8_Test.Search(TKDT1024DI8_Test.TestBuff[i]);
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
      SetLength(TestResultBuff, length(TKDT1024DI8_Test.TestBuff));
      SetLength(TestResultIndex, length(TKDT1024DI8_Test.TestBuff));
      TKDT1024DI8_Test.Search(TKDT1024DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if KDT1024DI8Distance(TKDT1024DI8_Test.TestBuff[TestResultIndex[i]], TestResultBuff[TestResultIndex[i]]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      DoStatusNoLn('...kMean');
      TKDT1024DI8_Test.Clear;
      { kMean test }
      TKDT1024DI8_Test.BuildKDTreeWithCluster(TKDT1024DI8_Test.TestBuff, 10, 1, KMeanOutIndex);
      { parallel search test }
      TKDT1024DI8_Test.Search(TKDT1024DI8_Test.TestBuff, TestResultBuff, TestResultIndex);

      for i := 0 to length(TestResultIndex) - 1 do
        if TestResultIndex[i] <> KMeanOutIndex[i] then
            errored := True;
    end;

  SetLength(TKDT1024DI8_Test.TestBuff, 0);
  SetLength(TestResultBuff, 0);
  SetLength(TestResultIndex, 0);
  SetLength(KMeanOutIndex, 0);
  TKDT1024DI8_Test.Clear;

  DoStatusNoLn('...');
  if errored then
      DoStatusNoLn('error!')
  else
      DoStatusNoLn('passed ok %dms', [GetTimeTick - t]);
  DoStatusNoLn;

  DisposeObject(TKDT1024DI8_Test);
end;


procedure Test_All;
begin
  TKDT1DI8.Test();
  TKDT2DI8.Test();
  TKDT3DI8.Test();
  TKDT4DI8.Test();
  TKDT5DI8.Test();
  TKDT6DI8.Test();
  TKDT7DI8.Test();
  TKDT8DI8.Test();
  TKDT9DI8.Test();
  TKDT10DI8.Test();
  TKDT11DI8.Test();
  TKDT12DI8.Test();
  TKDT13DI8.Test();
  TKDT14DI8.Test();
  TKDT15DI8.Test();
  TKDT16DI8.Test();
  TKDT17DI8.Test();
  TKDT18DI8.Test();
  TKDT19DI8.Test();
  TKDT20DI8.Test();
  TKDT21DI8.Test();
  TKDT22DI8.Test();
  TKDT23DI8.Test();
  TKDT24DI8.Test();
  TKDT256DI8.Test();
  TKDT512DI8.Test();
  TKDT1024DI8.Test();
end;





initialization

finalization

end.

