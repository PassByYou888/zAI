{ ****************************************************************************** }
{ * Dynamic KDTree          writen by QQ 600585@qq.com                         * }
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
unit KDTree;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, PascalStrings,
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF}
  KM, MemoryStream64, Geometry2DUnit, UnicodeMixedLib;

type
  TKDTree_VecType = TKMFloat;
  PKDTree_VecType = PKMFloat;

  TKDTree_Vec = TKMFloatArray;
  PKDTree_Vec = PKMFloatArray;

  TKDTree_Source = record
    Buff: TKDTree_Vec;
    Index: Int64;
    Token: TPascalString;
  end;

  PKDTree_Source = ^TKDTree_Source;

  TKDTree_DynamicVecBuffer = TKMFloat2DArray;
  PKDTree_DynamicVecBuffer = PKMFloat2DArray;

  TKDTree_SourceBuffer = array [0 .. MaxInt div SizeOf(PKDTree_Source) - 1] of PKDTree_Source;
  PKDTree_SourceBuffer = ^TKDTree_SourceBuffer;

  TKDTreeDyanmicSourceBuffer = array of PKDTree_Source;
  PKDTreeDyanmicSourceBuffer = ^TKDTreeDyanmicSourceBuffer;

  TKDTreeDyanmicStoreBuffer = array of TKDTree_Source;
  PKDTreeDyanmicStoreBuffer = ^TKDTreeDyanmicStoreBuffer;

  PKDTree_Node = ^TKDTree_Node;

  TKDTree_Node = record
    Parent, Right, Left: PKDTree_Node;
    Vec: PKDTree_Source;
  end;

  TKDTree = class(TCoreClassObject)
  public type
    TKDTree_BuildCall = procedure(const IndexFor: NativeInt; var Source: TKDTree_Source; const Data: Pointer);
    TKDTree_BuildMethod = procedure(const IndexFor: NativeInt; var Source: TKDTree_Source; const Data: Pointer) of object;
{$IFDEF FPC}
    TKDTree_BuildProc = procedure(const IndexFor: NativeInt; var Source: TKDTree_Source; const Data: Pointer) is nested;
{$ELSE FPC}
    TKDTree_BuildProc = reference to procedure(const IndexFor: NativeInt; var Source: TKDTree_Source; const Data: Pointer);
{$ENDIF FPC}
  private
    FAxisCount: Integer;
    KDStoreBuff: TKDTreeDyanmicStoreBuffer;
    KDBuff: TKDTreeDyanmicSourceBuffer;
    NodeCounter: NativeInt;
    KDNodes: array of PKDTree_Node;
    function InternalBuildKdTree(const KDSourceBufferPtr: PKDTree_SourceBuffer; const PlanCount, Depth: NativeInt): PKDTree_Node;
    function GetData(const Index: NativeInt): PKDTree_Source;
  public
    RootNode: PKDTree_Node;

    constructor Create(const axis: Integer); virtual;
    destructor Destroy; override;

    procedure Clear;

    property Count: NativeInt read NodeCounter;
    function StoreBuffPtr: PKDTreeDyanmicStoreBuffer;
    property SourceP[const index: NativeInt]: PKDTree_Source read GetData; default;
    property AxisCount: Integer read FAxisCount;

    { backcall fill }
    procedure BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDTree_BuildCall);
    procedure BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDTree_BuildMethod);
    procedure BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDTree_BuildProc);
    { backcall k-means++ clusterization }
    procedure BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TKMIntegerArray; const Data: Pointer; const OnTrigger: TKDTree_BuildCall); overload;
    procedure BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TKMIntegerArray; const Data: Pointer; const OnTrigger: TKDTree_BuildMethod); overload;
    procedure BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TKMIntegerArray; const Data: Pointer; const OnTrigger: TKDTree_BuildProc); overload;
    { search }
    function Search(const Buff: TKDTree_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDTree_Node; overload;
    function Search(const Buff: TKDTree_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDTree_Node; overload;
    function Search(const Buff: TKDTree_Vec; var SearchedDistanceMin: Double): PKDTree_Node; overload;
    function Search(const Buff: TKDTree_Vec): PKDTree_Node; overload;
    function SearchToken(const Buff: TKDTree_Vec): TPascalString;
    { Parallel search }
    procedure Search(const inBuff: TKDTree_DynamicVecBuffer; var OutIndex: TKMIntegerArray); overload;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);
    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromFile(FileName: SystemString);
    procedure PrintNodeTree(const NodePtr: PKDTree_Node);
    procedure PrintBuffer;

    class function Vec(const s: SystemString): TKDTree_Vec; overload;
    class function Vec(const v: TKDTree_Vec): SystemString; overload;
    class function Distance(const v1, v2: TKDTree_Vec): Double;
  end;

  TKDTreeData = record
    Vec: TKDTree_Vec;
    Token: SystemString;
  end;

  PKDTreeData = ^TKDTreeData;

  TKDTreeDataList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TKDTreeData>;

  TKDTreeDataList = class(TKDTreeDataList_Decl)
  private
    procedure KDTree_Input(const IndexFor: NativeInt; var Source: TKDTree_Source; const Data: Pointer);
  public
    procedure Build(kd: TKDTree);
    procedure BuildKDTreeWithCluster(kd: TKDTree; classifierNum: NativeInt; ResetSeed_: Boolean);
    function ComputeMaxClassifierVecFromKMeans(classifierNum: NativeInt; ResetSeed_: Boolean): TKDTree_Vec;
    procedure Add(Vec: TKDTree_Vec; Token: SystemString); overload;
    procedure Add(Vec: TVec2); overload;
  end;

procedure Test_KDTree(const axis: Integer);

implementation

uses TextParsing, DoStatusIO;

const
  SaveToken = $9;

function TKDTree.InternalBuildKdTree(const KDSourceBufferPtr: PKDTree_SourceBuffer; const PlanCount, Depth: NativeInt): PKDTree_Node;
  function SortCompare(const p1, p2: PKDTree_Source; const axis: NativeInt): ShortInt; inline;
  begin
    if p1^.Buff[axis] = p2^.Buff[axis] then
      begin
        if p1^.Index = p2^.Index then
            Result := 0
        else if p1^.Index < p2^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if p1^.Buff[axis] < p2^.Buff[axis] then
        Result := -1
    else
        Result := 1;
  end;
  procedure InternalSort(const SortBuffer: PKDTree_SourceBuffer; L, r: NativeInt; const axis: NativeInt); inline;
  var
    i, j: NativeInt;
    p, t: PKDTree_Source;
  begin
    repeat
      i := L;
      j := r;
      p := SortBuffer^[(L + r) shr 1];
      repeat
        while SortCompare(SortBuffer^[i], p, axis) < 0 do
            inc(i);
        while SortCompare(SortBuffer^[j], p, axis) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer^[i];
                SortBuffer^[i] := SortBuffer^[j];
                SortBuffer^[j] := t;
              end;
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, axis);
      L := i;
    until i >= r;
  end;

var
  M: NativeInt;
  axis: NativeInt;
  kdBuffPtr: PKDTree_SourceBuffer;
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
      Result^.Vec := KDSourceBufferPtr^[0];

      KDNodes[NodeCounter] := Result;
      inc(NodeCounter);
    end
  else
    begin
      axis := Depth mod FAxisCount;
      M := PlanCount div 2;

      kdBuffPtr := GetMemory(PlanCount * SizeOf(Pointer));
      CopyPtr(@KDSourceBufferPtr^[0], @kdBuffPtr^[0], PlanCount * SizeOf(Pointer));

      if PlanCount > 1 then
          InternalSort(@kdBuffPtr^[0], 0, PlanCount - 1, axis);

      new(Result);
      Result^.Parent := nil;
      Result^.Vec := kdBuffPtr^[M];

      KDNodes[NodeCounter] := Result;
      inc(NodeCounter);

      Result^.Left := InternalBuildKdTree(@kdBuffPtr^[0], M, Depth + 1);
      if Result^.Left <> nil then
          Result^.Left^.Parent := Result;

      Result^.Right := InternalBuildKdTree(@kdBuffPtr^[M + 1], PlanCount - (M + 1), Depth + 1);
      if Result^.Right <> nil then
          Result^.Right^.Parent := Result;

      FreeMemory(kdBuffPtr);
    end;
end;

function TKDTree.GetData(const Index: NativeInt): PKDTree_Source;
begin
  Result := @KDStoreBuff[index];
end;

constructor TKDTree.Create(const axis: Integer);
begin
  inherited Create;
  FAxisCount := axis;
  NodeCounter := 0;
  RootNode := nil;
  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);
end;

destructor TKDTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TKDTree.Clear;
var
  i: NativeInt;
begin
  i := 0;
  while i < length(KDNodes) do
    begin
      Dispose(PKDTree_Node(KDNodes[i]));
      inc(i);
    end;
  for i := 0 to length(KDStoreBuff) - 1 do
    begin
      KDStoreBuff[i].Token := '';
      SetLength(KDStoreBuff[i].Buff, 0);
    end;

  SetLength(KDNodes, 0);
  SetLength(KDStoreBuff, 0);
  SetLength(KDBuff, 0);

  NodeCounter := 0;
  RootNode := nil;
end;

function TKDTree.StoreBuffPtr: PKDTreeDyanmicStoreBuffer;
begin
  Result := @KDStoreBuff;
end;

procedure TKDTree.BuildKDTreeC(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDTree_BuildCall);
var
  i: NativeInt;
  j: Integer;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      SetLength(KDStoreBuff[i].Buff, AxisCount);
      for j := 0 to AxisCount - 1 do
          KDStoreBuff[i].Buff[j] := 0;
      OnTrigger(i, KDStoreBuff[i], Data);
      inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDTree.BuildKDTreeM(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDTree_BuildMethod);
var
  i: NativeInt;
  j: Integer;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      SetLength(KDStoreBuff[i].Buff, AxisCount);
      for j := 0 to AxisCount - 1 do
          KDStoreBuff[i].Buff[j] := 0;
      OnTrigger(i, KDStoreBuff[i], Data);
      inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDTree.BuildKDTreeP(const PlanCount: NativeInt; const Data: Pointer; const OnTrigger: TKDTree_BuildProc);
var
  i: NativeInt;
  j: Integer;
begin
  Clear;

  if PlanCount <= 0 then
      Exit;

  SetLength(KDStoreBuff, PlanCount);
  SetLength(KDBuff, PlanCount);
  SetLength(KDNodes, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      KDBuff[i] := @KDStoreBuff[i];
      KDStoreBuff[i].Index := i;
      KDStoreBuff[i].Token := '';
      SetLength(KDStoreBuff[i].Buff, AxisCount);
      for j := 0 to AxisCount - 1 do
          KDStoreBuff[i].Buff[j] := 0;
      OnTrigger(i, KDStoreBuff[i], Data);
      inc(i);
    end;

  j := PlanCount;

  RootNode := InternalBuildKdTree(@KDBuff[0], j, 0);
end;

procedure TKDTree.BuildKDTreeWithClusterC(const PlanCount, k, Restarts: NativeInt; var OutIndex: TKMIntegerArray; const Data: Pointer; const OnTrigger: TKDTree_BuildCall);
var
  TempStoreBuff: TKDTreeDyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      SetLength(TempStoreBuff[i].Buff, AxisCount);
      for j := 0 to AxisCount - 1 do
          TempStoreBuff[i].Buff[j] := 0;
      OnTrigger(i, TempStoreBuff[i], Data);
      inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), FAxisCount);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to FAxisCount - 1 do
        Source[i, j] := TempStoreBuff[i].Buff[j];

  if KMeansCluster(Source, FAxisCount, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          SetLength(KDStoreBuff[i].Buff, AxisCount);
          for j := 0 to FAxisCount - 1 do
              KDStoreBuff[i].Buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  for i := 0 to length(TempStoreBuff) - 1 do
      SetLength(TempStoreBuff[i].Buff, 0);
  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDTree.BuildKDTreeWithClusterM(const PlanCount, k, Restarts: NativeInt; var OutIndex: TKMIntegerArray; const Data: Pointer; const OnTrigger: TKDTree_BuildMethod);
var
  TempStoreBuff: TKDTreeDyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      SetLength(TempStoreBuff[i].Buff, AxisCount);
      for j := 0 to AxisCount - 1 do
          TempStoreBuff[i].Buff[j] := 0;
      OnTrigger(i, TempStoreBuff[i], Data);
      inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), FAxisCount);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to FAxisCount - 1 do
        Source[i, j] := TempStoreBuff[i].Buff[j];

  if KMeansCluster(Source, FAxisCount, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          SetLength(KDStoreBuff[i].Buff, AxisCount);
          for j := 0 to FAxisCount - 1 do
              KDStoreBuff[i].Buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  for i := 0 to length(TempStoreBuff) - 1 do
      SetLength(TempStoreBuff[i].Buff, 0);
  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

procedure TKDTree.BuildKDTreeWithClusterP(const PlanCount, k, Restarts: NativeInt; var OutIndex: TKMIntegerArray; const Data: Pointer; const OnTrigger: TKDTree_BuildProc);
var
  TempStoreBuff: TKDTreeDyanmicStoreBuffer;
  Source: TKMFloat2DArray;
  KArray: TKMFloat2DArray;
  i, j: NativeInt;
begin
  Clear;
  SetLength(TempStoreBuff, PlanCount);

  i := 0;
  while i < PlanCount do
    begin
      TempStoreBuff[i].Index := i;
      TempStoreBuff[i].Token := '';
      SetLength(TempStoreBuff[i].Buff, AxisCount);
      for j := 0 to AxisCount - 1 do
          TempStoreBuff[i].Buff[j] := 0;
      OnTrigger(i, TempStoreBuff[i], Data);
      inc(i);
    end;

  SetLength(Source, length(TempStoreBuff), FAxisCount);
  for i := 0 to length(TempStoreBuff) - 1 do
    for j := 0 to FAxisCount - 1 do
        Source[i, j] := TempStoreBuff[i].Buff[j];

  if KMeansCluster(Source, FAxisCount, k, umlMax(Restarts, 1), KArray, OutIndex) = 1 then
    begin
      SetLength(KDStoreBuff, k);
      SetLength(KDBuff, k);
      SetLength(KDNodes, k);

      for i := 0 to k - 1 do
        begin
          KDBuff[i] := @KDStoreBuff[i];
          KDStoreBuff[i].Index := i;
          KDStoreBuff[i].Token := '';
          SetLength(KDStoreBuff[i].Buff, AxisCount);
          for j := 0 to FAxisCount - 1 do
              KDStoreBuff[i].Buff[j] := KArray[j, i];
        end;

      RootNode := InternalBuildKdTree(@KDBuff[0], k, 0);

      for i := 0 to length(OutIndex) - 1 do
          OutIndex[i] := TempStoreBuff[OutIndex[i]].Index;

      SetLength(KArray, 0);
    end;

  for i := 0 to length(TempStoreBuff) - 1 do
      SetLength(TempStoreBuff[i].Buff, 0);
  SetLength(TempStoreBuff, 0);
  SetLength(Source, 0);
end;

function TKDTree.Search(const Buff: TKDTree_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt; const NearestNodes: TCoreClassList): PKDTree_Node;

var
  NearestNeighbour: PKDTree_Node;

  function FindParentNode(const buffPtr: PKDTree_Vec; const NodePtr: PKDTree_Node): PKDTree_Node;
  var
    Next: PKDTree_Node;
    Depth, axis: NativeInt;
  begin
    Result := nil;
    Depth := 0;
    Next := NodePtr;
    while Next <> nil do
      begin
        Result := Next;
        axis := Depth mod FAxisCount;
        if buffPtr^[axis] > Next^.Vec^.Buff[axis] then
            Next := Next^.Right
        else
            Next := Next^.Left;
        Depth := Depth + 1;
      end;
  end;

  procedure ScanSubtree(const NodePtr: PKDTree_Node; const buffPtr: PKDTree_Vec; const Depth: NativeInt; const NearestNodes: TCoreClassList);
  var
    Dist: Double;
    axis: NativeInt;
  begin
    if NodePtr = nil then
        Exit;

    inc(SearchedCounter);

    if NearestNodes <> nil then
        NearestNodes.Add(NodePtr);

    Dist := Distance(buffPtr^, NodePtr^.Vec^.Buff);
    if Dist < SearchedDistanceMin then
      begin
        SearchedDistanceMin := Dist;
        NearestNeighbour := NodePtr;
      end
    else if (Dist = SearchedDistanceMin) and (NodePtr^.Vec^.Index < NearestNeighbour^.Vec^.Index) then
        NearestNeighbour := NodePtr;

    axis := Depth mod FAxisCount;
    Dist := NodePtr^.Vec^.Buff[axis] - buffPtr^[axis];

    if Dist * Dist > SearchedDistanceMin then
      begin
        if NodePtr^.Vec^.Buff[axis] > buffPtr^[axis] then
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

  function SortCompare(const buffPtr: PKDTree_Vec; const p1, p2: PKDTree_Node): ShortInt;
  var
    d1, d2: Double;
  begin
    d1 := Distance(buffPtr^, p1^.Vec^.Buff);
    d2 := Distance(buffPtr^, p2^.Vec^.Buff);
    if d1 = d2 then
      begin
        if p1^.Vec^.Index = p2^.Vec^.Index then
            Result := 0
        else if p1^.Vec^.Index < p2^.Vec^.Index then
            Result := -1
        else
            Result := 1;
      end
    else if d1 < d2 then
        Result := -1
    else
        Result := 1;
  end;

  procedure InternalSort(var SortBuffer: TCoreClassPointerList; L, r: NativeInt; const buffPtr: PKDTree_Vec);
  var
    i, j: NativeInt;
    p, t: PKDTree_Node;
  begin
    repeat
      i := L;
      j := r;
      p := SortBuffer[(L + r) shr 1];
      repeat
        while SortCompare(buffPtr, SortBuffer[i], p) < 0 do
            inc(i);
        while SortCompare(buffPtr, SortBuffer[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j, buffPtr);
      L := i;
    until i >= r;
  end;

var
  Parent: PKDTree_Node;
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
  if length(Buff) <> FAxisCount then
      Exit;

  Parent := FindParentNode(@Buff, RootNode);
  NearestNeighbour := Parent;

  SearchedDistanceMin := Distance(Buff, Parent^.Vec^.Buff);

  ScanSubtree(RootNode, @Buff, 0, NearestNodes);
  if NearestNeighbour = nil then
      NearestNeighbour := RootNode;
  Result := NearestNeighbour;

  if NearestNodes <> nil then
    begin
      Result := NearestNeighbour;
      if NearestNodes.Count > 1 then
          InternalSort(NearestNodes.ListData^, 0, NearestNodes.Count - 1, @Buff);

      if NearestNodes.Count > 0 then
          Result := PKDTree_Node(NearestNodes[0]);
    end;
end;

function TKDTree.Search(const Buff: TKDTree_Vec; var SearchedDistanceMin: Double; var SearchedCounter: NativeInt): PKDTree_Node;
begin
  Result := Search(Buff, SearchedDistanceMin, SearchedCounter, nil);
end;

function TKDTree.Search(const Buff: TKDTree_Vec; var SearchedDistanceMin: Double): PKDTree_Node;
var
  SearchedCounter: NativeInt;
begin
  Result := Search(Buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDTree.Search(const Buff: TKDTree_Vec): PKDTree_Node;
var
  SearchedDistanceMin: Double;
  SearchedCounter: NativeInt;
begin
  Result := Search(Buff, SearchedDistanceMin, SearchedCounter);
end;

function TKDTree.SearchToken(const Buff: TKDTree_Vec): TPascalString;
var
  p: PKDTree_Node;
begin
  p := Search(Buff);
  if p <> nil then
      Result := p^.Vec^.Token
  else
      Result := '';
end;

procedure TKDTree.Search(const inBuff: TKDTree_DynamicVecBuffer; var OutIndex: TKMIntegerArray);

{$IFDEF Parallel}
var
  inBuffPtr: PKDTree_DynamicVecBuffer;
  outIndexPtr: PKMIntegerArray;

{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: Integer);
  var
    p: PKDTree_Node;
  begin
    p := Search(inBuffPtr^[pass]);
    outIndexPtr^[pass] := p^.Vec^.Index;
  end;
{$ENDIF FPC}


begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  inBuffPtr := @inBuff;
  outIndexPtr := @OutIndex;

  GlobalMemoryHook.v := False;
  try
{$IFDEF FPC}
    FPCParallelFor(@FPC_ParallelFor, 0, length(inBuff) - 1);
{$ELSE FPC}
    DelphiParallelFor(Int64(0), Int64(length(inBuff) - 1),
      procedure(pass: Int64)
      var
        p: PKDTree_Node;
      begin
        p := Search(inBuffPtr^[pass]);
        outIndexPtr^[pass] := p^.Vec^.Index;
      end);
{$ENDIF FPC}
  finally
      GlobalMemoryHook.v := True;
  end;
end;

{$ELSE Parallel}


var
  i: NativeInt;
  p: PKDTree_Node;
begin
  if length(inBuff) <> length(OutIndex) then
      Exit;

  for i := 0 to length(inBuff) - 1 do
    begin
      p := Search(inBuff[i]);
      OutIndex[i] := p^.Vec^.Index;
    end;
end;
{$ENDIF Parallel}


procedure TKDTree.SaveToStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  buff_L: Integer;
  token_B: TBytes;
  token_L: Integer;
begin
  cnt := length(KDStoreBuff);
  st := SaveToken;
  ID := FAxisCount;

  stream.write(st, 4);
  stream.write(ID, 4);

  stream.write(cnt, 8);
  buff_L := FAxisCount * SizeOf(TKDTree_VecType);
  i := 0;
  while i < cnt do
    begin
      stream.write(KDStoreBuff[i].Buff[0], buff_L);
      stream.write(KDStoreBuff[i].Index, 8);
      token_B := KDStoreBuff[i].Token.Bytes;
      token_L := length(token_B);
      stream.write(token_L, 4);
      if token_L > 0 then
        begin
          stream.write(token_B[0], token_L);
          SetLength(token_B, 0);
        end;
      inc(i);
    end;
end;

procedure TKDTree.LoadFromStream(stream: TCoreClassStream);
var
  cnt: Int64;
  st, ID: Integer;
  i: NativeInt;
  buff_L: Integer;
  token_B: TBytes;
  token_L: Integer;
begin
  Clear;

  stream.read(st, 4);
  stream.read(ID, 4);

  if st <> SaveToken then
      RaiseInfo('kdtree token error!');
  if ID <> FAxisCount then
      RaiseInfo('kdtree axis error!');

  stream.read(cnt, 8);

  SetLength(KDStoreBuff, cnt);

  buff_L := FAxisCount * SizeOf(TKDTree_VecType);

  i := 0;
  try
    while i < cnt do
      begin
        SetLength(KDStoreBuff[i].Buff, FAxisCount);
        if stream.read(KDStoreBuff[i].Buff[0], buff_L) <> buff_L then
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
        inc(i);
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
      inc(i);
    end;

  if cnt > 0 then
      RootNode := InternalBuildKdTree(@KDBuff[0], cnt, 0);
end;

procedure TKDTree.SaveToFile(FileName: SystemString);
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

procedure TKDTree.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TKDTree.PrintNodeTree(const NodePtr: PKDTree_Node);
  procedure DoPrintNode(prefix: SystemString; const p: PKDTree_Node);
  begin
    DoStatus('%s + %d (%s) ', [prefix, p^.Vec^.Index, Vec(p^.Vec^.Buff)]);
    if p^.Left <> nil then
        DoPrintNode(prefix + ' |-----', p^.Left);
    if p^.Right <> nil then
        DoPrintNode(prefix + ' |-----', p^.Right);
  end;

begin
  DoPrintNode('', NodePtr);
end;

procedure TKDTree.PrintBuffer;
var
  i: NativeInt;
begin
  for i := 0 to length(KDStoreBuff) - 1 do
      DoStatus('%d - %d: %s ', [i, KDStoreBuff[i].Index, Vec(KDStoreBuff[i].Buff)]);
end;

class function TKDTree.Vec(const s: SystemString): TKDTree_Vec;
var
  t: TTextParsing;
  SplitOutput: TArrayPascalString;
  c, i, j: NativeInt;
begin
  t := TTextParsing.Create(s, tsText, nil);
  c := t.SplitChar(1, ', ', '', SplitOutput);
  if c > 0 then
    begin
      SetLength(Result, c);

      j := 0;
      for i := 0 to length(SplitOutput) - 1 do
        if umlGetNumTextType(SplitOutput[i]) <> ntUnknow then
          begin
            Result[j] := umlStrToFloat(SplitOutput[i], 0);
            inc(j);
          end;
    end;
  DisposeObject(t);
end;

class function TKDTree.Vec(const v: TKDTree_Vec): SystemString;
var
  i: NativeInt;
begin
  Result := '';
  for i := 0 to length(v) - 1 do
    begin
      if i > 0 then
          Result := Result + ' ';
      Result := Result + umlFloatToStr(v[i]);
    end;
end;

class function TKDTree.Distance(const v1, v2: TKDTree_Vec): Double;
var
  i: NativeInt;
begin
  Result := 0;
  for i := 0 to length(v1) - 1 do
      Result := Result + (v2[i] - v1[i]) * (v2[i] - v1[i]);
end;

procedure Test_BuildC(const IndexFor: NativeInt; var Source: TKDTree_Source; const Data: Pointer);
var
  i: Integer;
begin
  for i := 0 to length(Source.Buff) - 1 do
      Source.Buff[i] := PKDTree_DynamicVecBuffer(Data)^[IndexFor][i];
  Source.Token := umlIntToStr(IndexFor);
end;

procedure Test_KDTree(const axis: Integer);
var
  TKDTree_Test: TKDTree;
  t: TTimeTick;
  i, j: NativeInt;
  TestResultIndex: TKMIntegerArray;
  KMeanBuildOutIndex: TKMIntegerArray;
  errored: Boolean;
  TestBuff: TKDTree_DynamicVecBuffer;
  m64: TMemoryStream64;
  p: PKDTree_Node;
  n: TPascalString;
begin
  errored := False;
  n := 'test KDTree';
  t := GetTimeTick;

  n.Append('...build');
  TKDTree_Test := TKDTree.Create(axis);

  SetLength(TestBuff, 1000);
  for i := 0 to length(TestBuff) - 1 do
    begin
      SetLength(TestBuff[i], TKDTree_Test.AxisCount);
      for j := 0 to TKDTree_Test.AxisCount - 1 do
          TestBuff[i][j] := i + 1;
    end;

{$IFDEF FPC}
  TKDTree_Test.BuildKDTreeC(length(TestBuff), @TestBuff, @Test_BuildC);
{$ELSE FPC}
  TKDTree_Test.BuildKDTreeC(length(TestBuff), @TestBuff, Test_BuildC);
{$ENDIF FPC}
  { save/load test }
  n.Append('...save/load');
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  TKDTree_Test.SaveToStream(m64);
  m64.Position := 0;
  TKDTree_Test.LoadFromStream(m64);
  for i := 0 to length(TestBuff) - 1 do
    begin
      p := TKDTree_Test.Search(TestBuff[i]);
      if p^.Vec^.Index <> i then
          errored := True;
      if not p^.Vec^.Token.Same(umlIntToStr(i)) then
          errored := True;
      if errored then
          break;
    end;
  DisposeObject(m64);

  if not errored then
    begin
      { Parallel search test }
      n.Append('...Parallel');
      SetLength(TestResultIndex, length(TestBuff));
      TKDTree_Test.Search(TestBuff, TestResultIndex);
      for i := 0 to length(TestResultIndex) - 1 do
        if TKDTree.Distance(TestBuff[TestResultIndex[i]], TestBuff[i]) <> 0 then
            errored := True;
    end;

  if not errored then
    begin
      n.Append('...kMean');
      TKDTree_Test.Clear;
      { kMean test }
{$IFDEF FPC}
      TKDTree_Test.BuildKDTreeWithClusterC(length(TestBuff), 10, 1, KMeanBuildOutIndex, @TestBuff, @Test_BuildC);
{$ELSE FPC}
      TKDTree_Test.BuildKDTreeWithClusterC(length(TestBuff), 10, 1, KMeanBuildOutIndex, @TestBuff, Test_BuildC);
{$ENDIF FPC}
      { Parallel search test }
      TKDTree_Test.Search(TestBuff, TestResultIndex);
      for i := 0 to length(TestResultIndex) - 1 do
        if KMeanBuildOutIndex[i] <> TestResultIndex[i] then
            errored := True;
    end;

  for i := 0 to length(TestBuff) - 1 do
      SetLength(TestBuff[i], 0);

  SetLength(TestBuff, 0);
  SetLength(TestResultIndex, 0);
  TKDTree_Test.Clear;

  n.Append('...');
  if errored then
      n.Append('error!')
  else
      n.Append('passed ok %dms', [GetTimeTick - t]);
  DisposeObject(TKDTree_Test);
  DoStatus(n);
  n := '';
end;

procedure TKDTreeDataList.KDTree_Input(const IndexFor: NativeInt; var Source: TKDTree_Source; const Data: Pointer);
var
  i: Integer;
  v: TKDTreeData;
begin
  v := Items[IndexFor];
  for i := 0 to length(v.Vec) - 1 do
      Source.Buff[i] := v.Vec[i];
  Source.Token := v.Token;
  Source.Index := IndexFor;
end;

procedure TKDTreeDataList.Build(kd: TKDTree);
begin
  kd.BuildKDTreeM(Count, nil, {$IFDEF FPC}@{$ENDIF FPC}KDTree_Input);
end;

procedure TKDTreeDataList.BuildKDTreeWithCluster(kd: TKDTree; classifierNum: NativeInt; ResetSeed_: Boolean);
var
  kmIndexOut: TKMIntegerArray;
begin
  if ResetSeed_ then
      SetMT19937Seed(0);

  kd.BuildKDTreeWithClusterM(Count, classifierNum, 1, kmIndexOut, nil, {$IFDEF FPC}@{$ENDIF FPC}KDTree_Input);
end;

function TKDTreeDataList.ComputeMaxClassifierVecFromKMeans(classifierNum: NativeInt; ResetSeed_: Boolean): TKDTree_Vec;
var
  kd: TKDTree;
  numVec: TKMIntegerArray;
  i, maxI: Integer;
  Buff: TKDTree_Vec;
begin
  SetLength(Result, 0);
  if Count = 0 then
      Exit;
  kd := TKDTree.Create(length(First.Vec));
  BuildKDTreeWithCluster(kd, classifierNum, ResetSeed_);

  SetLength(numVec, kd.Count);
  for i := 0 to Count - 1 do
      inc(numVec[kd.Search(Items[i].Vec)^.Vec^.Index]);

  maxI := 0;
  for i := 1 to length(numVec) - 1 do
    if numVec[i] > numVec[maxI] then
        maxI := i;

  Buff := kd.SourceP[maxI]^.Buff;
  SetLength(Result, length(Buff));
  for i := 0 to length(Buff) - 1 do
      Result[i] := Buff[i];
  DisposeObject(kd);
end;

procedure TKDTreeDataList.Add(Vec: TKDTree_Vec; Token: SystemString);
var
  d: TKDTreeData;
begin
  d.Vec := Vec;
  d.Token := Token;
  inherited Add(d);
end;

procedure TKDTreeDataList.Add(Vec: TVec2);
var
  v: TKDTree_Vec;
begin
  SetLength(v, 2);
  v[0] := Vec[0];
  v[1] := Vec[1];
  Add(v, '');
end;

end.
