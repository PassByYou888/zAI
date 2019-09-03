unit Expret_Node;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, DataFrameEngine, ListEngine, PascalStrings;

type
  TENode_Base = class;

  TENodeBaseClass = class of TENode_Base;

  TENodeEvent = procedure(Sender: TENode_Base);
  TENodeProgress = procedure(Sender: TENode_Base; deltaTime: Double);
  TNodeInitedStates = set of (nisInstalled, nisLoaded);

  TENode_Base = class(TCoreClassPersistent)
  protected
    FList: TCoreClassListForObj;
    FOwner: TENode_Base;
    FID: Integer;
    FText: SystemString;
    FData: TCoreClassObject;

    FOnDestroy: TENodeEvent;
    FOnLoaded: TENodeEvent;
    FOnProgress: TENodeProgress;

    FPushStateList: TCoreClassListForObj;
    FFirstOpenState, FLastLoadState: TDataFrameEngine;

    FInternalInitedStates: TNodeInitedStates;
    FAcceptProgress: Boolean;

    function GetItems(index: Integer): TENode_Base; virtual;
    function GetItemsID(AID: Integer): TENode_Base; virtual;
  public
    ClickCounter: Cardinal;
    constructor Create(AOwner: TENode_Base); virtual;
    destructor Destroy; override;

    property Text: SystemString read FText write FText;
    function ROOT: TENode_Base;
    function Path: SystemString;
    function FromPath(ph: SystemString): TENode_Base;
    function TranslationPath(ph: SystemString): SystemString;

    function GetRegistedName: SystemString;

    function NewNode(NC: TENodeBaseClass): TENode_Base;
    procedure Add(n: TENode_Base);
    procedure Delete(idx: Integer);
    function Count: Integer;
    procedure Clear;

    procedure Progress(deltaTime: Double); virtual;

    procedure LoadFromCompiledDataFrame(DataFrame: TDataFrameEngine); virtual;

    procedure SaveState(DataFrame: TDataFrameEngine); virtual;
    procedure LoadState(DataFrame: TDataFrameEngine); virtual;

    procedure PushState;
    procedure PopState;
    procedure ReductionToFirstOpenState;
    procedure ReductionToLastLoadState;

    procedure Install; virtual;
    procedure FullInstall;

    procedure Loaded;
    procedure FullLoaded;

    property ItemsIndex[index: Integer]: TENode_Base read GetItems; default;
    property ItemsID[AID: Integer]: TENode_Base read GetItemsID;

    function ID: Integer;
    function Owner: TENode_Base;

    property Data: TCoreClassObject read FData write FData;

    property AcceptProgress: Boolean read FAcceptProgress write FAcceptProgress;
    procedure ResetAcceptProgressState(v: Boolean);
  end;

  TENode = class(TENode_Base)
  end;

  TENode_Root = class(TENode_Base)
  protected
    FEntryInteractiveNode: SystemString;
  public
    constructor Create(AOwner: TENode_Base); override;
    destructor Destroy; override;
    procedure LoadFromCompiledDataFrame(DataFrame: TDataFrameEngine); override;
    procedure SetNodeRootTriggerIntf(ADestroy, ALoaded: TENodeEvent; AProgress: TENodeProgress);
    property EntryInteractiveNode: SystemString read FEntryInteractiveNode write FEntryInteractiveNode;
  end;

  TENode_Interactive = class(TENode)
  protected
    FOnInHere: TENodeEvent;
    FOnOutHere: TENodeEvent;

    FVisibledTitleText: Boolean;
    FOriginText: TCoreClassStrings;

    FOriginTextTable: TCoreClassListForObj;
    FActiveOriginTextIndex: Integer;

    procedure SetActiveOriginTextIndex(const Value: Integer);
  public
    constructor Create(AOwner: TENode_Base); override;
    destructor Destroy; override;

    procedure LoadFromCompiledDataFrame(DataFrame: TDataFrameEngine); override;

    procedure SaveState(DataFrame: TDataFrameEngine); override;
    procedure LoadState(DataFrame: TDataFrameEngine); override;

    procedure GetInteractiveSubNodeList(output: TCoreClassListForObj);

    // interface operation
    procedure InHere;
    procedure OutHere;

    property VisibledTitleText: Boolean read FVisibledTitleText write FVisibledTitleText;
    property OriginText: TCoreClassStrings read FOriginText;

    property OriginTextTable: TCoreClassListForObj read FOriginTextTable;
    property ActiveOriginTextIndex: Integer read FActiveOriginTextIndex write SetActiveOriginTextIndex;
  end;

  TENode_InteractiveChildren = class(TENode)
  protected
    FOnJump: TENodeEvent;
    FDetail: SystemString;
    FUsesPopupJump: Boolean;
    FJumpInteractiveNode: SystemString;
    FVisible: Boolean;

    FUsedPlanJump: Boolean;
    FPlanJumpTable: TCoreClassStrings;
    FActivePlanIndex: Integer;
  public
    constructor Create(AOwner: TENode_Base); override;
    destructor Destroy; override;

    procedure LoadFromCompiledDataFrame(DataFrame: TDataFrameEngine); override;

    procedure SaveState(DataFrame: TDataFrameEngine); override;
    procedure LoadState(DataFrame: TDataFrameEngine); override;

    function GetOwnerInteractiveNode: TENode_Interactive;

    // interface operation
    function DoJumpInteractiveNode: TENode_Interactive;

    property Visible: Boolean read FVisible write FVisible;

    function GetJumpInteractiveNode: TENode_Interactive;

    property detail: SystemString read FDetail write FDetail;
    property UsesPopupJump: Boolean read FUsesPopupJump write FUsesPopupJump;

    property JumpInteractiveNode: SystemString read FJumpInteractiveNode write FJumpInteractiveNode;

    property UsedPlanJump: Boolean read FUsedPlanJump write FUsedPlanJump;
    property PlanJumpTable: TCoreClassStrings read FPlanJumpTable;
    property ActivePlanIndex: Integer read FActivePlanIndex write FActivePlanIndex;
  end;

  TENode_PopupBackEvent = procedure(const PopupBackPath: SystemString);

  TENode_PopupBack = class(TENode)
  protected
    FOnJump: TENode_PopupBackEvent;
    FVisible: Boolean;
  public
    constructor Create(AOwner: TENode_Base); override;
    destructor Destroy; override;

    procedure LoadFromCompiledDataFrame(DataFrame: TDataFrameEngine); override;

    procedure SaveState(DataFrame: TDataFrameEngine); override;
    procedure LoadState(DataFrame: TDataFrameEngine); override;

    function GetOwnerInteractiveNode: TENode_Interactive;
    procedure DoPopupBack(n: TENode_Interactive);

    property Visible: Boolean read FVisible write FVisible;
  end;

  TENode_Introduce = class;

  TENode_Container = class(TENode)
  protected
    FIntroduceSource: TENode_Introduce;
  public
    constructor Create(AOwner: TENode_Base); override;
    destructor Destroy; override;

    procedure SaveState(DataFrame: TDataFrameEngine); override;
    procedure LoadState(DataFrame: TDataFrameEngine); override;

    function GetIntroduceSourceInteractiveNode: TENode_Interactive;
    procedure GetInteractiveSubNodeList(output: TCoreClassListForObj);

    property IntroduceSource: TENode_Introduce read FIntroduceSource write FIntroduceSource;
  end;

  TENode_Introduce = class(TENode)
  protected
    FIntroduceNodePath: SystemString;
  public
    constructor Create(AOwner: TENode_Base); override;
    destructor Destroy; override;

    procedure SaveState(DataFrame: TDataFrameEngine); override;
    procedure LoadState(DataFrame: TDataFrameEngine); override;

    procedure LoadFromCompiledDataFrame(DataFrame: TDataFrameEngine); override;
    function GetOwnerInteractiveNode: TENode_Interactive;
    procedure GetInteractiveSubNodeList(output: TCoreClassListForObj);

    property IntroduceNodePath: SystemString read FIntroduceNodePath write FIntroduceNodePath;
  end;

  TRegistedENodeClassStruct = record
    EngineContextClass: TENodeBaseClass;
    Name: SystemString;
  end;

  PRegistedENodeClassStruct = ^TRegistedENodeClassStruct;

function RegistedENodeClassList: TCoreClassList;
procedure RegisterENodeClass(cName: SystemString; EngineContextClass: TENodeBaseClass); overload;
function GetENodeClass(cName: SystemString): PRegistedENodeClassStruct;
function GetENodeName(EngineContextClass: TClass): SystemString;

function BuildENodeFromCompiledStream(const stream: TCoreClassStream): TENode_Base;

procedure SaveENodeState(output: TCoreClassStream; RootNode: TENode_Base);
procedure LoadENodeState(stream: TCoreClassStream; RootNode: TENode_Base);

function GetNodeCount(const n: TENode_Base): Integer;

implementation

uses UnicodeMixedLib, DoStatusIO;

function GetNodeCount(const n: TENode_Base): Integer;
var
  i: Integer;
begin
  Result := n.Count;
  for i := 0 to n.Count - 1 do
      Result := Result + GetNodeCount(n[i]);
end;

var
  _RegistedENodeClassList: TCoreClassList = nil;

function RegistedENodeClassList: TCoreClassList;
begin
  if _RegistedENodeClassList = nil then
      _RegistedENodeClassList := TCoreClassList.Create;
  Result := _RegistedENodeClassList;
end;

procedure _ClearRegistedEngineContextClassList;
var
  i: Integer;
  p: PRegistedENodeClassStruct;
begin
  if RegistedENodeClassList.Count > 0 then
    for i := 0 to _RegistedENodeClassList.Count - 1 do
      begin
        p := _RegistedENodeClassList[i];
        Dispose(p);
      end;
  DisposeObject(_RegistedENodeClassList);
  _RegistedENodeClassList := nil;
end;

procedure RegisterENodeClass(cName: SystemString; EngineContextClass: TENodeBaseClass);
var
  p: PRegistedENodeClassStruct;
begin
  if GetENodeClass(cName) <> nil then
      RaiseInfo('%s already registed!', [cName]);
  new(p);
  p^.Name := cName;
  p^.EngineContextClass := EngineContextClass;
  RegistedENodeClassList.Add(p);
end;

function GetENodeClass(cName: SystemString): PRegistedENodeClassStruct;
var
  i: Integer;
  p: PRegistedENodeClassStruct;
begin
  Result := nil;
  if RegistedENodeClassList.Count > 0 then
    for i := 0 to _RegistedENodeClassList.Count - 1 do
      begin
        p := _RegistedENodeClassList[i];

        if umlSameText(p^.Name, cName) then
            Exit(p);
      end;
end;

function GetENodeName(EngineContextClass: TClass): SystemString;
var
  i: Integer;
  p: PRegistedENodeClassStruct;
begin
  Result := '';
  if RegistedENodeClassList.Count > 0 then
    for i := 0 to _RegistedENodeClassList.Count - 1 do
      begin
        p := _RegistedENodeClassList[i];
        if p^.EngineContextClass = EngineContextClass then
            Exit(p^.Name);
      end;
end;

function BuildENodeFromCompiledStream(const stream: TCoreClassStream): TENode_Base;
  function __LoadNodeFromBuild(const AOwnerNode: TENode_Base; const df: TDataFrameEngine): TENode_Base;
  var
    clName: SystemString;
    cnt, i: Integer;
    p: PRegistedENodeClassStruct;
    subdf: TDataFrameEngine;
  begin
    clName := df.Reader.ReadString;
    cnt := df.Reader.ReadInteger;

    p := GetENodeClass(clName);
    Assert(p <> nil, 'bad context compiled format!');

    Result := p^.EngineContextClass.Create(AOwnerNode);
    Assert(Result <> nil, 'bad create context class');

    if AOwnerNode <> nil then
        AOwnerNode.Add(Result);

    Result.LoadFromCompiledDataFrame(df);

    subdf := TDataFrameEngine.Create;
    for i := 0 to cnt - 1 do
      begin
        df.Reader.ReadDataFrame(subdf);
        subdf.Reader.index := 0;
        __LoadNodeFromBuild(Result, subdf);
        subdf.Clear;
      end;
    DisposeObject(subdf);

    Result.FFirstOpenState.Clear;
    Result.SaveState(Result.FFirstOpenState);

    Result.FLastLoadState.Clear;
    Result.SaveState(Result.FLastLoadState);
  end;

var
  tmpdf: TDataFrameEngine;
  n: TENode_Base;
begin
  tmpdf := TDataFrameEngine.Create;
  tmpdf.DecodeFrom(stream);
  n := __LoadNodeFromBuild(nil, tmpdf);
  DisposeObject(tmpdf);
  Result := n;
end;

procedure SaveENodeState(output: TCoreClassStream; RootNode: TENode_Base);

  procedure __SaveNode(df: TDataFrameEngine; n: TENode_Base);
  var
    subdf: TDataFrameEngine;
    i: Integer;
  begin
    df.WriteString(GetENodeName(n.ClassType));
    df.WriteInteger(n.Count);
    df.WriteInteger(n.ID);
    n.SaveState(df);

    for i := 0 to n.Count - 1 do
      begin
        subdf := TDataFrameEngine.Create;
        __SaveNode(subdf, n[i]);
        df.WriteDataFrame(subdf);
        DisposeObject(subdf);
      end;
  end;

var
  df: TDataFrameEngine;
begin
  df := TDataFrameEngine.Create;

  __SaveNode(df, RootNode);

  df.EncodeTo(output);
  DisposeObject(df);
end;

procedure LoadENodeState(stream: TCoreClassStream; RootNode: TENode_Base);
  procedure __LoadNode(const AOwnerNode: TENode_Base; const df: TDataFrameEngine);
  var
    clName: SystemString;
    cnt, ID, i: Integer;
    p: PRegistedENodeClassStruct;
    subdf: TDataFrameEngine;
  begin
    clName := df.Reader.ReadString;
    cnt := df.Reader.ReadInteger;
    ID := df.Reader.ReadInteger;

    p := GetENodeClass(clName);
    Assert(p <> nil, 'bad context compiled format!');

    Assert(AOwnerNode.ClassType = p^.EngineContextClass, 'bad state context class');

    Assert(ID = AOwnerNode.ID, 'bad state id');
    Assert(cnt = AOwnerNode.Count, 'bad children context index');

    AOwnerNode.LoadState(df);

    subdf := TDataFrameEngine.Create;
    for i := 0 to AOwnerNode.Count - 1 do
      begin
        df.Reader.ReadDataFrame(subdf);
        subdf.Reader.index := 0;
        __LoadNode(AOwnerNode[i], subdf);
        subdf.Clear;
      end;
    DisposeObject(subdf);

    AOwnerNode.FLastLoadState.Clear;
    AOwnerNode.SaveState(AOwnerNode.FLastLoadState);
  end;

var
  tmpdf: TDataFrameEngine;
begin
  tmpdf := TDataFrameEngine.Create;
  tmpdf.DecodeFrom(stream);
  __LoadNode(RootNode, tmpdf);
  DisposeObject(tmpdf);
end;

function TENode_Base.GetItems(index: Integer): TENode_Base;
begin
  Result := FList[index] as TENode_Base;
end;

function TENode_Base.GetItemsID(AID: Integer): TENode_Base;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if ItemsIndex[i].FID = AID then
        Exit(ItemsIndex[i]);
end;

constructor TENode_Base.Create(AOwner: TENode_Base);
begin
  inherited Create;
  FOwner := AOwner;
  FList := TCoreClassListForObj.Create;
  FText := '';
  ClickCounter := 0;

  FInternalInitedStates := [];
  FData := nil;

  FOnDestroy := nil;
  FOnLoaded := nil;
  FOnProgress := nil;

  FPushStateList := TCoreClassListForObj.Create;
  FFirstOpenState := TDataFrameEngine.Create;
  FLastLoadState := TDataFrameEngine.Create;

  FAcceptProgress := False;
end;

destructor TENode_Base.Destroy;
var
  i: Integer;
begin
  if Assigned(FOnDestroy) then
    begin
      try
          FOnDestroy(Self);
      except
      end;
    end;

  if FOwner <> nil then
    begin
      i := 0;
      while i < FOwner.FList.Count do
        begin
          if FOwner.FList[i] = Self then
              FOwner.FList.Delete(i)
          else
              inc(i);
        end;
    end;

  for i := 0 to FPushStateList.Count - 1 do
      DisposeObject(FPushStateList[i]);

  DisposeObject(FPushStateList);
  DisposeObject(FFirstOpenState);
  DisposeObject(FLastLoadState);

  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

function TENode_Base.ROOT: TENode_Base;
begin
  Result := Self;
  while Result.Owner <> nil do
      Result := Result.Owner;
end;

function TENode_Base.Path: SystemString;
var
  n: TENode_Base;
begin
  n := Self;
  Result := PFormat('%d', [n.ID]);
  while n.Owner <> nil do
    begin
      n := n.Owner;
      Result := PFormat('%d:%s', [n.ID, Result]);
    end;
end;

function TENode_Base.FromPath(ph: SystemString): TENode_Base;
var
  r: TENode_Base;
  n: SystemString;
  nid: Integer;
begin
  Result := nil;
  if umlTrimSpace(ph).Len = 0 then
      Exit;

  r := ROOT;
  n := ph;

  if umlStrToInt(umlGetFirstStr(n, ':').Text) = r.ID then
    begin
      n := umlDeleteFirstStr(n, ':').Text;
      while n <> '' do
        begin
          try
            nid := umlStrToInt(umlGetFirstStr(n, ':').Text, 0);
            r := r.ItemsID[nid];
            if r = nil then
                Break;
            n := umlDeleteFirstStr(n, ':').Text;
          except
            r := nil;
            Break;
          end;
        end;
    end
  else
      r := nil;

  Result := r;
end;

function TENode_Base.TranslationPath(ph: SystemString): SystemString;
var
  n: TENode_Base;
begin
  Result := '';
  n := FromPath(ph);

  if n <> nil then
    begin
      Result := PFormat('%s', [n.Text]);
      while n.Owner <> nil do
        begin
          n := n.Owner;
          Result := PFormat('%s \ %s', [n.Text, Result]);
        end;
    end;
end;

function TENode_Base.GetRegistedName: SystemString;
begin
  Result := GetENodeName(ClassType);
end;

function TENode_Base.NewNode(NC: TENodeBaseClass): TENode_Base;
begin
  Result := NC.Create(Self);
  Add(Result);
end;

procedure TENode_Base.Add(n: TENode_Base);
begin
  FList.Add(n);
end;

procedure TENode_Base.Delete(idx: Integer);
begin
  try
      DisposeObject(TENode_Base(FList[idx]));
  except
  end;
end;

function TENode_Base.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TENode_Base.Clear;
begin
  while Count > 0 do
      DisposeObject(ItemsIndex[0]);
end;

procedure TENode_Base.Progress(deltaTime: Double);
var
  i: Integer;
begin
  if (AcceptProgress) and (Assigned(FOnProgress)) then
    begin
      try
          FOnProgress(Self, deltaTime);
      except
      end;
    end;

  for i := 0 to Count - 1 do
    begin
      try
          ItemsIndex[i].Progress(deltaTime);
      except
      end;
    end;
end;

procedure TENode_Base.LoadFromCompiledDataFrame(DataFrame: TDataFrameEngine);
begin
  FID := DataFrame.Reader.ReadInteger;
  FText := DataFrame.Reader.ReadString;
end;

procedure TENode_Base.SaveState(DataFrame: TDataFrameEngine);
begin
  DataFrame.WriteString(FText);
  DataFrame.WriteCardinal(ClickCounter);
end;

procedure TENode_Base.LoadState(DataFrame: TDataFrameEngine);
begin
  FText := DataFrame.Reader.ReadString;
  ClickCounter := DataFrame.Reader.ReadCardinal;
end;

procedure TENode_Base.PushState;
var
  DataFrame: TDataFrameEngine;
begin
  DataFrame := TDataFrameEngine.Create;
  SaveState(DataFrame);
  DataFrame.Reader.index := 0;
  FPushStateList.Add(DataFrame);
end;

procedure TENode_Base.PopState;
var
  DataFrame: TDataFrameEngine;
begin
  if FPushStateList.Count > 0 then
    begin
      DataFrame := TDataFrameEngine(FPushStateList[FPushStateList.Count - 1]);
      DataFrame.Reader.index := 0;
      LoadState(DataFrame);
      DisposeObject(DataFrame);
      FPushStateList.Delete(FPushStateList.Count - 1);
    end;
end;

procedure TENode_Base.ReductionToFirstOpenState;
begin
  FFirstOpenState.Reader.index := 0;
  LoadState(FFirstOpenState);
end;

procedure TENode_Base.ReductionToLastLoadState;
begin
  FLastLoadState.Reader.index := 0;
  LoadState(FLastLoadState);
end;

procedure TENode_Base.Install;
begin
  FInternalInitedStates := FInternalInitedStates + [nisInstalled];
end;

procedure TENode_Base.FullInstall;
  procedure _Install(n: TENode_Base);
  var
    i: Integer;
  begin
    n.Install;
    for i := 0 to n.Count - 1 do
        _Install(n.ItemsIndex[i]);
  end;

begin
  _Install(Self);
end;

procedure TENode_Base.Loaded;
begin
  FInternalInitedStates := FInternalInitedStates + [nisLoaded];

  if Assigned(FOnLoaded) then
      FOnLoaded(Self);
end;

procedure TENode_Base.FullLoaded;
  procedure _Loaded(n: TENode_Base);
  var
    i: Integer;
  begin
    try
        n.Loaded;
    except
    end;
    for i := 0 to n.Count - 1 do
        _Loaded(n.ItemsIndex[i]);
  end;

begin
  _Loaded(Self);
end;

function TENode_Base.ID: Integer;
begin
  Result := FID;
end;

function TENode_Base.Owner: TENode_Base;
begin
  Result := FOwner;
end;

procedure TENode_Base.ResetAcceptProgressState(v: Boolean);
  procedure __reset(n: TENode_Base);
  var
    i: Integer;
  begin
    for i := 0 to n.Count - 1 do
        __reset(n[i]);
    n.FAcceptProgress := v;
  end;

begin
  __reset(ROOT);
end;

constructor TENode_Root.Create(AOwner: TENode_Base);
begin
  inherited Create(AOwner);
  FEntryInteractiveNode := '';
end;

destructor TENode_Root.Destroy;
begin
  inherited Destroy;
end;

procedure TENode_Root.LoadFromCompiledDataFrame(DataFrame: TDataFrameEngine);
begin
  inherited LoadFromCompiledDataFrame(DataFrame);
  FEntryInteractiveNode := DataFrame.Reader.ReadString;
end;

procedure TENode_Root.SetNodeRootTriggerIntf(ADestroy, ALoaded: TENodeEvent; AProgress: TENodeProgress);
begin
  FOnDestroy := ADestroy;
  FOnLoaded := ALoaded;
  FOnProgress := AProgress;
end;

procedure TENode_Interactive.SetActiveOriginTextIndex(const Value: Integer);
begin
  if (FActiveOriginTextIndex >= 0) and (FActiveOriginTextIndex < FOriginTextTable.Count) then
    begin
      FOriginText.Assign(TCoreClassStrings(FOriginTextTable[FActiveOriginTextIndex]));
      FActiveOriginTextIndex := Value;
    end;
end;

constructor TENode_Interactive.Create(AOwner: TENode_Base);
begin
  inherited Create(AOwner);
  FOnInHere := nil;
  FOnOutHere := nil;
  FOriginText := TCoreClassStringList.Create;

  FOriginTextTable := TCoreClassListForObj.Create;
  FActiveOriginTextIndex := -1;
end;

destructor TENode_Interactive.Destroy;
var
  i: Integer;
begin
  for i := 0 to FOriginTextTable.Count - 1 do
      DisposeObject(FOriginTextTable[i]);

  DisposeObject(FOriginText);
  DisposeObject(FOriginTextTable);
  inherited Destroy;
end;

procedure TENode_Interactive.LoadFromCompiledDataFrame(DataFrame: TDataFrameEngine);
var
  i, cnt: Integer;
  ns: TCoreClassStrings;
begin
  inherited LoadFromCompiledDataFrame(DataFrame);
  FVisibledTitleText := DataFrame.Reader.ReadBool;
  DataFrame.Reader.ReadStrings(FOriginText);

  FActiveOriginTextIndex := DataFrame.Reader.ReadInteger;
  cnt := DataFrame.Reader.ReadInteger;
  for i := 0 to FOriginTextTable.Count - 1 do
      DisposeObject(FOriginTextTable[i]);
  FOriginTextTable.Clear;
  for i := 0 to cnt - 1 do
    begin
      ns := TCoreClassStringList.Create;
      DataFrame.Reader.ReadStrings(ns);
      FOriginTextTable.Add(ns);
    end;
end;

procedure TENode_Interactive.SaveState(DataFrame: TDataFrameEngine);
var
  i: Integer;
begin
  inherited SaveState(DataFrame);
  DataFrame.WriteBool(FVisibledTitleText);
  DataFrame.WriteStrings(FOriginText);

  DataFrame.WriteInteger(FActiveOriginTextIndex);
  DataFrame.WriteInteger(FOriginTextTable.Count);
  for i := 0 to FOriginTextTable.Count - 1 do
      DataFrame.WriteStrings(TCoreClassStrings(FOriginTextTable[i]));
end;

procedure TENode_Interactive.LoadState(DataFrame: TDataFrameEngine);
var
  i, cnt: Integer;
  ns: TCoreClassStrings;
begin
  inherited LoadState(DataFrame);
  FVisibledTitleText := DataFrame.Reader.ReadBool;
  DataFrame.Reader.ReadStrings(FOriginText);

  FActiveOriginTextIndex := DataFrame.Reader.ReadInteger;
  cnt := DataFrame.Reader.ReadInteger;
  for i := 0 to FOriginTextTable.Count - 1 do
      DisposeObject(FOriginTextTable[i]);
  FOriginTextTable.Clear;
  for i := 0 to cnt - 1 do
    begin
      ns := TCoreClassStringList.Create;
      DataFrame.Reader.ReadStrings(ns);
      FOriginTextTable.Add(ns);
    end;
end;

procedure TENode_Interactive.GetInteractiveSubNodeList(output: TCoreClassListForObj);
var
  i: Integer;
  n: TENode_Base;
begin
  for i := 0 to Count - 1 do
    begin
      n := ItemsIndex[i];
      if n is TENode_InteractiveChildren then
          output.Add(n)
      else if n is TENode_PopupBack then
          output.Add(n)
      else if n is TENode_Introduce then
          TENode_Introduce(n).GetInteractiveSubNodeList(output);
    end;
end;

procedure TENode_Interactive.InHere;
begin
  if Assigned(FOnInHere) then
      FOnInHere(Self);
end;

procedure TENode_Interactive.OutHere;
begin
  if Assigned(FOnOutHere) then
      FOnOutHere(Self);
end;

constructor TENode_InteractiveChildren.Create(AOwner: TENode_Base);
begin
  inherited Create(AOwner);
  FOnJump := nil;
  FUsesPopupJump := False;
  FJumpInteractiveNode := '';
  FVisible := True;
  FDetail := '';
  FUsedPlanJump := False;
  FPlanJumpTable := TCoreClassStringList.Create;
  FActivePlanIndex := -1;
end;

destructor TENode_InteractiveChildren.Destroy;
begin
  DisposeObject(FPlanJumpTable);
  inherited Destroy;
end;

procedure TENode_InteractiveChildren.LoadFromCompiledDataFrame(DataFrame: TDataFrameEngine);
begin
  inherited LoadFromCompiledDataFrame(DataFrame);
  FUsesPopupJump := DataFrame.Reader.ReadBool;
  FJumpInteractiveNode := DataFrame.Reader.ReadString;
  FVisible := DataFrame.Reader.ReadBool;
  FDetail := DataFrame.Reader.ReadString;

  FUsedPlanJump := DataFrame.Reader.ReadBool;
  DataFrame.Reader.ReadStrings(FPlanJumpTable);
  FActivePlanIndex := DataFrame.Reader.ReadInteger;
end;

procedure TENode_InteractiveChildren.SaveState(DataFrame: TDataFrameEngine);
begin
  inherited SaveState(DataFrame);
  DataFrame.WriteBool(FUsesPopupJump);
  DataFrame.WriteString(FJumpInteractiveNode);
  DataFrame.WriteBool(FVisible);
  DataFrame.WriteString(FDetail);

  DataFrame.WriteBool(FUsedPlanJump);
  DataFrame.WriteStrings(FPlanJumpTable);
  DataFrame.WriteInteger(FActivePlanIndex);
end;

procedure TENode_InteractiveChildren.LoadState(DataFrame: TDataFrameEngine);
begin
  inherited LoadState(DataFrame);
  FUsesPopupJump := DataFrame.Reader.ReadBool;
  FJumpInteractiveNode := DataFrame.Reader.ReadString;
  FVisible := DataFrame.Reader.ReadBool;
  FDetail := DataFrame.Reader.ReadString;

  FUsedPlanJump := DataFrame.Reader.ReadBool;
  DataFrame.Reader.ReadStrings(FPlanJumpTable);
  FActivePlanIndex := DataFrame.Reader.ReadInteger;
end;

function TENode_InteractiveChildren.GetOwnerInteractiveNode: TENode_Interactive;
begin
  if Owner is TENode_Container then
      Result := TENode_Container(Owner).GetIntroduceSourceInteractiveNode
  else if Owner is TENode_Interactive then
      Result := TENode_Interactive(Owner)
  else
      Assert(False, 'Illegal struct');
end;

function TENode_InteractiveChildren.DoJumpInteractiveNode: TENode_Interactive;
begin
  if Assigned(FOnJump) then
      FOnJump(Self);
  Result := GetJumpInteractiveNode;
end;

function TENode_InteractiveChildren.GetJumpInteractiveNode: TENode_Interactive;
var
  n: TENode_Base;
begin
  if FUsedPlanJump then
    begin
      if (FActivePlanIndex >= 0) and (FActivePlanIndex < FPlanJumpTable.Count) then
          n := FromPath(FPlanJumpTable[FActivePlanIndex])
      else
          DoStatus('plan jump index error');
    end
  else
      n := FromPath(JumpInteractiveNode);

  if n is TENode_Interactive then
      Result := n as TENode_Interactive
  else if n is TENode_Container then
      Result := TENode_Container(n).GetIntroduceSourceInteractiveNode
  else
      Result := nil;
end;

constructor TENode_PopupBack.Create(AOwner: TENode_Base);
begin
  inherited Create(AOwner);
  FOnJump := nil;
  FVisible := True;
end;

destructor TENode_PopupBack.Destroy;
begin
  inherited Destroy;
end;

procedure TENode_PopupBack.LoadFromCompiledDataFrame(DataFrame: TDataFrameEngine);
begin
  inherited LoadFromCompiledDataFrame(DataFrame);
  FVisible := DataFrame.Reader.ReadBool;
end;

procedure TENode_PopupBack.SaveState(DataFrame: TDataFrameEngine);
begin
  inherited SaveState(DataFrame);
  DataFrame.WriteBool(FVisible);
end;

procedure TENode_PopupBack.LoadState(DataFrame: TDataFrameEngine);
begin
  inherited LoadState(DataFrame);
  FVisible := DataFrame.Reader.ReadBool;
end;

function TENode_PopupBack.GetOwnerInteractiveNode: TENode_Interactive;
begin
  if Owner is TENode_Container then
      Result := TENode_Container(Owner).GetIntroduceSourceInteractiveNode
  else if Owner is TENode_Interactive then
      Result := TENode_Interactive(Owner)
  else
      Assert(False, 'Illegal struct');
end;

procedure TENode_PopupBack.DoPopupBack(n: TENode_Interactive);
begin
  if Assigned(FOnJump) then
      FOnJump(n.Path);
end;

constructor TENode_Container.Create(AOwner: TENode_Base);
begin
  inherited Create(AOwner);
  FIntroduceSource := nil;
end;

destructor TENode_Container.Destroy;
begin
  inherited Destroy;
end;

procedure TENode_Container.SaveState(DataFrame: TDataFrameEngine);
var
  n: SystemString;
begin
  inherited SaveState(DataFrame);
  if FIntroduceSource <> nil then
      n := FIntroduceSource.Path
  else
      n := '';
  DataFrame.WriteString(n);
end;

procedure TENode_Container.LoadState(DataFrame: TDataFrameEngine);
var
  n: SystemString;
begin
  inherited LoadState(DataFrame);
  n := DataFrame.Reader.ReadString;
  if n <> '' then
      FIntroduceSource := (ROOT.FromPath(n) as TENode_Introduce)
  else
      FIntroduceSource := nil;
end;

function TENode_Container.GetIntroduceSourceInteractiveNode: TENode_Interactive;
begin
  Assert(FIntroduceSource <> nil, 'Illegal Introduce Source');
  Result := FIntroduceSource.GetOwnerInteractiveNode;
end;

procedure TENode_Container.GetInteractiveSubNodeList(output: TCoreClassListForObj);
var
  i: Integer;
  n: TENode_Base;
begin
  for i := 0 to Count - 1 do
    begin
      n := ItemsIndex[i];
      if n is TENode_InteractiveChildren then
          output.Add(n)
      else if n is TENode_PopupBack then
          output.Add(n)
      else if n is TENode_Introduce then
          TENode_Introduce(n).GetInteractiveSubNodeList(output);
    end;
end;

constructor TENode_Introduce.Create(AOwner: TENode_Base);
begin
  inherited Create(AOwner);
end;

destructor TENode_Introduce.Destroy;
begin
  inherited Destroy;
end;

procedure TENode_Introduce.SaveState(DataFrame: TDataFrameEngine);
begin
  inherited SaveState(DataFrame);
  DataFrame.WriteString(FIntroduceNodePath);
end;

procedure TENode_Introduce.LoadState(DataFrame: TDataFrameEngine);
begin
  inherited LoadState(DataFrame);
  FIntroduceNodePath := DataFrame.Reader.ReadString;
end;

procedure TENode_Introduce.LoadFromCompiledDataFrame(DataFrame: TDataFrameEngine);
begin
  inherited LoadFromCompiledDataFrame(DataFrame);
  FIntroduceNodePath := DataFrame.Reader.ReadString;
end;

function TENode_Introduce.GetOwnerInteractiveNode: TENode_Interactive;
begin
  if Owner is TENode_Container then
      Result := TENode_Container(Owner).GetIntroduceSourceInteractiveNode
  else if Owner is TENode_Interactive then
      Result := TENode_Interactive(Owner)
  else
      Assert(False, 'Illegal struct');
end;

procedure TENode_Introduce.GetInteractiveSubNodeList(output: TCoreClassListForObj);
var
  n: TENode_Base;
  i: Integer;
begin
  n := FromPath(FIntroduceNodePath);
  if n is TENode_Container then
    begin
      TENode_Container(n).GetInteractiveSubNodeList(output);
      TENode_Container(n).IntroduceSource := Self;
    end;
end;

initialization

RegisterENodeClass('!Base', TENode_Base);
RegisterENodeClass('Root', TENode_Root);
RegisterENodeClass('Base', TENode);
RegisterENodeClass('Interactive', TENode_Interactive);
RegisterENodeClass('InteractiveChildren', TENode_InteractiveChildren);
RegisterENodeClass('InteractivePopupBack', TENode_PopupBack);
RegisterENodeClass('Container', TENode_Container);
RegisterENodeClass('Introduce', TENode_Introduce);

finalization

_ClearRegistedEngineContextClassList;

end.
