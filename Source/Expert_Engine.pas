unit Expert_Engine;

{$INCLUDE zDefine.inc}

interface

uses SysUtils, Variants, DateUtils, Math,

  CoreClasses, Expret_Node,
  DataFrameEngine, ObjectDataHashField, ObjectDataManager, ObjectDataHashItem, MemoryStream64,
  DoStatusIO, TextDataEngine, ListEngine, UnicodeMixedLib, PascalStrings;

type
  TContextInterfaceDataEngine = class(TCoreClassPersistent)
  protected
    FRootNode: TENode_Base;
    FInteractive: TENode_Interactive;
    FInteractiveText: SystemString;
    FLoadedCompleted: Boolean;
    FCanSavedState: Boolean;
    FDebugEnabled: Boolean;
    FSpeed: Double;

    FInteractiveChildren: TCoreClassListForObj;
    FInteractiveStates: TCoreClassListForObj;

    FPushStateList: TCoreClassListForObj;
    FStateFilePrefix: SystemString;

    procedure InternalClick(n: TENode_Base; TriggerOnJump: Boolean);
  public
    constructor Create(DataPackage: TCoreClassStream); virtual;
    destructor Destroy; override;

    procedure Install;

    procedure Loaded; overload;
    procedure Loaded(StateFile: SystemString); overload;

    procedure Progress(deltaTime: Double);

    procedure DisableSaveState;
    procedure EnabledSaveState;

    procedure PushState;
    procedure PopState;

    procedure NodeJump(Path: SystemString);
    procedure PopupJump(Path: SystemString);
    function PopupBack: TENode_Interactive;
    function InternalPopupBack: TENode_Interactive;
    function CanPopupBack: Boolean;
    procedure Click(n: TENode_Base);
    procedure UpdateInteractiveNode;
    function NodeIsActive(n: TENode_Base): Boolean;

    function GetClickInteractiveNode(n: TENode_Base): TENode_Interactive;

    procedure SaveState(DataFrame: TDataFrameEngine);
    procedure LoadState(DataFrame: TDataFrameEngine);

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(n: SystemString);
    function LoadFromFile(n: SystemString): Boolean;
    function CanLoadFile(n: SystemString): Boolean;
    function GetCanLoadFileTime(n: SystemString): TDateTime;

    property InteractiveText: SystemString read FInteractiveText;
    property Interactive: TENode_Interactive read FInteractive;
    property InteractiveChildren: TCoreClassListForObj read FInteractiveChildren;

    property RootNode: TENode_Base read FRootNode;
    property CanSavedState: Boolean read FCanSavedState;
    property DebugEnabled: Boolean read FDebugEnabled write FDebugEnabled;
    property Speed: Double read FSpeed write FSpeed;
  end;

implementation

procedure TContextInterfaceDataEngine.InternalClick(n: TENode_Base; TriggerOnJump: Boolean);
var
  jn: TENode_Interactive;
begin
  if (n is TENode_Root) then
    begin
      inc(n.ClickCounter);
      NodeJump(TENode_Root(n).EntryInteractiveNode);
    end
  else if (n is TENode_InteractiveChildren) then
    begin
      with TENode_InteractiveChildren(n) do
        begin
          if TriggerOnJump then
              jn := DoJumpInteractiveNode
          else
              jn := GetJumpInteractiveNode;

          if jn <> nil then
            begin
              inc(n.ClickCounter);
              if UsesPopupJump then
                  PopupJump(jn.Path)
              else
                  NodeJump(jn.Path);
            end
          else
              RaiseInfo('Illegal jump');
        end;
    end
  else if (n is TENode_PopupBack) then
    begin
      jn := InternalPopupBack;
      if jn <> nil then
        begin
          inc(n.ClickCounter);
          TENode_PopupBack(n).DoPopupBack(jn);
          NodeJump(jn.Path);
        end;
    end;
end;

constructor TContextInterfaceDataEngine.Create(DataPackage: TCoreClassStream);

  procedure _InitNode(n: TENode_Base);
  var
    i: Integer;
  begin
    n.Data := Self;
    for i := 0 to n.Count - 1 do
        _InitNode(n.ItemsIndex[i]);
  end;

var
  df: TDataFrameEngine;
  ASize: Integer;
  NodeData, ms: TMemoryStream64;
  ZDecompStream: TDecompressionStream;
begin
  inherited Create;

  DataPackage.Position := 0;
  DataPackage.ReadBuffer(ASize, C_Integer_Size);

  ZDecompStream := TDecompressionStream.Create(DataPackage);
  ms := TMemoryStream64.Create;
  ms.CopyFrom(ZDecompStream, ASize);
  DisposeObject(ZDecompStream);

  df := TDataFrameEngine.Create;
  ms.Position := 0;
  df.DecodeFrom(ms);
  DisposeObject(ms);
  ms := nil;

  NodeData := TMemoryStream64.Create;
  df.Reader.ReadStream(NodeData);

  FStateFilePrefix := df.Reader.ReadString;
  DisposeObject(df);

  // load node
  NodeData.Position := 0;
  FRootNode := BuildENodeFromCompiledStream(NodeData);
  _InitNode(FRootNode);

  // dispose temp class
  DisposeObject(NodeData);

  // init global
  FInteractiveText := '';
  FInteractive := nil;
  FInteractiveChildren := TCoreClassListForObj.Create;
  FInteractiveStates := TCoreClassListForObj.Create;
  FPushStateList := TCoreClassListForObj.Create;
  FLoadedCompleted := False;
  FCanSavedState := True;
  FDebugEnabled := False;
  FSpeed := 1.0;
end;

destructor TContextInterfaceDataEngine.Destroy;
var
  i: Integer;
begin
  DisposeObject(FRootNode);
  DisposeObject(FInteractiveChildren);
  DisposeObject(FInteractiveStates);

  for i := 0 to FPushStateList.Count - 1 do
      DisposeObject(FPushStateList[i]);

  DisposeObject(FPushStateList);

  inherited Destroy;
end;

procedure TContextInterfaceDataEngine.Install;
begin
  FRootNode.FullInstall;
end;

procedure TContextInterfaceDataEngine.Loaded;
begin
  FRootNode.FullLoaded;
  FLoadedCompleted := True;
  Click(FRootNode.ROOT);
end;

procedure TContextInterfaceDataEngine.Loaded(StateFile: SystemString);
begin
  FRootNode.FullLoaded;
  FLoadedCompleted := True;
  LoadFromFile(StateFile);
end;

procedure TContextInterfaceDataEngine.Progress(deltaTime: Double);
begin
  if not FLoadedCompleted then
      Exit;

  FRootNode.Progress(deltaTime);
end;

procedure TContextInterfaceDataEngine.DisableSaveState;
begin
  FCanSavedState := False;
end;

procedure TContextInterfaceDataEngine.EnabledSaveState;
begin
  FCanSavedState := True;
end;

procedure TContextInterfaceDataEngine.PushState;
var
  DataFrame: TDataFrameEngine;
begin
  if not FCanSavedState then
      Exit;
  DataFrame := TDataFrameEngine.Create;
  SaveState(DataFrame);
  DataFrame.Reader.index := 0;
  FPushStateList.Add(DataFrame);
end;

procedure TContextInterfaceDataEngine.PopState;
var
  DataFrame: TDataFrameEngine;
begin
  if not FCanSavedState then
      Exit;
  if FPushStateList.Count > 0 then
    begin
      DataFrame := TDataFrameEngine(FPushStateList[FPushStateList.Count - 1]);
      DataFrame.Reader.index := 0;
      LoadState(DataFrame);
      DisposeObject(DataFrame);
      FPushStateList.Delete(FPushStateList.Count - 1);
    end;
end;

procedure TContextInterfaceDataEngine.NodeJump(Path: SystemString);
var
  n: TENode_Base;
  i: Integer;
begin
  if FRootNode = nil then
      Exit;
  n := FRootNode.FromPath(Path);
  if (n is TENode_Interactive) then
    begin
      if FInteractive <> nil then
          FInteractive.OutHere;
      FInteractive := n as TENode_Interactive;
      FInteractiveChildren.Clear;
      FInteractive.GetInteractiveSubNodeList(FInteractiveChildren);

      FRootNode.ResetAcceptProgressState(False);
      FInteractive.AcceptProgress := True;
      i := 0;
      while i < FInteractiveChildren.Count do
        begin
          n := FInteractiveChildren[i] as TENode_Base;
          n.AcceptProgress := True;
          if n is TENode_InteractiveChildren then
            begin
              if (not TENode_InteractiveChildren(n).Visible) then
                  FInteractiveChildren.Delete(i)
              else
                  inc(i);
            end
          else if n is TENode_PopupBack then
            begin
              if (not CanPopupBack) or (not TENode_PopupBack(n).Visible) then
                  FInteractiveChildren.Delete(i)
              else
                begin
                  inc(i);
                end;
            end
          else
              FInteractiveChildren.Delete(i);
        end;

      FInteractive.InHere;

      if (FInteractive.ActiveOriginTextIndex >= 0)
        and (FInteractive.ActiveOriginTextIndex < FInteractive.OriginTextTable.Count) then
          FInteractive.OriginText.Assign(TCoreClassStrings(FInteractive.OriginTextTable[FInteractive.ActiveOriginTextIndex]));

      FInteractiveText := FInteractive.OriginText.Text;
    end
  else
      RaiseInfo('Interactive Illegal');
end;

procedure TContextInterfaceDataEngine.PopupJump(Path: SystemString);
begin
  if FInteractiveStates <> nil then
      FInteractiveStates.Add(FInteractive);
  NodeJump(Path);
end;

function TContextInterfaceDataEngine.PopupBack: TENode_Interactive;
begin
  Result := InternalPopupBack;
  if Result <> nil then
      NodeJump(Result.Path);
end;

function TContextInterfaceDataEngine.InternalPopupBack: TENode_Interactive;
var
  n: TENode_Interactive;
begin
  Result := nil;
  if FInteractiveStates.Count > 0 then
    begin
      n := FInteractiveStates[FInteractiveStates.Count - 1] as TENode_Interactive;
      FInteractiveStates.Delete(FInteractiveStates.Count - 1);
      Result := n;
    end
  else
      RaiseInfo('Illegal Popup back');
end;

function TContextInterfaceDataEngine.CanPopupBack: Boolean;
begin
  Result := FInteractiveStates.Count > 0;
end;

procedure TContextInterfaceDataEngine.Click(n: TENode_Base);
begin
  InternalClick(n, False);
end;

procedure TContextInterfaceDataEngine.UpdateInteractiveNode;
var
  n: TENode_Base;
  i: Integer;
begin
  FInteractiveChildren.Clear;
  FInteractive.GetInteractiveSubNodeList(FInteractiveChildren);

  FRootNode.ResetAcceptProgressState(False);
  FInteractive.AcceptProgress := True;
  i := 0;
  while i < FInteractiveChildren.Count do
    begin
      n := FInteractiveChildren[i] as TENode_Base;
      n.AcceptProgress := True;
      if n is TENode_InteractiveChildren then
        begin
          if (not TENode_InteractiveChildren(n).Visible) then
              FInteractiveChildren.Delete(i)
          else
              inc(i);
        end
      else if n is TENode_PopupBack then
        begin
          if (not CanPopupBack) or (not TENode_PopupBack(n).Visible) then
              FInteractiveChildren.Delete(i)
          else
              inc(i);
        end
      else
          FInteractiveChildren.Delete(i);
    end;

  if (FInteractive.ActiveOriginTextIndex >= 0)
    and (FInteractive.ActiveOriginTextIndex < FInteractive.OriginTextTable.Count) then
      FInteractive.OriginText.Assign(TCoreClassStrings(FInteractive.OriginTextTable[FInteractive.ActiveOriginTextIndex]));

  FInteractiveText := FInteractive.OriginText.Text;
end;

function TContextInterfaceDataEngine.NodeIsActive(n: TENode_Base): Boolean;
begin
  Result := n.AcceptProgress;
end;

function TContextInterfaceDataEngine.GetClickInteractiveNode(n: TENode_Base): TENode_Interactive;
var
  jn: TENode_Interactive;
begin
  Result := nil;
  if (n is TENode_Root) then
    begin
      Result := n.ROOT.FromPath(TENode_Root(n).EntryInteractiveNode) as TENode_Interactive;
    end
  else if (n is TENode_InteractiveChildren) then
    begin
      with TENode_InteractiveChildren(n) do
        begin
          jn := GetJumpInteractiveNode;
          if jn <> nil then
            begin
              Result := jn;
            end
          else
              RaiseInfo('Illegal jump');
        end;
    end
  else if (n is TENode_PopupBack) then
    begin
      if FInteractiveStates.Count > 0 then
          Result := FInteractiveStates[FInteractiveStates.Count - 1] as TENode_Interactive;
    end;
end;

procedure TContextInterfaceDataEngine.SaveState(DataFrame: TDataFrameEngine);
var
  ms: TMemoryStream64;
  i: Integer;
begin
  ms := TMemoryStream64.Create;

  SaveENodeState(ms, FRootNode);
  DataFrame.WriteStream(ms);
  ms.Clear;

  DataFrame.WriteString(FInteractiveText);
  DataFrame.WriteString(FInteractive.Path);

  DataFrame.WriteInteger(FInteractiveStates.Count);
  for i := 0 to FInteractiveStates.Count - 1 do
      DataFrame.WriteString((FInteractiveStates[i] as TENode_Base).Path);

  DataFrame.WriteBool(FDebugEnabled);

  DataFrame.WriteDouble(FSpeed);

  DisposeObject(ms);
end;

procedure TContextInterfaceDataEngine.LoadState(DataFrame: TDataFrameEngine);
var
  ms: TMemoryStream64;
  i, cnt: Integer;
begin
  ms := TMemoryStream64.Create;

  ms.Clear;
  DataFrame.Reader.ReadStream(ms);
  LoadENodeState(ms, FRootNode);

  FInteractiveText := DataFrame.Reader.ReadString;
  FInteractive := FRootNode.FromPath(DataFrame.Reader.ReadString) as TENode_Interactive;

  FInteractiveStates.Clear;
  cnt := DataFrame.Reader.ReadInteger;
  for i := 0 to cnt - 1 do
      FInteractiveStates.Add(FRootNode.FromPath(DataFrame.Reader.ReadString));

  FDebugEnabled := DataFrame.Reader.ReadBool;

  FSpeed := DataFrame.Reader.ReadDouble;

  DisposeObject(ms);
end;

procedure TContextInterfaceDataEngine.SaveToStream(stream: TCoreClassStream);
var
  ms: TMemoryStream64;
  DataFrame: TDataFrameEngine;
  ZCompStream: TCompressionStream;
  ASize: Integer;
begin
  if not FCanSavedState then
      Exit;
  DataFrame := TDataFrameEngine.Create;
  SaveState(DataFrame);

  ms := TMemoryStream64.Create;
  DataFrame.EncodeTo(ms);
  ASize := ms.Size;
  stream.WriteBuffer(ASize, C_Integer_Size);
  ZCompStream := TCompressionStream.Create(stream);
  ms.Position := 0;
  ZCompStream.CopyFrom(ms, ms.Size);
  DisposeObject(ZCompStream);
  DisposeObject(ms);
  stream.Position := 0;

  DisposeObject(DataFrame);
end;

procedure TContextInterfaceDataEngine.LoadFromStream(stream: TCoreClassStream);
var
  DataFrame: TDataFrameEngine;
  ASize: Integer;
  ms: TMemoryStream64;
  ZDecompStream: TDecompressionStream;
begin
  inherited Create;

  stream.Position := 0;
  stream.ReadBuffer(ASize, C_Integer_Size);

  ZDecompStream := TDecompressionStream.Create(stream);
  ms := TMemoryStream64.Create;
  ms.CopyFrom(ZDecompStream, ASize);
  DisposeObject(ZDecompStream);

  DataFrame := TDataFrameEngine.Create;
  ms.Position := 0;
  DataFrame.DecodeFrom(ms);
  DisposeObject(ms);
  ms := nil;

  LoadState(DataFrame);

  DisposeObject(DataFrame);
end;

procedure TContextInterfaceDataEngine.SaveToFile(n: SystemString);
var
  ms: TMemoryStream64;
begin
  if not FCanSavedState then
      Exit;
  ms := TMemoryStream64.Create;
  SaveToStream(ms);
  ms.SaveToFile(umlCombineFileName(umlCurrentPath, Format('%s%s.State', [FStateFilePrefix, n])));
  DisposeObject(ms);
end;

function TContextInterfaceDataEngine.LoadFromFile(n: SystemString): Boolean;
var
  ms: TMemoryStream64;
begin
  ms := TMemoryStream64.Create;
  ms.LoadFromFile(umlCombineFileName(umlCurrentPath, Format('%s%s.State', [FStateFilePrefix, n])));
  ms.Position := 0;
  LoadFromStream(ms);
  DisposeObject(ms);
  Result := True;
end;

function TContextInterfaceDataEngine.CanLoadFile(n: SystemString): Boolean;
begin
  Result := umlFileExists(umlCombineFileName(umlCurrentPath, Format('%s%s.State', [FStateFilePrefix, n])));
end;

function TContextInterfaceDataEngine.GetCanLoadFileTime(n: SystemString): TDateTime;
begin
  Result := umlGetFileTime(umlCombineFileName(umlCurrentPath, Format('%s%s.State', [FStateFilePrefix, n])));
end;

end.
