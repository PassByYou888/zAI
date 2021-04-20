(*
 *	 Unit owner: d10.�����
 *	       blog: http://www.cnblogs.com/dksoft
 *     homePage: www.diocp.org
 *
 *   2015-04-13 13:03:47
 *     ͨ�ö����
 *
 *)
unit utils_objectPool;

interface

uses
  utils_queues, Windows, SysUtils, utils_hashs, utils_BufferPool, utils_strings;

type
{$IFDEF UNICODE}
  TOnCreateObjectEvent = reference to function:TObject;
{$ELSE}
  TOnCreateObjectEvent = function:TObject of object;
{$ENDIF}

  TObjectPool = class(TObject)
  private
    FName: String;
    FUsingCount: Integer;

    FObjectList: TBaseQueue;
    FOnCreateObjectEvent: TOnCreateObjectEvent;
    FCount: Integer;
    FOutTime: Integer;
    FReleaseTime: Integer;
  public
    constructor Create(AOnCreateObjectEvent: TOnCreateObjectEvent);

    destructor Destroy; override;

    /// <summary>
    ///   ������󣬷��̰߳�ȫ
    /// </summary>
    procedure Clear;
    
    /// <summary>
    ///   �ȴ����ж���黹
    /// </summary>
    function WaitFor(pvTimeOut: Cardinal): Boolean;


    /// <summary>
    ///   ��ȡ����
    /// </summary>
    function GetObject:TObject;

    /// <summary>
    ///   �黹����
    /// </summary>
    procedure ReleaseObject(pvObject:TObject);

    /// <summary>
    ///   ����ʹ������
    /// </summary>
    property UsingCount: Integer read FUsingCount;

    /// <summary>
    ///   ��������
    /// </summary>
    property Count: Integer read FCount;

    /// <summary>
    ///   ���ƣ��������ڼ�ص���
    /// </summary>
    property Name: String read FName write FName;


    /// <summary>
    ///  �������
    /// </summary>
    property OutTime: Integer read FOutTime write FOutTime;

    /// <summary>
    ///   ���ش���
    /// </summary>
    property ReleaseTime: Integer read FReleaseTime write FReleaseTime;


    /// <summary>
    ///   ���������¼�
    /// </summary>
    property OnCreateObjectEvent: TOnCreateObjectEvent read FOnCreateObjectEvent
        write FOnCreateObjectEvent;
  end;


  TMaxPoolItem = class(TObject)
  private
    FEnable: Boolean;
    FData: TObject;
    FDataReleaseType:Integer;
    FMax:Integer;
    FCount:Integer;
  public
    destructor Destroy; override;
  end;


  /// <summary>
  ///   �������ö��������ô����ĳ�
  /// </summary>
  TMaxObjectPool = class(TObject)
  private
    FOutCounter:Integer;
    FObjMap: TDHashTableSafe;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>TMaxObjectPool.Clear
    /// </summary>
    /// <returns>
    ///   0: �ɹ�
    ///  -1: ������ʹ�õĶ���
    /// </returns>
    function Clear: Integer;

    /// <summary>TMaxObjectPool.WaitForRelease
    /// </summary>
    /// <returns>
    ///   0: �ȴ����(�����ͷ�)
    ///  -1: ��ʱ
    /// </returns>
    /// <param name="pvTimeOut"> (Integer) </param>
    function WaitForRelease(pvTimeOut:Integer): Integer;

    /// <summary>TMaxObjectPool.CheckPutObject
    /// </summary>
    /// <returns>
    ///    0: �ɹ�
    ///   -1: �Ѿ�����
    /// </returns>
    /// <param name="pvID"> (String) </param>
    /// <param name="pvData"> (TObject) </param>
    /// <param name="pvMaxNum"> (Integer) </param>
    function CheckPutObject(pvID: String; pvData: TObject; pvMaxNum: Integer;
        pvOwnObject: Boolean = true): Integer;
        
    /// <summary>TMaxObjectPool.CheckRemoveObject
    /// </summary>
    /// <returns>
    ///   0: �ɹ�
    ///  -1: ������
    ///  -2: ������ʹ��
    /// </returns>
    /// <param name="pvID"> (string) </param>
    function CheckRemoveObject(pvID:string): Integer;
    
    /// <summary>TMaxObjectPool.CheckGetObject
    /// </summary>
    /// <returns>
    ///  nil:���ɹ�
    /// </returns>
    /// <param name="pvID"> (string) </param>
    function CheckGetObject(pvID: string): TObject;



    /// <summary>TMaxObjectPool.CheckGetCurrentCount
    /// </summary>
    /// <returns>
    ///   ��ȡ��ǰʹ������
    ///  -1: ������
    /// </returns>
    /// <param name="pvID"> (String) </param>
    function CheckGetCurrentCount(pvID:String): Integer;

    /// <summary>TMaxObjectPool.ReleaseObject
    /// </summary>
    /// <returns>
    ///   0: �ɹ�
    ///  -1: ����ƥ��
    ///  -2: �ظ��黹
    ///  -3: ������
    /// </returns>
    /// <param name="pvID"> (String) </param>
    /// <param name="pvObject"> (TObject) </param>
    function ReleaseObject(pvID:String; pvObject:TObject): Integer;

    /// <summary>
    ///  ��ȡ�ص�ʹ�����
    /// </summary>
    function GetPoolInfo(pvSpliteStr: String = sLineBreak): String;
  end;

implementation

var
  __ProcessIDStr :String;

procedure WriteFileMsg(pvMsg:String; pvFilePre:string);
var
  lvFileName, lvBasePath:String;
  lvLogFile: TextFile;
begin
  try
    lvBasePath :=ExtractFilePath(ParamStr(0)) + 'log';
    ForceDirectories(lvBasePath);
    lvFileName :=lvBasePath + '\' + __ProcessIDStr+ '_' + pvFilePre +
     FormatDateTime('mmddhhnn', Now()) + '.log';

    AssignFile(lvLogFile, lvFileName);
    if (FileExists(lvFileName)) then
      append(lvLogFile)
    else
      rewrite(lvLogFile);

    writeln(lvLogFile, pvMsg);
    flush(lvLogFile);
    CloseFile(lvLogFile);
  except
    ;
  end;
end;

procedure TObjectPool.Clear;
begin
  FUsingCount := 0;
  FCount := 0;
  FObjectList.FreeDataObject;
  FObjectList.Clear;
end;

constructor TObjectPool.Create(AOnCreateObjectEvent: TOnCreateObjectEvent);
begin
  inherited Create;
  FCount := 0;
  FUsingCount := 0;
  FObjectList := TBaseQueue.Create();
  FOnCreateObjectEvent := AOnCreateObjectEvent;
end;

destructor TObjectPool.Destroy;
begin
  FObjectList.FreeDataObject;
  FObjectList.Free;
  inherited Destroy;
end;

function TObjectPool.GetObject: TObject;
begin
  Result := FObjectList.DeQueue;
  if Result = nil then
  begin
    Assert(Assigned(FOnCreateObjectEvent));
    Result := FOnCreateObjectEvent();
    Assert(Result <> nil);
    InterlockedIncrement(FCount);
  end;
  InterlockedIncrement(FUsingCount);
  
  InterlockedIncrement(FOutTime);

end;

procedure TObjectPool.ReleaseObject(pvObject:TObject);
begin
  FObjectList.EnQueue(pvObject);
  InterlockedDecrement(FUsingCount);
  InterlockedIncrement(FReleaseTime);
end;

function TObjectPool.WaitFor(pvTimeOut: Cardinal): Boolean;
var
  l:Cardinal;
  c:Integer;
begin
  l := GetTickCount;
  c := FUsingCount;
  while (c > 0) do
  begin
    {$IFDEF MSWINDOWS}
    SwitchToThread;
    {$ELSE}
    TThread.Yield;
    {$ENDIF}

    if GetTickCount - l > pvTimeOut then
    begin
      WriteFileMsg(Format('(%s)WaitFor �ȴ���ʱ, ��ǰδ�黹����:%d', [FName, c]), 'WaitFor');
      Break;
    end;
    c := FUsingCount;
  end;

  Result := FUsingCount = 0;
end;

constructor TMaxObjectPool.Create;
begin
  inherited Create;
  FObjMap := TDHashTableSafe.Create();
  FOutCounter := 0;
end;

destructor TMaxObjectPool.Destroy;
begin
  FObjMap.FreeAllDataAsObject;
  FreeAndNil(FObjMap);
  inherited Destroy;
end;

function TMaxObjectPool.CheckGetCurrentCount(pvID:String): Integer;
var
  lvItem:TMaxPoolItem;
begin
  FObjMap.Lock;
  try
    lvItem :=TMaxPoolItem(FObjMap.ValueMap[pvID]);
    if lvItem <> nil then
    begin
      Result := lvItem.FCount;
    end else
    begin
      Result := -1;
    end;
  finally
    FObjMap.UnLock;
  end;
end;

function TMaxObjectPool.CheckPutObject(pvID: String; pvData: TObject; pvMaxNum:
    Integer; pvOwnObject: Boolean = true): Integer;
var
  lvItem:TMaxPoolItem;
begin
  FObjMap.Lock;
  try
    if FObjMap.ValueMap[pvID] <> nil then
    begin
      Result := -1;
    end else
    begin
      lvItem := TMaxPoolItem.Create;
      lvItem.FData := pvData;
      lvItem.FMax := pvMaxNum;
      lvItem.FCount := 0;
      lvItem.FEnable := True;
      
      if pvOwnObject then
      begin
        lvItem.FDataReleaseType := 1;
      end else
      begin
        lvItem.FDataReleaseType := 0;
      end;
      FObjMap.ValueMap[pvID] := lvItem;

      Result := 0; 
    end;
  finally
    FObjMap.UnLock;
  end;  
end;

function TMaxObjectPool.CheckRemoveObject(pvID:string): Integer;
var
  lvItem:TMaxPoolItem;
begin
  FObjMap.Lock;
  try
    lvItem := TMaxPoolItem(FObjMap.ValueMap[pvID]);
    if lvItem = nil then
    begin
      Result := -1;
    end else
    begin
      if lvItem.FCount> 0 then
      begin
        Result := -2;
      end; 
      lvItem.Free;
      FObjMap.Remove(pvID);
      Result := 0;
    end;                     
  finally
    FObjMap.UnLock;
  end;
end;

function TMaxObjectPool.CheckGetObject(pvID: string): TObject;
var
  lvItem:TMaxPoolItem;
begin
  Result := nil;
  FObjMap.Lock;
  try
    lvItem :=TMaxPoolItem(FObjMap.ValueMap[pvID]);
    if lvItem <> nil then
    begin
      if not lvItem.FEnable then Exit;
      if (lvItem.FMax >0) and (lvItem.FCount >= lvItem.FMax) then Exit;
      Result := lvItem.FData;
      Inc(lvItem.FCount);
      Inc(FOutCounter);
    end;
  finally
    FObjMap.UnLock;
  end;
end;

function TMaxObjectPool.Clear: Integer;
begin
  FObjMap.Lock;
  try
    if FOutCounter > 0 then
    begin
      Result := -1;
      Exit;
    end else
    begin
      FObjMap.FreeAllDataAsObject;
      FObjMap.Clear;
      Result := 0;
    end;
  finally
    FObjMap.UnLock;
  end;
end;



procedure InnerGetPoolInfo(const sender:Pointer; const v:Pointer);
var
  lvSB:TDStringBuilder;
  lvBucket:PDHashData;
  lvItem:TMaxPoolItem;
begin
  lvSB := TDStringBuilder(v);
  lvBucket := PDHashData(sender);
  lvItem := TMaxPoolItem(lvBucket.Data);
  lvSB.Append(lvBucket.Key).Append(':');
  lvSB.Append('use:').Append(lvItem.FCount).Append('/').Append(lvItem.FMax).Append(lvSB.LineBreak);
end;

function TMaxObjectPool.GetPoolInfo(pvSpliteStr: String = sLineBreak): String;
var
  lvSB:TDStringBuilder;
begin
  Result := '';
  FObjMap.Lock;
  try
    lvSB := TDStringBuilder.Create;
    try
      lvSB.LineBreak := pvSpliteStr;
      FObjMap.ForEach(InnerGetPoolInfo, Pointer(lvSB));
      Result := lvSB.ToString;
    finally
      lvSB.Free;
    end;

//
//    lvItem :=TMaxPoolItem(FObjMap.ValueMap[pvID]);
//    if lvItem <> nil then
//    begin
//      if not lvItem.FEnable then Exit;
//      if (lvItem.FMax >0) and (lvItem.FCount >= lvItem.FMax) then Exit;
//      Result := lvItem.FData;
//      Inc(lvItem.FCount);
//      Inc(FOutCounter);
//    end;
  finally
    FObjMap.UnLock;
  end;
end;

function TMaxObjectPool.ReleaseObject(pvID:String; pvObject:TObject): Integer;
var
  lvItem:TMaxPoolItem;
begin
  FObjMap.Lock;
  try
    lvItem :=TMaxPoolItem(FObjMap.ValueMap[pvID]);
    if lvItem <> nil then
    begin
      if (pvObject <> nil) and (lvItem.FData <> pvObject) then
      begin
        Result := -1;
      end else
      begin
        if (lvItem.FCount = 0) then
        begin
          Result := -2;
        end else
        begin
          Dec(lvItem.FCount);
          Dec(FOutCounter);
          Result := 0;
        end;
      end;
    end else
    begin
      Result := -3;
    end;
  finally
    FObjMap.UnLock;
  end;
end;

function TMaxObjectPool.WaitForRelease(pvTimeOut:Integer): Integer;
var
  lvTick:Cardinal;
begin
  Result := -1;
  lvTick := GetTickCount;
  while True do
  begin
    if FOutCounter = 0 then
    begin
      Result := 0;
      Break;
    end else
    begin
      if (tick_diff(lvTick, GetTickCount) >= Cardinal(pvTimeOut)) then
      begin
        Result := -1;
        Exit;
      end;
    end;
    Sleep(0);
  end;
end;

destructor TMaxPoolItem.Destroy;
begin
  if FDataReleaseType = 1 then
  begin
    try
      FreeObject(FData);
    except
    end;
  end;
  inherited Destroy;
end;

initialization
  __ProcessIDStr := IntToStr(GetCurrentProcessId);

end.
