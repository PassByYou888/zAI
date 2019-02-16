type
  TComputeDispatchData = record
    OnRunCall: TRunWithThreadCall;
    OnRunMethod: TRunWithThreadMethod;
{$IFNDEF FPC} OnRunProc: TRunWithThreadProc; {$ENDIF FPC}
    OnDoneCall: TRunWithThreadCall;
    OnDoneMethod: TRunWithThreadMethod;
{$IFNDEF FPC} OnDoneProc: TRunWithThreadProc; {$ENDIF FPC}
    UserData: Pointer;
    UserObject: TCoreClassObject;
    procedure Init;
  end;

  PComputeDispatchData = ^TComputeDispatchData;

{$IFDEF FPC}
  TCoreComputeThreadPool = specialize TGenericsList<TComputeThread>;
  TComputeDispatchDataPool = specialize TGenericsList<PComputeDispatchData>;
{$ELSE FPC}
  TCoreComputeThreadPool = TGenericsObjectList<TComputeThread>;
  TComputeDispatchDataPool = TGenericsList<PComputeDispatchData>;
{$ENDIF FPC}

  TComputeDispatchTh = class(TCoreClassThread)
  protected
    function PickedIdleThread: TComputeThread;
    procedure Execute; override;
  end;

var
  CoreThreadPool: TCoreComputeThreadPool;
  CoreRuning: Boolean;

  ComputeDispatchCritical: TCritical;
  ComputeDispatchDataPool: TComputeDispatchDataPool;
  ComputeDispatchTh: TComputeDispatchTh;

procedure TComputeDispatchData.Init;
begin
  OnRunCall := nil;
  OnRunMethod := nil;
{$IFNDEF FPC} OnRunProc := nil; {$ENDIF FPC}
  OnDoneCall := nil;
  OnDoneMethod := nil;
{$IFNDEF FPC} OnDoneProc := nil; {$ENDIF FPC}
  UserData := nil;
  UserObject := nil;
end;

function TComputeDispatchTh.PickedIdleThread: TComputeThread;
var
  i: Integer;
begin
  while CoreRuning do
    begin
      for i := 0 to CoreThreadPool.Count - 1 do
        begin
          if CoreThreadPool[i].Suspended then
            begin
              Result := CoreThreadPool[i];
              exit;
            end;
        end;
    end;
  Result := nil;
end;

procedure TComputeDispatchTh.Execute;
var
  p: PComputeDispatchData;
  cTh: TComputeThread;
  i: Integer;
begin
  while CoreRuning do
    begin
      if ComputeDispatchDataPool.Count > 0 then
        begin
          ComputeDispatchCritical.Acquire;
          p := ComputeDispatchDataPool[0];
          ComputeDispatchDataPool.Delete(0);
          ComputeDispatchCritical.Release;

          if not CoreRuning then
              break;

          cTh := PickedIdleThread();

          if cTh = nil then
              break;

          cTh.OnRunCall := p^.OnRunCall;
          cTh.OnRunMethod := p^.OnRunMethod;
{$IFNDEF FPC} cTh.OnRunProc := p^.OnRunProc; {$ENDIF FPC}
          cTh.OnDoneCall := p^.OnDoneCall;
          cTh.OnDoneMethod := p^.OnDoneMethod;
{$IFNDEF FPC} cTh.OnDoneProc := p^.OnDoneProc; {$ENDIF FPC}
          cTh.UserData := p^.UserData;
          cTh.UserObject := p^.UserObject;
          dispose(p);
          cTh.Suspended := False;
        end
      else
          TCoreClassThread.Sleep(1);
    end;

  ComputeDispatchCritical.Acquire;
  for i := 0 to ComputeDispatchDataPool.Count - 1 do
      dispose(ComputeDispatchDataPool[i]);
  ComputeDispatchDataPool.Clear;
  ComputeDispatchCritical.Release;

  ComputeDispatchTh := nil;
end;

procedure PostComputeDispatchData(const data: TComputeDispatchData);
var
  p: PComputeDispatchData;
begin
  new(p);
  p^ := data;
  ComputeDispatchCritical.Acquire;
  ComputeDispatchDataPool.Add(p);
  ComputeDispatchCritical.Release;
end;

procedure InitCoreThreadPool(Thread_Num: Integer);
var
  th: TComputeThread;
  i: Integer;
begin
  CoreThreadPool := TCoreComputeThreadPool.Create;
  CoreRuning := True;
  for i := 0 to Thread_Num - 1 do
    begin
      th := TComputeThread.Create;
      CoreThreadPool.Add(th);
    end;

  ComputeDispatchCritical := TCritical.Create;

  ComputeDispatchDataPool := TComputeDispatchDataPool.Create;

  ComputeDispatchTh := TComputeDispatchTh.Create(True);
  ComputeDispatchTh.FreeOnTerminate := True;
  ComputeDispatchTh.Suspended := False;
end;

procedure FreeCoreThreadPool;
var
  th: TComputeThread;
begin
  CoreRuning := False;
  while CoreThreadPool.Count > 0 do
    begin
      th := CoreThreadPool.First;
      if th.Suspended then
          th.Suspended := False;
      CheckThreadSynchronize(1);
    end;
  while ComputeDispatchTh <> nil do
      CheckThreadSynchronize(1);

  disposeObject(CoreThreadPool);
  disposeObject(ComputeDispatchCritical);
  disposeObject(ComputeDispatchDataPool);
end;

procedure TComputeThread.Execute;
begin
  while CoreRuning do
    begin
      try
        if Assigned(OnRunCall) then
            OnRunCall(Self);
        if Assigned(OnRunMethod) then
            OnRunMethod(Self);
{$IFNDEF FPC}
        if Assigned(OnRunProc) then
            OnRunProc(Self);
{$ENDIF FPC}
      except
      end;

      Synchronize({$IFDEF FPC}@{$ENDIF FPC}Done_Sync);

      Suspended := True;
    end;

  try
      Synchronize({$IFDEF FPC}@{$ENDIF FPC}Halt_Sync);
  except
  end;

  OnRunCall := nil;
  OnRunMethod := nil;
{$IFNDEF FPC} OnRunProc := nil; {$ENDIF FPC}
  OnDoneCall := nil;
  OnDoneMethod := nil;
{$IFNDEF FPC} OnDoneProc := nil; {$ENDIF FPC}
  UserData := nil;
  UserObject := nil;
end;

procedure TComputeThread.Done_Sync;
begin
  try
    if Assigned(OnDoneCall) then
        OnDoneCall(Self);
    if Assigned(OnDoneMethod) then
        OnDoneMethod(Self);
{$IFNDEF FPC}
    if Assigned(OnDoneProc) then
        OnDoneProc(Self);
{$ENDIF FPC}
  except
  end;
end;

procedure TComputeThread.Halt_Sync;
begin
  CoreThreadPool.Remove(Self);
end;

constructor TComputeThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;

  OnRunCall := nil;
  OnRunMethod := nil;
{$IFNDEF FPC} OnRunProc := nil; {$ENDIF FPC}
  OnDoneCall := nil;
  OnDoneMethod := nil;
{$IFNDEF FPC} OnDoneProc := nil; {$ENDIF FPC}
  UserData := nil;
  UserObject := nil;
end;

class procedure TComputeThread.RunC(const data: Pointer; const Obj: TCoreClassObject; const OnRun, OnDone: TRunWithThreadCall);
var
  Dispatch_: TComputeDispatchData;
begin
  Dispatch_.Init;
  Dispatch_.OnRunCall := OnRun;
  Dispatch_.OnDoneCall := OnDone;
  Dispatch_.UserData := data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TComputeThread.RunC(const data: Pointer; const Obj: TCoreClassObject; const OnRun: TRunWithThreadCall);
var
  Dispatch_: TComputeDispatchData;
begin
  Dispatch_.Init;
  Dispatch_.OnRunCall := OnRun;
  Dispatch_.UserData := data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TComputeThread.RunM(const data: Pointer; const Obj: TCoreClassObject; const OnRun, OnDone: TRunWithThreadMethod);
var
  Dispatch_: TComputeDispatchData;
begin
  Dispatch_.Init;
  Dispatch_.OnRunMethod := OnRun;
  Dispatch_.OnDoneMethod := OnDone;
  Dispatch_.UserData := data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TComputeThread.RunM(const data: Pointer; const Obj: TCoreClassObject; const OnRun: TRunWithThreadMethod);
var
  Dispatch_: TComputeDispatchData;
begin
  Dispatch_.Init;
  Dispatch_.OnRunMethod := OnRun;
  Dispatch_.UserData := data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

{$IFNDEF FPC}


class procedure TComputeThread.RunP(const data: Pointer; const Obj: TCoreClassObject; const OnRun, OnDone: TRunWithThreadProc);
var
  Dispatch_: TComputeDispatchData;
begin
  Dispatch_.Init;
  Dispatch_.OnRunProc := OnRun;
  Dispatch_.OnDoneProc := OnDone;
  Dispatch_.UserData := data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TComputeThread.RunP(const data: Pointer; const Obj: TCoreClassObject; const OnRun: TRunWithThreadProc);
var
  Dispatch_: TComputeDispatchData;
begin
  Dispatch_.Init;
  Dispatch_.OnRunProc := OnRun;
  Dispatch_.UserData := data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

{$ENDIF FPC}
