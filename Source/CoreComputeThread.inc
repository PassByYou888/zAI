type
{$IFDEF FPC}
  TThreadList_Decl = specialize TGenericsList<TComputeThread>;
{$ELSE FPC}
  TThreadList_Decl = TGenericsObjectList<TComputeThread>;
{$ENDIF FPC}


var
  CoreThreadPool: TThreadList_Decl;
  CoreRuning: Boolean;

procedure InitCoreThreadPool(Thread_Num: Integer);
var
  th: TComputeThread;
  i: Integer;
begin
  CoreThreadPool := TThreadList_Decl.Create;
  CoreRuning := True;
  for i := 0 to Thread_Num - 1 do
    begin
      th := TComputeThread.Create;
      CoreThreadPool.Add(th);
    end;
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
      CheckThreadSynchronize();
    end;
  DisposeObject(CoreThreadPool);
end;

function PickCoreThread: TComputeThread; inline;
var
  i: Integer;
begin
  while True do
    begin
      for i := 0 to CoreThreadPool.Count - 1 do
        begin
          if CoreThreadPool[i].Suspended then
            begin
              Result := CoreThreadPool[i];
              exit;
            end;
        end;
      CheckThreadSynchronize();
    end;
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

class function TComputeThread.RunC(const Data: Pointer; const Obj: TCoreClassObject; const OnRun, OnDone: TRunWithThreadCall): TComputeThread;
begin
  Result := PickCoreThread;
  Result.OnRunCall := OnRun;
  Result.OnDoneCall := OnDone;
  Result.UserData := Data;
  Result.UserObject := Obj;
  Result.Suspended := False;
end;

class function TComputeThread.RunC(const Data: Pointer; const Obj: TCoreClassObject; const OnRun: TRunWithThreadCall): TComputeThread;
begin
  Result := PickCoreThread;
  Result.OnRunCall := OnRun;
  Result.OnDoneCall := nil;
  Result.UserData := Data;
  Result.UserObject := Obj;
  Result.Suspended := False;
end;

class function TComputeThread.RunM(const Data: Pointer; const Obj: TCoreClassObject; const OnRun, OnDone: TRunWithThreadMethod): TComputeThread;
begin
  Result := PickCoreThread;
  Result.OnRunMethod := OnRun;
  Result.OnDoneMethod := OnDone;
  Result.UserData := Data;
  Result.UserObject := Obj;
  Result.Suspended := False;
end;

class function TComputeThread.RunM(const Data: Pointer; const Obj: TCoreClassObject; const OnRun: TRunWithThreadMethod): TComputeThread;
begin
  Result := PickCoreThread;
  Result.OnRunMethod := OnRun;
  Result.OnDoneMethod := nil;
  Result.UserData := Data;
  Result.UserObject := Obj;
  Result.Suspended := False;
end;

{$IFNDEF FPC}


class function TComputeThread.RunP(const Data: Pointer; const Obj: TCoreClassObject; const OnRun, OnDone: TRunWithThreadProc): TComputeThread;
begin
  Result := PickCoreThread;
  Result.OnRunProc := OnRun;
  Result.OnDoneProc := OnDone;
  Result.UserData := Data;
  Result.UserObject := Obj;
  Result.Suspended := False;
end;

class function TComputeThread.RunP(const Data: Pointer; const Obj: TCoreClassObject; const OnRun: TRunWithThreadProc): TComputeThread;
begin
  Result := PickCoreThread;
  Result.OnRunProc := OnRun;
  Result.OnDoneProc := nil;
  Result.UserData := Data;
  Result.UserObject := Obj;
  Result.Suspended := False;
end;

{$ENDIF FPC}
