{ ****************************************************************************** }
{ * memory Rasterization                                                       * }
{ * by QQ 600585@qq.com                                                        * }
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

constructor TRaster_IO.Create(Owner_: TRaster_IO_Processor);
begin
  inherited Create;
  Owner := Owner_;
  InputRaster := NewRaster();
  OutputRaster := NewRaster();
  IndexNumber := 0;
end;

destructor TRaster_IO.Destroy;
begin
  if InputRaster <> nil then
      DisposeObjectAndNil(InputRaster);
  if OutputRaster <> nil then
      DisposeObjectAndNil(OutputRaster);
  inherited Destroy;
end;

procedure TRaster_IO.ProcessBefore(UserData: Pointer);
begin

end;

function TRaster_IO.Process(UserData: Pointer): Boolean;
begin
  Result := True;
end;

procedure TRaster_IO.ProcessAfter(UserData: Pointer);
begin

end;

procedure TRaster_IO_Processor.LockInputBuffer;
begin
  LockObject(FInputBuffer);
end;

procedure TRaster_IO_Processor.UnLockInputBuffer;
begin
  UnLockObject(FInputBuffer);
end;

procedure TRaster_IO_Processor.IOProcessorThreadRun(ThSender: TCompute);

  function DoPickBuff(): TRaster_IO_Buffer;
  var
    i: Integer;
  begin
    Result := TRaster_IO_Buffer.Create;

    LockInputBuffer;
    for i := 0 to FInputBuffer.Count - 1 do
        Result.Add(FInputBuffer[i]);
    FInputBuffer.Clear;
    UnLockInputBuffer;
  end;

  procedure DoProcessPick(pickBuff: TRaster_IO_Buffer);
  var
    pass: Integer;
    vio: TRaster_IO;
    processed_ok: Boolean;
  begin
    for pass := 0 to pickBuff.Count - 1 do
      begin
        vio := pickBuff[pass];

        try
          vio.ProcessBefore(ThSender.UserData);
          processed_ok := vio.Process(ThSender.UserData);
          vio.ProcessAfter(ThSender.UserData);
        except
            processed_ok := False;
        end;

        if processed_ok then
          begin
            LockObject(FOutputBuffer);
            FOutputBuffer.Add(vio);
            UnLockObject(FOutputBuffer);
          end
        else
          begin
            DisposeObject(vio);
          end;
      end;
  end;

{$IFDEF Parallel}
  procedure DoParallelProcessPick(pickBuff: TRaster_IO_Buffer);
  var
    tmp_buff: TRaster_IO_Buffer;
    tmp_buff_state: array of Boolean;
{$IFDEF FPC}
    procedure Nested_ParallelFor(pass: Integer);
    var
      vio: TRaster_IO;
    begin
      LockObject(tmp_buff);
      vio := tmp_buff[pass];
      UnLockObject(tmp_buff);

      try
        vio.ProcessBefore(ThSender.UserData);
        tmp_buff_state[pass] := vio.Process(ThSender.UserData);
        vio.ProcessAfter(ThSender.UserData);
      except
          tmp_buff_state[pass] := False;
      end;
    end;
{$ENDIF FPC}

  var
    i: Integer;
  begin
    tmp_buff := pickBuff;
    SetLength(tmp_buff_state, tmp_buff.Count);

{$IFDEF FPC}
    FPCParallelFor(@Nested_ParallelFor, 0, tmp_buff.Count - 1);
{$ELSE FPC}
    DelphiParallelFor(0, tmp_buff.Count - 1, procedure(pass: Integer)
      var
        vio: TRaster_IO;
      begin
        LockObject(tmp_buff);
        vio := tmp_buff[pass];
        UnLockObject(tmp_buff);

        try
          vio.ProcessBefore(ThSender.UserData);
          tmp_buff_state[pass] := vio.Process(ThSender.UserData);
          vio.ProcessAfter(ThSender.UserData);
        except
            tmp_buff_state[pass] := False;
        end;
      end);
{$ENDIF FPC}
    LockObject(FOutputBuffer);
    for i := 0 to tmp_buff.Count - 1 do
      begin
        if tmp_buff_state[i] then
            FOutputBuffer.Add(tmp_buff[i])
        else
            DisposeObject(tmp_buff[i]);
      end;
    UnLockObject(FOutputBuffer);
    SetLength(tmp_buff_state, 0);
  end;
{$ENDIF Parallel}


var
  pickList: TRaster_IO_Buffer;
begin
  while (InputCount > 0) do
    begin
      pickList := DoPickBuff();
{$IFDEF Parallel}
      if FParallelProcessor then
          DoParallelProcessPick(pickList)
      else
{$ENDIF Parallel}
          DoProcessPick(pickList);
      DisposeObject(pickList);
      TCoreClassThread.Sleep(10);
    end;
  FIOThreadRuning.V := False;
end;

constructor TRaster_IO_Processor.Create(IO_Class_: TRaster_IO_Class);
begin
  inherited Create;
  FIO_Class := IO_Class_;
  FInputBuffer := TRaster_IO_Buffer.Create;
  FOutputBuffer := TRaster_IO_Buffer.Create;
  FIOThreadRuning := TAtomBool.Create(False);
  FParallelProcessor := False;
  FIndexNumber := 0;
end;

destructor TRaster_IO_Processor.Destroy;
begin
  while FIOThreadRuning.V do
      CheckThreadSynchronize(1);

  Clear;
  DisposeObject(FInputBuffer);
  DisposeObject(FOutputBuffer);
  DisposeObject(FIOThreadRuning);
  inherited Destroy;
end;

procedure TRaster_IO_Processor.Clear;
var
  i: Integer;
begin
  LockInputBuffer;
  LockObject(FOutputBuffer);

  for i := 0 to FInputBuffer.Count - 1 do
      DisposeObject(FInputBuffer[i]);
  FInputBuffer.Clear;

  for i := 0 to FOutputBuffer.Count - 1 do
      DisposeObject(FOutputBuffer[i]);
  FOutputBuffer.Clear;

  UnLockInputBuffer;
  UnLockObject(FOutputBuffer);
end;

procedure TRaster_IO_Processor.InputPicture(filename: TPascalString);
begin
  Input(NewRasterFromFile(filename), True);
end;

procedure TRaster_IO_Processor.InputPicture(stream: TCoreClassStream);
begin
  Input(NewRasterFromStream(stream), True);
end;

procedure TRaster_IO_Processor.Input(raster: TMemoryRaster; RasterInstance_: Boolean);
var
  vio: TRaster_IO;
begin
  vio := FIO_Class.Create(Self);

  if RasterInstance_ then
    begin
      if vio.InputRaster <> nil then
          DisposeObjectAndNil(vio.InputRaster);
      vio.InputRaster := raster;
    end
  else
      vio.InputRaster.Assign(raster);

  LockInputBuffer;
  inc(FIndexNumber);
  vio.IndexNumber := FIndexNumber;
  FInputBuffer.Add(vio);
  UnLockInputBuffer;
end;

function TRaster_IO_Processor.InputCount: Integer;
begin
  LockInputBuffer;
  Result := FInputBuffer.Count;
  UnLockInputBuffer;
end;

procedure TRaster_IO_Processor.Process(UserData: Pointer);
begin
  if (FIOThreadRuning.V) or (InputCount = 0) then
      exit;

  FIOThreadRuning.V := True;
  TCompute.RunM(UserData, Self, {$IFDEF FPC}@{$ENDIF FPC}IOProcessorThreadRun);
end;

function TRaster_IO_Processor.Finished: Boolean;
begin
  Result := (InputCount = 0) and (not FIOThreadRuning.V);
end;

procedure TRaster_IO_Processor.WaitProcessDone;
begin
  while not Finished do
    begin
      DoStatus();
      CheckThreadSynchronize(10);
    end;
end;

function TRaster_IO_Processor.LockOutputBuffer: TRaster_IO_Buffer;
begin
  LockObject(FOutputBuffer);
  Result := FOutputBuffer;
end;

procedure TRaster_IO_Processor.UnLockOutputBuffer(freeObj_: Boolean);
var
  i: Integer;
begin
  if freeObj_ then
    begin
      for i := 0 to FOutputBuffer.Count - 1 do
          DisposeObject(FOutputBuffer[i]);
      FOutputBuffer.Clear;
    end;
  UnLockObject(FOutputBuffer);
end;
