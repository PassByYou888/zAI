unit zAI_Reponse_FaceServer;

interface

uses Types, Classes, SysUtils, IOUtils, Threading, Windows,
  CoreClasses, PascalStrings, UnicodeMixedLib, DoStatusIO, DataFrameEngine, PhysicsIO,
  TextDataEngine, ListEngine, zDrawEngine, MemoryRaster, MemoryStream64, Geometry2DUnit, Geometry3DUnit,
  zAI, zAI_Common, zAI_TrainingTask, Learn, LearnTypes, KDTree,
  CommunicationFramework;

type
  TReponse_FaceServer = class;

  TTrainingProcessThread = class(TCoreClassThread)
  protected
    ExecCode: DWord;
    SA: TSecurityAttributes;
    SI: TStartupInfo;
    pi: TProcessInformation;
    StdOutPipeRead, StdOutPipeWrite: THandle;
    procedure Execute; override;
  public
    cmd, workPath: U_String;
    serv: TReponse_FaceServer;
    train_out: SystemString;
    constructor Create;
    destructor Destroy; override;
    procedure Kill;
  end;

  TFaceIOSpecial = class(TPeerIOUserSpecial)
  public
    Face_Stream: TMemoryStream64;
    constructor Create(AOwner: TPeerIO); override;
    destructor Destroy; override;

    procedure Progress; override;

    function GetFaceRaster: TMemoryRaster;
  end;

  TReponse_FaceServer = class(TPhysicsServer)
  private
    Metric: TAI;
    Metric_Resnet_Hnd: TMDNN_Handle;
    FaceDetParallel: TAI_Parallel;
    Face_Learn: TLearn;
    FaceDB: TAI_ImageMatrix;
    TrainRuning: Boolean;
    FaceChanged: Boolean;
    FaceChangedTimeTick: TTimeTick;
    FaceTrainingThread: TTrainingProcessThread;

    procedure cmd_FaceBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);

    procedure DoFaceChanged;
    procedure cmd_SaveFaceTh(ThSender: TComputeThread);
    procedure cmd_SaveFace(Sender: TPeerIO; InData: TDataFrameEngine);

    function CanRunFaceTraining: Boolean;
    function RunFaceTraining(var report: SystemString): Boolean;
    procedure FaceTrainingRunDone(th: TTrainingProcessThread);

    procedure cmd_RecFace_ThRun(ThSender: TStreamCmdThread; ThInData, ThOutData: TDataFrameEngine);
    procedure cmd_RecFace(Sender: TPeerIO; InData, OutData: TDataFrameEngine);

    procedure cmd_GetFaceList(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
    procedure cmd_DownloadFace(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
    procedure cmd_DeleteFace(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
    procedure cmd_UploadFace(Sender: TPeerIO; InData: TDataFrameEngine);

    procedure LoadFaceSystem;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Progress; override;
  end;

function GetAlignmentFaceAndMergeToMatrix(FaceDetParallel: TAI_Parallel; FaceDB: TAI_ImageMatrix;
  picture: TMemoryRaster; face_label: SystemString; Scale4x: Boolean): Boolean;

implementation

function GetAlignmentFaceAndMergeToMatrix(FaceDetParallel: TAI_Parallel; FaceDB: TAI_ImageMatrix;
  picture: TMemoryRaster; face_label: SystemString; Scale4x: Boolean): Boolean;
var
  p_io: TPeerIO;
  f_io: TFaceIOSpecial;
  mr: TMemoryRaster;
  ai: TAI;
  imgL: TAI_ImageList;
  Alignment: TAlignment;
  img: TAI_Image;
  i: Integer;
  near_det: TAI_DetectorDefine;
  m64: TMemoryStream64;
  same_ImgL: TAI_ImageList;
begin
  ai := FaceDetParallel.GetAndLockAI;

  // build alignment
  if Scale4x then
      Alignment := TAlignment_Face.Create(ai)
  else
      Alignment := TAlignment_FastFace.Create(ai);

  // build imagelist
  imgL := TAI_ImageList.Create;
  imgL.AddPicture(picture);

  // run alignment
  Alignment.Alignment(imgL);
  // Calibration face label
  imgL.CalibrationNullToken(face_label);
  imgL.FileInfo := face_label;
  // free alignment
  disposeObject(Alignment);
  FaceDetParallel.UnLockAI(ai);

  img := imgL.First;
  // clip face
  if img.DetectorDefineList.Count > 1 then
    begin
      near_det := img.DetectorDefineList[0];

      for i := 1 to img.DetectorDefineList.Count - 1 do
        if Vec2Distance(RectCentre(near_det.R), img.Raster.Centroid) >
          Vec2Distance(RectCentre(img.DetectorDefineList[i].R), img.Raster.Centroid) then
            near_det := img.DetectorDefineList[i];

      i := 0;
      while i < img.DetectorDefineList.Count do
        begin
          if img.DetectorDefineList[i] <> near_det then
            begin
              disposeObject(img.DetectorDefineList[i]);
              img.DetectorDefineList.Delete(i);
            end
          else
              inc(i);
        end;
    end;

  // merge to db
  if img.DetectorDefineList.Count = 1 then
    begin
      LockObject(FaceDB);
      same_ImgL := FaceDB.FindImageList(imgL.FileInfo);
      if same_ImgL <> nil then
        begin
          same_ImgL.Import(imgL);
          // clip old picture
          while same_ImgL.Count > 10 do
              same_ImgL.Delete(0);
          disposeObject(imgL);
        end
      else
        begin
          FaceDB.Add(imgL);
        end;

      UnLockObject(FaceDB);
      Result := True;
    end
  else
    begin
      disposeObject(imgL);
      Result := False;
    end;
end;

procedure TTrainingProcessThread.Execute;
const
  BuffSize = $FFFF;
var
  WasOK: Boolean;
  buffer: array [0 .. BuffSize] of Byte;
  BytesRead: Cardinal;
  line, n: TPascalString;
begin
  TThread.Synchronize(Self, procedure
    begin
      DoStatus(cmd);
    end);
  with SA do
    begin
      nLength := SizeOf(SA);
      bInheritHandle := True;
      lpSecurityDescriptor := nil;
    end;

  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0);

  try
    with SI do
      begin
        FillChar(SI, SizeOf(SI), 0);
        CB := SizeOf(SI);
        dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
        wShowWindow := SW_HIDE;
        hStdInput := GetStdHandle(STD_INPUT_HANDLE);
        hStdOutput := StdOutPipeWrite;
        hStdError := StdOutPipeWrite;
      end;

    WasOK := CreateProcess(nil, PWideChar(cmd.Text), nil, nil, True, 0, nil, PWideChar(workPath.Text), SI, pi);
    CloseHandle(StdOutPipeWrite);

    if WasOK then
      begin
        try
          repeat
            WasOK := ReadFile(StdOutPipeRead, buffer, BuffSize, BytesRead, nil);
            if (WasOK) and (BytesRead > 0) then
              begin
                buffer[BytesRead] := 0;
                OemToAnsi(@buffer, @buffer);
                line.Append(strPas(PAnsiChar(@buffer)));

                while line.Exists(#10) do
                  begin
                    n := umlGetFirstStr_Discontinuity(line, #10).DeleteChar(#13);
                    line := umlDeleteFirstStr_Discontinuity(line, #10);
                    TThread.Synchronize(Self, procedure
                      begin
                        DoStatus(n);
                      end);
                  end;
              end;
          until (not WasOK) or (BytesRead = 0);
          WaitForSingleObject(pi.hProcess, Infinite);
          GetExitCodeProcess(pi.hProcess, ExecCode);
        finally
          CloseHandle(pi.hThread);
          CloseHandle(pi.hProcess);
          TThread.Synchronize(Self, procedure
            begin
              serv.FaceTrainingRunDone(Self);
            end);
        end;
      end
    else
      begin
        ExecCode := 0;
      end;
  finally
      CloseHandle(StdOutPipeRead);
  end;
end;

constructor TTrainingProcessThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  cmd := '';
  workPath := umlCurrentPath;
  serv := nil;
  train_out := '';
end;

destructor TTrainingProcessThread.Destroy;
begin
  inherited Destroy;
end;

procedure TTrainingProcessThread.Kill;
begin
  TerminateProcess(pi.hProcess, 0);
end;

function TFaceIOSpecial.GetFaceRaster: TMemoryRaster;
begin
  Face_Stream.Position := 0;
  try
      Result := NewRasterFromStream(Face_Stream);
  except
      Result := nil;
  end;
end;

constructor TFaceIOSpecial.Create(AOwner: TPeerIO);
begin
  inherited Create(AOwner);
  Face_Stream := TMemoryStream64.Create;
end;

destructor TFaceIOSpecial.Destroy;
begin
  disposeObject(Face_Stream);
  inherited Destroy;
end;

procedure TFaceIOSpecial.Progress;
begin
  inherited Progress;
end;

procedure TReponse_FaceServer.cmd_FaceBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  f_io: TFaceIOSpecial;
begin
  f_io := TFaceIOSpecial(Sender.UserSpecial);
  f_io.Face_Stream.Clear;
  f_io.Face_Stream.WritePtr(InData, DataSize);
  f_io.Face_Stream.Position := 0;
end;

procedure TReponse_FaceServer.DoFaceChanged;
begin
  FaceChanged := True;
  FaceChangedTimeTick := GetTimeTick();
end;

type
  TFaceSaveData = record
    face_label: U_String;
    Scale4x: Boolean;
    mr: TMemoryRaster;
  end;

  PFaceSaveData = ^TFaceSaveData;

procedure TReponse_FaceServer.cmd_SaveFaceTh(ThSender: TComputeThread);
var
  p: PFaceSaveData;
begin
  p := PFaceSaveData(ThSender.UserData);

  if GetAlignmentFaceAndMergeToMatrix(FaceDetParallel, FaceDB, p^.mr, p^.face_label, p^.Scale4x) then
    begin
      DoStatus('Save face done.');
      TThread.Synchronize(ThSender, DoFaceChanged);
    end
  else
    begin
      DoStatus('no detection face.');
    end;
  disposeObject(p^.mr);
  dispose(p);
end;

procedure TReponse_FaceServer.cmd_SaveFace(Sender: TPeerIO; InData: TDataFrameEngine);
var
  p: PFaceSaveData;
begin
  new(p);

  p^.face_label := umlTrimSpace(InData.Reader.ReadString);
  if p^.face_label.L = 0 then
    begin
      DoStatus('Invalid face label.');
      dispose(p);
      exit;
    end;
  p^.Scale4x := InData.Reader.ReadBool;
  p^.mr := TFaceIOSpecial(Sender.UserSpecial).GetFaceRaster;

  TComputeThread.RunM(p, nil, cmd_SaveFaceTh);
end;

function TReponse_FaceServer.CanRunFaceTraining: Boolean;
var
  tokens: TArrayPascalString;
begin
  Result := False;

  if TrainRuning then
      exit;

  if not umlFileExists(AI_TrainingTool) then
      exit;

  if FaceDB.FoundNoTokenDefine then
      exit;

  tokens := FaceDB.DetectorTokens;
  if length(tokens) < 2 then
      exit;
  SetLength(tokens, 0);

  Result := True;
end;

function TReponse_FaceServer.RunFaceTraining(var report: SystemString): Boolean;
var
  tokens: TArrayPascalString;
  tt: TTrainingTask;
  Param: THashVariantList;

  d: TDateTime;
  Year, Month, Day: Word;

  i: Integer;

  dataSour: U_String;
  datafile: U_String;
  train_out: U_String;

  th: TTrainingProcessThread;
begin
  Result := False;
  if TrainRuning then
    begin
      report := 'other training mission is runing.';
      exit;
    end;

  tokens := FaceDB.DetectorTokens;

  if length(tokens) < 2 then
    begin
      report := 'face classification count < 2';
      exit;
    end;
  SetLength(tokens, 0);

  if FaceDB.FoundNoTokenDefine then
    begin
      report := 'invalid face label.';
      exit;
    end;

  if not umlFileExists(AI_TrainingTool) then
    begin
      report := PFormat('no found training tool:%s', [AI_TrainingTool.Text]);
      exit;
    end;

  DoStatus('build training data.');

  tt := TTrainingTask.CreateTask;
  Param := THashVariantList.Create;
  Param.SetDefaultValue('ComputeFunc', 'TrainMRN');
  Param.SetDefaultValue('source', 'input' + zAI_Common.C_ImageMatrix_Ext);
  Param.SetDefaultValue('syncfile', 'output' + C_Metric_Ext + '.sync');
  Param.SetDefaultValue('output', 'output' + C_Metric_Ext);
  Param.SetDefaultValue('timeout', 'e"1000*60*60*24*7"');
  Param.SetDefaultValue('weight_decay', 0.0001);
  Param.SetDefaultValue('momentum', 0.9);
  Param.SetDefaultValue('iterations_without_progress_threshold', 300);
  Param.SetDefaultValue('learning_rate', 0.1);
  Param.SetDefaultValue('completed_learning_rate', 0.0001);
  Param.SetDefaultValue('step_mini_batch_target_num', 5);
  Param.SetDefaultValue('step_mini_batch_raster_num', 5);
  Param.SetDefaultValue('fullGPU_Training', False);

  tt.Write('param.txt', Param);
  disposeObject(Param);

  LockObject(FaceDB);
  tt.Write('input' + zAI_Common.C_ImageMatrix_Ext, FaceDB, False);
  UnLockObject(FaceDB);

  d := umlNow();
  DecodeDate(d, Year, Month, Day);

  // set input file
  datafile := umlCombineFileName(umlGetFilePath(AI_TrainingTool), PFormat('face-Training %d-%d-%d.input', [Year, Month, Day]));
  i := 1;
  while umlFileExists(datafile) do
    begin
      train_out := umlChangeFileExt(datafile, PFormat('.input(%d)', [i]));
      inc(i);
    end;

  // save training task
  tt.SaveToFile(datafile);
  disposeObject(tt);

  // set output file
  i := 1;
  train_out := umlChangeFileExt(datafile, '.output');
  while umlFileExists(train_out) do
    begin
      train_out := umlChangeFileExt(datafile, PFormat('.output(%d)', [i]));
      inc(i);
    end;

  TrainRuning := True;
  FaceChanged := False;

  FaceTrainingThread := TTrainingProcessThread.Create;
  FaceTrainingThread.cmd := PFormat('"%s" "-ai:%s" "-i:%s" "-p:param.txt" "-o:%s" "-k:%s" "-product:%s"',
    [AI_TrainingTool.Text, AI_Engine_Library.Text, datafile.Text, train_out.Text, AI_UserKey.Text, 'TrainingTool']);
  FaceTrainingThread.workPath := umlGetFilePath(AI_TrainingTool);
  FaceTrainingThread.serv := Self;
  FaceTrainingThread.train_out := train_out;
  FaceTrainingThread.Suspended := False;

  report := 'solve.';

  Result := True;
end;

procedure TReponse_FaceServer.FaceTrainingRunDone(th: TTrainingProcessThread);
var
  tt: TTrainingTask;
  report: SystemString;
  check_result_successed: Boolean;
  m64: TMemoryStream64;
  n_metric: TMDNN_Handle;
  fn: U_String;

  tokens: TArrayPascalString;
  n: TPascalString;
begin
  DoStatus('Training done: "%s"', [th.cmd.Text]);
  if th.ExecCode = 1 then
    begin
      // trainer result
      tt := TTrainingTask.OpenTask(th.train_out);
      DoStatus('check training result.');
      check_result_successed := tt.CheckTrainingAfter('param.txt', report);
      DoStatus(report);
      if check_result_successed then
        begin
          m64 := TMemoryStream64.Create;
          tt.Read('output' + C_Metric_Ext, m64);

          DoStatus('rebuild metric.');
          n_metric := Metric.Metric_ResNet_Open_Stream(m64);
          Metric.Metric_ResNet_Close(Metric_Resnet_Hnd);
          Metric_Resnet_Hnd := n_metric;
          fn := umlCombineFileName(TPath.GetLibraryPath, 'face' + C_Metric_Ext);
          m64.SaveToFile(fn);
          disposeObject(m64);

          DoStatus('rebuild face vector.');
          Face_Learn.Clear;
          Metric.Metric_ResNet_SaveDetectorDefineToLearnEngine(Metric_Resnet_Hnd, False, FaceDB, Face_Learn);
          Face_Learn.Train();
          fn := umlCombineFileName(TPath.GetLibraryPath, 'face.learn');
          Face_Learn.SaveToFile(fn);

          DoStatus('reload finished.');

          if not FaceChanged then
            begin
              LockObject(FaceDB);
              tokens := FaceDB.DetectorTokens;
              DoStatus('total %d classifier', [length(tokens)]);
              for n in tokens do
                  DoStatus('"%s" include %d of face picture', [n.Text, FaceDB.GetDetectorTokenCount(n)]);
              UnLockObject(FaceDB);
            end;
        end;
      disposeObject(tt);
    end
  else
    begin
      // error
      DoStatus('training termination.');
    end;

  TrainRuning := False;
  FaceTrainingThread := nil;
end;

procedure TReponse_FaceServer.cmd_RecFace_ThRun(ThSender: TStreamCmdThread; ThInData, ThOutData: TDataFrameEngine);
type
  TFace_Result = record
    k: TLFloat;
    token: SystemString;
    R: TRectV2;
  end;
var
  p_io: TPeerIO;
  f_io: TFaceIOSpecial;
  depthRec: Boolean;
  mr: TMemoryRaster;
  ai: TAI;

  function RunRec(const k: TGeoFloat): Boolean;
  var
    face_hnd: TFace_handle;
    face_arry: TMemoryRasterArray;
    face_matrix: TLMatrix;
    face_Result: array of TFace_Result;
    i: Integer;
  begin
    Result := False;
    DoStatus('detector face from %d * %d', [mr.Width, mr.Height]);
    face_hnd := ai.Face_Detector_All(mr);

    if face_hnd <> nil then
      begin
        if ai.Face_chips_num(face_hnd) > 0 then
          begin
            // build face chip
            SetLength(face_arry, ai.Face_chips_num(face_hnd));
            for i := 0 to ai.Face_chips_num(face_hnd) - 1 do
                face_arry[i] := ai.Face_chips(face_hnd, i);

            // process metric
            TThread.Synchronize(ThSender.Thread, procedure
              begin
                face_matrix := ai.Metric_ResNet_Process(Metric_Resnet_Hnd, face_arry);
                SetLength(face_Result, length(face_matrix));
                TParallel.for(Low(face_matrix), high(face_matrix), procedure(pass: Integer)
                  var
                    LIndex: TLInt;
                    p: TLearn.PLearnMemory;
                  begin
                    LIndex := Face_Learn.ProcessMaxIndex(face_matrix[pass]);
                    p := Face_Learn.MemorySource[LIndex];
                    face_Result[pass].k := LDistance(face_matrix[pass], p^.m_in);
                    face_Result[pass].token := p^.token;
                    face_Result[pass].R := ai.Face_RectV2(face_hnd, pass);
                  end);
              end);

            // free chip
            for i := low(face_arry) to high(face_arry) do
                disposeObject(face_arry[i]);

            // close face handle
            ai.Face_Close(face_hnd);

            // process OutData
            ThOutData.WriteBool(True);

            for i := low(face_Result) to high(face_Result) do
              begin
                ThOutData.WriteString(face_Result[i].token);
                ThOutData.WriteDouble(face_Result[i].k);
                ThOutData.WriteRectV2(RectMul(face_Result[i].R, k));
              end;

            Result := True;
          end
        else
          begin
            // close face handle
            ai.Face_Close(face_hnd);

            if (depthRec) and (mr.Width * mr.Height < 1200 * 1200) then
              begin
                mr.Scale(2.0);
                if RunRec(k * 0.5) then
                    exit;
              end;

            ThOutData.WriteBool(False);
            ThOutData.WriteString('no detection face.');
          end;
      end
    else
      begin
        ThOutData.WriteBool(False);
        ThOutData.WriteString('no detection face.');
      end;
  end;

begin
  if Metric_Resnet_Hnd = nil then
    begin
      ThOutData.WriteBool(False);
      ThOutData.WriteString('no metric net.');
      exit;
    end;

  TThread.Synchronize(ThSender.Thread, procedure
    begin
      p_io := TPeerIO(ThSender.Framework.IOPool[ThSender.workID]);
      if p_io = nil then
          exit;
      f_io := TFaceIOSpecial(p_io.UserSpecial);

      mr := f_io.GetFaceRaster;
    end);

  if mr = nil then
    begin
      ThOutData.WriteBool(False);
      ThOutData.WriteString('error image.');
      exit;
    end;

  depthRec := ThInData.Reader.ReadBool;

  ai := FaceDetParallel.GetAndLockAI;

  try
      RunRec(1.0);
  finally
    FaceDetParallel.UnLockAI(ai);
    disposeObject(mr);
  end;
end;

procedure TReponse_FaceServer.cmd_RecFace(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
begin
  RunStreamWithDelayThreadM(Sender, nil, nil, InData, OutData, cmd_RecFace_ThRun);
end;

procedure TReponse_FaceServer.cmd_GetFaceList(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  tokens: TArrayPascalString;
  n: TPascalString;
begin
  LockObject(FaceDB);
  tokens := FaceDB.DetectorTokens;
  UnLockObject(FaceDB);

  for n in tokens do
      OutData.WriteString(n);
end;

procedure TReponse_FaceServer.cmd_DownloadFace(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  token: SystemString;
  imgL: TAI_ImageList;
  m64: TMemoryStream64;
begin
  token := InData.Reader.ReadString;
  LockObject(FaceDB);
  imgL := FaceDB.FindImageList(token);
  if imgL <> nil then
    begin
      OutData.WriteBool(True);
      m64 := TMemoryStream64.Create;
      imgL.SaveToStream(m64, True, True, TRasterSave.rsJPEG_RGB_Qualily90);
      OutData.WriteStream(m64);
      disposeObject(m64);
    end
  else
    begin
      OutData.WriteBool(False);
      OutData.WriteString(PFormat('no found "%s"', [token]));
    end;
  UnLockObject(FaceDB);
end;

procedure TReponse_FaceServer.cmd_DeleteFace(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  filter: U_String;
  i: Integer;
  c: Integer;
begin
  filter := InData.Reader.ReadString;
  LockObject(FaceDB);
  c := 0;
  i := 0;
  while i < FaceDB.Count do
    begin
      if umlMultipleMatch(filter, FaceDB[i].FileInfo) then
        begin
          disposeObject(FaceDB[i]);
          FaceDB.Delete(i);
          inc(c);
          DoFaceChanged;
        end;
      inc(i);
    end;
  UnLockObject(FaceDB);
  OutData.WriteInteger(c);
end;

procedure TReponse_FaceServer.cmd_UploadFace(Sender: TPeerIO; InData: TDataFrameEngine);
var
  faceToken: U_String;
  m64: TMemoryStream64;
  imgL: TAI_ImageList;
  i: Integer;
begin
  faceToken := InData.Reader.ReadString;
  m64 := TMemoryStream64.Create;
  InData.Reader.ReadStream(m64);
  m64.Position := 0;
  imgL := TAI_ImageList.Create;
  imgL.LoadFromStream(m64, True);
  imgL.FileInfo := faceToken;
  disposeObject(m64);

  LockObject(FaceDB);
  i := 0;
  while i < FaceDB.Count do
    begin
      if faceToken.Same(FaceDB[i].FileInfo) then
        begin
          disposeObject(FaceDB[i]);
          FaceDB.Delete(i);
        end;
      inc(i);
    end;
  FaceDB.Add(imgL);
  UnLockObject(FaceDB);

  DoFaceChanged;
end;

procedure TReponse_FaceServer.LoadFaceSystem;
var
  fn: U_String;
  tokens: TArrayPascalString;
  n: TPascalString;
begin
  Metric.Metric_ResNet_Close(Metric_Resnet_Hnd);
  fn := umlCombineFileName(TPath.GetLibraryPath, 'face' + C_Metric_Ext);
  if umlFileExists(fn) then
    begin
      Metric_Resnet_Hnd := Metric.Metric_ResNet_Open_Stream(fn);
    end;

  Face_Learn.Clear;
  fn := umlCombineFileName(TPath.GetLibraryPath, 'face.learn');
  if umlFileExists(fn) then
    begin
      Face_Learn.LoadFromFile(fn);
      Face_Learn.Train();
    end;

  fn := umlCombineFileName(TPath.GetLibraryPath, 'face' + C_ImageMatrix_Ext);
  if umlFileExists(fn) then
    begin
      FaceDB.LoadFromFile(fn);

      tokens := FaceDB.DetectorTokens;
      DoStatus('total %d classifier', [length(tokens)]);
      for n in tokens do
          DoStatus('"%s" include %d of face picture', [n.Text, FaceDB.GetDetectorTokenCount(n)]);
    end;
end;

constructor TReponse_FaceServer.Create;
var
  pic: TMemoryRaster;
  report: SystemString;
begin
  inherited Create;
  MaxCompleteBufferSize := 8 * 1024 * 1024; // 8M complete buffer
  SwitchMaxPerformance;
  SendDataCompressed := True;
  SyncOnCompleteBuffer := True;
  SyncOnResult := True;
  UserSpecialClass := TFaceIOSpecial;

  RegisterCompleteBuffer('FaceBuffer').OnExecute := cmd_FaceBuffer;
  RegisterDirectStream('SaveFace').OnExecute := cmd_SaveFace;
  RegisterStream('RecFace').OnExecute := cmd_RecFace;
  RegisterStream('GetFaceList').OnExecute := cmd_GetFaceList;
  RegisterStream('DownloadFace').OnExecute := cmd_DownloadFace;
  RegisterStream('DeleteFace').OnExecute := cmd_DeleteFace;
  RegisterDirectStream('UploadFace').OnExecute := cmd_UploadFace;

  // init ai
  Metric := TAI.OpenEngine();
  Metric_Resnet_Hnd := nil;
  FaceDetParallel := TAI_Parallel.Create;
  FaceDetParallel.Prepare_Parallel(10);
  FaceDetParallel.Prepare_Face;
  Face_Learn := TLearn.CreateClassifier(TLearnType.ltKDT, zAI.C_Metric_Dim);

  // face matrix database
  FaceDB := TAI_ImageMatrix.Create;

  // load face on disk
  LoadFaceSystem;

  TrainRuning := False;
  FaceChanged := False;
end;

destructor TReponse_FaceServer.Destroy;
begin
  StopService;
  disposeObject(Metric);
  disposeObject(FaceDetParallel);
  disposeObject(FaceDB);
  inherited Destroy;
end;

procedure TReponse_FaceServer.Progress;
var
  report: SystemString;
  fn: U_String;
  tokens: TArrayPascalString;
  n: TPascalString;
begin
  inherited Progress;

  if FaceChanged and (GetTimeTick - FaceChangedTimeTick > 30 * C_Tick_Second) and CanRunFaceTraining() then
    begin
      fn := umlCombineFileName(TPath.GetLibraryPath, 'face' + C_ImageMatrix_Ext);
      LockObject(FaceDB);
      FaceDB.SaveToFile(fn);

      tokens := FaceDB.DetectorTokens;
      DoStatus('total %d classifier', [length(tokens)]);
      for n in tokens do
          DoStatus('"%s" include %d of face picture', [n.Text, FaceDB.GetDetectorTokenCount(n)]);

      UnLockObject(FaceDB);

      RunFaceTraining(report);
      DoStatus(report);
    end;
end;

end.
