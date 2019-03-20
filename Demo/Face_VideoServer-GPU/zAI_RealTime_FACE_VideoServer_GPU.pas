unit zAI_RealTime_FACE_VideoServer_GPU;

interface

uses Classes, IOUtils, Threading,
  CoreClasses, PascalStrings, UnicodeMixedLib, DoStatusIO, DataFrameEngine,
  zDrawEngine, MemoryRaster, MemoryStream64, Geometry2DUnit, Geometry3DUnit,
  zAI, zAI_Common, Learn, LearnTypes,
  CommunicationFramework, CommunicationFrameworkDoubleTunnelIO_NoAuth;

type
  TRealTime_FACE_VideoServer = class;

  TFACE_VideoIO_ = class(TPeerIOUserSpecial)
  public
    VideoFrames: TMemoryStream64List;
    constructor Create(AOwner: TPeerIO); override;
    destructor Destroy; override;

    procedure Progress; override;
    procedure ClearVideoFrames;
  end;

  TRealTime_FACE_VideoServer = class(TCommunicationFramework_DoubleTunnelService_NoAuth)
  private
    // work on cpu
    face_sp_para: TAI_Parallel;
    // work on gpu
    Face_Metric: TAI;
    Face_MMOD_Hnd: TMMOD_Handle;
    Face_MDNN_Hnd: TMDNN_Handle;
    Face_LearnEng: TLearn;

    procedure cmd_VideoBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);

    procedure Process_OD_Video(ThSender: TComputeThread);
    procedure cmd_OD(Sender: TPeerIO; InData: TDataFrameEngine);
  public
    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
    destructor Destroy; override;

    procedure Progress; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    procedure SendVideo(send_id: Cardinal; frame: TMemoryStream64); overload;
  end;

implementation

procedure TFACE_VideoIO_.ClearVideoFrames;
var
  i: Integer;
begin
  for i := 0 to VideoFrames.Count - 1 do
      DisposeObject(VideoFrames[i]);
  VideoFrames.Clear;
end;

constructor TFACE_VideoIO_.Create(AOwner: TPeerIO);
begin
  inherited Create(AOwner);
  VideoFrames := TMemoryStream64List.Create;
end;

destructor TFACE_VideoIO_.Destroy;
begin
  ClearVideoFrames;
  DisposeObject(VideoFrames);
  inherited Destroy;
end;

procedure TFACE_VideoIO_.Progress;
begin
  inherited Progress;
end;

procedure TRealTime_FACE_VideoServer.cmd_VideoBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.WritePtr(InData, DataSize);
  m64.Position := 0;

  TFACE_VideoIO_(Sender.UserSpecial).VideoFrames.Add(m64);
end;

procedure TRealTime_FACE_VideoServer.Process_OD_Video(ThSender: TComputeThread);
type
  TFaceQueryData = record
    r: TRectV2;
    token: SystemString;
    Accuracy: TLFloat;
  end;

var
  p_recv_id: PCardinal;
  send_id: Cardinal;
  m64: TMemoryStream64;
  mr: TMemoryRaster;
  mmod_desc: TMMOD_Desc;
  ai: TAI;
  face_hnd: TFACE_Handle;
  face_arry: TMemoryRasterArray;
  face_query_arry: array of TFaceQueryData;
  d: TDrawEngine;
  i: Integer;
begin
  p_recv_id := ThSender.UserData;

  m64 := nil;

  TThread.Synchronize(ThSender, procedure
    var
      p_io: TPeerIO;
      v_io: TFACE_VideoIO_;
      j: Integer;
    begin
      p_io := RecvTunnel[p_recv_id^];
      if p_io = nil then
          exit;

      if not GetUserDefineRecvTunnel(p_io).LinkOk then
          exit;

      send_id := GetUserDefineRecvTunnel(p_io).SendTunnelID;

      v_io := TFACE_VideoIO_(p_io.UserSpecial);
      if v_io.VideoFrames.Count = 0 then
          exit;

      m64 := v_io.VideoFrames.Last;

      for j := 0 to v_io.VideoFrames.Count - 1 do
        if v_io.VideoFrames[j] <> m64 then
            DisposeObject(v_io.VideoFrames[j]);
      v_io.VideoFrames.Clear;
    end);

  if m64 = nil then
      exit;

  m64.Position := 0;
  mr := NewRasterFromStream(m64);
  DisposeObject(m64);

  // face detector work on GPU
  TThread.Synchronize(ThSender, procedure
    begin
      mmod_desc := Face_Metric.MMOD_DNN_Process(Face_MMOD_Hnd, mr);
    end);

  // face shape extract and alignment
  ai := face_sp_para.GetAndLockAI;

  face_hnd := ai.Face_Detector(mr, mmod_desc, zAI.C_Metric_ResNet_Image_Size);
  SetLength(face_arry, ai.Face_chips_num(face_hnd));

  for i := 0 to length(face_arry) - 1 do
      face_arry[i] := ai.Face_chips(face_hnd, i);

  // metric compute
  if length(face_arry) > 0 then
    begin
      TThread.Synchronize(ThSender, procedure
        var
          face_Mat: TLMatrix;
        begin
          face_Mat := Face_Metric.Metric_ResNet_Process(Face_MDNN_Hnd, face_arry);
          SetLength(face_query_arry, length(face_arry));
          TParallel.for(Low(face_Mat), high(face_Mat), procedure(pass: Integer)
            var
              LIndex: TLInt;
              p: TLearn.PLearnMemory;
            begin
              LIndex := Face_LearnEng.ProcessMaxIndex(face_Mat[pass]);
              p := Face_LearnEng.MemorySource[LIndex];
              face_query_arry[pass].r := mmod_desc[pass].r;
              face_query_arry[pass].token := Face_LearnEng.ProcessMaxIndexToken(face_Mat[pass]);
              face_query_arry[pass].Accuracy := LDistance(face_Mat[pass], p^.m_in);
            end);
          SetLength(face_Mat, 0, 0);
        end);

      // draw od box
      ai.DrawMMOD(mmod_desc, mr, DEColor(0, 1, 0, 1));

      // draw face information
      d := TDrawEngine.Create;
      d.Rasterization.SetWorkMemory(mr);
      for i := 0 to length(face_query_arry) - 1 do
        begin
          if face_query_arry[i].Accuracy < 0.2 then
            begin
              d.DrawText(PFormat('%s' + #13#10 + '%f', [face_query_arry[i].token, face_query_arry[i].Accuracy]),
                20, face_query_arry[i].r, DEColor(1, 1, 1, 1), True);
            end;
        end;
      d.Flush;
      DisposeObject(d);
    end
  else
      SetLength(face_query_arry, 0);

  ai.Face_Close(face_hnd);
  face_sp_para.UnLockAI(ai);
  DisposeRasterArray(face_arry);

  // encode jpeg
  m64 := TMemoryStream64.Create;
  mr.SaveToJpegRGBStream(m64, 50);
  // send now
  TThread.Synchronize(ThSender, procedure
    var
      i: Integer;
      de: TDataFrameEngine;
    begin
      SendVideo(send_id, m64);
      // send video info
      de := TDataFrameEngine.Create;
      for i := 0 to length(face_query_arry) - 1 do
        begin
          de.WriteRectV2(face_query_arry[i].r);
          de.WriteString(face_query_arry[i].token);
          de.WriteDouble(face_query_arry[i].Accuracy);
        end;

      SendTunnel.SendDirectStreamCmd(send_id, 'VideoInfo', de);
      DisposeObject(de);
    end);
  DisposeObject(m64);
  DisposeObject(mr);
  Dispose(p_recv_id);
end;

procedure TRealTime_FACE_VideoServer.cmd_OD(Sender: TPeerIO; InData: TDataFrameEngine);
var
  p: PCardinal;
begin
  if not GetUserDefineRecvTunnel(Sender).LinkOk then
      exit;

  new(p);
  p^ := Sender.ID;

  TComputeThread.RunM(p, nil, Process_OD_Video);
end;

constructor TRealTime_FACE_VideoServer.Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
var
  fn: U_String;
  m64: TMemoryStream64;
begin
  inherited Create(ARecvTunnel, ASendTunnel);
  RecvTunnel.UserSpecialClass := TFACE_VideoIO_;
  RecvTunnel.MaxCompleteBufferSize := 8 * 1024 * 1024; // 8M complete buffer
  SwitchAsMaxPerformance;

  // max network performance
  SendTunnel.SendDataCompressed := True;
  RecvTunnel.SendDataCompressed := True;
  RecvTunnel.CompleteBufferCompressed := False;
  SendTunnel.CompleteBufferCompressed := False;
  SendTunnel.SyncOnCompleteBuffer := True;
  SendTunnel.SyncOnResult := True;
  RecvTunnel.SyncOnCompleteBuffer := True;
  RecvTunnel.SyncOnResult := True;
  RecvTunnel.SequencePacketActivted := False;
  SendTunnel.SequencePacketActivted := False;

  // disable print state
  RecvTunnel.PrintParams['VideoBuffer'] := False;
  RecvTunnel.PrintParams['OD'] := False;
  SendTunnel.PrintParams['VideoBuffer'] := False;
  SendTunnel.PrintParams['VideoInfo'] := False;

  RegisterCommand;

  // cpu worker
  face_sp_para := TAI_Parallel.Create;
  face_sp_para.Prepare_Parallel;
  face_sp_para.Prepare_Face;

  // gpu worker
  Face_Metric := TAI.OpenEngine();

  // metric
  fn := umlCombineFileName(TPath.GetLibraryPath, 'RealTime_Face' + C_Metric_ResNet_Ext);
  Face_MDNN_Hnd := Face_Metric.Metric_ResNet_Open_Stream(fn);

  // mmod
  fn := umlCombineFileName(TPath.GetLibraryPath, 'human_face_detector' + C_MMOD_Ext);
  Face_MMOD_Hnd := Face_Metric.MMOD_DNN_Open_Stream(fn);

  // learn engine
  Face_LearnEng := TLearn.CreateClassifier(ltKDT, zAI.C_Metric_ResNet_Dim);
  fn := umlCombineFileName(TPath.GetLibraryPath, 'RealTime_Face' + C_Learn_Ext);
  Face_LearnEng.LoadFromFile(fn);
  Face_LearnEng.Train();
end;

destructor TRealTime_FACE_VideoServer.Destroy;
begin
  inherited Destroy;
end;

procedure TRealTime_FACE_VideoServer.Progress;
begin
  inherited Progress;
end;

procedure TRealTime_FACE_VideoServer.RegisterCommand;
begin
  inherited RegisterCommand;
  RecvTunnel.RegisterCompleteBuffer('VideoBuffer').OnExecute := cmd_VideoBuffer;
  RecvTunnel.RegisterDirectStream('OD').OnExecute := cmd_OD;
end;

procedure TRealTime_FACE_VideoServer.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  RecvTunnel.UnRegisted('VideoBuffer');
  RecvTunnel.UnRegisted('OD');
end;

procedure TRealTime_FACE_VideoServer.SendVideo(send_id: Cardinal; frame: TMemoryStream64);
begin
  SendTunnel.SendCompleteBuffer(send_id, 'VideoBuffer', frame.Memory, frame.Size, True);
  frame.DiscardMemory;
end;

end.
