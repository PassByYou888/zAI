{ ****************************************************************************** }
{ * AI face Recognition ON Video                                               * }
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
unit zAI_VideoFaceQueue;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, PascalStrings, UnicodeMixedLib, DoStatusIO, MemoryStream64, ListEngine,
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  MemoryRaster, Geometry2DUnit,
  H264, FFMPEG, FFMPEG_Reader, FFMPEG_Writer,
  zAI, zAI_Common, zAI_FFMPEG, Learn, LearnTypes;

type
  TFaceRecognitionQueue = class;
  TFaceInputQueue = class;

  PFaceDetectorData = ^TFaceDetectorData;
  PFaceMetricData = ^TFaceMetricData;

  TFaceMetricData = record
    Owner: PFaceDetectorData;
    Raster: TRaster;
    Data: TLVec;
    K: TLFloat;
    Token: U_String;
    Done: Boolean;
  end;

  TFaceDetectorData = record
    ID: Integer;
    Raster: TRaster;
    MMOD: TMMOD_Desc;
    Metric: array of TFaceMetricData;
    Done: Boolean;
    function IsBusy(): Boolean;
    function FoundToken(Token: PPascalString; IgnoreMaxK: TLFloat): Boolean;
  end;

  TFaceDetectorDataList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PFaceDetectorData>;
  TFaceMetricDataList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PFaceMetricData>;

  IOnFaceInputQueue = interface
    procedure DoInput(Sender: TFaceInputQueue; Raster: TRaster);
    procedure DoRunDetect(Sender: TFaceInputQueue; pD: PFaceDetectorData);
    procedure DoDetectDone(Sender: TFaceInputQueue; pD: PFaceDetectorData);
    procedure DoRunMetric(Sender: TFaceInputQueue; pM: PFaceMetricData);
    procedure DoMetricDone(Sender: TFaceInputQueue; pM: PFaceMetricData);
    procedure DoDetectAndMetricDone(Sender: TFaceInputQueue; pD: PFaceDetectorData);
    procedure DoQueueDone(Sender: TFaceInputQueue);
    procedure DoCutQueueOnDetection(Sender: TFaceInputQueue; bIndex, eIndex: Integer);
    procedure DoCutQueueOnMaxFrame(Sender: TFaceInputQueue; bIndex, eIndex: Integer);
  end;

  TFaceInputQueue = class
  private
    FRunning: Integer;
    FOwner: TFaceRecognitionQueue;
    FQueue: TFaceDetectorDataList;
    FCritical: TCritical;
    FCutNullQueue: Boolean;
    FMaxQueue: Integer;
    FIDSeed: Integer;
    procedure FaceDetector_OnResult(ThSender: TAI_DNN_Thread_MMOD6L; UserData: Pointer; Input: TMemoryRaster; output: TMMOD_Desc);
    procedure FaceMetric_OnResult(ThSender: TAI_DNN_Thread_Metric; UserData: Pointer; Input: TMemoryRaster; output: TLVec);
    procedure DoDelayCheckBusyAndFree;
  public
    OnInterface: IOnFaceInputQueue;
    constructor Create(Owner_: TFaceRecognitionQueue);
    destructor Destroy; override;
    function LockQueue: TFaceDetectorDataList;
    procedure UnLockQueue;
    procedure Clean;
    procedure DelayCheckBusyAndFree;
    procedure Input(raster_: TRaster; instance_: Boolean);
    procedure InputStream(reader: TFFMPEG_VideoStreamReader; stream: TCoreClassStream; MaxFrame: Integer);
    procedure InputH264Stream(stream: TCoreClassStream; MaxFrame: Integer);
    procedure InputMJPEGStream(stream: TCoreClassStream; MaxFrame: Integer);
    procedure InputFFMPEGSource(source: TPascalString; MaxFrame: Integer);
    function Busy: Boolean; overload;
    function Busy(bIndex, eIndex: Integer): Boolean; overload;
    function Delete(bIndex, eIndex: Integer): Boolean;
    procedure RemoveNullOutput;
    procedure GetQueueState(IgnoreMaxK: TLFloat; var Detector_Done_Num, Detector_Busy_Num, Metric_Done_Num, Metric_Busy_Num, NullQueue_Num, Invalid_box_Num: Integer; TokenInfo: THashVariantList);
    function AnalysisK(MaxFrame: Integer; IgnoreMaxK: TLFloat; var MaxK, avgK, MinK: TLFloat; var total_num, matched_num: Integer): TPascalString;
    procedure BuildQueueToImageList(Token: TPascalString; IgnoreMaxK: TLFloat; FitWidth_, FitHeight_: Integer; ImgL: TAI_ImageList);
    function Count: Integer;
    procedure SaveQueueAsPasH264Stream(stream: TCoreClassStream);
    procedure SaveQueueAsH264Stream(stream: TCoreClassStream);
    procedure SaveQueueAsMJPEGStream(stream: TCoreClassStream);

    property CutNullQueue: Boolean read FCutNullQueue write FCutNullQueue;
    property MaxQueue: Integer read FMaxQueue write FMaxQueue;
  end;

  TFaceRecognitionQueue = class
  private
    FDNN_Face_Detector_Pool: TAI_DNN_ThreadPool;
    FDNN_Face_Metric_Pool: TAI_DNN_ThreadPool;
    FParallel: TAI_Parallel;
    FLearn: TLearn;
  public
    constructor Create;
    destructor Destroy; override;

    procedure OpenMetricFile(MetricModel: U_String);
    procedure OpenMetricStream(MetricModel: TMemoryStream64);
    procedure OpenLearnFile(LearnModel: U_String);
    procedure OpenLearnStream(LearnModel: TMemoryStream64);

    procedure Wait;

    property DNN_Face_Detector_Pool: TAI_DNN_ThreadPool read FDNN_Face_Detector_Pool;
    property Parallel: TAI_Parallel read FParallel;
    property DNN_Face_Metric_Pool: TAI_DNN_ThreadPool read FDNN_Face_Metric_Pool;
    property Learn: TLearn read FLearn;
  end;

implementation

function TFaceDetectorData.IsBusy(): Boolean;
var
  i: Integer;
begin
  Result := True;
  if not Done then
      exit;
  for i := 0 to length(Metric) - 1 do
    if not Metric[i].Done then
        exit;
  Result := False;
end;

function TFaceDetectorData.FoundToken(Token: PPascalString; IgnoreMaxK: TLFloat): Boolean;
var
  i: Integer;
begin
  Result := False;
  if IsBusy() then
      exit;
  for i := 0 to length(Metric) - 1 do
    if Metric[i].Done then
      if Token^.Same(@Metric[i].Token) and (Metric[i].K < IgnoreMaxK) and RectInRect(MMOD[i].r, Raster.BoundsRectV2) then
          exit(True);
end;

procedure TFaceInputQueue.FaceDetector_OnResult(ThSender: TAI_DNN_Thread_MMOD6L; UserData: Pointer; Input: TMemoryRaster; output: TMMOD_Desc);
var
  p: PFaceDetectorData;
  AI: TAI;
  fHnd: TFace_Handle;
  i: Integer;
begin
  p := UserData;
  p^.MMOD := output;
  SetLength(p^.Metric, length(p^.MMOD));
  for i := 0 to length(p^.Metric) - 1 do
    begin
      p^.Metric[i].Owner := p;
      p^.Metric[i].Raster := nil;
      SetLength(p^.Metric[i].Data, 0);
      p^.Metric[i].K := 0;
      p^.Metric[i].Token := '';
      p^.Metric[i].Done := False;
    end;

  p^.Done := True;
  AtomDec(FRunning);

  if Assigned(OnInterface) then
    begin
      try
          OnInterface.DoDetectDone(Self, p);
      except
      end;
    end;

  if length(p^.MMOD) > 0 then
    begin
      AI := FOwner.FParallel.GetAndLockAI;
      fHnd := AI.Face_Detector(Input, p^.MMOD, C_Metric_Input_Size);
      for i := 0 to AI.Face_chips_num(fHnd) - 1 do
        begin
          p^.Metric[i].Raster := AI.Face_chips(fHnd, i);
          if Assigned(OnInterface) then
            begin
              try
                  OnInterface.DoRunMetric(Self, @p^.Metric[i]);
              except
              end;
            end;

          AtomInc(FRunning);
          with TAI_DNN_Thread_Metric(FOwner.DNN_Face_Metric_Pool.MinLoad_DNN_Thread) do
              ProcessM(@p^.Metric[i], p^.Metric[i].Raster, False, {$IFDEF FPC}@{$ENDIF FPC}FaceMetric_OnResult);
        end;
      AI.Face_Close(fHnd);
      FOwner.FParallel.UnLockAI(AI);
    end
  else if FCutNullQueue then
    begin
      FCritical.Lock;
      i := FQueue.IndexOf(p);
      FCritical.UnLock;
      if i >= 0 then
        if not Busy(0, i) then
          begin
            if Assigned(OnInterface) then
              begin
                try
                    OnInterface.DoCutQueueOnDetection(Self, 0, i);
                except
                end;
              end;
            Delete(0, i);
          end;
      if Assigned(OnInterface) then
        begin
          try
              OnInterface.DoDetectAndMetricDone(Self, p);
          except
          end;
        end;
    end;
  if not Busy then
    if Assigned(OnInterface) then
      begin
        try
            OnInterface.DoQueueDone(Self);
        except
        end;
      end;
end;

procedure TFaceInputQueue.FaceMetric_OnResult(ThSender: TAI_DNN_Thread_Metric; UserData: Pointer; Input: TMemoryRaster; output: TLVec);
var
  p: PFaceMetricData;
  i: Integer;
begin
  AtomDec(FRunning);
  p := UserData;
  p^.Data := LVecCopy(output);
  i := FOwner.FLearn.ProcessMaxIndex(p^.Data);
  p^.K := LDistance(p^.Data, FOwner.FLearn[i]^.m_in);
  p^.Token := FOwner.FLearn[i]^.Token;
  p^.Done := True;
  if Assigned(OnInterface) then
      OnInterface.DoMetricDone(Self, p);
  if not p^.Owner^.IsBusy then
    begin
      if Assigned(OnInterface) then
        begin
          try
              OnInterface.DoDetectAndMetricDone(Self, p^.Owner);
          except
          end;
        end;
      if Count > FMaxQueue then
        if not Busy(0, Count - FMaxQueue) then
          begin
            if Assigned(OnInterface) then
              begin
                try
                    OnInterface.DoCutQueueOnMaxFrame(Self, 0, Count - FMaxQueue);
                except
                end;
              end;
            Delete(0, Count - FMaxQueue);
          end;
    end;
  if not Busy then
    if Assigned(OnInterface) then
      begin
        try
            OnInterface.DoQueueDone(Self);
        except
        end;
      end;
end;

procedure TFaceInputQueue.DoDelayCheckBusyAndFree;
begin
  while (FRunning > 0) or (Busy) do
      TCompute.Sleep(10);
  DisposeObject(Self);
end;

constructor TFaceInputQueue.Create(Owner_: TFaceRecognitionQueue);
begin
  inherited Create;
  FRunning := 0;
  FOwner := Owner_;
  FQueue := TFaceDetectorDataList.Create;
  FCritical := TCritical.Create;
  FCutNullQueue := True;
  FMaxQueue := 300;
  FIDSeed := 0;
  OnInterface := nil;
end;

destructor TFaceInputQueue.Destroy;
begin
  Clean;
  DisposeObject(FQueue);
  FCritical.Free;
  inherited Destroy;
end;

function TFaceInputQueue.LockQueue: TFaceDetectorDataList;
begin
  FCritical.Lock;
  Result := FQueue;
end;

procedure TFaceInputQueue.UnLockQueue;
begin
  FCritical.UnLock;
end;

procedure TFaceInputQueue.Clean;
var
  i, j: Integer;
  pD: PFaceDetectorData;
  pM: PFaceMetricData;
begin
  while Busy do
      TCompute.Sleep(1);

  FCritical.Lock;
  for i := 0 to FQueue.Count - 1 do
    begin
      pD := FQueue[i];
      for j := 0 to length(pD^.Metric) - 1 do
        begin
          pM := @pD^.Metric[j];
          DisposeObject(pM^.Raster);
          SetLength(pM^.Data, 0);
          pM^.Token := '';
        end;
      DisposeObject(pD^.Raster);
      SetLength(pD^.MMOD, 0);
      SetLength(pD^.Metric, 0);
      dispose(pD);
    end;
  FQueue.Clear;
  FCritical.UnLock;
  FIDSeed := 0;
end;

procedure TFaceInputQueue.DelayCheckBusyAndFree;
begin
  TCompute.RunM_NP({$IFDEF FPC}@{$ENDIF FPC}DoDelayCheckBusyAndFree);
end;

procedure TFaceInputQueue.Input(raster_: TRaster; instance_: Boolean);
var
  r: TRaster;
  pD: PFaceDetectorData;
begin
  if instance_ then
      r := raster_
  else
      r := raster_.Clone;

  new(pD);
  pD^.ID := FIDSeed;
  AtomInc(FIDSeed);
  pD^.Raster := r;
  SetLength(pD^.MMOD, 0);
  SetLength(pD^.Metric, 0);
  pD^.Done := False;

  FCritical.Lock;
  FQueue.Add(pD);
  FCritical.UnLock;

  if Assigned(OnInterface) then
      OnInterface.DoInput(Self, r);
  if Assigned(OnInterface) then
      OnInterface.DoRunDetect(Self, pD);

  AtomInc(FRunning);
  with TAI_DNN_Thread_MMOD6L(FOwner.DNN_Face_Detector_Pool.MinLoad_DNN_Thread) do
      ProcessM(pD, r, False, {$IFDEF FPC}@{$ENDIF FPC}FaceDetector_OnResult);
end;

procedure TFaceInputQueue.InputStream(reader: TFFMPEG_VideoStreamReader; stream: TCoreClassStream; MaxFrame: Integer);
const
  C_Chunk_Buff_Size = 1 * 1024 * 1024;
var
  tempBuff: Pointer;
  chunk: NativeInt;
  L: TMemoryRasterList;
  i: Integer;
  curNum: Integer;
begin
  curNum := 0;
  tempBuff := GetMemory(C_Chunk_Buff_Size);
  stream.Position := 0;
  while (stream.Position < stream.Size) do
    begin
      chunk := umlMin(stream.Size - stream.Position, C_Chunk_Buff_Size);
      if chunk <= 0 then
          break;
      stream.Read(tempBuff^, chunk);
      reader.WriteBuffer(tempBuff, chunk);

      L := reader.LockVideoPool;
      while L.Count > 0 do
        begin
          Input(L[0], True);
          L.Delete(0);
          inc(curNum);
        end;
      reader.UnLockVideoPool(True);
      if (MaxFrame > 0) and (curNum > MaxFrame) then
          break;
    end;
  FreeMemory(tempBuff);
end;

procedure TFaceInputQueue.InputH264Stream(stream: TCoreClassStream; MaxFrame: Integer);
var
  reader: TFFMPEG_VideoStreamReader;
begin
  reader := TFFMPEG_VideoStreamReader.Create;
  reader.OpenH264Decodec;
  InputStream(reader, stream, MaxFrame);
  DisposeObject(reader);
end;

procedure TFaceInputQueue.InputMJPEGStream(stream: TCoreClassStream; MaxFrame: Integer);
var
  reader: TFFMPEG_VideoStreamReader;
begin
  reader := TFFMPEG_VideoStreamReader.Create;
  reader.OpenMJPEGDecodec;
  InputStream(reader, stream, MaxFrame);
  DisposeObject(reader);
end;

procedure TFaceInputQueue.InputFFMPEGSource(source: TPascalString; MaxFrame: Integer);
var
  reader: TFFMPEG_Reader;
  Raster: TRaster;
  curNum: Integer;
begin
  reader := TFFMPEG_Reader.Create(source);
  Raster := NewRaster();
  curNum := 0;
  while reader.ReadFrame(Raster, False) do
    begin
      Input(Raster, False);
      inc(curNum);
      if (MaxFrame > 0) and (curNum > MaxFrame) then
          break;
    end;
  DisposeObject(reader);
  DisposeObject(Raster);
end;

function TFaceInputQueue.Busy: Boolean;
begin
  Result := Busy(0, FQueue.Count - 1);
end;

function TFaceInputQueue.Busy(bIndex, eIndex: Integer): Boolean;
var
  i: Integer;
begin
  FCritical.Lock;
  Result := False;
  for i := umlMax(0, bIndex) to umlMin(FQueue.Count - 1, eIndex) do
      Result := Result or FQueue[i]^.IsBusy();
  FCritical.UnLock;
end;

function TFaceInputQueue.Delete(bIndex, eIndex: Integer): Boolean;
var
  i, j: Integer;
  pD: PFaceDetectorData;
  pM: PFaceMetricData;
begin
  Result := False;
  if not Busy(bIndex, eIndex) then
    begin
      FCritical.Lock;
      for i := umlMax(0, bIndex) to umlMin(FQueue.Count - 1, eIndex) do
        begin
          pD := FQueue[bIndex];
          for j := 0 to length(pD^.Metric) - 1 do
            begin
              pM := @pD^.Metric[j];
              DisposeObject(pM^.Raster);
              SetLength(pM^.Data, 0);
              pM^.Token := '';
            end;
          DisposeObject(pD^.Raster);
          SetLength(pD^.MMOD, 0);
          SetLength(pD^.Metric, 0);
          dispose(pD);
          FQueue.Delete(bIndex);
        end;
      FCritical.UnLock;
      Result := True;
    end;
end;

procedure TFaceInputQueue.RemoveNullOutput;
var
  i, j: Integer;
  pD: PFaceDetectorData;
  pM: PFaceMetricData;
begin
  FCritical.Lock;
  i := 0;
  while i < FQueue.Count do
    begin
      pD := FQueue[i];
      if (not pD^.IsBusy) and (length(pD^.MMOD) = 0) then
        begin
          for j := 0 to length(pD^.Metric) - 1 do
            begin
              pM := @pD^.Metric[j];
              DisposeObject(pM^.Raster);
              SetLength(pM^.Data, 0);
              pM^.Token := '';
            end;
          DisposeObject(pD^.Raster);
          SetLength(pD^.MMOD, 0);
          SetLength(pD^.Metric, 0);
          dispose(pD);
          FQueue.Delete(i);
        end
      else
          inc(i);
    end;
  FCritical.UnLock;
end;

procedure TFaceInputQueue.GetQueueState(IgnoreMaxK: TLFloat; var Detector_Done_Num, Detector_Busy_Num, Metric_Done_Num, Metric_Busy_Num, NullQueue_Num, Invalid_box_Num: Integer; TokenInfo: THashVariantList);
var
  i, j: Integer;
  pD: PFaceDetectorData;
  pM: PFaceMetricData;
begin
  Detector_Done_Num := 0;
  Detector_Busy_Num := 0;
  Metric_Done_Num := 0;
  Metric_Busy_Num := 0;
  NullQueue_Num := 0;
  Invalid_box_Num := 0;

  if TokenInfo <> nil then
      TokenInfo.Clear;

  FCritical.Lock;
  for i := 0 to FQueue.Count - 1 do
    begin
      pD := FQueue[i];
      if (pD^.Done) then
        begin
          for j := 0 to length(pD^.MMOD) - 1 do
            if RectInRect(pD^.MMOD[j].r, pD^.Raster.BoundsRectV2) then
                inc(Detector_Done_Num)
            else
                inc(Invalid_box_Num);

          if pD^.IsBusy then
              inc(Detector_Busy_Num)
          else if length(pD^.MMOD) = 0 then
              inc(NullQueue_Num);

          for j := 0 to length(pD^.Metric) - 1 do
            begin
              pM := @pD^.Metric[j];
              if pM^.Done then
                begin
                  inc(Metric_Done_Num);
                  if (TokenInfo <> nil) and (pM^.K < IgnoreMaxK) then
                      TokenInfo.IncValue(pM^.Token, 1);
                end
              else
                  inc(Metric_Busy_Num);
            end;
        end;
    end;
  FCritical.UnLock;
end;

function TFaceInputQueue.AnalysisK(MaxFrame: Integer; IgnoreMaxK: TLFloat; var MaxK, avgK, MinK: TLFloat; var total_num, matched_num: Integer): TPascalString;
var
  HL: THashObjectList;
  L: TFaceMetricDataList;
  LSUM: TFaceMetricDataList;
  num_: Integer;
  sum_: TLFloat;

  i, j: Integer;
  pD: PFaceDetectorData;
  pM: PFaceMetricData;
begin
  MaxK := 0;
  avgK := 0;
  MinK := 0;
  matched_num := 0;
  total_num := 0;
  Result := '';

  FCritical.Lock;

  HL := THashObjectList.CustomCreate(True, 1024);
  LSUM := nil;
  num_ := 0;
  for i := FQueue.Count - 1 downto 0 do
    begin
      pD := FQueue[i];
      if pD^.Done then
        for j := 0 to length(pD^.Metric) - 1 do
          begin
            pM := @pD^.Metric[j];
            if pM^.Done and (pM^.K < IgnoreMaxK) and RectInRect(pD^.MMOD[j].r, pD^.Raster.BoundsRectV2) then
              begin
                if not HL.Exists(pM^.Token) then
                    HL.Add(pM^.Token, TFaceMetricDataList.Create);
                L := TFaceMetricDataList(HL[pM^.Token]);
                L.Add(pM);

                if (LSUM = nil) or ((LSUM <> L) and (LSUM.Count < L.Count)) then
                    LSUM := L;
                inc(total_num);
              end;
            inc(num_);
          end;
      if (MaxFrame > 0) and (num_ > MaxFrame) then
          break;
    end;

  if (LSUM <> nil) and (LSUM.Count > 0) then
    begin
      sum_ := LSUM.First^.K;
      MaxK := sum_;
      MinK := sum_;
      Result := LSUM.First^.Token;
      for i := 1 to LSUM.Count - 1 do
        begin
          pM := LSUM[i];
          if pM^.K > MaxK then
              MaxK := pM^.K;
          if pM^.K < MinK then
              MinK := pM^.K;
          sum_ := sum_ + pM^.K;
        end;
      avgK := LSafeDivF(sum_, LSUM.Count);
      matched_num := LSUM.Count;
    end;

  DisposeObject(HL);

  FCritical.UnLock;
end;

procedure TFaceInputQueue.BuildQueueToImageList(Token: TPascalString; IgnoreMaxK: TLFloat; FitWidth_, FitHeight_: Integer; ImgL: TAI_ImageList);
var
  i, j: Integer;
  pD: PFaceDetectorData;
  pM: PFaceMetricData;
  img: TAI_Image;
  det: TAI_DetectorDefine;
begin
  FCritical.Lock;
  for i := 0 to FQueue.Count - 1 do
    begin
      pD := FQueue[i];
      if pD^.FoundToken(@Token, IgnoreMaxK) then
        begin
          img := TAI_Image.Create(ImgL);
          img.FileInfo := Token;
          ImgL.Add(img);
          img.ResetRaster(pD^.Raster.FitScaleAsNew(FitWidth_, FitHeight_));
          for j := 0 to length(pD^.Metric) - 1 do
            begin
              pM := @pD^.Metric[j];
              if Token.Same(pM^.Token) and (pM^.K < IgnoreMaxK) and RectInRect(pD^.MMOD[j].r, pD^.Raster.BoundsRectV2) then
                begin
                  det := img.DetectorDefineList.AddDetector(RoundRect(RectProjection(pD^.Raster.BoundsRectV2, img.Raster.BoundsRectV2, pD^.MMOD[j].r)), pM^.Token);
                  det.ResetPrepareRaster(pM^.Raster.Clone);
                end;
            end;
        end;
    end;
  FCritical.UnLock;
end;

function TFaceInputQueue.Count: Integer;
begin
  Result := FQueue.Count;
end;

procedure TFaceInputQueue.SaveQueueAsPasH264Stream(stream: TCoreClassStream);
var
  writer: TH264Writer;
  i: Integer;
begin
  if FQueue.Count = 0 then
      exit;
  writer := TH264Writer.Create(FQueue.First^.Raster.Width, FQueue.First^.Raster.Height, FQueue.Count, 30, stream);
  for i := 0 to FQueue.Count - 1 do
      writer.WriteFrame(FQueue[i]^.Raster);
  writer.Flush;
  DisposeObject(writer);
end;

procedure TFaceInputQueue.SaveQueueAsH264Stream(stream: TCoreClassStream);
var
  writer: TFFMPEG_Writer;
  i: Integer;
begin
  if FQueue.Count = 0 then
      exit;
  writer := TFFMPEG_Writer.Create(stream);
  writer.AutoFreeOutput := False;
  writer.OpenH264Codec(FQueue.First^.Raster.Width, FQueue.First^.Raster.Height, 30, 1024 * 2048);
  for i := 0 to FQueue.Count - 1 do
      writer.EncodeRaster(FQueue[i]^.Raster);
  writer.Flush;
  DisposeObject(writer);
end;

procedure TFaceInputQueue.SaveQueueAsMJPEGStream(stream: TCoreClassStream);
var
  writer: TFFMPEG_Writer;
  i: Integer;
begin
  if FQueue.Count = 0 then
      exit;
  writer := TFFMPEG_Writer.Create(stream);
  writer.AutoFreeOutput := False;
  writer.OpenJPEGCodec(FQueue.First^.Raster.Width, FQueue.First^.Raster.Height, 2, 31);
  for i := 0 to FQueue.Count - 1 do
      writer.EncodeRaster(FQueue[i]^.Raster);
  writer.Flush;
  DisposeObject(writer);
end;

constructor TFaceRecognitionQueue.Create;
var
  i: Integer;
begin
  inherited Create;
  CheckAndReadAIConfig();
  zAI.Prepare_AI_Engine();

  FDNN_Face_Detector_Pool := TAI_DNN_ThreadPool.Create;
  FDNN_Face_Detector_Pool.BuildPerDeviceThread(TAI_DNN_Thread_MMOD6L);
  for i := 0 to FDNN_Face_Detector_Pool.Count - 1 do
      TAI_DNN_Thread_MMOD6L(FDNN_Face_Detector_Pool[i]).Open_Face;

  FDNN_Face_Metric_Pool := TAI_DNN_ThreadPool.Create;
  FDNN_Face_Metric_Pool.BuildPerDeviceThread(TAI_DNN_Thread_Metric);

  FParallel := TAI_Parallel.Create;
  FParallel.Prepare_Parallel(FDNN_Face_Detector_Pool.Count);
  FParallel.Prepare_FaceSP;

  FLearn := TAI.Build_Metric_ResNet_Learn;
end;

destructor TFaceRecognitionQueue.Destroy;
begin
  DisposeObject(FDNN_Face_Detector_Pool);
  DisposeObject(FDNN_Face_Metric_Pool);
  DisposeObject(FParallel);
  DisposeObject(FLearn);
  inherited Destroy;
end;

procedure TFaceRecognitionQueue.OpenMetricFile(MetricModel: U_String);
var
  i: Integer;
begin
  for i := 0 to FDNN_Face_Metric_Pool.Count - 1 do
      TAI_DNN_Thread_Metric(FDNN_Face_Metric_Pool[i]).Open(MetricModel);
end;

procedure TFaceRecognitionQueue.OpenMetricStream(MetricModel: TMemoryStream64);
var
  i: Integer;
begin
  for i := 0 to FDNN_Face_Metric_Pool.Count - 1 do
      TAI_DNN_Thread_Metric(FDNN_Face_Metric_Pool[i]).Open_Stream(MetricModel);
end;

procedure TFaceRecognitionQueue.OpenLearnFile(LearnModel: U_String);
begin
  FLearn.LoadFromFile(LearnModel);
end;

procedure TFaceRecognitionQueue.OpenLearnStream(LearnModel: TMemoryStream64);
begin
  FLearn.LoadFromStream(LearnModel);
end;

procedure TFaceRecognitionQueue.Wait;
begin
  FDNN_Face_Detector_Pool.Wait;
  FDNN_Face_Metric_Pool.Wait;
end;

end.
