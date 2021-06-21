{ ****************************************************************************** }
{ * AI RealTime Video                                                          * }
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
unit zAI_RealTimeVideo;

{$INCLUDE zDefine.inc}

interface

uses
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  CoreClasses, PascalStrings, UnicodeMixedLib, ListEngine, Geometry2DUnit,
  DataFrameEngine, TextDataEngine,
  DoStatusIO,
  MemoryStream64,
  MemoryRaster,
  FFMPEG, FFMPEG_Reader, FFMPEG_Writer,
  zDrawEngine, PictureViewerInterface,
  zAI, zAI_Common, Learn, LearnTypes,
  zAI_RealTimeVideoInfo;

type
  // preset class
  TConfigData = class;
  TConfigData_Classifier_Metric = class;
  TConfigData_Classifier_ZMetric = class;
  TConfigData_OD = class;
  TConfigData_OD3L = class;
  TConfigData_OD6L = class;
  TConfigData_VideoSource = class;
  // define class of now
  TConfigData_Classifier_Metric_Class = class of TConfigData_Classifier_Metric;
  TConfigData_Classifier_ZMetric_Class = class of TConfigData_Classifier_ZMetric;
  TConfigData_OD3L_Class = class of TConfigData_OD3L;
  TConfigData_OD6L_Class = class of TConfigData_OD6L;
  TConfigData_VideoSource_Class = class of TConfigData_VideoSource;

  TConfigDataList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TConfigData>;

  TConfigDataList = class(TConfigDataList_Decl)
  public
    Metric_Class: TConfigData_Classifier_Metric_Class;
    ZMetric_Class: TConfigData_Classifier_ZMetric_Class;
    OD3L_Class: TConfigData_OD3L_Class;
    OD6L_Class: TConfigData_OD6L_Class;
    VideoSource_Class: TConfigData_VideoSource_Class;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Remove(obj: TConfigData);
    procedure Delete(Index: Integer);
    procedure Clear;
    procedure LoadConfigure(te: TTextDataEngine);
    procedure LoadConfigureFile(fileName_: U_String);
    function FindConfigData(Name: U_String): TConfigData;
    procedure DNNEnabledLastProcessRaster(value_: Boolean);

    procedure BeginUpdateDNNLastProcessRaster(viewer: TPictureViewerInterface);
    procedure EndUpdateDNNLastProcessRaster();
  end;

  TConfigData = class
  public
    Name: U_String;
    Type_: U_String;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Read(vl: THashVariantList); virtual; abstract;
    procedure ReadDone; virtual; abstract;
  end;

  TConfigData_Classifier = class(TConfigData)
  public
    ClassifierThread: TAI_DNN_ThreadPool;
    GPUThread: Integer;
    GPU: U_String;
    Model: U_String;
    CPUThreadCritical: Integer;
    GPUPerformanceCritical: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Read(vl: THashVariantList); override;
    procedure Process(Rect_: TRectV2; p: PVideoData_MetricInfo; Raster: TRaster); virtual; abstract;
  end;

  TConfigData_Classifier_Metric = class(TConfigData_Classifier)
  private
    procedure MetricAsyncResult(ThSender: TAI_DNN_Thread_Metric; UserData: Pointer; Input: TMemoryRaster; output: TLVec);
  public
    L: TLearn;
    LearnFile: U_String;
    MinK: TLFloat;
    LossInfo: U_String;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Read(vl: THashVariantList); override;
    procedure ReadDone; override;
    procedure Process(Rect_: TRectV2; p: PVideoData_MetricInfo; Raster: TRaster); override;
  end;

  TConfigData_Classifier_ZMetric = class(TConfigData_Classifier)
  private
    procedure ZMetricAsyncResult(ThSender: TAI_DNN_Thread_ZMetric; UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; output: TLVec);
  public
    SS_Width, SS_Height: Integer;
    L: TLearn;
    LearnFile: U_String;
    MinK: TLFloat;
    LossInfo: U_String;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Read(vl: THashVariantList); override;
    procedure ReadDone; override;
    procedure Process(Rect_: TRectV2; p: PVideoData_MetricInfo; Raster: TRaster); override;
  end;

  TConfigData_OD = class(TConfigData)
  public
    UsedFit: Boolean;
    FitX, FitY: Integer;
    ODThread: TAI_DNN_ThreadPool;
    GPUThread: Integer;
    GPU: U_String;
    Model: U_String;
    CPUThreadCritical: Integer;
    GPUPerformanceCritical: Integer;
    ClassifierProcessor: U_StringArray;
    ClassifierProcessorInstance: array of TConfigData_Classifier;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Read(vl: THashVariantList); override;
    procedure ReadDone; override;
    procedure Process(Async_: Boolean; ODInfo: PVideoData_ODInfo; Raster: TRaster); virtual; abstract;
    procedure ProcessClassifier(ODInfo: PVideoData_ODInfo; Raster: TMemoryRaster; MMOD_Desc: TMMOD_Desc);
  end;

  TConfigData_OD6L = class(TConfigData_OD)
  private
    procedure ProcessTh(ThSender: TCompute);
    procedure ODAsyncResult(ThSender: TAI_DNN_Thread_MMOD6L; UserData: Pointer; Raster: TMemoryRaster; MMOD_Desc: TMMOD_Desc);
  public
    procedure Read(vl: THashVariantList); override;
    procedure Process(Async_: Boolean; ODInfo: PVideoData_ODInfo; Raster: TRaster); override;
  end;

  TConfigData_OD3L = class(TConfigData_OD)
  private
    procedure ProcessTh(ThSender: TCompute);
    procedure ODAsyncResult(ThSender: TAI_DNN_Thread_MMOD3L; UserData: Pointer; Raster: TMemoryRaster; MMOD_Desc: TMMOD_Desc);
  public
    procedure Read(vl: THashVariantList); override;
    procedure Process(Async_: Boolean; ODInfo: PVideoData_ODInfo; Raster: TRaster); override;
  end;

  TTrackerData = class
  private
    ThNum: Integer;
    AI: TAI;
    RefrenceDESCBuff: TMMOD_Desc_Array;
    TrackerHndBuff: TTracker_Handle_ArrayOfArray;
    TrackerInput: TVideoDataList;
  public
    constructor Create(AI_: TAI);
    destructor Destroy; override;
    procedure NewRefrenceDESC(VideoData_: PVideoData);
  end;

  TTrackerComputeInstance = class
  private
    AI: TAI;
    RefrenceDESCBuff: TMMOD_Desc_Array;
    TrackerHndBuff: TTracker_Handle_ArrayOfArray;
    TrackerInput: TVideoDataList;
    ThNum: PInteger;
    VideoSource: TConfigData_VideoSource;
    procedure DoRun(ThSender: TCompute);
  public
    constructor Create(Data_: TTrackerData; VideoSource_: TConfigData_VideoSource);
  end;

  TTVideoEncodeQueueOrder = {$IFDEF FPC}specialize {$ENDIF FPC} TCriticalOrderStruct<PVideoData>;

  TVideoSofEncodeInstance = class
  private
    Queue: TTVideoEncodeQueueOrder;
    Activted: TAtomBool;
    ThIsEnd: Boolean;
    procedure FreeOrderStruct(var p: PVideoData);
    procedure RenderAIRaster(p: PVideoData);
    procedure EncoderThRun(ThSender: TCompute);
  public
    Owner: TConfigData_VideoSource;
    EncodeNum: Integer;
    constructor Create(Owner_: TConfigData_VideoSource);
    destructor Destroy; override;
    procedure Encode(p: PVideoData);
  end;

  TOnBeginPush = procedure(Sender: TConfigData_VideoSource; URL: U_String) of object;
  TOnEndPush = procedure(Sender: TConfigData_VideoSource) of object;
  TOnVideoSourceEvent = procedure(Sender: TConfigData_VideoSource) of object;
  TOnVideoRaster = procedure(Sender: TConfigData_VideoSource; Raster: TRaster) of object;
  TOnDoneRaster = procedure(Sender: TConfigData_VideoSource; VideoData: PVideoData) of object;
  TOnDoneEncode = procedure(Sender: TConfigData_VideoSource; BeginTime_, EndTime_: TDateTime; Long_: TTimeTick;
    VideoBuffer, AIRendererBuffer: TFFMPEG_Writer; DataBuffer: TDFE; HashTokens: THashVariantList) of object;

  TConfigData_VideoSource = class(TConfigData)
  private
    procedure OpenVideo_SyncOD();
    procedure OpenVideo_FullGPU();
  public
    // parameter
    Reader: TFFMPEG_Reader;
    VideoSource: U_String;
    VideoInfo: U_String;
    VideoSource_X, VideoSource_Y: Integer;
    UsedFit: Boolean;
    FitX, FitY: Integer;
    Cuvid: Boolean;
    UsedCorrelationTracker: Boolean;
    MaxTrackerThread: Integer;
    MaxInputQueue: Integer;
    FullGPU: Boolean;
    AsyncODProcessor: Boolean;
    ThreadSleep: Integer;
    Encode: Boolean;
    EncodeTimeTickLong: TTimeTick;
    EncodeLaunchMinFrame: Integer;
    EncodeLaunchMaxFrame: Integer;
    EncodePSF: Integer;
    EncodeGOP: Integer;
    EncodeBFrame: Integer;
    EncodeBitrate: Integer;
    VideoPush: Boolean;
    VideoPushURL: U_String;
    ShowRealTimeWindow: Boolean;
    ShowEncoderWindow: Boolean;
    ShowAIWindow: Boolean;
    ODProcessor: U_StringArray;
    ODProcessorInstance: array of TConfigData_OD;
    // states
    Running: TAtomBool;
    Busy: TAtomBool;
    ID: Int64;
    FrameDoneDNN: Integer;
    FrameLoss: Integer;
    FrameTracker: Integer;
    FrameBusy: Integer;
    TrackerThreadNum: Integer;
    // event
    OnOpenVideo: TOnVideoSourceEvent;
    OnCloseVideo: TOnVideoSourceEvent;
    OnVideoBeginPush: TOnBeginPush;
    OnVideoEndPush: TOnEndPush;
    OnResetRaster: TOnVideoSourceEvent;
    OnPrepareRaster: TOnVideoRaster;
    OnDoneVideoRaster: TOnDoneRaster;
    OnDoneAIRaster: TOnVideoRaster;
    OnDoneEncode: TOnDoneEncode;
    // user define
    UserData: Pointer;
    UserObject: TCoreClassObject;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Read(vl: THashVariantList); override;

    // processor
    procedure ReadDone; override;
    procedure OpenVideo; virtual;
    procedure OpenVideoOnThread;
    procedure CloseVideo; virtual;

    // trigger
    procedure DoOpenVideo; virtual;
    procedure DoCloseVideo; virtual;
    procedure DoBeginVideoPush(PushURL_: U_String); virtual;
    procedure DoEndVideoPush(); virtual;
    procedure DoResetRaster(); virtual;
    procedure DoPrepareRaster(Raster: TRaster); virtual;
    procedure DoDoneVideoRaster(p: PVideoData); virtual;
    procedure DoDoneAIRaster(Raster: TRaster); virtual;
    procedure DoDoneEncode(BeginTime_, EndTime_: TDateTime; Long_: TTimeTick;
      VideoBuffer, AIRendererBuffer: TFFMPEG_Writer; DataBuffer: TDFE; HashTokens: THashVariantList); virtual;
  end;

const
  C_RealTimeVideo_Configure_Templet =
    '[Detector]'#13#10 +
    'Type=OD6L or OD3L'#13#10 +
    'UsedFit=False'#13#10 +
    'FitX=1280'#13#10 +
    'FitY=720'#13#10 +
    'GPUThread=1'#13#10 +
    'GPUDevice=0'#13#10 +
    'CPUThreadCritical=10'#13#10 +
    'GPUPerformanceCritical=10'#13#10 +
    'Model=file.svm_dnn_od'#13#10 +
    'ClassifierProcessor=ZMetric_Classifier,Metric_Classifier'#13#10 +
    #13#10 +
    '[ZMetric_Classifier]'#13#10 +
    'Type=ZMetric'#13#10 +
    'SS_Width=150'#13#10 +
    'SS_Height=150'#13#10 +
    'MinK=-1'#13#10 +
    'LossInfo=Unrecognized'#13#10 +
    'GPUThread=1'#13#10 +
    'GPUDevice=0'#13#10 +
    'CPUThreadCritical=10'#13#10 +
    'GPUPerformanceCritical=10'#13#10 +
    'Learn=file.learn'#13#10 +
    'Model=file.ZMetric'#13#10 +
    #13#10 +
    '[Metric_Classifier]'#13#10 +
    'Type=Metric'#13#10 +
    'MinK=-1'#13#10 +
    'LossInfo=Unrecognized'#13#10 +
    'GPUThread=1'#13#10 +
    'GPUDevice=0'#13#10 +
    'CPUThreadCritical=10'#13#10 +
    'GPUPerformanceCritical=10'#13#10 +
    'Learn=file.learn'#13#10 +
    'Model=file.Metric'#13#10 +
    #13#10 +
    '[Video]'#13#10 +
    'Type=Video'#13#10 +
    'VideoInfo=info...'#13#10 +
    'VideoSource=video source...(mp4/rtsp/mkv/rtmp)'#13#10 +
    'UsedFit=False'#13#10 +
    'FitX=1280'#13#10 +
    'FitY=720'#13#10 +
    'Cuvid=False'#13#10 +
    'UsedCorrelationTracker=False'#13#10 +
    'MaxTrackerThread=5'#13#10 +
    'MaxInputQueue=0'#13#10 +
    'FullGPU=True'#13#10 +
    'AsyncODProcessor=True'#13#10 +
    'ThreadSleep=0'#13#10 +
    'Encode=True'#13#10 +
    'EncodeTimeTickLong=exp(10*1000)'#13#10 +
    'EncodeLaunchMinFrame=100'#13#10 +
    'EncodeLaunchMaxFrame=500'#13#10 +
    'EncodePSF=25'#13#10 +
    'EncodeGOP=15'#13#10 +
    'EncodeBFrame=0'#13#10 +
    'EncodeBitrate=exp(1024*1024)'#13#10 +
    'VideoPush=False'#13#10 +
    'VideoPushURL=rtsp/rtmp/rtp'#13#10 +
    'ShowRealTimeWindow=False'#13#10 +
    'ShowEncoderWindow=False'#13#10 +
    'ShowAIWindow=False'#13#10 +
    'ODProcessor=Detector'#13#10;

implementation

constructor TConfigDataList.Create;
begin
  inherited Create;
  Metric_Class := TConfigData_Classifier_Metric;
  ZMetric_Class := TConfigData_Classifier_ZMetric;
  OD3L_Class := TConfigData_OD3L;
  OD6L_Class := TConfigData_OD6L;
  VideoSource_Class := TConfigData_VideoSource;
end;

destructor TConfigDataList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TConfigDataList.Remove(obj: TConfigData);
begin
  DisposeObject(obj);
  inherited Remove(obj);
end;

procedure TConfigDataList.Delete(Index: Integer);
begin
  if (index >= 0) and (index < Count) then
    begin
      DisposeObject(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TConfigDataList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      DisposeObject(Items[i]);
  inherited Clear;
end;

procedure TConfigDataList.LoadConfigure(te: TTextDataEngine);
var
  ns: TPascalStringList;
  i: Integer;
  Name, Type_: U_String;
  vl: THashVariantList;
  D: TConfigData;
  OD: TConfigData_OD;
  Metric: TConfigData_Classifier_Metric;
  ZMetric: TConfigData_Classifier_ZMetric;
  Video: TConfigData_VideoSource;
  j: Integer;
begin
  ns := TPascalStringList.Create;
  te.GetSectionList(ns);

  for i := 0 to ns.Count - 1 do
    begin
      name := ns[i];
      vl := te.VariantList[name];
      Type_.Text := vl.GetDefaultValue('Type', '');
      if Type_.Same('OD', 'MMOD', 'OD6L', 'MMOD6L', 'Detector', 'Detect', 'ObjectDetect', 'Object') then
        begin
          OD := OD6L_Class.Create;
          OD.Name := name;
          OD.Type_ := Type_;
          try
            OD.Read(vl);
            Add(OD);
          except
              DoStatus('configure %s error.', [name.Text]);
          end;
        end
      else if Type_.Same('OD3L', 'MMOD3L', 'Detector3L', 'Detect3L', 'ObjectDetect3L', 'Object3L') then
        begin
          OD := OD3L_Class.Create;
          OD.Name := name;
          OD.Type_ := Type_;
          try
            OD.Read(vl);
            Add(OD);
          except
              DoStatus('configure %s error.', [name.Text]);
          end;
        end
      else if Type_.Same('ZMetric') then
        begin
          ZMetric := ZMetric_Class.Create;
          ZMetric.Name := name;
          ZMetric.Type_ := Type_;
          try
            ZMetric.Read(vl);
            Add(ZMetric);
          except
              DoStatus('configure %s error.', [name.Text]);
          end;
        end
      else if Type_.Same('Metric') then
        begin
          Metric := Metric_Class.Create;
          Metric.Name := name;
          Metric.Type_ := Type_;
          try
            Metric.Read(vl);
            Add(Metric);
          except
              DoStatus('configure %s error.', [name.Text]);
          end;
        end
      else if Type_.Same('Video', 'FFMPEG', 'RTSP', 'VideoSource', 'Camera') then
        begin
          Video := VideoSource_Class.Create;
          Video.Name := Name;
          Video.Type_ := Type_;
          try
            Video.Read(vl);
            Add(Video);
          except
              DoStatus('configure %s error.', [name.Text]);
          end;
        end
      else
          DoStatus('configrue %s error type %s', [name.Text, Type_.Text]);
    end;
  DisposeObject(ns);

  for i := 0 to Count - 1 do
    begin
      if Items[i] is TConfigData_OD then
        begin
          OD := Items[i] as TConfigData_OD;
          SetLength(OD.ClassifierProcessorInstance, Length(OD.ClassifierProcessor));
          for j := 0 to Length(OD.ClassifierProcessorInstance) - 1 do
            begin
              D := FindConfigData(OD.ClassifierProcessor[j]);
              if D is TConfigData_Classifier then
                  OD.ClassifierProcessorInstance[j] := D as TConfigData_Classifier
              else
                  DoStatus('%s classifier Processor configure %s error.', [OD.Name.Text, OD.ClassifierProcessor[j]]);
            end;
          if Length(OD.ClassifierProcessorInstance) = 0 then
              DoStatus('%s No classifier configure.', [OD.Name.Text]);
        end
      else if Items[i] is TConfigData_VideoSource then
        begin
          Video := Items[i] as TConfigData_VideoSource;
          SetLength(Video.ODProcessorInstance, Length(Video.ODProcessor));
          for j := 0 to Length(Video.ODProcessorInstance) - 1 do
            begin
              D := FindConfigData(Video.ODProcessor[j]);
              if D is TConfigData_OD then
                  Video.ODProcessorInstance[j] := D as TConfigData_OD
              else
                  DoStatus('%s Detector configure %s error.', [Video.Name.Text, Video.ODProcessor[j]]);
            end;
          if Length(Video.ODProcessorInstance) = 0 then
              DoStatus('%s No Detector configure.', [Video.Name.Text]);
        end;
    end;

  for i := 0 to Count - 1 do
      Items[i].ReadDone();
  DoStatus('Load ConfigData done.');
end;

procedure TConfigDataList.LoadConfigureFile(fileName_: U_String);
var
  te: TTextDataEngine;
begin
  te := TTextDataEngine.Create;
  te.LoadFromFile(fileName_);
  LoadConfigure(te);
  DisposeObject(te);
end;

function TConfigDataList.FindConfigData(Name: U_String): TConfigData;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if name.Same(@Items[i].Name) then
      begin
        Result := Items[i];
        break;
      end;
end;

procedure TConfigDataList.DNNEnabledLastProcessRaster(value_: Boolean);
var
  i: Integer;
  OD6L: TConfigData_OD6L;
  OD3L: TConfigData_OD3L;
  classifier_: TConfigData_Classifier;
begin
  for i := 0 to Count - 1 do
    begin
      if Items[i] is TConfigData_OD6L then
        begin
          OD6L := Items[i] as TConfigData_OD6L;
          OD6L.ODThread.EnabledLastProcessRaster(value_);
        end
      else if Items[i] is TConfigData_OD3L then
        begin
          OD3L := Items[i] as TConfigData_OD3L;
          OD3L.ODThread.EnabledLastProcessRaster(value_);
        end
      else if Items[i] is TConfigData_Classifier then
        begin
          classifier_ := Items[i] as TConfigData_Classifier;
          classifier_.ClassifierThread.EnabledLastProcessRaster(value_);
        end;
    end;
end;

procedure TConfigDataList.BeginUpdateDNNLastProcessRaster(viewer: TPictureViewerInterface);
  procedure DoUpdateViewer(L: TMemoryRasterList);
  var
    i: Integer;
    Data_: TPictureViewerData;
  begin
    for i := 0 to L.Count - 1 do
      begin
        Data_ := viewer.FoundPicture(L[i]);
        if Data_ = nil then
            Data_ := viewer.InputPicture(L[i], True, False);
        Data_.texInfo := Data_.Raster.UserText;
      end;
  end;

var
  i: Integer;
  OD6L: TConfigData_OD6L;
  OD3L: TConfigData_OD3L;
  ZMetric: TConfigData_Classifier_ZMetric;
begin
  for i := 0 to Count - 1 do
    begin
      if Items[i] is TConfigData_OD6L then
        begin
          OD6L := Items[i] as TConfigData_OD6L;
          DoUpdateViewer(OD6L.ODThread.LockLastRasterList);
        end
      else if Items[i] is TConfigData_OD3L then
        begin
          OD3L := Items[i] as TConfigData_OD3L;
          DoUpdateViewer(OD3L.ODThread.LockLastRasterList);
        end
      else if Items[i] is TConfigData_Classifier_ZMetric then
        begin
          ZMetric := Items[i] as TConfigData_Classifier_ZMetric;
          DoUpdateViewer(ZMetric.ClassifierThread.LockLastRasterList);
        end;
    end;
end;

procedure TConfigDataList.EndUpdateDNNLastProcessRaster();
var
  i: Integer;
  OD6L: TConfigData_OD6L;
  OD3L: TConfigData_OD3L;
  ZMetric: TConfigData_Classifier_ZMetric;
begin
  for i := 0 to Count - 1 do
    begin
      if Items[i] is TConfigData_OD6L then
        begin
          OD6L := Items[i] as TConfigData_OD6L;
          OD6L.ODThread.UnLockLastRasterList;
        end
      else if Items[i] is TConfigData_OD3L then
        begin
          OD3L := Items[i] as TConfigData_OD3L;
          OD3L.ODThread.UnLockLastRasterList;
        end
      else if Items[i] is TConfigData_Classifier_ZMetric then
        begin
          ZMetric := Items[i] as TConfigData_Classifier_ZMetric;
          ZMetric.ClassifierThread.UnLockLastRasterList;
        end;
    end;
end;

constructor TConfigData.Create;
begin
  inherited Create;
  Name := '';
  Type_ := '';
end;

destructor TConfigData.Destroy;
begin
  inherited Destroy;
end;

constructor TConfigData_Classifier.Create;
begin
  inherited Create;
  ClassifierThread := TAI_DNN_ThreadPool.Create;
  GPUThread := 1;
  GPU := '';
  Model := '';
  CPUThreadCritical := 10;
  GPUPerformanceCritical := 10;
end;

destructor TConfigData_Classifier.Destroy;
begin
  DisposeObjectAndNil(ClassifierThread);
  inherited Destroy;
end;

procedure TConfigData_Classifier.Read(vl: THashVariantList);
begin
  GPUThread := vl.GetDefaultValue('GPUThread', GPUThread);
  GPU := vl.GetDefaultValue('GPUDevice', GPU.Text);
  Model := vl.GetDefaultValue('Model', Model.Text);
  CPUThreadCritical := vl.GetDefaultValue('CPUThreadCritical', CPUThreadCritical);
  GPUPerformanceCritical := vl.GetDefaultValue('GPUPerformanceCritical', GPUPerformanceCritical);

  if not FileExistsFromConfigure(Model) then
    begin
      DoStatus('configure %s Model file Error: %s', [name.Text, Model.Text]);
      exit;
    end;
end;

procedure TConfigData_Classifier_Metric.MetricAsyncResult(ThSender: TAI_DNN_Thread_Metric; UserData: Pointer; Input: TMemoryRaster; output: TLVec);
var
  MetricInfo: PVideoData_MetricInfo;
  i: TLInt;
begin
  MetricInfo := UserData;
  i := L.ProcessMaxIndex(output);
  MetricInfo^.k := LDistance(output, L[i]^.m_in);
  if (MinK < 0) or (MinK > MetricInfo^.k) then
      MetricInfo^.Token := L[i]^.Token
  else
      MetricInfo^.Token := LossInfo;
  MetricInfo^.Done := True;
end;

constructor TConfigData_Classifier_Metric.Create;
begin
  inherited Create;
  L := TAI.Build_Metric_ResNet_Learn;
  LearnFile := '';
  MinK := -1;
  LossInfo := '';
end;

destructor TConfigData_Classifier_Metric.Destroy;
begin
  DisposeObjectAndNil(L);
  inherited Destroy;
end;

procedure TConfigData_Classifier_Metric.Read(vl: THashVariantList);
begin
  inherited Read(vl);
  LearnFile := vl.GetDefaultValue('Learn', LearnFile.Text);
  MinK := vl.GetDefaultValue('MinK', MinK);
  LossInfo := vl.GetDefaultValue('LossInfo', LossInfo.Text);
end;

procedure TConfigData_Classifier_Metric.ReadDone;
var
  i: Integer;
begin
  if not FileExistsFromConfigure(Model) then
    begin
      DoStatus('configure %s Model file Error: %s', [name.Text, Model.Text]);
      exit;
    end;

  if not FileExistsFromConfigure(LearnFile) then
    begin
      DoStatus('configure %s Learn file Error: %s', [name.Text, Model.Text]);
      exit;
    end;

  ClassifierThread.BuildPerDeviceThread(ExpLIVec(GPU), GPUThread, TAI_DNN_Thread_Metric);
  for i := 0 to ClassifierThread.Count - 1 do
    begin
      ClassifierThread[i].Name := Name;
      ClassifierThread[i].CPUThreadCritical := CPUThreadCritical;
      ClassifierThread[i].GPUPerformanceCritical := GPUPerformanceCritical;
      TAI_DNN_Thread_Metric(ClassifierThread[i]).Open(WhereFileFromConfigure(Model));
    end;
  L.LoadFromFile(WhereFileFromConfigure(LearnFile));
end;

procedure TConfigData_Classifier_Metric.Process(Rect_: TRectV2; p: PVideoData_MetricInfo; Raster: TRaster);
begin
  TAI_DNN_Thread_Metric(ClassifierThread.MinLoad_DNN_Thread).ProcessM(p,
    Raster.BuildAreaOffsetScaleSpace(Rect_, zAI.C_Metric_Input_Size, zAI.C_Metric_Input_Size),
    True, {$IFDEF FPC}@{$ENDIF FPC}MetricAsyncResult);
end;

procedure TConfigData_Classifier_ZMetric.ZMetricAsyncResult(ThSender: TAI_DNN_Thread_ZMetric; UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; output: TLVec);
var
  MetricInfo: PVideoData_MetricInfo;
  i: TLInt;
begin
  MetricInfo := UserData;
  i := L.ProcessMaxIndex(output);
  MetricInfo^.k := LDistance(output, L[i]^.m_in);
  if (MinK < 0) or (MinK > MetricInfo^.k) then
      MetricInfo^.Token := L[i]^.Token
  else
      MetricInfo^.Token := LossInfo;
  MetricInfo^.Done := True;
end;

constructor TConfigData_Classifier_ZMetric.Create;
begin
  inherited Create;
  SS_Width := 150;
  SS_Height := 150;
  L := TAI.Build_ZMetric_Learn;
  LearnFile := '';
  MinK := -1;
  LossInfo := '';
end;

destructor TConfigData_Classifier_ZMetric.Destroy;
begin
  DisposeObjectAndNil(L);
  inherited Destroy;
end;

procedure TConfigData_Classifier_ZMetric.Read(vl: THashVariantList);
begin
  inherited Read(vl);
  SS_Width := vl.GetDefaultValue('SS_Width', SS_Width);
  SS_Height := vl.GetDefaultValue('SS_Height', SS_Height);
  LearnFile := vl.GetDefaultValue('Learn', LearnFile.Text);
  MinK := vl.GetDefaultValue('MinK', MinK);
  LossInfo := vl.GetDefaultValue('LossInfo', LossInfo.Text);
end;

procedure TConfigData_Classifier_ZMetric.ReadDone;
var
  i: Integer;
begin
  if not FileExistsFromConfigure(Model) then
    begin
      DoStatus('configure %s Model file Error: %s', [name.Text, Model.Text]);
      exit;
    end;

  if not FileExistsFromConfigure(LearnFile) then
    begin
      DoStatus('configure %s Learn file Error: %s', [name.Text, Model.Text]);
      exit;
    end;

  ClassifierThread.BuildPerDeviceThread(ExpLIVec(GPU), GPUThread, TAI_DNN_Thread_ZMetric);
  for i := 0 to ClassifierThread.Count - 1 do
    begin
      ClassifierThread[i].Name := Name;
      ClassifierThread[i].CPUThreadCritical := CPUThreadCritical;
      ClassifierThread[i].GPUPerformanceCritical := GPUPerformanceCritical;
      TAI_DNN_Thread_ZMetric(ClassifierThread[i]).Open(WhereFileFromConfigure(Model));
    end;
  L.LoadFromFile(WhereFileFromConfigure(LearnFile));
end;

procedure TConfigData_Classifier_ZMetric.Process(Rect_: TRectV2; p: PVideoData_MetricInfo; Raster: TRaster);
begin
  TAI_DNN_Thread_ZMetric(ClassifierThread.MinLoad_DNN_Thread).ProcessM(p,
    Raster.BuildAreaOffsetScaleSpace(Rect_, SS_Width, SS_Height),
    SS_Width, SS_Height, True, {$IFDEF FPC}@{$ENDIF FPC}ZMetricAsyncResult);
end;

constructor TConfigData_OD.Create;
begin
  inherited Create;
  UsedFit := False;
  FitX := 1280;
  FitY := 720;
  ODThread := TAI_DNN_ThreadPool.Create;
  GPUThread := 1;
  GPU := '';
  Model := '';
  CPUThreadCritical := 10;
  GPUPerformanceCritical := 10;
  SetLength(ClassifierProcessor, 0);
  SetLength(ClassifierProcessorInstance, 0);
end;

destructor TConfigData_OD.Destroy;
begin
  DisposeObjectAndNil(ODThread);
  inherited Destroy;
end;

procedure TConfigData_OD.Read(vl: THashVariantList);
begin
  UsedFit := vl.GetDefaultValue('UsedFit', UsedFit);
  FitX := vl.GetDefaultValue('FitX', FitX);
  FitY := vl.GetDefaultValue('FitY', FitX);
  GPUThread := vl.GetDefaultValue('GPUThread', GPUThread);
  GPU := vl.GetDefaultValue('GPUDevice', GPU.Text);
  Model := vl.GetDefaultValue('Model', Model.Text);
  CPUThreadCritical := vl.GetDefaultValue('CPUThreadCritical', CPUThreadCritical);
  GPUPerformanceCritical := vl.GetDefaultValue('GPUPerformanceCritical', GPUPerformanceCritical);
  umlGetSplitArray(vl.GetDefaultValue('ClassifierProcessor', ''), ClassifierProcessor, ',;|');
  SetLength(ClassifierProcessorInstance, 0);
end;

procedure TConfigData_OD.ReadDone;
begin

end;

procedure TConfigData_OD.ProcessClassifier(ODInfo: PVideoData_ODInfo; Raster: TMemoryRaster; MMOD_Desc: TMMOD_Desc);
var
  i, j: Integer;
begin
  ODInfo^.Init(Length(MMOD_Desc));

  if Length(MMOD_Desc) > 0 then
    begin
      for i := 0 to Length(MMOD_Desc) - 1 do
        begin
          ODInfo^.ODBoxInfo[i].Init(ODInfo, Length(ClassifierProcessorInstance));
          ODInfo^.ODBoxInfo[i].Box := RectProjection(Raster.BoundsRectV2, ODInfo^.Owner^.Raster.BoundsRectV2, MMOD_Desc[i].R);
          ODInfo^.ODBoxInfo[i].confidence := MMOD_Desc[i].confidence;
          ODInfo^.ODBoxInfo[i].Token := MMOD_Desc[i].Token;
        end;

      for i := 0 to Length(ODInfo^.ODBoxInfo) - 1 do
        begin
          for j := 0 to Length(ClassifierProcessorInstance) - 1 do
            if ClassifierProcessorInstance[j].ClassifierThread.Count > 0 then
              begin
                ClassifierProcessorInstance[j].Process(MMOD_Desc[i].R, @ODInfo^.ODBoxInfo[i].Metric[j], Raster);
              end;
        end;
    end;
  ODInfo^.Done := True;
end;

procedure TConfigData_OD6L.ProcessTh(ThSender: TCompute);
var
  th: TAI_DNN_Thread_MMOD6L;
  Raster: TRaster;
begin
  th := TAI_DNN_Thread_MMOD6L(ODThread.MinLoad_DNN_Thread);
  if UsedFit then
      Raster := TRaster(ThSender.UserObject).NonlinearFitScaleAsNew(FitX, FitY)
  else
      Raster := TRaster(ThSender.UserObject);
  th.ProcessM(ThSender.UserData, Raster, UsedFit, {$IFDEF FPC}@{$ENDIF FPC}ODAsyncResult);
end;

procedure TConfigData_OD6L.ODAsyncResult(ThSender: TAI_DNN_Thread_MMOD6L; UserData: Pointer; Raster: TMemoryRaster; MMOD_Desc: TMMOD_Desc);
begin
  ProcessClassifier(UserData, Raster, MMOD_Desc);
end;

procedure TConfigData_OD6L.Read(vl: THashVariantList);
var
  i: Integer;
begin
  inherited Read(vl);

  if not FileExistsFromConfigure(Model) then
    begin
      DoStatus('configure %s Model file Error: %s', [name.Text, Model.Text]);
      exit;
    end;

  ODThread.BuildPerDeviceThread(ExpLIVec(GPU), GPUThread, TAI_DNN_Thread_MMOD6L);
  for i := 0 to ODThread.Count - 1 do
    begin
      ODThread[i].Name := Name;
      ODThread[i].CPUThreadCritical := CPUThreadCritical;
      ODThread[i].GPUPerformanceCritical := GPUPerformanceCritical;
      TAI_DNN_Thread_MMOD6L(ODThread[i]).Open(WhereFileFromConfigure(Model));
    end;
end;

procedure TConfigData_OD6L.Process(Async_: Boolean; ODInfo: PVideoData_ODInfo; Raster: TRaster);
begin
  if ODThread.Count = 0 then
    begin
      ODInfo^.Init(0);
      ODInfo^.Done := True;
      exit;
    end;
  if UsedFit or Async_ then
      TCompute.RunM(ODInfo, Raster, {$IFDEF FPC}@{$ENDIF FPC}ProcessTh)
  else
      TAI_DNN_Thread_MMOD6L(ODThread.MinLoad_DNN_Thread).ProcessM(ODInfo, Raster, False, {$IFDEF FPC}@{$ENDIF FPC}ODAsyncResult);
end;

procedure TConfigData_OD3L.ProcessTh(ThSender: TCompute);
var
  th: TAI_DNN_Thread_MMOD3L;
  Raster: TRaster;
begin
  th := TAI_DNN_Thread_MMOD3L(ODThread.MinLoad_DNN_Thread);
  if UsedFit then
      Raster := TRaster(ThSender.UserObject).NonlinearFitScaleAsNew(FitX, FitY)
  else
      Raster := TRaster(ThSender.UserObject);
  th.ProcessM(ThSender.UserData, Raster, UsedFit, {$IFDEF FPC}@{$ENDIF FPC}ODAsyncResult);
end;

procedure TConfigData_OD3L.ODAsyncResult(ThSender: TAI_DNN_Thread_MMOD3L; UserData: Pointer; Raster: TMemoryRaster; MMOD_Desc: TMMOD_Desc);
begin
  ProcessClassifier(UserData, Raster, MMOD_Desc);
end;

procedure TConfigData_OD3L.Read(vl: THashVariantList);
var
  i: Integer;
begin
  inherited Read(vl);

  if not FileExistsFromConfigure(Model) then
    begin
      DoStatus('configure %s Model file Error: %s', [name.Text, Model.Text]);
      exit;
    end;

  ODThread.BuildPerDeviceThread(ExpLIVec(GPU), GPUThread, TAI_DNN_Thread_MMOD3L);
  for i := 0 to ODThread.Count - 1 do
    begin
      ODThread[i].Name := Name;
      ODThread[i].CPUThreadCritical := CPUThreadCritical;
      ODThread[i].GPUPerformanceCritical := GPUPerformanceCritical;
      TAI_DNN_Thread_MMOD3L(ODThread[i]).Open(WhereFileFromConfigure(Model));
    end;
end;

procedure TConfigData_OD3L.Process(Async_: Boolean; ODInfo: PVideoData_ODInfo; Raster: TRaster);
begin
  if ODThread.Count = 0 then
    begin
      ODInfo^.Init(0);
      ODInfo^.Done := True;
      exit;
    end;
  if UsedFit or Async_ then
      TCompute.RunM(ODInfo, Raster, {$IFDEF FPC}@{$ENDIF FPC}ProcessTh)
  else
      TAI_DNN_Thread_MMOD3L(ODThread.MinLoad_DNN_Thread).ProcessM(ODInfo, Raster, False, {$IFDEF FPC}@{$ENDIF FPC}ODAsyncResult);
end;

constructor TTrackerData.Create(AI_: TAI);
begin
  inherited Create;
  AI := AI_;
  SetLength(RefrenceDESCBuff, 0, 0);
  SetLength(TrackerHndBuff, 0, 0);
  TrackerInput := TVideoDataList.Create;
  ThNum := 0;
end;

destructor TTrackerData.Destroy;
begin
  while ThNum > 0 do
      TCompute.Sleep(1);
  SetLength(RefrenceDESCBuff, 0, 0);
  SetLength(TrackerHndBuff, 0, 0);
  DisposeObject(TrackerInput);
  inherited Destroy;
end;

procedure TTrackerData.NewRefrenceDESC(VideoData_: PVideoData);
var
  i, j: Integer;
begin
  SetLength(RefrenceDESCBuff, 0, 0);
  SetLength(RefrenceDESCBuff, Length(VideoData_^.ODInfo));
  for i := 0 to Length(VideoData_^.ODInfo) - 1 do
    begin
      SetLength(RefrenceDESCBuff[i], Length(VideoData_^.ODInfo[i].ODBoxInfo));
      for j := 0 to Length(VideoData_^.ODInfo[i].ODBoxInfo) - 1 do
        begin
          RefrenceDESCBuff[i, j].R := VideoData_^.ODInfo[i].ODBoxInfo[j].Box;
          RefrenceDESCBuff[i, j].confidence := VideoData_^.ODInfo[i].ODBoxInfo[j].confidence;
          RefrenceDESCBuff[i, j].Token := VideoData_^.ODInfo[i].ODBoxInfo[j].Token;
        end;
    end;
  try
      TrackerHndBuff := AI.Tracker_Open_Multi(False, VideoData_^.Raster, RefrenceDESCBuff);
  except
  end;
end;

procedure TTrackerComputeInstance.DoRun(ThSender: TCompute);
var
  i, pass: Integer;
  p: PVideoData;
begin
  for i := 0 to TrackerInput.Count - 1 do
    begin
      p := TrackerInput[i];
      try
          AI.Tracker_Update_Multi(False, TrackerHndBuff, p^.Raster, RefrenceDESCBuff);
      except
      end;

      for pass := 0 to Length(RefrenceDESCBuff) - 1 do
        begin
          if (Length(RefrenceDESCBuff) > pass) and (Length(p^.ODInfo) > pass) then
              VideoSource.ODProcessorInstance[pass].ProcessClassifier(@p^.ODInfo[pass], p^.Raster, RefrenceDESCBuff[pass]);
        end;
    end;
  try
    AI.Tracker_Close(TrackerHndBuff);
    DisposeObjectAndNil(TrackerInput);
    SetLength(RefrenceDESCBuff, 0, 0);
  except
  end;
  // update thread state
  AtomDec(ThNum^);
  Free;
end;

constructor TTrackerComputeInstance.Create(Data_: TTrackerData; VideoSource_: TConfigData_VideoSource);
var
  i, j: Integer;
begin
  inherited Create;

  // copy states
  AI := Data_.AI;
  SetLength(RefrenceDESCBuff, Length(Data_.RefrenceDESCBuff));
  for i := 0 to Length(Data_.RefrenceDESCBuff) - 1 do
    begin
      SetLength(RefrenceDESCBuff[i], Length(Data_.RefrenceDESCBuff[i]));
      for j := 0 to Length(Data_.RefrenceDESCBuff[i]) - 1 do
          RefrenceDESCBuff[i, j] := Data_.RefrenceDESCBuff[i, j];
    end;
  // reset states
  SetLength(Data_.RefrenceDESCBuff, 0, 0);

  // copy states
  TrackerHndBuff := Data_.TrackerHndBuff;
  SetLength(TrackerHndBuff, Length(Data_.TrackerHndBuff));
  for i := 0 to Length(Data_.TrackerHndBuff) - 1 do
    begin
      SetLength(TrackerHndBuff[i], Length(Data_.TrackerHndBuff[i]));
      for j := 0 to Length(Data_.TrackerHndBuff[i]) - 1 do
          TrackerHndBuff[i, j] := Data_.TrackerHndBuff[i, j];
    end;
  // reset states
  SetLength(Data_.TrackerHndBuff, 0, 0);

  // reset states
  TrackerInput := Data_.TrackerInput;
  Data_.TrackerInput := TVideoDataList.Create;

  // update thread state
  ThNum := @Data_.ThNum;
  AtomInc(ThNum^);
  VideoSource := VideoSource_;

  // copy states
  TCompute.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}DoRun);
end;

procedure TVideoSofEncodeInstance.FreeOrderStruct(var p: PVideoData);
begin
  p^.Free;
  Dispose(p);
  AtomDec(EncodeNum);
end;

procedure TVideoSofEncodeInstance.RenderAIRaster(p: PVideoData);
var
  D: TDrawEngine;
  i, j: Integer;
  boxInfo: PVideoData_ODBoxInfo;
begin
  D := p^.Raster.DrawEngine;

  for i := 0 to Length(p^.ODInfo) - 1 do
    for j := 0 to Length(p^.ODInfo[i].ODBoxInfo) - 1 do
      begin
        boxInfo := @p^.ODInfo[i].ODBoxInfo[j];

        if Length(boxInfo^.Metric) > 0 then
          begin
            D.DrawLabelBox(
              TDrawEngine.RebuildNumColor(boxInfo^.CombineMetricInfo, '|color(1,0,0)|', '||'),
              14, DEColor(0.5, 0.5, 0.5), boxInfo^.Box, DEColor(1, 1, 1, 0.8), 3);
          end
        else
          begin
            D.DrawBox(boxInfo^.Box, DEColor(1, 1, 1, 0.8), 3);
          end;
      end;

  D.Flush;
  Owner.DoDoneAIRaster(p^.Raster);
end;

procedure TVideoSofEncodeInstance.EncoderThRun(ThSender: TCompute);
var
  HashTokens: THashVariantList;
  Encoder_: TFFMPEG_Writer;
  DataBuffer: TDFE;
  AIEncoder_: TFFMPEG_Writer;
  procedure Flush(BeginTime, EndTime: TDateTime; BeginTick, EndTick: TTimeTick);
  begin
    if (Encoder_ <> nil) and (DataBuffer <> nil) then
      if (DataBuffer.Count > 0) and (Encoder_.Size > 0) then
          Owner.DoDoneEncode(BeginTime, EndTime, EndTick - BeginTick, Encoder_, AIEncoder_, DataBuffer, HashTokens);

    DisposeObjectAndNil(Encoder_);
    DisposeObjectAndNil(DataBuffer);
    HashTokens.Clear;
    DisposeObjectAndNil(AIEncoder_);
  end;

  procedure ExtractMetricInfo(p: PVideoData);
  var
    i, j, k: Integer;
    boxInfo_: PVideoData_ODBoxInfo;
  begin
    for i := 0 to Length(p^.ODInfo) - 1 do
      for j := 0 to Length(p^.ODInfo[i].ODBoxInfo) - 1 do
        begin
          boxInfo_ := @p^.ODInfo[i].ODBoxInfo[j];
          for k := 0 to Length(boxInfo_^.Metric) - 1 do
            begin
              HashTokens.IncValue(boxInfo_^.Metric[k].Token, 1);
            end;
        end;
  end;

var
  p: PVideoData;
  DF: TDFE;
  BeginTime_: TDateTime;
  EndTime_: TDateTime;
  BeginTick_: TTimeTick;
  EndTick_: TTimeTick;
  Updated_: Integer;
begin
  HashTokens := THashVariantList.CustomCreate($FFFF);
  Encoder_ := nil;
  DataBuffer := nil;
  AIEncoder_ := nil;

  BeginTime_ := umlNow();
  EndTime_ := BeginTime_;
  BeginTick_ := GetTimeTick();
  EndTick_ := BeginTick_;

  while Activted.V do
    begin
      if Queue.Current <> nil then
          p := Queue.Current^.Data
      else
          p := nil;

      if (p <> nil) and (p^.Raster <> nil) then
        begin
          if (Encoder_ <> nil) and ((Encoder_.LastWidth <> p^.Raster.Width) or (Encoder_.LastHeight <> p^.Raster.Height)) then
            begin
              DisposeObjectAndNil(Encoder_);
              DisposeObjectAndNil(DataBuffer);
              HashTokens.Clear;
              DisposeObjectAndNil(AIEncoder_);
            end;

          if Encoder_ = nil then
            begin
              try
                Encoder_ := TFFMPEG_Writer.Create(TMemoryStream64.CustomCreate(1024 * 1024));
                Encoder_.AutoFreeOutput := True;
                Encoder_.OpenCodec(TAVCodecID.AV_CODEC_ID_H264,
                  p^.Raster.Width,
                  p^.Raster.Height,
                  Owner.EncodePSF,
                  Owner.EncodeGOP,
                  Owner.EncodeBFrame,
                  Owner.EncodeBitrate);

                BeginTime_ := p^.Time_;
                BeginTick_ := p^.Tick_;

                AIEncoder_ := TFFMPEG_Writer.Create(TMemoryStream64.CustomCreate(1024 * 1024));
                AIEncoder_.AutoFreeOutput := True;
                AIEncoder_.OpenCodec(TAVCodecID.AV_CODEC_ID_H264,
                  p^.Raster.Width,
                  p^.Raster.Height,
                  Owner.EncodePSF,
                  Owner.EncodeGOP,
                  Owner.EncodeBFrame,
                  Owner.EncodeBitrate);
              except
                DisposeObjectAndNil(Encoder_);
                DisposeObjectAndNil(DataBuffer);
                HashTokens.Clear;
                DisposeObjectAndNil(AIEncoder_);
              end;
            end;

          EndTime_ := p^.Time_;
          EndTick_ := p^.Tick_;

          if Encoder_ <> nil then
            begin
              Updated_ := 0;
              Encoder_.EncodeRaster(p^.Raster, Updated_);

              if DataBuffer = nil then
                  DataBuffer := TDFE.Create;

              DF := TDFE.Create;
              p^.Encode(DF);
              DataBuffer.WriteDataFrame(DF);
              DisposeObjectAndNil(DF);

              ExtractMetricInfo(p);

              RenderAIRaster(p);
              AIEncoder_.EncodeRaster(p^.Raster, Updated_);

              if (Updated_ > 0) and
                ((Encoder_.EncodeNum >= Owner.EncodeLaunchMaxFrame) or ((Owner.EncodeTimeTickLong > 0) and
                (p^.Tick_ - BeginTick_ > Owner.EncodeTimeTickLong) and (Encoder_.EncodeNum >= Owner.EncodeLaunchMinFrame))) then
                begin
                  Flush(BeginTime_, EndTime_, BeginTick_, EndTick_);
                end;
            end;

          Queue.Next;
        end
      else
          TCompute.Sleep(1);
    end;
  DisposeObjectAndNil(HashTokens);
  DisposeObjectAndNil(Encoder_);
  DisposeObjectAndNil(DataBuffer);
  DisposeObjectAndNil(AIEncoder_);

  DisposeObjectAndNil(Activted);
  DisposeObjectAndNil(Queue);

  ThIsEnd := True;
end;

constructor TVideoSofEncodeInstance.Create(Owner_: TConfigData_VideoSource);
begin
  inherited Create;
  Owner := Owner_;
  Queue := TTVideoEncodeQueueOrder.Create;
  Queue.OnFree := {$IFDEF FPC}@{$ENDIF FPC}FreeOrderStruct;
  Activted := TAtomBool.Create(True);
  ThIsEnd := False;
  EncodeNum := 0;

  TCompute.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}EncoderThRun);
end;

destructor TVideoSofEncodeInstance.Destroy;
begin
  Activted.V := False;
  while not ThIsEnd do
      TCompute.Sleep(1);
  inherited Destroy;
end;

procedure TVideoSofEncodeInstance.Encode(p: PVideoData);
begin
  Queue.Push(p);
  AtomInc(EncodeNum);
end;

procedure TConfigData_VideoSource.OpenVideo_SyncOD();
var
  Inited: Boolean;
  tmpList: TVideoDataList;
  Tracker_: TTrackerData;
  Now_: TDateTime;
  Tick_: TTimeTick;
  EncodeInstance: TVideoSofEncodeInstance;

  procedure InitLocal;
  begin
    Inited := False;
    tmpList := TVideoDataList.Create;
    if UsedCorrelationTracker then
        Tracker_ := TTrackerData.Create(TAI.OpenEngine);
    if Encode then
        EncodeInstance := TVideoSofEncodeInstance.Create(Self);
  end;

  procedure FreeLocal;
  begin
    if UsedCorrelationTracker then
      begin
        DisposeObjectAndNil(Tracker_.AI);
        DisposeObjectAndNil(Tracker_);
      end;
    DisposeObjectAndNil(tmpList);
    if Encode then
      begin
        DisposeObjectAndNil(EncodeInstance);
      end;

    FrameDoneDNN := 0;
    FrameLoss := 0;
    FrameTracker := 0;
    FrameBusy := 0;
    TrackerThreadNum := 0;
  end;

  procedure UpdateLossRaster(Raster: TRaster);
  var
    p: PVideoData;
  begin
    new(p);
    p^.Init(Length(ODProcessorInstance));
    p^.Time_ := Now_;
    p^.Tick_ := Tick_;
    p^.VideoInfo := VideoInfo;
    p^.Raster := Raster.Clone;
    p^.ID := ID;
    p^.Loss := True;
    tmpList.Add(p);

    if UsedCorrelationTracker and ((MaxInputQueue <= 0) or (tmpList.Count < MaxInputQueue)) then
      begin
        if (MaxTrackerThread <= 0) or (Tracker_.ThNum < MaxTrackerThread) then
          begin
            p^.Loss := False;
            p^.CorrelationTracker := True;
            Tracker_.TrackerInput.Add(p);
          end;
        TrackerThreadNum := Tracker_.ThNum;
      end;

    if p^.Loss then
        inc(FrameLoss)
    else if p^.CorrelationTracker then
        inc(FrameTracker);
  end;

  procedure CheckDoneData;
  var
    p: PVideoData;
  begin
    while tmpList.Count > 0 do
      begin
        p := tmpList[0];
        if p^.Loss or p^.AllIsDone then
          begin
            DoDoneVideoRaster(p);
            if Encode then
                EncodeInstance.Encode(p)
            else
              begin
                p^.Free;
                Dispose(p);
              end;
            tmpList.Delete(0);
          end
        else
            break;
      end;
    FrameBusy := tmpList.Count;
  end;

var
  Raster: TRaster;
  CurrentVideoData_: PVideoData;
  i: Integer;
begin
  Running.V := True;
  Busy.V := True;

  TRaster.Parallel := False;
  Raster := NewRaster();
  CurrentVideoData_ := nil;
  InitLocal;
  while Running.V do
    begin
      try
        Reader := TFFMPEG_Reader.Create(VideoSource, Cuvid);
        if VideoPush then
            DoBeginVideoPush(VideoPushURL);
        VideoSource_X := Reader.Width;
        VideoSource_Y := Reader.Height;
        if UsedFit then
            Reader.ResetFit(FitX, FitY);
        DoResetRaster();
        while Running.V and Reader.ReadFrame(Raster, False) do
          begin
            Now_ := umlNow();
            Tick_ := GetTimeTick;
            DoPrepareRaster(Raster);
            if CurrentVideoData_ <> nil then
              begin
                if CurrentVideoData_^.ODIsDone then
                  begin
                    if UsedCorrelationTracker then
                      begin
                        // copy instance and compute in thread
                        if tmpList.Count > 0 then
                            TTrackerComputeInstance.Create(Tracker_, Self);
                        // new data instance
                        Tracker_.NewRefrenceDESC(CurrentVideoData_);
                        TrackerThreadNum := Tracker_.ThNum;
                      end;
                    CurrentVideoData_ := nil;
                    CheckDoneData();
                  end
                else
                  begin
                    // loss and tracker
                    UpdateLossRaster(Raster);
                  end;
              end;

            if CurrentVideoData_ = nil then
              begin
                // DNN-OD
                new(CurrentVideoData_);
                CurrentVideoData_^.Init(Length(ODProcessorInstance));
                CurrentVideoData_^.Time_ := Now_;
                CurrentVideoData_^.Tick_ := Tick_;
                CurrentVideoData_^.VideoInfo := VideoInfo;
                CurrentVideoData_^.Raster := Raster.Clone;
                CurrentVideoData_^.ID := ID;
                CurrentVideoData_^.Loss := False;
                CurrentVideoData_^.DNN := True;
                inc(FrameDoneDNN);
                for i := 0 to Length(ODProcessorInstance) - 1 do
                    ODProcessorInstance[i].Process(AsyncODProcessor, @CurrentVideoData_^.ODInfo[i], CurrentVideoData_^.Raster);
                while not Inited do
                  begin
                    if CurrentVideoData_^.ODIsDone then
                      begin
                        Inited := True;
                      end
                    else
                        TCompute.Sleep(1);
                  end;
                tmpList.Add(CurrentVideoData_);
              end;
            inc(ID);
            if ThreadSleep > 0 then
                TCompute.Sleep(ThreadSleep);
          end;
        if VideoPush then
            DoEndVideoPush();
        DisposeObjectAndNil(Reader);
      except
        DisposeObjectAndNil(Reader);
        TCompute.Sleep(5000);
      end;
    end;

  TCompute.Sleep(1000);
  DoStatus('wait %s...', [VideoInfo.Text]);
  if UsedCorrelationTracker then
    begin
      while (CurrentVideoData_ <> nil) and (not CurrentVideoData_^.ODIsDone) do
          TCompute.Sleep(1);

      if Tracker_.TrackerInput.Count > 0 then
          TTrackerComputeInstance.Create(Tracker_, Self);

      while Tracker_.ThNum > 0 do
        begin
          if (tmpList.Count > 0) then
              CheckDoneData();
          TrackerThreadNum := Tracker_.ThNum;
          TCompute.Sleep(10);
        end;
    end;

  while (tmpList.Count > 0) do
    begin
      CheckDoneData();
      TCompute.Sleep(100);
    end;

  FreeLocal;
  DisposeObjectAndNil(Raster);
  Busy.V := False;
  DoStatus('%s stop.', [VideoInfo.Text]);
end;

procedure TConfigData_VideoSource.OpenVideo_FullGPU;
var
  tmpList: TVideoDataList;
  Now_: TDateTime;
  Tick_: TTimeTick;
  EncodeInstance: TVideoSofEncodeInstance;

  procedure InitLocal;
  begin
    tmpList := TVideoDataList.Create;
    if Encode then
        EncodeInstance := TVideoSofEncodeInstance.Create(Self);
  end;

  procedure FreeLocal;
  begin
    DisposeObjectAndNil(tmpList);
    if Encode then
      begin
        DisposeObjectAndNil(EncodeInstance);
      end;
    FrameDoneDNN := 0;
    FrameLoss := 0;
    FrameTracker := 0;
    FrameBusy := 0;
    TrackerThreadNum := 0;
  end;

  procedure UpdateLossRaster(Raster: TRaster);
  var
    p: PVideoData;
  begin
    new(p);
    p^.Init(Length(ODProcessorInstance));
    p^.Time_ := Now_;
    p^.Tick_ := Tick_;
    p^.VideoInfo := VideoInfo;
    p^.Raster := Raster.Clone;
    p^.ID := ID;
    p^.Loss := True;
    tmpList.Add(p);
    inc(FrameLoss)
  end;

  procedure CheckDoneData;
  var
    p: PVideoData;
  begin
    while tmpList.Count > 0 do
      begin
        p := tmpList[0];
        if p^.Loss or p^.AllIsDone then
          begin
            DoDoneVideoRaster(p);
            if Encode then
                EncodeInstance.Encode(p)
            else
              begin
                p^.Free;
                Dispose(p);
              end;
            tmpList.Delete(0);
          end
        else
            break;
      end;
    FrameBusy := tmpList.Count;
  end;

var
  Raster: TRaster;
  CurrentVideoData_: PVideoData;
  i: Integer;
begin
  Running.V := True;
  Busy.V := True;

  TRaster.Parallel := False;
  Raster := NewRaster();
  CurrentVideoData_ := nil;
  InitLocal;
  while Running.V do
    begin
      try
        Reader := TFFMPEG_Reader.Create(VideoSource, Cuvid);
        if VideoPush then
            DoBeginVideoPush(VideoPushURL);
        VideoSource_X := Reader.Width;
        VideoSource_Y := Reader.Height;
        if UsedFit then
            Reader.ResetFit(FitX, FitY);
        DoResetRaster();
        while Running.V and Reader.ReadFrame(Raster, False) do
          begin
            Now_ := umlNow();
            Tick_ := GetTimeTick;
            DoPrepareRaster(Raster);

            if (MaxInputQueue > 0) and (tmpList.Count > MaxInputQueue) then
              begin
                UpdateLossRaster(Raster);
              end
            else
              begin
                // DNN-OD
                new(CurrentVideoData_);
                CurrentVideoData_^.Init(Length(ODProcessorInstance));
                CurrentVideoData_^.Time_ := Now_;
                CurrentVideoData_^.Tick_ := Tick_;
                CurrentVideoData_^.VideoInfo := VideoInfo;
                CurrentVideoData_^.Raster := Raster.Clone;
                CurrentVideoData_^.ID := ID;
                CurrentVideoData_^.Loss := False;
                CurrentVideoData_^.DNN := True;
                for i := 0 to Length(ODProcessorInstance) - 1 do
                    ODProcessorInstance[i].Process(AsyncODProcessor, @CurrentVideoData_^.ODInfo[i], CurrentVideoData_^.Raster);
                tmpList.Add(CurrentVideoData_);
                inc(FrameDoneDNN);
              end;

            inc(ID);
            CheckDoneData();
            if ThreadSleep > 0 then
                TCompute.Sleep(ThreadSleep);
          end;
        if VideoPush then
            DoEndVideoPush();
        DisposeObjectAndNil(Reader);
      except
        DisposeObjectAndNil(Reader);
        TCompute.Sleep(5000);
      end;
    end;

  TCompute.Sleep(1000);
  DoStatus('wait %s...', [VideoInfo.Text]);

  while (tmpList.Count > 0) do
    begin
      CheckDoneData();
      TCompute.Sleep(100);
    end;

  FreeLocal;
  DisposeObjectAndNil(Raster);
  Busy.V := False;
  DoStatus('%s stop.', [VideoInfo.Text]);
end;

constructor TConfigData_VideoSource.Create;
begin
  inherited Create;

  // base prameter
  Reader := nil;
  VideoSource := '';
  VideoInfo := '';
  VideoSource_X := 0;
  VideoSource_Y := 0;
  UsedFit := False;
  FitX := 1280;
  FitY := 720;
  Cuvid := False;
  UsedCorrelationTracker := False;
  MaxTrackerThread := 5;
  MaxInputQueue := 100;
  FullGPU := True;
  AsyncODProcessor := True;
  ThreadSleep := 0;

  // encoder
  Encode := False;
  EncodeTimeTickLong := 10 * 1000;
  EncodeLaunchMinFrame := 100;
  EncodeLaunchMaxFrame := 500;
  EncodePSF := 25;
  EncodeGOP := 15;
  EncodeBFrame := 0;
  EncodeBitrate := 1024 * 1024;

  // push
  VideoPush := False;
  VideoPushURL := '';

  // window
  ShowRealTimeWindow := False;
  ShowEncoderWindow := False;
  ShowAIWindow := False;

  // processor
  SetLength(ODProcessor, 0);
  SetLength(ODProcessorInstance, 0);

  // state
  Running := TAtomBool.Create(False);
  Busy := TAtomBool.Create(False);
  ID := 0;
  FrameDoneDNN := 0;
  FrameLoss := 0;
  FrameTracker := 0;
  FrameBusy := 0;
  TrackerThreadNum := 0;

  // event
  OnOpenVideo := nil;
  OnCloseVideo := nil;
  OnVideoBeginPush := nil;
  OnVideoEndPush := nil;
  OnResetRaster := nil;
  OnPrepareRaster := nil;
  OnDoneVideoRaster := nil;
  OnDoneAIRaster := nil;
  OnDoneEncode := nil;

  // user define
  UserData := nil;
  UserObject := nil;
end;

destructor TConfigData_VideoSource.Destroy;
var
  isFree_: Boolean;
begin
  Running.V := False;

  while Busy.V do
      TCompute.Sleep(10);

  DisposeObjectAndNil(Reader);
  DisposeObjectAndNil(Running);
  DisposeObjectAndNil(Busy);
  inherited Destroy;
end;

procedure TConfigData_VideoSource.Read(vl: THashVariantList);
begin
  VideoSource := Name;
  VideoInfo := Name;

  // video info
  VideoSource.Text := vl.GetDefaultValue('VideoSource', VideoSource.Text);
  VideoInfo.Text := vl.GetDefaultValue('VideoInfo', VideoInfo.Text);

  // fit
  UsedFit := vl.GetDefaultValue('UsedFit', UsedFit);
  FitX := vl.GetDefaultValue('FitX', FitX);
  FitY := vl.GetDefaultValue('FitY', FitX);

  // gpu switch
  Cuvid := vl.GetDefaultValue('Cuvid', Cuvid);

  // tracker
  UsedCorrelationTracker := vl.GetDefaultValue('UsedCorrelationTracker', UsedCorrelationTracker);
  MaxTrackerThread := vl.GetDefaultValue('MaxTrackerThread', MaxTrackerThread);

  // queue
  MaxInputQueue := vl.GetDefaultValue('MaxInputQueue', MaxInputQueue);

  // performance
  FullGPU := vl.GetDefaultValue('FullGPU', FullGPU);
  AsyncODProcessor := vl.GetDefaultValue('AsyncODProcessor', AsyncODProcessor);
  ThreadSleep := vl.GetDefaultValue('ThreadSleep', ThreadSleep);

  // encoder
  Encode := vl.GetDefaultValue('Encode', Encode);
  EncodeTimeTickLong := vl.GetDefaultValue('EncodeTimeTickLong', EncodeTimeTickLong);
  EncodeLaunchMinFrame := vl.GetDefaultValue('EncodeLaunchMinFrame', EncodeLaunchMinFrame);
  EncodeLaunchMaxFrame := vl.GetDefaultValue('EncodeLaunchMaxFrame', EncodeLaunchMaxFrame);
  EncodePSF := vl.GetDefaultValue('EncodePSF', EncodePSF);
  EncodeGOP := vl.GetDefaultValue('EncodeGOP', EncodeGOP);
  EncodeBFrame := vl.GetDefaultValue('EncodeBFrame', EncodeBFrame);
  EncodeBitrate := vl.GetDefaultValue('EncodeBitrate', EncodeBitrate);

  // push
  VideoPushURL := vl.GetDefaultValue('VideoPushURL', VideoPushURL);
  VideoPush := vl.GetDefaultValue('VideoPush', VideoPush and (VideoPushURL.L > 0));

  // window
  ShowRealTimeWindow := vl.GetDefaultValue('ShowRealTimeWindow', ShowRealTimeWindow);
  ShowEncoderWindow := vl.GetDefaultValue('ShowEncoderWindow', ShowEncoderWindow);
  ShowAIWindow := vl.GetDefaultValue('ShowAIWindow', ShowAIWindow);

  // detector
  umlGetSplitArray(vl.GetDefaultValue('ODProcessor', ''), ODProcessor, ',;|');
  SetLength(ODProcessorInstance, 0);
end;

procedure TConfigData_VideoSource.ReadDone;
begin
end;

procedure TConfigData_VideoSource.OpenVideo;
begin
  DoOpenVideo;

  if FullGPU then
      OpenVideo_FullGPU
  else
      OpenVideo_SyncOD;

  DoCloseVideo;
end;

procedure TConfigData_VideoSource.OpenVideoOnThread;
begin
  TCompute.RunM_NP({$IFDEF FPC}@{$ENDIF FPC}OpenVideo);
  while not Busy.V do
      TCompute.Sleep(1);
end;

procedure TConfigData_VideoSource.CloseVideo;
begin
  Running.V := False;
end;

procedure TConfigData_VideoSource.DoOpenVideo;
begin
  if Assigned(OnOpenVideo) then
    begin
      try
          OnOpenVideo(Self);
      except
      end;
    end;
end;

procedure TConfigData_VideoSource.DoCloseVideo;
begin
  if Assigned(OnCloseVideo) then
    begin
      try
          OnCloseVideo(Self);
      except
      end;
    end;
end;

procedure TConfigData_VideoSource.DoBeginVideoPush(PushURL_: U_String);
begin
  if Assigned(OnVideoBeginPush) then
    begin
      try
          OnVideoBeginPush(Self, PushURL_);
      except
      end;
    end;
end;

procedure TConfigData_VideoSource.DoEndVideoPush;
begin
  if Assigned(OnVideoEndPush) then
    begin
      try
          OnVideoEndPush(Self);
      except
      end;
    end;
end;

procedure TConfigData_VideoSource.DoResetRaster();
begin
  if Assigned(OnResetRaster) then
    begin
      try
          OnResetRaster(Self);
      except
      end;
    end;
end;

procedure TConfigData_VideoSource.DoPrepareRaster(Raster: TRaster);
begin
  if Assigned(OnPrepareRaster) then
    begin
      try
          OnPrepareRaster(Self, Raster);
      except
      end;
    end;
end;

procedure TConfigData_VideoSource.DoDoneVideoRaster(p: PVideoData);
begin
  if Assigned(OnDoneVideoRaster) then
    begin
      try
          OnDoneVideoRaster(Self, p);
      except
      end;
    end;
end;

procedure TConfigData_VideoSource.DoDoneAIRaster(Raster: TRaster);
begin
  if Assigned(OnDoneAIRaster) then
    begin
      try
          OnDoneAIRaster(Self, Raster);
      except
      end;
    end;
end;

procedure TConfigData_VideoSource.DoDoneEncode(BeginTime_, EndTime_: TDateTime; Long_: TTimeTick; VideoBuffer, AIRendererBuffer: TFFMPEG_Writer; DataBuffer: TDFE; HashTokens: THashVariantList);
begin
  if Assigned(OnDoneEncode) then
    begin
      try
          OnDoneEncode(Self, BeginTime_, EndTime_, Long_, VideoBuffer, AIRendererBuffer, DataBuffer, HashTokens);
      except
      end;
    end;
end;

end.
