unit realtime_MMOD_Video_FMXClientFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Objects, FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.ExtCtrls,

  System.IOUtils,

  CoreClasses, DoStatusIO,
  zDrawEngineInterface_SlowFMX, zDrawEngine, MemoryRaster, MemoryStream64,
  PascalStrings, UnicodeMixedLib, Geometry2DUnit, Geometry3DUnit, Cadencer, FFMPEG, FFMPEG_Reader,
  CommunicationFramework, CommunicationFrameworkDoubleTunnelIO_NoAuth, PhysicsIO,
  zAI_RealTime_MMOD_VideoClient;

type
  Trealtime_MMOD_Video_FMXClientForm = class(TForm, ICadencerProgressInterface)
    SysProgress_Timer: TTimer;
    Video_RealSendTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure SysProgress_TimerTimer(Sender: TObject);
    procedure Video_RealSendTimerTimer(Sender: TObject);
  private
    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
    procedure CadencerProgress(const deltaTime, newTime: Double);
    procedure OD_Result(Sender: TRealTime_MMOD_VideoClient; video_stream: TMemoryStream64; video_info: TMMOD_Video_Info);
  public
    drawIntf: TDrawEngineInterface_FMX;
    // ffmpeg的视频贞解码引擎，Demo只支持文件，推流和串流，自行实现
    mpeg_r: TFFMPEG_Reader;
    // 服务器发回来的当前贞
    mpeg_frame: TDETexture;
    // 定时器引擎
    cadencer_eng: TCadencer;
    // mmod专用客户端接口
    realtime_od_cli: TRealTime_MMOD_VideoClient;
    procedure CheckConnect;
    procedure DoInput;
  end;

var
  realtime_MMOD_Video_FMXClientForm: Trealtime_MMOD_Video_FMXClientForm;

implementation

{$R *.fmx}


procedure Trealtime_MMOD_Video_FMXClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusMethod);
  // 使用zDrawEngine做外部绘图时(比如游戏，面向paintbox)，都需要一个绘图接口
  // TDrawEngineInterface_FMX是面向FMX的绘图core接口
  // 如果不指定绘图接口，zDrawEngine会默认使用软件光栅绘图(比较慢)
  drawIntf := TDrawEngineInterface_FMX.Create;

  // 超市Demo的数据集在 binary\market_training.OX 中
  // 使用命令行训练，TrainingTool.exe "-i:c:\zAI\Binary\market_training.OX" "-o:c:\market_training_output.ox"
  // 训练比较耗时，在GTX Titan X nvidia公版gpu卡中，花了5-7小时间，设备配置如果不够，不用浪费时间做训练
  // 待训练完成，将market_training_output.ox中的output.svm_dnn_od复制成c:\zAI\Binary\RealTime_MMOD.svm_dnn_od，重启服务器即可使用

  // 带有重叠样本的训练超参数使用FilePackageTool.exe打开market_training.OX，阅读param.txt的赋值即可
  // 制作和训练视频样本，请参考DNN-OD的建模指南文档
  // 本demo的所有识别处理均由gpu服务器完成，前端支持android，ios，任何IOT设备

  // 使用ffmpeg的视频帧解码器打开超市视频，market2.mp4是降低过分辨率的视频，原版视频分辨率太高，因为串流时需要jpeg编码解码，影响性能
  mpeg_r := TFFMPEG_Reader.Create(umlCombineFileName(TPath.GetLibraryPath, 'market2.mp4'));

  // 当前绘制的视频帧
  mpeg_frame := TDrawEngine.NewTexture;

  // cadencer引擎
  cadencer_eng := TCadencer.Create;
  cadencer_eng.ProgressInterface := Self;

  realtime_od_cli := TRealTime_MMOD_VideoClient.Create(TPhysicsClient.Create, TPhysicsClient.Create);
  realtime_od_cli.On_MMOD_Result := OD_Result;
  CheckConnect;
end;

procedure Trealtime_MMOD_Video_FMXClientForm.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  d: TDrawEngine;
begin
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);
  d.ViewOptions := [voFPS];
  d.FPSFontColor := DEColor(0.5, 0.5, 1, 1);

  d.FillBox(d.ScreenRect, DEColor(0, 0, 0, 1));
  d.FitDrawPicture(mpeg_frame, mpeg_frame.BoundsRectV2, d.ScreenRect, 1.0);

  // 执行绘图指令
  d.Flush;
end;

procedure Trealtime_MMOD_Video_FMXClientForm.SysProgress_TimerTimer(Sender: TObject);
begin
  realtime_od_cli.Progress;
end;

procedure Trealtime_MMOD_Video_FMXClientForm.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  DrawPool(Self).PostScrollText(5, AText, 16, DEColor(1, 1, 1, 1));
end;

procedure Trealtime_MMOD_Video_FMXClientForm.CadencerProgress(const deltaTime, newTime: Double);
begin
  EnginePool.Progress(deltaTime);
  Invalidate;
end;

procedure Trealtime_MMOD_Video_FMXClientForm.OD_Result(Sender: TRealTime_MMOD_VideoClient; video_stream: TMemoryStream64; video_info: TMMOD_Video_Info);
begin
  video_stream.Position := 0;
  mpeg_frame.LoadFromStream(video_stream);
  mpeg_frame.Update;
  cadencer_eng.Progress;
end;

procedure Trealtime_MMOD_Video_FMXClientForm.CheckConnect;
begin
  realtime_od_cli.AsyncConnectP('127.0.0.1', 7866, 7867, procedure(const cState: Boolean)
    begin
      if not cState then
        begin
          CheckConnect;
          exit;
        end;
      realtime_od_cli.TunnelLinkP(procedure(const lState: Boolean)
        begin
        end);
    end);
end;

procedure Trealtime_MMOD_Video_FMXClientForm.DoInput;
var
  mr: TMemoryRaster;
begin
  if not realtime_od_cli.LinkOk then
      exit;

  mr := NewRaster();
  while not mpeg_r.ReadFrame(mr, False) do
      mpeg_r.Seek(0);

  realtime_od_cli.Input_MMOD(mr);
  disposeObject(mr);
end;

procedure Trealtime_MMOD_Video_FMXClientForm.Video_RealSendTimerTimer(Sender:
    TObject);
begin
  DoInput;
end;

end.
