unit VideoTrackerFrm_CPU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Objects, FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.ExtCtrls,

  System.IOUtils,

  CoreClasses, zAI, zAI_Common, zDrawEngineInterface_SlowFMX, zDrawEngine, MemoryRaster, MemoryStream64,
  PascalStrings, UnicodeMixedLib, Geometry2DUnit, Geometry3DUnit, Cadencer, Y4M, h264Image;

type
  TForm1 = class(TForm, ICadencerProgressInterface)
    Memo1: TMemo;
    PaintBox1: TPaintBox;
    Timer1: TTimer;
    Tracker_CheckBox: TCheckBox;
    HistogramEqualizeCheckBox: TCheckBox;
    AntialiasCheckBox: TCheckBox;
    SepiaCheckBox: TCheckBox;
    SharpenCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
  private
    procedure CadencerProgress(const deltaTime, newTime: Double);
  public
    drawIntf: TDrawEngineInterface_FMX;
    mpeg_y4m: TY4MReader;
    frame: TDETexture;
    cadencer_eng: TCadencer;
    ai: TAI;
    od_hnd: TOD_Handle;
    tracker_hnd: TTracker_Handle;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


procedure TForm1.FormCreate(Sender: TObject);
begin
  // 读取zAI的配置
  ReadAIConfig;

  // 这一步会连接Key服务器，验证ZAI的Key
  // 连接服务器验证Key是在启动引擎时一次性的验证，只会当程序启动时才会验证，假如验证不能通过，zAI将会拒绝工作
  // 在程序运行中，反复创建TAI，不会发生远程验证
  // 验证需要一个userKey，通过userkey推算出ZAI在启动时生成的随机Key，userkey可以通过web申请，也可以联系作者发放
  // 验证key都是抗量子级，无法被破解
  zAI.Prepare_AI_Engine();

  // 使用zDrawEngine做外部绘图时(比如游戏，面向paintbox)，都需要一个绘图接口
  // TDrawEngineInterface_FMX是面向FMX的绘图core接口
  // 如果不指定绘图接口，zDrawEngine会默认使用软件光栅绘图(比较慢)
  drawIntf := TDrawEngineInterface_FMX.Create;

  // mpeg yv12视频帧格式
  mpeg_y4m := TY4MReader.Create(umlCombineFileName(TPath.GetLibraryPath, 'dog.y4m'));

  // 当前绘制的视频帧
  frame := TDrawEngine.NewTexture;

  // cadencer引擎
  cadencer_eng := TCadencer.Create;
  cadencer_eng.ProgressInterface := Self;

  // ai引擎
  ai := TAI.OpenEngine();

  // 加载svm-od的检测器(cpu对象检测器)
  od_hnd := ai.OD_Open_Stream(umlCombineFileName(TPath.GetLibraryPath, 'dog_video.svm_od'));

  // 初始化追踪器
  tracker_hnd := nil;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  cadencer_eng.Progress;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
  procedure Raster_DetectAndDraw(mr: TMemoryRaster);
  var
    d: TDrawEngine;
    od_desc: TOD_Desc;
    tracker_r: TRectV2;
  begin
    // 使用dnn-od来检测小狗
    // 这里的参数含义是：最大只检测2个目标对象
    od_desc := ai.OD_Process(od_hnd, mr, 2);

    d := TDrawEngine.Create;
    d.ViewOptions := [];

    // drawEngine的输出方式是直接内存映射
    // 这种方式是0像素copy，直接写入到mr的bit内存
    // 我们处理ffmpeg视频流都可以使用drawengine实现立即绘图，因为它不会做任何多余的像素copy
    d.Rasterization.SetWorkMemory(mr);
    d.SetSize(mr);

    // 判断是否检测到小狗
    if length(od_desc) = 0 then
      begin
        if Tracker_CheckBox.IsChecked then
          if tracker_hnd <> nil then
            begin
              // 如果od没有检测到小狗，并且我们确定追踪器是开启的，开始追踪上一个od成功的框体
              ai.Tracker_Update(tracker_hnd, mr, tracker_r);
              // 把tracker追踪器的框体以粉红色画出来
              d.DrawCorner(TV2Rect4.Init(tracker_r, 45), DEColor(1, 0.5, 0.5, 1), 20, 3);
            end;
      end
    else
      begin
        // 如果OD检测出了小狗
        // 我们重开一个追踪器
        // Tracker也是对一个框体进行学习，不过它不像od那样会做很多收敛处理，tracker几乎是实时学习的
        ai.Tracker_Close(tracker_hnd);

        if Tracker_CheckBox.IsChecked then
          begin
            ai.Tracker_Close(tracker_hnd);
            tracker_hnd := ai.Tracker_Open(mr, RectV2(od_desc[0]));
            tracker_r := RectV2(od_desc[0]);
            // 把tracker追踪器的框体以粉红色画出来
            d.DrawCorner(TV2Rect4.Init(tracker_r, 45), DEColor(1, 0.5, 0.5, 1), 20, 3);
          end;

        // 把OD的框体以蓝色画出来
        d.DrawCorner(TV2Rect4.Init(RectV2(od_desc[0]), 0), DEColor(0.5, 0.5, 1, 1), 20, 2);
      end;

    // 执行绘图流指令
    d.Flush;
    disposeObject(d);

    // 这里演示了对视频输出做后期处理的部分方法

    // Sepia是非常漂亮的色彩系，常用于美工风格定义
    if SepiaCheckBox.IsChecked then
        Sepia32(mr, 12);

    // 使用色彩直方图修复yv12丢失的色彩
    // 让图像输出看起来更有电视感觉
    if HistogramEqualizeCheckBox.IsChecked then
        HistogramEqualize(mr);

    // 反锯齿
    if AntialiasCheckBox.IsChecked then
        Antialias32(mr, 1);

    // 锐化
    if SharpenCheckBox.IsChecked then
        Sharpen(mr, False);
  end;

var
  d: TDrawEngine;
begin
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);
  d.ViewOptions := [devpFPS];
  d.FPSFontColor := DEColor(0.5, 0.5, 1, 1);

  mpeg_y4m.ReadFrame();
  YV12ToRaster(mpeg_y4m.Image, frame);
  Raster_DetectAndDraw(frame);
  frame.FastUpdateTexture;

  d.DrawTexture(frame, frame.BoundsRectV2, d.ScreenRect, 1.0);

  if mpeg_y4m.CurrentFrame >= mpeg_y4m.FrameCount then
    begin
      mpeg_y4m.SeekFirstFrame;
      d.LastNewTime := 0;
      ai.Tracker_Close(tracker_hnd);
    end;

  // 执行绘图指令
  d.Flush;
end;

procedure TForm1.CadencerProgress(const deltaTime, newTime: Double);
begin
  EnginePool.Progress(deltaTime);
  Invalidate;
end;

end.
