unit VideoTrackerFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Objects, FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.ExtCtrls,

  System.IOUtils,

  CoreClasses, zAI, zAI_Common, zDrawEngineInterface_SlowFMX, zDrawEngine, MemoryRaster, MemoryStream64,
  DoStatusIO, PascalStrings, UnicodeMixedLib, Geometry2DUnit, Geometry3DUnit, Cadencer, FFMPEG, FFMPEG_Reader;

type
  TVideoTrackerForm = class(TForm, ICadencerProgressInterface)
    Memo1: TMemo;
    PaintBox1: TPaintBox;
    Timer1: TTimer;
    Tracker_CheckBox: TCheckBox;
    TrackBar1: TTrackBar;
    ProgressBar1: TProgressBar;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure Timer1Timer(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
    procedure TrackBar1Change(Sender: TObject);
  private
    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
    procedure CadencerProgress(const deltaTime, newTime: Double);
  public
    drawIntf: TDrawEngineInterface_FMX;
    ai: TAI;
    tracker_hnd: TTracker_Handle;
    cadencer_eng: TCadencer;
    imgList: TMemoryRasterList;
    FillVideo: Boolean;
    Frame: TDETexture;

    mouse_down: Boolean;
    down_PT: TVec2;
    move_PT: TVec2;
    LastDrawRect: TRectV2;
  end;

var
  VideoTrackerForm: TVideoTrackerForm;

implementation

{$R *.fmx}


procedure TVideoTrackerForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: Integer;
begin
  DisposeObject(Frame);
  EnginePool.Clear;

  for i := 0 to imgList.Count - 1 do
      DisposeObject(imgList[i]);
  DisposeObject(imgList);

  ai.Tracker_Close(tracker_hnd);

  DisposeObject(drawIntf);
  DisposeObject(ai);
  DisposeObject(cadencer_eng);
end;

procedure TVideoTrackerForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not FillVideo;
end;

procedure TVideoTrackerForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusMethod);
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

  // ai引擎
  ai := TAI.OpenEngine();
  // 初始化追踪器
  tracker_hnd := nil;

  // cadencer引擎
  cadencer_eng := TCadencer.Create;
  cadencer_eng.ProgressInterface := Self;

  // 解码视频内容链表
  imgList := TMemoryRasterList.Create;

  FillVideo := True;

  Frame := TDrawEngine.NewTexture();

  mouse_down := False;
  down_PT := Vec2(0, 0);
  move_PT := Vec2(0, 0);

  ProgressBar1.Visible := True;
  ProgressBar1.Min := 0;

  // 使用TComputeThread后台解码
  TComputeThread.RunP(nil, nil, procedure(ThSender: TComputeThread)
    var
      // mp4视频帧格式
      M4: TFFMPEG_Reader;
      mr: TMemoryRaster;
      nr: TMemoryRaster;
    begin
      DoStatus('请等一会，正在初始化视频内容');
      M4 := TFFMPEG_Reader.Create(umlCombineFileName(TPath.GetLibraryPath, 'tracker_video.mp4'));
      TThread.Synchronize(ThSender, procedure
        begin
          ProgressBar1.Max := M4.Total_Frame;
        end);

      mr := NewRaster();
      while M4.ReadFrame(mr, False) do
        begin
          if (Frame.Width <> mr.Width) or (Frame.Height <> mr.Height) then
              TThread.Synchronize(ThSender, procedure
              begin
                Frame.Assign(mr);
                Frame.ReleaseGPUMemory;
              end);

          nr := NewRaster();
          nr.Assign(mr);
          imgList.Add(nr);

          TThread.Synchronize(ThSender, procedure
            begin
              ProgressBar1.Value := M4.Current_Frame;
            end);
        end;
      DisposeObject(mr);
      DisposeObject(M4);
      DoStatus('视频内容已经初始化完成');

      TThread.Synchronize(ThSender, procedure
        begin
          TrackBar1.Max := imgList.Count;
          TrackBar1.Min := 0;
          TrackBar1.Value := 0;
          FillVideo := False;

          ProgressBar1.Visible := False;
        end);
    end);
end;

procedure TVideoTrackerForm.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  mouse_down := True;
  down_PT := Vec2(X, Y);
end;

procedure TVideoTrackerForm.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  move_PT := Vec2(X, Y);
end;

procedure TVideoTrackerForm.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  d: TRectV2;
begin
  mouse_down := False;
  move_PT := Vec2(X, Y);

  if FillVideo then
      exit;

  ai.Tracker_Close(tracker_hnd);
  d := RectTransform(LastDrawRect, Frame.BoundsRectV2, RectV2(down_PT, move_PT));
  tracker_hnd := ai.Tracker_Open(Frame, d);
end;

procedure TVideoTrackerForm.Timer1Timer(Sender: TObject);
begin
  DoStatus();
  cadencer_eng.Progress;
end;

procedure TVideoTrackerForm.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
var
  d: TDrawEngine;
  trackerResult: Double;
  trackerBox: TRectV2;
begin
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);
  d.ViewOptions := [voFPS];
  d.FPSFontColor := DEColor(0.5, 0.5, 1, 1);
  d.FillBox(d.ScreenRect, DEColor(0, 0, 0));

  LastDrawRect := d.FitDrawPicture(Frame, Frame.BoundsRectV2, d.ScreenRect, 1.0);
  d.DrawBox(LastDrawRect, DEColor(1, 0, 0, 0.5), 1);

  if mouse_down then
    begin
      trackerBox := RectV2(down_PT, move_PT);
      d.DrawBox(trackerBox, DEColor(0, 1, 0, 1), 1);
      d.DrawCorner(TV2Rect4.Init(trackerBox), DEColor(0, 1, 0, 1), 20, 5);
    end
  else if (not FillVideo) and (tracker_hnd <> nil) and (Tracker_CheckBox.IsChecked) then
    begin
      trackerResult := ai.Tracker_Update(tracker_hnd, Frame, trackerBox);
      trackerBox := RectTransform(Frame.BoundsRectV2, LastDrawRect, trackerBox);
      d.DrawBox(trackerBox, DEColor(1.0, 0.5, 0.5), 2);

      d.BeginCaptureShadow(Vec2(2, 2), 0.9);
      d.DrawText(PFormat('%f', [trackerResult]), 12, trackerBox, DEColor(1.0, 1.0, 1.0, 1.0), True);
      d.EndCaptureShadow;
    end;

  // 执行绘图指令
  d.Flush;
end;

procedure TVideoTrackerForm.TrackBar1Change(Sender: TObject);
var
  idx: Integer;
begin
  idx := Round(TrackBar1.Value);
  if (idx >= 0) and (idx < imgList.Count) then
    begin
      Frame.Assign(imgList[idx]);
      Frame.ReleaseGPUMemory;
    end;
end;

procedure TVideoTrackerForm.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TVideoTrackerForm.CadencerProgress(const deltaTime, newTime: Double);
begin
  EnginePool.Progress(deltaTime);
  Invalidate;
end;

end.
