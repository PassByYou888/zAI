unit Face_AlignmentFrm_Parallel;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Objects, FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.ExtCtrls,

  System.IOUtils,
  System.Threading,

  CoreClasses, DoStatusIO, zAI, zAI_Common, zDrawEngineInterface_SlowFMX, zDrawEngine, MemoryRaster, MemoryStream64,
  PascalStrings, UnicodeMixedLib, Geometry2DUnit, Geometry3DUnit;

type
  TFace_DetForm = class(TForm)
    Memo1: TMemo;
    PaintBox1: TPaintBox;
    AddPicButton: TButton;
    OpenDialog: TOpenDialog;
    Timer1: TTimer;
    Scale2CheckBox: TCheckBox;
    procedure AddPicButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure PaintBox1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    lbc_Down: Boolean;
    lbc_pt: TVec2;
    procedure DoStatus_Hook_(AText: SystemString; const ID: Integer);
  public
    { Public declarations }
    drawIntf: TDrawEngineInterface_FMX;
    rList: TMemoryRasterList;
    ai_parallel: TAI_Parallel;
  end;

var
  Face_DetForm: TFace_DetForm;

implementation

{$R *.fmx}


procedure TFace_DetForm.AddPicButtonClick(Sender: TObject);
var
  i: Integer;
begin
  OpenDialog.Filter := TBitmapCodecManager.GetFilterString;
  if not OpenDialog.Execute then
      exit;

  for i := 0 to rList.Count - 1 do
      DisposeObject(rList[i]);
  rList.clear;

  TComputeThread.RunP(nil, nil, procedure(Sender: TComputeThread)
    begin
      TParallel.for(0, OpenDialog.Files.Count - 1, procedure(pass: Integer)
        var
          ai: TAI;
          mr, nmr: TMemoryRaster;
          d: TDrawEngine;
          j: Integer;
          face_hnd: TFACE_Handle;
          sp_desc: TArrayVec2;
          r: TRectV2;
        begin
          ai := ai_parallel.GetAndLockAI;
          mr := NewRasterFromFile(OpenDialog.Files[pass]);

          if Scale2CheckBox.IsChecked then
            begin
              nmr := NewRaster;
              nmr.ZoomFrom(mr, mr.width * 4, mr.height * 4);
              face_hnd := ai.Face_Detector_All(nmr, 150);
              DisposeObject(nmr);
            end
          else
            begin
              face_hnd := ai.Face_Detector_All(mr, 150);
            end;

          if face_hnd <> nil then
            begin
              d := TDrawEngine.Create;
              d.Rasterization.SetWorkMemory(mr);
              d.SetSize(mr);
              for j := 0 to ai.Face_Rect_Num(face_hnd) - 1 do
                begin
                  sp_desc := ai.Face_ShapeV2(face_hnd, j);
                  if Scale2CheckBox.IsChecked then
                      sp_desc := Vec2Mul(sp_desc, 0.25);
                  DrawFaceSP(sp_desc, DEColor(1, 0, 0, 0.5), d);

                  r := ai.Face_RectV2(face_hnd, j);
                  if Scale2CheckBox.IsChecked then
                      r := RectMul(r, 0.25);
                  d.DrawCorner(TV2Rect4.Init(r, 0), DEColor(1, 0, 0, 0.9), 30, 4);
                  d.DrawText(IntToStr(j), 20, r, DEColor(1, 1, 1, 1), False);
                end;
              d.Flush;
              DisposeObject(d);
            end;

          ai.Face_Close(face_hnd);

          LockObject(rList);
          rList.Add(mr);
          UnLockObject(rList);

          ai_parallel.UnLockAI(ai);
        end);
    end);
end;

procedure TFace_DetForm.DoStatus_Hook_(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TFace_DetForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DeleteDoStatusHook(Self);
end;

procedure TFace_DetForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHookM(Self, DoStatus_Hook_);

  // 读取zAI的配置
  ReadAIConfig;
  // 这一步会连接Key服务器，验证ZAI的Key
  // 连接服务器验证Key是在启动引擎时一次性的验证，只会当程序启动时才会验证，假如验证不能通过，zAI将会拒绝工作
  // 在程序运行中，反复创建TAI，不会发生远程验证
  // 验证需要一个userKey，通过userkey推算出ZAI在启动时生成的随机Key，userkey可以通过web申请，也可以联系作者发放
  // 验证key都是抗量子级，无法被破解
  zAI.Prepare_AI_Engine();

  DoStatus('初始化几何检测器引擎.');
  TComputeThread.RunP(nil, nil, procedure(Sender: TComputeThread)
    begin
      ai_parallel := TAI_Parallel.Create;

      // 初始化并行数等同于cpu核心数量，并且指定AI引擎
      ai_parallel.Prepare_Parallel(zAI.Prepare_AI_Engine(), cpuCount);

      // shape predictor几何检测器的训练数据非常大
      // 并行化处理需要至少1G以上内存
      ai_parallel.Prepare_Face;

      DoStatus('已经完成几何检测器引擎.');
    end);

  drawIntf := TDrawEngineInterface_FMX.Create;
  rList := TMemoryRasterList.Create;

  lbc_Down := False;
  lbc_pt := Vec2(0, 0);
end;

procedure TFace_DetForm.PaintBox1MouseDown(Sender: TObject; Button:
  TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  lbc_pt := Vec2(TControl(Sender).LocalToAbsolute(Pointf(X, Y)));
end;

procedure TFace_DetForm.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState;
X, Y: Single);
var
  abs_pt, pt: TVec2;
  d: TDrawEngine;
begin
  abs_pt := Vec2(TControl(Sender).LocalToAbsolute(Pointf(X, Y)));
  pt := Vec2Sub(abs_pt, lbc_pt);
  d := DrawPool(Sender);

  if (ssLeft in Shift) then
      d.Offset := Vec2Add(d.Offset, pt);

  lbc_pt := Vec2(TControl(Sender).LocalToAbsolute(Pointf(X, Y)));
end;

procedure TFace_DetForm.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: Single);
begin
  lbc_Down := False;
end;

procedure TFace_DetForm.PaintBox1MouseWheel(Sender: TObject; Shift:
  TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  Handled := True;
  if WheelDelta > 0 then
    begin
      with DrawPool(PaintBox1) do
          Scale := Scale + 0.05;
    end
  else
    begin
      with DrawPool(PaintBox1) do
          Scale := Scale - 0.05;
    end;
end;

procedure TFace_DetForm.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
var
  d: TDrawEngine;
begin
  // 让DrawIntf的绘图实例输出在paintbox1
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);

  // 显示边框和帧率
  d.ViewOptions := [voEdge];

  // 背景被填充成黑色，这里的画图指令并不是立即执行的，而是形成命令流队列存放在DrawEngine的一个容器中
  d.FillBox(d.ScreenRect, DEColor(0, 0, 0, 1));

  LockObject(rList);
  d.DrawPicturePackingInScene(rList, 5, Vec2(0, 0), 1.0);
  UnLockObject(rList);

  d.BeginCaptureShadow(Vec2(1, 1), 0.9);
  d.DrawText(d.LastDrawInfo + #13#10 + '按下鼠标变换坐标，滚轮可以缩放', 12, d.ScreenRect, DEColor(0.5, 1, 0.5, 1), False);
  d.EndCaptureShadow;
  d.Flush;
end;

procedure TFace_DetForm.Timer1Timer(Sender: TObject);
begin
  DoStatus;
  EnginePool.Progress(Interval2Delta(Timer1.Interval));
  Invalidate;
end;

end.
