unit surfFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Objects, FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.ExtCtrls,

  System.IOUtils,

  CoreClasses, DoStatusIO, zAI, zAI_Common, zDrawEngineInterface_SlowFMX, zDrawEngine, MemoryRaster, MemoryStream64,
  PascalStrings, UnicodeMixedLib, Geometry2DUnit, Geometry3DUnit, Cadencer, Y4M, h264Image;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure DoStatus_Hook_(AText: SystemString; const ID: Integer);
  public
    drawIntf: TDrawEngineInterface_FMX;
    surf_out: TMemoryRaster;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


procedure TForm1.DoStatus_Hook_(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  ai: TAI;
  r1, r2: TMemoryRaster;
  d1, d2: TSurf_DescBuffer;
  matched: TSurfMatchedBuffer;
  tk: TTimeTick;
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

  // 使用zDrawEngine做外部绘图时(比如游戏，面向paintbox)，都需要一个绘图接口
  // TDrawEngineInterface_FMX是面向FMX的绘图core接口
  // 如果不指定绘图接口，zDrawEngine会默认使用软件光栅绘图(比较慢)
  drawIntf := TDrawEngineInterface_FMX.Create;

  ai := TAI.OpenEngine();

  // 读取图片
  r1 := NewRasterFromFile(umlCombineFileName(AI_Configure_Path, 'surf_1.bmp'));
  r2 := NewRasterFromFile(umlCombineFileName(AI_Configure_Path, 'surf_2.bmp'));

  // 使用surf比对图片，并且输出到surf_out
  tk := GetTimeTick();
  d1 := ai.fast_surf(r1, 20000, 10.0);
  DoStatus('分析 surf_1.bmp 耗时:%dms', [GetTimeTick() - tk]);

  tk := GetTimeTick();
  d2 := ai.fast_surf(r2, 20000, 10.0);
  DoStatus('分析 surf_1.bmp 耗时:%dms', [GetTimeTick() - tk]);

  tk := GetTimeTick();
  ai.BuildFeatureView(r1, d1);
  ai.BuildFeatureView(r2, d2);
  DoStatus('图形构建耗时:%dms', [GetTimeTick() - tk]);

  tk := GetTimeTick();
  matched := ai.Surf_Matched(0.4, r1, r2, d1, d2);
  DoStatus('surf特征匹配耗时:%dms', [GetTimeTick() - tk]);
  surf_out := ai.BuildMatchInfoView(matched);

  disposeObject(ai);
  disposeObject([r1, r2]);
end;

procedure TForm1.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  d: TDrawEngine;
begin
  // 让DrawIntf的绘图实例输出在paintbox1
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);

  // 显示边框和帧率
  d.ViewOptions := [voFPS, voEdge];

  // 背景被填充成黑色，这里的画图指令并不是立即执行的，而是形成命令流队列存放在DrawEngine的一个容器中
  d.FillBox(d.ScreenRect, DEColor(0, 0, 0, 1));

  d.FitDrawPicture(surf_out, surf_out.BoundsRectV2, d.ScreenRect, 1.0);
  d.Flush;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  EnginePool.Progress(Interval2Delta(Timer1.Interval));
  Invalidate;
end;

end.
