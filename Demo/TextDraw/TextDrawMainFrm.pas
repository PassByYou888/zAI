unit TextDrawMainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  CoreClasses, PascalStrings, zDrawEngine, zDrawEngineInterface_SlowFMX, Geometry2DUnit;

type
  TTextDrawMainForm = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    drawIntf: TDrawEngineInterface_FMX;
    angle: TDEFloat;
  end;

var
  TextDrawMainForm: TTextDrawMainForm;

implementation

{$R *.fmx}


procedure TTextDrawMainForm.FormCreate(Sender: TObject);
begin
  drawIntf := TDrawEngineInterface_FMX.Create;
  angle := 0;
end;

procedure TTextDrawMainForm.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  d: TDrawEngine;
  n: string;
begin
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);
  // voFPS:显示每秒帧率
  // voEdge:显示边界
  // voTextBox:显示文本框
  d.ViewOptions := [voFPS, voEdge { , voTextBox } ];
  d.FillBox(d.ScreenRect, DEColor(0.5, 0.5, 0.5));

  // 文本表达式是一种脚本，语法为 |脚本|文本，脚本必须以|开头并且以|结束，当脚本结束后就是文本内容
  // size:xx画出来的尺寸，同样也可以写成|s:xx|
  // color(r,g,b,a)，颜色
  // |color(1,0,0,1)|xx，画红色文字xx，也可以写成|red:1|xx
  // |color(255,0,0,255)|xx，画红色文字xx，如果当参数数值大于1.0用byte赋值
  // |s:11|，表示使用11号size，color会自动使用调用drawText的常量
  // |color(1,1,1,1)|,表示画成白色的文字，由于脚本内没有给出而文字的字号，使用调用drawText的常量
  // || 空脚本表示使用默认文本size+color画
  n := '|size:20|20字号的大字|s:10|10字号的小字' + #13#10 +
    '|color(1,0,0,1),size:15|15字号的红色字|| 默认文字 |color(0,1,0,1),size:32|特大字体';

  // 归一处理:防止angle系数在动画计算时变得过大丢失精度
  angle := NormalizeDegAngle(angle + 15 * d.LastDeltaTime);
  // angle := angle + 15 * d.LastDeltaTime;

  // 开始捕获影子,Vec2(2,5)是影子的偏移量
  // BeginCaptureShadow只对文字和图片有效，对shape图形无效
  d.BeginCaptureShadow(vec2(2, 5), 0.9);

  // 文本表达式在绘制时会使用zExpression的cache机制，高频率绘制不会反复编译表达式
  d.DrawText(
    n,                   // 文本
    12,                  // 文本size
    d.ScreenRect,        // 画文字的框体
    DEColor(1, 1, 1, 1), // 文字颜色
    True,                // 文字是否居中
    vec2(0.5, 0.5),      // 旋转轴心，尺度坐标系，vec2(0.5,0.5)表示中央，vec2(0,0)表示左上，vec2(1,1)表示右下
    angle                // 旋转角度
    );

  // 结束影子捕获
  d.EndCaptureShadow;

  // 上面的Fillbox,DrawText这些命令在调用时并不会立即绘制而是存放到一种command list队列
  // flush方法是将command list队列的命令立即画出来
  // command list是一种性能优化机制，它通常被放在一个独立线程，而flush则放在一个另一个线程，这类机制常用于高级性能优化
  d.Flush;
end;

procedure TTextDrawMainForm.Timer1Timer(Sender: TObject);
begin
  EnginePool.Progress(Interval2Delta(TTimer(Sender).Interval));
  Invalidate;
end;

end.
