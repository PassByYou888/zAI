unit SPDemoFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Objects, FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.ExtCtrls,

  System.IOUtils,

  CoreClasses, PascalStrings, DoStatusIO,
  zAI, zAI_Common, zDrawEngineInterface_SlowFMX, zDrawEngine, MemoryRaster, MemoryStream64,
  UnicodeMixedLib, Geometry2DUnit, Geometry3DUnit;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    PaintBox1: TPaintBox;
    detSPButton: TButton;
    Timer1: TTimer;
    ProjButton: TButton;
    LowProjButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
    procedure detSPButtonClick(Sender: TObject);
    procedure ProjButtonClick(Sender: TObject);
    procedure LowProjButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
  public
    { Public declarations }
    drawIntf: TDrawEngineInterface_FMX;
    source, dest: TMemoryRaster;
    dest_od: TOD_Desc;
    dest_sp: array of TArrayVec2;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


uses ShowImageFrm;

procedure TForm1.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TForm1.FormCreate(Sender: TObject);
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

  source := NewRasterFromFile(umlCombineFileName(TPath.GetLibraryPath, 'bear_sp.jpg'));
  dest := NewRaster();
  // dest.SetSize(source.width, source.height, RasterColorF(0, 0, 0, 1));
  dest.Assign(source);
  SetLength(dest_od, 0);
  SetLength(dest_sp, 0);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  EnginePool.Progress(Interval2Delta(Timer1.Interval));
  Invalidate;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
var
  d: TDrawEngine;
  sr, dr, r: TRectV2;
  t_r: TRectV2;
  od_r: TOD_Rect;
  arryV2: TArrayVec2;
  i: Integer;
  v2, t_v2: TVec2;
  vl: TVec2List;
  alpha: TGeoFloat;
  alpha_bound: Boolean;
begin
  // 我们在PaintBox画sp的状态

  // 让DrawIntf的绘图实例输出在paintbox1
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);

  // 一些动态变量
  alpha := 1.0;
  alpha_bound := True;
  alpha := d.UserVariants.GetDefaultValue('alpha', alpha);
  alpha_bound := d.UserVariants.GetDefaultValue('alpha_bound', alpha_bound);
  alpha := BounceFloat(alpha, d.LastDeltaTime * 5, 1.0, 0.5, alpha_bound);

  // 显示边框和帧率
  d.ViewOptions := [voFPS, voEdge];

  // 背景被填充成黑色，这里的画图指令并不是立即执行的，而是形成命令流队列存放在DrawEngine的一个容器中
  d.FillBox(d.ScreenRect, DEColor(0, 0, 0, 1));

  // 将绘图背景分割成两个，一边是原图像，一边是目标图像

  // 画原，画图指令并不是立即执行的，而是形成命令流队列存放在DrawEngine的一个容器中
  sr := d.ScreenRect;
  sr[1, 1] := d.height * 0.5;
  r := d.FitDrawPicture(source, source.BoundsRectV2, sr, 1.0); // 画出来
  d.BeginCaptureShadow(Vec2(1, 1), 0.9);                       // 开始捕获画文字的影子
  d.DrawText('原图像', 12, r, DEColor(1, 0, 0, 1), False);        // 画字
  d.EndCaptureShadow;                                          // 结束捕获画文字的影子

  // 画目标，这里的画图指令并不是立即执行的，而是形成命令流队列存放在DrawEngine的一个容器中
  dr := d.ScreenRect;
  dr[0, 1] := d.height * 0.5;
  r := d.FitDrawPicture(dest, dest.BoundsRectV2, dr, 1.0);   // 画出来,返回的r是画图fit后的框体，r是纹理的实际屏幕框体的坐标系
  d.BeginCaptureShadow(Vec2(1, 1), 0.9);                     // 开始捕获画文字的影子
  d.DrawText('检测后的图像', 12, r, DEColor(1, 0, 0, 1.0), False); // 画字
  d.EndCaptureShadow;                                        // 结束捕获画文字的影子

  // 画目标的检测状态
  for od_r in dest_od do
    begin
      t_r := RectTransformToDest(dest.BoundsRectV2, r, rectV2(od_r)); // 变换方框的坐标系，变换规律类似投影，但是比投影更简单，没有旋转变换，只有缩放的平移变换

      // 画图指令并不是立即执行的，而是形成命令流队列存放在DrawEngine的一个容器中
      d.DrawCorner(TV2Rect4.Init(t_r, 0), DEColor(1, 0, 1, 1), 40, 2); // 把检测器的包围框画出来
    end;

  // 画目标的sp状态
  // 在一些变脸app中，它们利用了sp技术覆盖画图
  for arryV2 in dest_sp do
    begin
      vl := TVec2List.Create;
      for v2 in arryV2 do
        begin
          t_v2 := Vec2TransformToDest(dest.BoundsRectV2, r, v2); // 变换点的坐标系，变换规律类似投影，但是比投影更简单，没有旋转变换，只有缩放的平移变换
          vl.Add(t_v2);

          // 画图指令并不是立即执行的，而是形成命令流队列存放在DrawEngine的一个容器中
          // d.DrawEllipse(t_v2, 5, DEColor(1, 0, 1, alpha));  // 画圆
          d.DrawPoint(t_v2, DEColor(0, 0, 1, alpha), 5, 2); // 画标记
        end;

      d.BeginCaptureShadow(Vec2(1, 1), 0.9);
      d.DrawText('左耳', 11, DEColor(1, 0, 1, alpha), vl[0]^);
      d.DrawText('右耳', 11, DEColor(1, 0, 1, alpha), vl[1]^);
      d.DrawText('左脸', 11, DEColor(1, 0, 1, alpha), vl[2]^);
      d.DrawText('右脸', 11, DEColor(1, 0, 1, alpha), vl[3]^);
      d.DrawText('鼻子', 11, DEColor(1, 0, 1, alpha), vl[4]^);
      d.EndCaptureShadow;

      // 凸包
      vl.ConvexHull();
      // 用spline闭合线圈住熊本熊五官
      d.DrawPL(20, vl, True, DEColor(0, 0, 1, 0.3), 2);

      disposeObject(vl);
    end;

  // 执行绘图指令
  d.Flush;

  d.UserVariants.SetDefaultValue('alpha', alpha);
  d.UserVariants.SetDefaultValue('alpha_bound', alpha_bound);
end;

procedure TForm1.detSPButtonClick(Sender: TObject);
var
  ai: TAI;

  // 方框化的检测器句柄
  od_hnd: TOD_Handle;

  // 坐标化的预测器句柄
  sp_hnd: TSP_Handle;

  od_r: TOD_Rect;
  i: Integer;
begin
  ai := TAI.OpenEngine();

  // SP需要在一个方框中才能检测出有规律的散布坐标，因为全图检查，像素太多，误判率太高，因此sp的工作机制设计都需要一个框体作为检测区域的计算参考

  // 读熊本熊的od取数据集
  od_hnd := ai.OD_Open_Stream(umlCombineFileName(TPath.GetLibraryPath, 'bear.svm_od'));
  // 读熊本熊的sp取数据集
  sp_hnd := ai.SP_Open_Stream(umlCombineFileName(TPath.GetLibraryPath, 'bear.shape'));

  // 第一步，我们需要先对熊本熊做一个框体检测
  // 第二步，当我们找到的熊本熊的框体以后，在框体中，才能做散布坐标检测

  // 在source图像，做最大10个框体检测
  // 返回框体的动态数组
  dest_od := ai.OD_Process(od_hnd, source, 10);
  DrawPool(PaintBox1).PostScrollText(5, PFormat('检测到 %d 个OD框', [length(dest_od)]), 12, DEColor(1, 0, 0, 1));
  SetLength(dest_sp, length(dest_od));
  for i := low(dest_od) to high(dest_od) do
    begin
      // od_r是当前遍历中的框体检测结果
      // 这时候，我们直接在od_r框体内做sp检测
      od_r := dest_od[i];
      dest_sp[i] := ai.SP_Process_Vec2(sp_hnd, source, od_r);
    end;

  ai.SP_Close(sp_hnd);
  ai.OD_Close(od_hnd);
  disposeObject(ai);
end;

procedure TForm1.ProjButtonClick(Sender: TObject);
var
  arryV2: TArrayVec2;
  t_v2: TVec2;
  l_Ear, r_Ear, l_face, r_face: TVec2;
  mr: TMemoryRaster;
  proj_s, proj_d: TV2Rect4;
begin
  // sp的投影等同于对齐，在实际应用中，不必拘泥于必须是框体投影

  // 我们知道了熊本熊的两个耳朵坐标(0,1)
  // 我们知道了熊本熊的两个圆圈红脸坐标(2,3)
  // 我们知道了熊本熊的鼻尖坐标(4)，我们不用这个坐标
  // 这时候，我们有了4个坐标，它是一个不对等框体
  // 在Geometry2DUnit中有一个，TV2Rect4，这是用4个Vec2坐标描述Rect的数据结构
  // 在MemoryRaster中有一个，Projection方法，这用TV2Rect4对等投影的方法

  for arryV2 in dest_sp do
    begin
      l_Ear := arryV2[0];  // 左耳
      r_Ear := arryV2[1];  // 右耳
      l_face := arryV2[2]; // 左脸
      r_face := arryV2[3]; // 右脸

      mr := NewRaster();
      mr.SetSize(400, 300, RasterColorF(0,0,0,1));

      // 投影发射坐标系
      proj_s.LeftTop := l_Ear;
      proj_s.RightTop := r_Ear;
      proj_s.LeftBottom := l_face;
      proj_s.RightBottom := r_face;

      // 投影目标坐标系
      proj_d := TV2Rect4.Init(mr.BoundsRectV2, 0);

      // 投影
      // 人脸对齐的原理就是投影，人脸要比这里的计算投影的方法复杂
      // 人脸拥有68个sp坐标，用的是拟合计算最后的投影坐标系
      // 这里的投影坐标系只有5个，demo出原理以后，可以自己去开发sp的对齐系统
      // 2次三角框体投影
      dest.ProjectionTo(mr, proj_s, proj_d, True, 1.0);

      ShowImage(mr);
      disposeObject(mr);
    end;
end;

procedure TForm1.LowProjButtonClick(Sender: TObject);
var
  arryV2: TArrayVec2;
  t_v2: TVec2;
  l_Ear, r_Ear, l_face, r_face: TVec2;
  mr: TMemoryRaster;
  proj_s, proj_d: TVec2List;
  nose_t: TVec2;
begin
  // 这里是演示使用重心坐标来对齐投影
  // 我们知道了熊本熊的两个耳朵坐标(0,1)
  // 我们知道了熊本熊的两个圆圈红脸坐标(2,3)
  // 我们知道了熊本熊的鼻尖坐标(4)，我们不用这个坐标
  // MemoryRaster内核投影模型都是三角形填充技术(带模板三角填充尖端技术，像素不会错位)
  // 当我们的应用需求超出了框体范围时，我们可以使用定制化的三角形组合投影成不同的形状以达到对齐的要求

  for arryV2 in dest_sp do
    begin
      l_Ear := arryV2[0];  // 左耳
      r_Ear := arryV2[1];  // 右耳
      l_face := arryV2[2]; // 左脸
      r_face := arryV2[3]; // 右脸

      mr := NewRaster();
      mr.SetSize(400, 300, RasterColorF(0,0,0,1));

      // 投影发射坐标系
      proj_s := TVec2List.Create;
      proj_s.Add(l_Ear);
      proj_s.Add(r_Ear);
      proj_s.Add(r_face);
      proj_s.Add(l_face);

      // 投影目标坐标系
      proj_d := TVec2List.Create;
      proj_d.Add(Vec2(0, 0));
      proj_d.Add(Vec2(mr.width, 0));
      proj_d.Add(Vec2(mr.width, mr.height));
      proj_d.Add(Vec2(0, mr.height));

      // 以重心平展三角
      // 4次三角框体投影
      mr.Vertex.FillPoly(proj_s, proj_d, dest, True, 1.0);

      ShowImage(mr);
      disposeObject(mr);
    end;
end;

end.
