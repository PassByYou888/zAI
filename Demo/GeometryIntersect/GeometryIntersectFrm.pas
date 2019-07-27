unit GeometryIntersectFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,

  Threading, SyncObjs,

  CoreClasses, PascalStrings, UnicodeMixedLib, Geometry2DUnit, MemoryRaster,
  MemoryStream64, DoStatusIO, zDrawEngine, zDrawEngineInterface_FMX;

type
  TGeometryIntersectForm = class(TForm)
    Timer1: TTimer;
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure Timer1Timer(Sender: TObject);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  private
  public
    dIntf: TDrawEngineInterface_FMX;
    {
      TDeflectionPolygon是一种相对坐标系的几何数据集,它并不是用绝对值来描述坐标位置
      TDeflectionPolygon每个坐标以angle+distance,角度+长度来描述坐标
      本Demo基于TDeflectionPolygon坐标系绘图时,使用了TDeflectionPolygon.Points方法将相对坐标系转换成了绝对坐标系(只有绝对坐标系才能与外部绘图api相接口,比如d2d,opengl)
      在TMemoryRaster中画文本属于常用的光栅功能,而字体的旋转,缩放,变形等等在处理,并没有使用对称矩阵,所有的文本的坐标系都使用了TDeflectionPolygon进行变换
      在图像语义分割(分割结构是几何结构),文字识别的排版(坐标系的排版输出都是几何结构)
      理解坐标系等同于理解TDeflectionPolygon几何运作思路的核心

      TVec2List里面存储的几何数据则是绝对坐标值

      在使用时我们要注意区分TVec2List和TDeflectionPolygon的坐标系,
      在多数情况下,TVec2List用以描绘几何形的场景,图像坐标值
      TDeflectionPolygon则用以描绘随时变化的场景内容和图像内容
    }
    geoBuff: TGenericsList<TDeflectionPolygon>;
    ColorBuff: TGenericsList<TPolyDrawOption>;

    constructor Create(AOwner: TComponent); override;
    procedure compute_LineInt(pt1, pt2: TVec2; poly: TDeflectionPolygon; output: TVec2List);
    function ComputeIntersectVec: TVec2List;
    procedure RebuildGeoBuff;
  end;

var
  GeometryIntersectForm: TGeometryIntersectForm;

implementation

{$R *.fmx}


procedure TGeometryIntersectForm.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  d: TDrawEngine;
  i: Integer;
  intersectVecList: TVec2List;
begin
  dIntf.SetSurface(Canvas, Self);
  d := DrawPool(Self, dIntf);

  d.DrawOptions := [voFPS, voEdge];
  d.FillBox(d.ScreenRect, DEColor(0, 0, 0, 1));

  // 画poly几何形
  for i := 0 to geoBuff.Count - 1 do
      d.DrawPolyInScene(geoBuff[i], True, ColorBuff[i]);

  // 实时计算交替几何形相交
  intersectVecList := ComputeIntersectVec();

  // 把相交坐标画在屏幕上
  for i := 0 to intersectVecList.Count - 1 do
      d.DrawPoint(intersectVecList[i]^, DEColor(1.0, 1.0, 1.0, 1.0), 5, 1);

  DisposeObject(intersectVecList);

  d.Flush;
end;

procedure TGeometryIntersectForm.Timer1Timer(Sender: TObject);
var
  d: Double;
  i: Integer;
begin
  // 将internal整型时间转换成秒单位的浮点时间
  // 这种转换是逻辑转换,浮点时间是固定的,并不是物理时间
  // 如果要使用物理时间计算delta,需要引用cadencer的api接口,请参考使用了cadencer的api相关demo
  d := Interval2Delta(Timer1.Interval);
  // 绘图引擎主循环
  EnginePool.Progress(d);

  // 让poly几何形转动
  // poly使用的是相对坐标系,我么直接更改poly的angle,等同于TVec2List重构了一次几何,在Poly坐标系中,是0开销实现几何变形,因为poly使用相对坐标系
  // 以每秒45度的速度进行旋转计算,然后归一化半圆角
  // 归一概念在degAngle是将180度作为一个归一条件进行处理,越过180以-0..-180度开始计算,重叠起来等同于360度
  for i := 0 to geoBuff.Count - 1 do
      geoBuff[i].Angle := NormalizeDegAngle(geoBuff[i].Angle + 45 * d);

  Invalidate;
end;

procedure TGeometryIntersectForm.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  // 按下鼠标任意键后,重建几何结构
  RebuildGeoBuff;
end;

constructor TGeometryIntersectForm.Create(AOwner: TComponent);
begin
  inherited;
  dIntf := TDrawEngineInterface_FMX.Create;
  geoBuff := TGenericsList<TDeflectionPolygon>.Create;
  ColorBuff := TGenericsList<TPolyDrawOption>.Create;
  RebuildGeoBuff;
end;

procedure TGeometryIntersectForm.compute_LineInt(pt1, pt2: TVec2; poly: TDeflectionPolygon; output: TVec2List);
var
  i: Integer;
  dpt1, dpt2: TVec2;
  v: TVec2;
begin
  // 相交点技术非常块,以每秒nM/s输入Line数量为单位
  // 本Demo对相交计算的cpu开销几乎为0
  dpt1 := poly.Points[0];
  for i := 1 to poly.Count - 1 do
    begin
      // poly.Points是将poly的相对坐标系以绝对坐标取出来
      // poly中还有Expands方法,表示凸对称,常用于表示几何空间的计算处理
      // 在启发式几何导航算法中,expands被用于构建多边形的凹凸核心空间,这是导航网络的核心算法地基
      dpt2 := poly.Points[i];
      // Intersect是个原子api,计算两条以绝地坐标描绘的线条的相交点
      // 在Poly中有现成的相交计算api,我们在这里特地使用原子api来计算是为了更好说明相交计算的正确做法
      if Intersect(pt1, pt2, dpt1, dpt2, v) then
          output.Add(v);
      dpt1 := dpt2;
    end;
  // 闭合线规则
  // 在结尾时,让程序逻辑处理一下线条的闭合部分
  dpt2 := poly.Points[0];
  // Intersect是个原子api,计算两条以绝地坐标描绘的线条的相交点
  // 在Poly中有现成的相交计算api,我们在这里特地使用原子api来计算是为了更好说明相交计算的正确做法
  if Intersect(pt1, pt2, dpt1, dpt2, v) then
      output.Add(v);
end;

function TGeometryIntersectForm.ComputeIntersectVec: TVec2List;
var
  tmp: TGenericsList<TDeflectionPolygon>;
  poly: TDeflectionPolygon;
  i, j: Integer;
  pt1, pt2: TVec2;
begin
  // 高速遍历GeoBuff所有几何型相交点范式
  Result := TVec2List.Create;
  tmp := TGenericsList<TDeflectionPolygon>.Create;

  // 先创建一个链表容器
  for i := 0 to geoBuff.Count - 1 do
      tmp.Add(geoBuff[i]);

  // 在链表容器使用对称排除法遍历
  while tmp.Count > 0 do
    begin
      poly := tmp.First;
      for i := 1 to tmp.Count - 1 do
        begin
          pt1 := poly.Points[0];
          for j := 1 to poly.Count - 1 do
            begin
              pt2 := poly.Points[j];
              compute_LineInt(pt1, pt2, tmp[i], Result);
              pt1 := pt2;
            end;
          pt2 := poly.Points[0];
          compute_LineInt(pt1, pt2, tmp[i], Result);
        end;
      tmp.Delete(0);
    end;
  DisposeObject(tmp);
end;

procedure TGeometryIntersectForm.RebuildGeoBuff;
const
  edge = 50;
var
  i, j: Integer;
  vl: TVec2List;
  v: TVec2;
  poly: TDeflectionPolygon;
  pdo: TPolyDrawOption;
begin
  for i := 0 to geoBuff.Count - 1 do
      DisposeObject(geoBuff[i]);
  geoBuff.Clear;
  ColorBuff.Clear;

  // 构建随机几何形
  for j := 1 to 10 do
    begin
      vl := TVec2List.Create;
      for i := 1 to 20 do
          vl.Add(umlRandomRange(edge, round(width) - edge), umlRandomRange(edge, round(height) - edge));

      // 把复杂几何转换成凸包,方便查看
      vl.ConvexHull();

      poly := TDeflectionPolygon.Create;
      poly.RebuildPoly(vl);
      geoBuff.Add(poly);

      // 几何颜色,线条宽等等绘图参数
      pdo.LineWidth := 2;
      pdo.PointScreenRadius := 3;
      pdo.LineColor := DEColor(umlRandomRange(1, 9) * 0.1, umlRandomRange(1, 9) * 0.1, umlRandomRange(1, 9) * 0.1, 0.5);
      pdo.PointColor := DEColor(0.5, 0.5, 0.5, 0.5);
      ColorBuff.Add(pdo);

      DisposeObject(vl);
    end;

  // 构建线条
  for j := 1 to 15 do
    begin
      vl := TVec2List.Create;
      for i := 1 to 2 do
          vl.Add(umlRandomRange(edge, round(width) - edge), umlRandomRange(edge, round(height) - edge));

      poly := TDeflectionPolygon.Create;
      poly.RebuildPoly(vl);
      geoBuff.Add(poly);

      // 几何颜色,线条宽等等绘图参数
      pdo.LineWidth := 3;
      pdo.PointScreenRadius := 3;
      pdo.LineColor := DEColor(umlRandomRange(1, 9) * 0.1, umlRandomRange(1, 9) * 0.1, umlRandomRange(1, 9) * 0.1, 1);
      pdo.PointColor := DEColor(0.5, 0.5, 0.5, 0.5);
      ColorBuff.Add(pdo);

      DisposeObject(vl);
    end;
end;

end.
