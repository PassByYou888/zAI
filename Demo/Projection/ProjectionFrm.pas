unit ProjectionFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Objects,

  System.IOUtils,

  CoreClasses, PascalStrings, UnicodeMixedLib,
  Geometry2DUnit,
  zDrawEngine, MemoryRaster, zDrawEngineInterface_SlowFMX, FMX.Layouts;

type
  TForm1 = class(TForm)
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    s_LeftTop: TCircle;
    s_RightTop: TCircle;
    s_LeftBottom: TCircle;
    s_RightBottom: TCircle;
    d_RightTop: TCircle;
    d_LeftTop: TCircle;
    d_LeftBottom: TCircle;
    d_RightBottom: TCircle;
    s_Image: TImage;
    d_Image: TImage;
    procedure FormCreate(Sender: TObject);
    procedure cMouseDown(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Single);
    procedure cMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure cMouseUp(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Single);
    procedure cPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
  private
    { Private declarations }
  public
    { Public declarations }
    ori, sour, dest: TMemoryRaster;

    downObj: TCircle;
    lastPt: TPointf;

    procedure RunProj;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


procedure TForm1.FormCreate(Sender: TObject);
begin
  ori := NewRasterFromFile(umlCombineFileName(TPath.GetLibraryPath, 'person_face (6).jpg'));

  sour := NewRaster();
  sour.Assign(ori);

  s_Image.SetBounds(0, 0, sour.Width, sour.Height);
  MemoryBitmapToBitmap(sour, s_Image.Bitmap);

  dest := NewRaster();
  dest.SetSize(round(d_Image.Width), round(d_Image.Height), RasterColor(0, 0, 0));
  MemoryBitmapToBitmap(dest, d_Image.Bitmap);

  downObj := nil;
  lastPt := Pointf(0, 0);
  RunProj();
end;

procedure TForm1.RunProj;
var
  ps, pd: TV2Rect4;
begin
  ps.LeftTop := Vec2(s_LeftTop.Position.Point);
  ps.RightTop := Vec2(s_RightTop.Position.Point);
  ps.RightBottom := Vec2(s_RightBottom.Position.Point);
  ps.LeftBottom := Vec2(s_LeftBottom.Position.Point);

  pd.LeftTop := Vec2(d_LeftTop.Position.Point);
  pd.RightTop := Vec2(d_RightTop.Position.Point);
  pd.RightBottom := Vec2(d_RightBottom.Position.Point);
  pd.LeftBottom := Vec2(d_LeftBottom.Position.Point);

  sour.Assign(ori);

  dest.SetSize(round(d_Image.Width), round(d_Image.Height), RasterColor(0, 0, 0));

  // ProjectionTo是基于框体投影的原子方法(更底层的是三角vertex)
  // 原理：将4顶点的原，投影到目标4顶点的目标
  // MemoryRaster投影使用了4个顶点来描述不规则框体(TV2Rect4)，TRect是两个顶点描述等边框体
  // TV2Rect4拥有几何缩放，几何旋转，几何平移，包围边延伸等等基础功能，而MemoryRaster拥有像素混合功能，两者结合就是2d投影解决方案
  // MemoryRaster投影使用双三角拼接成不规则框体，属于不多见的三角微分投影，不同于我们常用的是对称矩阵投影，三角投影是复杂几何投影的地基支持
  // 在zAI的图像处理中，投影被大量应用于对齐，快照，尺度空间，甚至在MemoryRaster画字都是投影
  // 投影的像素都是按alpha混合叠加的，不是覆盖
  // 请区分开投影和Draw的概念，投影是输入输出，draw是单一的输出
  sour.ProjectionTo(dest, ps, pd, True, 1.0);

  MemoryBitmapToBitmap(dest, d_Image.Bitmap);

  sour.Agg.LineWidth := 5;
  sour.DrawRect(ps, RasterColorF(1, 0.5, 0.5));
  MemoryBitmapToBitmap(sour, s_Image.Bitmap);
end;

procedure TForm1.cMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  downObj := TCircle(Sender);
  lastPt := TControl(downObj.Parent).AbsoluteToLocal(downObj.LocalToAbsolute(Pointf(X, Y)));
end;

procedure TForm1.cMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
  pt: TPointf;
begin
  if downObj <> Sender then
      exit;

  pt := TControl(downObj.Parent).AbsoluteToLocal(downObj.LocalToAbsolute(Pointf(X, Y)));

  downObj.Position.Point := downObj.Position.Point + (pt - lastPt);
  lastPt := pt;
end;

procedure TForm1.cMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  downObj := nil;
  RunProj();
end;

procedure TForm1.cPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  n: U_String;
begin
  Canvas.Fill.Color := TAlphaColorRec.Red;
  n := TComponent(Sender).Name;
  n.DeleteFirst;
  n.DeleteFirst;
  Canvas.FillText(ARect, n, False, 1.0, [], TTextAlign.Leading);
end;

end.
