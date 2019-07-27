unit FillColorFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,

  Threading, SyncObjs,

  CoreClasses, PascalStrings, UnicodeMixedLib, Geometry2DUnit, MemoryRaster,
  MemoryStream64, DoStatusIO, zDrawEngine, zDrawEngineInterface_FMX;

type
  TFillColorForm = class(TForm)
    Timer1: TTimer;
    Timer2: TTimer;
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    dIntf: TDrawEngineInterface_FMX;
    tex: TDETexture;

    constructor Create(AOwner: TComponent); override;

    procedure BuildGeometry(w, h, steps: Integer; output: TMemoryRaster);
  end;

var
  FillColorForm: TFillColorForm;

implementation

{$R *.fmx}


procedure TFillColorForm.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  d: TDrawEngine;
begin
  dIntf.SetSurface(Canvas, Self);
  d := DrawPool(Self, dIntf);

  d.DrawOptions := [voFPS, voEdge];
  d.FillBox(d.ScreenRect, DEColor(0, 0, 0, 1));

  d.FitDrawPicture(tex, tex.BoundsRectV2, d.ScreenRect, 1.0);
  d.Flush;
end;

procedure TFillColorForm.Timer1Timer(Sender: TObject);
begin
  EnginePool.Progress(Interval2Delta(Timer1.Interval));
  Invalidate;
end;

procedure TFillColorForm.Timer2Timer(Sender: TObject);
begin
  TComputeThread.RunP(nil, tex, procedure(thSender: TComputeThread)
    var
      n: TDETexture;
    begin
      n := TDrawEngine.NewTexture;
      BuildGeometry(Round(Width), Round(Height), 15, n);
      TThread.Synchronize(thSender, procedure
        begin
          DisposeObject(tex);
          tex := n;
        end);
    end);
end;

constructor TFillColorForm.Create(AOwner: TComponent);
begin
  inherited;
  dIntf := TDrawEngineInterface_FMX.Create;
  tex := TDrawEngine.NewTexture;
  BuildGeometry(Round(Width), Round(Height), 15, tex);
end;

procedure TFillColorForm.BuildGeometry(w, h, steps: Integer; output: TMemoryRaster);
const
  edge = 30;
var
  j: Integer;
  vl: TVec2List;
  v: TVec2;
begin
  output.SetSize(w, h, RasterColorF(0, 0, 0, 1));

  vl := TVec2List.Create;
  // 根据steps系数构建原始坐标系
  for j := 0 to steps - 1 do
      vl.Add(umlRandomRange(edge, w - edge), umlRandomRange(edge, h - edge));
  // 计算几何点集凸包
  vl.ConvexHull;

  // 并行化填充凸包
  // 并行化填充像素效率会低于三角填充 定律:算法级优化结果高于程序和硬件级优化
  // 注意:并行化填充可以支持凹凸多边形
  // 注意:三角填充只能凸多边形
  // 在图像语义分割技术中,像素填充是作为主要输入的数据源,并行填充技术被大量应用
  TParallel.for(0, h - 1, procedure(pass: Integer)
    var
      i: Integer;
    begin
      // 这里的程序模式很类似shader可编程管线
      for i := 0 to w - 1 do
        if vl.InHere(Vec2(i, pass)) then
            output.Pixel[i, pass] := RasterColorF(0.5, 0, 0, 1);
    end);
  // 三角填充，这种方法是内置的，多用于直接画图
  // output.Vertex.FillPoly(vl, RasterColorF(0, 1, 0, 0.5));

  // 画包围线，填充法只会在指定背景色边缘画轮廓
  output.FillNoneBGColorBorder(RasterColorF(0, 0, 0, 1), RColorF(1, 1, 1, 1), 4);

  // 画重心轴
  output.DrawCrossF(vl.Centroid, 10, RasterColorF(1, 1, 1, 1));

  DisposeObject(vl);
end;

end.
