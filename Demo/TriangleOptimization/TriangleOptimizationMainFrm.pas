unit TriangleOptimizationMainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,

  CoreClasses, PascalStrings, UnicodeMixedLib,
  zDrawEngine, zDrawEngineInterface_SlowFMX, Geometry2DUnit;

type
  TTriangleOptimizationMainForm = class(TForm)
    Timer1: TTimer;
    pb1: TPaintBox;
    pb2: TPaintBox;
    pb3: TPaintBox;
    pb4: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure pb1Paint(Sender: TObject; Canvas: TCanvas);
    procedure pb2Paint(Sender: TObject; Canvas: TCanvas);
    procedure pb3Paint(Sender: TObject; Canvas: TCanvas);
    procedure pb4Paint(Sender: TObject; Canvas: TCanvas);
    procedure pbClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    drawIntf: TDrawEngineInterface_FMX;
    DemoPolygon: T2DPolygonGraph;
    pts1, pts2, pts3: TTriangleList;

    procedure BuildPolygon;
  end;

var
  TriangleOptimizationMainForm: TTriangleOptimizationMainForm;

implementation

{$R *.fmx}


procedure TTriangleOptimizationMainForm.BuildPolygon;
const
  edge = 10;
var
  i: Integer;
  r: TRectV2;
  v: TVec2;
  poly: T2DPolygon;
begin
  DemoPolygon.Clear;
  for i := 1 to 50 do
      DemoPolygon.Surround.Add(umlRandomRangeF(edge, round(pb1.width) - edge), umlRandomRangeF(edge, round(pb1.height) - edge));
  DemoPolygon.Surround.ConvexHull();

  r := DemoPolygon.Surround.BoundBox();

  while DemoPolygon.CollapsesCount < 1 do
    begin
      poly := T2DPolygon.Create;
      poly.Owner := DemoPolygon;
      while poly.Count < 5 do
        begin
          v := Vec2(umlRandomRangeF(r[0, 0], r[1, 0]), umlRandomRangeF(r[0, 1], r[1, 1]));
          if DemoPolygon.InSurround(v) and (not DemoPolygon.InCollapse(v)) then
              poly.Add(v);
        end;
      poly.ConvexHull();
      DemoPolygon.AddCollapse(poly);
    end;

  pts1.BuildTriangle(DemoPolygon);
  pts2.BuildTriangle(DemoPolygon, 10, 10, 1000);
  pts3.BuildTriangle(DemoPolygon, 20, 2, 300);
end;

procedure TTriangleOptimizationMainForm.FormCreate(Sender: TObject);
begin
  drawIntf := TDrawEngineInterface_FMX.Create;

  DemoPolygon := T2DPolygonGraph.Create;
  pts1 := TTriangleList.Create;
  pts2 := TTriangleList.Create;
  pts3 := TTriangleList.Create;
  BuildPolygon;
end;

procedure TTriangleOptimizationMainForm.pb1Paint(Sender: TObject; Canvas: TCanvas);
var
  d: TDrawEngine;
  i: Integer;
begin
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);

  d.FillBox(d.ScreenRect, DEColor(0.2, 0.2, 0.2, 1));

  d.DrawText('|s:16|原始多边形||' + #13#10 + 'craete by.qq600585', 12, d.ScreenRect, DEColor(1, 1, 1, 1), False);

  d.DrawPL(DemoPolygon.Surround, True, DEColor(1, 1, 1, 1), 1);
  for i := 0 to DemoPolygon.CollapsesCount - 1 do
      d.DrawPL(DemoPolygon.Collapses[i], True, DEColor(1, 1, 1, 1), 1);

  d.Flush;
end;

procedure TTriangleOptimizationMainForm.pb2Paint(Sender: TObject; Canvas: TCanvas);
var
  d: TDrawEngine;
  i: Integer;
begin
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);

  d.FillBox(d.ScreenRect, DEColor(0.2, 0.2, 0.2, 1));

  d.DrawText('|s:16|三角切割方式1||' + #13#10 + 'craete by.qq600585', 12, d.ScreenRect, DEColor(1, 1, 1, 1), False);

  d.DrawPL(DemoPolygon.Surround, True, DEColor(1, 1, 1, 1), 1);
  for i := 0 to DemoPolygon.CollapsesCount - 1 do
      d.DrawPL(DemoPolygon.Collapses[i], True, DEColor(1, 1, 1, 1), 1);

  d.DrawTriangle(False, pts1, DEColor(1, 0.5, 0.5, 0.9), 1, True);

  d.Flush;
end;

procedure TTriangleOptimizationMainForm.pb3Paint(Sender: TObject; Canvas: TCanvas);
var
  d: TDrawEngine;
  i: Integer;
begin
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);

  d.FillBox(d.ScreenRect, DEColor(0.2, 0.2, 0.2, 1));

  d.DrawText('|s:16|三角切割方式2||' + #13#10 + 'craete by.qq600585', 12, d.ScreenRect, DEColor(1, 1, 1, 1), False);

  d.DrawPL(DemoPolygon.Surround, True, DEColor(1, 1, 1, 1), 1);
  for i := 0 to DemoPolygon.CollapsesCount - 1 do
      d.DrawPL(DemoPolygon.Collapses[i], True, DEColor(1, 1, 1, 1), 1);

  d.DrawTriangle(False, pts2, DEColor(1, 0.5, 0.5, 0.9), 1, True);

  d.Flush;
end;

procedure TTriangleOptimizationMainForm.pb4Paint(Sender: TObject; Canvas: TCanvas);
var
  d: TDrawEngine;
  i: Integer;
begin
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);

  d.FillBox(d.ScreenRect, DEColor(0.2, 0.2, 0.2, 1));

  d.DrawText('|s:16|三角切割方式3||' + #13#10 + 'craete by.qq600585', 12, d.ScreenRect, DEColor(1, 1, 1, 1), False);

  d.DrawPL(DemoPolygon.Surround, True, DEColor(1, 1, 1, 1), 1);
  for i := 0 to DemoPolygon.CollapsesCount - 1 do
      d.DrawPL(DemoPolygon.Collapses[i], True, DEColor(1, 1, 1, 1), 1);

  d.DrawTriangle(False, pts3, DEColor(1, 0.5, 0.5, 0.9), 1, True);

  d.Flush;
end;

procedure TTriangleOptimizationMainForm.pbClick(Sender: TObject);
begin
  BuildPolygon;
end;

procedure TTriangleOptimizationMainForm.Timer1Timer(Sender: TObject);
begin
  EnginePool.Progress(Interval2Delta(TTimer(Sender).Interval));
  Invalidate;
end;

end.
