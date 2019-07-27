unit PolygonScaleAndExtractMainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,

  CoreClasses, UnicodeMixedLib, PascalStrings, Geometry2DUnit,
  zDrawEngine, zDrawEngineInterface_SlowFMX;

type
  TPolygonScaleAndExtractMainForm = class(TForm)
    pb1: TPaintBox;
    pb2: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure pb1Click(Sender: TObject);
    procedure pb2Paint(Sender: TObject; Canvas: TCanvas);
    procedure pb1Paint(Sender: TObject; Canvas: TCanvas);
  private
    drawIntf: TDrawEngineInterface_FMX;
    samePolygon1, samePolygon2: TVec2List;
  public
  end;

var
  PolygonScaleAndExtractMainForm: TPolygonScaleAndExtractMainForm;

implementation

{$R *.fmx}


procedure TPolygonScaleAndExtractMainForm.FormCreate(Sender: TObject);
begin
  drawIntf := TDrawEngineInterface_FMX.Create;

  samePolygon1 := TVec2List.Create;
  samePolygon2 := TVec2List.Create;

  pb1Click(nil);
end;

procedure TPolygonScaleAndExtractMainForm.pb1Click(Sender: TObject);
const
  edge = 50;
var
  i: Integer;
begin
  samePolygon1.Clear;
  for i := 1 to 50 do
      samePolygon1.Add(umlRandomRange(edge, round(pb1.width) - edge), umlRandomRange(edge, round(pb1.height) - edge));
  samePolygon1.ConvexHull();

  samePolygon2.Assign(samePolygon1);
  Invalidate;
end;

procedure TPolygonScaleAndExtractMainForm.pb2Paint(Sender: TObject; Canvas: TCanvas);
var
  d: TDrawEngine;
  np: TVec2List;
begin
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);

  d.FillBox(d.ScreenRect, DEColor(0.2, 0.2, 0.2, 1));

  d.DrawText('|s:16,color(0.5,0.5,1,1)|多边形等距放大会走形|color(0,1,0,1)| 点击鼠标重构|s:10|' + #13#10 + 'craete by.qq600585', 12, d.ScreenRect, DEColor(1, 1, 1, 1), False);

  np := TVec2List.Create;
  np.Assign(samePolygon2);
  d.DrawPL(False, np, True, DEColor(1, 1, 1, 1), 1);

  np.Clear;
  samePolygon2.ExpandDistanceAsList(15, np);
  d.DrawPL(True, np, True, DEColor(1, 1, 1, 1), 1);

  disposeObject(np);

  d.Flush;
end;

procedure TPolygonScaleAndExtractMainForm.pb1Paint(Sender: TObject; Canvas: TCanvas);
var
  d: TDrawEngine;
  c1, c2: TVec2; // 重心轴
  np: TVec2List;
begin
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);

  d.FillBox(d.ScreenRect, DEColor(0.2, 0.2, 0.2, 1));

  d.DrawText('|s:16,color(0.5,0.5,1,1)|乘法放大不会走形|color(0,1,0,1)| 点击鼠标重构|s:10|' + #13#10 + 'craete by.qq600585', 12, d.ScreenRect, DEColor(1, 1, 1, 1), False);

  np := TVec2List.Create;
  np.Assign(samePolygon1);
  c1 := np.Centroid;
  d.DrawPL(False, np, True, DEColor(1, 1, 1, 1), 1);

  np.Scale(1.1);
  c2 := np.Centroid;
  // 用重心轴重新构造多边形的位置，方便查看
  np.Transform(Vec2Sub(c1, c2));
  d.DrawPL(True, np, True, DEColor(1, 1, 1, 1), 1);

  disposeObject(np);

  d.Flush;
end;

end.
