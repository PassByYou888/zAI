unit SoftRenderFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.ExtCtrls,

  zDrawEngine, zDrawEngineInterface_SlowFMX, CoreClasses, MemoryRaster, Geometry2DUnit;

type
  TForm1 = class(TForm)
    RenderButton: TButton;
    Timer1: TTimer;
    PaintBox: TPaintBox;
    Image: TImage;
    usedAggCheckBox: TCheckBox;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
    procedure RenderButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    fmxIntf: TDrawEngineInterface_FMX;
    procedure Render(Draw: TDrawEngine);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


procedure TForm1.Render(Draw: TDrawEngine);
begin
  if not Draw.ReadyOK then
      Exit;
  Draw.ViewOptions := [voEdge];
  Draw.SetSize;
  Draw.FillBox(Draw.ScreenRect, DEColor(0.5, 0.5, 0.5, 1));

  Draw.FillBox(RectV2(100, 100, 250, 250), -180, DEColor(1, 1, 1, 0.2));
  Draw.DrawBox(RectV2(49, 100, 151, 151), -15, DEColor(1, 0.5, 0.5, 0.9), 2);
  Draw.FillEllipse(DERect(50, 100, 300, 250), DEColor(1, 1, 1, 0.5));

  Draw.BeginCaptureShadow(Vec2(4, 4), 0.9);
  Draw.DrawText('|s:10,color(1,1,1,1)|Hello|color(1,1,1,1)| world' + #13#10 +
    '||' + #13#10 + 'default text |s:22,color(0,1,0,1)| big green' + #13#10 + 'big green line 2' + #13#10 + 'big green line 3',
    18, Draw.ScreenRect, DEColor(1, 0, 0, 0.8), True, DEVec(0.5, 0.5), -22);
  Draw.EndCaptureShadow;

  Draw.Flush;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DisposeObject(fmxIntf);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  fmxIntf := TDrawEngineInterface_FMX.Create;
  PaintBox.Canvas.Font.Style := [TFontStyle.fsBold];
end;

procedure TForm1.PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
var
  d: TDrawEngine;
begin
  if fmxIntf = nil then
      Exit;
  fmxIntf.SetSurface(Canvas, Sender);
  d := TDrawEngine.Create;
  d.DrawInterface := fmxIntf;
  d.SetSize;
  Render(d);
  DisposeObject(d);
end;

procedure TForm1.RenderButtonClick(Sender: TObject);
var
  d: TDrawEngine;
begin
  d := TDrawEngine.Create;
  d.Rasterization.UsedAgg := usedAggCheckBox.IsChecked;
  d.Rasterization.SetSize(RectV2(0, 0, Image.width, Image.height));
  // d.Rasterization.Memory.Vertex.DrawTriangleEdge:=True;
  Render(d);
  MemoryBitmapToBitmap(d.Rasterization.Memory, Image.Bitmap);
  DisposeObject(d);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Invalidate;
end;

end.
