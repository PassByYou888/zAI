unit SoftRenderFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.ExtCtrls,

  zDrawEngine, zDrawEngineInterface_FMX, CoreClasses, MemoryRaster, Geometry2DUnit;

type
  TForm1 = class(TForm)
    RenderButton: TButton;
    Timer1: TTimer;
    PaintBox: TPaintBox;
    Image: TImage;
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
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


procedure Render(Draw: TDrawEngine);
var
  d: TTimeTick;
begin
  if not Draw.ReadyOK then
      exit;
  Draw.SetSize;
  Draw.FillBox(Draw.ScreenRect, DEColor(0, 0, 0, 1));
  Draw.DrawText('Hello world', 18, Draw.ScreenRect, DEColor(1, 0, 0, 0.8), True, DEVec(0.5, 0.5), -15);
  Draw.FillBox(RectV2(100, 100, 150, 150), 15, DEColor(1, 1, 1, 0.8));
  Draw.DrawBox(RectV2(100, 100, 150, 150), -15, DEColor(1, 0.5, 0.5, 0.8), 5);
  Draw.DrawEllipse(DEVec(200, 100), 70, DEColor(1, 0, 0, 0.8));
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
      exit;
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
  d.Rasterization.SetSize(RectV2(0, 0, Image.Width, Image.Height));
  d.Rasterization.UsedAgg := True;
  Render(d);
  MemoryBitmapToBitmap(d.Rasterization.Memory, Image.Bitmap);
  DisposeObject(d);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Invalidate;
end;

end.
