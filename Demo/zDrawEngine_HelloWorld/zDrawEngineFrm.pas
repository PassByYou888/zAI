unit zDrawEngineFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  System.IOUtils,

  CoreClasses, UnicodeMixedLib,
  Geometry2DUnit, zDrawEngine, MemoryRaster, zDrawEngineInterface_SlowFMX;

type
  TzDrawEngineForm = class(TForm)
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    drawIntf: TDrawEngineInterface_FMX;
    background, raster: TMemoryRaster;
    angle: TGeoFloat;
  end;

var
  zDrawEngineForm: TzDrawEngineForm;

implementation

{$R *.fmx}


procedure TzDrawEngineForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  EnginePool.Clear;
  DisposeObject([drawIntf, background, raster]);
end;

procedure TzDrawEngineForm.FormCreate(Sender: TObject);
begin
  drawIntf := TDrawEngineInterface_FMX.Create;
  background := NewRaster();
  background.SetSize(256, 256);
  FillBlackGrayBackgroundTexture(background, 32);
  raster := NewRasterFromFile(umlCombineFileName(TPath.GetLibraryPath, 'canglaoshi.bmp'));
  angle := 0;
end;

procedure TzDrawEngineForm.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  d: TDrawEngine;
  fi, fj: TGeoFloat;
begin
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);
  d.ViewOptions := [voFPS, voEdge];

  // draw background
  fi := 0;
  while fi < d.width do
    begin
      fj := 0;
      while fj < d.height do
        begin
          d.DrawPicture(background, background.BoundsRectV2,
            RectAdd(background.BoundsRectV2, Vec2(fi, fj)), 0, 1.0);
          fj := fj + background.height - 1;
        end;
      fi := fi + background.width - 1;
    end;

  d.DrawPicture(raster, raster.BoundsRectV2, raster.BoundsRectV2, angle, 1.0);
  angle := NormalizeDegAngle(angle + d.LastDeltaTime * 45);
  d.Flush;
end;

procedure TzDrawEngineForm.Timer1Timer(Sender: TObject);
begin
  EnginePool.Progress(Interval2Delta(Timer1.Interval));
  Invalidate;
end;

end.
