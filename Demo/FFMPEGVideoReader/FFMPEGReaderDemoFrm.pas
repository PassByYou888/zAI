unit FFMPEGReaderDemoFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  System.IOUtils,

  CoreClasses, UnicodeMixedLib,
  Geometry2DUnit, zDrawEngine, MemoryRaster, zDrawEngineInterface_SlowFMX,
  FFMPEG, FFMPEG_Reader;

type
  TFFMPEGReaderDemoForm = class(TForm)
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure Timer1Timer(Sender: TObject);
  private
  public
    drawIntf: TDrawEngineInterface_FMX;
    raster: TDETexture;
    fr: TFFMPEG_Reader;
  end;

var
  FFMPEGReaderDemoForm: TFFMPEGReaderDemoForm;

implementation

{$R *.fmx}


procedure TFFMPEGReaderDemoForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  EnginePool.Clear;
  DisposeObject([drawIntf, raster]);
end;

procedure TFFMPEGReaderDemoForm.FormCreate(Sender: TObject);
begin
  drawIntf := TDrawEngineInterface_FMX.Create;
  raster := DefaultTextureClass.Create;
  fr := TFFMPEG_Reader.Create(umlCombineFileName(TPath.GetLibraryPath, 'market.mp4').Text);
end;

procedure TFFMPEGReaderDemoForm.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  d: TDrawEngine;
  fi, fj: TGeoFloat;
begin
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);
  d.ViewOptions := [devpFPS, devpFrameEndge];

  d.FillBox(d.ScreenRect, DEColor(0, 0, 0));
  if fr.ReadFrame(raster, False) then
    begin
      raster.FastUpdateTexture;
      d.FitDrawTexture(raster, raster.BoundsRectV2, d.ScreenRect, 1.0);
      d.DrawText(Format('%f:%f', [fr.Current, fr.Total]), 16, d.ScreenRect, DEColor(1, 0, 0, 1), True);
    end
  else
      fr.Seek(0);
  d.Flush;
end;

procedure TFFMPEGReaderDemoForm.Timer1Timer(Sender: TObject);
begin
  EnginePool.Progress(Interval2Delta(Timer1.Interval));
  Invalidate;
end;

end.
