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
  d.ViewOptions := [devpFrameEndge];

  d.FillBox(d.ScreenRect, DEColor(0, 0, 0));
  while not fr.ReadFrame(raster, False) do
    begin
      fr.Seek(0);
      fr.Current_Frame := 0;
    end;

  raster.FastUpdateTexture;
  d.FitDrawTexture(raster, raster.BoundsRectV2, d.ScreenRect, 1.0);
  d.BeginCaptureShadow(Vec2(1, 1), 1.0);
  d.DrawText(Format('time:%f:%f' + #13#10 + 'frame:%d:%d', [fr.Current, fr.Total, fr.Current_Frame, fr.Total_Frame]),
    24, d.ScreenRect, DEColor(0.2, 1, 0.2, 1), False);
  d.EndCaptureShadow;
  d.Flush;
end;

procedure TFFMPEGReaderDemoForm.Timer1Timer(Sender: TObject);
begin
  EnginePool.Progress(Interval2Delta(Timer1.Interval));
  Invalidate;
end;

end.
