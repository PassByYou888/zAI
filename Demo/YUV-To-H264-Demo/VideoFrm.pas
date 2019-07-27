unit VideoFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.StdCtrls, FMX.ListBox, FMX.Controls.Presentation, FMX.Objects,
  FMX.Layouts,

  MemoryRaster, zDrawEngine, Geometry2DUnit, zDrawEngineInterface_SlowFMX,
  Y4M, H264, PascalStrings, CoreClasses, UnicodeMixedLib;

type
  TVideoForm = class(TForm)
    PaintBox: TPaintBox;
    OpenButton: TButton;
    OpenDialog: TOpenDialog;
    ComboBox: TComboBox;
    Label1: TLabel;
    Memo: TMemo;
    ProcessButton: TButton;
    Timer: TTimer;
    StopButton: TButton;
    Y4MSaveDialog: TSaveDialog;
    clientLayout: TLayout;
    h264SaveDialog: TSaveDialog;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
    procedure ProcessButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    yR: TY4MReader;
    drawIntf: TDrawEngineInterface_FMX;
    VideoTexture: TDETexture;
  end;

var
  VideoForm: TVideoForm;

implementation

{$R *.fmx}


procedure TVideoForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(VideoTexture);
  DisposeObject(drawIntf);
end;

procedure TVideoForm.FormCreate(Sender: TObject);
begin
  drawIntf := TDrawEngineInterface_FMX.Create;
  VideoTexture := DefaultTextureClass.Create;
end;

procedure TVideoForm.OpenButtonClick(Sender: TObject);
begin
  if not OpenDialog.Execute then
      exit;
  yR := TY4MReader.CreateOnFile(OpenDialog.FileName);
  yR.ReadFrame.SaveToRaster(VideoTexture);
  VideoTexture.ReleaseGPUMemory;
end;

procedure TVideoForm.PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
var
  d: TDrawEngine;
  vInfo: SystemString;
  vSiz: TDEVec;
  vR: TDERect;
begin
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);
  d.ViewOptions := [voFPS];
  d.SetSize;

  d.FillBox(d.ScreenRect, DEColor(0, 0, 0, 1));
  d.DrawPicture(VideoTexture, VideoTexture.BoundsRectV2, d.ScreenRect, 1.0);
  vInfo := Format('Frame information: %d * %d', [VideoTexture.Width, VideoTexture.Height]);
  vSiz := d.GetTextSize(vInfo, 12);
  vR := DERect(DEVec(0, 0), vSiz);
  vR := RectOffset(vR, DEVec(0, 20));

  d.BeginCaptureShadow(Vec2(2, 2), 0.9);
  d.DrawText(vInfo, 12, vR, DEColor(1, 0, 0.9, 1), True, Vec2(1, 0), -25);
  d.EndCaptureShadow;
  d.Flush;
end;

procedure TVideoForm.ProcessButtonClick(Sender: TObject);

  procedure doY4MEncoder();
  var
    enWriter: TY4MWriter;
    i: Integer;
    raster: TMemoryRaster;
  begin
    if not Y4MSaveDialog.Execute then
        exit;

    clientLayout.Enabled := False;
    Timer.Enabled := False;

    enWriter := TY4MWriter.CreateOnFile(yR.Width, yR.Height, Round(yR.FrameRate + 0.5), Y4MSaveDialog.FileName);
    raster := TMemoryRaster.Create;
    yR.SeekFirstFrame;
    for i := yR.CurrentFrame to yR.FrameCount - 1 do
      begin
        yR.ReadFrame.SaveToRaster(raster);
        raster.DrawText('hello world', 0, 0, Vec2(1, 0), -15, 0.9, 28, RasterColorF(1, 0, 0, 0.5));
        enWriter.WriteFrame(raster);
        Memo.Text := Format('encoder information: YUV for mpeg2' + #13#10 +
          'output %s = %s' + #13#10 +
          'size %d * %d' + #13#10 +
          'frame %d / %d',
          [umlGetFileName(Y4MSaveDialog.FileName).Text,
          umlSizeToStr(enWriter.Y4MSize).Text,
          yR.Width, yR.Height, yR.FrameCount, i]);
        Application.ProcessMessages;
      end;
    DisposeObject(enWriter);
    DisposeObject(raster);

    Timer.Enabled := True;
    clientLayout.Enabled := True;
  end;

  procedure doH264Encoder();
  var
    enWriter: TH264Writer;
    i: Integer;
    raster: TMemoryRaster;
  begin
    if not h264SaveDialog.Execute then
        exit;

    clientLayout.Enabled := False;
    Timer.Enabled := False;

    enWriter := TH264Writer.Create(yR.Width, yR.Height, yR.FrameCount, yR.FrameRate, h264SaveDialog.FileName);
    raster := TMemoryRaster.Create;
    yR.SeekFirstFrame;
    for i := yR.CurrentFrame to yR.FrameCount - 1 do
      begin
        yR.ReadFrame.SaveToRaster(raster);
        raster.DrawText('hello world', 0, 0, Vec2(1, 0), -15, 0.9, 28, RasterColorF(1, 0, 0, 0.5));
        enWriter.WriteFrame(raster);
        Memo.Text := Format('encoder information: H264' + #13#10 +
          'output %s = %s' + #13#10 +
          'size %d * %d' + #13#10 +
          'frame %d / %d',
          [umlGetFileName(h264SaveDialog.FileName).Text,
          umlSizeToStr(enWriter.H264Size).Text,
          yR.Width, yR.Height, yR.FrameCount, i]);
        Application.ProcessMessages;
      end;
    DisposeObject(enWriter);
    DisposeObject(raster);

    Timer.Enabled := True;
    clientLayout.Enabled := True;
  end;

begin
  if yR = nil then
      exit;
  if ComboBox.ItemIndex = 0 then
      doY4MEncoder
  else
      doH264Encoder;
  Memo.Lines.Clear;
end;

procedure TVideoForm.StopButtonClick(Sender: TObject);
begin
  if (yR <> nil) then
    begin
      DisposeObject(yR);
      yR := nil;
    end;
end;

procedure TVideoForm.TimerTimer(Sender: TObject);
begin
  if (yR <> nil) then
    begin
      if (yR.CurrentFrame < yR.FrameCount) then
        begin
          yR.ReadFrame.SaveToRaster(VideoTexture);
          VideoTexture.ReleaseGPUMemory;
        end
      else
          yR.SeekFirstFrame;
    end;
  Invalidate;
  EnginePool.Progress(Interval2Delta(10));
end;

end.
