unit Render2VideoFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.StdCtrls, FMX.ListBox, FMX.Controls.Presentation, FMX.Objects,
  FMX.Layouts, FMX.Edit,

  MemoryRaster, zDrawEngine, Geometry2DUnit, zDrawEngineInterface_SlowFMX,
  Y4M, H264, zDrawEngineSoftH264, zDrawEngineSoftY4M, PascalStrings, CoreClasses,
  Cadencer, UnicodeMixedLib;

type
  TRender2VideoForm = class(TForm, ICadencerProgressInterface)
    ClientLayout: TLayout;
    Layout1: TLayout;
    PaintBox: TPaintBox;
    PlayButton: TButton;
    Layout2: TLayout;
    Label1: TLabel;
    fpsEdit: TEdit;
    Timer: TTimer;
    h264SaveDialog: TSaveDialog;
    Y4MSaveDialog: TSaveDialog;
    ComboBox: TComboBox;
    BuildButton: TButton;
    Layout3: TLayout;
    Label2: TLabel;
    TotalFrameEdit: TEdit;
    ProgressBar: TProgressBar;
    SizeInfoLabel: TLabel;
    procedure BuildButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure fpsEditChange(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    drawIntf: TDrawEngineInterface_FMX;
    FixedCadencer: TCadencer;
    procedure RenderFrame(d: TDrawEngine);
    procedure CadencerProgress(const deltaTime, newTime: Double);
  end;

var
  Render2VideoForm: TRender2VideoForm;

implementation

{$R *.fmx}


procedure TRender2VideoForm.BuildButtonClick(Sender: TObject);
  procedure doY4MEncoder();
  var
    i: Integer;
    total: Integer;
    dIntf: TDrawEngine_YUV4MPEG;
  begin
    if not Y4MSaveDialog.Execute then
        exit;

    ClientLayout.Enabled := False;
    Timer.Enabled := False;
    total := umlStrToInt(TotalFrameEdit.Text, 0) - 1;

    dIntf := TDrawEngine_YUV4MPEG.CreateOnFile(
      Trunc(PaintBox.Width),
      Trunc(PaintBox.Height),
      Trunc(1 / FixedCadencer.FixedDeltaTime),
      Y4MSaveDialog.FileName);
    ProgressBar.Max := total;
    ProgressBar.Min := 0;
    for i := 0 to total do
      begin
        RenderFrame(dIntf.Engine);
        dIntf.Engine.Progress(FixedCadencer.FixedDeltaTime);
        ProgressBar.Value := i;
        SizeInfoLabel.Text := umlSizeToStr(dIntf.Y4MSize);
        Application.ProcessMessages;
      end;

    DisposeObject(dIntf);

    Timer.Enabled := True;
    ClientLayout.Enabled := True;
  end;

  procedure doH264Encoder();
  var
    i: Integer;
    total: Integer;
    dIntf: TDrawEngine_H264;
  begin
    if not h264SaveDialog.Execute then
        exit;

    ClientLayout.Enabled := False;
    Timer.Enabled := False;
    total := umlStrToInt(TotalFrameEdit.Text, 0) - 1;

    dIntf := TDrawEngine_H264.Create(
      Trunc(PaintBox.Width),
      Trunc(PaintBox.Height),
      total + 1,
      Trunc(1 / FixedCadencer.FixedDeltaTime),
      h264SaveDialog.FileName);
    ProgressBar.Max := total;
    ProgressBar.Min := 0;
    for i := 0 to total do
      begin
        RenderFrame(dIntf.Engine);
        dIntf.Engine.Progress(FixedCadencer.FixedDeltaTime);
        ProgressBar.Value := i;
        SizeInfoLabel.Text := umlSizeToStr(dIntf.H264Size);
        Application.ProcessMessages;
      end;

    DisposeObject(dIntf);

    Timer.Enabled := True;
    ClientLayout.Enabled := True;
  end;

begin
  if ComboBox.ItemIndex = 0 then
      doY4MEncoder
  else
      doH264Encoder;
end;

procedure TRender2VideoForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(drawIntf);
end;

procedure TRender2VideoForm.CadencerProgress(const deltaTime, newTime: Double);
begin
  EnginePool.Progress(deltaTime);
end;

procedure TRender2VideoForm.FormCreate(Sender: TObject);
begin
  drawIntf := TDrawEngineInterface_FMX.Create;
  FixedCadencer := TCadencer.Create;
  FixedCadencer.FixedDeltaTime := 1.0 / umlStrToInt(fpsEdit.Text, 30);
  FixedCadencer.ProgressIntf := Self;
end;

procedure TRender2VideoForm.fpsEditChange(Sender: TObject);
begin
  FixedCadencer.FixedDeltaTime := 1.0 / umlStrToInt(fpsEdit.Text, 30);
end;

procedure TRender2VideoForm.PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
var
  d: TDrawEngine;
begin
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);
  d.ViewOptions := [voFPS];
  RenderFrame(d);
end;

procedure TRender2VideoForm.RenderFrame(d: TDrawEngine);
var
  a: TDEFloat;
begin
  d.SetSize;
  d.FillBox(d.ScreenRect, DEColor(0, 0, 0, 1));

  a := d.UserVariants.GetDefaultValue('angle', 0);
  a := a + d.LastDeltaTime * 45;
  d.UserVariants['angle'] := NormalizeDegAngle(a);

  d.DrawText('hello world', 22, d.ScreenRect, DEColor(1, 0.5, 0.5, 0.9), True, Vec2(0.5, 0.5), a);

  d.Flush;
end;

procedure TRender2VideoForm.TimerTimer(Sender: TObject);
begin
  FixedCadencer.Progress;
  Invalidate;
end;

end.
