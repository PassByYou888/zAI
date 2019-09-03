unit BoxColorDetectorMainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,

  CoreClasses,
  PascalStrings,
  UnicodeMixedLib,
  DoStatusIO,
  Geometry2DUnit,
  MemoryRaster,
  zDrawEngine,
  zDrawEngineInterface_SlowFMX;

type
  TBoxColorDetectorMainForm = class(TForm)
    Image1: TImage;
    Image2: TImage;
    detButton: TButton;
    RCLineDetButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure detButtonClick(Sender: TObject);
    procedure RCLineDetButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    raster: TMemoryRaster;
  end;

var
  BoxColorDetectorMainForm: TBoxColorDetectorMainForm;

implementation

{$R *.fmx}


procedure TBoxColorDetectorMainForm.FormCreate(Sender: TObject);
var
  r: TRect;
  i: Integer;
begin
  raster := NewRaster;
  BitmapToMemoryBitmap(Image1.Bitmap, raster);

  RandSeed := 2;
  for i := 0 to 19 do
    begin
      r.Left := umlRandomRange(5, raster.Width - 6);
      r.Top := umlRandomRange(5, raster.Height - 6);
      r.Right := umlRandomRange(5, raster.Width - 6);
      r.Bottom := umlRandomRange(5, raster.Height - 6);
      raster.DrawRect(RectV2(r), RColorF(1, 1, 1, 1));
    end;
  MemoryBitmapToBitmap(raster, Image1.Bitmap);
end;

procedure TBoxColorDetectorMainForm.detButtonClick(Sender: TObject);
var
  n: TMemoryRaster;
  RCLines: TRCLines;
  r: TRectV2;
begin
  n := NewRaster();
  n.SetSize(raster.Width, raster.Height, RColor(0, 0, 0));
  RCLines := TRCLines.Create(raster, RColorF(1, 1, 1, 1), 0.2, 10);
  while RCLines.ProcessFormulaBox(r) do
    begin
      n.DrawRect(r, RColorF(1, 0, 0, 1));
      n.DrawText(Format('%d*%d', [RoundWidth(r), RoundHeight(r)]), Round(r[0, 0] + 4), Round(r[0, 1] + 4), 12, RColorF(1, 0, 0, 1));
    end;
  disposeObject(RCLines);
  MemoryBitmapToBitmap(n, Image2.Bitmap);
  disposeObject(n);
end;

procedure TBoxColorDetectorMainForm.RCLineDetButtonClick(Sender: TObject);
var
  n: TMemoryRaster;
  RCLines: TRCLines;
  p: PRCLine;
  i: Integer;
begin
  n := NewRaster();
  n.SetSize(raster.Width, raster.Height, RColor(0, 0, 0));
  RCLines := TRCLines.Create(raster, RColorF(1, 1, 1, 1), 0.2, 10);

  for i := 0 to RCLines.Count - 1 do
    begin
      p := RCLines[i];
      n.LineF(vec2(p^.Bp), vec2(p^.Ep), RandomRColor, True, True);
    end;

  disposeObject(RCLines);
  MemoryBitmapToBitmap(n, Image2.Bitmap);
  disposeObject(n);
end;

end.
