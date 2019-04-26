unit GaussPyramidsRegionFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.TabControl, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.Layouts, FMX.ListBox, FMX.Objects, FMX.StdCtrls,
  FMX.Surfaces, System.Math,

  CoreClasses, DoStatusIO, MemoryRaster, PascalStrings, ObjectDataManager, ItemStream,
  UnicodeMixedLib, Learn, LearnTypes, PyramidSpace, Geometry2DUnit,

  zDrawEngine, zDrawEngineInterface_SlowFMX, FMX.ExtCtrls;

type
  TGaussPyramidsForm = class(TForm)
    TabControl1: TTabControl;
    Memo1: TMemo;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    OpenDialog1: TOpenDialog;
    Layout1: TLayout;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Splitter1: TSplitter;
    PaintBox1: TPaintBox;
    Timer1: TTimer;
    PaintBox2: TPaintBox;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    ImageViewer1: TImageViewer;
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
    procedure Timer1Timer(Sender: TObject);
    procedure PaintBox2Paint(Sender: TObject; Canvas: TCanvas);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure Button4Click(Sender: TObject);
    procedure PaintBox1Resize(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure PaintBox2Resize(Sender: TObject);
    procedure PaintBox2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  private
    { Private declarations }
  public
    { Public declarations }
    ft1, ft2: TFeature;
    ort1, ort2: TDETexture;
    t1, t2: TDETexture;
    vl1, vl2: TVec2List;
    b1, b2: TDERect;
    drawIntf: TDrawEngineInterface_FMX;
    procedure DoStatusM(AText: SystemString; const ID: Integer);
    constructor Create(AOwner: TComponent); override;
  end;

var
  GaussPyramidsForm: TGaussPyramidsForm;

implementation

{$R *.fmx}


procedure TGaussPyramidsForm.Button1Click(Sender: TObject);
begin
  if not OpenDialog1.Execute then
      exit;
  DisposeObject(ft1);
  ft1 := nil;
  vl1.Clear;

  LoadMemoryBitmap(OpenDialog1.FileName, ort1);
  t1.Assign(ort1);
  t1.ReleaseFMXResource;

  TabControl1.ActiveTab := TabItem1;
end;

procedure TGaussPyramidsForm.Button2Click(Sender: TObject);
begin
  if not OpenDialog1.Execute then
      exit;
  DisposeObject(ft2);
  ft2 := nil;
  vl2.Clear;

  LoadMemoryBitmap(OpenDialog1.FileName, ort2);
  t2.Assign(ort2);
  t2.ReleaseFMXResource;

  TabControl1.ActiveTab := TabItem2;
end;

procedure TGaussPyramidsForm.Button3Click(Sender: TObject);
var
  mi: TArrayMatchInfo;
  f: TLFloat;
  mr: TMemoryRaster;
  dt: TTimeTick;
begin
  if ft1 = nil then
      exit;
  if ft2 = nil then
      exit;

  dt := GetTimeTick;
  f := MatchFeature(ft1, ft2, mi);
  DoStatus('分析特征相近度:%s 总共匹配了 %d 个相似特征', [FloatToStr(f), length(mi)]);
  DoStatus('分析特征所花费的时间:%dms ', [GetTimeTick - dt]);

  if length(mi) = 0 then
    begin
      DoStatus('没有相似特征');
      exit;
    end;

  dt := GetTimeTick;
  mr := BuildMatchInfoView(mi, Min((ft1.width + ft2.width) * 0.05, 5), False);
  DoStatus('生成特征分析视图所花费的时间:%dms ', [GetTimeTick - dt]);

  if mr <> nil then
    begin
      MemoryBitmapToBitmap(mr, ImageViewer1.Bitmap);
      DisposeObject(mr);
    end
  else
      DoStatus('没有相似特征');

  TabControl1.ActiveTab := TabItem3;
end;

procedure TGaussPyramidsForm.Button4Click(Sender: TObject);
var
  nvl: TVec2List;
  w, h: TLInt;
begin
  DisposeObject(ft1);
  nvl := TVec2List.Create;
  nvl.Assign(vl1);
  nvl.Transform(-b1[0, 0], -b1[0, 1]);
  w := ort1.width;
  h := ort1.Height;
  ComputeSamplerSize(w, h);
  nvl.Mul(w / Rectwidth(b1), h / RectHeight(b1));

  ft1 := TFeature.CreateWithRasterClip(ort1, nvl);
  ft1.LinkRaster := ort1;
  DisposeObject(t1);
  t1 := ft1.CreateFeatureViewer((ort1.width + ort1.Height) * 0.5 * 0.005, RasterColorF(1, 0, 0, 0.5)) as TDETexture;
end;

procedure TGaussPyramidsForm.Button5Click(Sender: TObject);
begin
  t1.Assign(ort1);
  t1.ReleaseFMXResource;
  vl1.Clear;
  DisposeObject(ft1);
  ft1 := nil;
end;

procedure TGaussPyramidsForm.Button6Click(Sender: TObject);
begin
  t2.Assign(ort2);
  t2.ReleaseFMXResource;
  vl2.Clear;
  DisposeObject(ft2);
  ft2 := nil;
end;

procedure TGaussPyramidsForm.Button7Click(Sender: TObject);
var
  nvl: TVec2List;
  w, h: TLInt;
begin
  DisposeObject(ft2);
  nvl := TVec2List.Create;
  nvl.Assign(vl2);
  nvl.Transform(-b2[0, 0], -b2[0, 1]);
  w := ort2.width;
  h := ort2.Height;
  ComputeSamplerSize(w, h);
  nvl.Mul(w / Rectwidth(b2), h / RectHeight(b2));

  ft2 := TFeature.CreateWithRasterClip(ort2, nvl);
  ft2.LinkRaster := ort2;
  DisposeObject(t2);
  t2 := ft2.CreateFeatureViewer((ort2.width + ort2.Height) * 0.5 * 0.005, RasterColorF(1, 1, 0, 0.5)) as TDETexture;
end;

constructor TGaussPyramidsForm.Create(AOwner: TComponent);
begin
  AddDoStatusHook(Self, DoStatusM);
  drawIntf := TDrawEngineInterface_FMX.Create;
  ft1 := nil;
  ft2 := nil;
  t1 := DefaultTextureClass.Create;
  t2 := DefaultTextureClass.Create;
  ort1 := DefaultTextureClass.Create;
  ort2 := DefaultTextureClass.Create;

  vl1 := TVec2List.Create;
  vl2 := TVec2List.Create;
  inherited;
end;

procedure TGaussPyramidsForm.DoStatusM(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TGaussPyramidsForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(Self);
  DisposeObject(drawIntf);
  DisposeObject([ort1, ort2]);
  DisposeObject([t1, t2]);
  DisposeObject([vl1, vl2]);
end;

procedure TGaussPyramidsForm.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  vl1.Add(Vec2(X, Y));
end;

procedure TGaussPyramidsForm.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
var
  d: TDrawEngine;
  i: Integer;
  pd: TPolyDrawOption;
begin
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);
  d.DrawOptions := [devpFPS];
  d.FillBox(d.ScreenRect, DEColor(0, 0, 0, 1));
  b1 := d.FitDrawTexture(t1, t1.BoundsRectV2, d.ScreenRect, 1.0);

  pd.LineColor := DEColor(1, 0, 0, 1);
  pd.PointColor := DEColor(1, 0.5, 0.5, 1);
  pd.LineWidth := 2;
  pd.PointScreenRadius := 10;
  d.DrawPLInScene(vl1, True, pd);

  d.DrawBox(b1, DEColor(1, 0, 0, 0.5), 2);
  d.Flush;
end;

procedure TGaussPyramidsForm.PaintBox1Resize(Sender: TObject);
begin
  vl1.Clear;
end;

procedure TGaussPyramidsForm.PaintBox2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  vl2.Add(Vec2(X, Y));
end;

procedure TGaussPyramidsForm.PaintBox2Paint(Sender: TObject; Canvas: TCanvas);
var
  d: TDrawEngine;
  pd: TPolyDrawOption;
begin
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);
  d.DrawOptions := [devpFPS];
  d.FillBox(d.ScreenRect, DEColor(0, 0, 0, 1));
  b2 := d.FitDrawTexture(t2, t2.BoundsRectV2, d.ScreenRect, 1.0);

  pd.LineColor := DEColor(1, 0, 0, 1);
  pd.PointColor := DEColor(1, 0.5, 0.5, 1);
  pd.LineWidth := 2;
  pd.PointScreenRadius := 10;
  d.DrawPLInScene(vl2, True, pd);

  d.DrawBox(b2, DEColor(1, 0, 0, 0.5), 2);
  d.Flush;
end;

procedure TGaussPyramidsForm.PaintBox2Resize(Sender: TObject);
begin
  vl2.Clear;
end;

procedure TGaussPyramidsForm.Timer1Timer(Sender: TObject);
begin
  EnginePool.Progress(Interval2Delta(Timer1.Interval));
  Invalidate;
end;

end.
