unit GaussPyramidsFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.TabControl, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.Layouts, FMX.ListBox, FMX.Objects, FMX.StdCtrls, System.Math,

  FMX.Surfaces,

  CoreClasses, DoStatusIO, MemoryRaster, PascalStrings, ObjectDataManager, ItemStream,
  UnicodeMixedLib, Learn, LearnTypes, PyramidSpace, zDrawEngineInterface_SlowFMX;

type
  TGaussPyramidsForm = class(TForm)
    TabControl1: TTabControl;
    Memo1: TMemo;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    OpenDialog1: TOpenDialog;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Layout1: TLayout;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ft1, ft2: TFeature;
    procedure DoStatusM(AText: SystemString; const ID: Integer);
  end;

var
  GaussPyramidsForm: TGaussPyramidsForm;
  { 下列函数是从 zDrawEngine->FMX 接口拔出的 }
  { 因为zDrawEngine的体系有点巨大，懒于整理，不便开源 }

implementation

{$R *.fmx}


procedure TGaussPyramidsForm.Button1Click(Sender: TObject);
var
  mr: TMemoryRaster;
  dt: TTimeTick;
begin
  if not OpenDialog1.Execute then
      exit;
  if ft1 <> nil then
    begin
      DisposeObject(ft1.LinkRaster);
      DisposeObject(ft1);
    end;
  ft1 := nil;
  dt := GetTimeTick;
  ft1 := TFeature.CreateWithRasterFile(OpenDialog1.FileName);
  ft1.LinkRaster := NewRasterFromFile(OpenDialog1.FileName);
  DoStatus('提取 %s 特征所花费的时间:%dms 特征数:%d ', [ExtractFileName(OpenDialog1.FileName), GetTimeTick - dt, ft1.Count]);

  dt := GetTimeTick;
  mr := ft1.CreateFeatureViewer((ft1.LinkRaster.width + ft1.LinkRaster.height) * 0.5 * 0.005, RasterColorF(1, 1, 0, 0.5));
  DoStatus('生成 %s 视图所花费的时间:%dms ', [ExtractFileName(OpenDialog1.FileName), GetTimeTick - dt]);

  MemoryBitmapToBitmap(mr, Image1.Bitmap);
  DisposeObject(mr);

  TabControl1.ActiveTab := TabItem1;
end;

procedure TGaussPyramidsForm.Button2Click(Sender: TObject);
var
  mr: TMemoryRaster;
  dt: TTimeTick;
begin
  if not OpenDialog1.Execute then
      exit;
  if ft2 <> nil then
    begin
      DisposeObject(ft2.LinkRaster);
      DisposeObject(ft2);
    end;
  ft2 := nil;
  dt := GetTimeTick;
  ft2 := TFeature.CreateWithRasterFile(OpenDialog1.FileName);
  ft2.LinkRaster := NewRasterFromFile(OpenDialog1.FileName);
  DoStatus('提取 %s 特征所花费的时间:%dms 特征数:%d ', [ExtractFileName(OpenDialog1.FileName), GetTimeTick - dt, ft2.Count]);

  dt := GetTimeTick;
  mr := ft2.CreateFeatureViewer((ft2.LinkRaster.width + ft2.LinkRaster.height) * 0.5 * 0.005, RasterColorF(0, 1, 1, 0.5));
  DoStatus('生成 %s 视图所花费的时间:%dms ', [ExtractFileName(OpenDialog1.FileName), GetTimeTick - dt]);

  MemoryBitmapToBitmap(mr, Image2.Bitmap);
  DisposeObject(mr);
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
  mr := BuildMatchInfoView(mi, Min((ft1.width + ft2.width) * 0.05,3), False);
  DoStatus('生成特征分析视图所花费的时间:%dms ', [GetTimeTick - dt]);

  if mr <> nil then
    begin
      MemoryBitmapToBitmap(mr, Image3.Bitmap);
      DisposeObject(mr);
    end
  else
      DoStatus('没有相似特征');

  TabControl1.ActiveTab := TabItem3;
end;

procedure TGaussPyramidsForm.DoStatusM(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TGaussPyramidsForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusM);
  ft1 := nil;
  ft2 := nil;
end;

procedure TGaussPyramidsForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(Self);
end;

initialization

finalization

end.
