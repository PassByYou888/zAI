unit MemoryRasterMoreFontMainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, System.IOUtils,

  CoreClasses, UnicodeMixedLib, PascalStrings, Geometry2DUnit, MemoryRaster,
  zDrawEngine, zDrawEngineInterface_SlowFMX, FMX.Layouts, FMX.ExtCtrls;

type
  TMemoryRasterMoreFontMainForm = class(TForm)
    ImageViewer1: TImageViewer;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MemoryRasterMoreFontMainForm: TMemoryRasterMoreFontMainForm;

implementation

{$R *.fmx}


procedure TMemoryRasterMoreFontMainForm.FormCreate(Sender: TObject);

  procedure d(raster: TMemoryRaster; f: TFontRaster; var y: Integer);
  var
    siz: TVec2;
    n: string;
  begin
    raster.Font := f;
    n := 'ABC abc 123 (456+xyz) !@#$%^&*()-=';
    siz := raster.TextSize(n, f.FontSize);
    raster.DrawText(n, 10, y, Vec2(0.5, 0.5), 5, 1.0, f.FontSize, RColorF(1, 1, 1));
    inc(y, round(siz[1]) + 10);
  end;

var
  rfont1, rfont2, rfont3, rfont4, rfont5, rfont6: TFontRaster;
  raster: TMemoryRaster;
  y, h: Integer;
  f: TFontRaster;
begin
  rfont1 := TFontRaster.Create;
  rfont2 := TFontRaster.Create;
  rfont3 := TFontRaster.Create;
  rfont4 := TFontRaster.Create;
  rfont5 := TFontRaster.Create;
  rfont6 := TFontRaster.Create;

  // zFont文件使用FontBuild工具创建
  // zFont存放的是字体光栅数据，它有强大的通用性，在任何平台通用
  // demo使用的zfont不包含中文光栅，只包含了0-255字符
  rfont1.LoadFromFile(umlCombineFileName(TPath.GetLibraryPath, 'font_demo_1.zfont'));
  rfont2.LoadFromFile(umlCombineFileName(TPath.GetLibraryPath, 'font_demo_2.zfont'));
  rfont3.LoadFromFile(umlCombineFileName(TPath.GetLibraryPath, 'font_demo_3.zfont'));
  rfont4.LoadFromFile(umlCombineFileName(TPath.GetLibraryPath, 'font_demo_4.zfont'));
  rfont5.LoadFromFile(umlCombineFileName(TPath.GetLibraryPath, 'font_demo_5.zfont'));
  rfont6.LoadFromFile(umlCombineFileName(TPath.GetLibraryPath, 'font_demo_6.zfont'));

  raster := NewRaster;

  // 初始化光栅尺寸和背景
  raster.SetSize(1024, 512, RColor(0, 0, 0));

  // 从y坐标10开始画
  y := 50;

  // 把不同font画到光栅去
  // 这是优质光栅，比windows的字体更加圆润平滑
  d(raster, rfont1, y);
  d(raster, rfont2, y);
  d(raster, rfont3, y);
  d(raster, rfont4, y);
  d(raster, rfont5, y);
  d(raster, rfont6, y);

  // 把MemoryRaster光栅转换成FMX光栅
  MemoryBitmapToBitmap(raster, ImageViewer1.Bitmap);

  // 释放
  disposeObject(raster);
  disposeObject([rfont1, rfont2, rfont3, rfont4, rfont5, rfont6]);
end;

end.
