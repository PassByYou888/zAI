program DrawEngine4FMXBitmap;

{$APPTYPE CONSOLE}


uses
  System.StartUpCopy,
  System.IOUtils,
  FMX.Graphics,
  CoreClasses,
  PascalStrings,
  UnicodeMixedLib,
  DoStatusIO,
  Geometry2DUnit,
  MemoryRaster,
  zDrawEngine,
  zDrawEngineInterface_SlowFMX;

{$R *.res}


// dorender是softRenderer demo中的方法，我们直接将它粘贴过来使用
procedure DoRender(Draw: TDrawEngine);
begin
  if not Draw.ReadyOK then
      Exit;
  Draw.ViewOptions := [voEdge];
  Draw.SetSize;
  Draw.FillBox(Draw.ScreenRect, DEColor(0.5, 0.5, 0.5, 1));

  Draw.FillBox(RectV2(100, 100, 250, 250), -180, DEColor(1, 1, 1, 0.2));
  Draw.DrawBox(RectV2(49, 100, 151, 151), -15, DEColor(1, 0.5, 0.5, 0.9), 2);
  Draw.FillEllipse(DERect(50, 100, 300, 250), DEColor(1, 1, 1, 0.5));

  Draw.BeginCaptureShadow(Vec2(4, 4), 0.9);
  Draw.DrawText('|s:10,color(1,1,1,1)|Hello|color(1,1,1,1)| world' + #13#10 +
    '||' + #13#10 + 'default text |s:22,color(0,1,0,1)| big green' + #13#10 + 'big green line 2' + #13#10 + 'big green line 3',
    18, Draw.ScreenRect, DEColor(1, 0, 0, 0.8), True, DEVec(0.5, 0.5), -22);
  Draw.EndCaptureShadow;

  Draw.Flush;
end;

procedure RenderToFMXBitmap;
var
  bmp: FMX.Graphics.TBitmap;
  dIntf: TDrawEngineInterface_FMX;
  d: TDrawEngine;
begin
  bmp := FMX.Graphics.TBitmap.Create;
  bmp.SetSize(512, 512);

  // TDrawEngineInterface_FMX是DrawEngine绘图中间层，我们将它的输出指定为一张fmx内置的bmp位图
  dIntf := TDrawEngineInterface_FMX.Create;
  dIntf.SetSurface(bmp.Canvas, bmp);

  // drawEngine初始化
  d := TDrawEngine.Create;
  d.DrawInterface := dIntf;
  d.SetSize;

  // 画图
  // FMX在windows平台下Bitmap会默认使用d2d绘图api，天生支持反锯齿，并且具备硬件加速功能
  // bmp的硬件加速是指画图过程加速，待画图完成，系统会将gpu显存中的像素重新copy到bmp的光栅中，copy这一步都是很慢的
  DoRender(d);

  // 释放接口
  disposeObject(dIntf);
  disposeObject(d);

  // 将bmp保存下来便于我们观看
  bmp.SaveToFile(umlCombineFileName(TPath.GetLibraryPath, 'fmx_bitmap_demo_output.bmp'));
  DoStatus('FMX bitmap file ' + umlCombineFileName(TPath.GetLibraryPath, 'fmx_bitmap_demo_output.bmp'));
  disposeObject(bmp);
end;

begin
  RenderToFMXBitmap;
  DoStatus('press any key to exit.');
  readln;

end.
