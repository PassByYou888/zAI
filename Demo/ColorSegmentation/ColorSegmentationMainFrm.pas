unit ColorSegmentationMainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, System.IOUtils,

  CoreClasses, UnicodeMixedLib, PascalStrings, Geometry2DUnit, MemoryRaster,
  zDrawEngine, zDrawEngineInterface_SlowFMX;

type
  TColorSegmentationMainForm = class(TForm)
    segListPB: TPaintBox;
    segPB: TPaintBox;
    Timer1: TTimer;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure segListPBPaint(Sender: TObject; Canvas: TCanvas);
    procedure segPBMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure segPBPaint(Sender: TObject; Canvas: TCanvas);
    procedure Timer1Timer(Sender: TObject);
  private
    drawIntf: TDrawEngineInterface_FMX;
  public
    tex: TMemoryRaster;
    tex_box: TRectV2;
    colors: TRColors;
    pickColor: TRColor;
    SegImgList: TMemoryRasterList;

    // 按阈值裁剪相近颜色
    function RemoveColor(c: TRColor): Boolean;

    // 输入给的分割器的颜色分类ID
    procedure DoSegColor(Color: TRColor; var Classify: TSegClassify);

    // 构建分隔
    procedure BuildSeg;
  end;

var
  ColorSegmentationMainForm: TColorSegmentationMainForm;
  color_threshold: TGeoFloat = 0.4;

implementation

{$R *.fmx}


procedure TColorSegmentationMainForm.FormCreate(Sender: TObject);
begin
  drawIntf := TDrawEngineInterface_FMX.Create;
  tex := NewRasterFromFile(umlCombineFileName(TPath.GetLibraryPath, 'ColorSeg1.bmp'));
  colors := AnalysisColors(tex, nil, 65535);
  SegImgList := TMemoryRasterList.Create;

  TComputeThread.RunP(nil, nil, procedure(Sender: TComputeThread)
    var
      i: Integer;
    begin
      DrawPool(segPB).PostScrollText(15, Format('发现 |s:12,color(1,0,0)|%d||种颜色 正在均值化处理...', [colors.count]),
        16, DEColor(0.5, 1.0, 0.5));
      while i < colors.count do
        begin
          if RemoveColor(colors[i]) then
              i := 0
          else
              inc(i);
        end;
      RemoveColor(RColor(0, 0, 0));
      colors.Remove(RColor(0, 0, 0));
      DrawPool(segPB).PostScrollText(15, Format('均值化以后剩余 |s:12,color(1,0,0)|%d||种颜色...', [colors.count]),
        16, DEColor(0.5, 1.0, 0.5));
    end);
end;

procedure TColorSegmentationMainForm.segListPBPaint(Sender: TObject; Canvas: TCanvas);
var
  d: TDrawEngine;
  r: TRectV2;
  offset: TVec2;
  s: TGeoFloat;
begin
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);
  d.ViewOptions := [voEdge];

  d.FillBox(d.ScreenRect, DEColor(0.2, 0.2, 0.2));

  LockObject(SegImgList);
  d.BeginCaptureShadow(Vec2(-5, 5), 0.9);
  r := d.DrawPicturePackingInScene(SegImgList, 10, Vec2(0, 0), 1.0);
  d.EndCaptureShadow;
  UnLockObject(SegImgList);

  FitScale(r, d.ScreenRect, offset, s);
  d.offset := offset;
  d.Scale := s;

  d.DrawText('像素分割后', 16, d.ScreenRect, DEColor(0.5, 1.0, 0.5), False);
  d.Flush;
end;

procedure TColorSegmentationMainForm.segPBMouseUp(Sender: TObject; Button:
  TMouseButton; Shift: TShiftState; X, Y: Single);
var
  pt: TVec2;
  i: Integer;
  c: TRColorEntry;
begin
  LockObject(SegImgList);
  for i := 0 to SegImgList.count - 1 do
      DisposeObject(SegImgList[i]);
  SegImgList.Clear;
  UnLockObject(SegImgList);

  pt := RectProjection(tex_box, tex.BoundsRectV2, Vec2(X, Y));
  if PointInRect(pt, tex.BoundsRectV2) then
    begin

      pickColor := tex.PixelVec[pt];

      if RColorDistance(pickColor, RColor(0, 0, 0)) < color_threshold then
        begin
          DrawPool(segPB).PostScrollText(5, '不能拾取黑色', 24, DEColor(1, 0, 0));
          exit;
        end;

      c.BGRA := pickColor;

      DrawPool(segPB).PostScrollText(5, Format('正在分割颜色|color(%d,%d,%d)|(%d,%d,%d)||' + #13#10, [c.r, c.G, c.B, c.r, c.G, c.B]),
        24, DEColor(1.0, 1.0, 1.0));

      BuildSeg;
    end;
end;

procedure TColorSegmentationMainForm.segPBPaint(Sender: TObject; Canvas: TCanvas);
var
  d: TDrawEngine;
  n: U_String;
  i: Integer;
  c: TRColorEntry;
begin
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);
  d.ViewOptions := [voEdge];

  d.FillBox(d.ScreenRect, DEColor(0.5, 0.5, 0.5));

  tex_box := d.FitDrawPicture(tex, tex.BoundsRectV2, RectEdge(d.ScreenRect, -20), 1.0);
  d.DrawDotLineBox(tex_box, Vec2(0.5, 0.5), 0, DEColor(0.8, 0.1, 0.4), 3);

  n := '';
  LockObject(colors);
  for i := 0 to colors.count - 1 do
    begin
      c.BGRA := colors[i];
      n.Append(Format('||猜测结果 |color(%d,%d,%d)|(%d,%d,%d)||' + #13#10, [c.r, c.G, c.B, c.r, c.G, c.B]));
    end;
  d.DrawText('|s:16|选择一种颜色' + #13#10 + n, 12, d.ScreenRect, DEColor(0.5, 1.0, 0.5), False);
  UnLockObject(colors);

  d.Flush;
end;

procedure TColorSegmentationMainForm.Timer1Timer(Sender: TObject);
begin
  EnginePool.Progress(Interval2Delta(Timer1.Interval));
  Invalidate;
end;

function TColorSegmentationMainForm.RemoveColor(c: TRColor): Boolean;
var
  i: Integer;
begin
  Result := False;
  i := 0;
  LockObject(colors);
  while i < colors.count do
    begin
      // 按阈值裁剪相近颜色
      if (colors[i] <> c) and (RColorDistance(c, colors[i]) < color_threshold) then
        begin
          colors.Delete(i);
          Result := True;
        end
      else
          inc(i);
    end;
  UnLockObject(colors);
end;

procedure TColorSegmentationMainForm.DoSegColor(Color: TRColor; var Classify: TSegClassify);
var
  i: Integer;
begin
  // 输入给的分割器的颜色分类ID
  // Classify给 0 表示这个颜色不必分割，如果大于0的话，分割器会按Classify进行分类
  Classify := 0;
  if RColorDistance(Color, RColor(0, 0, 0)) < color_threshold then
      exit;

  // 如果拾取的颜色范围在阈值color_threshold内就告诉分割器，这是 Classify 的1分类
  if RColorDistance(pickColor, Color) < color_threshold then
      Classify := 1;
end;

procedure TColorSegmentationMainForm.BuildSeg;
begin
  TComputeThread.RunP(nil, nil, procedure(ThSender: TComputeThread)
    var
      s: TColorSegmentation;
      first_total, i, j, k: Integer;
      sp: TSegPool;
      nm: TMemoryRaster;
    begin
      s := TColorSegmentation.Create(tex);

      // 分割器的颜色分类接口
      s.OnSegColor := DoSegColor;

      // 执行分割
      s.BuildSegmentation;

      // 记录一下首次分割总数
      first_total := s.count;

      // 构建分割以后会出现噪音，这里我们将噪音移出
      // 阈值100表示分割碎片块像素总和如果低于100个
      s.RemoveNoise(100);

      for i := 0 to s.count - 1 do
        begin
          sp := s[i];
          // BuildDatamap方法会将分割数据投影到一个新光栅中
          nm := sp.BuildClipDatamap(RColor(0, 0, 0, 0), pickColor);
          // 在显示的分割结果上给分割图形画上边框
          nm.FillNoneBGColorBorder(RColor(0, 0, 0, 0), RColorInv(pickColor), 4);

          LockObject(SegImgList);
          SegImgList.Add(nm);
          UnLockObject(SegImgList);
        end;

      DrawPool(segPB).PostScrollText(5,
        Format('分割报告:检测到 |s:16,color(1,0,0)|%d|| 个像素图形(包含像素碎片图形),而实际有效只有 |s:16,color(1,0,0)|%d|| 个图形', [first_total, SegImgList.count]),
        20, DEColor(1, 1, 1));
      DisposeObject(s);
    end);
end;

end.
