{ ****************************************************************************** }
{ * draw engine for FMX                                                        * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }
unit zDrawEngineInterface_SlowFMX;

{$INCLUDE ..\zDefine.inc}

interface

uses System.Math.Vectors, System.Math,
  FMX.Forms,
  FMX.Graphics, System.UITypes, System.Types, FMX.Types, FMX.Controls,
  FMX.Types3D, FMX.Surfaces, System.UIConsts, Geometry3DUnit, ListEngine,
  PascalStrings,
  CoreClasses, zDrawEngine, UnicodeMixedLib, Geometry2DUnit, MemoryRaster;

type
  TDrawEngineInterface_FMX = class(TDrawEngineInterface)
  private
    FCanvas: TCanvas;
    FOwnerCanvasScale: TDEFloat;
    FLineWidth: TDEFloat;
    FCanvasSave: TCanvasSaveState;
    FDebug: Boolean;
    FCurrSiz: TDEVec;

    procedure SetCanvas(const Value: TCanvas);
  public
    procedure SetSize(r: TDERect); override;
    procedure SetLineWidth(w: TDEFloat); override;
    procedure DrawDotLine(pt1, pt2: TDEVec; COLOR: TDEColor); override;
    procedure DrawLine(pt1, pt2: TDEVec; COLOR: TDEColor); override;
    procedure DrawRect(r: TDERect; angle: TDEFloat; COLOR: TDEColor); override;
    procedure FillRect(r: TDERect; angle: TDEFloat; COLOR: TDEColor); override;
    procedure DrawEllipse(r: TDERect; COLOR: TDEColor); override;
    procedure FillEllipse(r: TDERect; COLOR: TDEColor); override;
    procedure FillPolygon(PolygonBuff: TArrayVec2; COLOR: TDEColor); override;
    procedure DrawText(Shadow: Boolean; Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean; RotateVec: TDEVec; angle: TDEFloat); override;
    procedure DrawPicture(Shadow: Boolean; t: TCoreClassObject; sour, dest: TDE4V; alpha: TDEFloat); override;
    procedure Flush; override;
    procedure ResetState; override;
    procedure BeginDraw; override;
    procedure EndDraw; override;
    function CurrentScreenSize: TDEVec; override;
    function GetTextSize(const Text: SystemString; Size: TDEFloat): TDEVec; override;
    function ReadyOK: Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetSurface(c: TCanvas; OwnerCtrl: TObject);
    property Canvas: TCanvas read FCanvas write SetCanvas;

    property Debug: Boolean read FDebug write FDebug;
    // only work in mobile device and gpu fast mode
    property OwnerCanvasScale: TDEFloat read FOwnerCanvasScale write FOwnerCanvasScale;
    property CanvasScale: TDEFloat read FOwnerCanvasScale write FOwnerCanvasScale;
    property ScreenSize: TDEVec read FCurrSiz;
  end;

  TDETexture_FMX = class(TDETexture)
  protected
    FTexture: TBitmap;
    function GetTexture: TBitmap;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ReleaseGPUMemory; override;
    procedure SyncTextureMemory;

    property Texture: TBitmap read GetTexture;
  end;

  TResourceTexture = class(TDETexture_FMX)
  protected
    FLastLoadFile: SystemString;
  public
    constructor Create; overload; override;
    constructor Create(FileName: SystemString); overload; virtual;

    procedure LoadFromFileIO(FileName: SystemString);
    property LastLoadFile: SystemString read FLastLoadFile;
  end;

  TResourceTextureIntf = class(TCoreClassInterfacedObject)
  public
    Texture: TResourceTexture;
    TextureRect: TDERect;
    SizeScale: TDEVec;

    constructor Create(tex: TResourceTexture); virtual;
    destructor Destroy; override;
    function SizeOfVec: TDEVec;
    procedure ChangeTexture(tex: TResourceTexture); virtual;
  end;

  TResourceTextureCache = class(TCoreClassObject)
  protected
    TextureList: THashObjectList;
  public
    DefaultTexture: TResourceTexture;

    constructor Create; virtual;
    destructor Destroy; override;
    function CreateResourceTexture(FileName: SystemString): TResourceTextureIntf;
    procedure ReleaseAllFMXRsource;
  end;

function c2c(c: TDEColor): TAlphaColor; inline; overload;
function c2c(c: TAlphaColor): TDEColor; inline; overload;
function p2p(pt: TDEVec): TPointf; inline; overload;
function r2r(r: TDERect): TRectf; inline; overload;
function r2r(r: TRect): TRectf; inline; overload;
function AlphaColor2RasterColor(c: TAlphaColor): TRasterColor; inline;
function DE4V2Corners(sour: TDE4V): TCornersF; inline;
function DEColor(c: TAlphaColor): TDEColor; inline; overload;
function PrepareColor(const SrcColor: TAlphaColor; const Opacity: TDEFloat): TAlphaColor; inline;
procedure MakeMatrixRotation(angle, width, height, x, y, RotationCenter_X, RotationCenter_Y: TDEFloat; var OutputMatrix: TMatrix; var OutputRect: TRectf); inline;

procedure MemoryBitmapToSurface(raster: TMemoryRaster; Surface: TBitmapSurface); overload;
procedure MemoryBitmapToSurface(raster: TMemoryRaster; sourRect: TRect; Surface: TBitmapSurface); overload;
procedure SurfaceToMemoryBitmap(Surface: TBitmapSurface; raster: TMemoryRaster);
procedure MemoryBitmapToBitmap(raster: TMemoryRaster; bmp: TBitmap); overload;
procedure MemoryBitmapToBitmap(raster: TMemoryRaster; sourRect: TRect; bmp: TBitmap); overload;
procedure BitmapToMemoryBitmap(bmp: TBitmap; raster: TMemoryRaster);

function CanLoadMemoryBitmap(f: SystemString): Boolean;
procedure LoadMemoryBitmap(f: SystemString; raster: TMemoryRaster); overload;
procedure LoadMemoryBitmap(f: SystemString; raster: TSequenceMemoryRaster); overload;
procedure LoadMemoryBitmap(f: SystemString; raster: TDETexture); overload;
procedure LoadMemoryBitmap(stream: TCoreClassStream; raster: TMemoryRaster); overload;

procedure SaveMemoryBitmap(f: SystemString; raster: TMemoryRaster); overload;
procedure SaveMemoryBitmap(raster: TMemoryRaster; fileExt: SystemString; DestStream: TCoreClassStream); overload;
procedure SaveMemoryBitmap(raster: TSequenceMemoryRaster; fileExt: SystemString; DestStream: TCoreClassStream); overload;

var
  // resource texture cache
  TextureCache: TResourceTextureCache = nil;

implementation

uses
{$IF Defined(ANDROID) or Defined(IOS)}
  FMX.Canvas.GPU, FMX.TextLayout.GPU, FMX.StrokeBuilder, FMX.Canvas.GPU.Helpers,
{$ENDIF}
  DoStatusIO, MemoryStream64, MediaCenter;

function c2c(c: TDEColor): TAlphaColor;
begin
  Result := TAlphaColorF.Create(c[0], c[1], c[2], 1.0).ToAlphaColor;
end;

function c2c(c: TAlphaColor): TDEColor;
begin
  with TAlphaColorF.Create(c) do
      Result := DEColor(r, g, b, 1.0);
end;

function p2p(pt: TDEVec): TPointf;
begin
  Result := Point2Pointf(pt);
end;

function r2r(r: TDERect): TRectf;
begin
  Result := MakeRectf(r);
end;

function r2r(r: TRect): TRectf;
begin
  Result := TRectf.Create(r);
end;

function AlphaColor2RasterColor(c: TAlphaColor): TRasterColor;
var
  ce: TRasterColorEntry;
begin
  ce.r := TAlphaColorRec(c).r;
  ce.g := TAlphaColorRec(c).g;
  ce.b := TAlphaColorRec(c).b;
  ce.a := TAlphaColorRec(c).a;
  Result := ce.BGRA;
end;

function DE4V2Corners(sour: TDE4V): TCornersF;
begin
  with TV2Rect4.Init(sour.MakeRectV2, sour.angle) do
    begin
      Result[0] := Point2Pointf(LeftTop);
      Result[1] := Point2Pointf(RightTop);
      Result[2] := Point2Pointf(RightBottom);
      Result[3] := Point2Pointf(LeftBottom);
    end;
end;

function DEColor(c: TAlphaColor): TDEColor;
begin
  with TAlphaColorF.Create(c) do
      Result := DEColor(r, g, b, a);
end;

function PrepareColor(const SrcColor: TAlphaColor; const Opacity: TDEFloat): TAlphaColor;
begin
  if Opacity <= 1.0 then
    begin
      TAlphaColorRec(Result).r := Round(TAlphaColorRec(SrcColor).r * Opacity);
      TAlphaColorRec(Result).g := Round(TAlphaColorRec(SrcColor).g * Opacity);
      TAlphaColorRec(Result).b := Round(TAlphaColorRec(SrcColor).b * Opacity);
      TAlphaColorRec(Result).a := Round(TAlphaColorRec(SrcColor).a * Opacity);
    end
  else if (TAlphaColorRec(SrcColor).a < $FF) then
      Result := PremultiplyAlpha(SrcColor)
  else
      Result := SrcColor;
end;

procedure MakeMatrixRotation(angle, width, height, x, y, RotationCenter_X, RotationCenter_Y: TDEFloat; var OutputMatrix: TMatrix; var OutputRect: TRectf); inline;
const
  Scale_X = 1.0;
  Scale_Y = 1.0;
var
  ScaleMatrix, rotMatrix, m1, m2: TMatrix;
begin
  ScaleMatrix := TMatrix.identity;
  ScaleMatrix.m11 := Scale_X;
  ScaleMatrix.m22 := Scale_Y;
  OutputMatrix := ScaleMatrix;

  m1 := TMatrix.identity;
  m1.m31 := -(RotationCenter_X * width * Scale_X + x);
  m1.m32 := -(RotationCenter_Y * height * Scale_Y + y);
  m2 := TMatrix.identity;
  m2.m31 := RotationCenter_X * width * Scale_X + x;
  m2.m32 := RotationCenter_Y * height * Scale_Y + y;
  rotMatrix := m1 * (TMatrix.CreateRotation(DegToRad(angle)) * m2);
  OutputMatrix := OutputMatrix * rotMatrix;

  OutputRect.TopLeft := Pointf(x, y);
  OutputRect.BottomRight := Pointf(x + width, y + height);
end;

procedure MemoryBitmapToSurface(raster: TMemoryRaster; Surface: TBitmapSurface);
var
  y, x: Integer;
  p1, p2: PCardinal;
  c: TRasterColorEntry;
begin
  try
{$IF Defined(ANDROID) or Defined(IOS)}
    Surface.SetSize(raster.width, raster.height, TPixelFormat.RGBA);
{$ELSE}
    Surface.SetSize(raster.width, raster.height, TPixelFormat.BGRA);
{$ENDIF}
    raster.ReadyBits;
    for y := 0 to Surface.height - 1 do
      begin
        p1 := PCardinal(raster.ScanLine[y]);
        p2 := PCardinal(Surface.ScanLine[y]);
        for x := 0 to raster.width - 1 do
          begin
{$IF Defined(ANDROID) or Defined(IOS) or Defined(OSX)}
            c.BGRA := RGBA2BGRA(TRasterColor(p1^));
{$ELSE}
            c.BGRA := TRasterColor(p1^);
{$IFEND}
            with TAlphaColorRec(p2^) do
              begin
                r := c.r;
                g := c.g;
                b := c.b;
                a := c.a;
              end;
            inc(p1);
            inc(p2);
          end;
      end;
  except
  end;
end;

procedure MemoryBitmapToSurface(raster: TMemoryRaster; sourRect: TRect; Surface: TBitmapSurface);
var
  nb: TMemoryRaster;
begin
  nb := TMemoryRaster.Create;
  nb.DrawMode := dmBlend;
  nb.SetSize(sourRect.width, sourRect.height, RasterColor(0, 0, 0, 0));
  raster.ReadyBits;
  raster.DrawTo(nb, 0, 0, sourRect);
  MemoryBitmapToSurface(nb, Surface);
  DisposeObject(nb);
end;

procedure SurfaceToMemoryBitmap(Surface: TBitmapSurface; raster: TMemoryRaster);
var
  y, x: Integer;
begin
  try
    raster.SetSize(Surface.width, Surface.height);
    for y := 0 to Surface.height - 1 do
      begin
        for x := 0 to Surface.width - 1 do
          with TAlphaColorRec(Surface.pixels[x, y]) do
              raster.Pixel[x, y] := RasterColor(r, g, b, a)
      end;
  except
  end;
end;

procedure MemoryBitmapToBitmap(raster: TMemoryRaster; bmp: TBitmap);
var
  Surface: TBitmapSurface;
begin
  Surface := TBitmapSurface.Create;
  MemoryBitmapToSurface(raster, Surface);
  bmp.Assign(Surface);
  DisposeObject(Surface);
end;

procedure MemoryBitmapToBitmap(raster: TMemoryRaster; sourRect: TRect; bmp: TBitmap);
var
  Surface: TBitmapSurface;
begin
  Surface := TBitmapSurface.Create;
  MemoryBitmapToSurface(raster, sourRect, Surface);
  bmp.Assign(Surface);
  DisposeObject(Surface);
end;

procedure BitmapToMemoryBitmap(bmp: TBitmap; raster: TMemoryRaster);
var
  Surface: TBitmapSurface;
  BitmapData: TBitmapData;
  i: Integer;
begin
  if bmp.BytesPerPixel = 4 then
    begin
      raster.SetSize(bmp.width, bmp.height);

      if bmp.Map(TMapAccess.Read, BitmapData) then
        for i := 0 to bmp.height - 1 do
            CopyPtr(BitmapData.GetScanline(i), raster.ScanLine[i], BitmapData.BytesPerLine);
      bmp.Unmap(BitmapData);

      if bmp.PixelFormat = TPixelFormat.RGBA then
          raster.FormatBGRA;
    end;
end;

function CanLoadMemoryBitmap(f: SystemString): Boolean;
begin
  try
      Result := TMemoryRaster.CanLoadFile(f) or TBitmapCodecManager.CodecExists(f);
  except
      Result := False;
  end;
end;

procedure LoadMemoryBitmap(f: SystemString; raster: TMemoryRaster);
var
  Surf: TBitmapSurface;
begin
  if raster.CanLoadFile(f) then
    begin
      raster.LoadFromFile(f);
    end
  else
    begin
      Surf := TBitmapSurface.Create;
      try
        if TBitmapCodecManager.LoadFromFile(f, Surf, TCanvasManager.DefaultCanvas.GetAttribute(TCanvasAttribute.MaxBitmapSize)) then
            SurfaceToMemoryBitmap(Surf, raster);
      finally
          DisposeObject(Surf);
      end;
    end;
end;

procedure LoadMemoryBitmap(stream: TCoreClassStream; raster: TMemoryRaster);
var
  Surf: TBitmapSurface;
begin
  if raster.CanLoadStream(stream) then
    begin
      raster.LoadFromStream(stream);
    end
  else
    begin
      Surf := TBitmapSurface.Create;
      try
        if TBitmapCodecManager.LoadFromStream(stream, Surf, TCanvasManager.DefaultCanvas.GetAttribute(TCanvasAttribute.MaxBitmapSize)) then
            SurfaceToMemoryBitmap(Surf, raster);
      finally
          DisposeObject(Surf);
      end;
    end;
end;

procedure LoadMemoryBitmap(f: SystemString; raster: TSequenceMemoryRaster);
begin
  if raster.CanLoadFile(f) then
      raster.LoadFromFile(f)
  else
      LoadMemoryBitmap(f, TMemoryRaster(raster));
end;

procedure LoadMemoryBitmap(f: SystemString; raster: TDETexture);
begin
  LoadMemoryBitmap(f, TSequenceMemoryRaster(raster));
  raster.ReleaseGPUMemory;
end;

procedure SaveMemoryBitmap(f: SystemString; raster: TMemoryRaster);
var
  Surf: TBitmapSurface;
begin
  if umlMultipleMatch(['*.bmp'], f) then
      raster.SaveToFile(f)
  else if umlMultipleMatch(['*.jpg', '*.jpeg'], f) then
      raster.SaveToJpegYCbCrFile(f, 90)
  else if umlMultipleMatch(['*.seq'], f) then
      raster.SaveToZLibCompressFile(f)
  else
    begin
      Surf := TBitmapSurface.Create;
      try
        MemoryBitmapToSurface(raster, Surf);
        TBitmapCodecManager.SaveToFile(f, Surf, nil);
      finally
          DisposeObject(Surf);
      end;
    end;
end;

procedure SaveMemoryBitmap(raster: TMemoryRaster; fileExt: SystemString; DestStream: TCoreClassStream);
var
  Surf: TBitmapSurface;
begin
  if umlMultipleMatch(['.bmp'], fileExt) then
      raster.SaveToBmp32Stream(DestStream)
  else
    begin
      Surf := TBitmapSurface.Create;
      try
        MemoryBitmapToSurface(raster, Surf);
        TBitmapCodecManager.SaveToStream(DestStream, Surf, fileExt);
      finally
          DisposeObject(Surf);
      end;
    end;
end;

procedure SaveMemoryBitmap(raster: TSequenceMemoryRaster; fileExt: SystemString; DestStream: TCoreClassStream);
var
  Surf: TBitmapSurface;
begin
  if umlMultipleMatch(['.bmp'], fileExt) then
      raster.SaveToBmp32Stream(DestStream)
  else if umlMultipleMatch(['.seq'], fileExt) then
      raster.SaveToStream(DestStream)
  else
    begin
      Surf := TBitmapSurface.Create;
      try
        MemoryBitmapToSurface(raster, Surf);
        TBitmapCodecManager.SaveToStream(DestStream, Surf, fileExt);
      finally
          DisposeObject(Surf);
      end;
    end;
end;

procedure TDrawEngineInterface_FMX.SetCanvas(const Value: TCanvas);
begin
  if Value = nil then
    begin
      FCanvas := nil;
      Exit;
    end;

  FCanvas := Value;
  FCurrSiz := DEVec(FCanvas.width, FCanvas.height);
end;

procedure TDrawEngineInterface_FMX.SetSize(r: TDERect);
begin
  FCanvas.IntersectClipRect(MakeRectf(r));
end;

procedure TDrawEngineInterface_FMX.SetLineWidth(w: TDEFloat);
begin
  if not IsEqual(FLineWidth, w) then
    begin
      FLineWidth := w;
      FCanvas.Stroke.Thickness := w;
    end;
end;

procedure TDrawEngineInterface_FMX.DrawDotLine(pt1, pt2: TDEVec; COLOR: TDEColor);
begin
  FCanvas.Stroke.Dash := TStrokeDash.Dot;
  FCanvas.Stroke.COLOR := c2c(COLOR);
  FCanvas.fill.COLOR := FCanvas.Stroke.COLOR;
  FCanvas.DrawLine(p2p(pt1), p2p(pt2), COLOR[3]);
end;

procedure TDrawEngineInterface_FMX.DrawLine(pt1, pt2: TDEVec; COLOR: TDEColor);
begin
  FCanvas.Stroke.Dash := TStrokeDash.Solid;
  FCanvas.Stroke.COLOR := c2c(COLOR);
  FCanvas.fill.COLOR := FCanvas.Stroke.COLOR;
  FCanvas.DrawLine(p2p(pt1), p2p(pt2), COLOR[3]);
end;

procedure TDrawEngineInterface_FMX.DrawRect(r: TDERect; angle: TDEFloat; COLOR: TDEColor);
var
  M, bak: TMatrix;
  rf: TRectf;
begin
  if angle <> 0 then
    begin
      bak := FCanvas.Matrix;
      MakeMatrixRotation(angle, RectWidth(r), RectHeight(r),
        MinF(r[0, 0], r[1, 0]), MinF(r[0, 1], r[1, 1]), 0.5, 0.5, M, rf);
      FCanvas.MultiplyMatrix(M);
    end;

  FCanvas.Stroke.Dash := TStrokeDash.Solid;
  FCanvas.Stroke.COLOR := c2c(COLOR);
  FCanvas.fill.COLOR := FCanvas.Stroke.COLOR;
  FCanvas.DrawRect(r2r(r), 0, 0, [], COLOR[3]);

  if angle <> 0 then
      FCanvas.SetMatrix(bak);
end;

procedure TDrawEngineInterface_FMX.FillRect(r: TDERect; angle: TDEFloat; COLOR: TDEColor);
var
  M, bak: TMatrix;
  rf: TRectf;
begin
  if angle <> 0 then
    begin
      bak := FCanvas.Matrix;
      MakeMatrixRotation(angle, RectWidth(r), RectHeight(r),
        MinF(r[0, 0], r[1, 0]), MinF(r[0, 1], r[1, 1]), 0.5, 0.5, M, rf);
      FCanvas.MultiplyMatrix(M);
    end;

  FCanvas.Stroke.Dash := TStrokeDash.Solid;
  FCanvas.Stroke.COLOR := c2c(COLOR);
  FCanvas.fill.COLOR := FCanvas.Stroke.COLOR;
  FCanvas.FillRect(r2r(r), 0, 0, [], COLOR[3]);

  if angle <> 0 then
      FCanvas.SetMatrix(bak);
end;

procedure TDrawEngineInterface_FMX.DrawEllipse(r: TDERect; COLOR: TDEColor);
begin
  FCanvas.Stroke.Dash := TStrokeDash.Solid;
  FCanvas.Stroke.COLOR := c2c(COLOR);
  FCanvas.fill.COLOR := FCanvas.Stroke.COLOR;
  FCanvas.DrawEllipse(r2r(r), COLOR[3]);
end;

procedure TDrawEngineInterface_FMX.FillEllipse(r: TDERect; COLOR: TDEColor);
begin
  FCanvas.Stroke.Dash := TStrokeDash.Solid;
  FCanvas.Stroke.COLOR := c2c(COLOR);
  FCanvas.fill.COLOR := FCanvas.Stroke.COLOR;
  FCanvas.FillEllipse(r2r(r), COLOR[3]);
end;

procedure TDrawEngineInterface_FMX.FillPolygon(PolygonBuff: TArrayVec2; COLOR: TDEColor);
var
  polygon_: TPolygon;
  i: Integer;
begin
  FCanvas.Stroke.Dash := TStrokeDash.Solid;
  FCanvas.Stroke.COLOR := c2c(COLOR);
  FCanvas.fill.COLOR := FCanvas.Stroke.COLOR;

  SetLength(polygon_, length(PolygonBuff));
  for i := 0 to length(PolygonBuff) - 1 do
      polygon_[i] := p2p(PolygonBuff[i]);

  FCanvas.FillPolygon(polygon_, COLOR[3]);
end;

procedure TDrawEngineInterface_FMX.DrawText(Shadow: Boolean; Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean; RotateVec: TDEVec; angle: TDEFloat);
var
  M, bak: TMatrix;
  rf: TRectf;
  TA: TTextAlign;
begin
  if angle <> 0 then
    begin
      bak := FCanvas.Matrix;
      MakeMatrixRotation(angle, RectWidth(r), RectHeight(r),
        MinF(r[0, 0], r[1, 0]), MinF(r[0, 1], r[1, 1]), RotateVec[0], RotateVec[1], M, rf);
      FCanvas.MultiplyMatrix(M);
    end;

  FCanvas.Stroke.Dash := TStrokeDash.Solid;
  FCanvas.Stroke.COLOR := c2c(COLOR);
  FCanvas.fill.COLOR := FCanvas.Stroke.COLOR;
  FCanvas.Font.Size := Size;

  if center then
      TA := TTextAlign.center
  else
      TA := TTextAlign.Leading;

  FCanvas.FillText(r2r(r), Text, False, COLOR[3], [], TA, TA);

  if angle <> 0 then
      FCanvas.SetMatrix(bak);
end;

procedure TDrawEngineInterface_FMX.DrawPicture(Shadow: Boolean; t: TCoreClassObject; sour, dest: TDE4V; alpha: TDEFloat);
var
  newSour, newDest: TDE4V;
  M, bak: TMatrix;
  r: TRectf;
begin
  newSour := sour;
  newDest := dest;

  if (t is TDETexture_FMX) then
    begin
      if (TDETexture_FMX(t).width = 0) or (TDETexture_FMX(t).height = 0) then
          Exit;
      if newSour.IsZero then
          newSour := TDE4V.Init(TDETexture_FMX(t).BoundsRectV2, 0);
      if newDest.IsZero then
          newDest := newSour;
      if not IsEqual(newDest.angle, 0, 0.01) then
        begin
          bak := FCanvas.Matrix;
          MakeMatrixRotation(newDest.angle, newDest.width, newDest.height,
            MinF(newDest.Left, newDest.Right), MinF(newDest.Top, newDest.Bottom), 0.5, 0.5, M, r);
          FCanvas.MultiplyMatrix(M);
          FCanvas.DrawBitmap(TDETexture_FMX(t).Texture, newSour.MakeRectf, newDest.MakeRectf, alpha, False);
          FCanvas.SetMatrix(bak);
        end
      else
          FCanvas.DrawBitmap(TDETexture_FMX(t).Texture, newSour.MakeRectf, newDest.MakeRectf, alpha, True);
    end
  else if (t is TBitmap) then
    begin
      if (TBitmap(t).width = 0) or (TBitmap(t).height = 0) then
          Exit;
      if newSour.IsZero then
          newSour := TDE4V.Init(TBitmap(t).BoundsF, 0);
      if newDest.IsZero then
          newDest := newSour;
      if not IsEqual(newDest.angle, 0, 0.1) then
        begin
          bak := FCanvas.Matrix;
          MakeMatrixRotation(newDest.angle, newDest.width, newDest.height, MinF(newDest.Left, newDest.Right), MinF(newDest.Top, newDest.Bottom), 0.5, 0.5, M, r);
          FCanvas.MultiplyMatrix(M);
          FCanvas.DrawBitmap(TBitmap(t), newSour.MakeRectf, newDest.MakeRectf, alpha, False);
          FCanvas.SetMatrix(bak);
        end
      else
          FCanvas.DrawBitmap(TBitmap(t), newSour.MakeRectf, newDest.MakeRectf, alpha, False);
    end
  else
      DoStatus('no interface texture! ' + t.ClassName);

  if FDebug then
    begin
      FCanvas.Stroke.Dash := TStrokeDash.Solid;
      FCanvas.Stroke.COLOR := c2c(DEColor(1, 0.5, 0.5, 1));
      FCanvas.fill.COLOR := FCanvas.Stroke.COLOR;
      FCanvas.DrawLine(Point2Pointf(newDest.Centroid), Point2Pointf(PointRotation(newDest.Centroid, (newDest.width + newDest.height) * 0.5, (newDest.angle))), 0.5);
      FCanvas.DrawRect(Geometry2DUnit.TV2Rect4.Init(newDest.MakeRectV2, newDest.angle).BoundRectf, 0, 0, [], 0.3);
    end;
end;

procedure TDrawEngineInterface_FMX.Flush;
begin
  FCanvas.Flush;
end;

procedure TDrawEngineInterface_FMX.ResetState;
begin
  FLineWidth := FCanvas.Stroke.Thickness;

  FCanvas.Stroke.Kind := TBrushKind.Solid;
  FCanvas.Stroke.Dash := TStrokeDash.Solid;
  FCanvas.fill.Kind := TBrushKind.Solid;
  FCanvas.Stroke.Thickness := 1;
end;

procedure TDrawEngineInterface_FMX.BeginDraw;
begin
  FCanvasSave := FCanvas.SaveState;
  FCanvas.BeginScene;
end;

procedure TDrawEngineInterface_FMX.EndDraw;
begin
  if FCanvasSave <> nil then
    begin
      FCanvas.RestoreState(FCanvasSave);
      FCanvasSave := nil;
    end;
  FCanvas.EndScene;
end;

function TDrawEngineInterface_FMX.CurrentScreenSize: TDEVec;
begin
  Result := FCurrSiz;
end;

function TDrawEngineInterface_FMX.GetTextSize(const Text: SystemString; Size: TDEFloat): TDEVec;
var
  r: TRectf;
begin
  r := Rectf(0, 0, 10000, 10000);
  FCanvas.Font.Size := Size;
  FCanvas.MeasureText(r, Text, False, [], TTextAlign.Leading, TTextAlign.Leading);
  Result[0] := r.Right;
  Result[1] := r.Bottom;
end;

function TDrawEngineInterface_FMX.ReadyOK: Boolean;
begin
  Result := FCanvas <> nil;
end;

constructor TDrawEngineInterface_FMX.Create;
begin
  inherited Create;
  FCanvas := nil;
  FOwnerCanvasScale := 1.0;
  FCanvasSave := nil;
  FDebug := False;
  FCurrSiz := DEVec(100, 100);
end;

destructor TDrawEngineInterface_FMX.Destroy;
begin
  inherited Destroy;
end;

procedure TDrawEngineInterface_FMX.SetSurface(c: TCanvas; OwnerCtrl: TObject);
var
  pf: TPointf;
begin
  FCanvas := c;
  if OwnerCtrl is TControl then
    begin
      pf := TControl(OwnerCtrl).AbsoluteScale;
      FOwnerCanvasScale := (pf.x + pf.y) * 0.5;
      FCurrSiz := DEVec(TControl(OwnerCtrl).width, TControl(OwnerCtrl).height);
    end
  else if OwnerCtrl is TCustomForm then
    begin
      FOwnerCanvasScale := 1.0;
      FCurrSiz := DEVec(TCustomForm(OwnerCtrl).ClientWidth, TCustomForm(OwnerCtrl).ClientHeight);
    end
  else if OwnerCtrl is TBitmap then
    begin
      FOwnerCanvasScale := 1.0;
      FCurrSiz := DEVec(TBitmap(OwnerCtrl).width, TBitmap(OwnerCtrl).height);
    end
  else
    begin
      FOwnerCanvasScale := 1.0;
      FCurrSiz := DEVec(c.width, c.height);
    end;
end;

function TDETexture_FMX.GetTexture: TBitmap;
begin
  if FTexture = nil then
    begin
      FTexture := TBitmap.Create;
      MemoryBitmapToBitmap(Self, FTexture);
    end;
  DrawUsage;
  Result := FTexture;
end;

constructor TDETexture_FMX.Create;
begin
  inherited Create;
  FTexture := nil;
end;

destructor TDETexture_FMX.Destroy;
begin
  ReleaseGPUMemory;
  inherited Destroy;
end;

procedure TDETexture_FMX.ReleaseGPUMemory;
begin
  if FTexture <> nil then
      DisposeObject(FTexture);
  FTexture := nil;
end;

procedure TDETexture_FMX.SyncTextureMemory;
begin
  GetTexture();
end;

constructor TResourceTexture.Create;
begin
  inherited Create;
  FLastLoadFile := '';
end;

constructor TResourceTexture.Create(FileName: SystemString);
begin
  inherited Create;
  FLastLoadFile := '';

  if FileName <> '' then
      LoadFromFileIO(FileName);
end;

procedure TResourceTexture.LoadFromFileIO(FileName: SystemString);
var
  stream: TCoreClassStream;
begin
  FLastLoadFile := '';
  if FileIOExists(FileName) then
    begin
      try
        stream := FileIOOpen(FileName);
        stream.Position := 0;
        LoadMemoryBitmap(stream, Self);
        DisposeObject(stream);
        FLastLoadFile := FileName;
      except
          RaiseInfo('texture "%s" format error! ', [FileName]);
      end;
    end
  else
      RaiseInfo('file "%s" no exists', [FileName]);
end;

constructor TResourceTextureIntf.Create(tex: TResourceTexture);
begin
  inherited Create;
  Texture := tex;
  TextureRect := Texture.BoundsRectV2;
  SizeScale := DEVec(1.0, 1.0);
end;

destructor TResourceTextureIntf.Destroy;
begin
  inherited Destroy;
end;

function TResourceTextureIntf.SizeOfVec: TDEVec;
begin
  Result := DEVec(RectWidth(TextureRect) * SizeScale[0], RectHeight(TextureRect) * SizeScale[1]);
end;

procedure TResourceTextureIntf.ChangeTexture(tex: TResourceTexture);
begin
  Texture := tex;
  TextureRect := Texture.BoundsRectV2;
  SizeScale := DEVec(1.0, 1.0);
end;

constructor TResourceTextureCache.Create;
begin
  inherited Create;
  TextureList := THashObjectList.CustomCreate(True, 1024);
  DefaultTexture := TResourceTexture.Create('');
  DefaultTexture.SetSize(2, 2, RasterColorF(0, 0, 0, 1.0));
end;

destructor TResourceTextureCache.Destroy;
begin
  DisposeObject(TextureList);
  DisposeObject(DefaultTexture);
  inherited Destroy;
end;

function TResourceTextureCache.CreateResourceTexture(FileName: SystemString): TResourceTextureIntf;
var
  tex: TResourceTexture;
begin
  if FileName = '' then
      Exit(nil);

  FileName := umlTrimSpace(FileName);

  if FileName = '' then
    begin
      tex := DefaultTexture;
    end
  else
    begin
      if not TextureList.Exists(FileName) then
        begin
          if FileIOExists(FileName) then
            begin
              try
                tex := TResourceTexture.Create(FileName);
                TextureList.Add(FileName, tex);
              except
                  tex := DefaultTexture;
              end;
            end
          else
              tex := DefaultTexture;
        end
      else
          tex := TextureList[FileName] as TResourceTexture;
    end;

  Result := TResourceTextureIntf.Create(tex);
  Result.TextureRect := tex.BoundsRectV2;
  Result.SizeScale := DEVec(1.0, 1.0);
end;

procedure TResourceTextureCache.ReleaseAllFMXRsource;
begin
  TextureList.ProgressP(
    procedure(const Name: PSystemString; Obj: TCoreClassObject)
    begin
      if Obj is TDETexture_FMX then
          TDETexture_FMX(Obj).ReleaseGPUMemory;
    end);
end;

function _NewRaster: TMemoryRaster;
begin
  Result := DefaultTextureClass.Create;
end;

function _NewRasterFromFile(const fn: string): TMemoryRaster;
begin
  Result := NewRaster();
  LoadMemoryBitmap(fn, TResourceTexture(Result));
end;

function _NewRasterFromStream(const stream: TCoreClassStream): TMemoryRaster;
begin
  Result := NewRaster();
  LoadMemoryBitmap(stream, TResourceTexture(Result));
end;

procedure _SaveRaster(mr: TMemoryRaster; const fn: string);
begin
  SaveMemoryBitmap(fn, mr);
end;

initialization

DefaultTextureClass := TResourceTexture;

TextureCache := TResourceTextureCache.Create;

NewRaster := _NewRaster;
NewRasterFromFile := _NewRasterFromFile;
NewRasterFromStream := _NewRasterFromStream;
SaveRaster := _SaveRaster;

finalization


DisposeObject(TextureCache);

end.
