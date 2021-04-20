{ ****************************************************************************** }
{ * draw engine for VCL                                                        * }
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
unit zDrawEngineInterface_VCL;

{$INCLUDE ..\zDefine.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.ExtCtrls,
  Vcl.Imaging.jpeg, Vcl.Imaging.pngimage,

  CoreClasses, PascalStrings, MemoryStream64,
  zDrawEngine, UnicodeMixedLib, Geometry2DUnit, MemoryRaster;

type
  TDrawEngineInterface_VCL = class(TDrawEngine_Raster)
  public
    BindBitmap: TBitmap;
    constructor Create; override;
    destructor Destroy; override;
    procedure Flush; override;
  end;

procedure MemoryBitmapToBitmap(mr: TMemoryRaster; bmp: TBitmap);
procedure BitmapToMemoryBitmap(bmp: TBitmap; mr: TMemoryRaster);

implementation

constructor TDrawEngineInterface_VCL.Create;
begin
  inherited Create;
  BindBitmap := TBitmap.Create;
end;

destructor TDrawEngineInterface_VCL.Destroy;
begin
  DisposeObject(BindBitmap);
  inherited Destroy;
end;

procedure TDrawEngineInterface_VCL.Flush;
begin
  inherited Flush;
  MemoryBitmapToBitmap(Memory, BindBitmap);
end;

procedure MemoryBitmapToBitmap(mr: TMemoryRaster; bmp: TBitmap);
var
  i: integer;
  bak_event: TNotifyEvent;
  bak_progress: TProgressEvent;
begin
  bak_event := bmp.OnChange;
  bak_progress := bmp.OnProgress;
  bmp.OnChange := nil;
  bmp.OnProgress := nil;
  bmp.PixelFormat := TPixelFormat.pf32bit;
  bmp.SetSize(mr.Width, mr.Height);
  for i := 0 to mr.Height - 1 do
      CopyPtr(mr.ScanLine[i], bmp.ScanLine[i], mr.Width * 4);
  bmp.OnChange := bak_event;
  bmp.OnProgress := bak_progress;
  bmp.Modified := True;
end;

procedure BitmapToMemoryBitmap(bmp: TBitmap; mr: TMemoryRaster);
var
  i, j: integer;
  rgb_p: PRGBArray;
  rgba_p: PRColorArray;
begin
  if bmp.PixelFormat = TPixelFormat.pf32bit then
    begin
      mr.SetSize(bmp.Width, bmp.Height);
      for i := 0 to bmp.Height - 1 do
          CopyPtr(bmp.ScanLine[i], mr.ScanLine[i], mr.Width * 4);
    end
  else if bmp.PixelFormat = TPixelFormat.pf24bit then
    begin
      mr.SetSize(bmp.Width, bmp.Height);
      for i := 0 to bmp.Height - 1 do
        begin
          rgb_p := PRGBArray(bmp.ScanLine[i]);
          rgba_p := mr.ScanLine[i];
          for j := 0 to bmp.Width - 1 do
              rgba_p^[i] := RGB2RGBA(rgb_p^[i]);
        end;
    end
  else
      RaiseInfo('no support.');
end;

function _NewRaster: TMemoryRaster;
begin
  Result := TRaster.Create;
end;

function _NewRasterFromFile(const fn: string): TMemoryRaster;
var
  pic: TPicture;
  bmp: TBitmap;
begin
  if not TMemoryRaster.CanLoadFile(fn) then
    begin
      pic := TPicture.Create;
      pic.LoadFromFile(fn);
      bmp := TBitmap.Create;
      bmp.Assign(pic.Graphic);
      Result := NewRaster();
      BitmapToMemoryBitmap(bmp, Result);
      DisposeObject([pic, bmp]);
    end
  else
    begin
      Result := NewRaster();
      Result.LoadFromFile(fn);
    end;
end;

function _NewRasterFromStream(const stream: TCoreClassStream): TMemoryRaster;
var
  pic: TPicture;
  bmp: TBitmap;
begin
  if not TMemoryRaster.CanLoadStream(stream) then
    begin
      pic := TPicture.Create;
      pic.LoadFromStream(stream);
      bmp := TBitmap.Create;
      bmp.PixelFormat := TPixelFormat.pf32bit;
      bmp.Assign(pic.Graphic);
      Result := NewRaster();
      BitmapToMemoryBitmap(bmp, Result);
      DisposeObject([pic, bmp]);
    end
  else
    begin
      Result := NewRaster();
      Result.LoadFromStream(stream);
    end;
end;

procedure _SaveRaster(b: TMemoryRaster; const f: string);
begin
  if umlMultipleMatch(['*.bmp'], f) then
      b.SaveToFile(f)
  else if umlMultipleMatch(['*.seq'], f) then
      b.SaveToZLibCompressFile(f)
  else if umlMultipleMatch(['*.yv12'], f) then
      b.SaveToYV12File(f)
  else if umlMultipleMatch(['*.jls'], f) then
      b.SaveToJpegLS3File(f)
  else
      b.SaveToFile(f);
end;

initialization

NewRaster := _NewRaster;
NewRasterFromFile := _NewRasterFromFile;
NewRasterFromStream := _NewRasterFromStream;
SaveRaster := _SaveRaster;

end.
