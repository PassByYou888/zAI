{ ****************************************************************************** }
{ * h264Image.pas        by qq600585                                           * }
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

unit h264Image;

{$INCLUDE zDefine.inc}

interface

uses
  h264Types, h264Util, CoreClasses, MemoryRaster;

const
  QPARAM_AUTO = 52;

type
  TPlanarImage = class
  private
    w, h: int32_t;
    qp: uint8_t;
    procedure SetQParam(const AValue: uint8_t);
  public
    frame_num: int32_t;
    plane: array [0 .. 2] of uint8_p; // pointers to image planes (0 - luma; 1,2 - chroma U/V)
    stride, stride_c: int32_t;        // plane strides

    property QParam: uint8_t read qp write SetQParam;
    property width: int32_t read w;
    property height: int32_t read h;

    constructor Create(const width_, height_: int32_t);
    destructor Destroy; override;
    procedure SwapUV;

    procedure LoadFromRaster(raster: TMemoryRaster);
    procedure SaveToRaster(raster: TMemoryRaster);
  end;

procedure YV12ToRaster(const sour: TPlanarImage; const dest: TMemoryRaster); overload;
procedure RasterToYV12(const sour: TMemoryRaster; const dest: TPlanarImage); overload;

implementation

uses h264Common;

procedure TPlanarImage.SetQParam(const AValue: uint8_t);
begin
  if AValue > 51 then
      qp := QPARAM_AUTO
  else
      qp := AValue;
end;

constructor TPlanarImage.Create(const width_, height_: int32_t);
var
  memsize: int32_t;
begin
  inherited Create;
  w := width_;
  h := height_;
  memsize := w * h + (w * h) div 2;
  plane[0] := GetMemory(memsize);
  plane[1] := plane[0] + w * h;
  plane[2] := plane[1] + (w * h) div 4;
  stride := w;
  stride_c := w div 2;
  qp := QPARAM_AUTO;
end;

destructor TPlanarImage.Destroy;
begin
  FreeMemory(plane[0]);
  inherited Destroy;
end;

procedure TPlanarImage.SwapUV;
begin
  swap_ptr(plane[1], plane[2]);
end;

procedure TPlanarImage.LoadFromRaster(raster: TMemoryRaster);
begin
  RasterToYV12(raster, Self);
end;

procedure TPlanarImage.SaveToRaster(raster: TMemoryRaster);
begin
  YV12ToRaster(Self, raster);
end;

procedure YV12ToRaster(const sour: TPlanarImage; const dest: TMemoryRaster);
begin
  YV12ToRaster(sour.plane[0], sour.plane[1], sour.plane[2], sour.w, sour.h, sour.stride, sour.stride_c, dest, False, False);
end;

procedure RasterToYV12(const sour: TMemoryRaster; const dest: TPlanarImage);
begin
  RasterToYV12(sour, dest.plane[0], dest.plane[1], dest.plane[2], dest.w, dest.h);
end;

end.  

