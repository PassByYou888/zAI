{ ****************************************************************************** }
{ * h264 writer               by qq600585                                      * }
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
unit h264;

{$INCLUDE zDefine.inc}

interface

uses SysUtils, CoreClasses, PascalStrings, UnicodeMixedLib, MemoryRaster,
  h264Image, h264Types, h264Encoder, h264Parameters, Y4M;

type
  TH264Writer = class
  private
    ioHandle: TIOHnd;
    FFrameCount: uint32_t;
    img: TPlanarImage;
    Param: TEncodingParameters;
    Encoder: TFevh264Encoder;
    buffer: PByte;
  public
    constructor Create(const w, h, totalframe: int32_t; psf: Single; const FileName: TPascalString); overload;
    constructor Create(const w, h, totalframe: int32_t; psf: Single; const stream: TCoreClassStream); overload;

    destructor Destroy; override;

    procedure WriteFrame(raster: TMemoryRaster);
    procedure WriteY4M(r: TY4MReader);
    procedure Flush;

    property FrameCount: uint32_t read FFrameCount;
    function H264Size: Int64_t;
    function width: uint16_t;
    function height: uint16_t;
    function PerSecondFrame: Single;
  end;

implementation

constructor TH264Writer.Create(const w, h, totalframe: int32_t; psf: Single; const FileName: TPascalString);
begin
  inherited Create;
  umlFileCreate(FileName, ioHandle);
  FFrameCount := 0;
  img := TPlanarImage.Create(w, h);
  Param := TEncodingParameters.Create;
  Param.SetStreamParams(w, h, totalframe, psf);
  Param.AnalysisLevel := 2;
  Encoder := TFevh264Encoder.Create(Param);
  buffer := GetMemory(w * h * 4);
end;

constructor TH264Writer.Create(const w, h, totalframe: int32_t; psf: Single; const stream: TCoreClassStream);
begin
  inherited Create;
  umlFileCreateAsStream('stream', stream, ioHandle);
  FFrameCount := 0;
  img := TPlanarImage.Create(w, h);
  Param := TEncodingParameters.Create;
  Param.SetStreamParams(w, h, totalframe, psf);
  Param.AnalysisLevel := 2;
  Encoder := TFevh264Encoder.Create(Param);
  buffer := GetMemory(w * h * 4);
end;

destructor TH264Writer.Destroy;
begin
  FreeMemory(buffer);
  DisposeObject(Param);
  DisposeObject(Encoder);
  DisposeObject(img);
  umlFileClose(ioHandle);
  inherited Destroy;
end;

procedure TH264Writer.WriteFrame(raster: TMemoryRaster);
var
  t1, t2, pixl, enl: TTimeTick;
  oSiz: uint32_t;
  ssd: Int64;
begin
  if FFrameCount >= Param.FrameCount then
      Exit;

  t1 := GetTimeTick;
  img.LoadFromRaster(raster);
  t2 := GetTimeTick;
  pixl := t2 - t1;

  t1 := t2;
  Encoder.EncodeFrame(img, buffer, oSiz);
  t2 := GetTimeTick;
  enl := t2 - t1;

  umlFileWrite(ioHandle, oSiz, buffer^);

  inc(FFrameCount);
end;

procedure TH264Writer.WriteY4M(r: TY4MReader);
var
  i: int32_t;
  p_img: TPlanarImage;
  raster: TMemoryRaster;
begin
  raster := TMemoryRaster.Create;
  r.SeekFirstFrame;
  for i := r.CurrentFrame to r.FrameCount - 1 do
    begin
      p_img := r.ReadFrame;
      p_img.SaveToRaster(raster);
      WriteFrame(raster);
    end;
  DisposeObject(raster);
end;

procedure TH264Writer.Flush;
begin
  umlFileFlushWrite(ioHandle);
end;

function TH264Writer.H264Size: Int64_t;
begin
  Result := umlFileSize(ioHandle);
end;

function TH264Writer.width: uint16_t;
begin
  Result := Param.FrameWidth;
end;

function TH264Writer.height: uint16_t;
begin
  Result := Param.FrameHeight;
end;

function TH264Writer.PerSecondFrame: Single;
begin
  Result := Param.FrameRate;
end;

end.  
 
 
 
