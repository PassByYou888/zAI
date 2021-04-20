{ ****************************************************************************** }
{ * FFMPEG video Writer               by qq600585                              * }
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
unit FFMPEG_Writer;

{$INCLUDE zDefine.inc}

interface

uses SysUtils, CoreClasses, PascalStrings, UnicodeMixedLib, MemoryStream64, MemoryRaster, DoStatusIO, FFMPEG;

type
  TFFMPEG_Writer = class(TCoreClassObject)
  protected
    videoCodec: PAVCodec;
    videoCodecCtx: PAVCodecContext;
    AVPacket_ptr: PAVPacket;
    Frame, FrameRGB: PAVFrame;
    Sws_Ctx: PSwsContext;
    FOutput: TCoreClassStream;
    FAutoFreeOutput: Boolean;
    FPixelFormat: TAVPixelFormat;
    FLastWidth, FLastHeight: Integer;
    function InternalOpenCodec(const codec: PAVCodec; const Width, Height, PSF, gop, bFrame, quantizerMin, quantizerMax: Integer; const Bitrate: Int64): Boolean;
  public
    constructor Create(output_: TCoreClassStream);
    destructor Destroy; override;

    class procedure PrintEncodec();

    function OpenCodec(const codec_name: U_String; const Width, Height, PSF, gop, bFrame: Integer; const Bitrate: Int64): Boolean; overload;
    function OpenCodec(const codec_id: TAVCodecID; const Width, Height, PSF, gop, bFrame: Integer; const Bitrate: Int64): Boolean; overload;
    function OpenH264Codec(const Width, Height, PSF: Integer; const Bitrate: Int64): Boolean; overload;
    function OpenH264Codec(const codec_name: U_String; const Width, Height, PSF: Integer; const Bitrate: Int64): Boolean; overload;
    // default quantizerMin=2, quantizerMax=31
    function OpenJPEGCodec(const Width, Height, quantizerMin, quantizerMax: Integer): Boolean; overload;
    function OpenJPEGCodec(const Width, Height: Integer): Boolean; overload;
    procedure CloseCodec;

    function EncodeRaster(raster: TMemoryRaster): Boolean;
    procedure Flush;

    function Size: Int64;
    function LockOutput: TCoreClassStream;
    procedure UnLockOutoput;
    property AutoFreeOutput: Boolean read FAutoFreeOutput write FAutoFreeOutput;
    property PixelFormat: TAVPixelFormat read FPixelFormat write FPixelFormat;
    property LastWidth: Integer read FLastWidth;
    property LastHeight: Integer read FLastHeight;
  end;

implementation

function TFFMPEG_Writer.InternalOpenCodec(const codec: PAVCodec; const Width, Height, PSF, gop, bFrame, quantizerMin, quantizerMax: Integer; const Bitrate: Int64): Boolean;
var
  r: Integer;
begin
  Result := False;
  videoCodec := codec;
  if not Assigned(videoCodec) then
    begin
      DoStatus('not found Codec.');
      exit;
    end;

  videoCodecCtx := avcodec_alloc_context3(videoCodec);
  if not Assigned(videoCodecCtx) then
    begin
      DoStatus('Could not allocate video codec context');
      exit;
    end;

  AVPacket_ptr := av_packet_alloc();

  videoCodecCtx^.bit_rate := Bitrate;
  videoCodecCtx^.Width := Width - (Width mod 2);
  videoCodecCtx^.Height := Height - (Height mod 2);
  videoCodecCtx^.time_base.num := 1;
  videoCodecCtx^.time_base.den := PSF;
  videoCodecCtx^.framerate.num := PSF;
  videoCodecCtx^.framerate.den := 1;
  videoCodecCtx^.gop_size := gop;
  videoCodecCtx^.max_b_frames := bFrame;
  videoCodecCtx^.pix_fmt := FPixelFormat;
  videoCodecCtx^.qmin := quantizerMin;
  videoCodecCtx^.qmax := quantizerMax;

  r := avcodec_open2(videoCodecCtx, videoCodec, nil);
  if r < 0 then
    begin
      DoStatus('Could not open codec: %s', [av_err2str(r)]);
      exit;
    end;

  // alloc frame
  Frame := av_frame_alloc();
  FrameRGB := av_frame_alloc();
  if (FrameRGB = nil) or (Frame = nil) then
    begin
      DoStatus('Could not allocate AVFrame structure');
      exit;
    end;

  Frame^.format := Ord(videoCodecCtx^.pix_fmt);
  Frame^.Width := videoCodecCtx^.Width;
  Frame^.Height := videoCodecCtx^.Height;
  Frame^.pts := 0;

  // alignment
  r := av_frame_get_buffer(Frame, 32);
  if r < 0 then
    begin
      DoStatus('Could not allocate the video frame data');
      exit;
    end;

  FrameRGB^.format := Ord(AV_PIX_FMT_RGB32);
  FrameRGB^.Width := Frame^.Width;
  FrameRGB^.Height := Frame^.Height;

  Sws_Ctx := sws_getContext(
    FrameRGB^.Width,
    FrameRGB^.Height,
    AV_PIX_FMT_RGB32,
    Frame^.Width,
    Frame^.Height,
    FPixelFormat,
    SWS_BILINEAR,
    nil,
    nil,
    nil);

  FLastWidth := Width;
  FLastHeight := Height;
  Result := True;
end;

constructor TFFMPEG_Writer.Create(output_: TCoreClassStream);
begin
  inherited Create;
  videoCodecCtx := nil;
  videoCodec := nil;
  AVPacket_ptr := nil;
  Frame := nil;
  FrameRGB := nil;
  Sws_Ctx := nil;
  FOutput := output_;
  FAutoFreeOutput := False;
  FPixelFormat := AV_PIX_FMT_YUV420P;
  FLastWidth := 0;
  FLastHeight := 0;
end;

destructor TFFMPEG_Writer.Destroy;
begin
  CloseCodec;
  if FAutoFreeOutput then
      DisposeObject(FOutput);
  inherited Destroy;
end;

class procedure TFFMPEG_Writer.PrintEncodec;
var
  codec: PAVCodec;
begin
  codec := av_codec_next(nil);
  while codec <> nil do
    begin
      if av_codec_is_encoder(codec) = 1 then
          DoStatus('ID[%d] Name[%s] %s', [Integer(codec^.id), string(codec^.name), string(codec^.long_name)]);
      codec := av_codec_next(codec);
    end;
end;

function TFFMPEG_Writer.OpenCodec(const codec_name: U_String; const Width, Height, PSF, gop, bFrame: Integer; const Bitrate: Int64): Boolean;
var
  tmp: Pointer;
begin
  FPixelFormat := AV_PIX_FMT_YUV420P;
  tmp := codec_name.BuildPlatformPChar();
  Result := InternalOpenCodec(avcodec_find_encoder_by_name(tmp), Width, Height, PSF, gop, bFrame, 2, 31, Bitrate);
  U_String.FreePlatformPChar(tmp);
end;

function TFFMPEG_Writer.OpenCodec(const codec_id: TAVCodecID; const Width, Height, PSF, gop, bFrame: Integer; const Bitrate: Int64): Boolean;
begin
  FPixelFormat := AV_PIX_FMT_YUV420P;
  Result := InternalOpenCodec(avcodec_find_encoder(codec_id), Width, Height, PSF, gop, bFrame, 2, 31, Bitrate);
end;

function TFFMPEG_Writer.OpenH264Codec(const Width, Height, PSF: Integer; const Bitrate: Int64): Boolean;
begin
  Result := OpenCodec(AV_CODEC_ID_H264, Width, Height, PSF, PSF div 2, 1, Bitrate);
end;

function TFFMPEG_Writer.OpenH264Codec(const codec_name: U_String; const Width, Height, PSF: Integer; const Bitrate: Int64): Boolean;
begin
  Result := OpenCodec(codec_name, Width, Height, PSF, PSF div 2, 1, Bitrate);
end;

function TFFMPEG_Writer.OpenJPEGCodec(const Width, Height, quantizerMin, quantizerMax: Integer): Boolean;
begin
  FPixelFormat := AV_PIX_FMT_YUVJ420P;
  Result := InternalOpenCodec(avcodec_find_encoder(AV_CODEC_ID_MJPEG), Width, Height, 25, 1, 0, quantizerMin, quantizerMax, 1024 * 1024);
end;

function TFFMPEG_Writer.OpenJPEGCodec(const Width, Height: Integer): Boolean;
begin
  Result := OpenJPEGCodec(Width, Height, 2, 31);
end;

procedure TFFMPEG_Writer.CloseCodec;
begin
  if videoCodecCtx <> nil then
      avcodec_free_context(@videoCodecCtx);

  if Frame <> nil then
      av_frame_free(@Frame);

  if AVPacket_ptr <> nil then
      av_packet_free(@AVPacket_ptr);

  if Sws_Ctx <> nil then
      sws_freeContext(Sws_Ctx);

  if FrameRGB <> nil then
      av_frame_free(@FrameRGB);

  videoCodecCtx := nil;
  videoCodec := nil;
  AVPacket_ptr := nil;
  Frame := nil;
  FrameRGB := nil;
  Sws_Ctx := nil;
end;

function TFFMPEG_Writer.EncodeRaster(raster: TMemoryRaster): Boolean;
var
  r: Integer;
begin
  Result := False;
  if FrameRGB = nil then
      exit;
  if raster = nil then
      exit;
  if Sws_Ctx = nil then
      exit;
  if Frame = nil then
      exit;
  if videoCodecCtx = nil then
      exit;
  if AVPacket_ptr = nil then
      exit;

  LockObject(FOutput);
  try
    // FrameRGB
    FrameRGB^.data[0] := @raster.Bits^[0];
    FrameRGB^.Width := Frame^.Width;
    FrameRGB^.Height := Frame^.Height;
    FrameRGB^.linesize[0] := Frame^.Width * 4;

    // transform BGRA to YV420
    sws_scale(
      Sws_Ctx,
      @FrameRGB^.data,
      @FrameRGB^.linesize,
      0,
      Frame^.Height,
      @Frame^.data,
      @Frame^.linesize);

    (* make sure the frame data is writable *)
    r := av_frame_make_writable(Frame);
    if r < 0 then
      begin
        DoStatus('av_frame_make_writable failed!');
        exit;
      end;

    r := avcodec_send_frame(videoCodecCtx, Frame);
    if r < 0 then
      begin
        DoStatus('Error sending a frame for encoding');
        exit;
      end;

    // seek stream to end
    FOutput.Position := FOutput.Size;

    while r >= 0 do
      begin
        r := avcodec_receive_packet(videoCodecCtx, AVPacket_ptr);
        if (r = AVERROR_EAGAIN) or (r = AVERROR_EOF) then
            Break;
        if r < 0 then
          begin
            DoStatus('Error during encoding');
            exit;
          end;

        FOutput.Write(AVPacket_ptr^.data^, AVPacket_ptr^.Size);
        av_packet_unref(AVPacket_ptr);
      end;
    Result := True;
    AtomInc(Frame^.pts);
  finally
      UnLockObject(FOutput);
  end;
end;

procedure TFFMPEG_Writer.Flush;
var
  buff: array [0 .. 3] of Byte;
begin
  LockObject(FOutput);
  try
    // seek stream to end
    FOutput.Position := FOutput.Size;

    buff[0] := 0;
    buff[1] := 0;
    buff[2] := 1;
    buff[3] := $B7;

    FOutput.Write(buff[0], 4);
  finally
      UnLockObject(FOutput);
  end;
end;

function TFFMPEG_Writer.Size: Int64;
begin
  Result := LockOutput().Size;
  UnLockOutoput();
end;

function TFFMPEG_Writer.LockOutput: TCoreClassStream;
begin
  LockObject(FOutput);
  Result := FOutput;
end;

procedure TFFMPEG_Writer.UnLockOutoput;
begin
  UnLockObject(FOutput);
end;

end.
