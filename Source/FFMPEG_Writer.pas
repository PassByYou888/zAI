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
    VideoCodec: PAVCodec;
    VideoCodecCtx: PAVCodecContext;
    AVPacket_Ptr: PAVPacket;
    Frame, FrameRGB: PAVFrame;
    SWS_CTX: PSwsContext;
    FOutput: TCoreClassStream;
    FAutoFreeOutput: Boolean;
    FPixelFormat: TAVPixelFormat;
    FLastWidth, FLastHeight: Integer;
    FEncodeNum: Integer;
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

    function EncodeRaster(raster: TMemoryRaster; var Updated: Integer): Boolean; overload;
    function EncodeRaster(raster: TMemoryRaster): Boolean; overload;
    procedure Flush;

    property EncodeNum: Integer read FEncodeNum;
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
  VideoCodec := codec;
  if not Assigned(VideoCodec) then
    begin
      DoStatus('not found Codec.');
      exit;
    end;

  VideoCodecCtx := avcodec_alloc_context3(VideoCodec);
  if not Assigned(VideoCodecCtx) then
    begin
      DoStatus('Could not allocate video codec context');
      exit;
    end;

  AVPacket_Ptr := av_packet_alloc();

  VideoCodecCtx^.bit_rate := Bitrate;
  VideoCodecCtx^.Width := Width - (Width mod 2);
  VideoCodecCtx^.Height := Height - (Height mod 2);
  VideoCodecCtx^.time_base.num := 1;
  VideoCodecCtx^.time_base.den := PSF;
  VideoCodecCtx^.framerate.num := PSF;
  VideoCodecCtx^.framerate.den := 1;
  VideoCodecCtx^.gop_size := gop;
  VideoCodecCtx^.max_b_frames := bFrame;
  VideoCodecCtx^.pix_fmt := FPixelFormat;
  VideoCodecCtx^.qmin := quantizerMin;
  VideoCodecCtx^.qmax := quantizerMax;

  r := avcodec_open2(VideoCodecCtx, VideoCodec, nil);
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

  Frame^.format := Ord(VideoCodecCtx^.pix_fmt);
  Frame^.Width := VideoCodecCtx^.Width;
  Frame^.Height := VideoCodecCtx^.Height;
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

  SWS_CTX := sws_getContext(
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
  FEncodeNum := 0;
  Result := True;
end;

constructor TFFMPEG_Writer.Create(output_: TCoreClassStream);
begin
  inherited Create;
  VideoCodecCtx := nil;
  VideoCodec := nil;
  AVPacket_Ptr := nil;
  Frame := nil;
  FrameRGB := nil;
  SWS_CTX := nil;
  FOutput := output_;
  FAutoFreeOutput := False;
  FPixelFormat := AV_PIX_FMT_YUV420P;
  FLastWidth := 0;
  FLastHeight := 0;
  FEncodeNum := 0;
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
  if VideoCodecCtx <> nil then
      avcodec_free_context(@VideoCodecCtx);

  if Frame <> nil then
      av_frame_free(@Frame);

  if AVPacket_Ptr <> nil then
      av_packet_free(@AVPacket_Ptr);

  if SWS_CTX <> nil then
      sws_freeContext(SWS_CTX);

  if FrameRGB <> nil then
      av_frame_free(@FrameRGB);

  VideoCodecCtx := nil;
  VideoCodec := nil;
  AVPacket_Ptr := nil;
  Frame := nil;
  FrameRGB := nil;
  SWS_CTX := nil;
end;

function TFFMPEG_Writer.EncodeRaster(raster: TMemoryRaster; var Updated: Integer): Boolean;
var
  r: Integer;
begin
  Result := False;
  if FrameRGB = nil then
      exit;
  if raster = nil then
      exit;
  if SWS_CTX = nil then
      exit;
  if Frame = nil then
      exit;
  if VideoCodecCtx = nil then
      exit;
  if AVPacket_Ptr = nil then
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
      SWS_CTX,
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

    r := avcodec_send_frame(VideoCodecCtx, Frame);
    if r < 0 then
      begin
        DoStatus('Error sending a frame for encoding');
        exit;
      end;

    // seek stream to end
    FOutput.Position := FOutput.Size;

    while r >= 0 do
      begin
        r := avcodec_receive_packet(VideoCodecCtx, AVPacket_Ptr);
        if (r = AVERROR_EAGAIN) or (r = AVERROR_EOF) then
            Break;
        if r < 0 then
          begin
            DoStatus('Error during encoding');
            exit;
          end;

        FOutput.Write(AVPacket_Ptr^.data^, AVPacket_Ptr^.Size);
        inc(Updated);
        av_packet_unref(AVPacket_Ptr);
      end;
    Result := True;
    AtomInc(Frame^.pts);
  finally
    AtomInc(FEncodeNum);
    UnLockObject(FOutput);
  end;
end;

function TFFMPEG_Writer.EncodeRaster(raster: TMemoryRaster): Boolean;
var
  Updated: Integer;
begin
  Updated := 0;
  Result := EncodeRaster(raster, Updated);
end;

procedure TFFMPEG_Writer.Flush;
var
  r: Integer;
begin
  LockObject(FOutput);
  try
    (*
      avcodec_send_frame(VideoCodecCtx, here It can be NULL), in which case it is considered a flush packet.
      This signals the end of the stream. If the encoder still has packets buffered,
      it will return them after this call.
      Once flushing mode has been entered, additional flush packets are ignored, and sending frames will return AVERROR_EOF.
    *)
    r := avcodec_send_frame(VideoCodecCtx, nil);
    if r < 0 then
      begin
        DoStatus('Error sending eof frame');
        exit;
      end;

    // seek stream to end
    FOutput.Position := FOutput.Size;

    while r >= 0 do
      begin
        r := avcodec_receive_packet(VideoCodecCtx, AVPacket_Ptr);
        if (r = AVERROR_EAGAIN) or (r = AVERROR_EOF) then
            Break;
        if r < 0 then
          begin
            DoStatus('Error during encoding');
            Break;
          end;

        FOutput.Write(AVPacket_Ptr^.data^, AVPacket_Ptr^.Size);
        av_packet_unref(AVPacket_Ptr);
      end;
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
