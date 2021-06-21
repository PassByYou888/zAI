{ ****************************************************************************** }
{ * FFMPEG video Reader               by qq600585                              * }
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
unit FFMPEG_Reader;

{$INCLUDE zDefine.inc}

interface

uses SysUtils, Classes, CoreClasses, PascalStrings, UnicodeMixedLib, MemoryStream64, MemoryRaster, DoStatusIO, FFMPEG;

type
  TFFMPEG_Reader = class;
  TFFMPEG_VideoStreamReader = class;

  TFFMPEG_Reader = class(TCoreClassObject)
  private
    FVideoSource: TPascalString;
    FWorkOnGPU: Boolean;
    FFormatCtx: PAVFormatContext;
    FVideoCodecCtx: PAVCodecContext;
    FAudioCodecCtx: PAVCodecContext;
    FVideoCodec: PAVCodec;
    FAudioCodec: PAVCodec;
    Frame, FrameRGB: PAVFrame;
    FrameRGB_buffer: PByte;
    FSWS_CTX: PSwsContext;
    VideoStreamIndex: integer;
    AudioStreamIndex: integer;
    VideoStream: PAVStream;
    AVPacket_ptr: PAVPacket;
  public
    Current: Double;
    Current_Frame: int64;
    Width, Height: integer;
    property VideoSource: TPascalString read FVideoSource;
    property WorkOnGPU: Boolean read FWorkOnGPU;

    constructor Create(const VideoSource_: TPascalString); overload;
    constructor Create(const VideoSource_: TPascalString; const useGPU_: Boolean); overload;
    destructor Destroy; override;

    procedure OpenVideo(const VideoSource_: TPascalString; useGPU_: Boolean); overload;
    procedure OpenVideo(const VideoSource_: TPascalString); overload;
    procedure CloseVideo;

    procedure ResetFit(NewWidth, NewHeight: integer);

    function NextFrame(): Boolean;
    function ReadFrame(output: TMemoryRaster; RasterizationCopy_: Boolean): Boolean;

    procedure Seek(second: Double);
    function Total: Double;
    function CurrentStream_Total_Frame: int64;
    function CurrentStream_PerSecond_Frame(): Double;
    function CurrentStream_PerSecond_FrameRound(): integer;
    property PSF: Double read CurrentStream_PerSecond_Frame;
    property PSFRound: integer read CurrentStream_PerSecond_FrameRound;
    property RoundPSF: integer read CurrentStream_PerSecond_FrameRound;
    property PSF_I: integer read CurrentStream_PerSecond_FrameRound;
  end;

  TOnWriteBufferBefore = procedure(Sender: TFFMPEG_VideoStreamReader; var p: Pointer; var siz: NativeUInt) of object;
  TOnWriteBufferAfter = procedure(Sender: TFFMPEG_VideoStreamReader; p: Pointer; siz: NativeUInt; decodeFrameNum: integer) of object;
  TOnVideoFillNewRaster = procedure(Sender: TFFMPEG_VideoStreamReader; Raster: TMemoryRaster; var SaveToPool: Boolean) of object;

  TFFMPEG_VideoStreamReader = class(TCoreClassObject)
  private
    FVideoCodecCtx: PAVCodecContext;
    FVideoCodec: PAVCodec;
    AVParser: PAVCodecParserContext;
    AVPacket_ptr: PAVPacket;
    Frame, FrameRGB: PAVFrame;
    FrameRGB_buffer: PByte;
    FSWS_CTX: PSwsContext;
    SwapBuff: TMemoryStream64;
    VideoRasterPool: TMemoryRasterList;
  protected
    procedure DoWriteBufferBefore(var p: Pointer; var siz: NativeUInt); virtual;
    procedure DoVideoFillNewRaster(Raster: TMemoryRaster; var SaveToPool: Boolean); virtual;
    procedure DoWriteBufferAfter(p: Pointer; siz: NativeUInt; decodeFrameNum: integer); virtual;

    procedure InternalOpenDecodec(const codec: PAVCodec);
  public
    OnWriteBufferBefore: TOnWriteBufferBefore;
    OnVideoFillNewRaster: TOnVideoFillNewRaster;
    OnWriteBufferAfter: TOnWriteBufferAfter;

    constructor Create;
    destructor Destroy; override;

    class procedure PrintDecodec();

    procedure OpenDecodec(const codec_name: U_String); overload;
    procedure OpenDecodec(const codec_id: TAVCodecID); overload;
    procedure OpenDecodec(); overload; // AV_CODEC_ID_H264
    procedure OpenH264Decodec();
    procedure OpenMJPEGDecodec();
    procedure CloseCodec;

    // parser and decode frame
    // return decode frame number on this step
    function WriteBuffer(p: Pointer; siz: NativeUInt): integer; overload;
    function WriteBuffer(stream_: TCoreClassStream): integer; overload;

    function DecodedRasterNum: integer;
    function LockVideoPool: TMemoryRasterList;
    procedure UnLockVideoPool(freeRaster_: Boolean); overload;
    procedure UnLockVideoPool(); overload;
    procedure ClearVideoPool;
  end;

  // pascal h264
function ExtractVideoAsPasH264(VideoSource_: TPascalString; dest: TCoreClassStream): integer; overload;
function ExtractVideoAsPasH264(VideoSource_, DestH264: TPascalString): integer; overload;

// hardware h264
function ExtractVideoAsH264(VideoSource_: TPascalString; dest: TCoreClassStream; Bitrate: int64): integer; overload;
function ExtractVideoAsH264(VideoSource_: TPascalString; DestH264: TPascalString; Bitrate: int64): integer; overload;

var
  // Buffer size used for online video(rtsp/rtmp/http/https), 720p 1080p 2K 4K 8K support
  FFMPEG_Reader_BufferSize: integer; // default 128M

implementation

uses H264, FFMPEG_Writer, Geometry2DUnit;

function ExtractVideoAsPasH264(VideoSource_: TPascalString; dest: TCoreClassStream): integer;
var
  ff: TFFMPEG_Reader;
  h: TH264Writer;
  Raster: TMemoryRaster;
  tk: TTimeTick;
begin
  DoStatus('ffmpeg open ', [VideoSource_.Text]);
  try
    ff := TFFMPEG_Reader.Create(VideoSource_);
    DoStatus('create h264 stream %d*%d total: %d', [ff.Width, ff.Height, ff.CurrentStream_Total_Frame]);
  except
    Result := 0;
    exit;
  end;
  h := TH264Writer.Create(ff.Width, ff.Height, ff.CurrentStream_Total_Frame, ff.CurrentStream_PerSecond_Frame, dest);
  Raster := TMemoryRaster.Create;
  tk := GetTimeTick();
  while ff.ReadFrame(Raster, False) do
    begin
      h.WriteFrame(Raster);
      if GetTimeTick() - tk > 2000 then
        begin
          DoStatus('%s -> h264.stream progress %d/%d', [umlGetFileName(VideoSource_).Text, h.FrameCount, ff.CurrentStream_Total_Frame]);
          h.Flush;
          tk := GetTimeTick();
        end;
    end;
  Result := h.FrameCount;
  disposeObject(ff);
  disposeObject(h);
  DoStatus('done %s -> h264 stream.', [umlGetFileName(VideoSource_).Text]);
end;

function ExtractVideoAsPasH264(VideoSource_, DestH264: TPascalString): integer;
var
  ff: TFFMPEG_Reader;
  h: TH264Writer;
  Raster: TMemoryRaster;
  tk: TTimeTick;
begin
  DoStatus('ffmpeg open ', [VideoSource_.Text]);
  try
    ff := TFFMPEG_Reader.Create(VideoSource_);
    DoStatus('create h264 stream %d*%d total: %d', [ff.Width, ff.Height, ff.CurrentStream_Total_Frame]);
  except
    Result := 0;
    exit;
  end;
  h := TH264Writer.Create(ff.Width, ff.Height, ff.CurrentStream_Total_Frame, ff.CurrentStream_PerSecond_Frame, DestH264);
  Raster := TMemoryRaster.Create;
  tk := GetTimeTick();
  while ff.ReadFrame(Raster, False) do
    begin
      h.WriteFrame(Raster);
      if GetTimeTick() - tk > 2000 then
        begin
          DoStatus('%s -> %s progress %d/%d', [umlGetFileName(VideoSource_).Text, umlGetFileName(DestH264).Text, h.FrameCount, ff.CurrentStream_Total_Frame]);
          h.Flush;
          tk := GetTimeTick();
        end;
    end;
  Result := h.FrameCount;
  disposeObject(ff);
  disposeObject(h);
  DoStatus('done %s -> %s', [umlGetFileName(VideoSource_).Text, umlGetFileName(DestH264).Text]);
end;

function ExtractVideoAsH264(VideoSource_: TPascalString; dest: TCoreClassStream; Bitrate: int64): integer;
var
  ff: TFFMPEG_Reader;
  h: TFFMPEG_Writer;
  Raster: TMemoryRaster;
  tk: TTimeTick;
begin
  Result := 0;
  DoStatus('ffmpeg open ', [VideoSource_.Text]);
  try
    ff := TFFMPEG_Reader.Create(VideoSource_);
    DoStatus('create h264 stream %d*%d total: %d', [ff.Width, ff.Height, ff.CurrentStream_Total_Frame]);
  except
      exit;
  end;
  h := TFFMPEG_Writer.Create(dest);
  if h.OpenH264Codec(ff.Width, ff.Height, Round(ff.CurrentStream_PerSecond_Frame), Bitrate) then
    begin
      Raster := TMemoryRaster.Create;
      tk := GetTimeTick();
      while ff.ReadFrame(Raster, False) do
        begin
          h.EncodeRaster(Raster);
          if GetTimeTick() - tk > 2000 then
            begin
              DoStatus('%s -> h264.stream progress %d/%d', [umlGetFileName(VideoSource_).Text, Result, ff.CurrentStream_Total_Frame]);
              h.Flush;
              tk := GetTimeTick();
            end;
          inc(Result);
        end;
      disposeObject(Raster);
    end;
  disposeObject(ff);
  disposeObject(h);
  DoStatus('done %s -> h264 stream.', [umlGetFileName(VideoSource_).Text]);
end;

function ExtractVideoAsH264(VideoSource_: TPascalString; DestH264: TPascalString; Bitrate: int64): integer;
var
  fs: TCoreClassFileStream;
begin
  try
    fs := TCoreClassFileStream.Create(DestH264, fmCreate);
    Result := ExtractVideoAsH264(VideoSource_, fs, Bitrate);
    disposeObject(fs);
  except
      Result := 0;
  end;
end;

constructor TFFMPEG_Reader.Create(const VideoSource_: TPascalString);
begin
  inherited Create;
  OpenVideo(VideoSource_);
end;

constructor TFFMPEG_Reader.Create(const VideoSource_: TPascalString; const useGPU_: Boolean);
begin
  inherited Create;
  OpenVideo(VideoSource_, useGPU_);
end;

destructor TFFMPEG_Reader.Destroy;
begin
  CloseVideo;
  inherited Destroy;
end;

procedure TFFMPEG_Reader.OpenVideo(const VideoSource_: TPascalString; useGPU_: Boolean);
var
  gpu_decodec: PAVCodec;
  AV_Options: PPAVDictionary;
  tmp: Pointer;
  i: integer;
  av_st: PPAVStream;
  p: Pointer;
  numByte: integer;
begin
  FVideoSource := VideoSource_;
  FWorkOnGPU := False;

  AV_Options := nil;
  FFormatCtx := nil;
  FVideoCodecCtx := nil;
  FAudioCodecCtx := nil;
  FVideoCodec := nil;
  FAudioCodec := nil;
  Frame := nil;
  FrameRGB := nil;
  AVPacket_ptr := nil;
  FSWS_CTX := nil;

  p := VideoSource_.BuildPlatformPChar;

  // Open video file
  try
    tmp := TPascalString(IntToStr(FFMPEG_Reader_BufferSize)).BuildPlatformPChar;
    av_dict_set(@AV_Options, 'buffer_size', tmp, 0);
    av_dict_set(@AV_Options, 'stimeout', '6000000', 0);
    av_dict_set(@AV_Options, 'rtsp_flags', '+prefer_tcp', 0);
    av_dict_set(@AV_Options, 'rtsp_transport', '+tcp', 0);
    TPascalString.FreePlatformPChar(tmp);

    if (avformat_open_input(@FFormatCtx, PAnsiChar(p), nil, @AV_Options) <> 0) then
      begin
        RaiseInfo('Could not open source file %s', [VideoSource_.Text]);
        exit;
      end;

    // Retrieve stream information
    if avformat_find_stream_info(FFormatCtx, nil) < 0 then
      begin
        if FFormatCtx <> nil then
            avformat_close_input(@FFormatCtx);

        RaiseInfo('Could not find stream information %s', [VideoSource_.Text]);
        exit;
      end;

    if IsConsole then
        av_dump_format(FFormatCtx, 0, PAnsiChar(p), 0);

    VideoStreamIndex := -1;
    AudioStreamIndex := -1;
    VideoStream := nil;
    av_st := FFormatCtx^.streams;
    for i := 0 to FFormatCtx^.nb_streams - 1 do
      begin
        if av_st^^.codec^.codec_type = AVMEDIA_TYPE_VIDEO then
          begin
            VideoStreamIndex := av_st^^.index;
            FVideoCodecCtx := av_st^^.codec;
            VideoStream := av_st^;
          end
        else if av_st^^.codec^.codec_type = AVMEDIA_TYPE_AUDIO then
          begin
            AudioStreamIndex := av_st^^.index;
            FAudioCodecCtx := av_st^^.codec;
          end;
        inc(av_st);
      end;

    if VideoStreamIndex = -1 then
      begin
        RaiseInfo('Dont find a video stream');
        exit;
      end;

    FVideoCodec := avcodec_find_decoder(FVideoCodecCtx^.codec_id);
    if FVideoCodec = nil then
      begin
        RaiseInfo('Unsupported FVideoCodec!');
        exit;
      end;

    if (useGPU_) and (CurrentPlatform in [epWin32, epWin64]) and (FVideoCodecCtx^.codec_id in [AV_CODEC_ID_H264, AV_CODEC_ID_HEVC]) then
      begin
        gpu_decodec := nil;
        if FVideoCodecCtx^.codec_id = AV_CODEC_ID_H264 then
            gpu_decodec := avcodec_find_decoder_by_name('h264_cuvid')
        else if FVideoCodecCtx^.codec_id = AV_CODEC_ID_HEVC then
            gpu_decodec := avcodec_find_decoder_by_name('hevc_cuvid');

        if (avcodec_open2(FVideoCodecCtx, gpu_decodec, nil) < 0) then
          begin
            if avcodec_open2(FVideoCodecCtx, FVideoCodec, nil) < 0 then
              begin
                RaiseInfo('Could not open FVideoCodec');
                exit;
              end;
          end
        else
          begin
            FVideoCodec := gpu_decodec;
            FWorkOnGPU := True;
          end;
      end
    else
      begin
        if avcodec_open2(FVideoCodecCtx, FVideoCodec, nil) < 0 then
          begin
            RaiseInfo('Could not open FVideoCodec');
            exit;
          end;
      end;

    if AudioStreamIndex >= 0 then
      begin
        FAudioCodec := avcodec_find_decoder(FAudioCodecCtx^.codec_id);
        if FAudioCodec <> nil then
            avcodec_open2(FAudioCodecCtx, FAudioCodec, nil);
      end;

    Width := FVideoCodecCtx^.Width;
    Height := FVideoCodecCtx^.Height;

    Frame := av_frame_alloc();
    FrameRGB := av_frame_alloc();
    if (FrameRGB = nil) or (Frame = nil) then
        RaiseInfo('Could not allocate AVFrame structure');
    numByte := avpicture_get_size(AV_PIX_FMT_RGB32, Width, Height);
    FrameRGB_buffer := av_malloc(numByte * sizeof(Cardinal));
    FSWS_CTX := sws_getContext(
      FVideoCodecCtx^.Width,
      FVideoCodecCtx^.Height,
      FVideoCodecCtx^.pix_fmt,
      Width,
      Height,
      AV_PIX_FMT_RGB32,
      SWS_FAST_BILINEAR,
      nil,
      nil,
      nil);
    avpicture_fill(PAVPicture(FrameRGB), FrameRGB_buffer, AV_PIX_FMT_RGB32, Width, Height);

    AVPacket_ptr := av_packet_alloc();

    Current := 0;
    Current_Frame := 0;
  finally
      TPascalString.FreePlatformPChar(p);
  end;
end;

procedure TFFMPEG_Reader.OpenVideo(const VideoSource_: TPascalString);
begin
  OpenVideo(VideoSource_, True);
end;

procedure TFFMPEG_Reader.CloseVideo;
begin
  if AVPacket_ptr <> nil then
      av_free_packet(AVPacket_ptr);

  if FrameRGB_buffer <> nil then
      av_free(FrameRGB_buffer);

  if FrameRGB <> nil then
      av_free(FrameRGB);

  if Frame <> nil then
      av_free(Frame);

  if FVideoCodecCtx <> nil then
      avcodec_close(FVideoCodecCtx);

  if FAudioCodecCtx <> nil then
      avcodec_close(FAudioCodecCtx);

  if FFormatCtx <> nil then
      avformat_close_input(@FFormatCtx);

  if FSWS_CTX <> nil then
      sws_freeContext(FSWS_CTX);

  FFormatCtx := nil;
  FVideoCodecCtx := nil;
  FAudioCodecCtx := nil;
  FVideoCodec := nil;
  FAudioCodec := nil;
  Frame := nil;
  FrameRGB := nil;
  FrameRGB_buffer := nil;
  AVPacket_ptr := nil;
  FSWS_CTX := nil;
end;

procedure TFFMPEG_Reader.ResetFit(NewWidth, NewHeight: integer);
var
  numByte: integer;
  R: TRectV2;
begin
  if FrameRGB_buffer <> nil then
      av_free(FrameRGB_buffer);
  FrameRGB_buffer := nil;
  if FrameRGB <> nil then
      av_free(FrameRGB);
  FrameRGB := nil;
  if FSWS_CTX <> nil then
      sws_freeContext(FSWS_CTX);
  FSWS_CTX := nil;

  R := FitRect(FVideoCodecCtx^.Width, FVideoCodecCtx^.Height, RectV2(0, 0, NewWidth, NewHeight));
  Width := Round(RectWidth(R));
  Height := Round(RectHeight(R));

  FrameRGB := av_frame_alloc();
  if (FrameRGB = nil) then
      RaiseInfo('Could not allocate AVFrame structure');
  numByte := avpicture_get_size(AV_PIX_FMT_RGB32, Width, Height);
  FrameRGB_buffer := av_malloc(numByte * sizeof(Cardinal));
  FSWS_CTX := sws_getContext(
    FVideoCodecCtx^.Width,
    FVideoCodecCtx^.Height,
    FVideoCodecCtx^.pix_fmt,
    Width,
    Height,
    AV_PIX_FMT_RGB32,
    SWS_FAST_BILINEAR,
    nil,
    nil,
    nil);
  avpicture_fill(PAVPicture(FrameRGB), FrameRGB_buffer, AV_PIX_FMT_RGB32, Width, Height);
end;

function TFFMPEG_Reader.NextFrame(): Boolean;
var
  done: Boolean;
  R: integer;
begin
  Result := False;
  done := False;
  try
    while (av_read_frame(FFormatCtx, AVPacket_ptr) >= 0) do
      begin
        if (AVPacket_ptr^.stream_index = VideoStreamIndex) then
          begin
            R := avcodec_send_packet(FVideoCodecCtx, AVPacket_ptr);
            if R < 0 then
              begin
                if FWorkOnGPU then
                  begin
                    av_packet_unref(AVPacket_ptr);
                    CloseVideo;
                    OpenVideo(FVideoSource, False);
                    Result := NextFrame();
                    exit;
                  end;
                DoStatus('Error sending a packet for decoding');
              end;

            done := False;
            while True do
              begin
                R := avcodec_receive_frame(FVideoCodecCtx, Frame);

                // success, a frame was returned
                if R = 0 then
                    break;

                // AVERROR(EAGAIN): output is not available in this state - user must try to send new input
                if R = AVERROR_EAGAIN then
                  begin
                    av_packet_unref(AVPacket_ptr);
                    Result := NextFrame();
                    exit;
                  end;

                // AVERROR_EOF: the decoder has been fully flushed, and there will be no more output frames
                if R = AVERROR_EOF then
                  begin
                    avcodec_flush_buffers(FVideoCodecCtx);
                    continue;
                  end;

                // error
                if R < 0 then
                  begin
                    if FWorkOnGPU then
                      begin
                        av_packet_unref(AVPacket_ptr);
                        CloseVideo;
                        OpenVideo(FVideoSource, False);
                        Result := NextFrame();
                        exit;
                      end;
                    done := True;
                    break;
                  end;
              end;

            if (not done) then
                inc(Current_Frame);
            Result := True;
            done := True;
          end;

        av_packet_unref(AVPacket_ptr);
        if done then
            break;
      end;
  except
  end;
end;

function TFFMPEG_Reader.ReadFrame(output: TMemoryRaster; RasterizationCopy_: Boolean): Boolean;
var
  done: Boolean;
  R: integer;
begin
  Result := False;
  done := False;
  try
    while (av_read_frame(FFormatCtx, AVPacket_ptr) >= 0) do
      begin
        if (AVPacket_ptr^.stream_index = VideoStreamIndex) then
          begin
            R := avcodec_send_packet(FVideoCodecCtx, AVPacket_ptr);
            if R < 0 then
              begin
                if FWorkOnGPU then
                  begin
                    av_packet_unref(AVPacket_ptr);
                    CloseVideo;
                    OpenVideo(FVideoSource, False);
                    Result := ReadFrame(output, RasterizationCopy_);
                    exit;
                  end;
                DoStatus('Error sending a packet for decoding');
              end;

            done := False;
            while True do
              begin
                R := avcodec_receive_frame(FVideoCodecCtx, Frame);

                // success, a frame was returned
                if R = 0 then
                    break;

                // AVERROR(EAGAIN): output is not available in this state - user must try to send new input
                if R = AVERROR_EAGAIN then
                  begin
                    av_packet_unref(AVPacket_ptr);
                    Result := ReadFrame(output, RasterizationCopy_);
                    exit;
                  end;

                // AVERROR_EOF: the decoder has been fully flushed, and there will be no more output frames
                if R = AVERROR_EOF then
                  begin
                    avcodec_flush_buffers(FVideoCodecCtx);
                    continue;
                  end;

                // error
                if R < 0 then
                  begin
                    if FWorkOnGPU then
                      begin
                        av_packet_unref(AVPacket_ptr);
                        CloseVideo;
                        OpenVideo(FVideoSource, False);
                        Result := ReadFrame(output, RasterizationCopy_);
                        exit;
                      end;
                    done := True;
                    break;
                  end;
              end;

            if (not done) then
              begin
                sws_scale(
                  FSWS_CTX,
                  @Frame^.data,
                  @Frame^.linesize,
                  0,
                  FVideoCodecCtx^.Height,
                  @FrameRGB^.data,
                  @FrameRGB^.linesize);

                if RasterizationCopy_ then
                  begin
                    output.SetSize(Width, Height);
                    CopyPtr(FrameRGB^.data[0], @output.Bits^[0], FVideoCodecCtx^.Width * FVideoCodecCtx^.Height * 4);
                  end
                else
                    output.SetWorkMemory(FrameRGB^.data[0], Width, Height);

                if (AVPacket_ptr^.pts > 0) and (av_q2d(VideoStream^.time_base) > 0) then
                    Current := AVPacket_ptr^.pts * av_q2d(VideoStream^.time_base);
                done := True;
                inc(Current_Frame);
              end;
            Result := True;
          end;

        av_packet_unref(AVPacket_ptr);
        if done then
            exit;
      end;
  except
  end;
  output.Reset;
end;

procedure TFFMPEG_Reader.Seek(second: Double);
begin
  if second = 0 then
    begin
      CloseVideo;
      OpenVideo(FVideoSource, FWorkOnGPU);
    end
  else
      av_seek_frame(FFormatCtx, -1, Round(second * AV_TIME_BASE), AVSEEK_FLAG_ANY);
end;

function TFFMPEG_Reader.Total: Double;
begin
  Result := umlMax(FFormatCtx^.duration / AV_TIME_BASE, 0);
end;

function TFFMPEG_Reader.CurrentStream_Total_Frame: int64;
begin
  Result := umlMax(VideoStream^.nb_frames, 0);
end;

function TFFMPEG_Reader.CurrentStream_PerSecond_Frame(): Double;
begin
  with VideoStream^.r_frame_rate do
      Result := umlMax(num / den, 0);
end;

function TFFMPEG_Reader.CurrentStream_PerSecond_FrameRound(): integer;
begin
  Result := Round(CurrentStream_PerSecond_Frame());
end;

procedure TFFMPEG_VideoStreamReader.DoWriteBufferBefore(var p: Pointer; var siz: NativeUInt);
begin
  if assigned(OnWriteBufferBefore) then
      OnWriteBufferBefore(Self, p, siz);
end;

procedure TFFMPEG_VideoStreamReader.DoVideoFillNewRaster(Raster: TMemoryRaster; var SaveToPool: Boolean);
begin
  if assigned(OnVideoFillNewRaster) then
      OnVideoFillNewRaster(Self, Raster, SaveToPool);
end;

procedure TFFMPEG_VideoStreamReader.DoWriteBufferAfter(p: Pointer; siz: NativeUInt; decodeFrameNum: integer);
begin
  if assigned(OnWriteBufferAfter) then
      OnWriteBufferAfter(Self, p, siz, decodeFrameNum);
end;

procedure TFFMPEG_VideoStreamReader.InternalOpenDecodec(const codec: PAVCodec);
var
  tmp: Pointer;
  AV_Options: PPAVDictionary;
begin
  FVideoCodec := codec;

  if FVideoCodec = nil then
      RaiseInfo('no found decoder', []);

  AVParser := av_parser_init(Ord(FVideoCodec^.id));
  if not assigned(AVParser) then
      RaiseInfo('Parser not found');

  FVideoCodecCtx := avcodec_alloc_context3(FVideoCodec);
  if not assigned(FVideoCodecCtx) then
      RaiseInfo('Could not allocate video Codec context');

  AV_Options := nil;
  tmp := TPascalString(IntToStr(FFMPEG_Reader_BufferSize)).BuildPlatformPChar;
  av_dict_set(@AV_Options, 'buffer_size', tmp, 0);
  TPascalString.FreePlatformPChar(tmp);

  if avcodec_open2(FVideoCodecCtx, FVideoCodec, @AV_Options) < 0 then
      RaiseInfo('Could not open Codec.');

  AVPacket_ptr := av_packet_alloc();
  Frame := av_frame_alloc();
end;

constructor TFFMPEG_VideoStreamReader.Create;
begin
  inherited Create;
  FVideoCodecCtx := nil;
  FVideoCodec := nil;
  AVParser := nil;
  AVPacket_ptr := nil;
  Frame := nil;
  FrameRGB := nil;
  FrameRGB_buffer := nil;
  FSWS_CTX := nil;

  SwapBuff := TMemoryStream64.CustomCreate(128 * 1024);
  VideoRasterPool := TMemoryRasterList.Create;
  OnWriteBufferBefore := nil;
  OnVideoFillNewRaster := nil;
  OnWriteBufferAfter := nil;
end;

destructor TFFMPEG_VideoStreamReader.Destroy;
begin
  CloseCodec();
  disposeObject(SwapBuff);

  ClearVideoPool();
  disposeObject(VideoRasterPool);
  inherited Destroy;
end;

class procedure TFFMPEG_VideoStreamReader.PrintDecodec;
var
  codec: PAVCodec;
begin
  codec := av_codec_next(nil);
  while codec <> nil do
    begin
      if av_codec_is_decoder(codec) = 1 then
          DoStatus('ID[%d] Name[%s] %s', [integer(codec^.id), string(codec^.name), string(codec^.long_name)]);
      codec := av_codec_next(codec);
    end;
end;

procedure TFFMPEG_VideoStreamReader.OpenDecodec(const codec_name: U_String);
var
  tmp: Pointer;
  codec: PAVCodec;
begin
  tmp := codec_name.BuildPlatformPChar();
  codec := avcodec_find_decoder_by_name(tmp);
  U_String.FreePlatformPChar(tmp);

  if codec = nil then
      RaiseInfo('no found decoder: %s', [codec_name.Text]);

  InternalOpenDecodec(codec);
end;

procedure TFFMPEG_VideoStreamReader.OpenDecodec(const codec_id: TAVCodecID);
begin
  InternalOpenDecodec(avcodec_find_decoder(codec_id));
end;

procedure TFFMPEG_VideoStreamReader.OpenDecodec();
begin
  OpenDecodec(AV_CODEC_ID_H264);
end;

procedure TFFMPEG_VideoStreamReader.OpenH264Decodec();
begin
  OpenDecodec(AV_CODEC_ID_H264);
end;

procedure TFFMPEG_VideoStreamReader.OpenMJPEGDecodec;
begin
  OpenDecodec(AV_CODEC_ID_MJPEG);
end;

procedure TFFMPEG_VideoStreamReader.CloseCodec;
begin
  LockObject(SwapBuff);
  if AVParser <> nil then
      av_parser_close(AVParser);

  if FVideoCodecCtx <> nil then
      avcodec_free_context(@FVideoCodecCtx);

  if Frame <> nil then
      av_frame_free(@Frame);

  if AVPacket_ptr <> nil then
      av_packet_free(@AVPacket_ptr);

  if FrameRGB_buffer <> nil then
      av_free(FrameRGB_buffer);

  if FSWS_CTX <> nil then
      sws_freeContext(FSWS_CTX);

  if FrameRGB <> nil then
      av_frame_free(@FrameRGB);

  FVideoCodecCtx := nil;
  FVideoCodec := nil;
  AVParser := nil;
  AVPacket_ptr := nil;
  Frame := nil;
  FrameRGB := nil;
  FrameRGB_buffer := nil;
  FSWS_CTX := nil;

  SwapBuff.Clear;
  UnLockObject(SwapBuff);
end;

function TFFMPEG_VideoStreamReader.WriteBuffer(p: Pointer; siz: NativeUInt): integer;
var
  decodeFrameNum: integer;

  function decode(): Boolean;
  var
    R: integer;
    numByte: integer;
    vr: TMemoryRaster;
    SaveToPool: Boolean;
  begin
    Result := False;

    R := avcodec_send_packet(FVideoCodecCtx, AVPacket_ptr);
    if R < 0 then
      begin
        RaiseInfo('Error sending a packet for decoding');
        exit;
      end;

    while R >= 0 do
      begin
        R := avcodec_receive_frame(FVideoCodecCtx, Frame);
        if (R = AVERROR_EAGAIN) or (R = AVERROR_EOF) then
            break;

        if R < 0 then
          begin
            RaiseInfo('Error during decoding');
            exit;
          end;

        // check ffmpeg color conversion and scaling
        if (FrameRGB = nil) or (FrameRGB^.Width <> Frame^.Width) or (FrameRGB^.Height <> Frame^.Height) then
          begin
            if FrameRGB <> nil then
                av_frame_free(@FrameRGB);
            if FrameRGB_buffer <> nil then
                av_free(FrameRGB_buffer);
            if FSWS_CTX <> nil then
                sws_freeContext(FSWS_CTX);

            FrameRGB := av_frame_alloc();
            numByte := avpicture_get_size(AV_PIX_FMT_RGB32, Frame^.Width, Frame^.Height);
            FrameRGB_buffer := av_malloc(numByte * sizeof(Cardinal));
            FSWS_CTX := sws_getContext(
              Frame^.Width,
              Frame^.Height,
              FVideoCodecCtx^.pix_fmt,
              Frame^.Width,
              Frame^.Height,
              AV_PIX_FMT_RGB32,
              SWS_FAST_BILINEAR,
              nil,
              nil,
              nil);
            avpicture_fill(PAVPicture(FrameRGB), FrameRGB_buffer, AV_PIX_FMT_RGB32, Frame^.Width, Frame^.Height);
            FrameRGB^.Width := Frame^.Width;
            FrameRGB^.Height := Frame^.Height;
          end;

        try
          sws_scale(
            FSWS_CTX,
            @Frame^.data,
            @Frame^.linesize,
            0,
            Frame^.Height,
            @FrameRGB^.data,
            @FrameRGB^.linesize);

          // extract frame to Raster
          vr := NewRaster();
          vr.SetSize(FrameRGB^.Width, FrameRGB^.Height);
          CopyRColor(FrameRGB^.data[0]^, vr.Bits^[0], FrameRGB^.Width * FrameRGB^.Height);

          SaveToPool := True;
          DoVideoFillNewRaster(vr, SaveToPool);
          if SaveToPool then
              VideoRasterPool.Add(vr);

          inc(decodeFrameNum);
        except
            FrameRGB := nil;
        end;
      end;

    Result := True;
  end;

var
  np: Pointer;
  nsiz: NativeUInt;
  bufPos: int64;
  R: integer;
  nbuff: TMemoryStream64;
begin
  decodeFrameNum := 0;
  Result := 0;
  if (p = nil) or (siz = 0) then
      exit;

  LockObject(SwapBuff);
  LockObject(VideoRasterPool);
  try
    np := p;
    nsiz := siz;
    DoWriteBufferBefore(np, nsiz);
    SwapBuff.Position := SwapBuff.Size;
    SwapBuff.WritePtr(np, nsiz);

    bufPos := 0;
    while SwapBuff.Size - bufPos > 0 do
      begin
        R := av_parser_parse2(AVParser, FVideoCodecCtx, @AVPacket_ptr^.data, @AVPacket_ptr^.Size,
          SwapBuff.PositionAsPtr(bufPos), SwapBuff.Size - bufPos, AV_NOPTS_VALUE, AV_NOPTS_VALUE, 0);

        if R < 0 then
            RaiseInfo('Error while parsing');

        inc(bufPos, R);

        if AVPacket_ptr^.Size <> 0 then
          if not decode() then
              break;
      end;

    if SwapBuff.Size - bufPos > 0 then
      begin
        nbuff := TMemoryStream64.CustomCreate(SwapBuff.Delta);
        SwapBuff.Position := bufPos;
        nbuff.CopyFrom(SwapBuff, SwapBuff.Size - bufPos);
        SwapBuff.NewParam(nbuff);
        nbuff.DiscardMemory;
        disposeObject(nbuff);
      end
    else
        SwapBuff.Clear;
  finally
    UnLockObject(SwapBuff);
    UnLockObject(VideoRasterPool);
  end;

  Result := decodeFrameNum;
  av_packet_unref(AVPacket_ptr);
  DoWriteBufferAfter(np, nsiz, decodeFrameNum);
end;

function TFFMPEG_VideoStreamReader.WriteBuffer(stream_: TCoreClassStream): integer;
const
  C_Chunk_Buff_Size = 1 * 1024 * 1024;
var
  tempBuff: Pointer;
  chunk: NativeInt;
begin
  if stream_ is TMemoryStream64 then
    begin
      Result := WriteBuffer(TMemoryStream64(stream_).Memory, stream_.Size);
      exit;
    end;
  if stream_ is TMemoryStream then
    begin
      Result := WriteBuffer(TMemoryStream(stream_).Memory, stream_.Size);
      exit;
    end;

  tempBuff := System.GetMemory(C_Chunk_Buff_Size);
  stream_.Position := 0;
  while (stream_.Position < stream_.Size) do
    begin
      chunk := umlMin(stream_.Size - stream_.Position, C_Chunk_Buff_Size);
      if chunk <= 0 then
          break;
      stream_.Read(tempBuff^, chunk);
      WriteBuffer(tempBuff, chunk);
    end;
  System.FreeMemory(tempBuff);
end;

function TFFMPEG_VideoStreamReader.DecodedRasterNum: integer;
begin
  LockObject(VideoRasterPool);
  Result := VideoRasterPool.Count;
  UnLockObject(VideoRasterPool);
end;

function TFFMPEG_VideoStreamReader.LockVideoPool: TMemoryRasterList;
begin
  LockObject(VideoRasterPool);
  Result := VideoRasterPool;
end;

procedure TFFMPEG_VideoStreamReader.UnLockVideoPool(freeRaster_: Boolean);
var
  i: integer;
begin
  if freeRaster_ then
    begin
      for i := 0 to VideoRasterPool.Count - 1 do
          disposeObject(VideoRasterPool[i]);
      VideoRasterPool.Clear;
    end;
  UnLockObject(VideoRasterPool);
end;

procedure TFFMPEG_VideoStreamReader.UnLockVideoPool();
begin
  UnLockVideoPool(False);
end;

procedure TFFMPEG_VideoStreamReader.ClearVideoPool;
begin
  LockVideoPool();
  UnLockVideoPool(True);
end;

initialization

// Buffer size used for online video(rtsp or rtmp), 720p 1080p 2K 4K 8K support
FFMPEG_Reader_BufferSize := 128 * 1024 * 1024; // default 128M

end.
