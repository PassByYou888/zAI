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

uses SysUtils, CoreClasses, PascalStrings, UnicodeMixedLib, MemoryStream64, MemoryRaster, DoStatusIO, FFMPEG;

type
  TFFMPEG_Reader = class;
  TFFMPEG_VideoStreamReader = class;

  TFFMPEG_Reader = class(TCoreClassObject)
  private
    FVideoSource: TPascalString;
    FWorkOnGPU: Boolean;
    FormatCtx: PAVFormatContext;
    videoCodecCtx: PAVCodecContext;
    audioCodecCtx: PAVCodecContext;
    videoCodec: PAVCodec;
    audioCodec: PAVCodec;
    Frame, FrameRGB: PAVFrame;
    FrameRGB_buffer: PByte;
    Sws_Ctx: PSwsContext;
    videoStreamIndex: integer;
    audioStreamIndex: integer;
    videoStream: PAVStream;
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
    videoCodecCtx: PAVCodecContext;
    videoCodec: PAVCodec;
    AVParser: PAVCodecParserContext;
    AVPacket_ptr: PAVPacket;
    Frame, FrameRGB: PAVFrame;
    FrameRGB_buffer: PByte;
    Sws_Ctx: PSwsContext;
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
    function WriteBuffer(p: Pointer; siz: NativeUInt): integer; virtual;

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

uses H264, FFMPEG_Writer;

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
  fs := TCoreClassFileStream.Create(DestH264, fmCreate);
  try
      Result := ExtractVideoAsH264(VideoSource_, fs, Bitrate);
  except
  end;
  disposeObject(fs);
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
  FormatCtx := nil;
  videoCodecCtx := nil;
  audioCodecCtx := nil;
  videoCodec := nil;
  audioCodec := nil;
  Frame := nil;
  FrameRGB := nil;
  AVPacket_ptr := nil;
  Sws_Ctx := nil;

  p := VideoSource_.BuildPlatformPChar;

  // Open video file
  try
    tmp := TPascalString(IntToStr(FFMPEG_Reader_BufferSize)).BuildPlatformPChar;
    av_dict_set(@AV_Options, 'buffer_size', tmp, 0);
    av_dict_set(@AV_Options, 'stimeout', '6000000', 0);
    av_dict_set(@AV_Options, 'rtsp_flags', '+prefer_tcp', 0);
    av_dict_set(@AV_Options, 'rtsp_transport', '+tcp', 0);
    TPascalString.FreePlatformPChar(tmp);

    if (avformat_open_input(@FormatCtx, PAnsiChar(p), nil, @AV_Options) <> 0) then
      begin
        RaiseInfo('Could not open source file %s', [VideoSource_.Text]);
        exit;
      end;

    // Retrieve stream information
    if avformat_find_stream_info(FormatCtx, nil) < 0 then
      begin
        if FormatCtx <> nil then
            avformat_close_input(@FormatCtx);

        RaiseInfo('Could not find stream information %s', [VideoSource_.Text]);
        exit;
      end;

    if IsConsole then
        av_dump_format(FormatCtx, 0, PAnsiChar(p), 0);

    videoStreamIndex := -1;
    audioStreamIndex := -1;
    videoStream := nil;
    av_st := FormatCtx^.streams;
    for i := 0 to FormatCtx^.nb_streams - 1 do
      begin
        if av_st^^.codec^.codec_type = AVMEDIA_TYPE_VIDEO then
          begin
            videoStreamIndex := av_st^^.index;
            videoCodecCtx := av_st^^.codec;
            videoStream := av_st^;
          end
        else if av_st^^.codec^.codec_type = AVMEDIA_TYPE_AUDIO then
          begin
            audioStreamIndex := av_st^^.index;
            audioCodecCtx := av_st^^.codec;
          end;
        inc(av_st);
      end;

    if videoStreamIndex = -1 then
      begin
        RaiseInfo('Dont find a video stream');
        exit;
      end;

    videoCodec := avcodec_find_decoder(videoCodecCtx^.codec_id);
    if videoCodec = nil then
      begin
        RaiseInfo('Unsupported videoCodec!');
        exit;
      end;

    if (useGPU_) and (CurrentPlatform in [epWin32, epWin64]) and (videoCodecCtx^.codec_id in [AV_CODEC_ID_H264, AV_CODEC_ID_HEVC]) then
      begin
        gpu_decodec := nil;
        if videoCodecCtx^.codec_id = AV_CODEC_ID_H264 then
            gpu_decodec := avcodec_find_decoder_by_name('h264_cuvid')
        else if videoCodecCtx^.codec_id = AV_CODEC_ID_HEVC then
            gpu_decodec := avcodec_find_decoder_by_name('hevc_cuvid');

        if (avcodec_open2(videoCodecCtx, gpu_decodec, nil) < 0) then
          begin
            if avcodec_open2(videoCodecCtx, videoCodec, nil) < 0 then
              begin
                RaiseInfo('Could not open videoCodec');
                exit;
              end;
          end
        else
          begin
            videoCodec := gpu_decodec;
            FWorkOnGPU := True;
          end;
      end
    else
      begin
        if avcodec_open2(videoCodecCtx, videoCodec, nil) < 0 then
          begin
            RaiseInfo('Could not open videoCodec');
            exit;
          end;
      end;

    if audioStreamIndex >= 0 then
      begin
        audioCodec := avcodec_find_decoder(audioCodecCtx^.codec_id);
        if audioCodec <> nil then
            avcodec_open2(audioCodecCtx, audioCodec, nil);
      end;

    Frame := av_frame_alloc();
    FrameRGB := av_frame_alloc();
    if (FrameRGB = nil) or (Frame = nil) then
        RaiseInfo('Could not allocate AVFrame structure');
    numByte := avpicture_get_size(AV_PIX_FMT_RGB32, videoCodecCtx^.Width, videoCodecCtx^.Height);
    FrameRGB_buffer := av_malloc(numByte * sizeof(Cardinal));
    Sws_Ctx := sws_getContext(
      videoCodecCtx^.Width,
      videoCodecCtx^.Height,
      videoCodecCtx^.pix_fmt,
      videoCodecCtx^.Width,
      videoCodecCtx^.Height,
      AV_PIX_FMT_RGB32,
      SWS_FAST_BILINEAR,
      nil,
      nil,
      nil);
    avpicture_fill(PAVPicture(FrameRGB), FrameRGB_buffer, AV_PIX_FMT_RGB32, videoCodecCtx^.Width, videoCodecCtx^.Height);

    AVPacket_ptr := av_packet_alloc();

    Width := videoCodecCtx^.Width;
    Height := videoCodecCtx^.Height;

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

  if videoCodecCtx <> nil then
      avcodec_close(videoCodecCtx);

  if audioCodecCtx <> nil then
      avcodec_close(audioCodecCtx);

  if FormatCtx <> nil then
      avformat_close_input(@FormatCtx);

  if Sws_Ctx <> nil then
      sws_freeContext(Sws_Ctx);

  FormatCtx := nil;
  videoCodecCtx := nil;
  audioCodecCtx := nil;
  videoCodec := nil;
  audioCodec := nil;
  Frame := nil;
  FrameRGB := nil;
  AVPacket_ptr := nil;
  Sws_Ctx := nil;
end;

function TFFMPEG_Reader.NextFrame(): Boolean;
var
  done: Boolean;
  r: integer;
begin
  Result := False;
  done := False;
  try
    while (av_read_frame(FormatCtx, AVPacket_ptr) >= 0) do
      begin
        if (AVPacket_ptr^.stream_index = videoStreamIndex) then
          begin
            r := avcodec_send_packet(videoCodecCtx, AVPacket_ptr);
            if r < 0 then
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
                r := avcodec_receive_frame(videoCodecCtx, Frame);

                // success, a frame was returned
                if r = 0 then
                    break;

                // AVERROR(EAGAIN): output is not available in this state - user must try to send new input
                if r = AVERROR_EAGAIN then
                  begin
                    av_packet_unref(AVPacket_ptr);
                    Result := NextFrame();
                    exit;
                  end;

                // AVERROR_EOF: the decoder has been fully flushed, and there will be no more output frames
                if r = AVERROR_EOF then
                  begin
                    avcodec_flush_buffers(videoCodecCtx);
                    continue;
                  end;

                // error
                if r < 0 then
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
  r: integer;
begin
  Result := False;
  done := False;
  try
    while (av_read_frame(FormatCtx, AVPacket_ptr) >= 0) do
      begin
        if (AVPacket_ptr^.stream_index = videoStreamIndex) then
          begin
            r := avcodec_send_packet(videoCodecCtx, AVPacket_ptr);
            if r < 0 then
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
                r := avcodec_receive_frame(videoCodecCtx, Frame);

                // success, a frame was returned
                if r = 0 then
                    break;

                // AVERROR(EAGAIN): output is not available in this state - user must try to send new input
                if r = AVERROR_EAGAIN then
                  begin
                    av_packet_unref(AVPacket_ptr);
                    Result := ReadFrame(output, RasterizationCopy_);
                    exit;
                  end;

                // AVERROR_EOF: the decoder has been fully flushed, and there will be no more output frames
                if r = AVERROR_EOF then
                  begin
                    avcodec_flush_buffers(videoCodecCtx);
                    continue;
                  end;

                // error
                if r < 0 then
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
                  Sws_Ctx,
                  @Frame^.data,
                  @Frame^.linesize,
                  0,
                  videoCodecCtx^.Height,
                  @FrameRGB^.data,
                  @FrameRGB^.linesize);

                if RasterizationCopy_ then
                  begin
                    output.SetSize(videoCodecCtx^.Width, videoCodecCtx^.Height);
                    CopyRColor(FrameRGB^.data[0]^, output.Bits^[0], videoCodecCtx^.Width * videoCodecCtx^.Height);
                  end
                else
                    output.SetWorkMemory(FrameRGB^.data[0], videoCodecCtx^.Width, videoCodecCtx^.Height);

                if (AVPacket_ptr^.pts > 0) and (av_q2d(videoStream^.time_base) > 0) then
                    Current := AVPacket_ptr^.pts * av_q2d(videoStream^.time_base);
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
      av_seek_frame(FormatCtx, -1, Round(second * AV_TIME_BASE), AVSEEK_FLAG_ANY);
end;

function TFFMPEG_Reader.Total: Double;
begin
  Result := umlMax(FormatCtx^.duration / AV_TIME_BASE, 0);
end;

function TFFMPEG_Reader.CurrentStream_Total_Frame: int64;
begin
  Result := umlMax(videoStream^.nb_frames, 0);
end;

function TFFMPEG_Reader.CurrentStream_PerSecond_Frame(): Double;
begin
  with videoStream^.r_frame_rate do
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
  videoCodec := codec;

  if videoCodec = nil then
      RaiseInfo('no found decoder', []);

  AVParser := av_parser_init(Ord(videoCodec^.id));
  if not assigned(AVParser) then
      RaiseInfo('Parser not found');

  videoCodecCtx := avcodec_alloc_context3(videoCodec);
  if not assigned(videoCodecCtx) then
      RaiseInfo('Could not allocate video Codec context');

  AV_Options := nil;
  tmp := TPascalString(IntToStr(FFMPEG_Reader_BufferSize)).BuildPlatformPChar;
  av_dict_set(@AV_Options, 'buffer_size', tmp, 0);
  TPascalString.FreePlatformPChar(tmp);

  if avcodec_open2(videoCodecCtx, videoCodec, @AV_Options) < 0 then
      RaiseInfo('Could not open Codec.');

  AVPacket_ptr := av_packet_alloc();
  Frame := av_frame_alloc();
end;

constructor TFFMPEG_VideoStreamReader.Create;
begin
  inherited Create;
  videoCodecCtx := nil;
  videoCodec := nil;
  AVParser := nil;
  AVPacket_ptr := nil;
  Frame := nil;
  FrameRGB := nil;
  FrameRGB_buffer := nil;
  Sws_Ctx := nil;

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

  if videoCodecCtx <> nil then
      avcodec_free_context(@videoCodecCtx);

  if Frame <> nil then
      av_frame_free(@Frame);

  if AVPacket_ptr <> nil then
      av_packet_free(@AVPacket_ptr);

  if FrameRGB_buffer <> nil then
      av_free(FrameRGB_buffer);

  if Sws_Ctx <> nil then
      sws_freeContext(Sws_Ctx);

  if FrameRGB <> nil then
      av_frame_free(@FrameRGB);

  videoCodecCtx := nil;
  videoCodec := nil;
  AVParser := nil;
  AVPacket_ptr := nil;
  Frame := nil;
  FrameRGB := nil;
  FrameRGB_buffer := nil;
  Sws_Ctx := nil;

  SwapBuff.Clear;
  UnLockObject(SwapBuff);
end;

function TFFMPEG_VideoStreamReader.WriteBuffer(p: Pointer; siz: NativeUInt): integer;
var
  decodeFrameNum: integer;

  function decode(): Boolean;
  var
    r: integer;
    numByte: integer;
    vr: TMemoryRaster;
    SaveToPool: Boolean;
  begin
    Result := False;

    r := avcodec_send_packet(videoCodecCtx, AVPacket_ptr);
    if r < 0 then
      begin
        RaiseInfo('Error sending a packet for decoding');
        exit;
      end;

    while r >= 0 do
      begin
        r := avcodec_receive_frame(videoCodecCtx, Frame);
        if (r = AVERROR_EAGAIN) or (r = AVERROR_EOF) then
            break;

        if r < 0 then
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
            if Sws_Ctx <> nil then
                sws_freeContext(Sws_Ctx);

            FrameRGB := av_frame_alloc();
            numByte := avpicture_get_size(AV_PIX_FMT_RGB32, Frame^.Width, Frame^.Height);
            FrameRGB_buffer := av_malloc(numByte * sizeof(Cardinal));
            Sws_Ctx := sws_getContext(
              Frame^.Width,
              Frame^.Height,
              videoCodecCtx^.pix_fmt,
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
            Sws_Ctx,
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
  r: integer;
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
        r := av_parser_parse2(AVParser, videoCodecCtx, @AVPacket_ptr^.data, @AVPacket_ptr^.Size,
          SwapBuff.PositionAsPtr(bufPos), SwapBuff.Size - bufPos, AV_NOPTS_VALUE, AV_NOPTS_VALUE, 0);

        if r < 0 then
            RaiseInfo('Error while parsing');

        inc(bufPos, r);

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
