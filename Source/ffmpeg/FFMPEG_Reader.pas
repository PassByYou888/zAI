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

{$INCLUDE ..\zDefine.inc}

interface

uses SysUtils, CoreClasses, PascalStrings, UnicodeMixedLib, MemoryRaster, FFMPEG, DoStatusIO;

type
  TFFMPEG_Reader = class
  private
    src_filename: RawByteString;
    pFormatCtx: PAVFormatContext;
    videoCodecCtx: PAVCodecContext;
    audioCodecCtx: PAVCodecContext;
    videoCodec: PAVCodec;
    audioCodec: PAVCodec;
    pFrame: PAVFrame;
    pFrameRGB: PAVFrame;
    numBytes: integer;
    buffer: PByte;
    sws_ctx: PSwsContext;
    videoStream: integer;
    audioStream: integer;
    AVPacket_ptr: PAVPacket;
  public
    constructor Create(const FileName: TPascalString);
    destructor Destroy; override;

    function ReadFrame(output: TMemoryRaster; RasterizationCopy_: Boolean): Boolean;
    procedure Seek(second: Double);
    function Total: Double;
    function Total_Frame: int64;

  var
    Current: Double;
    Current_Frame: int64;
  end;

implementation

constructor TFFMPEG_Reader.Create(const FileName: TPascalString);
var
  i: integer;
  av_st: PPAVStream;
  p: Pointer;
begin
  inherited Create;

  pFormatCtx := nil;
  videoCodecCtx := nil;
  audioCodecCtx := nil;
  videoCodec := nil;
  audioCodec := nil;
  pFrame := nil;
  pFrameRGB := nil;
  AVPacket_ptr := nil;

  p := FileName.BuildPlatformPChar;

  // Open video file
  try
    if (avformat_open_input(@pFormatCtx, PAnsiChar(p), nil, nil) <> 0) then
      begin
        DoStatus('Could not open source file %s', [FileName.Text]);
        exit;
      end;

    // Retrieve stream information
    if avformat_find_stream_info(pFormatCtx, nil) < 0 then
      begin
        DoStatus('Could not find stream information %s', [FileName.Text]);
        exit;
      end;

    if IsConsole then
        av_dump_format(pFormatCtx, 0, PAnsiChar(p), 0);

    videoStream := -1;
    audioStream := -1;
    av_st := pFormatCtx^.streams;
    for i := 0 to pFormatCtx^.nb_streams - 1 do
      begin
        if av_st^^.codec^.codec_type = AVMEDIA_TYPE_VIDEO then
          begin
            videoStream := av_st^^.index;
            videoCodecCtx := av_st^^.codec;
          end
        else if av_st^^.codec^.codec_type = AVMEDIA_TYPE_AUDIO then
          begin
            audioStream := av_st^^.index;
            audioCodecCtx := av_st^^.codec;
          end;
        inc(av_st);
      end;

    if videoStream = -1 then
      begin
        DoStatus('Dont find a video stream');
        exit;
      end;

    videoCodec := avcodec_find_decoder(videoCodecCtx^.codec_id);
    if videoCodec = nil then
      begin
        DoStatus('Unsupported codec!');
        exit;
      end;

    if avcodec_open2(videoCodecCtx, videoCodec, nil) < 0 then
      begin
        DoStatus('Could not open codec');
        exit;
      end;

    if audioStream >= 0 then
      begin
        audioCodec := avcodec_find_decoder(audioCodecCtx^.codec_id);
        if audioCodec <> nil then
            avcodec_open2(audioCodecCtx, audioCodec, nil);
      end;

    pFrame := av_frame_alloc();

    pFrameRGB := av_frame_alloc();
    if not assigned(pFrameRGB) then
      begin
        DoStatus('Could not allocate AVFrame structure');
        exit;
      end;

    numBytes := avpicture_get_size(AV_PIX_FMT_RGB32, videoCodecCtx^.width, videoCodecCtx^.height);
    buffer := av_malloc(numBytes * sizeof(Cardinal));

    sws_ctx :=
      sws_getContext
      (
      videoCodecCtx^.width,
      videoCodecCtx^.height,
      videoCodecCtx^.pix_fmt,
      videoCodecCtx^.width,
      videoCodecCtx^.height,
      AV_PIX_FMT_RGB32,
      SWS_FAST_BILINEAR,
      nil,
      nil,
      nil
      );
    avpicture_fill(PAVPicture(pFrameRGB), buffer, AV_PIX_FMT_RGB32, videoCodecCtx^.width, videoCodecCtx^.height);
    AVPacket_ptr := av_packet_alloc();

    Current := 0;
    Current_Frame := 0;
  finally
      TPascalString.FreePlatformPChar(p);
  end;
end;

destructor TFFMPEG_Reader.Destroy;
begin
  if AVPacket_ptr <> nil then
      av_free_packet(AVPacket_ptr);

  if buffer <> nil then
      av_free(buffer);

  if pFrameRGB <> nil then
      av_free(pFrameRGB);

  if pFrame <> nil then
      av_free(pFrame);

  if videoCodecCtx <> nil then
      avcodec_close(videoCodecCtx);

  if audioCodecCtx <> nil then
      avcodec_close(audioCodecCtx);

  if pFormatCtx <> nil then
      avformat_close_input(@pFormatCtx);

  inherited Destroy;
end;

function TFFMPEG_Reader.ReadFrame(output: TMemoryRaster; RasterizationCopy_: Boolean): Boolean;
var
  frameFinished: integer;
begin
  Result := False;
  frameFinished := 0;
  try
    while (av_read_frame(pFormatCtx, AVPacket_ptr) >= 0) do
      begin
        if (AVPacket_ptr^.stream_index = videoStream) then
          begin
            avcodec_decode_video2(videoCodecCtx, pFrame, @frameFinished, AVPacket_ptr);
            if frameFinished > 0 then
              begin
                sws_scale
                  (
                  sws_ctx,
                  @pFrame^.data,
                  @pFrame^.linesize,
                  0,
                  videoCodecCtx^.height,
                  @pFrameRGB^.data,
                  @pFrameRGB^.linesize
                  );

                if RasterizationCopy_ then
                  begin
                    output.SetSize(videoCodecCtx^.width, videoCodecCtx^.height);
                    CopyRasterColor(pFrameRGB^.data[0]^, output.Bits^[0], videoCodecCtx^.width * videoCodecCtx^.height);
                  end
                else
                    output.SetWorkMemory(pFrameRGB^.data[0], videoCodecCtx^.width, videoCodecCtx^.height);

                Result := True;

                Current := AVPacket_ptr^.pts * av_q2d(pFormatCtx^.streams^^.time_base);
                inc(Current_Frame);
                exit;
              end;
          end;
      end;
  except
  end;
  output.Reset;
end;

procedure TFFMPEG_Reader.Seek(second: Double);
begin
  av_seek_frame(pFormatCtx, -1, Round(second * AV_TIME_BASE), AVSEEK_FLAG_ANY);
end;

function TFFMPEG_Reader.Total: Double;
begin
  Result := pFormatCtx^.streams^^.duration * av_q2d(pFormatCtx^.streams^^.time_base);
end;

function TFFMPEG_Reader.Total_Frame: int64;
begin
  Result := pFormatCtx^.streams^^.nb_frames;
end;

end.
