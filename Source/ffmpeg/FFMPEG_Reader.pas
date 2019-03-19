{ ****************************************************************************** }
{ * FFMPEG video Reader support       by qq600585                              * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit FFMPEG_Reader;

{$INCLUDE ..\zDefine.inc}

interface

uses CoreClasses, PascalStrings, UnicodeMixedLib, MemoryRaster, FFMPEG, DoStatusIO;

type
  TFFMPEG_Reader = class
  private
    src_filename: RawByteString;
    pFormatCtx: PAVFormatContext;
    pCodecCtx: PAVCodecContext;
    pCodec: PAVCodec;
    optionsDict: PAVDictionary;
    pFrame: PAVFrame;
    pFrameRGB: PAVFrame;
    numBytes: integer;
    buffer: PByte;
    sws_ctx: PSwsContext;
    videoStream: integer;
  public
    constructor Create(const FileName: RawByteString);
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

constructor TFFMPEG_Reader.Create(const FileName: RawByteString);
var
  i: integer;
begin
  inherited Create;

  // Open video file
  if (avformat_open_input(@pFormatCtx, PAnsiChar(FileName), nil, nil) <> 0) then
    begin
      RaiseInfo('Could not open source file %s', [FileName]);
      exit;
    end;

  // Retrieve stream information
  if avformat_find_stream_info(pFormatCtx, nil) < 0 then
    begin
      RaiseInfo('Could not find stream information %s', [FileName]);
      exit;
    end;

  // Dump information about file onto standard error
  av_dump_format(pFormatCtx, 0, PAnsiChar(FileName), 0);

  // Find the first video stream
  videoStream := -1;
  for i := 0 to pFormatCtx^.nb_streams - 1 do
    begin
      if pFormatCtx^.streams^^.codec^.codec_type = AVMEDIA_TYPE_VIDEO then
        begin
          videoStream := i;
          // Get a pointer to the codec context for the video stream
          pCodecCtx := pFormatCtx^.streams^^.codec;
          break;
        end
      else
          inc(pFormatCtx^.streams);
    end;

  if videoStream = -1 then
    begin
      RaiseInfo('Dont find a video stream');
      exit;
    end;

  // Find the decoder for the video stream
  pCodec := avcodec_find_decoder(pCodecCtx^.codec_id);
  if not assigned(pCodec) then
    begin
      RaiseInfo('Unsupported codec!');
      exit;
    end;

  // Open codec
  if avcodec_open2(pCodecCtx, pCodec, @optionsDict) < 0 then
    begin
      RaiseInfo('Could not open codec');
      exit;
    end;

  // Allocate video frame
  pFrame := av_frame_alloc;

  // Allocate an AVFrame structure
  pFrameRGB := av_frame_alloc();
  if not assigned(pFrameRGB) then
    begin
      RaiseInfo('Could not allocate AVFrame structure');
      exit;
    end;

  // Determine required buffer size and allocate buffer
  numBytes := avpicture_get_size(AV_PIX_FMT_RGB32, pCodecCtx^.width, pCodecCtx^.height);
  buffer := av_malloc(numBytes * sizeof(cardinal));

  sws_ctx :=
    sws_getContext
    (
    pCodecCtx^.width,
    pCodecCtx^.height,
    pCodecCtx^.pix_fmt,
    pCodecCtx^.width,
    pCodecCtx^.height,
    AV_PIX_FMT_RGB32,
    SWS_FAST_BILINEAR,
    nil,
    nil,
    nil
    );
  avpicture_fill(PAVPicture(pFrameRGB), buffer, AV_PIX_FMT_RGB32, pCodecCtx^.width, pCodecCtx^.height);

  Current := 0;
  Current_Frame := 0;
end;

destructor TFFMPEG_Reader.Destroy;
begin
  av_free(buffer);
  av_free(pFrameRGB);
  av_free(pFrame);
  avcodec_close(pCodecCtx);
  avformat_close_input(@pFormatCtx);
  inherited Destroy;
end;

function TFFMPEG_Reader.ReadFrame(output: TMemoryRaster; RasterizationCopy_: Boolean): Boolean;
var
  packet: TAVPacket;
  frameFinished: integer;
begin
  Result := False;
  frameFinished := 0;
  while (av_read_frame(pFormatCtx, @packet) >= 0) do
    begin
      if (packet.stream_index = videoStream) then
        begin
          avcodec_decode_video2(pCodecCtx, pFrame, @frameFinished, @packet);
          if frameFinished > 0 then
            begin
              sws_scale
                (
                sws_ctx,
                @pFrame^.data,
                @pFrame^.linesize,
                0,
                pCodecCtx^.height,
                @pFrameRGB^.data,
                @pFrameRGB^.linesize
                );

              if RasterizationCopy_ then
                begin
                  output.SetSize(pCodecCtx^.width, pCodecCtx^.height);
                  CopyRasterColor(pFrameRGB^.data[0]^, output.Bits^[0], pCodecCtx^.width * pCodecCtx^.height);
                end
              else
                  output.SetWorkMemory(pFrameRGB^.data[0], pCodecCtx^.width, pCodecCtx^.height);

              Result := True;

              Current := packet.pts * av_q2d(pFormatCtx^.streams^^.time_base);
              inc(Current_Frame);
              av_free_packet(@packet);
              exit;
            end;
        end;
      av_free_packet(@packet);
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
