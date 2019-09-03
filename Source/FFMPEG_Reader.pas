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
    FLastFileName: TPascalString;
    src_filename: RawByteString;
    FormatCtx: PAVFormatContext;
    videoCodecCtx: PAVCodecContext;
    audioCodecCtx: PAVCodecContext;
    videoCodec: PAVCodec;
    audioCodec: PAVCodec;
    Frame, FrameRGB: PAVFrame;
    FrameRGB_buffer: PByte;
    Sws_Ctx: PSwsContext;
    videoStream: integer;
    audioStream: integer;
    AVPacket_ptr: PAVPacket;
  public
    Current: Double;
    Current_Frame: int64;
    Width, Height: integer;
    property LastFileName: TPascalString read FLastFileName;

    constructor Create(const FileName: TPascalString);
    destructor Destroy; override;

    procedure OpenVideo(const FileName: TPascalString);
    procedure CloseVideo;

    function ReadFrame(output: TMemoryRaster; RasterizationCopy_: Boolean): Boolean;

    procedure Seek(second: Double);
    function Total: Double;
    function Total_Frame: int64;
    function PerSecond_Frame(): Double;
    property PSF: Double read PerSecond_Frame;
    function PerSecond_FrameRound(): integer;
    property PSFRound: integer read PerSecond_FrameRound;
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
  public
    OnWriteBufferBefore: TOnWriteBufferBefore;
    OnVideoFillNewRaster: TOnVideoFillNewRaster;
    OnWriteBufferAfter: TOnWriteBufferAfter;

    constructor Create;
    destructor Destroy; override;

    procedure OpenCodec(const codec_id: TAVCodecID); overload;
    procedure OpenCodec(); overload; // AV_CODEC_ID_H264
    procedure CloseCodec;

    // parser and decode frame
    // return decode frame number on this step
    function WriteBuffer(p: Pointer; siz: NativeUInt): integer; virtual;

    function LockVideoPool: TMemoryRasterList;
    procedure UnLockVideoPool(freeRaster_: Boolean); overload;
    procedure UnLockVideoPool(); overload;
    procedure ClearVideoPool;
  end;

function ExtractVideoAsH264(VideoSource: TPascalString; dest: TCoreClassStream): integer; overload;
function ExtractVideoAsH264(VideoSource, DestH264: TPascalString): integer; overload;

implementation

uses H264;

function ExtractVideoAsH264(VideoSource: TPascalString; dest: TCoreClassStream): integer;
var
  ff: TFFMPEG_Reader;
  h: TH264Writer;
  Raster: TMemoryRaster;
  tk: TTimeTick;
begin
  DoStatus('ffmpeg open ', [VideoSource.Text]);
  ff := TFFMPEG_Reader.Create(VideoSource);
  DoStatus('create h264 stream %d*%d total: %d', [ff.Width, ff.Height, ff.Total_Frame]);
  h := TH264Writer.Create(ff.Width, ff.Height, ff.Total_Frame, ff.PerSecond_Frame, dest);
  Raster := TMemoryRaster.Create;
  tk := GetTimeTick();
  while ff.ReadFrame(Raster, False) do
    begin
      h.WriteFrame(Raster);
      if GetTimeTick() - tk > 2000 then
        begin
          DoStatus('%s -> h264.stream progress %d/%d', [umlGetFileName(VideoSource).Text, h.FrameCount, ff.Total_Frame]);
          h.Flush;
          tk := GetTimeTick();
        end;
    end;
  Result := h.FrameCount;
  disposeObject(ff);
  disposeObject(h);
  DoStatus('done %s -> h264 stream.', [umlGetFileName(VideoSource).Text]);
end;

function ExtractVideoAsH264(VideoSource, DestH264: TPascalString): integer;
var
  ff: TFFMPEG_Reader;
  h: TH264Writer;
  Raster: TMemoryRaster;
  tk: TTimeTick;
begin
  DoStatus('ffmpeg open ', [VideoSource.Text]);
  ff := TFFMPEG_Reader.Create(VideoSource);
  DoStatus('create h264 stream %d*%d total: %d', [ff.Width, ff.Height, ff.Total_Frame]);
  h := TH264Writer.Create(ff.Width, ff.Height, ff.Total_Frame, ff.PerSecond_Frame, DestH264);
  Raster := TMemoryRaster.Create;
  tk := GetTimeTick();
  while ff.ReadFrame(Raster, False) do
    begin
      h.WriteFrame(Raster);
      if GetTimeTick() - tk > 2000 then
        begin
          DoStatus('%s -> %s progress %d/%d', [umlGetFileName(VideoSource).Text, umlGetFileName(DestH264).Text, h.FrameCount, ff.Total_Frame]);
          h.Flush;
          tk := GetTimeTick();
        end;
    end;
  Result := h.FrameCount;
  disposeObject(ff);
  disposeObject(h);
  DoStatus('done %s -> %s', [umlGetFileName(VideoSource).Text, umlGetFileName(DestH264).Text]);
end;

constructor TFFMPEG_Reader.Create(const FileName: TPascalString);
begin
  inherited Create;
  OpenVideo(FileName);
end;

destructor TFFMPEG_Reader.Destroy;
begin
  CloseVideo;
  inherited Destroy;
end;

procedure TFFMPEG_Reader.OpenVideo(const FileName: TPascalString);

var
  i: integer;
  av_st: PPAVStream;
  p: Pointer;
  numByte: integer;
begin
  FLastFileName := FileName;

  FormatCtx := nil;
  videoCodecCtx := nil;
  audioCodecCtx := nil;
  videoCodec := nil;
  audioCodec := nil;
  Frame := nil;
  FrameRGB := nil;
  AVPacket_ptr := nil;
  Sws_Ctx := nil;

  p := FileName.BuildPlatformPChar;

  // Open video file
  try
    if (avformat_open_input(@FormatCtx, PAnsiChar(p), nil, nil) <> 0) then
      begin
        RaiseInfo('Could not open source file %s', [FileName.Text]);
        exit;
      end;

    // Retrieve stream information
    if avformat_find_stream_info(FormatCtx, nil) < 0 then
      begin
        if FormatCtx <> nil then
            avformat_close_input(@FormatCtx);

        RaiseInfo('Could not find stream information %s', [FileName.Text]);
        exit;
      end;

    if IsConsole then
        av_dump_format(FormatCtx, 0, PAnsiChar(p), 0);

    videoStream := -1;
    audioStream := -1;
    av_st := FormatCtx^.streams;
    for i := 0 to FormatCtx^.nb_streams - 1 do
      begin
        if av_st^^.Codec^.codec_type = AVMEDIA_TYPE_VIDEO then
          begin
            videoStream := av_st^^.index;
            videoCodecCtx := av_st^^.Codec;
          end
        else if av_st^^.Codec^.codec_type = AVMEDIA_TYPE_AUDIO then
          begin
            audioStream := av_st^^.index;
            audioCodecCtx := av_st^^.Codec;
          end;
        inc(av_st);
      end;

    if videoStream = -1 then
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

    if avcodec_open2(videoCodecCtx, videoCodec, nil) < 0 then
      begin
        RaiseInfo('Could not open videoCodec');
        exit;
      end;

    if audioStream >= 0 then
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
      SWS_BILINEAR,
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

function TFFMPEG_Reader.ReadFrame(output: TMemoryRaster; RasterizationCopy_: Boolean): Boolean;

var
  frameFinished: integer;
begin
  Result := False;
  frameFinished := 0;
  try
    while (av_read_frame(FormatCtx, AVPacket_ptr) >= 0) do
      begin
        if (AVPacket_ptr^.stream_index = videoStream) then
          begin
            avcodec_decode_video2(videoCodecCtx, Frame, @frameFinished, AVPacket_ptr);
            if frameFinished > 0 then
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

                Result := True;

                Current := AVPacket_ptr^.pts * av_q2d(FormatCtx^.streams^^.time_base);
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
  if Current = second then
      exit;

  if second = 0 then
    begin
      CloseVideo;
      OpenVideo(FLastFileName);
      exit;
    end;

  av_seek_frame(FormatCtx, -1, Round(second * AV_TIME_BASE), AVSEEK_FLAG_ANY);
end;

function TFFMPEG_Reader.Total: Double;
begin
  Result := FormatCtx^.streams^^.duration * av_q2d(FormatCtx^.streams^^.time_base);
end;

function TFFMPEG_Reader.Total_Frame: int64;
begin
  Result := FormatCtx^.streams^^.nb_frames;
end;

function TFFMPEG_Reader.PerSecond_Frame(): Double;
begin
  with FormatCtx^.streams^^.r_frame_rate do
      Result := num / den;
end;

function TFFMPEG_Reader.PerSecond_FrameRound(): integer;
begin
  Result := Round(PerSecond_Frame());
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

procedure TFFMPEG_VideoStreamReader.OpenCodec(const codec_id: TAVCodecID);
begin
  AVPacket_ptr := av_packet_alloc();
  videoCodec := avcodec_find_decoder(codec_id);

  if not assigned(videoCodec) then
      RaiseInfo('not found Codec.');

  AVParser := av_parser_init(Ord(videoCodec^.id));
  if not assigned(AVParser) then
      RaiseInfo('Parser not found');

  videoCodecCtx := avcodec_alloc_context3(videoCodec);
  if not assigned(videoCodecCtx) then
      RaiseInfo('Could not allocate video Codec context');

  if avcodec_open2(videoCodecCtx, videoCodec, nil) < 0 then
      RaiseInfo('Could not open Codec.');

  Frame := av_frame_alloc();
end;

procedure TFFMPEG_VideoStreamReader.OpenCodec();
begin
  OpenCodec(AV_CODEC_ID_H264);
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
              SWS_BILINEAR,
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

end.
