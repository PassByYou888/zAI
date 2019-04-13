{ ****************************************************************************** }
{ * Yuv for Mpeg               by qq600585                                     * }
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

unit Y4M;

{$INCLUDE zDefine.inc}

interface

uses SysUtils, CoreClasses, PascalStrings, UnicodeMixedLib, MemoryRaster, h264Image, h264Types;

type
  TY4MReader = class
  private
    FWidth, FHeight: uint16_t;
    FFrameCount: uint32_t;
    FCurrentFrame: uint32_t;
    FFrameRate: Single;
    ioHandle: TIOHnd;
    FStartPos: Int64_t;
    FFileHeaderSize: Word;
    FFrameSize: uint32_t;
    FImage: TPlanarImage;
    procedure ParseHeader;
  public
    constructor Create(const FileName: TPascalString); overload;
    constructor Create(const stream: TCoreClassStream; const autoFreeSteam: Boolean); overload;
    destructor Destroy; override;

    procedure SeekFirstFrame;
    function ReadFrame: TPlanarImage; overload;
    function ReadFrame(frameIndex: uint32_t): TPlanarImage; overload;
    property Image: TPlanarImage read FImage;

    property width: uint16_t read FWidth;
    property height: uint16_t read FHeight;
    property FrameCount: uint32_t read FFrameCount;
    property CurrentFrame: uint32_t read FCurrentFrame;
    property FrameRate: Single read FFrameRate;
  end;

  TY4MWriter = class
  private
    ioHandle: TIOHnd;
    FPerSecondFrame: uint16_t;
    FFrameCount: uint32_t;
    FImage: TPlanarImage;
  public
    (*
      w: frame FWidth
      h: frame FHeight
      psf: per second frame
      filename: output filename
    *)
    constructor Create(const w, h, psf: uint16_t; const FileName: TPascalString); overload;

    (*
      w: frame FWidth
      h: frame FHeight
      psf: per second frame
      stream: output stream
    *)
    constructor Create(const w, h, psf: uint16_t; const stream: TCoreClassStream); overload;

    destructor Destroy; override;

    procedure WriteFrame(raster: TMemoryRaster);
    procedure Flush;
    property PerSecondFrame: uint16_t read FPerSecondFrame;
    property FrameCount: uint32_t read FFrameCount;
    function Y4MSize: Int64_t;
  end;

implementation

type
  TY4MToken = array [0 .. 8] of uint8_t;
  TFRAME_MAGIC = array [0 .. 5] of uint8_t;

var
  Y4M_Token: TY4MToken = ($59, $55, $56, $34, $4D, $50, $45, $47, $32);
  FRAME_MAGIC: TFRAME_MAGIC = ($46, $52, $41, $4D, $45, $0A);
  FRAME_MAGIC_SIZE: int32_t = 6;

procedure TY4MReader.ParseHeader;
var
  i, Num, DENOM: Integer;
  c, param_c: uint8_t;
  token: TY4MToken;
  s: TPascalString;
begin
  umlBlockRead(ioHandle, token[0], 9);

  if not CompareMemory(@token[0], @Y4M_Token[0], 9) then
      RaiseInfo('Not a Y4M format');

  umlBlockRead(ioHandle, c, 1);

  repeat
    umlBlockRead(ioHandle, param_c, 1);
    s := '';
    umlBlockRead(ioHandle, c, 1);
    repeat
      s.Append(SystemChar(c));
      umlBlockRead(ioHandle, c, 1);
    until (c = 10) or (c = 32);
    case param_c of
      Ord('W'):
        FWidth := umlStrToInt(s);
      Ord('H'):
        FHeight := umlStrToInt(s);
      Ord('F'):
        begin
          Num := umlStrToInt(umlGetFirstStr(s, ':'));
          DENOM := umlStrToInt(umlGetLastStr(s, ':'));
          FFrameRate := Num / DENOM;
        end;
    end;
  until c = 10;

  FFileHeaderSize := umlFilePOS(ioHandle);
end;

constructor TY4MReader.Create(const FileName: TPascalString);
begin
  inherited Create;
  FFrameCount := 0;
  FCurrentFrame := 0;

  umlFileOpen(FileName, ioHandle, True);
  ParseHeader;

  FFrameSize := FWidth * FHeight + (FWidth * FHeight div 2);
  FFrameCount := (umlFileSize(ioHandle) - FFileHeaderSize) div (FRAME_MAGIC_SIZE + Int64(FFrameSize));

  FStartPos := umlFileGetPOS(ioHandle);
  FImage := TPlanarImage.Create(FWidth, FHeight);
end;

constructor TY4MReader.Create(const stream: TCoreClassStream; const autoFreeSteam: Boolean);
begin
  inherited Create;
  FFrameCount := 0;
  FCurrentFrame := 0;

  umlFileOpenAsStream('stream', stream, ioHandle, True);
  ioHandle.AutoFree := autoFreeSteam;
  ParseHeader;

  FFrameSize := FWidth * FHeight + (FWidth * FHeight div 2);
  FFrameCount := (umlFileSize(ioHandle) - FFileHeaderSize) div (FRAME_MAGIC_SIZE + Int64(FFrameSize));

  FStartPos := umlFileGetPOS(ioHandle);
  FImage := TPlanarImage.Create(FWidth, FHeight);
end;

destructor TY4MReader.Destroy;
begin
  DisposeObject(FImage);
  umlFileClose(ioHandle);
  inherited Destroy;
end;

procedure TY4MReader.SeekFirstFrame;
begin
  umlFileSeek(ioHandle, FStartPos);
  FCurrentFrame := 0;
end;

function TY4MReader.ReadFrame: TPlanarImage;
var
  magic: TFRAME_MAGIC;
begin
  umlBlockRead(ioHandle, magic[0], FRAME_MAGIC_SIZE);
  if not CompareMemory(@magic[0], @FRAME_MAGIC[0], FRAME_MAGIC_SIZE) then
      RaiseInfo('Not a Y4M Frame');
  umlBlockRead(ioHandle, FImage.plane[0]^, FFrameSize);
  inc(FCurrentFrame);
  FImage.frame_num := FCurrentFrame;
  Result := FImage;
end;

function TY4MReader.ReadFrame(frameIndex: uint32_t): TPlanarImage;
var
  FP: Int64_t;
  magic: TFRAME_MAGIC;
begin
  Result := FImage;
  if frameIndex >= FFrameCount then
      Exit;
  if FCurrentFrame = frameIndex then
      Exit;

  FCurrentFrame := frameIndex;
  FP := FStartPos + frameIndex * (FFrameSize + FRAME_MAGIC_SIZE);
  umlFileSeek(ioHandle, FP);

  umlBlockRead(ioHandle, magic[0], FRAME_MAGIC_SIZE);
  if not CompareMemory(@magic[0], @FRAME_MAGIC[0], FRAME_MAGIC_SIZE) then
      RaiseInfo('Not a Y4M Frame');
  umlBlockRead(ioHandle, FImage.plane[0]^, FFrameSize);
  FImage.frame_num := FCurrentFrame;
  Result := FImage;
end;

constructor TY4MWriter.Create(const w, h, psf: uint16_t; const FileName: TPascalString);
var
  s: TPascalString;
  b: TBytes;
begin
  inherited Create;
  s := PFormat('YUV4MPEG2 W%d H%d F%d:1 Ip A0:0' + #10, [w, h, psf]);
  s.FastGetBytes(b);

  umlFileCreate(FileName, ioHandle);
  umlBlockWrite(ioHandle, b[0], length(b));

  SetLength(b, 0);
  s := '';

  FPerSecondFrame := psf;
  FFrameCount := 0;
  FImage := TPlanarImage.Create(w, h);
end;

constructor TY4MWriter.Create(const w, h, psf: uint16_t; const stream: TCoreClassStream);
var
  s: TPascalString;
  b: TBytes;
begin
  inherited Create;
  s := PFormat('YUV4MPEG2 W%d H%d F%d:1 Ip A0:0' + #10, [w, h, psf]);
  s.FastGetBytes(b);

  umlFileCreateAsStream('stream', stream, ioHandle);
  umlBlockWrite(ioHandle, b[0], length(b));

  SetLength(b, 0);
  s := '';

  FPerSecondFrame := psf;
  FFrameCount := 0;
  FImage := TPlanarImage.Create(w, h);
end;

destructor TY4MWriter.Destroy;
begin
  DisposeObject(FImage);
  umlFileClose(ioHandle);
  inherited Destroy;
end;

procedure TY4MWriter.WriteFrame(raster: TMemoryRaster);
var
  FrameSize: int32_t;
begin
  umlBlockWrite(ioHandle, FRAME_MAGIC[0], FRAME_MAGIC_SIZE);
  FrameSize := FImage.width * FImage.height + (FImage.width * FImage.height div 2);
  FImage.LoadFromRaster(raster);
  umlBlockWrite(ioHandle, FImage.plane[0]^, FrameSize);
  inc(FFrameCount);
end;

procedure TY4MWriter.Flush;
begin
  umlFileFlushWrite(ioHandle);
end;

function TY4MWriter.Y4MSize: Int64_t;
begin
  Result := umlFileSize(ioHandle);
end;

end.
