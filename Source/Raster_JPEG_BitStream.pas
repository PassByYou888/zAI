{ ****************************************************************************** }
{ * memory Rasterization JPEG support                                          * }
{ * by QQ 600585@qq.com                                                        * }
{ ****************************************************************************** }
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
unit Raster_JPEG_BitStream;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, PascalStrings, MemoryStream64, UnicodeMixedLib, DoStatusIO, Raster_JPEG_type;

type
  TBitReader = class(TJPEG_Persistent)
  private
    FBits: Cardinal;
    FBitsLeft: Integer;
    FStream: TMemoryStream64;
    { Errors - we don't use exceptions here, since we need highspeed }
    FHitMarker: Boolean;
    FHitEndOfStream: Boolean;
    FMarkerTag: Byte;
  protected
    function GetStreamPos: Int64; virtual;
    procedure SetStreamPos(const Value: Int64); virtual;
  public
    { These are public for fast access. Not the most beatiful under OO design }
    { but speed requests sacrifices }
    ThisByte: PByte;
    NextByte: PByte;
    constructor Create(S: TMemoryStream64); virtual;
    function GetBits(Count: Integer): Cardinal;
    procedure RemoveBits(Count: Integer); virtual; abstract;
    procedure Reset; virtual;
    procedure Reload; virtual;
    function HasError: Boolean;
    { Are we at a marker, and there are no more bits? }
    function HitMarkerNoBitsLeft: Boolean;
    property HitEndOfStream: Boolean read FHitEndOfStream;
    { Are we at a marker? (there may still be up to 24 bits left) }
    property HitMarker: Boolean read FHitMarker;
    property MarkerTag: Byte read FMarkerTag;
    property Bits: Cardinal read FBits write FBits;
    property BitsLeft: Integer read FBitsLeft write FBitsLeft;
    property StreamPos: Int64 read GetStreamPos write SetStreamPos;
  end;

  { Optimized bit reader directly from memory }
  TMemoryBitReader = class(TBitReader)
  private
    FNextInMem, FLastInMem: PByte;
  protected
    function GetStreamPos: Int64; override;
    procedure SetStreamPos(const Value: Int64); override;
  public
    constructor Create(S: TMemoryStream64); override;
    procedure RemoveBits(Count: Integer); override;
    procedure Reset; override;
    procedure Reload; override;
  end;

  { Bit writer that saves to any stream. The writer has a buffer of approx. 1K to }
  { store the data locally until it fills up or Restart is called. In that case }
  { the buffer is flushed to the stream. }
  TBitWriter = class(TJPEG_Persistent)
  private
    FStored: Cardinal;
    FBitsStored: Integer;
    FStream: TMemoryStream64;
    FBuffer: array [0 .. 1030] of Byte;
    FBufferPos: Integer;
    procedure Emit(B: Byte); inline;
    procedure FlushBuffer;
  public
    constructor Create(S: TMemoryStream64); virtual;
    function CountBits(Value_: Integer): Integer;
    procedure PutCode(Code_: PHuffmanCode); virtual;
    procedure PutCodeExtend(Code_: PHuffmanCode; Value_: Integer; BitCount_: Integer); virtual;
    procedure PutBits(Bits: Cardinal; Count: Integer); virtual;
    procedure Restart; virtual;
  end;

  TDryRunBitWriter = class(TBitWriter)
  private
    FHistogram: P8bitHuffmanHistogram;
  public
    procedure PutCode(Code_: PHuffmanCode); override;
    procedure PutCodeExtend(Code_: PHuffmanCode; Value_: Integer; BitCount_: Integer); override;
    procedure PutBits(Bits: Cardinal; Count: Integer); override;
    procedure Restart; override;
    property Histogram: P8bitHuffmanHistogram read FHistogram write FHistogram;
  end;

implementation

function TBitReader.GetStreamPos: Int64;
begin
  Result := FStream.Position;
end;

procedure TBitReader.SetStreamPos(const Value: Int64);
begin
  FStream.Position := Value;
end;

constructor TBitReader.Create(S: TMemoryStream64);
begin
  inherited Create;
  { These two points to bits register, which is inverted in memory, }
  { so increment by 3 for first (MSB) byte, and by 2 for next byte }
  ThisByte := @FBits;
  inc(ThisByte, 3);
  NextByte := @FBits;
  inc(NextByte, 2);
  FStream := S;
end;

function TBitReader.GetBits(Count: Integer): Cardinal;
begin
  { Count is guaranteed <= 16 under normal circumstances }
  if Count > FBitsLeft then
      FHitEndOfStream := True;
  Result := FBits shr (32 - Count);
  RemoveBits(Count);
end;

procedure TBitReader.Reset;
begin
  FBits := 0;
  FBitsLeft := 0;
  FHitMarker := False;
  FHitEndOfStream := False;
  FMarkerTag := 0;
end;

procedure TBitReader.Reload;
begin
  RemoveBits(0);
end;

function TBitReader.HasError: Boolean;
begin
  Result := FHitMarker or FHitEndOfStream;
end;

function TBitReader.HitMarkerNoBitsLeft: Boolean;
begin
  Result := FHitMarker and (FBitsLeft <= 0);
end;

function TMemoryBitReader.GetStreamPos: Int64;
begin
  Result := NativeUInt(FNextInMem) - NativeUInt(TMemoryStream64(FStream).Memory);
end;

procedure TMemoryBitReader.SetStreamPos(const Value: Int64);
begin
  FNextInMem := TMemoryStream64(FStream).Memory;
  inc(FNextInMem, Value);
end;

constructor TMemoryBitReader.Create(S: TMemoryStream64);
begin
  inherited Create(S);
  { Set pointers in memory }
  FNextInMem := TMemoryStream64(S).Memory;
  FLastInMem := FNextInMem;
  inc(FNextInMem, S.Position);
  inc(FLastInMem, S.Size);
  { Fill up register, with trick to call RemoveBits }
  RemoveBits(0);
end;

procedure TMemoryBitReader.RemoveBits(Count: Integer);
begin
  FBits := FBits shl Count;
  dec(FBitsLeft, Count);
  while (FBitsLeft <= 24) do
    begin
      if FNextInMem = FLastInMem then
          break;

      if FNextInMem^ = $FF then
        begin
          if FHitMarker then
              break;

          { Skipping $FF00 and markers }

          { increment next and verify next=last }
          inc(FNextInMem);
          if FNextInMem = FLastInMem then
              break;

          if FNextInMem^ = $00 then
            begin
              { Skip $00, add $FF }
              FBits := FBits + $FF shl (24 - FBitsLeft);
              inc(FBitsLeft, 8);

              { increment next and verify next=last }
              inc(FNextInMem);
              if FNextInMem = FLastInMem then
                  break;

              continue;
            end
          else
            begin
              { We hit a marker }
              FHitMarker := True;
              FMarkerTag := FNextInMem^;
              dec(FNextInMem);
              break;
            end;
        end;
      FBits := FBits + FNextInMem^ shl (24 - FBitsLeft);
      inc(FBitsLeft, 8);
      inc(FNextInMem);
    end;
end;

procedure TMemoryBitReader.Reset;
begin
  inherited;
  { We must adjust the streams position too }
  FStream.Position := NativeUInt(FNextInMem) - NativeUInt(TMemoryStream64(FStream).Memory);
end;

procedure TMemoryBitReader.Reload;
begin
  { Set pointers in memory }
  FNextInMem := TMemoryStream64(FStream).Memory;
  inc(FNextInMem, FStream.Position);
  RemoveBits(0);
end;

procedure TBitWriter.Emit(B: Byte);
begin
  FBuffer[FBufferPos] := B;
  inc(FBufferPos);
  if B = $FF then
    begin
      FBuffer[FBufferPos] := 0;
      inc(FBufferPos);
    end;
end;

procedure TBitWriter.FlushBuffer;
begin
  if FBufferPos > 0 then
      FStream.Write(FBuffer[0], FBufferPos);
  FBufferPos := 0;
end;

constructor TBitWriter.Create(S: TMemoryStream64);
begin
  inherited Create;
  FStream := S;
end;

function TBitWriter.CountBits(Value_: Integer): Integer;
var
  v: Integer;
begin
  v := if_(Value_ < 0, -Value_, Value_);
  Result := 0;
  while v > 0 do
    begin
      inc(Result);
      v := v shr 1;
    end;
end;

procedure TBitWriter.PutCode(Code_: PHuffmanCode);
begin
{$IFDEF JPEG_Debug}
  if Code_^.L = 0 then
    begin
      DoDebugOut(Self, wsWarn, 'invalid Huffman code');
    end;
{$ENDIF JPEG_Debug}
  PutBits(Code_^.Code, Code_^.L);
end;

procedure TBitWriter.PutCodeExtend(Code_: PHuffmanCode; Value_, BitCount_: Integer);
begin
  PutCode(Code_);
  if BitCount_ = 0 then
      exit;
  if BitCount_ = 1 then
    begin
      if Value_ > 0 then
          PutBits(1, 1)
      else
          PutBits(0, 1);
      exit;
    end;
  if Value_ > 0 then
      PutBits(Value_, BitCount_)
  else
      PutBits(Value_ - cExtendOffset[BitCount_], BitCount_);
end;

procedure TBitWriter.PutBits(Bits: Cardinal; Count: Integer);
begin
  inc(FBitsStored, Count);
  FStored := FStored + Bits shl (32 - FBitsStored);
  if FBitsStored >= 16 then
    begin
      Emit(FStored shr 24);
      Emit(FStored shr 16 and $FF);
      FStored := FStored shl 16;
      FBitsStored := FBitsStored - 16;
      if FBufferPos >= 1024 then
          FlushBuffer;
    end;
end;

procedure TBitWriter.Restart;
begin
  if FBitsStored > 0 then
    begin
      while FBitsStored mod 8 <> 0 do
          PutBits(1, 1);
      if FBitsStored = 8 then
          Emit(FStored shr 24);
    end;
  FStored := 0;
  FBitsStored := 0;
  FlushBuffer;
end;

procedure TDryRunBitWriter.PutCode(Code_: PHuffmanCode);
begin
  { increment the histogram }
  inc(FHistogram^[Code_^.v]);
end;

procedure TDryRunBitWriter.PutCodeExtend(Code_: PHuffmanCode; Value_, BitCount_: Integer);
begin
  PutCode(Code_);
end;

procedure TDryRunBitWriter.PutBits(Bits: Cardinal; Count: Integer);
begin
end;

procedure TDryRunBitWriter.Restart;
begin
end;

end.
