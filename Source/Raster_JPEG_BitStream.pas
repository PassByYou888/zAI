{ ****************************************************************************** }
{ * memory Rasterization JPEG support                                          * }
{ * by QQ 600585@qq.com                                                        * }
{ ****************************************************************************** }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit Raster_JPEG_BitStream;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, PascalStrings, MemoryStream64, UnicodeMixedLib, DoStatusIO, Raster_JPEG_type;

type
  // Abstract bit reader class
  TBitReader = class(TJPEG_Persistent)
  private
    FBits: cardinal;
    FBitsLeft: integer;
    FStream: TMemoryStream64;
    // Errors - we don't use exceptions here, since we need highspeed
    FHitMarker: boolean;
    FHitEndOfStream: boolean;
    FMarkerTag: byte;
  protected
    function GetStreamPos: int64; virtual;
    procedure SetStreamPos(const Value: int64); virtual;
  public
    // These are public for fast access. Not the most beatiful under OO design
    // but speed requests sacrifices
    ThisByte: PByte;
    NextByte: PByte;
    constructor Create(S: TMemoryStream64); virtual;
    function GetBits(Count: integer): cardinal;
    procedure RemoveBits(Count: integer); virtual; abstract;
    procedure Reset; virtual;
    procedure Reload; virtual;
    function HasError: boolean;
    // Are we at a marker, and there are no more bits?
    function HitMarkerNoBitsLeft: boolean;
    property HitEndOfStream: boolean read FHitEndOfStream;
    // Are we at a marker? (there may still be up to 24 bits left)
    property HitMarker: boolean read FHitMarker;
    property MarkerTag: byte read FMarkerTag;
    property Bits: cardinal read FBits write FBits;
    property BitsLeft: integer read FBitsLeft write FBitsLeft;
    property StreamPos: int64 read GetStreamPos write SetStreamPos;
  end;

  // Optimized bit reader directly from memory
  TMemoryBitReader = class(TBitReader)
  private
    FNextInMem, FLastInMem: PByte;
  protected
    function GetStreamPos: int64; override;
    procedure SetStreamPos(const Value: int64); override;
  public
    constructor Create(S: TMemoryStream64); override;
    procedure RemoveBits(Count: integer); override;
    procedure Reset; override;
    procedure Reload; override;
  end;

  // Bit writer that saves to any stream. The writer has a buffer of approx. 1K to
  // store the data locally until it fills up or Restart is called. In that case
  // the buffer is flushed to the stream.
  TBitWriter = class(TJPEG_Persistent)
  private
    FStored: cardinal;
    FBitsStored: integer;
    FStream: TMemoryStream64;
    FBuffer: array [0 .. 1030] of byte;
    FBufferPos: integer;
    procedure Emit(B: byte);
    procedure FlushBuffer;
  public
    constructor Create(S: TMemoryStream64); virtual;
    function CountBits(AValue: integer): integer;
    procedure PutCode(ACode: PsdHuffmanCode); virtual;
    procedure PutCodeExtend(ACode: PsdHuffmanCode; AValue: integer; ABitCount: integer); virtual;
    procedure PutBits(Bits: cardinal; Count: integer); virtual;
    procedure Restart; virtual;
  end;

  TDryRunBitWriter = class(TBitWriter)
  private
    FHistogram: Psd8bitHuffmanHistogram;
  public
    procedure PutCode(ACode: PsdHuffmanCode); override;
    procedure PutCodeExtend(ACode: PsdHuffmanCode; AValue: integer; ABitCount: integer); override;
    procedure PutBits(Bits: cardinal; Count: integer); override;
    procedure Restart; override;
    property Histogram: Psd8bitHuffmanHistogram read FHistogram write FHistogram;
  end;

implementation

{ TBitReader }

constructor TBitReader.Create(S: TMemoryStream64);
begin
  inherited Create;
  // These two points to bits register, which is inverted in memory,
  // so increment by 3 for first (MSB) byte, and by 2 for next byte
  ThisByte := @FBits;
  inc(ThisByte, 3);
  NextByte := @FBits;
  inc(NextByte, 2);
  FStream := S;
end;

function TBitReader.GetBits(Count: integer): cardinal;
begin
  // Count is guaranteed <= 16 under normal circumstances
  if Count > FBitsLeft then
      FHitEndOfStream := True;
  Result := FBits shr (32 - Count);
  RemoveBits(Count);
end;

function TBitReader.GetStreamPos: int64;
begin
  Result := FStream.Position;
end;

function TBitReader.HasError: boolean;
begin
  Result := FHitMarker or FHitEndOfStream;
end;

function TBitReader.HitMarkerNoBitsLeft: boolean;
begin
  Result := FHitMarker and (FBitsLeft <= 0);
end;

procedure TBitReader.Reload;
begin
  // Fill 'r up
  RemoveBits(0);
end;

procedure TBitReader.Reset;
begin
  FBits := 0;
  FBitsLeft := 0;
  FHitMarker := False;
  FHitEndOfStream := False;
  FMarkerTag := 0;
end;

procedure TBitReader.SetStreamPos(const Value: int64);
begin
  FStream.Position := Value;
end;

{ TMemoryBitReader }

constructor TMemoryBitReader.Create(S: TMemoryStream64);
begin
  inherited Create(S);
  // Set pointers in memory
  FNextInMem := TMemoryStream64(S).Memory;
  FLastInMem := FNextInMem;
  inc(FNextInMem, S.Position);
  inc(FLastInMem, S.Size);
  // Fill up register, with trick to call RemoveBits
  RemoveBits(0);
end;

function TMemoryBitReader.GetStreamPos: int64;
begin
  Result := NativeUInt(FNextInMem) - NativeUInt(TMemoryStream64(FStream).Memory);
end;

procedure TMemoryBitReader.Reload;
begin
  // Set pointers in memory
  FNextInMem := TMemoryStream64(FStream).Memory;
  inc(FNextInMem, FStream.Position);
  RemoveBits(0);
end;

procedure TMemoryBitReader.RemoveBits(Count: integer);
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

          // Skipping $FF00 and markers

          // increment next and verify next=last
          inc(FNextInMem);
          if FNextInMem = FLastInMem then
              break;

          if FNextInMem^ = $00 then
            begin
              // Skip $00, add $FF
              FBits := FBits + $FF shl (24 - FBitsLeft);
              inc(FBitsLeft, 8);

              // increment next and verify next=last
              inc(FNextInMem);
              if FNextInMem = FLastInMem then
                  break;

              continue;
            end
          else
            begin
              // We hit a marker
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
  // We must adjust the streams position too
  FStream.Position := NativeUInt(FNextInMem) - NativeUInt(TMemoryStream64(FStream).Memory);
end;

procedure TMemoryBitReader.SetStreamPos(const Value: int64);
begin
  FNextInMem := TMemoryStream64(FStream).Memory;
  inc(FNextInMem, Value);
end;

{ TBitWriter }

function TBitWriter.CountBits(AValue: integer): integer;
begin
  if AValue < 0 then
      AValue := -AValue;
  Result := 0;
  while AValue > 0 do
    begin
      inc(Result);
      AValue := AValue shr 1;
    end;
end;

constructor TBitWriter.Create(S: TMemoryStream64);
begin
  inherited Create;
  FStream := S;
end;

procedure TBitWriter.Emit(B: byte);
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

procedure TBitWriter.PutBits(Bits: cardinal; Count: integer);
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

procedure TBitWriter.PutCode(ACode: PsdHuffmanCode);
begin
  if ACode^.L = 0 then
    begin
      DoDebugOut(Self, wsWarn, 'invalid Huffman code');
    end;
  PutBits(ACode^.Code, ACode^.L);
end;

procedure TBitWriter.PutCodeExtend(ACode: PsdHuffmanCode; AValue, ABitCount: integer);
begin
  PutCode(ACode);
  if ABitCount = 0 then
      exit;
  if ABitCount = 1 then
    begin
      if AValue > 0 then
          PutBits(1, 1)
      else
          PutBits(0, 1);
      exit;
    end;
  if AValue > 0 then
      PutBits(AValue, ABitCount)
  else
      PutBits(AValue - cExtendOffset[ABitCount], ABitCount);
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

{ TDryRunBitWriter }

procedure TDryRunBitWriter.PutBits(Bits: cardinal; Count: integer);
begin
  // this does nothing
end;

procedure TDryRunBitWriter.PutCode(ACode: PsdHuffmanCode);
begin
  // increment the histogram
  inc(FHistogram^[ACode^.V]);
end;

procedure TDryRunBitWriter.PutCodeExtend(ACode: PsdHuffmanCode; AValue,
  ABitCount: integer);
begin
  PutCode(ACode);
end;

procedure TDryRunBitWriter.Restart;
begin
  // this does nothing
end;

end.
