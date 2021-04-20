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
unit Raster_JPEG_MapIterator;

{$INCLUDE zDefine.inc}

interface

uses
  CoreClasses, Raster_JPEG_type;

type
  TIteratorMethod = (
    imReaderX,    // From x=0  , y=0   to x=W-1, y=0  , then to x=0  , y=1   and on
    imReaderXInv, // From x=W-1, y=0   to x=0  , y=0  , then to x=W-1, y=1   and on
    imReaderXBtm, // From x=0  , y=H-1 to x=W-1, y=H-1, then to X=1  , y=H-1 and on
    imReaderY,    // From x=0  , y=0   to x=0  , y=H-1, then to x=1  , y=0   and on
    imReaderYInv, // From x=0  , y=H-1 to x=0  , y=0  , then to x=1  , y=H-1 and on
    imZigZag,     // Zig zag: top line from left to right, then next from right to left, etc
    imLineByLine, // Move to next cell to the right, until end of the scanline
    imColByCol    // Move to next cell down, until last row
    );

  // device-independent pixel PFormat
  TPixelFormat = (spf1bit, spf2bit, spf4bit, spf8bit, spf10bit, spf12bit, spf15bit, spf16bit, spf24bit, spf30bit, spf32bit, spf36bit, spf48bit, spf64bit);

  // TMapIterator is a lightweight class that allows iterating along all kinds
  // of maps: often the 8, 16, 24 and 32 bitmaps are used but not necessarily, also
  // generic arrays can be used. Based on Method (TIteratorMethod), ScanStride
  // and CellStride, the class iterates along the map, starting with function
  // "First" and iterating with function "Next" until the result is nil.
  TMapIterator = class(TJPEG_Persistent)
  private
    FCount: Integer;
    FDelta: Integer;
    FLine: Integer;
    FLineFirst: PByte;
    FThis: PByte;
    FMap: PByte;
    FWidth: Integer;
    FHeight: Integer;
    FScanStride: Integer;
    FCellStride: Integer;
    FBitCount: Integer;
    FMethod: TIteratorMethod;
    function IsDirect: Boolean;
    function GetDirection: Integer;
  public
    procedure CopyFrom(Source: TMapIterator);
    // Call First to get a pointer to the first cell
    function First: PByte;
    // Call Next iteratively to get the next cell, until it returns nil
    function Next: PByte;
    // Get the pointer to the cell at X,Y. Next does *not* work from this
    // position, the First/Next paradigm cannot be mixed.
    function At(X, Y: Integer): PByte;
    // Same thing, but checks dimensions and returns nil if not within them
    function SafeAt(X, Y: Integer): PByte;
    // integer at X, Y
    function IntAt(X, Y: Integer): PInteger;
    // "single" float at X, Y
    function SingleAt(X, Y: Integer): PSingle;
    // Increment the Map pointer so it points to a different channel (e.g.
    // 0 for Blue, 1 for Red, 2 for Green in RGB 24bpp bitmap).
    procedure IncrementMap(Channel_: Integer);
    // Distance between cell 0 of scanline 0 and cell 0 of scanline 1 in bytes
    property ScanStride: Integer read FScanStride write FScanStride;
    // Distance between cell 0 and cell 1
    property CellStride: Integer read FCellStride write FCellStride;
    // Number of bits for each cell, eg with PixelFormat = pf15bits, BitCount = 15.
    // This property is merely for client apps that want to exchange their individual
    // pixel data.
    property BitCount: Integer read FBitCount write FBitCount;
    // Pointer to cell 0, 0
    property Map: PByte read FMap write FMap;
    // Width of map in pixels
    property Width: Integer read FWidth write FWidth;
    // Height of map in pixels
    property Height: Integer read FHeight write FHeight;
    // Iterator method, see TIteratorMethod for an explanation.
    property Method: TIteratorMethod read FMethod write FMethod;
    // Current line number (if multi-line iteration)
    property Line: Integer read FLine;
    // Current direction (if zig-zag)
    property Direction: Integer read GetDirection;
  end;

  // device-independent pixelformat functions
function sdPixelFormatToBitCount(const Format_: TPixelFormat): Cardinal;
function sdPixelFormatToByteCount(const Format_: TPixelFormat): Cardinal;
function sdBitCountToPixelFormat(const BitCount_: Cardinal): TPixelFormat;

implementation

function TMapIterator.IsDirect: Boolean;
begin
  Result := (FCellStride * FWidth = FScanStride);
end;

function TMapIterator.GetDirection: Integer;
begin
  if FDelta > 0 then
      Result := 1
  else
      Result := -1;
end;

procedure TMapIterator.CopyFrom(Source: TMapIterator);
var
  S: TMapIterator;
begin
  S := Source;
  FCount := S.FCount;
  FDelta := S.FDelta;
  FLine := S.FLine;
  FLineFirst := S.FLineFirst;
  FThis := S.FThis;
  FMap := S.FMap;
  FWidth := S.FWidth;
  FHeight := S.FHeight;
  FScanStride := S.FScanStride;
  FCellStride := S.FCellStride;
  FMethod := S.FMethod;
end;

function TMapIterator.First: PByte;
begin
  FThis := FMap;
  FLine := 0;

  // Iterator method
  case FMethod of
    imReaderX:
      begin
        if IsDirect then
            FCount := FWidth * FHeight
        else
            FCount := FWidth;
        FDelta := FCellStride;
      end;
    imReaderXInv:
      begin
        FCount := FWidth;
        FDelta := -FCellStride;
        inc(FThis, (FWidth - 1) * FCellStride);
      end;
    imReaderXBtm:
      begin
        FCount := FWidth;
        FDelta := FCellStride;
        inc(FThis, (FHeight - 1) * FScanStride);
      end;
    imReaderY, imColByCol:
      begin
        FCount := FHeight;
        FDelta := FScanStride;
      end;
    imReaderYInv:
      begin
        FCount := FHeight;
        FDelta := -FScanStride;
        inc(FThis, (FHeight - 1) * FScanStride);
      end;
    imLineByLine, imZigZag:
      begin
        FCount := FWidth;
        FDelta := FCellStride;
      end;
    else
      FThis := nil;
  end;

  FLineFirst := FThis;
  Result := FThis;
end;

function TMapIterator.Next: PByte;
begin
  dec(FCount);

  // increment the pointer by delta as long as there are enough iterations
  // (FCount > 0)
  if FCount > 0 then
    begin
      // increment
      inc(FThis, FDelta);
    end
  else
    begin
      // No more simple iterations are available (FCount <= 0), so now we look at
      // the iterator method
      case FMethod of
        imLineByLine, imColByCol:
          begin
            // Just one line or col, so we exit with nil
            Result := nil;
            exit;
          end;
        imReaderX, imReaderXInv:
          begin
            inc(FLine);
            if (FLine = FHeight) or IsDirect then
              begin
                Result := nil;
                exit;
              end;
            inc(FLineFirst, FScanStride);
            FThis := FLineFirst;
            FCount := FWidth;
          end;
        imReaderXBtm:
          begin
            inc(FLine);
            if FLine = FHeight then
              begin
                Result := nil;
                exit;
              end;
            dec(FLineFirst, FScanStride);
            FThis := FLineFirst;
            FCount := FWidth;
          end;
        imReaderY, imReaderYInv:
          begin
            inc(FLine);
            if FLine = FWidth then
              begin
                Result := nil;
                exit;
              end;
            inc(FLineFirst, FCellStride);
            FThis := FLineFirst;
            FCount := FHeight;
          end;
        imZigZag:
          begin
            inc(FLine);
            if FLine = FHeight then
              begin
                Result := nil;
                exit;
              end;
            inc(FLineFirst, FScanStride);
            FThis := FLineFirst;
            if odd(FLine) then
                inc(FThis, FCellStride * (FWidth - 1));
            FDelta := -FDelta;
            FCount := FWidth;
          end;
      end;
    end;
  Result := FThis;
end;

function TMapIterator.At(X, Y: Integer): PByte;
begin
  Result := FMap;
  inc(Result, X * FCellStride + Y * FScanStride);
end;

function TMapIterator.SafeAt(X, Y: Integer): PByte;
begin
  Result := nil;

  // Check dimensions
  if (X < 0) or (X >= FWidth) or (Y < 0) or (Y >= FHeight) then
      exit;

  // Dimensions ok, so call At
  Result := At(X, Y);
end;

function TMapIterator.IntAt(X, Y: Integer): PInteger;
begin
  Result := PInteger(At(X, Y));
end;

function TMapIterator.SingleAt(X, Y: Integer): PSingle;
begin
  Result := PSingle(At(X, Y));
end;

procedure TMapIterator.IncrementMap(Channel_: Integer);
begin
  inc(FMap, Channel_);
end;

function sdPixelFormatToBitCount(const Format_: TPixelFormat): Cardinal;
begin
  case Format_ of
    spf1bit: Result := 1;
    spf2bit: Result := 2;
    spf4bit: Result := 4;
    spf8bit: Result := 8;
    spf10bit: Result := 10;
    spf12bit: Result := 12;
    spf15bit: Result := 15;
    spf16bit: Result := 16;
    spf24bit: Result := 24;
    spf30bit: Result := 32;
    spf32bit: Result := 32;
    spf36bit: Result := 36;
    spf48bit: Result := 48;
    spf64bit: Result := 64;
    else Result := 0;
  end;
end;

function sdPixelFormatToByteCount(const Format_: TPixelFormat): Cardinal;
begin
  case Format_ of
    spf1bit: Result := 1;
    spf2bit: Result := 1;
    spf4bit: Result := 1;
    spf8bit: Result := 1;
    spf10bit: Result := 2;
    spf12bit: Result := 2;
    spf15bit: Result := 2;
    spf16bit: Result := 2;
    spf24bit: Result := 3;
    spf30bit: Result := 4;
    spf32bit: Result := 4;
    spf36bit: Result := 5;
    spf48bit: Result := 6;
    spf64bit: Result := 8;
    else Result := 0;
  end;
end;

function sdBitCountToPixelFormat(const BitCount_: Cardinal): TPixelFormat;
begin
  case BitCount_ of
    1: Result := spf1bit;
    2: Result := spf2bit;
    4: Result := spf4bit;
    8: Result := spf8bit;
    10: Result := spf10bit;
    12: Result := spf12bit;
    15: Result := spf15bit;
    16: Result := spf16bit;
    24: Result := spf24bit;
    32: Result := spf32bit;
    36: Result := spf36bit;
    48: Result := spf48bit;
    64: Result := spf64bit;
    else Result := spf8bit;
  end;
end;

end.
