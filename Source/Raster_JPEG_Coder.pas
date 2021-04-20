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
unit Raster_JPEG_Coder;

{$INCLUDE zDefine.inc}

interface

uses
  SysUtils,
  CoreClasses, MemoryStream64, PascalStrings,
  Raster_JPEG_type,
  Raster_JPEG_Marker,
  Raster_JPEG_MapIterator,
  Raster_JPEG_DICT,
  Raster_JPEG_BitStream,
  Raster_JPEG_ColorTransforms,
  Raster_JPEG_Huffman;

type
  // Generic coder class. This is the base class for special coders, like
  // TJpegBaselineCoder and TJpegProgressiveCoder.
  TJpegCoder = class(TJPEG_Persistent)
  protected
    FInfo: TJpegInfo;              // reference to jpeg coding info
    FMethod: TJpegDCTCodingMethod; // fast or accurate
    FHasCoefficients: Boolean;
    FHasSamples: Boolean;
    FScale: TJpegScale;
    FTileMode: Boolean;
  public
    constructor Create(Owner_: TJPEG_Base_Object; Info_: TJpegInfo); virtual;
    procedure Clear; virtual;
    procedure Initialize(Scale_: TJpegScale); virtual;
    procedure Encode(S: TMemoryStream64; Iteration: Cardinal); virtual; abstract;
    procedure Decode(S: TMemoryStream64; Iteration: Cardinal); virtual; abstract;
    procedure DecodeBlock(S: TMemoryStream64; XStart, YStart, XCount, YCount: Integer); virtual; abstract;
    procedure Finalize; virtual;
    procedure ForwardDCT; virtual; abstract;
    procedure InverseDCT; virtual; abstract;

    // Get the values from the image described with map iterator Image_, and put them in the sample maps. Use Transform_ to transform the colors.
    procedure SamplesFromImage(Image_: TMapIterator; Transform_: TColorTransform); virtual; abstract;

    // Build the image that is described with the map iterator Image_, based on the decoded samples.
    // Transform the decoded samples color space to the image color space with Transform_.
    procedure SamplesToImage(Image_: TMapIterator; Transform_: TColorTransform); virtual; abstract;

    function CreateDHTMarker: TDHTMarker; virtual;
    property Method: TJpegDCTCodingMethod read FMethod write FMethod;
    property HasCoefficients: Boolean read FHasCoefficients write FHasCoefficients;
    property HasSamples: Boolean read FHasSamples write FHasSamples;
    property Scale: TJpegScale read FScale;
    property TileMode: Boolean read FTileMode write FTileMode;
  end;

  // Common ancestor for blockbased jpeg codecs like baseline and progressive. It
  // contains a list of blockmaps, which contain DCT coefficients and raw samples
  // for each frame component in the image. We do not reuse the coefficient memory
  // for the samples, so we can still do operations on the coefficients after
  // doing the IDCT.
  TJpegBlockCoder = class(TJpegCoder)
  private
    FMaps: TBlockMapList;
    FBuffer: array of Byte;
    FBufferCellStride: Integer;
    FBufferScanStride: Integer;
  protected
    FBlockStride: Integer;
    procedure CorrectBlockStride;
    function BlockstrideForScale(Scale_: TJpegScale): Integer; virtual;
    procedure GetBlockstrideParams(BlockStride_: Integer; var BlockWidth_, McuWidth_, McuHeight_: Integer);
    procedure McuRowFromBuffer(McuY: Integer; BlockWidth_: Integer);
    procedure McuRowToBuffer(McuY: Integer; BlockWidth_: Integer);
    procedure SetupMaps(SpecialSize: Boolean; HorzMcuCount_, VertMcuCount_: Integer);
  public
    constructor Create(Owner_: TJPEG_Base_Object; Info_: TJpegInfo); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure SamplesFromImage(Image_: TMapIterator; Transform_: TColorTransform); override;
    procedure SamplesToImage(Image_: TMapIterator; Transform_: TColorTransform); override;
    procedure ForwardDCT; override;
    procedure InverseDCT; override;
    property Maps: TBlockMapList read FMaps;
    property BlockStride: Integer read FBlockStride;
  end;

  // The Jpeg Baseline coder implements the baseline huffman DC and AC decoding and encoding
  TJpegBaselineCoder = class(TJpegBlockCoder)
  protected
    FDCCoders: TEntropyCoderList;
    FACCoders: TEntropyCoderList;
    FMcu: array of TMcuBlock;
    FMcuBlockCount: Integer;
    FBitReader: TBitReader;
    FBitWriter: TBitWriter;
    FMcuIndex: Integer;
    FHorzMcuCount, FVertMcuCount: Integer;
    FRstIndex: Integer;
    FZigZag: PZigZagArray;
    FIsDryRun: Boolean;
    FTiles: TJpegTileList;
    procedure DoMcuBlockCount;
    procedure InitializeDecoderTables; virtual;
    procedure InitializeEncoderTables; virtual;
    procedure DecodeMcu(McuX_, McuY_: Integer; Skip: Boolean); virtual;
    procedure EncodeMcu(McuX_, McuY_: Integer); virtual;
    procedure ResetDecoder;
    procedure ResetEncoder;
    procedure HandleEndOfStreamError(S: TMemoryStream64); virtual;
    procedure HandleRestartInterval(S: TMemoryStream64; Warn: Boolean); virtual;
    procedure HandleHitMarkerError(S: TMemoryStream64); virtual;
    function HandleDNLMarker(McuY_: Integer; S: TMemoryStream64): Boolean; virtual;
    procedure ResizeVerticalMcu(NewVertMcuCount: Integer); virtual;
  public
    constructor Create(Owner_: TJPEG_Base_Object; Info_: TJpegInfo); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Initialize(Scale_: TJpegScale); override;
    procedure Decode(S: TMemoryStream64; Iteration: Cardinal); override;
    procedure DecodeBlock(S: TMemoryStream64; XStart, YStart, XCount, YCount: Integer); override;
    procedure Encode(S: TMemoryStream64; Iteration: Cardinal); override;
    procedure EncodeStripStart(S: TMemoryStream64);
    procedure EncodeStrip(S: TMemoryStream64);
    procedure EncodeStripClose;
    function CreateDHTMarker: TDHTMarker; override;
  end;

  TJpegProgressiveCoder = class(TJpegBaselineCoder)
  private
    FEOBRun: Integer;
    FIsDCBand: Boolean;
    FIsFirst: Boolean;
  protected
    procedure DecodeMcu(McuX_, McuY_: Integer; Skip: Boolean); override;
    procedure InitializeDecoderTables; override;
    function BlockstrideForScale(Scale_: TJpegScale): Integer; override;
    procedure HandleRestartInterval(S: TMemoryStream64; Warn: Boolean); override;
  public
    procedure Decode(S: TMemoryStream64; Iteration: Cardinal); override;
    procedure Finalize; override;
  end;

  // Same as baseline coder
  TJpegExtendedCoder = class(TJpegBaselineCoder)
  end;

implementation

procedure TJpegCoder.Clear;
begin
  FHasCoefficients := False;
  FHasSamples := False;
  FScale := jsFull;
end;

constructor TJpegCoder.Create(Owner_: TJPEG_Base_Object; Info_: TJpegInfo);
begin
  inherited Create;
  FOwner := Owner_;
  FInfo := Info_;
end;

function TJpegCoder.CreateDHTMarker: TDHTMarker;
begin
  Result := nil;
end;

procedure TJpegCoder.Finalize;
begin
  // default does nothing
end;

procedure TJpegCoder.Initialize(Scale_: TJpegScale);
begin
  FScale := Scale_;
end;

{ TJpegBlockCoder }

function TJpegBlockCoder.BlockstrideForScale(Scale_: TJpegScale): Integer;
begin
  case Scale_ of
    jsFull: Result := 64;
    jsDiv2: Result := 16;
    jsDiv4: Result := 4;
    jsDiv8: Result := 1;
    else
      Result := 0;
  end;
end;

procedure TJpegBlockCoder.Clear;
var
  i: Integer;
begin
  inherited;
  // We only clear the backup coefficents, not the data itself, so that any new
  // SetSize may reuse already allocated memory
  for i := 0 to Maps.Count - 1 do
      Maps[i].ClearCoefBackup;
end;

procedure TJpegBlockCoder.CorrectBlockStride;
var
  i, NewSize: Integer;
begin
  if (FBlockStride = 64) and (FScale <> jsFull) then
    begin
      // We must reduce the map blockstrides first
      NewSize := 0;
      case FScale of
        jsDiv2: NewSize := 4;
        jsDiv4: NewSize := 2;
        jsDiv8: NewSize := 1;
      end;
      for i := 0 to FMaps.Count - 1 do
          FMaps[i].ReduceBlockSize(NewSize);
      FBlockStride := NewSize * NewSize;
    end;
end;

constructor TJpegBlockCoder.Create(Owner_: TJPEG_Base_Object; Info_: TJpegInfo);
begin
  inherited;
  FMaps := TBlockMapList.Create;
end;

destructor TJpegBlockCoder.Destroy;
begin
  DisposeObject(FMaps);
  FMaps := nil;
  inherited;
end;

procedure TJpegBlockCoder.ForwardDCT;
var
  i: Integer;
  FDCT: TJpegFDCT;
begin
  FDCT := TJpegFDCT.Create;
  try
    for i := 0 to FInfo.FFrameCount - 1 do
      begin
        FDCT.Map := FMaps[i];
        FDCT.PerformFDCT(FInfo.FQuantizationTables[FInfo.FFrames[i].FQTable]);
      end;
  finally
      FDCT.Free;
  end;
end;

procedure TJpegBlockCoder.GetBlockstrideParams(BlockStride_: Integer; var BlockWidth_, McuWidth_, McuHeight_: Integer);
begin
  case BlockStride_ of
    64:
      begin
        BlockWidth_ := 8;
        McuWidth_ := FInfo.FMcuWidth;
        McuHeight_ := FInfo.FMcuHeight;
      end;
    16:
      begin
        BlockWidth_ := 4;
        McuWidth_ := FInfo.FMcuWidth div 2;
        McuHeight_ := FInfo.FMcuHeight div 2;
      end;
    4:
      begin
        BlockWidth_ := 2;
        McuWidth_ := FInfo.FMcuWidth div 4;
        McuHeight_ := FInfo.FMcuHeight div 4;
      end;
    1:
      begin
        BlockWidth_ := 1;
        McuWidth_ := FInfo.FMcuWidth div 8;
        McuHeight_ := FInfo.FMcuHeight div 8;
      end;
    else
      BlockWidth_ := 0; // avoid warnings
      McuWidth_ := 0;
      McuHeight_ := 0;
  end;
end;

procedure TJpegBlockCoder.InverseDCT;
var
  i: Integer;
  IDCT: TJpegIDCT;
begin
  IDCT := TJpegIDCT.Create;
  try
    IDCT.Method := FMethod;
    for i := 0 to FInfo.FFrameCount - 1 do
      begin
        IDCT.Map := FMaps[i];
        IDCT.BuildQuantTableFrom(FInfo.FQuantizationTables[FInfo.FFrames[i].FQTable]);
        IDCT.PerformIDCT;
      end;
  finally
      IDCT.Free;
  end;
end;

procedure TJpegBlockCoder.McuRowFromBuffer(McuY, BlockWidth_: Integer);
var
  i, j, row, col, xblock, yblock, yi, m, V: Integer;
  XRepeat, YRepeat, XYArea: Integer;
  PixBlockStride: Integer;
  Map: TJpegBlockMap;
  Frame: TFrameComponent;
  PFirst, PScan, PBlock, PPixel, PCopy, PValue: Pbyte;
begin
  PFirst := @FBuffer[0];
  // Loop through all maps
  for m := 0 to FInfo.FFrameCount - 1 do
    begin
      // Process Map
      Map := FMaps[m];
      Frame := FInfo.FFrames[m];
      PScan := PFirst;
      XRepeat := FInfo.FHorzSamplingMax div Frame.FHorzSampling;
      YRepeat := FInfo.FVertSamplingMax div Frame.FVertSampling;
      XYArea := XRepeat * YRepeat;
      PixBlockStride := BlockWidth_ * XRepeat * FBufferCellStride;
      // We process VertSampling rows
      for yi := 0 to Frame.FVertSampling - 1 do
        begin
          // y is the block row-index into the map
          yblock := McuY * Frame.FVertSampling + yi;
          // Reset the block pointer to the start of the scanline
          PBlock := PScan;
          // We process a row of DCT blocks
          for xblock := 0 to Map.HorzBlockCount - 1 do
            begin
              // Pointer to the samples in this block
              PValue := Map.GetSamplePointer(xblock, yblock);
              // Reset the pixel pointer to the start of the block
              PPixel := PBlock;
              // Rows of block
              for row := 0 to BlockWidth_ - 1 do
                begin
                  // Check for optimized version
                  if (XRepeat = 1) and (YRepeat = 1) then
                    begin
                      // Optimized version for no repeats
                      // Columns of block
                      for col := 0 to BlockWidth_ - 1 do
                        begin
                          // Copy pixel to value
                          PValue^ := PPixel^;
                          inc(PPixel, FBufferCellStride);
                          inc(PValue);
                        end;
                    end
                  else
                    begin
                      // Repeats in at least one direction
                      for col := 0 to BlockWidth_ - 1 do
                        begin
                          // Copy pixel(s) to value and average
                          V := 0;
                          for i := 0 to XRepeat - 1 do
                            begin
                              inc(V, PPixel^);
                              // vertical repeats?
                              PCopy := PPixel;
                              for j := 1 to YRepeat - 1 do
                                begin
                                  inc(PCopy, FBufferScanStride);
                                  inc(V, PCopy^);
                                end;
                              inc(PPixel, FBufferCellStride);
                            end;
                          PValue^ := V div XYArea;
                          inc(PValue);
                        end;
                    end;
                  // Go to the next row in the block. Since we ran through the row, we
                  // must also undo the blockstride
                  inc(PPixel, FBufferScanStride * YRepeat - PixBlockStride);
                end;
              //
              inc(PBlock, PixBlockStride);
            end;
          inc(PScan, FBufferScanStride * BlockWidth_ * YRepeat);
        end;
      inc(PFirst);
    end;
end;

procedure TJpegBlockCoder.McuRowToBuffer(McuY: Integer; BlockWidth_: Integer);
var
  i, j, row, col, xblock, yblock, yi, m: Integer;
  XRepeat, YRepeat: Integer;
  PixBlockStride: Integer;
  Map: TJpegBlockMap;
  Frame: TFrameComponent;
  PFirst, PScan, PBlock, PPixel, PCopy, PValue: Pbyte;
begin
  PFirst := @FBuffer[0];
  // Loop through all maps
  for m := 0 to FInfo.FFrameCount - 1 do
    begin
      // Process Map
      Map := FMaps[m];
      Frame := FInfo.FFrames[m];
      PScan := PFirst;
      XRepeat := FInfo.FHorzSamplingMax div Frame.FHorzSampling;
      YRepeat := FInfo.FVertSamplingMax div Frame.FVertSampling;
      PixBlockStride := BlockWidth_ * XRepeat * FBufferCellStride;
      // We process VertSampling rows
      for yi := 0 to Frame.FVertSampling - 1 do
        begin
          // y is the block row-index into the map
          yblock := McuY * Frame.FVertSampling + yi;
          // Reset the block pointer to the start of the scanline
          PBlock := PScan;
          // We process a row of DCT blocks
          for xblock := 0 to Map.HorzBlockCount - 1 do
            begin
              // Pointer to the samples in this block
              PValue := Map.GetSamplePointer(xblock, yblock);
              // Reset the pixel pointer to the start of the block
              PPixel := PBlock;
              // Rows of block
              for row := 0 to BlockWidth_ - 1 do
                begin
                  // Check for optimized version
                  if (XRepeat = 1) and (YRepeat = 1) then
                    begin
                      // Optimized version for no repeats
                      // Columns of block
                      for col := 0 to BlockWidth_ - 1 do
                        begin
                          // Copy value to pixel
                          PPixel^ := PValue^;
                          inc(PPixel, FBufferCellStride);
                          inc(PValue);
                        end;
                    end
                  else
                    begin
                      // Repeats in at least one direction
                      for col := 0 to BlockWidth_ - 1 do
                        begin
                          // Copy value to pixel(s)
                          for i := 0 to XRepeat - 1 do
                            begin
                              PPixel^ := PValue^;
                              // vertical repeats?
                              PCopy := PPixel;
                              for j := 1 to YRepeat - 1 do
                                begin
                                  inc(PCopy, FBufferScanStride);
                                  PCopy^ := PValue^;
                                end;
                              inc(PPixel, FBufferCellStride);
                            end;
                          inc(PValue);
                        end;
                    end;
                  // Go to the next row in the block. Since we ran through the row, we
                  // must also undo the blockstride
                  inc(PPixel, FBufferScanStride * YRepeat - PixBlockStride);
                end;
              //
              inc(PBlock, PixBlockStride);
            end;
          inc(PScan, FBufferScanStride * BlockWidth_ * YRepeat);
        end;
      inc(PFirst);
    end;
end;

procedure TJpegBlockCoder.SamplesFromImage(Image_: TMapIterator; Transform_: TColorTransform);
var
  x, y, yi, BufPos, HorzCount: Integer;
  BlockWidth, McuWidth, McuHeight: Integer;
  PCell, PBuff, PImage: Pbyte;
begin
  GetBlockstrideParams(FBlockStride, BlockWidth, McuWidth, McuHeight);

  // Create a buffer of McuHeight scanlines
  HorzCount := FInfo.FHorzMcuCount * McuWidth;

  FBufferCellStride := FInfo.FFrameCount;
  FBufferScanStride := HorzCount * FBufferCellStride;

  Image_.Method := imColByCol;
  PImage := Image_.First;

  // checks
  if not assigned(PImage) then
    begin
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsFail, 'image is not assigned!');
{$ENDIF JPEG_Debug}
      exit;
    end;

  if (Image_.CellStride < FBufferCellStride) then
    begin
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsFail,
        PFormat('image cellstride insufficient (image cellstride = %d, buffer cellstride = %d)',
        [Image_.CellStride, FBufferCellStride]));
{$ENDIF JPEG_Debug}
      exit;
    end;

  // create a buffer
  SetLength(FBuffer, FBufferScanStride * McuHeight);

  y := 0;
  while assigned(PImage) and (y < FInfo.FVertMcuCount) do
    begin
      // Color convert and put data in buffer
      BufPos := 0;

      yi := 0;
      while assigned(PImage) and (yi < McuHeight) do
        begin
          if Image_.CellStride = FBufferCellStride then
            begin
              // Transform one row of colors with default image cellstride
              Transform_.Transform(PImage, @FBuffer[BufPos], Image_.Width);
            end
          else
            begin
              // transform just one pixel
              x := 0;
              PCell := PImage;
              PBuff := @FBuffer[BufPos];
              while x < Image_.Width do
                begin
                  // 1 pixel transformation
                  Transform_.Transform(PCell, PBuff, 1);
                  // increment pointers
                  inc(PBuff, FBufferCellStride);
                  inc(PCell, Image_.CellStride);
                  inc(x);
                end;
            end;
          // next scanline
          PImage := Image_.Next;
          inc(BufPos, FBufferScanStride);
          // increment yi
          inc(yi);
        end;

      // Combine buffer into jpeg sample maps
      McuRowFromBuffer(y, BlockWidth);
      // increment y
      inc(y);
    end;
end;

procedure TJpegBlockCoder.SamplesToImage(Image_: TMapIterator; Transform_: TColorTransform);
var
  x, y, yi, BufPos, HorzCount: Integer;
  BlockWidth, McuWidth, McuHeight: Integer;
  PCell, PBuff, PImage: Pbyte;
begin
  GetBlockstrideParams(FBlockStride, BlockWidth, McuWidth, McuHeight);

  // Create a buffer of McuHeight scanlines
  if FTileMode then
      HorzCount := FInfo.FTileWidth
  else
      HorzCount := FInfo.FHorzMcuCount * McuWidth;

  FBufferCellStride := FInfo.FFrameCount;
  FBufferScanStride := HorzCount * FBufferCellStride;

  // We only do the first col 0, thus this iterator loops through all the rows,
  // col 0.
  Image_.Method := imColByCol;
  PImage := Image_.First;

  // checks
  if not assigned(PImage) then
    begin
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsFail, 'image is not assigned!');
{$ENDIF JPEG_Debug}
      exit;
    end;

  // eg Adobe YCCK -> RGB uses 4 ch orig to 3 ch internal, so this strict check
  // needs to be relaxed: Transform_.DstCellStride instead of FBufferCellStride
  if (Image_.CellStride < Transform_.DstCellStride) then
    begin
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsFail, PFormat('image cellstride insufficient (image cellstride = %d, transform dest cellstride = %d)', [Image_.CellStride, FBufferCellStride]));
{$ENDIF JPEG_Debug}
      exit;
    end;

  // create a buffer
  SetLength(FBuffer, FBufferScanStride * McuHeight);

  y := 0;
  while assigned(PImage) and (y < FInfo.FVertMcuCount) do
    begin
      // Combine jpeg sample maps into buffer of width x mcu height
      McuRowToBuffer(y, BlockWidth);

      // Color convert and put data in image
      BufPos := 0;
      yi := 0;
      while assigned(PImage) and (yi < McuHeight) do
        begin
          if Image_.CellStride = FBufferCellStride then
            begin
              // Transform one row of colors with default image cellstride
              Transform_.Transform(@FBuffer[BufPos], PImage, Image_.Width);
            end
          else
            begin
              // transform just one pixel
              x := 0;
              PCell := PImage;
              PBuff := @FBuffer[BufPos];
              while x < Image_.Width do
                begin
                  // 1 pixel transformation
                  Transform_.Transform(PBuff, PCell, 1);
                  // increment pointers
                  inc(PBuff, FBufferCellStride);
                  inc(PCell, Image_.CellStride);
                  inc(x);
                end;
            end;
          // next scanline
          PImage := Image_.Next;
          inc(BufPos, FBufferScanStride);
          // increment yi
          inc(yi);
        end;
      // increment y
      inc(y)
    end;
end;

procedure TJpegBlockCoder.SetupMaps(SpecialSize: Boolean; HorzMcuCount_, VertMcuCount_: Integer);
var
  i, HorzSampling, VertSampling: Integer;
begin
  // Calculate Hmax, Vmax
  FInfo.FHorzSamplingMax := 0;
  FInfo.FVertSamplingMax := 0;
  for i := 0 to FInfo.FFrameCount - 1 do
    begin
      HorzSampling := FInfo.FFrames[i].FHorzSampling;
      if HorzSampling >= FInfo.FHorzSamplingMax then
          FInfo.FHorzSamplingMax := HorzSampling;
      VertSampling := FInfo.FFrames[i].FVertSampling;
      if VertSampling >= FInfo.FVertSamplingMax then
          FInfo.FVertSamplingMax := VertSampling;
    end;

  // MCU size in pixels
  FInfo.FMcuWidth := FInfo.FHorzSamplingMax * 8;
  FInfo.FMcuHeight := FInfo.FVertSamplingMax * 8;

  // MCU count
  FInfo.FHorzMcuCount := (FInfo.FWidth + FInfo.FMcuWidth - 1) div FInfo.FMcuWidth;
  FInfo.FVertMcuCount := (FInfo.FHeight + FInfo.FMcuHeight - 1) div FInfo.FMcuHeight;

  // create maps with given counts
  if SpecialSize then
    begin
      for i := 0 to FInfo.FFrameCount - 1 do
          FMaps[i].SetSize(HorzMcuCount_, VertMcuCount_, FInfo.FFrames[i], FBlockStride);
    end
  else
    begin
      for i := 0 to FInfo.FFrameCount - 1 do
          FMaps[i].SetSize(FInfo.FHorzMcuCount, FInfo.FVertMcuCount, FInfo.FFrames[i], FBlockStride);
    end;

end;

procedure TJpegBaselineCoder.Clear;
begin
  inherited;
  FDCCoders.Clear;
  FACCoders.Clear;
  FTiles.Clear;
end;

constructor TJpegBaselineCoder.Create(Owner_: TJPEG_Base_Object; Info_: TJpegInfo);
begin
  inherited;
  FDCCoders := TEntropyCoderList.Create;
  FACCoders := TEntropyCoderList.Create;
  FTiles := TJpegTileList.Create;
end;

function TJpegBaselineCoder.CreateDHTMarker: TDHTMarker;
var
  i: Integer;
  C: T8bitHuffmanEncoder;
  Item: PDHTMarkerInfo;
  ItemCount: Integer;
begin
  Result := TDHTMarker.Create(FInfo, mkDHT);
  ItemCount := 0;

  // Loop through the DC tables
  for i := 0 to FDCCoders.Count - 1 do
    begin
      C := FDCCoders[i] as T8bitHuffmanEncoder;
      if C is T8bitHuffmanEncoder then
        begin
          SetLength(Result.FMarkerInfo, ItemCount + 1);
          Item := @Result.FMarkerInfo[ItemCount];
          Item^.Tc := 0;
          Item^.Th := i;
          inc(ItemCount);
          C.OptimiseHuffmanFromHistogram(Item^);
        end;
    end;

  // Loop through the AC tables
  for i := 0 to FACCoders.Count - 1 do
    begin
      C := FACCoders[i] as T8bitHuffmanEncoder;
      if C is T8bitHuffmanEncoder then
        begin
          SetLength(TDHTMarker(Result).FMarkerInfo, ItemCount + 1);
          Item := @TDHTMarker(Result).FMarkerInfo[ItemCount];
          Item^.Tc := 1;
          Item^.Th := i;
          inc(ItemCount);
          C.OptimiseHuffmanFromHistogram(Item^);
        end;
    end;
  if ItemCount = 0 then
    begin
      DisposeObject(Result);
      Result := nil;
    end;
end;

procedure TJpegBaselineCoder.Decode(S: TMemoryStream64; Iteration: Cardinal);
var
  Tile: TJpegTile;
  i: Integer;
  McuX, McuY: Integer;
  CountTotal: int64;
  CountCodes, CountBits: int64;
begin
  if Iteration = 0 then
    begin
      // reset position
      S.Position := 0;
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsInfo, PFormat('decoding starts (position:%d, iter:%d)', [S.Position, Iteration]));
{$ENDIF JPEG_Debug}
    end
  else
    begin
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsInfo, PFormat('decoding continues (position:%d, iter:%d)', [S.Position, Iteration]));
{$ENDIF JPEG_Debug}
    end;

  // Count number of blocks in MCU and number of MCU cycles
  DoMcuBlockCount;

  // Initialize the decoder tables for DC and AC in this scan
  InitializeDecoderTables;

  // Initialize bit reader
  FBitReader := TMemoryBitReader.Create(S);

  try

    FTiles.Clear;
    FMcuIndex := 0;
    FRstIndex := 0;
    McuX := 0;
    McuY := 0;
    repeat
      if (McuX = 0) and FInfo.FWaitForDNL then
        begin
          // Check if we have enough size vertically, in case of waiting for DNL marker
          if McuY >= FVertMcuCount then
            // Resize the maps, 16 MCU lines at a time. This 16 is an arbitrary number
              ResizeVerticalMcu(McuY + 16);
        end;

      // Tiled loading? Then we create the tile info for each 8 McuX blocks
      if FTileMode and (McuX mod 8 = 0) then
        begin
          Tile := TJpegTile.Create;
          Tile.FMcuIndex := FMcuIndex;
          Tile.FStreamPos := FBitReader.StreamPos;
          Tile.FBits := FBitReader.Bits;
          Tile.FBitsLeft := FBitReader.BitsLeft;
          SetLength(Tile.FPredictors, FInfo.FScans.Count);
          for i := 0 to FInfo.FScans.Count - 1 do
              Tile.FPredictors[i] := FInfo.FScans[i].FPredictor;
          FTiles.Add(Tile);
        end;

      // Decode one MCU, skip if tiled loading is in effect
      DecodeMcu(McuX, McuY, FTileMode);
      inc(FMcuIndex);
      inc(McuX);
      if McuX = FHorzMcuCount then
        begin
          McuX := 0;
          inc(McuY);
          if FInfo.FWaitForDNL then
            if HandleDNLMarker(McuY, S) then
                Break;
        end;

      // Check for errors
      if FBitReader.HitEndOfStream then
        begin
          HandleEndOfStreamError(S);
        end;

      // Check for restart interval
      if (FInfo.FRestartInterval > 0) and (FMcuIndex mod FInfo.FRestartInterval = 0) then
        begin
          HandleRestartInterval(S, True);
        end;

      // Check for markers
      if FBitReader.HitMarkerNoBitsLeft then
        begin
          HandleHitMarkerError(S);
          McuX := FMcuIndex mod FHorzMcuCount;
          McuY := FMcuIndex div FHorzMcuCount;
        end;

    until not FInfo.FWaitForDNL and (McuY = FVertMcuCount);

    // For good measure we add one more tile if in tilemode (without any data though)
    if FTileMode then
      begin
        Tile := TJpegTile.Create;
        Tile.FMcuIndex := FMcuIndex;
        FTiles.Add(Tile);
      end;

    ResetDecoder;

    CountCodes := 0;
    CountBits := 0;
    for i := 0 to FDCCoders.Count - 1 do
      begin
        if FDCCoders[i] is TDCBaselineHuffmanDecoder then
          begin
            inc(CountCodes, TDCBaselineHuffmanDecoder(FDCCoders[i]).FCountCodes);
            inc(CountBits, TDCBaselineHuffmanDecoder(FDCCoders[i]).FCountBits);
          end;
      end;
    for i := 0 to FACCoders.Count - 1 do
      begin
        if FACCoders[i] is TACBaselineHuffmanDecoder then
          begin
            inc(CountCodes, TACBaselineHuffmanDecoder(FACCoders[i]).FCountCodes);
            inc(CountBits, TACBaselineHuffmanDecoder(FACCoders[i]).FCountBits);
          end;
      end;

    // Report
    CountTotal := CountCodes + CountBits;

    // if CountTotal = 0, avoid div by zero
    if CountTotal = 0 then
        CountTotal := 1;

{$IFDEF JPEG_Debug}
    DoDebugOut(Self, wsInfo, PFormat('Codes bitcout = %d (%3.1f%%)', [CountCodes, CountCodes * 100 / CountTotal]));
    DoDebugOut(Self, wsInfo, PFormat('Bits  bitcout = %d (%3.1f%%)', [CountBits, CountBits * 100 / CountTotal]));
{$ENDIF JPEG_Debug}
  finally
    DisposeObject(FBitReader);
    FBitReader := nil;
    FHasCoefficients := True;
    FHasSamples := False;
  end;
end;

procedure TJpegBaselineCoder.DecodeBlock(S: TMemoryStream64; XStart, YStart, XCount, YCount: Integer);
var
  x, y, i, Idx, McuIdx: Integer;
  Tile: TJpegTile;
begin
  // Setup maps with this special count
  SetupMaps(True, XCount, YCount);

  // Initialize bit reader
  FBitReader := TMemoryBitReader.Create(S);

  try

    for y := 0 to YCount - 1 do
      begin
        if y + YStart >= FVertMcuCount then
            Break;
        FMcuIndex := (y + YStart) * FHorzMcuCount + XStart;

        // Find tile that has equal or smaller mcuindex
        Idx := FTiles.IndexByMcuIndex(FMcuIndex); // index in tilelist
        if Idx = FTiles.Count then
          begin
{$IFDEF JPEG_Debug}
            DoDebugOut(Self, wsFail, sRangeErrorInTileLoading);
{$ENDIF JPEG_Debug}
            exit;
          end;
        if FTiles[Idx].FMcuIndex > FMcuIndex then
            dec(Idx);

        // Position bitreader and reset predictors
        Tile := FTiles[Idx];
        FBitReader.StreamPos := Tile.FStreamPos;
        FBitReader.Bits := Tile.FBits;
        FBitReader.BitsLeft := Tile.FBitsLeft;
        for i := 0 to length(Tile.FPredictors) - 1 do
            FInfo.FScans[i].FPredictor := Tile.FPredictors[i];

        // Skip preceding mcu's
        McuIdx := Tile.FMcuIndex;
        while McuIdx < FMcuIndex do
          begin
            DecodeMcu(0, 0, True);
            inc(McuIdx);
            if (FInfo.FRestartInterval > 0) and (McuIdx mod FInfo.FRestartInterval = 0) then
                HandleRestartInterval(S, False);
          end;

        for x := 0 to XCount - 1 do
          begin
            if x + XStart >= FHorzMcuCount then
                Break;
            // Now don't skip
            DecodeMcu(x, y, False);
            inc(FMcuIndex);
            // Check for restart interval
            if (FInfo.FRestartInterval > 0) and (FMcuIndex mod FInfo.FRestartInterval = 0) then
                HandleRestartInterval(S, False);
          end;
      end;

  finally
    DisposeObject(FBitReader);
    FBitReader := nil;
    FHasCoefficients := True;
    FHasSamples := False;
  end;
end;

procedure TJpegBaselineCoder.DecodeMcu(McuX_, McuY_: Integer; Skip: Boolean);
var
  i: Integer;
  McuBlock: PMCUBlock;
  Dummy: TCoefBlock;
begin
  for i := 0 to FMcuBlockCount - 1 do
    begin
      // The current MCU block
      McuBlock := @FMcu[i];

      // Initialize MCU values pointer
      if Skip then
          McuBlock^.Values := @Dummy[0]
      else
          McuBlock^.Values := Maps[McuBlock^.MapIdx].GetCoefPointerMCU(McuX_, McuY_, McuBlock^.BlockIdx);

      // Each MCU block has an index to a DC and AC table, use it to do the decoding
      TDCBaselineHuffmanDecoder(FDCCoders[McuBlock^.DCTable]).DecodeMcuBlock(McuBlock^, FBitReader);
      if (FScale = jsDiv8) or Skip then
          TACBaselineHuffmanDecoder(FACCoders[McuBlock^.ACTable]).DecodeMcuBlockSkip(FBitReader)
      else
          TACBaselineHuffmanDecoder(FACCoders[McuBlock^.ACTable]).DecodeMcuBlock(McuBlock^, FBitReader, FZigZag);
      if FBitReader.HitEndOfStream then
          exit;
    end;
end;

destructor TJpegBaselineCoder.Destroy;
begin
  DisposeObject(FDCCoders);
  FDCCoders := nil;
  DisposeObject(FACCoders);
  FACCoders := nil;
  DisposeObject(FTiles);
  FTiles := nil;
  inherited;
end;

procedure TJpegBaselineCoder.DoMcuBlockCount;
var
  HSize, VSize: Integer;
  i: Integer;
  Frame: TFrameComponent;
begin
  if FInfo.FScanCount = 1 then
    begin
      // Single channel: spec tells there can only be one MCU block
      FMcuBlockCount := 1;
      // calculate # blocks in horz and vert direction
      Frame := FInfo.FFrames[FInfo.FScans[0].FComponent];
      HSize := 8 * FInfo.FHorzSamplingMax div Frame.FHorzSampling;
      VSize := 8 * FInfo.FVertSamplingMax div Frame.FVertSampling;
      FHorzMcuCount := (FInfo.FWidth + HSize - 1) div HSize;
      FVertMcuCount := (FInfo.FHeight + VSize - 1) div VSize;
    end
  else
    begin
      // Multi channel
      FHorzMcuCount := FInfo.FHorzMcuCount;
      FVertMcuCount := FInfo.FVertMcuCount;
      FMcuBlockCount := 0;
      for i := 0 to FInfo.FScanCount - 1 do
          inc(FMcuBlockCount, Maps[FInfo.FScans[i].FComponent].McuBlockCount(FInfo.FScanCount));
    end;
  SetLength(FMcu, FMcuBlockCount);
end;

procedure TJpegBaselineCoder.Encode(S: TMemoryStream64; Iteration: Cardinal);
var
  B: Byte;
  McuX, McuY: Integer;
begin
  FIsDryRun := (S = nil);

  // Count number of blocks in MCU and number of MCU cycles
  DoMcuBlockCount;

  // Initialize the encoder tables for DC and AC in this scan
  InitializeEncoderTables;

  // Initialize bit writer
  if FIsDryRun then
      FBitWriter := TDryRunBitWriter.Create(S)
  else
      FBitWriter := TBitWriter.Create(S);

  try
    FMcuIndex := 0;
    FRstIndex := 0;
    McuX := 0;
    McuY := 0;

    repeat
      // Encode one MCU
      EncodeMcu(McuX, McuY);
      inc(FMcuIndex);
      inc(McuX);
      if McuX = FHorzMcuCount then
        begin
          McuX := 0;
          inc(McuY);
        end;
      if McuY = FVertMcuCount then
          Break;
      // Check for restart interval
      if (FInfo.FRestartInterval > 0) and (FMcuIndex mod FInfo.FRestartInterval = 0) then
        begin
          // Restart interval
          ResetEncoder;
          if not FIsDryRun then
            begin
              // write RST
              B := $FF;
              S.Write(B, 1);
              B := (FRstIndex mod 8) + mkRST0;
              S.Write(B, 1);
            end;
          inc(FRstIndex);
        end;
    until (McuY = FVertMcuCount);

    ResetEncoder;
  finally
    DisposeObject(FBitWriter);
    FBitWriter := nil;
  end;
end;

procedure TJpegBaselineCoder.EncodeMcu(McuX_, McuY_: Integer);
var
  i: Integer;
  McuBlock: PMCUBlock;
  DC: TDCBaselineHuffmanEncoder;
  AC: TACBaselineHuffmanEncoder;
begin
  for i := 0 to FMcuBlockCount - 1 do
    begin
      // The current MCU block
      McuBlock := @FMcu[i];
      // Initialize MCU values pointer
      McuBlock^.Values := Maps[McuBlock^.MapIdx].GetCoefPointerMCU(McuX_, McuY_, McuBlock^.BlockIdx);
      // Each MCU block has an index to a DC and AC table, use it to do the encoding
      DC := TDCBaselineHuffmanEncoder(FDCCoders[McuBlock^.DCTable]);
      AC := TACBaselineHuffmanEncoder(FACCoders[McuBlock^.ACTable]);
      if FIsDryRun then
          TDryRunBitWriter(FBitWriter).Histogram := DC.Histogram;
      DC.EncodeMcuBlock(McuBlock^, FBitWriter);
      if FIsDryRun then
          TDryRunBitWriter(FBitWriter).Histogram := AC.Histogram;
      AC.EncodeMcuBlock(McuBlock^, FBitWriter);
    end;
end;

procedure TJpegBaselineCoder.EncodeStrip(S: TMemoryStream64);
var
  McuX: Integer;
  B: Byte;
begin
  McuX := 0;
  repeat
    // Encode one MCU
    EncodeMcu(McuX, 0);
    inc(FMcuIndex);
    inc(McuX);
    if McuX = FHorzMcuCount then
        Break;

    // Check for restart interval
    if (FInfo.FRestartInterval > 0) and (FMcuIndex mod FInfo.FRestartInterval = 0) then
      begin
        // Restart interval
        ResetEncoder;
        // write RST
        B := $FF;
        S.Write(B, 1);
        B := (FRstIndex mod 8) + mkRST0;
        S.Write(B, 1);
        inc(FRstIndex);
      end;
  until False;
end;

procedure TJpegBaselineCoder.EncodeStripClose;
begin
  ResetEncoder;
  DisposeObject(FBitWriter);
  FBitWriter := nil;
end;

procedure TJpegBaselineCoder.EncodeStripStart(S: TMemoryStream64);
begin
  // Setup maps to the size of one strip
  SetupMaps(True, FInfo.FHorzMcuCount, 1);

  // Count number of blocks in MCU and number of MCU cycles
  DoMcuBlockCount;

  // Initialize the encoder tables for DC and AC in this scan
  InitializeEncoderTables;

  // Initialize bit writer
  FBitWriter := TBitWriter.Create(S);

  FMcuIndex := 0;
  FRstIndex := 0;
end;

function TJpegBaselineCoder.HandleDNLMarker(McuY_: Integer; S: TMemoryStream64): Boolean;
var
  ReadBytes: Integer;
  B, Tag: Byte;
begin
  Result := False;
  if FBitReader.HitMarker then
    begin
      // It should be a DNL marker
      ResetDecoder;
      ReadBytes := S.Read(B, 1);
      if not(ReadBytes = 1) or (B <> $FF) then
        begin
{$IFDEF JPEG_Debug}
          DoDebugOut(Self, wsFail, sDNLMarkerExpected);
{$ENDIF JPEG_Debug}
          exit;
        end;
      S.Read(Tag, 1);
      if Tag <> mkDNL then
        begin
{$IFDEF JPEG_Debug}
          DoDebugOut(Self, wsWarn, sDNLMarkerExpected);
{$ENDIF JPEG_Debug}
          exit;
        end;
      FInfo.FWaitForDNL := False;
      ResizeVerticalMcu(McuY_);
      Result := True;
    end;
end;

procedure TJpegBaselineCoder.HandleEndOfStreamError(S: TMemoryStream64);
begin
  // Serious error: there weren't enough bits in the stream
  if FBitReader.HitMarkerNoBitsLeft then
    begin
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsFail, PFormat('Error: Hit Marker $%s', [IntToHex(FBitReader.MarkerTag, 2)]));
{$ENDIF JPEG_Debug}
    end;
  ResetDecoder;
{$IFDEF JPEG_Debug}
  DoDebugOut(Self, wsFail, PFormat('Error: Premature stream end at position %d', [S.Position]));
{$ENDIF JPEG_Debug}
end;

procedure TJpegBaselineCoder.HandleHitMarkerError(S: TMemoryStream64);
begin
  case FBitReader.MarkerTag of
    mkRST0 .. mkRST7:
      begin
        // We found a restart too early, set McuIndex to the correct value
{$IFDEF JPEG_Debug}
        DoDebugOut(Self, wsWarn, PFormat('Restart interval %d (too early)', [FRstIndex]));
{$ENDIF JPEG_Debug}
        inc(FRstIndex);
        FMcuIndex := FRstIndex * FInfo.FRestartInterval;
        ResetDecoder;
        S.Seek(2, TSeekOrigin.soCurrent);
        FBitReader.Reload;
      end;
  end; // case
end;

procedure TJpegBaselineCoder.HandleRestartInterval(S: TMemoryStream64; Warn: Boolean);
// Restart interval
var
  SuperfluousCount, ReadBytes: Integer;
  B: Byte;
  Tag: Byte;
begin
  ResetDecoder;
  // find + skip restart
  SuperfluousCount := 0;
  repeat
    ReadBytes := S.Read(B, 1);
    if B = $FF then
      begin
        S.Read(Tag, 1);
        case Tag of
          mkRST0 .. mkRST7:
            begin
              // Check restart interval
              if Warn then
                if (Tag - mkRST0) <> (FRstIndex mod 8) then
                  begin
{$IFDEF JPEG_Debug}
                    DoDebugOut(Self, wsWarn, PFormat('WARNING: Restart interval error (expected: %d, found: %d)', [Tag - mkRST0, FRstIndex mod 8]));
{$ENDIF JPEG_Debug}
                  end;
              Break;
            end;
          mkEOI:
            begin
              S.Seek(-2, TSeekOrigin.soCurrent);
              Break;
            end;
          else
            // Any other tag is an error
            if Warn then
              begin
{$IFDEF JPEG_Debug}
                DoDebugOut(Self, wsWarn, sUnexpectedMarkerInEncodedStream);
{$ENDIF JPEG_Debug}
              end;
            Break;
        end;
      end;
    // If we're here, we had superfluous bytes in the stream
    if ReadBytes > 0 then
        inc(SuperfluousCount);
  until ReadBytes = 0;
  if SuperfluousCount > 0 then
    begin
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsWarn, PFormat('WARNING: %d superfluous bytes found at pos %d', [SuperfluousCount, S.Position - SuperfluousCount]));
{$ENDIF JPEG_Debug}
    end;
  inc(FRstIndex);
  FBitReader.Reload;
end;

procedure TJpegBaselineCoder.Initialize(Scale_: TJpegScale);
begin
  inherited;
  // Determine blockstride
  FBlockStride := BlockstrideForScale(Scale);
  // Setup image maps in frame
  if FTileMode then
      SetupMaps(True, 0, 0) // In tilemode, we create maps with zero size, size will be set later
  else
      SetupMaps(False, 0, 0); // otherwise we create maps at full-size
end;

procedure TJpegBaselineCoder.InitializeDecoderTables;
var
  i, j, Idx: Integer;
  Scan: TScanComponent;
  DC: TDCBaselineHuffmanDecoder;
  AC: TACBaselineHuffmanDecoder;
begin
  // Zigzag array that is used
  case BlockStride of
    64: FZigZag := @cJpegInverseZigZag8x8;
    16: FZigZag := @cJpegInverseZigZag4x4;
    4: FZigZag := @cJpegInverseZigZag2x2;
    1: FZigZag := @cJpegInverseZigZag1x1;
  end;

  // Initialize used tables
  FDCCoders.Clear;
  FACCoders.Clear;
  Idx := 0;

  // Loop through image components in scan
  for i := 0 to FInfo.FScanCount - 1 do
    begin
      // Scan's i-th image component info
      Scan := FInfo.FScans[i];
      // Create DC and AC decoders for i-th image
      if not assigned(FDCCoders[Scan.FDCTable])
        and (FInfo.FDCHuffmanTables[Scan.FDCTable].Count > 0) then
        begin
          DC := TDCBaselineHuffmanDecoder.CreateDebug(FOwner);
          FDCCoders[Scan.FDCTable] := DC;
          DC.GenerateLookupTables(FInfo.FDCHuffmanTables[Scan.FDCTable]);
        end;
      if not assigned(FACCoders[Scan.FACTable])
        and (FInfo.FACHuffmanTables[Scan.FACTable].Count > 0) then
        begin
          AC := TACBaselineHuffmanDecoder.CreateDebug(FOwner);
          FACCoders[Scan.FACTable] := AC;
          AC.GenerateLookupTables(FInfo.FACHuffmanTables[Scan.FACTable]);
        end;
      // Assign table numbers to MCU blocks
      for j := 0 to Maps[Scan.FComponent].McuBlockCount(FInfo.FScanCount) - 1 do
        begin
          FMcu[Idx].DCTable := Scan.FDCTable;
          FMcu[Idx].ACTable := Scan.FACTable;
          FMcu[Idx].PPred := @Scan.FPredictor;
          FMcu[Idx].BlockIdx := j;
          FMcu[Idx].MapIdx := Scan.FComponent;
          inc(Idx);
        end;
    end;
end;

procedure TJpegBaselineCoder.InitializeEncoderTables;
var
  i, j, Idx: Integer;
  Scan: TScanComponent;
  DC: TDCBaselineHuffmanEncoder;
  AC: TACBaselineHuffmanEncoder;
begin
  // Initialize used tables
  FDCCoders.Clear;
  FACCoders.Clear;
  Idx := 0;

  // Loop through image components in scan
  for i := 0 to FInfo.FScanCount - 1 do
    begin
      // Scan's i-th image component info
      Scan := FInfo.FScans[i];
      // Create DC and AC decoders for i-th image
      if (not assigned(FDCCoders[Scan.FDCTable])) and ((FInfo.FDCHuffmanTables[Scan.FDCTable].Count > 0) or FIsDryRun) then
        begin
          DC := TDCBaselineHuffmanEncoder.CreateDebug(FOwner);
          FDCCoders[Scan.FDCTable] := DC;
          DC.GenerateCodeTable(FInfo.FDCHuffmanTables[Scan.FDCTable]);
        end;
      if (not assigned(FACCoders[Scan.FACTable])) and ((FInfo.FACHuffmanTables[Scan.FACTable].Count > 0) or FIsDryRun) then
        begin
          AC := TACBaselineHuffmanEncoder.CreateDebug(FOwner);
          FACCoders[Scan.FACTable] := AC;
          AC.GenerateCodeTable(FInfo.FACHuffmanTables[Scan.FACTable]);
        end;
      // Assign table numbers to MCU blocks
      for j := 0 to Maps[Scan.FComponent].McuBlockCount(FInfo.FScanCount) - 1 do
        begin
          FMcu[Idx].DCTable := Scan.FDCTable;
          FMcu[Idx].ACTable := Scan.FACTable;
          FMcu[Idx].PPred := @Scan.FPredictor;
          FMcu[Idx].BlockIdx := j;
          FMcu[Idx].MapIdx := Scan.FComponent;
          inc(Idx);
        end;
    end;
end;

procedure TJpegBaselineCoder.ResetDecoder;
var
  i: Integer;
begin
  FBitReader.Reset;
  // Also reset the DC PRED values
  for i := 0 to FInfo.FScanCount - 1 do
      FInfo.FScans[i].FPredictor := 0;
end;

procedure TJpegBaselineCoder.ResetEncoder;
var
  i: Integer;
begin
  if assigned(FBitWriter) then
      FBitWriter.Restart;
  // Also reset the DC PRED values
  for i := 0 to FInfo.FScanCount - 1 do
      FInfo.FScans[i].FPredictor := 0;
end;

procedure TJpegBaselineCoder.ResizeVerticalMcu(NewVertMcuCount: Integer);
var
  i: Integer;
  HorzBlockCount, VertBlockCount: Integer;
begin
  FVertMcuCount := NewVertMcuCount;
  FInfo.FVertMcuCount := NewVertMcuCount;

  // Resize maps
  for i := 0 to FInfo.FFrameCount - 1 do
    begin
      HorzBlockCount := FInfo.FHorzMcuCount * FInfo.FFrames[i].FHorzSampling;
      VertBlockCount := FInfo.FVertMcuCount * FInfo.FFrames[i].FVertSampling;
      Maps[i].Resize(HorzBlockCount, VertBlockCount);
    end;
end;

{ TJpegProgressiveCoder }

function TJpegProgressiveCoder.BlockstrideForScale(Scale_: TJpegScale): Integer;
begin
  // Blockstride is *always* 64 for progressive coding, because the coder depends
  // on AC coefficents being set.
  Result := 64;
end;

procedure TJpegProgressiveCoder.Decode(S: TMemoryStream64; Iteration: Cardinal);
begin
  // Decide which band (DC or AC) and whether first scan
  FIsDCBand := FInfo.FSpectralStart = 0;
  FIsFirst := FInfo.FApproxHigh = 0;
  FEOBRun := 0;

  if FTileMode then
      raise Exception.Create(sCannotUseTileMode);

  // Use the standard decoder, with overridden methods
  inherited Decode(S, Iteration);
end;

procedure TJpegProgressiveCoder.DecodeMcu(McuX_, McuY_: Integer; Skip: Boolean);
var
  i: Integer;
  McuBlock: PMCUBlock;
begin
  // if (McuX_=0) and (McuY_=0) then
  // DoDebugOut(Self, wsInfo, PFormat(
  // 'progressive decode mcux=%d mcuy=%d isdcband=%d isfirst=%d eobrun=%d',
  // [McuX_, McuY_, integer(FIsDCBand), integer(FIsFirst), FEOBRun]));

  for i := 0 to FMcuBlockCount - 1 do
    begin
      // The current MCU block
      McuBlock := @FMcu[i];

      // Initialize MCU values pointer
      if FInfo.FScanCount > 1 then
          McuBlock^.Values := Maps[McuBlock^.MapIdx].GetCoefPointerMCU(McuX_, McuY_, McuBlock^.BlockIdx)
      else
          McuBlock^.Values := Maps[McuBlock^.MapIdx].GetCoefPointer(McuX_, McuY_);

      // Each MCU block has an index to a DC and AC table, use it to do the decoding
      if FIsDCBand and assigned(FDCCoders[McuBlock^.DCTable]) then
        begin
          if FIsFirst then
              TDCProgressiveHuffmanDecoder(FDCCoders[McuBlock^.DCTable]).DecodeProgFirst(McuBlock^, FBitReader, FInfo.FApproxLow)
          else
              TDCProgressiveHuffmanDecoder(FDCCoders[McuBlock^.DCTable]).DecodeProgRefine(McuBlock^, FBitReader, FInfo.FApproxLow);
        end;
      if not FIsDCBand and assigned(FACCoders[McuBlock^.ACTable]) then
        begin
          if FIsFirst then
              TACProgressiveHuffmanDecoder(FACCoders[McuBlock^.ACTable]).DecodeProgFirst(McuBlock^, FBitReader, FEOBRun, FInfo.FSpectralStart, FInfo.FSpectralEnd, FInfo.FApproxLow)
          else
              TACProgressiveHuffmanDecoder(FACCoders[McuBlock^.ACTable]).DecodeProgRefine(McuBlock^, FBitReader, FEOBRun, FInfo.FSpectralStart, FInfo.FSpectralEnd, FInfo.FApproxLow);
        end;

      if FBitReader.HitEndOfStream then
        begin
{$IFDEF JPEG_Debug}
          DoDebugOut(Self, wsInfo, 'hit end of stream');
{$ENDIF JPEG_Debug}
          exit;
        end;
    end;
end;

procedure TJpegProgressiveCoder.Finalize;
begin
  CorrectBlockStride;
end;

procedure TJpegProgressiveCoder.HandleRestartInterval(S: TMemoryStream64; Warn: Boolean);
begin
  inherited;
  // Reset EOB run
  FEOBRun := 0;
end;

procedure TJpegProgressiveCoder.InitializeDecoderTables;
var
  i, j, Idx: Integer;
  Scan: TScanComponent;
  DC: TDCProgressiveHuffmanDecoder;
  AC: TACProgressiveHuffmanDecoder;
begin
  // Initialize tables to use (max 4 per AC/DC)
  FDCCoders.Clear;
  FACCoders.Clear;
  Idx := 0;

  // Loop through image components in scan
  for i := 0 to FInfo.FScanCount - 1 do
    begin
      // Scan's i-th image component info
      Scan := FInfo.FScans[i];

      // Create DC and AC decoders for i-th image
      if FIsDCBand
        and not assigned(FDCCoders[Scan.FDCTable])
        and (FInfo.FDCHuffmanTables[Scan.FDCTable].Count > 0) then
        begin
          DC := TDCProgressiveHuffmanDecoder.CreateDebug(FOwner);
          FDCCoders[Scan.FDCTable] := DC;
          DC.GenerateLookupTables(FInfo.FDCHuffmanTables[Scan.FDCTable]);
        end;
      if not FIsDCBand
        and not assigned(FACCoders[Scan.FACTable])
        and (FInfo.FACHuffmanTables[Scan.FACTable].Count > 0) then
        begin
          AC := TACProgressiveHuffmanDecoder.CreateDebug(FOwner);
          FACCoders[Scan.FACTable] := AC;
          AC.GenerateLookupTables(FInfo.FACHuffmanTables[Scan.FACTable]);
        end;

      // Assign table numbers to MCU blocks
      for j := 0 to Maps[Scan.FComponent].McuBlockCount(FInfo.FScanCount) - 1 do
        begin
          FMcu[Idx].DCTable := Scan.FDCTable;
          FMcu[Idx].ACTable := Scan.FACTable;
          FMcu[Idx].PPred := @Scan.FPredictor;
          FMcu[Idx].BlockIdx := j;
          FMcu[Idx].MapIdx := Scan.FComponent;
          inc(Idx);
        end;
    end;
end;

end.
