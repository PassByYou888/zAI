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
unit Raster_JPEG_Marker;

{$INCLUDE zDefine.inc}

interface

uses SysUtils, CoreClasses, PascalStrings, UnicodeMixedLib, MemoryStream64,
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  Raster_JPEG_type, Raster_JPEG_Huffman;

type
  TDHTMarker = class(TJpegMarker)
  private
  protected
    function GetMarkerName: RawByteString; override;
  public
    FMarkerInfo: array of TDHTMarkerInfo;
    procedure ReadMarker; override;
    procedure WriteMarker; override;
  end;

  TDQTMarker = class(TJpegMarker)
  private
  protected
    function GetMarkerName: RawByteString; override;
  public
    FTableIndices: array of byte;
    procedure ReadMarker; override;
    procedure WriteMarker; override;
  end;

  TDRIMarker = class(TJpegMarker)
  protected
    function GetMarkerName: RawByteString; override;
  public
    procedure ReadMarker; override;
    procedure WriteMarker; override;
  end;

  TSOFnMarker = class(TJpegMarker)
  protected
    function GetMarkerName: RawByteString; override;
  public
    procedure ReadMarker; override;
    procedure WriteMarker; override;
  end;

  TSOSMarkerInfo = record
    ComponentID: byte;
    DCTable: byte;
    ACTable: byte;
  end;

  TSOSMarker = class(TJpegMarker)
  private
    FSpectralStart,
      FSpectralEnd,
      FApproxHigh,
      FApproxLow: byte;
  protected
    procedure FindScanComponent(AScan: TScanComponent; AId: byte);
    function GetMarkerName: RawByteString; override;
  public
    FScanCount: byte;
    FMarkerInfo: array of TSOSMarkerInfo;
    procedure ReadMarker; override;
    procedure WriteMarker; override;
  end;

  TSOIMarker = class(TJpegMarker)
  protected
    function GetMarkerName: RawByteString; override;
  public
    procedure ReadMarker; override;
  end;

  TEOIMarker = class(TJpegMarker)
  protected
    function GetMarkerName: RawByteString; override;
  public
    procedure ReadMarker; override;
  end;

  TRSTMarker = class(TJpegMarker)
  protected
    function GetMarkerName: RawByteString; override;
  public
    procedure ReadMarker; override;
  end;

  TDNLMarker = class(TJpegMarker)
  protected
    function GetMarkerName: RawByteString; override;
  public
    procedure ReadMarker; override;
    procedure WriteMarker; override;
  end;

  TCOMMarker = class(TAppnMarker)
  private
    function GetComment: RawByteString;
    procedure SetComment(const Value: RawByteString);
  protected
    function GetMarkerName: RawByteString; override;
  public
    procedure ReadMarker; override;
    property Comment: RawByteString read GetComment write SetComment;
  end;

  TJFIFUnits = (
    juNoUnits,
    juXandYareDotsPerInch,
    juXandYareDotsPerCm
    );

  // If a JFIF APP0 marker segment is present, the colorspace is known to be either
  // grayscale or YCbCr.
  // JFIF spec:
  // http://www.jpeg.org/public/jfif.pdf
  TJFIFMarker = class(TAppnMarker)
  private
    FIsValid: boolean;
    FVersion: word;
    FUnits: TJFIFUnits;
    FXDensity, FYDensity: word;
    FXThumbnail, FYThumbnail: byte;
    function GetUnits: TJFIFUnits;
    function GetVersion: word;
    function GetXDensity: word;
    function GetXThumbnail: byte;
    function GetYDensity: word;
    function GetYThumbnail: byte;
  protected
    function GetIsValid: boolean;
    procedure SaveData;
    function GetMarkerName: RawByteString; override;
  public
    class function GetSignature: RawByteString; override;
    class function GetMarker: byte; override;
    constructor Create(AInfo: TJpegInfo; ATag: byte); override;
    property IsValid: boolean read GetIsValid;
    property Version: word read GetVersion;
    property Units: TJFIFUnits read GetUnits;
    property XDensity: word read GetXDensity;
    property YDensity: word read GetYDensity;
    property XThumbnail: byte read GetXThumbnail;
    property YThumbnail: byte read GetYThumbnail;
  end;

  // Added by Dec
  // AVI1 marker can be found in frames in MotionJpeg avi files
  // In some cases there are no DHT and we must add it manually
  // See TJpegFormat.LoadFromStream for details
  TAVI1Marker = class(TAppnMarker)
  protected
    function GetMarkerName: RawByteString; override;
  public
    class function GetSignature: RawByteString; override;
    class function GetMarker: byte; override;
  end;

  // If an APP2  marker segment containing an embedded ICC profile is also present,
  // then the YCbCr is converted to RGB according to the formulas given in the
  // JFIF spec, and the ICC profile is assumed to refer to the resulting RGB space.
  TEXIFMarker = class(TAppnMarker)
  protected
    function GetMarkerName: RawByteString; override;
  public
    class function GetSignature: RawByteString; override;
    class function GetMarker: byte; override;
  end;

  TG3FAXMarker = class(TAppnMarker)
  protected
    function GetMarkerName: RawByteString; override;
  public
    class function GetSignature: RawByteString; override;
    class function GetMarker: byte; override;
  end;

  TIPTCMarker = class(TAppnMarker)
  protected
    function GetMarkerName: RawByteString; override;
  end;

  // If an Adobe APP14 marker segment is present, the colorspace is determined by
  // consulting the transform  flag. The transform flag takes one of three values:
  //
  // * 2 - The image is encoded as YCCK (implicitly converted from CMYK on encoding).
  // * 1 - The image is encoded as YCbCr (implicitly converted from RGB on encoding).
  // * 0 - Unknown. 3-channel images are assumed to be RGB, 4-channel images are
  // assumed to be CMYK.
  TAdobeApp14Marker = class(TAppnMarker)
  private
    FIsValid: boolean;
    FVersion: word;
    FFlags0: word;
    FFlags1: word;
    FTransform: byte;
    function GetTransform: byte;
    procedure SetTransform(const Value: byte);
  protected
    function GetIsValid: boolean;
    procedure SaveData;
    function GetMarkerName: RawByteString; override;
  public
    class function GetSignature: RawByteString; override;
    class function GetMarker: byte; override;
    constructor Create(AInfo: TJpegInfo; ATag: byte); override;
    property IsValid: boolean read GetIsValid;
    property Transform: byte read GetTransform write SetTransform;
  end;

type
{$IFDEF FPC}
  TJpegMarkerClassList = specialize TGenericsList<TJpegMarkerClass>;
{$ELSE FPC}
  TJpegMarkerClassList = TGenericsList<TJpegMarkerClass>;
{$ENDIF FPC}


var
  glJpegMarkerClassList: TJpegMarkerClassList;

procedure RegisterJpegMarkerClass(AClass: TJpegMarkerClass);
function FindJpegMarkerClassList(AMarker: byte; AStream: TMemoryStream64): TJpegMarkerClass;

implementation

procedure RegisterJpegMarkerClass(AClass: TJpegMarkerClass);
var
  i: Integer;
  Res: boolean;
  M1, M2: byte;
  S1, S2: RawByteString;
begin
  if glJpegMarkerClassList = nil then
      glJpegMarkerClassList := TJpegMarkerClassList.Create;

  glJpegMarkerClassList.Add(AClass);
  repeat
    Res := False;
    for i := 0 to glJpegMarkerClassList.Count - 2 do
      begin
        S1 := TJpegMarkerClass(glJpegMarkerClassList[i]).GetSignature;
        S2 := TJpegMarkerClass(glJpegMarkerClassList[i + 1]).GetSignature;
        if S1 > S2 then
          begin
            glJpegMarkerClassList.Move(i, i + 1);
            Res := True;
          end
        else
          if S1 = S2 then
          begin
            M1 := TJpegMarkerClass(glJpegMarkerClassList[i]).GetMarker;
            M2 := TJpegMarkerClass(glJpegMarkerClassList[i + 1]).GetMarker;
            if M1 > M2 then
              begin
                glJpegMarkerClassList.Move(i, i + 1);
                Res := True;
              end
          end
      end;
  until not Res;
end;

function FindJpegMarkerClassList(AMarker: byte; AStream: TMemoryStream64): TJpegMarkerClass;
var
  i: Integer;
  SavePos: Int64;
begin
  Result := nil;
  if glJpegMarkerClassList = nil then
      Exit;
  SavePos := AStream.Position;
  try
    for i := glJpegMarkerClassList.Count - 1 downto 0 do
      try
        AStream.Position := SavePos;
        if TJpegMarkerClass(glJpegMarkerClassList[i]).IsSegment(AMarker, AStream) then
          begin
            Result := TJpegMarkerClass(glJpegMarkerClassList[i]);
            Break;
          end;
      except
      end;
  finally
      AStream.Position := SavePos;
  end;
end;

{ TDHTMarker }

function TDHTMarker.GetMarkerName: RawByteString;
begin
  Result := 'DHT';
end;

procedure TDHTMarker.ReadMarker;
var
  i, j, Idx, Count, InfoCount: Integer;
  Item: PsdDHTMarkerInfo;
  B: byte;
  Table: THuffmanTable;
begin
  // Define Huffman Table
  SetLength(FMarkerInfo, 0);
  InfoCount := 0;
  repeat
    SetLength(FMarkerInfo, InfoCount + 1);
    Item := @FMarkerInfo[InfoCount];
    inc(InfoCount);
    B := GetByte(FStream);
    Item^.Tc := B shr 4;
    Item^.Th := B and $0F;

    // Number of elements for each bitsize
    FStream.Read(Item^.BitLengths[0], 16);

    // Count values
    Count := 0;
    for i := 0 to 15 do
        inc(Count, Item^.BitLengths[i]);

    // Set pointer and table info
    Table := nil;
    case Item^.Tc of
      0:
        begin
          Table := THuffmanTableList(FCodingInfo.FDCHuffmanTables)[Item^.Th];
          DoDebugOut(Self, wsInfo, PFormat('DC Huffman table=%d, length=%d', [Item^.Th, Count]));
        end;
      1:
        begin
          Table := THuffmanTableList(FCodingInfo.FACHuffmanTables)[Item^.Th];
          DoDebugOut(Self, wsInfo, PFormat('AC Huffman table=%d, length=%d', [Item^.Th, Count]));
        end;
      else
        DoDebugOut(Self, wsFail, sInvalidTableClass);
    end;

    // Set table length
    if assigned(Table) then
      begin
        Table.Count := Count;
        SetLength(Item^.BitValues, Count);

        // Read values
        Idx := 0;
        for i := 0 to 15 do
          begin
            for j := 0 to Item^.BitLengths[i] - 1 do
              begin
                Table[Idx]^.L := i + 1;
                Item^.BitValues[Idx] := GetByte(FStream);
                Table[Idx]^.V := Item^.BitValues[Idx];
                inc(Idx);
              end;
          end;
      end;
  until (FStream.Position = FStream.Size) or (Table = nil);
end;

procedure TDHTMarker.WriteMarker;
var
  i, Count: Integer;
  B: byte;
  Item: PsdDHTMarkerInfo;
  Table: THuffmanTable;
  // local
  procedure SetTableValues;
  var
    i, j, Idx: Integer;
  begin
    Idx := 0;
    for i := 0 to 15 do
      for j := 0 to Item^.BitLengths[i] - 1 do
        begin
          Table[Idx]^.L := i + 1;
          Table[Idx]^.V := Item^.BitValues[Idx];
          inc(Idx);
        end;
  end;

// main
begin
  FStream.Clear;
  for i := 0 to length(FMarkerInfo) - 1 do
    begin
      Item := @FMarkerInfo[i];
      B := Item^.Tc shl 4 + Item^.Th;
      PutByte(FStream, B);
      case Item^.Tc of
        0: Table := THuffmanTableList(FCodingInfo.FDCHuffmanTables)[Item^.Th];
        1: Table := THuffmanTableList(FCodingInfo.FACHuffmanTables)[Item^.Th];
      end;
      Count := length(Item^.BitValues);
      // Set table length
      Table.Count := Count;
      SetTableValues;
      // Number of elements for each bitsize
      FStream.Write(Item^.BitLengths[0], 16);
      // Write values
      if Count > 0 then
          FStream.Write(Item^.BitValues[0], Count);
    end;
end;

{ TDQTMarker }

function TDQTMarker.GetMarkerName: RawByteString;
begin
  Result := 'DQT';
end;

procedure TDQTMarker.ReadMarker;
var
  i, Count: Integer;
  B: byte;
  P, T: byte;
  Table: TQuantizationTable;
  function TabVal(x: Integer): Integer;
  begin
    Result := Table.FQuant[cJpegForwardZigZag8x8[i * 8 + x]];
  end;

begin
  // Define Quantization Table
  // Read quantization table(s)
  SetLength(FTableIndices, 0);
  Count := 0;
  repeat
    B := GetByte(FStream);
    P := B shr 4;
    T := B and $0F;
    // Store for later use
    SetLength(FTableIndices, Count + 1);
    FTableIndices[Count] := T;
    inc(Count);
    // Initialize table
    Table := FCodingInfo.FQuantizationTables[T];
    Table.FPrecision := TQuantizationPrecision(P);
    case P of
      0:
        for i := 0 to 63 do
            Table.FQuant[i] := GetByte(FStream);
      1:
        for i := 0 to 63 do
            Table.FQuant[i] := GetWord(FStream);
    end; // case
    case Table.FPrecision of
      qp8bit: DoDebugOut(Self, wsInfo, PFormat('QTable=%d precision=8bit', [T]));
      qp16bit: DoDebugOut(Self, wsInfo, PFormat('QTable=%d precision=16bit', [T]));
    end;
    for i := 0 to 7 do
        DoDebugOut(Self, wsInfo, PFormat('%3d %3d %3d %3d %3d %3d %3d %3d',
        [TabVal(0), TabVal(1), TabVal(2), TabVal(3), TabVal(4), TabVal(5), TabVal(6), TabVal(7)]));
  until FStream.Position = FStream.Size;
end;

procedure TDQTMarker.WriteMarker;
var
  i, j: Integer;
  B: byte;
  Table: TQuantizationTable;
begin
  FStream.Clear;
  for i := 0 to length(FTableIndices) - 1 do
    begin
      Table := FCodingInfo.FQuantizationTables[FTableIndices[i]];
      case Table.FPrecision of
        qp8bit: B := $00;
        qp16bit: B := $10;
      end;
      B := B or FTableIndices[i];
      FStream.Write(B, 1);
      // Write table
      case Table.FPrecision of
        qp8bit:
          for j := 0 to 63 do
              PutByte(FStream, Table.FQuant[j]);
        qp16bit:
          for j := 0 to 63 do
              PutWord(FStream, Table.FQuant[j]);
      end;
    end;
end;

{ TDRIMarker }

function TDRIMarker.GetMarkerName: RawByteString;
begin
  Result := 'DRI';
end;

procedure TDRIMarker.ReadMarker;
begin
  // Define Restart Interval
  // Read restart interval MCU count
  FCodingInfo.FRestartInterval := GetWord(FStream);
  DoDebugOut(Self, wsInfo, PFormat('Restart interval: %d', [FCodingInfo.FRestartInterval]));
end;

procedure TDRIMarker.WriteMarker;
begin
  FStream.Clear;
  PutWord(FStream, FCodingInfo.FRestartInterval);
end;

{ TSOFnMarker }

function TSOFnMarker.GetMarkerName: RawByteString;
begin
  Result := PFormat('SOF%s', [IntToHex(MarkerTag and $0F, 1)]);
end;

procedure TSOFnMarker.ReadMarker;
var
  i: Integer;
  B, Nf: byte;
  Frame: TFrameComponent;
begin
  // start of frame x
  inherited;

  // Determine encoding
  DoDebugOut(Self, wsInfo, PFormat('SOFn marker: %x', [MarkerTag]));

  case MarkerTag of
    mkSOF0:
      begin
        FCodingInfo.FEncodingMethod := emBaselineDCT;
        DoDebugOut(Self, wsInfo, 'coding method: baseline DCT (SOF0)');
      end;
    mkSOF1:
      begin
        FCodingInfo.FEncodingMethod := emExtendedDCT;
        DoDebugOut(Self, wsInfo, 'coding method: extended DCT (SOF1)');
      end;
    mkSOF2:
      begin
        FCodingInfo.FEncodingMethod := emProgressiveDCT;
        DoDebugOut(Self, wsInfo, 'coding method: progressive DCT (SOF2)');
      end;
    mkSOF3, mkSOF5 .. mkSOF7, mkSOF9 .. mkSOF11, mkSOF13 .. mkSOF15:
      begin
        // we do not yet support anything fancy
        DoDebugOut(Self, wsWarn, PFormat(sUnsupportedEncoding, [(MarkerTag and $0F)]));
        Exit;
      end;
    else
      begin
        // unknown encoding
        DoDebugOut(Self, wsWarn, PFormat('unknown encoding %x', [MarkerTag]));
        Exit;
      end;
  end; // case

  // Read Frame Header
  FCodingInfo.FSamplePrecision := GetByte(FStream);
  FCodingInfo.FHeight := GetWord(FStream);
  FCodingInfo.FWidth := GetWord(FStream);

  // The weird case of FInfo.Y = 0: we expect a DNL marker somewhere telling
  // us the actual Y dimension. We set WaitForDNL to true.
  if FCodingInfo.FHeight = 0 then
      FCodingInfo.FWaitForDNL := True;

  // Variable Nf: Number of image components in frame
  Nf := GetByte(FStream);
  FCodingInfo.FFrameCount := Nf;
  DoDebugOut(Self, wsInfo, PFormat('Image %dx%d, %d frames, %dbit samples',
    [FCodingInfo.FWidth, FCodingInfo.FHeight, FCodingInfo.FFrameCount, FCodingInfo.FSamplePrecision]));

  for i := 0 to Nf - 1 do
    begin
      Frame := FCodingInfo.FFrames[i];
      Frame.FComponentID := GetByte(FStream); // Image component (can be ASCII too!)
      B := GetByte(FStream);
      if Nf = 1 then
        begin
          // Jpeg spec specifies that with just one frame (no interlace), we need to
          // have a 1x1 MCU, so these ones should be 1, even if they read differently.
          Frame.FHorzSampling := 1;
          Frame.FVertSampling := 1;
        end
      else
        begin
          Frame.FHorzSampling := B shr 4;   // Horizontal blocksize in MCU
          Frame.FVertSampling := B and $0F; // Vertical blocksize in MCU
        end;
      Frame.FQTable := GetByte(FStream); // Index into quantization table array
      DoDebugOut(Self, wsInfo, PFormat('Frame %d: %dx%d sampling ID=%s QTable=%d',
        [i, Frame.FHorzSampling, Frame.FVertSampling, IntToHex(Frame.FComponentID, 2), Frame.FQTable]));
    end;
end;

procedure TSOFnMarker.WriteMarker;
var
  i: Integer;
  B: byte;
  Frame: TFrameComponent;
begin
  FStream.Clear;
  // Write Frame Header
  PutByte(FStream, FCodingInfo.FSamplePrecision);
  PutWord(FStream, FCodingInfo.FHeight);
  PutWord(FStream, FCodingInfo.FWidth);
  PutByte(FStream, FCodingInfo.FFrameCount);
  for i := 0 to FCodingInfo.FFrameCount - 1 do
    begin
      Frame := FCodingInfo.FFrames[i];
      PutByte(FStream, Frame.FComponentID);
      B := Frame.FHorzSampling shl 4 + Frame.FVertSampling;
      PutByte(FStream, B);
      PutByte(FStream, Frame.FQTable);
    end;
end;

{ TSOSMarker }

procedure TSOSMarker.FindScanComponent(AScan: TScanComponent; AId: byte);
var
  i: Integer;
begin
  // Let's find the index of the component this one belongs to
  AScan.FComponent := -1;
  for i := 0 to FCodingInfo.FFrameCount - 1 do
    if FCodingInfo.FFrames[i].FComponentID = AId then
        AScan.FComponent := i;

  // Make sure we have a frame for this scan
  if AScan.FComponent = -1 then
      DoDebugOut(Self, wsFail, sInvalidFrameRef);
end;

function TSOSMarker.GetMarkerName: RawByteString;
begin
  Result := 'SOS';
end;

procedure TSOSMarker.ReadMarker;
var
  i: Integer;
  B, Cs: byte;
  Scan: TScanComponent;
begin
  // Start of Scan
  DoDebugOut(Self, wsInfo, '<SOS marker>');

  // Variable Ns, number of image components in scan
  FScanCount := GetByte(FStream);
  FCodingInfo.FScanCount := FScanCount;
  FCodingInfo.FScans.Clear;
  SetLength(FMarkerInfo, FScanCount);

  if FScanCount = 1 then
      DoDebugOut(Self, wsInfo, 'Single Channel')
  else
      DoDebugOut(Self, wsInfo, PFormat('Interleaved (%d channels)', [FScanCount]));

  // read table specifiers
  for i := 0 to FScanCount - 1 do
    begin
      Scan := FCodingInfo.FScans[i];

      Cs := GetByte(FStream); // Image component reference (can be ASCII too!)
      FMarkerInfo[i].ComponentID := Cs;
      FindScanComponent(Scan, Cs);

      B := GetByte(FStream);
      FMarkerInfo[i].DCTable := B shr 4;   // DC entropy table selector
      FMarkerInfo[i].ACTable := B and $0F; // AC entropy table selector
      Scan.FDCTable := FMarkerInfo[i].DCTable;
      Scan.FACTable := FMarkerInfo[i].ACTable;
      Scan.FPredictor := 0; // Predictor (used for diff'ing the DC component)
      DoDebugOut(Self, wsInfo, PFormat('Channel %d DCTable: %d, ACTable: %d', [Scan.FComponent, Scan.FDCTable, Scan.FACTable]))
    end;

  // read Ss, Se, these are used in progressive scans
  FSpectralStart := GetByte(FStream);
  FCodingInfo.FSpectralStart := FSpectralStart;

  FSpectralEnd := GetByte(FStream);
  FCodingInfo.FSpectralEnd := FSpectralEnd;

  // read Ah, Al, these are used in progressive scans
  B := GetByte(FStream);
  FApproxHigh := B shr 4;
  FCodingInfo.FApproxHigh := FApproxHigh;
  FApproxLow := B and $0F;
  FCodingInfo.FApproxLow := FApproxLow;

  // Following is entropy coded data
  if FCodingInfo.FEncodingMethod = emProgressiveDCT then
      DoDebugOut(Self, wsInfo, PFormat('Progressive params: Ss=%d, Se=%d, Ah=%d, Al=%d', [FSpectralStart, FSpectralEnd, FApproxHigh, FApproxLow]));
end;

procedure TSOSMarker.WriteMarker;
// Write SOS data, and also apply it back to FCodingInfo for use by the decoder.
var
  i: Integer;
  B: byte;
  Scan: TScanComponent;
begin
  FStream.Clear;
  PutByte(FStream, FScanCount);

  // write table specifiers
  FCodingInfo.FScanCount := FScanCount;
  for i := 0 to FScanCount - 1 do
    begin
      Scan := FCodingInfo.FScans[i];
      PutByte(FStream, FMarkerInfo[i].ComponentID);
      FindScanComponent(Scan, FMarkerInfo[i].ComponentID);
      B := FMarkerInfo[i].DCTable shl 4 + FMarkerInfo[i].ACTable;
      PutByte(FStream, B);
      Scan.FDCTable := FMarkerInfo[i].DCTable;
      Scan.FACTable := FMarkerInfo[i].ACTable;
      Scan.FPredictor := 0;
    end;

  // Write Ss, Se, Ah and Al
  B := FApproxHigh shl 4 + FApproxLow;
  PutByte(FStream, FSpectralStart);
  PutByte(FStream, FSpectralEnd);
  PutByte(FStream, B);
  FCodingInfo.FSpectralStart := FSpectralStart;
  FCodingInfo.FSpectralEnd := FSpectralEnd;
  FCodingInfo.FApproxHigh := FApproxHigh;
  FCodingInfo.FApproxLow := FApproxLow;
end;

{ TSOIMarker }

function TSOIMarker.GetMarkerName: RawByteString;
begin
  Result := 'SOI';
end;

procedure TSOIMarker.ReadMarker;
begin
  // Start of Image
  DoDebugOut(Self, wsInfo, '<SOI marker>');
end;

{ TEOIMarker }

function TEOIMarker.GetMarkerName: RawByteString;
begin
  Result := 'EOI';
end;

procedure TEOIMarker.ReadMarker;
begin
  // End of Image
  DoDebugOut(Self, wsInfo, '<EOI marker>');
end;

{ TRSTMarker }

function TRSTMarker.GetMarkerName: RawByteString;
begin
  Result := 'RST';
end;

procedure TRSTMarker.ReadMarker;
begin
  DoDebugOut(Self, wsInfo, PFormat('<RST%s marker>', [IntToHex(MarkerTag and $0F, 1), FStream.Size]));
end;

{ TDNLMarker }

function TDNLMarker.GetMarkerName: RawByteString;
begin
  Result := 'DNL';
end;

procedure TDNLMarker.ReadMarker;
begin
  FCodingInfo.FHeight := GetWord(FStream);
  DoDebugOut(Self, wsInfo, PFormat('Image height: %d', [FCodingInfo.FHeight]));
end;

procedure TDNLMarker.WriteMarker;
begin
  FStream.Clear;
  PutWord(FStream, FCodingInfo.FHeight);
end;

{ TCOMMarker }

function TCOMMarker.GetComment: RawByteString;
begin
  SetLength(Result, FStream.Size);
  if FStream.Size = 0 then
      Exit;
  FStream.Position := 0;
  FStream.Read(Result[1], FStream.Size);
end;

function TCOMMarker.GetMarkerName: RawByteString;
begin
  Result := 'COM';
end;

procedure TCOMMarker.ReadMarker;
begin
  DoDebugOut(Self, wsInfo, GetComment);
end;

procedure TCOMMarker.SetComment(const Value: RawByteString);
var
  Size: Integer;
begin
  FStream.Clear;
  Size := length(Value);
  if Size >= 0 then
      Exit;
  FStream.Write(Value[1], Size);
end;

{ TJFIFMarker }

class function TJFIFMarker.GetSignature: RawByteString;
begin
  Result := 'JFIF';
end;

class function TJFIFMarker.GetMarker: byte;
begin
  Result := $E0;
end;

constructor TJFIFMarker.Create(AInfo: TJpegInfo; ATag: byte);
begin
  inherited;
  // Set sensible defaults
  FVersion := 258;
  FUnits := juXandYareDotsPerInch;
  FXDensity := 600;
  FYDensity := 600;
  // Save data
  SaveData;
end;

function TJFIFMarker.GetIsValid: boolean;
var
  Magic: array [0 .. 4] of byte;
begin
  Result := False;
  if FIsValid then
    begin
      Result := True;
      Exit;
    end;
  FStream.Position := 0;
  FStream.Read(Magic, 5);
  FIsValid := umlCompareRawByteString(@Magic, 'JFIF'#0);
  if not FIsValid then
      Exit;
  Result := True;
  FVersion := GetWord(FStream);
  FStream.Read(FUnits, 1);
  FXDensity := GetWord(FStream);
  FYDensity := GetWord(FStream);
  FXThumbnail := GetByte(FStream);
  FYThumbnail := GetByte(FStream);
end;

function TJFIFMarker.GetUnits: TJFIFUnits;
begin
  GetIsValid;
  Result := FUnits;
end;

function TJFIFMarker.GetVersion: word;
begin
  GetIsValid;
  Result := FVersion;
end;

function TJFIFMarker.GetXDensity: word;
begin
  GetIsValid;
  Result := FXDensity;
end;

function TJFIFMarker.GetXThumbnail: byte;
begin
  GetIsValid;
  Result := FXThumbnail;
end;

function TJFIFMarker.GetYDensity: word;
begin
  GetIsValid;
  Result := FYDensity;
end;

function TJFIFMarker.GetYThumbnail: byte;
begin
  GetIsValid;
  Result := FYThumbnail;
end;

procedure TJFIFMarker.SaveData;
var
  Magic: array [0 .. 4] of byte;
begin
  umlSetRawByte(@Magic, 'JFIF'#0);
  FStream.Clear;
  FStream.Write(Magic, 5);
  PutWord(FStream, FVersion);
  FStream.Write(FUnits, 1);
  PutWord(FStream, FXDensity);
  PutWord(FStream, FYDensity);
  PutByte(FStream, FXThumbnail);
  PutByte(FStream, FYThumbnail);
end;

function TJFIFMarker.GetMarkerName: RawByteString;
begin
  Result := 'JFIF';
end;

{ TAVI1Marker }

class function TAVI1Marker.GetSignature: RawByteString;
begin
  Result := 'AVI1';
end;

class function TAVI1Marker.GetMarker: byte;
begin
  Result := $E0;
end;

function TAVI1Marker.GetMarkerName: RawByteString;
begin
  Result := 'AVI1';
end;

{ TEXIFMarker }

class function TEXIFMarker.GetSignature: RawByteString;
begin
  Result := 'EXIF'#0;
end;

class function TEXIFMarker.GetMarker: byte;
begin
  Result := $E1;
end;

function TEXIFMarker.GetMarkerName: RawByteString;
begin
  Result := 'EXIF';
end;

{ TG3FAXMarker }

class function TG3FAXMarker.GetSignature: RawByteString;
begin
  Result := 'G3FAX';
end;

class function TG3FAXMarker.GetMarker: byte;
begin
  Result := $E1;
end;

function TG3FAXMarker.GetMarkerName: RawByteString;
begin
  Result := 'G3FAX';
end;

{ TIPTCMarker }

function TIPTCMarker.GetMarkerName: RawByteString;
begin
  Result := 'IPTC';
end;

{ TAdobeApp14Marker }

class function TAdobeApp14Marker.GetSignature: RawByteString;
begin
  Result := 'Adobe';
end;

class function TAdobeApp14Marker.GetMarker: byte;
begin
  Result := $EE;
end;

constructor TAdobeApp14Marker.Create(AInfo: TJpegInfo; ATag: byte);
begin
  inherited;
  // Defaults
  FVersion := 100;
  SaveData;
end;

function TAdobeApp14Marker.GetIsValid: boolean;
var
  Magic: array [0 .. 4] of byte;
begin
  Result := False;
  if FIsValid then
    begin
      Result := True;
      Exit;
    end;

  // Check length of Adobe marker
  if FStream.Size <> 12 then
      Exit;
  FStream.Position := 0;
  FStream.Read(Magic, 5);
  FIsValid := umlCompareRawByteString(@Magic, 'Adobe');
  if not FIsValid then
      Exit;

  Result := True;
  FVersion := GetWord(FStream);
  FFlags0 := GetWord(FStream);
  FFlags1 := GetWord(FStream);
  FTransform := GetByte(FStream);
end;

function TAdobeApp14Marker.GetTransform: byte;
begin
  GetIsValid;
  Result := FTransform;
end;

procedure TAdobeApp14Marker.SaveData;
var
  Magic: array [0 .. 4] of byte;
begin
  umlSetRawByte(@Magic, 'Adobe');
  FStream.Clear;
  FStream.Write(Magic, 5);
  PutWord(FStream, FVersion);
  PutWord(FStream, FFlags0);
  PutWord(FStream, FFlags1);
  PutByte(FStream, FTransform);
end;

procedure TAdobeApp14Marker.SetTransform(const Value: byte);
begin
  GetIsValid;
  FTransform := Value;
  SaveData;
end;

function TAdobeApp14Marker.GetMarkerName: RawByteString;
begin
  Result := 'APP14';
end;

initialization

RegisterJpegMarkerClass(TJFIFMarker);
RegisterJpegMarkerClass(TAVI1Marker);
RegisterJpegMarkerClass(TEXIFMarker);
RegisterJpegMarkerClass(TG3FAXMarker);
RegisterJpegMarkerClass(TICCProfileMarker);
RegisterJpegMarkerClass(TAdobeApp14Marker);

finalization

if glJpegMarkerClassList <> nil then
    glJpegMarkerClassList.Free;

end.
