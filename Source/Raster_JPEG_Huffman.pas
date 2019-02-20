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
unit Raster_JPEG_Huffman;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, MemoryStream64, Raster_JPEG_type, Raster_JPEG_BitStream;

type
  // Huffman table values + codes specified in DHT marker
  THuffmanTable = class(TCoreClassPersistent)
  private
    FItems: array of THuffmanCode;
    function GetItems(Index: integer): PsdHuffmanCode;
    function GetCount: integer;
    procedure SetCount(const Value: integer);
  public
    property Items[Index: integer]: PsdHuffmanCode read GetItems; default;
    property Count: integer read GetCount write SetCount;
  end;

  THuffmanTableList = class(TCoreClassObjectList)
  private
    function GetItems(Index: integer): THuffmanTable;
  public
    property Items[Index: integer]: THuffmanTable read GetItems; default;
  end;

  TEntropyCoder = class(TJPEG_Persistent)
  public
    constructor Create; virtual;
  end;

  TEntropyCoderList = class(TCoreClassObjectList)
  private
    function GetItems(Index: integer): TEntropyCoder;
    procedure SetItems(Index: integer; const Value: TEntropyCoder);
  public
    property Items[Index: integer]: TEntropyCoder read GetItems write SetItems; default;
  end;

  // Generic Huffman coder implementing shared methods
  THuffmanCoder = class(TEntropyCoder)
  private
  protected
    FCodes: array of THuffmanCode;
  public
    procedure GenerateCodeTable(ATable: THuffmanTable); virtual;
  end;

  // Generic Huffman decoder
  THuffmanDecoder = class(THuffmanCoder)
  private
  protected
    FLookup: array of THuffmanLookupTable;
    FLookupCount: word;
  public
    FCountCodes: integer;
    FCountBits: integer;
    procedure AddToLookupTable(Table, Code, Len, Value: integer);
    procedure GenerateLookupTables(Table: THuffmanTable); virtual;
  end;

  // General 8-bit huffman decoder
  T8bitHuffmanDecoder = class(THuffmanDecoder)
  public
    procedure GenerateLookupTables(Table: THuffmanTable); override;
  end;

  THuffmanNode = class
  private
    FBitCount: integer;
    FCount: integer;
    FCode: PsdHuffmanCode;
    FB0: THuffmanNode;
    FB1: THuffmanNode;
  public
    destructor Destroy; override;
    property BitCount: integer read FBitCount write FBitCount;
    property Count: integer read FCount write FCount;
    property Code: PsdHuffmanCode read FCode write FCode;
    property B0: THuffmanNode read FB0 write FB0;
    property B1: THuffmanNode read FB1 write FB1;
  end;

  THuffmanNodeList = class(TCustomSortedList)
  private
    function GetItems(Index: integer): THuffmanNode;
  protected
    function DoCompare(Item1, Item2: TObject): integer; override;
  public
    property Items[Index: integer]: THuffmanNode read GetItems; default;
  end;

  // General 8-bit huffman encoder
  T8bitHuffmanEncoder = class(THuffmanCoder)
  private
    FHistogram: T8bitHuffmanHistogram;
    FNodes: THuffmanNodeList; // list of huffman nodes (count, code and leaves)
    function GetHistogram: Psd8bitHuffmanHistogram;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure GenerateCodeTable(ATable: THuffmanTable); override;
    procedure OptimiseHuffmanFromHistogram(var Item: TDHTMarkerInfo);
    property Histogram: Psd8bitHuffmanHistogram read GetHistogram;
  end;

  // Specific Huffman DC baseline decoder
  TDCBaselineHuffmanDecoder = class(T8bitHuffmanDecoder)
  private
  public
    procedure DecodeMcuBlock(var ABlock: TMcuBlock; AReader: TBitReader);
  end;

  // Specific Huffman AC baseline decoder
  TACBaselineHuffmanDecoder = class(T8bitHuffmanDecoder)
  private
  public
    procedure DecodeMcuBlock(var ABlock: TMcuBlock; AReader: TBitReader;
      AZigZag: PsdZigZagArray);
    // Special routine for jsDiv8 scale loading, just skipping this data
    procedure DecodeMcuBlockSkip(AReader: TBitReader);
  end;

  // Specific Huffman DC baseline encoder
  TDCBaselineHuffmanEncoder = class(T8bitHuffmanEncoder)
  public
    procedure EncodeMcuBlock(var ABlock: TMcuBlock; AWriter: TBitWriter);
  end;

  // Specific Huffman AC baseline encoder
  TACBaselineHuffmanEncoder = class(T8bitHuffmanEncoder)
  public
    procedure EncodeMcuBlock(var ABlock: TMcuBlock; AWriter: TBitWriter);
  end;

  TDCProgressiveHuffmanDecoder = class(TDCBaselineHuffmanDecoder)
  public
    // Progressive
    procedure DecodeProgFirst(var ABlock: TMcuBlock; AReader: TBitReader;
      ApproxLow: integer);
    procedure DecodeProgRefine(var ABlock: TMcuBlock; AReader: TBitReader;
      ApproxLow: integer);
  end;

  TACProgressiveHuffmanDecoder = class(TACBaselineHuffmanDecoder)
  public
    // Progressive
    procedure DecodeProgFirst(var ABlock: TMcuBlock; AReader: TBitReader;
      var EOBRun: integer; SSStart, SSEnd, ApproxLow: integer);
    procedure DecodeProgRefine(var ABlock: TMcuBlock; AReader: TBitReader;
      var EOBRun: integer; SSStart, SSEnd, ApproxLow: integer);
  end;

implementation

{ THuffmanTable }

function THuffmanTable.GetCount: integer;
begin
  Result := length(FItems);
end;

function THuffmanTable.GetItems(Index: integer): PsdHuffmanCode;
begin
  Result := @FItems[Index];
end;

procedure THuffmanTable.SetCount(const Value: integer);
begin
  SetLength(FItems, Value);
end;

{ THuffmanTableList }

function THuffmanTableList.GetItems(Index: integer): THuffmanTable;
begin
  if Index >= Count then
      Count := Index + 1;
  Result := THuffmanTable(inherited Items[Index]);
  if not assigned(Result) then
    begin
      Result := THuffmanTable.Create;
      inherited Items[Index] := Result;
    end;
end;

{ TEntropyCoder }

constructor TEntropyCoder.Create;
begin
  inherited Create;
end;

{ TEntropyCoderList }

function TEntropyCoderList.GetItems(Index: integer): TEntropyCoder;
begin
  if Index >= Count then
      Count := Index + 1;
  Result := TEntropyCoder(inherited Items[Index]);
end;

procedure TEntropyCoderList.SetItems(Index: integer;
  const Value: TEntropyCoder);
begin
  if Index >= Count then
      Count := Index + 1;
  inherited Items[Index] := Value;
end;

{ THuffmanCoder }

procedure THuffmanCoder.GenerateCodeTable(ATable: THuffmanTable);
var
  i, k, Idx, Len: integer;
  Code, Size: integer;
  MaxVal: integer;
begin
  // Generate a list of codes for the table (See Fig. C.2)
  Code := 0;
  MaxVal := 0;
  Size := ATable[0]^.L;
  k := 0;
  Len := ATable.Count;
  while k < Len do
    begin
      while (k < Len) and (ATable[k]^.L = Size) do
        begin
          ATable[k]^.Code := Code;
          if ATable[k]^.V > MaxVal then
              MaxVal := ATable[k]^.V;
          inc(Code);
          inc(k);
        end;
      Code := Code shl 1;
      inc(Size);
    end;

  SetLength(FCodes, MaxVal + 1); // 0..MaxVal
  for i := 0 to ATable.Count - 1 do
    begin
      Idx := ATable[i]^.V;
      FCodes[Idx].L := ATable[i]^.L;
      FCodes[Idx].V := ATable[i]^.V;
      FCodes[Idx].Code := ATable[i]^.Code;
    end;

end;

{ THuffmanDecoder }

procedure THuffmanDecoder.AddToLookupTable(Table, Code, Len, Value: integer);
var
  i, Iter, Mask: integer;
  Base: integer;
  Next: integer;
  Lookup: PsdHuffmanLookupTable;
begin
  Lookup := @FLookup[Table];
  if Len <= 8 then
    begin
      // Fill all the lsb bit entries with the same value
      Iter := 1 shl (8 - Len);
      Base := Code shl (8 - Len);
      for i := 0 to Iter - 1 do
        begin
          Lookup^.Len[Base + i] := Len;
          Lookup^.Value[Base + i] := Value;
        end;
    end
  else
    begin
      // We need to follow a table or instantiate one
      Base := Code shr (Len - 8);
      Next := Lookup^.Value[Base];
      if Next = 0 then
        begin
          // No followup table yet, create one
          inc(FLookupCount);
          if (length(FLookup) <= FLookupCount) then
            begin
              SetLength(FLookup, length(FLookup) * 2);
            end;

          // Next table, set its pointer
          Next := FLookupCount;
          FLookup[Table].Value[Base] := Next;
        end;
      // There is a follow up table, add
      Mask := 1 shl (Len - 8) - 1;
      AddToLookupTable(Next, Code and Mask, Len - 8, Value);
    end;
end;

procedure THuffmanDecoder.GenerateLookupTables(Table: THuffmanTable);
begin
  // Generate the code table first
  GenerateCodeTable(Table);
  // Start with clean 4 lookup tables
  SetLength(FLookup, 0);
  SetLength(FLookup, 4);
  FLookupCount := 0;
  // Default does nothing more
end;

{ T8bitHuffmanDecoder }

procedure T8bitHuffmanDecoder.GenerateLookupTables(Table: THuffmanTable);
var
  i: integer;
begin
  inherited;
  for i := 0 to length(FCodes) - 1 do begin
      if FCodes[i].L > 0 then
          AddToLookupTable(0, FCodes[i].Code, FCodes[i].L, i);
    end;
end;

{ THuffmanNode }

destructor THuffmanNode.Destroy;
begin
  DisposeObject(FB0);
  FB0 := nil;
  DisposeObject(FB1);
  FB1 := nil;
  inherited Destroy;
end;

{ THuffmanNodeList }

function THuffmanNodeList.DoCompare(Item1, Item2: TObject): integer;
var
  L1, L2: THuffmanNode;
begin
  // Sort by count, smallest first
  L1 := THuffmanNode(Item1);
  L2 := THuffmanNode(Item2);

  // Compare by bitcount first (smallest bitcount first)
  Result := CompareInteger(L1.BitCount, L2.BitCount);
  if Result = 0 then
    begin
      // Compare by frequency count (largest count first)
      Result := -CompareInteger(L1.Count, L2.Count);
    end;
end;

function THuffmanNodeList.GetItems(Index: integer): THuffmanNode;
begin
  Result := THuffmanNode(inherited Items[index]);
end;

{ T8bitHuffmanEncoder }

constructor T8bitHuffmanEncoder.Create;
begin
  inherited;
  // do not own objects
  FNodes := THuffmanNodeList.Create(False);
end;

destructor T8bitHuffmanEncoder.Destroy;
begin
  DisposeObject(FNodes);
  FNodes := nil;
  inherited Destroy;
end;

procedure T8bitHuffmanEncoder.GenerateCodeTable(ATable: THuffmanTable);
var
  i: integer;
begin
  if ATable.Count = 0 then
    begin
      // Uninitialized table: create just codes for histogramming
      SetLength(FCodes, 256);
      for i := 0 to 255 do
          FCodes[i].V := i;
      exit;
    end;
  inherited;
end;

function T8bitHuffmanEncoder.GetHistogram: Psd8bitHuffmanHistogram;
begin
  Result := @FHistogram;
end;

procedure T8bitHuffmanEncoder.OptimiseHuffmanFromHistogram(var Item: TDHTMarkerInfo);
// Create an optimized huffman table from the data gathered in the histogram by
// the dry-run
var
  i: integer;
  N, N0, N1, Top: THuffmanNode;

  // Recursive procedure: add values with their bitcount to the nodelist
  procedure AddBranch(ABranch: THuffmanNode; ABitCount: integer);
  begin
    // Branch B0
    if assigned(ABranch.B0.Code) then
      begin
        ABranch.B0.BitCount := ABitCount;
        FNodes.Add(ABranch.B0);
      end
    else
        AddBranch(ABranch.B0, ABitCount + 1);

    // Branch B1
    if assigned(ABranch.B1.Code) then
      begin
        ABranch.B1.BitCount := ABitCount;
        FNodes.Add(ABranch.B1);
      end
    else
        AddBranch(ABranch.B1, ABitCount + 1);
  end;

// main
begin
  // initialise the FNodes before clearing and adding!
  if not assigned(FNodes) then
    begin
      FNodes := THuffmanNodeList.Create(False);
    end;

  // Start by adding nodes in sorted fashion
  FNodes.Clear;
  for i := 0 to length(FCodes) - 1 do
    begin
      if FHistogram[i] = 0 then
          continue;
      N := THuffmanNode.Create;
      N.Code := @FCodes[i];
      N.Count := FHistogram[i];
      FNodes.Add(N);
    end;

  // Initialize huffman data
  SetLength(Item.BitValues, FNodes.Count);
  for i := 0 to 15 do
      Item.BitLengths[i] := 0;
  if FNodes.Count = 0 then
      exit;

  // Repeat combining nodes until there's only one
  while FNodes.Count >= 2 do
    begin
      // Two last nodes with smallest frequency count
      N0 := FNodes[FNodes.Count - 1];
      N1 := FNodes[FNodes.Count - 2];

      // Delete two last from list
      FNodes.Delete(FNodes.Count - 1);
      FNodes.Delete(FNodes.Count - 1);

      // New containing node
      N := THuffmanNode.Create;
      N.B0 := N0;
      N.B1 := N1;
      N.Count := N0.Count + N1.Count;

      // Add new one to list (sorted)
      FNodes.Add(N);
    end;

  // Top item
  Top := FNodes[0];
  FNodes.Clear;

  // Start adding them again, now sorted by bitcount
  if assigned(Top.Code) then
    begin
      // If there is only one, we add it directly with bitcount 1
      Top.BitCount := 1;
      FNodes.Add(Top);
    end
  else
    begin
      // Recursive call on the tree
      AddBranch(Top, 1);
    end;

  // Since our table is compacted, and the jpeg spec says we must not have codes
  // with all ones, we will increase the bitcount of the last item
  N := FNodes[FNodes.Count - 1];
  N.BitCount := N.BitCount + 1;

  // Check maximum bit count; this should NOT exceed 16 bits for jpeg
  while FNodes[FNodes.Count - 1].BitCount > 16 do
    begin
      // Extract last two with largest bitcounts
      N0 := FNodes[FNodes.Count - 1];
      N1 := FNodes[FNodes.Count - 2];
      FNodes.Delete(FNodes.Count - 1);
      FNodes.Delete(FNodes.Count - 1);

      // Find item with at least 2 bits less
      i := FNodes.Count - 1;
      while FNodes[i].BitCount > N0.BitCount - 2 do
          dec(i);
      N := FNodes[i];
      FNodes.Delete(i);

      // Increment this leaf, decrement one of the other two, and set the other
      // to the same as this one. This preserves bitspace
      N.BitCount := N.BitCount + 1;
      N0.BitCount := N0.BitCount - 1;
      N1.BitCount := N.BitCount;

      // Add these again in a sorted way
      FNodes.Add(N);
      FNodes.Add(N0);
      FNodes.Add(N1);
    end;

  // We should now have a sorted list of codes by bitcount, and we can construct
  // the huffman table
  for i := 0 to FNodes.Count - 1 do
    begin
      N := FNodes[i];
      inc(Item.BitLengths[N.BitCount - 1]);
      Item.BitValues[i] := N.Code^.V;
    end;
  FNodes.Clear;
  Top.Free;
end;

{ TDCBaselineHuffmanDecoder }

procedure TDCBaselineHuffmanDecoder.DecodeMcuBlock(var ABlock: TMcuBlock; AReader: TBitReader);
var
  S, Code: smallint; // S = category
  Bits: word;
  Idx, Len: byte;
  Table: PsdHuffmanLookupTable;
begin
  // Get the S code. Since its guaranteed to have <= 16 bits we can use
  // this two-step mechanism (without loop)
  Idx := AReader.ThisByte^;
  Table := @FLookup[0];
  Len := Table^.Len[Idx];
  S := Table^.Value[Idx];
  if Len = 0 then
    begin
      Idx := AReader.NextByte^;
      Table := @FLookup[S];
      Len := 8 + Table^.Len[Idx];
      S := Table^.Value[Idx];
    end;

  // We already have the code, but need to actually remove the bits from the stream
  AReader.RemoveBits(Len);
  inc(FCountCodes, Len);

  // Process the S code, it's an index into a category. We find "Code", and correct
  // it with the Pred value (undifferencing)
  inc(FCountBits, S);
  case S of
    0: Code := ABlock.PPred^;
    1:
      begin
        if AReader.GetBits(1) = 1 then
            Code := ABlock.PPred^ + 1
        else
            Code := ABlock.PPred^ - 1;
      end;
    else
      // We use the EXTEND function, Figure F12
      Bits := AReader.GetBits(S);
      if Bits < cExtendTest[S] then
          Code := ABlock.PPred^ + Bits + cExtendOffset[S]
      else
          Code := ABlock.PPred^ + Bits;
  end; // case

  // Update block
  ABlock.Values^[0] := Code;

  // Update image component's predictor
  ABlock.PPred^ := Code;
end;

{ TACBaselineHuffmanDecoder }

procedure TACBaselineHuffmanDecoder.DecodeMcuBlock(var ABlock: TMcuBlock;
  AReader: TBitReader; AZigZag: PsdZigZagArray);
var
  k, kz: integer; // Position in zigzag
  Values: PsdCoefBlock;
  RS, R, S: integer; // RS = range,category
  Bits, Idx, Len: integer;
  Table1, Table2: PsdHuffmanLookupTable;
  ThisByte: PByte;
begin
  // Prepare some local variables for fast access
  Table1 := @FLookup[0];
  ThisByte := AReader.ThisByte;
  Values := ABlock.Values;

  // DC did k = 0, now we're at k = 1
  k := 1;
  repeat
    // Get the RS code. Since its guaranteed to have <= 16 bits we can use
    // this two-step mechanism (without loop)
    Idx := ThisByte^;
    Len := Table1^.Len[Idx];
    RS := Table1^.Value[Idx];
    if Len = 0 then
      begin
        Idx := AReader.NextByte^;
        Table2 := @FLookup[RS];
        Len := 8 + Table2^.Len[Idx];
        RS := Table2^.Value[Idx];
      end;

    // We already have the code, but need to actually remove the bits from the stream
    AReader.RemoveBits(Len);
    inc(FCountCodes, Len);
    // Split range,category
    R := RS shr 4;
    S := RS and $0F;

    if S = 0 then
      begin
        if R = 15 then
          begin
            // 16 sample runlength, no sample setting
            inc(k, 16);
            continue;
          end
        else
          begin
            // All other values except R = 0 are undefined, we take it as to
            // jump out for these too. R=0,S=0 means end of block
            break;
          end;
      end;

    // Increment range-coded index
    inc(k, R);

    // Process the S code, it's an index into a category.
    // We use the EXTEND function, Figure F12
    Bits := AReader.GetBits(S);
    inc(FCountBits, S);
    kz := AZigZag^[k];
    if kz > 0 then
      begin
        if S = 1 then
          begin
            // Optimized for S = 1 (very often)
            if Bits = 0 then
                Values^[kz] := -1
            else
                Values^[kz] := 1;
          end
        else
          begin
            // S > 1
            if Bits < cExtendTest[S] then
                Values^[kz] := Bits + cExtendOffset[S]
            else
                Values^[kz] := Bits;
          end;
      end;
    inc(k);

    // Check if we're at the end of the 8x8 zigzagging
  until k > 63;
end;

procedure TACBaselineHuffmanDecoder.DecodeMcuBlockSkip(AReader: TBitReader);
var
  k: integer;        // Position in zigzag
  RS, R, S: integer; // RS = range,category
  Idx, Len: integer;
  Table1, Table2: PsdHuffmanLookupTable;
  ThisByte: PByte;
begin
  // Prepare some local variables for fast access
  Table1 := @FLookup[0];
  ThisByte := AReader.ThisByte;

  // DC did k = 0, now we're at k = 1
  k := 1;
  repeat
    // Get the RS code. Since its guaranteed to have <= 16 bits we can use
    // this two-step mechanism (without loop)
    Idx := ThisByte^;
    Len := Table1^.Len[Idx];
    RS := Table1^.Value[Idx];
    if Len = 0 then
      begin
        Idx := AReader.NextByte^;
        Table2 := @FLookup[RS];
        Len := 8 + Table2^.Len[Idx];
        RS := Table2^.Value[Idx];
      end;

    // We already have the code, but need to actually remove the bits from the stream
    AReader.RemoveBits(Len);

    // Split range,category
    R := RS shr 4;
    S := RS and $0F;

    if S = 0 then
      begin
        if R = 15 then
          begin
            // 16 sample runlength, no sample setting
            inc(k, 16);
            continue;
          end
        else
          begin
            // All other values except R = 0 are undefined, we take it as to
            // jump out for these too. R=0,S=0 means end of block
            break;
          end;
      end;

    // Increment range-coded index
    inc(k, R + 1);

    // Process the S code, it's an index into a category.
    // We use the EXTEND function, Figure F12
    AReader.GetBits(S);

    // Check if we're at the end of the 8x8 zigzagging
  until k > 63;
end;

{ TDCBaselineHuffmanEncoder }

procedure TDCBaselineHuffmanEncoder.EncodeMcuBlock(var ABlock: TMcuBlock;
  AWriter: TBitWriter);
var
  S, Diff: smallint; // S = category
begin
  Diff := ABlock.Values^[0] - ABlock.PPred^;
  ABlock.PPred^ := ABlock.Values^[0];

  // count the bits
  S := AWriter.CountBits(Diff);

  // Put S code  + extend
  AWriter.PutCodeExtend(@FCodes[S], Diff, S);
end;

{ TACBaselineHuffmanEncoder }

procedure TACBaselineHuffmanEncoder.EncodeMcuBlock(var ABlock: TMcuBlock;
  AWriter: TBitWriter);
var
  k: integer; // Position in zigzag
  Values: PsdCoefBlock;
  RS, R, S, Diff: integer; // RS = range,category
begin
  Values := ABlock.Values;
  R := 0;
  k := 1;
  repeat
    Diff := Values^[cJpegInverseZigZag8x8[k]];
    inc(k);
    if Diff = 0 then
      begin
        inc(R);
        continue;
      end;
    while R >= 16 do
      begin
        // Code an RS = $F0
        AWriter.PutCode(@FCodes[$F0]);
        dec(R, 16);
      end;
    // Code the value
    S := AWriter.CountBits(Diff);
    // RS value
    RS := R shl 4 + S;
    R := 0;
    AWriter.PutCodeExtend(@FCodes[RS], Diff, S);
  until k = 64;

  // if we have R > 0 this means we must code end of block
  if R > 0 then
      AWriter.PutCode(@FCodes[$00]);
end;

{ TDCProgressiveHuffmanDecoder }

procedure TDCProgressiveHuffmanDecoder.DecodeProgFirst(var ABlock: TMcuBlock; AReader: TBitReader; ApproxLow: integer);
var
  S, Code: smallint; // S = category
  Bits: word;
  Idx, Len: byte;
  Table: PsdHuffmanLookupTable;
begin
  // DoDebugOut(Self, wsInfo, 'DC DecodeProgFirst');

  // Get the S code. Since its guaranteed to have <= 16 bits we can use
  // this two-step mechanism (without loop)
  Idx := AReader.ThisByte^;
  Table := @FLookup[0];
  Len := Table^.Len[Idx];
  S := Table^.Value[Idx];
  if Len = 0 then
    begin
      Idx := AReader.NextByte^;
      Table := @FLookup[S];
      Len := 8 + Table^.Len[Idx];
      S := Table^.Value[Idx];
    end;

  // We already have the code, but need to actually remove the bits from the stream
  AReader.RemoveBits(Len);

  // Process the S code, it's an index into a category. We find "Code", and correct
  // it with the Pred value (undifferencing)
  Code := 0;
  if S > 0 then
    begin
      // We use the EXTEND function, Figure F12
      Bits := AReader.GetBits(S);
      if Bits < cExtendTest[S] then
          Code := Bits + cExtendOffset[S]
      else
          Code := Bits;
    end;
  inc(Code, ABlock.PPred^);

  // Update image component's predictor
  ABlock.PPred^ := Code;

  // Update block
  ABlock.Values^[0] := Code shl ApproxLow;
end;

procedure TDCProgressiveHuffmanDecoder.DecodeProgRefine(var ABlock: TMcuBlock;
  AReader: TBitReader; ApproxLow: integer);
var
  Plus: integer;
  Value: Psmallint;
begin
  // DoDebugOut(Self, wsInfo, 'DC DecodeProgRefine');
  Plus := 1 shl ApproxLow;
  Value := @ABlock.Values^[0];

  // Update block
  if AReader.GetBits(1) = 1 then
    begin
      if Value^ > 0 then
          inc(Value^, Plus)
      else
          dec(Value^, Plus);
    end;
end;

{ TACProgressiveHuffmanDecoder }

procedure TACProgressiveHuffmanDecoder.DecodeProgFirst(var ABlock: TMcuBlock;
  AReader: TBitReader; var EOBRun: integer; SSStart, SSEnd, ApproxLow: integer);
var
  k, kz: integer; // Position in zigzag
  Values: PsdCoefBlock;
  RS, R, S: integer; // RS = range,category
  Idx, Len: integer;
  Table1, Table2: PsdHuffmanLookupTable;
  ThisByte, NextByte: PByte;
begin
  // DoDebugOut(Self, wsInfo, 'AC DecodeProgFirst');

  // Part of EOB run? In that case, decrement and exit
  if EOBRun > 0 then
    begin
      dec(EOBRun);
      exit;
    end;

  // Prepare some local variables for fast access
  Table1 := @FLookup[0];
  ThisByte := AReader.ThisByte;
  NextByte := AReader.NextByte;
  Values := ABlock.Values;

  // Start of the spectral band
  k := SSStart;

  // Check if we're at the end of the spectral band
  while k <= SSEnd do
    begin

      // Get the RS code. Since its guaranteed to have <= 16 bits we can use
      // this two-step mechanism (without loop)
      Idx := ThisByte^;
      Len := Table1^.Len[Idx];
      RS := Table1^.Value[Idx];
      if Len = 0 then
        begin
          Idx := NextByte^;
          Table2 := @FLookup[RS];
          Len := 8 + Table2^.Len[Idx];
          RS := Table2^.Value[Idx];
        end;

      // We already have the code, but need to actually remove the bits from the stream
      AReader.RemoveBits(Len);

      // Split range,category
      R := RS shr 4;
      S := RS and $0F;

      if S <> 0 then
        begin

          // Increment range-coded index
          inc(k, R);

          // Process the S code, it's an index into a category.
          // We use the EXTEND function, Figure F12
          R := AReader.GetBits(S);
          if R < cExtendTest[S] then
              S := R + cExtendOffset[S]
          else
              S := R;

          kz := cJpegInverseZigZag8x8[k];
          if kz > 0 then
              Values^[kz] := S shl ApproxLow;
        end
      else
        begin

          if R = 15 then
            begin

              // 16 sample runlength, no sample setting
              inc(k, 15);

            end
          else
            begin

              // EOB run
              EOBRun := 1 shl R;
              if R > 0 then
                begin
                  R := AReader.GetBits(R);
                  inc(EOBRun, R);
                end;
              dec(EOBRun);
              break;

            end;
        end;
      inc(k);
    end;
end;

procedure TACProgressiveHuffmanDecoder.DecodeProgRefine(var ABlock: TMcuBlock;
  AReader: TBitReader; var EOBRun: integer; SSStart, SSEnd, ApproxLow: integer);
var
  k, kz: integer;
  Values: PsdCoefBlock;
  RS, R, S, Plus: integer; // RS = range,category
  Idx, Len: integer;
  Table1, Table2: PsdHuffmanLookupTable;
  ThisByte, NextByte: PByte;
begin
  // DoDebugOut(Self, wsInfo, 'AC DecodeProgRefine');

  // Prepare some local variables for fast access
  Plus := 1 shl ApproxLow;
  Table1 := @FLookup[0];
  ThisByte := AReader.ThisByte;
  NextByte := AReader.NextByte;
  Values := ABlock.Values;

  // Start of the spectral band
  k := SSStart;

  // Not part of EOB run?
  if EOBRun = 0 then
    begin

      while k <= SSEnd do
        begin
          // Get the RS code. Since its guaranteed to have <= 16 bits we can use
          // this two-step mechanism (without loop)
          Idx := ThisByte^;
          Len := Table1^.Len[Idx];
          RS := Table1^.Value[Idx];
          if Len = 0 then
            begin
              Idx := NextByte^;
              Table2 := @FLookup[RS];
              Len := 8 + Table2^.Len[Idx];
              RS := Table2^.Value[Idx];
            end;

          // We already have the code, but need to actually remove the bits from the stream
          AReader.RemoveBits(Len);

          // Split range,category
          R := RS shr 4;
          S := RS and $0F;

          if (S = 0) and (R < 15) then
            begin
              // EOB run
              EOBRun := 1 shl R;
              if R <> 0 then
                begin
                  R := AReader.GetBits(R);
                  inc(EOBRun, R);
                end;
              break;
            end;

          if S <> 0 then
            begin
              case AReader.GetBits(1) of
                1: S := Plus;
                0: S := -Plus;
              end;
            end;

          // Fill values for remainder
          repeat
            kz := cJpegInverseZigZag8x8[k];
            if Values^[kz] <> 0 then
              begin
                if AReader.GetBits(1) = 1 then
                  begin
                    if Values^[kz] > 0 then
                        inc(Values^[kz], Plus)
                    else
                        dec(Values^[kz], Plus);
                  end;
              end
            else
              begin
                dec(R);
                if R < 0 then
                    break;
              end;
            inc(k);
          until k > SSEnd;

          if k <= SSEnd then
            begin
              if S <> 0 then
                begin
                  kz := cJpegInverseZigZag8x8[k];
                  if kz > 0 then
                      Values^[kz] := S;
                end;
            end;

          // Increment range-coded index
          inc(k);

        end; // while
    end;     // EOBRun = 0

  // Deal with EOBRun
  if EOBRun > 0 then
    begin

      while k <= SSEnd do
        begin
          kz := cJpegInverseZigZag8x8[k];
          if Values^[kz] <> 0 then
            begin
              if AReader.GetBits(1) = 1 then
                begin
                  if Values^[kz] > 0 then
                      inc(Values^[kz], Plus)
                  else
                      dec(Values^[kz], Plus);
                end;
            end;
          inc(k);
        end;

      // decrement the EOB run
      dec(EOBRun);
    end;
end;

end.
