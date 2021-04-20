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
unit Raster_JPEG_Huffman;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, MemoryStream64, Raster_JPEG_type, Raster_JPEG_BitStream;

type
  TEntropyCoder = class(TJPEG_Persistent)
  public
    constructor Create; virtual;
  end;

  TEntropyCoderList = class(TCoreClassObjectList)
  private
    function GetItems(Index: Integer): TEntropyCoder;
    procedure SetItems(Index: Integer; const Value: TEntropyCoder);
  public
    property Items[Index: Integer]: TEntropyCoder read GetItems write SetItems; default;
  end;

  // Generic Huffman coder implementing shared methods
  THuffmanCoder = class(TEntropyCoder)
  protected
    FCodes: array of THuffmanCode;
  public
    procedure GenerateCodeTable(Table_: THuffmanTable); virtual;
  end;

  // Generic Huffman decoder
  THuffmanDecoder = class(THuffmanCoder)
  protected
    FLookup: array of THuffmanLookupTable;
    FLookupCount: Word;
  public
    FCountCodes: Integer;
    FCountBits: Integer;
    procedure AddToLookupTable(Table, Code, Len, Value: Integer);
    procedure GenerateLookupTables(Table: THuffmanTable); virtual;
  end;

  // General 8-bit huffman decoder
  T8bitHuffmanDecoder = class(THuffmanDecoder)
  public
    procedure GenerateLookupTables(Table: THuffmanTable); override;
  end;

  THuffmanNode = class
  private
    FBitCount: Integer;
    FCount: Integer;
    FCode: PHuffmanCode;
    FB0: THuffmanNode;
    FB1: THuffmanNode;
  public
    destructor Destroy; override;
    property BitCount: Integer read FBitCount write FBitCount;
    property Count: Integer read FCount write FCount;
    property Code: PHuffmanCode read FCode write FCode;
    property B0: THuffmanNode read FB0 write FB0;
    property B1: THuffmanNode read FB1 write FB1;
  end;

  THuffmanNodeList = class(TCustomSortedList)
  private
    function GetItems(Index: Integer): THuffmanNode;
  protected
    function DoCompare(Item1, Item2: TObject): Integer; override;
  public
    property Items[Index: Integer]: THuffmanNode read GetItems; default;
  end;

  // General 8-bit huffman encoder
  T8bitHuffmanEncoder = class(THuffmanCoder)
  private
    FHistogram: T8bitHuffmanHistogram;
    FNodes: THuffmanNodeList; // list of huffman nodes (count, code and leaves)
    function GetHistogram: P8bitHuffmanHistogram;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure GenerateCodeTable(Table_: THuffmanTable); override;
    procedure OptimiseHuffmanFromHistogram(var Item: TDHTMarkerInfo);
    property Histogram: P8bitHuffmanHistogram read GetHistogram;
  end;

  // Specific Huffman DC baseline decoder
  TDCBaselineHuffmanDecoder = class(T8bitHuffmanDecoder)
  public
    procedure DecodeMcuBlock(var Block_: TMCUBlock; Reader_: TBitReader);
  end;

  // Specific Huffman AC baseline decoder
  TACBaselineHuffmanDecoder = class(T8bitHuffmanDecoder)
  public
    procedure DecodeMcuBlock(var Block_: TMCUBlock; Reader_: TBitReader; AZigZag: PZigZagArray);
    // Special routine for jsDiv8 scale loading, just skipping this data
    procedure DecodeMcuBlockSkip(Reader_: TBitReader);
  end;

  // Specific Huffman DC baseline encoder
  TDCBaselineHuffmanEncoder = class(T8bitHuffmanEncoder)
  public
    procedure EncodeMcuBlock(var Block_: TMCUBlock; Writer_: TBitWriter);
  end;

  // Specific Huffman AC baseline encoder
  TACBaselineHuffmanEncoder = class(T8bitHuffmanEncoder)
  public
    procedure EncodeMcuBlock(var Block_: TMCUBlock; Writer_: TBitWriter);
  end;

  TDCProgressiveHuffmanDecoder = class(TDCBaselineHuffmanDecoder)
  public
    // Progressive
    procedure DecodeProgFirst(var Block_: TMCUBlock; Reader_: TBitReader; ApproxLow: Integer);
    procedure DecodeProgRefine(var Block_: TMCUBlock; Reader_: TBitReader; ApproxLow: Integer);
  end;

  TACProgressiveHuffmanDecoder = class(TACBaselineHuffmanDecoder)
  public
    // Progressive
    procedure DecodeProgFirst(var Block_: TMCUBlock; Reader_: TBitReader; var EOBRun: Integer; SSStart, SSEnd, ApproxLow: Integer);
    procedure DecodeProgRefine(var Block_: TMCUBlock; Reader_: TBitReader; var EOBRun: Integer; SSStart, SSEnd, ApproxLow: Integer);
  end;

implementation

constructor TEntropyCoder.Create;
begin
  inherited Create;
end;

function TEntropyCoderList.GetItems(Index: Integer): TEntropyCoder;
begin
  if Index >= Count then
      Count := Index + 1;
  Result := TEntropyCoder(inherited Items[Index]);
end;

procedure TEntropyCoderList.SetItems(Index: Integer;
  const Value: TEntropyCoder);
begin
  if Index >= Count then
      Count := Index + 1;
  inherited Items[Index] := Value;
end;

procedure THuffmanCoder.GenerateCodeTable(Table_: THuffmanTable);
var
  i, k, Idx, Len: Integer;
  Code, Size: Integer;
  MaxVal: Integer;
begin
  // Generate a list of codes for the table (See Fig. C.2)
  Code := 0;
  MaxVal := 0;
  Size := Table_[0]^.L;
  k := 0;
  Len := Table_.Count;
  while k < Len do
    begin
      while (k < Len) and (Table_[k]^.L = Size) do
        begin
          Table_[k]^.Code := Code;
          if Table_[k]^.V > MaxVal then
              MaxVal := Table_[k]^.V;
          inc(Code);
          inc(k);
        end;
      Code := Code shl 1;
      inc(Size);
    end;

  SetLength(FCodes, MaxVal + 1); // 0..MaxVal
  for i := 0 to Table_.Count - 1 do
    begin
      Idx := Table_[i]^.V;
      FCodes[Idx].L := Table_[i]^.L;
      FCodes[Idx].V := Table_[i]^.V;
      FCodes[Idx].Code := Table_[i]^.Code;
    end;
end;

procedure THuffmanDecoder.AddToLookupTable(Table, Code, Len, Value: Integer);
var
  i, Iter, Mask: Integer;
  Base: Integer;
  Next: Integer;
  Lookup: PHuffmanLookupTable;
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

procedure T8bitHuffmanDecoder.GenerateLookupTables(Table: THuffmanTable);
var
  i: Integer;
begin
  inherited;
  for i := 0 to length(FCodes) - 1 do begin
      if FCodes[i].L > 0 then
          AddToLookupTable(0, FCodes[i].Code, FCodes[i].L, i);
    end;
end;

destructor THuffmanNode.Destroy;
begin
  DisposeObject(FB0);
  FB0 := nil;
  DisposeObject(FB1);
  FB1 := nil;
  inherited Destroy;
end;

function THuffmanNodeList.DoCompare(Item1, Item2: TObject): Integer;
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

function THuffmanNodeList.GetItems(Index: Integer): THuffmanNode;
begin
  Result := THuffmanNode(inherited Items[index]);
end;

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

procedure T8bitHuffmanEncoder.GenerateCodeTable(Table_: THuffmanTable);
var
  i: Integer;
begin
  if Table_.Count = 0 then
    begin
      // Uninitialized table: create just codes for histogramming
      SetLength(FCodes, 256);
      for i := 0 to 255 do
          FCodes[i].V := i;
      exit;
    end;
  inherited;
end;

function T8bitHuffmanEncoder.GetHistogram: P8bitHuffmanHistogram;
begin
  Result := @FHistogram;
end;

procedure T8bitHuffmanEncoder.OptimiseHuffmanFromHistogram(var Item: TDHTMarkerInfo);
// Create an optimized huffman table from the data gathered in the histogram by the dry-run
var
  i: Integer;
  N, N0, N1, Top: THuffmanNode;

  // Recursive procedure: add values with their bitcount to the nodelist
  procedure AddBranch(Branch_: THuffmanNode; BitCount_: Integer);
  begin
    // Branch B0
    if assigned(Branch_.B0.Code) then
      begin
        Branch_.B0.BitCount := BitCount_;
        FNodes.Add(Branch_.B0);
      end
    else
        AddBranch(Branch_.B0, BitCount_ + 1);

    // Branch B1
    if assigned(Branch_.B1.Code) then
      begin
        Branch_.B1.BitCount := BitCount_;
        FNodes.Add(Branch_.B1);
      end
    else
        AddBranch(Branch_.B1, BitCount_ + 1);
  end;

begin
  // initialise the FNodes before clearing and adding!
  if not assigned(FNodes) then
      FNodes := THuffmanNodeList.Create(False);

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

procedure TDCBaselineHuffmanDecoder.DecodeMcuBlock(var Block_: TMCUBlock; Reader_: TBitReader);
var
  S, Code: SmallInt; // S = category
  Bits: Word;
  Idx, Len: Byte;
  Table: PHuffmanLookupTable;
begin
  // Get the S code. Since its guaranteed to have <= 16 bits we can use
  // this two-step mechanism (without loop)
  Idx := Reader_.ThisByte^;
  Table := @FLookup[0];
  Len := Table^.Len[Idx];
  S := Table^.Value[Idx];
  if Len = 0 then
    begin
      Idx := Reader_.NextByte^;
      Table := @FLookup[S];
      Len := 8 + Table^.Len[Idx];
      S := Table^.Value[Idx];
    end;

  // We already have the code, but need to actually remove the bits from the stream
  Reader_.RemoveBits(Len);
  inc(FCountCodes, Len);

  // Process the S code, it's an index into a category. We find "Code", and correct
  // it with the Pred value (undifferencing)
  inc(FCountBits, S);
  case S of
    0: Code := Block_.PPred^;
    1:
      begin
        if Reader_.GetBits(1) = 1 then
            Code := Block_.PPred^ + 1
        else
            Code := Block_.PPred^ - 1;
      end;
    else
      // We use the EXTEND function, Figure F12
      Bits := Reader_.GetBits(S);
      if Bits < cExtendTest[S] then
          Code := Block_.PPred^ + Bits + cExtendOffset[S]
      else
          Code := Block_.PPred^ + Bits;
  end; // case

  // Update block
  Block_.Values^[0] := Code;

  // Update image component's predictor
  Block_.PPred^ := Code;
end;

procedure TACBaselineHuffmanDecoder.DecodeMcuBlock(var Block_: TMCUBlock; Reader_: TBitReader; AZigZag: PZigZagArray);
var
  k, kz: Integer; // Position in zigzag
  Values: PCoefBlock;
  RS, R, S: Integer; // RS = range,category
  Bits, Idx, Len: Integer;
  Table1, Table2: PHuffmanLookupTable;
  ThisByte: PByte;
begin
  // Prepare some local variables for fast access
  Table1 := @FLookup[0];
  ThisByte := Reader_.ThisByte;
  Values := Block_.Values;

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
        Idx := Reader_.NextByte^;
        Table2 := @FLookup[RS];
        Len := 8 + Table2^.Len[Idx];
        RS := Table2^.Value[Idx];
      end;

    // We already have the code, but need to actually remove the bits from the stream
    Reader_.RemoveBits(Len);
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
    Bits := Reader_.GetBits(S);
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

procedure TACBaselineHuffmanDecoder.DecodeMcuBlockSkip(Reader_: TBitReader);
var
  k: Integer;        // Position in zigzag
  RS, R, S: Integer; // RS = range,category
  Idx, Len: Integer;
  Table1, Table2: PHuffmanLookupTable;
  ThisByte: PByte;
begin
  // Prepare some local variables for fast access
  Table1 := @FLookup[0];
  ThisByte := Reader_.ThisByte;

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
        Idx := Reader_.NextByte^;
        Table2 := @FLookup[RS];
        Len := 8 + Table2^.Len[Idx];
        RS := Table2^.Value[Idx];
      end;

    // We already have the code, but need to actually remove the bits from the stream
    Reader_.RemoveBits(Len);

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
    Reader_.GetBits(S);

    // Check if we're at the end of the 8x8 zigzagging
  until k > 63;
end;

procedure TDCBaselineHuffmanEncoder.EncodeMcuBlock(var Block_: TMCUBlock; Writer_: TBitWriter);
var
  S, Diff: SmallInt; // S = category
begin
  Diff := Block_.Values^[0] - Block_.PPred^;
  Block_.PPred^ := Block_.Values^[0];

  // count the bits
  S := Writer_.CountBits(Diff);

  // Put S code  + extend
  Writer_.PutCodeExtend(@FCodes[S], Diff, S);
end;

procedure TACBaselineHuffmanEncoder.EncodeMcuBlock(var Block_: TMCUBlock; Writer_: TBitWriter);
var
  k: Integer; // Position in zigzag
  Values: PCoefBlock;
  RS, R, S, Diff: Integer; // RS = range,category
begin
  Values := Block_.Values;
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
        Writer_.PutCode(@FCodes[$F0]);
        dec(R, 16);
      end;
    // Code the value
    S := Writer_.CountBits(Diff);
    // RS value
    RS := R shl 4 + S;
    R := 0;
    Writer_.PutCodeExtend(@FCodes[RS], Diff, S);
  until k = 64;

  // if we have R > 0 this means we must code end of block
  if R > 0 then
      Writer_.PutCode(@FCodes[$00]);
end;

procedure TDCProgressiveHuffmanDecoder.DecodeProgFirst(var Block_: TMCUBlock; Reader_: TBitReader; ApproxLow: Integer);
var
  S, Code: SmallInt; // S = category
  Bits: Word;
  Idx, Len: Byte;
  Table: PHuffmanLookupTable;
begin
  // DoDebugOut(Self, wsInfo, 'DC DecodeProgFirst');

  // Get the S code. Since its guaranteed to have <= 16 bits we can use
  // this two-step mechanism (without loop)
  Idx := Reader_.ThisByte^;
  Table := @FLookup[0];
  Len := Table^.Len[Idx];
  S := Table^.Value[Idx];
  if Len = 0 then
    begin
      Idx := Reader_.NextByte^;
      Table := @FLookup[S];
      Len := 8 + Table^.Len[Idx];
      S := Table^.Value[Idx];
    end;

  // We already have the code, but need to actually remove the bits from the stream
  Reader_.RemoveBits(Len);

  // Process the S code, it's an index into a category. We find "Code", and correct
  // it with the Pred value (undifferencing)
  Code := 0;
  if S > 0 then
    begin
      // We use the EXTEND function, Figure F12
      Bits := Reader_.GetBits(S);
      if Bits < cExtendTest[S] then
          Code := Bits + cExtendOffset[S]
      else
          Code := Bits;
    end;
  inc(Code, Block_.PPred^);

  // Update image component's predictor
  Block_.PPred^ := Code;

  // Update block
  Block_.Values^[0] := Code shl ApproxLow;
end;

procedure TDCProgressiveHuffmanDecoder.DecodeProgRefine(var Block_: TMCUBlock; Reader_: TBitReader; ApproxLow: Integer);
var
  Plus: Integer;
  Value: Psmallint;
begin
  // DoDebugOut(Self, wsInfo, 'DC DecodeProgRefine');
  Plus := 1 shl ApproxLow;
  Value := @Block_.Values^[0];

  // Update block
  if Reader_.GetBits(1) = 1 then
    begin
      if Value^ > 0 then
          inc(Value^, Plus)
      else
          dec(Value^, Plus);
    end;
end;

procedure TACProgressiveHuffmanDecoder.DecodeProgFirst(var Block_: TMCUBlock; Reader_: TBitReader; var EOBRun: Integer; SSStart, SSEnd, ApproxLow: Integer);
var
  k, kz: Integer; // Position in zigzag
  Values: PCoefBlock;
  RS, R, S: Integer; // RS = range,category
  Idx, Len: Integer;
  Table1, Table2: PHuffmanLookupTable;
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
  ThisByte := Reader_.ThisByte;
  NextByte := Reader_.NextByte;
  Values := Block_.Values;

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
      Reader_.RemoveBits(Len);

      // Split range,category
      R := RS shr 4;
      S := RS and $0F;

      if S <> 0 then
        begin

          // Increment range-coded index
          inc(k, R);

          // Process the S code, it's an index into a category.
          // We use the EXTEND function, Figure F12
          R := Reader_.GetBits(S);
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
                  R := Reader_.GetBits(R);
                  inc(EOBRun, R);
                end;
              dec(EOBRun);
              break;
            end;
        end;
      inc(k);
    end;
end;

procedure TACProgressiveHuffmanDecoder.DecodeProgRefine(var Block_: TMCUBlock; Reader_: TBitReader; var EOBRun: Integer; SSStart, SSEnd, ApproxLow: Integer);
var
  k, kz: Integer;
  Values: PCoefBlock;
  RS, R, S, Plus: Integer; // RS = range,category
  Idx, Len: Integer;
  Table1, Table2: PHuffmanLookupTable;
  ThisByte, NextByte: PByte;
begin
  // DoDebugOut(Self, wsInfo, 'AC DecodeProgRefine');

  // Prepare some local variables for fast access
  Plus := 1 shl ApproxLow;
  Table1 := @FLookup[0];
  ThisByte := Reader_.ThisByte;
  NextByte := Reader_.NextByte;
  Values := Block_.Values;

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
          Reader_.RemoveBits(Len);

          // Split range,category
          R := RS shr 4;
          S := RS and $0F;

          if (S = 0) and (R < 15) then
            begin
              // EOB run
              EOBRun := 1 shl R;
              if R <> 0 then
                begin
                  R := Reader_.GetBits(R);
                  inc(EOBRun, R);
                end;
              break;
            end;

          if S <> 0 then
            begin
              case Reader_.GetBits(1) of
                1: S := Plus;
                0: S := -Plus;
              end;
            end;

          // Fill values for remainder
          repeat
            kz := cJpegInverseZigZag8x8[k];
            if Values^[kz] <> 0 then
              begin
                if Reader_.GetBits(1) = 1 then
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
              if Reader_.GetBits(1) = 1 then
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
