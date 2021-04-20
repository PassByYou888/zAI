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
unit Raster_JPEG_type;

{$INCLUDE zDefine.inc}

interface

uses
  SysUtils,
  CoreClasses,
  PascalStrings,
  MemoryStream64,
  UnicodeMixedLib,
  DoStatusIO;

type
  TWarnStyle = (wsInfo, wsHint, wsWarn, wsFail);

  // event with debug data
  TDebugEvent = procedure(Sender: TObject; WarnStyle: TWarnStyle; const Message_: TPascalString) of object;

  // simple update event
  TUpdateEvent = procedure(Sender: TObject) of object;

  TJPEG_Base_Object = class(TCoreClassObject)
  protected
    FOwner: TJPEG_Base_Object;
{$IFDEF JPEG_Debug}
    FOnDebugOut: TDebugEvent;
{$ENDIF JPEG_Debug}
  public
    constructor Create(Owner_: TJPEG_Base_Object); virtual;
{$IFDEF JPEG_Debug}
    procedure DoDebugOut(Sender: TObject; WarnStyle: TWarnStyle; const Message_: TPascalString); virtual;
    property OnDebugOut: TDebugEvent read FOnDebugOut write FOnDebugOut;
{$ENDIF JPEG_Debug}
    property Owner: TJPEG_Base_Object read FOwner;
  end;

  TJPEG_Persistent = class(TCoreClassObject)
  protected
    FOwner: TJPEG_Base_Object;
{$IFDEF JPEG_Debug}
    procedure DoDebugOut(Sender: TCoreClassObject; WarnStyle: TWarnStyle; const Message_: TPascalString); virtual;
{$ENDIF JPEG_Debug}
  public
    constructor Create;
    constructor CreateDebug(Owner_: TJPEG_Base_Object); virtual;
  end;

  // allow external CMS to manage colors in the bitmap (client code needs to determine whether the
  // Map_ is actually a TBitmap in Windows, in case of NativeJpg.pas)
  TJpegExternalCMSEvent = procedure(Sender: TCoreClassObject; var Map_: TCoreClassObject) of object;

  TJpegScale = (
    jsFull, // Read the complete image (DC + AC 1..63)
    jsDiv2, // Read only 1/2 of the image (DC + AC 1..15)
    jsDiv4, // Read only 1/4 of the image (DC + AC 1..3)
    jsDiv8  // Read only 1/8 of the image (DC only)
    );

  TJpegColorSpace = (
    jcAutoDetect, // Auto-detect the colorspace from the file
    jcGray,       // 1-Channel grayscale
    jcGrayA,      // 1-Channel grayscale with Alpha channel
    jcRGB,        // (standard) RGB
    jcRGBA,       // (standard) RGB with Alpha channel
    jcYCbCr,      // Jpeg Y-Cb-Cr
    jcYCbCrA,     // Jpeg Y-Cb-Cr with Alpha channel
    jcCMYK,       // CMYK
    jcYCbCrK,     // CMYK represented in 4 channels as YCbCrK
    jcYCCK,       // YCCK
    jcPhotoYCC,   // Photo YCC
    jcPhotoYCCA,  // Photo YCCA
    jcITUCieLAB   // ITU G3FAX CieLAB (for use in colour faxes)
    );

  TJpegDCTCodingMethod = (
    dmFast,
    dmAccurate
    );

  // Supported encoding methods in this implementation
  TJpegEncodingMethod = (
    emUnspecified,
    emBaselineDCT,
    emExtendedDCT,
    emProgressiveDCT
    );

  TQuantizationPrecision = (
    qp8bit,
    qp16bit
    );

  TJpegQuality = 1 .. 100;

  TCoefBlock = array [0 .. 63] of SmallInt;
  PCoefBlock = ^TCoefBlock;
  TSampleBlock = array [0 .. 63] of Byte;
  PSampleBlock = ^TSampleBlock;

  TZigZagArray = array [0 .. 63 + 16] of Byte;
  PZigZagArray = ^TZigZagArray;
  TIntArray64 = array [0 .. 63] of Integer;

  // Minimum Coded Unit block (MCU)
  TMCUBlock = record
    Values: PCoefBlock;
    PPred: PSmallint;
    DCTable: Integer;
    ACTable: Integer;
    BlockIdx: Integer;
    MapIdx: Integer;
  end;

  PMCUBlock = ^TMCUBlock;

  // Huffman code
  THuffmanCode = record
    L: Integer;    // Symbol length
    Code: Integer; // Associated huffman code
    V: Integer;    // Value for huffman code
  end;

  PHuffmanCode = ^THuffmanCode;

  TDHTMarkerInfo = record
    BitLengths: array [0 .. 15] of Byte;
    BitValues: array of Byte;
    Tc, Th: Byte;
  end;

  PDHTMarkerInfo = ^TDHTMarkerInfo;

  // Lookup table for Huffman decoding. The Huffman code is left-aligned
  // in the table, Len indicates the number of bits to take out of the stream,
  // Value is the associated symbol. If Len = 0, Value indicates an index
  // to a follow-up table (for codelengths > 8)
  THuffmanLookupTable = record
    Len: array [0 .. 255] of Byte;
    Value: array [0 .. 255] of SmallInt;
  end;

  PHuffmanLookupTable = ^THuffmanLookupTable;

  // Used to construct a histogram (frequency count) of encoded huffman symbols
  T8bitHuffmanHistogram = array [0 .. 255] of Integer;
  P8bitHuffmanHistogram = ^T8bitHuffmanHistogram;

  // Quantization table specified in DQT marker
  TQuantizationTable = class(TJPEG_Persistent)
  public
    // Quantization values Q
    FQuant: array [0 .. 63] of word;
    // Precision P
    FPrecision: TQuantizationPrecision;
    // transpose
    procedure Transpose;
  end;

  TQuantizationTableList = class(TCoreClassObjectList)
  private
    function GetItems(Index: Integer): TQuantizationTable;
  public
    property Items[Index: Integer]: TQuantizationTable read GetItems; default;
  end;

  // Frame component specified in SOF marker
  TFrameComponent = class(TCoreClassPersistent)
  public
    // Horizontal sampling factor H
    FHorzSampling: Integer;
    // Vertical sampling factor V
    FVertSampling: Integer;
    // Component identifier C (can be ascii)
    FComponentID: Integer;
    // Quantization table destination Tq
    FQTable: Integer;
  end;

  TFrameComponentList = class(TCoreClassObjectList)
  private
    function GetItems(Index: Integer): TFrameComponent;
  public
    property Items[Index: Integer]: TFrameComponent read GetItems; default;
  end;

  // Scan component specified in SOS marker
  TScanComponent = class(TCoreClassPersistent)
  public
    // Index into frame components list, Cidx
    FComponent: Integer;
    // DC entropy table destination Td
    FDCTable: Integer;
    // AC entropy table destination Ta
    FACTable: Integer;
    // Used as predictor in DC coding
    FPredictor: SmallInt;
  end;

  TScanComponentList = class(TCoreClassObjectList)
  private
    function GetItems(Index: Integer): TScanComponent;
  public
    property Items[Index: Integer]: TScanComponent read GetItems; default;
  end;

  // Holds data for one image component in the frame, provides method
  // to add/extract samples to/from the MCU currently being decoded/encoded
  TJpegBlockMap = class(TJPEG_Persistent)
  private
    FCoef: array of SmallInt;
    FCoefBackup: array of SmallInt; // used when adjusting brightness/contrast
    FSample: array of Byte;
    FFrame: TFrameComponent;  // Pointer to frame info
    FHorzBlockCount: Integer; // Horizontal block count
    FVertBlockCount: Integer; // Vertical block count
    FBlockStride: Integer;    // number of samples per block
    FScanStride: Integer;     // width of a scanline
  protected
    procedure CreateMap; virtual;
  public
    procedure SetSize(HorzMcuCount_, VertMcuCount_: Integer;
      Frame_: TFrameComponent; BlockStride_: Integer);
    procedure Resize(HorzBlockCount_, VertBlockCount_: Integer);
    procedure ReduceBlockSize(NewSize_: Integer);
    // Number of blocks in the MCU belonging to this image
    function McuBlockCount(ScanCount_: Integer): Integer;
    // Total number of blocks in image (size / 8x8)
    function TotalBlockCount: Integer;
    function GetCoefPointerMCU(McuX_, McuY_, McuIdx_: Integer): Pointer;
    function GetCoefPointer(BlockX, BlockY: Integer): Pointer;
    function GetSamplePointer(BlockX, BlockY: Integer): Pointer;
    function FirstCoef: Pointer;
    function FirstCoefBackup: Pointer;
    function HasCoefBackup: Boolean;
    procedure MakeCoefBackup;
    procedure ClearCoefBackup;
    procedure SaveRawValues(const FileName_: string);
    property HorzBlockCount: Integer read FHorzBlockCount;
    property VertBlockCount: Integer read FVertBlockCount;
    property BlockStride: Integer read FBlockStride;
    property ScanStride: Integer read FScanStride;
    property Frame: TFrameComponent read FFrame;
  end;

  TBlockMapList = class(TCoreClassObjectList)
  private
    function GetItems(Index: Integer): TJpegBlockMap;
  public
    property Items[Index: Integer]: TJpegBlockMap read GetItems; default;
  end;

  TJpegTile = class
  public
    FMcuIndex: Integer;
    FStreamPos: int64;
    FBits: cardinal;
    FBitsLeft: Integer;
    FPredictors: array of SmallInt;
  end;

  TJpegTileList = class(TCoreClassObjectList)
  private
    function GetItems(Index: Integer): TJpegTile;
  public
    function IndexByMcuIndex(McuIndex_: Integer): Integer;
    property Items[Index: Integer]: TJpegTile read GetItems; default;
  end;

  // Huffman table values + codes specified in DHT marker
  THuffmanTable = class(TCoreClassPersistent)
  private
    FItems: array of THuffmanCode;
    function GetItems(Index: Integer): PHuffmanCode;
    function GetCount: Integer;
    procedure SetCount(const Value: Integer);
  public
    property Items[Index: Integer]: PHuffmanCode read GetItems; default;
    property Count: Integer read GetCount write SetCount;
  end;

  THuffmanTableList = class(TCoreClassObjectList)
  private
    function GetItems(Index: Integer): THuffmanTable;
  public
    property Items[Index: Integer]: THuffmanTable read GetItems; default;
  end;

  // Collected component data from markers
  TJpegInfo = class(TCoreClassPersistent)
  public
    // Repository of tables, are updated after DQT or DHT markers
    FDCHuffmanTables: THuffmanTableList;
    FACHuffmanTables: THuffmanTableList;
    FQuantizationTables: TQuantizationTableList;
    // List of frames
    FFrames: TFrameComponentList;
    // List of scans
    FScans: TScanComponentList;
    // Number of image components in frame, Nf
    FFrameCount: Integer;
    // Number of image components in scan, Ns
    FScanCount: Integer;
    // Maximum of all H_i in current scan, Hmax
    FHorzSamplingMax: Integer;
    // Maximum of all V_i in current scan, Vmax
    FVertSamplingMax: Integer;
    // Restart interval MCU count (0 means disabled), updated after RST marker, Ri
    FRestartInterval: Integer;
    // Image width, X
    FWidth: Integer;
    // Image Height, Y
    FHeight: Integer;
    // Jpeg encoding method
    FEncodingMethod: TJpegEncodingMethod;
    // Sample precision in bits for samples, P;
    FSamplePrecision: Integer;
    // Start of spectral selection, Ss
    FSpectralStart: Integer;
    // End of spectral selection, Se
    FSpectralEnd: Integer;
    // Succ Approximation high bitpos, Ah
    FApproxHigh: Integer;
    // Succ Approximation low bitpos, Al
    FApproxLow: Integer;
    // Width of the MCU block in pixels
    FMcuWidth: Integer;
    // Height of the MCU block in pixels
    FMcuHeight: Integer;
    // Horizontal MCU count
    FHorzMcuCount: Integer;
    // Vertical MCU count
    FVertMcuCount: Integer;
    //
    FWaitForDNL: Boolean;
    // Width of a tile in pixels during TileMode
    FTileWidth: Integer;
    // Height of a tile in pixels during TileMode
    FTileHeight: Integer;
    //
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
  end;

  TJpegMarker = class(TJPEG_Persistent)
  private
    FMarkerTag: Byte;
    // FOwner: TJPEG_Base_Object;
  protected
    FStream: TMemoryStream64;
    FCodingInfo: TJpegInfo;
    function GetMarkerName: TPascalString; virtual;
    procedure StoreData(S: TCoreClassStream; Size: Integer);
{$IFDEF JPEG_Debug}
    procedure DebugSample(S: TCoreClassStream; Size: Integer);
{$ENDIF JPEG_Debug}
  public
    constructor Create(CodingInfo_: TJpegInfo; Tag_: Byte); virtual;
    destructor Destroy; override;
    class function GetByte(S: TCoreClassStream): Byte;
    class function GetWord(S: TCoreClassStream): word;
    class procedure PutByte(S: TCoreClassStream; B: Byte);
    class procedure PutWord(S: TCoreClassStream; W: word);
    class function GetSignature: TPascalString; virtual;
    class function GetMarker: Byte; virtual;
    class function IsSegment(Marker_: Byte; Stream_: TCoreClassStream): Boolean; virtual;
    procedure LoadFromStream(S: TCoreClassStream; Size: Integer);
    procedure SaveToStream(S: TCoreClassStream);
    procedure ReadMarker; virtual;
    procedure WriteMarker; virtual;
    // Any of the mkXXXX constants defined in sdJpegConsts
    property MarkerTag: Byte read FMarkerTag;
    // 3letter description of the marker or the hex description
    property MarkerName: TPascalString read GetMarkerName;
    // marker data stored in its stream
    property Stream: TMemoryStream64 read FStream;
    // Reference to owner TJpegFormat, set when adding to the list, and used
    // for DoDebugOut
    property Owner: TJPEG_Base_Object read FOwner write FOwner;
  end;

  TJpegMarkerSet = set of Byte;

  TJpegMarkerClass = class of TJpegMarker;

  TJpegMarkerList = class(TCoreClassObjectList)
  private
    FOwner: TJPEG_Base_Object; // Reference to owner TJpegFormat
    function GetItems(Index: Integer): TJpegMarker;
  public
    constructor Create(Owner_: TJPEG_Base_Object);
    function ByTag(MarkerTag_: Byte): TJpegMarker;
    function ByClass(Class_: TJpegMarkerClass): TJpegMarker;
    function HasMarker(Set_: TJpegMarkerSet): Boolean;
    procedure RemoveMarkers(Set_: TJpegMarkerSet);
    procedure InsertAfter(Set_: TJpegMarkerSet; Marker_: TJpegMarker);
    procedure Add(Item_: TCoreClassObject);
    property Items[Index: Integer]: TJpegMarker read GetItems; default;
  end;

  TAPPnMarker = class(TJpegMarker)
  protected
    function GetMarkerName: TPascalString; override;
  public
    procedure ReadMarker; override;
  end;

  TICCProfileMarker = class(TAPPnMarker)
  private
    FIsValid: Boolean;
    FCurrentMarker: Byte;
    FMarkerCount: Byte;
    function GetCurrentMarker: Byte;
    function GetMarkerCount: Byte;
    function GetData: Pointer;
    function GetDataLength: Integer;
    procedure SetDataLength(const Value: Integer);
    procedure SetCurrentMarker(const Value: Byte);
    procedure SetMarkerCount(const Value: Byte);
  protected
    function GetIsValid: Boolean;
    function GetMarkerName: TPascalString; override;
  public
    class function GetSignature: TPascalString; override;
    class function GetMarker: Byte; override;
    property IsValid: Boolean read GetIsValid;
    property CurrentMarker: Byte read GetCurrentMarker write SetCurrentMarker;
    property MarkerCount: Byte read GetMarkerCount write SetMarkerCount;
    property Data: Pointer read GetData;
    property DataLength: Integer read GetDataLength write SetDataLength;
  end;

  // ICC color profile class
  TJpegICCProfile = class(TCoreClassPersistent)
  private
    FData: array of Byte;
    function GetData: Pointer;
    function GetDataLength: Integer;
  public
    procedure LoadFromStream(S: TCoreClassStream);
    procedure LoadFromFile(const FileName_: string);
    procedure SaveToFile(const FileName_: string);
    procedure SaveToStream(S: TCoreClassStream);
    procedure ReadFromMarkerList(List_: TJpegMarkerList);
    procedure WriteToMarkerList(List_: TJpegMarkerList);
    property Data: Pointer read GetData;
    property DataLength: Integer read GetDataLength;
  end;

const
  cWarnStyleNames: array [TWarnStyle] of string = ('info', 'hint', 'warn', 'fail');

  // Jpeg markers defined in Table B.1
  mkNone = 0;

  mkSOF0 = $C0; // Baseline DCT + Huffman encoding
  mkSOF1 = $C1; // Extended Sequential DCT + Huffman encoding
  mkSOF2 = $C2; // Progressive DCT + Huffman encoding
  mkSOF3 = $C3; // Lossless (sequential) + Huffman encoding

  mkSOF5 = $C5; // Differential Sequential DCT + Huffman encoding
  mkSOF6 = $C6; // Differential Progressive DCT + Huffman encoding
  mkSOF7 = $C7; // Differential Lossless (sequential) + Huffman encoding

  mkJPG = $C8;   // Reserved for Jpeg extensions
  mkSOF9 = $C9;  // Extended Sequential DCT + Arithmetic encoding
  mkSOF10 = $CA; // Progressive DCT + Arithmetic encoding
  mkSOF11 = $CB; // Lossless (sequential) + Arithmetic encoding

  mkSOF13 = $CD; // Differential Sequential DCT + Arithmetic encoding
  mkSOF14 = $CE; // Differential Progressive DCT + Arithmetic encoding
  mkSOF15 = $CF; // Differential Lossless (sequential) + Arithmetic encoding

  mkDHT = $C4; // Define Huffman Table

  mkDAC = $CC; // Define Arithmetic Coding

  mkRST0 = $D0; // Restart markers
  mkRST1 = $D1;
  mkRST2 = $D2;
  mkRST3 = $D3;
  mkRST4 = $D4;
  mkRST5 = $D5;
  mkRST6 = $D6;
  mkRST7 = $D7;

  mkSOI = $D8; // Start of Image
  mkEOI = $D9; // End of Image
  mkSOS = $DA; // Start of Scan
  mkDQT = $DB; // Define Quantization Table
  mkDNL = $DC; // Define Number of Lines
  mkDRI = $DD; // Define Restart Interval
  mkDHP = $DE; // Define Hierarchical Progression
  mkEXP = $DF; // Expand reference components

  // For APPn markers see:
  // http://www.ozhiker.com/electronics/pjmt/jpeg_info/app_segments.html

  mkAPP0 = $E0; // APPn markers - APP0 = JFIF
  mkAPP1 = $E1; // APP1 = EXIF or XMP
  mkAPP2 = $E2; // ICC colour profile
  mkAPP3 = $E3;
  mkAPP4 = $E4;
  mkAPP5 = $E5;
  mkAPP6 = $E6;
  mkAPP7 = $E7;
  mkAPP8 = $E8;
  mkAPP9 = $E9;
  mkAPP10 = $EA;
  mkAPP11 = $EB;
  mkAPP12 = $EC;
  mkAPP13 = $ED; // APP13 = IPTC or Adobe IRB
  mkAPP14 = $EE; // APP14 = Adobe
  mkAPP15 = $EF;

  mkJPG0 = $F0; // JPGn markers - reserved for JPEG extensions
  mkJPG13 = $FD;
  mkCOM = $FE; // Comment

  mkTEM = $01; // Reserved for temporary use

  cColorSpaceNames: array [TJpegColorSpace] of string =
    ('AutoDetect', 'Gray', 'GrayA', 'RGB', 'RGBA', 'YCbCr', 'YCbCrA', 'CMYK', 'CMYK as YCbCrK', 'YCCK', 'PhotoYCC', 'PhotoYCCA', 'ITU CieLAB');

  cDefaultJpgCompressionQuality = 80;

  // This matrix maps zigzag position to the left/right
  // top/down normal position inside the 8x8 block.
  cJpegInverseZigZag1x1: TZigZagArray =
    (0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0);

  cJpegInverseZigZag2x2: TZigZagArray =
    (0, 1, 2, 0, 3, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0);

  cJpegInverseZigZag4x4: TZigZagArray =
    (0, 1, 4, 8, 5, 2, 3, 6,
    9, 12, 0, 13, 10, 7, 0, 0,
    0, 11, 14, 0, 0, 0, 0, 0,
    15, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0);

  cJpegInverseZigZag8x8: TZigZagArray =
    (0, 1, 8, 16, 9, 2, 3, 10,
    17, 24, 32, 25, 18, 11, 4, 5,
    12, 19, 26, 33, 40, 48, 41, 34,
    27, 20, 13, 6, 7, 14, 21, 28,
    35, 42, 49, 56, 57, 50, 43, 36,
    29, 22, 15, 23, 30, 37, 44, 51,
    58, 59, 52, 45, 38, 31, 39, 46,
    53, 60, 61, 54, 47, 55, 62, 63,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0);

  cJpegForwardZigZag8x8: TZigZagArray =
    (0, 1, 5, 6, 14, 15, 27, 28,
    2, 4, 7, 13, 16, 26, 29, 42,
    3, 8, 12, 17, 25, 30, 41, 43,
    9, 11, 18, 24, 31, 40, 44, 53,
    10, 19, 23, 32, 39, 45, 52, 54,
    20, 22, 33, 38, 46, 51, 55, 60,
    21, 34, 37, 47, 50, 56, 59, 61,
    35, 36, 48, 49, 57, 58, 62, 63,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0);

  cJpegNaturalZigZag8x8: TZigZagArray =
    (0, 1, 2, 3, 4, 5, 6, 7,
    8, 9, 10, 11, 12, 13, 14, 15,
    16, 17, 18, 19, 20, 21, 22, 23,
    24, 25, 26, 27, 28, 29, 30, 31,
    32, 33, 34, 35, 36, 37, 38, 39,
    40, 41, 42, 43, 44, 45, 46, 47,
    48, 49, 50, 51, 52, 53, 54, 55,
    56, 57, 58, 59, 60, 61, 62, 63,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0);

  // entry n equals 1 shl (n-1)
  cExtendTest: array [0 .. 15] of Integer = ($0000, $0001, $0002, $0004, $0008, $0010, $0020, $0040, $0080, $0100, $0200, $0400, $0800, $1000, $2000, $4000);

  // entry n equals (-1 shl n) + 1
  cExtendOffset: array [0 .. 15] of Integer =
    (0, ((-1) shl 1) + 1, ((-1) shl 2) + 1, ((-1) shl 3) + 1, ((-1) shl 4) + 1,
    ((-1) shl 5) + 1, ((-1) shl 6) + 1, ((-1) shl 7) + 1, ((-1) shl 8) + 1,
    ((-1) shl 9) + 1, ((-1) shl 10) + 1, ((-1) shl 11) + 1, ((-1) shl 12) + 1,
    ((-1) shl 13) + 1, ((-1) shl 14) + 1, ((-1) shl 15) + 1);

  // These are the sample quantization tables given in JPEG spec section K.1.
  // The spec says that the values given produce "good" quality, and
  // when divided by 2, "very good" quality.
  cStdLuminanceQuantTbl: TIntArray64 =
    (16, 11, 10, 16, 24, 40, 51, 61,
    12, 12, 14, 19, 26, 58, 60, 55,
    14, 13, 16, 24, 40, 57, 69, 56,
    14, 17, 22, 29, 51, 87, 80, 62,
    18, 22, 37, 56, 68, 109, 103, 77,
    24, 35, 55, 64, 81, 104, 113, 92,
    49, 64, 78, 87, 103, 121, 120, 101,
    72, 92, 95, 98, 112, 100, 103, 99);

  cStdChrominanceQuantTbl: TIntArray64 =
    (17, 18, 24, 47, 99, 99, 99, 99,
    18, 21, 26, 66, 99, 99, 99, 99,
    24, 26, 56, 99, 99, 99, 99, 99,
    47, 66, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99);

  // These are standard Huffman tables for general use
  cHuffmanBitsDcLum: array [0 .. 15] of Byte = (0, 1, 5, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0);
  cHuffmanValDCLum: array [0 .. 11] of Byte = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11);

  cHuffmanBitsDCChrom: array [0 .. 15] of Byte = (0, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0);
  cHuffmanValDCChrom: array [0 .. 11] of Byte = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11);

  cHuffmanBitsACLum: array [0 .. 15] of Byte = (0, 2, 1, 3, 3, 2, 4, 3, 5, 5, 4, 4, 0, 0, 1, $7D);

  cHuffmanValACLum: array [0 .. 161] of Byte =
    ($01, $02, $03, $00, $04, $11, $05, $12,
    $21, $31, $41, $06, $13, $51, $61, $07,
    $22, $71, $14, $32, $81, $91, $A1, $08,
    $23, $42, $B1, $C1, $15, $52, $D1, $F0,
    $24, $33, $62, $72, $82, $09, $0A, $16,
    $17, $18, $19, $1A, $25, $26, $27, $28,
    $29, $2A, $34, $35, $36, $37, $38, $39,
    $3A, $43, $44, $45, $46, $47, $48, $49,
    $4A, $53, $54, $55, $56, $57, $58, $59,
    $5A, $63, $64, $65, $66, $67, $68, $69,
    $6A, $73, $74, $75, $76, $77, $78, $79,
    $7A, $83, $84, $85, $86, $87, $88, $89,
    $8A, $92, $93, $94, $95, $96, $97, $98,
    $99, $9A, $A2, $A3, $A4, $A5, $A6, $A7,
    $A8, $A9, $AA, $B2, $B3, $B4, $B5, $B6,
    $B7, $B8, $B9, $BA, $C2, $C3, $C4, $C5,
    $C6, $C7, $C8, $C9, $CA, $D2, $D3, $D4,
    $D5, $D6, $D7, $D8, $D9, $DA, $E1, $E2,
    $E3, $E4, $E5, $E6, $E7, $E8, $E9, $EA,
    $F1, $F2, $F3, $F4, $F5, $F6, $F7, $F8,
    $F9, $FA);

  cHuffmanBitsACChrom: array [0 .. 15] of Byte = (0, 2, 1, 2, 4, 4, 3, 4, 7, 5, 4, 4, 0, 1, 2, $77);
  cHuffmanValACChrom: array [0 .. 161] of Byte =
    ($00, $01, $02, $03, $11, $04, $05, $21,
    $31, $06, $12, $41, $51, $07, $61, $71,
    $13, $22, $32, $81, $08, $14, $42, $91,
    $A1, $B1, $C1, $09, $23, $33, $52, $F0,
    $15, $62, $72, $D1, $0A, $16, $24, $34,
    $E1, $25, $F1, $17, $18, $19, $1A, $26,
    $27, $28, $29, $2A, $35, $36, $37, $38,
    $39, $3A, $43, $44, $45, $46, $47, $48,
    $49, $4A, $53, $54, $55, $56, $57, $58,
    $59, $5A, $63, $64, $65, $66, $67, $68,
    $69, $6A, $73, $74, $75, $76, $77, $78,
    $79, $7A, $82, $83, $84, $85, $86, $87,
    $88, $89, $8A, $92, $93, $94, $95, $96,
    $97, $98, $99, $9A, $A2, $A3, $A4, $A5,
    $A6, $A7, $A8, $A9, $AA, $B2, $B3, $B4,
    $B5, $B6, $B7, $B8, $B9, $BA, $C2, $C3,
    $C4, $C5, $C6, $C7, $C8, $C9, $CA, $D2,
    $D3, $D4, $D5, $D6, $D7, $D8, $D9, $DA,
    $E2, $E3, $E4, $E5, $E6, $E7, $E8, $E9,
    $EA, $F2, $F3, $F4, $F5, $F6, $F7, $F8,
    $F9, $FA);

  // Motion Jpeg DHT segment
  cMjpgDHTSeg: packed array [0 .. 415] of Byte = (
    $00, $00, $01, $05, $01, $01, $01, $01, $01, $01, $00, $00, $00, $00, $00,
    $00, $00, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $01,
    $00, $03, $01, $01, $01, $01, $01, $01, $01, $01, $01, $00, $00, $00, $00,

    $00, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $10, $00,
    $02, $01, $03, $03, $02, $04, $03, $05, $05, $04, $04, $00, $00, $01, $7D,
    $01, $02, $03, $00, $04, $11, $05, $12, $21, $31, $41, $06, $13, $51, $61,
    $07, $22, $71, $14, $32, $81, $91, $A1, $08, $23, $42, $B1, $C1, $15, $52,
    $D1, $F0, $24, $33, $62, $72, $82, $09, $0A, $16, $17, $18, $19, $1A, $25,
    $26, $27, $28, $29, $2A, $34, $35, $36, $37, $38, $39, $3A, $43, $44, $45,
    $46, $47, $48, $49, $4A, $53, $54, $55, $56, $57, $58, $59, $5A, $63, $64,

    $65, $66, $67, $68, $69, $6A, $73, $74, $75, $76, $77, $78, $79, $7A, $83,
    $84, $85, $86, $87, $88, $89, $8A, $92, $93, $94, $95, $96, $97, $98, $99,
    $9A, $A2, $A3, $A4, $A5, $A6, $A7, $A8, $A9, $AA, $B2, $B3, $B4, $B5, $B6,
    $B7, $B8, $B9, $BA, $C2, $C3, $C4, $C5, $C6, $C7, $C8, $C9, $CA, $D2, $D3,
    $D4, $D5, $D6, $D7, $D8, $D9, $DA, $E1, $E2, $E3, $E4, $E5, $E6, $E7, $E8,
    $E9, $EA, $F1, $F2, $F3, $F4, $F5, $F6, $F7, $F8, $F9, $FA, $11, $00, $02,
    $01, $02, $04, $04, $03, $04, $07, $05, $04, $04, $00, $01, $02, $77, $00,

    $01, $02, $03, $11, $04, $05, $21, $31, $06, $12, $41, $51, $07, $61, $71,
    $13, $22, $32, $81, $08, $14, $42, $91, $A1, $B1, $C1, $09, $23, $33, $52,
    $F0, $15, $62, $72, $D1, $0A, $16, $24, $34, $E1, $25, $F1, $17, $18, $19,
    $1A, $26, $27, $28, $29, $2A, $35, $36, $37, $38, $39, $3A, $43, $44, $45,
    $46, $47, $48, $49, $4A, $53, $54, $55, $56, $57, $58, $59, $5A, $63, $64,
    $65, $66, $67, $68, $69, $6A, $73, $74, $75, $76, $77, $78, $79, $7A, $82,
    $83, $84, $85, $86, $87, $88, $89, $8A, $92, $93, $94, $95, $96, $97, $98,

    $99, $9A, $A2, $A3, $A4, $A5, $A6, $A7, $A8, $A9, $AA, $B2, $B3, $B4, $B5,
    $B6, $B7, $B8, $B9, $BA, $C2, $C3, $C4, $C5, $C6, $C7, $C8, $C9, $CA, $D2,
    $D3, $D4, $D5, $D6, $D7, $D8, $D9, $DA, $E2, $E3, $E4, $E5, $E6, $E7, $E8,
    $E9, $EA, $F2, $F3, $F4, $F5, $F6, $F7, $F8, $F9, $FA);

  sInternalError = 'Internal error';
  sUnsupportedEncoding = 'Unsupported encoding: SOF%d';
  sMarkerExpected = 'Jpeg Marker expected';
  sUnsupportedBitsPerSample = 'Unsupported bits per sample';
  sInvalidTableClass = 'Invalid table class in DHT marker';
  sInputStreamChopped = 'Input stream prematurely chopped';
  sUnexpectedMarkerInEncodedStream = 'Unexpected marker in encoded stream';
  sInvalidFrameRef = 'Invalid frame reference in scan component';
  sNoColorTransformation = 'No color transformation available for current settings';
  sNoDCTCoefficentsAvailable = 'No DCT coefficients available (compress first)';
  sOperationOnlyFor8x8 = 'Operation can only be performed with LoadScale = jsFull';
  sBitmapIsEmptyCannotSave = 'Bitmap is empty; cannot save';
  sInvalidFormatForSelectedCS = 'Invalid bitmap PFormat for selected color space';
  sCommentCannotBeSet = 'Comment cannot be set before assigning bitmap';
  sDNLMarkerExpected = 'DNL marker expected';
  sUnsupportedColorSpace = 'Unsupported color space';
  sOnProvideStripMustBeAssigned = 'OnProvideStrip must be assigned';
  sOnCreateMapMustBeAssigned = 'OnCreateMap must be assigned';
  sCannotUseTileMode = 'Cannot use tilemode with progressive jpeg';
  sRangeErrorInTileLoading = 'Range error in tiled loading: make sure to select tilemode';

{$IFDEF JPEG_Debug}
function sdDebugMessageToString(Sender: TCoreClassObject; WarnStyle: TWarnStyle; const Message_: TPascalString): TPascalString;
{$ENDIF JPEG_Debug}

function sdClassName(Object_: TCoreClassObject): TPascalString;

// general functions
function IntMin(i1, i2: Integer): Integer; inline;

// divisor based on scale
function sdGetDivisor(Scale_: TJpegScale): Integer;

type
  TItemCompareEvent = function(Item1, Item2: TObject; Info: Pointer): Integer of object;
  TItemCompareMethod = function(Item1, Item2: TObject; Info: Pointer): Integer;
  TPointerCompareMethod = function(Ptr1, Ptr2: Pointer): Integer;

  TCustomObjectList = class(TCoreClassObjectList)
  public
    procedure Append(Item_: TObject);
  end;

  // Keep a sorted list of objects, sort them by the object's locally unique ID (Luid),
  // which is just an integer (32bits, instead of a real 128bits Guid).
  // Override method GetLuid, it should return the Luid of object Item_.
  // TLuidList is used for compatibility with legacy code (formerly TUniqueIDList)
  TLuidList = class(TCustomObjectList)
  protected
    function GetLuid(Item_: TObject): Integer; virtual; abstract;
    function IndexByLuid(const LUID_: Integer; out Index: Integer): Boolean;
  public
    function NextLuid: Integer;
    function HasLuid(const LUID_: Integer): Boolean;
    procedure RemoveByLuid(const LUID_: Integer);
    function Add(Item_: TObject): Integer;
  end;

  // Keep a sorted list of objects, sort them by the object's globally unique ID (Guid).
  // Override method GetGuid, it should return the guid of object Item_.
  TGuidList = class(TCustomObjectList)
  protected
    function GetGuid(Item_: TObject): TGuid; virtual; abstract;
    function IndexByGuid(const GUID_: TGuid; out Index: Integer): Boolean;
  public
    function HasGuid(const GUID_: TGuid): Boolean;
    procedure RemoveByGuid(const GUID_: TGuid);
    function Add(Item_: TObject): Integer;
  end;

  // TCustomSortedList is a TCoreClassObjectList descendant providing easy sorting
  // capabilities, while keeping simplicity. Override the DoCompare method
  // to compare two items.
  TCustomSortedList = class(TCustomObjectList)
  private
    FSorted: Boolean;
    procedure SetSorted(Value_: Boolean);
  protected
    // Override this method to implement the object comparison between two
    // items. The default just compares the item pointers
    function DoCompare(Item1, Item2: TObject): Integer; virtual;
  public
    constructor Create(OwnsObjects_: Boolean = true);
    function Add(Item_: TObject): Integer;
    // AddUnique behaves just like Add but checks if the item to add is unique
    // by checking the result of the Find function. If the item is found it is
    // replaced by the new item (old item removed), unless RaiseError = True, in
    // that case an exception is raised.
    function AddUnique(Item: TObject; RaiseError: Boolean = false): Integer; virtual;
    function Find(Item: TObject; out Index: Integer): Boolean; virtual;
    // Find (multiple) items equal to Item, and return Index of first equal
    // item and the number of multiples in Count
    procedure FindMultiple(Item: TObject; out AIndex, Count_: Integer); virtual;
    procedure Sort; virtual;
    property Sorted: Boolean read FSorted write SetSorted default true;
  end;

  // Some basic compare routines
function CompareCardinal(C1, C2: cardinal): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CompareInteger(Int1, Int2: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CompareLongWord(LW1, LW2: longword): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CompareInt64(const Int1, Int2: int64): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ComparePointer(Item1, Item2: Pointer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CompareBool(Bool1, Bool2: Boolean): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CompareSingle(const Single1, Single2: single): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CompareDouble(const Double1, Double2: double): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
// Compare globally unique ID (TGUID, defined in SysUtils)
function CompareGuid(const Guid1, Guid2: TGuid): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}

// GUID methods
function IsEqualGuid(const Guid1, Guid2: TGuid): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsEmptyGuid(const GUID_: TGuid): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function NewGuid: TGuid;
// streaming of TGuid can be found in sdStreamableData.pas

type

  TCustomSorter = class
  private
    FCompareMethod: TPointerCompareMethod;
    FFirst: Pointer;
    FStride: Integer;
    FCount: Integer;
  public
    property CompareMethod: TPointerCompareMethod read FCompareMethod write FCompareMethod;
    property First: Pointer read FFirst write FFirst;
    property Stride: Integer read FStride write FStride;
    property Count: Integer read FCount write FCount;
    procedure Sort;
  end;

procedure sdSortArraySingle(First_: PSingle; Count_: Integer);
procedure sdSortArrayDouble(First_: PDouble; Count_: Integer);
procedure sdSortArrayInteger(First_: PInteger; Count_: Integer);

function sdAverageOfArrayInteger(First_: PInteger; Count_: Integer): double;
function sdMinimumOfArrayInteger(First_: PInteger; Count_: Integer): Integer;
function sdMaximumOfArrayInteger(First_: PInteger; Count_: Integer): Integer;

function sdMinimumOfArrayDouble(First_: PDouble; Count_: Integer): double;
function sdMaximumOfArrayDouble(First_: PDouble; Count_: Integer): double;

// Walking average of array in SFirst, put result in DFirst. Both must be of
// length Count_. The average is done with a window of WindowSize_, and the
// center pixel in Center_ (e.g. Center_ = 1, WindowSize_ = 3 for 3 values w.a.).
// After running, the values must still be divided by WindowSize_
procedure sdWalkingAverageArrayInteger(SFirst, DFirst: PInteger; Count_, Center_, WindowSize_: Integer);

// Walking median of array in SFirst, put result in DFirst. Both must be of
// length Count_. The median is done with a window of WindowSize_, and the
// center pixel in Center_ (e.g. Center_ = 1, WindowSize_ = 3 for 3 values w.m.).
procedure sdWalkingMedianArrayInteger(SFirst, DFirst: PInteger; Count_, Center_, WindowSize_: Integer);

procedure sdWalkingAverageArrayDouble(SFirst, DFirst: PDouble; Count_, Center_, WindowSize_: Integer);
procedure sdWalkingMedianArrayDouble(SFirst, DFirst: PDouble; Count_, Center_, WindowSize_: Integer);

const
  sAddingNonUniqueObject = 'Adding non-unique object to list is not allowed';
  sListMustBeSorted = 'List must be sorted';
  cEmptyGuid: TGuid = (D1: 0; D2: 0; D3: 0; D4: (0, 0, 0, 0, 0, 0, 0, 0));

implementation

{ TJPEG_Base_Object }

constructor TJPEG_Base_Object.Create(Owner_: TJPEG_Base_Object);
begin
  inherited Create;
  FOwner := Owner_;
{$IFDEF JPEG_Debug}
  FOnDebugOut := nil;
{$ENDIF JPEG_Debug}
end;

{$IFDEF JPEG_Debug}


procedure TJPEG_Base_Object.DoDebugOut(Sender: TCoreClassObject; WarnStyle: TWarnStyle; const Message_: TPascalString);
var
  Owner_: TCoreClassObject;
begin
  Owner_ := Self;
  while Owner_ is TJPEG_Base_Object do
    begin
      if assigned(TJPEG_Base_Object(Owner_).FOnDebugOut) then
        begin
          TJPEG_Base_Object(Owner_).FOnDebugOut(Sender, WarnStyle, Message_);
          exit;
        end;
      Owner_ := TJPEG_Base_Object(Owner_).FOwner;
    end;
end;
{$ENDIF JPEG_Debug}

{ TJPEG_Persistent }

constructor TJPEG_Persistent.Create;
begin
  inherited Create;
  FOwner := nil;
end;

constructor TJPEG_Persistent.CreateDebug(Owner_: TJPEG_Base_Object);
begin
  Create;
  FOwner := Owner_;
end;

{$IFDEF JPEG_Debug}


procedure TJPEG_Persistent.DoDebugOut(Sender: TCoreClassObject; WarnStyle: TWarnStyle; const Message_: TPascalString);
begin
  if FOwner <> nil then
      FOwner.DoDebugOut(Sender, WarnStyle, Message_);
end;
{$ENDIF JPEG_Debug}

{ Functions }

{$IFDEF JPEG_Debug}


function sdDebugMessageToString(Sender: TCoreClassObject; WarnStyle: TWarnStyle; const Message_: TPascalString): TPascalString;
var
  SenderString: TPascalString;
begin
  if assigned(Sender) then
      SenderString := Sender.ClassName
  else
      SenderString := '';
  Result := '[' + cWarnStyleNames[WarnStyle] + '] ' + SenderString + ': ' + Message_;
end;
{$ENDIF JPEG_Debug}


function sdClassName(Object_: TCoreClassObject): TPascalString;
begin
  Result := 'nil';
  if assigned(Object_) then
      Result := Object_.ClassName;
end;

{ TQuantizationTable }

procedure TQuantizationTable.Transpose;
var
  x, y, i, j: Integer;
  Temp: word;
begin
  // transpose indices in table, but we must do this with the forward zigzag
  for y := 0 to 6 do
    for x := y + 1 to 7 do
      begin
        i := cJpegForwardZigZag8x8[x + y * 8];
        j := cJpegForwardZigZag8x8[x * 8 + y];
        Temp := FQuant[i];
        FQuant[i] := FQuant[j];
        FQuant[j] := Temp;
      end;
end;

{ TQuantizationTableList }

function TQuantizationTableList.GetItems(Index: Integer): TQuantizationTable;
begin
  if Index >= Count then
      Count := Index + 1;
  Result := TQuantizationTable(inherited Items[Index]);
  if not assigned(Result) then
    begin
      Result := TQuantizationTable.Create;
      inherited Items[Index] := Result;
    end;
end;

{ TFrameComponentList }

function TFrameComponentList.GetItems(Index: Integer): TFrameComponent;
begin
  if Index >= Count then
      Count := Index + 1;
  Result := TFrameComponent(inherited Items[Index]);
  if not assigned(Result) then
    begin
      Result := TFrameComponent.Create;
      inherited Items[Index] := Result;
    end;
end;

{ TScanComponentList }

function TScanComponentList.GetItems(Index: Integer): TScanComponent;
begin
  if Index >= Count then
      Count := Index + 1;
  Result := TScanComponent(inherited Items[Index]);
  if not assigned(Result) then
    begin
      Result := TScanComponent.Create;
      inherited Items[Index] := Result;
    end;
end;

{ TJpegBlockMap }

procedure TJpegBlockMap.ClearCoefBackup;
begin
  SetLength(FCoefBackup, 0);
end;

procedure TJpegBlockMap.CreateMap;
var
  Count: Integer;
begin
  FScanStride := FHorzBlockCount * FBlockStride;
  Count := FScanStride * FVertBlockCount;
  SetLength(FCoef, Count);
  SetLength(FSample, Count);
  // Clear the coefficients (since the decoder doesn't always reset them to 0)
  if Count > 0 then
      FillChar(FCoef[0], Count * SizeOf(SmallInt), 0);
  // Clear backup
  ClearCoefBackup;
end;

function TJpegBlockMap.FirstCoef: Pointer;
begin
  Result := @FCoef[0];
end;

function TJpegBlockMap.FirstCoefBackup: Pointer;
begin
  Result := @FCoefBackup[0];
end;

function TJpegBlockMap.GetCoefPointer(BlockX, BlockY: Integer): Pointer;
begin
  Result := @FCoef[BlockX * FBlockStride + BlockY * FScanStride];
end;

function TJpegBlockMap.GetCoefPointerMCU(McuX_, McuY_, McuIdx_: Integer): Pointer;
var
  x, y: Integer;
begin
  x := FFrame.FHorzSampling * McuX_;
  y := FFrame.FVertSampling * McuY_;
  while McuIdx_ >= FFrame.FHorzSampling do
    begin
      inc(y);
      dec(McuIdx_, FFrame.FHorzSampling);
    end;
  inc(x, McuIdx_);
  Result := @FCoef[x * FBlockStride + y * FScanStride];
end;

function TJpegBlockMap.GetSamplePointer(BlockX, BlockY: Integer): Pointer;
begin
  Result := @FSample[BlockX * FBlockStride + BlockY * FScanStride];
end;

function TJpegBlockMap.HasCoefBackup: Boolean;
begin
  Result := length(FCoefBackup) > 0;
end;

procedure TJpegBlockMap.MakeCoefBackup;
var
  Count: Integer;
begin
  Count := length(FCoef);
  if Count <= 0 then
      exit;
  SetLength(FCoefBackup, Count);
  Move(FCoef[0], FCoefBackup[0], Count * SizeOf(SmallInt));
end;

function TJpegBlockMap.McuBlockCount(ScanCount_: Integer): Integer;
begin
  if ScanCount_ = 1 then
      Result := 1
  else
      Result := FFrame.FHorzSampling * FFrame.FVertSampling;
end;

procedure TJpegBlockMap.ReduceBlockSize(NewSize_: Integer);
var
  i, j, Count, Stride: Integer;
  Sc, Dc: PSmallint;
  Ss, Ds: Pbyte;
begin
  if FBlockStride <> 64 then
      exit;

  Count := FHorzBlockCount * FVertBlockCount;

  // coefs
  Sc := @FCoef[0];
  Dc := Sc;
  Stride := NewSize_ * SizeOf(SmallInt);
  for i := 0 to Count - 1 do
    begin
      for j := 0 to 7 do
        begin
          if j < NewSize_ then
            begin
              Move(Sc^, Dc^, Stride);
              inc(Dc, NewSize_);
            end;
          inc(Sc, 8);
        end;
    end;

  // samples
  Ss := @FSample[0];
  Ds := Ss;
  Stride := NewSize_ * SizeOf(Byte);
  for i := 0 to Count - 1 do
    begin
      for j := 0 to 7 do
        begin
          if j < NewSize_ then
            begin
              Move(Ss^, Ds^, Stride);
              inc(Ds, NewSize_);
            end;
          inc(Ss, 8);
        end;
    end;
  FBlockStride := NewSize_ * NewSize_;
  Resize(FHorzBlockCount, FVertBlockCount);
end;

procedure TJpegBlockMap.Resize(HorzBlockCount_, VertBlockCount_: Integer);
var
  Count: Integer;
begin
  FHorzBlockCount := HorzBlockCount_;
  FVertBlockCount := VertBlockCount_;
  FScanStride := FHorzBlockCount * FBlockStride;
  Count := FScanStride * FVertBlockCount;
  SetLength(FCoef, Count);
  SetLength(FSample, Count);
  SetLength(FCoefBackup, 0);
end;

procedure TJpegBlockMap.SaveRawValues(const FileName_: string);
var
  i, x, y: Integer;
  F: TCoreClassFileStream;
  Block: PCoefBlock;
  procedure WriteS(const S: TPascalString);
  var
    buff: TBytes;
  begin
    buff := S.ANSI;
    F.Write(buff[0], length(buff));
  end;

begin
  F := TCoreClassFileStream.Create(FileName_, fmCreate);
  try
    for y := 0 to FVertBlockCount - 1 do
      begin
        WriteS(PFormat('Line %d:', [y]) + #13#10);
        for x := 0 to FHorzBlockCount - 1 do
          begin
            WriteS(PFormat(' Block %d:', [x]) + #13#10);
            WriteS(' ');
            Block := GetCoefPointer(x, y);
            for i := 0 to 63 do
                WriteS(IntToStr(Block^[i]) + ' ');
            WriteS(#13#10);
          end;
      end;
  finally
      F.Free;
  end;
end;

procedure TJpegBlockMap.SetSize(HorzMcuCount_, VertMcuCount_: Integer;
  Frame_: TFrameComponent; BlockStride_: Integer);
begin
  FFrame := Frame_;
  FBlockStride := BlockStride_;

  // Determine block dimensions
  FHorzBlockCount := HorzMcuCount_ * FFrame.FHorzSampling;
  FVertBlockCount := VertMcuCount_ * FFrame.FVertSampling;

  // Assume the data is valid, we can create the map
  CreateMap;
end;

function TJpegBlockMap.TotalBlockCount: Integer;
begin
  Result := FHorzBlockCount * FVertBlockCount;
end;

{ TBlockMapList }

function TBlockMapList.GetItems(Index: Integer): TJpegBlockMap;
begin
  if Index >= Count then
      Count := Index + 1;
  Result := TJpegBlockMap(inherited Items[Index]);
  if not assigned(Result) then
    begin
      Result := TJpegBlockMap.Create;
      inherited Items[Index] := Result;
    end;
end;

{ TJpegTileList }

function TJpegTileList.GetItems(Index: Integer): TJpegTile;
begin
  Result := TJpegTile(inherited Items[Index]);
end;

function TJpegTileList.IndexByMcuIndex(McuIndex_: Integer): Integer;
var
  Min, Max: Integer;
begin
  // Find position for insert - binary method
  Min := 0;
  Max := Count;
  while Min < Max do begin
      Result := (Min + Max) div 2;
      case CompareInteger(Items[Result].FMcuIndex, McuIndex_) of
        - 1: Min := Result + 1;
        0: exit;
        1: Max := Result;
      end;
    end;
  Result := Min;
end;

function THuffmanTable.GetCount: Integer;
begin
  Result := length(FItems);
end;

function THuffmanTable.GetItems(Index: Integer): PHuffmanCode;
begin
  Result := @FItems[Index];
end;

procedure THuffmanTable.SetCount(const Value: Integer);
begin
  SetLength(FItems, Value);
end;

function THuffmanTableList.GetItems(Index: Integer): THuffmanTable;
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

procedure TJpegInfo.Clear;
begin
  // Clear all data in Info
  FDCHuffmanTables.Clear;
  FACHuffmanTables.Clear;
  FQuantizationTables.Clear;
  FFrames.Clear;
  FScans.Clear;

  FFrameCount := 0;
  FScanCount := 0;
  FHorzSamplingMax := 0;
  FVertSamplingMax := 0;
  FRestartInterval := 0;
  FWidth := 0;
  FHeight := 0;
  FEncodingMethod := emUnspecified;
  FSamplePrecision := 0;
  FSpectralStart := 0;
  FSpectralEnd := 0;
  FApproxHigh := 0;
  FApproxLow := 0;
  FWaitForDNL := false;
end;

constructor TJpegInfo.Create;
begin
  inherited Create;
  FDCHuffmanTables := THuffmanTableList.Create;
  FACHuffmanTables := THuffmanTableList.Create;
  FQuantizationTables := TQuantizationTableList.Create;
  FFrames := TFrameComponentList.Create;
  FScans := TScanComponentList.Create;
end;

destructor TJpegInfo.Destroy;
begin
  DisposeObjectAndNil(FDCHuffmanTables);
  DisposeObjectAndNil(FACHuffmanTables);
  DisposeObjectAndNil(FQuantizationTables);
  DisposeObjectAndNil(FFrames);
  DisposeObjectAndNil(FScans);
  inherited;
end;

{ TJpegMarker }

constructor TJpegMarker.Create(CodingInfo_: TJpegInfo; Tag_: Byte);
begin
  inherited Create;
  FCodingInfo := CodingInfo_;
  FMarkerTag := Tag_;
  FStream := TMemoryStream64.CustomCreate(512 * 1024);
end;

{$IFDEF JPEG_Debug}


procedure TJpegMarker.DebugSample(S: TCoreClassStream; Size: Integer);
var
  i: Integer;
  B: Byte;
  Msg: TPascalString;
begin
  Msg := '';
  S.Position := 0;
  for i := 0 to IntMin(Size, 32) - 1 do
    begin
      S.Read(B, 1);
      Msg := Msg + IntToHex(B, 2);
      if i mod 4 = 3 then
          Msg := Msg + '-';
    end;
  S.Position := 0;
  DoDebugOut(Self, wsInfo, Msg + '...');
end;
{$ENDIF JPEG_Debug}


destructor TJpegMarker.Destroy;
begin
  DisposeObjectAndNil(FStream);
  inherited;
end;

class function TJpegMarker.GetByte(S: TCoreClassStream): Byte;
begin
  S.Read(Result, 1);
end;

function TJpegMarker.GetMarkerName: TPascalString;
begin
  Result := IntToHex(FMarkerTag, 2);
end;

class function TJpegMarker.GetWord(S: TCoreClassStream): word;
var
  W: word;
begin
  S.Read(W, 2);
  Result := Swap(W);
end;

procedure TJpegMarker.LoadFromStream(S: TCoreClassStream; Size: Integer);
begin
{$IFDEF JPEG_Debug}
  DoDebugOut(Self, wsInfo, PFormat('<loading marker %s, length:%d>', [MarkerName.Text, Size]));
{$ENDIF JPEG_Debug}
  // by default, we copy the marker data to the marker stream,
  // overriding methods may use other means
  StoreData(S, Size);
  // Read the marker (default does nothing but is overridden in descendants)
  ReadMarker;
end;

class procedure TJpegMarker.PutByte(S: TCoreClassStream; B: Byte);
begin
  S.Write(B, 1);
end;

class procedure TJpegMarker.PutWord(S: TCoreClassStream; W: word);
begin
  W := Swap(W);
  S.Write(W, 2);
end;

procedure TJpegMarker.ReadMarker;
begin
  // default does nothing
end;

procedure TJpegMarker.SaveToStream(S: TCoreClassStream);
begin
  // the default SaveToStream method. If the marker was modified, the FStream was already
  // updated with .WriteMarker
  if FStream.Size > 0 then
    begin
{$IFDEF JPEG_Debug}
      DoDebugOut(Self, wsInfo, PFormat('saving marker %s, length:%d', [MarkerName.Text, FStream.Size]));
{$ENDIF JPEG_Debug}
      FStream.Position := 0;
      S.CopyFrom(FStream, FStream.Size);
    end;
end;

procedure TJpegMarker.StoreData(S: TCoreClassStream; Size: Integer);
begin
  // We store the data for later use
  FStream.Clear;
  FStream.CopyFrom(S, Size);
  FStream.Position := 0;
end;

procedure TJpegMarker.WriteMarker;
begin
  // default does nothing
end;

// Added by Dec begin
class function TJpegMarker.GetSignature: TPascalString;
begin
  Result := '';
end;

class function TJpegMarker.GetMarker: Byte;
begin
  Result := 0;
end;

class function TJpegMarker.IsSegment(Marker_: Byte; Stream_: TCoreClassStream): Boolean;
var
  S: word;
  buff: TBytes;
  Sign: TPascalString;
begin
  Result := Marker_ = GetMarker;
  if not Result then
      exit;
  Sign := GetSignature;
  if Sign = '' then
      exit;
  S := GetWord(Stream_);
  buff := Sign.ANSI;
  Result := S >= length(buff);
  if not Result then
      exit;

  Stream_.ReadBuffer(buff[0], length(buff));
  Sign.ANSI := buff;
  Result := Sign.Same(GetSignature());
end;
// Added by Dec end

{ TJpegMarkerList }

procedure TJpegMarkerList.Add(Item_: TCoreClassObject);
begin
  if not(Item_ is TJpegMarker) then
    begin
      raise Exception.Create('not a TJpegMarker');
    end;
  inherited Add(Item_);
  TJpegMarker(Item_).Owner := FOwner;
end;

function TJpegMarkerList.ByTag(MarkerTag_: Byte): TJpegMarker;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    begin
      if Items[i].MarkerTag = MarkerTag_ then
        begin
          Result := Items[i];
          exit;
        end;
    end;
end;

function TJpegMarkerList.ByClass(Class_: TJpegMarkerClass): TJpegMarker;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    begin
      if Items[i] is Class_ then
        begin
          Result := Items[i];
          exit;
        end;
    end;
end;

constructor TJpegMarkerList.Create(Owner_: TJPEG_Base_Object);
begin
  inherited Create(true);
  FOwner := Owner_;
end;

function TJpegMarkerList.GetItems(Index: Integer): TJpegMarker;
begin
  Result := TJpegMarker(inherited Items[index]);
end;

function TJpegMarkerList.HasMarker(Set_: TJpegMarkerSet): Boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 0 to Count - 1 do
    begin
      if Items[i].MarkerTag in Set_ then
        begin
          Result := true;
          exit;
        end;
    end;
end;

procedure TJpegMarkerList.InsertAfter(Set_: TJpegMarkerSet; Marker_: TJpegMarker);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    begin
      if Items[i].MarkerTag in Set_ then
        begin
          Insert(i + 1, Marker_);
          exit;
        end;
    end;

  // If none found, just add the marker
  Add(Marker_);
end;

procedure TJpegMarkerList.RemoveMarkers(Set_: TJpegMarkerSet);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    if Items[i].MarkerTag in Set_ then
        Delete(i);
end;

{ TAPPnMarker }

procedure TAPPnMarker.ReadMarker;
begin
{$IFDEF JPEG_Debug}
  DoDebugOut(Self, wsInfo, PFormat('<%s marker, length:%d>', [MarkerName.Text, FStream.Size]));
  // show first bytes as hex
  DebugSample(FStream, FStream.Size);
{$ENDIF JPEG_Debug}
end;

function TAPPnMarker.GetMarkerName: TPascalString;
begin
  Result := PFormat('APP%d', [FMarkerTag and $0F]);
end;

{ TICCProfileMarker }

class function TICCProfileMarker.GetSignature: TPascalString;
begin
  Result := 'ICC_PROFILE'#0;
end;

class function TICCProfileMarker.GetMarker: Byte;
begin
  Result := $E2;
end;

function TICCProfileMarker.GetCurrentMarker: Byte;
begin
  GetIsValid;
  Result := FCurrentMarker;
end;

function TICCProfileMarker.GetData: Pointer;
var
  PData: Pbyte;
begin
  GetIsValid;
  if not FIsValid then
      Result := nil
  else
    begin
      PData := FStream.Memory;
      inc(PData, 14);
      Result := PData;
    end;
end;

function TICCProfileMarker.GetDataLength: Integer;
begin
  GetIsValid;
  if not FIsValid then
      Result := 0
  else
      Result := FStream.Size - 14;
end;

function TICCProfileMarker.GetIsValid: Boolean;
var
  Magic: array [0 .. 11] of Byte;
begin
  Result := false;
  if FIsValid then
    begin
      Result := true;
      exit;
    end;
  FStream.Position := 0;
  FStream.Read(Magic, 12);
  FIsValid := umlCompareByteString(@Magic, 'ICC_PROFILE');
  if not FIsValid then
      exit;
  Result := true;
  FCurrentMarker := GetByte(FStream);
  FMarkerCount := GetByte(FStream);
  // ICC-Profile data follows
end;

function TICCProfileMarker.GetMarkerCount: Byte;
begin
  GetIsValid;
  Result := FMarkerCount;
end;

procedure TICCProfileMarker.SetCurrentMarker(const Value: Byte);
begin
  FStream.Position := 12;
  PutByte(FStream, Value);
  FCurrentMarker := Value;
end;

procedure TICCProfileMarker.SetDataLength(const Value: Integer);
var
  Magic: TPascalString;
begin
  FStream.Size := Value + 14;
  FStream.Position := 0;
  Magic := 'ICC_PROFILE'#0;
  FStream.WriteANSI(Magic, 12);
end;

procedure TICCProfileMarker.SetMarkerCount(const Value: Byte);
begin
  FStream.Position := 13;
  PutByte(FStream, Value);
  FMarkerCount := Value;
end;

function TICCProfileMarker.GetMarkerName: TPascalString;
begin
  Result := 'ICCProfile';
end;

{ TJpegICCProfile }

function TJpegICCProfile.GetData: Pointer;
begin
  if length(FData) > 0 then
      Result := @FData[0]
  else
      Result := nil;
end;

function TJpegICCProfile.GetDataLength: Integer;
begin
  Result := length(FData);
end;

procedure TJpegICCProfile.LoadFromFile(const FileName_: string);
var
  F: TCoreClassFileStream;
begin
  F := TCoreClassFileStream.Create(FileName_, fmOpenRead or fmShareDenyNone);
  try
      LoadFromStream(F);
  finally
      F.Free;
  end;
end;

procedure TJpegICCProfile.LoadFromStream(S: TCoreClassStream);
begin
  SetLength(FData, S.Size);
  S.Position := 0;
  S.Read(FData[0], S.Size);
end;

procedure TJpegICCProfile.ReadFromMarkerList(List_: TJpegMarkerList);
var
  i, j, DataLen, MarkerCount: Integer;
  Markers: array of TICCProfileMarker;
  M: TICCProfileMarker;
  P: Pbyte;
begin
  // Determine total length and get list of markers
  DataLen := 0;
  MarkerCount := 0;
  SetLength(Markers, List_.Count);
  for i := 0 to List_.Count - 1 do
    if List_[i] is TICCProfileMarker then
      begin
        M := TICCProfileMarker(List_[i]);
        if not M.IsValid then
            continue;
        inc(DataLen, M.DataLength);
        Markers[MarkerCount] := M;
        inc(MarkerCount);
      end;
  if DataLen <= 0 then
      exit;

  // Sort markers by index
  for i := 0 to MarkerCount - 2 do
    for j := i + 1 to MarkerCount - 1 do
      if Markers[i].CurrentMarker > Markers[j].CurrentMarker then
        begin
          M := Markers[i];
          Markers[i] := Markers[j];
          Markers[j] := M;
        end;

  // Extract marker data into our data
  SetLength(FData, DataLen);
  P := @FData[0];
  for i := 0 to MarkerCount - 1 do
    begin
      Move(Markers[i].Data^, P^, Markers[i].DataLength);
      inc(P, Markers[i].DataLength);
    end;
end;

procedure TJpegICCProfile.SaveToFile(const FileName_: string);
var
  F: TCoreClassFileStream;
begin
  F := TCoreClassFileStream.Create(FileName_, fmCreate);
  try
      SaveToStream(F);
  finally
      F.Free;
  end;
end;

procedure TJpegICCProfile.SaveToStream(S: TCoreClassStream);
begin
  if length(FData) > 0 then
      S.Write(FData[0], length(FData))
end;

procedure TJpegICCProfile.WriteToMarkerList(List_: TJpegMarkerList);
const
  cChunkSize = 60000;
var
  i, Count, Chunk, Left, Base: Integer;
  Markers: array of TICCProfileMarker;
  P: Pbyte;
begin
  // Create an array of markers with the profile data
  Count := (DataLength + cChunkSize - 1) div cChunkSize;
  Left := DataLength;
  P := Data;
  SetLength(Markers, Count);
  for i := 0 to Count - 1 do
    begin
      Markers[i] := TICCProfileMarker.Create(nil, mkAPP2);
      Chunk := IntMin(Left, cChunkSize);
      Markers[i].DataLength := Chunk;
      Move(P^, Markers[i].Data^, Chunk);
      Markers[i].CurrentMarker := i + 1;
      Markers[i].MarkerCount := Count;
      inc(P, Chunk);
      dec(Left, Chunk);
    end;
  // Insert them into the markerlist
  Base := IntMin(List_.Count, 2);
  for i := Count - 1 downto 0 do
      List_.Insert(Base, Markers[i]);
end;

function IntMin(i1, i2: Integer): Integer;
begin
  if i1 < i2 then
      Result := i1
  else
      Result := i2;
end;

function sdGetDivisor(Scale_: TJpegScale): Integer;
begin
  case Scale_ of
    jsFull: Result := 1;
    jsDiv2: Result := 2;
    jsDiv4: Result := 4;
    jsDiv8: Result := 8;
    else
      Result := 1;
  end;
end;

function CompareCardinal(C1, C2: cardinal): Integer;
begin
  if C1 < C2 then
      Result := -1
  else
    if C1 > C2 then
      Result := 1
  else
      Result := 0;
end;

function CompareInteger(Int1, Int2: Integer): Integer;
begin
  if Int1 < Int2 then
      Result := -1
  else
    if Int1 > Int2 then
      Result := 1
  else
      Result := 0;
end;

function CompareLongWord(LW1, LW2: longword): Integer;
begin
  if LW1 < LW2 then
      Result := -1
  else
    if LW1 > LW2 then
      Result := 1
  else
      Result := 0;
end;

function CompareInt64(const Int1, Int2: int64): Integer;
begin
  if Int1 < Int2 then
      Result := -1
  else
    if Int1 > Int2 then
      Result := 1
  else
      Result := 0;
end;

function ComparePointer(Item1, Item2: Pointer): Integer;
begin
  if NativeUInt(Item1) < NativeUInt(Item2) then
      Result := -1
  else if NativeUInt(Item1) > NativeUInt(Item2) then
      Result := 1
  else
      Result := 0;
end;

function CompareBool(Bool1, Bool2: Boolean): Integer;
begin
  if Bool1 < Bool2 then
      Result := -1
  else
    if Bool1 > Bool2 then
      Result := 1
  else
      Result := 0;
end;

function CompareSingle(const Single1, Single2: single): Integer;
begin
  if Single1 < Single2 then
      Result := -1
  else
    if Single1 > Single2 then
      Result := 1
  else
      Result := 0;
end;

function CompareDouble(const Double1, Double2: double): Integer;
begin
  if Double1 < Double2 then
      Result := -1
  else
    if Double1 > Double2 then
      Result := 1
  else
      Result := 0;
end;

function CompareGuid(const Guid1, Guid2: TGuid): Integer;
var
  i: Integer;
  a, B: PCardinal;
begin
  a := PCardinal(@Guid1);
  B := PCardinal(@Guid2);
  i := 0;
  Result := CompareCardinal(a^, B^);
  while (Result = 0) and (i < 3) do
    begin
      inc(i);
      inc(a);
      inc(B);
      Result := CompareCardinal(a^, B^);
    end;
end;

function IsEqualGuid(const Guid1, Guid2: TGuid): Boolean;
begin
  Result := CompareGuid(Guid1, Guid2) = 0;
end;

function IsEmptyGuid(const GUID_: TGuid): Boolean;
begin
  Result := CompareGuid(GUID_, cEmptyGuid) = 0;
end;

function NewGuid: TGuid;
var
  Guid: TGuid;
begin
  CreateGUID(Guid);
  Result := Guid;
end;

// For use with custom sorter and procedures

function ComparePSingle(Ptr1, Ptr2: Pointer): Integer;
begin
  Result := CompareSingle(PSingle(Ptr1)^, PSingle(Ptr2)^);
end;

function ComparePDouble(Ptr1, Ptr2: Pointer): Integer;
begin
  Result := CompareDouble(PDouble(Ptr1)^, PDouble(Ptr2)^);
end;

function ComparePInteger(Ptr1, Ptr2: Pointer): Integer;
begin
  Result := CompareInteger(PInteger(Ptr1)^, PInteger(Ptr2)^);
end;

{ TCustomObjectList }

procedure TCustomObjectList.Append(Item_: TObject);
begin
  Insert(Count, Item_);
end;

{ TLuidList }

function TLuidList.Add(Item_: TObject): Integer;
begin
  // do we have Item_?
  if IndexByLuid(GetLuid(Item_), Result) then
      inherited Items[Result] := Item_
  else
    begin
      // Insert
      Insert(Result, Item_);
    end;
end;

function TLuidList.HasLuid(const LUID_: Integer): Boolean;
var
  Index: Integer;
begin
  Result := IndexByLuid(LUID_, Index);
end;

function TLuidList.IndexByLuid(const LUID_: Integer;
  out Index: Integer): Boolean;
var
  Min, Max: Integer;
begin
  Result := false;

  // Find position for insert - binary method
  Index := 0;
  Min := 0;
  Max := Count;
  while Min < Max do
    begin
      Index := (Min + Max) div 2;
      case CompareInteger(GetLuid(ListData^[Index]), LUID_) of
        - 1: Min := Index + 1;
        0: begin
            Result := true;
            exit;
          end;
        1: Max := Index;
      end;
    end;
  Index := Min;
end;

function TLuidList.NextLuid: Integer;
begin
  if Count = 0 then
      Result := 1
  else
      Result := GetLuid(ListData^[Count - 1]) + 1;
end;

procedure TLuidList.RemoveByLuid(const LUID_: Integer);
var
  Index: Integer;
begin
  if IndexByLuid(LUID_, Index) then
      Delete(Index);
end;

{ TGuidList }

function TGuidList.Add(Item_: TObject): Integer;
begin
  // do we have Item_?
  if IndexByGuid(GetGuid(Item_), Result) then
    // Replace existing
      inherited Items[Result] := Item_
  else
    begin
      // Insert
      Insert(Result, Item_);
    end;
end;

function TGuidList.HasGuid(const GUID_: TGuid): Boolean;
var
  Index: Integer;
begin
  Result := IndexByGuid(GUID_, Index);
end;

function TGuidList.IndexByGuid(const GUID_: TGuid;
  out Index: Integer): Boolean;
var
  Min, Max: Integer;
begin
  Result := false;

  // Find position for insert - binary method
  Index := 0;
  Min := 0;
  Max := Count;
  while Min < Max do
    begin
      Index := (Min + Max) div 2;
      case CompareGuid(GetGuid(ListData^[Index]), GUID_) of
        - 1: Min := Index + 1;
        0: begin
            Result := true;
            exit;
          end;
        1: Max := Index;
      end;
    end;
  Index := Min;
end;

procedure TGuidList.RemoveByGuid(const GUID_: TGuid);
var
  Index: Integer;
begin
  if IndexByGuid(GUID_, Index) then
      Delete(Index);
end;

{ TCustomSortedList }

function TCustomSortedList.Add(Item_: TObject): Integer;
begin
  if Sorted then
    begin

      Find(Item_, Result);
      Insert(Result, Item_);

    end
  else

      Result := inherited Add(Item_);
end;

function TCustomSortedList.AddUnique(Item: TObject; RaiseError: Boolean): Integer;
begin
  if Find(Item, Result) then
    begin
      if RaiseError then
          raise Exception.Create(sAddingNonUniqueObject);
      Delete(Result);
    end;
  Insert(Result, Item);
end;

constructor TCustomSortedList.Create(OwnsObjects_: Boolean);
begin
  inherited Create(OwnsObjects_);
  FSorted := true;
end;

function TCustomSortedList.DoCompare(Item1, Item2: TObject): Integer;
begin
  Result := ComparePointer(Item1, Item2);
end;

function TCustomSortedList.Find(Item: TObject; out Index: Integer): Boolean;
var
  AMin, AMax: Integer;
begin
  Result := false;

  if Sorted then
    begin

      // Find position for insert - binary method
      Index := 0;
      AMin := 0;
      AMax := Count;
      while AMin < AMax do
        begin
          Index := (AMin + AMax) div 2;
          case DoCompare(ListData^[Index], Item) of
            - 1: AMin := Index + 1;
            0: begin
                Result := true;
                exit;
              end;
            1: AMax := Index;
          end;
        end;
      Index := AMin;

    end
  else
    begin

      // If not a sorted list, then find it with the IndexOf() method
      Index := IndexOf(Item);
      if Index >= 0 then
        begin
          Result := true;
          exit;
        end;

      // Not found: set it to Count
      Index := Count;
    end;
end;

procedure TCustomSortedList.FindMultiple(Item: TObject; out AIndex, Count_: Integer);
var
  IdxStart: Integer;
  IdxClose: Integer;
begin
  if not Sorted then
      raise Exception.Create(sListMustBeSorted);

  Count_ := 0;

  // Find one
  if not Find(Item, AIndex) then
      exit;

  // Check upward from item
  IdxStart := AIndex;
  while (IdxStart > 0) and (DoCompare(ListData^[IdxStart - 1], Item) = 0) do
      dec(IdxStart);

  // Check downward from item
  IdxClose := AIndex;
  while (IdxClose < Count - 1) and (DoCompare(ListData^[IdxClose + 1], Item) = 0) do
      inc(IdxClose);

  // Result
  AIndex := IdxStart;
  Count_ := IdxClose - IdxStart + 1;
end;

procedure TCustomSortedList.SetSorted(Value_: Boolean);
begin
  if Value_ <> FSorted then
    begin
      FSorted := Value_;
      if FSorted then
          Sort;
    end;
end;

procedure TCustomSortedList.Sort;
// local
  procedure QuickSort(iLo, iHi: Integer);
  var
    Lo, Hi, Mid: Integer;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := (Lo + Hi) div 2;
    repeat
      while DoCompare(ListData^[Lo], ListData^[Mid]) < 0 do
          inc(Lo);
      while DoCompare(ListData^[Hi], ListData^[Mid]) > 0 do
          dec(Hi);
      if Lo <= Hi then
        begin
          Exchange(Lo, Hi);
          if Mid = Lo then
              Mid := Hi
          else
            if Mid = Hi then
              Mid := Lo;
          inc(Lo);
          dec(Hi);
        end;
    until Lo > Hi;

    if Hi > iLo then
        QuickSort(iLo, Hi);

    if Lo < iHi then
        QuickSort(Lo, iHi);
  end;

// main
begin
  if Count > 1 then
    begin
      QuickSort(0, Count - 1);
    end;
  FSorted := true;
end;

{ TCustomSorter }

procedure TCustomSorter.Sort;
var
  Buf: array of Byte;
  PB: Pbyte;
  // local
  function DoCompare(Idx1, Idx2: Integer): Integer;
  var
    P1, P2: Pbyte;
  begin
    P1 := First;
    inc(P1, FStride * Idx1);
    P2 := First;
    inc(P2, FStride * Idx2);
    Result := FCompareMethod(P1, P2);
  end;
// local
  procedure Exchange(Idx1, Idx2: Integer);
  var
    P1, P2: Pbyte;
  begin
    P1 := First;
    inc(P1, FStride * Idx1);
    P2 := First;
    inc(P2, FStride * Idx2);
    Move(P1^, PB^, Stride);
    Move(P2^, P1^, Stride);
    Move(PB^, P2^, Stride);
  end;
// local
  procedure QuickSort(iLo, iHi: Integer);
  var
    Lo, Hi, Mid: longint;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := (Lo + Hi) div 2;
    repeat
      while DoCompare(Lo, Mid) < 0 do
          inc(Lo);
      while DoCompare(Hi, Mid) > 0 do
          dec(Hi);
      if Lo <= Hi then
        begin
          // Swap pointers;
          Exchange(Lo, Hi);
          if Mid = Lo then
              Mid := Hi
          else
            if Mid = Hi then
              Mid := Lo;
          inc(Lo);
          dec(Hi);
        end;
    until Lo > Hi;

    if Hi > iLo then
        QuickSort(iLo, Hi);

    if Lo < iHi then
        QuickSort(Lo, iHi);
  end;

// main
begin
  if Count > 1 then
    begin
      SetLength(Buf, Stride);
      PB := @Buf[0];
      QuickSort(0, Count - 1);
    end;
end;

procedure sdSortArraySingle(First_: PSingle; Count_: Integer);
var
  Sorter: TCustomSorter;
begin
  Sorter := TCustomSorter.Create;
  try
    Sorter.CompareMethod := {$IFDEF FPC}@{$ENDIF FPC}ComparePSingle;
    Sorter.First := First_;
    Sorter.Stride := SizeOf(single);
    Sorter.Count := Count_;
    Sorter.Sort;
  finally
      Sorter.Free;
  end;
end;

procedure sdSortArrayDouble(First_: PDouble; Count_: Integer);
var
  Sorter: TCustomSorter;
begin
  Sorter := TCustomSorter.Create;
  try
    Sorter.CompareMethod := {$IFDEF FPC}@{$ENDIF FPC}ComparePDouble;
    Sorter.First := First_;
    Sorter.Stride := SizeOf(double);
    Sorter.Count := Count_;
    Sorter.Sort;
  finally
      Sorter.Free;
  end;
end;

procedure sdSortArrayInteger(First_: PInteger; Count_: Integer);
var
  Sorter: TCustomSorter;
begin
  Sorter := TCustomSorter.Create;
  try
    Sorter.CompareMethod := {$IFDEF FPC}@{$ENDIF FPC}ComparePInteger;
    Sorter.First := First_;
    Sorter.Stride := SizeOf(Integer);
    Sorter.Count := Count_;
    Sorter.Sort;
  finally
      Sorter.Free;
  end;
end;

function sdAverageOfArrayInteger(First_: PInteger; Count_: Integer): double;
var
  i: Integer;
  Total: int64;
begin
  Total := 0;
  for i := 0 to Count_ - 1 do
    begin
      inc(Total, First_^);
      inc(First_);
    end;

  Result := Total / Count_;
end;

function sdMinimumOfArrayInteger(First_: PInteger; Count_: Integer): Integer;
begin
  if Count_ = 0 then
    begin
      Result := 0;
      exit;
    end;

  Result := First_^;

  while Count_ > 0 do
    begin
      if First_^ < Result then
          Result := First_^;
      dec(Count_);
      inc(First_);
    end;
end;

function sdMaximumOfArrayInteger(First_: PInteger; Count_: Integer): Integer;
begin
  if Count_ = 0 then
    begin
      Result := 0;
      exit;
    end;

  Result := First_^;

  while Count_ > 0 do
    begin
      if First_^ > Result then
          Result := First_^;
      dec(Count_);
      inc(First_);
    end;
end;

function sdMinimumOfArrayDouble(First_: PDouble; Count_: Integer): double;
begin
  if Count_ = 0 then
    begin
      Result := 0;
      exit;
    end;

  Result := First_^;

  while Count_ > 0 do
    begin
      if First_^ > Result then
          Result := First_^;
      dec(Count_);
      inc(First_);
    end;
end;

function sdMaximumOfArrayDouble(First_: PDouble; Count_: Integer): double;
begin
  if Count_ = 0 then
    begin
      Result := 0;
      exit;
    end;

  Result := First_^;

  while Count_ > 0 do
    begin
      if First_^ > Result then
          Result := First_^;
      dec(Count_);
      inc(First_);
    end;
end;

procedure sdWalkingAverageArrayInteger(SFirst, DFirst: PInteger; Count_, Center_, WindowSize_: Integer);
var
  i: Integer;
  SLast: PInteger;
  Cum: Integer; // cumulative
begin
  // Only process if we have enough values
  if (Count_ < WindowSize_) or (WindowSize_ < Center_) or (WindowSize_ <= 0) then
      exit;

  // Start area
  SLast := SFirst;

  // Initialize cumulative
  Cum := SLast^ * Center_;

  // Collect values into window
  for i := 0 to (WindowSize_ - Center_) - 1 do
    begin
      Cum := Cum + SFirst^;
      inc(SFirst);
    end;

  // Do first part
  for i := 0 to Center_ - 1 do
    begin
      DFirst^ := Cum;
      inc(DFirst);
      Cum := Cum + SFirst^;
      Cum := Cum - SLast^;
      inc(SFirst);
    end;

  // Bulk
  for i := 0 to Count_ - WindowSize_ - 1 do
    begin
      DFirst^ := Cum;
      inc(DFirst);
      inc(Cum, SFirst^);
      dec(Cum, SLast^);
      inc(SFirst);
      inc(SLast);
    end;

  // make sure we're at the last element, not one beyond
  dec(SFirst);

  // Close area
  for i := Center_ to WindowSize_ - 1 do
    begin
      DFirst^ := Cum;
      inc(DFirst);
      Cum := Cum + SFirst^;
      Cum := Cum - SLast^;
      inc(SLast);
    end;
end;

procedure sdWalkingAverageArrayDouble(SFirst, DFirst: PDouble; Count_, Center_, WindowSize_: Integer);
var
  i: Integer;
  SLast: PDouble;
  Cum, Scale: double;
begin
  // Only process if we have enough values
  if (Count_ < WindowSize_) or (WindowSize_ < Center_) or (WindowSize_ <= 0) then
      exit;

  // Start area
  Scale := 1 / WindowSize_;
  SLast := SFirst;

  // Initialize cumulative
  Cum := SLast^ * Center_;

  // Collect values into window
  for i := 0 to (WindowSize_ - Center_) - 1 do
    begin
      Cum := Cum + SFirst^;
      inc(SFirst);
    end;

  // Do first part
  for i := 0 to Center_ - 1 do
    begin
      DFirst^ := Cum * Scale;
      inc(DFirst);
      Cum := Cum + SFirst^;
      Cum := Cum - SLast^;
      inc(SFirst);
    end;

  // Bulk
  for i := 0 to Count_ - WindowSize_ - 1 do
    begin
      DFirst^ := Cum * Scale;
      inc(DFirst);
      Cum := Cum + SFirst^;
      Cum := Cum - SLast^;
      inc(SFirst);
      inc(SLast);
    end;

  // make sure we're at the last element, not one beyond
  dec(SFirst);

  // Close area
  for i := Center_ to WindowSize_ - 1 do
    begin
      DFirst^ := Cum * Scale;
      inc(DFirst);
      Cum := Cum + SFirst^;
      Cum := Cum - SLast^;
      inc(SLast);
    end;
end;

procedure sdWalkingMedianArrayInteger(SFirst, DFirst: PInteger; Count_, Center_, WindowSize_: Integer);
var
  W, Ws: array of Integer;
  WsCenter, WEnd: PInteger;
  i, WSize, WM1Size: Integer;
  Sorter: TCustomSorter;
  // local, add SFirst^ value to the unsorted array
  procedure AddToCumulative;
  begin
    Move(W[1], W[0], WM1Size);
    WEnd^ := SFirst^;
  end;
// local, this sorts the values, and gets the median
  function GetCumulative: Integer;
  begin
    Move(W[0], Ws[0], WSize);
    Sorter.Sort;
    Result := WsCenter^;
  end;

// main
begin
  // Only process if we have enough values
  if (Count_ < WindowSize_) or (WindowSize_ < Center_) or (WindowSize_ <= 0) then
      exit;

  // Initialization
  SetLength(W, WindowSize_);
  SetLength(Ws, WindowSize_);
  WsCenter := @Ws[Center_];
  WEnd := @W[WindowSize_ - 1];
  WSize := WindowSize_ * SizeOf(Integer);
  WM1Size := (WindowSize_ - 1) * SizeOf(Integer);

  Sorter := TCustomSorter.Create;
  try
    Sorter.CompareMethod := {$IFDEF FPC}@{$ENDIF FPC}ComparePInteger;
    Sorter.First := @Ws[0];
    Sorter.Stride := SizeOf(Integer);
    Sorter.Count := WindowSize_;

    // Initialize cumulative
    for i := 0 to Center_ - 1 do
        AddToCumulative;

    // Collect values into window
    for i := 0 to (WindowSize_ - Center_) - 1 do
      begin
        AddToCumulative;
        inc(SFirst);
      end;

    // Do first part
    for i := 0 to Center_ - 1 do
      begin
        DFirst^ := GetCumulative;
        inc(DFirst);
        AddToCumulative;
        inc(SFirst);
      end;

    // Bulk
    for i := 0 to Count_ - WindowSize_ - 1 do
      begin
        DFirst^ := GetCumulative;
        inc(DFirst);
        AddToCumulative;
        inc(SFirst);
      end;

    // make sure we're at the last element, not one beyond
    dec(SFirst);

    // Close area
    for i := Center_ to WindowSize_ - 1 do
      begin
        DFirst^ := GetCumulative;
        inc(DFirst);
        AddToCumulative;
      end;

  finally
      Sorter.Free;
  end;
end;

procedure sdWalkingMedianArrayDouble(SFirst, DFirst: PDouble; Count_, Center_, WindowSize_: Integer);
var
  W, Ws: array of double;
  WsCenter, WEnd: PDouble;
  i, WSize, WM1Size: Integer;
  Sorter: TCustomSorter;
  // local, add SFirst^ value to the unsorted array
  procedure AddToCumulative;
  begin
    Move(W[1], W[0], WM1Size);
    WEnd^ := SFirst^;
  end;
// local, this sorts the values, and gets the median
  function GetCumulative: double;
  begin
    Move(W[0], Ws[0], WSize);
    Sorter.Sort;
    Result := WsCenter^;
  end;

// main
begin
  // Only process if we have enough values
  if (Count_ < WindowSize_) or (WindowSize_ < Center_) or (WindowSize_ <= 0) then
      exit;

  // Initialization
  SetLength(W, WindowSize_);
  SetLength(Ws, WindowSize_);
  WsCenter := @Ws[Center_];
  WEnd := @W[WindowSize_ - 1];
  WSize := WindowSize_ * SizeOf(double);
  WM1Size := (WindowSize_ - 1) * SizeOf(double);

  Sorter := TCustomSorter.Create;
  try
    Sorter.CompareMethod := {$IFDEF FPC}@{$ENDIF FPC}ComparePDouble;
    Sorter.First := @Ws[0];
    Sorter.Stride := SizeOf(double);
    Sorter.Count := WindowSize_;

    // Initialize cumulative
    for i := 0 to Center_ - 1 do
        AddToCumulative;

    // Collect values into window
    for i := 0 to (WindowSize_ - Center_) - 1 do
      begin
        AddToCumulative;
        inc(SFirst);
      end;

    // Do first part
    for i := 0 to Center_ - 1 do
      begin
        DFirst^ := GetCumulative;
        inc(DFirst);
        AddToCumulative;
        inc(SFirst);
      end;

    // Bulk
    for i := 0 to Count_ - WindowSize_ - 1 do
      begin
        DFirst^ := GetCumulative;
        inc(DFirst);
        AddToCumulative;
        inc(SFirst);
      end;

    // make sure we're at the last element, not one beyond
    dec(SFirst);

    // Close area
    for i := Center_ to WindowSize_ - 1 do
      begin
        DFirst^ := GetCumulative;
        inc(DFirst);
        AddToCumulative;
      end;

  finally
      Sorter.Free;
  end;
end;

end.
