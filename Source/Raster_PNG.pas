{ ****************************************************************************** }
{ * memory Rasterization Portable Network Graphic support                      * }
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
unit Raster_PNG;

{$INCLUDE zDefine.inc}

{$IFDEF DEBUG}
{$DEFINE ValidateEveryReadOperation}
{$DEFINE CheckCRC}
{$ELSE DEBUG}
{$UNDEF ValidateEveryReadOperation}
{$UNDEF CheckCRC}
{$ENDIF DEBUG}

interface

uses CoreClasses, PascalStrings, UnicodeMixedLib, MemoryStream64, MemoryRaster, Geometry2DUnit;

function IsPNG(const Stream: TCoreClassStream): Boolean;
procedure LoadRasterFromPNG(raster: TMemoryRaster; Stream: TCoreClassStream);
procedure SaveRasterToPNG(raster: TMemoryRaster; Stream: TCoreClassStream);

implementation

uses
  SysUtils,
{$IFDEF FPC}
  zbase,
  zdeflate,
  zinflate,
{$ELSE FPC}
  ZLib,
{$ENDIF FPC}
  Math;

type
  TPNGByteArray = array [0 .. MaxInt div SizeOf(Byte) - 1] of Byte;
  PPNGByteArray = ^TPNGByteArray;
  TColorType = Byte;
  TFilterMethod = Byte;
  TAdaptiveFilterMethod = Byte;
  TAvailableAdaptiveFilterMethod = (aafmSub, aafmUp, aafmAverage, aafmPaeth);
  TAvailableAdaptiveFilterMethods = set of TAvailableAdaptiveFilterMethod;
  TInterlaceMethod = Byte;

  TRGB24 = packed record
    R, G, B: Byte;
  end;

  PRGB24 = ^TRGB24;
  TRGB24Array = array [0 .. MaxInt div SizeOf(TRGB24) - 1] of TRGB24;
  PRGB24Array = ^TRGB24Array;

  TRGB24Word = packed record
    R, G, B: Word;
  end;

  PRGB24Word = ^TRGB24Word;

  TRGB32 = packed record
    R, G, B, A: Byte;
  end;

  PRGB32 = ^TRGB32;

  TRGB32Word = packed record
    R, G, B, A: Word;
  end;

  PRGB32Word = ^TRGB32Word;

  TChunkName = array [0 .. 3] of Byte;
  PChunkName = ^TChunkName;

  EPngError = class(Exception);

{$IFDEF FPC}
  TZStreamRec = z_stream;
{$ENDIF}

  TCustomChunk = class(TCoreClassPersistent)
  protected
    function GetChunkName: TChunkName; virtual; abstract;
    function GetChunkSize: Cardinal; virtual; abstract;
  public
    procedure ReadFromStream(Stream: TCoreClassStream; ChunkSize: Cardinal); virtual;
    procedure WriteToStream(Stream: TCoreClassStream); virtual;

    property ChunkName: TChunkName read GetChunkName;
    property ChunkSize: Cardinal read GetChunkSize;
  end;

  TCustomDefinedChunk = class(TCustomChunk)
  protected
    function GetChunkName: TChunkName; override;
    class function GetClassChunkName: TChunkName; virtual; abstract;
  public
    property ChunkName: TChunkName read GetClassChunkName;
  end;

  TCustomDefinedChunkClass = class of TCustomDefinedChunk;

  TChunkPngImageHeader = class(TCustomDefinedChunk)
  private
    FWidth: TGeoInt;
    FHeight: TGeoInt;
    FBitDepth: Byte;
    FColorType: TColorType;
    FCompressionMethod: Byte;
    FFilterMethod: TFilterMethod;
    FInterlaceMethod: TInterlaceMethod;
    FAdaptiveFilterMethods: TAvailableAdaptiveFilterMethods;
    function GetHasPalette: Boolean;
    function GetBytesPerRow: TGeoInt;
    function GetPixelByteSize: TGeoInt;
    procedure SetCompressionMethod(const Value: Byte);
    procedure SetFilterMethod(const Value: TFilterMethod);
    procedure SetAdaptiveFilterMethods(const Value: TAvailableAdaptiveFilterMethods);
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    constructor Create; virtual;

    procedure ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    procedure ResetToDefault; virtual;

    property Width: TGeoInt read FWidth write FWidth;
    property Height: TGeoInt read FHeight write FHeight;
    property BitDepth: Byte read FBitDepth write FBitDepth;
    property ColorType: TColorType read FColorType write FColorType;
    property CompressionMethod: Byte read FCompressionMethod write SetCompressionMethod;
    property AdaptiveFilterMethods: TAvailableAdaptiveFilterMethods read FAdaptiveFilterMethods write SetAdaptiveFilterMethods;
    property FilterMethod: TFilterMethod read FFilterMethod write SetFilterMethod;
    property InterlaceMethod: TInterlaceMethod read FInterlaceMethod write FInterlaceMethod;
    property HasPalette: Boolean read GetHasPalette;

    property BytesPerRow: TGeoInt read GetBytesPerRow;
    property PixelByteSize: TGeoInt read GetPixelByteSize;
  end;

  TCustomDefinedChunkWithHeader = class(TCustomDefinedChunk)
  protected
    FHeader: TChunkPngImageHeader;

    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    constructor Create(Header_: TChunkPngImageHeader); reintroduce; virtual;

    procedure HeaderChanged; virtual;

    property Header: TChunkPngImageHeader read FHeader;
  end;

  TCustomDefinedChunkWithHeaderClass = class of TCustomDefinedChunkWithHeader;

  TChunkPngImageData = class(TCustomDefinedChunkWithHeader)
  private
    FData: TMemoryStream64;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    constructor Create(Header_: TChunkPngImageHeader); override;
    destructor Destroy; override;

    procedure ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property Data: TMemoryStream64 read FData;
  end;

  TChunkPngPalette = class(TCustomDefinedChunkWithHeader)
  private
    FPaletteEntries: array of TRGB24;
    function GetPaletteEntry(Index: Cardinal): TRGB24;
    function GetCount: TGeoInt;
    procedure SetCount(const Value: TGeoInt);
    procedure SetPaletteEntry(Index: Cardinal; const Value: TRGB24);
  protected
    procedure AssignTo(Dest: TCoreClassPersistent); override;
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;
    procedure PaletteEntriesChanged; virtual;
  public
    procedure ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property PaletteEntry[Index: Cardinal]: TRGB24 read GetPaletteEntry write SetPaletteEntry; default;
    property Count: TGeoInt read GetCount write SetCount;
  end;

  TChunkPngGamma = class(TCustomDefinedChunkWithHeader)
  private
    FGamma: Cardinal;
    function GetGammaAsSingle: TGeoFloat;
    procedure SetGammaAsSingle(const Value: TGeoFloat);
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    procedure ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property Gamma: Cardinal read FGamma write FGamma;
    property GammaAsSingle: TGeoFloat read GetGammaAsSingle write SetGammaAsSingle;
  end;

  TChunkPngStandardColorSpaceRGB = class(TCustomDefinedChunkWithHeader)
  private
    FRenderingIntent: Byte;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    procedure ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property RenderingIntent: Byte read FRenderingIntent write FRenderingIntent;
  end;

  TChunkPngPrimaryChromaticities = class(TCustomDefinedChunkWithHeader)
  private
    FWhiteX: TGeoInt;
    FWhiteY: TGeoInt;
    FRedX: TGeoInt;
    FRedY: TGeoInt;
    FGreenX: TGeoInt;
    FGreenY: TGeoInt;
    FBlueX: TGeoInt;
    FBlueY: TGeoInt;
    function GetBlueX: TGeoFloat;
    function GetBlueY: TGeoFloat;
    function GetGreenX: TGeoFloat;
    function GetGreenY: TGeoFloat;
    function GetRedX: TGeoFloat;
    function GetRedY: TGeoFloat;
    function GetWhiteX: TGeoFloat;
    function GetWhiteY: TGeoFloat;
    procedure SetBlueX(const Value: TGeoFloat);
    procedure SetBlueY(const Value: TGeoFloat);
    procedure SetGreenX(const Value: TGeoFloat);
    procedure SetGreenY(const Value: TGeoFloat);
    procedure SetRedX(const Value: TGeoFloat);
    procedure SetRedY(const Value: TGeoFloat);
    procedure SetWhiteX(const Value: TGeoFloat);
    procedure SetWhiteY(const Value: TGeoFloat);
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    procedure ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property WhiteX: TGeoInt read FWhiteX write FWhiteX;
    property WhiteY: TGeoInt read FWhiteY write FWhiteY;
    property RedX: TGeoInt read FRedX write FRedX;
    property RedY: TGeoInt read FRedY write FRedY;
    property GreenX: TGeoInt read FGreenX write FGreenX;
    property GreenY: TGeoInt read FGreenY write FGreenY;
    property BlueX: TGeoInt read FBlueX write FBlueX;
    property BlueY: TGeoInt read FBlueY write FBlueY;

    property WhiteXAsSingle: TGeoFloat read GetWhiteX write SetWhiteX;
    property WhiteYAsSingle: TGeoFloat read GetWhiteY write SetWhiteY;
    property RedXAsSingle: TGeoFloat read GetRedX write SetRedX;
    property RedYAsSingle: TGeoFloat read GetRedY write SetRedY;
    property GreenXAsSingle: TGeoFloat read GetGreenX write SetGreenX;
    property GreenYAsSingle: TGeoFloat read GetGreenY write SetGreenY;
    property BlueXAsSingle: TGeoFloat read GetBlueX write SetBlueX;
    property BlueYAsSingle: TGeoFloat read GetBlueY write SetBlueY;
  end;

  TChunkPngTime = class(TCustomDefinedChunkWithHeader)
  private
    FYear: Word;
    FMonth: Byte;
    FDay: Byte;
    FHour: Byte;
    FMinute: Byte;
    FSecond: Byte;
    function GetModifiedDateTime: TDateTime;
    procedure SetModifiedDateTime(const Value: TDateTime);
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    procedure ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property Year: Word read FYear write FYear;
    property Month: Byte read FMonth write FMonth;
    property Day: Byte read FDay write FDay;
    property Hour: Byte read FHour write FHour;
    property Minute: Byte read FMinute write FMinute;
    property Second: Byte read FSecond write FSecond;
    property ModifiedDateTime: TDateTime read GetModifiedDateTime write SetModifiedDateTime;
  end;

  TChunkPngEmbeddedIccProfile = class(TCustomDefinedChunkWithHeader)
  private
    FProfileName: TPascalString;
    FCompressionMethod: Byte;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    procedure ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property ProfileName: TPascalString read FProfileName write FProfileName;
    property CompressionMethod: Byte read FCompressionMethod write FCompressionMethod;
  end;

  TCustomPngSignificantBits = class(TCoreClassPersistent)
  protected
    class function GetChunkSize: Cardinal; virtual; abstract;
  public
    constructor Create(BitDepth: TGeoInt = 8); virtual;

    procedure ReadFromStream(Stream: TCoreClassStream); virtual; abstract;
    procedure WriteToStream(Stream: TCoreClassStream); virtual; abstract;

    property ChunkSize_: Cardinal read GetChunkSize;
  end;

  TPngSignificantBitsFormat0 = class(TCustomPngSignificantBits)
  private
    FGrayBits: Byte;
  protected
    class function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    constructor Create(BitDepth: TGeoInt = 8); override;

    procedure ReadFromStream(Stream: TCoreClassStream); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property GrayBits: Byte read FGrayBits write FGrayBits;
  end;

  TPngSignificantBitsFormat23 = class(TCustomPngSignificantBits)
  private
    FRedBits: Byte;
    FBlueBits: Byte;
    FGreenBits: Byte;
  protected
    class function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    constructor Create(BitDepth: TGeoInt = 8); override;

    procedure ReadFromStream(Stream: TCoreClassStream); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property RedBits: Byte read FRedBits write FRedBits;
    property BlueBits: Byte read FBlueBits write FBlueBits;
    property GreenBits: Byte read FGreenBits write FGreenBits;
  end;

  TPngSignificantBitsFormat4 = class(TCustomPngSignificantBits)
  private
    FGrayBits: Byte;
    FAlphaBits: Byte;
  protected
    class function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    constructor Create(BitDepth: TGeoInt = 8); override;

    procedure ReadFromStream(Stream: TCoreClassStream); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property GrayBits: Byte read FGrayBits write FGrayBits;
    property AlphaBits: Byte read FAlphaBits write FAlphaBits;
  end;

  TPngSignificantBitsFormat6 = class(TCustomPngSignificantBits)
  private
    FRedBits: Byte;
    FBlueBits: Byte;
    FGreenBits: Byte;
    FAlphaBits: Byte;
  protected
    class function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    constructor Create(BitDepth: TGeoInt = 8); override;

    procedure ReadFromStream(Stream: TCoreClassStream); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property RedBits: Byte read FRedBits write FRedBits;
    property BlueBits: Byte read FBlueBits write FBlueBits;
    property GreenBits: Byte read FGreenBits write FGreenBits;
    property AlphaBits: Byte read FAlphaBits write FAlphaBits;
  end;

  TChunkPngSignificantBits = class(TCustomDefinedChunkWithHeader)
  private
    FSignificantBits: TCustomPngSignificantBits;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    constructor Create(Header_: TChunkPngImageHeader); override;
    destructor Destroy; override;

    procedure ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    procedure HeaderChanged; override;

    property SignificantBits: TCustomPngSignificantBits read FSignificantBits;
  end;

  TCustomPngBackgroundColor = class(TCoreClassPersistent)
  protected
    class function GetChunkSize: Cardinal; virtual; abstract;
  public
    procedure ReadFromStream(Stream: TCoreClassStream); virtual; abstract;
    procedure WriteToStream(Stream: TCoreClassStream); virtual; abstract;

    property ChunkSize_: Cardinal read GetChunkSize;
  end;

  TPngBackgroundColorFormat04 = class(TCustomPngBackgroundColor)
  private
    FGraySampleValue: Word;
  protected
    class function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    procedure ReadFromStream(Stream: TCoreClassStream); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property GraySampleValue: Word read FGraySampleValue write FGraySampleValue;
  end;

  TPngBackgroundColorFormat26 = class(TCustomPngBackgroundColor)
  private
    FRedSampleValue: Word;
    FBlueSampleValue: Word;
    FGreenSampleValue: Word;
  protected
    class function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    procedure ReadFromStream(Stream: TCoreClassStream); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property RedSampleValue: Word read FRedSampleValue write FRedSampleValue;
    property BlueSampleValue: Word read FBlueSampleValue write FBlueSampleValue;
    property GreenSampleValue: Word read FGreenSampleValue write FGreenSampleValue;
  end;

  TPngBackgroundColorFormat3 = class(TCustomPngBackgroundColor)
  private
    FIndex: Byte;
  protected
    class function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    procedure ReadFromStream(Stream: TCoreClassStream); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property PaletteIndex: Byte read FIndex write FIndex;
  end;

  TChunkPngBackgroundColor = class(TCustomDefinedChunkWithHeader)
  private
    FBackground: TCustomPngBackgroundColor;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    constructor Create(Header_: TChunkPngImageHeader); override;
    destructor Destroy; override;

    procedure ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    procedure HeaderChanged; override;

    property Background: TCustomPngBackgroundColor read FBackground;
  end;

  TChunkPngImageHistogram = class(TCustomDefinedChunkWithHeader)
  private
    FHistogram: array of Word;
    function GetCount: TGeoInt;
    function GetFrequency(Index: Cardinal): Word;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;
  public
    procedure ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property Count: TGeoInt read GetCount;
    property Frequency[Index: Cardinal]: Word read GetFrequency;
  end;

  TSuggestedPalette8ByteEntry = record
    Red: Byte;
    Green: Byte;
    Blue: Byte;
    Alpha: Byte;
    Frequency: Word;
  end;

  PSuggestedPalette8ByteEntry = ^TSuggestedPalette8ByteEntry;
  TSuggestedPalette8ByteArray = array [0 .. 0] of TSuggestedPalette8ByteEntry;
  PSuggestedPalette8ByteArray = ^TSuggestedPalette8ByteArray;

  TSuggestedPalette16ByteEntry = record
    Red: Word;
    Green: Word;
    Blue: Word;
    Alpha: Word;
    Frequency: Word;
  end;

  PSuggestedPalette16ByteEntry = ^TSuggestedPalette16ByteEntry;
  TSuggestedPalette16ByteArray = array [0 .. 0] of TSuggestedPalette16ByteEntry;
  PSuggestedPalette16ByteArray = ^TSuggestedPalette16ByteArray;

  TChunkPngSuggestedPalette = class(TCustomDefinedChunkWithHeader)
  private
    FPaletteName: TPascalString;
    FData: Pointer;
    FCount: TGeoInt;
    FSampleDepth: Byte;
    function GetCount: TGeoInt;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;
  public
    constructor Create(Header_: TChunkPngImageHeader); override;

    procedure ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property Count: TGeoInt read GetCount;
  end;

  TCustomPngTransparency = class(TCoreClassPersistent)
  protected
    function GetChunkSize: Cardinal; virtual; abstract;
  public
    procedure ReadFromStream(Stream: TCoreClassStream); virtual;
    procedure WriteToStream(Stream: TCoreClassStream); virtual;

    property ChunkSize_: Cardinal read GetChunkSize;
  end;

  TPngTransparencyFormat0 = class(TCustomPngTransparency)
  private
    FGraySampleValue: Word;
  protected
    procedure AssignTo(Dest: TCoreClassPersistent); override;
    function GetChunkSize: Cardinal; override;
  public
    procedure ReadFromStream(Stream: TCoreClassStream); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property GraySampleValue: Word read FGraySampleValue write FGraySampleValue;
  end;

  TPngTransparencyFormat2 = class(TCustomPngTransparency)
  private
    FRedSampleValue: Word;
    FBlueSampleValue: Word;
    FGreenSampleValue: Word;
  protected
    procedure AssignTo(Dest: TCoreClassPersistent); override;
    function GetChunkSize: Cardinal; override;
  public
    procedure ReadFromStream(Stream: TCoreClassStream); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property RedSampleValue: Word read FRedSampleValue write FRedSampleValue;
    property BlueSampleValue: Word read FBlueSampleValue write FBlueSampleValue;
    property GreenSampleValue: Word read FGreenSampleValue write FGreenSampleValue;
  end;

  TPngTransparencyFormat3 = class(TCustomPngTransparency)
  private
    function GetCount: TGeoInt;
    function GetTransparency(Index: Cardinal): Byte;
  protected
    FTransparency: array of Byte;
    procedure AssignTo(Dest: TCoreClassPersistent); override;
    function GetChunkSize: Cardinal; override;
  public
    procedure ReadFromStream(Stream: TCoreClassStream); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property Count: TGeoInt read GetCount;
    property Transparency[Index: Cardinal]: Byte read GetTransparency;
  end;

  TChunkPngTransparency = class(TCustomDefinedChunkWithHeader)
  protected
    FTransparency: TCustomPngTransparency;
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    constructor Create(Header_: TChunkPngImageHeader); override;
    destructor Destroy; override;

    procedure ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    procedure HeaderChanged; override;

    property Transparency: TCustomPngTransparency read FTransparency;
  end;

  TChunkPngPhysicalPixelDimensions = class(TCustomDefinedChunkWithHeader)
  private
    FPixelsPerUnitX: Cardinal;
    FPixelsPerUnitY: Cardinal;
    FUnit: Byte;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    procedure ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property PixelsPerUnitX: Cardinal read FPixelsPerUnitX write FPixelsPerUnitX;
    property PixelsPerUnitY: Cardinal read FPixelsPerUnitY write FPixelsPerUnitY;
    property PixelUnit: Byte read FUnit write FUnit;
  end;

  TChunkPngPhysicalScale = class(TCustomDefinedChunkWithHeader)
  private
    FUnitSpecifier: Byte;
    FUnitsPerPixelX: TGeoFloat;
    FUnitsPerPixelY: TGeoFloat;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    procedure ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property UnitSpecifier: Byte read FUnitSpecifier write FUnitSpecifier;
    property UnitsPerPixelX: TGeoFloat read FUnitsPerPixelX write FUnitsPerPixelX;
    property UnitsPerPixelY: TGeoFloat read FUnitsPerPixelY write FUnitsPerPixelY;
  end;

  TChunkPngImageOffset = class(TCustomDefinedChunkWithHeader)
  private
    FImagePositionX: TGeoInt;
    FImagePositionY: TGeoInt;
    FUnitSpecifier: Byte;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    procedure ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property UnitSpecifier: Byte read FUnitSpecifier write FUnitSpecifier;
    property ImagePositionX: TGeoInt read FImagePositionX write FImagePositionX;
    property ImagePositionY: TGeoInt read FImagePositionY write FImagePositionY;
  end;

  TChunkPngPixelCalibrator = class(TCustomDefinedChunkWithHeader)
  private
    FCalibratorName: TPascalString;
    FOriginalZeroes: array [0 .. 1] of TGeoInt;
    FEquationType: Byte;
    FNumberOfParams: Byte;
    FUnitName: TPascalString;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    procedure ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property CalibratorName: TPascalString read FCalibratorName write FCalibratorName;
    property OriginalZeroMin: TGeoInt read FOriginalZeroes[0] write FOriginalZeroes[0];
    property OriginalZeroMax: TGeoInt read FOriginalZeroes[1] write FOriginalZeroes[1];
    property EquationType: Byte read FEquationType write FEquationType;
    property NumberOfParams: Byte read FNumberOfParams write FNumberOfParams;
  end;

  TCustomChunkPngText = class(TCustomDefinedChunkWithHeader)
  private
    procedure SetKeyword(const Value: TPascalString);
    procedure SetText(const Value: TPascalString);
  protected
    FKeyword: TPascalString;
    FText: TPascalString;
    procedure AssignTo(Dest: TCoreClassPersistent); override;
    procedure KeywordChanged; virtual;
    procedure TextChanged; virtual;
  public
    property Keyword: TPascalString read FKeyword write SetKeyword;
    property Text: TPascalString read FText write SetText;
  end;

  TChunkPngText = class(TCustomChunkPngText)
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;
  public
    procedure ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;
  end;

  TChunkPngCompressedText = class(TCustomChunkPngText)
  private
    FCompressionMethod: Byte;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    procedure ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property CompressionMethod: Byte read FCompressionMethod write FCompressionMethod;
  end;

  TChunkPngInternationalText = class(TCustomChunkPngText)
  private
    FCompressionMethod: Byte;
    FCompressionFlag: Byte;
    FLanguageString: TPascalString;
    FTranslatedKeyword: U_String;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    procedure ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property CompressionMethod: Byte read FCompressionMethod write FCompressionMethod;
    property CompressionFlag: Byte read FCompressionFlag write FCompressionFlag;
    property LanguageString: TPascalString read FLanguageString write FLanguageString;
    property TranslatedKeyword: U_String read FTranslatedKeyword write FTranslatedKeyword;
  end;

  TChunkPngUnknown = class(TCustomChunk)
  private
    function GetData(Index: TGeoInt): Byte;
    procedure SetData(Index: TGeoInt; const Value: Byte);
  protected
    FChunkName: TChunkName;
    FDataStream: TMemoryStream64;
    function GetChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;
    function CalculateChecksum: TGeoInt;
    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    constructor Create(ChunkName_: TChunkName); virtual;
    destructor Destroy; override;

    procedure ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal); override;
    procedure WriteToStream(Stream: TCoreClassStream); override;

    property Data[index: TGeoInt]: Byte read GetData write SetData;
    property DataStream: TMemoryStream64 read FDataStream;
  end;

  TChunkList = class(TCoreClassPersistent)
  private
    FChunks: array of TCustomChunk;
    function GetCount: TGeoInt;
  protected
    function GetChunk(Index: TGeoInt): TCustomChunk;
    procedure AssignTo(Dest: TCoreClassPersistent); override;
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    procedure Add(Item: TCustomChunk);
    procedure Clear; virtual;
    procedure Delete(Index: Cardinal);
    function IndexOf(Item: TCustomChunk): TGeoInt;
    procedure Remove(Item: TCustomChunk);

    property Count: TGeoInt read GetCount;
    property Chunks[Index: TGeoInt]: TCustomChunk read GetChunk; default;
  end;

  TCustomPngCoder = class
  protected
    FStream: TCoreClassStream;
    FHeader: TChunkPngImageHeader;
    FGamma: TChunkPngGamma;
    FPalette: TChunkPngPalette;
    FTransparency: TCustomPngTransparency;

    FRowBuffer: array [0 .. 1] of PPNGByteArray;
    FAlphaTable: PPNGByteArray;
    FMappingTable: PPNGByteArray;
    procedure BuildMappingTables; virtual;

    procedure EncodeFilterSub(CurrentRow, PreviousRow, OutputRow: PPNGByteArray; BytesPerRow, PixelByteSize: TGeoInt);
    procedure EncodeFilterUp(CurrentRow, PreviousRow, OutputRow: PPNGByteArray; BytesPerRow, PixelByteSize: TGeoInt);
    procedure EncodeFilterAverage(CurrentRow, PreviousRow, OutputRow: PPNGByteArray; BytesPerRow, PixelByteSize: TGeoInt);
    procedure EncodeFilterPaeth(CurrentRow, PreviousRow, OutputRow: PPNGByteArray; BytesPerRow, PixelByteSize: TGeoInt);

    procedure DecodeFilterSub(CurrentRow, PreviousRow: PPNGByteArray; BytesPerRow, PixelByteSize: NativeInt);
    procedure DecodeFilterUp(CurrentRow, PreviousRow: PPNGByteArray; BytesPerRow, PixelByteSize: NativeInt);
    procedure DecodeFilterAverage(CurrentRow, PreviousRow: PPNGByteArray; BytesPerRow, PixelByteSize: NativeInt);
    procedure DecodeFilterPaeth(CurrentRow, PreviousRow: PPNGByteArray; BytesPerRow, PixelByteSize: NativeInt);

    procedure EncodeFilterRow(CurrentRow, PreviousRow, OutputRow, TempBuffer: PPNGByteArray; BytesPerRow, PixelByteSize: TGeoInt); virtual; abstract;
    procedure DecodeFilterRow(FilterMethod: TAdaptiveFilterMethod; CurrentRow, PreviousRow: PPNGByteArray; BytesPerRow, PixelByteSize: TGeoInt); virtual; abstract;
  public
    constructor Create(Stream: TCoreClassStream; Header: TChunkPngImageHeader;
      Gamma: TChunkPngGamma = nil; Palette: TChunkPngPalette = nil;
      Transparency: TCustomPngTransparency = nil); virtual;
    destructor Destroy; override;
  end;

  TScanLineCallback = function(raster: TObject; Y: TGeoInt): Pointer of object;

  TCustomPngDecoder = class(TCustomPngCoder)
  protected
    procedure EncodeFilterRow(CurrentRow, PreviousRow, OutputRow, TempBuffer: PPNGByteArray; BytesPerRow, PixelByteSize: TGeoInt); override;
    procedure DecodeFilterRow(FilterMethod: TAdaptiveFilterMethod; CurrentRow, PreviousRow: PPNGByteArray; BytesPerRow, PixelByteSize: TGeoInt); override;
  public
    procedure DecodeToScanline(raster: TObject; ScanLineCallback: TScanLineCallback); virtual; abstract;
  end;

  TCustomPngDecoderClass = class of TCustomPngDecoder;

  TCustomPngEncoder = class(TCustomPngCoder)
  protected
    procedure EncodeFilterRow(CurrentRow, PreviousRow, OutputRow, TempBuffer: PPNGByteArray; BytesPerRow, PixelByteSize: TGeoInt); override;
    procedure DecodeFilterRow(FilterMethod: TAdaptiveFilterMethod; CurrentRow, PreviousRow: PPNGByteArray; BytesPerRow, PixelByteSize: TGeoInt); override;
  public
    procedure EncodeFromScanline(raster: TObject; ScanLineCallback: TScanLineCallback); virtual; abstract;
  end;

  TCustomPngEncoderClass = class of TCustomPngEncoder;

  TCustomPngTranscoder = class(TCustomPngCoder)
  protected
    procedure EncodeFilterRow(CurrentRow, PreviousRow, OutputRow, TempBuffer: PPNGByteArray; BytesPerRow, PixelByteSize: TGeoInt); override;
    procedure DecodeFilterRow(FilterMethod: TAdaptiveFilterMethod; CurrentRow, PreviousRow: PPNGByteArray; BytesPerRow, PixelByteSize: TGeoInt); override;

    procedure Transcode; virtual; abstract;
  public
    constructor Create(Stream: TCoreClassStream; Header: TChunkPngImageHeader;
      Gamma: TChunkPngGamma = nil; Palette: TChunkPngPalette = nil;
      Transparency: TCustomPngTransparency = nil); override;
    destructor Destroy; override;
  end;

  TCustomPngTranscoderClass = class of TCustomPngTranscoder;

  TPortableNetworkGraphic = class(TCoreClassPersistent)
  private
    FCompressionLevel: Byte;
    function GetBitDepth: Byte;
    function GetColorType: TColorType;
    function GetCompressionMethod: Byte;
    function GetFilterMethod: TFilterMethod;
    function GetHeight: TGeoInt;
    function GetInterlaceMethod: TInterlaceMethod;
    function GetPaletteEntry(Index: TGeoInt): TRGB24;
    function GetPaletteEntryCount: TGeoInt;
    function GetWidth: TGeoInt;
    function GetGamma: TGeoFloat;
    function GetModifiedTime: TDateTime;
    function GetPixelsPerUnitX: Cardinal;
    function GetPixelsPerUnitY: Cardinal;
    function GetPixelUnit: Byte;
    procedure SetPixelsPerUnitX(const Value: Cardinal);
    procedure SetPixelsPerUnitY(const Value: Cardinal);
    procedure SetPixelUnit(const Value: Byte);
    procedure SetBitDepth(const Value: Byte);
    procedure SetChromaChunk(const Value: TChunkPngPrimaryChromaticities);
    procedure SetColorType(const Value: TColorType);
    procedure SetCompressionMethod(const Value: Byte);
    procedure SetCompressionLevel(const Value: Byte);
    procedure SetFilterMethods(const Value: TAvailableAdaptiveFilterMethods);
    procedure SetFilterMethod(const Value: TFilterMethod);
    procedure SetGamma(const Value: TGeoFloat);
    procedure SetModifiedTime(const Value: TDateTime);
    procedure SetHeight(const Value: TGeoInt);
    procedure SetImageHeader(const Value: TChunkPngImageHeader);
    procedure SetInterlaceMethod(const Value: TInterlaceMethod);
    procedure SetGammaChunk(const Value: TChunkPngGamma);
    procedure SetPaletteChunk(const Value: TChunkPngPalette);
    procedure SetTransparencyChunk(const Value: TChunkPngTransparency);
    procedure SetPhysicalDimensions(const Value: TChunkPngPhysicalPixelDimensions);
    procedure SetSignificantBits(const Value: TChunkPngSignificantBits);
    procedure SetTimeChunk(const Value: TChunkPngTime);
    procedure SetWidth(const Value: TGeoInt);

    function CalculateCRC(Buffer: PByte; Count: Cardinal): Cardinal; overload;
    function CalculateCRC(Stream: TCoreClassStream): Cardinal; overload;
{$IFDEF CheckCRC}
    function CheckCRC(Stream: TCoreClassStream; CRC: Cardinal): Boolean;
{$ENDIF CheckCRC}
    procedure ReadImageDataChunk(Stream: TCoreClassStream; Size: TGeoInt);
    procedure ReadUnknownChunk(Stream: TCoreClassStream; ChunkName: TChunkName; ChunkSize_: TGeoInt);
    function GetFilterMethods: TAvailableAdaptiveFilterMethods;
    procedure SetBackgroundChunk(const Value: TChunkPngBackgroundColor);
  protected
    FImageHeader: TChunkPngImageHeader;
    FPaletteChunk: TChunkPngPalette;
    FGammaChunk: TChunkPngGamma;
    FTimeChunk: TChunkPngTime;
    FSignificantBits: TChunkPngSignificantBits;
    FPhysicalDimensions: TChunkPngPhysicalPixelDimensions;
    FChromaChunk: TChunkPngPrimaryChromaticities;
    FTransparencyChunk: TChunkPngTransparency;
    FBackgroundChunk: TChunkPngBackgroundColor;
    FDataChunkList: TChunkList;
    FAdditionalChunkList: TChunkList;

    procedure Clear; virtual;
    procedure AssignTo(Dest: TCoreClassPersistent); override;

    procedure CopyImageData(Stream: TCoreClassStream);
    procedure StoreImageData(Stream: TCoreClassStream);
    procedure DecompressImageDataToStream(Stream: TCoreClassStream);
    procedure CompressImageDataFromStream(Stream: TCoreClassStream);

    procedure CompressionLevelChanged; virtual;
    procedure AdaptiveFilterMethodsChanged; virtual;
    procedure InterlaceMethodChanged; virtual;

    property ImageHeader: TChunkPngImageHeader read FImageHeader write SetImageHeader;
    property PaletteChunk: TChunkPngPalette read FPaletteChunk write SetPaletteChunk;
    property TransparencyChunk: TChunkPngTransparency read FTransparencyChunk write SetTransparencyChunk;
    property BackgroundChunk: TChunkPngBackgroundColor read FBackgroundChunk write SetBackgroundChunk;
    property GammaChunk: TChunkPngGamma read FGammaChunk write SetGammaChunk;
    property TimeChunk: TChunkPngTime read FTimeChunk write SetTimeChunk;
    property PhysicalPixelDimensionsChunk: TChunkPngPhysicalPixelDimensions read FPhysicalDimensions write SetPhysicalDimensions;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Assign(Source: TCoreClassPersistent); override;

    procedure LoadFromStream(Stream: TCoreClassStream); virtual;
    procedure SaveToStream(Stream: TCoreClassStream); virtual;

    class function CanLoad(Stream: TCoreClassStream): Boolean; overload;

    function HasPhysicalPixelDimensionsInformation: Boolean;
    function HasGammaInformation: Boolean;
    function HasModifiedTimeInformation: Boolean;
    procedure RemovePhysicalPixelDimensionsInformation;
    procedure RemoveGammaInformation;
    procedure RemoveModifiedTimeInformation;

    property Width: TGeoInt read GetWidth write SetWidth;
    property Height: TGeoInt read GetHeight write SetHeight;
    property BitDepth: Byte read GetBitDepth write SetBitDepth;
    property ColorType: TColorType read GetColorType write SetColorType;
    property CompressionMethod: Byte read GetCompressionMethod write SetCompressionMethod;
    property CompressionLevel: Byte read FCompressionLevel write SetCompressionLevel;
    property AdaptiveFilterMethods: TAvailableAdaptiveFilterMethods read GetFilterMethods write SetFilterMethods;
    property FilterMethod: TFilterMethod read GetFilterMethod write SetFilterMethod;
    property InterlaceMethod: TInterlaceMethod read GetInterlaceMethod write SetInterlaceMethod;
    property PaletteEntry[Index: TGeoInt]: TRGB24 read GetPaletteEntry;
    property PaletteEntryCount: TGeoInt read GetPaletteEntryCount;
    property Gamma: TGeoFloat read GetGamma write SetGamma;
    property ModifiedTime: TDateTime read GetModifiedTime write SetModifiedTime;
    property PixelsPerUnitX: Cardinal read GetPixelsPerUnitX write SetPixelsPerUnitX;
    property PixelsPerUnitY: Cardinal read GetPixelsPerUnitY write SetPixelsPerUnitY;
    property PixelUnit: Byte read GetPixelUnit write SetPixelUnit;

    property SignificantBitsChunk: TChunkPngSignificantBits read FSignificantBits write SetSignificantBits;
    property PrimaryChromaticitiesChunk: TChunkPngPrimaryChromaticities read FChromaChunk write SetChromaChunk;
  end;

  TPngNonInterlacedToAdam7Transcoder = class(TCustomPngTranscoder)
  protected
    procedure Transcode; override;
  end;

  TPngAdam7ToNonInterlacedTranscoder = class(TCustomPngTranscoder)
  protected
    procedure Transcode; override;
  end;

  TCRCTable = array [0 .. $FF] of Cardinal;
  PCRCTable = ^TCRCTable;

var
  GCRCTable: PCRCTable;
  GPngChunkClasses: array of TCustomDefinedChunkWithHeaderClass;

const
  ctGrayscale = 0;
  ctTrueColor = 2;
  ctIndexedColor = 3;
  ctGrayscaleAlpha = 4;
  ctTrueColorAlpha = 6;

  fmAdaptiveFilter = 0;

  afmNone = 0;
  afmSub = 1;
  afmUp = 2;
  afmAverage = 3;
  afmPaeth = 4;

  imNone = 0;
  imAdam7 = 1;

  CPngMagic: TChunkName = ($0D, $0A, $1A, $0A);
  CRowStart: array [0 .. 6] of TGeoInt = (0, 0, 4, 0, 2, 0, 1);
  CColumnStart: array [0 .. 6] of TGeoInt = (0, 4, 0, 2, 0, 1, 0);
  CRowIncrement: array [0 .. 6] of TGeoInt = (8, 8, 8, 4, 4, 2, 2);
  CColumnIncrement: array [0 .. 6] of TGeoInt = (8, 8, 4, 4, 2, 2, 1);

  PNG_SIG: TChunkName = ($89, Byte('P'), Byte('N'), Byte('G'));

  RCStrAncillaryUnknownChunk = 'Unknown chunk is marked as ancillary';
  RCStrChunkSizeTooSmall = 'Chunk size too small!';
  RCStrDataIncomplete = 'Data not complete';
  RCStrDirectCompressionMethodSetError = 'Compression Method may not be specified directly yet!';
  RCStrDirectFilterMethodSetError = 'Filter Method may not be specified directly yet!';
  RCStrDirectGammaSetError = 'Gamma may not be specified directly yet!';
  RCStrDirectHeightSetError = 'Height may not be specified directly yet!';
  RCStrDirectWidthSetError = 'Width may not be specified directly yet!';
  RCStrEmptyChunkList = 'Chunk list is empty';
  RCStrHeaderInvalid = 'The provided header is not valid!';
  RCStrIncompletePalette = 'Palette is incomplete';
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';
  RCStrNewHeaderError = 'New header may not be nil!';
  RCStrNotAValidPNGFile = 'Not a valid PNG file';
  RCStrNotYetImplemented = 'Not yet implemented';
  RCStrPaletteLimited = 'Palette is limited to 256 entries';
  RCStrSeveralChromaChunks = 'Primary chromaticities chunk defined twice!';
  RCStrSeveralGammaChunks = 'Gamma chunk defined twice!';
  RCStrSeveralPaletteChunks = 'Palette chunk defined twice!';
  RCStrSeveralTransparencyChunks = 'Transparency chunk defined twice!';
  RCStrSeveralBackgroundChunks = 'Background chunk defined twice!';
  RCStrSeveralPhysicalPixelDimensionChunks = 'Several physical pixel dimenson chunks found';
  RCStrSeveralSignificantBitsChunksFound = 'Several significant bits chunks found';
  RCStrSeveralTimeChunks = 'Time chunk appears twice!';
  RCStrUnknownColorType = 'Unknown color type!';
  RCStrUnspecifiedPixelUnit = 'Unspecified unit';
  RCStrUnsupportedCompressionMethod = 'Compression method not supported!';
  RCStrUnsupportedCompressMethod = 'Unsupported compression method';
  RCStrUnsupportedFilter = 'Unsupported Filter';
  RCStrUnsupportedFilterMethod = 'Unsupported filter method';
  RCStrUnsupportedInterlaceMethod = 'Unsupported interlace method';
  RCStrWrongBitdepth = 'Wrong Bitdepth';
  RCStrWrongInterlaceMethod = 'Wrong interlace method';
  RCStrWrongPixelPerUnit = 'Pixel per unit may not be zero!';
  RCStrWrongTransparencyFormat = 'Wrong transparency format';
  RCStrInvalidCompressionLevel = 'Invalid compression level';
  RCStrBitDepthTranscodingError = 'Bit depth may not be specified directly yet!';
  RCStrColorTypeTranscodingError = 'Color Type may not be specified directly yet!';
  RCStrGrayscale = 'Grayscale';
  RCStrTrueColor = 'True Color';
  RCStrIndexedColor = 'Indexed Color';
  RCStrGrayscaleAlpha = 'Transparent Grayscale';
  RCStrTrueColorAlpha = 'Transparent True Color';
  RCStrInterlacingNone = 'None';
  RCStrInterlacingAdam7 = 'Adam7';
{$IFDEF CheckCRC}
  RCStrCRCError = 'CRC Error';
{$ENDIF CheckCRC}
  RCStrUnsupportedFormat = 'Unsupported Format';

function CompareChunkName(const s1, s2: TChunkName): Boolean; overload; forward;
function CompareChunkName(const s1: TChunkName; const s2: TPascalString): Boolean; overload; forward;
function MakeRawByteString(const s: TChunkName): TPascalString; forward;
function MakeChunkName(const s: TPascalString): TChunkName; forward;
function ReadPNGStringFromStream(Stream: TCoreClassStream; L: Integer): TPascalString; forward;
procedure WritePNGStringToStream(s: TPascalString; Stream: TCoreClassStream); forward;

procedure RegisterPngChunk(ChunkClass: TCustomDefinedChunkWithHeaderClass); forward;
procedure RegisterPngChunks(ChunkClasses: array of TCustomDefinedChunkWithHeaderClass); forward;
function FindPngChunkByChunkName(ChunkName: TChunkName): TCustomDefinedChunkWithHeaderClass; forward;

function ColorTypeToString(Value: TColorType): U_String; forward;
function InterlaceMethodToString(Value: TInterlaceMethod): U_String; forward;
procedure BuildCRCTable(Polynomial: Cardinal); forward;

function IsPngChunkRegistered(ChunkClass: TCustomDefinedChunkWithHeaderClass): Boolean;
var
  ChunkClassIndex: TGeoInt;
begin
  Result := False;
  for ChunkClassIndex := 0 to Length(GPngChunkClasses) - 1 do
    if GPngChunkClasses[ChunkClassIndex] = ChunkClass then
      begin
        Result := True;
        Exit;
      end;
end;

function CompareChunkName(const s1, s2: TChunkName): Boolean;
begin
  Result := CompareMemory(@s1[0], @s2[0], SizeOf(TChunkName));
end;

function CompareChunkName(const s1: TChunkName; const s2: TPascalString): Boolean;
begin
  Result := CompareChunkName(s1, MakeChunkName(s2));
end;

function MakeRawByteString(const s: TChunkName): TPascalString;
begin
  Result.L := 4;
  Result[1] := SystemChar(s[0]);
  Result[2] := SystemChar(s[1]);
  Result[3] := SystemChar(s[2]);
  Result[4] := SystemChar(s[3]);
end;

function MakeChunkName(const s: TPascalString): TChunkName;
begin
  Result[0] := Byte(s[1]);
  Result[1] := Byte(s[2]);
  Result[2] := Byte(s[3]);
  Result[3] := Byte(s[4]);
end;

function ReadPNGStringFromStream(Stream: TCoreClassStream; L: Integer): TPascalString;
var
  i: TGeoInt;
  tmp: TBytes;
begin
  Result := '';
  i := 0;
  Setlength(tmp, L);
  while (Stream.Position < Stream.Size) do
    begin
      Stream.Read(tmp[i], 1);
      if tmp[i] = 0 then
        begin
          Setlength(tmp, i);
          Result.ANSI := tmp;
          Break;
        end;
      Inc(i);
    end;
end;

procedure WritePNGStringToStream(s: TPascalString; Stream: TCoreClassStream);
var
  tmp: TBytes;
  tmp_end: Byte;
begin
  tmp := s.ANSI;
  if Length(tmp) > 0 then
    begin
      Stream.Write(tmp[0], Length(tmp));
      Setlength(tmp, 0);
    end;
  tmp_end := 0;
  Stream.Write(tmp_end, 1);
end;

procedure RegisterPngChunk(ChunkClass: TCustomDefinedChunkWithHeaderClass);
begin
  Assert(IsPngChunkRegistered(ChunkClass) = False);
  Setlength(GPngChunkClasses, Length(GPngChunkClasses) + 1);
  GPngChunkClasses[Length(GPngChunkClasses) - 1] := ChunkClass;
end;

procedure RegisterPngChunks(ChunkClasses: array of TCustomDefinedChunkWithHeaderClass);
var
  ChunkClassIndex: TGeoInt;
begin
  for ChunkClassIndex := 0 to Length(ChunkClasses) - 1 do
      RegisterPngChunk(ChunkClasses[ChunkClassIndex]);
end;

function FindPngChunkByChunkName(ChunkName: TChunkName): TCustomDefinedChunkWithHeaderClass;
var
  ChunkClassIndex: TGeoInt;
begin
  Result := nil;
  for ChunkClassIndex := 0 to Length(GPngChunkClasses) - 1 do
    if CompareChunkName(GPngChunkClasses[ChunkClassIndex].GetClassChunkName, ChunkName) then
      begin
        Result := GPngChunkClasses[ChunkClassIndex];
        Exit;
      end;
end;

type
  T16Bit = record
    case TGeoInt of
      0: (v: Word);
      1: (B: array [0 .. 1] of Byte);
  end;

  T32Bit = record
    case TGeoInt of
      0: (v: Cardinal);
      1: (B: array [0 .. 3] of Byte);
  end;

function Swap16(Value: Word): Word;
var
  t: Byte;
begin
  with T16Bit(Value) do
    begin
      t := B[0];
      B[0] := B[1];
      B[1] := t;
      Result := v;
    end;
end;

function Swap32(Value: Cardinal): Cardinal;
var
  Temp: Byte;
begin
  with T32Bit(Value) do
    begin
      Temp := B[0];
      B[0] := B[3];
      B[3] := Temp;
      Temp := B[1];
      B[1] := B[2];
      B[2] := Temp;
      Result := v;
    end;
end;

procedure Flip16(var Value);
var
  t: Byte;
begin
  with T16Bit(Value) do
    begin
      t := B[0];
      B[0] := B[1];
      B[1] := t;
    end;
end;

procedure Flip32(var Value);
var
  Temp: Byte;
begin
  with T32Bit(Value) do
    begin
      Temp := B[0];
      B[0] := B[3];
      B[3] := Temp;
      Temp := B[1];
      B[1] := B[2];
      B[2] := Temp;
    end;
end;

function ReadSwappedWord(Stream: TCoreClassStream): Word;
begin
{$IFDEF ValidateEveryReadOperation}
  if Stream.Read(Result, SizeOf(Word)) <> SizeOf(Word) then
      raiseInfo('ReadSwappedWord error!');
{$ELSE}
  Stream.Read(Result, SizeOf(Word));
{$ENDIF}
  Result := Swap16(Result);
end;

function ReadSwappedSmallInt(Stream: TCoreClassStream): SmallInt;
begin
{$IFDEF ValidateEveryReadOperation}
  if Stream.Read(Result, SizeOf(SmallInt)) <> SizeOf(SmallInt) then
      raiseInfo('ReadSwappedSmallInt error!');
{$ELSE}
  Stream.Read(Result, SizeOf(SmallInt));
{$ENDIF}
  Result := Swap16(Result);
end;

function ReadSwappedCardinal(Stream: TCoreClassStream): Cardinal;
begin
{$IFDEF ValidateEveryReadOperation}
  if Stream.Read(Result, SizeOf(Cardinal)) <> SizeOf(Cardinal) then
      raiseInfo('ReadSwappedCardinal error!');
{$ELSE}
  Stream.Read(Result, SizeOf(Cardinal));
{$ENDIF}
  Result := Swap32(Result);
end;

procedure WriteSwappedWord(Stream: TCoreClassStream; Value: Word);
begin
  Value := Swap16(Value);
  Stream.Write(Value, SizeOf(Word));
end;

procedure WriteSwappedSmallInt(Stream: TCoreClassStream; Value: SmallInt);
begin
  Value := Swap16(Value);
  Stream.Write(Value, SizeOf(SmallInt));
end;

procedure WriteSwappedCardinal(Stream: TCoreClassStream; Value: Cardinal);
begin
  Value := Swap32(Value);
  Stream.Write(Value, SizeOf(Cardinal));
end;

function ColorTypeToString(Value: TColorType): U_String;
begin
  case Value of
    ctGrayscale: Result := RCStrGrayscale;
    ctTrueColor: Result := RCStrTrueColor;
    ctIndexedColor: Result := RCStrIndexedColor;
    ctGrayscaleAlpha: Result := RCStrGrayscaleAlpha;
    ctTrueColorAlpha: Result := RCStrTrueColorAlpha;
    else Result := 'undefined';
  end;
end;

function InterlaceMethodToString(Value: TInterlaceMethod): U_String;
begin
  case Value of
    imNone: Result := RCStrInterlacingNone;
    imAdam7: Result := RCStrInterlacingAdam7;
    else raiseInfo('InterlaceMethodToString error!');
  end;
end;

procedure ZCompress(Data: Pointer; Size: TGeoInt; const Output: TCoreClassStream; Level: TGeoInt); overload;
const
  CBufferSize = $8000;
var
  ZStreamRecord: TZStreamRec;
  ZResult: TGeoInt;
  TempBuffer: Pointer;
begin
  FillPtr(@ZStreamRecord, SizeOf(TZStreamRec), 0);

  with ZStreamRecord do
    begin
      next_in := Data;
      avail_in := Size;
{$IFNDEF FPC}
{$IFNDEF ZLibEx}
      zalloc := zlibAllocMem;
      zfree := zlibFreeMem;
{$ENDIF}
{$ENDIF}
    end;

{$IFDEF FPC}
  if deflateInit(ZStreamRecord, Level) <> 0 then
      raise EPngError.Create('Error during compression');
{$ELSE}
  if deflateInit(ZStreamRecord, Level) <> 0 then
      raise EPngError.Create('Error during compression');
{$ENDIF}
  TempBuffer := System.GetMemory(CBufferSize);
  try
    while ZStreamRecord.avail_in > 0 do
      begin
        ZStreamRecord.next_out := TempBuffer;
        ZStreamRecord.avail_out := CBufferSize;

        deflate(ZStreamRecord, Z_NO_FLUSH);

        Output.Write(TempBuffer^, CBufferSize - ZStreamRecord.avail_out);
      end;

    repeat
      ZStreamRecord.next_out := TempBuffer;
      ZStreamRecord.avail_out := CBufferSize;

      ZResult := deflate(ZStreamRecord, Z_FINISH);

      Output.Write(TempBuffer^, CBufferSize - ZStreamRecord.avail_out);
    until (ZResult = Z_STREAM_END) and (ZStreamRecord.avail_out > 0);
  finally
      System.FreeMemory(TempBuffer);
  end;

  if deflateEnd(ZStreamRecord) > 0 then
      raise EPngError.Create('Error on stream validation');
end;

procedure ZCompress(const Input: TMemoryStream64; const Output: TCoreClassStream; Level: TGeoInt); overload;
begin
  ZCompress(Input.Memory, Input.Size, Output, Level);
end;

procedure ZDecompress(Data: Pointer; Size: TGeoInt; const Output: TCoreClassStream); overload;
const
  CBufferSize = $8000;
var
  ZStreamRecord: TZStreamRec;
  ZResult: TGeoInt;
  TempBuffer: Pointer;
begin
  FillPtr(@ZStreamRecord, SizeOf(TZStreamRec), 0);

  with ZStreamRecord do
    begin
      next_in := Data;
      avail_in := Size;
{$IFNDEF FPC}
{$IFNDEF ZLibEx}
      zalloc := zlibAllocMem;
      zfree := zlibFreeMem;
{$ENDIF}
{$ENDIF}
    end;

{$IFDEF FPC}
  if inflateInit(ZStreamRecord) <> 0 then
      raise EPngError.Create('Error during decompression');
{$ELSE}
  if inflateInit(ZStreamRecord) <> 0 then
      raise EPngError.Create('Error during decompression');
{$ENDIF}
  TempBuffer := System.GetMemory(CBufferSize);
  try
    ZResult := Z_OK;

    while (ZStreamRecord.avail_in > 0) and (ZResult = Z_OK) do
      begin
        ZStreamRecord.next_out := TempBuffer;
        ZStreamRecord.avail_out := CBufferSize;

        ZResult := inflate(ZStreamRecord, Z_NO_FLUSH);

        Output.Write(TempBuffer^, CBufferSize - ZStreamRecord.avail_out);
      end;

    repeat
      ZStreamRecord.next_out := TempBuffer;
      ZStreamRecord.avail_out := CBufferSize;

      ZResult := inflate(ZStreamRecord, Z_FINISH);

      Output.Write(TempBuffer^, CBufferSize - ZStreamRecord.avail_out);
    until (ZResult = Z_STREAM_END) and (ZStreamRecord.avail_out > 0);
  finally
      System.FreeMemory(TempBuffer);
  end;

  if inflateEnd(ZStreamRecord) > 0 then
      raise EPngError.Create('Error on stream validation');
end;

procedure ZDecompress(const Input: TMemoryStream64; const Output: TCoreClassStream); overload;
begin
  ZDecompress(Input.Memory, Input.Size, Output);
end;

constructor TCustomPngSignificantBits.Create(BitDepth: TGeoInt);
begin
  inherited Create;
end;

procedure TCustomPngTransparency.ReadFromStream(Stream: TCoreClassStream);
begin

end;

procedure TCustomPngTransparency.WriteToStream(Stream: TCoreClassStream);
begin

end;

procedure TCustomChunk.ReadFromStream(Stream: TCoreClassStream;
  ChunkSize: Cardinal);
begin

end;

procedure TCustomChunk.WriteToStream(Stream: TCoreClassStream);
begin

end;

function TCustomDefinedChunk.GetChunkName: TChunkName;
begin
  Result := GetClassChunkName;
end;

constructor TChunkPngUnknown.Create(ChunkName_: TChunkName);
begin
  FChunkName := ChunkName_;
  FDataStream := TMemoryStream64.Create;
end;

destructor TChunkPngUnknown.Destroy;
begin
  DisposeObjectAndNil(FDataStream);
  inherited;
end;

function TChunkPngUnknown.CalculateChecksum: TGeoInt;
var
  B: Byte;
begin
  with FDataStream do
    begin
      Position := 0;
      Result := 0;
      while Position < Size do
        begin
          Read(B, 1);
          Result := Result + B;
        end;
    end;
end;

procedure TChunkPngUnknown.AssignTo(Dest: TCoreClassPersistent);
begin
  inherited;

  if Dest is TChunkPngUnknown then
    begin
      TChunkPngUnknown(Dest).FDataStream.CopyFrom(FDataStream, FDataStream.Size);
      TChunkPngUnknown(Dest).FChunkName := FChunkName;
    end;
end;

function TChunkPngUnknown.GetData(Index: TGeoInt): Byte;
begin
  if (Index >= 0) and (Index < FDataStream.Size) then
    with FDataStream do
      begin
        Position := Index;
        Read(Result, 1);
      end
  else
      raise EPngError.CreateFmt(RCStrIndexOutOfBounds, [index]);
end;

function TChunkPngUnknown.GetChunkSize: Cardinal;
begin
  Result := FDataStream.Size;
end;

function TChunkPngUnknown.GetChunkName: TChunkName;
begin
  Result := FChunkName;
end;

procedure TChunkPngUnknown.ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal);
begin
  with Stream do
    begin
      Assert(ChunkSize_ <= Size);

      FDataStream.Clear;
      FDataStream.Position := 0;
      if ChunkSize_ > 0 then
          FDataStream.CopyFrom(Stream, ChunkSize_);
    end;
end;

procedure TChunkPngUnknown.WriteToStream(Stream: TCoreClassStream);
begin
  with Stream do
    begin
      FDataStream.Position := 0;
      CopyFrom(FDataStream, FDataStream.Position);
    end;
end;

procedure TChunkPngUnknown.SetData(Index: TGeoInt; const Value: Byte);
begin
  if (Index >= 0) and (Index < FDataStream.Size) then
    with FDataStream do
      begin
        Position := Index;
        Write(Value, 1);
      end
  else
      raise EPngError.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

constructor TChunkPngImageHeader.Create;
begin
  inherited;
  FAdaptiveFilterMethods := [aafmSub, aafmUp, aafmAverage, aafmPaeth];

  ResetToDefault;
end;

procedure TChunkPngImageHeader.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TChunkPngImageHeader then
    with TChunkPngImageHeader(Dest) do
      begin
        FWidth := Self.FWidth;
        FHeight := Self.FHeight;
        FBitDepth := Self.FBitDepth;
        FColorType := Self.FColorType;
        FCompressionMethod := Self.FCompressionMethod;
        FFilterMethod := Self.FFilterMethod;
        FInterlaceMethod := Self.FInterlaceMethod;
        FAdaptiveFilterMethods := Self.FAdaptiveFilterMethods;
      end
  else
      inherited;
end;

function TChunkPngImageHeader.GetBytesPerRow: TGeoInt;
begin
  case FColorType of
    ctGrayscale, ctIndexedColor: Result := ((FWidth * FBitDepth + $7) and not $7) shr 3;
    ctGrayscaleAlpha: Result := 2 * (FBitDepth shr 3) * FWidth;
    ctTrueColor: Result := 3 * (FBitDepth shr 3) * FWidth;
    ctTrueColorAlpha: Result := 4 * (FBitDepth shr 3) * FWidth;
    else raise EPngError.Create(RCStrUnknownColorType);
  end;
end;

class function TChunkPngImageHeader.GetClassChunkName: TChunkName;
begin
  Result := MakeChunkName('IHDR');
end;

function TChunkPngImageHeader.GetChunkSize: Cardinal;
begin
  Result := 13;
end;

procedure TChunkPngImageHeader.ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal);
begin
  with Stream do
    begin
      if (ChunkSize_ > Size) or (GetChunkSize > ChunkSize_) then
          raise EPngError.Create(RCStrChunkSizeTooSmall);

      // read width
      FWidth := ReadSwappedCardinal(Stream);

      // read height
      FHeight := ReadSwappedCardinal(Stream);

      // read bit depth
      Read(FBitDepth, 1);

      // read Color type
      Read(FColorType, 1);

      // check consistency between Color type and bit depth
      case FColorType of
        ctGrayscale: if not(FBitDepth in [1, 2, 4, 8, 16]) then
              raise EPngError.Create(RCStrWrongBitdepth);
        ctTrueColor, ctGrayscaleAlpha, ctTrueColorAlpha: if not(FBitDepth in [8, 16]) then
              raise EPngError.Create(RCStrWrongBitdepth);
        ctIndexedColor: if not(FBitDepth in [1, 2, 4, 8]) then
              raise EPngError.Create(RCStrWrongBitdepth);
      end;

      // read compression method
      Read(FCompressionMethod, 1);

      // check for compression method
      if FCompressionMethod <> 0 then
          raise EPngError.Create(RCStrUnsupportedCompressMethod);

      // read filter method
      Read(FFilterMethod, 1);

      // check for filter method
      if FFilterMethod <> fmAdaptiveFilter then
          raise EPngError.Create(RCStrUnsupportedFilterMethod);

      // read interlace method
      Read(FInterlaceMethod, 1);

      // check for interlace method
      if not(FInterlaceMethod in [imNone, imAdam7]) then
          raise EPngError.Create(RCStrUnsupportedInterlaceMethod);
    end;
end;

procedure TChunkPngImageHeader.WriteToStream(Stream: TCoreClassStream);
begin
  with Stream do
    begin
      // write width
      WriteSwappedCardinal(Stream, FWidth);

      // write height
      WriteSwappedCardinal(Stream, FHeight);

      // write bit depth
      Write(FBitDepth, 1);

      // write Color type
      Write(FColorType, 1);

      // write compression method
      Write(FCompressionMethod, 1);

      // write filter method
      Write(FFilterMethod, 1);

      // write interlace method
      Write(FInterlaceMethod, 1);
    end;
end;

function TChunkPngImageHeader.GetPixelByteSize: TGeoInt;
begin
  case ColorType of
    ctGrayscale: if FBitDepth = 16 then
          Result := 2
      else
          Result := 1;
    ctTrueColor: Result := 3 * FBitDepth div 8;
    ctIndexedColor: Result := 1;
    ctGrayscaleAlpha: Result := 2 * FBitDepth div 8;
    ctTrueColorAlpha: Result := 4 * FBitDepth div 8;
    else Result := 0;
  end;
end;

function TChunkPngImageHeader.GetHasPalette: Boolean;
begin
  Result := FColorType in [ctIndexedColor];
end;

procedure TChunkPngImageHeader.ResetToDefault;
begin
  FWidth := 0;
  FHeight := 0;
  FBitDepth := 8;
  FColorType := ctTrueColor;
  FCompressionMethod := 0;
  FFilterMethod := fmAdaptiveFilter;
  FInterlaceMethod := imNone;
end;

procedure TChunkPngImageHeader.SetAdaptiveFilterMethods(
  const Value: TAvailableAdaptiveFilterMethods);
begin
  FAdaptiveFilterMethods := Value;
end;

procedure TChunkPngImageHeader.SetCompressionMethod(const Value: Byte);
begin
  // check for compression method
  if Value <> 0 then
      raise EPngError.Create(RCStrUnsupportedCompressMethod);
end;

procedure TChunkPngImageHeader.SetFilterMethod(const Value: TFilterMethod);
begin
  // check for filter method
  if Value <> fmAdaptiveFilter then
      raise EPngError.Create(RCStrUnsupportedFilterMethod);
end;

procedure TCustomDefinedChunkWithHeader.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TCustomDefinedChunkWithHeader then
    with TCustomDefinedChunkWithHeader(Dest) do
      begin
        FHeader.Assign(Self.FHeader);
      end
  else
      inherited;
end;

constructor TCustomDefinedChunkWithHeader.Create(Header_: TChunkPngImageHeader);
begin
  if not(Header_ is TChunkPngImageHeader) then
      raise EPngError.Create(RCStrHeaderInvalid);

  FHeader := Header_;
  inherited Create;
end;

procedure TCustomDefinedChunkWithHeader.HeaderChanged;
begin
  // purely virtual, do nothing by default
end;

procedure TChunkPngPalette.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TChunkPngPalette then
    begin
      Setlength(TChunkPngPalette(Dest).FPaletteEntries, Length(FPaletteEntries));
      CopyPtr(@FPaletteEntries[0], @TChunkPngPalette(Dest).FPaletteEntries[0], Length(FPaletteEntries) * SizeOf(TRGB24));
    end
  else
      inherited;
end;

class function TChunkPngPalette.GetClassChunkName: TChunkName;
begin
  Result := MakeChunkName('PLTE');
end;

function TChunkPngPalette.GetPaletteEntry(Index: Cardinal): TRGB24;
begin
  if (Index < Count) then
      Result := FPaletteEntries[Index]
  else
      raise EPngError.Create(RCStrIndexOutOfBounds);
end;

procedure TChunkPngPalette.SetPaletteEntry(Index: Cardinal; const Value: TRGB24);
begin
  if (Index < Count) then
      FPaletteEntries[Index] := Value
  else
      raise EPngError.Create(RCStrIndexOutOfBounds);
end;

function TChunkPngPalette.GetCount: TGeoInt;
begin
  Result := Length(FPaletteEntries);
end;

function TChunkPngPalette.GetChunkSize: Cardinal;
begin
  Result := Length(FPaletteEntries) * SizeOf(TRGB24);
end;

procedure TChunkPngPalette.ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal);
begin
  with Stream do
    begin
      if (ChunkSize_ mod SizeOf(TRGB24)) <> 0 then
          raise EPngError.Create(RCStrIncompletePalette);

      Setlength(FPaletteEntries, ChunkSize_ div SizeOf(TRGB24));
      Read(FPaletteEntries[0], Length(FPaletteEntries) * SizeOf(TRGB24));
    end;
end;

procedure TChunkPngPalette.WriteToStream(Stream: TCoreClassStream);
begin
  if ChunkSize > 0 then
      Stream.Write(FPaletteEntries[0], ChunkSize);
end;

procedure TChunkPngPalette.PaletteEntriesChanged;
begin
  // nothing todo here yet
end;

procedure TChunkPngPalette.SetCount(const Value: TGeoInt);
begin
  if Value > 256 then
      raise EPngError.Create(RCStrPaletteLimited);

  if Value <> Cardinal(Length(FPaletteEntries)) then
    begin
      Setlength(FPaletteEntries, Value);
      PaletteEntriesChanged;
    end;
end;

procedure TChunkPngTransparency.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TChunkPngTransparency then
    with TChunkPngTransparency(Dest) do
      begin
        FTransparency.Assign(Self.FTransparency);
      end
  else
      inherited;
end;

constructor TChunkPngTransparency.Create(Header_: TChunkPngImageHeader);
begin
  inherited;
  case Header.ColorType of
    ctGrayscale: FTransparency := TPngTransparencyFormat0.Create;
    ctTrueColor: FTransparency := TPngTransparencyFormat2.Create;
    ctIndexedColor: FTransparency := TPngTransparencyFormat3.Create;
  end;
end;

destructor TChunkPngTransparency.Destroy;
begin
  if Assigned(FTransparency) then
      DisposeObjectAndNil(FTransparency);
  inherited;
end;

class function TChunkPngTransparency.GetClassChunkName: TChunkName;
begin
  Result := MakeChunkName('tRNS');
end;

procedure TChunkPngTransparency.HeaderChanged;
var
  OldTransparency: TCustomPngTransparency;
begin
  inherited;

  // store old transparency object
  OldTransparency := FTransparency;

  // change transparency object class
  case FHeader.ColorType of
    ctGrayscale:
      if not(FTransparency is TPngTransparencyFormat0) then
        begin
          FTransparency := TPngTransparencyFormat0.Create;
          if Assigned(OldTransparency) then
            begin
              FTransparency.Assign(OldTransparency);
              DisposeObjectAndNil(OldTransparency);
            end;
        end;
    ctTrueColor:
      if not(FTransparency is TPngTransparencyFormat2) then
        begin
          FTransparency := TPngTransparencyFormat2.Create;
          if Assigned(OldTransparency) then
            begin
              FTransparency.Assign(OldTransparency);
              DisposeObjectAndNil(OldTransparency);
            end;
        end;
    ctIndexedColor:
      if not(FTransparency is TPngTransparencyFormat3) then
        begin
          FTransparency := TPngTransparencyFormat3.Create;
          if Assigned(OldTransparency) then
            begin
              FTransparency.Assign(OldTransparency);
              DisposeObjectAndNil(OldTransparency);
            end;
        end;
    else
      if Assigned(FTransparency) then
          DisposeObjectAndNil(FTransparency);
  end;
end;

function TChunkPngTransparency.GetChunkSize: Cardinal;
begin
  if Assigned(FTransparency) then
      Result := FTransparency.ChunkSize_
  else
      Result := 0;
end;

procedure TChunkPngTransparency.ReadFromStream(Stream: TCoreClassStream;
  ChunkSize_: Cardinal);
begin
  if Assigned(FTransparency) then
      FTransparency.ReadFromStream(Stream);
end;

procedure TChunkPngTransparency.WriteToStream(Stream: TCoreClassStream);
begin
  // check consistency
  case FHeader.ColorType of
    ctGrayscale: if not(FTransparency is TPngTransparencyFormat0) then
          raise EPngError.Create(RCStrWrongTransparencyFormat);
    ctTrueColor: if not(FTransparency is TPngTransparencyFormat2) then
          raise EPngError.Create(RCStrWrongTransparencyFormat);
    ctIndexedColor: if not(FTransparency is TPngTransparencyFormat3) then
          raise EPngError.Create(RCStrWrongTransparencyFormat);
  end;

  if Assigned(FTransparency) then
      FTransparency.WriteToStream(Stream);
end;

procedure TPngTransparencyFormat0.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TPngTransparencyFormat0 then
    with TPngTransparencyFormat0(Dest) do
      begin
        FGraySampleValue := Self.FGraySampleValue;
      end
  else
      inherited;
end;

function TPngTransparencyFormat0.GetChunkSize: Cardinal;
begin
  Result := 2;
end;

procedure TPngTransparencyFormat0.ReadFromStream(Stream: TCoreClassStream);
begin
  inherited;

  FGraySampleValue := ReadSwappedWord(Stream);
end;

procedure TPngTransparencyFormat0.WriteToStream(Stream: TCoreClassStream);
begin
  inherited;

  WriteSwappedWord(Stream, FGraySampleValue);
end;

procedure TPngTransparencyFormat2.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TPngTransparencyFormat2 then
    with TPngTransparencyFormat2(Dest) do
      begin
        FRedSampleValue := Self.FRedSampleValue;
        FBlueSampleValue := Self.FBlueSampleValue;
        FGreenSampleValue := Self.FGreenSampleValue;
      end
  else
      inherited;
end;

function TPngTransparencyFormat2.GetChunkSize: Cardinal;
begin
  Result := 6;
end;

procedure TPngTransparencyFormat2.ReadFromStream(Stream: TCoreClassStream);
begin
  inherited;

  FRedSampleValue := ReadSwappedWord(Stream);
  FBlueSampleValue := ReadSwappedWord(Stream);
  FGreenSampleValue := ReadSwappedWord(Stream);
end;

procedure TPngTransparencyFormat2.WriteToStream(Stream: TCoreClassStream);
begin
  inherited;

  WriteSwappedWord(Stream, FRedSampleValue);
  WriteSwappedWord(Stream, FBlueSampleValue);
  WriteSwappedWord(Stream, FGreenSampleValue);
end;

procedure TPngTransparencyFormat3.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TPngTransparencyFormat3 then
    begin
      Setlength(TPngTransparencyFormat3(Dest).FTransparency, Length(FTransparency));
      CopyPtr(@FTransparency[0], @TPngTransparencyFormat3(Dest).FTransparency[0], Length(FTransparency));
    end
  else
      inherited;
end;

function TPngTransparencyFormat3.GetChunkSize: Cardinal;
begin
  Result := Count;
end;

function TPngTransparencyFormat3.GetCount: TGeoInt;
begin
  Result := Length(FTransparency);
end;

function TPngTransparencyFormat3.GetTransparency(Index: Cardinal): Byte;
begin
  if Index < Count then
      Result := FTransparency[Index]
  else
      raise EPngError.Create(RCStrIndexOutOfBounds);
end;

procedure TPngTransparencyFormat3.ReadFromStream(Stream: TCoreClassStream);
begin
  inherited;

  with Stream do
    begin
      Setlength(FTransparency, Size - Position);
      Read(FTransparency[0], Length(FTransparency));
    end;
end;

procedure TPngTransparencyFormat3.WriteToStream(Stream: TCoreClassStream);
begin
  inherited;

  Stream.Write(FTransparency[0], Length(FTransparency));
end;

procedure TChunkPngPhysicalPixelDimensions.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TChunkPngPhysicalPixelDimensions then
    with TChunkPngPhysicalPixelDimensions(Dest) do
      begin
        FPixelsPerUnitX := Self.FPixelsPerUnitX;
        FPixelsPerUnitY := Self.FPixelsPerUnitY;
        FUnit := Self.FUnit;
      end
  else
      inherited;
end;

class function TChunkPngPhysicalPixelDimensions.GetClassChunkName: TChunkName;
begin
  Result := MakeChunkName('pHYs');
end;

function TChunkPngPhysicalPixelDimensions.GetChunkSize: Cardinal;
begin
  Result := 9;
end;

procedure TChunkPngPhysicalPixelDimensions.ReadFromStream(Stream: TCoreClassStream;
  ChunkSize_: Cardinal);
begin
  with Stream do
    begin
      if (ChunkSize_ > Size) or (GetChunkSize > ChunkSize_) then
          raise EPngError.Create(RCStrChunkSizeTooSmall);

      // read pixels per unit, X axis
      FPixelsPerUnitX := ReadSwappedCardinal(Stream);

      // read pixels per unit, Y axis
      FPixelsPerUnitY := ReadSwappedCardinal(Stream);

      // read unit
      Read(FUnit, 1);
    end;
end;

procedure TChunkPngPhysicalPixelDimensions.WriteToStream(Stream: TCoreClassStream);
begin
  with Stream do
    begin
      // write pixels per unit, X axis
      WriteSwappedCardinal(Stream, FPixelsPerUnitX);

      // write pixels per unit, Y axis
      WriteSwappedCardinal(Stream, FPixelsPerUnitY);

      // write unit
      Write(FUnit, 1);
    end;
end;

procedure TChunkPngPhysicalScale.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TChunkPngPhysicalScale then
    with TChunkPngPhysicalScale(Dest) do
      begin
        FUnitSpecifier := Self.FUnitSpecifier;
        FUnitsPerPixelX := Self.FUnitsPerPixelX;
        FUnitsPerPixelY := Self.FUnitsPerPixelY;
      end
  else
      inherited;
end;

class function TChunkPngPhysicalScale.GetClassChunkName: TChunkName;
begin
  Result := MakeChunkName('sCAL');
end;

function TChunkPngPhysicalScale.GetChunkSize: Cardinal;
begin
  Result := 4;
end;

procedure TChunkPngPhysicalScale.ReadFromStream(Stream: TCoreClassStream;
  ChunkSize_: Cardinal);
begin
  with Stream do
    begin
      if (ChunkSize_ > Size) or (GetChunkSize > ChunkSize_) then
          raise EPngError.Create(RCStrChunkSizeTooSmall);

      // read unit specifier
      Read(FUnitSpecifier, 1);

      // yet todo, see http://www.libpng.org/pub/png/book/chapter11.html#png.ch11.div.9
    end;
end;

procedure TChunkPngPhysicalScale.WriteToStream(Stream: TCoreClassStream);
begin
  raise EPngError.Create(RCStrNotYetImplemented);
  // yet todo, see http://www.libpng.org/pub/png/book/chapter11.html#png.ch11.div.9
end;

procedure TChunkPngImageOffset.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TChunkPngImageOffset then
    with TChunkPngImageOffset(Dest) do
      begin
        FImagePositionX := Self.FImagePositionX;
        FImagePositionY := Self.FImagePositionY;
        FUnitSpecifier := Self.FUnitSpecifier;
      end
  else
      inherited;
end;

class function TChunkPngImageOffset.GetClassChunkName: TChunkName;
begin
  Result := MakeChunkName('oFFs');
end;

function TChunkPngImageOffset.GetChunkSize: Cardinal;
begin
  Result := 9;
end;

procedure TChunkPngImageOffset.ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal);
begin
  with Stream do
    begin
      if (ChunkSize_ > Size) or (GetChunkSize > ChunkSize_) then
          raise EPngError.Create(RCStrChunkSizeTooSmall);

      // read image positions
      FImagePositionX := ReadSwappedCardinal(Stream);
      FImagePositionY := ReadSwappedCardinal(Stream);

      // read unit specifier
      Read(FUnitSpecifier, 1);
    end;
end;

procedure TChunkPngImageOffset.WriteToStream(Stream: TCoreClassStream);
begin
  // write image positions
  WriteSwappedCardinal(Stream, FImagePositionX);
  WriteSwappedCardinal(Stream, FImagePositionY);

  // write unit specifier
  Write(FUnitSpecifier, 1);
end;

procedure TChunkPngPixelCalibrator.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TChunkPngPixelCalibrator then
    with TChunkPngPixelCalibrator(Dest) do
      begin
        FCalibratorName := Self.FCalibratorName;
        FOriginalZeroes[0] := Self.FOriginalZeroes[0];
        FOriginalZeroes[1] := Self.FOriginalZeroes[1];
        FEquationType := Self.FEquationType;
        FNumberOfParams := Self.FNumberOfParams;
        FUnitName := Self.FUnitName;
      end
  else
      inherited;
end;

class function TChunkPngPixelCalibrator.GetClassChunkName: TChunkName;
begin
  Result := MakeChunkName('pCAL');
end;

function TChunkPngPixelCalibrator.GetChunkSize: Cardinal;
begin
  Result := 9;
end;

procedure TChunkPngPixelCalibrator.ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal);
begin
  with Stream do
    begin
      // read keyword
      FCalibratorName := ReadPNGStringFromStream(Stream, 80);

      // read original zeros
      FOriginalZeroes[0] := ReadSwappedCardinal(Stream);
      FOriginalZeroes[1] := ReadSwappedCardinal(Stream);

      // read equation type
      Stream.Read(FEquationType, 1);

      // read number of parameters
      Stream.Read(FNumberOfParams, 1);

      // read keyword
      FUnitName := ReadPNGStringFromStream(Stream, 80);
    end;
end;

procedure TChunkPngPixelCalibrator.WriteToStream(Stream: TCoreClassStream);
begin
  inherited;

end;

procedure TCustomChunkPngText.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TCustomChunkPngText then
    with TCustomChunkPngText(Dest) do
      begin
        FKeyword := Self.FKeyword;
        FText := Self.FText;
      end
  else
      inherited;
end;

procedure TCustomChunkPngText.SetKeyword(const Value: TPascalString);
begin
  if FKeyword <> Value then
    begin
      FKeyword := Value;
      KeywordChanged;
    end;
end;

procedure TCustomChunkPngText.SetText(const Value: TPascalString);
begin
  if FText <> Value then
    begin
      FText := Value;
      TextChanged;
    end;
end;

procedure TCustomChunkPngText.KeywordChanged;
begin
  // yet empty
end;

procedure TCustomChunkPngText.TextChanged;
begin
  // yet empty
end;

class function TChunkPngText.GetClassChunkName: TChunkName;
begin
  Result := MakeChunkName('tEXt');
end;

function TChunkPngText.GetChunkSize: Cardinal;
begin
  Result := Length(FKeyword.ANSI) + Length(FText.ANSI) + 1;
end;

procedure TChunkPngText.ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal);
var
  Index: TGeoInt;
  tmp: TBytes;
begin
  with Stream do
    begin
      // read keyword
      FKeyword := ReadPNGStringFromStream(Stream, 80);

      // read text
      Index := 0;
      Setlength(tmp, Size - Position);
      while (Position < Size) do
        begin
          Read(tmp[Index], SizeOf(Byte));
          Inc(Index);
        end;
      FText.ANSI := tmp;
    end;
end;

procedure TChunkPngText.WriteToStream(Stream: TCoreClassStream);
var
  tmp: TBytes;
begin
  // write keyword
  WritePNGStringToStream(FKeyword, Stream);

  // write text
  tmp := FText.ANSI;
  Stream.Write(tmp[0], Length(tmp));
end;

procedure TChunkPngCompressedText.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TChunkPngCompressedText then
    with TChunkPngCompressedText(Dest) do
      begin
        FCompressionMethod := Self.FCompressionMethod;
      end
  else
      inherited;
end;

class function TChunkPngCompressedText.GetClassChunkName: TChunkName;
begin
  Result := MakeChunkName('zTXt');
end;

function TChunkPngCompressedText.GetChunkSize: Cardinal;
var
  OutputStream: TMemoryStream64;
  tmp: TBytes;
begin
  OutputStream := TMemoryStream64.Create;
  try
    // compress text
    tmp := FText.ANSI;
    ZCompress(@tmp[0], Length(tmp), OutputStream, Z_DEFAULT_COMPRESSION);
    Setlength(tmp, 0);

    // calculate chunk size
    Result := Length(FKeyword.ANSI) + OutputStream.Size + 1;
  finally
      DisposeObjectAndNil(OutputStream);
  end;
end;

procedure TChunkPngCompressedText.ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal);
var
  DataIn: Pointer;
  DataInSize: TGeoInt;
  Output: TMemoryStream64;
  tmp: TBytes;
begin
  inherited;

  with Stream do
    begin
      // read keyword
      FKeyword := ReadPNGStringFromStream(Stream, 80);

      // read compression method
      Read(FCompressionMethod, SizeOf(Byte));

      // read text
      if FCompressionMethod = 0 then
        begin
          DataInSize := Size - Position;
          DataIn := System.GetMemory(DataInSize);
          try
            Read(DataIn^, DataInSize);

            Output := TMemoryStream64.Create;
            try
              ZDecompress(DataIn, DataInSize, Output);
              Setlength(tmp, Output.Size);
              CopyPtr(Output.Memory, @tmp[0], Output.Size);
              FText.ANSI := tmp;
              Setlength(tmp, 0);
            finally
                DisposeObjectAndNil(Output);
            end;
          finally
              System.FreeMemory(DataIn);
          end;
        end;
    end;
end;

procedure TChunkPngCompressedText.WriteToStream(Stream: TCoreClassStream);
var
  OutputStream: TMemoryStream64;
  tmp: TBytes;
begin
  OutputStream := TMemoryStream64.Create;
  try
    // compress text
    tmp := FText.ANSI;
    ZCompress(@tmp[0], Length(tmp), OutputStream, Z_DEFAULT_COMPRESSION);

    with Stream do
      begin
        // write keyword
        WritePNGStringToStream(FKeyword, Stream);

        // write compression method
        Write(FCompressionMethod, SizeOf(Byte));

        // write text
        Write(OutputStream.Memory^, OutputStream.Size);
      end;
  finally
      DisposeObjectAndNil(OutputStream);
  end;
end;

procedure TChunkPngInternationalText.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TChunkPngInternationalText then
    with TChunkPngInternationalText(Dest) do
      begin
        FCompressionMethod := Self.FCompressionMethod;
        FCompressionFlag := Self.FCompressionFlag;
        FLanguageString := Self.FLanguageString;
        FTranslatedKeyword := Self.FTranslatedKeyword;
      end
  else
      inherited;
end;

class function TChunkPngInternationalText.GetClassChunkName: TChunkName;
begin
  Result := MakeChunkName('iTXt');
end;

function TChunkPngInternationalText.GetChunkSize: Cardinal;
begin
  Result := 0;
end;

procedure TChunkPngInternationalText.ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal);
var
  Index: TGeoInt;
begin
  inherited;

  with Stream do
    begin
      // read keyword
      FKeyword := ReadPNGStringFromStream(Stream, 80);

      // read compression flag
      Read(FCompressionFlag, SizeOf(Byte));

      // read compression method
      Read(FCompressionMethod, SizeOf(Byte));

      // read language U_String
      FLanguageString := ReadPNGStringFromStream(Stream, 10);

      // yet todo!
      Exit;
    end;
end;

procedure TChunkPngInternationalText.WriteToStream(Stream: TCoreClassStream);
begin
  raise EPngError.Create(RCStrNotYetImplemented);
end;

constructor TChunkPngImageData.Create(Header_: TChunkPngImageHeader);
begin
  inherited;
  FData := TMemoryStream64.Create;
end;

destructor TChunkPngImageData.Destroy;
begin
  DisposeObjectAndNil(FData);
  inherited;
end;

procedure TChunkPngImageData.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TChunkPngImageData then
    with TChunkPngImageData(Dest) do
      begin
        FData.Seek(0, TSeekOrigin.soBeginning);
        Self.FData.Seek(0, TSeekOrigin.soBeginning);
        FData.CopyFrom(Self.FData, Self.FData.Size);
        FData.Seek(0, TSeekOrigin.soBeginning);
      end
  else
      inherited;
end;

class function TChunkPngImageData.GetClassChunkName: TChunkName;
begin
  Result := MakeChunkName('IDAT');
end;

function TChunkPngImageData.GetChunkSize: Cardinal;
begin
  Result := FData.Size;
end;

procedure TChunkPngImageData.ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal);
begin
  inherited;

  FData.CopyFrom(Stream, ChunkSize_);
end;

procedure TChunkPngImageData.WriteToStream(Stream: TCoreClassStream);
begin
  FData.Seek(0, TSeekOrigin.soBeginning);
  Stream.CopyFrom(FData, FData.Size);
end;

procedure TChunkPngTime.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TChunkPngTime then
    with TChunkPngTime(Dest) do
      begin
        FYear := Self.FYear;
        FMonth := Self.FMonth;
        FDay := Self.FDay;
        FHour := Self.FHour;
        FMinute := Self.FMinute;
        FSecond := Self.FSecond;
      end
  else
      inherited;
end;

class function TChunkPngTime.GetClassChunkName: TChunkName;
begin
  Result := MakeChunkName('tIME');
end;

function TChunkPngTime.GetModifiedDateTime: TDateTime;
begin
  Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Second, 0);
end;

function TChunkPngTime.GetChunkSize: Cardinal;
begin
  Result := 7;
end;

procedure TChunkPngTime.ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal);
begin
  with Stream do
    begin
      if (ChunkSize_ > Size) or (GetChunkSize > ChunkSize_) then
          raise EPngError.Create(RCStrChunkSizeTooSmall);

      // read year
      FYear := ReadSwappedWord(Stream);

      // read month
      Read(FMonth, SizeOf(Byte));

      // read day
      Read(FDay, SizeOf(Byte));

      // read hour
      Read(FHour, SizeOf(Byte));

      // read minute
      Read(FMinute, SizeOf(Byte));

      // read second
      Read(FSecond, SizeOf(Byte));
    end;
end;

procedure TChunkPngTime.WriteToStream(Stream: TCoreClassStream);
begin
  with Stream do
    begin
      // write year
      WriteSwappedWord(Stream, FYear);

      // write month
      Write(FMonth, SizeOf(Byte));

      // write day
      Write(FDay, SizeOf(Byte));

      // write hour
      Write(FHour, SizeOf(Byte));

      // write minute
      Write(FMinute, SizeOf(Byte));

      // write second
      Write(FSecond, SizeOf(Byte));
    end;
end;

procedure TChunkPngTime.SetModifiedDateTime(const Value: TDateTime);
var
  mnth_: Word;
  Day_: Word;
  Hour_: Word;
  min_: Word;
  sec_: Word;
  msec_: Word;
begin
  DecodeDate(Value, FYear, mnth_, Day_);
  FMonth := mnth_;
  FDay := Day_;
  DecodeTime(Value, Hour_, min_, sec_, msec_);
  FHour := Hour_;
  FMinute := min_;
  FSecond := sec_;
end;

procedure TChunkPngEmbeddedIccProfile.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TChunkPngEmbeddedIccProfile then
    with TChunkPngEmbeddedIccProfile(Dest) do
      begin
        FProfileName := Self.FProfileName;
        FCompressionMethod := Self.FCompressionMethod;
      end
  else
      inherited;
end;

class function TChunkPngEmbeddedIccProfile.GetClassChunkName: TChunkName;
begin
  Result := MakeChunkName('iCCP');
end;

function TChunkPngEmbeddedIccProfile.GetChunkSize: Cardinal;
begin
  Result := Length(FProfileName.ANSI) + 2;
end;

procedure TChunkPngEmbeddedIccProfile.ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal);
begin
  with Stream do
    begin
      // read keyword
      FProfileName := ReadPNGStringFromStream(Stream, 80);

      // read compression method
      Read(FCompressionMethod, 1);

      // not yet completed
    end;
end;

procedure TChunkPngEmbeddedIccProfile.WriteToStream(Stream: TCoreClassStream);
begin
  with Stream do
    begin
      // write keyword
      WritePNGStringToStream(FProfileName, Stream);

      // write compression method
      Write(FCompressionMethod, 1);
    end;
end;

procedure TChunkPngGamma.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TChunkPngGamma then
    with TChunkPngGamma(Dest) do
      begin
        FGamma := Self.FGamma;
      end
  else
      inherited;
end;

class function TChunkPngGamma.GetClassChunkName: TChunkName;
begin
  Result := MakeChunkName('gAMA');
end;

function TChunkPngGamma.GetGammaAsSingle: TGeoFloat;
begin
  Result := FGamma * 1E-5;
end;

procedure TChunkPngGamma.SetGammaAsSingle(const Value: TGeoFloat);
begin
  FGamma := Round(Value * 1E5);
end;

function TChunkPngGamma.GetChunkSize: Cardinal;
begin
  Result := 4;
end;

procedure TChunkPngGamma.ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal);
begin
  with Stream do
    begin
      if (ChunkSize_ > Size) or (GetChunkSize > ChunkSize_) then
          raise EPngError.Create(RCStrChunkSizeTooSmall);

      // read gamma
      FGamma := ReadSwappedCardinal(Stream);
    end;
end;

procedure TChunkPngGamma.WriteToStream(Stream: TCoreClassStream);
begin
  with Stream do
    begin
      // write gamma
      WriteSwappedCardinal(Stream, FGamma);
    end;
end;

procedure TChunkPngStandardColorSpaceRGB.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TChunkPngStandardColorSpaceRGB then
    with TChunkPngStandardColorSpaceRGB(Dest) do
      begin
        FRenderingIntent := Self.FRenderingIntent;
      end
  else
      inherited;
end;

class function TChunkPngStandardColorSpaceRGB.GetClassChunkName: TChunkName;
begin
  Result := MakeChunkName('sRGB');
end;

function TChunkPngStandardColorSpaceRGB.GetChunkSize: Cardinal;
begin
  Result := 1;
end;

procedure TChunkPngStandardColorSpaceRGB.ReadFromStream(Stream: TCoreClassStream;
  ChunkSize_: Cardinal);
begin
  with Stream do
    begin
      if (ChunkSize_ > Size) or (GetChunkSize > ChunkSize_) then
          raise EPngError.Create(RCStrChunkSizeTooSmall);

      // read rendering intent
      Read(FRenderingIntent, SizeOf(Byte));
    end;
end;

procedure TChunkPngStandardColorSpaceRGB.WriteToStream(Stream: TCoreClassStream);
begin
  // write rendering intent
  Stream.Write(FRenderingIntent, SizeOf(Byte));
end;

class function TChunkPngPrimaryChromaticities.GetClassChunkName: TChunkName;
begin
  Result := MakeChunkName('cHRM');
end;

procedure TChunkPngPrimaryChromaticities.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TChunkPngPrimaryChromaticities then
    with TChunkPngPrimaryChromaticities(Dest) do
      begin
        FWhiteX := Self.FWhiteX;
        FWhiteY := Self.FWhiteY;
        FRedX := Self.FRedX;
        FRedY := Self.FRedY;
        FGreenX := Self.FGreenX;
        FGreenY := Self.FGreenY;
        FBlueX := Self.FBlueX;
        FBlueY := Self.FBlueY;
      end
  else
      inherited;
end;

function TChunkPngPrimaryChromaticities.GetBlueX: TGeoFloat;
begin
  Result := FBlueX * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetBlueY: TGeoFloat;
begin
  Result := FBlueY * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetGreenX: TGeoFloat;
begin
  Result := FGreenX * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetGreenY: TGeoFloat;
begin
  Result := FGreenY * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetRedX: TGeoFloat;
begin
  Result := FRedX * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetRedY: TGeoFloat;
begin
  Result := FRedY * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetWhiteX: TGeoFloat;
begin
  Result := FWhiteX * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetWhiteY: TGeoFloat;
begin
  Result := FWhiteY * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetChunkSize: Cardinal;
begin
  Result := 32;
end;

procedure TChunkPngPrimaryChromaticities.ReadFromStream(Stream: TCoreClassStream;
  ChunkSize_: Cardinal);
begin
  with Stream do
    begin
      if (ChunkSize_ > Size) or (GetChunkSize > ChunkSize_) then
          raise EPngError.Create(RCStrChunkSizeTooSmall);

      // read white point x
      FWhiteX := ReadSwappedCardinal(Stream);

      // read white point y
      FWhiteY := ReadSwappedCardinal(Stream);

      // read red x
      FRedX := ReadSwappedCardinal(Stream);

      // read red y
      FRedY := ReadSwappedCardinal(Stream);

      // read green x
      FGreenX := ReadSwappedCardinal(Stream);

      // read green y
      FGreenY := ReadSwappedCardinal(Stream);

      // read blue x
      FBlueX := ReadSwappedCardinal(Stream);

      // read blue y
      FBlueY := ReadSwappedCardinal(Stream);
    end;
end;

procedure TChunkPngPrimaryChromaticities.WriteToStream(Stream: TCoreClassStream);
begin
  with Stream do
    begin
      if (ChunkSize > Size) or (GetChunkSize > ChunkSize) then
          raise EPngError.Create(RCStrChunkSizeTooSmall);

      // write white point x
      WriteSwappedCardinal(Stream, FWhiteX);

      // write white point y
      WriteSwappedCardinal(Stream, FWhiteY);

      // write red x
      WriteSwappedCardinal(Stream, FRedX);

      // write red y
      WriteSwappedCardinal(Stream, FRedY);

      // write green x
      WriteSwappedCardinal(Stream, FGreenX);

      // write green y
      WriteSwappedCardinal(Stream, FGreenY);

      // write blue x
      WriteSwappedCardinal(Stream, FBlueX);

      // write blue y
      WriteSwappedCardinal(Stream, FBlueY);
    end;
end;

procedure TChunkPngPrimaryChromaticities.SetBlueX(const Value: TGeoFloat);
begin
  FBlueX := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetBlueY(const Value: TGeoFloat);
begin
  FBlueY := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetGreenX(const Value: TGeoFloat);
begin
  FGreenX := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetGreenY(const Value: TGeoFloat);
begin
  FGreenY := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetRedX(const Value: TGeoFloat);
begin
  FRedX := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetRedY(const Value: TGeoFloat);
begin
  FRedY := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetWhiteX(const Value: TGeoFloat);
begin
  FWhiteX := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetWhiteY(const Value: TGeoFloat);
begin
  FWhiteY := Round(Value * 1E6);
end;

constructor TPngSignificantBitsFormat0.Create(BitDepth: TGeoInt = 8);
begin
  inherited;
  FGrayBits := BitDepth;
end;

procedure TPngSignificantBitsFormat0.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TPngSignificantBitsFormat0 then
    with TPngSignificantBitsFormat0(Dest) do
      begin
        FGrayBits := Self.FGrayBits;
      end
  else
      inherited;
end;

class function TPngSignificantBitsFormat0.GetChunkSize: Cardinal;
begin
  Result := 1;
end;

procedure TPngSignificantBitsFormat0.ReadFromStream(Stream: TCoreClassStream);
begin
  Stream.Read(FGrayBits, 1);
end;

procedure TPngSignificantBitsFormat0.WriteToStream(Stream: TCoreClassStream);
begin
  Stream.Write(FGrayBits, 1);
end;

constructor TPngSignificantBitsFormat23.Create(BitDepth: TGeoInt = 8);
begin
  inherited;
  FRedBits := BitDepth;
  FGreenBits := BitDepth;
  FBlueBits := BitDepth;
end;

procedure TPngSignificantBitsFormat23.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TPngSignificantBitsFormat23 then
    with TPngSignificantBitsFormat23(Dest) do
      begin
        FRedBits := Self.FRedBits;
        FBlueBits := Self.FBlueBits;
        FGreenBits := Self.FGreenBits;
      end
  else
      inherited;
end;

class function TPngSignificantBitsFormat23.GetChunkSize: Cardinal;
begin
  Result := 3;
end;

procedure TPngSignificantBitsFormat23.ReadFromStream(Stream: TCoreClassStream);
begin
  Stream.Read(FRedBits, 1);
  Stream.Read(FGreenBits, 1);
  Stream.Read(FBlueBits, 1);
end;

procedure TPngSignificantBitsFormat23.WriteToStream(Stream: TCoreClassStream);
begin
  Stream.Write(FRedBits, 1);
  Stream.Write(FGreenBits, 1);
  Stream.Write(FBlueBits, 1);
end;

constructor TPngSignificantBitsFormat4.Create(BitDepth: TGeoInt = 8);
begin
  inherited;
  FGrayBits := BitDepth;
  FAlphaBits := BitDepth;
end;

procedure TPngSignificantBitsFormat4.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TPngSignificantBitsFormat4 then
    with TPngSignificantBitsFormat4(Dest) do
      begin
        FGrayBits := Self.FGrayBits;
        FAlphaBits := Self.FAlphaBits;
      end
  else if Dest is TPngSignificantBitsFormat0 then
    with TPngSignificantBitsFormat0(Dest) do
        FGrayBits := Self.FGrayBits
  else
      inherited;
end;

class function TPngSignificantBitsFormat4.GetChunkSize: Cardinal;
begin
  Result := 2;
end;

procedure TPngSignificantBitsFormat4.ReadFromStream(Stream: TCoreClassStream);
begin
  Stream.Read(FGrayBits, 1);
  Stream.Read(FAlphaBits, 1);
end;

procedure TPngSignificantBitsFormat4.WriteToStream(Stream: TCoreClassStream);
begin
  Stream.Write(FGrayBits, 1);
  Stream.Write(FAlphaBits, 1);
end;

constructor TPngSignificantBitsFormat6.Create(BitDepth: TGeoInt = 8);
begin
  inherited;
  FRedBits := BitDepth;
  FGreenBits := BitDepth;
  FBlueBits := BitDepth;
  FAlphaBits := BitDepth;
end;

procedure TPngSignificantBitsFormat6.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TPngSignificantBitsFormat6 then
    with TPngSignificantBitsFormat6(Dest) do
      begin
        FRedBits := Self.FRedBits;
        FBlueBits := Self.FBlueBits;
        FGreenBits := Self.FGreenBits;
        FAlphaBits := Self.FAlphaBits;
      end
  else if Dest is TPngSignificantBitsFormat23 then
    with TPngSignificantBitsFormat23(Dest) do
      begin
        FRedBits := Self.FRedBits;
        FBlueBits := Self.FBlueBits;
        FGreenBits := Self.FGreenBits;
      end
  else
      inherited;
end;

class function TPngSignificantBitsFormat6.GetChunkSize: Cardinal;
begin
  Result := 4;
end;

procedure TPngSignificantBitsFormat6.ReadFromStream(Stream: TCoreClassStream);
begin
  Stream.Read(FRedBits, 1);
  Stream.Read(FGreenBits, 1);
  Stream.Read(FBlueBits, 1);
  Stream.Read(FAlphaBits, 1);
end;

procedure TPngSignificantBitsFormat6.WriteToStream(Stream: TCoreClassStream);
begin
  Stream.Write(FRedBits, 1);
  Stream.Write(FGreenBits, 1);
  Stream.Write(FBlueBits, 1);
  Stream.Write(FAlphaBits, 1);
end;

procedure TChunkPngSignificantBits.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TChunkPngSignificantBits then
    with TChunkPngSignificantBits(Dest) do
      begin
        FSignificantBits.Assign(Self.FSignificantBits);
      end
  else
      inherited;
end;

constructor TChunkPngSignificantBits.Create(Header_: TChunkPngImageHeader);
begin
  inherited;

  case Header.ColorType of
    ctGrayscale: FSignificantBits := TPngSignificantBitsFormat0.Create(Header.BitDepth);
    ctTrueColor, ctIndexedColor: FSignificantBits := TPngSignificantBitsFormat23.Create(Header.BitDepth);
    ctGrayscaleAlpha: FSignificantBits := TPngSignificantBitsFormat4.Create(Header.BitDepth);
    ctTrueColorAlpha: FSignificantBits := TPngSignificantBitsFormat6.Create(Header.BitDepth);
  end;
end;

destructor TChunkPngSignificantBits.Destroy;
begin
  if Assigned(FSignificantBits) then
      DisposeObjectAndNil(FSignificantBits);

  inherited;
end;

class function TChunkPngSignificantBits.GetClassChunkName: TChunkName;
begin
  Result := MakeChunkName('sBIT');
end;

procedure TChunkPngSignificantBits.HeaderChanged;
var
  OldSignificantBits: TCustomPngSignificantBits;
begin
  inherited;

  // store old SignificantBits object
  OldSignificantBits := FSignificantBits;

  // change SignificantBits object class
  case FHeader.ColorType of
    ctGrayscale:
      if not(FSignificantBits is TPngSignificantBitsFormat0) then
        begin
          FSignificantBits := TPngSignificantBitsFormat0.Create(FHeader.BitDepth);
          if Assigned(OldSignificantBits) then
            begin
              FSignificantBits.Assign(OldSignificantBits);
              DisposeObjectAndNil(OldSignificantBits);
            end;
        end;
    ctTrueColor, ctIndexedColor:
      if not(FSignificantBits is TPngSignificantBitsFormat23) then
        begin
          FSignificantBits := TPngSignificantBitsFormat23.Create(FHeader.BitDepth);
          if Assigned(OldSignificantBits) then
            begin
              FSignificantBits.Assign(OldSignificantBits);
              DisposeObjectAndNil(OldSignificantBits);
            end;
        end;
    ctTrueColorAlpha:
      if not(FSignificantBits is TPngSignificantBitsFormat4) then
        begin
          FSignificantBits := TPngSignificantBitsFormat4.Create(FHeader.BitDepth);
          if Assigned(OldSignificantBits) then
            begin
              FSignificantBits.Assign(OldSignificantBits);
              DisposeObjectAndNil(OldSignificantBits);
            end;
        end;
    ctGrayscaleAlpha:
      if not(FSignificantBits is TPngSignificantBitsFormat6) then
        begin
          FSignificantBits := TPngSignificantBitsFormat6.Create(FHeader.BitDepth);
          if Assigned(OldSignificantBits) then
            begin
              FSignificantBits.Assign(OldSignificantBits);
              DisposeObjectAndNil(OldSignificantBits);
            end;
        end;
    else
      if Assigned(FSignificantBits) then
          DisposeObjectAndNil(FSignificantBits);
  end;
end;

function TChunkPngSignificantBits.GetChunkSize: Cardinal;
begin
  if Assigned(FSignificantBits) then
      Result := FSignificantBits.GetChunkSize
  else
      Result := 0;
end;

procedure TChunkPngSignificantBits.ReadFromStream(Stream: TCoreClassStream;
  ChunkSize_: Cardinal);
begin
  if Assigned(FSignificantBits) then
    begin
      if Stream.Size < FSignificantBits.ChunkSize_ then
          raise EPngError.Create(RCStrChunkSizeTooSmall);

      FSignificantBits.ReadFromStream(Stream);
    end;
end;

procedure TChunkPngSignificantBits.WriteToStream(Stream: TCoreClassStream);
begin
  if Assigned(FSignificantBits) then
      FSignificantBits.WriteToStream(Stream);
end;

procedure TPngBackgroundColorFormat04.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TPngBackgroundColorFormat04 then
    with TPngBackgroundColorFormat04(Dest) do
      begin
        FGraySampleValue := Self.FGraySampleValue;
      end
  else
      inherited;
end;

class function TPngBackgroundColorFormat04.GetChunkSize: Cardinal;
begin
  Result := 2;
end;

procedure TPngBackgroundColorFormat04.ReadFromStream(Stream: TCoreClassStream);
begin
  FGraySampleValue := ReadSwappedWord(Stream);
end;

procedure TPngBackgroundColorFormat04.WriteToStream(Stream: TCoreClassStream);
begin
  WriteSwappedWord(Stream, FGraySampleValue);
end;

procedure TPngBackgroundColorFormat26.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TPngBackgroundColorFormat26 then
    with TPngBackgroundColorFormat26(Dest) do
      begin
        FRedSampleValue := Self.FRedSampleValue;
        FBlueSampleValue := Self.FBlueSampleValue;
        FGreenSampleValue := Self.FGreenSampleValue;
      end
  else
      inherited;
end;

class function TPngBackgroundColorFormat26.GetChunkSize: Cardinal;
begin
  Result := 6;
end;

procedure TPngBackgroundColorFormat26.ReadFromStream(Stream: TCoreClassStream);
begin
  FRedSampleValue := ReadSwappedWord(Stream);
  FGreenSampleValue := ReadSwappedWord(Stream);
  FBlueSampleValue := ReadSwappedWord(Stream);
end;

procedure TPngBackgroundColorFormat26.WriteToStream(Stream: TCoreClassStream);
begin
  WriteSwappedWord(Stream, FRedSampleValue);
  WriteSwappedWord(Stream, FGreenSampleValue);
  WriteSwappedWord(Stream, FBlueSampleValue);
end;

procedure TPngBackgroundColorFormat3.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TPngBackgroundColorFormat3 then
    with TPngBackgroundColorFormat3(Dest) do
      begin
        FIndex := Self.FIndex;
      end
  else
      inherited;
end;

class function TPngBackgroundColorFormat3.GetChunkSize: Cardinal;
begin
  Result := 1;
end;

procedure TPngBackgroundColorFormat3.ReadFromStream(Stream: TCoreClassStream);
begin
  Stream.Read(FIndex, 1);
end;

procedure TPngBackgroundColorFormat3.WriteToStream(Stream: TCoreClassStream);
begin
  Stream.Write(FIndex, 1);
end;

procedure TChunkPngBackgroundColor.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TChunkPngBackgroundColor then
    with TChunkPngBackgroundColor(Dest) do
      begin
        FBackground.Assign(Self.FBackground);
      end
  else
      inherited;
end;

constructor TChunkPngBackgroundColor.Create(Header_: TChunkPngImageHeader);
begin
  inherited;

  case Header.ColorType of
    ctGrayscale, ctGrayscaleAlpha: FBackground := TPngBackgroundColorFormat04.Create;
    ctTrueColor, ctTrueColorAlpha: FBackground := TPngBackgroundColorFormat26.Create;
    ctIndexedColor: FBackground := TPngBackgroundColorFormat3.Create;
  end;
end;

destructor TChunkPngBackgroundColor.Destroy;
begin
  if Assigned(FBackground) then
      DisposeObjectAndNil(FBackground);
  inherited;
end;

class function TChunkPngBackgroundColor.GetClassChunkName: TChunkName;
begin
  Result := MakeChunkName('bKGD');
end;

procedure TChunkPngBackgroundColor.HeaderChanged;
var
  OldBackground: TCustomPngBackgroundColor;
begin
  inherited;

  // store old background object
  OldBackground := FBackground;

  // change background object class
  case FHeader.ColorType of
    ctGrayscale, ctGrayscaleAlpha:
      if not(FBackground is TPngBackgroundColorFormat04) then
        begin
          FBackground := TPngBackgroundColorFormat04.Create;
          if Assigned(OldBackground) then
            begin
              FBackground.Assign(OldBackground);
              DisposeObjectAndNil(OldBackground);
            end;
        end;
    ctTrueColor, ctTrueColorAlpha:
      if not(FBackground is TPngBackgroundColorFormat26) then
        begin
          FBackground := TPngBackgroundColorFormat26.Create;
          if Assigned(OldBackground) then
            begin
              FBackground.Assign(OldBackground);
              DisposeObjectAndNil(OldBackground);
            end;
        end;
    ctIndexedColor:
      if not(FBackground is TPngBackgroundColorFormat3) then
        begin
          FBackground := TPngBackgroundColorFormat3.Create;
          if Assigned(OldBackground) then
            begin
              FBackground.Assign(OldBackground);
              DisposeObjectAndNil(OldBackground);
            end;
        end;
    else
      if Assigned(FBackground) then
          DisposeObjectAndNil(FBackground);
  end;
end;

function TChunkPngBackgroundColor.GetChunkSize: Cardinal;
begin
  if Assigned(FBackground) then
      Result := FBackground.GetChunkSize
  else
      Result := 0;
end;

procedure TChunkPngBackgroundColor.ReadFromStream(Stream: TCoreClassStream;
  ChunkSize_: Cardinal);
begin
  if Assigned(FBackground) then
    begin
      if Stream.Size < FBackground.ChunkSize_ then
          raise EPngError.Create(RCStrChunkSizeTooSmall);

      FBackground.ReadFromStream(Stream);
    end;
end;

procedure TChunkPngBackgroundColor.WriteToStream(Stream: TCoreClassStream);
begin
  if Assigned(FBackground) then
      FBackground.WriteToStream(Stream);
end;

class function TChunkPngImageHistogram.GetClassChunkName: TChunkName;
begin
  Result := MakeChunkName('hIST');
end;

function TChunkPngImageHistogram.GetCount: TGeoInt;
begin
  Result := Length(FHistogram);
end;

function TChunkPngImageHistogram.GetFrequency(Index: Cardinal): Word;
begin
  if Index < Count then
      Result := FHistogram[Index]
  else
      raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TChunkPngImageHistogram.GetChunkSize: Cardinal;
begin
  Result := Count * SizeOf(Word);
end;

procedure TChunkPngImageHistogram.ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal);
var
  Index: TGeoInt;
begin
  // check size
  if (ChunkSize_ > Stream.Size) or (GetChunkSize > ChunkSize_) then
      raise EPngError.Create(RCStrChunkSizeTooSmall);

  // adjust histogram array size
  Setlength(FHistogram, ChunkSize_ div 2);

  // read histogram data
  for Index := 0 to Length(FHistogram) - 1 do
      FHistogram[Index] := ReadSwappedWord(Stream);
end;

procedure TChunkPngImageHistogram.WriteToStream(Stream: TCoreClassStream);
var
  Index: TGeoInt;
begin
  // write histogram data
  for Index := 0 to Length(FHistogram) - 1 do
      WriteSwappedWord(Stream, FHistogram[Index]);
end;

constructor TChunkPngSuggestedPalette.Create(Header_: TChunkPngImageHeader);
begin
  inherited;
  FData := nil;
  FCount := 0;
end;

class function TChunkPngSuggestedPalette.GetClassChunkName: TChunkName;
begin
  Result := MakeChunkName('sPLT');
end;

function TChunkPngSuggestedPalette.GetCount: TGeoInt;
begin
  Result := FCount;
end;

function TChunkPngSuggestedPalette.GetChunkSize: Cardinal;
begin
  Result := Cardinal(Length(FPaletteName.ANSI)) + 2 +
    (4 * (FSampleDepth shr 3) + 2) * Count;
end;

procedure TChunkPngSuggestedPalette.ReadFromStream(Stream: TCoreClassStream; ChunkSize_: Cardinal);
var
  Index: TGeoInt;
  DataSize: TGeoInt;
begin
  with Stream do
    begin
      if (ChunkSize_ > Size) or (GetChunkSize > ChunkSize_) then
          raise EPngError.Create(RCStrChunkSizeTooSmall);

      // read palette name
      FPaletteName := ReadPNGStringFromStream(Stream, 80);

      // read sample depth
      Read(FSampleDepth, 1);

      DataSize := TGeoInt(ChunkSize_) - Length(FPaletteName.ANSI) - 2;
      Assert(DataSize >= 0);
      Assert(DataSize mod 2 = 0);
      Assert(DataSize mod (4 * (FSampleDepth shr 3) + 2) = 0);
      FCount := DataSize div (4 * (FSampleDepth shr 3) + 2);
      ReallocMem(FData, DataSize);

      if FSampleDepth = 8 then
        for Index := 0 to FCount - 1 do
          with PSuggestedPalette8ByteArray(FData)^[Index] do
            begin
              Read(Red, 1);
              Read(Green, 1);
              Read(Blue, 1);
              Read(Alpha, 1);
              Frequency := ReadSwappedWord(Stream);
            end
      else if FSampleDepth = 16 then
        for Index := 0 to FCount - 1 do
          with PSuggestedPalette16ByteArray(FData)^[Index] do
            begin
              Red := ReadSwappedWord(Stream);
              Green := ReadSwappedWord(Stream);
              Blue := ReadSwappedWord(Stream);
              Alpha := ReadSwappedWord(Stream);
              Frequency := ReadSwappedWord(Stream);
            end;
    end;
end;

procedure TChunkPngSuggestedPalette.WriteToStream(Stream: TCoreClassStream);
begin
  raise EPngError.Create(RCStrNotYetImplemented);

  // yet todo
end;

constructor TChunkList.Create();
begin
  inherited Create;
  Setlength(FChunks, 0);
end;

destructor TChunkList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TChunkList.Add(Item: TCustomChunk);
begin
  Setlength(FChunks, Length(FChunks) + 1);
  FChunks[Length(FChunks) - 1] := Item;
end;

procedure TChunkList.AssignTo(Dest: TCoreClassPersistent);
var
  Index: TGeoInt;
  ChunkClass: TCustomDefinedChunkWithHeaderClass;
begin
  if Dest is TChunkList then
    with TChunkList(Dest) do
      begin
        Clear;
        Setlength(FChunks, Self.Count);
        for Index := 0 to Self.Count - 1 do
          if Self.FChunks[Index] is TCustomDefinedChunkWithHeader then
            begin
              ChunkClass := TCustomDefinedChunkWithHeaderClass(Self.FChunks[Index].ClassType);
              FChunks[Index] := ChunkClass.Create(TCustomDefinedChunkWithHeader(Self.FChunks[Index]).FHeader);
              FChunks[Index].Assign(Self.FChunks[Index]);
            end
          else
              inherited;
      end
  else
      inherited;
end;

procedure TChunkList.Clear;
var
  Index: TGeoInt;
begin
  for Index := 0 to Count - 1 do
      DisposeObjectAndNil(FChunks[Index]);
  Setlength(FChunks, 0)
end;

procedure TChunkList.Delete(Index: Cardinal);
begin
  if Index >= Count then
      raise EPngError.Create(RCStrEmptyChunkList);
  DisposeObjectAndNil(FChunks[Index]);
  if Index < Count then
      CopyPtr(@FChunks[Index + 1], @FChunks[Index], (Count - Index) * SizeOf(Pointer));
  Setlength(FChunks, Length(FChunks) - 1);
end;

function TChunkList.GetChunk(Index: TGeoInt): TCustomChunk;
begin
  if Cardinal(Index) >= Cardinal(Count) then
      raise EPngError.CreateFmt(RCStrIndexOutOfBounds, [Index])
  else
      Result := FChunks[Index];
end;

function TChunkList.GetCount: TGeoInt;
begin
  Result := Length(FChunks);
end;

function TChunkList.IndexOf(Item: TCustomChunk): TGeoInt;
begin
  for Result := 0 to Count - 1 do
    if FChunks[Result] = Item then
        Exit;
  Result := -1;
end;

procedure TChunkList.Remove(Item: TCustomChunk);
begin
  Delete(IndexOf(Item));
end;

constructor TCustomPngCoder.Create(Stream: TCoreClassStream;
  Header: TChunkPngImageHeader; Gamma: TChunkPngGamma = nil;
  Palette: TChunkPngPalette = nil; Transparency: TCustomPngTransparency = nil);
begin
  FStream := Stream;
  FHeader := Header;
  FGamma := Gamma;
  FPalette := Palette;
  FTransparency := Transparency;
  FMappingTable := nil;
  FAlphaTable := nil;
  BuildMappingTables;
  inherited Create;
end;

destructor TCustomPngCoder.Destroy;
begin
  Dispose(FMappingTable);
  Dispose(FAlphaTable);
  inherited;
end;

procedure TCustomPngCoder.BuildMappingTables;
var
  Index: TGeoInt;
  Palette: PRGB24Array;
  FracVal: TGeoFloat;
  Color: TRGB24;
  MaxByte: Byte;
  PreCalcGamma: Extended;
const
  COne255th: Extended = 1 / $FF;
begin
  if FHeader.HasPalette then
    begin
      if Assigned(FPalette) then
        begin
          FMappingTable := System.GetMemory(FPalette.Count * SizeOf(TRGB24));
          Palette := PRGB24Array(FMappingTable);

          if Assigned(FGamma) then
            begin
              PreCalcGamma := 1 / (FGamma.Gamma * 2.2E-5);
              for Index := 0 to FPalette.Count - 1 do
                begin
                  Color := FPalette.PaletteEntry[Index];
                  Palette^[Index].R := RoundAsByte(Power((Color.R * COne255th), PreCalcGamma) * $FF);
                  Palette^[Index].G := RoundAsByte(Power((Color.G * COne255th), PreCalcGamma) * $FF);
                  Palette^[Index].B := RoundAsByte(Power((Color.B * COne255th), PreCalcGamma) * $FF);
                end;
            end
          else
            for Index := 0 to FPalette.Count - 1 do
                Palette^[Index] := FPalette.PaletteEntry[Index];
        end
      else
        begin
          // create gray scale palette
          FMappingTable := System.GetMemory(256 * SizeOf(TRGB24));
          Palette := PRGB24Array(FMappingTable);
          MaxByte := ((1 shl FHeader.BitDepth) - 1) and $FF;
          FracVal := 1 / MaxByte;

          if Assigned(FGamma) then
            begin
              PreCalcGamma := 1 / (FGamma.Gamma * 2.2E-5);
              for Index := 0 to FPalette.Count - 1 do
                begin
                  Palette^[Index].R := RoundAsByte(Power(Index * FracVal, PreCalcGamma) * $FF);
                  Palette^[Index].G := Palette^[Index].R;
                  Palette^[Index].B := Palette^[Index].B;
                end;
            end
          else
            begin
              for Index := 0 to MaxByte do
                begin
                  Palette^[Index].R := RoundAsByte($FF * (Index * FracVal));
                  Palette^[Index].G := Palette^[Index].R;
                  Palette^[Index].B := Palette^[Index].R;
                end;
            end;
        end;

      // build alpha table
      FAlphaTable := System.GetMemory(256);
      FillPtr(@FAlphaTable^, 256, $FF);

      // eventually fill alpha table
      if FTransparency is TPngTransparencyFormat3 then
        with TPngTransparencyFormat3(FTransparency) do
          for Index := 0 to Count - 1 do
              FAlphaTable^[Index] := Transparency[Index];
    end
  else
    begin
      FMappingTable := System.GetMemory(256);
      if Assigned(FGamma) and (FGamma.Gamma <> 0) then
        begin
          PreCalcGamma := 1 / (FGamma.Gamma * 2.2E-5);
          for Index := 0 to $FF do
              FMappingTable^[Index] := RoundAsByte(Power((Index * COne255th), PreCalcGamma) * $FF);
        end
      else
        for Index := 0 to $FF do
            FMappingTable^[Index] := Index;
    end;
end;

procedure TCustomPngCoder.DecodeFilterSub(CurrentRow, PreviousRow: PPNGByteArray; BytesPerRow, PixelByteSize: NativeInt);
var
  Index: TGeoInt;
begin
  for Index := PixelByteSize + 1 to BytesPerRow do
      CurrentRow^[Index] := (CurrentRow^[Index] + CurrentRow^[Index - PixelByteSize]) and $FF;
end;

procedure TCustomPngCoder.DecodeFilterUp(CurrentRow, PreviousRow: PPNGByteArray; BytesPerRow, PixelByteSize: NativeInt);
var
  Index: TGeoInt;
begin
  for Index := 1 to BytesPerRow do
      CurrentRow^[Index] := (CurrentRow^[Index] + PreviousRow^[Index]) and $FF;
end;

procedure TCustomPngCoder.DecodeFilterAverage(CurrentRow, PreviousRow: PPNGByteArray; BytesPerRow, PixelByteSize: NativeInt);
var
  Index: TGeoInt;
begin
  for Index := 1 to PixelByteSize do
      CurrentRow^[Index] := (CurrentRow^[Index] + PreviousRow^[Index] shr 1) and $FF;

  for Index := PixelByteSize + 1 to BytesPerRow do
      CurrentRow^[Index] := (CurrentRow^[Index] +
      (CurrentRow^[Index - PixelByteSize] + PreviousRow^[Index]) shr 1) and $FF;
end;

function PaethPredictor(A, B, c: Byte): TGeoInt;
var
  DistA, DistB, DistC: TGeoInt;
begin
  DistA := Abs(B - c);
  DistB := Abs(A - c);
  DistC := Abs(A + B - c * 2);

  if (DistA <= DistB) and (DistA <= DistC) then
      Result := A
  else
    if DistB <= DistC then
      Result := B
  else
      Result := c;
end;

procedure TCustomPngCoder.DecodeFilterPaeth(CurrentRow, PreviousRow: PPNGByteArray;
  BytesPerRow, PixelByteSize: NativeInt);
var
  Index: TGeoInt;
begin
  DecodeFilterUp(CurrentRow, PreviousRow, PixelByteSize, PixelByteSize);

  for Index := PixelByteSize + 1 to BytesPerRow do
      CurrentRow^[Index] := (CurrentRow^[Index] +
      PaethPredictor(CurrentRow^[Index - PixelByteSize], PreviousRow^[Index],
      PreviousRow^[Index - PixelByteSize])) and $FF;
end;

procedure TCustomPngCoder.EncodeFilterSub(CurrentRow, PreviousRow, OutputRow: PPNGByteArray;
  BytesPerRow, PixelByteSize: TGeoInt);
var
  Index: TGeoInt;
begin
  // copy first pixel
  CopyPtr(@CurrentRow^[1], @OutputRow^[1], PixelByteSize);

  for Index := PixelByteSize + 1 to BytesPerRow do
      OutputRow^[Index] := (CurrentRow^[Index] - CurrentRow^[Index - PixelByteSize]) and $FF;
end;

procedure TCustomPngCoder.EncodeFilterUp(CurrentRow, PreviousRow, OutputRow: PPNGByteArray;
  BytesPerRow, PixelByteSize: TGeoInt);
var
  Index: TGeoInt;
begin
  for Index := 1 to BytesPerRow do
      OutputRow^[Index] := (CurrentRow^[Index] - PreviousRow^[Index]) and $FF;
end;

procedure TCustomPngCoder.EncodeFilterAverage(CurrentRow, PreviousRow, OutputRow: PPNGByteArray;
  BytesPerRow, PixelByteSize: TGeoInt);
var
  Index: TGeoInt;
begin
  for Index := 1 to PixelByteSize do
      OutputRow^[Index] := (CurrentRow^[Index] - PreviousRow^[Index] shr 1) and $FF;

  for Index := PixelByteSize + 1 to BytesPerRow do
      OutputRow^[Index] := (CurrentRow^[Index] - (CurrentRow^[Index - PixelByteSize] + PreviousRow^[Index]) shr 1) and $FF;
end;

procedure TCustomPngCoder.EncodeFilterPaeth(CurrentRow, PreviousRow, OutputRow: PPNGByteArray;
  BytesPerRow, PixelByteSize: TGeoInt);
var
  Index: TGeoInt;
begin
  EncodeFilterUp(CurrentRow, PreviousRow, OutputRow, PixelByteSize, PixelByteSize);

  for Index := PixelByteSize + 1 to BytesPerRow do
      OutputRow^[Index] := (CurrentRow^[Index] -
      PaethPredictor(CurrentRow^[Index - PixelByteSize], PreviousRow^[Index],
      PreviousRow^[Index - PixelByteSize])) and $FF;
end;

procedure TCustomPngDecoder.DecodeFilterRow(FilterMethod: TAdaptiveFilterMethod;
  CurrentRow, PreviousRow: PPNGByteArray; BytesPerRow, PixelByteSize: TGeoInt);
begin
  case FilterMethod of
    afmNone:;
    afmSub: DecodeFilterSub(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);
    afmUp: DecodeFilterUp(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);
    afmAverage: DecodeFilterAverage(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);
    afmPaeth: DecodeFilterPaeth(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);
    else
      raise EPngError.Create(RCStrUnsupportedFilter);
  end;
end;

procedure TCustomPngDecoder.EncodeFilterRow(CurrentRow, PreviousRow, OutputRow,
  TempBuffer: PPNGByteArray; BytesPerRow, PixelByteSize: TGeoInt);
begin
  raise Exception.Create('Class is only meant for decoding');
end;

function CalculateRowSum(CurrentRow: PPNGByteArray; BytesPerRow: TGeoInt): Cardinal;
var
  Index: TGeoInt;
begin
  Result := 0;
  for Index := 1 to BytesPerRow do
      Result := Result + Cardinal(Abs(SmallInt(CurrentRow^[Index])));
end;

procedure TCustomPngEncoder.EncodeFilterRow(CurrentRow, PreviousRow,
  OutputRow, TempBuffer: PPNGByteArray; BytesPerRow, PixelByteSize: TGeoInt);
var
  PixelIndex: TGeoInt;
  CurrentSum: Cardinal;
  BestSum: Cardinal;
begin
  BestSum := 0;
  OutputRow^[0] := 0;
  for PixelIndex := 1 to BytesPerRow do
      BestSum := BestSum + CurrentRow^[PixelIndex];
  CopyPtr(@CurrentRow^[1], @OutputRow^[1], BytesPerRow);

  // check whether sub pre filter shall be used
  if aafmSub in FHeader.AdaptiveFilterMethods then
    begin
      // calculate sub filter
      EncodeFilterSub(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
      CurrentSum := CalculateRowSum(TempBuffer, BytesPerRow);

      // check if sub filter is the current best filter
      if CurrentSum < BestSum then
        begin
          BestSum := CurrentSum;
          CopyPtr(@TempBuffer^[1], @OutputRow^[1], BytesPerRow);
          OutputRow^[0] := 1;
        end;
    end;

  // check whether up pre filter shall be used
  if aafmUp in FHeader.AdaptiveFilterMethods then
    begin
      // calculate up filter
      EncodeFilterUp(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
      CurrentSum := CalculateRowSum(TempBuffer, BytesPerRow);

      // check if up filter is the current best filter
      if CurrentSum < BestSum then
        begin
          BestSum := CurrentSum;
          CopyPtr(@TempBuffer^[1], @OutputRow^[1], BytesPerRow);
          OutputRow^[0] := 2;
        end;
    end;

  // check whether average pre filter shall be used
  if aafmAverage in FHeader.AdaptiveFilterMethods then
    begin
      // calculate average filter
      EncodeFilterAverage(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
      CurrentSum := CalculateRowSum(TempBuffer, BytesPerRow);

      // check if average filter is the current best filter
      if CurrentSum < BestSum then
        begin
          BestSum := CurrentSum;
          CopyPtr(@TempBuffer^[1], @OutputRow^[1], BytesPerRow);
          OutputRow^[0] := 3;
        end;
    end;

  // check whether paeth pre filter shall be used
  if aafmPaeth in FHeader.AdaptiveFilterMethods then
    begin
      // calculate paeth filter
      EncodeFilterPaeth(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
      CurrentSum := CalculateRowSum(TempBuffer, BytesPerRow);

      // check if paeth filter is the current best filter
      if CurrentSum < BestSum then
        begin
          CopyPtr(@TempBuffer^[1], @OutputRow^[1], BytesPerRow);
          OutputRow^[0] := 4;
        end;
    end;
end;

procedure TCustomPngEncoder.DecodeFilterRow(FilterMethod: TAdaptiveFilterMethod;
  CurrentRow, PreviousRow: PPNGByteArray; BytesPerRow, PixelByteSize: TGeoInt);
begin
  raise Exception.Create('Class is only meant for encoding');
end;

constructor TCustomPngTranscoder.Create(Stream: TCoreClassStream;
  Header: TChunkPngImageHeader; Gamma: TChunkPngGamma = nil;
  Palette: TChunkPngPalette = nil; Transparency: TCustomPngTransparency = nil);
begin
  inherited;
  FRowBuffer[0] := System.GetMemory(FHeader.BytesPerRow + 1);
  FRowBuffer[1] := System.GetMemory(FHeader.BytesPerRow + 1);
end;

destructor TCustomPngTranscoder.Destroy;
begin
  Dispose(FRowBuffer[0]);
  Dispose(FRowBuffer[1]);
  inherited;
end;

procedure TCustomPngTranscoder.DecodeFilterRow(
  FilterMethod: TAdaptiveFilterMethod; CurrentRow, PreviousRow: PPNGByteArray;
  BytesPerRow, PixelByteSize: TGeoInt);
begin
  case FilterMethod of
    afmNone:;
    afmSub: DecodeFilterSub(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);
    afmUp: DecodeFilterUp(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);
    afmAverage: DecodeFilterAverage(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);
    afmPaeth: DecodeFilterPaeth(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);
    else
      raise EPngError.Create(RCStrUnsupportedFilter);
  end;
end;

procedure TCustomPngTranscoder.EncodeFilterRow(CurrentRow, PreviousRow,
  OutputRow, TempBuffer: PPNGByteArray; BytesPerRow, PixelByteSize: TGeoInt);
var
  PixelIndex: TGeoInt;
  CurrentSum: Cardinal;
  BestSum: Cardinal;
begin
  BestSum := 0;
  OutputRow^[0] := 0;
  for PixelIndex := 1 to BytesPerRow do
      BestSum := BestSum + CurrentRow^[PixelIndex];
  CopyPtr(@CurrentRow^[1], @OutputRow^[1], BytesPerRow);

  // check whether sub pre filter shall be used
  if aafmSub in FHeader.AdaptiveFilterMethods then
    begin
      // calculate sub filter
      EncodeFilterSub(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
      CurrentSum := CalculateRowSum(TempBuffer, BytesPerRow);

      // check if sub filter is the current best filter
      if CurrentSum < BestSum then
        begin
          BestSum := CurrentSum;
          CopyPtr(@TempBuffer^[1], @OutputRow^[1], BytesPerRow);
          OutputRow^[0] := 1;
        end;
    end;

  // check whether up pre filter shall be used
  if aafmUp in FHeader.AdaptiveFilterMethods then
    begin
      // calculate up filter
      EncodeFilterUp(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
      CurrentSum := CalculateRowSum(TempBuffer, BytesPerRow);

      // check if up filter is the current best filter
      if CurrentSum < BestSum then
        begin
          BestSum := CurrentSum;
          CopyPtr(@TempBuffer^[1], @OutputRow^[1], BytesPerRow);
          OutputRow^[0] := 2;
        end;
    end;

  // check whether average pre filter shall be used
  if aafmAverage in FHeader.AdaptiveFilterMethods then
    begin
      // calculate average filter
      EncodeFilterAverage(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
      CurrentSum := CalculateRowSum(TempBuffer, BytesPerRow);

      // check if average filter is the current best filter
      if CurrentSum < BestSum then
        begin
          BestSum := CurrentSum;
          CopyPtr(@TempBuffer^[1], @OutputRow^[1], BytesPerRow);
          OutputRow^[0] := 3;
        end;
    end;

  // check whether paeth pre filter shall be used
  if aafmPaeth in FHeader.AdaptiveFilterMethods then
    begin
      // calculate paeth filter
      EncodeFilterPaeth(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
      CurrentSum := CalculateRowSum(TempBuffer, BytesPerRow);

      // check if paeth filter is the current best filter
      if CurrentSum < BestSum then
        begin
          CopyPtr(@TempBuffer^[1], @OutputRow^[1], BytesPerRow);
          OutputRow^[0] := 4;
        end;
    end;
end;

constructor TPortableNetworkGraphic.Create;
begin
  inherited;

  FImageHeader := nil;
  FPaletteChunk := nil;
  FGammaChunk := nil;
  FTimeChunk := nil;
  FSignificantBits := nil;
  FPhysicalDimensions := nil;
  FChromaChunk := nil;
  FTransparencyChunk := nil;
  FBackgroundChunk := nil;
  FDataChunkList := nil;
  FAdditionalChunkList := nil;

  FImageHeader := TChunkPngImageHeader.Create;
  FDataChunkList := TChunkList.Create;
  FAdditionalChunkList := TChunkList.Create;

  FCompressionLevel := Z_BEST_COMPRESSION;
end;

destructor TPortableNetworkGraphic.Destroy;
begin
  FAdditionalChunkList.Clear;

  DisposeObjectAndNil(FAdditionalChunkList);
  DisposeObjectAndNil(FDataChunkList);
  DisposeObjectAndNil(FImageHeader);

  // free palette chunk
  if Assigned(FPaletteChunk) then
      DisposeObjectAndNil(FPaletteChunk);

  // free gamma chunk
  if Assigned(FGammaChunk) then
      DisposeObjectAndNil(FGammaChunk);

  // free time chunk
  if Assigned(FTimeChunk) then
      DisposeObjectAndNil(FTimeChunk);

  // free time chunk
  if Assigned(FSignificantBits) then
      DisposeObjectAndNil(FSignificantBits);

  // free physical pixel dimensions chunk
  if Assigned(FPhysicalDimensions) then
      DisposeObjectAndNil(FPhysicalDimensions);

  // free primary chromaticities chunk
  if Assigned(FChromaChunk) then
      DisposeObjectAndNil(FChromaChunk);

  // free transparency chunk
  if Assigned(FTransparencyChunk) then
      DisposeObjectAndNil(FTransparencyChunk);

  // free transparency chunk
  if Assigned(FBackgroundChunk) then
      DisposeObjectAndNil(FBackgroundChunk);

  inherited;
end;

procedure TPortableNetworkGraphic.SetPaletteChunk(
  const Value: TChunkPngPalette);
begin
  if Assigned(FPaletteChunk) then
    if Assigned(Value) then
        FPaletteChunk.Assign(Value)
    else
        DisposeObjectAndNil(FPaletteChunk)
  else
    if Assigned(Value) then
    begin
      FPaletteChunk := TChunkPngPalette.Create(FImageHeader);
      FPaletteChunk.Assign(Value);
    end;
end;

procedure TPortableNetworkGraphic.SetPhysicalDimensions(
  const Value: TChunkPngPhysicalPixelDimensions);
begin
  if Assigned(FPhysicalDimensions) then
    if Assigned(Value) then
        FPhysicalDimensions.Assign(Value)
    else
        DisposeObjectAndNil(FPhysicalDimensions)
  else
    if Assigned(Value) then
    begin
      FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create(FImageHeader);
      FPhysicalDimensions.Assign(Value);
    end;
end;

procedure TPortableNetworkGraphic.SetSignificantBits(
  const Value: TChunkPngSignificantBits);
begin
  if Assigned(FSignificantBits) then
    if Assigned(Value) then
        FSignificantBits.Assign(Value)
    else
        DisposeObjectAndNil(FSignificantBits)
  else
    if Assigned(Value) then
    begin
      FSignificantBits := TChunkPngSignificantBits.Create(FImageHeader);
      FSignificantBits.Assign(Value);
    end;
end;

procedure TPortableNetworkGraphic.SetTimeChunk(const Value: TChunkPngTime);
begin
  if Assigned(FTimeChunk) then
    if Assigned(Value) then
        FTimeChunk.Assign(Value)
    else
        DisposeObjectAndNil(FTimeChunk)
  else
    if Assigned(Value) then
    begin
      FTimeChunk := TChunkPngTime.Create(FImageHeader);
      FTimeChunk.Assign(Value);
    end;
end;

procedure TPortableNetworkGraphic.SetTransparencyChunk(
  const Value: TChunkPngTransparency);
begin
  if Assigned(FTransparencyChunk) then
    if Assigned(Value) then
        FTransparencyChunk.Assign(Value)
    else
        DisposeObjectAndNil(FTransparencyChunk)
  else
    if Assigned(Value) then
    begin
      FTransparencyChunk := TChunkPngTransparency.Create(FImageHeader);
      FTransparencyChunk.Assign(Value);
    end;
end;

procedure TPortableNetworkGraphic.SetPixelsPerUnitX(const Value: Cardinal);
begin
  if Value = 0 then
      raise EPngError.Create(RCStrWrongPixelPerUnit);

  if not Assigned(FPhysicalDimensions) then
      FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create(FImageHeader);

  FPhysicalDimensions.PixelsPerUnitX := Value;
end;

procedure TPortableNetworkGraphic.SetPixelsPerUnitY(const Value: Cardinal);
begin
  if Value = 0 then
      raise EPngError.Create(RCStrWrongPixelPerUnit);

  if not Assigned(FPhysicalDimensions) then
      FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create(FImageHeader);

  FPhysicalDimensions.PixelsPerUnitY := Value;
end;

procedure TPortableNetworkGraphic.SetPixelUnit(const Value: Byte);
begin
  if Value > 1 then
      raise EPngError.Create(RCStrUnspecifiedPixelUnit);

  if not Assigned(FPhysicalDimensions) then
      FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create(FImageHeader);

  FPhysicalDimensions.PixelUnit := Value;
end;

procedure TPortableNetworkGraphic.SetChromaChunk(
  const Value: TChunkPngPrimaryChromaticities);
begin
  if Assigned(FChromaChunk) then
    if Assigned(Value) then
        FChromaChunk.Assign(Value)
    else
        DisposeObjectAndNil(FChromaChunk)
  else
    if Assigned(Value) then
    begin
      FChromaChunk := TChunkPngPrimaryChromaticities.Create(FImageHeader);
      FChromaChunk.Assign(Value);
    end;
end;

procedure TPortableNetworkGraphic.SetGammaChunk(const Value: TChunkPngGamma);
begin
  if Assigned(FGammaChunk) then
    if Assigned(Value) then
        FGammaChunk.Assign(Value)
    else
        DisposeObjectAndNil(FGammaChunk)
  else
    if Assigned(Value) then
    begin
      FGammaChunk := TChunkPngGamma.Create(FImageHeader);
      FGammaChunk.Assign(Value);
    end;
end;

procedure TPortableNetworkGraphic.SetBackgroundChunk(
  const Value: TChunkPngBackgroundColor);
begin
  if Assigned(FGammaChunk) then
    if Assigned(Value) then
        FBackgroundChunk.Assign(Value)
    else
        DisposeObjectAndNil(FBackgroundChunk)
  else
    if Assigned(Value) then
    begin
      FBackgroundChunk := TChunkPngBackgroundColor.Create(FImageHeader);
      FBackgroundChunk.Assign(Value);
    end;
end;

procedure TPortableNetworkGraphic.SetImageHeader(
  const Value: TChunkPngImageHeader);
begin
  if not Assigned(Value) then
      raise EPngError.Create(RCStrNewHeaderError)
  else
      FImageHeader.Assign(Value);
end;

procedure TPortableNetworkGraphic.SetBitDepth(const Value: Byte);
begin
  raise EPngError.CreateFmt(RCStrBitDepthTranscodingError, [Value]);
end;

procedure TPortableNetworkGraphic.SetColorType(const Value: TColorType);
begin
  raise EPngError.CreateFmt(RCStrColorTypeTranscodingError, [TGeoInt(Value)]);
end;

procedure TPortableNetworkGraphic.SetFilterMethods(
  const Value: TAvailableAdaptiveFilterMethods);
begin
  if Assigned(FImageHeader) then
    if FImageHeader.FAdaptiveFilterMethods <> Value then
      begin
        FImageHeader.FAdaptiveFilterMethods := Value;
        AdaptiveFilterMethodsChanged;
      end;
end;

procedure TPortableNetworkGraphic.SetCompressionLevel(const Value: Byte);
begin
  if not(Value in [1 .. 9]) then
      raise EPngError.Create(RCStrInvalidCompressionLevel);

  if FCompressionLevel <> Value then
    begin
      FCompressionLevel := Value;
      CompressionLevelChanged;
    end;
end;

procedure TPortableNetworkGraphic.SetCompressionMethod(const Value: Byte);
begin
  raise EPngError.CreateFmt(RCStrDirectCompressionMethodSetError, [Value]);
end;

procedure TPortableNetworkGraphic.SetFilterMethod(const Value: TFilterMethod);
begin
  raise EPngError.CreateFmt(RCStrDirectFilterMethodSetError, [TGeoInt(Value)]);
end;

procedure TPortableNetworkGraphic.SetWidth(const Value: TGeoInt);
begin
  raise EPngError.CreateFmt(RCStrDirectWidthSetError, [Value]);
end;

procedure TPortableNetworkGraphic.SetInterlaceMethod(
  const Value: TInterlaceMethod);
begin
  if Value <> FImageHeader.InterlaceMethod then
    begin
      InterlaceMethodChanged;
      FImageHeader.InterlaceMethod := Value;
    end;
end;

procedure TPortableNetworkGraphic.SetModifiedTime(const Value: TDateTime);
begin
  if Assigned(FTimeChunk) then
      FTimeChunk.ModifiedDateTime := Value;
end;

procedure TPortableNetworkGraphic.SetGamma(const Value: TGeoFloat);
begin
  raise EPngError.CreateFmt(RCStrDirectGammaSetError, [Value]);
end;

procedure TPortableNetworkGraphic.SetHeight(const Value: TGeoInt);
begin
  raise EPngError.CreateFmt(RCStrDirectHeightSetError, [Value]);
end;

procedure TPortableNetworkGraphic.CopyImageData(Stream: TCoreClassStream);
var
  DataIndex: TGeoInt;
begin
  // combine all data chunks first
  for DataIndex := 0 to FDataChunkList.Count - 1 do
    begin
      // make sure the chunk is inded an image data chunk
      Assert(FDataChunkList[DataIndex] is TChunkPngImageData);

      // concat current chunk to data stream
      with TChunkPngImageData(FDataChunkList[DataIndex]) do
        begin
          Data.Seek(0, TSeekOrigin.soBeginning);
          Stream.CopyFrom(Data, Data.Size);
        end;
    end;
end;

procedure TPortableNetworkGraphic.StoreImageData(Stream: TCoreClassStream);
var
  DataChunk: TChunkPngImageData;
  ChunkSize_: TGeoInt;
begin
  // delete old image data
  FDataChunkList.Clear;

  ChunkSize_ := Stream.Size;
  while Stream.Position < Stream.Size do
    begin
      DataChunk := TChunkPngImageData.Create(ImageHeader);

      if (Stream.Size - Stream.Position) < ChunkSize_ then
          ChunkSize_ := (Stream.Size - Stream.Position);

      // copy data to IDAT chunk
      DataChunk.Data.CopyFrom(Stream, ChunkSize_);

      // add data chunk to data chunk list
      FDataChunkList.Add(DataChunk);
    end;
end;

procedure TPortableNetworkGraphic.DecompressImageDataToStream(Stream: TCoreClassStream);
var
  DataStream: TMemoryStream64;
begin
  DataStream := TMemoryStream64.Create;
  try
    // copy image data from all data chunks to one continous data stream
    CopyImageData(DataStream);

    // check whether compression method is supported
    if FImageHeader.CompressionMethod <> 0 then
        raise EPngError.Create(RCStrUnsupportedCompressionMethod);

    // reset data stream position to zero
    DataStream.Seek(0, TSeekOrigin.soBeginning);

    // decompress z-stream
    ZDecompress(DataStream, Stream);
  finally
      DisposeObjectAndNil(DataStream);
  end;
end;

procedure TPortableNetworkGraphic.CompressImageDataFromStream(Stream: TCoreClassStream);
var
  DataStream: TMemoryStream64;
begin
  DataStream := TMemoryStream64.Create;
  try
    // set compression method
    FImageHeader.CompressionMethod := 0;

    // compress Stream to DataStream
    if Stream is TMemoryStream64 then
        ZCompress(TMemoryStream64(Stream), DataStream, FCompressionLevel)
    else
        raise EPngError.Create(RCStrNotYetImplemented);

    // reset data stream position to zero
    DataStream.Seek(0, TSeekOrigin.soBeginning);

    // copy image data from all data chunks to one continous data stream
    StoreImageData(DataStream);
  finally
      DisposeObjectAndNil(DataStream);
  end;
end;

class function TPortableNetworkGraphic.CanLoad(Stream: TCoreClassStream): Boolean;
var
  ChunkID, PNGMagic_: TChunkName;
begin
  Result := Stream.Size >= 8;

  if Result then
    begin
      Stream.Read(ChunkID, 4);
      Stream.Read(PNGMagic_, 4);
      Stream.Seek(-8, TSeekOrigin.soCurrent);
      Result := CompareChunkName(ChunkID, PNG_SIG) and CompareChunkName(PNGMagic_, CPngMagic);
    end;
end;

procedure TPortableNetworkGraphic.LoadFromStream(Stream: TCoreClassStream);
var
  ChunkName: TChunkName;
  ChunkSize_: TGeoInt;
  ChunkCRC: Cardinal;
  ChunkClass: TCustomDefinedChunkWithHeaderClass;
  Chunk: TCustomDefinedChunkWithHeader;
  MemoryStream: TMemoryStream64;
begin
  with Stream do
    begin
      Clear;

      // check for minimum file size
      if Size < 8 then
          raise EPngError.Create(RCStrNotAValidPNGFile);

      // read chunk ID
      Read(ChunkName, 4);
      if not CompareChunkName(ChunkName, PNG_SIG) then
          raise EPngError.Create(RCStrNotAValidPNGFile);

      // read PNG magic
      Read(ChunkName, 4);
      if not CompareChunkName(ChunkName, CPngMagic) then
          raise EPngError.Create(RCStrNotAValidPNGFile);

      MemoryStream := TMemoryStream64.Create;
      try
        // read image header chunk size
        ChunkSize_ := ReadSwappedCardinal(Stream);
        if ChunkSize_ > Stream.Size - 12 then
            raise EPngError.Create(RCStrNotAValidPNGFile);

        // read image header chunk ID
        Read(ChunkName, 4);
        if not CompareChunkName(ChunkName, 'IHDR') then
            raise EPngError.Create(RCStrNotAValidPNGFile);

        // reset position to the chunk start and copy stream to memory
        Seek(-4, TSeekOrigin.soCurrent);
        MemoryStream.CopyFrom(Stream, ChunkSize_ + 4);
        MemoryStream.Seek(4, TSeekOrigin.soBeginning);

        // load image header
        FImageHeader.ReadFromStream(MemoryStream, ChunkSize_);

        // read image header chunk size
        ChunkCRC := 0;
        Read(ChunkCRC, 4);
{$IFDEF CheckCRC}
        if not CheckCRC(MemoryStream, Swap32(ChunkCRC)) then
            raise EPngError.Create(RCStrCRCError);
{$ENDIF CheckCRC}
        while Stream.Position < Stream.Size do
          begin
            // read image header chunk size
            ChunkSize_ := ReadSwappedCardinal(Stream);
            if ChunkSize_ > Stream.Size - Stream.Position - 4 then
                raise EPngError.Create(RCStrNotAValidPNGFile);

            // read chunk ID
            Read(ChunkName, 4);

            // check for stream end
            if CompareChunkName(ChunkName, 'IEND') then
              begin
                // read image header chunk size
                Read(ChunkCRC, 4);

{$IFDEF CheckCRC}
                if ChunkCRC <> 2187346606 then
                    raise EPngError.Create(RCStrCRCError);
{$ENDIF CheckCRC}
                Break;
              end;

            // reset position to the chunk start and copy stream to memory
            Seek(-4, TSeekOrigin.soCurrent);
            MemoryStream.Clear;
            MemoryStream.CopyFrom(Stream, ChunkSize_ + 4);

            // reset memory stream to beginning of the chunk
            MemoryStream.Seek(4, TSeekOrigin.soBeginning);

            if CompareChunkName(ChunkName, 'IHDR') then
                raise EPngError.Create(RCStrNotAValidPNGFile)
            else if CompareChunkName(ChunkName, 'IDAT') then
                ReadImageDataChunk(MemoryStream, ChunkSize_)
            else if CompareChunkName(ChunkName, 'gAMA') then
              begin
                if Assigned(FGammaChunk)
                then
                    raise EPngError.Create(RCStrSeveralGammaChunks);
                FGammaChunk := TChunkPngGamma.Create(FImageHeader);
                FGammaChunk.ReadFromStream(MemoryStream, ChunkSize_);
              end
            else if CompareChunkName(ChunkName, 'cHRM') then
              begin
                if Assigned(FChromaChunk) then
                    raise EPngError.Create(RCStrSeveralChromaChunks);
                FChromaChunk := TChunkPngPrimaryChromaticities.Create(FImageHeader);
                FChromaChunk.ReadFromStream(MemoryStream, ChunkSize_);
              end
            else if CompareChunkName(ChunkName, 'tIME') then
              begin
                if Assigned(FTimeChunk) then
                    raise EPngError.Create(RCStrSeveralTimeChunks);
                FTimeChunk := TChunkPngTime.Create(FImageHeader);
                FTimeChunk.ReadFromStream(MemoryStream, ChunkSize_);
              end
            else if CompareChunkName(ChunkName, 'sBIT') then
              begin
                if Assigned(FSignificantBits) then
                    raise EPngError.Create(RCStrSeveralSignificantBitsChunksFound);
                FSignificantBits := TChunkPngSignificantBits.Create(FImageHeader);
                FSignificantBits.ReadFromStream(MemoryStream, ChunkSize_);
              end
            else if CompareChunkName(ChunkName, 'pHYs') then
              begin
                if Assigned(FPhysicalDimensions) then
                    raise EPngError.Create(RCStrSeveralPhysicalPixelDimensionChunks);
                FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create(FImageHeader);
                FPhysicalDimensions.ReadFromStream(MemoryStream, ChunkSize_);
              end
            else if CompareChunkName(ChunkName, 'PLTE') then
              begin
                if Assigned(FPaletteChunk) then
                    raise EPngError.Create(RCStrSeveralPaletteChunks);
                FPaletteChunk := TChunkPngPalette.Create(FImageHeader);
                FPaletteChunk.ReadFromStream(MemoryStream, ChunkSize_);
              end
            else if CompareChunkName(ChunkName, 'tRNS') then
              begin
                if Assigned(FTransparencyChunk) then
                    raise EPngError.Create(RCStrSeveralTransparencyChunks);
                FTransparencyChunk := TChunkPngTransparency.Create(FImageHeader);
                FTransparencyChunk.ReadFromStream(MemoryStream, ChunkSize_);
              end
            else if CompareChunkName(ChunkName, 'bKGD') then
              begin
                if Assigned(FBackgroundChunk) then
                    raise EPngError.Create(RCStrSeveralBackgroundChunks);
                FBackgroundChunk := TChunkPngBackgroundColor.Create(FImageHeader);
                FBackgroundChunk.ReadFromStream(MemoryStream, ChunkSize_);
              end
            else
              begin
                ChunkClass := FindPngChunkByChunkName(ChunkName);
                if ChunkClass <> nil then
                  begin
                    Chunk := ChunkClass.Create(FImageHeader);
                    Chunk.ReadFromStream(MemoryStream, ChunkSize_);
                    FAdditionalChunkList.Add(Chunk);
                  end
                else
                  begin
                    // check if chunk is ancillary
                    if (Byte(ChunkName[0]) and $80) = 0 then
                        ReadUnknownChunk(MemoryStream, ChunkName, ChunkSize_)
                    else
                        raise EPngError.Create(RCStrAncillaryUnknownChunk);
                  end;
              end;

            // read & check CRC
            Read(ChunkCRC, 4);
{$IFDEF CheckCRC}
            if not CheckCRC(MemoryStream, Swap32(ChunkCRC)) then
                raise EPngError.Create(RCStrCRCError);
{$ENDIF CheckCRC}
          end;
      finally
          DisposeObjectAndNil(MemoryStream);
      end;
    end;
end;

procedure TPortableNetworkGraphic.SaveToStream(Stream: TCoreClassStream);
var
  ChunkName: TChunkName;
  ChunkSize_: Cardinal;
  CRC: Cardinal;
  MemoryStream: TMemoryStream64;
  Index: TGeoInt;

  procedure SaveChunkToStream(Chunk: TCustomChunk);
  begin
    MemoryStream.Clear;

    // store chunk size directly to stream
    ChunkSize_ := Chunk.ChunkSize;
    WriteSwappedCardinal(Stream, ChunkSize_);

    // store chunk name to memory stream
    ChunkName := Chunk.ChunkName;
    MemoryStream.Write(ChunkName, 4);

    // save chunk to memory stream
    Chunk.WriteToStream(MemoryStream);

    // copy memory stream to stream
    MemoryStream.Seek(0, TSeekOrigin.soBeginning);
    Stream.CopyFrom(MemoryStream, MemoryStream.Size);

    // calculate and write CRC
    CRC := Swap32(CalculateCRC(MemoryStream));
    Stream.Write(CRC, SizeOf(Cardinal));
  end;

begin
  with Stream do
    begin
      // write chunk ID
      ChunkName := PNG_SIG;
      Write(ChunkName, 4);

      // write PNG magic
      ChunkName := CPngMagic;
      Write(ChunkName, 4);

      MemoryStream := TMemoryStream64.Create;
      try
        // store chunk size directly to stream
        ChunkSize_ := FImageHeader.ChunkSize;
        WriteSwappedCardinal(Stream, ChunkSize_);

        // store chunk name to memory stream
        ChunkName := FImageHeader.ChunkName;
        MemoryStream.Write(ChunkName, 4);

        // save image header to memory stream
        FImageHeader.WriteToStream(MemoryStream);

        // copy memory stream to stream
        MemoryStream.Seek(0, TSeekOrigin.soBeginning);
        Stream.CopyFrom(MemoryStream, MemoryStream.Size);

        // calculate and write CRC
        CRC := Swap32(CalculateCRC(MemoryStream));
        Write(CRC, SizeOf(Cardinal));

        // eventually save physical pixel dimensions chunk
        if Assigned(FPhysicalDimensions) then
            SaveChunkToStream(FPhysicalDimensions);

        // eventually save significant bits chunk
        if Assigned(FSignificantBits) then
            SaveChunkToStream(FSignificantBits);

        // eventually save gamma chunk
        if Assigned(FGammaChunk) then
            SaveChunkToStream(FGammaChunk);

        // eventually save chroma chunk
        if Assigned(FChromaChunk) then
            SaveChunkToStream(FChromaChunk);

        // eventually save palette chunk
        if Assigned(FPaletteChunk) then
            SaveChunkToStream(FPaletteChunk);

        // eventually save transparency chunk
        if Assigned(FTransparencyChunk) then
            SaveChunkToStream(FTransparencyChunk);

        // eventually save background chunk
        if Assigned(FBackgroundChunk) then
            SaveChunkToStream(FBackgroundChunk);

        // store additional chunks
        for Index := 0 to FAdditionalChunkList.Count - 1 do
            SaveChunkToStream(TCustomChunk(FAdditionalChunkList[Index]));

        // save data streams
        for Index := 0 to FDataChunkList.Count - 1 do
            SaveChunkToStream(TCustomChunk(FDataChunkList[Index]));
      finally
          DisposeObjectAndNil(MemoryStream);
      end;

      // write chunk size
      WriteSwappedCardinal(Stream, 0);

      // write chunk ID
      ChunkName := MakeChunkName('IEND');
      Write(ChunkName, 4);

      // write CRC
      CRC := 2187346606;
      Write(CRC, 4);
    end;
end;

procedure TPortableNetworkGraphic.ReadUnknownChunk(Stream: TCoreClassStream;
  ChunkName: TChunkName; ChunkSize_: TGeoInt);
var
  UnknownChunk: TChunkPngUnknown;
begin
  UnknownChunk := TChunkPngUnknown.Create(ChunkName);
  UnknownChunk.ReadFromStream(Stream, ChunkSize_);
  FAdditionalChunkList.Add(UnknownChunk);
end;

procedure TPortableNetworkGraphic.RemoveGammaInformation;
begin
  if Assigned(FGammaChunk) then
      DisposeObjectAndNil(FGammaChunk);
end;

procedure TPortableNetworkGraphic.RemoveModifiedTimeInformation;
begin
  if Assigned(FTimeChunk) then
      DisposeObjectAndNil(FTimeChunk);
end;

procedure TPortableNetworkGraphic.RemovePhysicalPixelDimensionsInformation;
begin
  if Assigned(FPhysicalDimensions) then
      DisposeObjectAndNil(FPhysicalDimensions);
end;

procedure TPortableNetworkGraphic.CompressionLevelChanged;
var
  TempStream: TMemoryStream64;
begin
  TempStream := TMemoryStream64.Create;
  try
    DecompressImageDataToStream(TempStream);
    TempStream.Seek(0, TSeekOrigin.soBeginning);
    CompressImageDataFromStream(TempStream);
  finally
      DisposeObjectAndNil(TempStream);
  end;
end;

procedure TPortableNetworkGraphic.AdaptiveFilterMethodsChanged;
begin
  if FDataChunkList.Count > 0 then
    begin
      // transcoding!
      raise EPngError.Create(RCStrNotYetImplemented);
    end;
end;

procedure TPortableNetworkGraphic.InterlaceMethodChanged;
var
  TempStream: TMemoryStream64;
  TranscoderClass: TCustomPngTranscoderClass;
begin
  TempStream := TMemoryStream64.Create;
  try
    DecompressImageDataToStream(TempStream);
    TempStream.Seek(0, TSeekOrigin.soBeginning);

    case FImageHeader.InterlaceMethod of
      imNone: TranscoderClass := TPngNonInterlacedToAdam7Transcoder;
      imAdam7: TranscoderClass := TPngAdam7ToNonInterlacedTranscoder;
      else
        raise EPngError.Create(RCStrWrongInterlaceMethod);
    end;

    with TranscoderClass.Create(TempStream, FImageHeader) do
      try
          Transcode;
      finally
          Free;
      end;

    TempStream.Seek(0, TSeekOrigin.soBeginning);
    CompressImageDataFromStream(TempStream);
  finally
      DisposeObjectAndNil(TempStream);
  end;
end;

procedure TPortableNetworkGraphic.ReadImageDataChunk(Stream: TCoreClassStream; Size: TGeoInt);
var
  ImageDataChunk: TChunkPngImageData;
begin
  ImageDataChunk := TChunkPngImageData.Create(FImageHeader);
  ImageDataChunk.ReadFromStream(Stream, Size);
  FDataChunkList.Add(ImageDataChunk);
end;

procedure TPortableNetworkGraphic.Assign(Source: TCoreClassPersistent);
begin
  if Source is TPortableNetworkGraphic then
    with TPortableNetworkGraphic(Source) do
      begin
        if Assigned(Self.FImageHeader) then
            Self.FImageHeader.Assign(FImageHeader);

        // assign palette chunk
        if Assigned(Self.FPaletteChunk) then
          if Assigned(FPaletteChunk) then
              Self.FPaletteChunk.Assign(FPaletteChunk)
          else
              DisposeObjectAndNil(Self.FPaletteChunk)
        else if Assigned(FPaletteChunk) then
          begin
            Self.FPaletteChunk := TChunkPngPalette.Create(FImageHeader);
            Self.FPaletteChunk.Assign(FPaletteChunk);
          end;

        // assign gamma chunk
        if Assigned(Self.FGammaChunk) then
          if Assigned(FGammaChunk) then
              Self.FGammaChunk.Assign(FGammaChunk)
          else
              DisposeObjectAndNil(Self.FGammaChunk)
        else if Assigned(FGammaChunk) then
          begin
            Self.FGammaChunk := TChunkPngGamma.Create(FImageHeader);
            Self.FGammaChunk.Assign(FGammaChunk);
          end;

        // assign time chunk
        if Assigned(Self.FTimeChunk) then
          if Assigned(FTimeChunk) then
              Self.FTimeChunk.Assign(FTimeChunk)
          else
              DisposeObjectAndNil(Self.FTimeChunk)
        else if Assigned(FTimeChunk) then
          begin
            Self.FTimeChunk := TChunkPngTime.Create(FImageHeader);
            Self.FTimeChunk.Assign(FTimeChunk);
          end;

        // assign significant bits
        if Assigned(Self.FSignificantBits) then
          if Assigned(FSignificantBits) then
              Self.FSignificantBits.Assign(FSignificantBits)
          else
              DisposeObjectAndNil(Self.FSignificantBits)
        else if Assigned(FSignificantBits) then
          begin
            Self.FSignificantBits := TChunkPngSignificantBits.Create(FImageHeader);
            Self.FSignificantBits.Assign(FSignificantBits);
          end;

        // assign physical dimensions
        if Assigned(Self.FPhysicalDimensions) then
          if Assigned(FPhysicalDimensions) then
              Self.FPhysicalDimensions.Assign(FPhysicalDimensions)
          else
              DisposeObjectAndNil(Self.FPhysicalDimensions)
        else if Assigned(FPhysicalDimensions) then
          begin
            Self.FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create(FImageHeader);
            Self.FPhysicalDimensions.Assign(FPhysicalDimensions);
          end;

        // assign primary chromaticities
        if Assigned(Self.FChromaChunk) then
          if Assigned(FChromaChunk) then
              Self.FChromaChunk.Assign(FChromaChunk)
          else
              DisposeObjectAndNil(Self.FChromaChunk)
        else if Assigned(FChromaChunk) then
          begin
            Self.FChromaChunk := TChunkPngPrimaryChromaticities.Create(FImageHeader);
            Self.FChromaChunk.Assign(FChromaChunk);
          end;

        // assign transparency
        if Assigned(Self.FTransparencyChunk) then
          if Assigned(FTransparencyChunk) then
              Self.FTransparencyChunk.Assign(FTransparencyChunk)
          else
              DisposeObjectAndNil(Self.FTransparencyChunk)
        else if Assigned(FTransparencyChunk) then
          begin
            Self.FTransparencyChunk := TChunkPngTransparency.Create(FImageHeader);
            Self.FTransparencyChunk.Assign(FTransparencyChunk);
          end;

        // assign background
        if Assigned(Self.FBackgroundChunk) then
          if Assigned(FBackgroundChunk) then
              Self.FBackgroundChunk.Assign(FBackgroundChunk)
          else
              DisposeObjectAndNil(Self.FBackgroundChunk)
        else if Assigned(FBackgroundChunk) then
          begin
            Self.FBackgroundChunk := TChunkPngBackgroundColor.Create(FImageHeader);
            Self.FBackgroundChunk.Assign(FBackgroundChunk);
          end;

        if Assigned(Self.FDataChunkList) then
            Self.FDataChunkList.Assign(FDataChunkList);
        if Assigned(Self.FAdditionalChunkList) then
            Self.FAdditionalChunkList.Assign(FAdditionalChunkList);
      end
  else
      inherited;
end;

procedure TPortableNetworkGraphic.AssignTo(Dest: TCoreClassPersistent);
begin
  if Dest is TPortableNetworkGraphic then
    with TPortableNetworkGraphic(Dest) do
      begin
        FImageHeader.Assign(Self.FImageHeader);
        FPaletteChunk.Assign(Self.FPaletteChunk);
        FGammaChunk.Assign(Self.FGammaChunk);
        FTimeChunk.Assign(Self.FTimeChunk);
        FSignificantBits.Assign(Self.FSignificantBits);
        FPhysicalDimensions.Assign(Self.FPhysicalDimensions);
        FChromaChunk.Assign(Self.FChromaChunk);
        FTransparencyChunk.Assign(Self.FTransparencyChunk);
        FBackgroundChunk.Assign(Self.FBackgroundChunk);
        FDataChunkList.Assign(Self.FDataChunkList);
        FAdditionalChunkList.Assign(Self.FAdditionalChunkList);
      end
  else
      inherited;
end;

{$IFDEF OverflowCheck}{$Q-}{$ENDIF}
{$IFDEF RangeCheck}{$R-}{$ENDIF}


function TPortableNetworkGraphic.CalculateCRC(Stream: TCoreClassStream): Cardinal;
var
  CrcValue: Cardinal;
  Value: Byte;
begin
  if Stream is TMemoryStream64 then
      Result := CalculateCRC(TMemoryStream64(Stream).Memory, Stream.Size)
  else
    with Stream do
      begin
        Seek(0, TSeekOrigin.soBeginning);

        // initialize CRC
        CrcValue := $FFFFFFFF;
{$IFDEF FPC}
        Value := 0;
{$ENDIF}
        while Position < Size do
          begin
            Read(Value, 1);

            CrcValue := GCRCTable^[(CrcValue xor Value) and $FF] xor (CrcValue shr 8);
          end;

        Result := (CrcValue xor $FFFFFFFF);

        Seek(0, TSeekOrigin.soBeginning);
      end;
end;

function TPortableNetworkGraphic.CalculateCRC(Buffer: PByte; Count: Cardinal): Cardinal;
var
  CrcValue: Cardinal;
  Pos: Cardinal;
begin
  // ignore size (offset by 4 bytes)
  Pos := 0;

  // initialize CRC
  CrcValue := $FFFFFFFF;

  while Pos < Count do
    begin
      CrcValue := GCRCTable^[(CrcValue xor Buffer^) and $FF] xor (CrcValue shr 8);
      Inc(Buffer);
      Inc(Pos);
    end;

  Result := (CrcValue xor $FFFFFFFF);
end;

{$IFDEF OverflowCheck}{$Q+}{$ENDIF}
{$IFDEF RangeCheck}{$R+}{$ENDIF}

{$IFDEF CheckCRC}


function TPortableNetworkGraphic.CheckCRC(Stream: TCoreClassStream; CRC: Cardinal): Boolean;
begin
  Result := CalculateCRC(Stream) = CRC;
end;
{$ENDIF CheckCRC}


function TPortableNetworkGraphic.GetBitDepth: Byte;
begin
  Result := FImageHeader.BitDepth;
end;

function TPortableNetworkGraphic.GetColorType: TColorType;
begin
  Result := FImageHeader.ColorType;
end;

function TPortableNetworkGraphic.GetCompressionMethod: Byte;
begin
  Result := FImageHeader.CompressionMethod;
end;

function TPortableNetworkGraphic.GetFilterMethod: TFilterMethod;
begin
  Result := FImageHeader.FilterMethod;
end;

function TPortableNetworkGraphic.GetFilterMethods: TAvailableAdaptiveFilterMethods;
begin
  Result := FImageHeader.FAdaptiveFilterMethods;
end;

function TPortableNetworkGraphic.GetGamma: TGeoFloat;
begin
  if Assigned(FGammaChunk) then
      Result := FGammaChunk.GammaAsSingle
  else
      Result := 1;
end;

function TPortableNetworkGraphic.GetHeight: TGeoInt;
begin
  Result := FImageHeader.Height;
end;

function TPortableNetworkGraphic.GetInterlaceMethod: TInterlaceMethod;
begin
  Result := FImageHeader.InterlaceMethod;
end;

function TPortableNetworkGraphic.GetModifiedTime: TDateTime;
begin
  if Assigned(FTimeChunk) then
    with FTimeChunk do
        Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Second, 0)
  else
      Result := 0;
end;

function TPortableNetworkGraphic.GetPaletteEntry(Index: TGeoInt): TRGB24;
begin
  if Assigned(FPaletteChunk) then
      Result := FPaletteChunk.PaletteEntry[Index]
  else
      raise EPngError.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TPortableNetworkGraphic.GetPaletteEntryCount: TGeoInt;
begin
  if Assigned(FPaletteChunk) then
      Result := FPaletteChunk.Count
  else
      Result := 0;
end;

function TPortableNetworkGraphic.GetPixelsPerUnitX: Cardinal;
begin
  if Assigned(FPhysicalDimensions) then
      Result := FPhysicalDimensions.PixelsPerUnitX
  else
      Result := 1;
end;

function TPortableNetworkGraphic.GetPixelsPerUnitY: Cardinal;
begin
  if Assigned(FPhysicalDimensions) then
      Result := FPhysicalDimensions.PixelsPerUnitY
  else
      Result := 1;
end;

function TPortableNetworkGraphic.GetPixelUnit: Byte;
begin
  if Assigned(FPhysicalDimensions) then
      Result := FPhysicalDimensions.PixelUnit
  else
      Result := 0;
end;

function TPortableNetworkGraphic.GetWidth: TGeoInt;
begin
  Result := FImageHeader.Width;
end;

function TPortableNetworkGraphic.HasGammaInformation: Boolean;
begin
  Result := Assigned(FGammaChunk);
end;

function TPortableNetworkGraphic.HasModifiedTimeInformation: Boolean;
begin
  Result := Assigned(FTimeChunk);
end;

function TPortableNetworkGraphic.HasPhysicalPixelDimensionsInformation: Boolean;
begin
  Result := Assigned(FPhysicalDimensions);
end;

procedure TPortableNetworkGraphic.Clear;
begin
  // clear chunk lists
  FDataChunkList.Clear;
  FAdditionalChunkList.Clear;

  // reset image header to default
  FImageHeader.ResetToDefault;

  // free palette chunk
  if Assigned(FPaletteChunk) then
      DisposeObjectAndNil(FPaletteChunk);

  // free gamma chunk
  if Assigned(FGammaChunk) then
      DisposeObjectAndNil(FGammaChunk);

  // free gamma chunk
  if Assigned(FChromaChunk) then
      DisposeObjectAndNil(FChromaChunk);

  // free transparency chunk
  if Assigned(FTransparencyChunk) then
      DisposeObjectAndNil(FTransparencyChunk);

  // free background chunk
  if Assigned(FBackgroundChunk) then
      DisposeObjectAndNil(FBackgroundChunk);

  // free time chunk
  if Assigned(FTimeChunk) then
      DisposeObjectAndNil(FTimeChunk);

  // free time chunk
  if Assigned(FSignificantBits) then
      DisposeObjectAndNil(FSignificantBits);

  // free physical pixel dimensions chunk
  if Assigned(FPhysicalDimensions) then
      DisposeObjectAndNil(FPhysicalDimensions);
end;

procedure TPngNonInterlacedToAdam7Transcoder.Transcode;
var
  CurrentRow: TGeoInt;
  RowByteSize: TGeoInt;
  PixelPerRow: TGeoInt;
  PixelByteSize: TGeoInt;
  CurrentPass: TGeoInt;
  Index: TGeoInt;
  PassRow: TGeoInt;
  Source: PByte;
  Destination: PByte;
  TempData: PPNGByteArray;
  OutputRow: PPNGByteArray;
  TempBuffer: PPNGByteArray;
begin
  // initialize variables
  CurrentRow := 0;
  RowByteSize := 0;
  PixelByteSize := FHeader.PixelByteSize;

  TempData := System.GetMemory(FHeader.Height * FHeader.BytesPerRow);
  Destination := PByte(TempData);
  try

    /// ////////////////////////////////
    // decode image (non-interlaced) //
    /// ////////////////////////////////

    // clear previous row
    FillPtr(@FRowBuffer[1 - CurrentRow]^[0], FHeader.BytesPerRow + 1, 0);

    for Index := 0 to FHeader.Height - 1 do
      begin
        // read data from stream
        if FStream.Read(FRowBuffer[CurrentRow]^[0], FHeader.BytesPerRow + 1) <> FHeader.BytesPerRow + 1 then
            raise EPngError.Create(RCStrDataIncomplete);

        // filter current row
        DecodeFilterRow(TAdaptiveFilterMethod(FRowBuffer[CurrentRow]^[0]),
          FRowBuffer[CurrentRow], FRowBuffer[1 - CurrentRow], FHeader.BytesPerRow,
          PixelByteSize);

        // transfer data from row to temp data
        CopyPtr(@FRowBuffer[CurrentRow]^[1], Destination, PixelByteSize * FHeader.Width);
        Inc(Destination, FHeader.Width * PixelByteSize);

        // flip current row
        CurrentRow := 1 - CurrentRow;
      end;

    // reset position to zero
    FStream.Seek(0, TSeekOrigin.soBeginning);

    // The Adam7 interlacer uses 7 passes to create the complete image
    for CurrentPass := 0 to 6 do
      begin
        // calculate some intermediate variables
        PixelPerRow := (FHeader.Width - CColumnStart[CurrentPass] +
          CColumnIncrement[CurrentPass] - 1) div CColumnIncrement[CurrentPass];

        with FHeader do
          case ColorType of
            ctGrayscale: RowByteSize := (PixelPerRow * BitDepth + 7) div 8;
            ctIndexedColor: RowByteSize := (PixelPerRow * BitDepth + 7) div 8;
            ctTrueColor: RowByteSize := (PixelPerRow * BitDepth * 3) div 8;
            ctGrayscaleAlpha: RowByteSize := (PixelPerRow * BitDepth * 2) div 8;
            ctTrueColorAlpha: RowByteSize := (PixelPerRow * BitDepth * 4) div 8;
            else Continue;
          end;

        PassRow := CRowStart[CurrentPass];

        // clear previous row
        FillPtr(@FRowBuffer[1 - CurrentRow]^[0], RowByteSize + 1, 0);

        // check if pre filter is used and eventually calculate pre filter
        if (FHeader.ColorType <> ctIndexedColor) and
          not(FHeader.AdaptiveFilterMethods = []) then
          begin
            OutputRow := System.GetMemory(RowByteSize + 1);
            TempBuffer := System.GetMemory(RowByteSize + 1);
            try
              while PassRow < FHeader.Height do
                begin
                  Index := CColumnStart[CurrentPass];
                  Source := @TempData^[PassRow * FHeader.BytesPerRow + Index * PixelByteSize];
                  Destination := @FRowBuffer[CurrentRow]^[1];

                  repeat
                    // copy bytes per pixels
                    CopyPtr(Source, Destination, PixelByteSize);

                    Inc(Source, CColumnIncrement[CurrentPass] * PixelByteSize);
                    Inc(Destination, PixelByteSize);
                    Inc(Index, CColumnIncrement[CurrentPass]);
                  until Index >= FHeader.Width;

                  // filter current row
                  EncodeFilterRow(FRowBuffer[CurrentRow], FRowBuffer[1 - CurrentRow],
                    OutputRow, TempBuffer, RowByteSize, FHeader.PixelByteSize);
                  Assert(OutputRow^[0] in [0 .. 4]);

                  // write data to data stream
                  FStream.Write(OutputRow^[0], RowByteSize + 1);

                  // prepare for the next pass
                  Inc(PassRow, CRowIncrement[CurrentPass]);
                  CurrentRow := 1 - CurrentRow;
                end;
            finally
              Dispose(OutputRow);
              Dispose(TempBuffer);
            end;
          end
        else
          while PassRow < FHeader.Height do
            begin
              Index := CColumnStart[CurrentPass];
              Source := @TempData^[PassRow * FHeader.BytesPerRow + Index * PixelByteSize];
              Destination := @FRowBuffer[CurrentRow]^[1];

              repeat
                // copy bytes per pixels
                CopyPtr(Source, Destination, PixelByteSize);

                Inc(Source, CColumnIncrement[CurrentPass] * PixelByteSize);
                Inc(Destination, PixelByteSize);
                Inc(Index, CColumnIncrement[CurrentPass]);
              until Index >= FHeader.Width;

              // set filter method 0
              FRowBuffer[CurrentRow]^[0] := 0;

              // write data to data stream
              FStream.Write(FRowBuffer[CurrentRow]^[0], RowByteSize + 1);

              // prepare for the next pass
              Inc(PassRow, CRowIncrement[CurrentPass]);
              CurrentRow := 1 - CurrentRow;
            end;
      end;
  finally
      Dispose(TempData);
  end;
end;

procedure TPngAdam7ToNonInterlacedTranscoder.Transcode;
var
  CurrentRow: TGeoInt;
  RowByteSize: TGeoInt;
  PixelPerRow: TGeoInt;
  PixelByteSize: TGeoInt;
  CurrentPass: TGeoInt;
  Index: TGeoInt;
  PassRow: TGeoInt;
  Source: PByte;
  Destination: PByte;
  TempData: PPNGByteArray;
  OutputRow: PPNGByteArray;
  TempBuffer: PPNGByteArray;
begin
  // initialize variables
  CurrentRow := 0;
  RowByteSize := 0;
  PixelByteSize := FHeader.PixelByteSize;

  TempData := System.GetMemory(FHeader.Height * FHeader.BytesPerRow);
  try

    /// //////////////////////////////////
    // decode image (Adam7-interlaced) //
    /// //////////////////////////////////

    // The Adam7 deinterlacer uses 7 passes to create the complete image
    for CurrentPass := 0 to 6 do
      begin
        // calculate some intermediate variables
        PixelPerRow := (FHeader.Width - CColumnStart[CurrentPass] +
          CColumnIncrement[CurrentPass] - 1) div CColumnIncrement[CurrentPass];

        with FHeader do
          case ColorType of
            ctGrayscale: RowByteSize := (PixelPerRow * BitDepth + 7) div 8;
            ctIndexedColor: RowByteSize := (PixelPerRow * BitDepth + 7) div 8;
            ctTrueColor: RowByteSize := (PixelPerRow * BitDepth * 3) div 8;
            ctGrayscaleAlpha: RowByteSize := (PixelPerRow * BitDepth * 2) div 8;
            ctTrueColorAlpha: RowByteSize := (PixelPerRow * BitDepth * 4) div 8;
            else Continue;
          end;

        PassRow := CRowStart[CurrentPass];

        // clear previous row
        FillPtr(@FRowBuffer[1 - CurrentRow]^[0], RowByteSize, 0);

        // process pixels
        while PassRow < FHeader.Height do
          begin
            // get interlaced row data
            if FStream.Read(FRowBuffer[CurrentRow]^[0], RowByteSize + 1) <> (RowByteSize + 1) then
                raise EPngError.Create(RCStrDataIncomplete);

            DecodeFilterRow(TAdaptiveFilterMethod(FRowBuffer[CurrentRow]^[0]),
              FRowBuffer[CurrentRow], FRowBuffer[1 - CurrentRow], RowByteSize, PixelByteSize);

            Index := CColumnStart[CurrentPass];
            Source := @FRowBuffer[CurrentRow]^[1];
            Destination := @TempData^[PassRow * FHeader.BytesPerRow + Index * PixelByteSize];
            repeat
              // copy bytes per pixels
              CopyPtr(Source, Destination, PixelByteSize);

              Inc(Source, PixelByteSize);
              Inc(Destination, CColumnIncrement[CurrentPass] * PixelByteSize);
              Inc(Index, CColumnIncrement[CurrentPass]);
            until Index >= FHeader.Width;

            // prepare for the next pass
            Inc(PassRow, CRowIncrement[CurrentPass]);
            CurrentRow := 1 - CurrentRow;
          end;
      end;

    // reset position to zero
    FStream.Seek(0, TSeekOrigin.soBeginning);

    /// //////////////////////////////
    // encode image non-interlaced //
    /// //////////////////////////////

    // clear previous row buffer
    FillPtr(@FRowBuffer[1 - CurrentRow]^[0], FHeader.BytesPerRow, 0);
    Source := PByte(TempData);

    // check if pre filter is used and eventually calculate pre filter
    if (FHeader.ColorType <> ctIndexedColor) and
      not(FHeader.AdaptiveFilterMethods = []) then
      begin
        OutputRow := System.GetMemory(FHeader.BytesPerRow + 1);
        TempBuffer := System.GetMemory(FHeader.BytesPerRow + 1);
        try
          for Index := 0 to FHeader.Height - 1 do
            begin
              // copy bytes per pixels
              CopyPtr(Source, @FRowBuffer[CurrentRow]^[1], FHeader.Width * PixelByteSize);
              Inc(Source, FHeader.Width * PixelByteSize);

              // filter current row
              EncodeFilterRow(FRowBuffer[CurrentRow], FRowBuffer[1 - CurrentRow],
                OutputRow, TempBuffer, FHeader.BytesPerRow, FHeader.PixelByteSize);

              // write data to data stream
              FStream.Write(OutputRow^[0], FHeader.BytesPerRow + 1);

              // flip current row used
              CurrentRow := 1 - CurrentRow;
            end;
        finally
          Dispose(OutputRow);
          Dispose(TempBuffer);
        end;
      end
    else
      for Index := 0 to FHeader.Height - 1 do
        begin
          // copy bytes per pixels
          CopyPtr(Source, @FRowBuffer[CurrentRow]^[1], FHeader.Width * PixelByteSize);
          Inc(Source, FHeader.Width * PixelByteSize);

          // set filter method to none
          FRowBuffer[CurrentRow]^[0] := 0;

          // write data to data stream
          FStream.Write(FRowBuffer[CurrentRow]^[0], FHeader.BytesPerRow + 1);

          // flip current row used
          CurrentRow := 1 - CurrentRow;
        end;

  finally
      Dispose(TempData);
  end;
end;

type
  TPortableNetworkMemoryRaster = class(TPortableNetworkGraphic)
  private
    procedure AssignPropertiesFromRaster(raster: TMemoryRaster);
    function GetBackgroundColor: TRColor;
  protected
    function RasterScanline(raster: TObject; Y: TGeoInt): Pointer; virtual;
  public
    constructor Create; override;
    procedure AssignTo(Dest: TCoreClassObject);
    procedure Assign(Source: TCoreClassObject);

    function IsPremultiplied: Boolean;
    procedure DrawToRaster(raster: TMemoryRaster); virtual;

    property Background: TRColor read GetBackgroundColor;
  end;

  TCustomPngNonInterlacedDecoder = class(TCustomPngDecoder)
  protected
    FBytesPerRow: TGeoInt;
    FRowByteSize: TGeoInt;
    procedure TransferData(Source: Pointer; Destination: PRColor); virtual; abstract;
  public
    constructor Create(Stream: TCoreClassStream; Header: TChunkPngImageHeader;
      Gamma: TChunkPngGamma = nil; Palette: TChunkPngPalette = nil;
      Transparency: TCustomPngTransparency = nil); override;
    destructor Destroy; override;

    procedure DecodeToScanline(raster: TObject; ScanLineCallback: TScanLineCallback); override;
  end;

  TPngNonInterlacedGrayscale1bitDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PRColor); override;
  end;

  TPngNonInterlacedGrayscale2bitDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PRColor); override;
  end;

  TPngNonInterlacedGrayscale4bitDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PRColor); override;
  end;

  TPngNonInterlacedGrayscale8bitDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PRColor); override;
  end;

  TPngNonInterlacedGrayscale16bitDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PRColor); override;
  end;

  TPngNonInterlacedTrueColor8bitDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PRColor); override;
  end;

  TPngNonInterlacedTrueColor16bitDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PRColor); override;
  end;

  TPngNonInterlacedPaletteDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PRColor); override;
  end;

  TPngNonInterlacedPalette8bitDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PRColor); override;
  end;

  TPngNonInterlacedGrayscaleAlpha8bitDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PRColor); override;
  end;

  TPngNonInterlacedGrayscaleAlpha16bitDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PRColor); override;
  end;

  TPngNonInterlacedTrueColorAlpha8bitDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PRColor); override;
  end;

  TPngNonInterlacedTrueColorAlpha16bitDecoder = class(TCustomPngNonInterlacedDecoder)
  protected
    procedure TransferData(Source: Pointer; Destination: PRColor); override;
  end;

  TCustomPngAdam7Decoder = class(TCustomPngDecoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PRColor); virtual; abstract;
  public
    constructor Create(Stream: TCoreClassStream; Header: TChunkPngImageHeader;
      Gamma: TChunkPngGamma = nil; Palette: TChunkPngPalette = nil;
      Transparency: TCustomPngTransparency = nil); override;
    destructor Destroy; override;
    procedure DecodeToScanline(raster: TObject; ScanLineCallback: TScanLineCallback); override;
  end;

  TPngAdam7Grayscale1bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PRColor); override;
  end;

  TPngAdam7Grayscale2bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PRColor); override;
  end;

  TPngAdam7Grayscale4bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PRColor); override;
  end;

  TPngAdam7Grayscale8bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PRColor); override;
  end;

  TPngAdam7Grayscale16bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PRColor); override;
  end;

  TPngAdam7TrueColor8bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PRColor); override;
  end;

  TPngAdam7TrueColor16bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PRColor); override;
  end;

  TPngAdam7Palette1bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PRColor); override;
  end;

  TPngAdam7Palette2bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PRColor); override;
  end;

  TPngAdam7Palette4bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PRColor); override;
  end;

  TPngAdam7Palette8bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PRColor); override;
  end;

  TPngAdam7GrayscaleAlpha8bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PRColor); override;
  end;

  TPngAdam7GrayscaleAlpha16bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PRColor); override;
  end;

  TPngAdam7TrueColorAlpha8bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PRColor); override;
  end;

  TPngAdam7TrueColorAlpha16bitDecoder = class(TCustomPngAdam7Decoder)
  protected
    procedure TransferData(const Pass: Byte; Source: Pointer; Destination: PRColor); override;
  end;

  TCustomPngNonInterlacedEncoder = class(TCustomPngEncoder)
  protected
    FBytesPerRow: TGeoInt;
    FRowByteSize: TGeoInt;
    function ColorInPalette(Color: TRColor): TGeoInt; virtual;
    procedure TransferData(Source: PRColor; Destination: Pointer); virtual; abstract;
  public
    constructor Create(Stream: TCoreClassStream; Header: TChunkPngImageHeader;
      Gamma: TChunkPngGamma = nil; Palette: TChunkPngPalette = nil;
      Transparency: TCustomPngTransparency = nil); override;
    destructor Destroy; override;
    procedure EncodeFromScanline(raster: TObject; ScanLineCallback: TScanLineCallback); override;
  end;

  TPngNonInterlacedGrayscale1bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: PRColor; Destination: Pointer); override;
  end;

  TPngNonInterlacedGrayscale2bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: PRColor; Destination: Pointer); override;
  end;

  TPngNonInterlacedGrayscale4bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: PRColor; Destination: Pointer); override;
  end;

  TPngNonInterlacedGrayscale8bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: PRColor; Destination: Pointer); override;
  end;

  TPngNonInterlacedTrueColor8bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: PRColor; Destination: Pointer); override;
  end;

  TPngNonInterlacedPalette1bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: PRColor; Destination: Pointer); override;
  end;

  TPngNonInterlacedPalette2bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: PRColor; Destination: Pointer); override;
  end;

  TPngNonInterlacedPalette4bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: PRColor; Destination: Pointer); override;
  end;

  TPngNonInterlacedPalette8bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: PRColor; Destination: Pointer); override;
  end;

  TPngNonInterlacedGrayscaleAlpha8bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: PRColor; Destination: Pointer); override;
  end;

  TPngNonInterlacedTrueColorAlpha8bitEncoder = class(TCustomPngNonInterlacedEncoder)
  protected
    procedure TransferData(Source: PRColor; Destination: Pointer); override;
  end;

  TPalette24 = array of TRGB24;

  TPngHistogramEntry = class
  private
    FColor: TRColor;
    FCount: TGeoInt;
  public
    constructor Create(Key: TRColor);
    procedure Advance;

    property Count: TGeoInt read FCount write FCount;
    property Color: TRColor read FColor;
  end;

  TPngPalette = class
  private
    FItems: array of TRColor;
    FCount: TGeoInt;
    procedure Remove(Index: TGeoInt);
  protected
    function GetItem(Index: TGeoInt): TRColor;
    function Find(const Item: TRColor; var Index: TGeoInt): Boolean;
    function Compare(const item1, item2: TRColor): TGeoInt;
    procedure InsertItem(Index: TGeoInt; const anItem: TRColor);
  public
    function Add(const Item: TRColor): TGeoInt; overload;
    function IndexOf(const Value: TRColor): TGeoInt;
    procedure GetNearest(var Value: TRColor);

    procedure Clear;
    procedure LimitTo(Count: TGeoInt);

    property Items[index: TGeoInt]: TRColor read GetItem; default;
    property Count: TGeoInt read FCount;
  end;

function ColorIndexInPalette(Color: TRColor; Palette: TPalette24): TGeoInt;
begin
  for Result := 0 to Length(Palette) - 1 do
    if (TRColorEntry(Color).R = Palette[Result].R) and
      (TRColorEntry(Color).G = Palette[Result].G) and
      (TRColorEntry(Color).B = Palette[Result].B) then
        Exit;
  Result := -1;
end;

function IsPNG(const Stream: TCoreClassStream): Boolean;
begin
  with TPortableNetworkMemoryRaster.Create do
    begin
      try
          Result := CanLoad(Stream);
      except
          Result := False;
      end;
      Free;
    end;
end;

procedure LoadRasterFromPNG(raster: TMemoryRaster; Stream: TCoreClassStream);
begin
  with TPortableNetworkMemoryRaster.Create do
    begin
      try
        LoadFromStream(Stream);
        AssignTo(raster);
      except
          raster.Reset;
      end;
      Free;
    end;
end;

procedure SaveRasterToPNG(raster: TMemoryRaster; Stream: TCoreClassStream);
begin
  with TPortableNetworkMemoryRaster.Create do
    begin
      try
        Assign(raster);
        SaveToStream(Stream);
      except
      end;
      Free;
    end;
end;

procedure TPortableNetworkMemoryRaster.AssignPropertiesFromRaster(raster: TMemoryRaster);
var
  Index, PalIndex: TGeoInt;
  IsAlpha: Boolean;
  IsGrayScale: Boolean;
  IsPalette: Boolean;
  Color: TRColor;
  TempPalette: TPalette24;
  TempAlpha: Byte;
begin
  with raster do
    begin
      ReadyBits();

      // basic properties
      ImageHeader.Width := Width;
      ImageHeader.Height := Height;
      ImageHeader.CompressionMethod := 0;
      ImageHeader.InterlaceMethod := imNone;

      // initialize
      Setlength(TempPalette, 0);
      IsGrayScale := True;
      IsPalette := True;
      IsAlpha := False;
      TempAlpha := 0;

      // check every pixel in the raster for the use of the alpha channel,
      // whether the image is grayscale or whether the colors can be stored
      // as a palette (and build the palette at the same time)
      for Index := 0 to Width * Height - 1 do
        begin
          Color := DirectBits^[Index];

          // check whether the palette is empty
          if Length(TempPalette) = 0 then
            begin
              IsAlpha := TRColorEntry(Color).A < $FF;

              // eventually store first alpha component
              if IsAlpha then
                  TempAlpha := TRColorEntry(Color).A;

              Setlength(TempPalette, 1);
              TempPalette[0].R := TRColorEntry(Color).R;
              TempPalette[0].G := TRColorEntry(Color).G;
              TempPalette[0].B := TRColorEntry(Color).B;
              IsGrayScale := (TRColorEntry(Color).R = TRColorEntry(Color).G) and (TRColorEntry(Color).B = TRColorEntry(Color).G);
            end
          else
            begin
              // check alpha channel
              if (TRColorEntry(Color).A < $FF) then
                begin
                  if IsAlpha then
                    if IsPalette and (TempAlpha <> TRColorEntry(Color).A) then
                        IsPalette := False
                    else
                  else
                      TempAlpha := TRColorEntry(Color).A;

                  IsAlpha := True;
                end;
              if ColorIndexInPalette(Color, TempPalette) < 0 then
                begin
                  if IsPalette then
                    begin
                      if (Length(TempPalette) < 256) then
                        begin
                          PalIndex := Length(TempPalette);
                          Setlength(TempPalette, Length(TempPalette) + 1);
                          TempPalette[PalIndex].R := TRColorEntry(Color).R;
                          TempPalette[PalIndex].G := TRColorEntry(Color).G;
                          TempPalette[PalIndex].B := TRColorEntry(Color).B;
                          if IsGrayScale and (not((TRColorEntry(Color).R = TRColorEntry(Color).G) and (TRColorEntry(Color).B = TRColorEntry(Color).G))) then
                              IsGrayScale := False;
                        end
                      else
                          IsPalette := False;
                    end
                  else if not((TRColorEntry(Color).R = TRColorEntry(Color).G) and (TRColorEntry(Color).B = TRColorEntry(Color).G)) then
                      IsGrayScale := False;
                end;
            end;

          if IsAlpha and (not IsPalette) and (not IsGrayScale) then
              Break;
        end;

      // temporary fix for the case that a palette and an alpha channel has been detected
      if IsPalette and IsAlpha then
          IsPalette := False;

      // set image header
      with ImageHeader do
        if IsGrayScale then
          begin
            if IsAlpha then
              begin
                ColorType := ctGrayscaleAlpha;
                BitDepth := 8;
              end
            else
              begin
                ColorType := ctIndexedColor; // ctGrayscale
                if Length(TempPalette) <= 2 then
                    BitDepth := 1
                else if Length(TempPalette) <= 4 then
                    BitDepth := 2
                else if Length(TempPalette) <= 16 then
                    BitDepth := 4
                else
                    BitDepth := 8;
              end;
          end
        else if IsPalette then
          begin
            ColorType := ctIndexedColor;
            if Length(TempPalette) <= 2 then
                BitDepth := 1
            else if Length(TempPalette) <= 4 then
                BitDepth := 2
            else if Length(TempPalette) <= 16 then
                BitDepth := 4
            else
                BitDepth := 8;
          end
        else if IsAlpha then
          begin
            ColorType := ctTrueColorAlpha;
            BitDepth := 8;
          end
        else
          begin
            ColorType := ctTrueColor;
            BitDepth := 8;
          end;

      // eventually prepare palette
      if ImageHeader.HasPalette then
        begin
          Assert(Length(TempPalette) <= 256);

          if not Assigned(FPaletteChunk) then
              FPaletteChunk := TChunkPngPalette.Create(ImageHeader);

          FPaletteChunk.Count := Length(TempPalette);
          for Index := 0 to Length(TempPalette) - 1 do
              FPaletteChunk.PaletteEntry[Index] := TempPalette[Index];
        end;

      // delete any gama correction table
      if Assigned(FGammaChunk) then
          DisposeObjectAndNil(FGammaChunk);
    end;
end;

function TPortableNetworkMemoryRaster.GetBackgroundColor: TRColor;
var
  ResultColor32: TRColorEntry absolute Result;
begin
  if Assigned(FBackgroundChunk) then
    begin
      if FBackgroundChunk.Background is TPngBackgroundColorFormat04 then
        with TPngBackgroundColorFormat04(FBackgroundChunk.Background) do
          begin
            ResultColor32.R := GraySampleValue;
            ResultColor32.G := GraySampleValue;
            ResultColor32.B := GraySampleValue;
            ResultColor32.A := $FF;
          end
      else if FBackgroundChunk.Background is TPngBackgroundColorFormat26 then
        with TPngBackgroundColorFormat26(FBackgroundChunk.Background) do
          begin
            ResultColor32.R := RedSampleValue;
            ResultColor32.G := GreenSampleValue;
            ResultColor32.B := BlueSampleValue;
            ResultColor32.A := $FF;
          end
      else if FBackgroundChunk.Background is TPngBackgroundColorFormat3 then
        with TPngBackgroundColorFormat3(FBackgroundChunk.Background) do
          begin
            ResultColor32.R := PaletteEntry[PaletteIndex].R;
            ResultColor32.G := PaletteEntry[PaletteIndex].R;
            ResultColor32.B := PaletteEntry[PaletteIndex].R;
            ResultColor32.A := $FF;
          end;
    end
  else
      Result := $0;
end;

function TPortableNetworkMemoryRaster.RasterScanline(raster: TObject; Y: TGeoInt): Pointer;
begin
  if raster is TMemoryRaster then
      Result := TMemoryRaster(raster).ScanLine[Y]
  else
      Result := nil;
end;

constructor TPortableNetworkMemoryRaster.Create;
begin
  inherited Create;
end;

procedure TPortableNetworkMemoryRaster.AssignTo(Dest: TCoreClassObject);
begin
  if Dest is TMemoryRaster then
    begin
      TMemoryRaster(Dest).SetSize(ImageHeader.Width, ImageHeader.Height, 0);
      DrawToRaster(TMemoryRaster(Dest));
    end;
end;

procedure TPortableNetworkMemoryRaster.Assign(Source: TCoreClassObject);
var
  EncoderClass: TCustomPngEncoderClass;
  DataStream: TMemoryStream64;
begin
  if Source is TMemoryRaster then
    with TMemoryRaster(Source) do
      begin
        // Assign
        AssignPropertiesFromRaster(TMemoryRaster(Source));

        case ImageHeader.ColorType of
          ctGrayscale:
            case ImageHeader.BitDepth of
              1: EncoderClass := TPngNonInterlacedGrayscale1bitEncoder;
              2: EncoderClass := TPngNonInterlacedGrayscale2bitEncoder;
              4: EncoderClass := TPngNonInterlacedGrayscale4bitEncoder;
              8: EncoderClass := TPngNonInterlacedGrayscale8bitEncoder;
              else raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctTrueColor: EncoderClass := TPngNonInterlacedTrueColor8bitEncoder;
          ctIndexedColor:
            case ImageHeader.BitDepth of
              1: EncoderClass := TPngNonInterlacedPalette1bitEncoder;
              2: EncoderClass := TPngNonInterlacedPalette2bitEncoder;
              4: EncoderClass := TPngNonInterlacedPalette4bitEncoder;
              8: EncoderClass := TPngNonInterlacedPalette8bitEncoder;
              else raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctGrayscaleAlpha: EncoderClass := TPngNonInterlacedGrayscaleAlpha8bitEncoder;
          ctTrueColorAlpha: EncoderClass := TPngNonInterlacedTrueColorAlpha8bitEncoder;
          else raise EPngError.Create(RCStrUnsupportedFormat);
        end;

        DataStream := TMemoryStream64.Create;
        with DataStream do
          try
            with EncoderClass.Create(DataStream, FImageHeader, FGammaChunk, FPaletteChunk) do
              begin
                try
                    EncodeFromScanline(TMemoryRaster(Source), {$IFDEF FPC}@{$ENDIF FPC}RasterScanline);
                finally
                    Free;
                end;
              end;

            // reset data stream position
            DataStream.Seek(0, TSeekOrigin.soBeginning);

            // compress image data from data stream
            CompressImageDataFromStream(DataStream);
          finally
              DisposeObjectAndNil(DataStream);
          end;
      end;
end;

function TPortableNetworkMemoryRaster.IsPremultiplied: Boolean;
var
  TempRaster: TMemoryRaster;
  Pointer: PRColorArray;
  Value: TRColorEntry;
  Index: TGeoInt;
begin
  // this code checks whether the raster is *NOT* premultiplied
  // unfortunately this is just a weak check and might fail

  Result := True;
  TempRaster := TMemoryRaster.Create;
  try
    AssignTo(TempRaster);
    Pointer := @TempRaster.Bits^[0];
    for Index := 0 to TempRaster.Width * TempRaster.Height - 1 do
      begin
        Value.BGRA := Pointer^[Index];
        if (Value.R > Value.A) or (Value.G > Value.A) or (Value.B > Value.A) then
          begin
            Result := False;
            Exit;
          end;
      end;
  finally
      TempRaster.Free;
  end;
end;

procedure TPortableNetworkMemoryRaster.DrawToRaster(raster: TMemoryRaster);
var
  DecoderClass: TCustomPngDecoderClass;
  DataStream: TMemoryStream64;
  Transparency: TCustomPngTransparency;
begin
  DataStream := TMemoryStream64.Create;
  try
    // decompress image data to data stream
    DecompressImageDataToStream(DataStream);

    // reset data stream position
    DataStream.Seek(0, TSeekOrigin.soBeginning);

    case ImageHeader.InterlaceMethod of
      imNone:
        case ImageHeader.ColorType of
          ctGrayscale:
            case ImageHeader.BitDepth of
              1: DecoderClass := TPngNonInterlacedGrayscale1bitDecoder;
              2: DecoderClass := TPngNonInterlacedGrayscale2bitDecoder;
              4: DecoderClass := TPngNonInterlacedGrayscale4bitDecoder;
              8: DecoderClass := TPngNonInterlacedGrayscale8bitDecoder;
              16: DecoderClass := TPngNonInterlacedGrayscale16bitDecoder;
              else raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctTrueColor:
            case ImageHeader.BitDepth of
              8: DecoderClass := TPngNonInterlacedTrueColor8bitDecoder;
              16: DecoderClass := TPngNonInterlacedTrueColor16bitDecoder;
              else raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctIndexedColor:
            case ImageHeader.BitDepth of
              1, 2, 4: DecoderClass := TPngNonInterlacedPaletteDecoder;
              8: DecoderClass := TPngNonInterlacedPalette8bitDecoder;
              else raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctGrayscaleAlpha:
            case ImageHeader.BitDepth of
              8: DecoderClass := TPngNonInterlacedGrayscaleAlpha8bitDecoder;
              16: DecoderClass := TPngNonInterlacedGrayscaleAlpha16bitDecoder;
              else raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctTrueColorAlpha:
            case ImageHeader.BitDepth of
              8: DecoderClass := TPngNonInterlacedTrueColorAlpha8bitDecoder;
              16: DecoderClass := TPngNonInterlacedTrueColorAlpha16bitDecoder;
              else raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          else raise EPngError.Create(RCStrUnsupportedFormat);
        end;
      imAdam7:
        case ImageHeader.ColorType of
          ctGrayscale:
            case ImageHeader.BitDepth of
              1: DecoderClass := TPngAdam7Grayscale1bitDecoder;
              2: DecoderClass := TPngAdam7Grayscale2bitDecoder;
              4: DecoderClass := TPngAdam7Grayscale4bitDecoder;
              8: DecoderClass := TPngAdam7Grayscale8bitDecoder;
              16: DecoderClass := TPngAdam7Grayscale16bitDecoder;
              else raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctTrueColor:
            case ImageHeader.BitDepth of
              8: DecoderClass := TPngAdam7TrueColor8bitDecoder;
              16: DecoderClass := TPngAdam7TrueColor16bitDecoder;
              else raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctIndexedColor:
            case ImageHeader.BitDepth of
              1: DecoderClass := TPngAdam7Palette1bitDecoder;
              2: DecoderClass := TPngAdam7Palette2bitDecoder;
              4: DecoderClass := TPngAdam7Palette4bitDecoder;
              8: DecoderClass := TPngAdam7Palette8bitDecoder;
              else raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctGrayscaleAlpha:
            case ImageHeader.BitDepth of
              8: DecoderClass := TPngAdam7GrayscaleAlpha8bitDecoder;
              16: DecoderClass := TPngAdam7GrayscaleAlpha16bitDecoder;
              else raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctTrueColorAlpha:
            case ImageHeader.BitDepth of
              8: DecoderClass := TPngAdam7TrueColorAlpha8bitDecoder;
              16: DecoderClass := TPngAdam7TrueColorAlpha16bitDecoder;
              else
                raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          else
            raise EPngError.Create(RCStrUnsupportedFormat);
        end;
      else
        raise EPngError.Create(RCStrUnsupportedFormat);
    end;

    if Assigned(FTransparencyChunk) then
        Transparency := FTransparencyChunk.Transparency
    else
        Transparency := nil;

    with DecoderClass.Create(DataStream, FImageHeader, FGammaChunk, FPaletteChunk, Transparency) do
      begin
        try
            DecodeToScanline(raster, {$IFDEF FPC}@{$ENDIF FPC}RasterScanline);
        finally
            Free;
        end;
      end;
    raster.BlendBlack;
  finally
      DisposeObjectAndNil(DataStream);
  end;
end;

const
  CGrayScaleTable1Bit: array [0 .. 1] of Byte = ($00, $FF);
  CGrayScaleTable2Bit: array [0 .. 3] of Byte = ($00, $55, $AA, $FF);
  CGrayScaleTable4Bit: array [0 .. 15] of Byte = ($00, $11, $22, $33, $44, $55, $66, $77, $88, $99, $AA, $BB, $CC, $DD, $EE, $FF);

constructor TCustomPngNonInterlacedDecoder.Create(Stream: TCoreClassStream;
  Header: TChunkPngImageHeader; Gamma: TChunkPngGamma;
  Palette: TChunkPngPalette; Transparency: TCustomPngTransparency);
begin
  inherited;
  FBytesPerRow := FHeader.BytesPerRow;
  FRowByteSize := FBytesPerRow + 1;
  GetMem(FRowBuffer[0], FRowByteSize);
  GetMem(FRowBuffer[1], FRowByteSize);
end;

destructor TCustomPngNonInterlacedDecoder.Destroy;
begin
  Dispose(FRowBuffer[0]);
  Dispose(FRowBuffer[1]);
  inherited;
end;

procedure TCustomPngNonInterlacedDecoder.DecodeToScanline(raster: TObject; ScanLineCallback: TScanLineCallback);
var
  Index: TGeoInt;
  CurrentRow: TGeoInt;
  PixelByteSize: TGeoInt;
  AdaptiveFilterMethod: TAdaptiveFilterMethod;
  UsedFilters: TAvailableAdaptiveFilterMethods;
begin
  // initialize variables
  CurrentRow := 0;
  UsedFilters := [];
  PixelByteSize := FHeader.PixelByteSize;

  FillPtr(@FRowBuffer[1 - CurrentRow]^[0], FRowByteSize, 0);

  for Index := 0 to FHeader.Height - 1 do
    begin
      // read data from stream
      if FStream.Read(FRowBuffer[CurrentRow]^[0], FRowByteSize) <> FRowByteSize then
          raise EPngError.Create(RCStrDataIncomplete);

      // get active filter method
      AdaptiveFilterMethod := TAdaptiveFilterMethod(FRowBuffer[CurrentRow]^[0]);

      // filter current row
      DecodeFilterRow(AdaptiveFilterMethod, FRowBuffer[CurrentRow],
        FRowBuffer[1 - CurrentRow], FBytesPerRow, PixelByteSize);

      // log used row pre filters
      case AdaptiveFilterMethod of
        afmSub: UsedFilters := UsedFilters + [aafmSub];
        afmUp: UsedFilters := UsedFilters + [aafmUp];
        afmAverage: UsedFilters := UsedFilters + [aafmAverage];
        afmPaeth: UsedFilters := UsedFilters + [aafmPaeth];
      end;

      // transfer data from row to image
      TransferData(@FRowBuffer[CurrentRow]^[1], ScanLineCallback(raster, Index));

      // flip current row
      CurrentRow := 1 - CurrentRow;
    end;
  FHeader.AdaptiveFilterMethods := UsedFilters;
end;

procedure TPngNonInterlacedGrayscale1bitDecoder.TransferData(Source: Pointer; Destination: PRColor);
var
  Index: TGeoInt;
  Src: PByte absolute Source;
  BitIndex: Byte;
begin
  BitIndex := 8;

  for Index := 0 to FHeader.Width - 1 do
    begin
      Dec(BitIndex);
      PRColorEntry(Destination)^.R := FMappingTable^[CGrayScaleTable1Bit[(Src^ shr BitIndex) and $1]];
      PRColorEntry(Destination)^.G := PRColorEntry(Destination)^.R;
      PRColorEntry(Destination)^.B := PRColorEntry(Destination)^.R;
      PRColorEntry(Destination)^.A := $FF;
      if BitIndex = 0 then
        begin
          BitIndex := 8;
          Inc(Src);
        end;
      Inc(Destination);
    end;
end;

procedure TPngNonInterlacedGrayscale2bitDecoder.TransferData(Source: Pointer;
  Destination: PRColor);
var
  Index: TGeoInt;
  Src: PByte absolute Source;
  BitIndex: Byte;
begin
  BitIndex := 8;

  for Index := 0 to FHeader.Width - 1 do
    begin
      Dec(BitIndex, 2);
      PRColorEntry(Destination)^.R := FMappingTable^[CGrayScaleTable2Bit[(Src^ shr BitIndex) and $3]];
      PRColorEntry(Destination)^.G := PRColorEntry(Destination)^.R;
      PRColorEntry(Destination)^.B := PRColorEntry(Destination)^.R;
      PRColorEntry(Destination)^.A := $FF;
      if BitIndex = 0 then
        begin
          BitIndex := 8;
          Inc(Src);
        end;
      Inc(Destination);
    end;
end;

procedure TPngNonInterlacedGrayscale4bitDecoder.TransferData(Source: Pointer;
  Destination: PRColor);
var
  Index: TGeoInt;
  Src: PByte absolute Source;
  BitIndex: Byte;
begin
  BitIndex := 8;

  for Index := 0 to FHeader.Width - 1 do
    begin
      Dec(BitIndex, 4);
      PRColorEntry(Destination)^.R := FMappingTable^[CGrayScaleTable4Bit[(Src^ shr BitIndex) and $F]];
      PRColorEntry(Destination)^.G := PRColorEntry(Destination)^.R;
      PRColorEntry(Destination)^.B := PRColorEntry(Destination)^.R;
      PRColorEntry(Destination)^.A := $FF;
      if BitIndex = 0 then
        begin
          BitIndex := 8;
          Inc(Src);
        end;
      Inc(Destination);
    end;
end;

procedure TPngNonInterlacedGrayscale8bitDecoder.TransferData(Source: Pointer;
  Destination: PRColor);
var
  Index: TGeoInt;
  Src: PByte absolute Source;
begin
  for Index := 0 to FHeader.Width - 1 do
    begin
      PRColorEntry(Destination)^.R := FMappingTable^[Src^];
      PRColorEntry(Destination)^.G := PRColorEntry(Destination)^.R;
      PRColorEntry(Destination)^.B := PRColorEntry(Destination)^.R;
      PRColorEntry(Destination)^.A := $FF;
      Inc(Src);
      Inc(Destination);
    end;
end;

procedure TPngNonInterlacedGrayscale16bitDecoder.TransferData(
  Source: Pointer; Destination: PRColor);
var
  Index: TGeoInt;
  Src: PWord absolute Source;
begin
  for Index := 0 to FHeader.Width - 1 do
    begin
      PRColorEntry(Destination)^.R := FMappingTable^[Src^ and $FF];
      PRColorEntry(Destination)^.G := PRColorEntry(Destination)^.R;
      PRColorEntry(Destination)^.B := PRColorEntry(Destination)^.R;
      PRColorEntry(Destination)^.A := $FF;
      Inc(Src);
      Inc(Destination);
    end;
end;

procedure TPngNonInterlacedTrueColor8bitDecoder.TransferData(Source: Pointer;
  Destination: PRColor);
var
  Index: TGeoInt;
  Src: PRGB24 absolute Source;
begin
  for Index := 0 to FHeader.Width - 1 do
    begin
      PRColorEntry(Destination)^.R := FMappingTable^[Src^.R];
      PRColorEntry(Destination)^.G := FMappingTable^[Src^.G];
      PRColorEntry(Destination)^.B := FMappingTable^[Src^.B];
      PRColorEntry(Destination)^.A := $FF;
      Inc(Src);
      Inc(Destination);
    end;
end;

procedure TPngNonInterlacedTrueColor16bitDecoder.TransferData(
  Source: Pointer; Destination: PRColor);
var
  Index: TGeoInt;
  Src: PRGB24Word absolute Source;
begin
  for Index := 0 to FHeader.Width - 1 do
    begin
      PRColorEntry(Destination)^.R := FMappingTable^[Src^.R and $FF];
      PRColorEntry(Destination)^.G := FMappingTable^[Src^.G and $FF];
      PRColorEntry(Destination)^.B := FMappingTable^[Src^.B and $FF];
      PRColorEntry(Destination)^.A := $FF;
      Inc(Src);
      Inc(Destination);
    end;
end;

procedure TPngNonInterlacedPaletteDecoder.TransferData(Source: Pointer;
  Destination: PRColor);
var
  Index: TGeoInt;
  Src: PByte absolute Source;
  Palette: PRGB24Array;
  Color: TRGB24;
  BitIndex: Byte;
  BitMask: Byte;
  BitDepth: Byte;
begin
  BitIndex := 8;
  BitDepth := FHeader.BitDepth;
  BitMask := (1 shl BitDepth) - 1;
  Palette := PRGB24Array(FMappingTable);

  for Index := 0 to FHeader.Width - 1 do
    begin
      Dec(BitIndex, BitDepth);
      Color := Palette^[(Src^ shr BitIndex) and BitMask];
      PRColorEntry(Destination)^.R := Color.R;
      PRColorEntry(Destination)^.G := Color.G;
      PRColorEntry(Destination)^.B := Color.B;
      PRColorEntry(Destination)^.A := FAlphaTable^[(Src^ shr BitIndex) and BitMask];
      if BitIndex = 0 then
        begin
          BitIndex := 8;
          Inc(Src);
        end;
      Inc(Destination);
    end;
end;

procedure TPngNonInterlacedPalette8bitDecoder.TransferData(Source: Pointer;
  Destination: PRColor);
var
  Index: TGeoInt;
  Src: PByte absolute Source;
  Palette: PRGB24Array;
begin
  Palette := PRGB24Array(FMappingTable);
  for Index := 0 to FHeader.Width - 1 do
    begin
      PRColorEntry(Destination)^.R := Palette^[Src^].R;
      PRColorEntry(Destination)^.G := Palette^[Src^].G;
      PRColorEntry(Destination)^.B := Palette^[Src^].B;
      PRColorEntry(Destination)^.A := FAlphaTable^[Src^];
      Inc(Src);
      Inc(Destination);
    end;
end;

procedure TPngNonInterlacedGrayscaleAlpha8bitDecoder.TransferData(
  Source: Pointer; Destination: PRColor);
var
  Index: TGeoInt;
  Src: PByte absolute Source;
begin
  for Index := 0 to FHeader.Width - 1 do
    begin
      PRColorEntry(Destination)^.R := FMappingTable^[Src^];
      Inc(Src);
      PRColorEntry(Destination)^.G := PRColorEntry(Destination)^.R;
      PRColorEntry(Destination)^.B := PRColorEntry(Destination)^.R;
      PRColorEntry(Destination)^.A := Src^;
      Inc(Src);
      Inc(Destination);
    end;
end;

procedure TPngNonInterlacedGrayscaleAlpha16bitDecoder.TransferData(
  Source: Pointer; Destination: PRColor);
var
  Index: TGeoInt;
  Src: PWord absolute Source;
begin
  for Index := 0 to FHeader.Width - 1 do
    begin
      PRColorEntry(Destination)^.R := FMappingTable^[Src^ and $FF];
      Inc(Src);
      PRColorEntry(Destination)^.G := PRColorEntry(Destination)^.R;
      PRColorEntry(Destination)^.B := PRColorEntry(Destination)^.R;
      PRColorEntry(Destination)^.A := Src^ and $FF;
      Inc(Src);
      Inc(Destination);
    end;
end;

procedure ConvertColorNonInterlacedTrueColorAlpha8bit(Src: PRGB32;
  Dst: PRColorEntry; Count: TGeoInt; MappingTable: PPNGByteArray);
var
  Index: TGeoInt;
begin
  for Index := 0 to Count - 1 do
    begin
      Dst^.R := MappingTable^[Src^.R];
      Dst^.G := MappingTable^[Src^.G];
      Dst^.B := MappingTable^[Src^.B];
      Dst^.A := Src^.A;
      Inc(Src);
      Inc(Dst);
    end;
end;

procedure TPngNonInterlacedTrueColorAlpha8bitDecoder.TransferData(
  Source: Pointer; Destination: PRColor);
begin
  ConvertColorNonInterlacedTrueColorAlpha8bit(PRGB32(Source),
    PRColorEntry(Destination), FHeader.Width, FMappingTable);
end;

procedure TPngNonInterlacedTrueColorAlpha16bitDecoder.TransferData(
  Source: Pointer; Destination: PRColor);
var
  Index: TGeoInt;
  Src: PRGB32Word absolute Source;
begin
  for Index := 0 to FHeader.Width - 1 do
    begin
      PRColorEntry(Destination)^.R := FMappingTable^[Src^.R and $FF];
      PRColorEntry(Destination)^.G := FMappingTable^[Src^.G and $FF];
      PRColorEntry(Destination)^.B := FMappingTable^[Src^.B and $FF];
      PRColorEntry(Destination)^.A := Src^.A and $FF;
      Inc(Src);
      Inc(Destination);
    end;
end;

constructor TCustomPngAdam7Decoder.Create(Stream: TCoreClassStream;
  Header: TChunkPngImageHeader; Gamma: TChunkPngGamma;
  Palette: TChunkPngPalette; Transparency: TCustomPngTransparency);
begin
  inherited;

  // allocate row buffer memory
  GetMem(FRowBuffer[0], FHeader.BytesPerRow + 1);
  GetMem(FRowBuffer[1], FHeader.BytesPerRow + 1);
end;

destructor TCustomPngAdam7Decoder.Destroy;
begin
  Dispose(FRowBuffer[0]);
  Dispose(FRowBuffer[1]);
  inherited;
end;

procedure TCustomPngAdam7Decoder.DecodeToScanline(
  raster: TObject; ScanLineCallback: TScanLineCallback);
var
  CurrentRow: TGeoInt;
  RowByteSize: TGeoInt;
  PixelPerRow: TGeoInt;
  PixelByteSize: TGeoInt;
  CurrentPass: TGeoInt;
  PassRow: TGeoInt;
  UsedFilters: TAvailableAdaptiveFilterMethods;
begin
  // initialize variables
  CurrentRow := 0;
  RowByteSize := 0;
  UsedFilters := [];
  PixelByteSize := FHeader.PixelByteSize;

  // The Adam7 interlacer uses 7 passes to create the complete image
  for CurrentPass := 0 to 6 do
    begin
      // calculate some intermediate variables
      PixelPerRow := (FHeader.Width - CColumnStart[CurrentPass] + CColumnIncrement[CurrentPass] - 1) div CColumnIncrement[CurrentPass];

      with FHeader do
        case ColorType of
          ctGrayscale, ctIndexedColor: RowByteSize := (PixelPerRow * BitDepth + 7) div 8;
          ctTrueColor: RowByteSize := (PixelPerRow * BitDepth * 3) div 8;
          ctGrayscaleAlpha: RowByteSize := (PixelPerRow * BitDepth * 2) div 8;
          ctTrueColorAlpha: RowByteSize := (PixelPerRow * BitDepth * 4) div 8;
          else Continue;
        end;
      if RowByteSize = 0 then
          Continue;

      PassRow := CRowStart[CurrentPass];

      // clear previous row
      FillPtr(@FRowBuffer[1 - CurrentRow]^[0], RowByteSize, 0);

      // process pixel
      while PassRow < FHeader.Height do
        begin
          // get interlaced row data
          if FStream.Read(FRowBuffer[CurrentRow]^[0], RowByteSize + 1) <> (RowByteSize + 1) then
              raise EPngError.Create(RCStrDataIncomplete);

          DecodeFilterRow(TAdaptiveFilterMethod(FRowBuffer[CurrentRow]^[0]), FRowBuffer[CurrentRow], FRowBuffer[1 - CurrentRow], RowByteSize, PixelByteSize);

          // log used row pre filters
          case TAdaptiveFilterMethod(FRowBuffer[CurrentRow]) of
            afmSub: UsedFilters := UsedFilters + [aafmSub];
            afmUp: UsedFilters := UsedFilters + [aafmUp];
            afmAverage: UsedFilters := UsedFilters + [aafmAverage];
            afmPaeth: UsedFilters := UsedFilters + [aafmPaeth];
          end;

          // transfer and deinterlace image data
          TransferData(CurrentPass, @FRowBuffer[CurrentRow]^[1], ScanLineCallback(raster, PassRow));

          // prepare for the next pass
          Inc(PassRow, CRowIncrement[CurrentPass]);
          CurrentRow := 1 - CurrentRow;
        end;
    end;
  FHeader.AdaptiveFilterMethods := UsedFilters;
end;

procedure TPngAdam7Grayscale1bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PRColor);
var
  Index: TGeoInt;
  BitIndex: TGeoInt;
  Src: PByte absolute Source;
begin
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  BitIndex := 8;
  repeat
    Dec(BitIndex);
    PRColorEntry(Destination)^.R := FMappingTable^[CGrayScaleTable1Bit[(Src^ shr BitIndex) and $1]];
    PRColorEntry(Destination)^.G := PRColorEntry(Destination)^.R;
    PRColorEntry(Destination)^.B := PRColorEntry(Destination)^.R;
    PRColorEntry(Destination)^.A := $FF;

    if BitIndex = 0 then
      begin
        BitIndex := 8;
        Inc(Src);
      end;

    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;

procedure TPngAdam7Grayscale2bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PRColor);
var
  Index: TGeoInt;
  BitIndex: TGeoInt;
  Src: PByte absolute Source;
begin
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  BitIndex := 8;
  repeat
    Dec(BitIndex, 2);
    PRColorEntry(Destination)^.R := FMappingTable^[CGrayScaleTable2Bit[((Src^ shr BitIndex) and $3)]];
    PRColorEntry(Destination)^.G := PRColorEntry(Destination)^.R;
    PRColorEntry(Destination)^.B := PRColorEntry(Destination)^.R;
    PRColorEntry(Destination)^.A := $FF;

    if BitIndex = 0 then
      begin
        BitIndex := 8;
        Inc(Src);
      end;

    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;

procedure TPngAdam7Grayscale4bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PRColor);
var
  Index: TGeoInt;
  BitIndex: TGeoInt;
  Src: PByte absolute Source;
begin
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  BitIndex := 8;
  repeat
    Dec(BitIndex, 4);
    PRColorEntry(Destination)^.R := FMappingTable^[CGrayScaleTable4Bit[((Src^ shr BitIndex) and $F)]];
    PRColorEntry(Destination)^.G := PRColorEntry(Destination)^.R;
    PRColorEntry(Destination)^.B := PRColorEntry(Destination)^.R;
    PRColorEntry(Destination)^.A := $FF;

    if BitIndex = 0 then
      begin
        BitIndex := 8;
        Inc(Src);
      end;

    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;

procedure TPngAdam7Grayscale8bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PRColor);
var
  Index: TGeoInt;
  Src: PByte absolute Source;
begin
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  repeat
    PRColorEntry(Destination)^.R := FMappingTable^[Src^];
    Inc(Src);
    PRColorEntry(Destination)^.G := PRColorEntry(Destination)^.R;
    PRColorEntry(Destination)^.B := PRColorEntry(Destination)^.R;
    PRColorEntry(Destination)^.A := $FF;

    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;

procedure TPngAdam7Grayscale16bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PRColor);
var
  Index: TGeoInt;
  Src: PWord absolute Source;
begin
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  repeat
    PRColorEntry(Destination)^.R := FMappingTable^[Src^ and $FF];
    Inc(Src);
    PRColorEntry(Destination)^.G := PRColorEntry(Destination)^.R;
    PRColorEntry(Destination)^.B := PRColorEntry(Destination)^.R;
    PRColorEntry(Destination)^.A := $FF;

    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;

procedure TPngAdam7TrueColor8bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PRColor);
var
  Index: TGeoInt;
  Src: PRGB24 absolute Source;
begin
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  repeat
    PRColorEntry(Destination)^.R := FMappingTable^[Src^.R];
    PRColorEntry(Destination)^.G := FMappingTable^[Src^.G];
    PRColorEntry(Destination)^.B := FMappingTable^[Src^.B];
    PRColorEntry(Destination)^.A := $FF;

    Inc(Src);
    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;

procedure TPngAdam7TrueColor16bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PRColor);
var
  Index: TGeoInt;
  Src: PRGB24Word absolute Source;
begin
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  repeat
    PRColorEntry(Destination)^.R := FMappingTable^[Src^.R and $FF];
    PRColorEntry(Destination)^.G := FMappingTable^[Src^.G and $FF];
    PRColorEntry(Destination)^.B := FMappingTable^[Src^.B and $FF];
    PRColorEntry(Destination)^.A := $FF;

    Inc(Src);
    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;

procedure TPngAdam7Palette1bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PRColor);
var
  Index: TGeoInt;
  BitIndex: TGeoInt;
  Src: PByte absolute Source;
  Palette: PRGB24Array;
  Color: TRGB24;
begin
  BitIndex := 8;
  Palette := PRGB24Array(FMappingTable);
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  repeat
    Dec(BitIndex);
    Color := Palette^[(Src^ shr BitIndex) and $1];
    PRColorEntry(Destination)^.R := Color.R;
    PRColorEntry(Destination)^.G := Color.G;
    PRColorEntry(Destination)^.B := Color.B;
    PRColorEntry(Destination)^.A := FAlphaTable^[(Src^ shr BitIndex) and $1];

    if BitIndex = 0 then
      begin
        BitIndex := 8;
        Inc(Src);
      end;
    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;

procedure TPngAdam7Palette2bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PRColor);
var
  Index: TGeoInt;
  BitIndex: TGeoInt;
  Src: PByte absolute Source;
  Palette: PRGB24Array;
  Color: TRGB24;
begin
  BitIndex := 8;
  Palette := PRGB24Array(FMappingTable);
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  repeat
    Dec(BitIndex, 2);
    Color := Palette^[(Src^ shr BitIndex) and $3];
    PRColorEntry(Destination)^.R := Color.R;
    PRColorEntry(Destination)^.G := Color.G;
    PRColorEntry(Destination)^.B := Color.B;
    PRColorEntry(Destination)^.A := FAlphaTable^[(Src^ shr BitIndex) and $3];

    if BitIndex = 0 then
      begin
        BitIndex := 8;
        Inc(Src);
      end;
    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;

procedure TPngAdam7Palette4bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PRColor);
var
  Index: TGeoInt;
  BitIndex: TGeoInt;
  Src: PByte absolute Source;
  Palette: PRGB24Array;
  Color: TRGB24;
begin
  BitIndex := 8;
  Palette := PRGB24Array(FMappingTable);
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  repeat
    Dec(BitIndex, 4);
    Color := Palette^[(Src^ shr BitIndex) and $F];
    PRColorEntry(Destination)^.R := Color.R;
    PRColorEntry(Destination)^.G := Color.G;
    PRColorEntry(Destination)^.B := Color.B;
    PRColorEntry(Destination)^.A := FAlphaTable^[(Src^ shr BitIndex) and $F];

    if BitIndex = 0 then
      begin
        BitIndex := 8;
        Inc(Src);
      end;
    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;

procedure TPngAdam7Palette8bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PRColor);
var
  Index: TGeoInt;
  Src: PByte absolute Source;
  Palette: PRGB24Array;
begin
  Palette := PRGB24Array(FMappingTable);
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  repeat
    PRColorEntry(Destination)^.R := Palette^[Src^].R;
    PRColorEntry(Destination)^.G := Palette^[Src^].G;
    PRColorEntry(Destination)^.B := Palette^[Src^].B;
    PRColorEntry(Destination)^.A := FAlphaTable^[Src^];

    Inc(Src);
    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;

procedure TPngAdam7GrayscaleAlpha8bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PRColor);
var
  Index: TGeoInt;
  Src: PByte absolute Source;
begin
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  repeat
    PRColorEntry(Destination)^.R := FMappingTable^[Src^];
    Inc(Src);
    PRColorEntry(Destination)^.G := PRColorEntry(Destination)^.R;
    PRColorEntry(Destination)^.B := PRColorEntry(Destination)^.R;
    PRColorEntry(Destination)^.A := Src^;
    Inc(Src);

    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;

procedure TPngAdam7GrayscaleAlpha16bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PRColor);
var
  Index: TGeoInt;
  Src: PWord absolute Source;
begin
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  repeat
    PRColorEntry(Destination)^.R := FMappingTable^[Src^ and $FF];
    Inc(Src);
    PRColorEntry(Destination)^.G := PRColorEntry(Destination)^.R;
    PRColorEntry(Destination)^.B := PRColorEntry(Destination)^.R;
    PRColorEntry(Destination)^.A := Src^ and $FF;
    Inc(Src);

    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;

procedure TPngAdam7TrueColorAlpha8bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PRColor);
var
  Index: TGeoInt;
  SrcPtr: PRGB32 absolute Source;
begin
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  repeat
    PRColorEntry(Destination)^.R := FMappingTable^[SrcPtr^.R];
    PRColorEntry(Destination)^.G := FMappingTable^[SrcPtr^.G];
    PRColorEntry(Destination)^.B := FMappingTable^[SrcPtr^.B];
    PRColorEntry(Destination)^.A := SrcPtr^.A;

    Inc(SrcPtr);
    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;

procedure TPngAdam7TrueColorAlpha16bitDecoder.TransferData(const Pass: Byte;
  Source: Pointer; Destination: PRColor);
var
  Index: TGeoInt;
  SrcPtr: PRGB32Word absolute Source;
begin
  Index := CColumnStart[Pass];
  Inc(Destination, Index);
  repeat
    PRColorEntry(Destination)^.R := FMappingTable^[SrcPtr^.R and $FF];
    PRColorEntry(Destination)^.G := FMappingTable^[SrcPtr^.G and $FF];
    PRColorEntry(Destination)^.B := FMappingTable^[SrcPtr^.B and $FF];
    PRColorEntry(Destination)^.A := SrcPtr^.A and $FF;

    Inc(SrcPtr);
    Inc(Destination, CColumnIncrement[Pass]);
    Inc(Index, CColumnIncrement[Pass]);
  until Index >= FHeader.Width;
end;

constructor TCustomPngNonInterlacedEncoder.Create(Stream: TCoreClassStream;
  Header: TChunkPngImageHeader; Gamma: TChunkPngGamma;
  Palette: TChunkPngPalette; Transparency: TCustomPngTransparency);
begin
  inherited;
  FBytesPerRow := FHeader.BytesPerRow;
  FRowByteSize := FBytesPerRow + 1;
  GetMem(FRowBuffer[0], FRowByteSize);
  GetMem(FRowBuffer[1], FRowByteSize);
end;

destructor TCustomPngNonInterlacedEncoder.Destroy;
begin
  Dispose(FRowBuffer[0]);
  Dispose(FRowBuffer[1]);
  inherited;
end;

function TCustomPngNonInterlacedEncoder.ColorInPalette(Color: TRColor): TGeoInt;
var
  Color24: TRGB24;
begin
  for Result := 0 to FPalette.Count - 1 do
    begin
      Color24 := FPalette.PaletteEntry[Result];
      if (TRColorEntry(Color).R = Color24.R) and
        (TRColorEntry(Color).G = Color24.G) and
        (TRColorEntry(Color).B = Color24.B) then
          Exit;
    end;
  Result := -1;
end;

procedure TCustomPngNonInterlacedEncoder.EncodeFromScanline(raster: TObject; ScanLineCallback: TScanLineCallback);
var
  Index: TGeoInt;
  CurrentRow: TGeoInt;
  OutputRow: PPNGByteArray;
  TempBuffer: PPNGByteArray;
begin
  // initialize variables
  CurrentRow := 0;
  FillPtr(@FRowBuffer[1 - CurrentRow]^[0], FRowByteSize, 0);

  // check if pre filter is used and eventually calculate pre filter
  if FHeader.ColorType <> ctIndexedColor then
    begin
      Assert(FRowByteSize = FBytesPerRow + 1);
      GetMem(OutputRow, FRowByteSize);
      GetMem(TempBuffer, FRowByteSize);
      try
        for Index := 0 to FHeader.Height - 1 do
          begin
            // transfer data from image to current row
            TransferData(ScanLineCallback(raster, Index), @FRowBuffer[CurrentRow]^[1]);

            // filter current row
            EncodeFilterRow(FRowBuffer[CurrentRow], FRowBuffer[1 - CurrentRow],
              OutputRow, TempBuffer, FBytesPerRow, FHeader.PixelByteSize);
            Assert(OutputRow^[0] in [0 .. 4]);

            // write data to data stream
            FStream.Write(OutputRow^[0], FRowByteSize);

            // flip current row used
            CurrentRow := 1 - CurrentRow;
          end;
      finally
        Dispose(OutputRow);
        Dispose(TempBuffer);
      end;
    end
  else
    for Index := 0 to FHeader.Height - 1 do
      begin
        // transfer data from image to current row
        TransferData(ScanLineCallback(raster, Index), @FRowBuffer[CurrentRow]^[1]);

        // set filter method to none
        FRowBuffer[CurrentRow]^[0] := 0;

        // write data to data stream
        FStream.Write(FRowBuffer[CurrentRow]^[0], FRowByteSize);

        // flip current row used
        CurrentRow := 1 - CurrentRow;
      end;
end;

procedure TPngNonInterlacedGrayscale1bitEncoder.TransferData(Source: PRColor; Destination: Pointer);
var
  Index: TGeoInt;
  Dest: PByte absolute Destination;
  BitIndex: Byte;
begin
  BitIndex := 8;

  for Index := 0 to FHeader.Width - 1 do
    begin
      Dec(BitIndex);
      Dest^ := (Dest^ and not($1 shl BitIndex)) or
        (((PRColorEntry(Source)^.R shr 7) and $1) shl BitIndex);

      if BitIndex = 0 then
        begin
          BitIndex := 8;
          Inc(Dest);
        end;
      Inc(Source);
    end;
end;

procedure TPngNonInterlacedGrayscale2bitEncoder.TransferData(Source: PRColor; Destination: Pointer);
var
  Index: TGeoInt;
  Dest: PByte absolute Destination;
  BitIndex: Byte;
begin
  BitIndex := 8;

  for Index := 0 to FHeader.Width - 1 do
    begin
      Dec(BitIndex, 2);
      Dest^ := (Dest^ and not($3 shl BitIndex)) or
        (((PRColorEntry(Source)^.R shr 6) and $3) shl BitIndex);

      if BitIndex = 0 then
        begin
          BitIndex := 8;
          Inc(Dest);
        end;
      Inc(Source);
    end;
end;

procedure TPngNonInterlacedGrayscale4bitEncoder.TransferData(Source: PRColor; Destination: Pointer);
var
  Index: TGeoInt;
  Dest: PByte absolute Destination;
  BitIndex: Byte;
begin
  BitIndex := 8;

  for Index := 0 to FHeader.Width - 1 do
    begin
      Dec(BitIndex, 4);
      Dest^ := (Dest^ and not($F shl BitIndex)) or
        (((PRColorEntry(Source)^.R shr 4) and $F) shl BitIndex);

      if BitIndex = 0 then
        begin
          BitIndex := 8;
          Inc(Dest);
        end;
      Inc(Source);
    end;
end;

procedure TPngNonInterlacedGrayscale8bitEncoder.TransferData(Source: PRColor; Destination: Pointer);
var
  Index: TGeoInt;
  Dest: PByte absolute Destination;
begin
  for Index := 0 to FHeader.Width - 1 do
    begin
      Dest^ := PRColorEntry(Source)^.R;
      Inc(Source);
      Inc(Dest);
    end;
end;

procedure TPngNonInterlacedTrueColor8bitEncoder.TransferData(Source: PRColor; Destination: Pointer);
var
  Index: TGeoInt;
  Dest: PRGB24 absolute Destination;
begin
  for Index := 0 to FHeader.Width - 1 do
    begin
      Dest^.R := PRColorEntry(Source)^.R;
      Dest^.G := PRColorEntry(Source)^.G;
      Dest^.B := PRColorEntry(Source)^.B;
      Inc(Source);
      Inc(Dest);
    end;
end;

procedure TPngNonInterlacedPalette1bitEncoder.TransferData(Source: PRColor; Destination: Pointer);
var
  Index: TGeoInt;
  Dest: PByte absolute Destination;
  BitIndex: Byte;
begin
  BitIndex := 8;

  for Index := 0 to FHeader.Width - 1 do
    begin
      Dec(BitIndex);
      Dest^ := (Dest^ and not($1 shl BitIndex)) or
        ((ColorInPalette(Source^) and $1) shl BitIndex);

      if BitIndex = 0 then
        begin
          BitIndex := 8;
          Inc(Dest);
        end;
      Inc(Source);
    end;
end;

procedure TPngNonInterlacedPalette2bitEncoder.TransferData(Source: PRColor; Destination: Pointer);
var
  Index: TGeoInt;
  Dest: PByte absolute Destination;
  BitIndex: Byte;
begin
  BitIndex := 8;

  for Index := 0 to FHeader.Width - 1 do
    begin
      Dec(BitIndex, 2);
      Dest^ := (Dest^ and not($3 shl BitIndex)) or
        ((ColorInPalette(Source^) and $3) shl BitIndex);

      if BitIndex = 0 then
        begin
          BitIndex := 8;
          Inc(Dest);
        end;
      Inc(Source);
    end;
end;

procedure TPngNonInterlacedPalette4bitEncoder.TransferData(Source: PRColor; Destination: Pointer);
var
  Index: TGeoInt;
  Dest: PByte absolute Destination;
  BitIndex: Byte;
begin
  BitIndex := 8;

  for Index := 0 to FHeader.Width - 1 do
    begin
      Dec(BitIndex, 4);
      Dest^ := (Dest^ and not($F shl BitIndex)) or
        ((ColorInPalette(Source^) and $F) shl BitIndex);

      if BitIndex = 0 then
        begin
          BitIndex := 8;
          Inc(Dest);
        end;
      Inc(Source);
    end;
end;

procedure TPngNonInterlacedPalette8bitEncoder.TransferData(Source: PRColor; Destination: Pointer);
var
  Index: TGeoInt;
  Dest: PByte absolute Destination;
begin
  for Index := 0 to FHeader.Width - 1 do
    begin
      Dest^ := ColorInPalette(Source^);
      Inc(Source);
      Inc(Dest);
    end;
end;

procedure TPngNonInterlacedGrayscaleAlpha8bitEncoder.TransferData(Source: PRColor; Destination: Pointer);
var
  Index: TGeoInt;
  Dest: PByte absolute Destination;
begin
  for Index := 0 to FHeader.Width - 1 do
    begin
      Dest^ := PRColorEntry(Source)^.R;
      Inc(Dest);
      Dest^ := PRColorEntry(Source)^.A;
      Inc(Dest);
      Inc(Source);
    end;
end;

procedure TPngNonInterlacedTrueColorAlpha8bitEncoder.TransferData(Source: PRColor; Destination: Pointer);
var
  Index: TGeoInt;
  Dest: PRGB32 absolute Destination;
begin
  for Index := 0 to FHeader.Width - 1 do
    begin
      Dest^.R := PRColorEntry(Source)^.R;
      Dest^.G := PRColorEntry(Source)^.G;
      Dest^.B := PRColorEntry(Source)^.B;
      Dest^.A := PRColorEntry(Source)^.A;
      Inc(Dest);
      Inc(Source);
    end;
end;

function TPngPalette.Add(const Item: TRColor): TGeoInt;
begin
  Find(Item, Result{%H-});
  InsertItem(Result, Item);
end;

procedure TPngPalette.Clear;
begin
  Setlength(FItems, 0);
  FCount := 0;
end;

function TPngPalette.Compare(const item1, item2: TRColor): TGeoInt;
begin
  Result := item1 - item2;
end;

function TPngPalette.Find(const Item: TRColor; var Index: TGeoInt): Boolean;
var
  lo, hi, mid, compResult: TGeoInt;
begin
  Result := False;
  lo := 0;
  hi := FCount - 1;
  while lo <= hi do
    begin
      mid := (lo + hi) shr 1;
      compResult := Compare(FItems[mid], Item);
      if compResult < 0 then
          lo := mid + 1
      else
        begin
          hi := mid - 1;
          if compResult = 0 then
              Result := True;
        end;
    end;

  index := lo;
end;

function TPngPalette.GetItem(Index: TGeoInt): TRColor;
begin
  Result := FItems[index];
end;

procedure TPngPalette.GetNearest(var Value: TRColor);
var
  Index, MinIndex: TGeoInt;
  Distance, MinDistance: TGeoInt;
begin
  if IndexOf(Value) < 0 then
    begin
      MinDistance :=
        Sqr(TRColorEntry(Value).R - TRColorEntry(FItems[0]).R) +
        Sqr(TRColorEntry(Value).G - TRColorEntry(FItems[0]).G) +
        Sqr(TRColorEntry(Value).B - TRColorEntry(FItems[0]).B);
      MinIndex := 0;
      for Index := 1 to Count - 1 do
        begin
          Distance :=
            Sqr(TRColorEntry(Value).R - TRColorEntry(FItems[Index]).R) +
            Sqr(TRColorEntry(Value).G - TRColorEntry(FItems[Index]).G) +
            Sqr(TRColorEntry(Value).B - TRColorEntry(FItems[Index]).B);
          if Distance < MinDistance then
            begin
              Distance := MinDistance;
              MinIndex := Index;
            end;
        end;

      Value := FItems[MinIndex];
    end;
end;

function TPngPalette.IndexOf(const Value: TRColor): TGeoInt;
begin
  if not Find(Value, Result{%H-}) then
      Result := -1;
end;

procedure TPngPalette.InsertItem(Index: TGeoInt; const anItem: TRColor);
begin
  if Count = Length(FItems) then
      Setlength(FItems, Count + 8 + (Count shr 4));

  if index < Count then
      System.Move(FItems[Index], FItems[Index + 1], (Count - Index) * SizeOf(TRColor));

  Inc(FCount);
  FItems[index] := anItem;
end;

procedure TPngPalette.LimitTo(Count: TGeoInt);
begin
  Setlength(FItems, Count);
  FCount := Count;
end;

procedure TPngPalette.Remove(Index: TGeoInt);
var
  n: TGeoInt;
begin
  Dec(FCount);
  n := FCount - index;
  if n > 0 then
      System.Move(FItems[Index + 1], FItems[Index], n * SizeOf(TRColor));
  Setlength(FItems, FCount);
end;

constructor TPngHistogramEntry.Create(Key: TRColor);
begin
  FColor := Key;
end;

procedure TPngHistogramEntry.Advance;
begin
  Inc(FCount);
end;

procedure BuildCRCTable(Polynomial: Cardinal);
var
  c: Cardinal;
  n, k: TGeoInt;
begin
  // allocate CRC table memory
  GCRCTable := System.GetMemory(256 * SizeOf(Cardinal));

  // fill CRC table
  for n := 0 to $FF do
    begin
      c := n;
      for k := 0 to 7 do
        begin
          if (c and 1) <> 0 then
              c := Polynomial xor (c shr 1)
          else
              c := c shr 1;
        end;
      GCRCTable^[n] := c;
    end;
end;

procedure Testpng;
begin
  with TRaster.Create do
    begin
      LoadFromFile('c:\1.png');
      SaveToPNGFile('c:\2.png');
      Free;
    end;
end;

initialization

BuildCRCTable($EDB88320);
RegisterPngChunks([TChunkPngImageData, TChunkPngPalette, TChunkPngGamma,
  TChunkPngStandardColorSpaceRGB, TChunkPngPrimaryChromaticities,
  TChunkPngTime, TChunkPngTransparency, TChunkPngEmbeddedIccProfile,
  TChunkPngPhysicalPixelDimensions, TChunkPngText, TChunkPngSuggestedPalette,
  TChunkPngCompressedText, TChunkPngInternationalText,
  TChunkPngImageHistogram, TChunkPngBackgroundColor,
  TChunkPngSignificantBits, TChunkPngImageOffset, TChunkPngPixelCalibrator]);

finalization

Dispose(GCRCTable);

end.
