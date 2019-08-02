{ ****************************************************************************** }
{ * memory Rasterization                                                       * }
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
unit MemoryRaster;

{$INCLUDE zDefine.inc}

interface

uses Types, Math, Variants, CoreClasses, MemoryStream64, Geometry2DUnit, Geometry3DUnit,
  PascalStrings, UnicodeMixedLib,
{$IFDEF FPC}
  UPascalStrings,
  FPCGenericStructlist,
{$ELSE FPC}
  System.IOUtils,
{$ENDIF FPC}
  ListEngine,
  AggBasics, Agg2D, AggColor32,
  JLSCodec, Raster_JPEG_type, Raster_JPEG_Image;

type
{$REGION 'base define'}
  TRColor = TAggPackedRgba8;
  PRColor = ^TRColor;

  TRasterColor = TRColor;
  PRasterColor = PRColor;

  TRGBA = TRColor;
  PRGBA = PRColor;

  TBGRA = TRColor;
  PBGRA = PRColor;

  TRColorArray = array [0 .. MaxInt div SizeOf(TRasterColor) - 1] of TRasterColor;
  PRColorArray = ^TRColorArray;

  TRasterColorArray = TRColorArray;
  PRasterColorArray = PRColorArray;

  TRColorEntry = packed record
    case Byte of
      0: (B, G, R, A: Byte);
      1: (BGRA: TRasterColor);
      2: (buff: array [0 .. 3] of Byte)
  end;

  PRColorEntry = ^TRColorEntry;

  TRasterColorEntry = TRColorEntry;
  PRasterColorEntry = PRColorEntry;

  TRGB = array [0 .. 2] of Byte;
  PRGB = ^TRGB;

  TRGBArray = array [0 .. MaxInt div SizeOf(TRGB) - 1] of TRGB;
  PRGBArray = ^TRGBArray;

  TRColorEntryArray = array [0 .. MaxInt div SizeOf(TRColorEntry) - 1] of TRColorEntry;
  PRColorEntryArray = ^TRColorEntryArray;

  TRasterColorEntryArray = TRColorEntryArray;
  PRasterColorEntryArray = PRColorEntryArray;

  TArrayOfRColorEntry = array of TRColorEntry;
  TArrayOfRasterColorEntry = TArrayOfRColorEntry;

  TDrawMode = (dmOpaque, dmBlend, dmTransparent);
  TCombineMode = (cmBlend, cmMerge);

  TByteRaster = array of array of Byte;
  PByteRaster = ^TByteRaster;

  TWordRaster = array of array of Word;
  PWordRaster = ^TWordRaster;

  TMemoryRaster = class;
  TMemoryRaster_AggImage = class;
  TMemoryRaster_Agg2D = class;
  TVertexMap = class;
  TFontRaster = class;
  TRasterSerialized = class;

  // rasterization save format.
  TRasterSave = (
    rsRGBA, rsRGB,
    rsYV12, rsHalfYUV, rsQuartYUV, rsFastYV12, rsFastHalfYUV, rsFastQuartYUV,
    rsJpeg_RGBA_Qualily90, rsJPEG_RGB_Qualily90, rsJPEG_Gray_Qualily90, rsJPEG_GrayA_Qualily90,
    rsJpeg_RGBA_Qualily80, rsJPEG_RGB_Qualily80, rsJPEG_Gray_Qualily80, rsJPEG_GrayA_Qualily80,
    rsJpeg_RGBA_Qualily70, rsJPEG_RGB_Qualily70, rsJPEG_Gray_Qualily70, rsJPEG_GrayA_Qualily70,
    rsJpeg_RGBA_Qualily60, rsJPEG_RGB_Qualily60, rsJPEG_Gray_Qualily60, rsJPEG_GrayA_Qualily60,
    rsJpeg_RGBA_Qualily50, rsJPEG_RGB_Qualily50, rsJPEG_Gray_Qualily50, rsJPEG_GrayA_Qualily50
    );

  TOnGetRasterizationMemory = procedure(Sender: TMemoryRaster) of object;

  TRColors_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TRColor>;
  TRColors = TRColors_Decl;
  TRasterColors = TRColors;

{$ENDREGION 'base define'}
{$REGION 'MemoryRaster'}

  TMemoryRaster = class(TCoreClassObject)
  private
    FSerializedEngine: TRasterSerialized;
    FMemorySerializedPosition: Int64;
    FActivted: Boolean;
    FActiveTimeTick: TTimeTick;

    FFreeBits: Boolean;
    FBits: PRColorArray;
    FWidth, FHeight: Integer;
    FDrawMode: TDrawMode;
    FCombineMode: TCombineMode;

    FVertex: TVertexMap;
    FFont: TFontRaster;

    FAggNeed: Boolean;
    FAggImage: TMemoryRaster_AggImage;
    FAgg: TMemoryRaster_Agg2D;

    FMasterAlpha: Cardinal;
    FOuterColor: TRColor;

    FUserObject: TCoreClassObject;
    FUserData: Pointer;
    FUserText: SystemString;
    FUserToken: SystemString;
    FUserVariant: Variant;

    function GetVertex: TVertexMap;

    function GetFont: TFontRaster;
    procedure SetFont(f: TFontRaster); overload;

    function GetAggImage: TMemoryRaster_AggImage;
    function GetAgg: TMemoryRaster_Agg2D;

    function GetBits: PRColorArray;

    function GetPixel(const X, Y: Integer): TRColor;
    procedure SetPixel(const X, Y: Integer; const Value: TRColor);

    function GetPixelBGRA(const X, Y: Integer): TRColor;
    procedure SetPixelBGRA(const X, Y: Integer; const Value: TRColor);

    function GetPixelPtr(const X, Y: Integer): PRColor;

    function GetScanLine(Y: Integer): PRColorArray;
    function GetWidth0: TGeoFloat;
    function GetHeight0: TGeoFloat;
    function GetWidth0i: Integer;
    function GetHeight0i: Integer;

    function GetPixelRed(const X, Y: Integer): Byte;
    procedure SetPixelRed(const X, Y: Integer; const Value: Byte);

    function GetPixelGreen(const X, Y: Integer): Byte;
    procedure SetPixelGreen(const X, Y: Integer; const Value: Byte);

    function GetPixelBlue(const X, Y: Integer): Byte;
    procedure SetPixelBlue(const X, Y: Integer; const Value: Byte);

    function GetPixelAlpha(const X, Y: Integer): Byte;
    procedure SetPixelAlpha(const X, Y: Integer; const Value: Byte);

    function GetGray(const X, Y: Integer): Byte;
    procedure SetGray(const X, Y: Integer; const Value: Byte);

    function GetGrayS(const X, Y: Integer): TGeoFloat;
    procedure SetGrayS(const X, Y: Integer; const Value: TGeoFloat);

    function GetGrayD(const X, Y: Integer): Double;
    procedure SetGrayD(const X, Y: Integer; const Value: Double);

    function GetPixelF(const X, Y: TGeoFloat): TRColor;
    procedure SetPixelF(const X, Y: TGeoFloat; const Value: TRColor);

    function GetPixelVec(const v2: TVec2): TRColor;
    procedure SetPixelVec(const v2: TVec2; const Value: TRColor);

    function GetPixelLinearF(const X, Y: TGeoFloat): TRColor;
    function GetPixelLinear(const X, Y: Integer): TRColor;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function ActiveTimeTick: TTimeTick;

    { serialized recycle memory }
    property SerializedEngine: TRasterSerialized read FSerializedEngine write FSerializedEngine;
    function SerializedAndRecycleMemory(RSeri: TRasterSerialized): Int64; overload;
    function SerializedAndRecycleMemory(): Int64; overload;
    function UnserializedMemory(RSeri: TRasterSerialized): Int64; overload;
    function UnserializedMemory(): Int64; overload;
    function RecycleMemory(): Int64;
    procedure ReadyBits;

    { memory map }
    procedure SetWorkMemory(WorkMemory: Pointer; NewWidth, NewHeight: Integer); overload;
    procedure SetWorkMemory(raster: TMemoryRaster); overload;
    function IsMemoryMap: Boolean;

    { triangle vertex map }
    procedure OpenVertex;
    procedure CloseVertex;
    property Vertex: TVertexMap read GetVertex;

    { font rasterization support }
    procedure OpenFont;
    procedure CloseFont;
    property Font: TFontRaster read GetFont write SetFont;

    { agg rasterization }
    property AggImage: TMemoryRaster_AggImage read GetAggImage;
    property Agg: TMemoryRaster_Agg2D read GetAgg;
    procedure OpenAgg;
    procedure CloseAgg;
    procedure FreeAgg;
    function AggActivted: Boolean;

    { general }
    procedure Reset; virtual;
    procedure Assign(sour: TMemoryRaster); virtual;
    procedure Clear; overload;
    procedure Clear(FillColor: TRColor); overload; virtual;
    function MemorySize: Integer;
    procedure SetSize(NewWidth, NewHeight: Integer); overload; virtual;
    procedure SetSize(NewWidth, NewHeight: Integer; const ClearColor: TRColor); overload; virtual;
    procedure SetSizeF(NewWidth, NewHeight: TGeoFloat; const ClearColor: TRColor); overload;
    procedure SetSizeR(R: TRectV2; const ClearColor: TRColor); overload;
    function SizeOfPoint: TPoint;
    function SizeOf2DPoint: TVec2;
    function Size2D: TVec2;
    function Size0: TVec2;
    function Empty: Boolean;
    function BoundsRect: TRect;
    function BoundsRectV2: TRectV2;
    function BoundsV2Rect4: TV2Rect4;
    function Centroid: TVec2;
    function Centre: TVec2;
    function InHere(const X, Y: Integer): Boolean;

    { pixel operation }
    procedure FlipHorz;
    procedure FlipVert;
    procedure Rotate90;
    procedure Rotate180;
    procedure Rotate270;
    procedure Rotate(dest: TMemoryRaster; Angle: TGeoFloat; Endge: Integer); overload;
    procedure Rotate(Angle: TGeoFloat; Endge: Integer; BackgroundColor: TRColor); overload;
    procedure CalibrateRotate(BackgroundColor: TRColor); overload;
    procedure CalibrateRotate; overload;
    procedure NoLineZoomLine(const Source, dest: TMemoryRaster; const pass: Integer);
    procedure NoLineZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
    procedure NoLineZoom(const NewWidth, NewHeight: Integer);
    procedure ZoomLine(const Source, dest: TMemoryRaster; const pass: Integer);
    procedure ZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer); overload;
    procedure ZoomFrom(const Source: TMemoryRaster; const f: TGeoFloat); overload;
    procedure Zoom(const NewWidth, NewHeight: Integer);
    procedure FastBlurZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
    procedure FastBlurZoom(const NewWidth, NewHeight: Integer);
    procedure GaussianBlurZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
    procedure GaussianBlurZoom(const NewWidth, NewHeight: Integer);
    procedure GrayscaleBlurZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
    procedure GrayscaleBlurZoom(const NewWidth, NewHeight: Integer);
    procedure Scale(k: TGeoFloat);
    procedure NoLineScale(k: TGeoFloat);
    procedure FitScale(NewWidth, NewHeight: TGeoFloat);
    function FormatAsBGRA: TMemoryRaster;
    procedure FormatBGRA;
    function BuildRGB(cSwapBR: Boolean): PRGBArray;
    procedure InputRGB(var buff; W, H: Integer; cSwapBR: Boolean);
    procedure OutputRGB(var buff; cSwapBR: Boolean);
    procedure ColorTransparent(c: TRColor);
    procedure ColorBlend(c: TRColor);
    procedure Grayscale;
    procedure ExtractGray(var output: TByteRaster);
    procedure ExtractRed(var output: TByteRaster);
    procedure ExtractGreen(var output: TByteRaster);
    procedure ExtractBlue(var output: TByteRaster);
    procedure ExtractAlpha(var output: TByteRaster);
    function ComputeAreaScaleSpace(clipArea: TRectV2; SS_width, SS_height: Integer): TRectV2; overload;
    function ComputeAreaScaleSpace(clipArea: TRect; SS_width, SS_height: Integer): TRect; overload;
    function BuildAreaOffsetScaleSpace(clipArea: TRectV2; SS_width, SS_height: Integer): TMemoryRaster; overload;
    function BuildAreaOffsetScaleSpace(clipArea: TRect; SS_width, SS_height: Integer): TMemoryRaster; overload;
    function BuildAreaCopy(clipArea: TRectV2): TMemoryRaster; overload;
    function BuildAreaCopy(clipArea: TRect): TMemoryRaster; overload;
    function ExistsColor(c: TRColor): Boolean;
    function FindFirstColor(c: TRColor): TPoint;
    function FindLastColor(c: TRColor): TPoint;
    function FindNearColor(c: TRColor; Pt: TVec2): TPoint;
    function ColorBoundsRectV2(c: TRColor): TRectV2;
    function ColorBoundsRect(c: TRColor): TRect;
    function NoneColorBoundsRectV2(c: TRColor): TRectV2;
    function NoneColorBoundsRect(c: TRColor): TRect;

    { shape support }
    procedure Line(x1, y1, x2, y2: Integer; Color: TRColor; L: Boolean); virtual;     // L = draw close pixel
    procedure LineF(x1, y1, x2, y2: TGeoFloat; Color: TRColor; L: Boolean); overload; // L = draw close pixel
    procedure LineF(p1, p2: TVec2; Color: TRColor; L: Boolean); overload;             // L = draw close pixel
    procedure LineF(p1, p2: TVec2; Color: TRColor; L, Cross: Boolean); overload;      // L = draw close pixel
    procedure FillRect(x1, y1, x2, y2: Integer; Color: TRColor); overload;
    procedure FillRect(Dstx, Dsty, LineDist: Integer; Color: TRColor); overload;
    procedure FillRect(Dst: TVec2; LineDist: Integer; Color: TRColor); overload;
    procedure FillRect(R: TRectV2; Color: TRColor); overload;
    procedure FillRect(R: TRectV2; Angle: TGeoFloat; Color: TRColor); overload;
    procedure DrawRect(R: TRect; Color: TRColor); overload;
    procedure DrawRect(R: TRectV2; Color: TRColor); overload;
    procedure DrawRect(R: TV2Rect4; Color: TRColor); overload;
    procedure DrawRect(R: TRectV2; Angle: TGeoFloat; Color: TRColor); overload;
    procedure DrawTriangle(tri: TTriangle; Transform: Boolean; Color: TRColor; Cross: Boolean);
    procedure DrawCross(Dstx, Dsty, LineDist: Integer; Color: TRColor); overload;
    procedure DrawCrossF(Dstx, Dsty, LineDist: TGeoFloat; Color: TRColor); overload;
    procedure DrawCrossF(Dst: TVec2; LineDist: TGeoFloat; Color: TRColor); overload;
    procedure DrawPointListLine(pl: TVec2List; Color: TRColor; wasClose: Boolean);
    procedure DrawCircle(CC: TVec2; R: TGeoFloat; Color: TRColor);
    procedure FillCircle(CC: TVec2; R: TGeoFloat; Color: TRColor);
    procedure DrawEllipse(CC: TVec2; xRadius, yRadius: TGeoFloat; Color: TRColor); overload;
    procedure DrawEllipse(R: TRectV2; Color: TRColor); overload;
    procedure FillEllipse(CC: TVec2; xRadius, yRadius: TGeoFloat; Color: TRColor); overload;
    procedure FillEllipse(R: TRectV2; Color: TRColor); overload;
    procedure FillTriangle(t1, t2, t3: TVec2; Color: TRColor); overload;
    procedure FillTriangle(t1, t2, t3: TPoint; Color: TRColor); overload;
    procedure FillTriangle(t1, t2, t3: TPointf; Color: TRColor); overload;

    { fill, draw a polygon }
    procedure FillPolygon(PolygonBuff: TArrayVec2; Color: TRColor); overload;
    procedure DrawPolygon(PolygonBuff: TArrayVec2; Color: TRColor); overload;
    procedure FillPolygon(Polygon: T2DPolygon; Color: TRColor); overload;
    procedure DrawPolygon(Polygon: T2DPolygon; Color: TRColor); overload;
    procedure FillPolygon(Polygon: T2DPolygonGraph; Color: TRColor); overload;
    procedure DrawPolygon(Polygon: T2DPolygonGraph; Color: TRColor); overload;

    { pixel border }
    function PixelAtBorder(const X, Y: Integer; const BGColor, BorderColor: TRColor; const halfBorderSize: Integer; var detectColor: TRColor): Boolean;
    procedure FillNoneBGColorBorder(BGColor, BorderColor: TRColor; BorderSize: Integer);

    { rasterization text support }
    function TextSize(Text: SystemString; siz: TGeoFloat): TVec2;
    procedure DrawText(Text: SystemString; X, Y: Integer; RotateVec: TVec2; Angle, alpha, siz: TGeoFloat; TextColor: TRColor); overload;
    procedure DrawText(Text: SystemString; X, Y: Integer; siz: TGeoFloat; TextColor: TRColor); overload;

    { Projection: usage hardware simulator }
    procedure ProjectionTo(Dst: TMemoryRaster; const sourRect, DestRect: TV2Rect4; const bilinear_sampling: Boolean; const alpha: TGeoFloat); overload;
    procedure ProjectionTo(Dst: TMemoryRaster; const sourRect, DestRect: TRectV2; const bilinear_sampling: Boolean; const alpha: TGeoFloat); overload;
    procedure Projection(const DestRect: TV2Rect4; const Color: TRColor); overload;
    procedure Projection(sour: TMemoryRaster; const sourRect, DestRect: TV2Rect4; const bilinear_sampling: Boolean; const alpha: TGeoFloat); overload;
    procedure Projection(sour: TMemoryRaster; const sourRect, DestRect: TRectV2; const bilinear_sampling: Boolean; const alpha: TGeoFloat); overload;

    { Projection polygon sampler }
    procedure ProjectionPolygonTo(const sour_Polygon: TVec2List; Dst: TMemoryRaster; DestRect: TRectV2; const bilinear_sampling: Boolean; const alpha: TGeoFloat); overload;
    procedure ProjectionPolygonTo(const sour_Polygon: T2DPolygonGraph; Dst: TMemoryRaster; DestRect: TRectV2; const bilinear_sampling: Boolean; const alpha: TGeoFloat); overload;

    { blend draw }
    procedure Draw(Src: TMemoryRaster); overload;
    procedure Draw(Dstx, Dsty: Integer; Src: TMemoryRaster); overload;
    procedure Draw(Dstx, Dsty: Integer; const SrcRect: TRect; Src: TMemoryRaster); overload;
    procedure DrawTo(Dst: TMemoryRaster); overload;
    procedure DrawTo(Dst: TMemoryRaster; Dstx, Dsty: Integer; const SrcRect: TRect); overload;
    procedure DrawTo(Dst: TMemoryRaster; Dstx, Dsty: Integer); overload;
    procedure DrawTo(Dst: TMemoryRaster; DstPt: TVec2); overload;

    { file format }
    class function CanLoadStream(stream: TCoreClassStream): Boolean; virtual;
    procedure LoadFromBmpStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream); virtual;

    procedure SaveToStream(stream: TCoreClassStream; RasterSave_: TRasterSave); overload; // selected format
    procedure SaveToStream(stream: TCoreClassStream); overload; virtual;                  // published,32bit bitmap
    procedure SaveToBmp32Stream(stream: TCoreClassStream);                                // published,32bit bitmap,include alpha
    procedure SaveToBmp24Stream(stream: TCoreClassStream);                                // published,24bit bitmap,no alpha
    procedure SaveToZLibCompressStream(stream: TCoreClassStream);                         // custom,24bit no alpha
    procedure SaveToDeflateCompressStream(stream: TCoreClassStream);                      // custom,24bit no alpha
    procedure SaveToBRRCCompressStream(stream: TCoreClassStream);                         // custom,24bit no alpha
    procedure SaveToJpegLS1Stream(stream: TCoreClassStream);                              // published,jls8bit
    procedure SaveToJpegLS3Stream(stream: TCoreClassStream);                              // published,jls24bit
    procedure SaveToYV12Stream(stream: TCoreClassStream);                                 // custom,no alpha
    procedure SaveToFastYV12Stream(stream: TCoreClassStream);                             // custom,no alpha
    procedure SaveToHalfYUVStream(stream: TCoreClassStream);                              // custom,no alpha
    procedure SaveToFastHalfYUVStream(stream: TCoreClassStream);                          // custom,no alpha
    procedure SaveToQuartYUVStream(stream: TCoreClassStream);                             // custom,no alpha
    procedure SaveToFastQuartYUVStream(stream: TCoreClassStream);                         // custom,no alpha
    procedure SaveToJpegRGBAStream(stream: TCoreClassStream; Quality: TJpegQuality);      // custom,32bit YCbCrA
    procedure SaveToJpegRGBStream(stream: TCoreClassStream; Quality: TJpegQuality);       // published,24bit YCbCr
    procedure SaveToJpegCMYKRGBStream(stream: TCoreClassStream; Quality: TJpegQuality);   // custom,24bit CMYK
    procedure SaveToJpegGrayStream(stream: TCoreClassStream; Quality: TJpegQuality);      // published,8bit grayscale
    procedure SaveToJpegGrayAStream(stream: TCoreClassStream; Quality: TJpegQuality);     // custom,16bit grayscale+alpha

    class function CanLoadFile(fn: SystemString): Boolean;
    procedure LoadFromFile(fn: SystemString); virtual;

    procedure SaveToBmp32File(fn: SystemString);                              // published,32bit bitmap,include alpha
    procedure SaveToBmp24File(fn: SystemString);                              // published,24bit bitmap,no alpha
    procedure SaveToFile(fn: SystemString);                                   // published,32bit bitmap,include alpha
    procedure SaveToZLibCompressFile(fn: SystemString);                       // custom,24bit no alpha
    procedure SaveToDeflateCompressFile(fn: SystemString);                    // custom,24bit no alpha
    procedure SaveToBRRCCompressFile(fn: SystemString);                       // custom,24bit no alpha
    procedure SaveToJpegLS1File(fn: SystemString);                            // published,jls8bit
    procedure SaveToJpegLS3File(fn: SystemString);                            // published,jls24bit
    procedure SaveToYV12File(fn: SystemString);                               // custom,no alpha
    procedure SaveToFastYV12File(fn: SystemString);                           // custom,no alpha
    procedure SaveToHalfYUVFile(fn: SystemString);                            // custom,no alpha
    procedure SaveToFastHalfYUVFile(fn: SystemString);                        // custom,no alpha
    procedure SaveToQuartYUVFile(fn: SystemString);                           // custom,no alpha
    procedure SaveToFastQuartYUVFile(fn: SystemString);                       // custom,no alpha
    procedure SaveToJpegRGBAFile(fn: SystemString; Quality: TJpegQuality);    // custom,32bit YCbCrA
    procedure SaveToJpegRGBFile(fn: SystemString; Quality: TJpegQuality);     // published,24bit YCbCr
    procedure SaveToJpegCMYKRGBFile(fn: SystemString; Quality: TJpegQuality); // custom,24bit CMYK
    procedure SaveToJpegGrayFile(fn: SystemString; Quality: TJpegQuality);    // published,8bit grayscale
    procedure SaveToJpegGrayAFile(fn: SystemString; Quality: TJpegQuality);   // custom,8bit grayscale + 8bit alpha

    { Rasterization pixel }
    property Pixel[const X, Y: Integer]: TRColor read GetPixel write SetPixel; default;
    property PixelBGRA[const X, Y: Integer]: TRColor read GetPixelBGRA write SetPixelBGRA;
    property PixelPtr[const X, Y: Integer]: PRColor read GetPixelPtr;
    property PixelRed[const X, Y: Integer]: Byte read GetPixelRed write SetPixelRed;
    property PixelGreen[const X, Y: Integer]: Byte read GetPixelGreen write SetPixelGreen;
    property PixelBlue[const X, Y: Integer]: Byte read GetPixelBlue write SetPixelBlue;
    property PixelAlpha[const X, Y: Integer]: Byte read GetPixelAlpha write SetPixelAlpha;
    property PixelGray[const X, Y: Integer]: Byte read GetGray write SetGray;
    property PixelGrayS[const X, Y: Integer]: TGeoFloat read GetGrayS write SetGrayS;
    property PixelGrayD[const X, Y: Integer]: Double read GetGrayD write SetGrayD;
    property PixelF[const X, Y: TGeoFloat]: TRColor read GetPixelF write SetPixelF;
    property PixelVec[const v2: TVec2]: TRColor read GetPixelVec write SetPixelVec;
    property PixelLinearF[const X, Y: TGeoFloat]: TRColor read GetPixelLinearF;
    property PixelLinear[const X, Y: Integer]: TRColor read GetPixelLinear;
    property ScanLine[Y: Integer]: PRColorArray read GetScanLine;
    property Bits: PRColorArray read GetBits;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Width0: TGeoFloat read GetWidth0;
    property Height0: TGeoFloat read GetHeight0;
    property Width0i: Integer read GetWidth0i;
    property Height0i: Integer read GetHeight0i;

    { blend options }
    property DrawMode: TDrawMode read FDrawMode write FDrawMode default dmOpaque;
    property CombineMode: TCombineMode read FCombineMode write FCombineMode default cmBlend;
    property MasterAlpha: Cardinal read FMasterAlpha write FMasterAlpha;
    property OuterColor: TRColor read FOuterColor write FOuterColor;

    { user define }
    property UserObject: TCoreClassObject read FUserObject write FUserObject;
    property UserData: Pointer read FUserData write FUserData;
    property UserText: SystemString read FUserText write FUserText;
    property UserToken: SystemString read FUserToken write FUserToken;
    property UserVariant: Variant read FUserVariant write FUserVariant;
  end;

  TMemoryRasterClass = class of TMemoryRaster;

  TMemoryRasterArray = array of TMemoryRaster;

  TMemoryRaster2DArray = array of TMemoryRasterArray;

  TMemoryRasterList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TMemoryRaster>;
  TByteRasterList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TByteRaster>;

  TMemoryRasterList = class(TMemoryRasterList_Decl)
  public
  end;

  TByteRasterList = class(TByteRasterList_Decl)
  public
    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);
  end;

  TRasterArray = array of TMemoryRaster;
  TRasterMatrix = array of TRasterArray;

  TRaster = TMemoryRaster;

{$ENDREGION 'MemoryRaster'}
{$REGION 'MemoryRasterSerialized'}

  TRasterSerialized = class
  protected
    FStream: TCoreClassStream;
    FAutoFreeStream: Boolean;
    FCritical: TCritical;
    FWriteList, FReadList: TMemoryRasterList;
  public
    constructor Create(stream_: TCoreClassStream);
    destructor Destroy; override;

    function Write(R: TMemoryRaster): Int64;
    function Read(R: TMemoryRaster): Int64;
    procedure Remove(R: TMemoryRaster);
    procedure Clear;

    property AutoFreeStream: Boolean read FAutoFreeStream write FAutoFreeStream;
    property stream: TCoreClassStream read FStream;
    property Critical: TCritical read FCritical;
    property WriteList: TMemoryRasterList read FWriteList;
    property ReadList: TMemoryRasterList read FReadList;
  end;

{$ENDREGION 'MemoryRasterSerialized'}
{$REGION 'TSequenceMemoryRaster'}

  TSequenceMemoryRaster = class(TRaster)
  protected
    FTotal: Integer;
    FColumn: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear(FillColor: TRColor); override;
    procedure SetSize(NewWidth, NewHeight: Integer; const ClearColor: TRColor); override;

    procedure Reset; override;
    procedure Assign(sour: TMemoryRaster); override;

    class function CanLoadStream(stream: TCoreClassStream): Boolean; override;
    procedure LoadFromStream(stream: TCoreClassStream); override;
    procedure SaveToStream(stream: TCoreClassStream); override;

    property Total: Integer read FTotal write FTotal;
    property Column: Integer read FColumn write FColumn;

    function SequenceFrameRect(index: Integer): TRect;
    procedure ExportSequenceFrame(index: Integer; output: TMemoryRaster);
    procedure ReverseSequence(output: TSequenceMemoryRaster);
    procedure GradientSequence(output: TSequenceMemoryRaster);
    function FrameWidth: Integer;
    function FrameHeight: Integer;
    function FrameRect2D: TRectV2;
    function FrameRect: TRect;
  end;

  TSequenceMemoryRasterClass = class of TSequenceMemoryRaster;

{$ENDREGION 'TSequenceMemoryRaster'}
{$REGION 'AGG'}

  TMemoryRaster_AggImage = class(TAgg2DImage)
  public
    constructor Create(raster: TMemoryRaster); overload;
    procedure Attach(raster: TMemoryRaster); overload;
  end;

  TMemoryRaster_Agg2D = class(TAgg2D)
  private
    function GetImageBlendColor: TRColor;
    procedure SetImageBlendColor(const Value: TRColor);
    function GetFillColor: TRColor;
    procedure SetFillColor(const Value: TRColor);
    function GetLineColor: TRColor;
    procedure SetLineColor(const Value: TRColor);
  public
    procedure Attach(raster: TMemoryRaster); overload;

    procedure FillLinearGradient(x1, y1, x2, y2: Double; c1, c2: TRColor; Profile: Double = 1);
    procedure LineLinearGradient(x1, y1, x2, y2: Double; c1, c2: TRColor; Profile: Double = 1);

    procedure FillRadialGradient(X, Y, R: Double; c1, c2: TRColor; Profile: Double = 1); overload;
    procedure LineRadialGradient(X, Y, R: Double; c1, c2: TRColor; Profile: Double = 1); overload;

    procedure FillRadialGradient(X, Y, R: Double; c1, c2, c3: TRColor); overload;
    procedure LineRadialGradient(X, Y, R: Double; c1, c2, c3: TRColor); overload;

    property ImageBlendColor: TRColor read GetImageBlendColor write SetImageBlendColor;
    property FillColor: TRColor read GetFillColor write SetFillColor;
    property LineColor: TRColor read GetLineColor write SetLineColor;
  end;

{$ENDREGION 'AGG'}
{$REGION 'Vertex'}

  PVertexMap = ^TVertexMap;

  TVertexMap = class(TCoreClassObject)
  private type
    { Setup interpolation constants for linearly varying vaues }
    TBilerpConsts = packed record
      A, B, c: TGeoFloat;
    end;

    { fragment mode }
    TFragSampling = (fsSolid, fsNearest, fsLinear);
    TNearestWriteBuffer = array of Byte;
    TSamplerBlend = procedure(const Sender: PVertexMap; const f, M: TRColor; var B: TRColor);
    TComputeSamplerColor = function(const Sender: PVertexMap; const Sampler: TMemoryRaster; const X, Y: TGeoFloat): TRColor;
  private
    // rasterization nearest templet
    FNearestWriteBuffer: TNearestWriteBuffer;
    FNearestWriterID: Byte;
    FCurrentUpdate: ShortInt;
    // sampler shader
    ComputeNearest: TComputeSamplerColor;
    ComputeLinear: TComputeSamplerColor;
    ComputeBlend: TSamplerBlend;

    // fill triangle
    procedure RasterizeTriangle(const FS: TFragSampling; const sc: TRColor; const tex: TMemoryRaster; const SamplerTri, RenderTri: TTriangle);
    // fragment
    procedure FillFragment(const FS: TFragSampling; const sc: TRColor; const tex: TMemoryRaster;
      const bitDst, j, start_x, frag_count: Integer; const attr_v, attr_u: TBilerpConsts);
    // nearest state buffer
    procedure NewWriterBuffer;
    // internal
    procedure internal_Draw(const RenderTri: TTriangle; const Sampler: TRColor); overload;
    procedure internal_Draw(const SamplerTri, RenderTri: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean); overload;
    procedure internal_Draw(const SamplerTri, RenderTri: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: TGeoFloat); overload;
  public
    // draw triangle edge
    DrawTriangleEdge: Boolean;
    TriangleEdgeColor: TRColor;
    // render window
    Window: TMemoryRaster;
    WindowSize: Integer;
    // user define
    UserData: Pointer;

    constructor Create(raster: TMemoryRaster);
    destructor Destroy; override;

    property NearestWriterID: Byte read FNearestWriterID;
    property NearestWriteBuffer: TNearestWriteBuffer read FNearestWriteBuffer;
    function BeginUpdate: Byte;
    procedure EndUpdate;

    (*
      all Absolute coordiantes
    *)
    procedure DrawTriangle(const v1, v2, v3: TVec2; const Sampler: TRColor); overload;
    procedure DrawTriangle(const RenderTri: TTriangle; const Sampler: TRColor); overload;
    procedure DrawTriangle(const SamplerTri, RenderTri: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean); overload;
    procedure DrawTriangle(const SamplerTri, RenderTri: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: TGeoFloat); overload;

    (*
      SamVec: (TV2Rect4) sampler Absolute coordiantes
      RenVec: (TV2Rect4) renderer Absolute coordiantes
      Sampler: MemoryRaster or Solid color
      bilinear_sampling: used Linear sampling
    *)
    procedure DrawRect(const RenVec: TV2Rect4; const Sampler: TRColor); overload;
    procedure DrawRect(const SamVec, RenVec: TV2Rect4; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: TGeoFloat); overload;

    (*
      SamVec: (TRectV2) sampler Absolute coordiantes
      RenVec: (TRectV2) renderer Absolute coordiantes
      RenAngle: (TGeoFloat) renderer rotation
      Sampler: MemoryRaster or Solid color
      bilinear_sampling: used Linear sampling
    *)
    procedure DrawRect(const RenVec: TRectV2; const Sampler: TRColor); overload;
    procedure DrawRect(const SamVec, RenVec: TRectV2; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: TGeoFloat); overload;
    procedure DrawRect(const RenVec: TRectV2; const RenAngle: TGeoFloat; const Sampler: TRColor); overload;
    procedure DrawRect(const SamVec, RenVec: TRectV2; const RenAngle: TGeoFloat; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: TGeoFloat); overload;

    (*
      SamVec: (TV2Rect4) sampler Absolute coordiantes
      RenVec: (TRectV2) renderer Absolute coordiantes
      RenAngle: (TGeoFloat) renderer rotation
      Sampler: MemoryRaster or Solid color
      bilinear_sampling: used Linear sampling
    *)
    procedure DrawRect(const SamVec: TV2Rect4; const RenVec: TRectV2; const RenAngle: TGeoFloat; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: TGeoFloat); overload;

    (*
      SamVec: (TVec2List) sampler Absolute coordiantes
      RenVec: (TVec2List) renderer Absolute coordiantes
      cen: Centroid coordinate
      Sampler: MemoryRaster or Solid color
      bilinear_sampling: used Linear sampling
    *)
    procedure FillPoly(const RenVec: TVec2List; const cen: TVec2; const Sampler: TRColor); overload;
    procedure FillPoly(const RenVec: TVec2List; const Sampler: TRColor); overload;
    procedure FillPoly(const SamVec, RenVec: TVec2List; const SamCen, RenCen: TVec2; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: Single); overload;
    procedure FillPoly(const SamVec, RenVec: TVec2List; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: Single); overload;
  end;

{$ENDREGION 'Vertex'}
{$REGION 'TFontRaster'}

  TFontRaster = class(TCoreClassObject)
  private type
    PFontCharDefine = ^TFontCharDefine;

    TFontCharDefine = packed record
      Activted: Boolean;
      X, Y: Word;
      W, H: Byte;
    end;

    TFontTable = array [0 .. MaxInt div SizeOf(TFontCharDefine) - 1] of TFontCharDefine;
    PFontTable = ^TFontTable;

    TFontBitRaster = array [0 .. MaxInt - 1] of Byte;
    PFontBitRaster = ^TFontBitRaster;

{$IFDEF FPC}
    TFontRasterString = TUPascalString;
    TFontRasterChar = USystemChar;
{$ELSE FPC}
    TFontRasterString = TPascalString;
    TFontRasterChar = SystemChar;
{$ENDIF FPC}

    TFontDrawState = record
      Owner: TFontRaster;
      DestColor: TRColor;
    end;

    PFontDrawState = ^TFontDrawState;
  private const
    C_WordDefine: TFontCharDefine = (Activted: False; X: 0; Y: 0; W: 0; H: 0);
    C_MAXWORD = $FFFF;
  protected
    FOnlyInstance: Boolean;
    FFontTable: PFontTable;
    FFragRaster: array of TMemoryRaster;
    FBitRaster: PFontBitRaster;
    FFontSize: Integer;
    FActivtedWord: Integer;
    FWidth: Integer;
    FHeight: Integer;
  public
    constructor Create; overload;
    constructor Create(ShareFont: TFontRaster); overload;
    destructor Destroy; override;

    // generate word
    procedure Add(c: TFontRasterChar; raster: TMemoryRaster);
    procedure Remove(c: TFontRasterChar);
    procedure Clear;
    procedure Build(fontSiz: Integer);

    property FontSize: Integer read FFontSize;
    property ActivtedWord: Integer read FActivtedWord;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;

    // store
    procedure LoadFromStream(stream: TCoreClassStream);
    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromFile(filename: TPascalString);
    procedure SaveToFile(filename: TPascalString);

    procedure ExportRaster(stream: TCoreClassStream; partitionLine: Boolean);

    // draw font
    function CharSize(const c: TFontRasterChar): TPoint;
    function TextSize(const s: TFontRasterString; charVec2List: TVec2List): TVec2; overload;
    function TextSize(const s: TFontRasterString): TVec2; overload;
    function TextWidth(const s: TFontRasterString): Word;
    function TextHeight(const s: TFontRasterString): Word;

    function Draw(Text: TFontRasterString; Dst: TMemoryRaster; dstVec: TVec2; dstColor: TRColor;
      const bilinear_sampling: Boolean; const alpha: TGeoFloat; const axis: TVec2; const Angle, Scale: TGeoFloat): TVec2; overload;

    procedure Draw(Text: TFontRasterString; Dst: TMemoryRaster; dstVec: TVec2; dstColor: TRColor); overload;
  end;
{$ENDREGION 'TFontRaster'}
{$REGION 'Color Segmentation'}

  TColorSegmentation = class;
  TSegClassify = Cardinal;
  PSeg = ^TSeg;

  TSeg = record
    Y, L, R: Integer;
    LTop, RTop, LBot, RBot: PSeg;
    GroupID: Integer;
    Classify: TSegClassify;
  end;

  TSegPool_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PSeg>;

  TSegPool = class(TSegPool_Decl)
  private
    FBoundsCached: Boolean;
    FBoundsCache: TRectV2;
  public
    Owner: TColorSegmentation;
    constructor Create;
    procedure AddSeg(const buff: array of PSeg);
    procedure SortY;
    function Classify: TSegClassify;
    function BoundsRectV2(cache: Boolean): TRectV2; overload;
    function BoundsRectV2: TRectV2; overload;
    function BoundsRect: TRect;
    function Left: Integer;
    function Top: Integer;
    function Width: Integer;
    function Height: Integer;
    function PixelArea: Integer;
    function Area: Integer;
    procedure FillTo(dest: TMemoryRaster; DataColor: TRColor);
    function BuildDatamap(backColor, DataColor: TRColor): TMemoryRaster;
    function BuildClipDatamap(backColor, DataColor: TRColor): TMemoryRaster;
    function BuildClipMap(Source: TMemoryRaster; backColor: TRColor): TMemoryRaster;
  end;

  TSegPoolGroup = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TSegPool>;

  TSegMap = array of array of PSeg;
  TOnSegColor = procedure(Color: TRColor; var Classify: TSegClassify) of Object;

  TColorSegmentation = class(TCoreClassObject)
  private
    FRaster: TMemoryRaster;
    FRasterBoundsRect: TRect;
    FWidth, FHeight: Integer;
    FSegMap: TSegMap;
    FSource: TSegPool_Decl;
    FSegPoolGroup: TSegPoolGroup;
    FOnSegColor: TOnSegColor;

    function NewSeg(X, Y: Integer; Classify: TSegClassify): PSeg;
    function FindSegPool(p: PSeg): TSegPool;
    function GetOrCreateSegPool(p: PSeg): TSegPool;
    procedure AddSegToGroup(p: PSeg);

    function GetSegPool(X, Y: Integer): TSegPool;
    function GetItems(index: Integer): TSegPool;
  public
    constructor CustomCreate(Raster_: TMemoryRaster; BoundsRect_: TRect);
    constructor Create(Raster_: TMemoryRaster);
    destructor Destroy; override;

    function DoSegColor(Color: TRColor): TSegClassify; virtual;
    procedure BuildSegmentation;
    procedure UpdateSegMap;
    procedure MergeOverlapSegmentation;
    function RemoveNoise(PixelNoiseThreshold: Integer): Boolean;

    function Count: Integer;
    property Items[index: Integer]: TSegPool read GetItems; default;
    property SegPool[X, Y: Integer]: TSegPool read GetSegPool;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property OnSegColor: TOnSegColor read FOnSegColor write FOnSegColor;
  end;
{$ENDREGION 'Color Segmentation'}
{$REGION 'RasterAPI'}


procedure Wait_SystemFont_Init;

function ClampInt(const Value, Min, Max: Integer): Integer; overload;
function ClampByte3(const Value, Min, Max: Byte): Byte;
function ClampByte(const Value: Integer): Byte;

procedure DisposeRasterArray(var arry: TMemoryRasterArray);

procedure BlendBlock(Dst: TMemoryRaster; dstRect: TRect; Src: TMemoryRaster; Srcx, Srcy: Integer; CombineOp: TDrawMode);
procedure BlockTransfer(Dst: TMemoryRaster; Dstx: Integer; Dsty: Integer; DstClip: TRect; Src: TMemoryRaster; SrcRect: TRect; CombineOp: TDrawMode);

procedure FillRasterColor(var X; Count: Cardinal; Value: TRasterColor);
procedure CopyRasterColor(const Source; var dest; Count: Cardinal);
function RandomRasterColor(const A: Byte = $FF): TRasterColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RasterColor(const R, G, B, A: Byte): TRasterColor; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RasterColor(const R, G, B: Byte): TRasterColor; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RasterColorInv(const c: TRasterColor): TRasterColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RasterAlphaColor(const c: TRasterColor; const A: Byte): TRasterColor;
function RasterAlphaColorF(const c: TRasterColor; const A: Single): TRasterColor;
function RasterColorF(const R, G, B, A: TGeoFloat): TRasterColor; overload;
function RasterColorF(const R, G, B: TGeoFloat): TRasterColor; overload;
procedure RasterColor2F(const c: TRasterColor; var R, G, B, A: TGeoFloat); overload;
procedure RasterColor2F(const c: TRasterColor; var R, G, B: TGeoFloat); overload;
function RasterColor2Vec4(const c: TRasterColor): TVec4;
function RasterColor2Vector4(const c: TRasterColor): TVector4;
function RasterColor2Vec3(const c: TRasterColor): TVec3;
function RasterColor2Vector3(const c: TRasterColor): TVector3;
function RasterColor2Gray(const c: TRasterColor): Byte; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RasterColor2GrayS(const c: TRasterColor): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RasterColor2GrayD(const c: TRasterColor): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure FillRColor(var X; Count: Cardinal; Value: TRColor);
procedure CopyRColor(const Source; var dest; Count: Cardinal);
function RandomRColor(const A: Byte = $FF): TRColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RColor(const R, G, B, A: Byte): TRColor; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RColor(const R, G, B: Byte): TRColor; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RColorInv(const c: TRColor): TRColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RColorF(const R, G, B, A: TGeoFloat): TRColor; overload;
function RColorF(const R, G, B: TGeoFloat): TRColor; overload;
procedure RColor2F(const c: TRColor; var R, G, B, A: TGeoFloat); overload;
procedure RColor2F(const c: TRColor; var R, G, B: TGeoFloat); overload;
function RColor2Vec4(const c: TRColor): TVec4;
function RColor2Vector4(const c: TRColor): TVector4;
function RColor2Vec3(const c: TRColor): TVec3;
function RColor2Vector3(const c: TRColor): TVector3;
function RColor2Gray(const c: TRColor): Byte; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RColor2GrayS(const c: TRColor): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RColor2GrayD(const c: TRColor): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RColorDistance(c1, c2: TRColor): TGeoFloat;

function RGBA2BGRA(const sour: TRColor): TRColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function BGRA2RGBA(const sour: TRColor): TRColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RGBA2RGB(const sour: TRColor): TRGB; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RGBA2BGR(const sour: TRColor): TRGB; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RGB2BGR(const sour: TRGB): TRGB; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function BGR2RGB(const sour: TRGB): TRGB; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RGB2RGBA(const sour: TRGB): TRColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure SwapBR(var sour: TRGB); {$IFDEF INLINE_ASM} inline; {$ENDIF}overload;
procedure SwapBR(var sour: TRColor); {$IFDEF INLINE_ASM} inline; {$ENDIF}overload;

function MaxRGBComponent(sour: TRColor): Byte; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MaxRGBIndex(sour: TRColor): Byte; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MinRGBComponent(sour: TRColor): Byte; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MinRGBIndex(sour: TRColor): Byte; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function AggColor(const Value: TRColor): TAggColorRgba8; {$IFDEF INLINE_ASM} inline; {$ENDIF}overload;
function AggColor(const R, G, B: TGeoFloat; const A: TGeoFloat = 1.0): TAggColorRgba8; {$IFDEF INLINE_ASM} inline; {$ENDIF}overload;
function AggColor(const Value: TAggColorRgba8): TRColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}overload;

function ComputeSize(const MAX_Width, MAX_Height: Integer; var Width, Height: Integer): Single;

procedure FastBlur(Source, dest: TMemoryRaster; radius: Double; const Bounds: TRect); overload;
procedure FastBlur(Source: TMemoryRaster; radius: Double; const Bounds: TRect); overload;
procedure GaussianBlur(Source, dest: TMemoryRaster; radius: Double; const Bounds: TRect); overload;
procedure GaussianBlur(Source: TMemoryRaster; radius: Double; const Bounds: TRect); overload;
procedure GrayscaleBlur(Source, dest: TMemoryRaster; radius: Double; const Bounds: TRect); overload;
procedure GrayscaleBlur(Source: TMemoryRaster; radius: Double; const Bounds: TRect); overload;

procedure Antialias32(const DestMR: TMemoryRaster; AXOrigin, AYOrigin, AXFinal, AYFinal: Integer); overload;
procedure Antialias32(const DestMR: TMemoryRaster; const AAmount: Integer); overload;
procedure HistogramEqualize(const mr: TMemoryRaster); overload;
procedure HistogramEqualize(const mr1, mr2: TMemoryRaster); overload;
procedure RemoveRedEyes(const mr: TMemoryRaster);
procedure Sepia32(const mr: TMemoryRaster; const Depth: Byte);
procedure Sharpen(const DestMR: TMemoryRaster; const SharpenMore: Boolean);
procedure AddColorNoise32(const Source: TMemoryRaster; const AAmount: Integer);
procedure AddMonoNoise32(const Source: TMemoryRaster; const AAmount: Integer);

type
  TDiagonalDirection = (ddLeftDiag, ddRightDiag);
procedure Diagonal(const Source, dest: TMemoryRaster; const backColor: TRColor; const Amount: Integer; const DiagDirection: TDiagonalDirection);

procedure GrayscaleToAlpha(Src: TMemoryRaster);
procedure AlphaToGrayscale(Src: TMemoryRaster);
procedure IntensityToAlpha(Src: TMemoryRaster);
procedure ReversalAlpha(Src: TMemoryRaster);
procedure RGBToGrayscale(Src: TMemoryRaster);

procedure FillBlackGrayBackgroundTexture(bk: TMemoryRaster; block_size: Integer);

procedure ColorToTransparent(SrcColor: TRColor; Src, Dst: TMemoryRaster);

function BuildSequenceFrame(bmp32List: TCoreClassListForObj; Column: Integer; Transparent: Boolean): TSequenceMemoryRaster;
function GetSequenceFrameRect(bmp: TMemoryRaster; Total, Column, index: Integer): TRect;
procedure GetSequenceFrameOutput(bmp: TMemoryRaster; Total, Column, index: Integer; output: TMemoryRaster);

function AnalysisColors(mr: TMemoryRaster; ignoreColors: TRColors; MaxCount: Integer): TRColors;

function BlendReg(f, B: TRColor): TRColor; register;
procedure BlendMem(f: TRColor; var B: TRColor); register;
function BlendRegEx(f, B, M: TRColor): TRColor; register;
procedure BlendMemEx(f: TRColor; var B: TRColor; M: TRColor); register;
procedure BlendLine(Src, Dst: PRColor; Count: Integer); register;
procedure BlendLineEx(Src, Dst: PRColor; Count: Integer; M: TRColor); register;
function CombineReg(X, Y, W: TRColor): TRColor; register;
procedure CombineMem(X: TRColor; var Y: TRColor; W: TRColor); register;
procedure CombineLine(Src, Dst: PRColor; Count: Integer; W: TRColor); register;
function MergeReg(f, B: TRColor): TRColor; register;
function MergeRegEx(f, B, M: TRColor): TRColor; register;
procedure MergeMem(f: TRColor; var B: TRColor); register;
procedure MergeMemEx(f: TRColor; var B: TRColor; M: TRColor); register;
procedure MergeLine(Src, Dst: PRColor; Count: Integer); register;
procedure MergeLineEx(Src, Dst: PRColor; Count: Integer; M: TRColor); register;

{
  JPEG-LS Codec
  This code is based on http://www.stat.columbia.edu/~jakulin/jpeg-ls/mirror.htm
  Converted from C to Pascal. 2017

  fixed by 600585@qq.com, v2.3
  2018-5
}
procedure jls_RasterToRaw3(ARaster: TMemoryRaster; RawStream: TCoreClassStream);
procedure jls_RasterToRaw1(ARaster: TMemoryRaster; RawStream: TCoreClassStream);
procedure jls_GrayRasterToRaw1(const ARaster: PByteRaster; RawStream: TCoreClassStream);
function EncodeJpegLSRasterToStream3(ARaster: TMemoryRaster; const stream: TCoreClassStream): Boolean;
function EncodeJpegLSRasterToStream1(ARaster: TMemoryRaster; const stream: TCoreClassStream): Boolean; overload;
function DecodeJpegLSRasterFromStream(const stream: TCoreClassStream; ARaster: TMemoryRaster): Boolean;
function EncodeJpegLSGrayRasterToStream(const ARaster: PByteRaster; const stream: TCoreClassStream): Boolean; overload;
function DecodeJpegLSGrayRasterFromStream(const stream: TCoreClassStream; var ARaster: TByteRaster): Boolean;

{
  document rotation detected
  by 600585@qq.com

  2018-8
}
{ Calculates rotation angle for given 8bit grayscale image.
  Useful for finding skew of scanned documents etc.
  Uses Hough transform internally.
  MaxAngle is maximal (abs. value) expected skew angle in degrees (to speed things up)
  and Threshold (0..255) is used to classify pixel as black (text) or white (background).
  Area of interest rectangle can be defined to restrict the detection to
  work only in defined part of image (useful when the document has text only in
  smaller area of page and non-text features outside the area confuse the rotation detector).
  Various calculations stats can be retrieved by passing Stats parameter. }
function DocmentRotationDetected(const MaxAngle: TGeoFloat; const Treshold: Integer; raster: TMemoryRaster): TGeoFloat;

{
  YV12
}
procedure YV12ToRasterization(sour: TCoreClassStream; dest: TMemoryRaster);
procedure RasterizationToYV12(Compressed: Boolean; sour: TMemoryRaster; dest: TCoreClassStream);

{
  Half YV12
}
procedure HalfYUVToRasterization(sour: TCoreClassStream; dest: TMemoryRaster);
procedure RasterizationToHalfYUV(Compressed: Boolean; sour: TMemoryRaster; dest: TCoreClassStream);

{
  quart YV12
}
procedure QuartYUVToRasterization(sour: TCoreClassStream; dest: TMemoryRaster);
procedure RasterizationToQuartYUV(Compressed: Boolean; sour: TMemoryRaster; dest: TCoreClassStream);

{
  byte raster
}
procedure SaveByteRasterToStream(raster: TByteRaster; stream: TCoreClassStream);
procedure LoadByteRasterFromStream(var raster: TByteRaster; stream: TCoreClassStream);

{
  word raster
}
procedure SaveWordRasterToStream(raster: TWordRaster; stream: TCoreClassStream);
procedure LoadWordRasterFromStream(var raster: TWordRaster; stream: TCoreClassStream);

var
  NewRaster: function: TMemoryRaster;
  NewRasterFromFile: function(const fn: string): TMemoryRaster;
  NewRasterFromStream: function(const stream: TCoreClassStream): TMemoryRaster;
  SaveRaster: procedure(mr: TMemoryRaster; const fn: string);
{$ENDREGION 'RasterAPI'}

implementation

uses
{$IFDEF parallel}
{$IFDEF FPC}
  mtprocs,
{$ELSE}
  Threading,
{$ENDIF FPC}
{$ENDIF}
  h264Common, CoreCompress, DoStatusIO, DataFrameEngine, Raster_JPEG;

{$REGION 'InternalDefines'}


var
  RcTable: array [Byte, Byte] of Byte;
  DivTable: array [Byte, Byte] of Byte;
  SystemFont: TFontRaster;

type
  TLUT8 = array [Byte] of Byte;
  TLogicalOperator = (loXOR, loAND, loOR);

  TByteArray = array [0 .. MaxInt div SizeOf(Byte) - 1] of Byte;
  PByteArray = ^TByteArray;

  TBmpHeader = packed record
    bfType: Word;
    bfSize: Integer;
    bfReserved1: Word;
    bfReserved2: Word;
    bfOffBits: Integer;
    biSize: Integer;
    biWidth: Integer;
    biHeight: Integer;
    biPlanes: Word;
    biBitCount: Word;
    biCompression: Integer;
    biSizeImage: Integer;
    biXPelsPerMeter: Integer;
    biYPelsPerMeter: Integer;
    biClrUsed: Integer;
    biClrImportant: Integer;
  end;

  TYV12Head = packed record
    Version: Byte;
    Compessed: Byte;
    Width: Integer;
    Height: Integer;
  end;

  TBlendLine = procedure(Src, Dst: PRColor; Count: Integer);
  TBlendLineEx = procedure(Src, Dst: PRColor; Count: Integer; M: TRColor);

  TRasterSerializedHeader = packed record
    Width, Height: Integer;
    siz: Int64;
    UsedAGG: Boolean;
  end;

const
  ZERO_RECT: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);

{$ENDREGION 'InternalDefines'}

function IntersectRect_(out Dst: TRect; const r1, r2: TRect): Boolean; forward;
procedure OffsetRect_(var R: TRect; dx, dy: Integer); forward;
function IsRectEmpty_(const R: TRect): Boolean; forward;

{$INCLUDE MemoryRaster_RasterClass.inc}
{$INCLUDE MemoryRaster_SequenceClass.inc}
{$INCLUDE MemoryRaster_Vertex.inc}
{$INCLUDE MemoryRaster_Agg.inc}
{$INCLUDE MemoryRaster_Font.inc}
{$INCLUDE MemoryRaster_ExtApi.inc}
{$INCLUDE MemoryRaster_ColorSegmentation.inc}


function NewRaster_: TMemoryRaster;
begin
  Result := TMemoryRaster.Create;
end;

function NewRasterFromFile_(const fn: string): TMemoryRaster;
begin
  Result := NewRaster();
  Result.LoadFromFile(fn);
end;

function NewRasterFromStream_(const stream: TCoreClassStream): TMemoryRaster;
var
  m64: TMemoryStream64;
begin
  Result := NewRaster();

  stream.Position := 0;
  m64 := TMemoryStream64.Create;
  if stream is TMemoryStream64 then
      m64.SetPointerWithProtectedMode(TMemoryStream64(stream).Memory, TMemoryStream64(stream).Size)
  else
      m64.CopyFrom(stream, stream.Size);
  m64.Position := 0;

  Result.LoadFromStream(m64);

  DisposeObject(m64);
end;

procedure SaveRaster_(mr: TMemoryRaster; const fn: string);
begin
  mr.SaveToFile(fn);
end;

initialization

NewRaster := {$IFDEF FPC}@{$ENDIF FPC}NewRaster_;
NewRasterFromFile := {$IFDEF FPC}@{$ENDIF FPC}NewRasterFromFile_;
NewRasterFromStream := {$IFDEF FPC}@{$ENDIF FPC}NewRasterFromStream_;
SaveRaster := {$IFDEF FPC}@{$ENDIF FPC}SaveRaster_;

MakeMergeTables;
Init_DefaultFont;

finalization

Free_DefaultFont;

end.
