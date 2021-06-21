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

uses Types, Math, Variants, TypInfo, CoreClasses, MemoryStream64, Geometry2DUnit, Geometry3DUnit,
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

  TYIQ = record
  private
    function GetRGB: TRColor;
    procedure SetRGB(const Value: TRColor);
    function GetRGBA(A: Byte): TRColor;
    procedure SetRGBA(A: Byte; const Value: TRColor);
  public
    Y, I, Q: TGeoFloat;
    property RGB: TRColor read GetRGB write SetRGB;
    property RGBA[A: Byte]: TRColor read GetRGBA write SetRGBA;
  end;

  THSI = record
  private
    function GetRGB: TRColor;
    procedure SetRGB(const Value: TRColor);
    function GetRGBA(A: Byte): TRColor;
    procedure SetRGBA(A: Byte; const Value: TRColor);
  public
    H, S, I: TGeoFloat;
    property RGB: TRColor read GetRGB write SetRGB;
    property RGBA[A: Byte]: TRColor read GetRGBA write SetRGBA;
  end;

  TCMYK = record
  private
    function GetRGB: TRColor;
    procedure SetRGB(const Value: TRColor);
    function GetRGBA(A: Byte): TRColor;
    procedure SetRGBA(A: Byte; const Value: TRColor);
  public
    C, M, Y, K: TGeoFloat;
    property RGB: TRColor read GetRGB write SetRGB;
    property RGBA[A: Byte]: TRColor read GetRGBA write SetRGBA;
  end;

  PRColorEntry = ^TRColorEntry;
  PHSI = ^THSI;
  PYIQ = ^TYIQ;
  PCMYK = ^TCMYK;

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
  TByteBuffer = array [0 .. MaxInt - 1] of Byte;
  PByteBuffer = ^TByteBuffer;
  TWordBuffer = array [0 .. MaxInt div SizeOf(Word) - 1] of Word;
  PWordBuffer = ^TWordBuffer;

  TMemoryRaster = class;
  TMemoryRaster_AggImage = class;
  TMemoryRaster_Agg2D = class;
  TRasterVertex = class;
  TFontRaster = class;
  TRasterSerialized = class;
  TMorphomaticsValue = TGeoFloat;
  PMorphomaticsValue = ^TMorphomaticsValue;
  TMorphomatics = class;
  TMorphologyBinaryzation = class;
  TMorphologyClassify = Cardinal;
  TOnGetPixelSegClassify = procedure(X, Y: Integer; Color: TRColor; var Classify: TMorphologyClassify) of Object;
  TOnGetMorphomaticsSegClassify = procedure(X, Y: Integer; Morph: TMorphomaticsValue; var Classify: TMorphologyClassify) of Object;
  TMorphologySegmentation = class;
  TMorphologyRCLines = class;

  TMorphologyPixel = (
    mpGrayscale,
    mpYIQ_Y, mpYIQ_I, mpYIQ_Q,
    mpHSI_H, mpHSI_S, mpHSI_I,
    mpCMYK_C, mpCMYK_M, mpCMYK_Y, mpCMYK_K,
    mpR, mpG, mpB, mpA,
    // approximate color
    mpApproximateBlack,
    mpApproximateWhite,
    mpCyan,
    mpMagenta,
    mpYellow
    );
  TMorphPixel = TMorphologyPixel;
  TMorphPix = TMorphologyPixel;
  TMPix = TMorphologyPixel;

  TMorphologyPixelInfo = array [TMorphologyPixel] of SystemString;

  // short define
  TRaster = TMemoryRaster;
  TMorphMath = TMorphomatics;
  TMMath = TMorphomatics;
  TMorphBin = TMorphologyBinaryzation;
  TMBin = TMorphologyBinaryzation;
  TMorphSeg = TMorphologySegmentation;
  TMSeg = TMorphologySegmentation;
  TMorphRCLines = TMorphologyRCLines;
  TMRCLines = TMorphologyRCLines;
  TMRCL = TMorphologyRCLines;

  // rasterization save format.
  TRasterSaveFormat = (
    rsRGBA, rsRGB,
    rsYV12, rsHalfYUV, rsQuartYUV, rsFastYV12, rsFastHalfYUV, rsFastQuartYUV,
    rsJPEG_YCbCrA_Qualily90, rsJPEG_YCbCr_Qualily90, rsJPEG_Gray_Qualily90, rsJPEG_GrayA_Qualily90,
    rsJPEG_YCbCrA_Qualily80, rsJPEG_YCbCr_Qualily80, rsJPEG_Gray_Qualily80, rsJPEG_GrayA_Qualily80,
    rsJPEG_YCbCrA_Qualily70, rsJPEG_YCbCr_Qualily70, rsJPEG_Gray_Qualily70, rsJPEG_GrayA_Qualily70,
    rsJPEG_YCbCrA_Qualily60, rsJPEG_YCbCr_Qualily60, rsJPEG_Gray_Qualily60, rsJPEG_GrayA_Qualily60,
    rsJPEG_YCbCrA_Qualily50, rsJPEG_YCbCr_Qualily50, rsJPEG_Gray_Qualily50, rsJPEG_GrayA_Qualily50,
    rsJPEG_CMYK_Qualily90, rsJPEG_CMYK_Qualily80, rsJPEG_CMYK_Qualily70, rsJPEG_CMYK_Qualily60, rsJPEG_CMYK_Qualily50,
    rsJPEG_YCbCrA_Qualily100, rsJPEG_YCbCr_Qualily100, rsJPEG_Gray_Qualily100, rsJPEG_GrayA_Qualily100, rsJPEG_CMYK_Qualily100,
    rsGrayscale, rsColor255, rsPNG
    );

  TOnGetRasterizationMemory = procedure(Sender: TMemoryRaster) of object;

  TRColors_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TRColor>;
  TRColors = TRColors_Decl;
  TRasterColors = TRColors;

  THoughLine = record
    Count, index: Integer;
    alpha, Distance: TGeoFloat;
  end;

  THoughLineArry = array of THoughLine;

{$ENDREGION 'base define'}
{$REGION 'MemoryRaster'}

  TMemoryRaster = class(TCoreClassObject)
  private
    FDrawEngineMap: TCoreClassObject;

    FSerializedEngine: TRasterSerialized;
    FMemorySerializedPosition: Int64;
    FActivted: Boolean;
    FActiveTimeTick: TTimeTick;

    FFreeBits: Boolean;
    FBits: PRColorArray;
    FWidth, FHeight: Integer;
    FDrawMode: TDrawMode;
    FCombineMode: TCombineMode;

    FVertex: TRasterVertex;
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

    FExtra: THashStringList;

    function GetExtra: THashStringList;
    function GetVertex: TRasterVertex;

    function GetFont: TFontRaster;
    procedure SetFont(f: TFontRaster); overload;

    function GetAggImage: TMemoryRaster_AggImage;
    function GetAgg: TMemoryRaster_Agg2D;

    function GetBits: PRColorArray;

    function GetPixel(const X, Y: Integer): TRColor;
    procedure SetPixel(const X, Y: Integer; const Value: TRColor);

    function GetFastPixel(const X, Y: Integer): TRColor;
    procedure SetFastPixel(const X, Y: Integer; const Value: TRColor);

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

    function GetPixelLinearMetric(const X, Y: TGeoFloat): TRColor;
    function GetPixelLinear(const X, Y: Integer): TRColor;

    function GetPixelYIQ(const X, Y: Integer): TYIQ;
    procedure SetPixelYIQ(const X, Y: Integer; const Value: TYIQ);
    function GetPixelHSI(const X, Y: Integer): THSI;
    procedure SetPixelHSI(const X, Y: Integer; const Value: THSI);
    function GetPixelCMYK(const X, Y: Integer): TCMYK;
    procedure SetPixelCMYK(const X, Y: Integer; const Value: TCMYK);
  public
    // global: parallel Morphomatics
    class var Parallel: Boolean;
  public
    // local: parallel Morphomatics
    LocalParallel: Boolean;

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
    procedure SetWorkMemory(Forever: Boolean; WorkMemory: Pointer; NewWidth, NewHeight: Integer); overload;
    procedure SetWorkMemory(WorkMemory: Pointer; NewWidth, NewHeight: Integer); overload;
    procedure SetWorkMemory(Forever: Boolean; raster: TMemoryRaster); overload;
    procedure SetWorkMemory(raster: TMemoryRaster); overload;
    function IsMemoryMap: Boolean;

    { triangle vertex map }
    procedure OpenVertex;
    procedure CloseVertex;
    property Vertex: TRasterVertex read GetVertex;

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
    procedure NoUsage; virtual;
    procedure Update;
    procedure DiscardMemory;
    procedure SwapInstance(dest: TMemoryRaster);
    function BitsSame(sour: TMemoryRaster): Boolean;
    procedure Reset; virtual;
    function Clone: TMemoryRaster; virtual;
    procedure Assign(sour: TMemoryRaster); overload; virtual;
    procedure Assign(sour: TMorphologyBinaryzation); overload; virtual;
    procedure Assign(sour: TMorphomatics); overload; virtual;
    procedure Clear; overload;
    procedure Clear(FillColor: TRColor); overload; virtual;
    function MemorySize: Integer;
    function GetMD5: TMD5;
    function GetCRC32: Cardinal;
    procedure SetSize(NewWidth, NewHeight: Integer); overload; virtual;
    procedure SetSize(NewWidth, NewHeight: Integer; const ClearColor: TRColor); overload; virtual;
    procedure SetSizeF(NewWidth, NewHeight: TGeoFloat; const ClearColor: TRColor); overload;
    procedure SetSizeF(NewWidth, NewHeight: TGeoFloat); overload;
    procedure SetSizeR(R: TRectV2; const ClearColor: TRColor); overload;
    procedure SetSizeR(R: TRectV2); overload;
    procedure SetSizeR(R: TRect; const ClearColor: TRColor); overload;
    procedure SetSizeR(R: TRect); overload;
    function SizeOfPoint: TPoint;
    function SizeOf2DPoint: TVec2;
    function Size2D: TVec2;
    function Size0: TVec2;
    function Empty: Boolean;
    property IsEmpty: Boolean read Empty;
    function BoundsRect: TRect;
    function BoundsRect0: TRect;
    function BoundsRectV2: TRectV2;
    function BoundsRectV20: TRectV2;
    function BoundsV2Rect4: TV2Rect4;
    function BoundsV2Rect40: TV2Rect4;
    function Centroid: TVec2;
    function Centre: TVec2;
    function InHere(const X, Y: Integer): Boolean;

    { pixel operation }
    procedure FlipHorz;
    procedure FlipVert;
    procedure Rotate90;
    procedure Rotate180;
    procedure Rotate270;
    function Rotate(dest: TMemoryRaster; Angle: TGeoFloat; Edge: Integer): TV2R4; overload;
    function Rotate(Angle: TGeoFloat; Edge: Integer; BackgroundColor: TRColor): TV2R4; overload;
    function Rotate(dest: TMemoryRaster; Axis: TVec2; Angle: TGeoFloat; Edge: Integer): TV2R4; overload;
    function Rotate(Axis: TVec2; Angle: TGeoFloat; Edge: Integer; BackgroundColor: TRColor): TV2R4; overload;
    procedure CalibrateRotate_LineDistance(BackgroundColor: TRColor);
    procedure CalibrateRotate_LineMatched(BackgroundColor: TRColor);
    procedure CalibrateRotate_AVG(BackgroundColor: TRColor);
    procedure CalibrateRotate(BackgroundColor: TRColor); overload;
    procedure CalibrateRotate; overload;
    procedure NonlinearZoomLine(const Source, dest: TMemoryRaster; const pass: Integer);
    procedure NonlinearZoomFrom(const Source: TMemoryRaster; const NewWidth, NewHeight: Integer);
    procedure NonlinearZoom(const NewWidth, NewHeight: Integer);
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
    procedure Scale(K: TGeoFloat);
    procedure FastBlurScale(K: TGeoFloat);
    procedure GaussianBlurScale(K: TGeoFloat);
    procedure NonlinearScale(K: TGeoFloat);
    procedure FitScale(NewWidth, NewHeight: TGeoFloat); overload;
    procedure FitScale(R: TRectV2); overload;
    function FitScaleAsNew(NewWidth, NewHeight: TGeoFloat): TMemoryRaster; overload;
    function FitScaleAsNew(R: TRectV2): TMemoryRaster; overload;
    function NonlinearFitScaleAsNew(NewWidth, NewHeight: TGeoFloat): TMemoryRaster; overload;
    function NonlinearFitScaleAsNew(R: TRectV2): TMemoryRaster; overload;
    procedure SigmaGaussian(const SIGMA: TGeoFloat; const SigmaGaussianKernelFactor: Integer); overload;
    procedure SigmaGaussian(const SIGMA: TGeoFloat); overload;
    procedure SigmaGaussian(parallel_: Boolean; const SIGMA: TGeoFloat; const SigmaGaussianKernelFactor: Integer); overload;
    procedure SigmaGaussian(parallel_: Boolean; const SIGMA: TGeoFloat); overload;
    function FormatAsBGRA: TMemoryRaster;
    procedure FormatBGRA;
    function BuildRGB(cSwapBR: Boolean): PRGBArray;
    procedure InputRGB(var buff; W, H: Integer; cSwapBR: Boolean);
    procedure OutputRGB(var buff; cSwapBR: Boolean);
    function EncryptGrayscale(): PByteBuffer;
    function EncryptColor255(): PByteBuffer;
    function EncryptColor65535(): PWordBuffer;
    procedure DecryptGrayscale(Width_, Height_: Integer; buffer: PByteBuffer);
    procedure DecryptColor255(Width_, Height_: Integer; buffer: PByteBuffer);
    procedure DecryptColor65535(Width_, Height_: Integer; buffer: PWordBuffer);
    procedure ColorReplace(const old_c, new_c: TRColor);
    procedure ColorTransparent(c_: TRColor);
    procedure ColorBlend(C: TRColor);
    procedure Grayscale;
    procedure Gradient(level: Byte);
    procedure ExtractGray(var output: TByteRaster);
    procedure ExtractRed(var output: TByteRaster);
    procedure ExtractGreen(var output: TByteRaster);
    procedure ExtractBlue(var output: TByteRaster);
    procedure ExtractAlpha(var output: TByteRaster);
    function ComputeAreaScaleSpace(clipArea: TRectV2; SS_width, SS_height: TGeoFloat): TRectV2; overload;
    function ComputeAreaScaleSpace(clipArea: TRect; SS_width, SS_height: Integer): TRect; overload;
    function BuildAreaOffsetScaleSpace(clipArea: TRectV2; SS_width, SS_height: Integer): TMemoryRaster; overload;
    function BuildAreaOffsetScaleSpace(clipArea: TRect; SS_width, SS_height: Integer): TMemoryRaster; overload;
    function BuildAreaCopyAs(clipArea: TRectV2): TMemoryRaster; overload;
    function BuildAreaCopyAs(clipArea: TRect): TMemoryRaster; overload;
    function FastAreaCopyAs(X1, Y1, X2, Y2: TGeoInt): TMemoryRaster;
    procedure FastAreaCopyFrom(Source: TMemoryRaster; DestX, DestY: Integer);
    function ExistsColor(C: TRColor): Boolean;
    function FindFirstColor(C: TRColor): TPoint;
    function FindLastColor(C: TRColor): TPoint;
    function FindNearColor(C: TRColor; PT: TVec2): TPoint;
    function ColorBoundsRectV2(C: TRColor): TRectV2;
    function ColorBoundsRect(C: TRColor): TRect;
    function ConvexHull(C: TRColor): TVec2List;
    function NoneColorBoundsRectV2(C: TRColor): TRectV2;
    function NoneColorBoundsRect(C: TRColor): TRect;
    procedure BlendColor(bk: TRColor);
    procedure BlendBlack();
    procedure Black();

    { shape support }
    procedure Line(X1, Y1, X2, Y2: Integer; Color: TRColor; L: Boolean); virtual;                              // L = draw closed pixel
    procedure LineF(X1, Y1, X2, Y2: TGeoFloat; Color: TRColor; L: Boolean); overload;                          // L = draw closed pixel
    procedure LineF(p1, p2: TVec2; Color: TRColor; L: Boolean); overload;                                      // L = draw closed pixel
    procedure LineF(p1, p2: TVec2; Color: TRColor; L: Boolean; LineDist: TGeoFloat; Cross: Boolean); overload; // L = draw closed pixel
    procedure FillRect(X1, Y1, X2, Y2: Integer; Color: TRColor); overload;
    procedure FillRect(Dstx, Dsty, LineDist: Integer; Color: TRColor); overload;
    procedure FillRect(Dst: TVec2; LineDist: Integer; Color: TRColor); overload;
    procedure FillRect(R: TRect; Color: TRColor); overload;
    procedure FillRect(R: TRectV2; Color: TRColor); overload;
    procedure FillRect(R: TRectV2; Angle: TGeoFloat; Color: TRColor); overload;
    procedure DrawRect(R: TRect; Color: TRColor); overload;
    procedure DrawRect(R: TRectV2; Color: TRColor); overload;
    procedure DrawRect(R: TV2Rect4; Color: TRColor); overload;
    procedure DrawRect(R: TRectV2; Angle: TGeoFloat; Color: TRColor); overload;
    procedure DrawTriangle(tri: TTriangle; Transform: Boolean; Color: TRColor; Cross: Boolean);
    procedure DrawFlatCross(Dst: TVec2; LineDist: TGeoFloat; Color: TRColor);
    procedure DrawCross(Dstx, Dsty, LineDist: Integer; Color: TRColor); overload;
    procedure DrawCrossF(Dstx, Dsty, LineDist: TGeoFloat; Color: TRColor); overload;
    procedure DrawCrossF(Dst: TVec2; LineDist: TGeoFloat; Color: TRColor); overload;
    procedure DrawCrossF(Polygon: TVec2List; LineDist: TGeoFloat; Color: TRColor); overload;
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
    procedure DrawPolygon(Polygon: T2DPolygonGraph; SurroundColor, CollapseColor: TRColor); overload;
    procedure DrawPolygonCross(Polygon: T2DPolygonGraph; LineDist: TGeoFloat; SurroundColor, CollapseColor: TRColor);
    procedure DrawPolygonLine(Polygon: TLines; Color: TRColor; wasClose: Boolean); overload;
    procedure DrawPolygon(Polygon: TDeflectionPolygon; ExpandDist: TGeoFloat; Color: TRColor); overload;

    { pixel border }
    function PixelAtNoneBGBorder(const X, Y: Integer; const BGColor, BorderColor: TRColor; const halfBorderSize: Integer; var detectColor: TRColor): Boolean; overload;
    function PixelAtNoneBGBorder(const X, Y: Integer; const BGColor: TRColor; const halfBorderSize: Integer; var detectColor: TRColor): Boolean; overload;
    procedure FillNoneBGColorBorder(parallel_: Boolean; BGColor, BorderColor: TRColor; BorderSize: Integer); overload;
    procedure FillNoneBGColorBorder(BGColor, BorderColor: TRColor; BorderSize: Integer); overload;
    procedure FillNoneBGColorAlphaBorder(parallel_: Boolean; BGColor, BorderColor: TRColor; BorderSize: Integer; output: TMemoryRaster); overload;
    procedure FillNoneBGColorAlphaBorder(BGColor, BorderColor: TRColor; BorderSize: Integer; output: TMemoryRaster); overload;

    { rasterization text support }
    function TextSize(Text: SystemString; siz: TGeoFloat): TVec2;
    procedure DrawText(Text: SystemString; X, Y: TGeoFloat; RotateVec: TVec2; Angle, alpha, siz: TGeoFloat; TextColor: TRColor); overload;
    procedure DrawText(Text: SystemString; X, Y: TGeoFloat; siz: TGeoFloat; TextColor: TRColor); overload;
    procedure DrawText(Text: SystemString; X, Y: TGeoFloat; RotateVec: TVec2; Angle, alpha, siz: TGeoFloat; TextColor: TRColor; var DrawCoordinate: TArrayV2R4); overload;
    procedure DrawText(Text: SystemString; X, Y: TGeoFloat; siz: TGeoFloat; TextColor: TRColor; var DrawCoordinate: TArrayV2R4); overload;
    function ComputeDrawTextCoordinate(Text: SystemString; X, Y: TGeoFloat; RotateVec: TVec2; Angle, siz: TGeoFloat; var DrawCoordinate, BoundBoxCoordinate: TArrayV2R4): TVec2;
    // compute text bounds size
    function ComputeTextSize(Text: SystemString; RotateVec: TVec2; Angle, siz: TGeoFloat): TVec2;
    // compute text ConvexHull
    function ComputeTextConvexHull(Text: SystemString; X, Y: TGeoFloat; RotateVec: TVec2; Angle, siz: TGeoFloat): TArrayVec2;

    { zDrawEngine support }
    function GetDrawEngineMap: TCoreClassObject;
    property DrawEngineMap: TCoreClassObject read GetDrawEngineMap;

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

    { Morphology }
    function BuildMorphologySegmentation(OnGetPixelSegClassify: TOnGetPixelSegClassify): TMorphologySegmentation; overload;
    function BuildMorphologySegmentation(): TMorphologySegmentation; overload;
    procedure BuildMorphomaticsTo(MorphPix_: TMorphologyPixel; output_: TMorphomatics);
    function BuildMorphomatics(MorphPix_: TMorphologyPixel): TMorphomatics;
    procedure BuildApproximateMorphomaticsTo(ApproximateColor_: TRColor; output_: TMorphomatics);
    function BuildApproximateMorphomatics(ApproximateColor_: TRColor): TMorphomatics;
    procedure DrawMorphomatics(MorphPix_: TMorphologyPixel; Morph: TMorphomatics); overload;
    procedure DrawMorphomatics(Morph: TMorphomatics); overload;
    procedure DrawBinaryzation(Morph: TMorphologyBinaryzation); overload;
    procedure DrawBinaryzation(MorphPix_: TMorphologyPixel; Morph: TMorphologyBinaryzation); overload;
    function BuildHistogram(MorphPix_: TMorphologyPixel; Height_: Integer; hColor: TRColor): TMemoryRaster;

    { load stream format }
    class function CanLoadStream(stream: TCoreClassStream): Boolean; virtual;
    procedure LoadFromBmpStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream); virtual;

    { save stream format }
    procedure SaveToStream(stream: TCoreClassStream; RasterSave_: TRasterSaveFormat); overload; // selected format
    procedure SaveToStream(stream: TCoreClassStream); overload; virtual;                        // published,32bit bitmap
    procedure SaveToBmp32Stream(stream: TCoreClassStream);                                      // published,32bit bitmap,include alpha
    procedure SaveToBmp24Stream(stream: TCoreClassStream);                                      // published,24bit bitmap,no alpha
    procedure SaveToZLibCompressStream(stream: TCoreClassStream);                               // custom,32bit include alpha
    procedure SaveToDeflateCompressStream(stream: TCoreClassStream);                            // custom,32bit include alpha
    procedure SaveToBRRCCompressStream(stream: TCoreClassStream);                               // custom,32bit include alpha
    procedure SaveToJpegLS1Stream(stream: TCoreClassStream);                                    // published,jls8bit
    procedure SaveToJpegLS3Stream(stream: TCoreClassStream);                                    // published,jls24bit
    procedure SaveToYV12Stream(stream: TCoreClassStream);                                       // custom,no alpha
    procedure SaveToFastYV12Stream(stream: TCoreClassStream);                                   // custom,no alpha
    procedure SaveToHalfYUVStream(stream: TCoreClassStream);                                    // custom,no alpha
    procedure SaveToFastHalfYUVStream(stream: TCoreClassStream);                                // custom,no alpha
    procedure SaveToQuartYUVStream(stream: TCoreClassStream);                                   // custom,no alpha
    procedure SaveToFastQuartYUVStream(stream: TCoreClassStream);                               // custom,no alpha
    procedure SaveToJpegYCbCrAStream(stream: TCoreClassStream; Quality: TJpegQuality);          // custom,32bit YCbCrA
    procedure SaveToJpegYCbCrStream(stream: TCoreClassStream; Quality: TJpegQuality);           // published,24bit YCbCr
    procedure SaveToJpegCMYKStream(stream: TCoreClassStream; Quality: TJpegQuality);            // custom,24bit CMYK
    procedure SaveToJpegGrayStream(stream: TCoreClassStream; Quality: TJpegQuality);            // published,8bit grayscale
    procedure SaveToJpegGrayAStream(stream: TCoreClassStream; Quality: TJpegQuality);           // custom,16bit grayscale+alpha
    procedure SaveToGrayscaleStream(stream: TCoreClassStream);                                  // custom grayscale,no alpha
    procedure SaveToColor255Stream(stream: TCoreClassStream);                                   // custom color 255,no alpha
    procedure SaveToColor65535Stream(stream: TCoreClassStream);                                 // custom color 65535,no alpha
    // png support
    procedure SaveToPNGStream(stream: TCoreClassStream); // published, Portable Network Graphic, automated detect and save as gray,rgb24,rgba32 format

    { load file format }
    class function CanLoadFile(fn: SystemString): Boolean;
    procedure LoadFromFile(fn: SystemString); virtual;

    { save file format }
    procedure SaveToBmp32File(fn: SystemString);                             // published,32bit bitmap,include alpha
    procedure SaveToBmp24File(fn: SystemString);                             // published,24bit bitmap,no alpha
    procedure SaveToFile(fn: SystemString);                                  // save format from ext name .jpg .jpeg .png .bmp .yv12 .jls .hyuv .qyuv .zlib_bmp .deflate_bmp .BRRC_bmp .gray .grayscale .255 .256 .64K
    procedure SaveToZLibCompressFile(fn: SystemString);                      // custom,32bit include alpha
    procedure SaveToDeflateCompressFile(fn: SystemString);                   // custom,32bit include alpha
    procedure SaveToBRRCCompressFile(fn: SystemString);                      // custom,32bit include alpha
    procedure SaveToJpegLS1File(fn: SystemString);                           // published,jls8bit
    procedure SaveToJpegLS3File(fn: SystemString);                           // published,jls24bit
    procedure SaveToYV12File(fn: SystemString);                              // custom,no alpha
    procedure SaveToFastYV12File(fn: SystemString);                          // custom,no alpha
    procedure SaveToHalfYUVFile(fn: SystemString);                           // custom,no alpha
    procedure SaveToFastHalfYUVFile(fn: SystemString);                       // custom,no alpha
    procedure SaveToQuartYUVFile(fn: SystemString);                          // custom,no alpha
    procedure SaveToFastQuartYUVFile(fn: SystemString);                      // custom,no alpha
    procedure SaveToJpegYCbCrAFile(fn: SystemString; Quality: TJpegQuality); // custom,32bit YCbCrA
    procedure SaveToJpegYCbCrFile(fn: SystemString; Quality: TJpegQuality);  // published,24bit YCbCr
    procedure SaveToJpegCMYKFile(fn: SystemString; Quality: TJpegQuality);   // custom,24bit CMYK
    procedure SaveToJpegGrayFile(fn: SystemString; Quality: TJpegQuality);   // published,8bit grayscale
    procedure SaveToJpegGrayAFile(fn: SystemString; Quality: TJpegQuality);  // custom,8bit grayscale + 8bit alpha
    procedure SaveToGrayscaleFile(fn: SystemString);                         // custom grapscale,no alpha
    procedure SaveToColor255File(fn: SystemString);                          // custom color 255,no alpha
    procedure SaveToColor65535File(fn: SystemString);                        // custom color 65535,no alpha
    // published, Portable Network Graphic, automated detect and save as gray,rgb24,rgba32 format
    procedure SaveToPNGFile(fn: SystemString);

    { Rasterization pixel }
    property Pixel[const X, Y: Integer]: TRColor read GetPixel write SetPixel; default;
    property FastPixel[const X, Y: Integer]: TRColor read GetFastPixel write SetFastPixel;
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
    property PixelLinearMetric[const X, Y: TGeoFloat]: TRColor read GetPixelLinearMetric;
    property PixelLinear[const X, Y: Integer]: TRColor read GetPixelLinear;
    property PixelYIQ[const X, Y: Integer]: TYIQ read GetPixelYIQ write SetPixelYIQ;
    property PixelHSI[const X, Y: Integer]: THSI read GetPixelHSI write SetPixelHSI;
    property PixelCMYK[const X, Y: Integer]: TCMYK read GetPixelCMYK write SetPixelCMYK;
    property ScanLine[Y: Integer]: PRColorArray read GetScanLine;
    property Bits: PRColorArray read GetBits;
    property DirectBits: PRColorArray read FBits;
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
    property Extra: THashStringList read GetExtra;
  end;

  TMemoryRasterClass = class of TMemoryRaster;

  TMemoryRasterArray = array of TMemoryRaster;
  TRasterArray = TMemoryRasterArray;
  TMemoryRaster2DArray = array of TMemoryRasterArray;
  TRaster2DArray = TMemoryRaster2DArray;
  TRasterMatrix = TMemoryRaster2DArray;
  TMemoryRaster2DMatrix = class;
  TMemoryRasterList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TMemoryRaster>;

  TMemoryRasterList = class(TMemoryRasterList_Decl)
  private
    FCritical: TCritical;
  public
    AutoFreeRaster: Boolean;
    UserToken: U_String;
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure UnLock;
    procedure Remove(obj: TMemoryRaster);
    procedure Delete(index: Integer);
    procedure Clear;
    procedure AddRasterList(L_: TMemoryRasterList);
    procedure AddRaster2DMatrix(M_: TMemoryRaster2DMatrix);

    function BuildArray: TMemoryRasterArray;
    procedure Clean;
  end;

  TMemoryRaster2DMatrix_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TMemoryRasterList>;

  TMemoryRaster2DMatrix = class(TMemoryRaster2DMatrix_Decl)
  private
    FCritical: TCritical;
  public
    AutoFreeRaster: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure UnLock;
    procedure Remove(obj: TMemoryRasterList);
    procedure Delete(index: Integer);
    procedure Clear;
    procedure AddRaster2DMatrix(M_: TMemoryRaster2DMatrix);

    function BuildArray: TMemoryRaster2DArray;
    procedure Clean;
  end;

  TRasterList = TMemoryRasterList;
  TRaster2DMatrix = TMemoryRaster2DMatrix;

  TByteRasterList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TByteRaster>;

  TByteRasterList = class(TByteRasterList_Decl)
  public
    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);
  end;

{$ENDREGION 'MemoryRaster'}
{$REGION 'Serialized'}

  TRasterSerialized = class
  protected
    FStream: TCoreClassStream;
    FAutoFreeStream: Boolean;
    FCritical: TCritical;
    FWriteHistory, FReadHistory: TMemoryRasterList;
    FEnabledWriteHistory, FEnabledReadHistory: Boolean;
  public
    constructor Create(stream_: TCoreClassStream);
    destructor Destroy; override;

    function Write(R: TMemoryRaster): Int64;
    function read(R: TMemoryRaster): Int64;
    procedure Remove(R: TMemoryRaster);
    procedure ClearHistory;

    property AutoFreeStream: Boolean read FAutoFreeStream write FAutoFreeStream;
    property stream: TCoreClassStream read FStream;
    property Critical: TCritical read FCritical;
    function StreamSize: Int64;
    function StreamFile: U_String;

    property WriteHistory: TMemoryRasterList read FWriteHistory;
    property ReadHistory: TMemoryRasterList read FReadHistory;
    property EnabledWriteHistory: Boolean read FEnabledWriteHistory write FEnabledWriteHistory;
    property EnabledReadHistory: Boolean read FEnabledReadHistory write FEnabledReadHistory;
  end;

  TRasterSerializedPool_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TRasterSerialized>;
  TRasterSerializedPool = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<TRasterSerializedPool_Decl>;

{$ENDREGION 'Serialized'}
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

    procedure FillLinearGradient(X1, Y1, X2, Y2: Double; c1, c2: TRColor; Profile: Double = 1);
    procedure LineLinearGradient(X1, Y1, X2, Y2: Double; c1, c2: TRColor; Profile: Double = 1);

    procedure FillRadialGradient(X, Y, R: Double; c1, c2: TRColor; Profile: Double = 1); overload;
    procedure LineRadialGradient(X, Y, R: Double; c1, c2: TRColor; Profile: Double = 1); overload;

    procedure FillRadialGradient(X, Y, R: Double; c1, c2, c3: TRColor); overload;
    procedure LineRadialGradient(X, Y, R: Double; c1, c2, c3: TRColor); overload;

    property ImageBlendColor: TRColor read GetImageBlendColor write SetImageBlendColor;
    property FillColor: TRColor read GetFillColor write SetFillColor;
    property LineColor: TRColor read GetLineColor write SetLineColor;
  end;

{$ENDREGION 'AGG'}
{$REGION 'Rasterization Vertex'}

  TRasterVertex = class(TCoreClassObject)
  private type
    { Setup interpolation constants for linearly varying vaues }
    TBilerpConsts = packed record
      A, B, C: Double;
    end;

    { fragment mode }
    TFragmentSampling = (fsSolid, fsNearest, fsLinear);
    TNearestWriteBuffer = array of Byte;
    TSamplerBlend = procedure(const Sender: TRasterVertex; const f, M: TRColor; var B: TRColor);
    TComputeSamplerColor = function(const Sender: TRasterVertex; const Sampler: TMemoryRaster; const X, Y: TGeoFloat): TRColor;
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
    procedure RasterizeTriangle(const FS: TFragmentSampling; const sc: TRColor; const tex: TMemoryRaster; const SamplerTri, RenderTri: TTriangle);
    // fragment
    procedure FillFragment(const FS: TFragmentSampling; const sc: TRColor; const tex: TMemoryRaster;
      const bitDst, j, start_x, frag_count: Int64; var attr_v, attr_u: TBilerpConsts);
    // nearest state buffer
    procedure NewWriterBuffer;
    // internal
    procedure internal_Draw(const RenderTri: TTriangle; const Sampler: TRColor); overload;
    procedure internal_Draw(const SamplerTri, RenderTri: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean); overload;
    procedure internal_Draw(const SamplerTri, RenderTri: TTriangle; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: TGeoFloat); overload;
  public
    // global: draw triangle edge
    class var DebugTriangle: Boolean;
    class var DebugTriangleColor: TRColor;
    // global: parallel vertex
    class var Parallel: Boolean;
    class var ParallelHeightTrigger, ParallelWidthTrigger: Int64;
  public
    LockSamplerCoord: Boolean;
    // local: parallel vertex
    LocalParallel: Boolean;
    // render window
    Window: TMemoryRaster;
    WindowSize: Int64;
    // user define
    UserData: Pointer;

    constructor Create(raster: TMemoryRaster);
    destructor Destroy; override;

    property NearestWriterID: Byte read FNearestWriterID;
    property NearestWriteBuffer: TNearestWriteBuffer read FNearestWriteBuffer;
    function BeginUpdate: Byte;
    procedure EndUpdate;

    (*
      input absolute coordiantes
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
    procedure FillPoly(const SamVec, RenVec: TVec2List; const SamCen, RenCen: TVec2; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: TGeoFloat); overload;
    procedure FillPoly(const SamVec, RenVec: TVec2List; const Sampler: TMemoryRaster; const bilinear_sampling: Boolean; const alpha: TGeoFloat); overload;
  end;

{$ENDREGION 'Rasterization Vertex'}
{$REGION 'TFontRaster'}

  TFontRasterList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TFontRaster>;

  TFontRaster = class(TCoreClassObject)
  public type
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

    TFontDrawState = record
      Owner: TFontRaster;
      DestColor: TRColor;
    end;

    PFontDrawState = ^TFontDrawState;

    TCharacterBoundBox = record
      Cache: Boolean;
      Box: TRect;
    end;

    TCharacterBoundBoxCache = array [0 .. MaxInt div SizeOf(TCharacterBoundBox) - 1] of TCharacterBoundBox;
    PCharacterBoundBoxCache = ^TCharacterBoundBoxCache;

{$IFDEF FPC}
    TFontRasterString = TUPascalString;
    TFontRasterChar = USystemChar;
    TFontRasterArrayString = TUArrayPascalString;
{$ELSE FPC}
    TFontRasterString = TPascalString;
    TFontRasterChar = SystemChar;
    TFontRasterArrayString = TArrayPascalString;
{$ENDIF FPC}

    TFontTextInfo = record
      Font: TFontRaster;
      Text: TFontRasterString;
      Box, PhysicsBox: TArrayV2R4;
    end;

    TFontTextInfos = array of TFontTextInfo;
  public const
    C_WordDefine: TFontCharDefine = (Activted: False; X: 0; Y: 0; W: 0; H: 0);
    C_MAXWORD = $FFFF;
  protected
    FOnlyInstance: Boolean;
    FFontTable: PFontTable;
    FCritical: TCritical;
    FCharacterBoundBoxCache: PCharacterBoundBoxCache;
    FFragRaster: TMemoryRasterArray;
    FBitRaster: PFontBitRaster;
    FFontSize: Integer;
    FActivtedWord: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FFontInfo: TFontRasterString;
  public
    X_Spacing, Y_Spacing: Integer;
    property FontInfo: TFontRasterString read FFontInfo;

    constructor Create; overload;
    constructor Create(ShareFont: TFontRaster); overload;
    destructor Destroy; override;

    // generate font
    function FragRasterIsNull(C: TFontRasterChar): Boolean;
    procedure Add(C: TFontRasterChar; raster: TMemoryRaster);
    procedure Remove(C: TFontRasterChar);
    procedure Clear;
    procedure ClearFragRaster;

    // build font
    procedure Build(fontInfo_: TFontRasterString; fontSiz: Integer; Status_: Boolean);

    // compute font
    function ValidChar(C: TFontRasterChar): Boolean;
    function GetBox(C: TFontRasterChar): TRect;
    function ComputeBoundBox(C: TFontRasterChar): TRect;
    function IsVisibled(C: TFontRasterChar): Boolean;
    property FontSize: Integer read FFontSize;
    property ActivtedWord: Integer read FActivtedWord;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property BitRaster: PFontBitRaster read FBitRaster;

    // store
    procedure Assign(Source: TFontRaster);
    procedure LoadFromStream(stream: TCoreClassStream);
    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromFile(filename: TPascalString);
    procedure SaveToFile(filename: TPascalString);

    // build font
    function BuildRaster(partitionLine: Boolean): TMemoryRaster;
    function BuildMorphomatics(): TMorphomatics;
    procedure ExportRaster(stream: TCoreClassStream; partitionLine: Boolean); overload;
    procedure ExportRaster(filename: TPascalString; partitionLine: Boolean); overload;

    // draw font
    function CharSize(const C: TFontRasterChar): TPoint;
    function TextSize(const S: TFontRasterString; charVec2List: TVec2List): TVec2; overload;
    function TextSize(const S: TFontRasterString): TVec2; overload;
    function TextWidth(const S: TFontRasterString): Word;
    function TextHeight(const S: TFontRasterString): Word;

    // compute coordinate
    function ComputeDrawBoundBox(Text: TFontRasterString; dstVec, Axis: TVec2; Angle, Scale: TGeoFloat; var DrawCoordinate, BoundBoxCoordinate: TArrayV2R4): TVec2; overload;
    function ComputeDrawBoundBox(Text: TFontRasterString; dstVec, Axis: TVec2; Angle, Scale: TGeoFloat; var DrawCoordinate: TArrayV2R4): TVec2; overload;
    function ComputeDrawCoordinate(Text: TFontRasterString; X, Y: TGeoFloat; RotateVec: TVec2; Angle, siz: TGeoFloat; var DrawCoordinate, BoundBoxCoordinate: TArrayV2R4): TVec2; overload;
    function ComputeDrawCoordinate(Text: TFontRasterString; X, Y: TGeoFloat; RotateVec: TVec2; Angle, siz: TGeoFloat; var DrawCoordinate: TArrayV2R4): TVec2; overload;

    // draw
    procedure DrawBit(fontRect: TRect; Dst: TMemoryRaster; DstRect: TV2Rect4; dstColor: TRColor; bilinear_sampling: Boolean; alpha: TGeoFloat);
    function Draw(Text: TFontRasterString; Dst: TMemoryRaster; dstVec: TVec2; dstColor: TRColor; bilinear_sampling: Boolean; alpha: TGeoFloat; Axis: TVec2; Angle, Scale: TGeoFloat; var DrawCoordinate: TArrayV2R4): TVec2; overload;
    function Draw(Text: TFontRasterString; Dst: TMemoryRaster; dstVec: TVec2; dstColor: TRColor; bilinear_sampling: Boolean; alpha: TGeoFloat; Axis: TVec2; Angle, Scale: TGeoFloat): TVec2; overload;
    procedure Draw(Text: TFontRasterString; Dst: TMemoryRaster; dstVec: TVec2; dstColor: TRColor); overload;

    // build text raster
    function BuildText(Text: TFontRasterString; RotateVec: TVec2; Angle, alpha, siz: TGeoFloat; TextColor: TRColor; var DrawCoordinate, BoundBoxCoordinate: TArrayV2R4): TMemoryRaster; overload;
    function BuildText(Edge: Integer; Text: TFontRasterString; RotateVec: TVec2; Angle, alpha, siz: TGeoFloat; TextColor: TRColor; var DrawCoordinate, BoundBoxCoordinate: TArrayV2R4): TMemoryRaster; overload;

    // build text from random font
    class function BuildTextRaster(Random_: TRandom; PhysicsBox_: Boolean; X_Spacing_, Y_Spacing_: Integer; Margin_: TGeoFloat;
      Fonts: TFontRasterList; FontSize_, Angle_: TGeoFloat; Text_: TFontRasterArrayString; var OutputInfo: TFontTextInfos): TMemoryRaster; overload;
    class function BuildTextRaster(Random_: TRandom; PhysicsBox_: Boolean; X_Spacing_, Y_Spacing_: Integer; Margin_: TGeoFloat;
      Fonts: TFontRasterList; FontSize_, Angle_: TGeoFloat; Text_: TFontRasterString; var OutputInfo: TFontTextInfos): TMemoryRaster; overload;
    class function BuildTextRaster(PhysicsBox_: Boolean; X_Spacing_, Y_Spacing_: Integer; Margin_: TGeoFloat;
      Fonts: TFontRasterList; FontSize_, Angle_: TGeoFloat; Text_: TFontRasterString; color_: TRColor; var OutputInfo: TFontTextInfos): TMemoryRaster; overload;

    // compute text bounds size
    function ComputeTextSize(Text: TFontRasterString; RotateVec: TVec2; Angle, siz: TGeoFloat): TVec2;
    // compute text ConvexHull
    function ComputeTextConvexHull(Text: TFontRasterString; X, Y: TGeoFloat; RotateVec: TVec2; Angle, siz: TGeoFloat): TArrayVec2;

    // advance draw
    procedure DrawText(Text: TFontRasterString; Dst: TMemoryRaster; X, Y: TGeoFloat; RotateVec: TVec2; Angle, alpha, siz: TGeoFloat; TextColor: TRColor); overload;
    procedure DrawText(Text: TFontRasterString; Dst: TMemoryRaster; X, Y: TGeoFloat; siz: TGeoFloat; TextColor: TRColor); overload;
    procedure DrawText(Text: TFontRasterString; Dst: TMemoryRaster; X, Y: TGeoFloat; RotateVec: TVec2; Angle, alpha, siz: TGeoFloat; TextColor: TRColor; var DrawCoordinate: TArrayV2R4); overload;
    procedure DrawText(Text: TFontRasterString; Dst: TMemoryRaster; X, Y: TGeoFloat; siz: TGeoFloat; TextColor: TRColor; var DrawCoordinate: TArrayV2R4); overload;
  end;

{$ENDREGION 'TFontRaster'}
{$REGION 'Morphomatics'}

  TMorphomaticsVector = array of TMorphomaticsValue;
  TMorphomaticsMatrix = array of TMorphomaticsVector;

  TMorphomaticsBits = array [0 .. MaxInt div SizeOf(TMorphomaticsValue) - 1] of TMorphomaticsValue;
  PMorphomaticsBits = ^TMorphomaticsBits;

  THistogramData = array [0 .. $FF] of Integer;

  TMorphFilter = (mfAverage, mfWeightedAVG, mfGeometricMean, mfMedian, mfMax, mfMin, mfMiddlePoint, mfTruncatedAVG,
    mfPrevitt, mfSobel, mfSharr, mfLaplace);

  TMorphomaticsDraw = {$IFDEF FPC}specialize {$ENDIF FPC}TLineProcessor<TMorphomaticsValue>;

  TMorphomaticsList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TMorphomatics>;

  TMorphomaticsList = class(TMorphomaticsList_Decl)
  public
    procedure Clean;
  end;

  TMorphMathList = TMorphomaticsList;
  TMorphomaticsPool = TMorphomaticsList;
  TMorphMathPool = TMorphomaticsList;

  TMorphomatics = class
  private
    FBits: PMorphomaticsBits;
    FWidth, FHeight: Integer;
    function GetPixel(const X, Y: Integer): TMorphomaticsValue;
    procedure SetPixel(const X, Y: Integer; const Value: TMorphomaticsValue);
    function GetPixelPtr(const X, Y: Integer): PMorphomaticsValue;
    function GetScanLine(const Y: Integer): PMorphomaticsBits;
    function FindMedian(const N: Integer; arry: TMorphomaticsVector): TMorphomaticsValue;
    procedure FastSort(var arry: TMorphomaticsVector);
  public
    // global: parallel Morphomatics
    class var Parallel: Boolean;
  public
    // local: parallel Morphomatics
    LocalParallel: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure FreeBits;
    procedure SetSize(Width_, Height_: Integer); overload;
    procedure SetSize(Width_, Height_: Integer; Value: TMorphomaticsValue); overload;
    procedure SetSizeF(const Width_, Height_: TGeoFloat); overload;
    procedure SetSizeF(const Width_, Height_: TGeoFloat; const Value: TMorphomaticsValue); overload;
    procedure SetSizeR(const R: TRectV2); overload;
    procedure SetSizeR(const R: TRectV2; const Value: TMorphomaticsValue); overload;
    procedure FillValue(Value: TMorphomaticsValue);
    procedure FillRandomValue();
    procedure FillValueFromPolygon(Polygon: TVec2List; InsideValue, OutsideValue: TMorphomaticsValue);

    function Clone: TMorphomatics;
    procedure Assign(sour: TMorphomatics);
    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);
    procedure SwapData(dest: TMorphomatics);
    procedure Scale(K: TGeoFloat);
    procedure FitScale(NewWidth, NewHeight: TGeoFloat);
    function FitScaleAsNew(NewWidth, NewHeight: TGeoFloat): TMorphomatics;
    procedure DrawTo(MorphPix_: TMorphologyPixel; dest: TMemoryRaster); overload;
    procedure DrawTo(dest: TMemoryRaster); overload;
    function BuildViewer(MorphPix_: TMorphologyPixel): TMemoryRaster; overload;
    function BuildViewer(): TMemoryRaster; overload;
    procedure BuildViewerFile(MorphPix_: TMorphologyPixel; filename_: SystemString); overload;
    procedure BuildViewerFile(filename_: SystemString); overload;
    procedure GetHistogramData(var H: THistogramData);
    procedure BuildHistogramTo(Height_: Integer; hColor: TRColor; output_: TMemoryRaster);
    function BuildHistogram(Height_: Integer; hColor: TRColor): TMemoryRaster;
    procedure DrawLine(const X1, Y1, X2, Y2: Integer; const PixelValue_: TMorphomaticsValue; const L: Boolean);
    procedure FillBox(const X1, Y1, X2, Y2: Integer; const PixelValue_: TMorphomaticsValue);
    function BuildHoughLine(const MaxAngle_, AlphaStep_, Treshold_: TGeoFloat; const BestLinesCount_: Integer): THoughLineArry;
    procedure ProjectionTo(SourMorph_, DestMorph_: TMorphologyPixel; Dst: TMorphomatics; sourRect, DestRect: TV2Rect4; bilinear_sampling: Boolean; alpha: TGeoFloat); overload;
    procedure ProjectionTo(SourMorph_, DestMorph_: TMorphologyPixel; Dst: TMorphomatics; sourRect, DestRect: TRectV2; bilinear_sampling: Boolean; alpha: TGeoFloat); overload;
    procedure Projection(SourMorph_, DestMorph_: TMorphologyPixel; DestRect: TV2Rect4; PixelValue_: TMorphomaticsValue); overload;
    procedure ProjectionTo(Dst: TMorphomatics; sourRect, DestRect: TV2Rect4; bilinear_sampling: Boolean; alpha: TGeoFloat); overload;
    procedure ProjectionTo(Dst: TMorphomatics; sourRect, DestRect: TRectV2; bilinear_sampling: Boolean; alpha: TGeoFloat); overload;
    procedure Projection(DestRect: TV2Rect4; PixelValue_: TMorphomaticsValue); overload;

    function Width0: Integer;
    function Height0: Integer;
    function SizeOfPoint: TPoint;
    function SizeOf2DPoint: TVec2;
    function Size2D: TVec2;
    function Size0: TVec2;
    function BoundsRect: TRect;
    function BoundsRect0: TRect;
    function BoundsRectV2: TRectV2;
    function BoundsRectV20: TRectV2;
    function BoundsV2Rect4: TV2Rect4;
    function BoundsV2Rect40: TV2Rect4;
    function Centroid: TVec2;
    function Centre: TVec2;
    function InHere(const X, Y: Integer): Boolean;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Pixel[const X, Y: Integer]: TMorphomaticsValue read GetPixel write SetPixel; default;
    property PixelPtr[const X, Y: Integer]: PMorphomaticsValue read GetPixelPtr;
    property ScanLine[const Y: Integer]: PMorphomaticsBits read GetScanLine;
    property Bits: PMorphomaticsBits read FBits;

    // filter
    procedure SigmaGaussian(const SIGMA: TGeoFloat; const SigmaGaussianKernelFactor: Integer);
    procedure Average(BoxW, BoxH: Integer);
    procedure WeightedAVG(BoxW, BoxH: Integer);
    procedure GeometricMean(BoxW, BoxH: Integer);
    procedure Median(BoxW, BoxH: Integer);
    procedure Maximum(BoxW, BoxH: Integer);
    procedure Minimum(BoxW, BoxH: Integer);
    procedure MiddlePoint(BoxW, BoxH: Integer);
    procedure TruncatedAVG(BoxW, BoxH, d: Integer);
    procedure Previtt(AdditiveToOriginal: Boolean);
    procedure Sobel(AdditiveToOriginal: Boolean);
    procedure Sharr(AdditiveToOriginal: Boolean);
    procedure Laplace(AdditiveToOriginal: Boolean);
    procedure ProcessFilter(filter: TMorphFilter);

    // classic morphomatics transform
    procedure Linear(K, B: TMorphomaticsValue);
    procedure Logarithms(C: TMorphomaticsValue);
    procedure Gamma(C, Gamma: TMorphomaticsValue);
    procedure HistogramEqualization();
    procedure Contrast(K: TMorphomaticsValue);
    procedure Gradient(level: Byte);
    procedure Clamp(MinV, MaxV: TMorphomaticsValue);
    procedure Invert;

    // symbol transform
    procedure ADD_(Morph: TMorphomatics); overload;
    procedure SUB_(Morph: TMorphomatics); overload;
    procedure MUL_(Morph: TMorphomatics); overload;
    procedure DIV_(Morph: TMorphomatics); overload;

    // phototype transform
    procedure ADD_(f: TMorphomaticsValue); overload;
    procedure SUB_(f: TMorphomaticsValue); overload;
    procedure MUL_(f: TMorphomaticsValue); overload;
    procedure DIV_(f: TMorphomaticsValue); overload;

    // Binaryzation symbol transform
    procedure ADD_(bin: TMorphologyBinaryzation; K: TMorphomaticsValue); overload;
    procedure SUB_(bin: TMorphologyBinaryzation; K: TMorphomaticsValue); overload;
    procedure MUL_(bin: TMorphologyBinaryzation; K: TMorphomaticsValue); overload;
    procedure DIV_(bin: TMorphologyBinaryzation; K: TMorphomaticsValue); overload;

    // grayscale morphology operation
    procedure Dilatation(ConvolutionKernel: TMorphologyBinaryzation; output: TMorphomatics); overload;
    procedure Erosion(ConvolutionKernel: TMorphologyBinaryzation; output: TMorphomatics); overload;
    procedure Opening(ConvolutionKernel: TMorphologyBinaryzation; output: TMorphomatics); overload;
    procedure Closing(ConvolutionKernel: TMorphologyBinaryzation; output: TMorphomatics); overload;
    procedure OpeningAndClosing(ConvolutionKernel: TMorphologyBinaryzation; output: TMorphomatics); overload;
    procedure ClosingAndOpening(ConvolutionKernel: TMorphologyBinaryzation; output: TMorphomatics); overload;

    procedure Dilatation(ConvolutionKernel: TMorphologyBinaryzation); overload;
    procedure Erosion(ConvolutionKernel: TMorphologyBinaryzation); overload;
    procedure Opening(ConvolutionKernel: TMorphologyBinaryzation); overload;
    procedure Closing(ConvolutionKernel: TMorphologyBinaryzation); overload;
    procedure OpeningAndClosing(ConvolutionKernel: TMorphologyBinaryzation); overload;
    procedure ClosingAndOpening(ConvolutionKernel: TMorphologyBinaryzation); overload;

    // quick morphology operation
    procedure Dilatation(const ConvolutionSizeX, ConvolutionSizeY: Integer; output: TMorphomatics); overload;
    procedure Erosion(const ConvolutionSizeX, ConvolutionSizeY: Integer; output: TMorphomatics); overload;
    procedure Opening(const ConvolutionSizeX, ConvolutionSizeY: Integer; output: TMorphomatics); overload;
    procedure Closing(const ConvolutionSizeX, ConvolutionSizeY: Integer; output: TMorphomatics); overload;
    procedure OpeningAndClosing(const ConvolutionSizeX, ConvolutionSizeY: Integer; output: TMorphomatics); overload;
    procedure ClosingAndOpening(const ConvolutionSizeX, ConvolutionSizeY: Integer; output: TMorphomatics); overload;

    procedure Dilatation(const ConvolutionSizeX, ConvolutionSizeY: Integer); overload;
    procedure Erosion(const ConvolutionSizeX, ConvolutionSizeY: Integer); overload;
    procedure Opening(const ConvolutionSizeX, ConvolutionSizeY: Integer); overload;
    procedure Closing(const ConvolutionSizeX, ConvolutionSizeY: Integer); overload;
    procedure OpeningAndClosing(const ConvolutionSizeX, ConvolutionSizeY: Integer); overload;
    procedure ClosingAndOpening(const ConvolutionSizeX, ConvolutionSizeY: Integer); overload;

    // Morphology Binaryzation
    function Binarization(Thresold: TMorphomaticsValue): TMorphologyBinaryzation;
    function Binarization_InRange(Min_, Max_: TMorphomaticsValue): TMorphologyBinaryzation;
    function Binarization_Bernsen(R: Integer; ContrastThresold: TMorphomaticsValue): TMorphologyBinaryzation;
    function Binarization_FloydSteinbergDithering: TMorphologyBinaryzation;
    {
      Thresholding using Otsu's method (which chooses the threshold to minimize the intraclass variance of the black and white pixels!).
      Functions returns calculated threshold level value [0..255].
      If BinarizeImage is True then the Image is automatically converted to binary using computed threshold level.
    }
    function Binarization_OTSU: TMorphologyBinaryzation;

    class procedure Test();
  end;
{$ENDREGION 'Morphomatics'}
{$REGION 'Binaryzation'}

  TBinaryzationValue = Boolean;
  PBinaryzationValue = ^TBinaryzationValue;

  TBinaryzationBits = array [0 .. MaxInt div SizeOf(TBinaryzationValue) - 1] of TBinaryzationValue;
  PBinaryzationBits = ^TBinaryzationBits;

  TBinaryzationOperation = (boNone,
    boDilatation, boErosion, boOpening, boClosing, boOpeningAndClosing, boClosingAndOpening,
    boOR, boAND, boXOR);

  TMorphologyBinaryzationDraw_ = {$IFDEF FPC}specialize {$ENDIF FPC}TLineProcessor<TBinaryzationValue>;
  TMorphologyBinaryzationDraw = class(TMorphologyBinaryzationDraw_);

  TMorphologyBinaryzationLineHitAnalysis_ = {$IFDEF FPC}specialize {$ENDIF FPC}TLineProcessor<TBinaryzationValue>;

  TMorphologyBinaryzationLineHitAnalysis = class(TMorphologyBinaryzationLineHitAnalysis_)
  private
    FPixelSum: NativeInt;
    FPixelValue: TBinaryzationValue;
  public
    function AnalysisBox(const X1, Y1, X2, Y2: NativeInt; const PixelValue_: TBinaryzationValue): NativeInt;
    function AnalysisLine(const X1, Y1, X2, Y2: NativeInt; const PixelValue_: TBinaryzationValue): NativeInt;
    procedure Process(const vp: TMorphologyBinaryzationLineHitAnalysis_.PT_; const v: TBinaryzationValue); override;
  end;

  TMorphologyBinaryzation_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TMorphologyBinaryzation>;

  TMorphologyBinaryzationList = class(TMorphomaticsList_Decl)
  public
    procedure Clean;
  end;

  TMorphBinList = TMorphologyBinaryzationList;
  TMorphologyBinaryzationPool = TMorphologyBinaryzationList;
  TMorphBinPool = TMorphologyBinaryzationList;

  TMorphologyBinaryzation = class
  private
    FBits: PBinaryzationBits;
    FWidth, FHeight: Integer;
    function GetPixel(const X, Y: Integer): TBinaryzationValue; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure SetPixel(const X, Y: Integer; const Value: TBinaryzationValue); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  public
    // global: parallel Binaryzation
    class var Parallel: Boolean;
  public
    // local: parallel Binaryzation
    LocalParallel: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure FreeBits;
    procedure SetSize(const Width_, Height_: Integer); overload;
    procedure SetSize(const Width_, Height_: Integer; const Value: TBinaryzationValue); overload;
    procedure SetSizeF(const Width_, Height_: TGeoFloat); overload;
    procedure SetSizeF(const Width_, Height_: TGeoFloat; const Value: TBinaryzationValue); overload;
    procedure SetSizeR(const R: TRectV2); overload;
    procedure SetSizeR(const R: TRectV2; const Value: TBinaryzationValue); overload;
    procedure SetConvolutionSize(const Width_, Height_: Integer; const Value: TBinaryzationValue);
    procedure FillValue(Value: TBinaryzationValue);
    procedure FillRandomValue();
    procedure FillValueFromPolygon(Polygon: TVec2List; InsideValue, OutsideValue: TBinaryzationValue);
    function ValueSum(Value: TBinaryzationValue): Integer;
    procedure DrawLine(const X1, Y1, X2, Y2: Integer; const PixelValue_: TBinaryzationValue; const L: Boolean);
    procedure FillBox(const X1, Y1, X2, Y2: Integer; const PixelValue_: TBinaryzationValue);
    function LineHitSum(const X1, Y1, X2, Y2: Integer; const PixelValue_: TBinaryzationValue; const L: Boolean): Integer;
    function BoxHitSum(const X1, Y1, X2, Y2: Integer; const PixelValue_: TBinaryzationValue): Integer; overload;
    function BoxHitSum(const R: TRect; const PixelValue_: TBinaryzationValue): Integer; overload;
    function BoxHitSum(const R: TRectV2; const PixelValue_: TBinaryzationValue): Integer; overload;
    function BuildHoughLine(const MaxAngle_, AlphaStep_: TGeoFloat; const BestLinesCount_: Integer): THoughLineArry;
    procedure ProjectionTo(SourMorph_, DestMorph_: TMorphologyPixel; Dst: TMorphologyBinaryzation; sourRect, DestRect: TV2Rect4; bilinear_sampling: Boolean; alpha: TGeoFloat); overload;
    procedure ProjectionTo(SourMorph_, DestMorph_: TMorphologyPixel; Dst: TMorphologyBinaryzation; sourRect, DestRect: TRectV2; bilinear_sampling: Boolean; alpha: TGeoFloat); overload;
    procedure Projection(SourMorph_, DestMorph_: TMorphologyPixel; DestRect: TV2Rect4; Value: TBinaryzationValue); overload;
    procedure ProjectionTo(Dst: TMorphologyBinaryzation; sourRect, DestRect: TV2Rect4; bilinear_sampling: Boolean; alpha: TGeoFloat); overload;
    procedure ProjectionTo(Dst: TMorphologyBinaryzation; sourRect, DestRect: TRectV2; bilinear_sampling: Boolean; alpha: TGeoFloat); overload;
    procedure Projection(DestRect: TV2Rect4; Value: TBinaryzationValue); overload;
    procedure IfThenSet(IfValue: TBinaryzationValue; dest: TMemoryRaster; destValue: TRColor);

    function Clone: TMorphologyBinaryzation;
    procedure Assign(sour: TMorphologyBinaryzation);
    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);
    procedure SwapData(dest: TMorphologyBinaryzation);
    procedure Invert;
    function BuildMorphologySegmentation(): TMorphologySegmentation;
    function BuildMorphomatics(): TMorphomatics;
    procedure DrawTo(raster: TMemoryRaster); overload;
    procedure DrawTo(MorphPix_: TMorphologyPixel; raster: TMemoryRaster); overload;
    function BuildViewer(): TMemoryRaster; overload;
    function BuildViewer(MorphPix_: TMorphologyPixel): TMemoryRaster; overload;
    procedure BuildViewerFile(filename_: SystemString); overload;
    procedure BuildViewerFile(MorphPix_: TMorphologyPixel; filename_: SystemString); overload;
    function ConvexHull(): TVec2List;
    function BoundsRectV2(const Value: TBinaryzationValue; var Sum_: Integer): TRectV2; overload;
    function BoundsRectV2(const Value: TBinaryzationValue): TRectV2; overload;
    function BoundsRect(const Value: TBinaryzationValue; var Sum_: Integer): TRect; overload;
    function BoundsRect(const Value: TBinaryzationValue): TRect; overload;
    function Width0: Integer;
    function Height0: Integer;
    function SizeOfPoint: TPoint;
    function SizeOf2DPoint: TVec2;
    function Size2D: TVec2;
    function Size0: TVec2;
    function BoundsRect: TRect; overload;
    function BoundsRect0: TRect;
    function BoundsRectV2: TRectV2; overload;
    function BoundsRectV20: TRectV2;
    function BoundsV2Rect4: TV2Rect4;
    function BoundsV2Rect40: TV2Rect4;
    function Centroid: TVec2;
    function Centre: TVec2;
    function InHere(const X, Y: Integer): Boolean;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Pixel[const X, Y: Integer]: TBinaryzationValue read GetPixel write SetPixel; default;
    property Bits: PBinaryzationBits read FBits;

    // convolution operation
    procedure Dilatation(ConvolutionKernel, output: TMorphologyBinaryzation); overload;
    procedure Erosion(ConvolutionKernel, output: TMorphologyBinaryzation); overload;
    procedure Opening(ConvolutionKernel, output: TMorphologyBinaryzation); overload;
    procedure Closing(ConvolutionKernel, output: TMorphologyBinaryzation); overload;
    procedure OpeningAndClosing(ConvolutionKernel, output: TMorphologyBinaryzation); overload;
    procedure ClosingAndOpening(ConvolutionKernel, output: TMorphologyBinaryzation); overload;
    procedure Skeleton(ConvolutionKernel, output: TMorphologyBinaryzation); overload;

    procedure Dilatation(ConvolutionKernel: TMorphologyBinaryzation); overload;
    procedure Erosion(ConvolutionKernel: TMorphologyBinaryzation); overload;
    procedure Opening(ConvolutionKernel: TMorphologyBinaryzation); overload;
    procedure Closing(ConvolutionKernel: TMorphologyBinaryzation); overload;
    procedure OpeningAndClosing(ConvolutionKernel: TMorphologyBinaryzation); overload;
    procedure ClosingAndOpening(ConvolutionKernel: TMorphologyBinaryzation); overload;
    procedure Skeleton(ConvolutionKernel: TMorphologyBinaryzation); overload;

    // quick morphology operation
    procedure Dilatation(const ConvolutionSizeX, ConvolutionSizeY: Integer; output: TMorphologyBinaryzation); overload;
    procedure Erosion(const ConvolutionSizeX, ConvolutionSizeY: Integer; output: TMorphologyBinaryzation); overload;
    procedure Opening(const ConvolutionSizeX, ConvolutionSizeY: Integer; output: TMorphologyBinaryzation); overload;
    procedure Closing(const ConvolutionSizeX, ConvolutionSizeY: Integer; output: TMorphologyBinaryzation); overload;
    procedure OpeningAndClosing(const ConvolutionSizeX, ConvolutionSizeY: Integer; output: TMorphologyBinaryzation); overload;
    procedure ClosingAndOpening(const ConvolutionSizeX, ConvolutionSizeY: Integer; output: TMorphologyBinaryzation); overload;
    procedure Skeleton(const ConvolutionSizeX, ConvolutionSizeY: Integer; output: TMorphologyBinaryzation); overload;

    procedure Dilatation(const ConvolutionSizeX, ConvolutionSizeY: Integer); overload;
    procedure Erosion(const ConvolutionSizeX, ConvolutionSizeY: Integer); overload;
    procedure Opening(const ConvolutionSizeX, ConvolutionSizeY: Integer); overload;
    procedure Closing(const ConvolutionSizeX, ConvolutionSizeY: Integer); overload;
    procedure OpeningAndClosing(const ConvolutionSizeX, ConvolutionSizeY: Integer); overload;
    procedure ClosingAndOpening(const ConvolutionSizeX, ConvolutionSizeY: Integer); overload;
    procedure Skeleton(const ConvolutionSizeX, ConvolutionSizeY: Integer); overload;

    // this Transformation is a symbol operation, not a convolution
    procedure OR_(Source, output: TMorphologyBinaryzation); overload;
    procedure AND_(Source, output: TMorphologyBinaryzation); overload;
    procedure XOR_(Source, output: TMorphologyBinaryzation); overload;
    procedure OR_(Source: TMorphologyBinaryzation); overload;
    procedure AND_(Source: TMorphologyBinaryzation); overload;
    procedure XOR_(Source: TMorphologyBinaryzation); overload;

    procedure Process(Operation_: TBinaryzationOperation; Data: TMorphologyBinaryzation);

    procedure Print;

    class procedure Test();
  end;
{$ENDREGION 'Binaryzation'}
{$REGION 'Segmentation'}

  PMorphologySegData = ^TMorphologySegData;

  TMorphologyGeoData = record
    X, Y: Integer;
    segPtr: PMorphologySegData;
  end;

  PMorphologyGeoData = ^TMorphologyGeoData;

  TSegmentationGeometry_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PMorphologyGeoData>;

  TSegmentationGeometry = class(TSegmentationGeometry_Decl)
  public
    constructor Create;
    destructor Destroy; override;
    procedure InsertGeo(index: Integer; X, Y: Integer; segPtr: PMorphologySegData);
    procedure AddGeo(X, Y: Integer; segPtr: PMorphologySegData);
    procedure Remove(p: PMorphologyGeoData);
    procedure Delete(index: TGeoInt);
    procedure Clear;
  end;

  TMorphologySegData = record
    Y, L, R: Integer;                            // (L,Y) (R,Y)
    LTop: PMorphologySegData;                    // connection to left top
    RTop: PMorphologySegData;                    // connection to right top
    LBot: PMorphologySegData;                    // connection to left bottom
    RBot: PMorphologySegData;                    // connection to right bottom
    Left: PMorphologySegData;                    // connection to left
    Right: PMorphologySegData;                   // connection to right
    GroupID: Integer;                            // Morphology group ID
    Classify: TMorphologyClassify;               // classify
    LGeometry, RGeometry: TSegmentationGeometry; // internal geometry
    index: Integer;                              // container index from TMorphologyPool_Decl
  end;

  TMorphologyPool_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PMorphologySegData>;

  TMorphologyPool = class(TMorphologyPool_Decl)
  private
    FBoundsCached: Boolean;
    FBoundsCache: TRectV2;
    FPixelSumCache: Integer;
    FClassify: TMorphologyClassify;
    FGroupID: Integer;
  public
    Owner: TMorphologySegmentation;
    constructor Create;
    procedure AddSeg(const buff: array of PMorphologySegData);
    procedure SortY;
    function BoundsRectV2(Cache: Boolean): TRectV2; overload;
    function BoundsRectV2: TRectV2; overload;
    function BoundsRect: TRect;
    function Centre: TVec2;
    function Left: Integer;
    function Top: Integer;
    function Width: Integer;
    function Height: Integer;
    function PixelSum: Integer;
    function Area: Integer;
    function BuildBinaryzation(): TMorphologyBinaryzation;
    procedure FillToBinaryzation(morphBin_: TMorphologyBinaryzation);
    procedure DrawTo(dest: TMemoryRaster; DataColor: TRColor);
    procedure ProjectionTo(Source, dest: TMemoryRaster);
    function Projection(Source: TMemoryRaster): TMemoryRaster;
    function BuildDatamap(backColor, DataColor: TRColor): TMemoryRaster;
    function BuildClipDatamap(backColor, DataColor: TRColor): TMemoryRaster;
    function BuildClipMap(Source: TMemoryRaster; backColor: TRColor): TMemoryRaster;
    function BuildVertex(): T2DPolygon;
    function BuildConvexHull(): T2DPolygon;
    function BuildLines(Reduction: TGeoFloat): TLinesList;
    function IsGroup(const X, Y: Integer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function IsEdge(const X, Y: Integer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ScanEdge(const Y, L, R: Integer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function BuildGeometry(Reduction: TGeoFloat): T2DPolygonGraph;
    function BuildConvolutionGeometry(Reduction: TGeoFloat; Operation_: TBinaryzationOperation; ConvolutionKernel: TMorphologyBinaryzation): T2DPolygonGraph; overload;
    function BuildConvolutionGeometry(Reduction: TGeoFloat): T2DPolygonGraph; overload;
    property Classify: TMorphologyClassify read FClassify;
    property GroupID: Integer read FGroupID;
  end;

  TMorphologyPoolList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TMorphologyPool>;

  TMorphologySegMap = array of array of PMorphologySegData;
  TMorphologySegClassifyMap = array of array of TMorphologyClassify;

  TConvolutionKernelProc = procedure(ConvolutionKernel: TMorphologyBinaryzation) of object;

  TMorphologySegmentation = class(TCoreClassObject)
  private
    FWidth, FHeight: Integer;
    FSegMap: TMorphologySegMap;
    FSource: TMorphologyPool_Decl;
    FMorphologyPoolList: TMorphologyPoolList;
    FOnGetPixelSegClassify: TOnGetPixelSegClassify;
    FOnGetMorphomaticsSegClassify: TOnGetMorphomaticsSegClassify;

    function NewMorphologySegData(X, Y: Integer; Classify: TMorphologyClassify): PMorphologySegData;
    function FindPool(p: PMorphologySegData): TMorphologyPool;
    function GetOrCreatePool(p: PMorphologySegData): TMorphologyPool;
    procedure AddToGroup(p: PMorphologySegData);

    function GetPools(X, Y: Integer): TMorphologyPool;
    function GetItems(index: Integer): TMorphologyPool;
    procedure ResetSource;
    procedure PrepareMap(Width_, Height_: Integer);
    procedure InternalFillMap(var classifyMap_: TMorphologySegClassifyMap; Width_, Height_: Integer);
    procedure ExtractSegLinkToGroup();
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear();

    // pixel segmentation
    property OnGetPixelSegClassify: TOnGetPixelSegClassify read FOnGetPixelSegClassify write FOnGetPixelSegClassify;
    function DoGetPixelSegClassify(X, Y: Integer; Color: TRColor): TMorphologyClassify; virtual;
    procedure BuildSegmentation(raster: TMemoryRaster); overload;

    // advanced pixel segmentation
    procedure BuildSegmentation(raster: TMemoryRaster;
      ConvolutionOperations: array of TBinaryzationOperation; ConvWidth, ConvHeight, MaxClassifyCount, MinGranularity: Integer); overload;

    // morphomatics segmentation
    property OnGetMorphomaticsSegClassify: TOnGetMorphomaticsSegClassify read FOnGetMorphomaticsSegClassify write FOnGetMorphomaticsSegClassify;
    function DoGetMorphomaticsSegClassify(X, Y: Integer; Morph: TMorphomaticsValue): TMorphologyClassify; virtual;
    procedure BuildSegmentation(Morph: TMorphomatics); overload;

    // binaryzation segmentation
    procedure BuildSegmentation(Binaryzation: TMorphologyBinaryzation); overload;

    // build morphology segmentation map
    procedure BuildSegmentation(var classifyMap_: TMorphologySegClassifyMap; Width_, Height_: Integer); overload;
    function GetClassifyMap: TMorphologySegClassifyMap;

    // build morphology segmentation link
    procedure UpdateMorphologyPool();

    // merge all overlop boundbox of Segmentation
    procedure MergeOverlapSegmentation();

    // remove noise
    function RemoveNoise(PixelNoiseThreshold: Integer): Boolean;

    // data support
    function Clone: TMorphologySegmentation;
    procedure Assign(sour: TMorphologySegmentation);
    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    function BuildBinaryzation(): TMorphologyBinaryzation;
    function Projection(Source: TMemoryRaster): TMemoryRaster;
    function BuildViewer(): TMemoryRaster;

    function Count: Integer;
    property PoolCount: Integer read Count;
    property Items[index: Integer]: TMorphologyPool read GetItems; default;
    property Pools[X, Y: Integer]: TMorphologyPool read GetPools;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    function Width0: Integer;
    function Height0: Integer;
    function SizeOfPoint: TPoint;
    function SizeOf2DPoint: TVec2;
    function Size2D: TVec2;
    function Size0: TVec2;
    function BoundsRect: TRect; overload;
    function BoundsRect0: TRect;
    function BoundsRectV2: TRectV2; overload;
    function BoundsRectV20: TRectV2;
    function BoundsV2Rect4: TV2Rect4;
    function BoundsV2Rect40: TV2Rect4;
    function Centroid: TVec2;
    function Centre: TVec2;
    function InHere(const X, Y: Integer): Boolean;

    // test operation
    class procedure Test(inputfile, outputfile: SystemString);
  end;

{$ENDREGION 'Segmentation'}
{$REGION 'RCLines Detector'}

  TMorphologyRCLineStyle = (lsRow, lsCol);

  TMorphologyRCLine = record
    Bp, Ep: TPoint;
    Style: TMorphologyRCLineStyle;
  end;

  PMorphologyRCLine = ^TMorphologyRCLine;

  TMorphologyRCLineList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC}TGenericsList<PMorphologyRCLine>;

  TMorphologyRCLines = class(TMorphologyRCLineList_Decl)
  public
    constructor Create;
    class function BuildLines(map: TMorphologyBinaryzation; MinLineLength: Integer): TMorphologyRCLines;
    class function BuildIntersectSegment(map: TMorphologyBinaryzation; MinLineLength: Integer): TMorphologyRCLines;
    destructor Destroy; override;
    procedure AddRCLine(Bx, By, Ex, Ey: Integer; Style: TMorphologyRCLineStyle);
    function SumLine(Style: TMorphologyRCLineStyle): Integer;
    function BuildFormulaBox(): TRectV2List;

    procedure Remove(p1, p2, p3, p4: PMorphologyRCLine); overload;
    procedure Remove(p: PMorphologyRCLine); overload;
    procedure Delete(index: Integer);
    procedure Clear;
  end;

{$ENDREGION 'RCLines Detector'}
{$REGION 'RasterizationIOProcessor'}

  TRaster_IO_Processor = class;

  // async IO input define
  TRaster_IO = class(TCoreClassInterfacedObject)
  public
    Owner: TRaster_IO_Processor;
    InputRaster: TMemoryRaster;
    OutputRaster: TMemoryRaster;
    IndexNumber: UInt64;

    constructor Create(Owner_: TRaster_IO_Processor); virtual;
    destructor Destroy; override;
    procedure ProcessBefore(UserData: Pointer); virtual;
    // if process result is true, append the to output
    function Process(UserData: Pointer): Boolean; virtual;
    procedure ProcessAfter(UserData: Pointer); virtual;
  end;

  TRaster_IO_Buffer = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TRaster_IO>;

  TRaster_IO_Class = class of TRaster_IO;

  // async IO processor
  TRaster_IO_Processor = class(TCoreClassInterfacedObject)
  protected
    FIO_Class: TRaster_IO_Class;
    FInputBuffer, FOutputBuffer: TRaster_IO_Buffer;
    FIOThreadRuning: TAtomBool;
    FParallelProcessor: Boolean;
    FIndexNumber: UInt64;
    procedure LockInputBuffer;
    procedure UnLockInputBuffer;
    procedure IOProcessorThreadRun(ThSender: TCompute);
  public
    constructor Create(IO_Class_: TRaster_IO_Class); virtual;
    destructor Destroy; override;

    procedure Clear;

    // input and process
    procedure InputPicture(filename: TPascalString); overload;
    procedure InputPicture(stream: TCoreClassStream); overload;
    procedure Input(raster: TMemoryRaster; RasterInstance_: Boolean);
    function InputCount: Integer;
    procedure Process(UserData: Pointer);
    function Finished: Boolean;
    procedure WaitProcessDone;

    // output
    function LockOutputBuffer: TRaster_IO_Buffer;
    procedure UnLockOutputBuffer(freeObj_: Boolean);

    // IO Class
    property IO_Class: TRaster_IO_Class read FIO_Class write FIO_Class;
    property ParallelProcessor: Boolean read FParallelProcessor write FParallelProcessor;
  end;
{$ENDREGION 'RasterizationIOProcessor'}
{$REGION 'RasterAPI'}


function Wait_SystemFont_Init: TFontRaster;

function ClampInt(const Value, IMin, IMax: Integer): Integer; inline;
function ClampByte3(const Value, IMin, IMax: Byte): Byte; inline;

function ClampByte(const Value: Cardinal): Byte; overload;
function ClampByte(const Value: Integer): Byte; overload;
function ClampByte(const Value: UInt64): Byte; overload;
function ClampByte(const Value: Int64): Byte; overload;
function RoundAsByte(const Value: Double): Byte;

procedure DisposeRasterArray(var arry: TMemoryRasterArray);

procedure BlendBlock(Dst: TMemoryRaster; DstRect: TRect; Src: TMemoryRaster; Srcx, Srcy: Integer; CombineOp: TDrawMode);
procedure BlockTransfer(Dst: TMemoryRaster; Dstx: Integer; Dsty: Integer; DstClip: TRect; Src: TMemoryRaster; SrcRect: TRect; CombineOp: TDrawMode);

procedure FillRasterColor(BitPtr: Pointer; Count: Cardinal; Value: TRasterColor);
procedure CopyRasterColor(const Source; var dest; Count: Cardinal);
function RandomRasterColor(rand: TRandom; const A_, Min_, Max_: Byte): TRColor; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RandomRasterColor(const A_, Min_, Max_: Byte): TRColor; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RandomRasterColor(const A: Byte): TRColor; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RandomRasterColor(): TRColor; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RandomRasterColorF(const minR, maxR, minG, maxG, minB, maxB, minA, maxA: TGeoFloat): TRColor; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RandomRasterColorF(rand: TRandom; minR, maxR, minG, maxG, minB, maxB, minA, maxA: TGeoFloat): TRColor; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RasterColor(const v: TVec4): TRColor; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RasterColor(const R, G, B, A: Byte): TRasterColor; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RasterColor(const R, G, B: Byte): TRasterColor; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RasterColorInv(const C: TRasterColor): TRasterColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RasterAlphaColor(const C: TRasterColor; const A: Byte): TRasterColor;
function RasterAlphaColorF(const C: TRasterColor; const A: TGeoFloat): TRasterColor;
function RasterColorF(const R, G, B, A: TGeoFloat): TRasterColor; overload;
function RasterColorF(const R, G, B: TGeoFloat): TRasterColor; overload;
procedure RasterColor2F(const C: TRasterColor; var R, G, B, A: TGeoFloat); overload;
procedure RasterColor2F(const C: TRasterColor; var R, G, B: TGeoFloat); overload;
function RasterColor2Vec4(const C: TRasterColor): TVec4;
function RasterColor2Vector4(const C: TRasterColor): TVector4;
function RasterColor2Vec3(const C: TRasterColor): TVec3;
function RasterColor2Vector3(const C: TRasterColor): TVector3;
function RasterColor2Gray(const C: TRasterColor): Byte; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RasterColor2GrayS(const C: TRasterColor): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RasterColor2GrayD(const C: TRasterColor): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function SetRasterColorAlpha(const C: TRasterColor; const A: Byte): TRColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure FillRColor(BitPtr: Pointer; Count: Cardinal; Value: TRColor);
procedure CopyRColor(const Source; var dest; Count: Cardinal);
function RandomRColor(rand: TRandom; const A_, Min_, Max_: Byte): TRColor; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RandomRColor(const A_, Min_, Max_: Byte): TRColor; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RandomRColor(const A: Byte): TRColor; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RandomRColor(): TRColor; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RandomRColorF(const minR, maxR, minG, maxG, minB, maxB, minA, maxA: TGeoFloat): TRColor; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RandomRColorF(rand: TRandom; minR, maxR, minG, maxG, minB, maxB, minA, maxA: TGeoFloat): TRColor; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RColor(const S: U_String): TRColor; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RColor(const v: TVec4): TRColor; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RColor(const R, G, B, A: Byte): TRColor; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RColor(const R, G, B: Byte): TRColor; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function RColorInv(const C: TRColor): TRColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RAlphaColor(const C: TRasterColor; const A: Byte): TRasterColor;
function RAlphaColorF(const C: TRasterColor; const A: TGeoFloat): TRasterColor;
function RColorF(const R, G, B, A: TGeoFloat): TRColor; overload;
function RColorF(const R, G, B: TGeoFloat): TRColor; overload;
procedure RColor2F(const C: TRColor; var R, G, B, A: TGeoFloat); overload;
procedure RColor2F(const C: TRColor; var R, G, B: TGeoFloat); overload;
function RColor2Vec4(const C: TRColor): TVec4;
function RColor2Vector4(const C: TRColor): TVector4;
function RColor2Vec3(const C: TRColor): TVec3;
function RColor2Vector3(const C: TRColor): TVector3;
function RColor2Gray(const C: TRColor): Byte; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RColor2GrayS(const C: TRColor): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RColor2GrayD(const C: TRColor): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function SetRColorAlpha(const C: TRasterColor; const A: Byte): TRColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RColorDistanceMax(c1, c2: TRColor): Byte; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RColorDistanceSum(c1, c2: TRColor): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RColorDistance(c1, c2: TRColor): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RColorDistanceByte(c1, c2: TRColor): Byte; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RColorGradient(C: TRColor; level: Byte): TRColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RColorGrayGradient(C: TRColor; level: Byte): Byte; {$IFDEF INLINE_ASM} inline; {$ENDIF}

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

function RColorToApproximateMorph(Color, ApproximateColor_: TRColor): TMorphomaticsValue; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RColorToMorph(Color: TRColor; MorphPix: TMorphologyPixel): TMorphomaticsValue; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure MorphToRColor(MorphPix: TMorphologyPixel; Value: TMorphomaticsValue; var Color: TRColor); {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure FillColorTable(const RBits, GBits, BBits: Byte; const DestCount: Integer; dest: PRColorArray);
function FindColorIndex(Color: TRColor; const DestCount: Integer; dest: PRColorArray): Integer;
function FindColor255Index(Color: TRColor): Integer;
function FindColor65535Index(Color: TRColor): Integer;

function AggColor(const Value: TRColor): TAggColorRgba8; {$IFDEF INLINE_ASM} inline; {$ENDIF}overload;
function AggColor(const R, G, B: TGeoFloat; const A: TGeoFloat = 1.0): TAggColorRgba8; {$IFDEF INLINE_ASM} inline; {$ENDIF}overload;
function AggColor(const Value: TAggColorRgba8): TRColor; {$IFDEF INLINE_ASM} inline; {$ENDIF}overload;

function ComputeSize(const MAX_Width, MAX_Height: Integer; var Width, Height: Integer): TGeoFloat;

procedure FastBlur(Source, dest: TMemoryRaster; radius: Double; const Bounds: TRect); overload;
procedure FastBlur(Source: TMemoryRaster; radius: Double; const Bounds: TRect); overload;
procedure GaussianBlur(Source, dest: TMemoryRaster; radius: Double; const Bounds: TRect); overload;
procedure GaussianBlur(Source: TMemoryRaster; radius: Double; const Bounds: TRect); overload;
procedure GrayscaleBlur(Source, dest: TMemoryRaster; radius: Double; const Bounds: TRect); overload;
procedure GrayscaleBlur(Source: TMemoryRaster; radius: Double; const Bounds: TRect); overload;

procedure Antialias32(const DestMR: TMemoryRaster; AXOrigin, AYOrigin, AXFinal, AYFinal: Integer); overload;
procedure Antialias32(const DestMR: TMemoryRaster; const Amount_: Integer); overload;
procedure Antialias32(const DestMR: TMemoryRaster); overload;
procedure HistogramEqualize(const mr: TMemoryRaster); overload;
procedure HistogramEqualize(const mr1, mr2: TMemoryRaster); overload;
procedure RemoveRedEyes(const mr: TMemoryRaster);
procedure Sepia32(const mr: TMemoryRaster; const Depth: Byte);
procedure Sharpen(const DestMR: TMemoryRaster; const SharpenMore: Boolean);
procedure AddColorNoise32(const Source: TMemoryRaster; const Amount_: Integer);
procedure AddMonoNoise32(const Source: TMemoryRaster; const Amount_: Integer);

type
  TDiagonalDirection = (ddLeftDiag, ddRightDiag);
procedure Diagonal(const Source, dest: TMemoryRaster; const backColor: TRColor; const Amount: Integer; const DiagDirection: TDiagonalDirection);

procedure GrayscaleToAlpha(Src: TMemoryRaster);
procedure AlphaToGrayscale(Src: TMemoryRaster);
procedure IntensityToAlpha(Src: TMemoryRaster);
procedure ReversalAlpha(Src: TMemoryRaster);
procedure RGBToGrayscale(Src: TMemoryRaster);

procedure FillBlackGrayBackgroundTexture(bk: TMemoryRaster; block_size: Integer; bkColor, color1, color2: TRColor); overload;
procedure FillBlackGrayBackgroundTexture(bk: TMemoryRaster; block_size: Integer); overload;

procedure ColorToTransparent(SrcColor: TRColor; Src, Dst: TMemoryRaster);

function BuildSequenceFrame(bmp32List: TCoreClassListForObj; Column: Integer; Transparent: Boolean): TSequenceMemoryRaster;
function GetSequenceFrameRect(bmp: TMemoryRaster; Total, Column, index: Integer): TRect;
procedure GetSequenceFrameOutput(bmp: TMemoryRaster; Total, Column, index: Integer; output: TMemoryRaster);

function AnalysisColors(mr: TMemoryRaster; ignoreColors: TRColors; MaxCount: Integer): TRColors;

function BlendReg(f, B: TRColor): TRColor; register;
procedure BlendMem(f: TRColor; var B: TRColor); register;
function BlendRegEx(f, B, M: TRColor): TRColor; register;
procedure BlendMemEx(const f: TRColor; var B: TRColor; M: TRColor); register;
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
  Various calculations stats can be retrieved by passing Stats parameter.
}
function BuildRasterHoughLine(const MaxAngle_, AlphaStep_: TGeoFloat; const BestLinesCount_: Integer; raster_: TMemoryRaster): THoughLineArry;
function BuildMorphHoughLine(const MaxAngle_, AlphaStep_, Treshold_: TGeoFloat; const BestLinesCount_: Integer; morph_: TMorphMath): THoughLineArry;
function BuildBinHoughLine(const MaxAngle_, AlphaStep_: TGeoFloat; const BestLinesCount_: Integer; bin_: TMorphBin): THoughLineArry;
function DocumentRotationDetected_MaxMatched(var BestLines: THoughLineArry): TGeoFloat;
function DocumentRotationDetected_MaxDistance(var BestLines: THoughLineArry): TGeoFloat;
function DocumentRotationDetected_AVG(var BestLines: THoughLineArry): TGeoFloat;

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

{
  MorphologySegmentation: fill in Vacancy
}
procedure ClassifyMapFillVacancy(Width, Height: Integer; var classifyMap: TMorphologySegClassifyMap);

{
  MorphologySegmentation: rebuild classifyMap
}
procedure ClassifyMapConvolution(Width, Height: Integer; var classifyMap: TMorphologySegClassifyMap; Classify: TMorphologyClassify;
  Operation_: TBinaryzationOperation; ConvolutionKernel: TMorphologyBinaryzation); overload;
procedure ClassifyMapConvolution(Width, Height: Integer; var classifyMap: TMorphologySegClassifyMap;
  Operations: array of TBinaryzationOperation; ConvolutionKernel: TMorphologyBinaryzation; MaxClassifyCount, MinGranularity: Integer); overload;

{
  rastermization performance test.
}

procedure TestRasterSavePerformance(inputfile: SystemString);

{$ENDREGION 'RasterAPI'}
{$REGION 'Constant'}


const
  CMorphologyPixelInfo: TMorphologyPixelInfo =
    (
    'Grayscale',
    'Luminance',
    'In-phase',
    'Quadrature-phase',
    'Hue',
    'Saturation',
    'Intensity',
    'Cyan',
    'Magenta',
    'Yellow',
    'Black',
    'Red',
    'Green',
    'Blue',
    'Alpha',
    'Black',
    'White',
    'Cyan',
    'Magenta',
    'mpYellow'
    );

  // Some predefined color constants
  rcBlack32 = TRColor($FF000000);
  rcDimGray32 = TRColor($FF3F3F3F);
  rcGray32 = TRColor($FF7F7F7F);
  rcLightGray32 = TRColor($FFBFBFBF);
  rcWhite32 = TRColor($FFFFFFFF);
  rcMaroon32 = TRColor($FF7F0000);
  rcGreen32 = TRColor($FF007F00);
  rcOlive32 = TRColor($FF7F7F00);
  rcNavy32 = TRColor($FF00007F);
  rcPurple32 = TRColor($FF7F007F);
  rcTeal32 = TRColor($FF007F7F);
  rcRed32 = TRColor($FFFF0000);
  rcLime32 = TRColor($FF00FF00);
  rcYellow32 = TRColor($FFFFFF00);
  rcBlue32 = TRColor($FF0000FF);
  rcFuchsia32 = TRColor($FFFF00FF);
  rcAqua32 = TRColor($FF00FFFF);
  rcAliceBlue32 = TRColor($FFF0F8FF);
  rcAntiqueWhite32 = TRColor($FFFAEBD7);
  rcAquamarine32 = TRColor($FF7FFFD4);
  rcAzure32 = TRColor($FFF0FFFF);
  rcBeige32 = TRColor($FFF5F5DC);
  rcBisque32 = TRColor($FFFFE4C4);
  rcBlancheDalmond32 = TRColor($FFFFEBCD);
  rcBlueViolet32 = TRColor($FF8A2BE2);
  rcBrown32 = TRColor($FFA52A2A);
  rcBurlyWood32 = TRColor($FFDEB887);
  rcCadetblue32 = TRColor($FF5F9EA0);
  rcChartReuse32 = TRColor($FF7FFF00);
  rcChocolate32 = TRColor($FFD2691E);
  rcCoral32 = TRColor($FFFF7F50);
  rcCornFlowerBlue32 = TRColor($FF6495ED);
  rcCornSilk32 = TRColor($FFFFF8DC);
  rcCrimson32 = TRColor($FFDC143C);
  rcDarkBlue32 = TRColor($FF00008B);
  rcDarkCyan32 = TRColor($FF008B8B);
  rcDarkGoldenRod32 = TRColor($FFB8860B);
  rcDarkGray32 = TRColor($FFA9A9A9);
  rcDarkGreen32 = TRColor($FF006400);
  rcDarkGrey32 = TRColor($FFA9A9A9);
  rcDarkKhaki32 = TRColor($FFBDB76B);
  rcDarkMagenta32 = TRColor($FF8B008B);
  rcDarkOliveGreen32 = TRColor($FF556B2F);
  rcDarkOrange32 = TRColor($FFFF8C00);
  rcDarkOrchid32 = TRColor($FF9932CC);
  rcDarkRed32 = TRColor($FF8B0000);
  rcDarkSalmon32 = TRColor($FFE9967A);
  rcDarkSeaGreen32 = TRColor($FF8FBC8F);
  rcDarkSlateBlue32 = TRColor($FF483D8B);
  rcDarkSlateGray32 = TRColor($FF2F4F4F);
  rcDarkSlateGrey32 = TRColor($FF2F4F4F);
  rcDarkTurquoise32 = TRColor($FF00CED1);
  rcDarkViolet32 = TRColor($FF9400D3);
  rcDeepPink32 = TRColor($FFFF1493);
  rcDeepSkyBlue32 = TRColor($FF00BFFF);
  rcDodgerBlue32 = TRColor($FF1E90FF);
  rcFireBrick32 = TRColor($FFB22222);
  rcFloralWhite32 = TRColor($FFFFFAF0);
  rcGainsBoro32 = TRColor($FFDCDCDC);
  rcGhostWhite32 = TRColor($FFF8F8FF);
  rcGold32 = TRColor($FFFFD700);
  rcGoldenRod32 = TRColor($FFDAA520);
  rcGreenYellow32 = TRColor($FFADFF2F);
  rcGrey32 = TRColor($FF808080);
  rcHoneyDew32 = TRColor($FFF0FFF0);
  rcHotPink32 = TRColor($FFFF69B4);
  rcIndianRed32 = TRColor($FFCD5C5C);
  rcIndigo32 = TRColor($FF4B0082);
  rcIvory32 = TRColor($FFFFFFF0);
  rcKhaki32 = TRColor($FFF0E68C);
  rcLavender32 = TRColor($FFE6E6FA);
  rcLavenderBlush32 = TRColor($FFFFF0F5);
  rcLawnGreen32 = TRColor($FF7CFC00);
  rcLemonChiffon32 = TRColor($FFFFFACD);
  rcLightBlue32 = TRColor($FFADD8E6);
  rcLightCoral32 = TRColor($FFF08080);
  rcLightCyan32 = TRColor($FFE0FFFF);
  rcLightGoldenRodYellow32 = TRColor($FFFAFAD2);
  rcLightGreen32 = TRColor($FF90EE90);
  rcLightGrey32 = TRColor($FFD3D3D3);
  rcLightPink32 = TRColor($FFFFB6C1);
  rcLightSalmon32 = TRColor($FFFFA07A);
  rcLightSeagreen32 = TRColor($FF20B2AA);
  rcLightSkyblue32 = TRColor($FF87CEFA);
  rcLightSlategray32 = TRColor($FF778899);
  rcLightSlategrey32 = TRColor($FF778899);
  rcLightSteelblue32 = TRColor($FFB0C4DE);
  rcLightYellow32 = TRColor($FFFFFFE0);
  rcLtGray32 = TRColor($FFC0C0C0);
  rcMedGray32 = TRColor($FFA0A0A4);
  rcDkGray32 = TRColor($FF808080);
  rcMoneyGreen32 = TRColor($FFC0DCC0);
  rcLegacySkyBlue32 = TRColor($FFA6CAF0);
  rcCream32 = TRColor($FFFFFBF0);
  rcLimeGreen32 = TRColor($FF32CD32);
  rcLinen32 = TRColor($FFFAF0E6);
  rcMediumAquamarine32 = TRColor($FF66CDAA);
  rcMediumBlue32 = TRColor($FF0000CD);
  rcMediumOrchid32 = TRColor($FFBA55D3);
  rcMediumPurple32 = TRColor($FF9370DB);
  rcMediumSeaGreen32 = TRColor($FF3CB371);
  rcMediumSlateBlue32 = TRColor($FF7B68EE);
  rcMediumSpringGreen32 = TRColor($FF00FA9A);
  rcMediumTurquoise32 = TRColor($FF48D1CC);
  rcMediumVioletRed32 = TRColor($FFC71585);
  rcMidnightBlue32 = TRColor($FF191970);
  rcMintCream32 = TRColor($FFF5FFFA);
  rcMistyRose32 = TRColor($FFFFE4E1);
  rcMoccasin32 = TRColor($FFFFE4B5);
  rcNavajoWhite32 = TRColor($FFFFDEAD);
  rcOldLace32 = TRColor($FFFDF5E6);
  rcOliveDrab32 = TRColor($FF6B8E23);
  rcOrange32 = TRColor($FFFFA500);
  rcOrangeRed32 = TRColor($FFFF4500);
  rcOrchid32 = TRColor($FFDA70D6);
  rcPaleGoldenRod32 = TRColor($FFEEE8AA);
  rcPaleGreen32 = TRColor($FF98FB98);
  rcPaleTurquoise32 = TRColor($FFAFEEEE);
  rcPaleVioletred32 = TRColor($FFDB7093);
  rcPapayaWhip32 = TRColor($FFFFEFD5);
  rcPeachPuff32 = TRColor($FFFFDAB9);
  rcPeru32 = TRColor($FFCD853F);
  rcPlum32 = TRColor($FFDDA0DD);
  rcPowderBlue32 = TRColor($FFB0E0E6);
  rcRosyBrown32 = TRColor($FFBC8F8F);
  rcRoyalBlue32 = TRColor($FF4169E1);
  rcSaddleBrown32 = TRColor($FF8B4513);
  rcSalmon32 = TRColor($FFFA8072);
  rcSandyBrown32 = TRColor($FFF4A460);
  rcSeaGreen32 = TRColor($FF2E8B57);
  rcSeaShell32 = TRColor($FFFFF5EE);
  rcSienna32 = TRColor($FFA0522D);
  rcSilver32 = TRColor($FFC0C0C0);
  rcSkyblue32 = TRColor($FF87CEEB);
  rcSlateBlue32 = TRColor($FF6A5ACD);
  rcSlateGray32 = TRColor($FF708090);
  rcSlateGrey32 = TRColor($FF708090);
  rcSnow32 = TRColor($FFFFFAFA);
  rcSpringgreen32 = TRColor($FF00FF7F);
  rcSteelblue32 = TRColor($FF4682B4);
  rcTan32 = TRColor($FFD2B48C);
  rcThistle32 = TRColor($FFD8BFD8);
  rcTomato32 = TRColor($FFFF6347);
  rcTurquoise32 = TRColor($FF40E0D0);
  rcViolet32 = TRColor($FFEE82EE);
  rcWheat32 = TRColor($FFF5DEB3);
  rcWhitesmoke32 = TRColor($FFF5F5F5);
  rcYellowgreen32 = TRColor($FF9ACD32);
  rcTrWhite32 = TRColor($7FFFFFFF);
  rcTrGray32 = TRColor($7F7F7F7F);
  rcTrBlack32 = TRColor($7F000000);
  rcTrRed32 = TRColor($7FFF0000);
  rcTrGreen32 = TRColor($7F00FF00);
  rcTrBlue32 = TRColor($7F0000FF);
{$ENDREGION 'Constant'}
{$REGION 'Var'}


var
  NewRaster: function: TMemoryRaster;
  NewRasterFromFile: function(const fn: string): TMemoryRaster;
  NewRasterFromStream: function(const stream: TCoreClassStream): TMemoryRaster;
  SaveRaster: procedure(mr: TMemoryRaster; const fn: string);

  {
    Morphology Convolution Kernel
  }
  Bin3x3, Bin5x5, Bin7x7, Bin9x9, Bin11x11, Bin13x13, Bin15x15, Bin17x17, Bin19x19, Bin21x21, Bin23x23, Bin25x25, Bin51x51, Bin99x99: TMorphologyBinaryzation;

  {
    Rastermization Serialized Pool
  }
  RasterSerializedPool: TRasterSerializedPool;

  {
    Color Table
  }
  Color255: array [Byte] of TRColor;
  Color65535: array [Word] of TRColor;

{$ENDREGION 'Var'}

implementation

uses h264Common, CoreCompress, DoStatusIO, DataFrameEngine, Raster_JPEG, Raster_PNG, zExpression, OpCode, zDrawEngine;

{$REGION 'InternalDefines'}


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

  TAtomFontRaster = {$IFDEF FPC}specialize {$ENDIF FPC}TAtomVar<TFontRaster>;

const
  ZERO_RECT: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);

var
  RcTable: array [Byte, Byte] of Byte;
  DivTable: array [Byte, Byte] of Byte;
  SystemFont: TAtomFontRaster;

{$ENDREGION 'InternalDefines'}

function IntersectRect_(out Dst: TRect; const r1, r2: TRect): Boolean; forward;
procedure OffsetRect_(var R: TRect; dx, dy: Integer); forward;
function IsRectEmpty_(const R: TRect): Boolean; forward;

{$INCLUDE MemoryRaster_SigmaGaussian.inc}
{$INCLUDE MemoryRaster_RasterClass.inc}
{$INCLUDE MemoryRaster_Agg.inc}
{$INCLUDE MemoryRaster_API.inc}
{$INCLUDE MemoryRaster_SequenceClass.inc}
{$INCLUDE MemoryRaster_Vertex.inc}
{$INCLUDE MemoryRaster_Font.inc}
{$INCLUDE MemoryRaster_Morphomatics.inc}
{$INCLUDE MemoryRaster_MorphologyBinaryzation.inc}
{$INCLUDE MemoryRaster_MorphologySegmentation.inc}
{$INCLUDE MemoryRaster_MorphologyRCLines.inc}
{$INCLUDE MemoryRaster_IOProcessor.inc}

{$REGION 'Intf'}


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

  disposeObject(m64);
end;

procedure SaveRaster_(mr: TMemoryRaster; const fn: string);
begin
  mr.SaveToFile(fn);
end;
{$ENDREGION 'Intf'}


initialization

TMemoryRaster.Parallel := {$IFDEF MemoryRaster_Parallel}True{$ELSE MemoryRaster_Parallel}False{$ENDIF MemoryRaster_Parallel};
TRasterVertex.DebugTriangle := False;
TRasterVertex.DebugTriangleColor := RColor($FF, $7F, $7F, $7F);
TRasterVertex.Parallel := {$IFDEF Vertex_Parallel}True{$ELSE Vertex_Parallel}False{$ENDIF Vertex_Parallel};
TRasterVertex.ParallelHeightTrigger := 500;
TRasterVertex.ParallelWidthTrigger := 100;
TMorphomatics.Parallel := {$IFDEF Morphomatics_Parallel}True{$ELSE Morphomatics_Parallel}False{$ENDIF Morphomatics_Parallel};
TMorphologyBinaryzation.Parallel := {$IFDEF MorphologyBinaryzation_Parallel}True{$ELSE MorphologyBinaryzation_Parallel}False{$ENDIF MorphologyBinaryzation_Parallel};

NewRaster := {$IFDEF FPC}@{$ENDIF FPC}NewRaster_;
NewRasterFromFile := {$IFDEF FPC}@{$ENDIF FPC}NewRasterFromFile_;
NewRasterFromStream := {$IFDEF FPC}@{$ENDIF FPC}NewRasterFromStream_;
SaveRaster := {$IFDEF FPC}@{$ENDIF FPC}SaveRaster_;

MakeMergeTables;
Init_DefaultFont;
InitBinaryzationPreset;
RasterSerializedPool := TRasterSerializedPool.Create(TRasterSerializedPool_Decl.Create);

FillColorTable(3, 3, 2, $FF, @Color255);
FillColorTable(6, 5, 5, $FFFF, @Color65535);

finalization

disposeObject(RasterSerializedPool.v);
disposeObject(RasterSerializedPool);
Free_DefaultFont;
FreeBinaryzationPreset;

end.
