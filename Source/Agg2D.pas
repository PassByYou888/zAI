{ ****************************************************************************** }
{ * memory Rasterization on AGG                                                * }
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

(*
  ////////////////////////////////////////////////////////////////////////////////
  //                                                                            //
  //  Anti-Grain Geometry (modernized Pascal fork, aka 'AggPasMod')             //
  //    Maintained by Christian-W. Budde (Christian@pcjv.de)                    //
  //    Copyright (c) 2012-2017                                                 //
  //                                                                            //
  //  Based on:                                                                 //
  //    Pascal port by Milan Marusinec alias Milano (milan@marusinec.sk)        //
  //    Copyright (c) 2005-2006, see http://www.aggpas.org                      //
  //                                                                            //
  //  Original License:                                                         //
  //    Anti-Grain Geometry - Version 2.4 (Public License)                      //
  //    Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)     //
  //    Contact: McSeem@antigrain.com / McSeemAgg@yahoo.com                     //
  //                                                                            //
  //  Permission to copy, use, modify, sell and distribute this software        //
  //  is granted provided this copyright notice appears in all copies.          //
  //  This software is provided "as is" without express or implied              //
  //  warranty, and with no claim as to its suitability for any purpose.        //
  //                                                                            //
  ////////////////////////////////////////////////////////////////////////////////
*)
unit Agg2D;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}

interface

uses
  Math,
  AggBasics, AggMath, AggArray, AggTransAffine, AggTransViewport, AggPathStorage, AggConvStroke, AggConvDash, AggConvTransform, AggConvCurve,
  AggRenderingBuffer, AggRendererBase, AggRendererScanLine, AggSpanGradient, AggSpanImageFilterRgba, AggSpanImageResampleRgba, AggSpanConverter,
  AggSpanInterpolatorLinear, AggSpanAllocator, AggRasterizerScanLineAA, AggGammaFunctions, AggScanlineUnpacked, AggArc, AggBezierArc, AggRoundedRect,
  AggPixelFormat, AggPixelFormatRgba, AggColor32, AggMathStroke, AggImageFilters, AggVertexSource, AggRenderScanlines,
  AggAlphaMaskUnpacked8, AggArrowHead, AggBitsetIterator, AggBlur, AggBoundingRect, AggBSpline, AggClipLiangBarsky, AggColorConversion, AggControl,
  AggConvAdaptorVcgen, AggConvAdaptorVpgen, AggConvBSpline, AggConvClipPolygon, AggConvClipPolyline, AggConvConcat, AggConvContour, AggConvMarker,
  AggConvMarkerAdaptor, AggConvSegmentator, AggConvShortenPath, AggConvSmoothPoly, AggCurves, AggDdaLine, AggEllipse, AggEllipseBresenham, AggGammaLUT,
  AggGradientLut, AggLineAABasics, AggPathStorageInteger, AggPatternFiltersRgba, AggPixelFormatAlphaMaskAdaptor, AggPixelFormatGray, AggPixelFormatRgb,
  AggPixelFormatRgbPacked, AggPixelFormatTransposer, AggQuadratureOscillator, AggRasterizerCellsAA, AggRasterizerCompoundAA, AggRasterizerOutline,
  AggRasterizerOutlineAA, AggRasterizerScanLine, AggRasterizerScanlineClip, AggRendererMarkers, AggRendererMultiClip, AggRendererOutlineAA,
  AggRendererOutlineImage, AggRendererPrimitives, AggRenderingBufferDynaRow, AggScanline, AggScanlineBin, AggScanlineBooleanAlgebra, AggScanlinePacked,
  AggScanlineStorageAA, AggScanlineStorageBin, AggShortenPath, AggSimulEq, AggSpanGenerator, AggSpanGouraud, AggSpanGouraudGray, AggSpanGouraudRgba,
  AggSpanGradientAlpha, AggSpanGradientContour, AggSpanGradientImage, AggSpanImageFilter, AggSpanImageFilterGray, AggSpanImageFilterRgb, AggSpanImageResample,
  AggSpanImageResampleGray, AggSpanImageResampleRgb, AggSpanInterpolatorAdaptor, AggSpanInterpolatorPerspective, AggSpanInterpolatorTrans, AggSpanPattern,
  AggSpanPatternFilterGray, AggSpanPatternFilterRgb, AggSpanPatternFilterRgba, AggSpanPatternResampleGray, AggSpanPatternResampleRgb,
  AggSpanPatternResampleRgba, AggSpanPatternRgb, AggSpanPatternRgba, AggSpanSolid, AggSpanSubdivAdaptor, AggSpiral, AggTransBilinear, AggTransDoublePath,
  AggTransPerspective, AggTransSinglePath, AggTransWarpMagnifier, AggVcgenBSpline, AggVcgenContour, AggVcgenDash, AggVcgenMarkersTerm, AggVcgenSmoothPoly1,
  AggVcgenStroke, AggVcgenVertexSequence, AggVertexSequence, AggVpGen, AggVpGenClipPolygon, AggVpGenClipPolyline, AggVpGenSegmentator;

type
  TAggPixelFormat = (pfRGBA, pfBGRA);
  PAggColorRgba8 = ^TAggColorRgba8;
  TAggColorRgba8 = TAggRgba8;
  TAggGradient = (grdSolid, grdLinear, grdRadial);
  TAggDirection = (dirCW, dirCCW);
  TAggDrawPathFlag = (dpfFillOnly, dpfStrokeOnly, dpfFillAndStroke, dpfFillWithHorizontalLineColor);
  TAggViewportOption = (voAnisotropic, voXMinYMin, voXMidYMin, voXMaxYMin, voXMinYMid, voXMidYMid, voXMaxYMid, voXMinYMax, voXMidYMax, voXMaxYMax);
  TAggImageFilterType = (ifNoFilter, ifBilinear, ifHanning, ifHermite, ifQuadric, ifBicubic, ifCatrom, ifSpline16, ifSpline36, ifBlackman144);
  TAggImageResample = (irNever, irAlways, irOnZoomOut);
  PAggTransformations = ^TAggTransformations;

  TAggTransformations = record
    AffineMatrix: TAggParallelogram;
  end;

  TAgg2DImage = class
  private
    FRenderingBuffer: TAggRenderingBuffer;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetScanLine(index: Cardinal): Pointer;
  public
    constructor Create(buffer: PInt8u; AWidth, AHeight: Cardinal; stride: Integer);
    destructor Destroy; override;

    procedure Attach(buffer: PInt8u; AWidth, AHeight: Cardinal; stride: Integer);

    procedure PreMultiply;
    procedure DeMultiply;

    property ScanLine[index: Cardinal]: Pointer read GetScanLine;
    property width: Integer read GetWidth;
    property height: Integer read GetHeight;
  end;

  TAgg2DRasterizerGamma = class(TAggVertexSource)
  private
    FAlpha: TAggGammaMultiply;
    FGamma: TAggGammaPower;
  public
    constructor Create(alpha, Gamma: Double);
    destructor Destroy; override;

    function FuncOperatorGamma(x: Double): Double; override;
  end;

  TAgg2D = class
  private
    FRenderingBuffer: TAggRenderingBuffer;

    FRendererBase: TAggRendererBase;
    FRendererBaseComp: TAggRendererBase;
    FRendererBasePre: TAggRendererBase;
    FRendererBaseCompPre: TAggRendererBase;

    FRendererSolid: TAggRendererScanLineAASolid;
    FRendererSolidComp: TAggRendererScanLineAASolid;

    FAllocator: TAggSpanAllocator;
    FClipBox: TRectDouble;

    FBlendMode: TAggBlendMode;
    FImageBlendMode: TAggBlendMode;

    FScanLine: TAggScanLineUnpacked8;
    FRasterizer: TAggRasterizerScanLineAA;

    FMasterAlpha: Double;
    FAntiAliasGamma: Double;

    FFillGradient: TAggPodAutoArray;
    FLineGradient: TAggPodAutoArray;

    FLineCap: TAggLineCap;
    FLineJoin: TAggLineJoin;

    FFillGradientFlag: TAggGradient;
    FLineGradientFlag: TAggGradient;

    FFillGradientMatrix: TAggTransAffine;
    FLineGradientMatrix: TAggTransAffine;

    FFillGradientD1: Double;
    FLineGradientD1: Double;
    FFillGradientD2: Double;
    FLineGradientD2: Double;

    FImageFilter: TAggImageFilterType;
    FImageResample: TAggImageResample;
    FImageFilterLUT: TAggImageFilter;

    FFillGradientInterpolator: TAggSpanInterpolatorLinear;
    FLineGradientInterpolator: TAggSpanInterpolatorLinear;

    FLinearGradientFunction: TAggGradientX;
    FRadialGradientFunction: TAggGradientCircle;

    FLineWidth: Double;
    FEvenOddFlag: Boolean;

    FPath: TAggPathStorage;
    FTransform: TAggTransAffine;

    FConvCurve: TAggConvCurve;
    FConvDash: TAggConvDash;
    FConvStroke: TAggConvStroke;

    FPathTransform: TAggConvTransform;
    FStrokeTransform: TAggConvTransform;

    // Other Pascal-specific members
    FGammaNone: TAggGammaNone;
    FGammaAgg2D: TAgg2DRasterizerGamma;

    FImageFilterBilinear: TAggImageFilterBilinear;
    FImageFilterHanning: TAggImageFilterHanning;
    FImageFilterHermite: TAggImageFilterHermite;
    FImageFilterQuadric: TAggImageFilterQuadric;
    FImageFilterBicubic: TAggImageFilterBicubic;
    FImageFilterCatrom: TAggImageFilterCatrom;
    FImageFilterSpline16: TAggImageFilterSpline16;
    FImageFilterSpline36: TAggImageFilterSpline36;
    FImageFilterBlackman144: TAggImageFilterBlackman144;

    procedure Render(AFillColor: Boolean); overload;

    procedure AddLine(x1, y1, x2, y2: Double);
    procedure AddEllipse(Cx, Cy, RX, RY: Double; dir: TAggDirection); overload;
    procedure AddEllipse(center, radius: TPointDouble; dir: TAggDirection); overload;

    procedure RenderImage(img: TAgg2DImage; x1, y1, x2, y2: Integer; Parl: PDouble); overload;
    procedure RenderImage(img: TAgg2DImage; Rect: TRectInteger; Parl: PDouble); overload;
    procedure SetImageFilter(f: TAggImageFilterType);

    procedure SetImageResample(f: TAggImageResample); overload;
    procedure SetLineCap(Cap: TAggLineCap); overload;
    procedure SetLineJoin(Join: TAggLineJoin); overload;
    procedure SetFillEvenOdd(EvenOddFlag: Boolean); overload;
    procedure SetBlendMode(Value: TAggBlendMode);
    procedure SetImageBlendMode(Value: TAggBlendMode);
    procedure SetFillColor(c: TAggColorRgba8); overload;
    procedure SetLineColor(c: TAggColorRgba8); overload;
    procedure SetImageBlendColor(c: TAggColorRgba8); overload;
    procedure SetMasterAlpha(a: Double); overload;
    procedure SetAntiAliasGamma(g: Double);
    procedure SetLineWidth(w: Double);
    function GetRow(y: Cardinal): PInt8u;
  protected
    FImageBlendColor: TAggColorRgba8;
    FFillColor: TAggColorRgba8;
    FLineColor: TAggColorRgba8;

    FPixelFormat: TAggPixelFormat;
    FPixelFormatProc: TAggPixelFormatProcessor;
    FPixelFormatComp: TAggPixelFormatProcessor;
    FPixelFormatPre: TAggPixelFormatProcessor;
    FPixelFormatCompPre: TAggPixelFormatProcessor;

    procedure UpdateApproximationScale;
    procedure UpdateRasterizerGamma;

    property RenderingBuffer: TAggRenderingBuffer read FRenderingBuffer;
    property RendererBase: TAggRendererBase read FRendererBase;
    property RendererBaseComp: TAggRendererBase read FRendererBaseComp;
    property RendererBasePre: TAggRendererBase read FRendererBasePre;
    property RendererBaseCompPre: TAggRendererBase read FRendererBaseCompPre;
    property Rasterizer: TAggRasterizerScanLineAA read FRasterizer;
    property Path: TAggPathStorage read FPath;
  public
    constructor Create(PixelFormat: TAggPixelFormat = pfBGRA); overload; virtual;
    constructor Create(buffer: PInt8u; width, height: Cardinal; stride: Integer; PixelFormat: TAggPixelFormat = pfBGRA); overload; virtual;
    destructor Destroy; override;

    // Setup
    procedure Attach(buffer: PInt8u; width, height: Cardinal; stride: Integer); overload;
    procedure Attach(img: TAgg2DImage); overload;

    procedure ClipBox(x1, y1, x2, y2: Double); overload;
    function ClipBox: TRectDouble; overload;

    procedure ClearAll(c: TAggColorRgba8); overload;
    procedure ClearAll(r, g, b: Cardinal; a: Cardinal = 255); overload;

    procedure ClearClipBox(c: TAggColorRgba8); overload;
    procedure ClearClipBox(r, g, b: Cardinal; a: Cardinal = 255); overload;

    // Conversions
    procedure WorldToScreen(x, y: PDouble); overload;
    procedure WorldToScreen(var x, y: Double); overload;
    procedure ScreenToWorld(x, y: PDouble); overload;
    procedure ScreenToWorld(var x, y: Double); overload;
    function WorldToScreen(Scalar: Double): Double; overload;
    function ScreenToWorld(Scalar: Double): Double; overload;

    procedure AlignPoint(x, y: PDouble); overload;
    procedure AlignPoint(var x, y: Double); overload;

    function InBox(WorldX, WorldY: Double): Boolean; overload;
    function InBox(World: TPointDouble): Boolean; overload;

    // General Attributes
    procedure SetFillColor(r, g, b: Cardinal; a: Cardinal = 255); overload;
    procedure SetLineColor(r, g, b: Cardinal; a: Cardinal = 255); overload;
    procedure SetImageBlendColor(r, g, b: Cardinal; a: Cardinal = 255); overload;
    procedure NoFill;
    procedure NoLine;

    procedure FillLinearGradient(x1, y1, x2, y2: Double; c1, c2: TAggColorRgba8; Profile: Double = 1);
    procedure LineLinearGradient(x1, y1, x2, y2: Double; c1, c2: TAggColorRgba8; Profile: Double = 1);

    procedure FillRadialGradient(x, y, r: Double; c1, c2: TAggColorRgba8; Profile: Double = 1); overload;
    procedure LineRadialGradient(x, y, r: Double; c1, c2: TAggColorRgba8; Profile: Double = 1); overload;

    procedure FillRadialGradient(x, y, r: Double; c1, c2, c3: TAggColorRgba8); overload;
    procedure LineRadialGradient(x, y, r: Double; c1, c2, c3: TAggColorRgba8); overload;

    procedure FillRadialGradient(x, y, r: Double); overload;
    procedure LineRadialGradient(x, y, r: Double); overload;

    procedure RemoveAllDashes;
    procedure AddDash(DashLength, GapLength: Double);

    // Transformations
    function GetTransformations: TAggTransformations;
    procedure SetTransformations(var tr: TAggTransformations); overload;
    procedure SetTransformations(v0, v1, v2, v3, v4, V5: Double); overload;
    procedure ResetTransformations;

    procedure Affine(tr: TAggTransAffine); overload;
    procedure Affine(var tr: TAggTransformations); overload;

    procedure Rotate(angle: Double);
    procedure Scale(SX, SY: Double);
    procedure Skew(SX, SY: Double);
    procedure Translate(x, y: Double);

    procedure Parallelogram(x1, y1, x2, y2: Double; Para: PDouble);

    procedure viewport(WorldX1, WorldY1, WorldX2, WorldY2, ScreenX1, ScreenY1, ScreenX2, ScreenY2: Double; opt: TAggViewportOption = voXMidYMid); overload;
    procedure viewport(World, Screen: TRectDouble; opt: TAggViewportOption = voXMidYMid); overload;

    // Basic Shapes
    procedure Line(x1, y1, x2, y2: Double);
    procedure Triangle(x1, y1, x2, y2, x3, y3: Double);
    procedure Rectangle(x1, y1, x2, y2: Double);

    procedure RoundedRect(x1, y1, x2, y2, r: Double); overload;
    procedure RoundedRect(Rect: TRectDouble; r: Double); overload;
    procedure RoundedRect(x1, y1, x2, y2, RX, RY: Double); overload;
    procedure RoundedRect(Rect: TRectDouble; RX, RY: Double); overload;
    procedure RoundedRect(x1, y1, x2, y2, RxBottom, RyBottom, RxTop, RyTop: Double); overload;

    procedure Ellipse(Cx, Cy, RX, RY: Double);
    procedure Circle(Cx, Cy, radius: Double);

    procedure Arc(Cx, Cy, RX, RY, Start, Sweep: Double);
    procedure Star(Cx, Cy, r1, r2, startAngle: Double; NumRays: Integer);

    procedure Curve(x1, y1, x2, y2, x3, y3: Double); overload;
    procedure Curve(x1, y1, x2, y2, x3, y3, x4, y4: Double); overload;

    procedure Polygon(xy: PPointDouble; NumPoints: Integer; flag: TAggDrawPathFlag = dpfFillAndStroke);
    procedure Polyline(xy: PPointDouble; NumPoints: Integer);

    // Path commands
    procedure ResetPath;

    procedure MoveTo(x, y: Double);
    procedure MoveRel(dx, dy: Double);

    procedure LineTo(x, y: Double);
    procedure LineRel(dx, dy: Double);

    procedure HorizontalLineTo(x: Double);
    procedure HorizontalLineRel(dx: Double);

    procedure VerticalLineTo(y: Double);
    procedure VerticalLineRel(dy: Double);

    procedure ArcTo(RX, RY, angle: Double; LargeArcFlag, SweepFlag: Boolean; x, y: Double); overload;
    procedure ArcRel(RX, RY, angle: Double; LargeArcFlag, SweepFlag: Boolean; dx, dy: Double); overload;

    procedure QuadricCurveTo(XCtrl, YCtrl, XTo, YTo: Double); overload;
    procedure QuadricCurveRel(DxCtrl, DyCtrl, DxTo, DyTo: Double); overload;
    procedure QuadricCurveTo(XTo, YTo: Double); overload;
    procedure QuadricCurveRel(DxTo, DyTo: Double); overload;

    procedure CubicCurveTo(XCtrl1, YCtrl1, XCtrl2, YCtrl2, XTo, YTo: Double); overload;
    procedure CubicCurveRel(DxCtrl1, DyCtrl1, DxCtrl2, DyCtrl2, DxTo, DyTo: Double); overload;
    procedure CubicCurveTo(XCtrl2, YCtrl2, XTo, YTo: Double); overload;
    procedure CubicCurveRel(XCtrl2, YCtrl2, XTo, YTo: Double); overload;

    procedure ClosePolygon;

    procedure DrawPath(flag: TAggDrawPathFlag = dpfFillAndStroke);
    procedure DrawPathNoTransform(flag: TAggDrawPathFlag = dpfFillAndStroke);

    // Image Transformations
    procedure TransformImage(img: TAgg2DImage; ImgX1, ImgY1, ImgX2, ImgY2: Integer; DstX1, DstY1, DstX2, DstY2: Double); overload;
    procedure TransformImage(img: TAgg2DImage; ImgRect: TRectInteger; DstX1, DstY1, DstX2, DstY2: Double); overload;
    procedure TransformImage(img: TAgg2DImage; ImgRect: TRectInteger; Destination: TRectDouble); overload;

    procedure TransformImage(img: TAgg2DImage; DstX1, DstY1, DstX2, DstY2: Double); overload;
    procedure TransformImage(img: TAgg2DImage; Destination: TRectDouble); overload;

    procedure TransformImage(img: TAgg2DImage; ImgX1, ImgY1, ImgX2, ImgY2: Integer; Parallelogram: PDouble); overload;
    procedure TransformImage(img: TAgg2DImage; ImgRect: TRectInteger; Parallelogram: PDouble); overload;

    procedure TransformImage(img: TAgg2DImage; Parallelogram: PDouble); overload;
    procedure TransformImagePath(img: TAgg2DImage; ImgX1, ImgY1, ImgX2, ImgY2: Integer; DstX1, DstY1, DstX2, DstY2: Double); overload;
    procedure TransformImagePath(img: TAgg2DImage; DstX1, DstY1, DstX2, DstY2: Double); overload;
    procedure TransformImagePath(img: TAgg2DImage; ImgX1, ImgY1, ImgX2, ImgY2: Integer; Parallelogram: PDouble); overload;
    procedure TransformImagePath(img: TAgg2DImage; Parallelogram: PDouble); overload;

    // Image Blending (no transformations available)
    procedure BlendImage(img: TAgg2DImage; ImgX1, ImgY1, ImgX2, ImgY2: Integer; Dstx, Dsty: Double; alpha: Cardinal = 255); overload;
    procedure BlendImage(img: TAgg2DImage; Dstx, Dsty: Double; alpha: Cardinal = 255); overload;

    // Copy image directly, together with alpha-channel
    procedure CopyImage(img: TAgg2DImage; ImgX1, ImgY1, ImgX2, ImgY2: Integer; Dstx, Dsty: Double); overload;
    procedure CopyImage(img: TAgg2DImage; ImgRect: TRectInteger; Destination: TPointDouble); overload;

    procedure CopyImage(img: TAgg2DImage; Dstx, Dsty: Double); overload;
    procedure CopyImage(img: TAgg2DImage; Destination: TPointDouble); overload;

    property AntiAliasGamma: Double read FAntiAliasGamma write SetAntiAliasGamma;
    property ImageBlendColor: TAggColorRgba8 read FImageBlendColor write SetImageBlendColor;
    property FillColor: TAggColorRgba8 read FFillColor write SetFillColor;
    property LineColor: TAggColorRgba8 read FLineColor write SetLineColor;
    property ImageFilter: TAggImageFilterType read FImageFilter write SetImageFilter;
    property BlendMode: TAggBlendMode read FBlendMode write SetBlendMode;
    property LineWidth: Double read FLineWidth write SetLineWidth;
    property LineJoin: TAggLineJoin read FLineJoin write SetLineJoin;
    property LineCap: TAggLineCap read FLineCap write SetLineCap;
    property FillEvenOdd: Boolean read FEvenOddFlag write SetFillEvenOdd;
    property ImageResample: TAggImageResample read FImageResample write SetImageResample;
    property Row[y: Cardinal]: PInt8u read GetRow;
    property ImageBlendMode: TAggBlendMode read FImageBlendMode write SetImageBlendMode;
    property MasterAlpha: Double read FMasterAlpha write SetMasterAlpha;
  end;

  TAggSpanConvImageBlend = class(TAggSpanConvertor)
  private
    FMode: TAggBlendMode;
    FColor: TAggColorRgba8;
    FPixel: TAggPixelFormatProcessor; // FPixelFormatCompPre
  public
    constructor Create(BlendMode: TAggBlendMode; c: TAggColorRgba8; p: TAggPixelFormatProcessor);
    procedure Convert(Span: PAggColor; x, y: Integer; Len: Cardinal); override;
  end;

function OperatorIsEqual(c1, c2: PAggColorRgba8): Boolean;
function OperatorIsNotEqual(c1, c2: PAggColorRgba8): Boolean;
procedure Agg2DRendererRender(Gr: TAgg2D; RendererBase: TAggRendererBase; RenSolid: TAggRendererScanLineAASolid; FillColor: Boolean); overload;
procedure Agg2DRendererRenderImage(Gr: TAgg2D; img: TAgg2DImage; RendererBase: TAggRendererBase; Interpolator: TAggSpanInterpolatorLinear);

implementation

var
  GApproxScale: Double = 2;

  { TAgg2DImage }

constructor TAgg2DImage.Create(buffer: PInt8u; AWidth, AHeight: Cardinal;
  stride: Integer);
begin
  FRenderingBuffer := TAggRenderingBuffer.Create(buffer, AWidth, AHeight, stride);
end;

destructor TAgg2DImage.Destroy;
begin
  FRenderingBuffer.Free;
  inherited;
end;

procedure TAgg2DImage.Attach(buffer: PInt8u; AWidth, AHeight: Cardinal; stride: Integer);
begin
  FRenderingBuffer.Attach(buffer, AWidth, AHeight, stride);
end;

function TAgg2DImage.GetWidth: Integer;
begin
  Result := FRenderingBuffer.width;
end;

function TAgg2DImage.GetHeight: Integer;
begin
  Result := FRenderingBuffer.height;
end;

function TAgg2DImage.GetScanLine(index: Cardinal): Pointer;
begin
  Result := FRenderingBuffer.Row(index)
end;

procedure TAgg2DImage.PreMultiply;
var
  PixelFormatProcessor: TAggPixelFormatProcessor;
begin
  PixelFormatRgba32(PixelFormatProcessor, FRenderingBuffer);
  PixelFormatProcessor.PreMultiply;
end;

procedure TAgg2DImage.DeMultiply;
var
  PixelFormatProcessor: TAggPixelFormatProcessor;
begin
  PixelFormatRgba32(PixelFormatProcessor, FRenderingBuffer);
  PixelFormatProcessor.DeMultiply;
end;

{ TAgg2DRasterizerGamma }

constructor TAgg2DRasterizerGamma.Create(alpha, Gamma: Double);
begin
  FAlpha := TAggGammaMultiply.Create(alpha);
  FGamma := TAggGammaPower.Create(Gamma);
end;

destructor TAgg2DRasterizerGamma.Destroy;
begin
  FAlpha.Free;
  FGamma.Free;

  inherited;
end;

function TAgg2DRasterizerGamma.FuncOperatorGamma(x: Double): Double;
begin
  Result := FAlpha.FuncOperatorGamma(FGamma.FuncOperatorGamma(x));
end;

{ TAgg2D }

constructor TAgg2D.Create(PixelFormat: TAggPixelFormat = pfBGRA);
begin
  FGammaAgg2D := nil;

  FRenderingBuffer := TAggRenderingBuffer.Create;

  FPixelFormat := PixelFormat;
  case PixelFormat of
    pfRGBA:
      begin
        PixelFormatRgba32(FPixelFormatProc, FRenderingBuffer);
        PixelFormatCustomBlendRgba(FPixelFormatComp, FRenderingBuffer, @BlendModeAdaptorRgba, CAggOrderRgba);
        PixelFormatRgba32(FPixelFormatPre, FRenderingBuffer);
        PixelFormatCustomBlendRgba(FPixelFormatCompPre, FRenderingBuffer, @BlendModeAdaptorRgba, CAggOrderRgba);
      end;
    pfBGRA:
      begin
        PixelFormatBgra32(FPixelFormatProc, FRenderingBuffer);
        PixelFormatCustomBlendRgba(FPixelFormatComp, FRenderingBuffer, @BlendModeAdaptorRgba, CAggOrderBgra);
        PixelFormatBgra32(FPixelFormatPre, FRenderingBuffer);
        PixelFormatCustomBlendRgba(FPixelFormatCompPre, FRenderingBuffer, @BlendModeAdaptorRgba, CAggOrderBgra);
      end;
  end;

  FRendererBase := TAggRendererBase.Create(FPixelFormatProc);
  FRendererBaseComp := TAggRendererBase.Create(FPixelFormatComp);
  FRendererBasePre := TAggRendererBase.Create(FPixelFormatPre);
  FRendererBaseCompPre := TAggRendererBase.Create(FPixelFormatCompPre);

  FRendererSolid := TAggRendererScanLineAASolid.Create(FRendererBase);
  FRendererSolidComp := TAggRendererScanLineAASolid.Create(FRendererBaseComp);

  FAllocator := TAggSpanAllocator.Create;
  FClipBox := RectDouble(0, 0, 0, 0);

  FBlendMode := bmAlpha;
  FImageBlendMode := bmDestination;

  FImageBlendColor.Initialize(0, 0, 0);

  FScanLine := TAggScanLineUnpacked8.Create;
  FRasterizer := TAggRasterizerScanLineAA.Create;

  FMasterAlpha := 1;
  FAntiAliasGamma := 1;

  FFillColor.Initialize(255, 255, 255);
  FLineColor.Initialize(0, 0, 0);

  FFillGradient := TAggPodAutoArray.Create(256, SizeOf(TAggColor));
  FLineGradient := TAggPodAutoArray.Create(256, SizeOf(TAggColor));

  FLineCap := lcRound;
  FLineJoin := ljRound;

  FFillGradientFlag := grdSolid;
  FLineGradientFlag := grdSolid;

  FFillGradientMatrix := TAggTransAffine.Create;
  FLineGradientMatrix := TAggTransAffine.Create;

  FFillGradientD1 := 0;
  FLineGradientD1 := 0;
  FFillGradientD2 := 100;
  FLineGradientD2 := 100;

  FImageFilter := ifBlackman144;
  FImageResample := irNever;

  FGammaNone := TAggGammaNone.Create;

  FImageFilterBilinear := TAggImageFilterBilinear.Create;
  FImageFilterHanning := TAggImageFilterHanning.Create;
  FImageFilterHermite := TAggImageFilterHermite.Create;
  FImageFilterQuadric := TAggImageFilterQuadric.Create;
  FImageFilterBicubic := TAggImageFilterBicubic.Create;
  FImageFilterCatrom := TAggImageFilterCatrom.Create;
  FImageFilterSpline16 := TAggImageFilterSpline16.Create;
  FImageFilterSpline36 := TAggImageFilterSpline36.Create;
  FImageFilterBlackman144 := TAggImageFilterBlackman144.Create;

  FImageFilterLUT := TAggImageFilter.Create(FImageFilterBilinear, True);

  FLinearGradientFunction := TAggGradientX.Create;
  FRadialGradientFunction := TAggGradientCircle.Create;

  FFillGradientInterpolator := TAggSpanInterpolatorLinear.Create(FFillGradientMatrix);
  FLineGradientInterpolator := TAggSpanInterpolatorLinear.Create(FLineGradientMatrix);

  FLineWidth := 1;
  FEvenOddFlag := False;

  FPath := TAggPathStorage.Create;
  FTransform := TAggTransAffine.Create;

  FConvCurve := TAggConvCurve.Create(FPath);
  FConvDash := TAggConvDash.Create(FConvCurve);
  FConvStroke := TAggConvStroke.Create(FConvCurve);

  FPathTransform := TAggConvTransform.Create(FConvCurve, FTransform);
  FStrokeTransform := TAggConvTransform.Create(FConvStroke, FTransform);

  SetLineCap(FLineCap);
  SetLineJoin(FLineJoin);
end;

constructor TAgg2D.Create(buffer: PInt8u; width, height: Cardinal;
  stride: Integer; PixelFormat: TAggPixelFormat = pfBGRA);
begin
  Create(PixelFormat);
  Attach(buffer, width, height, stride);
end;

destructor TAgg2D.Destroy;
begin
  FRendererBase.Free;
  FRendererBaseComp.Free;
  FRendererBasePre.Free;
  FRendererBaseCompPre.Free;

  FRendererSolid.Free;
  FRendererSolidComp.Free;
  FRenderingBuffer.Free;

  FPathTransform.Free;
  FStrokeTransform.Free;

  FAllocator.Free;

  FScanLine.Free;
  FRasterizer.Free;
  FTransform.Free;

  FFillGradient.Free;
  FLineGradient.Free;

  FLinearGradientFunction.Free;
  FRadialGradientFunction.Free;

  FFillGradientInterpolator.Free;
  FLineGradientInterpolator.Free;

  FFillGradientMatrix.Free;
  FLineGradientMatrix.Free;

  FImageFilterBilinear.Free;
  FImageFilterHanning.Free;
  FImageFilterHermite.Free;
  FImageFilterQuadric.Free;
  FImageFilterBicubic.Free;
  FImageFilterCatrom.Free;
  FImageFilterSpline16.Free;
  FImageFilterSpline36.Free;
  FImageFilterBlackman144.Free;

  FImageFilterLUT.Free;
  FPath.Free;
  FGammaNone.Free;
  FGammaAgg2D.Free;

  FConvCurve.Free;
  FConvDash.Free;
  FConvStroke.Free;

  FPixelFormatProc.Free;
  FPixelFormatComp.Free;
  FPixelFormatPre.Free;
  FPixelFormatCompPre.Free;
end;

procedure TAgg2D.Attach(buffer: PInt8u; width, height: Cardinal; stride: Integer);
begin
  FRenderingBuffer.Attach(buffer, width, height, stride);

  FRendererBase.ResetClipping(True);
  FRendererBaseComp.ResetClipping(True);
  FRendererBasePre.ResetClipping(True);
  FRendererBaseCompPre.ResetClipping(True);

  ResetTransformations;

  SetLineWidth(1);
  SetLineColor(0, 0, 0);
  SetFillColor(255, 255, 255);

  ClipBox(0, 0, width, height);
  LineCap := lcRound;
  LineJoin := ljRound;

  SetImageFilter(ifBilinear);
  SetImageResample(irNever);

  FMasterAlpha := 1;
  FAntiAliasGamma := 1;

  FRasterizer.Gamma(FGammaNone);

  FBlendMode := bmAlpha;
end;

procedure TAgg2D.Attach(img: TAgg2DImage);
begin
  Attach(img.FRenderingBuffer.buffer, img.FRenderingBuffer.width,
    img.FRenderingBuffer.height, img.FRenderingBuffer.stride);
end;

procedure TAgg2D.ClipBox(x1, y1, x2, y2: Double);
var
  Rect: TRectInteger;
begin
  FClipBox := RectDouble(x1, y1, x2, y2);

  Rect := RectInteger(Trunc(x1), Trunc(y1), Trunc(x2), Trunc(y2));

  FRendererBase.SetClipBox(Rect.x1, Rect.y1, Rect.x2, Rect.y2);
  FRendererBaseComp.SetClipBox(Rect.x1, Rect.y1, Rect.x2, Rect.y2);
  FRendererBasePre.SetClipBox(Rect.x1, Rect.y1, Rect.x2, Rect.y2);
  FRendererBaseCompPre.SetClipBox(Rect.x1, Rect.y1, Rect.x2, Rect.y2);

  FRasterizer.SetClipBox(x1, y1, x2, y2);
end;

function TAgg2D.ClipBox: TRectDouble;
begin
  Result := FClipBox;
end;

procedure TAgg2D.ClearAll(c: TAggColorRgba8);
begin
  FRendererBase.Clear(c);
end;

procedure TAgg2D.ClearAll(r, g, b: Cardinal; a: Cardinal = 255);
var
  Clr: TAggColorRgba8;
begin
  Clr.Initialize(r, g, b, a);
  ClearAll(Clr);
end;

procedure TAgg2D.ClearClipBox(c: TAggColorRgba8);
var
  Clr: TAggColor;
begin
  Clr.FromRgba8(c);

  FRendererBase.CopyBar(0, 0, FRendererBase.width, FRendererBase.height, @Clr);
end;

procedure TAgg2D.ClearClipBox(r, g, b: Cardinal; a: Cardinal = 255);
var
  Clr: TAggColorRgba8;
begin
  Clr.Initialize(r, g, b, a);
  ClearClipBox(Clr);
end;

procedure TAgg2D.WorldToScreen(x, y: PDouble);
begin
  FTransform.Transform(FTransform, x, y);
end;

procedure TAgg2D.WorldToScreen(var x, y: Double);
begin
  FTransform.Transform(FTransform, @x, @y);
end;

procedure TAgg2D.ScreenToWorld(x, y: PDouble);
begin
  FTransform.InverseTransform(FTransform, x, y);
end;

procedure TAgg2D.ScreenToWorld(var x, y: Double);
begin
  FTransform.InverseTransform(FTransform, @x, @y);
end;

function TAgg2D.WorldToScreen(Scalar: Double): Double;
var
  Rect: TRectDouble;
begin
  Rect.x1 := 0;
  Rect.y1 := 0;
  Rect.x2 := Scalar;
  Rect.y2 := Scalar;

  WorldToScreen(Rect.x1, Rect.y1);
  WorldToScreen(Rect.x2, Rect.y2);

  Result := Sqrt(0.5 * (Sqr(Rect.x2 - Rect.x1) + Sqr(Rect.y2 - Rect.y1)));
end;

function TAgg2D.ScreenToWorld(Scalar: Double): Double;
var
  x1, y1, x2, y2: Double;
begin
  x1 := 0;
  y1 := 0;
  x2 := Scalar;
  y2 := Scalar;

  ScreenToWorld(@x1, @y1);
  ScreenToWorld(@x2, @y2);

  Result := Sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)) * 0.7071068;
end;

procedure TAgg2D.AlignPoint(x, y: PDouble);
begin
  WorldToScreen(x, y);

  x^ := Floor(x^) + 0.5;
  y^ := Floor(y^) + 0.5;

  ScreenToWorld(x, y);
end;

procedure TAgg2D.AlignPoint(var x, y: Double);
begin
  WorldToScreen(x, y);

  x := Floor(x) + 0.5;
  y := Floor(y) + 0.5;

  ScreenToWorld(x, y);
end;

function TAgg2D.InBox(WorldX, WorldY: Double): Boolean;
begin
  WorldToScreen(@WorldX, @WorldY);

  Result := FRendererBase.InBox(Trunc(WorldX), Trunc(WorldY));
end;

function TAgg2D.InBox(World: TPointDouble): Boolean;
begin
  WorldToScreen(World.x, World.y);

  Result := FRendererBase.InBox(Trunc(World.x), Trunc(World.y));
end;

procedure TAgg2D.SetBlendMode(Value: TAggBlendMode);
begin
  FBlendMode := Value;

  FPixelFormatComp.BlendMode := Value;
  FPixelFormatCompPre.BlendMode := Value;
end;

procedure TAgg2D.SetImageBlendMode(Value: TAggBlendMode);
begin
  FImageBlendMode := Value;
end;

procedure TAgg2D.SetImageBlendColor(c: TAggColorRgba8);
begin
  FImageBlendColor := c;
end;

procedure TAgg2D.SetImageBlendColor(r, g, b: Cardinal; a: Cardinal = 255);
var
  Clr: TAggColorRgba8;
begin
  Clr.Initialize(r, g, b, a);
  SetImageBlendColor(Clr);
end;

procedure TAgg2D.SetMasterAlpha(a: Double);
begin
  FMasterAlpha := a;

  UpdateRasterizerGamma;
end;

procedure TAgg2D.SetAntiAliasGamma(g: Double);
begin
  FAntiAliasGamma := g;

  UpdateRasterizerGamma;
end;

procedure TAgg2D.SetFillColor(c: TAggColorRgba8);
begin
  FFillColor := c;
  FFillGradientFlag := grdSolid;
end;

procedure TAgg2D.SetFillColor(r, g, b: Cardinal; a: Cardinal = 255);
var
  Clr: TAggColorRgba8;
begin
  Clr.Initialize(r, g, b, a);
  SetFillColor(Clr);
end;

procedure TAgg2D.NoFill;
var
  Clr: TAggColorRgba8;
begin
  Clr.Initialize(0, 0, 0, 0);
  SetFillColor(Clr);
end;

procedure TAgg2D.SetLineColor(c: TAggColorRgba8);
begin
  FLineColor := c;
  FLineGradientFlag := grdSolid;
end;

procedure TAgg2D.SetLineColor(r, g, b: Cardinal; a: Cardinal = 255);
var
  Clr: TAggColorRgba8;
begin
  Clr.Initialize(r, g, b, a);
  SetLineColor(Clr);
end;

procedure TAgg2D.NoLine;
var
  Clr: TAggColorRgba8;
begin
  Clr.Initialize(0, 0, 0, 0);
  SetLineColor(Clr);
end;

procedure TAgg2D.FillLinearGradient(x1, y1, x2, y2: Double;
  c1, c2: TAggColorRgba8; Profile: Double = 1);
var
  i, StartGradient, StopGradient: Integer;
  k, angle: Double;
  c: TAggColorRgba8;
  Clr: TAggColor;
begin
  StartGradient := 128 - Trunc(Profile * 127);
  StopGradient := 128 + Trunc(Profile * 127);

  if StopGradient <= StartGradient then
      StopGradient := StartGradient + 1;

  k := 1 / (StopGradient - StartGradient);
  i := 0;

  while i < StartGradient do
    begin
      Clr.FromRgba8(c1);

      Move(Clr, FFillGradient[i]^, SizeOf(TAggColor));
      inc(i);
    end;

  while i < StopGradient do
    begin
      c := c1.Gradient(c2, (i - StartGradient) * k);

      Clr.FromRgba8(c);

      Move(Clr, FFillGradient[i]^, SizeOf(TAggColor));
      inc(i);
    end;

  while i < 256 do
    begin
      Clr.FromRgba8(c2);

      Move(Clr, FFillGradient[i]^, SizeOf(TAggColor));
      inc(i);
    end;

  angle := ArcTan2(y2 - y1, x2 - x1);

  FFillGradientMatrix.Reset;
  FFillGradientMatrix.Rotate(angle);
  FFillGradientMatrix.Translate(x1, y1);
  FFillGradientMatrix.Multiply(FTransform);
  FFillGradientMatrix.Invert;

  FFillGradientD1 := 0;
  FFillGradientD2 := Sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1));
  FFillGradientFlag := grdLinear;

  FFillColor.Initialize(0, 0, 0); // Set some real TAggColorRgba8
end;

procedure TAgg2D.LineLinearGradient(x1, y1, x2, y2: Double; c1, c2: TAggColorRgba8;
  Profile: Double = 1);
var
  i, StartGradient, StopGradient: Integer;
  k, angle: Double;
  c: TAggColorRgba8;
  Clr: TAggColor;
begin
  StartGradient := 128 - Trunc(Profile * 128);
  StopGradient := 128 + Trunc(Profile * 128);

  if StopGradient <= StartGradient then
      StopGradient := StartGradient + 1;

  k := 1 / (StopGradient - StartGradient);
  i := 0;

  while i < StartGradient do
    begin
      Clr.FromRgba8(c1);

      Move(Clr, FLineGradient[i]^, SizeOf(TAggColor));
      inc(i);
    end;

  while i < StopGradient do
    begin
      c := c1.Gradient(c2, (i - StartGradient) * k);

      Clr.FromRgba8(c);

      Move(Clr, FLineGradient[i]^, SizeOf(TAggColor));
      inc(i);
    end;

  while i < 256 do
    begin
      Clr.FromRgba8(c2);

      Move(Clr, FLineGradient[i]^, SizeOf(TAggColor));
      inc(i);
    end;

  angle := ArcTan2(y2 - y1, x2 - x1);

  FLineGradientMatrix.Reset;
  FLineGradientMatrix.Rotate(angle);
  FLineGradientMatrix.Translate(x1, y1);
  FLineGradientMatrix.Multiply(FTransform); { ! }
  FLineGradientMatrix.Invert;

  FLineGradientD1 := 0;
  FLineGradientD2 := Hypot((x2 - x1), (y2 - y1));
  FLineGradientFlag := grdLinear;
end;

procedure TAgg2D.FillRadialGradient(x, y, r: Double; c1, c2: TAggColorRgba8;
  Profile: Double = 1);
var
  i, StartGradient, StopGradient: Integer;

  k: Double;
  c: TAggColorRgba8;

  Clr: TAggColor;
begin
  StartGradient := 128 - Trunc(Profile * 127);
  StopGradient := 128 + Trunc(Profile * 127);

  if StopGradient <= StartGradient then
      StopGradient := StartGradient + 1;

  k := 1 / (StopGradient - StartGradient);
  i := 0;

  while i < StartGradient do
    begin
      Clr.FromRgba8(c1);

      Move(Clr, FFillGradient[i]^, SizeOf(TAggColor));
      inc(i);
    end;

  while i < StopGradient do
    begin
      c := c1.Gradient(c2, (i - StartGradient) * k);

      Clr.FromRgba8(c);

      Move(Clr, FFillGradient[i]^, SizeOf(TAggColor));
      inc(i);
    end;

  while i < 256 do
    begin
      Clr.FromRgba8(c2);

      Move(Clr, FFillGradient[i]^, SizeOf(TAggColor));
      inc(i);
    end;

  FFillGradientD2 := WorldToScreen(r);

  WorldToScreen(@x, @y);

  FFillGradientMatrix.Reset;
  FFillGradientMatrix.Translate(x, y);
  FFillGradientMatrix.Invert;

  FFillGradientD1 := 0;
  FFillGradientFlag := grdRadial;

  FFillColor.Initialize(0, 0, 0); // Set some real TAggColorRgba8
end;

procedure TAgg2D.LineRadialGradient(x, y, r: Double; c1, c2: TAggColorRgba8;
  Profile: Double = 1);
var
  i, StartGradient, StopGradient: Integer;
  k: Double;
  c: TAggColorRgba8;
  Clr: TAggColor;
begin
  StartGradient := 128 - Trunc(Profile * 128);
  StopGradient := 128 + Trunc(Profile * 128);

  if StopGradient <= StartGradient then
      StopGradient := StartGradient + 1;

  k := 1 / (StopGradient - StartGradient);
  i := 0;

  while i < StartGradient do
    begin
      Clr.FromRgba8(c1);

      Move(Clr, FLineGradient[i]^, SizeOf(TAggColor));
      inc(i);
    end;

  while i < StopGradient do
    begin
      c := c1.Gradient(c2, (i - StartGradient) * k);

      Clr.FromRgba8(c);

      Move(Clr, FLineGradient[i]^, SizeOf(TAggColor));
      inc(i);
    end;

  while i < 256 do
    begin
      Clr.FromRgba8(c2);

      Move(Clr, FLineGradient[i]^, SizeOf(TAggColor));
      inc(i);
    end;

  FLineGradientD2 := WorldToScreen(r);

  WorldToScreen(x, y);

  FLineGradientMatrix.Reset;
  FLineGradientMatrix.Translate(x, y);
  FLineGradientMatrix.Invert;

  FLineGradientD1 := 0;
  FLineGradientFlag := grdRadial;
end;

procedure TAgg2D.FillRadialGradient(x, y, r: Double; c1, c2, c3: TAggColorRgba8);
var
  i: Integer;
  c: TAggColorRgba8;
  Clr: TAggColor;
begin
  i := 0;

  while i < 128 do
    begin
      c := c1.Gradient(c2, i / 127);

      Clr.FromRgba8(c);

      Move(Clr, FFillGradient[i]^, SizeOf(TAggColor));
      inc(i);
    end;

  while i < 256 do
    begin
      c := c2.Gradient(c3, (i - 128) / 127);

      Clr.FromRgba8(c);

      Move(Clr, FFillGradient[i]^, SizeOf(TAggColor));
      inc(i);
    end;

  FFillGradientD2 := WorldToScreen(r);

  WorldToScreen(@x, @y);

  FFillGradientMatrix.Reset;
  FFillGradientMatrix.Translate(x, y);
  FFillGradientMatrix.Invert;

  FFillGradientD1 := 0;
  FFillGradientFlag := grdRadial;

  FFillColor.Initialize(0, 0, 0); // Set some real TAggColorRgba8
end;

procedure TAgg2D.LineRadialGradient(x, y, r: Double; c1, c2, c3: TAggColorRgba8);
var
  i: Integer;
  c: TAggColorRgba8;
  Clr: TAggColor;
begin
  i := 0;

  while i < 128 do
    begin
      c := c1.Gradient(c2, i / 127);

      Clr.FromRgba8(c);

      Move(Clr, FLineGradient[i]^, SizeOf(TAggColor));
      inc(i);
    end;

  while i < 256 do
    begin
      c := c2.Gradient(c3, (i - 128) / 127);

      Clr.FromRgba8(c);

      Move(Clr, FLineGradient[i]^, SizeOf(TAggColor));
      inc(i);
    end;

  FLineGradientD2 := WorldToScreen(r);

  WorldToScreen(@x, @y);

  FLineGradientMatrix.Reset;
  FLineGradientMatrix.Translate(x, y);
  FLineGradientMatrix.Invert;

  FLineGradientD1 := 0;
  FLineGradientFlag := grdRadial;
end;

procedure TAgg2D.FillRadialGradient(x, y, r: Double);
begin
  FFillGradientD2 := WorldToScreen(r);

  WorldToScreen(@x, @y);

  FFillGradientMatrix.Reset;
  FFillGradientMatrix.Translate(x, y);
  FFillGradientMatrix.Invert;

  FFillGradientD1 := 0;
end;

procedure TAgg2D.LineRadialGradient(x, y, r: Double);
begin
  FLineGradientD2 := WorldToScreen(r);

  WorldToScreen(@x, @y);

  FLineGradientMatrix.Reset;
  FLineGradientMatrix.Translate(x, y);
  FLineGradientMatrix.Invert;

  FLineGradientD1 := 0;
end;

procedure TAgg2D.SetLineWidth(w: Double);
begin
  FLineWidth := w;
  FConvStroke.width := w;
end;

procedure TAgg2D.SetLineCap(Cap: TAggLineCap);
begin
  FLineCap := Cap;
  FConvStroke.LineCap := Cap;
end;

procedure TAgg2D.SetLineJoin(Join: TAggLineJoin);
begin
  FLineJoin := Join;

  FConvStroke.LineJoin := Join;
end;

procedure TAgg2D.SetFillEvenOdd(EvenOddFlag: Boolean);
begin
  FEvenOddFlag := EvenOddFlag;

  if EvenOddFlag then
      FRasterizer.FillingRule := frEvenOdd
  else
      FRasterizer.FillingRule := frNonZero;
end;

function TAgg2D.GetTransformations: TAggTransformations;
begin
  FTransform.StoreTo(@Result.AffineMatrix[0]);
end;

procedure TAgg2D.SetTransformations(var tr: TAggTransformations);
begin
  FTransform.LoadFrom(@tr.AffineMatrix[0]);
  UpdateApproximationScale;
end;

procedure TAgg2D.SetTransformations(v0, v1, v2, v3, v4, V5: Double);
var
  M: TAggParallelogram;
begin
  M[0] := v0;
  M[1] := v1;
  M[2] := v2;
  M[3] := v3;
  M[4] := v4;
  M[5] := V5;

  FTransform.LoadFrom(@M);
  UpdateApproximationScale;
end;

procedure TAgg2D.ResetTransformations;
begin
  FTransform.Reset;
end;

procedure TAgg2D.Affine(tr: TAggTransAffine);
begin
  FTransform.Multiply(tr);
  UpdateApproximationScale;
end;

procedure TAgg2D.Affine(var tr: TAggTransformations);
var
  TA: TAggTransAffine;
begin
  TA := TAggTransAffine.Create(tr.AffineMatrix[0], tr.AffineMatrix[1],
    tr.AffineMatrix[2], tr.AffineMatrix[3], tr.AffineMatrix[4],
    tr.AffineMatrix[5]);
  try
      Affine(TA);
  finally
      TA.Free;
  end;
end;

procedure TAgg2D.Rotate(angle: Double);
begin
  FTransform.Rotate(angle);
end;

procedure TAgg2D.Scale(SX, SY: Double);
begin
  FTransform.Scale(SX, SY);
  UpdateApproximationScale;
end;

procedure TAgg2D.Skew(SX, SY: Double);
var
  Tas: TAggTransAffineSkewing;
begin
  Tas := TAggTransAffineSkewing.Create(SX, SY);
  try
      FTransform.Multiply(Tas);
  finally
      Tas.Free;
  end;
end;

procedure TAgg2D.Translate(x, y: Double);
begin
  FTransform.Translate(x, y);
end;

procedure TAgg2D.Parallelogram(x1, y1, x2, y2: Double; Para: PDouble);
var
  TA: TAggTransAffine;
begin
  TA := TAggTransAffine.Create(x1, y1, x2, y2, PAggParallelogram(Para));
  try
      FTransform.Multiply(TA);
  finally
      TA.Free;
  end;

  UpdateApproximationScale;
end;

procedure TAgg2D.viewport(WorldX1, WorldY1, WorldX2, WorldY2, ScreenX1, ScreenY1,
  ScreenX2, ScreenY2: Double; opt: TAggViewportOption = voXMidYMid);
var
  VP: TAggTransViewport;
  mx: TAggTransAffine;
begin
  VP := TAggTransViewport.Create;
  try
    case opt of
      voAnisotropic: VP.PreserveAspectRatio(0, 0, arStretch);
      voXMinYMin: VP.PreserveAspectRatio(0, 0, arMeet);
      voXMidYMin: VP.PreserveAspectRatio(0.5, 0, arMeet);
      voXMaxYMin: VP.PreserveAspectRatio(1, 0, arMeet);
      voXMinYMid: VP.PreserveAspectRatio(0, 0.5, arMeet);
      voXMidYMid: VP.PreserveAspectRatio(0.5, 0.5, arMeet);
      voXMaxYMid: VP.PreserveAspectRatio(1, 0.5, arMeet);
      voXMinYMax: VP.PreserveAspectRatio(0, 1, arMeet);
      voXMidYMax: VP.PreserveAspectRatio(0.5, 1, arMeet);
      voXMaxYMax: VP.PreserveAspectRatio(1, 1, arMeet);
    end;

    VP.WorldViewport(WorldX1, WorldY1, WorldX2, WorldY2);
    VP.DeviceViewport(ScreenX1, ScreenY1, ScreenX2, ScreenY2);

    mx := TAggTransAffine.Create;
    try
      VP.ToAffine(mx);
      FTransform.Multiply(mx);
    finally
        mx.Free;
    end;
  finally
      VP.Free;
  end;

  UpdateApproximationScale;
end;

procedure TAgg2D.viewport(World, Screen: TRectDouble; opt: TAggViewportOption);
var
  VP: TAggTransViewport;
  mx: TAggTransAffine;
begin
  VP := TAggTransViewport.Create;
  try
    case opt of
      voAnisotropic: VP.PreserveAspectRatio(0, 0, arStretch);
      voXMinYMin: VP.PreserveAspectRatio(0, 0, arMeet);
      voXMidYMin: VP.PreserveAspectRatio(0.5, 0, arMeet);
      voXMaxYMin: VP.PreserveAspectRatio(1, 0, arMeet);
      voXMinYMid: VP.PreserveAspectRatio(0, 0.5, arMeet);
      voXMidYMid: VP.PreserveAspectRatio(0.5, 0.5, arMeet);
      voXMaxYMid: VP.PreserveAspectRatio(1, 0.5, arMeet);
      voXMinYMax: VP.PreserveAspectRatio(0, 1, arMeet);
      voXMidYMax: VP.PreserveAspectRatio(0.5, 1, arMeet);
      voXMaxYMax: VP.PreserveAspectRatio(1, 1, arMeet);
    end;

    VP.WorldViewport(World);
    VP.DeviceViewport(Screen);

    mx := TAggTransAffine.Create;
    try
      VP.ToAffine(mx);
      FTransform.Multiply(mx);
    finally
        mx.Free;
    end;
  finally
      VP.Free;
  end;

  UpdateApproximationScale;
end;

procedure TAgg2D.Line(x1, y1, x2, y2: Double);
begin
  FPath.RemoveAll;

  AddLine(x1, y1, x2, y2);
  DrawPath(dpfStrokeOnly);
end;

procedure TAgg2D.Triangle(x1, y1, x2, y2, x3, y3: Double);
begin
  FPath.RemoveAll;
  FPath.MoveTo(x1, y1);
  FPath.LineTo(x2, y2);
  FPath.LineTo(x3, y3);
  FPath.ClosePolygon;

  DrawPath(dpfFillAndStroke);
end;

procedure TAgg2D.Rectangle(x1, y1, x2, y2: Double);
begin
  FPath.RemoveAll;
  FPath.MoveTo(x1, y1);
  FPath.LineTo(x2, y1);
  FPath.LineTo(x2, y2);
  FPath.LineTo(x1, y2);
  FPath.ClosePolygon;

  DrawPath(dpfFillAndStroke);
end;

procedure TAgg2D.RemoveAllDashes;
begin
  FConvDash.RemoveAllDashes;
  FConvStroke.Source := FConvCurve;
end;

procedure TAgg2D.RoundedRect(x1, y1, x2, y2, r: Double);
var
  RC: TAggRoundedRect;
begin
  FPath.RemoveAll;
  RC := TAggRoundedRect.Create(x1, y1, x2, y2, r);
  try
    RC.NormalizeRadius;
    RC.ApproximationScale := WorldToScreen(1) * GApproxScale;

    FPath.AddPath(RC, 0, False);
  finally
      RC.Free;
  end;

  DrawPath(dpfFillAndStroke);
end;

procedure TAgg2D.RoundedRect(Rect: TRectDouble; r: Double);
var
  RC: TAggRoundedRect;
begin
  FPath.RemoveAll;
  RC := TAggRoundedRect.Create(Rect.x1, Rect.y1, Rect.x2, Rect.y2, r);
  try
    RC.NormalizeRadius;
    RC.ApproximationScale := WorldToScreen(1) * GApproxScale;

    FPath.AddPath(RC, 0, False);
  finally
      RC.Free;
  end;

  DrawPath(dpfFillAndStroke);
end;

procedure TAgg2D.RoundedRect(x1, y1, x2, y2, RX, RY: Double);
var
  RC: TAggRoundedRect;
begin
  FPath.RemoveAll;
  RC := TAggRoundedRect.Create;
  try
    RC.Rect(x1, y1, x2, y2);
    RC.radius(RX, RY);
    RC.NormalizeRadius;

    FPath.AddPath(RC, 0, False);
  finally
      RC.Free;
  end;

  DrawPath(dpfFillAndStroke);
end;

procedure TAgg2D.RoundedRect(Rect: TRectDouble; RX, RY: Double);
var
  RC: TAggRoundedRect;
begin
  FPath.RemoveAll;
  RC := TAggRoundedRect.Create;
  try
    RC.Rect(Rect.x1, Rect.y1, Rect.x2, Rect.y2);
    RC.radius(RX, RY);
    RC.NormalizeRadius;

    FPath.AddPath(RC, 0, False);
  finally
      RC.Free;
  end;

  DrawPath(dpfFillAndStroke);
end;

procedure TAgg2D.RoundedRect(x1, y1, x2, y2, RxBottom, RyBottom, RxTop, RyTop: Double);
var
  RC: TAggRoundedRect;
begin
  FPath.RemoveAll;
  RC := TAggRoundedRect.Create;
  try
    RC.Rect(x1, y1, x2, y2);
    RC.radius(RxBottom, RyBottom, RxTop, RyTop);
    RC.NormalizeRadius;

    RC.ApproximationScale := WorldToScreen(1) * GApproxScale;

    FPath.AddPath(RC, 0, False);
  finally
      RC.Free;
  end;

  DrawPath(dpfFillAndStroke);
end;

procedure TAgg2D.Ellipse(Cx, Cy, RX, RY: Double);
var
  El: TAggBezierArc;
begin
  FPath.RemoveAll;

  El := TAggBezierArc.Create(Cx, Cy, RX, RY, 0, 2 * pi);
  try
      FPath.AddPath(El, 0, False);
  finally
      El.Free;
  end;

  FPath.ClosePolygon;

  DrawPath(dpfFillAndStroke);
end;

procedure TAgg2D.Circle(Cx, Cy, radius: Double);
var
  El: TAggBezierArc;
begin
  FPath.RemoveAll;

  El := TAggBezierArc.Create(Cx, Cy, radius, radius, 0, 2 * pi);
  try
      FPath.AddPath(El, 0, False);
  finally
      El.Free;
  end;

  FPath.ClosePolygon;

  DrawPath(dpfFillAndStroke);
end;

procedure TAgg2D.Arc(Cx, Cy, RX, RY, Start, Sweep: Double);
var
  ar: TAggArc;
begin
  FPath.RemoveAll;

  ar := TAggArc.Create(Cx, Cy, RX, RY, Start, Sweep, False);
  try
      FPath.AddPath(ar, 0, False);
  finally
      ar.Free;
  end;

  DrawPath(dpfStrokeOnly);
end;

procedure TAgg2D.Star(Cx, Cy, r1, r2, startAngle: Double; NumRays: Integer);
var
  DA, a, x, y: Double;
  i: Integer;
begin
  FPath.RemoveAll;

  DA := pi / NumRays;
  a := startAngle;

  i := 0;

  while i < NumRays do
    begin
      SinCosScale(a, y, x, r2);
      x := x + Cx;
      y := y + Cy;

      if i <> 0 then
          FPath.LineTo(x, y)
      else
          FPath.MoveTo(x, y);

      a := a + DA;

      SinCosScale(a, y, x, r1);
      FPath.LineTo(x + Cx, y + Cy);

      a := a + DA;

      inc(i);
    end;

  ClosePolygon;
  DrawPath(dpfFillAndStroke);
end;

procedure TAgg2D.Curve(x1, y1, x2, y2, x3, y3: Double);
begin
  FPath.RemoveAll;
  FPath.MoveTo(x1, y1);
  FPath.Curve3(x2, y2, x3, y3);

  DrawPath(dpfStrokeOnly);
end;

procedure TAgg2D.Curve(x1, y1, x2, y2, x3, y3, x4, y4: Double);
begin
  FPath.RemoveAll;
  FPath.MoveTo(x1, y1);
  FPath.Curve4(x2, y2, x3, y3, x4, y4);

  DrawPath(dpfStrokeOnly);
end;

procedure TAgg2D.Polygon(xy: PPointDouble; NumPoints: Integer;
  flag: TAggDrawPathFlag = dpfFillAndStroke);
begin
  FPath.RemoveAll;
  FPath.AddPoly(xy, NumPoints);

  ClosePolygon;
  DrawPath(flag);
end;

procedure TAgg2D.Polyline(xy: PPointDouble; NumPoints: Integer);
begin
  FPath.RemoveAll;
  FPath.AddPoly(xy, NumPoints);

  DrawPath(dpfStrokeOnly);
end;

function TAgg2D.GetRow(y: Cardinal): PInt8u;
begin
  Result := FRenderingBuffer.Row(y);
end;

procedure TAgg2D.ResetPath;
begin
  FPath.RemoveAll;
end;

procedure TAgg2D.MoveTo(x, y: Double);
begin
  FPath.MoveTo(x, y);
end;

procedure TAgg2D.MoveRel(dx, dy: Double);
begin
  FPath.MoveRelative(dx, dy);
end;

procedure TAgg2D.LineTo(x, y: Double);
begin
  FPath.LineTo(x, y);
end;

procedure TAgg2D.LineRel(dx, dy: Double);
begin
  FPath.LineRelative(dx, dy);
end;

procedure TAgg2D.HorizontalLineTo(x: Double);
begin
  FPath.HorizontalLineTo(x);
end;

procedure TAgg2D.HorizontalLineRel(dx: Double);
begin
  FPath.HorizontalLineRelative(dx);
end;

procedure TAgg2D.VerticalLineTo(y: Double);
begin
  FPath.VerticalLineTo(y);
end;

procedure TAgg2D.VerticalLineRel(dy: Double);
begin
  FPath.VerticalLineRelative(dy);
end;

procedure TAgg2D.ArcTo(RX, RY, angle: Double; LargeArcFlag, SweepFlag: Boolean; x, y: Double);
begin
  FPath.ArcTo(RX, RY, angle, LargeArcFlag, SweepFlag, x, y);
end;

procedure TAgg2D.ArcRel(RX, RY, angle: Double; LargeArcFlag, SweepFlag: Boolean; dx, dy: Double);
begin
  FPath.ArcRelative(RX, RY, angle, LargeArcFlag, SweepFlag, dx, dy);
end;

procedure TAgg2D.QuadricCurveTo(XCtrl, YCtrl, XTo, YTo: Double);
begin
  FPath.Curve3(XCtrl, YCtrl, XTo, YTo);
end;

procedure TAgg2D.QuadricCurveRel(DxCtrl, DyCtrl, DxTo, DyTo: Double);
begin
  FPath.Curve3Relative(DxCtrl, DyCtrl, DxTo, DyTo);
end;

procedure TAgg2D.QuadricCurveTo(XTo, YTo: Double);
begin
  FPath.Curve3(XTo, YTo);
end;

procedure TAgg2D.QuadricCurveRel(DxTo, DyTo: Double);
begin
  FPath.Curve3Relative(DxTo, DyTo);
end;

procedure TAgg2D.CubicCurveTo(XCtrl1, YCtrl1, XCtrl2, YCtrl2, XTo, YTo: Double);
begin
  FPath.Curve4(XCtrl1, YCtrl1, XCtrl2, YCtrl2, XTo, YTo);
end;

procedure TAgg2D.CubicCurveRel(DxCtrl1, DyCtrl1, DxCtrl2, DyCtrl2, DxTo, DyTo: Double);
begin
  FPath.Curve4Relative(DxCtrl1, DyCtrl1, DxCtrl2, DyCtrl2, DxTo, DyTo);
end;

procedure TAgg2D.CubicCurveTo(XCtrl2, YCtrl2, XTo, YTo: Double);
begin
  FPath.Curve4(XCtrl2, YCtrl2, XTo, YTo);
end;

procedure TAgg2D.CubicCurveRel(XCtrl2, YCtrl2, XTo, YTo: Double);
begin
  FPath.Curve4Relative(XCtrl2, YCtrl2, XTo, YTo);
end;

procedure TAgg2D.AddDash(DashLength, GapLength: Double);
begin
  FConvDash.AddDash(DashLength, GapLength);
  FConvStroke.Source := FConvDash;
end;

procedure TAgg2D.AddEllipse(Cx, Cy, RX, RY: Double; dir: TAggDirection);
var
  ar: TAggBezierArc;
begin
  if dir = dirCCW then
      ar := TAggBezierArc.Create(Cx, Cy, RX, RY, 0, 2 * pi)
  else
      ar := TAggBezierArc.Create(Cx, Cy, RX, RY, 0, -2 * pi);
  try
      FPath.AddPath(ar, 0, False);
  finally
      ar.Free;
  end;

  FPath.ClosePolygon;
end;

procedure TAgg2D.AddEllipse(center, radius: TPointDouble; dir: TAggDirection);
var
  ar: TAggBezierArc;
begin
  if dir = dirCCW then
      ar := TAggBezierArc.Create(center.x, center.y, radius, 0, 2 * pi)
  else
      ar := TAggBezierArc.Create(center.x, center.y, radius, 0, -2 * pi);
  try
      FPath.AddPath(ar, 0, False);
  finally
      ar.Free;
  end;

  FPath.ClosePolygon;
end;

procedure TAgg2D.ClosePolygon;
begin
  FPath.ClosePolygon;
end;

procedure TAgg2D.DrawPath(flag: TAggDrawPathFlag = dpfFillAndStroke);
begin
  FRasterizer.Reset;

  case flag of
    dpfFillOnly:
      if FFillColor.a <> 0 then
        begin
          FRasterizer.AddPath(FPathTransform);

          Render(True);
        end;

    dpfStrokeOnly:
      if (FLineColor.a <> 0) and (FLineWidth > 0) then
        begin
          FRasterizer.AddPath(FStrokeTransform);

          Render(False);
        end;

    dpfFillAndStroke:
      begin
        if FFillColor.a <> 0 then
          begin
            FRasterizer.AddPath(FPathTransform);

            Render(True);
          end;

        if (FLineColor.a <> 0) and (FLineWidth > 0) then
          begin
            FRasterizer.AddPath(FStrokeTransform);

            Render(False);
          end;
      end;

    dpfFillWithHorizontalLineColor:
      if FLineColor.a <> 0 then
        begin
          FRasterizer.AddPath(FPathTransform);

          Render(False);
        end;
  end;
end;

procedure TAgg2D.DrawPathNoTransform(flag: TAggDrawPathFlag = dpfFillAndStroke);
begin
end;

procedure TAgg2D.SetImageFilter(f: TAggImageFilterType);
begin
  FImageFilter := f;
  case f of
    ifBilinear: FImageFilterLUT.Calculate(FImageFilterBilinear, True);
    ifHanning: FImageFilterLUT.Calculate(FImageFilterHanning, True);
    ifHermite: FImageFilterLUT.Calculate(FImageFilterHermite, True);
    ifQuadric: FImageFilterLUT.Calculate(FImageFilterQuadric, True);
    ifBicubic: FImageFilterLUT.Calculate(FImageFilterBicubic, True);
    ifCatrom: FImageFilterLUT.Calculate(FImageFilterCatrom, True);
    ifSpline16: FImageFilterLUT.Calculate(FImageFilterSpline16, True);
    ifSpline36: FImageFilterLUT.Calculate(FImageFilterSpline36, True);
    ifBlackman144: FImageFilterLUT.Calculate(FImageFilterBlackman144, True);
  end;
end;

procedure TAgg2D.SetImageResample(f: TAggImageResample);
begin
  FImageResample := f;
end;

procedure TAgg2D.TransformImage(img: TAgg2DImage; ImgX1, ImgY1, ImgX2,
  ImgY2: Integer; DstX1, DstY1, DstX2, DstY2: Double);
var
  Parall: TAggParallelogram;
begin
  ResetPath;
  MoveTo(DstX1, DstY1);
  LineTo(DstX2, DstY1);
  LineTo(DstX2, DstY2);
  LineTo(DstX1, DstY2);
  ClosePolygon;

  Parall[0] := DstX1;
  Parall[1] := DstY1;
  Parall[2] := DstX2;
  Parall[3] := DstY1;
  Parall[4] := DstX2;
  Parall[5] := DstY2;

  RenderImage(img, ImgX1, ImgY1, ImgX2, ImgY2, @Parall[0]);
end;

procedure TAgg2D.TransformImage(img: TAgg2DImage;
  DstX1, DstY1, DstX2, DstY2: Double);
var
  Parall: TAggParallelogram;
begin
  ResetPath;
  MoveTo(DstX1, DstY1);
  LineTo(DstX2, DstY1);
  LineTo(DstX2, DstY2);
  LineTo(DstX1, DstY2);
  ClosePolygon;

  Parall[0] := DstX1;
  Parall[1] := DstY1;
  Parall[2] := DstX2;
  Parall[3] := DstY1;
  Parall[4] := DstX2;
  Parall[5] := DstY2;

  RenderImage(img, 0, 0, img.FRenderingBuffer.width,
    img.FRenderingBuffer.height, @Parall[0]);
end;

procedure TAgg2D.TransformImage(img: TAgg2DImage; Destination: TRectDouble);
var
  Parall: TAggParallelogram;
begin
  ResetPath;
  MoveTo(Destination.x1, Destination.y1);
  LineTo(Destination.x2, Destination.y1);
  LineTo(Destination.x2, Destination.y2);
  LineTo(Destination.x1, Destination.y2);
  ClosePolygon;

  Parall[0] := Destination.x1;
  Parall[1] := Destination.y1;
  Parall[2] := Destination.x2;
  Parall[3] := Destination.y1;
  Parall[4] := Destination.x2;
  Parall[5] := Destination.y2;

  RenderImage(img, 0, 0, img.FRenderingBuffer.width,
    img.FRenderingBuffer.height, @Parall[0]);
end;

procedure TAgg2D.TransformImage(img: TAgg2DImage; ImgX1, ImgY1, ImgX2, ImgY2: Integer; Parallelogram: PDouble);
begin
  ResetPath;

  MoveTo(PDouble(PtrComp(Parallelogram))^,
    PDouble(PtrComp(Parallelogram) + SizeOf(Double))^);

  LineTo(PDouble(PtrComp(Parallelogram) + 2 * SizeOf(Double))^,
    PDouble(PtrComp(Parallelogram) + 3 * SizeOf(Double))^);

  LineTo(PDouble(PtrComp(Parallelogram) + 4 * SizeOf(Double))^,
    PDouble(PtrComp(Parallelogram) + 5 * SizeOf(Double))^);

  LineTo(PDouble(PtrComp(Parallelogram))^ +
    PDouble(PtrComp(Parallelogram) + 4 * SizeOf(Double))^ -
    PDouble(PtrComp(Parallelogram) + 2 * SizeOf(Double))^,
    PDouble(PtrComp(Parallelogram) + SizeOf(Double))^ +
    PDouble(PtrComp(Parallelogram) + 5 * SizeOf(Double))^ -
    PDouble(PtrComp(Parallelogram) + 3 * SizeOf(Double))^);

  ClosePolygon;

  RenderImage(img, ImgX1, ImgY1, ImgX2, ImgY2, Parallelogram);
end;

procedure TAgg2D.TransformImage(img: TAgg2DImage; ImgRect: TRectInteger;
  Parallelogram: PDouble);
begin
  ResetPath;

  MoveTo(PDouble(PtrComp(Parallelogram))^,
    PDouble(PtrComp(Parallelogram) + SizeOf(Double))^);

  LineTo(PDouble(PtrComp(Parallelogram) + 2 * SizeOf(Double))^,
    PDouble(PtrComp(Parallelogram) + 3 * SizeOf(Double))^);

  LineTo(PDouble(PtrComp(Parallelogram) + 4 * SizeOf(Double))^,
    PDouble(PtrComp(Parallelogram) + 5 * SizeOf(Double))^);

  LineTo(PDouble(PtrComp(Parallelogram))^ +
    PDouble(PtrComp(Parallelogram) + 4 * SizeOf(Double))^ -
    PDouble(PtrComp(Parallelogram) + 2 * SizeOf(Double))^,
    PDouble(PtrComp(Parallelogram) + SizeOf(Double))^ +
    PDouble(PtrComp(Parallelogram) + 5 * SizeOf(Double))^ -
    PDouble(PtrComp(Parallelogram) + 3 * SizeOf(Double))^);

  ClosePolygon;

  RenderImage(img, ImgRect, Parallelogram);
end;

procedure TAgg2D.TransformImage(img: TAgg2DImage; Parallelogram: PDouble);
begin
  ResetPath;

  MoveTo(PDouble(Parallelogram)^, PDouble(PtrComp(Parallelogram) +
    SizeOf(Double))^);

  LineTo(PDouble(PtrComp(Parallelogram) + 2 * SizeOf(Double))^,
    PDouble(PtrComp(Parallelogram) + 3 * SizeOf(Double))^);

  LineTo(PDouble(PtrComp(Parallelogram) + 4 * SizeOf(Double))^,
    PDouble(PtrComp(Parallelogram) + 5 * SizeOf(Double))^);

  LineTo(PDouble(Parallelogram)^ +
    PDouble(PtrComp(Parallelogram) + 4 * SizeOf(Double))^ -
    PDouble(PtrComp(Parallelogram) + 2 * SizeOf(Double))^,
    PDouble(PtrComp(Parallelogram) + SizeOf(Double))^ +
    PDouble(PtrComp(Parallelogram) + 5 * SizeOf(Double))^ -
    PDouble(PtrComp(Parallelogram) + 3 * SizeOf(Double))^);

  ClosePolygon;

  RenderImage(img, 0, 0, img.FRenderingBuffer.width,
    img.FRenderingBuffer.height, Parallelogram);
end;

procedure TAgg2D.TransformImage(img: TAgg2DImage; ImgRect: TRectInteger; DstX1, DstY1, DstX2, DstY2: Double);
var
  Parall: TAggParallelogram;
begin
  Parall[0] := DstX1;
  Parall[1] := DstY1;
  Parall[2] := DstX2;
  Parall[3] := DstY1;
  Parall[4] := DstX2;
  Parall[5] := DstY2;

  RenderImage(img, ImgRect, @Parall[0]);
end;

procedure TAgg2D.TransformImage(img: TAgg2DImage; ImgRect: TRectInteger; Destination: TRectDouble);
var
  Parall: TAggParallelogram;
begin
  Parall[0] := Destination.x1;
  Parall[1] := Destination.y1;
  Parall[2] := Destination.x2;
  Parall[3] := Destination.y1;
  Parall[4] := Destination.x2;
  Parall[5] := Destination.y2;

  RenderImage(img, ImgRect, @Parall[0]);
end;

procedure TAgg2D.TransformImagePath(img: TAgg2DImage; ImgX1, ImgY1, ImgX2, ImgY2: Integer; DstX1, DstY1, DstX2, DstY2: Double);
var
  Parall: TAggParallelogram;
begin
  Parall[0] := DstX1;
  Parall[1] := DstY1;
  Parall[2] := DstX2;
  Parall[3] := DstY1;
  Parall[4] := DstX2;
  Parall[5] := DstY2;

  RenderImage(img, ImgX1, ImgY1, ImgX2, ImgY2, @Parall[0]);
end;

procedure TAgg2D.TransformImagePath(img: TAgg2DImage; DstX1, DstY1, DstX2, DstY2: Double);
var
  Parall: TAggParallelogram;
begin
  Parall[0] := DstX1;
  Parall[1] := DstY1;
  Parall[2] := DstX2;
  Parall[3] := DstY1;
  Parall[4] := DstX2;
  Parall[5] := DstY2;

  RenderImage(img, 0, 0, img.FRenderingBuffer.width,
    img.FRenderingBuffer.height, @Parall[0]);
end;

procedure TAgg2D.TransformImagePath(img: TAgg2DImage; ImgX1, ImgY1, ImgX2, ImgY2: Integer; Parallelogram: PDouble);
begin
  RenderImage(img, ImgX1, ImgY1, ImgX2, ImgY2, Parallelogram);
end;

procedure TAgg2D.TransformImagePath(img: TAgg2DImage; Parallelogram: PDouble);
begin
  RenderImage(img, 0, 0, img.FRenderingBuffer.width,
    img.FRenderingBuffer.height, Parallelogram);
end;

procedure TAgg2D.BlendImage(img: TAgg2DImage; ImgX1, ImgY1, ImgX2, ImgY2: Integer; Dstx, Dsty: Double; alpha: Cardinal = 255);
var
  PixF: TAggPixelFormatProcessor;
  r: TRectInteger;
begin
  WorldToScreen(@Dstx, @Dsty);
  PixelFormatRgba32(PixF, img.FRenderingBuffer);
  r := RectInteger(ImgX1, ImgY1, ImgX2, ImgY2);

  if FBlendMode = bmAlpha then
      FRendererBasePre.BlendFrom(PixF, @r, Trunc(Dstx) - ImgX1,
      Trunc(Dsty) - ImgY1, alpha)
  else
      FRendererBaseCompPre.BlendFrom(PixF, @r, Trunc(Dstx) - ImgX1,
      Trunc(Dsty) - ImgY1, alpha);
end;

procedure TAgg2D.BlendImage(img: TAgg2DImage; Dstx, Dsty: Double; alpha: Cardinal = 255);
var
  PixF: TAggPixelFormatProcessor;
begin
  WorldToScreen(@Dstx, @Dsty);
  PixelFormatRgba32(PixF, img.FRenderingBuffer);

  FRendererBasePre.BlendFrom(PixF, nil, Trunc(Dstx), Trunc(Dsty), alpha);

  if FBlendMode = bmAlpha then
      FRendererBasePre.BlendFrom(PixF, nil, Trunc(Dstx), Trunc(Dsty), alpha)
  else
      FRendererBaseCompPre.BlendFrom(PixF, nil, Trunc(Dstx), Trunc(Dsty), alpha);
end;

procedure TAgg2D.CopyImage(img: TAgg2DImage; ImgX1, ImgY1, ImgX2, ImgY2: Integer; Dstx, Dsty: Double);
var
  r: TRectInteger;
begin
  WorldToScreen(@Dstx, @Dsty);
  r := RectInteger(ImgX1, ImgY1, ImgX2, ImgY2);

  FRendererBase.CopyFrom(img.FRenderingBuffer, @r, Trunc(Dstx) - ImgX1,
    Trunc(Dsty) - ImgY1);
end;

procedure TAgg2D.CopyImage(img: TAgg2DImage; ImgRect: TRectInteger; Destination: TPointDouble);
begin
  WorldToScreen(@Destination.x, @Destination.y);

  FRendererBase.CopyFrom(img.FRenderingBuffer, @ImgRect,
    Trunc(Destination.x) - ImgRect.x1, Trunc(Destination.y) - ImgRect.y1);
end;

procedure TAgg2D.CopyImage(img: TAgg2DImage; Dstx, Dsty: Double);
begin
  WorldToScreen(@Dstx, @Dsty);

  FRendererBase.CopyFrom(img.FRenderingBuffer, nil, Trunc(Dstx), Trunc(Dsty));
end;

procedure TAgg2D.CopyImage(img: TAgg2DImage; Destination: TPointDouble);
begin
  WorldToScreen(@Destination.x, @Destination.y);

  FRendererBase.CopyFrom(img.FRenderingBuffer, nil, Trunc(Destination.x),
    Trunc(Destination.y));
end;

procedure TAgg2D.Render(AFillColor: Boolean);
begin
  if FBlendMode = bmAlpha then
      Agg2DRendererRender(Self, FRendererBase, FRendererSolid, AFillColor)
  else
      Agg2DRendererRender(Self, FRendererBaseComp, FRendererSolidComp, AFillColor);
end;

procedure TAgg2D.AddLine(x1, y1, x2, y2: Double);
begin
  FPath.MoveTo(x1, y1);
  FPath.LineTo(x2, y2);
end;

procedure TAgg2D.UpdateApproximationScale;
begin
  FConvCurve.ApproximationScale := WorldToScreen(1) * GApproxScale;
  FConvStroke.ApproximationScale := WorldToScreen(1) * GApproxScale;
end;

procedure TAgg2D.UpdateRasterizerGamma;
begin
  if Assigned(FGammaAgg2D) then
      FGammaAgg2D.Free;

  FGammaAgg2D := TAgg2DRasterizerGamma.Create(FMasterAlpha, FAntiAliasGamma);
  FRasterizer.Gamma(FGammaAgg2D);
end;

procedure TAgg2D.RenderImage(img: TAgg2DImage; x1, y1, x2, y2: Integer; Parl: PDouble);
var
  Mtx: TAggTransAffine;
  Interpolator: TAggSpanInterpolatorLinear;
begin
  FRasterizer.Reset;
  FRasterizer.AddPath(FPathTransform);

  Mtx := TAggTransAffine.Create(x1, y1, x2, y2, PAggParallelogram(Parl));
  try
    Mtx.Multiply(FTransform);
    Mtx.Invert;

    Interpolator := TAggSpanInterpolatorLinear.Create(Mtx);
    try
      if FBlendMode = bmAlpha then
          Agg2DRendererRenderImage(Self, img, FRendererBasePre, Interpolator)
      else
          Agg2DRendererRenderImage(Self, img, FRendererBaseCompPre, Interpolator);
    finally
        Interpolator.Free;
    end;
  finally
      Mtx.Free;
  end;
end;

procedure TAgg2D.RenderImage(img: TAgg2DImage; Rect: TRectInteger; Parl: PDouble);
var
  Mtx: TAggTransAffine;
  Interpolator: TAggSpanInterpolatorLinear;
begin
  FRasterizer.Reset;
  FRasterizer.AddPath(FPathTransform);

  Mtx := TAggTransAffine.Create(Rect.x1, Rect.y1, Rect.x2, Rect.y2,
    PAggParallelogram(Parl));
  try
    Mtx.Multiply(FTransform);
    Mtx.Invert;

    Interpolator := TAggSpanInterpolatorLinear.Create(Mtx);
    try
      if FBlendMode = bmAlpha then
          Agg2DRendererRenderImage(Self, img, FRendererBasePre, Interpolator)
      else
          Agg2DRendererRenderImage(Self, img, FRendererBaseCompPre, Interpolator);
    finally
        Interpolator.Free;
    end;
  finally
      Mtx.Free;
  end;
end;

{ TAggSpanConvImageBlend }

constructor TAggSpanConvImageBlend.Create(BlendMode: TAggBlendMode; c: TAggColorRgba8; p: TAggPixelFormatProcessor);
begin
  FMode := BlendMode;
  FColor := c;
  FPixel := p;
end;

procedure TAggSpanConvImageBlend.Convert(Span: PAggColor; x, y: Integer; Len: Cardinal);
var
  l2, a: Cardinal;
  s2: PAggColorRgba8;
begin
  if FMode <> bmDestination then
    begin
      l2 := Len;
      s2 := PAggColorRgba8(Span);

      repeat
        BlendModeAdaptorClipToDestinationRgbaPre(FPixel, FMode,
          PInt8u(s2), FColor.r, FColor.g, FColor.b, CAggBaseMask, CAggCoverFull);

        inc(PtrComp(s2), SizeOf(TAggColorRgba8));
        dec(l2);
      until l2 = 0;
    end;

  if FColor.a < CAggBaseMask then
    begin
      l2 := Len;
      s2 := PAggColorRgba8(Span);
      a := FColor.a;

      repeat
        s2.r := (s2.r * a) shr CAggBaseShift;
        s2.g := (s2.g * a) shr CAggBaseShift;
        s2.b := (s2.b * a) shr CAggBaseShift;
        s2.a := (s2.a * a) shr CAggBaseShift;

        inc(PtrComp(s2), SizeOf(TAggColorRgba8));
        dec(l2);
      until l2 = 0;
    end;
end;

function OperatorIsEqual(c1, c2: PAggColorRgba8): Boolean;
begin
  Result := (c1.r = c2.r) and (c1.g = c2.g) and (c1.b = c2.b) and (c1.a = c2.a);
end;

function OperatorIsNotEqual(c1, c2: PAggColorRgba8): Boolean;
begin
  Result := not OperatorIsEqual(c1, c2);
end;

procedure Agg2DRendererRender(Gr: TAgg2D; RendererBase: TAggRendererBase; RenSolid: TAggRendererScanLineAASolid; FillColor: Boolean);
var
  Span: TAggSpanGradient;
  Ren: TAggRendererScanLineAA;
  Clr: TAggColor;
begin
  if (FillColor and (Gr.FFillGradientFlag = grdLinear)) or
    (not FillColor and (Gr.FLineGradientFlag = grdLinear)) then
    if FillColor then
      begin
        Span := TAggSpanGradient.Create(Gr.FAllocator,
          Gr.FFillGradientInterpolator, Gr.FLinearGradientFunction,
          Gr.FFillGradient, Gr.FFillGradientD1, Gr.FFillGradientD2);
        try
          Ren := TAggRendererScanLineAA.Create(RendererBase, Span);
          try
              RenderScanLines(Gr.FRasterizer, Gr.FScanLine, Ren);
          finally
              Ren.Free;
          end;
        finally
            Span.Free;
        end;
      end
    else
      begin
        Span := TAggSpanGradient.Create(Gr.FAllocator,
          Gr.FLineGradientInterpolator, Gr.FLinearGradientFunction,
          Gr.FLineGradient, Gr.FLineGradientD1, Gr.FLineGradientD2);
        try
          Ren := TAggRendererScanLineAA.Create(RendererBase, Span);
          try
              RenderScanLines(Gr.FRasterizer, Gr.FScanLine, Ren);
          finally
              Ren.Free;
          end;
        finally
            Span.Free;
        end;
      end
  else if (FillColor and (Gr.FFillGradientFlag = grdRadial)) or
    (not FillColor and (Gr.FLineGradientFlag = grdRadial)) then
    if FillColor then
      begin
        Span := TAggSpanGradient.Create(Gr.FAllocator,
          Gr.FFillGradientInterpolator, Gr.FRadialGradientFunction,
          Gr.FFillGradient, Gr.FFillGradientD1, Gr.FFillGradientD2);
        try
          Ren := TAggRendererScanLineAA.Create(RendererBase, Span);
          try
              RenderScanLines(Gr.FRasterizer, Gr.FScanLine, Ren);
          finally
              Ren.Free;
          end;
        finally
            Span.Free;
        end;
      end
    else
      begin
        Span := TAggSpanGradient.Create(Gr.FAllocator,
          Gr.FLineGradientInterpolator, Gr.FRadialGradientFunction,
          Gr.FLineGradient, Gr.FLineGradientD1, Gr.FLineGradientD2);
        try
          Ren := TAggRendererScanLineAA.Create(RendererBase, Span);
          try
              RenderScanLines(Gr.FRasterizer, Gr.FScanLine, Ren);
          finally
              Ren.Free;
          end;
        finally
            Span.Free;
        end;
      end
  else
    begin
      if FillColor then
          Clr.FromRgba8(Gr.FFillColor)
      else
          Clr.FromRgba8(Gr.FLineColor);

      RenSolid.SetColor(@Clr);
      RenderScanLines(Gr.FRasterizer, Gr.FScanLine, RenSolid);
    end;
end;

procedure Agg2DRendererRenderImage(Gr: TAgg2D; img: TAgg2DImage; RendererBase: TAggRendererBase; Interpolator: TAggSpanInterpolatorLinear);
var
  Blend: TAggSpanConvImageBlend;

  SI: TAggSpanImageFilterRgba;
  sg: TAggSpanImageFilterRgbaNN;
  SB: TAggSpanImageFilterRgbaBilinear;
  s2: TAggSpanImageFilterRgba2x2;
  SA: TAggSpanImageResampleRgbaAffine;
  sc: TAggSpanConverter;
  Ri: TAggRendererScanLineAA;
  Clr: TAggColor;
  Resample: Boolean;
  SX, SY: Double;
begin
  Blend := TAggSpanConvImageBlend.Create(Gr.FImageBlendMode,
    Gr.FImageBlendColor, Gr.FPixelFormatCompPre);
  try
    if Gr.FImageFilter = ifNoFilter then
      begin
        Clr.Clear;
        case Gr.FPixelFormat of
          pfRGBA: sg := TAggSpanImageFilterRgbaNN.Create(Gr.FAllocator, img.FRenderingBuffer, @Clr, Interpolator, CAggOrderRgba);
          pfBGRA: sg := TAggSpanImageFilterRgbaNN.Create(Gr.FAllocator, img.FRenderingBuffer, @Clr, Interpolator, CAggOrderBgra);
        end;
        try
          sc := TAggSpanConverter.Create(sg, Blend);
          try
            Ri := TAggRendererScanLineAA.Create(RendererBase, sc);
            try
                RenderScanLines(Gr.FRasterizer, Gr.FScanLine, Ri);
            finally
                Ri.Free;
            end;
          finally
              sc.Free;
          end;
        finally
            sg.Free;
        end;
      end
    else
      begin
        Resample := Gr.FImageResample = irAlways;

        if Gr.FImageResample = irOnZoomOut then
          begin
            Interpolator.Transformer.GetScalingAbs(SX, SY);

            if (SX > 1.125) or (SY > 1.125) then
                Resample := True;
          end;

        if Resample then
          begin
            Clr.Clear;
            case Gr.FPixelFormat of
              pfRGBA: SA := TAggSpanImageResampleRgbaAffine.Create(Gr.FAllocator, img.FRenderingBuffer, @Clr, Interpolator, Gr.FImageFilterLUT, CAggOrderRgba);
              pfBGRA: SA := TAggSpanImageResampleRgbaAffine.Create(Gr.FAllocator, img.FRenderingBuffer, @Clr, Interpolator, Gr.FImageFilterLUT, CAggOrderBgra);
            end;
            try
              sc := TAggSpanConverter.Create(SA, Blend);
              try
                Ri := TAggRendererScanLineAA.Create(RendererBase, sc);
                try
                    RenderScanLines(Gr.FRasterizer, Gr.FScanLine, Ri);
                finally
                    Ri.Free;
                end;
              finally
                  sc.Free;
              end;
            finally
                SA.Free;
            end;
          end
        else if Gr.FImageFilter = ifBilinear then
          begin
            Clr.Clear;
            case Gr.FPixelFormat of
              pfRGBA:
                SB := TAggSpanImageFilterRgbaBilinear.Create(Gr.FAllocator,
                  img.FRenderingBuffer, @Clr, Interpolator, CAggOrderRgba);
              pfBGRA:
                SB := TAggSpanImageFilterRgbaBilinear.Create(Gr.FAllocator,
                  img.FRenderingBuffer, @Clr, Interpolator, CAggOrderBgra);
            end;
            try
              sc := TAggSpanConverter.Create(SB, Blend);
              try
                Ri := TAggRendererScanLineAA.Create(RendererBase, sc);
                try
                    RenderScanLines(Gr.FRasterizer, Gr.FScanLine, Ri);
                finally
                    Ri.Free;
                end;
              finally
                  sc.Free;
              end;
            finally
                SB.Free;
            end;
          end
        else if Gr.FImageFilterLUT.Diameter = 2 then
          begin
            Clr.Clear;
            case Gr.FPixelFormat of
              pfRGBA:
                s2 := TAggSpanImageFilterRgba2x2.Create(Gr.FAllocator,
                  img.FRenderingBuffer, @Clr, Interpolator, Gr.FImageFilterLUT,
                  CAggOrderRgba);
              pfBGRA:
                s2 := TAggSpanImageFilterRgba2x2.Create(Gr.FAllocator,
                  img.FRenderingBuffer, @Clr, Interpolator, Gr.FImageFilterLUT,
                  CAggOrderBgra);
            end;
            try
              sc := TAggSpanConverter.Create(s2, Blend);
              try
                Ri := TAggRendererScanLineAA.Create(RendererBase, sc);
                try
                    RenderScanLines(Gr.FRasterizer, Gr.FScanLine, Ri);
                finally
                    Ri.Free;
                end;
              finally
                  sc.Free;
              end;
            finally
                s2.Free;
            end;
          end
        else
          begin
            Clr.Clear;
            case Gr.FPixelFormat of
              pfRGBA:
                SI := TAggSpanImageFilterRgba.Create(Gr.FAllocator,
                  img.FRenderingBuffer, @Clr, Interpolator, Gr.FImageFilterLUT,
                  CAggOrderRgba);
              pfBGRA:
                SI := TAggSpanImageFilterRgba.Create(Gr.FAllocator,
                  img.FRenderingBuffer, @Clr, Interpolator, Gr.FImageFilterLUT,
                  CAggOrderBgra);
            end;
            try
              sc := TAggSpanConverter.Create(SI, Blend);
              try
                Ri := TAggRendererScanLineAA.Create(RendererBase, sc);
                try
                    RenderScanLines(Gr.FRasterizer, Gr.FScanLine, Ri);
                finally
                    Ri.Free;
                end;
              finally
                  sc.Free;
              end;
            finally
                SI.Free;
            end;
          end;
      end;
  finally
      Blend.Free;
  end;
end;

end.
