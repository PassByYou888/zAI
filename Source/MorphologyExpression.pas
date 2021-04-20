{ ****************************************************************************** }
{ * Morphology Expression support                                              * }
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
unit MorphologyExpression;

{$INCLUDE zDefine.inc}

interface

uses
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  TypInfo, Variants,
  PascalStrings, CoreClasses, UnicodeMixedLib, ListEngine, DoStatusIO, TextParsing, DataFrameEngine, MemoryStream64,
  zExpression, OpCode, Geometry2DUnit, Geometry3DUnit, MemoryRaster, zDrawEngine;

type
  TMorphExpStep = class;
  TMorphExpSteps = class;
  TMorphExp = class;
  TMorphExpRunTime = class;

  TExp_API = class
  private
    SegGradientLevel: Byte;
  protected
    // registration api
    procedure RegInternalAPI(Exp: TMorphExpRunTime);

    // step
    function FindStep(meRT: TMorphExpRunTime; token: U_String): TMorphExpStep;

    // input
    function MorphExp_Main(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_Inherited(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_Token(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;

    // copy
    function MorphExp_AssignInput(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;

    // rasterization
    function MorphExp_Sigma(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_Gradient(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_Scale(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_FitSize(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_BlendBlack(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_BlendWhite(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_DrawToRaster(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_LFBlendRaster(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_RFBlendRaster(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_FlipHorz(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_FlipVert(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_Rotate(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_HoughRotate(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_Raster(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_CombineMorphToRaster(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;

    // Segmentation
    procedure DoGetPixelSegClassify(X, Y: Integer; Color: TRColor; var Classify: TMorphologyClassify);
    procedure DoGetMorphomaticsSegClassify(X, Y: Integer; Morph: TMorphomaticsValue; var Classify: TMorphologyClassify);
    function MorphExp_BuildSegmentation(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_BinarizationMaxSeg(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_ProjectionMaxSeg(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_ProjectionMaxSegAsClip(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_RemoveSegNoise(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_BinarizationSeg(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_ProjectionSeg(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;

    // Morphomatics
    function MorphExp_BuildMorphomatics(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_BuildApproximateMorphomatics(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;

    // Morphomatics: classic morphomatics filter
    function Filter_AVG(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Filter_WeightedAVG(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Filter_GeometricMean(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Filter_Median(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Filter_Max(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Filter_Min(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Filter_MiddlePoint(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Filter_TruncatedAVG(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Filter_Previtt(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Filter_Sobel(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Filter_Sharr(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Filter_Laplace(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;

    // Morphomatics: classic morphomatics transform
    function Transform_Linear(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Transform_Log(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Transform_Gamma(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Transform_HistogramEqualization(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Transform_Contrast(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Transform_Gradient(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Transform_Clamp(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Transform_Add(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Transform_Sub(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Transform_Mul(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Transform_Div(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Transform_LFCombineAdd(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Transform_LFCombineSub(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Transform_LFCombineMul(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Transform_LFCombineDiv(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Transform_RFCombineAdd(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Transform_RFCombineSub(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Transform_RFCombineMul(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Transform_RFCombineDiv(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Transform_MorphFromPolygon(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;

    // Morphology Binaryzation
    function Binarization(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Binarization_InRange(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Binarization_Bernsen(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Binarization_FloydSteinbergDithering(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function Binarization_OTSU(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;

    // Morphomatics and Binaryzation
    function MorphExp_Invert(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_Dilatation(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_Erosion(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_Opening(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_Closing(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_OpeningAndClosing(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_ClosingAndOpening(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_Skeleton(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;

    // output
    function MorphExp_SetOutput(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function MorphExp_ViewHistogram(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
  end;

  TMorphExpRunTime = class(TOpCustomRunTime)
  public
    Step: TMorphExpStep;
    API: TExp_API;
    constructor Create;
    destructor Destroy; override;
    function MorphExp: TMorphExp;
  end;

  TMorphData = class
  public
    Raster: TMemoryRaster;
    Segmentation: TMorphologySegmentation;
    Binaryzation: TMorphologyBinaryzation;
    Morphomatics: TMorphomatics;

    DebugViewer: TMemoryRaster; // debug used

    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function FoundData: Integer;
    function Assign(sour: TMorphData): Integer;
    procedure BuildDebugViewer;
  end;

  TMorphExpStep = class
  public
    ExpStyle: TTextStyle;
    ExpStr, ExpResult: SystemString; // expression body and result
    MorphExp: TMorphExp;             // owner expression engine
    InData: TMorphData;              // link for prev step OutData,seealso result data
    OutData: TMorphData;             // result data
    token: U_String;                 // step label
    constructor Create(exp_: TMorphExp); virtual;
    destructor Destroy; override;
    procedure ResetData;
  end;

  TMorphExpStepList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TMorphExpStep>;

  TMorphExpSteps = class(TMorphExpStepList_Decl)
  public
    constructor Create;
    destructor Destroy; override;

    procedure Remove(obj: TMorphExpStep);
    procedure Delete(index: Integer);
    procedure Clear;

    function FindTokenStep(token: U_String): TMorphExpStep;
  end;

  TMorphExp = class(TCoreClassObject)
  private
    FSteps: TMorphExpSteps;
    FInData, FOutData: TMorphData;
    FShowLog: Boolean;
  public
    // custom const define for expression
    VL_Const: THashVariantList;
    property InData: TMorphData read FInData;

    constructor Create;
    destructor Destroy; override;

    // reset all
    procedure Reset();

    // set input
    procedure InputRaster(fileName: SystemString); overload;
    procedure InputRaster(stream: TCoreClassStream); overload;
    procedure InputRaster(Raster: TMemoryRaster); overload;

    // add text expression
    procedure AddExp(Exp: U_String);
    procedure AddPascalExp(Exp: U_String);
    procedure AddCExp(Exp: U_String);

    // run
    function Run(): Boolean; overload;
    function Run(Raster: TMemoryRaster; script: U_String): Boolean; overload;
    property Steps: TMorphExpSteps read FSteps;

    // output data
    property OutData: TMorphData read FOutData;
    function CheckOutData: Boolean;
    property ShowLog: Boolean read FShowLog write FShowLog;

    class procedure Test();
  end;

  TOnRegExternalAPI_C = procedure(Exp: TMorphExpRunTime);
  TOnRegExternalAPI_M = procedure(Exp: TMorphExpRunTime) of object;
{$IFDEF FPC}
  TOnRegExternalAPI_P = procedure(Exp: TMorphExpRunTime) is nested;
{$ELSE FPC}
  TOnRegExternalAPI_P = reference to procedure(Exp: TMorphExpRunTime);
{$ENDIF FPC}

function RegMorphExpExternalAPI(OnRegC: TOnRegExternalAPI_C; OnRegM: TOnRegExternalAPI_M; OnRegP: TOnRegExternalAPI_P): Pointer;
procedure RemoveMorphExpExternalAPI(p: Pointer);

implementation

type
  POnRegExternalAPIData = ^TOnRegExternalAPIData;

  TOnRegExternalAPIData = record
    OnRegExternalAPI_C: TOnRegExternalAPI_C;
    OnRegExternalAPI_M: TOnRegExternalAPI_M;
    OnRegExternalAPI_P: TOnRegExternalAPI_P;
  end;

  TOnRegExternalAPIDataList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<POnRegExternalAPIData>;

var
  ExternalAPIDataList_: TOnRegExternalAPIDataList;

procedure InitExternalAPI;
begin
  ExternalAPIDataList_ := TOnRegExternalAPIDataList.Create;
end;

procedure FreeExternalAPI;
var
  i: Integer;
begin
  for i := 0 to ExternalAPIDataList_.Count - 1 do
      dispose(ExternalAPIDataList_[i]);
  ExternalAPIDataList_.Clear;
  DisposeObject(ExternalAPIDataList_);
end;

function RegMorphExpExternalAPI(OnRegC: TOnRegExternalAPI_C; OnRegM: TOnRegExternalAPI_M; OnRegP: TOnRegExternalAPI_P): Pointer;
var
  p: POnRegExternalAPIData;
begin
  new(p);
  p^.OnRegExternalAPI_C := OnRegC;
  p^.OnRegExternalAPI_M := OnRegM;
  p^.OnRegExternalAPI_P := OnRegP;
  ExternalAPIDataList_.Add(p);
  Result := p;
end;

procedure RemoveMorphExpExternalAPI(p: Pointer);
begin
  if p = nil then
      exit;
  ExternalAPIDataList_.Remove(POnRegExternalAPIData(p));
  dispose(POnRegExternalAPIData(p));
end;

procedure TExp_API.RegInternalAPI(Exp: TMorphExpRunTime);
begin
  Exp.RegObjectOpM('Main', 'Main(): Morphology expression main', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_Main)^.Category := 'Morphology';
  Exp.RegObjectOpM('GetMain', 'GetMain(): Morphology expression main', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_Main)^.Category := 'Morphology';

  Exp.RegObjectOpM('Inherited', 'Inherited(step name): data Inherited ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_Inherited)^.Category := 'Morphology';
  Exp.RegObjectOpM('In', 'In(step name): input data  ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_Inherited)^.Category := 'Morphology';
  Exp.RegObjectOpM('Input', 'Input(step name): input data  ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_Inherited)^.Category := 'Morphology';

  Exp.RegObjectOpM('Label', 'Label(step name): set step name ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_Token)^.Category := 'Morphology';
  Exp.RegObjectOpM('L', 'L(step name): set step name ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_Token)^.Category := 'Morphology';
  Exp.RegObjectOpM('N', 'N(step name): set step name ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_Token)^.Category := 'Morphology';

  Exp.RegObjectOpM('NOP', 'NOP(): copy input data ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_AssignInput)^.Category := 'Morphology';
  Exp.RegObjectOpM('Copy', 'Copy(): copy input data ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_AssignInput)^.Category := 'Morphology';
  Exp.RegObjectOpM('Clone', 'Clone(): copy input data ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_AssignInput)^.Category := 'Morphology';
  Exp.RegObjectOpM('Assign', 'Assign(): copy input data ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_AssignInput)^.Category := 'Morphology';

  Exp.RegObjectOpM('Sigma', 'Sigma(sigma Thresold, Gaussian Kernel size): Sigma Gaussian ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_Sigma)^.Category := 'Morphology';
  Exp.RegObjectOpM('Gradient', 'Gradient(Gradient level): build Gradient ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_Gradient)^.Category := 'Morphology';
  Exp.RegObjectOpM('Scale', 'Scale(k): scale raster ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_Scale)^.Category := 'Morphology';
  Exp.RegObjectOpM('FitSize', 'FitSize(Width, Height): scale raster ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_FitSize)^.Category := 'Morphology';
  Exp.RegObjectOpM('FitScale', 'FitScale(Width, Height): scale raster ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_FitSize)^.Category := 'Morphology';
  Exp.RegObjectOpM('Fit', 'Fit(Width, Height): scale raster ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_FitSize)^.Category := 'Morphology';
  Exp.RegObjectOpM('Black', 'Black(): background is black ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_BlendBlack)^.Category := 'Morphology';
  Exp.RegObjectOpM('White', 'White(): background is white ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_BlendWhite)^.Category := 'Morphology';
  Exp.RegObjectOpM('Draw', 'Draw(token,TMorphologyPixel): draw raster ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_DrawToRaster)^.Category := 'Morphology';
  Exp.RegObjectOpM('LFBlend', 'LFBlend(token..n): left to right blend raster ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_LFBlendRaster)^.Category := 'Morphology';
  Exp.RegObjectOpM('RFBlend', 'RFBlend(token..n): right to left blend raster ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_RFBlendRaster)^.Category := 'Morphology';
  Exp.RegObjectOpM('FlipHorz', 'FlipHorz(): flip horz ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_FlipHorz)^.Category := 'Morphology';
  Exp.RegObjectOpM('FlipVert', 'FlipVert(): flip vert ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_FlipVert)^.Category := 'Morphology';
  Exp.RegObjectOpM('Rotate', 'MorphExp_Rotate(angle): rotate ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_Rotate)^.Category := 'Morphology';
  Exp.RegObjectOpM('HoughRotate', 'HoughRotate(style): Calibrate Rotate, style = 0,1,2', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_HoughRotate)^.Category := 'Morphology';
  Exp.RegObjectOpM('Raster', 'Raster(): build raster ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_Raster)^.Category := 'Morphology';
  Exp.RegObjectOpM('CombineMorphToRaster', 'CombineMorphToRaster(TMorphologyPixel,Morphomatics data): Combine Morph as new raster ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_CombineMorphToRaster)^.Category := 'Morphology';

  Exp.RegObjectOpM('BuildSegmentation', 'BuildSegmentation(): build Segmentation ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_BuildSegmentation)^.Category := 'Morphology';
  Exp.RegObjectOpM('Seg', 'Seg(): build Segmentation ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_BuildSegmentation)^.Category := 'Morphology';
  Exp.RegObjectOpM('BinMaxSeg', 'BinMaxSeg(): build Binarization from max segmentation ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_BinarizationMaxSeg)^.Category := 'Morphology';
  Exp.RegObjectOpM('ProjectionMaxSeg', 'ProjectionMaxSeg(source): build projection raster from max segmentation ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_ProjectionMaxSeg)^.Category := 'Morphology';
  Exp.RegObjectOpM('ProjMaxSeg', 'ProjMaxSeg(source): build projection raster from max segmentation ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_ProjectionMaxSeg)^.Category := 'Morphology';

  Exp.RegObjectOpM('ProjectionMaxSegCut', 'ProjectionMaxSegCut(source): build projection cut raster from max segmentation ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_ProjectionMaxSegAsClip)^.Category := 'Morphology';
  Exp.RegObjectOpM('ProjMaxSegCut', 'ProjMaxSegCut(source): build projection cut raster from max segmentation ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_ProjectionMaxSegAsClip)^.Category := 'Morphology';

  Exp.RegObjectOpM('RemoveSegNoise', 'RemoveSegNoise(PixelNoiseThreshold): remove noise from segmentation ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_RemoveSegNoise)^.Category := 'Morphology';
  Exp.RegObjectOpM('RemoveSeg', 'RemoveSeg(PixelNoiseThreshold): remove noise from segmentation ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_RemoveSegNoise)^.Category := 'Morphology';
  Exp.RegObjectOpM('BinSeg', 'BinSeg(): build Binarization from all segmentation ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_BinarizationSeg)^.Category := 'Morphology';
  Exp.RegObjectOpM('ProjectionSeg', 'ProjectionSeg(source): build projection raster from all segmentation ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_ProjectionSeg)^.Category := 'Morphology';
  Exp.RegObjectOpM('ProjSeg', 'ProjSeg(source): build projection raster from all segmentation ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_ProjectionSeg)^.Category := 'Morphology';

  Exp.RegObjectOpM('BuildMorphomatics', 'BuildMorphomatics(TMorphologyPixel): build output of Morphomatics ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_BuildMorphomatics)^.Category := 'Morphology';
  Exp.RegObjectOpM('Morph', 'Morph(TMorphologyPixel): build output of Morphomatics ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_BuildMorphomatics)^.Category := 'Morphology';

  Exp.RegObjectOpM('BuildApproximateMorphomatics', 'BuildApproximateMorphomatics(r,g,b): build output of Morphomatics ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_BuildApproximateMorphomatics)^.Category := 'Morphology';
  Exp.RegObjectOpM('ApproximateMorphomatics', 'ApproximateMorphomatics(r,g,b): build output of Morphomatics ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_BuildApproximateMorphomatics)^.Category := 'Morphology';
  Exp.RegObjectOpM('ApproximateMorph', 'ApproximateMorph(r,g,b): build output of Morphomatics ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_BuildApproximateMorphomatics)^.Category := 'Morphology';
  Exp.RegObjectOpM('CustomMorph', 'CustomMorph(r,g,b): build output of Morphomatics ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_BuildApproximateMorphomatics)^.Category := 'Morphology';

  Exp.RegObjectOpM('Filter_AVG', 'Filter_AVG(boxW, boxH): Morphomatics avg filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_AVG)^.Category := 'Morphology';
  Exp.RegObjectOpM('AVG', 'AVG(boxW, boxH): Morphomatics avg filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_AVG)^.Category := 'Morphology';

  Exp.RegObjectOpM('Filter_WeightedAVG', 'Filter_WeightedAVG(boxW, boxH): Morphomatics WeightedAVG filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_WeightedAVG)^.Category := 'Morphology';
  Exp.RegObjectOpM('WeightedAVG', 'WeightedAVG(boxW, boxH): Morphomatics WeightedAVG filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_WeightedAVG)^.Category := 'Morphology';

  Exp.RegObjectOpM('Filter_GeometricMean', 'Filter_GeometricMean(boxW, boxH): Morphomatics GeometricMean filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_GeometricMean)^.Category := 'Morphology';
  Exp.RegObjectOpM('GeometricMean', 'GeometricMean(boxW, boxH): Morphomatics GeometricMean filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_GeometricMean)^.Category := 'Morphology';
  Exp.RegObjectOpM('GeoMean', 'GeoMean(boxW, boxH): Morphomatics GeometricMean filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_GeometricMean)^.Category := 'Morphology';

  Exp.RegObjectOpM('Filter_Median', 'Filter_Median(boxW, boxH): Morphomatics Median filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_Median)^.Category := 'Morphology';
  Exp.RegObjectOpM('Median', 'Median(boxW, boxH): Morphomatics Median filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_Median)^.Category := 'Morphology';

  Exp.RegObjectOpM('Filter_Max', 'Filter_Max(boxW, boxH): Morphomatics max filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_Max)^.Category := 'Morphology';
  Exp.RegObjectOpM('&Max', '&Max(boxW, boxH): Morphomatics max filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_Max)^.Category := 'Morphology';
  Exp.RegObjectOpM('Maximum', 'Maximum(boxW, boxH): Morphomatics max filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_Max)^.Category := 'Morphology';

  Exp.RegObjectOpM('Filter_Min', 'Filter_Min(boxW, boxH): Morphomatics min filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_Min)^.Category := 'Morphology';
  Exp.RegObjectOpM('&Min', '&Min(boxW, boxH): Morphomatics min filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_Min)^.Category := 'Morphology';
  Exp.RegObjectOpM('Minimum', 'Minimum(boxW, boxH): Morphomatics min filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_Min)^.Category := 'Morphology';

  Exp.RegObjectOpM('Filter_MiddlePoint', 'Filter_MiddlePoint(boxW, boxH): Morphomatics MiddlePoint filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_MiddlePoint)^.Category := 'Morphology';
  Exp.RegObjectOpM('MiddlePoint', 'MiddlePoint(boxW, boxH): Morphomatics MiddlePoint filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_MiddlePoint)^.Category := 'Morphology';
  Exp.RegObjectOpM('Middle', 'Middle(boxW, boxH): Morphomatics MiddlePoint filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_MiddlePoint)^.Category := 'Morphology';

  Exp.RegObjectOpM('Filter_TruncatedAVG', 'Filter_TruncatedAVG(boxW, boxH, distance): Morphomatics TruncatedAVG filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_TruncatedAVG)^.Category := 'Morphology';
  Exp.RegObjectOpM('TruncatedAVG', 'TruncatedAVG(boxW, boxH, distance): Morphomatics TruncatedAVG filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_TruncatedAVG)^.Category := 'Morphology';

  Exp.RegObjectOpM('Filter_Previtt', 'Filter_Previtt(AdditiveToOriginal): Morphomatics Previtt filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_Previtt)^.Category := 'Morphology';
  Exp.RegObjectOpM('Previtt', 'Previtt(AdditiveToOriginal): Morphomatics Previtt filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_Previtt)^.Category := 'Morphology';

  Exp.RegObjectOpM('Filter_Sobel', 'Filter_Sobel(AdditiveToOriginal): Morphomatics Sobel filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_Sobel)^.Category := 'Morphology';
  Exp.RegObjectOpM('Sobel', 'Sobel(AdditiveToOriginal): Morphomatics Sobel filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_Sobel)^.Category := 'Morphology';

  Exp.RegObjectOpM('Filter_Sharr', 'Filter_Sharr(AdditiveToOriginal): Morphomatics Sharr filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_Sharr)^.Category := 'Morphology';
  Exp.RegObjectOpM('Sharr', 'Sharr(AdditiveToOriginal): Morphomatics Sharr filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_Sharr)^.Category := 'Morphology';

  Exp.RegObjectOpM('Filter_Laplace', 'Filter_Laplace(AdditiveToOriginal): Morphomatics Laplace filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_Laplace)^.Category := 'Morphology';
  Exp.RegObjectOpM('Laplace', 'Laplace(AdditiveToOriginal): Morphomatics Laplace filter ', {$IFDEF FPC}@{$ENDIF FPC}Filter_Laplace)^.Category := 'Morphology';

  Exp.RegObjectOpM('Transform_Linear', 'Transform_Linear(K, B): Morphomatics Linear Transform ', {$IFDEF FPC}@{$ENDIF FPC}Transform_Linear)^.Category := 'Morphology';
  Exp.RegObjectOpM('Linear', 'Linear(K, B): Morphomatics Linear Transform ', {$IFDEF FPC}@{$ENDIF FPC}Transform_Linear)^.Category := 'Morphology';

  Exp.RegObjectOpM('Transform_Log', 'Transform_Log(C): Morphomatics log Transform ', {$IFDEF FPC}@{$ENDIF FPC}Transform_Log)^.Category := 'Morphology';
  Exp.RegObjectOpM('Log', 'Log(C): Morphomatics log Transform ', {$IFDEF FPC}@{$ENDIF FPC}Transform_Log)^.Category := 'Morphology';
  Exp.RegObjectOpM('Logarithms', 'Logarithms(C): Morphomatics log Transform ', {$IFDEF FPC}@{$ENDIF FPC}Transform_Log)^.Category := 'Morphology';

  Exp.RegObjectOpM('Transform_Gamma', 'Transform_Gamma(C, GAMMA): Morphomatics GAMMA Transform ', {$IFDEF FPC}@{$ENDIF FPC}Transform_Gamma)^.Category := 'Morphology';
  Exp.RegObjectOpM('Gamma', 'Gamma(C, GAMMA): Morphomatics GAMMA Transform ', {$IFDEF FPC}@{$ENDIF FPC}Transform_Gamma)^.Category := 'Morphology';

  Exp.RegObjectOpM('Transform_HistogramEqualization', 'Transform_HistogramEqualization(): Morphomatics HistogramEqualization Transform ', {$IFDEF FPC}@{$ENDIF FPC}Transform_HistogramEqualization)^.Category := 'Morphology';
  Exp.RegObjectOpM('HistogramEqualization', 'HistogramEqualization(): Morphomatics HistogramEqualization Transform ', {$IFDEF FPC}@{$ENDIF FPC}Transform_HistogramEqualization)^.Category := 'Morphology';
  Exp.RegObjectOpM('Hist', 'Hist(): Morphomatics HistogramEqualization Transform ', {$IFDEF FPC}@{$ENDIF FPC}Transform_HistogramEqualization)^.Category := 'Morphology';

  Exp.RegObjectOpM('Transform_Contrast', 'Transform_Contrast(K): Morphomatics Contrast Transform ', {$IFDEF FPC}@{$ENDIF FPC}Transform_Contrast)^.Category := 'Morphology';
  Exp.RegObjectOpM('Contrast', 'Contrast(K): Morphomatics Contrast Transform ', {$IFDEF FPC}@{$ENDIF FPC}Transform_Contrast)^.Category := 'Morphology';

  Exp.RegObjectOpM('Transform_Gradient', 'Transform_Gradient(level): Morphomatics Gradient Transform ', {$IFDEF FPC}@{$ENDIF FPC}Transform_Gradient)^.Category := 'Morphology';
  Exp.RegObjectOpM('Transform_Clamp', 'Transform_Clamp(min, max): Morphomatics Clamp Transform ', {$IFDEF FPC}@{$ENDIF FPC}Transform_Clamp)^.Category := 'Morphology';

  Exp.RegObjectOpM('Transform_Add', 'Transform_Add(Value): Morphomatics Add operation ', {$IFDEF FPC}@{$ENDIF FPC}Transform_Add)^.Category := 'Morphology';
  Exp.RegObjectOpM('Transform_Sub', 'Transform_Sub(Value): Morphomatics Sub operation ', {$IFDEF FPC}@{$ENDIF FPC}Transform_Sub)^.Category := 'Morphology';
  Exp.RegObjectOpM('Transform_Mul', 'Transform_Mul(Value): Morphomatics Mul operation ', {$IFDEF FPC}@{$ENDIF FPC}Transform_Mul)^.Category := 'Morphology';
  Exp.RegObjectOpM('Transform_Div', 'Transform_Div(Value): Morphomatics Div operation ', {$IFDEF FPC}@{$ENDIF FPC}Transform_Div)^.Category := 'Morphology';

  Exp.RegObjectOpM('&Add', '&Add(Value): Morphomatics Add operation ', {$IFDEF FPC}@{$ENDIF FPC}Transform_Add)^.Category := 'Morphology';
  Exp.RegObjectOpM('&Sub', '&Sub(Value): Morphomatics Sub operation ', {$IFDEF FPC}@{$ENDIF FPC}Transform_Sub)^.Category := 'Morphology';
  Exp.RegObjectOpM('&Mul', '&Mul(Value): Morphomatics Mul operation ', {$IFDEF FPC}@{$ENDIF FPC}Transform_Mul)^.Category := 'Morphology';
  Exp.RegObjectOpM('&Div', '&Div(Value): Morphomatics Div operation ', {$IFDEF FPC}@{$ENDIF FPC}Transform_Div)^.Category := 'Morphology';
  Exp.RegObjectOpM('&Clamp', '&Clamp(min, max): Morphomatics Clamp Transform ', {$IFDEF FPC}@{$ENDIF FPC}Transform_Clamp)^.Category := 'Morphology';

  Exp.RegObjectOpM('LFCombineAdd', 'LFCombineAdd(Value): Morphomatics Left to right combine add operation ', {$IFDEF FPC}@{$ENDIF FPC}Transform_LFCombineAdd)^.Category := 'Morphology';
  Exp.RegObjectOpM('LFCombineSub', 'LFCombineSub(Value): Morphomatics Left to right combine sub operation ', {$IFDEF FPC}@{$ENDIF FPC}Transform_LFCombineSub)^.Category := 'Morphology';
  Exp.RegObjectOpM('LFCombineMul', 'LFCombineMul(Value): Morphomatics Left to right combine mul operation ', {$IFDEF FPC}@{$ENDIF FPC}Transform_LFCombineMul)^.Category := 'Morphology';
  Exp.RegObjectOpM('LFCombineDiv', 'LFCombineDiv(Value): Morphomatics Left to right combine div operation ', {$IFDEF FPC}@{$ENDIF FPC}Transform_LFCombineDiv)^.Category := 'Morphology';

  Exp.RegObjectOpM('RFCombineAdd', 'RLFCombineAdd(Value): Morphomatics right to left combine add operation ', {$IFDEF FPC}@{$ENDIF FPC}Transform_RFCombineAdd)^.Category := 'Morphology';
  Exp.RegObjectOpM('RFCombineSub', 'RLFCombineSub(Value): Morphomatics right to left combine sub operation ', {$IFDEF FPC}@{$ENDIF FPC}Transform_RFCombineSub)^.Category := 'Morphology';
  Exp.RegObjectOpM('RFCombineMul', 'RLFCombineMul(Value): Morphomatics right to left combine mul operation ', {$IFDEF FPC}@{$ENDIF FPC}Transform_RFCombineMul)^.Category := 'Morphology';
  Exp.RegObjectOpM('RFCombineDiv', 'RLFCombineDev(Value): Morphomatics right to left combine div operation ', {$IFDEF FPC}@{$ENDIF FPC}Transform_RFCombineDiv)^.Category := 'Morphology';

  Exp.RegObjectOpM('MorphFromPolygon', 'MorphFromPolygon(InsideValue, OutsideValue, Polygon): Morphomatics mul polygon ', {$IFDEF FPC}@{$ENDIF FPC}Transform_MorphFromPolygon)^.Category := 'Morphology';
  Exp.RegObjectOpM('MorphPolygon', 'MorphPolygon(InsideValue, OutsideValue, Polygon): Morphomatics mul polygon ', {$IFDEF FPC}@{$ENDIF FPC}Transform_MorphFromPolygon)^.Category := 'Morphology';
  Exp.RegObjectOpM('MorphPoly', 'MorphPoly(InsideValue, OutsideValue, Polygon): Morphomatics mul polygon ', {$IFDEF FPC}@{$ENDIF FPC}Transform_MorphFromPolygon)^.Category := 'Morphology';

  Exp.RegObjectOpM('Binarization', 'Binarization(Thresold): build output of Binarization ', {$IFDEF FPC}@{$ENDIF FPC}Binarization)^.Category := 'Morphology';
  Exp.RegObjectOpM('Bin', 'Bin(Thresold): build output of Binarization ', {$IFDEF FPC}@{$ENDIF FPC}Binarization)^.Category := 'Morphology';

  Exp.RegObjectOpM('Binarization_InRange', 'Binarization_InRange(Min, Max): build output of Binarization ', {$IFDEF FPC}@{$ENDIF FPC}Binarization_InRange)^.Category := 'Morphology';
  Exp.RegObjectOpM('BinRange', 'BinRange(Min, Max): build output of Binarization ', {$IFDEF FPC}@{$ENDIF FPC}Binarization_InRange)^.Category := 'Morphology';

  Exp.RegObjectOpM('Binarization_Bernsen', 'Binarization_Bernsen(Range, Thresold): build output of Binarization ', {$IFDEF FPC}@{$ENDIF FPC}Binarization_Bernsen)^.Category := 'Morphology';
  Exp.RegObjectOpM('BinBernsen', 'BinBernsen(Range, Thresold): build output of Binarization ', {$IFDEF FPC}@{$ENDIF FPC}Binarization_Bernsen)^.Category := 'Morphology';
  Exp.RegObjectOpM('Bernsen', 'Bernsen(Range, Thresold): build output of Binarization ', {$IFDEF FPC}@{$ENDIF FPC}Binarization_Bernsen)^.Category := 'Morphology';

  Exp.RegObjectOpM('Binarization_FloydSteinbergDithering', 'Binarization_FloydSteinbergDithering(): build output of Binarization ', {$IFDEF FPC}@{$ENDIF FPC}Binarization_FloydSteinbergDithering)^.Category := 'Morphology';
  Exp.RegObjectOpM('BinFloydSteinbergDithering', 'BinFloydSteinbergDithering(): build output of Binarization ', {$IFDEF FPC}@{$ENDIF FPC}Binarization_FloydSteinbergDithering)^.Category := 'Morphology';
  Exp.RegObjectOpM('FloydSteinbergDithering', 'FloydSteinbergDithering(): build output of Binarization ', {$IFDEF FPC}@{$ENDIF FPC}Binarization_FloydSteinbergDithering)^.Category := 'Morphology';

  Exp.RegObjectOpM('Binarization_OTSU', 'Binarization_OTSU(): build output of Binarization ', {$IFDEF FPC}@{$ENDIF FPC}Binarization_OTSU)^.Category := 'Morphology';
  Exp.RegObjectOpM('BinOTSU', 'BinOTSU(): build output of Binarization ', {$IFDEF FPC}@{$ENDIF FPC}Binarization_OTSU)^.Category := 'Morphology';
  Exp.RegObjectOpM('OTSU', 'OTSU(): build output of Binarization ', {$IFDEF FPC}@{$ENDIF FPC}Binarization_OTSU)^.Category := 'Morphology';

  Exp.RegObjectOpM('Invert', 'Invert(): build output of Morphomatics or Binarization ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_Invert)^.Category := 'Morphology';
  Exp.RegObjectOpM('Inv', 'Inv(): build output of Morphomatics or Binarization ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_Invert)^.Category := 'Morphology';
  Exp.RegObjectOpM('Dilatation', 'Dilatation(Convolution Kernel Size w,h): build output of Morphomatics or Binarization ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_Dilatation)^.Category := 'Morphology';
  Exp.RegObjectOpM('Erosion', 'Erosion(Convolution Kernel Size w,h): build output of Morphomatics or Binarization ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_Erosion)^.Category := 'Morphology';
  Exp.RegObjectOpM('Opening', 'Opening(Convolution Kernel Size w,h): build output of Morphomatics or Binarization ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_Opening)^.Category := 'Morphology';
  Exp.RegObjectOpM('Closing', 'Closing(Convolution Kernel Size w,h): build output of Morphomatics or Binarization ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_Closing)^.Category := 'Morphology';
  Exp.RegObjectOpM('OpeningAndClosing', 'OpeningAndClosing(Convolution Kernel Size w,h): build output of Morphomatics or Binarization ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_OpeningAndClosing)^.Category := 'Morphology';
  Exp.RegObjectOpM('ClosingAndOpening', 'ClosingAndOpening(Convolution Kernel Size w,h): build output of Morphomatics or Binarization ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_ClosingAndOpening)^.Category := 'Morphology';
  Exp.RegObjectOpM('Skeleton', 'Skeleton(): build Skeleton output of Binarization ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_Skeleton)^.Category := 'Morphology';

  Exp.RegObjectOpM('ViewHistogram', 'ViewHistogram(): build Histogram for viewer ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_ViewHistogram)^.Category := 'Morphology';

  Exp.RegObjectOpM('IsOut', 'IsOut(): set output data from this step. ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_SetOutput)^.Category := 'Morphology';
  Exp.RegObjectOpM('SetOutput', 'SetOutput(): set output data from this step. ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_SetOutput)^.Category := 'Morphology';
  Exp.RegObjectOpM('Output', 'Output(): set output data from this step. ', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_SetOutput)^.Category := 'Morphology';
end;

function TExp_API.FindStep(meRT: TMorphExpRunTime; token: U_String): TMorphExpStep;
var
  index: Integer;
begin
  Result := nil;
  if token.L = 0 then
      exit;
  index := meRT.MorphExp.FSteps.IndexOf(meRT.Step) - 1;
  while index >= 0 do
    begin
      if token.Same(meRT.MorphExp.FSteps[index].token) then
        begin
          Result := meRT.MorphExp.FSteps[index];
          break;
        end
      else
          dec(index);
    end;
end;

function TExp_API.MorphExp_Main(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  meRT.Step.InData := meRT.MorphExp.FInData;
  meRT.Step.OutData.Assign(meRT.MorphExp.FInData);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_Inherited(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  prev_Step: TMorphExpStep;
  token: U_String;
  index: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;

  index := meRT.MorphExp.FSteps.IndexOf(meRT.Step) - 1;
  if length(Param) = 1 then
    begin
      token := umlVarToStr(Param[0], False);
      while index > 0 do
        begin
          if token.Same(meRT.MorphExp.FSteps[index].token) then
              break
          else
              dec(index);
        end;
    end;

  while index >= 0 do
    begin
      prev_Step := meRT.MorphExp.FSteps[index];
      if prev_Step.OutData.FoundData > 0 then
        begin
          meRT.Step.InData := prev_Step.OutData;
          Result := OpRunTime.Trigger^.Name + '[ok]';
          exit;
        end
      else if prev_Step.InData.FoundData > 0 then
        begin
          meRT.Step.InData := prev_Step.InData;
          Result := OpRunTime.Trigger^.Name + '[ok]';
          exit;
        end
      else
          dec(index);
    end;
  Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
end;

function TExp_API.MorphExp_Token(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  case length(Param) of
    1:
      begin
        meRT.Step.token := umlVarToStr(Param[0], False);
        Result := OpRunTime.Trigger^.Name + PFormat('[label: %s]', [meRT.Step.token.Text]);
      end;
    else
      begin
        meRT.Step.token := '';
        Result := OpRunTime.Trigger^.Name + PFormat('[Label is NULL]', []);
      end;
  end;
end;

function TExp_API.MorphExp_AssignInput(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  sigma_: TGeoFloat;
  SigmaGaussianKernelFactor_: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_Sigma(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  sigma_: TGeoFloat;
  SigmaGaussianKernelFactor_: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Raster = nil) and (meRT.Step.InData.Morphomatics = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be raster or Morphomatics input]';
      exit;
    end;
  meRT.Step.OutData.Assign(meRT.Step.InData);

  sigma_ := 3.0;
  SigmaGaussianKernelFactor_ := 3;
  case length(Param) of
    2:
      begin
        sigma_ := Param[0];
        SigmaGaussianKernelFactor_ := Param[1];
      end;
    1: sigma_ := Param[0];
  end;

  if meRT.Step.OutData.Raster <> nil then
      meRT.Step.OutData.Raster.SigmaGaussian(sigma_, SigmaGaussianKernelFactor_);
  if meRT.Step.OutData.Morphomatics <> nil then
      meRT.Step.OutData.Morphomatics.SigmaGaussian(sigma_, SigmaGaussianKernelFactor_);

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_Gradient(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  level: Byte;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Raster = nil) and (meRT.Step.InData.Morphomatics = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be raster or Morphomatics input]';
      exit;
    end;

  meRT.Step.OutData.Assign(meRT.Step.InData);

  level := $FF;
  case length(Param) of
    1: level := Param[0];
  end;

  if meRT.Step.OutData.Raster <> nil then
      meRT.Step.OutData.Raster.Gradient(level);
  if meRT.Step.OutData.Morphomatics <> nil then
      meRT.Step.OutData.Morphomatics.Gradient(level);

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_Scale(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  k: TGeoFloat;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Raster = nil) and (meRT.Step.InData.Morphomatics = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be raster or Morphomatics input]';
      exit;
    end;

  meRT.Step.OutData.Assign(meRT.Step.InData);

  k := 1.0;
  case length(Param) of
    1: k := Param[0];
  end;

  if meRT.Step.InData.Raster <> nil then
      meRT.Step.OutData.Raster.Scale(k)
  else if meRT.Step.InData.Morphomatics <> nil then
      meRT.Step.OutData.Morphomatics.Scale(k);

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_FitSize(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  Width, Height: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Raster = nil) and (meRT.Step.InData.Morphomatics = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be raster or Morphomatics input]';
      exit;
    end;

  meRT.Step.OutData.Assign(meRT.Step.InData);

  Width := 512;
  Height := 512;
  case length(Param) of
    1:
      begin
        Width := Param[0];
        Height := Param[0];
      end;
    2:
      begin
        Width := Param[0];
        Height := Param[1];
      end;
  end;

  if meRT.Step.InData.Raster <> nil then
      meRT.Step.OutData.Raster.FitScale(Width, Height)
  else if meRT.Step.InData.Morphomatics <> nil then
      meRT.Step.OutData.Morphomatics.FitScale(Width, Height);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_BlendBlack(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Raster = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be raster input]';
      exit;
    end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Raster.BlendColor(RColor(0, 0, 0));

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_BlendWhite(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Raster = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be raster input]';
      exit;
    end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Raster.BlendColor(RColor($FF, $FF, $FF));

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_DrawToRaster(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  MorphPix_: TMorphologyPixel;
  drawToRasterToken: U_String;
  step_: TMorphExpStep;
  i: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Raster <> nil) then
    begin
      MorphPix_ := mpGrayscale;
      drawToRasterToken := '';
      case length(Param) of
        1: drawToRasterToken := umlVarToStr(Param[0], False);
        2:
          begin
            drawToRasterToken := umlVarToStr(Param[0], False);
            MorphPix_ := Param[1];
          end;
      end;
      step_ := FindStep(meRT, drawToRasterToken);
      if step_ <> nil then
          meRT.Step.OutData.Assign(step_.OutData)
      else
        begin
          meRT.Step.OutData.Raster := NewRaster();
          meRT.Step.OutData.Raster.SetSize(meRT.Step.InData.Raster.Width, meRT.Step.InData.Raster.Height, RColor(0, 0, 0, 0));
        end;

      meRT.Step.OutData.Raster.Draw(meRT.Step.InData.Raster);
      Result := OpRunTime.Trigger^.Name + '[ok]';
    end
  else if (meRT.Step.InData.Morphomatics <> nil) then
    begin
      MorphPix_ := mpGrayscale;
      drawToRasterToken := '';
      case length(Param) of
        1: drawToRasterToken := umlVarToStr(Param[0], False);
        2:
          begin
            drawToRasterToken := umlVarToStr(Param[0], False);
            MorphPix_ := Param[1];
          end;
      end;
      step_ := FindStep(meRT, drawToRasterToken);
      if step_ <> nil then
          meRT.Step.OutData.Assign(step_.OutData)
      else
          meRT.Step.OutData.Raster := NewRaster();

      meRT.Step.OutData.Raster.DrawMorphomatics(MorphPix_, meRT.Step.InData.Morphomatics);
      for i := 2 to length(Param) - 1 do
          meRT.Step.OutData.Raster.DrawMorphomatics(Param[i], meRT.Step.InData.Morphomatics);

      Result := OpRunTime.Trigger^.Name + '[ok]';
    end
  else if (meRT.Step.InData.Binaryzation <> nil) then
    begin
      MorphPix_ := mpGrayscale;
      drawToRasterToken := '';
      case length(Param) of
        1: drawToRasterToken := umlVarToStr(Param[0], False);
        2:
          begin
            drawToRasterToken := umlVarToStr(Param[0], False);
            MorphPix_ := Param[1];
          end;
      end;
      step_ := FindStep(meRT, drawToRasterToken);
      if step_ <> nil then
          meRT.Step.OutData.Assign(step_.OutData)
      else
          meRT.Step.OutData.Raster := NewRaster();

      meRT.Step.OutData.Raster.DrawBinaryzation(MorphPix_, meRT.Step.InData.Binaryzation);
      for i := 2 to length(Param) - 1 do
          meRT.Step.OutData.Raster.DrawBinaryzation(Param[i], meRT.Step.InData.Binaryzation);
      Result := OpRunTime.Trigger^.Name + '[ok]';
    end;
end;

function TExp_API.MorphExp_LFBlendRaster(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  i: Integer;
  n: U_String;
  step_: TMorphExpStep;
  w, h: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  w := 0;
  h := 0;

  // check
  for i := Low(Param) to High(Param) do
    begin
      n := umlVarToStr(Param[i], False);
      step_ := FindStep(meRT, n);
      if (step_ <> nil) and (step_.OutData.FoundData > 0) and (step_.OutData.Raster <> nil) then
        begin
          w := umlMax(w, step_.OutData.Raster.Width);
          h := umlMax(h, step_.OutData.Raster.Height);
        end
      else
        begin
          Result := OpRunTime.Trigger^.Name + PFormat('[error: "%s" must be raster input]', [n.Text]);
          exit;
        end;
    end;

  meRT.Step.OutData.Clear;
  meRT.Step.OutData.Raster := NewRaster();
  meRT.Step.OutData.Raster.SetSize(w, h, RColor(0, 0, 0, 0));

  // blend
  for i := Low(Param) to High(Param) do
      FindStep(meRT, umlVarToStr(Param[i], False)).OutData.Raster.DrawTo(meRT.Step.OutData.Raster);

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_RFBlendRaster(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  i: Integer;
  n: U_String;
  step_: TMorphExpStep;
  w, h: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  w := 0;
  h := 0;

  // check
  for i := High(Param) downto Low(Param) do
    begin
      n := umlVarToStr(Param[i], False);
      step_ := FindStep(meRT, n);
      if (step_ <> nil) and (step_.OutData.FoundData > 0) and (step_.OutData.Raster <> nil) then
        begin
          w := umlMax(w, step_.OutData.Raster.Width);
          h := umlMax(h, step_.OutData.Raster.Height);
        end
      else
        begin
          Result := OpRunTime.Trigger^.Name + PFormat('[error: "%s" must be raster input]', [n.Text]);
          exit;
        end;
    end;

  meRT.Step.OutData.Clear;
  meRT.Step.OutData.Raster := NewRaster();
  meRT.Step.OutData.Raster.SetSize(w, h, RColor(0, 0, 0, 0));

  // blend
  for i := High(Param) downto Low(Param) do
      FindStep(meRT, umlVarToStr(Param[i], False)).OutData.Raster.DrawTo(meRT.Step.OutData.Raster);

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_FlipHorz(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  Ang: TGeoFloat;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Raster = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be raster input]';
      exit;
    end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Raster.FlipHorz();
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_FlipVert(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  Ang: TGeoFloat;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Raster = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be raster input]';
      exit;
    end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Raster.FlipVert();
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_Rotate(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  Ang: TGeoFloat;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Raster = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be raster input]';
      exit;
    end;

  meRT.Step.OutData.Assign(meRT.Step.InData);

  Ang := 0;
  case length(Param) of
    1: Ang := Param[0];
  end;

  case Round(Ang) of
    90: meRT.Step.OutData.Raster.Rotate90();
    -90, 270: meRT.Step.OutData.Raster.Rotate270();
    180: meRT.Step.OutData.Raster.Rotate270();
    else meRT.Step.OutData.Raster.Rotate(Ang, 0, RColor(0, 0, 0, 0));
  end;
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_HoughRotate(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  s: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Raster = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be raster input]';
      exit;
    end;

  s := 0;
  case length(Param) of
    1: s := Param[0];
  end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  case s of
    1: meRT.Step.OutData.Raster.CalibrateRotate_LineDistance(RColor(0, 0, 0, 0));
    2: meRT.Step.OutData.Raster.CalibrateRotate_LineMatched(RColor(0, 0, 0, 0));
    else meRT.Step.OutData.Raster.CalibrateRotate_AVG(RColor(0, 0, 0, 0));
  end;

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_Raster(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  Result := OpRunTime.Trigger^.Name + '[ok]';
  if (meRT.Step.InData.Raster <> nil) then
      meRT.Step.OutData.Assign(meRT.Step.InData)
  else if (meRT.Step.InData.Morphomatics <> nil) then
    begin
      if length(Param) > 0 then
          meRT.Step.OutData.Raster := meRT.Step.InData.Morphomatics.BuildViewer(Param[0])
      else
          meRT.Step.OutData.Raster := meRT.Step.InData.Morphomatics.BuildViewer()
    end
  else if (meRT.Step.InData.Binaryzation <> nil) then
    begin
      if length(Param) > 0 then
          meRT.Step.OutData.Raster := meRT.Step.InData.Binaryzation.BuildViewer(Param[0])
      else
          meRT.Step.OutData.Raster := meRT.Step.InData.Binaryzation.BuildViewer();
    end
  else if (meRT.Step.InData.Segmentation <> nil) then
    begin
      meRT.Step.OutData.Raster := meRT.Step.InData.Segmentation.BuildViewer();
    end
  else
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
end;

function TExp_API.MorphExp_CombineMorphToRaster(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  i: Integer;
  n: U_String;
  mp: TMorphPixel;
  step_: TMorphExpStep;
  w, h: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  w := 0;
  h := 0;
  i := 0;

  // check
  while i < length(Param) do
    begin
      n := umlVarToStr(Param[i], False);
      inc(i);
      if i < length(Param) then
        begin
          n := umlVarToStr(Param[i], False);
          step_ := FindStep(meRT, n);
          if (step_ <> nil) and (step_.OutData.FoundData > 0) and (step_.OutData.Morphomatics <> nil) then
            begin
              w := umlMax(w, step_.OutData.Morphomatics.Width);
              h := umlMax(h, step_.OutData.Morphomatics.Height);
            end
          else
            begin
              Result := OpRunTime.Trigger^.Name + PFormat('[error: "%s" must be Morphomatics input]', [n.Text]);
              exit;
            end;
          inc(i);
        end
      else
        begin
          Result := OpRunTime.Trigger^.Name + PFormat('[error: "%s" must be TMorphPixel,Morphomatics input]', [n.Text]);
          exit;
        end;
    end;

  meRT.Step.OutData.Clear;
  meRT.Step.OutData.Raster := NewRaster();
  meRT.Step.OutData.Raster.SetSize(w, h, RColor(0, 0, 0, $FF));

  // combine
  i := 0;
  while i < length(Param) do
    begin
      mp := Param[i];
      inc(i);
      n := umlVarToStr(Param[i], False);
      step_ := FindStep(meRT, n);
      inc(i);
      meRT.Step.OutData.Raster.DrawMorphomatics(mp, step_.OutData.Morphomatics);
    end;

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

procedure TExp_API.DoGetPixelSegClassify(X, Y: Integer; Color: TRColor; var Classify: TMorphologyClassify);
begin
  if SegGradientLevel = 0 then
      Classify := Color
  else
      Classify := RColorGradient(Color, SegGradientLevel);
end;

procedure TExp_API.DoGetMorphomaticsSegClassify(X, Y: Integer; Morph: TMorphomaticsValue; var Classify: TMorphologyClassify);
begin
  if SegGradientLevel = 0 then
      Classify := Round(Morph * $FF)
  else
      Classify := Trunc(Trunc(Trunc(umlClamp(Morph, 0.0, 1.0) * $FF) / ($FF / 16)) * ($FF / 16));
end;

function TExp_API.MorphExp_BuildSegmentation(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  SegGradientLevel := 3;
  case length(Param) of
    1: SegGradientLevel := Param[0];
  end;

  if meRT.Step.InData.Raster <> nil then
    begin
      meRT.Step.OutData.Segmentation := TMorphologySegmentation.Create;
      meRT.Step.OutData.Segmentation.OnGetPixelSegClassify := {$IFDEF FPC}@{$ENDIF FPC}DoGetPixelSegClassify;
      meRT.Step.OutData.Segmentation.BuildSegmentation(meRT.Step.InData.Raster);
      meRT.Step.OutData.Segmentation.RemoveNoise(100);
      Result := OpRunTime.Trigger^.Name + '[ok]';
    end
  else if meRT.Step.InData.Morphomatics <> nil then
    begin
      meRT.Step.OutData.Segmentation := TMorphologySegmentation.Create;
      meRT.Step.OutData.Segmentation.OnGetMorphomaticsSegClassify := {$IFDEF FPC}@{$ENDIF FPC}DoGetMorphomaticsSegClassify;
      meRT.Step.OutData.Segmentation.BuildSegmentation(meRT.Step.InData.Morphomatics);
      meRT.Step.OutData.Segmentation.RemoveNoise(100);
      Result := OpRunTime.Trigger^.Name + '[ok]';
    end
  else if meRT.Step.InData.Binaryzation <> nil then
    begin
      meRT.Step.OutData.Segmentation := TMorphologySegmentation.Create;
      meRT.Step.OutData.Segmentation.BuildSegmentation(meRT.Step.InData.Binaryzation);
      meRT.Step.OutData.Segmentation.RemoveNoise(100);
      Result := OpRunTime.Trigger^.Name + '[ok]';
    end
  else
      Result := OpRunTime.Trigger^.Name + '[error: must be raster or Morphology or Binaryzation input]';
end;

function TExp_API.MorphExp_BinarizationMaxSeg(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Segmentation <> nil) and (meRT.Step.InData.Segmentation.Count > 0) then
    begin
      meRT.Step.OutData.Binaryzation := meRT.Step.InData.Segmentation[0].BuildBinaryzation;
      Result := OpRunTime.Trigger^.Name + '[ok]';
    end
  else
      Result := OpRunTime.Trigger^.Name + '[error: must be Segmentation input]';
end;

function TExp_API.MorphExp_ProjectionMaxSeg(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  SourceRasterToken: U_String;
  step_: TMorphExpStep;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Segmentation = nil) or (meRT.Step.InData.Segmentation.Count <= 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Segmentation input]';
      exit;
    end;

  SourceRasterToken := '';
  case length(Param) of
    1: SourceRasterToken := umlVarToStr(Param[0], False);
  end;

  step_ := FindStep(meRT, SourceRasterToken);
  if (step_ = nil) or (step_.OutData.Raster = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: projection source]';
      exit;
    end;

  meRT.Step.OutData.Raster := meRT.Step.InData.Segmentation[0].Projection(step_.OutData.Raster);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_ProjectionMaxSegAsClip(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  SourceRasterToken: U_String;
  step_: TMorphExpStep;
  r: TRectV2;
  tmp: TMemoryRaster;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Segmentation = nil) or (meRT.Step.InData.Segmentation.Count <= 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Segmentation input]';
      exit;
    end;

  SourceRasterToken := '';
  case length(Param) of
    1: SourceRasterToken := umlVarToStr(Param[0], False);
  end;

  step_ := FindStep(meRT, SourceRasterToken);
  if (step_ = nil) or (step_.OutData.Raster = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: projection source]';
      exit;
    end;

  r := meRT.Step.InData.Segmentation[0].BoundsRectV2;
  tmp := meRT.Step.InData.Segmentation[0].Projection(step_.OutData.Raster);
  meRT.Step.OutData.Raster := tmp.BuildAreaCopyAs(r);
  DisposeObject(tmp);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_RemoveSegNoise(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  PixelNoiseThreshold: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Segmentation = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Segmentation input]';
      exit;
    end;

  PixelNoiseThreshold := 100;
  case length(Param) of
    1: PixelNoiseThreshold := Param[0];
  end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Segmentation.RemoveNoise(PixelNoiseThreshold);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_BinarizationSeg(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Segmentation <> nil) then
    begin
      meRT.Step.OutData.Binaryzation := meRT.Step.InData.Segmentation.BuildBinaryzation;
      Result := OpRunTime.Trigger^.Name + '[ok]';
    end
  else
      Result := OpRunTime.Trigger^.Name + '[error: must be Segmentation input]';
end;

function TExp_API.MorphExp_ProjectionSeg(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  SourceRasterToken: U_String;
  step_: TMorphExpStep;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Segmentation = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Segmentation input]';
      exit;
    end;

  SourceRasterToken := '';
  case length(Param) of
    1: SourceRasterToken := umlVarToStr(Param[0], False);
  end;

  step_ := FindStep(meRT, SourceRasterToken);
  if (step_ = nil) or (step_.OutData.Raster = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: projection source]';
      exit;
    end;

  meRT.Step.OutData.Raster := meRT.Step.InData.Segmentation.Projection(step_.OutData.Raster);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_BuildMorphomatics(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  MorphPix_: TMorphologyPixel;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Raster <> nil then
    begin
      MorphPix_ := TMorphologyPixel.mpGrayscale;
      case length(Param) of
        1: MorphPix_ := Param[0];
      end;

      meRT.Step.OutData.Morphomatics := meRT.Step.InData.Raster.BuildMorphomatics(MorphPix_);
      Result := OpRunTime.Trigger^.Name + '[ok]';
    end
  else if meRT.Step.InData.Binaryzation <> nil then
    begin
      meRT.Step.OutData.Morphomatics := meRT.Step.InData.Binaryzation.BuildMorphomatics();
      Result := OpRunTime.Trigger^.Name + '[ok]';
    end
  else
      Result := OpRunTime.Trigger^.Name + '[error: must be raster input]';
end;

function TExp_API.MorphExp_BuildApproximateMorphomatics(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  v: Byte;
  Color: TRColorEntry;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Raster <> nil then
    begin
      Color.BGRA := RColor(0, 0, 0);
      if length(Param) = 3 then
        begin
          Color.r := ifv_(Param[0] > 1.0, Param[0], Param[0] * $FF);
          Color.G := ifv_(Param[1] > 1.0, Param[1], Param[1] * $FF);
          Color.B := ifv_(Param[2] > 1.0, Param[2], Param[2] * $FF);
        end;

      meRT.Step.OutData.Morphomatics := meRT.Step.InData.Raster.BuildApproximateMorphomatics(Color.BGRA);
      Result := OpRunTime.Trigger^.Name + '[ok]';
    end
  else if meRT.Step.InData.Binaryzation <> nil then
    begin
      meRT.Step.OutData.Morphomatics := meRT.Step.InData.Binaryzation.BuildMorphomatics();
      Result := OpRunTime.Trigger^.Name + '[ok]';
    end
  else
      Result := OpRunTime.Trigger^.Name + '[error: must be raster input]';
end;

function TExp_API.Filter_AVG(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  BoxW, BoxH: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics = nil then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics input]';
      exit;
    end;

  BoxW := 5;
  BoxH := 5;
  case length(Param) of
    1:
      begin
        BoxW := Param[0];
        BoxH := Param[0];
      end;
    2:
      begin
        BoxW := Param[0];
        BoxH := Param[1];
      end;
  end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Morphomatics.Average(BoxW, BoxH);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Filter_WeightedAVG(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  BoxW, BoxH: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics = nil then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics input]';
      exit;
    end;

  BoxW := 5;
  BoxH := 5;
  case length(Param) of
    1:
      begin
        BoxW := Param[0];
        BoxH := Param[0];
      end;
    2:
      begin
        BoxW := Param[0];
        BoxH := Param[1];
      end;
  end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Morphomatics.WeightedAVG(BoxW, BoxH);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Filter_GeometricMean(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  BoxW, BoxH: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics = nil then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics input]';
      exit;
    end;

  BoxW := 5;
  BoxH := 5;
  case length(Param) of
    1:
      begin
        BoxW := Param[0];
        BoxH := Param[0];
      end;
    2:
      begin
        BoxW := Param[0];
        BoxH := Param[1];
      end;
  end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Morphomatics.GeometricMean(BoxW, BoxH);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Filter_Median(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  BoxW, BoxH: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics = nil then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics input]';
      exit;
    end;

  BoxW := 5;
  BoxH := 5;
  case length(Param) of
    1:
      begin
        BoxW := Param[0];
        BoxH := Param[0];
      end;
    2:
      begin
        BoxW := Param[0];
        BoxH := Param[1];
      end;
  end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Morphomatics.Median(BoxW, BoxH);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Filter_Max(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  BoxW, BoxH: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics = nil then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics input]';
      exit;
    end;

  BoxW := 5;
  BoxH := 5;
  case length(Param) of
    1:
      begin
        BoxW := Param[0];
        BoxH := Param[0];
      end;
    2:
      begin
        BoxW := Param[0];
        BoxH := Param[1];
      end;
  end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Morphomatics.Maximum(BoxW, BoxH);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Filter_Min(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  BoxW, BoxH: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics = nil then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics input]';
      exit;
    end;

  BoxW := 5;
  BoxH := 5;
  case length(Param) of
    1:
      begin
        BoxW := Param[0];
        BoxH := Param[0];
      end;
    2:
      begin
        BoxW := Param[0];
        BoxH := Param[1];
      end;
  end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Morphomatics.Minimum(BoxW, BoxH);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Filter_MiddlePoint(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  BoxW, BoxH: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics = nil then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics input]';
      exit;
    end;

  BoxW := 5;
  BoxH := 5;
  case length(Param) of
    1:
      begin
        BoxW := Param[0];
        BoxH := Param[0];
      end;
    2:
      begin
        BoxW := Param[0];
        BoxH := Param[1];
      end;
  end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Morphomatics.MiddlePoint(BoxW, BoxH);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Filter_TruncatedAVG(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  BoxW, BoxH, d: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics = nil then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics input]';
      exit;
    end;

  BoxW := 5;
  BoxH := 5;
  d := 2;
  case length(Param) of
    1:
      begin
        BoxW := Param[0];
        BoxH := Param[0];
        d := BoxW div 2;
      end;
    2:
      begin
        BoxW := Param[0];
        BoxH := Param[1];
        d := BoxW div 2;
      end;
    3:
      begin
        BoxW := Param[0];
        BoxH := Param[1];
        d := Param[2];
      end;
  end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Morphomatics.TruncatedAVG(BoxW, BoxH, 3);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Filter_Previtt(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  AdditiveToOriginal: Boolean;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics = nil then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics input]';
      exit;
    end;

  AdditiveToOriginal := False;
  case length(Param) of
    1: AdditiveToOriginal := Param[0];
  end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Morphomatics.Previtt(AdditiveToOriginal);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Filter_Sobel(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  AdditiveToOriginal: Boolean;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics = nil then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics input]';
      exit;
    end;

  AdditiveToOriginal := False;
  case length(Param) of
    1: AdditiveToOriginal := Param[0];
  end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Morphomatics.Sobel(AdditiveToOriginal);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Filter_Sharr(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  AdditiveToOriginal: Boolean;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics = nil then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics input]';
      exit;
    end;

  AdditiveToOriginal := False;
  case length(Param) of
    1: AdditiveToOriginal := Param[0];
  end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Morphomatics.Sharr(AdditiveToOriginal);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Filter_Laplace(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  AdditiveToOriginal: Boolean;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics = nil then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics input]';
      exit;
    end;

  AdditiveToOriginal := False;
  case length(Param) of
    1: AdditiveToOriginal := Param[0];
  end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Morphomatics.Laplace(AdditiveToOriginal);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Transform_Linear(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  k, B: TMorphomaticsValue;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics = nil then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics input]';
      exit;
    end;

  k := 0.9;
  B := 0.1;
  case length(Param) of
    1:
      begin
        k := Param[0];
      end;
    2:
      begin
        k := Param[0];
        B := Param[1];
      end;
  end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Morphomatics.Linear(k, B);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Transform_Log(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  c: TMorphomaticsValue;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics = nil then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics input]';
      exit;
    end;

  c := 1.0;
  case length(Param) of
    1:
      begin
        c := Param[0];
      end;
  end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Morphomatics.Logarithms(c);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Transform_Gamma(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  c, GAMMA: TMorphomaticsValue;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics = nil then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics input]';
      exit;
    end;

  c := 0.5;
  GAMMA := 1.0;
  case length(Param) of
    1:
      begin
        c := Param[0];
      end;
    2:
      begin
        c := Param[0];
        GAMMA := Param[1];
      end;
  end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Morphomatics.GAMMA(c, GAMMA);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Transform_HistogramEqualization(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics = nil then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics input]';
      exit;
    end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Morphomatics.HistogramEqualization();
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Transform_Contrast(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  k: TMorphomaticsValue;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics = nil then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics input]';
      exit;
    end;

  k := 1.0;
  case length(Param) of
    1:
      begin
        k := Param[0];
      end;
  end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Morphomatics.Contrast(k);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Transform_Gradient(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  level: Byte;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics = nil then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics input]';
      exit;
    end;

  level := 16;
  case length(Param) of
    1:
      begin
        level := Param[0];
      end;
  end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Morphomatics.Gradient(level);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Transform_Clamp(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  MinV, MaxV: TMorphomaticsValue;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics = nil then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics input]';
      exit;
    end;

  MinV := 0.0;
  MaxV := 1.0;
  case length(Param) of
    1:
      begin
        MinV := Param[0];
      end;
    2:
      begin
        MinV := Param[0];
        MaxV := Param[1];
      end;
  end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Morphomatics.Clamp(MinV, MaxV);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Transform_Add(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  v: TMorphomaticsValue;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics = nil then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics input]';
      exit;
    end;

  v := 0.0;
  if length(Param) > 0 then
      v := Param[0];

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Morphomatics.Add_(v);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Transform_Sub(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  v: TMorphomaticsValue;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics = nil then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics input]';
      exit;
    end;

  v := 0.0;
  if length(Param) > 0 then
      v := Param[0];

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Morphomatics.Sub_(v);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Transform_Mul(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  v: TMorphomaticsValue;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics = nil then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics input]';
      exit;
    end;

  v := 0.0;
  if length(Param) > 0 then
      v := Param[0];

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Morphomatics.Mul_(v);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Transform_Div(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  v: TMorphomaticsValue;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics = nil then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics input]';
      exit;
    end;

  v := 0.0;
  if length(Param) > 0 then
      v := Param[0];

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Morphomatics.Div_(v);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Transform_LFCombineAdd(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  i: Integer;
  n: U_String;
  step_: TMorphExpStep;
  w, h: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  w := 0;
  h := 0;

  // check
  for i := Low(Param) to High(Param) do
    begin
      n := umlVarToStr(Param[i], False);
      step_ := FindStep(meRT, n);
      if (step_ <> nil) and (step_.OutData.FoundData > 0) and (step_.OutData.Morphomatics <> nil) then
        begin
          w := umlMax(w, step_.OutData.Morphomatics.Width);
          h := umlMax(h, step_.OutData.Morphomatics.Height);
        end
      else
        begin
          Result := OpRunTime.Trigger^.Name + PFormat('[error: "%s" must be Morphomatics input]', [n.Text]);
          exit;
        end;
    end;

  meRT.Step.OutData.Clear;
  meRT.Step.OutData.Morphomatics := TMorphomatics.Create;
  meRT.Step.OutData.Morphomatics.SetSize(w, h, 0);
  meRT.Step.OutData.Morphomatics.Add_(FindStep(meRT, umlVarToStr(Param[Low(Param)], False)).OutData.Morphomatics);

  // fill
  for i := Low(Param) + 1 to High(Param) do
      meRT.Step.OutData.Morphomatics.Add_(FindStep(meRT, umlVarToStr(Param[i], False)).OutData.Morphomatics);

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Transform_LFCombineSub(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  i: Integer;
  n: U_String;
  step_: TMorphExpStep;
  w, h: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  w := 0;
  h := 0;

  // check
  for i := Low(Param) to High(Param) do
    begin
      n := umlVarToStr(Param[i], False);
      step_ := FindStep(meRT, n);
      if (step_ <> nil) and (step_.OutData.FoundData > 0) and (step_.OutData.Morphomatics <> nil) then
        begin
          w := umlMax(w, step_.OutData.Morphomatics.Width);
          h := umlMax(h, step_.OutData.Morphomatics.Height);
        end
      else
        begin
          Result := OpRunTime.Trigger^.Name + PFormat('[error: "%s" must be Morphomatics input]', [n.Text]);
          exit;
        end;
    end;

  meRT.Step.OutData.Clear;
  meRT.Step.OutData.Morphomatics := TMorphomatics.Create;
  meRT.Step.OutData.Morphomatics.SetSize(w, h, 0);
  meRT.Step.OutData.Morphomatics.Add_(FindStep(meRT, umlVarToStr(Param[Low(Param)], False)).OutData.Morphomatics);

  // fill
  for i := Low(Param) + 1 to High(Param) do
      meRT.Step.OutData.Morphomatics.Sub_(FindStep(meRT, umlVarToStr(Param[i], False)).OutData.Morphomatics);

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Transform_LFCombineMul(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  i: Integer;
  n: U_String;
  step_: TMorphExpStep;
  w, h: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  w := 0;
  h := 0;

  // check
  for i := Low(Param) to High(Param) do
    begin
      n := umlVarToStr(Param[i], False);
      step_ := FindStep(meRT, n);
      if (step_ <> nil) and (step_.OutData.FoundData > 0) and (step_.OutData.Morphomatics <> nil) then
        begin
          w := umlMax(w, step_.OutData.Morphomatics.Width);
          h := umlMax(h, step_.OutData.Morphomatics.Height);
        end
      else
        begin
          Result := OpRunTime.Trigger^.Name + PFormat('[error: "%s" must be Morphomatics input]', [n.Text]);
          exit;
        end;
    end;

  meRT.Step.OutData.Clear;
  meRT.Step.OutData.Morphomatics := TMorphomatics.Create;
  meRT.Step.OutData.Morphomatics.SetSize(w, h, 0);
  meRT.Step.OutData.Morphomatics.Add_(FindStep(meRT, umlVarToStr(Param[Low(Param)], False)).OutData.Morphomatics);

  // fill
  for i := Low(Param) + 1 to High(Param) do
      meRT.Step.OutData.Morphomatics.Mul_(FindStep(meRT, umlVarToStr(Param[i], False)).OutData.Morphomatics);

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Transform_LFCombineDiv(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  i: Integer;
  n: U_String;
  step_: TMorphExpStep;
  w, h: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  w := 0;
  h := 0;

  // check
  for i := Low(Param) to High(Param) do
    begin
      n := umlVarToStr(Param[i], False);
      step_ := FindStep(meRT, n);
      if (step_ <> nil) and (step_.OutData.FoundData > 0) and (step_.OutData.Morphomatics <> nil) then
        begin
          w := umlMax(w, step_.OutData.Morphomatics.Width);
          h := umlMax(h, step_.OutData.Morphomatics.Height);
        end
      else
        begin
          Result := OpRunTime.Trigger^.Name + PFormat('[error: "%s" must be Morphomatics input]', [n.Text]);
          exit;
        end;
    end;

  meRT.Step.OutData.Clear;
  meRT.Step.OutData.Morphomatics := TMorphomatics.Create;
  meRT.Step.OutData.Morphomatics.SetSize(w, h, 0);
  meRT.Step.OutData.Morphomatics.Add_(FindStep(meRT, umlVarToStr(Param[Low(Param)], False)).OutData.Morphomatics);

  // fill
  for i := Low(Param) + 1 to High(Param) do
      meRT.Step.OutData.Morphomatics.Div_(FindStep(meRT, umlVarToStr(Param[i], False)).OutData.Morphomatics);

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Transform_RFCombineAdd(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  i: Integer;
  n: U_String;
  step_: TMorphExpStep;
  w, h: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  w := 0;
  h := 0;

  // check
  for i := High(Param) downto Low(Param) do
    begin
      n := umlVarToStr(Param[i], False);
      step_ := FindStep(meRT, n);
      if (step_ <> nil) and (step_.OutData.FoundData > 0) and (step_.OutData.Morphomatics <> nil) then
        begin
          w := umlMax(w, step_.OutData.Morphomatics.Width);
          h := umlMax(h, step_.OutData.Morphomatics.Height);
        end
      else
        begin
          Result := OpRunTime.Trigger^.Name + PFormat('[error: "%s" must be Morphomatics input]', [n.Text]);
          exit;
        end;
    end;

  meRT.Step.OutData.Clear;
  meRT.Step.OutData.Morphomatics := TMorphomatics.Create;
  meRT.Step.OutData.Morphomatics.SetSize(w, h, 0);
  meRT.Step.OutData.Morphomatics.Add_(FindStep(meRT, umlVarToStr(Param[High(Param)], False)).OutData.Morphomatics);

  // fill
  for i := High(Param) - 1 downto Low(Param) do
      meRT.Step.OutData.Morphomatics.Add_(FindStep(meRT, umlVarToStr(Param[i], False)).OutData.Morphomatics);

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Transform_RFCombineSub(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  i: Integer;
  n: U_String;
  step_: TMorphExpStep;
  w, h: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  w := 0;
  h := 0;

  // check
  for i := High(Param) downto Low(Param) do
    begin
      n := umlVarToStr(Param[i], False);
      step_ := FindStep(meRT, n);
      if (step_ <> nil) and (step_.OutData.FoundData > 0) and (step_.OutData.Morphomatics <> nil) then
        begin
          w := umlMax(w, step_.OutData.Morphomatics.Width);
          h := umlMax(h, step_.OutData.Morphomatics.Height);
        end
      else
        begin
          Result := OpRunTime.Trigger^.Name + PFormat('[error: "%s" must be Morphomatics input]', [n.Text]);
          exit;
        end;
    end;

  meRT.Step.OutData.Clear;
  meRT.Step.OutData.Morphomatics := TMorphomatics.Create;
  meRT.Step.OutData.Morphomatics.SetSize(w, h, 0);
  meRT.Step.OutData.Morphomatics.Add_(FindStep(meRT, umlVarToStr(Param[High(Param)], False)).OutData.Morphomatics);

  // fill
  for i := High(Param) - 1 downto Low(Param) do
      meRT.Step.OutData.Morphomatics.Sub_(FindStep(meRT, umlVarToStr(Param[i], False)).OutData.Morphomatics);

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Transform_RFCombineMul(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  i: Integer;
  n: U_String;
  step_: TMorphExpStep;
  w, h: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  w := 0;
  h := 0;

  // check
  for i := High(Param) downto Low(Param) do
    begin
      n := umlVarToStr(Param[i], False);
      step_ := FindStep(meRT, n);
      if (step_ <> nil) and (step_.OutData.FoundData > 0) and (step_.OutData.Morphomatics <> nil) then
        begin
          w := umlMax(w, step_.OutData.Morphomatics.Width);
          h := umlMax(h, step_.OutData.Morphomatics.Height);
        end
      else
        begin
          Result := OpRunTime.Trigger^.Name + PFormat('[error: "%s" must be Morphomatics input]', [n.Text]);
          exit;
        end;
    end;

  meRT.Step.OutData.Clear;
  meRT.Step.OutData.Morphomatics := TMorphomatics.Create;
  meRT.Step.OutData.Morphomatics.SetSize(w, h, 0);
  meRT.Step.OutData.Morphomatics.Add_(FindStep(meRT, umlVarToStr(Param[High(Param)], False)).OutData.Morphomatics);

  // fill
  for i := High(Param) - 1 downto Low(Param) do
      meRT.Step.OutData.Morphomatics.Mul_(FindStep(meRT, umlVarToStr(Param[i], False)).OutData.Morphomatics);

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Transform_RFCombineDiv(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  i: Integer;
  n: U_String;
  step_: TMorphExpStep;
  w, h: Integer;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  w := 0;
  h := 0;

  // check
  for i := High(Param) downto Low(Param) do
    begin
      n := umlVarToStr(Param[i], False);
      step_ := FindStep(meRT, n);
      if (step_ <> nil) and (step_.OutData.FoundData > 0) and (step_.OutData.Morphomatics <> nil) then
        begin
          w := umlMax(w, step_.OutData.Morphomatics.Width);
          h := umlMax(h, step_.OutData.Morphomatics.Height);
        end
      else
        begin
          Result := OpRunTime.Trigger^.Name + PFormat('[error: "%s" must be Morphomatics input]', [n.Text]);
          exit;
        end;
    end;

  meRT.Step.OutData.Clear;
  meRT.Step.OutData.Morphomatics := TMorphomatics.Create;
  meRT.Step.OutData.Morphomatics.SetSize(w, h, 0);
  meRT.Step.OutData.Morphomatics.Add_(FindStep(meRT, umlVarToStr(Param[High(Param)], False)).OutData.Morphomatics);

  // fill
  for i := High(Param) - 1 downto Low(Param) do
      meRT.Step.OutData.Morphomatics.Div_(FindStep(meRT, umlVarToStr(Param[i], False)).OutData.Morphomatics);

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Transform_MorphFromPolygon(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  InsideValue, OutsideValue: TMorphomaticsValue;
  buff: TArrayVec2;
  poly: T2DPolygon;
  Morph: TMorphMath;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;
  if meRT.Step.InData.Morphomatics = nil then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics input]';
      exit;
    end;

  if length(Param) <> 3 then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: parameter illegal]';
      exit;
    end;

  InsideValue := Param[0];
  OutsideValue := Param[1];
  buff := StrToArrayVec2(Param[2]);
  poly := T2DPolygon.Create;
  poly.GiveListDataFromBuff(buff);
  SetLength(buff, 0);
  Morph := TMorphMath.Create;
  Morph.SetSize(meRT.Step.InData.Morphomatics.Width, meRT.Step.InData.Morphomatics.Height);
  Morph.FillValueFromPolygon(poly, InsideValue, OutsideValue);
  DisposeObject(poly);
  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Morphomatics.Mul_(Morph);
  DisposeObject(Morph);
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Binarization(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  Thresold: TMorphomaticsValue;
  Morph: TMorphMath;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Raster = nil) and (meRT.Step.InData.Morphomatics = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be raster or Morphomatics input]';
      exit;
    end;

  Thresold := 0.5;
  case length(Param) of
    1: Thresold := Param[0];
  end;

  if meRT.Step.InData.Morphomatics <> nil then
    begin
      meRT.Step.OutData.Binaryzation := meRT.Step.InData.Morphomatics.Binarization(Thresold);
    end;
  if meRT.Step.InData.Raster <> nil then
    begin
      Morph := meRT.Step.InData.Raster.BuildMorphomatics(TMorphPixel.mpYIQ_Y);
      meRT.Step.OutData.Binaryzation := Morph.Binarization(Thresold);
      DisposeObject(Morph);
    end;

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Binarization_InRange(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  Min_, Max_: TMorphomaticsValue;
  Morph: TMorphMath;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Raster = nil) and (meRT.Step.InData.Morphomatics = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be raster or Morphomatics input]';
      exit;
    end;

  Min_ := 0.1;
  Max_ := 0.9;
  case length(Param) of
    1:
      begin
        Min_ := Param[0];
      end;
    2:
      begin
        Min_ := Param[0];
        Max_ := Param[1];
      end;
  end;

  if meRT.Step.InData.Morphomatics <> nil then
    begin
      meRT.Step.OutData.Binaryzation := meRT.Step.InData.Morphomatics.Binarization_InRange(Min_, Max_);
    end;
  if meRT.Step.InData.Raster <> nil then
    begin
      Morph := meRT.Step.InData.Raster.BuildMorphomatics(TMorphPixel.mpYIQ_Y);
      meRT.Step.OutData.Binaryzation := Morph.Binarization_InRange(Min_, Max_);
      DisposeObject(Morph);
    end;

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Binarization_Bernsen(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  r: Integer;
  ContrastThresold: TMorphomaticsValue;
  Morph: TMorphMath;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Raster = nil) and (meRT.Step.InData.Morphomatics = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be raster or Morphomatics input]';
      exit;
    end;

  r := 5;
  ContrastThresold := 0.5;
  case length(Param) of
    1:
      begin
        r := Param[0];
      end;
    2:
      begin
        r := Param[0];
        ContrastThresold := Param[1];
      end;
  end;

  if meRT.Step.InData.Morphomatics <> nil then
    begin
      meRT.Step.OutData.Binaryzation := meRT.Step.InData.Morphomatics.Binarization_Bernsen(r, ContrastThresold);
    end;
  if meRT.Step.InData.Raster <> nil then
    begin
      Morph := meRT.Step.InData.Raster.BuildMorphomatics(TMorphPixel.mpYIQ_Y);
      meRT.Step.OutData.Binaryzation := Morph.Binarization_Bernsen(r, ContrastThresold);
      DisposeObject(Morph);
    end;
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Binarization_FloydSteinbergDithering(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  Morph: TMorphMath;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Raster = nil) and (meRT.Step.InData.Morphomatics = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be raster or Morphomatics input]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics <> nil then
    begin
      meRT.Step.OutData.Binaryzation := meRT.Step.InData.Morphomatics.Binarization_FloydSteinbergDithering();
    end;
  if meRT.Step.InData.Raster <> nil then
    begin
      Morph := meRT.Step.InData.Raster.BuildMorphomatics(TMorphPixel.mpYIQ_Y);
      meRT.Step.OutData.Binaryzation := Morph.Binarization_FloydSteinbergDithering();
      DisposeObject(Morph);
    end;
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.Binarization_OTSU(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  Morph: TMorphMath;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Raster = nil) and (meRT.Step.InData.Morphomatics = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be raster or Morphomatics input]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics <> nil then
    begin
      meRT.Step.OutData.Binaryzation := meRT.Step.InData.Morphomatics.Binarization_OTSU();
    end;
  if meRT.Step.InData.Raster <> nil then
    begin
      Morph := meRT.Step.InData.Raster.BuildMorphomatics(TMorphPixel.mpYIQ_Y);
      meRT.Step.OutData.Binaryzation := Morph.Binarization_OTSU();
      DisposeObject(Morph);
    end;
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_Invert(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Morphomatics = nil) and (meRT.Step.InData.Binaryzation = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics or Binaryzation input]';
      exit;
    end;

  meRT.Step.OutData.Assign(meRT.Step.InData);

  if meRT.Step.InData.Morphomatics <> nil then
      meRT.Step.OutData.Morphomatics.Invert();
  if meRT.Step.InData.Binaryzation <> nil then
      meRT.Step.OutData.Binaryzation.Invert();

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_Dilatation(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  CKW, CKH: Integer;
  ConvolutionKernel_: TMorphologyBinaryzation;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Morphomatics = nil) and (meRT.Step.InData.Binaryzation = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics or Binaryzation input]';
      exit;
    end;

  CKW := 5;
  CKH := 5;
  case length(Param) of
    1:
      begin
        CKW := Param[0];
        CKH := Param[0];
      end;
    2:
      begin
        CKW := Param[0];
        CKH := Param[1];
      end;
  end;

  ConvolutionKernel_ := TMorphologyBinaryzation.Create;
  ConvolutionKernel_.SetConvolutionSize(CKW, CKH, True);
  meRT.Step.OutData.Assign(meRT.Step.InData);

  if meRT.Step.InData.Morphomatics <> nil then
      meRT.Step.OutData.Morphomatics.Dilatation(ConvolutionKernel_);
  if meRT.Step.InData.Binaryzation <> nil then
      meRT.Step.OutData.Binaryzation.Dilatation(ConvolutionKernel_);

  DisposeObject(ConvolutionKernel_);

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_Erosion(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  CKW, CKH: Integer;
  ConvolutionKernel_: TMorphologyBinaryzation;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Morphomatics = nil) and (meRT.Step.InData.Binaryzation = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics or Binaryzation input]';
      exit;
    end;

  CKW := 5;
  CKH := 5;
  case length(Param) of
    1:
      begin
        CKW := Param[0];
        CKH := Param[0];
      end;
    2:
      begin
        CKW := Param[0];
        CKH := Param[1];
      end;
  end;

  ConvolutionKernel_ := TMorphologyBinaryzation.Create;
  ConvolutionKernel_.SetConvolutionSize(CKW, CKH, True);
  meRT.Step.OutData.Assign(meRT.Step.InData);

  if meRT.Step.InData.Morphomatics <> nil then
      meRT.Step.OutData.Morphomatics.Erosion(ConvolutionKernel_);
  if meRT.Step.InData.Binaryzation <> nil then
      meRT.Step.OutData.Binaryzation.Erosion(ConvolutionKernel_);

  DisposeObject(ConvolutionKernel_);

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_Opening(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  CKW, CKH: Integer;
  ConvolutionKernel_: TMorphologyBinaryzation;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Morphomatics = nil) and (meRT.Step.InData.Binaryzation = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics or Binaryzation input]';
      exit;
    end;

  CKW := 5;
  CKH := 5;
  case length(Param) of
    1:
      begin
        CKW := Param[0];
        CKH := Param[0];
      end;
    2:
      begin
        CKW := Param[0];
        CKH := Param[1];
      end;
  end;

  ConvolutionKernel_ := TMorphologyBinaryzation.Create;
  ConvolutionKernel_.SetConvolutionSize(CKW, CKH, True);
  meRT.Step.OutData.Assign(meRT.Step.InData);

  if meRT.Step.InData.Morphomatics <> nil then
      meRT.Step.OutData.Morphomatics.Opening(ConvolutionKernel_);
  if meRT.Step.InData.Binaryzation <> nil then
      meRT.Step.OutData.Binaryzation.Opening(ConvolutionKernel_);

  DisposeObject(ConvolutionKernel_);

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_Closing(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  CKW, CKH: Integer;
  ConvolutionKernel_: TMorphologyBinaryzation;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Morphomatics = nil) and (meRT.Step.InData.Binaryzation = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics or Binaryzation input]';
      exit;
    end;

  CKW := 5;
  CKH := 5;
  case length(Param) of
    1:
      begin
        CKW := Param[0];
        CKH := Param[0];
      end;
    2:
      begin
        CKW := Param[0];
        CKH := Param[1];
      end;
  end;

  ConvolutionKernel_ := TMorphologyBinaryzation.Create;
  ConvolutionKernel_.SetConvolutionSize(CKW, CKH, True);
  meRT.Step.OutData.Assign(meRT.Step.InData);

  if meRT.Step.InData.Morphomatics <> nil then
      meRT.Step.OutData.Morphomatics.Closing(ConvolutionKernel_);
  if meRT.Step.InData.Binaryzation <> nil then
      meRT.Step.OutData.Binaryzation.Closing(ConvolutionKernel_);

  DisposeObject(ConvolutionKernel_);

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_OpeningAndClosing(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  CKW, CKH: Integer;
  ConvolutionKernel_: TMorphologyBinaryzation;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Morphomatics = nil) and (meRT.Step.InData.Binaryzation = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics or Binaryzation input]';
      exit;
    end;

  CKW := 5;
  CKH := 5;
  case length(Param) of
    1:
      begin
        CKW := Param[0];
        CKH := Param[0];
      end;
    2:
      begin
        CKW := Param[0];
        CKH := Param[1];
      end;
  end;

  ConvolutionKernel_ := TMorphologyBinaryzation.Create;
  ConvolutionKernel_.SetConvolutionSize(CKW, CKH, True);
  meRT.Step.OutData.Assign(meRT.Step.InData);

  if meRT.Step.InData.Morphomatics <> nil then
      meRT.Step.OutData.Morphomatics.OpeningAndClosing(ConvolutionKernel_);
  if meRT.Step.InData.Binaryzation <> nil then
      meRT.Step.OutData.Binaryzation.OpeningAndClosing(ConvolutionKernel_);

  DisposeObject(ConvolutionKernel_);

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_ClosingAndOpening(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  CKW, CKH: Integer;
  ConvolutionKernel_: TMorphologyBinaryzation;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Morphomatics = nil) and (meRT.Step.InData.Binaryzation = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics or Binaryzation input]';
      exit;
    end;

  CKW := 5;
  CKH := 5;
  case length(Param) of
    1:
      begin
        CKW := Param[0];
        CKH := Param[0];
      end;
    2:
      begin
        CKW := Param[0];
        CKH := Param[1];
      end;
  end;

  ConvolutionKernel_ := TMorphologyBinaryzation.Create;
  ConvolutionKernel_.SetConvolutionSize(CKW, CKH, True);
  meRT.Step.OutData.Assign(meRT.Step.InData);

  if meRT.Step.InData.Morphomatics <> nil then
      meRT.Step.OutData.Morphomatics.ClosingAndOpening(ConvolutionKernel_);
  if meRT.Step.InData.Binaryzation <> nil then
      meRT.Step.OutData.Binaryzation.ClosingAndOpening(ConvolutionKernel_);

  DisposeObject(ConvolutionKernel_);

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_Skeleton(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  CKW, CKH: Integer;
  ConvolutionKernel_: TMorphologyBinaryzation;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Binaryzation = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Binaryzation input]';
      exit;
    end;

  CKW := 5;
  CKH := 5;
  case length(Param) of
    1:
      begin
        CKW := Param[0];
        CKH := Param[0];
      end;
    2:
      begin
        CKW := Param[0];
        CKH := Param[1];
      end;
  end;

  ConvolutionKernel_ := TMorphologyBinaryzation.Create;
  ConvolutionKernel_.SetConvolutionSize(CKW, CKH, True);

  meRT.Step.OutData.Assign(meRT.Step.InData);
  meRT.Step.OutData.Binaryzation.Skeleton(ConvolutionKernel_);
  DisposeObject(ConvolutionKernel_);

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_SetOutput(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  Thresold: TMorphomaticsValue;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.OutData = nil) or (meRT.Step.OutData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  meRT.MorphExp.FOutData := meRT.Step.OutData;
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TExp_API.MorphExp_ViewHistogram(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
  Thresold: TMorphomaticsValue;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if meRT.Step.InData.Morphomatics = nil then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be Morphomatics input]';
      exit;
    end;

  meRT.Step.OutData.Raster := meRT.Step.InData.Morphomatics.BuildHistogram(100, RColorF(1, 1, 1));
  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

constructor TMorphExpRunTime.Create;
var
  i: Integer;
begin
  inherited Create;
  Step := nil;
  API := TExp_API.Create;
  API.RegInternalAPI(Self);
  for i := 0 to ExternalAPIDataList_.Count - 1 do
    begin
      if Assigned(ExternalAPIDataList_[i]^.OnRegExternalAPI_C) then
          ExternalAPIDataList_[i]^.OnRegExternalAPI_C(Self);

      if Assigned(ExternalAPIDataList_[i]^.OnRegExternalAPI_M) then
          ExternalAPIDataList_[i]^.OnRegExternalAPI_M(Self);

      if Assigned(ExternalAPIDataList_[i]^.OnRegExternalAPI_P) then
          ExternalAPIDataList_[i]^.OnRegExternalAPI_P(Self);
    end;
end;

destructor TMorphExpRunTime.Destroy;
begin
  DisposeObject(API);
  inherited Destroy;
end;

function TMorphExpRunTime.MorphExp: TMorphExp;
begin
  Result := Step.MorphExp;
end;

constructor TMorphData.Create;
begin
  inherited Create;
  Raster := nil;
  Segmentation := nil;
  Binaryzation := nil;
  Morphomatics := nil;
  DebugViewer := nil;
end;

destructor TMorphData.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TMorphData.Clear;
begin
  try
      DisposeObjectAndNil(Raster);
  except
  end;

  try
      DisposeObjectAndNil(Segmentation);
  except
  end;

  try
      DisposeObjectAndNil(Binaryzation);
  except
  end;

  try
      DisposeObjectAndNil(Morphomatics);
  except
  end;

  try
      DisposeObjectAndNil(DebugViewer);
  except
  end;
end;

function TMorphData.FoundData: Integer;
begin
  Result := 0;

  if Raster <> nil then
      inc(Result);
  if Segmentation <> nil then
      inc(Result);
  if Binaryzation <> nil then
      inc(Result);
  if Morphomatics <> nil then
      inc(Result);
end;

function TMorphData.Assign(sour: TMorphData): Integer;
begin
  Clear;
  Result := 0;

  if sour.Raster <> nil then
    begin
      Raster := NewRaster();
      Raster.Assign(sour.Raster);
      inc(Result);
    end;
  if sour.Segmentation <> nil then
    begin
      Segmentation := TMorphologySegmentation.Create;
      Segmentation.Assign(sour.Segmentation);
      inc(Result);
    end;
  if sour.Binaryzation <> nil then
    begin
      Binaryzation := TMorphologyBinaryzation.Create;
      Binaryzation.Assign(sour.Binaryzation);
      inc(Result);
    end;
  if sour.Morphomatics <> nil then
    begin
      Morphomatics := TMorphomatics.Create;
      Morphomatics.Assign(sour.Morphomatics);
      inc(Result);
    end;
end;

procedure TMorphData.BuildDebugViewer;
var
  rasterLst: TMemoryRasterList;
  nR: TMemoryRaster;
  i: Integer;
  L, w, h: Integer;
  r: TRectV2;
  tmpFinal: TMemoryRaster;
begin
  DisposeObjectAndNil(DebugViewer);
  tmpFinal := nil;

  rasterLst := TMemoryRasterList.Create;
  if Raster <> nil then
    begin
      nR := NewRaster();
      nR.Assign(Raster);
      rasterLst.Add(nR);
    end;
  if Segmentation <> nil then
    begin
      nR := Segmentation.BuildViewer();
      rasterLst.Add(nR);
    end;
  if Binaryzation <> nil then
    begin
      nR := Binaryzation.BuildViewer();
      rasterLst.Add(nR);
    end;
  if Morphomatics <> nil then
    begin
      nR := Morphomatics.BuildViewer();
      rasterLst.Add(nR);
    end;

  if rasterLst.Count > 0 then
    begin
      if rasterLst.Count = 1 then
          tmpFinal := rasterLst.First
      else
        begin
          w := 0;
          h := 0;
          for i := 0 to rasterLst.Count - 1 do
            begin
              inc(w, rasterLst[i].Width + 1);
              h := umlMax(rasterLst[i].Height + 10, h);
            end;
          tmpFinal := NewRaster();
          tmpFinal.SetSize(w, h, RColor(0, 0, 0, 0));

          L := 0;
          for i := 0 to rasterLst.Count - 1 do
            begin
              r[0] := Vec2(L, (h - rasterLst[i].Height) div 2);
              r[1] := Vec2Add(r[0], rasterLst[i].Size2D);
              tmpFinal.FillRect(r, RColorF(0.5, 0.5, 0.5, 1.0));
              rasterLst[i].DrawTo(tmpFinal, Round(r[0, 0]), Round(r[0, 1]));
              inc(L, rasterLst[i].Width + 1);
            end;

          for i := 0 to rasterLst.Count - 1 do
              DisposeObject(rasterLst[i]);
        end;
    end;
  DisposeObject(rasterLst);
  DebugViewer := tmpFinal;
end;

constructor TMorphExpStep.Create(exp_: TMorphExp);
begin
  inherited Create;
  ExpStyle := TTextStyle.tsPascal;
  ExpStr := '';
  ExpResult := '';
  MorphExp := exp_;
  InData := nil;
  OutData := TMorphData.Create;
  token := '';
end;

destructor TMorphExpStep.Destroy;
begin
  DisposeObject(OutData);
  inherited Destroy;
end;

procedure TMorphExpStep.ResetData;
begin
  InData := nil;
  OutData.Clear;
end;

constructor TMorphExpSteps.Create;
begin
  inherited Create;
end;

destructor TMorphExpSteps.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TMorphExpSteps.Remove(obj: TMorphExpStep);
begin
  DisposeObject(obj);
  inherited Remove(obj);
end;

procedure TMorphExpSteps.Delete(index: Integer);
begin
  if (index >= 0) and (index < Count) then
    begin
      DisposeObject(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TMorphExpSteps.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      DisposeObject(Items[i]);
  inherited Clear;
end;

function TMorphExpSteps.FindTokenStep(token: U_String): TMorphExpStep;
var
  i: Integer;
begin
  if token.L > 0 then
    for i := 0 to Count - 1 do
      if token.Same(@Items[i].token) then
        begin
          Result := Items[i];
          exit;
        end;
  Result := nil;
end;

constructor TMorphExp.Create;
var
  MorphPix_: TMorphologyPixel;
begin
  inherited Create;
  FSteps := TMorphExpSteps.Create;
  FInData := TMorphData.Create;
  FOutData := nil;
  FShowLog := True;
  VL_Const := THashVariantList.CustomCreate($FF);

  for MorphPix_ in [mpGrayscale,
    mpYIQ_Y, mpYIQ_I, mpYIQ_Q,
    mpHSI_H, mpHSI_S, mpHSI_I,
    mpCMYK_C, mpCMYK_M, mpCMYK_Y, mpCMYK_K,
    mpR, mpG, mpB, mpA] do
      VL_Const[GetEnumName(TypeInfo(TMorphologyPixel), Integer(MorphPix_))] := MorphPix_;
end;

destructor TMorphExp.Destroy;
begin
  DisposeObject(FInData);
  DisposeObject(FSteps);
  DisposeObject(VL_Const);
  inherited Destroy;
end;

procedure TMorphExp.Reset;
begin
  FInData.Clear;
  FSteps.Clear;
end;

procedure TMorphExp.InputRaster(fileName: SystemString);
begin
  FInData.Clear;
  FInData.Raster := NewRasterFromFile(fileName);
end;

procedure TMorphExp.InputRaster(stream: TCoreClassStream);
begin
  FInData.Clear;
  FInData.Raster := NewRasterFromStream(stream);
end;

procedure TMorphExp.InputRaster(Raster: TMemoryRaster);
begin
  FInData.Clear;
  FInData.Raster := NewRaster();
  FInData.Raster.Assign(Raster);
end;

procedure TMorphExp.AddExp(Exp: U_String);
begin
  AddPascalExp(Exp);
end;

procedure TMorphExp.AddPascalExp(Exp: U_String);
var
  s: TMorphExpStep;
begin
  if Exp.TrimChar(#9#32).L = 0 then
      exit;
  s := TMorphExpStep.Create(Self);
  s.ExpStyle := tsPascal;
  s.ExpStr := Exp;
  FSteps.Add(s);
end;

procedure TMorphExp.AddCExp(Exp: U_String);
var
  s: TMorphExpStep;
begin
  if Exp.TrimChar(#9#32).L = 0 then
      exit;
  s := TMorphExpStep.Create(Self);
  s.ExpStyle := tsC;
  s.ExpStr := Exp;
  FSteps.Add(s);
end;

function TMorphExp.Run(): Boolean;
var
  rt: TMorphExpRunTime;
  i: Integer;
  s: TMorphExpStep;
  r: Variant;
  n, expr: U_String;
  err: Boolean;
  spec: TPascalStringList;
begin
  for i := 0 to FSteps.Count - 1 do
      FSteps[i].ResetData();
  rt := TMorphExpRunTime.Create;
  err := False;
  spec := TPascalStringList.Create;
  spec.Add('&');
  for i := 0 to FSteps.Count - 1 do
    begin
      s := FSteps[i];
      rt.Step := s;

      n := s.ExpStr;
      if n.TrimChar(#32#9).L = 0 then
          n := 'in,Copy';

      try
          r := EvaluateExpressionValue(False, spec, False, s.ExpStyle, n, rt, VL_Const);
      except
      end;

      if VarIsNull(r) then
        begin
          err := True;
          if FShowLog then
              DoStatus('fatal error: ' + s.ExpStr);
          break;
        end
      else
        begin
          n.Text := umlVarToStr(r, False);
          s.ExpResult := n;
        end;
      if FShowLog then
        begin
          expr := s.ExpStr;
          if expr.L > 30 then
              expr := expr.Copy(1, 27) + '...';
          if n.L > 30 then
              n := n.Copy(1, 27) + '...';
          DoStatus(expr + ' = ' + n);
        end;
      err := n.Exists('error');
      if err then
          break;
    end;
  DisposeObject(rt);
  DisposeObject(spec);
  Result := (not err);
end;

function TMorphExp.Run(Raster: TMemoryRaster; script: U_String): Boolean;
var
  pl: TPascalStringList;
  i: Integer;
begin
  Reset;
  InputRaster(Raster);
  pl := TPascalStringList.Create;
  pl.AsText := script;
  for i := 0 to pl.Count - 1 do
      AddExp(pl[i]);
  DisposeObject(pl);
  Result := Run();
end;

function TMorphExp.CheckOutData: Boolean;
begin
  Result := (FInData.FoundData > 0) and (FOutData <> nil) and (FOutData.FoundData > 0);
end;

class procedure TMorphExp.Test;
var
  r: TMemoryRaster;
begin
  r := NewRaster();
  r.SetSize(1024, 1024, RColor(0, 0, 0));
  AddColorNoise32(r, 100);

  with TMorphExp.Create do
    begin
      InputRaster(r);
      AddExp('main');
      AddExp('Inherited,Sigma(5.5,3.5)');
      AddExp('Inherited,BuildMorphomatics(mpYIQ_Y)');
      AddExp('Inherited,Binarization(0.5),SetOutput');
      if Run() then
        begin
          // final is Binaryzation output
        end;
      Free;
    end;
  DisposeObject(r);
end;

initialization

InitExternalAPI();

finalization

FreeExternalAPI();

end.
