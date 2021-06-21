{ ****************************************************************************** }
{ * zDrawEngine                                                                 * }
{ * written by QQ 600585@qq.com                                                * }
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
unit zDrawEngine;

{$INCLUDE zDefine.inc}

interface

uses Variants, Types,
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  CoreClasses, Cadencer, Geometry2DUnit, GeometryLib, Geometry3DUnit, UnicodeMixedLib,
  ListEngine, TextDataEngine, MemoryRaster, PascalStrings, TextParsing, zExpression, OpCode,
  MemoryStream64, NotifyObjectBase;

type
{$REGION 'Base Define'}
  TDrawEngine = class;
  TDrawEngine_Raster = class;
  TEffect = class;
  TDEColor = TVec4;
  PDEColor = ^TDEColor;
  TDEVec = TVec2;
  PDEVec = ^TDEVec;
  TDERect = TRectV2;
  PDERect = ^TRectV2;
  TDEFloat = TGeoFloat;
  PDEFloat = ^TDEFloat;

  TPolyDrawOption = record
    LineColor: TDEColor;
    PointColor: TDEColor;
    LineWidth: TDEFloat;
    PointScreenRadius: TDEFloat;
  end;

  PPolyDrawOption = ^TPolyDrawOption;

  TDSegmentionText = record
    Text: SystemString;
    Size: TDEFloat;
    COLOR: TDEColor;
  end;

  TDSegmentionLine = array of TDSegmentionText;
  TDArraySegmentionText = array of TDSegmentionLine;

  TDViewerOption = (voFPS, voEdge, voPictureState, voTextBox);
  TDViewerOptions = set of TDViewerOption;
{$ENDREGION 'Base Define'}
{$REGION 'DrawEngine RectType'}
  PDE4V = ^TDE4V;

  TDE4V = record
  public
    Left, Top, Right, Bottom, Angle: TDEFloat;

    // method api
    function IsEqual(Dest: TDE4V): Boolean;
    function IsZero: Boolean;
    function width: TDEFloat;
    function height: TDEFloat;
    function MakeRectV2: TDERect; overload;
    function MakeRectf: TRectf; overload;
    function BoundRect: TDERect;
    function Centroid: TDEVec;
    function Add(v: TDEVec): TDE4V; overload;
    function Add(x, y: TDEFloat): TDE4V; overload;
    function Scale(f: TDEFloat): TDE4V; overload;
    function GetDistance(Dest: TDE4V): TDEFloat;
    function GetAngleDistance(Dest: TDE4V): TDEFloat;
    function MovementToLerp(Dest: TDE4V; mLerp, rLerp: Double): TDE4V;
    function MovementToDistance(Dest: TDE4V; mSpeed, rSpeed: TDEFloat): TDE4V;
    function MovementToDistanceCompleteTime(Dest: TDE4V; mSpeed, rSpeed: TDEFloat): Double;
    function Fit(Dest: TDE4V): TDE4V; overload;
    function Fit(Dest: TDERect): TDE4V; overload;

    // static init
    class function Init(r: TDERect; Ang: TDEFloat): TDE4V; overload; static;
    class function Init(r: TDERect): TDE4V; overload; static;
    class function Init(r: TRectf; Ang: TDEFloat): TDE4V; overload; static;
    class function Init(r: TRectf): TDE4V; overload; static;
    class function Init(r: TRect; Ang: TDEFloat): TDE4V; overload; static;
    class function Init(r: TRect): TDE4V; overload; static;
    class function Init(CenPos: TDEVec; Width_, Height_, Ang: TDEFloat): TDE4V; overload; static;
    class function Init(Width_, Height_, Ang: TDEFloat): TDE4V; overload; static;
    class function Init: TDE4V; overload; static;

    // static create
    class function Create(r: TDERect; Ang: TDEFloat): TDE4V; overload; static;
    class function Create(r: TDERect): TDE4V; overload; static;
    class function Create(r: TRectf; Ang: TDEFloat): TDE4V; overload; static;
    class function Create(r: TRectf): TDE4V; overload; static;
    class function Create(r: TRect; Ang: TDEFloat): TDE4V; overload; static;
    class function Create(r: TRect): TDE4V; overload; static;
    class function Create(CenPos: TDEVec; Width_, Height_, Ang: TDEFloat): TDE4V; overload; static;
    class function Create(Width_, Height_, Ang: TDEFloat): TDE4V; overload; static;
    class function Create: TDE4V; overload; static;
  end;
{$ENDREGION 'DrawEngine RectType'}
{$REGION 'Texture'}

  TDETexture = class(TSequenceMemoryRaster)
  protected
    LastDrawUsage: TTimeTick;
    FIsShadow: Boolean;
    FStaticShadow: TDETexture;
    FSIGMA: TGeoFloat;
    FSigmaGaussianKernelFactor: Integer;
  public
    Name: SystemString;
    constructor Create; override;
    destructor Destroy; override;

    procedure DrawUsage; virtual;
    procedure ReleaseGPUMemory; virtual;
    procedure NoUsage; override;

    function StaticShadow(): TDETexture; overload;
    function StaticShadow(const SIGMA_: TGeoFloat; const SigmaGaussianKernelFactor_: Integer): TDETexture; overload;
    property IsShadow: Boolean read FIsShadow;
  end;

  TDETextureList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TDETexture>;

  TDETextureList = class(TDETextureList_Decl)
  private
    Critical: TCritical;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddTexture(tex: TDETexture);
    procedure RemoveTexture(tex: TDETexture);
    procedure ReleaseGPUMemory;
    procedure ReleaseNoUsageTextureMemory(timeout: TTimeTick);
  end;

  TDETextureClass = class of TDETexture;
  TGetTexture = procedure(TextureOfName: SystemString; var Texture: TDETexture);

{$ENDREGION 'Texture'}
{$REGION 'DrawEngine Interface'}

  TDrawEngineInterface = class(TCoreClassObject)
  public
    procedure SetSize(r: TDERect); virtual;
    procedure SetLineWidth(w: TDEFloat); virtual;
    procedure DrawDotLine(pt1, pt2: TDEVec; COLOR: TDEColor); virtual;
    procedure DrawLine(pt1, pt2: TDEVec; COLOR: TDEColor); virtual;
    procedure DrawRect(r: TDERect; Angle: TDEFloat; COLOR: TDEColor); virtual;
    procedure FillRect(r: TDERect; Angle: TDEFloat; COLOR: TDEColor); virtual;
    procedure DrawEllipse(r: TDERect; COLOR: TDEColor); virtual;
    procedure FillEllipse(r: TDERect; COLOR: TDEColor); virtual;
    procedure FillPolygon(PolygonBuff: TArrayVec2; COLOR: TDEColor); virtual;
    procedure DrawText(Shadow: Boolean; Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean; RotateVec: TDEVec; Angle: TDEFloat); virtual;
    procedure DrawPicture(Shadow: Boolean; t: TCoreClassObject; Sour, Dest: TDE4V; Alpha: TDEFloat); virtual;
    procedure Flush; virtual;
    procedure ResetState; virtual;
    procedure BeginDraw; virtual;
    procedure EndDraw; virtual;
    function CurrentScreenSize: TDEVec; virtual;
    function GetTextSize(const Text: SystemString; Size: TDEFloat): TDEVec; virtual;
    function ReadyOK: Boolean; virtual;
  end;
{$ENDREGION 'DrawEngine Interface'}
{$REGION 'Command Sequence'}

  TDrawCommandParam_1Float = record
    f: TDEFloat;
  end;

  PDrawCommandParam_1Float = ^TDrawCommandParam_1Float;

  TDrawCommandParam_1Rect = record
    r: TDERect;
  end;

  PDrawCommandParam_1Rect = ^TDrawCommandParam_1Rect;

  TDrawCommandParam_PT_Color = record
    pt1, pt2: TDEVec;
    COLOR: TDEColor;
  end;

  PDrawCommandParam_PT_Color = ^TDrawCommandParam_PT_Color;

  TDrawCommandParam_Rect_Color = record
    r: TDERect;
    Angle: TDEFloat;
    COLOR: TDEColor;
  end;

  PDrawCommandParam_Rect_Color = ^TDrawCommandParam_Rect_Color;

  TDrawCommandParam_Polygon = record
    PolygonBuff: TArrayVec2;
    COLOR: TDEColor;
  end;

  PDrawCommandParam_Polygon = ^TDrawCommandParam_Polygon;

  TDrawCommandParam_DrawText = record
    Text: SystemString;
    Size: TDEFloat;
    r: TDERect;
    COLOR: TDEColor;
    center: Boolean;
    RotateVec: TDEVec;
    Angle: TDEFloat;
    bak_r: TDERect;
    bak_color: TDEColor;
    function IsNormal: Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  end;

  PDrawCommandParam_DrawText = ^TDrawCommandParam_DrawText;

  TDrawCommandParam_Picture = record
    t: TCoreClassObject;
    Sour, Dest: TDE4V;
    Alpha: TDEFloat;
    bak_t: TCoreClassObject;
    bak_dest: TDE4V;
    bak_alpha: TDEFloat;
    function IsNormal: Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  end;

  PDrawCommandParam_Picture = ^TDrawCommandParam_Picture;

  TCustomDraw_Method = procedure(Sender: TDrawEngine; DrawInterface: TDrawEngineInterface;
    const UserData: Pointer; const UserObject: TCoreClassObject) of object;

  TDrawCommandParam_Custom = record
    OnDraw: TCustomDraw_Method;
    UserData: Pointer;
    UserObject: TCoreClassObject;
  end;

  PDrawCommandParam_Custom = ^TDrawCommandParam_Custom;

  TDrawCommandType = (dctSetSize, dctSetLineWidth,
    dctDotLine, dctLine, dctDrawRect, dctFillRect, dctDrawEllipse, dctFillEllipse, dctPolygon,
    dctDrawText, dctDrawPicture, dctUserCustom, dctFlush);

  TDrawExecute = class;

  TDrawCommand = record
    t: TDrawCommandType;
    Data: Pointer;
    procedure DoFreeData;
    procedure Execute(OwnerDrawExecute: TDrawExecute; Draw: TDrawEngineInterface); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure CopyTo(var Dst: TDrawCommand);
  end;

  PDrawCommand = ^TDrawCommand;

  TTextureOutputState = record
    Source: TCoreClassObject;
    SourceRect, DestScreen: TV2Rect4;
    Alpha: TDEFloat;
    Normal: Boolean;
    Index: Integer;
  end;

  PTextureOutputState = ^TTextureOutputState;
  TTextureOutputStateBuffer = array of TTextureOutputState;

  TDrawQueue = class(TCoreClassObject)
  protected
    FOwner: TDrawEngine;
    FCommandList: TCoreClassList;

    FStartDrawShadowIndex: Integer;
    FScreenShadowOffset: TDEVec;
    FScreenShadowAlpha: TDEFloat;
    FShadowSIGMA: TGeoFloat;
    FShadowSigmaGaussianKernelFactor: Integer;
  public
    constructor Create(AOwner: TDrawEngine);
    destructor Destroy; override;

    procedure Assign(Source: TDrawQueue);
    // queue manager
    procedure Clear(ForceFree: Boolean);
    // post command
    procedure SetSize(r: TDERect);
    procedure SetLineWidth(w: TDEFloat);
    procedure DrawDotLine(pt1, pt2: TDEVec; COLOR: TDEColor);
    procedure DrawLine(pt1, pt2: TDEVec; COLOR: TDEColor);
    procedure DrawRect(r: TDERect; Angle: TDEFloat; COLOR: TDEColor);
    procedure FillRect(r: TDERect; Angle: TDEFloat; COLOR: TDEColor);
    procedure DrawEllipse(pt: TDEVec; radius: TDEFloat; COLOR: TDEColor); overload;
    procedure DrawEllipse(r: TDERect; COLOR: TDEColor); overload;
    procedure FillEllipse(pt: TDEVec; radius: TDEFloat; COLOR: TDEColor); overload;
    procedure FillEllipse(r: TDERect; COLOR: TDEColor); overload;
    procedure FillPolygon(PolygonBuff: TArrayVec2; COLOR: TDEColor);
    procedure DrawText(const Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean; RotateVec: TDEVec; Angle: TDEFloat);
    procedure DrawPicture(t: TCoreClassObject; Sour, Dest: TDE4V; Alpha: TDEFloat);
    procedure DrawUserCustom(const OnDraw: TCustomDraw_Method; const UserData: Pointer; const UserObject: TCoreClassObject);
    procedure Flush;

    procedure BeginCaptureShadow(const ScreenOffsetVec_: TDEVec; const alpha_: TDEFloat); overload;
    procedure BeginCaptureShadow(const ScreenOffsetVec_: TDEVec; const alpha_: TDEFloat; ShadowSIGMA_: TGeoFloat; ShadowSigmaGaussianKernelFactor_: Integer); overload;
    procedure EndCaptureShadow;

    procedure BuildTextureOutputState(var buff: TTextureOutputStateBuffer);

    property Owner: TDrawEngine read FOwner;
  end;

{$ENDREGION 'Command Sequence'}
{$REGION 'draw execute'}

  TDrawExecute = class(TCoreClassObject)
  protected
    FOwner: TDrawEngine;
    FCommandList: TCoreClassList;
  private
    FSourQueue: TDrawQueue;
    procedure Sync_PickQueue;
  public
    constructor Create(AOwner: TDrawEngine);
    destructor Destroy; override;

    procedure Clear;

    procedure PickQueue(Queue: TDrawQueue); overload;
    procedure PickQueue(Thread: TCoreClassThread; Queue: TDrawQueue); overload;
    procedure Execute(Draw: TDrawEngineInterface);
    property Owner: TDrawEngine read FOwner;
  end;
{$ENDREGION 'draw execute'}
{$REGION 'UI base'}

  TDrawEngine_UIBase = class;

  TDrawEngine_UIClick = procedure(Sender: TDrawEngine_UIBase) of object;

  TDrawEngine_UIBase = class(TCoreClassObject)
  private
  public
    DataObject: TCoreClassObject;
    DataPointer: Pointer;
    DataVariant: Variant;
    Owner: TDrawEngine;
    OnClick: TDrawEngine_UIClick;
    Visibled: Boolean;

    constructor Create(AOwner: TDrawEngine);
    destructor Destroy; override;

    function TapDown(x, y: TDEFloat): Boolean; virtual;
    function TapMove(x, y: TDEFloat): Boolean; virtual;
    function TapUp(x, y: TDEFloat): Boolean; virtual;

    procedure DoClick; virtual;

    procedure DoDraw; virtual;
  end;

  TDrawEngine_UIClass = class of TDrawEngine_UIBase;

  TDrawEngine_RectButton = class(TDrawEngine_UIBase)
  private
    Downed: Boolean;
    DownPT, MovePT, UpPT: TDEVec;
  public
    Button: TDERect;
    Text: SystemString;
    TextSize: Integer;

    constructor Create(AOwner: TDrawEngine);
    destructor Destroy; override;

    function TapDown(x, y: TDEFloat): Boolean; override;
    function TapMove(x, y: TDEFloat): Boolean; override;
    function TapUp(x, y: TDEFloat): Boolean; override;

    procedure DoDraw; override;
  end;

{$ENDREGION 'UI base'}
{$REGION 'Scroll text'}

  TScrollTextSource = class(TCoreClassObject)
  public
    LifeTime: Double;
    TextSize: Integer;
    TextColor: TDEColor;
    Text: SystemString;
    Tag: TCoreClassObject;
  end;
{$ENDREGION 'Scroll text'}
{$REGION 'Sequence animation'}

  TSequenceAnimationPlayMode = (sapmLoop, sapmPlayOne);

  TSequenceAnimationBase = class(TCoreClassObject)
  protected
    Effect: TEffect;
    Owner: TDrawEngine;
    procedure Progress(deltaTime: Double);
  public
    Source: TCoreClassObject;
    width: Integer;
    height: Integer;
    Total: Integer;
    Column: Integer;
    CompleteTime: Double;
    PlayMode: TSequenceAnimationPlayMode;
    OverAnimationSmoothTime: Double;
    flag: Variant;

    CurrentTime: Double;
    LastUsed: Boolean;

    constructor Create(AOwner: TDrawEngine); virtual;
    destructor Destroy; override;

    function SequenceAnimationPlaying: Boolean;
    function GetOverAnimationSmoothAlpha(Alpha: TDEFloat): TDEFloat;
    function IsOver: Boolean;
    function SequenceIndex: Integer;
    function SequenceFrameRect: TDE4V;
    procedure LoadFromStream(stream: TCoreClassStream);
    procedure SaveToStream(stream: TCoreClassStream);
  end;
{$ENDREGION 'Sequence animation'}
{$REGION 'Particle'}

  TParticleData = record
    Source: TSequenceAnimationBase;
    Position: TDEVec;
    radius: TDEFloat;
    Angle: TDEFloat;
    Alpha: TDEFloat;
    Acceleration: TDEFloat;
    CurrentTime: Double;
  end;

  PParticleData = ^TParticleData;

  TParticles = class(TCoreClassObject)
  protected
    Effect: TEffect;
    Owner: TDrawEngine;
    ParticleBuff: TCoreClassList;
    PrepareParticleCount: Double;
    NoEnabledAutoFree: Boolean;
    LastDrawPosition: TDEVec;

    procedure Progress(deltaTime: Double);
  public
    SequenceTexture: TCoreClassObject;
    SequenceTextureCompleteTime: Double;
    MaxParticle: Integer;
    ParticleSize: TDEFloat;
    ParticleSizeMinScale: TDEFloat;
    ParticleSizeMaxScale: TDEFloat;
    MinAlpha: TDEFloat;
    MaxAlpha: TDEFloat;
    GenerateRange: TDERect;
    Dispersion: TDEVec;
    DispersionAcceleration: TDEFloat;
    RotationOfSecond: TDEFloat;
    GenSpeedOfPerSecond: Integer;
    LifeTime: Double;
    Enabled: Boolean;
    Visible: Boolean;

    constructor Create(AOwner: TDrawEngine); virtual;
    destructor Destroy; override;

    function VisibledParticle: Integer;
    procedure FinishAndDelayFree;
    procedure LoadFromStream(stream: TCoreClassStream);
    procedure SaveToStream(stream: TCoreClassStream);
  end;

  TTParticleClass = class of TParticles;
{$ENDREGION 'Particle'}
{$REGION 'Effect'}
  TEffectMode = (emSequenceAnimation, emParticle, emNo);

  TEffect = class(TCoreClassObject)
  public
    Owner: TDrawEngine;
    Mode: TEffectMode;
    Particle: TParticles;
    SequenceAnimation: TSequenceAnimationBase;
    SequenceAnimation_Width: TDEFloat;
    SequenceAnimation_Height: TDEFloat;
    SequenceAnimation_Angle: TDEFloat;
    SequenceAnimation_Alpha: TDEFloat;

    constructor Create(AOwner: TDrawEngine); virtual;
    destructor Destroy; override;

    procedure Reset;

    procedure LoadFromStream(stream: TCoreClassStream);
    procedure SaveToStream(stream: TCoreClassStream);

    procedure Draw(Pos: TDEVec);
    procedure DrawInScene(Pos: TDEVec);
  end;
{$ENDREGION 'Effect'}
{$REGION 'DrawEngine pool'}

  PDrawEnginePoolData = ^TDrawEnginePoolData;

  TDrawEnginePoolData = record
    DrawEng: TDrawEngine;
    workObj: TCoreClassObject;
    LastActivted: TTimeTick;
  end;

  TDrawEngineClass = class of TDrawEngine;

  TDrawEnginePool = class(TCoreClassInterfacedObject, ICadencerProgressInterface)
  protected
    FCritical: TCritical;
    FDrawEngineClass: TDrawEngineClass;
    FDrawEngineList: TCoreClassList;
    FPostProgress: TNProgressPost;
    FCadEng: TCadencer;
    FLastDeltaTime: Double;
    procedure CadencerProgress(const deltaTime, newTime: Double);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure ClearActivtedTimeOut(tick: TTimeTick);

    procedure Progress(deltaTime: Double); overload;
    function Progress(): Double; overload;

    property DrawEngineClass: TDrawEngineClass read FDrawEngineClass write FDrawEngineClass;

    function GetEng(const workObj: TCoreClassObject; const Draw: TDrawEngineInterface): TDrawEngine; overload;
    function GetEng(const workObj: TCoreClassObject): TDrawEngine; overload;
    function EngNum: Integer;

    // delay run support
    property ProgressEngine: TNProgressPost read FPostProgress;
    property ProgressPost: TNProgressPost read FPostProgress;
    property PostProgress: TNProgressPost read FPostProgress;
    property PostRun: TNProgressPost read FPostProgress;
    property PostExecute: TNProgressPost read FPostProgress;

    // cadencer engine
    property CadencerEngine: TCadencer read FCadEng;
  end;
{$ENDREGION 'DrawEngine pool'}
{$REGION 'Expression'}

  TDrawTextExpressionRunTime = class(TOpCustomRunTime)
  private
    function cc(v: Variant): TDEFloat;
    function oprt_size(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function oprt_rgba(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function oprt_bgra(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function oprt_red(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function oprt_green(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function oprt_blue(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function oprt_alpha(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function oprt_r(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function oprt_g(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function oprt_b(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
    function oprt_a(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
  protected
    Size: PDEFloat;
    COLOR: PDEColor;
    constructor CustomCreate(maxHashLen: Integer); override;
    destructor Destroy; override;
    procedure InternalReg; override;
  end;

  TDrawTextExpressionRunTimeClass = class of TDrawTextExpressionRunTime;
{$ENDREGION 'Expression'}
{$REGION 'DrawEngine soft rasterization'}

  TSoftRaster_TextCoordinate = record
    C: SystemChar;
    DrawBox, BoundBox: TV2R4;
  end;

  TSoftRaster_TextCoordinateList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TSoftRaster_TextCoordinate>;

  TDrawEngine_Raster = class(TDrawEngineInterface)
  private
    FMemory: TDETexture;
    FUsedAgg: Boolean;
    FEngine: TDrawEngine;
    FFreeEngine: Boolean;
    FTextCoordinateList: TSoftRaster_TextCoordinateList;

    function DEColor2RasterColor(const COLOR: TDEColor): TRasterColor; overload;
    function DEColor2RasterColor(const COLOR: TDEColor; const Alpha: Byte): TRasterColor; overload;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetSize(r: TDERect); override;
    procedure SetLineWidth(w: TDEFloat); override;
    procedure DrawDotLine(pt1, pt2: TDEVec; COLOR: TDEColor); override;
    procedure DrawLine(pt1, pt2: TDEVec; COLOR: TDEColor); override;
    procedure DrawRect(r: TDERect; Angle: TDEFloat; COLOR: TDEColor); override;
    procedure FillRect(r: TDERect; Angle: TDEFloat; COLOR: TDEColor); override;
    procedure DrawEllipse(r: TDERect; COLOR: TDEColor); override;
    procedure FillEllipse(r: TDERect; COLOR: TDEColor); override;
    procedure FillPolygon(PolygonBuff: TArrayVec2; COLOR: TDEColor); override;
    procedure DrawText(Shadow: Boolean; Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean; RotateVec: TDEVec; Angle: TDEFloat); override;
    procedure DrawPicture(Shadow: Boolean; t: TCoreClassObject; Sour, Dest: TDE4V; Alpha: TDEFloat); override;
    procedure Flush; override;
    procedure ResetState; override;
    procedure BeginDraw; override;
    procedure EndDraw; override;
    function CurrentScreenSize: TDEVec; override;
    function GetTextSize(const Text: SystemString; Size: TDEFloat): TDEVec; override;
    function ReadyOK: Boolean; override;
  public
    property Memory: TDETexture read FMemory;
    property UsedAgg: Boolean read FUsedAgg write FUsedAgg;
    function Engine: TDrawEngine;
    procedure SetWorkMemory(m: TMemoryRaster);
    property TextCoordinate: TSoftRaster_TextCoordinateList read FTextCoordinateList;
  end;
{$ENDREGION 'DrawEngine soft rasterization'}
{$REGION 'DeflectionPolygonListRenderer'}

  TDeflectionPolygonListRenderer = class(TDeflectionPolygonList)
  public
    RendererConfigure: THashTextEngine;
    constructor Create;
    destructor Destroy; override;

    procedure RebuildConfigure;
    procedure BuildRendererConfigure(FileName: SystemString); overload;
    procedure BuildRendererConfigure(stream: TCoreClassStream); overload;

    procedure LoadRendererConfigure(stream: TCoreClassStream); overload;
    procedure LoadRendererConfigure(FileName: SystemString); overload;

    procedure Render(d: TDrawEngine; Dest: TRectV2; InScene: Boolean);
  end;
{$ENDREGION 'DeflectionPolygonListRenderer'}
{$REGION 'DrawEngine Core'}

  TDrawEngine = class(TCoreClassInterfacedObject, ICadencerProgressInterface)
  protected
    FRasterization: TDrawEngine_Raster;
    FDrawInterface: TDrawEngineInterface;
    FDrawCommand: TDrawQueue;
    FDrawExecute: TDrawExecute;
    FCommandCounter: Integer;
    FPerformaceCounter: Cardinal;
    FLastPerformaceTime: TTimeTick;
    FFrameCounterOfPerSec: Double;
    FCommandCounterOfPerSec: Double;
    FWidth, FHeight: TDEFloat;
    FLastDeltaTime, FLastNewTime: Double;
    FViewOptions: TDViewerOptions;
    FLastDrawInfo: SystemString;
    FTextSizeCache: THashList;
    FMaxScrollText: Integer;
    FScrollTextList: TCoreClassListForObj;
    FDownPT, FMovePT, FUpPT: TDEVec;
    FLastAcceptDownUI: TDrawEngine_UIBase;
    FUIList: TCoreClassListForObj;
    FSequenceAnimationBuffer: TCoreClassListForObj;
    FParticleBuffer: TCoreClassListForObj;
    FLastDynamicSeqenceFlag: Cardinal;

    FCadencerEng: TCadencer;
    FPostProgress: TNProgressPost;

    FFPSFontSize: TDEFloat;
    FFPSFontColor: TDEColor;
    FScreenFrameColor: TDEColor;
    FScreenFrameSize: TDEFloat;

    FTextureLibrary: THashObjectList;
    FOnGetTexture: TGetTexture;
    FDefaultTexture: TDETexture;
    FPictureFlushInfo: TTextureOutputStateBuffer;

    FUserData: Pointer;
    FUserValue: Variant;
    FUserVariants: THashVariantList;
    FUserObjects: THashObjectList;
    FUserAutoFreeObjects: THashObjectList;

    FTextureOutputStateBox: TDERect;

    procedure SetDrawInterface(const Value: TDrawEngineInterface);
    procedure DoFlush; virtual;

    function DoTapDown(x, y: TDEFloat): Boolean; virtual;
    function DoTapMove(x, y: TDEFloat): Boolean; virtual;
    function DoTapUp(x, y: TDEFloat): Boolean; virtual;

    procedure TextSizeCacheDoDataFree(p: Pointer);

    function GetUserVariants: THashVariantList;
    function GetUserObjects: THashObjectList;
    function GetUserAutoFreeObjects: THashObjectList;

    // cadencer interface
    procedure CadencerProgress(const deltaTime, newTime: Double);
  public
    Scale: TDEFloat;
    Offset: TDEVec;

    constructor Create;
    destructor Destroy; override;

    property ViewOptions: TDViewerOptions read FViewOptions write FViewOptions;
    property DrawOptions: TDViewerOptions read FViewOptions write FViewOptions;
    property Options: TDViewerOptions read FViewOptions write FViewOptions;
    property LastDrawInfo: SystemString read FLastDrawInfo;

    { coordinate: scene to screen }
    function SceneToScreen(pt: TDEVec): TDEVec; overload;
    function SceneToScreen(x, y: TDEFloat): TDEVec; overload;
    function SceneToScreen(r: TDE4V): TDE4V; overload;
    function SceneToScreen(r: TDERect): TDERect; overload;
    function SceneToScreen(r: TRectf): TRectf; overload;
    function SceneToScreen(r: TRect): TRect; overload;
    function SceneToScreen(r: TV2Rect4): TV2Rect4; overload;
    function SceneToScreen(buff: TArrayVec2): TArrayVec2; overload;
    { coordinate: screen to scene }
    function ScreenToScene(pt: TDEVec): TDEVec; overload;
    function ScreenToScene(x, y: TDEFloat): TDEVec; overload;
    function ScreenToScene(r: TDERect): TDERect; overload;
    function ScreenToScene(r: TRectf): TRectf; overload;
    function ScreenToScene(r: TRect): TRect; overload;
    function ScreenToScene(r: TDE4V): TDE4V; overload;
    function ScreenToScene(r: TV2Rect4): TV2Rect4; overload;
    function ScreenToScene(buff: TArrayVec2): TArrayVec2; overload;

    { camera }
    function GetCameraR: TDERect;
    procedure SetCameraR(const Value: TDERect);
    property CameraR: TDERect read GetCameraR write SetCameraR;
    function GetCamera: TDEVec;
    procedure SetCamera(const Value: TDEVec);
    property Camera: TDEVec read GetCamera write SetCamera;
    procedure ScaleCamera(f: TDEFloat);
    procedure ScaleCameraFromWheelDelta(WheelDelta: Integer);
    procedure ResetCamera();

    { state }
    property LastDeltaTime: Double read FLastDeltaTime;
    property LastNewTime: Double read FLastNewTime write FLastNewTime;
    function ReadyOK: Boolean;
    property FrameCounterOfPerSec: Double read FFrameCounterOfPerSec;
    property CommandCounterOfPerSec: Double read FCommandCounterOfPerSec;

    { screen }
    function ScreenCentreToScene: TDEVec;
    function ScreenRectToScene: TDERect;
    function ScreenRectV2: TDERect;
    property ScreenRect: TDERect read ScreenRectV2;
    function ScreenV2Rect4: TV2Rect4;
    procedure SetSize; overload;
    procedure SetSize(w, h: TDEFloat); overload;
    procedure SetSize(siz: TDEVec); overload;
    procedure SetSize(raster: TMemoryRaster); overload;
    procedure SetSizeAndOffset(r: TDERect); overload;
    property width: TDEFloat read FWidth;
    property height: TDEFloat read FHeight;
    function SizeVec: TDEVec;
    function SceneWidth: TDEFloat;
    function SceneHeight: TDEFloat;

    { draw buffer }
    procedure SetDrawBounds(w, h: TDEFloat); overload;
    procedure SetDrawBounds(siz: TDEVec); overload;
    procedure SetDrawBounds(r: TDERect); overload;
    procedure SetDrawBounds(r: TRectf); overload;

    { compute text }
    procedure ClearTextCache;
    function GetTextSize(const t: SystemString; Size: TDEFloat): TDEVec; overload;
    function GetTextSize(const buff: TDSegmentionLine): TDEVec; overload;
    function GetTextSize(const buff: TDArraySegmentionText): TDEVec; overload;
    function GetTextSizeR(const Text: SystemString; Size: TDEFloat): TDERect; overload;
    function GetTextSizeR(const buff: TDArraySegmentionText): TDERect; overload;
    function ComputeScaleTextSize(const t: SystemString; Size: TDEFloat; MaxSiz: TDEVec): TDEFloat;

    { scroll text and UI }
    property MaxScrollText: Integer read FMaxScrollText write FMaxScrollText;
    procedure ClearScrollText;
    procedure PostScrollText(LifeTime: Double; Text_: SystemString; Size: Integer; COLOR: TDEColor); overload;
    procedure PostScrollText(Tag: TCoreClassObject; LifeTime: Double; Text_: SystemString; Size: Integer; COLOR: TDEColor); overload;
    function GetLastPostScrollText: SystemString;

    procedure ClearUI;
    procedure AllUINoVisibled;
    function TapDown(x, y: TDEFloat): Boolean;
    function TapMove(x, y: TDEFloat): Boolean;
    function TapUp(x, y: TDEFloat): Boolean;

    { shadow }
    procedure BeginCaptureShadow(const ScreenOffsetVec: TDEVec; const Alpha: TDEFloat); overload;
    procedure BeginCaptureShadow(const ScreenOffsetVec: TDEVec; const Alpha: TDEFloat; ShadowSIGMA: TGeoFloat; ShadowSigmaGaussianKernelFactor: Integer); overload;
    procedure EndCaptureShadow;
    function CaptureShadow: Boolean;
    function LastCaptureScreenShadowOffsetVec: TDEVec;
    function LastCaptureScreenShadowAlpha: TDEFloat;

    { compute clip rect }
    function ScreenRectInScreen(r: TDERect): Boolean; overload;
    function ScreenRectInScreen(r: TV2Rect4): Boolean; overload;
    function SceneRectInScreen(r: TDERect): Boolean; overload;
    function SceneRectInScreen(r: TV2Rect4): Boolean; overload;

    { custom draw }
    procedure DrawUserCustom(const OnDraw: TCustomDraw_Method; const UserData: Pointer; const UserObject: TCoreClassObject);

    { draw geometry: array vec2 line }
    procedure DrawOutSideSmoothArrayLine(DotLine: Boolean; arry: TArrayVec2; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
    procedure DrawOutSideSmoothArrayLineInScene(DotLine: Boolean; arry: TArrayVec2; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
    procedure DrawInSideSmoothArrayLine(DotLine: Boolean; arry: TArrayVec2; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
    procedure DrawInSideSmoothArrayLineInScene(DotLine: Boolean; arry: TArrayVec2; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
    procedure DrawArrayLine(arry: TArrayVec2; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawArrayLineInScene(arry: TArrayVec2; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawOutSideSmoothPL(DotLine: Boolean; pl: TVec2List; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
    procedure DrawOutSideSmoothPLInScene(DotLine: Boolean; pl: TVec2List; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
    procedure DrawInSideSmoothPL(DotLine: Boolean; pl: TVec2List; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
    procedure DrawInSideSmoothPLInScene(DotLine: Boolean; pl: TVec2List; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
    procedure DrawPL(DotLine: Boolean; pl: TVec2List; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawPLInScene(DotLine: Boolean; pl: TVec2List; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawPL(pl: TVec2List; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawPLInScene(pl: TVec2List; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawPLInScene(pl: TVec2List; ClosedLine: Boolean; opt: TPolyDrawOption); overload;
    procedure DrawPolyInScene(Poly: TDeflectionPolygon; ClosedLine: Boolean; opt: TPolyDrawOption);
    procedure DrawPolyExpandInScene(Poly: TDeflectionPolygon; ExpandDistance: TDEFloat; ClosedLine: Boolean; opt: TPolyDrawOption);
    procedure DrawTriangle(DotLine: Boolean; const t: TTriangle; COLOR: TDEColor; LineWidth: TDEFloat; DrawCentre: Boolean); overload;
    procedure DrawTriangle(DotLine: Boolean; const t: TTriangleList; COLOR: TDEColor; LineWidth: TDEFloat; DrawCentre: Boolean); overload;
    procedure DrawTriangleInScene(DotLine: Boolean; const t: TTriangle; COLOR: TDEColor; LineWidth: TDEFloat; DrawCentre: Boolean); overload;
    procedure DrawTriangleInScene(DotLine: Boolean; const t: TTriangleList; COLOR: TDEColor; LineWidth: TDEFloat; DrawCentre: Boolean); overload;

    { draw dotline }
    procedure DrawDotLine(pt1, pt2: TDEVec; COLOR: TDEColor; LineWidth: TDEFloat);
    procedure DrawDotLineInScene(pt1, pt2: TDEVec; COLOR: TDEColor; LineWidth: TDEFloat);

    { draw line }
    procedure DrawLine(pt1, pt2: TDEVec; COLOR: TDEColor; LineWidth: TDEFloat);
    procedure DrawLineInScene(pt1, pt2: TDEVec; COLOR: TDEColor; LineWidth: TDEFloat);

    { draw corner }
    procedure DrawCorner(Box: TV2Rect4; COLOR: TDEColor; BoundLineLength, LineWidth: TDEFloat); overload;
    procedure DrawCorner(Box: TDERect; COLOR: TDEColor; BoundLineLength, LineWidth: TDEFloat); overload;
    procedure DrawCornerInScene(Box: TV2Rect4; COLOR: TDEColor; BoundLineLength, LineWidth: TDEFloat); overload;
    procedure DrawCornerInScene(Box: TDERect; COLOR: TDEColor; BoundLineLength, LineWidth: TDEFloat); overload;

    { draw DE4V rect }
    procedure DrawDE4V(d: TDE4V; COLOR: TDEColor; LineWidth: TDEFloat);
    procedure DrawDE4VInScene(d: TDE4V; COLOR: TDEColor; LineWidth: TDEFloat);

    { draw screen point }
    procedure DrawScreenPoint(pt: TDEVec; COLOR: TDEColor; LineWidth: TDEFloat);
    procedure DrawScreenPointInScene(pt: TDEVec; COLOR: TDEColor; LineWidth: TDEFloat);

    { draw point }
    procedure DrawCross(pt: TDEVec; COLOR: TDEColor; LineLength, LineWidth: TDEFloat);
    procedure DrawPoint(pt: TDEVec; COLOR: TDEColor; LineLength, LineWidth: TDEFloat);
    procedure DrawPointInScene(pt: TDEVec; COLOR: TDEColor; LineLength, LineWidth: TDEFloat);
    procedure DrawDotLinePoint(pt: TDEVec; COLOR: TDEColor; LineLength, LineWidth: TDEFloat);
    procedure DrawDotLinePointInScene(pt: TDEVec; COLOR: TDEColor; LineLength, LineWidth: TDEFloat);

    procedure DrawArrayVec2(arry: TArrayVec2; COLOR: TDEColor; LineLength, LineWidth: TDEFloat);
    procedure DrawArrayVec2InScene(arry: TArrayVec2; COLOR: TDEColor; LineLength, LineWidth: TDEFloat);

    { draw box }
    procedure DrawBox(Box: TV2Rect4; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawBoxInScene(Box: TV2Rect4; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawBox(r: TDERect; axis: TDEVec; Angle: TDEFloat; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawBoxInScene(r: TDERect; axis: TDEVec; Angle: TDEFloat; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawBox(r: TDERect; Angle: TDEFloat; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawBoxInScene(r: TDERect; Angle: TDEFloat; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawBox(r: TDERect; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawBoxInScene(r: TDERect; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawLabelBox(lab: SystemString; labSiz: TDEFloat; labColor: TDEColor; Box: TDERect; boxColor: TDEColor; boxLineWidth: TDEFloat);
    procedure DrawLabelBoxInScene(lab: SystemString; labSiz: TDEFloat; labColor: TDEColor; Box: TDERect; boxColor: TDEColor; boxLineWidth: TDEFloat);

    { draw dot line box }
    procedure DrawDotLineBox(Box: TV2Rect4; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawDotLineBoxInScene(Box: TV2Rect4; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawDotLineBox(r: TDERect; axis: TDEVec; Angle: TDEFloat; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawDotLineBoxInScene(r: TDERect; axis: TDEVec; Angle: TDEFloat; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawDotLineBox(r: TDERect; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawDotLineBoxInScene(r: TDERect; COLOR: TDEColor; LineWidth: TDEFloat); overload;

    { fill box }
    procedure FillBox(); overload;
    procedure FillBox(Box: TV2Rect4); overload;
    procedure FillBox(Box: TDERect); overload;
    procedure FillBox(Box: TV2Rect4; COLOR: TDEColor); overload;
    procedure FillBoxInScene(Box: TV2Rect4; COLOR: TDEColor); overload;
    procedure FillBox(r: TDERect; Angle: TDEFloat; COLOR: TDEColor); overload;
    procedure FillBoxInScene(r: TDERect; Angle: TDEFloat; COLOR: TDEColor); overload;
    procedure FillBox(r: TDERect; COLOR: TDEColor); overload;
    procedure FillBoxInScene(r: TDERect; COLOR: TDEColor); overload;

    { draw ellipse }
    procedure DrawEllipse(pt: TDEVec; radius: TDEFloat; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawEllipse(pt: TDEVec; radius: TDEFloat; COLOR: TDEColor); overload;
    procedure DrawEllipse(r: TDERect; COLOR: TDEColor); overload;
    procedure DrawEllipseInScene(pt: TDEVec; radius: TDEFloat; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawEllipseInScene(pt: TDEVec; radius: TDEFloat; COLOR: TDEColor); overload;
    procedure DrawEllipseInScene(r: TDERect; COLOR: TDEColor); overload;

    { fill ellipse }
    procedure FillEllipse(pt: TDEVec; radius: TDEFloat; COLOR: TDEColor); overload;
    procedure FillEllipse(r: TDERect; COLOR: TDEColor); overload;
    procedure FillEllipseInScene(pt: TDEVec; radius: TDEFloat; COLOR: TDEColor); overload;
    procedure FillEllipseInScene(r: TDERect; COLOR: TDEColor); overload;

    { fill and draw a polygon }
    procedure FillPolygon(PolygonBuff: TArrayVec2; COLOR: TDEColor);
    procedure FillPolygonInScene(PolygonBuff: TArrayVec2; COLOR: TDEColor);
    procedure DrawPolygon(PolygonBuff: TArrayVec2; COLOR: TDEColor; LineWidth: TDEFloat);
    procedure DrawPolygonInScene(PolygonBuff: TArrayVec2; COLOR: TDEColor; LineWidth: TDEFloat);
    procedure DrawPolygonDotLine(PolygonBuff: TArrayVec2; COLOR: TDEColor; LineWidth: TDEFloat);
    procedure DrawPolygonDotLineInScene(PolygonBuff: TArrayVec2; COLOR: TDEColor; LineWidth: TDEFloat);

    { draw text }
    class function RebuildTextColor(Text: SystemString; ts: TTextStyle;
      TextDecl_prefix_, TextDecl_postfix_,
      Comment_prefix_, Comment_postfix_,
      Number_prefix_, Number_postfix_,
      Symbol_prefix_, Symbol_postfix_,
      Ascii_prefix_, Ascii_postfix_: SystemString): SystemString;
    class function RebuildNumAndWordColor(Text: SystemString;
      Number_prefix_, Number_postfix_: SystemString;
      Ascii_matched_, Ascii_replace_: array of SystemString): SystemString;
    class function RebuildNumColor(Text: SystemString; Number_prefix_, Number_postfix_: SystemString): SystemString;
    function DrawText(const Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean; RotateVec: TDEVec; Angle: TDEFloat): TV2Rect4; overload;
    function DrawText(const Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean): TV2Rect4; overload;
    function DrawText(const Text: SystemString; Size: TDEFloat; COLOR: TDEColor; ScreenPt: TDEVec): TV2Rect4; overload;
    function DrawText(const Text: SystemString; Size: TDEFloat; COLOR: TDEColor; ScreenPt: TDEVec; Angle: TDEFloat): TV2Rect4; overload;
    function DrawText(const lb, le: TDEVec; const Text: SystemString; Size: TDEFloat; COLOR: TDEColor): TV2Rect4; overload;
    { draw text in scene }
    function DrawTextInScene(const Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean; RotateVec: TDEVec; Angle: TDEFloat): TV2Rect4; overload;
    function DrawTextInScene(const Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean): TV2Rect4; overload;
    function DrawTextInScene(const Text: SystemString; Size: TDEFloat; COLOR: TDEColor; ScenePos: TDEVec): TV2Rect4; overload;
    function DrawTextInScene(const Text: SystemString; Size: TDEFloat; COLOR: TDEColor; ScenePos: TDEVec; Angle: TDEFloat): TV2Rect4; overload;
    function DrawTextInScene(const lb, le: TDEVec; const Text: SystemString; Size: TDEFloat; COLOR: TDEColor): TV2Rect4; overload;

    { draw order text }
    function DrawSegmentionText(const buff: TDArraySegmentionText; pt: TDEVec; RotateVec: TDEVec; Angle: TDEFloat): TV2Rect4; overload;
    function DrawSegmentionText(const buff: TDArraySegmentionText; pt: TDEVec): TV2Rect4; overload;
    { draw order text in scene }
    function DrawSegmentionTextInScene(const buff: TDArraySegmentionText; pt: TDEVec; RotateVec: TDEVec; Angle: TDEFloat): TV2Rect4; overload;
    function DrawSegmentionTextInScene(const buff: TDArraySegmentionText; pt: TDEVec): TV2Rect4; overload;

    { draw tile texture }
    procedure DrawTile(t: TCoreClassObject; Sour: TDERect; Alpha: TDEFloat); overload;
    procedure DrawTile(t: TCoreClassObject); overload;
    { draw texture }
    procedure DrawPicture(t: TCoreClassObject; Sour, DestScreen: TDE4V; Alpha: TDEFloat); overload;
    procedure DrawPicture(t: TCoreClassObject; Sour: TDERect; DestScreen: TDE4V; Alpha: TDEFloat); overload;
    procedure DrawPicture(t: TCoreClassObject; Sour, DestScreen: TDERect; Alpha: TDEFloat); overload;
    procedure DrawPicture(t: TCoreClassObject; Sour: TDERect; destScreenPt: TDEVec; Angle, Alpha: TDEFloat); overload;
    procedure DrawPicture(t: TCoreClassObject; Sour, DestScreen: TDERect; Angle, Alpha: TDEFloat); overload;
    function DrawPicture(indentEndge: Boolean; t: TCoreClassObject; Sour, DestScreen: TDERect; Alpha: TDEFloat): TDERect; overload;
    { fit draw texture }
    procedure FitDrawPicture(t: TCoreClassObject; Sour, DestScreen: TDERect; Angle, Alpha: TDEFloat); overload;
    function FitDrawPicture(t: TCoreClassObject; Sour, DestScreen: TDERect; Alpha: TDEFloat): TDERect; overload;
    function FitDrawPicture(indentEndge: Boolean; t: TCoreClassObject; Sour, DestScreen: TDERect; Alpha: TDEFloat): TDERect; overload;
    { draw texture in scene }
    procedure DrawPictureInScene(t: TCoreClassObject; Sour, destScene: TDE4V; Alpha: TDEFloat); overload;
    procedure DrawPictureInScene(t: TCoreClassObject; Sour: TDERect; destScene: TDE4V; Alpha: TDEFloat); overload;
    procedure DrawPictureInScene(t: TCoreClassObject; destScene: TDE4V; Alpha: TDEFloat); overload;
    procedure DrawPictureInScene(t: TCoreClassObject; Sour, destScene: TDERect; Alpha: TDEFloat); overload;
    procedure DrawPictureInScene(t: TCoreClassObject; Sour: TDERect; destScenePt: TDEVec; Angle, Alpha: TDEFloat); overload;
    procedure DrawPictureInScene(t: TCoreClassObject; Sour, destScene: TDERect; Angle, Alpha: TDEFloat); overload;
    procedure DrawPictureInScene(t: TCoreClassObject; destScenePt: TDEVec; Width_, Height_, Angle, Alpha: TDEFloat); overload;
    function DrawPictureInScene(indentEndge: Boolean; t: TCoreClassObject; Sour, destScene: TDERect; Alpha: TDEFloat): TDERect; overload;
    { fit draw texture in scene }
    procedure FitDrawPictureInScene(t: TCoreClassObject; Sour, destScene: TDERect; Angle, Alpha: TDEFloat); overload;
    function FitDrawPictureInScene(t: TCoreClassObject; Sour, destScene: TDERect; Alpha: TDEFloat): TDERect; overload;
    function FitDrawPictureInScene(indentEndge: Boolean; t: TCoreClassObject; Sour, destScene: TDERect; Alpha: TDEFloat): TDERect; overload;
    { draw texture packing in scene }
    function DrawRectPackingInScene(rp: TRectPacking; destOffset: TDEVec; Alpha: TDEFloat; ShowBox: Boolean): TDERect;
    function DrawPicturePackingInScene(Input_: TMemoryRasterList; Margins: TDEFloat; destOffset: TDEVec; Alpha: TDEFloat; ShowBox: Boolean): TDERect; overload;
    function DrawPicturePackingInScene(Input_: TMemoryRasterList; Margins: TDEFloat; destOffset: TDEVec; Alpha: TDEFloat): TDERect; overload;
    function DrawPictureMatrixPackingInScene(Input_: TMemoryRaster2DMatrix; Margins: TDEFloat; destOffset: TDEVec; Alpha: TDEFloat; ShowBox: Boolean): TDERect;
    { draw text packing in scene }
    function DrawTextPackingInScene(Input_: TArrayPascalString; text_color: TDEColor; text_siz, Margins: TDEFloat; destOffset: TDEVec; ShowBox: Boolean): TDERect; overload;
    function DrawTextPackingInScene(Input_: TArrayPascalString; text_color: TDEColor; text_siz, Margins: TDEFloat; destOffset: TDEVec): TDERect; overload;

    { sequence animation }
    function CreateSequenceAnimation(stream: TCoreClassStream): TSequenceAnimationBase; overload;
    function GetOrCreateSequenceAnimation(flag: Variant; t: TCoreClassObject): TSequenceAnimationBase;
    function SequenceAnimationPlaying(flag: Variant; t: TCoreClassObject): Boolean;
    function SequenceAnimationIsOver(flag: Variant; t: TCoreClassObject): Boolean;
    function ExistsSequenceAnimation(SA: TSequenceAnimationBase): Boolean;
    function GetNewSequenceFlag: Variant;
    { prototype: draw sequence texture }
    function ManualDrawSequenceTexture(flag: Variant; t: TCoreClassObject; TextureWidth, TextureHeight, Total, Column: Integer; CompleteTime: Double; Looped: Boolean;
      DestScreen: TDE4V; Alpha: TDEFloat): TSequenceAnimationBase; virtual;
    { draw sequence texture }
    function DrawSequenceTexture(flag: Variant; t: TCoreClassObject; TextureWidth, TextureHeight, Total, Column: Integer; CompleteTime: Double; Looped: Boolean;
      DestScreen: TDE4V; Alpha: TDEFloat): TSequenceAnimationBase; overload;
    function DrawSequenceTexture(flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; DestScreen: TDE4V; Alpha: TDEFloat): TSequenceAnimationBase; overload;
    function DrawSequenceTexture(flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; DestScreen: TDERect; Alpha: TDEFloat): TSequenceAnimationBase; overload;
    function DrawSequenceTexture(flag: Variant; t: TDETexture; CompleteTime: Double; DestScreen: TDE4V; Alpha: TDEFloat): TSequenceAnimationBase; overload;
    function FitDrawSequenceTexture(flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; DestScreen: TDERect; Alpha: TDEFloat): TDERect; overload;
    function FitDrawSequenceTexture(indentEndge: Boolean; flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; DestScreen: TDERect; Alpha: TDEFloat): TDERect; overload;
    procedure DrawSequenceTexture(SA: TSequenceAnimationBase; DestScreen: TDE4V; Alpha: TDEFloat); overload;
    { draw sequence texture in scene }
    function DrawSequenceTextureInScene(flag: Variant; t: TCoreClassObject; TextureWidth, TextureHeight, Total, Column: Integer; CompleteTime: Double; Looped: Boolean;
      destScene: TDE4V; Alpha: TDEFloat): TSequenceAnimationBase; overload;
    function DrawSequenceTextureInScene(flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; destScene: TDE4V; Alpha: TDEFloat): TSequenceAnimationBase; overload;
    function DrawSequenceTextureInScene(flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; destScene: TDERect; Alpha: TDEFloat): TSequenceAnimationBase; overload;
    function DrawSequenceTextureInScene(flag: Variant; t: TDETexture; CompleteTime: Double; destScene: TDE4V; Alpha: TDEFloat): TSequenceAnimationBase; overload;
    function FitDrawSequenceTextureInScene(flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; destScene: TDERect; Alpha: TDEFloat): TDERect; overload;
    function FitDrawSequenceTextureInScene(indentEndge: Boolean; flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; destScene: TDERect; Alpha: TDEFloat): TDERect; overload;
    procedure DrawSequenceTextureInScene(SA: TSequenceAnimationBase; destScene: TDE4V; Alpha: TDEFloat); overload;

    { particles }
    function CreateParticles: TParticles; overload;
    function CreateParticles(stream: TCoreClassStream): TParticles; overload;
    procedure DeleteParticles(p: TParticles);
    procedure FreeAndDeleteParticles(p: TParticles);
    procedure ClearParticles;
    function ExistsParticles(p: TParticles): Boolean;
    function TotalParticleData: Integer;
    function ParticleCount: Integer;
    function GetParticles(const Index: Integer): TParticles;
    property Particles[const index: Integer]: TParticles read GetParticles;
    procedure DrawParticle(Particle: TParticles; DestScreen: TDEVec);
    procedure DrawParticleInScene(Particle: TParticles; destScene: TDEVec);

    { texture IO }
    function GetTexture(TextureName: SystemString): TDETexture;
    function GetTextureName(t: TCoreClassObject): SystemString;
    class function NewTexture: TDETexture;

    { flush - thread }
    procedure PrepareTextureOutputState;
    procedure PrepareFlush;
    procedure ClearFlush;
    procedure Flush; overload;
    procedure Flush(Prepare: Boolean); overload;
    procedure CopyFlushTo(Dst: TDrawExecute);
    property PictureFlushInfo: TTextureOutputStateBuffer read FPictureFlushInfo;

    { cadencer progress }
    procedure Progress(deltaTime: Double); overload; virtual;

    { auto progress }
    function Progress(): Double; overload;

    // delay run support
    property ProgressEngine: TNProgressPost read FPostProgress;
    property ProgressPost: TNProgressPost read FPostProgress;
    property PostProgress: TNProgressPost read FPostProgress;
    property PostRun: TNProgressPost read FPostProgress;
    property PostExecute: TNProgressPost read FPostProgress;

    // cadencer engine
    property CadencerEngine: TCadencer read FCadencerEng;

    { build-in rasterization }
    property Rasterization: TDrawEngine_Raster read FRasterization;
    { draw interface }
    property DrawInterface: TDrawEngineInterface read FDrawInterface write SetDrawInterface;
    procedure SetDrawInterfaceAsDefault;
    { draw sequence }
    property DrawCommand: TDrawQueue read FDrawCommand;
    property DrawExecute: TDrawExecute read FDrawExecute;

    { misc }
    property FPSFontSize: TDEFloat read FFPSFontSize write FFPSFontSize;
    property FPSFontColor: TDEColor read FFPSFontColor write FFPSFontColor;
    property ScreenFrameColor: TDEColor read FScreenFrameColor write FScreenFrameColor;
    property EdgeColor: TDEColor read FScreenFrameColor write FScreenFrameColor;
    property ScreenFrameSize: TDEFloat read FScreenFrameSize write FScreenFrameSize;
    property EdgeSize: TDEFloat read FScreenFrameSize write FScreenFrameSize;

    { texture }
    property TextureLibrary: THashObjectList read FTextureLibrary;
    property OnGetTexture: TGetTexture read FOnGetTexture write FOnGetTexture;
    property DefaultTexture: TDETexture read FDefaultTexture;
    property TextureOutputStateBox: TDERect read FTextureOutputStateBox write FTextureOutputStateBox;

    { user variant }
    property UserVariants: THashVariantList read GetUserVariants;
    property UserObjects: THashObjectList read GetUserObjects;
    property UserAutoFreeObjects: THashObjectList read GetUserAutoFreeObjects;
    property UserData: Pointer read FUserData write FUserData;
    property UserValue: Variant read FUserValue write FUserValue;
  end;

  TMemoryRasterHelperForDrawEngine = class helper for TMemoryRaster
  public
    function GetDrawEngine: TDrawEngine;
    property DrawEngine: TDrawEngine read GetDrawEngine;
  end;
{$ENDREGION 'DrawEngine Core'}
{$REGION 'misc'}


const
  NULLVec: TDEVec = (0, 0);
  ZeroVec: TDEVec = (0, 0);

function DrawPool(workObj: TCoreClassObject; Draw: TDrawEngineInterface): TDrawEngine; overload;
function DrawPool(workObj: TCoreClassObject): TDrawEngine; overload;
function DrawPool(): TDrawEnginePool; overload;
function DrawEnginePool(workObj: TCoreClassObject; Draw: TDrawEngineInterface): TDrawEngine; overload;
function DrawEnginePool(workObj: TCoreClassObject): TDrawEngine; overload;

function DEVec(x, y: TDEFloat): TDEVec; overload;
function DEVec(pt: TPointf): TDEVec; overload;
function DEColorInv(const COLOR: TDEColor): TDEColor; overload;
function DEColorInv(const r, g, b, a: TDEFloat): TDEColor; overload;
function DEColor(const r, g, b, a: TDEFloat): TDEColor; overload;
function DEColor(const r, g, b: TDEFloat): TDEColor; overload;
function DEColor(const C: TDEColor; const Alpha: TDEFloat): TDEColor; overload;
function DEColor(const C: TVec3; const Alpha: TDEFloat): TDEColor; overload;
function DEColor(const C: TVec4): TDEColor; overload;
function DEColor(const C: TVector3; const Alpha: TDEFloat): TDEColor; overload;
function DEColor(const C: TVector4): TDEColor; overload;
function DEColor(const C: TRasterColor): TDEColor; overload;
function DEColor2RasterColor(const C: TDEColor): TRasterColor;

function DColor2RColor(const C: TDEColor): TRasterColor;
function RColor2DColor(const C: TRasterColor): TDEColor;

function DEAlpha(C: TDEColor): TDEFloat;
function DERect(const x, y, radius: TDEFloat): TDERect; overload;
function DERect(const x1, y1, x2, y2: TDEFloat): TDERect; overload;
function DERect(const p1, p2: T2DPoint): TDERect; overload;
function DERect(const x, y: TDEFloat; const p2: T2DPoint): TDERect; overload;
function DERect(const Rect: TRect): TDERect; overload;
function DERect(const Rect: TRectf): TDERect; overload;

function Interval2Delta(interval: Integer): Double;

procedure FitScale(const Sour, Dest: TDERect; var outOffset: TDEVec; var outScale: TDEFloat); overload;
procedure FitScale(const Sour: TDERect; const DestWidth, DestHeight: TDEFloat; var outOffset: TDEVec; var outScale: TDEFloat); overload;
procedure FitScale(const Sour: TRectf; const DestWidth, DestHeight: TDEFloat; var outOffset: TDEVec; var outScale: TDEFloat); overload;
procedure FitScale(const Sour, Dest: TRectf; var outOffset: TDEVec; var outScale: TDEFloat); overload;
procedure FitScale(const sourWidth, sourHeight, DestWidth, DestHeight: TDEFloat; var outOffset: TDEVec; var outScale: TDEFloat); overload;

function IsSegmentionText(const s: SystemString): Boolean;
function ArraySegmentionTextToString(var buff: TDArraySegmentionText): TPascalString;
function FillSegmentionText(const s: TPascalString; Size: TDEFloat; COLOR: TDEColor; RT: TDrawTextExpressionRunTimeClass): TDArraySegmentionText;
procedure FreeSegmentionText(var buff: TDArraySegmentionText);
{$ENDREGION 'misc'}


var
  DefaultTextureClass: TDETextureClass = TDETexture;
  EnginePool: TDrawEnginePool = nil;
  TexturePool: TDETextureList = nil;

implementation

uses Math, DataFrameEngine, DoStatusIO;

function DrawPool(workObj: TCoreClassObject; Draw: TDrawEngineInterface): TDrawEngine;
begin
  Result := EnginePool.GetEng(workObj, Draw);
end;

function DrawPool(workObj: TCoreClassObject): TDrawEngine;
begin
  Result := EnginePool.GetEng(workObj);
end;

function DrawPool(): TDrawEnginePool;
begin
  Result := EnginePool;
end;

function DrawEnginePool(workObj: TCoreClassObject; Draw: TDrawEngineInterface): TDrawEngine;
begin
  Result := EnginePool.GetEng(workObj, Draw);
end;

function DrawEnginePool(workObj: TCoreClassObject): TDrawEngine;
begin
  Result := EnginePool.GetEng(workObj);
end;

function DEVec(x, y: TDEFloat): TDEVec;
begin
  Result[0] := x;
  Result[1] := y;
end;

function DEVec(pt: TPointf): TDEVec;
begin
  Result[0] := pt.x;
  Result[1] := pt.y;
end;

function DEColorInv(const COLOR: TDEColor): TDEColor;
begin
  Result[0] := 1.0 - COLOR[0];
  Result[1] := 1.0 - COLOR[1];
  Result[2] := 1.0 - COLOR[2];
  Result[3] := COLOR[3];
end;

function DEColorInv(const r, g, b, a: TDEFloat): TDEColor;
begin
  Result[0] := 1.0 - r;
  Result[1] := 1.0 - g;
  Result[2] := 1.0 - b;
  Result[3] := a;
end;

function DEColor(const r, g, b, a: TDEFloat): TDEColor;
begin
  Result[0] := r;
  Result[1] := g;
  Result[2] := b;
  Result[3] := a;
end;

function DEColor(const r, g, b: TDEFloat): TDEColor;
begin
  Result[0] := r;
  Result[1] := g;
  Result[2] := b;
  Result[3] := 1.0;
end;

function DEColor(const C: TDEColor; const Alpha: TDEFloat): TDEColor;
begin
  Result := C;
  Result[3] := Alpha;
end;

function DEColor(const C: TVec3; const Alpha: TDEFloat): TDEColor;
begin
  Result[0] := C[0];
  Result[1] := C[1];
  Result[2] := C[2];
  Result[3] := Alpha;
end;

function DEColor(const C: TVec4): TDEColor;
begin
  Result[0] := C[0];
  Result[1] := C[1];
  Result[2] := C[2];
  Result[3] := C[3];
end;

function DEColor(const C: TVector3; const Alpha: TDEFloat): TDEColor;
begin
  Result[0] := C.buff[0];
  Result[1] := C.buff[1];
  Result[2] := C.buff[2];
  Result[3] := Alpha;
end;

function DEColor(const C: TVector4): TDEColor;
begin
  Result[0] := C.buff[0];
  Result[1] := C.buff[1];
  Result[2] := C.buff[2];
  Result[3] := C.buff[3];
end;

function DEColor(const C: TRasterColor): TDEColor;
begin
  RColor2F(C, Result[0], Result[1], Result[2], Result[3]);
end;

function DEColor2RasterColor(const C: TDEColor): TRasterColor;
begin
  Result := RColorF(C[0], C[1], C[2], C[3]);
end;

function DColor2RColor(const C: TDEColor): TRasterColor;
begin
  Result := RColorF(C[0], C[1], C[2], C[3]);
end;

function RColor2DColor(const C: TRasterColor): TDEColor;
begin
  RColor2F(C, Result[0], Result[1], Result[2], Result[3]);
end;

function DEAlpha(C: TDEColor): TDEFloat;
begin
  Result := C[3];
end;

function DERect(const x, y, radius: TDEFloat): TDERect;
begin
  Result[0][0] := x - radius;
  Result[0][1] := y - radius;
  Result[1][0] := x + radius;
  Result[1][1] := y + radius;
end;

function DERect(const x1, y1, x2, y2: TDEFloat): TDERect;
begin
  Result[0][0] := x1;
  Result[0][1] := y1;
  Result[1][0] := x2;
  Result[1][1] := y2;
end;

function DERect(const p1, p2: T2DPoint): TDERect;
begin
  Result[0] := p1;
  Result[1] := p2;
end;

function DERect(const x, y: TDEFloat; const p2: T2DPoint): TDERect;
begin
  Result[0] := DEVec(x, y);
  Result[1] := p2;
end;

function DERect(const Rect: TRect): TDERect;
begin
  Result[0][0] := Rect.Left;
  Result[0][1] := Rect.Top;
  Result[1][0] := Rect.Right;
  Result[1][1] := Rect.Bottom;
end;

function DERect(const Rect: TRectf): TDERect;
begin
  Result[0][0] := Rect.Left;
  Result[0][1] := Rect.Top;
  Result[1][0] := Rect.Right;
  Result[1][1] := Rect.Bottom;
end;

function Interval2Delta(interval: Integer): Double;
begin
  Result := 1.0 / (1000.0 / interval);
end;

procedure FitScale(const Sour, Dest: TDERect; var outOffset: TDEVec; var outScale: TDEFloat);
var
  r: TDERect;
begin
  // compute scale
  r := RectFit(Sour, Dest);
  outScale := RectWidth(r) / RectWidth(Sour);
  outOffset := r[0];
end;

procedure FitScale(const Sour: TDERect; const DestWidth, DestHeight: TDEFloat; var outOffset: TDEVec; var outScale: TDEFloat);
begin
  FitScale(Sour, DERect(0, 0, DestWidth, DestHeight), outOffset, outScale);
end;

procedure FitScale(const Sour: TRectf; const DestWidth, DestHeight: TDEFloat; var outOffset: TDEVec; var outScale: TDEFloat);
begin
  FitScale(DERect(Sour), DERect(0, 0, DestWidth, DestHeight), outOffset, outScale);
end;

procedure FitScale(const Sour, Dest: TRectf; var outOffset: TDEVec; var outScale: TDEFloat);
begin
  FitScale(DERect(Sour), DERect(Dest), outOffset, outScale);
end;

procedure FitScale(const sourWidth, sourHeight, DestWidth, DestHeight: TDEFloat; var outOffset: TDEVec; var outScale: TDEFloat);
begin
  FitScale(DERect(0, 0, sourWidth, sourHeight), DERect(0, 0, DestWidth, DestHeight), outOffset, outScale);
end;

function IsSegmentionText(const s: SystemString): Boolean;
var
  i: SystemChar;
  cc: Integer;
begin
  cc := 0;
  for i in s do
    if i = '|' then
        inc(cc);
  Result := (cc >= 2) and ((cc mod 2) = 0);
end;

function ArraySegmentionTextToString(var buff: TDArraySegmentionText): TPascalString;
var
  i, j: Integer;
begin
  Result := '';
  for j := Low(buff) to high(buff) do
    for i := Low(buff[j]) to high(buff[j]) do
        Result.Append(buff[j, i].Text);
end;

function FillSegmentionText(const s: TPascalString; Size: TDEFloat; COLOR: TDEColor; RT: TDrawTextExpressionRunTimeClass): TDArraySegmentionText;
type
  TSegmentionTextList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TDSegmentionText>;
  TSegmentionTextLists = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TSegmentionTextList>;

var
  oprt: TDrawTextExpressionRunTime;

  function FillSegmentionTextSet_(const order_s: SystemString; var o: TDSegmentionText): Boolean;
  type
    TFillOrderState = (fosWait, fosBeginProc, fosBeginVar);
  var
    t: TTextParsing;
    i: Integer;
    p, paramB, paramE: PTokenData;
    state: TFillOrderState;
    vName, vParam: TPascalString;
    v: Variant;
  begin
    Result := False;

    t := TTextParsing.Create(order_s, tsText);
    try
      i := 0;
      state := fosWait;
      p := nil;
      paramB := nil;
      paramE := nil;

      while i < t.TokenCount do
        begin
          case state of
            fosWait:
              begin
                p := t.TokenProbeR(i, [ttAscii]);
                if p = nil then
                    break;
                vName := p^.Text;
                i := p^.Index + 1;
                p := t.TokenProbeR(i, [ttSymbol]);
                if p = nil then
                  begin
                    DoStatus('Illegal declaration: %s', [vName.Text]);
                    exit;
                  end;
                i := p^.Index;
                if p^.Text.Same('=', ':') then
                  begin
                    state := fosBeginVar;
                    paramB := p;
                    paramE := p;
                  end
                else if p^.Text.Same('(') then
                  begin
                    state := fosBeginProc;
                    paramB := p;
                    paramE := p;
                  end
                else if p^.Text.Same(',') then
                    inc(i)
                else
                  begin
                    DoStatus('Illegal declaration: %s', [vName.Text]);
                    exit;
                  end;
                continue;
              end;
            fosBeginProc:
              begin
                paramE := t.IndentSymbolEndProbeR(paramE^.Index, '(', ')');
                if paramE = nil then
                  begin
                    DoStatus('Illegal function indent: %s', [vName.Text]);
                    exit;
                  end;

                vParam := t.TokenCombine(paramB^.Index, paramE^.Index);

                if umlTrimSpace(vParam) <> '' then
                  begin
                    if oprt = nil then
                        oprt := RT.Create;

                    oprt.Size := @o.Size;
                    oprt.COLOR := @o.COLOR;
                    if VarIsNull(EvaluateExpressionValue(tsPascal, vName.Text + vParam.Text, oprt)) then
                      begin
                        DoStatus('Illegal Expression: %s', [vName.Text + vParam.Text]);
                        exit;
                      end;
                  end;

                if paramE = nil then
                    break;

                i := paramE^.Index + 1;
                state := fosWait;
              end;
            fosBeginVar:
              begin
                if paramE <> nil then
                    paramE := t.TokenProbeR(paramE^.Index + 1, [ttSymbol]);

                if paramE <> nil then
                  begin
                    if paramE^.Text.Same('(') then
                      begin
                        paramE := t.IndentSymbolEndProbeR(paramE^.Index, '(', ')');
                        if paramE = nil then
                          begin
                            DoStatus('Illegal variant set indent: %s', [vName.Text]);
                            exit;
                          end;
                        continue;
                      end
                    else if not paramE^.Text.Same(',', ';') then
                        continue
                    else
                        vParam := t.TokenCombine(paramB^.Index + 1, paramE^.Index - 1);
                  end
                else
                    vParam := t.TokenCombine(paramB^.Index + 1, t.TokenCount - 1);

                vParam := umlTrimSpace(vParam);
                if vParam.Len > 0 then
                  begin
                    if oprt = nil then
                        oprt := RT.Create;

                    oprt.Size := @o.Size;
                    oprt.COLOR := @o.COLOR;
                    v := EvaluateExpressionValue(tsPascal, vParam.Text, oprt);

                    if vName.Same('s', 'size', 'siz') then
                        o.Size := v
                    else if vName.Same('red', 'r') then
                        o.COLOR[0] := v
                    else if vName.Same('green', 'g') then
                        o.COLOR[1] := v
                    else if vName.Same('blue', 'b') then
                        o.COLOR[2] := v
                    else if vName.Same('alpha', 'a') then
                        o.COLOR[3] := v
                    else
                      begin
                        DoStatus('Invalid variable: %s', [vName.Text]);
                        exit;
                      end;
                  end;

                if paramE = nil then
                    break;

                i := paramE^.Index + 1;
                state := fosWait;
              end;
          end;
        end;

      Result := True;
    finally
        disposeObject(t);
    end;
  end;

  procedure ProcessLBreak(o: TDSegmentionText; L: TSegmentionTextLists);
  var
    n_o: TDSegmentionText;
    n: TPascalString;
  begin
    n := umlDeleteChar(o.Text, #13);

    n_o.Text := umlGetFirstStr_Discontinuity(n, #10);
    n_o.Size := o.Size;
    n_o.COLOR := o.COLOR;
    L.Last.Add(n_o);

    while n.Exists(#10) do
      begin
        n := umlDeleteFirstStr_Discontinuity(n, #10);
        n_o.Text := umlGetFirstStr_Discontinuity(n, #10);
        n_o.Size := o.Size;
        n_o.COLOR := o.COLOR;
        L.Add(TSegmentionTextList.Create);
        L.Last.Add(n_o);
      end;
  end;

var
  t: TTextParsing;
  i, j: Integer;
  L: TSegmentionTextLists;
  o: TDSegmentionText;
  pb, pe: PTokenData;
  error: Boolean;
  n: TPascalString;
begin
  SetLength(Result, 0);

  if not IsSegmentionText(s) then
    begin
      SetLength(Result, 1, 1);
      Result[0, 0].Text := s;
      Result[0, 0].Size := Size;
      Result[0, 0].COLOR := COLOR;
      exit;
    end;

  t := TTextParsing.Create(s, tsText);
  L := TSegmentionTextLists.Create;
  L.Add(TSegmentionTextList.Create);
  oprt := nil;

  error := False;

  o.Text := '';
  o.Size := Size;
  o.COLOR := COLOR;

  i := t.FirstToken^.Index;
  while i < t.TokenCount do
    begin
      pb := t.TokenProbeR(i, [ttSymbol], '|');
      if pb = nil then
        begin
          o.Text := t.TokenCombine(i, t.TokenCount - 1);
          ProcessLBreak(o, L);
          break;
        end
      else
        begin
          if pb^.Index > i then
            begin
              o.Text := t.TokenCombine(i, pb^.Index - 1);
              ProcessLBreak(o, L);
            end;
          pe := t.TokenProbeR(pb^.Index + 1, [ttSymbol], '|');
          if pe = nil then
            begin
              DoStatus('%s There is no end sign:"|", or there should to be no "|" sign.', [s.Text]);
              error := True;
              break;
            end
          else
            begin
              i := pe^.Index + 1;

              // fill order preset
              if pb^.Index + 1 < pe^.Index - 1 then
                begin
                  n := t.TokenCombine(pb^.Index + 1, pe^.Index - 1);
                  n := umlTrimSpace(n);
                end
              else
                  n := '';

              if (n.Len > 0) and (not n.Same('default', 'nil', 'null', 'origin')) then
                begin
                  o.Size := Size;
                  o.COLOR := COLOR;
                  if not FillSegmentionTextSet_(n, o) then
                    begin
                      error := True;
                      break;
                    end;
                end
              else
                begin
                  o.Size := Size;
                  o.COLOR := COLOR;
                end;
            end;
        end;
    end;
  disposeObject(t);

  if not error then
    begin
      SetLength(Result, L.Count);
      for j := 0 to L.Count - 1 do
        begin
          SetLength(Result[j], L[j].Count);
          for i := 0 to L[j].Count - 1 do
              Result[j, i] := L[j][i];
        end;
    end;

  for i := 0 to L.Count - 1 do
      disposeObject(L[i]);
  disposeObject(L);

  if oprt <> nil then
      disposeObject(oprt);
end;

procedure FreeSegmentionText(var buff: TDArraySegmentionText);
var
  i, j: Integer;
begin
  for j := low(buff) to high(buff) do
    begin
      for i := low(buff[j]) to high(buff[j]) do
          buff[j, i].Text := '';
      SetLength(buff[j], 0);
    end;
  SetLength(buff, 0);
end;

function TDE4V.IsEqual(Dest: TDE4V): Boolean;
begin
  Result := Geometry2DUnit.IsEqual(Left, Dest.Left) and
    Geometry2DUnit.IsEqual(Top, Dest.Top) and
    Geometry2DUnit.IsEqual(Right, Dest.Right) and
    Geometry2DUnit.IsEqual(Bottom, Dest.Bottom) and
    Geometry2DUnit.IsEqual(Angle, Dest.Angle);
end;

function TDE4V.IsZero: Boolean;
begin
  Result :=
    Geometry2DUnit.IsZero(Left) and
    Geometry2DUnit.IsZero(Top) and
    Geometry2DUnit.IsZero(Right) and
    Geometry2DUnit.IsZero(Bottom);
end;

function TDE4V.width: TDEFloat;
begin
  if Right > Left then
      Result := Right - Left
  else
      Result := Left - Right;
end;

function TDE4V.height: TDEFloat;
begin
  if Bottom > Top then
      Result := Bottom - Top
  else
      Result := Top - Bottom;
end;

function TDE4V.MakeRectV2: TDERect;
begin
  Result := DERect(Left, Top, Right, Bottom);
end;

function TDE4V.MakeRectf: TRectf;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

function TDE4V.BoundRect: TDERect;
begin
  Result := Geometry2DUnit.TV2Rect4.Init(MakeRectV2, Angle).BoundRect;
end;

function TDE4V.Centroid: TDEVec;
begin
  Result := Geometry2DUnit.TV2Rect4.Init(MakeRectV2, Angle).Centroid;
end;

function TDE4V.Add(v: TDEVec): TDE4V;
var
  r: TDERect;
begin
  r := MakeRectV2;
  r[0] := Vec2Add(r[0], v);
  r[1] := Vec2Add(r[1], v);
  Result := Init(r, Angle);
end;

function TDE4V.Add(x, y: TDEFloat): TDE4V;
var
  r: TDERect;
begin
  r := MakeRectV2;
  r[0] := Vec2Add(r[0], x, y);
  r[1] := Vec2Add(r[1], x, y);
  Result := Init(r, Angle);
end;

function TDE4V.Scale(f: TDEFloat): TDE4V;
begin
  Result.Left := Left * f;
  Result.Top := Top * f;
  Result.Right := Right * f;
  Result.Bottom := Bottom * f;
  Result.Angle := Angle;
end;

function TDE4V.GetDistance(Dest: TDE4V): TDEFloat;
begin
  Result := Geometry3DUnit.Distance(MakeRectV2, Dest.MakeRectV2);
end;

function TDE4V.GetAngleDistance(Dest: TDE4V): TDEFloat;
begin
  Result := Geometry3DUnit.AngleDistance(Angle, Dest.Angle);
end;

function TDE4V.MovementToLerp(Dest: TDE4V; mLerp, rLerp: Double): TDE4V;
var
  r: TDERect;
begin
  Result.Angle := MovementLerp(Angle, Dest.Angle, rLerp);

  r := MovementLerp(MakeRectV2, Dest.MakeRectV2, mLerp);
  Result.Left := r[0][0];
  Result.Top := r[0][1];
  Result.Right := r[1][0];
  Result.Bottom := r[1][1];
end;

function TDE4V.MovementToDistance(Dest: TDE4V; mSpeed, rSpeed: TDEFloat): TDE4V;
var
  r: TDERect;
begin
  Result.Angle := SmoothAngle(Angle, Dest.Angle, rSpeed);

  r := MovementDistance(MakeRectV2, Dest.MakeRectV2, mSpeed);
  Result.Left := r[0][0];
  Result.Top := r[0][1];
  Result.Right := r[1][0];
  Result.Bottom := r[1][1];
end;

function TDE4V.MovementToDistanceCompleteTime(Dest: TDE4V; mSpeed, rSpeed: TDEFloat): Double;
var
  d1, d2: Double;
begin
  d1 := Geometry3DUnit.AngleRollDistanceDeltaTime(Angle, Dest.Angle, rSpeed);
  d2 := Geometry3DUnit.MovementDistanceDeltaTime(MakeRectV2, Dest.MakeRectV2, mSpeed);
  if d1 > d2 then
      Result := d1
  else
      Result := d2;
end;

function TDE4V.Fit(Dest: TDE4V): TDE4V;
var
  r: TDERect;
begin
  r := RectFit(Dest.MakeRectV2, MakeRectV2);
  Result.Angle := Angle;
  Result.Left := r[0][0];
  Result.Top := r[0][1];
  Result.Right := r[1][0];
  Result.Bottom := r[1][1];
end;

function TDE4V.Fit(Dest: TDERect): TDE4V;
var
  r: TDERect;
begin
  r := RectFit(Dest, MakeRectV2);
  Result.Angle := Angle;
  Result.Left := r[0][0];
  Result.Top := r[0][1];
  Result.Right := r[1][0];
  Result.Bottom := r[1][1];
end;

class function TDE4V.Init(r: TDERect; Ang: TDEFloat): TDE4V;
begin
  with Result do
    begin
      Left := r[0][0];
      Top := r[0][1];
      Right := r[1][0];
      Bottom := r[1][1];
      Angle := Ang;
    end;
end;

class function TDE4V.Init(r: TDERect): TDE4V;
begin
  Result := TDE4V.Init(r, 0);
end;

class function TDE4V.Init(r: TRectf; Ang: TDEFloat): TDE4V;
begin
  Result := Init(DERect(r), Ang);
end;

class function TDE4V.Init(r: TRectf): TDE4V;
begin
  Result := TDE4V.Init(r, 0);
end;

class function TDE4V.Init(r: TRect; Ang: TDEFloat): TDE4V;
begin
  Result := Init(DERect(r), Ang);
end;

class function TDE4V.Init(r: TRect): TDE4V;
begin
  Result := TDE4V.Init(r, 0);
end;

class function TDE4V.Init(CenPos: TDEVec; Width_, Height_, Ang: TDEFloat): TDE4V;
var
  r: TDERect;
begin
  r[0][0] := CenPos[0] - Width_ * 0.5;
  r[0][1] := CenPos[1] - Height_ * 0.5;
  r[1][0] := CenPos[0] + Width_ * 0.5;
  r[1][1] := CenPos[1] + Height_ * 0.5;
  Result := Init(r, Ang);
end;

class function TDE4V.Init(Width_, Height_, Ang: TDEFloat): TDE4V;
begin
  Result := Init(DERect(0, 0, Width_, Height_), Ang);
end;

class function TDE4V.Init: TDE4V;
begin
  Result := Init(ZeroRect, 0);
end;

class function TDE4V.Create(r: TDERect; Ang: TDEFloat): TDE4V;
begin
  with Result do
    begin
      Left := r[0][0];
      Top := r[0][1];
      Right := r[1][0];
      Bottom := r[1][1];
      Angle := Ang;
    end;
end;

class function TDE4V.Create(r: TDERect): TDE4V;
begin
  Result := TDE4V.Create(r, 0);
end;

class function TDE4V.Create(r: TRectf; Ang: TDEFloat): TDE4V;
begin
  Result := Create(DERect(r), Ang);
end;

class function TDE4V.Create(r: TRectf): TDE4V;
begin
  Result := TDE4V.Create(r, 0);
end;

class function TDE4V.Create(r: TRect; Ang: TDEFloat): TDE4V;
begin
  Result := Create(DERect(r), Ang);
end;

class function TDE4V.Create(r: TRect): TDE4V;
begin
  Result := TDE4V.Create(r, 0);
end;

class function TDE4V.Create(CenPos: TDEVec; Width_, Height_, Ang: TDEFloat): TDE4V;
var
  r: TDERect;
begin
  r[0][0] := CenPos[0] - Width_ * 0.5;
  r[0][1] := CenPos[1] - Height_ * 0.5;
  r[1][0] := CenPos[0] + Width_ * 0.5;
  r[1][1] := CenPos[1] + Height_ * 0.5;
  Result := Create(r, Ang);
end;

class function TDE4V.Create(Width_, Height_, Ang: TDEFloat): TDE4V;
begin
  Result := Create(DERect(0, 0, Width_, Height_), Ang);
end;

class function TDE4V.Create: TDE4V;
begin
  Result := Create(ZeroRect, 0);
end;

constructor TDETexture.Create;
begin
  inherited Create;
  LastDrawUsage := GetTimeTick();
  FIsShadow := False;
  FStaticShadow := nil;
  FSIGMA := 0;
  FSigmaGaussianKernelFactor := 0;

  if TexturePool <> nil then
      TexturePool.AddTexture(Self);
end;

destructor TDETexture.Destroy;
begin
  if FStaticShadow <> nil then
      disposeObject(FStaticShadow);

  if TexturePool <> nil then
      TexturePool.RemoveTexture(Self);
  inherited Destroy;
end;

procedure TDETexture.DrawUsage;
begin
  LastDrawUsage := GetTimeTick();
end;

procedure TDETexture.ReleaseGPUMemory;
begin
end;

procedure TDETexture.NoUsage;
begin
  inherited NoUsage;
  ReleaseGPUMemory();
  DisposeObjectAndNil(FStaticShadow);
end;

function TDETexture.StaticShadow: TDETexture;
begin
  Result := StaticShadow(0, 0);
end;

function TDETexture.StaticShadow(const SIGMA_: TGeoFloat; const SigmaGaussianKernelFactor_: Integer): TDETexture;
var
  i: Integer;
  bits_: PRColorArray;
begin
  if (not IsEqual(FSIGMA, SIGMA_)) or (FSigmaGaussianKernelFactor <> SigmaGaussianKernelFactor_) and (FStaticShadow <> nil) then
      DisposeObjectAndNil(FStaticShadow);

  if FStaticShadow = nil then
    begin
      if not FIsShadow then
        begin
          FStaticShadow := DefaultTextureClass.Create;
          FStaticShadow.FIsShadow := True;
          FStaticShadow.SetSize(width, height);
          bits_ := FStaticShadow.Bits;
          for i := (width * height) - 1 downto 0 do
              bits_^[i] := RasterColor(0, 0, 0, TRasterColorEntry(Bits^[i]).a);

          if (SIGMA_ > 0) and (SigmaGaussianKernelFactor_ > 0) then
              FStaticShadow.SigmaGaussian(SIGMA_, SigmaGaussianKernelFactor_);

          FSIGMA := SIGMA_;
          FSigmaGaussianKernelFactor := SigmaGaussianKernelFactor_;
        end
      else
          Result := Self;
    end;
  Result := FStaticShadow;
end;

constructor TDETextureList.Create;
begin
  inherited Create;
  Critical := TCritical.Create;
end;

destructor TDETextureList.Destroy;
begin
  ReleaseGPUMemory();
  disposeObject(Critical);
  Critical := nil;
  inherited Destroy;
end;

procedure TDETextureList.AddTexture(tex: TDETexture);
begin
  Critical.Acquire;
  inherited Add(tex);
  Critical.Release;
end;

procedure TDETextureList.RemoveTexture(tex: TDETexture);
var
  i: Integer;
begin
  Critical.Acquire;
  i := Count - 1;
  while i >= 0 do
    begin
      if Items[i] = tex then
        begin
          delete(i);
          break;
        end
      else
          dec(i);
    end;
  Critical.Release;
end;

procedure TDETextureList.ReleaseGPUMemory;
var
  i: Integer;
begin
  Critical.Acquire;
  for i := 0 to Count - 1 do
      Items[i].ReleaseGPUMemory();
  Critical.Release;
end;

procedure TDETextureList.ReleaseNoUsageTextureMemory(timeout: TTimeTick);
var
  i: Integer;
  tk: TTimeTick;
begin
  Critical.Acquire;
  tk := GetTimeTick();
  for i := 0 to Count - 1 do
    if tk - Items[i].LastDrawUsage > timeout then
        Items[i].ReleaseGPUMemory();
  Critical.Release;
end;

procedure TDrawEngineInterface.SetSize(r: TDERect);
begin

end;

procedure TDrawEngineInterface.SetLineWidth(w: TDEFloat);
begin

end;

procedure TDrawEngineInterface.DrawDotLine(pt1, pt2: TDEVec; COLOR: TDEColor);
begin

end;

procedure TDrawEngineInterface.DrawLine(pt1, pt2: TDEVec; COLOR: TDEColor);
begin

end;

procedure TDrawEngineInterface.DrawRect(r: TDERect; Angle: TDEFloat; COLOR: TDEColor);
begin

end;

procedure TDrawEngineInterface.FillRect(r: TDERect; Angle: TDEFloat; COLOR: TDEColor);
begin

end;

procedure TDrawEngineInterface.DrawEllipse(r: TDERect; COLOR: TDEColor);
begin

end;

procedure TDrawEngineInterface.FillEllipse(r: TDERect; COLOR: TDEColor);
begin

end;

procedure TDrawEngineInterface.FillPolygon(PolygonBuff: TArrayVec2; COLOR: TDEColor);
begin

end;

procedure TDrawEngineInterface.DrawText(Shadow: Boolean; Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean; RotateVec: TDEVec; Angle: TDEFloat);
begin

end;

procedure TDrawEngineInterface.DrawPicture(Shadow: Boolean; t: TCoreClassObject; Sour, Dest: TDE4V; Alpha: TDEFloat);
begin

end;

procedure TDrawEngineInterface.Flush;
begin

end;

procedure TDrawEngineInterface.ResetState;
begin

end;

procedure TDrawEngineInterface.BeginDraw;
begin

end;

procedure TDrawEngineInterface.EndDraw;
begin

end;

function TDrawEngineInterface.CurrentScreenSize: TDEVec;
begin
  Result := NULLVec;
end;

function TDrawEngineInterface.GetTextSize(const Text: SystemString; Size: TDEFloat): TDEVec;
begin
  Result := NULLVec;
end;

function TDrawEngineInterface.ReadyOK: Boolean;
begin
  Result := False;
end;

function TDrawCommandParam_DrawText.IsNormal: Boolean;
begin
  Result := IsEqual(r, bak_r) and VectorEquals(COLOR, bak_color);
end;

function TDrawCommandParam_Picture.IsNormal: Boolean;
begin
  Result := (t = bak_t) and IsEqual(Alpha, bak_alpha) and Dest.IsEqual(bak_dest);
end;

procedure TDrawCommand.DoFreeData;
begin
  case t of
    dctSetLineWidth: Dispose(PDrawCommandParam_1Float(Data));
    dctDotLine, dctLine: Dispose(PDrawCommandParam_PT_Color(Data));
    dctSetSize: Dispose(PDrawCommandParam_1Rect(Data));
    dctDrawRect, dctFillRect, dctDrawEllipse, dctFillEllipse: Dispose(PDrawCommandParam_Rect_Color(Data));
    dctPolygon:
      begin
        SetLength(PDrawCommandParam_Polygon(Data)^.PolygonBuff, 0);
        Dispose(PDrawCommandParam_Polygon(Data));
      end;
    dctDrawText:
      begin
        PDrawCommandParam_DrawText(Data)^.Text := '';
        Dispose(PDrawCommandParam_DrawText(Data));
      end;
    dctDrawPicture: Dispose(PDrawCommandParam_Picture(Data));
    dctUserCustom: Dispose(PDrawCommandParam_Custom(Data));
  end;
end;

procedure TDrawCommand.Execute(OwnerDrawExecute: TDrawExecute; Draw: TDrawEngineInterface);
begin
  case t of
    dctSetSize: with PDrawCommandParam_1Rect(Data)^ do
          Draw.SetSize(r);
    dctSetLineWidth: with PDrawCommandParam_1Float(Data)^ do
          Draw.SetLineWidth(f);
    dctDotLine: with PDrawCommandParam_PT_Color(Data)^ do
          Draw.DrawDotLine(pt1, pt2, COLOR);
    dctLine: with PDrawCommandParam_PT_Color(Data)^ do
          Draw.DrawLine(pt1, pt2, COLOR);
    dctDrawRect: with PDrawCommandParam_Rect_Color(Data)^ do
          Draw.DrawRect(r, Angle, COLOR);
    dctFillRect: with PDrawCommandParam_Rect_Color(Data)^ do
          Draw.FillRect(r, Angle, COLOR);
    dctDrawEllipse: with PDrawCommandParam_Rect_Color(Data)^ do
          Draw.DrawEllipse(r, COLOR);
    dctFillEllipse: with PDrawCommandParam_Rect_Color(Data)^ do
          Draw.FillEllipse(r, COLOR);
    dctPolygon: with PDrawCommandParam_Polygon(Data)^ do
          Draw.FillPolygon(PolygonBuff, COLOR);
    dctDrawText: with PDrawCommandParam_DrawText(Data)^ do
          Draw.DrawText(not IsNormal, Text, Size, r, COLOR, center, RotateVec, Angle);
    dctDrawPicture: with PDrawCommandParam_Picture(Data)^ do
          Draw.DrawPicture(not IsNormal, t, Sour, Dest, Alpha);
    dctUserCustom: with PDrawCommandParam_Custom(Data)^ do
          OnDraw(OwnerDrawExecute.FOwner, Draw, UserData, UserObject);
    dctFlush: Draw.Flush;
  end;
end;

procedure TDrawCommand.CopyTo(var Dst: TDrawCommand);
var
  i: Integer;
begin
  Dst.t := t;
  Dst.Data := nil;
  case t of
    dctSetSize:
      begin
        new(PDrawCommandParam_1Rect(Dst.Data));
        PDrawCommandParam_1Rect(Dst.Data)^ := PDrawCommandParam_1Rect(Data)^;
      end;
    dctSetLineWidth:
      begin
        new(PDrawCommandParam_1Float(Dst.Data));
        PDrawCommandParam_1Float(Dst.Data)^ := PDrawCommandParam_1Float(Data)^;
      end;
    dctDotLine:
      begin
        new(PDrawCommandParam_PT_Color(Dst.Data));
        PDrawCommandParam_PT_Color(Dst.Data)^ := PDrawCommandParam_PT_Color(Data)^;
      end;
    dctLine:
      begin
        new(PDrawCommandParam_PT_Color(Dst.Data));
        PDrawCommandParam_PT_Color(Dst.Data)^ := PDrawCommandParam_PT_Color(Data)^;
      end;
    dctDrawRect:
      begin
        new(PDrawCommandParam_Rect_Color(Dst.Data));
        PDrawCommandParam_Rect_Color(Dst.Data)^ := PDrawCommandParam_Rect_Color(Data)^;
      end;
    dctFillRect:
      begin
        new(PDrawCommandParam_Rect_Color(Dst.Data));
        PDrawCommandParam_Rect_Color(Dst.Data)^ := PDrawCommandParam_Rect_Color(Data)^;
      end;
    dctDrawEllipse:
      begin
        new(PDrawCommandParam_Rect_Color(Dst.Data));
        PDrawCommandParam_Rect_Color(Dst.Data)^ := PDrawCommandParam_Rect_Color(Data)^;
      end;
    dctFillEllipse:
      begin
        new(PDrawCommandParam_Rect_Color(Dst.Data));
        PDrawCommandParam_Rect_Color(Dst.Data)^ := PDrawCommandParam_Rect_Color(Data)^;
      end;
    dctPolygon:
      begin
        new(PDrawCommandParam_Polygon(Dst.Data));
        PDrawCommandParam_Polygon(Dst.Data)^.COLOR := PDrawCommandParam_Polygon(Data)^.COLOR;
        for i := 0 to length(PDrawCommandParam_Polygon(Data)^.PolygonBuff) - 1 do
            PDrawCommandParam_Polygon(Dst.Data)^.PolygonBuff[i] := PDrawCommandParam_Polygon(Data)^.PolygonBuff[i];
      end;
    dctDrawText:
      begin
        new(PDrawCommandParam_DrawText(Dst.Data));
        PDrawCommandParam_DrawText(Dst.Data)^ := PDrawCommandParam_DrawText(Data)^;
      end;
    dctDrawPicture:
      begin
        new(PDrawCommandParam_Picture(Dst.Data));
        PDrawCommandParam_Picture(Dst.Data)^ := PDrawCommandParam_Picture(Data)^;
      end;
    dctUserCustom:
      begin
        new(PDrawCommandParam_Custom(Dst.Data));
        PDrawCommandParam_Custom(Dst.Data)^ := PDrawCommandParam_Custom(Data)^;
      end;
  end;
end;

constructor TDrawQueue.Create(AOwner: TDrawEngine);
begin
  inherited Create;
  FOwner := AOwner;
  FCommandList := TCoreClassList.Create;

  FStartDrawShadowIndex := -1;
  FScreenShadowOffset := NULLVec;
  FScreenShadowAlpha := 0.5;
  FShadowSIGMA := 0;
  FShadowSigmaGaussianKernelFactor := 0;
end;

destructor TDrawQueue.Destroy;
begin
  Clear(True);
  disposeObject(FCommandList);
  inherited Destroy;
end;

procedure TDrawQueue.Assign(Source: TDrawQueue);
var
  i: Integer;
  p: PDrawCommand;
begin
  LockObject(FCommandList);
  LockObject(Source.FCommandList);
  for i := 0 to Source.FCommandList.Count - 1 do
    begin
      new(p);
      PDrawCommand(Source.FCommandList[i])^.CopyTo(p^);
      FCommandList.Add(p);
    end;
  UnLockObject(FCommandList);
  UnLockObject(Source.FCommandList);
end;

procedure TDrawQueue.Clear(ForceFree: Boolean);
var
  i: Integer;
  p: PDrawCommand;
begin
  LockObject(FCommandList);
  for i := 0 to FCommandList.Count - 1 do
    begin
      p := FCommandList[i];
      if ForceFree then
          p^.DoFreeData;
      Dispose(p);
    end;
  FCommandList.Clear;
  UnLockObject(FCommandList);
end;

procedure TDrawQueue.SetSize(r: TDERect);
var
  p: PDrawCommand;
  Data: PDrawCommandParam_1Rect;
begin
  new(p);
  new(Data);

  Data^.r := r;

  p^.t := dctSetSize;
  p^.Data := Data;

  LockObject(FCommandList);
  FCommandList.Add(p);
  UnLockObject(FCommandList);
end;

procedure TDrawQueue.SetLineWidth(w: TDEFloat);
var
  p: PDrawCommand;
  Data: PDrawCommandParam_1Float;
begin
  new(p);
  new(Data);

  Data^.f := w;

  p^.t := dctSetLineWidth;
  p^.Data := Data;

  LockObject(FCommandList);
  FCommandList.Add(p);
  UnLockObject(FCommandList);
end;

procedure TDrawQueue.DrawDotLine(pt1, pt2: TDEVec; COLOR: TDEColor);
var
  p: PDrawCommand;
  Data: PDrawCommandParam_PT_Color;
begin
  if DEAlpha(COLOR) > 0 then
    begin
      new(p);
      new(Data);

      Data^.pt1 := pt1;
      Data^.pt2 := pt2;
      Data^.COLOR := COLOR;

      p^.t := dctDotLine;
      p^.Data := Data;

      LockObject(FCommandList);
      FCommandList.Add(p);
      UnLockObject(FCommandList);
    end;
end;

procedure TDrawQueue.DrawLine(pt1, pt2: TDEVec; COLOR: TDEColor);
var
  p: PDrawCommand;
  Data: PDrawCommandParam_PT_Color;
begin
  if DEAlpha(COLOR) > 0 then
    begin
      new(p);
      new(Data);

      Data^.pt1 := pt1;
      Data^.pt2 := pt2;
      Data^.COLOR := COLOR;

      p^.t := dctLine;
      p^.Data := Data;

      LockObject(FCommandList);
      FCommandList.Add(p);
      UnLockObject(FCommandList);
    end;
end;

procedure TDrawQueue.DrawRect(r: TDERect; Angle: TDEFloat; COLOR: TDEColor);
var
  p: PDrawCommand;
  Data: PDrawCommandParam_Rect_Color;
begin
  if DEAlpha(COLOR) > 0 then
    begin
      new(p);
      new(Data);

      Data^.r := r;
      Data^.Angle := Angle;
      Data^.COLOR := COLOR;

      p^.t := dctDrawRect;
      p^.Data := Data;

      LockObject(FCommandList);
      FCommandList.Add(p);
      UnLockObject(FCommandList);
    end;
end;

procedure TDrawQueue.FillRect(r: TDERect; Angle: TDEFloat; COLOR: TDEColor);
var
  p: PDrawCommand;
  Data: PDrawCommandParam_Rect_Color;
begin
  if DEAlpha(COLOR) > 0 then
    begin
      new(p);
      new(Data);

      Data^.r := r;
      Data^.Angle := Angle;
      Data^.COLOR := COLOR;

      p^.t := dctFillRect;
      p^.Data := Data;

      LockObject(FCommandList);
      FCommandList.Add(p);
      UnLockObject(FCommandList);
    end;
end;

procedure TDrawQueue.DrawEllipse(pt: TDEVec; radius: TDEFloat; COLOR: TDEColor);
var
  r: TDERect;
begin
  if DEAlpha(COLOR) > 0 then
    begin
      r[0][0] := pt[0] - radius;
      r[0][1] := pt[1] - radius;
      r[1][0] := pt[0] + radius;
      r[1][1] := pt[1] + radius;
      DrawEllipse(r, COLOR);
    end;
end;

procedure TDrawQueue.DrawEllipse(r: TDERect; COLOR: TDEColor);
var
  p: PDrawCommand;
  Data: PDrawCommandParam_Rect_Color;
begin
  if DEAlpha(COLOR) > 0 then
    begin
      new(p);
      new(Data);

      Data^.r := r;
      Data^.COLOR := COLOR;

      p^.t := dctDrawEllipse;
      p^.Data := Data;

      LockObject(FCommandList);
      FCommandList.Add(p);
      UnLockObject(FCommandList);
    end;
end;

procedure TDrawQueue.FillEllipse(pt: TDEVec; radius: TDEFloat; COLOR: TDEColor);
var
  r: TDERect;
begin
  if DEAlpha(COLOR) > 0 then
    begin
      r[0][0] := pt[0] - radius;
      r[0][1] := pt[1] - radius;
      r[1][0] := pt[0] + radius;
      r[1][1] := pt[1] + radius;
      FillEllipse(r, COLOR);
    end;
end;

procedure TDrawQueue.FillEllipse(r: TDERect; COLOR: TDEColor);
var
  p: PDrawCommand;
  Data: PDrawCommandParam_Rect_Color;
begin
  if DEAlpha(COLOR) > 0 then
    begin
      new(p);
      new(Data);

      Data^.r := r;
      Data^.COLOR := COLOR;

      p^.t := dctFillEllipse;
      p^.Data := Data;

      LockObject(FCommandList);
      FCommandList.Add(p);
      UnLockObject(FCommandList);
    end;
end;

procedure TDrawQueue.FillPolygon(PolygonBuff: TArrayVec2; COLOR: TDEColor);
var
  p: PDrawCommand;
  Data: PDrawCommandParam_Polygon;
  i: Integer;
begin
  if DEAlpha(COLOR) > 0 then
    begin
      new(p);
      new(Data);

      SetLength(Data^.PolygonBuff, length(PolygonBuff));
      for i := 0 to length(PolygonBuff) - 1 do
          Data^.PolygonBuff[i] := PolygonBuff[i];
      Data^.COLOR := COLOR;

      p^.t := dctPolygon;
      p^.Data := Data;

      LockObject(FCommandList);
      FCommandList.Add(p);
      UnLockObject(FCommandList);
    end;
end;

procedure TDrawQueue.DrawText(const Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean; RotateVec: TDEVec; Angle: TDEFloat);
var
  PrepareDraw: Boolean;
  p: PDrawCommand;
  Data: PDrawCommandParam_DrawText;
begin
  if DEAlpha(COLOR) > 0 then
    begin
      PrepareDraw := False;
      PrepareDraw := PrepareDraw or RectWithinRect(r, Owner.ScreenRect);
      PrepareDraw := PrepareDraw or RectWithinRect(Owner.ScreenRect, r);
      PrepareDraw := PrepareDraw or RectToRectIntersect(Owner.ScreenRect, r);
      PrepareDraw := PrepareDraw or RectToRectIntersect(r, Owner.ScreenRect);
      if PrepareDraw then
        begin
          new(p);
          new(Data);

          Data^.Text := Text;
          Data^.Size := Size;
          Data^.r := r;
          Data^.COLOR := COLOR;
          Data^.center := center;
          Data^.RotateVec := RotateVec;
          Data^.Angle := Angle;

          Data^.bak_r := Data^.r;
          Data^.bak_color := Data^.COLOR;

          p^.t := dctDrawText;
          p^.Data := Data;

          if (FStartDrawShadowIndex >= 0) then
            begin
              Data^.r := Geometry2DUnit.RectOffset(Data^.r, FScreenShadowOffset);
              Data^.COLOR := DEColor(0, 0, 0, Data^.COLOR[3] * FScreenShadowAlpha);
            end;

          LockObject(FCommandList);
          FCommandList.Add(p);
          UnLockObject(FCommandList);
        end;
    end;
end;

procedure TDrawQueue.DrawPicture(t: TCoreClassObject; Sour, Dest: TDE4V; Alpha: TDEFloat);
var
  PrepareDraw: Boolean;
  p: PDrawCommand;
  Data: PDrawCommandParam_Picture;
  r: TDERect;
begin
  if Alpha > 0 then
    begin
      r := Dest.BoundRect;
      PrepareDraw := False;
      PrepareDraw := PrepareDraw or RectWithinRect(r, Owner.ScreenRect);
      PrepareDraw := PrepareDraw or RectWithinRect(Owner.ScreenRect, r);
      PrepareDraw := PrepareDraw or RectToRectIntersect(Owner.ScreenRect, r);
      PrepareDraw := PrepareDraw or RectToRectIntersect(r, Owner.ScreenRect);
      if PrepareDraw then
        begin
          new(p);
          new(Data);

          Data^.t := t;
          Data^.Sour := Sour;
          Data^.Dest := Dest;
          Data^.Alpha := Alpha;

          Data^.bak_t := t;
          Data^.bak_dest := Dest;
          Data^.bak_alpha := Alpha;

          p^.t := dctDrawPicture;
          p^.Data := Data;

          if (FStartDrawShadowIndex >= 0) and (Data^.t is TDETexture) and (not TDETexture(Data^.t).FIsShadow) then
            begin
              Data^.t := TDETexture(Data^.t).StaticShadow(FShadowSIGMA, FShadowSigmaGaussianKernelFactor);
              Data^.Dest := Data^.Dest.Add(FScreenShadowOffset);
              Data^.Alpha := Data^.Alpha * FScreenShadowAlpha;
            end;

          LockObject(FCommandList);
          FCommandList.Add(p);
          UnLockObject(FCommandList);
        end;
    end;
end;

procedure TDrawQueue.DrawUserCustom(const OnDraw: TCustomDraw_Method; const UserData: Pointer; const UserObject: TCoreClassObject);
var
  p: PDrawCommand;
  Data: PDrawCommandParam_Custom;
begin
  new(p);
  new(Data);

  Data^.OnDraw := OnDraw;
  Data^.UserData := UserData;
  Data^.UserObject := UserObject;

  p^.t := dctUserCustom;
  p^.Data := Data;

  LockObject(FCommandList);
  FCommandList.Add(p);
  UnLockObject(FCommandList);
end;

procedure TDrawQueue.Flush;
var
  p: PDrawCommand;
begin
  new(p);

  p^.t := dctFlush;
  p^.Data := nil;

  LockObject(FCommandList);
  FCommandList.Add(p);
  UnLockObject(FCommandList);
end;

procedure TDrawQueue.BeginCaptureShadow(const ScreenOffsetVec_: TDEVec; const alpha_: TDEFloat);
begin
  BeginCaptureShadow(ScreenOffsetVec_, alpha_, 0, 0);
end;

procedure TDrawQueue.BeginCaptureShadow(const ScreenOffsetVec_: TDEVec; const alpha_: TDEFloat; ShadowSIGMA_: TGeoFloat; ShadowSigmaGaussianKernelFactor_: Integer);
begin
  EndCaptureShadow;
  FStartDrawShadowIndex := FCommandList.Count;
  FScreenShadowOffset := ScreenOffsetVec_;
  FScreenShadowAlpha := alpha_;
  FShadowSIGMA := ShadowSIGMA_;
  FShadowSigmaGaussianKernelFactor := ShadowSigmaGaussianKernelFactor_;
end;

procedure TDrawQueue.EndCaptureShadow;
var
  i: Integer;
  lst: TCoreClassList;

  p: PDrawCommand;
  pTextureData: PDrawCommandParam_Picture;
  pTextData: PDrawCommandParam_DrawText;
begin
  if FStartDrawShadowIndex >= 0 then
    begin
      i := FStartDrawShadowIndex;
      FStartDrawShadowIndex := -1;

      lst := TCoreClassList.Create;

      LockObject(FCommandList);
      while i < FCommandList.Count do
        begin
          p := PDrawCommand(FCommandList[i]);
          if (p^.t = dctDrawPicture) and (PDrawCommandParam_Picture(p^.Data)^.t is TDETexture) and
            (TDETexture(PDrawCommandParam_Picture(p^.Data)^.t).FIsShadow) then
            begin
              new(pTextureData);
              pTextureData^ := PDrawCommandParam_Picture(p^.Data)^;
              pTextureData^.t := pTextureData^.bak_t;
              pTextureData^.Dest := pTextureData^.bak_dest;
              pTextureData^.Alpha := pTextureData^.bak_alpha;

              new(p);
              p^.t := dctDrawPicture;
              p^.Data := pTextureData;
              lst.Add(p);
              inc(i);
            end
          else if (p^.t = dctDrawText) then
            begin
              new(pTextData);
              pTextData^ := PDrawCommandParam_DrawText(p^.Data)^;
              pTextData^.r := pTextData^.bak_r;
              pTextData^.COLOR := pTextData^.bak_color;

              new(p);
              p^.t := dctDrawText;
              p^.Data := pTextData;
              lst.Add(p);
              inc(i);
            end
          else
            begin
              lst.Add(p);
              FCommandList.delete(i);
            end;
        end;

      for i := 0 to lst.Count - 1 do
          FCommandList.Add(lst[i]);
      UnLockObject(FCommandList);

      disposeObject(lst);
    end;
end;

procedure TDrawQueue.BuildTextureOutputState(var buff: TTextureOutputStateBuffer);
var
  i, j: Integer;
  p: PDrawCommand;
  ptex: PDrawCommandParam_Picture;
begin
  try
    j := 0;
    for i := 0 to FCommandList.Count - 1 do
      if (PDrawCommand(FCommandList[i])^.t = dctDrawPicture) then
          inc(j);

    SetLength(buff, j);
    j := 0;
    LockObject(FCommandList);
    for i := 0 to FCommandList.Count - 1 do
      begin
        p := PDrawCommand(FCommandList[i]);
        if (p^.t = dctDrawPicture) then
          begin
            ptex := PDrawCommandParam_Picture(p^.Data);
            buff[j].Source := ptex^.t;
            buff[j].SourceRect := TV2Rect4.Init(ptex^.Sour.MakeRectV2, ptex^.Sour.Angle);
            buff[j].DestScreen := TV2Rect4.Init(ptex^.Dest.MakeRectV2, ptex^.Dest.Angle);
            buff[j].Alpha := ptex^.Alpha;
            buff[j].Normal := ptex^.IsNormal;
            buff[j].Index := i;
            inc(j);
          end;
      end;
    UnLockObject(FCommandList);
  except
  end;
end;

constructor TDrawExecute.Create(AOwner: TDrawEngine);
begin
  inherited Create;
  FOwner := AOwner;
  FCommandList := TCoreClassList.Create;
  FSourQueue := nil;
end;

destructor TDrawExecute.Destroy;
begin
  Clear;

  disposeObject(FCommandList);
  inherited Destroy;
end;

procedure TDrawExecute.Clear;
var
  i: Integer;
  p: PDrawCommand;
begin
  for i := 0 to FCommandList.Count - 1 do
    begin
      p := FCommandList[i];
      p^.DoFreeData;
      Dispose(p);
    end;
  FCommandList.Clear;
end;

procedure TDrawExecute.PickQueue(Queue: TDrawQueue);
var
  i: Integer;
begin
  LockObject(Queue.FCommandList);
  for i := 0 to Queue.FCommandList.Count - 1 do
      FCommandList.Add(Queue.FCommandList[i]);
  Queue.FCommandList.Clear;
  UnLockObject(Queue.FCommandList);
end;

procedure TDrawExecute.PickQueue(Thread: TCoreClassThread; Queue: TDrawQueue);
begin
  FSourQueue := Queue;
  Thread.Synchronize(Thread, {$IFDEF FPC}@{$ENDIF FPC}Sync_PickQueue);
  FSourQueue := nil;
end;

procedure TDrawExecute.Sync_PickQueue;
begin
  PickQueue(FSourQueue);
end;

procedure TDrawExecute.Execute(Draw: TDrawEngineInterface);
var
  i: Integer;
  p: PDrawCommand;
begin
  Draw.ResetState;
  Draw.BeginDraw;
  for i := 0 to FCommandList.Count - 1 do
    begin
      try
          PDrawCommand(FCommandList[i])^.Execute(Self, Draw);
      except
      end;
    end;
  Draw.Flush;
  Draw.EndDraw;

  for i := 0 to FCommandList.Count - 1 do
    begin
      p := FCommandList[i];
      p^.DoFreeData;
      Dispose(p);
    end;
  FCommandList.Clear;
end;

constructor TDrawEngine_UIBase.Create(AOwner: TDrawEngine);
begin
  inherited Create;
  DataObject := nil;
  DataPointer := nil;
  DataVariant := Null;
  Owner := AOwner;
  OnClick := nil;
  Visibled := True;

  Owner.FUIList.Add(Self);
end;

destructor TDrawEngine_UIBase.Destroy;
var
  i: Integer;
begin
  i := 0;
  while Owner.FUIList.Count > i do
    begin
      if Owner.FUIList[i] = Self then
          Owner.FUIList.delete(i)
      else
          inc(i);
    end;

  inherited Destroy;
end;

function TDrawEngine_UIBase.TapDown(x, y: TDEFloat): Boolean;
begin
  Result := False;
end;

function TDrawEngine_UIBase.TapMove(x, y: TDEFloat): Boolean;
begin
  Result := False;
end;

function TDrawEngine_UIBase.TapUp(x, y: TDEFloat): Boolean;
begin
  Result := False;
end;

procedure TDrawEngine_UIBase.DoClick;
begin
  if Assigned(OnClick) then
      OnClick(Self);
end;

procedure TDrawEngine_UIBase.DoDraw;
begin
end;

constructor TDrawEngine_RectButton.Create(AOwner: TDrawEngine);
begin
  inherited Create(AOwner);
  Downed := False;
  DownPT := NULLPoint;
  MovePT := NULLPoint;
  UpPT := NULLPoint;
  Button := NULLRect;
  TextSize := 9;
end;

destructor TDrawEngine_RectButton.Destroy;
begin
  inherited Destroy;
end;

function TDrawEngine_RectButton.TapDown(x, y: TDEFloat): Boolean;
begin
  if PointInRect(DEVec(x, y), Button) then
    begin
      Downed := True;
      DownPT := DEVec(x, y);
      MovePT := DownPT;
      UpPT := DownPT;
      Result := True;
    end
  else
    begin
      Result := inherited TapDown(x, y);
      Downed := False;
      DownPT := NULLPoint;
      MovePT := NULLPoint;
      UpPT := NULLPoint;
    end;
end;

function TDrawEngine_RectButton.TapMove(x, y: TDEFloat): Boolean;
begin
  if Downed then
    begin
      MovePT := DEVec(x, y);
      UpPT := MovePT;
      Result := True;
    end
  else
    begin
      Result := inherited TapMove(x, y);
    end;
end;

function TDrawEngine_RectButton.TapUp(x, y: TDEFloat): Boolean;
begin
  if Downed then
    begin
      UpPT := DEVec(x, y);
      DoClick;
      Downed := False;
      Result := True;
    end
  else
    begin
      Result := inherited TapUp(x, y);
    end;
end;

procedure TDrawEngine_RectButton.DoDraw;
var
  r: TDERect;
  C: TDEColor;
begin
  inherited DoDraw;
  if Downed then
    begin
      r := Button;
      r[0] := Vec2Add(r[0], DEVec(2, 2));
      r[1] := Vec2Add(r[1], DEVec(2, 2));
    end
  else
      r := Button;

  Owner.FDrawCommand.SetLineWidth(1);
  C := DEColor(0, 0, 0, 0.0);
  Owner.FDrawCommand.FillRect(r, 0, C);
  C := DEColor(1, 1, 1, 1);
  Owner.FDrawCommand.DrawRect(r, 0, C);
  Owner.FDrawCommand.DrawText(Text, TextSize, r, C, True, DEVec(0.5, 0.5), 0);
end;

procedure TSequenceAnimationBase.Progress(deltaTime: Double);
begin
  CurrentTime := CurrentTime + deltaTime;

  if PlayMode = sapmLoop then
    begin
      while (CurrentTime > CompleteTime) do
          CurrentTime := CurrentTime - CompleteTime;
    end;
end;

constructor TSequenceAnimationBase.Create(AOwner: TDrawEngine);
begin
  inherited Create;
  Effect := nil;
  Owner := AOwner;
  Source := nil;
  width := 0;
  height := 0;
  Total := 0;
  Column := 0;
  CompleteTime := 0;
  PlayMode := sapmPlayOne;
  OverAnimationSmoothTime := 0.5;
  flag := Null;
  CurrentTime := 0;
  LastUsed := False;
end;

destructor TSequenceAnimationBase.Destroy;
begin
  if Effect <> nil then
      Effect.SequenceAnimation := nil;
  inherited Destroy;
end;

function TSequenceAnimationBase.SequenceAnimationPlaying: Boolean;
begin
  Result := CurrentTime <= CompleteTime;
end;

function TSequenceAnimationBase.GetOverAnimationSmoothAlpha(Alpha: TDEFloat): TDEFloat;
var
  v: TDEFloat;
begin
  if SequenceAnimationPlaying then
      Result := Alpha
  else
    begin
      v := CurrentTime - CompleteTime;
      if (v > OverAnimationSmoothTime) then
          Result := 0.0
      else if v > 0 then
        begin
          Result := Alpha - (1.0 / (OverAnimationSmoothTime / v));
          if Result < 0 then
              Result := 0.0;
        end
      else
          Result := Alpha;
    end;
end;

function TSequenceAnimationBase.IsOver: Boolean;
begin
  Result := (not SequenceAnimationPlaying) and (CurrentTime > CompleteTime + OverAnimationSmoothTime);
end;

function TSequenceAnimationBase.SequenceIndex: Integer;
begin
  if CurrentTime <= CompleteTime then
      Result := Round((CurrentTime / CompleteTime) * (Total - 1))
  else
      Result := Total - 1;
end;

function TSequenceAnimationBase.SequenceFrameRect: TDE4V;
var
  idx: Integer;
  rowIdx, colIdx: Integer;
  Row: Integer;
  Width_, Height_: Integer;
begin
  if Total <= 1 then
      exit(TDE4V.Init(width, height, 0));

  if Column > Total then
      Column := Total;

  idx := SequenceIndex;
  colIdx := idx mod Column;
  rowIdx := idx div Column;
  Row := Total div Column;
  if Total mod Column > 0 then
      inc(Row);

  Width_ := width div Column;
  Height_ := height div Row;

  Result := TDE4V.Init(Rect(colIdx * Width_, rowIdx * Height_, colIdx * Width_ + Width_, rowIdx * Height_ + Height_), 0);
end;

procedure TSequenceAnimationBase.LoadFromStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
begin
  df := TDataFrameEngine.Create;
  df.LoadFromStream(stream);

  Source := Owner.GetTexture(df.Reader.ReadString);
  width := df.Reader.ReadInteger;
  height := df.Reader.ReadInteger;
  Total := df.Reader.ReadInteger;
  Column := df.Reader.ReadInteger;
  CompleteTime := df.Reader.ReadDouble;
  PlayMode := TSequenceAnimationPlayMode(df.Reader.ReadInteger);

  flag := Owner.GetNewSequenceFlag;

  CurrentTime := 0;
  LastUsed := True;

  disposeObject(df);
end;

procedure TSequenceAnimationBase.SaveToStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
begin
  df := TDataFrameEngine.Create;

  df.WriteString(Owner.GetTextureName(Source));
  df.WriteInteger(width);
  df.WriteInteger(height);
  df.WriteInteger(Total);
  df.WriteInteger(Column);
  df.WriteDouble(CompleteTime);
  df.WriteInteger(Integer(PlayMode));

  df.SaveToStream(stream);
  disposeObject(df);
end;

procedure TParticles.Progress(deltaTime: Double);
var
  p: PParticleData;
  i: Integer;
  k: TDEFloat;
begin
  // gen particle
  if Enabled then
    begin
      PrepareParticleCount := PrepareParticleCount + (deltaTime * GenSpeedOfPerSecond);
      while PrepareParticleCount >= 1 do
        begin
          PrepareParticleCount := PrepareParticleCount - 1;

          new(p);
          p^.Source := Owner.GetOrCreateSequenceAnimation(Owner.GetNewSequenceFlag, SequenceTexture);
          p^.Source.CompleteTime := umlRandomRangeD(SequenceTextureCompleteTime * 0.9, SequenceTextureCompleteTime * 1.1);
          p^.Source.PlayMode := TSequenceAnimationPlayMode.sapmLoop;
          p^.Source.LastUsed := True;

          p^.Position := DEVec(
            umlRandomRangeS(GenerateRange[0][0], GenerateRange[1][0]) + LastDrawPosition[0],
            umlRandomRangeS(GenerateRange[0][1], GenerateRange[1][1]) + LastDrawPosition[1]);

          p^.radius := ParticleSize * umlRandomRangeS(ParticleSizeMinScale, ParticleSizeMaxScale);
          p^.Angle := 0;
          p^.Alpha := MaxAlpha;
          p^.CurrentTime := 0;
          p^.Acceleration := 0;

          ParticleBuff.Add(p);
        end;
    end;

  while ParticleBuff.Count > MaxParticle do
    begin
      p := ParticleBuff[ParticleBuff.Count - 1];
      Dispose(p);
      ParticleBuff.delete(ParticleBuff.Count - 1);
    end;

  // particle life
  i := 0;
  while i < ParticleBuff.Count do
    begin
      p := ParticleBuff[i];
      p^.CurrentTime := p^.CurrentTime + deltaTime;
      if p^.CurrentTime > LifeTime then
        begin
          Dispose(p);
          ParticleBuff.delete(i);
        end
      else
        begin
          p^.Acceleration := p^.Acceleration + DispersionAcceleration * deltaTime;
          k := Vec2Distance(ZeroPoint, Dispersion);
          p^.Position := MovementDistance(p^.Position, Vec2Add(p^.Position, Dispersion), (k + k * p^.Acceleration) * deltaTime);
          p^.Alpha := Clamp(MaxAlpha - (p^.CurrentTime / LifeTime) * MaxAlpha, MinAlpha, 1.0);
          p^.Angle := p^.Angle + deltaTime * RotationOfSecond;
          inc(i);
        end;
    end;
end;

constructor TParticles.Create(AOwner: TDrawEngine);
begin
  inherited Create;
  Effect := nil;
  Owner := AOwner;
  ParticleBuff := TCoreClassList.Create;
  PrepareParticleCount := 0;
  NoEnabledAutoFree := False;
  LastDrawPosition := ZeroPoint;

  // temp define
  SequenceTexture := Owner.DefaultTexture;

  SequenceTextureCompleteTime := 1.0;
  MinAlpha := 0.0;
  MaxAlpha := 1.0;
  MaxParticle := 100;
  ParticleSize := 10;
  ParticleSizeMinScale := 0.4;
  ParticleSizeMaxScale := 0.6;
  GenerateRange := DERect(0, 0, 0, 0);
  Dispersion := DEVec(0, 0);
  DispersionAcceleration := 0;
  RotationOfSecond := 0;
  GenSpeedOfPerSecond := 50;
  LifeTime := 2.0;
  Enabled := True;
  Visible := True;
end;

destructor TParticles.Destroy;
var
  i: Integer;
begin
  if Effect <> nil then
      Effect.Particle := nil;
  if Owner <> nil then
      Owner.DeleteParticles(Self);

  for i := 0 to ParticleBuff.Count - 1 do
      Dispose(PParticleData(ParticleBuff[i]));
  disposeObject(ParticleBuff);

  inherited Destroy;
end;

function TParticles.VisibledParticle: Integer;
begin
  Result := ParticleBuff.Count;
end;

procedure TParticles.FinishAndDelayFree;
begin
  NoEnabledAutoFree := True;
  Enabled := False;
end;

procedure TParticles.LoadFromStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
begin
  df := TDataFrameEngine.Create;
  df.LoadFromStream(stream);

  SequenceTexture := Owner.GetTexture(df.Reader.ReadString);
  SequenceTextureCompleteTime := df.Reader.ReadDouble;
  MaxParticle := df.Reader.ReadInteger;
  ParticleSize := df.Reader.ReadSingle;
  ParticleSizeMinScale := df.Reader.ReadSingle;
  ParticleSizeMaxScale := df.Reader.ReadSingle;
  MinAlpha := df.Reader.ReadSingle;
  MaxAlpha := df.Reader.ReadSingle;
  with df.Reader.ReadArraySingle do
      GenerateRange := DERect(buffer[0], buffer[1], buffer[2], buffer[3]);
  with df.Reader.ReadArraySingle do
      Dispersion := DEVec(buffer[0], buffer[1]);
  DispersionAcceleration := df.Reader.ReadSingle;
  RotationOfSecond := df.Reader.ReadSingle;
  GenSpeedOfPerSecond := df.Reader.ReadInteger;
  LifeTime := df.Reader.ReadDouble;
  Enabled := df.Reader.ReadBool;
  Visible := df.Reader.ReadBool;

  disposeObject(df);
end;

procedure TParticles.SaveToStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
begin
  df := TDataFrameEngine.Create;

  df.WriteString(Owner.GetTextureName(SequenceTexture));
  df.WriteDouble(SequenceTextureCompleteTime);
  df.WriteInteger(MaxParticle);
  df.WriteSingle(ParticleSize);
  df.WriteSingle(ParticleSizeMinScale);
  df.WriteSingle(ParticleSizeMaxScale);
  df.WriteSingle(MinAlpha);
  df.WriteSingle(MaxAlpha);
  with df.WriteArraySingle do
    begin
      Add(GenerateRange[0][0]);
      Add(GenerateRange[0][1]);
      Add(GenerateRange[1][0]);
      Add(GenerateRange[1][1]);
    end;
  with df.WriteArraySingle do
    begin
      Add(Dispersion[0]);
      Add(Dispersion[0]);
    end;
  df.WriteSingle(DispersionAcceleration);
  df.WriteSingle(RotationOfSecond);
  df.WriteInteger(GenSpeedOfPerSecond);
  df.WriteDouble(LifeTime);
  df.WriteBool(Enabled);
  df.WriteBool(Visible);

  df.SaveToStream(stream);
  disposeObject(df);
end;

constructor TEffect.Create(AOwner: TDrawEngine);
begin
  inherited Create;
  Owner := AOwner;
  Mode := emNo;
  Particle := nil;
  SequenceAnimation := nil;
  SequenceAnimation_Width := 0;
  SequenceAnimation_Height := 0;
  SequenceAnimation_Angle := 0;
  SequenceAnimation_Alpha := 1.0;
end;

destructor TEffect.Destroy;
begin
  Reset;
  inherited;
end;

procedure TEffect.Reset;
begin
  if Particle <> nil then
      disposeObject(Particle);
  if SequenceAnimation <> nil then
      disposeObject(SequenceAnimation);
end;

procedure TEffect.LoadFromStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
  ms: TMemoryStream64;
begin
  Reset;
  df := TDataFrameEngine.Create;
  df.LoadFromStream(stream);

  Mode := TEffectMode(df.Reader.ReadInteger);
  ms := TMemoryStream64.Create;
  df.Reader.ReadStream(ms);
  ms.Position := 0;
  case Mode of
    emSequenceAnimation:
      begin
        SequenceAnimation := Owner.CreateSequenceAnimation(ms);
        SequenceAnimation.Effect := Self;
        SequenceAnimation_Width := df.Reader.ReadSingle;
        SequenceAnimation_Height := df.Reader.ReadSingle;
        SequenceAnimation_Angle := df.Reader.ReadSingle;
        SequenceAnimation_Alpha := df.Reader.ReadSingle;
      end;
    emParticle:
      begin
        Particle := Owner.CreateParticles;
        Particle.LoadFromStream(ms);
        Particle.Effect := Self;
        SequenceAnimation_Width := 0;
        SequenceAnimation_Height := 0;
        SequenceAnimation_Angle := 0;
        SequenceAnimation_Alpha := 1.0;
      end;
  end;
  disposeObject(ms);

  disposeObject(df);
end;

procedure TEffect.SaveToStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
  ms: TMemoryStream64;
begin
  Reset;
  df := TDataFrameEngine.Create;

  df.WriteInteger(Integer(Mode));
  ms := TMemoryStream64.Create;
  case Mode of
    emSequenceAnimation:
      begin
        SequenceAnimation.SaveToStream(ms);
        df.WriteStream(ms);
        df.WriteSingle(SequenceAnimation_Width);
        df.WriteSingle(SequenceAnimation_Height);
        df.WriteSingle(SequenceAnimation_Angle);
        df.WriteSingle(SequenceAnimation_Alpha);
      end;
    emParticle:
      begin
        Particle.SaveToStream(ms);
        df.WriteStream(ms);
      end;
  end;
  disposeObject(ms);

  df.SaveToStream(stream);

  disposeObject(df);
end;

procedure TEffect.Draw(Pos: TDEVec);
begin
  case Mode of
    emSequenceAnimation:
      begin
        if SequenceAnimation = nil then
            exit;
        Owner.DrawSequenceTexture(SequenceAnimation,
          TDE4V.Init(MakeRectV2(
          Pos[0] - SequenceAnimation_Width * 0.5, Pos[1] - SequenceAnimation_Height * 0.5,
          Pos[0] + SequenceAnimation_Width * 0.5, Pos[1] + SequenceAnimation_Height * 0.5), SequenceAnimation_Angle), SequenceAnimation_Alpha);
      end;
    emParticle:
      begin
        if Particle = nil then
            exit;
        Owner.DrawParticle(Particle, Pos);
      end;
  end;
end;

procedure TEffect.DrawInScene(Pos: TDEVec);
begin
  case Mode of
    emSequenceAnimation:
      begin
        if SequenceAnimation = nil then
            exit;
        Owner.DrawSequenceTextureInScene(SequenceAnimation,
          TDE4V.Init(MakeRectV2(
          Pos[0] - SequenceAnimation_Width * 0.5, Pos[1] - SequenceAnimation_Height * 0.5,
          Pos[0] + SequenceAnimation_Width * 0.5, Pos[1] + SequenceAnimation_Height * 0.5), SequenceAnimation_Angle), SequenceAnimation_Alpha);
      end;
    emParticle:
      begin
        if Particle = nil then
            exit;
        Owner.DrawParticleInScene(Particle, Pos);
      end;
  end;
end;

procedure TDrawEnginePool.CadencerProgress(const deltaTime, newTime: Double);
begin
  Progress(deltaTime);
end;

constructor TDrawEnginePool.Create;
begin
  inherited Create;
  FCritical := TCritical.Create;
  FDrawEngineClass := TDrawEngine;
  FDrawEngineList := TCoreClassList.Create;
  FPostProgress := TNProgressPost.Create;
  FCadEng := TCadencer.Create;
  FCadEng.ProgressInterface := Self;
  FLastDeltaTime := 0;
end;

destructor TDrawEnginePool.Destroy;
begin
  Clear;
  disposeObject(FDrawEngineList);
  disposeObject(FPostProgress);
  disposeObject(FCadEng);
  disposeObject(FCritical);
  inherited Destroy;
end;

procedure TDrawEnginePool.Clear;
var
  i: Integer;
  p: PDrawEnginePoolData;
begin
  FCritical.Lock;
  for i := 0 to FDrawEngineList.Count - 1 do
    begin
      p := FDrawEngineList[i];
      disposeObject(p^.DrawEng);
      Dispose(p);
    end;
  FDrawEngineList.Clear;
  FCritical.UnLock;
end;

procedure TDrawEnginePool.ClearActivtedTimeOut(tick: TTimeTick);
var
  i: Integer;
  p: PDrawEnginePoolData;
  tk: TTimeTick;
begin
  FCritical.Lock;
  tk := GetTimeTick;
  i := FDrawEngineList.Count - 1;
  while i >= 0 do
    begin
      p := FDrawEngineList[i];
      if tk - p^.LastActivted > tick then
        begin
          disposeObject(p^.DrawEng);
          Dispose(p);
          FDrawEngineList.delete(i);
        end;
      dec(i);
    end;
  FCritical.UnLock;
end;

procedure TDrawEnginePool.Progress(deltaTime: Double);
var
  i: Integer;
begin
  ClearActivtedTimeOut(60 * 1000);
  FCritical.Lock;
  FPostProgress.Progress(deltaTime);
  for i := 0 to FDrawEngineList.Count - 1 do
      PDrawEnginePoolData(FDrawEngineList[i])^.DrawEng.Progress(deltaTime);
  FLastDeltaTime := deltaTime;
  FCritical.UnLock;
end;

function TDrawEnginePool.Progress(): Double;
begin
  FCadEng.Progress();
  Result := FLastDeltaTime;
end;

function TDrawEnginePool.GetEng(const workObj: TCoreClassObject; const Draw: TDrawEngineInterface): TDrawEngine;
var
  i: Integer;
  p: PDrawEnginePoolData;
begin
  FCritical.Lock;
  for i := 0 to FDrawEngineList.Count - 1 do
    begin
      p := FDrawEngineList[i];
      if p^.workObj = workObj then
        begin
          p^.LastActivted := GetTimeTick();
          if i > 0 then
              FDrawEngineList.Move(i, 0);
          if p^.DrawEng.FDrawInterface <> Draw then
              p^.DrawEng.FDrawInterface := Draw;
          p^.DrawEng.SetSize;
          FCritical.UnLock;
          Result := p^.DrawEng;
          exit;
        end;
    end;
  new(p);
  p^.DrawEng := FDrawEngineClass.Create;
  p^.DrawEng.FDrawInterface := Draw;
  p^.DrawEng.ViewOptions := [];
  p^.workObj := workObj;
  p^.LastActivted := GetTimeTick;
  p^.DrawEng.SetSize;
  FDrawEngineList.Add(p);
  FCritical.UnLock;
  Result := p^.DrawEng;
end;

function TDrawEnginePool.GetEng(const workObj: TCoreClassObject): TDrawEngine;
var
  i: Integer;
  p: PDrawEnginePoolData;
begin
  FCritical.Lock;
  for i := 0 to FDrawEngineList.Count - 1 do
    begin
      p := FDrawEngineList[i];
      if p^.workObj = workObj then
        begin
          p^.LastActivted := GetTimeTick();
          if i > 0 then
              FDrawEngineList.Move(i, 0);
          FCritical.UnLock;
          Result := p^.DrawEng;
          exit;
        end;
    end;
  new(p);
  p^.DrawEng := FDrawEngineClass.Create;
  p^.DrawEng.ViewOptions := [];
  p^.workObj := workObj;
  p^.LastActivted := GetTimeTick;
  FDrawEngineList.Add(p);
  FCritical.UnLock;
  Result := p^.DrawEng;
end;

function TDrawEnginePool.EngNum: Integer;
begin
  Result := FDrawEngineList.Count;
end;

function TDrawEngine_Raster.DEColor2RasterColor(const COLOR: TDEColor): TRasterColor;
begin
  Result := RasterColorF(COLOR[0], COLOR[1], COLOR[2], COLOR[3]);
end;

function TDrawEngine_Raster.DEColor2RasterColor(const COLOR: TDEColor; const Alpha: Byte): TRasterColor;
begin
  Result := RasterColorF(COLOR[0], COLOR[1], COLOR[2], COLOR[3]);
end;

constructor TDrawEngine_Raster.Create;
begin
  inherited Create;
  FEngine := nil;
  FMemory := DefaultTextureClass.Create;
  FUsedAgg := True;
  FFreeEngine := False;
  FTextCoordinateList := TSoftRaster_TextCoordinateList.Create;
end;

destructor TDrawEngine_Raster.Destroy;
begin
  disposeObject(FMemory);
  if (FFreeEngine) and (FEngine <> nil) then
      disposeObject(FEngine);
  disposeObject(FTextCoordinateList);
  inherited Destroy;
end;

procedure TDrawEngine_Raster.SetSize(r: TDERect);
begin
  if not FMemory.IsMemoryMap then
      FMemory.SetSize(Round(RectWidth(r)), Round(RectHeight(r)), RasterColor(0, 0, 0, 0));
end;

procedure TDrawEngine_Raster.SetLineWidth(w: TDEFloat);
begin
  if not FUsedAgg then
      exit;

  FMemory.Agg.LineWidth := w
end;

procedure TDrawEngine_Raster.DrawDotLine(pt1, pt2: TDEVec; COLOR: TDEColor);
begin
  FMemory.LineF(pt1, pt2, DEColor2RasterColor(COLOR), True);
end;

procedure TDrawEngine_Raster.DrawLine(pt1, pt2: TDEVec; COLOR: TDEColor);
begin
  FMemory.LineF(pt1, pt2, DEColor2RasterColor(COLOR), True);
end;

procedure TDrawEngine_Raster.DrawRect(r: TDERect; Angle: TDEFloat; COLOR: TDEColor);
begin
  FMemory.DrawRect(r, Angle, DEColor2RasterColor(COLOR));
end;

procedure TDrawEngine_Raster.FillRect(r: TDERect; Angle: TDEFloat; COLOR: TDEColor);
begin
  FMemory.FillRect(r, Angle, DEColor2RasterColor(COLOR));
end;

procedure TDrawEngine_Raster.DrawEllipse(r: TDERect; COLOR: TDEColor);
var
  C: TDEVec;
begin
  C := RectCentre(r);
  FMemory.DrawEllipse(C, RectWidth(r) * 0.5, RectHeight(r) * 0.5, DEColor2RasterColor(COLOR));
end;

procedure TDrawEngine_Raster.FillEllipse(r: TDERect; COLOR: TDEColor);
var
  C: TDEVec;
begin
  C := RectCentre(r);
  FMemory.FillEllipse(C, RectWidth(r) * 0.5, RectHeight(r) * 0.5, DEColor2RasterColor(COLOR));
end;

procedure TDrawEngine_Raster.FillPolygon(PolygonBuff: TArrayVec2; COLOR: TDEColor);
begin
  FMemory.FillPolygon(PolygonBuff, DEColor2RasterColor(COLOR));
end;

procedure TDrawEngine_Raster.DrawText(Shadow: Boolean; Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean; RotateVec: TDEVec; Angle: TDEFloat);
var
  vSiz: TDEVec;
  x, y: TDEFloat;
  DrawCoordinate, BoundBoxCoordinate: TArrayV2R4;
  i: Integer;
  info: TSoftRaster_TextCoordinate;
begin
  vSiz := FMemory.TextSize(Text, Size);
  if center then
    begin
      x := r[0, 0] + (RectWidth(r) - vSiz[0]) * 0.5;
      y := r[0, 1] + (RectHeight(r) - vSiz[1]) * 0.5;
    end
  else
    begin
      x := r[0, 0];
      y := r[0, 1];
    end;
  FMemory.DrawText(Text, Round(x), Round(y), RotateVec, Angle, 1.0, Size, DEColor2RasterColor(COLOR));

  if not Shadow then
    begin
      // compute text coordinates
      FMemory.ComputeDrawTextCoordinate(Text, Round(x), Round(y), RotateVec, Angle, Size, DrawCoordinate, BoundBoxCoordinate);
      for i := 0 to length(DrawCoordinate) - 1 do
        begin
          info.C := Text[FirstCharPos + i];
          info.DrawBox := DrawCoordinate[i];
          info.BoundBox := BoundBoxCoordinate[i];
          FTextCoordinateList.Add(info);
        end;
      SetLength(DrawCoordinate, 0);
      SetLength(BoundBoxCoordinate, 0);
    end;
end;

procedure TDrawEngine_Raster.DrawPicture(Shadow: Boolean; t: TCoreClassObject; Sour, Dest: TDE4V; Alpha: TDEFloat);
begin
  if t is TDETexture then
      TDETexture(t).DrawUsage;

  if t is TMemoryRaster then
      TMemoryRaster(t).ProjectionTo(FMemory, TV2Rect4.Init(Sour.MakeRectV2, Sour.Angle), TV2Rect4.Init(Dest.MakeRectV2, Dest.Angle), True, Alpha)
  else
      DoStatus('texture error! ' + t.ClassName);
end;

procedure TDrawEngine_Raster.Flush;
begin
end;

procedure TDrawEngine_Raster.ResetState;
begin
end;

procedure TDrawEngine_Raster.BeginDraw;
begin
  if FUsedAgg then
      FMemory.OpenAgg
  else
      FMemory.CloseAgg;
  FTextCoordinateList.Clear;
end;

procedure TDrawEngine_Raster.EndDraw;
begin
  if not FUsedAgg then
      FMemory.CloseAgg;
end;

function TDrawEngine_Raster.CurrentScreenSize: TDEVec;
begin
  Result := FMemory.SizeOf2DPoint;
end;

function TDrawEngine_Raster.GetTextSize(const Text: SystemString; Size: TDEFloat): TDEVec;
begin
  Result := FMemory.TextSize(Text, Size);
end;

function TDrawEngine_Raster.ReadyOK: Boolean;
begin
  Result := True;
end;

function TDrawEngine_Raster.Engine: TDrawEngine;
begin
  if FEngine = nil then
    begin
      FEngine := TDrawEngineClass.Create;
      FFreeEngine := True;
    end;
  FEngine.FDrawInterface := Self;
  Result := FEngine;
end;

function TDrawTextExpressionRunTime.cc(v: Variant): TDEFloat;
begin
  if v > 1 then
      Result := v / $FF
  else
      Result := v;
end;

function TDrawTextExpressionRunTime.oprt_size(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
begin
  Result := 0;
  if length(Param) > 0 then
      TDrawTextExpressionRunTime(OpRunTime).Size^ := Param[0];
end;

function TDrawTextExpressionRunTime.oprt_rgba(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Min(4, length(Param)) - 1 do
      COLOR^[i] := cc(Param[i]);
end;

function TDrawTextExpressionRunTime.oprt_bgra(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Min(4, length(Param)) - 1 do
      COLOR^[i] := cc(Param[i]);
  Swap(COLOR^[0], COLOR^[2]);
end;

function TDrawTextExpressionRunTime.oprt_red(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
begin
  Result := 0;
  if length(Param) > 0 then
      COLOR^[0] := cc(Param[0]);
end;

function TDrawTextExpressionRunTime.oprt_green(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
begin
  Result := 0;
  if length(Param) > 0 then
      COLOR^[1] := cc(Param[0]);
end;

function TDrawTextExpressionRunTime.oprt_blue(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
begin
  Result := 0;
  if length(Param) > 0 then
      COLOR^[2] := cc(Param[0]);
end;

function TDrawTextExpressionRunTime.oprt_alpha(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
begin
  Result := 0;
  if length(Param) > 0 then
      COLOR^[3] := cc(Param[0]);
end;

function TDrawTextExpressionRunTime.oprt_r(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
begin
  Result := COLOR^[0];
end;

function TDrawTextExpressionRunTime.oprt_g(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
begin
  Result := COLOR^[1];
end;

function TDrawTextExpressionRunTime.oprt_b(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
begin
  Result := COLOR^[2];
end;

function TDrawTextExpressionRunTime.oprt_a(OpRunTime: TOpCustomRunTime; var Param: TOpParam): Variant;
begin
  Result := COLOR^[3];
end;

constructor TDrawTextExpressionRunTime.CustomCreate(maxHashLen: Integer);
begin
  inherited CustomCreate(maxHashLen);
  Size := nil;
  COLOR := nil;
end;

destructor TDrawTextExpressionRunTime.Destroy;
begin
  inherited Destroy;
end;

procedure TDrawTextExpressionRunTime.InternalReg;
begin
  inherited InternalReg;
  RegObjectOpM('size', {$IFDEF FPC}@{$ENDIF FPC}oprt_size);
  RegObjectOpM('s', {$IFDEF FPC}@{$ENDIF FPC}oprt_size);
  RegObjectOpM('siz', {$IFDEF FPC}@{$ENDIF FPC}oprt_size);

  RegObjectOpM('c', {$IFDEF FPC}@{$ENDIF FPC}oprt_rgba);
  RegObjectOpM('cr', {$IFDEF FPC}@{$ENDIF FPC}oprt_rgba);
  RegObjectOpM('color', {$IFDEF FPC}@{$ENDIF FPC}oprt_rgba);
  RegObjectOpM('setcolor', {$IFDEF FPC}@{$ENDIF FPC}oprt_rgba);
  RegObjectOpM('rgb', {$IFDEF FPC}@{$ENDIF FPC}oprt_rgba);
  RegObjectOpM('rgba', {$IFDEF FPC}@{$ENDIF FPC}oprt_rgba);

  RegObjectOpM('bgr', {$IFDEF FPC}@{$ENDIF FPC}oprt_bgra);
  RegObjectOpM('bgra', {$IFDEF FPC}@{$ENDIF FPC}oprt_bgra);

  RegObjectOpM('red', {$IFDEF FPC}@{$ENDIF FPC}oprt_red);
  RegObjectOpM('green', {$IFDEF FPC}@{$ENDIF FPC}oprt_green);
  RegObjectOpM('blue', {$IFDEF FPC}@{$ENDIF FPC}oprt_blue);
  RegObjectOpM('alpha', {$IFDEF FPC}@{$ENDIF FPC}oprt_alpha);

  RegObjectOpM('r', {$IFDEF FPC}@{$ENDIF FPC}oprt_r);
  RegObjectOpM('g', {$IFDEF FPC}@{$ENDIF FPC}oprt_g);
  RegObjectOpM('b', {$IFDEF FPC}@{$ENDIF FPC}oprt_b);
  RegObjectOpM('a', {$IFDEF FPC}@{$ENDIF FPC}oprt_a);
end;

procedure TDrawEngine_Raster.SetWorkMemory(m: TMemoryRaster);
begin
  Memory.SetWorkMemory(m);
  Engine.SetSize(m);
end;

constructor TDeflectionPolygonListRenderer.Create;
begin
  inherited Create;
  RendererConfigure := THashTextEngine.Create;
end;

destructor TDeflectionPolygonListRenderer.Destroy;
begin
  disposeObject(RendererConfigure);
  inherited Destroy;
end;

procedure TDeflectionPolygonListRenderer.RebuildConfigure;
var
  i: Integer;
  Poly: TDeflectionPolygon;
begin
  RendererConfigure.Clear;
  for i := 0 to Count - 1 do
    begin
      Poly := Items[i];
      if (Poly.Name <> '') and (not RendererConfigure.Exists(Poly.Name)) then
        begin
          RendererConfigure.HitString[Poly.Name, 'LineStyle'] := 'Polygon';
          RendererConfigure.HitString[Poly.Name, 'LineWidth'] := '1';
          RendererConfigure.HitString[Poly.Name, 'LineColor'] := '1,1,1';
          RendererConfigure.HitString[Poly.Name, 'LineAlpha'] := '1.0';
          RendererConfigure.HitString[Poly.Name, 'LineVisible'] := 'True';

          RendererConfigure.HitString[Poly.Name, 'FillStyle'] := 'Polygon';
          RendererConfigure.HitString[Poly.Name, 'FillColor'] := '0.5,0.5,0.5';
          RendererConfigure.HitString[Poly.Name, 'FillAlpha'] := '0.5';
          RendererConfigure.HitString[Poly.Name, 'FillVisible'] := 'True';

          RendererConfigure.HitString[Poly.Name, 'Text'] := Poly.Name;
          RendererConfigure.HitString[Poly.Name, 'TextColor'] := '1,1,1';
          RendererConfigure.HitString[Poly.Name, 'TextAlpha'] := '1.0';
          RendererConfigure.HitString[Poly.Name, 'TextSize'] := '12';
          RendererConfigure.HitString[Poly.Name, 'TextVisible'] := 'True';
        end;
    end;
end;

procedure TDeflectionPolygonListRenderer.BuildRendererConfigure(FileName: SystemString);
begin
  RebuildConfigure;
  RendererConfigure.SaveToFile(FileName);
end;

procedure TDeflectionPolygonListRenderer.BuildRendererConfigure(stream: TCoreClassStream);
begin
  RebuildConfigure;
  RendererConfigure.SaveToStream(stream);
end;

procedure TDeflectionPolygonListRenderer.LoadRendererConfigure(stream: TCoreClassStream);
begin
  RendererConfigure.Clear;
  RendererConfigure.LoadFromStream(stream);
end;

procedure TDeflectionPolygonListRenderer.LoadRendererConfigure(FileName: SystemString);
begin
  RendererConfigure.Clear;
  RendererConfigure.LoadFromFile(FileName);
end;

procedure TDeflectionPolygonListRenderer.Render(d: TDrawEngine; Dest: TRectV2; InScene: Boolean);
type
  TRenderStyle = (rsInsideSpline, rsOutsideSpline, rsPolygon);

  function RS(s: TPascalString): TRenderStyle;
  begin
    if s.Same('InsideSpline', 'Inside') then
        Result := rsInsideSpline
    else if s.Same('OutsideSpline', 'Outside') then
        Result := rsOutsideSpline
    else
        Result := rsPolygon
  end;

var
  nl: TListPascalString;
  i: Integer;
  Poly: TDeflectionPolygon;
  s: TRenderStyle;
  buff: TArrayVec2;
  color3: TVec3;
  Alpha: TGeoFloat;
  width: TGeoFloat;
  Text: SystemString;
  TextSize: TGeoFloat;
  lb, le, sizV2: TVec2;
begin
  nl := TListPascalString.Create;
  RendererConfigure.GetSectionList(nl);

  for i := 0 to nl.Count - 1 do
    begin
      Poly := FindPolygon(nl[i]);
      if Poly <> nil then
        begin
          if umlStrToBool(RendererConfigure.GetDefaultText(Poly.Name, 'FillVisible', 'False')) then
            begin
              s := RS(RendererConfigure.GetDefaultText(Poly.Name, 'FillStyle', 'Polygon'));
              case s of
                rsInsideSpline: buff := Poly.BuildProjectionSplineSmoothInSideClosedArray(BackgroundBox, Dest);
                rsOutsideSpline: buff := Poly.BuildProjectionSplineSmoothOutSideClosedArray(BackgroundBox, Dest);
                else buff := Poly.BuildProjectionArray(BackgroundBox, Dest);
              end;
              color3 := StrToVec3(RendererConfigure.GetDefaultText(Poly.Name, 'FillColor', '0.5,0.5,0.5'));
              Alpha := umlStrToFloat(RendererConfigure.GetDefaultText(Poly.Name, 'FillAlpha', '0.5'), 0.5);

              if InScene then
                  d.FillPolygonInScene(buff, DEColor(color3, Alpha))
              else
                  d.FillPolygon(buff, DEColor(color3, Alpha));
              SetLength(buff, 0);
            end;

          if umlStrToBool(RendererConfigure.GetDefaultText(Poly.Name, 'LineVisible', 'False')) then
            begin
              s := RS(RendererConfigure.GetDefaultText(Poly.Name, 'LineStyle', 'Polygon'));
              case s of
                rsInsideSpline: buff := Poly.BuildProjectionSplineSmoothInSideClosedArray(BackgroundBox, Dest);
                rsOutsideSpline: buff := Poly.BuildProjectionSplineSmoothOutSideClosedArray(BackgroundBox, Dest);
                else buff := Poly.BuildProjectionArray(BackgroundBox, Dest);
              end;
              color3 := StrToVec3(RendererConfigure.GetDefaultText(Poly.Name, 'LineColor', '1,1,1'));
              Alpha := umlStrToFloat(RendererConfigure.GetDefaultText(Poly.Name, 'LineAlpha', '1.0'), 1.0);
              width := umlStrToFloat(RendererConfigure.GetDefaultText(Poly.Name, 'LineWidth', '1.0'), 1.0);

              if InScene then
                  d.DrawPolygonInScene(buff, DEColor(color3, Alpha), width)
              else
                  d.DrawPolygon(buff, DEColor(color3, Alpha), width);

              SetLength(buff, 0);
            end;

          if umlStrToBool(RendererConfigure.GetDefaultText(Poly.Name, 'TextVisible', 'False')) then
            begin
              Text := RendererConfigure.GetDefaultText(Poly.Name, 'Text', Poly.Name);
              color3 := StrToVec3(RendererConfigure.GetDefaultText(Poly.Name, 'TextColor', '1,1,1'));
              Alpha := umlStrToFloat(RendererConfigure.GetDefaultText(Poly.Name, 'TextAlpha', '1'), 1);
              TextSize := umlStrToFloat(RendererConfigure.GetDefaultText(Poly.Name, 'TextSize', '12'), 12);

              sizV2 := d.GetTextSize(Text, TextSize);

              le := RectProjection(BackgroundBox, Dest, Poly.Position);
              lb := RectProjection(BackgroundBox, Dest, PointRotation(Poly.Position, sizV2[0], Poly.Angle));

              d.BeginCaptureShadow(Vec2(1, 1), 0.9);
              if InScene then
                  d.DrawTextInScene(lb, le, Text, TextSize, DEColor(color3, Alpha))
              else
                  d.DrawText(lb, le, Text, TextSize, DEColor(color3, Alpha));
              d.EndCaptureShadow;
            end;
        end;
    end;
  disposeObject(nl);
end;

procedure TDrawEngine.SetDrawInterface(const Value: TDrawEngineInterface);
begin
  if Value = nil then
      FDrawInterface := FRasterization
  else
      FDrawInterface := Value;
end;

procedure TDrawEngine.DoFlush;
begin
end;

function TDrawEngine.DoTapDown(x, y: TDEFloat): Boolean;
begin
  Result := False;
end;

function TDrawEngine.DoTapMove(x, y: TDEFloat): Boolean;
begin
  Result := False;
end;

function TDrawEngine.DoTapUp(x, y: TDEFloat): Boolean;
begin
  Result := False;
end;

procedure TDrawEngine.TextSizeCacheDoDataFree(p: Pointer);
begin
  Dispose(PDEVec(p));
end;

function TDrawEngine.GetUserVariants: THashVariantList;
begin
  if FUserVariants = nil then
      FUserVariants := THashVariantList.Create;

  Result := FUserVariants;
end;

function TDrawEngine.GetUserObjects: THashObjectList;
begin
  if FUserObjects = nil then
      FUserObjects := THashObjectList.Create(False);

  Result := FUserObjects;
end;

function TDrawEngine.GetUserAutoFreeObjects: THashObjectList;
begin
  if FUserAutoFreeObjects = nil then
      FUserAutoFreeObjects := THashObjectList.Create(True);

  Result := FUserAutoFreeObjects;
end;

procedure TDrawEngine.CadencerProgress(const deltaTime, newTime: Double);
begin
  Progress(deltaTime);
end;

constructor TDrawEngine.Create;
begin
  inherited Create;

  FRasterization := TDrawEngine_Raster.Create;
  FRasterization.FEngine := Self;
  FRasterization.FFreeEngine := False;
  FDrawInterface := FRasterization;

  FDrawCommand := TDrawQueue.Create(Self);
  FDrawExecute := TDrawExecute.Create(Self);

  Scale := 1.0;
  Offset := DEVec(0, 0);

  FCommandCounter := 0;
  FPerformaceCounter := 0;
  FLastPerformaceTime := GetTimeTick;
  FFrameCounterOfPerSec := 0;
  FCommandCounterOfPerSec := 0;
  FWidth := 0;
  FHeight := 0;
  FLastDeltaTime := 0;
  FLastNewTime := 0;
  FViewOptions := [voFPS, voEdge];
  FLastDrawInfo := '';

  FTextSizeCache := THashList.Create;
  FTextSizeCache.SetHashBlockCount(2000);
  FTextSizeCache.AccessOptimization := True;
  FTextSizeCache.AutoFreeData := True;
  FTextSizeCache.IgnoreCase := False;
  FTextSizeCache.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}TextSizeCacheDoDataFree;
  FMaxScrollText := 500;
  FScrollTextList := TCoreClassListForObj.Create;

  FDownPT := NULLPoint;
  FMovePT := NULLPoint;
  FUpPT := NULLPoint;
  FLastAcceptDownUI := nil;
  FUIList := TCoreClassListForObj.Create;

  FSequenceAnimationBuffer := TCoreClassListForObj.Create;
  FParticleBuffer := TCoreClassListForObj.Create;
  FLastDynamicSeqenceFlag := 0;

  FCadencerEng := TCadencer.Create;
  FCadencerEng.ProgressInterface := Self;
  FPostProgress := TNProgressPost.Create;

  FFPSFontSize := 12;
  FFPSFontColor := DEColor(1, 1, 1, 1);
  FScreenFrameColor := DEColor(0.5, 0.2, 0.2, 0.5);
  FScreenFrameSize := 1;

  FTextureLibrary := THashObjectList.Create(True);
  FOnGetTexture := nil;

  FDefaultTexture := DefaultTextureClass.Create;
  FDefaultTexture.SetSize(2, 2, RasterColorF(0, 0, 0, 1));

  SetLength(FPictureFlushInfo, 0);

  FTextureOutputStateBox := DERect(0, 0, 100, 100);

  FUserData := nil;
  FUserValue := Null;
  FUserVariants := nil;
  FUserObjects := nil;
  FUserAutoFreeObjects := nil;
end;

destructor TDrawEngine.Destroy;
var
  i: Integer;
begin
  ClearScrollText;
  ClearUI;

  disposeObject(FDrawCommand);
  disposeObject(FDrawExecute);
  disposeObject(FScrollTextList);
  disposeObject(FUIList);
  disposeObject(FTextSizeCache);

  for i := 0 to FSequenceAnimationBuffer.Count - 1 do
      disposeObject(FSequenceAnimationBuffer[i]);
  disposeObject(FSequenceAnimationBuffer);

  ClearParticles;
  disposeObject(FParticleBuffer);
  disposeObject(FTextureLibrary);
  disposeObject(FDefaultTexture);

  disposeObject(FCadencerEng);
  disposeObject(FPostProgress);

  disposeObject(FRasterization);

  if FUserVariants <> nil then
      disposeObject(FUserVariants);
  if FUserObjects <> nil then
      disposeObject(FUserObjects);
  if FUserAutoFreeObjects <> nil then
      disposeObject(FUserAutoFreeObjects);
  inherited Destroy;
end;

function TDrawEngine.SceneToScreen(pt: TDEVec): TDEVec;
begin
  Result[0] := Offset[0] + (pt[0] * Scale);
  Result[1] := Offset[1] + (pt[1] * Scale);
end;

function TDrawEngine.SceneToScreen(x, y: TDEFloat): TDEVec;
begin
  Result := SceneToScreen(DEVec(x, y));
end;

function TDrawEngine.SceneToScreen(r: TDE4V): TDE4V;
begin
  Result := TDE4V.Init(SceneToScreen(r.MakeRectV2), r.Angle);
end;

function TDrawEngine.SceneToScreen(r: TDERect): TDERect;
begin
  Result[0] := SceneToScreen(r[0]);
  Result[1] := SceneToScreen(r[1]);
end;

function TDrawEngine.SceneToScreen(r: TRectf): TRectf;
begin
  Result := MakeRectf(SceneToScreen(RectV2(r)));
end;

function TDrawEngine.SceneToScreen(r: TRect): TRect;
begin
  Result := MakeRect(SceneToScreen(RectV2(r)));
end;

function TDrawEngine.SceneToScreen(r: TV2Rect4): TV2Rect4;
begin
  Result.LeftTop := SceneToScreen(r.LeftTop);
  Result.RightTop := SceneToScreen(r.RightTop);
  Result.RightBottom := SceneToScreen(r.RightBottom);
  Result.LeftBottom := SceneToScreen(r.LeftBottom);
end;

function TDrawEngine.SceneToScreen(buff: TArrayVec2): TArrayVec2;
var
  i: Integer;
begin
  SetLength(Result, length(buff));
  for i := 0 to length(buff) - 1 do
      Result[i] := SceneToScreen(buff[i]);
end;

function TDrawEngine.ScreenToScene(pt: TDEVec): TDEVec;
begin
  Result[0] := (pt[0] - Offset[0]) / Scale;
  Result[1] := (pt[1] - Offset[1]) / Scale;
end;

function TDrawEngine.ScreenToScene(x, y: TDEFloat): TDEVec;
begin
  Result := ScreenToScene(DEVec(x, y));
end;

function TDrawEngine.ScreenToScene(r: TDERect): TDERect;
begin
  Result[0] := ScreenToScene(r[0]);
  Result[1] := ScreenToScene(r[1]);
end;

function TDrawEngine.ScreenToScene(r: TRectf): TRectf;
begin
  Result := MakeRectf(ScreenToScene(RectV2(r)));
end;

function TDrawEngine.ScreenToScene(r: TRect): TRect;
begin
  Result := MakeRect(ScreenToScene(RectV2(r)));
end;

function TDrawEngine.ScreenToScene(r: TDE4V): TDE4V;
begin
  Result := TDE4V.Init(ScreenToScene(r.MakeRectV2), r.Angle);
end;

function TDrawEngine.ScreenToScene(r: TV2Rect4): TV2Rect4;
begin
  Result.LeftTop := ScreenToScene(r.LeftTop);
  Result.RightTop := ScreenToScene(r.RightTop);
  Result.RightBottom := ScreenToScene(r.RightBottom);
  Result.LeftBottom := ScreenToScene(r.LeftBottom);
end;

function TDrawEngine.ScreenToScene(buff: TArrayVec2): TArrayVec2;
var
  i: Integer;
begin
  SetLength(Result, length(buff));
  for i := 0 to length(buff) - 1 do
      Result[i] := ScreenToScene(buff[i]);
end;

function TDrawEngine.GetCameraR: TDERect;
begin
  Result := ScreenRectToScene;
end;

procedure TDrawEngine.SetCameraR(const Value: TDERect);
var
  r: TDERect;
begin
  r := RectFit(ScreenRect, Value, True);
  Scale := width / RectWidth(r);
  Offset[0] := -(r[0, 0] * Scale);
  Offset[1] := -(r[0, 1] * Scale);
end;

function TDrawEngine.GetCamera: TDEVec;
begin
  Result := ScreenCentreToScene;
end;

procedure TDrawEngine.SetCamera(const Value: TDEVec);
var
  siz: TDEVec;
begin
  siz := RectSize(CameraR);
  CameraR := RectV2(Value, siz[0], siz[1]);
end;

procedure TDrawEngine.ScaleCamera(f: TDEFloat);
var
  siz: TDEVec;
  sr: TDERect;
begin
  sr := ScreenRectToScene;
  siz := Vec2Div(RectSize(sr), f);
  SetCameraR(RectV2(RectCentre(sr), siz[0], siz[1]));
end;

procedure TDrawEngine.ScaleCameraFromWheelDelta(WheelDelta: Integer);
begin
  if WheelDelta > 0 then
      ScaleCamera(1.1)
  else
      ScaleCamera(0.9);
end;

procedure TDrawEngine.ResetCamera;
begin
  Scale := 1.0;
  Offset := Vec2(0, 0);
end;

function TDrawEngine.ReadyOK: Boolean;
begin
  Result := (FDrawInterface <> nil) and (FDrawInterface.ReadyOK);
end;

function TDrawEngine.ScreenCentreToScene: TDEVec;
begin
  Result := ScreenToScene(DEVec(width * 0.5, height * 0.5));
end;

function TDrawEngine.ScreenRectToScene: TDERect;
begin
  Result[0] := ScreenToScene(0, 0);
  Result[1] := ScreenToScene(width, height);
end;

function TDrawEngine.ScreenRectV2: TDERect;
begin
  Result[0] := NULLPoint;
  Result[1][0] := width;
  Result[1][1] := height;
end;

function TDrawEngine.ScreenV2Rect4: TV2Rect4;
begin
  Result := TV2Rect4.Init(width, height);
end;

procedure TDrawEngine.SetSize;
var
  v: TDEVec;
begin
  if ReadyOK then
    begin
      v := FDrawInterface.CurrentScreenSize;
      SetSize(v[0], v[1]);
    end;
end;

procedure TDrawEngine.SetSize(w, h: TDEFloat);
begin
  FWidth := w;
  FHeight := h;
end;

procedure TDrawEngine.SetSize(siz: TDEVec);
begin
  FWidth := siz[0];
  FHeight := siz[1];
end;

procedure TDrawEngine.SetSize(raster: TMemoryRaster);
begin
  SetSize(raster.Size2D);
end;

procedure TDrawEngine.SetSizeAndOffset(r: TDERect);
begin
  FWidth := RectWidth(r);
  FHeight := RectHeight(r);
  Offset := r[0];
end;

function TDrawEngine.SizeVec: TDEVec;
begin
  Result[0] := FWidth;
  Result[1] := FHeight;
end;

function TDrawEngine.SceneWidth: TDEFloat;
begin
  Result := width * Scale;
end;

function TDrawEngine.SceneHeight: TDEFloat;
begin
  Result := height * Scale;
end;

procedure TDrawEngine.SetDrawBounds(w, h: TDEFloat);
begin
  FWidth := w;
  FHeight := h;
  FDrawCommand.SetSize(DERect(0, 0, w, h));
end;

procedure TDrawEngine.SetDrawBounds(siz: TDEVec);
begin
  SetDrawBounds(siz[0], siz[1]);
end;

procedure TDrawEngine.SetDrawBounds(r: TDERect);
begin
  SetDrawBounds(RectWidth(r), RectHeight(r));
end;

procedure TDrawEngine.SetDrawBounds(r: TRectf);
begin
  SetDrawBounds(DERect(r));
end;

procedure TDrawEngine.ClearTextCache;
begin
  FTextSizeCache.Clear;
end;

function TDrawEngine.GetTextSize(const t: SystemString; Size: TDEFloat): TDEVec;
var
  buff: TDArraySegmentionText;
  p: PDEVec;
  n: SystemString;
begin
  if (FDrawInterface <> nil) and (t <> '') and (FDrawInterface.ReadyOK) then
    begin
      if IsSegmentionText(t) then
        begin
          buff := FillSegmentionText(t, Size, DEColor(1, 1, 1, 1), TDrawTextExpressionRunTime);
          Result := GetTextSize(buff);
          FreeSegmentionText(buff);
        end
      else
        begin
          n := umlFloatToStr(Size) + '_' + t;
          p := FTextSizeCache[n];
          if p = nil then
            begin
              new(p);
              LockObject(FDrawExecute);
              try
                  p^ := FDrawInterface.GetTextSize(t, Size);
              except
              end;
              UnLockObject(FDrawExecute);
              if FTextSizeCache.Count > 2000 then
                  FTextSizeCache.Clear;
              FTextSizeCache.Add(n, p, False);
            end;
          Result := p^;
        end;
    end
  else
      Result := NULLPoint;
end;

function TDrawEngine.GetTextSize(const buff: TDSegmentionLine): TDEVec;
var
  i: Integer;
  r4_siz: TDEVec;
begin
  Result := Vec2(0, 0);

  if length(buff) = 0 then
      exit;

  for i := 0 to length(buff) - 1 do
    begin
      r4_siz := GetTextSize(buff[i].Text, buff[i].Size);

      Result[0] := Result[0] + r4_siz[0];
      if r4_siz[1] > Result[1] then
          Result[1] := r4_siz[1];
    end;
end;

function TDrawEngine.GetTextSize(const buff: TDArraySegmentionText): TDEVec;
var
  i: Integer;
  r4_siz: TDEVec;
begin
  Result := Vec2(0, 0);

  if length(buff) = 0 then
      exit;

  for i := 0 to length(buff) - 1 do
    begin
      r4_siz := GetTextSize(buff[i]);

      Result[1] := Result[1] + r4_siz[1];
      if r4_siz[0] > Result[0] then
          Result[0] := r4_siz[0];
    end;
end;

function TDrawEngine.GetTextSizeR(const Text: SystemString; Size: TDEFloat): TDERect;
begin
  Result[0] := NULLVec;
  Result[1] := GetTextSize(Text, Size);
end;

function TDrawEngine.GetTextSizeR(const buff: TDArraySegmentionText): TDERect;
begin
  Result[0] := NULLVec;
  Result[1] := GetTextSize(buff);
end;

function TDrawEngine.ComputeScaleTextSize(const t: SystemString; Size: TDEFloat; MaxSiz: TDEVec): TDEFloat;
var
  L: TDEFloat;
  lsiz: TVec2;
begin
  L := Size;
  lsiz := GetTextSize(t, L);
  // compute scale
  if lsiz[0] > MaxSiz[0] then
      L := L * (MaxSiz[0] / lsiz[0])
  else if lsiz[1] > MaxSiz[1] then
      L := L * (MaxSiz[1] / lsiz[1]);
  Result := L;
end;

procedure TDrawEngine.ClearScrollText;
var
  i: Integer;
begin
  for i := 0 to FScrollTextList.Count - 1 do
      disposeObject(FScrollTextList[i]);

  FScrollTextList.Clear;
end;

procedure TDrawEngine.PostScrollText(LifeTime: Double; Text_: SystemString; Size: Integer; COLOR: TDEColor);
var
  n: U_String;
  Sour: TScrollTextSource;
begin
  if not ReadyOK then
      exit;

  n := Text_;

  if n.Exists(#10) then
    begin
      n := n.DeleteChar(#13);
      PostScrollText(LifeTime, umlGetFirstStr_Discontinuity(n, #10), Size, COLOR);
      n := umlDeleteFirstStr_Discontinuity(n, #10);
      PostScrollText(LifeTime, n, Size, COLOR);
      exit;
    end;

  Sour := TScrollTextSource.Create;
  Sour.LifeTime := LifeTime;
  Sour.TextSize := Size;
  Sour.TextColor := COLOR;
  Sour.Text := Text_;
  Sour.Tag := nil;
  FScrollTextList.Add(Sour);

  while FScrollTextList.Count > FMaxScrollText do
    begin
      disposeObject(FScrollTextList[0]);
      FScrollTextList.delete(0);
    end;
end;

procedure TDrawEngine.PostScrollText(Tag: TCoreClassObject; LifeTime: Double; Text_: SystemString; Size: Integer; COLOR: TDEColor);
var
  n: U_String;
  i: Integer;
  Sour: TScrollTextSource;
begin
  if not ReadyOK then
      exit;

  n := Text_;
  if n.Exists(#10) then
    begin
      n := n.DeleteChar(#13);
      PostScrollText(Tag, LifeTime, umlGetFirstStr_Discontinuity(n, #10), Size, COLOR);
      n := umlDeleteFirstStr_Discontinuity(n, #10);
      PostScrollText(Tag, LifeTime, n, Size, COLOR);
      exit;
    end;

  Sour := nil;
  for i := 0 to FScrollTextList.Count - 1 do
    begin
      if TScrollTextSource(FScrollTextList[i]).Tag = Tag then
        begin
          Sour := TScrollTextSource(FScrollTextList[i]);
          break;
        end;
    end;
  if Sour = nil then
    begin
      Sour := TScrollTextSource.Create;
      FScrollTextList.Add(Sour);
    end;

  Sour.LifeTime := LifeTime;
  Sour.TextSize := Size;
  Sour.TextColor := COLOR;
  Sour.Text := Text_;
  Sour.Tag := Tag;
end;

function TDrawEngine.GetLastPostScrollText: SystemString;
begin
  Result := '';
  if FScrollTextList.Count > 0 then
      Result := TScrollTextSource(FScrollTextList[FScrollTextList.Count - 1]).Text;
end;

procedure TDrawEngine.ClearUI;
begin
  while FUIList.Count > 0 do
      disposeObject(FUIList[0]);

  FLastAcceptDownUI := nil;
end;

procedure TDrawEngine.AllUINoVisibled;
var
  i: Integer;
  ui: TDrawEngine_UIBase;
begin
  i := 0;
  while i < FUIList.Count do
    begin
      ui := FUIList[i] as TDrawEngine_UIBase;
      ui.Visibled := False;
      inc(i);
    end;
end;

function TDrawEngine.TapDown(x, y: TDEFloat): Boolean;
var
  i: Integer;
  ui: TDrawEngine_UIBase;
begin
  FDownPT := DEVec(x, y);
  FMovePT := FDownPT;
  FUpPT := FMovePT;
  FLastAcceptDownUI := nil;

  i := 0;
  while i < FUIList.Count do
    begin
      ui := FUIList[i] as TDrawEngine_UIBase;
      if (ui.Visibled) and (ui.TapDown(x, y)) then
        begin
          FLastAcceptDownUI := ui;
          Result := True;
          exit;
        end;
      inc(i);
    end;
  Result := DoTapDown(x, y);
end;

function TDrawEngine.TapMove(x, y: TDEFloat): Boolean;
var
  i: Integer;
  ui: TDrawEngine_UIBase;
begin
  FMovePT := DEVec(x, y);
  FUpPT := FMovePT;
  if FLastAcceptDownUI <> nil then
    begin
      Result := FLastAcceptDownUI.TapMove(x, y);
    end
  else
    begin
      i := 0;
      while i < FUIList.Count do
        begin
          ui := FUIList[i] as TDrawEngine_UIBase;
          if (ui.Visibled) and (ui.TapMove(x, y)) then
            begin
              Result := True;
              exit;
            end;
          inc(i);
        end;
      Result := DoTapMove(x, y);
    end;
end;

function TDrawEngine.TapUp(x, y: TDEFloat): Boolean;
var
  i: Integer;
  ui: TDrawEngine_UIBase;
begin
  FUpPT := DEVec(x, y);
  if FLastAcceptDownUI <> nil then
    begin
      Result := FLastAcceptDownUI.TapUp(x, y);
    end
  else
    begin
      i := 0;
      while i < FUIList.Count do
        begin
          ui := FUIList[i] as TDrawEngine_UIBase;
          if (ui.Visibled) and (ui.TapUp(x, y)) then
            begin
              Result := True;
              exit;
            end;
          inc(i);
        end;
      Result := DoTapUp(x, y);
    end;
end;

procedure TDrawEngine.BeginCaptureShadow(const ScreenOffsetVec: TDEVec; const Alpha: TDEFloat);
begin
  FDrawCommand.BeginCaptureShadow(ScreenOffsetVec, Alpha);
end;

procedure TDrawEngine.BeginCaptureShadow(const ScreenOffsetVec: TDEVec; const Alpha: TDEFloat; ShadowSIGMA: TGeoFloat; ShadowSigmaGaussianKernelFactor: Integer);
begin
  FDrawCommand.BeginCaptureShadow(ScreenOffsetVec, Alpha, ShadowSIGMA, ShadowSigmaGaussianKernelFactor);
end;

procedure TDrawEngine.EndCaptureShadow;
begin
  FDrawCommand.EndCaptureShadow;
end;

function TDrawEngine.CaptureShadow: Boolean;
begin
  Result := FDrawCommand.FStartDrawShadowIndex >= 0;
end;

function TDrawEngine.LastCaptureScreenShadowOffsetVec: TDEVec;
begin
  Result := FDrawCommand.FScreenShadowOffset;
end;

function TDrawEngine.LastCaptureScreenShadowAlpha: TDEFloat;
begin
  Result := FDrawCommand.FScreenShadowAlpha;
end;

function TDrawEngine.ScreenRectInScreen(r: TDERect): Boolean;
begin
  Result := RectWithinRect(r, ScreenRect);
  Result := Result or RectWithinRect(ScreenRect, r);
  Result := Result or RectToRectIntersect(ScreenRect, r);
  Result := Result or RectToRectIntersect(r, ScreenRect);
end;

function TDrawEngine.ScreenRectInScreen(r: TV2Rect4): Boolean;
begin
  Result := ScreenRectInScreen(r.BoundRect);
end;

function TDrawEngine.SceneRectInScreen(r: TDERect): Boolean;
begin
  Result := ScreenRectInScreen(SceneToScreen(r));
end;

function TDrawEngine.SceneRectInScreen(r: TV2Rect4): Boolean;
begin
  Result := ScreenRectInScreen(SceneToScreen(r));
end;

procedure TDrawEngine.DrawUserCustom(const OnDraw: TCustomDraw_Method; const UserData: Pointer; const UserObject: TCoreClassObject);
begin
  if Assigned(OnDraw) then
      FDrawCommand.DrawUserCustom(OnDraw, UserData, UserObject);
end;

procedure TDrawEngine.DrawOutSideSmoothArrayLine(DotLine: Boolean; arry: TArrayVec2; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
var
  pl: TVec2List;
begin
  pl := TVec2List.Create;
  pl.AssignFromArrayV2(arry);
  DrawOutSideSmoothPL(DotLine, pl, ClosedLine, COLOR, LineWidth);
  disposeObject(pl);
end;

procedure TDrawEngine.DrawOutSideSmoothArrayLineInScene(DotLine: Boolean; arry: TArrayVec2; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
var
  pl: TVec2List;
begin
  pl := TVec2List.Create;
  pl.AssignFromArrayV2(arry);
  DrawOutSideSmoothPLInScene(DotLine, pl, ClosedLine, COLOR, LineWidth);
  disposeObject(pl);
end;

procedure TDrawEngine.DrawInSideSmoothArrayLine(DotLine: Boolean; arry: TArrayVec2; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
var
  pl: TVec2List;
begin
  pl := TVec2List.Create;
  pl.AssignFromArrayV2(arry);
  DrawInSideSmoothPL(DotLine, pl, ClosedLine, COLOR, LineWidth);
  disposeObject(pl);
end;

procedure TDrawEngine.DrawInSideSmoothArrayLineInScene(DotLine: Boolean; arry: TArrayVec2; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
var
  pl: TVec2List;
begin
  pl := TVec2List.Create;
  pl.AssignFromArrayV2(arry);
  DrawInSideSmoothPLInScene(DotLine, pl, ClosedLine, COLOR, LineWidth);
  disposeObject(pl);
end;

procedure TDrawEngine.DrawArrayLine(arry: TArrayVec2; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
var
  pl: TVec2List;
begin
  pl := TVec2List.Create;
  pl.AssignFromArrayV2(arry);
  DrawPL(pl, ClosedLine, COLOR, LineWidth);
  disposeObject(pl);
end;

procedure TDrawEngine.DrawArrayLineInScene(arry: TArrayVec2; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
var
  pl: TVec2List;
begin
  pl := TVec2List.Create;
  pl.AssignFromArrayV2(arry);
  DrawPLInScene(pl, ClosedLine, COLOR, LineWidth);
  disposeObject(pl);
end;

procedure TDrawEngine.DrawOutSideSmoothPL(DotLine: Boolean; pl: TVec2List; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
var
  n: TVec2List;
begin
  n := TVec2List.Create;
  if ClosedLine then
      pl.SplineSmoothOutSideClosed(n)
  else
      pl.SplineSmoothOpened(n);
  DrawPL(DotLine, n, ClosedLine, COLOR, LineWidth);
  disposeObject(n);
end;

procedure TDrawEngine.DrawOutSideSmoothPLInScene(DotLine: Boolean; pl: TVec2List; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
var
  n: TVec2List;
begin
  n := TVec2List.Create;
  if ClosedLine then
      pl.SplineSmoothOutSideClosed(n)
  else
      pl.SplineSmoothOpened(n);
  DrawPLInScene(DotLine, n, ClosedLine, COLOR, LineWidth);
  disposeObject(n);
end;

procedure TDrawEngine.DrawInSideSmoothPL(DotLine: Boolean; pl: TVec2List; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
var
  n: TVec2List;
begin
  n := TVec2List.Create;
  if ClosedLine then
      pl.SplineSmoothInSideClosed(n)
  else
      pl.SplineSmoothOpened(n);
  DrawPL(DotLine, n, ClosedLine, COLOR, LineWidth);
  disposeObject(n);
end;

procedure TDrawEngine.DrawInSideSmoothPLInScene(DotLine: Boolean; pl: TVec2List; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
var
  n: TVec2List;
begin
  n := TVec2List.Create;
  if ClosedLine then
      pl.SplineSmoothInSideClosed(n)
  else
      pl.SplineSmoothOpened(n);
  DrawPLInScene(DotLine, n, ClosedLine, COLOR, LineWidth);
  disposeObject(n);
end;

procedure TDrawEngine.DrawPL(DotLine: Boolean; pl: TVec2List; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
var
  i: Integer;
  t1, t2: TDEVec;
begin
  FDrawCommand.SetLineWidth(LineWidth);

  for i := 1 to pl.Count - 1 do
    begin
      t1 := pl[i - 1]^;
      t2 := pl[i]^;
      if DotLine then
          FDrawCommand.DrawDotLine(t1, t2, COLOR)
      else
          FDrawCommand.DrawLine(t1, t2, COLOR);
    end;
  if (ClosedLine) and (pl.Count > 1) then
    begin
      t1 := pl.First^;
      t2 := pl.Last^;
      if DotLine then
          FDrawCommand.DrawDotLine(t1, t2, COLOR)
      else
          FDrawCommand.DrawLine(t1, t2, COLOR);
    end;
end;

procedure TDrawEngine.DrawPLInScene(DotLine: Boolean; pl: TVec2List; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
var
  i: Integer;
  t1, t2: TDEVec;
begin
  FDrawCommand.SetLineWidth(LineWidth);

  for i := 1 to pl.Count - 1 do
    begin
      t1 := SceneToScreen(pl[i - 1]^);
      t2 := SceneToScreen(pl[i]^);
      if DotLine then
          FDrawCommand.DrawDotLine(t1, t2, COLOR)
      else
          FDrawCommand.DrawLine(t1, t2, COLOR);
    end;
  if (ClosedLine) and (pl.Count > 1) then
    begin
      t1 := SceneToScreen(pl.First^);
      t2 := SceneToScreen(pl.Last^);
      if DotLine then
          FDrawCommand.DrawDotLine(t1, t2, COLOR)
      else
          FDrawCommand.DrawLine(t1, t2, COLOR);
    end;
end;

procedure TDrawEngine.DrawPL(pl: TVec2List; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  DrawPL(False, pl, ClosedLine, COLOR, LineWidth);
end;

procedure TDrawEngine.DrawPLInScene(pl: TVec2List; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
var
  i: Integer;
  t1, t2: TDEVec;
begin
  FDrawCommand.SetLineWidth(LineWidth * Scale);

  for i := 1 to pl.Count - 1 do
    begin
      t1 := SceneToScreen(pl[i - 1]^);
      t2 := SceneToScreen(pl[i]^);
      FDrawCommand.DrawLine(t1, t2, COLOR);
    end;
  if (ClosedLine) and (pl.Count > 1) then
    begin
      t1 := SceneToScreen(pl.Last^);
      t2 := SceneToScreen(pl.First^);
      FDrawCommand.DrawLine(t1, t2, COLOR);
    end;
end;

procedure TDrawEngine.DrawPLInScene(pl: TVec2List; ClosedLine: Boolean; opt: TPolyDrawOption);
var
  i: Integer;
  t1, t2: TDEVec;
  r: TDERect;
begin
  FDrawCommand.SetLineWidth(opt.LineWidth * Scale);
  for i := 0 to pl.Count - 1 do
    begin
      t1 := SceneToScreen(pl[i]^);
      r[0][0] := t1[0] - opt.PointScreenRadius;
      r[0][1] := t1[1] - opt.PointScreenRadius;
      r[1][0] := t1[0] + opt.PointScreenRadius;
      r[1][1] := t1[1] + opt.PointScreenRadius;
      FDrawCommand.DrawEllipse(r, opt.PointColor);
    end;

  for i := 1 to pl.Count - 1 do
    begin
      t1 := SceneToScreen(pl[i - 1]^);
      t2 := SceneToScreen(pl[i]^);
      FDrawCommand.DrawLine(t1, t2, opt.LineColor);
    end;
  if (ClosedLine) and (pl.Count > 1) then
    begin
      t1 := SceneToScreen(pl.First^);
      t2 := SceneToScreen(pl.Last^);
      FDrawCommand.DrawLine(t1, t2, opt.LineColor);
    end;
end;

procedure TDrawEngine.DrawPolyInScene(Poly: TDeflectionPolygon; ClosedLine: Boolean; opt: TPolyDrawOption);
var
  i: Integer;
  t1, t2: TDEVec;
  r: TDERect;
begin
  if Poly.Count = 0 then
      exit;
  FDrawCommand.SetLineWidth(opt.LineWidth * Scale);

  for i := 1 to Poly.Count - 1 do
    begin
      t1 := SceneToScreen(Poly.Points[i - 1]);
      t2 := SceneToScreen(Poly.Points[i]);
      FDrawCommand.DrawLine(t1, t2, opt.LineColor);
    end;
  if (ClosedLine) and (Poly.Count > 1) then
    begin
      t1 := SceneToScreen(Poly.Points[0]);
      t2 := SceneToScreen(Poly.Points[Poly.Count - 1]);
      FDrawCommand.DrawLine(t1, t2, opt.LineColor);
    end;
  for i := 0 to Poly.Count - 1 do
    begin
      t1 := SceneToScreen(Poly.Points[i]);
      r[0][0] := t1[0] - opt.PointScreenRadius;
      r[0][1] := t1[1] - opt.PointScreenRadius;
      r[1][0] := t1[0] + opt.PointScreenRadius;
      r[1][1] := t1[1] + opt.PointScreenRadius;
      FDrawCommand.DrawEllipse(r, opt.PointColor);
    end;
end;

procedure TDrawEngine.DrawPolyExpandInScene(Poly: TDeflectionPolygon; ExpandDistance: TDEFloat; ClosedLine: Boolean; opt: TPolyDrawOption);
var
  i: Integer;
  t1, t2: TDEVec;
  r: TDERect;
begin
  FDrawCommand.SetLineWidth(opt.LineWidth * Scale);

  for i := 0 to Poly.Count - 1 do
    begin
      t1 := SceneToScreen(Poly.Expands[i, ExpandDistance]);
      r[0][0] := t1[0] - opt.PointScreenRadius;
      r[0][1] := t1[1] - opt.PointScreenRadius;
      r[1][0] := t1[0] + opt.PointScreenRadius;
      r[1][1] := t1[1] + opt.PointScreenRadius;
      FDrawCommand.DrawEllipse(r, opt.PointColor);
    end;

  for i := 1 to Poly.Count - 1 do
    begin
      t1 := SceneToScreen(Poly.Expands[i - 1, ExpandDistance]);
      t2 := SceneToScreen(Poly.Expands[i, ExpandDistance]);
      FDrawCommand.DrawLine(t1, t2, opt.LineColor);
    end;
  if (ClosedLine) and (Poly.Count > 1) then
    begin
      t1 := SceneToScreen(Poly.Expands[0, ExpandDistance]);
      t2 := SceneToScreen(Poly.Expands[Poly.Count - 1, ExpandDistance]);
      FDrawCommand.DrawLine(t1, t2, opt.LineColor);
    end;
end;

procedure TDrawEngine.DrawTriangle(DotLine: Boolean; const t: TTriangle; COLOR: TDEColor; LineWidth: TDEFloat; DrawCentre: Boolean);
begin
  if DotLine then
    begin
      DrawDotLine(t[0], t[1], COLOR, LineWidth);
      DrawDotLine(t[1], t[2], COLOR, LineWidth);
      DrawDotLine(t[2], t[0], COLOR, LineWidth);
    end
  else
    begin
      DrawLine(t[0], t[1], COLOR, LineWidth);
      DrawLine(t[1], t[2], COLOR, LineWidth);
      DrawLine(t[2], t[0], COLOR, LineWidth);
    end;
  if DrawCentre then
      DrawPoint(TriCentre(t), COLOR, LineWidth * 2, LineWidth);
end;

procedure TDrawEngine.DrawTriangle(DotLine: Boolean; const t: TTriangleList; COLOR: TDEColor; LineWidth: TDEFloat; DrawCentre: Boolean);
var
  i: Integer;
begin
  for i := 0 to t.Count - 1 do
      DrawTriangle(DotLine, t[i]^, COLOR, LineWidth, DrawCentre);
end;

procedure TDrawEngine.DrawTriangleInScene(DotLine: Boolean; const t: TTriangle; COLOR: TDEColor; LineWidth: TDEFloat; DrawCentre: Boolean);
begin
  if DotLine then
    begin
      DrawDotLineInScene(t[0], t[1], COLOR, LineWidth);
      DrawDotLineInScene(t[1], t[2], COLOR, LineWidth);
      DrawDotLineInScene(t[2], t[0], COLOR, LineWidth);
    end
  else
    begin
      DrawLineInScene(t[0], t[1], COLOR, LineWidth);
      DrawLineInScene(t[1], t[2], COLOR, LineWidth);
      DrawLineInScene(t[2], t[0], COLOR, LineWidth);
    end;
  if DrawCentre then
      DrawPointInScene(TriCentre(t), COLOR, LineWidth * 2, LineWidth);
end;

procedure TDrawEngine.DrawTriangleInScene(DotLine: Boolean; const t: TTriangleList; COLOR: TDEColor; LineWidth: TDEFloat; DrawCentre: Boolean);
var
  i: Integer;
begin
  for i := 0 to t.Count - 1 do
      DrawTriangleInScene(DotLine, t[i]^, COLOR, LineWidth, DrawCentre);
end;

procedure TDrawEngine.DrawDotLine(pt1, pt2: TDEVec; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  FDrawCommand.SetLineWidth(LineWidth);
  FDrawCommand.DrawDotLine(pt1, pt2, COLOR);
end;

procedure TDrawEngine.DrawDotLineInScene(pt1, pt2: TDEVec; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  DrawDotLine(SceneToScreen(pt1), SceneToScreen(pt2), COLOR, LineWidth * Scale);
end;

procedure TDrawEngine.DrawLine(pt1, pt2: TDEVec; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  FDrawCommand.SetLineWidth(LineWidth);
  FDrawCommand.DrawLine(pt1, pt2, COLOR);
end;

procedure TDrawEngine.DrawLineInScene(pt1, pt2: TDEVec; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  DrawLine(SceneToScreen(pt1), SceneToScreen(pt2), COLOR, LineWidth * Scale);
end;

procedure TDrawEngine.DrawCorner(Box: TV2Rect4; COLOR: TDEColor; BoundLineLength, LineWidth: TDEFloat);
  procedure DrawCornerLine_(left_, cen_, right_: TDEVec);
  begin
    if Vec2Distance(cen_, right_) > BoundLineLength then
        FDrawCommand.DrawLine(cen_, Vec2LerpTo(cen_, right_, BoundLineLength), COLOR);
    if Vec2Distance(cen_, left_) > BoundLineLength then
        FDrawCommand.DrawLine(cen_, Vec2LerpTo(cen_, left_, BoundLineLength), COLOR);
  end;

begin
  if (Vec2Distance(Box.LeftTop, Box.RightTop) < BoundLineLength * 2) or
    (Vec2Distance(Box.RightTop, Box.RightBottom) < BoundLineLength * 2) or
    (Vec2Distance(Box.RightBottom, Box.LeftBottom) < BoundLineLength * 2) or
    (Vec2Distance(Box.LeftBottom, Box.LeftTop) < BoundLineLength * 2) then
    begin
      DrawLine(Box.LeftTop, Box.RightTop, COLOR, LineWidth);
      DrawLine(Box.RightTop, Box.RightBottom, COLOR, LineWidth);
      DrawLine(Box.RightBottom, Box.LeftBottom, COLOR, LineWidth);
      DrawLine(Box.LeftBottom, Box.LeftTop, COLOR, LineWidth);
    end
  else
    begin
      FDrawCommand.SetLineWidth(LineWidth);
      DrawCornerLine_(Box.LeftBottom, Box.LeftTop, Box.RightTop);
      DrawCornerLine_(Box.LeftTop, Box.RightTop, Box.RightBottom);
      DrawCornerLine_(Box.RightTop, Box.RightBottom, Box.LeftBottom);
      DrawCornerLine_(Box.LeftTop, Box.LeftBottom, Box.RightBottom);
    end;
end;

procedure TDrawEngine.DrawCorner(Box: TDERect; COLOR: TDEColor; BoundLineLength, LineWidth: TDEFloat);
begin
  DrawCorner(TV2Rect4.Init(Box, 0), COLOR, BoundLineLength, LineWidth);
end;

procedure TDrawEngine.DrawCornerInScene(Box: TV2Rect4; COLOR: TDEColor; BoundLineLength, LineWidth: TDEFloat);
begin
  DrawCorner(SceneToScreen(Box), COLOR, BoundLineLength * Scale, LineWidth * Scale);
end;

procedure TDrawEngine.DrawCornerInScene(Box: TDERect; COLOR: TDEColor; BoundLineLength, LineWidth: TDEFloat);
begin
  DrawCorner(SceneToScreen(Box), COLOR, BoundLineLength * Scale, LineWidth * Scale);
end;

procedure TDrawEngine.DrawDE4V(d: TDE4V; COLOR: TDEColor; LineWidth: TDEFloat);
var
  pr: TV2Rect4;
begin
  pr := TV2Rect4.Init(d.MakeRectV2, d.Angle);
  DrawLine(pr.LeftTop, pr.RightTop, COLOR, LineWidth);
  DrawLine(pr.RightTop, pr.RightBottom, COLOR, LineWidth);
  DrawLine(pr.RightBottom, pr.LeftBottom, COLOR, LineWidth);
  DrawLine(pr.LeftBottom, pr.LeftTop, COLOR, LineWidth);
end;

procedure TDrawEngine.DrawDE4VInScene(d: TDE4V; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  DrawDE4V(SceneToScreen(d), COLOR, LineWidth * Scale);
end;

procedure TDrawEngine.DrawScreenPoint(pt: TDEVec; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  DrawLine(Vec2(pt[0], 0), Vec2(pt[0], height), COLOR, LineWidth);
  DrawLine(Vec2(0, pt[1]), Vec2(width, pt[1]), COLOR, LineWidth);
end;

procedure TDrawEngine.DrawScreenPointInScene(pt: TDEVec; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  DrawScreenPoint(SceneToScreen(pt), COLOR, LineWidth);
end;

procedure TDrawEngine.DrawCross(pt: TDEVec; COLOR: TDEColor; LineLength, LineWidth: TDEFloat);
begin
  DrawPoint(pt, COLOR, LineLength, LineWidth);
end;

procedure TDrawEngine.DrawPoint(pt: TDEVec; COLOR: TDEColor; LineLength, LineWidth: TDEFloat);
var
  pt1, pt2: TDEVec;
begin
  FDrawCommand.SetLineWidth(LineWidth);

  pt1[0] := pt[0] - LineLength;
  pt1[1] := pt[1] + LineLength;
  pt2[0] := pt[0] + LineLength;
  pt2[1] := pt[1] - LineLength;
  FDrawCommand.DrawLine(pt1, pt2, COLOR);

  pt1[0] := pt[0] + LineLength;
  pt1[1] := pt[1] + LineLength;
  pt2[0] := pt[0] - LineLength;
  pt2[1] := pt[1] - LineLength;
  FDrawCommand.DrawLine(pt1, pt2, COLOR);
end;

procedure TDrawEngine.DrawPointInScene(pt: TDEVec; COLOR: TDEColor; LineLength, LineWidth: TDEFloat);
begin
  DrawPoint(SceneToScreen(pt), COLOR, LineLength * Scale, LineWidth * Scale);
end;

procedure TDrawEngine.DrawDotLinePoint(pt: TDEVec; COLOR: TDEColor; LineLength, LineWidth: TDEFloat);
var
  pt1, pt2: TDEVec;
begin
  FDrawCommand.SetLineWidth(LineWidth);

  pt1[0] := pt[0] - LineLength;
  pt1[1] := pt[1] + LineLength;
  pt2[0] := pt[0] + LineLength;
  pt2[1] := pt[1] - LineLength;
  FDrawCommand.DrawDotLine(pt1, pt2, COLOR);

  pt1[0] := pt[0] + LineLength;
  pt1[1] := pt[1] + LineLength;
  pt2[0] := pt[0] - LineLength;
  pt2[1] := pt[1] - LineLength;
  FDrawCommand.DrawDotLine(pt1, pt2, COLOR);
end;

procedure TDrawEngine.DrawDotLinePointInScene(pt: TDEVec; COLOR: TDEColor; LineLength, LineWidth: TDEFloat);
begin
  DrawDotLinePoint(SceneToScreen(pt), COLOR, LineLength * Scale, LineWidth * Scale);
end;

procedure TDrawEngine.DrawArrayVec2(arry: TArrayVec2; COLOR: TDEColor; LineLength, LineWidth: TDEFloat);
var
  i: Integer;
begin
  for i := low(arry) to high(arry) do
      DrawPoint(arry[i], COLOR, LineLength, LineWidth);
end;

procedure TDrawEngine.DrawArrayVec2InScene(arry: TArrayVec2; COLOR: TDEColor; LineLength, LineWidth: TDEFloat);
var
  i: Integer;
begin
  for i := low(arry) to high(arry) do
      DrawPointInScene(arry[i], COLOR, LineLength, LineWidth);
end;

procedure TDrawEngine.DrawBox(Box: TV2Rect4; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  DrawLine(Box.LeftTop, Box.RightTop, COLOR, LineWidth);
  DrawLine(Box.RightTop, Box.RightBottom, COLOR, LineWidth);
  DrawLine(Box.RightBottom, Box.LeftBottom, COLOR, LineWidth);
  DrawLine(Box.LeftBottom, Box.LeftTop, COLOR, LineWidth);
end;

procedure TDrawEngine.DrawBoxInScene(Box: TV2Rect4; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  DrawBox(SceneToScreen(Box), COLOR, LineWidth * Scale);
end;

procedure TDrawEngine.DrawBox(r: TDERect; axis: TDEVec; Angle: TDEFloat; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  DrawBox(TV2Rect4.Init(r, axis, Angle), COLOR, LineWidth);
end;

procedure TDrawEngine.DrawBoxInScene(r: TDERect; axis: TDEVec; Angle: TDEFloat; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  DrawBox(SceneToScreen(r), SceneToScreen(axis), Angle, COLOR, LineWidth * Scale);
end;

procedure TDrawEngine.DrawBox(r: TDERect; Angle: TDEFloat; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  FDrawCommand.SetLineWidth(LineWidth);
  FDrawCommand.DrawRect(r, Angle, COLOR);
end;

procedure TDrawEngine.DrawBoxInScene(r: TDERect; Angle: TDEFloat; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  DrawBox(SceneToScreen(r), Angle, COLOR, LineWidth * Scale);
end;

procedure TDrawEngine.DrawBox(r: TDERect; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  DrawBox(r, 0, COLOR, LineWidth);
end;

procedure TDrawEngine.DrawBoxInScene(r: TDERect; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  DrawBox(SceneToScreen(r), COLOR, LineWidth * Scale);
end;

procedure TDrawEngine.DrawLabelBox(lab: SystemString; labSiz: TDEFloat; labColor: TDEColor;
  Box: TDERect; boxColor: TDEColor; boxLineWidth: TDEFloat);
var
  L: TDEFloat;
  r, lr: TDERect;
  lsiz, bsiz: TVec2;
begin
  r := ForwardRect(Box);
  DrawBox(r, boxColor, boxLineWidth);
  if lab = '' then
      exit;
  L := labSiz;
  lsiz := GetTextSize(lab, L);
  bsiz := RectSize(r);
  if (lsiz[0] < bsiz[0]) and (lsiz[1] < bsiz[1]) then
    begin
      lr[0] := r[0];
      lr[1] := Vec2Add(lr[0], lsiz);
    end
  else
    begin
      // compute font scale size
      if lsiz[0] > bsiz[0] * 0.8 then
          L := L * (bsiz[0] * 0.8 / lsiz[0])
      else
          L := L * (bsiz[1] * 0.8 / lsiz[1]);
      lsiz := GetTextSize(lab, L);
      lr[0] := r[0];
      lr[1] := Vec2Add(lr[0], lsiz);
    end;

  FillBox(lr, boxColor);
  DrawText(lab, L, lr, labColor, False);
end;

procedure TDrawEngine.DrawLabelBoxInScene(lab: SystemString; labSiz: TDEFloat; labColor: TDEColor;
  Box: TDERect; boxColor: TDEColor; boxLineWidth: TDEFloat);
begin
  DrawLabelBox(lab, labSiz, labColor, SceneToScreen(Box), boxColor, boxLineWidth);
end;

procedure TDrawEngine.DrawDotLineBox(Box: TV2Rect4; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  DrawDotLine(Box.LeftTop, Box.RightTop, COLOR, LineWidth);
  DrawDotLine(Box.RightTop, Box.RightBottom, COLOR, LineWidth);
  DrawDotLine(Box.RightBottom, Box.LeftBottom, COLOR, LineWidth);
  DrawDotLine(Box.LeftBottom, Box.LeftTop, COLOR, LineWidth);
end;

procedure TDrawEngine.DrawDotLineBoxInScene(Box: TV2Rect4; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  DrawDotLineBox(SceneToScreen(Box), COLOR, LineWidth * Scale);
end;

procedure TDrawEngine.DrawDotLineBox(r: TDERect; axis: TDEVec; Angle: TDEFloat; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  DrawDotLineBox(TV2Rect4.Init(r, axis, Angle), COLOR, LineWidth);
end;

procedure TDrawEngine.DrawDotLineBoxInScene(r: TDERect; axis: TDEVec; Angle: TDEFloat; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  DrawDotLineBox(SceneToScreen(r), SceneToScreen(axis), Angle, COLOR, LineWidth * Scale);
end;

procedure TDrawEngine.DrawDotLineBox(r: TDERect; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  DrawDotLineBox(TV2Rect4.Init(r), COLOR, LineWidth);
end;

procedure TDrawEngine.DrawDotLineBoxInScene(r: TDERect; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  DrawDotLineBox(SceneToScreen(r), COLOR, LineWidth * Scale);
end;

procedure TDrawEngine.FillBox();
begin
  FillBox(ScreenRect);
end;

procedure TDrawEngine.FillBox(Box: TV2Rect4);
begin
  FillBox(Box, DEColor(0, 0, 0));
end;

procedure TDrawEngine.FillBox(Box: TDERect);
begin
  FillBox(Box, DEColor(0, 0, 0));
end;

procedure TDrawEngine.FillBox(Box: TV2Rect4; COLOR: TDEColor);
var
  buff: TArrayVec2;
begin
  buff := Box.GetArrayVec2();
  FillPolygon(buff, COLOR);
  SetLength(buff, 0);
end;

procedure TDrawEngine.FillBoxInScene(Box: TV2Rect4; COLOR: TDEColor);
begin
  FillBox(SceneToScreen(Box), COLOR);
end;

procedure TDrawEngine.FillBox(r: TDERect; Angle: TDEFloat; COLOR: TDEColor);
begin
  FDrawCommand.FillRect(r, Angle, COLOR);
end;

procedure TDrawEngine.FillBoxInScene(r: TDERect; Angle: TDEFloat; COLOR: TDEColor);
begin
  FillBox(SceneToScreen(r), Angle, COLOR);
end;

procedure TDrawEngine.FillBox(r: TDERect; COLOR: TDEColor);
begin
  FillBox(r, 0, COLOR);
end;

procedure TDrawEngine.FillBoxInScene(r: TDERect; COLOR: TDEColor);
begin
  FillBox(SceneToScreen(r), COLOR);
end;

procedure TDrawEngine.DrawEllipse(pt: TDEVec; radius: TDEFloat; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  FDrawCommand.SetLineWidth(LineWidth);
  FDrawCommand.DrawEllipse(pt, radius, COLOR);
end;

procedure TDrawEngine.DrawEllipse(pt: TDEVec; radius: TDEFloat; COLOR: TDEColor);
begin
  FDrawCommand.DrawEllipse(pt, radius, COLOR);
end;

procedure TDrawEngine.DrawEllipse(r: TDERect; COLOR: TDEColor);
begin
  FDrawCommand.DrawEllipse(r, COLOR);
end;

procedure TDrawEngine.DrawEllipseInScene(pt: TDEVec; radius: TDEFloat; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  DrawEllipse(SceneToScreen(pt), radius * Scale, COLOR, LineWidth);
end;

procedure TDrawEngine.DrawEllipseInScene(pt: TDEVec; radius: TDEFloat; COLOR: TDEColor);
begin
  DrawEllipse(SceneToScreen(pt), radius * Scale, COLOR);
end;

procedure TDrawEngine.DrawEllipseInScene(r: TDERect; COLOR: TDEColor);
begin
  DrawEllipse(SceneToScreen(r), COLOR);
end;

procedure TDrawEngine.FillEllipse(pt: TDEVec; radius: TDEFloat; COLOR: TDEColor);
begin
  FDrawCommand.FillEllipse(pt, radius, COLOR);
end;

procedure TDrawEngine.FillEllipse(r: TDERect; COLOR: TDEColor);
begin
  FDrawCommand.FillEllipse(r, COLOR);
end;

procedure TDrawEngine.FillEllipseInScene(pt: TDEVec; radius: TDEFloat; COLOR: TDEColor);
begin
  FillEllipse(SceneToScreen(pt), radius * Scale, COLOR);
end;

procedure TDrawEngine.FillEllipseInScene(r: TDERect; COLOR: TDEColor);
begin
  FillEllipse(SceneToScreen(r), COLOR);
end;

procedure TDrawEngine.FillPolygon(PolygonBuff: TArrayVec2; COLOR: TDEColor);
begin
  FDrawCommand.FillPolygon(PolygonBuff, COLOR);
end;

procedure TDrawEngine.FillPolygonInScene(PolygonBuff: TArrayVec2; COLOR: TDEColor);
var
  buff: TArrayVec2;
begin
  buff := SceneToScreen(PolygonBuff);
  FillPolygon(buff, COLOR);
  SetLength(buff, 0);
end;

procedure TDrawEngine.DrawPolygon(PolygonBuff: TArrayVec2; COLOR: TDEColor; LineWidth: TDEFloat);
var
  L, i: Integer;
  t1, t2: TDEVec;
begin
  L := length(PolygonBuff);
  FDrawCommand.SetLineWidth(LineWidth);

  for i := 1 to L - 1 do
    begin
      t1 := PolygonBuff[i - 1];
      t2 := PolygonBuff[i];
      FDrawCommand.DrawLine(t1, t2, COLOR);
    end;
  if (L > 1) then
    begin
      t1 := PolygonBuff[0];
      t2 := PolygonBuff[L - 1];
      FDrawCommand.DrawLine(t1, t2, COLOR);
    end;
end;

procedure TDrawEngine.DrawPolygonInScene(PolygonBuff: TArrayVec2; COLOR: TDEColor; LineWidth: TDEFloat);
var
  buff: TArrayVec2;
begin
  buff := SceneToScreen(PolygonBuff);
  DrawPolygon(buff, COLOR, LineWidth);
  SetLength(buff, 0);
end;

procedure TDrawEngine.DrawPolygonDotLine(PolygonBuff: TArrayVec2; COLOR: TDEColor; LineWidth: TDEFloat);
var
  L, i: Integer;
  t1, t2: TDEVec;
begin
  L := length(PolygonBuff);
  FDrawCommand.SetLineWidth(LineWidth);

  for i := 1 to L - 1 do
    begin
      t1 := PolygonBuff[i - 1];
      t2 := PolygonBuff[i];
      FDrawCommand.DrawDotLine(t1, t2, COLOR);
    end;
  if (L > 1) then
    begin
      t1 := PolygonBuff[0];
      t2 := PolygonBuff[L - 1];
      FDrawCommand.DrawDotLine(t1, t2, COLOR);
    end;
end;

procedure TDrawEngine.DrawPolygonDotLineInScene(PolygonBuff: TArrayVec2; COLOR: TDEColor; LineWidth: TDEFloat);
var
  buff: TArrayVec2;
begin
  buff := SceneToScreen(PolygonBuff);
  DrawPolygonDotLineInScene(buff, COLOR, LineWidth);
  SetLength(buff, 0);
end;

class function TDrawEngine.RebuildTextColor(Text: SystemString; ts: TTextStyle;
  TextDecl_prefix_, TextDecl_postfix_,
  Comment_prefix_, Comment_postfix_,
  Number_prefix_, Number_postfix_,
  Symbol_prefix_, Symbol_postfix_,
  Ascii_prefix_, Ascii_postfix_: SystemString): SystemString;
var
  tp: TTextParsing;
  i: Integer;
  p: PTokenData;
begin
  if not IsSegmentionText(Text) then
    begin
      tp := TTextParsing.Create(Text, ts);
      for i := 0 to tp.TokenCount - 1 do
        begin
          p := tp[i];
          case p^.tokenType of
            ttTextDecl: p^.Text := TextDecl_prefix_ + p^.Text + TextDecl_postfix_;
            ttComment: p^.Text := Comment_prefix_ + p^.Text + Comment_prefix_;
            ttNumber: p^.Text := Number_prefix_ + p^.Text + Number_postfix_;
            ttSymbol: p^.Text := Symbol_prefix_ + p^.Text + Symbol_postfix_;
            ttAscii: p^.Text := Ascii_prefix_ + p^.Text + Ascii_postfix_;
          end;
        end;
      tp.RebuildToken;
      Result := tp.Text;
      disposeObject(tp);
    end
  else
      Result := Text;
end;

class function TDrawEngine.RebuildNumAndWordColor(Text: SystemString;
  Number_prefix_, Number_postfix_: SystemString;
  Ascii_matched_, Ascii_replace_: array of SystemString): SystemString;
var
  tp: TTextParsing;
  i, j: Integer;
  p: PTokenData;
begin
  if length(Ascii_matched_) <> length(Ascii_replace_) then
      raiseInfo('error.');
  if not IsSegmentionText(Text) then
    begin
      tp := TTextParsing.Create(Text);
      for i := 0 to tp.TokenCount - 1 do
        begin
          p := tp[i];
          case p^.tokenType of
            ttNumber: p^.Text := Number_prefix_ + p^.Text + Number_postfix_;
            ttAscii:
              begin
                for j := 0 to length(Ascii_matched_) - 1 do
                  if umlMultipleMatch(True, Ascii_matched_[j], p^.Text) then
                      p^.Text := Ascii_replace_[j];
              end;
          end;
        end;
      tp.RebuildToken;
      Result := tp.Text;
      disposeObject(tp);
    end
  else
      Result := Text;
end;

class function TDrawEngine.RebuildNumColor(Text: SystemString; Number_prefix_, Number_postfix_: SystemString): SystemString;
var
  tp: TTextParsing;
  i: Integer;
  p: PTokenData;
begin
  if not IsSegmentionText(Text) then
    begin
      tp := TTextParsing.Create(Text);
      for i := 0 to tp.TokenCount - 1 do
        begin
          p := tp[i];
          case p^.tokenType of
            ttNumber: p^.Text := Number_prefix_ + p^.Text + Number_postfix_;
          end;
        end;
      tp.RebuildToken;
      Result := tp.Text;
      disposeObject(tp);
    end
  else
      Result := Text;
end;

function TDrawEngine.DrawText(const Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean; RotateVec: TDEVec; Angle: TDEFloat): TV2Rect4;
var
  nr: TDERect;
  buff: TDArraySegmentionText;
  siz, order_pt: TDEVec;
begin
  nr := ForwardRect(r);
  buff := FillSegmentionText(Text, Size, COLOR, TDrawTextExpressionRunTime);
  siz := GetTextSize(buff);
  if center then
    begin
      order_pt[0] := nr[0, 0] + (RectWidth(nr) - siz[0]) * 0.5;
      order_pt[1] := nr[0, 1] + (RectHeight(nr) - siz[1]) * 0.5;
    end
  else
      order_pt := nr[0];

  Result := DrawSegmentionText(buff, order_pt, RotateVec, Angle);
  FreeSegmentionText(buff);
end;

function TDrawEngine.DrawText(const Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean): TV2Rect4;
begin
  Result := DrawText(Text, Size, r, COLOR, center, DEVec(0.5, 0.5), 0);
end;

function TDrawEngine.DrawText(const Text: SystemString; Size: TDEFloat; COLOR: TDEColor; ScreenPt: TDEVec): TV2Rect4;
var
  siz: TDEVec;
  r: TDERect;
begin
  siz := GetTextSize(Text, Size);
  r[0] := ScreenPt;
  r[1] := Vec2Add(ScreenPt, siz);
  Result := DrawText(Text, Size, r, COLOR, False);
end;

function TDrawEngine.DrawText(const Text: SystemString; Size: TDEFloat; COLOR: TDEColor; ScreenPt: TDEVec; Angle: TDEFloat): TV2Rect4;
var
  siz: TDEVec;
  r: TDERect;
begin
  siz := GetTextSize(Text, Size);
  r[0] := ScreenPt;
  r[1] := Vec2Add(ScreenPt, siz);
  Result := DrawText(Text, Size, r, COLOR, False, Vec2(0, 0), Angle);
end;

function TDrawEngine.DrawText(const lb, le: TDEVec; const Text: SystemString; Size: TDEFloat; COLOR: TDEColor): TV2Rect4;
var
  buff: TDArraySegmentionText;
  siz: TDEVec;
begin
  buff := FillSegmentionText(Text, Size, COLOR, TDrawTextExpressionRunTime);
  siz := GetTextSize(Text, Size);
  Result := DrawSegmentionText(buff, Vec2LerpTo(lb, le, (Vec2Distance(lb, le) - siz[0]) * 0.5), Vec2(0, 0), PointAngle(le, lb));
  FreeSegmentionText(buff);
end;

function TDrawEngine.DrawTextInScene(const Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean; RotateVec: TDEVec; Angle: TDEFloat): TV2Rect4;
begin
  Result := DrawText(Text, Size * Scale, SceneToScreen(r), COLOR, center, RotateVec, Angle);
end;

function TDrawEngine.DrawTextInScene(const Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean): TV2Rect4;
begin
  Result := DrawTextInScene(Text, Size, r, COLOR, center, DEVec(0.5, 0.5), 0);
end;

function TDrawEngine.DrawTextInScene(const Text: SystemString; Size: TDEFloat; COLOR: TDEColor; ScenePos: TDEVec): TV2Rect4;
var
  siz: TDEVec;
  r: TDERect;
begin
  siz := GetTextSize(Text, Size);
  r[0] := ScenePos;
  r[1] := Vec2Add(ScenePos, siz);
  Result := DrawTextInScene(Text, Size, r, COLOR, False);
end;

function TDrawEngine.DrawTextInScene(const Text: SystemString; Size: TDEFloat; COLOR: TDEColor; ScenePos: TDEVec; Angle: TDEFloat): TV2Rect4;
var
  siz: TDEVec;
  r: TDERect;
begin
  siz := GetTextSize(Text, Size);
  r[0] := ScenePos;
  r[1] := Vec2Add(ScenePos, siz);
  Result := DrawTextInScene(Text, Size, r, COLOR, False, Vec2(0, 0), Angle);
end;

function TDrawEngine.DrawTextInScene(const lb, le: TDEVec; const Text: SystemString; Size: TDEFloat; COLOR: TDEColor): TV2Rect4;
begin
  Result := DrawText(SceneToScreen(lb), SceneToScreen(le), Text, Size, COLOR);
end;

function TDrawEngine.DrawSegmentionText(const buff: TDArraySegmentionText; pt: TDEVec; RotateVec: TDEVec; Angle: TDEFloat): TV2Rect4;
var
  r4_x, r4_y: TDEFloat;
  lsiz, siz, rotAxis, r4_centre, r4_siz: TDEVec;
  r4_rect: TDERect;
  r4: TV2Rect4;
  j, i: Integer;
begin
  Result := TV2Rect4.Init();
  if length(buff) = 0 then
      exit;
  // order text size
  siz := GetTextSize(buff);
  // rectangle axis
  rotAxis := Vec2Add(pt, Vec2Mul(siz, RotateVec));
  r4_x := pt[0];
  r4_y := pt[1];
  for j := low(buff) to high(buff) do
    begin
      lsiz := GetTextSize(buff[j]);
      for i := low(buff[j]) to high(buff[j]) do
        begin
          // compute segmentation text size
          r4_siz := GetTextSize(buff[j, i].Text, buff[j, i].Size);
          // right offset
          r4_rect[0, 0] := r4_x;
          r4_rect[0, 1] := r4_y + (lsiz[1] - r4_siz[1]) * 0.5;
          r4_rect[1] := Vec2Add(r4_rect[0], r4_siz);
          r4_x := r4_rect[1, 0];
          // segmentation transform to local
          r4 := TV2Rect4.Init(ForwardRect(r4_rect));
          r4 := r4.Rotation(rotAxis, Angle);
          r4_centre := Vec2Middle(r4.LeftTop, r4.RightBottom);

          // now draw!
          FDrawCommand.DrawText(
            buff[j, i].Text,
            buff[j, i].Size,                                            // text size
            RectV2(r4_centre, r4_siz[0], r4_siz[1]),                    // compute new rectangle
            buff[j, i].COLOR,                                           // color
            False,                                                      // centre
            Vec2(0.5, 0.5),                                             // rotation axis coordinate
            Vec2Angle(r4_centre, Vec2Middle(r4.LeftTop, r4.LeftBottom)) // transform angle
            );

          // draw box
          if voTextBox in FViewOptions then
              DrawBox(r4, DEColor(1, 0, 0, 0.5), 1);
        end;
      // next line
      r4_x := pt[0];
      r4_y := r4_y + lsiz[1];
    end;

  Result := TV2Rect4.Init(RectV2(pt, Vec2Add(pt, siz)), rotAxis, Angle);
end;

function TDrawEngine.DrawSegmentionText(const buff: TDArraySegmentionText; pt: TDEVec): TV2Rect4;
begin
  Result := DrawSegmentionText(buff, pt, Vec2(0.5, 0.5), 0);
end;

function TDrawEngine.DrawSegmentionTextInScene(const buff: TDArraySegmentionText; pt: TDEVec; RotateVec: TDEVec; Angle: TDEFloat): TV2Rect4;
begin
  Result := DrawSegmentionText(buff, SceneToScreen(pt), Vec2(0.5, 0.5), 0);
end;

function TDrawEngine.DrawSegmentionTextInScene(const buff: TDArraySegmentionText; pt: TDEVec): TV2Rect4;
begin
  Result := DrawSegmentionTextInScene(buff, pt, Vec2(0.5, 0.5), 0);
end;

procedure TDrawEngine.DrawTile(t: TCoreClassObject; Sour: TDERect; Alpha: TDEFloat);
var
  i, j, w, h: TDEFloat;
begin
  w := RectWidth(Sour);
  h := RectHeight(Sour);
  i := 0;
  while i < width do
    begin
      j := 0;
      while j < height do
        begin
          DrawPicture(t, Sour, RectV2(i, j, i + w, j + h), Alpha);
          j := j + h;
        end;
      i := i + w;
    end;
end;

procedure TDrawEngine.DrawTile(t: TCoreClassObject);
begin
  if t is TDETexture then
      DrawTile(t, TDETexture(t).BoundsRectV2, 1.0);
end;

procedure TDrawEngine.DrawPicture(t: TCoreClassObject; Sour, DestScreen: TDE4V; Alpha: TDEFloat);
begin
  FDrawCommand.DrawPicture(t, Sour, DestScreen, Alpha);
end;

procedure TDrawEngine.DrawPicture(t: TCoreClassObject; Sour: TDERect; DestScreen: TDE4V; Alpha: TDEFloat);
begin
  DrawPicture(t, TDE4V.Init(Sour, 0), DestScreen, Alpha);
end;

procedure TDrawEngine.DrawPicture(t: TCoreClassObject; Sour, DestScreen: TDERect; Alpha: TDEFloat);
begin
  DrawPicture(t, TDE4V.Init(Sour, 0), TDE4V.Init(DestScreen, 0), Alpha);
end;

procedure TDrawEngine.DrawPicture(t: TCoreClassObject; Sour: TDERect; destScreenPt: TDEVec; Angle, Alpha: TDEFloat);
var
  w, h: TDEFloat;
begin
  w := Sour[1][0] - Sour[0][0];
  h := Sour[1][1] - Sour[0][1];
  DrawPicture(t, TDE4V.Init(Sour, 0), TDE4V.Init(destScreenPt, w, h, Angle), Alpha);
end;

procedure TDrawEngine.DrawPicture(t: TCoreClassObject; Sour, DestScreen: TDERect; Angle, Alpha: TDEFloat);
begin
  DrawPicture(t, TDE4V.Init(Sour, 0), TDE4V.Init(DestScreen, Angle), Alpha);
end;

function TDrawEngine.DrawPicture(indentEndge: Boolean; t: TCoreClassObject; Sour, DestScreen: TDERect; Alpha: TDEFloat): TDERect;
begin
  if indentEndge then
      Result := RectEdge(DestScreen, Vec2Mul(RectSize(DestScreen), -0.05))
  else
      Result := DestScreen;

  DrawPicture(t, Sour, Result, Alpha);
end;

procedure TDrawEngine.FitDrawPicture(t: TCoreClassObject; Sour, DestScreen: TDERect; Angle, Alpha: TDEFloat);
begin
  DrawPicture(t, Sour, RectFit(Sour, DestScreen), Angle, Alpha);
end;

function TDrawEngine.FitDrawPicture(t: TCoreClassObject; Sour, DestScreen: TDERect; Alpha: TDEFloat): TDERect;
begin
  Result := RectFit(Sour, DestScreen);
  DrawPicture(t, Sour, Result, Alpha);
end;

function TDrawEngine.FitDrawPicture(indentEndge: Boolean; t: TCoreClassObject; Sour, DestScreen: TDERect; Alpha: TDEFloat): TDERect;
begin
  if indentEndge then
      Result := RectEdge(DestScreen, Vec2Mul(RectSize(DestScreen), -0.05))
  else
      Result := DestScreen;

  FitDrawPicture(t, Sour, Result, Alpha);
end;

procedure TDrawEngine.DrawPictureInScene(t: TCoreClassObject; Sour, destScene: TDE4V; Alpha: TDEFloat);
begin
  DrawPicture(t, Sour, SceneToScreen(destScene), Alpha);
end;

procedure TDrawEngine.DrawPictureInScene(t: TCoreClassObject; Sour: TDERect; destScene: TDE4V; Alpha: TDEFloat);
begin
  DrawPictureInScene(t, TDE4V.Init(Sour, 0), destScene, Alpha);
end;

procedure TDrawEngine.DrawPictureInScene(t: TCoreClassObject; destScene: TDE4V; Alpha: TDEFloat);
begin
  DrawPictureInScene(t, TDE4V.Init, destScene, Alpha);
end;

procedure TDrawEngine.DrawPictureInScene(t: TCoreClassObject; Sour, destScene: TDERect; Alpha: TDEFloat);
begin
  DrawPictureInScene(t, TDE4V.Init(Sour, 0), TDE4V.Init(destScene, 0), Alpha);
end;

procedure TDrawEngine.DrawPictureInScene(t: TCoreClassObject; Sour: TDERect; destScenePt: TDEVec; Angle, Alpha: TDEFloat);
var
  w, h: TDEFloat;
begin
  w := Sour[1][0] - Sour[0][0];
  h := Sour[1][1] - Sour[0][1];
  DrawPictureInScene(t, TDE4V.Init(Sour, 0), TDE4V.Init(destScenePt, w, h, Angle), Alpha);
end;

procedure TDrawEngine.DrawPictureInScene(t: TCoreClassObject; Sour, destScene: TDERect; Angle, Alpha: TDEFloat);
begin
  DrawPictureInScene(t, TDE4V.Init(Sour, 0), TDE4V.Init(destScene, Angle), Alpha);
end;

procedure TDrawEngine.DrawPictureInScene(t: TCoreClassObject; destScenePt: TDEVec; Width_, Height_, Angle, Alpha: TDEFloat);
begin
  DrawPictureInScene(t, TDE4V.Init, TDE4V.Init(destScenePt, Width_, Height_, Angle), Alpha);
end;

function TDrawEngine.DrawPictureInScene(indentEndge: Boolean; t: TCoreClassObject; Sour, destScene: TDERect; Alpha: TDEFloat): TDERect;
begin
  if indentEndge then
      Result := RectEdge(destScene, Vec2Mul(RectSize(destScene), -0.05))
  else
      Result := destScene;

  DrawPictureInScene(t, Sour, Result, Alpha);
end;

procedure TDrawEngine.FitDrawPictureInScene(t: TCoreClassObject; Sour, destScene: TDERect; Angle, Alpha: TDEFloat);
begin
  DrawPictureInScene(t, Sour, RectFit(Sour, destScene), Angle, Alpha);
end;

function TDrawEngine.FitDrawPictureInScene(t: TCoreClassObject; Sour, destScene: TDERect; Alpha: TDEFloat): TDERect;
begin
  Result := RectFit(Sour, destScene);
  DrawPictureInScene(t, Sour, Result, Alpha);
end;

function TDrawEngine.FitDrawPictureInScene(indentEndge: Boolean; t: TCoreClassObject; Sour, destScene: TDERect; Alpha: TDEFloat): TDERect;
begin
  if indentEndge then
      Result := RectEdge(destScene, Vec2Mul(RectSize(destScene), -0.05))
  else
      Result := destScene;

  Result := FitDrawPictureInScene(t, Sour, Result, Alpha);
end;

function TDrawEngine.DrawRectPackingInScene(rp: TRectPacking; destOffset: TDEVec; Alpha: TDEFloat; ShowBox: Boolean): TDERect;
var
  i: Integer;
  t: TMemoryRaster;
  r: TDERect;
begin
  Result := NULLRect;
  for i := 0 to rp.Count - 1 do
    begin
      t := rp[i]^.Data2 as TMemoryRaster;
      r := RectAdd(rp[i]^.Rect, destOffset);
      if i = 0 then
          Result := r
      else
          Result := BoundRect(Result, r);
      DrawPictureInScene(t, t.BoundsRectV2, r, Alpha);
      if ShowBox then
          DrawLabelBox(PFormat('%d - %d*%d', [i + 1, t.width, t.height]), 12, DEColor(1, 1, 1), SceneToScreen(r), DEColor(1, 0, 0), 2);
    end;
end;

function TDrawEngine.DrawPicturePackingInScene(Input_: TMemoryRasterList; Margins: TDEFloat; destOffset: TDEVec; Alpha: TDEFloat; ShowBox: Boolean): TDERect;
var
  rp: TRectPacking;
  i: Integer;
begin
  Result := NULLRect;
  if Input_.Count = 0 then
      exit;
  rp := TRectPacking.Create;
  rp.Margins := Margins;

  for i := 0 to Input_.Count - 1 do
      rp.Add(nil, Input_[i], Input_[i].BoundsRectV2);

  rp.Build();
  Result := DrawRectPackingInScene(rp, destOffset, Alpha, ShowBox);
  disposeObject(rp);
end;

function TDrawEngine.DrawPicturePackingInScene(Input_: TMemoryRasterList; Margins: TDEFloat; destOffset: TDEVec; Alpha: TDEFloat): TDERect;
begin
  Result := DrawPicturePackingInScene(Input_, Margins, destOffset, Alpha, True);
end;

function TDrawEngine.DrawPictureMatrixPackingInScene(Input_: TMemoryRaster2DMatrix; Margins: TDEFloat; destOffset: TDEVec; Alpha: TDEFloat; ShowBox: Boolean): TDERect;
var
  rp: TRectPacking;
  i, j: Integer;
  L: TMemoryRasterList;
  tr: TRectPacking;
  r, rr: TDERect;
begin
  Result := NULLRect;
  if Input_.Count = 0 then
      exit;

  rp := TRectPacking.Create;
  rp.Margins := Margins;

  for i := 0 to Input_.Count - 1 do
    begin
      L := Input_[i];
      if L.Count = 0 then
          continue;
      tr := TRectPacking.Create;
      tr.Margins := 2;
      tr.UserToken := L.UserToken;
      for j := 0 to L.Count - 1 do
          tr.Add(nil, L[j], L[j].BoundsRectV2);
      tr.Build();
      rp.Add(nil, tr, tr.GetBoundsBox);
    end;
  rp.Build();

  for i := 0 to rp.Count - 1 do
    begin
      tr := rp[i]^.Data2 as TRectPacking;
      r := RectAdd(rp[i]^.Rect, destOffset);
      rr := DrawRectPackingInScene(tr, r[0], Alpha, False);
      DrawLabelBox(PFormat('%s %d', [tr.UserToken.Text, tr.Count]), 12, DEColor(1, 1, 1), SceneToScreen(r), DEColor(1, 0, 0), 1);
      disposeObject(tr);

      if i = 0 then
          Result := rr
      else
          Result := BoundRect(Result, rr);
    end;

  disposeObject(rp);
end;

function TDrawEngine.DrawTextPackingInScene(Input_: TArrayPascalString; text_color: TDEColor; text_siz, Margins: TDEFloat; destOffset: TDEVec; ShowBox: Boolean): TDERect;
var
  rp: TRectPacking;
  i: Integer;
  p: PPascalString;
  r: TDERect;
begin
  Result := NULLRect;
  if length(Input_) = 0 then
      exit;
  rp := TRectPacking.Create;
  rp.Margins := Margins;

  for i := 0 to length(Input_) - 1 do
      rp.Add(@Input_[i], nil, GetTextSizeR(Input_[i], text_siz));

  rp.Build();

  for i := 0 to rp.Count - 1 do
    begin
      p := rp[i]^.Data1;
      r := RectAdd(rp[i]^.Rect, destOffset);
      if i = 0 then
          Result := r
      else
          Result := BoundRect(Result, r);

      DrawTextInScene(p^, text_siz, r, text_color, True);
      if ShowBox then
          DrawBoxInScene(r, DEColor(1, 0, 0), 2);
    end;

  disposeObject(rp);
end;

function TDrawEngine.DrawTextPackingInScene(Input_: TArrayPascalString; text_color: TDEColor; text_siz, Margins: TDEFloat; destOffset: TDEVec): TDERect;
begin
  Result := DrawTextPackingInScene(Input_, text_color, text_siz, Margins, destOffset, True);
end;

function TDrawEngine.CreateSequenceAnimation(stream: TCoreClassStream): TSequenceAnimationBase;
begin
  Result := TSequenceAnimationBase.Create(Self);
  Result.LoadFromStream(stream);
  FSequenceAnimationBuffer.Add(Result);
end;

function TDrawEngine.GetOrCreateSequenceAnimation(flag: Variant; t: TCoreClassObject): TSequenceAnimationBase;
var
  i: Integer;
begin
  for i := 0 to FSequenceAnimationBuffer.Count - 1 do
    begin
      Result := FSequenceAnimationBuffer[i] as TSequenceAnimationBase;
      try
        if (Result.Source = t) and (VarType(Result.flag) = VarType(flag)) and (umlSameVarValue(Result.flag, flag)) then
            exit;
      except
      end;
    end;

  Result := TSequenceAnimationBase.Create(Self);
  Result.Source := t;
  Result.flag := flag;

  if t is TSequenceMemoryRaster then
    begin
      Result.width := TSequenceMemoryRaster(t).width;
      Result.height := TSequenceMemoryRaster(t).height;
      Result.Total := TSequenceMemoryRaster(t).Total;
      Result.Column := TSequenceMemoryRaster(t).Column;
    end;
  Result.CompleteTime := 1.0;
  Result.PlayMode := TSequenceAnimationPlayMode.sapmPlayOne;

  Result.LastUsed := True;
  FSequenceAnimationBuffer.Add(Result);
end;

function TDrawEngine.SequenceAnimationPlaying(flag: Variant; t: TCoreClassObject): Boolean;
var
  i: Integer;
  SA: TSequenceAnimationBase;
begin
  Result := False;
  SA := nil;
  for i := 0 to FSequenceAnimationBuffer.Count - 1 do
    begin
      SA := FSequenceAnimationBuffer[i] as TSequenceAnimationBase;
      if (SA.Source = t) and (VarType(SA.flag) = VarType(flag)) and (umlSameVarValue(SA.flag, flag)) then
          break;
    end;
  if SA = nil then
      exit;
  Result := SA.SequenceAnimationPlaying;
end;

function TDrawEngine.SequenceAnimationIsOver(flag: Variant; t: TCoreClassObject): Boolean;
var
  i: Integer;
  SA: TSequenceAnimationBase;
begin
  Result := True;
  SA := nil;
  for i := 0 to FSequenceAnimationBuffer.Count - 1 do
    begin
      SA := FSequenceAnimationBuffer[i] as TSequenceAnimationBase;
      if (SA.Source = t) and (VarType(SA.flag) = VarType(flag)) and (umlSameVarValue(SA.flag, flag)) then
          break;
    end;
  if SA = nil then
      exit;
  Result := SA.IsOver;
end;

function TDrawEngine.ExistsSequenceAnimation(SA: TSequenceAnimationBase): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FSequenceAnimationBuffer.Count - 1 do
    if FSequenceAnimationBuffer[i] = SA then
        exit(True);
end;

function TDrawEngine.GetNewSequenceFlag: Variant;
begin
  Result := FLastDynamicSeqenceFlag;
  FLastDynamicSeqenceFlag := FLastDynamicSeqenceFlag + 1;
end;

function TDrawEngine.ManualDrawSequenceTexture(flag: Variant; t: TCoreClassObject; TextureWidth, TextureHeight, Total, Column: Integer; CompleteTime: Double; Looped: Boolean;
  DestScreen: TDE4V; Alpha: TDEFloat): TSequenceAnimationBase;
var
  SA: TSequenceAnimationBase;
begin
  Result := nil;
  if Total = 0 then
      exit;
  if Column = 0 then
      exit;

  SA := GetOrCreateSequenceAnimation(flag, t);
  SA.width := TextureWidth;
  SA.height := TextureHeight;
  SA.Total := Total;
  SA.Column := Column;
  SA.CompleteTime := CompleteTime;
  if Looped then
      SA.PlayMode := TSequenceAnimationPlayMode.sapmLoop
  else
      SA.PlayMode := TSequenceAnimationPlayMode.sapmPlayOne;

  SA.LastUsed := True;
  DrawPicture(SA.Source, SA.SequenceFrameRect, DestScreen, SA.GetOverAnimationSmoothAlpha(Alpha));
  Result := SA;
end;

function TDrawEngine.DrawSequenceTexture(flag: Variant; t: TCoreClassObject; TextureWidth, TextureHeight, Total, Column: Integer; CompleteTime: Double; Looped: Boolean;
  DestScreen: TDE4V; Alpha: TDEFloat): TSequenceAnimationBase;
begin
  Result := ManualDrawSequenceTexture(flag, t, TextureWidth, TextureHeight, Total, Column, CompleteTime, Looped, DestScreen, Alpha);
end;

function TDrawEngine.DrawSequenceTexture(flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; DestScreen: TDE4V; Alpha: TDEFloat): TSequenceAnimationBase;
begin
  Result := DrawSequenceTexture(flag, t, t.width, t.height, t.Total, t.Column, CompleteTime, Looped, DestScreen, Alpha);
end;

function TDrawEngine.DrawSequenceTexture(flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; DestScreen: TDERect; Alpha: TDEFloat): TSequenceAnimationBase;
begin
  Result := DrawSequenceTexture(flag, t, t.width, t.height, t.Total, t.Column, CompleteTime, Looped, TDE4V.Init(DestScreen, 0), Alpha);
end;

function TDrawEngine.DrawSequenceTexture(flag: Variant; t: TDETexture; CompleteTime: Double; DestScreen: TDE4V; Alpha: TDEFloat): TSequenceAnimationBase;
begin
  Result := DrawSequenceTexture(flag, t, t.width, t.height, t.Total, t.Column, CompleteTime, False, DestScreen, Alpha);
end;

function TDrawEngine.FitDrawSequenceTexture(flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; DestScreen: TDERect; Alpha: TDEFloat): TDERect;
begin
  Result := RectFit(t.FrameRect2D, DestScreen);
  DrawSequenceTextureInScene(flag, t, t.width, t.height, t.Total, t.Column, CompleteTime, Looped, TDE4V.Init(Result, 0), Alpha);
end;

function TDrawEngine.FitDrawSequenceTexture(indentEndge: Boolean; flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; DestScreen: TDERect; Alpha: TDEFloat): TDERect;
var
  d: TDERect;
begin
  if indentEndge then
      d := RectEdge(DestScreen, Vec2Mul(RectSize(DestScreen), -0.05))
  else
      d := DestScreen;

  Result := RectFit(t.FrameRect2D, d);
  DrawSequenceTextureInScene(flag, t, t.width, t.height, t.Total, t.Column, CompleteTime, Looped, TDE4V.Init(Result, 0), Alpha);
end;

procedure TDrawEngine.DrawSequenceTexture(SA: TSequenceAnimationBase; DestScreen: TDE4V; Alpha: TDEFloat);
begin
  SA.LastUsed := True;
  DrawPicture(SA.Source, SA.SequenceFrameRect, DestScreen, SA.GetOverAnimationSmoothAlpha(Alpha));
end;

function TDrawEngine.DrawSequenceTextureInScene(flag: Variant; t: TCoreClassObject; TextureWidth, TextureHeight, Total, Column: Integer; CompleteTime: Double; Looped: Boolean;
  destScene: TDE4V; Alpha: TDEFloat): TSequenceAnimationBase;
begin
  Result := DrawSequenceTexture(flag, t, TextureWidth, TextureHeight, Total, Column, CompleteTime, Looped, SceneToScreen(destScene), Alpha);
end;

function TDrawEngine.DrawSequenceTextureInScene(flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; destScene: TDE4V; Alpha: TDEFloat): TSequenceAnimationBase;
begin
  Result := DrawSequenceTextureInScene(flag, t, t.width, t.height, t.Total, t.Column, CompleteTime, Looped, destScene, Alpha);
end;

function TDrawEngine.DrawSequenceTextureInScene(flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; destScene: TDERect; Alpha: TDEFloat): TSequenceAnimationBase;
begin
  Result := DrawSequenceTextureInScene(flag, t, t.width, t.height, t.Total, t.Column, CompleteTime, Looped, TDE4V.Init(destScene, 0), Alpha);
end;

function TDrawEngine.DrawSequenceTextureInScene(flag: Variant; t: TDETexture; CompleteTime: Double; destScene: TDE4V; Alpha: TDEFloat): TSequenceAnimationBase;
begin
  Result := DrawSequenceTextureInScene(flag, t, t.width, t.height, t.Total, t.Column, CompleteTime, False, destScene, Alpha);
end;

function TDrawEngine.FitDrawSequenceTextureInScene(flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; destScene: TDERect; Alpha: TDEFloat): TDERect;
begin
  Result := RectFit(t.FrameRect2D, destScene);
  DrawSequenceTextureInScene(flag, t, t.width, t.height, t.Total, t.Column, CompleteTime, Looped, TDE4V.Init(Result, 0), Alpha);
end;

function TDrawEngine.FitDrawSequenceTextureInScene(indentEndge: Boolean; flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; destScene: TDERect; Alpha: TDEFloat): TDERect;
var
  d: TDERect;
begin
  if indentEndge then
      d := RectEdge(destScene, Vec2Mul(RectSize(destScene), -0.05))
  else
      d := destScene;

  Result := RectFit(t.FrameRect2D, d);
  DrawSequenceTextureInScene(flag, t, t.width, t.height, t.Total, t.Column, CompleteTime, Looped, TDE4V.Init(Result, 0), Alpha);
end;

procedure TDrawEngine.DrawSequenceTextureInScene(SA: TSequenceAnimationBase; destScene: TDE4V; Alpha: TDEFloat);
begin
  DrawSequenceTexture(SA, SceneToScreen(destScene), Alpha);
end;

function TDrawEngine.CreateParticles: TParticles;
begin
  Result := TParticles.Create(Self);
  FParticleBuffer.Add(Result);
end;

function TDrawEngine.CreateParticles(stream: TCoreClassStream): TParticles;
begin
  Result := TParticles.Create(Self);
  Result.LoadFromStream(stream);
  FParticleBuffer.Add(Result);
end;

procedure TDrawEngine.DeleteParticles(p: TParticles);
var
  i: Integer;
begin
  i := 0;
  while i < FParticleBuffer.Count do
    if FParticleBuffer[i] = p then
        FParticleBuffer.delete(i)
    else
        inc(i);
end;

procedure TDrawEngine.FreeAndDeleteParticles(p: TParticles);
var
  i: Integer;
begin
  i := 0;
  while i < FParticleBuffer.Count do
    if FParticleBuffer[i] = p then
        FParticleBuffer.delete(i)
    else
        inc(i);
  p.Owner := nil;
  disposeObject(p);
end;

procedure TDrawEngine.ClearParticles;
var
  i: Integer;
begin
  for i := 0 to FParticleBuffer.Count - 1 do
    begin
      TParticles(FParticleBuffer[i]).Owner := nil;
      disposeObject(FParticleBuffer[i]);
    end;
  FParticleBuffer.Clear;
end;

function TDrawEngine.ExistsParticles(p: TParticles): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FParticleBuffer.Count - 1 do
    if FParticleBuffer[i] = p then
        exit(True);
end;

function TDrawEngine.TotalParticleData: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FParticleBuffer.Count - 1 do
      Result := Result + TParticles(FParticleBuffer[i]).ParticleBuff.Count;
end;

function TDrawEngine.ParticleCount: Integer;
begin
  Result := FParticleBuffer.Count;
end;

function TDrawEngine.GetParticles(const Index: Integer): TParticles;
begin
  Result := FParticleBuffer[index] as TParticles;
end;

procedure TDrawEngine.DrawParticle(Particle: TParticles; DestScreen: TDEVec);
var
  i: Integer;
  p: PParticleData;
begin
  if (Particle.Owner <> Self) then
    begin
      if Particle.Owner <> nil then
          Particle.Owner.DeleteParticles(Particle);
      DeleteParticles(Particle);
      FParticleBuffer.Add(Particle);
    end;

  Particle.Owner := Self;
  Particle.LastDrawPosition := DestScreen;

  if Particle.Visible then
    begin
      try
        for i := 0 to Particle.ParticleBuff.Count - 1 do
          begin
            p := Particle.ParticleBuff[i];
            DrawSequenceTexture(p^.Source, TDE4V.Init(p^.Position, p^.radius * 2, p^.radius * 2, p^.Angle), p^.Alpha);
          end;
      except
      end;
    end;
end;

procedure TDrawEngine.DrawParticleInScene(Particle: TParticles; destScene: TDEVec);
begin
  DrawParticle(Particle, SceneToScreen(destScene));
end;

function TDrawEngine.GetTexture(TextureName: SystemString): TDETexture;
begin
  Result := FTextureLibrary[TextureName] as TDETexture;
  if Result = nil then
    begin
      if Assigned(FOnGetTexture) then
          FOnGetTexture(TextureName, Result);
      if Result <> nil then
        begin
          Result.Name := TextureName;
          FTextureLibrary.Add(TextureName, Result);
        end;
    end;
  if Result = nil then
      PostScrollText(10, 'no exists Texture ' + TextureName, 12, DEColor(1, 0.5, 0.5, 1));
end;

function TDrawEngine.GetTextureName(t: TCoreClassObject): SystemString;
begin
  if t is TDETexture then
      Result := TDETexture(t).Name
  else
      Result := FTextureLibrary.GetObjAsName(t);
end;

class function TDrawEngine.NewTexture: TDETexture;
begin
  Result := DefaultTextureClass.Create;
end;

procedure TDrawEngine.PrepareTextureOutputState;
var
  bakScale: TDEFloat;
  bakOffset: TDEVec;
  r: TDERect;

  rl: TRectPacking;
  i: Integer;
  ptex: PTextureOutputState;
  pr: PRectPackData;
begin
  bakScale := Scale;
  bakOffset := Offset;
  try
    rl := TRectPacking.Create;

    for i := Low(FPictureFlushInfo) to High(FPictureFlushInfo) do
      begin
        ptex := @(FPictureFlushInfo[i]);
        if not rl.Data2Exists(ptex^.Source) then
          begin
            if ptex^.Source is TMemoryRaster then
                rl.Add(ptex, ptex^.Source, ptex^.SourceRect.BoundRect);
          end;
      end;
    rl.Build();

    r := RectFit(DERect(0, 0, rl.MaxWidth + 4, rl.MaxHeight + 4), FTextureOutputStateBox);
    Scale := RectWidth(r) / rl.MaxWidth;
    Offset := r[0];

    FillBox(FTextureOutputStateBox, DEColor(0, 0, 0, 0.95));

    for i := 0 to rl.Count - 1 do
      begin
        pr := rl[i];
        ptex := pr^.Data1;
        DrawPictureInScene(ptex^.Source, ptex^.SourceRect.BoundRect, TDE4V.Init(pr^.Rect, 0), 0.5);
      end;
    DrawText('Texture:' + umlIntToStr(rl.Count).Text + ' Area:' + umlIntToStr(Round(rl.MaxWidth)).Text + ' x ' + umlIntToStr(Round(rl.MaxHeight)).Text,
      10, DEColor(1, 1, 1, 1), r[0]);
    disposeObject(rl);
  except
  end;
  Scale := bakScale;
  Offset := bakOffset;
end;

procedure TDrawEngine.PrepareFlush;
var
  lastTime: TTimeTick;
  i: Integer;
  pt, siz: TDEVec;
  r: TDERect;
  st: TScrollTextSource;
  ui: TDrawEngine_UIBase;
  SA: TSequenceAnimationBase;
begin
  lastTime := GetTimeTick;
  inc(FPerformaceCounter);

  DoFlush;

  BeginCaptureShadow(Vec2(1, 1), 0.9);
  pt := DEVec(width - 5, height - 5);
  i := FScrollTextList.Count - 1;
  while i >= 0 do
    begin
      st := FScrollTextList[i] as TScrollTextSource;
      siz := GetTextSize(st.Text, st.TextSize);
      if st.LifeTime > 0 then
        begin
          r[0] := Vec2Sub(pt, siz);
          r[1] := Vec2Add(r[0], siz);
          pt[1] := pt[1] - siz[1];
          DrawText(st.Text, st.TextSize, r, st.TextColor, False);
        end;
      dec(i);
    end;
  EndCaptureShadow;

  i := 0;
  while i < FUIList.Count do
    begin
      ui := FUIList[i] as TDrawEngine_UIBase;
      if ui.Visibled then
          ui.DoDraw;
      inc(i);
    end;

  if voEdge in FViewOptions then
    begin
      FDrawCommand.SetLineWidth(FScreenFrameSize);
      FDrawCommand.DrawRect(MakeRect(1, 1, width - 1, height - 1), 0, FScreenFrameColor);
    end;

  FLastDrawInfo :=
    'resolution: ' + umlIntToStr(Round(width)).Text + ' * ' + umlIntToStr(Round(height)).Text;
  if Round(FrameCounterOfPerSec) > 0 then
      FLastDrawInfo := FLastDrawInfo + ' fps: ' + umlIntToStr(Round(FrameCounterOfPerSec)).Text + ' pipe: ' + umlIntToStr(Round(CommandCounterOfPerSec)).Text;

  if voFPS in FViewOptions then
    begin
      BeginCaptureShadow(Vec2(1, 1), 1.0);
      DrawText(FLastDrawInfo, FFPSFontSize, FFPSFontColor, DEVec(5, 5));
      EndCaptureShadow;
    end;

  if lastTime - FLastPerformaceTime > 1000 then
    begin
      FFrameCounterOfPerSec := FPerformaceCounter / ((lastTime - FLastPerformaceTime) / 1000);
      FCommandCounterOfPerSec := FCommandCounter / ((lastTime - FLastPerformaceTime) / 1000);
      FLastPerformaceTime := lastTime;
      FPerformaceCounter := 0;
      FCommandCounter := 0;
    end;

  i := 0;
  while i < FSequenceAnimationBuffer.Count do
    begin
      SA := FSequenceAnimationBuffer[i] as TSequenceAnimationBase;
      if not SA.LastUsed then
        begin
          disposeObject(SA);
          FSequenceAnimationBuffer.delete(i);
        end
      else
          inc(i);
    end;

  if voPictureState in FViewOptions then
      PrepareTextureOutputState;
end;

procedure TDrawEngine.ClearFlush;
begin
  FDrawCommand.Clear(True);
  FDrawExecute.Clear;
  SetLength(FPictureFlushInfo, 0);
end;

procedure TDrawEngine.Flush;
begin
  Flush(True);
end;

procedure TDrawEngine.Flush(Prepare: Boolean);
begin
  try
    FDrawCommand.BuildTextureOutputState(FPictureFlushInfo);

    if Prepare then
        PrepareFlush;

    if FDrawInterface <> nil then
      begin
        LockObject(FDrawExecute);
        try
          FDrawExecute.PickQueue(FDrawCommand);
          FCommandCounter := FCommandCounter + FDrawExecute.FCommandList.Count;
          FDrawExecute.Execute(FDrawInterface);
        finally
            UnLockObject(FDrawExecute);
        end;
      end
    else
        ClearFlush;
  except
  end;
end;

procedure TDrawEngine.CopyFlushTo(Dst: TDrawExecute);
begin
  LockObject(Dst);
  try
      Dst.PickQueue(FDrawCommand);
  finally
      UnLockObject(Dst);
  end;
end;

procedure TDrawEngine.Progress(deltaTime: Double);
var
  i: Integer;
  SA: TSequenceAnimationBase;
  st: TScrollTextSource;
  p: TParticles;
begin
  i := 0;
  while i < FSequenceAnimationBuffer.Count do
    begin
      SA := FSequenceAnimationBuffer[i] as TSequenceAnimationBase;
      SA.Progress(deltaTime);
      SA.LastUsed := False;
      inc(i);
    end;

  i := 0;
  while i < FScrollTextList.Count do
    begin
      st := FScrollTextList[i] as TScrollTextSource;
      if st.LifeTime - deltaTime < 0 then
        begin
          disposeObject(st);
          FScrollTextList.delete(i);
        end
      else
        begin
          if deltaTime > 0 then
              st.TextColor[3] := st.TextColor[3] - st.TextColor[3] * (deltaTime / st.LifeTime);
          st.LifeTime := st.LifeTime - deltaTime;
          inc(i);
        end;
    end;

  i := 0;
  while i < FParticleBuffer.Count do
    begin
      try
        p := FParticleBuffer[i] as TParticles;

        p.Progress(deltaTime);

        try
          if (p.NoEnabledAutoFree) and (((not p.Enabled) and (p.Visible) and (p.VisibledParticle = 0)) or
            ((not p.Enabled) and (not p.Visible))) then
            begin
              p.Owner := nil;
              disposeObject(p);
              FParticleBuffer.delete(i);
            end
          else
            begin
              inc(i);
            end;
        except
        end;
      except
      end;
    end;

  FLastDeltaTime := deltaTime;
  FLastNewTime := FLastNewTime + FLastDeltaTime;

  FPostProgress.Progress(deltaTime);
end;

function TDrawEngine.Progress(): Double;
begin
  FCadencerEng.Progress();
  Result := FLastDeltaTime;
end;

procedure TDrawEngine.SetDrawInterfaceAsDefault;
begin
  DrawInterface := FRasterization;
end;

function TMemoryRasterHelperForDrawEngine.GetDrawEngine: TDrawEngine;
begin
  Result := TDrawEngine(DrawEngineMap);
end;

initialization

DefaultTextureClass := TDETexture;
EnginePool := TDrawEnginePool.Create;
TexturePool := TDETextureList.Create;

finalization

if EnginePool <> nil then
  begin
    disposeObject(EnginePool);
    EnginePool := nil;
  end;

disposeObject(TexturePool);
TexturePool := nil;

end.
