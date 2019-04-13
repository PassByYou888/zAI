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

uses Variants, Types, CoreClasses, Geometry2DUnit, Geometry3DUnit, UnicodeMixedLib,
  ListEngine, MemoryRaster, PascalStrings, DataFrameEngine, MemoryStream64, NotifyObjectBase;

type
  TDrawEngine = class;
  TDrawEngine_Raster = class;

  TDEColor = TVec4;
  PDEColor = ^TDEColor;

  TDEVec = TVec2;
  PDEVec = ^TDEVec;

  TDERect = TRectV2;
  PDERect = ^TRectV2;
  TDEFloat = TGeoFloat;

  TDETexture = class(TSequenceMemoryRaster)
  protected
    IsStaticShadow: Boolean;
    FStaticShadow: TDETexture;
    function GetStaticShadow: TDETexture; virtual;
  public
    Name: SystemString;
    constructor Create; override;
    destructor Destroy; override;

    procedure ReleaseFMXResource; virtual;
    procedure FastUpdateTexture; virtual;

    property StaticShadow: TDETexture read GetStaticShadow;
  end;

  TDETextureClass = class of TDETexture;

  TDE4V = record
  public
    Left, Top, Right, Bottom: TDEFloat;
    angle: TDEFloat;

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
    function GetDistance(dest: TDE4V): TDEFloat;
    function GetAngleDistance(dest: TDE4V): TDEFloat;
    function MovementToLerp(dest: TDE4V; mLerp, rLerp: Double): TDE4V;
    function MovementToDistance(dest: TDE4V; mSpeed, rSpeed: TDEFloat): TDE4V;
    function MovementToDistanceCompleteTime(dest: TDE4V; mSpeed, rSpeed: TDEFloat): Double;
    function Fit(dest: TDE4V): TDE4V; overload;
    function Fit(dest: TDERect): TDE4V; overload;
    class function Init(r: TDERect; Ang: TDEFloat): TDE4V; overload; static;
    class function Init(r: TRectf; Ang: TDEFloat): TDE4V; overload; static;
    class function Init(r: TRect; Ang: TDEFloat): TDE4V; overload; static;
    class function Init(CenPos: TDEVec; AWidth, AHeight, Ang: TDEFloat): TDE4V; overload; static;
    class function Init(AWidth, AHeight, Ang: TDEFloat): TDE4V; overload; static;
    class function Init: TDE4V; overload; static;
  end;

  TDrawEngineInterface = class(TCoreClassObject)
  public
    procedure SetSize(r: TDERect); virtual;
    procedure SetLineWidth(w: TDEFloat); virtual;
    procedure DrawLine(pt1, pt2: TDEVec; COLOR: TDEColor); virtual;
    procedure DrawRect(r: TDERect; angle: TDEFloat; COLOR: TDEColor); virtual;
    procedure FillRect(r: TDERect; angle: TDEFloat; COLOR: TDEColor); virtual;
    procedure DrawEllipse(r: TDERect; COLOR: TDEColor); virtual;
    procedure FillEllipse(r: TDERect; COLOR: TDEColor); virtual;
    procedure DrawText(Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean; RotateVec: TDEVec; angle: TDEFloat); virtual;
    procedure DrawTexture(t: TCoreClassObject; sour, dest: TDE4V; alpha: TDEFloat); virtual;
    procedure Flush; virtual;
    procedure ResetState; virtual;
    procedure BeginDraw; virtual;
    procedure EndDraw; virtual;
    function CurrentScreenSize: TDEVec; virtual;
    function GetTextSize(Text: SystemString; Size: TDEFloat): TDEVec; virtual;
    function ReadyOK: Boolean; virtual;
  end;

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
    angle: TDEFloat;
    COLOR: TDEColor;
  end;

  PDrawCommandParam_Rect_Color = ^TDrawCommandParam_Rect_Color;

  TDrawCommandParam_Textout = record
    Text: SystemString;
    Size: TDEFloat;
    r: TDERect;
    COLOR: TDEColor;
    center: Boolean;
    RotateVec: TDEVec;
    angle: TDEFloat;
    bak_r: TDERect;
    bak_color: TDEColor;
  end;

  PDrawCommandParam_Textout = ^TDrawCommandParam_Textout;

  TDrawCommandParam_Texture = record
    t: TCoreClassObject;
    sour, dest: TDE4V;
    alpha: TDEFloat;
    bak_t: TCoreClassObject;
    bak_dest: TDE4V;
    bak_alpha: TDEFloat;
  end;

  PDrawCommandParam_Texture = ^TDrawCommandParam_Texture;

  TCustomDrawMethod = procedure(Sender: TDrawEngine; const UserData: Pointer; const UserObject: TCoreClassObject) of object;

  TDrawCommandParam_UserCustom = record
    OnDraw: TCustomDrawMethod;
    UserData: Pointer;
    UserObject: TCoreClassObject;
  end;

  PDrawCommandParam_UserCustom = ^TDrawCommandParam_UserCustom;

  TDrawCommandType = (dctSetSize, dctSetLineWidth,
    dctLine, dctDrawRect, dctFillRect, dctDrawEllipse, dctFillEllipse,
    dctDrawText, dctDrawTexture, dctUserCustom, dctFlush);

  TDrawExecute = class;

  TDrawCommand = record
    t: TDrawCommandType;
    Data: Pointer;
    procedure DoFreeData;
    procedure Execute(OwnerDrawExecute: TDrawExecute; Draw: TDrawEngineInterface);
    procedure CopyTo(var Dst: TDrawCommand);
  end;

  PDrawCommand = ^TDrawCommand;

  TTextureOutputState = record
    Source: TCoreClassObject;
    SourceRect: TDE4V;
    DestScreen: TDE4V;
    alpha: TDEFloat;
    index: Integer;
  end;

  PTextureOutputState = ^TTextureOutputState;

  TTextureOutputStateBuffer = array of TTextureOutputState;

  TDrawQueue = class(TCoreClassObject)
  protected
    FOwner: TDrawEngine;
    FCommandList: TCoreClassList;

    FStartDrawShadowIndex: Integer;
    FShadowVec: TDEVec;
    FShadowAlpha: TDEFloat;
  public
    constructor Create(AOwner: TDrawEngine);
    destructor Destroy; override;

    procedure Assign(Source: TDrawQueue);
    // queue manager
    procedure Clear(ForceFree: Boolean);
    // post command
    procedure SetSize(r: TDERect);
    procedure SetLineWidth(w: TDEFloat);
    procedure DrawLine(pt1, pt2: TDEVec; COLOR: TDEColor);
    procedure DrawRect(r: TDERect; angle: TDEFloat; COLOR: TDEColor);
    procedure FillRect(r: TDERect; angle: TDEFloat; COLOR: TDEColor);
    procedure DrawEllipse(pt: TDEVec; radius: TDEFloat; COLOR: TDEColor); overload;
    procedure DrawEllipse(r: TDERect; COLOR: TDEColor); overload;
    procedure FillEllipse(pt: TDEVec; radius: TDEFloat; COLOR: TDEColor); overload;
    procedure FillEllipse(r: TDERect; COLOR: TDEColor); overload;
    procedure DrawText(Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean; RotateVec: TDEVec; angle: TDEFloat);
    procedure DrawTexture(t: TCoreClassObject; sour, dest: TDE4V; alpha: TDEFloat);
    procedure DrawUserCustom(const OnDraw: TCustomDrawMethod; const UserData: Pointer; const UserObject: TCoreClassObject);
    procedure Flush;
    procedure BeginCaptureShadow(const OffsetVec: TDEVec; const alpha: TDEFloat);
    procedure EndCaptureShadow;
    procedure BuildTextureOutputState(var buff: TTextureOutputStateBuffer);
    property Owner: TDrawEngine read FOwner;
  end;

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

  TPolyDrawOption = record
    LineColor: TDEColor;
    PointColor: TDEColor;
    LineWidth: TDEFloat;
    PointScreenRadius: TDEFloat;
  end;

  PPolyDrawOption = ^TPolyDrawOption;

  TDrawEngineViewOption = (devpFPS, devpFrameEndge, devpTextBox, devpTextureState);
  TDrawEngineViewOptions = set of TDrawEngineViewOption;

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

  TScrollTextSource = class(TCoreClassObject)
  public
    LifeTime: Double;
    textRectSize: TDEVec;
    TextSize: Integer;
    TextColor: TDEColor;
    Text: SystemString;
    Tag: TCoreClassObject;
  end;

  TEffect = class;

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
    function GetOverAnimationSmoothAlpha(alpha: TDEFloat): TDEFloat;
    function IsOver: Boolean;
    function SequenceIndex: Integer;
    function SequenceFrameRect: TDE4V;
    procedure LoadFromStream(stream: TCoreClassStream);
    procedure SaveToStream(stream: TCoreClassStream);
  end;

  TParticleData = record
    Source: TSequenceAnimationBase;
    Position: TDEVec;
    radius: TDEFloat;
    angle: TDEFloat;
    alpha: TDEFloat;
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

  TGetTexture = procedure(TextureOfName: SystemString; var Texture: TDETexture);

  TDrawEngine = class(TCoreClassObject)
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
    FViewOptions: TDrawEngineViewOptions;
    FLastDrawInfo: SystemString;
    FTextSizeCache: THashList;
    FScrollTextList: TCoreClassListForObj;
    FDownPT, FMovePT, FUpPT: TDEVec;
    FLastAcceptDownUI: TDrawEngine_UIBase;
    FUIList: TCoreClassListForObj;
    FSequenceAnimationBuffer: TCoreClassListForObj;
    FParticleBuffer: TCoreClassListForObj;
    FLastDynamicSeqenceFlag: Cardinal;
    FFPSFontColor: TDEColor;
    FScreenFrameColor: TDEColor;
    FTextureLibrary: THashObjectList;
    FOnGetTexture: TGetTexture;
    FDefaultTexture: TDETexture;

    // user
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
  public
    Scale: TDEFloat;
    Offset: TDEVec;

    constructor Create;
    destructor Destroy; override;

    property ViewOptions: TDrawEngineViewOptions read FViewOptions write FViewOptions;
    property DrawOptions: TDrawEngineViewOptions read FViewOptions write FViewOptions;
    property Options: TDrawEngineViewOptions read FViewOptions write FViewOptions;
    property LastDrawInfo: SystemString read FLastDrawInfo;

    { coordinate }
    function SceneToScreen(pt: TDEVec): TDEVec; overload;
    function SceneToScreen(x, y: TDEFloat): TDEVec; overload;
    function SceneToScreen(r: TDE4V): TDE4V; overload;
    function SceneToScreen(r: TDERect): TDERect; overload;
    function SceneToScreen(r: TV2Rect4): TV2Rect4; overload;
    function ScreenToScene(pt: TDEVec): TDEVec; overload;
    function ScreenToScene(x, y: TDEFloat): TDEVec; overload;
    function ScreenToScene(r: TDERect): TDERect; overload;
    function ScreenToScene(r: TDE4V): TDE4V; overload;
    function ScreenToScene(r: TV2Rect4): TV2Rect4; overload;
    function SceneToScreenDistance(ScenePt1, ScenePt2: TDEVec): TDEFloat;
    function ScreenToSceneDistance(ScreenPt1, ScreenPt2: TDEVec): TDEFloat;
    function ScreenCenterOfWorld: TDEVec;
    function SceneRectFromScreen: TDERect;
    function ScreenRect: TDERect;

    { state }
    property LastDeltaTime: Double read FLastDeltaTime;
    property LastNewTime: Double read FLastNewTime write FLastNewTime;
    function ReadyOK: Boolean;
    property FrameCounterOfPerSec: Double read FFrameCounterOfPerSec;
    property CommandCounterOfPerSec: Double read FCommandCounterOfPerSec;

    { screen }
    procedure SetSize; overload;
    procedure SetSize(w, h: TDEFloat); overload;
    procedure SetSize(siz: TDEVec); overload;
    procedure SetSize(raster: TMemoryRaster); overload;
    procedure SetSizeAndOffset(r: TDERect); overload;
    property width: TDEFloat read FWidth;
    property height: TDEFloat read FHeight;
    function SceneWidth: TDEFloat;
    function SceneHeight: TDEFloat;

    { draw buffer }
    procedure SetDrawBounds(w, h: TDEFloat); overload;
    procedure SetDrawBounds(siz: TDEVec); overload;
    procedure SetDrawBounds(r: TDERect); overload;
    procedure SetDrawBounds(r: TRectf); overload;

    { compute text }
    function GetTextSize(Text: SystemString; Size: TDEFloat): TDEVec;
    function GetTextSizeR(Text: SystemString; Size: TDEFloat): TDERect;

    { scroll text and UI }
    procedure ClearScrollText;
    procedure PostScrollText(LifeTime: Double; Text: SystemString; Size: Integer; COLOR: TDEColor); overload;
    procedure PostScrollText(Tag: TCoreClassObject; LifeTime: Double; Text: SystemString; Size: Integer; COLOR: TDEColor); overload;
    function GetLastPostScrollText: SystemString;
    procedure ClearUI;
    procedure AllUINoVisibled;
    function TapDown(x, y: TDEFloat): Boolean;
    function TapMove(x, y: TDEFloat): Boolean;
    function TapUp(x, y: TDEFloat): Boolean;

    { shadow }
    procedure BeginCaptureShadow(const OffsetVec: TDEVec; const alpha: TDEFloat);
    procedure EndCaptureShadow;
    function CaptureShadow: Boolean;
    function LastCaptureShadowOffsetVec: TDEVec;
    function LastCaptureShadowAlpha: TDEFloat;

    { compute clip rect }
    function ScreenRectInScreen(r: TDERect): Boolean; overload;
    function ScreenRectInScreen(r: TV2Rect4): Boolean; overload;
    function SceneRectInScreen(r: TDERect): Boolean; overload;
    function SceneRectInScreen(r: TV2Rect4): Boolean; overload;

    { custom draw }
    procedure DrawUserCustom(const OnDraw: TCustomDrawMethod; const UserData: Pointer; const UserObject: TCoreClassObject);

    { draw line: vec2List + Poly }
    procedure DrawArrayVec2_Line(SmoothLevel: TDEFloat; arry: TArrayVec2; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawArrayVec2_Line(arry: TArrayVec2; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawArrayVec2_LineInScene(SmoothLevel: TDEFloat; arry: TArrayVec2; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawArrayVec2_LineInScene(arry: TArrayVec2; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawPL(SmoothLevel: TDEFloat; pl: TVec2List; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawPL(pl: TVec2List; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawPLInScene(pl: TVec2List; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawPLInScene(SmoothLevel: TDEFloat; pl: TVec2List; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawPLInScene(pl: TVec2List; ClosedLine: Boolean; opt: TPolyDrawOption); overload;
    procedure DrawPolyInScene(Poly: TPoly; ClosedLine: Boolean; opt: TPolyDrawOption);
    procedure DrawPolyExpandInScene(Poly: TPoly; ExpandDistance: TDEFloat; ClosedLine: Boolean; opt: TPolyDrawOption);

    { draw line }
    procedure DrawLine(pt1, pt2: TDEVec; COLOR: TDEColor; LineWidth: TDEFloat);
    procedure DrawLineInScene(pt1, pt2: TDEVec; COLOR: TDEColor; LineWidth: TDEFloat);

    { draw corner }
    procedure DrawCorner(box: TV2Rect4; COLOR: TDEColor; BoundLineLength, LineWidth: TDEFloat);
    procedure DrawCornerInScene(box: TV2Rect4; COLOR: TDEColor; BoundLineLength, LineWidth: TDEFloat);

    { draw DE4V rect }
    procedure DrawDE4V(d: TDE4V; COLOR: TDEColor; LineWidth: TDEFloat);
    procedure DrawDE4VInScene(d: TDE4V; COLOR: TDEColor; LineWidth: TDEFloat);

    { draw point }
    procedure DrawArrayVec2(arry: TArrayVec2; COLOR: TDEColor; LineLength, LineWidth: TDEFloat);
    procedure DrawArrayVec2InScene(arry: TArrayVec2; COLOR: TDEColor; LineLength, LineWidth: TDEFloat);
    procedure DrawPoint(pt: TDEVec; COLOR: TDEColor; LineLength, LineWidth: TDEFloat);
    procedure DrawPointInScene(pt: TDEVec; COLOR: TDEColor; LineLength, LineWidth: TDEFloat);

    { draw box }
    procedure DrawBox(r: TDERect; angle: TGeoFloat; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawBoxInScene(r: TDERect; angle: TGeoFloat; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawBox(r: TDERect; COLOR: TDEColor; LineWidth: TDEFloat); overload;
    procedure DrawBoxInScene(r: TDERect; COLOR: TDEColor; LineWidth: TDEFloat); overload;

    { fill box }
    procedure FillBox(r: TDERect; angle: TGeoFloat; COLOR: TDEColor); overload;
    procedure FillBoxInScene(r: TDERect; angle: TGeoFloat; COLOR: TDEColor); overload;
    procedure FillBox(r: TDERect; COLOR: TDEColor); overload;
    procedure FillBoxInScene(r: TDERect; COLOR: TDEColor); overload;

    { draw ellipse }
    procedure DrawEllipse(pt: TDEVec; radius: TDEFloat; COLOR: TDEColor); overload;
    procedure DrawEllipse(r: TDERect; COLOR: TDEColor); overload;
    procedure DrawEllipseInScene(pt: TDEVec; radius: TDEFloat; COLOR: TDEColor); overload;
    procedure DrawEllipseInScene(r: TDERect; COLOR: TDEColor); overload;

    { fill ellipse }
    procedure FillEllipse(pt: TDEVec; radius: TDEFloat; COLOR: TDEColor); overload;
    procedure FillEllipse(r: TDERect; COLOR: TDEColor); overload;
    procedure FillEllipseInScene(pt: TDEVec; radius: TDEFloat; COLOR: TDEColor); overload;
    procedure FillEllipseInScene(r: TDERect; COLOR: TDEColor); overload;

    { draw text }
    procedure DrawText(Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean; RotateVec: TDEVec; angle: TDEFloat); overload;
    procedure DrawText(Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean); overload;
    procedure DrawText(Text: SystemString; Size: TDEFloat; COLOR: TDEColor; ScreenPt: TDEVec); overload;
    { draw text in scene }
    procedure DrawTextInScene(Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean; RotateVec: TDEVec; angle: TDEFloat); overload;
    procedure DrawTextInScene(Text: SystemString; Size: TDEFloat; COLOR: TDEColor; ScenePos: TDEVec); overload;
    procedure DrawTextInScene(Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean); overload;

    { draw texture }
    procedure DrawTexture(t: TCoreClassObject; sour, DestScreen: TDE4V; alpha: TDEFloat); overload;
    procedure DrawTexture(t: TCoreClassObject; sour: TDERect; DestScreen: TDE4V; alpha: TDEFloat); overload;
    procedure DrawTexture(t: TCoreClassObject; sour, DestScreen: TDERect; alpha: TDEFloat); overload;
    procedure DrawTexture(t: TCoreClassObject; sour: TDERect; destScreenPt: TDEVec; angle, alpha: TDEFloat); overload;
    procedure DrawTexture(t: TCoreClassObject; sour, DestScreen: TDERect; angle, alpha: TDEFloat); overload;
    function DrawTexture(indentEndge: Boolean; t: TCoreClassObject; sour, DestScreen: TDERect; alpha: TDEFloat): TDERect; overload;
    { fit draw texture }
    procedure FitDrawTexture(t: TCoreClassObject; sour, destScene: TDERect; angle, alpha: TDEFloat); overload;
    function FitDrawTexture(t: TCoreClassObject; sour, destScene: TDERect; alpha: TDEFloat): TDERect; overload;
    function FitDrawTexture(indentEndge: Boolean; t: TCoreClassObject; sour, destScene: TDERect; alpha: TDEFloat): TDERect; overload;
    { draw texture in scene }
    procedure DrawTextureInScene(t: TCoreClassObject; sour, destScene: TDE4V; alpha: TDEFloat); overload;
    procedure DrawTextureInScene(t: TCoreClassObject; sour: TDERect; destScene: TDE4V; alpha: TDEFloat); overload;
    procedure DrawTextureInScene(t: TCoreClassObject; destScene: TDE4V; alpha: TDEFloat); overload;
    procedure DrawTextureInScene(t: TCoreClassObject; sour, destScene: TDERect; alpha: TDEFloat); overload;
    procedure DrawTextureInScene(t: TCoreClassObject; sour: TDERect; destScenePt: TDEVec; angle, alpha: TDEFloat); overload;
    procedure DrawTextureInScene(t: TCoreClassObject; sour, destScene: TDERect; angle, alpha: TDEFloat); overload;
    procedure DrawTextureInScene(t: TCoreClassObject; destScenePt: TDEVec; AWidth, AHeight, angle, alpha: TDEFloat); overload;
    function DrawTextureInScene(indentEndge: Boolean; t: TCoreClassObject; sour, destScene: TDERect; alpha: TDEFloat): TDERect; overload;
    { fit draw texture in scene }
    procedure FitDrawTextureInScene(t: TCoreClassObject; sour, destScene: TDERect; angle, alpha: TDEFloat); overload;
    function FitDrawTextureInScene(t: TCoreClassObject; sour, destScene: TDERect; alpha: TDEFloat): TDERect; overload;
    function FitDrawTextureInScene(indentEndge: Boolean; t: TCoreClassObject; sour, destScene: TDERect; alpha: TDEFloat): TDERect; overload;
    { draw texture packing in scene }
    function DrawTexturePackingInScene(packing_t: TMemoryRasterList; Margins: TGeoFloat; destOffset: TDEVec; alpha: TDEFloat): TRectV2;

    { sequence animation }
    function CreateSequenceAnimation(stream: TCoreClassStream): TSequenceAnimationBase; overload;
    function GetOrCreateSequenceAnimation(flag: Variant; t: TCoreClassObject): TSequenceAnimationBase;
    function SequenceAnimationPlaying(flag: Variant; t: TCoreClassObject): Boolean;
    function SequenceAnimationIsOver(flag: Variant; t: TCoreClassObject): Boolean;
    function ExistsSequenceAnimation(SA: TSequenceAnimationBase): Boolean;
    function GetNewSequenceFlag: Variant;
    function ManualDrawSequenceTexture(flag: Variant; t: TCoreClassObject; TextureWidth, TextureHeight, Total, Column: Integer; CompleteTime: Double; Looped: Boolean;
      DestScreen: TDE4V; alpha: TDEFloat): TSequenceAnimationBase; virtual;
    function DrawSequenceTexture(flag: Variant; t: TCoreClassObject; TextureWidth, TextureHeight, Total, Column: Integer; CompleteTime: Double; Looped: Boolean;
      DestScreen: TDE4V; alpha: TDEFloat): TSequenceAnimationBase; overload;
    function DrawSequenceTexture(flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; DestScreen: TDE4V; alpha: TDEFloat): TSequenceAnimationBase; overload;
    function DrawSequenceTexture(flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; DestScreen: TDERect; alpha: TDEFloat): TSequenceAnimationBase; overload;
    function DrawSequenceTexture(flag: Variant; t: TDETexture; CompleteTime: Double; DestScreen: TDE4V; alpha: TDEFloat): TSequenceAnimationBase; overload;
    function FitDrawSequenceTexture(flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; DestScreen: TDERect; alpha: TDEFloat): TDERect; overload;
    function FitDrawSequenceTexture(indentEndge: Boolean; flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; DestScreen: TDERect; alpha: TDEFloat): TDERect; overload;
    procedure DrawSequenceTexture(SA: TSequenceAnimationBase; DestScreen: TDE4V; alpha: TDEFloat); overload;
    function DrawSequenceTextureInScene(flag: Variant; t: TCoreClassObject; TextureWidth, TextureHeight, Total, Column: Integer; CompleteTime: Double; Looped: Boolean;
      destScene: TDE4V; alpha: TDEFloat): TSequenceAnimationBase; overload;
    function DrawSequenceTextureInScene(flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; destScene: TDE4V; alpha: TDEFloat): TSequenceAnimationBase; overload;
    function DrawSequenceTextureInScene(flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; destScene: TDERect; alpha: TDEFloat): TSequenceAnimationBase; overload;
    function DrawSequenceTextureInScene(flag: Variant; t: TDETexture; CompleteTime: Double; destScene: TDE4V; alpha: TDEFloat): TSequenceAnimationBase; overload;
    function FitDrawSequenceTextureInScene(flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; destScene: TDERect; alpha: TDEFloat): TDERect; overload;
    function FitDrawSequenceTextureInScene(indentEndge: Boolean; flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; destScene: TDERect; alpha: TDEFloat): TDERect; overload;
    procedure DrawSequenceTextureInScene(SA: TSequenceAnimationBase; destScene: TDE4V; alpha: TDEFloat); overload;

    { particles }
    function CreateParticles: TParticles; overload;
    function CreateParticles(stream: TCoreClassStream): TParticles; overload;
    procedure DeleteParticles(p: TParticles);
    procedure FreeAndDeleteParticles(p: TParticles);
    procedure ClearParticles;
    function ExistsParticles(p: TParticles): Boolean;
    function TotalParticleData: Integer;
    function ParticleCount: Integer;
    function GetParticles(const index: Integer): TParticles;
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

    { cadencer progress }
    procedure Progress(deltaTime: Double); virtual;

    { build-in rasterization }
    property Rasterization: TDrawEngine_Raster read FRasterization;
    { draw interface }
    property DrawInterface: TDrawEngineInterface read FDrawInterface write SetDrawInterface;
    { preset container }
    property DrawCommand: TDrawQueue read FDrawCommand;
    property DrawExecute: TDrawExecute read FDrawExecute;

    { misc }
    property FPSFontColor: TDEColor read FFPSFontColor write FFPSFontColor;
    property ScreenFrameColor: TDEColor read FScreenFrameColor write FScreenFrameColor;

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

  PDrawEnginePoolData = ^TDrawEnginePoolData;

  TDrawEnginePoolData = record
    DrawEng: TDrawEngine;
    workObj: TCoreClassObject;
    LastActivted: Cardinal;
  end;

  TDrawEngineClass = class of TDrawEngine;

  TDrawEnginePool = class(TCoreClassPersistent)
  protected
    FDefaultDrawEngineClass: TDrawEngineClass;
    FDrawEngineList: TCoreClassList;
    FPostProgress: TNProgressPost;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure ClearActivtedTimeOut(tickLen: Cardinal);

    procedure Progress(deltaTime: Double);

    property DefaultDrawEngineClass: TDrawEngineClass read FDefaultDrawEngineClass write FDefaultDrawEngineClass;

    function GetEng(const workObj: TCoreClassObject; const Draw: TDrawEngineInterface): TDrawEngine; overload;
    function GetEng(const workObj: TCoreClassObject): TDrawEngine; overload;

    // delay run support
    property ProgressEngine: TNProgressPost read FPostProgress;
    property ProgressPost: TNProgressPost read FPostProgress;
    property PostProgress: TNProgressPost read FPostProgress;
    property PostRun: TNProgressPost read FPostProgress;
    property PostExecute: TNProgressPost read FPostProgress;
  end;

  TDrawEngine_Raster = class(TDrawEngineInterface)
  private
    FMemory: TDETexture;
    FUsedAgg: Boolean;
    FEngine: TDrawEngine;
    FFreeEngine: Boolean;
    function DEColor2RasterColor(const COLOR: TDEColor): TRasterColor; overload;
    function DEColor2RasterColor(const COLOR: TDEColor; const alpha: Byte): TRasterColor; overload;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetSize(r: TDERect); override;
    procedure SetLineWidth(w: TDEFloat); override;
    procedure DrawLine(pt1, pt2: TDEVec; COLOR: TDEColor); override;
    procedure DrawRect(r: TDERect; angle: TDEFloat; COLOR: TDEColor); override;
    procedure FillRect(r: TDERect; angle: TDEFloat; COLOR: TDEColor); override;
    procedure DrawEllipse(r: TDERect; COLOR: TDEColor); override;
    procedure FillEllipse(r: TDERect; COLOR: TDEColor); override;
    procedure DrawText(Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean; RotateVec: TDEVec; angle: TDEFloat); override;
    procedure DrawTexture(t: TCoreClassObject; sour, dest: TDE4V; alpha: TDEFloat); override;
    procedure Flush; override;
    procedure ResetState; override;
    procedure BeginDraw; override;
    procedure EndDraw; override;
    function CurrentScreenSize: TDEVec; override;
    function GetTextSize(Text: SystemString; Size: TDEFloat): TDEVec; override;
    function ReadyOK: Boolean; override;
  public
    property Memory: TDETexture read FMemory;
    property UsedAgg: Boolean read FUsedAgg write FUsedAgg;
    function Engine: TDrawEngine;
    procedure SetWorkMemory(m: TMemoryRaster);
  end;

const
  NULLVec: TDEVec = (0, 0);

function DrawPool(workObj: TCoreClassObject; Draw: TDrawEngineInterface): TDrawEngine; overload;
function DrawPool(workObj: TCoreClassObject): TDrawEngine; overload;

function DEVec(x, y: TDEFloat): TDEVec; overload;
function DEVec(pt: TPointf): TDEVec; overload;
function DEColorInv(const COLOR: TDEColor): TDEColor; overload;
function DEColorInv(const r, g, b, a: TDEFloat): TDEColor; overload;
function DEColor(const r, g, b, a: TDEFloat): TDEColor; overload;
function DEColor(const r, g, b: TDEFloat): TDEColor; overload;
function DEColor(const c: TDEColor; const alpha: TDEFloat): TDEColor; overload;
function DEColor2RasterColor(const c: TDEColor): TRasterColor;
function DEAlpha(c: TDEColor): TDEFloat;
function DERect(const x, y, radius: TDEFloat): TDERect; overload;
function DERect(const x1, y1, x2, y2: TDEFloat): TDERect; overload;
function DERect(const p1, p2: T2DPoint): TDERect; overload;
function DERect(const x, y: TDEFloat; const p2: T2DPoint): TDERect; overload;
function DERect(const Rect: TRect): TDERect; overload;
function DERect(const Rect: TRectf): TDERect; overload;

function Interval2Delta(interval: Integer): Double;

procedure FitScale(const sour, dest: TDERect; var outOffset: TDEVec; var outScale: TDEFloat); overload;
procedure FitScale(const sour: TDERect; const DestWidth, DestHeight: TDEFloat; var outOffset: TDEVec; var outScale: TDEFloat); overload;
procedure FitScale(const sour: TRectf; const DestWidth, DestHeight: TDEFloat; var outOffset: TDEVec; var outScale: TDEFloat); overload;
procedure FitScale(const sour, dest: TRectf; var outOffset: TDEVec; var outScale: TDEFloat); overload;
procedure FitScale(const sourWidth, sourHeight, DestWidth, DestHeight: TDEFloat; var outOffset: TDEVec; var outScale: TDEFloat); overload;

var
  DefaultTextureClass: TDETextureClass = TDETexture;
  EnginePool: TDrawEnginePool = nil;

implementation

uses Math;

function DrawPool(workObj: TCoreClassObject; Draw: TDrawEngineInterface): TDrawEngine;
begin
  Result := EnginePool.GetEng(workObj, Draw);
end;

function DrawPool(workObj: TCoreClassObject): TDrawEngine;
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

function DEColor(const c: TDEColor; const alpha: TDEFloat): TDEColor;
begin
  Result := c;
  Result[3] := alpha;
end;

function DEColor2RasterColor(const c: TDEColor): TRasterColor;
begin
  Result := RasterColorF(c[0], c[1], c[2], c[3]);
end;

function DEAlpha(c: TDEColor): TDEFloat;
begin
  Result := c[3];
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

procedure FitScale(const sour, dest: TDERect; var outOffset: TDEVec; var outScale: TDEFloat);
var
  r: TDERect;
begin
  // compute scale
  r := RectFit(sour, dest);
  outScale := RectWidth(r) / RectWidth(sour);
  outOffset := r[0];
end;

procedure FitScale(const sour: TDERect; const DestWidth, DestHeight: TDEFloat; var outOffset: TDEVec; var outScale: TDEFloat);
begin
  FitScale(sour, DERect(0, 0, DestWidth, DestHeight), outOffset, outScale);
end;

procedure FitScale(const sour: TRectf; const DestWidth, DestHeight: TDEFloat; var outOffset: TDEVec; var outScale: TDEFloat);
begin
  FitScale(DERect(sour), DERect(0, 0, DestWidth, DestHeight), outOffset, outScale);
end;

procedure FitScale(const sour, dest: TRectf; var outOffset: TDEVec; var outScale: TDEFloat);
begin
  FitScale(DERect(sour), DERect(dest), outOffset, outScale);
end;

procedure FitScale(const sourWidth, sourHeight, DestWidth, DestHeight: TDEFloat; var outOffset: TDEVec; var outScale: TDEFloat);
begin
  FitScale(DERect(0, 0, sourWidth, sourHeight), DERect(0, 0, DestWidth, DestHeight), outOffset, outScale);
end;

function TDETexture.GetStaticShadow: TDETexture;
var
  i: Integer;
  p1, p2: PRasterColorEntry;
begin
  if FStaticShadow = nil then
    begin
      if not IsStaticShadow then
        begin
          FStaticShadow := DefaultTextureClass.Create;
          FStaticShadow.IsStaticShadow := True;
          FStaticShadow.SetSize(width, height);
          for i := (width * height) - 1 downto 0 do
            begin
              p1 := @Bits^[i];
              p2 := @FStaticShadow.Bits^[i];
              p2^.BGRA := RasterColor(0, 0, 0, p1^.a);
            end;
        end
      else
          Result := Self;
    end;
  Result := FStaticShadow;
end;

constructor TDETexture.Create;
begin
  inherited Create;
  FStaticShadow := nil;
  IsStaticShadow := False;
end;

destructor TDETexture.Destroy;
begin
  if FStaticShadow <> nil then
      DisposeObject(FStaticShadow);
  inherited Destroy;
end;

procedure TDETexture.ReleaseFMXResource;
begin
end;

procedure TDETexture.FastUpdateTexture;
begin
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
  Result := Geometry2DUnit.TV2Rect4.Init(MakeRectV2, angle).BoundRect;
end;

function TDE4V.Centroid: TDEVec;
begin
  Result := Geometry2DUnit.TV2Rect4.Init(MakeRectV2, angle).Centroid;
end;

function TDE4V.Add(v: TDEVec): TDE4V;
var
  r: TDERect;
begin
  r := MakeRectV2;
  r[0] := Vec2Add(r[0], v);
  r[1] := Vec2Add(r[1], v);
  Result := Init(r, angle);
end;

function TDE4V.Add(x, y: TDEFloat): TDE4V;
var
  r: TDERect;
begin
  r := MakeRectV2;
  r[0] := Vec2Add(r[0], x, y);
  r[1] := Vec2Add(r[1], x, y);
  Result := Init(r, angle);
end;

function TDE4V.Scale(f: TDEFloat): TDE4V;
begin
  Result.Left := Left * f;
  Result.Top := Top * f;
  Result.Right := Right * f;
  Result.Bottom := Bottom * f;
  Result.angle := angle;
end;

function TDE4V.GetDistance(dest: TDE4V): TDEFloat;
begin
  Result := Geometry3DUnit.Distance(MakeRectV2, dest.MakeRectV2);
end;

function TDE4V.GetAngleDistance(dest: TDE4V): TDEFloat;
begin
  Result := Geometry3DUnit.AngleDistance(angle, dest.angle);
end;

function TDE4V.MovementToLerp(dest: TDE4V; mLerp, rLerp: Double): TDE4V;
var
  r: TDERect;
begin
  Result.angle := MovementLerp(angle, dest.angle, rLerp);

  r := MovementLerp(MakeRectV2, dest.MakeRectV2, mLerp);
  Result.Left := r[0][0];
  Result.Top := r[0][1];
  Result.Right := r[1][0];
  Result.Bottom := r[1][1];
end;

function TDE4V.MovementToDistance(dest: TDE4V; mSpeed, rSpeed: TDEFloat): TDE4V;
var
  r: TDERect;
begin
  Result.angle := SmoothAngle(angle, dest.angle, rSpeed);

  r := MovementDistance(MakeRectV2, dest.MakeRectV2, mSpeed);
  Result.Left := r[0][0];
  Result.Top := r[0][1];
  Result.Right := r[1][0];
  Result.Bottom := r[1][1];
end;

function TDE4V.MovementToDistanceCompleteTime(dest: TDE4V; mSpeed, rSpeed: TDEFloat): Double;
var
  d1, d2: Double;
begin
  d1 := Geometry3DUnit.AngleRollDistanceDeltaTime(angle, dest.angle, rSpeed);
  d2 := Geometry3DUnit.MovementDistanceDeltaTime(MakeRectV2, dest.MakeRectV2, mSpeed);
  if d1 > d2 then
      Result := d1
  else
      Result := d2;
end;

function TDE4V.Fit(dest: TDE4V): TDE4V;
var
  r: TDERect;
begin
  r := RectFit(dest.MakeRectV2, MakeRectV2);
  Result.angle := angle;
  Result.Left := r[0][0];
  Result.Top := r[0][1];
  Result.Right := r[1][0];
  Result.Bottom := r[1][1];
end;

function TDE4V.Fit(dest: TDERect): TDE4V;
var
  r: TDERect;
begin
  r := RectFit(dest, MakeRectV2);
  Result.angle := angle;
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
      angle := Ang;
    end;
end;

class function TDE4V.Init(r: TRectf; Ang: TDEFloat): TDE4V;
begin
  Result := Init(DERect(r), Ang);
end;

class function TDE4V.Init(r: TRect; Ang: TDEFloat): TDE4V;
begin
  Result := Init(DERect(r), Ang);
end;

class function TDE4V.Init(CenPos: TDEVec; AWidth, AHeight, Ang: TDEFloat): TDE4V;
var
  r: TDERect;
begin
  r[0][0] := CenPos[0] - AWidth * 0.5;
  r[0][1] := CenPos[1] - AHeight * 0.5;
  r[1][0] := CenPos[0] + AWidth * 0.5;
  r[1][1] := CenPos[1] + AHeight * 0.5;
  Result := Init(r, Ang);
end;

class function TDE4V.Init(AWidth, AHeight, Ang: TDEFloat): TDE4V;
begin
  Result := Init(DERect(0, 0, AWidth, AHeight), Ang);
end;

class function TDE4V.Init: TDE4V;
begin
  Result := Init(ZeroRect, 0);
end;

procedure TDrawEngineInterface.SetSize(r: TDERect);
begin

end;

procedure TDrawEngineInterface.SetLineWidth(w: TDEFloat);
begin

end;

procedure TDrawEngineInterface.DrawLine(pt1, pt2: TDEVec; COLOR: TDEColor);
begin

end;

procedure TDrawEngineInterface.DrawRect(r: TDERect; angle: TDEFloat; COLOR: TDEColor);
begin

end;

procedure TDrawEngineInterface.FillRect(r: TDERect; angle: TDEFloat; COLOR: TDEColor);
begin

end;

procedure TDrawEngineInterface.DrawEllipse(r: TDERect; COLOR: TDEColor);
begin

end;

procedure TDrawEngineInterface.FillEllipse(r: TDERect; COLOR: TDEColor);
begin

end;

procedure TDrawEngineInterface.DrawText(Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean; RotateVec: TDEVec; angle: TDEFloat);
begin

end;

procedure TDrawEngineInterface.DrawTexture(t: TCoreClassObject; sour, dest: TDE4V; alpha: TDEFloat);
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

function TDrawEngineInterface.GetTextSize(Text: SystemString; Size: TDEFloat): TDEVec;
begin
  Result := NULLVec;
end;

function TDrawEngineInterface.ReadyOK: Boolean;
begin
  Result := False;
end;

procedure TDrawCommand.DoFreeData;
begin
  case t of
    dctSetLineWidth: Dispose(PDrawCommandParam_1Float(Data));
    dctLine: Dispose(PDrawCommandParam_PT_Color(Data));
    dctSetSize: Dispose(PDrawCommandParam_1Rect(Data));
    dctDrawRect, dctFillRect, dctDrawEllipse, dctFillEllipse: Dispose(PDrawCommandParam_Rect_Color(Data));
    dctDrawText: Dispose(PDrawCommandParam_Textout(Data));
    dctDrawTexture: Dispose(PDrawCommandParam_Texture(Data));
    dctUserCustom: Dispose(PDrawCommandParam_UserCustom(Data));
  end;
end;

procedure TDrawCommand.Execute(OwnerDrawExecute: TDrawExecute; Draw: TDrawEngineInterface);
begin
  case t of
    dctSetSize: with PDrawCommandParam_1Rect(Data)^ do
          Draw.SetSize(r);
    dctSetLineWidth: with PDrawCommandParam_1Float(Data)^ do
          Draw.SetLineWidth(f);
    dctLine: with PDrawCommandParam_PT_Color(Data)^ do
          Draw.DrawLine(pt1, pt2, COLOR);
    dctDrawRect: with PDrawCommandParam_Rect_Color(Data)^ do
          Draw.DrawRect(r, angle, COLOR);
    dctFillRect: with PDrawCommandParam_Rect_Color(Data)^ do
          Draw.FillRect(r, angle, COLOR);
    dctDrawEllipse: with PDrawCommandParam_Rect_Color(Data)^ do
          Draw.DrawEllipse(r, COLOR);
    dctFillEllipse: with PDrawCommandParam_Rect_Color(Data)^ do
          Draw.FillEllipse(r, COLOR);
    dctDrawText: with PDrawCommandParam_Textout(Data)^ do
          Draw.DrawText(Text, Size, r, COLOR, center, RotateVec, angle);
    dctDrawTexture: with PDrawCommandParam_Texture(Data)^ do
          Draw.DrawTexture(t, sour, dest, alpha);
    dctUserCustom: with PDrawCommandParam_UserCustom(Data)^ do
          OnDraw(OwnerDrawExecute.FOwner, UserData, UserObject);
    dctFlush: Draw.Flush;
  end;
end;

procedure TDrawCommand.CopyTo(var Dst: TDrawCommand);
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
    dctDrawText:
      begin
        new(PDrawCommandParam_Textout(Dst.Data));
        PDrawCommandParam_Textout(Dst.Data)^ := PDrawCommandParam_Textout(Data)^;
      end;
    dctDrawTexture:
      begin
        new(PDrawCommandParam_Texture(Dst.Data));
        PDrawCommandParam_Texture(Dst.Data)^ := PDrawCommandParam_Texture(Data)^;
      end;
    dctUserCustom:
      begin
        new(PDrawCommandParam_UserCustom(Dst.Data));
        PDrawCommandParam_UserCustom(Dst.Data)^ := PDrawCommandParam_UserCustom(Data)^;
      end;
  end;
end;

constructor TDrawQueue.Create(AOwner: TDrawEngine);
begin
  inherited Create;
  FOwner := AOwner;
  FCommandList := TCoreClassList.Create;
  FStartDrawShadowIndex := -1;
  FShadowVec := NULLVec;
  FShadowAlpha := 0.5;
end;

destructor TDrawQueue.Destroy;
begin
  Clear(True);
  DisposeObject(FCommandList);
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
  for i := 0 to FCommandList.Count - 1 do
    begin
      p := FCommandList[i];
      if ForceFree then
          p^.DoFreeData;
      Dispose(p);
    end;
  FCommandList.Clear;
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

procedure TDrawQueue.DrawRect(r: TDERect; angle: TDEFloat; COLOR: TDEColor);
var
  p: PDrawCommand;
  Data: PDrawCommandParam_Rect_Color;
begin
  if DEAlpha(COLOR) > 0 then
    begin
      new(p);
      new(Data);

      Data^.r := r;
      Data^.angle := angle;
      Data^.COLOR := COLOR;

      p^.t := dctDrawRect;
      p^.Data := Data;

      LockObject(FCommandList);
      FCommandList.Add(p);
      UnLockObject(FCommandList);
    end;
end;

procedure TDrawQueue.FillRect(r: TDERect; angle: TDEFloat; COLOR: TDEColor);
var
  p: PDrawCommand;
  Data: PDrawCommandParam_Rect_Color;
begin
  if DEAlpha(COLOR) > 0 then
    begin
      new(p);
      new(Data);

      Data^.r := r;
      Data^.angle := angle;
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

procedure TDrawQueue.DrawText(Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean; RotateVec: TDEVec; angle: TDEFloat);
var
  PrepareDraw: Boolean;
  p: PDrawCommand;
  Data: PDrawCommandParam_Textout;
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
          Data^.angle := angle;

          Data^.bak_r := Data^.r;
          Data^.bak_color := Data^.COLOR;

          p^.t := dctDrawText;
          p^.Data := Data;

          if (FStartDrawShadowIndex >= 0) then
            begin
              Data^.r := Geometry2DUnit.RectOffset(Data^.r, FShadowVec);
              Data^.COLOR := DEColor(0, 0, 0, Data^.COLOR[3] * FShadowAlpha);
            end;

          LockObject(FCommandList);
          FCommandList.Add(p);
          UnLockObject(FCommandList);
        end;
    end;
end;

procedure TDrawQueue.DrawTexture(t: TCoreClassObject; sour, dest: TDE4V; alpha: TDEFloat);
var
  PrepareDraw: Boolean;
  p: PDrawCommand;
  Data: PDrawCommandParam_Texture;
  r: TDERect;
begin
  if alpha > 0 then
    begin
      r := dest.BoundRect;
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
          Data^.sour := sour;
          Data^.dest := dest;
          Data^.alpha := alpha;

          Data^.bak_t := t;
          Data^.bak_dest := dest;
          Data^.bak_alpha := alpha;

          p^.t := dctDrawTexture;
          p^.Data := Data;

          if (FStartDrawShadowIndex >= 0) and (Data^.t is TDETexture) and (not TDETexture(Data^.t).IsStaticShadow) then
            begin
              Data^.t := TDETexture(Data^.t).GetStaticShadow;
              Data^.dest := Data^.dest.Add(FShadowVec);
              Data^.alpha := Data^.alpha * FShadowAlpha;
            end;

          LockObject(FCommandList);
          FCommandList.Add(p);
          UnLockObject(FCommandList);
        end;
    end;
end;

procedure TDrawQueue.DrawUserCustom(const OnDraw: TCustomDrawMethod; const UserData: Pointer; const UserObject: TCoreClassObject);
var
  p: PDrawCommand;
  Data: PDrawCommandParam_UserCustom;
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

procedure TDrawQueue.BeginCaptureShadow(const OffsetVec: TDEVec; const alpha: TDEFloat);
begin
  EndCaptureShadow;
  FStartDrawShadowIndex := FCommandList.Count;
  FShadowVec := OffsetVec;
  FShadowAlpha := alpha;
end;

procedure TDrawQueue.EndCaptureShadow;
var
  i, b: Integer;
  lst: TCoreClassList;

  p: PDrawCommand;
  pTextureData: PDrawCommandParam_Texture;
  pTextData: PDrawCommandParam_Textout;
begin
  if FStartDrawShadowIndex >= 0 then
    begin
      i := FStartDrawShadowIndex;
      b := FStartDrawShadowIndex;
      FStartDrawShadowIndex := -1;

      lst := TCoreClassList.Create;

      while i < FCommandList.Count do
        begin
          p := PDrawCommand(FCommandList[i]);
          if (p^.t = dctDrawTexture) and (PDrawCommandParam_Texture(p^.Data)^.t is TDETexture) and
            (TDETexture(PDrawCommandParam_Texture(p^.Data)^.t).IsStaticShadow) then
            begin
              new(pTextureData);
              pTextureData^ := PDrawCommandParam_Texture(p^.Data)^;
              pTextureData^.t := pTextureData^.bak_t;
              pTextureData^.dest := pTextureData^.bak_dest;
              pTextureData^.alpha := pTextureData^.bak_alpha;

              new(p);
              p^.t := dctDrawTexture;
              p^.Data := pTextureData;
              lst.Add(p);
              inc(i);
            end
          else if (p^.t = dctDrawText) then
            begin
              new(pTextData);
              pTextData^ := PDrawCommandParam_Textout(p^.Data)^;
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
              FCommandList.Delete(i);
            end;
        end;

      LockObject(FCommandList);
      for i := 0 to lst.Count - 1 do
          FCommandList.Add(lst[i]);
      UnLockObject(FCommandList);

      DisposeObject(lst);
    end;
end;

procedure TDrawQueue.BuildTextureOutputState(var buff: TTextureOutputStateBuffer);
var
  i, j: Integer;
  p: PDrawCommand;
  ptex: PDrawCommandParam_Texture;
begin
  try
    j := 0;

    for i := 0 to FCommandList.Count - 1 do
      if PDrawCommand(FCommandList[i])^.t = dctDrawTexture then
          inc(j);

    SetLength(buff, j);

    j := 0;
    for i := 0 to FCommandList.Count - 1 do
      begin
        p := PDrawCommand(FCommandList[i]);
        if p^.t = dctDrawTexture then
          begin
            ptex := PDrawCommandParam_Texture(p^.Data);
            buff[j].Source := ptex^.t;
            buff[j].SourceRect := ptex^.sour;
            buff[j].DestScreen := ptex^.dest;
            buff[j].alpha := ptex^.alpha;
            buff[j].index := i;

            inc(j);
          end;
      end;
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

  DisposeObject(FCommandList);
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
  try
    for i := 0 to FCommandList.Count - 1 do
        PDrawCommand(FCommandList[i])^.Execute(Self, Draw);
  except
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
          Owner.FUIList.Delete(i)
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
  c: TDEColor;
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
  c := DEColor(0, 0, 0, 0.0);
  Owner.FDrawCommand.FillRect(r, 0, c);
  c := DEColor(1, 1, 1, 1);
  Owner.FDrawCommand.DrawRect(r, 0, c);
  Owner.FDrawCommand.DrawText(Text, TextSize, r, c, True, DEVec(0.5, 0.5), 0);
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

function TSequenceAnimationBase.GetOverAnimationSmoothAlpha(alpha: TDEFloat): TDEFloat;
var
  v: TDEFloat;
begin
  if SequenceAnimationPlaying then
      Result := alpha
  else
    begin
      v := CurrentTime - CompleteTime;
      if (v > OverAnimationSmoothTime) then
          Result := 0.0
      else if v > 0 then
        begin
          Result := alpha - (1.0 / (OverAnimationSmoothTime / v));
          if Result < 0 then
              Result := 0.0;
        end
      else
          Result := alpha;
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
  AWidth, AHeight: Integer;
begin
  if Total <= 1 then
      Exit(TDE4V.Init(width, height, 0));

  if Column > Total then
      Column := Total;

  idx := SequenceIndex;
  colIdx := idx mod Column;
  rowIdx := idx div Column;
  Row := Total div Column;
  if Total mod Column > 0 then
      inc(Row);

  AWidth := width div Column;
  AHeight := height div Row;

  Result := TDE4V.Init(Rect(colIdx * AWidth, rowIdx * AHeight, colIdx * AWidth + AWidth, rowIdx * AHeight + AHeight), 0);
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

  DisposeObject(df);
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
  DisposeObject(df);
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

          p^.radius := ParticleSize * umlRandomRangeS(0.4, 0.6);
          p^.angle := 0;
          p^.alpha := MaxAlpha;
          p^.CurrentTime := 0;
          p^.Acceleration := 0;

          ParticleBuff.Add(p);
        end;
    end;

  while ParticleBuff.Count > MaxParticle do
    begin
      p := ParticleBuff[ParticleBuff.Count - 1];
      Dispose(p);
      ParticleBuff.Delete(ParticleBuff.Count - 1);
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
          ParticleBuff.Delete(i);
        end
      else
        begin
          p^.Acceleration := p^.Acceleration + DispersionAcceleration * deltaTime;
          k := Vec2Distance(ZeroPoint, Dispersion);
          p^.Position := MovementDistance(p^.Position, Vec2Add(p^.Position, Dispersion), (k + k * p^.Acceleration) * deltaTime);
          p^.alpha := Clamp(MaxAlpha - (p^.CurrentTime / LifeTime) * MaxAlpha, MinAlpha, 1.0);
          p^.angle := p^.angle + deltaTime * RotationOfSecond;
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
  DisposeObject(ParticleBuff);

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

  DisposeObject(df);
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
  DisposeObject(df);
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
      DisposeObject(Particle);
  if SequenceAnimation <> nil then
      DisposeObject(SequenceAnimation);
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
  DisposeObject(ms);

  DisposeObject(df);
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
  DisposeObject(ms);

  df.SaveToStream(stream);

  DisposeObject(df);
end;

procedure TEffect.Draw(Pos: TDEVec);
begin
  case Mode of
    emSequenceAnimation:
      begin
        if SequenceAnimation = nil then
            Exit;
        Owner.DrawSequenceTexture(SequenceAnimation,
          TDE4V.Init(MakeRectV2(
          Pos[0] - SequenceAnimation_Width * 0.5, Pos[1] - SequenceAnimation_Height * 0.5,
          Pos[0] + SequenceAnimation_Width * 0.5, Pos[1] + SequenceAnimation_Height * 0.5), SequenceAnimation_Angle), SequenceAnimation_Alpha);
      end;
    emParticle:
      begin
        if Particle = nil then
            Exit;
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
            Exit;
        Owner.DrawSequenceTextureInScene(SequenceAnimation,
          TDE4V.Init(MakeRectV2(
          Pos[0] - SequenceAnimation_Width * 0.5, Pos[1] - SequenceAnimation_Height * 0.5,
          Pos[0] + SequenceAnimation_Width * 0.5, Pos[1] + SequenceAnimation_Height * 0.5), SequenceAnimation_Angle), SequenceAnimation_Alpha);
      end;
    emParticle:
      begin
        if Particle = nil then
            Exit;
        Owner.DrawParticleInScene(Particle, Pos);
      end;
  end;
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
  FViewOptions := [devpFPS, devpFrameEndge];
  FLastDrawInfo := '';

  FTextSizeCache := THashList.Create;
  FTextSizeCache.SetHashBlockCount(1024);
  FTextSizeCache.AutoFreeData := True;
  FTextSizeCache.IgnoreCase := False;
  FTextSizeCache.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}TextSizeCacheDoDataFree;
  FScrollTextList := TCoreClassListForObj.Create;

  FDownPT := NULLPoint;
  FMovePT := NULLPoint;
  FUpPT := NULLPoint;
  FLastAcceptDownUI := nil;
  FUIList := TCoreClassListForObj.Create;

  FSequenceAnimationBuffer := TCoreClassListForObj.Create;
  FParticleBuffer := TCoreClassListForObj.Create;
  FLastDynamicSeqenceFlag := 0;

  FFPSFontColor := DEColor(1, 1, 1, 1);
  FScreenFrameColor := DEColor(0.5, 0.2, 0.2, 0.5);

  FTextureLibrary := THashObjectList.Create(True);
  FOnGetTexture := nil;

  FDefaultTexture := DefaultTextureClass.Create;
  FDefaultTexture.SetSize(2, 2, RasterColorF(0, 0, 0, 1));

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

  DisposeObject(FDrawCommand);
  DisposeObject(FDrawExecute);
  DisposeObject(FScrollTextList);
  DisposeObject(FUIList);
  DisposeObject(FTextSizeCache);

  for i := 0 to FSequenceAnimationBuffer.Count - 1 do
      DisposeObject(FSequenceAnimationBuffer[i]);
  DisposeObject(FSequenceAnimationBuffer);

  ClearParticles;
  DisposeObject(FParticleBuffer);
  DisposeObject(FTextureLibrary);
  DisposeObject(FDefaultTexture);

  DisposeObject(FRasterization);

  if FUserVariants <> nil then
      DisposeObject(FUserVariants);
  if FUserObjects <> nil then
      DisposeObject(FUserObjects);
  if FUserAutoFreeObjects <> nil then
      DisposeObject(FUserAutoFreeObjects);
  inherited Destroy;
end;

function TDrawEngine.SceneToScreen(pt: TDEVec): TDEVec;
begin
  Result[0] := (pt[0] * Scale) + Offset[0];
  Result[1] := (pt[1] * Scale) + Offset[1];
end;

function TDrawEngine.SceneToScreen(x, y: TDEFloat): TDEVec;
begin
  Result := SceneToScreen(DEVec(x, y));
end;

function TDrawEngine.SceneToScreen(r: TDE4V): TDE4V;
begin
  Result := TDE4V.Init(SceneToScreen(r.MakeRectV2), r.angle);
end;

function TDrawEngine.SceneToScreen(r: TDERect): TDERect;
begin
  Result[0] := SceneToScreen(r[0]);
  Result[1] := SceneToScreen(r[1]);
end;

function TDrawEngine.SceneToScreen(r: TV2Rect4): TV2Rect4;
begin
  Result.LeftTop := SceneToScreen(r.LeftTop);
  Result.RightTop := SceneToScreen(r.RightTop);
  Result.RightBottom := SceneToScreen(r.RightBottom);
  Result.LeftBottom := SceneToScreen(r.LeftBottom);
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

function TDrawEngine.ScreenToScene(r: TDE4V): TDE4V;
begin
  Result := TDE4V.Init(ScreenToScene(r.MakeRectV2), r.angle);
end;

function TDrawEngine.ScreenToScene(r: TV2Rect4): TV2Rect4;
begin
  Result.LeftTop := ScreenToScene(r.LeftTop);
  Result.RightTop := ScreenToScene(r.RightTop);
  Result.RightBottom := ScreenToScene(r.RightBottom);
  Result.LeftBottom := ScreenToScene(r.LeftBottom);
end;

function TDrawEngine.SceneToScreenDistance(ScenePt1, ScenePt2: TDEVec): TDEFloat;
begin
  Result := Vec2Distance(SceneToScreen(ScenePt1), SceneToScreen(ScenePt2));
end;

function TDrawEngine.ScreenToSceneDistance(ScreenPt1, ScreenPt2: TDEVec): TDEFloat;
begin
  Result := Vec2Distance(ScreenToScene(ScreenPt1), ScreenToScene(ScreenPt2));
end;

function TDrawEngine.ScreenCenterOfWorld: TDEVec;
begin
  Result := ScreenToScene(DEVec(width * 0.5, height * 0.5));
end;

function TDrawEngine.SceneRectFromScreen: TDERect;
begin
  Result[0] := ScreenToScene(0, 0);
  Result[1] := ScreenToScene(width, height);
end;

function TDrawEngine.ScreenRect: TDERect;
begin
  Result[0] := NULLPoint;
  Result[1][0] := width;
  Result[1][1] := height;
end;

function TDrawEngine.ReadyOK: Boolean;
begin
  Result := (FDrawInterface <> nil) and (FDrawInterface.ReadyOK);
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

function TDrawEngine.GetTextSize(Text: SystemString; Size: TDEFloat): TDEVec;
var
  p: PDEVec;
  n: SystemString;
begin
  if (FDrawInterface <> nil) and (Text <> '') and (FDrawInterface.ReadyOK) then
    begin
      n := umlFloatToStr(Size).Text + '_' + Text;
      p := FTextSizeCache[n];
      if p = nil then
        begin
          new(p);
          LockObject(FDrawExecute);
          try
              p^ := FDrawInterface.GetTextSize(Text, Size);
          finally
              UnLockObject(FDrawExecute);
          end;
          FTextSizeCache.Add(n, p, False);
        end;
      Result := p^;
    end
  else
      Result := NULLPoint;
end;

function TDrawEngine.GetTextSizeR(Text: SystemString; Size: TDEFloat): TDERect;
begin
  Result[0] := NULLVec;
  Result[1] := GetTextSize(Text, Size);
end;

procedure TDrawEngine.ClearScrollText;
var
  i: Integer;
begin
  for i := 0 to FScrollTextList.Count - 1 do
      DisposeObject(FScrollTextList[i]);

  FScrollTextList.Clear;
end;

procedure TDrawEngine.PostScrollText(LifeTime: Double; Text: SystemString; Size: Integer; COLOR: TDEColor);
var
  sour: TScrollTextSource;
begin
  if not ReadyOK then
      Exit;

  sour := TScrollTextSource.Create;
  sour.LifeTime := LifeTime;
  sour.textRectSize := GetTextSize(Text, Size);
  sour.TextSize := Size;
  sour.TextColor := COLOR;
  sour.Text := Text;
  sour.Tag := nil;
  FScrollTextList.Add(sour);
end;

procedure TDrawEngine.PostScrollText(Tag: TCoreClassObject; LifeTime: Double; Text: SystemString; Size: Integer; COLOR: TDEColor);
var
  i: Integer;
  sour: TScrollTextSource;
begin
  if not ReadyOK then
      Exit;

  sour := nil;
  for i := 0 to FScrollTextList.Count - 1 do
    begin
      if TScrollTextSource(FScrollTextList[i]).Tag = Tag then
        begin
          sour := TScrollTextSource(FScrollTextList[i]);
          Break;
        end;
    end;
  if sour = nil then
    begin
      sour := TScrollTextSource.Create;
      FScrollTextList.Add(sour);
    end;

  sour.LifeTime := LifeTime;
  sour.textRectSize := GetTextSize(Text, Size);
  sour.TextSize := Size;
  sour.TextColor := COLOR;
  sour.Text := Text;
  sour.Tag := Tag;
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
      DisposeObject(FUIList[0]);

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
          Exit;
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
              Exit;
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
              Exit;
            end;
          inc(i);
        end;
      Result := DoTapUp(x, y);
    end;
end;

procedure TDrawEngine.BeginCaptureShadow(const OffsetVec: TDEVec; const alpha: TDEFloat);
begin
  FDrawCommand.BeginCaptureShadow(OffsetVec, alpha);
end;

procedure TDrawEngine.EndCaptureShadow;
begin
  FDrawCommand.EndCaptureShadow;
end;

function TDrawEngine.CaptureShadow: Boolean;
begin
  Result := FDrawCommand.FStartDrawShadowIndex >= 0;
end;

function TDrawEngine.LastCaptureShadowOffsetVec: TDEVec;
begin
  Result := FDrawCommand.FShadowVec;
end;

function TDrawEngine.LastCaptureShadowAlpha: TDEFloat;
begin
  Result := FDrawCommand.FShadowAlpha;
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

procedure TDrawEngine.DrawUserCustom(const OnDraw: TCustomDrawMethod; const UserData: Pointer; const UserObject: TCoreClassObject);
begin
  if Assigned(OnDraw) then
      FDrawCommand.DrawUserCustom(OnDraw, UserData, UserObject);
end;

procedure TDrawEngine.DrawArrayVec2_Line(SmoothLevel: TDEFloat; arry: TArrayVec2; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
var
  pl: TVec2List;
begin
  pl := TVec2List.Create;
  pl.AssignFromArrayV2(arry);
  DrawPL(SmoothLevel, pl, ClosedLine, COLOR, LineWidth);
  DisposeObject(pl);
end;

procedure TDrawEngine.DrawArrayVec2_Line(arry: TArrayVec2; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
var
  pl: TVec2List;
begin
  pl := TVec2List.Create;
  pl.AssignFromArrayV2(arry);
  DrawPL(pl, ClosedLine, COLOR, LineWidth);
  DisposeObject(pl);
end;

procedure TDrawEngine.DrawArrayVec2_LineInScene(SmoothLevel: TDEFloat; arry: TArrayVec2; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
var
  pl: TVec2List;
begin
  pl := TVec2List.Create;
  pl.AssignFromArrayV2(arry);
  DrawPLInScene(SmoothLevel, pl, ClosedLine, COLOR, LineWidth);
  DisposeObject(pl);
end;

procedure TDrawEngine.DrawArrayVec2_LineInScene(arry: TArrayVec2; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
var
  pl: TVec2List;
begin
  pl := TVec2List.Create;
  pl.AssignFromArrayV2(arry);
  DrawPLInScene(pl, ClosedLine, COLOR, LineWidth);
  DisposeObject(pl);
end;

procedure TDrawEngine.DrawPL(SmoothLevel: TDEFloat; pl: TVec2List; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
var
  n: TVec2List;
begin
  if SmoothLevel > 0 then
    begin
      n := TVec2List.Create;
      pl.SplineSmooth(n, SmoothLevel);
      DrawPL(n, ClosedLine, COLOR, LineWidth);
      DisposeObject(n);
    end
  else
      DrawPL(pl, ClosedLine, COLOR, LineWidth);
end;

procedure TDrawEngine.DrawPL(pl: TVec2List; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
var
  i: Integer;
  t1, t2: TDEVec;
begin
  FDrawCommand.SetLineWidth(LineWidth);

  for i := 1 to pl.Count - 1 do
    begin
      t1 := pl[i - 1]^;
      t2 := pl[i]^;
      FDrawCommand.DrawLine(t1, t2, COLOR);
    end;
  if (ClosedLine) and (pl.Count > 1) then
    begin
      t1 := pl.First^;
      t2 := pl.Last^;
      FDrawCommand.DrawLine(t1, t2, COLOR);
    end;
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
      t1 := SceneToScreen(pl.First^);
      t2 := SceneToScreen(pl.Last^);
      FDrawCommand.DrawLine(t1, t2, COLOR);
    end;
end;

procedure TDrawEngine.DrawPLInScene(SmoothLevel: TDEFloat; pl: TVec2List; ClosedLine: Boolean; COLOR: TDEColor; LineWidth: TDEFloat);
var
  n: TVec2List;
begin
  n := TVec2List.Create;
  pl.SplineSmooth(n, SmoothLevel);
  DrawPLInScene(n, ClosedLine, COLOR, LineWidth);
  DisposeObject(n);
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

procedure TDrawEngine.DrawPolyInScene(Poly: TPoly; ClosedLine: Boolean; opt: TPolyDrawOption);
var
  i: Integer;
  t1, t2: TDEVec;
  r: TDERect;
begin
  if Poly.Count < 3 then
      Exit;
  FDrawCommand.SetLineWidth(opt.LineWidth * Scale);
  for i := 0 to Poly.Count - 1 do
    begin
      t1 := SceneToScreen(Poly.Points[i]);
      r[0][0] := t1[0] - opt.PointScreenRadius;
      r[0][1] := t1[1] - opt.PointScreenRadius;
      r[1][0] := t1[0] + opt.PointScreenRadius;
      r[1][1] := t1[1] + opt.PointScreenRadius;
      FDrawCommand.DrawEllipse(r, opt.PointColor);
    end;

  t1 := SceneToScreen(Poly.Position);
  r[0][0] := t1[0] - opt.PointScreenRadius;
  r[0][1] := t1[1] - opt.PointScreenRadius;
  r[1][0] := t1[0] + opt.PointScreenRadius;
  r[1][1] := t1[1] + opt.PointScreenRadius;
  FDrawCommand.DrawEllipse(r, opt.PointColor);

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
end;

procedure TDrawEngine.DrawPolyExpandInScene(Poly: TPoly; ExpandDistance: TDEFloat; ClosedLine: Boolean; opt: TPolyDrawOption);
var
  i: Integer;
  t1, t2: TDEVec;
  r: TDERect;
begin
  if Poly.Count < 3 then
      Exit;

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

procedure TDrawEngine.DrawLine(pt1, pt2: TDEVec; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  FDrawCommand.SetLineWidth(LineWidth);
  FDrawCommand.DrawLine(pt1, pt2, COLOR);
end;

procedure TDrawEngine.DrawLineInScene(pt1, pt2: TDEVec; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  DrawLine(SceneToScreen(pt1), SceneToScreen(pt2), COLOR, LineWidth * Scale);
end;

procedure TDrawEngine.DrawCorner(box: TV2Rect4; COLOR: TDEColor; BoundLineLength, LineWidth: TDEFloat);
  procedure DrawCornerLine_(left_, cen_, right_: TVec2);
  begin
    if Vec2Distance(cen_, right_) > BoundLineLength then
        FDrawCommand.DrawLine(cen_, Vec2LerpTo(cen_, right_, BoundLineLength), COLOR);
    if Vec2Distance(cen_, left_) > BoundLineLength then
        FDrawCommand.DrawLine(cen_, Vec2LerpTo(cen_, left_, BoundLineLength), COLOR);
  end;

begin
  if (Vec2Distance(box.LeftTop, box.RightTop) < BoundLineLength * 2) or
    (Vec2Distance(box.RightTop, box.RightBottom) < BoundLineLength * 2) or
    (Vec2Distance(box.RightBottom, box.LeftBottom) < BoundLineLength * 2) or
    (Vec2Distance(box.LeftBottom, box.LeftTop) < BoundLineLength * 2) then
    begin
      DrawLine(box.LeftTop, box.RightTop, COLOR, LineWidth);
      DrawLine(box.RightTop, box.RightBottom, COLOR, LineWidth);
      DrawLine(box.RightBottom, box.LeftBottom, COLOR, LineWidth);
      DrawLine(box.LeftBottom, box.LeftTop, COLOR, LineWidth);
    end
  else
    begin
      FDrawCommand.SetLineWidth(LineWidth);
      DrawCornerLine_(box.LeftBottom, box.LeftTop, box.RightTop);
      DrawCornerLine_(box.LeftTop, box.RightTop, box.RightBottom);
      DrawCornerLine_(box.RightTop, box.RightBottom, box.LeftBottom);
      DrawCornerLine_(box.LeftTop, box.LeftBottom, box.RightBottom);
    end;
end;

procedure TDrawEngine.DrawCornerInScene(box: TV2Rect4; COLOR: TDEColor; BoundLineLength, LineWidth: TDEFloat);
begin
  DrawCorner(SceneToScreen(box), COLOR, BoundLineLength * Scale, LineWidth * Scale);
end;

procedure TDrawEngine.DrawDE4V(d: TDE4V; COLOR: TDEColor; LineWidth: TDEFloat);
var
  pr: TV2Rect4;
begin
  pr := TV2Rect4.Init(d.MakeRectV2, d.angle);
  DrawLine(pr.LeftTop, pr.RightTop, COLOR, LineWidth);
  DrawLine(pr.RightTop, pr.RightBottom, COLOR, LineWidth);
  DrawLine(pr.RightBottom, pr.LeftBottom, COLOR, LineWidth);
  DrawLine(pr.LeftBottom, pr.LeftTop, COLOR, LineWidth);
end;

procedure TDrawEngine.DrawDE4VInScene(d: TDE4V; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  DrawDE4V(SceneToScreen(d), COLOR, LineWidth * Scale);
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

procedure TDrawEngine.DrawBox(r: TDERect; angle: TGeoFloat; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  FDrawCommand.SetLineWidth(LineWidth);
  FDrawCommand.DrawRect(r, angle, COLOR);
end;

procedure TDrawEngine.DrawBoxInScene(r: TDERect; angle: TGeoFloat; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  DrawBox(SceneToScreen(r), angle, COLOR, LineWidth * Scale);
end;

procedure TDrawEngine.DrawBox(r: TDERect; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  DrawBox(r, 0, COLOR, LineWidth);
end;

procedure TDrawEngine.DrawBoxInScene(r: TDERect; COLOR: TDEColor; LineWidth: TDEFloat);
begin
  DrawBox(SceneToScreen(r), COLOR, LineWidth * Scale);
end;

procedure TDrawEngine.FillBox(r: TDERect; angle: TGeoFloat; COLOR: TDEColor);
begin
  FDrawCommand.FillRect(r, angle, COLOR);
end;

procedure TDrawEngine.FillBoxInScene(r: TDERect; angle: TGeoFloat; COLOR: TDEColor);
begin
  FillBox(SceneToScreen(r), angle, COLOR);
end;

procedure TDrawEngine.FillBox(r: TDERect; COLOR: TDEColor);
begin
  FillBox(r, 0, COLOR);
end;

procedure TDrawEngine.FillBoxInScene(r: TDERect; COLOR: TDEColor);
begin
  FillBox(SceneToScreen(r), COLOR);
end;

procedure TDrawEngine.DrawEllipse(pt: TDEVec; radius: TDEFloat; COLOR: TDEColor);
begin
  FDrawCommand.DrawEllipse(pt, radius, COLOR);
end;

procedure TDrawEngine.DrawEllipse(r: TDERect; COLOR: TDEColor);
begin
  FDrawCommand.DrawEllipse(r, COLOR);
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

procedure TDrawEngine.DrawText(Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean; RotateVec: TDEVec; angle: TDEFloat);
begin
  FDrawCommand.DrawText(Text, Size, r, COLOR, center, RotateVec, angle);
end;

procedure TDrawEngine.DrawText(Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean);
var
  siz: TDEVec;
  box: TDERect;
begin
  DrawText(Text, Size, r, COLOR, center, DEVec(0.5, 0.5), 0);

  if devpTextBox in FViewOptions then
    begin
      siz := GetTextSize(Text, Size);
      if center then
        begin
          box[0][0] := (RectWidth(r) - siz[0]) * 0.5;
          box[0][1] := (RectHeight(r) - siz[1]) * 0.5;
          box[0] := Vec2Add(box[0], r[0]);
          box[1] := Vec2Add(box[0], siz);
          DrawBox(box, COLOR, 1);
        end
      else
        begin
          box[0][0] := 0;
          box[0][1] := (RectHeight(r) - siz[1]) * 0.5;
          box[0] := Vec2Add(box[0], r[0]);
          box[1] := Vec2Add(box[0], siz);
          DrawBox(box, COLOR, 1);
        end;
    end;
end;

procedure TDrawEngine.DrawText(Text: SystemString; Size: TDEFloat; COLOR: TDEColor; ScreenPt: TDEVec);
var
  siz: TDEVec;
  r: TDERect;
begin
  siz := GetTextSize(Text, Size);
  r[0] := ScreenPt;
  r[1] := Vec2Add(ScreenPt, siz);
  DrawText(Text, Size, r, COLOR, False);
end;

procedure TDrawEngine.DrawTextInScene(Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean; RotateVec: TDEVec; angle: TDEFloat);
begin
  DrawText(Text, Size * Scale, SceneToScreen(r), COLOR, center, RotateVec, angle);
end;

procedure TDrawEngine.DrawTextInScene(Text: SystemString; Size: TDEFloat; COLOR: TDEColor; ScenePos: TDEVec);
begin
  DrawText(Text, Size * Scale, COLOR, SceneToScreen(ScenePos));
end;

procedure TDrawEngine.DrawTextInScene(Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean);
begin
  DrawText(Text, Size * Scale, SceneToScreen(r), COLOR, center);
end;

procedure TDrawEngine.DrawTexture(t: TCoreClassObject; sour, DestScreen: TDE4V; alpha: TDEFloat);
begin
  FDrawCommand.DrawTexture(t, sour, DestScreen, alpha);
end;

procedure TDrawEngine.DrawTexture(t: TCoreClassObject; sour: TDERect; DestScreen: TDE4V; alpha: TDEFloat);
begin
  DrawTexture(t, TDE4V.Init(sour, 0), DestScreen, alpha);
end;

procedure TDrawEngine.DrawTexture(t: TCoreClassObject; sour, DestScreen: TDERect; alpha: TDEFloat);
begin
  DrawTexture(t, TDE4V.Init(sour, 0), TDE4V.Init(DestScreen, 0), alpha);
end;

procedure TDrawEngine.DrawTexture(t: TCoreClassObject; sour: TDERect; destScreenPt: TDEVec; angle, alpha: TDEFloat);
var
  w, h: TDEFloat;
begin
  w := sour[1][0] - sour[0][0];
  h := sour[1][1] - sour[0][1];
  DrawTexture(t, TDE4V.Init(sour, 0), TDE4V.Init(destScreenPt, w, h, angle), alpha);
end;

procedure TDrawEngine.DrawTexture(t: TCoreClassObject; sour, DestScreen: TDERect; angle, alpha: TDEFloat);
begin
  DrawTexture(t, TDE4V.Init(sour, 0), TDE4V.Init(DestScreen, angle), alpha);
end;

function TDrawEngine.DrawTexture(indentEndge: Boolean; t: TCoreClassObject; sour, DestScreen: TDERect; alpha: TDEFloat): TDERect;
begin
  if indentEndge then
      Result := RectEndge(DestScreen, Vec2Mul(RectSize(DestScreen), -0.05))
  else
      Result := DestScreen;

  DrawTexture(t, sour, Result, alpha);
end;

procedure TDrawEngine.FitDrawTexture(t: TCoreClassObject; sour, destScene: TDERect; angle, alpha: TDEFloat);
begin
  DrawTexture(t, sour, RectFit(sour, destScene), angle, alpha);
end;

function TDrawEngine.FitDrawTexture(t: TCoreClassObject; sour, destScene: TDERect; alpha: TDEFloat): TDERect;
begin
  Result := RectFit(sour, destScene);
  DrawTexture(t, sour, Result, alpha);
end;

function TDrawEngine.FitDrawTexture(indentEndge: Boolean; t: TCoreClassObject; sour, destScene: TDERect; alpha: TDEFloat): TDERect;
begin
  if indentEndge then
      Result := RectEndge(destScene, Vec2Mul(RectSize(destScene), -0.05))
  else
      Result := destScene;

  FitDrawTexture(t, sour, Result, alpha);
end;

procedure TDrawEngine.DrawTextureInScene(t: TCoreClassObject; sour, destScene: TDE4V; alpha: TDEFloat);
begin
  DrawTexture(t, sour, SceneToScreen(destScene), alpha);
end;

procedure TDrawEngine.DrawTextureInScene(t: TCoreClassObject; sour: TDERect; destScene: TDE4V; alpha: TDEFloat);
begin
  DrawTextureInScene(t, TDE4V.Init(sour, 0), destScene, alpha);
end;

procedure TDrawEngine.DrawTextureInScene(t: TCoreClassObject; destScene: TDE4V; alpha: TDEFloat);
begin
  DrawTextureInScene(t, TDE4V.Init, destScene, alpha);
end;

procedure TDrawEngine.DrawTextureInScene(t: TCoreClassObject; sour, destScene: TDERect; alpha: TDEFloat);
begin
  DrawTextureInScene(t, TDE4V.Init(sour, 0), TDE4V.Init(destScene, 0), alpha);
end;

procedure TDrawEngine.DrawTextureInScene(t: TCoreClassObject; sour: TDERect; destScenePt: TDEVec; angle, alpha: TDEFloat);
var
  w, h: TDEFloat;
begin
  w := sour[1][0] - sour[0][0];
  h := sour[1][1] - sour[0][1];
  DrawTextureInScene(t, TDE4V.Init(sour, 0), TDE4V.Init(destScenePt, w, h, angle), alpha);
end;

procedure TDrawEngine.DrawTextureInScene(t: TCoreClassObject; sour, destScene: TDERect; angle, alpha: TDEFloat);
begin
  DrawTextureInScene(t, TDE4V.Init(sour, 0), TDE4V.Init(destScene, angle), alpha);
end;

procedure TDrawEngine.DrawTextureInScene(t: TCoreClassObject; destScenePt: TDEVec; AWidth, AHeight, angle, alpha: TDEFloat);
begin
  DrawTextureInScene(t, TDE4V.Init, TDE4V.Init(destScenePt, AWidth, AHeight, angle), alpha);
end;

function TDrawEngine.DrawTextureInScene(indentEndge: Boolean; t: TCoreClassObject; sour, destScene: TDERect; alpha: TDEFloat): TDERect;
begin
  if indentEndge then
      Result := RectEndge(destScene, Vec2Mul(RectSize(destScene), -0.05))
  else
      Result := destScene;

  DrawTextureInScene(t, sour, Result, alpha);
end;

procedure TDrawEngine.FitDrawTextureInScene(t: TCoreClassObject; sour, destScene: TDERect; angle, alpha: TDEFloat);
begin
  DrawTextureInScene(t, sour, RectFit(sour, destScene), angle, alpha);
end;

function TDrawEngine.FitDrawTextureInScene(t: TCoreClassObject; sour, destScene: TDERect; alpha: TDEFloat): TDERect;
begin
  Result := RectFit(sour, destScene);
  DrawTextureInScene(t, sour, Result, alpha);
end;

function TDrawEngine.FitDrawTextureInScene(indentEndge: Boolean; t: TCoreClassObject; sour, destScene: TDERect; alpha: TDEFloat): TDERect;
begin
  if indentEndge then
      Result := RectEndge(destScene, Vec2Mul(RectSize(destScene), -0.05))
  else
      Result := destScene;

  Result := FitDrawTextureInScene(t, sour, Result, alpha);
end;

function TDrawEngine.DrawTexturePackingInScene(packing_t: TMemoryRasterList; Margins: TGeoFloat; destOffset: TDEVec; alpha: TDEFloat): TRectV2;
var
  rp: TRectPacking;
  i: Integer;
  t: TMemoryRaster;
  r: TRectV2;
begin
  Result := NULLRect;
  if packing_t.Count = 0 then
      Exit;
  rp := TRectPacking.Create;

  for i := 0 to packing_t.Count - 1 do
      rp.Add(nil, packing_t[i], packing_t[i].BoundsRectV2);

  rp.MaxWidth := Margins;
  rp.Build();

  for i := 0 to rp.Count - 1 do
    begin
      t := rp[i]^.Data2 as TMemoryRaster;
      r := RectAdd(rp[i]^.Rect, destOffset);
      if i = 0 then
          Result := r
      else
          Result := BoundRect(Result, r);
      DrawTextureInScene(t, t.BoundsRectV2, r, alpha);
    end;

  DisposeObject(rp);
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
            Exit;
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
          Break;
    end;
  if SA = nil then
      Exit;
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
          Break;
    end;
  if SA = nil then
      Exit;
  Result := SA.IsOver;
end;

function TDrawEngine.ExistsSequenceAnimation(SA: TSequenceAnimationBase): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FSequenceAnimationBuffer.Count - 1 do
    if FSequenceAnimationBuffer[i] = SA then
        Exit(True);
end;

function TDrawEngine.GetNewSequenceFlag: Variant;
begin
  Result := FLastDynamicSeqenceFlag;
  FLastDynamicSeqenceFlag := FLastDynamicSeqenceFlag + 1;
end;

function TDrawEngine.ManualDrawSequenceTexture(flag: Variant; t: TCoreClassObject; TextureWidth, TextureHeight, Total, Column: Integer; CompleteTime: Double; Looped: Boolean;
  DestScreen: TDE4V; alpha: TDEFloat): TSequenceAnimationBase;
var
  SA: TSequenceAnimationBase;
begin
  Result := nil;
  if Total = 0 then
      Exit;
  if Column = 0 then
      Exit;

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
  DrawTexture(SA.Source, SA.SequenceFrameRect, DestScreen, SA.GetOverAnimationSmoothAlpha(alpha));
  Result := SA;
end;

function TDrawEngine.DrawSequenceTexture(flag: Variant; t: TCoreClassObject; TextureWidth, TextureHeight, Total, Column: Integer; CompleteTime: Double; Looped: Boolean;
  DestScreen: TDE4V; alpha: TDEFloat): TSequenceAnimationBase;
begin
  Result := ManualDrawSequenceTexture(flag, t, TextureWidth, TextureHeight, Total, Column, CompleteTime, Looped, DestScreen, alpha);
end;

function TDrawEngine.DrawSequenceTexture(flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; DestScreen: TDE4V; alpha: TDEFloat): TSequenceAnimationBase;
begin
  Result := DrawSequenceTexture(flag, t, t.width, t.height, t.Total, t.Column, CompleteTime, Looped, DestScreen, alpha);
end;

function TDrawEngine.DrawSequenceTexture(flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; DestScreen: TDERect; alpha: TDEFloat): TSequenceAnimationBase;
begin
  Result := DrawSequenceTexture(flag, t, t.width, t.height, t.Total, t.Column, CompleteTime, Looped, TDE4V.Init(DestScreen, 0), alpha);
end;

function TDrawEngine.DrawSequenceTexture(flag: Variant; t: TDETexture; CompleteTime: Double; DestScreen: TDE4V; alpha: TDEFloat): TSequenceAnimationBase;
begin
  Result := DrawSequenceTexture(flag, t, t.width, t.height, t.Total, t.Column, CompleteTime, False, DestScreen, alpha);
end;

function TDrawEngine.FitDrawSequenceTexture(flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; DestScreen: TDERect; alpha: TDEFloat): TDERect;
begin
  Result := RectFit(t.FrameRect2D, DestScreen);
  DrawSequenceTextureInScene(flag, t, t.width, t.height, t.Total, t.Column, CompleteTime, Looped, TDE4V.Init(Result, 0), alpha);
end;

function TDrawEngine.FitDrawSequenceTexture(indentEndge: Boolean; flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; DestScreen: TDERect; alpha: TDEFloat): TDERect;
var
  d: TDERect;
begin
  if indentEndge then
      d := RectEndge(DestScreen, Vec2Mul(RectSize(DestScreen), -0.05))
  else
      d := DestScreen;

  Result := RectFit(t.FrameRect2D, d);
  DrawSequenceTextureInScene(flag, t, t.width, t.height, t.Total, t.Column, CompleteTime, Looped, TDE4V.Init(Result, 0), alpha);
end;

procedure TDrawEngine.DrawSequenceTexture(SA: TSequenceAnimationBase; DestScreen: TDE4V; alpha: TDEFloat);
begin
  SA.LastUsed := True;
  DrawTexture(SA.Source, SA.SequenceFrameRect, DestScreen, SA.GetOverAnimationSmoothAlpha(alpha));
end;

function TDrawEngine.DrawSequenceTextureInScene(flag: Variant; t: TCoreClassObject; TextureWidth, TextureHeight, Total, Column: Integer; CompleteTime: Double; Looped: Boolean;
  destScene: TDE4V; alpha: TDEFloat): TSequenceAnimationBase;
begin
  Result := DrawSequenceTexture(flag, t, TextureWidth, TextureHeight, Total, Column, CompleteTime, Looped, SceneToScreen(destScene), alpha);
end;

function TDrawEngine.DrawSequenceTextureInScene(flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; destScene: TDE4V; alpha: TDEFloat): TSequenceAnimationBase;
begin
  Result := DrawSequenceTextureInScene(flag, t, t.width, t.height, t.Total, t.Column, CompleteTime, Looped, destScene, alpha);
end;

function TDrawEngine.DrawSequenceTextureInScene(flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; destScene: TDERect; alpha: TDEFloat): TSequenceAnimationBase;
begin
  Result := DrawSequenceTextureInScene(flag, t, t.width, t.height, t.Total, t.Column, CompleteTime, Looped, TDE4V.Init(destScene, 0), alpha);
end;

function TDrawEngine.DrawSequenceTextureInScene(flag: Variant; t: TDETexture; CompleteTime: Double; destScene: TDE4V; alpha: TDEFloat): TSequenceAnimationBase;
begin
  Result := DrawSequenceTextureInScene(flag, t, t.width, t.height, t.Total, t.Column, CompleteTime, False, destScene, alpha);
end;

function TDrawEngine.FitDrawSequenceTextureInScene(flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; destScene: TDERect; alpha: TDEFloat): TDERect;
begin
  Result := RectFit(t.FrameRect2D, destScene);
  DrawSequenceTextureInScene(flag, t, t.width, t.height, t.Total, t.Column, CompleteTime, Looped, TDE4V.Init(Result, 0), alpha);
end;

function TDrawEngine.FitDrawSequenceTextureInScene(indentEndge: Boolean; flag: Variant; t: TDETexture; CompleteTime: Double; Looped: Boolean; destScene: TDERect; alpha: TDEFloat): TDERect;
var
  d: TDERect;
begin
  if indentEndge then
      d := RectEndge(destScene, Vec2Mul(RectSize(destScene), -0.05))
  else
      d := destScene;

  Result := RectFit(t.FrameRect2D, d);
  DrawSequenceTextureInScene(flag, t, t.width, t.height, t.Total, t.Column, CompleteTime, Looped, TDE4V.Init(Result, 0), alpha);
end;

procedure TDrawEngine.DrawSequenceTextureInScene(SA: TSequenceAnimationBase; destScene: TDE4V; alpha: TDEFloat);
begin
  DrawSequenceTexture(SA, SceneToScreen(destScene), alpha);
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
        FParticleBuffer.Delete(i)
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
        FParticleBuffer.Delete(i)
    else
        inc(i);
  p.Owner := nil;
  DisposeObject(p);
end;

procedure TDrawEngine.ClearParticles;
var
  i: Integer;
begin
  for i := 0 to FParticleBuffer.Count - 1 do
    begin
      TParticles(FParticleBuffer[i]).Owner := nil;
      DisposeObject(FParticleBuffer[i]);
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
        Exit(True);
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

function TDrawEngine.GetParticles(const index: Integer): TParticles;
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
            DrawSequenceTexture(p^.Source, TDE4V.Init(p^.Position, p^.radius * 2, p^.radius * 2, p^.angle), p^.alpha);
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
  tsBuff: TTextureOutputStateBuffer;
  i: Integer;
  ptex: PTextureOutputState;
  pr: PRectPackData;
begin
  bakScale := Scale;
  bakOffset := Offset;
  try
    rl := TRectPacking.Create;
    FDrawCommand.BuildTextureOutputState(tsBuff);
    for i := 0 to length(tsBuff) - 1 do
      begin
        ptex := @(tsBuff[i]);
        if not rl.Data2Exists(ptex^.Source) then
          begin
            if ptex^.Source is TMemoryRaster then
                rl.Add(ptex, ptex^.Source, TMemoryRaster(ptex^.Source).BoundsRectV2)
            else
                rl.Add(ptex, ptex^.Source, ptex^.SourceRect.MakeRectV2);
          end;
      end;
    rl.Build(1024 * 1024, 1024 * 1024);

    r := RectFit(DERect(0, 0, rl.MaxWidth + 4, rl.MaxHeight + 4), FTextureOutputStateBox);
    Scale := RectWidth(r) / rl.MaxWidth;
    Offset := r[0];

    FillBox(FTextureOutputStateBox, DEColor(0, 0, 0, 0.95));

    for i := 0 to rl.Count - 1 do
      begin
        pr := rl[i];
        ptex := pr^.Data1;
        DrawTextureInScene(ptex^.Source, ptex^.SourceRect, TDE4V.Init(pr^.Rect, 0), 0.5);
      end;
    DrawText('Texture:' + umlIntToStr(rl.Count).Text + ' Area:' + umlIntToStr(Round(rl.MaxWidth)).Text + ' x ' + umlIntToStr(Round(rl.MaxHeight)).Text,
      10, DEColor(1, 1, 1, 1), r[0]);
    DisposeObject(rl);
  except
  end;
  Scale := bakScale;
  Offset := bakOffset;
end;

procedure TDrawEngine.PrepareFlush;
var
  lastTime: TTimeTick;
  i: Integer;
  pt: TDEVec;
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
      if st.LifeTime > 0 then
        begin
          r[0] := Vec2Sub(pt, st.textRectSize);
          r[1] := Vec2Add(r[0], st.textRectSize);
          pt[1] := pt[1] - st.textRectSize[1];
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

  if devpFrameEndge in FViewOptions then
    begin
      FDrawCommand.SetLineWidth(1);
      FDrawCommand.DrawRect(MakeRect(1, 1, width - 1, height - 1), 0, FScreenFrameColor);
    end;

  FLastDrawInfo :=
    'resolution: ' + umlIntToStr(Round(width)).Text + ' * ' + umlIntToStr(Round(height)).Text;
  if Round(FrameCounterOfPerSec) > 0 then
      FLastDrawInfo := FLastDrawInfo + ' fps: ' + umlIntToStr(Round(FrameCounterOfPerSec)).Text + ' pipe: ' + umlIntToStr(Round(CommandCounterOfPerSec)).Text;

  if devpFPS in FViewOptions then
    begin
      BeginCaptureShadow(Vec2(1, 1), 1.0);
      DrawText(FLastDrawInfo, 12, FFPSFontColor, DEVec(5, 5));
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
          DisposeObject(SA);
          FSequenceAnimationBuffer.Delete(i);
        end
      else
          inc(i);
    end;

  if devpTextureState in FViewOptions then
      PrepareTextureOutputState;
end;

procedure TDrawEngine.ClearFlush;
begin
  FDrawCommand.Clear(True);
  FDrawExecute.Clear;
end;

procedure TDrawEngine.Flush;
begin
  Flush(True);
end;

procedure TDrawEngine.Flush(Prepare: Boolean);
begin
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
          DisposeObject(st);
          FScrollTextList.Delete(i);
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
              DisposeObject(p);
              FParticleBuffer.Delete(i);
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
end;

constructor TDrawEnginePool.Create;
begin
  inherited Create;
  FDefaultDrawEngineClass := TDrawEngine;
  FDrawEngineList := TCoreClassList.Create;
  FPostProgress := TNProgressPost.Create;
end;

destructor TDrawEnginePool.Destroy;
begin
  Clear;
  DisposeObject(FDrawEngineList);
  DisposeObject(FPostProgress);
  inherited Destroy;
end;

procedure TDrawEnginePool.Clear;
var
  i: Integer;
  p: PDrawEnginePoolData;
begin
  for i := 0 to FDrawEngineList.Count - 1 do
    begin
      p := FDrawEngineList[i];
      DisposeObject(p^.DrawEng);
      Dispose(p);
    end;
  FDrawEngineList.Clear;
end;

procedure TDrawEnginePool.ClearActivtedTimeOut(tickLen: Cardinal);
var
  i: Integer;
  p: PDrawEnginePoolData;
begin
  i := 0;
  while i < FDrawEngineList.Count do
    begin
      p := FDrawEngineList[i];
      if GetTimeTick - p^.LastActivted > tickLen then
        begin
          DisposeObject(p^.workObj);
          Dispose(p);
          FDrawEngineList.Delete(i);
        end
      else
          inc(i);
    end;
end;

procedure TDrawEnginePool.Progress(deltaTime: Double);
var
  i: Integer;
begin
  FPostProgress.Progress(deltaTime);

  for i := 0 to FDrawEngineList.Count - 1 do
      PDrawEnginePoolData(FDrawEngineList[i])^.DrawEng.Progress(deltaTime);
end;

function TDrawEnginePool.GetEng(const workObj: TCoreClassObject; const Draw: TDrawEngineInterface): TDrawEngine;
var
  i: Integer;
  p: PDrawEnginePoolData;
begin
  for i := 0 to FDrawEngineList.Count - 1 do
    begin
      p := FDrawEngineList[i];
      if p^.workObj = workObj then
        begin
          p^.LastActivted := GetTimeTick();
          if p^.DrawEng.FDrawInterface <> Draw then
              p^.DrawEng.FDrawInterface := Draw;

          p^.DrawEng.SetSize;
          Exit(p^.DrawEng);
        end;
    end;
  new(p);
  p^.DrawEng := FDefaultDrawEngineClass.Create;
  p^.DrawEng.FDrawInterface := Draw;
  p^.DrawEng.ViewOptions := [];
  p^.workObj := workObj;
  p^.LastActivted := GetTimeTick;
  p^.DrawEng.SetSize;
  FDrawEngineList.Add(p);
  Result := p^.DrawEng;
end;

function TDrawEnginePool.GetEng(const workObj: TCoreClassObject): TDrawEngine;
var
  i: Integer;
  p: PDrawEnginePoolData;
begin
  for i := 0 to FDrawEngineList.Count - 1 do
    begin
      p := FDrawEngineList[i];
      if p^.workObj = workObj then
        begin
          p^.LastActivted := GetTimeTick();
          Exit(p^.DrawEng);
        end;
    end;
  new(p);
  p^.DrawEng := FDefaultDrawEngineClass.Create;
  p^.DrawEng.ViewOptions := [];
  p^.workObj := workObj;
  p^.LastActivted := GetTimeTick;
  FDrawEngineList.Add(p);
  Result := p^.DrawEng;
end;

function TDrawEngine_Raster.DEColor2RasterColor(const COLOR: TDEColor): TRasterColor;
begin
  Result := RasterColorF(COLOR[0], COLOR[1], COLOR[2], COLOR[3]);
end;

function TDrawEngine_Raster.DEColor2RasterColor(const COLOR: TDEColor; const alpha: Byte): TRasterColor;
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
end;

destructor TDrawEngine_Raster.Destroy;
begin
  DisposeObject(FMemory);
  if (FFreeEngine) and (FEngine <> nil) then
      DisposeObject(FEngine);
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
      Exit;

  FMemory.Agg.LineWidth := w
end;

procedure TDrawEngine_Raster.DrawLine(pt1, pt2: TDEVec; COLOR: TDEColor);
begin
  FMemory.LineF(pt1, pt2, DEColor2RasterColor(COLOR), True);
end;

procedure TDrawEngine_Raster.DrawRect(r: TDERect; angle: TDEFloat; COLOR: TDEColor);
begin
  FMemory.DrawRect(r, angle, DEColor2RasterColor(COLOR));
end;

procedure TDrawEngine_Raster.FillRect(r: TDERect; angle: TDEFloat; COLOR: TDEColor);
begin
  FMemory.FillRect(r, angle, DEColor2RasterColor(COLOR));
end;

procedure TDrawEngine_Raster.DrawEllipse(r: TDERect; COLOR: TDEColor);
var
  c: TDEVec;
begin
  c := RectCentre(r);
  FMemory.DrawEllipse(c, RectWidth(r) * 0.5, RectHeight(r) * 0.5, DEColor2RasterColor(COLOR));
end;

procedure TDrawEngine_Raster.FillEllipse(r: TDERect; COLOR: TDEColor);
var
  c: TDEVec;
begin
  c := RectCentre(r);
  FMemory.FillEllipse(c, RectWidth(r) * 0.5, RectHeight(r) * 0.5, DEColor2RasterColor(COLOR));
end;

procedure TDrawEngine_Raster.DrawText(Text: SystemString; Size: TDEFloat; r: TDERect; COLOR: TDEColor; center: Boolean; RotateVec: TDEVec; angle: TDEFloat);
var
  vSiz: TDEVec;
  x, y: TDEFloat;
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
  FMemory.DrawText(Text, Round(x), Round(y), RotateVec, angle, 1.0, Size, DEColor2RasterColor(COLOR));
end;

procedure TDrawEngine_Raster.DrawTexture(t: TCoreClassObject; sour, dest: TDE4V; alpha: TDEFloat);
begin
  if t is TMemoryRaster then
      TMemoryRaster(t).ProjectionTo(FMemory, TV2Rect4.Init(sour.MakeRectV2, sour.angle), TV2Rect4.Init(dest.MakeRectV2, dest.angle), True, alpha);
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

function TDrawEngine_Raster.GetTextSize(Text: SystemString; Size: TDEFloat): TDEVec;
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

procedure TDrawEngine_Raster.SetWorkMemory(m: TMemoryRaster);
begin
  Memory.SetWorkMemory(m);
  Engine.SetSize(m);
end;

initialization

DefaultTextureClass := TDETexture;
EnginePool := TDrawEnginePool.Create;

finalization

if EnginePool <> nil then
  begin
    DisposeObject(EnginePool);
    EnginePool := nil;
  end;

end.
