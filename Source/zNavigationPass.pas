{ ****************************************************************************** }
{ * path Pass struct create by qq600585                                     * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit zNavigationPass;

{$INCLUDE zDefine.inc}

interface

uses SysUtils, CoreClasses, Math, Geometry2DUnit, zNavigationPoly, MovementEngine;

type
  TPolyPassManager = class;
  TBasePass        = class;
  TNavBio          = class;
  TNavBioManager   = class;

  TPassState = record
    Owner: TBasePass;
    passed: TBasePass;
    State: ShortInt;

    function Enabled: Boolean; overload;
    function Enabled(ignore: TCoreClassListForObj): Boolean; overload;
  end;

  PPassState = ^TPassState;

  TBasePass = class(TCoreClassPersistent)
  private
    FDataList: TCoreClassList;
    FOwner: TPolyPassManager;
    FPassIndex: Integer;
    function GetData(index: Integer): PPassState;
    function GetState(index: Integer): ShortInt;
    procedure SetState(index: Integer; const Value: ShortInt);
  public
    constructor Create(AOwner: TPolyPassManager);
    destructor Destroy; override;

    procedure ClearPass;

    procedure Add(APass: TBasePass);
    procedure Delete; overload;
    procedure Delete(idx: Integer); overload;
    procedure Delete(APass: TBasePass); overload;
    function Count: Integer;
    function Exists(APass: TBasePass): Boolean;
    function IndexOf(APass: TBasePass): Integer;

    procedure BuildPass(AExpandDist: TGeoFloat); virtual; abstract;
    function pass(APass: TBasePass): Boolean; virtual; abstract;
    function GetPosition: TVec2; virtual; abstract;
    property Position: TVec2 read GetPosition;

    property State[index: Integer]: ShortInt read GetState write SetState;
    property Data[index: Integer]: PPassState read GetData; default;
    property PassIndex: Integer read FPassIndex write FPassIndex;

    property Owner: TPolyPassManager read FOwner;
  end;

  TPointPass = class(TBasePass)
  private
    FPoint: TVec2;
  public
    constructor Create(AOwner: TPolyPassManager; APoint: TVec2);
    procedure BuildPass(AExpandDist: TGeoFloat); override;
    function pass(APass: TBasePass): Boolean; override;
    function GetPosition: TVec2; override;
  end;

  TPolyPass = class(TBasePass)
  private
    FPoly: TPolyManagerChildren;
    FPolyIndex: Integer;
    FExtandDistance: TGeoFloat;
    FCachePoint: TVec2;
  public
    constructor Create(AOwner: TPolyPassManager; APoly: TPolyManagerChildren; APolyIndex: Integer; AExpandDist: TGeoFloat);
    procedure BuildPass(AExpandDist: TGeoFloat); override;
    function pass(APass: TBasePass): Boolean; override;
    function GetPosition: TVec2; override;
  end;

  TNavigationBioPass = class(TBasePass)
  private
    FNavBio: TNavBio;
    FNavBioIndex: Integer;
    FExtandDistance: TGeoFloat;
    FFastEndgePass: Boolean;
    FNeedUpdate: Boolean;
  public
    constructor Create(AOwner: TPolyPassManager;
      ANavBio: TNavBio; ANavBioIndex: Integer; AExpandDist: TGeoFloat; AFastEndgePass: Boolean);
    destructor Destroy; override;
    procedure BuildPass(AExpandDist: TGeoFloat); override;
    function pass(APass: TBasePass): Boolean; override;
    function GetPosition: TVec2; override;
  end;

  TPolyPassManager = class(TCoreClassPersistent)
  private type
    TCacheState     = (csUnCalc, csYes, csNo);
    TIntersectCache = array of array of array of array of TCacheState;
    TPointInCache   = array of array of TCacheState;
  private
    FPolyManager: TPolyManager;
    FNavBioManager: TNavBioManager;

    FPassList: TCoreClassListForObj;
    FExtandDistance: TGeoFloat;
    FPassStateIncremental: ShortInt;
  private
    FIntersectCache: TIntersectCache;
    FPointInCache: TPointInCache;
  protected
    function GetPass(index: Integer): TBasePass;
  public
    constructor Create(APolyManager: TPolyManager; ANavBioManager: TNavBioManager; AExpandDistance: TGeoFloat);
    destructor Destroy; override;

    function PointOk(AExpandDist: TGeoFloat; pt: TVec2): Boolean; overload;
    // include dynamic poly,ignore object
    function PointOk(AExpandDist: TGeoFloat; pt: TVec2; ignore: TCoreClassListForObj): Boolean; overload;

    function LineIntersect(AExpandDist: TGeoFloat; lb, le: TVec2): Boolean; overload;
    // include dynamic poly,ignore object
    function LineIntersect(AExpandDist: TGeoFloat; lb, le: TVec2; ignore: TCoreClassListForObj): Boolean; overload;

    function PointOkCache(AExpandDist: TGeoFloat; poly1: TPolyManagerChildren; idx1: Integer): Boolean;
    function LineIntersectCache(AExpandDist: TGeoFloat;
      poly1: TPolyManagerChildren; idx1: Integer; poly2: TPolyManagerChildren; idx2: Integer): Boolean;

    function ExistsNavBio(NavBio: TNavBio): Boolean;
    procedure AddNavBio(NavBio: TNavBio);
    // fast add navBio can auto build Pass for self
    procedure AddFastNavBio(NavBio: TNavBio);
    procedure DeleteNavBio(NavBio: TNavBio);

    procedure BuildNavBioPass;
    procedure BuildPass;
    procedure Rebuild;

    property ExtandDistance: TGeoFloat read FExtandDistance;
    function NewPassStateIncremental: ShortInt;

    function Add(AConn: TBasePass; const NowBuildPass: Boolean): Integer;
    procedure Delete(idx: Integer); overload;
    procedure Delete(AConn: TBasePass); overload;
    procedure Clear;
    function Count: Integer;
    function IndexOf(b: TBasePass): Integer;
    property pass[index: Integer]: TBasePass read GetPass; default;

    function TotalPassNodeCount: Integer;

    property PolyManager: TPolyManager read FPolyManager;
    property NavBioManager: TNavBioManager read FNavBioManager;
  end;

  TNavState = (nsStop, nsRun, nsPause, nsStatic);

  TPhysicsPropertys = set of (npNoBounce, npIgnoreCollision, npFlight);

  TChangeSource = (csBounce, csRun, csGroupRun, csInit, csExternal);

  TLastState = (lsDetectPoint, lsDirectLerpTo, lsProcessBounce, lsIgnore);

  TNavBioProcessState = record
  private
    FLastState: TLastState;
    procedure SetLastState(const Value: TLastState);
  public
    Owner: TNavBio;
    LerpTo: TVec2;
    LerpSpeed: TGeoFloat;
    Timer: Double;
    PositionChanged: Boolean;
    ChangeSource: TChangeSource;
    LastProcessed: Boolean;
    LastStateBounceWaitTime: Double;

    property LastState: TLastState read FLastState write SetLastState;
    procedure Init(AOwner: TNavBio);
    procedure ReleaseSelf;
  end;

  PNavBioProcessState = ^TNavBioProcessState;

  INavBioNotifyInterface = interface
    procedure DoRollAngleStart(Sender: TNavBio);
    procedure DoMovementStart(Sender: TNavBio; ToPosition: TVec2);
    procedure DoMovementOver(Sender: TNavBio);
    procedure DoMovementProcess(deltaTime: Double);
  end;

  TNavBio = class(TCoreClassInterfacedObject, IMovementEngineIntf)
  private
    FOwner: TNavBioManager;
    FPassManager: TPolyPassManager;

    // in Pass manager
    FEndgePassList: TCoreClassListForObj;

    FPosition: TVec2;
    FRollAngle: TGeoFloat;
    FRadius: TGeoFloat;
    FMovement: TMovementEngine;
    FMovementPath: TVec2List;

    FState: TNavState;
    FPhysicsPropertys: TPhysicsPropertys;

    FInternalStates: TNavBioProcessState;
    FNotifyIntf: INavBioNotifyInterface;
  protected
    // MovementEngine interface
    function GetPosition: TVec2;
    procedure SetPosition(const Value: TVec2);

    function GetRollAngle: TGeoFloat;
    procedure SetRollAngle(const Value: TGeoFloat);

    procedure DoStartMovement;
    procedure DoMovementDone;

    procedure DoRollMovementStart;
    procedure DoRollMovementOver;

    procedure DoLoop;

    procedure DoStop;
    procedure DoPause;
    procedure DoContinue;

    procedure DoMovementStepChange(OldStep, NewStep: TMovementStep);
  protected
    // return True so now execute smooth lerpto
    function SmoothBouncePosition(const OldPos: TVec2; var NewPos, NewLerpPos: TVec2): Boolean;

    procedure SetDirectPosition(const Value: TVec2);

    procedure SetState(const Value: TNavState);
    procedure SetPhysicsPropertys(const Value: TPhysicsPropertys);
    function GetSlices: Integer;
  protected
    FCollisionList: TCoreClassListForObj;

    procedure LinkCollision(p: TNavBio; const Associated: Boolean);
    procedure UnLinkCollision(p: TNavBio; const Associated: Boolean);
  public
    constructor Create(AOwner: TNavBioManager; APassManager: TPolyPassManager);
    destructor Destroy; override;

    procedure DirectionTo(ToPosition: TVec2);
    function MovementTo(ToPosition: TVec2): Boolean;
    procedure stop;
    procedure Hold;

    function GetBouncePosition(const OldPos: TVec2; var NewPos: TVec2): Boolean;
    procedure ProcessBounce(p1: TNavBio; deltaTime: Double);
    procedure Progress(deltaTime: Double);

    function IsPause: Boolean;
    function IsStop: Boolean;
    function IsStatic: Boolean;
    function IsRun: Boolean;
    function IsFlight: Boolean;
    property IsActived: Boolean read FInternalStates.PositionChanged;

    function CanBounce: Boolean;
    function CanCollision: Boolean;

    procedure Collision(Trigger: TNavBio); virtual;
    procedure UnCollision(Trigger: TNavBio); virtual;

    property Owner: TNavBioManager read FOwner;
    property PassManager: TPolyPassManager read FPassManager;

    property State: TNavState read FState write SetState;
    property DirectState: TNavState read FState write FState;
    property PhysicsPropertys: TPhysicsPropertys read FPhysicsPropertys write SetPhysicsPropertys;

    property Position: TVec2 read GetPosition write SetPosition;
    property DirectPosition: TVec2 read GetPosition write SetDirectPosition;
    property RollAngle: TGeoFloat read GetRollAngle write SetRollAngle;

    property radius: TGeoFloat read FRadius write FRadius;

    property Slices: Integer read GetSlices;

    property MovementPath: TVec2List read FMovementPath;
    property Movement: TMovementEngine read FMovement;
    property NotifyIntf: INavBioNotifyInterface read FNotifyIntf write FNotifyIntf;

    property CollisionList: TCoreClassListForObj read FCollisionList;

    function ExistsCollision(v: TNavBio): Boolean;

    property PositionChanged: Boolean read FInternalStates.PositionChanged write FInternalStates.PositionChanged;

    // TNavigationBioPass of list
    property EndgePassList: TCoreClassListForObj read FEndgePassList;
  end;

  TNavBioNotify = procedure(Sender: TNavBio) of object;

  TNavBioManager = class(TCoreClassPersistent)
  private
    FNavBioList: TCoreClassListForObj;

    FBounceWaitTime: Double;
    FSlices: Integer;
    FSmooth: Boolean;
    FIgnoreAllCollision: Boolean;

    FOnRebuildNavBioPass: TNavBioNotify;
  private
    function GetItems(index: Integer): TNavBio;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(PassManager: TPolyPassManager; pt: TVec2; angle: TGeoFloat): TNavBio;
    procedure Clear;
    function Count: Integer;
    function ActivtedCount: Integer;
    property Items[index: Integer]: TNavBio read GetItems; default;

    function PointOk(AExpandDist: TGeoFloat; pt: TVec2; ignore: TCoreClassListForObj): Boolean; overload;
    function PointOk(AExpandDist: TGeoFloat; pt: TVec2): Boolean; overload;
    function LineIntersect(AExpandDist: TGeoFloat; lb, le: TVec2; ignore: TCoreClassListForObj): Boolean; overload;
    function LineIntersect(AExpandDist: TGeoFloat; lb, le: TVec2): Boolean; overload;

    function DetectCollision(p1, p2: TNavBio): Boolean;

    procedure Progress(deltaTime: Double);

    property OnRebuildNavBioPass: TNavBioNotify read FOnRebuildNavBioPass write FOnRebuildNavBioPass;

    property BounceWaitTime: Double read FBounceWaitTime write FBounceWaitTime;
    property Smooth: Boolean read FSmooth write FSmooth;
    // slices default are 5
    property Slices: Integer read FSlices write FSlices;
    property IgnoreAllCollision: Boolean read FIgnoreAllCollision write FIgnoreAllCollision;
  end;

implementation

uses zNavigationPathFinding, Geometry3DUnit;

function TPassState.Enabled: Boolean;
begin
  Result := not Owner.FOwner.FNavBioManager.LineIntersect(Owner.FOwner.ExtandDistance, Owner.Position, passed.Position);
end;

function TPassState.Enabled(ignore: TCoreClassListForObj): Boolean;
begin
  Result := not Owner.FOwner.FNavBioManager.LineIntersect(Owner.FOwner.ExtandDistance, Owner.Position, passed.Position, ignore);
end;

function TBasePass.GetData(index: Integer): PPassState;
begin
  Result := FDataList[index];
end;

function TBasePass.GetState(index: Integer): ShortInt;
begin
  Result := PPassState(FDataList[index])^.State;
end;

procedure TBasePass.SetState(index: Integer; const Value: ShortInt);
var
  i: Integer;
  p1, p2: PPassState;
begin
  p1 := FDataList[index];
  p1^.State := Value;

  for i := 0 to p1^.passed.FDataList.Count - 1 do
    begin
      p2 := p1^.passed.FDataList[i];
      if p2^.passed = Self then
        begin
          p2^.State := Value;
          Break;
        end;
    end;
end;

constructor TBasePass.Create(AOwner: TPolyPassManager);
begin
  inherited Create;
  FDataList := TCoreClassList.Create;
  FOwner := AOwner;
  FPassIndex := -1;
end;

destructor TBasePass.Destroy;
begin
  ClearPass;
  DisposeObject(FDataList);
  inherited;
end;

procedure TBasePass.ClearPass;
begin
  while FDataList.Count > 0 do
      Delete(FDataList.Count - 1);
end;

procedure TBasePass.Add(APass: TBasePass);
var
  p1, p2: PPassState;
begin
  // add to self list
  new(p1);
  p1^.Owner := Self;
  p1^.passed := APass;
  p1^.State := 0;
  FDataList.Add(p1);
  // add to dest conect list
  new(p2);
  p2^.Owner := APass;
  p2^.passed := Self;
  p2^.State := 0;
  APass.FDataList.Add(p2);
end;

procedure TBasePass.Delete;
begin
  if FOwner <> nil then
      FOwner.Delete(Self);
end;

procedure TBasePass.Delete(idx: Integer);
var
  p1, p2: PPassState;
  i: Integer;
begin
  p1 := FDataList[idx];

  i := 0;
  while i < p1^.passed.FDataList.Count do
    begin
      p2 := p1^.passed.FDataList[i];
      if p2^.passed = Self then
        begin
          p1^.passed.FDataList.Delete(i);
          Dispose(p2);
        end
      else
          inc(i);
    end;
  Dispose(p1);
  FDataList.Delete(idx);
end;

procedure TBasePass.Delete(APass: TBasePass);
var
  i: Integer;
begin
  i := 0;
  while i < FDataList.Count do
    if PPassState(FDataList[i])^.passed = APass then
        Delete(i)
    else
        inc(i);
end;

function TBasePass.Count: Integer;
begin
  Result := FDataList.Count;
end;

function TBasePass.Exists(APass: TBasePass): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := Count - 1 downto 0 do
    if (Data[i]^.passed = APass) then
        Exit;
  Result := False;
end;

function TBasePass.IndexOf(APass: TBasePass): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Count - 1 downto 0 do
    if (Data[i]^.passed = APass) then
      begin
        Result := i;
        Break;
      end;
end;

constructor TPointPass.Create(AOwner: TPolyPassManager; APoint: TVec2);
begin
  inherited Create(AOwner);
  FPoint := APoint;
end;

procedure TPointPass.BuildPass(AExpandDist: TGeoFloat);
var
  i: Integer;
  b: TBasePass;
  pt: TVec2;
begin
  ClearPass;
  pt := GetPosition;
  for i := 0 to FOwner.Count - 1 do
    begin
      b := FOwner.pass[i];
      if (b <> Self) and (pass(b)) and (not Exists(b)) and (not FOwner.FPolyManager.LineIntersect(AExpandDist, pt, b.GetPosition)) then
          Add(b);
    end;
end;

function TPointPass.pass(APass: TBasePass): Boolean;
begin
  Result := True;
end;

function TPointPass.GetPosition: TVec2;
begin
  Result := FPoint;
end;

constructor TPolyPass.Create(AOwner: TPolyPassManager; APoly: TPolyManagerChildren; APolyIndex: Integer; AExpandDist: TGeoFloat);
begin
  inherited Create(AOwner);
  FPoly := APoly;
  FPolyIndex := APolyIndex;
  FExtandDistance := AExpandDist;
  FCachePoint := FPoly.Expands[FPolyIndex, FExtandDistance];
end;

procedure TPolyPass.BuildPass(AExpandDist: TGeoFloat);
var
  pt: TVec2;
  i: Integer;
  b: TBasePass;
  p: TPolyPass;
begin
  ClearPass;
  pt := GetPosition;
  for i := 0 to FOwner.Count - 1 do
    begin
      b := FOwner.pass[i];

      if b is TPolyPass then
        begin
          p := TPolyPass(b);
          if (p <> Self) and (pass(p)) and (not Exists(p)) and (not FOwner.LineIntersectCache(AExpandDist, FPoly, FPolyIndex, p.FPoly, p.FPolyIndex)) then
              Add(p);
        end
      else
        begin
          if (b <> Self) and (pass(b)) and (not Exists(b)) and (not FOwner.LineIntersect(AExpandDist, pt, b.GetPosition)) then
              Add(b);
        end;
    end;
end;

function TPolyPass.pass(APass: TBasePass): Boolean;
begin
  Result := True;
end;

function TPolyPass.GetPosition: TVec2;
begin
  Result := FCachePoint;
end;

constructor TNavigationBioPass.Create(AOwner: TPolyPassManager;
  ANavBio: TNavBio; ANavBioIndex: Integer; AExpandDist: TGeoFloat; AFastEndgePass: Boolean);
begin
  inherited Create(AOwner);
  FNavBio := ANavBio;
  FNavBioIndex := ANavBioIndex;
  FExtandDistance := AExpandDist;
  FFastEndgePass := AFastEndgePass;
  FNeedUpdate := True;

  FNavBio.EndgePassList.Add(Self);
end;

destructor TNavigationBioPass.Destroy;
var
  i: Integer;
begin
  i := 0;
  while i < FNavBio.EndgePassList.Count do
    begin
      if FNavBio.EndgePassList[i] = Self then
          FNavBio.EndgePassList.Delete(i)
      else
          inc(i);
    end;
  inherited Destroy;
end;

procedure TNavigationBioPass.BuildPass(AExpandDist: TGeoFloat);
var
  pt: TVec2;
  i: Integer;
  b: TBasePass;
begin
  ClearPass;
  if FFastEndgePass then
    begin
      pt := GetPosition;
      for i := 0 to FOwner.Count - 1 do
        begin
          b := FOwner.pass[i];

          if (b = Self) and (pass(b)) and (not Exists(b)) and
            (not FOwner.FNavBioManager.LineIntersect(AExpandDist - 1, pt, b.GetPosition)) then
              Add(b);
        end;
    end
  else
    begin
      pt := GetPosition;
      for i := 0 to FOwner.Count - 1 do
        begin
          b := FOwner.pass[i];

          if (b <> Self) and (pass(b)) and (not Exists(b)) and
            (not FOwner.LineIntersect(AExpandDist, pt, b.GetPosition)) and
            (not FOwner.FNavBioManager.LineIntersect(AExpandDist - 1, pt, b.GetPosition)) then
              Add(b);
        end;
    end;
  FNeedUpdate := False;
end;

function TNavigationBioPass.pass(APass: TBasePass): Boolean;
begin
  Result := True;
end;

function TNavigationBioPass.GetPosition: TVec2;
var
  r: TGeoFloat;
begin
  r := (FNavBio.radius + FExtandDistance) / Sin((180 - 360 / FNavBio.Slices) * 0.5 / 180 * pi);
  Result := PointRotation(FNavBio.DirectPosition, r, 360 / FNavBio.Slices * FNavBioIndex);
end;

function TPolyPassManager.GetPass(index: Integer): TBasePass;
begin
  Result := TBasePass(FPassList[index]);
end;

constructor TPolyPassManager.Create(APolyManager: TPolyManager; ANavBioManager: TNavBioManager; AExpandDistance: TGeoFloat);
begin
  inherited Create;
  FPolyManager := APolyManager;
  FNavBioManager := ANavBioManager;
  FPassList := TCoreClassListForObj.Create;
  FExtandDistance := AExpandDistance;
  FPassStateIncremental := 0;
end;

destructor TPolyPassManager.Destroy;
begin
  Clear;
  DisposeObject(FPassList);
  inherited;
end;

function TPolyPassManager.PointOk(AExpandDist: TGeoFloat; pt: TVec2): Boolean;
begin
  Result := FPolyManager.PointOk(AExpandDist, pt);
end;

function TPolyPassManager.PointOk(AExpandDist: TGeoFloat; pt: TVec2; ignore: TCoreClassListForObj): Boolean;
begin
  Result := PointOk(AExpandDist, pt);
  if Result then
      Result := FNavBioManager.PointOk(AExpandDist, pt, ignore);
end;

function TPolyPassManager.LineIntersect(AExpandDist: TGeoFloat; lb, le: TVec2): Boolean;
begin
  Result := FPolyManager.LineIntersect(AExpandDist, lb, le);
end;

function TPolyPassManager.LineIntersect(AExpandDist: TGeoFloat; lb, le: TVec2; ignore: TCoreClassListForObj): Boolean;
begin
  Result := LineIntersect(AExpandDist, lb, le);
  if not Result then
      Result := FNavBioManager.LineIntersect(AExpandDist, lb, le, ignore);
end;

function TPolyPassManager.PointOkCache(AExpandDist: TGeoFloat; poly1: TPolyManagerChildren; idx1: Integer): Boolean;
begin
  if FPointInCache[poly1.index][idx1] = csUnCalc then
    begin
      Result := PointOk(AExpandDist - 1, poly1.Expands[idx1, AExpandDist + 1]);

      if Result then
          FPointInCache[poly1.index][idx1] := csYes
      else
          FPointInCache[poly1.index][idx1] := csNo;
    end
  else
    begin
      Result := (FPointInCache[poly1.index][idx1] = csYes);
    end;
end;

function TPolyPassManager.LineIntersectCache(AExpandDist: TGeoFloat;
  poly1: TPolyManagerChildren; idx1: Integer; poly2: TPolyManagerChildren; idx2: Integer): Boolean;
begin
  if FIntersectCache[poly1.index][idx1][poly2.index][idx2] = csUnCalc then
    begin
      Result := LineIntersect(AExpandDist, poly1.Expands[idx1, AExpandDist + 1], poly2.Expands[idx2, AExpandDist + 1]);

      if Result then
        begin
          FIntersectCache[poly1.index][idx1][poly2.index][idx2] := csYes;
          FIntersectCache[poly2.index][idx2][poly1.index][idx1] := csYes;
        end
      else
        begin
          FIntersectCache[poly1.index][idx1][poly2.index][idx2] := csNo;
          FIntersectCache[poly2.index][idx2][poly1.index][idx1] := csNo;
        end;
    end
  else
    begin
      Result := (FIntersectCache[poly1.index][idx1][poly2.index][idx2] = csYes);
    end;
end;

function TPolyPassManager.ExistsNavBio(NavBio: TNavBio): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if (pass[i] is TNavigationBioPass) and (TNavigationBioPass(pass[i]).FNavBio = NavBio) then
        Exit;
  Result := False;
end;

procedure TPolyPassManager.AddNavBio(NavBio: TNavBio);
var
  i: Integer;
  d: TNavigationBioPass;
begin
  if ExistsNavBio(NavBio) then
      Exit;

  DeleteNavBio(NavBio);

  for i := 0 to NavBio.Slices - 1 do
    begin
      d := TNavigationBioPass.Create(Self, NavBio, i, FExtandDistance + 1, False);
      FPassList.Add(d);
    end;
end;

procedure TPolyPassManager.AddFastNavBio(NavBio: TNavBio);
var
  i: Integer;
  d: TNavigationBioPass;
  List: TCoreClassListForObj;
begin
  DeleteNavBio(NavBio);

  List := TCoreClassListForObj.Create;

  for i := 0 to NavBio.Slices - 1 do
    begin
      d := TNavigationBioPass.Create(Self, NavBio, i, FExtandDistance + 1, True);
      FPassList.Add(d);
      List.Add(d);
    end;

  for i := 0 to List.Count - 1 do
      TNavigationBioPass(List[i]).BuildPass(FExtandDistance - 1);

  DisposeObject(List);
end;

procedure TPolyPassManager.DeleteNavBio(NavBio: TNavBio);
var
  i: Integer;
begin
  i := 0;
  while i < Count do
    begin
      if (pass[i] is TNavigationBioPass) and (TNavigationBioPass(pass[i]).FNavBio = NavBio) then
          pass[i].Delete
      else
          inc(i);
    end;
end;

procedure TPolyPassManager.BuildNavBioPass;
var
  i: Integer;
  p: TNavBio;
begin
  for i := 0 to FNavBioManager.Count - 1 do
    begin
      p := FNavBioManager[i];
      if (p.IsStatic) then
          AddNavBio(p)
      else
          DeleteNavBio(p);
    end;
  for i := 0 to Count - 1 do
    begin
      if (pass[i] is TNavigationBioPass) and (TNavigationBioPass(pass[i]).FNeedUpdate) then
          pass[i].BuildPass(FExtandDistance - 1);
    end;
end;

procedure TPolyPassManager.BuildPass;

  function ReBuildCalcCache: Boolean;

  type
    TCache = array of array of TCacheState;
    PCache = ^TCache;

    procedure Build(const ACache: PCache);
    var
      i, j: Integer;
    begin
      SetLength(ACache^, FPolyManager.Count + 1);

      SetLength(ACache^[0], FPolyManager.Scene.Count);
      for i := 0 to FPolyManager.Scene.Count - 1 do
          ACache^[0][i] := csUnCalc;

      for i := 0 to FPolyManager.Count - 1 do
        begin
          SetLength(ACache^[1 + i], FPolyManager[i].Count);
          for j := 0 to FPolyManager[i].Count - 1 do
              ACache^[1 + i][j] := csUnCalc;
        end;
    end;

  var
    i, j: Integer;
    needRebuild: Boolean;
  begin
    needRebuild := False;

    if length(FIntersectCache) <> FPolyManager.Count + 1 then
        needRebuild := True;

    if not needRebuild then
      begin
        for i := 0 to FPolyManager.Count - 1 do
          begin
            if length(FIntersectCache[i + 1]) <> FPolyManager[i].Count then
              begin
                needRebuild := True;
                Break;
              end;
          end;
      end;

    if not needRebuild then
      if (length(FIntersectCache[0]) <> FPolyManager.Scene.Count) then
          needRebuild := True;

    Result := needRebuild;
    if not needRebuild then
        Exit;

    // rebuild intersect cache
    SetLength(FIntersectCache, FPolyManager.Count + 1);

    SetLength(FIntersectCache[0], FPolyManager.Scene.Count);
    for i := 0 to FPolyManager.Scene.Count - 1 do
        Build(@FIntersectCache[0][i]);

    for i := 0 to FPolyManager.Count - 1 do
      begin
        SetLength(FIntersectCache[1 + i], FPolyManager[i].Count);
        for j := 0 to FPolyManager[i].Count - 1 do
            Build(@FIntersectCache[1 + i][j]);
      end;

    // rebuild PointIn cache
    SetLength(FPointInCache, FPolyManager.Count + 1);

    SetLength(FPointInCache[0], FPolyManager.Scene.Count);
    for i := 0 to FPolyManager.Scene.Count - 1 do
        FPointInCache[0][i] := csUnCalc;

    for i := 0 to FPolyManager.Count - 1 do
      begin
        SetLength(FPointInCache[1 + i], FPolyManager[i].Count);
        for j := 0 to FPolyManager[i].Count - 1 do
            FPointInCache[1 + i][j] := csUnCalc;
      end;
  end;

  procedure ProcessPoly(APoly: TPolyManagerChildren);
  var
    i: Integer;
    b: TPolyPass;
  begin
    for i := 0 to APoly.Count - 1 do
      if PointOkCache(FExtandDistance, APoly, i) then
        begin
          b := TPolyPass.Create(Self, APoly, i, FExtandDistance + 1);
          b.BuildPass(FExtandDistance - 1);
          FPassList.Add(b);
        end;
  end;

var
  i: Integer;
begin
  if ReBuildCalcCache() then
    begin
      Clear;
      ProcessPoly(FPolyManager.Scene);
      for i := 0 to FPolyManager.Count - 1 do
          ProcessPoly(FPolyManager[i]);
    end;
end;

procedure TPolyPassManager.Rebuild;
begin
  SetLength(FIntersectCache, 0);
  BuildPass;
  BuildNavBioPass;
end;

function TPolyPassManager.NewPassStateIncremental: ShortInt;
begin
  inc(FPassStateIncremental);
  Result := FPassStateIncremental;
end;

function TPolyPassManager.Add(AConn: TBasePass; const NowBuildPass: Boolean): Integer;
begin
  Assert(AConn.FOwner = Self);
  if (AConn is TPointPass) then
    begin
      if (PointOk(FExtandDistance - 1, AConn.GetPosition)) and (NowBuildPass) then
          AConn.BuildPass(FExtandDistance - 1);
    end
  else if (AConn is TPolyPass) then
    begin
      if (PointOkCache(FExtandDistance, TPolyPass(AConn).FPoly, TPolyPass(AConn).FPolyIndex)) and (NowBuildPass) then
          AConn.BuildPass(FExtandDistance - 1);
    end;
  Result := FPassList.Add(AConn);
end;

procedure TPolyPassManager.Delete(idx: Integer);
begin
  DisposeObject(TBasePass(FPassList[idx]));
  FPassList.Delete(idx);
end;

procedure TPolyPassManager.Delete(AConn: TBasePass);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if FPassList[i] = AConn then
      begin
        Delete(i);
        Break;
      end;
end;

procedure TPolyPassManager.Clear;
begin
  while Count > 0 do
      Delete(Count - 1);
end;

function TPolyPassManager.Count: Integer;
begin
  Result := FPassList.Count;
end;

function TPolyPassManager.IndexOf(b: TBasePass): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if FPassList[i] = b then
      begin
        Result := i;
        Break;
      end;
end;

function TPolyPassManager.TotalPassNodeCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
      Result := Result + GetPass(i).Count;
end;

procedure TNavBioProcessState.SetLastState(const Value: TLastState);
begin
  FLastState := Value;
  LastStateBounceWaitTime := 0;
end;

procedure TNavBioProcessState.Init(AOwner: TNavBio);
begin
  Owner := AOwner;
  LastState := lsDetectPoint;
  LerpTo := NULLPoint;
  LerpSpeed := 0;
  Timer := 0;
  PositionChanged := True;
  ChangeSource := csInit;
  LastProcessed := False;
  LastStateBounceWaitTime := 0;
end;

procedure TNavBioProcessState.ReleaseSelf;
begin
end;

function TNavBio.GetPosition: TVec2;
begin
  Result := FPosition;
end;

procedure TNavBio.SetPosition(const Value: TVec2);
var
  oldpt, newpt: TVec2;
begin
  if FMovement.Active then
    begin
      DirectPosition := Value;
      Exit;
    end;

  FInternalStates.LastState := lsIgnore;

  oldpt := FPosition;
  newpt := Value;

  if Owner.Smooth then
    begin
      if SmoothBouncePosition(oldpt, newpt, FInternalStates.LerpTo) then
        begin
          FInternalStates.LastState := lsDirectLerpTo;
          FInternalStates.LerpSpeed := FMovement.RollMoveRatio * FMovement.MoveSpeed;
        end
      else
        begin
        end;
      DirectPosition := newpt;
    end
  else
    begin
      if GetBouncePosition(oldpt, newpt) then
          DirectPosition := newpt;
    end;
end;

function TNavBio.GetRollAngle: TGeoFloat;
begin
  Result := FRollAngle;
end;

procedure TNavBio.SetRollAngle(const Value: TGeoFloat);
begin
  if IsNan(Value) then
      raise Exception.Create('float is nan');

  FInternalStates.PositionChanged := True;

  if not(IsEqual(Value, FRollAngle)) then
      FRollAngle := Value;
end;

procedure TNavBio.DoStartMovement;
begin
  State := nsRun;
  if FNotifyIntf <> nil then
      FNotifyIntf.DoMovementStart(Self, FMovement.ToPosition);
end;

procedure TNavBio.DoMovementDone;
begin
  State := nsStop;
  if FNotifyIntf <> nil then
      FNotifyIntf.DoMovementOver(Self);
  FMovementPath.Clear;
end;

procedure TNavBio.DoRollMovementStart;
begin

end;

procedure TNavBio.DoRollMovementOver;
begin

end;

procedure TNavBio.DoLoop;
begin

end;

procedure TNavBio.DoStop;
begin
  State := nsStop;
  if (FNotifyIntf <> nil) then
      FNotifyIntf.DoMovementOver(Self);
end;

procedure TNavBio.DoPause;
begin
  State := nsPause;
  if (FNotifyIntf <> nil) then
      FNotifyIntf.DoMovementOver(Self);
end;

procedure TNavBio.DoContinue;
begin
  State := nsRun;
  if FNotifyIntf <> nil then
      FNotifyIntf.DoMovementStart(Self, FMovement.ToPosition);
end;

procedure TNavBio.DoMovementStepChange(OldStep, NewStep: TMovementStep);
begin

end;

function TNavBio.SmoothBouncePosition(const OldPos: TVec2; var NewPos, NewLerpPos: TVec2): Boolean;
var
  LineList: T2DLineList;

  function LineIsLink: Boolean;
  var
    i: Integer;
    L1, l2: P2DLine;
  begin
    Result := True;
    if LineList.Count = 1 then
        Exit;

    L1 := LineList[0];
    Result := False;
    for i := 1 to LineList.Count - 1 do
      begin
        l2 := LineList[i];
        if l2^.Poly <> L1^.Poly then
            Exit;
        if not((L1^.PolyIndex[1] = l2^.PolyIndex[0]) or (L1^.PolyIndex[0] = l2^.PolyIndex[1]) or
          (L1^.PolyIndex[1] = l2^.PolyIndex[1]) or (L1^.PolyIndex[0] = l2^.PolyIndex[0])) then
            Exit;
        L1 := l2;
      end;
    Result := True;
  end;

  function ProcessOfLinkMode: Boolean;
  var
    dir: TVec2;
    totaldist, Dist, a1, a2, a3: TGeoFloat;
    LastPos: TVec2;
    L: T2DLine;
  begin
    LastPos := NewPos;

    totaldist := PointDistance(OldPos, LastPos);

    L := LineList.NearLine(radius + 1, LastPos)^;

    LastPos := L.ExpandPoly(radius + 1).ClosestPointFromLine(LastPos);
    Dist := PointDistance(OldPos, LastPos);

    if Dist > totaldist then
      begin
        NewLerpPos := LastPos;
        NewPos := PointLerpTo(OldPos, LastPos, totaldist);
        Result := True;
      end
    else
      begin
        totaldist := totaldist - Dist;

        dir := PointNormalize(Vec2Sub(LastPos, OldPos));
        a1 := PointAngle(L.buff[0], L.buff[1]);
        a2 := PointAngle(L.buff[1], L.buff[0]);
        a3 := PointAngle(NULLPoint, dir);

        if AngleDistance(a1, a3) < AngleDistance(a2, a3) then
          // dir to index 0..1
            LastPos := L.Poly.LerpToEndge(LastPos, totaldist, radius + 1, L.PolyIndex[0], L.PolyIndex[1])
        else
          // dir to index 1..0
            LastPos := L.Poly.LerpToEndge(LastPos, totaldist, radius + 1, L.PolyIndex[1], L.PolyIndex[0]);

        NewLerpPos := LastPos;
        NewPos := LastPos;
        Result := False;
      end;
  end;

begin
  Result := False;
  NewLerpPos := NewPos;

  if (IsFlight) or (IsRun) or (FPassManager.PointOk(radius, NewPos)) then
      Exit;

  LineList := T2DLineList.Create;
  if FPassManager.PolyManager.Collision2Circle(NewPos, radius, LineList) then
    begin
      if LineIsLink then
          Result := ProcessOfLinkMode
      else
        begin
          NewLerpPos := OldPos;
          NewPos := OldPos;
          Result := False;
        end;
    end
  else
    begin
      NewLerpPos := FPassManager.PolyManager.GetMinimumFromPointToPoly(radius + 2, NewPos);
      Result := True;
    end;
  DisposeObject(LineList);
end;

procedure TNavBio.SetDirectPosition(const Value: TVec2);
begin
  if IsNan(Value) then
      raise Exception.Create('float is nan');

  FInternalStates.PositionChanged := True;

  // if not(IsEqual(Value, FPosition)) then
  FPosition := Value;
end;

procedure TNavBio.SetState(const Value: TNavState);
var
  OldState: TNavState;
  i: Integer;
begin
  if Value <> FState then
    begin
      OldState := FState;
      FState := Value;

      if OldState = nsStatic then
        begin
          if (Assigned(FOwner.OnRebuildNavBioPass)) then
              FOwner.OnRebuildNavBioPass(Self);
        end;

      FInternalStates.PositionChanged := True;

      for i := 0 to FCollisionList.Count - 1 do
          TNavBio(FCollisionList[i]).FInternalStates.PositionChanged := True;
    end;
end;

procedure TNavBio.SetPhysicsPropertys(const Value: TPhysicsPropertys);
var
  i: Integer;
begin
  if FPhysicsPropertys <> Value then
    begin
      FPhysicsPropertys := Value;

      FInternalStates.PositionChanged := True;

      for i := 0 to FCollisionList.Count - 1 do
          TNavBio(FCollisionList[i]).FInternalStates.PositionChanged := True;
    end;
end;

function TNavBio.GetSlices: Integer;
begin
  Result := FOwner.FSlices;
end;

procedure TNavBio.LinkCollision(p: TNavBio; const Associated: Boolean);
var
  i: Integer;
begin
  if p <> Self then
    begin
      if Associated then
          p.LinkCollision(Self, False);

      for i := FCollisionList.Count - 1 downto 0 do
        if FCollisionList[i] = p then
            Exit;

      FCollisionList.Add(p);

      if Associated then
          Collision(p);
    end;
end;

procedure TNavBio.UnLinkCollision(p: TNavBio; const Associated: Boolean);
var
  i: Integer;
begin
  if p <> Self then
    begin
      i := 0;
      while i < FCollisionList.Count do
        begin
          if FCollisionList[i] = p then
            begin
              FCollisionList.Delete(i);
              if Associated then
                  UnCollision(p);
            end
          else
              inc(i);
        end;

      if Associated then
          p.UnLinkCollision(Self, False);
    end;
end;

constructor TNavBio.Create(AOwner: TNavBioManager; APassManager: TPolyPassManager);
begin
  inherited Create;
  FOwner := AOwner;
  FPassManager := APassManager;
  FEndgePassList := TCoreClassListForObj.Create;

  FMovementPath := TVec2List.Create;

  FPosition := Make2DPoint(0, 0);
  FRadius := FPassManager.ExtandDistance;
  FMovement := TMovementEngine.Create;
  FMovement.Intf := Self;
  FNotifyIntf := nil;

  FState := nsStop;
  FPhysicsPropertys := [];

  FInternalStates.Init(Self);

  FCollisionList := TCoreClassListForObj.Create;

  FOwner.FNavBioList.Add(Self);
end;

destructor TNavBio.Destroy;
var
  i: Integer;
begin
  DisposeObject(FMovement);
  FMovement := nil;

  DisposeObject(FMovementPath);

  while FEndgePassList.Count > 0 do
      TBasePass(FEndgePassList[0]).Delete;

  for i := 0 to FCollisionList.Count - 1 do
      TNavBio(FCollisionList[i]).UnLinkCollision(Self, False);

  i := 0;
  while i < FOwner.FNavBioList.Count do
    begin
      if Self = FOwner.FNavBioList[i] then
          FOwner.FNavBioList.Delete(i)
      else
          inc(i);
    end;

  DisposeObject(FCollisionList);
  DisposeObject(FEndgePassList);
  FInternalStates.ReleaseSelf;

  inherited Destroy;
end;

procedure TNavBio.DirectionTo(ToPosition: TVec2);
begin
  if FMovement.Active then
      FMovement.stop;
  FInternalStates.LastState := lsIgnore;

  FMovementPath.Clear;
  FMovement.Start(ToPosition);
end;

function TNavBio.MovementTo(ToPosition: TVec2): Boolean;
var
  nsf: TNavStepFinding;
begin
  if FMovement.Active then
      FMovement.stop;

  FInternalStates.LastState := lsIgnore;

  FMovementPath.Clear;

  if IsFlight then
    begin
      FMovementPath.Add(Position);
      FMovementPath.Add(ToPosition);
      FMovement.Start(FMovementPath);
      Result := True;
      Exit;
    end;
  Result := False;

  nsf := TNavStepFinding.Create(PassManager);
  if nsf.PassManager.Count = 0 then
      nsf.PassManager.Rebuild;

  if not nsf.PassManager.PointOk(radius, ToPosition) then
      ToPosition := nsf.PassManager.PolyManager.GetMinimumFromPointToPoly(radius + 1, ToPosition);

  if not nsf.PassManager.PointOk(radius, DirectPosition) then
      DirectPosition := nsf.PassManager.PolyManager.GetMinimumFromPointToPoly(radius + 1, DirectPosition);

  if nsf.FindPath(Self, ToPosition) then
    begin
      while not nsf.FindingPathOver do
          nsf.NextStep;
      if (nsf.Success) then
        begin
          nsf.MakeLevel4OptimizationPath(FMovementPath);
          Result := True;
        end;
    end;
  DisposeObject(nsf);

  if Result then
      FMovement.Start(FMovementPath);
end;

procedure TNavBio.stop;
begin
  FMovement.stop;
end;

procedure TNavBio.Hold;
begin
  State := nsStatic;
end;

function TNavBio.GetBouncePosition(const OldPos: TVec2; var NewPos: TVec2): Boolean;
var
  LineList: T2DLineList;

  function LineIsLink: Boolean;
  var
    i: Integer;
    L1, l2: P2DLine;
  begin
    Result := True;
    if LineList.Count = 1 then
        Exit;

    L1 := LineList[0];
    Result := False;
    for i := 1 to LineList.Count - 1 do
      begin
        l2 := LineList[i];
        if l2^.Poly <> L1^.Poly then
            Exit;
        if not((L1^.PolyIndex[1] = l2^.PolyIndex[0]) or (L1^.PolyIndex[0] = l2^.PolyIndex[1]) or
          (L1^.PolyIndex[1] = l2^.PolyIndex[1]) or (L1^.PolyIndex[0] = l2^.PolyIndex[0])) then
            Exit;
        L1 := l2;
      end;
    Result := True;
  end;

  function ProcessOfLinkMode: Boolean;
  var
    dir: TVec2;
    totaldist, Dist, a1, a2, a3: TGeoFloat;
    LastPos: TVec2;
    L: T2DLine;
  begin
    LastPos := NewPos;

    totaldist := PointDistance(OldPos, LastPos);

    L := LineList.NearLine(radius + 1, LastPos)^;

    LastPos := L.ExpandPoly(radius + 1).ClosestPointFromLine(LastPos);

    Dist := PointDistance(OldPos, LastPos);

    if (Dist <= totaldist) and (Dist > 0) then
      begin
        totaldist := totaldist - Dist;

        dir := PointNormalize(Vec2Sub(LastPos, OldPos));
        a1 := PointAngle(L.buff[0], L.buff[1]);
        a2 := PointAngle(L.buff[1], L.buff[0]);
        a3 := PointAngle(NULLPoint, dir);

        if AngleDistance(a1, a3) < AngleDistance(a2, a3) then
          // dir to index 0..1
            LastPos := L.Poly.LerpToEndge(LastPos, totaldist, radius + 1, L.PolyIndex[0], L.PolyIndex[1])
        else
          // dir to index 1..0
            LastPos := L.Poly.LerpToEndge(LastPos, totaldist, radius + 1, L.PolyIndex[1], L.PolyIndex[0]);
      end;

    NewPos := LastPos;
    Result := True;
  end;

begin
  Result := True;

  if (IsFlight) or (IsRun) or (FPassManager.PointOk(radius, NewPos)) then
      Exit;

  LineList := T2DLineList.Create;
  if FPassManager.PolyManager.Collision2Circle(NewPos, radius, LineList) then
    begin
      if LineIsLink then
          Result := ProcessOfLinkMode
      else
          Result := False;
    end
  else
    begin
      NewPos := FPassManager.PolyManager.GetMinimumFromPointToPoly(radius + 2, NewPos);
      Result := True;
    end;
  DisposeObject(LineList);
end;

procedure TNavBio.ProcessBounce(p1: TNavBio; deltaTime: Double);
var
  d: TGeoFloat;
  pt: TVec2;
begin
  if (not CanBounce) or (not p1.CanBounce) then
      Exit;

  if p1.FInternalStates.LastState = lsDirectLerpTo then
    begin
      if p1.FInternalStates.LastStateBounceWaitTime + deltaTime > Owner.BounceWaitTime then
          p1.FInternalStates.LastState := lsDetectPoint
      else
        begin
          p1.FInternalStates.LastStateBounceWaitTime := p1.FInternalStates.LastStateBounceWaitTime + deltaTime;
          Exit;
        end;
    end;
  if FInternalStates.LastState = lsDirectLerpTo then
    begin
      if FInternalStates.LastStateBounceWaitTime + deltaTime > Owner.BounceWaitTime then
          FInternalStates.LastState := lsDetectPoint
      else
        begin
          FInternalStates.LastStateBounceWaitTime := FInternalStates.LastStateBounceWaitTime + deltaTime;
          Exit;
        end;
    end;

  if not CircleCollision(DirectPosition, p1.DirectPosition, radius, p1.radius) then
      Exit;

  FInternalStates.LastStateBounceWaitTime := 0;

  if (IsRun) and (p1.IsRun) then
    begin
    end
  else if (not p1.IsRun) and (p1.IsStop) and (p1.FInternalStates.LastState <> lsDirectLerpTo) then
    begin
      if IsEqual(p1.DirectPosition, DirectPosition) then
        begin
          d := (radius + p1.radius + 1);
          pt := PointLerpTo(p1.DirectPosition, PointRotation(p1.DirectPosition, 1, -p1.RollAngle), -d);
        end
      else
        begin
          d := (radius + p1.radius + 1) - PointDistance(p1.DirectPosition, DirectPosition);
          pt := PointLerpTo(p1.DirectPosition, DirectPosition, -d);
        end;

      if p1.GetBouncePosition(p1.DirectPosition, pt) then
        begin
          p1.FInternalStates.ChangeSource := csBounce;
          if (Owner.Smooth) then
            begin
              p1.FInternalStates.LastState := lsDirectLerpTo;
              p1.FInternalStates.LerpTo := pt;
              p1.FInternalStates.LerpSpeed := p1.FMovement.RollMoveRatio * p1.FMovement.MoveSpeed;

              if p1.FNotifyIntf <> nil then
                  p1.FNotifyIntf.DoMovementStart(p1, pt);
            end
          else
            begin
              p1.DirectPosition := pt;
              p1.FInternalStates.LastState := lsProcessBounce;
            end;
        end
      else
        begin
          p1.FInternalStates.LastState := lsDetectPoint;
        end;
    end;
end;

procedure TNavBio.Progress(deltaTime: Double);
var
  i: Integer;
  p: TNavBio;
  d: TGeoFloat;
begin
  FMovement.Progress(deltaTime);

  FInternalStates.ChangeSource := csExternal;

  if (IsRun) or (FMovement.Active) then
    begin
      if (not PassManager.PointOk(radius, DirectPosition)) and (not IsFlight) then
        begin
          FInternalStates.ChangeSource := csBounce;
          DirectPosition := PassManager.PolyManager.GetMinimumFromPointToPoly(radius + 2, DirectPosition);
        end;

      if IsFlight then
        begin
          if (CollisionList.Count > 0) and (CanBounce) then
            for i := 0 to CollisionList.Count - 1 do
              begin
                p := CollisionList[i] as TNavBio;
                if p.IsFlight then
                    ProcessBounce(p, deltaTime);
              end;
        end
      else
        begin
          if (CollisionList.Count > 0) and (CanBounce) then
            for i := 0 to CollisionList.Count - 1 do
              begin
                p := CollisionList[i] as TNavBio;
                if not p.IsFlight then
                    ProcessBounce(p, deltaTime);
              end;
        end;

      if FNotifyIntf <> nil then
          FNotifyIntf.DoMovementProcess(deltaTime);

    end
  else
    begin
      if IsStatic then
        begin
          if (FEndgePassList.Count = 0) and (FCollisionList.Count = 0) then
            begin
              if Assigned(FOwner.OnRebuildNavBioPass) then
                  FOwner.OnRebuildNavBioPass(Self);
            end
          else
            begin
              d := 4;
              FRadius := FRadius + d;
              if (CollisionList.Count > 0) and (CanBounce) then
                for i := 0 to CollisionList.Count - 1 do
                  begin
                    ProcessBounce(CollisionList[i] as TNavBio, deltaTime);
                  end;
              FRadius := FRadius - d;
            end;
        end
      else if (IsStop) then
        begin
          case FInternalStates.LastState of
            lsDetectPoint:
              begin
                if not IsFlight then
                  begin
                    if (FPassManager.PointOk(radius, DirectPosition)) then
                      begin
                        FInternalStates.LastState := lsProcessBounce;
                      end
                    else
                      begin
                        FInternalStates.ChangeSource := csBounce;
                        if Owner.Smooth then
                          begin
                            FInternalStates.LastState := lsDirectLerpTo;
                            FInternalStates.LerpTo := FPassManager.PolyManager.GetMinimumFromPointToPoly(radius + 1, DirectPosition);
                            FInternalStates.LerpSpeed := FMovement.RollMoveRatio * FMovement.MoveSpeed;
                          end
                        else
                          begin
                            DirectPosition := FPassManager.PolyManager.GetMinimumFromPointToPoly(radius + 1, DirectPosition);
                            FInternalStates.LastState := lsProcessBounce;
                          end;
                      end;
                  end
                else
                  begin
                    FInternalStates.LastState := lsProcessBounce;
                  end;
              end;
            lsDirectLerpTo:
              begin
                FInternalStates.ChangeSource := csBounce;
                d := PointDistance(DirectPosition, FInternalStates.LerpTo);
                if FInternalStates.LerpSpeed * deltaTime >= d then
                  begin
                    RollAngle := SmoothAngle(RollAngle, CalcAngle(DirectPosition, FInternalStates.LerpTo),
                      FMovement.RollSpeed * deltaTime);

                    DirectPosition := FInternalStates.LerpTo;
                    FInternalStates.LastState := lsProcessBounce;

                    if FNotifyIntf <> nil then
                        FNotifyIntf.DoMovementOver(Self);
                  end
                else
                  begin
                    RollAngle := SmoothAngle(RollAngle, CalcAngle(DirectPosition, FInternalStates.LerpTo),
                      FMovement.RollSpeed * deltaTime);

                    DirectPosition := PointLerpTo(DirectPosition, FInternalStates.LerpTo, FInternalStates.LerpSpeed * deltaTime);
                  end;

                if IsFlight then
                  begin
                    if (CollisionList.Count > 0) and (CanBounce) then
                      for i := 0 to CollisionList.Count - 1 do
                        begin
                          p := CollisionList[i] as TNavBio;
                          if p.IsFlight then
                              ProcessBounce(p, deltaTime);
                        end;
                  end
                else
                  begin
                    if (CollisionList.Count > 0) and (CanBounce) then
                      for i := 0 to CollisionList.Count - 1 do
                        begin
                          p := CollisionList[i] as TNavBio;
                          if (not p.IsFlight) then
                              ProcessBounce(p, deltaTime);
                        end;
                  end;

                if FNotifyIntf <> nil then
                    FNotifyIntf.DoMovementProcess(deltaTime);
              end;
            lsProcessBounce:
              begin
                if IsFlight then
                  begin
                    if (CollisionList.Count > 0) and (CanBounce) then
                      for i := 0 to CollisionList.Count - 1 do
                        begin
                          p := CollisionList[i] as TNavBio;
                          if p.IsFlight then
                              ProcessBounce(p, deltaTime);
                        end;
                  end
                else
                  begin
                    if (CollisionList.Count > 0) and (CanBounce) then
                      for i := 0 to CollisionList.Count - 1 do
                        begin
                          p := CollisionList[i] as TNavBio;
                          if not p.IsFlight then
                              ProcessBounce(p, deltaTime);
                        end;
                  end;
                FInternalStates.LastState := lsIgnore;
              end;
            lsIgnore:
              begin
                if FInternalStates.PositionChanged then
                  begin
                    FInternalStates.Timer := FInternalStates.Timer + deltaTime;
                    if FInternalStates.Timer >= 0.1 then
                      begin
                        FInternalStates.Timer := 0;
                        FInternalStates.LastState := lsDetectPoint;

                        FInternalStates.PositionChanged := False;
                      end;
                  end
                else
                    FInternalStates.Timer := 0
              end;
          end;
        end;
    end;
end;

function TNavBio.IsPause: Boolean;
begin
  Result := (FState = nsPause);
end;

function TNavBio.IsStop: Boolean;
begin
  Result := (FState = nsStop);
end;

function TNavBio.IsStatic: Boolean;
begin
  Result := (FState = nsStatic);
end;

function TNavBio.IsRun: Boolean;
begin
  Result := (FState = nsRun) or (FState = nsPause);
end;

function TNavBio.IsFlight: Boolean;
begin
  if IsStatic then
      Result := False
  else
      Result := (npFlight in FPhysicsPropertys);
end;

function TNavBio.CanBounce: Boolean;
begin
  if IsStatic then
      Result := True
  else if (npNoBounce in FPhysicsPropertys) then
      Result := False
  else
      Result := True;
end;

function TNavBio.CanCollision: Boolean;
begin
  if IsStatic then
      Result := True
  else if (npIgnoreCollision in FPhysicsPropertys) then
      Result := False
  else
      Result := True;
end;

procedure TNavBio.Collision(Trigger: TNavBio);
begin
end;

procedure TNavBio.UnCollision(Trigger: TNavBio);
begin
end;

function TNavBio.ExistsCollision(v: TNavBio): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to FCollisionList.Count - 1 do
    if FCollisionList[i] = v then
        Exit;
  Result := False;
end;

function TNavBioManager.GetItems(index: Integer): TNavBio;
begin
  Result := FNavBioList[index] as TNavBio;
end;

constructor TNavBioManager.Create;
begin
  inherited Create;
  FNavBioList := TCoreClassListForObj.Create;
  FBounceWaitTime := 5.0;
  FSlices := 5;
  FSmooth := True;
  FIgnoreAllCollision := False;

  FOnRebuildNavBioPass := nil;
end;

destructor TNavBioManager.Destroy;
begin
  Clear;
  DisposeObject(FNavBioList);
  inherited Destroy;
end;

function TNavBioManager.Add(PassManager: TPolyPassManager; pt: TVec2; angle: TGeoFloat): TNavBio;
begin
  Result := TNavBio.Create(Self, PassManager);
  Result.FPosition := pt;
  Result.FRollAngle := angle;
end;

procedure TNavBioManager.Clear;
begin
  while FNavBioList.Count > 0 do
      DisposeObject(TNavBio(FNavBioList[0]));
end;

function TNavBioManager.Count: Integer;
begin
  Result := FNavBioList.Count;
end;

function TNavBioManager.ActivtedCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if Items[i].IsActived then
        inc(Result);
end;

function TNavBioManager.PointOk(AExpandDist: TGeoFloat; pt: TVec2; ignore: TCoreClassListForObj): Boolean;
  function IsIgnore(v: TNavBio): Boolean;
  begin
    Result := (ignore <> nil) and (ignore.IndexOf(v) >= 0);
  end;

var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if not IsIgnore(Items[i]) then
      with Items[i] do
        if (IsStatic) and (Detect_Circle2Circle(pt, DirectPosition, AExpandDist, radius)) then
          begin
            Result := False;
            Exit;
          end;
  Result := True;
end;

function TNavBioManager.PointOk(AExpandDist: TGeoFloat; pt: TVec2): Boolean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    with Items[i] do
      if (IsStatic) and (Detect_Circle2Circle(pt, DirectPosition, AExpandDist, radius)) then
        begin
          Result := False;
          Exit;
        end;
  Result := True;
end;

function TNavBioManager.LineIntersect(AExpandDist: TGeoFloat; lb, le: TVec2; ignore: TCoreClassListForObj): Boolean;
  function IsIgnore(v: TNavBio): Boolean;
  begin
    Result := (ignore <> nil) and (ignore.IndexOf(v) >= 0);
  end;

var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if not IsIgnore(Items[i]) then
      with Items[i] do
        if (IsStatic) and (Detect_Circle2Line(DirectPosition, radius + AExpandDist, lb, le)) then
          begin
            Result := True;
            Exit;
          end;
  Result := False;
end;

function TNavBioManager.LineIntersect(AExpandDist: TGeoFloat; lb, le: TVec2): Boolean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    with Items[i] do
      if (IsStatic) and (Detect_Circle2Line(DirectPosition, radius + AExpandDist, lb, le)) then
        begin
          Result := True;
          Exit;
        end;
  Result := False;
end;

function TNavBioManager.DetectCollision(p1, p2: TNavBio): Boolean;
var
  r1, r2: TGeoFloat;
begin
  if p1.IsStatic then
      r1 := p1.radius + 4
  else
      r1 := p1.radius;

  if p2.IsStatic then
      r2 := p2.radius + 4
  else
      r2 := p2.radius;

  if (p1.CanCollision) and (p2.CanCollision) then
      Result := Detect_Circle2Circle(p1.DirectPosition, p2.DirectPosition, r1, r2)
  else
      Result := False;
end;

procedure TNavBioManager.Progress(deltaTime: Double);
var
  j, i: Integer;
  p1, p2: TNavBio;
begin
  if not FIgnoreAllCollision then
    for i := 0 to FNavBioList.Count - 1 do
      begin
        p1 := FNavBioList[i] as TNavBio;
        p1.FInternalStates.LastProcessed := False;
        if p1.CanCollision then
          for j := i + 1 to FNavBioList.Count - 1 do
            begin
              p2 := FNavBioList[j] as TNavBio;
              if (p1.PositionChanged) or (p2.PositionChanged) then
                begin
                  if DetectCollision(p1, p2) then
                      p1.LinkCollision(p2, True)
                  else
                      p1.UnLinkCollision(p2, True);
                end;
            end;
      end;

  for i := 0 to FNavBioList.Count - 1 do
    begin
      p1 := FNavBioList[i] as TNavBio;
      if FIgnoreAllCollision then
          p1.FInternalStates.LastProcessed := False;
      p1.Progress(deltaTime);
      p1.FInternalStates.LastProcessed := True;
    end;
end;

end. 
 
 
 
