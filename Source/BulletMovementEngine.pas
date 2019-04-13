{ ****************************************************************************** }
{ * bullet movement engine create by qq600585                                  * }
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
unit BulletMovementEngine;

{$INCLUDE zDefine.inc}

interface

uses SysUtils, Geometry2DUnit, CoreClasses, Math;

type
  TBulletMovementStep = record
    Position: TVec2;
    angle: TGeoFloat;
    index: Integer;
  end;

  IBulletMovementEngineIntf = interface
    function GetPosition: TVec2;
    procedure SetPosition(const Value: TVec2);

    function GetRollAngle: TGeoFloat;
    procedure SetRollAngle(const Value: TGeoFloat);

    procedure DoStartBulletMovement;
    procedure DoBulletMovementDone;

    procedure DoRollBulletMovementStart;
    procedure DoRollBulletMovementOver;

    procedure DoStop;
    procedure DoPause;
    procedure DoContinue;

    procedure DoBulletMovementStepChange(OldStep, NewStep: TBulletMovementStep);
    procedure doProgress(deltaTime: Double);
  end;

  TBulletMovementOperationMode = (momBulletMovementPath, momStopRollAngle);

  TBulletMovementEngine = class(TCoreClassObject)
  private
    FIntf: IBulletMovementEngineIntf;

    FSteps: array of TBulletMovementStep;

    FActive: Boolean;
    FPause: Boolean;
    FMoveSpeed: TGeoFloat;
    FRollSpeed: TGeoFloat;
    // BulletMovement operation mode
    FOperationMode: TBulletMovementOperationMode;

    FStopRollAngle: TGeoFloat;

    FLastProgressDeltaTime: Double;

    FCurrentPathStepTo: Integer;

    FFromPosition: TVec2;
    FToPosition: TVec2;
    FBulletMovementDone, FRollDone: Boolean;

    FUserObject: TCoreClassObject;
  protected
    function GetPosition: TVec2;
    procedure SetPosition(const Value: TVec2);

    function GetRollAngle: TGeoFloat;
    procedure SetRollAngle(const Value: TGeoFloat);

    function FirstStep: TBulletMovementStep;
    function LastStep: TBulletMovementStep;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start(ATo: TVec2); overload;
    procedure Start(APaths: TVec2List); overload;
    procedure Start; overload;
    procedure stop;
    procedure Pause;

    procedure Progress(const deltaTime: Double);

    property Intf: IBulletMovementEngineIntf read FIntf write FIntf;

    property Position: TVec2 read GetPosition write SetPosition;
    property RollAngle: TGeoFloat read GetRollAngle write SetRollAngle;

    // pause
    property IsPause: Boolean read FPause;

    // BulletMovementing
    property Active: Boolean read FActive;

    // speed
    property MoveSpeed: TGeoFloat read FMoveSpeed write FMoveSpeed;

    // roll speed
    property RollSpeed: TGeoFloat read FRollSpeed write FRollSpeed;

    // BulletMovement operation mode
    property OperationMode: TBulletMovementOperationMode read FOperationMode write FOperationMode;

    property FromPosition: TVec2 read FFromPosition;
    property ToPosition: TVec2 read FToPosition;
    property UserObject: TCoreClassObject read FUserObject write FUserObject;
  end;

  TBulletMovementManager = class(TCoreClassObject)
  private
    FList: TCoreClassListForObj;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(BE: TBulletMovementEngine);
    procedure Remove(BE: TBulletMovementEngine);
    function Count: Integer;
    procedure Clear;
    function Exists(BE: TBulletMovementEngine): Boolean;

    function GetItems(index: Integer): TBulletMovementEngine;
    property Items[index: Integer]: TBulletMovementEngine read GetItems; default;

    procedure Progress(const deltaTime: Double);
  end;

implementation

uses MovementEngine, Geometry3DUnit;

function TBulletMovementEngine.GetPosition: TVec2;
begin
  Result := FIntf.GetPosition;
end;

procedure TBulletMovementEngine.SetPosition(const Value: TVec2);
begin
  FIntf.SetPosition(Value);
end;

function TBulletMovementEngine.GetRollAngle: TGeoFloat;
begin
  Result := FIntf.GetRollAngle;
end;

procedure TBulletMovementEngine.SetRollAngle(const Value: TGeoFloat);
begin
  FIntf.SetRollAngle(Value);
end;

function TBulletMovementEngine.FirstStep: TBulletMovementStep;
begin
  Result := FSteps[0];
end;

function TBulletMovementEngine.LastStep: TBulletMovementStep;
begin
  Result := FSteps[length(FSteps) - 1];
end;

constructor TBulletMovementEngine.Create;
begin
  inherited Create;
  SetLength(FSteps, 0);
  FIntf := nil;

  FActive := False;
  FPause := False;
  FMoveSpeed := 300;
  FRollSpeed := 360;
  FOperationMode := momBulletMovementPath;

  FStopRollAngle := 0;

  FLastProgressDeltaTime := 0;

  FCurrentPathStepTo := -1;

  FFromPosition := NULLPoint;
  FToPosition := NULLPoint;

  FBulletMovementDone := False;
  FRollDone := False;
  FUserObject := nil;
end;

destructor TBulletMovementEngine.Destroy;
begin
  SetLength(FSteps, 0);
  FIntf := nil;
  inherited Destroy;
end;

procedure TBulletMovementEngine.Start(ATo: TVec2);
begin
  if not FActive then
    begin
      SetLength(FSteps, 0);
      FStopRollAngle := CalcAngle(Position, ATo);
      FOperationMode := momStopRollAngle;
      FActive := True;
      FPause := False;
      FToPosition := ATo;
      Intf.DoStartBulletMovement;
    end;
end;

procedure TBulletMovementEngine.Start(APaths: TVec2List);
var
  i: Integer;
begin
  APaths.FixedSameError;

  if not FActive then
    begin
      FCurrentPathStepTo := 0;
      FFromPosition := NULLPoint;
      FBulletMovementDone := False;
      FRollDone := False;
      FOperationMode := momBulletMovementPath;

      FActive := (APaths <> nil) and (APaths.Count > 0) and (FIntf <> nil);
      if FActive then
        begin
          SetLength(FSteps, APaths.Count);
          for i := 0 to APaths.Count - 1 do
            with FSteps[i] do
              begin
                Position := APaths[i]^;
                if i > 0 then
                    angle := CalcAngle(APaths[i - 1]^, APaths[i]^)
                else
                    angle := CalcAngle(Position, APaths[i]^);
                index := i;
              end;

          FPause := False;
          FFromPosition := Position;

          FStopRollAngle := 0;

          FToPosition := APaths.Last^;
          Intf.DoStartBulletMovement;
        end;
    end;
end;

procedure TBulletMovementEngine.Start;
begin
  if (FActive) and (FPause) then
    begin
      FPause := False;
      Intf.DoContinue;
    end;
end;

procedure TBulletMovementEngine.stop;
begin
  if FActive then
    begin
      SetLength(FSteps, 0);
      FCurrentPathStepTo := 0;
      FFromPosition := NULLPoint;
      FBulletMovementDone := False;
      FRollDone := True;
      FPause := False;
      FActive := False;
      FOperationMode := momBulletMovementPath;
      Intf.DoStop;
    end;
end;

procedure TBulletMovementEngine.Pause;
begin
  if not FPause then
    begin
      FPause := True;
      if FActive then
          Intf.DoPause;
    end;
end;

procedure TBulletMovementEngine.Progress(const deltaTime: Double);
var
  CurrentDeltaTime: Double;
  toStep: TBulletMovementStep;
  FromV, ToV, v: TVec2;
  dt, RT: Double;
  d: TGeoFloat;
begin
  FLastProgressDeltaTime := deltaTime;
  if FActive then
    begin
      CurrentDeltaTime := deltaTime;
      FActive := (length(FSteps) > 0) or (FOperationMode = momStopRollAngle);
      if (not FPause) and (FActive) then
        begin

          FIntf.doProgress(CurrentDeltaTime);

          case FOperationMode of
            momStopRollAngle:
              begin
                RollAngle := SmoothAngle(RollAngle, FStopRollAngle, deltaTime * FRollSpeed);
                FActive := not AngleEqual(RollAngle, FStopRollAngle);
              end;
            momBulletMovementPath:
              begin
                FromV := Position;

                while True do
                  begin
                    if FBulletMovementDone and FRollDone then
                      begin
                        FActive := False;
                        Break;
                      end;

                    if FBulletMovementDone and not FRollDone then
                      begin
                        RollAngle := SmoothAngle(RollAngle, LastStep.angle, deltaTime * FRollSpeed);
                        FRollDone := not AngleEqual(RollAngle, LastStep.angle);
                        Break;
                      end;

                    if FCurrentPathStepTo >= length(FSteps) then
                      begin
                        v := LastStep.Position;
                        Position := v;
                        if not AngleEqual(RollAngle, LastStep.angle) then
                          begin
                            FOperationMode := momStopRollAngle;
                            FStopRollAngle := LastStep.angle;
                          end
                        else
                            FActive := False;
                        Break;
                      end;

                    toStep := FSteps[FCurrentPathStepTo];
                    ToV := toStep.Position;
                    FBulletMovementDone := FCurrentPathStepTo >= length(FSteps);

                    if (FRollDone) and (not AngleEqual(RollAngle, toStep.angle)) then
                        FIntf.DoRollBulletMovementStart;

                    if (not FRollDone) and (AngleEqual(RollAngle, toStep.angle)) then
                        FIntf.DoRollBulletMovementOver;

                    FRollDone := AngleEqual(RollAngle, toStep.angle);

                    if FRollDone then
                      begin
                        // uses direct BulletMovement

                        dt := MovementDistanceDeltaTime(FromV, ToV, FMoveSpeed);
                        if dt > CurrentDeltaTime then
                          begin
                            // direct calc BulletMovement
                            v := MovementDistance(FromV, ToV, CurrentDeltaTime * FMoveSpeed);
                            Position := v;
                            Break;
                          end
                        else
                          begin
                            CurrentDeltaTime := CurrentDeltaTime - dt;
                            FromV := ToV;
                            inc(FCurrentPathStepTo);

                            // trigger execute event
                            if (FCurrentPathStepTo < length(FSteps)) then
                                FIntf.DoBulletMovementStepChange(toStep, FSteps[FCurrentPathStepTo]);
                          end;
                      end
                    else
                      begin
                        // uses roll attenuation BulletMovement
                        RT := AngleRollDistanceDeltaTime(RollAngle, toStep.angle, FRollSpeed);
                        d := Distance(FromV, ToV);

                        if RT >= CurrentDeltaTime then
                          begin
                            if d > CurrentDeltaTime * FMoveSpeed then
                              begin
                                // position vector dont cross endge for ToV
                                v := MovementDistance(FromV, ToV, CurrentDeltaTime * FMoveSpeed);
                                Position := v;
                                RollAngle := SmoothAngle(RollAngle, toStep.angle, CurrentDeltaTime * FRollSpeed);
                                Break;
                              end
                            else
                              begin
                                // position vector cross endge for ToV
                                dt := MovementDistanceDeltaTime(FromV, ToV, FMoveSpeed);
                                v := ToV;
                                Position := v;
                                RollAngle := SmoothAngle(RollAngle, toStep.angle, dt * FRollSpeed);
                                CurrentDeltaTime := CurrentDeltaTime - dt;
                                FromV := ToV;
                                inc(FCurrentPathStepTo);

                                // trigger execute event
                                if (FCurrentPathStepTo < length(FSteps)) then
                                    FIntf.DoBulletMovementStepChange(toStep, FSteps[FCurrentPathStepTo]);
                              end;
                          end
                        else
                          begin
                            // preprocess roll BulletMovement speed attenuation
                            if RT * FMoveSpeed > d then
                              begin
                                // position vector cross endge for ToV
                                dt := MovementDistanceDeltaTime(FromV, ToV, FMoveSpeed);
                                v := ToV;
                                Position := v;
                                RollAngle := SmoothAngle(RollAngle, toStep.angle, dt * FRollSpeed);
                                CurrentDeltaTime := CurrentDeltaTime - dt;
                                FromV := ToV;
                                inc(FCurrentPathStepTo);

                                // trigger execute event
                                if (FCurrentPathStepTo < length(FSteps)) then
                                    FIntf.DoBulletMovementStepChange(toStep, FSteps[FCurrentPathStepTo]);
                              end
                            else
                              begin
                                // position vector dont cross endge for ToV
                                v := MovementDistance(FromV, ToV, RT * FMoveSpeed);
                                Position := v;
                                RollAngle := toStep.angle;
                                CurrentDeltaTime := CurrentDeltaTime - RT;
                              end;
                          end;
                      end;
                  end;
              end;
          end;

          if (not FActive) then
            begin
              FCurrentPathStepTo := 0;
              FFromPosition := NULLPoint;
              FBulletMovementDone := False;
              FRollDone := False;
              FOperationMode := momBulletMovementPath;
              FIntf.DoBulletMovementDone;
            end;
        end;
    end;
end;

constructor TBulletMovementManager.Create;
begin
  inherited Create;
  FList := TCoreClassListForObj.Create;
end;

destructor TBulletMovementManager.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

procedure TBulletMovementManager.Add(BE: TBulletMovementEngine);
begin
  if not Exists(BE) then
      FList.Add(BE);
end;

procedure TBulletMovementManager.Remove(BE: TBulletMovementEngine);
var
  i: Integer;
begin
  i := 0;
  while i < FList.Count do
    if FList[i] = BE then
        FList.Delete(i)
    else
        inc(i);
end;

function TBulletMovementManager.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TBulletMovementManager.Clear;
begin
  FList.Clear;
end;

function TBulletMovementManager.Exists(BE: TBulletMovementEngine): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to FList.Count - 1 do
    if BE = FList[i] then
        Exit;

  Result := False;
end;

function TBulletMovementManager.GetItems(index: Integer): TBulletMovementEngine;
begin
  Result := FList[index] as TBulletMovementEngine;
end;

procedure TBulletMovementManager.Progress(const deltaTime: Double);
var
  i: Integer;
  BE: TBulletMovementEngine;
begin
  i := 0;
  while i < FList.Count do
    begin
      BE := TBulletMovementEngine(FList[i]);
      BE.Progress(deltaTime);
      if BE = FList[i] then
          inc(i);
    end;
end;

end. 
 
 
 
