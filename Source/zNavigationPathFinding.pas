{ ****************************************************************************** }
{ * Decision imp create by qq600585                                            * }
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
unit zNavigationPathFinding;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, zNavigationPass, Geometry2DUnit;

type
  TNavStepFinding = class(TCoreClassPersistent)
  private type
    TStepStackData = record
      PassIndex: Integer;
    end;

    PStepStackData = ^TStepStackData;

    TDecisionInt = -1 .. 1;
  private
    FPassManager: TPolyPassManager;
    FStackList: TCoreClassList;
    FSourcePositionPass, FTargetPositionPass: TPointPass;
    FSourcePositionPassIndex, FTargetPositionPassIndex: Integer;
    FSourcePosition, FTargetPosition: TVec2;
    FCurrentPassIndex: Integer;
    FPassStateID: ShortInt;
    FStepCount: Int64;
    FDone: Boolean;
    FAbort: Boolean;
    FIgnoreDynamicPoly: TCoreClassListForObj;

    procedure InitState;
    procedure FreeState;
    procedure ResetState;

    procedure PushState;
    procedure PopState;
    function IsEmptyStack: Boolean;
  private
    {
      return state
      -1, prev step
      0: next step
      1: done to end position
    }
    function Decision(const AStateID: ShortInt; const b, E: Integer; out PassIndex: Integer): TDecisionInt;
  public
    constructor Create(APassManager: TPolyPassManager);
    destructor Destroy; override;

    function FindPath(ASour: TNavBio; ADest: TVec2): Boolean;

    procedure ResetStep;
    procedure NextStep;
    function FindingPathOver: Boolean;

    procedure MakeCurrentPath(OutPath: TVec2List);
    // Remove more/spam corner nodes
    procedure MakeLevel1OptimizationPath(OutPath: TVec2List);
    // remmove intersect corner nodes
    procedure MakeLevel2OptimizationPath(OutPath: TVec2List);
    // remove near lerp corner nodes
    procedure MakeLevel3OptimizationPath(OutPath: TVec2List);
    // subdivision and lerp
    procedure MakeLevel4OptimizationPath(OutPath: TVec2List);

    function GetSearchDepth: Integer;
    function GetStepCount: Integer;

    property PassManager: TPolyPassManager read FPassManager;
    property Done: Boolean read FDone;
    property Abort: Boolean read FAbort;
    function Success: Boolean;
  end;

procedure Level1OptimizationPath(CM: TPolyPassManager; ignore: TCoreClassListForObj; APath: TVec2List; ARadius: TGeoFloat);
procedure Level2OptimizationPath(CM: TPolyPassManager; ignore: TCoreClassListForObj; APath: TVec2List; ARadius: TGeoFloat; LowLap: Boolean);
procedure Level3OptimizationPath(CM: TPolyPassManager; ignore: TCoreClassListForObj; APath: TVec2List; ARadius: TGeoFloat; LowLap: Boolean);
procedure Level4OptimizationPath(CM: TPolyPassManager; ignore: TCoreClassListForObj; APath: TVec2List; ARadius: TGeoFloat; LowLap: Boolean);

implementation

procedure Level1OptimizationPath(CM: TPolyPassManager; ignore: TCoreClassListForObj; APath: TVec2List; ARadius: TGeoFloat);
var
  i, j: Integer;
  pl: TVec2List;
begin
  pl := TVec2List.Create;
  pl.Assign(APath);
  APath.Clear;

  if pl.Count > 3 then
    begin
      i := 0;
      while i < pl.Count do
        begin
          APath.Add(pl[i]^);
          j := pl.Count - 1;
          while j > i do
            begin
              if not CM.LineIntersect(ARadius, pl[i]^, pl[j]^, ignore) then
                begin
                  APath.Add(pl[j]^);
                  i := j;
                  Break;
                end
              else
                  dec(j);
            end;
          inc(i);
        end;
    end
  else
      APath.Assign(pl);
  DisposeObject(pl);
end;

procedure Level2OptimizationPath(CM: TPolyPassManager; ignore: TCoreClassListForObj; APath: TVec2List; ARadius: TGeoFloat; LowLap: Boolean);
var
  pl: TVec2List;
  b, E, ipt: TVec2;
  idx1, idx2: Integer;
begin
  pl := TVec2List.Create;
  pl.Assign(APath);
  APath.Clear;
  if LowLap then
      Level1OptimizationPath(CM, ignore, pl, ARadius);

  if pl.Count >= 3 then
    begin
      b := pl[0]^;
      pl.Delete(0);
      APath.Add(b);
      while pl.Count > 0 do
        begin
          E := pl[0]^;
          pl.Delete(0);

          if (pl.Line2NearIntersect(b, E, False, idx1, idx2, ipt)) then
            begin
              APath.Add(ipt);
              E := pl[idx2]^;
              APath.Add(E);
              b := E;
              while idx2 > 0 do
                begin
                  pl.Delete(0);
                  dec(idx2);
                end;
            end
          else
            begin
              APath.Add(E);
              b := E;
            end;
        end;
      Level1OptimizationPath(CM, ignore, APath, ARadius);
    end
  else
      APath.Assign(pl);
  DisposeObject(pl);
end;

procedure Level3OptimizationPath(CM: TPolyPassManager; ignore: TCoreClassListForObj; APath: TVec2List; ARadius: TGeoFloat; LowLap: Boolean);
  function LerpLineCheck(sour, E, b: TVec2; out OutPt: TVec2): Boolean;
  var
    i: Integer;
    ne: TVec2;
    SmoothLevel: Integer;
  begin
    Result := False;
    SmoothLevel := Trunc(PointDistance(b, E) / (ARadius * 2));
    for i := 1 to SmoothLevel - 1 do
      begin
        ne := PointLerp(b, E, i * (1.0 / SmoothLevel));
        if not CM.LineIntersect(ARadius, sour, ne, ignore) then
          begin
            OutPt := ne;
            Result := True;
            Exit;
          end;
      end;
  end;

  function LerpCheck(sour: TVec2; pl: TVec2List; out OutPt: TVec2; out NextIdx: Integer): Boolean;
  var
    i: Integer;
    b, E: TVec2;
  begin
    Result := False;
    if (pl.Count > 1) then
      begin
        b := pl[0]^;
        for i := 1 to pl.Count - 1 do
          begin
            E := pl[i]^;
            if (PointDistance(b, E) > (ARadius * 2)) and (LerpLineCheck(sour, b, E, OutPt)) then
              begin
                NextIdx := i;
                Result := True;
                Exit;
              end;
            b := E;
          end;
      end;
  end;

var
  pl: TVec2List;
  b, ipt: TVec2;
  idx: Integer;
begin
  pl := TVec2List.Create;
  pl.Assign(APath);
  APath.Clear;
  if LowLap then
      Level2OptimizationPath(CM, ignore, pl, ARadius, True);

  if pl.Count >= 3 then
    begin
      while pl.Count > 0 do
        begin
          b := pl[0]^;
          APath.Add(b);
          pl.Delete(0);

          if LerpCheck(b, pl, ipt, idx) then
            begin
              APath.Add(ipt);
              APath.Add(pl[idx]^);
              while idx > 0 do
                begin
                  pl.Delete(0);
                  dec(idx);
                end;
            end;
        end;

      pl.Assign(APath);
      APath.Clear;
      while pl.Count > 0 do
        begin
          b := pl[0]^;
          APath.Add(b);
          pl.Delete(0);

          if LerpCheck(b, pl, ipt, idx) then
            begin
              APath.Add(ipt);
              APath.Add(pl[idx]^);
              while idx > 0 do
                begin
                  pl.Delete(0);
                  dec(idx);
                end;
            end;
        end;

      Level1OptimizationPath(CM, ignore, APath, ARadius);
    end
  else
      APath.Assign(pl);
  DisposeObject(pl);
end;

procedure Level4OptimizationPath(CM: TPolyPassManager; ignore: TCoreClassListForObj; APath: TVec2List; ARadius: TGeoFloat; LowLap: Boolean);
var
  pl, pl2: TVec2List;
  i: Integer;
begin
  pl := TVec2List.Create;
  pl.Assign(APath);
  if LowLap then
    begin
      Level3OptimizationPath(CM, ignore, pl, ARadius, True);
      pl.Reverse;
      Level3OptimizationPath(CM, ignore, pl, ARadius, False);
      pl.Reverse;
    end;
  APath.Clear;

  if pl.Count > 0 then
    begin
      pl2 := TVec2List.Create;
      for i := 0 to pl.Count - 1 do
          pl2.AddSubdivision(10, pl[i]^);
      Level3OptimizationPath(CM, ignore, pl2, ARadius, False);
      pl2.Reverse;
      Level3OptimizationPath(CM, ignore, pl2, ARadius, False);
      pl2.Reverse;

      pl.Clear;
      for i := 0 to pl2.Count - 1 do
          pl.AddSubdivision(10, pl2[i]^);
      Level3OptimizationPath(CM, ignore, pl, ARadius, False);
      pl.Reverse;
      Level3OptimizationPath(CM, ignore, pl, ARadius, False);
      pl.Reverse;

      APath.Assign(pl);
      DisposeObject(pl2);
    end;
  DisposeObject(pl);
end;

procedure TNavStepFinding.InitState;
begin
  FStackList := TCoreClassList.Create;
  FSourcePositionPass := nil;
  FTargetPositionPass := nil;
  FSourcePositionPassIndex := -1;
  FTargetPositionPassIndex := -1;
  FSourcePosition := NULLPoint;
  FTargetPosition := NULLPoint;
  FCurrentPassIndex := -1;
  FPassStateID := 0;
  FStepCount := 0;
  FDone := False;
  FAbort := False;
  FIgnoreDynamicPoly := TCoreClassListForObj.Create;
end;

procedure TNavStepFinding.FreeState;
begin
  ResetState;
  DisposeObject(FStackList);
  DisposeObject(FIgnoreDynamicPoly);
end;

procedure TNavStepFinding.ResetState;
var
  i: Integer;
begin
  for i := 0 to FStackList.Count - 1 do
      Dispose(PStepStackData(FStackList[i]));
  FStackList.Clear;

  if FSourcePositionPass <> nil then
    begin
      FSourcePositionPass.Delete;
    end;
  FSourcePositionPass := nil;

  if FTargetPositionPass <> nil then
    begin
      FTargetPositionPass.Delete;
    end;
  FTargetPositionPass := nil;

  FSourcePositionPassIndex := -1;
  FTargetPositionPassIndex := -1;
  FSourcePosition := NULLPoint;
  FTargetPosition := NULLPoint;

  FCurrentPassIndex := -1;
  FPassStateID := 0;
  FStepCount := 0;
  FDone := False;
  FAbort := False;

  FIgnoreDynamicPoly.Clear;
end;

procedure TNavStepFinding.PushState;
var
  p: PStepStackData;
begin
  new(p);
  p^.PassIndex := FCurrentPassIndex;
  FStackList.Add(p);
end;

procedure TNavStepFinding.PopState;
var
  p: PStepStackData;
begin
  if FStackList.Count > 0 then
    begin
      p := FStackList[FStackList.Count - 1];
      FCurrentPassIndex := p^.PassIndex;
      FStackList.Delete(FStackList.Count - 1);
      Dispose(p);
    end;
end;

function TNavStepFinding.IsEmptyStack: Boolean;
begin
  Result := FStackList.Count = 0;
end;

{
  return state
  -1, prev step
  0: next step
  1: done to end position
}

function TNavStepFinding.Decision(const AStateID: ShortInt; const b, E: Integer; out PassIndex: Integer): TDecisionInt;
var
  BC, eC: TBasePass;
  bp, EP: TVec2;

  Bi: Integer;
  d, BD: TGeoFloat;
  i: Integer;
begin
  if (b < 0) or (E < 0) then
    begin
      // return prev step
      Result := -1;
      Exit;
    end;

  BC := FPassManager[b];
  eC := FPassManager[E];

  if BC.Exists(eC) and (BC[BC.IndexOf(eC)]^.Enabled(FIgnoreDynamicPoly)) then
    begin
      // return success
      PassIndex := eC.PassIndex;
      Result := 1;
      Exit;
    end;

  bp := BC.GetPosition;
  EP := eC.GetPosition;

  Bi := -1;

  BD := 0;

  // compute distance
  for i := 0 to BC.Count - 1 do
    with BC.Data[i]^ do
      if (State <> AStateID) and (passed <> BC) and (Enabled(FIgnoreDynamicPoly)) then
        begin
          d := PointDistance(EP, passed.GetPosition);

          if (BD = 0) or (d < BD) then
            begin
              Bi := i;
              BD := d;
            end;
        end;

  if (Bi = -1) then
    begin
      // return prev step
      Result := -1;
      Exit;
    end;

  BC.State[Bi] := AStateID;
  PassIndex := BC.Data[Bi]^.passed.PassIndex;
  if PassIndex < 0 then
    begin
      // step prev step
      Result := -1;
      Exit;
    end;

  // next step
  Result := 0;
end;

constructor TNavStepFinding.Create(APassManager: TPolyPassManager);
begin
  inherited Create;
  FPassManager := APassManager;
  InitState;
end;

destructor TNavStepFinding.Destroy;
begin
  FreeState;
  inherited Destroy;
end;

function TNavStepFinding.FindPath(ASour: TNavBio; ADest: TVec2): Boolean;
var
  i: Integer;
begin
  ResetState;
  if ASour.IsFlight then
    begin
      FSourcePosition := ASour.DirectPosition;
      FTargetPosition := ADest;
      FSourcePositionPass := TPointPass.Create(FPassManager, ASour.DirectPosition);
      FTargetPositionPass := TPointPass.Create(FPassManager, ADest);
      FSourcePositionPassIndex := FPassManager.Add(FSourcePositionPass, False);
      FTargetPositionPassIndex := FPassManager.Add(FTargetPositionPass, False);
      Result := True;
      Exit;
    end;

  FIgnoreDynamicPoly.Add(ASour);
  Result := False;
  FSourcePosition := ASour.DirectPosition;
  FTargetPosition := ADest;
  FDone := not FPassManager.LineIntersect(FPassManager.ExtandDistance, ASour.DirectPosition, ADest, FIgnoreDynamicPoly);

  if not FPassManager.PointOk(FPassManager.ExtandDistance - 1, ASour.DirectPosition, FIgnoreDynamicPoly) then
      Exit;
  if not FPassManager.PointOk(FPassManager.ExtandDistance - 1, ADest, FIgnoreDynamicPoly) then
      Exit;

  if FDone then
    begin
      FSourcePositionPass := TPointPass.Create(FPassManager, ASour.DirectPosition);
      FTargetPositionPass := TPointPass.Create(FPassManager, ADest);
      FSourcePositionPassIndex := FPassManager.Add(FSourcePositionPass, False);
      FTargetPositionPassIndex := FPassManager.Add(FTargetPositionPass, False);
      Result := True;
    end
  else
    begin
      FSourcePositionPass := TPointPass.Create(FPassManager, ASour.DirectPosition);
      FTargetPositionPass := TPointPass.Create(FPassManager, ADest);
      FSourcePositionPassIndex := FPassManager.Add(FSourcePositionPass, True);
      FTargetPositionPassIndex := FPassManager.Add(FTargetPositionPass, True);
      for i := 0 to FPassManager.Count - 1 do
          FPassManager[i].PassIndex := i;

      FPassStateID := FPassManager.NewPassStateIncremental;

      case Decision(FPassStateID, FSourcePositionPassIndex, FTargetPositionPassIndex, FCurrentPassIndex) of
        - 1:
          begin
            // prev step
          end;
        0:
          begin
            // next step
            PushState;
            Result := True;
          end;
        else
          begin
            // done to dest
            FDone := True;
            Result := True;
          end;
      end;
    end;
end;

procedure TNavStepFinding.ResetStep;
var
  i: Integer;
begin
  if FTargetPositionPassIndex < 0 then
      Exit;
  if FSourcePositionPassIndex < 0 then
      Exit;
  if FSourcePositionPass = nil then
      Exit;
  if FTargetPositionPass = nil then
      Exit;

  FDone := False;
  FCurrentPassIndex := -1;
  FStepCount := 0;
  FDone := False;
  FAbort := False;

  for i := 0 to FStackList.Count - 1 do
      Dispose(PStepStackData(FStackList[i]));
  FStackList.Clear;

  FPassStateID := FPassManager.NewPassStateIncremental;

  case Decision(FPassStateID, FSourcePositionPassIndex, FTargetPositionPassIndex, FCurrentPassIndex) of
    - 1:
      begin
        // prev step
      end;
    0:
      begin
        // next step
        PushState;
      end;
    else
      begin
        // done to dest
        FDone := True;
      end;
  end;
end;

procedure TNavStepFinding.NextStep;
var
  r, i: Integer;
begin
  if FDone then
      Exit;
  if FAbort then
      Exit;
  if FCurrentPassIndex < 0 then
      Exit;
  if FTargetPositionPassIndex < 0 then
      Exit;
  if FSourcePositionPassIndex < 0 then
      Exit;
  if FSourcePositionPass = nil then
      Exit;
  if FTargetPositionPass = nil then
      Exit;

  inc(FStepCount);
  PushState;

  i := FCurrentPassIndex;
  r := Decision(FPassStateID, i, FTargetPositionPassIndex, FCurrentPassIndex);
  case r of
    - 1:
      begin
        // prev step
        PopState;
        if IsEmptyStack then
          begin
            FAbort := True;
            Exit;
          end;
        PopState;
      end;
    0:
      begin
        // next step
      end;
    else
      begin
        // done to dest
        FDone := True;
      end;
  end;
end;

function TNavStepFinding.FindingPathOver: Boolean;
begin
  Result := FDone or FAbort;
end;

procedure TNavStepFinding.MakeCurrentPath(OutPath: TVec2List);
var
  i: Integer;
begin
  if FSourcePositionPass = nil then
      Exit;
  if FTargetPositionPass = nil then
      Exit;
  OutPath.Add(FSourcePositionPass.GetPosition);
  for i := 0 to FStackList.Count - 1 do
      OutPath.Add(FPassManager[PStepStackData(FStackList[i])^.PassIndex].GetPosition);
  if FindingPathOver and (not FAbort) then
      OutPath.Add(FTargetPositionPass.GetPosition);
end;

procedure TNavStepFinding.MakeLevel1OptimizationPath(OutPath: TVec2List);
var
  pl: TVec2List;
begin
  if FSourcePositionPass = nil then
      Exit;
  if FTargetPositionPass = nil then
      Exit;
  pl := TVec2List.Create;
  MakeCurrentPath(pl);
  OutPath.Assign(pl);
  Level1OptimizationPath(FPassManager, FIgnoreDynamicPoly, OutPath, FPassManager.ExtandDistance);
  DisposeObject(pl);
end;

procedure TNavStepFinding.MakeLevel2OptimizationPath(OutPath: TVec2List);
var
  pl: TVec2List;
begin
  if FSourcePositionPass = nil then
      Exit;
  if FTargetPositionPass = nil then
      Exit;
  pl := TVec2List.Create;
  MakeCurrentPath(pl);
  OutPath.Assign(pl);
  Level2OptimizationPath(FPassManager, FIgnoreDynamicPoly, OutPath, FPassManager.ExtandDistance, True);
  DisposeObject(pl);
end;

procedure TNavStepFinding.MakeLevel3OptimizationPath(OutPath: TVec2List);
begin
  if FSourcePositionPass = nil then
      Exit;
  if FTargetPositionPass = nil then
      Exit;
  MakeCurrentPath(OutPath);
  Level3OptimizationPath(FPassManager, FIgnoreDynamicPoly, OutPath, FPassManager.ExtandDistance, True);
  OutPath.Reverse;
  Level3OptimizationPath(FPassManager, FIgnoreDynamicPoly, OutPath, FPassManager.ExtandDistance, True);
  OutPath.Reverse;
end;

procedure TNavStepFinding.MakeLevel4OptimizationPath(OutPath: TVec2List);
begin
  if FSourcePositionPass = nil then
      Exit;
  if FTargetPositionPass = nil then
      Exit;
  MakeCurrentPath(OutPath);
  Level4OptimizationPath(FPassManager, FIgnoreDynamicPoly, OutPath, FPassManager.ExtandDistance, True);
end;

function TNavStepFinding.GetSearchDepth: Integer;
begin
  Result := FStackList.Count;
end;

function TNavStepFinding.GetStepCount: Integer;
begin
  Result := FStepCount;
end;

function TNavStepFinding.Success: Boolean;
begin
  Result := (not Abort) and (FDone)
end;

end.
