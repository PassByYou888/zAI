{ ****************************************************************************** }
{ * memory Rasterization with AGG support                                      * }
{ * by QQ 600585@qq.com                                                        * }
{ ****************************************************************************** }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
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
unit AggCurves;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  Math,
  AggBasics,
  AggArray,
  AggVertexSource;

type
  TAggCurveApproximationMethod = (camInc, camDiv);
  TAggCurve3PointArray         = array [0 .. 2] of TPointDouble;
  TAggCurve4PointArray         = array [0 .. 3] of TPointDouble;

  PAggCurve3Points = ^TAggCurve3Points;

  TAggCurve3Points = record
    Point: TAggCurve3PointArray;
  public
    procedure Init(x1, y1, x2, y2, x3, y3: Double); overload;
    procedure Init(Point1, Point2, Point3: TPointDouble); overload;
  end;

  PAggCurve4Points = ^TAggCurve4Points;

  TAggCurve4Points = record
    Point: TAggCurve4PointArray;
  public
    procedure Init(x1, y1, x2, y2, x3, y3, x4, y4: Double); overload;
    procedure Init(Point1, Point2, Point3, Point4: TPointDouble); overload;
  end;

  TAggCustomCurve = class(TAggVertexSource)
  protected
    procedure SetApproximationScale(Value: Double); virtual; abstract;
    function GetApproximationScale: Double; virtual; abstract;

    procedure SetAngleTolerance(Value: Double); virtual; abstract;
    function GetAngleTolerance: Double; virtual; abstract;

    procedure SetCuspLimit(Value: Double); virtual; abstract;
    function GetCuspLimit: Double; virtual; abstract;

    procedure SetApproximationMethod(Value: TAggCurveApproximationMethod); virtual; abstract;
    function GetApproximationMethod: TAggCurveApproximationMethod; virtual; abstract;
  public
    procedure Reset; virtual; abstract;
    procedure Init3(Point1, Point2, Point3: TPointDouble); virtual;
    procedure Init4(Point1, Point2, Point3, Point4: TPointDouble); virtual;

    property ApproximationScale: Double read GetApproximationScale write SetApproximationScale;
    property AngleTolerance: Double read GetAngleTolerance write SetAngleTolerance;
    property CuspLimit: Double read GetCuspLimit write SetCuspLimit;
    property ApproximationMethod: TAggCurveApproximationMethod read GetApproximationMethod write SetApproximationMethod;
  end;

  TAggCurve3Inc = class(TAggCustomCurve)
  private
    FNumSteps, FStep: Integer;
    FScale: Double;
    FStart, FEnd, FDeltaF, FSavedF: TPointDouble;
    FDeltaDelta, FPoint, FSavedDelta: TPointDouble;
  protected
    procedure SetApproximationMethod(Value: TAggCurveApproximationMethod); override;
    function GetApproximationMethod: TAggCurveApproximationMethod; override;

    procedure SetApproximationScale(Value: Double); override;
    function GetApproximationScale: Double; override;

    procedure SetAngleTolerance(Value: Double); override;
    function GetAngleTolerance: Double; override;

    procedure SetCuspLimit(Value: Double); override;
    function GetCuspLimit: Double; override;
  public
    constructor Create; overload;
    constructor Create(x1, y1, x2, y2, x3, y3: Double); overload;
    constructor Create(Point1, Point2, Point3: TPointDouble); overload;
    constructor Create(CurvePoints: PAggCurve3Points); overload;

    procedure Reset; override;
    procedure Init3(Point1, Point2, Point3: TPointDouble); override;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(x, y: PDouble): Cardinal; override;
  end;

  TAggCurve3Div = class(TAggCustomCurve)
  private
    FApproximationScale, FDistanceToleranceSquare: Double;
    FDistanceToleranceManhattan, FAngleTolerance: Double;

    FCount: Cardinal;
    FPoints: TAggPodDeque;
  protected
    procedure SetApproximationMethod(Value: TAggCurveApproximationMethod); override;
    function GetApproximationMethod: TAggCurveApproximationMethod; override;

    procedure SetApproximationScale(Value: Double); override;
    function GetApproximationScale: Double; override;

    procedure SetAngleTolerance(Value: Double); override;
    function GetAngleTolerance: Double; override;

    procedure SetCuspLimit(Value: Double); override;
    function GetCuspLimit: Double; override;
  public
    constructor Create; overload;
    constructor Create(x1, y1, x2, y2, x3, y3: Double); overload;
    constructor Create(Point1, Point2, Point3: TPointDouble); overload;
    constructor Create(CurvePoints: PAggCurve3Points); overload;
    destructor Destroy; override;

    procedure Reset; override;
    procedure Init3(Point1, Point2, Point3: TPointDouble); override;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(x, y: PDouble): Cardinal; override;

    procedure Bezier(x1, y1, x2, y2, x3, y3: Double); overload;
    procedure Bezier(Point1, Point2, Point3: TPointDouble); overload;
    procedure RecursiveBezier(x1, y1, x2, y2, x3, y3: Double; level: Cardinal);
  end;

  TAggCurve4Inc = class(TAggCustomCurve)
  private
    FNumSteps, FStep: Integer;

    FStart, FEnd, FDeltaF, FSavedF, FSavedDelta: TPointDouble;
    FDeltaDelta, FPoint: TPointDouble;
    FSavedDeltaDelta, FDeltaDeltaDelta: TPointDouble;
    FScale: Double;
  protected
    procedure SetApproximationMethod(Value: TAggCurveApproximationMethod); override;
    function GetApproximationMethod: TAggCurveApproximationMethod; override;

    procedure SetApproximationScale(Value: Double); override;
    function GetApproximationScale: Double; override;

    procedure SetAngleTolerance(Value: Double); override;
    function GetAngleTolerance: Double; override;

    procedure SetCuspLimit(Value: Double); override;
    function GetCuspLimit: Double; override;
  public
    constructor Create; overload;
    constructor Create(x1, y1, x2, y2, x3, y3, x4, y4: Double); overload;
    constructor Create(Point1, Point2, Point3, Point4: TPointDouble); overload;
    constructor Create(CurvePoints: PAggCurve4Points); overload;

    procedure Reset; override;
    procedure Init4(Point1, Point2, Point3, Point4: TPointDouble); override;
    procedure Init(CurvePoints: PAggCurve4Points);

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(x, y: PDouble): Cardinal; override;
  end;

  TAggCurve4Div = class(TAggCustomCurve)
  private
    FApproximationScale, FDistanceToleranceSquare: Double;
    FDistanceToleranceManhattan, FAngleTolerance, FCuspLimit: Double;

    FCount: Cardinal;
    FPoints: TAggPodDeque;
  protected
    procedure SetApproximationMethod(Value: TAggCurveApproximationMethod); override;
    function GetApproximationMethod: TAggCurveApproximationMethod; override;

    procedure SetApproximationScale(Value: Double); override;
    function GetApproximationScale: Double; override;

    procedure SetAngleTolerance(Value: Double); override;
    function GetAngleTolerance: Double; override;

    procedure SetCuspLimit(Value: Double); override;
    function GetCuspLimit: Double; override;
  public
    constructor Create; overload;
    constructor Create(x1, y1, x2, y2, x3, y3, x4, y4: Double); overload;
    constructor Create(Point1, Point2, Point3, Point4: TPointDouble); overload;
    constructor Create(CurvePoints: PAggCurve4Points); overload;
    destructor Destroy; override;

    procedure Reset; override;
    procedure Init4(Point1, Point2, Point3, Point4: TPointDouble); override;

    procedure Init(CurvePoints: PAggCurve4Points);

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(x, y: PDouble): Cardinal; override;

    procedure Bezier(x1, y1, x2, y2, x3, y3, x4, y4: Double); overload;
    procedure Bezier(Point1, Point2, Point3, Point4: TPointDouble); overload;
    procedure RecursiveBezier(x1, y1, x2, y2, x3, y3, x4, y4: Double; level: Cardinal);
  end;

  TAggCurve3 = class(TAggCustomCurve)
  private
    FCurveInc: TAggCurve3Inc;
    FCurveDiv: TAggCurve3Div;

    FApproximationMethod: TAggCurveApproximationMethod;
  protected
    procedure SetApproximationMethod(Value: TAggCurveApproximationMethod); override;
    function GetApproximationMethod: TAggCurveApproximationMethod; override;

    procedure SetApproximationScale(Value: Double); override;
    function GetApproximationScale: Double; override;

    procedure SetAngleTolerance(Value: Double); override;
    function GetAngleTolerance: Double; override;

    procedure SetCuspLimit(Value: Double); override;
    function GetCuspLimit: Double; override;
  public
    constructor Create; overload;
    constructor Create(x1, y1, x2, y2, x3, y3: Double); overload;
    constructor Create(Point1, Point2, Point3: TPointDouble); overload;
    constructor Create(CurvePoints: PAggCurve3Points); overload;
    destructor Destroy; override;

    procedure Reset; override;
    procedure Init3(Point1, Point2, Point3: TPointDouble); override;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(x, y: PDouble): Cardinal; override;
  end;

  TAggCurve4 = class(TAggCustomCurve)
  private
    FCurveInc: TAggCurve4Inc;
    FCurveDiv: TAggCurve4Div;

    FApproximationMethod: TAggCurveApproximationMethod;
  protected
    procedure SetApproximationMethod(Value: TAggCurveApproximationMethod); override;
    function GetApproximationMethod: TAggCurveApproximationMethod; override;

    procedure SetApproximationScale(Value: Double); override;
    function GetApproximationScale: Double; override;

    procedure SetAngleTolerance(Value: Double); override;
    function GetAngleTolerance: Double; override;

    procedure SetCuspLimit(Value: Double); override;
    function GetCuspLimit: Double; override;
  public
    constructor Create; overload;
    constructor Create(x1, y1, x2, y2, x3, y3, x4, y4: Double); overload;
    constructor Create(Point1, Point2, Point3, Point4: TPointDouble); overload;
    constructor Create(CurvePoints: PAggCurve4Points); overload;
    destructor Destroy; override;

    procedure Reset; override;
    procedure Init4(Point1, Point2, Point3, Point4: TPointDouble); override;
    procedure Init(CurvePoints: PAggCurve4Points);

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(x, y: PDouble): Cardinal; override;
  end;

function CatromToBezier(x1, y1, x2, y2, x3, y3, x4, y4: Double): TAggCurve4Points; overload;
function CatromToBezier(CurvePoints: PAggCurve4Points): TAggCurve4Points; overload;

function UbSplineToBezier(x1, y1, x2, y2, x3, y3, x4, y4: Double): TAggCurve4Points; overload;
function UbSplineToBezier(CurvePoints: PAggCurve4Points): TAggCurve4Points; overload;

function HermiteToBezier(x1, y1, x2, y2, x3, y3, x4, y4: Double): TAggCurve4Points; overload;
function HermiteToBezier(CurvePoints: PAggCurve4Points): TAggCurve4Points; overload;

implementation


const
  CCurveDistanceEpsilon       = 1E-30;
  CCurveCollinearityEpsilon   = 1E-30;
  CCurveAngleToleranceEpsilon = 0.01;
  CCurveRecursionLimit        = 32;

function CatromToBezier(x1, y1, x2, y2, x3, y3, x4, y4: Double): TAggCurve4Points;
begin
  // Trans. matrix Catmull-Rom to Bezier
  //
  // 0       1       0       0
  // -1/6    1       1/6     0
  // 0       1/6     1       -1/6
  // 0       0       1       0
  //
  Result.Init(x2, y2, (-x1 + 6 * x2 + x3) / 6, (-y1 + 6 * y2 + y3) / 6,
    (x2 + 6 * x3 - x4) / 6, (y2 + 6 * y3 - y4) / 6, x3, y3);
end;

function CatromToBezier(CurvePoints: PAggCurve4Points): TAggCurve4Points;
begin
  with CurvePoints^ do
      Result := CatromToBezier(Point[0].x, Point[0].y, Point[1].x, Point[1].y,
      Point[2].x, Point[2].y, Point[3].x, Point[3].y)
end;

function UbSplineToBezier(x1, y1, x2, y2, x3, y3, x4, y4: Double)
  : TAggCurve4Points;
begin
  // Trans. matrix Uniform BSpline to Bezier
  //
  // 1/6     4/6     1/6     0
  // 0       4/6     2/6     0
  // 0       2/6     4/6     0
  // 0       1/6     4/6     1/6
  //
  Result.Init((x1 + 4 * x2 + x3) / 6, (y1 + 4 * y2 + y3) / 6,
    (4 * x2 + 2 * x3) / 6, (4 * y2 + 2 * y3) / 6, (2 * x2 + 4 * x3) / 6,
    (2 * y2 + 4 * y3) / 6, (x2 + 4 * x3 + x4) / 6, (y2 + 4 * y3 + y4) / 6);
end;

function UbSplineToBezier(CurvePoints: PAggCurve4Points): TAggCurve4Points;
begin
  with CurvePoints^ do
      Result := UbSplineToBezier(Point[0].x, Point[0].y, Point[1].x, Point[1].y,
      Point[2].x, Point[2].y, Point[3].x, Point[3].y)
end;

function HermiteToBezier(x1, y1, x2, y2, x3, y3, x4, y4: Double): TAggCurve4Points;
begin
  // Trans. matrix Hermite to Bezier
  //
  // 1       0       0       0
  // 1       0       1/3     0
  // 0       1       0       -1/3
  // 0       1       0       0
  //
  Result.Init(x1, y1, (3 * x1 + x3) / 3, (3 * y1 + y3) / 3,
    (3 * x2 - x4) / 3, (3 * y2 - y4) / 3, x2, y2);
end;

function HermiteToBezier(CurvePoints: PAggCurve4Points): TAggCurve4Points;
begin
  with CurvePoints^ do
      Result := HermiteToBezier(Point[0].x, Point[0].y, Point[1].x, Point[1].y,
      Point[2].x, Point[2].y, Point[3].x, Point[3].y)
end;

{ TAggCustomCurve }

procedure TAggCustomCurve.Init3(Point1, Point2, Point3: TPointDouble);
begin
end;

procedure TAggCustomCurve.Init4(Point1, Point2, Point3, Point4: TPointDouble);
begin
end;

{ TAggCurve3Points }

procedure TAggCurve3Points.Init(x1, y1, x2, y2, x3, y3: Double);
begin
  Point[0].x := x1;
  Point[0].y := y1;
  Point[1].x := x2;
  Point[1].y := y2;
  Point[2].x := x3;
  Point[2].y := y3;
end;

procedure TAggCurve3Points.Init(Point1, Point2, Point3: TPointDouble);
begin
  Point[0] := Point1;
  Point[1] := Point2;
  Point[2] := Point3;
end;

{ TAggCurve3Inc }

constructor TAggCurve3Inc.Create;
begin
  FNumSteps := 0;
  FStep := 0;
  FScale := 1.0;
end;

constructor TAggCurve3Inc.Create(x1, y1, x2, y2, x3, y3: Double);
begin
  Create;

  Init3(PointDouble(x1, y1), PointDouble(x2, y2), PointDouble(x3, y3));
end;

constructor TAggCurve3Inc.Create(Point1, Point2, Point3: TPointDouble);
begin
  Create;

  Init3(Point1, Point2, Point3);
end;

constructor TAggCurve3Inc.Create(CurvePoints: PAggCurve3Points);
begin
  Create;

  Init3(CurvePoints^.Point[0], CurvePoints^.Point[1], CurvePoints^.Point[2]);
end;

procedure TAggCurve3Inc.Reset;
begin
  FNumSteps := 0;
  FStep := -1;
end;

procedure TAggCurve3Inc.Init3(Point1, Point2, Point3: TPointDouble);
var
  Delta: array [0 .. 1] of TPointDouble;
  Len: Double;
  tmp: TPointDouble;
  SubDivideStep: array [0 .. 1] of Double;
begin
  FStart := Point1;
  FEnd := Point3;

  Delta[0] := PointDouble(Point2.x - Point1.x, Point2.y - Point1.y);
  Delta[1] := PointDouble(Point3.x - Point2.x, Point3.y - Point2.y);

  Len := Hypot(Delta[0].x, Delta[0].y) + Hypot(Delta[1].x, Delta[1].y);

  FNumSteps := Trunc(Len * 0.25 * FScale);

  if FNumSteps < 4 then
      FNumSteps := 4;

  SubDivideStep[0] := 1 / FNumSteps;
  SubDivideStep[1] := Sqr(SubDivideStep[0]);

  tmp.x := (Point1.x - Point2.x - Point2.x + Point3.x) * SubDivideStep[1];
  tmp.y := (Point1.y - Point2.y - Point2.x + Point3.y) * SubDivideStep[1];

  FSavedF := Point1;
  FPoint := Point1;

  FSavedDelta.x := tmp.x + (Point2.x - Point1.x) * (2 * SubDivideStep[0]);
  FSavedDelta.y := tmp.y + (Point2.y - Point1.y) * (2 * SubDivideStep[0]);
  FDeltaF := FSavedDelta;

  FDeltaDelta := PointDouble(2 * tmp.x, 2 * tmp.y);

  FStep := FNumSteps;
end;

procedure TAggCurve3Inc.SetApproximationMethod(
  Value: TAggCurveApproximationMethod);
begin
end;

function TAggCurve3Inc.GetApproximationMethod: TAggCurveApproximationMethod;
begin
  Result := camInc;
end;

procedure TAggCurve3Inc.SetApproximationScale(Value: Double);
begin
  FScale := Value;
end;

function TAggCurve3Inc.GetApproximationScale: Double;
begin
  Result := FScale;
end;

procedure TAggCurve3Inc.SetAngleTolerance(Value: Double);
begin
end;

function TAggCurve3Inc.GetAngleTolerance: Double;
begin
  Result := 0.0;
end;

procedure TAggCurve3Inc.SetCuspLimit(Value: Double);
begin
end;

function TAggCurve3Inc.GetCuspLimit: Double;
begin
  Result := 0.0;
end;

procedure TAggCurve3Inc.Rewind(PathID: Cardinal);
begin
  if FNumSteps = 0 then
    begin
      FStep := -1;

      Exit;
    end;

  FStep := FNumSteps;
  FPoint.x := FSavedF.x;
  FPoint.y := FSavedF.y;
  FDeltaF.x := FSavedDelta.x;
  FDeltaF.y := FSavedDelta.y;
end;

function TAggCurve3Inc.Vertex(x, y: PDouble): Cardinal;
begin
  if FStep < 0 then
    begin
      Result := CAggPathCmdStop;

      Exit;
    end;

  if FStep = FNumSteps then
    begin
      x^ := FStart.x;
      y^ := FStart.y;

      dec(FStep);

      Result := CAggPathCmdMoveTo;

      Exit;
    end;

  if FStep = 0 then
    begin
      x^ := FEnd.x;
      y^ := FEnd.y;

      dec(FStep);

      Result := CAggPathCmdLineTo;

      Exit;
    end;

  FPoint.x := FPoint.x + FDeltaF.x;
  FPoint.y := FPoint.y + FDeltaF.y;
  FDeltaF.x := FDeltaF.x + FDeltaDelta.x;
  FDeltaF.y := FDeltaF.y + FDeltaDelta.y;

  x^ := FPoint.x;
  y^ := FPoint.y;

  dec(FStep);

  Result := CAggPathCmdLineTo;
end;

{ TAggCurve3Div }

constructor TAggCurve3Div.Create;
begin
  FPoints := TAggPodDeque.Create(SizeOf(TPointDouble));

  FApproximationScale := 1;
  FAngleTolerance := 0;
  FCount := 0;
end;

constructor TAggCurve3Div.Create(x1, y1, x2, y2, x3, y3: Double);
begin
  Create;

  Init3(PointDouble(x1, y1), PointDouble(x2, y2), PointDouble(x3, y3));
end;

constructor TAggCurve3Div.Create(Point1, Point2, Point3: TPointDouble);
begin
  Create;

  Init3(Point1, Point2, Point3);
end;

constructor TAggCurve3Div.Create(CurvePoints: PAggCurve3Points);
begin
  Create;

  Init3(CurvePoints^.Point[0], CurvePoints^.Point[1], CurvePoints^.Point[2]);
end;

destructor TAggCurve3Div.Destroy;
begin
  FPoints.Free;
  inherited
end;

procedure TAggCurve3Div.Reset;
begin
  FPoints.RemoveAll;

  FCount := 0;
end;

procedure TAggCurve3Div.Init3(Point1, Point2, Point3: TPointDouble);
begin
  FPoints.RemoveAll;

  FDistanceToleranceSquare := Sqr(0.5 / FApproximationScale);
  FDistanceToleranceManhattan := 4.0 / FApproximationScale;

  Bezier(Point1, Point2, Point3);

  FCount := 0;
end;

procedure TAggCurve3Div.SetApproximationMethod(
  Value: TAggCurveApproximationMethod);
begin
end;

function TAggCurve3Div.GetApproximationMethod: TAggCurveApproximationMethod;
begin
  Result := camDiv;
end;

procedure TAggCurve3Div.SetApproximationScale(Value: Double);
begin
  if Value = 0 then
      FApproximationScale := 0.00001
  else
      FApproximationScale := Value;
end;

function TAggCurve3Div.GetApproximationScale: Double;
begin
  Result := FApproximationScale;
end;

procedure TAggCurve3Div.SetAngleTolerance(Value: Double);
begin
  FAngleTolerance := Value;
end;

function TAggCurve3Div.GetAngleTolerance: Double;
begin
  Result := FAngleTolerance;
end;

procedure TAggCurve3Div.SetCuspLimit(Value: Double);
begin
end;

function TAggCurve3Div.GetCuspLimit: Double;
begin
  Result := 0;
end;

procedure TAggCurve3Div.Rewind(PathID: Cardinal);
begin
  FCount := 0;
end;

function TAggCurve3Div.Vertex(x, y: PDouble): Cardinal;
var
  p: PPointDouble;
begin
  if FCount >= FPoints.Size then
    begin
      Result := CAggPathCmdStop;

      Exit;
    end;

  p := FPoints[FCount];

  inc(FCount);

  x^ := p.x;
  y^ := p.y;

  if FCount = 1 then
      Result := CAggPathCmdMoveTo
  else
      Result := CAggPathCmdLineTo;
end;

procedure TAggCurve3Div.Bezier(x1, y1, x2, y2, x3, y3: Double);
var
  pt: TPointDouble;
begin
  pt.x := x1;
  pt.y := y1;

  FPoints.Add(@pt);

  RecursiveBezier(x1, y1, x2, y2, x3, y3, 0);

  pt.x := x3;
  pt.y := y3;

  FPoints.Add(@pt);
end;

procedure TAggCurve3Div.Bezier(Point1, Point2, Point3: TPointDouble);
begin
  FPoints.Add(@Point1);

  RecursiveBezier(Point1.x, Point1.y, Point2.x, Point2.y, Point3.x, Point3.y,
    0);

  FPoints.Add(@Point3);
end;

procedure TAggCurve3Div.RecursiveBezier(x1, y1, x2, y2, x3, y3: Double;
  level: Cardinal);
var
  Delta: TPointDouble;
  X12, Y12, X23, Y23, X123, Y123, d, DA: Double;
  pt: TPointDouble;
begin
  if level > CCurveRecursionLimit then
      Exit;

  // Calculate all the mid-points of the line segments
  X12 := (x1 + x2) * 0.5;
  Y12 := (y1 + y2) * 0.5;
  X23 := (x2 + x3) * 0.5;
  Y23 := (y2 + y3) * 0.5;
  X123 := (X12 + X23) * 0.5;
  Y123 := (Y12 + Y23) * 0.5;

  Delta.x := x3 - x1;
  Delta.y := y3 - y1;
  d := Abs((x2 - x3) * Delta.y - (y2 - y3) * Delta.x);

  if d > CCurveCollinearityEpsilon then
    // Regular care
    if d * d <= FDistanceToleranceSquare * (Sqr(Delta.x) + Sqr(Delta.y)) then
      begin
        // If the curvature doesn't exceed the DistanceTolerance value
        // we tend to finish subdivisions.
        if FAngleTolerance < CCurveAngleToleranceEpsilon then
          begin
            pt.x := X123;
            pt.y := Y123;

            FPoints.Add(@pt);

            Exit;
          end;

        // Angle & Cusp Condition
        DA := Abs(ArcTan2(y3 - y2, x3 - x2) - ArcTan2(y2 - y1, x2 - x1));

        if DA >= pi then
            DA := 2 * pi - DA;

        if DA < FAngleTolerance then
          begin
            // Finally we can stop the recursion
            pt.x := X123;
            pt.y := Y123;

            FPoints.Add(@pt);

            Exit;
          end;

      end
    else
  else if Abs(x1 + x3 - x2 - x2) + Abs(y1 + y3 - y2 - y2) <= FDistanceToleranceManhattan
  then
    begin
      pt.x := X123;
      pt.y := Y123;

      FPoints.Add(@pt);

      Exit;
    end;

  // Continue subdivision
  RecursiveBezier(x1, y1, X12, Y12, X123, Y123, level + 1);
  RecursiveBezier(X123, Y123, X23, Y23, x3, y3, level + 1);
end;

{ TAggCurve4Points }

procedure TAggCurve4Points.Init(x1, y1, x2, y2, x3, y3, x4, y4: Double);
begin
  Point[0].x := x1;
  Point[0].y := y1;
  Point[1].x := x2;
  Point[1].y := y2;
  Point[2].x := x3;
  Point[2].y := y3;
  Point[3].x := x4;
  Point[3].y := y4;
end;

procedure TAggCurve4Points.Init(Point1, Point2, Point3, Point4: TPointDouble);
begin
  Point[0] := Point1;
  Point[1] := Point2;
  Point[2] := Point3;
  Point[3] := Point4;
end;

{ TAggCurve4Inc }

constructor TAggCurve4Inc.Create;
begin
  FNumSteps := 0;
  FStep := 0;
  FScale := 1.0;
end;

constructor TAggCurve4Inc.Create(x1, y1, x2, y2, x3, y3, x4, y4: Double);
begin
  Create(PointDouble(x1, y1), PointDouble(x2, y2), PointDouble(x3, y3),
    PointDouble(x4, y4));
end;

constructor TAggCurve4Inc.Create(Point1, Point2, Point3, Point4: TPointDouble);
begin
  Create;

  Init4(Point1, Point2, Point3, Point3);
end;

constructor TAggCurve4Inc.Create(CurvePoints: PAggCurve4Points);
begin
  Create;

  Init4(CurvePoints^.Point[0], CurvePoints^.Point[1],
    CurvePoints^.Point[2], CurvePoints^.Point[3]);
end;

procedure TAggCurve4Inc.Reset;
begin
  FNumSteps := 0;
  FStep := -1;
end;

procedure TAggCurve4Inc.Init4(Point1, Point2, Point3, Point4: TPointDouble);
var
  Delta: array [0 .. 2] of TPointDouble;
  SubDivideStep: array [0 .. 2] of Double;
  Len: Double;
  Pre: array [0 .. 3] of Double;
  Temp: array [0 .. 1] of TPointDouble;
begin
  FStart := Point1;
  FEnd := Point4;

  Delta[0].x := Point2.x - Point1.x;
  Delta[0].y := Point2.y - Point1.y;
  Delta[1].x := Point3.x - Point2.x;
  Delta[1].y := Point3.y - Point2.y;
  Delta[2].x := Point4.x - Point3.x;
  Delta[2].y := Point4.y - Point3.y;

  Len := Hypot(Delta[0].x, Delta[0].y) + Hypot(Delta[1].x, Delta[1].y) +
    Hypot(Delta[2].x, Delta[2].y);

  FNumSteps := Trunc(0.25 * Len * FScale);

  if FNumSteps < 4 then
      FNumSteps := 4;

  SubDivideStep[0] := 1 / FNumSteps;
  SubDivideStep[1] := Sqr(SubDivideStep[0]);
  SubDivideStep[2] := SubDivideStep[1] * SubDivideStep[0];

  Pre[0] := 3 * SubDivideStep[0];
  Pre[1] := 3 * SubDivideStep[1];
  Pre[2] := 6 * SubDivideStep[1];
  Pre[3] := 6 * SubDivideStep[2];

  Temp[0].x := Point1.x - Point2.x - Point2.x + Point3.x;
  Temp[0].y := Point1.y - Point2.y - Point2.y + Point3.y;

  Temp[1].x := (Point2.x - Point3.x) * 3.0 - Point1.x + Point4.x;
  Temp[1].y := (Point2.y - Point3.y) * 3.0 - Point1.y + Point4.y;

  FSavedF := Point1;
  FPoint := Point1;

  FSavedDelta.x := (Point2.x - Point1.x) * Pre[0] + Temp[0].x * Pre[1] +
    Temp[1].x * SubDivideStep[2];
  FSavedDelta.y := (Point2.y - Point1.y) * Pre[0] + Temp[0].y * Pre[1] +
    Temp[1].y * SubDivideStep[2];
  FDeltaF := FSavedDelta;

  FSavedDeltaDelta.x := Temp[0].x * Pre[2] + Temp[1].x * Pre[3];
  FSavedDeltaDelta.y := Temp[0].y * Pre[2] + Temp[1].y * Pre[3];
  FDeltaDelta := FSavedDeltaDelta;

  FDeltaDeltaDelta.x := Temp[1].x * Pre[3];
  FDeltaDeltaDelta.y := Temp[1].y * Pre[3];

  FStep := FNumSteps;
end;

procedure TAggCurve4Inc.Init(CurvePoints: PAggCurve4Points);
begin
  Init4(CurvePoints^.Point[0], CurvePoints^.Point[1],
    CurvePoints^.Point[2], CurvePoints^.Point[3]);
end;

function TAggCurve4Inc.GetApproximationMethod: TAggCurveApproximationMethod;
begin
  Result := camInc;
end;

procedure TAggCurve4Inc.SetAngleTolerance(Value: Double);
begin
end;

procedure TAggCurve4Inc.SetApproximationMethod(
  Value: TAggCurveApproximationMethod);
begin
end;

procedure TAggCurve4Inc.SetApproximationScale(Value: Double);
begin
  FScale := Value;
end;

procedure TAggCurve4Inc.SetCuspLimit(Value: Double);
begin
end;

function TAggCurve4Inc.GetApproximationScale: Double;
begin
  Result := FScale;
end;

function TAggCurve4Inc.GetAngleTolerance: Double;
begin
  Result := 0;
end;

function TAggCurve4Inc.GetCuspLimit: Double;
begin
  Result := 0;
end;

procedure TAggCurve4Inc.Rewind(PathID: Cardinal);
begin
  if FNumSteps = 0 then
    begin
      FStep := -1;

      Exit;
    end;

  FStep := FNumSteps;
  FPoint := FSavedF;
  FDeltaF := FSavedDelta;
  FDeltaDelta := FSavedDeltaDelta;
end;

function TAggCurve4Inc.Vertex(x, y: PDouble): Cardinal;
begin
  if FStep < 0 then
    begin
      Result := CAggPathCmdStop;

      Exit;
    end;

  if FStep = FNumSteps then
    begin
      x^ := FStart.x;
      y^ := FStart.y;

      dec(FStep);

      Result := CAggPathCmdMoveTo;

      Exit;
    end;

  if FStep = 0 then
    begin
      x^ := FEnd.x;
      y^ := FEnd.y;

      dec(FStep);

      Result := CAggPathCmdLineTo;

      Exit;
    end;

  FPoint.x := FPoint.x + FDeltaF.x;
  FPoint.y := FPoint.y + FDeltaF.y;
  FDeltaF.x := FDeltaF.x + FDeltaDelta.x;
  FDeltaF.y := FDeltaF.y + FDeltaDelta.y;
  FDeltaDelta.x := FDeltaDelta.x + FDeltaDeltaDelta.x;
  FDeltaDelta.y := FDeltaDelta.y + FDeltaDeltaDelta.y;

  x^ := FPoint.x;
  y^ := FPoint.y;

  dec(FStep);

  Result := CAggPathCmdLineTo;
end;

{ TAggCurve4Div }

constructor TAggCurve4Div.Create;
begin
  FPoints := TAggPodDeque.Create(SizeOf(TPointDouble));

  FApproximationScale := 1;
  FAngleTolerance := 0;

  FCuspLimit := 0;
  FCount := 0;
end;

constructor TAggCurve4Div.Create(x1, y1, x2, y2, x3, y3, x4, y4: Double);
begin
  Create(PointDouble(x1, y1), PointDouble(x2, y2), PointDouble(x3, y3),
    PointDouble(x4, y4));
end;

constructor TAggCurve4Div.Create(Point1, Point2, Point3, Point4: TPointDouble);
begin
  Create;

  Init4(Point1, Point2, Point3, Point4);
end;

constructor TAggCurve4Div.Create(CurvePoints: PAggCurve4Points);
begin
  Create;

  Init4(CurvePoints^.Point[0], CurvePoints^.Point[1], CurvePoints^.Point[2],
    CurvePoints^.Point[3]);
end;

destructor TAggCurve4Div.Destroy;
begin
  FPoints.Free;
  inherited
end;

procedure TAggCurve4Div.Reset;
begin
  FPoints.RemoveAll;

  FCount := 0;
end;

procedure TAggCurve4Div.Init4(Point1, Point2, Point3, Point4: TPointDouble);
begin
  FPoints.RemoveAll;

  FDistanceToleranceSquare := Sqr(0.5 / FApproximationScale);
  FDistanceToleranceManhattan := 4 / FApproximationScale;

  Bezier(Point1, Point2, Point3, Point4);

  FCount := 0;
end;

procedure TAggCurve4Div.Init(CurvePoints: PAggCurve4Points);
begin
  Init4(CurvePoints^.Point[0], CurvePoints^.Point[1], CurvePoints^.Point[2],
    CurvePoints^.Point[3]);
end;

procedure TAggCurve4Div.SetApproximationMethod(Value: TAggCurveApproximationMethod);
begin
end;

function TAggCurve4Div.GetApproximationMethod: TAggCurveApproximationMethod;
begin
  Result := camDiv;
end;

procedure TAggCurve4Div.SetApproximationScale(Value: Double);
begin
  if Value = 0 then
      FApproximationScale := 0.00001
  else
      FApproximationScale := Value;
end;

function TAggCurve4Div.GetApproximationScale: Double;
begin
  Result := FApproximationScale;
end;

procedure TAggCurve4Div.SetAngleTolerance(Value: Double);
begin
  FAngleTolerance := Value;
end;

function TAggCurve4Div.GetAngleTolerance: Double;
begin
  Result := FAngleTolerance;
end;

procedure TAggCurve4Div.SetCuspLimit(Value: Double);
begin
  if Value = 0.0 then
      FCuspLimit := 0
  else
      FCuspLimit := pi - Value;
end;

function TAggCurve4Div.GetCuspLimit: Double;
begin
  if FCuspLimit = 0.0 then
      Result := 0
  else
      Result := pi - FCuspLimit;
end;

procedure TAggCurve4Div.Rewind(PathID: Cardinal);
begin
  FCount := 0;
end;

function TAggCurve4Div.Vertex(x, y: PDouble): Cardinal;
var
  p: PPointDouble;
begin
  if FCount >= FPoints.Size then
    begin
      Result := CAggPathCmdStop;

      Exit;
    end;

  p := FPoints[FCount];

  inc(FCount);

  x^ := p.x;
  y^ := p.y;

  if FCount = 1 then
      Result := CAggPathCmdMoveTo
  else
      Result := CAggPathCmdLineTo;
end;

procedure TAggCurve4Div.Bezier(x1, y1, x2, y2, x3, y3, x4, y4: Double);
var
  pt: TPointDouble;
begin
  pt := PointDouble(x1, y1);
  FPoints.Add(@pt);

  RecursiveBezier(x1, y1, x2, y2, x3, y3, x4, y4, 0);

  pt := PointDouble(x4, y4);
  FPoints.Add(@pt);
end;

procedure TAggCurve4Div.Bezier(Point1, Point2, Point3, Point4: TPointDouble);
begin
  FPoints.Add(@Point1);

  RecursiveBezier(Point1.x, Point1.y, Point2.x, Point2.y, Point3.x, Point3.y,
    Point4.x, Point4.y, 0);

  FPoints.Add(@Point4);
end;

procedure TAggCurve4Div.RecursiveBezier(x1, y1, x2, y2, x3, y3, x4, y4: Double;
  level: Cardinal);
var
  X12, Y12,
    X23, Y23,
    X34, Y34,
    X123, Y123,
    X234, Y234,
    X1234, Y1234: Double;
  Delta: TPointDouble;
  d2, d3,
    Da1, Da2, A23: Double;

  pt: TPointDouble;
begin
  if level > CCurveRecursionLimit then
      Exit;

  // Calculate all the mid-points of the line segments
  X12 := (x1 + x2) * 0.5;
  Y12 := (y1 + y2) * 0.5;
  X23 := (x2 + x3) * 0.5;
  Y23 := (y2 + y3) * 0.5;
  X34 := (x3 + x4) * 0.5;
  Y34 := (y3 + y4) * 0.5;
  X123 := (X12 + X23) * 0.5;
  Y123 := (Y12 + Y23) * 0.5;
  X234 := (X23 + X34) * 0.5;
  Y234 := (Y23 + Y34) * 0.5;
  X1234 := (X123 + X234) * 0.5;
  Y1234 := (Y123 + Y234) * 0.5;

  // Try to approximate the full cubic TCurve by Value single straight line
  Delta.x := x4 - x1;
  Delta.y := y4 - y1;

  d2 := Abs((x2 - x4) * Delta.y - (y2 - y4) * Delta.x);
  d3 := Abs((x3 - x4) * Delta.y - (y3 - y4) * Delta.x);

  case ((Integer(d2 > CCurveCollinearityEpsilon) shl 1) +
    Integer(d3 > CCurveCollinearityEpsilon)) of
    // All collinear OR p1==p4
    0:
      if Abs(x1 + x3 - x2 - x2) + Abs(y1 + y3 - y2 - y2) +
        Abs(x2 + x4 - x3 - x3) + Abs(y2 + y4 - y3 - y3) <= FDistanceToleranceManhattan
      then
        begin
          pt.x := X1234;
          pt.y := Y1234;

          FPoints.Add(@pt);

          Exit;
        end;

    // p1,p2,p4 are collinear, p3 is considerable
    1:
      if Sqr(d3) <= FDistanceToleranceSquare * (Sqr(Delta.x) + Sqr(Delta.y)) then
        begin
          if FAngleTolerance < CCurveAngleToleranceEpsilon then
            begin
              pt.x := X23;
              pt.y := Y23;

              FPoints.Add(@pt);

              Exit;
            end;

          // Angle Condition
          Da1 := Abs(ArcTan2(y4 - y3, x4 - x3) - ArcTan2(y3 - y2, x3 - x2));

          if Da1 >= pi then
              Da1 := 2 * pi - Da1;

          if Da1 < FAngleTolerance then
            begin
              pt.x := x2;
              pt.y := y2;

              FPoints.Add(@pt);

              pt.x := x3;
              pt.y := y3;

              FPoints.Add(@pt);

              Exit;
            end;

          if FCuspLimit <> 0.0 then
            if Da1 > FCuspLimit then
              begin
                pt.x := x3;
                pt.y := y3;

                FPoints.Add(@pt);

                Exit;
              end;
        end;

    // p1,p3,p4 are collinear, p2 is considerable
    2:
      if Sqr(d2) <= FDistanceToleranceSquare * (Sqr(Delta.x) + Sqr(Delta.y)) then
        begin
          if FAngleTolerance < CCurveAngleToleranceEpsilon then
            begin
              pt.x := X23;
              pt.y := Y23;

              FPoints.Add(@pt);

              Exit;
            end;

          // Angle Condition
          Da1 := Abs(ArcTan2(y3 - y2, x3 - x2) - ArcTan2(y2 - y1, x2 - x1));

          if Da1 >= pi then
              Da1 := 2 * pi - Da1;

          if Da1 < FAngleTolerance then
            begin
              pt.x := x2;
              pt.y := y2;

              FPoints.Add(@pt);

              pt.x := x3;
              pt.y := y3;

              FPoints.Add(@pt);

              Exit;
            end;

          if FCuspLimit <> 0.0 then
            if Da1 > FCuspLimit then
              begin
                pt.x := x2;
                pt.y := y2;

                FPoints.Add(@pt);

                Exit;
              end;
        end;

    // Regular care
    3:
      if (d2 + d3) * (d2 + d3) <= FDistanceToleranceSquare *
        (Delta.x * Delta.x + Delta.y * Delta.y) then
        begin
          // If the curvature doesn't exceed the DistanceTolerance value
          // we tend to finish subdivisions.
          if FAngleTolerance < CCurveAngleToleranceEpsilon then
            begin
              pt.x := X23;
              pt.y := Y23;

              FPoints.Add(@pt);

              Exit;
            end;

          // Angle & Cusp Condition
          A23 := ArcTan2(y3 - y2, x3 - x2);
          Da1 := Abs(A23 - ArcTan2(y2 - y1, x2 - x1));
          Da2 := Abs(ArcTan2(y4 - y3, x4 - x3) - A23);

          if Da1 >= pi then
              Da1 := 2 * pi - Da1;

          if Da2 >= pi then
              Da2 := 2 * pi - Da2;

          if Da1 + Da2 < FAngleTolerance then
            begin
              // Finally we can stop the recursion
              pt.x := X23;
              pt.y := Y23;

              FPoints.Add(@pt);

              Exit;
            end;

          if FCuspLimit <> 0.0 then
            begin
              if Da1 > FCuspLimit then
                begin
                  pt.x := x2;
                  pt.y := y2;

                  FPoints.Add(@pt);

                  Exit;
                end;

              if Da2 > FCuspLimit then
                begin
                  pt.x := x3;
                  pt.y := y3;

                  FPoints.Add(@pt);

                  Exit;
                end;
            end;
        end;
  end;

  // Continue subdivision
  RecursiveBezier(x1, y1, X12, Y12, X123, Y123, X1234, Y1234, level + 1);
  RecursiveBezier(X1234, Y1234, X234, Y234, X34, Y34, x4, y4, level + 1);
end;

{ TAggCurve3 }

constructor TAggCurve3.Create;
begin
  FCurveInc := TAggCurve3Inc.Create;
  FCurveDiv := TAggCurve3Div.Create;

  FApproximationMethod := camDiv;
end;

constructor TAggCurve3.Create(Point1, Point2, Point3: TPointDouble);
begin
  Create;

  Init3(Point1, Point2, Point3);
end;

constructor TAggCurve3.Create(x1, y1, x2, y2, x3, y3: Double);
begin
  Create(PointDouble(x1, y1), PointDouble(x2, y2), PointDouble(x3, y3));
end;

constructor TAggCurve3.Create(CurvePoints: PAggCurve3Points);
begin
  Create;

  Init3(CurvePoints^.Point[0], CurvePoints^.Point[1], CurvePoints^.Point[2]);
end;

destructor TAggCurve3.Destroy;
begin
  FCurveInc.Free;
  FCurveDiv.Free;

  inherited;
end;

procedure TAggCurve3.Reset;
begin
  FCurveInc.Reset;
  FCurveDiv.Reset;
end;

procedure TAggCurve3.Init3(Point1, Point2, Point3: TPointDouble);
begin
  if FApproximationMethod = camInc then
      FCurveInc.Init3(Point1, Point2, Point3)
  else
      FCurveDiv.Init3(Point1, Point2, Point3);
end;

procedure TAggCurve3.SetApproximationMethod(
  Value: TAggCurveApproximationMethod);
begin
  FApproximationMethod := Value;
end;

function TAggCurve3.GetApproximationMethod: TAggCurveApproximationMethod;
begin
  Result := FApproximationMethod;
end;

procedure TAggCurve3.SetApproximationScale(Value: Double);
begin
  FCurveInc.SetApproximationScale(Value);
  FCurveDiv.SetApproximationScale(Value);
end;

function TAggCurve3.GetApproximationScale: Double;
begin
  Result := FCurveInc.GetApproximationScale;
end;

procedure TAggCurve3.SetAngleTolerance(Value: Double);
begin
  FCurveDiv.SetAngleTolerance(Value);
end;

function TAggCurve3.GetAngleTolerance: Double;
begin
  Result := FCurveDiv.GetAngleTolerance;
end;

procedure TAggCurve3.SetCuspLimit(Value: Double);
begin
  FCurveDiv.SetCuspLimit(Value);
end;

function TAggCurve3.GetCuspLimit: Double;
begin
  Result := FCurveDiv.GetCuspLimit;
end;

procedure TAggCurve3.Rewind(PathID: Cardinal);
begin
  if FApproximationMethod = camInc then
      FCurveInc.Rewind(PathID)
  else
      FCurveDiv.Rewind(PathID)
end;

function TAggCurve3.Vertex(x, y: PDouble): Cardinal;
begin
  if FApproximationMethod = camInc then
      Result := FCurveInc.Vertex(x, y)
  else
      Result := FCurveDiv.Vertex(x, y);
end;

{ TAggCurve4 }

constructor TAggCurve4.Create;
begin
  FCurveInc := TAggCurve4Inc.Create;
  FCurveDiv := TAggCurve4Div.Create;

  FApproximationMethod := camDiv;
end;

constructor TAggCurve4.Create(x1, y1, x2, y2, x3, y3, x4, y4: Double);
begin
  Create(PointDouble(x1, y1), PointDouble(x2, y2), PointDouble(x3, y3),
    PointDouble(x4, y4));
end;

constructor TAggCurve4.Create(Point1, Point2, Point3, Point4: TPointDouble);
begin
  Create;

  Init4(Point1, Point2, Point3, Point4);
end;

constructor TAggCurve4.Create(CurvePoints: PAggCurve4Points);
begin
  Create;

  Init4(CurvePoints^.Point[0], CurvePoints^.Point[1], CurvePoints^.Point[2],
    CurvePoints^.Point[3]);
end;

destructor TAggCurve4.Destroy;
begin
  FCurveInc.Free;
  FCurveDiv.Free;

  inherited;
end;

procedure TAggCurve4.Reset;
begin
  FCurveInc.Reset;
  FCurveDiv.Reset;
end;

procedure TAggCurve4.Init4(Point1, Point2, Point3, Point4: TPointDouble);
begin
  if FApproximationMethod = camInc then
      FCurveInc.Init4(Point1, Point2, Point3, Point4)
  else
      FCurveDiv.Init4(Point1, Point2, Point3, Point4);
end;

procedure TAggCurve4.Init(CurvePoints: PAggCurve4Points);
begin
  Init4(CurvePoints^.Point[0], CurvePoints^.Point[1], CurvePoints^.Point[2],
    CurvePoints^.Point[3]);
end;

procedure TAggCurve4.SetApproximationMethod(Value: TAggCurveApproximationMethod);
begin
  FApproximationMethod := Value;
end;

function TAggCurve4.GetApproximationMethod: TAggCurveApproximationMethod;
begin
  Result := FApproximationMethod;
end;

procedure TAggCurve4.SetApproximationScale(Value: Double);
begin
  FCurveInc.SetApproximationScale(Value);
  FCurveDiv.SetApproximationScale(Value);
end;

function TAggCurve4.GetApproximationScale: Double;
begin
  Result := FCurveInc.GetApproximationScale;
end;

procedure TAggCurve4.SetAngleTolerance(Value: Double);
begin
  FCurveDiv.SetAngleTolerance(Value);
end;

function TAggCurve4.GetAngleTolerance: Double;
begin
  Result := FCurveDiv.GetAngleTolerance;
end;

procedure TAggCurve4.SetCuspLimit(Value: Double);
begin
  FCurveDiv.SetCuspLimit(Value);
end;

function TAggCurve4.GetCuspLimit: Double;
begin
  Result := FCurveDiv.GetCuspLimit;
end;

procedure TAggCurve4.Rewind(PathID: Cardinal);
begin
  if FApproximationMethod = camInc then
      FCurveInc.Rewind(PathID)
  else
      FCurveDiv.Rewind(PathID);
end;

function TAggCurve4.Vertex(x, y: PDouble): Cardinal;
begin
  if FApproximationMethod = camInc then
      Result := FCurveInc.Vertex(x, y)
  else
      Result := FCurveDiv.Vertex(x, y);
end;

end. 
 
 
