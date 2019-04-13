{ ****************************************************************************** }
{ * memory Rasterization with AGG support                                      * }
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
unit AggBezierArc;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  Math,
  AggMath,
  AggBasics,
  AggVertexSource,
  AggTransAffine;

type
  TAggBezierArc = class(TAggCustomVertexSource)
  private
    FVertex, FNumVertices: Cardinal;

    FVertices: TDoubleMatrix2x6;

    FCmd: Cardinal;
  protected
    // Supplemantary functions. NumVertices() actually returns doubled
    // number of vertices. That is, for 1 vertex it returns 2.
    function GetVertices: PDoubleMatrix2x6;
    function GetNumVertices: Cardinal;

    procedure Init(x, y, RX, RY, startAngle, SweepAngle: Double);
  public
    constructor Create; overload;
    constructor Create(x, y, RX, RY, startAngle, SweepAngle: Double); overload;
    constructor Create(x, y: Double; radius: TPointDouble; startAngle, SweepAngle: Double); overload;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(x, y: PDouble): Cardinal; override;

    property NumVertices: Cardinal read GetNumVertices;
  end;

  // Compute an SVG-style bezier arc.
  //
  // Computes an elliptical arc from (x1, y1) to (x2, y2). The size and
  // orientation of the Ellipse are defined by two radii (rx, ry)
  // and an x-axis-rotation, which indicates how the Ellipse as a whole
  // is rotated relative to the current coordinate system. The center
  // (cx, cy) of the Ellipse is calculated automatically to satisfy the
  // constraints imposed by the other parameters.
  // large-arc-flag and sweep-flag contribute to the automatic calculations
  // and help determine how the arc is drawn.
  TAggBezierArcSvg = class(TAggBezierArc)
  private
    FRadiiOK: Boolean;
  public
    constructor Create; overload;
    constructor Create(x1, y1, RX, RY, angle: Double; LargeArcFlag, SweepFlag: Boolean; x2, y2: Double); overload;
    procedure Init(x0, y0, RX, RY, angle: Double; LargeArcFlag, SweepFlag: Boolean; x2, y2: Double);

    property RadiiOK: Boolean read FRadiiOK;
  end;

procedure ArcToBezier(Cx, Cy, RX, RY, startAngle, SweepAngle: Double; Curve: PDoubleArray8);

implementation

const
  CBezierArcAngleEpsilon = 0.01;

procedure ArcToBezier(Cx, Cy, RX, RY, startAngle, SweepAngle: Double;
  Curve: PDoubleArray8);
var
  i: Cardinal;

  sn, cs, x0, y0, TX, TY: Double;
  Px, Py: array [0 .. 3] of Double;
begin
  SinCos(SweepAngle * 0.5, y0, x0);
  TX := (1.0 - x0) * 4.0 / 3.0;
  TY := y0 - TX * x0 / y0;

  Px[0] := x0;
  Py[0] := -y0;
  Px[1] := x0 + TX;
  Py[1] := -TY;
  Px[2] := x0 + TX;
  Py[2] := TY;
  Px[3] := x0;
  Py[3] := y0;

  SinCos(startAngle + SweepAngle * 0.5, sn, cs);

  for i := 0 to 3 do
    begin
      Curve[i * 2] := Cx + RX * (Px[i] * cs - Py[i] * sn);
      Curve[i * 2 + 1] := Cy + RY * (Px[i] * sn + Py[i] * cs);
    end;
end;

{ TAggBezierArc }

constructor TAggBezierArc.Create;
begin
  FVertex := 26;

  FNumVertices := 0;

  FCmd := CAggPathCmdLineTo;
end;

constructor TAggBezierArc.Create(x, y, RX, RY, startAngle,
  SweepAngle: Double);
begin
  Init(x, y, RX, RY, startAngle, SweepAngle);
end;

constructor TAggBezierArc.Create(x, y: Double; radius: TPointDouble; startAngle,
  SweepAngle: Double);
begin
  Init(x, y, radius.x, radius.y, startAngle, SweepAngle);
end;

procedure TAggBezierArc.Init(x, y, RX, RY, startAngle, SweepAngle: Double);
var
  i: Integer;
  f: Double;

  sn, CN: Double;
  TotalSweep, LocalSweep, PrevSweep: Double;

  Done: Boolean;
begin
  i := Trunc(startAngle / (2.0 * pi));
  f := startAngle - (i * 2.0 * pi);

  startAngle := f;

  if SweepAngle >= 2.0 * pi then
      SweepAngle := 2.0 * pi;

  if SweepAngle <= -2.0 * pi then
      SweepAngle := -2.0 * pi;

  if Abs(SweepAngle) < 1E-10 then
    begin
      FNumVertices := 4;

      FCmd := CAggPathCmdLineTo;

      SinCosScale(startAngle, sn, CN, RY, RX);
      FVertices[0] := x + CN;
      FVertices[1] := y + sn;
      SinCosScale(startAngle + SweepAngle, sn, CN, RY, RX);
      FVertices[2] := x + CN;
      FVertices[3] := y + sn;

      Exit;
    end;

  TotalSweep := 0.0;
  LocalSweep := 0.0;

  FNumVertices := 2;

  FCmd := CAggPathCmdCurve4;
  Done := False;

  repeat
    if SweepAngle < 0.0 then
      begin
        PrevSweep := TotalSweep;
        LocalSweep := -pi * 0.5;
        TotalSweep := TotalSweep - (pi * 0.5);

        if TotalSweep <= SweepAngle + CBezierArcAngleEpsilon then
          begin
            LocalSweep := SweepAngle - PrevSweep;

            Done := True;
          end;

      end
    else
      begin
        PrevSweep := TotalSweep;
        LocalSweep := pi * 0.5;
        TotalSweep := TotalSweep + (pi * 0.5);

        if TotalSweep >= SweepAngle - CBezierArcAngleEpsilon then
          begin
            LocalSweep := SweepAngle - PrevSweep;

            Done := True;
          end;
      end;

    ArcToBezier(x, y, RX, RY, startAngle, LocalSweep,
      @FVertices[FNumVertices - 2]);

    FNumVertices := FNumVertices + 6;
    startAngle := startAngle + LocalSweep;

  until Done or (FNumVertices >= 26);
end;

procedure TAggBezierArc.Rewind(PathID: Cardinal);
begin
  FVertex := 0;
end;

function TAggBezierArc.Vertex(x, y: PDouble): Cardinal;
begin
  if FVertex >= FNumVertices then
    begin
      Result := CAggPathCmdStop;

      Exit;
    end;

  x^ := FVertices[FVertex];
  y^ := FVertices[FVertex + 1];

  inc(FVertex, 2);

  if FVertex = 2 then
      Result := CAggPathCmdMoveTo
  else
      Result := FCmd;
end;

function TAggBezierArc.GetNumVertices;
begin
  Result := FNumVertices;
end;

function TAggBezierArc.GetVertices;
begin
  Result := @FVertices;
end;

{ TAggBezierArcSvg }

constructor TAggBezierArcSvg.Create;
begin
  inherited Create;

  FRadiiOK := False;
end;

constructor TAggBezierArcSvg.Create(x1, y1, RX, RY, angle: Double;
  LargeArcFlag, SweepFlag: Boolean; x2, y2: Double);
begin
  inherited Create;

  FRadiiOK := False;

  Init(x1, y1, RX, RY, angle, LargeArcFlag, SweepFlag, x2, y2);
end;

procedure TAggBezierArcSvg.Init;
var
  i: Cardinal;

  v, p, n, Sq, x1, y1, Cx, Cy, Ux, Uy, Vx, Vy, Dx2, Dy2: Double;
  Prx, Pry, Px1, Py1, Cx1, Cy1, Sx2, Sy2, Sign, coef: Double;
  RadiiCheck, startAngle, SweepAngle, CN, sn: Double;

  Mtx: TAggTransAffineRotation;
begin
  FRadiiOK := True;

  if RX < 0.0 then
      RX := -RX;

  if RY < 0.0 then
      RY := -RX;

  // Calculate the middle point between
  // the current and the final points
  Dx2 := (x0 - x2) * 0.5;
  Dy2 := (y0 - y2) * 0.5;

  // Convert angle from degrees to radians
  SinCos(angle, sn, CN);

  // Calculate (x1, y1)
  x1 := CN * Dx2 + sn * Dy2;
  y1 := -sn * Dx2 + CN * Dy2;

  // Ensure radii are large enough
  Prx := RX * RX;
  Pry := RY * RY;
  Px1 := x1 * x1;
  Py1 := y1 * y1;

  // Check that radii are large enough
  RadiiCheck := Px1 / Prx + Py1 / Pry;

  if RadiiCheck > 1.0 then
    begin
      RX := Sqrt(RadiiCheck) * RX;
      RY := Sqrt(RadiiCheck) * RY;
      Prx := RX * RX;
      Pry := RY * RY;

      if RadiiCheck > 10.0 then
          FRadiiOK := False;
    end;

  // Calculate (cx1, cy1)
  if LargeArcFlag = SweepFlag then
      Sign := -1.0
  else
      Sign := 1.0;

  Sq := (Prx * Pry - Prx * Py1 - Pry * Px1) / (Prx * Py1 + Pry * Px1);

  if Sq < 0 then
      coef := Sign * Sqrt(0)
  else
      coef := Sign * Sqrt(Sq);

  Cx1 := coef * ((RX * y1) / RY);
  Cy1 := coef * -((RY * x1) / RX);

  // Calculate (cx, cy) from (cx1, cy1)
  Sx2 := (x0 + x2) / 2.0;
  Sy2 := (y0 + y2) / 2.0;
  Cx := Sx2 + (CN * Cx1 - sn * Cy1);
  Cy := Sy2 + (sn * Cx1 + CN * Cy1);

  // Calculate the StartAngle (angle1) and the SweepAngle (dangle)
  Ux := (x1 - Cx1) / RX;
  Uy := (y1 - Cy1) / RY;
  Vx := (-x1 - Cx1) / RX;
  Vy := (-y1 - Cy1) / RY;

  // Calculate the angle start
  n := Sqrt(Ux * Ux + Uy * Uy);
  p := Ux; // (1 * ux ) + (0 * uy )

  if Uy < 0 then
      Sign := -1.0
  else
      Sign := 1.0;

  v := p / n;

  if v < -1.0 then
      v := -1.0;

  if v > 1.0 then
      v := 1.0;

  startAngle := Sign * ArcCos(v);

  // Calculate the sweep angle
  n := Sqrt((Ux * Ux + Uy * Uy) * (Vx * Vx + Vy * Vy));
  p := Ux * Vx + Uy * Vy;

  if Ux * Vy - Uy * Vx < 0 then
      Sign := -1.0
  else
      Sign := 1.0;

  v := p / n;

  if v < -1.0 then
      v := -1.0;

  if v > 1.0 then
      v := 1.0;

  SweepAngle := Sign * ArcCos(v);

  if (not SweepFlag) and (SweepAngle > 0) then
      SweepAngle := SweepAngle - pi * 2.0
  else if SweepFlag and (SweepAngle < 0) then
      SweepAngle := SweepAngle + pi * 2.0;

  // We can now build and transform the resulting arc
  inherited Init(0.0, 0.0, RX, RY, startAngle, SweepAngle);

  Mtx := TAggTransAffineRotation.Create(angle);
  try
    Mtx.Translate(Cx, Cy);

    i := 2;

    while i < NumVertices - 2 do
      begin
        // Mtx.Transform(@FArc.Vertices[i], @FArc.Vertices[i + 1 ]);
        Mtx.Transform(Mtx, PDouble(PtrComp(GetVertices) + i * SizeOf(Double)),
          PDouble(PtrComp(GetVertices) + (i + 1) * SizeOf(Double)));

        inc(i, 2);
      end;
  finally
      Mtx.Free;
  end;

  // We must make sure that the starting and ending points
  // exactly coincide with the initial (x0,y0) and (x2,y2)
  GetVertices[0] := x0;
  GetVertices[1] := y0;

  if NumVertices > 2 then
    begin
      GetVertices[NumVertices - 2] := x2;
      GetVertices[NumVertices - 1] := y2;
    end;
end;

end. 
 
 
