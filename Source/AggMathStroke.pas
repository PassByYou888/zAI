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
unit AggMathStroke;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  Math,
  AggBasics,
  AggMath,
  AggVertexSequence,
  AggArray;

const
  // Minimal angle to calculate round joins, less than 0.1 degree.
  CAggStrokeTheta = 0.001;

type
  TAggMathStroke = class
  private
    FWidth: Double;
    FWidthAbs: Double;
    FWidthEps: Double;
    FMiterLimit: Double;
    FInnerMiterLimit: Double;
    FApproxScale: Double;

    FWidthSign: Integer;

    FLineCap: TAggLineCap;
    FLineJoin: TAggLineJoin;
    FInnerJoin: TAggInnerJoin;

    function GetWidth: Double;
    procedure SetApproximationScale(Value: Double);
    procedure SetInnerJoin(Value: TAggInnerJoin);
    procedure SetInnerMiterLimit(Value: Double);
    procedure SetLineCap(Value: TAggLineCap);
    procedure SetLineJoin(Value: TAggLineJoin);
    procedure SetMiterLimit(Value: Double);
    procedure SetWidth(Value: Double);
  public
    constructor Create;

    procedure CalculateArc(VC: TAggPodBVector; x, y, Dx1, Dy1, Dx2, Dy2: Double);
    procedure CalculateMiter(VC: TAggPodBVector; v0, v1, v2: PAggVertexDistance; Dx1, Dy1, Dx2, Dy2: Double; Lj: TAggLineJoin; Mlimit, Dbevel: Double);
    procedure CalculateCap(VC: TAggPodBVector; v0, v1: PAggVertexDistance; Len: Double);
    procedure CalculateJoin(VC: TAggPodBVector; v0, v1, v2: PAggVertexDistance; Len1, Len2: Double);

    procedure SetMiterLimitTheta(Value: Double);

    procedure AddVertex(VC: TAggPodBVector; x, y: Double);

    property LineCap: TAggLineCap read FLineCap write SetLineCap;
    property LineJoin: TAggLineJoin read FLineJoin write SetLineJoin;
    property InnerJoin: TAggInnerJoin read FInnerJoin write SetInnerJoin;

    property ApproximationScale: Double read FApproxScale write SetApproximationScale;
    property InnerMiterLimit: Double read FInnerMiterLimit write SetInnerMiterLimit;
    property MiterLimit: Double read FMiterLimit write SetMiterLimit;
    property width: Double read GetWidth write SetWidth;
  end;

procedure StrokeCalcArc(OutVertices: TAggPodDeque; x, y, Dx1, Dy1, Dx2, Dy2, width, ApproximationScale: Double);

procedure StrokeCalcMiter(OutVertices: TAggPodDeque; v0, v1, v2: PAggVertexDistance;
  Dx1, Dy1, Dx2, Dy2, width: Double; LineJoin: TAggLineJoin; MiterLimit, ApproximationScale: Double);

procedure StrokeCalcCap(OutVertices: TAggPodDeque; v0, v1: PAggVertexDistance;
  Len: Double; LineCap: TAggLineCap; width, ApproximationScale: Double);

procedure StrokeCalcJoin(OutVertices: TAggPodDeque; v0, v1, v2: PAggVertexDistance;
  Len1, Len2, width: Double; LineJoin: TAggLineJoin; InnerJoin: TAggInnerJoin; MiterLimit, InnerMiterLimit, ApproximationScale: Double);

implementation


{ TAggMathStroke }

constructor TAggMathStroke.Create;
begin
  FWidth := 0.5;
  FWidthAbs := 0.5;
  FWidthEps := 1 / 2048;
  FWidthSign := 1;

  FMiterLimit := 4.0;
  FInnerMiterLimit := 1.01;
  FApproxScale := 1.0;

  FLineCap := lcButt;
  FLineJoin := ljMiter;
  FInnerJoin := ijMiter;
end;

procedure TAggMathStroke.SetLineCap(Value: TAggLineCap);
begin
  FLineCap := Value;
end;

procedure TAggMathStroke.SetLineJoin(Value: TAggLineJoin);
begin
  FLineJoin := Value;
end;

procedure TAggMathStroke.SetInnerJoin(Value: TAggInnerJoin);
begin
  FInnerJoin := Value;
end;

procedure TAggMathStroke.SetWidth(Value: Double);
const
  CScale = 1 / 1024;
begin
  FWidth := Value * 0.5;

  if FWidth < 0 then
    begin
      FWidthAbs := -FWidth;
      FWidthSign := -1;
    end
  else
    begin
      FWidthAbs := FWidth;
      FWidthSign := 1;
    end;

  FWidthEps := FWidth * CScale;
end;

procedure TAggMathStroke.SetMiterLimit(Value: Double);
begin
  FMiterLimit := Value;
end;

procedure TAggMathStroke.SetMiterLimitTheta(Value: Double);
begin
  FMiterLimit := 1.0 / Sin(Value * 0.5);
end;

procedure TAggMathStroke.SetInnerMiterLimit(Value: Double);
begin
  FInnerMiterLimit := Value;
end;

procedure TAggMathStroke.SetApproximationScale(Value: Double);
begin
  FApproxScale := Value;
end;

function TAggMathStroke.GetWidth: Double;
begin
  Result := 2 * FWidth;
end;

procedure TAggMathStroke.CalculateCap(VC: TAggPodBVector; v0,
  v1: PAggVertexDistance; Len: Double);
var
  Delta: array [0 .. 1] of TPointDouble;
  DA, a1: Double;
  sn, CN: Double;
  i, n: Integer;
begin
  VC.RemoveAll;

  DA := 1 / Len;
  Delta[0].x := (v1.Pos.y - v0.Pos.y) * DA;
  Delta[0].y := (v1.Pos.x - v0.Pos.x) * DA;
  Delta[1] := PointDouble(0);

  Delta[0].x := Delta[0].x * FWidth;
  Delta[0].y := Delta[0].y * FWidth;

  if FLineCap <> lcRound then
    begin
      if FLineCap = lcSquare then
        begin
          Delta[1].x := Delta[0].y * FWidthSign;
          Delta[1].y := Delta[0].x * FWidthSign;
        end;

      AddVertex(VC, v0.Pos.x - Delta[0].x - Delta[1].x,
        v0.Pos.y + Delta[0].y - Delta[1].y);
      AddVertex(VC, v0.Pos.x + Delta[0].x - Delta[1].x,
        v0.Pos.y - Delta[0].y - Delta[1].y);
    end
  else
    begin
      DA := ArcCos(FWidthAbs / (FWidthAbs + 0.125 / FApproxScale)) * 2;
      n := Integer(Trunc(pi / DA));
      DA := pi / (n + 1);

      AddVertex(VC, v0.Pos.x - Delta[0].x, v0.Pos.y + Delta[0].y);

      if FWidthSign > 0 then
        begin
          a1 := ArcTan2(Delta[0].y, -Delta[0].x);
          a1 := a1 + DA;
          i := 0;

          while i < n do
            begin
              SinCosScale(a1, sn, CN, FWidth);
              AddVertex(VC, v0.Pos.x + CN, v0.Pos.y + sn);

              a1 := a1 + DA;

              inc(i);
            end;
        end
      else
        begin
          a1 := ArcTan2(-Delta[0].y, Delta[0].x);
          a1 := a1 - DA;
          i := 0;

          while i < n do
            begin
              SinCosScale(a1, sn, CN, FWidth);
              AddVertex(VC, v0.Pos.x + CN, v0.Pos.y + sn);

              a1 := a1 - DA;

              inc(i);
            end;
        end;

      AddVertex(VC, v0.Pos.x + Delta[0].x, v0.Pos.y - Delta[0].y);
    end;
end;

procedure TAggMathStroke.CalculateJoin(VC: TAggPodBVector;
  v0, v1, v2: PAggVertexDistance; Len1, Len2: Double);
var
  Dx1, Dy1, Dx2, Dy2, cp, Limit, dx, dy, Dbevel, Temp: Double;
begin
  Temp := FWidth / Len1;
  Dx1 := (v1.Pos.y - v0.Pos.y) * Temp;
  Dy1 := (v1.Pos.x - v0.Pos.x) * Temp;
  Temp := FWidth / Len2;
  Dx2 := (v2.Pos.y - v1.Pos.y) * Temp;
  Dy2 := (v2.Pos.x - v1.Pos.x) * Temp;

  VC.RemoveAll;

  cp := CrossProduct(v0.Pos.x, v0.Pos.y, v1.Pos.x, v1.Pos.y, v2.Pos.x, v2.Pos.y);

  if (cp <> 0) and ((cp > 0) = (FWidth > 0)) then
    begin
      // Inner join
      if Len1 < Len2 then
          Limit := Len1 / FWidthAbs
      else
          Limit := Len2 / FWidthAbs;

      if Limit < FInnerMiterLimit then
          Limit := FInnerMiterLimit;

      case FInnerJoin of
        ijMiter:
          CalculateMiter(VC, v0, v1, v2, Dx1, Dy1, Dx2, Dy2, ljMiterRevert,
            Limit, 0);

        ijJag, ijRound:
          begin
            cp := (Dx1 - Dx2) * (Dx1 - Dx2) + (Dy1 - Dy2) * (Dy1 - Dy2);

            if (cp < Len1 * Len1) and (cp < Len2 * Len2) then
                CalculateMiter(VC, v0, v1, v2, Dx1, Dy1, Dx2, Dy2,
                ljMiterRevert, Limit, 0)
            else if FInnerJoin = ijJag then
              begin
                AddVertex(VC, v1.Pos.x + Dx1, v1.Pos.y - Dy1);
                AddVertex(VC, v1.Pos.x, v1.Pos.y);
                AddVertex(VC, v1.Pos.x + Dx2, v1.Pos.y - Dy2);

              end
            else
              begin
                AddVertex(VC, v1.Pos.x + Dx1, v1.Pos.y - Dy1);
                AddVertex(VC, v1.Pos.x, v1.Pos.y);
                CalculateArc(VC, v1.Pos.x, v1.Pos.y, Dx2, -Dy2, Dx1, -Dy1);
                AddVertex(VC, v1.Pos.x, v1.Pos.y);
                AddVertex(VC, v1.Pos.x + Dx2, v1.Pos.y - Dy2);
              end;
          end;
        else
          begin
            // ijBevel
            AddVertex(VC, v1.Pos.x + Dx1, v1.Pos.y - Dy1);
            AddVertex(VC, v1.Pos.x + Dx2, v1.Pos.y - Dy2);
          end;
      end;
    end
  else
    begin
      // Outer join
      // ---------------
      // Calculate the distance between v1 and
      // the central point of the bevel line segment
      dx := (Dx1 + Dx2) * 0.5;
      dy := (Dy1 + Dy2) * 0.5;

      Dbevel := Hypot(dx, dy);

      if (FLineJoin = ljRound) or (FLineJoin = ljBevel) then
        begin
          // This is an optimization that reduces the number of points
          // in cases of almost collinear segments. If there's no
          // visible difference between bevel and miter joins we'd rather
          // use miter join because it adds only one point instead of two.
          //
          // Here we calculate the middle point between the bevel points
          // and then, the distance between v1 and this middle point.
          // At outer joins this distance always less than stroke width,
          // because it's actually the height of an isosceles triangle of
          // v1 and its two bevel points. If the difference between this
          // width and this value is small (no visible bevel) we can
          // add just one point.
          //
          // The constant in the expression makes the result approximately
          // the same as in round joins and caps. You can safely comment
          // out this entire "if".
          if FApproxScale * (FWidthAbs - Dbevel) < FWidthEps then
            begin
              if CalculateIntersection(v0.Pos.x + Dx1, v0.Pos.y - Dy1,
                v1.Pos.x + Dx1, v1.Pos.y - Dy1, v1.Pos.x + Dx2, v1.Pos.y - Dy2,
                v2.Pos.x + Dx2, v2.Pos.y - Dy2, @dx, @dy) then
                  AddVertex(VC, dx, dy)
              else
                  AddVertex(VC, v1.Pos.x + Dx1, v1.Pos.y - Dy1);

              Exit;
            end;
        end;

      case FLineJoin of
        ljMiter, ljMiterRevert, ljMiterRound:
          CalculateMiter(VC, v0, v1, v2, Dx1, Dy1, Dx2, Dy2, FLineJoin,
            FMiterLimit, Dbevel);

        ljRound:
          CalculateArc(VC, v1.Pos.x, v1.Pos.y, Dx1, -Dy1, Dx2, -Dy2);
        else
          begin
            // Bevel join
            AddVertex(VC, v1.Pos.x + Dx1, v1.Pos.y - Dy1);
            AddVertex(VC, v1.Pos.x + Dx2, v1.Pos.y - Dy2);
          end;
      end;
    end;
end;

procedure TAggMathStroke.AddVertex(VC: TAggPodBVector; x, y: Double);
var
  pt: TPointDouble;
begin
  pt.x := x;
  pt.y := y;

  VC.Add(@pt);
end;

procedure TAggMathStroke.CalculateArc(VC: TAggPodBVector;
  x, y, Dx1, Dy1, Dx2, Dy2: Double);
var
  a1, a2, DA: Double;
  sn, CN: Double;
  i, n: Integer;
begin
  a1 := ArcTan2(Dy1 * FWidthSign, Dx1 * FWidthSign);
  a2 := ArcTan2(Dy2 * FWidthSign, Dx2 * FWidthSign);
  DA := a1 - a2;
  DA := ArcCos(FWidthAbs / (FWidthAbs + 0.125 / FApproxScale)) * 2;

  AddVertex(VC, x + Dx1, y + Dy1);

  if FWidthSign > 0 then
    begin
      if a1 > a2 then
          a2 := a2 + 2 * pi;

      n := Integer(Trunc((a2 - a1) / DA));
      DA := (a2 - a1) / (n + 1);
      a1 := a1 + DA;
      i := 0;

      while i < n do
        begin
          SinCos(a1, sn, CN);
          AddVertex(VC, x + CN * FWidth, y + sn * FWidth);

          a1 := a1 + DA;

          inc(i);
        end;
    end
  else
    begin
      if a1 < a2 then
          a2 := a2 - 2 * pi;

      n := Integer(Trunc((a1 - a2) / DA));
      DA := (a1 - a2) / (n + 1);
      a1 := a1 - DA;
      i := 0;

      while i < n do
        begin
          SinCos(a1, sn, CN);
          AddVertex(VC, x + CN * FWidth, y + sn * FWidth);

          a1 := a1 - DA;

          inc(i);
        end;
    end;

  AddVertex(VC, x + Dx2, y + Dy2);
end;

procedure TAggMathStroke.CalculateMiter(VC: TAggPodBVector;
  v0, v1, v2: PAggVertexDistance; Dx1, Dy1, Dx2, Dy2: Double; Lj: TAggLineJoin;
  Mlimit, Dbevel: Double);
var
  XI, Yi, di, Lim, x2, y2, x1, y1: Double;
  MiterLimitExceeded, IntersectionFailed: Boolean;
begin
  XI := v1.Pos.x;
  Yi := v1.Pos.y;
  di := 1;
  Lim := FWidthAbs * Mlimit;

  MiterLimitExceeded := True; // Assume the worst
  IntersectionFailed := True; // Assume the worst

  if CalculateIntersection(v0.Pos.x + Dx1, v0.Pos.y - Dy1, v1.Pos.x + Dx1,
    v1.Pos.y - Dy1, v1.Pos.x + Dx2, v1.Pos.y - Dy2, v2.Pos.x + Dx2,
    v2.Pos.y - Dy2, @XI, @Yi) then
    begin
      // Calculation of the intersection succeeded
      di := CalculateDistance(v1.Pos.x, v1.Pos.y, XI, Yi);

      if di <= Lim then
        begin
          // Inside the miter limit
          AddVertex(VC, XI, Yi);

          MiterLimitExceeded := False;
        end;

      IntersectionFailed := False;
    end
  else
    begin
      // Calculation of the intersection failed, most probably
      // the three points lie one straight line.
      // First check if v0 and v2 lie on the opposite sides of vector:
      // (v1.x, v1.y) -> (v1.x+dx1, v1.y-dy1), that is, the perpendicular
      // to the line determined by vertices v0 and v1.
      // This condition determines whether the next line segments continues
      // the previous one or goes back.
      x2 := v1.Pos.x + Dx1;
      y2 := v1.Pos.y - Dy1;

      if (CrossProduct(v0.Pos.x, v0.Pos.y, v1.Pos.x, v1.Pos.y, x2, y2) < 0.0)
        = (CrossProduct(v1.Pos.x, v1.Pos.y, v2.Pos.x, v2.Pos.y, x2, y2) < 0.0) then
        begin
          // This case means that the next segment continues
          // the previous one (straight line)
          AddVertex(VC, v1.Pos.x + Dx1, v1.Pos.y - Dy1);

          MiterLimitExceeded := False;
        end;
    end;

  // Miter limit exceeded
  if MiterLimitExceeded then
    case Lj of
      ljMiterRevert:
        begin
          // For the compatibility with SVG, PDF, etc,
          // we use a simple bevel join instead of
          // "smart" bevel
          AddVertex(VC, v1.Pos.x + Dx1, v1.Pos.y - Dy1);
          AddVertex(VC, v1.Pos.x + Dx2, v1.Pos.y - Dy2);
        end;

      ljMiterRound:
        CalculateArc(VC, v1.Pos.x, v1.Pos.y, Dx1, -Dy1, Dx2, -Dy2);

      // If no miter-revert, calculate new dx1, dy1, dx2, dy2
      else
        if IntersectionFailed then
          begin
            Mlimit := Mlimit * FWidthSign;

            AddVertex(VC, v1.Pos.x + Dx1 + Dy1 * Mlimit, v1.Pos.y - Dy1 + Dx1 * Mlimit);

            AddVertex(VC, v1.Pos.x + Dx2 - Dy2 * Mlimit, v1.Pos.y - Dy2 - Dx2 * Mlimit);

          end
        else
          begin
            x1 := v1.Pos.x + Dx1;
            y1 := v1.Pos.y - Dy1;
            x2 := v1.Pos.x + Dx2;
            y2 := v1.Pos.y - Dy2;
            di := (Lim - Dbevel) / (di - Dbevel);

            AddVertex(VC, x1 + (XI - x1) * di, y1 + (Yi - y1) * di);

            AddVertex(VC, x2 + (XI - x2) * di, y2 + (Yi - y2) * di);
          end;
    end;
end;

procedure StrokeCalcArc(OutVertices: TAggPodDeque;
  x, y, Dx1, Dy1, Dx2, Dy2, width, ApproximationScale: Double);
var
  pt: TPointDouble;

  a1, a2, DA: Double;
  sn, CN: Double;

  Ccw: Boolean;
begin
  a1 := ArcTan2(Dy1, Dx1);
  a2 := ArcTan2(Dy2, Dx2);
  DA := a1 - a2;

  // Possible optimization. Not important at all; consumes time but happens rarely
  // if Abs(da ) < CAggStrokeTheta then
  // begin
  // pt.x:=(x + x + dx1 + dx2 ) * 0.5;
  // pt.y:=(y + y + dy1 + dy2 ) * 0.5;
  //
  // OutVertices.add(@pt );
  // exit;
  //
  // end;
  Ccw := (DA > 0.0) and (DA < pi);

  if width < 0 then
      width := -width;

  if ApproximationScale = 0 then
      ApproximationScale := 0.00001;

  DA := ArcCos(width / (width + 0.125 / ApproximationScale)) * 2;

  pt.x := x + Dx1;
  pt.y := y + Dy1;

  OutVertices.Add(@pt);

  if not Ccw then
    begin
      if a1 > a2 then
          a2 := a2 + (2 * pi);

      a2 := a2 - 0.25 * DA;
      a1 := a1 + DA;

      while a1 < a2 do
        begin
          SinCosScale(a1, sn, CN, width);
          pt.x := x + CN;
          pt.y := y + sn;

          OutVertices.Add(@pt);

          a1 := a1 + DA;
        end;
    end
  else
    begin
      if a1 < a2 then
          a2 := a2 - (2 * pi);

      a2 := a2 + 0.25 * DA;
      a1 := a1 - DA;

      while a1 > a2 do
        begin
          SinCosScale(a1, sn, CN, width);
          pt.x := x + CN;
          pt.y := y + sn;

          OutVertices.Add(@pt);

          a1 := a1 - DA;
        end;
    end;

  pt.x := x + Dx2;
  pt.y := y + Dy2;

  OutVertices.Add(@pt);
end;

procedure StrokeCalcMiter(OutVertices: TAggPodDeque;
  v0, v1, v2: PAggVertexDistance; Dx1, Dy1, Dx2, Dy2, width: Double;
  LineJoin: TAggLineJoin; MiterLimit, ApproximationScale: Double);
var
  pt: TPointDouble;
  XI, Yi, d1, Lim, x2, y2: Double;
  MiterLimitExceeded: Boolean;
begin
  XI := v1.Pos.x;
  Yi := v1.Pos.y;

  MiterLimitExceeded := True; // Assume the worst

  if CalculateIntersection(v0.Pos.x + Dx1, v0.Pos.y - Dy1, v1.Pos.x + Dx1,
    v1.Pos.y - Dy1, v1.Pos.x + Dx2, v1.Pos.y - Dy2, v2.Pos.x + Dx2,
    v2.Pos.y - Dy2, @XI, @Yi) then
    begin
      // Calculation of the intersection succeeded
      // ---------------------
      d1 := CalculateDistance(v1.Pos.x, v1.Pos.y, XI, Yi);
      Lim := width * MiterLimit;

      if d1 <= Lim then
        begin
          // Inside the miter limit
          // ---------------------
          pt.x := XI;
          pt.y := Yi;

          OutVertices.Add(@pt);

          MiterLimitExceeded := False;
        end;
    end
  else
    begin
      // Calculation of the intersection failed, most probably
      // the three points lie one straight line.
      // First check if v0 and v2 lie on the opposite sides of vector:
      // (v1.x, v1.y) -> (v1.x+dx1, v1.y-dy1), that is, the perpendicular
      // to the line determined by vertices v0 and v1.
      // This condition determines whether the next line segments continues
      // the previous one or goes back.
      // ----------------
      x2 := v1.Pos.x + Dx1;
      y2 := v1.Pos.y - Dy1;

      if (((x2 - v0.Pos.x) * Dy1 - (v0.Pos.y - y2) * Dx1 < 0.0) <>
        ((x2 - v2.Pos.x) * Dy1 - (v2.Pos.y - y2) * Dx1 < 0.0)) then
        begin
          // This case means that the next segment continues
          // the previous one (straight line)
          // -----------------
          pt.x := v1.Pos.x + Dx1;
          pt.y := v1.Pos.y - Dy1;

          OutVertices.Add(@pt);

          MiterLimitExceeded := False;
        end;
    end;

  if MiterLimitExceeded then
    // Miter limit exceeded
    // ------------------------
    case LineJoin of
      ljMiterRevert:
        begin
          // For the compatibility with SVG, PDF, etc,
          // we use a simple bevel join instead of
          // "smart" bevel
          // -------------------
          pt.x := v1.Pos.x + Dx1;
          pt.y := v1.Pos.y - Dy1;

          OutVertices.Add(@pt);

          pt.x := v1.Pos.x + Dx2;
          pt.y := v1.Pos.y - Dy2;

          OutVertices.Add(@pt);
        end;

      ljMiterRound:
        StrokeCalcArc(OutVertices, v1.Pos.x, v1.Pos.y, Dx1, -Dy1, Dx2, -Dy2, width,
          ApproximationScale);

      else
        begin
          // If no miter-revert, calculate new dx1, dy1, dx2, dy2
          // ----------------
          pt.x := v1.Pos.x + Dx1 + Dy1 * MiterLimit;
          pt.y := v1.Pos.y - Dy1 + Dx1 * MiterLimit;

          OutVertices.Add(@pt);

          pt.x := v1.Pos.x + Dx2 - Dy2 * MiterLimit;
          pt.y := v1.Pos.y - Dy2 - Dx2 * MiterLimit;

          OutVertices.Add(@pt);
        end;
    end;
end;

procedure StrokeCalcCap(OutVertices: TAggPodDeque; v0, v1: PAggVertexDistance;
  Len: Double; LineCap: TAggLineCap; width, ApproximationScale: Double);
var
  pt: TPointDouble;
  Dx1, Dy1, Dx2, Dy2, a1, a2, DA: Double;
  sn, CN: Double;
begin
  OutVertices.RemoveAll;

  DA := 1 / Len;
  Dx1 := (v1.Pos.y - v0.Pos.y) * DA;
  Dy1 := (v1.Pos.x - v0.Pos.x) * DA;
  Dx2 := 0;
  Dy2 := 0;

  Dx1 := Dx1 * width;
  Dy1 := Dy1 * width;

  if LineCap <> lcRound then
    begin
      if LineCap = lcSquare then
        begin
          Dx2 := Dy1;
          Dy2 := Dx1;
        end;

      pt.x := v0.Pos.x - Dx1 - Dx2;
      pt.y := v0.Pos.y + Dy1 - Dy2;

      OutVertices.Add(@pt);

      pt.x := v0.Pos.x + Dx1 - Dx2;
      pt.y := v0.Pos.y - Dy1 - Dy2;

      OutVertices.Add(@pt);
    end
  else
    begin
      a1 := ArcTan2(Dy1, -Dx1);
      a2 := a1 + pi;

      if ApproximationScale = 0 then
          ApproximationScale := 0.00001;

      DA := ArcCos(width / (width + 0.125 / ApproximationScale)) * 2;

      pt.x := v0.Pos.x - Dx1;
      pt.y := v0.Pos.y + Dy1;

      OutVertices.Add(@pt);

      a1 := a1 + DA;
      a2 := a2 - 0.25 * DA;

      while a1 < a2 do
        begin
          SinCosScale(a1, sn, CN, width);
          pt.x := v0.Pos.x + CN;
          pt.y := v0.Pos.y + sn;

          OutVertices.Add(@pt);

          a1 := a1 + DA;
        end;

      pt.x := v0.Pos.x + Dx1;
      pt.y := v0.Pos.y - Dy1;

      OutVertices.Add(@pt);
    end;
end;

procedure StrokeCalcJoin(OutVertices: TAggPodDeque;
  v0, v1, v2: PAggVertexDistance; Len1, Len2, width: Double;
  LineJoin: TAggLineJoin; InnerJoin: TAggInnerJoin; MiterLimit, InnerMiterLimit,
  ApproximationScale: Double);
var
  pt: TPointDouble;

  d, Dx1, Dy1, Dx2, Dy2: Double;
begin
  d := width / Len1;
  Dx1 := (v1.Pos.y - v0.Pos.y) * d;
  Dy1 := (v1.Pos.x - v0.Pos.x) * d;

  d := width / Len2;
  Dx2 := (v2.Pos.y - v1.Pos.y) * d;
  Dy2 := (v2.Pos.x - v1.Pos.x) * d;

  OutVertices.RemoveAll;

  if CalculatePointLocation(v0.Pos.x, v0.Pos.y, v1.Pos.x, v1.Pos.y, v2.Pos.x, v2.Pos.y) > 0 then
    // Inner join
    // ---------------
    case InnerJoin of
      ijMiter:
        StrokeCalcMiter(OutVertices, v0, v1, v2, Dx1, Dy1, Dx2, Dy2, width,
          ljMiterRevert, InnerMiterLimit, 1.0);

      ijJag, ijRound:
        begin
          d := Sqr(Dx1 - Dx2) + Sqr(Dy1 - Dy2);

          if (d < Len1 * Len1) and (d < Len2 * Len2) then
              StrokeCalcMiter(OutVertices, v0, v1, v2, Dx1, Dy1, Dx2, Dy2,
              width, ljMiterRevert, InnerMiterLimit, 1.0)

          else if InnerJoin = ijJag then
            begin
              pt.x := v1.Pos.x + Dx1;
              pt.y := v1.Pos.y - Dy1;

              OutVertices.Add(@pt);

              pt.x := v1.Pos.x;
              pt.y := v1.Pos.y;

              OutVertices.Add(@pt);

              pt.x := v1.Pos.x + Dx2;
              pt.y := v1.Pos.y - Dy2;

              OutVertices.Add(@pt);
            end
          else
            begin
              pt.x := v1.Pos.x + Dx1;
              pt.y := v1.Pos.y - Dy1;

              OutVertices.Add(@pt);

              pt.x := v1.Pos.x;
              pt.y := v1.Pos.y;

              OutVertices.Add(@pt);

              StrokeCalcArc(OutVertices, v1.Pos.x, v1.Pos.y, Dx2, -Dy2, Dx1, -Dy1,
                width, ApproximationScale);

              pt.x := v1.Pos.x;
              pt.y := v1.Pos.y;

              OutVertices.Add(@pt);

              pt.x := v1.Pos.x + Dx2;
              pt.y := v1.Pos.y - Dy2;

              OutVertices.Add(@pt);
            end;
        end;
      else // ijBevel
        begin
          pt.x := v1.Pos.x + Dx1;
          pt.y := v1.Pos.y - Dy1;

          OutVertices.Add(@pt);

          pt.x := v1.Pos.x + Dx2;
          pt.y := v1.Pos.y - Dy2;

          OutVertices.Add(@pt);
        end;

    end
  else
    // Outer join
    // ---------------
    case LineJoin of
      ljMiter, ljMiterRevert, ljMiterRound:
        StrokeCalcMiter(OutVertices, v0, v1, v2, Dx1, Dy1, Dx2, Dy2, width,
          LineJoin, MiterLimit, ApproximationScale);

      ljRound:
        StrokeCalcArc(OutVertices, v1.Pos.x, v1.Pos.y, Dx1, -Dy1, Dx2, -Dy2,
          width, ApproximationScale);

      else // Bevel join
        begin
          pt.x := v1.Pos.x + Dx1;
          pt.y := v1.Pos.y - Dy1;

          OutVertices.Add(@pt);

          pt.x := v1.Pos.x + Dx2;
          pt.y := v1.Pos.y - Dy2;

          OutVertices.Add(@pt);
        end;
    end;
end;

end. 
 
 
 
