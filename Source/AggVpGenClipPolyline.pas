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
unit AggVpGenClipPolyline;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggVpGen,
  AggVertexSource;

type
  TAggClippingFlag  = (cfX1, cfX2, cfY1, cfY2);
  TAggClippingFlags = set of TAggClippingFlag;

  TAggVpgenClipPolyline = class(TAggCustomVpgen)
  private
    FClipBox: TRectDouble;

    FPoint: array [0 .. 1] of TPointDouble;
    FF1: TAggClippingFlags;
    FF2: TAggClippingFlags;

    fx, fy: array [0 .. 1] of Double;
    FCmd: array [0 .. 1] of Cardinal;

    FNumVertices, FVertex: Cardinal;
  protected
    function MovePoint(x, y: PDouble; var Flags: TAggClippingFlags): Boolean;

    // Determine the clipping code of the vertex according to the
    // Cyrus-Beck line clipping algorithm
    function ClippingFlagsX(x: Double): TAggClippingFlags;
    function ClippingFlagsY(y: Double): TAggClippingFlags;
    function ClippingFlags(x, y: Double): TAggClippingFlags;

    procedure ClipLineSegment;

    function GetX1: Double;
    function GetY1: Double;
    function GetX2: Double;
    function GetY2: Double;

    function GetAutoClose: Boolean;
    function GetAutoUnclose: Boolean;
  public
    constructor Create; override;

    procedure Reset; override;
    procedure MoveTo(x, y: Double); override;
    procedure LineTo(x, y: Double); override;

    function Vertex(x, y: PDouble): Cardinal; override;

    procedure SetClipBox(x1, y1, x2, y2: Double); overload;
    procedure SetClipBox(Bounds: TRectDouble); overload;

    property x1: Double read GetX1;
    property y1: Double read GetY1;
    property x2: Double read GetX2;
    property y2: Double read GetY2;
  end;

implementation

const
  CClipEpsilon = 1E-10;

  { TAggVpgenClipPolyline }

constructor TAggVpgenClipPolyline.Create;
begin
  FClipBox := RectDouble(0, 0, 1, 1);

  FPoint[0] := PointDouble(0);
  FPoint[1] := PointDouble(0);
  FF1 := [];
  FF2 := [];

  FNumVertices := 0;
  FVertex := 0;
end;

procedure TAggVpgenClipPolyline.SetClipBox(x1, y1, x2, y2: Double);
begin
  FClipBox := RectDouble(x1, y1, x2, y2);
  FClipBox.Normalize;
end;

procedure TAggVpgenClipPolyline.SetClipBox(Bounds: TRectDouble);
begin
  FClipBox := Bounds;
  FClipBox.Normalize;
end;

function TAggVpgenClipPolyline.GetX1;
begin
  Result := FClipBox.x1;
end;

function TAggVpgenClipPolyline.GetY1;
begin
  Result := FClipBox.y1;
end;

function TAggVpgenClipPolyline.GetX2;
begin
  Result := FClipBox.x2;
end;

function TAggVpgenClipPolyline.GetY2;
begin
  Result := FClipBox.y2;
end;

function TAggVpgenClipPolyline.GetAutoClose;
begin
  Result := False;
end;

function TAggVpgenClipPolyline.GetAutoUnclose;
begin
  Result := True;
end;

procedure TAggVpgenClipPolyline.Reset;
begin
  FVertex := 0;
  FNumVertices := 0;
end;

procedure TAggVpgenClipPolyline.MoveTo;
begin
  FVertex := 0;
  FNumVertices := 0;

  FF1 := ClippingFlags(x, y);

  if FF1 = [] then
    begin
      fx[0] := x;
      fy[0] := y;

      FCmd[0] := CAggPathCmdMoveTo;

      FNumVertices := 1;
    end;

  FPoint[0] := PointDouble(x, y);
end;

procedure TAggVpgenClipPolyline.LineTo;
var
  f: TAggClippingFlags;
begin
  FVertex := 0;
  FNumVertices := 0;

  FPoint[1].x := x;
  FPoint[1].y := y;

  f := ClippingFlags(FPoint[1].x, FPoint[1].y);
  FF2 := f;

  if FF2 = FF1 then
    if FF2 = [] then
      begin
        fx[0] := x;
        fy[0] := y;

        FCmd[0] := CAggPathCmdLineTo;

        FNumVertices := 1;
      end
    else
  else
      ClipLineSegment;

  FF1 := f;
  FPoint[0].x := x;
  FPoint[0].y := y;
end;

function TAggVpgenClipPolyline.Vertex(x, y: PDouble): Cardinal;
begin
  if FVertex < FNumVertices then
    begin
      x^ := fx[FVertex];
      y^ := fy[FVertex];

      Result := FCmd[FVertex];

      inc(FVertex);
    end
  else
      Result := CAggPathCmdStop;
end;

function TAggVpgenClipPolyline.ClippingFlagsX(x: Double): TAggClippingFlags;
var
  f: TAggClippingFlags;
begin
  f := [];

  if x < FClipBox.x1 then
      f := f + [cfX1];

  if x > FClipBox.x2 then
      f := f + [cfX2];

  Result := f;
end;

function TAggVpgenClipPolyline.ClippingFlagsY(y: Double): TAggClippingFlags;
var
  f: TAggClippingFlags;
begin
  f := [];

  if y < FClipBox.y1 then
      f := f + [cfY1];

  if y > FClipBox.y2 then
      f := f + [cfY2];

  Result := f;
end;

function TAggVpgenClipPolyline.ClippingFlags;
begin
  Result := ClippingFlagsX(x) + ClippingFlagsY(y);
end;

function TAggVpgenClipPolyline.MovePoint(x, y: PDouble; var Flags: TAggClippingFlags): Boolean;
var
  Bound: Double;
begin
  if (cfX1 in Flags) or (cfX2 in Flags) then
    begin
      if cfX1 in Flags then
          Bound := FClipBox.x1
      else
          Bound := FClipBox.x2;

      y^ := (Bound - FPoint[0].x) * (FPoint[1].y - FPoint[0].y) /
        (FPoint[1].x - FPoint[0].x) + FPoint[0].y;
      x^ := Bound;

      Flags := ClippingFlagsY(y^);
    end;

  if (Abs(FPoint[1].y - FPoint[0].y) < CClipEpsilon) and
    (Abs(FPoint[1].x - FPoint[0].x) < CClipEpsilon) then
    begin
      Result := False;

      Exit;
    end;

  if (cfY1 in Flags) or (cfY2 in Flags) then
    begin
      if cfY1 in Flags then
          Bound := FClipBox.y1
      else
          Bound := FClipBox.y2;

      x^ := (Bound - FPoint[0].y) * (FPoint[1].x - FPoint[0].x) /
        (FPoint[1].y - FPoint[0].y) + FPoint[0].x;
      y^ := Bound;
    end;

  Flags := [];
  Result := True;
end;

procedure TAggVpgenClipPolyline.ClipLineSegment;
begin
  if FF1 + FF2 = [] then
    begin
      if FF1 <> [] then
        begin
          if not MovePoint(@FPoint[0].x, @FPoint[0].y, FF1) then
              Exit;

          if FF1 <> [] then
              Exit;

          fx[0] := FPoint[0].x;
          fy[0] := FPoint[0].y;

          FCmd[0] := CAggPathCmdMoveTo;

          FNumVertices := 1;
        end;

      if FF2 <> [] then // Move Point 2
        if not MovePoint(@FPoint[1].x, @FPoint[1].y, FF2) then
            Exit;

      fx[FNumVertices] := FPoint[1].x;
      fy[FNumVertices] := FPoint[1].y;

      FCmd[FNumVertices] := CAggPathCmdLineTo;

      inc(FNumVertices);
    end;
end;

end. 
 
 
