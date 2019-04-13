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
unit AggRendererMarkers;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggColor32,
  AggRendererBase,
  AggRendererPrimitives,
  AggEllipseBresenham;

type
  TAggMarker = (
    meSquare, meDiamond, meCircle, meCrossedCircle, meSemiEllipseLeft,
    meSemiEllipseRight, meSemiEllipseUp, meSemiEllipseDown,
    meTriangleLeft, meTriangleRight, meTriangleUp, meTriangleDown,
    meFourRays, meCross, meX, meDash, meDot, mePixel);

  TAggRendererMarkers = class(TAggRendererPrimitives)
  public
    constructor Create(RBuf: TAggRendererBase);

    function Visible(x, y, r: Integer): Boolean;

    procedure Square(x, y, r: Integer);
    procedure Diamond(x, y, r: Integer);

    procedure Circle(x, y, r: Integer);
    procedure CrossedCircle(x, y, r: Integer);

    procedure SemiEllipseLeft(x, y, r: Integer);
    procedure SemiEllipseRight(x, y, r: Integer);
    procedure SemiEllipseUp(x, y, r: Integer);
    procedure SemiEllipseDown(x, y, r: Integer);

    procedure TriangleLeft(x, y, r: Integer);
    procedure TriangleRight(x, y, r: Integer);
    procedure TriangleUp(x, y, r: Integer);
    procedure TriangleDown(x, y, r: Integer);

    procedure FourRays(x, y, r: Integer);

    procedure Cross(x, y, r: Integer);
    procedure Xing(x, y, r: Integer);
    procedure Dash(x, y, r: Integer);
    procedure dot(x, y, r: Integer);
    procedure Pixel(x, y, r: Integer);

    procedure Marker(x, y, r: Integer; MarkerType: TAggMarker);

    procedure Markers(n: Integer; x, y: PInteger; r: Integer; MarkerType: TAggMarker); overload;
    procedure Markers(n: Integer; x, y, r: PInteger; MarkerType: TAggMarker); overload;
    procedure Markers(n: Integer; x, y, r: PInteger; Fc: PAggColor; MarkerType: TAggMarker); overload;
    procedure Markers(n: Integer; x, y, r: PInteger; Fc, LC: PAggColor; MarkerType: TAggMarker); overload;
  end;

implementation


{ TAggRendererMarkers }

constructor TAggRendererMarkers.Create(RBuf: TAggRendererBase);
begin
  Assert(RBuf is TAggRendererBase);
  inherited Create(RBuf);
end;

function TAggRendererMarkers.Visible(x, y, r: Integer): Boolean;
var
  RC: TRectInteger;
begin
  RC := RectInteger(x - r, y - r, x + y, y + r);

  Result := RC.Clip(RenderBase.BoundingClipBox^);
end;

procedure TAggRendererMarkers.Square(x, y, r: Integer);
begin
  if Visible(x, y, r) then
    if r <> 0 then
        OutlinedRectangle(x - r, y - r, x + r, y + r)
    else
        RenderBase.BlendPixel(x, y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.Diamond(x, y, r: Integer);
var
  Delta: TPointInteger;
begin
  if Visible(x, y, r) then
    if r <> 0 then
      begin
        Delta := PointInteger(0, -r);

        repeat
          RenderBase.BlendPixel(x - Delta.x, y + Delta.y, @FLineColor,
            CAggCoverFull);
          RenderBase.BlendPixel(x + Delta.x, y + Delta.y, @FLineColor,
            CAggCoverFull);
          RenderBase.BlendPixel(x - Delta.x, y - Delta.y, @FLineColor,
            CAggCoverFull);
          RenderBase.BlendPixel(x + Delta.x, y - Delta.y, @FLineColor,
            CAggCoverFull);

          if Delta.x <> 0 then
            begin
              RenderBase.BlendHorizontalLine(x - Delta.x + 1, y + Delta.y,
                x + Delta.x - 1, @FFillColor, CAggCoverFull);
              RenderBase.BlendHorizontalLine(x - Delta.x + 1, y - Delta.y,
                x + Delta.x - 1, @FFillColor, CAggCoverFull);
            end;

          inc(Delta.y);
          inc(Delta.x);
        until Delta.y > 0;
      end
    else
        RenderBase.BlendPixel(x, y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.Circle(x, y, r: Integer);
begin
  if Visible(x, y, r) then
    if r <> 0 then
        OutlinedEllipse(x, y, r, r)
    else
        RenderBase.BlendPixel(x, y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.CrossedCircle(x, y, r: Integer);
var
  R6: Integer;
begin
  if Visible(x, y, r) then
    if r <> 0 then
      begin
        OutlinedEllipse(x, y, r, r);

        R6 := r + ShrInt32(r, 1);

        if r <= 2 then
            inc(R6);

        r := ShrInt32(r, 1);

        RenderBase.BlendHorizontalLine(x - R6, y, x - r, @FLineColor,
          CAggCoverFull);
        RenderBase.BlendHorizontalLine(x + r, y, x + R6, @FLineColor,
          CAggCoverFull);
        RenderBase.BlendVerticalLine(x, y - R6, y - r, @FLineColor,
          CAggCoverFull);
        RenderBase.BlendVerticalLine(x, y + r, y + R6, @FLineColor,
          CAggCoverFull);
      end
    else
        RenderBase.BlendPixel(x, y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.SemiEllipseLeft(x, y, r: Integer);
var
  r8: Integer;
  Delta: TPointInteger;
  Ei: TAggEllipseBresenhamInterpolator;
begin
  if Visible(x, y, r) then
    if r <> 0 then
      begin
        r8 := r * 4 div 5;
        Delta := PointInteger(0, -r);

        Ei.Initialize(r * 3 div 5, r + r8);

        repeat
          inc(Delta.x, Ei.deltax);
          inc(Delta.y, Ei.deltay);

          RenderBase.BlendPixel(x + Delta.y, y + Delta.x, @FLineColor,
            CAggCoverFull);
          RenderBase.BlendPixel(x + Delta.y, y - Delta.x, @FLineColor,
            CAggCoverFull);

          if (Ei.deltay <> 0) and (Delta.x <> 0) then
              RenderBase.BlendVerticalLine(x + Delta.y, y - Delta.x + 1,
              y + Delta.x - 1, @FFillColor, CAggCoverFull);

          Ei.IncOperator;
        until Delta.y >= r8;

        RenderBase.BlendVerticalLine(x + Delta.y, y - Delta.x, y + Delta.x,
          @FLineColor, CAggCoverFull);
      end
    else
        RenderBase.BlendPixel(x, y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.SemiEllipseRight(x, y, r: Integer);
var
  r8: Integer;
  Delta: TPointInteger;
  Ei: TAggEllipseBresenhamInterpolator;
begin
  if Visible(x, y, r) then
    if r <> 0 then
      begin
        r8 := r * 4 div 5;
        Delta := PointInteger(0, -r);

        Ei.Initialize(r * 3 div 5, r + r8);

        repeat
          inc(Delta.x, Ei.deltax);
          inc(Delta.y, Ei.deltay);

          RenderBase.BlendPixel(x - Delta.y, y + Delta.x, @FLineColor,
            CAggCoverFull);
          RenderBase.BlendPixel(x - Delta.y, y - Delta.x, @FLineColor,
            CAggCoverFull);

          if (Ei.deltay <> 0) and (Delta.x <> 0) then
              RenderBase.BlendVerticalLine(x - Delta.y, y - Delta.x + 1,
              y + Delta.x - 1, @FFillColor, CAggCoverFull);

          Ei.IncOperator;
        until Delta.y >= r8;

        RenderBase.BlendVerticalLine(x - Delta.y, y - Delta.x, y + Delta.x,
          @FLineColor, CAggCoverFull);
      end
    else
        RenderBase.BlendPixel(x, y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.SemiEllipseUp(x, y, r: Integer);
var
  r8: Integer;
  Delta: TPointInteger;
  Ei: TAggEllipseBresenhamInterpolator;
begin
  if Visible(x, y, r) then
    if r <> 0 then
      begin
        r8 := r * 4 div 5;
        Delta := PointInteger(0, -r);

        Ei.Initialize(r * 3 div 5, r + r8);

        repeat
          inc(Delta.x, Ei.deltax);
          inc(Delta.y, Ei.deltay);

          RenderBase.BlendPixel(x + Delta.x, y - Delta.y, @FLineColor,
            CAggCoverFull);
          RenderBase.BlendPixel(x - Delta.x, y - Delta.y, @FLineColor,
            CAggCoverFull);

          if (Ei.deltay <> 0) and (Delta.x <> 0) then
              RenderBase.BlendHorizontalLine(x - Delta.x + 1, y - Delta.y,
              x + Delta.x - 1, @FFillColor, CAggCoverFull);

          Ei.IncOperator;
        until Delta.y >= r8;

        RenderBase.BlendHorizontalLine(x - Delta.x, y - Delta.y - 1, x + Delta.x,
          @FLineColor, CAggCoverFull);
      end
    else
        RenderBase.BlendPixel(x, y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.SemiEllipseDown;
var
  r8: Integer;
  Delta: TPointInteger;
  Ei: TAggEllipseBresenhamInterpolator;
begin
  if Visible(x, y, r) then
    if r <> 0 then
      begin
        r8 := r * 4 div 5;
        Delta := PointInteger(0, -r);

        Ei.Initialize(r * 3 div 5, r + r8);

        repeat
          inc(Delta.x, Ei.deltax);
          inc(Delta.y, Ei.deltay);

          RenderBase.BlendPixel(x + Delta.x, y + Delta.y, @FLineColor,
            CAggCoverFull);
          RenderBase.BlendPixel(x - Delta.x, y + Delta.y, @FLineColor,
            CAggCoverFull);

          if (Ei.deltay <> 0) and (Delta.x <> 0) then
              RenderBase.BlendHorizontalLine(x - Delta.x + 1, y + Delta.y,
              x + Delta.x - 1, @FFillColor, CAggCoverFull);

          Ei.IncOperator;
        until Delta.y >= r8;

        RenderBase.BlendHorizontalLine(x - Delta.x, y + Delta.y + 1, x + Delta.x,
          @FLineColor, CAggCoverFull);
      end
    else
        RenderBase.BlendPixel(x, y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.TriangleLeft(x, y, r: Integer);
var
  Delta: TPointInteger;
  Flip, R6: Integer;
begin
  if Visible(x, y, r) then
    if r <> 0 then
      begin
        Delta := PointInteger(0, -r);
        Flip := 0;
        R6 := r * 3 div 5;

        repeat
          RenderBase.BlendPixel(x + Delta.y, y - Delta.x, @FLineColor,
            CAggCoverFull);
          RenderBase.BlendPixel(x + Delta.y, y + Delta.x, @FLineColor,
            CAggCoverFull);

          if Delta.x <> 0 then
              RenderBase.BlendVerticalLine(x + Delta.y, y - Delta.x + 1,
              y + Delta.x - 1, @FFillColor, CAggCoverFull);

          inc(Delta.y);
          inc(Delta.x, Flip);

          Flip := Flip xor 1;
        until Delta.y >= R6;

        RenderBase.BlendVerticalLine(x + Delta.y, y - Delta.x, y + Delta.x,
          @FLineColor, CAggCoverFull);
      end
    else
        RenderBase.BlendPixel(x, y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.TriangleRight(x, y, r: Integer);
var
  Delta: TPointInteger;
  Flip, R6: Integer;
begin
  if Visible(x, y, r) then
    if r <> 0 then
      begin
        Delta.y := -r;
        Delta.x := 0;
        Flip := 0;
        R6 := r * 3 div 5;

        repeat
          RenderBase.BlendPixel(x - Delta.y, y - Delta.x, @FLineColor,
            CAggCoverFull);
          RenderBase.BlendPixel(x - Delta.y, y + Delta.x, @FLineColor,
            CAggCoverFull);

          if Delta.x <> 0 then
              RenderBase.BlendVerticalLine(x - Delta.y, y - Delta.x + 1,
              y + Delta.x - 1, @FFillColor, CAggCoverFull);

          inc(Delta.y);
          inc(Delta.x, Flip);

          Flip := Flip xor 1;
        until Delta.y >= R6;

        RenderBase.BlendVerticalLine(x - Delta.y, y - Delta.x, y + Delta.x,
          @FLineColor, CAggCoverFull);
      end
    else
        RenderBase.BlendPixel(x, y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.TriangleUp(x, y, r: Integer);
var
  Delta: TPointInteger;
  Flip, R6: Integer;
begin
  if Visible(x, y, r) then
    if r <> 0 then
      begin
        Delta.y := -r;
        Delta.x := 0;
        Flip := 0;
        R6 := r * 3 div 5;

        repeat
          RenderBase.BlendPixel(x - Delta.x, y - Delta.y, @FLineColor,
            CAggCoverFull);
          RenderBase.BlendPixel(x + Delta.x, y - Delta.y, @FLineColor,
            CAggCoverFull);

          if Delta.x <> 0 then
              RenderBase.BlendHorizontalLine(x - Delta.x + 1, y - Delta.y,
              x + Delta.x - 1, @FFillColor, CAggCoverFull);

          inc(Delta.y);
          inc(Delta.x, Flip);

          Flip := Flip xor 1;
        until Delta.y >= R6;

        RenderBase.BlendHorizontalLine(x - Delta.x, y - Delta.y, x + Delta.x,
          @FLineColor, CAggCoverFull);
      end
    else
        RenderBase.BlendPixel(x, y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.TriangleDown(x, y, r: Integer);
var
  Delta: TPointInteger;
  Flip, R6: Integer;
begin
  if Visible(x, y, r) then
    if r <> 0 then
      begin
        Delta.y := -r;
        Delta.x := 0;
        Flip := 0;
        R6 := r * 3 div 5;

        repeat
          RenderBase.BlendPixel(x - Delta.x, y + Delta.y, @FLineColor,
            CAggCoverFull);
          RenderBase.BlendPixel(x + Delta.x, y + Delta.y, @FLineColor,
            CAggCoverFull);

          if Delta.x <> 0 then
              RenderBase.BlendHorizontalLine(x - Delta.x + 1, y + Delta.y,
              x + Delta.x - 1, @FFillColor, CAggCoverFull);

          inc(Delta.y);
          inc(Delta.x, Flip);

          Flip := Flip xor 1;
        until Delta.y >= R6;

        RenderBase.BlendHorizontalLine(x - Delta.x, y + Delta.y, x + Delta.x,
          @FLineColor, CAggCoverFull);
      end
    else
        RenderBase.BlendPixel(x, y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.FourRays(x, y, r: Integer);
var
  Delta: TPointInteger;
  Flip, r3: Integer;
begin
  if Visible(x, y, r) then
    if r <> 0 then
      begin
        Delta.y := -r;
        Delta.x := 0;
        Flip := 0;
        r3 := -(r div 3);

        repeat
          RenderBase.BlendPixel(x - Delta.x, y + Delta.y, @FLineColor,
            CAggCoverFull);
          RenderBase.BlendPixel(x + Delta.x, y + Delta.y, @FLineColor,
            CAggCoverFull);
          RenderBase.BlendPixel(x - Delta.x, y - Delta.y, @FLineColor,
            CAggCoverFull);
          RenderBase.BlendPixel(x + Delta.x, y - Delta.y, @FLineColor,
            CAggCoverFull);
          RenderBase.BlendPixel(x + Delta.y, y - Delta.x, @FLineColor,
            CAggCoverFull);
          RenderBase.BlendPixel(x + Delta.y, y + Delta.x, @FLineColor,
            CAggCoverFull);
          RenderBase.BlendPixel(x - Delta.y, y - Delta.x, @FLineColor,
            CAggCoverFull);
          RenderBase.BlendPixel(x - Delta.y, y + Delta.x, @FLineColor,
            CAggCoverFull);

          if Delta.x <> 0 then
            begin
              RenderBase.BlendHorizontalLine(x - Delta.x + 1, y + Delta.y,
                x + Delta.x - 1, @FFillColor, CAggCoverFull);
              RenderBase.BlendHorizontalLine(x - Delta.x + 1, y - Delta.y,
                x + Delta.x - 1, @FFillColor, CAggCoverFull);
              RenderBase.BlendVerticalLine(x + Delta.y, y - Delta.x + 1,
                y + Delta.x - 1, @FFillColor, CAggCoverFull);
              RenderBase.BlendVerticalLine(x - Delta.y, y - Delta.x + 1,
                y + Delta.x - 1, @FFillColor, CAggCoverFull);
            end;

          inc(Delta.y);
          inc(Delta.x, Flip);

          Flip := Flip xor 1;
        until Delta.y > r3;

        SolidRectangle(x + r3 + 1, y + r3 + 1, x - r3 - 1, y - r3 - 1);
      end
    else
        RenderBase.BlendPixel(x, y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.Cross(x, y, r: Integer);
begin
  if Visible(x, y, r) then
    if r <> 0 then
      begin
        RenderBase.BlendVerticalLine(x, y - r, y + r, @FLineColor, CAggCoverFull);
        RenderBase.BlendHorizontalLine(x - r, y, x + r, @FLineColor,
          CAggCoverFull);
      end
    else
        RenderBase.BlendPixel(x, y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.Xing(x, y, r: Integer);
var
  dy: Integer;
begin
  if Visible(x, y, r) then
    if r <> 0 then
      begin
        dy := -r * 7 div 10;

        repeat
          RenderBase.BlendPixel(x + dy, y + dy, @FLineColor, CAggCoverFull);
          RenderBase.BlendPixel(x - dy, y + dy, @FLineColor, CAggCoverFull);
          RenderBase.BlendPixel(x + dy, y - dy, @FLineColor, CAggCoverFull);
          RenderBase.BlendPixel(x - dy, y - dy, @FLineColor, CAggCoverFull);

          inc(dy);
        until dy >= 0;
      end
    else
        RenderBase.BlendPixel(x, y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.Dash(x, y, r: Integer);
begin
  if Visible(x, y, r) then
    if r <> 0 then
        RenderBase.BlendHorizontalLine(x - r, y, x + r, @FLineColor, CAggCoverFull)
    else
        RenderBase.BlendPixel(x, y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.dot(x, y, r: Integer);
begin
  if Visible(x, y, r) then
    if r <> 0 then
        SolidEllipse(x, y, r, r)
    else
        RenderBase.BlendPixel(x, y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.Pixel(x, y, r: Integer);
begin
  RenderBase.BlendPixel(x, y, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererMarkers.Marker(x, y, r: Integer; MarkerType: TAggMarker);
begin
  case MarkerType of
    meSquare:
      Square(x, y, r);
    meDiamond:
      Diamond(x, y, r);
    meCircle:
      Circle(x, y, r);
    meCrossedCircle:
      CrossedCircle(x, y, r);
    meSemiEllipseLeft:
      SemiEllipseLeft(x, y, r);
    meSemiEllipseRight:
      SemiEllipseRight(x, y, r);
    meSemiEllipseUp:
      SemiEllipseUp(x, y, r);
    meSemiEllipseDown:
      SemiEllipseDown(x, y, r);
    meTriangleLeft:
      TriangleLeft(x, y, r);
    meTriangleRight:
      TriangleRight(x, y, r);
    meTriangleUp:
      TriangleUp(x, y, r);
    meTriangleDown:
      TriangleDown(x, y, r);
    meFourRays:
      FourRays(x, y, r);
    meCross:
      Cross(x, y, r);
    meX:
      Xing(x, y, r);
    meDash:
      Dash(x, y, r);
    meDot:
      dot(x, y, r);
    mePixel:
      Pixel(x, y, r);
  end;
end;

procedure TAggRendererMarkers.Markers(n: Integer; x, y: PInteger; r: Integer;
  MarkerType: TAggMarker);
begin
  if n <= 0 then
      Exit;

  if r = 0 then
    begin
      repeat
        RenderBase.BlendPixel(x^, y^, @FFillColor, CAggCoverFull);

        inc(x);
        inc(y);
        dec(n);
      until n = 0;

      Exit;
    end;

  case MarkerType of
    meSquare:
      repeat
        Square(x^, y^, r);

        inc(x);
        inc(y);
        dec(n);
      until n = 0;

    meDiamond:
      repeat
        Diamond(x^, y^, r);

        inc(x);
        inc(y);
        dec(n);
      until n = 0;

    meCircle:
      repeat
        Circle(x^, y^, r);

        inc(x);
        inc(y);
        dec(n);
      until n = 0;

    meCrossedCircle:
      repeat
        CrossedCircle(x^, y^, r);

        inc(x);
        inc(y);
        dec(n);
      until n = 0;

    meSemiEllipseLeft:
      repeat
        SemiEllipseLeft(x^, y^, r);

        inc(x);
        inc(y);
        dec(n);
      until n = 0;

    meSemiEllipseRight:
      repeat
        SemiEllipseRight(x^, y^, r);

        inc(x);
        inc(y);
        dec(n);
      until n = 0;

    meSemiEllipseUp:
      repeat
        SemiEllipseUp(x^, y^, r);

        inc(x);
        inc(y);
        dec(n);
      until n = 0;

    meSemiEllipseDown:
      repeat
        SemiEllipseDown(x^, y^, r);

        inc(x);
        inc(y);
        dec(n);
      until n = 0;

    meTriangleLeft:
      repeat
        TriangleLeft(x^, y^, r);

        inc(x);
        inc(y);
        dec(n);
      until n = 0;

    meTriangleRight:
      repeat
        TriangleRight(x^, y^, r);

        inc(x);
        inc(y);
        dec(n);
      until n = 0;

    meTriangleUp:
      repeat
        TriangleUp(x^, y^, r);

        inc(x);
        inc(y);
        dec(n);
      until n = 0;

    meTriangleDown:
      repeat
        TriangleDown(x^, y^, r);

        inc(x);
        inc(y);
        dec(n);
      until n = 0;

    meFourRays:
      repeat
        FourRays(x^, y^, r);

        inc(x);
        inc(y);
        dec(n);
      until n = 0;

    meCross:
      repeat
        Cross(x^, y^, r);

        inc(x);
        inc(y);
        dec(n);
      until n = 0;

    meX:
      repeat
        Xing(x^, y^, r);

        inc(x);
        inc(y);
        dec(n);
      until n = 0;

    meDash:
      repeat
        Dash(x^, y^, r);

        inc(x);
        inc(y);
        dec(n);
      until n = 0;

    meDot:
      repeat
        dot(x^, y^, r);

        inc(x);
        inc(y);
        dec(n);
      until n = 0;

    mePixel:
      repeat
        Pixel(x^, y^, r);

        inc(x);
        inc(y);
        dec(n);
      until n = 0;
    end;
  end;

  procedure TAggRendererMarkers.Markers(n: Integer; x, y, r: PInteger;
    MarkerType: TAggMarker);
  begin
    if n <= 0 then
        Exit;

    case MarkerType of
      meSquare:
        repeat
          Square(x^, y^, r^);

          inc(x);
          inc(y);
          inc(r);
          dec(n);
        until n = 0;

      meDiamond:
        repeat
          Diamond(x^, y^, r^);

          inc(x);
          inc(y);
          inc(r);
          dec(n);
        until n = 0;

      meCircle:
        repeat
          Circle(x^, y^, r^);

          inc(x);
          inc(y);
          inc(r);
          dec(n);
        until n = 0;

      meCrossedCircle:
        repeat
          CrossedCircle(x^, y^, r^);

          inc(x);
          inc(y);
          inc(r);
          dec(n);
        until n = 0;

      meSemiEllipseLeft:
        repeat
          SemiEllipseLeft(x^, y^, r^);

          inc(x);
          inc(y);
          inc(r);
          dec(n);
        until n = 0;

      meSemiEllipseRight:
        repeat
          SemiEllipseRight(x^, y^, r^);

          inc(x);
          inc(y);
          inc(r);
          dec(n);
        until n = 0;

      meSemiEllipseUp:
        repeat
          SemiEllipseUp(x^, y^, r^);

          inc(x);
          inc(y);
          inc(r);
          dec(n);
        until n = 0;

      meSemiEllipseDown:
        repeat
          SemiEllipseDown(x^, y^, r^);

          inc(x);
          inc(y);
          inc(r);
          dec(n);
        until n = 0;

      meTriangleLeft:
        repeat
          TriangleLeft(x^, y^, r^);

          inc(x);
          inc(y);
          inc(r);
          dec(n);
        until n = 0;

      meTriangleRight:
        repeat
          TriangleRight(x^, y^, r^);

          inc(x);
          inc(y);
          inc(r);
          dec(n);
        until n = 0;

      meTriangleUp:
        repeat
          TriangleUp(x^, y^, r^);

          inc(x);
          inc(y);
          inc(r);
          dec(n);
        until n = 0;

      meTriangleDown:
        repeat
          TriangleDown(x^, y^, r^);

          inc(x);
          inc(y);
          inc(r);
          dec(n);
        until n = 0;

      meFourRays:
        repeat
          FourRays(x^, y^, r^);

          inc(x);
          inc(y);
          inc(r);
          dec(n);
        until n = 0;

      meCross:
        repeat
          Cross(x^, y^, r^);

          inc(x);
          inc(y);
          inc(r);
          dec(n);
        until n = 0;

      meX:
        repeat
          Xing(x^, y^, r^);

          inc(x);
          inc(y);
          inc(r);
          dec(n);
        until n = 0;

      meDash:
        repeat
          Dash(x^, y^, r^);

          inc(x);
          inc(y);
          inc(r);
          dec(n);
        until n = 0;

      meDot:
        repeat
          dot(x^, y^, r^);

          inc(x);
          inc(y);
          inc(r);
          dec(n);
        until n = 0;

      mePixel:
        repeat
          Pixel(x^, y^, r^);

          inc(x);
          inc(y);
          inc(r);
          dec(n);
        until n = 0;
      end;
    end;

    procedure TAggRendererMarkers.Markers(n: Integer; x, y, r: PInteger; Fc: PAggColor;
      MarkerType: TAggMarker);
    begin
      if n <= 0 then
          Exit;

      case MarkerType of
        meSquare:
          repeat
            FillColor := Fc^;

            Square(x^, y^, r^);

            inc(x);
            inc(y);
            inc(r);
            inc(PtrComp(Fc), SizeOf(TAggColor));
            dec(n);
          until n = 0;

        meDiamond:
          repeat
            FillColor := Fc^;

            Diamond(x^, y^, r^);

            inc(x);
            inc(y);
            inc(r);
            inc(PtrComp(Fc), SizeOf(TAggColor));
            dec(n);
          until n = 0;

        meCircle:
          repeat
            FillColor := Fc^;

            Circle(x^, y^, r^);

            inc(x);
            inc(y);
            inc(r);
            inc(PtrComp(Fc), SizeOf(TAggColor));
            dec(n);
          until n = 0;

        meCrossedCircle:
          repeat
            FillColor := Fc^;

            CrossedCircle(x^, y^, r^);

            inc(x);
            inc(y);
            inc(r);
            inc(PtrComp(Fc), SizeOf(TAggColor));
            dec(n);
          until n = 0;

        meSemiEllipseLeft:
          repeat
            FillColor := Fc^;

            SemiEllipseLeft(x^, y^, r^);

            inc(x);
            inc(y);
            inc(r);
            inc(PtrComp(Fc), SizeOf(TAggColor));
            dec(n);
          until n = 0;

        meSemiEllipseRight:
          repeat
            FillColor := Fc^;

            SemiEllipseRight(x^, y^, r^);

            inc(x);
            inc(y);
            inc(r);
            inc(PtrComp(Fc), SizeOf(TAggColor));
            dec(n);
          until n = 0;

        meSemiEllipseUp:
          repeat
            FillColor := Fc^;

            SemiEllipseUp(x^, y^, r^);

            inc(x);
            inc(y);
            inc(r);
            inc(PtrComp(Fc), SizeOf(TAggColor));
            dec(n);
          until n = 0;

        meSemiEllipseDown:
          repeat
            FillColor := Fc^;

            SemiEllipseDown(x^, y^, r^);

            inc(x);
            inc(y);
            inc(r);
            inc(PtrComp(Fc), SizeOf(TAggColor));
            dec(n);
          until n = 0;

        meTriangleLeft:
          repeat
            FillColor := Fc^;

            TriangleLeft(x^, y^, r^);

            inc(x);
            inc(y);
            inc(r);
            inc(PtrComp(Fc), SizeOf(TAggColor));
            dec(n);
          until n = 0;

        meTriangleRight:
          repeat
            FillColor := Fc^;

            TriangleRight(x^, y^, r^);

            inc(x);
            inc(y);
            inc(r);
            inc(PtrComp(Fc), SizeOf(TAggColor));
            dec(n);
          until n = 0;

        meTriangleUp:
          repeat
            FillColor := Fc^;

            TriangleUp(x^, y^, r^);

            inc(x);
            inc(y);
            inc(r);
            inc(PtrComp(Fc), SizeOf(TAggColor));
            dec(n);
          until n = 0;

        meTriangleDown:
          repeat
            FillColor := Fc^;

            TriangleDown(x^, y^, r^);

            inc(x);
            inc(y);
            inc(r);
            inc(PtrComp(Fc), SizeOf(TAggColor));
            dec(n);
          until n = 0;

        meFourRays:
          repeat
            FillColor := Fc^;

            FourRays(x^, y^, r^);

            inc(x);
            inc(y);
            inc(r);
            inc(PtrComp(Fc), SizeOf(TAggColor));
            dec(n);
          until n = 0;

        meCross:
          repeat
            FillColor := Fc^;

            Cross(x^, y^, r^);

            inc(x);
            inc(y);
            inc(r);
            inc(PtrComp(Fc), SizeOf(TAggColor));
            dec(n);
          until n = 0;

        meX:
          repeat
            FillColor := Fc^;

            Xing(x^, y^, r^);

            inc(x);
            inc(y);
            inc(r);
            inc(PtrComp(Fc), SizeOf(TAggColor));
            dec(n);
          until n = 0;

        meDash:
          repeat
            FillColor := Fc^;

            Dash(x^, y^, r^);

            inc(x);
            inc(y);
            inc(r);
            inc(PtrComp(Fc), SizeOf(TAggColor));
            dec(n);
          until n = 0;

        meDot:
          repeat
            FillColor := Fc^;

            dot(x^, y^, r^);

            inc(x);
            inc(y);
            inc(r);
            inc(PtrComp(Fc), SizeOf(TAggColor));
            dec(n);
          until n = 0;

        mePixel:
          repeat
            FillColor := Fc^;

            Pixel(x^, y^, r^);

            inc(x);
            inc(y);
            inc(r);
            inc(PtrComp(Fc), SizeOf(TAggColor));
            dec(n);
          until n = 0;
        end;
      end;

      procedure TAggRendererMarkers.Markers(n: Integer; x, y, r: PInteger;
        Fc, LC: PAggColor; MarkerType: TAggMarker);
      begin
        if n <= 0 then
            Exit;

        case MarkerType of
          meSquare:
            repeat
              FillColor := Fc^;
              LineColor := LC^;

              Square(x^, y^, r^);

              inc(x);
              inc(y);
              inc(r);
              inc(PtrComp(Fc), SizeOf(TAggColor));
              inc(PtrComp(LC), SizeOf(TAggColor));
              dec(n);
            until n = 0;

          meDiamond:
            repeat
              FillColor := Fc^;
              LineColor := LC^;

              Diamond(x^, y^, r^);

              inc(x);
              inc(y);
              inc(r);
              inc(PtrComp(Fc), SizeOf(TAggColor));
              inc(PtrComp(LC), SizeOf(TAggColor));
              dec(n);
            until n = 0;

          meCircle:
            repeat
              FillColor := Fc^;
              LineColor := LC^;

              Circle(x^, y^, r^);

              inc(x);
              inc(y);
              inc(r);
              inc(PtrComp(Fc), SizeOf(TAggColor));
              inc(PtrComp(LC), SizeOf(TAggColor));
              dec(n);
            until n = 0;

          meCrossedCircle:
            repeat
              FillColor := Fc^;
              LineColor := LC^;

              CrossedCircle(x^, y^, r^);

              inc(x);
              inc(y);
              inc(r);
              inc(PtrComp(Fc), SizeOf(TAggColor));
              inc(PtrComp(LC), SizeOf(TAggColor));
              dec(n);
            until n = 0;

          meSemiEllipseLeft:
            repeat
              FillColor := Fc^;
              LineColor := LC^;

              SemiEllipseLeft(x^, y^, r^);

              inc(x);
              inc(y);
              inc(r);
              inc(PtrComp(Fc), SizeOf(TAggColor));
              inc(PtrComp(LC), SizeOf(TAggColor));
              dec(n);
            until n = 0;

          meSemiEllipseRight:
            repeat
              FillColor := Fc^;
              LineColor := LC^;

              SemiEllipseRight(x^, y^, r^);

              inc(x);
              inc(y);
              inc(r);
              inc(PtrComp(Fc), SizeOf(TAggColor));
              inc(PtrComp(LC), SizeOf(TAggColor));
              dec(n);
            until n = 0;

          meSemiEllipseUp:
            repeat
              FillColor := Fc^;
              LineColor := LC^;

              SemiEllipseUp(x^, y^, r^);

              inc(x);
              inc(y);
              inc(r);
              inc(PtrComp(Fc), SizeOf(TAggColor));
              inc(PtrComp(LC), SizeOf(TAggColor));
              dec(n);
            until n = 0;

          meSemiEllipseDown:
            repeat
              FillColor := Fc^;
              LineColor := LC^;

              SemiEllipseDown(x^, y^, r^);

              inc(x);
              inc(y);
              inc(r);
              inc(PtrComp(Fc), SizeOf(TAggColor));
              inc(PtrComp(LC), SizeOf(TAggColor));
              dec(n);
            until n = 0;

          meTriangleLeft:
            repeat
              FillColor := Fc^;
              LineColor := LC^;

              TriangleLeft(x^, y^, r^);

              inc(x);
              inc(y);
              inc(r);
              inc(PtrComp(Fc), SizeOf(TAggColor));
              inc(PtrComp(LC), SizeOf(TAggColor));
              dec(n);
            until n = 0;

          meTriangleRight:
            repeat
              FillColor := Fc^;
              LineColor := LC^;

              TriangleRight(x^, y^, r^);

              inc(x);
              inc(y);
              inc(r);
              inc(PtrComp(Fc), SizeOf(TAggColor));
              inc(PtrComp(LC), SizeOf(TAggColor));
              dec(n);
            until n = 0;

          meTriangleUp:
            repeat
              FillColor := Fc^;
              LineColor := LC^;

              TriangleUp(x^, y^, r^);

              inc(x);
              inc(y);
              inc(r);
              inc(PtrComp(Fc), SizeOf(TAggColor));
              inc(PtrComp(LC), SizeOf(TAggColor));
              dec(n);
            until n = 0;

          meTriangleDown:
            repeat
              FillColor := Fc^;
              LineColor := LC^;

              TriangleDown(x^, y^, r^);

              inc(x);
              inc(y);
              inc(r);
              inc(PtrComp(Fc), SizeOf(TAggColor));
              inc(PtrComp(LC), SizeOf(TAggColor));
              dec(n);
            until n = 0;

          meFourRays:
            repeat
              FillColor := Fc^;
              LineColor := LC^;

              FourRays(x^, y^, r^);

              inc(x);
              inc(y);
              inc(r);
              inc(PtrComp(Fc), SizeOf(TAggColor));
              inc(PtrComp(LC), SizeOf(TAggColor));
              dec(n);
            until n = 0;

          meCross:
            repeat
              FillColor := Fc^;
              LineColor := LC^;

              Cross(x^, y^, r^);

              inc(x);
              inc(y);
              inc(r);
              inc(PtrComp(Fc), SizeOf(TAggColor));
              inc(PtrComp(LC), SizeOf(TAggColor));
              dec(n);
            until n = 0;

          meX:
            repeat
              FillColor := Fc^;
              LineColor := LC^;

              Xing(x^, y^, r^);

              inc(x);
              inc(y);
              inc(r);
              inc(PtrComp(Fc), SizeOf(TAggColor));
              inc(PtrComp(LC), SizeOf(TAggColor));
              dec(n);
            until n = 0;

          meDash:
            repeat
              FillColor := Fc^;
              LineColor := LC^;

              Dash(x^, y^, r^);

              inc(x);
              inc(y);
              inc(r);
              inc(PtrComp(Fc), SizeOf(TAggColor));
              inc(PtrComp(LC), SizeOf(TAggColor));
              dec(n);
            until n = 0;

          meDot:
            repeat
              FillColor := Fc^;
              LineColor := LC^;

              dot(x^, y^, r^);

              inc(x);
              inc(y);
              inc(r);
              inc(PtrComp(Fc), SizeOf(TAggColor));
              inc(PtrComp(LC), SizeOf(TAggColor));
              dec(n);
            until n = 0;

          mePixel:
            repeat
              FillColor := Fc^;
              LineColor := LC^;

              Pixel(x^, y^, r^);

              inc(x);
              inc(y);
              inc(r);
              inc(PtrComp(Fc), SizeOf(TAggColor));
              inc(PtrComp(LC), SizeOf(TAggColor));
              dec(n);
            until n = 0;
          end;
        end;

end. 
 
 
