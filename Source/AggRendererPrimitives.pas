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
unit AggRendererPrimitives;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggRendererBase,
  AggColor32,
  AggDdaLine,
  AggEllipseBresenham;

type
  TAggRendererPrimitives = class
  private
    FRenderBase: TAggRendererBase;
    FCurrent: TPointInteger;
    procedure SetFillColor(Value: TAggColor);
    procedure SetLineColor(Value: TAggColor);
  protected
    FFillColor, FLineColor: TAggColor;
  public
    constructor Create(RendBase: TAggRendererBase);

    function Coord(c: Double): Integer;

    procedure Rectangle(x1, y1, x2, y2: Integer); overload;
    procedure Rectangle(Rect: TRectInteger); overload;
    procedure SolidRectangle(x1, y1, x2, y2: Integer); overload;
    procedure SolidRectangle(Rect: TRectInteger); overload;
    procedure OutlinedRectangle(x1, y1, x2, y2: Integer); overload;
    procedure OutlinedRectangle(Rect: TRectInteger); overload;

    procedure Ellipse(x, y, RX, RY: Integer);
    procedure SolidEllipse(x, y, RX, RY: Integer);
    procedure OutlinedEllipse(x, y, RX, RY: Integer);

    procedure Line(x1, y1, x2, y2: Integer; Last: Boolean = False); overload;
    procedure Line(Point1, Point2: TPointInteger; Last: Boolean = False); overload;

    procedure MoveTo(x, y: Integer); overload;
    procedure MoveTo(Point: TPointInteger); overload;
    procedure LineTo(x, y: Integer; Last: Boolean = False); overload;
    procedure LineTo(Point: TPointInteger; Last: Boolean = False); overload;

    property RenderBase: TAggRendererBase read FRenderBase;

    property FillColor: TAggColor read FFillColor write SetFillColor;
    property LineColor: TAggColor read FLineColor write SetLineColor;
  end;

implementation


{ TAggRendererPrimitives }

constructor TAggRendererPrimitives.Create(RendBase: TAggRendererBase);
begin
  Assert(RendBase is TAggRendererBase);

  FRenderBase := RendBase;

  FCurrent := PointInteger(0);
end;

function TAggRendererPrimitives.Coord(c: Double): Integer;
begin
  Result := Trunc(c * CAggSubpixelSize);
end;

procedure TAggRendererPrimitives.SetFillColor(Value: TAggColor);
begin
  FFillColor := Value;
end;

procedure TAggRendererPrimitives.SetLineColor(Value: TAggColor);
begin
  FLineColor := Value;
end;

procedure TAggRendererPrimitives.Rectangle(x1, y1, x2, y2: Integer);
begin
  with FRenderBase do
    begin
      BlendHorizontalLine(x1, y1, x2 - 1, @FLineColor, CAggCoverFull);
      BlendVerticalLine(x2, y1, y2 - 1, @FLineColor, CAggCoverFull);
      BlendHorizontalLine(x1 + 1, y2, x2, @FLineColor, CAggCoverFull);
      BlendVerticalLine(x1, y1 + 1, y2, @FLineColor, CAggCoverFull);
    end;
end;

procedure TAggRendererPrimitives.Rectangle(Rect: TRectInteger);
begin
  with Rect, FRenderBase do
    begin
      BlendHorizontalLine(x1, y1, x2 - 1, @FLineColor, CAggCoverFull);
      BlendVerticalLine(x2, y1, y2 - 1, @FLineColor, CAggCoverFull);
      BlendHorizontalLine(x1 + 1, y2, x2, @FLineColor, CAggCoverFull);
      BlendVerticalLine(x1, y1 + 1, y2, @FLineColor, CAggCoverFull);
    end;
end;

procedure TAggRendererPrimitives.SolidRectangle(x1, y1, x2, y2: Integer);
begin
  FRenderBase.BlendBar(x1, y1, x2, y2, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererPrimitives.SolidRectangle(Rect: TRectInteger);
begin
  FRenderBase.BlendBar(Rect.x1, Rect.y1, Rect.x2, Rect.y2, @FFillColor,
    CAggCoverFull);
end;

procedure TAggRendererPrimitives.OutlinedRectangle(x1, y1, x2, y2: Integer);
begin
  Rectangle(x1, y1, x2, y2);
  FRenderBase.BlendBar(x1 + 1, y1 + 1, x2 - 1, y2 - 1, @FFillColor,
    CAggCoverFull);
end;

procedure TAggRendererPrimitives.OutlinedRectangle(Rect: TRectInteger);
begin
  Rectangle(Rect.x1, Rect.y1, Rect.x2, Rect.y2);
  FRenderBase.BlendBar(Rect.x1 + 1, Rect.y1 + 1, Rect.x2 - 1, Rect.y2 - 1,
    @FFillColor, CAggCoverFull);
end;

procedure TAggRendererPrimitives.Ellipse(x, y, RX, RY: Integer);
var
  Ei: TAggEllipseBresenhamInterpolator;
  Delta: TPointInteger;
begin
  Ei.Initialize(RX, RY);

  Delta := PointInteger(0, -RY);

  repeat
    inc(Delta.x, Ei.deltax);
    inc(Delta.y, Ei.deltay);

    with FRenderBase do
      begin
        BlendPixel(x + Delta.x, y + Delta.y, @FLineColor, CAggCoverFull);
        BlendPixel(x + Delta.x, y - Delta.y, @FLineColor, CAggCoverFull);
        BlendPixel(x - Delta.x, y - Delta.y, @FLineColor, CAggCoverFull);
        BlendPixel(x - Delta.x, y + Delta.y, @FLineColor, CAggCoverFull);
      end;

    Ei.IncOperator;
  until Delta.y >= 0;
end;

procedure TAggRendererPrimitives.SolidEllipse(x, y, RX, RY: Integer);
var
  Ei: TAggEllipseBresenhamInterpolator;
  Delta, LastDelta: TPointInteger;
begin
  Ei.Initialize(RX, RY);

  Delta := PointInteger(0, -RY);
  LastDelta := Delta;

  repeat
    inc(Delta.x, Ei.deltax);
    inc(Delta.y, Ei.deltay);

    if Delta.y <> LastDelta.y then
      begin
        FRenderBase.BlendHorizontalLine(x - LastDelta.x, y + LastDelta.y,
          x + LastDelta.x, @FFillColor, CAggCoverFull);
        FRenderBase.BlendHorizontalLine(x - LastDelta.x, y - LastDelta.y,
          x + LastDelta.x, @FFillColor, CAggCoverFull);
      end;

    LastDelta := Delta;

    Ei.IncOperator;
  until Delta.y >= 0;

  FRenderBase.BlendHorizontalLine(x - LastDelta.x, y + LastDelta.y,
    x + LastDelta.x, @FFillColor, CAggCoverFull);
end;

procedure TAggRendererPrimitives.OutlinedEllipse(x, y, RX, RY: Integer);
var
  Ei: TAggEllipseBresenhamInterpolator;
  Delta: TPointInteger;
begin
  Ei.Initialize(RX, RY);

  Delta := PointInteger(0, -RY);
  repeat
    inc(Delta.x, Ei.deltax);
    inc(Delta.y, Ei.deltay);

    FRenderBase.BlendPixel(x + Delta.x, y + Delta.y, @FLineColor, CAggCoverFull);
    FRenderBase.BlendPixel(x + Delta.x, y - Delta.y, @FLineColor, CAggCoverFull);
    FRenderBase.BlendPixel(x - Delta.x, y - Delta.y, @FLineColor, CAggCoverFull);
    FRenderBase.BlendPixel(x - Delta.x, y + Delta.y, @FLineColor, CAggCoverFull);

    if (Ei.deltay <> 0) and (Delta.x <> 0) then
      begin
        FRenderBase.BlendHorizontalLine(x - Delta.x + 1, y + Delta.y,
          x + Delta.x - 1, @FFillColor, CAggCoverFull);
        FRenderBase.BlendHorizontalLine(x - Delta.x + 1, y - Delta.y,
          x + Delta.x - 1, @FFillColor, CAggCoverFull);
      end;

    Ei.IncOperator;
  until Delta.y >= 0;
end;

procedure TAggRendererPrimitives.Line(x1, y1, x2, y2: Integer;
  Last: Boolean = False);
var
  Li: TAggLineBresenhamInterpolator;
  Len: Cardinal;
begin
  Li.Initialize(x1, y1, x2, y2);

  Len := Li.length;

  if Len = 0 then
    begin
      if Last then
          FRenderBase.BlendPixel(Li.LineLowResolution(x1),
          Li.LineLowResolution(y1), @FLineColor, CAggCoverFull);

      Exit;
    end;

  if Last then
      inc(Len);

  if Li.IsVer then
    repeat
      FRenderBase.BlendPixel(Li.x2, Li.y1, @FLineColor, CAggCoverFull);

      Li.Vstep;

      dec(Len);
    until Len = 0
  else
    repeat
      FRenderBase.BlendPixel(Li.x1, Li.y2, @FLineColor, CAggCoverFull);

      Li.HStep;

      dec(Len);
    until Len = 0;
end;

procedure TAggRendererPrimitives.Line(Point1, Point2: TPointInteger; Last: Boolean);
var
  Li: TAggLineBresenhamInterpolator;
  Len: Cardinal;
begin
  Li.Initialize(Point1, Point2);

  Len := Li.length;

  if Len = 0 then
    begin
      if Last then
          FRenderBase.BlendPixel(Li.LineLowResolution(Point1.x),
          Li.LineLowResolution(Point1.y), @FLineColor, CAggCoverFull);

      Exit;
    end;

  if Last then
      inc(Len);

  if Li.IsVer then
    repeat
      FRenderBase.BlendPixel(Li.x2, Li.y1, @FLineColor, CAggCoverFull);

      Li.Vstep;

      dec(Len);
    until Len = 0
  else
    repeat
      FRenderBase.BlendPixel(Li.x1, Li.y2, @FLineColor, CAggCoverFull);

      Li.HStep;

      dec(Len);
    until Len = 0;
end;

procedure TAggRendererPrimitives.MoveTo(x, y: Integer);
begin
  FCurrent := PointInteger(x, y);
end;

procedure TAggRendererPrimitives.MoveTo(Point: TPointInteger);
begin
  FCurrent := Point;
end;

procedure TAggRendererPrimitives.LineTo(x, y: Integer; Last: Boolean = False);
begin
  Line(FCurrent.x, FCurrent.y, x, y, Last);
  FCurrent := PointInteger(x, y);
end;

procedure TAggRendererPrimitives.LineTo(Point: TPointInteger; Last: Boolean);
begin
  Line(FCurrent, Point, Last);
  FCurrent := Point;
end;

end. 
 
 
