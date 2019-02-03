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
unit AggSpanGouraudRgba;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggColor32,
  AggDdaLine,
  AggSpanGouraud,
  AggSpanAllocator,
  AggMath;

const
  CAggSubpixelShift = 4;
  CAggSubpixelSize  = 1 shl CAggSubpixelShift;

type
  PAggRgbaCalc = ^TAggRgbaCalc;

  TAggRgbaCalc = record
  private
    f1, FDelta: TPointDouble;

    FRed1, FGreen1, FBlue1, FAlpha1: Integer;
    FDeltaRed, FDeltaGreen, FDeltaBlue, FDeltaAlpha: Integer;
    FRed, FGreen, FBlue, FAlpha, fx: Integer;
  public
    procedure Init(c1, c2: PAggCoordType);
    procedure Calc(y: Double);
  end;

  TAggSpanGouraudRgba = class(TAggSpanGouraud)
  private
    FSwap: Boolean;
    FY2: Integer;

    FRgba1, FRgba2, FRgba3: TAggRgbaCalc;
  public
    constructor Create(Alloc: TAggSpanAllocator); overload;
    constructor Create(Alloc: TAggSpanAllocator; c1, c2, c3: PAggColor; x1, y1, x2, y2, x3, y3, d: Double); overload;
    constructor Create(c1, c2, c3: PAggColor; x1, y1, x2, y2, x3, y3: Double; d: Double = 0); overload;

    procedure Prepare; overload;
    procedure Prepare(MaxSpanLength: Cardinal); overload; override;
    function Generate(x, y: Integer; Len: Cardinal): PAggColor; overload; override;
    procedure Generate(Span: PAggColor; x, y: Integer; Len: Cardinal); overload;
  end;

implementation


{ TAggRgbaCalc }

procedure TAggRgbaCalc.Init(c1, c2: PAggCoordType);
var
  deltay: Double;
begin
  f1.x := c1.x - 0.5;
  f1.y := c1.y - 0.5;
  FDelta.x := c2.x - c1.x;

  deltay := c2.y - c1.y;

  if deltay < 1E-5 then
      FDelta.y := 1E5
  else
      FDelta.y := 1.0 / deltay;

  FRed1 := c1.COLOR.Rgba8.r;
  FGreen1 := c1.COLOR.Rgba8.g;
  FBlue1 := c1.COLOR.Rgba8.b;
  FAlpha1 := c1.COLOR.Rgba8.a;
  FDeltaRed := c2.COLOR.Rgba8.r - FRed1;
  FDeltaGreen := c2.COLOR.Rgba8.g - FGreen1;
  FDeltaBlue := c2.COLOR.Rgba8.b - FBlue1;
  FDeltaAlpha := c2.COLOR.Rgba8.a - FAlpha1;
end;

procedure TAggRgbaCalc.Calc(y: Double);
var
  k: Double;
begin
  k := (y - f1.y) * FDelta.y;

  if k < 0.0 then
    begin
      FRed := FRed1;
      FGreen := FGreen1;
      FBlue := FBlue1;
      FAlpha := FAlpha1;
      fx := IntegerRound(f1.x * CAggSubpixelSize);
      Exit;
    end;

  if k > 1.0 then
      k := 1.0;

  FRed := FRed1 + IntegerRound(FDeltaRed * k);
  FGreen := FGreen1 + IntegerRound(FDeltaGreen * k);
  FBlue := FBlue1 + IntegerRound(FDeltaBlue * k);
  FAlpha := FAlpha1 + IntegerRound(FDeltaAlpha * k);
  fx := IntegerRound((f1.x + FDelta.x * k) * CAggSubpixelSize);
end;

{ TAggSpanGouraudRgba }

constructor TAggSpanGouraudRgba.Create(Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);
end;

constructor TAggSpanGouraudRgba.Create(Alloc: TAggSpanAllocator;
  c1, c2, c3: PAggColor; x1, y1, x2, y2, x3, y3, d: Double);
begin
  inherited Create(Alloc, c1, c2, c3, x1, y1, x2, y2, x3, y3, d);
end;

constructor TAggSpanGouraudRgba.Create(c1, c2, c3: PAggColor;
  x1, y1, x2, y2, x3, y3: Double; d: Double = 0);
begin
  inherited Create(nil, c1, c2, c3, x1, y1, x2, y2, x3, y3, d);
end;

procedure TAggSpanGouraudRgba.Prepare(MaxSpanLength: Cardinal);
var
  Coord: array [0 .. 2] of TAggCoordType;
begin
  inherited Prepare(MaxSpanLength);

  ArrangeVertices(@Coord);

  FY2 := Trunc(Coord[1].y);

  FSwap := CalculatePointLocation(Coord[0].x, Coord[0].y,
    Coord[2].x, Coord[2].y, Coord[1].x, Coord[1].y) < 0.0;

  FRgba1.Init(@Coord[0], @Coord[2]);
  FRgba2.Init(@Coord[0], @Coord[1]);
  FRgba3.Init(@Coord[1], @Coord[2]);
end;

procedure TAggSpanGouraudRgba.Prepare;
var
  Coord: array [0 .. 2] of TAggCoordType;
begin
  ArrangeVertices(@Coord);

  FY2 := Integer(Trunc(Coord[1].y));

  FSwap := CrossProduct(Coord[0].x, Coord[0].y, Coord[2].x, Coord[2].y,
    Coord[1].x, Coord[1].y) < 0.0;

  FRgba1.Init(@Coord[0], @Coord[2]);
  FRgba2.Init(@Coord[0], @Coord[1]);
  FRgba3.Init(@Coord[1], @Coord[2]);
end;

function TAggSpanGouraudRgba.Generate(x, y: Integer; Len: Cardinal): PAggColor;
const
  Lim = AggColor32.CAggBaseMask;
var
  PC1, PC2, t: PAggRgbaCalc;
  Nlen, Start, vr, Vg, VB, VA: Integer;
  r, g, b, a: TAggDdaLineInterpolator;
  Span: PAggColor;
begin
  FRgba1.Calc(y); // (FRgba1.FDelta.Y > 2) ? FRgba1.F1.Y : y);

  PC1 := @FRgba1;
  PC2 := @FRgba2;

  if y <= FY2 then
    // Bottom part of the triangle (first subtriangle)
      FRgba2.Calc(y + FRgba2.FDelta.y)
  else
    begin
      // Upper part (second subtriangle)
      FRgba3.Calc(y - FRgba3.FDelta.y);

      PC2 := @FRgba3;
    end;

  if FSwap then
    begin
      // It means that the triangle is oriented clockwise,
      // so that we need to swap the controlling structures
      t := PC2;
      PC2 := PC1;
      PC1 := t;
    end;

  // Get the horizontal length with subpixel accuracy
  // and protect it from division by zero
  Nlen := Abs(PC2.fx - PC1.fx);

  if Nlen <= 0 then
      Nlen := 1;

  r.Initialize(PC1.FRed, PC2.FRed, Nlen, 14);
  g.Initialize(PC1.FGreen, PC2.FGreen, Nlen, 14);
  b.Initialize(PC1.FBlue, PC2.FBlue, Nlen, 14);
  a.Initialize(PC1.FAlpha, PC2.FAlpha, Nlen, 14);

  // Calculate the starting point of the Gradient with subpixel
  // accuracy and correct (roll back) the interpolators.
  // This operation will also clip the beginning of the Span
  // if necessary.
  Start := PC1.fx - (x shl CAggSubpixelShift);

  r.DecOperator(Start);
  g.DecOperator(Start);
  b.DecOperator(Start);
  a.DecOperator(Start);

  inc(Nlen, Start);

  Span := Allocator.Span;

  // Beginning part of the Span. Since we rolled back the
  // interpolators, the color values may have overflow.
  // So that, we render the beginning part with checking
  // for overflow. It lasts until "start" is positive;
  // typically it's 1-2 pixels, but may be more in some cases.
  while (Len <> 0) and (Start > 0) do
    begin
      Span.Rgba8.r := Int8u(EnsureRange(r.y, 0, Lim));
      Span.Rgba8.g := Int8u(EnsureRange(g.y, 0, Lim));
      Span.Rgba8.b := Int8u(EnsureRange(b.y, 0, Lim));
      Span.Rgba8.a := Int8u(EnsureRange(a.y, 0, Lim));

      r.IncOperator(CAggSubpixelSize);
      g.IncOperator(CAggSubpixelSize);
      b.IncOperator(CAggSubpixelSize);
      a.IncOperator(CAggSubpixelSize);

      dec(Nlen, CAggSubpixelSize);
      dec(Start, CAggSubpixelSize);
      inc(PtrComp(Span), SizeOf(TAggColor));
      dec(Len);
    end;

  // Middle part, no checking for overflow.
  // Actual Spans can be longer than the calculated length
  // because of anti-aliasing, thus, the interpolators can
  // overflow. But while "nlen" is positive we are safe.
  while (Len <> 0) and (Nlen > 0) do
    begin
      Span.Rgba8.r := Int8u(r.y);
      Span.Rgba8.g := Int8u(g.y);
      Span.Rgba8.b := Int8u(b.y);
      Span.Rgba8.a := Int8u(a.y);

      r.IncOperator(CAggSubpixelSize);
      g.IncOperator(CAggSubpixelSize);
      b.IncOperator(CAggSubpixelSize);
      a.IncOperator(CAggSubpixelSize);

      dec(Nlen, CAggSubpixelSize);
      inc(PtrComp(Span), SizeOf(TAggColor));
      dec(Len);
    end;

  // Ending part; checking for overflow.
  // Typically it's 1-2 pixels, but may be more in some cases.
  while Len <> 0 do
    begin
      Span.Rgba8.r := Int8u(EnsureRange(r.y, 0, Lim));
      Span.Rgba8.g := Int8u(EnsureRange(g.y, 0, Lim));
      Span.Rgba8.b := Int8u(EnsureRange(b.y, 0, Lim));
      Span.Rgba8.a := Int8u(EnsureRange(a.y, 0, Lim));

      r.IncOperator(CAggSubpixelSize);
      g.IncOperator(CAggSubpixelSize);
      b.IncOperator(CAggSubpixelSize);
      a.IncOperator(CAggSubpixelSize);

      inc(PtrComp(Span), SizeOf(TAggColor));
      dec(Len);
    end;

  Result := Allocator.Span;
end;

procedure TAggSpanGouraudRgba.Generate(Span: PAggColor; x, y: Integer;
  Len: Cardinal);
const
  Lim = AggColor32.CAggBaseMask;
var
  PC1, PC2, t: PAggRgbaCalc;
  Nlen, Start, vr, Vg, VB, VA: Integer;
  r, g, b, a: TAggDdaLineInterpolator;
begin
  FRgba1.Calc(y); // (FRgba1.FDelta.Y > 2) ? FRgba1.F1.Y : y);

  PC1 := @FRgba1;
  PC2 := @FRgba2;

  if y <= FY2 then
    // Bottom part of the triangle (first subtriangle)
      FRgba2.Calc(y + FRgba2.FDelta.y)
  else
    begin
      // Upper part (second subtriangle)
      FRgba3.Calc(y - FRgba3.FDelta.y);

      PC2 := @FRgba3;
    end;

  if FSwap then
    begin
      // It means that the triangle is oriented clockwise,
      // so that we need to swap the controlling structures
      t := PC2;
      PC2 := PC1;
      PC1 := t;
    end;

  // Get the horizontal length with subpixel accuracy
  // and protect it from division by zero
  Nlen := Abs(PC2.fx - PC1.fx);

  if Nlen <= 0 then
      Nlen := 1;

  r.Initialize(PC1.FRed, PC2.FRed, Nlen, 14);
  g.Initialize(PC1.FGreen, PC2.FGreen, Nlen, 14);
  b.Initialize(PC1.FBlue, PC2.FBlue, Nlen, 14);
  a.Initialize(PC1.FAlpha, PC2.FAlpha, Nlen, 14);

  // Calculate the starting point of the Gradient with subpixel
  // accuracy and correct (roll back) the interpolators.
  // This operation will also clip the beginning of the span
  // if necessary.
  Start := PC1.fx - (x shl CAggSubpixelShift);

  r.DecOperator(Start);
  g.DecOperator(Start);
  b.DecOperator(Start);
  a.DecOperator(Start);

  inc(Nlen, Start);

  // Beginning part of the span. Since we rolled back the
  // interpolators, the color values may have overflow.
  // So that, we render the beginning part with checking
  // for overflow. It lasts until "start" is positive;
  // typically it's 1-2 pixels, but may be more in some cases.
  while (Len <> 0) and (Start > 0) do
    begin
      Span.Rgba8.r := Int8u(EnsureRange(r.y, 0, Lim));
      Span.Rgba8.g := Int8u(EnsureRange(g.y, 0, Lim));
      Span.Rgba8.b := Int8u(EnsureRange(b.y, 0, Lim));
      Span.Rgba8.a := Int8u(EnsureRange(a.y, 0, Lim));

      r.IncOperator(CAggSubpixelSize);
      g.IncOperator(CAggSubpixelSize);
      b.IncOperator(CAggSubpixelSize);
      a.IncOperator(CAggSubpixelSize);

      dec(Nlen, CAggSubpixelSize);
      dec(Start, CAggSubpixelSize);
      inc(PtrComp(Span), SizeOf(TAggColor));
      dec(Len);
    end;

  // Middle part, no checking for overflow.
  // Actual spans can be longer than the calculated length
  // because of anti-aliasing, thus, the interpolators can
  // overflow. But while "nlen" is positive we are safe.
  while (Len <> 0) and (Nlen > 0) do
    begin
      Span.Rgba8.r := Int8u(r.y);
      Span.Rgba8.g := Int8u(g.y);
      Span.Rgba8.b := Int8u(b.y);
      Span.Rgba8.a := Int8u(a.y);

      r.IncOperator(CAggSubpixelSize);
      g.IncOperator(CAggSubpixelSize);
      b.IncOperator(CAggSubpixelSize);
      a.IncOperator(CAggSubpixelSize);

      dec(Nlen, CAggSubpixelSize);
      inc(PtrComp(Span), SizeOf(TAggColor));
      dec(Len);
    end;

  // Ending part; checking for overflow.
  // Typically it's 1-2 pixels, but may be more in some cases.
  while Len <> 0 do
    begin
      Span.Rgba8.r := Int8u(EnsureRange(r.y, 0, Lim));
      Span.Rgba8.g := Int8u(EnsureRange(g.y, 0, Lim));
      Span.Rgba8.b := Int8u(EnsureRange(b.y, 0, Lim));
      Span.Rgba8.a := Int8u(EnsureRange(a.y, 0, Lim));

      r.IncOperator(CAggSubpixelSize);
      g.IncOperator(CAggSubpixelSize);
      b.IncOperator(CAggSubpixelSize);
      a.IncOperator(CAggSubpixelSize);

      inc(PtrComp(Span), SizeOf(TAggColor));
      dec(Len);
    end;
end;

end. 
 
 
