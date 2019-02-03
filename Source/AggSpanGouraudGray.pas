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
unit AggSpanGouraudGray;

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
  PAggGrayCalc = ^TAggGrayCalc;

  TAggGrayCalc = record
  private
    f1, FDelta: TPointDouble;
    FColorC1V, FColorC1A, FColorDeltaV, FColorDeltaA,
      FColorV, FColorA, fx: Integer;
  public
    procedure Init(c1, c2: PAggCoordType);
    procedure Calculate(y: Double);
  end;

  TAggSpanGouraudGray = class(TAggSpanGouraud)
  private
    FSwap: Boolean;
    FY2: Integer;
    FC1, FC2, FC3: TAggGrayCalc;
  public
    constructor Create(Alloc: TAggSpanAllocator); overload;
    constructor Create(Alloc: TAggSpanAllocator; c1, c2, c3: PAggColor; x1, y1, x2, y2, x3, y3, d: Double); overload;

    procedure Prepare(MaxSpanLength: Cardinal); override;
    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

implementation


{ TAggGrayCalc }

procedure TAggGrayCalc.Init;
var
  dy: Double;
begin
  f1 := PointDouble(c1.x - 0.5, c1.y - 0.5);
  FDelta.x := c2.x - c1.x;

  dy := c2.y - c1.y;

  if Abs(dy) < 1E-10 then
      FDelta.y := 1E10
  else
      FDelta.y := 1.0 / dy;

  FColorC1V := c1.COLOR.v;
  FColorC1A := c1.COLOR.Rgba8.a;
  FColorDeltaV := c2.COLOR.v - FColorC1V;
  FColorDeltaA := c2.COLOR.Rgba8.a - FColorC1A;
end;

procedure TAggGrayCalc.Calculate;
var
  k: Double;
begin
  k := EnsureRange((y - f1.y) * FDelta.y, 0, 1);

  FColorV := FColorC1V + IntegerRound(FColorDeltaV * k);
  FColorA := FColorC1A + IntegerRound(FColorDeltaA * k);
  fx := IntegerRound((f1.x + FDelta.x * k) * CAggSubpixelSize);
end;

{ TAggSpanGouraudGray }

constructor TAggSpanGouraudGray.Create(Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);
end;

constructor TAggSpanGouraudGray.Create(Alloc: TAggSpanAllocator;
  c1, c2, c3: PAggColor; x1, y1, x2, y2, x3, y3, d: Double);
begin
  inherited Create(Alloc, c1, c2, c3, x1, y1, x2, y2, x3, y3, d);
end;

procedure TAggSpanGouraudGray.Prepare;
var
  Coord: array [0 .. 2] of TAggCoordType;
begin
  inherited Prepare(MaxSpanLength);

  ArrangeVertices(@Coord);

  FY2 := Trunc(Coord[1].y);

  FSwap := CalculatePointLocation(Coord[0].x, Coord[0].y, Coord[2].x, Coord[2].y,
    Coord[1].x, Coord[1].y) < 0.0;

  FC1.Init(@Coord[0], @Coord[2]);
  FC2.Init(@Coord[0], @Coord[1]);
  FC3.Init(@Coord[1], @Coord[2]);
end;

function TAggSpanGouraudGray.Generate(x, y: Integer; Len: Cardinal): PAggColor;
const
  CLim = AggColor32.CAggBaseMask;
var
  PC1, PC2, t: PAggGrayCalc;

  Nlen, Start: Integer;

  v, a: TAggDdaLineInterpolator;
  Span: PAggColor;
begin
  FC1.Calculate(y);

  PC1 := @FC1;
  PC2 := @FC2;

  if y < FY2 then
    // Bottom part of the triangle (first subtriangle)
      FC2.Calculate(y + FC2.FDelta.y)
  else
    begin
      // Upper part (second subtriangle)
      FC3.Calculate(y - FC3.FDelta.y);

      PC2 := @FC3;
    end;

  // It means that the triangle is oriented clockwise,
  // so that we need to swap the controlling structures
  if FSwap then
    begin
      t := PC2;
      PC2 := PC1;
      PC1 := t;
    end;

  // Get the horizontal length with subpixel accuracy
  // and protect it from division by zero
  Nlen := Abs(PC2.fx - PC1.fx);

  if Nlen <= 0 then
      Nlen := 1;

  v.Initialize(PC1.FColorV, PC2.FColorV, Nlen, 14);
  a.Initialize(PC1.FColorA, PC2.FColorA, Nlen, 14);

  // Calculate the starting point of the Gradient with subpixel
  // accuracy and correct (roll back) the interpolators.
  // This operation will also clip the beginning of the Span
  // if necessary.
  Start := PC1.fx - (x shl CAggSubpixelShift);

  v.DecOperator(Start);
  a.DecOperator(Start);

  inc(Nlen, Start);

  Span := Allocator.Span;

  // Beginning part of the Span. Since we rolled back the
  // interpolators, the color values may have overfLow.
  // So that, we render the beginning part with checking
  // for overfLow. It lasts until "start" is positive;
  // typically it's 1-2 pixels, but may be more in some cases.
  while (Len <> 0) and (Start > 0) do
    begin
      Span.v := Int8u(EnsureRange(v.y, 0, CLim));
      Span.Rgba8.a := Int8u(EnsureRange(a.y, 0, CLim));

      v.IncOperator(CAggSubpixelSize);
      a.IncOperator(CAggSubpixelSize);

      dec(Nlen, CAggSubpixelSize);
      dec(Start, CAggSubpixelSize);
      inc(PtrComp(Span), SizeOf(TAggColor));
      dec(Len);
    end;

  // Middle part, no checking for overfLow.
  // Actual Spans can be longer than the calculated length
  // because of anti-aliasing, thus, the interpolators can
  // overfLow. But while "nlen" is positive we are safe.
  while (Len <> 0) and (Nlen > 0) do
    begin
      Span.v := Int8u(v.y);
      Span.Rgba8.a := Int8u(a.y);

      v.IncOperator(CAggSubpixelSize);
      a.IncOperator(CAggSubpixelSize);

      dec(Nlen, CAggSubpixelSize);
      inc(PtrComp(Span), SizeOf(TAggColor));
      dec(Len);
    end;

  // Ending part; checking for overfLow.
  // Typically it's 1-2 pixels, but may be more in some cases.
  while Len <> 0 do
    begin
      Span.v := Int8u(EnsureRange(v.y, 0, CLim));
      Span.Rgba8.a := Int8u(EnsureRange(a.y, 0, CLim));

      v.IncOperator(CAggSubpixelSize);
      a.IncOperator(CAggSubpixelSize);

      inc(PtrComp(Span), SizeOf(TAggColor));
      dec(Len);
    end;

  Result := Allocator.Span;
end;

end. 
 
 
