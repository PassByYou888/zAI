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
unit AggSpanPatternFilterRgba;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggColor32,
  AggSpanPattern,
  AggSpanImageFilter,
  AggSpanInterpolatorLinear,
  AggRenderingBuffer,
  AggSpanAllocator,
  AggImageFilters;

const
  CAggBaseShift = AggColor32.CAggBaseShift;
  CAggBaseMask  = AggColor32.CAggBaseMask;

type
  TAggCustomSpanPatternFilterRgba = class(TAggSpanImageFilter)
  protected
    FWrapModeX, FWrapModeY: TAggWrapMode;
    FOrder: TAggOrder;
    procedure SetSourceImage(Src: TAggRenderingBuffer); override;
  public
    constructor Create(Alloc: TAggSpanAllocator; Wx, Wy: TAggWrapMode; Order: TAggOrder); overload; virtual;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer; Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT; Wx, Wy: TAggWrapMode; Order: TAggOrder); overload; virtual;
  end;

  TAggSpanPatternFilterRgbaNN = class(TAggCustomSpanPatternFilterRgba)
  public
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer; Interpolator: TAggSpanInterpolator; Wx, Wy: TAggWrapMode; Order: TAggOrder); overload; virtual;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanPatternFilterRgbaBilinear = class(TAggCustomSpanPatternFilterRgba)
  public
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer; Interpolator: TAggSpanInterpolator; Wx, Wy: TAggWrapMode; Order: TAggOrder); overload; virtual;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanPatternFilterRgba2x2 = class(TAggCustomSpanPatternFilterRgba)
  public
    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanPatternFilterRgba = class(TAggCustomSpanPatternFilterRgba)
  public
    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

implementation


{ TAggCustomSpanPatternFilterRgba }

constructor TAggCustomSpanPatternFilterRgba.Create(Alloc: TAggSpanAllocator; Wx,
  Wy: TAggWrapMode; Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;

  FWrapModeX := Wx;
  FWrapModeY := Wy;

  FWrapModeX.Init(1);
  FWrapModeY.Init(1);
end;

constructor TAggCustomSpanPatternFilterRgba.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; Interpolator: TAggSpanInterpolator;
  Filter: TAggImageFilterLUT; Wx, Wy: TAggWrapMode; Order: TAggOrder);
var
  RGBA: TAggColor;
begin
  RGBA.Clear;

  inherited Create(Alloc, Src, @RGBA, Interpolator, Filter);

  FOrder := Order;

  FWrapModeX := Wx;
  FWrapModeY := Wy;

  FWrapModeX.Init(Src.width);
  FWrapModeY.Init(Src.height);
end;

procedure TAggCustomSpanPatternFilterRgba.SetSourceImage(
  Src: TAggRenderingBuffer);
begin
  inherited SetSourceImage(Src);

  FWrapModeX.Init(Src.width);
  FWrapModeY.Init(Src.height);
end;

{ TAggSpanPatternFilterRgbaNN }

constructor TAggSpanPatternFilterRgbaNN.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; Interpolator: TAggSpanInterpolator;
  Wx, Wy: TAggWrapMode; Order: TAggOrder);
begin
  inherited Create(Alloc, Src, Interpolator, nil, Wx, Wy, Order);
end;

function TAggSpanPatternFilterRgbaNN.Generate(x, y: Integer; Len: Cardinal): PAggColor;
var
  ForeGroundPointer: PInt8u;
  Span: PAggColor;
begin
  Interpolator.SetBegin(x + FilterDeltaXDouble, y + FilterDeltaYDouble, Len);

  Span := Allocator.Span;

  repeat
    Interpolator.Coordinates(@x, @y);

    x := FWrapModeX.FuncOperator(ShrInt32(x, CAggImageSubpixelShift));
    y := FWrapModeY.FuncOperator(ShrInt32(y, CAggImageSubpixelShift));

    ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(y)) + (x shl 2) *
      SizeOf(Int8u));

    Span.Rgba8.r := PInt8u(PtrComp(ForeGroundPointer) + FOrder.r * SizeOf(Int8u))^;
    Span.Rgba8.g := PInt8u(PtrComp(ForeGroundPointer) + FOrder.g * SizeOf(Int8u))^;
    Span.Rgba8.b := PInt8u(PtrComp(ForeGroundPointer) + FOrder.b * SizeOf(Int8u))^;
    Span.Rgba8.a := PInt8u(PtrComp(ForeGroundPointer) + FOrder.a * SizeOf(Int8u))^;

    inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

{ TAggSpanPatternFilterRgbaBilinear }

constructor TAggSpanPatternFilterRgbaBilinear.Create
  (Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
  Interpolator: TAggSpanInterpolator; Wx, Wy: TAggWrapMode;
  Order: TAggOrder);
begin
  inherited Create(Alloc, Src, Interpolator, nil, Wx, Wy, Order);
end;

function TAggSpanPatternFilterRgbaBilinear.Generate(x, y: Integer; Len: Cardinal): PAggColor;
var
  Fg: array [0 .. 3] of Cardinal;
  HiRes, LoRes: TPointInteger;
  Weight: Integer;
  x1, x2, y1, y2: Cardinal;
  ForeGroundPointer, Ptr1, Ptr2: PInt8u;
  Span: PAggColor;
begin
  Interpolator.SetBegin(x + FilterDeltaXDouble, y + FilterDeltaYDouble, Len);

  Span := Allocator.Span;

  repeat
    Interpolator.Coordinates(@HiRes.x, @HiRes.y);

    dec(HiRes.x, FilterDeltaXInteger);
    dec(HiRes.y, FilterDeltaYInteger);

    LoRes.x := ShrInt32(HiRes.x, CAggImageSubpixelShift);
    LoRes.y := ShrInt32(HiRes.y, CAggImageSubpixelShift);

    x1 := FWrapModeX.FuncOperator(LoRes.x);
    x2 := FWrapModeX.IncOperator;

    x1 := x1 shl 2;
    x2 := x2 shl 2;

    y1 := FWrapModeY.FuncOperator(LoRes.y);
    y2 := FWrapModeY.IncOperator;

    Ptr1 := SourceImage.Row(y1);
    Ptr2 := SourceImage.Row(y2);

    Fg[0] := CAggImageSubpixelSize * CAggImageSubpixelSize div 2;
    Fg[1] := Fg[0];
    Fg[2] := Fg[0];
    Fg[3] := Fg[0];

    HiRes.x := HiRes.x and CAggImageSubpixelMask;
    HiRes.y := HiRes.y and CAggImageSubpixelMask;

    ForeGroundPointer := PInt8u(PtrComp(Ptr1) + x1 * SizeOf(Int8u));
    Weight := (CAggImageSubpixelSize - HiRes.x) * (CAggImageSubpixelSize - HiRes.y);

    inc(Fg[0], Weight * ForeGroundPointer^);
    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    inc(Fg[1], Weight * ForeGroundPointer^);
    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    inc(Fg[2], Weight * ForeGroundPointer^);
    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    inc(Fg[3], Weight * ForeGroundPointer^);

    ForeGroundPointer := PInt8u(PtrComp(Ptr1) + x2 * SizeOf(Int8u));
    Weight := HiRes.x * (CAggImageSubpixelSize - HiRes.y);

    inc(Fg[0], Weight * ForeGroundPointer^);
    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    inc(Fg[1], Weight * ForeGroundPointer^);
    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    inc(Fg[2], Weight * ForeGroundPointer^);
    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    inc(Fg[3], Weight * ForeGroundPointer^);

    ForeGroundPointer := PInt8u(PtrComp(Ptr2) + x1 * SizeOf(Int8u));
    Weight := (CAggImageSubpixelSize - HiRes.x) * HiRes.y;

    inc(Fg[0], Weight * ForeGroundPointer^);
    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    inc(Fg[1], Weight * ForeGroundPointer^);
    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    inc(Fg[2], Weight * ForeGroundPointer^);
    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    inc(Fg[3], Weight * ForeGroundPointer^);

    ForeGroundPointer := PInt8u(PtrComp(Ptr2) + x2 * SizeOf(Int8u));
    Weight := HiRes.x * HiRes.y;

    inc(Fg[0], Weight * ForeGroundPointer^);
    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    inc(Fg[1], Weight * ForeGroundPointer^);
    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    inc(Fg[2], Weight * ForeGroundPointer^);
    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    inc(Fg[3], Weight * ForeGroundPointer^);

    Span.Rgba8.r := Int8u(Fg[FOrder.r] shr (CAggImageSubpixelShift * 2));
    Span.Rgba8.g := Int8u(Fg[FOrder.g] shr (CAggImageSubpixelShift * 2));
    Span.Rgba8.b := Int8u(Fg[FOrder.b] shr (CAggImageSubpixelShift * 2));
    Span.Rgba8.a := Int8u(Fg[FOrder.a] shr (CAggImageSubpixelShift * 2));

    inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

{ TAggSpanPatternFilterRgba2x2 }

function TAggSpanPatternFilterRgba2x2.Generate(x, y: Integer; Len: Cardinal): PAggColor;
var
  Fg: array [0 .. 3] of Cardinal;

  HiRes, LoRes: TPointInteger;
  Weight: Integer;

  x1, x2, y1, y2: Cardinal;

  ForeGroundPointer, Ptr1, Ptr2: PInt8u;

  Span: PAggColor;

  WeightArray: PInt16;
begin
  Interpolator.SetBegin(x + FilterDeltaXDouble, y + FilterDeltaYDouble, Len);

  Span := Allocator.Span;

  WeightArray := PInt16(PtrComp(Filter.WeightArray) +
    ShrInt32(Filter.Diameter div 2 - 1, CAggImageSubpixelShift));

  repeat
    Interpolator.Coordinates(@HiRes.x, @HiRes.y);

    dec(HiRes.x, FilterDeltaXInteger);
    dec(HiRes.y, FilterDeltaYInteger);

    LoRes.x := ShrInt32(HiRes.x, CAggImageSubpixelShift);
    LoRes.y := ShrInt32(HiRes.y, CAggImageSubpixelShift);

    x1 := FWrapModeX.FuncOperator(LoRes.x);
    x2 := FWrapModeX.IncOperator;

    x1 := x1 shl 2;
    x2 := x2 shl 2;

    y1 := FWrapModeY.FuncOperator(LoRes.y);
    y2 := FWrapModeY.IncOperator;

    Ptr1 := SourceImage.Row(y1);
    Ptr2 := SourceImage.Row(y2);

    Fg[0] := CAggImageFilterSize div 2;
    Fg[1] := Fg[0];
    Fg[2] := Fg[0];
    Fg[3] := Fg[0];

    HiRes.x := HiRes.x and CAggImageSubpixelMask;
    HiRes.y := HiRes.y and CAggImageSubpixelMask;

    ForeGroundPointer := PInt8u(PtrComp(Ptr1) + x1 * SizeOf(Int8u));
    Weight := ShrInt32(PInt16(PtrComp(WeightArray) +
      (HiRes.x + CAggImageSubpixelSize) * SizeOf(Int16))^ *
      PInt16(PtrComp(WeightArray) + (HiRes.y + CAggImageSubpixelSize) *
      SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

    inc(Fg[0], Weight * ForeGroundPointer^);
    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    inc(Fg[1], Weight * ForeGroundPointer^);
    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    inc(Fg[2], Weight * ForeGroundPointer^);
    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    inc(Fg[3], Weight * ForeGroundPointer^);

    ForeGroundPointer := PInt8u(PtrComp(Ptr1) + x2 * SizeOf(Int8u));
    Weight := ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.x * SizeOf(Int16))^
      * PInt16(PtrComp(WeightArray) + (HiRes.y + CAggImageSubpixelSize) *
      SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

    inc(Fg[0], Weight * ForeGroundPointer^);
    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    inc(Fg[1], Weight * ForeGroundPointer^);
    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    inc(Fg[2], Weight * ForeGroundPointer^);
    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    inc(Fg[3], Weight * ForeGroundPointer^);

    ForeGroundPointer := PInt8u(PtrComp(Ptr2) + x1 * SizeOf(Int8u));
    Weight := ShrInt32(PInt16(PtrComp(WeightArray) +
      (HiRes.x + CAggImageSubpixelSize) * SizeOf(Int16))^ *
      PInt16(PtrComp(WeightArray) + HiRes.y * SizeOf(Int16))^ +
      CAggImageFilterSize div 2, CAggImageFilterShift);

    inc(Fg[0], Weight * ForeGroundPointer^);
    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    inc(Fg[1], Weight * ForeGroundPointer^);
    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    inc(Fg[2], Weight * ForeGroundPointer^);
    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    inc(Fg[3], Weight * ForeGroundPointer^);

    ForeGroundPointer := PInt8u(PtrComp(Ptr2) + x2 * SizeOf(Int8u));
    Weight := ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.x * SizeOf(Int16))^
      * PInt16(PtrComp(WeightArray) + HiRes.y * SizeOf(Int16))^ +
      CAggImageFilterSize div 2, CAggImageFilterShift);

    inc(Fg[0], Weight * ForeGroundPointer^);
    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    inc(Fg[1], Weight * ForeGroundPointer^);
    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    inc(Fg[2], Weight * ForeGroundPointer^);
    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
    inc(Fg[3], Weight * ForeGroundPointer^);

    Fg[0] := Fg[0] shr CAggImageFilterShift;
    Fg[1] := Fg[1] shr CAggImageFilterShift;
    Fg[2] := Fg[2] shr CAggImageFilterShift;
    Fg[3] := Fg[3] shr CAggImageFilterShift;

    if Fg[FOrder.a] > CAggBaseMask then
        Fg[FOrder.a] := CAggBaseMask;

    if Fg[FOrder.r] > Fg[FOrder.a] then
        Fg[FOrder.r] := Fg[FOrder.a];

    if Fg[FOrder.g] > Fg[FOrder.a] then
        Fg[FOrder.g] := Fg[FOrder.a];

    if Fg[FOrder.b] > Fg[FOrder.a] then
        Fg[FOrder.b] := Fg[FOrder.a];

    Span.Rgba8.r := Int8u(Fg[FOrder.r]);
    Span.Rgba8.g := Int8u(Fg[FOrder.g]);
    Span.Rgba8.b := Int8u(Fg[FOrder.b]);
    Span.Rgba8.a := Int8u(Fg[FOrder.a]);

    inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

{ TAggSpanPatternFilterRgba }

function TAggSpanPatternFilterRgba.Generate(x, y: Integer; Len: Cardinal): PAggColor;
var
  Fg: array [0 .. 3] of Integer;
  Diameter, CountY: Cardinal;
  HiRes, LoRes: TPointInteger;
  Start, CountX, WeightY, FractX, IntX, Weight: Integer;
  RowPointer, ForeGroundPointer: PInt8u;
  WeightArray: PInt16;
  Span: PAggColor;
begin
  Interpolator.SetBegin(x + FilterDeltaXDouble, y + FilterDeltaYDouble, Len);

  Diameter := Filter.Diameter;
  Start := Filter.Start;
  WeightArray := Filter.WeightArray;

  Span := Allocator.Span;

  repeat
    Interpolator.Coordinates(@x, @y);

    dec(x, FilterDeltaXInteger);
    dec(y, FilterDeltaYInteger);

    HiRes.x := x;
    HiRes.x := y;

    FractX := HiRes.x and CAggImageSubpixelMask;
    CountY := Diameter;

    LoRes.y := FWrapModeY.FuncOperator
      (ShrInt32(y, CAggImageSubpixelShift) + Start);
    IntX := ShrInt32(x, CAggImageSubpixelShift) + Start;
    HiRes.y := CAggImageSubpixelMask - (HiRes.y and CAggImageSubpixelMask);

    Fg[0] := CAggImageFilterSize div 2;
    Fg[1] := Fg[0];
    Fg[2] := Fg[0];
    Fg[3] := Fg[0];

    repeat
      CountX := Diameter;
      WeightY := PInt16(PtrComp(WeightArray) + HiRes.y * SizeOf(Int16))^;

      HiRes.x := CAggImageSubpixelMask - FractX;
      LoRes.x := FWrapModeX.FuncOperator(IntX);

      RowPointer := SourceImage.Row(LoRes.y);

      repeat
        ForeGroundPointer := PInt8u(PtrComp(RowPointer) + (LoRes.x shl 2) * SizeOf(Int8u));
        Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HiRes.x *
          SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

        inc(Fg[0], ForeGroundPointer^ * Weight);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(Fg[1], ForeGroundPointer^ * Weight);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(Fg[2], ForeGroundPointer^ * Weight);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(Fg[3], ForeGroundPointer^ * Weight);
        inc(HiRes.x, CAggImageSubpixelSize);

        LoRes.x := FWrapModeX.IncOperator;

        dec(CountX);

      until CountX = 0;

      inc(HiRes.y, CAggImageSubpixelSize);

      LoRes.y := FWrapModeY.IncOperator;

      dec(CountY);

    until CountY = 0;

    Fg[0] := ShrInt32(Fg[0], CAggImageFilterShift);
    Fg[1] := ShrInt32(Fg[1], CAggImageFilterShift);
    Fg[2] := ShrInt32(Fg[2], CAggImageFilterShift);
    Fg[3] := ShrInt32(Fg[3], CAggImageFilterShift);

    if Fg[0] < 0 then
        Fg[0] := 0;

    if Fg[1] < 0 then
        Fg[1] := 0;

    if Fg[2] < 0 then
        Fg[2] := 0;

    if Fg[3] < 0 then
        Fg[3] := 0;

    if Fg[FOrder.a] > CAggBaseMask then
        Fg[FOrder.a] := CAggBaseMask;

    if Fg[FOrder.r] > Fg[FOrder.a] then
        Fg[FOrder.r] := Fg[FOrder.a];

    if Fg[FOrder.g] > Fg[FOrder.a] then
        Fg[FOrder.g] := Fg[FOrder.a];

    if Fg[FOrder.b] > Fg[FOrder.a] then
        Fg[FOrder.b] := Fg[FOrder.a];

    Span.Rgba8.r := Int8u(Fg[FOrder.r]);
    Span.Rgba8.g := Int8u(Fg[FOrder.g]);
    Span.Rgba8.b := Int8u(Fg[FOrder.b]);
    Span.Rgba8.a := Int8u(Fg[FOrder.a]);

    inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

end. 
 
 
