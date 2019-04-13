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
unit AggSpanPatternResampleRgb;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggColor32,
  AggSpanPattern,
  AggSpanImageResample,
  AggSpanInterpolatorLinear,
  AggRenderingBuffer,
  AggSpanAllocator,
  AggImageFilters;

const
  CAggBaseShift      = AggColor32.CAggBaseShift;
  CAggBaseMask       = AggColor32.CAggBaseMask;
  CAggDownscaleShift = CAggImageFilterShift;

type
  TAggSpanPatternResampleRgbAffine = class(TAggSpanImageResampleAffine)
  private
    FWrapModeX, FWrapModeY: TAggWrapMode;

    FOrder: TAggOrder;
  protected
    procedure SetSourceImage(Src: TAggRenderingBuffer); override;
  public
    constructor Create(Alloc: TAggSpanAllocator; Wx, Wy: TAggWrapMode; Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer; Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT; Wx, Wy: TAggWrapMode; Order: TAggOrder); overload;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanPatternResampleRgb = class(TAggSpanImageResample)
  private
    FWrapModeX, FWrapModeY: TAggWrapMode;

    FOrder: TAggOrder;
  protected
    procedure SetSourceImage(Src: TAggRenderingBuffer); override;
  public
    constructor Create(Alloc: TAggSpanAllocator; Wx, Wy: TAggWrapMode; Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer; Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT; Wx, Wy: TAggWrapMode; Order: TAggOrder); overload;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

implementation


{ TAggSpanPatternResampleRgbAffine }

constructor TAggSpanPatternResampleRgbAffine.Create
  (Alloc: TAggSpanAllocator; Wx, Wy: TAggWrapMode; Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;

  FWrapModeX := Wx;
  FWrapModeY := Wy;

  FWrapModeX.Init(1);
  FWrapModeY.Init(1);
end;

constructor TAggSpanPatternResampleRgbAffine.Create
  (Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
  Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT;
  Wx, Wy: TAggWrapMode; Order: TAggOrder);
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

procedure TAggSpanPatternResampleRgbAffine.SetSourceImage(
  Src: TAggRenderingBuffer);
begin
  inherited SetSourceImage(Src);

  FWrapModeX.Init(Src.width);
  FWrapModeY.Init(Src.height);
end;

function TAggSpanPatternResampleRgbAffine.Generate(x, y: Integer;
  Len: Cardinal): PAggColor;
var
  Span: PAggColor;
  Intr: TAggSpanInterpolator;
  Fg: array [0 .. 2] of Integer;
  radius, Max, LoRes, HiRes: TPointInteger;
  Diameter, FilterSize, TotalWeight, WeightY, Weight: Integer;
  InitialLoResX, InitialHiResX: Integer;
  RowPointer, ForeGroundPointer: PInt8u;
  WeightArray: PInt16;
begin
  Span := Allocator.Span;
  Intr := Interpolator;

  Intr.SetBegin(x + FilterDeltaXDouble, y + FilterDeltaYDouble, Len);

  Diameter := Filter.Diameter;
  FilterSize := Diameter shl CAggImageSubpixelShift;

  radius.x := ShrInt32(Diameter * FRadiusX, 1);
  radius.y := ShrInt32(Diameter * FRadiusY, 1);

  Max := PointInteger(SourceImage.width - 1, SourceImage.height - 1);
  WeightArray := Filter.WeightArray;

  repeat
    Intr.Coordinates(@x, @y);

    inc(x, FilterDeltaXInteger - radius.x);
    inc(y, FilterDeltaYInteger - radius.y);

    Fg[0] := CAggImageFilterSize div 2;
    Fg[1] := Fg[0];
    Fg[2] := Fg[0];

    LoRes.y := FWrapModeY.FuncOperator(ShrInt32(y, CAggImageSubpixelShift));
    HiRes.y := ShrInt32((CAggImageSubpixelMask -
      (y and CAggImageSubpixelMask)) * FRadiusYInv, CAggImageSubpixelShift);

    TotalWeight := 0;

    InitialLoResX := ShrInt32(x, CAggImageSubpixelShift);
    InitialHiResX := ShrInt32((CAggImageSubpixelMask -
      (x and CAggImageSubpixelMask)) * FRadiusXInv, CAggImageSubpixelShift);

    repeat
      WeightY := PInt16(PtrComp(WeightArray) + HiRes.y * SizeOf(Int16))^;

      LoRes.x := FWrapModeX.FuncOperator(InitialLoResX);
      HiRes.x := InitialHiResX;

      RowPointer := SourceImage.Row(LoRes.y);

      repeat
        ForeGroundPointer := PInt8u(PtrComp(RowPointer) + LoRes.x * 3 *
          SizeOf(Int8u));
        Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HiRes.x *
          SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggDownscaleShift);

        inc(Fg[0], ForeGroundPointer^ * Weight);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(Fg[1], ForeGroundPointer^ * Weight);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(Fg[2], ForeGroundPointer^ * Weight);
        inc(TotalWeight, Weight);
        inc(HiRes.x, FRadiusXInv);

        LoRes.x := FWrapModeX.IncOperator;
      until HiRes.x >= FilterSize;

      inc(HiRes.y, FRadiusYInv);

      LoRes.y := FWrapModeY.IncOperator;
    until HiRes.y >= FilterSize;

    Fg[0] := EnsureRange(Fg[0] div TotalWeight, 0, CAggBaseMask);
    Fg[1] := EnsureRange(Fg[1] div TotalWeight, 0, CAggBaseMask);
    Fg[2] := EnsureRange(Fg[2] div TotalWeight, 0, CAggBaseMask);

    Span.Rgba8.r := Int8u(Fg[FOrder.r]);
    Span.Rgba8.g := Int8u(Fg[FOrder.g]);
    Span.Rgba8.b := Int8u(Fg[FOrder.b]);
    Span.Rgba8.a := Int8u(CAggBaseMask);

    inc(PtrComp(Span), SizeOf(TAggColor));

    Intr.IncOperator;

    dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

{ TAggSpanPatternResampleRgb }

constructor TAggSpanPatternResampleRgb.Create(Alloc: TAggSpanAllocator;
  Wx, Wy: TAggWrapMode; Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;

  FWrapModeX := Wx;
  FWrapModeY := Wy;

  FWrapModeX.Init(1);
  FWrapModeY.Init(1);
end;

constructor TAggSpanPatternResampleRgb.Create(Alloc: TAggSpanAllocator;
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

procedure TAggSpanPatternResampleRgb.SetSourceImage;
begin
  inherited SetSourceImage(Src);

  FWrapModeX.Init(Src.width);
  FWrapModeY.Init(Src.height);
end;

function TAggSpanPatternResampleRgb.Generate(x, y: Integer;
  Len: Cardinal): PAggColor;
var
  Span: PAggColor;
  Intr: TAggSpanInterpolator;

  Fg: array [0 .. 2] of Integer;

  radius, Max, LoRes, HiRes: TPointInteger;
  Diameter, FilterSize, RX, RY, RxInv, RyInv, TotalWeight: Integer;
  InitialLoResX, InitialHiResX, WeightY, Weight: Integer;

  RowPointer, ForeGroundPointer: PInt8u;

  WeightArray: PInt16;
begin
  Span := Allocator.Span;
  Intr := Interpolator;

  Intr.SetBegin(x + FilterDeltaXDouble, y + FilterDeltaYDouble, Len);

  Diameter := Filter.Diameter;
  FilterSize := Diameter shl CAggImageSubpixelShift;
  WeightArray := Filter.WeightArray;

  repeat
    RxInv := CAggImageSubpixelSize;
    RyInv := CAggImageSubpixelSize;

    Intr.Coordinates(@x, @y);
    Intr.LocalScale(@RX, @RY);

    RX := ShrInt32(RX * FBlur.x, CAggImageSubpixelShift);
    RY := ShrInt32(RY * FBlur.y, CAggImageSubpixelShift);

    if RX < CAggImageSubpixelSize then
        RX := CAggImageSubpixelSize
    else
      begin
        if RX > CAggImageSubpixelSize * FScaleLimit then
            RX := CAggImageSubpixelSize * FScaleLimit;

        RxInv := CAggImageSubpixelSize * CAggImageSubpixelSize div RX;
      end;

    if RY < CAggImageSubpixelSize then
        RY := CAggImageSubpixelSize
    else
      begin
        if RY > CAggImageSubpixelSize * FScaleLimit then
            RY := CAggImageSubpixelSize * FScaleLimit;

        RyInv := CAggImageSubpixelSize * CAggImageSubpixelSize div RY;
      end;

    radius.x := ShrInt32(Diameter * RX, 1);
    radius.y := ShrInt32(Diameter * RY, 1);

    Max := PointInteger(SourceImage.width - 1, SourceImage.height - 1);

    inc(x, FilterDeltaXInteger - radius.x);
    inc(y, FilterDeltaYInteger - radius.y);

    Fg[0] := CAggImageFilterSize div 2;
    Fg[1] := Fg[0];
    Fg[2] := Fg[0];

    LoRes.y := FWrapModeY.FuncOperator(ShrInt32(y, CAggImageSubpixelShift));
    HiRes.y := ShrInt32((CAggImageSubpixelMask -
      (y and CAggImageSubpixelMask)) * RyInv, CAggImageSubpixelShift);

    TotalWeight := 0;

    InitialLoResX := ShrInt32(x, CAggImageSubpixelShift);
    InitialHiResX := ShrInt32((CAggImageSubpixelMask -
      (x and CAggImageSubpixelMask)) * RxInv, CAggImageSubpixelShift);

    repeat
      WeightY := PInt16(PtrComp(WeightArray) + HiRes.y * SizeOf(Int16))^;

      LoRes.x := FWrapModeX.FuncOperator(InitialLoResX);
      HiRes.x := InitialHiResX;

      RowPointer := SourceImage.Row(LoRes.y);

      repeat
        ForeGroundPointer := PInt8u(PtrComp(RowPointer) + LoRes.x * 3 *
          SizeOf(Int8u));
        Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HiRes.x *
          SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggDownscaleShift);

        inc(Fg[0], ForeGroundPointer^ * Weight);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(Fg[1], ForeGroundPointer^ * Weight);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(Fg[2], ForeGroundPointer^ * Weight);
        inc(TotalWeight, Weight);
        inc(HiRes.x, RxInv);

        LoRes.x := FWrapModeX.IncOperator;
      until HiRes.x >= FilterSize;

      inc(HiRes.y, RyInv);

      LoRes.y := FWrapModeY.IncOperator;
    until HiRes.y >= FilterSize;

    Fg[0] := EnsureRange(Fg[0] div TotalWeight, 0, CAggBaseMask);
    Fg[1] := EnsureRange(Fg[1] div TotalWeight, 0, CAggBaseMask);
    Fg[2] := EnsureRange(Fg[2] div TotalWeight, 0, CAggBaseMask);

    Span.Rgba8.r := Int8u(Fg[FOrder.r]);
    Span.Rgba8.g := Int8u(Fg[FOrder.g]);
    Span.Rgba8.b := Int8u(Fg[FOrder.b]);
    Span.Rgba8.a := Int8u(CAggBaseMask);

    inc(PtrComp(Span), SizeOf(TAggColor));

    Intr.IncOperator;

    dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

end. 
 
 
