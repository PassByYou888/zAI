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
unit AggSpanImageFilterRgba;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggColor32,
  AggImageFilters,
  AggSpanImageFilter,
  AggSpanAllocator,
  AggSpanInterpolatorLinear,
  AggRenderingBuffer;

const
  CAggBaseShift = AggColor32.CAggBaseShift;
  CAggBaseMask  = AggColor32.CAggBaseMask;

type
  TAggSpanImageFilterRgbaNN = class(TAggSpanImageFilter)
  private
    FOrder: TAggOrder;
  public
    constructor Create(Alloc: TAggSpanAllocator; Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Source: TAggRenderingBuffer; BackColor: PAggColor; Interpolator: TAggSpanInterpolator; Order: TAggOrder); overload;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanImageFilterRgbaBilinear = class(TAggSpanImageFilter)
  private
    FOrder: TAggOrder;
  public
    constructor Create(Alloc: TAggSpanAllocator; Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Source: TAggRenderingBuffer; BackColor: PAggColor; Interpolator: TAggSpanInterpolator; Order: TAggOrder); overload;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanImageFilterRgba2x2 = class(TAggSpanImageFilter)
  private
    FOrder: TAggOrder;
  public
    constructor Create(Alloc: TAggSpanAllocator; Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Source: TAggRenderingBuffer; BackColor: PAggColor; Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT; Order: TAggOrder); overload;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanImageFilterRgba = class(TAggSpanImageFilter)
  private
    FOrder: TAggOrder;
  public
    constructor Create(Alloc: TAggSpanAllocator; Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Source: TAggRenderingBuffer; BackColor: PAggColor; Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT; Order: TAggOrder); overload;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

implementation


{ TAggSpanImageFilterRgbaNN }

constructor TAggSpanImageFilterRgbaNN.Create(Alloc: TAggSpanAllocator;
  Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;
end;

constructor TAggSpanImageFilterRgbaNN.Create(Alloc: TAggSpanAllocator;
  Source: TAggRenderingBuffer; BackColor: PAggColor;
  Interpolator: TAggSpanInterpolator; Order: TAggOrder);
begin
  inherited Create(Alloc, Source, BackColor, Interpolator, nil);

  FOrder := Order;
end;

function TAggSpanImageFilterRgbaNN.Generate(x, y: Integer;
  Len: Cardinal): PAggColor;
var
  ForeGround: array [0 .. 3] of Cardinal;
  ForeGroundPointer: PInt8u;
  Span: PAggColor;
  Max: TPointInteger;
begin
  Interpolator.SetBegin(x + FilterDeltaXDouble, y + FilterDeltaYDouble, Len);

  Span := Allocator.Span;

  Max.x := SourceImage.width - 1;
  Max.y := SourceImage.height - 1;

  repeat
    Interpolator.Coordinates(@x, @y);

    x := ShrInt32(x, CAggImageSubpixelShift);
    y := ShrInt32(y, CAggImageSubpixelShift);

    if (x >= 0) and (y >= 0) and (x <= Max.x) and (y <= Max.y) then
      begin
        ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(y)) + (x shl 2) *
          SizeOf(Int8u));

        ForeGround[0] := ForeGroundPointer^;
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        ForeGround[1] := ForeGroundPointer^;
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        ForeGround[2] := ForeGroundPointer^;
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        ForeGround[3] := ForeGroundPointer^;
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
      end
    else
      begin
        ForeGround[FOrder.r] := GetBackgroundColor.Rgba8.r;
        ForeGround[FOrder.g] := GetBackgroundColor.Rgba8.g;
        ForeGround[FOrder.b] := GetBackgroundColor.Rgba8.b;
        ForeGround[FOrder.a] := GetBackgroundColor.Rgba8.a;
      end;

    Span.Rgba8.r := ForeGround[FOrder.r];
    Span.Rgba8.g := ForeGround[FOrder.g];
    Span.Rgba8.b := ForeGround[FOrder.b];
    Span.Rgba8.a := ForeGround[FOrder.a];

    inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

{ TAggSpanImageFilterRgbaBilinear }

constructor TAggSpanImageFilterRgbaBilinear.Create(Alloc: TAggSpanAllocator;
  Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;
end;

constructor TAggSpanImageFilterRgbaBilinear.Create(Alloc: TAggSpanAllocator;
  Source: TAggRenderingBuffer; BackColor: PAggColor;
  Interpolator: TAggSpanInterpolator; Order: TAggOrder);
begin
  inherited Create(Alloc, Source, BackColor, Interpolator, nil);

  FOrder := Order;
end;

function TAggSpanImageFilterRgbaBilinear.Generate(x, y: Integer;
  Len: Cardinal): PAggColor;
var
  ForeGround: array [0 .. 3] of Cardinal;
  Backup: TAggRgba8;
  ForeGroundPointer: PInt8u;
  Span: PAggColor;
  Max, HiRes, LoRes: TPointInteger;
  Weight: Cardinal;
begin
  Interpolator.SetBegin(x + FilterDeltaXDouble, y + FilterDeltaYDouble, Len);

  Backup.r := GetBackgroundColor.Rgba8.r;
  Backup.g := GetBackgroundColor.Rgba8.g;
  Backup.b := GetBackgroundColor.Rgba8.b;
  Backup.a := GetBackgroundColor.Rgba8.a;

  Span := Allocator.Span;

  Max.x := SourceImage.width - 1;
  Max.y := SourceImage.height - 1;

  repeat
    Interpolator.Coordinates(@HiRes.x, @HiRes.y);

    dec(HiRes.x, FilterDeltaXInteger);
    dec(HiRes.y, FilterDeltaYInteger);

    LoRes.x := ShrInt32(HiRes.x, CAggImageSubpixelShift);
    LoRes.y := ShrInt32(HiRes.y, CAggImageSubpixelShift);

    if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x < Max.x) and
      (LoRes.y < Max.y) then
      begin
        ForeGround[0] := CAggImageSubpixelSize * CAggImageSubpixelSize div 2;
        ForeGround[1] := ForeGround[0];
        ForeGround[2] := ForeGround[0];
        ForeGround[3] := ForeGround[0];

        HiRes.x := HiRes.x and CAggImageSubpixelMask;
        HiRes.y := HiRes.y and CAggImageSubpixelMask;

        ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) +
          (LoRes.x shl 2) * SizeOf(Int8u));
        Weight := (CAggImageSubpixelSize - HiRes.x) * (CAggImageSubpixelSize -
          HiRes.y);

        inc(ForeGround[0], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(ForeGround[1], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(ForeGround[2], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(ForeGround[3], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

        Weight := HiRes.x * (CAggImageSubpixelSize - HiRes.y);

        inc(ForeGround[0], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(ForeGround[1], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(ForeGround[2], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(ForeGround[3], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

        ForeGroundPointer := SourceImage.NextRow
          (PInt8u(PtrComp(ForeGroundPointer) - 8 * SizeOf(Int8u)));
        Weight := (CAggImageSubpixelSize - HiRes.x) * HiRes.y;

        inc(ForeGround[0], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(ForeGround[1], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(ForeGround[2], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(ForeGround[3], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

        Weight := HiRes.x * HiRes.y;

        inc(ForeGround[0], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(ForeGround[1], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(ForeGround[2], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(ForeGround[3], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

        ForeGround[0] := ForeGround[0] shr (CAggImageSubpixelShift * 2);
        ForeGround[1] := ForeGround[1] shr (CAggImageSubpixelShift * 2);
        ForeGround[2] := ForeGround[2] shr (CAggImageSubpixelShift * 2);
        ForeGround[3] := ForeGround[3] shr (CAggImageSubpixelShift * 2);
      end
    else
      begin
        if (LoRes.x < -1) or (LoRes.y < -1) or (LoRes.x > Max.x) or
          (LoRes.y > Max.y) then
          begin
            ForeGround[FOrder.r] := Backup.r;
            ForeGround[FOrder.g] := Backup.g;
            ForeGround[FOrder.b] := Backup.b;
            ForeGround[FOrder.a] := Backup.a;
          end
        else
          begin
            ForeGround[0] := CAggImageSubpixelSize * CAggImageSubpixelSize div 2;
            ForeGround[1] := ForeGround[0];
            ForeGround[2] := ForeGround[0];
            ForeGround[3] := ForeGround[0];

            HiRes.x := HiRes.x and CAggImageSubpixelMask;
            HiRes.y := HiRes.y and CAggImageSubpixelMask;

            Weight := (CAggImageSubpixelSize - HiRes.x) *
              (CAggImageSubpixelSize - HiRes.y);

            if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x <= Max.x) and
              (LoRes.y <= Max.y) then
              begin
                ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) +
                  (LoRes.x shl 2) * SizeOf(Int8u));

                inc(ForeGround[0], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(ForeGround[1], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(ForeGround[2], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(ForeGround[3], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
              end
            else
              begin
                inc(ForeGround[FOrder.r], Backup.r * Weight);
                inc(ForeGround[FOrder.g], Backup.g * Weight);
                inc(ForeGround[FOrder.b], Backup.b * Weight);
                inc(ForeGround[FOrder.a], Backup.a * Weight);
              end;

            inc(LoRes.x);

            Weight := HiRes.x * (CAggImageSubpixelSize - HiRes.y);

            if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x <= Max.x) and
              (LoRes.y <= Max.y) then
              begin
                ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) +
                  (LoRes.x shl 2) * SizeOf(Int8u));

                inc(ForeGround[0], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(ForeGround[1], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(ForeGround[2], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(ForeGround[3], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
              end
            else
              begin
                inc(ForeGround[FOrder.r], Backup.r * Weight);
                inc(ForeGround[FOrder.g], Backup.g * Weight);
                inc(ForeGround[FOrder.b], Backup.b * Weight);
                inc(ForeGround[FOrder.a], Backup.a * Weight);
              end;

            dec(LoRes.x);
            inc(LoRes.y);

            Weight := (CAggImageSubpixelSize - HiRes.x) * HiRes.y;

            if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x <= Max.x) and
              (LoRes.y <= Max.y) then
              begin
                ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) +
                  (LoRes.x shl 2) * SizeOf(Int8u));

                inc(ForeGround[0], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(ForeGround[1], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(ForeGround[2], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(ForeGround[3], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
              end
            else
              begin
                inc(ForeGround[FOrder.r], Backup.r * Weight);
                inc(ForeGround[FOrder.g], Backup.g * Weight);
                inc(ForeGround[FOrder.b], Backup.b * Weight);
                inc(ForeGround[FOrder.a], Backup.a * Weight);
              end;

            inc(LoRes.x);

            Weight := HiRes.x * HiRes.y;

            if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x <= Max.x) and
              (LoRes.y <= Max.y) then
              begin
                ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) +
                  (LoRes.x shl 2) * SizeOf(Int8u));

                inc(ForeGround[0], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(ForeGround[1], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(ForeGround[2], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(ForeGround[3], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
              end
            else
              begin
                inc(ForeGround[FOrder.r], Backup.r * Weight);
                inc(ForeGround[FOrder.g], Backup.g * Weight);
                inc(ForeGround[FOrder.b], Backup.b * Weight);
                inc(ForeGround[FOrder.a], Backup.a * Weight);
              end;

            ForeGround[0] := ForeGround[0] shr (CAggImageSubpixelShift * 2);
            ForeGround[1] := ForeGround[1] shr (CAggImageSubpixelShift * 2);
            ForeGround[2] := ForeGround[2] shr (CAggImageSubpixelShift * 2);
            ForeGround[3] := ForeGround[3] shr (CAggImageSubpixelShift * 2);
          end;
      end;

    Span.Rgba8.r := Int8u(ForeGround[FOrder.r]);
    Span.Rgba8.g := Int8u(ForeGround[FOrder.g]);
    Span.Rgba8.b := Int8u(ForeGround[FOrder.b]);
    Span.Rgba8.a := Int8u(ForeGround[FOrder.a]);

    inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

{ TAggSpanImageFilterRgba2x2 }

constructor TAggSpanImageFilterRgba2x2.Create(Alloc: TAggSpanAllocator;
  Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;
end;

constructor TAggSpanImageFilterRgba2x2.Create(Alloc: TAggSpanAllocator;
  Source: TAggRenderingBuffer; BackColor: PAggColor; Interpolator: TAggSpanInterpolator;
  Filter: TAggImageFilterLUT; Order: TAggOrder);
begin
  inherited Create(Alloc, Source, BackColor, Interpolator, Filter);

  FOrder := Order;
end;

function TAggSpanImageFilterRgba2x2.Generate(x, y: Integer;
  Len: Cardinal): PAggColor;
var
  ForeGround: array [0 .. 3] of Cardinal;
  Backup: TAggRgba8;
  BackgroundColor: PAggColor;
  ForeGroundPointer: PInt8u;
  Span: PAggColor;
  WeightArray: PInt16;
  Max, HiRes, LoRes: TPointInteger;
  Weight: Cardinal;
begin
  Interpolator.SetBegin(x + FilterDeltaXDouble, y + FilterDeltaYDouble, Len);

  BackgroundColor := GetBackgroundColor;
  Backup := BackgroundColor.Rgba8;

  Span := Allocator.Span;

  WeightArray := PInt16(PtrComp(Filter.WeightArray) +
    ((Filter.Diameter div 2 - 1) shl CAggImageSubpixelShift) * SizeOf(Int16));

  Max.x := SourceImage.width - 1;
  Max.y := SourceImage.height - 1;

  repeat
    Interpolator.Coordinates(@HiRes.x, @HiRes.y);

    dec(HiRes.x, FilterDeltaXInteger);
    dec(HiRes.y, FilterDeltaYInteger);

    LoRes.x := ShrInt32(HiRes.x, CAggImageSubpixelShift);
    LoRes.y := ShrInt32(HiRes.y, CAggImageSubpixelShift);

    ForeGround[0] := CAggImageFilterSize div 2;
    ForeGround[1] := ForeGround[0];
    ForeGround[2] := ForeGround[0];
    ForeGround[3] := ForeGround[0];

    if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x < Max.x) and
      (LoRes.y < Max.y) then
      begin
        HiRes.x := HiRes.x and CAggImageSubpixelMask;
        HiRes.y := HiRes.y and CAggImageSubpixelMask;

        ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) +
          (LoRes.x shl 2) * SizeOf(Int8u));
        Weight := ShrInt32(PInt16(PtrComp(WeightArray) +
          (HiRes.x + CAggImageSubpixelSize) * SizeOf(Int16))^ *
          PInt16(PtrComp(WeightArray) + (HiRes.y + CAggImageSubpixelSize) *
          SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

        inc(ForeGround[0], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(ForeGround[1], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(ForeGround[2], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(ForeGround[3], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

        Weight := ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.x * SizeOf(Int16)
          )^ * PInt16(PtrComp(WeightArray) + (HiRes.y + CAggImageSubpixelSize) *
          SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

        inc(ForeGround[0], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(ForeGround[1], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(ForeGround[2], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(ForeGround[3], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

        ForeGroundPointer := SourceImage.NextRow
          (PInt8u(PtrComp(ForeGroundPointer) - 8 * SizeOf(Int8u)));
        Weight := ShrInt32(PInt16(PtrComp(WeightArray) +
          (HiRes.x + CAggImageSubpixelSize) * SizeOf(Int16))^ *
          PInt16(PtrComp(WeightArray) + HiRes.y * SizeOf(Int16))^ +
          CAggImageFilterSize div 2, CAggImageFilterShift);

        inc(ForeGround[0], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(ForeGround[1], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(ForeGround[2], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(ForeGround[3], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

        Weight := ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.x * SizeOf(Int16)
          )^ * PInt16(PtrComp(WeightArray) + HiRes.y * SizeOf(Int16))^ +
          CAggImageFilterSize div 2, CAggImageFilterShift);

        inc(ForeGround[0], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(ForeGround[1], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(ForeGround[2], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(ForeGround[3], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

        ForeGround[0] := ForeGround[0] shr CAggImageFilterShift;
        ForeGround[1] := ForeGround[1] shr CAggImageFilterShift;
        ForeGround[2] := ForeGround[2] shr CAggImageFilterShift;
        ForeGround[3] := ForeGround[3] shr CAggImageFilterShift;

        if ForeGround[FOrder.a] > CAggBaseMask then
            ForeGround[FOrder.a] := CAggBaseMask;

        if ForeGround[FOrder.r] > ForeGround[FOrder.a] then
            ForeGround[FOrder.r] := ForeGround[FOrder.a];

        if ForeGround[FOrder.g] > ForeGround[FOrder.a] then
            ForeGround[FOrder.g] := ForeGround[FOrder.a];

        if ForeGround[FOrder.b] > ForeGround[FOrder.a] then
            ForeGround[FOrder.b] := ForeGround[FOrder.a];
      end
    else
      begin
        if (LoRes.x < -1) or (LoRes.y < -1) or (LoRes.x > Max.x) or
          (LoRes.y > Max.y) then
          begin
            ForeGround[FOrder.r] := Backup.r;
            ForeGround[FOrder.g] := Backup.g;
            ForeGround[FOrder.b] := Backup.b;
            ForeGround[FOrder.a] := Backup.a;
          end
        else
          begin
            HiRes.x := HiRes.x and CAggImageSubpixelMask;
            HiRes.y := HiRes.y and CAggImageSubpixelMask;

            Weight := ShrInt32(PInt16(PtrComp(WeightArray) +
              (HiRes.x + CAggImageSubpixelSize) * SizeOf(Int16))^ *
              PInt16(PtrComp(WeightArray) + (HiRes.y + CAggImageSubpixelSize) *
              SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

            if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x <= Max.x) and
              (LoRes.y <= Max.y) then
              begin
                ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) +
                  (LoRes.x shl 2) * SizeOf(Int8u));

                inc(ForeGround[0], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(ForeGround[1], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(ForeGround[2], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(ForeGround[3], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
              end
            else
              begin
                inc(ForeGround[FOrder.r], Backup.r * Weight);
                inc(ForeGround[FOrder.g], Backup.g * Weight);
                inc(ForeGround[FOrder.b], Backup.b * Weight);
                inc(ForeGround[FOrder.a], Backup.a * Weight);
              end;

            inc(LoRes.x);

            Weight := ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.x *
              SizeOf(Int16))^ * PInt16(PtrComp(WeightArray) +
              (HiRes.y + CAggImageSubpixelSize) * SizeOf(Int16))^ +
              CAggImageFilterSize div 2, CAggImageFilterShift);

            if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x <= Max.x) and
              (LoRes.y <= Max.y) then
              begin
                ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) +
                  (LoRes.x shl 2) * SizeOf(Int8u));

                inc(ForeGround[0], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(ForeGround[1], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(ForeGround[2], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(ForeGround[3], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
              end
            else
              begin
                inc(ForeGround[FOrder.r], Backup.r * Weight);
                inc(ForeGround[FOrder.g], Backup.g * Weight);
                inc(ForeGround[FOrder.b], Backup.b * Weight);
                inc(ForeGround[FOrder.a], Backup.a * Weight);
              end;

            dec(LoRes.x);
            inc(LoRes.y);

            Weight := ShrInt32(PInt16(PtrComp(WeightArray) +
              (HiRes.x + CAggImageSubpixelSize) * SizeOf(Int16))^ *
              PInt16(PtrComp(WeightArray) + HiRes.y * SizeOf(Int16))^ +
              CAggImageFilterSize div 2, CAggImageFilterShift);

            if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x <= Max.x) and
              (LoRes.y <= Max.y) then
              begin
                ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) +
                  (LoRes.x shl 2) * SizeOf(Int8u));

                inc(ForeGround[0], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(ForeGround[1], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(ForeGround[2], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(ForeGround[3], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
              end
            else
              begin
                inc(ForeGround[FOrder.r], Backup.r * Weight);
                inc(ForeGround[FOrder.g], Backup.g * Weight);
                inc(ForeGround[FOrder.b], Backup.b * Weight);
                inc(ForeGround[FOrder.a], Backup.a * Weight);
              end;

            inc(LoRes.x);

            Weight := ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.x *
              SizeOf(Int16))^ * PInt16(PtrComp(WeightArray) + HiRes.y *
              SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

            if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x <= Max.x) and
              (LoRes.y <= Max.y) then
              begin
                ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) +
                  (LoRes.x shl 2) * SizeOf(Int8u));

                inc(ForeGround[0], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(ForeGround[1], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(ForeGround[2], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(ForeGround[3], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
              end
            else
              begin
                inc(ForeGround[FOrder.r], Backup.r * Weight);
                inc(ForeGround[FOrder.g], Backup.g * Weight);
                inc(ForeGround[FOrder.b], Backup.b * Weight);
                inc(ForeGround[FOrder.a], Backup.a * Weight);
              end;

            ForeGround[0] := ForeGround[0] shr CAggImageFilterShift;
            ForeGround[1] := ForeGround[1] shr CAggImageFilterShift;
            ForeGround[2] := ForeGround[2] shr CAggImageFilterShift;
            ForeGround[3] := ForeGround[3] shr CAggImageFilterShift;

            if ForeGround[FOrder.a] > CAggBaseMask then
                ForeGround[FOrder.a] := CAggBaseMask;

            if ForeGround[FOrder.r] > ForeGround[FOrder.a] then
                ForeGround[FOrder.r] := ForeGround[FOrder.a];

            if ForeGround[FOrder.g] > ForeGround[FOrder.a] then
                ForeGround[FOrder.g] := ForeGround[FOrder.a];

            if ForeGround[FOrder.b] > ForeGround[FOrder.a] then
                ForeGround[FOrder.b] := ForeGround[FOrder.a];
          end;
      end;

    Span.Rgba8.r := Int8u(ForeGround[FOrder.r]);
    Span.Rgba8.g := Int8u(ForeGround[FOrder.g]);
    Span.Rgba8.b := Int8u(ForeGround[FOrder.b]);
    Span.Rgba8.a := Int8u(ForeGround[FOrder.a]);

    inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

{ TAggSpanImageFilterRgba }

constructor TAggSpanImageFilterRgba.Create(Alloc: TAggSpanAllocator;
  Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;
end;

constructor TAggSpanImageFilterRgba.Create(Alloc: TAggSpanAllocator;
  Source: TAggRenderingBuffer; BackColor: PAggColor;
  Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT;
  Order: TAggOrder);
begin
  inherited Create(Alloc, Source, BackColor, Interpolator, Filter);

  FOrder := Order;
end;

function TAggSpanImageFilterRgba.Generate(x, y: Integer;
  Len: Cardinal): PAggColor;
var
  ForeGround: array [0 .. 3] of Integer;
  Backup: TAggRgba8;
  ForeGroundPointer: PInt8u;
  BackgroundColor: PAggColor;
  Diameter, StepBack, CountY: Cardinal;

  Max, Max2, LoRes, HiRes: TPointInteger;
  Start, Start1, CountX, WeightY, Weight, FractX: Integer;
  WeightArray: PInt16;
  Span: PAggColor;
begin
  Interpolator.SetBegin(x + FilterDeltaXDouble, y + FilterDeltaYDouble, Len);

  BackgroundColor := GetBackgroundColor;
  Backup := BackgroundColor.Rgba8;

  Diameter := Filter.Diameter;
  Start := Filter.Start;
  Start1 := Start - 1;
  WeightArray := Filter.WeightArray;

  StepBack := Diameter shl 2;

  Span := Allocator.Span;

  Max.x := SourceImage.width + Start - 2;
  Max.y := SourceImage.height + Start - 2;

  Max2.x := SourceImage.width - Start - 1;
  Max2.y := SourceImage.height - Start - 1;

  repeat
    Interpolator.Coordinates(@x, @y);

    dec(x, FilterDeltaXInteger);
    dec(y, FilterDeltaYInteger);

    HiRes.x := x;
    HiRes.y := y;

    LoRes.x := ShrInt32(HiRes.x, CAggImageSubpixelShift);
    LoRes.y := ShrInt32(HiRes.y, CAggImageSubpixelShift);

    ForeGround[0] := CAggImageFilterSize div 2;
    ForeGround[1] := ForeGround[0];
    ForeGround[2] := ForeGround[0];
    ForeGround[3] := ForeGround[0];

    FractX := HiRes.x and CAggImageSubpixelMask;
    CountY := Diameter;

    if (LoRes.x >= -Start) and (LoRes.y >= -Start) and (LoRes.x <= Max.x) and
      (LoRes.y <= Max.y) then
      begin
        HiRes.y := CAggImageSubpixelMask - (HiRes.y and CAggImageSubpixelMask);
        ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y + Start)) +
          ((LoRes.x + Start) shl 2) * SizeOf(Int8u));

        repeat
          CountX := Diameter;
          WeightY := PInt16(PtrComp(WeightArray) + HiRes.y * SizeOf(Int16))^;
          HiRes.x := CAggImageSubpixelMask - FractX;

          repeat
            Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HiRes.x
              * SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

            inc(ForeGround[0], ForeGroundPointer^ * Weight);
            inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
            inc(ForeGround[1], ForeGroundPointer^ * Weight);
            inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
            inc(ForeGround[2], ForeGroundPointer^ * Weight);
            inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
            inc(ForeGround[3], ForeGroundPointer^ * Weight);
            inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

            inc(HiRes.x, CAggImageSubpixelSize);
            dec(CountX);

          until CountX = 0;

          inc(HiRes.y, CAggImageSubpixelSize);

          ForeGroundPointer := SourceImage.NextRow
            (PInt8u(PtrComp(ForeGroundPointer) - StepBack));

          dec(CountY);
        until CountY = 0;

        ForeGround[0] := ShrInt32(ForeGround[0], CAggImageFilterShift);
        ForeGround[1] := ShrInt32(ForeGround[1], CAggImageFilterShift);
        ForeGround[2] := ShrInt32(ForeGround[2], CAggImageFilterShift);
        ForeGround[3] := ShrInt32(ForeGround[3], CAggImageFilterShift);

        if ForeGround[0] < 0 then
            ForeGround[0] := 0;

        if ForeGround[1] < 0 then
            ForeGround[1] := 0;

        if ForeGround[2] < 0 then
            ForeGround[2] := 0;

        if ForeGround[3] < 0 then
            ForeGround[3] := 0;

        if ForeGround[FOrder.a] > CAggBaseMask then
            ForeGround[FOrder.a] := CAggBaseMask;

        if ForeGround[FOrder.r] > ForeGround[FOrder.a] then
            ForeGround[FOrder.r] := ForeGround[FOrder.a];

        if ForeGround[FOrder.g] > ForeGround[FOrder.a] then
            ForeGround[FOrder.g] := ForeGround[FOrder.a];

        if ForeGround[FOrder.b] > ForeGround[FOrder.a] then
            ForeGround[FOrder.b] := ForeGround[FOrder.a];
      end
    else
      begin
        if (LoRes.x < Start1) or (LoRes.y < Start1) or (LoRes.x > Max2.x) or
          (LoRes.y > Max2.y) then
          begin
            ForeGround[FOrder.r] := Backup.r;
            ForeGround[FOrder.g] := Backup.g;
            ForeGround[FOrder.b] := Backup.b;
            ForeGround[FOrder.a] := Backup.a;
          end
        else
          begin
            LoRes.y := ShrInt32(y, CAggImageSubpixelShift) + Start;
            HiRes.y := CAggImageSubpixelMask - (HiRes.y and CAggImageSubpixelMask);

            repeat
              CountX := Diameter;
              WeightY := PInt16(PtrComp(WeightArray) + HiRes.y * SizeOf(Int16))^;

              LoRes.x := ShrInt32(x, CAggImageSubpixelShift) + Start;
              HiRes.x := CAggImageSubpixelMask - FractX;

              repeat
                Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) +
                  HiRes.x * SizeOf(Int16))^ + CAggImageFilterSize div 2,
                  CAggImageFilterShift);

                if (LoRes.x >= 0) and (LoRes.y >= 0) and
                  (LoRes.x < Trunc(SourceImage.width)) and
                  (LoRes.y < Trunc(SourceImage.height)) then
                  begin
                    ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) +
                      (LoRes.x shl 2) * SizeOf(Int8u));

                    inc(ForeGround[0], ForeGroundPointer^ * Weight);
                    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                    inc(ForeGround[1], ForeGroundPointer^ * Weight);
                    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                    inc(ForeGround[2], ForeGroundPointer^ * Weight);
                    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                    inc(ForeGround[3], ForeGroundPointer^ * Weight);
                    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                  end
                else
                  begin
                    inc(ForeGround[FOrder.r], Backup.r * Weight);
                    inc(ForeGround[FOrder.g], Backup.g * Weight);
                    inc(ForeGround[FOrder.b], Backup.b * Weight);
                    inc(ForeGround[FOrder.a], Backup.a * Weight);
                  end;

                inc(HiRes.x, CAggImageSubpixelSize);
                inc(LoRes.x);
                dec(CountX);
              until CountX = 0;

              inc(HiRes.y, CAggImageSubpixelSize);
              inc(LoRes.y);
              dec(CountY);
            until CountY = 0;

            ForeGround[0] := ShrInt32(ForeGround[0], CAggImageFilterShift);
            ForeGround[1] := ShrInt32(ForeGround[1], CAggImageFilterShift);
            ForeGround[2] := ShrInt32(ForeGround[2], CAggImageFilterShift);
            ForeGround[3] := ShrInt32(ForeGround[3], CAggImageFilterShift);

            if ForeGround[0] < 0 then
                ForeGround[0] := 0;

            if ForeGround[1] < 0 then
                ForeGround[1] := 0;

            if ForeGround[2] < 0 then
                ForeGround[2] := 0;

            if ForeGround[3] < 0 then
                ForeGround[3] := 0;

            if ForeGround[FOrder.a] > CAggBaseMask then
                ForeGround[FOrder.a] := CAggBaseMask;

            if ForeGround[FOrder.r] > ForeGround[FOrder.a] then
                ForeGround[FOrder.r] := ForeGround[FOrder.a];

            if ForeGround[FOrder.g] > ForeGround[FOrder.a] then
                ForeGround[FOrder.g] := ForeGround[FOrder.a];

            if ForeGround[FOrder.b] > ForeGround[FOrder.a] then
                ForeGround[FOrder.b] := ForeGround[FOrder.a];
          end;
      end;

    Span.Rgba8.r := ForeGround[FOrder.r];
    Span.Rgba8.g := ForeGround[FOrder.g];
    Span.Rgba8.b := ForeGround[FOrder.b];
    Span.Rgba8.a := ForeGround[FOrder.a];

    inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

end. 
 
 
