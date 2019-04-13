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
unit AggSpanImageFilterRgb;

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
  TAggSpanImageFilterRgbNN = class(TAggSpanImageFilter)
  private
    FOrder: TAggOrder;
  public
    constructor Create(Alloc: TAggSpanAllocator; Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer; BackColor: PAggColor; inter: TAggSpanInterpolator; Order: TAggOrder); overload;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanImageFilterRgbBilinear = class(TAggSpanImageFilter)
  private
    FOrder: TAggOrder;
  public
    constructor Create(Alloc: TAggSpanAllocator; Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer; BackColor: PAggColor; inter: TAggSpanInterpolator; Order: TAggOrder); overload;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanImageFilterRgb2x2 = class(TAggSpanImageFilter)
  private
    FOrder: TAggOrder;
  public
    constructor Create(Alloc: TAggSpanAllocator; Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer; BackColor: PAggColor; inter: TAggSpanInterpolator; Filter: TAggImageFilterLUT; Order: TAggOrder); overload;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanImageFilterRgb = class(TAggSpanImageFilter)
  private
    FOrder: TAggOrder;
  public
    constructor Create(Alloc: TAggSpanAllocator; Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer; BackColor: PAggColor; inter: TAggSpanInterpolator; Filter: TAggImageFilterLUT; Order: TAggOrder); overload;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

implementation


{ TAggSpanImageFilterRgbNN }

constructor TAggSpanImageFilterRgbNN.Create(Alloc: TAggSpanAllocator;
  Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;
end;

constructor TAggSpanImageFilterRgbNN.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor; inter: TAggSpanInterpolator;
  Order: TAggOrder);
begin
  inherited Create(Alloc, Src, BackColor, inter, nil);

  FOrder := Order;
end;

function TAggSpanImageFilterRgbNN.Generate(x, y: Integer; Len: Cardinal): PAggColor;
var
  Fg: array [0 .. 2] of Cardinal;
  SrcAlpha: Cardinal;
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
        ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(y)) + (x + x + x) *
          SizeOf(Int8u));

        Fg[0] := ForeGroundPointer^;
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        Fg[1] := ForeGroundPointer^;
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        Fg[2] := ForeGroundPointer^;
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

        SrcAlpha := CAggBaseMask;

      end
    else
      begin
        Fg[FOrder.r] := GetBackgroundColor.Rgba8.r;
        Fg[FOrder.g] := GetBackgroundColor.Rgba8.g;
        Fg[FOrder.b] := GetBackgroundColor.Rgba8.b;
        SrcAlpha := GetBackgroundColor.Rgba8.a;
      end;

    Span.Rgba8.r := Int8u(Fg[FOrder.r]);
    Span.Rgba8.g := Int8u(Fg[FOrder.g]);
    Span.Rgba8.b := Int8u(Fg[FOrder.b]);
    Span.Rgba8.a := Int8u(SrcAlpha);

    inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

{ TAggSpanImageFilterRgbBilinear }

constructor TAggSpanImageFilterRgbBilinear.Create(Alloc: TAggSpanAllocator;
  Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;
end;

constructor TAggSpanImageFilterRgbBilinear.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor; inter: TAggSpanInterpolator;
  Order: TAggOrder);
begin
  inherited Create(Alloc, Src, BackColor, inter, nil);

  FOrder := Order;
end;

function TAggSpanImageFilterRgbBilinear.Generate(x, y: Integer; Len: Cardinal): PAggColor;
var
  Fg: array [0 .. 2] of Cardinal;
  SrcAlpha, Weight: Cardinal;
  Backup: TAggRgba8;
  ForeGroundPointer: PInt8u;
  Span: PAggColor;
  Max, HiRes, LoRes: TPointInteger;
begin
  Interpolator.SetBegin(x + FilterDeltaXDouble, y + FilterDeltaYDouble, Len);

  Backup := GetBackgroundColor.Rgba8;

  Span := Allocator.Span;

  Max.x := SourceImage.width - 1;
  Max.y := SourceImage.height - 1;

  repeat
    Interpolator.Coordinates(@HiRes.x, @HiRes.y);

    dec(HiRes.x, FilterDeltaXInteger);
    dec(HiRes.y, FilterDeltaYInteger);

    LoRes.x := ShrInt32(HiRes.x, CAggImageSubpixelShift);
    LoRes.y := ShrInt32(HiRes.y, CAggImageSubpixelShift);

    if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x < Max.x) and (LoRes.y < Max.y) then
      begin
        Fg[0] := CAggImageSubpixelSize * CAggImageSubpixelSize div 2;
        Fg[1] := Fg[0];
        Fg[2] := Fg[0];

        HiRes.x := HiRes.x and CAggImageSubpixelMask;
        HiRes.y := HiRes.y and CAggImageSubpixelMask;

        ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) +
          (LoRes.x + LoRes.x + LoRes.x) * SizeOf(Int8u));
        Weight := (CAggImageSubpixelSize - HiRes.x) * (CAggImageSubpixelSize - HiRes.y);

        inc(Fg[0], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(Fg[1], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(Fg[2], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

        Weight := HiRes.x * (CAggImageSubpixelSize - HiRes.y);

        inc(Fg[0], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(Fg[1], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(Fg[2], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

        ForeGroundPointer := SourceImage.NextRow
          (PInt8u(PtrComp(ForeGroundPointer) - 6 * SizeOf(Int8u)));
        Weight := (CAggImageSubpixelSize - HiRes.x) * HiRes.y;

        inc(Fg[0], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(Fg[1], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(Fg[2], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

        Weight := HiRes.x * HiRes.y;

        inc(Fg[0], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(Fg[1], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(Fg[2], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

        Fg[0] := Fg[0] shr (CAggImageSubpixelShift * 2);
        Fg[1] := Fg[1] shr (CAggImageSubpixelShift * 2);
        Fg[2] := Fg[2] shr (CAggImageSubpixelShift * 2);

        SrcAlpha := CAggBaseMask;
      end
    else
      begin
        if (LoRes.x < -1) or (LoRes.y < -1) or (LoRes.x > Max.x) or (LoRes.y > Max.y) then
          begin
            Fg[FOrder.r] := Backup.r;
            Fg[FOrder.g] := Backup.g;
            Fg[FOrder.b] := Backup.b;
            SrcAlpha := Backup.a;
          end
        else
          begin
            Fg[0] := CAggImageSubpixelSize * CAggImageSubpixelSize div 2;
            Fg[1] := Fg[0];
            Fg[2] := Fg[0];
            SrcAlpha := Fg[0];

            HiRes.x := HiRes.x and CAggImageSubpixelMask;
            HiRes.y := HiRes.y and CAggImageSubpixelMask;

            Weight := (CAggImageSubpixelSize - HiRes.x) * (CAggImageSubpixelSize - HiRes.y);

            if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x <= Max.x) and (LoRes.y <= Max.y)
            then
              begin
                ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) +
                  (LoRes.x + LoRes.x + LoRes.x) * SizeOf(Int8u));

                inc(Fg[0], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(Fg[1], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(Fg[2], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

                inc(SrcAlpha, Weight * CAggBaseMask);
              end
            else
              begin
                inc(Fg[FOrder.r], Backup.r * Weight);
                inc(Fg[FOrder.g], Backup.g * Weight);
                inc(Fg[FOrder.b], Backup.b * Weight);

                inc(SrcAlpha, Backup.a * Weight);
              end;

            inc(LoRes.x);

            Weight := HiRes.x * (CAggImageSubpixelSize - HiRes.y);

            if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x <= Max.x) and (LoRes.y <= Max.y)
            then
              begin
                ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) +
                  (LoRes.x + LoRes.x + LoRes.x) * SizeOf(Int8u));

                inc(Fg[0], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(Fg[1], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(Fg[2], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

                inc(SrcAlpha, Weight * CAggBaseMask);
              end
            else
              begin
                inc(Fg[FOrder.r], Backup.r * Weight);
                inc(Fg[FOrder.g], Backup.g * Weight);
                inc(Fg[FOrder.b], Backup.b * Weight);

                inc(SrcAlpha, Backup.a * Weight);
              end;

            dec(LoRes.x);
            inc(LoRes.y);

            Weight := (CAggImageSubpixelSize - HiRes.x) * HiRes.y;

            if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x <= Max.x) and (LoRes.y <= Max.y)
            then
              begin
                ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) +
                  (LoRes.x + LoRes.x + LoRes.x) * SizeOf(Int8u));

                inc(Fg[0], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(Fg[1], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(Fg[2], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

                inc(SrcAlpha, Weight * CAggBaseMask);
              end
            else
              begin
                inc(Fg[FOrder.r], Backup.r * Weight);
                inc(Fg[FOrder.g], Backup.g * Weight);
                inc(Fg[FOrder.b], Backup.b * Weight);

                inc(SrcAlpha, Backup.a * Weight);
              end;

            inc(LoRes.x);

            Weight := HiRes.x * HiRes.y;

            if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x <= Max.x) and (LoRes.y <= Max.y)
            then
              begin
                ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) +
                  (LoRes.x + LoRes.x + LoRes.x) * SizeOf(Int8u));

                inc(Fg[0], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(Fg[1], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(Fg[2], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

                inc(SrcAlpha, Weight * CAggBaseMask);
              end
            else
              begin
                inc(Fg[FOrder.r], Backup.r * Weight);
                inc(Fg[FOrder.g], Backup.g * Weight);
                inc(Fg[FOrder.b], Backup.b * Weight);

                inc(SrcAlpha, Backup.a * Weight);
              end;

            Fg[0] := Fg[0] shr (CAggImageSubpixelShift * 2);
            Fg[1] := Fg[0] shr (CAggImageSubpixelShift * 2);
            Fg[2] := Fg[0] shr (CAggImageSubpixelShift * 2);

            SrcAlpha := SrcAlpha shr (CAggImageSubpixelShift * 2);
          end;
      end;

    Span.Rgba8.r := Int8u(Fg[FOrder.r]);
    Span.Rgba8.g := Int8u(Fg[FOrder.g]);
    Span.Rgba8.b := Int8u(Fg[FOrder.b]);
    Span.Rgba8.a := Int8u(SrcAlpha);

    inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

{ TAggSpanImageFilterRgb2x2 }

constructor TAggSpanImageFilterRgb2x2.Create(Alloc: TAggSpanAllocator;
  Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;
end;

constructor TAggSpanImageFilterRgb2x2.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor; inter: TAggSpanInterpolator;
  Filter: TAggImageFilterLUT; Order: TAggOrder);
begin
  inherited Create(Alloc, Src, BackColor, inter, Filter);

  FOrder := Order;
end;

function TAggSpanImageFilterRgb2x2.Generate(x, y: Integer; Len: Cardinal): PAggColor;
var
  Fg: array [0 .. 2] of Cardinal;
  SrcAlpha, Weight: Cardinal;
  Backup: TAggRgba8;
  ForeGroundPointer: PInt8u;
  Span: PAggColor;
  WeightArray: PInt16;
  Max, HiRes, LoRes: TPointInteger;
begin
  Interpolator.SetBegin(x + FilterDeltaXDouble, y + FilterDeltaYDouble, Len);

  Backup := GetBackgroundColor.Rgba8;

  Span := Allocator.Span;

  WeightArray := PInt16(PtrComp(Filter.WeightArray) +
    ((Filter.Diameter div 2 - 1) shl CAggImageSubpixelShift) * SizeOf(Int16));

  Max := PointInteger(SourceImage.width - 1, SourceImage.height - 1);

  repeat
    Interpolator.Coordinates(@HiRes.x, @HiRes.y);

    dec(HiRes.x, FilterDeltaXInteger);
    dec(HiRes.y, FilterDeltaYInteger);

    LoRes.x := ShrInt32(HiRes.x, CAggImageSubpixelShift);
    LoRes.y := ShrInt32(HiRes.y, CAggImageSubpixelShift);

    if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x < Max.x) and (LoRes.y < Max.y) then
      begin
        Fg[0] := CAggImageFilterSize div 2;
        Fg[1] := Fg[0];
        Fg[2] := Fg[0];

        HiRes.x := HiRes.x and CAggImageSubpixelMask;
        HiRes.y := HiRes.y and CAggImageSubpixelMask;

        ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) +
          (LoRes.x + LoRes.x + LoRes.x) * SizeOf(Int8u));
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

        Weight := ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.x * SizeOf(Int16)
          )^ * PInt16(PtrComp(WeightArray) + (HiRes.y + CAggImageSubpixelSize) *
          SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

        inc(Fg[0], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(Fg[1], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(Fg[2], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

        ForeGroundPointer := SourceImage.NextRow
          (PInt8u(PtrComp(ForeGroundPointer) - 6 * SizeOf(Int8u)));
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

        Weight := ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.x * SizeOf(Int16)
          )^ * PInt16(PtrComp(WeightArray) + HiRes.y * SizeOf(Int16))^ +
          CAggImageFilterSize div 2, CAggImageFilterShift);

        inc(Fg[0], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(Fg[1], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
        inc(Fg[2], Weight * ForeGroundPointer^);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

        Fg[0] := Fg[0] shr CAggImageFilterShift;
        Fg[1] := Fg[1] shr CAggImageFilterShift;
        Fg[2] := Fg[2] shr CAggImageFilterShift;

        SrcAlpha := CAggBaseMask;

        if Fg[0] > CAggBaseMask then
            Fg[0] := CAggBaseMask;

        if Fg[1] > CAggBaseMask then
            Fg[1] := CAggBaseMask;

        if Fg[2] > CAggBaseMask then
            Fg[2] := CAggBaseMask;
      end
    else
      begin
        if (LoRes.x < -1) or (LoRes.y < -1) or (LoRes.x > Max.x) or (LoRes.y > Max.y) then
          begin
            Fg[FOrder.r] := Backup.r;
            Fg[FOrder.g] := Backup.g;
            Fg[FOrder.b] := Backup.b;
            SrcAlpha := Backup.a;
          end
        else
          begin
            Fg[0] := CAggImageFilterSize div 2;
            Fg[1] := Fg[0];
            Fg[2] := Fg[0];
            SrcAlpha := Fg[0];

            HiRes.x := HiRes.x and CAggImageSubpixelMask;
            HiRes.y := HiRes.y and CAggImageSubpixelMask;

            Weight := ShrInt32(PInt16(PtrComp(WeightArray) +
              (HiRes.x + CAggImageSubpixelSize) * SizeOf(Int16))^ *
              PInt16(PtrComp(WeightArray) + (HiRes.y + CAggImageSubpixelSize) *
              SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

            if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x <= Max.x) and (LoRes.y <= Max.y)
            then
              begin
                ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) +
                  (LoRes.x + LoRes.x + LoRes.x) * SizeOf(Int8u));

                inc(Fg[0], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(Fg[1], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(Fg[2], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

                inc(SrcAlpha, Weight * CAggBaseMask);
              end
            else
              begin
                inc(Fg[FOrder.r], Backup.r * Weight);
                inc(Fg[FOrder.g], Backup.g * Weight);
                inc(Fg[FOrder.b], Backup.b * Weight);

                inc(SrcAlpha, Backup.a * Weight);
              end;

            inc(LoRes.x);

            Weight := ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.x *
              SizeOf(Int16))^ * PInt16(PtrComp(WeightArray) +
              (HiRes.y + CAggImageSubpixelSize) * SizeOf(Int16))^ +
              CAggImageFilterSize div 2, CAggImageFilterShift);

            if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x <= Max.x) and (LoRes.y <= Max.y)
            then
              begin
                ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) +
                  (LoRes.x + LoRes.x + LoRes.x) * SizeOf(Int8u));

                inc(Fg[0], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(Fg[1], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(Fg[2], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

                inc(SrcAlpha, Weight * CAggBaseMask);
              end
            else
              begin
                inc(Fg[FOrder.r], Backup.r * Weight);
                inc(Fg[FOrder.g], Backup.g * Weight);
                inc(Fg[FOrder.b], Backup.b * Weight);

                inc(SrcAlpha, Backup.a * Weight);
              end;

            dec(LoRes.x);
            inc(LoRes.y);

            Weight := ShrInt32(PInt16(PtrComp(WeightArray) +
              (HiRes.x + CAggImageSubpixelSize) * SizeOf(Int16))^ *
              PInt16(PtrComp(WeightArray) + HiRes.y * SizeOf(Int16))^ +
              CAggImageFilterSize div 2, CAggImageFilterShift);

            if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x <= Max.x) and (LoRes.y <= Max.y)
            then
              begin
                ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) +
                  (LoRes.x + LoRes.x + LoRes.x) * SizeOf(Int8u));

                inc(Fg[0], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(Fg[1], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(Fg[2], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

                inc(SrcAlpha, Weight * CAggBaseMask);
              end
            else
              begin
                inc(Fg[FOrder.r], Backup.r * Weight);
                inc(Fg[FOrder.g], Backup.g * Weight);
                inc(Fg[FOrder.b], Backup.b * Weight);

                inc(SrcAlpha, Backup.a * Weight);
              end;

            inc(LoRes.x);

            Weight := ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.x *
              SizeOf(Int16))^ * PInt16(PtrComp(WeightArray) + HiRes.y *
              SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

            if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x <= Max.x) and (LoRes.y <= Max.y)
            then
              begin
                ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) +
                  (LoRes.x + LoRes.x + LoRes.x) * SizeOf(Int8u));

                inc(Fg[0], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(Fg[1], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(Fg[2], Weight * ForeGroundPointer^);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

                inc(SrcAlpha, Weight * CAggBaseMask);
              end
            else
              begin
                inc(Fg[FOrder.r], Backup.r * Weight);
                inc(Fg[FOrder.g], Backup.g * Weight);
                inc(Fg[FOrder.b], Backup.b * Weight);

                inc(SrcAlpha, Backup.a * Weight);
              end;

            Fg[0] := Fg[0] shr CAggImageFilterShift;
            Fg[1] := Fg[0] shr CAggImageFilterShift;
            Fg[2] := Fg[0] shr CAggImageFilterShift;

            SrcAlpha := SrcAlpha shr CAggImageFilterShift;

            if SrcAlpha > CAggBaseMask then
                SrcAlpha := CAggBaseMask;

            if Fg[0] > SrcAlpha then
                Fg[0] := SrcAlpha;

            if Fg[1] > SrcAlpha then
                Fg[1] := SrcAlpha;

            if Fg[2] > SrcAlpha then
                Fg[2] := SrcAlpha;
          end;
      end;

    Span.Rgba8.r := Int8u(Fg[FOrder.r]);
    Span.Rgba8.g := Int8u(Fg[FOrder.g]);
    Span.Rgba8.b := Int8u(Fg[FOrder.b]);
    Span.Rgba8.a := Int8u(SrcAlpha);

    inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

{ TAggSpanImageFilterRgb }

constructor TAggSpanImageFilterRgb.Create(Alloc: TAggSpanAllocator;
  Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;
end;

constructor TAggSpanImageFilterRgb.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor; inter: TAggSpanInterpolator;
  Filter: TAggImageFilterLUT; Order: TAggOrder);
begin
  inherited Create(Alloc, Src, BackColor, inter, Filter);

  FOrder := Order;
end;

function TAggSpanImageFilterRgb.Generate(x, y: Integer;
  Len: Cardinal): PAggColor;
var
  Fg: array [0 .. 2] of Integer;
  Max, Max2: TPointInteger;
  SrcAlpha, Start, Start1, CountX, WeightY, Weight, FractX: Integer;
  HiRes, LoRes: TPointInteger;
  Backup: TAggRgba8;
  ForeGroundPointer: PInt8u;
  Diameter, StepBack, CountY: Cardinal;
  WeightArray: PInt16;
  Span: PAggColor;
begin
  Interpolator.SetBegin(x + FilterDeltaXDouble, y + FilterDeltaYDouble, Len);

  Backup := GetBackgroundColor.Rgba8;

  Diameter := Filter.Diameter;
  Start := Filter.Start;
  Start1 := Start - 1;
  WeightArray := Filter.WeightArray;

  StepBack := Diameter * 3;

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

    Fg[0] := CAggImageFilterSize div 2;
    Fg[1] := Fg[0];
    Fg[2] := Fg[0];

    FractX := HiRes.x and CAggImageSubpixelMask;
    CountY := Diameter;

    if (LoRes.x >= -Start) and (LoRes.y >= -Start) and (LoRes.x <= Max.x) and
      (LoRes.y <= Max.y) then
      begin
        HiRes.y := CAggImageSubpixelMask - (HiRes.y and CAggImageSubpixelMask);
        ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y + Start)) +
          (LoRes.x + Start) * 3 * SizeOf(Int8u));

        repeat
          CountX := Diameter;
          WeightY := PInt16(PtrComp(WeightArray) + HiRes.y * SizeOf(Int16))^;
          HiRes.x := CAggImageSubpixelMask - FractX;

          repeat
            Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HiRes.x
              * SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

            inc(Fg[0], ForeGroundPointer^ * Weight);
            inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
            inc(Fg[1], ForeGroundPointer^ * Weight);
            inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
            inc(Fg[2], ForeGroundPointer^ * Weight);
            inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

            inc(HiRes.x, CAggImageSubpixelSize);
            dec(CountX);
          until CountX = 0;

          inc(HiRes.y, CAggImageSubpixelSize);

          ForeGroundPointer := SourceImage.NextRow
            (PInt8u(PtrComp(ForeGroundPointer) - StepBack * SizeOf(Int8u)));

          dec(CountY);
        until CountY = 0;

        Fg[0] := ShrInt32(Fg[0], CAggImageFilterShift);
        Fg[1] := ShrInt32(Fg[1], CAggImageFilterShift);
        Fg[2] := ShrInt32(Fg[2], CAggImageFilterShift);

        if Fg[0] < 0 then
            Fg[0] := 0;

        if Fg[1] < 0 then
            Fg[1] := 0;

        if Fg[2] < 0 then
            Fg[2] := 0;

        if Fg[0] > CAggBaseMask then
            Fg[0] := CAggBaseMask;

        if Fg[1] > CAggBaseMask then
            Fg[1] := CAggBaseMask;

        if Fg[2] > CAggBaseMask then
            Fg[2] := CAggBaseMask;

        SrcAlpha := CAggBaseMask;
      end
    else
      begin
        if (LoRes.x < Start1) or (LoRes.y < Start1) or (LoRes.x > Max2.x) or
          (LoRes.y > Max2.y) then
          begin
            Fg[FOrder.r] := Backup.r;
            Fg[FOrder.g] := Backup.g;
            Fg[FOrder.b] := Backup.b;
            SrcAlpha := Backup.a;
          end
        else
          begin
            SrcAlpha := CAggImageFilterSize div 2;

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
                      LoRes.x * 3 * SizeOf(Int8u));

                    inc(Fg[0], ForeGroundPointer^ * Weight);
                    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                    inc(Fg[1], ForeGroundPointer^ * Weight);
                    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                    inc(Fg[2], ForeGroundPointer^ * Weight);
                    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

                    inc(SrcAlpha, CAggBaseMask * Weight);
                  end
                else
                  begin
                    inc(Fg[FOrder.r], Backup.r * Weight);
                    inc(Fg[FOrder.g], Backup.g * Weight);
                    inc(Fg[FOrder.b], Backup.b * Weight);

                    inc(SrcAlpha, Backup.a * Weight);
                  end;

                inc(HiRes.x, CAggImageSubpixelSize);
                inc(LoRes.x);
                dec(CountX);
              until CountX = 0;

              inc(HiRes.y, CAggImageSubpixelSize);
              inc(LoRes.y);
              dec(CountY);

            until CountY = 0;

            Fg[0] := ShrInt32(Fg[0], CAggImageFilterShift);
            Fg[1] := ShrInt32(Fg[1], CAggImageFilterShift);
            Fg[2] := ShrInt32(Fg[2], CAggImageFilterShift);

            SrcAlpha := ShrInt32(SrcAlpha, CAggImageFilterShift);

            if Fg[0] < 0 then
                Fg[0] := 0;

            if Fg[1] < 0 then
                Fg[1] := 0;

            if Fg[2] < 0 then
                Fg[2] := 0;

            if SrcAlpha < 0 then
                SrcAlpha := 0;

            if SrcAlpha > CAggBaseMask then
                SrcAlpha := CAggBaseMask;

            if Fg[0] > SrcAlpha then
                Fg[0] := SrcAlpha;

            if Fg[1] > SrcAlpha then
                Fg[1] := SrcAlpha;

            if Fg[2] > SrcAlpha then
                Fg[2] := SrcAlpha;
          end;
      end;

    Span.Rgba8.r := Int8u(Fg[FOrder.r]);
    Span.Rgba8.g := Int8u(Fg[FOrder.g]);
    Span.Rgba8.b := Int8u(Fg[FOrder.b]);
    Span.Rgba8.a := Int8u(SrcAlpha);

    inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

end. 
 
 
