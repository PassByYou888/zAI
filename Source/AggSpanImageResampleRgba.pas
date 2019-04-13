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
unit AggSpanImageResampleRgba;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggColor32,
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
  TAggSpanImageResampleRgba = class(TAggSpanImageResample)
  private
    FOrder: TAggOrder;
  public
    constructor Create(Alloc: TAggSpanAllocator; Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
      BackColor: PAggColor; Interpolator: TAggSpanInterpolator;
      Filter: TAggImageFilterLUT; Order: TAggOrder); overload;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanImageResampleRgbaAffine = class(TAggSpanImageResampleAffine)
  private
    FOrder: TAggOrder;
  public
    constructor Create(Alloc: TAggSpanAllocator; Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
      BackColor: PAggColor; Interpolator: TAggSpanInterpolator;
      Filter: TAggImageFilterLUT; Order: TAggOrder); overload;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

implementation


{ TAggSpanImageResampleRgba }

constructor TAggSpanImageResampleRgba.Create(Alloc: TAggSpanAllocator;
  Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;
end;

constructor TAggSpanImageResampleRgba.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor;
  Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT;
  Order: TAggOrder);
begin
  inherited Create(Alloc, Src, BackColor, Interpolator, Filter);

  FOrder := Order;
end;

function TAggSpanImageResampleRgba.Generate(x, y: Integer; Len: Cardinal): PAggColor;
var
  Span: PAggColor;
  Fg: array [0 .. 3] of Integer;
  Backup: TAggRgba8;
  radius, Max, LoRes, HiRes: TPointInteger;
  Diameter, FilterSize, RX, RY, RxInv, RyInv, TotalWeight, InitialLoResX, InitialHiResX,
    WeightY, Weight: Integer;
  ForeGroundPointer: PInt8u;
  BackgroundColor: PAggColor;
  WeightArray: PInt16;
begin
  Span := Allocator.Span;

  Interpolator.SetBegin(x + FilterDeltaXDouble, y + FilterDeltaYDouble, Len);

  BackgroundColor := GetBackgroundColor;
  Backup := BackgroundColor.Rgba8;

  Diameter := Filter.Diameter;
  FilterSize := Diameter shl CAggImageSubpixelShift;

  WeightArray := Filter.WeightArray;

  repeat
    RxInv := CAggImageSubpixelSize;
    RyInv := CAggImageSubpixelSize;

    Interpolator.Coordinates(@x, @y);
    Interpolator.LocalScale(@RX, @RY);

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

    Max.x := SourceImage.width - 1;
    Max.y := SourceImage.height - 1;

    inc(x, FilterDeltaXInteger - radius.x);
    inc(y, FilterDeltaYInteger - radius.y);

    Fg[0] := CAggImageFilterSize div 2;
    Fg[1] := Fg[0];
    Fg[2] := Fg[0];
    Fg[3] := Fg[0];

    LoRes.y := ShrInt32(y, CAggImageSubpixelShift);
    HiRes.y := ShrInt32((CAggImageSubpixelMask - (y and CAggImageSubpixelMask)) *
      RyInv, CAggImageSubpixelShift);

    TotalWeight := 0;

    InitialLoResX := ShrInt32(x, CAggImageSubpixelShift);
    InitialHiResX := ShrInt32((CAggImageSubpixelMask - (x and CAggImageSubpixelMask)) *
      RxInv, CAggImageSubpixelShift);

    repeat
      WeightY := PInt16(PtrComp(WeightArray) + HiRes.y * SizeOf(Int16))^;

      LoRes.x := InitialLoResX;
      HiRes.x := InitialHiResX;

      if (LoRes.y >= 0) and (LoRes.y <= Max.y) then
        begin
          ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) + (LoRes.x shl 2) *
            SizeOf(Int8u));

          repeat
            Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HiRes.x
              * SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggDownscaleShift);

            if (LoRes.x >= 0) and (LoRes.x <= Max.x) then
              begin
                inc(Fg[0], ForeGroundPointer^ * Weight);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(Fg[1], ForeGroundPointer^ * Weight);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(Fg[2], ForeGroundPointer^ * Weight);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(Fg[3], ForeGroundPointer^ * Weight);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
              end
            else
              begin
                inc(Fg[FOrder.r], Backup.r * Weight);
                inc(Fg[FOrder.g], Backup.g * Weight);
                inc(Fg[FOrder.b], Backup.b * Weight);
                inc(Fg[FOrder.a], Backup.a * Weight);

                inc(PtrComp(ForeGroundPointer), 4 * SizeOf(Int8u));
              end;

            inc(TotalWeight, Weight);
            inc(HiRes.x, RxInv);
            inc(LoRes.x);
          until HiRes.x >= FilterSize;
        end
      else
        repeat
          Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HiRes.x
            * SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggDownscaleShift);

          inc(TotalWeight, Weight);
          inc(Fg[FOrder.r], Backup.r * Weight);
          inc(Fg[FOrder.g], Backup.g * Weight);
          inc(Fg[FOrder.b], Backup.b * Weight);
          inc(Fg[FOrder.a], Backup.a * Weight);
          inc(HiRes.x, RxInv);
        until HiRes.x >= FilterSize;

      inc(HiRes.y, RyInv);
      inc(LoRes.y);
    until HiRes.y >= FilterSize;

    Fg[0] := Fg[0] div TotalWeight;
    Fg[1] := Fg[1] div TotalWeight;
    Fg[2] := Fg[2] div TotalWeight;
    Fg[3] := Fg[3] div TotalWeight;

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

{ TAggSpanImageResampleRgbaAffine }

constructor TAggSpanImageResampleRgbaAffine.Create(Alloc: TAggSpanAllocator;
  Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;
end;

constructor TAggSpanImageResampleRgbaAffine.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor;
  Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT;
  Order: TAggOrder);
begin
  inherited Create(Alloc, Src, BackColor, Interpolator, Filter);

  FOrder := Order;
end;

function TAggSpanImageResampleRgbaAffine.Generate(x, y: Integer; Len: Cardinal): PAggColor;
var
  Fg: array [0 .. 3] of Integer;
  Backup: TAggRgba8;
  Span: PAggColor;
  radius, Max, LoRes, HiRes: TPointInteger;
  Diameter, FilterSize, TotalWeight, InitialLoResX, InitialHiResX, WeightY,
    Weight: Integer;
  ForeGroundPointer: PInt8u;
  BackgroundColor: PAggColor;
  WeightArray: PInt16;
begin
  Interpolator.SetBegin(x + FilterDeltaXDouble, y + FilterDeltaYDouble, Len);

  BackgroundColor := GetBackgroundColor;
  Backup := BackgroundColor.Rgba8;

  Span := Allocator.Span;

  Diameter := Filter.Diameter;
  FilterSize := Diameter shl CAggImageSubpixelShift;

  radius.x := ShrInt32(Diameter * FRadiusX, 1);
  radius.y := ShrInt32(Diameter * FRadiusY, 1);

  Max.x := SourceImage.width - 1;
  Max.y := SourceImage.height - 1;

  WeightArray := Filter.WeightArray;

  repeat
    Interpolator.Coordinates(@x, @y);

    inc(x, FilterDeltaXInteger - radius.x);
    inc(y, FilterDeltaYInteger - radius.y);

    Fg[0] := CAggImageFilterSize div 2;
    Fg[1] := Fg[0];
    Fg[2] := Fg[0];
    Fg[3] := Fg[0];

    LoRes.y := ShrInt32(y, CAggImageSubpixelShift);
    HiRes.y := ShrInt32((CAggImageSubpixelMask - (y and CAggImageSubpixelMask)) *
      FRadiusYInv, CAggImageSubpixelShift);

    TotalWeight := 0;

    InitialLoResX := ShrInt32(x, CAggImageSubpixelShift);
    InitialHiResX := ShrInt32((CAggImageSubpixelMask - (x and CAggImageSubpixelMask)) *
      FRadiusXInv, CAggImageSubpixelShift);

    repeat
      WeightY := PInt16(PtrComp(WeightArray) + HiRes.y * SizeOf(Int16))^;

      LoRes.x := InitialLoResX;
      HiRes.x := InitialHiResX;

      if (LoRes.y >= 0) and (LoRes.y <= Max.y) then
        begin
          ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) + (LoRes.x shl 2) *
            SizeOf(Int8u));

          repeat
            Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HiRes.x
              * SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggDownscaleShift);

            if (LoRes.x >= 0) and (LoRes.x <= Max.x) then
              begin
                inc(Fg[0], ForeGroundPointer^ * Weight);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(Fg[1], ForeGroundPointer^ * Weight);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(Fg[2], ForeGroundPointer^ * Weight);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                inc(Fg[3], ForeGroundPointer^ * Weight);
                inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
              end
            else
              begin
                inc(Fg[FOrder.r], Backup.r * Weight);
                inc(Fg[FOrder.g], Backup.g * Weight);
                inc(Fg[FOrder.b], Backup.b * Weight);
                inc(Fg[FOrder.a], Backup.a * Weight);

                inc(PtrComp(ForeGroundPointer), 4 * SizeOf(Int8u));
              end;

            inc(TotalWeight, Weight);
            inc(HiRes.x, FRadiusXInv);
            inc(LoRes.x);
          until HiRes.x >= FilterSize;
        end
      else
        repeat
          Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HiRes.x
            * SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggDownscaleShift);

          inc(TotalWeight, Weight);
          inc(Fg[FOrder.r], Backup.r * Weight);
          inc(Fg[FOrder.g], Backup.g * Weight);
          inc(Fg[FOrder.b], Backup.b * Weight);
          inc(Fg[FOrder.a], Backup.a * Weight);
          inc(HiRes.x, FRadiusXInv);
        until HiRes.x >= FilterSize;

      inc(HiRes.y, FRadiusYInv);
      inc(LoRes.y);
    until HiRes.y >= FilterSize;

    Fg[0] := Fg[0] div TotalWeight;
    Fg[1] := Fg[1] div TotalWeight;
    Fg[2] := Fg[2] div TotalWeight;
    Fg[3] := Fg[3] div TotalWeight;

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
 
 
