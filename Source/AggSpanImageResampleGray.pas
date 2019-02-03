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
unit AggSpanImageResampleGray;

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
  TAggSpanImageResampleGrayAffine = class(TAggSpanImageResampleAffine)
  public
    constructor Create(Alloc: TAggSpanAllocator); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
      BackColor: PAggColor; Interpolator: TAggSpanInterpolator;
      Filter: TAggImageFilterLUT); overload;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanImageResampleGray = class(TAggSpanImageResample)
  public
    constructor Create(Alloc: TAggSpanAllocator); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
      BackColor: PAggColor; Interpolator: TAggSpanInterpolator;
      Filter: TAggImageFilterLUT); overload;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

implementation


{ TAggSpanImageResampleGrayAffine }

constructor TAggSpanImageResampleGrayAffine.Create
  (Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);
end;

constructor TAggSpanImageResampleGrayAffine.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor;
  Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT);
begin
  inherited Create(Alloc, Src, BackColor, Interpolator, Filter);
end;

function TAggSpanImageResampleGrayAffine.Generate;
var
  Fg, SourceAlpha, Diameter, FilterSize, TotalWeight, WeightY, Weight: Integer;

  IniLowResX, IniHighResX: Integer;
  radius, Max: TPointInteger;
  LowRes, HighRes: TPointInteger;

  BackV, BackA: Int8u;
  Span: PAggColor;
  ForeGroundPointer: PInt8u;
  WeightArray: PInt16;
begin
  Interpolator.SetBegin(x + FilterDeltaXDouble, y + FilterDeltaYDouble, Len);

  BackV := GetBackgroundColor.v;
  BackA := GetBackgroundColor.Rgba8.a;

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

    Fg := CAggImageFilterSize div 2;
    SourceAlpha := Fg;

    LowRes.y := ShrInt32(y, CAggImageSubpixelShift);
    HighRes.y := ShrInt32((CAggImageSubpixelMask - (y and CAggImageSubpixelMask)) *
      FRadiusYInv, CAggImageSubpixelShift);

    TotalWeight := 0;

    IniLowResX := ShrInt32(x, CAggImageSubpixelShift);
    IniHighResX := ShrInt32((CAggImageSubpixelMask - (x and CAggImageSubpixelMask)) *
      FRadiusXInv, CAggImageSubpixelShift);

    repeat
      WeightY := PInt16(PtrComp(WeightArray) + HighRes.y * SizeOf(Int16))^;

      LowRes.x := IniLowResX;
      HighRes.x := IniHighResX;

      if (LowRes.y >= 0) and (LowRes.y <= Max.y) then
        begin
          ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LowRes.y)) +
            LowRes.x * SizeOf(Int8u));

          repeat
            Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HighRes.x
              * SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggDownscaleShift);

            if (LowRes.x >= 0) and (LowRes.x <= Max.x) then
              begin
                inc(Fg, ForeGroundPointer^ * Weight);
                inc(SourceAlpha, CAggBaseMask * Weight);
              end
            else
              begin
                inc(Fg, BackV * Weight);
                inc(SourceAlpha, BackA * Weight);
              end;

            inc(TotalWeight, Weight);
            inc(HighRes.x, FRadiusXInv);
            inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
            inc(LowRes.x);
          until HighRes.x >= FilterSize;
        end
      else
        repeat
          Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HighRes.x
            * SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggDownscaleShift);

          inc(TotalWeight, Weight);
          inc(Fg, BackV * Weight);
          inc(SourceAlpha, BackA * Weight);
          inc(HighRes.x, FRadiusXInv);
        until HighRes.x >= FilterSize;

      inc(HighRes.y, FRadiusYInv);
      inc(LowRes.y);
    until HighRes.y >= FilterSize;

    Fg := Fg div TotalWeight;
    SourceAlpha := SourceAlpha div TotalWeight;

    if Fg < 0 then
        Fg := 0;

    if SourceAlpha < 0 then
        SourceAlpha := 0;

    if SourceAlpha > CAggBaseMask then
        SourceAlpha := CAggBaseMask;

    if Fg > SourceAlpha then
        Fg := SourceAlpha;

    Span.v := Int8u(Fg);
    Span.Rgba8.a := Int8u(SourceAlpha);

    inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

{ TAggSpanImageResampleGray }

constructor TAggSpanImageResampleGray.Create(Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);
end;

constructor TAggSpanImageResampleGray.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor;
  Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT);
begin
  inherited Create(Alloc, Src, BackColor, Interpolator, Filter);
end;

function TAggSpanImageResampleGray.Generate;
var
  Span: PAggColor;
  Fg, SourceAlpha, Diameter, TotalWeight: Integer;
  IniLowResX, IniHighResX: Integer;
  Weight, FilterSize, WeightY: Integer;

  radius, Max, LowRes, HighRes, RadiusInv: TPointInteger;

  BackV, BackA: Int8u;
  WeightArray: PInt16;
  ForeGroundPointer: PInt8u;
begin
  Span := Allocator.Span;

  Interpolator.SetBegin(x + FilterDeltaXDouble, y + FilterDeltaYDouble, Len);

  BackV := GetBackgroundColor.v;
  BackA := GetBackgroundColor.Rgba8.a;

  Diameter := Filter.Diameter;
  FilterSize := Diameter shl CAggImageSubpixelShift;

  WeightArray := Filter.WeightArray;

  repeat
    RadiusInv.x := CAggImageSubpixelSize;
    RadiusInv.y := CAggImageSubpixelSize;

    Interpolator.Coordinates(@x, @y);
    Interpolator.LocalScale(@radius.x, @radius.y);

    radius.x := ShrInt32(radius.x * FBlur.x, CAggImageSubpixelShift);
    radius.y := ShrInt32(radius.y * FBlur.y, CAggImageSubpixelShift);

    if radius.x < CAggImageSubpixelSize then
        radius.x := CAggImageSubpixelSize
    else
      begin
        if radius.x > CAggImageSubpixelSize * FScaleLimit then
            radius.x := CAggImageSubpixelSize * FScaleLimit;

        RadiusInv.x := CAggImageSubpixelSize * CAggImageSubpixelSize div radius.x;
      end;

    if radius.y < CAggImageSubpixelSize then
        radius.y := CAggImageSubpixelSize
    else
      begin
        if radius.y > CAggImageSubpixelSize * FScaleLimit then
            radius.y := CAggImageSubpixelSize * FScaleLimit;

        RadiusInv.y := CAggImageSubpixelSize * CAggImageSubpixelSize div radius.y;
      end;

    radius.x := ShrInt32(Diameter * radius.x, 1);
    radius.y := ShrInt32(Diameter * radius.y, 1);

    Max.x := SourceImage.width - 1;
    Max.y := SourceImage.height - 1;

    inc(x, FilterDeltaXInteger - radius.x);
    inc(y, FilterDeltaYInteger - radius.y);

    Fg := CAggImageFilterSize div 2;
    SourceAlpha := Fg;

    LowRes.y := ShrInt32(y, CAggImageSubpixelShift);
    HighRes.y := ShrInt32((CAggImageSubpixelMask - (y and CAggImageSubpixelMask)) *
      RadiusInv.y, CAggImageSubpixelShift);

    TotalWeight := 0;

    IniLowResX := ShrInt32(x, CAggImageSubpixelShift);
    IniHighResX := ShrInt32((CAggImageSubpixelMask - (x and CAggImageSubpixelMask)) *
      RadiusInv.x, CAggImageSubpixelShift);

    repeat
      WeightY := PInt16(PtrComp(WeightArray) + HighRes.y * SizeOf(Int16))^;

      LowRes.x := IniLowResX;
      HighRes.x := IniHighResX;

      if (LowRes.y >= 0) and (LowRes.y <= Max.y) then
        begin
          ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LowRes.y)) +
            LowRes.x * SizeOf(Int8u));

          repeat
            Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HighRes.x
              * SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggDownscaleShift);

            if (LowRes.x >= 0) and (LowRes.x <= Max.x) then
              begin
                inc(Fg, ForeGroundPointer^ * Weight);
                inc(SourceAlpha, CAggBaseMask * Weight);
              end
            else
              begin
                inc(Fg, BackV * Weight);
                inc(SourceAlpha, BackA * Weight);
              end;

            inc(TotalWeight, Weight);
            inc(HighRes.x, RadiusInv.x);
            inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
            inc(LowRes.x);
          until HighRes.x >= FilterSize;
        end
      else
        repeat
          Weight := ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HighRes.x
            * SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggDownscaleShift);

          inc(TotalWeight, Weight);
          inc(Fg, BackV * Weight);
          inc(SourceAlpha, BackA * Weight);
          inc(HighRes.x, RadiusInv.x);
        until HighRes.x >= FilterSize;

      inc(HighRes.y, RadiusInv.y);
      inc(LowRes.y);

    until HighRes.y >= FilterSize;

    Fg := Fg div TotalWeight;
    SourceAlpha := SourceAlpha div TotalWeight;

    if Fg < 0 then
        Fg := 0;

    if SourceAlpha < 0 then
        SourceAlpha := 0;

    if SourceAlpha > CAggBaseMask then
        SourceAlpha := CAggBaseMask;

    if Fg > SourceAlpha then
        Fg := SourceAlpha;

    Span.v := Int8u(Fg);
    Span.Rgba8.a := Int8u(SourceAlpha);

    inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

end. 
 
 
