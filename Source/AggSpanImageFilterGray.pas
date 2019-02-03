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
unit AggSpanImageFilterGray;

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
  TAggSpanImageFilterGrayNN = class(TAggSpanImageFilter)
  public
    constructor Create(Alloc: TAggSpanAllocator); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer; BackColor: PAggColor; inter: TAggSpanInterpolator); overload;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanImageFilterGrayBilinear = class(TAggSpanImageFilter)
  public
    constructor Create(Alloc: TAggSpanAllocator); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer; BackColor: PAggColor; inter: TAggSpanInterpolator); overload;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanImageFilterGray2x2 = class(TAggSpanImageFilter)
  public
    constructor Create(Alloc: TAggSpanAllocator); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer; BackColor: PAggColor; inter: TAggSpanInterpolator; Filter: TAggImageFilterLUT); overload;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanImageFilterGray = class(TAggSpanImageFilter)
  public
    constructor Create(Alloc: TAggSpanAllocator); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer; BackColor: PAggColor; inter: TAggSpanInterpolator; Filter: TAggImageFilterLUT); overload;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

implementation


{ TAggSpanImageFilterGrayNN }

constructor TAggSpanImageFilterGrayNN.Create(Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);
end;

constructor TAggSpanImageFilterGrayNN.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor;
  inter: TAggSpanInterpolator);
begin
  inherited Create(Alloc, Src, BackColor, inter, nil);
end;

function TAggSpanImageFilterGrayNN.Generate(x, y: Integer; Len: Cardinal): PAggColor;
var
  Max: TPointInteger;
  Fg, SourceAlpha: Cardinal;
  Span: PAggColor;
begin
  Interpolator.SetBegin(x + FilterDeltaXDouble, y + FilterDeltaYDouble, Len);

  Span := Allocator.Span;

  Max := PointInteger(SourceImage.width - 1, SourceImage.height - 1);

  repeat
    Interpolator.Coordinates(@x, @y);

    x := ShrInt32(x, CAggImageSubpixelShift);
    y := ShrInt32(y, CAggImageSubpixelShift);

    if (x >= 0) and (y >= 0) and (x <= Max.x) and (y <= Max.y) then
      begin
        Fg := PInt8u(PtrComp(SourceImage.Row(y)) + x * SizeOf(Int8u))^;

        SourceAlpha := CAggBaseMask;
      end
    else
      begin
        Fg := GetBackgroundColor.v;

        SourceAlpha := GetBackgroundColor.Rgba8.a;
      end;

    Span.v := Int8u(Fg);
    Span.Rgba8.a := Int8u(SourceAlpha);

    inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

{ TAggSpanImageFilterGrayBilinear }

constructor TAggSpanImageFilterGrayBilinear.Create
  (Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);
end;

constructor TAggSpanImageFilterGrayBilinear.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor; inter: TAggSpanInterpolator);
begin
  inherited Create(Alloc, Src, BackColor, inter, nil);
end;

function TAggSpanImageFilterGrayBilinear.Generate(x, y: Integer; Len: Cardinal): PAggColor;
var
  Fg, SourceAlpha, Weight: Cardinal;
  BackV, BackAlpha: Int8u;
  ForeGroundPointer: PInt8u;
  Span: PAggColor;
  Max, HiRes, LoRes: TPointInteger;
begin
  Interpolator.SetBegin(x + FilterDeltaXDouble, y + FilterDeltaYDouble, Len);

  BackV := GetBackgroundColor.v;
  BackAlpha := GetBackgroundColor.Rgba8.a;

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
        Fg := CAggImageSubpixelSize * CAggImageSubpixelSize div 2;

        HiRes.x := HiRes.x and CAggImageSubpixelMask;
        HiRes.y := HiRes.y and CAggImageSubpixelMask;

        ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) + LoRes.x *
          SizeOf(Int8u));

        inc(Fg, ForeGroundPointer^ * (CAggImageSubpixelSize - HiRes.x) *
          (CAggImageSubpixelSize - HiRes.y));
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

        inc(Fg, ForeGroundPointer^ * (CAggImageSubpixelSize - HiRes.y) * HiRes.x);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

        ForeGroundPointer := SourceImage.NextRow(PInt8u(PtrComp(ForeGroundPointer) - 2));

        inc(Fg, ForeGroundPointer^ * (CAggImageSubpixelSize - HiRes.x) * HiRes.y);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

        inc(Fg, ForeGroundPointer^ * HiRes.x * HiRes.y);
        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

        Fg := Fg shr (CAggImageSubpixelShift * 2);

        SourceAlpha := CAggBaseMask;
      end
    else
      begin
        if (LoRes.x < -1) or (LoRes.y < -1) or (LoRes.x > Max.x) or (LoRes.y > Max.y) then
          begin
            Fg := BackV;
            SourceAlpha := BackAlpha;
          end
        else
          begin
            Fg := CAggImageSubpixelSize * CAggImageSubpixelSize div 2;
            SourceAlpha := Fg;

            HiRes.x := HiRes.x and CAggImageSubpixelMask;
            HiRes.y := HiRes.y and CAggImageSubpixelMask;

            Weight := (CAggImageSubpixelSize - HiRes.x) * (CAggImageSubpixelSize - HiRes.y);

            if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x <= Max.x) and (LoRes.y <= Max.y)
            then
              begin
                inc(Fg, Weight * PInt8u(PtrComp(SourceImage.Row(LoRes.y)) + LoRes.x *
                  SizeOf(Int8u))^);
                inc(SourceAlpha, Weight * CAggBaseMask);
              end
            else
              begin
                inc(Fg, BackV * Weight);
                inc(SourceAlpha, BackAlpha * Weight);
              end;

            inc(LoRes.x);

            Weight := HiRes.x * (CAggImageSubpixelSize - HiRes.y);

            if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x <= Max.x) and (LoRes.y <= Max.y)
            then
              begin
                inc(Fg, Weight * PInt8u(PtrComp(SourceImage.Row(LoRes.y)) + LoRes.x *
                  SizeOf(Int8u))^);
                inc(SourceAlpha, Weight * CAggBaseMask);
              end
            else
              begin
                inc(Fg, BackV * Weight);
                inc(SourceAlpha, BackAlpha * Weight);
              end;

            dec(LoRes.x);
            inc(LoRes.y);

            Weight := (CAggImageSubpixelSize - HiRes.x) * HiRes.y;

            if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x <= Max.x) and (LoRes.y <= Max.y)
            then
              begin
                inc(Fg, Weight * PInt8u(PtrComp(SourceImage.Row(LoRes.y)) + LoRes.x *
                  SizeOf(Int8u))^);
                inc(SourceAlpha, Weight * CAggBaseMask);
              end
            else
              begin
                inc(Fg, BackV * Weight);
                inc(SourceAlpha, BackAlpha * Weight);
              end;

            inc(LoRes.x);

            Weight := HiRes.x * HiRes.y;

            if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x <= Max.x) and (LoRes.y <= Max.y)
            then
              begin
                inc(Fg, Weight * PInt8u(PtrComp(SourceImage.Row(LoRes.y)) + LoRes.x *
                  SizeOf(Int8u))^);
                inc(SourceAlpha, Weight * CAggBaseMask);
              end
            else
              begin
                inc(Fg, BackV * Weight);
                inc(SourceAlpha, BackAlpha * Weight);
              end;

            Fg := Fg shr (CAggImageSubpixelShift * 2);
            SourceAlpha := SourceAlpha shr (CAggImageSubpixelShift * 2);
          end;
      end;

    Span.v := Int8u(Fg);
    Span.Rgba8.a := Int8u(SourceAlpha);

    inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

{ TAggSpanImageFilterGray2x2 }

constructor TAggSpanImageFilterGray2x2.Create(Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);
end;

constructor TAggSpanImageFilterGray2x2.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor; inter: TAggSpanInterpolator;
  Filter: TAggImageFilterLUT);
begin
  inherited Create(Alloc, Src, BackColor, inter, Filter);
end;

function TAggSpanImageFilterGray2x2.Generate(x, y: Integer; Len: Cardinal): PAggColor;
var
  Fg, SourceAlpha, Weight: Cardinal;
  BackV, BackAlpha: Int8u;
  ForeGroundPointer: PInt8u;
  Span: PAggColor;
  WeightArray: PInt16;
  Max, HiRes, LoRes: TPointInteger;
begin
  Interpolator.SetBegin(x + FilterDeltaXDouble, y + FilterDeltaYDouble, Len);

  BackV := GetBackgroundColor.v;
  BackAlpha := GetBackgroundColor.Rgba8.a;

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

    if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x < Max.x) and (LoRes.y < Max.y) then
      begin
        Fg := CAggImageFilterSize div 2;

        HiRes.x := HiRes.x and CAggImageSubpixelMask;
        HiRes.y := HiRes.y and CAggImageSubpixelMask;

        ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) + LoRes.x *
          SizeOf(Int8u));

        inc(Fg, ForeGroundPointer^ * ShrInt32(PInt16(PtrComp(WeightArray) +
          (HiRes.x + CAggImageSubpixelSize) * SizeOf(Int16))^ *
          PInt16(PtrComp(WeightArray) + (HiRes.y + CAggImageSubpixelSize) *
          SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift));

        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

        inc(Fg, ForeGroundPointer^ * ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.x *
          SizeOf(Int16))^ * PInt16(PtrComp(WeightArray) +
          (HiRes.y + CAggImageSubpixelSize) * SizeOf(Int16))^ +
          CAggImageFilterSize div 2, CAggImageFilterShift));

        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

        ForeGroundPointer := SourceImage.NextRow
          (PInt8u(PtrComp(ForeGroundPointer) - 2 * SizeOf(Int8u)));

        inc(Fg, ForeGroundPointer^ * ShrInt32(PInt16(PtrComp(WeightArray) +
          (HiRes.x + CAggImageSubpixelSize) * SizeOf(Int16))^ *
          PInt16(PtrComp(WeightArray) + HiRes.y * SizeOf(Int16))^ +
          CAggImageFilterSize div 2, CAggImageFilterShift));

        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

        inc(Fg, ForeGroundPointer^ * ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.x *
          SizeOf(Int16))^ * PInt16(PtrComp(WeightArray) + HiRes.y * SizeOf(Int16)
          )^ + CAggImageFilterSize div 2, CAggImageFilterShift));

        inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));

        Fg := Fg shr CAggImageFilterShift;

        if Fg > CAggBaseMask then
            Fg := CAggBaseMask;

        SourceAlpha := CAggBaseMask;

      end
    else
      begin
        if (LoRes.x < -1) or (LoRes.y < -1) or (LoRes.x > Max.x) or (LoRes.y > Max.y) then
          begin
            Fg := BackV;
            SourceAlpha := BackAlpha;
          end
        else
          begin
            Fg := CAggImageFilterSize div 2;
            SourceAlpha := Fg;

            HiRes.x := HiRes.x and CAggImageSubpixelMask;
            HiRes.y := HiRes.y and CAggImageSubpixelMask;

            Weight := ShrInt32(PInt16(PtrComp(WeightArray) +
              (HiRes.x + CAggImageSubpixelSize) * SizeOf(Int16))^ *
              PInt16(PtrComp(WeightArray) + (HiRes.y + CAggImageSubpixelSize) *
              SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

            if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x <= Max.x) and (LoRes.y <= Max.y)
            then
              begin
                inc(Fg, Weight * PInt8u(PtrComp(SourceImage.Row(LoRes.y)) + LoRes.x *
                  SizeOf(Int8u))^);
                inc(SourceAlpha, Weight * CAggBaseMask);
              end
            else
              begin
                inc(Fg, BackV * Weight);
                inc(SourceAlpha, BackAlpha * Weight);
              end;

            inc(LoRes.x);

            Weight := ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.x *
              SizeOf(Int16))^ * PInt16(PtrComp(WeightArray) +
              (HiRes.y + CAggImageSubpixelSize) * SizeOf(Int16))^ +
              CAggImageFilterSize div 2, CAggImageFilterShift);

            if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x <= Max.x) and (LoRes.y <= Max.y)
            then
              begin
                inc(Fg, Weight * PInt8u(PtrComp(SourceImage.Row(LoRes.y)) + LoRes.x *
                  SizeOf(Int8u))^);
                inc(SourceAlpha, Weight * CAggBaseMask);
              end
            else
              begin
                inc(Fg, BackV * Weight);
                inc(SourceAlpha, BackAlpha * Weight);
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
                inc(Fg, Weight * PInt8u(PtrComp(SourceImage.Row(LoRes.y)) + LoRes.x *
                  SizeOf(Int8u))^);
                inc(SourceAlpha, Weight * CAggBaseMask);
              end
            else
              begin
                inc(Fg, BackV * Weight);
                inc(SourceAlpha, BackAlpha * Weight);
              end;

            inc(LoRes.x);

            Weight := ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.x *
              SizeOf(Int16))^ * PInt16(PtrComp(WeightArray) + HiRes.y *
              SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift);

            if (LoRes.x >= 0) and (LoRes.y >= 0) and (LoRes.x <= Max.x) and (LoRes.y <= Max.y)
            then
              begin
                inc(Fg, Weight * PInt8u(PtrComp(SourceImage.Row(LoRes.y)) + LoRes.x *
                  SizeOf(Int8u))^);
                inc(SourceAlpha, Weight * CAggBaseMask);
              end
            else
              begin
                inc(Fg, BackV * Weight);
                inc(SourceAlpha, BackAlpha * Weight);
              end;

            Fg := Fg shr CAggImageFilterShift;
            SourceAlpha := SourceAlpha shr CAggImageFilterShift;

            if SourceAlpha > CAggBaseMask then
                SourceAlpha := CAggBaseMask;

            if Fg > SourceAlpha then
                Fg := SourceAlpha;
          end;
      end;

    Span.v := Int8u(Fg);
    Span.Rgba8.a := Int8u(SourceAlpha);

    inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

{ TAggSpanImageFilterGray }

constructor TAggSpanImageFilterGray.Create(Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);
end;

constructor TAggSpanImageFilterGray.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor; inter: TAggSpanInterpolator;
  Filter: TAggImageFilterLUT);
begin
  inherited Create(Alloc, Src, BackColor, inter, Filter);
end;

function TAggSpanImageFilterGray.Generate(x, y: Integer;
  Len: Cardinal): PAggColor;
var
  Fg, SourceAlpha, Start, Start1, FractX, Weight, CountX, WeightY: Integer;
  Max, Max2, HiRes, LoRes: TPointInteger;
  BackV, BackAlpha: Int8u;
  ForeGroundPointer: PInt8u;
  Diameter, CountY: Cardinal;
  WeightArray: PInt16;
  Span: PAggColor;
begin
  Interpolator.SetBegin(x + FilterDeltaXDouble, y + FilterDeltaYDouble, Len);

  BackV := GetBackgroundColor.v;
  BackAlpha := GetBackgroundColor.Rgba8.a;

  Diameter := Filter.Diameter;
  Start := Filter.Start;
  Start1 := Start - 1;
  WeightArray := Filter.WeightArray;

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

    Fg := CAggImageFilterSize div 2;

    FractX := HiRes.x and CAggImageSubpixelMask;
    CountY := Diameter;

    if (LoRes.x >= -Start) and (LoRes.y >= -Start) and (LoRes.x <= Max.x) and
      (LoRes.y <= Max.y) then
      begin
        HiRes.y := CAggImageSubpixelMask - (HiRes.y and CAggImageSubpixelMask);
        ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y + Start)) +
          (LoRes.x + Start) * SizeOf(Int8u));

        repeat
          CountX := Diameter;
          WeightY := PInt16(PtrComp(WeightArray) + HiRes.y * SizeOf(Int16))^;
          HiRes.x := CAggImageSubpixelMask - FractX;

          repeat
            inc(Fg, ForeGroundPointer^ * ShrInt32(WeightY * PInt16(PtrComp(WeightArray)
              + HiRes.x * SizeOf(Int16))^ + CAggImageFilterSize div 2,
              CAggImageFilterShift));

            inc(HiRes.x, CAggImageSubpixelSize);
            dec(CountX);
          until CountX = 0;

          inc(HiRes.y, CAggImageSubpixelSize);

          ForeGroundPointer := SourceImage.NextRow(PInt8u(PtrComp(ForeGroundPointer) - Diameter));

          dec(CountY);
        until CountY = 0;

        Fg := ShrInt32(Fg, CAggImageFilterShift);

        if Fg < 0 then
            Fg := 0;

        if Fg > CAggBaseMask then
            Fg := CAggBaseMask;

        SourceAlpha := CAggBaseMask;
      end
    else
      begin
        if (LoRes.x < Start1) or (LoRes.y < Start1) or (LoRes.x > Max2.x) or (LoRes.y > Max2.y)
        then
          begin
            Fg := BackV;
            SourceAlpha := BackAlpha;
          end
        else
          begin
            SourceAlpha := CAggImageFilterSize div 2;

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
                    ForeGroundPointer := PInt8u(PtrComp(SourceImage.Row(LoRes.y)) + LoRes.x *
                      SizeOf(Int8u));

                    inc(Fg, ForeGroundPointer^ * Weight);
                    inc(PtrComp(ForeGroundPointer), SizeOf(Int8u));
                    inc(SourceAlpha, CAggBaseMask * Weight);
                  end
                else
                  begin
                    inc(Fg, BackV * Weight);
                    inc(SourceAlpha, BackAlpha * Weight);
                  end;

                inc(HiRes.x, CAggImageSubpixelSize);
                inc(LoRes.x);
                dec(CountX);
              until CountX = 0;

              inc(HiRes.y, CAggImageSubpixelSize);
              inc(LoRes.y);
              dec(CountY);
            until CountY = 0;

            Fg := ShrInt32(Fg, CAggImageFilterShift);
            SourceAlpha := ShrInt32(SourceAlpha, CAggImageFilterShift);

            if Fg < 0 then
                Fg := 0;

            if SourceAlpha < 0 then
                SourceAlpha := 0;

            if SourceAlpha > CAggBaseMask then
                SourceAlpha := CAggBaseMask;

            if Fg > SourceAlpha then
                Fg := SourceAlpha;
          end;
      end;

    Span.v := Int8u(Fg);
    Span.Rgba8.a := Int8u(SourceAlpha);

    inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

end. 
 
 
