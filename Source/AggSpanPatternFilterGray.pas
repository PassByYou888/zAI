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
unit AggSpanPatternFilterGray;

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
  TAggSpanPatternFilterGrayNN = class(TAggSpanImageFilter)
  private
    FWrapModeX, FWrapModeY: TAggWrapMode;
  protected
    procedure SetSourceImage(Src: TAggRenderingBuffer); override;
  public
    constructor Create(Alloc: TAggSpanAllocator; Wx, Wy: TAggWrapMode); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer; Interpolator: TAggSpanInterpolator; Wx, Wy: TAggWrapMode); overload;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanPatternFilterGrayBilinear = class(TAggSpanImageFilter)
  private
    FWrapModeX, FWrapModeY: TAggWrapMode;
  protected
    procedure SetSourceImage(Src: TAggRenderingBuffer); override;
  public
    constructor Create(Alloc: TAggSpanAllocator; Wx, Wy: TAggWrapMode); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer; Interpolator: TAggSpanInterpolator; Wx, Wy: TAggWrapMode); overload;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanPatternFilterGray2x2 = class(TAggSpanImageFilter)
  private
    FWrapModeX, FWrapModeY: TAggWrapMode;
  protected
    procedure SetSourceImage(Src: TAggRenderingBuffer); override;
  public
    constructor Create(Alloc: TAggSpanAllocator; Wx, Wy: TAggWrapMode); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer; Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT; Wx, Wy: TAggWrapMode); overload;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggSpanPatternFilterGray = class(TAggSpanImageFilter)
  private
    FWrapModeX, FWrapModeY: TAggWrapMode;
  protected
    procedure SetSourceImage(Src: TAggRenderingBuffer); override;
  public
    constructor Create(Alloc: TAggSpanAllocator; Wx, Wy: TAggWrapMode); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer; Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT; Wx, Wy: TAggWrapMode); overload;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;
  end;

implementation


{ TAggSpanPatternFilterGrayNN }

constructor TAggSpanPatternFilterGrayNN.Create(Alloc: TAggSpanAllocator;
  Wx, Wy: TAggWrapMode);
begin
  inherited Create(Alloc);

  FWrapModeX := Wx;
  FWrapModeY := Wy;

  FWrapModeX.Init(1);
  FWrapModeY.Init(1);
end;

constructor TAggSpanPatternFilterGrayNN.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; Interpolator: TAggSpanInterpolator;
  Wx, Wy: TAggWrapMode);
var
  RGBA: TAggColor;

begin
  RGBA.Clear;

  inherited Create(Alloc, Src, @RGBA, Interpolator, nil);

  FWrapModeX := Wx;
  FWrapModeY := Wy;

  FWrapModeX.Init(Src.width);
  FWrapModeY.Init(Src.height);
end;

procedure TAggSpanPatternFilterGrayNN.SetSourceImage;
begin
  inherited SetSourceImage(Src);

  FWrapModeX.Init(Src.width);
  FWrapModeY.Init(Src.height);
end;

function TAggSpanPatternFilterGrayNN.Generate(x, y: Integer; Len: Cardinal): PAggColor;
var
  Span: PAggColor;
begin
  Interpolator.SetBegin(x + FilterDeltaXDouble, y + FilterDeltaYDouble, Len);

  Span := Allocator.Span;

  repeat
    Interpolator.Coordinates(@x, @y);

    x := FWrapModeX.FuncOperator(ShrInt32(x, CAggImageSubpixelShift));
    y := FWrapModeY.FuncOperator(ShrInt32(y, CAggImageSubpixelShift));

    Span.v := PInt8u(PtrComp(SourceImage.Row(y)) + x * SizeOf(Int8u))^;
    Span.Rgba8.a := CAggBaseMask;

    inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

{ TAggSpanPatternFilterGrayBilinear }

constructor TAggSpanPatternFilterGrayBilinear.Create
  (Alloc: TAggSpanAllocator; Wx, Wy: TAggWrapMode);
begin
  inherited Create(Alloc);

  FWrapModeX := Wx;
  FWrapModeY := Wy;

  FWrapModeX.Init(1);
  FWrapModeY.Init(1);
end;

constructor TAggSpanPatternFilterGrayBilinear.Create
  (Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
  Interpolator: TAggSpanInterpolator; Wx, Wy: TAggWrapMode);
var
  RGBA: TAggColor;

begin
  RGBA.Clear;

  inherited Create(Alloc, Src, @RGBA, Interpolator, nil);

  FWrapModeX := Wx;
  FWrapModeY := Wy;

  FWrapModeX.Init(Src.width);
  FWrapModeY.Init(Src.height);
end;

procedure TAggSpanPatternFilterGrayBilinear.SetSourceImage;
begin
  inherited SetSourceImage(Src);

  FWrapModeX.Init(Src.width);
  FWrapModeY.Init(Src.height);
end;

function TAggSpanPatternFilterGrayBilinear.Generate(x, y: Integer; Len: Cardinal): PAggColor;
var
  Fg, x1, x2, y1, y2: Cardinal;

  HiRes, LoRes: TPointInteger;

  Span: PAggColor;

  Ptr1, Ptr2: PInt8u;
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

    y1 := FWrapModeY.FuncOperator(LoRes.y);
    y2 := FWrapModeY.IncOperator;

    Ptr1 := SourceImage.Row(y1);
    Ptr2 := SourceImage.Row(y2);

    Fg := CAggImageSubpixelSize * CAggImageSubpixelSize div 2;

    HiRes.x := HiRes.x and CAggImageSubpixelMask;
    HiRes.y := HiRes.y and CAggImageSubpixelMask;

    inc(Fg, PInt8u(PtrComp(Ptr1) + x1 * SizeOf(Int8u))^ *
      (CAggImageSubpixelSize - HiRes.x) * (CAggImageSubpixelSize - HiRes.y));
    inc(Fg, PInt8u(PtrComp(Ptr1) + x2 * SizeOf(Int8u))^ * HiRes.x *
      (CAggImageSubpixelSize - HiRes.y));
    inc(Fg, PInt8u(PtrComp(Ptr2) + x1 * SizeOf(Int8u))^ *
      (CAggImageSubpixelSize - HiRes.x) * HiRes.y);
    inc(Fg, PInt8u(PtrComp(Ptr2) + x2 * SizeOf(Int8u))^ * HiRes.x * HiRes.y);

    Span.v := Int8u(Fg shr (CAggImageSubpixelShift * 2));
    Span.Rgba8.a := CAggBaseMask;

    inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

{ TAggSpanPatternFilterGray2x2 }

constructor TAggSpanPatternFilterGray2x2.Create(Alloc: TAggSpanAllocator;
  Wx, Wy: TAggWrapMode);
begin
  inherited Create(Alloc);

  FWrapModeX := Wx;
  FWrapModeY := Wy;

  FWrapModeX.Init(1);
  FWrapModeY.Init(1);
end;

constructor TAggSpanPatternFilterGray2x2.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; Interpolator: TAggSpanInterpolator;
  Filter: TAggImageFilterLUT; Wx, Wy: TAggWrapMode);
var
  RGBA: TAggColor;

begin
  RGBA.Clear;

  inherited Create(Alloc, Src, @RGBA, Interpolator, Filter);

  FWrapModeX := Wx;
  FWrapModeY := Wy;

  FWrapModeX.Init(Src.width);
  FWrapModeY.Init(Src.height);
end;

procedure TAggSpanPatternFilterGray2x2.SetSourceImage(Src: TAggRenderingBuffer);
begin
  inherited SetSourceImage(Src);

  FWrapModeX.Init(Src.width);
  FWrapModeY.Init(Src.height);
end;

function TAggSpanPatternFilterGray2x2.Generate(x, y: Integer; Len: Cardinal): PAggColor;
var
  Fg, x1, x2, y1, y2: Cardinal;

  HiRes, LoRes: TPointInteger;

  Ptr1, Ptr2: PInt8u;

  Span: PAggColor;

  WeightArray: PInt16;
begin
  Interpolator.SetBegin(x + FilterDeltaXDouble, y + FilterDeltaYDouble, Len);

  Span := Allocator.Span;

  WeightArray := PInt16(PtrComp(Filter.WeightArray) +
    ShrInt32(Filter.Diameter div 2 - 1, CAggImageSubpixelShift));

  repeat
    Interpolator.Coordinates(@HiRes.x, @HiRes.y);

    inc(HiRes.x, FilterDeltaXInteger);
    inc(HiRes.y, FilterDeltaYInteger);

    LoRes.x := ShrInt32(HiRes.x, CAggImageSubpixelShift);
    LoRes.y := ShrInt32(HiRes.y, CAggImageSubpixelShift);

    x1 := FWrapModeX.FuncOperator(LoRes.x);
    x2 := FWrapModeX.IncOperator;

    y1 := FWrapModeY.FuncOperator(LoRes.y);
    y2 := FWrapModeY.IncOperator;

    Ptr1 := SourceImage.Row(y1);
    Ptr2 := SourceImage.Row(y2);

    Fg := CAggImageFilterSize div 2;

    HiRes.x := HiRes.x and CAggImageSubpixelMask;
    HiRes.y := HiRes.y and CAggImageSubpixelMask;

    inc(Fg, PInt8u(PtrComp(Ptr1) + x1 * SizeOf(Int8u))^ *
      ShrInt32(PInt16(PtrComp(WeightArray) + (HiRes.x + CAggImageSubpixelSize) *
      SizeOf(Int16))^ * PInt16(PtrComp(WeightArray) +
      (HiRes.y + CAggImageSubpixelSize) * SizeOf(Int16))^ + CAggImageFilterSize div 2,
      CAggImageFilterShift));

    inc(Fg, PInt8u(PtrComp(Ptr1) + x2 * SizeOf(Int8u))^ *
      ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.x * SizeOf(Int16))^ *
      PInt16(PtrComp(WeightArray) + (HiRes.y + CAggImageSubpixelSize) *
      SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift));

    inc(Fg, PInt8u(PtrComp(Ptr2) + x1 * SizeOf(Int8u))^ *
      ShrInt32(PInt16(PtrComp(WeightArray) + (HiRes.x + CAggImageSubpixelSize) *
      SizeOf(Int16))^ * PInt16(PtrComp(WeightArray) + HiRes.y * SizeOf(Int16))^
      + CAggImageFilterSize div 2, CAggImageFilterShift));

    inc(Fg, PInt8u(PtrComp(Ptr2) + x2 * SizeOf(Int8u))^ *
      ShrInt32(PInt16(PtrComp(WeightArray) + HiRes.x * SizeOf(Int16))^ *
      PInt16(PtrComp(WeightArray) + HiRes.y * SizeOf(Int16))^ +
      CAggImageFilterSize div 2, CAggImageFilterShift));

    Fg := Fg shr CAggImageFilterShift;

    if Fg > CAggBaseMask then
        Fg := CAggBaseMask;

    Span.v := Int8u(Fg);
    Span.Rgba8.a := CAggBaseMask;

    inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    dec(Len);

  until Len = 0;

  Result := Allocator.Span;
end;

{ TAggSpanPatternFilterGray }

constructor TAggSpanPatternFilterGray.Create(Alloc: TAggSpanAllocator;
  Wx, Wy: TAggWrapMode);
begin
  inherited Create(Alloc);

  FWrapModeX := Wx;
  FWrapModeY := Wy;

  FWrapModeX.Init(1);
  FWrapModeY.Init(1);
end;

constructor TAggSpanPatternFilterGray.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; Interpolator: TAggSpanInterpolator;
  Filter: TAggImageFilterLUT; Wx, Wy: TAggWrapMode);
var
  RGBA: TAggColor;

begin
  RGBA.Clear;

  inherited Create(Alloc, Src, @RGBA, Interpolator, Filter);

  FWrapModeX := Wx;
  FWrapModeY := Wy;

  FWrapModeX.Init(Src.width);
  FWrapModeY.Init(Src.height);
end;

procedure TAggSpanPatternFilterGray.SetSourceImage(Src: TAggRenderingBuffer);
begin
  inherited SetSourceImage(Src);

  FWrapModeX.Init(Src.width);
  FWrapModeY.Init(Src.height);
end;

function TAggSpanPatternFilterGray.Generate(x, y: Integer; Len: Cardinal): PAggColor;
var
  Fg, Start, CountX, WeightY, FractX, XInt: Integer;
  HiRes, LoRes: TPointInteger;
  Diameter, Y_count: Cardinal;
  RowPointer: PInt8u;
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
    HiRes.y := y;

    FractX := HiRes.x and CAggImageSubpixelMask;
    Y_count := Diameter;

    LoRes.y := FWrapModeY.FuncOperator
      (ShrInt32(y, CAggImageSubpixelShift) + Start);
    XInt := ShrInt32(x, CAggImageSubpixelShift) + Start;

    HiRes.y := CAggImageSubpixelMask - (HiRes.y and CAggImageSubpixelMask);
    Fg := CAggImageFilterSize div 2;

    repeat
      CountX := Diameter;
      WeightY := PInt16(PtrComp(WeightArray) + HiRes.y * SizeOf(Int16))^;

      HiRes.x := CAggImageSubpixelMask - FractX;
      LoRes.x := FWrapModeX.FuncOperator(XInt);

      RowPointer := SourceImage.Row(LoRes.y);

      repeat
        inc(Fg, PInt8u(PtrComp(RowPointer) + LoRes.x * SizeOf(Int8u))^ *
          ShrInt32(WeightY * PInt16(PtrComp(WeightArray) + HiRes.x *
          SizeOf(Int16))^ + CAggImageFilterSize div 2, CAggImageFilterShift));

        inc(HiRes.x, CAggImageSubpixelSize);

        LoRes.x := FWrapModeX.IncOperator;

        dec(CountX);

      until CountX = 0;

      inc(HiRes.y, CAggImageSubpixelSize);

      LoRes.y := FWrapModeY.IncOperator;

      dec(Y_count);

    until Y_count = 0;

    Fg := ShrInt32(Fg, CAggImageFilterShift);

    if Fg < 0 then
        Fg := 0;

    if Fg > CAggBaseMask then
        Fg := CAggBaseMask;

    Span.v := Int8u(Fg);
    Span.Rgba8.a := CAggBaseMask;

    inc(PtrComp(Span), SizeOf(TAggColor));

    Interpolator.IncOperator;

    dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

end. 
 
 
