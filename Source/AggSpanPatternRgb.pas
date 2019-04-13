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
unit AggSpanPatternRgb;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggColor32,
  AggPixelFormat,
  AggPixelFormatRgb,
  AggSpanPattern,
  AggSpanAllocator,
  AggRenderingBuffer;

const
  CAggBaseShift = AggColor32.CAggBaseShift;
  CAggBaseMask  = AggColor32.CAggBaseMask;

type
  TAggSpanPatternRgb = class(TAggSpanPatternBase)
  private
    FWrapModeX, FWrapModeY: TAggWrapMode;
    FOrder: TAggOrder;
  protected
    procedure SetSourceImage(Src: TAggRenderingBuffer); override;
  public
    constructor Create(Alloc: TAggSpanAllocator; Wx, Wy: TAggWrapMode; Order: TAggOrder); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer; OffsetX, OffsetY: Cardinal; Wx, Wy: TAggWrapMode; Order: TAggOrder; alpha: Int8u = CAggBaseMask); overload;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; virtual;
  end;

implementation


{ TAggSpanPatternRgb }

constructor TAggSpanPatternRgb.Create(Alloc: TAggSpanAllocator;
  Wx, Wy: TAggWrapMode; Order: TAggOrder);
begin
  inherited Create(Alloc);

  FOrder := Order;

  FWrapModeX := Wx;
  FWrapModeY := Wy;

  FWrapModeX.Init(1);
  FWrapModeY.Init(1);
end;

constructor TAggSpanPatternRgb.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; OffsetX, OffsetY: Cardinal;
  Wx, Wy: TAggWrapMode; Order: TAggOrder; alpha: Int8u = CAggBaseMask);
begin
  inherited Create(Alloc, Src, OffsetX, OffsetY, alpha);

  FOrder := Order;

  FWrapModeX := Wx;
  FWrapModeY := Wy;

  FWrapModeX.Init(Src.width);
  FWrapModeY.Init(Src.height);
end;

procedure TAggSpanPatternRgb.SetSourceImage(Src: TAggRenderingBuffer);
begin
  inherited SetSourceImage(Src);

  FWrapModeX.Init(Src.width);
  FWrapModeY.Init(Src.height);
end;

function TAggSpanPatternRgb.Generate(x, y: Integer; Len: Cardinal): PAggColor;
var
  Span: PAggColor;
  SX: Cardinal;
  RowPointer, p: PInt8u;
begin
  Span := Allocator.Span;
  SX := FWrapModeX.FuncOperator(OffsetX + x);

  RowPointer := GetSourceImage.Row(FWrapModeY.FuncOperator(OffsetY + y));

  repeat
    p := PInt8u(PtrComp(RowPointer) + (SX + SX + SX) * SizeOf(Int8u));

    Span.Rgba8.r := PInt8u(PtrComp(p) + FOrder.r * SizeOf(Int8u))^;
    Span.Rgba8.g := PInt8u(PtrComp(p) + FOrder.g * SizeOf(Int8u))^;
    Span.Rgba8.b := PInt8u(PtrComp(p) + FOrder.b * SizeOf(Int8u))^;
    Span.Rgba8.a := GetAlphaInt;

    SX := FWrapModeX.IncOperator;

    inc(PtrComp(Span), SizeOf(TAggColor));
    dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

end. 
 
 
