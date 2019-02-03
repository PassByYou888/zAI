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
unit AggPatternFiltersRgba;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggLineAABasics,
  AggColor32;

type
  TAggPatternFilter = class
  public
    function Dilation: Cardinal; virtual; abstract;
    procedure PixelLowResolution(Buf: Pointer; p: PAggColor; x, y: Integer); virtual; abstract;
    procedure PixelHighResolution(Buf: Pointer; p: PAggColor; x, y: Integer); virtual; abstract;
  end;

  TAggPatternFilterNN = class(TAggPatternFilter)
  public
    function Dilation: Cardinal; override;
    procedure PixelLowResolution(Buf: Pointer; p: PAggColor; x, y: Integer); override;
    procedure PixelHighResolution(Buf: Pointer; p: PAggColor; x, y: Integer); override;
  end;

  TAggPatternFilterBilinearRgba = class(TAggPatternFilter)
  public
    function Dilation: Cardinal; override;
    procedure PixelLowResolution(Buf: Pointer; p: PAggColor; x, y: Integer); override;
    procedure PixelHighResolution(Buf: Pointer; p: PAggColor; x, y: Integer); override;
  end;

  TAggPatternFilterBilinearGray8 = class(TAggPatternFilterBilinearRgba)
  public
    procedure PixelHighResolution(Buf: Pointer; p: PAggColor; x, y: Integer); override;
  end;

implementation


{ TAggPatternFilterNN }

function TAggPatternFilterNN.Dilation;
begin
  Result := 0;
end;

procedure TAggPatternFilterNN.PixelLowResolution(Buf: Pointer; p: PAggColor;
  x, y: Integer);
begin
  p^.FromRgba8(PAggRgba8(PtrComp(PPAggRgba8(PtrComp(Buf) + y *
    SizeOf(Pointer))^) + x * SizeOf(TAggRgba8))^);
end;

procedure TAggPatternFilterNN.PixelHighResolution(Buf: Pointer; p: PAggColor;
  x, y: Integer);
begin
  p^.FromRgba8(PAggRgba8(PtrComp(PPAggRgba8(PtrComp(Buf) + ShrInt32(y,
    CAggLineSubpixelShift) * SizeOf(Pointer))^) + ShrInt32(x,
    CAggLineSubpixelShift) * SizeOf(TAggRgba8))^);
end;

{ TAggPatternFilterBilinearRgba }

function TAggPatternFilterBilinearRgba.Dilation;
begin
  Result := 1;
end;

procedure TAggPatternFilterBilinearRgba.PixelLowResolution;
begin
  p^.FromRgba8(PAggRgba8(PtrComp(PPAggRgba8(PtrComp(Buf) + y *
    SizeOf(Pointer))^) + x * SizeOf(TAggRgba8))^);
end;

procedure TAggPatternFilterBilinearRgba.PixelHighResolution;
var
  r, g, b, a, Weight: Int32u;
  LowRes: TPointInteger;
  PTR: PAggRgba8;
begin
  r := CAggLineSubpixelSize * CAggLineSubpixelSize div 2;
  g := r;
  b := g;
  a := b;

  LowRes.x := ShrInt32(x, CAggLineSubpixelShift);
  LowRes.y := ShrInt32(y, CAggLineSubpixelShift);

  x := x and CAggLineSubpixelMask;
  y := y and CAggLineSubpixelMask;

  PTR := PAggRgba8(PtrComp(PPAggRgba8(PtrComp(Buf) + LowRes.y * SizeOf(Pointer))^) +
    LowRes.x * SizeOf(TAggRgba8));

  Weight := (CAggLineSubpixelSize - x) * (CAggLineSubpixelSize - y);

  inc(r, Weight * PTR.r);
  inc(g, Weight * PTR.g);
  inc(b, Weight * PTR.b);
  inc(a, Weight * PTR.a);

  inc(PtrComp(PTR), SizeOf(TAggRgba8));

  Weight := x * (CAggLineSubpixelSize - y);

  inc(r, Weight * PTR.r);
  inc(g, Weight * PTR.g);
  inc(b, Weight * PTR.b);
  inc(a, Weight * PTR.a);

  PTR := PAggRgba8(PtrComp(PPAggRgba8(PtrComp(Buf) + (LowRes.y + 1) *
    SizeOf(Pointer))^) + LowRes.x * SizeOf(TAggRgba8));

  Weight := (CAggLineSubpixelSize - x) * y;

  inc(r, Weight * PTR.r);
  inc(g, Weight * PTR.g);
  inc(b, Weight * PTR.b);
  inc(a, Weight * PTR.a);

  inc(PtrComp(PTR), SizeOf(TAggRgba8));

  Weight := x * y;

  inc(r, Weight * PTR.r);
  inc(g, Weight * PTR.g);
  inc(b, Weight * PTR.b);
  inc(a, Weight * PTR.a);

  p.Rgba8.r := Int8u(r shr (CAggLineSubpixelShift * 2));
  p.Rgba8.g := Int8u(g shr (CAggLineSubpixelShift * 2));
  p.Rgba8.b := Int8u(b shr (CAggLineSubpixelShift * 2));
  p.Rgba8.a := Int8u(a shr (CAggLineSubpixelShift * 2));
end;

{ TAggPatternFilterBilinearGray8 }

procedure TAggPatternFilterBilinearGray8.PixelHighResolution;
var
  r, g, b, a, Weight: Int32u;
  LowRes: TPointInteger;
  PTR: PAggRgba8;
begin
  r := CAggLineSubpixelSize * CAggLineSubpixelSize div 2;
  g := r;
  b := g;
  a := b;

  LowRes.x := ShrInt32(x, CAggLineSubpixelShift);
  LowRes.y := ShrInt32(y, CAggLineSubpixelShift);

  x := x and CAggLineSubpixelMask;
  y := y and CAggLineSubpixelMask;

  PTR := PAggRgba8(PtrComp(PPAggRgba8(PtrComp(Buf) +
    LowRes.y * SizeOf(Pointer))^) + LowRes.x * SizeOf(TAggRgba8));

  Weight := (CAggLineSubpixelSize - x) * (CAggLineSubpixelSize - y);

  inc(r, Weight * PTR.r);
  inc(g, Weight * PTR.g);
  inc(b, Weight * PTR.b);
  inc(a, Weight * PTR.a);

  inc(PtrComp(PTR), SizeOf(TAggRgba8));

  Weight := x * (CAggLineSubpixelSize - y);

  inc(r, Weight * PTR.r);
  inc(g, Weight * PTR.g);
  inc(b, Weight * PTR.b);
  inc(a, Weight * PTR.a);

  PTR := PAggRgba8(PtrComp(PPAggRgba8(PtrComp(Buf) + (LowRes.y + 1) *
    SizeOf(Pointer))^) + LowRes.x * SizeOf(TAggRgba8));

  Weight := (CAggLineSubpixelSize - x) * y;

  inc(r, Weight * PTR.r);
  inc(g, Weight * PTR.g);
  inc(b, Weight * PTR.b);
  inc(a, Weight * PTR.a);

  inc(PtrComp(PTR), SizeOf(TAggRgba8));

  Weight := x * y;

  inc(r, Weight * PTR.r);
  inc(g, Weight * PTR.g);
  inc(b, Weight * PTR.b);
  inc(a, Weight * PTR.a);

  p.Rgba8.r := Int8u(r shr (CAggLineSubpixelShift * 2));
  p.Rgba8.g := Int8u(g shr (CAggLineSubpixelShift * 2));
  p.Rgba8.b := Int8u(b shr (CAggLineSubpixelShift * 2));
  p.Rgba8.a := Int8u(a shr (CAggLineSubpixelShift * 2));
  p.v := (p.Rgba8.r * 77 + p.Rgba8.g * 150 + p.Rgba8.b * 29) shr 8;
end;

end. 
 
 
