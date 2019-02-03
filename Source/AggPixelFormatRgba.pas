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
unit AggPixelFormatRgba;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggPixelFormat,
  AggColor32,
  AggRenderingBuffer;

procedure PixelFormatBgra32(out PixelFormatProcessor: TAggPixelFormatProcessor; RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatRgba32(out PixelFormatProcessor: TAggPixelFormatProcessor; RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatArgb32(out PixelFormatProcessor: TAggPixelFormatProcessor; RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatAbgr32(out PixelFormatProcessor: TAggPixelFormatProcessor; RenderingBuffer: TAggRenderingBuffer);

procedure PixelFormatBgra32Pre(out PixelFormatProcessor: TAggPixelFormatProcessor; RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatRgba32Pre(out PixelFormatProcessor: TAggPixelFormatProcessor; RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatArgb32Pre(out PixelFormatProcessor: TAggPixelFormatProcessor; RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatAbgr32Pre(out PixelFormatProcessor: TAggPixelFormatProcessor; RenderingBuffer: TAggRenderingBuffer);

procedure BlendModeAdaptorRgba(This: TAggPixelFormatProcessor; BlendMode: TAggBlendMode; p: PInt8u; CR, Cg, CB, ca, Cover: Cardinal);
procedure BlendModeAdaptorClipToDestinationRgbaPre(This: TAggPixelFormatProcessor; BlendMode: TAggBlendMode; p: PInt8u; CR, Cg, CB, ca, Cover: Cardinal);

procedure PixelFormatAlphaBlendRgba(out PixelFormatProcessor: TAggPixelFormatProcessor; RenderingBuffer: TAggRenderingBuffer; Order: TAggOrder);
procedure PixelFormatCustomBlendRgba(out PixelFormatProcessor: TAggPixelFormatProcessor; RenderingBuffer: TAggRenderingBuffer; BL: TAggFuncBlender; Order: TAggOrder);

implementation

function Format32Row(This: TAggPixelFormatProcessor; x, y: Integer): TAggRowDataType;
var
  p: PCardinal;
begin
  p := PCardinal(This.RenderingBuffer.Row(y));
  inc(p, x);
  Result.Initialize(x, This.width - 1, PInt8u(p));
end;

procedure Format32CopyFrom(This: TAggPixelFormatProcessor; From: TAggRenderingBuffer;
  Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal);
var
  PSrc, PDst: PCardinal;
begin
  PSrc := PCardinal(From.Row(Ysrc));
  inc(PSrc, Xsrc);

  PDst := PCardinal(This.RenderingBuffer.Row(Ydst));
  inc(PDst, Xdst);

  Move(PSrc^, PDst^, Len * 4);
end;

procedure Order32ForEachPixel(This: TAggPixelFormatProcessor; f: TAggFuncApplyGamma);
var
  y, Len: Cardinal;
  p: PInt8u;
begin
  y := 0;

  while y < This.height do
    begin
      Len := This.width;

      p := This.RenderingBuffer.Row(y);

      repeat
        f(This, p);

        inc(PtrComp(p), 4);
        dec(Len);
      until Len = 0;

      inc(y);
    end;
end;

procedure Order32GammaDirApply(This: TAggPixelFormatProcessor; p: PInt8u);
begin
  PInt8u(PtrComp(p) + This.Order.r)^ :=
    Int8u(This.Apply.dir[PInt8u(PtrComp(p) + This.Order.r)^]);
  PInt8u(PtrComp(p) + This.Order.g)^ :=
    Int8u(This.Apply.dir[PInt8u(PtrComp(p) + This.Order.g)^]);
  PInt8u(PtrComp(p) + This.Order.b)^ :=
    Int8u(This.Apply.dir[PInt8u(PtrComp(p) + This.Order.b)^]);
end;

procedure Order32GammaInvApply(This: TAggPixelFormatProcessor; p: PInt8u);
begin
  PInt8u(PtrComp(p) + This.Order.r)^ :=
    Int8u(This.Apply.Inv[PInt8u(PtrComp(p) + This.Order.r)^]);
  PInt8u(PtrComp(p) + This.Order.g)^ :=
    Int8u(This.Apply.Inv[PInt8u(PtrComp(p) + This.Order.g)^]);
  PInt8u(PtrComp(p) + This.Order.b)^ :=
    Int8u(This.Apply.Inv[PInt8u(PtrComp(p) + This.Order.b)^]);
end;

procedure Order32PixelPreMultiply(This: TAggPixelFormatProcessor; p: PInt8u);
var
  a: Cardinal;
begin
  a := PInt8u(PtrComp(p) + This.Order.a)^;

  if a = 0 then
    begin
      PInt8u(PtrComp(p) + This.Order.r)^ := 0;
      PInt8u(PtrComp(p) + This.Order.g)^ := 0;
      PInt8u(PtrComp(p) + This.Order.b)^ := 0;
    end
  else
    begin
      PInt8u(PtrComp(p) + This.Order.r)^ :=
        Int8u((PInt8u(PtrComp(p) + This.Order.r)^ * a + CAggBaseMask)
        shr CAggBaseShift);

      PInt8u(PtrComp(p) + This.Order.g)^ :=
        Int8u((PInt8u(PtrComp(p) + This.Order.g)^ * a + CAggBaseMask)
        shr CAggBaseShift);

      PInt8u(PtrComp(p) + This.Order.b)^ :=
        Int8u((PInt8u(PtrComp(p) + This.Order.b)^ * a + CAggBaseMask)
        shr CAggBaseShift);
    end;
end;

procedure Order32PixelDeMultiply(This: TAggPixelFormatProcessor; p: PInt8u);
var
  r, g, b, a: Cardinal;
begin
  a := PInt8u(PtrComp(p) + This.Order.a)^;

  if a = 0 then
    begin
      PInt8u(PtrComp(p) + This.Order.r)^ := 0;
      PInt8u(PtrComp(p) + This.Order.g)^ := 0;
      PInt8u(PtrComp(p) + This.Order.b)^ := 0;
    end
  else
    begin
      r := (PInt8u(PtrComp(p) + This.Order.r)^ * CAggBaseMask) div a;
      g := (PInt8u(PtrComp(p) + This.Order.g)^ * CAggBaseMask) div a;
      b := (PInt8u(PtrComp(p) + This.Order.b)^ * CAggBaseMask) div a;

      if r > CAggBaseMask then
          PInt8u(PtrComp(p) + This.Order.r)^ := CAggBaseMask
      else
          PInt8u(PtrComp(p) + This.Order.r)^ := r;

      if g > CAggBaseMask then
          PInt8u(PtrComp(p) + This.Order.g)^ := CAggBaseMask
      else
          PInt8u(PtrComp(p) + This.Order.g)^ := g;

      if b > CAggBaseMask then
          PInt8u(PtrComp(p) + This.Order.b)^ := CAggBaseMask
      else
          PInt8u(PtrComp(p) + This.Order.b)^ := b;
    end;
end;

{$INCLUDE AggPixelFormatBgra32.inc }


procedure PixelFormatBgra32(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderBgra;
  PixelFormatProcessor.PixWidth := 4;

  PixelFormatProcessor.CopyPixel := @Bgra32CopyPixel;
  PixelFormatProcessor.BlendPixel := @Bgra32BlendPixel;

  PixelFormatProcessor.Pixel := @Bgra32Pixel;
  PixelFormatProcessor.Row := @Format32Row;

  PixelFormatProcessor.CopyHorizontalLine := @Bgra32CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Bgra32CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Bgra32BlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Bgra32BlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Bgra32BlendSolidHorizontalSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Bgra32BlendSolidVerticalSpan;

  PixelFormatProcessor.CopyColorHSpan := @Bgra32CopyColorHorizontalSpan;
  PixelFormatProcessor.CopyColorVSpan := @Bgra32CopyColorVerticalSpan;

  PixelFormatProcessor.BlendColorHSpan := @Bgra32BlendColorHorizontalSpan;
  PixelFormatProcessor.BlendColorVSpan := @Bgra32BlendColorVerticalSpan;

  PixelFormatProcessor.CopyFrom := @Format32CopyFrom;
  PixelFormatProcessor.BlendFrom := @Bgra32BlendFrom;

  PixelFormatProcessor.BlendFromColor := @Bgra32BlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Bgra32BlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order32ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order32GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order32GammaInvApply;

  PixelFormatProcessor.PixelPreMultiply := @Order32PixelPreMultiply;
  PixelFormatProcessor.PixelDeMultiply := @Order32PixelDeMultiply;
end;

{$INCLUDE AggPixelFormatRgba32.inc }


procedure PixelFormatRgba32(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderRgba;

  PixelFormatProcessor.PixWidth := 4;

  PixelFormatProcessor.CopyPixel := @Rgba32CopyPixel;
  PixelFormatProcessor.BlendPixel := @Rgba32BlendPixel;

  PixelFormatProcessor.Pixel := @Rgba32Pixel;
  PixelFormatProcessor.Row := @Format32Row;

  PixelFormatProcessor.CopyHorizontalLine := @Rgba32CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Rgba32CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Rgba32BlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Rgba32BlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Rgba32BlendSolidHorizontalSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Rgba32BlendSolidVerticalSpan;

  PixelFormatProcessor.CopyColorHSpan := @Rgba32CopyColorHorizontalSpan;
  PixelFormatProcessor.CopyColorVSpan := @Rgba32CopyColorVerticalSpan;

  PixelFormatProcessor.BlendColorHSpan := @Rgba32BlendColorHorizontalSpan;
  PixelFormatProcessor.BlendColorVSpan := @Rgba32BlendColorVerticalSpan;

  PixelFormatProcessor.CopyFrom := @Format32CopyFrom;
  PixelFormatProcessor.BlendFrom := @Rgba32BlendFrom;

  PixelFormatProcessor.BlendFromColor := @Rgba32BlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Rgba32BlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order32ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order32GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order32GammaInvApply;

  PixelFormatProcessor.PixelPreMultiply := @Order32PixelPreMultiply;
  PixelFormatProcessor.PixelDeMultiply := @Order32PixelDeMultiply;
end;

{$INCLUDE AggPixelFormatArgb32.inc }


procedure PixelFormatArgb32(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderArgb;

  PixelFormatProcessor.PixWidth := 4;

  PixelFormatProcessor.CopyPixel := @ARGB32CopyPixel;
  PixelFormatProcessor.BlendPixel := @ARGB32BlendPixel;

  PixelFormatProcessor.Pixel := @ARGB32Pixel;
  PixelFormatProcessor.Row := @Format32Row;

  PixelFormatProcessor.CopyHorizontalLine := @ARGB32CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @ARGB32CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @ARGB32BlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @ARGB32BlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @ARGB32BlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @ARGB32BlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @ARGB32CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @ARGB32CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @ARGB32BlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @ARGB32BlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Format32CopyFrom;
  PixelFormatProcessor.BlendFrom := @ARGB32BlendFrom;

  PixelFormatProcessor.BlendFromColor := @ARGB32BlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @ARGB32BlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order32ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order32GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order32GammaInvApply;

  PixelFormatProcessor.PixelPreMultiply := @Order32PixelPreMultiply;
  PixelFormatProcessor.PixelDeMultiply := @Order32PixelDeMultiply;
end;

{$INCLUDE AggPixelFormatAbgr32.inc }


procedure PixelFormatAbgr32(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderAbgr;

  PixelFormatProcessor.PixWidth := 4;

  PixelFormatProcessor.CopyPixel := @Abgr32CopyPixel;
  PixelFormatProcessor.BlendPixel := @Abgr32BlendPixel;

  PixelFormatProcessor.Pixel := @Abgr32Pixel;
  PixelFormatProcessor.Row := @Format32Row;

  PixelFormatProcessor.CopyHorizontalLine := @Abgr32CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Abgr32CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Abgr32BlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Abgr32BlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Abgr32BlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Abgr32BlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Abgr32CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Abgr32CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Abgr32BlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Abgr32BlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Format32CopyFrom;
  PixelFormatProcessor.BlendFrom := @Abgr32BlendFrom;

  PixelFormatProcessor.BlendFromColor := @Abgr32BlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Abgr32BlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order32ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order32GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order32GammaInvApply;

  PixelFormatProcessor.PixelPreMultiply := @Order32PixelPreMultiply;
  PixelFormatProcessor.PixelDeMultiply := @Order32PixelDeMultiply;
end;

{$INCLUDE AggPixelFormatBgra32Pre.inc }


procedure PixelFormatBgra32Pre(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderBgra;

  PixelFormatProcessor.PixWidth := 4;

  PixelFormatProcessor.CopyPixel := @Bgra32CopyPixel;
  PixelFormatProcessor.BlendPixel := @Bgra32PreBlendPixel;

  PixelFormatProcessor.Pixel := @Bgra32Pixel;
  PixelFormatProcessor.Row := @Format32Row;

  PixelFormatProcessor.CopyHorizontalLine := @Bgra32CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Bgra32CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Bgra32PreBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Bgra32PreBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Bgra32PreBlendSolidHorizontalSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Bgra32PreBlendSolidVerticalSpan;

  PixelFormatProcessor.CopyColorHSpan := @Bgra32CopyColorHorizontalSpan;
  PixelFormatProcessor.CopyColorVSpan := @Bgra32CopyColorVerticalSpan;

  PixelFormatProcessor.BlendColorHSpan := @Bgra32PreBlendColorHorizontalSpan;
  PixelFormatProcessor.BlendColorVSpan := @Bgra32PreBlendColorVerticalSpan;

  PixelFormatProcessor.CopyFrom := @Format32CopyFrom;
  PixelFormatProcessor.BlendFrom := @Bgra32PreBlendFrom;

  PixelFormatProcessor.BlendFromColor := @Bgra32PreBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Bgra32PreBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order32ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order32GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order32GammaInvApply;

  PixelFormatProcessor.PixelPreMultiply := @Order32PixelPreMultiply;
  PixelFormatProcessor.PixelDeMultiply := @Order32PixelDeMultiply;
end;

{$INCLUDE AggPixelFormatRgba32Pre.inc }


procedure PixelFormatRgba32Pre(
  out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderRgba;

  PixelFormatProcessor.PixWidth := 4;

  PixelFormatProcessor.CopyPixel := @Rgba32CopyPixel;
  PixelFormatProcessor.BlendPixel := @Rgba32PreBlendPixel;

  PixelFormatProcessor.Pixel := @Rgba32Pixel;
  PixelFormatProcessor.Row := @Format32Row;

  PixelFormatProcessor.CopyHorizontalLine := @Rgba32CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Rgba32CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Rgba32PreBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Rgba32PreBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Rgba32PreBlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Rgba32PreBlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Rgba32CopyColorHorizontalSpan;
  PixelFormatProcessor.CopyColorVSpan := @Rgba32CopyColorVerticalSpan;

  PixelFormatProcessor.BlendColorHSpan := @Rgba32PreBlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Rgba32PreBlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Format32CopyFrom;
  PixelFormatProcessor.BlendFrom := @Rgba32PreBlendFrom;

  PixelFormatProcessor.BlendFromColor := @Rgba32PreBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Rgba32PreBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order32ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order32GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order32GammaInvApply;

  PixelFormatProcessor.PixelPreMultiply := @Order32PixelPreMultiply;
  PixelFormatProcessor.PixelDeMultiply := @Order32PixelDeMultiply;
end;

{$INCLUDE AggPixelFormatArgb32Pre.inc }


procedure PixelFormatArgb32Pre(
  out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderArgb;

  PixelFormatProcessor.PixWidth := 4;

  PixelFormatProcessor.CopyPixel := @ARGB32CopyPixel;
  PixelFormatProcessor.BlendPixel := @ARGB32PreBlendPixel;

  PixelFormatProcessor.Pixel := @ARGB32Pixel;
  PixelFormatProcessor.Row := @Format32Row;

  PixelFormatProcessor.CopyHorizontalLine := @ARGB32CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @ARGB32CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @ARGB32PreBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @ARGB32PreBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @ARGB32PreBlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @ARGB32PreBlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := ARGB32CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := ARGB32CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @ARGB32PreBlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @ARGB32PreBlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Format32CopyFrom;
  PixelFormatProcessor.BlendFrom := @ARGB32PreBlendFrom;

  PixelFormatProcessor.BlendFromColor := @ARGB32PreBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @ARGB32PreBlendFromLut;

  PixelFormatProcessor.ForEachPixel := @Order32ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order32GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order32GammaInvApply;

  PixelFormatProcessor.PixelPreMultiply := @Order32PixelPreMultiply;
  PixelFormatProcessor.PixelDeMultiply := @Order32PixelDeMultiply;
end;

{$INCLUDE AggPixelFormatAbgr32Pre.inc}


procedure PixelFormatAbgr32Pre(
  out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderAbgr;

  PixelFormatProcessor.PixWidth := 4;

  PixelFormatProcessor.CopyPixel := @Abgr32CopyPixel;
  PixelFormatProcessor.BlendPixel := @Abgr32PreBlendPixel;

  PixelFormatProcessor.Pixel := @Abgr32Pixel;
  PixelFormatProcessor.Row := @Format32Row;

  PixelFormatProcessor.CopyHorizontalLine := @Abgr32CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Abgr32CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Abgr32PreBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Abgr32PreBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Abgr32PreBlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Abgr32PreBlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := Abgr32CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := Abgr32CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Abgr32PreBlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Abgr32PreBlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Format32CopyFrom;
  PixelFormatProcessor.BlendFrom := @Abgr32PreBlendFrom;

  PixelFormatProcessor.BlendFromColor := @Abgr32PreBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Abgr32PreBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order32ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order32GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order32GammaInvApply;

  PixelFormatProcessor.PixelPreMultiply := @Order32PixelPreMultiply;
  PixelFormatProcessor.PixelDeMultiply := @Order32PixelDeMultiply;
end;

{$INCLUDE AggPixelFormatAlpha32.inc }


procedure PixelFormatAlphaBlendRgba(
  out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer; Order: TAggOrder);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := Order;

  PixelFormatProcessor.PixWidth := 4;

  PixelFormatProcessor.CopyPixel := @Alpha32CopyPixel;
  PixelFormatProcessor.BlendPixel := @Alpha32BlendPixel;

  PixelFormatProcessor.Pixel := @Alpha32Pixel;
  PixelFormatProcessor.Row := @Format32Row;

  PixelFormatProcessor.CopyHorizontalLine := @Alpha32CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Alpha32CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Alpha32BlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Alpha32BlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Alpha32BlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Alpha32BlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Alpha32CopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @Alpha32CopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @Alpha32BlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @Alpha32BlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Format32CopyFrom;
  PixelFormatProcessor.BlendFrom := @Alpha32BlendFrom;

  PixelFormatProcessor.BlendFromColor := @Alpha32BlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Alpha32BlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order32ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order32GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order32GammaInvApply;

  PixelFormatProcessor.PixelPreMultiply := @Order32PixelPreMultiply;
  PixelFormatProcessor.PixelDeMultiply := @Order32PixelDeMultiply;
end;

{$INCLUDE AggPixelFormatCubl32.inc }


procedure PixelFormatCustomBlendRgba(
  out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer; BL: TAggFuncBlender; Order: TAggOrder);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := Order;
  PixelFormatProcessor.Blender := BL;

  PixelFormatProcessor.PixWidth := 4;

  PixelFormatProcessor.CopyPixel := @CublCopyPixel;
  PixelFormatProcessor.BlendPixel := @CublBlendPixel;

  PixelFormatProcessor.Pixel := @CublPixel;
  PixelFormatProcessor.Row := @Format32Row;

  PixelFormatProcessor.CopyHorizontalLine := @CublCopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @CublCopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @CublBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @CublBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @CublBlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @CublBlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @CublCopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @CublCopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @CublBlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @CublBlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Format32CopyFrom;
  PixelFormatProcessor.BlendFrom := @CublBlendFrom;

  PixelFormatProcessor.BlendFromColor := @CublBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @CublBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order32ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order32GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order32GammaInvApply;

  PixelFormatProcessor.PixelPreMultiply := @Order32PixelPreMultiply;
  PixelFormatProcessor.PixelDeMultiply := @Order32PixelDeMultiply;
end;

procedure coRgbaClear(This: TAggPixelFormatProcessor; p: PInt8u;
  CR, Cg, CB, alpha, Cover: Cardinal);
begin
  if Cover < 255 then
    begin
      Cover := 255 - Cover;

      PInt8u(PtrComp(p) + This.Order.r)^ :=
        Int8u((PInt8u(PtrComp(p) + This.Order.r)^ * Cover + 255) shr 8);
      PInt8u(PtrComp(p) + This.Order.g)^ :=
        Int8u((PInt8u(PtrComp(p) + This.Order.g)^ * Cover + 255) shr 8);
      PInt8u(PtrComp(p) + This.Order.b)^ :=
        Int8u((PInt8u(PtrComp(p) + This.Order.b)^ * Cover + 255) shr 8);
      PInt8u(PtrComp(p) + This.Order.a)^ :=
        Int8u((PInt8u(PtrComp(p) + This.Order.a)^ * Cover + 255) shr 8);
    end
  else
      PCardinal(p)^ := 0;
end;

procedure coRgbaSrc(This: TAggPixelFormatProcessor; p: PInt8u;
  SR, sg, SB, SA, Cover: Cardinal);
var
  alpha: Cardinal;
begin
  if Cover < 255 then
    begin
      alpha := 255 - Cover;

      PInt8u(PtrComp(p) + This.Order.r)^ :=
        Int8u(((PInt8u(PtrComp(p) + This.Order.r)^ * alpha + 255) shr 8) +
        ((SR * Cover + 255) shr 8));
      PInt8u(PtrComp(p) + This.Order.g)^ :=
        Int8u(((PInt8u(PtrComp(p) + This.Order.g)^ * alpha + 255) shr 8) +
        ((sg * Cover + 255) shr 8));
      PInt8u(PtrComp(p) + This.Order.b)^ :=
        Int8u(((PInt8u(PtrComp(p) + This.Order.b)^ * alpha + 255) shr 8) +
        ((SB * Cover + 255) shr 8));
      PInt8u(PtrComp(p) + This.Order.a)^ :=
        Int8u(((PInt8u(PtrComp(p) + This.Order.a)^ * alpha + 255) shr 8) +
        ((SA * Cover + 255) shr 8));
    end
  else
    begin
      PInt8u(PtrComp(p) + This.Order.r)^ := SR;
      PInt8u(PtrComp(p) + This.Order.g)^ := sg;
      PInt8u(PtrComp(p) + This.Order.b)^ := SB;
      PInt8u(PtrComp(p) + This.Order.a)^ := SA;
    end;
end;

procedure coRgbaDst(This: TAggPixelFormatProcessor; p: PInt8u;
  CR, Cg, CB, alpha, Cover: Cardinal);
begin
end;

// Dca' = Sca + Dca.(1 - Sa)
// Da'  = Sa + Da - Sa.Da
procedure coRgbaSrcOver(This: TAggPixelFormatProcessor; p: PInt8u;
  SR, sg, SB, SA, Cover: Cardinal);
var
  S1a: Cardinal;
begin
  if Cover < 255 then
    begin
      SR := (SR * Cover + 255) shr 8;
      sg := (sg * Cover + 255) shr 8;
      SB := (SB * Cover + 255) shr 8;
      SA := (SA * Cover + 255) shr 8;
    end;

  S1a := CAggBaseMask - SA;

  PInt8u(PtrComp(p) + This.Order.r)^ :=
    Int8u(SR + ((PInt8u(PtrComp(p) + This.Order.r)^ * S1a + CAggBaseMask)
    shr CAggBaseShift));

  PInt8u(PtrComp(p) + This.Order.g)^ :=
    Int8u(sg + ((PInt8u(PtrComp(p) + This.Order.g)^ * S1a + CAggBaseMask)
    shr CAggBaseShift));

  PInt8u(PtrComp(p) + This.Order.b)^ :=
    Int8u(SB + ((PInt8u(PtrComp(p) + This.Order.b)^ * S1a + CAggBaseMask)
    shr CAggBaseShift));

  PInt8u(PtrComp(p) + This.Order.a)^ :=
    Int8u(SA + PInt8u(PtrComp(p) + This.Order.a)^ -
    ((SA * PInt8u(PtrComp(p) + This.Order.a)^ + CAggBaseMask)
    shr CAggBaseShift));
end;

// Dca' = Dca + Sca.(1 - Da)
// Da'  = Sa + Da - Sa.Da
procedure coRgbaDstOver(This: TAggPixelFormatProcessor; p: PInt8u;
  SR, sg, SB, SA, Cover: Cardinal);
var
  D1a: Cardinal;
begin
  if Cover < 255 then
    begin
      SR := (SR * Cover + 255) shr 8;
      sg := (sg * Cover + 255) shr 8;
      SB := (SB * Cover + 255) shr 8;
      SA := (SA * Cover + 255) shr 8;
    end;

  D1a := CAggBaseMask - PInt8u(PtrComp(p) + This.Order.a)^;

  PInt8u(PtrComp(p) + This.Order.r)^ :=
    Int8u(PInt8u(PtrComp(p) + This.Order.r)^ +
    ((SR * D1a + CAggBaseMask) shr CAggBaseShift));

  PInt8u(PtrComp(p) + This.Order.g)^ :=
    Int8u(PInt8u(PtrComp(p) + This.Order.g)^ +
    ((sg * D1a + CAggBaseMask) shr CAggBaseShift));

  PInt8u(PtrComp(p) + This.Order.b)^ :=
    Int8u(PInt8u(PtrComp(p) + This.Order.b)^ +
    ((SB * D1a + CAggBaseMask) shr CAggBaseShift));

  PInt8u(PtrComp(p) + This.Order.a)^ :=
    Int8u(SA + PInt8u(PtrComp(p) + This.Order.a)^ -
    ((SA * PInt8u(PtrComp(p) + This.Order.a)^ + CAggBaseMask)
    shr CAggBaseShift));
end;

// Dca' = Sca.Da
// Da'  = Sa.Da
procedure coRgbaSrcIn(This: TAggPixelFormatProcessor; p: PInt8u;
  SR, sg, SB, SA, Cover: Cardinal);
var
  DA, alpha: Cardinal;
begin
  DA := PInt8u(PtrComp(p) + This.Order.a)^;

  if Cover < 255 then
    begin
      alpha := 255 - Cover;

      PInt8u(PtrComp(p) + This.Order.r)^ :=
        Int8u(((PInt8u(PtrComp(p) + This.Order.r)^ * alpha + 255) shr 8) +
        ((((SR * DA + CAggBaseMask) shr CAggBaseShift) * Cover + 255) shr 8));

      PInt8u(PtrComp(p) + This.Order.g)^ :=
        Int8u(((PInt8u(PtrComp(p) + This.Order.g)^ * alpha + 255) shr 8) +
        ((((sg * DA + CAggBaseMask) shr CAggBaseShift) * Cover + 255) shr 8));

      PInt8u(PtrComp(p) + This.Order.b)^ :=
        Int8u(((PInt8u(PtrComp(p) + This.Order.b)^ * alpha + 255) shr 8) +
        ((((SB * DA + CAggBaseMask) shr CAggBaseShift) * Cover + 255) shr 8));

      PInt8u(PtrComp(p) + This.Order.a)^ :=
        Int8u(((PInt8u(PtrComp(p) + This.Order.a)^ * alpha + 255) shr 8) +
        ((((SA * DA + CAggBaseMask) shr CAggBaseShift) * Cover + 255) shr 8));
    end
  else
    begin
      PInt8u(PtrComp(p) + This.Order.r)^ :=
        Int8u((SR * DA + CAggBaseMask) shr CAggBaseShift);
      PInt8u(PtrComp(p) + This.Order.g)^ :=
        Int8u((sg * DA + CAggBaseMask) shr CAggBaseShift);
      PInt8u(PtrComp(p) + This.Order.b)^ :=
        Int8u((SB * DA + CAggBaseMask) shr CAggBaseShift);
      PInt8u(PtrComp(p) + This.Order.a)^ :=
        Int8u((SA * DA + CAggBaseMask) shr CAggBaseShift);
    end;
end;

// Dca' = Dca.Sa
// Da'  = Sa.Da
procedure coRgbaDstIn(This: TAggPixelFormatProcessor; p: PInt8u;
  CR, Cg, CB, SA, Cover: Cardinal);
begin
  if Cover < 255 then
      SA := CAggBaseMask - ((Cover * (CAggBaseMask - SA) + 255) shr 8);

  PInt8u(PtrComp(p) + This.Order.r)^ :=
    Int8u((PInt8u(PtrComp(p) + This.Order.r)^ * SA + CAggBaseMask)
    shr CAggBaseShift);
  PInt8u(PtrComp(p) + This.Order.g)^ :=
    Int8u((PInt8u(PtrComp(p) + This.Order.g)^ * SA + CAggBaseMask)
    shr CAggBaseShift);
  PInt8u(PtrComp(p) + This.Order.b)^ :=
    Int8u((PInt8u(PtrComp(p) + This.Order.b)^ * SA + CAggBaseMask)
    shr CAggBaseShift);
  PInt8u(PtrComp(p) + This.Order.a)^ :=
    Int8u((PInt8u(PtrComp(p) + This.Order.a)^ * SA + CAggBaseMask)
    shr CAggBaseShift);
end;

// Dca' = Sca.(1 - Da)
// Da'  = Sa.(1 - Da)
procedure coRgbaSrcOut(This: TAggPixelFormatProcessor; p: PInt8u;
  SR, sg, SB, SA, Cover: Cardinal);
var
  DA, alpha: Cardinal;
begin
  DA := CAggBaseMask - PInt8u(PtrComp(p) + This.Order.a)^;

  if Cover < 255 then
    begin
      alpha := 255 - Cover;

      PInt8u(PtrComp(p) + This.Order.r)^ :=
        Int8u(((PInt8u(PtrComp(p) + This.Order.r)^ * alpha + 255) shr 8) +
        ((((SR * DA + CAggBaseMask) shr CAggBaseShift) * Cover + 255) shr 8));

      PInt8u(PtrComp(p) + This.Order.g)^ :=
        Int8u(((PInt8u(PtrComp(p) + This.Order.g)^ * alpha + 255) shr 8) +
        ((((sg * DA + CAggBaseMask) shr CAggBaseShift) * Cover + 255) shr 8));

      PInt8u(PtrComp(p) + This.Order.b)^ :=
        Int8u(((PInt8u(PtrComp(p) + This.Order.b)^ * alpha + 255) shr 8) +
        ((((SB * DA + CAggBaseMask) shr CAggBaseShift) * Cover + 255) shr 8));

      PInt8u(PtrComp(p) + This.Order.a)^ :=
        Int8u(((PInt8u(PtrComp(p) + This.Order.a)^ * alpha + 255) shr 8) +
        ((((SA * DA + CAggBaseMask) shr CAggBaseShift) * Cover + 255) shr 8));
    end
  else
    begin
      PInt8u(PtrComp(p) + This.Order.r)^ :=
        Int8u((SR * DA + CAggBaseMask) shr CAggBaseShift);
      PInt8u(PtrComp(p) + This.Order.g)^ :=
        Int8u((sg * DA + CAggBaseMask) shr CAggBaseShift);
      PInt8u(PtrComp(p) + This.Order.b)^ :=
        Int8u((SB * DA + CAggBaseMask) shr CAggBaseShift);
      PInt8u(PtrComp(p) + This.Order.a)^ :=
        Int8u((SA * DA + CAggBaseMask) shr CAggBaseShift);
    end;
end;

// Dca' = Dca.(1 - Sa)
// Da'  = Da.(1 - Sa)
procedure coRgbaDstOut(This: TAggPixelFormatProcessor; p: PInt8u;
  CR, Cg, CB, SA, Cover: Cardinal);
begin
  if Cover < 255 then
      SA := (SA * Cover + 255) shr 8;

  SA := CAggBaseMask - SA;

  PInt8u(PtrComp(p) + This.Order.r)^ :=
    Int8u((PInt8u(PtrComp(p) + This.Order.r)^ * SA + CAggBaseShift)
    shr CAggBaseShift);
  PInt8u(PtrComp(p) + This.Order.g)^ :=
    Int8u((PInt8u(PtrComp(p) + This.Order.g)^ * SA + CAggBaseShift)
    shr CAggBaseShift);
  PInt8u(PtrComp(p) + This.Order.b)^ :=
    Int8u((PInt8u(PtrComp(p) + This.Order.b)^ * SA + CAggBaseShift)
    shr CAggBaseShift);
  PInt8u(PtrComp(p) + This.Order.a)^ :=
    Int8u((PInt8u(PtrComp(p) + This.Order.a)^ * SA + CAggBaseShift)
    shr CAggBaseShift);
end;

// Dca' = Sca.Da + Dca.(1 - Sa)
// Da'  = Da
procedure coRgbaSrcATop(This: TAggPixelFormatProcessor; p: PInt8u;
  SR, sg, SB, SA, Cover: Cardinal);
var
  DA: Cardinal;
begin
  if Cover < 255 then
    begin
      SR := (SR * Cover + 255) shr 8;
      sg := (sg * Cover + 255) shr 8;
      SB := (SB * Cover + 255) shr 8;
      SA := (SA * Cover + 255) shr 8;
    end;

  DA := PInt8u(PtrComp(p) + This.Order.a)^;
  SA := CAggBaseMask - SA;

  PInt8u(PtrComp(p) + This.Order.r)^ :=
    Int8u((SR * DA + PInt8u(PtrComp(p) + This.Order.r)^ * SA + CAggBaseMask)
    shr CAggBaseShift);
  PInt8u(PtrComp(p) + This.Order.g)^ :=
    Int8u((sg * DA + PInt8u(PtrComp(p) + This.Order.g)^ * SA + CAggBaseMask)
    shr CAggBaseShift);
  PInt8u(PtrComp(p) + This.Order.b)^ :=
    Int8u((SB * DA + PInt8u(PtrComp(p) + This.Order.b)^ * SA + CAggBaseMask)
    shr CAggBaseShift);
end;

// Dca' = Dca.Sa + Sca.(1 - Da)
// Da'  = Sa
procedure coRgbaDstATop(This: TAggPixelFormatProcessor; p: PInt8u;
  SR, sg, SB, SA, Cover: Cardinal);
var
  DA, alpha: Cardinal;
begin
  DA := CAggBaseMask - PInt8u(PtrComp(p) + This.Order.a)^;

  if Cover < 255 then
    begin
      alpha := 255 - Cover;

      SR := (PInt8u(PtrComp(p) + This.Order.r)^ * SA + SR * DA + CAggBaseMask)
        shr CAggBaseShift;
      sg := (PInt8u(PtrComp(p) + This.Order.g)^ * SA + sg * DA + CAggBaseMask)
        shr CAggBaseShift;
      SB := (PInt8u(PtrComp(p) + This.Order.b)^ * SA + SB * DA + CAggBaseMask)
        shr CAggBaseShift;

      PInt8u(PtrComp(p) + This.Order.r)^ :=
        Int8u(((PInt8u(PtrComp(p) + This.Order.r)^ * alpha + 255) shr 8) +
        ((SR * Cover + 255) shr 8));

      PInt8u(PtrComp(p) + This.Order.g)^ :=
        Int8u(((PInt8u(PtrComp(p) + This.Order.g)^ * alpha + 255) shr 8) +
        ((sg * Cover + 255) shr 8));

      PInt8u(PtrComp(p) + This.Order.b)^ :=
        Int8u(((PInt8u(PtrComp(p) + This.Order.b)^ * alpha + 255) shr 8) +
        ((SB * Cover + 255) shr 8));

      PInt8u(PtrComp(p) + This.Order.a)^ :=
        Int8u(((PInt8u(PtrComp(p) + This.Order.a)^ * alpha + 255) shr 8) +
        ((SA * Cover + 255) shr 8));
    end
  else
    begin
      PInt8u(PtrComp(p) + This.Order.r)^ :=
        Int8u((PInt8u(PtrComp(p) + This.Order.r)^ * SA + SR * DA + CAggBaseMask)
        shr CAggBaseShift);
      PInt8u(PtrComp(p) + This.Order.g)^ :=
        Int8u((PInt8u(PtrComp(p) + This.Order.g)^ * SA + sg * DA + CAggBaseMask)
        shr CAggBaseShift);
      PInt8u(PtrComp(p) + This.Order.b)^ :=
        Int8u((PInt8u(PtrComp(p) + This.Order.b)^ * SA + SB * DA + CAggBaseMask)
        shr CAggBaseShift);
      PInt8u(PtrComp(p) + This.Order.a)^ := Int8u(SA);
    end;
end;

// Dca' = Sca.(1 - Da) + Dca.(1 - Sa)
// Da'  = Sa + Da - 2.Sa.Da
procedure coRgbaXor(This: TAggPixelFormatProcessor; p: PInt8u;
  SR, sg, SB, SA, Cover: Cardinal);
var
  S1a, D1a: Cardinal;
begin
  if Cover < 255 then
    begin
      SR := (SR * Cover + 255) shr 8;
      sg := (sg * Cover + 255) shr 8;
      SB := (SB * Cover + 255) shr 8;
      SA := (SA * Cover + 255) shr 8;
    end;

  S1a := CAggBaseMask - SA;
  D1a := CAggBaseMask - PInt8u(PtrComp(p) + This.Order.a)^;

  PInt8u(PtrComp(p) + This.Order.r)^ :=
    Int8u((PInt8u(PtrComp(p) + This.Order.r)^ * S1a + SR * D1a + CAggBaseMask)
    shr CAggBaseShift);

  PInt8u(PtrComp(p) + This.Order.g)^ :=
    Int8u((PInt8u(PtrComp(p) + This.Order.g)^ * S1a + sg * D1a + CAggBaseMask)
    shr CAggBaseShift);

  PInt8u(PtrComp(p) + This.Order.b)^ :=
    Int8u((PInt8u(PtrComp(p) + This.Order.b)^ * S1a + SB * D1a + CAggBaseMask)
    shr CAggBaseShift);

  PInt8u(PtrComp(p) + This.Order.a)^ :=
    Int8u(SA + PInt8u(PtrComp(p) + This.Order.a)^ -
    ((SA * PInt8u(PtrComp(p) + This.Order.a)^ + CAggBaseMask div 2)
    shr (CAggBaseShift - 1)));
end;

// Dca' = Sca + Dca
// Da'  = Sa + Da
procedure coRgbaPlus(This: TAggPixelFormatProcessor; p: PInt8u;
  SR, sg, SB, SA, Cover: Cardinal);
var
  dr, DG, db, DA: Cardinal;
begin
  if Cover < 255 then
    begin
      SR := (SR * Cover + 255) shr 8;
      sg := (sg * Cover + 255) shr 8;
      SB := (SB * Cover + 255) shr 8;
      SA := (SA * Cover + 255) shr 8;
    end;

  dr := PInt8u(PtrComp(p) + This.Order.r)^ + SR;
  DG := PInt8u(PtrComp(p) + This.Order.g)^ + sg;
  db := PInt8u(PtrComp(p) + This.Order.b)^ + SB;
  DA := PInt8u(PtrComp(p) + This.Order.a)^ + SA;

  if dr > CAggBaseMask then
      PInt8u(PtrComp(p) + This.Order.r)^ := CAggBaseMask
  else
      PInt8u(PtrComp(p) + This.Order.r)^ := Int8u(dr);

  if DG > CAggBaseMask then
      PInt8u(PtrComp(p) + This.Order.g)^ := CAggBaseMask
  else
      PInt8u(PtrComp(p) + This.Order.g)^ := Int8u(DG);

  if db > CAggBaseMask then
      PInt8u(PtrComp(p) + This.Order.b)^ := CAggBaseMask
  else
      PInt8u(PtrComp(p) + This.Order.b)^ := Int8u(db);

  if DA > CAggBaseMask then
      PInt8u(PtrComp(p) + This.Order.a)^ := CAggBaseMask
  else
      PInt8u(PtrComp(p) + This.Order.a)^ := Int8u(DA);
end;

// Dca' = Dca - Sca
// Da' = 1 - (1 - Sa).(1 - Da)
procedure coRgbaMinus(This: TAggPixelFormatProcessor; p: PInt8u;
  SR, sg, SB, SA, Cover: Cardinal);
var
  dr, DG, db: Cardinal;
begin
  if Cover < 255 then
    begin
      SR := (SR * Cover + 255) shr 8;
      sg := (sg * Cover + 255) shr 8;
      SB := (SB * Cover + 255) shr 8;
      SA := (SA * Cover + 255) shr 8;
    end;

  dr := PInt8u(PtrComp(p) + This.Order.r)^ - SR;
  DG := PInt8u(PtrComp(p) + This.Order.g)^ - sg;
  db := PInt8u(PtrComp(p) + This.Order.b)^ - SB;

  if dr > CAggBaseMask then
      PInt8u(PtrComp(p) + This.Order.r)^ := 0
  else
      PInt8u(PtrComp(p) + This.Order.r)^ := Int8u(dr);

  if DG > CAggBaseMask then
      PInt8u(PtrComp(p) + This.Order.g)^ := 0
  else
      PInt8u(PtrComp(p) + This.Order.g)^ := Int8u(DG);

  if db > CAggBaseMask then
      PInt8u(PtrComp(p) + This.Order.b)^ := 0
  else
      PInt8u(PtrComp(p) + This.Order.b)^ := Int8u(db);

  PInt8u(PtrComp(p) + This.Order.a)^ :=
    Int8u(CAggBaseMask - (((CAggBaseMask - SA) * (CAggBaseMask -
    PInt8u(PtrComp(p) + This.Order.a)^) + CAggBaseMask) shr CAggBaseShift));
end;

// Dca' = Sca.Dca + Sca.(1 - Da) + Dca.(1 - Sa)
// Da'  = Sa + Da - Sa.Da
procedure coRgbaMultiply(This: TAggPixelFormatProcessor; p: PInt8u;
  SR, sg, SB, SA, Cover: Cardinal);
var
  S1a, D1a, dr, DG, db: Cardinal;
begin
  if Cover < 255 then
    begin
      SR := (SR * Cover + 255) shr 8;
      sg := (sg * Cover + 255) shr 8;
      SB := (SB * Cover + 255) shr 8;
      SA := (SA * Cover + 255) shr 8;
    end;

  S1a := CAggBaseMask - SA;
  D1a := CAggBaseMask - PInt8u(PtrComp(p) + This.Order.a)^;
  dr := PInt8u(PtrComp(p) + This.Order.r)^;
  DG := PInt8u(PtrComp(p) + This.Order.g)^;
  db := PInt8u(PtrComp(p) + This.Order.b)^;

  PInt8u(PtrComp(p) + This.Order.r)^ :=
    Int8u((SR * dr + SR * D1a + dr * S1a + CAggBaseMask) shr CAggBaseShift);
  PInt8u(PtrComp(p) + This.Order.g)^ :=
    Int8u((sg * DG + sg * D1a + DG * S1a + CAggBaseMask) shr CAggBaseShift);
  PInt8u(PtrComp(p) + This.Order.b)^ :=
    Int8u((SB * db + SB * D1a + db * S1a + CAggBaseMask) shr CAggBaseShift);
  PInt8u(PtrComp(p) + This.Order.a)^ :=
    Int8u(SA + PInt8u(PtrComp(p) + This.Order.a)^ -
    ((SA * PInt8u(PtrComp(p) + This.Order.a)^ + CAggBaseMask)
    shr CAggBaseShift));
end;

// Dca' = Sca + Dca - Sca.Dca
// Da'  = Sa + Da - Sa.Da
procedure coRgbaScreen(This: TAggPixelFormatProcessor; p: PInt8u;
  SR, sg, SB, SA, Cover: Cardinal);
var
  dr, DG, db, DA: Cardinal;
begin
  if Cover < 255 then
    begin
      SR := (SR * Cover + 255) shr 8;
      sg := (sg * Cover + 255) shr 8;
      SB := (SB * Cover + 255) shr 8;
      SA := (SA * Cover + 255) shr 8;
    end;

  dr := PInt8u(PtrComp(p) + This.Order.r)^;
  DG := PInt8u(PtrComp(p) + This.Order.g)^;
  db := PInt8u(PtrComp(p) + This.Order.b)^;
  DA := PInt8u(PtrComp(p) + This.Order.a)^;

  PInt8u(PtrComp(p) + This.Order.r)^ :=
    Int8u(SR + dr - ((SR * dr + CAggBaseMask) shr CAggBaseShift));
  PInt8u(PtrComp(p) + This.Order.g)^ :=
    Int8u(sg + DG - ((sg * DG + CAggBaseMask) shr CAggBaseShift));
  PInt8u(PtrComp(p) + This.Order.b)^ :=
    Int8u(SB + db - ((SB * db + CAggBaseMask) shr CAggBaseShift));
  PInt8u(PtrComp(p) + This.Order.a)^ :=
    Int8u(SA + DA - ((SA * DA + CAggBaseMask) shr CAggBaseShift));
end;

// if 2.Dca < Da
// Dca' = 2.Sca.Dca + Sca.(1 - Da) + Dca.(1 - Sa)
// otherwise
// Dca' = Sa.Da - 2.(Da - Dca).(Sa - Sca) + Sca.(1 - Da) + Dca.(1 - Sa)
//
// Da' = Sa + Da - Sa.Da
procedure coRgbaOverlay(This: TAggPixelFormatProcessor; p: PInt8u;
  SR, sg, SB, SA, Cover: Cardinal);
var
  D1a, S1a, dr, DG, db, DA, Sada: Cardinal;
begin
  if Cover < 255 then
    begin
      SR := (SR * Cover + 255) shr 8;
      sg := (sg * Cover + 255) shr 8;
      SB := (SB * Cover + 255) shr 8;
      SA := (SA * Cover + 255) shr 8;
    end;

  D1a := CAggBaseMask - PInt8u(PtrComp(p) + This.Order.a)^;
  S1a := CAggBaseMask - SA;
  dr := PInt8u(PtrComp(p) + This.Order.r)^;
  DG := PInt8u(PtrComp(p) + This.Order.g)^;
  db := PInt8u(PtrComp(p) + This.Order.b)^;
  DA := PInt8u(PtrComp(p) + This.Order.a)^;
  Sada := SA * PInt8u(PtrComp(p) + This.Order.a)^;

  if 2 * dr < DA then
      PInt8u(PtrComp(p) + This.Order.r)^ :=
      Int8u((2 * SR * dr + SR * D1a + dr * S1a) shr CAggBaseShift)
  else
      PInt8u(PtrComp(p) + This.Order.r)^ :=
      Int8u((Sada - 2 * (DA - dr) * (SA - SR) + SR * D1a + dr * S1a)
      shr CAggBaseShift);

  if 2 * DG < DA then
      PInt8u(PtrComp(p) + This.Order.g)^ :=
      Int8u((2 * sg * DG + sg * D1a + DG * S1a) shr CAggBaseShift)
  else
      PInt8u(PtrComp(p) + This.Order.g)^ :=
      Int8u((Sada - 2 * (DA - DG) * (SA - sg) + sg * D1a + DG * S1a)
      shr CAggBaseShift);

  if 2 * db < DA then
      PInt8u(PtrComp(p) + This.Order.b)^ :=
      Int8u((2 * SB * db + SB * D1a + db * S1a) shr CAggBaseShift)
  else
      PInt8u(PtrComp(p) + This.Order.b)^ :=
      Int8u((Sada - 2 * (DA - db) * (SA - SB) + SB * D1a + db * S1a)
      shr CAggBaseShift);

  PInt8u(PtrComp(p) + This.Order.a)^ :=
    Int8u(SA + DA - ((SA * DA + CAggBaseMask) shr CAggBaseShift));
end;

function Sd_min(a, b: Cardinal): Cardinal; inline;
begin
  if a < b then
      Result := a
  else
      Result := b;
end;

function Sd_max(a, b: Cardinal): Cardinal; inline;
begin
  if a > b then
      Result := a
  else
      Result := b;
end;

// Dca' = min(Sca.Da, Dca.Sa) + Sca.(1 - Da) + Dca.(1 - Sa)
// Da'  = Sa + Da - Sa.Da
procedure coRgbaDarken(This: TAggPixelFormatProcessor; p: PInt8u;
  SR, sg, SB, SA, Cover: Cardinal);
var
  D1a, S1a, dr, DG, db, DA: Cardinal;
begin
  if Cover < 255 then
    begin
      SR := (SR * Cover + 255) shr 8;
      sg := (sg * Cover + 255) shr 8;
      SB := (SB * Cover + 255) shr 8;
      SA := (SA * Cover + 255) shr 8;
    end;

  D1a := CAggBaseMask - PInt8u(PtrComp(p) + This.Order.a)^;
  S1a := CAggBaseMask - SA;
  dr := PInt8u(PtrComp(p) + This.Order.r)^;
  DG := PInt8u(PtrComp(p) + This.Order.g)^;
  db := PInt8u(PtrComp(p) + This.Order.b)^;
  DA := PInt8u(PtrComp(p) + This.Order.a)^;

  PInt8u(PtrComp(p) + This.Order.r)^ :=
    Int8u((Sd_min(SR * DA, dr * SA) + SR * D1a + dr * S1a) shr CAggBaseShift);
  PInt8u(PtrComp(p) + This.Order.g)^ :=
    Int8u((Sd_min(sg * DA, DG * SA) + sg * D1a + DG * S1a) shr CAggBaseShift);
  PInt8u(PtrComp(p) + This.Order.b)^ :=
    Int8u((Sd_min(SB * DA, db * SA) + SB * D1a + db * S1a) shr CAggBaseShift);
  PInt8u(PtrComp(p) + This.Order.a)^ :=
    Int8u(SA + DA - ((SA * DA + CAggBaseMask) shr CAggBaseShift));
end;

// Dca' = max(Sca.Da, Dca.Sa) + Sca.(1 - Da) + Dca.(1 - Sa)
// Da'  = Sa + Da - Sa.Da
procedure coRgbaLighten(This: TAggPixelFormatProcessor; p: PInt8u;
  SR, sg, SB, SA, Cover: Cardinal);
var
  D1a, S1a, dr, DG, db, DA: Cardinal;
begin
  if Cover < 255 then
    begin
      SR := (SR * Cover + 255) shr 8;
      sg := (sg * Cover + 255) shr 8;
      SB := (SB * Cover + 255) shr 8;
      SA := (SA * Cover + 255) shr 8;
    end;

  D1a := CAggBaseMask - PInt8u(PtrComp(p) + This.Order.a)^;
  S1a := CAggBaseMask - SA;
  dr := PInt8u(PtrComp(p) + This.Order.r)^;
  DG := PInt8u(PtrComp(p) + This.Order.g)^;
  db := PInt8u(PtrComp(p) + This.Order.b)^;
  DA := PInt8u(PtrComp(p) + This.Order.a)^;

  PInt8u(PtrComp(p) + This.Order.r)^ :=
    Int8u((Sd_max(SR * DA, dr * SA) + SR * D1a + dr * S1a) shr CAggBaseShift);
  PInt8u(PtrComp(p) + This.Order.g)^ :=
    Int8u((Sd_max(sg * DA, DG * SA) + sg * D1a + DG * S1a) shr CAggBaseShift);
  PInt8u(PtrComp(p) + This.Order.b)^ :=
    Int8u((Sd_max(SB * DA, db * SA) + SB * D1a + db * S1a) shr CAggBaseShift);
  PInt8u(PtrComp(p) + This.Order.a)^ :=
    Int8u(SA + DA - ((SA * DA + CAggBaseMask) shr CAggBaseShift));
end;

// if Sca.Da + Dca.Sa >= Sa.Da
// Dca' = Sa.Da + Sca.(1 - Da) + Dca.(1 - Sa)
// otherwise
// Dca' = Dca.Sa/(1-Sca/Sa) + Sca.(1 - Da) + Dca.(1 - Sa)
//
// Da'  = Sa + Da - Sa.Da
procedure coRgbaColorDodge(This: TAggPixelFormatProcessor; p: PInt8u;
  SR, sg, SB, SA, Cover: Cardinal);
var
  D1a, S1a, dr, DG, db, DA: Cardinal;
  Drsa, Dgsa, Dbsa, Srda, Sgda, Sbda, Sada: Integer;
begin
  if Cover < 255 then
    begin
      SR := (SR * Cover + 255) shr 8;
      sg := (sg * Cover + 255) shr 8;
      SB := (SB * Cover + 255) shr 8;
      SA := (SA * Cover + 255) shr 8;
    end;

  D1a := CAggBaseMask - PInt8u(PtrComp(p) + This.Order.a)^;
  S1a := CAggBaseMask - SA;
  dr := PInt8u(PtrComp(p) + This.Order.r)^;
  DG := PInt8u(PtrComp(p) + This.Order.g)^;
  db := PInt8u(PtrComp(p) + This.Order.b)^;
  DA := PInt8u(PtrComp(p) + This.Order.a)^;
  Drsa := dr * SA;
  Dgsa := DG * SA;
  Dbsa := db * SA;
  Srda := SR * DA;
  Sgda := sg * DA;
  Sbda := SB * DA;
  Sada := SA * DA;

  if Srda + Drsa >= Sada then
      PInt8u(PtrComp(p) + This.Order.r)^ :=
      Int8u(ShrInt32(Sada + SR * D1a + dr * S1a, CAggBaseShift))
  else
      PInt8u(PtrComp(p) + This.Order.r)^ :=
      Int8u(Drsa div (CAggBaseMask - (SR shl CAggBaseShift) div SA) +
      ((SR * D1a + dr * S1a) shr CAggBaseShift));

  if Sgda + Dgsa >= Sada then
      PInt8u(PtrComp(p) + This.Order.g)^ :=
      Int8u(ShrInt32(Sada + sg * D1a + DG * S1a, CAggBaseShift))
  else
      PInt8u(PtrComp(p) + This.Order.g)^ :=
      Int8u(Dgsa div (CAggBaseMask - (sg shl CAggBaseShift) div SA) +
      ((sg * D1a + DG * S1a) shr CAggBaseShift));

  if Sbda + Dbsa >= Sada then
      PInt8u(PtrComp(p) + This.Order.b)^ :=
      Int8u(ShrInt32(Sada + SB * D1a + db * S1a, CAggBaseShift))
  else
      PInt8u(PtrComp(p) + This.Order.b)^ :=
      Int8u(Dbsa div (CAggBaseMask - (SB shl CAggBaseShift) div SA) +
      ((SB * D1a + db * S1a) shr CAggBaseShift));

  PInt8u(PtrComp(p) + This.Order.a)^ :=
    Int8u(SA + DA - ((SA * DA + CAggBaseMask) shr CAggBaseShift));
end;

// if Sca.Da + Dca.Sa <= Sa.Da
// Dca' = Sca.(1 - Da) + Dca.(1 - Sa)
// otherwise
// Dca' = Sa.(Sca.Da + Dca.Sa - Sa.Da)/Sca + Sca.(1 - Da) + Dca.(1 - Sa)
//
// Da'  = Sa + Da - Sa.Da
procedure coRgbaColorBurn(This: TAggPixelFormatProcessor; p: PInt8u;
  SR, sg, SB, SA, Cover: Cardinal);
var
  D1a, S1a, dr, DG, db, DA: Cardinal;

  Drsa, Dgsa, Dbsa, Srda, Sgda, Sbda, Sada: Integer;

begin
  if Cover < 255 then
    begin
      SR := (SR * Cover + 255) shr 8;
      sg := (sg * Cover + 255) shr 8;
      SB := (SB * Cover + 255) shr 8;
      SA := (SA * Cover + 255) shr 8;
    end;

  D1a := CAggBaseMask - PInt8u(PtrComp(p) + This.Order.a)^;
  S1a := CAggBaseMask - SA;
  dr := PInt8u(PtrComp(p) + This.Order.r)^;
  DG := PInt8u(PtrComp(p) + This.Order.g)^;
  db := PInt8u(PtrComp(p) + This.Order.b)^;
  DA := PInt8u(PtrComp(p) + This.Order.a)^;
  Drsa := dr * SA;
  Dgsa := DG * SA;
  Dbsa := db * SA;
  Srda := SR * DA;
  Sgda := sg * DA;
  Sbda := SB * DA;
  Sada := SA * DA;

  if Srda + Drsa <= Sada then
      PInt8u(PtrComp(p) + This.Order.r)^ :=
      Int8u((SR * D1a + dr * S1a) shr CAggBaseShift)
  else
      PInt8u(PtrComp(p) + This.Order.r)^ :=
      Int8u(ShrInt32(SA * (Srda + Drsa - Sada) div SR + SR * D1a + dr * S1a,
      CAggBaseShift));

  if Sgda + Dgsa <= Sada then
      PInt8u(PtrComp(p) + This.Order.g)^ :=
      Int8u((sg * D1a + DG * S1a) shr CAggBaseShift)
  else
      PInt8u(PtrComp(p) + This.Order.g)^ :=
      Int8u(ShrInt32(SA * (Sgda + Dgsa - Sada) div sg + sg * D1a + DG * S1a,
      CAggBaseShift));

  if Sbda + Dbsa <= Sada then
      PInt8u(PtrComp(p) + This.Order.b)^ :=
      Int8u((SB * D1a + db * S1a) shr CAggBaseShift)
  else
      PInt8u(PtrComp(p) + This.Order.b)^ :=
      Int8u(ShrInt32(SA * (Sbda + Dbsa - Sada) div SB + SB * D1a + db * S1a,
      CAggBaseShift));

  PInt8u(PtrComp(p) + This.Order.a)^ :=
    Int8u(SA + DA - ((SA * DA + CAggBaseMask) shr CAggBaseShift));
end;

// if 2.Sca < Sa
// Dca' = 2.Sca.Dca + Sca.(1 - Da) + Dca.(1 - Sa)
// otherwise
// Dca' = Sa.Da - 2.(Da - Dca).(Sa - Sca) + Sca.(1 - Da) + Dca.(1 - Sa)
//
// Da'  = Sa + Da - Sa.Da
procedure coRgbaHardLight(This: TAggPixelFormatProcessor; p: PInt8u;
  SR, sg, SB, SA, Cover: Cardinal);
var
  D1a, S1a, dr, DG, db, DA, Sada: Cardinal;
begin
  if Cover < 255 then
    begin
      SR := (SR * Cover + 255) shr 8;
      sg := (sg * Cover + 255) shr 8;
      SB := (SB * Cover + 255) shr 8;
      SA := (SA * Cover + 255) shr 8;
    end;

  D1a := CAggBaseMask - PInt8u(PtrComp(p) + This.Order.a)^;
  S1a := CAggBaseMask - SA;
  dr := PInt8u(PtrComp(p) + This.Order.r)^;
  DG := PInt8u(PtrComp(p) + This.Order.g)^;
  db := PInt8u(PtrComp(p) + This.Order.b)^;
  DA := PInt8u(PtrComp(p) + This.Order.a)^;
  Sada := SA * DA;

  if 2 * SR < SA then
      PInt8u(PtrComp(p) + This.Order.r)^ :=
      Int8u((2 * SR * dr + SR * D1a + dr * S1a) shr CAggBaseShift)
  else
      PInt8u(PtrComp(p) + This.Order.r)^ :=
      Int8u((Sada - 2 * (DA - dr) * (SA - SR) + SR * D1a + dr * S1a)
      shr CAggBaseShift);

  if 2 * sg < SA then
      PInt8u(PtrComp(p) + This.Order.g)^ :=
      Int8u((2 * sg * DG + sg * D1a + DG * S1a) shr CAggBaseShift)
  else
      PInt8u(PtrComp(p) + This.Order.g)^ :=
      Int8u((Sada - 2 * (DA - DG) * (SA - sg) + sg * D1a + DG * S1a)
      shr CAggBaseShift);

  if 2 * SB < SA then
      PInt8u(PtrComp(p) + This.Order.b)^ :=
      Int8u((2 * SB * db + SB * D1a + db * S1a) shr CAggBaseShift)
  else
      PInt8u(PtrComp(p) + This.Order.b)^ :=
      Int8u((Sada - 2 * (DA - db) * (SA - SB) + SB * D1a + db * S1a)
      shr CAggBaseShift);

  PInt8u(PtrComp(p) + This.Order.a)^ :=
    Int8u(SA + DA - ((SA * DA + CAggBaseMask) shr CAggBaseShift));
end;

// if 2.Sca < Sa
// Dca' = Dca.(Sa + (1 - Dca/Da).(2.Sca - Sa)) + Sca.(1 - Da) + Dca.(1 - Sa)
// otherwise if 8.Dca <= Da
// Dca' = Dca.(Sa + (1 - Dca/Da).(2.Sca - Sa).(3 - 8.Dca/Da)) + Sca.(1 - Da) + Dca.(1 - Sa)
// otherwise
// Dca' = (Dca.Sa + ((Dca/Da)^(0.5).Da - Dca).(2.Sca - Sa)) + Sca.(1 - Da) + Dca.(1 - Sa)
//
// Da'  = Sa + Da - Sa.Da
procedure coRgbaSoftLight(This: TAggPixelFormatProcessor; p: PInt8u;
  r, g, b, a, Cover: Cardinal);
var
  SR, sg, SB, SA, dr, DG, db, DA: Double;
begin
  SR := (r * Cover) / (CAggBaseMask * 255);
  sg := (g * Cover) / (CAggBaseMask * 255);
  SB := (b * Cover) / (CAggBaseMask * 255);
  SA := (a * Cover) / (CAggBaseMask * 255);
  dr := PInt8u(PtrComp(p) + This.Order.r)^ / CAggBaseMask;
  DG := PInt8u(PtrComp(p) + This.Order.g)^ / CAggBaseMask;
  db := PInt8u(PtrComp(p) + This.Order.b)^ / CAggBaseMask;

  if PInt8u(PtrComp(p) + This.Order.a)^ <> 0 then
      DA := PInt8u(PtrComp(p) + This.Order.a)^ / CAggBaseMask
  else
      DA := 1 / CAggBaseMask;

  if Cover < 255 then
      a := (a * Cover + 255) shr 8;

  if 2 * SR < SA then
      dr := dr * (SA + (1 - dr / DA) * (2 * SR - SA)) + SR * (1 - DA) + dr
      * (1 - SA)
  else if 8 * dr <= DA then
      dr := dr * (SA + (1 - dr / DA) * (2 * SR - SA) * (3 - 8 * dr / DA)) + SR *
      (1 - DA) + dr * (1 - SA)
  else
      dr := (dr * SA + (Sqrt(dr / DA) * DA - dr) * (2 * SR - SA)) + SR * (1 - DA)
      + dr * (1 - SA);

  if 2 * sg < SA then
      DG := DG * (SA + (1 - DG / DA) * (2 * sg - SA)) + sg * (1 - DA) + DG
      * (1 - SA)
  else if 8 * DG <= DA then
      DG := DG * (SA + (1 - DG / DA) * (2 * sg - SA) * (3 - 8 * DG / DA)) + sg *
      (1 - DA) + DG * (1 - SA)
  else
      DG := (DG * SA + (Sqrt(DG / DA) * DA - DG) * (2 * sg - SA)) + sg * (1 - DA)
      + DG * (1 - SA);

  if 2 * SB < SA then
      db := db * (SA + (1 - db / DA) * (2 * SB - SA)) + SB * (1 - DA) + db
      * (1 - SA)
  else if 8 * db <= DA then
      db := db * (SA + (1 - db / DA) * (2 * SB - SA) * (3 - 8 * db / DA)) + SB *
      (1 - DA) + db * (1 - SA)
  else
      db := (db * SA + (Sqrt(db / DA) * DA - db) * (2 * SB - SA)) + SB * (1 - DA)
      + db * (1 - SA);

  PInt8u(PtrComp(p) + This.Order.r)^ := Int8u(Trunc(dr * CAggBaseMask));
  PInt8u(PtrComp(p) + This.Order.g)^ := Int8u(Trunc(DG * CAggBaseMask));
  PInt8u(PtrComp(p) + This.Order.b)^ := Int8u(Trunc(db * CAggBaseMask));
  PInt8u(PtrComp(p) + This.Order.a)^ :=
    Int8u(a + PInt8u(PtrComp(p) + This.Order.a)^ -
    ((a * PInt8u(PtrComp(p) + This.Order.a)^ + CAggBaseMask) shr CAggBaseShift));
end;

// Dca' = Sca + Dca - 2.min(Sca.Da, Dca.Sa)
// Da'  = Sa + Da - Sa.Da
procedure coRgbaDifference(This: TAggPixelFormatProcessor; p: PInt8u;
  SR, sg, SB, SA, Cover: Cardinal);
var
  dr, DG, db, DA: Cardinal;
begin
  if Cover < 255 then
    begin
      SR := (SR * Cover + 255) shr 8;
      sg := (sg * Cover + 255) shr 8;
      SB := (SB * Cover + 255) shr 8;
      SA := (SA * Cover + 255) shr 8;
    end;

  dr := PInt8u(PtrComp(p) + This.Order.r)^;
  DG := PInt8u(PtrComp(p) + This.Order.g)^;
  db := PInt8u(PtrComp(p) + This.Order.b)^;
  DA := PInt8u(PtrComp(p) + This.Order.a)^;

  PInt8u(PtrComp(p) + This.Order.r)^ :=
    Int8u(SR + dr - ((2 * Sd_min(SR * DA, dr * SA)) shr CAggBaseShift));
  PInt8u(PtrComp(p) + This.Order.g)^ :=
    Int8u(sg + DG - ((2 * Sd_min(sg * DA, DG * SA)) shr CAggBaseShift));
  PInt8u(PtrComp(p) + This.Order.b)^ :=
    Int8u(SB + db - ((2 * Sd_min(SB * DA, db * SA)) shr CAggBaseShift));
  PInt8u(PtrComp(p) + This.Order.a)^ :=
    Int8u(SA + DA - ((SA * DA + CAggBaseMask) shr CAggBaseShift));
end;

// Dca' = (Sca.Da + Dca.Sa - 2.Sca.Dca) + Sca.(1 - Da) + Dca.(1 - Sa)
// Da'  = Sa + Da - Sa.Da
procedure coRgbaExclusion(This: TAggPixelFormatProcessor; p: PInt8u;
  SR, sg, SB, SA, Cover: Cardinal);
var
  D1a, S1a, dr, DG, db, DA: Cardinal;
begin
  if Cover < 255 then
    begin
      SR := (SR * Cover + 255) shr 8;
      sg := (sg * Cover + 255) shr 8;
      SB := (SB * Cover + 255) shr 8;
      SA := (SA * Cover + 255) shr 8;
    end;

  D1a := CAggBaseMask - PInt8u(PtrComp(p) + This.Order.a)^;
  S1a := CAggBaseMask - SA;
  dr := PInt8u(PtrComp(p) + This.Order.r)^;
  DG := PInt8u(PtrComp(p) + This.Order.g)^;
  db := PInt8u(PtrComp(p) + This.Order.b)^;
  DA := PInt8u(PtrComp(p) + This.Order.a)^;

  PInt8u(PtrComp(p) + This.Order.r)^ :=
    Int8u((SR * DA + dr * SA - 2 * SR * dr + SR * D1a + dr * S1a)
    shr CAggBaseShift);
  PInt8u(PtrComp(p) + This.Order.g)^ :=
    Int8u((sg * DA + DG * SA - 2 * sg * DG + sg * D1a + DG * S1a)
    shr CAggBaseShift);
  PInt8u(PtrComp(p) + This.Order.b)^ :=
    Int8u((SB * DA + db * SA - 2 * SB * db + SB * D1a + db * S1a)
    shr CAggBaseShift);
  PInt8u(PtrComp(p) + This.Order.a)^ :=
    Int8u(SA + DA - ((SA * DA + CAggBaseMask) shr CAggBaseShift));
end;

procedure coRgbaContrast(This: TAggPixelFormatProcessor; p: PInt8u;
  SR, sg, SB, SA, Cover: Cardinal);
var
  dr, DG, db, DA, D2a, r, g, b: Integer;
  S2a: Cardinal;
begin
  if Cover < 255 then
    begin
      SR := (SR * Cover + 255) shr 8;
      sg := (sg * Cover + 255) shr 8;
      SB := (SB * Cover + 255) shr 8;
      SA := (SA * Cover + 255) shr 8;
    end;

  dr := PInt8u(PtrComp(p) + This.Order.r)^;
  DG := PInt8u(PtrComp(p) + This.Order.g)^;
  db := PInt8u(PtrComp(p) + This.Order.b)^;
  DA := PInt8u(PtrComp(p) + This.Order.a)^;
  D2a := ShrInt32(DA, 1);
  S2a := SA shr 1;

  r := ShrInt32((dr - D2a) * ((SR - S2a) * 2 + CAggBaseMask), CAggBaseShift) + D2a;
  g := ShrInt32((DG - D2a) * ((sg - S2a) * 2 + CAggBaseMask), CAggBaseShift) + D2a;
  b := ShrInt32((db - D2a) * ((SB - S2a) * 2 + CAggBaseMask), CAggBaseShift) + D2a;

  if r < 0 then
      r := 0;

  if g < 0 then
      g := 0;

  if b < 0 then
      b := 0;

  if r > DA then
      PInt8u(PtrComp(p) + This.Order.r)^ := Int8u(Trunc(DA))
  else
      PInt8u(PtrComp(p) + This.Order.r)^ := Int8u(Trunc(r));

  if g > DA then
      PInt8u(PtrComp(p) + This.Order.g)^ := Int8u(Trunc(DA))
  else
      PInt8u(PtrComp(p) + This.Order.g)^ := Int8u(Trunc(g));

  if b > DA then
      PInt8u(PtrComp(p) + This.Order.b)^ := Int8u(Trunc(DA))
  else
      PInt8u(PtrComp(p) + This.Order.b)^ := Int8u(Trunc(b));
end;

// Dca' = (Da - Dca) * Sa + Dca.(1 - Sa)
// Da'  = Sa + Da - Sa.Da
procedure coRgbaInvert(This: TAggPixelFormatProcessor; p: PInt8u;
  SR, sg, SB, SA, Cover: Cardinal);
var
  DA, dr, DG, db, S1a: Integer;
begin
  SA := (SA * Cover + 255) shr 8;

  if SA <> 0 then
    begin
      DA := PInt8u(PtrComp(p) + This.Order.a)^;
      dr := ShrInt32((DA - PInt8u(PtrComp(p) + This.Order.r)^) * SA +
        CAggBaseMask, CAggBaseShift);
      DG := ShrInt32((DA - PInt8u(PtrComp(p) + This.Order.g)^) * SA +
        CAggBaseMask, CAggBaseShift);
      db := ShrInt32((DA - PInt8u(PtrComp(p) + This.Order.b)^) * SA +
        CAggBaseMask, CAggBaseShift);
      S1a := CAggBaseMask - SA;

      PInt8u(PtrComp(p) + This.Order.r)^ :=
        Int8u(dr + ShrInt32(PInt8u(PtrComp(p) + This.Order.r)^ * S1a +
        CAggBaseMask, CAggBaseShift));

      PInt8u(PtrComp(p) + This.Order.g)^ :=
        Int8u(DG + ShrInt32(PInt8u(PtrComp(p) + This.Order.g)^ * S1a +
        CAggBaseMask, CAggBaseShift));

      PInt8u(PtrComp(p) + This.Order.b)^ :=
        Int8u(db + ShrInt32(PInt8u(PtrComp(p) + This.Order.b)^ * S1a +
        CAggBaseMask, CAggBaseShift));

      PInt8u(PtrComp(p) + This.Order.a)^ :=
        Int8u(SA + DA - ShrInt32(SA * DA + CAggBaseMask, CAggBaseShift));
    end;
end;

// Dca' = (Da - Dca) * Sca + Dca.(1 - Sa)
// Da'  = Sa + Da - Sa.Da
procedure coRgbaInvertRgb(This: TAggPixelFormatProcessor; p: PInt8u;
  SR, sg, SB, SA, Cover: Cardinal);
var
  DA, dr, DG, db, S1a: Integer;
begin
  if Cover < 255 then
    begin
      SR := ShrInt32(SR * Cover + 255, 8);
      sg := ShrInt32(sg * Cover + 255, 8);
      SB := ShrInt32(SB * Cover + 255, 8);
      SA := ShrInt32(SA * Cover + 255, 8);
    end;

  if SA <> 0 then
    begin
      DA := PInt8u(PtrComp(p) + This.Order.a)^;
      dr := ShrInt32((DA - PInt8u(PtrComp(p) + This.Order.r)^) * SR +
        CAggBaseMask, CAggBaseShift);
      DG := ShrInt32((DA - PInt8u(PtrComp(p) + This.Order.g)^) * sg +
        CAggBaseMask, CAggBaseShift);
      db := ShrInt32((DA - PInt8u(PtrComp(p) + This.Order.b)^) * SB +
        CAggBaseMask, CAggBaseShift);
      S1a := CAggBaseMask - SA;

      PInt8u(PtrComp(p) + This.Order.r)^ :=
        Int8u(dr + ShrInt32(PInt8u(PtrComp(p) + This.Order.r)^ * S1a +
        CAggBaseMask, CAggBaseShift));

      PInt8u(PtrComp(p) + This.Order.g)^ :=
        Int8u(DG + ShrInt32(PInt8u(PtrComp(p) + This.Order.g)^ * S1a +
        CAggBaseMask, CAggBaseShift));

      PInt8u(PtrComp(p) + This.Order.b)^ :=
        Int8u(db + ShrInt32(PInt8u(PtrComp(p) + This.Order.b)^ * S1a +
        CAggBaseMask, CAggBaseShift));

      PInt8u(PtrComp(p) + This.Order.a)^ :=
        Int8u(SA + DA - ShrInt32(SA * DA + CAggBaseMask, CAggBaseShift));
    end;
end;

const
  CBlendModeTableRgba: array [TAggBlendMode] of TAggFuncBlendPix = (coRgbaClear,
    coRgbaSrc, coRgbaDst, coRgbaSrcOver, coRgbaDstOver, coRgbaSrcIn,
    coRgbaDstIn, coRgbaSrcOut, coRgbaDstOut, coRgbaSrcATop, coRgbaDstATop,
    coRgbaXor, coRgbaPlus, coRgbaMinus, coRgbaMultiply, coRgbaScreen,
    coRgbaOverlay, coRgbaDarken, coRgbaLighten, coRgbaColorDodge,
    coRgbaColorBurn, coRgbaHardLight, coRgbaSoftLight, coRgbaDifference,
    coRgbaExclusion, coRgbaContrast, coRgbaInvert, coRgbaInvertRgb, nil);

procedure BlendModeAdaptorRgba(This: TAggPixelFormatProcessor;
  BlendMode: TAggBlendMode; p: PInt8u; CR, Cg, CB, ca, Cover: Cardinal);
begin
  CBlendModeTableRgba[BlendMode](This, p, (CR * ca + CAggBaseMask) shr CAggBaseShift,
    (Cg * ca + CAggBaseMask) shr CAggBaseShift, (CB * ca + CAggBaseMask) shr CAggBaseShift,
    ca, Cover);
end;

procedure BlendModeAdaptorClipToDestinationRgbaPre(This: TAggPixelFormatProcessor;
  BlendMode: TAggBlendMode; p: PInt8u; CR, Cg, CB, ca, Cover: Cardinal);
var
  DA: Cardinal;
begin
  DA := PInt8u(PtrComp(p) + This.Order.a)^;

  CBlendModeTableRgba[BlendMode](This, p, (CR * DA + CAggBaseMask) shr CAggBaseShift,
    (Cg * DA + CAggBaseMask) shr CAggBaseShift, (CB * DA + CAggBaseMask) shr CAggBaseShift,
    (ca * DA + CAggBaseMask) shr CAggBaseShift, Cover);
end;

end. 
 
 
