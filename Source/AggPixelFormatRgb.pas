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
unit AggPixelFormatRgb;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggPixelFormat,
  AggColor32,
  AggRenderingBuffer;

procedure PixelFormatBgr24(out PixelFormatProcessor: TAggPixelFormatProcessor; RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatRgb24(out PixelFormatProcessor: TAggPixelFormatProcessor; RenderingBuffer: TAggRenderingBuffer);

procedure PixelFormatBgr24Pre(out PixelFormatProcessor: TAggPixelFormatProcessor; RenderingBuffer: TAggRenderingBuffer);
procedure PixelFormatRgb24Pre(out PixelFormatProcessor: TAggPixelFormatProcessor; RenderingBuffer: TAggRenderingBuffer);

procedure PixelFormatBgr24Gamma(out PixelFormatProcessor: TAggPixelFormatProcessor; RenderingBuffer: TAggRenderingBuffer; Gamma: TAggGamma);
procedure PixelFormatRgb24Gamma(out PixelFormatProcessor: TAggPixelFormatProcessor; RenderingBuffer: TAggRenderingBuffer; Gamma: TAggGamma);

implementation

function Fmt24Row(This: TAggPixelFormatProcessor; x, y: Integer): TAggRowDataType;
begin
  Result.Initialize(x, This.width - 1,
    PInt8u(PtrComp(This.GetRenderingBuffer.Row(y)) + x * 3 * SizeOf(Int8u)));
end;

procedure Fmt24CopyFrom(This: TAggPixelFormatProcessor; From: TAggRenderingBuffer;
  Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal);
begin
  Move(PInt8u(PtrComp(From.Row(Ysrc)) + Xsrc * 3 * SizeOf(Int8u))^,
    PInt8u(PtrComp(This.GetRenderingBuffer.Row(Ydst)) + Xdst * 3 * SizeOf(Int8u))^,
    SizeOf(Int8u) * 3 * Len);
end;

procedure Order24GammaDirApply(This: TAggPixelFormatProcessor; p: PInt8u);
begin
  PInt8u(PtrComp(p) + This.Order.r)^ :=
    Int8u(This.Apply.dir[PInt8u(PtrComp(p) + This.Order.r)^]);
  PInt8u(PtrComp(p) + This.Order.g)^ :=
    Int8u(This.Apply.dir[PInt8u(PtrComp(p) + This.Order.g)^]);
  PInt8u(PtrComp(p) + This.Order.b)^ :=
    Int8u(This.Apply.dir[PInt8u(PtrComp(p) + This.Order.b)^]);
end;

procedure Order24GammaInvApply(This: TAggPixelFormatProcessor; p: PInt8u);
begin
  PInt8u(PtrComp(p) + This.Order.r)^ :=
    Int8u(This.Apply.Inv[PInt8u(PtrComp(p) + This.Order.r)^]);
  PInt8u(PtrComp(p) + This.Order.g)^ :=
    Int8u(This.Apply.Inv[PInt8u(PtrComp(p) + This.Order.g)^]);
  PInt8u(PtrComp(p) + This.Order.b)^ :=
    Int8u(This.Apply.Inv[PInt8u(PtrComp(p) + This.Order.b)^]);
end;

procedure Order24ForEachPixel(This: TAggPixelFormatProcessor; f: TAggFuncApplyGamma);
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

        inc(PtrComp(p), 3);
        dec(Len);
      until Len = 0;

      inc(y);
    end;
end;

{$INCLUDE AggPixelFormatBgr24.inc}


procedure PixelFormatBgr24(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderBgr;

  PixelFormatProcessor.PixWidth := 3;

  PixelFormatProcessor.CopyPixel := @Bgr24CopyPixel;
  PixelFormatProcessor.BlendPixel := @Bgr24BlendPixel;

  PixelFormatProcessor.Pixel := @Bgr24Pixel;
  PixelFormatProcessor.Row := @Fmt24Row;

  PixelFormatProcessor.CopyHorizontalLine := @Bgr24CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Bgr24CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Bgr24BlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Bgr24BlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Bgr24BlendSolidHorizontalSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Bgr24BlendSolidVerticalSpan;

  PixelFormatProcessor.CopyColorHSpan := @Bgr24CopyColorHorizontalSpan;
  PixelFormatProcessor.CopyColorVSpan := @Bgr24CopyColorVerticalSpan;

  PixelFormatProcessor.BlendColorHSpan := @Bgr24BlendColorHorizontalSpan;
  PixelFormatProcessor.BlendColorVSpan := @Bgr24BlendColorVerticalSpan;

  PixelFormatProcessor.CopyFrom := @Fmt24CopyFrom;
  PixelFormatProcessor.BlendFrom := @Bgr24BlendFrom;

  PixelFormatProcessor.BlendFromColor := @Bgr24BlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Bgr24BlendFromLut;

  PixelFormatProcessor.ForEachPixel := @Order24ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order24GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order24GammaInvApply;
end;

{$INCLUDE AggPixelFormatRgb24.inc}


procedure PixelFormatRgb24(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderRgb;

  PixelFormatProcessor.PixWidth := 3;

  PixelFormatProcessor.CopyPixel := @Rgb24CopyPixel;
  PixelFormatProcessor.BlendPixel := @Rgb24BlendPixel;

  PixelFormatProcessor.Pixel := @Rgb24Pixel;
  PixelFormatProcessor.Row := @Fmt24Row;

  PixelFormatProcessor.CopyHorizontalLine := @Rgb24CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Rgb24CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Rgb24BlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Rgb24BlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Rgb24BlendSolidHorizontalSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Rgb24BlendSolidVerticalSpan;

  PixelFormatProcessor.CopyColorHSpan := @Rgb24CopyColorHorizontalSpan;
  PixelFormatProcessor.CopyColorVSpan := @Rgb24CopyColorVerticalSpan;

  PixelFormatProcessor.BlendColorHSpan := @Rgb24BlendColorHorizontalSpan;
  PixelFormatProcessor.BlendColorVSpan := @Rgb24BlendColorVerticalSpan;

  PixelFormatProcessor.CopyFrom := @Fmt24CopyFrom;
  PixelFormatProcessor.BlendFrom := @Rgb24BlendFrom;

  PixelFormatProcessor.BlendFromColor := @Rgb24BlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Rgb24BlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order24ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order24GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order24GammaInvApply;
end;

{$INCLUDE AggPixelFormatBgr24Pre.inc}


procedure PixelFormatBgr24Pre(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderBgr;

  PixelFormatProcessor.PixWidth := 3;

  PixelFormatProcessor.CopyPixel := @Bgr24CopyPixel;
  PixelFormatProcessor.BlendPixel := @Bgr24PreBlendPixel;

  PixelFormatProcessor.Pixel := @Bgr24Pixel;
  PixelFormatProcessor.Row := @Fmt24Row;

  PixelFormatProcessor.CopyHorizontalLine := @Bgr24CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Bgr24CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Bgr24PreBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Bgr24PreBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Bgr24PreBlendSolidHorizontalSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Bgr24PreBlendSolidVerticalSpan;

  PixelFormatProcessor.CopyColorHSpan := @Bgr24CopyColorHorizontalSpan;
  PixelFormatProcessor.CopyColorVSpan := @Bgr24CopyColorVerticalSpan;

  PixelFormatProcessor.BlendColorHSpan := @Bgr24PreBlendColorHorizontalSpan;
  PixelFormatProcessor.BlendColorVSpan := @Bgr24PreBlendColorVerticalSpan;

  PixelFormatProcessor.CopyFrom := @Fmt24CopyFrom;
  PixelFormatProcessor.BlendFrom := @Bgr24PreBlendFrom;

  PixelFormatProcessor.BlendFromColor := @Bgr24PreBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Bgr24PreBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order24ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order24GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order24GammaInvApply;
end;

{$INCLUDE AggPixelFormatRgb24Pre.inc}


procedure PixelFormatRgb24Pre(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer);
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderRgb;

  PixelFormatProcessor.PixWidth := 3;

  PixelFormatProcessor.CopyPixel := @Rgb24CopyPixel;
  PixelFormatProcessor.BlendPixel := @Rgb24PreBlendPixel;

  PixelFormatProcessor.Pixel := @Rgb24Pixel;
  PixelFormatProcessor.Row := @Fmt24Row;

  PixelFormatProcessor.CopyHorizontalLine := @Rgb24CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Rgb24CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Rgb24PreBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Rgb24PreBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Rgb24PreBlendSolidHorizontalSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Rgb24PreBlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @Rgb24CopyColorHorizontalSpan;
  PixelFormatProcessor.CopyColorVSpan := @Rgb24CopyColorVerticalSpan;

  PixelFormatProcessor.BlendColorHSpan := @Rgb24PreBlendColorHorizontalSpan;
  PixelFormatProcessor.BlendColorVSpan := @Rgb24PreBlendColorVSpan;

  PixelFormatProcessor.CopyFrom := @Fmt24CopyFrom;
  PixelFormatProcessor.BlendFrom := @Rgb24PreBlendFrom;

  PixelFormatProcessor.BlendFromColor := @Rgb24PreBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Rgb24PreBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order24ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order24GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order24GammaInvApply;
end;

{$INCLUDE AggPixelFormatBgr24Gamma.inc}


procedure PixelFormatBgr24Gamma(out PixelFormatProcessor: TAggPixelFormatProcessor;
  RenderingBuffer: TAggRenderingBuffer; Gamma: TAggGamma);
begin
  Assert(Assigned(Gamma));

  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderBgr;
  PixelFormatProcessor.Gamma := Gamma;

  PixelFormatProcessor.PixWidth := 3;

  PixelFormatProcessor.CopyPixel := @Bgr24CopyPixel;
  PixelFormatProcessor.BlendPixel := @Bgr24GammaBlendPixel;

  PixelFormatProcessor.Pixel := @Bgr24Pixel;
  PixelFormatProcessor.Row := @Fmt24Row;

  PixelFormatProcessor.CopyHorizontalLine := @Bgr24CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Bgr24CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Bgr24GammaBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Bgr24GammaBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Bgr24GammaBlendSolidHorizontalSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Bgr24GammaBlendSolidVerticalSpan;

  PixelFormatProcessor.CopyColorHSpan := @Bgr24CopyColorHorizontalSpan;
  PixelFormatProcessor.CopyColorVSpan := @Bgr24CopyColorVerticalSpan;

  PixelFormatProcessor.BlendColorHSpan := @Bgr24GammaBlendColorHorizontalSpan;
  PixelFormatProcessor.BlendColorVSpan := @Bgr24GammaBlendColorVerticalSpan;

  PixelFormatProcessor.CopyFrom := @Fmt24CopyFrom;
  PixelFormatProcessor.BlendFrom := @Bgr24GammaBlendFrom;

  PixelFormatProcessor.BlendFromColor := @Bgr24GammaBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Bgr24GammaBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order24ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order24GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order24GammaInvApply;
end;

{$INCLUDE AggPixelFormatRgb24Gamma.inc}


procedure PixelFormatRgb24Gamma;
begin
  PixelFormatProcessor := TAggPixelFormatProcessor.Create(RenderingBuffer);

  PixelFormatProcessor.Order := CAggOrderRgb;
  PixelFormatProcessor.Gamma := Gamma;

  PixelFormatProcessor.PixWidth := 3;

  PixelFormatProcessor.CopyPixel := @Rgb24CopyPixel;
  PixelFormatProcessor.BlendPixel := @Rgb24GammaBlendPixel;

  PixelFormatProcessor.Pixel := @Rgb24Pixel;
  PixelFormatProcessor.Row := @Fmt24Row;

  PixelFormatProcessor.CopyHorizontalLine := @Rgb24CopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @Rgb24CopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @Rgb24GammaBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @Rgb24GammaBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @Rgb24GammaBlendSolidHorizontalSpan;
  PixelFormatProcessor.BlendSolidVSpan := @Rgb24GammaBlendSolidVerticalSpan;

  PixelFormatProcessor.CopyColorHSpan := @Rgb24CopyColorHorizontalSpan;
  PixelFormatProcessor.CopyColorVSpan := @Rgb24CopyColorVerticalSpan;

  PixelFormatProcessor.BlendColorHSpan := @Rgb24GammaBlendColorHorizontalSpan;
  PixelFormatProcessor.BlendColorVSpan := @Rgb24GammaBlendColorVerticalSpan;

  PixelFormatProcessor.CopyFrom := @Fmt24CopyFrom;
  PixelFormatProcessor.BlendFrom := @Rgb24GammaBlendFrom;

  PixelFormatProcessor.BlendFromColor := @Rgb24GammaBlendFromColor;
  PixelFormatProcessor.BlendFromLUT := @Rgb24GammaBlendFromLUT;

  PixelFormatProcessor.ForEachPixel := @Order24ForEachPixel;
  PixelFormatProcessor.GammaDirApply := @Order24GammaDirApply;
  PixelFormatProcessor.GammaInvApply := @Order24GammaInvApply;
end;

end. 
 
 
