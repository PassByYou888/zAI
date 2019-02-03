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
unit AggPixelFormatTransposer;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggPixelFormat,
  AggColor32,
  AggRenderingBuffer;

type
  TAggPixelFormatProcessorTransposer = class(TAggPixelFormatProcessor)
  private
    FPixelFormats: TAggPixelFormatProcessor;
  protected
    function GetWidth: Cardinal; override;
    function GetHeight: Cardinal; override;
  public
    constructor Create(Source: TAggPixelFormatProcessor);

    procedure Attach(Source: TAggPixelFormatProcessor);
  end;

procedure PixelFormatTransposer(var PixelFormatProcessor: TAggPixelFormatProcessorTransposer; Source: TAggPixelFormatProcessor);

implementation


{ TAggPixelFormatProcessorTransposer }

constructor TAggPixelFormatProcessorTransposer.Create(
  Source: TAggPixelFormatProcessor);
begin
  inherited Create(nil);

  Attach(Source);
end;

procedure TAggPixelFormatProcessorTransposer.Attach(
  Source: TAggPixelFormatProcessor);
begin
  FPixelFormats := Source;
  FRenderingBuffer := Source.RenderingBuffer;

  FGamma := Source.Gamma;
  FApply := Source.Apply;
  FOrder := Source.Order;

  FBlendMode := Source.BlendMode;
  FStep := Source.Step;
  FOffset := Source.Offset;
  FPixWidth := Source.PixWidth;

  Blender := Source.Blender;
  Row := Source.Row;

  CopyFrom := Source.CopyFrom;
  BlendFrom := Source.BlendFrom;

  ForEachPixel := Source.ForEachPixel;
  GammaDirApply := Source.GammaDirApply;
  GammaInvApply := Source.GammaInvApply;
end;

function TAggPixelFormatProcessorTransposer.GetWidth: Cardinal;
begin
  Result := FPixelFormats.height;
end;

function TAggPixelFormatProcessorTransposer.GetHeight: Cardinal;
begin
  Result := FPixelFormats.width;
end;

procedure TransposerCopyPixel(This: TAggPixelFormatProcessorTransposer;
  x, y: Integer; c: PAggColor);
begin
  This.FPixelFormats.CopyPixel(This.FPixelFormats, y, x, c);
end;

procedure TransposerBlendPixel(This: TAggPixelFormatProcessorTransposer;
  x, y: Integer; c: PAggColor; Cover: Int8u);
begin
  This.FPixelFormats.BlendPixel(This.FPixelFormats, y, x, c, Cover);
end;

function TransposerPixel(This: TAggPixelFormatProcessorTransposer;
  x, y: Integer): TAggColor;
begin
  Result := This.FPixelFormats.Pixel(This.FPixelFormats, y, x);
end;

procedure TransposerCopyHorizontalLine(This: TAggPixelFormatProcessorTransposer;
  x, y: Integer; Len: Cardinal; c: PAggColor);
begin
  This.FPixelFormats.CopyVerticalLine(This.FPixelFormats, y, x, Len, c);
end;

procedure TransposerCopyVerticalLine(This: TAggPixelFormatProcessorTransposer;
  x, y: Integer; Len: Cardinal; c: PAggColor);
begin
  This.FPixelFormats.CopyHorizontalLine(This.FPixelFormats, y, x, Len, c);
end;

procedure TransposerBlendHorizontalLine(This: TAggPixelFormatProcessorTransposer;
  x, y: Integer; Len: Cardinal; c: PAggColor; Cover: Int8u);
begin
  This.FPixelFormats.BlendVerticalLine(This.FPixelFormats, y, x, Len, c, Cover);
end;

procedure TransposerBlendVerticalLine(This: TAggPixelFormatProcessorTransposer;
  x, y: Integer; Len: Cardinal; c: PAggColor; Cover: Int8u);
begin
  This.FPixelFormats.BlendHorizontalLine(This.FPixelFormats, y, x, Len, c, Cover);
end;

procedure TransposerBlendSolidHSpan(This: TAggPixelFormatProcessorTransposer;
  x, y: Integer; Len: Cardinal; c: PAggColor; Covers: PInt8u);
begin
  This.FPixelFormats.BlendSolidVSpan(This.FPixelFormats, y, x, Len, c, Covers);
end;

procedure TransposerBlendSolidVSpan(This: TAggPixelFormatProcessorTransposer;
  x, y: Integer; Len: Cardinal; c: PAggColor; Covers: PInt8u);
begin
  This.FPixelFormats.BlendSolidHSpan(This.FPixelFormats, y, x, Len, c, Covers);
end;

procedure TransposerCopyColorHSpan(This: TAggPixelFormatProcessorTransposer;
  x, y: Integer; Len: Cardinal; Colors: PAggColor);
begin
  This.FPixelFormats.CopyColorVSpan(This.FPixelFormats, y, x, Len, Colors);
end;

procedure TransposerCopyColorVSpan(This: TAggPixelFormatProcessorTransposer;
  x, y: Integer; Len: Cardinal; Colors: PAggColor);
begin
  This.FPixelFormats.CopyColorHSpan(This.FPixelFormats, y, x, Len, Colors);
end;

procedure TransposerBlendColorHSpan(This: TAggPixelFormatProcessorTransposer;
  x, y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u;
  Cover: Int8u);
begin
  This.FPixelFormats.BlendColorVSpan(This.FPixelFormats, y, x, Len, Colors,
    Covers, Cover);
end;

procedure TransposerBlendColorVSpan(This: TAggPixelFormatProcessorTransposer;
  x, y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u;
  Cover: Int8u);
begin
  This.FPixelFormats.BlendColorHSpan(This.FPixelFormats, y, x, Len, Colors,
    Covers, Cover);
end;

procedure PixelFormatTransposer(
  var PixelFormatProcessor: TAggPixelFormatProcessorTransposer;
  Source: TAggPixelFormatProcessor);
begin
  PixelFormatProcessor := TAggPixelFormatProcessorTransposer.Create(Source);

  PixelFormatProcessor.CopyPixel := @TransposerCopyPixel;
  PixelFormatProcessor.BlendPixel := @TransposerBlendPixel;

  PixelFormatProcessor.Pixel := @TransposerPixel;

  PixelFormatProcessor.CopyHorizontalLine := @TransposerCopyHorizontalLine;
  PixelFormatProcessor.CopyVerticalLine := @TransposerCopyVerticalLine;

  PixelFormatProcessor.BlendHorizontalLine := @TransposerBlendHorizontalLine;
  PixelFormatProcessor.BlendVerticalLine := @TransposerBlendVerticalLine;

  PixelFormatProcessor.BlendSolidHSpan := @TransposerBlendSolidHSpan;
  PixelFormatProcessor.BlendSolidVSpan := @TransposerBlendSolidVSpan;

  PixelFormatProcessor.CopyColorHSpan := @TransposerCopyColorHSpan;
  PixelFormatProcessor.CopyColorVSpan := @TransposerCopyColorVSpan;

  PixelFormatProcessor.BlendColorHSpan := @TransposerBlendColorHSpan;
  PixelFormatProcessor.BlendColorVSpan := @TransposerBlendColorVSpan;
end;

end. 
 
 
 
