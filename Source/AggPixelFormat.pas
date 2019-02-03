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
unit AggPixelFormat;

(*
  ////////////////////////////////////////////////////////////////////////////////
  //                                                                            //
  //  This unit is originaly not the part of the AGG library.                   //
  //  aggPixelFormat unit & pixelformats object substitutes the templetized     //
  //  concept of a pixel polymorphism in c++.                                   //
  //                                                                            //
  ////////////////////////////////////////////////////////////////////////////////
*)



{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface

uses
  AggBasics,
  AggColor32,
  AggRenderingBuffer;

type
  TAggPixelFormatProcessor = class;

  TAggBlendMode = (bmClear, bmSource, bmDestination, bmSourceOver,
    bmDestinationOver, bmSourceIn, bmDestinationIn, bmSourceOut,
    bmDestinationOut, bmSourceATop, bmDestinationATop, bmXor, bmPlus, bmMinus,
    bmMultiply, bmScreen, bmOverlay, bmDarken, bmLighten, bmColorDodge,
    bmColorBurn, bmHardLight, bmSoftLight, bmDifference, bmExclusion,
    bmContrast, bmInvert, bmInvertRgb, bmAlpha);

  TAggFuncBlender  = procedure(This: TAggPixelFormatProcessor; Op: TAggBlendMode; p: PInt8u; CR, Cg, CB, ca, Cover: Cardinal);
  TAggFuncBlendPix = procedure(This: TAggPixelFormatProcessor; p: PInt8u; CR, Cg, CB, alpha, Cover: Cardinal);

  TAggFuncCopyPixel  = procedure(This: TAggPixelFormatProcessor; x, y: Integer; c: PAggColor);
  TAggFuncBlendPixel = procedure(This: TAggPixelFormatProcessor; x, y: Integer; c: PAggColor; Cover: Int8u);

  TAggFuncPixel = function(This: TAggPixelFormatProcessor; x, y: Integer): TAggColor;
  TAggFuncRow   = function(This: TAggPixelFormatProcessor; x, y: Integer): TAggRowDataType;

  TAggFuncCopyHorizontalLine = procedure(This: TAggPixelFormatProcessor; x, y: Integer; Len: Cardinal; c: PAggColor);
  TAggFuncCopyVerticalLine   = procedure(This: TAggPixelFormatProcessor; x, y: Integer; Len: Cardinal; c: PAggColor);

  TAggFuncBlendHorizontalLine = procedure(This: TAggPixelFormatProcessor; x, y: Integer; Len: Cardinal; c: PAggColor; Cover: Int8u);
  TAggFuncBlendVerticalLine   = procedure(This: TAggPixelFormatProcessor; x, y: Integer; Len: Cardinal; c: PAggColor; Cover: Int8u);

  TAggFuncBlendSolidHorizontalSpan = procedure(This: TAggPixelFormatProcessor; x, y: Integer; Len: Cardinal; c: PAggColor; Covers: PInt8u);
  TAggFuncBlendsolidVerticalSpan   = procedure(This: TAggPixelFormatProcessor; x, y: Integer; Len: Cardinal; c: PAggColor; Covers: PInt8u);

  TAggFuncCopyColorHorizontalSpan = procedure(This: TAggPixelFormatProcessor; x, y: Integer; Len: Cardinal; Colors: PAggColor);
  TAggFuncCopyColorVerticalSpan   = procedure(This: TAggPixelFormatProcessor; x, y: Integer; Len: Cardinal; Colors: PAggColor);

  TAggFuncBlendColorHorizontalSpan = procedure(This: TAggPixelFormatProcessor; x, y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
  TAggFuncBlendColorVerticalSpan   = procedure(This: TAggPixelFormatProcessor; x, y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);

  TAggFuncCopyFrom  = procedure(This: TAggPixelFormatProcessor; From: TAggRenderingBuffer; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal);
  TAggFuncBlendFrom = procedure(This: TAggPixelFormatProcessor; From: TAggPixelFormatProcessor; SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal; Cover: Int8u);

  TAggFuncBlendFromColor = procedure(This: TAggPixelFormatProcessor;
    From: TAggPixelFormatProcessor; COLOR: PAggColor; Xdst, Ydst, Xsrc,
    Ysrc: Integer; Len: Cardinal; Cover: Int8u);
  TAggFuncBlendFromLUT = procedure(This: TAggPixelFormatProcessor;
    From: TAggPixelFormatProcessor; ColorLUT: PAggColor; Xdst, Ydst, Xsrc,
    Ysrc: Integer; Len: Cardinal; Cover: Int8u);

  TAggFuncApplyGamma   = procedure(This: TAggPixelFormatProcessor; p: PInt8u);
  TAggFuncForEachPixel = procedure(This: TAggPixelFormatProcessor; f: TAggFuncApplyGamma);

  TAggPixelFormatProcessor = class
  protected
    FGamma, FApply: TAggGamma;
    FOrder: TAggOrder;
    FStep, FOffset, FPixWidth: Cardinal;
    FBlendMode: TAggBlendMode;
    FRenderingBuffer: TAggRenderingBuffer;
    function GetStride: Integer;
  protected
    function GetWidth: Cardinal; virtual;
    function GetHeight: Cardinal; virtual;
  public
    Blender: TAggFuncBlender;

    CopyPixel: TAggFuncCopyPixel;
    BlendPixel: TAggFuncBlendPixel;

    Pixel: TAggFuncPixel;
    Row: TAggFuncRow;

    CopyHorizontalLine: TAggFuncCopyHorizontalLine;
    CopyVerticalLine: TAggFuncCopyVerticalLine;

    BlendHorizontalLine: TAggFuncBlendHorizontalLine;
    BlendVerticalLine: TAggFuncBlendVerticalLine;

    BlendSolidHSpan: TAggFuncBlendSolidHorizontalSpan;
    BlendSolidVSpan: TAggFuncBlendsolidVerticalSpan;

    CopyColorHSpan: TAggFuncCopyColorHorizontalSpan;
    CopyColorVSpan: TAggFuncCopyColorVerticalSpan;

    BlendColorHSpan: TAggFuncBlendColorHorizontalSpan;
    BlendColorVSpan: TAggFuncBlendColorVerticalSpan;

    CopyFrom: TAggFuncCopyFrom;
    BlendFrom: TAggFuncBlendFrom;

    BlendFromColor: TAggFuncBlendFromColor;
    BlendFromLUT: TAggFuncBlendFromLUT;

    ForEachPixel: TAggFuncForEachPixel;
    GammaDirApply, GammaInvApply: TAggFuncApplyGamma;

    PixelPreMultiply, PixelDeMultiply: TAggFuncApplyGamma;

    constructor Create(rb: TAggRenderingBuffer; st: Cardinal = 1; Off: Cardinal = 0);

    function Attach(PixF: TAggPixelFormatProcessor; x1, y1, x2, y2: Integer): Boolean;
    function GetPixelPointer(x, y: Integer): PInt8u;
    function GetRowPointer(y: Integer): PInt8u;

    procedure ApplyGammaDir(Gamma: TAggGamma; Order: TAggOrder);
    procedure ApplyGammaInv(Gamma: TAggGamma; Order: TAggOrder);

    function GetRenderingBuffer: TAggRenderingBuffer;

    procedure PreMultiply;
    procedure DeMultiply;

    property RenderingBuffer: TAggRenderingBuffer read FRenderingBuffer;
    property Gamma: TAggGamma read FGamma write FGamma;
    property Apply: TAggGamma read FApply;
    property Order: TAggOrder read FOrder write FOrder;

    property BlendMode: TAggBlendMode read FBlendMode write FBlendMode;
    property Step: Cardinal read FStep;
    property Offset: Cardinal read FOffset;
    property width: Cardinal read GetWidth;
    property height: Cardinal read GetHeight;
    property stride: Integer read GetStride;
    property PixWidth: Cardinal read FPixWidth write FPixWidth;
  end;

  TAggPixelFormatProcessorClass = class of TAggPixelFormatProcessor;

  DefinePixelFormat        = procedure(out PixF: TAggPixelFormatProcessor; rb: TAggRenderingBuffer);
  DefinePixelFormatGamma   = procedure(out PixF: TAggPixelFormatProcessor; rb: TAggRenderingBuffer; Gamma: TAggGamma);
  DefinePixelFormatBlender = procedure(out PixF: TAggPixelFormatProcessor; rb: TAggRenderingBuffer; BL: TAggFuncBlender; Order: TAggOrder);

procedure PixelFormatUndefined(var PixF: TAggPixelFormatProcessor);

implementation


{ TAggPixelFormatProcessor }

constructor TAggPixelFormatProcessor.Create(rb: TAggRenderingBuffer; st: Cardinal = 1;
  Off: Cardinal = 0);
begin
  FRenderingBuffer := rb;
  FGamma := nil;
  FApply := nil;
  FOrder := CAggOrderBgra;

  FBlendMode := bmSourceOver;
  FStep := st;
  FOffset := Off;

  FPixWidth := 0;

  Blender := nil;

  CopyPixel := nil;
  BlendPixel := nil;

  Pixel := nil;
  Row := nil;

  CopyHorizontalLine := nil;
  CopyVerticalLine := nil;

  BlendHorizontalLine := nil;
  BlendVerticalLine := nil;

  BlendSolidHSpan := nil;
  BlendSolidVSpan := nil;

  CopyColorHSpan := nil;
  CopyColorVSpan := nil;

  BlendColorHSpan := nil;
  BlendColorVSpan := nil;

  CopyFrom := nil;
  BlendFrom := nil;

  BlendFromColor := nil;
  BlendFromLUT := nil;

  ForEachPixel := nil;
  GammaDirApply := nil;
  GammaInvApply := nil;

  PixelPreMultiply := nil;
  PixelDeMultiply := nil;
end;

function TAggPixelFormatProcessor.Attach(PixF: TAggPixelFormatProcessor;
  x1, y1, x2, y2: Integer): Boolean;
var
  r, c: TRectInteger;
  stride, y: Integer;
begin
  r := RectInteger(x1, y1, x2, y2);
  c := RectInteger(0, 0, PixF.width - 1, PixF.height - 1);

  if r.Clip(c) then
    begin
      stride := PixF.FRenderingBuffer.stride;

      if stride < 0 then
          y := r.y2
      else
          y := r.y1;

      FRenderingBuffer.Attach(PixF.GetPixelPointer(r.x1, y), (r.x2 - r.x1) + 1,
        (r.y2 - r.y1) + 1, stride);

      Result := True;

    end
  else
      Result := False;
end;

function TAggPixelFormatProcessor.GetPixelPointer(x, y: Integer): PInt8u;
begin
  Result := PInt8u(PtrComp(FRenderingBuffer.Row(y)) + x * FPixWidth + FOffset);
end;

function TAggPixelFormatProcessor.GetRowPointer(y: Integer): PInt8u;
begin
  Result := FRenderingBuffer.Row(y);
end;

function TAggPixelFormatProcessor.GetWidth;
begin
  Result := FRenderingBuffer.width;
end;

function TAggPixelFormatProcessor.GetHeight;
begin
  Result := FRenderingBuffer.height;
end;

function TAggPixelFormatProcessor.GetStride;
begin
  Result := FRenderingBuffer.stride;
end;

function TAggPixelFormatProcessor.GetRenderingBuffer: TAggRenderingBuffer;
begin
  Result := FRenderingBuffer;
end;

procedure TAggPixelFormatProcessor.ApplyGammaDir(Gamma: TAggGamma; Order: TAggOrder);
begin
  FApply := Gamma;
  FOrder := Order;

  ForEachPixel(Self, @GammaDirApply);
end;

procedure TAggPixelFormatProcessor.ApplyGammaInv(Gamma: TAggGamma; Order: TAggOrder);
begin
  FApply := Gamma;
  FOrder := Order;

  ForEachPixel(Self, @GammaInvApply);
end;

procedure TAggPixelFormatProcessor.PreMultiply;
begin
  ForEachPixel(@Self, @PixelPreMultiply);
end;

procedure TAggPixelFormatProcessor.DeMultiply;
begin
  ForEachPixel(@Self, @PixelDeMultiply);
end;

procedure PixelFormatUndefined(var PixF: TAggPixelFormatProcessor);
begin
  PixF := TAggPixelFormatProcessor.Create(nil);

  PixF.CopyPixel := nil;
  PixF.BlendPixel := nil;

  PixF.Pixel := nil;
  PixF.Row := nil;

  PixF.CopyHorizontalLine := nil;
  PixF.CopyVerticalLine := nil;

  PixF.BlendHorizontalLine := nil;
  PixF.BlendVerticalLine := nil;

  PixF.BlendSolidHSpan := nil;
  PixF.BlendSolidVSpan := nil;

  PixF.CopyColorHSpan := nil;
  PixF.CopyColorVSpan := nil;

  PixF.BlendColorHSpan := nil;
  PixF.BlendColorVSpan := nil;

  PixF.CopyFrom := nil;
  PixF.BlendFrom := nil;

  PixF.BlendFromColor := nil;
  PixF.BlendFromLUT := nil;

  PixF.ForEachPixel := nil;
  PixF.GammaDirApply := nil;
  PixF.GammaInvApply := nil;
end;

end. 
 
 
 
