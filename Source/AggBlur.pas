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
unit AggBlur;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface

uses
  AggBasics,
  AggArray,
  AggColor32,
  AggPixelFormat,
  AggPixelFormatTransposer;

type
  TAggStackBlur = class
  private
    FBuffer, FStack: TAggPodVector;
    procedure BlurX(img: TAggPixelFormatProcessor; radius: Cardinal);
    procedure BlurY(img: TAggPixelFormatProcessor; radius: Cardinal);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Blur(img: TAggPixelFormatProcessor; radius: Cardinal);
  end;

  TAggRecursiveBlur = class
  private
    FSum1, FSum2, FBuffer: TAggPodVector;
    procedure BlurX(img: TAggPixelFormatProcessor; radius: Double);
    procedure BlurY(img: TAggPixelFormatProcessor; radius: Double);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Blur(img: TAggPixelFormatProcessor; radius: Double);
  end;

procedure StackBlurGray8(img: TAggPixelFormatProcessor; RX, RY: Cardinal);
procedure StackBlurRgb24(img: TAggPixelFormatProcessor; RX, RY: Cardinal);
procedure StackBlurRgba32(img: TAggPixelFormatProcessor; RX, RY: Cardinal);

implementation

const
  GStackBlur8Mul: array [0..254] of Int16u = (512, 512, 456, 512, 328, 456,
    335, 512, 405, 328, 271, 456, 388, 335, 292, 512, 454, 405, 364, 328, 298,
    271, 496, 456, 420, 388, 360, 335, 312, 292, 273, 512, 482, 454, 428, 405,
    383, 364, 345, 328, 312, 298, 284, 271, 259, 496, 475, 456, 437, 420, 404,
    388, 374, 360, 347, 335, 323, 312, 302, 292, 282, 273, 265, 512, 497, 482,
    468, 454, 441, 428, 417, 405, 394, 383, 373, 364, 354, 345, 337, 328, 320,
    312, 305, 298, 291, 284, 278, 271, 265, 259, 507, 496, 485, 475, 465, 456,
    446, 437, 428, 420, 412, 404, 396, 388, 381, 374, 367, 360, 354, 347, 341,
    335, 329, 323, 318, 312, 307, 302, 297, 292, 287, 282, 278, 273, 269, 265,
    261, 512, 505, 497, 489, 482, 475, 468, 461, 454, 447, 441, 435, 428, 422,
    417, 411, 405, 399, 394, 389, 383, 378, 373, 368, 364, 359, 354, 350, 345,
    341, 337, 332, 328, 324, 320, 316, 312, 309, 305, 301, 298, 294, 291, 287,
    284, 281, 278, 274, 271, 268, 265, 262, 259, 257, 507, 501, 496, 491, 485,
    480, 475, 470, 465, 460, 456, 451, 446, 442, 437, 433, 428, 424, 420, 416,
    412, 408, 404, 400, 396, 392, 388, 385, 381, 377, 374, 370, 367, 363, 360,
    357, 354, 350, 347, 344, 341, 338, 335, 332, 329, 326, 323, 320, 318, 315,
    312, 310, 307, 304, 302, 299, 297, 294, 292, 289, 287, 285, 282, 280, 278,
    275, 273, 271, 269, 267, 265, 263, 261, 259);

  GStackBlur8Shr: array [0..254] of Int8u = (9, 11, 12, 13, 13, 14, 14, 15,
    15, 15, 15, 16, 16, 16, 16, 17, 17, 17, 17, 17, 17, 17, 18, 18, 18, 18, 18,
    18, 18, 18, 18, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 20,
    20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 21, 21,
    21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21,
    21, 21, 21, 21, 21, 21, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
    22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
    22, 22, 22, 22, 22, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23,
    23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23,
    23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23,
    23, 23, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
    24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
    24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
    24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24);

type
  PStackCalculator = ^TStackCalculator;
  TStackCalculator = record
    v, r, g, b, a: Cardinal;
  public
    procedure Clear;

    procedure Add(c: TAggColor); overload;
    procedure Add(c: TAggColor; k: Cardinal); overload;
    procedure Add(const c: TStackCalculator); overload;
    procedure Sub(c: TAggColor); overload;
    procedure Sub(const c: TStackCalculator); overload;

    procedure CalculatePixel(c: PAggColor; ADiv: Cardinal); overload;
    procedure CalculatePixel(c: PAggColor; AMul, AShr: Cardinal); overload;
  end;

  PGaussCalculator = ^TGaussCalculator;
  TGaussCalculator = record
    v, r, g, b, a: Double;
  public
    procedure FromPixel(c: TAggColor);

    procedure Calculate(b1, b2, b3, b4: Double;
      const c1, c2, c3, c4: TGaussCalculator);

    procedure ToPixel(c: PAggColor);
  end;


{ TStackCalculator }

procedure TStackCalculator.Clear;
begin
  v := 0;
  r := 0;
  g := 0;
  b := 0;
  a := 0;
end;

procedure TStackCalculator.Add(c: TAggColor);
begin
  inc(v, c.v);
  inc(r, c.Rgba8.r);
  inc(g, c.Rgba8.g);
  inc(b, c.Rgba8.b);
  inc(a, c.Rgba8.a);
end;

procedure TStackCalculator.Add(const c: TStackCalculator);
begin
  inc(v, c.v);
  inc(r, c.r);
  inc(g, c.g);
  inc(b, c.b);
  inc(a, c.a);
end;

procedure TStackCalculator.Add(c: TAggColor; k: Cardinal);
begin
  inc(v, c.v * k);
  inc(r, c.Rgba8.r * k);
  inc(g, c.Rgba8.g * k);
  inc(b, c.Rgba8.b * k);
  inc(a, c.Rgba8.a * k);
end;

procedure TStackCalculator.Sub(c: TAggColor);
begin
  dec(v, c.v);
  dec(r, c.Rgba8.r);
  dec(g, c.Rgba8.g);
  dec(b, c.Rgba8.b);
  dec(a, c.Rgba8.a);
end;

procedure TStackCalculator.Sub(const c: TStackCalculator);
begin
  dec(v, c.v);
  dec(r, c.r);
  dec(g, c.g);
  dec(b, c.b);
  dec(a, c.a);
end;

procedure TStackCalculator.CalculatePixel(c: PAggColor; ADiv: Cardinal);
begin
  c.v := Int8u(v div ADiv);
  c.Rgba8.r := Int8u(r div ADiv);
  c.Rgba8.g := Int8u(g div ADiv);
  c.Rgba8.b := Int8u(b div ADiv);
  c.Rgba8.a := Int8u(a div ADiv);
end;

procedure TStackCalculator.CalculatePixel(c: PAggColor; AMul, AShr: Cardinal);
begin
  c.v := Int8u((v * AMul) shr AShr);
  c.Rgba8.r := Int8u((r * AMul) shr AShr);
  c.Rgba8.g := Int8u((g * AMul) shr AShr);
  c.Rgba8.b := Int8u((b * AMul) shr AShr);
  c.Rgba8.a := Int8u((a * AMul) shr AShr);
end;


{ TGaussCalculator }

procedure TGaussCalculator.FromPixel(c: TAggColor);
begin
  v := c.v;
  r := c.Rgba8.r;
  g := c.Rgba8.g;
  b := c.Rgba8.b;
  a := c.Rgba8.a;
end;

procedure TGaussCalculator.Calculate(b1, b2, b3, b4: Double;
  const c1, c2, c3, c4: TGaussCalculator);
begin
  v := b1 * c1.v + b2 * c2.v + b3 * c3.v + b4 * c4.v;
  r := b1 * c1.r + b2 * c2.r + b3 * c3.r + b4 * c4.r;
  g := b1 * c1.g + b2 * c2.g + b3 * c3.g + b4 * c4.g;
  b := b1 * c1.b + b2 * c2.b + b3 * c3.b + b4 * c4.b;
  a := b1 * c1.a + b2 * c2.a + b3 * c3.a + b4 * c4.a;
end;

procedure TGaussCalculator.ToPixel(c: PAggColor);
begin
  c.v := Int8u(UnsignedRound(v));
  c.Rgba8.r := Int8u(UnsignedRound(r));
  c.Rgba8.g := Int8u(UnsignedRound(g));
  c.Rgba8.b := Int8u(UnsignedRound(b));
  c.Rgba8.a := Int8u(UnsignedRound(a));
end;


{ TAggStackBlur }

constructor TAggStackBlur.Create;
begin
  FBuffer := TAggPodVector.Create(SizeOf(TAggColor));
  FStack := TAggPodVector.Create(SizeOf(TAggColor));
end;

destructor TAggStackBlur.Destroy;
begin
  FBuffer.Free;
  FStack.Free;
  inherited;
end;

procedure TAggStackBlur.BlurX(img: TAggPixelFormatProcessor; radius: Cardinal);
var
  x, y, xp, i, StackPointer, StackStart, w, h, Wm: Cardinal;
  ADiv, DivSum, MulSum, ShrSum, MaxVal: Cardinal;
  pix: TAggColor;
  StackPix, TempColor: PAggColor;
  Sum, SumIn, SumOut: TStackCalculator;
begin
  if radius < 1 then
    Exit;

  w := img.width;
  h := img.height;
  Wm := w - 1;
  ADiv := radius * 2 + 1;

  DivSum := (radius + 1) * (radius + 1);
  MulSum := 0;
  ShrSum := 0;
  MaxVal := CAggBaseMask;

  if (MaxVal <= 255) and (radius < 255) then
  begin
    MulSum := GStackBlur8Mul[radius];
    ShrSum := GStackBlur8Shr[radius];
  end;

  FBuffer.Allocate(w, 128);
  FStack.Allocate(ADiv, 32);

  y := 0;

  while y < h do
  begin
    Sum.Clear;
    SumIn.Clear;
    SumOut.Clear;

    pix := img.Pixel(img, 0, y);

    i := 0;

    while i <= radius do
    begin
      Move(pix, FStack[i]^, SizeOf(TAggColor));

      Sum.Add(pix, i + 1);
      SumOut.Add(pix);

      inc(i);
    end;

    i := 1;

    while i <= radius do
    begin
      if i > Wm then
        pix := img.Pixel(img, Wm, y)
      else
        pix := img.Pixel(img, i, y);

      Move(pix, FStack[i + radius]^, SizeOf(TAggColor));

      Sum.Add(pix, radius + 1 - i);
      SumIn.Add(pix);

      inc(i);
    end;

    StackPointer := radius;

    x := 0;

    while x < w do
    begin
      if MulSum <> 0 then
        Sum.CalculatePixel(PAggColor(FBuffer[x]), MulSum, ShrSum)
      else
        Sum.CalculatePixel(PAggColor(FBuffer[x]), DivSum);

      Sum.Sub(SumOut);

      StackStart := StackPointer + ADiv - radius;

      if StackStart >= ADiv then
        dec(StackStart, ADiv);

      StackPix := FStack[StackStart];

      SumOut.Sub(StackPix^);

      xp := x + radius + 1;

      if xp > Wm then
        xp := Wm;

      pix := img.Pixel(img, xp, y);

      StackPix^ := pix;

      SumIn.Add(pix);
      Sum.Add(SumIn);

      inc(StackPointer);

      if StackPointer >= ADiv then
        StackPointer := 0;

      StackPix := FStack[StackPointer];

      SumOut.Add(StackPix^);
      SumIn.Sub(StackPix^);

      inc(x);
    end;

    TempColor := FBuffer[0];

    img.CopyColorHSpan(img, 0, y, w, TempColor);

    inc(y);
  end;
end;

procedure TAggStackBlur.BlurY(img: TAggPixelFormatProcessor; radius: Cardinal);
var
  Img2: TAggPixelFormatProcessorTransposer;
begin
  PixelFormatTransposer(Img2, img);
  try
    BlurX(Img2, radius);
  finally
    Img2.Free;
  end;
end;

procedure TAggStackBlur.Blur(img: TAggPixelFormatProcessor; radius: Cardinal);
var
  Img2: TAggPixelFormatProcessorTransposer;
begin
  BlurX(img, radius);
  PixelFormatTransposer(Img2, img);
  try
    BlurX(Img2, radius);
  finally
    Img2.Free;
  end;
end;


{ TAggRecursiveBlur }

constructor TAggRecursiveBlur.Create;
begin
  FSum1 := TAggPodVector.Create(SizeOf(TGaussCalculator));
  FSum2 := TAggPodVector.Create(SizeOf(TGaussCalculator));
  FBuffer := TAggPodVector.Create(SizeOf(TAggColor));
end;

destructor TAggRecursiveBlur.Destroy;
begin
  FSum1.Free;
  FSum2.Free;
  FBuffer.Free;
  inherited;
end;

procedure TAggRecursiveBlur.BlurX(img: TAggPixelFormatProcessor; radius: Double);
var
  s, q, q2, Q3, b0, b1, b2, b3, b: Double;
  w, h, Wm, x, y: Integer;
  c: TGaussCalculator;
  G0, G1: PGaussCalculator;
begin
  if radius < 0.62 then
    Exit;

  if img.width < 3 then
    Exit;

  s := radius * 0.5;

  if s < 2.5 then
    q := 3.97156 - 4.14554 * Sqrt(1 - 0.26891 * s)
  else
    q := 0.98711 * s - 0.96330;

  q2 := q * q;
  Q3 := q2 * q;
  b0 := 1.0 / (1.578250 + 2.444130 * q + 1.428100 * q2 + 0.422205 * Q3);
  b1 := 2.44413 * q + 2.85619 * q2 + 1.26661 * Q3;
  b2 := -1.42810 * q2 + -1.26661 * Q3;
  b3 := 0.422205 * Q3;
  b := 1 - (b1 + b2 + b3) * b0;
  b1 := b1 * b0;
  b2 := b2 * b0;
  b3 := b3 * b0;
  w := img.width;
  h := img.height;
  Wm := w - 1;

  FSum1.Allocate(w);
  FSum2.Allocate(w);
  FBuffer.Allocate(w);

  y := 0;

  while y < h do
  begin
    G0 := PGaussCalculator(FSum1[0]);

    c.FromPixel(img.Pixel(img, 0, y));
    G0.Calculate(b, b1, b2, b3, c, c, c, c);

    G1 := PGaussCalculator(FSum1[1]);

    c.FromPixel(img.Pixel(img, 1, y));
    G1.Calculate(b, b1, b2, b3, c, G0^, G0^, G0^);

    c.FromPixel(img.Pixel(img, 2, y));
    PGaussCalculator(FSum1[2]).Calculate(b, b1, b2, b3, c,
      G1^, G0^, G0^);

    x := 3;

    while x < w do
    begin
      c.FromPixel(img.Pixel(img, x, y));

      PGaussCalculator(FSum1[x]).Calculate(b, b1, b2, b3, c,
        PGaussCalculator(FSum1[x - 1])^,
        PGaussCalculator(FSum1[x - 2])^,
        PGaussCalculator(FSum1[x - 3])^);

      inc(x);
    end;

    G0 := PGaussCalculator(FSum1[Wm]);
    G1 := PGaussCalculator(FSum2[Wm]);

    G1.Calculate(b, b1, b2, b3, G0^, G0^, G0^, G0^);

    PGaussCalculator(FSum2[Wm - 1]).Calculate(b, b1, b2, b3,
      PGaussCalculator(FSum1[Wm - 1])^, G1^, G1^, G1^);

    PGaussCalculator(FSum2[Wm - 2]).Calculate(b, b1, b2, b3,
      PGaussCalculator(FSum1[Wm - 2])^, PGaussCalculator(FSum2[Wm - 1])^,
      G1^, G1^);

    G1.ToPixel(PAggColor(FBuffer[Wm]));

    PGaussCalculator(FSum2[Wm - 1])
      .ToPixel(PAggColor(FBuffer[Wm - 1]));

    PGaussCalculator(FSum2[Wm - 2])
      .ToPixel(PAggColor(FBuffer[Wm - 2]));

    x := Wm - 3;

    while x >= 0 do
    begin
      PGaussCalculator(FSum2[x])
        .Calculate(b, b1, b2, b3, PGaussCalculator(FSum1[x])^,
        PGaussCalculator(FSum2[x + 1])^,
        PGaussCalculator(FSum2[x + 2])^,
        PGaussCalculator(FSum2[x + 3])^);

      PGaussCalculator(FSum2[x]).ToPixel(PAggColor(FBuffer[x]));

      dec(x);
    end;

    img.CopyColorHSpan(img, 0, y, w, FBuffer[0]);

    inc(y);
  end;
end;

procedure TAggRecursiveBlur.BlurY(img: TAggPixelFormatProcessor; radius: Double);
var
  Img2: TAggPixelFormatProcessorTransposer;
begin
  PixelFormatTransposer(Img2, img);
  try
    BlurX(Img2, radius);
  finally
    Img2.Free;
  end;
end;

procedure TAggRecursiveBlur.Blur(img: TAggPixelFormatProcessor; radius: Double);
var
  Img2: TAggPixelFormatProcessorTransposer;
begin
  BlurX(img, radius);
  PixelFormatTransposer(Img2, img);
  try
    BlurX(Img2, radius);
  finally
    Img2.Free;
  end;
end;

procedure StackBlurGray8(img: TAggPixelFormatProcessor; RX, RY: Cardinal);
var
  stride: Integer;
  x, y, xp, yp, i, pix, StackPixel, Sum, SumIn, SumOut: Cardinal;
  StackPointer, StackStart, w, h, Wm, Hm, ADiv, MulSum, ShrSum: Cardinal;
  SourcePixelPointer, DestinationPixelPointer: PInt8u;
  Stack: TAggPodVector;
begin
  w := img.width;
  h := img.height;
  Wm := w - 1;
  Hm := h - 1;

  Stack := TAggPodVector.Create(SizeOf(Int8u));

  if RX > 0 then
  begin
    if RX > 254 then
      RX := 254;

    ADiv := RX * 2 + 1;

    MulSum := GStackBlur8Mul[RX];
    ShrSum := GStackBlur8Shr[RX];

    Stack.Allocate(ADiv);

    y := 0;

    while y < h do
    begin
      Sum := 0;
      SumIn := 0;
      SumOut := 0;

      SourcePixelPointer := img.GetPixelPointer(0, y);
      pix := SourcePixelPointer^;

      i := 0;

      while i <= RX do
      begin
        PInt8u(Stack[i])^ := pix;

        inc(Sum, pix * (i + 1));
        inc(SumOut, pix);

        inc(i);
      end;

      i := 1;

      while i <= RX do
      begin
        if i <= Wm then
          inc(PtrComp(SourcePixelPointer), img.Step);

        pix := SourcePixelPointer^;

        PInt8u(Stack[i + RX])^ := pix;

        inc(Sum, pix * (RX + 1 - i));
        inc(SumIn, pix);

        inc(i);
      end;

      StackPointer := RX;
      xp := RX;

      if xp > Wm then
        xp := Wm;

      SourcePixelPointer := img.GetPixelPointer(xp, y);
      DestinationPixelPointer := img.GetPixelPointer(0, y);

      x := 0;

      while x < w do
      begin
        DestinationPixelPointer^ := Int8u((Sum * MulSum) shr ShrSum);

        inc(PtrComp(DestinationPixelPointer), img.Step);
        dec(Sum, SumOut);

        StackStart := StackPointer + ADiv - RX;

        if StackStart >= ADiv then
          dec(StackStart, ADiv);

        dec(SumOut, PInt8u(Stack[StackStart])^);

        if xp < Wm then
        begin
          inc(PtrComp(SourcePixelPointer), img.Step);

          pix := SourcePixelPointer^;

          inc(xp);
        end;

        PInt8u(Stack[StackStart])^ := pix;

        inc(SumIn, pix);
        inc(Sum, SumIn);

        inc(StackPointer);

        if StackPointer >= ADiv then
          StackPointer := 0;

        StackPixel := PInt8u(Stack[StackPointer])^;

        inc(SumOut, StackPixel);
        dec(SumIn, StackPixel);

        inc(x);
      end;

      inc(y);
    end;
  end;

  if RY > 0 then
  begin
    if RY > 254 then
      RY := 254;

    ADiv := RY * 2 + 1;

    MulSum := GStackBlur8Mul[RY];
    ShrSum := GStackBlur8Shr[RY];

    Stack.Allocate(ADiv);

    stride := img.stride;

    x := 0;

    while x < w do
    begin
      Sum := 0;
      SumIn := 0;
      SumOut := 0;

      SourcePixelPointer := img.GetPixelPointer(x, 0);
      pix := SourcePixelPointer^;

      i := 0;

      while i <= RY do
      begin
        PInt8u(Stack[i])^ := pix;

        inc(Sum, pix * (i + 1));
        inc(SumOut, pix);

        inc(i);
      end;

      i := 1;

      while i <= RY do
      begin
        if i <= Hm then
          inc(PtrComp(SourcePixelPointer), stride);

        pix := SourcePixelPointer^;

        PInt8u(Stack[i + RY])^ := pix;

        inc(Sum, pix * (RY + 1 - i));
        inc(SumIn, pix);

        inc(i);
      end;

      StackPointer := RY;
      yp := RY;

      if yp > Hm then
        yp := Hm;

      SourcePixelPointer := img.GetPixelPointer(x, yp);
      DestinationPixelPointer := img.GetPixelPointer(x, 0);

      y := 0;

      while y < h do
      begin
        DestinationPixelPointer^ := Int8u((Sum * MulSum) shr ShrSum);

        inc(PtrComp(DestinationPixelPointer), stride);
        dec(Sum, SumOut);

        StackStart := StackPointer + ADiv - RY;

        if StackStart >= ADiv then
          dec(StackStart, ADiv);

        dec(SumOut, PInt8u(Stack[StackStart])^);

        if yp < Hm then
        begin
          inc(PtrComp(SourcePixelPointer), stride);

          pix := SourcePixelPointer^;

          inc(yp);
        end;

        PInt8u(Stack[StackStart])^ := pix;

        inc(SumIn, pix);
        inc(Sum, SumIn);

        inc(StackPointer);

        if StackPointer >= ADiv then
          StackPointer := 0;

        StackPixel := PInt8u(Stack[StackPointer])^;

        inc(SumOut, StackPixel);
        dec(SumIn, StackPixel);

        inc(y);
      end;

      inc(x);
    end;
  end;

  Stack.Free;
end;

procedure StackBlurRgb24(img: TAggPixelFormatProcessor; RX, RY: Cardinal);
var
  r, g, b, stride: Integer;
  x, y, xp, yp, i, StackPointer, StackStart: Cardinal;
  SumRed, SumGreen, SumBlue: Cardinal;
  SumInRed, SumInGreen, SumInBlue: Cardinal;
  SumOutRed, SumOutGreen, SumOutBlue: Cardinal;
  w, h, Wm, Hm, ADiv, MulSum, ShrSum: Cardinal;
  SourcePixelPointer, DestinationPixelPointer: PInt8u;
  StackPixelPointer: PAggColor;
  Stack: TAggPodArray;
begin
  r := img.Order.r;
  g := img.Order.g;
  b := img.Order.b;

  w := img.width;
  h := img.height;
  Wm := w - 1;
  Hm := h - 1;

  Stack := TAggPodArray.Create(SizeOf(TAggColor));

  if RX > 0 then
  begin
    if RX > 254 then
      RX := 254;

    ADiv := RX * 2 + 1;
    MulSum := GStackBlur8Mul[RX];
    ShrSum := GStackBlur8Shr[RX];

    Stack.Allocate(ADiv);

    y := 0;

    while y < h do
    begin
      SumRed := 0;
      SumGreen := 0;
      SumBlue := 0;
      SumInRed := 0;
      SumInGreen := 0;
      SumInBlue := 0;
      SumOutRed := 0;
      SumOutGreen := 0;
      SumOutBlue := 0;

      SourcePixelPointer := img.GetPixelPointer(0, y);

      i := 0;

      while i <= RX do
      begin
        StackPixelPointer := Stack[i];

        StackPixelPointer.Rgba8.r := PInt8u(PtrComp(SourcePixelPointer) + r)^;
        StackPixelPointer.Rgba8.g := PInt8u(PtrComp(SourcePixelPointer) + g)^;
        StackPixelPointer.Rgba8.b := PInt8u(PtrComp(SourcePixelPointer) + b)^;

        inc(SumRed, PInt8u(PtrComp(SourcePixelPointer) + r)^ * (i + 1));
        inc(SumGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^ * (i + 1));
        inc(SumBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^ * (i + 1));

        inc(SumOutRed, PInt8u(PtrComp(SourcePixelPointer) + r)^);
        inc(SumOutGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^);
        inc(SumOutBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^);

        inc(i);
      end;

      i := 1;

      while i <= RX do
      begin
        if i <= Wm then
          inc(PtrComp(SourcePixelPointer), img.PixWidth);

        StackPixelPointer := Stack[i + RX];

        StackPixelPointer.Rgba8.r := PInt8u(PtrComp(SourcePixelPointer) + r)^;
        StackPixelPointer.Rgba8.g := PInt8u(PtrComp(SourcePixelPointer) + g)^;
        StackPixelPointer.Rgba8.b := PInt8u(PtrComp(SourcePixelPointer) + b)^;

        inc(SumRed, PInt8u(PtrComp(SourcePixelPointer) + r)^ * (RX + 1 - i));
        inc(SumGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^ * (RX + 1 - i));
        inc(SumBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^ * (RX + 1 - i));

        inc(SumInRed, PInt8u(PtrComp(SourcePixelPointer) + r)^);
        inc(SumInGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^);
        inc(SumInBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^);

        inc(i);
      end;

      StackPointer := RX;
      xp := RX;

      if xp > Wm then
        xp := Wm;

      SourcePixelPointer := img.GetPixelPointer(xp, y);
      DestinationPixelPointer := img.GetPixelPointer(0, y);

      x := 0;

      while x < w do
      begin
        PInt8u(PtrComp(DestinationPixelPointer) + r)^ :=
          Int8u((SumRed * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + g)^ :=
          Int8u((SumGreen * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + b)^ :=
          Int8u((SumBlue * MulSum) shr ShrSum);

        inc(PtrComp(DestinationPixelPointer), img.PixWidth);

        dec(SumRed, SumOutRed);
        dec(SumGreen, SumOutGreen);
        dec(SumBlue, SumOutBlue);

        StackStart := StackPointer + ADiv - RX;

        if StackStart >= ADiv then
          dec(StackStart, ADiv);

        StackPixelPointer := Stack[StackStart];

        dec(SumOutRed, StackPixelPointer.Rgba8.r);
        dec(SumOutGreen, StackPixelPointer.Rgba8.g);
        dec(SumOutBlue, StackPixelPointer.Rgba8.b);

        if xp < Wm then
        begin
          inc(PtrComp(SourcePixelPointer), img.PixWidth);
          inc(xp);
        end;

        StackPixelPointer.Rgba8.r := PInt8u(PtrComp(SourcePixelPointer) + r)^;
        StackPixelPointer.Rgba8.g := PInt8u(PtrComp(SourcePixelPointer) + g)^;
        StackPixelPointer.Rgba8.b := PInt8u(PtrComp(SourcePixelPointer) + b)^;

        inc(SumInRed, PInt8u(PtrComp(SourcePixelPointer) + r)^);
        inc(SumInGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^);
        inc(SumInBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^);

        inc(SumRed, SumInRed);
        inc(SumGreen, SumInGreen);
        inc(SumBlue, SumInBlue);

        inc(StackPointer);

        if StackPointer >= ADiv then
          StackPointer := 0;

        StackPixelPointer := Stack[StackPointer];

        inc(SumOutRed, StackPixelPointer.Rgba8.r);
        inc(SumOutGreen, StackPixelPointer.Rgba8.g);
        inc(SumOutBlue, StackPixelPointer.Rgba8.b);
        dec(SumInRed, StackPixelPointer.Rgba8.r);
        dec(SumInGreen, StackPixelPointer.Rgba8.g);
        dec(SumInBlue, StackPixelPointer.Rgba8.b);

        inc(x);
      end;

      inc(y);
    end;
  end;

  if RY > 0 then
  begin
    if RY > 254 then
      RY := 254;

    ADiv := RY * 2 + 1;

    MulSum := GStackBlur8Mul[RY];
    ShrSum := GStackBlur8Shr[RY];

    Stack.Allocate(ADiv);

    stride := img.stride;

    x := 0;

    while x < w do
    begin
      SumRed := 0;
      SumGreen := 0;
      SumBlue := 0;
      SumInRed := 0;
      SumInGreen := 0;
      SumInBlue := 0;
      SumOutRed := 0;
      SumOutGreen := 0;
      SumOutBlue := 0;

      SourcePixelPointer := img.GetPixelPointer(x, 0);

      i := 0;

      while i <= RY do
      begin
        StackPixelPointer := Stack[i];

        StackPixelPointer.Rgba8.r := PInt8u(PtrComp(SourcePixelPointer) + r)^;
        StackPixelPointer.Rgba8.g := PInt8u(PtrComp(SourcePixelPointer) + g)^;
        StackPixelPointer.Rgba8.b := PInt8u(PtrComp(SourcePixelPointer) + b)^;

        inc(SumRed, PInt8u(PtrComp(SourcePixelPointer) + r)^ * (i + 1));
        inc(SumGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^ * (i + 1));
        inc(SumBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^ * (i + 1));
        inc(SumOutRed, PInt8u(PtrComp(SourcePixelPointer) + r)^);
        inc(SumOutGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^);
        inc(SumOutBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^);

        inc(i);
      end;

      i := 1;

      while i <= RY do
      begin
        if i <= Hm then
          inc(PtrComp(SourcePixelPointer), stride);

        StackPixelPointer := Stack[i + RY];

        StackPixelPointer.Rgba8.r := PInt8u(PtrComp(SourcePixelPointer) + r)^;
        StackPixelPointer.Rgba8.g := PInt8u(PtrComp(SourcePixelPointer) + g)^;
        StackPixelPointer.Rgba8.b := PInt8u(PtrComp(SourcePixelPointer) + b)^;

        inc(SumRed, PInt8u(PtrComp(SourcePixelPointer) + r)^ * (RY + 1 - i));
        inc(SumGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^ * (RY + 1 - i));
        inc(SumBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^ * (RY + 1 - i));
        inc(SumInRed, PInt8u(PtrComp(SourcePixelPointer) + r)^);
        inc(SumInGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^);
        inc(SumInBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^);

        inc(i);
      end;

      StackPointer := RY;
      yp := RY;

      if yp > Hm then
        yp := Hm;

      SourcePixelPointer := img.GetPixelPointer(x, yp);
      DestinationPixelPointer := img.GetPixelPointer(x, 0);

      y := 0;

      while y < h do
      begin
        PInt8u(PtrComp(DestinationPixelPointer) + r)^ :=
          Int8u((SumRed * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + g)^ :=
          Int8u((SumGreen * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + b)^ :=
          Int8u((SumBlue * MulSum) shr ShrSum);

        inc(PtrComp(DestinationPixelPointer), stride);

        dec(SumRed, SumOutRed);
        dec(SumGreen, SumOutGreen);
        dec(SumBlue, SumOutBlue);

        StackStart := StackPointer + ADiv - RY;

        if StackStart >= ADiv then
          dec(StackStart, ADiv);

        StackPixelPointer := Stack[StackStart];

        dec(SumOutRed, StackPixelPointer.Rgba8.r);
        dec(SumOutGreen, StackPixelPointer.Rgba8.g);
        dec(SumOutBlue, StackPixelPointer.Rgba8.b);

        if yp < Hm then
        begin
          inc(PtrComp(SourcePixelPointer), stride);

          inc(yp);
        end;

        StackPixelPointer.Rgba8.r := PInt8u(PtrComp(SourcePixelPointer) + r)^;
        StackPixelPointer.Rgba8.g := PInt8u(PtrComp(SourcePixelPointer) + g)^;
        StackPixelPointer.Rgba8.b := PInt8u(PtrComp(SourcePixelPointer) + b)^;

        inc(SumInRed, PInt8u(PtrComp(SourcePixelPointer) + r)^);
        inc(SumInGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^);
        inc(SumInBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^);
        inc(SumRed, SumInRed);
        inc(SumGreen, SumInGreen);
        inc(SumBlue, SumInBlue);

        inc(StackPointer);

        if StackPointer >= ADiv then
          StackPointer := 0;

        StackPixelPointer := Stack[StackPointer];

        inc(SumOutRed, StackPixelPointer.Rgba8.r);
        inc(SumOutGreen, StackPixelPointer.Rgba8.g);
        inc(SumOutBlue, StackPixelPointer.Rgba8.b);
        dec(SumInRed, StackPixelPointer.Rgba8.r);
        dec(SumInGreen, StackPixelPointer.Rgba8.g);
        dec(SumInBlue, StackPixelPointer.Rgba8.b);

        inc(y);
      end;

      inc(x);
    end;
  end;

  Stack.Free;
end;

procedure StackBlurRgba32(img: TAggPixelFormatProcessor; RX, RY: Cardinal);
var
  r, g, b, a, stride: Integer;
  x, y, xp, yp, i, StackPointer, StackStart: Cardinal;
  SumRed, SumGreen, SumBlue, SumAlpha: Cardinal;
  SumInRed, SumInGreen, SumInBlue, SumInAlpha: Cardinal;
  SumOutRed, SumOutGreen, SumOutBlue, SumOutAlpha: Cardinal;
  w, h, Wm, Hm, ADiv, MulSum, ShrSum: Cardinal;

  SourcePixelPointer, DestinationPixelPointer: PInt8u;
  StackPixelPointer: PAggColor;
  Stack: TAggPodArray;
begin
  r := img.Order.r;
  g := img.Order.g;
  b := img.Order.b;
  a := img.Order.a;

  w := img.width;
  h := img.height;
  Wm := w - 1;
  Hm := h - 1;

  Stack := TAggPodArray.Create(SizeOf(TAggColor));

  if RX > 0 then
  begin
    if RX > 254 then
      RX := 254;

    ADiv := RX * 2 + 1;
    MulSum := GStackBlur8Mul[RX];
    ShrSum := GStackBlur8Shr[RX];

    Stack.Allocate(ADiv);

    y := 0;

    while y < h do
    begin
      SumRed := 0;
      SumGreen := 0;
      SumBlue := 0;
      SumAlpha := 0;
      SumInRed := 0;
      SumInGreen := 0;
      SumInBlue := 0;
      SumInAlpha := 0;
      SumOutRed := 0;
      SumOutGreen := 0;
      SumOutBlue := 0;
      SumOutAlpha := 0;

      SourcePixelPointer := img.GetPixelPointer(0, y);

      i := 0;

      while i <= RX do
      begin
        StackPixelPointer := Stack[i];

        StackPixelPointer.Rgba8.r := PInt8u(PtrComp(SourcePixelPointer) + r)^;
        StackPixelPointer.Rgba8.g := PInt8u(PtrComp(SourcePixelPointer) + g)^;
        StackPixelPointer.Rgba8.b := PInt8u(PtrComp(SourcePixelPointer) + b)^;
        StackPixelPointer.Rgba8.a := PInt8u(PtrComp(SourcePixelPointer) + a)^;

        inc(SumRed, PInt8u(PtrComp(SourcePixelPointer) + r)^ * (i + 1));
        inc(SumGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^ * (i + 1));
        inc(SumBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^ * (i + 1));
        inc(SumAlpha, PInt8u(PtrComp(SourcePixelPointer) + a)^ * (i + 1));

        inc(SumOutRed, PInt8u(PtrComp(SourcePixelPointer) + r)^);
        inc(SumOutGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^);
        inc(SumOutBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^);
        inc(SumOutAlpha, PInt8u(PtrComp(SourcePixelPointer) + a)^);

        inc(i);
      end;

      i := 1;

      while i <= RX do
      begin
        if i <= Wm then
          inc(PtrComp(SourcePixelPointer), img.PixWidth);

        StackPixelPointer := Stack[i + RX];

        StackPixelPointer.Rgba8.r := PInt8u(PtrComp(SourcePixelPointer) + r)^;
        StackPixelPointer.Rgba8.g := PInt8u(PtrComp(SourcePixelPointer) + g)^;
        StackPixelPointer.Rgba8.b := PInt8u(PtrComp(SourcePixelPointer) + b)^;
        StackPixelPointer.Rgba8.a := PInt8u(PtrComp(SourcePixelPointer) + a)^;

        inc(SumRed, PInt8u(PtrComp(SourcePixelPointer) + r)^ * (RX + 1 - i));
        inc(SumGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^ * (RX + 1 - i));
        inc(SumBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^ * (RX + 1 - i));
        inc(SumAlpha, PInt8u(PtrComp(SourcePixelPointer) + a)^ * (RX + 1 - i));

        inc(SumInRed, PInt8u(PtrComp(SourcePixelPointer) + r)^);
        inc(SumInGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^);
        inc(SumInBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^);
        inc(SumInAlpha, PInt8u(PtrComp(SourcePixelPointer) + a)^);

        inc(i);
      end;

      StackPointer := RX;
      xp := RX;

      if xp > Wm then
        xp := Wm;

      SourcePixelPointer := img.GetPixelPointer(xp, y);
      DestinationPixelPointer := img.GetPixelPointer(0, y);

      x := 0;

      while x < w do
      begin
        PInt8u(PtrComp(DestinationPixelPointer) + r)^ :=
          Int8u((SumRed * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + g)^ :=
          Int8u((SumGreen * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + b)^ :=
          Int8u((SumBlue * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + a)^ :=
          Int8u((SumAlpha * MulSum) shr ShrSum);

        inc(PtrComp(DestinationPixelPointer), img.PixWidth);

        dec(SumRed, SumOutRed);
        dec(SumGreen, SumOutGreen);
        dec(SumBlue, SumOutBlue);
        dec(SumAlpha, SumOutAlpha);

        StackStart := StackPointer + ADiv - RX;

        if StackStart >= ADiv then
          dec(StackStart, ADiv);

        StackPixelPointer := Stack[StackStart];

        dec(SumOutRed, StackPixelPointer.Rgba8.r);
        dec(SumOutGreen, StackPixelPointer.Rgba8.g);
        dec(SumOutBlue, StackPixelPointer.Rgba8.b);
        dec(SumOutAlpha, StackPixelPointer.Rgba8.a);

        if xp < Wm then
        begin
          inc(PtrComp(SourcePixelPointer), img.PixWidth);
          inc(xp);
        end;

        StackPixelPointer.Rgba8.r := PInt8u(PtrComp(SourcePixelPointer) + r)^;
        StackPixelPointer.Rgba8.g := PInt8u(PtrComp(SourcePixelPointer) + g)^;
        StackPixelPointer.Rgba8.b := PInt8u(PtrComp(SourcePixelPointer) + b)^;
        StackPixelPointer.Rgba8.a := PInt8u(PtrComp(SourcePixelPointer) + a)^;

        inc(SumInRed, PInt8u(PtrComp(SourcePixelPointer) + r)^);
        inc(SumInGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^);
        inc(SumInBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^);
        inc(SumInAlpha, PInt8u(PtrComp(SourcePixelPointer) + a)^);

        inc(SumRed, SumInRed);
        inc(SumGreen, SumInGreen);
        inc(SumBlue, SumInBlue);
        inc(SumAlpha, SumInAlpha);

        inc(StackPointer);

        if StackPointer >= ADiv then
          StackPointer := 0;

        StackPixelPointer := Stack[StackPointer];

        inc(SumOutRed, StackPixelPointer.Rgba8.r);
        inc(SumOutGreen, StackPixelPointer.Rgba8.g);
        inc(SumOutBlue, StackPixelPointer.Rgba8.b);
        inc(SumOutAlpha, StackPixelPointer.Rgba8.a);
        dec(SumInRed, StackPixelPointer.Rgba8.r);
        dec(SumInGreen, StackPixelPointer.Rgba8.g);
        dec(SumInBlue, StackPixelPointer.Rgba8.b);
        dec(SumInAlpha, StackPixelPointer.Rgba8.a);

        inc(x);
      end;

      inc(y);
    end;
  end;

  if RY > 0 then
  begin
    if RY > 254 then
      RY := 254;

    ADiv := RY * 2 + 1;

    MulSum := GStackBlur8Mul[RY];
    ShrSum := GStackBlur8Shr[RY];

    Stack.Allocate(ADiv);

    stride := img.stride;

    x := 0;

    while x < w do
    begin
      SumRed := 0;
      SumGreen := 0;
      SumBlue := 0;
      SumAlpha := 0;
      SumInRed := 0;
      SumInGreen := 0;
      SumInBlue := 0;
      SumInAlpha := 0;
      SumOutRed := 0;
      SumOutGreen := 0;
      SumOutBlue := 0;
      SumOutAlpha := 0;

      SourcePixelPointer := img.GetPixelPointer(x, 0);

      i := 0;

      while i <= RY do
      begin
        StackPixelPointer := Stack[i];

        StackPixelPointer.Rgba8.r := PInt8u(PtrComp(SourcePixelPointer) + r)^;
        StackPixelPointer.Rgba8.g := PInt8u(PtrComp(SourcePixelPointer) + g)^;
        StackPixelPointer.Rgba8.b := PInt8u(PtrComp(SourcePixelPointer) + b)^;
        StackPixelPointer.Rgba8.a := PInt8u(PtrComp(SourcePixelPointer) + a)^;

        inc(SumRed, PInt8u(PtrComp(SourcePixelPointer) + r)^ * (i + 1));
        inc(SumGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^ * (i + 1));
        inc(SumBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^ * (i + 1));
        inc(SumAlpha, PInt8u(PtrComp(SourcePixelPointer) + a)^ * (i + 1));
        inc(SumOutRed, PInt8u(PtrComp(SourcePixelPointer) + r)^);
        inc(SumOutGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^);
        inc(SumOutBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^);
        inc(SumOutAlpha, PInt8u(PtrComp(SourcePixelPointer) + a)^);

        inc(i);
      end;

      i := 1;

      while i <= RY do
      begin
        if i <= Hm then
          inc(PtrComp(SourcePixelPointer), stride);

        StackPixelPointer := Stack[i + RY];

        StackPixelPointer.Rgba8.r := PInt8u(PtrComp(SourcePixelPointer) + r)^;
        StackPixelPointer.Rgba8.g := PInt8u(PtrComp(SourcePixelPointer) + g)^;
        StackPixelPointer.Rgba8.b := PInt8u(PtrComp(SourcePixelPointer) + b)^;
        StackPixelPointer.Rgba8.a := PInt8u(PtrComp(SourcePixelPointer) + a)^;

        inc(SumRed, PInt8u(PtrComp(SourcePixelPointer) + r)^ * (RY + 1 - i));
        inc(SumGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^ * (RY + 1 - i));
        inc(SumBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^ * (RY + 1 - i));
        inc(SumAlpha, PInt8u(PtrComp(SourcePixelPointer) + a)^ * (RY + 1 - i));
        inc(SumInRed, PInt8u(PtrComp(SourcePixelPointer) + r)^);
        inc(SumInGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^);
        inc(SumInBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^);
        inc(SumInAlpha, PInt8u(PtrComp(SourcePixelPointer) + a)^);

        inc(i);
      end;

      StackPointer := RY;
      yp := RY;

      if yp > Hm then
        yp := Hm;

      SourcePixelPointer := img.GetPixelPointer(x, yp);
      DestinationPixelPointer := img.GetPixelPointer(x, 0);

      y := 0;

      while y < h do
      begin
        PInt8u(PtrComp(DestinationPixelPointer) + r)^ :=
          Int8u((SumRed * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + g)^ :=
          Int8u((SumGreen * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + b)^ :=
          Int8u((SumBlue * MulSum) shr ShrSum);
        PInt8u(PtrComp(DestinationPixelPointer) + a)^ :=
          Int8u((SumAlpha * MulSum) shr ShrSum);

        inc(PtrComp(DestinationPixelPointer), stride);

        dec(SumRed, SumOutRed);
        dec(SumGreen, SumOutGreen);
        dec(SumBlue, SumOutBlue);
        dec(SumAlpha, SumOutAlpha);

        StackStart := StackPointer + ADiv - RY;

        if StackStart >= ADiv then
          dec(StackStart, ADiv);

        StackPixelPointer := Stack[StackStart];

        dec(SumOutRed, StackPixelPointer.Rgba8.r);
        dec(SumOutGreen, StackPixelPointer.Rgba8.g);
        dec(SumOutBlue, StackPixelPointer.Rgba8.b);
        dec(SumOutAlpha, StackPixelPointer.Rgba8.a);

        if yp < Hm then
        begin
          inc(PtrComp(SourcePixelPointer), stride);

          inc(yp);
        end;

        StackPixelPointer.Rgba8.r := PInt8u(PtrComp(SourcePixelPointer) + r)^;
        StackPixelPointer.Rgba8.g := PInt8u(PtrComp(SourcePixelPointer) + g)^;
        StackPixelPointer.Rgba8.b := PInt8u(PtrComp(SourcePixelPointer) + b)^;
        StackPixelPointer.Rgba8.a := PInt8u(PtrComp(SourcePixelPointer) + a)^;

        inc(SumInRed, PInt8u(PtrComp(SourcePixelPointer) + r)^);
        inc(SumInGreen, PInt8u(PtrComp(SourcePixelPointer) + g)^);
        inc(SumInBlue, PInt8u(PtrComp(SourcePixelPointer) + b)^);
        inc(SumInAlpha, PInt8u(PtrComp(SourcePixelPointer) + a)^);
        inc(SumRed, SumInRed);
        inc(SumGreen, SumInGreen);
        inc(SumBlue, SumInBlue);
        inc(SumAlpha, SumInAlpha);

        inc(StackPointer);

        if StackPointer >= ADiv then
          StackPointer := 0;

        StackPixelPointer := Stack[StackPointer];

        inc(SumOutRed, StackPixelPointer.Rgba8.r);
        inc(SumOutGreen, StackPixelPointer.Rgba8.g);
        inc(SumOutBlue, StackPixelPointer.Rgba8.b);
        inc(SumOutAlpha, StackPixelPointer.Rgba8.a);
        dec(SumInRed, StackPixelPointer.Rgba8.r);
        dec(SumInGreen, StackPixelPointer.Rgba8.g);
        dec(SumInBlue, StackPixelPointer.Rgba8.b);
        dec(SumInAlpha, StackPixelPointer.Rgba8.a);

        inc(y);
      end;

      inc(x);
    end;
  end;

  Stack.Free;
end;

end. 
 
 
