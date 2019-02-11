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
unit AggRendererBase;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggRenderingBuffer,
  AggPixelFormat,
  AggColor32;

type
  TAggRendererBase = class
  private
    FClipBox: TRectInteger;
    procedure InitializePixelFormatProcessor;
  protected
    FPixelFormatProcessor: TAggPixelFormatProcessor;
    FOwnPixelFormatProcessor: Boolean;
    function GetWidth: Cardinal;
    function GetHeight: Cardinal;

    function GetXMin: Integer;
    function GetYMin: Integer;
    function GetXMax: Integer;
    function GetYMax: Integer;

    function GetBoundingXMin: Integer; virtual;
    function GetBoundingYMin: Integer; virtual;
    function GetBoundingXMax: Integer; virtual;
    function GetBoundingYMax: Integer; virtual;
  public
    constructor Create(PixelFormatProcessor: TAggPixelFormatProcessor; OwnPixelFormatProcessor: Boolean = False); overload; virtual;
    constructor Create(PixelFormatProcessorClass: TAggPixelFormatProcessorClass; RenderingBuffer: TAggRenderingBuffer); overload; virtual;
    destructor Destroy; override;

    function SetClipBox(x1, y1, x2, y2: Integer): Boolean; overload;
    function SetClipBox(Bounds: TRectInteger): Boolean; overload;
    procedure ResetClipping(Visibility: Boolean); virtual;
    procedure ClipBoxNaked(x1, y1, x2, y2: Integer);

    function InBox(x, y: Integer): Boolean; overload;
    function InBox(Point: TPointInteger): Boolean; overload;

    procedure FirstClipBox; virtual;
    function NextClipBox: Boolean; virtual;

    function GetClipBox: PRectInteger;

    function BoundingClipBox: PRectInteger; virtual;

    procedure Clear(c: PAggColor); overload;
    procedure Clear(c: TAggRgba8); overload;

    procedure CopyPixel(x, y: Integer; c: PAggColor); virtual;
    procedure BlendPixel(x, y: Integer; c: PAggColor; Cover: Int8u); virtual;
    function Pixel(x, y: Integer): TAggColor; virtual;

    procedure CopyHorizontalLine(x1, y, x2: Integer; c: PAggColor); virtual;
    procedure CopyVerticalLine(x, y1, y2: Integer; c: PAggColor); virtual;

    procedure BlendHorizontalLine(x1, y, x2: Integer; c: PAggColor; Cover: Int8u); virtual;
    procedure BlendVerticalLine(x, y1, y2: Integer; c: PAggColor; Cover: Int8u); virtual;

    procedure CopyBar(x1, y1, x2, y2: Integer; c: PAggColor); virtual;
    procedure BlendBar(x1, y1, x2, y2: Integer; c: PAggColor; Cover: Int8u); virtual;

    procedure BlendSolidHSpan(x, y, Len: Integer; c: PAggColor; Covers: PInt8u); virtual;
    procedure BlendSolidVSpan(x, y, Len: Integer; c: PAggColor; Covers: PInt8u); virtual;

    procedure CopyColorHSpan(x, y, Len: Integer; Colors: PAggColor); virtual;
    procedure BlendColorHSpan(x, y, Len: Integer; Colors: PAggColor; Covers: PInt8u; Cover: Int8u = CAggCoverFull); virtual;
    procedure BlendColorVSpan(x, y, Len: Integer; Colors: PAggColor; Covers: PInt8u; Cover: Int8u = CAggCoverFull); virtual;

    procedure CopyColorHSpanNoClip(x, y, Len: Integer; Colors: PAggColor);
    procedure BlendColorHSpanNoClip(x, y, Len: Integer; Colors: PAggColor; Covers: PInt8u; Cover: Int8u = CAggCoverFull);
    procedure BlendColorVSpanNoClip(x, y, Len: Integer; Colors: PAggColor; Covers: PInt8u; Cover: Int8u = CAggCoverFull);

    function ClipRectArea(var Dst, Src: TRectInteger; Wsrc, Hsrc: Integer): TRectInteger;

    procedure CopyFrom(Src: TAggRenderingBuffer; RectSourcePointer: PRectInteger = nil; dx: Integer = 0; dy: Integer = 0); virtual;
    procedure BlendFrom(Src: TAggPixelFormatProcessor; RectSourcePointer: PRectInteger = nil; dx: Integer = 0; dy: Integer = 0; Cover: Int8u = CAggCoverFull); virtual;

    procedure BlendFromColor(Src: TAggPixelFormatProcessor; COLOR: PAggColor; RectSourcePointer: PRectInteger = nil; dx: Integer = 0; dy: Integer = 0; Cover: Int8u = CAggCoverFull);

    procedure BlendFromLUT(Src: TAggPixelFormatProcessor; AColorLUT: PAggColor; RectSourcePointer: PRectInteger = nil; dx: Integer = 0; dy: Integer = 0; Cover: Int8u = CAggCoverFull);

    property PixelFormatProcessor: TAggPixelFormatProcessor read FPixelFormatProcessor;

    property OwnPixelFormatProcessor: Boolean read FOwnPixelFormatProcessor write FOwnPixelFormatProcessor;

    property width: Cardinal read GetWidth;
    property height: Cardinal read GetHeight;

    property XMin: Integer read GetXMin;
    property YMin: Integer read GetYMin;
    property XMax: Integer read GetXMax;
    property YMax: Integer read GetYMax;

    property BoundingXMin: Integer read GetBoundingXMin;
    property BoundingYMin: Integer read GetBoundingYMin;
    property BoundingXMax: Integer read GetBoundingXMax;
    property BoundingYMax: Integer read GetBoundingYMax;
  end;

implementation


{ TAggRendererBase }

constructor TAggRendererBase.Create(PixelFormatProcessor: TAggPixelFormatProcessor;
  OwnPixelFormatProcessor: Boolean = False);
begin
  FPixelFormatProcessor := PixelFormatProcessor;
  FOwnPixelFormatProcessor := OwnPixelFormatProcessor;

  InitializePixelFormatProcessor;
end;

constructor TAggRendererBase.Create(PixelFormatProcessorClass:
  TAggPixelFormatProcessorClass; RenderingBuffer: TAggRenderingBuffer);
begin
  FPixelFormatProcessor := PixelFormatProcessorClass.Create(RenderingBuffer);
  FOwnPixelFormatProcessor := True;

  InitializePixelFormatProcessor;
end;

destructor TAggRendererBase.Destroy;
begin
  if FOwnPixelFormatProcessor then
      FPixelFormatProcessor.Free;

  inherited;
end;

procedure TAggRendererBase.InitializePixelFormatProcessor;
var
  w, h: Integer;
begin
  w := 0;
  h := 0;
  if Assigned(FPixelFormatProcessor) then
    begin
      if (FPixelFormatProcessor.width > 0) then
          w := FPixelFormatProcessor.width - 1;
      if (FPixelFormatProcessor.height > 0) then
          h := FPixelFormatProcessor.height - 1;
    end;

  FClipBox := RectInteger(0, 0, w, h);
end;

function TAggRendererBase.GetWidth: Cardinal;
begin
  Result := FPixelFormatProcessor.width;
end;

function TAggRendererBase.GetHeight: Cardinal;
begin
  Result := FPixelFormatProcessor.height;
end;

procedure TAggRendererBase.ClipBoxNaked(x1, y1, x2, y2: Integer);
begin
  FClipBox.x1 := x1;
  FClipBox.y1 := y1;
  FClipBox.x2 := x2;
  FClipBox.y2 := y2;
end;

function TAggRendererBase.InBox(x, y: Integer): Boolean;
begin
  Result := (x >= FClipBox.x1) and (y >= FClipBox.y1) and
    (x <= FClipBox.x2) and (y <= FClipBox.y2);
end;

function TAggRendererBase.InBox(Point: TPointInteger): Boolean;
begin
  Result := (Point.x >= FClipBox.x1) and (Point.y >= FClipBox.y1) and
    (Point.x <= FClipBox.x2) and (Point.y <= FClipBox.y2);
end;

function TAggRendererBase.SetClipBox(x1, y1, x2, y2: Integer): Boolean;
var
  CB, RectClip: TRectInteger;
begin
  CB := RectInteger(x1, y1, x2, y2);
  CB.Normalize;

  RectClip := RectInteger(0, 0, width - 1, height - 1);
  if CB.Clip(RectClip) then
    begin
      FClipBox := CB;
      Result := True;
      Exit;
    end;

  FClipBox := RectInteger(1, 1, 0, 0);
  Result := False;
end;

function TAggRendererBase.SetClipBox(Bounds: TRectInteger): Boolean;
var
  RectClip: TRectInteger;
begin
  FClipBox := Bounds;
  FClipBox.Normalize;

  RectClip := RectInteger(0, 0, width - 1, height - 1);

  if FClipBox.Clip(RectClip) then
    begin
      Result := True;
      Exit;
    end;

  FClipBox := RectInteger(1, 1, 0, 0);

  Result := False;
end;

procedure TAggRendererBase.ResetClipping;
begin
  if Visibility then
    begin
      FClipBox := RectInteger(0, 0, width - 1, height - 1);
      Exit;
    end;
  FClipBox := RectInteger(1, 1, 0, 0);
end;

procedure TAggRendererBase.FirstClipBox;
begin
end;

function TAggRendererBase.NextClipBox: Boolean;
begin
  Result := False;
end;

function TAggRendererBase.GetClipBox: PRectInteger;
begin
  Result := @FClipBox;
end;

function TAggRendererBase.GetXMin;
begin
  Result := FClipBox.x1;
end;

function TAggRendererBase.GetYMin;
begin
  Result := FClipBox.y1;
end;

function TAggRendererBase.GetXMax;
begin
  Result := FClipBox.x2;
end;

function TAggRendererBase.GetYMax;
begin
  Result := FClipBox.y2;
end;

function TAggRendererBase.BoundingClipBox: PRectInteger;
begin
  Result := @FClipBox;
end;

function TAggRendererBase.GetBoundingXMin;
begin
  Result := FClipBox.x1;
end;

function TAggRendererBase.GetBoundingYMin;
begin
  Result := FClipBox.y1;
end;

function TAggRendererBase.GetBoundingXMax;
begin
  Result := FClipBox.x2;
end;

function TAggRendererBase.GetBoundingYMax;
begin
  Result := FClipBox.y2;
end;

procedure TAggRendererBase.Clear(c: PAggColor);
var
  y: Cardinal;
begin
  if (width > 0) and (height > 0) then
    for y := 0 to GetHeight - 1 do
        FPixelFormatProcessor.CopyHorizontalLine(FPixelFormatProcessor, 0, y,
        width, c);
end;

procedure TAggRendererBase.Clear(c: TAggRgba8);
var
  AggColor32: TAggColor;
  y: Cardinal;
begin
  AggColor32.Rgba8 := c;
  if (width > 0) and (height > 0) then
    for y := 0 to height - 1 do
        FPixelFormatProcessor.CopyHorizontalLine(FPixelFormatProcessor, 0, y,
        width, @AggColor32);
end;

procedure TAggRendererBase.CopyPixel(x, y: Integer; c: PAggColor);
begin
  if InBox(x, y) then
      FPixelFormatProcessor.CopyPixel(FPixelFormatProcessor, x, y, c);
end;

procedure TAggRendererBase.BlendPixel(x, y: Integer; c: PAggColor; Cover: Int8u);
begin
  if InBox(x, y) then
      FPixelFormatProcessor.BlendPixel(FPixelFormatProcessor, x, y, c, Cover);
end;

function TAggRendererBase.Pixel(x, y: Integer): TAggColor;
begin
  if InBox(x, y) then
      Result := FPixelFormatProcessor.Pixel(FPixelFormatProcessor, x, y);
end;

procedure TAggRendererBase.CopyHorizontalLine(x1, y, x2: Integer; c: PAggColor);
var
  t: Integer;
begin
  if x1 > x2 then
    begin
      t := x2;
      x2 := x1;
      x1 := t;
    end;

  if y > GetYMax then
      Exit;

  if y < GetYMin then
      Exit;

  if x1 > GetXMax then
      Exit;

  if x2 < GetXMin then
      Exit;

  if x1 < GetXMin then
      x1 := GetXMin;

  if x2 > GetXMax then
      x2 := GetXMax;

  FPixelFormatProcessor.CopyHorizontalLine(FPixelFormatProcessor, x1, y,
    x2 - x1 + 1, c);
end;

procedure TAggRendererBase.CopyVerticalLine(x, y1, y2: Integer; c: PAggColor);
var
  t: Integer;
begin
  if y1 > y2 then
    begin
      t := y2;
      y2 := y1;
      y1 := t;
    end;

  if x > GetXMax then
      Exit;

  if x < GetXMin then
      Exit;

  if y1 > GetYMax then
      Exit;

  if y2 < GetYMin then
      Exit;

  if y1 < GetYMin then
      y1 := GetYMin;

  if y2 > GetYMax then
      y2 := GetYMax;

  FPixelFormatProcessor.CopyVerticalLine(FPixelFormatProcessor, x, y1,
    y2 - y1 + 1, c);
end;

procedure TAggRendererBase.BlendHorizontalLine(x1, y, x2: Integer; c: PAggColor;
  Cover: Int8u);
var
  t: Integer;
begin
  if x1 > x2 then
    begin
      t := x2;
      x2 := x1;
      x1 := t;
    end;

  if y > GetYMax then
      Exit;

  if y < GetYMin then
      Exit;

  if x1 > GetXMax then
      Exit;

  if x2 < GetXMin then
      Exit;

  if x1 < GetXMin then
      x1 := GetXMin;

  if x2 > GetXMax then
      x2 := GetXMax;

  FPixelFormatProcessor.BlendHorizontalLine(FPixelFormatProcessor, x1, y,
    x2 - x1 + 1, c, Cover);
end;

procedure TAggRendererBase.BlendVerticalLine(x, y1, y2: Integer; c: PAggColor;
  Cover: Int8u);
var
  t: Integer;
begin
  if y1 > y2 then
    begin
      t := y2;
      y2 := y1;
      y1 := t;
    end;

  if x > GetXMax then
      Exit;

  if x < GetXMin then
      Exit;

  if y1 > GetYMax then
      Exit;

  if y2 < GetYMin then
      Exit;

  if y1 < GetYMin then
      y1 := GetYMin;

  if y2 > GetYMax then
      y2 := GetYMax;

  FPixelFormatProcessor.BlendVerticalLine(FPixelFormatProcessor, x, y1,
    y2 - y1 + 1, c, Cover);
end;

procedure TAggRendererBase.CopyBar(x1, y1, x2, y2: Integer; c: PAggColor);
var
  y: Integer;
  RectClip: TRectInteger;
begin
  RectClip := RectInteger(x1, y1, x2, y2);
  RectClip.Normalize;

  if RectClip.Clip(GetClipBox^) then
    begin
      y := RectClip.y1;

      while y <= RectClip.y2 do
        begin
          FPixelFormatProcessor.CopyHorizontalLine(FPixelFormatProcessor,
            RectClip.x1, y, RectClip.x2 - RectClip.x1 + 1, c);

          inc(y);
        end;
    end;
end;

procedure TAggRendererBase.BlendBar(x1, y1, x2, y2: Integer; c: PAggColor;
  Cover: Int8u);
var
  RectClip: TRectInteger;
  y: Integer;
begin
  RectClip := RectInteger(x1, y1, x2, y2);
  RectClip.Normalize;

  if RectClip.Clip(GetClipBox^) then
    begin
      y := RectClip.y1;

      while y <= RectClip.y2 do
        begin
          FPixelFormatProcessor.BlendHorizontalLine(FPixelFormatProcessor,
            RectClip.x1, y, Cardinal(RectClip.x2 - RectClip.x1 + 1), c, Cover);

          inc(y);
        end;
    end;
end;

procedure TAggRendererBase.BlendSolidHSpan(x, y, Len: Integer; c: PAggColor;
  Covers: PInt8u);
begin
  if y > GetYMax then
      Exit;

  if y < GetYMin then
      Exit;

  if x < GetXMin then
    begin
      dec(Len, GetXMin - x);

      if Len <= 0 then
          Exit;

      inc(PtrComp(Covers), (GetXMin - x) * SizeOf(Int8u));

      x := GetXMin;
    end;

  if x + Len > GetXMax then
    begin
      Len := GetXMax - x + 1;

      if Len <= 0 then
          Exit;
    end;

  FPixelFormatProcessor.BlendSolidHSpan(FPixelFormatProcessor, x, y, Len, c,
    Covers);
end;

procedure TAggRendererBase.BlendSolidVSpan(x, y, Len: Integer; c: PAggColor;
  Covers: PInt8u);
begin
  if x > GetXMax then
      Exit;

  if x < GetXMin then
      Exit;

  if y < GetYMin then
    begin
      dec(Len, GetYMin - y);

      if Len <= 0 then
          Exit;

      inc(PtrComp(Covers), (GetYMin - y) * SizeOf(Int8u));

      y := GetYMin;
    end;

  if y + Len > GetYMax then
    begin
      Len := GetYMax - y + 1;

      if Len <= 0 then
          Exit;
    end;

  FPixelFormatProcessor.BlendSolidVSpan(FPixelFormatProcessor, x, y, Len, c,
    Covers);
end;

procedure TAggRendererBase.CopyColorHSpan(x, y, Len: Integer;
  Colors: PAggColor);
var
  d: Integer;
begin
  if y > GetYMax then
      Exit;

  if y < GetYMin then
      Exit;

  if x < GetXMin then
    begin
      d := GetXMin - x;

      dec(Len, d);

      if Len <= 0 then
          Exit;

      inc(PtrComp(Colors), d * SizeOf(TAggColor));

      x := GetXMin;
    end;

  if x + Len > GetXMax then
    begin
      Len := GetXMax - x + 1;

      if Len <= 0 then
          Exit;
    end;

  FPixelFormatProcessor.CopyColorHSpan(FPixelFormatProcessor, x, y, Len,
    Colors);
end;

procedure TAggRendererBase.BlendColorHSpan(x, y, Len: Integer;
  Colors: PAggColor; Covers: PInt8u; Cover: Int8u = CAggCoverFull);
var
  d: Integer;
begin
  if y > GetYMax then
      Exit;

  if y < GetYMin then
      Exit;

  if x < GetXMin then
    begin
      d := GetXMin - x;

      dec(Len, d);

      if Len <= 0 then
          Exit;

      if Covers <> nil then
          inc(PtrComp(Covers), d * SizeOf(Int8u));

      inc(PtrComp(Colors), d * SizeOf(TAggColor));

      x := GetXMin;
    end;

  if x + Len > GetXMax then
    begin
      Len := GetXMax - x + 1;

      if Len <= 0 then
          Exit;
    end;

  FPixelFormatProcessor.BlendColorHSpan(FPixelFormatProcessor, x, y, Len,
    Colors, Covers, Cover);
end;

procedure TAggRendererBase.BlendColorVSpan(x, y, Len: Integer;
  Colors: PAggColor; Covers: PInt8u; Cover: Int8u = CAggCoverFull);
var
  d: Integer;
begin
  if x > GetXMax then
      Exit;

  if x < GetXMin then
      Exit;

  if y < GetYMin then
    begin
      d := GetYMin - y;

      dec(Len, d);

      if Len <= 0 then
          Exit;

      if Covers <> nil then
          inc(PtrComp(Covers), d * SizeOf(Int8u));

      inc(PtrComp(Colors), d * SizeOf(TAggColor));

      y := GetYMin;
    end;

  if y + Len > GetYMax then
    begin
      Len := GetYMax - y + 1;

      if Len <= 0 then
          Exit;
    end;

  FPixelFormatProcessor.BlendColorVSpan(FPixelFormatProcessor, x, y, Len,
    Colors, Covers, Cover);
end;

procedure TAggRendererBase.CopyColorHSpanNoClip(x, y, Len: Integer; Colors: PAggColor);
begin
  // not implemented
end;

procedure TAggRendererBase.BlendColorHSpanNoClip(x, y, Len: Integer;
  Colors: PAggColor; Covers: PInt8u; Cover: Int8u = CAggCoverFull);
begin
  FPixelFormatProcessor.BlendColorHSpan(FPixelFormatProcessor, x, y, Len,
    Colors, Covers, Cover);
end;

procedure TAggRendererBase.BlendColorVSpanNoClip(x, y, Len: Integer;
  Colors: PAggColor; Covers: PInt8u; Cover: Int8u = CAggCoverFull);
begin
  FPixelFormatProcessor.BlendColorVSpan(FPixelFormatProcessor, x, y, Len,
    Colors, Covers, Cover);
end;

function TAggRendererBase.ClipRectArea(var Dst, Src: TRectInteger; Wsrc,
  Hsrc: Integer): TRectInteger;
var
  RectClip, CB: TRectInteger;
begin
  RectClip := RectInteger(0, 0, 0, 0);

  CB := GetClipBox^;

  inc(CB.x2);
  inc(CB.y2);

  if Src.x1 < 0 then
    begin
      Dst.x1 := Dst.x1 - Src.x1;
      Src.x1 := 0;
    end;

  if Src.y1 < 0 then
    begin
      Dst.y1 := Dst.y1 - Src.y1;
      Src.y1 := 0;
    end;

  if Src.x2 > Wsrc then
      Src.x2 := Wsrc;

  if Src.y2 > Hsrc then
      Src.y2 := Hsrc;

  if Dst.x1 < CB.x1 then
    begin
      Src.x1 := Src.x1 + (CB.x1 - Dst.x1);
      Dst.x1 := CB.x1;
    end;

  if Dst.y1 < CB.y1 then
    begin
      Src.y1 := Src.y1 + (CB.y1 - Dst.y1);
      Dst.y1 := CB.y1;
    end;

  if Dst.x2 > CB.x2 then
      Dst.x2 := CB.x2;

  if Dst.y2 > CB.y2 then
      Dst.y2 := CB.y2;

  RectClip.x2 := Dst.x2 - Dst.x1;
  RectClip.y2 := Dst.y2 - Dst.y1;

  if RectClip.x2 > Src.x2 - Src.x1 then
      RectClip.x2 := Src.x2 - Src.x1;

  if RectClip.y2 > Src.y2 - Src.y1 then
      RectClip.y2 := Src.y2 - Src.y1;

  Result := RectClip;
end;

procedure TAggRendererBase.CopyFrom(Src: TAggRenderingBuffer; RectSourcePointer:
  PRectInteger = nil; dx: Integer = 0; dy: Integer = 0);
var
  RectSource, RectDest, RectClip: TRectInteger;
  IncY: Integer;
begin
  RectSource := RectInteger(0, 0, Src.width, Src.height);

  if RectSourcePointer <> nil then
    begin
      RectSource.x1 := RectSourcePointer.x1;
      RectSource.y1 := RectSourcePointer.y1;
      RectSource.x2 := RectSourcePointer.x2 + 1;
      RectSource.y2 := RectSourcePointer.y2 + 1;
    end;

  RectDest := RectInteger(RectSource.x1 + dx, RectSource.y1 + dy,
    RectSource.x2 + dx, RectSource.y2 + dy);

  RectClip := ClipRectArea(RectDest, RectSource, Src.width, Src.height);

  if RectClip.x2 > 0 then
    begin
      IncY := 1;

      if RectDest.y1 > RectSource.y1 then
        begin
          RectSource.y1 := RectSource.y1 + (RectClip.y2 - 1);
          RectDest.y1 := RectDest.y1 + (RectClip.y2 - 1);

          IncY := -1;
        end;

      while RectClip.y2 > 0 do
        begin
          FPixelFormatProcessor.CopyFrom(FPixelFormatProcessor, Src, RectDest.x1,
            RectDest.y1, RectSource.x1, RectSource.y1, RectClip.x2);

          RectDest.y1 := RectDest.y1 + IncY;
          RectSource.y1 := RectSource.y1 + IncY;

          dec(RectClip.y2);
        end;
    end;
end;

procedure TAggRendererBase.BlendFrom(Src: TAggPixelFormatProcessor;
  RectSourcePointer: PRectInteger = nil; dx: Integer = 0; dy: Integer = 0;
  Cover: Int8u = CAggCoverFull);
var
  RectSource, RectDest, RectClip: TRectInteger;
  IncY, X1src, X1dst, Len: Integer;
  Rw: TAggRowDataType;
begin
  if RectSourcePointer <> nil then
    begin
      RectSource.x1 := RectSourcePointer.x1;
      RectSource.y1 := RectSourcePointer.y1;
      RectSource.x2 := RectSourcePointer.x2 + 1;
      RectSource.y2 := RectSourcePointer.y2 + 1;
    end
  else
    begin
      RectSource.x1 := 0;
      RectSource.y1 := 0;
      RectSource.x2 := Src.width;
      RectSource.y2 := Src.height;
    end;

  RectDest := RectInteger(RectSource.x1 + dx, RectSource.y1 + dy,
    RectSource.x2 + dx, RectSource.y2 + dy);

  RectClip := ClipRectArea(RectDest, RectSource, Src.width, Src.height);

  if RectClip.x2 > 0 then
    begin
      IncY := 1;

      if RectDest.y1 > RectSource.y1 then
        begin
          RectSource.y1 := RectSource.y1 + (RectClip.y2 - 1);
          RectDest.y1 := RectDest.y1 + (RectClip.y2 - 1);

          IncY := -1;
        end;

      while RectClip.y2 > 0 do
        begin
          Rw := Src.Row(Src, RectSource.x1, RectSource.y1);

          if Rw.PTR <> nil then
            begin
              X1src := RectSource.x1;
              X1dst := RectDest.x1;
              Len := RectClip.x2;

              if Rw.x1 > X1src then
                begin
                  inc(X1dst, Rw.x1 - X1src);
                  dec(Len, Rw.x1 - X1src);

                  X1src := Rw.x1;
                end;

              if Len > 0 then
                begin
                  if X1src + Len - 1 > Rw.x2 then
                      dec(Len, X1src + Len - Rw.x2 - 1);

                  if Len > 0 then
                      FPixelFormatProcessor.BlendFrom(FPixelFormatProcessor, Src, Rw.PTR,
                      X1dst, RectDest.y1, X1src, RectSource.y1, Len, Cover);
                end;
            end;

          inc(RectDest.y1, IncY);
          inc(RectSource.y1, IncY);
          dec(RectClip.y2);
        end;
    end;
end;

procedure TAggRendererBase.BlendFromColor(Src: TAggPixelFormatProcessor;
  COLOR: PAggColor; RectSourcePointer: PRectInteger = nil; dx: Integer = 0;
  dy: Integer = 0; Cover: Int8u = CAggCoverFull);
var
  RectSource, RectDest, RectClip: TRectInteger;
  Rw: TAggRowDataType;
  IncY, X1src, X1dst, Len: Integer;
begin
  RectSource := RectInteger(0, 0, Src.width, Src.height);

  if RectSourcePointer <> nil then
    begin
      RectSource.x1 := RectSourcePointer.x1;
      RectSource.y1 := RectSourcePointer.y1;
      RectSource.x2 := RectSourcePointer.x2 + 1;
      RectSource.y2 := RectSourcePointer.y2 + 1;
    end;

  RectDest := RectInteger(RectSource.x1 + dx, RectSource.y1 + dy,
    RectSource.x2 + dx, RectSource.y2 + dy);

  RectClip := ClipRectArea(RectDest, RectSource, Src.width, Src.height);

  if RectClip.x2 > 0 then
    begin
      IncY := 1;

      if RectDest.y1 > RectSource.y1 then
        begin
          RectSource.y1 := RectSource.y1 + RectClip.y2 - 1;
          RectDest.y1 := RectDest.y1 + RectClip.y2 - 1;
          IncY := -1;
        end;

      while RectClip.y2 > 0 do
        begin
          Rw := Src.Row(Src, 0, RectSource.y1);

          if Rw.PTR <> nil then
            begin
              X1src := RectSource.x1;
              X1dst := RectDest.x1;
              Len := RectClip.x2;

              if Rw.x1 > X1src then
                begin
                  inc(X1dst, Rw.x1 - X1src);
                  dec(Len, Rw.x1 - X1src);

                  X1src := Rw.x1;
                end;

              if Len > 0 then
                begin
                  if X1src + Len - 1 > Rw.x2 then
                      dec(Len, X1src + Len - Rw.x2 - 1);

                  if Len > 0 then
                      FPixelFormatProcessor.BlendFromColor(FPixelFormatProcessor, Src,
                      COLOR, X1dst, RectDest.y1, X1src, RectSource.y1, Len, Cover);
                end;
            end;

          inc(RectDest.y1, IncY);
          inc(RectSource.y1, IncY);
          dec(RectClip.y2);
        end;
    end;
end;

procedure TAggRendererBase.BlendFromLUT(Src: TAggPixelFormatProcessor;
  AColorLUT: PAggColor; RectSourcePointer: PRectInteger = nil; dx: Integer = 0;
  dy: Integer = 0; Cover: Int8u = CAggCoverFull);
var
  RectSource, RectDest, RectClip: TRectInteger;
  Rw: TAggRowDataType;
  IncY, X1src, X1dst, Len: Integer;
begin
  RectSource := RectInteger(0, 0, Src.width, Src.height);

  if RectSourcePointer <> nil then
    begin
      RectSource.x1 := RectSourcePointer.x1;
      RectSource.y1 := RectSourcePointer.y1;
      RectSource.x2 := RectSourcePointer.x2 + 1;
      RectSource.y2 := RectSourcePointer.y2 + 1;
    end;

  RectDest := RectInteger(RectSource.x1 + dx, RectSource.y1 + dy, RectSource.x2 + dx,
    RectSource.y2 + dy);

  RectClip := ClipRectArea(RectDest, RectSource, Src.width, Src.height);

  if RectClip.x2 > 0 then
    begin
      IncY := 1;

      if RectDest.y1 > RectSource.y1 then
        begin
          RectSource.y1 := RectSource.y1 + RectClip.y2 - 1;
          RectDest.y1 := RectDest.y1 + RectClip.y2 - 1;
          IncY := -1;
        end;

      while RectClip.y2 > 0 do
        begin
          Rw := Src.Row(Src, 0, RectSource.y1);

          if Rw.PTR <> nil then
            begin
              X1src := RectSource.x1;
              X1dst := RectDest.x1;
              Len := RectClip.x2;

              if Rw.x1 > X1src then
                begin
                  inc(X1dst, Rw.x1 - X1src);
                  dec(Len, Rw.x1 - X1src);

                  X1src := Rw.x1;
                end;

              if Len > 0 then
                begin
                  if X1src + Len - 1 > Rw.x2 then
                      dec(Len, X1src + Len - Rw.x2 - 1);

                  if Len > 0 then
                      FPixelFormatProcessor.BlendFromLUT(FPixelFormatProcessor, Src,
                      AColorLUT, X1dst, RectDest.y1, X1src, RectSource.y1, Len, Cover);
                end;
            end;

          inc(RectDest.y1, IncY);
          inc(RectSource.y1, IncY);
          dec(RectClip.y2);
        end;
    end;
end;

end. 
 
 
 
