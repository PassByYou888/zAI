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
unit AggRendererMultiClip;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggArray,
  AggColor32,
  AggRenderingBuffer,
  AggRendererBase,
  AggPixelFormat;

type
  TAggRendererMultiClip = class(TAggRendererBase)
  private
    FClip: TAggPodDeque;
    FCurrentClibBoxIndex: Cardinal;
    FBounds: TRectInteger;
  protected
    function GetBoundingXMin: Integer; override;
    function GetBoundingYMin: Integer; override;
    function GetBoundingXMax: Integer; override;
    function GetBoundingYMax: Integer; override;
  public
    constructor Create(PixelFormatProcessor: TAggPixelFormatProcessor; OwnPixelFormatProcessor: Boolean = False); override;
    destructor Destroy; override;

    function GetBoundingClipBox: PRectInteger; virtual;

    procedure FirstClipBox; override;
    function NextClipBox: Boolean; override;

    procedure ResetClipping(Visibility: Boolean); override;

    procedure AddClipBox(x1, y1, x2, y2: Integer); overload;
    procedure AddClipBox(Rect: TRectInteger); overload;

    procedure CopyPixel(x, y: Integer; c: PAggColor); override;
    procedure BlendPixel(x, y: Integer; c: PAggColor; Cover: Int8u); override;
    function Pixel(x, y: Integer): TAggColor; override;

    procedure CopyHorizontalLine(x1, y, x2: Integer; c: PAggColor); override;
    procedure CopyVerticalLine(x, y1, y2: Integer; c: PAggColor); override;

    procedure BlendHorizontalLine(x1, y, x2: Integer; c: PAggColor; Cover: Int8u); override;
    procedure BlendVerticalLine(x, y1, y2: Integer; c: PAggColor; Cover: Int8u); override;

    procedure CopyBar(x1, y1, x2, y2: Integer; c: PAggColor); override;
    procedure BlendBar(x1, y1, x2, y2: Integer; c: PAggColor; Cover: Int8u); override;

    procedure BlendSolidHSpan(x, y, Len: Integer; c: PAggColor; Covers: PInt8u); override;
    procedure BlendSolidVSpan(x, y, Len: Integer; c: PAggColor; Covers: PInt8u); override;

    procedure CopyColorHSpan(x, y, Len: Integer; Colors: PAggColor); override;
    procedure BlendColorHSpan(x, y, Len: Integer; Colors: PAggColor; Covers: PInt8u; Cover: Int8u = CAggCoverFull); override;
    procedure BlendColorVSpan(x, y, Len: Integer; Colors: PAggColor; Covers: PInt8u; Cover: Int8u = CAggCoverFull); override;

    procedure CopyFrom(From: TAggRenderingBuffer; RC: PRectInteger = nil; tox: Integer = 0; ToY: Integer = 0); override;
  end;

implementation


{ TAggRendererMultiClip }

constructor TAggRendererMultiClip.Create(PixelFormatProcessor: TAggPixelFormatProcessor;
  OwnPixelFormatProcessor: Boolean = False);
begin
  inherited Create(PixelFormatProcessor, OwnPixelFormatProcessor);

  FClip := TAggPodDeque.Create(SizeOf(TRectInteger), 4);
  FBounds := RectInteger(GetXMin, GetYMin, GetXMax, GetYMax);

  FCurrentClibBoxIndex := 0;
end;

destructor TAggRendererMultiClip.Destroy;
begin
  FClip.Free;
  inherited;
end;

function TAggRendererMultiClip.GetBoundingClipBox;
begin
  Result := @FBounds;
end;

function TAggRendererMultiClip.GetBoundingXMin;
begin
  Result := FBounds.x1;
end;

function TAggRendererMultiClip.GetBoundingYMin;
begin
  Result := FBounds.y1;
end;

function TAggRendererMultiClip.GetBoundingXMax;
begin
  Result := FBounds.x2;
end;

function TAggRendererMultiClip.GetBoundingYMax;
begin
  Result := FBounds.y2;
end;

procedure TAggRendererMultiClip.FirstClipBox;
var
  CB: PRectInteger;
begin
  FCurrentClibBoxIndex := 0;

  if FClip.Size <> 0 then
    begin
      CB := FClip[0];

      ClipBoxNaked(CB.x1, CB.y1, CB.x2, CB.y2);
    end;
end;

function TAggRendererMultiClip.NextClipBox;
var
  CB: PRectInteger;
begin
  inc(FCurrentClibBoxIndex);

  if FCurrentClibBoxIndex < FClip.Size then
    begin
      CB := FClip[FCurrentClibBoxIndex];

      ClipBoxNaked(CB.x1, CB.y1, CB.x2, CB.y2);

      Result := True;

      Exit;
    end;

  Result := False;
end;

procedure TAggRendererMultiClip.ResetClipping;
begin
  inherited ResetClipping(Visibility);

  FClip.RemoveAll;

  FCurrentClibBoxIndex := 0;

  FBounds := GetClipBox^;
end;

procedure TAggRendererMultiClip.AddClipBox(x1, y1, x2, y2: Integer);
begin
  AddClipBox(RectInteger(x1, y1, x2, y2));
end;

procedure TAggRendererMultiClip.AddClipBox(Rect: TRectInteger);
var
  RC: TRectInteger;
begin
  Rect.Normalize;
  RC := RectInteger(0, 0, width - 1, height - 1);

  if Rect.Clip(RC) then
    begin
      FClip.Add(@Rect);

      if Rect.x1 < FBounds.x1 then
          FBounds.x1 := Rect.x1;

      if Rect.y1 < FBounds.y1 then
          FBounds.y1 := Rect.y1;

      if Rect.x2 > FBounds.x2 then
          FBounds.x2 := Rect.x2;

      if Rect.y2 > FBounds.y2 then
          FBounds.y2 := Rect.y2;
    end;
end;

procedure TAggRendererMultiClip.CopyPixel(x, y: Integer; c: PAggColor);
begin
  FirstClipBox;

  repeat
    if InBox(x, y) then
      begin
        FPixelFormatProcessor.CopyPixel(FPixelFormatProcessor, x, y, c);

        Break;
      end;
  until not NextClipBox;
end;

procedure TAggRendererMultiClip.BlendPixel(x, y: Integer; c: PAggColor; Cover: Int8u);
begin
  FirstClipBox;

  repeat
    if InBox(x, y) then
      begin
        FPixelFormatProcessor.BlendPixel(FPixelFormatProcessor, x, y, c, Cover);

        Break;
      end;
  until not NextClipBox;
end;

function TAggRendererMultiClip.Pixel;
begin
  FirstClipBox;

  repeat
    if InBox(x, y) then
      begin
        Result := FPixelFormatProcessor.Pixel(FPixelFormatProcessor, x, y);

        Exit;
      end;
  until not NextClipBox;

  Result.Clear;
end;

procedure TAggRendererMultiClip.CopyHorizontalLine(x1, y, x2: Integer;
  c: PAggColor);
begin
  FirstClipBox;

  repeat
      inherited CopyHorizontalLine(x1, y, x2, c);
  until not NextClipBox;
end;

procedure TAggRendererMultiClip.CopyVerticalLine(x, y1, y2: Integer;
  c: PAggColor);
begin
  FirstClipBox;

  repeat
      inherited CopyVerticalLine(x, y1, y2, c);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.BlendHorizontalLine(x1, y, x2: Integer;
  c: PAggColor; Cover: Int8u);
begin
  FirstClipBox;

  repeat
      inherited BlendHorizontalLine(x1, y, x2, c, Cover);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.BlendVerticalLine(x, y1, y2: Integer;
  c: PAggColor; Cover: Int8u);
begin
  FirstClipBox;

  repeat
      inherited BlendVerticalLine(x, y1, y2, c, Cover);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.CopyBar(x1, y1, x2, y2: Integer; c: PAggColor);
begin
  FirstClipBox;

  repeat
      inherited CopyBar(x1, y1, x2, y2, c);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.BlendBar(x1, y1, x2, y2: Integer; c: PAggColor;
  Cover: Int8u);
begin
  FirstClipBox;

  repeat
      inherited BlendBar(x1, y1, x2, y2, c, Cover);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.BlendSolidHSpan(x, y, Len: Integer;
  c: PAggColor; Covers: PInt8u);
begin
  FirstClipBox;

  repeat
      inherited BlendSolidHSpan(x, y, Len, c, Covers);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.BlendSolidVSpan(x, y, Len: Integer;
  c: PAggColor; Covers: PInt8u);
begin
  FirstClipBox;

  repeat
      inherited BlendSolidVSpan(x, y, Len, c, Covers);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.CopyColorHSpan(x, y, Len: Integer;
  Colors: PAggColor);
begin
  FirstClipBox;

  repeat
      inherited CopyColorHSpan(x, y, Len, Colors);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.BlendColorHSpan(x, y, Len: Integer;
  Colors: PAggColor; Covers: PInt8u; Cover: Int8u = CAggCoverFull);
begin
  FirstClipBox;

  repeat
      inherited BlendColorHSpan(x, y, Len, Colors, Covers, Cover);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.BlendColorVSpan(x, y, Len: Integer;
  Colors: PAggColor; Covers: PInt8u; Cover: Int8u = CAggCoverFull);
begin
  FirstClipBox;

  repeat
      inherited BlendColorVSpan(x, y, Len, Colors, Covers, Cover);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.CopyFrom(From: TAggRenderingBuffer;
  RC: PRectInteger = nil; tox: Integer = 0; ToY: Integer = 0);
begin
  FirstClipBox;

  repeat
      inherited CopyFrom(From, RC, tox, ToY);

  until not NextClipBox;
end;

end. 
 
 
 
