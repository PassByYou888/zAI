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
unit AggSpanGradientContour;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  Math,
  AggBasics,
  AggColor32,
  AggSpanGradient,
  AggPathStorage,
  AggBoundingRect,
  AggConvCurve,
  AggConvStroke,
  AggConvTransform,
  AggTransAffine,
  AggRenderingBuffer,
  AggRendererBase,
  AggRendererScanLine,
  AggRendererPrimitives,
  AggRenderScanlines,
  AggRasterizerOutline,
  AggRasterizerScanLineAA,
  AggScanline,
  AggScanlineUnpacked,
  AggPixelFormat,
  AggPixelFormatGray;

type
  PAggIntArray = ^TAggIntArray;
  TAggIntArray = array [0 .. 65535] of Integer;

  PAggSingleArray = ^TAggSingleArray;
  TAggSingleArray = array [0 .. 65535] of Single;

  TAggGradientContour = class(TAggCustomGradient)
  private
    FBuffer: Pointer;
    FWidth, FHeight, FFrame: Integer;

    FD1, FD2: Double;

    procedure SetFrame(f: Integer); overload;

    function Calculate(x, y, d: Integer): Integer; override;

    procedure SetD1(d: Double);
    procedure SetD2(d: Double);
  public
    constructor Create(AD1: Double = 0; AD2: Double = 100);
    destructor Destroy; override;

    function ContourCreate(ps: TAggPathStorage): Pointer;

    property frame: Integer read FFrame write SetFrame;
    property ContourWidth: Integer read FWidth;
    property ContourHeight: Integer read FHeight;

    property d1: Double read FD1 write SetD1;
    property d2: Double read FD2 write SetD2;
  end;

implementation

const
  CInfinity = 1E20;

  { TAggGradientContour }

constructor TAggGradientContour.Create(AD1: Double = 0; AD2: Double = 100);
begin
  inherited Create;

  FBuffer := nil;
  FWidth := 0;
  FHeight := 0;
  FFrame := 10;

  FD1 := AD1;
  FD2 := AD2;
end;

destructor TAggGradientContour.Destroy;
begin
  if FBuffer <> nil then
      AggFreeMem(FBuffer, FWidth * FHeight);
end;

function Square(x: Integer): Integer;
begin
  Result := x * x;
end;

// DT algorithm by: Pedro Felzenszwalb
procedure dt(Spanf, Spang, Spanr: PAggSingleArray; Spann: PAggIntArray;
  length: Integer);
var
  k, q: Integer;
  s: Single;
begin
  k := 0;

  Spann[0] := 0;
  Spang[0] := -CInfinity;
  Spang[1] := +CInfinity;

  q := 1;

  while q <= length - 1 do
    begin
      s := ((Spanf[q] + Square(q)) - (Spanf[Spann[k]] + Square(Spann[k]))) /
        (2 * q - 2 * Spann[k]);

      while s <= Spang[k] do
        begin
          dec(k);

          s := ((Spanf[q] + Square(q)) - (Spanf[Spann[k]] + Square(Spann[k]))) /
            (2 * q - 2 * Spann[k]);
        end;

      inc(k);

      Spann[k] := q;
      Spang[k] := s;

      Spang[k + 1] := +CInfinity;

      inc(q);
    end;

  k := 0;
  q := 0;

  while q <= length - 1 do
    begin
      while Spang[k + 1] < q do
          inc(k);

      Spanr[q] := Square(q - Spann[k]) + Spanf[Spann[k]];

      inc(q);
    end;
end;

function TAggGradientContour.ContourCreate(ps: TAggPathStorage): Pointer;
var
  rb: TAggRenderingBuffer;
  pf: TAggPixelFormatProcessor;

  Ras: TAggRasterizerOutline;
  Mtx: TAggTransAffine;

  RGBA: TAggColor;
  Renb: TAggRendererBase;
  Prim: TAggRendererPrimitives;
  conv: TAggConvCurve;

  Trans: TAggConvTransform;

  Min, Max, Scale: Single;

  x1, y1, x2, y2: Double;
  width, height, length, Fcx, Fcy: Integer;
  buffer, Image: Pointer;
  Src: PInt8u;
  Dst, IM, Spanf, Spang, Spanr: PSingle;
  Spann: PInteger;
begin
  Result := nil;
  buffer := nil;

  if ps <> nil then
    begin
      { I. Render Black And White NonAA Stroke of the Path }
      { Path Bounding Box + Some GetFrame Space Around [configurable] }
      conv := TAggConvCurve.Create(ps);
      try
        if BoundingRectSingle(conv, 0, @x1, @y1, @x2, @y2) then
          begin
            { Create BW Rendering Surface }
            width := Ceil(x2 - x1) + FFrame * 2 + 1;
            height := Ceil(y2 - y1) + FFrame * 2 + 1;

            if AggGetMem(buffer, width * height) then
              begin
                FillChar(buffer^, width * height, 255);

                { Setup VG Engine & Render }
                rb := TAggRenderingBuffer.Create;
                rb.Attach(buffer, width, height, width);

                PixelFormatGray8(pf, rb);

                Renb := TAggRendererBase.Create(pf, True);
                Prim := TAggRendererPrimitives.Create(Renb);
                Ras := TAggRasterizerOutline.Create(Prim);
                try
                  Mtx := TAggTransAffine.Create;
                  try
                    Mtx.Translate(FFrame - x1, FFrame - y1);

                    Trans := TAggConvTransform.Create(conv, Mtx);
                    try
                      RGBA.Black;
                      Prim.LineColor := RGBA;
                      Ras.AddPath(Trans);
                    finally
                        Trans.Free;
                    end;
                  finally
                      Mtx.Free;
                  end;
                  rb.Free;
                finally
                  Ras.Free;
                  Prim.Free;
                  Renb.Free;
                end;

                { II. Distance Transform }
                { Create Float Buffer + 0 vs CInfinity (1e20) assignment }
                if AggGetMem(Image, width * height * SizeOf(Single)) then
                  begin
                    Src := buffer;
                    Dst := Image;

                    for Fcy := 0 to height - 1 do
                      for Fcx := 0 to width - 1 do
                        begin
                          if Src^ = 0 then
                              Dst^ := 0
                          else
                              Dst^ := CInfinity;

                          inc(PtrComp(Src));
                          inc(PtrComp(Dst), SizeOf(Single));
                        end;

                    { DT of 2d }
                    { SubBuff<float> max width,height }
                    length := width;

                    if height > length then
                        length := height;

                    Spanf := nil;
                    Spang := nil;
                    Spanr := nil;
                    Spann := nil;

                    if AggGetMem(Pointer(Spanf), length * SizeOf(Single)) and
                      AggGetMem(Pointer(Spang), (length + 1) * SizeOf(Single)) and
                      AggGetMem(Pointer(Spanr), length * SizeOf(Single)) and
                      AggGetMem(Pointer(Spann), length * SizeOf(Integer)) then
                      begin
                        { Transform along columns }
                        for Fcx := 0 to width - 1 do
                          begin
                            IM := Pointer(PtrComp(Image) + Fcx * SizeOf(Single));
                            Dst := Spanf;

                            for Fcy := 0 to height - 1 do
                              begin
                                Dst^ := IM^;

                                inc(PtrComp(Dst), SizeOf(Single));
                                inc(PtrComp(IM), width * SizeOf(Single));
                              end;

                            { DT of 1d }
                            dt(Pointer(Spanf), Pointer(Spang), Pointer(Spanr),
                              Pointer(Spann), height);

                            IM := Pointer(PtrComp(Image) + Fcx * SizeOf(Single));
                            Dst := Spanr;

                            for Fcy := 0 to height - 1 do
                              begin
                                IM^ := Dst^;

                                inc(PtrComp(Dst), SizeOf(Single));
                                inc(PtrComp(IM), width * SizeOf(Single));
                              end;
                          end;

                        { Transform along rows }
                        for Fcy := 0 to height - 1 do
                          begin
                            IM := Pointer(PtrComp(Image) + Fcy * width * SizeOf(Single));
                            Dst := Spanf;

                            for Fcx := 0 to width - 1 do
                              begin
                                Dst^ := IM^;

                                inc(PtrComp(Dst), SizeOf(Single));
                                inc(PtrComp(IM), SizeOf(Single));
                              end;

                            { DT of 1d }
                            dt(Pointer(Spanf), Pointer(Spang), Pointer(Spanr),
                              Pointer(Spann), width);

                            IM := Pointer(PtrComp(Image) + Fcy * width * SizeOf(Single));
                            Dst := Spanr;

                            for Fcx := 0 to width - 1 do
                              begin
                                IM^ := Dst^;

                                inc(PtrComp(Dst), SizeOf(Single));
                                inc(PtrComp(IM), SizeOf(Single));
                              end;
                          end;

                        { Take Square Roots, Min & Max }
                        Dst := Image;
                        Min := Sqrt(Dst^);
                        Max := Min;

                        for Fcy := 0 to height - 1 do
                          for Fcx := 0 to width - 1 do
                            begin
                              Dst^ := Sqrt(Dst^);

                              if Min > Dst^ then
                                  Min := Dst^;

                              if Max < Dst^ then
                                  Max := Dst^;

                              inc(PtrComp(Dst), SizeOf(Single));
                            end;

                        { III. Convert To Grayscale }
                        if Min = Max then
                            FillChar(buffer^, width * height, 0)
                        else
                          begin
                            Scale := 255 / (Max - Min);

                            Src := buffer;
                            Dst := Image;

                            for Fcy := 0 to height - 1 do
                              for Fcx := 0 to width - 1 do
                                begin
                                  Src^ := Int8u(Trunc((Dst^ - Min) * Scale));

                                  inc(PtrComp(Src));
                                  inc(PtrComp(Dst), SizeOf(Single));
                                end;
                          end;

                        { OK }
                        if FBuffer <> nil then
                            AggFreeMem(FBuffer, FWidth * FHeight);

                        FBuffer := buffer;
                        FWidth := width;
                        FHeight := height;

                        buffer := nil;
                        Result := FBuffer;
                      end;

                    if Spanf <> nil then
                        AggFreeMem(Pointer(Spanf), length * SizeOf(Single));

                    if Spang <> nil then
                        AggFreeMem(Pointer(Spang), (length + 1) * SizeOf(Single));

                    if Spanr <> nil then
                        AggFreeMem(Pointer(Spanr), length * SizeOf(Single));

                    if Spann <> nil then
                        AggFreeMem(Pointer(Spann), length * SizeOf(Integer));

                    AggFreeMem(Image, width * height * SizeOf(Single));
                  end;
              end;
          end;
      finally
          conv.Free;
      end;

      if buffer <> nil then
          AggFreeMem(buffer, width * height);
    end;
end;

procedure TAggGradientContour.SetD1(d: Double);
begin
  FD1 := d;
end;

procedure TAggGradientContour.SetD2(d: Double);
begin
  FD2 := d;
end;

procedure TAggGradientContour.SetFrame(f: Integer);
begin
  FFrame := f;
end;

function TAggGradientContour.Calculate(x, y, d: Integer): Integer;
var
  Px, Py: Integer;
  Pixel: PInt8u;
begin
  if FBuffer <> nil then
    begin
      Px := ShrInt32(x, CAggGradientSubpixelShift);
      Py := ShrInt32(y, CAggGradientSubpixelShift);

      Px := Px mod FWidth;

      if Px < 0 then
          Px := FWidth + Px;

      Py := Py mod FHeight;

      if Py < 0 then
          Py := FHeight + Py;

      Pixel := PInt8u(PtrComp(FBuffer) + Py * FWidth + Px);
      Result := Round(Pixel^ * (FD2 / 256) + FD1) shl CAggGradientSubpixelShift;
    end
  else
      Result := 0;
end;

end. 
 
 
