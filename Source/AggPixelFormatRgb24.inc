//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixRgb(p: PInt8u; CR, Cg, CB, alpha: Integer;
  Cover: Cardinal = 0);
begin
  TAggOrderRGB(Pointer(p)^).r := TAggOrderRGB(Pointer(p)^).r +
    ((CR - TAggOrderRGB(Pointer(p)^).r) * alpha) shr CAggBaseShift;
  TAggOrderRGB(Pointer(p)^).g := TAggOrderRGB(Pointer(p)^).g +
    ((Cg - TAggOrderRGB(Pointer(p)^).g) * alpha) shr CAggBaseShift;
  TAggOrderRGB(Pointer(p)^).b := TAggOrderRGB(Pointer(p)^).b +
    ((CB - TAggOrderRGB(Pointer(p)^).b) * alpha) shr CAggBaseShift;
end;

procedure CopyOrBlendPixRgb(p: PInt8u; c: PAggColor;
  Cover: Cardinal); overload;
var
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    alpha := (c.Rgba8.a * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      TAggOrderRGB(Pointer(p)^).r := c.Rgba8.r;
      TAggOrderRGB(Pointer(p)^).g := c.Rgba8.g;
      TAggOrderRGB(Pointer(p)^).b := c.Rgba8.b;
    end
    else
      BlendPixRgb(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Cover);
  end;
end;

procedure CopyOrBlendPixRgb(p: PInt8u; c: PAggColor); overload;
begin
  if c.Rgba8.a <> 0 then
    if c.Rgba8.a = CAggBaseMask then
    begin
      TAggOrderRGB(Pointer(p)^).r := c.Rgba8.r;
      TAggOrderRGB(Pointer(p)^).g := c.Rgba8.g;
      TAggOrderRGB(Pointer(p)^).b := c.Rgba8.b;
    end
    else
      BlendPixRgb(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, c.Rgba8.a);
end;

procedure Rgb24CopyPixel(This: TAggPixelFormatProcessor; x, y: Integer;
  c: PAggColor);
var
  p: PAggOrderRgb;
begin
  p := PAggOrderRgb(This.RenderingBuffer.Row(y));
  inc(p, x);

  p^.r := c.Rgba8.r;
  p^.g := c.Rgba8.g;
  p^.b := c.Rgba8.b;
end;

procedure Rgb24BlendPixel(This: TAggPixelFormatProcessor; x, y: Integer;
  c: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPixRgb(PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x + x + x),
    c, Cover);
end;

function Rgb24Pixel(This: TAggPixelFormatProcessor; x, y: Integer): TAggColor;
var
  p: PAggOrderRgb;
begin
  p := PAggOrderRgb(This.RenderingBuffer.Row(y));
  inc(p, x);

  Result.Rgba8.r := p^.r;
  Result.Rgba8.g := p^.g;
  Result.Rgba8.b := p^.b;
  Result.Rgba8.a := $FF;
end;

procedure Rgb24CopyHorizontalLine(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x + x + x);

  repeat
    TAggOrderRGB(Pointer(p)^).r := c.Rgba8.r;
    TAggOrderRGB(Pointer(p)^).g := c.Rgba8.g;
    TAggOrderRGB(Pointer(p)^).b := c.Rgba8.b;

    inc(PtrComp(p), 3);
    dec(Len);
  until Len = 0;
end;

procedure Rgb24CopyVerticalLine(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x + x + x);

  repeat
    TAggOrderRGB(Pointer(p)^).r := c.Rgba8.r;
    TAggOrderRGB(Pointer(p)^).g := c.Rgba8.g;
    TAggOrderRGB(Pointer(p)^).b := c.Rgba8.b;

    p := This.RenderingBuffer.NextRow(p);

    dec(Len);
  until Len = 0;
end;

procedure Rgb24BlendHorizontalLine(This: TAggPixelFormatProcessor;
  x, y: Integer; Len: Cardinal; c: PAggColor; Cover: Int8u);
var
  p: PInt8u;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x + x + x);

    alpha := (c.Rgba8.a * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
      repeat
        TAggOrderRGB(Pointer(p)^).r := c.Rgba8.r;
        TAggOrderRGB(Pointer(p)^).g := c.Rgba8.g;
        TAggOrderRGB(Pointer(p)^).b := c.Rgba8.b;

        inc(PtrComp(p), 3);
        dec(Len);
      until Len = 0
    else
      repeat
        BlendPixRgb(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha);

        inc(PtrComp(p), 3);
        dec(Len);
      until Len = 0;
  end;
end;

procedure Rgb24BlendVerticalLine(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor; Cover: Int8u);
var
  p: PInt8u;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x + x + x);

    alpha := (c.Rgba8.a * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
      repeat
        TAggOrderRGB(Pointer(p)^).r := c.Rgba8.r;
        TAggOrderRGB(Pointer(p)^).g := c.Rgba8.g;
        TAggOrderRGB(Pointer(p)^).b := c.Rgba8.b;

        p := PInt8u(This.RenderingBuffer.NextRow(p));

        dec(Len);
      until Len = 0
    else
      repeat
        BlendPixRgb(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Cover);

        p := PInt8u(This.RenderingBuffer.NextRow(p));

        dec(Len);
      until Len = 0;
  end;
end;

procedure Rgb24BlendSolidHorizontalSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor; Covers: PInt8u);
var
  p: PInt8u;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x + x + x);

    repeat
      alpha := (c.Rgba8.a * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
      begin
        TAggOrderRGB(Pointer(p)^).r := c.Rgba8.r;
        TAggOrderRGB(Pointer(p)^).g := c.Rgba8.g;
        TAggOrderRGB(Pointer(p)^).b := c.Rgba8.b;

      end
      else
        BlendPixRgb(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Covers^);

      inc(PtrComp(p), 3);
      inc(PtrComp(Covers));
      dec(Len);
    until Len = 0;
  end;
end;

procedure Rgb24BlendSolidVerticalSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor; Covers: PInt8u);
var
  p: PInt8u;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x + x + x);

    repeat
      alpha := (c.Rgba8.a * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
      begin
        TAggOrderRGB(Pointer(p)^).r := c.Rgba8.r;
        TAggOrderRGB(Pointer(p)^).g := c.Rgba8.g;
        TAggOrderRGB(Pointer(p)^).b := c.Rgba8.b;
      end
      else
        BlendPixRgb(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Covers^);

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      inc(PtrComp(Covers));
      dec(Len);
    until Len = 0;
  end;
end;

procedure Rgb24BlendColorHorizontalSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x + x + x);

  if Covers <> nil then
    repeat
      CopyOrBlendPixRgb(p, Colors, Covers^);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(Covers), SizeOf(Int8u));
      inc(PtrComp(p), 3);
      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixRgb(p, Colors);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(p), 3);
      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixRgb(p, Colors, Cover);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(p), 3);
      dec(Len);
    until Len = 0;
end;

procedure Rgb24BlendColorVerticalSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x + x + x);

  if Covers <> nil then
    repeat
      CopyOrBlendPixRgb(p, Colors, Covers^);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(Covers), SizeOf(Int8u));

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixRgb(p, Colors);

      inc(PtrComp(Colors), SizeOf(TAggColor));

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixRgb(p, Colors, Cover);

      inc(PtrComp(Colors), SizeOf(TAggColor));

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      dec(Len);
    until Len = 0;
end;

procedure Rgb24BlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PSrc, PDst: PInt8u;

  COLOR: TAggColor;
  alpha: Cardinal;
begin
  PSrc := SourcePtr;
  PDst := PInt8u(PtrComp(This.RenderingBuffer.Row(Ydst)) + Xdst * 3 *
    SizeOf(Int8u));

  if Cover = 255 then
    repeat
      alpha := PInt8u(PtrComp(PSrc) + From.Order.a)^;

      if alpha <> 0 then
        if alpha = CAggBaseMask then
        begin
          TAggOrderRGB(Pointer(PDst)^).r :=
            PInt8u(PtrComp(PSrc) + From.Order.r)^;
          TAggOrderRGB(Pointer(PDst)^).g :=
            PInt8u(PtrComp(PSrc) + From.Order.g)^;
          TAggOrderRGB(Pointer(PDst)^).b :=
            PInt8u(PtrComp(PSrc) + From.Order.b)^;

        end
        else
          BlendPixRgb(PDst, PInt8u(PtrComp(PSrc) + From.Order.r)^,
            PInt8u(PtrComp(PSrc) + From.Order.g)^,
            PInt8u(PtrComp(PSrc) + From.Order.b)^, alpha);

      inc(PtrComp(PSrc), 4);
      inc(PtrComp(PDst), 3);
      dec(Len);

    until Len = 0
  else
    repeat
      COLOR.FromRgbaInteger(PInt8u(PtrComp(PSrc) + From.Order.r)^,
        PInt8u(PtrComp(PSrc) + From.Order.g)^,
        PInt8u(PtrComp(PSrc) + From.Order.b)^,
        PInt8u(PtrComp(PSrc) + From.Order.a)^);

      CopyOrBlendPixRgb(PDst, @COLOR, Cover);

      inc(PtrComp(PSrc), 4);
      inc(PtrComp(PDst), 3);
      dec(Len);
    until Len = 0;
end;

procedure Rgb24CopyColorHorizontalSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  p: PInt8u;

begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x + x + x);

  repeat
    TAggOrderRGB(Pointer(p)^).r := Colors.Rgba8.r;
    TAggOrderRGB(Pointer(p)^).g := Colors.Rgba8.g;
    TAggOrderRGB(Pointer(p)^).b := Colors.Rgba8.b;

    inc(PtrComp(Colors), SizeOf(TAggColor));
    inc(PtrComp(p), 3);
    dec(Len);
  until Len = 0;
end;

procedure Rgb24CopyColorVerticalSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x + x + x);

  repeat
    TAggOrderRGB(Pointer(p)^).r := Colors.Rgba8.r;
    TAggOrderRGB(Pointer(p)^).g := Colors.Rgba8.g;
    TAggOrderRGB(Pointer(p)^).b := Colors.Rgba8.b;

    p := PInt8u(This.RenderingBuffer.NextRow(p));

    inc(PtrComp(Colors), SizeOf(TAggColor));
    dec(Len);
  until Len = 0;
end;

procedure Rgb24BlendFromColor(This, From: TAggPixelFormatProcessor;
  COLOR: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Ppsz: Cardinal;
  PSrc, PDst: PInt8u;
begin
  Ppsz := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PInt8u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      Xdst * 3 * SizeOf(Int8u));

    repeat
      CopyOrBlendPixRgb(PDst, COLOR, ShrInt32(PSrc^ * Cover + CAggBaseMask,
        CAggBaseShift));

      inc(PtrComp(PSrc), Ppsz);
      inc(PtrComp(PDst), 3);
      dec(Len);
    until Len = 0;
  end;
end;

procedure Rgb24BlendFromLUT(This, From: TAggPixelFormatProcessor;
  ColorLUT: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Ppsz: Cardinal;
  PSrc, PDst: PInt8u;
  COLOR: PAggColor;
begin
  Ppsz := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PInt8u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      Xdst * 3 * SizeOf(Int8u));

    if Cover = 255 then
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

        BlendPixRgb(PDst, COLOR.Rgba8.r, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.a);

        inc(PtrComp(PSrc), Ppsz);
        inc(PtrComp(PDst), 3);
        dec(Len);
      until Len = 0
    else
      repeat
        CopyOrBlendPixRgb(PDst,
          PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor)), Cover);

        inc(PtrComp(PSrc), Ppsz);
        inc(PtrComp(PDst), 3);
        dec(Len);
      until Len = 0;
  end;
end; 
 
 
 
