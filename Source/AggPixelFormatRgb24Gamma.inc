//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixRgbGamma(Gamma: TAggGamma; p: PInt8u;
  CR, Cg, CB, alpha: Integer; Cover: Cardinal = 0);
var
  r, g, b: Cardinal;
begin
  r := Gamma.dir[TAggOrderRGB(Pointer(p)^).r];
  g := Gamma.dir[TAggOrderRGB(Pointer(p)^).g];
  b := Gamma.dir[TAggOrderRGB(Pointer(p)^).b];

  TAggOrderRGB(Pointer(p)^).r :=
    Int8u(Integer(Gamma.Inv[(((Integer(Gamma.dir[CR]) - r) * alpha)
    shr CAggBaseShift) + r]));
  TAggOrderRGB(Pointer(p)^).g :=
    Int8u(Integer(Gamma.Inv[(((Integer(Gamma.dir[Cg]) - g) * alpha)
    shr CAggBaseShift) + g]));
  TAggOrderRGB(Pointer(p)^).b :=
    Int8u(Integer(Gamma.Inv[(((Integer(Gamma.dir[CB]) - b) * alpha)
    shr CAggBaseShift) + b]));
end;

procedure CopyOrBlendPixRgbGamma(Gamma: TAggGamma; p: PInt8u;
  c: PAggColor; Cover: Cardinal); overload;
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
      BlendPixRgbGamma(Gamma, p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Cover);
  end;
end;

procedure CopyOrBlendPixRgbGamma(Gamma: TAggGamma; p: PInt8u;
  c: PAggColor); overload;
begin
  if c.Rgba8.a <> 0 then
    if c.Rgba8.a = CAggBaseMask then
    begin
      TAggOrderRGB(Pointer(p)^).r := c.Rgba8.r;
      TAggOrderRGB(Pointer(p)^).g := c.Rgba8.g;
      TAggOrderRGB(Pointer(p)^).b := c.Rgba8.b;
    end
    else
      BlendPixRgbGamma(Gamma, p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, c.Rgba8.a);
end;

procedure Rgb24GammaBlendPixel(This: TAggPixelFormatProcessor; x, y: Integer;
  c: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPixRgbGamma(This.Gamma,
    PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x + x + x), c, Cover);
end;

procedure Rgb24GammaBlendHorizontalLine(This: TAggPixelFormatProcessor;
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
        BlendPixRgbGamma(This.Gamma, p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha);

        inc(PtrComp(p), 3);
        dec(Len);
      until Len = 0;
  end;
end;

procedure Rgb24GammaBlendVerticalLine(This: TAggPixelFormatProcessor;
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

        p := PInt8u(This.RenderingBuffer.NextRow(p));

        dec(Len);
      until Len = 0
    else
      repeat
        BlendPixRgbGamma(This.Gamma, p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha,
          Cover);

        p := PInt8u(This.RenderingBuffer.NextRow(p));

        dec(Len);
      until Len = 0;
  end;
end;

procedure Rgb24GammaBlendSolidHorizontalSpan(This: TAggPixelFormatProcessor;
  x, y: Integer; Len: Cardinal; c: PAggColor; Covers: PInt8u);
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
        BlendPixRgbGamma(This.Gamma, p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha,
          Covers^);

      inc(PtrComp(p), 3);
      inc(PtrComp(Covers));
      dec(Len);
    until Len = 0;
  end;
end;

procedure Rgb24GammaBlendSolidVerticalSpan(This: TAggPixelFormatProcessor;
  x, y: Integer; Len: Cardinal; c: PAggColor; Covers: PInt8u);
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
        BlendPixRgbGamma(This.Gamma, p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha,
          Covers^);

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      inc(PtrComp(Covers));
      dec(Len);
    until Len = 0;
  end;
end;

procedure Rgb24GammaBlendColorHorizontalSpan(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x + x + x);

  if Covers <> nil then
    repeat
      CopyOrBlendPixRgbGamma(This.Gamma, p, Colors, Covers^);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(Covers), SizeOf(Int8u));
      inc(PtrComp(p), 3);
      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixRgbGamma(This.Gamma, p, Colors);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(p), 3);
      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixRgbGamma(This.Gamma, p, Colors, Cover);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(p), 3);
      dec(Len);
    until Len = 0;
end;

procedure Rgb24GammaBlendColorVerticalSpan(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x + x + x);

  if Covers <> nil then
    repeat
      CopyOrBlendPixRgbGamma(This.Gamma, p, Colors, Covers^);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(Covers), SizeOf(Int8u));

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixRgbGamma(This.Gamma, p, Colors);

      inc(PtrComp(Colors), SizeOf(TAggColor));

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixRgbGamma(This.Gamma, p, Colors, Cover);

      inc(PtrComp(Colors), SizeOf(TAggColor));

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      dec(Len);
    until Len = 0;
end;

procedure Rgb24GammaBlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PSrc, PDst: PInt8u;

  COLOR: TAggColor;
  alpha: Cardinal;
begin
  PSrc := SourcePtr;
  PDst := PInt8u(PtrComp(This.RenderingBuffer.Row(Ydst)) + 3 * Xdst *
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
          BlendPixRgbGamma(This.Gamma, PDst,
            PInt8u(PtrComp(PSrc) + From.Order.r)^,
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

      CopyOrBlendPixRgbGamma(This.Gamma, PDst, @COLOR, Cover);

      inc(PtrComp(PSrc), 4);
      inc(PtrComp(PDst), 3);
      dec(Len);
    until Len = 0;
end;

procedure Rgb24GammaBlendFromColor(This, From: TAggPixelFormatProcessor;
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
      3 * Xdst * SizeOf(Int8u));

    repeat
      CopyOrBlendPixRgbGamma(This.Gamma, PDst, COLOR,
        ShrInt32(PSrc^ * Cover + CAggBaseMask, CAggBaseShift));

      inc(PtrComp(PSrc), Ppsz);
      inc(PtrComp(PDst), 3);
      dec(Len);
    until Len = 0;
  end;
end;

procedure Rgb24GammaBlendFromLUT(This, From: TAggPixelFormatProcessor;
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
      3 * Xdst * SizeOf(Int8u));

    if Cover = 255 then
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

        BlendPixRgbGamma(This.Gamma, PDst, COLOR.Rgba8.r, COLOR.Rgba8.g,
          COLOR.Rgba8.b, COLOR.Rgba8.a);

        inc(PtrComp(PSrc), Ppsz);
        inc(PtrComp(PDst), 3);
        dec(Len);
      until Len = 0
    else
      repeat
        CopyOrBlendPixRgbGamma(This.Gamma, PDst,
          PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor)), Cover);

        inc(PtrComp(PSrc), Ppsz);
        inc(PtrComp(PDst), 3);
        dec(Len);
      until Len = 0;
  end;
end; 
 
 
 
