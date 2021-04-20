//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixRgbPre(p: PInt8u;
  CR, Cg, CB, alpha, Cover: Cardinal); overload;
begin
  alpha := CAggBaseMask - alpha;
  Cover := (Cover + 1) shl (CAggBaseShift - 8);

  TAggOrderRGB(Pointer(p)^).r :=
    Int8u((TAggOrderRGB(Pointer(p)^).r * alpha + CR * Cover) shr CAggBaseShift);
  TAggOrderRGB(Pointer(p)^).g :=
    Int8u((TAggOrderRGB(Pointer(p)^).g * alpha + Cg * Cover) shr CAggBaseShift);
  TAggOrderRGB(Pointer(p)^).b :=
    Int8u((TAggOrderRGB(Pointer(p)^).b * alpha + CB * Cover) shr CAggBaseShift);
end;

procedure BlendPixRgbPre(p: PInt8u; CR, Cg, CB, alpha: Cardinal); overload;
begin
  alpha := CAggBaseMask - alpha;

  TAggOrderRGB(Pointer(p)^).r :=
    Int8u(((TAggOrderRGB(Pointer(p)^).r * alpha) shr CAggBaseShift) + CR);
  TAggOrderRGB(Pointer(p)^).g :=
    Int8u(((TAggOrderRGB(Pointer(p)^).g * alpha) shr CAggBaseShift) + Cg);
  TAggOrderRGB(Pointer(p)^).b :=
    Int8u(((TAggOrderRGB(Pointer(p)^).b * alpha) shr CAggBaseShift) + CB);
end;

procedure CopyOrBlendPixRgbPre(p: PInt8u; c: PAggColor;
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
      BlendPixRgbPre(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Cover);
  end;
end;

procedure CopyOrBlendPixRgbPre(p: PInt8u; c: PAggColor); overload;
begin
  if c.Rgba8.a <> 0 then
    if c.Rgba8.a = CAggBaseMask then
    begin
      TAggOrderRGB(Pointer(p)^).r := c.Rgba8.r;
      TAggOrderRGB(Pointer(p)^).g := c.Rgba8.g;
      TAggOrderRGB(Pointer(p)^).b := c.Rgba8.b;
    end
    else
      BlendPixRgbPre(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, c.Rgba8.a);
end;

procedure Rgb24PreBlendPixel(This: TAggPixelFormatProcessor; x, y: Integer;
  c: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPixRgbPre(PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x + x + x),
    c, Cover);
end;

procedure Rgb24PreBlendHorizontalLine(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; c: PAggColor; Cover: Int8u);
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
        BlendPixRgbPre(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha);

        inc(PtrComp(p), 3);
        dec(Len);
      until Len = 0;
  end;
end;

procedure Rgb24PreBlendVerticalLine(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; c: PAggColor; Cover: Int8u);
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
        BlendPixRgbPre(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Cover);

        p := PInt8u(This.RenderingBuffer.NextRow(p));

        dec(Len);
      until Len = 0;
  end;
end;

procedure Rgb24PreBlendSolidHorizontalSpan(This: TAggPixelFormatProcessor; x, y: Integer;
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
        BlendPixRgbPre(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Covers^);

      inc(PtrComp(p), 3);
      inc(PtrComp(Covers));
      dec(Len);
    until Len = 0;
  end;
end;

procedure Rgb24PreBlendSolidVSpan(This: TAggPixelFormatProcessor; x, y: Integer;
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
        BlendPixRgbPre(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Covers^);

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      inc(PtrComp(Covers));
      dec(Len);
    until Len = 0;
  end;
end;

procedure Rgb24PreBlendColorHorizontalSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x + x + x);

  if Covers <> nil then
    repeat
      CopyOrBlendPixRgbPre(p, Colors, Covers^);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(Covers), SizeOf(Int8u));
      inc(PtrComp(p), 3);
      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixRgbPre(p, Colors);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(p), 3);
      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixRgbPre(p, Colors, Cover);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(p), 3);
      dec(Len);
    until Len = 0;
end;

procedure Rgb24PreBlendColorVSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x + x + x);

  if Covers <> nil then
    repeat
      CopyOrBlendPixRgbPre(p, Colors, Covers^);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(Covers), SizeOf(Int8u));

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixRgbPre(p, Colors);

      inc(PtrComp(Colors), SizeOf(TAggColor));

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixRgbPre(p, Colors, Cover);

      inc(PtrComp(Colors), SizeOf(TAggColor));

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      dec(Len);
    until Len = 0;
end;

procedure Rgb24PreBlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PDst: PInt8u;

  COLOR: TAggColor;
  alpha: Cardinal;
begin
  PDst := PInt8u(PtrComp(This.RenderingBuffer.Row(Ydst)) + Xdst * 3 *
    SizeOf(Int8u));

  if Cover = 255 then
    repeat
      alpha := PInt8u(PtrComp(SourcePtr) + From.Order.a)^;

      if alpha <> 0 then
        if alpha = CAggBaseMask then
        begin
          TAggOrderRGB(Pointer(PDst)^).r :=
            PInt8u(PtrComp(SourcePtr) + From.Order.r)^;
          TAggOrderRGB(Pointer(PDst)^).g :=
            PInt8u(PtrComp(SourcePtr) + From.Order.g)^;
          TAggOrderRGB(Pointer(PDst)^).b :=
            PInt8u(PtrComp(SourcePtr) + From.Order.b)^;
        end
        else
          BlendPixRgbPre(PDst, PInt8u(PtrComp(SourcePtr) + From.Order.r)^,
            PInt8u(PtrComp(SourcePtr) + From.Order.g)^,
            PInt8u(PtrComp(SourcePtr) + From.Order.b)^, alpha);

      inc(PtrComp(SourcePtr), 4);
      inc(PtrComp(PDst), 3);
      dec(Len);
    until Len = 0
  else
    repeat
      COLOR.FromRgbaInteger(PInt8u(PtrComp(SourcePtr) + From.Order.r)^,
        PInt8u(PtrComp(SourcePtr) + From.Order.g)^,
        PInt8u(PtrComp(SourcePtr) + From.Order.b)^,
        PInt8u(PtrComp(SourcePtr) + From.Order.a)^);

      CopyOrBlendPixRgbPre(PDst, @COLOR, Cover);

      inc(PtrComp(SourcePtr), 4);
      inc(PtrComp(PDst), 3);
      dec(Len);
    until Len = 0;
end;

procedure Rgb24PreBlendFromColor(This, From: TAggPixelFormatProcessor;
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
    PDst := PInt8u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) + Xdst * 3 *
      SizeOf(Int8u));

    repeat
      CopyOrBlendPixRgbPre(PDst, COLOR,
        ShrInt32(PSrc^ * Cover + CAggBaseMask, CAggBaseShift));

      inc(PtrComp(PSrc), Ppsz);
      inc(PtrComp(PDst), 3);
      dec(Len);
    until Len = 0;
  end;
end;

procedure Rgb24PreBlendFromLUT(This, From: TAggPixelFormatProcessor;
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

        BlendPixRgbPre(PDst, COLOR.Rgba8.r, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.a);

        inc(PtrComp(PSrc), Ppsz);
        inc(PtrComp(PDst), 3);
        dec(Len);
      until Len = 0
    else
      repeat
        CopyOrBlendPixRgbPre(PDst,
          PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor)), Cover);

        inc(PtrComp(PSrc), Ppsz);
        inc(PtrComp(PDst), 3);
        dec(Len);
      until Len = 0;
  end;
end; 
 
 
 
