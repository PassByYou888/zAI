//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixBgra(p: PAggOrderBgra; CR, Cg, CB, alpha: Integer; Cover: Cardinal = 0);
begin
  p^.r := ((CR - p^.r) * alpha + (p^.r shl CAggBaseShift)) shr CAggBaseShift;
  p^.g := ((Cg - p^.g) * alpha + (p^.g shl CAggBaseShift)) shr CAggBaseShift;
  p^.b := ((CB - p^.b) * alpha + (p^.b shl CAggBaseShift)) shr CAggBaseShift;
  p^.a := (alpha + p^.a) - ((alpha * p^.a + CAggBaseMask) shr CAggBaseShift);
end;

procedure CopyOrBlendPixBgra(p: PAggOrderBgra;
  CR, Cg, CB, alpha: Cardinal); inline; overload;
begin
  if alpha <> 0 then
    if alpha = CAggBaseMask then
    begin
      p^.r := CR;
      p^.g := Cg;
      p^.b := CB;
      p^.a := CAggBaseMask;
    end
    else
      BlendPixBgra(p, CR, Cg, CB, alpha);
end;

procedure CopyOrBlendPixBgra(p: PAggOrderBgra;
  CR, Cg, CB, alpha, Cover: Cardinal); inline; overload;
begin
  if Cover = 255 then
    CopyOrBlendPixBgra(p, CR, Cg, CB, alpha)
  else if alpha <> 0 then
  begin
    alpha := (alpha * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      p^.r := CR;
      p^.g := Cg;
      p^.b := CB;
      p^.a := CAggBaseMask;
    end
    else
      BlendPixBgra(p, CR, Cg, CB, alpha, Cover);
  end;
end;

procedure Bgra32CopyPixel(This: TAggPixelFormatProcessor; x, y: Integer;
  c: PAggColor);
var
  p: PAggOrderBgra;
begin
  p := PAggOrderBgra(This.RenderingBuffer.Row(y));
  inc(p, x);

  p^.r := c.Rgba8.r;
  p^.g := c.Rgba8.g;
  p^.b := c.Rgba8.b;
  p^.a := c.Rgba8.a;
end;

procedure Bgra32BlendPixel(This: TAggPixelFormatProcessor; x, y: Integer;
  c: PAggColor; Cover: Int8u);
var
  p: PAggOrderBgra;
begin
  p := PAggOrderBgra(This.RenderingBuffer.Row(y));
  inc(p, x);

  CopyOrBlendPixBgra(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, c.Rgba8.a, Cover);
end;

function Bgra32Pixel(This: TAggPixelFormatProcessor; x, y: Integer): TAggColor;
var
  p: PAggOrderBgra;
begin
  p := PAggOrderBgra(This.RenderingBuffer.Row(y));
  inc(p, x);

  Result.FromRgbaInteger(p^.r, p^.g, p^.b, p^.a);
end;

procedure Bgra32CopyHorizontalLine(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; c: PAggColor);
var
  p: PAggOrderBgra;
  v: TAggOrderBGRA;
begin
  p := PAggOrderBgra(This.RenderingBuffer.Row(y));
  inc(p, x);

  v.r := c.Rgba8.r;
  v.g := c.Rgba8.g;
  v.b := c.Rgba8.b;
  v.a := c.Rgba8.a;

  Fill32Bit(p^, Len, v);
end;

procedure Bgra32CopyVerticalLine(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor);
var
  p: PAggOrderBgra;
  v: TAggOrderBGRA;
begin
  p := PAggOrderBgra(This.RenderingBuffer.Row(y));
  inc(p, x);

  v.r := c.Rgba8.r;
  v.g := c.Rgba8.g;
  v.b := c.Rgba8.b;
  v.a := c.Rgba8.a;

  if Len > 0 then
    repeat
      p^ := v;
      p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

      dec(Len);
    until Len = 0;
end;

procedure Bgra32BlendHorizontalLine(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; c: PAggColor; Cover: Int8u);
var
  p: PAggOrderBgra;
  v: TAggOrderBGRA;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PAggOrderBgra(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2));

    alpha := (c.Rgba8.a * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      v.r := c.Rgba8.r;
      v.g := c.Rgba8.g;
      v.b := c.Rgba8.b;
      v.a := c.Rgba8.a;

      Fill32Bit(p^, Len, v);
    end
    else if Cover = 255 then
      repeat
        BlendPixBgra(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha);

        inc(p);
        dec(Len);
      until Len = 0
    else
      repeat
        BlendPixBgra(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Cover);

        inc(p);
        dec(Len);
      until Len = 0;
  end;
end;

procedure Bgra32BlendVerticalLine(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; c: PAggColor; Cover: Int8u);
var
  p: PAggOrderBgra;
  v: TAggOrderBGRA;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PAggOrderBgra(This.RenderingBuffer.Row(y));
    inc(p, x);

    alpha := (c.Rgba8.a * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      v.r := c.Rgba8.r;
      v.g := c.Rgba8.g;
      v.b := c.Rgba8.b;
      v.a := c.Rgba8.a;

      repeat
        p^ := v;
        p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

        dec(Len);
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        BlendPixBgra(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha);

        p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

        dec(Len);
      until Len = 0
    else
      repeat
        BlendPixBgra(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Cover);

        p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

        dec(Len);
      until Len = 0;
  end;
end;

procedure Bgra32BlendSolidHorizontalSpan(This: TAggPixelFormatProcessor;
  x, y: Integer; Len: Cardinal; c: PAggColor; Covers: PInt8u);
var
  p: PAggOrderBgra;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PAggOrderBgra(This.RenderingBuffer.Row(y));
    inc(p, x);

    repeat
      alpha := (c.Rgba8.a * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
      begin
        p^.r := c.Rgba8.r;
        p^.g := c.Rgba8.g;
        p^.b := c.Rgba8.b;
        p^.a := CAggBaseMask;
      end
      else
        BlendPixBgra(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Covers^);

      inc(p);
      inc(Covers);

      dec(Len);
    until Len = 0;
  end;
end;

procedure Bgra32BlendSolidVerticalSpan(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; c: PAggColor; Covers: PInt8u);
var
  p: PAggOrderBgra;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PAggOrderBgra(This.RenderingBuffer.Row(y));
    inc(p, x);

    repeat
      alpha := (c.Rgba8.a * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
      begin
        p^.r := c.Rgba8.r;
        p^.g := c.Rgba8.g;
        p^.b := c.Rgba8.b;
        p^.a := CAggBaseMask;
      end
      else
        BlendPixBgra(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Covers^);

      p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

      inc(Covers);
      dec(Len);
    until Len = 0;
  end;
end;

procedure Bgra32BlendColorHorizontalSpan(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PAggOrderBgra;
begin
  p := PAggOrderBgra(This.RenderingBuffer.Row(y));
  inc(p, x);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgra(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a, Covers^);

      inc(Covers);
      inc(p);
      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgra(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a);

      inc(p);
      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgra(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a, Cover);

      inc(p);
      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0;
end;

procedure Bgra32BlendColorVerticalSpan(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PAggOrderBgra;
begin
  p := PAggOrderBgra(This.RenderingBuffer.Row(y));
  inc(p, x);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgra(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a, Covers^);

      inc(Covers);

      p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgra(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a);

      p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgra(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a, Cover);

      p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0;
end;

procedure Bgra32BlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PSrc, PDst: PAggOrderBgra;
  Incp: Integer;
begin
  PSrc := PAggOrderBgra(SourcePtr);
  PDst := PAggOrderBgra(This.RenderingBuffer.Row(Ydst));
  inc(PDst, Xdst);
  Incp := 4;

  if PtrComp(Xdst) > PtrComp(Xsrc) then
  begin
    inc(PtrComp(PSrc), ((Len - 1) shl 2));
    inc(PtrComp(PDst), ((Len - 1) shl 2));

    Incp := -4;
  end;

  if Cover = 255 then
    repeat
      CopyOrBlendPixBgra(PDst, PSrc^.r, PSrc^.g, PSrc^.b, PSrc^.a);

      inc(PtrComp(PSrc), Incp);
      inc(PtrComp(PDst), Incp);
      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgra(PDst, PSrc^.r, PSrc^.g, PSrc^.b, PSrc^.a, Cover);

      inc(PtrComp(PSrc), Incp);
      inc(PtrComp(PDst), Incp);
      dec(Len);
    until Len = 0;
end;

procedure Bgra32CopyColorHorizontalSpan(This: TAggPixelFormatProcessor;
  x, y: Integer; Len: Cardinal; Colors: PAggColor);
var
  p: PAggOrderBgra;
begin
  p := PAggOrderBgra(This.RenderingBuffer.Row(y));
  inc(p, x);

  repeat
    p^.r := Colors.Rgba8.r;
    p^.g := Colors.Rgba8.g;
    p^.b := Colors.Rgba8.b;
    p^.a := Colors.Rgba8.a;

    inc(PtrComp(Colors), SizeOf(TAggColor));
    inc(p);
    dec(Len);
  until Len = 0;
end;

procedure Bgra32CopyColorVerticalSpan(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; Colors: PAggColor);
var
  p: PAggOrderBgra;
begin
  p := PAggOrderBgra(This.RenderingBuffer.Row(y));
  inc(p, x);

  repeat
    p^.r := Colors.Rgba8.r;
    p^.g := Colors.Rgba8.g;
    p^.b := Colors.Rgba8.b;
    p^.a := Colors.Rgba8.a;

    p := PAggOrderBgra(This.RenderingBuffer.NextRow(PInt8u(p)));

    inc(PtrComp(Colors), SizeOf(TAggColor));
    dec(Len);
  until Len = 0;
end;

procedure Bgra32BlendFromColor(This, From: TAggPixelFormatProcessor;
  COLOR: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PixWidth: Cardinal;
  Source: PInt8u;
  dest: PAggOrderBgra;
begin
  PixWidth := From.PixWidth;
  Source := Pointer(PtrComp(From.GetRowPointer(Ysrc)) + Xsrc * PixWidth);

  if Source <> nil then
  begin
    dest := PAggOrderBgra(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    inc(dest, Xdst);

    repeat
      CopyOrBlendPixBgra(dest, COLOR.Rgba8.r, COLOR.Rgba8.g, COLOR.Rgba8.b,
        COLOR.Rgba8.a, ShrInt32(Source^ * Cover + CAggBaseMask, CAggBaseShift));

      inc(PtrComp(Source), PixWidth);
      inc(dest);
      dec(Len);
    until Len = 0;
  end;
end;

procedure Bgra32BlendFromLUT(This, From: TAggPixelFormatProcessor;
  ColorLUT: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PixWidth: Cardinal;

  Source: PInt8u;
  dest: PAggOrderBgra;

  COLOR: PAggColor;
begin
  PixWidth := From.PixWidth;
  Source := Pointer(PtrComp(From.GetRowPointer(Ysrc)) + Xsrc * PixWidth);

  if Source <> nil then
  begin
    dest := PAggOrderBgra(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    inc(dest, Xdst);

    if Cover = 255 then
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + Source^ * SizeOf(TAggColor));

        CopyOrBlendPixBgra(dest, COLOR.Rgba8.r, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.a);

        inc(PtrComp(Source), PixWidth);
        inc(PtrComp(dest), 4);
        dec(Len);
      until Len = 0
    else
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + Source^ * SizeOf(TAggColor));

        CopyOrBlendPixBgra(dest, COLOR.Rgba8.r, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.a, Cover);

        inc(PtrComp(Source), PixWidth);
        inc(PtrComp(dest), 4);
        dec(Len);
      until Len = 0;
  end;
end; 
 
 
 
