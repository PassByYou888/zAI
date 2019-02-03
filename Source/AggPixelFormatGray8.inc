//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixGray(p: PInt8u; cv, alpha: Cardinal; Cover: Cardinal = 0);
begin
  p^ := Int8u((((cv - p^) * alpha) + (p^ shl CAggBaseShift)) shr CAggBaseShift);
end;

procedure CopyOrBlendPixGray(p: PInt8u; c: PAggColor;
  Cover: Cardinal); overload;
var
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    alpha := (c.Rgba8.a * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
      p^ := c.v
    else
      BlendPixGray(p, c.v, alpha, Cover);
  end;
end;

procedure CopyOrBlendPixGray(p: PInt8u; c: PAggColor); overload;
begin
  if c.Rgba8.a <> 0 then
    if c.Rgba8.a = CAggBaseMask then
      p^ := c.v
    else
      BlendPixGray(p, c.v, c.Rgba8.a);
end;

procedure Gray8CopyPixel(This: TAggPixelFormatProcessor; x, y: Integer;
  c: PAggColor);
begin
  PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x * This.Step +
    This.Offset)^ := c.v;
end;

procedure Gray8BlendPixel(This: TAggPixelFormatProcessor; x, y: Integer;
  c: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPixGray(PInt8u(PtrComp(This.RenderingBuffer.Row(y)) +
    x * This.Step + This.Offset), c, Cover);
end;

function Gray8Pixel(This: TAggPixelFormatProcessor; x, y: Integer): TAggColor;
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x * This.Step +
    This.Offset);

  Result.FromValueInteger(p^);
end;

procedure Gray8CopyHorizontalLine(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x * This.Step +
    This.Offset);

  repeat
    p^ := c.v;

    inc(PtrComp(p), This.Step);
    dec(Len);
  until Len = 0;
end;

procedure Gray8CopyVerticalLine(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x * This.Step +
    This.Offset);

  repeat
    p^ := c.v;
    p := This.RenderingBuffer.NextRow(p);

    dec(Len);
  until Len = 0;
end;

procedure Gray8BlendHorizontalLine(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; c: PAggColor; Cover: Int8u);
var
  p: PInt8u;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x * This.Step +
      This.Offset);

    alpha := (c.Rgba8.a * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
      repeat
        p^ := c.v;

        inc(PtrComp(p), This.Step);
        dec(Len);
      until Len = 0
    else
      repeat
        BlendPixGray(p, c.v, alpha, Cover);

        inc(PtrComp(p), This.Step);
        dec(Len);
      until Len = 0;
  end;
end;

procedure Gray8BlendVerticalLine(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor; Cover: Int8u);
var
  p: PInt8u;

  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x * This.Step +
      This.Offset);

    alpha := (c.Rgba8.a * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
      repeat
        p^ := c.v;
        p := This.RenderingBuffer.NextRow(p);

        dec(Len);
      until Len = 0
    else
      repeat
        BlendPixGray(p, c.v, alpha, Cover);

        p := This.RenderingBuffer.NextRow(p);

        dec(Len);
      until Len = 0;
  end;
end;

procedure Gray8BlendSolidHSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor; Covers: PInt8u);
var
  p: PInt8u;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x * This.Step +
      This.Offset);

    repeat
      alpha := (c.Rgba8.a * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
        p^ := c.v
      else
        BlendPixGray(p, c.v, alpha, Covers^);

      inc(PtrComp(p), This.Step);
      inc(PtrComp(Covers));
      dec(Len);
    until Len = 0;
  end;
end;

procedure Gray8BlendSolidVSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor; Covers: PInt8u);
var
  p: PInt8u;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x * This.Step +
      This.Offset);

    repeat
      alpha := (c.Rgba8.a * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
        p^ := c.v
      else
        BlendPixGray(p, c.v, alpha, Covers^);

      p := This.RenderingBuffer.NextRow(p);

      inc(PtrComp(Covers), SizeOf(Int8u));
      dec(Len);
    until Len = 0;
  end;
end;

procedure Gray8BlendColorHSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x * This.Step +
    This.Offset);

  if Covers <> nil then
    repeat
      CopyOrBlendPixGray(p, Colors, Covers^);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(Covers), SizeOf(Int8u));
      inc(PtrComp(p), This.Step);
      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      if Colors.Rgba8.a = CAggBaseMask then
        p^ := Colors.v
      else
        CopyOrBlendPixGray(p, Colors);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(p), This.Step);
      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixGray(p, Colors, Cover);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(p), This.Step);
      dec(Len);
    until Len = 0;
end;

procedure Gray8BlendColorVSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x * This.Step +
    This.Offset);

  if Covers <> nil then
    repeat
      CopyOrBlendPixGray(p, Colors, Covers^);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(Covers), SizeOf(Int8u));

      p := This.RenderingBuffer.NextRow(p);

      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      if Colors.Rgba8.a = CAggBaseMask then
        p^ := Colors.v
      else
        CopyOrBlendPixGray(p, Colors);

      p := This.RenderingBuffer.NextRow(p);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixGray(p, Colors, Cover);

      inc(PtrComp(Colors), SizeOf(TAggColor));

      p := This.RenderingBuffer.NextRow(p);

      dec(Len);
    until Len = 0;
end;

procedure Gray8CopyFrom(This: TAggPixelFormatProcessor;
  From: TAggRenderingBuffer; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal);
begin
  Move(PInt8u(PtrComp(From.Row(Ysrc)) + Xsrc * SizeOf(Int8u))^,
    PInt8u(PtrComp(This.RenderingBuffer.Row(Ydst)) + Xdst * SizeOf(Int8u))^,
    Len * SizeOf(Int8u));
end;

procedure Gray8CopyColorHSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x * This.Step +
    This.Offset);

  repeat
    p^ := Colors.v;

    inc(PtrComp(Colors), SizeOf(TAggColor));
    inc(PtrComp(p), This.Step);
    dec(Len);
  until Len = 0;
end;

procedure Gray8CopyColorVSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x * This.Step +
    This.Offset);

  repeat
    p^ := Colors.v;

    inc(PtrComp(Colors), SizeOf(TAggColor));

    p := This.RenderingBuffer.NextRow(p);

    dec(Len);
  until Len = 0;
end;

procedure Gray8BlendFromColor(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; COLOR: PAggColor; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  PixWidth: Cardinal;
  PSrc, PDst: PInt8u;
begin
  PixWidth := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PInt8u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      Xdst * This.Step + This.Offset);

    repeat
      CopyOrBlendPixGray(PDst, COLOR, ShrInt32(PSrc^ * Cover + CAggBaseMask,
        CAggBaseShift));

      inc(PtrComp(PSrc), PixWidth);
      inc(PtrComp(PDst), This.Step);
      dec(Len);
    until Len = 0;
  end;
end;

procedure Gray8BlendFromLUT(This, From: TAggPixelFormatProcessor;
  ColorLUT: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PixWidth: Cardinal;

  PSrc, PDst: PInt8u;
begin
  PixWidth := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PInt8u(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      Xdst * This.Step + This.Offset);

    repeat
      CopyOrBlendPixGray(PDst, PAggColor(PtrComp(ColorLUT) + PSrc^ *
        SizeOf(TAggColor)), Cover);

      inc(PtrComp(PSrc), PixWidth);
      inc(PtrComp(PDst), This.Step);
      dec(Len);
    until Len = 0;
  end;
end; 
 
 
 
