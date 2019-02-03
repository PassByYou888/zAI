//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixArgb(p: PCardinal; CR, Cg, CB, alpha: Integer;
  Cover: Cardinal = 0);
var
  r, g, b, a: Int8u;
begin
  r := TAggOrderARGB(p^).r;
  g := TAggOrderARGB(p^).g;
  b := TAggOrderARGB(p^).b;
  a := TAggOrderARGB(p^).a;

  TAggOrderARGB(p^).r := ((CR - r) * alpha + (r shl CAggBaseShift)) shr CAggBaseShift;
  TAggOrderARGB(p^).g := ((Cg - g) * alpha + (g shl CAggBaseShift)) shr CAggBaseShift;
  TAggOrderARGB(p^).b := ((CB - b) * alpha + (b shl CAggBaseShift)) shr CAggBaseShift;
  TAggOrderARGB(p^).a := (alpha + a) - ((alpha * a + CAggBaseMask) shr CAggBaseShift);
end;

procedure CopyOrBlendPixArgb(p: PCardinal;
  CR, Cg, CB, alpha: Cardinal); overload;
begin
  if alpha <> 0 then
    if alpha = CAggBaseMask then
    begin
      TAggOrderARGB(p^).r := CR;
      TAggOrderARGB(p^).g := Cg;
      TAggOrderARGB(p^).b := CB;
      TAggOrderARGB(p^).a := CAggBaseMask;
    end
    else
      BlendPixArgb(p, CR, Cg, CB, alpha);
end;

procedure CopyOrBlendPixArgb(p: PCardinal;
  CR, Cg, CB, alpha, Cover: Cardinal); overload;
begin
  if Cover = 255 then
    CopyOrBlendPixArgb(p, CR, Cg, CB, alpha)
  else if alpha <> 0 then
  begin
    alpha := (alpha * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      TAggOrderARGB(p^).r := CR;
      TAggOrderARGB(p^).g := Cg;
      TAggOrderARGB(p^).b := CB;
      TAggOrderARGB(p^).a := CAggBaseMask;
    end
    else
      BlendPixArgb(p, CR, Cg, CB, alpha, Cover);
  end;
end;

procedure ARGB32CopyPixel(This: TAggPixelFormatProcessor; x, y: Integer; c: PAggColor);
var
  p: PCardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2));

  TAggOrderARGB(p^).r := c.Rgba8.r;
  TAggOrderARGB(p^).g := c.Rgba8.g;
  TAggOrderARGB(p^).b := c.Rgba8.b;
  TAggOrderARGB(p^).a := c.Rgba8.a;
end;

procedure ARGB32BlendPixel(This: TAggPixelFormatProcessor; x, y: Integer;
  c: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPixArgb(PCardinal(PtrComp(This.RenderingBuffer.Row(y)) +
    (x shl 2)), c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, c.Rgba8.a, Cover);
end;

function ARGB32Pixel(This: TAggPixelFormatProcessor; x, y: Integer): TAggColor;
var
  p: PAggOrderArgb;
begin
  p := PAggOrderArgb(This.RenderingBuffer.Row(y));
  inc(p, x);

  Result.FromRgbaInteger(p^.r, p^.g, p^.b, p^.a);
end;

procedure ARGB32CopyHorizontalLine(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; c: PAggColor);
var
  p: PInt8u;
  v: Cardinal;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2));

  TAggOrderARGB(v).r := c.Rgba8.r;
  TAggOrderARGB(v).g := c.Rgba8.g;
  TAggOrderARGB(v).b := c.Rgba8.b;
  TAggOrderARGB(v).a := c.Rgba8.a;

  if Len > 0 then
    repeat
      Cardinal(Pointer(p)^) := v;

      inc(PtrComp(p), 4);
      dec(Len);
    until Len = 0;
end;

procedure ARGB32CopyVerticalLine(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; c: PAggColor);
var
  p: PCardinal;
  v: Cardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2));

  TAggOrderARGB(v).r := c.Rgba8.r;
  TAggOrderARGB(v).g := c.Rgba8.g;
  TAggOrderARGB(v).b := c.Rgba8.b;
  TAggOrderARGB(v).a := c.Rgba8.a;

  if Len > 0 then
    repeat
      p^ := v;
      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      dec(Len);
    until Len = 0;
end;

procedure ARGB32BlendHorizontalLine(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; c: PAggColor; Cover: Int8u);
var
  p: PCardinal;
  v: Cardinal;

  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PCardinal(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2));

    alpha := (c.Rgba8.a * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      TAggOrderARGB(v).r := c.Rgba8.r;
      TAggOrderARGB(v).g := c.Rgba8.g;
      TAggOrderARGB(v).b := c.Rgba8.b;
      TAggOrderARGB(v).a := c.Rgba8.a;

      repeat
        p^ := v;

        inc(PtrComp(p), SizeOf(Cardinal));
        dec(Len);
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        BlendPixArgb(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha);

        inc(PtrComp(p), SizeOf(Cardinal));
        dec(Len);
      until Len = 0
    else
      repeat
        BlendPixArgb(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Cover);

        inc(PtrComp(p), SizeOf(Cardinal));
        dec(Len);
      until Len = 0;
  end;
end;

procedure ARGB32BlendVerticalLine(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; c: PAggColor; Cover: Int8u);
var
  p: PCardinal;
  v: Cardinal;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PCardinal(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2));

    alpha := (c.Rgba8.a * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      TAggOrderARGB(v).r := c.Rgba8.r;
      TAggOrderARGB(v).g := c.Rgba8.g;
      TAggOrderARGB(v).b := c.Rgba8.b;
      TAggOrderARGB(v).a := c.Rgba8.a;

      repeat
        p^ := v;
        p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        BlendPixArgb(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha);

        p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

        dec(Len);
      until Len = 0
    else
      repeat
        BlendPixArgb(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Cover);

        p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

        dec(Len);
      until Len = 0;
  end;
end;

procedure ARGB32BlendSolidHSpan(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; c: PAggColor; Covers: PInt8u);
var
  p: PCardinal;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PCardinal(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2));

    repeat
      alpha := (c.Rgba8.a * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
      begin
        TAggOrderARGB(p^).r := c.Rgba8.r;
        TAggOrderARGB(p^).g := c.Rgba8.g;
        TAggOrderARGB(p^).b := c.Rgba8.b;
        TAggOrderARGB(p^).a := CAggBaseMask;
      end
      else
        BlendPixArgb(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Covers^);

      inc(PtrComp(p), 4);
      inc(PtrComp(Covers));

      dec(Len);
    until Len = 0;
  end;
end;

procedure ARGB32BlendSolidVSpan(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; c: PAggColor; Covers: PInt8u);
var
  p: PCardinal;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PCardinal(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2));

    repeat
      alpha := (c.Rgba8.a * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
      begin
        TAggOrderARGB(p^).r := c.Rgba8.r;
        TAggOrderARGB(p^).g := c.Rgba8.g;
        TAggOrderARGB(p^).b := c.Rgba8.b;
        TAggOrderARGB(p^).a := CAggBaseMask;
      end
      else
        BlendPixArgb(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Covers^);

      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      inc(PtrComp(Covers), SizeOf(Int8u));
      dec(Len);
    until Len = 0;
  end;
end;

procedure ARGB32BlendColorHSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PCardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2));

  if Covers <> nil then
    repeat
      CopyOrBlendPixArgb(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a, Covers^);

      inc(PtrComp(Covers), SizeOf(Int8u));
      inc(PtrComp(p), 4);
      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixArgb(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a);

      inc(PtrComp(p), 4);
      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixArgb(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a, Cover);

      inc(PtrComp(p), 4);
      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0;
end;

procedure ARGB32BlendColorVSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PCardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2));

  if Covers <> nil then
    repeat
      CopyOrBlendPixArgb(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a, Covers^);

      inc(PtrComp(Covers), SizeOf(Int8u));

      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixArgb(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a);

      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixArgb(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a, Cover);

      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0;
end;

procedure ARGB32BlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PSrc, PDst: PCardinal;
  Incp: Integer;
begin
  PSrc := PCardinal(SourcePtr);
  PDst := PCardinal(PtrComp(This.RenderingBuffer.Row(Ydst)) + (Xdst shl 2) *
    SizeOf(Int8u));
  Incp := 4;

  if PtrComp(Xdst) > PtrComp(Xsrc) then
  begin
    inc(PtrComp(PSrc), ((Len - 1) shl 2) * SizeOf(Int8u));
    inc(PtrComp(PDst), ((Len - 1) shl 2) * SizeOf(Int8u));

    Incp := -4;
  end;

  if Cover = 255 then
    repeat
      CopyOrBlendPixArgb(PDst, TAggOrderARGB(PSrc^).r, TAggOrderARGB(PSrc^).g,
        TAggOrderARGB(PSrc^).b, TAggOrderARGB(PSrc^).a);

      inc(PtrComp(PSrc), Incp);
      inc(PtrComp(PDst), Incp);
      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixArgb(PDst, TAggOrderARGB(PSrc^).r, TAggOrderARGB(PSrc^).g,
        TAggOrderARGB(PSrc^).b, TAggOrderARGB(PSrc^).a, Cover);

      inc(PtrComp(PSrc), Incp);
      inc(PtrComp(PDst), Incp);
      dec(Len);
    until Len = 0;
end;

procedure ARGB32CopyColorHSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  p: PCardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2));

  repeat
    TAggOrderARGB(Pointer(p)^).r := Colors.Rgba8.r;
    TAggOrderARGB(Pointer(p)^).g := Colors.Rgba8.g;
    TAggOrderARGB(Pointer(p)^).b := Colors.Rgba8.b;
    TAggOrderARGB(Pointer(p)^).a := Colors.Rgba8.a;

    inc(PtrComp(Colors), SizeOf(TAggColor));
    inc(PtrComp(p), 4);
    dec(Len);
  until Len = 0;
end;

procedure ARGB32CopyColorVSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  p: PCardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2));

  repeat
    TAggOrderARGB(Pointer(p)^).r := Colors.Rgba8.r;
    TAggOrderARGB(Pointer(p)^).g := Colors.Rgba8.g;
    TAggOrderARGB(Pointer(p)^).b := Colors.Rgba8.b;
    TAggOrderARGB(Pointer(p)^).a := Colors.Rgba8.a;

    p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

    inc(PtrComp(Colors), SizeOf(TAggColor));
    dec(Len);
  until Len = 0;
end;

procedure ARGB32BlendFromColor(This, From: TAggPixelFormatProcessor;
  COLOR: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Ppsz: Cardinal;

  PSrc: PInt8u;
  PDst: PCardinal;

begin
  Ppsz := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PCardinal(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      (Xdst shl 2));

    repeat
      CopyOrBlendPixArgb(PDst, COLOR.Rgba8.r, COLOR.Rgba8.g, COLOR.Rgba8.b,
        COLOR.Rgba8.a, ShrInt32(PSrc^ * Cover + CAggBaseMask, CAggBaseShift));

      inc(PtrComp(PSrc), Ppsz);
      inc(PtrComp(PDst), 4);
      dec(Len);
    until Len = 0;
  end;
end;

procedure ARGB32BlendFromLUT(This, From: TAggPixelFormatProcessor;
  ColorLUT: PAggColor; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  Ppsz: Cardinal;

  PSrc: PInt8u;
  PDst: PCardinal;

  COLOR: PAggColor;

begin
  Ppsz := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PCardinal(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
      (Xdst shl 2));

    if Cover = 255 then
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

        CopyOrBlendPixArgb(PDst, COLOR.Rgba8.r, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.a);

        inc(PtrComp(PSrc), Ppsz);
        inc(PtrComp(PDst), 4);
        dec(Len);
      until Len = 0
    else
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

        CopyOrBlendPixArgb(PDst, COLOR.Rgba8.r, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.a, Cover);

        inc(PtrComp(PSrc), Ppsz);
        inc(PtrComp(PDst), 4);
        dec(Len);
      until Len = 0;
  end;
end; 
 
 
 
