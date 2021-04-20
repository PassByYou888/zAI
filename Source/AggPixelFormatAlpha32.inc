//
// AggPas 2.4 RM3 pixel format definition file
//

procedure Alpha32CopyHorizontalLine(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; c: PAggColor);
var
  p: PCardinal;
  v: Cardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(x, y, Len)) + (x shl 2));

  PInt8u(PtrComp(@v) + This.Order.r)^ := c.Rgba8.r;
  PInt8u(PtrComp(@v) + This.Order.g)^ := c.Rgba8.g;
  PInt8u(PtrComp(@v) + This.Order.b)^ := c.Rgba8.b;
  PInt8u(PtrComp(@v) + This.Order.a)^ := c.Rgba8.a;

  if Len > 0 then
    repeat
      p^ := v;

      inc(PtrComp(p), 4);
      dec(Len);
    until Len = 0;
end;

procedure Alpha32BlendSolidHSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor; Covers: PInt8u);
var
  p: PCardinal;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(x, y, Len)) + (x shl 2));

    repeat
      alpha := (c.Rgba8.a * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
      begin
        PInt8u(PtrComp(p) + This.Order.r)^ := c.Rgba8.r;
        PInt8u(PtrComp(p) + This.Order.g)^ := c.Rgba8.g;
        PInt8u(PtrComp(p) + This.Order.b)^ := c.Rgba8.b;
        PInt8u(PtrComp(p) + This.Order.a)^ := CAggBaseMask;
      end
      else
        case This.Order.r of
          0:
            BlendPixRgba(PAggOrderRgba(p), c.Rgba8.r, c.Rgba8.g, c.Rgba8.b,
              alpha, Covers^);
          1:
            BlendPixArgb(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Covers^);
          2:
            BlendPixBgra(PAggOrderBgra(p), c.Rgba8.r, c.Rgba8.g, c.Rgba8.b,
              alpha, Covers^);
          3:
            BlendPixAbgr(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Covers^);
        end;

      inc(PtrComp(p), 4);
      inc(PtrComp(Covers));

      dec(Len);
    until Len = 0;
  end;
end;

procedure Alpha32BlendColorHSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PCardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(x, y, Len)));
  inc(p, x);

  if Covers <> nil then
    repeat
      case This.Order.r of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(p), Colors.Rgba8.r, Colors.Rgba8.g,
            Colors.Rgba8.b, Colors.Rgba8.a, Covers^);
        1:
          CopyOrBlendPixArgb(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
            Colors.Rgba8.a, Covers^);
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(p), Colors.Rgba8.r, Colors.Rgba8.g,
            Colors.Rgba8.b, Colors.Rgba8.a, Covers^);
        3:
          CopyOrBlendPixAbgr(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
            Colors.Rgba8.a, Covers^);
      end;

      inc(PtrComp(Covers), SizeOf(Int8u));
      inc(p);
      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      case This.Order.r of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(p), Colors.Rgba8.r, Colors.Rgba8.g,
            Colors.Rgba8.b, Colors.Rgba8.a);
        1:
          CopyOrBlendPixArgb(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
            Colors.Rgba8.a);
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(p), Colors.Rgba8.r, Colors.Rgba8.g,
            Colors.Rgba8.b, Colors.Rgba8.a);
        3:
          CopyOrBlendPixAbgr(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
            Colors.Rgba8.a);
      end;

      inc(p);
      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0
  else
    repeat
      case This.Order.r of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(p), Colors.Rgba8.r, Colors.Rgba8.g,
            Colors.Rgba8.b, Colors.Rgba8.a, Cover);
        1:
          CopyOrBlendPixArgb(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
            Colors.Rgba8.a, Cover);
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(p), Colors.Rgba8.r, Colors.Rgba8.g,
            Colors.Rgba8.b, Colors.Rgba8.a, Cover);
        3:
          CopyOrBlendPixAbgr(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
            Colors.Rgba8.a, Cover);
      end;

      inc(p);
      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0;
end;

procedure Alpha32BlendFrom(This, From: TAggPixelFormatProcessor;
  SourcePtr: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal;
  Cover: Int8u);
var
  PSrc, PDst: PCardinal;
  Incp: Integer;
begin
  PSrc := PCardinal(SourcePtr);
  PDst := PCardinal(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)) +
    (Xdst shl 2) * SizeOf(Int8u));
  Incp := 4;

  if PtrComp(Xdst) > PtrComp(Xsrc) then
  begin
    inc(PtrComp(PSrc), ((Len - 1) shl 2) * SizeOf(Int8u));
    inc(PtrComp(PDst), ((Len - 1) shl 2) * SizeOf(Int8u));

    Incp := -4;
  end;

  if Cover = 255 then
    repeat
      case This.Order.r of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(PDst),
            PInt8u(PtrComp(PSrc) + From.Order.r)^,
            PInt8u(PtrComp(PSrc) + From.Order.g)^,
            PInt8u(PtrComp(PSrc) + From.Order.b)^,
            PInt8u(PtrComp(PSrc) + From.Order.a)^);
        1:
          CopyOrBlendPixArgb(PDst, PInt8u(PtrComp(PSrc) + From.Order.r)^,
            PInt8u(PtrComp(PSrc) + From.Order.g)^,
            PInt8u(PtrComp(PSrc) + From.Order.b)^,
            PInt8u(PtrComp(PSrc) + From.Order.a)^);
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(PDst),
            PInt8u(PtrComp(PSrc) + From.Order.r)^,
            PInt8u(PtrComp(PSrc) + From.Order.g)^,
            PInt8u(PtrComp(PSrc) + From.Order.b)^,
            PInt8u(PtrComp(PSrc) + From.Order.a)^);
        3:
          CopyOrBlendPixAbgr(PDst, PInt8u(PtrComp(PSrc) + From.Order.r)^,
            PInt8u(PtrComp(PSrc) + From.Order.g)^,
            PInt8u(PtrComp(PSrc) + From.Order.b)^,
            PInt8u(PtrComp(PSrc) + From.Order.a)^);
      end;

      inc(PtrComp(PSrc), Incp);
      inc(PtrComp(PDst), Incp);
      dec(Len);
    until Len = 0
  else
    repeat
      case This.Order.r of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(PDst),
            PInt8u(PtrComp(PSrc) + From.Order.r)^,
            PInt8u(PtrComp(PSrc) + From.Order.g)^,
            PInt8u(PtrComp(PSrc) + From.Order.b)^,
            PInt8u(PtrComp(PSrc) + From.Order.a)^, Cover);
        1:
          CopyOrBlendPixArgb(PDst, PInt8u(PtrComp(PSrc) + From.Order.r)^,
            PInt8u(PtrComp(PSrc) + From.Order.g)^,
            PInt8u(PtrComp(PSrc) + From.Order.b)^,
            PInt8u(PtrComp(PSrc) + From.Order.a)^, Cover);
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(PDst),
            PInt8u(PtrComp(PSrc) + From.Order.r)^,
            PInt8u(PtrComp(PSrc) + From.Order.g)^,
            PInt8u(PtrComp(PSrc) + From.Order.b)^,
            PInt8u(PtrComp(PSrc) + From.Order.a)^, Cover);
        3:
          CopyOrBlendPixAbgr(PDst, PInt8u(PtrComp(PSrc) + From.Order.r)^,
            PInt8u(PtrComp(PSrc) + From.Order.g)^,
            PInt8u(PtrComp(PSrc) + From.Order.b)^,
            PInt8u(PtrComp(PSrc) + From.Order.a)^, Cover);
      end;

      inc(PtrComp(PSrc), Incp);
      inc(PtrComp(PDst), Incp);
      dec(Len);
    until Len = 0;
end;

function Alpha32Pixel(This: TAggPixelFormatProcessor; x, y: Integer): TAggColor;
var
  p: PCardinal;
begin
  p := PCardinal(This.RenderingBuffer.Row(y));

  if p <> nil then
  begin
    inc(PtrComp(p), x shl 2);

    Result.FromRgbaInteger(PInt8u(PtrComp(p) + This.Order.r)^,
      PInt8u(PtrComp(p) + This.Order.g)^, PInt8u(PtrComp(p) + This.Order.b)
      ^, PInt8u(PtrComp(p) + This.Order.a)^);
  end;
end;

procedure Alpha32CopyPixel(This: TAggPixelFormatProcessor; x, y: Integer;
  c: PAggColor);
var
  p: PCardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(x, y, 1)) + (x shl 2));

  PInt8u(PtrComp(p) + This.Order.r)^ := c.Rgba8.r;
  PInt8u(PtrComp(p) + This.Order.g)^ := c.Rgba8.g;
  PInt8u(PtrComp(p) + This.Order.b)^ := c.Rgba8.b;
  PInt8u(PtrComp(p) + This.Order.a)^ := c.Rgba8.a;
end;

procedure Alpha32BlendPixel(This: TAggPixelFormatProcessor; x, y: Integer;
  c: PAggColor; Cover: Int8u);
begin
  case This.Order.r of
    0:
      CopyOrBlendPixRgba(PAggOrderRgba(PtrComp(This.RenderingBuffer.RowXY(x, y, 1)) +
        (x shl 2)), c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, c.Rgba8.a, Cover);
    1:
      CopyOrBlendPixArgb(PCardinal(PtrComp(This.RenderingBuffer.RowXY(x, y, 1)) +
        (x shl 2)), c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, c.Rgba8.a, Cover);
    2:
      CopyOrBlendPixBgra(PAggOrderBgra(PtrComp(This.RenderingBuffer.RowXY(x, y, 1)) +
        (x shl 2)), c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, c.Rgba8.a, Cover);
    3:
      CopyOrBlendPixAbgr(PCardinal(PtrComp(This.RenderingBuffer.RowXY(x, y, 1)) +
        (x shl 2)), c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, c.Rgba8.a, Cover);
  end;
end;

procedure Alpha32CopyVerticalLine(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; c: PAggColor);
var
  p: PCardinal;
  v: Cardinal;
begin
  PInt8u(PtrComp(@v) + This.Order.r)^ := c.Rgba8.r;
  PInt8u(PtrComp(@v) + This.Order.g)^ := c.Rgba8.g;
  PInt8u(PtrComp(@v) + This.Order.b)^ := c.Rgba8.b;
  PInt8u(PtrComp(@v) + This.Order.a)^ := c.Rgba8.a;

  if Len > 0 then
    repeat
      p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(x, y, 1)) + (x shl 2));
      p^ := v;

      inc(y);
      dec(Len);
    until Len = 0;
end;

procedure Alpha32BlendHorizontalLine(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; c: PAggColor; Cover: Int8u);
var
  p: PCardinal;
  v: Cardinal;

  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(x, y, Len)) + (x shl 2));

    alpha := (c.Rgba8.a * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      PInt8u(PtrComp(@v) + This.Order.r)^ := c.Rgba8.r;
      PInt8u(PtrComp(@v) + This.Order.g)^ := c.Rgba8.g;
      PInt8u(PtrComp(@v) + This.Order.b)^ := c.Rgba8.b;
      PInt8u(PtrComp(@v) + This.Order.a)^ := c.Rgba8.a;

      repeat
        p^ := v;

        inc(PtrComp(p), SizeOf(Cardinal));
        dec(Len);
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        case This.Order.r of
          0:
            BlendPixRgba(PAggOrderRgba(p), c.Rgba8.r, c.Rgba8.g, c.Rgba8.b,
              alpha);
          1:
            BlendPixArgb(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha);
          2:
            BlendPixBgra(PAggOrderBgra(p), c.Rgba8.r, c.Rgba8.g, c.Rgba8.b,
              alpha);
          3:
            BlendPixAbgr(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha);
        end;

        inc(PtrComp(p), SizeOf(Cardinal));
        dec(Len);
      until Len = 0
    else
      repeat
        case This.Order.r of
          0:
            BlendPixRgba(PAggOrderRgba(p), c.Rgba8.r, c.Rgba8.g, c.Rgba8.b,
              alpha, Cover);
          1:
            BlendPixArgb(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Cover);
          2:
            BlendPixBgra(PAggOrderBgra(p), c.Rgba8.r, c.Rgba8.g, c.Rgba8.b,
              alpha, Cover);
          3:
            BlendPixAbgr(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Cover);
        end;

        inc(PtrComp(p), SizeOf(Cardinal));
        dec(Len);
      until Len = 0;
  end;
end;

procedure Alpha32BlendVerticalLine(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; c: PAggColor; Cover: Int8u);
var
  p: PCardinal;
  v: Cardinal;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    alpha := (c.Rgba8.a * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      PInt8u(PtrComp(@v) + This.Order.r)^ := c.Rgba8.r;
      PInt8u(PtrComp(@v) + This.Order.g)^ := c.Rgba8.g;
      PInt8u(PtrComp(@v) + This.Order.b)^ := c.Rgba8.b;
      PInt8u(PtrComp(@v) + This.Order.a)^ := c.Rgba8.a;

      repeat
        p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(x, y, 1)) + (x shl 2));
        p^ := v;

        inc(y);
        dec(Len);
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(x, y, 1)) + (x shl 2));

        case This.Order.r of
          0:
            BlendPixRgba(PAggOrderRgba(p), c.Rgba8.r, c.Rgba8.g, c.Rgba8.b,
              alpha);
          1:
            BlendPixArgb(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha);
          2:
            BlendPixBgra(PAggOrderBgra(p), c.Rgba8.r, c.Rgba8.g, c.Rgba8.b,
              alpha);
          3:
            BlendPixAbgr(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha);
        end;

        inc(y);
        dec(Len);
      until Len = 0
    else
      repeat
        p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(x, y, 1)) + (x shl 2));

        case This.Order.r of
          0:
            BlendPixRgba(PAggOrderRgba(p), c.Rgba8.r, c.Rgba8.g, c.Rgba8.b,
              alpha, Cover);
          1:
            BlendPixArgb(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Cover);
          2:
            BlendPixBgra(PAggOrderBgra(p), c.Rgba8.r, c.Rgba8.g, c.Rgba8.b,
              alpha, Cover);
          3:
            BlendPixAbgr(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Cover);
        end;

        inc(y);
        dec(Len);
      until Len = 0;
  end;
end;

procedure Alpha32BlendSolidVSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor; Covers: PInt8u);
var
  p: PCardinal;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    repeat
      p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(x, y, 1)) + (x shl 2));

      alpha := (c.Rgba8.a * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
      begin
        PInt8u(PtrComp(p) + This.Order.r)^ := c.Rgba8.r;
        PInt8u(PtrComp(p) + This.Order.g)^ := c.Rgba8.g;
        PInt8u(PtrComp(p) + This.Order.b)^ := c.Rgba8.b;
        PInt8u(PtrComp(p) + This.Order.a)^ := CAggBaseMask;
      end
      else
        case This.Order.r of
          0:
            BlendPixRgba(PAggOrderRgba(p), c.Rgba8.r, c.Rgba8.g, c.Rgba8.b,
              alpha, Covers^);
          1:
            BlendPixArgb(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Covers^);
          2:
            BlendPixBgra(PAggOrderBgra(p), c.Rgba8.r, c.Rgba8.g, c.Rgba8.b,
              alpha, Covers^);
          3:
            BlendPixAbgr(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Covers^);
        end;

      inc(PtrComp(Covers), SizeOf(Int8u));
      inc(y);
      dec(Len);
    until Len = 0;
  end;
end;

procedure Alpha32BlendColorVSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PCardinal;

begin
  if Covers <> nil then
    repeat
      p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(x, y, 1)) + (x shl 2));

      case This.Order.r of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(p), Colors.Rgba8.r, Colors.Rgba8.g,
            Colors.Rgba8.b, Colors.Rgba8.a, Covers^);
        1:
          CopyOrBlendPixArgb(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
            Colors.Rgba8.a, Covers^);
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(p), Colors.Rgba8.r, Colors.Rgba8.g,
            Colors.Rgba8.b, Colors.Rgba8.a, Covers^);
        3:
          CopyOrBlendPixAbgr(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
            Colors.Rgba8.a, Covers^);
      end;

      inc(PtrComp(Covers), SizeOf(Int8u));
      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(y);
      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(x, y, 1)) + (x shl 2));

      case This.Order.r of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(p), Colors.Rgba8.r, Colors.Rgba8.g,
            Colors.Rgba8.b, Colors.Rgba8.a);
        1:
          CopyOrBlendPixArgb(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
            Colors.Rgba8.a);
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(p), Colors.Rgba8.r, Colors.Rgba8.g,
            Colors.Rgba8.b, Colors.Rgba8.a);
        3:
          CopyOrBlendPixAbgr(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
            Colors.Rgba8.a);
      end;

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(y);
      dec(Len);
    until Len = 0
  else
    repeat
      p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(x, y, 1)) + (x shl 2));

      case This.Order.r of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(p), Colors.Rgba8.r, Colors.Rgba8.g,
            Colors.Rgba8.b, Colors.Rgba8.a, Cover);
        1:
          CopyOrBlendPixArgb(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
            Colors.Rgba8.a, Cover);
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(p), Colors.Rgba8.r, Colors.Rgba8.g,
            Colors.Rgba8.b, Colors.Rgba8.a, Cover);
        3:
          CopyOrBlendPixAbgr(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
            Colors.Rgba8.a, Cover);
      end;

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(y);
      dec(Len);
    until Len = 0;
end;

procedure Alpha32CopyColorHSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  p: PCardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(x, y, Len)) + (x shl 2));

  repeat
    PInt8u(PtrComp(p) + This.Order.r)^ := Colors.Rgba8.r;
    PInt8u(PtrComp(p) + This.Order.g)^ := Colors.Rgba8.g;
    PInt8u(PtrComp(p) + This.Order.b)^ := Colors.Rgba8.b;
    PInt8u(PtrComp(p) + This.Order.a)^ := Colors.Rgba8.a;

    inc(PtrComp(Colors), SizeOf(TAggColor));
    inc(PtrComp(p), 4);
    dec(Len);
  until Len = 0;
end;

procedure Alpha32CopyColorVSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  p: PCardinal;
begin
  repeat
    p := PCardinal(PtrComp(This.RenderingBuffer.RowXY(x, y, 1)) + (x shl 2));

    PInt8u(PtrComp(p) + This.Order.r)^ := Colors.Rgba8.r;
    PInt8u(PtrComp(p) + This.Order.g)^ := Colors.Rgba8.g;
    PInt8u(PtrComp(p) + This.Order.b)^ := Colors.Rgba8.b;
    PInt8u(PtrComp(p) + This.Order.a)^ := Colors.Rgba8.a;

    inc(PtrComp(Colors), SizeOf(TAggColor));
    inc(y);
    dec(Len);
  until Len = 0;
end;

procedure Alpha32BlendFromColor(This, From: TAggPixelFormatProcessor;
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
    PDst := PCardinal(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)));
    inc(PDst, Xdst);

    repeat
      case This.Order.r of
        0:
          CopyOrBlendPixRgba(PAggOrderRgba(PDst), COLOR.Rgba8.r, COLOR.Rgba8.g,
            COLOR.Rgba8.b, COLOR.Rgba8.a, ShrInt32(PSrc^ * Cover + CAggBaseMask,
            CAggBaseShift));
        1:
          CopyOrBlendPixArgb(PDst, COLOR.Rgba8.r, COLOR.Rgba8.g, COLOR.Rgba8.b,
            COLOR.Rgba8.a, ShrInt32(PSrc^ * Cover + CAggBaseMask, CAggBaseShift));
        2:
          CopyOrBlendPixBgra(PAggOrderBgra(PDst), COLOR.Rgba8.r, COLOR.Rgba8.g,
            COLOR.Rgba8.b, COLOR.Rgba8.a, ShrInt32(PSrc^ * Cover + CAggBaseMask,
            CAggBaseShift));
        3:
          CopyOrBlendPixAbgr(PDst, COLOR.Rgba8.r, COLOR.Rgba8.g, COLOR.Rgba8.b,
            COLOR.Rgba8.a, ShrInt32(PSrc^ * Cover + CAggBaseMask, CAggBaseShift));
      end;

      inc(PtrComp(PSrc), Ppsz);
      inc(PDst);
      dec(Len);
    until Len = 0;
  end;
end;

procedure Alpha32BlendFromLUT(This, From: TAggPixelFormatProcessor;
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
    PDst := PCardinal(PtrComp(This.RenderingBuffer.RowXY(Xdst, Ydst, Len)));
    inc(PDst, Xdst);

    if Cover = 255 then
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

        case This.Order.r of
          0:
            CopyOrBlendPixRgba(PAggOrderRgba(PDst), COLOR.Rgba8.r,
              COLOR.Rgba8.g, COLOR.Rgba8.b, COLOR.Rgba8.a);
          1:
            CopyOrBlendPixArgb(PDst, COLOR.Rgba8.r, COLOR.Rgba8.g,
              COLOR.Rgba8.b, COLOR.Rgba8.a);
          2:
            CopyOrBlendPixBgra(PAggOrderBgra(PDst), COLOR.Rgba8.r,
              COLOR.Rgba8.g, COLOR.Rgba8.b, COLOR.Rgba8.a);
          3:
            CopyOrBlendPixAbgr(PDst, COLOR.Rgba8.r, COLOR.Rgba8.g,
              COLOR.Rgba8.b, COLOR.Rgba8.a);
        end;

        inc(PtrComp(PSrc), Ppsz);
        inc(PDst);
        dec(Len);
      until Len = 0
    else
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

        case This.Order.r of
          0:
            CopyOrBlendPixRgba(PAggOrderRgba(PDst), COLOR.Rgba8.r,
              COLOR.Rgba8.g, COLOR.Rgba8.b, COLOR.Rgba8.a, Cover);
          1:
            CopyOrBlendPixArgb(PDst, COLOR.Rgba8.r, COLOR.Rgba8.g,
              COLOR.Rgba8.b, COLOR.Rgba8.a, Cover);
          2:
            CopyOrBlendPixBgra(PAggOrderBgra(PDst), COLOR.Rgba8.r,
              COLOR.Rgba8.g, COLOR.Rgba8.b, COLOR.Rgba8.a, Cover);
          3:
            CopyOrBlendPixAbgr(PDst, COLOR.Rgba8.r, COLOR.Rgba8.g,
              COLOR.Rgba8.b, COLOR.Rgba8.a, Cover);
        end;

        inc(PtrComp(PSrc), Ppsz);
        inc(PDst);
        dec(Len);
      until Len = 0;
  end;
end; 
 
 
 
