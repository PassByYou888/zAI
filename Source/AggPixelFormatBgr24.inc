//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixBgr(p: PAggOrderBgr; CR, Cg, CB, alpha: Integer;
  Cover: Cardinal = 0);
begin
  p^.r := p^.r + ((CR - p^.r) * alpha) shr CAggBaseShift;
  p^.g := p^.g + ((Cg - p^.g) * alpha) shr CAggBaseShift;
  p^.b := p^.b + ((CB - p^.b) * alpha) shr CAggBaseShift;
end;

procedure CopyOrBlendPixBgr(p: PAggOrderBgr; c: PAggColor; Cover: Cardinal);
  overload;
var
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    alpha := (c.Rgba8.a * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      p^.r := c.Rgba8.r;
      p^.g := c.Rgba8.g;
      p^.b := c.Rgba8.b;
    end
    else
      BlendPixBgr(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Cover);
  end;
end;

procedure CopyOrBlendPixBgr(p: PAggOrderBgr; c: PAggColor); overload;
begin
  if c.Rgba8.a <> 0 then
    if c.Rgba8.a = CAggBaseMask then
    begin
      p^.r := c.Rgba8.r;
      p^.g := c.Rgba8.g;
      p^.b := c.Rgba8.b;
    end
    else
      BlendPixBgr(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, c.Rgba8.a);
end;

procedure Bgr24CopyPixel(This: TAggPixelFormatProcessor; x, y: Integer;
  c: PAggColor);
var
  p: PAggOrderBgr;
begin
  p := PAggOrderBgr(This.RenderingBuffer.Row(y));
  inc(p, x);

  p^.r := c.Rgba8.r;
  p^.g := c.Rgba8.g;
  p^.b := c.Rgba8.b;
end;

procedure Bgr24BlendPixel(This: TAggPixelFormatProcessor; x, y: Integer;
  c: PAggColor; Cover: Int8u);
var
  p: PAggOrderBgr;
begin
  p := PAggOrderBgr(This.RenderingBuffer.Row(y));
  inc(p, x);
  CopyOrBlendPixBgr(p, c, Cover);
end;

function Bgr24Pixel(This: TAggPixelFormatProcessor; x, y: Integer): TAggColor;
var
  p: PAggOrderBgr;
begin
  p := PAggOrderBgr(This.RenderingBuffer.Row(y));
  inc(p, x);

  Result.FromRgbaInteger(p^.r, p^.g, p^.b);
end;

procedure Bgr24CopyHorizontalLine(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor);
var
  p: PAggOrderBgr;
begin
  p := PAggOrderBgr(This.RenderingBuffer.Row(y));
  inc(p, x);

  repeat
    p^.r := c.Rgba8.r;
    p^.g := c.Rgba8.g;
    p^.b := c.Rgba8.b;

    inc(p);
    dec(Len);
  until Len = 0;
end;

procedure Bgr24CopyVerticalLine(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor);
var
  p: PAggOrderBgr;
begin
  p := PAggOrderBgr(This.RenderingBuffer.Row(y));
  inc(p, x);

  repeat
    p^.r := c.Rgba8.r;
    p^.g := c.Rgba8.g;
    p^.b := c.Rgba8.b;

    p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));

    dec(Len);
  until Len = 0;
end;

procedure Bgr24BlendHorizontalLine(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor; Cover: Int8u);
var
  p: PAggOrderBgr;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PAggOrderBgr(This.RenderingBuffer.Row(y));
    inc(p, x);

    alpha := (c.Rgba8.a * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
      repeat
        p^.r := c.Rgba8.r;
        p^.g := c.Rgba8.g;
        p^.b := c.Rgba8.b;

        inc(PtrComp(p), 3);
        dec(Len);
      until Len = 0
    else
      repeat
        BlendPixBgr(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha);

        inc(PtrComp(p), 3);
        dec(Len);
      until Len = 0;
  end;
end;

procedure Bgr24BlendVerticalLine(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor; Cover: Int8u);
var
  p: PAggOrderBgr;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PAggOrderBgr(This.RenderingBuffer.Row(y));
    inc(p, x);

    alpha := (c.Rgba8.a * (Cover + 1)) shr 8;
    if alpha = CAggBaseMask then
      repeat
        p^.r := c.Rgba8.r;
        p^.g := c.Rgba8.g;
        p^.b := c.Rgba8.b;

        p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));
        dec(Len);
      until Len = 0
    else
      repeat
        BlendPixBgr(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Cover);

        p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));
        dec(Len);
      until Len = 0;
  end;
end;

procedure Bgr24BlendSolidHorizontalSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor; Covers: PInt8u);
var
  p: PAggOrderBgr;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PAggOrderBgr(This.RenderingBuffer.Row(y));
    inc(p, x);

    repeat
      alpha := (c.Rgba8.a * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
      begin
        p^.r := c.Rgba8.r;
        p^.g := c.Rgba8.g;
        p^.b := c.Rgba8.b;
      end
      else
        BlendPixBgr(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Covers^);

      inc(PtrComp(p), 3);
      inc(Covers);
      dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24BlendSolidVerticalSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor; Covers: PInt8u);
var
  p: PAggOrderBgr;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PAggOrderBgr(This.RenderingBuffer.Row(y));
    inc(p, x);

    repeat
      alpha := (c.Rgba8.a * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
      begin
        p^.r := c.Rgba8.r;
        p^.g := c.Rgba8.g;
        p^.b := c.Rgba8.b;
      end
      else
        BlendPixBgr(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Covers^);

      p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));

      inc(PtrComp(Covers));
      dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24BlendColorHorizontalSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PAggOrderBgr;
begin
  p := PAggOrderBgr(This.RenderingBuffer.Row(y));
  inc(p, x);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgr(p, Colors, Covers^);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(Covers, 1);
      inc(PtrComp(p), 3);
      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgr(p, Colors);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(p), 3);
      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgr(p, Colors, Cover);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(p), 3);
      dec(Len);
    until Len = 0;
end;

procedure Bgr24BlendColorVerticalSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PAggOrderBgr;
begin
  p := PAggOrderBgr(This.RenderingBuffer.Row(y));
  inc(p, x);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgr(p, Colors, Covers^);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(Covers);

      p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));

      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgr(p, Colors);

      inc(PtrComp(Colors), SizeOf(TAggColor));

      p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));

      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgr(p, Colors, Cover);

      inc(PtrComp(Colors), SizeOf(TAggColor));

      p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));

      dec(Len);
    until Len = 0;
end;

procedure Bgr24BlendFrom(This, From: TAggPixelFormatProcessor;
  PSrc: PInt8u; Xdst, Ydst, Xsrc, Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  dest: PAggOrderBgr;
  COLOR: TAggColor;
  alpha: Cardinal;
begin
  dest := PAggOrderBgr(This.RenderingBuffer.Row(Ydst));
  inc(dest, Xdst);

  if Cover = 255 then
    repeat
      alpha := PInt8u(PtrComp(PSrc) + From.Order.a)^;

      if alpha <> 0 then
        if alpha = CAggBaseMask then
        begin
          dest^.r := PInt8u(PtrComp(PSrc) + From.Order.r)^;
          dest^.g := PInt8u(PtrComp(PSrc) + From.Order.g)^;
          dest^.b := PInt8u(PtrComp(PSrc) + From.Order.b)^;
        end
        else
          BlendPixBgr(dest, PInt8u(PtrComp(PSrc) + From.Order.r)^,
            PInt8u(PtrComp(PSrc) + From.Order.g)^,
            PInt8u(PtrComp(PSrc) + From.Order.b)^, alpha);

      inc(PtrComp(PSrc), 4);
      inc(dest);
      dec(Len);
    until Len = 0
  else
    repeat
      COLOR.FromRgbaInteger(PInt8u(PtrComp(PSrc) + From.Order.r)^,
        PInt8u(PtrComp(PSrc) + From.Order.g)^,
        PInt8u(PtrComp(PSrc) + From.Order.b)^,
        PInt8u(PtrComp(PSrc) + From.Order.a)^);

      CopyOrBlendPixBgr(dest, @COLOR, Cover);

      inc(PtrComp(PSrc), 4);
      inc(PtrComp(dest), 3);
      dec(Len);
    until Len = 0;
end;

procedure Bgr24CopyColorHorizontalSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  p: PAggOrderBgr;
begin
  p := PAggOrderBgr(This.RenderingBuffer.Row(y));
  inc(p, x);

  repeat
    p^.r := Colors.Rgba8.r;
    p^.g := Colors.Rgba8.g;
    p^.b := Colors.Rgba8.b;

    inc(PtrComp(Colors), SizeOf(TAggColor));
    inc(PtrComp(p), 3);
    dec(Len);
  until Len = 0;
end;

procedure Bgr24CopyColorVerticalSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor);
var
  p: PAggOrderBgr;
begin
  p := PAggOrderBgr(This.RenderingBuffer.Row(y));
  inc(p, x);

  repeat
    p^.r := Colors.Rgba8.r;
    p^.g := Colors.Rgba8.g;
    p^.b := Colors.Rgba8.b;

    p := PAggOrderBgr(This.RenderingBuffer.NextRow(PInt8u(p)));

    inc(PtrComp(Colors), SizeOf(TAggColor));
    dec(Len);
  until Len = 0;
end;

procedure Bgr24BlendFromColor(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; COLOR: PAggColor; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  PixWidth: Cardinal;
  Source: PInt8u;
  dest: PAggOrderBgr;
begin
  PixWidth := From.PixWidth;
  Source := PInt8u(From.GetRowPointer(Ysrc));

  if Source <> nil then
  begin
    dest := PAggOrderBgr(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    inc(dest, Xdst);

    repeat
      CopyOrBlendPixBgr(dest, COLOR, ShrInt32(Source^ * Cover +
        CAggBaseMask, CAggBaseShift));

      inc(PtrComp(Source), PixWidth);
      inc(PtrComp(dest), 3);
      dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24BlendFromLut(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; ColorLUT: PAggColor; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  PixWidth: Cardinal;
  Source: PInt8u;
  dest: PAggOrderBgr;
  COLOR: PAggColor;
begin
  PixWidth := From.PixWidth;
  Source := PInt8u(From.GetRowPointer(Ysrc));

  if Source <> nil then
  begin
    dest := PAggOrderBgr(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    inc(dest, Xdst);

    if Cover = 255 then
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + Source^ * SizeOf(TAggColor));

        BlendPixBgr(dest, COLOR.Rgba8.r, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.a);

        inc(PtrComp(Source), PixWidth);
        inc(PtrComp(dest), 3);
        dec(Len);
      until Len = 0
    else
      repeat
        CopyOrBlendPixBgr(dest,
          PAggColor(PtrComp(ColorLUT) + Source^ * SizeOf(TAggColor)), Cover);

        inc(PtrComp(Source), PixWidth);
        inc(PtrComp(dest), 3);
        dec(Len);
      until Len = 0;
  end;
end; 
 
 
 
