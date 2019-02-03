//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixRgba(p: PAggOrderRgba; CR, Cg, CB, alpha: Integer; Cover: Cardinal = 0);
begin
  p^.r := ((CR - p^.r) * alpha + (p^.r shl CAggBaseShift)) shr CAggBaseShift;
  p^.g := ((Cg - p^.g) * alpha + (p^.g shl CAggBaseShift)) shr CAggBaseShift;
  p^.b := ((CB - p^.b) * alpha + (p^.b shl CAggBaseShift)) shr CAggBaseShift;
  p^.a := (alpha + p^.a) - ((alpha * p^.a + CAggBaseMask) shr CAggBaseShift);
end;

procedure CopyOrBlendPixRgba(p: PAggOrderRgba;
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
      BlendPixRgba(p, CR, Cg, CB, alpha);
end;

procedure CopyOrBlendPixRgba(p: PAggOrderRgba;
  CR, Cg, CB, alpha, Cover: Cardinal); inline; overload;
begin
  if Cover = 255 then
    CopyOrBlendPixRgba(p, CR, Cg, CB, alpha)
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
      BlendPixRgba(p, CR, Cg, CB, alpha, Cover);
  end;
end;

procedure Rgba32CopyPixel(This: TAggPixelFormatProcessor; x, y: Integer;
  c: PAggColor);
var
  p: PAggOrderRgba;
begin
  p := PAggOrderRgba(This.RenderingBuffer.Row(y));
  inc(p, x);

  p^.r := c.Rgba8.r;
  p^.g := c.Rgba8.g;
  p^.b := c.Rgba8.b;
  p^.a := c.Rgba8.a;
end;

procedure Rgba32BlendPixel(This: TAggPixelFormatProcessor;
  x, y: Integer; c: PAggColor; Cover: Int8u);
var
  p: PAggOrderRgba;
begin
  p := PAggOrderRgba(This.RenderingBuffer.Row(y));
  inc(p, x);

  CopyOrBlendPixRgba(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, c.Rgba8.a, Cover);
end;

function Rgba32Pixel(This: TAggPixelFormatProcessor; x, y: Integer): TAggColor;
var
  p: PAggOrderRgba;
begin
  p := PAggOrderRgba(This.RenderingBuffer.Row(y));
  inc(p, x);

  Result.FromRgbaInteger(p^.r, p^.g, p^.b, p^.a);
end;

procedure Rgba32CopyHorizontalLine(This: TAggPixelFormatProcessor;
  x, y: Integer; Len: Cardinal; c: PAggColor);
var
  p: PAggOrderRgba;
  v: TAggOrderRGBA;
begin
  p := PAggOrderRgba(This.RenderingBuffer.Row(y));
  inc(p, x);

  v.r := c.Rgba8.r;
  v.g := c.Rgba8.g;
  v.b := c.Rgba8.b;
  v.a := c.Rgba8.a;

  Fill32Bit(p^, Len, v);
end;

procedure Rgba32CopyVerticalLine(This: TAggPixelFormatProcessor;
  x, y: Integer; Len: Cardinal; c: PAggColor);
var
  p: PAggOrderRgba;
  v: TAggOrderRGBA;
begin
  p := PAggOrderRgba(This.RenderingBuffer.Row(y));
  inc(p, x);

  v.r := c.Rgba8.r;
  v.g := c.Rgba8.g;
  v.b := c.Rgba8.b;
  v.a := c.Rgba8.a;

  if Len > 0 then
    repeat
      p^ := v;
      p := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(p)));

      dec(Len);
    until Len = 0;
end;

procedure Rgba32BlendHorizontalLine(This: TAggPixelFormatProcessor;
  x, y: Integer; Len: Cardinal; c: PAggColor; Cover: Int8u);
var
  p: PAggOrderRgba;
  v: TAggOrderRGBA;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PAggOrderRgba(This.RenderingBuffer.Row(y));
    inc(p, x);

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
        BlendPixRgba(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha);

        inc(p);
        dec(Len);
      until Len = 0
    else
      repeat
        BlendPixRgba(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Cover);

        inc(p);
        dec(Len);
      until Len = 0;
  end;
end;

procedure Rgba32BlendVerticalLine(This: TAggPixelFormatProcessor;
  x, y: Integer; Len: Cardinal; c: PAggColor; Cover: Int8u);
var
  p: PAggOrderRgba;
  v: TAggOrderRGBA;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PAggOrderRgba(This.RenderingBuffer.Row(y));
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
        p := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(p)));
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        BlendPixRgba(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha);

        p := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(p)));

        dec(Len);
      until Len = 0
    else
      repeat
        BlendPixRgba(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Cover);

        p := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(p)));

        dec(Len);
      until Len = 0;
  end;
end;

procedure Rgba32BlendSolidHorizontalSpan(This: TAggPixelFormatProcessor;
  x, y: Integer; Len: Cardinal; c: PAggColor; Covers: PInt8u);
var
  p: PAggOrderRgba;
  PClr: PAggRgba8;
  alpha: Cardinal;
begin
  PClr := @c^.Rgba8;
  if PClr^.a <> 0 then
  begin
    p := PAggOrderRgba(This.RenderingBuffer.Row(y));
    inc(p, x);

    repeat
      alpha := (PClr^.a * (Covers^ + 1)) shr 8;

      if alpha = CAggBaseMask then
      begin
        p^.r := PClr^.r;
        p^.g := PClr^.g;
        p^.b := PClr^.b;
        p^.a := CAggBaseMask;
      end
      else
        BlendPixRgba(p, PClr^.r, PClr^.g, PClr^.b, alpha, Covers^);

      inc(p);
      inc(Covers);

      dec(Len);
    until Len = 0;
  end;
end;

procedure Rgba32BlendSolidVerticalSpan(This: TAggPixelFormatProcessor;
  x, y: Integer; Len: Cardinal; c: PAggColor; Covers: PInt8u);
var
  p: PAggOrderRgba;
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    p := PAggOrderRgba(This.RenderingBuffer.Row(y));
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
        BlendPixRgba(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Covers^);

      p := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(p)));

      inc(Covers);
      dec(Len);
    until Len = 0;
  end;
end;

procedure Rgba32BlendColorHorizontalSpan(This: TAggPixelFormatProcessor;
  x, y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u;
  Cover: Int8u);
var
  p: PAggOrderRgba;
begin
  p := PAggOrderRgba(This.RenderingBuffer.Row(y));
  inc(p, x);

  if Covers <> nil then
    repeat
      CopyOrBlendPixRgba(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a, Covers^);

      inc(Covers);
      inc(p);
      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixRgba(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a);

      inc(p);
      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixRgba(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a, Cover);

      inc(p);
      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0;
end;

procedure Rgba32BlendColorVerticalSpan(This: TAggPixelFormatProcessor;
  x, y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u;
  Cover: Int8u);
var
  p: PAggOrderRgba;
begin
  p := PAggOrderRgba(This.RenderingBuffer.Row(y));
  inc(p, x);

  if Covers <> nil then
    repeat
      CopyOrBlendPixRgba(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a, Covers^);

      inc(Covers);

      p := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(p)));

      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixRgba(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a);

      p := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(p)));

      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixRgba(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a, Cover);

      p := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(p)));

      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0;
end;

procedure Rgba32BlendFrom(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; Psrc_: PInt8u; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  PSrc, PDst: PAggOrderRgba;
  Incp: Integer;
begin
  PSrc := PAggOrderRgba(Psrc_);
  PDst := PAggOrderRgba(This.RenderingBuffer.Row(Ydst));
  inc(PDst, Xdst);
  Incp := 4;

  if PtrComp(Xdst) > PtrComp(Xsrc) then
  begin
    inc(PtrComp(PSrc), (Len - 1) shl 2);
    inc(PtrComp(PDst), (Len - 1) shl 2);

    Incp := -4;
  end;

  if Cover = 255 then
    repeat
      CopyOrBlendPixRgba(PDst, TAggOrderRGBA(PSrc^).r, TAggOrderRGBA(PSrc^).g,
        TAggOrderRGBA(PSrc^).b, TAggOrderRGBA(PSrc^).a);

      inc(PtrComp(PSrc), Incp);
      inc(PtrComp(PDst), Incp);
      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixRgba(PDst, TAggOrderRGBA(PSrc^).r, TAggOrderRGBA(PSrc^).g,
        TAggOrderRGBA(PSrc^).b, TAggOrderRGBA(PSrc^).a, Cover);

      inc(PtrComp(PSrc), Incp);
      inc(PtrComp(PDst), Incp);
      dec(Len);
    until Len = 0;
end;

procedure Rgba32CopyColorHorizontalSpan(This: TAggPixelFormatProcessor;
  x, y: Integer; Len: Cardinal; Colors: PAggColor);
var
  p: PAggOrderRgba;
begin
  p := PAggOrderRgba(This.RenderingBuffer.Row(y));
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

procedure Rgba32CopyColorVerticalSpan(This: TAggPixelFormatProcessor;
  x, y: Integer; Len: Cardinal; Colors: PAggColor);
var
  p: PAggOrderRgba;
begin
  p := PAggOrderRgba(This.RenderingBuffer.Row(y));
  inc(p, x);

  repeat
    p^.r := Colors.Rgba8.r;
    p^.g := Colors.Rgba8.g;
    p^.b := Colors.Rgba8.b;
    p^.a := Colors.Rgba8.a;

    p := PAggOrderRgba(This.RenderingBuffer.NextRow(PInt8u(p)));

    inc(PtrComp(Colors), SizeOf(TAggColor));
    dec(Len);
  until Len = 0;
end;

procedure Rgba32BlendFromColor(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; COLOR: PAggColor; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  Ppsz: Cardinal;

  PSrc: PInt8u;
  PDst: PAggOrderRgba;
begin
  Ppsz := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PAggOrderRgba(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    inc(PDst, Xdst);

    repeat
      CopyOrBlendPixRgba(PDst, COLOR.Rgba8.r, COLOR.Rgba8.g, COLOR.Rgba8.b,
        COLOR.Rgba8.a, ShrInt32(PSrc^ * Cover + CAggBaseMask, CAggBaseShift));

      inc(PtrComp(PSrc), Ppsz);
      inc(PtrComp(PDst), 4);
      dec(Len);
    until Len = 0;
  end;
end;

procedure Rgba32BlendFromLUT(This: TAggPixelFormatProcessor;
  From: TAggPixelFormatProcessor; ColorLUT: PAggColor; Xdst, Ydst, Xsrc,
  Ysrc: Integer; Len: Cardinal; Cover: Int8u);
var
  Ppsz: Cardinal;

  PSrc: PInt8u;
  PDst: PAggOrderRgba;

  COLOR: PAggColor;
begin
  Ppsz := From.PixWidth;
  PSrc := From.GetRowPointer(Ysrc);

  if PSrc <> nil then
  begin
    PDst := PAggOrderRgba(This.RenderingBuffer.RowXY(Xdst, Ydst, Len));
    inc(PDst, Xdst);

    if Cover = 255 then
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

        CopyOrBlendPixRgba(PDst, COLOR.Rgba8.r, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.a);

        inc(PtrComp(PSrc), Ppsz);
        inc(PtrComp(PDst), 4);
        dec(Len);
      until Len = 0
    else
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

        CopyOrBlendPixRgba(PDst, COLOR.Rgba8.r, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.a, Cover);

        inc(PtrComp(PSrc), Ppsz);
        inc(PtrComp(PDst), 4);
        dec(Len);
      until Len = 0;
  end;
end; 
 
 
 
