//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixBgrPre(p: PInt8u;
  CR, Cg, CB, alpha, Cover: Cardinal); overload;
begin
  alpha := CAggBaseMask - alpha;
  Cover := (Cover + 1) shl (CAggBaseShift - 8);

  TAggOrderBGR(Pointer(p)^).r :=
    Int8u((TAggOrderBGR(Pointer(p)^).r * alpha + CR * Cover) shr CAggBaseShift);
  TAggOrderBGR(Pointer(p)^).g :=
    Int8u((TAggOrderBGR(Pointer(p)^).g * alpha + Cg * Cover) shr CAggBaseShift);
  TAggOrderBGR(Pointer(p)^).b :=
    Int8u((TAggOrderBGR(Pointer(p)^).b * alpha + CB * Cover) shr CAggBaseShift);

end;

procedure BlendPixBgrPre(p: PInt8u; CR, Cg, CB, alpha: Cardinal); overload;
begin
  alpha := CAggBaseMask - alpha;

  TAggOrderBGR(Pointer(p)^).r :=
    Int8u(((TAggOrderBGR(Pointer(p)^).r * alpha) shr CAggBaseShift) + CR);
  TAggOrderBGR(Pointer(p)^).g :=
    Int8u(((TAggOrderBGR(Pointer(p)^).g * alpha) shr CAggBaseShift) + Cg);
  TAggOrderBGR(Pointer(p)^).b :=
    Int8u(((TAggOrderBGR(Pointer(p)^).b * alpha) shr CAggBaseShift) + CB);

end;

procedure CopyOrBlendPixBgrPre(p: PInt8u; c: PAggColor;
  Cover: Cardinal); overload;
var
  alpha: Cardinal;
begin
  if c.Rgba8.a <> 0 then
  begin
    alpha := (c.Rgba8.a * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      TAggOrderBGR(Pointer(p)^).r := c.Rgba8.r;
      TAggOrderBGR(Pointer(p)^).g := c.Rgba8.g;
      TAggOrderBGR(Pointer(p)^).b := c.Rgba8.b;
    end
    else
      BlendPixBgrPre(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Cover);
  end;
end;

procedure CopyOrBlendPixBgrPre(p: PInt8u; c: PAggColor); overload;
begin
  if c.Rgba8.a <> 0 then
    if c.Rgba8.a = CAggBaseMask then
    begin
      TAggOrderBGR(Pointer(p)^).r := c.Rgba8.r;
      TAggOrderBGR(Pointer(p)^).g := c.Rgba8.g;
      TAggOrderBGR(Pointer(p)^).b := c.Rgba8.b;
    end
    else
      BlendPixBgrPre(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, c.Rgba8.a);
end;

procedure Bgr24PreBlendPixel(This: TAggPixelFormatProcessor; x, y: Integer;
  c: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPixBgrPre(PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x + x + x),
    c, Cover);
end;

procedure Bgr24PreBlendHorizontalLine(This: TAggPixelFormatProcessor; x,
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
        TAggOrderBGR(Pointer(p)^).r := c.Rgba8.r;
        TAggOrderBGR(Pointer(p)^).g := c.Rgba8.g;
        TAggOrderBGR(Pointer(p)^).b := c.Rgba8.b;

        inc(PtrComp(p), 3);
        dec(Len);
      until Len = 0
    else
      repeat
        BlendPixBgrPre(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha);

        inc(PtrComp(p), 3);
        dec(Len);
      until Len = 0;
  end;
end;

procedure Bgr24PreBlendVerticalLine(This: TAggPixelFormatProcessor; x,
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
        TAggOrderBGR(Pointer(p)^).r := c.Rgba8.r;
        TAggOrderBGR(Pointer(p)^).g := c.Rgba8.g;
        TAggOrderBGR(Pointer(p)^).b := c.Rgba8.b;

        p := PInt8u(This.RenderingBuffer.NextRow(p));

        dec(Len);
      until Len = 0
    else
      repeat
        BlendPixBgrPre(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Cover);

        p := PInt8u(This.RenderingBuffer.NextRow(p));

        dec(Len);
      until Len = 0;
  end;
end;

procedure Bgr24PreBlendSolidHorizontalSpan(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; c: PAggColor; Covers: PInt8u);
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
        TAggOrderBGR(Pointer(p)^).r := c.Rgba8.r;
        TAggOrderBGR(Pointer(p)^).g := c.Rgba8.g;
        TAggOrderBGR(Pointer(p)^).b := c.Rgba8.b;
      end
      else
        BlendPixBgrPre(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Covers^);

      inc(PtrComp(p), 3);
      inc(PtrComp(Covers));
      dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24PreBlendSolidVerticalSpan(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; c: PAggColor; Covers: PInt8u);
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
        TAggOrderBGR(Pointer(p)^).r := c.Rgba8.r;
        TAggOrderBGR(Pointer(p)^).g := c.Rgba8.g;
        TAggOrderBGR(Pointer(p)^).b := c.Rgba8.b;
      end
      else
        BlendPixBgrPre(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Covers^);

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      inc(PtrComp(Covers));
      dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24PreBlendColorHorizontalSpan(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x + x + x);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgrPre(p, Colors, Covers^);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(Covers), SizeOf(Int8u));
      inc(PtrComp(p), 3);
      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgrPre(p, Colors);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(p), 3);
      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgrPre(p, Colors, Cover);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(PtrComp(p), 3);
      dec(Len);
    until Len = 0;
end;

procedure Bgr24PreBlendColorVerticalSpan(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PInt8u;
begin
  p := PInt8u(PtrComp(This.RenderingBuffer.Row(y)) + x + x + x);

  if Covers <> nil then
    repeat
      CopyOrBlendPixBgrPre(p, Colors, Covers^);

      inc(PtrComp(Colors), SizeOf(TAggColor));
      inc(Covers);

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixBgrPre(p, Colors);

      inc(PtrComp(Colors), SizeOf(TAggColor));

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixBgrPre(p, Colors, Cover);

      inc(PtrComp(Colors), SizeOf(TAggColor));

      p := PInt8u(This.RenderingBuffer.NextRow(p));

      dec(Len);
    until Len = 0;
end;

procedure Bgr24PreBlendFrom(This, From: TAggPixelFormatProcessor;
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
          TAggOrderBGR(Pointer(PDst)^).r :=
            PInt8u(PtrComp(PSrc) + From.Order.r)^;
          TAggOrderBGR(Pointer(PDst)^).g :=
            PInt8u(PtrComp(PSrc) + From.Order.g)^;
          TAggOrderBGR(Pointer(PDst)^).b :=
            PInt8u(PtrComp(PSrc) + From.Order.b)^;
        end
        else
          BlendPixBgrPre(PDst, PInt8u(PtrComp(PSrc) + From.Order.r)^,
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

      CopyOrBlendPixBgrPre(PDst, @COLOR, Cover);

      inc(PtrComp(PSrc), 4);
      inc(PtrComp(PDst), 3);
      dec(Len);
    until Len = 0;
end;

procedure Bgr24PreBlendFromColor(This, From: TAggPixelFormatProcessor;
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
      CopyOrBlendPixBgrPre(PDst, COLOR,
        ShrInt32(PSrc^ * Cover + CAggBaseMask, CAggBaseShift));

      inc(PtrComp(PSrc), Ppsz);
      inc(PtrComp(PDst), 3);
      dec(Len);
    until Len = 0;
  end;
end;

procedure Bgr24PreBlendFromLUT(This, From: TAggPixelFormatProcessor;
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

        BlendPixBgrPre(PDst, COLOR.Rgba8.r, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.a);

        inc(PtrComp(PSrc), Ppsz);
        inc(PtrComp(PDst), 3);
        dec(Len);
      until Len = 0
    else
      repeat
        CopyOrBlendPixBgrPre(PDst,
          PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor)), Cover);

        inc(PtrComp(PSrc), Ppsz);
        inc(PtrComp(PDst), 3);
        dec(Len);
      until Len = 0;
  end;
end; 
 
 
 
