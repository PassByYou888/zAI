//
// AggPas 2.4 RM3 pixel format definition file
//

procedure BlendPixAbgrPre(p: PCardinal;
  CR, Cg, CB, alpha: Cardinal); overload;
begin
  alpha := CAggBaseMask - alpha;

  TAggOrderABGR(p^).r := Int8u(((TAggOrderABGR(p^).r * alpha) shr CAggBaseShift) + CR);
  TAggOrderABGR(p^).g := Int8u(((TAggOrderABGR(p^).g * alpha) shr CAggBaseShift) + Cg);
  TAggOrderABGR(p^).b := Int8u(((TAggOrderABGR(p^).b * alpha) shr CAggBaseShift) + CB);
  TAggOrderABGR(p^).a := Int8u(CAggBaseMask - ((alpha * (CAggBaseMask -
    TAggOrderABGR(p^).a)) shr CAggBaseShift));
end;

procedure BlendPixAbgrPre(p: PCardinal;
  CR, Cg, CB, alpha, Cover: Cardinal); overload;
begin
  alpha := CAggBaseMask - alpha;
  Cover := (Cover + 1) shl (CAggBaseShift - 8);

  TAggOrderABGR(p^).r := Int8u((TAggOrderABGR(p^).r * alpha + CR * Cover)
    shr CAggBaseShift);
  TAggOrderABGR(p^).g := Int8u((TAggOrderABGR(p^).g * alpha + Cg * Cover)
    shr CAggBaseShift);
  TAggOrderABGR(p^).b := Int8u((TAggOrderABGR(p^).b * alpha + CB * Cover)
    shr CAggBaseShift);
  TAggOrderABGR(p^).a := Int8u(CAggBaseMask - ((alpha * (CAggBaseMask -
    TAggOrderABGR(p^).a)) shr CAggBaseShift));
end;

procedure CopyOrBlendPixAbgrPre(p: PCardinal;
  CR, Cg, CB, alpha: Cardinal); overload;
begin
  if alpha <> 0 then
    if alpha = CAggBaseMask then
    begin
      TAggOrderABGR(p^).r := CR;
      TAggOrderABGR(p^).g := Cg;
      TAggOrderABGR(p^).b := CB;
      TAggOrderABGR(p^).a := CAggBaseMask;
    end
    else
      BlendPixAbgrPre(p, CR, Cg, CB, alpha);
end;

procedure CopyOrBlendPixAbgrPre(p: PCardinal;
  CR, Cg, CB, alpha, Cover: Cardinal); overload;
begin
  if Cover = 255 then
    CopyOrBlendPixAbgrPre(p, CR, Cg, CB, alpha)
  else if alpha <> 0 then
  begin
    alpha := (alpha * (Cover + 1)) shr 8;

    if alpha = CAggBaseMask then
    begin
      TAggOrderABGR(p^).r := CR;
      TAggOrderABGR(p^).g := Cg;
      TAggOrderABGR(p^).b := CB;
      TAggOrderABGR(p^).a := CAggBaseMask;
    end
    else
      BlendPixAbgrPre(p, CR, Cg, CB, alpha, Cover);
  end;
end;

procedure Abgr32PreBlendPixel(This: TAggPixelFormatProcessor; x, y: Integer;
  c: PAggColor; Cover: Int8u);
begin
  CopyOrBlendPixAbgrPre(PCardinal(PtrComp(This.RenderingBuffer.Row(y)) +
    (x shl 2)), c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, c.Rgba8.a, Cover);
end;

procedure Abgr32PreBlendHorizontalLine(This: TAggPixelFormatProcessor; x,
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
      TAggOrderABGR(v).r := c.Rgba8.r;
      TAggOrderABGR(v).g := c.Rgba8.g;
      TAggOrderABGR(v).b := c.Rgba8.b;
      TAggOrderABGR(v).a := c.Rgba8.a;

      repeat
        p^ := v;

        inc(PtrComp(p), SizeOf(Cardinal));
        dec(Len);
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        BlendPixAbgrPre(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha);

        inc(PtrComp(p), SizeOf(Cardinal));
        dec(Len);
      until Len = 0
    else
      repeat
        BlendPixAbgrPre(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Cover);

        inc(PtrComp(p), SizeOf(Cardinal));
        dec(Len);
      until Len = 0;
  end;
end;

procedure Abgr32PreBlendVerticalLine(This: TAggPixelFormatProcessor; x, y: Integer;
  Len: Cardinal; c: PAggColor; Cover: Int8u);
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
      TAggOrderABGR(v).r := c.Rgba8.r;
      TAggOrderABGR(v).g := c.Rgba8.g;
      TAggOrderABGR(v).b := c.Rgba8.b;
      TAggOrderABGR(v).a := c.Rgba8.a;

      repeat
        p^ := v;
        p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));
      until Len = 0;
    end
    else if Cover = 255 then
      repeat
        BlendPixAbgrPre(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha);

        p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

        dec(Len);
      until Len = 0
    else
      repeat
        BlendPixAbgrPre(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Cover);

        p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

        dec(Len);
      until Len = 0;
  end;
end;

procedure Abgr32PreBlendSolidHSpan(This: TAggPixelFormatProcessor; x,
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
        TAggOrderABGR(p^).r := c.Rgba8.r;
        TAggOrderABGR(p^).g := c.Rgba8.g;
        TAggOrderABGR(p^).b := c.Rgba8.b;
        TAggOrderABGR(p^).a := CAggBaseMask;
      end
      else
        BlendPixAbgrPre(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Covers^);

      inc(PtrComp(p), 4);
      inc(PtrComp(Covers));

      dec(Len);
    until Len = 0;
  end;
end;

procedure Abgr32PreBlendSolidVSpan(This: TAggPixelFormatProcessor; x,
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
        TAggOrderABGR(p^).r := c.Rgba8.r;
        TAggOrderABGR(p^).g := c.Rgba8.g;
        TAggOrderABGR(p^).b := c.Rgba8.b;
        TAggOrderABGR(p^).a := CAggBaseMask;
      end
      else
        BlendPixAbgrPre(p, c.Rgba8.r, c.Rgba8.g, c.Rgba8.b, alpha, Covers^);

      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      inc(PtrComp(Covers), SizeOf(Int8u));
      dec(Len);
    until Len = 0;
  end;
end;

procedure Abgr32PreBlendColorHSpan(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PCardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2));

  if Covers <> nil then
    repeat
      CopyOrBlendPixAbgrPre(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a, Covers^);

      inc(PtrComp(Covers), SizeOf(Int8u));
      inc(PtrComp(p), 4);
      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixAbgrPre(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a);

      inc(PtrComp(p), 4);
      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixAbgrPre(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a, Cover);

      inc(PtrComp(p), 4);
      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0;
end;

procedure Abgr32PreBlendColorVSpan(This: TAggPixelFormatProcessor; x,
  y: Integer; Len: Cardinal; Colors: PAggColor; Covers: PInt8u; Cover: Int8u);
var
  p: PCardinal;
begin
  p := PCardinal(PtrComp(This.RenderingBuffer.Row(y)) + (x shl 2));

  if Covers <> nil then
    repeat
      CopyOrBlendPixAbgrPre(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a, Covers^);

      inc(PtrComp(Covers), SizeOf(Int8u));

      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0
  else if Cover = 255 then
    repeat
      CopyOrBlendPixAbgrPre(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a);

      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixAbgrPre(p, Colors.Rgba8.r, Colors.Rgba8.g, Colors.Rgba8.b,
        Colors.Rgba8.a, Cover);

      p := PCardinal(This.RenderingBuffer.NextRow(PInt8u(p)));

      inc(PtrComp(Colors), SizeOf(TAggColor));
      dec(Len);
    until Len = 0;
end;

procedure Abgr32PreBlendFrom(This, From: TAggPixelFormatProcessor;
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
      CopyOrBlendPixAbgrPre(PDst, TAggOrderABGR(PSrc^).r, TAggOrderABGR(PSrc^).g,
        TAggOrderABGR(PSrc^).b, TAggOrderABGR(PSrc^).a);

      inc(PtrComp(PSrc), Incp);
      inc(PtrComp(PDst), Incp);
      dec(Len);
    until Len = 0
  else
    repeat
      CopyOrBlendPixAbgrPre(PDst, TAggOrderABGR(PSrc^).r, TAggOrderABGR(PSrc^).g,
        TAggOrderABGR(PSrc^).b, TAggOrderABGR(PSrc^).a, Cover);

      inc(PtrComp(PSrc), Incp);
      inc(PtrComp(PDst), Incp);
      dec(Len);
    until Len = 0;
end;

procedure Abgr32PreBlendFromColor(This, From: TAggPixelFormatProcessor;
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
      CopyOrBlendPixAbgrPre(PDst, COLOR.Rgba8.r, COLOR.Rgba8.g, COLOR.Rgba8.b,
        COLOR.Rgba8.a, ShrInt32(PSrc^ * Cover + CAggBaseMask, CAggBaseShift));

      inc(PtrComp(PSrc), Ppsz);
      inc(PtrComp(PDst), 4);
      dec(Len);
    until Len = 0;
  end;
end;

procedure Abgr32PreBlendFromLUT(This, From: TAggPixelFormatProcessor;
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

        CopyOrBlendPixAbgrPre(PDst, COLOR.Rgba8.r, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.a);

        inc(PtrComp(PSrc), Ppsz);
        inc(PtrComp(PDst), 4);
        dec(Len);
      until Len = 0
    else
      repeat
        COLOR := PAggColor(PtrComp(ColorLUT) + PSrc^ * SizeOf(TAggColor));

        CopyOrBlendPixAbgrPre(PDst, COLOR.Rgba8.r, COLOR.Rgba8.g, COLOR.Rgba8.b,
          COLOR.Rgba8.a, Cover);

        inc(PtrComp(PSrc), Ppsz);
        inc(PtrComp(PDst), 4);
        dec(Len);
      until Len = 0;
  end;
end; 
 
 
 
